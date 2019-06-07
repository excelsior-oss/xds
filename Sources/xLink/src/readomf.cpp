#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "debug.h"
#include "idents.h"
#include "messages.h"
#include "xdebug.h"
#include "xmem.h"

#include "omf.h"

#include "readomf0.h"


struct lcomdef {
        ident name;
        int   length;
};

struct local {
        Segment      * seg;
        struct group * group;
        int            length;          /* For (L)COMDEF only */
        int            offset;
        unsigned char  kind;
};

class OMFFileReader : public OMF0Reader {
  private:
    /* Used when resolving intra-module references */
    Bool WasLocals, WasLCOMDEFs;
    struct local * LocalsTable;
    int            LocalsSize;

    /* LCOMDEFs Table */
    struct lcomdef * LocalComdefsTable;
    int              NLComdefs;
    int              LComdefsSize;
    
    /* Groups Table */
    struct group ** GroupsTable;
    int             NGroups;
    int             GroupsSize;

    Bool            filenameWasSet;

    dword GetCommLen(void);
    void * getFixupTarget (byte k_target, int i);

    void ResolveTarget (struct fixup * f, struct local * p);
    void ResolveLocals (void);

  protected:
    void extdef (void);
    void pubdef (char kind);
    void coment (void);
    void fixupp (void);
    void comdat (void);
    void segdef (void);
    void grpdef (void);
    void comdef (char kind);
    void modend (void);
    void theadr (void);

    void NewModule (void);
    void EndModule (void);

  public:
    void ReadRecord (void);
    Bool IsLibrary(void);
    void Read (byte * rawdata, long size, char * filename);

    OMFFileReader();
    ~OMFFileReader();
};

/*----------------------------------------------------------------------------*/

void OMFFileReader::NewModule (void)
{
    OMF0Reader :: NewModule ();

    NGroups =
    NLComdefs =
    0;

    WasLocals = WasLCOMDEFs = false;
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::EndModule (void)
{
    if (WasLocals) {
        ResolveLocals ();
    }
    OMF0Reader :: EndModule ();
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::ResolveTarget (struct fixup * f, struct local * p)
{
    if (p -> seg) {
        f -> k_target = TK_SEG;
        f -> target   = p -> seg;
    } else if (p -> group) {
        f -> k_target = TK_GROUP;
        f -> target   = p -> group;
    } else {
        ASSERT_FALSE();
    }

    f -> fx_offset += p -> offset;
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::ResolveLocals (void)
{
        int i;
        struct local * p;
        struct pubname * q;
        struct lcomdef * r;
        Segment * s;
        struct fixup * f;

        if (LocalsSize <= NAMES.getTableSize()) {
                i = (NAMES.getTableSize() + 1) * 2;
                LocalsTable = (struct local *) xrealloc (LocalsTable,
                                               LocalsSize * sizeof (struct local),
                                               i * sizeof (struct local));
                LocalsSize = i;
        }
        i = 0;
        do {
                LocalsTable [i] . kind = 0;
        } while (++ i <= NAMES.getTableSize());
        for (i = 0, q = PubNamesTable; i < NPubNames; i ++, q ++)
                if (q -> kind & K_LOCAL) {
                        p = LocalsTable + /*FIXME*/(int) (q -> name);
                        if (p -> kind)
                                Message(xERROR, msgNAME_WAS_TWICE_DECLARED,
                                             NAMES.Index2Str (q -> name),
                                             GetCurrentOBJFileName ());
                        else {
                                p -> kind   = (byte) (K_PUBDEF + K_LOCAL);
                                p -> group  = q -> group;
                                p -> seg    = q -> seg;
                                p -> offset = q -> offset;
                                p -> length = 0;
                                VerboseMessage (INFO_OMFREAD, "OMF: (Locals) ID %s (#%d) (lpubdef)\n", NAMES.Index2Str(q -> name), (int)(q -> name));
                        }
                }
        if (WasLCOMDEFs) {
                for (i = 0, r = LocalComdefsTable; i < NLComdefs; i ++, r ++) {
                                p = LocalsTable + /*FIXME*/(int) (r -> name);
                                if (p -> kind)
                                        if (p -> kind != K_COMDEF + K_LOCAL)
                                               Message (xERROR, msgNAME_WAS_TWICE_DECLARED,
                                                             NAMES.Index2Str (q -> name),
                                                             GetCurrentOBJFileName ());
                                        else {
                                                if (p -> length < r -> length)
                                                        p -> length = r -> length;
                                        }
                                else {
                                        p -> kind   = (byte) (K_COMDEF + K_LOCAL);
                                        p -> group  = NULL;
                                        p -> seg    = NULL;
                                        p -> length = r -> length;
                                        p -> offset = 0;
                                        VerboseMessage (INFO_OMFREAD, "OMF: (Locals) ID %s (#%d) (lcomdef)\n", NAMES.Index2Str(q -> name), (int)(q -> name));
                                }
                        }
                for (i = 0, r = LocalComdefsTable; i < NLComdefs; i ++, r ++) {
                                p = LocalsTable + /*FIXME*/(int) (r -> name);
                                p -> seg = new Segment (BSS, BSS, BSS, false, p -> length);
                        }
        }
        for (s = CurrentFile -> segs; s; s = s -> next)
                if (s -> nfixups)
                        for (i = s -> nfixups, f = s -> fixups; i; f ++, i --) {
                                if (f -> k_target == TK_ID) {
                                        p = LocalsTable + (int)(f -> target);
                                        if (p -> kind) {
                                                VerboseMessage (INFO_OMFREAD, "OMF: (Locals) fixup to ID %s (#%d) resolved", NAMES.Index2Str((ident)(f -> target)), (int)(f -> target));
                                                ResolveTarget (f, p);
                                        } else {
                                                VerboseMessage (INFO_OMFREAD, "OMF: (Locals) fixup to ID %s (#%d) not found", NAMES.Index2Str((ident)(f -> target)), (int)(f -> target));
                                        }
                                }
                        }
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::extdef ()
{
    while (Available() > 0) {
        ident id = GetIdent ();
        GetIndex (); // skip type index
        ExtNamesTable.addElement (id);
        VerboseMessage (INFO_OMFREAD, "OMF: EXTDEF %s\n", NAMES.Index2Str(id));
    }
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::pubdef (char kind)
{
    int index;
    struct group   * group;
    Segment     * seg;
    struct pubname * q;
    struct pub     * Public;

    index = GetIndex ();
    group = index ? GroupsTable [index - 1] : NULL;
    index = GetIndex ();
    if (index)
        seg = SegsTable [index - 1];
    else {
        seg   = NULL;
        index = GetWord ();       /* skip frame */
    }
    while (Available() > 0) {
        Grow (NPubNames, PubNamesSize, 32, PubNamesTable, struct pubname, q);
        q -> name = GetIdent ();
        q -> offset = GetWordOrDword ();
        GetIndex ();
        if (seg && seg -> overlay)
             GetWordOrDword ();
        q -> seg   = seg;
        q -> group = group;
        q -> kind  = kind;

        if (! (kind & K_LOCAL) ) {
            NewPublicName (q -> name, q -> seg, q -> offset, T_UNKNOWN);
        }

        Public = (struct pub *) allocateForever (sizeof(struct pub));
        Public -> next = CurrentFile -> publics;
        CurrentFile -> publics = Public;
        Public -> seg    = seg;
        Public -> name   = q -> name;
        Public -> offset = q -> offset;

        if (kind & K_LOCAL) {
            VerboseMessage (INFO_OMFREAD, "OMF: LPUBDEF %s\n", NAMES.Index2Str(q -> name));
        } else {
            VerboseMessage (INFO_OMFREAD, "OMF: PUBDEF %s\n", NAMES.Index2Str(q -> name));
        }
    }
}

/*----------------------------------------------------------------------------*/

void * OMFFileReader::getFixupTarget (byte k_target, int i) {
    switch (k_target) {
        case TK_SEG:
            return SegsTable [i - 1];

        case TK_GROUP:
            return GroupsTable [i - 1];

        case TK_ID:
            return (void *) (ExtNamesTable[i-1]);

        default:
            ASSERT_FALSE ();
    }
    return NULL;
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::coment ()
{
    byte rec, subrec, ord;
    Bool intname_present;
    ident imp_name, name, module, entry;
    short  ordinal;
    byte kind;
    char buf [256];
    char * r;

    rec = GetByte ();                          /* Comment type */
    rec = GetByte ();

    VerboseMessage (INFO_OMFREAD, "OMF: COMENT %d\n", rec);

    switch (rec) {
        case 0xA0:
            subrec = GetByte ();
            switch (subrec) {
                case 0x01:                      /* import definition */
                    ord = GetByte ();
                    name = GetIdent ();
                    memcpy (buf, cur_pos + 1, * cur_pos);
                    buf [* cur_pos] = 0;
                    rec_idx += (* cur_pos) + 1;
                    cur_pos += (* cur_pos) + 1;

                    module = NAMES.Str2Index (ConvDLLName (buf));
                    if (ord) {
                        entry = /*FIXME*/(ident) GetWord ();
                        kind = K_IMPORT | K_BY_ORDINAL;
                    } else {
                        if (* cur_pos) {
                            entry = GetIdent ();
                        } else {
                            entry = name;
                            cur_pos++;
                        }
                        kind = K_IMPORT;
                    }
                    if (ord)
                        VerboseMessage (INFO_OMFREAD, "OMF: IMPDEF %s = %d (%s)\n", NAMES.Index2Str(name), (int)entry, NAMES.Index2Str(module));
                    else
                        VerboseMessage (INFO_OMFREAD, "OMF: IMPDEF %s = %s (%s)\n", NAMES.Index2Str(name), NAMES.Index2Str(entry), NAMES.Index2Str(module));
                    r = dup(NAMES.Index2Str(name), NAMES.Index2StrLen(name));
                    if (check_imp_var_suffix(r)) {
                        imp_name = NAMES.Str2Index (r);
                        nameInfo * q = (nameInfo *) (NAMES.getInfo (imp_name));
                        if (q) {
                            if ((q -> kind & ~K_IMPFUNC) == (kind & ~K_IMPFUNC) &&
                               ((importNameInfo *)q) -> getModuleName () == module)
                            {
                                q -> kind &= ~K_IMPFUNC;
                                break;
                            } else
                                name = imp_name;
                        }
                    } else
                        kind |= K_IMPFUNC;
                    NewImportName (name, kind, module, /*FIXME*/(int)entry);
                    break;

                case 0x02:                      /* export definition */
                    kind = GetByte ();
                    name = GetIdent ();
                    intname_present = (*cur_pos != 0) ? true : false;
                    if (intname_present) {
                          imp_name = GetIdent ();
                    } else {
                          imp_name = name;
                          cur_pos ++;
                    }
                    ordinal = 0;
                    if (kind & 0x80)
                          ordinal = GetWord ();
                    VerboseMessage (INFO_OMFREAD, "OMF: EXPDEF %s = %s (%d) i=%d\n", NAMES.Index2Str(name), NAMES.Index2Str(imp_name), (int)ordinal, intname_present);
                    NewExpdef(imp_name, name, ordinal, intname_present);
            }
            break;
        case 0xA1:
            subrec = GetByte ();
            switch (subrec) {
                case 0x04:                      /* Debug format HLL 4 */
                    DebugInfo_Format = DBG_FMT_HLL4;
                    break;
                case 0x45:                      /* Excelsior's Debug Format NB99 (HLL4 with long names) */
                    DebugInfo_Format = DBG_FMT_NB99;
                    break;
                case 0xED:                      /* Excelsior's Debug Format EDIF (HLL4 in standalone files) */
                    if (CurrentFile -> dbgcomment) {
                        Message (xERROR, msgINCORRECT_RECORD_FORMAT, GetCurrentOBJFileName (), "COMENT", "only one COMENT of A1/ED type allowed");
                    } else {
                        DebugInfo_Format = DBG_FMT_EDIF;
                        DebugFormat |= DBG_FMT_EDIF;
                        if (!FirstEDIFModule)
                            FirstEDIFModule = GetCurrentOBJFileName ();

                        CurrentFile -> dbgcomment = GetString (Available (), true);
                    }
                    break;
            }
            break;

        case MODNAM:
            CurrentFile -> setFilename (GetString (Available ()));
            filenameWasSet = true;
            break;

        case FULNAM:
            if (!filenameWasSet)
                Message(xFATAL, msgFULNAM_WO_MODNAM,  GetCurrentOBJFileName ());

            CurrentFile -> setSource(GetString (Available ()));
            break;

        case VERSTAMP:
            dword version = GetDword();
            ASSERT (VerStamp != NULL);
            VerStamp->setVersionStamp (version);
            break;
    }
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::comdat (void)
{
    Message(xFATAL, msgUNSUPPORTED_OMF_RECORD_TYPE, GetCurrentOBJFileName ());
}


/*----------------------------------------------------------------------------*/

void OMFFileReader::fixupp (void)
{
    int n = 0;

    Segment * seg = LedataSeg;
    if (seg && seg -> clazz == BSS)
        Message(xFATAL, msgFIXUP_IN_BSS_SEG, GetCurrentOBJFileName (), NAMES.Index2Str (seg -> name));

    while (Available() > 0) {
        byte first = GetByte ();
        if (first & 0x80) {
            /* fixup */
            if (! seg)
                Message(xFATAL, msgFIXUP_WITHOUT_LEDATA, GetCurrentOBJFileName ());

            byte locat = (byte) ((first >> 2) & 0x0f);

            byte fixupKind = locat;
            if (! (first & 0x40))
                fixupKind |= FX_SELFRELATIVE;

            dword offset = GetByte ();
            offset |= (first & 3) << 8;
            offset += LastOffset;

            byte   targetKind = 0;
            void * target     = NULL;

            byte fix_data = GetByte ();
            if (fix_data & 0x80) {
                /* previously declared frame */
                n = (fix_data >> 4) & 3;
            } else {
                /* frame */
                byte method = (byte) ((fix_data >> 4) & 7);
                switch (method) {
                    case FK_SEG:
                    case FK_GROUP:
                    case FK_ID:
                        GetIndex ();
                        break;
                    case FK_CURSEG:
                    case FK_TARGET:
                        break;
                    default:
                        ASSERT_FALSE ();
                }
            }
            if (fix_data & 0x08) {
                /* previously declared target */
                n = fix_data & 3;
                targetKind = target_kinds [n];
                target     = targets [n];
            } else {
                /* target */
                int i = GetIndex ();
                targetKind = (byte) (fix_data & 3);
                target     = getFixupTarget (targetKind, i);
            }
            dword fx_offset = 0;
            if (fix_data & 0x04)
                fx_offset = 0;
            else
                fx_offset = GetWordOrDword ();

            byte fxKind = 0;
            switch (fixupKind) {
                case FX_SELFRELATIVE + FX_OFFSET32:
                                  fxKind = FIXUP_SELFRELOFFS32; break;
                case FX_OFFSET32: fxKind = FIXUP_ADDRESS32;     break;
                case FX_FAR16_16: fxKind = FIXUP_FAR16_16;      break;
                case FX_FAR16_32: fxKind = FIXUP_FAR16_32;      break;

                default:
                    VerboseMessage (INFO_OMFREAD, "OMF: FIXUP kind=%d\n", fxKind);
                    switch (targetKind) {
                        case TK_SEG:
                            VerboseMessage (INFO_OMFREAD, "OMF: FIXUP target segment: %s\n", NAMES.Index2Str(((Segment*)target)->getName()));
                            break;
                        case TK_GROUP:
                            VerboseMessage (INFO_OMFREAD, "OMF: FIXUP target group\n");
                            break;
                        case TK_ID:
                            VerboseMessage (INFO_OMFREAD, "OMF: FIXUP target id: %s\n", NAMES.Index2Str((ident)target));
                            break;
                        default:
                            ASSERT_FALSE ();
                    }
                    Message(xERROR, msgINCORRECT_RECORD_FORMAT, filename, "OMF/FIXUP", "unsupported fixup kind");
                    return;
            }
            VerboseMessage (INFO_OMFREAD, "OMF: FIXUP FX:%d OFFS=%XH TRG<%d %XH OFFS=%d>\n", fxKind, offset, targetKind, target, fx_offset);
            addFixup (seg, fxKind, offset, targetKind, target, fx_offset);
        } else {
            /* frame or target declaration */
            n = first & 3;
            byte method = (byte) ((first >> 2) & 7);
            int i = 0;
            if (first & 0x40)
                switch (method) {
                    case FK_SEG:
                    case FK_GROUP:
                    case FK_ID:
                        i = GetIndex ();
                        break;
                    case FK_CURSEG:
                    case FK_TARGET:
                        break;
                    default:
                        ASSERT_FALSE ();
                }
            else {
                i = GetIndex ();
                target_kinds [n] = (byte) (method & 3);
                targets      [n] = getFixupTarget (target_kinds [n], i);
            }
        }
    }
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::segdef (void)
{
    int attributes, alignment, combination, frame, offset;
    int index;
    dword length;

    ident name    = INVALID_ID;
    ident clazz   = INVALID_ID;
    ident overlay = 0;

    attributes  = GetByte ();
    alignment   = (attributes >> 5) & 7;
    combination = (attributes >> 2) & 7;
    if (alignment == 0) {
        frame  = GetWord ();
        offset = GetByte ();
    }
    length = GetWordOrDword ();

    index = GetIndex ();
    if (index)
        name = getLName (index);

    index = GetIndex ();
    if (index)
        clazz = getLName (index);

    index = GetIndex ();
    if (index)
        overlay = getLName (index);

    if (! (attributes & 1) && (attributes & 2))
        length = 1L << 16;

    dword align = 1;
    dword addr  = (dword) -1;

    if (xExtraAlignment)
        align = Alignment (length);
    else
        switch (alignment) {
            case 0:
                align = 0;
                addr  = frame;
                break;
            case 1:
                align = 1;
                break;
            case 2:
                align = 2;
                break;
            case 3:
                align = 16;
                break;
            case 4:
                align = 4096;
                break;
            case 5:
                align = 4;
                break;
            case 6:
            case 7:
                align = (dword) -1;
                break;
        }

    // support for nasm-generated object files
    if (clazz == NAMES.Str2Index ("")) {
        if (name == NAMES.Str2Index ("text")) {
            clazz = CODE;
        } else if (name == NAMES.Str2Index ("data")) {
            clazz = DATA;
        } else if (name == NAMES.Str2Index ("bss")) {
            clazz = BSS;
        }
    }

    Segment * q = new Segment (name, name, clazz, (Bool)(!(attributes & 1)), length, align);

    q -> overlay = overlay;
    q -> address = addr;

    Segment ** w;
    Grow (NSegs, SegsSize, 32, SegsTable, Segment *, w);
    * w = q;

#if defined (OBSOLETE)
    q -> combination = combination;
    q -> attributes  = attributes;
#endif

   VerboseMessage (INFO_OMFREAD, "OMF: SEGDEF %s class=%s align=%d len=%XH\n",
                   NAMES.Index2Str (name), NAMES.Index2Str (clazz), align, length);
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::grpdef (void)
{
    int index, b;
    ident name, seg;
    struct group * q;
    struct elem ** r;
    struct group ** w;
    Bool   found;

    index = GetIndex ();
    name = getLName (index);
    if (LNamesTable [index - 1] . kind & K_LOCAL)
        Message(xFATAL, msgLOCAL_GRP_NOT_SUPPORTED, GetCurrentOBJFileName (), NAMES.Index2Str (name));
    found = false;
    for (q = GroupList; q; q = q -> next)
        if (name == q -> name) {
            found = true;
            break;
        }
    if (!found) {
        q = (struct group *) xalloc (sizeof (struct group));
        q -> name = name;
        q -> segs = NULL;
        q -> next = GroupList;
        GroupList = q;
    }
    Grow (NGroups, GroupsSize, 32, GroupsTable, struct group *, w);
    * w = q;
    while (Available() > 0) {
        b = GetByte ();
        if (b != 0xff)
            Message(xFATAL, msgGRP_TYPE_NOT_SUPPORTED, GetCurrentOBJFileName (), b);
        seg = GetSegName ();
        found = false;
        for (r = & (q -> segs); * r; r = & ((* r) -> next))
            if ((* r) -> name == seg) {
                found = true;
                break;
            }
        if (!found) {
             * r = (struct elem *) xalloc (sizeof (struct elem));
            (* r) -> name = seg;
            (* r) -> next = NULL;
        }
    }
}

/*----------------------------------------------------------------------------*/

dword OMFFileReader::GetCommLen(void)
{
    dword result = GetByte();
    if (result > 0x80)
        switch (result) {
            case 0x81:
                return GetWord ();
            case 0x84:
                result =  (*  cur_pos     )       |
                         ((* (cur_pos + 1)) << 8) |
                         ((* (cur_pos + 2)) << 16);
                Advance (3);
                return result;
            case 0x88:
                return GetDword();
            default:
                Message(xFATAL, msgUNKNOWN_COMDAT, GetCurrentOBJFileName (), result);
        }
    return 0;
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::comdef (char kind)
{
    int data_type, comm_len, comm_count;

    while (Available() > 0) {
        ident q = GetIdent ();
        ExtNamesTable.addElement (q);
        GetIndex ();
        data_type = GetByte ();
        switch (data_type) {
            case 0x61:
                comm_count  = GetCommLen ();
                comm_len    = GetCommLen () * comm_count;
                break;
            case 0x62:
                comm_len    = GetCommLen ();
                break;
            default:
                Message(xFATAL, msgUNKNOWN_COMMON_TYPE, GetCurrentOBJFileName (), data_type);
        }
        if (kind & K_LOCAL) {
            struct lcomdef * l;
            Grow (NLComdefs, LComdefsSize, 32, LocalComdefsTable, struct lcomdef, l);
            l -> name   = q;
            l -> length = comm_len;
        } else
            NewCommonName (q, comm_len);
    }
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::modend (void)
{
    byte type = GetByte ();
    if (type & 0x40) {
        if (xWasEntryPoint) {
            Message(xWARNING, msgIGNORED_ENTRY_POINT, GetCurrentOBJFileName (), xEntryPointFile);
            return;
        }
        byte fix_data = GetByte ();

        int n = 0;
        if (fix_data & 0x80) {
            n = (fix_data >> 4) & 3;
        } else {
            int method = (fix_data >> 4) & 7;
            switch (method) {
                case FK_SEG:
                case FK_GROUP:
                case FK_ID:
                    GetIndex ();
                    break;
                case FK_CURSEG:
                    if (! LedataSeg)
                        Message(xFATAL, msgINVALID_ENTRY_POINT, GetCurrentOBJFileName ());
                    break;
            }
        }
        byte k_target = 0;
        void * target = NULL;
        if (fix_data & 0x08) {
            n = fix_data & 3;
            k_target = target_kinds [n];
            target   = targets [n];
        } else {
            ident i = GetIndex ();
            byte method = (byte) (fix_data & 3);
            k_target = method;
            switch (method) {
                case TK_SEG:
                    target = SegsTable [i - 1];
                    break;
                case TK_GROUP:
                    target = GroupsTable [i - 1];
                    break;
                case TK_ID:
                    target = (void *) (ExtNamesTable[i-1]);
                    break;
            }
        }
        dword fx_offset = GetWordOrDword ();

        setEntryPoint (FIXUP_FAR16_32, k_target, target, fx_offset, GetCurrentOBJFileName ());
    }
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::theadr (void)
{
    new OBJFile (dup ( (char *)(cur_pos + 1), * cur_pos), libname);
    const char* fname = CurrentFile -> getFilename();
    CurrentFile -> setSource (dup(fname, strlen(fname)));
    filenameWasSet = false;
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::ReadRecord (void)
{
    if (size < 3)
        Message(xFATAL, msgILLEGAL_RECORD_LENGTH);
    rec_start = cur_pos;
    rec_type  = GetByte();
    rec_len   = GetWord() - 1;
    rec_idx   = 0;
    if (rec_len > (dword)size)
        Message(xFATAL, msgRECORD_TOO_LONG);
    switch (rec_type) {
        case LIBHDR:
            LibPageSize = rec_len + 4;
            break;

        case LIBEND:
            size = 0;
            return;

        case THEADR:
        case LHEADR:
            theadr ();
            break;

        case LNAMES:
            lnames (0);
            break;

        case LLNAMES:
            WasLocals = true;
            lnames ((char) K_LOCAL);
            break;

        case SEGDEF:
        case SEGDEF | REC32:
            segdef ();
            break;

        case GRPDEF:
            grpdef ();
            break;

        case EXTDEF:
        case LEXTDEF:
            extdef ();
            break;

        case COMDEF:
            comdef (K_COMDEF);
            break;

        case LCOMDEF:
            WasLocals = WasLCOMDEFs = true;
            comdef ((char) (K_COMDEF + K_LOCAL));
            break;

        case PUBDEF:
        case PUBDEF | REC32:
            pubdef (K_PUBDEF);
            break;

        case LPUBDEF:
        case LPUBDEF | REC32:
            WasLocals = true;
            pubdef ((char) (K_PUBDEF + K_LOCAL));
            break;

        case LEDATA:
        case LEDATA | REC32:
            ledata ();
            break;

        case LIDATA:
        case LIDATA | REC32:
            lidata ();
            break;

        case LINNUM:
        case LINNUM | REC32:
            linnum ();
            break;

        case FIXUPP:
        case FIXUPP | REC32:
            fixupp ();
            break;

        case COMENT:
            coment ();
            break;

        case MODEND:
        case MODEND | REC32:
            modend ();
            EndModule ();
            if (CurrentFile -> lib) {
                NewModule ();
                cur_pos = rec_start + ((rec_start + 3 + rec_len) - rawdata + LibPageSize) /
                          LibPageSize * LibPageSize - (rec_start - rawdata);
                size -= cur_pos - rec_start;
                return;
            }
            break;

        case TYPDEF:
            break;

        case COMDAT:
        case COMDAT | REC32:
            comdat ();
            break;

        default:
            Message (xFATAL, msgUNKNOWN_RECORD_TYPE, filename, rec_type);
    }
    cur_pos = rec_start + (rec_len + 1 /* REC TYPE */ + 2 /* REC LEN */ + 1 /* CHKSUM */);
    size    = size      - (rec_len + 1 /* REC TYPE */ + 2 /* REC LEN */ + 1 /* CHKSUM */);
    return;
}

Bool OMFFileReader::IsLibrary()
{
    return (*rawdata == LIBHDR);
}

/*----------------------------------------------------------------------------*/

void OMFFileReader::Read (byte * data, long datasize, char * fname)
{
    VerboseMessage(INFO_OMFREAD, "OMF: Reading File %s\n", fname);

    filename          = fname;
    rawdata  = cur_pos = data;
    size               = datasize;

    DebugInfo_Format  = DBG_FMT_CV;

    NewModule ();

    libname = (IsLibrary()) ? filename : NULL;
    do {
       ReadRecord ();
    } while (size > 0);
}

/*----------------------------------------------------------------------------*/

OMFFileReader :: OMFFileReader (void)
{
    GroupsTable   = NULL;
    GroupsSize    = 0;
    LocalsTable   = NULL;
    LocalsSize    = 0;
    LocalComdefsTable = NULL;
    LComdefsSize      = 0;
}

/*----------------------------------------------------------------------------*/

OMFFileReader :: ~OMFFileReader ()
{
    xfree (GroupsTable);
    xfree (LocalsTable);
    xfree (LocalComdefsTable);
}

/*----------------------------------------------------------------------------*/

static OMFFileReader * OMFReader;

void InitOMF  (void) { OMFReader = new OMFFileReader(); }
void ClearOMF (void) { delete OMFReader; }
void ReadOMF  (byte * rawdata, long size, char * filename) { OMFReader->Read(rawdata, size, filename); }

close_namespace

