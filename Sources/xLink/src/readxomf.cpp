#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

// for typestable.h
#include <set>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "debug.h"
#include "idents.h"
#include "messages.h"
#include "xdebug.h"
#include "xmem.h"
#include "typestable.h"

#include "xomf.h"

#include "readomf0.h"

/*----------------------------------------------------------------------------*/

class RawData {
  private:
    byte   kind;
    dword  elemCount;
    byte * data;
    void * object;

  public:
    RawData (byte _kind, dword _elemCount, byte * _data) :
        kind (_kind), elemCount (_elemCount), data (_data), object (0) {}

    byte getKind () {
        return kind;
    }

    JavaStringNode * getJavaString () {
        ASSERT (kind == DK_UNICODESTR);
        if (!object)
            object = NewJavaString (elemCount, (unichar *)data);
        return (JavaStringNode *) object;
    }

    ByteStringNode * getByteString () {
        ASSERT (kind == DK_BYTESTR);
        if (!object)
            object = NewByteString (elemCount, data);
        return (ByteStringNode *) object;
    }

};

/*----------------------------------------------------------------------------*/

class XOMFFileReader : public OMF0Reader {
  private:
    /* Raw Data Table */
    int NRawData;
    int RawDataTableSize;
    RawData ** RawDataTable;

    // Local types table for the current module
    LocalTypesTable* localTypesTable;

    void scanObjectModule (dword* objFileLen, dword* skipLen);

  protected:
    inline ident GetXIdent() {
        ident id = NAMES.Str2Index ((char *)(cur_pos + 2), * ((word *)cur_pos));
        rec_idx += (* ((word *)cur_pos)) + 2;
        cur_pos += (* ((word *)cur_pos)) + 2;
        return id;
    }

    void x_impdef   (void);
    void x_expdef   (void);
    void x_rawdata  (void);
    void x_fixup    (void);

    void extdef (void);
    void pubdef ();
    void coment (void);

    void header (void);
    void segdef (void);

    void NewModule (void);
    void EndModule (void);

  public:
    void ReadRecord (void);
    void Read (byte * rawdata, long size, char * filename);
    XOMFFileReader();
    ~XOMFFileReader();
};

/*----------------------------------------------------------------------------*/

void XOMFFileReader::NewModule (void)
{
    OMF0Reader :: NewModule ();

    NRawData = 0;
}

/*----------------------------------------------------------------------------*/

void XOMFFileReader::EndModule (void)
{
    OMF0Reader :: EndModule ();

    for (int i = 0; i < NRawData; i++) {
        delete RawDataTable [i];
        RawDataTable [i] = NULL;
    }
    NRawData = 0;
}

/*----------------------------------------------------------------------------*/

void XOMFFileReader::extdef ()
{
    while (Available() > 0) {
        ident id = GetXIdent ();
        ExtNamesTable.addElement (id);
        GetIndex (); // skip type index
        VerboseMessage (INFO_XOMFREAD, "XOMF: EXTDEF %s\n", NAMES.Index2Str(id));
    }
}

/*----------------------------------------------------------------------------*/

void XOMFFileReader::pubdef ()
{
    Segment * seg;

    int index = GetIndex ();
    if (index)
        seg = SegsTable [index - 1];
    else {
        Message (xERROR, msgINCORRECT_RECORD_FORMAT, filename, "PUBDEF", "base segment index is zero");
        return;
    }
    while (Available() > 0) {
        struct pubname * q;
        Grow (NPubNames, PubNamesSize, 32, PubNamesTable, struct pubname, q);
        q -> name   = GetXIdent ();
        q -> offset = GetDword ();
        int type    = GetByte ();
        int hash    = 0;
        if (type == XOMF_TYPE_TD) {
            hash = (int) GetDword ();
        }
        q -> seg    = seg;
        q -> group  = NULL;
        q -> kind   = K_PUBDEF;

        switch (type) {
            case XOMF_TYPE_DATA: NewPublicName (q -> name, q -> seg, q -> offset, T_DATA);
                                 break;

            case XOMF_TYPE_CODE: NewPublicName (q -> name, q -> seg, q -> offset, T_CODE);
                                 break;

            case XOMF_TYPE_TD:   NewPublicName (q -> name, q -> seg, q -> offset, T_TYPEDESC);
                                 localTypesTable->registerTypeEntry (q -> name, TDEntry_Exported, hash);
                                 break;

            case XOMF_TYPE_ATD:  NewPublicName (q -> name, q -> seg, q -> offset, T_TYPEDESC);
                                 localTypesTable->registerTypeEntry (q -> name, TDEntry_Absent);
                                 break;

            default:
                VerboseMessage ("type = %d\n", type);
                Message (xERROR, msgINCORRECT_RECORD_FORMAT, filename, "PUBDEF", "incorrect public symbol type");
                continue;
        }

        struct pub * Public = (struct pub *) allocateForever (sizeof(struct pub));
        Public -> next = CurrentFile -> publics;
        CurrentFile -> publics = Public;
        Public -> seg    = seg;
        Public -> name   = q -> name;
        Public -> offset = q -> offset;
    }
}

/*----------------------------------------------------------------------------*/

void XOMFFileReader::coment ()
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

    VerboseMessage (INFO_XOMFREAD, "XOMF: COMENT %d\n", rec);

    switch (rec) {
        case 0xA0:
            subrec = GetByte ();
            switch (subrec) {
                case 0x01:                      /* import definition */
                    ord = GetByte ();
                    name = GetXIdent ();
                    memcpy (buf, cur_pos + 1, * cur_pos);
                    buf [* cur_pos] = 0;
                    rec_idx += (* cur_pos) + 1;
                    cur_pos += (* cur_pos) + 1;

                    module = NAMES.Str2Index (ConvDLLName(buf));
                    if (ord) {
                        entry = /*FIXME*/(ident)GetWord ();
                        kind = K_IMPORT | K_BY_ORDINAL;
                    } else {
                        if (* cur_pos) {
                            entry = GetXIdent ();
                        } else {
                            entry = name;
                            cur_pos++;
                        }
                        kind = K_IMPORT;
                    }
                    if (ord)
                        VerboseMessage (INFO_XOMFREAD, "XOMF: IMPDEF %s = %d (%s)\n", NAMES.Index2Str(name), (int)entry, NAMES.Index2Str(module));
                    else
                        VerboseMessage (INFO_XOMFREAD, "XOMF: IMPDEF %s = %s (%s)\n", NAMES.Index2Str(name), NAMES.Index2Str(entry), NAMES.Index2Str(module));
                    r = dup(NAMES.Index2Str(name), NAMES.Index2StrLen(name));
                    if (check_imp_var_suffix(r)) {
                        imp_name = NAMES.Str2Index (r);
                        nameInfo * q = (nameInfo *) (NAMES.getInfo (imp_name));
                        if (q) {
                            if ((q -> kind & ~K_IMPFUNC) == (kind & ~K_IMPFUNC) &&
                               ((importNameInfo *)q) -> getModuleName() == module)
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
                    name = GetXIdent ();
                    intname_present = (*((word *)cur_pos) != 0) ? true : false;
                    if (intname_present) {
                          imp_name = GetXIdent ();
                    } else {
                          imp_name = name;
                          cur_pos ++;
                    }
                    ordinal = 0;
                    if (kind & 0x80)
                          ordinal = GetWord ();
                    VerboseMessage (INFO_XOMFREAD, "XOMF: EXPDEF %s = %s (%d) i=%d\n", NAMES.Index2Str(name), NAMES.Index2Str(imp_name), (int)ordinal, intname_present);
                    NewExpdef(imp_name, name, ordinal, intname_present);
            }
            break;
        case 0xA1:
            subrec = GetByte ();
            switch (subrec) {
                case 0x04:                      /* Debug format HLL 4 */
                    Message (xERROR, msgOBSOLETE_RECORD, "COMENT/A1/04", filename);
                    break;
                case 0x45:                      /* Excelsior's Debug Format NB99 (HLL4 with long names) */
                    Message (xERROR, msgOBSOLETE_RECORD, "COMENT/A1/45", filename);
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
            Message (xERROR, msgOBSOLETE_RECORD, "COMENT/MODNAM", filename);
            break;

        case FULNAM:
            Message (xERROR, msgOBSOLETE_RECORD, "COMENT/FULNAM", filename);
            break;

        case VERSTAMP:
            Message (xERROR, msgOBSOLETE_RECORD, "COMENT/VERSTAMP", filename);
            break;
    }
}

/*----------------------------------------------------------------------------*/

void XOMFFileReader::x_impdef (void)
{
    char * dllnameBuf = GetZString ( 4 /* size to reserve for .dll extension */);
    if (*dllnameBuf == 0) {
        Message (xERROR, msgINCORRECT_RECORD_FORMAT, filename, "XOMF_IMPDEF", "empty DLL name");
        return;
    }

    ident  dllname  = NAMES.Str2Index (ConvDLLName (dllnameBuf));
    int    hash     = (int) GetDword ();
    ident  name     = GetZStringIdent ();
    dword  nentries = GetDword ();

    struct JImportGroup * g = GetJImportGroup (dllname, hash, name, nentries);

    for (dword i = 0; i < nentries; i++) {
        dword itemIndex = GetDword ();
        byte  itemType  = GetByte ();
        if ((itemType != XOMF_TYPE_DATA) &&
            (itemType != XOMF_TYPE_CODE) &&
            (itemType != XOMF_TYPE_TD))
        {
            Message(xERROR, msgINCORRECT_RECORD_FORMAT, filename, "XOMF_IMPDEF", "invalid element type");
            return;
        }
        int type = 0;
        if (itemType == XOMF_TYPE_CODE)
            type = K_IMPFUNC;
        ident intname = GetZStringIdent ();
        NewJImportItem  (g, itemIndex, intname, type);
    }
}

/*----------------------------------------------------------------------------*/

void XOMFFileReader::x_expdef (void)
{
    int    hash     = (int) GetDword ();
    ident  name     = GetZStringIdent ();
    dword  nentries = GetDword ();

    int g = NewJExportGroup (hash, name, nentries);

    for (dword i = 0; i < nentries; i++) {
        ident intname = GetZStringIdent ();
        NewJExportItem  (g, i, intname);
    }
}

/*----------------------------------------------------------------------------*/

void XOMFFileReader::x_rawdata (void)
{
    byte   dataKind = GetByte ();
    dword  elemSize = 0;
    switch (dataKind) {
        case DK_BYTESTR:
            elemSize = 1;
            break;

        case DK_UNICODESTR:
            elemSize = 2;
            break;

        default:
            Message(xERROR, msgINCORRECT_RECORD_FORMAT, filename, "XOMF_RAWDATA", "invalid data kind");
            return;
    }
    dword elemCount = GetDword ();
    dword dataLen   = elemCount*elemSize;
    if (dataLen > (rec_len - rec_idx)) {
        Message(xERROR, msgINCORRECT_RECORD_FORMAT, filename, "XOMF_RAWDATA", "data length exceed record length");
        return;
    }
    RawData ** rawDataTableEntry;
    Grow (NRawData, RawDataTableSize, 32, RawDataTable, RawData *, rawDataTableEntry);
    * rawDataTableEntry = new RawData (dataKind, elemCount, cur_pos);

    VerboseMessage (INFO_XOMFREAD, "XOMF: RAWDATA kind = %d elemCnt = %d\n", dataKind, elemCount);

    if (IsPrintable (INFO_XOMFREAD)) {
        char * str = (char *) xalloc (elemCount + 1);
        for (dword i = 0; i < elemCount; i++)
          str [i] = (char) ((*(cur_pos + elemSize*i)) & 0x7F);
        str [elemCount] = '\0';
        VerboseMessage (INFO_XOMFREAD, "XOMF: RAWDATA: \"%s\"\n", str);
        xfree (str);
    }

    Advance (dataLen);
}

/*----------------------------------------------------------------------------*/

void XOMFFileReader::x_fixup (void)
{
    Segment * seg = LedataSeg;
    if (! seg) {
        Message(xERROR, msgFIXUP_WITHOUT_LEDATA, GetCurrentOBJFileName ());
        return;
    }
    if (seg && seg -> clazz == BSS) {
        Message(xERROR, msgFIXUP_IN_BSS_SEG, GetCurrentOBJFileName (), NAMES.Index2Str (seg -> name));
        return;
    }
    if (seg -> fixupssize == 0) {
        seg->allocateFixups(Available()/(1 + 4 + 1 + 4));
    }
    while (Available () > 0) {
        byte  fixupType    = GetByte  ();
        dword sourceOffset = GetDword ();
        byte  targetKind   = GetByte  ();
        dword targetObject = GetDword ();

        VerboseMessage (INFO_XOMFREAD, "XOMF: FIXUP %d SRC=%XH TRG <%d, OBJ %d>\n", fixupType, sourceOffset, targetKind, targetObject);

        byte fixupKind = 0;

        switch (fixupType) {
            case XOMF_FX_ADDR32:
                fixupKind = FIXUP_ADDRESS32;
                break;

            case XOMF_FX_OFFS32:
                fixupKind = FIXUP_SELFRELOFFS32;
                break;

            case XOMF_FX_FAR48:
                fixupKind = FIXUP_FAR16_32;
                break;

            case XOMF_FX_TDINDEX16:
                fixupKind = FIXUP_TDINDEX16;
                break;

            case XOMF_FX_TDINDEX32:
                fixupKind = FIXUP_TDINDEX32;
                break;

            case XOMF_FX_JSTR32:
                fixupKind = FIXUP_JAVASTRING32;
                break;

            case XOMF_FX_BYTESTR32:
                fixupKind = FIXUP_BYTESTRING32;
                break;

            case XOMF_FX_CONSTADDR32:
            case XOMF_FX_CONSTADDR32_EXTRA_THUNK:
                fixupKind = FIXUP_CONSTADDRESS32;
                break;

            default:
                Message(xERROR, msgINCORRECT_RECORD_FORMAT, filename, "XOMF_FIXUP", "invalid fixup type");
                return;
        }

        if (sourceOffset >= (dword)(seg->getLen())) {
            Message(xERROR, msgINCORRECT_RECORD_FORMAT, filename, "XOMF_FIXUP", "source offset exceed segment length");
            return;
        }

        byte k_target = 0;
        void * target = NULL;

        switch (targetKind) {
            case XOMF_TK_SEG:
                if (((fixupType != XOMF_FX_ADDR32) &&
                     (fixupType != XOMF_FX_OFFS32) &&
                     (fixupType != XOMF_FX_CONSTADDR32) &&
                     (fixupType != XOMF_FX_CONSTADDR32_EXTRA_THUNK) &&
                     (fixupType != XOMF_FX_FAR48)) || (targetObject == 0) || (targetObject > (dword)NSegs))
                {
                    VerboseMessage ("XOMF_FIXUP: XOMF_TK_SEG targetObject = %d; NSegs = %d\n", targetObject, NSegs);
                    Message(xERROR, msgINCORRECT_RECORD_FORMAT, filename, "XOMF_FIXUP", "illegal target");
                    return;
                }
                k_target = TK_SEG;
                target   = (void *) (SegsTable [targetObject-1]);
                break;

            case XOMF_TK_ID:
                {
                if (((fixupType != XOMF_FX_ADDR32) &&
                     (fixupType != XOMF_FX_OFFS32) &&
                     (fixupType != XOMF_FX_FAR48 ) &&
                     (fixupType != XOMF_FX_CONSTADDR32) &&
                     (fixupType != XOMF_FX_CONSTADDR32_EXTRA_THUNK) &&
                     (fixupType != XOMF_FX_TDINDEX16) &&
                     (fixupType != XOMF_FX_TDINDEX32)) ||
                    (targetObject == 0) ||
                    (targetObject > ExtNamesTable.getCount ()))
                {
                    VerboseMessage ("XOMF_FIXUP: XOMF_TK_ID targetObject = %d; EXTNAMEs = %d\n", targetObject, ExtNamesTable.getCount ());
                    Message(xERROR, msgINCORRECT_RECORD_FORMAT, filename, "XOMF_FIXUP", "illegal target");
                    return;
                }
                ident id = ExtNamesTable [targetObject-1];

                if (id == LOCAL_TYPES_TABLE) {
                    id = localTypesTable->getTableName();
                    localTypesTable->setTableReferenced();
                }

                k_target = TK_ID;
                target   = (void *) id;

                if ((fixupType == XOMF_FX_TDINDEX16) || (fixupType == XOMF_FX_TDINDEX32)) {
                    const char* fixupTypeStr = (fixupType == XOMF_FX_TDINDEX16) ? "XOMF_FX_TDINDEX16" : "XOMF_FX_TDINDEX32";
                    VerboseMessage (INFO_XOMFREAD, "XOMF: %s fixup to %s\n", fixupTypeStr, NAMES.Index2Str (id));

                    localTypesTable->registerTypeEntry (id, TDEntry_Unknown);
                }
                }
                break;

            case XOMF_TK_RAWDATA:
                {
                if (((fixupType != XOMF_FX_JSTR32) &&
                     (fixupType != XOMF_FX_BYTESTR32)) || (targetObject == 0) || (targetObject > (dword)RawDataTableSize))
                {
                    VerboseMessage ("XOMF_FIXUP: XOMF_TK_RAWDATA targetObject = %d; RAWDATAs = %d\n", targetObject, RawDataTableSize);
                    Message(xERROR, msgINCORRECT_RECORD_FORMAT, filename, "XOMF_FIXUP", "illegal target");
                    return;
                }
                RawData * data = RawDataTable [targetObject-1];
                if (fixupType == XOMF_FX_JSTR32) {
                    if (data->getKind () != DK_UNICODESTR) {
                        Message(xERROR, msgINCORRECT_RECORD_FORMAT, filename, "XOMF_FIXUP", "unicode string data kind expected");
                        return;
                    }
                    k_target = TK_TARGET32;
                    target   = (void *) (data->getJavaString());
                } else {
                    // fixupType == XOMF_FX_BYTESTR
                    if (data->getKind () != DK_BYTESTR) {
                        Message(xERROR, msgINCORRECT_RECORD_FORMAT, filename, "XOMF_FIXUP", "byte string data kind expected");
                        return;
                    }
                    k_target = TK_TARGET32;
                    target   = (void *) (data->getByteString());
                }
                }
                break;

            default:
                VerboseMessage ("XOMF_FIXUP: targetKind = %d\n", targetKind);
                Message(xERROR, msgINCORRECT_RECORD_FORMAT, filename, "XOMF_FIXUP", "invalid target kind");
                return;
        }

        if (fixupType == XOMF_FX_CONSTADDR32_EXTRA_THUNK) {
            if ((k_target != TK_ID) && (k_target != TK_SEG)) {
                Message(xERROR, msgINCORRECT_RECORD_FORMAT, filename, "XOMF_FIXUP", "invalid target kind for extra thunk");
            }
            ident thunkId = newExtraThunk (k_target, target);

            k_target = TK_ID;
            target   = (void *) thunkId;
        }

        addFixup (seg, fixupKind, sourceOffset, k_target, target, 0);
    }
}

/*----------------------------------------------------------------------------*/

/*
 * Scan the object module to determine its length and the position of
 * XOMF_OBJEND record
 */
void XOMFFileReader::scanObjectModule (dword* objFileLen, dword* skipLen)
{
    byte * objFileData = rec_start;

    *objFileLen = 0;
    *skipLen = 0;

    byte * rec = rec_start;
    dword rec_len = 0;
    for (;;)
    {
        rec_len = 1 + 4 + *((dword *)(rec + 1));
        *objFileLen += rec_len;
        if (*objFileLen > (dword)size) {
            Message(xFATAL, msgRECORD_TOO_LONG, filename);
        }

        if (*rec == XOMF_OBJEND)
            break;

        rec      += rec_len;
        *skipLen += rec_len;
    }
    ASSERT (*objFileLen <= size);
}


void XOMFFileReader::header (void)
{
    byte   commonFlag     = GetByte ();
    byte   moduleType     = GetByte ();
    char * sourceFileName = GetZString ();
    char * moduleName     = GetZString ();
    char * uidName        = GetZString ();
    dword versionStamp    = GetDword ();
    byte  debugInfoFmt    = GetByte ();

    byte * commonData = NULL;
    dword commonLen = 0;

    switch (moduleType) {
        case XOMF_MODULE_NORMAL:
            break;

        case XOMF_MODULE_STACKTRACEINFO:
            if (!xEmitStackTraceInfo) {
                // Skip this object module

                dword objFileLen = 0;
                dword skipLen = 0;
                scanObjectModule (&objFileLen, &skipLen);

                // skip the object module data up to OBJEND record
                rec_len = skipLen - (1 + 4 + 1);

                return;
            }
            break;

        default:
            Message (xERROR, msgINCORRECT_RECORD_FORMAT, filename, "HEADER", "unknown object module type");
    }

    if (commonFlag != 0) {
        if ((uidName == NULL) || (*uidName == 0)) {
            Message (xERROR, msgINCORRECT_RECORD_FORMAT, filename, "HEADER", "no UID Name for Common object module");
            return;
        }

        byte * objFileData = rec_start;
        dword objFileLen = 0;
        dword skipLen = 0;

        // Scan the object file to determine its length
        scanObjectModule (&objFileLen, &skipLen);

        OBJFile * file = FileList->findFileByUIDName (uidName);
        if (file != NULL) {
            // skip the object module data up to OBJEND record
            rec_len = skipLen - (1 + 4 + 1);

            // check the files for coincidence
            if (! file->isCommon()) {
                Message (xERROR, msgINCORRECT_RECORD_FORMAT, filename, "HEADER", "UID Name of Common object module was used in non-common object module");
                return;
            }

            const byte *objFileData2 = file->getCommonData();
            dword objFileLen2  = file->getCommonLen();

            if ((objFileLen != objFileLen2) ||
                memcmp (objFileData, objFileData2, objFileLen))
            {
                Message (xERROR, msgCOMMON_OBJ_FILES_NOT_MATCH, filename, file->getFilename());
                return;
            }

            return;
        }

        commonData = objFileData;
        commonLen  = objFileLen;
    }

    new OBJFile (sourceFileName, libname);

    if (commonFlag) {
        ASSERT (commonData != NULL);
        ASSERT (commonLen  != 0);

        CurrentFile -> setCommon (commonData, commonLen);
    }

    CurrentFile -> setSource (sourceFileName);

    if (moduleName && (*moduleName))
        CurrentFile -> setFilename (moduleName);

    if (uidName && (*uidName)) {
        CurrentFile -> setUIDName (uidName);
    } else if (moduleName && (*moduleName)) {
        CurrentFile -> setUIDName (moduleName);
    } else {
        CurrentFile -> setUIDName (CurrentFile -> getFilename());
    }

    ASSERT (VerStamp != NULL);
    VerStamp->setVersionStamp (versionStamp);

    switch (debugInfoFmt) {
        case XOMF_DEBUG_FORMAT_NO:
            DebugInfo_Format = DBG_FMT_NO;
            break;

        case XOMF_DEBUG_FORMAT_CV:
            DebugInfo_Format = DBG_FMT_CV;
            DebugFormat |= DBG_FMT_CV;

            if (!FirstCVModule)
                FirstCVModule = GetCurrentOBJFileName ();
            break;

        case XOMF_DEBUG_FORMAT_HLL4:
            DebugInfo_Format = DBG_FMT_HLL4;
            DebugFormat |= DBG_FMT_HLL4;

            if (!FirstHLL4Module)
                FirstHLL4Module = GetCurrentOBJFileName ();
            break;

        case XOMF_DEBUG_FORMAT_NB99:
            DebugInfo_Format = DBG_FMT_NB99;
            DebugFormat |= DBG_FMT_NB99;

            if (!FirstNB99Module)
                FirstNB99Module = GetCurrentOBJFileName ();
            break;

        case XOMF_DEBUG_FORMAT_EDIF:
            DebugInfo_Format = DBG_FMT_EDIF;
            DebugFormat |= DBG_FMT_EDIF;
            if (!FirstEDIFModule)
                FirstEDIFModule = GetCurrentOBJFileName ();
            break;

        default:
            Message (xERROR, msgINCORRECT_RECORD_FORMAT, filename, "HEADER", "unknown debug info format");
    }
}

/*----------------------------------------------------------------------------*/

void XOMFFileReader::segdef (void)
{
    ident name = GetZStringIdent ();
    if (name == EMPTY_ID)
        name = INVALID_ID;

    ident groupname = INVALID_ID;
    int index = GetIndex ();
    if (index)
        groupname = getLName (index);
    ASSERT (groupname != EMPTY_ID);

    byte  class_code = GetByte ();
    dword length     = GetDword ();
    byte  align_code = GetByte ();

    ident clazz = INVALID_ID;
    switch (class_code) {
        case SEGCLASS_CODE:        clazz = CODE;  break;
        case SEGCLASS_DATA:        clazz = DATA;  break;
        case SEGCLASS_BSS:         clazz = BSS;   break;
        case SEGCLASS_RODATA:      clazz = CONST; break;
        case SEGCLASS_DEBUG:       clazz = DEBUG; break;
        case SEGCLASS_NULLCHECKS:  clazz = NULLCHECKS; break;
        case SEGCLASS_STACKTRACE:  clazz = STACKTRACE; break;
        default:
            Message (xERROR, msgINCORRECT_RECORD_FORMAT, filename, "SEGDEF", "incorrect segment class");
            return;
    }

    if (groupname == INVALID_ID)
        groupname = clazz;

    if (name == INVALID_ID)
        name = groupname;

    int alignment = 0;
    switch (align_code) {
        case SEGALIGN_1:    alignment = 1;    break;
        case SEGALIGN_2:    alignment = 2;    break;
        case SEGALIGN_4:    alignment = 4;    break;
        case SEGALIGN_8:    alignment = 8;    break;
        case SEGALIGN_16:   alignment = 16;   break;
        case SEGALIGN_4096: alignment = 4096; break;
        default:
            Message (xERROR, msgINCORRECT_RECORD_FORMAT, filename, "SEGDEF", "incorrect segment alignment");
            return;
    }

    Segment * q = new Segment (name, groupname, clazz, false, length, alignment, localTypesTable);

    Segment ** w;
    Grow (NSegs, SegsSize, 32, SegsTable, Segment *, w);
    * w = q;

    VerboseMessage (INFO_XOMFREAD, "XOMF: SEGDEF %s grp=%s class=%s align=%d len=%XH\n",
                    NAMES.Index2Str (name), NAMES.Index2Str (groupname), NAMES.Index2Str (clazz), alignment, length);
}

/*----------------------------------------------------------------------------*/

void XOMFFileReader::ReadRecord (void)
{
    if (size < 3) {
        VerboseMessage ("file %s\n", filename);
        VerboseMessage ("pos = %d; size = %d, last rec_type = %xh\n", cur_pos, size, rec_type);
        Message(xFATAL, msgILLEGAL_RECORD_LENGTH);
    }

    rec_start = cur_pos;
    rec_type  = GetByte();
    rec_len   = GetDword() - 1;
    rec_idx   = 0;
    if (rec_len > (dword)size)
        Message(xFATAL, msgRECORD_TOO_LONG, filename);
    switch (rec_type) {
        case XOMF_HEADER:
            header ();
            break;

        case XOMF_LIBHDR:
            LibPageSize = rec_len + 1 /* REC TYPE */ + 4 /* REC LEN */ + 1 /* CHKSUM */;
            break;

        case LIBEND:
            size = 0;
            return;

        case XOMF_THEADR:
        case LHEADR:
            Message (xERROR, msgOBSOLETE_RECORD, "XOMF_THEADR/LHEADR", filename);
            break;

        case LNAMES:
            lnames (0);
            break;

        case LLNAMES:
            Message (xERROR, msgOBSOLETE_RECORD, "LLNAMES", filename);
            break;

        case SEGDEF:
        case SEGDEF | REC32:
            Message (xERROR, msgOBSOLETE_RECORD, "SEGDEF", filename);
            break;

        case XOMF_SEGDEF:
            segdef ();
            break;

        case GRPDEF:
            Message (xERROR, msgOBSOLETE_RECORD, "GRPDEF", filename);
            break;

        case EXTDEF:
        case LEXTDEF:
            extdef ();
            break;

        case COMDEF:
            Message (xERROR, msgOBSOLETE_RECORD, "COMDEF", filename);
            break;

        case LCOMDEF:
            Message (xERROR, msgOBSOLETE_RECORD, "LCOMDEF", filename);
            break;

        case PUBDEF:
        case PUBDEF | REC32:
            pubdef ();
            break;

        case LPUBDEF:
        case LPUBDEF | REC32:
            Message (xERROR, msgOBSOLETE_RECORD, "LPUBDEF", filename);
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
            Message (xERROR, msgOBSOLETE_RECORD, "FIXUPP", filename);
            break;

        case COMENT:
            coment ();
            break;

        case MODEND:
        case MODEND | REC32:
            Message (xERROR, msgOBSOLETE_RECORD, "MODEND", filename);

        case XOMF_OBJEND:
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
            Message (xERROR, msgOBSOLETE_RECORD, "TYPDEF", filename);
            break;

        case XOMF_JSTRINGDESC:
            Message (xERROR, msgOBSOLETE_RECORD, "XOMF_JSTRINGDESC", filename);
            break;

        case XOMF_EXPDEF:
            x_expdef ();
            break;

        case XOMF_IMPDEF:
            x_impdef ();
            break;

        case XOMF_RAWDATA:
            x_rawdata ();
            break;

        case XOMF_FIXUP:
            x_fixup ();
            break;

        default:
            Message (xFATAL, msgUNKNOWN_RECORD_TYPE, filename, rec_type);
    }
    cur_pos = rec_start + (rec_len + 1 /* REC TYPE */ + 4 /* REC LEN */ + 1 /* CHKSUM */);
    size    = size      - (rec_len + 1 /* REC TYPE */ + 4 /* REC LEN */ + 1 /* CHKSUM */);
    return;
}

/*----------------------------------------------------------------------------*/
void XOMFFileReader::Read (byte * data, long datasize, char * fname)
{
    VerboseMessage (INFO_XOMFREAD, "XOMF: Reading File %s\n", fname);

    filename           = fname;
    rawdata  = cur_pos = data;
    size               = datasize;

    localTypesTable = newLocalTypesTable(filename);

    do {
        if (size < 8) {
            Message(xERROR, msgILLEGAL_FILE_FORMAT, filename);
            return;
        }

        dword formatSignature = GetDword ();
        dword formatVersion   = GetDword ();
        size -= 8;

        if (formatVersion != XOMF_FORMAT_VERSION) {
            Message(xERROR, msgUNSUPPORTED_XOMF_VERSION, filename, formatVersion, XOMF_FORMAT_VERSION);
            return;
        }

        DebugInfo_Format = DBG_FMT_NO;

        NewModule ();

        libname = NULL;

        do {
            ReadRecord ();
        } while (rec_type != XOMF_OBJEND);
    } while (size > 0);

    localTypesTable = NULL;
}

/*----------------------------------------------------------------------------*/

XOMFFileReader::XOMFFileReader (void)
{
    NRawData = 0;
    RawDataTableSize = 0;
    RawDataTable = NULL;
    localTypesTable = NULL;
}

/*----------------------------------------------------------------------------*/

XOMFFileReader::~XOMFFileReader ()
{
    xfree (RawDataTable);
}

/*----------------------------------------------------------------------------*/

static XOMFFileReader * XOMFReader;

void InitXOMF  (void) { XOMFReader = new XOMFFileReader(); }
void ClearXOMF (void) { delete XOMFReader; }
void ReadXOMF  (byte * rawdata, long size, char * filename) { XOMFReader->Read(rawdata, size, filename); }

close_namespace

