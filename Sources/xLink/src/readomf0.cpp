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

/*----------------------------------------------------------------------------*/

void OMF0Reader::NewExpdef (ident intname, ident extname, word ordinal, Bool intname_present)
{
    if (!intname_present) {
        NewExport (extname, intname, NULL, 0, ordinal, EFLAG_SOURCE_OBJMOD, INVALID_ID);
    } else {
        struct expdef * new_expdef = (struct expdef *) xalloc(sizeof(struct expdef));
        new_expdef -> intname         = intname;
        new_expdef -> extname         = extname;
        new_expdef -> ordinal         = ordinal;
        new_expdef -> intname_present = intname_present;
        new_expdef -> next            = ExpdefsList;
        ExpdefsList                   = new_expdef;
    }
}

/*----------------------------------------------------------------------------*/

void OMF0Reader::ClearExpdefsList (void)
{
    struct expdef * e;
    while (ExpdefsList != NULL) {
        e = ExpdefsList;
        ExpdefsList = ExpdefsList -> next;
        xfree (e);
    }
}

/*----------------------------------------------------------------------------*/

void OMF0Reader::NewModule (void)
{
    NLNames =
    NPubNames =
    NSegs =
    0;

    ExtNamesTable.Clean ();

    LedataSeg = NULL;

    ClearExpdefsList();

    last_linnum = NULL;
}

/*----------------------------------------------------------------------------*/

void OMF0Reader::EndModule (void)
{
    if (ExpdefsList)
        CollectExpdefs ();

    last_linnum = NULL;
}

/*----------------------------------------------------------------------------*/

void OMF0Reader::CollectExpdefs (void)
{
    int              j;
    struct expdef  * p;
    struct pubname * q;
    Segment *     seg;
    dword            offset;
    Bool             found;

    for (p = ExpdefsList; p; p = p->next) {
        seg    = NULL;
        offset = 0;
        if (p -> intname_present) {
            found = false;
            for (j = NPubNames, q = PubNamesTable; j; q ++, j --)
                if (q -> name == p -> intname)
                    if (! (q -> kind & K_LOCAL)) {
                        found = true;
                        break;
                    } else if (q -> seg) {
                        seg          = q -> seg;
                        offset       = q -> offset;
                        p -> intname = p -> extname;
                        found = true;
                        break;
                    }
            if (!found) {
                Message(xERROR, msgINTERNAL_NAME_NOT_FOUND, GetCurrentOBJFileName (), NAMES.Index2Str (p -> intname));
                continue;
            }
        }
        NewExport(p->extname, p->intname, seg, offset, p->ordinal, EFLAG_SOURCE_OBJMOD, INVALID_ID);
    }
}

/*----------------------------------------------------------------------------*/

void OMF0Reader::lnames (char kind)
{
    struct lname * q;

    while (Available() > 0) {
        Grow (NLNames, LNamesSize, 32, LNamesTable, struct lname, q);
        q -> name = GetIdent ();
        q -> kind = kind;
    }
}

/*----------------------------------------------------------------------------*/

void OMF0Reader::ledata (void)
{
    Segment * s;
    int seg, len, offset, ind;

    seg = GetIndex ();
    LedataSeg = s = SegsTable [seg - 1];
    offset =GetWordOrDword ();
    LastOffset = offset;
    if (s -> overlay) {
        ind = GetWordOrDword ();
    }
    len = Available ();
    if (offset + len > s -> getLen())
        Message(xFATAL, msgTOO_MUCH_DATA_FOR_SEG, GetCurrentOBJFileName (), NAMES.Index2Str (s -> name));
    if (s -> clazz == BSS) {
        while (len) {
            if (* cur_pos ++)
                Message(xFATAL, msgCANNOT_INIT_BSS_SEG, GetCurrentOBJFileName (), NAMES.Index2Str (s -> name));
            len --;
        }
    } else {
        memcpy (s -> getText() + offset, cur_pos, len);
    }

   VerboseMessage (INFO_OMFREAD | INFO_XOMFREAD, "[X]OMF: LEDATA <seg %s len=%XH>, offs %XH len %XH\n",
                   NAMES.Index2Str (s -> name), s -> getLen(),
                   offset, len);
}

/*----------------------------------------------------------------------------*/

void OMF0Reader::lidata_block (int * offset, Segment * s)
{
    int    rep_count, block_count, len, b;

    rep_count   = GetWordOrDword ();
    block_count = GetWord ();
    if (block_count == 0) {
        len = GetByte ();
        if ((* offset + len) > s -> getLen())
            Message(xFATAL, msgTOO_MUCH_DATA_FOR_SEG, GetCurrentOBJFileName (), NAMES.Index2Str (s -> name));
        GetRawMem(s -> getText() + * offset, len);
        * offset += len;
    } else {
        dword  rec_idx_save = rec_idx;
        byte * cur_pos_save = cur_pos;
        while (rep_count > 0) {
            rec_idx = rec_idx_save;
            cur_pos = cur_pos_save;
            for (b = 0; b < block_count; b ++)
                lidata_block (offset, s);
            rep_count --;
        }
    }
}

void OMF0Reader::lidata (void)
{
    Segment * s;
    int seg, offset;

    seg = GetIndex ();
    s = SegsTable [seg - 1];
    if (s -> clazz == BSS)
        Message(xFATAL, msgCANNOT_INIT_BSS_SEG, GetCurrentOBJFileName (), NAMES.Index2Str (s -> name));
    LedataSeg = NULL;
    offset = GetWordOrDword ();
    while (Available() >= 4)
       lidata_block (& offset, s);
}

/*----------------------------------------------------------------------------*/

void OMF0Reader::linnum ()
{
    // group
    GetIndex ();

    int index = GetIndex ();
    Segment * seg = index ? SegsTable [index - 1] : NULL;

    struct linnum * linnum_rec = NULL;
    switch (DebugInfo_Format) {
        case DBG_FMT_NO:
        case DBG_FMT_EDIF:
            break;

        case DBG_FMT_HLL4:
            DebugFormat |= DBG_FMT_HLL4;

            if (!FirstHLL4Module)
                FirstHLL4Module = GetCurrentOBJFileName ();

            linnum_rec = (struct linnum *) xalloc(sizeof(struct linnum));

            if (last_linnum != NULL) {
                ASSERT (CurrentFile -> linnums != NULL);
                last_linnum -> next = linnum_rec;
            } else {
                ASSERT (CurrentFile -> linnums == NULL);
                CurrentFile -> linnums = linnum_rec;
            }
            last_linnum = linnum_rec;

            linnum_rec -> next   = NULL;
            linnum_rec -> length = Available ();
            linnum_rec -> seg    = seg;
            linnum_rec -> text   = (byte *) xalloc (linnum_rec -> length);
            GetRawMem (linnum_rec -> text, linnum_rec -> length);
            break;

        case DBG_FMT_NB99:

            DebugFormat |= DBG_FMT_NB99;

            if (!FirstNB99Module)
                FirstNB99Module = GetCurrentOBJFileName ();

            linnum_rec = (struct linnum *) xalloc(sizeof(struct linnum));

            if (last_linnum != NULL) {
                ASSERT (CurrentFile -> linnums != NULL);
                last_linnum -> next = linnum_rec;
            } else {
                ASSERT (CurrentFile -> linnums == NULL);
                CurrentFile -> linnums = linnum_rec;
            }
            last_linnum = linnum_rec;

            linnum_rec -> next   = NULL;
            linnum_rec -> length = Available ();
            linnum_rec -> seg    = seg;
            linnum_rec -> text   = (byte *) xalloc (linnum_rec -> length);
            GetRawMem (linnum_rec -> text, linnum_rec -> length);
            break;

        case DBG_FMT_CV:

            DebugFormat |= DBG_FMT_CV;

            if (!FirstCVModule)
                FirstCVModule = GetCurrentOBJFileName ();

            if (!seg)
                Message(xFATAL, msgNO_LINNUM_SEG, GetCurrentOBJFileName ());

            while (Available() > 0) {
                struct Line * q;
                Grow (seg -> nlines, seg -> linessize, 16, seg -> lines, struct Line, q);
                q -> line   = GetWord ();
                q -> offset = GetWordOrDword ();
            }
            break;

        default:
            ASSERT_FALSE ();
    }
}

/*----------------------------------------------------------------------------*/

OMF0Reader::OMF0Reader (void) :
    LNamesTable   (NULL),
    LNamesSize    (0),
    PubNamesTable (NULL),
    PubNamesSize  (0),
    SegsTable     (NULL),
    SegsSize      (0),
    ExpdefsList   (NULL)
{
    ExtNamesTable = GrowingArray<ident>(0, 32);
}

/*----------------------------------------------------------------------------*/

OMF0Reader::~OMF0Reader ()
{
    xfree (LNamesTable);
    xfree (PubNamesTable);
    xfree (SegsTable);

    ClearExpdefsList();
}

/*----------------------------------------------------------------------------*/

close_namespace

