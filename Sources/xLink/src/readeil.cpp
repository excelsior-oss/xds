#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "idents.h"
#include "messages.h"
#include "xdebug.h"
#include "readeil.h"

extern void ReadEIL (byte * rawdata, long size, char * /*filename*/)
{
    byte *cur_rec = rawdata;
    ident dllname_id = INVALID_ID;
    char  dllname[256];
    Bool  strict_link_to_dll;

    cur_rec++;                      // skip signature
    while (*cur_rec != EIL_END)
    {
        ASSERT (cur_rec - rawdata <= size);
        switch (*cur_rec) {
            case EIL_DLLNAME:
                {
                    memcpy(dllname, cur_rec + 2, *(cur_rec + 1));
                    dllname[*(cur_rec + 1)] = '\0';
                    cur_rec += *(cur_rec + 1) + 1 + 1;

                    dllname_id = NAMES.Str2Index (ConvDLLName (dllname));
                    strict_link_to_dll = xStrictLinkToDLL;
                }
                break;

            case EIL_IMPORT:
                {
                    ASSERT (dllname_id != INVALID_ID);
                    byte type = *(cur_rec + 1);
                    ident name = NAMES.Str2Index ((char *)(cur_rec + 4), *(word *)(cur_rec + 2));
                    cur_rec += *(word *)(cur_rec + 2) + 1 + 1 + 2;
                    int entry = 0;
                    int kind  = K_IMPORT;

                    if (type & EIL_BY_ORDINAL) {
                        entry = *(word *)cur_rec;
                        cur_rec += 2;
                        kind |= K_BY_ORDINAL;
                    } else
                        entry = /*FIXME*/(int)name;

                    if (type & EIL_TYPE_CODE)
                        kind |= K_IMPFUNC;

                    importNameInfo * nm = (importNameInfo *) (NAMES.getInfo (name));
                    if (nm)
                        VerboseMessage (INFO_EILREAD, "EILRead: %s(%s %d) in conflict with local\n", NAMES.Index2Str(name), NAMES.Index2Str(dllname_id), entry);
                    else {
                        VerboseMessage (INFO_EILREAD, "EILRead: %s(%s %d) TYPE %d\n", NAMES.Index2Str(name), NAMES.Index2Str(dllname_id), entry, type);
                        nm = NewImportName  (name, kind, dllname_id, entry);
                        if (nm && strict_link_to_dll) {
                            nm -> kind = nm -> kind | K_USED;
                            strict_link_to_dll = false;
                        }
                    }
                }
                break;
            default:
                ASSERT_FALSE();
                break;
        }
    }
}

close_namespace

