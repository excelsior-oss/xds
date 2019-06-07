#include <string.h>
#include <stdio.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "idents.h"
#include "messages.h"
#include "xdebug.h"
#include "xmem.h"
#include "writer.h"
#include "writeeil.h"
#include "eil.h"

static Storage * LibImage;

static void AddImportLibItem(ident ImpName, word ordinal, byte type)
{
    LibImage->PutB(EIL_IMPORT);
    if (xUseOrdFlag && ordinal)
       LibImage->PutB(type | EIL_BY_ORDINAL);
    else
       LibImage->PutB(type);
    LibImage->Put2(NAMES.Index2StrLen(ImpName));
    LibImage->PutS(NAMES.Index2Str(ImpName), NAMES.Index2StrLen(ImpName));
    if (xUseOrdFlag && ordinal)
       LibImage->Put2(ordinal);
}

static void StartImpLib() {
    LibImage = new Storage(xFileAlign);
    LibImage->PutB(EIL_SIGNATURE);
    LibImage->PutB(EIL_DLLNAME);
    LibImage->PutName(StrippedOutputFileName);
}

static void EndImpLib(){
    LibImage->PutB(EIL_END);
}

static void WriteLibFile(char *name){
        FILE *fp;

        fp = fopen (name, "wb");
        if (fp == NULL)
                Message(xFATAL, msgUNABLE_TO_OPEN_FILE, name);
        if (fwrite (LibImage->Ptr, LibImage->Index, 1, fp) != 1) {
                fclose (fp);
                Message(xFATAL, msgUNABLE_TO_WRITE_FILE, name);
        }
        if (fclose (fp))
                Message(xFATAL, msgUNABLE_TO_WRITE_FILE, name);
}

extern void WriteEILImpLib()
{
    if (!StrippedOutputFileName) {
        StrippedOutputFileName = strrchr (xOutputFileName, '\\');
        if (!StrippedOutputFileName)
            StrippedOutputFileName = strrchr (xOutputFileName, '/');
        if (!StrippedOutputFileName)
            StrippedOutputFileName = xOutputFileName;
        else
            StrippedOutputFileName++;
    }

    StartImpLib();
    for (int n = 0; n < ILNumberOfExports; n ++) {
        Export * s = ILExportsTable[n];
        byte type = 0;
        if (s->flag & EFLAG_VARIABLE)
            type = EIL_TYPE_DATA;
        else
            type = EIL_TYPE_CODE;
        AddImportLibItem(s->extname, s->ordinal, type);
    }
    EndImpLib();
    WriteLibFile(xOutputLibName);
    xfree (ILExportsTable);
}

close_namespace

