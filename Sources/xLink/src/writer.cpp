
#include <stdio.h>

#include "xdefs.h"

open_namespace

#include "xos.h"
#include "xmem.h"
#include "idents.h"
#include "messages.h"
#include "struct.h"
#include "args.h"
#include "debug.h"

#include "xpe.h"
#include "writepe.h"
#include "writelx.h"
#include "writeelf.h"
#include "implib.h"
#include "writeeil.h"

#include "writer.h"

dword   StubSize;
byte  * Stub;

const char * StrippedOutputFileName =  NULL;

// Read Stub

void GetStub()
{
    if (xIMAGE_FORMAT == xELF_IMAGE_FORMAT)
       return;

    FILE *fp;
    if (xStubFileName) {
       fp = fopen (xStubFileName, "rb");
       if (fp == NULL)
          Message(xFATAL, msgUNABLE_TO_OPEN_FILE, xStubFileName);
       fseek (fp, 0L, SEEK_END);
       dword StubFileSize = ftell (fp);
       if (StubFileSize < sizeof (IMAGE_DOS_HEADER))
          Message(xFATAL, msgILLEGAL_STUB_FILE, xStubFileName);
       StubSize = (StubFileSize + 3) & ~ 3;
       Stub = (byte *) xalloc (StubSize);
       memset (Stub, 0, StubSize);
       fseek (fp, 0L, SEEK_SET);
       if (fread (Stub, 1, StubFileSize, fp) != StubFileSize)
          Message(xFATAL, msgUNABLE_TO_READ_FILE, xStubFileName);
       fclose (fp);
    } else {
       if (xIMAGE_FORMAT == xPE_IMAGE_FORMAT) {
          StubSize = DefaultPEStubSize;
          Stub     = DefaultPEStub;
       } else {
          StubSize = DefaultLXStubSize;
          Stub     = DefaultLXStub;
       }
    }
    PIMAGE_DOS_HEADER pDOS_HDR = (PIMAGE_DOS_HEADER) Stub;
    if (pDOS_HDR -> e_magic != IMAGE_DOS_SIGNATURE || pDOS_HDR -> e_lfanew)
       Message(xFATAL, msgILLEGAL_STUB_FILE, xStubFileName);
    pDOS_HDR -> e_lfanew = StubSize;
}

void WriteOutFile()
{
    if ((xIMAGE_FORMAT == xPE_IMAGE_FORMAT) || (xIMAGE_FORMAT == xLX_IMAGE_FORMAT)) {
        if ( !HasExtension(xOutputFileName) )
           xOutputFileName = MakeExtension (xOutputFileName, xDLLFlag ? "dll" : "exe");
    }

    OS -> RemoveFile (xOutputFileName);
//    remove (xOutputFileName);

    GetStub();

    if (xDoDebug)
        CheckDebugInfoFormat ();

    switch (xIMAGE_FORMAT) {
        case xPE_IMAGE_FORMAT:  WritePE (xOutputFileName);
                                break;

        case xLX_IMAGE_FORMAT:  WriteLX (xOutputFileName, xDoDebug, xDLLFlag);
                                break;

        case xELF_IMAGE_FORMAT: WriteELF (xOutputFileName);
                                break;

        default:
            ASSERT_FALSE();
    }

    if (xDLLFlag && xImpLibFlag)
       if (xEILImpLib)
          WriteEILImpLib();
       else
          WriteImpLib();
}

dword getOffset(dword vadr, word object)
{
    if (xIMAGE_FORMAT == xLX_IMAGE_FORMAT) {
       return LX_getOffset(vadr, object);
    } else if (xIMAGE_FORMAT == xPE_IMAGE_FORMAT) {
       return PE_getOffset(vadr, object);
    } else if (xIMAGE_FORMAT == xELF_IMAGE_FORMAT) {
       return ELF_getOffset(vadr, object);
    } else {
       ASSERT_FALSE();
       return 0;
    }
}

word getObjectNumber(dword vadr)
{
    if (xIMAGE_FORMAT == xLX_IMAGE_FORMAT) {
       return LX_getObjectNumber(vadr);
    } else if (xIMAGE_FORMAT == xPE_IMAGE_FORMAT) {
       return PE_getObjectNumber(vadr);
    } else if (xIMAGE_FORMAT == xELF_IMAGE_FORMAT) {
       return ELF_getObjectNumber(vadr);
    } else {
       ASSERT_FALSE();
       return 0;
    }
}

close_namespace

