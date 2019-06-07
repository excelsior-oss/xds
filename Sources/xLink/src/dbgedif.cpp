
#include <stdlib.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "xmem.h"
#include "writer.h"
#include "debug.h"
#include "dbgedif.h"
#include "xos.h"

struct debugFile {
    OBJFile * f;
    int segs;
};

void CreateEDIFDebugInfo (void) {

    char * EDIFileName = MakeExtension (xOutputFileName, "edi");
    remove (EDIFileName);

    FILE * EDIFile = fopen (EDIFileName, "wb");
    if (EDIFile == NULL)
        Message(xFATAL, msgUNABLE_TO_OPEN_FILE, EDIFileName);

    DebugInfo = newStorage(65536);

    dword timeStamp = time (0);

    DebugInfo->PutS ("EDIF", 4); /* Signature */
    DebugInfo->Put4 (timeStamp); /* ConsistentKey */

    /* Modules Record */

    DebugInfo->PutS ("$$MO", 4);

    dword recordLenOffs = DebugInfo->Index;
    DebugInfo->Put4 (0);                     // reserve space for record length

    int DebugFilesSize = 1024;
    struct debugFile * DebugFiles = (struct debugFile *) xalloc (sizeof(struct debugFile) * DebugFilesSize);
    int NDebugFiles = 0;

    OBJFile * ff = FileList;
    for (; ff; ff = ff->next) {
        if (ff -> dbgcomment == NULL)
            continue;
        Bool fileAdded = false;
        for (Segment * s = ff->segs; s; s = s->next)
            if (s -> isProcessed () && (s -> address > 0) && (s -> getLen() > 0)) {
                if (!fileAdded) {
                    // Add file to DebugFiles
                    if (NDebugFiles == DebugFilesSize) {
                        DebugFilesSize += DebugFilesSize;
                        DebugFiles = (struct debugFile *) xrealloc (DebugFiles,
                                                                    sizeof(struct debugFile) * NDebugFiles,
                                                                    sizeof(struct debugFile) * DebugFilesSize);
                    }
                    DebugFiles [NDebugFiles].f    = ff;
                    DebugFiles [NDebugFiles].segs = 0;
                    NDebugFiles++;
                    fileAdded = true;
                }
                DebugFiles [NDebugFiles - 1].segs++;
            }
    }

    DebugInfo->Put4 (NDebugFiles);

    for (int i = 0; i < NDebugFiles; i++) {
        ASSERT (DebugFiles[i].f -> dbgcomment != NULL);

        DebugInfo->PutS (DebugFiles[i].f -> dbgcomment, strlen (DebugFiles[i].f -> dbgcomment) + 1);
        DebugInfo->Align4 ();
        DebugInfo->Put4 (DebugFiles[i].segs);

        int segNum = 1;
        for (Segment * s = DebugFiles[i].f -> segs; s; s = s -> next) {
            if (s -> isProcessed () && (s -> address > 0) && (s -> getLen() > 0)) {
                word  obj = getObjectNumber (s -> address);
                int len = s -> getLen();
                if (s->link) {
                    ASSERT (s -> link -> isProcessed () && (s -> link -> address > 0));
                    len = ((s->address + s->getLen() + s->link->alignment-1) & ~(s->link->alignment-1)) - s->address;
                    ASSERT (len >= s->getLen());
                }
                DebugInfo->Put4 (segNum);
                DebugInfo->Put4 (obj);
                DebugInfo->Put4 (getOffset(s -> address, obj));
                DebugInfo->Put4 (len);
            }
            segNum++;
        }
    }
    xfree (DebugFiles);

    * ((dword *) (DebugInfo->Ptr + recordLenOffs)) = DebugInfo->Index - (recordLenOffs + 4);

    /* Publics Record */

    DebugInfo->PutS ("$$PU", 4);

    recordLenOffs = DebugInfo->Index;
    DebugInfo->Put4 (0);                     // reserve space for record length

    dword NPublics_offset = DebugInfo->Index;

    DebugInfo->Put4 (0);

    int NPublics = 0;
    for (ff = FileList; ff; ff = ff->next) {
        for (struct pub * p = ff -> publics; p; p = p->next) {
            if (p -> seg && (p -> seg -> isProcessed ()) && (p -> seg -> address > 0)) {
                dword vadr = p -> seg -> address + p -> offset;
                word  obj  = getObjectNumber (vadr);

                DebugInfo->PutS (NAMES.Index2Str (p -> name), NAMES.Index2StrLen (p -> name) + 1);
                DebugInfo->Align4 ();

                DebugInfo -> Put4 (obj);
                DebugInfo -> Put4(getOffset(vadr, obj));

                NPublics++;
            }
        }
    }

    *((int *) (DebugInfo -> Ptr + NPublics_offset)) = NPublics;

    * ((dword *) (DebugInfo->Ptr + recordLenOffs)) = DebugInfo->Index - (recordLenOffs + 4);

    DebugInfo->PutS ("EDIF", 4);              /* Signature  */
    DebugInfo->Put4 (DebugInfo->Index + 4);   /* BaseOffset */

    if (fwrite (DebugInfo->Ptr, DebugInfo->Index, 1, EDIFile) != 1) {
        fclose (EDIFile);
        Message(xFATAL, msgUNABLE_TO_WRITE_FILE, EDIFileName);
    }

    delete DebugInfo;

    DebugInfo = newStorage (16);
    DebugInfo->PutS ("EDIF", 4);              /* Signature */
    DebugInfo->Put4 (timeStamp);              /* ConsistentKey */


    char * fullEDIFFileName = OS -> GetFullPath (EDIFileName);

    DebugInfo->PutS (fullEDIFFileName, strlen (fullEDIFFileName) + 1);
    DebugInfo->Align4 ();

    DebugInfo->PutS ("EDIF", 4);              /* Signature  */
    DebugInfo->Put4 (DebugInfo->Index + 4);   /* BaseOffset */
}

close_namespace

