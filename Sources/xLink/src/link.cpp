#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "xdefs.h"

open_namespace

#include "xos.h"
#include "struct.h"

#include "xshell.h"
#include "idents.h"
#include "messages.h"
#include "xdebug.h"
#include "xmem.h"

#include "reader.h"
#include "writer.h"

/*----------------------------------------------------------------------------*/
/*                    Final statistics printing                               */
/*----------------------------------------------------------------------------*/

int numberOfNames = 0;

void CalcNames (ident, void *info) {
    if (info)
        numberOfNames++;
}

static void OutStatistics (void)
{
    switch (TotalErrors) {
        case 0: 
            printf ("No errors, ");
            break;
        case 1:
            printf ("1 error, ");
            break;
        default:
            printf ("%d errors, ", TotalErrors);
            break;
    }
    switch (TotalWarnings) {
        case 0:
            puts ("no warnings");
            break;
        case 1:
            puts ("1 warning");
            break;
        default:
            printf ("%d warnings\n", TotalWarnings);
            break;
    }
    NAMES.Iterate (CalcNames);
    VerboseMessage ("TOTAL: %d idents, %d names\n", NAMES.getTableSize(), numberOfNames);
}

/*----------------------------------------------------------------------------*/
/*                             MAIN                                           */
/*----------------------------------------------------------------------------*/

void ShowLogo (void) {
    fflush (stdout);
    /* This comment here is required for buildsystem: -+-FixPoint-+- */
    puts ("\nXDS Link Version 2.11.19 Copyright (c) Excelsior 1995-2008.");
    fflush (stdout);
}

void Init (void) {
    InitMem ();
    InitIds ();
    InitOptions();
    InitializeReaders();
    InitIR();
}

int Link (void) {
    if (xNoLink) {
        if (xUseShell) DisconnectShell ();
        return 0;
    }

    clock_t startTime;

    startTime = startTimer ();
    for (struct Job * j = getNextJob (); j; j = getNextJob ()) {
        switch (j -> kind) {
            case JOB_READ_FILE:
                ReadFileParameter (j -> parameter);
                break;

            case JOB_READ_FROM_RESOURCE:
                ReadResourceParameter (j -> parameter);
                break;

            default:
                ASSERT_FALSE ();
        }
    }
    FinalizeReaders();
    stopTimer ("TIME: Reading    %dm %ds %d/%d\n", startTime);

    if (TotalErrors == 0) {
        if ((xOutputFileName == NULL) || (*xOutputFileName == 0))
            Message(xFATAL, msgNO_FILE_SPECIFIED);
        if (xOutputLibName == NULL)
            xOutputLibName = MakeExtension (xOutputFileName, "lib");
        CurrentFile = NULL;
        xWasEntryPoint = !xNoEntryPoint && xWasEntryPoint;
        if (!xWasEntryPoint && !( xDLLFlag && ((Exports != NULL) || (numberOfResourceDirectories != 0))))
            Message(xFATAL, xDLLFlag ? msgNO_DLL_ENTRY : msgNO_PROG_ENTRY);

        startTime = startTimer ();
        ProcessIR();
        stopTimer ("TIME: Processing %dm %ds %d/%d\n", startTime);

        if (TotalErrors == 0) {
            if (xDoMapFile)
                CreateMapFile (xMapFileName ? 
                               HasExtension (xMapFileName) ? xMapFileName :
                               MakeExtension (xMapFileName,    "map") :
                               MakeExtension (xOutputFileName, "map"));

            if (xWriteLinkInfo)
                WriteLinkInfo ();

            startTime = startTimer ();
            WriteOutFile ();
            stopTimer ("TIME: Write out  %dm %ds %d/%d\n", startTime);
        }
    }
    if (!xSilent)
        OutStatistics ();
    VerboseMessage (INFO_MEMUSAGE, "AF Memory allocated: %d,  memory used: %d,  gap: %d\n", AFBusyMem, AFUsedMem, AFGapMem);
    if (xUseShell)
        DisconnectShell ();
    return TotalErrors ? 255 : 0;
}

int XCDECL main0 (int argc, char * argv [])
{
    initOS ();
    ShowLogo ();
    if (argc == 1) {
        ShowHelp ();
        return 0;
    }
    int jmp_code = setjmp (ABORT_JMP_BUF);
    if (!jmp_code) {
        Init ();

        const char *cmdLine = OS->GetCmdLine();
        if (cmdLine != NULL) {
            ParseCommandLine (cmdLine, true);
        } else {
            for (int i = 1; i < argc; i++) {
                ParseArgument(argv[i]);
            }
        }

        return Link();
    } else {
        return 255;
    }
}

char * ErrorMessage = NULL;


#ifdef __cplusplus
extern "C" {
#endif

int XEXPORT InvokeXLink (char *cmdLine, int isSilent) {
    int errcode = 0;

    initOS ();
    xSilent = (Bool) isSilent;
    if (!xSilent)
        ShowLogo ();
    if ((cmdLine == NULL) || (*cmdLine == '\0')) {
        if (!xSilent)
            ShowHelp ();
    } else {
        int jmp_code = setjmp (ABORT_JMP_BUF);
        if (!jmp_code) {
            //
            Init ();
            ParseCommandLine(cmdLine, false);
            int link_code = Link();
            //
            if (link_code != 0)
               ErrorMessage = ERROR_MESSAGE_BUF;
            errcode = link_code;
        } else {
            if (jmp_code == ERROR_EXIT)
               ErrorMessage = ERROR_MESSAGE_BUF;
            else
               ErrorMessage = ASSERT_MESSAGE_BUF;
            errcode = 255;
        }
    }

#if defined(xos_LINUX) && defined(SHARED_LIBRARY)
    // looks like gcc 3.2 doesn't support the __attribute__((destructor))
    // stuff for C++ code, so we are forced to free the memory manually
    free_heap();
#endif    

    return errcode;
}

typedef char * errmsg;
errmsg XEXPORT GetErrorMessage () {
    return ErrorMessage;
}

#ifdef __cplusplus
} // "C"
#endif

close_namespace

int XCDECL main (int argc, char * argv [])
{
    return from_namespace main0(argc, argv);
}
