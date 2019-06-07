
#ifndef _ARGS_H_
#define _ARGS_H_

#define xSUBSYSTEM_FS  0
#define xSUBSYSTEM_CUI 1
#define xSUBSYSTEM_GUI 2

#define ENCRYPTION_KEY_MAX_VALUE 255
#define ENCRYPTION_KEY_NO_ENCRYPTION_VALUE 0
#define ENCRYPTION_KEY_UNDEFINED_VALUE 999

extern
Bool xDoDebug,
     xSmart,
     xFixed,
     xExtraAlignment,
     xDLLFlag,
     xCreateTimeDateStamps,
     xImpLibFlag,
     xUseOrdFlag,
     xDoMapFile,
     xMapByName,
     xWasOutputName,
     xUseShell,
     xNoEntryPoint,
     xWasEntryPoint,
     xWasImageBase,
     xWasStub,
     xVerbose,
     xEILImpLib,
     xWritableCodeSection,
     xStrictLinkToDLL,
     xMeasureTime,
     xNoExportNames,
     xNoConsistencyInfo,
     xSilent,
     xWriteLinkInfo,
     xReadLinkInfo,
     xSparse,
     xAddImportJumpsCodeviewPublic,
     xEmitNullCheckInfo,
     xUseProgramInterpreter,
     xLargeAddressAware,
     xNoLink,
     xJetComponent,
     xAutoImageBase,
     xEmitDynamicLookupTable,
     xEmitStackTraceInfo,
     xNoDynstrSizeCheck,
     xPrintSectionSizes,
     xWriteGCCImportLibrary,
     xSplitTypesTable;


extern
dword xImageBase,
      xObjectOffset,
      xFileAlign,
      xSystem,
      xStackSize,
      xStackCommit,
      xHeapSize,
      xHeapCommit,
      xOSmajor,
      xOSminor,
      xWarningLevel,
      xOptStrLevel,
      xVCode,
      xEncryptStrings;

extern char
     * xOutputFileName,
     * xOutputLibName,
     * xMapFileName,
     * xStubFileName,
     * xWriteLinkInfoFile,
     * xReadLinkInfoFile,
     * xProgramInterpreter,
     * xAutoImageBasePrevComponent,
     * xDynamicLookupTable,
     * xSectionSizesLog;

extern const char
     * xEntryPointFile;

extern
dword xIMAGE_FORMAT;

extern void InitOptions(void);
extern void ParseArgument(char *Arg);
extern void ParseCommandLine (const char* cmdLine, Bool skipZeroArg);

extern void ShowHelp (void);

extern const char * getDefine (const char *defName);
extern char * processDefsAndDup (const char *str);

//-----------------------------------------------------------------------------

#define JOB_READ_FILE             1
#define JOB_READ_FROM_RESOURCE    2

struct Job {
    int          kind;
    char       * parameter;
    struct Job * next;
    Bool         xSmart;                   // context-sensitive option
    Bool         xStrictLinkToDLL;         // context-sensitive option
};

extern struct Job * getNextJob ();

#endif
