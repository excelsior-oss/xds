<*+ M2EXTENSIONS *>
--------------------------------------------------------------------------------
--                      Excelsior XDS Test Coverage System
--                          (c) 2015, Excelsior Ltd.
-- Module:       xrTCSx86
-- Mission:  Test Coverage Run-Time Support
-- Synonym:  tc
-- Authors:  Lvov Konstantin
-- Created:  24-Jul-2015
--               
-- Modula-2 definition module should be always provided with implementation module. 
-- So, stub implementation of run-time functions are provided. Real implementation
-- are not required in compile time.                       
--------------------------------------------------------------------------------
<* ALIGNMENT = '8' *> 
IMPLEMENTATION MODULE xrTCSx86;

IMPORT sys := SYSTEM;

<* PUSH *>
<* WOFF301+ *>
<* WOFF311+ *>

PROCEDURE InitIterationCounter ( index_0: TIndex
                               ; VAR local_counter: TCounter
                               ; VAR counters: ARRAY OF TCounter
                               );
BEGIN
  ASSERT(FALSE);
END InitIterationCounter;

--------------------------------------------------------------------------------
PROCEDURE IncreaseIterationCounter ( index_0: TIndex
                                   ; index_1: TIndex
                                   ; index_N: TIndex
                                   ; VAR local_counter: TCounter
                                   ; VAR counters: ARRAY OF TCounter
                                   );
BEGIN
  ASSERT(FALSE);
END IncreaseIterationCounter;


--------------------------------------------------------------------------------
PROCEDURE InitRecursionDepth ( VAR depth : TCounter
                             ; VAR was_return: BOOLEAN );
BEGIN
  ASSERT(FALSE);
END InitRecursionDepth;

--------------------------------------------------------------------------------
PROCEDURE IncreaseRecursionDepth ( index_0: TIndex
                                 ; index_1: TIndex
                                 ; index_N: TIndex
                                 ; VAR depth: TCounter
                                 ; VAR was_return: BOOLEAN
                                 ; VAR counters: ARRAY OF TCounter
                                 );
BEGIN
  ASSERT(FALSE);
END IncreaseRecursionDepth;

--------------------------------------------------------------------------------
PROCEDURE DecreaseRecursionDepth ( VAR depth: TCounter
                                 ; VAR was_return: BOOLEAN
                                 );
BEGIN
  ASSERT(FALSE);
END DecreaseRecursionDepth;

--------------------------------------------------------------------------------
PROCEDURE DecreaseVariableValue (VAR value: TCounter);
BEGIN
  ASSERT(FALSE);
END DecreaseVariableValue;

--------------------------------------------------------------------------------
PROCEDURE IncreaseConditionCounter ( condition: BOOLEAN
                                   ; index_true: TIndex
                                   ; index_false: TIndex
                                   ; VAR counters: ARRAY OF TCounter
                                   ): BOOLEAN;
BEGIN
  ASSERT(FALSE);
  RETURN FALSE;
END IncreaseConditionCounter;


--------------------------------------------------------------------------------
PROCEDURE IncreaseRelationCounterSigned ( relation: TRelation
                                        ; arg_left: TSignedArgument
                                        ; arg_right: TSignedArgument
                                        ; index: TIndex
                                        ; VAR counters: ARRAY OF TCounter
                                        ): BOOLEAN;
BEGIN
  ASSERT(FALSE);
  RETURN FALSE;
END IncreaseRelationCounterSigned;

--------------------------------------------------------------------------------
PROCEDURE IncreaseRelationCounterUnsigned ( relation: TRelation
                                          ; arg_left: TUnsignedArgument
                                          ; arg_right: TUnsignedArgument
                                          ; index: TIndex
                                          ; VAR counters: ARRAY OF TCounter
                                          ): BOOLEAN;
BEGIN
  ASSERT(FALSE);
  RETURN FALSE;
END IncreaseRelationCounterUnsigned;

--------------------------------------------------------------------------------
PROCEDURE IncreaseRelationCounter ( condition: BOOLEAN
                                  ; equals: BOOLEAN
                                  ; index: TIndex
                                  ; VAR counters: ARRAY OF TCounter
                                  ): BOOLEAN;
BEGIN
  ASSERT(FALSE);
  RETURN FALSE;
END IncreaseRelationCounter;

--------------------------------------------------------------------------------
PROCEDURE IncreaseCounter ( index: TIndex
                          ; VAR counters: ARRAY OF TCounter
                          );
BEGIN
  ASSERT(FALSE);
END IncreaseCounter;

--------------------------------------------------------------------------------
PROCEDURE IncreaseCounterExt ( index: TIndex
                             ; Init: PROC
                             ; VAR counters: ARRAY OF TCounter
                             );
BEGIN
  ASSERT(FALSE);
END IncreaseCounterExt;


--------------------------------------------------------------------------------
PROCEDURE RegistryModule ( module:    TModuleInfoPtr
                         ; modtime:   TTime
                         ; crcsum:    TCRCSum
                         ; modname:   ARRAY OF CHAR
                         ; procnames: ARRAY OF CHAR
                         ; srcrefs:   ARRAY OF TSourceRef
                         ; counters:  ARRAY OF TCounter
                         );
BEGIN
  ASSERT(FALSE);
END RegistryModule;


--------------------------------------------------------------------------------
PROCEDURE RegistryModuleDynamic ( module:     TModuleInfoPtr
                                ; modtime:    TTime
                                ; crcsum:     TCRCSum
                                ; modname:    ARRAY OF CHAR
                                ; procnames:  ARRAY OF CHAR
                                ; srcrefs:    ARRAY OF TSourceRef 
                                ; VAR counters: sys.ADDRESS 
                                );
BEGIN
  ASSERT(FALSE);
END RegistryModuleDynamic;


--------------------------------------------------------------------------------
PROCEDURE VersionSupported (version: TVersion): BOOLEAN;
BEGIN
  ASSERT(FALSE);
  RETURN FALSE;
END VersionSupported;


PROCEDURE MakeBackUp (fname: ARRAY OF CHAR);
BEGIN
  ASSERT(FALSE);
END MakeBackUp;

PROCEDURE ReadTCFile ( fname: ARRAY OF CHAR
                     ; regModuleList: TModuleInfoPtr
                     ; VAR unregModuleList: TModuleInfoPtr );
BEGIN
  ASSERT(FALSE);
END ReadTCFile;

PROCEDURE WriteTCFile ( fname: ARRAY OF CHAR
                      ; regModuleList: TModuleInfoPtr
                      ; VAR unregModuleList: TModuleInfoPtr
                      ; targets: TTargetSystemSet );
BEGIN
  ASSERT(FALSE);
END WriteTCFile;

PROCEDURE UpdateTCFile ( fname: ARRAY OF CHAR
                       ; regModuleList: TModuleInfoPtr
                       ; VAR unregModuleList: TModuleInfoPtr
                       ; skipReading: BOOLEAN
                       ; targets: TTargetSystemSet );
BEGIN
  ASSERT(FALSE);
END UpdateTCFile;

PROCEDURE GetProcNameLength ( procs: ARRAY OF CHAR
                            ; index: TIndex
                            ): CARDINAL;
BEGIN
  ASSERT(FALSE);
  RETURN 0;
END GetProcNameLength;

PROCEDURE GetProcName ( procs: ARRAY OF CHAR
                      ; index: TIndex
                      ; VAR proc: ARRAY OF CHAR
                      );
BEGIN
  ASSERT(FALSE);
END GetProcName;
  
PROCEDURE CancelTCFileOperation (VAR unregModuleList: TModuleInfoPtr);
BEGIN
  ASSERT(FALSE);
END CancelTCFileOperation;

PROCEDURE NewModuleInfo ( count: TIndex
                        ; modname: ARRAY OF CHAR
                        ; modtime: TTime
                        ; crcsum: TCRCSum ): TModuleInfoPtr;
BEGIN
  ASSERT(FALSE);
  RETURN NIL;
END NewModuleInfo;

PROCEDURE GetClone (moduleList: TModuleInfoPtr): TModuleInfoPtr;
BEGIN
  ASSERT(FALSE);
  RETURN NIL;
END GetClone;

PROCEDURE allocate (VAR addr: sys.ADDRESS; amount: CARDINAL);
BEGIN
  ASSERT(FALSE);
END allocate;

PROCEDURE free (VAR addr: sys.ADDRESS; amount: CARDINAL);
BEGIN
  ASSERT(FALSE);
END free;

PROCEDURE blocks_allocate (VAR addr: sys.ADDRESS; amount: CARDINAL);
BEGIN
  ASSERT(FALSE);
END blocks_allocate;

PROCEDURE blocks_free (VAR addr: sys.ADDRESS; amount: CARDINAL);
BEGIN
  ASSERT(FALSE);
END blocks_free;


--------------------------------------------------------------------------------
PROCEDURE ItereateFileLines ( fname: ARRAY OF CHAR
                            ; action: FILE_LINE_ACTION );
BEGIN
  ASSERT(FALSE);
END ItereateFileLines;

PROCEDURE ItereateEntries ( fname: ARRAY OF CHAR
                          -- module header; procedure_action's and
                          -- counter_action's will follow
                          ; module_action: MODULE_ACTION
                          -- procedure header; counter_action's will follow
                          ; procedure_action: PROCEDURE_ACTION
                          -- single 
                          ; counter_action: COUNTER_ACTION );
BEGIN
  ASSERT(FALSE);
END ItereateEntries;

<* POP *>

END xrTCSx86.
