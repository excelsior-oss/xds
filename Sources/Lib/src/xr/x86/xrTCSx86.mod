<*+ M2EXTENSIONS *>
--------------------------------------------------------------------------------
--                   Excelsior XDS Test Coverage Runtime Library
--                          (c) 2015, Excelsior Ltd.
-- Module:       xrTCSx86
-- Mission:      Test Coverage Run-Time Support
-- Synonym:      tc
-- Authors:      Lvov Konstantin
-- Created:      10-Dec-2002
--------------------------------------------------------------------------------
<* ALIGNMENT = '8' *> 

<* IF ~ DEFINED(__GEN_MIPS__) THEN *>  <* NEW __GEN_MIPS__- *>  <* END *>
<* IF ~ DEFINED(GHS_POSIX)    THEN *>  <* NEW GHS_POSIX- *>     <* END *>
<* IF ~ DEFINED(dbg_tcs)      THEN *>  <* NEW dbg_tcs- *>       <* END *>

<* IF ~ DEFINED(TARGET_VAX)  THEN *>  <* NEW TARGET_VAX- *>   <* END *>
<* IF ~ DEFINED(TARGET_MIPS) THEN *>  <* NEW TARGET_MIPS- *>  <* END *>
<* IF ~ DEFINED(TARGET_SPARC) THEN *>  <* NEW TARGET_SPARC- *>  <* END *>
<* IF ~ DEFINED(TARGET_PPC)   THEN *>  <* NEW TARGET_PPC- *>    <* END *>
<* IF ~ DEFINED(TARGET_386)  THEN *>  <* NEW TARGET_386- *>   <* END *>

<* IF ~ DEFINED(__GEN_VAX__)  THEN *>  <* NEW __GEN_VAX__- *>   <* END *>
<* IF ~ DEFINED(__GEN_MIPS__) THEN *>  <* NEW __GEN_MIPS__- *>  <* END *>
<* IF ~ DEFINED(__GEN_SPARC__) THEN *>  <* NEW __GEN_SPARC__- *>  <* END *>
<* IF ~ DEFINED(__GEN_PPC__)   THEN *>  <* NEW __GEN_PPC__- *>    <* END *>
<* IF ~ DEFINED(__GEN_X86__)  THEN *>  <* NEW __GEN_X86__- *>   <* END *>

<* IF ~ DEFINED(dbg_tcs)      THEN *>  <* NEW dbg_tcs- *>       <* END *>

<* IF __GEN_VAX__ THEN *>   
  <*+ GENDEBUG *>   
<* END *>

IMPLEMENTATION MODULE xrTCSx86;

IMPORT  xmRTS,              xrFName                                 
     ,  sys := SYSTEM,      os := xtcsOS
     ;

<* IF DEFINED(GENDLL) AND GENDLL THEN *>
IMPORT  ProgEnv;
<* END *>

--------------------------------------------------------------------------------
--                  Accumulate Test Coverage Information
--------------------------------------------------------------------------------

PROCEDURE INIT_TESTCOVERAGE (); FORWARD;
PROCEDURE GetCloneModuleInfo (module: TModuleInfoPtr): TModuleInfoPtr; FORWARD;
PROCEDURE allocate (VAR addr: sys.ADDRESS; amount: CARDINAL); FORWARD;

--------------------------------------------------------------------------------
<* PUSH *>
<* WOFF123+ *>  -- designator is read-only
PROCEDURE RegistryModule ( module:     TModuleInfoPtr
                         ; modtime:    TTime
                         ; crcsum:     TCRCSum
                         ; modname-:   ARRAY OF CHAR
                         ; procnames-: ARRAY OF CHAR
                         ; srcrefs-:   ARRAY OF TSourceRef 
                         ; counters-:  ARRAY OF TCounter 
                         );
BEGIN
  ASSERT( module # NIL );
  IF (module^.count # 0) THEN  RETURN  END;
  ASSERT( HIGH(srcrefs) = HIGH(counters) );

  IF (StaticModuleList = NIL) AND (DynamicModuleList = NIL) THEN
    INIT_TESTCOVERAGE();
  END;

  module^.count      := HIGH(srcrefs)+1;

  module^.modtime  := modtime;
  module^.crcsum   := crcsum;
  module^.procnames:= sys.REF(procnames);
  module^.modname  := sys.REF(modname);
  module^.srcrefs  := sys.REF(srcrefs);
  module^.counters := sys.REF(counters);
  sys.FILL( module^.counters, 0, SIZE(TCounter) * module^.count );

  module^.next     := StaticModuleList;
  StaticModuleList := module;
END RegistryModule;



PROCEDURE RegistryModuleDynamic ( module:     TModuleInfoPtr
                                ; modtime:    TTime
                                ; crcsum:     TCRCSum
                                ; modname-:   ARRAY OF CHAR
                                ; procnames-: ARRAY OF CHAR
                                ; srcrefs-:   ARRAY OF TSourceRef 
                                ; VAR counters: sys.ADDRESS
                                );
BEGIN
  ASSERT( module # NIL );
  IF (module^.count # 0) THEN  RETURN  END;

  IF (StaticModuleList = NIL) AND (DynamicModuleList = NIL) THEN
    INIT_TESTCOVERAGE();
  END;

  module^.count    := HIGH(srcrefs)+1;
  module^.modtime  := modtime;
  module^.crcsum   := crcsum;
  module^.procnames:= sys.REF(procnames);
  module^.modname  := sys.REF(modname);
  module^.srcrefs  := sys.REF(srcrefs);
  module^.counters := NIL;

  module   := GetCloneModuleInfo (module);
  counters := module^.counters;

  module^.next      := DynamicModuleList;
  DynamicModuleList := module;
END RegistryModuleDynamic;
<* POP *>


--------------------------------------------------------------------------------
PROCEDURE DecreaseCounter ( index: TIndex
                          ; VAR counters: ARRAY OF TCounter 
                          );
BEGIN
  ASSERT( (index >= 0) AND (index < LEN(counters)) );
  ASSERT( counters[index] > 0 );

  DEC(counters[index]);
END DecreaseCounter;


--------------------------------------------------------------------------------
PROCEDURE IncreaseCounter ( index: TIndex
                          ; VAR counters: ARRAY OF TCounter 
                          );
BEGIN
  ASSERT( (index >= 0) AND (index < LEN(counters)) );

  IF counters[index] < MAX(TCounter) THEN
    INC(counters[index]);
  END;
END IncreaseCounter;


--------------------------------------------------------------------------------
PROCEDURE IncreaseCounterExt ( index: TIndex 
                             ; Init: PROC
                             ; VAR counters: ARRAY OF TCounter 

                             );

BEGIN
  Init(); -- 2do: insert init flag & check it
  IncreaseCounter (index, counters);
END IncreaseCounterExt;


--------------------------------------------------------------------------------
PROCEDURE InitIterationCounter ( index_0: TIndex
                               ; VAR local_counter: TCounter
                               ; VAR counters: ARRAY OF TCounter 
                               );
BEGIN
  local_counter := 0;
  IncreaseCounter(index_0, counters);
END InitIterationCounter;

--------------------------------------------------------------------------------
PROCEDURE IncreaseIterationCounter ( index_0: TIndex
                                   ; index_1: TIndex
                                   ; index_N: TIndex
                                   ; VAR local_counter: TCounter
                                   ; VAR counters: ARRAY OF TCounter 
                                   );
BEGIN
  IF local_counter < MAX(TCounter) THEN
    INC(local_counter);
  END;
  CASE local_counter OF
  | 1: 
      DecreaseCounter(index_0, counters);
      IncreaseCounter(index_1, counters);
  | 2:
      DecreaseCounter(index_1, counters);
      IncreaseCounter(index_N, counters);
  ELSE
  END; 
END IncreaseIterationCounter;

--------------------------------------------------------------------------------
PROCEDURE InitRecursionDepth ( VAR depth : TCounter
                             ; VAR was_return: BOOLEAN );
BEGIN
  depth := 0;
  was_return := FALSE;
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
  IF was_return THEN
    CASE depth OF
    | 0:
        IncreaseCounter(index_0, counters);
    | 1:
        IncreaseCounter(index_1, counters);
    ELSE
        IncreaseCounter(index_N, counters);
    END;
  ELSE
    CASE depth OF
    | 0:
        IncreaseCounter(index_0, counters);
    | 1:
        DecreaseCounter(index_0, counters);
        IncreaseCounter(index_1, counters);
    | 2:
        DecreaseCounter(index_1, counters);
        IncreaseCounter(index_N, counters);
    ELSE
    END;
  END;

  was_return := FALSE;

  IF depth < MAX(TCounter) THEN
    INC(depth);
  END;
END IncreaseRecursionDepth;

--------------------------------------------------------------------------------
PROCEDURE DecreaseRecursionDepth ( VAR depth: TCounter
                                 ; VAR was_return: BOOLEAN
                                 );
BEGIN
  DEC(depth);
  was_return := TRUE;
END DecreaseRecursionDepth;


--------------------------------------------------------------------------------
PROCEDURE DecreaseVariableValue (VAR value: TCounter);
BEGIN
  DEC(value);
END DecreaseVariableValue;


--------------------------------------------------------------------------------
PROCEDURE IncreaseRelationCounterSigned ( relation: TRelation
                                        ; arg_left, arg_right: TSignedArgument
                                        ; index: TIndex
                                        ; VAR counters: ARRAY OF TCounter
                                        ): BOOLEAN;
BEGIN
  IF arg_left = arg_right THEN
    IncreaseCounter(index, counters);
  END;
  CASE relation OF
  | rel_LessThen:
      RETURN arg_left < arg_right;
  | rel_LessEqual:
      RETURN arg_left <= arg_right;
  | rel_GreaterThen:
      RETURN arg_left > arg_right;
  | rel_GreaterEqual:
      RETURN arg_left >= arg_right;
  END;
END IncreaseRelationCounterSigned;

--------------------------------------------------------------------------------
PROCEDURE IncreaseRelationCounterUnsigned ( relation: TRelation
                                          ; arg_left, arg_right: TUnsignedArgument
                                          ; index: TIndex
                                          ; VAR counters: ARRAY OF TCounter
                                          ): BOOLEAN;
BEGIN
  IF arg_left = arg_right THEN
    IncreaseCounter(index, counters);
  END;
  CASE relation OF
  | rel_LessThen:
      RETURN arg_left < arg_right;
  | rel_LessEqual:
      RETURN arg_left <= arg_right;
  | rel_GreaterThen:
      RETURN arg_left > arg_right;
  | rel_GreaterEqual:
      RETURN arg_left >= arg_right;
  END;
END IncreaseRelationCounterUnsigned;

--------------------------------------------------------------------------------
PROCEDURE IncreaseRelationCounter ( condition: BOOLEAN
                                   ; equal: BOOLEAN
                                   ; index: TIndex
                                   ; VAR counters: ARRAY OF TCounter
                                   ): BOOLEAN;

BEGIN
  IF equal THEN
    IncreaseCounter(index, counters);
  END;
  RETURN condition;
END IncreaseRelationCounter;

--------------------------------------------------------------------------------
PROCEDURE IncreaseConditionCounter ( condition: BOOLEAN
                                   ; index_true: TIndex
                                   ; index_false: TIndex
                                   ; VAR counters: ARRAY OF TCounter 
                                   ): BOOLEAN;
VAR index: TIndex;
BEGIN
  IF condition THEN
    index := index_true;
  ELSE
    index := index_false;
  END;
  IncreaseCounter(index, counters);
  RETURN condition;
END IncreaseConditionCounter;


--------------------------------------------------------------------------------
--                   Auxiliary Objects and Procedures
--------------------------------------------------------------------------------
CONST
  strcmp = xmRTS.X2C_STRCMP_PROC; 

  OutWithProcedureName = TestCriterion{tc_C1_Procedure, tc_C1_Module};

--------------------------------------------------------------------------------
PROCEDURE allocate (VAR addr: sys.ADDRESS; amount: CARDINAL);
BEGIN
  addr := os.X2C_malloc (amount);
END allocate;

PROCEDURE free (VAR addr: sys.ADDRESS; amount: CARDINAL);
BEGIN
  os.X2C_free (addr,  amount);
END free;

--------------------------------------------------------------------------------
CONST
  BLOCK_SIZE = 1024;

PROCEDURE get_blocks_number (amount: CARDINAL): CARDINAL;
BEGIN
  RETURN (amount DIV BLOCK_SIZE) + 1;
END get_blocks_number;

PROCEDURE blocks_allocate (VAR addr: sys.ADDRESS; amount: CARDINAL);
BEGIN
  addr := os.X2C_malloc (get_blocks_number(amount) * BLOCK_SIZE);
END blocks_allocate;

PROCEDURE blocks_free (VAR addr: sys.ADDRESS; amount: CARDINAL);
BEGIN
  os.X2C_free (addr, get_blocks_number(amount) * BLOCK_SIZE);
END blocks_free;


--------------------------------------------------------------------------------
-- identical to M2 ISO Strings.Append 
PROCEDURE Append (s: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);
  VAR pos, i, len: CARDINAL;
BEGIN
  pos:=LENGTH(d);
  len:=LENGTH(s);
  IF pos+len >HIGH(d)+1 THEN len:=HIGH(d)+1 - pos END;
  i:=0;
  WHILE i<len DO d[pos]:=s[i]; INC(i); INC(pos) END;
  IF pos<=HIGH(d) THEN d[pos]:=0C END;
END Append;


--------------------------------------------------------------------------------
<* PUSH *>
<* WOFF123+ *>  -- designator is read-only
PROCEDURE isStringEqual (str1-, str2-: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN strcmp( sys.ADR(str1), LENGTH(str1)
               , sys.ADR(str2), LENGTH(str2)
               ) = 0;
END isStringEqual;
<* POP *>

--------------------------------------------------------------------------------
<* IF DEFINED(GENDLL) AND GENDLL THEN *>
CONST 
  EnvString   = ProgEnv.String;
  ProgramName = ProgEnv.ProgramName;

<* ELSE *>

--------------------------------------------------------------------------------
<* PUSH *>
<* WOFF123+ *>  -- designator is read-only
PROCEDURE EnvString(name-: ARRAY OF CHAR; VAR str: ARRAY OF CHAR);
BEGIN
  os.X2C_EnvString( sys.CAST(xmRTS.X2C_pCHAR, sys.ADR(name))
                  , sys.CAST(xmRTS.X2C_pCHAR, sys.ADR(str))
                  , HIGH(str)+1 );
END EnvString;
<* POP *>

--------------------------------------------------------------------------------
PROCEDURE ProgramName(VAR name: ARRAY OF CHAR);
VAR p: xmRTS.X2C_pCHAR;
BEGIN
  p := os.X2C_GetProgramName();
  IF (p = NIL) THEN
    name[0] := 0C;
  ELSE
    COPY(sys.CAST(TStringPtr, p)^, name);
  END; 
END ProgramName;

<* END *> -- <* IF DEFINED(GENDLL) AND GENDLL THEN *>


--------------------------------------------------------------------------------
PROCEDURE NewModuleInfo ( count: TIndex
                        ; modname-: ARRAY OF CHAR
                        ; modtime: TTime
                        ; crcsum: TCRCSum ): TModuleInfoPtr;
VAR module: TModuleInfoPtr;
    size: sys.CARD32;
BEGIN
  IF count < 0 THEN  RETURN NIL END;

  allocate (module, SIZE(TModuleInfo));
  sys.FILL( module, 0, SIZE(TModuleInfo) );

  size := LENGTH(modname) + 1;
  allocate (module^.modname, size);
  sys.FILL( module^.modname, 0, size );

  size := SIZE(TSourceRef) * sys.CARD32(count);
  allocate (module^.srcrefs, size);
  sys.FILL( module^.srcrefs, 0, size );

  size := SIZE(TCounter) * sys.CARD32(count);
  allocate (module^.counters, size);
  sys.FILL( module^.counters, 0, size );

  module^.count   := count;
  module^.modtime := modtime;
  module^.crcsum  := crcsum;

  COPY (modname, TModuleName(module^.modname)^);

  RETURN module; 
END NewModuleInfo;


--------------------------------------------------------------------------------
PROCEDURE DisposeModule (VAR module: TModuleInfoPtr);
BEGIN
  IF module = NIL THEN RETURN END;
<* IF dbg_tcs THEN *>
  os.printf("    -- TCS [DisposeModule] begin\n");
  os.printf("       --     module= %s\n", TModuleName(module^.modname)^);
  os.printf("       --     free modname (%d)\n", LENGTH(TModuleName(module^.modname)^)+1);
<* END *>
  free (module^.modname,  LENGTH(TModuleName(module^.modname)^)+1);

  IF module^.procnames # NIL THEN
  <* IF dbg_tcs THEN *>
    os.printf("       --     page_free procnames (%d)\n", LENGTH(TProcNames(module^.procnames)^) + 1);
  <* END *>
    blocks_free( module^.procnames, LENGTH(TProcNames(module^.procnames)^));
  END;

<* IF dbg_tcs THEN *>
  os.printf("       --     free srcrefs (%d)\n", SIZE(TSourceRef) * sys.CARD32(module^.count));
<* END *>
  free (module^.srcrefs,  SIZE(TSourceRef) * sys.CARD32(module^.count)); 

<* IF dbg_tcs THEN *>
  os.printf("       --     free counters (%d)\n", SIZE(TCounter) * sys.CARD32(module^.count));
<* END *>
  free (module^.counters, SIZE(TCounter) * sys.CARD32(module^.count));

<* IF dbg_tcs THEN *>
  os.printf("       --     free module (%d)\n", SIZE(TModuleInfo));
<* END *>
  free (module, SIZE(TModuleInfo));

  module := NIL;
<* IF dbg_tcs THEN *>
  os.printf("    -- TCS [DisposeModule] end\n");
<* END *>
END DisposeModule;


--------------------------------------------------------------------------------
--                       Process  Messages and Errors 
--------------------------------------------------------------------------------
CONST
 StdOutS  = os.X2C_StdOutS;
 StdOutD  = os.X2C_StdOutD;
 StdOutLn = os.X2C_StdOutN;

 MAX_MessageLen = 41;

TYPE 
  TMessage = (
    msgFileOpen
  , msgMakeBackup 
  , msgIncompatibleModuleInfo
  , msgIncorrectModuleInfo
  , msgInconsistencyModuleInfo
  , msgInvalidFileName
  , msgUpdate
  , msgDifferentModifyTime
  , msgDifferentCRCSum
  , msgIncopatibleVersion
  );

  TMessageText = ARRAY TMessage OF ARRAY [0..MAX_MessageLen] OF CHAR;

CONST
 MessageText = TMessageText {
   "File open error "
 , "Backup creation error "
 , "Incompatible module logs "
 , "Incorrect module log "
 , "Inconsistency module logs "
 , "Invalid file name "
 , "Update: "
 , "Different source code modification time"
 , "Different source code CRC sum"
 , "Incompatible version of "
 };

 MessagePrefix = "#RTS Test Coverage: ";
 MergeError    = "merge information error: ";
 EmptyPrefix   = "                    "; 

--------------------------------------------------------------------------------
PROCEDURE Error (msg: TMessage; str-: ARRAY OF CHAR; index:=-1: TIndex);
BEGIN
  StdOutLn ();
  StdOutS (MessagePrefix, 1);
  StdOutS (MergeError, 1);
  StdOutLn ();

  StdOutS (EmptyPrefix, 1);
  StdOutS (MessageText[msg], 1);
  StdOutS (str, 1);
  IF index >= 0 THEN
    StdOutS (" at index ", 1);
    StdOutD (index, 1);
  END;
  StdOutLn ();
  INC (ErrorCnt);
END Error;


--------------------------------------------------------------------------------
VAR
  SilentMode: BOOLEAN; 

PROCEDURE Message (msg: TMessage; str-: ARRAY OF CHAR);
BEGIN
  IF NOT SilentMode THEN 
    StdOutLn ();
    StdOutS  (MessagePrefix, 1);
    StdOutS  (MessageText[msg], 1);
    StdOutS  (str, 1);
    StdOutLn ();
  END;
END Message;


--------------------------------------------------------------------------------
--                     Open/Close File Operation
--------------------------------------------------------------------------------
TYPE
  FILE = os.X2C_OSFILE;
  TNumberBuf = ARRAY [0..11] OF CHAR;

CONST
  BADFILE = FILE(NIL);

  LogFileExt = ".tc"; 
  BackUpExt  = ".tcb";

  MAXPATHLEN = 1024;

VAR
  UseCurrentDir: BOOLEAN; 

--------------------------------------------------------------------------------
PROCEDURE ReplaceExtension (VAR name: ARRAY OF CHAR; ext-: ARRAY OF CHAR);
VAR f: xrFName.Format;
BEGIN
  xrFName.X2C_ParseFileName (name, f);
  IF f.ok THEN
    IF f.extPos > 0 THEN
      name[f.extPos-1] := 0C;
    END;
    Append (ext, name);
  ELSE
    Error (msgInvalidFileName, name);
    ASSERT( FALSE );
  END;
END ReplaceExtension;

--------------------------------------------------------------------------------
PROCEDURE GetInfoFileName (VAR name: ARRAY OF CHAR);
BEGIN
  name[0] := 0C;
  EnvString(TCS_OUT_ENV_VAR, name);
  IF name[0] = 0C THEN
    ProgramName(name);
    IF (name[0] = 0C) THEN
      RETURN; -- can't get it now. try later
    END;
    IF UseCurrentDir THEN
      xrFName.X2C_ExtractBaseName (name, name);
      Append (LogFileExt, name);
    ELSE
      ReplaceExtension (name, LogFileExt);
    END;
  END;
END GetInfoFileName;


--------------------------------------------------------------------------------
PROCEDURE MakeBackUp (fname-: ARRAY OF CHAR);
VAR newfname: ARRAY [0..MAXPATHLEN-1] OF CHAR;
--    buf: TNumberBuf;
--    i, pos: sys.CARD32;
--    res: sys.int;
BEGIN
<* IF dbg_tcs THEN *>
  os.printf("-- TCS [MakeBackUp] begin\n");
  os.printf("--     fname= %s\n", fname);
<* END *>
  IF UseCurrentDir THEN
    xrFName.X2C_ExtractBaseName (fname, newfname);
    Append (BackUpExt, newfname);
  ELSE
    COPY (fname, newfname);
    ReplaceExtension (newfname, BackUpExt);
  END;

  IF os.X2C_Exists (newfname) THEN
    os.X2C_Remove (newfname);
  END;
(*
  res := os.X2C_Rename (fname, newfname);
  IF res # 0 THEN
    StdOutLn ();
    StdOutS ("*** Cannot rename file: errorcode= ", 1);
    StdOutD (res, 1);
    StdOutS (" ***", 1);
    StdOutLn ();
*)
  IF os.X2C_Rename (fname, newfname) # 0 THEN
    Error (msgMakeBackup, newfname);
  END;

(*
  i := 1;   
  WHILE os.X2C_Rename (fname, newfname) # 0 DO
    pos := 0;
    xrFName.X2C_ExtractBaseName (fname, newfname);
    os.X2C_DecToStr (buf, pos, i);
    Append (buf, newfname);
    Append (BackUpExt, newfname);
    INC(i);
  END;
*)
<* IF dbg_tcs THEN *>
  os.printf("-- TCS [MakeBackUp] end\n");
<* END *>
END MakeBackUp;


--------------------------------------------------------------------------------
PROCEDURE OpenFile (name-: ARRAY OF CHAR): FILE;
VAR file: FILE;
BEGIN
  IF os.X2C_FileOpenRead (file, name) # 0 THEN
    Error (msgFileOpen, name);
    file := BADFILE;
  END;
  RETURN file;
END OpenFile;

 
--------------------------------------------------------------------------------
PROCEDURE CreateFile (name-: ARRAY OF CHAR): FILE;
VAR file: FILE;
BEGIN
  IF os.X2C_FileOpenWrite (file, name) # 0 THEN
    Error (msgFileOpen, name);
    file := BADFILE;
  END;
  RETURN file;
END CreateFile;

 
--------------------------------------------------------------------------------
PROCEDURE CloseFile (VAR file: FILE);
BEGIN
  IF file # BADFILE THEN
    sys.EVAL( os.X2C_FileClose(file) );
    file := BADFILE;
  END;
END CloseFile;


--------------------------------------------------------------------------------
--                        Read/Write File Operation
--------------------------------------------------------------------------------
CONST
  TAB     = 11C;
  CR      = 15C;
  LF      = 12C;
  COMMENT = '%';
  SPACE   = ' ';
  SPACES  = "        ";

<* IF DEFINED(GENDLL) AND GENDLL THEN *>
  NEWLINE = "" + CR + LF;
<* ELSIF __GEN_X86__ THEN *>
  NEWLINE = "" + CR + LF;
<* ELSE *>
  NEWLINE = "" + LF;
<* END *>

--------------------------------------------------------------------------------
CONST
  StringSIZE  = 1024;
  ReadBufSIZE = StringSIZE*8;

TYPE
  TString = ARRAY [0..StringSIZE-1] OF CHAR;

  -- buffered file
  -- must be passed as VAR-parameter only
  BFILE = RECORD
    file: FILE;
    buf:  ARRAY [0..ReadBufSIZE-1] OF CHAR;
    len:  sys.CARD32;
    pos:  sys.CARD32;
  END; 


--------------------------------------------------------------------------------
<* PUSH *>
<* WOFF123+ *>  -- designator is read-only
PROCEDURE FILE_outS (file: FILE; str-: ARRAY OF CHAR; len: CARDINAL);
BEGIN
<* IF dbg_tcs THEN *>
  os.printf("      -- TCS [FILE_outS] begin\n");
  os.printf("         -- len = %d\n", len);
<* END *>
  IF (len > 0) THEN
    IF os.X2C_FileWrite (file, sys.ADR(str), len) # 0 THEN
      ASSERT(FALSE);
    END;
  END;
<* IF dbg_tcs THEN *>
  os.printf("      -- TCS [FILE_outS] end\n");
<* END *>
END FILE_outS;
<* POP *>


--------------------------------------------------------------------------------
PROCEDURE BFILE_AttachFILE_Write (VAR bfile: BFILE; file: FILE);
BEGIN
  bfile.file := file;
  bfile.pos  := 0;
  bfile.len  := 0;
END BFILE_AttachFILE_Write;


--------------------------------------------------------------------------------
PROCEDURE flush (VAR file: BFILE);
BEGIN
<* IF dbg_tcs THEN *>
  os.printf("    -- TCS [flush] begin\n");
<* END *>
  IF file.len > 0 THEN
    FILE_outS (file.file, file.buf, file.len);
    file.pos := 0;
    file.len := 0;
  END;
<* IF dbg_tcs THEN *>
  os.printf("    -- TCS [flush] end\n");
<* END *>
END flush;


--------------------------------------------------------------------------------
PROCEDURE outS (VAR file: BFILE; str-: ARRAY OF CHAR; len: CARDINAL);
VAR i: CARDINAL;
BEGIN
  IF (len > 0) THEN
    IF file.len + len > ReadBufSIZE THEN
      flush (file);
    END;
    FOR i:= 0 TO len-1 DO
      file.buf[file.pos+i] := str[i];
    END;
    INC(file.pos, len);
    INC(file.len, len);
  END;
END outS;


--------------------------------------------------------------------------------
PROCEDURE writeLn (VAR file: BFILE);
BEGIN
  outS (file, NEWLINE, LENGTH(NEWLINE));
END writeLn;


--------------------------------------------------------------------------------
PROCEDURE writeS (VAR file: BFILE; str-: ARRAY OF CHAR; width: CARDINAL);
VAR len: CARDINAL;
BEGIN
  len := LENGTH (str);
  outS (file, str, len);

  IF width > len THEN
    WHILE (width - len) > 8 DO  outS (file, SPACES, 8); INC(len, 8) END;
    IF width > len THEN outS (file, SPACES, width - len) END;
  END;
END writeS;


--------------------------------------------------------------------------------
PROCEDURE writeDec (VAR file: BFILE; number: sys.INT32; width: CARDINAL);
VAR buf: TNumberBuf;
    len: sys.CARD32;
BEGIN
  len := 0;
  os.X2C_DecToStr (buf, len, number);
  outS (file, buf, len);

  IF width > len THEN
    WHILE (width - len) > 8 DO  outS (file, SPACES, 8); INC(len, 8) END;
    IF width > len THEN outS (file, SPACES, width - len) END;
  END;
END writeDec;


--------------------------------------------------------------------------------
PROCEDURE writeHex (VAR file: BFILE; number: sys.CARD32; width: CARDINAL);
VAR buf: TNumberBuf;
    len: sys.CARD32;
BEGIN
  len := 0;
  os.X2C_HexToStr (buf, len, number);
  outS (file, buf, 8);

  IF width > 8 THEN
    len := 8;
    WHILE (width - len) > 8 DO  outS (file, SPACES, 8); INC(len, 8) END;
    IF width > len THEN outS (file, SPACES, width - len) END;
  END;
END writeHex;


--------------------------------------------------------------------------------
PROCEDURE BFILE_AttachFILE_Read (VAR bfile: BFILE; file: FILE);
BEGIN
  bfile.file := file;
  bfile.pos  := ReadBufSIZE;
  bfile.len  := ReadBufSIZE;
END BFILE_AttachFILE_Read;


--------------------------------------------------------------------------------
PROCEDURE inS (VAR file: BFILE; VAR str: ARRAY OF CHAR): BOOLEAN;
VAR i: sys.CARD32;
    last_ch: CHAR;
BEGIN
  str[0] := 0C;

  i := 0;
  -- extract string from buffer
  WHILE i < HIGH(str) DO
    -- update buffer
    IF file.pos >= file.len THEN
      IF file.len < ReadBufSIZE THEN
        RETURN FALSE;
      ELSE
        file.pos := 0;
        last_ch  := file.buf[file.len-1];
        sys.EVAL( os.X2C_FileRead(file.file, sys.ADR(file.buf), file.len) );
        IF (file.len = 0) THEN
          IF i = 0 THEN
            RETURN FALSE;
          ELSE
            str[i] := 0C;
            RETURN TRUE;
          END;
        ELSIF (last_ch = CR) & (file.buf[file.pos] = LF) THEN
          -- line separator has got on boundary of 'file.buf'
          -- skip second part of line separator
          INC(file.pos);
        END;
      END;
    END;

    -- check line separator
    IF file.buf[file.pos] = CR THEN
      str[i] := 0C;
      INC(file.pos);
      IF (file.pos >= file.len) THEN
        RETURN TRUE;
      END;
      -- skip second part of line separator
      ASSERT(file.buf[file.pos] = LF);
      INC(file.pos);
      RETURN TRUE;
    ELSIF file.buf[file.pos] = LF THEN
      str[i] := 0C;
      INC(file.pos);
      RETURN TRUE;
    END;

    str[i] := file.buf[file.pos];
    INC(i);
    INC(file.pos);
  END;

--  ASSERT(FALSE);
  RETURN FALSE;
END inS;


--------------------------------------------------------------------------------
PROCEDURE readS (VAR file: BFILE; VAR str: ARRAY OF CHAR): BOOLEAN;
VAR res: BOOLEAN;
BEGIN
  REPEAT
    res := inS (file, str);
  UNTIL (NOT res) OR (str[0] # COMMENT);
  RETURN res;
END readS;


--------------------------------------------------------------------------------
--                    Procedure name lists
--------------------------------------------------------------------------------
PROCEDURE ProcListAppend (module: TModuleInfoPtr; proc: ARRAY OF CHAR);
VAR
  procList, newProcList: TProcNames;
  curLen, newLen: CARDINAL;
  curPages, newPages: CARDINAL;
  i: CARDINAL;
BEGIN
  procList := TProcNames(module^.procnames);
  IF procList # NIL THEN
    curLen   := LENGTH(procList^);
    curPages := get_blocks_number(curLen);
  ELSE
    curLen   := 0;
    curPages := 0;
  END;
  newLen   := curLen + LENGTH(proc) + 1;
  newPages := get_blocks_number(newLen);
  IF curPages # newPages THEN
    blocks_allocate(newProcList, newLen);
    IF procList # NIL THEN
      COPY(procList^, newProcList^);
      blocks_free(procList, curLen)
    END;
    module^.procnames := newProcList;
  END;
  FOR i := curLen TO newLen - 2 DO
    TProcNames(module^.procnames)^[i] := proc[i - curLen]
  END;
  TProcNames(module^.procnames)^[newLen - 1] := PROCNAME_SEPARATOR;
  TProcNames(module^.procnames)^[newLen] := 0C
END ProcListAppend;

--------------------------------------------------------------------------------
PROCEDURE GetProcNameLength (procs-: ARRAY OF CHAR; index: TIndex): CARDINAL;
VAR
  i: CARDINAL;
BEGIN
  i := index;
  WHILE procs[i] # PROCNAME_SEPARATOR DO
    INC(i)
  END;
  RETURN i - CARDINAL(index)
END GetProcNameLength;

--------------------------------------------------------------------------------
PROCEDURE GetProcName ( procs-: ARRAY OF CHAR
                      ; index: TIndex
                      ; VAR proc: ARRAY OF CHAR );
VAR
  i: CARDINAL;
BEGIN
  i := index;
  WHILE (procs[i] # PROCNAME_SEPARATOR) AND (i - CARDINAL(index) <= HIGH(proc)) DO
    proc[i - CARDINAL(index)] := procs[i];
    INC(i)
  END;
  IF i - CARDINAL(index) <= HIGH(proc) THEN
    proc[i - CARDINAL(index)] := 0C
  END
END GetProcName;

--------------------------------------------------------------------------------
PROCEDURE GetProcIndex (module: TModuleInfoPtr; proc-: ARRAY OF CHAR): INTEGER;
VAR
  i, start: CARDINAL;
  procs: TProcNames;
BEGIN
  procs := module^.procnames;
  IF procs = NIL THEN
    RETURN -1
  END;
  i := 0;
  start := 0;
  LOOP
    IF procs^[start + i] = PROCNAME_SEPARATOR THEN
      IF (i > HIGH(proc)) OR (proc[i] = 0C) THEN
        RETURN start
      END;
      INC(start, i + 1);
      i := 0
    ELSIF procs^[start + i] = 0C THEN
      RETURN -1
    ELSIF procs^[start + i] = proc[i] THEN
      INC(i)
    ELSE
      REPEAT
        INC(i)
      UNTIL procs^[start + i] = PROCNAME_SEPARATOR;
      INC(start, i + 1);
      i := 0
    END
  END
END GetProcIndex;


--------------------------------------------------------------------------------
--                    Read Test Coverage Information
--------------------------------------------------------------------------------
TYPE 
  TModuleInfoHeader = RECORD
    count:   TIndex;
    modtime: TTime;
    crcsum:  TCRCSum;
    modname: TString;
  END;

  TStringPos = sys.CARD32;

--------------------------------------------------------------------------------
PROCEDURE FindField ( str-: ARRAY OF CHAR
                    ; start_sym, end_sym: CHAR
                    ; VAR start_pos, end_pos: TStringPos
                    ): BOOLEAN;
VAR str_len: TStringPos; 
BEGIN
  str_len := LENGTH(str);
  -- skip to start symbol
  IF start_sym = "" THEN
    WHILE (start_pos < str_len) AND (str[start_pos] = ' ') DO 
      INC(start_pos) 
    END;
  ELSE
    WHILE (start_pos < str_len) AND (str[start_pos] # start_sym) DO 
      INC(start_pos) 
    END;
    INC(start_pos);
  END;
  IF start_pos = str_len THEN RETURN FALSE END;

  end_pos := start_pos;
  -- skip to end symbol
  WHILE (end_pos < str_len) AND (str[end_pos] # end_sym) DO 
    INC(end_pos);  
  END;
--  IF (end_sym # "") AND (end_pos >= str_len) THEN RETURN FALSE END;
  DEC(end_pos);
  IF start_pos > end_pos THEN RETURN FALSE END;

  RETURN TRUE;
END FindField;


--------------------------------------------------------------------------------
<* PUSH *>
<* WOFF123+ *>  -- designator is read-only
PROCEDURE StrToType ( str-: ARRAY OF CHAR
                    ; VAR pos: TStringPos
                    ; VAR type: TestConditionType ): BOOLEAN;
VAR start_pos: TStringPos;
    itype: TestConditionType;
BEGIN
  start_pos := pos;
  IF NOT FindField (str, '@', ' ', start_pos, pos) THEN RETURN FALSE END;

  -- find corresponding type 
  FOR itype := MIN(TestConditionType) TO MAX(TestConditionType) DO
    IF strcmp ( sys.ADR(TestConditionNames[itype])
              , LENGTH(TestConditionNames[itype])
              , sys.ADR(str[start_pos])
              , pos-start_pos+1 
              ) = 0
    THEN
      type := itype;
      INC(pos);
      RETURN TRUE;
    END;
  END;

  RETURN FALSE;
END StrToType;
<* POP *>


--------------------------------------------------------------------------------
PROCEDURE StrToCounter ( str-: ARRAY OF CHAR
                       ; VAR pos: TStringPos
                       ; VAR count: TCounter ): BOOLEAN;
VAR start_pos: TStringPos;
    i: TStringPos;
BEGIN
  start_pos := pos;
  IF NOT FindField (str, '=', ' ', start_pos, pos) THEN RETURN FALSE END;

  count := 0;
  FOR i := start_pos TO pos DO
    count := count*10 + TCounter(ORD(str[i]) - ORD('0'));
  END;

  INC(pos);
  RETURN TRUE;
END StrToCounter;


--------------------------------------------------------------------------------
PROCEDURE StrToVersion ( str-: ARRAY OF CHAR
                       ; VAR pos: TStringPos
                       ; VAR version: TVersion ): BOOLEAN;
VAR start_pos: TStringPos;
    i: TStringPos;
BEGIN
  start_pos := pos;
  IF NOT FindField (str, '=', ' ', start_pos, pos) THEN RETURN FALSE END;

  version := 0;
  FOR i := start_pos TO pos DO
    version := version*10 + TVersion(ORD(str[i]) - ORD('0'));
  END;

  INC(pos);
  RETURN TRUE;
END StrToVersion;


--------------------------------------------------------------------------------
PROCEDURE StrToTargetSystems ( str-: ARRAY OF CHAR
                             ; VAR pos: TStringPos
                             ; VAR targets: TTargetSystemSet ): BOOLEAN;
VAR start_pos: TStringPos;
    i: TStringPos;
    value: TCard32;
BEGIN
  start_pos := pos;
  IF NOT FindField (str, '=', ' ', start_pos, pos) THEN RETURN FALSE END;

  value := 0;
  FOR i := start_pos TO pos DO
    value := value*10 + TCard32(ORD(str[i]) - ORD('0'));
  END;
  targets := TTargetSystemSet(value);

  INC(pos);
  RETURN TRUE;
END StrToTargetSystems;


--------------------------------------------------------------------------------
PROCEDURE StrToCRCSum ( str-: ARRAY OF CHAR
                      ; VAR pos: TStringPos
                      ; VAR count: TCRCSum ): BOOLEAN;
VAR start_pos: TStringPos;
    i: TStringPos;
BEGIN
  start_pos := pos;
  IF NOT FindField (str, ' ', ' ', start_pos, pos) THEN RETURN FALSE END;

  count := 0;
  FOR i := start_pos   TO pos DO
    count := count*10h;
    IF ORD(str[i]) >= ORD('a') THEN
      INC(count, TCRCSum(ORD(str[i]) - ORD('a') + 10));
    ELSIF ORD(str[i]) >= ORD('A') THEN
      INC(count, TCRCSum(ORD(str[i]) - ORD('A') + 10));
    ELSE
      INC(count, TCRCSum(ORD(str[i]) - ORD('0')));
    END;
  END;

  INC(pos);
  RETURN TRUE;
END StrToCRCSum;


--------------------------------------------------------------------------------
PROCEDURE StrToName ( str-: ARRAY OF CHAR
                    ; VAR pos: TStringPos
                    ; VAR name: ARRAY OF CHAR ): BOOLEAN;
VAR start_pos: TStringPos;
    i, len: TStringPos;
BEGIN
  start_pos := pos;
  IF NOT FindField (str, '"', '"', start_pos, pos) THEN RETURN FALSE END;

  len := pos - start_pos;
  ASSERT( len < HIGH(name) );

  FOR i := 0 TO len DO
    name[i] := str[start_pos+i];
  END;
  name[len+1] := 0C;

  INC(pos, 2);   -- symbol '"' is part of name and must be skipped 
  RETURN TRUE;
END StrToName;


--------------------------------------------------------------------------------
PROCEDURE StrToProcName ( module: TModuleInfoPtr
                        ; type: TestConditionType
                        ; str-: ARRAY OF CHAR
                        ; VAR pos: TStringPos
                        ; VAR index: TIndex
                        ): BOOLEAN;
CONST
  maxIdentLen = 255;
VAR start_pos: TStringPos;
    i, len: TStringPos;
    name: ARRAY[0..maxIdentLen] OF CHAR;
BEGIN
  IF type IN OutWithProcedureName THEN
    start_pos := pos;
    IF NOT FindField (str, '"', '"', start_pos, pos) THEN
      RETURN FALSE
    END;
    len := pos - start_pos + 1;
    IF len > maxIdentLen THEN
      len := maxIdentLen
    END;
    FOR i := 0 TO len - 1 DO
      name[i] := str[start_pos+i]
    END;
    name[len] := 0C;
    index := GetProcIndex(module, name);
    IF index = -1 THEN
      ProcListAppend(module, name);
      index := GetProcIndex(module, name);
      ASSERT(index # -1)
    END;
    INC(pos, 2) -- symbol '"' is part of name and must be skipped
  END;
  RETURN TRUE
END StrToProcName;


--------------------------------------------------------------------------------
PROCEDURE StrToModTime ( str-: ARRAY OF CHAR
                       ; VAR pos:  TStringPos
                       ; VAR time: TTime ): BOOLEAN;
VAR start_pos: TStringPos;
    i: TStringPos;
BEGIN
  start_pos := pos;
--  IF NOT FindField (str, '', ']', start_pos, pos) THEN RETURN FALSE END;
  IF NOT FindField (str, '', ' ', start_pos, pos) THEN RETURN FALSE END;
  -- in order to be independ from ShowTextTime On/Off cases
  IF (str[pos] = ']') THEN DEC(pos) END;

  time := 0;
  FOR i := start_pos TO pos DO
    time := time*10 + TTime(ORD(str[i]) - ORD('0'));
  END;

  INC(pos);
  RETURN TRUE;
END StrToModTime;            


--------------------------------------------------------------------------------
PROCEDURE StrToIndex ( str-: ARRAY OF CHAR
                     ; VAR pos: TStringPos
                     ; VAR index: TIndex
                     ): BOOLEAN;
VAR start_pos: TStringPos;
    i: TStringPos;
BEGIN
  start_pos := pos;
  IF NOT FindField (str, '', ' ', start_pos, pos) THEN RETURN FALSE END;

  index := 0;
  FOR i := start_pos TO pos DO
    index := index*10 + TIndex(ORD(str[i]) - ORD('0'));
  END;

  INC(pos);
  RETURN TRUE;
END StrToIndex;


--------------------------------------------------------------------------------
PROCEDURE StrToLine ( str-: ARRAY OF CHAR
                    ; VAR pos: TStringPos
                    ; VAR line: TSourceLine
                    ): BOOLEAN;
VAR start_pos: TStringPos;
    i: TStringPos;
BEGIN
  start_pos := pos;
  IF NOT FindField (str, '', '.', start_pos, pos) THEN RETURN FALSE END;

  line := 0;
  FOR i := start_pos TO pos DO
    line := line*10 + TSourceLine(ORD(str[i]) - ORD('0'));
  END;

  INC(pos);
  RETURN TRUE;
END StrToLine;


--------------------------------------------------------------------------------
PROCEDURE StrToCol ( str-: ARRAY OF CHAR
                   ; VAR pos: TStringPos
                   ; VAR col: TSourceCol
                   ): BOOLEAN;
VAR start_pos: TStringPos;
    i: TStringPos;
BEGIN
  start_pos := pos;
  IF NOT FindField (str, '.', ' ', start_pos, pos) THEN RETURN FALSE END;
  IF (str[pos] = ']') THEN DEC(pos) END;

  col := 0;
  FOR i := start_pos TO pos DO
    col := col*10 + TSourceCol(ORD(str[i]) - ORD('0'));
  END;

  INC(pos);
  RETURN TRUE;
END StrToCol;


--------------------------------------------------------------------------------
PROCEDURE StrToModuleInfoHeader ( str-: ARRAY OF CHAR
                                ; VAR header: TModuleInfoHeader ): BOOLEAN;
VAR pos: TStringPos; 
    type: TestConditionType;
BEGIN
  pos := 0;
  IF StrToType(str, pos, type) AND (type = tc_File) AND
     StrToCounter(str, pos, header.count)           AND
     StrToName(str, pos, header.modname)            AND
     StrToCRCSum(str, pos, header.crcsum)           AND
     StrToModTime(str, pos, header.modtime)            
  THEN
    RETURN TRUE;
  ELSE   
    RETURN FALSE;
  END;
END StrToModuleInfoHeader;


--------------------------------------------------------------------------------
PROCEDURE CheckVersion ( str-: ARRAY OF CHAR
                       ; fname-: ARRAY OF CHAR ): BOOLEAN;
VAR pos: TStringPos; 
    type: TestConditionType;
    version: TVersion;
BEGIN
  pos := 0;
  version := 0;
  IF StrToType(str, pos, type) AND (type = tc_Version) AND
     StrToVersion(str, pos, version)                   AND
     VersionSupported(version)
  THEN
  <* IF NOT __GEN_VAX__ THEN *>
    Version := version;
  <* END *>
    RETURN TRUE;
  ELSE   
    Error (msgIncopatibleVersion, fname);
    RETURN FALSE;
  END;
END CheckVersion;


--------------------------------------------------------------------------------
PROCEDURE MergeTargetSystemsInfo (str-: ARRAY OF CHAR): BOOLEAN;
VAR pos: TStringPos;
    type: TestConditionType;
    targets: TTargetSystemSet;
BEGIN
  pos := 0;
  targets := UNKNOWN_TargetSystem;
  IF StrToType(str, pos, type) AND (type = tc_Targets) AND
     StrToTargetSystems(str, pos, targets)
  THEN
    TargetSystems := TargetSystems + targets;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END MergeTargetSystemsInfo;


--------------------------------------------------------------------------------
--                    Merge Test Coverage Information
--------------------------------------------------------------------------------

PROCEDURE FindModuleInfoByName ( name: ARRAY OF CHAR
                               ; statModuleList, DynamicModuleList: TModuleInfoPtr
                               ): TModuleInfoPtr;
VAR module: TModuleInfoPtr;
    len: sys.CARD32;

    -- 1 --  FindModuleInfoByName -----------------------------------------------
    PROCEDURE Search (): BOOLEAN;
    BEGIN
      WHILE module # NIL DO 
        IF strcmp ( module^.modname, LENGTH(TModuleName(module^.modname)^)
                  , sys.ADR(name), len 
                  ) = 0
        THEN 
          RETURN TRUE;
        END;
        module := module^.next;
      END;
      RETURN FALSE;
    END Search;


-- 0 --  FindModuleInfoByName ---------------------------------------------------
BEGIN
  len := LENGTH(name);

  module := statModuleList;
  IF Search () THEN
    RETURN module;
  END;

  module := DynamicModuleList;
  IF Search () THEN
    RETURN module;
  END;

  RETURN NIL;
END FindModuleInfoByName;


--------------------------------------------------------------------------------
PROCEDURE IsComptibleLogs ( module: TModuleInfoPtr
                          ; header-: TModuleInfoHeader ): BOOLEAN;
BEGIN
--  IF module^.modtime # header.modtime THEN
--    Error (msgDifferentModifyTime, header.modname);
--  ELSIF module^.crcsum # header.crcsum THEN
  IF module^.crcsum # header.crcsum THEN
    Error (msgDifferentCRCSum, header.modname);
  ELSIF module^.count # header.count THEN
    Error (msgIncompatibleModuleInfo, header.modname);
  ELSE
    RETURN TRUE;
  END;
  RETURN FALSE;
END IsComptibleLogs;

 
--------------------------------------------------------------------------------
PROCEDURE MergeModuleInfo (VAR file: BFILE; module: TModuleInfoPtr);

    -- 1 -- MergeModuleInfo -----------------------------------------------------
    PROCEDURE MergeCounter (count1, count2: TCounter): TCounter;
    BEGIN
      -- the sum of counters
      ASSERT( (count1 >= 0) AND (count2 >= 0) );
      IF (MAX(TCounter) - count1 > count2) THEN  RETURN count1+count2;
      ELSE                                       RETURN MAX(TCounter);
      END;
      -- the maximum of counters
--      IF (count1 > count2) THEN   RETURN  count1;
--      ELSE                        RETURN  count2;
--      END;
    END MergeCounter;
       
-- 0 -- MergeModuleInfo ---------------------------------------------------------
VAR i: TIndex;
    str: TString;
    pos: TStringPos; 
    index: TIndex;
    type:  TestConditionType;
    count: TCounter;
    name:  TString;
    proc:  TIndex;
    line, end_line: TSourceLine;
    col, end_col:   TSourceCol;
    modname_len: sys.CARD32;
BEGIN
  modname_len := LENGTH(TModuleName(module^.modname)^);
  FOR i := 0 TO module^.count-1 DO
    pos := 0;
    IF readS (file, str)                 AND
       StrToIndex   (str, pos, index)    AND
       StrToType    (str, pos, type)     AND
       StrToCounter (str, pos, count)    AND
       StrToName    (str, pos, name)     AND
       StrToLine    (str, pos, line)     AND
       StrToCol     (str, pos, col)      AND    
       StrToLine    (str, pos, end_line) AND
       StrToCol     (str, pos, end_col)  AND
       StrToProcName(module, type, str, pos, proc)
    THEN
      IF (index = i)                                            AND
         (line = TSourceRefs(module^.srcrefs)^[i].line)         AND
         (col  = TSourceRefs(module^.srcrefs)^[i].col)          AND
         (end_line = TSourceRefs(module^.srcrefs)^[i].end_line) AND
         (end_col  = TSourceRefs(module^.srcrefs)^[i].end_col)  AND
         (NOT (type IN OutWithProcedureName) OR
           (proc = TSourceRefs(module^.srcrefs)^[i].proc))      AND
         (strcmp ( module^.modname, modname_len
                 , sys.ADR(name), LENGTH(name) 
                 ) = 0
         )
      THEN
        TCounters(module^.counters)^[i] := 
          MergeCounter (count, TCounters(module^.counters)^[i] );
      ELSE
        Error(msgInconsistencyModuleInfo, TModuleName(module^.modname)^, i); 
        RETURN; 
      END;
    ELSE
      Error(msgIncorrectModuleInfo, TModuleName(module^.modname)^, i); 
      RETURN; 
    END;
  END;
END MergeModuleInfo;

 
--------------------------------------------------------------------------------
PROCEDURE ReadUnRegisteredModuleInfo ( VAR file: BFILE
                                     ; header-: TModuleInfoHeader
                                     ; VAR DynamicModuleList: TModuleInfoPtr );
VAR module: TModuleInfoPtr;
    modname_len: sys.CARD32;
    i: TIndex;
    str: TString;
    pos: TStringPos; 
    index: TIndex;
    type:  TestConditionType;
    count: TCounter;
    name:  TString;
    proc:  TIndex;
    line, end_line: TSourceLine;
    col, end_col:   TSourceCol;
BEGIN
  module := NewModuleInfo ( header.count
                          , header.modname
                          , header.modtime
                          , header.crcsum );
  IF module = NIL THEN RETURN END;

  modname_len := LENGTH(TModuleName(module^.modname)^);
  FOR i := 0 TO module^.count-1 DO
    pos := 0;
    IF readS (file, str)                 AND
       StrToIndex   (str, pos, index)    AND
       StrToType    (str, pos, type)     AND
       StrToCounter (str, pos, count)    AND
       StrToName    (str, pos, name)     AND
       StrToLine    (str, pos, line)     AND
       StrToCol     (str, pos, col)      AND   
       StrToLine    (str, pos, end_line) AND
       StrToCol     (str, pos, end_col)  AND
       StrToProcName(module, type, str, pos, proc)
    THEN
      IF strcmp ( module^.modname, modname_len
                , sys.ADR(name), LENGTH(name) 
                ) = 0
      THEN
        TSourceRefs(module^.srcrefs)^[index].type     := type;
        TSourceRefs(module^.srcrefs)^[index].proc     := proc;
        TSourceRefs(module^.srcrefs)^[index].line     := line;
        TSourceRefs(module^.srcrefs)^[index].col      := col;
        TSourceRefs(module^.srcrefs)^[index].end_line := end_line;
        TSourceRefs(module^.srcrefs)^[index].end_col  := end_col;
        TCounters(module^.counters)^[index]           := count;
      ELSE
        Error(msgInconsistencyModuleInfo, TModuleName(module^.modname)^, i); 
        DisposeModule (module);
        RETURN; 
      END;
    ELSE
      Error(msgIncorrectModuleInfo, TModuleName(module^.modname)^, i); 
      DisposeModule (module);
      RETURN; 
    END;
  END;

  module^.next      := DynamicModuleList;
  DynamicModuleList := module;
END ReadUnRegisteredModuleInfo;
 

--------------------------------------------------------------------------------
PROCEDURE ReadStatistics ( file: FILE
                         ; fname-: ARRAY OF CHAR
                         ; statModuleList: TModuleInfoPtr
                         ; VAR DynamicModuleList: TModuleInfoPtr );
VAR bfile: BFILE;
    str: TString;
    modheader: TModuleInfoHeader;
    module: TModuleInfoPtr;
    str_enabled: BOOLEAN;
BEGIN
<* IF dbg_tcs THEN *>
  os.printf("    -- TCS [ReadStatistics] begin\n");
<* END *>
  BFILE_AttachFILE_Read (bfile, file);
  IF readS (bfile, str) AND CheckVersion(str, fname) THEN
    str_enabled := readS (bfile, str) AND NOT MergeTargetSystemsInfo(str);
    WHILE str_enabled OR readS (bfile, str) DO
      str_enabled := FALSE;
      IF StrToModuleInfoHeader (str, modheader) THEN
      <* IF dbg_tcs THEN *>
        os.printf("    --     module= %s\n", modheader.modname);
      <* END *>
        module := FindModuleInfoByName ( modheader.modname
                                       , statModuleList, DynamicModuleList );
        IF module # NIL THEN
          IF IsComptibleLogs (module, modheader) THEN
            MergeModuleInfo (bfile, module);
          END;
        ELSE
          ReadUnRegisteredModuleInfo (bfile, modheader, DynamicModuleList);
        END;
      END;
    END; -- WHILE readS (...
  END;
<* IF dbg_tcs THEN *>
  os.printf("    -- TCS [ReadStatistics] begin\n");
<* END *>
END ReadStatistics;
 

--------------------------------------------------------------------------------
PROCEDURE ReadTCFile ( fname-: ARRAY OF CHAR
                     ; statModuleList: TModuleInfoPtr
                     ; VAR DynamicModuleList: TModuleInfoPtr );
VAR file: FILE;
BEGIN
<* IF dbg_tcs THEN *>
  os.printf("-- TCS [ReadTCFile] begin\n");
  os.printf("--     fname= %s\n", fname);
<* END *>
  file := OpenFile (fname);
  IF file # BADFILE THEN
    ReadStatistics (file, fname, statModuleList, DynamicModuleList);
    CloseFile (file);
  END;
<* IF dbg_tcs THEN *>
  os.printf("-- TCS [ReadTCFile] end\n");
<* END *>
END ReadTCFile;


--------------------------------------------------------------------------------
--                     Save Test Coverage Information
--------------------------------------------------------------------------------

PROCEDURE WriteModuleModifyTime (VAR file: BFILE; time: TTime);
VAR ini, res: os.X2C_TimeStruct;
BEGIN
  writeDec (file, time, 0);
  writeS   (file, ' ', 1);

  os.X2C_GetTime (ini);
  ini.year  := 1970;
  ini.month := 1;
  ini.day   := 1;
  ini.hour  := 0;
  ini.min   := 0;
  ini.sec   := 0;
  ini.fracs := 0;
  os.X2C_TimeSecAdd (ini, time, res);

  writeDec (file, res.day, 1);
  writeS   (file, '.', 1);
  writeDec (file, res.month, 1);
  writeS   (file, '.', 1);
  writeDec (file, res.year, 1);
  writeS   (file, ' ', 1);

  writeDec (file, res.hour, 1);
  writeS   (file, ':', 1);
  writeDec (file, res.min, 1);
END WriteModuleModifyTime;


--------------------------------------------------------------------------------
PROCEDURE WriteModuleHeader (VAR file: BFILE; module: TModuleInfoPtr);
BEGIN
<* IF dbg_tcs THEN *>
  os.printf("    --     module: %s; next= %x\n", TModuleName(module^.modname)^, module^.next);
  os.printf("    --     count= %d\n", module^.count);
  os.printf("    --     modtime= %d\n", module^.modtime);
  os.printf("    --     crcsum= %d\n", module^.crcsum);
<* END *>
  writeS   (file, '     @', 1);
  writeS   (file, TestConditionNames[tc_File], MAX_TypeNameLen);
  writeS   (file, ' =', 1);
  writeDec (file, module^.count, 8);
  writeS   (file, ' ["', 1);
  writeS   (file, TModuleName(module^.modname)^, 1);
  writeS   (file, '" ', 1);
  writeHex (file, module^.crcsum, 8);
  writeS   (file, ' ', 1);
  WriteModuleModifyTime (file, module^.modtime);
  writeS   (file, ']', 1);
  writeLn  (file);
END WriteModuleHeader;


--------------------------------------------------------------------------------
PROCEDURE WriteModule (VAR file: BFILE; module: TModuleInfoPtr);
VAR i: TIndex;
    procName: ARRAY[0..255] OF CHAR;
BEGIN
<* IF dbg_tcs THEN *>
  os.printf("    -- TCS [WriteModule] begin\n");
<* END *>
  WriteModuleHeader (file, module);
  FOR i:= 0 TO module^.count-1 DO
    -- "%d @%s =%d [%s %d.%d]"
    writeDec (file, i, 4);
    writeS   (file, ' @', 1);
    writeS   (file, TestConditionNames[TSourceRefs(module^.srcrefs)^[i].type], MAX_TypeNameLen);
    writeS   (file, ' =', 1);
    writeDec (file, TCounters(module^.counters)^[i], 8);
    writeS   (file, ' ["', 1);
    writeS   (file, TModuleName(module^.modname)^, 1);
    writeS   (file, '" ', 1);
    writeDec (file, TSourceRefs(module^.srcrefs)^[i].line, 1);
    writeS   (file, '.', 1);
    writeDec (file, TSourceRefs(module^.srcrefs)^[i].col, 1);
    writeS   (file, ' ', 1);
    writeDec (file, TSourceRefs(module^.srcrefs)^[i].end_line, 1);
    writeS   (file, '.', 1);
    writeDec (file, TSourceRefs(module^.srcrefs)^[i].end_col, 1);
    IF TSourceRefs(module^.srcrefs)^[i].type IN OutWithProcedureName THEN
      writeS (file, ' "', 1);
      GetProcName(TProcNames(module^.procnames)^,
        TSourceRefs(module^.srcrefs)^[i].proc, procName);
      writeS (file, procName, 1);
      writeS (file, '"', 1)
    END;
    writeS   (file, ']', 1);
    writeLn  (file);
  END;
<* IF dbg_tcs THEN *>
  os.printf("    -- TCS [WriteModule] end\n");
<* END *>
END WriteModule;


--------------------------------------------------------------------------------
PROCEDURE WriteVersion (VAR file: BFILE; version: sys.INT32);
BEGIN
<* IF dbg_tcs THEN *>
  os.printf("    -- TCS [WriteVersion] begin\n");
<* END *>
  writeS   (file, '     @', 1);
  writeS   (file, TestConditionNames[tc_Version], MAX_TypeNameLen);
  writeS   (file, ' =', 1);
  writeDec (file, version, 8);
  writeLn  (file);
<* IF dbg_tcs THEN *>
  os.printf("    -- TCS [WriteVersion] end\n");
<* END *>
END WriteVersion;


--------------------------------------------------------------------------------
PROCEDURE WriteTargetSystemsInfo (VAR file: BFILE; targets: TTargetSystemSet);
VAR i: TTargetSystem;
    first: BOOLEAN;
BEGIN
<* IF dbg_tcs THEN *>
  os.printf("    -- TCS [WriteTargetSystemsInfo] begin\n");
<* END *>
  writeS (file, '     @', 1);
  writeS (file, TestConditionNames[tc_Targets], MAX_TypeNameLen);
  writeS (file, ' =', 1);
  writeDec (file, sys.CAST(TCard32, targets), 8);
  IF TargetSystems # UNKNOWN_TargetSystem THEN
    first := TRUE;
    writeS (file, ' [', 1);
    FOR i := MIN(TTargetSystem) TO MAX(TTargetSystem) DO
      IF (i IN TargetSystems) THEN
        IF first THEN  writeS (file, '"', 1);       first := FALSE;
        ELSE           writeS (file, ', "', 1);
        END;
        writeS (file, TargetSystemsNames[i], 1);
        writeS (file, '"', 1);
      END;
    END;
    writeS (file, ']', 1);
  END;
  writeLn  (file);
<* IF dbg_tcs THEN *>
  os.printf("    -- TCS [WriteTargetSystemsInfo] end\n");
<* END *>
END WriteTargetSystemsInfo;


--------------------------------------------------------------------------------
PROCEDURE WriteStatistics ( file: FILE
                          ; statModuleList: TModuleInfoPtr
                          ; VAR DynamicModuleList: TModuleInfoPtr );
VAR bfile: BFILE;
    tmp: TModuleInfoPtr;
BEGIN
<* IF dbg_tcs THEN *>
  os.printf("  -- TCS [WriteStatistics] begin\n");
<* END *>
  BFILE_AttachFILE_Write (bfile, file);

  WriteVersion (bfile, TCS_RTS_VERSION);
  WriteTargetSystemsInfo(bfile, TargetSystems);

  WHILE statModuleList # NIL DO
    WriteModule (bfile, statModuleList);
    statModuleList := statModuleList^.next;
  END;

<* IF dbg_tcs THEN *>
  os.printf("  -- TCS [WriteStatistics] middle\n");
<* END *>

  WHILE DynamicModuleList # NIL DO
    WriteModule (bfile, DynamicModuleList);
    tmp := DynamicModuleList;
    DynamicModuleList := DynamicModuleList^.next;
    DisposeModule (tmp);
  END;

  flush (bfile);
<* IF dbg_tcs THEN *>
  os.printf("  -- TCS [WriteStatistics] end\n");
<* END *>
END WriteStatistics;


--------------------------------------------------------------------------------
PROCEDURE WriteTCFile ( fname-: ARRAY OF CHAR
                      ; statModuleList: TModuleInfoPtr
                      ; VAR DynamicModuleList: TModuleInfoPtr
                      ; targets: TTargetSystemSet
                      );
VAR file: FILE;
BEGIN
<* IF dbg_tcs THEN *>
  os.printf("-- TCS [WriteTCFile] begin\n");
  os.printf("--     fname= %s\n", fname);
<* END *>
  TargetSystems := TargetSystems + targets;
  file := CreateFile (fname);
  IF file # BADFILE THEN
    WriteStatistics (file, statModuleList, DynamicModuleList);
    CloseFile (file);
  END;
<* IF dbg_tcs THEN *>
  os.printf("-- TCS [WriteTCFile] end\n");
<* END *>
END WriteTCFile;

--------------------------------------------------------------------------------
PROCEDURE UpdateTCFile ( fname-: ARRAY OF CHAR
                       ; statModuleList: TModuleInfoPtr
                       ; VAR DynamicModuleList: TModuleInfoPtr
                       ; skipReading: BOOLEAN
                       ; targets: TTargetSystemSet
                       );
BEGIN
  ErrorCnt := 0;

  IF NOT skipReading THEN
    IF os.X2C_Exists (fname) THEN
      ReadTCFile (fname, statModuleList, DynamicModuleList);
      MakeBackUp (fname);
    END;
  END;

  WriteTCFile (fname, statModuleList, DynamicModuleList, targets);
END UpdateTCFile;


--------------------------------------------------------------------------------
PROCEDURE CancelTCFileOperation (VAR DynamicModuleList: TModuleInfoPtr);
VAR tmp: TModuleInfoPtr;             
BEGIN
  WHILE DynamicModuleList # NIL DO 
    tmp := DynamicModuleList;
    DynamicModuleList := DynamicModuleList^.next;
    DisposeModule (tmp);
  END;
END CancelTCFileOperation;


--------------------------------------------------------------------------------
--                Initialization/Finalization Routines
--------------------------------------------------------------------------------
VAR 
  InfoFileName: ARRAY [0..MAXPATHLEN-1] OF CHAR;

<* IF __GEN_MIPS__ THEN *>
PROCEDURE ["C"] / X2C_INIT_TESTCOVERAGE_start();
PROCEDURE ["C"] / X2C_EXIT_TESTCOVERAGE_start();
<* END *>

--------------------------------------------------------------------------------
-- only to notify debugger
PROCEDURE [CallConv] X2C_TESTCOVERAGE_COMPLETE();
BEGIN
END X2C_TESTCOVERAGE_COMPLETE;

--------------------------------------------------------------------------------
-- Must be call latest as possible. Merge and store statistics. 
PROCEDURE [CallConv] X2C_EXIT_TESTCOVERAGE ();
BEGIN
<* IF dbg_tcs THEN *>
  os.printf("-- TCS [X2C_EXIT_TESTCOVERAGE] begin\n");
<* END *>

<* IF __GEN_MIPS__ THEN *>
  X2C_EXIT_TESTCOVERAGE_start();
<* END *>
  IF (StaticModuleList # NIL) OR (DynamicModuleList # NIL) THEN
    IF (InfoFileName[0] = 0C) THEN
      GetInfoFileName (InfoFileName);
      IF (InfoFileName[0] = 0C) THEN
        ASSERT(FALSE);
      END;
    END;
    UpdateTCFile(InfoFileName, StaticModuleList, DynamicModuleList, SkipTCFileReading);
    SkipTCFileReading := TRUE;
    Message (msgUpdate, InfoFileName);
  END;

  X2C_TESTCOVERAGE_COMPLETE();

  IF ErrorCnt > 0 THEN
    HALT(15);
  END;

<* IF dbg_tcs THEN *>
  os.printf("-- TCS [X2C_EXIT_TESTCOVERAGE] end\n");
<* END *>
END X2C_EXIT_TESTCOVERAGE;

<* IF __GEN_X86__ OR (DEFINED(GENDLL) AND GENDLL) THEN *>
-- function is defined in startup module
PROCEDURE ["C"] / X2C_FINALLY(proc: PROC);

--------------------------------------------------------------------------------
-- Modula-2 proxy function for xmRTS.X2C_FINALLY 
PROCEDURE ExitTestCoverage();
BEGIN
  X2C_EXIT_TESTCOVERAGE()
END ExitTestCoverage;
<* END *>

--------------------------------------------------------------------------------
PROCEDURE INIT_TESTCOVERAGE ();
VAR value: ARRAY [0..63] OF CHAR;
BEGIN
<* IF dbg_tcs THEN *>
  os.printf("-- TCS [INIT_TESTCOVERAGE] begin\n");
<* END *>

<* IF __GEN_MIPS__ THEN *>
  X2C_INIT_TESTCOVERAGE_start();
<* END *>

  ErrorCnt := 0;
  UseCurrentDir     := FALSE;
<* IF __GEN_VAX__ THEN *>
  IF sys.CAST(sys.CARD8, SkipTCFileReading) # ORD(TRUE) THEN
    SkipTCFileReading := FALSE;
  END;
<* ELSE *>
  SkipTCFileReading := FALSE;
<* END *>

  GetInfoFileName (InfoFileName);
  OutFileName := sys.REF(InfoFileName);

  value[0] := 0C;
  EnvString(TCS_MESSAGE_ENV_VAR, value);
  SilentMode := isStringEqual(value, TCS_MESSAGE_OFF); 

<* IF __GEN_VAX__ THEN *>
  TargetSystems := TTargetSystemSet{
                   <* IF    TARGET_VAX  THEN *>     tgs_VAX
                   <* ELSIF TARGET_MIPS THEN *>     tgs_MIPS
                   <* ELSIF TARGET_386  THEN *>     tgs_x86
                   <* ELSIF TARGET_C     THEN *>     tgs_C
                   <* ELSIF TARGET_SPARC THEN *>     tgs_SPARC
                   <* ELSIF TARGET_PPC   THEN *>     tgs_PPC
                   <* ELSE *>
                     <* IF    __GEN_VAX__  THEN *>  tgs_VAX
                     <* ELSIF __GEN_MIPS__ THEN *>  tgs_MIPS
                     <* ELSIF __GEN_X86__  THEN *>  tgs_x86
                     <* ELSIF __GEN_C__     THEN *>  tgs_C
                     <* ELSIF __GEN_SPARC__ THEN *>  tgs_SPARC
                     <* ELSIF __GEN_PPC__   THEN *>  tgs_PPC
                     <* ELSE *>                     UNKNOWN_TargetSystem
                     <* END  *>
                   <* END *>
                   };
<* END *>

<* IF __GEN_X86__ OR (DEFINED(GENDLL) AND GENDLL) THEN *>
  X2C_FINALLY(ExitTestCoverage);
<* ELSE *>
  os.X2C_atexit(X2C_EXIT_TESTCOVERAGE);
<* END *>

<* IF dbg_tcs THEN *>
  os.printf("-- TCS [INIT_TESTCOVERAGE] end\n");
<* END *>
END INIT_TESTCOVERAGE;


--------------------------------------------------------------------------------
PROCEDURE VersionSupported (version: TVersion): BOOLEAN;
BEGIN
  RETURN (version = TCS_RTS_VERSION)
      OR (version = 100)
       ;
END VersionSupported;



--------------------------------------------------------------------------------
--                     Support Test Coverage Utilities
--------------------------------------------------------------------------------
PROCEDURE ItereateFileLines ( fname-: ARRAY OF CHAR
                            ; action: FILE_LINE_ACTION );
VAR file: FILE;
    bfile: BFILE;
    str: TString;
BEGIN
  file := OpenFile (fname);
  IF file # BADFILE THEN
    BFILE_AttachFILE_Read (bfile, file);
    WHILE inS (bfile, str) DO
      action (str);
    END;
    CloseFile (file);
  END;
END ItereateFileLines;


--------------------------------------------------------------------------------
PROCEDURE ItereateEntries ( fname: ARRAY OF CHAR
                          ; module_action: MODULE_ACTION
                          ; procedure_action: PROCEDURE_ACTION
                          ; counter_action: COUNTER_ACTION );
VAR
  info, module: TModuleInfoPtr;
  i: TIndex;
  procName: TProcedureName;
BEGIN
  TargetSystems := UNKNOWN_TargetSystem;
  info := NIL;
  ReadTCFile (fname, NIL, info);
  module := info;
  WHILE module # NIL DO
    module_action (module);
    FOR i := 0 TO module^.count-1 DO
      WITH TSourceRefs(module^.srcrefs)^[i] DO
        IF type IN OutWithProcedureName THEN
          GetProcName(TProcNames(module^.procnames)^, proc, procName);
          procedure_action(procName, proc)
        END
      END;
      counter_action ( TCounters(module^.counters)^[i]
                     , sys.ADR(TSourceRefs(module^.srcrefs)^[i]));
    END;
    module := module^.next;
  END;
  CancelTCFileOperation(info);
END ItereateEntries;

--------------------------------------------------------------------------------
--                     Clone Test Coverage Information
--------------------------------------------------------------------------------

PROCEDURE GetCloneModuleInfo (module: TModuleInfoPtr): TModuleInfoPtr;
VAR clone: TModuleInfoPtr;
    size: sys.CARD32;
BEGIN
  IF module = NIL THEN  RETURN NIL; END;

  clone := NewModuleInfo ( module^.count
                         , TModuleName(module^.modname)^
                         , module^.modtime
                         , module^.crcsum );

  size := LENGTH(TProcNames(module^.procnames)^) + 1;
  blocks_allocate (clone^.procnames, size);
  sys.MOVE( module^.procnames, clone^.procnames, size );

  size := SIZE(TSourceRef) * sys.CARD32(clone^.count);
  sys.MOVE( module^.srcrefs, clone^.srcrefs, size );

  IF (module^.counters # NIL) THEN
    size := SIZE(TCounter) * sys.CARD32(clone^.count);
    sys.MOVE( module^.counters, clone^.counters, size );
  END;

  RETURN clone;
END GetCloneModuleInfo;

--------------------------------------------------------------------------------
PROCEDURE GetClone (moduleList: TModuleInfoPtr): TModuleInfoPtr;
VAR clonedList, clone, tmp: TModuleInfoPtr;
BEGIN
  clonedList := NIL;
  WHILE moduleList # NIL DO
    clone := GetCloneModuleInfo(moduleList);
    clone^.next := clonedList;
    clonedList  := clone;
    moduleList  := moduleList^.next;
  END;

  clone := clonedList;
  clonedList := NIL;
  WHILE clone # NIL DO
    tmp := clone^.next;
    clone^.next := clonedList;
    clonedList  := clone;
    clone := tmp;
  END;

  RETURN clonedList;
END GetClone;

END xrTCSx86.
