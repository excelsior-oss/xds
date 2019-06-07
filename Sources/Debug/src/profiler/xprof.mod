-- Главный модуль компоненты xprof
-- Профилировщик

MODULE xProf;

<* Storage+ *>
<* ALIGNMENT = "1" *>

IMPORT sys := SYSTEM;
IMPORT arg := ProgEnv;
IMPORT fmt := FormStr;
IMPORT fs  := FileSys;

IMPORT xs  := xStr;
IMPORT fil := File;

IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;
IMPORT bld := DI_Build;
IMPORT kt  := KrnTypes;

IMPORT pt  := PrfTypes;
IMPORT pb  := PrfBuild;

FROM Printf IMPORT printf;


<* IF env_target='x86os2' THEN *>

IMPORT rf  := RndFile;
IMPORT xfp := xFilePos;
IMPORT rio := RawIO;
IMPORT ioc := IOChan;

IMPORT OS2;

<* ELSIF env_target='x86nt' THEN *>

IMPORT win := Windows;
IMPORT prg := PrgNames;

<* END *>


CONST
  PRODUCT   = "trace execution utility";
  COPYRIGHT = "1997-2001 Excelsior";


PROCEDURE Copyright;
BEGIN
  printf("\n%s, %s, Version %s\n(c) %s\n\n", pt.IDENTKEY, PRODUCT, pt.VERSION, COPYRIGHT);
END Copyright;



CONST
  SLEEP_TIME_DEFAULT = 55;


VAR
  prog_name: xs.String;
  prog_args: xs.String;
  SleepTime: CARDINAL;


PROCEDURE Help;
BEGIN
  printf("Usage:   xprof [ ('-'|'/') options ] program [ arguments ]\n\n");
  printf('Options: R=<rate>    interval between snapshots\n');
  printf('         <rate> is a number specifying the approximate time\n');
  printf('         interval (in milliseconds) between snapshots\n');
  printf('         default: 55, minimum: 32\n');
  HALT (0);
END Help;


PROCEDURE Options (opt-: ARRAY OF CHAR);
VAR
  s: xs.String;
  ok: BOOLEAN;
BEGIN
  CASE CAP(opt[1]) OF
  | 'R' : IF opt[2] # '=' THEN Help; END;
          xs.Extract(opt, 3, LENGTH(opt), s);
          SleepTime := xs.StrToCard(s, 10, ok);
          IF NOT ok OR (SleepTime < 32) THEN Help; END;
  ELSE
    Help;
  END;
END Options;


PROCEDURE ParseCommandLine;
VAR
  k, i, p: CARDINAL;
  a: xs.String;
  f: BOOLEAN;
BEGIN
  k := arg.ArgNumber();
  IF k = 0 THEN Help; END;
  i := 0;
  LOOP
    IF (i = k) THEN EXIT; END;
    arg.GetArg(i, a);
    IF (a[0] = '/') OR (a[0] = '-') THEN
      Options(a);
    ELSE
      EXIT;
    END;
    INC(i);
  END;
  IF i = k THEN Help; END;
  arg.GetArg(i, prog_name);
  fil.AddExtension(prog_name, 'exe');
  prog_args := '';
  INC(i);
  WHILE i < k DO
    arg.GetArg(i, a);
    <* PUSH *>
    <* WOFF903+ *>
    p := xs.CharPos (a, ' ', f);
    <* POP *>
    IF f THEN
      xs.Insert ('"', 0, a);
      xs.Append ('"', a);
    END;
    INC(i);
    xs.Append(a, prog_args);
    xs.Append(' ', prog_args);
  END;
END ParseCommandLine;



<* IF env_target = 'x86os2' THEN *>

VAR
  DbgBuffer: OS2.uDB_t;
  PID      : OS2.PID;
  SessionID: OS2.ULONG;
  ProgramAccess: OS2.HMTX;

TYPE
  DB = sys.CARD8;
  DW = sys.CARD16;
  DD = sys.CARD32;

  lx_header = RECORD
    signature  : ARRAY [0..1] OF CHAR;
    border     : DB;     (* byte ordering                             *)
    worder     : DB;     (* word ordering                             *)
    lefl       : DD;     (* Linear EXE Format Level                   *)
    cpu        : DW;     (* Module CPU type                           *)
    ostype     : DW;     (* Module OS type                            *)
    mver       : DD;     (* Version of LX module                      *)
    mflags     : BITSET; (* Flag bits for the module                  *)
    mpages     : DD;     (* Number of pages in module                 *)
    eipobj     : DD;     (* The Object number to which the Entry      *)
                         (* Address is relative                       *)
    eip        : DD;     (*  Entry address of module                  *)
    espobj     : DD;     (* The Object number to  which the ESP is    *)
                         (*  relative                                 *)
    esp        : DD;     (* Starting stack address of module          *)
    psize      : DD;     (* The size of one page for this system      *)
    poffshift  : DD;     (* The shift left  bits  for  page offsets   *)
    fupssize   : DD;     (* Total  size  of  the  fixup information   *)
                         (* in bytes                                  *)
    fupschsum  : DD;     (* Checksum for fixup information            *)
    lderssize  : DD;     (* Size  of  memory  resident tables         *)
    lderchsum  : DD;     (* Checksum   for  loader  section           *)
    objtoffs   : DD;     (* Object Table offset                       *)
    objinmod   : DD;     (* Object Table Count                        *)
    objptoffs  : DD;     (* Object Page Table offset                  *)
    objipoff   : DD;     (* Object  Iterated  Pages  offset           *)
    restoffs   : DD;     (* Resource Table offset                     *)
    restentr   : DD;     (* Number  of entries  in Resource Table     *)
    resntoffs  : DD;     (* Resident Name Table offset                *)
    enttoffs   : DD;     (* Entry Table offset                        *)
    moddoffs   : DD;     (* Module  Format Directives Table offset    *)
    moddirvs   : DD;     (* Number of Module Format Directives in     *)
                         (* the Table                                 *)
    fupptoffs  : DD;     (* Fixup Page Table offset                   *)
    fuptroffs  : DD;     (* Fixup Record Table Offset                 *)
    impmodoffs : DD;     (* Import  Module Name Table offset          *)
    impmodentr : DD;     (* The number of entries in the Import       *)
                         (* Module Name Table                         *)
    impprtoffs : DD;     (* Import Procedure  Name Table offset       *)
    ppchkoffs  : DD;     (* Per-Page  Checksum  Table offset          *)
    dpoffs     : DD;     (* Data Pages Offset                         *)
    numprelp   : DD;     (* Number of Preload pages for this module   *)
    nonrestoffs: DD;     (* Non-Resident  Name  Table offset          *)
    nonrestlen : DD;     (* Number of bytes in the Non-resident       *)
                         (* name table                                *)
    nrestchksum: DD;     (* Non-Resident  Name  Table Checksum        *)
    autodsobj  : DD;     (* The Auto Data  Segment Object number      *)
    debinfoffs : DD;     (* Debug Information offset                  *)
    debinflen  : DD;     (* Debug Information length                  *)
    ninstpp    : DD;     (* Instance  pages  in  preload section      *)
    ninstdem   : DD;     (* Instance  pages  in  demand section       *)
    heapsize   : DD;     (* Heap size added to the Auto DS Object     *)
  END;

  OBJ    = RECORD
             vSize: DD;
             rAddr: DD;
             Flags: DD;
             pInd : DD;
             pEntr: DD;
             Reserved: DD;
           END;


VAR
  CodeSeg   : CARDINAL;
  EIP       : CARDINAL;
  HModule   : OS2.HMODULE;


PROCEDURE GetNum_EIPBase(name-: ARRAY OF CHAR; VAR eip: CARDINAL): CARDINAL;

  PROCEDURE fseek(file: rf.ChanId; pos: CARDINAL);
  VAR
    fp: rf.FilePos;
  BEGIN
    xfp.CardToPos(fp, pos);
    rf.SetPos(file, fp);
  END fseek;

VAR
  offs: CARDINAL;
  hdr : lx_header;
  file: rf.ChanId;
  io_res: rf.OpenResults;
BEGIN
  rf.OpenOld(file, name, rf.raw, io_res);
  IF io_res # rf.opened THEN RETURN 0 END;
  fseek(file,03CH);
  rio.Read(file, offs);
  fseek(file, offs);
  rio.Read(file, hdr);
  rf.Close(file);
  IF hdr.signature # 'LX' THEN
    RETURN 0;
  END;
  eip := hdr.eip;
  CodeSeg := hdr.eipobj;
  RETURN CodeSeg;
END GetNum_EIPBase;


PROCEDURE BuildInfo(HMod: CARDINAL; VAR inf: kt.EXEC_INFO): BOOLEAN;
VAR
  file : rf.ChanId;
  res  : rf.OpenResults;

  PROCEDURE fseek(pos: CARDINAL);
  VAR
    fp: rf.FilePos;
  BEGIN
    xfp.CardToPos(fp, pos);
    rf.SetPos(file, fp);
  END fseek;

  PROCEDURE read(VAR buf: ARRAY OF sys.LOC): BOOLEAN;
    VAR x: CARDINAL;
  BEGIN
    x:=SIZE(buf);
    ioc.RawRead(file, sys.ADR(buf), x, x);
    RETURN x=SIZE(buf);
  END read;

VAR
  offs, i: CARDINAL;
  hdr    : lx_header;
  obj    : OBJ;
  fp     : xfp.FilePos;
  ps     : CARDINAL;
  entry  : RECORD
             name: ARRAY [0..3] OF CHAR;
             offs: CARDINAL;
           END;
BEGIN
  fil.ExtractFileName (inf.full_name, inf.short_name);
  rf.OpenOld(file, inf.full_name, rf.raw, res);
  IF res # rf.opened THEN RETURN FALSE END;

  fseek(03CH);
  rio.Read(file, offs);
  fseek(offs);
  rio.Read(file, hdr);
  IF ( hdr.signature # 'LX') THEN RETURN FALSE END;

  WITH inf DO
    IF { 8, 9} <= hdr.mflags THEN
      app_type := kt.windowed
    ELSE
      app_type := kt.console
    END;

    N_Objects     := hdr.objinmod;
    IF hdr.eipobj # 0 THEN
      Code_Object :=  hdr.eipobj - 1;
    ELSE
      Code_Object :=  MAX(CARDINAL);
    END;

    NEW(Objects, N_Objects);

    fseek(offs+hdr.objtoffs);
    FOR i := 1 TO N_Objects DO
      rio.Read(file, obj);
      WITH Objects^[i-1] DO
        Attributes := kt.ATTRIBS{};
        RelocationBase    := obj.rAddr;
        IF (5 IN BITSET(obj.Flags)) AND NOT (7 IN BITSET(obj.Flags)) THEN
          Begin := MAX(CARDINAL)-1;
          End := MAX(CARDINAL);
        ELSE
          End := obj.vSize;
          IF 0 IN BITSET(obj.Flags) THEN
            INCL (Attributes, kt.read);
          END;
          IF 1 IN BITSET(obj.Flags) THEN
            INCL (Attributes, kt.write);
          END;
          IF 2 IN BITSET(obj.Flags) THEN
            INCL (Attributes, kt.execute);
            IF Code_Object = MAX(CARDINAL) THEN Code_Object := i-1; END;
          END;
          IF 13 IN BITSET(obj.Flags) THEN
            INCL( Attributes, kt.bit_32);
          END;
          WITH DbgBuffer DO
            Cmd   := OS2.DBG_C_NumToAddr;
            Pid   := PID;
            Value := i;
            MTE   := HMod;
          END;
          IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN RETURN FALSE END;
          IF DbgBuffer.Cmd # OS2.DBG_N_Success THEN
            IF DbgBuffer.Cmd = OS2.DBG_N_Error THEN
              ASSERT(FALSE, DbgBuffer.Value);
            ELSE
              ASSERT(FALSE);
            END;
          END;
          Begin := DbgBuffer.Addr;
          INC(End, Begin);
        END;
      END;
    END;
  END;

  inf.DebugInfoStart := 0;

  IF hdr.debinfoffs # 0 THEN
    inf.DebugInfoStart :=  hdr.debinfoffs;
    inf.DebugInfoSize := hdr.debinflen;

    fseek(inf.DebugInfoStart);
    IF NOT read(entry.name) THEN RETURN FALSE; END;
    COPY(entry.name, inf.DebugInfoTag);
  ELSE
    fp := rf.EndPos(file);
    IF NOT xfp.PosToCard(ps, fp) THEN RETURN FALSE END;
    IF ps <= 8 THEN RETURN FALSE END;
    fseek(ps-8);

    IF NOT read(entry) THEN RETURN FALSE END;
    COPY(entry.name, inf.DebugInfoTag);
    inf.DebugInfoSize := entry.offs;
    IF ps <= entry.offs THEN
      inf.DebugInfoSize := 0;
      inf.DebugInfoTag  := '';
    ELSE
      offs := ps - entry.offs;

      fseek(offs);
      IF read(entry) & (entry.name = inf.DebugInfoTag) THEN
        inf.DebugInfoStart := offs;
      ELSE
        inf.DebugInfoSize := 0;
        inf.DebugInfoTag  := '';
      END;
    END;
  END;

  rf.Close(file);
  RETURN TRUE;
END BuildInfo;



PROCEDURE LoadProgram (): BOOLEAN;

TYPE
  PSTRING = POINTER TO ARRAY [0..1024] OF CHAR;

VAR
  path: OS2.PCSZ;
  PATH: PSTRING;
  fprog_name: xs.String;

  start_data: OS2.STARTDATA;
  ErrorStr  : xs.String;
  Title     : xs.String;
  string    : xs.String;

  entry, i  : CARDINAL;
  com: dt.ComNo;
  inx: dt.INDEX;
  HModule_local: CARDINAL;

  component: dt.COMPONENT;
  begin, end: kt.ADDRESS;

BEGIN
  IF NOT fs.Exists(prog_name) THEN
    OS2.DosScanEnv("PATH", path);
    PATH := PSTRING(path);
    IF OS2.DosSearchPath(OS2.SEARCH_IGNORENETERRS + OS2.SEARCH_CUR_DIRECTORY,
                      PATH^, prog_name, fprog_name, SIZE(fprog_name)) # 0 THEN RETURN FALSE; END;
  ELSE
    fil.ModifyFileName(prog_name, fprog_name);
  END;

  COPY(fprog_name, prog_name);
  Title := 'The xProf application: ';
  xs.Append(prog_name, Title);
  WITH start_data DO
    Length        := SIZE(start_data);
    Related       := OS2.SSF_RELATED_CHILD;
    FgBg          := OS2.SSF_FGBG_BACK;
    TraceOpt      := OS2.SSF_TRACEOPT_TRACE;
    PgmTitle      := sys.ADR(Title);
    PgmName       := sys.ADR(prog_name);
    PgmInputs     := sys.ADR(prog_args);
    TermQ         := NIL;
    Environment   := NIL;
    InheritOpt    := OS2.SSF_INHERTOPT_PARENT;
    SessionType   := OS2.SSF_TYPE_DEFAULT;
    IconFile      := NIL;
    PgmHandle     := 0;
    PgmControl    := 0;
    InitXPos      := 0;
    InitYPos      := 0;
    Reserved      := 0;
    ObjectBuffer  := sys.ADR(ErrorStr);
    ObjectBuffLen := SIZE(ErrorStr);
  END;

  IF OS2.DosStartSession(start_data, SessionID, PID) # 0 THEN RETURN FALSE END;
  OS2.DosSelectSession(SessionID);

  WITH DbgBuffer DO
    Cmd   := OS2.DBG_C_Connect;
    Pid   := PID;
    Tid   := 0;
    Addr  := 1; -- 0
    Value := 1;
  END;
  OS2.DosReleaseMutexSem(ProgramAccess);
  IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN RETURN FALSE END;
  IF DbgBuffer.Cmd # OS2.DBG_N_Success THEN RETURN FALSE END;

  WITH DbgBuffer DO
    Cmd   := OS2.DBG_C_Go;
    Pid   := PID;
  END;
  IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN RETURN FALSE END;

  LOOP
    CASE DbgBuffer.Cmd OF
    | OS2.DBG_N_ModuleLoad :
      HModule_local := DbgBuffer.Value;
      OS2.DosQueryModuleName(HModule_local, SIZE(string), string);
      i := GetNum_EIPBase(string, entry);
      IF i#0 THEN
        component := dt.EmptyComponent;
        WITH component DO
          EI.Handle := DbgBuffer.Value;
          COPY(string, EI.full_name);
          ASSERT(BuildInfo(EI.Handle, EI));
          Name := 0;
          DI := dt.EmptyDebugInfo;
          bld.AddComponent(component);
          ASSERT(tls.FindComponentByHandle (EI.Handle, com));
          ASSERT(tls.GetIndex (com, inx));
          IF (EI.Objects # NIL) AND (EI.Code_Object < EI.N_Objects) THEN
            begin := EI.Objects^[EI.Code_Object].Begin;
            end := EI.Objects^[EI.Code_Object].End;
          ELSE
            begin := 0;
            end := 0;
          END;
          pb.AddTableComp (begin, end, EI.full_name, inx);
        END;

        HModule := HModule_local;
        EIP := entry;
        WITH DbgBuffer DO
          Cmd   := OS2.DBG_C_NumToAddr;
          Pid   := PID;
          Value := i;
          MTE   := HModule;
        END;
        IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN RETURN FALSE END;
        ASSERT(DbgBuffer.Cmd = 0);
        EXIT;
      END;
      WITH DbgBuffer DO
        Cmd   := OS2.DBG_C_Go;
        Pid   := PID;
      END;
    ELSE
      ASSERT(FALSE);
    END;
    IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN RETURN FALSE END;
  END;

  RETURN TRUE;
END LoadProgram;
<* END *>



<* IF env_target = 'x86nt' THEN *>

TYPE
  THREADS_TABLE_RECORD = RECORD
                           Id    : CARDINAL;
                           handle: win.HANDLE;
                         END;

  THREADS_TABLE = POINTER TO ARRAY OF THREADS_TABLE_RECORD;

  THREADS = RECORD
              Threads: THREADS_TABLE;
              count  : CARDINAL;
            END;

VAR
  Threads: THREADS;

CONST
  HN_Threads = 256;

PROCEDURE AddThread(Id: CARDINAL; handle: win.HANDLE);
VAR
  tmp: THREADS_TABLE;
BEGIN
  WITH Threads DO
    IF Threads = NIL THEN
      NEW(Threads, HN_Threads);
      count := 0;
    ELSIF count = HIGH(Threads^)+1 THEN
      NEW(tmp, HIGH(Threads^)+1 + HN_Threads);
      sys.MOVE(sys.ADR(Threads^), sys.ADR(tmp^), SIZE(Threads^));
      DISPOSE(Threads);
      Threads := tmp;
    END;
    Threads^[count].Id     := Id;
    Threads^[count].handle := handle;
    INC(count);
  END;
END AddThread;

PROCEDURE RemoveThread(Id: CARDINAL);
VAR
  i, j: CARDINAL;
BEGIN
  WITH Threads DO
    ASSERT(count > 0);
    FOR i := 0 TO count - 1 DO
      IF Threads^[i].Id = Id THEN
        FOR j := i+1 TO count - 1 DO
          Threads^[j-1] := Threads^[j];
        END;
        DEC(count);
        RETURN;
      END;
    END;
  END;
END RemoveThread;

PROCEDURE SuspendThreads;
VAR
  i: CARDINAL;
BEGIN
  WITH Threads DO
    IF count > 0 THEN
      FOR i := 0 TO count - 1 DO
        win.SuspendThread(Threads^[i].handle);
      END;
    END;
  END;
END SuspendThreads;

PROCEDURE ResumeThreads;
VAR
  i: CARDINAL;
BEGIN
  WITH Threads DO
    IF count > 0 THEN
      FOR i := 0 TO count - 1 DO
        win.ResumeThread(Threads^[i].handle);
      END;
    END;
  END;
END ResumeThreads;


VAR
  H_Process, H_Thread: win.HANDLE;


PROCEDURE Get(source: CARDINAL; dest: sys.ADDRESS; len: CARDINAL): BOOLEAN;
VAR
  read_len: CARDINAL;
BEGIN
  IF NOT win.ReadProcessMemory(H_Process, win.LPCVOID(source), dest, len, read_len) THEN RETURN FALSE END;
<* PUSH *>
<* WOFF304+ *>
  RETURN read_len = len;
<* POP *>
END Get;

VAR
  di: win.DEBUG_EVENT;


PROCEDURE BuildObjects (base: CARDINAL; VAR info: kt.EXEC_INFO): BOOLEAN;
VAR
  CodeBegin: CARDINAL;
  pos, i   : CARDINAL;
  header   : win.IMAGE_NT_HEADERS;
  Section  : win.IMAGE_SECTION_HEADER;

BEGIN
  CodeBegin := 0;
  pos := CARDINAL(base);
  IF NOT Get(pos, sys.ADR(CodeBegin), 2) THEN RETURN FALSE; END;
  IF CodeBegin # win.IMAGE_DOS_SIGNATURE THEN RETURN FALSE; END;
  INC(pos, SIZE(win.IMAGE_DOS_HEADER));
  IF NOT Get(pos-4, sys.ADR(CodeBegin), 4) THEN RETURN FALSE; END;
  pos := CARDINAL(base) + CodeBegin;
  IF NOT Get(pos, sys.ADR(header), SIZE(header)) THEN RETURN FALSE; END;
  INC(pos, SIZE(header));
  CodeBegin := CARDINAL(base) + header.OptionalHeader.BaseOfCode;
  IF header.Signature # win.IMAGE_NT_SIGNATURE THEN RETURN FALSE; END;

  NEW(info.Objects, header.FileHeader.NumberOfSections);
  info.N_Objects := header.FileHeader.NumberOfSections;

  FOR i := 1 TO header.FileHeader.NumberOfSections DO
    IF NOT Get(pos, sys.ADR(Section), SIZE(Section)) THEN RETURN FALSE; END;
    INC(pos, SIZE(Section));
    WITH info.Objects^[i-1] DO
      Begin := Section.VirtualAddress + base;
      End    := Begin + Section.VirtualSize - 1;
      IF Begin = CodeBegin THEN
        info.Code_Object := i - 1;
      END;
      Attributes := kt.ATTRIBS{};

      IF BITSET(win.IMAGE_SCN_MEM_EXECUTE)*BITSET(Section.Characteristics) # {} THEN
        INCL(Attributes, kt.execute);
      END;
      IF BITSET(win.IMAGE_SCN_MEM_READ)*BITSET(Section.Characteristics) # {} THEN
        INCL(Attributes, kt.read);
      END;
      IF BITSET(win.IMAGE_SCN_MEM_WRITE)*BITSET(Section.Characteristics) # {} THEN
        INCL(Attributes, kt.write);
      END;
      IF BITSET(win.IMAGE_SCN_MEM_16BIT)*BITSET(Section.Characteristics) = {} THEN
        INCL(Attributes, kt.bit_32);
      END;
    END;
  END;
  RETURN TRUE;
END BuildObjects;


PROCEDURE LoadProgram (): BOOLEAN;
VAR
  Title, PATH, f_name: ARRAY [0..1024] OF CHAR;
  a   : win.PSTR;
  Cmd_Line: xs.String;
  startup_info: win.STARTUPINFO;
  pi: win.PROCESS_INFORMATION;
  component: dt.COMPONENT;
  com: dt.ComNo;
  inx: dt.INDEX;

  begin, end: kt.ADDRESS;

BEGIN
  IF NOT fs.Exists(prog_name) THEN
    arg.String("PATH", PATH);
    IF win.SearchPath(PATH, prog_name, NIL, SIZE(f_name), f_name, a) = 0 THEN RETURN FALSE; END;
  ELSE
    fil.ModifyFileName(prog_name, f_name);
  END;

  Title := 'The XDS Profiler application: ';
  xs.Append(f_name, Title);

  WITH startup_info DO
    cb              := SIZE(startup_info);
    lpReserved      := NIL;
    lpDesktop       := NIL;
    lpTitle         := sys.ADR(Title);
    dwFlags         := win.STARTF_SET{};
    cbReserved2     := 0;
    lpReserved2     := NIL;
  END;

  COPY(f_name, prog_name);
  COPY(f_name, Cmd_Line);
  xs.Append(' ', Cmd_Line);
  xs.Append(prog_args, Cmd_Line);
  IF NOT win.CreateProcess(NIL, sys.ADR(Cmd_Line), NIL, NIL, TRUE,
                    win.DEBUG_ONLY_THIS_PROCESS+win.CREATE_NEW_CONSOLE+
                    win.NORMAL_PRIORITY_CLASS, NIL, NIL,
                    startup_info, pi)  THEN RETURN FALSE END;

  LOOP
    win.WaitForDebugEvent(di , win.INFINITE);
    WITH di DO
      CASE dwDebugEventCode OF
      | win.CREATE_PROCESS_DEBUG_EVENT:
        WITH CreateProcessInfo DO
          H_Process := hProcess;
          H_Thread  := hThread;

          AddThread (pi.dwThreadId, hThread);
          component := dt.EmptyComponent;
          IF NOT BuildObjects (CARDINAL(lpBaseOfImage), component.EI) THEN RETURN FALSE; END;
          component.EI.Handle := CARDINAL(lpBaseOfImage);
          component.EI.hFile  := CARDINAL(hFile);
          ASSERT(prg.RetrieveModuleInfo (H_Process, component.EI, FALSE) = 0);
          WITH component DO
            EI.Handle := CARDINAL(H_Process);
            EI.hFile  := CARDINAL(hFile);
            COPY(prog_name, EI.full_name);
            Name := 0;
            DI := dt.EmptyDebugInfo;
            bld.AddComponent(component);
            ASSERT(tls.FindComponentByHandle (EI.Handle, com));
            ASSERT(tls.GetIndex (com, inx));
            IF (EI.Objects # NIL) AND (EI.Code_Object < EI.N_Objects) THEN
              begin := EI.Objects^[EI.Code_Object].Begin;
              end := EI.Objects^[EI.Code_Object].End;
            ELSE
              begin := 0;
              end := 0;
            END;
            pb.AddTableComp (begin, end, EI.full_name, inx);
          END;

          EXIT;
        END;
      ELSE
      END;
    END;
    ASSERT(win.ContinueDebugEvent(di.dwProcessId, di.dwThreadId, win.DBG_CONTINUE));
  END;
  RETURN TRUE;
END LoadProgram;


PROCEDURE GetIP (H_Thread: win.HANDLE; VAR IP: CARDINAL): BOOLEAN;
VAR
  Context: win.CONTEXT;
BEGIN
  Context.ContextFlags := win.CONTEXT_CONTROL;
  IF NOT win.GetThreadContext(H_Thread, Context) THEN RETURN FALSE; END;
  IP := Context.Eip;
  RETURN  TRUE
END GetIP;

<* ELSIF env_target = 'x86os2' THEN *>


PROCEDURE GetIP (VAR IP: CARDINAL): BOOLEAN;
VAR
  DbgBuffer: OS2.uDB_t;
BEGIN
  WITH DbgBuffer DO
    Cmd := OS2.DBG_C_ReadReg;
    Pid := PID;
    Tid := 0;
  END;
  IF (OS2.DosDebug(sys.ADR(DbgBuffer)) # 0) OR (DbgBuffer.Cmd # OS2.DBG_N_Success) THEN
    IP := 0;
    RETURN FALSE;
  ELSE
    IP := DbgBuffer.EIP;
    RETURN TRUE;
  END;
END GetIP;

VAR
  TID_SH: OS2.TID;

<* PUSH *>
<* WOFF301+ *>
PROCEDURE ["SysCall"] Sleepyhead (p: OS2.ULONG);
<* POP *>

BEGIN
  OS2.DosSetPriority(OS2.PRTYS_THREAD, OS2.PRTYC_TIMECRITICAL, OS2.PRTYD_MAXIMUM, 0);
<* PUSH *>
<* WOFF310+ *>
  LOOP
<* POP *>
    OS2.DosSleep(SleepTime);
    WITH DbgBuffer DO
      Cmd := OS2.DBG_C_Stop;
      Pid := PID;
    END;
    OS2.DosDebug(sys.ADR(DbgBuffer));
  END;
END Sleepyhead;


PROCEDURE SnapShot;
VAR
  component: dt.COMPONENT;
  com      : dt.ComNo;
  inx      : dt.INDEX;
  tmp      : CARDINAL;
  snapshot : pt.SNAPSHOT;
  IP       : CARDINAL;

  begin, end: kt.ADDRESS;

BEGIN
  OS2.DosCreateMutexSem(NIL, ProgramAccess, 0, TRUE);
  OS2.DosCreateThread(TID_SH, Sleepyhead, 0, 0, 32000);

  LOOP
    WITH DbgBuffer DO
      Cmd := OS2.DBG_C_Go;
      Pid := PID;
    END;
    IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN EXIT; END;
    CASE DbgBuffer.Cmd OF
    | OS2.DBG_N_Success, OS2.DBG_N_AsyncStop:
      WITH snapshot DO
        IF GetIP (IP) THEN
          IF tls.FindComObjByAddr (IP, com, tmp) THEN
            Object := tmp;
            Offset := IP - dt.Components.Components^[com].EI.Objects^[Object].Begin;
            ASSERT(tls.GetIndex (com, Index));
            pb.AddTableSnapshot(snapshot);
          END;
        END;
      END;

    | OS2.DBG_N_ModuleLoad:
      component := dt.EmptyComponent;
      WITH component DO
        EI.Handle := DbgBuffer.Value;
        IF OS2.DosQueryModuleName(EI.Handle, SIZE(EI.full_name), EI.full_name) = 0 THEN
          ASSERT(BuildInfo(EI.Handle, EI));
          Name := 0;
          DI := dt.EmptyDebugInfo;
          bld.AddComponent(component);
          ASSERT(tls.FindComponentByHandle (EI.Handle, com));
          ASSERT(tls.GetIndex (com, inx));
          IF (EI.Objects # NIL) AND (EI.Code_Object < EI.N_Objects) THEN
            begin := EI.Objects^[EI.Code_Object].Begin;
            end := EI.Objects^[EI.Code_Object].End;
          ELSE
            begin := 0;
            end := 0;
          END;
          pb.AddTableComp (begin, end, EI.full_name, inx);
        END;
      END;

    | OS2.DBG_N_ModuleFree, OS2.DBG_N_ThreadCreate, OS2.DBG_N_ThreadTerm:

    | OS2.DBG_N_Exception, OS2.DBG_N_ProcTerm:
      EXIT;

    | OS2.DBG_N_Error:
      ASSERT(FALSE, DbgBuffer.Value);

    ELSE
      ASSERT(FALSE, DbgBuffer.Cmd);
    END;
  END;
END SnapShot;

<* END *>


<* IF env_target = 'x86nt' THEN *>

VAR
  cont_flag: CARDINAL;

PROCEDURE SnapShot;
VAR
  component: dt.COMPONENT;
  com      : dt.ComNo;
  tmp      : CARDINAL;
  inx      : dt.INDEX;
  snapshot : pt.SNAPSHOT;
  IP       : CARDINAL;

  begin, end: kt.ADDRESS;

BEGIN
  cont_flag := win.DBG_CONTINUE;
  LOOP
    ASSERT(win.ContinueDebugEvent(di.dwProcessId, di.dwThreadId, cont_flag));
    cont_flag := win.DBG_CONTINUE;
    LOOP
      IF win.WaitForDebugEvent(di, SleepTime) THEN
        EXIT;
      END;
      SuspendThreads;
      WITH snapshot DO
        IF GetIP(H_Thread, IP) THEN
          IF tls.FindComObjByAddr (IP, com, tmp) THEN
            Object := tmp;
            Offset := IP - dt.Components.Components^[com].EI.Objects^[Object].Begin;
            ASSERT(tls.GetIndex (com, Index));
            pb.AddTableSnapshot(snapshot);
          END;
        END;
      END;
      ResumeThreads;
    END;
    WITH di DO
      CASE dwDebugEventCode OF
      | win.EXCEPTION_DEBUG_EVENT:
        WITH Exception DO
          WITH ExceptionRecord DO
            CASE ExceptionCode OF
            | win.EXCEPTION_SINGLE_STEP:
            | win.EXCEPTION_BREAKPOINT:
            ELSE
              cont_flag := win.DBG_EXCEPTION_NOT_HANDLED;
              IF (dwFirstChance = 0) OR (ExceptionFlags = win.EXCEPTION_NONCONTINUABLE) THEN
                RETURN;
              END;
            END;
          END;
        END;

      | win.LOAD_DLL_DEBUG_EVENT:
        component := dt.EmptyComponent;
        WITH component DO
          Name := 0;
          WITH EI DO
            Handle := kt.ADDRESS(LoadDll.lpBaseOfDll);
            hFile  := CARDINAL(LoadDll.hFile);
          END;
          ASSERT(BuildObjects(CARDINAL(LoadDll.lpBaseOfDll), EI));
          ASSERT(prg.RetrieveModuleInfo (H_Process, EI, TRUE) = 0);
          DI := dt.EmptyDebugInfo;
          bld.AddComponent(component);
          ASSERT(tls.FindComponentByHandle (EI.Handle, com));
          ASSERT(tls.GetIndex (com, inx));
          IF (EI.Objects # NIL) AND (EI.Code_Object < EI.N_Objects) THEN
            begin := EI.Objects^[EI.Code_Object].Begin;
            end := EI.Objects^[EI.Code_Object].End;
          ELSE
            begin := 0;
            end := 0;
          END;
          pb.AddTableComp (begin, end, EI.full_name, inx);
        END;

      | win.UNLOAD_DLL_DEBUG_EVENT:
<* PUSH *>
<* WOFF903+ *>
        IF tls.RemoveComponent (kt.ADDRESS(UnloadDll.lpBaseOfDll)) THEN END;
<* POP *>

      | win.CREATE_THREAD_DEBUG_EVENT:
          AddThread(di.dwThreadId, CreateThread.hThread);

      | win.EXIT_THREAD_DEBUG_EVENT:
          RemoveThread(di.dwThreadId);

      | win.EXIT_PROCESS_DEBUG_EVENT:
        RETURN;

      | win.OUTPUT_DEBUG_STRING_EVENT:

      ELSE
        RETURN;
      END;
    END;
  END;
END SnapShot;

<* END *>


PROCEDURE Init;
BEGIN
  prog_name := '';
  prog_args := '';
  SleepTime := SLEEP_TIME_DEFAULT;
<* IF env_target = 'x86nt' THEN *>
  Threads := THREADS {NIL, 0};
<* END *>
END Init;


PROCEDURE ExecuteProgram;

  MODULE WriteProtocol;

  IMPORT xs, fil, pt, pb, prog_name, printf;

  VAR
    tmp: xs.String;

  BEGIN
  FINALLY
    fil.ExtractFileName (prog_name, tmp);
    IF pb.WriteProtocol (tmp, pt.TRACE_EXECUTION) < 0 THEN
      printf('Can not write trace file. (%s)\n', tmp);
    ELSE
      printf('Trace file was successfully created. (%s)\n', tmp);
    END;
  END WriteProtocol;


BEGIN
  printf("Program %s is running...\n", prog_name);
  SnapShot;
  printf("Program terminated.\n\n");
EXCEPT
  printf("Ctrl-Break pressed, program stopped.\n\n");
  RETURN;
END ExecuteProgram;


BEGIN
  Copyright;
  Init;
  ParseCommandLine;
  IF NOT LoadProgram() THEN
    printf('Program %s not loaded.\n', prog_name);
    HALT (1);
  END;
  ExecuteProgram;
END xProf.
