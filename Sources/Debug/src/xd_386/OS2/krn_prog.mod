<* Storage+ *>
<* ALIGNMENT="1" *>
IMPLEMENTATION MODULE Krn_Prog;

IMPORT Printf;
<* IF PM THEN *>
IMPORT Console;
<* END *>

IMPORT sys := SYSTEM;
IMPORT io  := InOut;
IMPORT rf  := RndFile;
IMPORT ioc := IOChan;
IMPORT xfp := xFilePos;
IMPORT rio := RawIO;
IMPORT fs  := FileSys;
IMPORT fil := File;
IMPORT brk := CtrlC;
IMPORT fmt := FormStr;

IMPORT OS2;

IMPORT kt  := KrnTypes;
IMPORT mem := Krn_Mem;
IMPORT utl := Krn_Dbg;

IMPORT msg := MsgNo;
IMPORT opt := Options;
IMPORT srt := Sort;
IMPORT xStr;

IMPORT dt  := DI_Types;

IMPORT eve := Events;
IMPORT emm := Exe_Mem;
IMPORT rmt := Remote;

IMPORT thr := Threads;

IMPORT re  := ReadExp;

FROM Krn_Dbg IMPORT DbgBuffer, PID, TID, HModule, MyPID, MyHwnd, Program_started, Program_started_2, Program_stopped;

VAR
  info   : kt.EXEC_INFO;
  Loaded : BOOLEAN;
  Title: xStr.String;
  ErrorStr: xStr.String;
  start_data : OS2.STARTDATA;
  Args: xStr.String;

TYPE
  DB = sys.CARD8;

  DW = sys.CARD16;

  DD = sys.CARD32;

  LX_HEADER = RECORD
    signature: ARRAY [0..1] OF CHAR;
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

PROCEDURE ReadExport (full_name: ARRAY OF CHAR; VAR exp: kt.EXPORTS): BOOLEAN;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.ReadExport (full_name, exp);
  ELSE
    RETURN re.ReadExport (full_name, exp);
  END;
END ReadExport;

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
  hdr    : LX_HEADER;
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
  IF res # rf.opened THEN RETURN FALSE; END;

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

    IF hdr.eipobj # 0 THEN
      Code_Object :=  hdr.eipobj - 1;
    ELSE
      Code_Object :=  MAX(CARDINAL);
    END;

    N_Objects := hdr.objinmod;
    IF N_Objects = 0 THEN RETURN FALSE; END;
    NEW(Objects, N_Objects);

    fseek(offs+hdr.objtoffs);
    FOR i := 1 TO N_Objects DO
      rio.Read(file, obj);
      WITH Objects^[i-1] DO
        Attributes := kt.ATTRIBS{};
        RelocationBase    := obj.rAddr;
        IF 0 IN BITSET(obj.Flags) THEN
          INCL( Attributes, kt.read);
        END;
        IF 1 IN BITSET(obj.Flags) THEN
          INCL( Attributes, kt.write);
        END;
        IF 2 IN BITSET(obj.Flags) THEN
          INCL( Attributes, kt.execute);
          IF Code_Object = MAX(CARDINAL) THEN Code_Object := i-1; END;
        END;
        IF 13 IN BITSET(obj.Flags) THEN
          INCL( Attributes, kt.bit_32);
        END;
        IF (4 IN BITSET(obj.Flags)) AND (i-1 # Code_Object) THEN
          Begin := MAX(CARDINAL)-1;
          End := MAX(CARDINAL);
        ELSE
          End := obj.vSize;
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
    IF Code_Object # MAX(CARDINAL) THEN
      StartupEntry := Objects^[Code_Object].Begin+hdr.eip;
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

PROCEDURE GetProgramInfo (VAR inf: kt.EXEC_INFO): BOOLEAN;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.GetProgramInfo (inf);
  END;

  inf := info;
  RETURN TRUE;
END GetProgramInfo;



PROCEDURE PreLoad(): BOOLEAN;
VAR
  event: eve.EVENT;
BEGIN
  TID := MAX(CARDINAL);
  HModule := MAX(CARDINAL);

  WITH DbgBuffer DO
    Cmd   := OS2.DBG_C_Connect;
    Pid   := PID;
    Tid   := 0;
    Addr  := 0; -- 1
    Value := 1;
  END;
  IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN RETURN FALSE END;
  IF DbgBuffer.Cmd # OS2.DBG_N_Success THEN RETURN FALSE END;

  WITH DbgBuffer DO
    Cmd   := OS2.DBG_C_Stop;
    Pid   := PID;
  END;

  LOOP
    IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN RETURN FALSE END;

    CASE DbgBuffer.Cmd OF
    | OS2.DBG_N_Success:
      EXIT;

    | OS2.DBG_N_ModuleLoad :
      IF HModule = MAX(CARDINAL) THEN
        HModule := DbgBuffer.Value;
        IF NOT BuildInfo(HModule, info) THEN END;
      ELSE
        WITH event DO
          Event := eve.CompCreated;
          sys.FILL(sys.ADR(Component), 0, SIZE(Component));
          Component.Handle := DbgBuffer.Value;
          IF OS2.DosQueryModuleName(Component.Handle, SIZE(Component.full_name), Component.full_name) = 0 THEN
            IF NOT BuildInfo(Component.Handle, Component) THEN END;
            ASSERT(eve.AddEvent(event));
          END;
        END;
      END;

    | OS2.DBG_N_ThreadCreate:
      WITH DbgBuffer DO
        IF TID = MAX(CARDINAL) THEN TID := Tid; END;

        WITH event DO
          Event   := eve.ThreadCreated;
          THandle := Tid;
        END;
        ASSERT(eve.AddEvent(event));
        thr.AddThread (event.THandle);
      END;

    ELSE
      io.WriteInt(DbgBuffer.Cmd, 0); io.WriteLn;
      ASSERT(FALSE);
    END;
    WITH DbgBuffer DO
      Cmd   := OS2.DBG_C_Stop;
      Pid   := PID;
    END;
  END;

  RETURN TRUE;
END PreLoad;


PROCEDURE ["SysCall"] PMHack(dummy: OS2.ULONG);
VAR
   hab : OS2.HAB;
   hmq : OS2.HMQ;
   qmsg: OS2.QMSG;          (* message structure *)
   hwnd: OS2.HWND;
   pid : OS2.PID;
   tid : OS2.TID;
BEGIN
  hab := OS2.WinInitialize(0);
  ASSERT(hab # OS2.NULLHANDLE);
  hmq := OS2.WinCreateMsgQueue(hab, 0);
  ASSERT(hmq # OS2.NULLHANDLE);
  LOOP
    OS2.DosWaitEventSem(Program_stopped, OS2.SEM_INDEFINITE_WAIT);
    ASSERT(OS2.WinSetCapture(OS2.HWND_DESKTOP, OS2.HWND_THREADCAPTURE));
    LOOP
      OS2.WinGetMsg(hab, qmsg, 0, 0, 0);
      hwnd := OS2.WinWindowFromPoint(OS2.HWND_DESKTOP, qmsg.ptl, TRUE);
      IF (hwnd # OS2.NULLHANDLE) AND
          OS2.WinQueryWindowProcess(hwnd, pid, tid) AND
          (pid = MyPID) THEN
        OS2.WinSetCapture(OS2.HWND_DESKTOP, OS2.NULLHANDLE);
        OS2.WinPostMsg(hwnd, qmsg.msg, qmsg.mp1, qmsg.mp2);
        OS2.WinSetCapture(OS2.HWND_DESKTOP, OS2.HWND_THREADCAPTURE);
      END;
      IF OS2.DosWaitEventSem(Program_started_2, OS2.SEM_IMMEDIATE_RETURN) = 0 THEN EXIT END;
    END;
    ASSERT(OS2.WinSetCapture(OS2.HWND_DESKTOP, OS2.NULLHANDLE));
    WHILE OS2.WinPeekMsg(hab, qmsg, 0, 0, 0, OS2.PM_REMOVE) DO END;
  END;
END PMHack;

VAR
  PMHackStarted: BOOLEAN;
  PMHackTID    : OS2.TID;

PROCEDURE Start_PM_Hack(): BOOLEAN;
BEGIN
  utl.StartSmartSelector;
  RETURN OS2.DosCreateThread(PMHackTID,
                             PMHack,
                             0,
                             OS2.CREATE_READY+OS2.STACK_COMMITTED,
                             8000H) = OS2.NO_ERROR;
END Start_PM_Hack;


PROCEDURE StartProgram (name-: ARRAY OF CHAR; args-: ARRAY OF CHAR): CARDINAL;
VAR
  PATH: xStr.txt_ptr;
  a   : OS2.PCSZ;
  rc  : OS2.APIRET;
  pid : OS2.PID;
  sid : OS2.ULONG;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.StartProgram (name, args);
  END;

  IF Loaded THEN
    RETURN msg.ProgramAlredyStarted;
  END;

  info := kt.EmptyExecInfo;

  OS2.DosScanEnv("PATH", a);
  PATH := xStr.txt_ptr(a);
  IF NOT fs.Exists(name) THEN
    IF OS2.DosSearchPath(OS2.SEARCH_IGNORENETERRS + OS2.SEARCH_CUR_DIRECTORY,
                      PATH^, name, info.full_name, SIZE(info.full_name)) # 0 THEN

      RETURN msg.ProgramNotFound;
    END;
  ELSE
    fil.ModifyFileName(name, info.full_name);
  END;

  Title := 'The XD debuggee: ';
  xStr.Append(info.full_name, Title);
  WITH start_data DO
    Length        := SIZE(start_data);
    Related       := OS2.SSF_RELATED_CHILD;
    FgBg          := OS2.SSF_FGBG_BACK;
    TraceOpt      := OS2.SSF_TRACEOPT_TRACE;
    PgmTitle      := sys.ADR(Title);
    PgmName       := sys.ADR(info.full_name);
    PgmInputs     := sys.ADR(args);
    TermQ         := NIL;
    Environment   := NIL;
    InheritOpt    := OS2.SSF_INHERTOPT_PARENT;
    SessionType   := OS2.SSF_TYPE_DEFAULT;
    IconFile      := NIL;
    PgmHandle     := 0;
    PgmControl    := 0;
    InitXPos      := 0;
    InitYPos      := 0;
    InitXSize     := 0;
    InitYSize     := 0;
    Reserved      := 0;
    ObjectBuffer  := sys.ADR(ErrorStr);
    ObjectBuffLen := SIZE(ErrorStr);
  END;

  rc := OS2.DosStartSession(start_data, sid, pid);
  IF rc # 0 THEN
    RETURN msg.CannotCreateProcess;
  END;
  utl.NewDebuggee (pid,sid);
  utl.SwitchToDebuggee;
  utl.SwitchToDebugger;

  COPY(args, Args);

  IF NOT PreLoad() THEN RETURN msg.CannotCreateProcess; END;


<* IF PM THEN *>
  IF NOT opt.KernelInRemoteMode THEN
    IF info.app_type = kt.windowed THEN
      IF NOT Console.Start_PM_Hack() THEN --*FSA
        RETURN msg.ProgramNotFound;
      END;
    END;
  END;
<* END *>


  Loaded := TRUE;
  RETURN 0;
END StartProgram;

PROCEDURE GetDebugInfo (info-: kt.EXEC_INFO; info_addr: sys.ADDRESS): BOOLEAN;
VAR
  file: rf.ChanId;
  res : rf.OpenResults;
  fp  : xfp.FilePos;
  len : CARDINAL;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.GetDebugInfo (info, info_addr);
  END;

  rf.OpenOld(file, info.full_name, rf.raw, res);
  IF res # rf.opened THEN RETURN FALSE END;

  IF info.DebugInfoStart = 0 THEN RETURN FALSE END;

  xfp.CardToPos(fp, info.DebugInfoStart);
  rf.SetPos(file, fp);
  ioc.RawRead (file, info_addr, info.DebugInfoSize, len);

  rf.Close(file);

  RETURN len = info.DebugInfoSize;
END GetDebugInfo;

(* Подготовить выполнение программы с указанной "точки входа" *)
PROCEDURE RestartProgram (): BOOLEAN;
VAR
  pid : OS2.PID;
  sid : OS2.ULONG;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.RestartProgram ();
  END;
  UnloadProgram();
  IF OS2.DosStartSession (start_data, sid, pid) # 0 THEN
    RETURN FALSE;
  END;
  utl.NewDebuggee (pid,sid);
  utl.SwitchToDebuggee;
  utl.SwitchToDebugger;
  RETURN PreLoad();
END RestartProgram;

<* IF DEFINED(xd_debug) & xd_debug THEN *>
PROCEDURE PrintNotification;
VAR
  XCPT_Report: OS2.EXCEPTIONREPORTRECORD;
  str: ARRAY [0..255] OF CHAR;
BEGIN

  IF opt.Debug(opt.Another) THEN

    io.WriteString("---------------------------------------------"); io.WriteLn;
    CASE DbgBuffer.Cmd OF
    | -1:
      io.WriteString("Error ");io.WriteInt(DbgBuffer.Value, 0); io.WriteLn;
    | -7:
      CASE DbgBuffer.Value OF
      | 0:
        CASE DbgBuffer.Buffer OF
        | OS2.XCPT_BREAKPOINT:
          io.WriteString("BreakPoint Pre-First Notification, Address = ");
          io.WriteHex(DbgBuffer.Addr, 0); io.WriteLn;
        | OS2.XCPT_SINGLE_STEP:
          io.WriteString("SingleStep Pre-First Notification, Address = ");
          io.WriteHex(DbgBuffer.Addr, 0); io.WriteLn;
        ELSE
          io.WriteString("Unexpected Exception"); io.WriteLn;
        END;
      | 1:
        io.WriteString("Exception First Notification"); io.WriteLn;
        io.WriteString("  Address of exception      = "); io.WriteHex(DbgBuffer.Addr,   0); io.WriteLn;
        io.WriteString("  Address of report record  = "); io.WriteHex(DbgBuffer.Buffer, 0); io.WriteLn;
        io.WriteString("  Address of context record = "); io.WriteHex(DbgBuffer.Len,    0); io.WriteLn;
        ASSERT(mem.Get(DbgBuffer.Buffer, sys.ADR(XCPT_Report), SIZE(XCPT_Report)));
        WITH XCPT_Report DO
          io.WriteString("    XCPT Report:"); io.WriteLn;
          io.WriteString("      Exception number "); io.WriteHex(ExceptionNum, 0);  io.WriteLn;
          io.WriteString("      fHandlerFlags "); io.WriteHex(fHandlerFlags, 0); io.WriteLn;
          io.WriteString("      NestedExceptionReportRecord "); io.WriteHex(CARDINAL(NestedExceptionReportRecord), 0); io.WriteLn;
          io.WriteString("      ExceptionAddress "); io.WriteHex(CARDINAL(ExceptionAddress), 0); io.WriteLn;
          io.WriteString("      cParameters "); io.WriteCard(cParameters, 0); io.WriteLn;
        END;

      | 2:
        io.WriteString("Exception Last Notification"); io.WriteLn;
        io.WriteString("  Address of exception      = "); io.WriteHex(DbgBuffer.Addr,   0); io.WriteLn;
        io.WriteString("  Address of report record  = "); io.WriteHex(DbgBuffer.Buffer, 0); io.WriteLn;
        io.WriteString("  Address of context record = "); io.WriteHex(DbgBuffer.Len,    0); io.WriteLn;
      | 3:
        io.WriteString("Exception Stack Invalid"); io.WriteLn;
        io.WriteString("  Address of exception = "); io.WriteHex(DbgBuffer.Addr,   0); io.WriteLn;
        io.WriteString("  Exception Number     = "); io.WriteHex(DbgBuffer.Buffer, 0); io.WriteLn;
      ELSE
        io.WriteString("Unexpected DbgBuffer.Value"); io.WriteLn;
      END;
    | OS2.DBG_N_ModuleLoad:
      io.WriteString("New Module Loaded"); io.WriteLn;
      io.WriteString("  HModule = "); io.WriteHex(DbgBuffer.Value, 0); io.WriteLn;
      str := '';
      OS2.DosQueryModuleName(DbgBuffer.Value, SIZE(str), str);
      io.WriteString("  Module Name is "); io.WriteString(str); io.WriteLn;
    | -9:
      io.WriteString("CoProcessorError = "); io.WriteInt(DbgBuffer.Value, 0); io.WriteLn;
    | -14:
      io.WriteString("WatchPoint hit");io.WriteLn;
      io.WriteString("  Address = "); io.WriteHex(DbgBuffer.Addr,   0); io.WriteLn;
      io.WriteString("  PID     = "); io.WriteHex(DbgBuffer.Value, 0); io.WriteLn;
      io.WriteString("  TID     = "); io.WriteHex(DbgBuffer.Len, 0); io.WriteLn;
      io.WriteString("  HMODULE = "); io.WriteHex(DbgBuffer.MTE, 0); io.WriteLn;
      io.WriteString("  Index   = "); io.WriteHex(DbgBuffer.Index, 0); io.WriteLn;
    | -15:
      io.WriteString("New Thread Creation, TID = "); io.WriteHex(DbgBuffer.Tid, 0); io.WriteLn;
    ELSE
      io.WriteString("Unknown Yet ");io.WriteInt(DbgBuffer.Cmd, 0);io.WriteLn;
    END;
  END;
END PrintNotification;
<* END *>

VAR
  WasException: BOOLEAN;

PROCEDURE ProcessEvents (add_step: BOOLEAN);
VAR
  XCPT_Report: OS2.EXCEPTIONREPORTRECORD;
  ret        : CARDINAL;
  event      : eve.EVENT;

BEGIN
  WasException := FALSE;
  LOOP
    IF DbgBuffer.Cmd = 0 THEN EXIT END;

<* IF DEFINED(xd_debug) & xd_debug THEN *>
    PrintNotification;
<* END *>

    CASE DbgBuffer.Cmd OF
    | OS2.DBG_N_Error:
      event.Event   := eve.InternalError;
      event.ErrorNo := DbgBuffer.Value;
      ASSERT(eve.AddEvent(event));
      RETURN;

    | OS2.DBG_N_Exception:
      CASE DbgBuffer.Value OF
      | OS2.DBG_X_PRE_FIRST_CHANCE:
        CASE DbgBuffer.Buffer OF
        | OS2.XCPT_SINGLE_STEP:
          IF add_step THEN
            event.IP    := DbgBuffer.Addr;
            event.Event := eve.SingleStep;
            ASSERT(eve.AddEvent(event));
          END;
          WITH DbgBuffer DO
            Cmd   := OS2.DBG_C_Continue;
            Pid   := PID;
            Tid   := TID;
            Value := INTEGER(OS2.XCPT_CONTINUE_STOP);
          END;

        | OS2.XCPT_BREAKPOINT:
          event.IP            := DbgBuffer.Addr;
          event.Event         := eve.BreakpointHit;
          event.BreakpointInd := 0;
          ASSERT(eve.AddEvent(event));
          WITH DbgBuffer DO
            Cmd   := OS2.DBG_C_Continue;
            Pid   := PID;
            Tid   := TID;
            Value := INTEGER(OS2.XCPT_CONTINUE_STOP);
          END;
        END;

      | OS2.DBG_X_FIRST_CHANCE:
        ASSERT(mem.Get(DbgBuffer.Buffer, sys.ADR(XCPT_Report), SIZE(XCPT_Report)));
        CASE XCPT_Report.ExceptionNum OF
        | OS2.XCPT_PROCESS_TERMINATE:
          IF DbgBuffer.Tid = 1 THEN
            event.IP            := kt.ADDRESS(XCPT_Report.ExceptionAddress);
            event.Event         := eve.Exception;
            event.Exception_ID  := eve.ProgramException;
            event.XCPT_INFO_1   := CARDINAL(0);
            event.XCPT_INFO_2   := XCPT_Report.ExceptionAddress;
            event.XCPT_INFO_3   := CARDINAL(0);
            event.XCPT_INFO_4   := CARDINAL(0);
            ASSERT(eve.AddEvent(event));
          END;
          WITH DbgBuffer DO
            Cmd   := OS2.DBG_C_Continue;
            Pid   := PID;
            Tid   := TID;
            Value := OS2.XCPT_CONTINUE_SEARCH;
          END;

        | OS2.XCPT_GUARD_PAGE_VIOLATION:
          WITH DbgBuffer DO
            Cmd   := OS2.DBG_C_Continue;
            Pid   := PID;
            Tid   := TID;
            Value := OS2.XCPT_CONTINUE_SEARCH;
          END;

        ELSE
          IF opt.ExceptionOnFirstChance THEN
            WasException := TRUE;
            event.IP            := kt.ADDRESS(XCPT_Report.ExceptionAddress);
            event.Event         := eve.Exception;
            event.Exception_ID  := eve.ProgramException;
            event.XCPT_INFO_1   := XCPT_Report.ExceptionNum;
            ASSERT(CARDINAL(event.XCPT_INFO_1)#0);
            event.XCPT_INFO_2   := XCPT_Report.ExceptionAddress;
            event.XCPT_INFO_3   := CARDINAL(1);
            event.XCPT_INFO_4   := XCPT_Report.ExceptionInfo[0];
            ASSERT(eve.AddEvent(event));
            WITH DbgBuffer DO
              Cmd   := OS2.DBG_C_Stop;
              Pid   := PID;
            END;
          ELSE
            WITH DbgBuffer DO
              Cmd   := OS2.DBG_C_Continue;
              Pid   := PID;
              Tid   := TID;
              Value := OS2.XCPT_CONTINUE_SEARCH;
            END;
          END;
        END;

      | OS2.DBG_X_LAST_CHANCE:
        ASSERT(mem.Get(DbgBuffer.Buffer, sys.ADR(XCPT_Report), SIZE(XCPT_Report)));
        CASE XCPT_Report.ExceptionNum OF
        | OS2.XCPT_GUARD_PAGE_VIOLATION:
          WITH DbgBuffer DO
            Cmd   := OS2.DBG_C_Continue;
            Pid   := PID;
            Tid   := TID;
            Value := INTEGER(OS2.XCPT_CONTINUE_EXECUTION);
          END;
        ELSE
          WasException := TRUE;
          event.IP            := kt.ADDRESS(XCPT_Report.ExceptionAddress);
          event.Event         := eve.Exception;
          event.Exception_ID  := eve.ProgramException;
          event.XCPT_INFO_1   := XCPT_Report.ExceptionNum;
          ASSERT(CARDINAL(event.XCPT_INFO_1)#0);
          event.XCPT_INFO_2   := XCPT_Report.ExceptionAddress;
          event.XCPT_INFO_3   := CARDINAL(0);
          event.XCPT_INFO_4   := XCPT_Report.ExceptionInfo[0];
          ASSERT(eve.AddEvent(event));
          WITH DbgBuffer DO
            Cmd   := OS2.DBG_C_Stop;
            Pid   := PID;
          END;
        END;

      | OS2.DBG_X_STACK_INVALID:
        WasException := TRUE;
        event.IP            := DbgBuffer.Addr;
        event.Event         := eve.Exception;
        event.Exception_ID  := eve.ProgramException;
        event.XCPT_INFO_1   := DbgBuffer.Buffer;
        ASSERT(CARDINAL(event.XCPT_INFO_1)#0);
        event.XCPT_INFO_2   := DbgBuffer.Addr;
        event.XCPT_INFO_3   := CARDINAL(0);
        event.XCPT_INFO_4   := XCPT_Report.ExceptionInfo[0];
        ASSERT(eve.AddEvent(event));
        RETURN;
      END;

    | OS2.DBG_N_ModuleLoad:
      IF HModule = MAX(CARDINAL) THEN
        HModule := DbgBuffer.Value
      ELSE
        WITH event DO
          Event := eve.CompCreated;
          Component := kt.EmptyExecInfo;
          Component.Handle := DbgBuffer.Value;
          Stopable := TRUE;
          IF OS2.DosQueryModuleName(DbgBuffer.Value, SIZE(Component.full_name), Component.full_name) = 0 THEN
            IF NOT BuildInfo(Component.Handle, Component) THEN END;
            ASSERT(eve.AddEvent(event));
          END;
        END;
      END;

      WITH DbgBuffer DO
        Cmd   := OS2.DBG_C_Continue;
        Pid   := PID;
        Tid   := TID;
        Value := INTEGER(OS2.XCPT_CONTINUE_STOP);
      END;

    | OS2.DBG_N_ModuleFree:
      WITH event DO
        Event := eve.CompDestroyed;
        Handle := DbgBuffer.Value;
      END;

      ASSERT(eve.AddEvent(event));

      WITH DbgBuffer DO
        Cmd   := OS2.DBG_C_Go;
        Pid   := PID;
      END;
    | OS2.DBG_N_Watchpoint:
      event.IP             := DbgBuffer.EIP;
      event.MemAccess_Ind  := DbgBuffer.Index;
      event.Event          := eve.MemoryAccess;
      ASSERT(eve.AddEvent(event));
      WITH DbgBuffer DO
        Cmd   := OS2.DBG_C_Stop;
        Pid   := PID;
      END;
    | OS2.DBG_N_ThreadCreate:
      WITH DbgBuffer DO
        IF TID = MAX(CARDINAL) THEN
          TID := Tid;
        END;

        WITH event DO
          Event   := eve.ThreadCreated;
          THandle := Tid;
        END;
        ASSERT(eve.AddEvent(event));
        thr.AddThread (event.THandle);

        Cmd   := OS2.DBG_C_Continue;
        Pid   := PID;
        -- Tid   := DbgBuffer.Tid;
        Value := OS2.XCPT_CONTINUE_SEARCH;
      END;
    | OS2.DBG_N_ProcTerm:
      event.IP           := DbgBuffer.EIP;
      event.Event        := eve.Exception;
      event.Exception_ID := eve.ProgramException;
      event.XCPT_INFO_1  := CARDINAL(0);
      event.XCPT_INFO_2  := event.IP;
      event.XCPT_INFO_3  := CARDINAL(0);
      event.XCPT_INFO_4  := XCPT_Report.ExceptionInfo[0];
      ASSERT(eve.AddEvent(event));
      WITH DbgBuffer DO
        Cmd   := OS2.DBG_C_Stop;
        Pid   := PID;
      END;

    | OS2.DBG_N_ThreadTerm:
      WITH DbgBuffer DO
        WITH event DO
          Event   := eve.ThreadDestroyed;
          THandle := Tid;
        END;
        ASSERT(eve.AddEvent(event));
        Cmd   := OS2.DBG_C_Go;
        Pid   := PID;
      END;
      thr.ThreadKilled (event.THandle);

    | OS2.DBG_N_AsyncStop:
      WITH event DO
        IP := DbgBuffer.EIP;
        Event:= eve.Exception;
        Exception_ID := eve.UserException;
        XCPT_INFO_1 := CARDINAL(0);
        XCPT_INFO_2 := IP;
        XCPT_INFO_3 := CARDINAL(0);
        XCPT_INFO_4 := CARDINAL(0);
      END;
      ASSERT(eve.AddEvent(event));
      WITH DbgBuffer DO
        Cmd   := OS2.DBG_C_Continue;
        Pid   := PID;
        Tid   := TID;
        Value := INTEGER(OS2.XCPT_CONTINUE_STOP);
      END;

    ELSE
      ASSERT(FALSE, DbgBuffer.Cmd);
    END;
    ret := OS2.DosDebug(sys.ADR(DbgBuffer));
    IF ret # 0 THEN
      event.Event   := eve.InternalError;
      event.ErrorNo := ret;
      ASSERT(eve.AddEvent(event));
      RETURN;
    END;
  END;
END ProcessEvents;


PROCEDURE UnloadProgram;
BEGIN
  IF opt.RemoteMode THEN
    rmt.UnloadProgram;
    RETURN;
  END;

  Loaded := FALSE;
  WITH DbgBuffer DO
    Cmd   := OS2.DBG_C_Term;
    Pid   := PID;
  END;
  IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN RETURN END;
  IF DbgBuffer.Cmd # OS2.DBG_N_Success THEN RETURN END;

  OS2.DosStopSession(OS2.STOP_SESSION_SPECIFIED, utl.SID);
  thr.ClearThreads();
END UnloadProgram;


PROCEDURE Execute (go_mode: GO_MODE);
VAR
  count: CARDINAL;
  ThrdBuffer: OS2.TStat_t;
  rc : CARDINAL;
  event: eve.EVENT;
BEGIN
  IF opt.RemoteMode THEN
    rmt.Execute (go_mode);
    RETURN;
  END;

  OS2.DosResetEventSem(Program_stopped, count);
  OS2.DosPostEventSem(Program_started);
  OS2.DosPostEventSem(Program_started_2);

  IF WasException THEN
    WITH DbgBuffer DO
      Cmd   := OS2.DBG_C_Continue;
      Pid   := PID;
      Tid   := TID;
      Value := OS2.XCPT_CONTINUE_SEARCH;
    END;
    rc := OS2.DosDebug(sys.ADR(DbgBuffer));
    WasException := FALSE;
    IF rc = OS2.ERROR_INTERRUPT THEN
      WITH event DO
        IP := emm.GetIP();
        Event:= eve.Exception;
        Exception_ID := eve.UserException;
        XCPT_INFO_1 := CARDINAL(0);
        XCPT_INFO_2 := IP;
        XCPT_INFO_3 := CARDINAL(0);
        XCPT_INFO_4 := CARDINAL(0);
      END;
      ASSERT(eve.AddEvent(event));
    ELSE
      ProcessEvents(go_mode.add_step);
    END;
  ELSE
    CASE go_mode.mode OF
    | SingleStep:
      WITH DbgBuffer DO
        Cmd   := OS2.DBG_C_SStep;
        Pid   := PID;
        --Tid   := 0;
        Tid   := TID;
      END;
      rc := OS2.DosDebug(sys.ADR(DbgBuffer));
      IF rc = OS2.ERROR_INTERRUPT THEN
        WITH event DO
          IP := emm.GetIP();
          Event:= eve.Exception;
          Exception_ID := eve.UserException;
          XCPT_INFO_1 := CARDINAL(0);
          XCPT_INFO_2 := IP;
          XCPT_INFO_3 := CARDINAL(0);
          XCPT_INFO_4 := CARDINAL(0);
        END;
        ASSERT(eve.AddEvent(event));
      ELSE
        ProcessEvents(go_mode.add_step);
      END;
    | Go:
      WITH DbgBuffer DO
        --IF TID = MAX(CARDINAL) THEN
          Cmd   := OS2.DBG_C_Go;
          Pid   := PID;
        --ELSE
        --  Cmd   := OS2.DBG_C_Continue;
        --  Pid   := PID;
        --  Tid   := TID;
        --  Value := INTEGER(OS2.XCPT_CONTINUE_EXECUTION);
        --END;
      END;
      rc := OS2.DosDebug(sys.ADR(DbgBuffer));
      IF rc = OS2.ERROR_INTERRUPT THEN
        WITH event DO
          IP := emm.GetIP();
          Event:= eve.Exception;
          Exception_ID := eve.UserException;
          XCPT_INFO_1 := CARDINAL(0);
          XCPT_INFO_2 := IP;
          XCPT_INFO_3 := CARDINAL(0);
          XCPT_INFO_4 := CARDINAL(0);
        END;
        ASSERT(eve.AddEvent(event));
      ELSE
        ProcessEvents(FALSE);
      END;
    END;
  END;

  OS2.DosPostEventSem(Program_stopped);
  OS2.DosResetEventSem(Program_started_2, count);

  WITH DbgBuffer DO
    Cmd   := OS2.DBG_C_ThrdStat;
    Pid   := PID;
    Tid   := 0;
    Buffer:= CARDINAL(sys.ADR(ThrdBuffer));
    Len   := SIZE(ThrdBuffer);
  END;
  ASSERT(OS2.DosDebug(sys.ADR(DbgBuffer)) = 0);
  ASSERT(DbgBuffer.Cmd = OS2.DBG_N_Success);
  TID := DbgBuffer.Tid;

END Execute;


(*
VAR
  InterruptProgram  : OS2.HEV;
  InterruptDbgBuffer: OS2.uDB_t;

<* PUSH *>
<* WOFF301+ *>
PROCEDURE ["SysCall"] BreakProgram (par: OS2.ULONG);
<* POP *>
BEGIN
  WITH InterruptDbgBuffer DO
    Cmd := OS2.DBG_C_Stop;
    Pid := PID;
  END;
  OS2.DosDebug (sys.ADR(InterruptDbgBuffer));
  OS2.DosPostEventSem (InterruptProgram);
  OS2.DosExit (0, 0);
END BreakProgram;


VAR
  BreakProgramTID: OS2.TID;

PROCEDURE ["C"] CtrlBreakHandler (): BOOLEAN;
BEGIN
  IF ProgramExecuting THEN
    ASSERT(OS2.DosCreateEventSem (NIL, InterruptProgram, 0, FALSE)=0);
    OS2.DosCreateThread (BreakProgramTID, BreakProgram, 0, 0, 32000);
    IF OS2.DosWaitEventSem (InterruptProgram, 1000) # 0 THEN
      OS2.DosKillThread (BreakProgramTID);
      IF opt.WarningBell THEN
        OS2.DosBeep(500,  20);
        OS2.DosBeep(1000, 150);
      END;
    END;
    OS2.DosCloseEventSem (InterruptProgram);
  END;
  RETURN TRUE;
END CtrlBreakHandler;
*)

PROCEDURE ["C"] CtrlBreakHandler (): BOOLEAN;
BEGIN
  RETURN TRUE;
END CtrlBreakHandler;

VAR
  oldBreakHandler: brk.BreakHandler;

BEGIN
  PMHackStarted          := FALSE;
  opt.KernelInRemoteMode := FALSE;
  ProgramExecuting       := FALSE;
  WasException           := FALSE;
  oldBreakHandler        := brk.SetBreakHandler (CtrlBreakHandler);
END Krn_Prog.
