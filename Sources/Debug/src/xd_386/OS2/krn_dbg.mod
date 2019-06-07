IMPLEMENTATION MODULE Krn_Dbg;

IMPORT OS2;

IMPORT fmt := FormStr;

IMPORT opt := Options;

IMPORT kex := KrnExec;
IMPORT kt  := KrnTypes;


VAR
  MyHswitch       : OS2.HSWITCH;
  DebuggeeHswitch : OS2.HSWITCH;

<* PUSH *>
<* WOFF301+ *>
PROCEDURE ["SysCall"] SmartSelector(par: OS2.ULONG);
<* POP *>
VAR
  count: OS2.ULONG;
BEGIN
<* PUSH *>
<* WOFF310+ *>
  LOOP
    OS2.DosWaitEventSem(Program_started, OS2.SEM_INDEFINITE_WAIT);
    OS2.DosResetEventSem(Program_started, count);
    IF OS2.DosWaitEventSem(Program_stopped, opt.SSS_Delay) <> 0 THEN
      SwitchToDebuggee;
    END;
  END;
<* POP *>
END SmartSelector;


VAR
  SS_tid: OS2.TID;
  SS_Started: BOOLEAN;


PROCEDURE EnableSwitchSession (): BOOLEAN;
BEGIN
  RETURN opt.in_dialog AND NOT opt.KernelInRemoteMode AND (kex.ProgInfo.app_type # kt.windowed);
END EnableSwitchSession;


PROCEDURE StartSmartSelector;
VAR
  name: ARRAY [0..255] OF CHAR;
BEGIN
  IF EnableSwitchSession() THEN
    IF NOT SS_Started THEN
      ASSERT(OS2.DosCreateEventSem(NIL, Program_started_2, 0, FALSE)=0);
      ASSERT(OS2.DosCreateEventSem(NIL, Program_started,   0, FALSE)=0);
      ASSERT(OS2.DosCreateEventSem(NIL, Program_stopped,   0, FALSE)=0);
      OS2.DosCreateThread(SS_tid, SmartSelector, 0, 0, 32000);
      SS_Started := TRUE;
    END;
  END;
END StartSmartSelector;


PROCEDURE StopSmartSelector;
BEGIN
  IF EnableSwitchSession() THEN
    IF SS_Started THEN
      OS2.DosCloseEventSem(Program_started);
      OS2.DosCloseEventSem(Program_started_2);
      OS2.DosCloseEventSem(Program_stopped);
      OS2.DosKillThread(SS_tid);
      SS_Started := FALSE;
    END;
  END;
END StopSmartSelector;


PROCEDURE SwitchToDebuggee;
VAR
  hswitch : OS2.HSWITCH;
  swcntrl : OS2.SWCNTRL;
BEGIN
  IF EnableSwitchSession() THEN
    IF DebuggeeHswitch = OS2.NULLHANDLE THEN
      hswitch := OS2.WinQuerySwitchHandle(0, PID);
      IF (hswitch <> OS2.NULLHANDLE) AND
         (OS2.WinQuerySwitchEntry(hswitch,swcntrl) = OS2.NO_ERROR) AND
         (swcntrl.idProcess = PID) AND
         (swcntrl.hwnd <> OS2.NULLHANDLE) THEN
        DebuggeeHswitch := hswitch;
      ELSE
        RETURN
      END;
    END;
    OS2.WinSwitchToProgram(DebuggeeHswitch);
--    OS2.DosSelectSession(SID);
  END;
END SwitchToDebuggee;

PROCEDURE SwitchToDebugger;
BEGIN
  IF EnableSwitchSession() THEN
    OS2.WinSwitchToProgram(MyHswitch);
  END;
END SwitchToDebugger;

PROCEDURE NewDebuggee(pid: OS2.PID; sid: OS2.ULONG);
BEGIN
  PID := pid;
  SID := sid;
  DebuggeeHswitch := OS2.NULLHANDLE;
END NewDebuggee;

(*

PROCEDURE Get_MyHandle;
VAR
  N, len : CARDINAL;
  buf    : sys.ADDRESS;
  pSwitchEntryArr: POINTER TO ARRAY [0..0FFFEH] OF OS2.SWENTRY;
  i      : CARDINAL;
  hab    : OS2.HAB;
  hmq    : OS2.HMQ;
  error  : CARDINAL;
  hwndFrame: OS2.HWND;

BEGIN
  hab := OS2.WinInitialize(0);
  hmq := OS2.WinCreateMsgQueue(hab, 0);
  IF hmq = OS2.NULLHANDLE THEN
    error := OS2.WinGetLastError(hab);
    printf('Error = %x %d\n', error, error MOD 10000H);
  END;
  IF OS2.WinQueryTaskTitle(0, old_Title, SIZE(old_Title)) # 0 THEN
    old_Title := '';
  END;
  printf('%s\n', old_Title);
  IF OS2.WinQuerySessionTitle(hab, 0, old_Title, SIZE(old_Title)) # 0 THEN
    old_Title := '';
  END;
  printf('%s\n', old_Title);
  IF OS2.WinQueryWindowText(0, SIZE(old_Title), old_Title) # 0 THEN
    old_Title := '';
  END;
  printf('%s\n', old_Title);
  hwndFrame := OS2.WinQueryWindow(MyHWND, OS2.QW_PARENT);
  printf('%x %x\n', MyHWND, hwndFrame);
  IF NOT OS2.WinSetWindowText(hwndFrame, 'XD Debugger for OS/2') THEN
    error := OS2.WinGetLastError(hab);
    printf('Error = %x %d\n', error, error MOD 10000H);
  END;

  --swctl.szSwtitle := 'XD Debugger for OS/2';
  --OS2.WinChangeSwitchEntry(hswitch, swctl);

  N := OS2.WinQuerySwitchList(0, NIL, 0);
  ASSERT(N # 0);
  len := N * SIZE(OS2.SWBLOCK) + SIZE(OS2.HSWITCH);
  ALLOCATE(buf, len);
  N := OS2.WinQuerySwitchList(0, buf, len);
  pSwitchEntryArr := sys.ADDADR(buf, SIZE(OS2.ULONG));
  FOR i := 0 TO N-1 DO
    WITH pSwitchEntryArr^[i] DO
      io.WriteHex(swctl.idProcess, 0);  io.WriteString(' ');
      io.WriteHex(MyPID, 0);   io.WriteString(' ');
      io.WriteString(swctl.szSwtitle);  io.WriteLn;

      IF swctl.idProcess = MyPID THEN
        MyHWND := swctl.hwnd;
        swctl.szSwtitle := 'XD Debugger for OS/2';
        OS2.WinChangeSwitchEntry(hswitch, swctl);
        DEALLOCATE(buf, len);
        RETURN;
      END;
    END;
  END;
END Get_MyHandle;
*)

VAR
  ptib : OS2.PTIB;
  ppib : OS2.PPIB;
  swctl  : OS2.SWCNTRL;

BEGIN
  HModule := MAX(CARDINAL);

  OS2.DosGetInfoBlocks(ptib, ppib);
  MyPID := ppib^.pib_ulpid;

  MyHswitch := OS2.WinQuerySwitchHandle(0, MyPID);
  ASSERT(OS2.WinQuerySwitchEntry(MyHswitch, swctl)=0);
  MyHwnd := swctl.hwnd;
  DebuggeeHswitch := OS2.NULLHANDLE;

  PID := MAX(CARDINAL);
  TID := MAX(CARDINAL);

  SS_Started := FALSE;
FINALLY
  StopSmartSelector;
END Krn_Dbg.
