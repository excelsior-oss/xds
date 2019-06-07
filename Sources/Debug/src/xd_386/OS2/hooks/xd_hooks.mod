<* CHECKNIL- *>
<* CHECKPROC- *>
IMPLEMENTATION MODULE XD_Hooks;

IMPORT OS2;

IMPORT sys := SYSTEM;

PROCEDURE ["SysCall"] SendMsgHook(hab: OS2.HAB; VAR msg: OS2.SMHSTRUCT; fInterTask: OS2.BOOL);
TYPE
  P4 = POINTER TO CARDINAL;

VAR
  pid: OS2.PID;
  tid: OS2.TID;
  hq: OS2.HQUEUE;
  pvoid: OS2.PVOID;
  --PPID: P4;

  MODULE CloseQueue;
  IMPORT hq, OS2, pvoid;
  BEGIN
  FINALLY
    OS2.DosCloseQueue(hq);
    OS2.DosFreeMem(pvoid);
  END CloseQueue;

BEGIN
  IF OS2.DosOpenQueue(pid, hq, '\\QUEUES\\xd_hook.que') # 0 THEN OS2.DosBeep(2000, 10); END;
  IF OS2.DosGetNamedSharedMem(pvoid, '\\SHAREMEM\\xd_hook_pid', OS2.PAG_READ) = 0 THEN
    OS2.DosWriteQueue(hq, msg.hwnd, 4, sys.ADR(tid), 0 );
    OS2.DosWriteQueue(hq, msg.msg, 4, sys.ADR(tid), 0 );
    IF OS2.WinQueryWindowProcess(msg.hwnd, pid, tid) THEN
      OS2.DosWriteQueue(hq, pid, 4, sys.ADR(tid), 0 );
    ELSE
      OS2.DosWriteQueue(hq, MAX(CARDINAL), 4, sys.ADR(tid), 0 );
    END;
    OS2.DosWriteQueue(hq, 0, 4, sys.ADR(tid), 0 );
    IF msg.msg = 195H THEN
      OS2.DosBeep(2000, 10)
    END;
  ELSE
    OS2.DosWriteQueue(hq, MAX(CARDINAL), 4, sys.ADR(tid), 0 );
  END;
END SendMsgHook;

PROCEDURE ["SysCall"] InputHook(hab: OS2.HAB; VAR event: OS2.QMSG; fs: OS2.ULONG): OS2.BOOL;
TYPE
  P4 = POINTER TO CARDINAL;
VAR
  pid: OS2.PID;
  tid: OS2.TID;
  hq: OS2.HQUEUE;
  pvoid: OS2.PVOID;
  PPID: P4;

  MODULE CloseQueue;
  IMPORT hq, OS2, pvoid;
  BEGIN
  FINALLY
    OS2.DosCloseQueue(hq);
    OS2.DosFreeMem(pvoid);
  END CloseQueue;

BEGIN
  IF OS2.DosOpenQueue(pid, hq, '\\QUEUES\\xd_hook.que') # 0 THEN OS2.DosBeep(2000, 100); END;
  IF OS2.DosGetNamedSharedMem(pvoid, '\\SHAREMEM\\xd_hook_pid', OS2.PAG_READ) # 0 THEN
    OS2.DosBeep(2000, 100);
    RETURN FALSE;
  END;
  --IF (_hwnd # event.hwnd) THEN
    --OS2.DosWriteQueue(hq, event.hwnd, 4, sys.ADR(tid), 0 );
    --OS2.DosWriteQueue(hq, event.msg, 4, sys.ADR(tid), 0 );
 --   _hwnd := event.hwnd;
 -- END;
  IF OS2.WinQueryWindowProcess(event.hwnd, pid, tid) THEN
 --   IF  (pid # _pid) THEN
      PPID := P4(pvoid);
--      OS2.DosWriteQueue(hq, PPID^, 4, sys.ADR(tid), 0 );
--      OS2.DosWriteQueue(hq, pid, 4, sys.ADR(tid), 0 );
 --     _pid := pid;
 --   END;
    IF pid = PPID^ THEN
      RETURN TRUE;
    END;
  ELSE
    --OS2.DosWriteQueue(hq, MAX(CARDINAL), 4, sys.ADR(tid), 0 );
  END;
  RETURN FALSE;
END InputHook;


BEGIN
END XD_Hooks.
