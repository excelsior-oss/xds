IMPLEMENTATION MODULE Krn_Thrs;

IMPORT sys := SYSTEM;
IMPORT io  := InOut;
IMPORT fmt := FormStr;

IMPORT WIN := Windows;

IMPORT dbg := Krn_Dbg;
IMPORT thr := Threads;

IMPORT opt := Options;
IMPORT rmt := Remote;


PROCEDURE GetThreadDescription(i: CARDINAL; VAR buf: ARRAY OF CHAR);
VAR
  Tstate   : ARRAY [0..32] OF CHAR;
  TPriority: ARRAY [0..32] OF CHAR;
  count    : CARDINAL;
  THandle  : WIN.HANDLE;
  TID      : WIN.DWORD;
BEGIN
  IF opt.RemoteMode THEN
    rmt.GetThreadDescription (i, buf);
  ELSE
    WITH thr.Threads.Threads^[i] DO
      THandle := Handle;
      TID := ID;
    END;
    CASE WIN.GetThreadPriority (THandle) OF
    | WIN.THREAD_PRIORITY_IDLE:
      TPriority := 'Idle';
    | WIN.THREAD_PRIORITY_LOWEST:
      TPriority := 'Lowest';
    | WIN.THREAD_PRIORITY_BELOW_NORMAL:
      TPriority := 'Below normal';
    | WIN.THREAD_PRIORITY_NORMAL:
      TPriority := 'Normal';
    | WIN.THREAD_PRIORITY_ABOVE_NORMAL:
      TPriority := 'Above normal';
    | WIN.THREAD_PRIORITY_HIGHEST:
      TPriority := 'Highest';
    | WIN.THREAD_PRIORITY_TIME_CRITICAL:
      TPriority := 'Time critical';
    | WIN.THREAD_PRIORITY_ERROR_RETURN:
      TPriority := 'Unknown';
    END;
    IF dbg.H_Thread = THandle THEN
      Tstate := 'Current';
    ELSE
      count := WIN.SuspendThread (THandle);
      IF count = 0 THEN
        Tstate := 'Runnable';
      ELSIF count = MAX(CARDINAL) THEN
        Tstate := 'Unknown';
      ELSE
        fmt.print(Tstate, 'Suspended %d times', count);
      END;
      WIN.ResumeThread (THandle);
    END;
    fmt.print (buf, 'TID=%-3d Handle=0x%$8X Priority=%-13s State=%s', TID, THandle, TPriority, Tstate);
  END;
END GetThreadDescription;


PROCEDURE SwitchToThread(i: CARDINAL): BOOLEAN;
--VAR
--  count: CARDINAL;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.SwitchToThread (i);
  ELSE
    dbg.H_Thread := thr.Threads.Threads^[i].Handle;
    RETURN TRUE;
  END;
END SwitchToThread;


PROCEDURE SuspendThread(i: CARDINAL): BOOLEAN;
VAR
  count: CARDINAL;
  Handle: WIN.HANDLE;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.SuspendThread (i);
  ELSE
    Handle := thr.Threads.Threads^[i].Handle;
    IF dbg.H_Thread = Handle THEN
      RETURN FALSE;
    END;
    count := WIN.SuspendThread (Handle);
    RETURN count # MAX(CARDINAL);
  END;
END SuspendThread;


PROCEDURE ResumeThread (i: CARDINAL): BOOLEAN;
VAR
  count: CARDINAL;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.ResumeThread (i);
  ELSE
    count := WIN.ResumeThread (thr.Threads.Threads^[i].Handle);
    RETURN count # MAX(CARDINAL);
  END;
END ResumeThread;


END Krn_Thrs.
