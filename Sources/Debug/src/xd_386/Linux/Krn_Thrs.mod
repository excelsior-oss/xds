IMPLEMENTATION MODULE Krn_Thrs;

IMPORT sys := SYSTEM;
--IMPORT io  := InOut;
IMPORT fmt := FormStr;

IMPORT lnx := Linux;

IMPORT dbg := Krn_Dbg;
IMPORT thr := Threads;

IMPORT opt := Options;
IMPORT rmt := Remote;


FROM Krn_Dbg IMPORT H_Thread;--, H_Process;--, Id_Process, Id_Thread;
FROM Printf IMPORT printf;


PROCEDURE GetThreadDescription (i: THREAD_INX; VAR buf: ARRAY OF CHAR);
VAR
  Tstate   : ARRAY [0..32] OF CHAR;
  TPriority: ARRAY [0..32] OF CHAR;
  count    : CARDINAL;
  THandle  : lnx.THANDLE;
BEGIN
  IF opt.RemoteMode THEN
    rmt.GetThreadDescription (i, buf);
  ELSE

    WITH thr.Threads.Threads^[i] DO
      THandle := Handle;
    END;

--    printf ("*** GetThreadDescription: %d, THandle=0x%x\n", i, THandle);
(*
    IF NOT lnx.get_thread_info (THandle, TID, LWP) THEN
      RETURN;
    END;
*)
    TPriority := 'TPriority';
(*
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
*)
    IF dbg.H_Thread = THandle THEN
      Tstate := 'Current';
    ELSE
      Tstate := 'Unknown';
(*
      count := 12;
--      count := WIN.SuspendThread (THandle);
      IF count = 0 THEN
        Tstate := 'Runnable';
      ELSIF count = MAX(CARDINAL) THEN
        Tstate := 'Unknown';
      ELSE
        fmt.print(Tstate, 'Suspended %d times', count);
      END;
--      WIN.ResumeThread (THandle);
*)
    END;
    IF lnx.is_multithreaded () THEN
      fmt.print (buf, '{pid=%-3d, lwp=%-3d, tid=%-3d} Priority=%-13s State=%s', 
                 THandle^.pid, THandle^.lwp, THandle^.tid, TPriority, Tstate);
    ELSE
      fmt.print (buf, '{pid=%-3d} Priority=%-13s State=%s', 
                 THandle^.pid, TPriority, Tstate);
    END;
(*
    fmt.print (buf, '{pid=%-3d, lwp=%-3d, tid=%-3d} Priority=%-13s State=%s', 
               THandle^.pid, THandle^.lwp, THandle^.tid, TPriority, Tstate);
*)
  END;
END GetThreadDescription;

PROCEDURE SwitchToThread       (i: THREAD_INX): BOOLEAN;
BEGIN
  printf ("*** SwitchToThread: %d\n", i);
  IF opt.RemoteMode THEN
    RETURN rmt.SwitchToThread (i);
  ELSE
    dbg.H_Thread := thr.Threads.Threads^[i].Handle;
    RETURN TRUE;
  END;
END SwitchToThread;

PROCEDURE SuspendThread        (i: THREAD_INX): BOOLEAN;
BEGIN
  printf ("*** SuspendThread: %d\n", i);
  RETURN FALSE;
END SuspendThread;

PROCEDURE ResumeThread         (i: THREAD_INX): BOOLEAN;
BEGIN
  printf ("*** ResumeThread: %d\n", i);
  RETURN FALSE;
END ResumeThread;


END Krn_Thrs.


