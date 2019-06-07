<* Storage+ *>
IMPLEMENTATION MODULE Krn_Thrs;

IMPORT sys := SYSTEM;
IMPORT io  := InOut;
IMPORT fmt := FormStr;

IMPORT OS2;

IMPORT dbg := Krn_Dbg;

IMPORT opt := Options;
IMPORT rmt := Remote;

IMPORT thr := Threads;



PROCEDURE GetThreadDescription (i: CARDINAL; VAR buf: ARRAY OF CHAR);
VAR
  Tstate    : ARRAY [0..32] OF CHAR;
  Dstate    : ARRAY [0..32] OF CHAR;
  ThrdBuffer: OS2.TStat_t;
  DbgBuffer : OS2.uDB_t;
BEGIN
  IF opt.RemoteMode THEN
    rmt.GetThreadDescription (i, buf);
  ELSE
    WITH ThrdBuffer DO
      DbgState  := MAX(sys.CARD8);
      TState    := MAX(sys.CARD8);
      TPriority := MAX(sys.CARD16);
    END;
    WITH DbgBuffer DO
      Cmd   := OS2.DBG_C_ThrdStat;
      Pid   := dbg.PID;
      Tid   := thr.Threads.Threads^[i].TID;
      Buffer:= CARDINAL(sys.ADR(ThrdBuffer));
      Len   := SIZE(ThrdBuffer);
    END;
    ASSERT(OS2.DosDebug(sys.ADR(DbgBuffer)) = 0);
    ASSERT(dbg.DbgBuffer.Cmd = OS2.DBG_N_Success);
    CASE ThrdBuffer.DbgState OF
    | OS2.DBG_D_Thawed: Dstate := 'Thawed';
    | OS2.DBG_D_Frozen: Dstate := 'Frozen';
    ELSE
      io.WriteString('Unknown DbgState');
      io.WriteLn;
      ASSERT(FALSE);
    END;
    CASE ThrdBuffer.TState OF
    | OS2.DBG_T_Runnable:  Tstate := 'Runnable';
    | OS2.DBG_T_Suspended: Tstate := 'Suspended';
    | OS2.DBG_T_Blocked:   Tstate := 'Blocked';
    | OS2.DBG_T_CritSec:   Tstate := 'CritSec';
    ELSE
      io.WriteString('Unknown TState');
      io.WriteLn;
      ASSERT(FALSE);
    END;
    IF dbg.TID = thr.Threads.Threads^[i].TID THEN
      Tstate := 'Current';
    END;
    fmt.print(buf, 'ID=%$3d DbgState=%s TState=%-9.9s Priority=%d', thr.Threads.Threads^[i].TID, Dstate, Tstate, ThrdBuffer.TPriority);
  END;
END GetThreadDescription;


PROCEDURE SwitchToThread (i: CARDINAL): BOOLEAN;
VAR
  ThrdBuffer: OS2.TStat_t;
  DbgBuffer : OS2.uDB_t;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.SwitchToThread (i);
  ELSE
    WITH ThrdBuffer DO
      DbgState  := MAX(sys.CARD8);
      TState    := MAX(sys.CARD8);
      TPriority := MAX(sys.CARD16);
    END;
    WITH DbgBuffer DO
      Cmd   := OS2.DBG_C_ThrdStat;
      Pid   := dbg.PID;
      Tid   := thr.Threads.Threads^[i].TID;
      Buffer:= CARDINAL(sys.ADR(ThrdBuffer));
      Len   := SIZE(ThrdBuffer);
    END;
    dbg.TID := thr.Threads.Threads^[i].TID;
    RETURN TRUE;
  END;
END SwitchToThread;


PROCEDURE SuspendThread (i: CARDINAL): BOOLEAN;
VAR
  DbgBuffer: OS2.uDB_t;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.SuspendThread (i);
  ELSE
    IF dbg.TID = thr.Threads.Threads^[i].TID THEN
      RETURN FALSE;
    END;
    WITH DbgBuffer DO
      Cmd   := OS2.DBG_C_Freeze;
      Pid   := dbg.PID;
      Tid   := thr.Threads.Threads^[i].TID;
    END;
    ASSERT(OS2.DosDebug(sys.ADR(DbgBuffer)) = 0);
    ASSERT(dbg.DbgBuffer.Cmd = OS2.DBG_N_Success);
    RETURN TRUE;
  END;
END SuspendThread;


PROCEDURE ResumeThread (i: CARDINAL): BOOLEAN;
VAR
  DbgBuffer: OS2.uDB_t;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.ResumeThread (i);
  ELSE
    WITH DbgBuffer DO
      Cmd   := OS2.DBG_C_Resume;
      Pid   := dbg.PID;
      Tid   := thr.Threads.Threads^[i].TID;
    END;
    ASSERT(OS2.DosDebug(sys.ADR(DbgBuffer)) = 0);
    ASSERT(dbg.DbgBuffer.Cmd = OS2.DBG_N_Success);
    RETURN TRUE;
  END;
END ResumeThread;


END Krn_Thrs.
