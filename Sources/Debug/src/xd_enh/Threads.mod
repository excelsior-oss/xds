<* Storage+ *>
IMPLEMENTATION MODULE Threads;

IMPORT sys := SYSTEM;
IMPORT io  := InOut;
IMPORT fmt := FormStr;

IMPORT opt := Options;
IMPORT rmt := Remote;

IMPORT dbg := Krn_Dbg;
IMPORT kth  := Krn_Thrs;
IMPORT dth  := Def_Thrs;



PROCEDURE AddThread  (thread: dth.THREAD): kth.THREAD_INX;
CONST
  N = 16;
VAR
  tmp : PATHREADS;
BEGIN
  ASSERT (NOT opt.RemoteMode);
  WITH Threads DO
    IF Threads = NIL THEN
      NEW (Threads, N);
      Count := 0;
    ELSIF Count = HIGH(Threads^) THEN
      NEW (tmp, HIGH(Threads^)+1+N);
      sys.MOVE(sys.ADR(Threads^), sys.ADR(tmp^), SIZE(Threads^));
      Threads := tmp;
    END;
    Threads^[Count] := thread;
    INC (Count);
    RETURN Count-1;
  END;
END AddThread;



PROCEDURE NewThread  (): kth.THREAD_INX;
BEGIN
  ASSERT (NOT opt.RemoteMode);
  RETURN AddThread (dth.InitialThread);
END NewThread;




PROCEDURE ThreadKilled (thread: kth.THREAD_INX);
BEGIN
  ASSERT (NOT opt.RemoteMode);
  WITH Threads DO
    ASSERT (Count#0);
    IF thread # Count-1 THEN
      sys.MOVE (sys.ADR(Threads^[thread+1]), sys.ADR(Threads^[thread]), SIZE(dth.THREAD)*(Count-1-thread));
    END;
    DEC (Count);
  END;
END ThreadKilled;


PROCEDURE ClearThreads;
BEGIN
  Threads.Count := 0;
END ClearThreads;


PROCEDURE NThreads (): CARDINAL;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.NThreads ();
  ELSE
    RETURN Threads.Count;
  END;
END NThreads;



BEGIN
  Threads := THREADS{ 0, NIL };
END Threads.
