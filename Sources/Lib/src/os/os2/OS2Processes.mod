(* Copyright (c) 1998 XDS Ltd, Russia. All Rights Reserved. *)

IMPLEMENTATION MODULE OS2Processes;

IMPORT  xosThreads, SYSTEM, xmRTS, M2EXCEPTION, xrThreads;

(* Note!
   Folowing type and constant are copied from
   Semaphores.mod.
   Later they should be defined in xrThreads instead
   to refuse possible conflicts *)

CONST
  magic_s = 856FH;

TYPE
  Semaphore = POINTER TO SemRec;
  SemRec = RECORD
    magic : CARDINAL;
    handle: HEV;
  END;

PROCEDURE GetSemaphoreHandle(s: SEMAPHORE): HEV;
  VAR x: Semaphore;
BEGIN
  x:=SYSTEM.CAST(Semaphore,s);
  IF x^.magic#magic_s THEN
    xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.sysException));
  END;
  RETURN x^.handle;
END GetSemaphoreHandle;

PROCEDURE GetThreadHandle(p: ProcessId): TID;
  VAR pid: xrThreads.X2C_Process;
BEGIN
  pid:=SYSTEM.CAST(xrThreads.X2C_Process,p);
  RETURN SYSTEM.CAST(TID,pid^.thread);
END GetThreadHandle;

PROCEDURE MakeSource(h: HEV): Sources;
BEGIN
  RETURN SYSTEM.CAST(Sources,h);
END MakeSource;


END OS2Processes.
