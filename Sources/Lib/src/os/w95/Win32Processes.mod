(* Copyright (c) 1996 xTech Ltd, Russia. All Rights Reserved. *)
IMPLEMENTATION MODULE Win32Processes;

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
    handle: HANDLE;
  END;

PROCEDURE GetSemaphoreHandle(s: SEMAPHORE): HANDLE;
  VAR x: Semaphore;
BEGIN
  x:=SYSTEM.CAST(Semaphore,s);
  IF x^.magic#magic_s THEN
    xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.sysException));
  END;
  RETURN x^.handle;
END GetSemaphoreHandle;

PROCEDURE GetThreadHandle(p: ProcessId): HANDLE;
  VAR pid: xrThreads.X2C_Process;
BEGIN
  pid:=SYSTEM.CAST(xrThreads.X2C_Process,p);
  RETURN SYSTEM.CAST(HANDLE,pid^.thread);
END GetThreadHandle;

PROCEDURE GetThreadId(p: ProcessId): DWORD;
  VAR pid: xrThreads.X2C_Process;
BEGIN
  pid:=SYSTEM.CAST(xrThreads.X2C_Process,p);
  RETURN SYSTEM.CAST(DWORD,pid^.threadId);
END GetThreadId;

PROCEDURE MakeSource(h: HANDLE): Sources;
BEGIN
  RETURN SYSTEM.CAST(Sources,h);
END MakeSource;

END Win32Processes.
