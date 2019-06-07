(* Copyright (c) XDS 1999. All Rights Reserved. *)
(*
 * XDS library.
 * Interface to multithreading features of the underlying OS.
 *)

<* M2EXTENSIONS+ *>

IMPLEMENTATION MODULE Threads; (* Snowman 25.06.99 *)

IMPORT SYSTEM, EXCEPTIONS;
IMPORT Strings;
IMPORT xmRTS, xosThreads, xrThreads;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

TYPE
  Thread    = xrThreads.X2C_Process;
  Event     = xosThreads.X2C_BSemaphore;            
  Semaphore = xosThreads.X2C_Semaphore;
  Mutex     = xosThreads.X2C_Mutex;
  Key       = xosThreads.X2C_Key;

VAR
  source: EXCEPTIONS.ExceptionSource;

PROCEDURE RAISE (n: SYSTEM.INT32; s-: ARRAY OF CHAR);
VAR
  msg : ARRAY [0..255] OF CHAR;
BEGIN
  Strings.Concat("Threads.",s,msg);
  EXCEPTIONS.RAISE(source,SYSTEM.CAST(CARDINAL,n),msg);
END RAISE;

PROCEDURE IsThreadsException () : BOOLEAN;
BEGIN
  RETURN EXCEPTIONS.IsCurrentSource(source)
END IsThreadsException;

PROCEDURE CurrentThread(): Thread;
VAR
  a : SYSTEM.ADDRESS;
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_GetThreadWord(a);
  IF rc <> 0 THEN RAISE(rc,"CurrentThread") END;
  RETURN SYSTEM.CAST(Thread,a);
END CurrentThread;

PROCEDURE CreateThread(VAR t: Thread;
                        proc: PROC;
                       stack: CARDINAL;
                         par: SYSTEM.ADDRESS;
                        prio: INTEGER;
                     suspend: BOOLEAN);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xrThreads.X2C_CreateISOProcess(t, SYSTEM.CAST(xrThreads.Body,proc),
                                       stack, par, prio);
  IF rc <> 0 THEN RAISE(rc,"CreateThread"); END;
  IF suspend THEN
    rc := xosThreads.X2C_SuspendThread(t^.thread);
    IF rc <> 0 THEN RAISE(rc,"CreateThread"); END;
  END;
  rc := xosThreads.X2C_SetBoolSemaphore(t^.controll);
  IF rc <> 0 THEN RAISE(rc,"CreateThread"); END;
END CreateThread;

PROCEDURE DeleteThread(VAR t: Thread);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_DeleteThread(t^.thread);
  IF rc <> 0 THEN RAISE(rc, "DeleteThread") END;
  rc := xrThreads.X2C_DeleteISOProcess(t);
  IF rc <> 0 THEN RAISE(rc, "DeleteThread") END;
  t := NIL;
END DeleteThread;

PROCEDURE SuspendThread(t: Thread);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_SuspendThread(t^.thread);
  IF rc <> 0 THEN RAISE(rc, "SuspendThread") END;
END SuspendThread;

PROCEDURE ResumeThread (t: Thread);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_ResumeThread(t^.thread);
  IF rc <> 0 THEN RAISE(rc, "ResumeThread") END;
END ResumeThread;

PROCEDURE ExitThread;
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xrThreads.X2C_DeleteISOProcess(CurrentThread());
  IF rc <> 0 THEN RAISE(rc, "ExitThread") END;
  xosThreads.X2C_ExitThread;
END ExitThread;

PROCEDURE GetThreadPriority(t: Thread): INTEGER;
VAR
  rc: SYSTEM.INT32;
  prio: INTEGER;
BEGIN
  rc := xosThreads.X2C_GetThreadPriority(t^.thread, prio);
  IF rc <> 0 THEN RAISE(rc, "GetThreadPriority") END;
  RETURN prio;
END GetThreadPriority;

PROCEDURE SetThreadPriority(t: Thread; prio: INTEGER);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_SetThreadPriority(t^.thread, prio);
  IF rc <> 0 THEN RAISE(rc, "SetThreadPriority") END;
END SetThreadPriority;


PROCEDURE CreateMutex(VAR m: Mutex);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_CreateMutex(m);
  IF rc <> 0 THEN RAISE(rc, "CreateMutex") END;
END CreateMutex;

PROCEDURE DeleteMutex(VAR m: Mutex);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_DeleteMutex(m);
  IF rc <> 0 THEN RAISE(rc, "DeleteMutex") END;
END DeleteMutex;

PROCEDURE LockMutex(m: Mutex);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_EnterMutex(m);
  IF rc <> 0 THEN RAISE(rc, "LockMutex") END;
END LockMutex;

PROCEDURE UnlockMutex(m: Mutex);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_ExitMutex(m);
  IF rc <> 0 THEN RAISE(rc, "UnlockMutex") END;
END UnlockMutex;

PROCEDURE CreateEvent (VAR e: Event);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_CreateBoolSemaphore(e);
  IF rc <> 0 THEN RAISE(rc, "CreateEvent") END;
END CreateEvent;

PROCEDURE DeleteEvent (VAR e: Event);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_DeleteBoolSemaphore(e);
  IF rc <> 0 THEN RAISE(rc, "DeleteEvent") END;
END DeleteEvent;

PROCEDURE WaitEvent (e: Event);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_AcquireBoolSemaphore(e);
  IF rc <> 0 THEN RAISE(rc, "WaitEvent") END;
END WaitEvent;

PROCEDURE ResetEvent (e: Event); (* block *) 
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_ResetBoolSemaphore(e);
  IF rc <> 0 THEN RAISE(rc, "ResetEvent") END;
END ResetEvent;

PROCEDURE SetEvent (e: Event);   (* release *) 
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_SetBoolSemaphore(e);
  IF rc <> 0 THEN RAISE(rc, "SetEvent") END;
END SetEvent;

PROCEDURE CreateSemaphore(VAR s: Semaphore; init, max: CARDINAL);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_CreateSemaphore(s,init,max);
  IF rc <> 0 THEN RAISE(rc, "CreateSemaphore") END;
END CreateSemaphore;

PROCEDURE DeleteSemaphore(VAR s: Semaphore);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_DeleteSemaphore(s);
  IF rc <> 0 THEN RAISE(rc, "DeleteSemaphore") END;
END DeleteSemaphore;

PROCEDURE WaitSemaphore(s: Semaphore);
VAR
  rc: SYSTEM.INT32;
  awaited: BOOLEAN;
BEGIN
  rc := xosThreads.X2C_AcquireSemaphore(s,FALSE,awaited);
  IF rc <> 0 THEN RAISE(rc, "AcquireSemaphore") END;
END WaitSemaphore;

PROCEDURE TryWaitSemaphore(s: Semaphore) : BOOLEAN;
VAR
  rc: SYSTEM.INT32;
  awaited: BOOLEAN;
BEGIN
  rc := xosThreads.X2C_AcquireSemaphore(s,TRUE,awaited);
  IF rc <> 0 THEN RAISE(rc, "AcquireSemaphore") END;
  RETURN NOT awaited;
END TryWaitSemaphore;

PROCEDURE ReleaseSemaphore(s: Semaphore);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_ReleaseSemaphore(s);
  IF rc <> 0 THEN RAISE(rc, "ReleaseSemaphore") END;
END ReleaseSemaphore;

PROCEDURE CreateKey(): Key;
VAR
  rc : SYSTEM.INT32;
  key: Key;
BEGIN
  rc := xosThreads.X2C_CreateKey(key);
  IF rc <> 0 THEN RAISE(rc, "CreateKey") END;
  RETURN key;
END CreateKey;

PROCEDURE DeleteKey(VAR key: Key);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_DeleteKey(key);
  IF rc <> 0 THEN RAISE(rc, "DeleteKey") END;
END DeleteKey;

PROCEDURE SetKeyValue(key: Key; value: SYSTEM.ADDRESS);
VAR
  rc: SYSTEM.INT32;
BEGIN
  rc := xosThreads.X2C_SetKeyValue(key, value);
  IF rc <> 0 THEN RAISE(rc, "SetKeyValue") END;
END SetKeyValue;

PROCEDURE GetKeyValue(key: Key): SYSTEM.ADDRESS;
VAR
  rc: SYSTEM.INT32;
  value: SYSTEM.ADDRESS;
BEGIN
  rc := xosThreads.X2C_GetKeyValue(key, value);
  IF rc <> 0 THEN RAISE(rc, "GetKeyValue") END;
  RETURN value;
END GetKeyValue;

BEGIN
  EXCEPTIONS.AllocateSource(source);
END Threads.
