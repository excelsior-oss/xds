(* Copyright (c) XDS 1996,98.  All Rights Reserved *)

(*
 * XDS library.
 * Interface of XDS libraries to threads features of underlying OS.
 *)

<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE xosThreads; (* VitVit'n'Hady. 10.07.96 11:04 *)

IMPORT  SYSTEM, xWin32, W := Windows, xosMalloc;

TYPE
  X2C_Mutex = SYSTEM.ADDRESS;
  X2C_Thread = SYSTEM.ADDRESS;
  X2C_Semaphore = SYSTEM.ADDRESS;
  X2C_BSemaphore = SYSTEM.ADDRESS;
  X2C_Key = SYSTEM.ADDRESS;

CONST
  malloc = xosMalloc.X2C_gmalloc;
  free   = xosMalloc.X2C_gfree;

VAR index: xWin32.DWORD;

PROCEDURE ["C"] X2C_INIT_THREADS(): SYSTEM.INT32;
BEGIN
  index:=xWin32.TlsAlloc();
  IF index=0FFFFFFFFH THEN
    RETURN xWin32.GetLastError();
  END;
  RETURN 0;
END X2C_INIT_THREADS;

PROCEDURE ["C"] X2C_CurrentThread(): X2C_Thread;
BEGIN
  RETURN SYSTEM.CAST(X2C_Thread,xWin32.GetCurrentThread());
END X2C_CurrentThread;

PROCEDURE ["C"] X2C_MyThreadHandle(VAR h: X2C_Thread): SYSTEM.INT32;
  VAR prs,new: xWin32.HANDLE;
BEGIN
  prs:=xWin32.GetCurrentProcess();
  IF xWin32.DuplicateHandle(prs,xWin32.GetCurrentThread(),prs,new,
                            {},FALSE,xWin32.DUPLICATE_SAME_ACCESS)
  THEN
    h:=SYSTEM.CAST(X2C_Thread,new);
    RETURN 0;
  END;
  RETURN xWin32.GetLastError();
END X2C_MyThreadHandle;

PROCEDURE ["C"] X2C_GetThreadWord(VAR w: SYSTEM.ADDRESS): SYSTEM.INT32;
BEGIN
  w:=xWin32.TlsGetValue(index);
  IF w=NIL THEN RETURN xWin32.GetLastError() END;
  RETURN 0;
END X2C_GetThreadWord;

PROCEDURE ["C"] X2C_SetThreadWord(w: SYSTEM.ADDRESS): SYSTEM.INT32;
BEGIN
  IF xWin32.TlsSetValue(index,w) THEN RETURN 0 END;
  RETURN xWin32.GetLastError();
END X2C_SetThreadWord;

PROCEDURE ["C"] X2C_GetMyThreadId(VAR id: SYSTEM.CARD32);
BEGIN
  id:=xWin32.GetCurrentThreadId();
END X2C_GetMyThreadId;

TYPE
  StartParm = POINTER TO SParmRec;
  SParmRec = RECORD
    proc: X2C_THREAD_PROC;
    parm: SYSTEM.ADDRESS;
  END;

PROCEDURE ["StdCall"] startThread(p: SYSTEM.ADDRESS): SYSTEM.INT32;
  VAR sp: StartParm; proc: X2C_THREAD_PROC;
BEGIN
  sp:=SYSTEM.CAST(StartParm,p);
  proc:=sp^.proc;
  p:=sp^.parm;
  free(sp);
  proc(p);
  RETURN 0;
END startThread;

PROCEDURE ["C"] X2C_CreateThread(VAR t: X2C_Thread;
                                  proc: X2C_THREAD_PROC;
                                 stack: SYSTEM.CARD32;
                                 param: SYSTEM.ADDRESS;
                                 prio: SYSTEM.INT32): SYSTEM.INT32;
  VAR sp: StartParm;
       h: xWin32.HANDLE;
     res: SYSTEM.INT32;
  ignore: xWin32.DWORD;
BEGIN
  sp:=malloc(SIZE(SParmRec));
  IF sp=NIL THEN RETURN xWin32.ERROR_NOT_ENOUGH_MEMORY END;
  sp^.proc:=proc;
  sp^.parm:=param;
  h:=xWin32.CreateThread(NIL,stack,startThread,sp,xWin32.CREATE_SUSPENDED,ignore);
  IF h=0 THEN
    res:=xWin32.GetLastError();
    free(sp);
    RETURN res;
  END;
  IF prio#0 THEN
    IF prio <-2 THEN prio:=-15 END;
    IF prio > 2 THEN prio:= 15 END;
    IF NOT xWin32.SetThreadPriority(h,prio) THEN END;
  END;
  t:=SYSTEM.CAST(X2C_Thread,h);
  RETURN 0;
END X2C_CreateThread;

PROCEDURE ["C"] X2C_DeleteThread(VAR t: X2C_Thread): SYSTEM.INT32;
BEGIN
  IF NOT xWin32.TerminateThread(SYSTEM.CAST(xWin32.HANDLE,t),0) THEN
    RETURN xWin32.GetLastError();
  END;
  IF xWin32.CloseHandle(SYSTEM.CAST(xWin32.HANDLE,t)) THEN END;
END X2C_DeleteThread;

PROCEDURE ["C"] X2C_SuspendThread(t: X2C_Thread): SYSTEM.INT32;
BEGIN
  IF xWin32.SuspendThread(SYSTEM.CAST(xWin32.HANDLE,t))=0FFFFFFFFH THEN
    RETURN xWin32.GetLastError();
  END;
  RETURN 0;
END X2C_SuspendThread;

PROCEDURE ["C"] X2C_ResumeThread (t: X2C_Thread): SYSTEM.INT32;
BEGIN
  IF xWin32.ResumeThread(SYSTEM.CAST(xWin32.HANDLE,t))=0FFFFFFFFH THEN
    RETURN xWin32.GetLastError();
  END;
  RETURN 0;
END X2C_ResumeThread;

PROCEDURE ["C"] X2C_ExitThread();
BEGIN
  xWin32.ExitThread(0);
END X2C_ExitThread;

PROCEDURE ["C"] X2C_ScheduleMyThread;
BEGIN
  xWin32.Sleep(0);
END X2C_ScheduleMyThread;

PROCEDURE ["C"] X2C_GetThreadPriority(t: X2C_Thread; VAR prio: SYSTEM.INT32): SYSTEM.INT32;
BEGIN
  prio := xWin32.GetThreadPriority(SYSTEM.CAST(xWin32.HANDLE,t));
  IF prio = xWin32.THREAD_PRIORITY_ERROR_RETURN THEN
    RETURN xWin32.GetLastError();
  ELSE
    RETURN 0
  END;
END X2C_GetThreadPriority;  

PROCEDURE ["C"] X2C_SetThreadPriority(t: X2C_Thread; prio: SYSTEM.INT32): SYSTEM.INT32;
BEGIN
  IF NOT xWin32.SetThreadPriority(SYSTEM.CAST(xWin32.HANDLE,t),prio) THEN
    RETURN xWin32.GetLastError();
  ELSE
    RETURN 0
  END;
END X2C_SetThreadPriority;  

(*///////////////////////// Critical sections /////////////////////////*)

(* For historical reasons these functions has word 'mutex' in their names
   (OS/2 critical sections suspend all threads, so OS/2 implementation uses
   mutex semaphores).
*) 

PROCEDURE ["C"] X2C_CreateMutex (VAR m :X2C_Mutex) : SYSTEM.INT32;
BEGIN
  m := malloc (SIZE(W.RTL_CRITICAL_SECTION));
  IF (m = NIL) THEN RETURN W.ERROR_NOT_ENOUGH_MEMORY END;
  W.InitializeCriticalSection (SYSTEM.CAST(W.PRTL_CRITICAL_SECTION,m)^);
  RETURN 0;
END X2C_CreateMutex;

PROCEDURE ["C"] X2C_DeleteMutex (VAR m :X2C_Mutex) : SYSTEM.INT32;
BEGIN
  W.DeleteCriticalSection (SYSTEM.CAST(W.PRTL_CRITICAL_SECTION,m)^);
  free (m);
  m := NIL;
  RETURN 0;
END X2C_DeleteMutex;

PROCEDURE ["C"] X2C_EnterMutex (m :X2C_Mutex) : SYSTEM.INT32;
BEGIN
  W.EnterCriticalSection (SYSTEM.CAST(W.PRTL_CRITICAL_SECTION,m)^);
  RETURN 0;
END X2C_EnterMutex;

PROCEDURE ["C"] X2C_ExitMutex (m :X2C_Mutex) : SYSTEM.INT32;
BEGIN
  W.LeaveCriticalSection (SYSTEM.CAST(W.PRTL_CRITICAL_SECTION,m)^);
  RETURN 0;
END X2C_ExitMutex;

(*

Original implementation:

PROCEDURE ["C"] X2C_CreateMutex (VAR m: X2C_Mutex): SYSTEM.INT32;
  VAR x: X2C_Mutex; h: xWin32.HANDLE;
BEGIN
  h:=xWin32.CreateMutexA(NIL,FALSE,NIL);
  IF h=0 THEN RETURN xWin32.GetLastError() END;
  m:=SYSTEM.CAST(X2C_Mutex,h);
  RETURN 0;
END X2C_CreateMutex;

PROCEDURE ["C"] X2C_DeleteMutex (VAR m: X2C_Mutex): SYSTEM.INT32;
  VAR h: xWin32.HANDLE;
BEGIN
  h:=SYSTEM.CAST(xWin32.HANDLE,m);
  IF xWin32.CloseHandle(h) THEN RETURN 0 END;
  RETURN xWin32.GetLastError();
END X2C_DeleteMutex;

PROCEDURE ["C"] X2C_EnterMutex (m: X2C_Mutex): SYSTEM.INT32;
BEGIN
  IF xWin32.WaitForSingleObject(SYSTEM.CAST(xWin32.HANDLE,m),MAX(SYSTEM.CARD32))=xWin32.WAIT_FAILED THEN
    RETURN xWin32.GetLastError()
  END;
  RETURN 0;
END X2C_EnterMutex;

PROCEDURE ["C"] X2C_ExitMutex (m: X2C_Mutex): SYSTEM.INT32;
BEGIN
  IF xWin32.ReleaseMutex(SYSTEM.CAST(xWin32.HANDLE,m)) THEN RETURN 0 END;
  RETURN xWin32.GetLastError();
END X2C_ExitMutex;
*)

(*///////////////////////////// Semaphores /////////////////////////////*)

PROCEDURE ["C"] X2C_CreateSemaphore(VAR m: X2C_Semaphore; init, max: SYSTEM.INT32): SYSTEM.INT32;
  VAR h: xWin32.HANDLE;
BEGIN
  h:=xWin32.CreateSemaphoreA(NIL,init,max,NIL);
  IF h=0 THEN
    RETURN xWin32.GetLastError();
  END;
  m:=SYSTEM.CAST(X2C_Semaphore,h);
  RETURN 0;
END X2C_CreateSemaphore;


PROCEDURE ["C"] X2C_DeleteSemaphore(VAR m: X2C_Semaphore): SYSTEM.INT32;
  VAR h: xWin32.HANDLE;
BEGIN
  h:=SYSTEM.CAST(xWin32.HANDLE,m);
  IF xWin32.CloseHandle(h) THEN RETURN 0 END;
  RETURN xWin32.GetLastError();
END X2C_DeleteSemaphore;

PROCEDURE ["C"] X2C_AcquireSemaphore (m: X2C_Semaphore; nowait: BOOLEAN; VAR awaited: BOOLEAN): SYSTEM.INT32;
  VAR res: xWin32.DWORD; to: SYSTEM.CARD32;
BEGIN
  IF nowait THEN to:=0 ELSE to:=MAX(SYSTEM.CARD32) END;
  res:=xWin32.WaitForSingleObject(SYSTEM.CAST(xWin32.HANDLE,m),to);
  IF res=xWin32.WAIT_FAILED THEN RETURN xWin32.GetLastError() END;
  awaited:=(res#xWin32.WAIT_TIMEOUT);
  RETURN 0;
END X2C_AcquireSemaphore;

PROCEDURE ["C"] X2C_ReleaseSemaphore(m: X2C_Semaphore): SYSTEM.INT32;
  VAR res: SYSTEM.INT32;
BEGIN
  res:=0;
  IF ~xWin32.ReleaseSemaphore(SYSTEM.CAST(xWin32.HANDLE,m),1,NIL) THEN
    res:=xWin32.GetLastError();
    IF (res=xWin32.ERROR_TOO_MANY_POSTS) OR (res=xWin32.ERROR_INVALID_PARAMETER) THEN
      res:=0
    END;
  END;
  RETURN res;
END X2C_ReleaseSemaphore;

(*///////////////////////////////// Events /////////////////////////////////*)


PROCEDURE ["C"] X2C_CreateBoolSemaphore (VAR e :X2C_BSemaphore) :SYSTEM.INT32;
BEGIN
  e := W.CreateEvent (NIL, TRUE, FALSE, NIL);  -- manual, initially blocked
  IF (e = NIL) THEN RETURN W.GetLastError() END;
  RETURN 0;
END X2C_CreateBoolSemaphore;


PROCEDURE ["C"] X2C_DeleteBoolSemaphore (VAR e :X2C_BSemaphore) :SYSTEM.INT32;
BEGIN
  IF W.CloseHandle (e) THEN RETURN 0 END;
  RETURN W.GetLastError();
END X2C_DeleteBoolSemaphore;


PROCEDURE ["C"] X2C_AcquireBoolSemaphore (e :X2C_BSemaphore) :SYSTEM.INT32;
BEGIN
  IF (W.WaitForSingleObject (e, W.INFINITE) = W.WAIT_FAILED) THEN
    RETURN W.GetLastError()
  END;
  RETURN 0;
END X2C_AcquireBoolSemaphore;


PROCEDURE ["C"] X2C_SetBoolSemaphore (e :X2C_BSemaphore) :SYSTEM.INT32;
BEGIN
  IF W.SetEvent (e) THEN RETURN 0 END;
  RETURN xWin32.GetLastError();
END X2C_SetBoolSemaphore;


PROCEDURE ["C"] X2C_ResetBoolSemaphore (e :X2C_BSemaphore) :SYSTEM.INT32;
BEGIN
  IF W.ResetEvent (e) THEN RETURN 0 END;
  RETURN xWin32.GetLastError();
END X2C_ResetBoolSemaphore;




PROCEDURE ["C"] X2C_SemaphoreToEvent(m: X2C_BSemaphore): X2C_EventSource;
BEGIN
  RETURN SYSTEM.CAST(X2C_EventSource,m);
END X2C_SemaphoreToEvent;


PROCEDURE ["C"] X2C_WaitEvents(s-: ARRAY OF X2C_EventSource; no: CARDINAL): SYSTEM.INT32;
  VAR res: xWin32.DWORD;
BEGIN
  res:=xWin32.WaitForMultipleObjects(no,SYSTEM.ADR(s),FALSE,MAX(xWin32.DWORD));
  IF res=xWin32.WAIT_FAILED THEN
    RETURN xWin32.GetLastError()
  END;
  RETURN 0;
END X2C_WaitEvents;

PROCEDURE ["C"] X2C_CreateKey(VAR key: X2C_Key) : SYSTEM.INT32;
VAR
  index: xWin32.DWORD;
BEGIN
  index := xWin32.TlsAlloc();
  IF index = 0FFFFFFFFH THEN
    RETURN xWin32.GetLastError();
  END;
  key := SYSTEM.CAST(SYSTEM.ADDRESS,index);
  RETURN 0;
END X2C_CreateKey;

PROCEDURE ["C"] X2C_DeleteKey(key: X2C_Key) : SYSTEM.INT32;
BEGIN
  IF NOT xWin32.TlsFree(SYSTEM.CAST(xWin32.DWORD,key)) THEN  
    RETURN xWin32.GetLastError();
  END;
  RETURN 0;
END X2C_DeleteKey;

PROCEDURE ["C"] X2C_SetKeyValue(key: X2C_Key; value: SYSTEM.ADDRESS): SYSTEM.INT32;
BEGIN
  IF NOT xWin32.TlsSetValue(SYSTEM.CAST(xWin32.DWORD,key),value) THEN
    RETURN xWin32.GetLastError();
  END;
  RETURN 0;
END X2C_SetKeyValue;

PROCEDURE ["C"] X2C_GetKeyValue(key: X2C_Key; VAR value: SYSTEM.ADDRESS): SYSTEM.INT32;
BEGIN
  value := xWin32.TlsGetValue(SYSTEM.CAST(xWin32.DWORD,key));
  IF value = NIL THEN
    RETURN xWin32.GetLastError();
  END;
  RETURN 0;
END X2C_GetKeyValue;


PROCEDURE ["C"] X2C_SyncThreadState(thr: X2C_Thread;
                            VAR stk_end: SYSTEM.ADDRESS;
                                    buf: SYSTEM.ADDRESS;
                               dmp_size: SYSTEM.CARD32;
                          VAR reg_dsize: SYSTEM.CARD32): SYSTEM.INT32;
  VAR ctx: xWin32.CONTEXT;
BEGIN
  IF dmp_size<28 THEN RETURN xWin32.ERROR_NOT_ENOUGH_MEMORY END;
  ctx.flags:=xWin32.CONTEXT_FULL;
  IF NOT xWin32.GetThreadContext(SYSTEM.CAST(xWin32.HANDLE,thr),ctx) THEN
    RETURN xWin32.GetLastError();
  END;
  stk_end:=SYSTEM.CAST(SYSTEM.ADDRESS,ctx.Esp);
  reg_dsize:=28;
  SYSTEM.PUT(buf,ctx.Eax); buf:=SYSTEM.ADDADR(buf,4);
  SYSTEM.PUT(buf,ctx.Ecx); buf:=SYSTEM.ADDADR(buf,4);
  SYSTEM.PUT(buf,ctx.Edx); buf:=SYSTEM.ADDADR(buf,4);
  SYSTEM.PUT(buf,ctx.Ebx); buf:=SYSTEM.ADDADR(buf,4);
  SYSTEM.PUT(buf,ctx.Edi); buf:=SYSTEM.ADDADR(buf,4);
  SYSTEM.PUT(buf,ctx.Esi); buf:=SYSTEM.ADDADR(buf,4);
  SYSTEM.PUT(buf,ctx.Ebp);
  RETURN 0;
END X2C_SyncThreadState;

END xosThreads.