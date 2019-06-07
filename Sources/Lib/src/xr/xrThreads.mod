(* Copyright (c) 1994,95,99 XDS Ltd, Russia. All Rights Reserved. *)
<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<* +m2extensions*>
<*- gendebug    *> (* Don enable! History would not work (SYSTEM.CODE) *)
IMPLEMENTATION MODULE xrThreads; (* Hady. 12.07.96 12:52 *)

IMPORT  xmRTS, SYSTEM;

<* IF multithread THEN *>
IMPORT OS:=xosThreads, X2C;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

TYPE
  Thread = OS.X2C_Thread;
  Mutex  = OS.X2C_Mutex;

PROCEDURE ["C"] X2C_NewMutex(VAR m: xmRTS.X2C_MUTEX);
  VAR x: Mutex;
BEGIN
  IF OS.X2C_CreateMutex(x)#0 THEN X2C.X2C_TRAP(X2C.X2C_internalError) END;
  m:=SYSTEM.CAST(xmRTS.X2C_MUTEX,x);
END X2C_NewMutex;

PROCEDURE ["C"] X2C_DelMutex(VAR m: xmRTS.X2C_MUTEX);
  VAR x: Mutex;
BEGIN
  x:=SYSTEM.CAST(Mutex,m);
  IF OS.X2C_DeleteMutex(x)#0 THEN X2C.X2C_TRAP(X2C.X2C_internalError) END;
END X2C_DelMutex;

PROCEDURE ["C"] X2C_LockMutex(m: xmRTS.X2C_MUTEX);
BEGIN
  IF OS.X2C_EnterMutex(SYSTEM.CAST(Mutex,m))#0 THEN
    X2C.X2C_TRAP(X2C.X2C_internalError)
  END;
END X2C_LockMutex;

PROCEDURE ["C"] X2C_FreeMutex(m: xmRTS.X2C_MUTEX);
BEGIN
  IF OS.X2C_ExitMutex(SYSTEM.CAST(Mutex,m))#0 THEN
    X2C.X2C_TRAP(X2C.X2C_internalError)
  END;
END X2C_FreeMutex;

PROCEDURE ["C"] startThread(parm: SYSTEM.ADDRESS);
VAR
  p  : X2C_Process;
  rc : SYSTEM.INT32;
BEGIN
  p := SYSTEM.CAST(X2C_Process,parm);
  xmRTS.X2C_InitCoroutine(p^.default, SYSTEM.ADR(parm));
  p^.current := p^.default;
  rc := OS.X2C_SetThreadWord(p);
  IF rc <> 0 THEN
(* here we cannot raise an ordinary exception because
   there is no current coroutine here *)
                    (* 012345678901234567890123456789 *)
(* !!!
    xrtsOS.X2C_StdOut("#RTS: init thread error ",23);
    xrtsOS.X2C_StdOutD(res,0);
    xrtsOS.X2C_StdOutN;
    xrtsOS.X2C_StdOutFlush;
    xrtsOS.X2C_doexit(res);
*)
  END;
  OS.X2C_GetMyThreadId(p^.threadId);
  p^.sources[0] := OS.X2C_SemaphoreToEvent(p^.controll);
  p^.scount := 1;
  X2C_RegisterThread(p);
  xmRTS.X2C_RegisterCoroutine(p^.default);
  rc := OS.X2C_SetBoolSemaphore (p^.controll1);
  IF rc <> 0 THEN
    X2C.X2C_TRAP(X2C.X2C_internalError);
  END;
  rc := OS.X2C_AcquireBoolSemaphore (p^.controll);
  IF rc <> 0 THEN
    X2C.X2C_TRAP(X2C.X2C_internalError);
  END;
  p^.body;
  rc := X2C_DeleteISOProcess(p);
  IF rc <> 0 THEN
    X2C.X2C_TRAP(X2C.X2C_internalError);
  END;
  OS.X2C_ExitThread;
END startThread;

PROCEDURE ["C"] X2C_CreateISOProcess(VAR p: X2C_Process;
                                      proc: Body;
                                     stack: CARDINAL;
                                       par: SYSTEM.ADDRESS;
                                      prio: INTEGER): SYSTEM.INT32;
VAR
  rc: SYSTEM.INT32;
BEGIN
  NEW(p);
  IF p = NIL THEN X2C.X2C_TRAP(X2C.X2C_noMemoryException) END;
  WITH p^ DO
    param   := par;
    info    := NIL;
    current := NIL;
    urgency := prio;
    tags    := {};
    body    := proc;
    scount  := 0;
    NEW(default);
    IF default = NIL THEN X2C.X2C_TRAP(X2C.X2C_noMemoryException) END;
  END;
  rc := OS.X2C_CreateBoolSemaphore(p^.controll);
  IF rc <> 0 THEN
    DISPOSE(p^.default);
    DISPOSE(p);
    RETURN rc;
  END;
  rc := OS.X2C_CreateBoolSemaphore(p^.controll1);
  IF rc <> 0 THEN
    DISPOSE(p^.default);
    DISPOSE(p);
    RETURN rc;
  END;
  rc := OS.X2C_CreateThread(p^.thread,startThread,stack,p,prio);
  IF rc <> 0 THEN
    OS.X2C_DeleteBoolSemaphore(p^.controll);
    DISPOSE(p^.default);
    DISPOSE(p);
    RETURN rc;
  END;
  rc := OS.X2C_ResumeThread(p^.thread);
  IF rc <> 0 THEN
    OS.X2C_DeleteBoolSemaphore(p^.controll);
    OS.X2C_DeleteThread(p^.thread);
    DISPOSE(p^.default);
    DISPOSE(p);
    RETURN rc;
  END;
  rc := OS.X2C_AcquireBoolSemaphore (p^.controll1);
  OS.X2C_DeleteBoolSemaphore(p^.controll1);
  IF rc <> 0 THEN
    OS.X2C_DeleteBoolSemaphore(p^.controll);
    OS.X2C_DeleteThread(p^.thread);
    DISPOSE(p^.default);
    DISPOSE(p);
    RETURN rc;
  END; 
  
  RETURN 0;
END X2C_CreateISOProcess;

PROCEDURE ["C"] X2C_DeleteISOProcess(p: X2C_Process) : SYSTEM.INT32;
VAR
  rc: SYSTEM.INT32;
BEGIN
  X2C_UnregisterThread(p);
  xmRTS.X2C_UnregisterCoroutine(p^.default);
  rc := OS.X2C_DeleteBoolSemaphore(p^.controll);
  IF rc <> 0 THEN RETURN rc END;
  IF NOT (X2C_main_thread IN p^.tags) THEN
    DISPOSE(p^.default);
    DISPOSE(p);
  END;
  RETURN 0;
END X2C_DeleteISOProcess;

VAR
  prs: X2C_Process;
  plock: xmRTS.X2C_MUTEX;

PROCEDURE current(): X2C_Process;
  VAR a: SYSTEM.ADDRESS;
BEGIN
  IF OS.X2C_GetThreadWord(a)#0 THEN X2C.X2C_TRAP(X2C.X2C_internalError) END;
  RETURN a;
END current;

PROCEDURE ["C"] X2C_RegisterThread(p: X2C_Process);
BEGIN
  X2C_LockMutex(plock);
    IF prs=NIL THEN
      p^.fwd:=p;
      p^.bck:=p;
      prs:=p
    ELSE
      p^.fwd:=prs;
      p^.bck:=prs^.bck;
      p^.fwd^.bck:=p;
      p^.bck^.fwd:=p;
    END;
  X2C_FreeMutex(plock);
END X2C_RegisterThread;

PROCEDURE ["C"] X2C_UnregisterThread(p: X2C_Process);
BEGIN
  X2C_LockMutex(plock);
    IF p=prs THEN prs:=prs^.fwd END;
    IF p=prs THEN
      prs:=NIL
    ELSE
      p^.fwd^.bck:=p^.bck;
      p^.bck^.fwd:=p^.fwd;
    END;
    p^.fwd:=NIL;
    p^.bck:=NIL;
  X2C_FreeMutex(plock);
END X2C_UnregisterThread;

PROCEDURE ["C"] X2C_StartIterateThreads;
BEGIN
  X2C_LockMutex(plock);
END X2C_StartIterateThreads;

PROCEDURE ["C"] X2C_StopIterateThreads;
BEGIN
  X2C_FreeMutex(plock);
END X2C_StopIterateThreads;

PROCEDURE ["C"] X2C_SetCurrent(c: xmRTS.X2C_Coroutine);
  VAR p: X2C_Process;
BEGIN
  p:=current();
  p^.current:=c;
END X2C_SetCurrent;

PROCEDURE ["C"] X2C_GetCurrent(): xmRTS.X2C_Coroutine;
  VAR p: X2C_Process;
BEGIN
  p:=current();
  RETURN p^.current;
END X2C_GetCurrent;

VAR
  mainPrs: X2C_ProcessDesc;

PROCEDURE ["C"] X2C_InitThreads(): SYSTEM.INT32;
  VAR p: X2C_Process;
    res: SYSTEM.INT32;
      x: Mutex;
BEGIN
  res:=OS.X2C_INIT_THREADS();
  IF res#0 THEN RETURN res END;
  p:=SYSTEM.ADR(mainPrs);
  WITH p^ DO
    bck:=p;
    fwd:=p;
    param:=NIL;
    info:=NIL;
    current:=NIL;
    OS.X2C_GetMyThreadId(threadId);
    res:=OS.X2C_MyThreadHandle(thread);
    IF res#0 THEN RETURN res END;
    urgency:=0;
    res:=OS.X2C_CreateBoolSemaphore(controll);
    IF res#0 THEN RETURN res END;
    sources[0]:=OS.X2C_SemaphoreToEvent(controll);
    scount:=1;
    tags:={X2C_main_thread};
  END;
  prs:=p;
  res:=OS.X2C_SetThreadWord(p);
  IF res=0 THEN
    res:=OS.X2C_CreateMutex(x);
    IF res=0 THEN plock:=SYSTEM.CAST(xmRTS.X2C_MUTEX,x) END;
  END;
  RETURN res;
END X2C_InitThreads;

PROCEDURE ["C"] X2C_PrepareToGC;
  VAR p,t: X2C_Process;
      res: SYSTEM.INT32;
      c0,ct: xmRTS.X2C_Coroutine;
BEGIN
  xmRTS.X2C_LockCoroutines;
  c0:=xmRTS.X2C_GetCurrent(); ct:=c0;
  REPEAT
    ct^.reg_dsize:=0;
    ct:=ct^.fwd;
  UNTIL ct=c0;

  X2C_StartIterateThreads;
    p:=current(); t:=p;
    LOOP
      t:=t^.fwd;
      IF t=p THEN EXIT END;
      IF NOT (X2C_no_GC IN t^.tags) THEN
        res:=OS.X2C_SuspendThread(t^.thread);
        IF res#0 THEN
          WHILE p#t DO
            IF NOT (X2C_no_GC IN t^.tags) THEN
              OS.X2C_ResumeThread(p^.thread);
            END;
            p:=p^.fwd;
          END;
          xmRTS.X2C_UnlockCoroutines;
          X2C.X2C_TRAP(X2C.X2C_internalError);
        END;
      END;
    END;
  X2C_StopIterateThreads;
  xmRTS.X2C_UnlockCoroutines; (* no need to lock them, 'cause all threads are stopped *)
  t:=p;
  REPEAT
    IF NOT (X2C_no_GC IN t^.tags) THEN
      res:=OS.X2C_SyncThreadState(
             t^.thread,
             t^.current^.stk_end,
             SYSTEM.ADR(t^.current^.reg_dump),
             xmRTS.X2C_REGSIZE*SIZE(SYSTEM.CARD32),
             t^.current^.reg_dsize
             );
      IF res#0 THEN
        t:=p;
        REPEAT
          IF NOT (X2C_no_GC IN t^.tags) THEN
            OS.X2C_ResumeThread(t^.thread);
          END;
          t:=t^.fwd;
        UNTIL t=p;
        X2C.X2C_TRAP(X2C.X2C_internalError);
      END;
    ELSE
      t^.current^.reg_dsize:=0;
    END;
    t:=t^.fwd;
  UNTIL t=p;
  ct:=c0;
  REPEAT
    IF ct^.reg_dsize=0 THEN xmRTS.X2C_CopyJmpBuf(ct) END;
    ct:=ct^.fwd;
  UNTIL ct=c0;
END X2C_PrepareToGC;

PROCEDURE ["C"] X2C_FreeAfterGC;
  VAR p,t: X2C_Process; res: SYSTEM.INT32;
BEGIN
  p:=current(); t:=p;
  REPEAT
    IF NOT (X2C_no_GC IN t^.tags) THEN
      res:=OS.X2C_ResumeThread(t^.thread);
      IF res#0 THEN
        X2C.X2C_TRAP(X2C.X2C_internalError);
      END;
    END;
    t:=t^.fwd;
  UNTIL t=p;
END X2C_FreeAfterGC;

<* ELSE *>

VAR current: xmRTS.X2C_Coroutine;

PROCEDURE ["C"] X2C_SetCurrent(c: xmRTS.X2C_Coroutine);
BEGIN
  current:=c;
END X2C_SetCurrent;

PROCEDURE ["C"] X2C_GetCurrent(): xmRTS.X2C_Coroutine;
BEGIN
  RETURN current
END X2C_GetCurrent;

PROCEDURE ["C"] X2C_InitThreads(): SYSTEM.INT32;
BEGIN
  current:=NIL;
  RETURN 0;
END X2C_InitThreads;

PROCEDURE ["C"] X2C_PrepareToGC;
  VAR c: xmRTS.X2C_Coroutine;
BEGIN
  c:=current;
  REPEAT
    xmRTS.X2C_CopyJmpBuf(c);
    c:=c^.fwd;
  UNTIL  c=current;
END X2C_PrepareToGC;

PROCEDURE ["C"] X2C_FreeAfterGC;
BEGIN
  (* empty implementation here *)
END X2C_FreeAfterGC;

<* END *>

END xrThreads.
