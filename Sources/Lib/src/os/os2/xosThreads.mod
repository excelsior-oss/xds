(* Copyright (c)1996,97,99 XDS Ltd. All Rights Reserved *)
(*
 * XDS library.
 * Interface of XDS libraries to threads features of underlying OS.
 *)

<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE xosThreads; (* VitVit'n'Hady 10.07.96 11:04 *)

IMPORT SYSTEM, O := xOS2, xrnStart, xosMalloc;

FROM SYSTEM IMPORT INT32, ADDRESS;

TYPE
  X2C_Thread     = ADDRESS;
  X2C_Mutex      = ADDRESS;
  X2C_BSemaphore = ADDRESS;
  X2C_Semaphore  = POINTER TO RECORD
                                event :O.HEV;
                                mutex :O.HMTX;
                                count :INT32;
                                max   :INT32;
                              END;
(*
   REPRESENTATION INVARIANT:  (count >=0) & (count <= max) &
                              (count = 0 <=> event is in blocked state);
                            
 *)
  X2C_Key        = O.PULONG;

CONST
  malloc = xosMalloc.X2C_gmalloc;
  free   = xosMalloc.X2C_gfree;


(*///////////////////////////////// threads /////////////////////////*)

VAR
  tlsAdr :ADDRESS; -- this virt addr's backed by diffferent phis memory


PROCEDURE ["C"] X2C_INIT_THREADS() :SYSTEM.INT32;
VAR
  rc :O.APIRET;
  pl :O.PULONG;
BEGIN
  rc := O.DosAllocThreadLocalMemory( 1, pl ); -- alloc 1 double word
  tlsAdr := SYSTEM.ADDRESS(pl);
  RETURN rc;
END X2C_INIT_THREADS;


PROCEDURE ["C"] X2C_CurrentThread() :X2C_Thread;
VAR
  ptib :O.PTIB;
  ppib :O.PPIB;
BEGIN
  O.DosGetInfoBlocks (ptib, ppib);
  RETURN X2C_Thread (ptib^.tib_ptib2^.tib2_ultid);
END X2C_CurrentThread;


PROCEDURE ["C"] X2C_MyThreadHandle(VAR h: X2C_Thread) :SYSTEM.INT32;
BEGIN
   h := X2C_CurrentThread();
   RETURN 0;
END X2C_MyThreadHandle;


PROCEDURE ["C"] X2C_GetThreadWord (VAR w:SYSTEM.ADDRESS) :SYSTEM.INT32;
BEGIN
  SYSTEM.GET (tlsAdr, w );
  RETURN 0;
END X2C_GetThreadWord;


PROCEDURE ["C"] X2C_SetThreadWord(w: SYSTEM.ADDRESS): SYSTEM.INT32;
BEGIN
  SYSTEM.PUT (tlsAdr, w );
  RETURN 0;
END X2C_SetThreadWord;

PROCEDURE ["C"] X2C_GetMyThreadId(VAR id: SYSTEM.CARD32);
BEGIN
  id := 0; (* no that things in OS/2 as "ThreadId" in Win32 *)
END X2C_GetMyThreadId;

----------------------------- thread creating

TYPE
  StartParm = RECORD
                proc: X2C_THREAD_PROC;
                parm: SYSTEM.ADDRESS;
              END;

  pStartParm = POINTER TO StartParm;

PROCEDURE [O.APIENTRY] startThread (p :O.ULONG);
VAR
  sp   :pStartParm;
  proc :X2C_THREAD_PROC;
  aprm :SYSTEM.ADDRESS;
  eh   :O.EXCEPTIONREGISTRATIONRECORD;  (* !!! must be on the stack *)
BEGIN
  IF xrnStart.X2C_SetOSXHandler(eh) = O.ok THEN
    sp   := SYSTEM.CAST( pStartParm, p );
    proc := sp^.proc;
    aprm := sp^.parm;
    free( SYSTEM.ADDRESS(p));
    proc( aprm);
  END;
END startThread;

PROCEDURE ["C"] X2C_CreateThread ( VAR t     :X2C_Thread;
                                       proc  :X2C_THREAD_PROC;
                                       stack :SYSTEM.CARD32;
                                       param :SYSTEM.ADDRESS;
                                       prio  :SYSTEM.INT32 ) :SYSTEM.INT32;
VAR
  sp  :pStartParm;
  tid :O.TID;
  rc  :O.APIRET;
BEGIN
  sp := malloc(SIZE(StartParm));
  IF (sp=NIL) THEN RETURN INT32 (O.ERROR_NOT_ENOUGH_MEMORY) END;
  sp^.proc := proc;
  sp^.parm := param;

  rc := O.DosCreateThread(tid, startThread, O.ULONG(sp), O.CREATE_SUSPENDED, stack);
  IF (rc # O.NO_ERROR ) THEN
    free (sp);
    RETURN INT32(rc);
  END;

  IF    prio < -32 THEN prio := 32
  ELSIF prio > 95  THEN prio := 159
  ELSE  INC(prio,64);
  END;

  O.DosSetPriority (O.PRTYS_THREAD, prio DIV 32, prio MOD 32, tid );

  t := X2C_Thread(tid);
  RETURN 0;
END X2C_CreateThread;

-------------------------------

PROCEDURE ["C"] X2C_DeleteThread(VAR t :X2C_Thread) :SYSTEM.INT32;
BEGIN
 RETURN INT32(O.DosKillThread ( O.TID(t) ));
END X2C_DeleteThread;

PROCEDURE ["C"] X2C_SuspendThread( t:X2C_Thread) :SYSTEM.INT32;
BEGIN
  RETURN INT32(O.DosSuspendThread (O.TID(t)));
END X2C_SuspendThread;

PROCEDURE ["C"] X2C_ResumeThread ( t:X2C_Thread) :SYSTEM.INT32;
BEGIN
  RETURN INT32(O.DosResumeThread (O.TID(t)));
END X2C_ResumeThread;

PROCEDURE ["C"] X2C_ExitThread();
BEGIN
  O.DosExit (O.EXIT_THREAD, 0);
END X2C_ExitThread;

PROCEDURE ["C"] X2C_ScheduleMyThread();
BEGIN
  (* not supported: do nothing *)
END X2C_ScheduleMyThread;

PROCEDURE ["C"] X2C_GetThreadPriority(t: X2C_Thread; VAR prio: SYSTEM.INT32): SYSTEM.INT32;
VAR
  ptib :O.PTIB;
  ppib :O.PPIB;
  rc   :O.APIRET;
BEGIN
  rc := O.DosGetInfoBlocks (ptib, ppib);
  IF rc = O.NO_ERROR THEN
    prio := ptib^.tib_ptib2^.tib2_ulpri;
  END;
  RETURN rc
END X2C_GetThreadPriority;  

PROCEDURE ["C"] X2C_SetThreadPriority(t: X2C_Thread; prio: SYSTEM.INT32): SYSTEM.INT32;
BEGIN
  RETURN O.DosSetPriority (O.PRTYS_THREAD, prio DIV 32, prio MOD 32, SYSTEM.CAST(O.TID,t));
END X2C_SetThreadPriority;  

(*//////////////////////////// Event semaphores /////////////////////////////*)


PROCEDURE ["C"] X2C_CreateBoolSemaphore(VAR m :X2C_BSemaphore) :SYSTEM.INT32;
VAR
  hev :O.HEV;
  rc  :O.APIRET;
  i   :INTEGER;
BEGIN
  rc := O.DosCreateEventSem (NIL, hev, O.DC_SEM_SHARED, FALSE);
  (* created in blocked state *)
  m  := X2C_BSemaphore(hev);
  RETURN INT32(rc);
END X2C_CreateBoolSemaphore;


PROCEDURE ["C"] X2C_DeleteBoolSemaphore(VAR m :X2C_BSemaphore) :SYSTEM.INT32;
BEGIN
  RETURN  INT32( O.DosCloseEventSem (O.HEV(m)) );
END X2C_DeleteBoolSemaphore;


PROCEDURE ["C"] X2C_AcquireBoolSemaphore (m :X2C_BSemaphore) :SYSTEM.INT32;
BEGIN
  O.DosWaitEventSem (O.HEV(m), O.SEM_INDEFINITE_WAIT);
  RETURN 0;
END X2C_AcquireBoolSemaphore;


PROCEDURE ["C"] X2C_ResetBoolSemaphore (m: X2C_BSemaphore) :SYSTEM.INT32;
VAR 
  ignore :SYSTEM.CARD32;
BEGIN
  O.DosResetEventSem (O.HEV(m), ignore);
  RETURN 0;
END X2C_ResetBoolSemaphore;


PROCEDURE ["C"] X2C_SetBoolSemaphore (m :X2C_BSemaphore) :SYSTEM.INT32;
BEGIN
  O.DosPostEventSem ( O.HEV(m) );
  RETURN 0;
END X2C_SetBoolSemaphore;


(*//////////////////////// Win32-like semaphores ///////////////////////////*)

PROCEDURE ["C"] X2C_CreateSemaphore (VAR m :X2C_Semaphore; init, max :SYSTEM.INT32) :SYSTEM.INT32;
VAR
  event :O.HEV;
  mutex :O.HMTX;
  b     :SYSTEM.BOOL32;
  rc    :O.APIRET;
BEGIN

  IF NOT ( (init>=0) & (max>0) & (init <=max) ) THEN
    RETURN INT32(O.ERROR_INVALID_PARAMETER)
  END;

  IF (init > 0) THEN
    (* to bypass the bug in BE: VAL BOOL -> BOOL32 doesn't work *)
    b := TRUE
  ELSE
    b := FALSE
  END;
  rc := O.DosCreateEventSem (NIL, event, O.DC_SEM_SHARED, b);
  IF (rc # O.NO_ERROR) THEN RETURN INT32(rc) END;

  rc := O.DosCreateMutexSem (NIL, mutex, O.DC_SEM_SHARED, FALSE);
  IF (rc # O.NO_ERROR) THEN RETURN INT32(rc) END;

  m := malloc (SIZE(m^));
  IF (m = NIL) THEN RETURN INT32(O.ERROR_NOT_ENOUGH_MEMORY) END;

  m^.event := event;
  m^.mutex := mutex;
  m^.count := init;
  m^.max   := max;
  RETURN 0;
END X2C_CreateSemaphore;

PROCEDURE ["C"] X2C_DeleteSemaphore (VAR m :X2C_Semaphore) :SYSTEM.INT32;
BEGIN
  O.DosCloseEventSem (m^.event);
  O.DosCloseMutexSem (m^.mutex);
  free (m);
  RETURN 0;
END X2C_DeleteSemaphore;

PROCEDURE ["C"] X2C_AcquireSemaphore (m :X2C_Semaphore; nowait :BOOLEAN; VAR awaited :BOOLEAN) :SYSTEM.INT32;
VAR
  ignore :CARDINAL;
BEGIN
  WITH m^ DO
    O.DosRequestMutexSem (mutex, O.SEM_INDEFINITE_WAIT);
    LOOP
      IF (nowait) THEN
        awaited := (count # 0);
        EXIT;
      END;
      IF (count = 0) THEN
        (* hang itself *)
        O.DosResetEventSem (event, ignore);
        O.DosReleaseMutexSem (mutex);
        (*
           BAD SPOT: if thread switching occurs here & another thread executes
           X2C_ReleaseSemaphore, it may cause incorrect state of the "count" field
           (namely, blocked semaphore with non-zero count) - see the solution in
           X2C_ReleaseSemaphore
         *)
        LOOP 
          O.DosWaitEventSem (event, O.SEM_INDEFINITE_WAIT);
          O.DosRequestMutexSem (mutex, O.SEM_INDEFINITE_WAIT);
          IF (count = 0) THEN
            O.DosResetEventSem (event, ignore); 
            (* continue waiting - some thread had time to capture the sem *)
            O.DosReleaseMutexSem (mutex)
          ELSE 
            EXIT
          END;
        END;  
      END;
      DEC (count);
      EXIT;
    END;
    O.DosReleaseMutexSem (mutex);
  END;
  RETURN 0;
END X2C_AcquireSemaphore;

PROCEDURE ["C"] X2C_ReleaseSemaphore (m :X2C_Semaphore) :SYSTEM.INT32;
BEGIN
  WITH m^ DO
    O.DosRequestMutexSem (mutex, O.SEM_INDEFINITE_WAIT);
    O.DosPostEventSem (event);
    (*
       DosPostEventSem MUST be called anyway (with no respect to value of
       the "count" field) & carried out before any manipulations with the field
        - see the problem in X2C_AcquireSemaphore
     *)
    IF (count # max) THEN INC (count) END;
    O.DosReleaseMutexSem (mutex);
  END;
  RETURN 0;
END X2C_ReleaseSemaphore;


(*///////////////////////// mutex semaphores //////////////////////////////*)

PROCEDURE ["C"] X2C_CreateMutex(VAR m :X2C_Mutex) :SYSTEM.INT32;
VAR
  hmtx :O.HMTX;
  rc   :O.APIRET;
BEGIN
  rc := O.DosCreateMutexSem( NIL, hmtx, O.DC_SEM_SHARED, FALSE );
  m  := X2C_Mutex(hmtx);
  RETURN INT32(rc);
END X2C_CreateMutex;


PROCEDURE ["C"] X2C_DeleteMutex (VAR m :X2C_Mutex) :SYSTEM.INT32;
BEGIN
  RETURN INT32(O.DosCloseMutexSem (O.HMTX(m)));
END X2C_DeleteMutex;


PROCEDURE ["C"] X2C_EnterMutex(m :X2C_Mutex) :SYSTEM.INT32;
BEGIN
  RETURN INT32(O.DosRequestMutexSem (O.HMTX(m), O.SEM_INDEFINITE_WAIT) );
END X2C_EnterMutex;


PROCEDURE ["C"] X2C_ExitMutex(m :X2C_Mutex) :SYSTEM.INT32;
BEGIN
  RETURN INT32(O.DosReleaseMutexSem(O.HMTX(m)));
END X2C_ExitMutex;


(*//////////////////////// multiple semaphores ///////////////////////*)

PROCEDURE ["C"] X2C_SemaphoreToEvent(m :X2C_BSemaphore) :X2C_EventSource;
BEGIN
  RETURN SYSTEM.CAST(X2C_EventSource, m);
END X2C_SemaphoreToEvent;


PROCEDURE ["C"] X2C_WaitEvents (s- :ARRAY OF X2C_EventSource; no :CARDINAL) :SYSTEM.INT32;
TYPE
  tP2SemRecA = POINTER TO ARRAY [0..0FFFFFFH] OF O.SEMRECORD;
(* this type are used only to cast rather than to instantiate *)
VAR
  i      :CARDINAL;
  rc     :O.APIRET;
  pSemRA :tP2SemRecA;
  hmux   :O.HMUX;

BEGIN
  pSemRA := tP2SemRecA( malloc( no * SIZE (O.SEMRECORD) ) );

  FOR i:=0 TO no-1 DO
     pSemRA^[i].hsemCur := O.HSEM(s[i]);
     pSemRA^[i].ulUser  := 0;
  END;
  rc := O.DosCreateMuxWaitSem (NIL, hmux, no, pSemRA^, O.DC_SEM_SHARED + O.DCMW_WAIT_ANY );
  IF ( rc # O.NO_ERROR ) THEN RETURN rc; END;

  free ( ADDRESS(pSemRA) );

  -- waiting while any one of the event sem list is posted (signaled)
  rc := O.DosWaitMuxWaitSem ( hmux, O.SEM_INDEFINITE_WAIT, i );
  IF ( rc # O.NO_ERROR ) THEN RETURN rc; END;

  RETURN INT32(O.DosCloseMuxWaitSem ( hmux ));
END X2C_WaitEvents;

PROCEDURE ["C"] X2C_CreateKey(VAR key: X2C_Key) : SYSTEM.INT32;
BEGIN
  RETURN O.DosAllocThreadLocalMemory(1, key);
END X2C_CreateKey;

PROCEDURE ["C"] X2C_DeleteKey(key: X2C_Key) : SYSTEM.INT32;
BEGIN
  RETURN O.DosFreeThreadLocalMemory(key);
END X2C_DeleteKey;

PROCEDURE ["C"] X2C_SetKeyValue(key: X2C_Key; value: SYSTEM.ADDRESS): SYSTEM.INT32;
BEGIN
  SYSTEM.PUT(key, value);
  RETURN 0;
END X2C_SetKeyValue;

PROCEDURE ["C"] X2C_GetKeyValue(key: X2C_Key; VAR value: SYSTEM.ADDRESS): SYSTEM.INT32;
BEGIN
  SYSTEM.GET(key, value);
  RETURN 0;
END X2C_GetKeyValue;

PROCEDURE ["C"] X2C_SyncThreadState(thr: X2C_Thread;
                            VAR stk_end: SYSTEM.ADDRESS;
                                    buf: SYSTEM.ADDRESS;
                               dmp_size: SYSTEM.CARD32;
                          VAR reg_dsize: SYSTEM.CARD32): SYSTEM.INT32;
BEGIN
  ASSERT (FALSE);
  RETURN 0;
END X2C_SyncThreadState;

END xosThreads.
