(* Copyright (c) 1999 XDS Ltd, Russia. All Rights Reserved.  *)
(* Copyright (c) 2003 Excelsior, Russia. All Rights Reserved. *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosThreads; (* Jek 13.02.2003 *)

IMPORT SYSTEM, errno, signal;

IMPORT ptrace;

FROM types IMPORT uid_t, pid_t, size_t;
FROM stdlib IMPORT malloc, free;
FROM unistd IMPORT geteuid, getpid, sysconf;
FROM select IMPORT fd_set, select;
FROM x2cLib IMPORT FD_SET, FD_ZERO, get_errno;
FROM wait IMPORT waitpid, __WALL;
FROM pthread IMPORT
    pthread_t
  , pthread_key_t
  , pthread_attr_t
  , pthread_mutex_t
  , pthread_cond_t
  , pthread_create
  , pthread_cancel
  , pthread_kill
  , pthread_exit
  , pthread_yield
  , pthread_self
  , pthread_equal
  , pthread_getschedparam
  , pthread_setschedparam
  , pthread_attr_init
  , pthread_attr_setinheritsched
  , pthread_attr_setstacksize
  , pthread_attr_setschedpolicy
  , pthread_attr_getschedparam
  , pthread_attr_setschedparam
  , pthread_attr_destroy
  , pthread_mutex_init
  , pthread_mutex_destroy
  , pthread_mutex_lock
  , pthread_mutex_unlock
  , pthread_cond_init
  , pthread_cond_destroy
  , pthread_cond_wait
  , pthread_cond_signal
  , pthread_cond_broadcast
  , pthread_key_create
  , pthread_key_delete
  , pthread_setspecific
  , pthread_getspecific
  , sched_param
  , PTHREAD_THREADS_MAX
  , PTHREAD_EXPLICIT_SCHED
  , PTHREAD_PROC
  , SCHED_RR
  , _SC_THREADS
  ;

TYPE
  X2C_Thread = pthread_t;

  X2C_Key = pthread_key_t;

  X2C_Mutex_rec = RECORD
    mutex : pthread_mutex_t;
    thread : pthread_t;
    count : SYSTEM.INT32;
  END;

  X2C_Mutex = POINTER TO X2C_Mutex_rec;

  X2C_BSemaphore_rec = RECORD
    state    : SYSTEM.CARD32;           -- State of the semaphore.
    lock     : pthread_mutex_t;  -- Serializes access to <state>
    signaled : pthread_cond_t;   -- Blocks IF state == 0 (nonsignaled)
  END;

  X2C_BSemaphore = POINTER TO X2C_BSemaphore_rec;

  X2C_Semaphore_rec = RECORD
    count          : SYSTEM.CARD32;           -- Current count of the semaphore.
    max            : SYSTEM.CARD32;           -- Maximum count of the semaphore.
    waiters        : SYSTEM.CARD32;           -- Number of threads that are waiting.  
    lock           : pthread_mutex_t;  -- Serializes access to <count> and <waiters>.
    count_nonzero  : pthread_cond_t;   -- Blocks when <count> == 0.
  END;

  X2C_Semaphore = POINTER TO X2C_Semaphore_rec;


CONST
  xosSCHED_POLICY = SCHED_RR;

VAR
  key : X2C_Key;
  thread_pids : ARRAY[0..PTHREAD_THREADS_MAX-1] OF pid_t;

-----------------------------------------------------------------------------
PROCEDURE thread_index(thread : pthread_t): pid_t;
BEGIN
  RETURN SYSTEM.CARD32(thread) MOD PTHREAD_THREADS_MAX;
END thread_index;
-----------------------------------------------------------------------------



PROCEDURE ["C"] X2C_INIT_THREADS(): SYSTEM.INT32;
VAR
  sch : sched_param;
  temp : SYSTEM.INT32;
  myuid : uid_t;

BEGIN
  myuid := geteuid();
                        
  IF sysconf(_SC_THREADS) = -1 THEN RETURN errno.ENOSYS END;
  
  IF myuid = 0 THEN -- I'm a superuser
    temp := pthread_getschedparam( pthread_self(), temp, sch );
    IF temp # 0 THEN RETURN temp END;
    sch.sched_priority := 1;
    temp := pthread_setschedparam( pthread_self(), xosSCHED_POLICY, sch );
    IF temp # 0 THEN RETURN temp END;
  END;

  (* Store the main thread's process id. *)
  thread_pids[thread_index(pthread_self())] := getpid(); 

  RETURN pthread_key_create(key, NIL);
END X2C_INIT_THREADS;


PROCEDURE ["C"] X2C_CurrentThread(): X2C_Thread;
BEGIN
  RETURN pthread_self();
END X2C_CurrentThread;


PROCEDURE ["C"] X2C_MyThreadHandle(VAR h: X2C_Thread): SYSTEM.INT32;
BEGIN
  h := pthread_self();
  RETURN 0;
END X2C_MyThreadHandle;


PROCEDURE ["C"] X2C_GetThreadWord(VAR w: SYSTEM.ADDRESS): SYSTEM.INT32;
BEGIN
  w := pthread_getspecific(key);
  RETURN 0;
END X2C_GetThreadWord;


PROCEDURE ["C"] X2C_SetThreadWord(w: SYSTEM.ADDRESS): SYSTEM.INT32;
BEGIN
  RETURN pthread_setspecific(key, w);
END X2C_SetThreadWord;


PROCEDURE ["C"] X2C_GetMyThreadId(VAR id: SYSTEM.CARD32);
BEGIN
  id := 0;
END X2C_GetMyThreadId;


PROCEDURE ["C"] X2C_ScheduleMyThread();
BEGIN
  pthread_yield();
END X2C_ScheduleMyThread;


TYPE
  argument = POINTER TO argument_STR;
  argument_STR = RECORD
    proc      : X2C_THREAD_PROC;
    arg       : SYSTEM.ADDRESS;
    c         : pthread_cond_t;
    m         : pthread_mutex_t;
  END;


(*
 * Internal thread initialization routine. It stores the thread's own
 * process_id in thread-specific buffer and then starts up a user start
 * routine.
 *)
PROCEDURE ["C"] init_thread(param: SYSTEM.ADDRESS);
VAR
  a : argument;
BEGIN
  a := param;

  (* Store the thread's process id. *)
  thread_pids[thread_index(pthread_self())] := getpid();

  (* We should wait while the parent doesn't wait on condition. *)
  pthread_mutex_lock(a^.m);

  (* Resume the parent. *)
  pthread_cond_signal(a^.c);
  pthread_mutex_unlock(a^.m);

  (* Wait for the parent, it should suspend me. *)
  pthread_mutex_lock(a^.m);

  (* Let's go on! *)
  a^.proc(a^.arg); 
  
  pthread_cond_destroy(a^.c);
  pthread_mutex_destroy(a^.m);
  free(a);
END init_thread;


PROCEDURE ["C"] X2C_CreateThread(VAR   t: X2C_Thread;
                                    proc: X2C_THREAD_PROC;
                                   stack: SYSTEM.CARD32;
                                   param: SYSTEM.ADDRESS;
                                    prio: SYSTEM.INT32
                                ) :SYSTEM.INT32;
VAR
  my_t  : pthread_t;
  attr  : pthread_attr_t;
  err   : SYSTEM.INT32;
  sch   : sched_param;
  myuid : uid_t;
  a     : argument;

BEGIN
  myuid := geteuid();
         
  pthread_attr_init ( attr );
  err := pthread_attr_setinheritsched( attr, PTHREAD_EXPLICIT_SCHED );
  IF err # 0 THEN RETURN err END;
  pthread_attr_setstacksize ( attr, stack );

  IF myuid = 0 THEN -- superuser
    -- SCHED_RR - round robin; for superuser only
    err := pthread_attr_setschedpolicy( attr, xosSCHED_POLICY );
    IF err # 0 THEN RETURN err END;
    
    -- get current scheduling parameters
    err := pthread_attr_getschedparam( attr, sch );
    IF err # 0 THEN RETURN err END;

    -- set priority
    IF prio < 1 THEN prio := 1  END;
    IF prio >99 THEN prio := 99 END;
    sch.sched_priority := prio;
    err := pthread_attr_setschedparam( attr, sch );
    IF err # 0 THEN RETURN err END;
  END;

  (* Initialize the thread's argument. *)
  a := malloc(SIZE(argument_STR));
  
  a^.proc := proc;
  a^.arg := param;
  
  pthread_cond_init(a^.c, NIL);
  pthread_mutex_init(a^.m, NIL);
  
  pthread_mutex_lock(a^.m);

  err := pthread_create ( my_t, attr, init_thread, a);

  pthread_cond_wait(a^.c, a^.m);
  
  (* Suspend the created thread. *)
  pthread_kill( my_t, signal.SIGSTOP );

  pthread_mutex_unlock(a^.m);  
  
  t := SYSTEM.CAST(X2C_Thread, my_t);
  pthread_attr_destroy( attr );
  RETURN err;
END X2C_CreateThread;


PROCEDURE ["C"] X2C_GetThreadPriority(t: X2C_Thread; VAR prio: SYSTEM.INT32): SYSTEM.INT32;
VAR
  p         : sched_param;
  temp, err : SYSTEM.INT32;
BEGIN         
  err := pthread_getschedparam( t, temp, p );   
  IF err # 0 THEN RETURN err END;
  prio := p.sched_priority;
  RETURN 0;
END X2C_GetThreadPriority;


PROCEDURE ["C"] X2C_SetThreadPriority(t: X2C_Thread; prio: SYSTEM.INT32): SYSTEM.INT32;
VAR
  sch  : sched_param;
  temp : SYSTEM.INT32;
BEGIN
  IF prio < 1  THEN prio := 1  END;
  IF prio > 99 THEN prio := 99 END;
  temp := pthread_getschedparam( t, temp, sch );
  IF temp # 0 THEN RETURN temp END;
  sch.sched_priority := prio;
  -- If the user doesn't have superusers permissions, it returns EPERM.
  RETURN pthread_setschedparam( t, xosSCHED_POLICY, sch );
END X2C_SetThreadPriority;


PROCEDURE ["C"] X2C_DeleteThread(VAR t: X2C_Thread): SYSTEM.INT32;
BEGIN
  RETURN pthread_cancel(t);
END X2C_DeleteThread; 


PROCEDURE ["C"] X2C_SuspendThread(t: X2C_Thread): SYSTEM.INT32;
BEGIN
  RETURN pthread_kill( t, signal.SIGSTOP );
END X2C_SuspendThread;


PROCEDURE ["C"] X2C_ResumeThread (t: X2C_Thread): SYSTEM.INT32;
BEGIN
  RETURN pthread_kill( t, signal.SIGCONT );
END X2C_ResumeThread;


PROCEDURE ["C"] X2C_ExitThread();
BEGIN
  pthread_exit(NIL);
END X2C_ExitThread;


PROCEDURE ["C"] X2C_CreateMutex(VAR m: X2C_Mutex): SYSTEM.INT32;
BEGIN
  m := malloc(SIZE(X2C_Mutex_rec));
  IF m = NIL THEN RETURN errno.ENOMEM END;
  pthread_mutex_init(m^.mutex, NIL);
  -- m^.thread is left uninitialized
  m^.count := 0;
  RETURN 0;
END X2C_CreateMutex;


PROCEDURE ["C"] X2C_DeleteMutex(VAR m: X2C_Mutex): SYSTEM.INT32;
VAR
  err : SYSTEM.INT32;
BEGIN
  IF m = NIL THEN RETURN errno.EINVAL END;
  err := pthread_mutex_destroy(m^.mutex);
  free(m);
  m := NIL;
  RETURN err;
END X2C_DeleteMutex;


PROCEDURE ["C"] X2C_EnterMutex(m: X2C_Mutex): SYSTEM.INT32;
VAR
  err  : SYSTEM.INT32;
  self : pthread_t;
BEGIN
  IF m = NIL THEN RETURN errno.EINVAL END;
  self := pthread_self();
  IF (m^.count > 0) AND (pthread_equal(self, m^.thread) # 0) THEN
    INC(m^.count);
    RETURN 0
  END;
  err := pthread_mutex_lock(m^.mutex);
  IF err # 0 THEN RETURN err END;
  m^.thread := self;
  INC(m^.count);
  RETURN 0;       
END X2C_EnterMutex;


PROCEDURE ["C"] X2C_ExitMutex(m: X2C_Mutex): SYSTEM.INT32;
VAR
  self : pthread_t;
BEGIN
  IF m = NIL THEN RETURN errno.EINVAL END;
  self := pthread_self();
  IF (m^.count > 0) AND (pthread_equal(self,m^.thread) # 0) THEN
    DEC(m^.count);
    IF m^.count # 0 THEN RETURN 0
    ELSE RETURN pthread_mutex_unlock(m^.mutex) END;
  ELSE
    RETURN errno.EPERM -- not owner or not locked
  END;
END X2C_ExitMutex;


PROCEDURE ["C"] X2C_CreateBoolSemaphore (VAR m: X2C_BSemaphore) :SYSTEM.INT32;
VAR
  s : X2C_BSemaphore;
BEGIN
  s := malloc(SIZE(X2C_BSemaphore_rec));
  IF s = NIL THEN RETURN errno.ENOMEM END;
  pthread_mutex_init (s^.lock, NIL);
  pthread_cond_init (s^.signaled, NIL);
  s^.state := 0;
  m := s;
  RETURN 0;       
END X2C_CreateBoolSemaphore;


PROCEDURE ["C"] X2C_DeleteBoolSemaphore (VAR m: X2C_BSemaphore) :SYSTEM.INT32;
VAR
  err : SYSTEM.INT32;
BEGIN
  IF m = NIL THEN RETURN errno.EINVAL END;
  pthread_mutex_destroy(m^.lock);
  err := pthread_cond_destroy(m^.signaled);
  IF err # 0 THEN RETURN err END;
  free(m);
  m := NIL;
  RETURN 0;       
END X2C_DeleteBoolSemaphore;


PROCEDURE ["C"] X2C_AcquireBoolSemaphore (m :X2C_BSemaphore) :SYSTEM.INT32;
VAR
  err : SYSTEM.INT32;
BEGIN
  IF m = NIL THEN RETURN errno.EINVAL END;
  err := pthread_mutex_lock (m^.lock);
  IF err # 0 THEN RETURN err END;
  WHILE m^.state = 0 DO pthread_cond_wait (m^.signaled, m^.lock); END;
  RETURN pthread_mutex_unlock (m^.lock);
END X2C_AcquireBoolSemaphore;


(* set in blocked state *) 
PROCEDURE ["C"] X2C_ResetBoolSemaphore (m: X2C_BSemaphore) :SYSTEM.INT32;
VAR
  err : SYSTEM.INT32;
BEGIN
  IF m = NIL THEN RETURN errno.EINVAL END;
  err := pthread_mutex_lock (m^.lock);
  IF err # 0 THEN RETURN err END;
  m^.state := 0;
  RETURN pthread_mutex_unlock (m^.lock);
END X2C_ResetBoolSemaphore;


(* set in signaled state *) 
PROCEDURE ["C"] X2C_SetBoolSemaphore (m: X2C_BSemaphore) :SYSTEM.INT32;
VAR
  err : SYSTEM.INT32;
BEGIN
  IF m = NIL THEN RETURN errno.EINVAL END;
  err := pthread_mutex_lock (m^.lock);
  IF err # 0 THEN RETURN err END;
  -- send notification to all waiters
  pthread_cond_broadcast (m^.signaled);
  m^.state := 1;
  RETURN pthread_mutex_unlock (m^.lock);
END X2C_SetBoolSemaphore;


PROCEDURE ["C"] X2C_CreateSemaphore (VAR m: X2C_Semaphore; init, max: SYSTEM.INT32): SYSTEM.INT32;
VAR        
  s : X2C_Semaphore;
BEGIN
  s := malloc(SIZE(X2C_Semaphore_rec));
  IF s = NIL THEN RETURN errno.ENOMEM END;
  pthread_mutex_init (s^.lock, NIL);
  pthread_cond_init (s^.count_nonzero, NIL);
  s^.count   := init;
  s^.max     := max;
  s^.waiters := 0;
  m := s;
  RETURN 0;       
END X2C_CreateSemaphore;


PROCEDURE ["C"] X2C_DeleteSemaphore(VAR m: X2C_Semaphore): SYSTEM.INT32;
VAR        
  err : SYSTEM.INT32;
  s : X2C_Semaphore;
BEGIN
  IF m = NIL THEN RETURN errno.EINVAL END;
  pthread_mutex_destroy(m^.lock);
  err := pthread_cond_destroy(m^.count_nonzero);
  IF err # 0 THEN RETURN err END;
  free(m);
  m := NIL;
  RETURN 0;
END X2C_DeleteSemaphore;


PROCEDURE ["C"] X2C_AcquireSemaphore(m: X2C_Semaphore; nowait: BOOLEAN; VAR awaited: BOOLEAN): SYSTEM.INT32;
VAR
  err : SYSTEM.INT32;
BEGIN
  IF m = NIL THEN RETURN errno.EINVAL END;
  err := pthread_mutex_lock (m^.lock);
  IF err # 0 THEN RETURN err END;
  IF nowait THEN
    awaited := (m^.count = 0);
    IF m^.count > 0 THEN DEC(m^.count) END;
  ELSE
    INC(m^.waiters);   -- Increase the number of waiters for Release to work correctly

    -- Wait until the semaphore count is > 0, then atomically release
    -- <lock> and wait for <count_nonzero> to be signaled.
    WHILE m^.count = 0 DO pthread_cond_wait (m^.count_nonzero, m^.lock) END;

    -- <m^.lock> is now held again
    DEC(m^.waiters);   -- Decrement the waiters count
    DEC(m^.count);     -- Decrement the semaphore's count
  END;
  RETURN pthread_mutex_unlock (m^.lock);
END X2C_AcquireSemaphore;


PROCEDURE ["C"] X2C_ReleaseSemaphore(m: X2C_Semaphore): SYSTEM.INT32;
VAR
  err : SYSTEM.INT32;
BEGIN
  IF m = NIL THEN RETURN errno.EINVAL END;
  err := pthread_mutex_lock (m^.lock);
  IF err # 0 THEN RETURN err END;

  -- Always allow one thread to continue IF it is waiting. */
  IF m^.waiters > 0 THEN pthread_cond_signal (m^.count_nonzero) END; 

  -- Increment the semaphore's count.
  IF m^.count < m^.max THEN INC(m^.count) END;
  RETURN pthread_mutex_unlock (m^.lock);
END X2C_ReleaseSemaphore;


PROCEDURE ["C"] X2C_SemaphoreToEvent(m: X2C_BSemaphore): X2C_EventSource;
BEGIN
  RETURN SYSTEM.CAST(X2C_EventSource, m);
END X2C_SemaphoreToEvent;


TYPE
  cardpair = RECORD
    adr : SYSTEM.ADDRESS;
    card : SYSTEM.CARD32;
  END;

  cardpair_ptr = POINTER TO cardpair;


PROCEDURE ["C"] WaitPOSIXEvents(p : SYSTEM.ADDRESS);
TYPE
  aX2C_EventSource = ARRAY OF X2C_EventSource;
  paX2C_EventSource = POINTER TO aX2C_EventSource;
VAR
  s     : paX2C_EventSource;  -- set of events
  no    : SYSTEM.CARD32;      -- events number
  i, m  : SYSTEM.CARD32;
  fds   : fd_set;
  pair  : cardpair; -- a pair of parameters: pointer to array of sources and a number
  err   : SYSTEM.INT32;
BEGIN
  pair := SYSTEM.CAST(cardpair_ptr, p)^;

  s := SYSTEM.CAST(paX2C_EventSource, pair.adr);
  no := pair.card;

  -- empty set
  FD_ZERO(fds);
  m := 0;

  -- indexing from 1
  FOR i := 1 TO no-1 DO
    FD_SET(s^[i], fds);
    -- find a maximum file descriptor in the array
    IF s^[i] > m THEN m := SYSTEM.CAST(SYSTEM.CARD32, s^[i]) END;
  END;
  
  -- Suppose that file desriptors are correct, i.e. the user
  -- got it with POSIXProcesses.MakeSource call, which
  -- checks its argument.

  -- If some events signalled, set process' control semaphore
  -- to resume execution.
  
  -- If result of this call is 0, it means that semaphore s[0]
  -- set by someone else, hence we should't do anything.

  IF select(m+1, SYSTEM.ADR(fds), NIL, NIL, NIL) # 0 THEN
    X2C_SetBoolSemaphore(SYSTEM.CAST(X2C_BSemaphore, s^[0]))
  END;
END WaitPOSIXEvents;


(*
 * Note: this procedure may be (and should be) called only from Processes.Wait
 * procedure. ProcessId.sources[0] event is always booolean semaphore, and
 * all next event sources in this array are file descriptors (in POSIX
 * implementation). So, we should use select() function in case if no > 1 only,
 * and give to it the array of event sources S without zeroth cell.
 * To synchronize waiting of that semaphore and all events (if they are
 * present), we create an extra thread that will wait for events. If semaphore
 * signalled, we emit an signal that kills the additional thread.
 *
 *)
PROCEDURE ["C"] X2C_WaitEvents(s: ARRAY OF X2C_EventSource; no: CARDINAL): SYSTEM.INT32;
VAR
  err : SYSTEM.INT32;
  events_waiter : pthread_t;
  pair : cardpair;
BEGIN
  IF no = 0 THEN RETURN errno.EINVAL END;
  
  IF no = 1 THEN RETURN X2C_AcquireBoolSemaphore(SYSTEM.CAST(X2C_BSemaphore, s[0]))
  ELSE
    pair.adr := SYSTEM.ADR(s);
    pair.card := no;
    err := pthread_create(events_waiter, NIL, WaitPOSIXEvents, SYSTEM.ADR(pair));
    IF err # 0 THEN RETURN err END;
    err := X2C_AcquireBoolSemaphore(SYSTEM.CAST(X2C_BSemaphore, s[0]));

    -- Now it is time to kill useless waiter thread. If it no
    -- more exist already, nothing happens (the next call will
    -- return ESRCH and we will ignore it).
    pthread_kill(events_waiter, signal.SIGCHLD);
    RETURN err;
  END;
END X2C_WaitEvents;


PROCEDURE ["C"] X2C_SyncThreadState(thr: X2C_Thread;
                            VAR stk_end: SYSTEM.ADDRESS;
                                reg_buf: SYSTEM.ADDRESS;
                               dmp_size: SYSTEM.CARD32;
                          VAR reg_dsize: SYSTEM.CARD32): SYSTEM.INT32;
VAR
  stacksize  : size_t;
  regs       : ptrace.regs;
  err        : SYSTEM.INT32;
  status     : SYSTEM.INT32;
  pid        : pid_t;
BEGIN
  IF dmp_size<28 THEN RETURN errno.ENOMEM; END;

  reg_dsize:=28;

  -- assume the thread is suspended
  pid := thread_pids[thread_index(pthread_t(thr))];

  err := ptrace.ptrace(ptrace.PTRACE_ATTACH, pid, NIL, NIL);
  IF err = -1 THEN RETURN get_errno(); END;

  err := waitpid(pid, status, __WALL);
  IF err = -1 THEN RETURN get_errno(); END;
                   
  err := ptrace.ptrace(ptrace.PTRACE_GETREGS, pid, NIL, SYSTEM.ADR(regs));
  IF err = -1 THEN RETURN get_errno(); END;

  err := ptrace.ptrace(ptrace.PTRACE_DETACH, pid, NIL, NIL);
  IF err = -1 THEN RETURN get_errno(); END;

  stk_end := SYSTEM.CAST(SYSTEM.ADDRESS, regs.esp);

  SYSTEM.PUT(reg_buf,regs.eax); reg_buf:=SYSTEM.ADDADR(reg_buf,4);
  SYSTEM.PUT(reg_buf,regs.ecx); reg_buf:=SYSTEM.ADDADR(reg_buf,4);
  SYSTEM.PUT(reg_buf,regs.edx); reg_buf:=SYSTEM.ADDADR(reg_buf,4);
  SYSTEM.PUT(reg_buf,regs.ebx); reg_buf:=SYSTEM.ADDADR(reg_buf,4);
  SYSTEM.PUT(reg_buf,regs.edi); reg_buf:=SYSTEM.ADDADR(reg_buf,4);
  SYSTEM.PUT(reg_buf,regs.esi); reg_buf:=SYSTEM.ADDADR(reg_buf,4);
  SYSTEM.PUT(reg_buf,regs.ebp);

  RETURN 0;
END X2C_SyncThreadState;



PROCEDURE ["C"] X2C_CreateKey(VAR k : X2C_Key): SYSTEM.INT32;
BEGIN
  RETURN pthread_key_create( k, NIL );
END X2C_CreateKey;


PROCEDURE ["C"] X2C_DeleteKey(k : X2C_Key): SYSTEM.INT32;
BEGIN
  RETURN pthread_key_delete( k );
END X2C_DeleteKey;


PROCEDURE ["C"] X2C_SetKeyValue(k : X2C_Key; value: SYSTEM.ADDRESS ): SYSTEM.INT32;
BEGIN
  RETURN pthread_setspecific( k, value );
END X2C_SetKeyValue;


PROCEDURE ["C"] X2C_GetKeyValue(k : X2C_Key; VAR value: SYSTEM.ADDRESS ): SYSTEM.INT32;
BEGIN
  value := pthread_getspecific( k );
  RETURN 0;
END X2C_GetKeyValue;


END xosThreads.
