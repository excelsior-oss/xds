<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE Processes;

IMPORT  SYSTEM, EXCEPTIONS;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

<* IF multithread THEN *>

IMPORT xrThreads, xosThreads;

VAR
  source: EXCEPTIONS.ExceptionSource;

TYPE ProcessId = xrThreads.X2C_Process;

PROCEDURE raise(x: ProcessesExceptions; msg-: ARRAY OF CHAR);
BEGIN
  EXCEPTIONS.RAISE(source,ORD(x),msg);
END raise;

PROCEDURE syserr(code: CARDINAL; pname: ARRAY OF CHAR);

  CONST msglen = 256;

  VAR msg: ARRAY [0..msglen-1] OF CHAR; pos: CARDINAL;

  PROCEDURE dectostr(VAR s: ARRAY OF CHAR; VAR pos: CARDINAL; no: CARDINAL);
    VAR l,i: CARDINAL;
  BEGIN
    i:=1000000000; l:=10;
    WHILE i>no DO i:=i DIV 10; DEC(l) END;
    IF l=0 THEN l:=1 END;
    pos:=pos+l;
    i:=pos;
    WHILE l>0 DO
      DEC(i);
      s[i]:=CHR(ORD("0")+(no MOD 10));
      no:=no DIV 10;
      DEC(l)
    END;
    ASSERT(no=0);
  END dectostr;

  PROCEDURE app(VAR s: ARRAY OF CHAR; VAR pos: CARDINAL; a-: ARRAY OF CHAR);
    VAR i: CARDINAL;
  BEGIN
    i:=0;
    WHILE (i<=HIGH(a)) & (a[i]#0C) & (pos<HIGH(s)) DO
      s[pos]:=a[i]; INC(pos); INC(i)
    END;
  END app;

BEGIN
  pos:=0;
  app(msg,pos,"Processes.");
  app(msg,pos,pname);
  app(msg,pos,": system error ");
  dectostr(msg,pos,code);
  IF pos<msglen THEN msg[pos]:=0C END;
  raise(processError,msg);
END syserr;


PROCEDURE Create (procBody: Body; space: CARDINAL; procUrg: Urgency;
                  procParams: Parameter; VAR procId: ProcessId);
VAR
  res: SYSTEM.INT32;
BEGIN
  res := xrThreads.X2C_CreateISOProcess(procId,
                                        SYSTEM.CAST(xrThreads.Body,procBody),
                                        space, procParams, procUrg);
  IF res <> 0 THEN syserr(res,"Create"); END;
END Create;

PROCEDURE Start (procBody: Body; extraSpace: CARDINAL; procUrg: Urgency;
                 procParams: Parameter; VAR procId: ProcessId);
BEGIN
  Create(procBody,extraSpace,procUrg,procParams,procId);
  Activate(procId);
END Start;

PROCEDURE StopMe;
  (* Terminates the calling process.
     The process must not be associated with a source of events.
  *)
VAR
  res: SYSTEM.INT32;
BEGIN
  res := xrThreads.X2C_DeleteISOProcess(Me());
  IF res <> 0 THEN syserr(res,"StopMe"); END;
  xosThreads.X2C_ExitThread;
END StopMe;

PROCEDURE SuspendMe;
VAR
  p: ProcessId;
  res: SYSTEM.INT32;
BEGIN
  p := Me();
  xosThreads.X2C_ResetBoolSemaphore (p^.controll);
  res:=xosThreads.X2C_AcquireBoolSemaphore (p^.controll);
  IF res <> 0 THEN syserr(res,"SuspendMe"); END;
END SuspendMe;

PROCEDURE Activate(p: ProcessId);
VAR
  res: SYSTEM.INT32;
BEGIN
  res := xosThreads.X2C_SetBoolSemaphore(p^.controll);
  IF res <> 0 THEN syserr(res,"Activate"); END;
END Activate;

PROCEDURE SuspendMeAndActivate(p: ProcessId);
BEGIN
  IF p = Me() THEN
    xosThreads.X2C_ScheduleMyThread();
  ELSE
    Activate(p);   (* !!! non-atomic *)
    SuspendMe;
  END;
END SuspendMeAndActivate;

PROCEDURE Me(): ProcessId;
VAR
  a: SYSTEM.ADDRESS;
  res: SYSTEM.INT32;
BEGIN
  res := xosThreads.X2C_GetThreadWord(a);
  IF res <> 0 THEN syserr(res,"Me") END;
  RETURN a;
END Me;

PROCEDURE Switch(p: ProcessId; VAR info: Parameter);
BEGIN
  p^.info := info;
  Activate(p);
  SuspendMe();
  p := Me();
  info := p^.info;
END Switch;

PROCEDURE Wait;
  (* Causes the calling process to enter the waiting state.  The procedure will return when
     the calling process is activated by another process, or when one of its associated
     eventSources has generated an event.
  *)
VAR
  p: ProcessId;
  res: SYSTEM.INT32;
BEGIN
  p := Me();
  res := xosThreads.X2C_WaitEvents(p^.sources,p^.scount);
  IF res <> 0 THEN syserr(res,"Wait") END;
END Wait;

PROCEDURE Attach(e: Sources);
VAR
  p: ProcessId;
BEGIN
  p := Me();
  IF p^.scount <= xrThreads.X2C_MaxSources THEN
    p^.sources[p^.scount] := e;
    INC(p^.scount);
  END;
END Attach;

PROCEDURE Detach(e: Sources);
VAR
  i: CARDINAL;
  p: ProcessId;
BEGIN
  p := Me();
  i := 1;
  WHILE (i < p^.scount) & (p^.sources[i] # e) DO INC(i) END;
  IF i < p^.scount THEN
    INC(i);
    WHILE (i < p^.scount) DO p^.sources[i-1] := p^.sources[i]; INC(i) END;
    DEC(p^.scount);
  END;
END Detach;

PROCEDURE IsAttached(e: Sources): BOOLEAN;
VAR
  i: CARDINAL;
  p: ProcessId;
BEGIN  
  p := Me();
  i := 1;
  WHILE (i < p^.scount) & (p^.sources[i] <> e) DO INC(i) END;
  RETURN (i < p^.scount);
END IsAttached;

PROCEDURE Handler(e: Sources): ProcessId;
VAR
  p,t: ProcessId;
  i: CARDINAL;
BEGIN
  xrThreads.X2C_StartIterateThreads;
  p := Me();
  t := p;
  REPEAT
    i := 1;
    WHILE (i < t^.scount) & (t^.sources[i] <> e) DO INC(i) END;
    IF i < t^.scount THEN
      xrThreads.X2C_StopIterateThreads;
      RETURN t;
    END;
    t := t^.fwd;
  UNTIL t = p;
  xrThreads.X2C_StopIterateThreads;
  RETURN NIL;
END Handler;

PROCEDURE MyParam(): Parameter;
VAR
  p: ProcessId;
BEGIN
  p := Me();
  RETURN p^.param;
END MyParam;

PROCEDURE UrgencyOf (procId: ProcessId): Urgency;
BEGIN
  RETURN procId^.urgency;
END UrgencyOf;

PROCEDURE init;
BEGIN
END init;

<* ELSE *>

IMPORT prs:=XPrs, cor:=COROUTINES, rts:=xmRTS;

TYPE
  ProcessId = prs.Process;

VAR
  source: EXCEPTIONS.ExceptionSource;
  events: ARRAY [0..31] OF ProcessId;
  hcnt  : CARDINAL; (* count of waiting processes *)
  corpse: ProcessId;

PROCEDURE GoNext;
  VAR c: cor.COROUTINE;
BEGIN
  WHILE prs.current=NIL DO
    cor.LISTEN(INTERRUPTIBLE);
    cor.LISTEN(UNINTERRUPTIBLE);
  END;
  ASSERT(prs.current^.state=prs.st_ready);
  ASSERT(prs.current^.magic=prs.MAGIC);
  cor.TRANSFER(c,prs.current^.body);
  IF corpse=NIL THEN RETURN END;
  IF corpse^.wsp#NIL THEN DEALLOCATE(corpse^.wsp,corpse^.wsp_size) END;
  corpse^.magic:=0;
  DISPOSE(corpse);
  ASSERT(corpse=NIL);
END GoNext;

PROCEDURE Tie(p: ProcessId);
  VAR min,max,l: ProcessId;
BEGIN
  ASSERT(p^.magic=prs.MAGIC);
  p^.state:=prs.st_ready;
  p^.info:=NIL;
  IF prs.current=NIL THEN
    prs.current:=p;
    p^.fwd:=p;
    p^.bck:=p;
  ELSE
    prs.current^.state:=prs.st_ready;
    prs.current^.body:=cor.CURRENT();
    IF p^.urgency=prs.current^.urgency THEN
      l:=prs.current;
    ELSE
      max:=prs.current; min:=max;
      WHILE (min^.bck#max) & (min^.bck^.urgency<=min^.urgency) DO
        min:=min^.bck;
      END;
      max:=min^.bck;
      ASSERT(max^.urgency>=min^.urgency);
      ASSERT(max^.urgency=prs.current^.urgency);
      IF (p^.urgency>=max^.urgency) OR (p^.urgency<=min^.urgency) THEN
        l:=min;
      ELSE
        l:=min;
        WHILE l^.urgency<p^.urgency DO l:=l^.fwd END;
      END;
    END;
    p^.fwd:=l;
    p^.bck:=l^.bck;
    p^.fwd^.bck:=p;
    p^.bck^.fwd:=p;
    IF prs.current^.urgency<p^.urgency THEN prs.current:=p END;
  END;
END Tie;

PROCEDURE UnTie;
  VAR p: ProcessId;
BEGIN
  p:=prs.current;
  ASSERT(p^.magic=prs.MAGIC);
  prs.current:=p^.fwd;
  IF p=prs.current THEN
    prs.current:=NIL;
  ELSE
    p^.fwd^.bck:=p^.bck;
    p^.bck^.fwd:=p^.fwd;
  END;
  p^.fwd:=NIL;
  p^.bck:=NIL;
  p^.body:=cor.CURRENT();
  p^.state:=prs.st_passive;
END UnTie;

PROCEDURE execBody;
  VAR p: ProcessId;
BEGIN
  p:=prs.current;
  p^.execBody;
  StopMe;
END execBody;

PROCEDURE Create (procBody: Body; extraSpace: CARDINAL; procUrg: Urgency;
                  procParams: Parameter; VAR procId: ProcessId);
(* Creates a new process with procBody as its body, and with urgency and parameters
   given by procUrg and procParams.  At least as much workspace (in units of
   SYSTEM.LOC) as is specified by extraSpace is allocated to the process.
   An identity for the new process is returned in procId.
   The process is created in the passive state; it will not run until activated.
*)
  VAR p: ProcessId; wsp: SYSTEM.ADDRESS; size: CARDINAL;

  PROCEDURE cre;
  BEGIN
    IF (p=NIL) OR (wsp=NIL) THEN
      EXCEPTIONS.RAISE(source,ORD(processError),"No memory for process stack");
    END;
    cor.NEWCOROUTINE(execBody,wsp,size,p^.body);
    p^.magic:=prs.MAGIC;
    p^.bck:=NIL;
    p^.fwd:=NIL;
    p^.param:=procParams;
    p^.info:=NIL;
    p^.urgency:=procUrg;
    p^.wsp:=wsp;
    p^.wsp_size:=size;
    p^.state:=prs.st_passive;
    p^.execBody:=procBody;
    procId:=p;
  EXCEPT
    IF p#NIL THEN DISPOSE(p) END;
    IF wsp#NIL THEN DEALLOCATE(wsp,size) END;
    procId:=NIL;
  END cre;

BEGIN
  p:=NIL;
  wsp:=NIL;
  procId:=NIL;
  size:=extraSpace+1024+SIZE(rts.X2C_Coroutine_STR);
  (* ^-  this must be done before exception trap will be installed! *)
  NEW(p);
  ALLOCATE(wsp,size);
  cre;
END Create;

PROCEDURE Start (procBody: Body; extraSpace: CARDINAL; procUrg: Urgency;
                 procParams: Parameter; VAR procId: ProcessId);
  (* Creates a new process, with parameters as for Create.
     The process is created in the ready state; it is eligible to run immediately.
  *)
BEGIN
  Create(procBody,extraSpace,procUrg,procParams,procId);
  Activate(procId);
END Start;

PROCEDURE StopMe;
  (* Terminates the calling process.
     The process must not be associated with a source of events.
  *)
  VAR p: ProcessId;
BEGIN
  cor.LISTEN(UNINTERRUPTIBLE);
  p:=prs.current;
  UnTie;
  IF prs.current=NIL THEN
    Tie(p);
    cor.LISTEN(INTERRUPTIBLE);
    HALT(0);
  END;
  corpse:=p;
  GoNext;
END StopMe;

PROCEDURE SuspendMe[UNINTERRUPTIBLE];
  (* Causes the calling process to enter the passive state.  The procedure only returns
     when the calling process is again activated by another process.
  *)
  VAR p: ProcessId;
BEGIN
  p:=prs.current;
  UnTie;
  IF (prs.current=NIL) & (hcnt=0) THEN
    Tie(p);
    EXCEPTIONS.RAISE(source,ORD(passiveProgram),
    	"Processes.SuspendMe: Passive program");
  END;
  GoNext;
END SuspendMe;

PROCEDURE Activate[UNINTERRUPTIBLE](p: ProcessId);
  (* Causes the process identified by procId to enter the ready state, and thus to become
     eligible to run again.
  *)
BEGIN
  IF (p=NIL) OR (p^.magic#prs.MAGIC) THEN
    EXCEPTIONS.RAISE(source,ORD(processError),
    	"Processes.Activate: Invalid argument");
  END;
  IF (p^.state#prs.st_passive) & (p^.state#prs.st_waiting) THEN RETURN END;
  ASSERT(prs.current#NIL);
  Tie(p);
  GoNext;
END Activate;

PROCEDURE SuspendMeAndActivate[UNINTERRUPTIBLE](p: ProcessId);
  (* Executes an atomic sequence of SuspendMe() and Activate(procId). *)
BEGIN
  IF (p=NIL) OR (p^.magic#prs.MAGIC) THEN
    EXCEPTIONS.RAISE(source,ORD(processError),
    	"Processes.SuspendMeAndActivate: Invalid argument");
  END;
  IF (p^.state#prs.st_passive) &
     (p^.state#prs.st_waiting) &
     (p#prs.current)
  THEN RETURN;
  END;
  UnTie;
  Tie(p);
  GoNext;
END SuspendMeAndActivate;

PROCEDURE Switch[UNINTERRUPTIBLE](p: ProcessId; VAR info: Parameter);
  (* Causes the calling process to enter the passive state; the process identified by procId
     becomes the currently executing process.
     info is used to pass parameter information from the calling to the activated process.
     On return, info will contain information from the process that chooses to switch back to
     this one (or will be NIL if Activate or SuspendMeAndActivate are used instead of
     Switch).
  *)
BEGIN
  IF (p=NIL) OR
     (p^.magic#prs.MAGIC) OR
     (p^.state=prs.st_ready)
  THEN
    EXCEPTIONS.RAISE(source,ORD(processError),
        "Processes.Switch: Invalid argument");
  END;
  IF prs.current^.urgency<=p^.urgency THEN UnTie END;
  Tie(p);
  p^.info:=info;
  GoNext;
  info:=prs.current^.info;
END Switch;

PROCEDURE Wait[UNINTERRUPTIBLE];
  (* Causes the calling process to enter the waiting state.  The procedure will return when
     the calling process is activated by another process, or when one of its associated
     eventSources has generated an event.
  *)
  VAR p: ProcessId;
BEGIN
  p:=prs.current;
  UnTie;
  p^.state:=prs.st_waiting;
  GoNext;
END Wait;

PROCEDURE Attach[UNINTERRUPTIBLE](e: Sources);
  (* Associates the specified eventSource with the calling process. *)
BEGIN
  IF e<=HIGH(events) THEN
    IF events[e]=NIL THEN INC(hcnt) END;
    events[e]:=prs.current;
  END;
END Attach;

PROCEDURE Detach[UNINTERRUPTIBLE](e: Sources);
  (* Dissociates the specified eventSource from the program. *)
BEGIN
  IF e<=HIGH(events) THEN
    IF events[e]#NIL THEN DEC(hcnt) END;
    events[e]:=NIL;
  END;
END Detach;

PROCEDURE IsAttached(e: Sources): BOOLEAN;
  (* Returns TRUE if and only if the specified eventSource is currently associated with
     one of the processes of the program.
  *)
BEGIN
  RETURN (e<=HIGH(events)) & (events[e]#NIL);
END IsAttached;

PROCEDURE Handler(e: Sources): ProcessId;
  (* Returns the identity of the process, if any, that is associated with the specified
     eventSource.
  *)
BEGIN
  IF e>HIGH(events) THEN RETURN NIL END;
  RETURN events[e];
END Handler;

PROCEDURE Me(): ProcessId;
  (* Returns the identity of the calling process (as assigned when the process was first
     created).
  *)
BEGIN
  RETURN prs.current;
END Me;

PROCEDURE MyParam(): Parameter;
  (* Returns the value specified as procParams when the calling process was created. *)
BEGIN
  RETURN prs.current^.param;
END MyParam;

PROCEDURE UrgencyOf (procId: ProcessId): Urgency;
  (* Returns the urgency established when the process identified by procId was first
     created.
  *)
BEGIN
  RETURN procId^.urgency;
END UrgencyOf;

PROCEDURE init;
  VAR i: INTEGER;
BEGIN
  hcnt:=0;
  corpse:=NIL;
  FOR i:=0 TO HIGH(events) DO events[i]:=NIL END;
  NEW(prs.current);
  IF prs.current=NIL THEN
    EXCEPTIONS.RAISE(source,ORD(processError),
        'No memory for initialization of "Processes"');
  END;
  prs.current^.magic:=prs.MAGIC;
  prs.current^.fwd:=prs.current;
  prs.current^.bck:=prs.current;
  prs.current^.param:=NIL;
  prs.current^.info:=NIL;
  prs.current^.body:=cor.CURRENT();
  prs.current^.urgency:=0;
  prs.current^.wsp:=NIL;
  prs.current^.wsp_size:=0;
  prs.current^.state:=prs.st_ready;
END init;

<* END *>

PROCEDURE ProcessesException(): ProcessesExceptions;
  (* If the current coroutine is in the exceptional execution state because of the raising
     of a language exception, returns the corresponding enumeration value, and
     otherwise raises an exception.
  *)
BEGIN
  RETURN VAL(ProcessesExceptions,EXCEPTIONS.CurrentNumber(source))
END ProcessesException;

PROCEDURE IsProcessesException(): BOOLEAN;
  (* Returns TRUE if the current coroutine is in the exceptional execution state
     because of the raising of an exception in a routine from this module; otherwise
     returns FALSE.
  *)
BEGIN
  RETURN EXCEPTIONS.IsCurrentSource(source)
END IsProcessesException;


BEGIN
  EXCEPTIONS.AllocateSource(source);
  init;
END Processes.

