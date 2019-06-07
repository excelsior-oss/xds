<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE Semaphores;

IMPORT SYSTEM;
FROM EXCEPTIONS IMPORT ExceptionSource, AllocateSource, IsCurrentSource, RAISE;
FROM Storage    IMPORT ALLOCATE, DEALLOCATE;
<* IF multithread THEN *>
IMPORT  xosThreads;
<* ELSE *>
FROM Processes  IMPORT ProcessId, Activate, Me, SuspendMe;
<* END *>

CONST
  magic_s = 856FH;
  magic_i = 1094H;

VAR
  source: ExceptionSource;

<* IF multithread THEN *>

TYPE
  SEMAPHORE = POINTER TO SemRec;

  SemRec = RECORD
    magic : CARDINAL;
    handle: xosThreads.X2C_Semaphore;
  END;

PROCEDURE Create(VAR s: SEMAPHORE; i: CARDINAL);
  VAR x: SEMAPHORE;
    res: SYSTEM.INT32;
BEGIN
  IF i>VAL(CARDINAL,MAX(INTEGER)) THEN
    RAISE(source,0,"Semphores.Create: bad initial count");
  END;
  NEW(x);
  IF x=NIL THEN
    RAISE(source,0,"Semphores.Create: no memory for semaphore");
  END;
  res:=xosThreads.X2C_CreateSemaphore(x^.handle,VAL(SYSTEM.INT32,i),MAX(SYSTEM.INT32));
  IF res#0 THEN
    DISPOSE(x);
    RAISE(source,SYSTEM.CAST(CARDINAL,res),"Semphores.Create: system error");
  END;
  x^.magic:=magic_s;
  s:=x;
END Create;

PROCEDURE Destroy(VAR s: SEMAPHORE);
  VAR res: SYSTEM.INT32;
BEGIN
  IF (s=NIL) OR (s^.magic#magic_s) THEN
    RAISE(source,1,"Semphores.Destroy: invalid argument");
  END;
  res:=xosThreads.X2C_DeleteSemaphore(s^.handle);
  IF res#0 THEN
    RAISE(source,SYSTEM.CAST(CARDINAL,res),"Semphores.Destroy: system error");
  END;
  s^.magic:=0;
  DISPOSE(s);
  s:=NIL;
END Destroy;

PROCEDURE Claim(s: SEMAPHORE);
  VAR res: SYSTEM.INT32; awa: BOOLEAN;
BEGIN
  IF (s=NIL) OR (s^.magic#magic_s) THEN
    RAISE(source,2,"Semphores.Claim: invalid argument");
  END;
  res:=xosThreads.X2C_AcquireSemaphore(s^.handle,FALSE,awa);
  IF res#0 THEN
    RAISE(source,SYSTEM.CAST(CARDINAL,res),"Semphores.Claim: system error");
  END;
END Claim;

PROCEDURE Release(s: SEMAPHORE);
  VAR res: SYSTEM.INT32;
BEGIN
  IF (s=NIL) OR (s^.magic#magic_s) THEN
    RAISE(source,3,"Semphores.Release: invalid argument");
  END;
  res:=xosThreads.X2C_ReleaseSemaphore(s^.handle);
  IF (res#0) THEN
    RAISE(source,SYSTEM.CAST(CARDINAL,res),"Semphores.Release: system error");
  END;
END Release;

PROCEDURE CondClaim(s: SEMAPHORE): BOOLEAN;
  VAR res: SYSTEM.INT32; awa: BOOLEAN;
BEGIN
  IF (s=NIL) OR (s^.magic#magic_s) THEN
    RAISE(source,4,"Semphores.CondClaim: invalid argument");
  END;
  res:=xosThreads.X2C_AcquireSemaphore(s^.handle,TRUE,awa);
  IF res#0 THEN
    RAISE(source,SYSTEM.CAST(CARDINAL,res),"Semphores.Claim: system error");
  END;
  RETURN NOT awa;
END CondClaim;

<* ELSE *>

TYPE
  SEMAPHORE = POINTER TO SemRec;
  Item = POINTER TO ItemRec;

  SemRec = RECORD
    magic: CARDINAL;
    cnt  : CARDINAL;
    lst  : Item;
  END;

  ItemRec = RECORD
    magic: CARDINAL;
    prs  : ProcessId;
    fwd  : Item;
    bck  : Item;
  END;

PROCEDURE Tie(VAR list: Item; i: Item);
BEGIN
  IF list=NIL THEN
    list:=i; i^.fwd:=i; i^.bck:=i;
  ELSE
    i^.fwd:=list;
    i^.bck:=list^.bck;
    i^.fwd^.bck:=i;
    i^.bck^.fwd:=i;
  END;
END Tie;

PROCEDURE UnTie(VAR list: Item; i: Item);
BEGIN
  IF i^.fwd=i THEN
    ASSERT(list=i);
    list:=NIL;
  ELSE
    IF list=i THEN list:=i^.fwd END;
    i^.fwd^.bck:=i^.bck;
    i^.bck^.fwd:=i^.fwd;
    i^.fwd:=i;
    i^.bck:=i;
  END;
END UnTie;

PROCEDURE Create(VAR s: SEMAPHORE; i: CARDINAL);
BEGIN
  NEW(s);
  IF s=NIL THEN
    RAISE(source,0,"Semphores.Create: no memory for semaphore");
  END;
  s^.magic:=magic_s;
  s^.cnt:=i;
  s^.lst:=NIL;
END Create;

PROCEDURE Destroy[UNINTERRUPTIBLE](VAR s: SEMAPHORE);
  VAR i: Item;
BEGIN
  IF (s=NIL) OR (s^.magic#magic_s) THEN
    RAISE(source,1,"Semphores.Destroy: invalid argument");
  END;
  WHILE s^.lst#NIL DO
    ASSERT(s^.cnt=0);
    i:=s^.lst;
    UnTie(s^.lst,i);
    ASSERT(i^.magic=magic_i);
    i^.magic:=0;
    Activate(i^.prs);
  END;
  s^.magic:=0;
  DISPOSE(s);
END Destroy;

PROCEDURE Claim[UNINTERRUPTIBLE](s: SEMAPHORE);
  VAR i: ItemRec;
BEGIN
  IF (s=NIL) OR (s^.magic#magic_s) THEN
    RAISE(source,2,"Semphores.Claim: invalid argument");
  END;
  IF s^.cnt>0 THEN DEC(s^.cnt); RETURN END;
  i.magic:=magic_i;
  i.prs:=Me();
  Tie(s^.lst,SYSTEM.ADR(i));
  SuspendMe;
END Claim;

PROCEDURE Release[UNINTERRUPTIBLE](s: SEMAPHORE);
  VAR i: Item;
BEGIN
  IF (s=NIL) OR (s^.magic#magic_s) THEN
    RAISE(source,3,"Semphores.Release: invalid argument");
  END;
  IF s^.lst=NIL THEN INC(s^.cnt); RETURN END;
  i:=s^.lst;
  UnTie(s^.lst,i);
  ASSERT(i^.magic=magic_i);
  i^.magic:=0;
  Activate(i^.prs);
END Release;

PROCEDURE CondClaim[UNINTERRUPTIBLE](s: SEMAPHORE): BOOLEAN;
BEGIN
  IF (s=NIL) OR (s^.magic#magic_s) THEN
    RAISE(source,4,"Semphores.CondClaim: invalid argument");
  END;
  IF s^.cnt>0 THEN DEC(s^.cnt); RETURN TRUE END;
  RETURN FALSE;
END CondClaim;

<* END *>

PROCEDURE IsSemaphoreException(): BOOLEAN;
BEGIN
  RETURN IsCurrentSource(source)
END IsSemaphoreException;

BEGIN
  AllocateSource(source);
END Semaphores.

