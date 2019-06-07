(* Copyright (c) xTech 1993-96. All Rights Reserved. *)
<* +m2extensions *>
IMPLEMENTATION MODULE TermFile; (* Hady. 04.08.96 10:21 *)

IMPORT  xmRTS, xlibOS, xrtsOS,
        ChanConsts, IOConsts,
        IOLink, IOChan,
        SYSTEM, platform;

CONST
  LF = 12C;
  SEP0 = platform.SEP0;
  SEP1 = platform.SEP1;

TYPE
  INT = SYSTEM.INT32;
  Object = IOLink.DeviceTablePtr;

CONST
  BUF_HIGH = 255;
  BUF_LOW  =  -8;

  echoFlag = ChanConsts.echoFlag;
  textFlag = ChanConsts.textFlag;

VAR
  buf : ARRAY [BUF_LOW..BUF_HIGH] OF CHAR;
  blen: INTEGER; (* last valid position in buffer *)
  bpos: INTEGER; (* current char in the buffer    *)
  epos: INTEGER; (* last echoed symbol position in the buffer *)

  flin: BOOLEAN; (* TRUE if last line was readen in line mode and
                   end-of-line has not been detected yet *)

  init: BOOLEAN; (* hardware init flag *)

  <* IF multithread THEN *>
   mtx: xmRTS.X2C_MUTEX;
  <* END *>

PROCEDURE lock;
BEGIN
  <* IF multithread THEN *> xmRTS.X2C_LockMutex(mtx); <* END *>
END lock;

PROCEDURE unlock;
BEGIN
  <* IF multithread THEN *> xmRTS.X2C_FreeMutex(mtx); <* END *>
END unlock;

PROCEDURE shiftBuf;
  VAR i,j: INTEGER;
BEGIN
  i:=blen; j:=-1;
  WHILE (i>=0) & (j>=BUF_LOW) DO
    buf[j]:=buf[i]; DEC(i); DEC(j);
  END;
  bpos:=0; epos:=-1;
END shiftBuf;

PROCEDURE next(echo: BOOLEAN): INT;
  VAR i,res: INT; rd: SYSTEM.CARD32;
BEGIN
  IF bpos>=blen THEN
    shiftBuf;
    IF ~ echo OR flin THEN
      res:=xlibOS.X2C_ttyReadLE(SYSTEM.ADR(buf[0]),BUF_HIGH,rd);
    ELSE
      res:=xlibOS.X2C_ttyReadNE(SYSTEM.ADR(buf[0]),BUF_HIGH,rd);
    END;
    IF res#0 THEN RETURN res END;
    blen:=VAL(INT,rd)-1;
    IF ~ echo THEN
      flin:=TRUE; epos:=blen;
    END;
  ELSE;
    INC(bpos);
  END;
  RETURN 0;
END next;

PROCEDURE getc(x: Object; VAR ch: CHAR): INT;
  VAR res: INT;
BEGIN
  res:=next(echoFlag IN x^.flags);
  IF res#0 THEN RETURN res END;
  ch:=buf[bpos];
  IF SEP1=0C THEN
    IF (SEP0#LF) & (textFlag IN x^.flags) & (ch=SEP0) THEN ch:=LF END;
  ELSE
    IF (textFlag IN x^.flags) & (ch=SEP0) THEN
      res:=next(echoFlag IN x^.flags);
      IF res#0 THEN RETURN res END;
      IF buf[bpos]=SEP1 THEN
        ch:=LF
      ELSE DEC(bpos)
      END;
    END;
  END;
  RETURN 0;
END getc;

PROCEDURE ungetc(x: Object; ch: CHAR);
BEGIN
  IF ((SEP1#0C) OR (SEP0#LF)) & (textFlag IN x^.flags) & (ch=LF) THEN
    IF SEP1=0C THEN ch:=SEP0
    ELSE
      buf[bpos]:=SEP1; DEC(bpos); ch:=SEP0;
    END;
  END;
  buf[bpos]:=ch; DEC(bpos);
END ungetc;

PROCEDURE doecho(do: BOOLEAN): INT;
  VAR ch: CHAR; res: INT;
BEGIN
  WHILE (epos<bpos) DO
    INC(epos);
    IF do THEN
      ch:=buf[epos];
      res:=xlibOS.X2C_ttyWrite(SYSTEM.ADR(ch),1);
      IF res#0 THEN RETURN res END;
    END;
  END;
  RETURN 0;
END doecho;

PROCEDURE HardError(x: Object; res: INT);
  VAR num: ARRAY [0..11] OF CHAR; pos: SYSTEM.CARD32;
BEGIN
  pos:=0;
  xrtsOS.X2C_DecToStr(num,pos,res); num[pos]:=0C;
  IOLink.RAISEdevException(x^.cid,x^.did,IOChan.hardDeviceError,num);
END HardError;

PROCEDURE SoftError(x: Object; res: INT);
  VAR num: ARRAY [0..11] OF CHAR; pos: SYSTEM.CARD32;
BEGIN
  pos:=0;
  xrtsOS.X2C_DecToStr(num,pos,res); num[pos]:=0C;
  IOLink.RAISEdevException(x^.cid,x^.did,IOChan.softDeviceError,num);
END SoftError;

PROCEDURE DoWrite(x: Object; a: SYSTEM.ADDRESS; n: CARDINAL);
  VAR res: INT;
      s: ARRAY [0..BUF_HIGH] OF CHAR;
      i: CARDINAL;
      ch: CHAR;

  PROCEDURE flush;
  BEGIN
    res:=xlibOS.X2C_ttyWrite(SYSTEM.ADR(s),i);
  END flush;

  PROCEDURE put(ch: CHAR);
  BEGIN
    IF i>BUF_HIGH THEN flush; END;
    s[i]:=ch; INC(i);
  END put;

BEGIN
  IF n=0 THEN RETURN END;
  IF ((SEP0#LF) OR (SEP1#0C)) & (textFlag IN x^.flags) THEN
    i:=0; res:=0;
    REPEAT
      SYSTEM.GET(a,ch); a:=SYSTEM.ADDADR(a,SIZE(CHAR)); DEC(n);
      IF ch=LF THEN
        IF SEP1=0C THEN ch:=SEP0
        ELSE
          put(SEP0); ch:=SEP1;
        END;
      END;
      put(ch);
    UNTIL (n=0) OR (res#0);
    IF (res=0) & (i>0) THEN flush END;
  ELSE
    res:=xlibOS.X2C_ttyWrite(a,n);
  END;
  IF res#0 THEN HardError(x,res) END;
END DoWrite;

PROCEDURE WriteLn(x: Object);
  VAR ch: CHAR;
BEGIN
  ch:=LF;
  DoWrite(x,SYSTEM.ADR(ch),1);
END WriteLn;

PROCEDURE Look(x: Object; VAR ch: CHAR; VAR res: IOConsts.ReadResults);
  VAR c: CARDINAL; r: INT; cc: CHAR;
BEGIN
  ASSERT(textFlag IN x^.flags);
  lock();
    r:=getc(x,cc);
    IF r=0 THEN
      IF cc=LF THEN
        res:=IOConsts.endOfLine;
      ELSE
        ch:=cc;
        res:=IOConsts.allRight;
      END;
      ungetc(x,cc);
    ELSE
      res:=IOConsts.endOfInput;
    END;
  unlock();
  x^.result:=res;
END Look;

PROCEDURE Skip(x: Object);
  VAR c: CARDINAL; res: INTEGER; ch: CHAR;
BEGIN
  ASSERT(textFlag IN x^.flags);
  lock();
    res:=getc(x,ch);
    IF res#0 THEN
      unlock();
      SoftError(x,res);
    END;
    IF ch=LF THEN flin:=FALSE END;
    res:=doecho(FALSE);
  unlock();
  IF res#0 THEN SoftError(x,res) END;
  x^.result:=IOConsts.allRight;
END Skip;

PROCEDURE SkipLook(x: Object; VAR ch: CHAR; VAR res: IOConsts.ReadResults);
BEGIN
  Skip(x); Look(x,ch,res);
END SkipLook;

PROCEDURE TextRead(x: Object; a: SYSTEM.ADDRESS; n: CARDINAL; VAR locs: CARDINAL);
  VAR c: CARDINAL;
    res: INTEGER;
     ch: CHAR;
BEGIN
  ASSERT(textFlag IN x^.flags);
  lock();
    locs:=0;
    WHILE locs<n DO
      res:=getc(x,ch);
      IF res#0 THEN
        unlock();
        SoftError(x,res);
      END;
      IF ch=LF THEN
        ungetc(x,ch);
        IF locs>0 THEN x^.result:=IOConsts.allRight;
        ELSE           x^.result:=IOConsts.endOfLine;
        END;
        unlock(); RETURN
      END;
      res:=doecho(TRUE);
      IF res#0 THEN
        unlock();
        SoftError(x,res);
      END;
      SYSTEM.PUT(a,ch); a:=SYSTEM.ADDADR(a,SIZE(CHAR)); INC(locs);
    END;
  unlock();
  x^.result:=IOConsts.allRight;
END TextRead;

PROCEDURE RawRead(x: Object; a: SYSTEM.ADDRESS; n: CARDINAL; VAR locs: CARDINAL);
  VAR res: INTEGER; c,k: CARDINAL; ch,pch: CHAR;
BEGIN
  x^.result:=IOConsts.allRight;
  locs:=0;
  IF n=0 THEN RETURN END;
  lock();
    locs:=0; c:=0;
    pch:=0C;
    WHILE locs<n DO
      res:=getc(x,ch);
      IF res=0 THEN res:=doecho(TRUE) END;
      IF res#0 THEN
        unlock();
        SoftError(x,res);
      END;
      IF ((textFlag IN x^.flags) & (ch=LF)) OR
         ((SEP1=0C) & (ch=SEP0)) OR
         ((ch=SEP1) & (pch=SEP0)) THEN
        flin:=FALSE
      END;
      SYSTEM.PUT(a,ch); a:=SYSTEM.ADDADR(a,SIZE(CHAR)); INC(locs);
    END;
  unlock();
END RawRead;

PROCEDURE IniRead(x: Object);
BEGIN
  IF ChanConsts.rawFlag IN x^.flags THEN
    x^.doRawRead:=RawRead;
  END;
  IF ChanConsts.textFlag IN x^.flags THEN
    x^.doLook:=Look;
    x^.doSkip:=Skip;
    x^.doSkipLook:=SkipLook;
    x^.doTextRead:=TextRead
  END;
END IniRead;

PROCEDURE IniWrite(x: Object);
BEGIN
  IF ChanConsts.rawFlag IN x^.flags THEN
    x^.doRawWrite:=DoWrite;
  END;
  IF ChanConsts.textFlag IN x^.flags THEN
    x^.doTextWrite:=DoWrite;
    x^.doLnWrite:=WriteLn;
  END;
END IniWrite;

PROCEDURE initDevice(): INT;
  VAR res: INT;
BEGIN
  res:=xlibOS.X2C_InitTTY();
  IF res#0 THEN RETURN res END;
  bpos:=0;
  blen:=0;
  epos:=0;
  flin:=FALSE;
  init:=TRUE;
  RETURN 0;
END initDevice;

VAR did: IOLink.DeviceId;

PROCEDURE Open(VAR cid: ChanId; flags: FlagSet; VAR res: OpenResults);
  VAR x: Object;
BEGIN
  lock();
    IF NOT init THEN
      IF initDevice()#0 THEN
        res:=ChanConsts.otherProblem;
        unlock();
        RETURN
      END;
    END;
    IF NOT (ChanConsts.rawFlag IN flags) THEN INCL(flags,ChanConsts.textFlag) END;
    IOLink.MakeChan(did,cid);
    IF cid=IOChan.InvalidChan() THEN
      res:=ChanConsts.outOfChans;
      unlock(); RETURN
    END;
    x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
    x^.flags:=flags;
    IF ChanConsts.readFlag  IN flags THEN IniRead(x)  END;
    IF ChanConsts.writeFlag IN flags THEN IniWrite(x) END;
  unlock();
  res:=ChanConsts.opened;
END Open;

PROCEDURE IsTermFile (cid: ChanId): BOOLEAN;
BEGIN
  RETURN IOLink.IsDevice(cid,did)
END IsTermFile;

PROCEDURE Close(VAR cid: ChanId);
BEGIN
  IOLink.UnMakeChan(did,cid);
END Close;

BEGIN
  ASSERT(LENGTH(platform.lineSep)<=-BUF_LOW,100H);
  IOLink.AllocateDeviceId(did);
  <* IF multithread THEN *> xmRTS.X2C_NewMutex(mtx); <* END *>
  init:=FALSE;
END TermFile.
