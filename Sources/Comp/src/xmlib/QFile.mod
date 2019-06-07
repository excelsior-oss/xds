(* Copyright (c) 1994,97 XDS Ltd, Russia. All Rights Reserved. *)
(* Quick raw sequential data streams *)
(* Based on ISO RndFile *)
<*+ M2EXTENSIONS *>
<*+ M2ADDTYPES *>
IMPLEMENTATION MODULE QFile;

IMPORT  SYSTEM, IOLink, IOChan, cc:=ChanConsts, io:=IOConsts, RndFile, xFilePos;

FROM Storage IMPORT  ALLOCATE, DEALLOCATE;

CONST BufSize = 4096;

TYPE
  Object = IOLink.DeviceTablePtr;
  File = POINTER TO FileDesc;
  FileDesc = RECORD
    cid: ChanId;
    out: BOOLEAN;
    buf: ARRAY [0..BufSize-1] OF CHAR;
    pos: CARDINAL;
    len: CARDINAL; (* for read only  *)
    org: CARDINAL; (* for write only *)
  END;

VAR did: IOLink.DeviceId;

(*----------------------------------------------------------------*)

PROCEDURE update(f: File);
BEGIN
  IOChan.RawWrite(f^.cid,SYSTEM.ADR(f^.buf),f^.pos);
  INC(f^.org,f^.pos);
  f^.pos:=0;
END update;

PROCEDURE RawWrite(x: Object; a: SYSTEM.ADDRESS; n: CARDINAL);
  VAR f: File;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  IF f^.pos+n > BufSize THEN update(f) END;
  IF n > BufSize THEN IOChan.RawWrite(f^.cid,a,n); INC(f^.org,f^.pos);
  ELSE
    IF n = 1 THEN
      f^.buf[f^.pos]:=SYSTEM.CAST(CHAR,a^);
    ELSE
      SYSTEM.MOVE(a,SYSTEM.ADR(f^.buf[f^.pos]),n);
    END;
    INC(f^.pos,n)
  END;
END RawWrite;

PROCEDURE ReadFromBuf(f: File; a: SYSTEM.ADDRESS; ofs,n: CARDINAL; VAR len: CARDINAL);
BEGIN
  IF f^.pos < f^.len THEN
    len:=f^.len-f^.pos;
    IF len > n THEN len:=n END;
    SYSTEM.MOVE(SYSTEM.ADR(f^.buf[f^.pos]),SYSTEM.ADDADR(a,ofs),n);
    INC(f^.pos,len);
  ELSE len:=0
  END;
END ReadFromBuf;

PROCEDURE RawRead(x: Object; a: SYSTEM.ADDRESS; n: CARDINAL; VAR locs: CARDINAL);
  VAR f: File; len,ofs: CARDINAL;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  locs:=0;
  IF n=0 THEN x^.result:=io.allRight; RETURN END;
  ReadFromBuf(f,a,0,n,len);
  ofs:=len; DEC(n,len); INC(locs,len);
  IF n > 0 THEN
    IF n > BufSize DIV 2 THEN
      IOChan.RawRead(f^.cid,SYSTEM.ADDADR(a,ofs),n,len);
      INC(locs,len);
    ELSIF f^.len = BufSize THEN (* not eof *)
      f^.pos:=0;
      IOChan.RawRead(f^.cid,SYSTEM.ADR(f^.buf),BufSize,f^.len);
      ReadFromBuf(f,a,ofs,n,len);
      INC(locs,len);
    END;
  END;
  IF locs=0 THEN x^.result:=io.endOfInput;
  ELSE x^.result:=io.allRight;
  END;
END RawRead;

PROCEDURE GetName(x: Object; VAR s: ARRAY OF CHAR);
  VAR f: File;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  IOChan.GetName(f^.cid,s);
END GetName;

PROCEDURE Flush(x: Object);
  VAR f: File;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  IF f^.out & (f^.pos>0) THEN update(f) END;
END Flush;

(*----------------------------------------------------------------*)

PROCEDURE create(cid: ChanId; seq: ChanId; VAR res: OpenResults);
  VAR x: IOLink.DeviceTablePtr; flags: FlagSet;
      f: File;
BEGIN
  NEW(f);
  IF f=NIL THEN res:=cc.otherProblem; RETURN END;
  f^.pos:=0;
  f^.len:=0;
  f^.org:=0;
  f^.cid:=seq;
  f^.out:=FALSE;
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  x^.cd:=f;
  flags:=IOChan.CurrentFlags(seq);
  x^.flags:=flags;
  IF cc.writeFlag IN flags THEN
    f^.out:=TRUE;
    x^.doRawWrite:=RawWrite;
  ELSIF cc.readFlag IN flags THEN
    x^.doRawRead:=RawRead;
    f^.len:=BufSize; f^.pos:=BufSize;
  END;
  x^.doGetName:=GetName;
  x^.doFlush:=Flush;
  res:=cc.opened;
END create;

PROCEDURE OpenRead(VAR cid: ChanId; name: ARRAY OF CHAR;
                     flags: FlagSet; VAR res: OpenResults);
  VAR seq: ChanId;
BEGIN
  IF cc.textFlag  IN flags THEN res:=cc.noTextOperations; RETURN END;
  IF cc.writeFlag IN flags THEN res:=cc.wrongFlags; RETURN END;
  RndFile.OpenOld(seq,name,flags+cc.raw+cc.read+cc.old,res);
  IF res=cc.opened THEN
    IOLink.MakeChan(did,cid);
    IF cid=IOChan.InvalidChan() THEN
      RndFile.Close(seq); res:=cc.outOfChans;
    ELSE
      create(cid,seq,res);
      IF res#cc.opened THEN IOLink.UnMakeChan(did,cid) END;
    END;
  END;
END OpenRead;

PROCEDURE OpenWrite(VAR cid: ChanId; name: ARRAY OF CHAR;
                 flags: FlagSet; VAR res: OpenResults);
  VAR seq: ChanId;
BEGIN
  IF cc.textFlag IN flags THEN res:=cc.noTextOperations; RETURN END;
  IF cc.readFlag IN flags THEN res:=cc.wrongFlags; RETURN END;
  RndFile.OpenClean(seq,name,flags+cc.raw+cc.write+cc.old,res);
  IF res=cc.opened THEN
    IOLink.MakeChan(did,cid);
    IF cid=IOChan.InvalidChan() THEN
      RndFile.Close(seq); res:=cc.outOfChans;
    ELSE
      create(cid,seq,res);
      IF res#cc.opened THEN IOLink.UnMakeChan(did,cid) END;
    END;
  END;
END OpenWrite;

PROCEDURE Length(cid: ChanId): SYSTEM.CARD32;
  VAR x: IOLink.DeviceTablePtr; f: File; p: SYSTEM.CARD32;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  f:=SYSTEM.CAST(File,x^.cd);
  IF cc.writeFlag IN x^.flags THEN
    RETURN f^.org+f^.pos
  ELSE
    IF xFilePos.PosToCard(p,RndFile.EndPos(f^.cid)) THEN RETURN p END;
    RETURN MAX(SYSTEM.CARD32);
  END;
END Length;

PROCEDURE IsQFile(cid: ChanId): BOOLEAN;
BEGIN
  RETURN IOLink.IsDevice(cid,did)
END IsQFile;

PROCEDURE Close(VAR cid: ChanId);
  VAR f: File; x: Object;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  f:=SYSTEM.CAST(File,x^.cd);
  Flush(x);
  RndFile.Close(f^.cid);
  DISPOSE(f);
  IOLink.UnMakeChan(did,cid);
END Close;

BEGIN
  IOLink.AllocateDeviceId(did);
END QFile.
