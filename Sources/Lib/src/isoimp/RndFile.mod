(* Copyright (c) xTech 1993. All Rights Reserved. *)
(* Based on xDevData *)
<*+ M2EXTENSIONS *>
<*+ M2ADDTYPES *>
IMPLEMENTATION MODULE RndFile;

IMPORT  SYSTEM, IOLink, IOChan, ChanConsts;
IMPORT  xlibOS, xDevData, xrInt64;

<* IF EXCEPTIONS THEN *>  IMPORT  EXCEPTIONS;
<* ELSE *>                IMPORT  XRaise;
<* END *>


TYPE
  FPOS = RECORD
    CASE : BOOLEAN OF
      |TRUE : rec: xlibOS.X2C_FPOS;
      |FALSE: arr: FilePos;
    END;
  END;

<* IF EXCEPTIONS THEN *>
  VAR source: EXCEPTIONS.ExceptionSource;
<* END *>

PROCEDURE raise;
BEGIN
  <* IF EXCEPTIONS THEN *>
    EXCEPTIONS.RAISE(source,0,"RndFile.Exception");
  <* ELSE *>
    XRaise.RAISE(XRaise.RndFile,"RndFile.Exception");
  <* END *>
END raise;

(*----------------------------------------------------------------*)

VAR did: IOLink.DeviceId;

PROCEDURE OpenOld(VAR cid: ChanId; name: ARRAY OF CHAR; flags: FlagSet;
                  VAR res: OpenResults);
  VAR f: xlibOS.X2C_OSFHANDLE;
   tags: BITSET;
     fn: xDevData.FileName;
BEGIN
  INCL(flags,ChanConsts.oldFlag);
  IF NOT (ChanConsts.textFlag IN flags) THEN INCL(flags,ChanConsts.rawFlag) END;
  IF NOT (ChanConsts.writeFlag IN flags) THEN INCL(flags,ChanConsts.readFlag) END;
  IOLink.MakeChan(did,cid);
  IF cid=IOChan.InvalidChan() THEN
    res:=ChanConsts.outOfChans; RETURN
  END;
  xDevData.MakeName(fn,name,res);
  IF res#ChanConsts.opened THEN
    IOLink.UnMakeChan(did,cid); RETURN
  END;

  IF ChanConsts.writeFlag IN flags THEN
       tags:=xlibOS.X2C_fAccessWrite
  ELSE tags:={}
  END;
  IF ChanConsts.readFlag IN flags THEN tags:=tags+xlibOS.X2C_fAccessRead END;
  IF ChanConsts.textFlag IN flags THEN tags:=tags+xlibOS.X2C_fModeText END;
  IF ChanConsts.rawFlag IN flags THEN tags:=tags+xlibOS.X2C_fModeRaw END;

  res:=xlibOS.X2C_fOpen(f,fn^,tags);
  IF res#ChanConsts.opened THEN
    IOLink.UnMakeChan(did,cid);
    xDevData.UnMakeName(fn);
    RETURN
  END;

  xDevData.Open(IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,""),
                f,fn,flags,xDevData.bmFull,res);

  IF res#ChanConsts.opened THEN
    IOLink.UnMakeChan(did,cid);
    xDevData.UnMakeName(fn);
    xlibOS.X2C_fClose(f);
  END;
END OpenOld;

PROCEDURE OpenClean(VAR cid: ChanId; name: ARRAY OF CHAR; flags: FlagSet;
                    VAR res: OpenResults);
  VAR f: xlibOS.X2C_OSFHANDLE;
   tags: BITSET;
     fn: xDevData.FileName;
BEGIN
  INCL(flags,ChanConsts.writeFlag);
  IF NOT (ChanConsts.textFlag IN flags) THEN INCL(flags,ChanConsts.rawFlag) END;

  IOLink.MakeChan(did,cid);
  IF cid=IOChan.InvalidChan() THEN
    res:=ChanConsts.outOfChans;
    RETURN
  END;

  xDevData.MakeName(fn,name,res);
  IF res#ChanConsts.opened THEN IOLink.UnMakeChan(did,cid); RETURN END;

  IF NOT (ChanConsts.oldFlag IN flags) & xlibOS.X2C_Exists(fn^) THEN
    res:=ChanConsts.fileExists;
    xDevData.UnMakeName(fn);
    IOLink.UnMakeChan(did,cid);
    RETURN
  END;

  IF ChanConsts.readFlag IN flags THEN
       tags:=xlibOS.X2C_fAccessRead+xlibOS.X2C_fAccessWrite
  ELSE tags:=xlibOS.X2C_fAccessWrite;
  END;
  tags:=tags+xlibOS.X2C_fModeNew;
  IF ChanConsts.textFlag IN flags THEN tags:=tags+xlibOS.X2C_fModeText END;
  IF ChanConsts.rawFlag IN flags  THEN tags:=tags+xlibOS.X2C_fModeRaw  END;

  res:=xlibOS.X2C_fOpen(f,fn^,tags);
  IF res#ChanConsts.opened THEN
    xDevData.UnMakeName(fn);
    IOLink.UnMakeChan(did,cid);
    RETURN
  END;

  xDevData.Open(IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,""),
                f,fn,flags,xDevData.bmFull,res);

  IF res#ChanConsts.opened THEN
    xDevData.UnMakeName(fn);
    IOLink.UnMakeChan(did,cid);
    xlibOS.X2C_fClose(f);
  END;
END OpenClean;

PROCEDURE IsRndFile(cid: ChanId): BOOLEAN;
  VAR x: xDevData.Object;
     dd: xDevData.DevData;
BEGIN
  IF IOLink.IsDevice(cid,did) THEN
    x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
    dd:=SYSTEM.CAST(xDevData.DevData,x^.cd);
    RETURN (xlibOS.X2C_fGetFileType(dd^.cf)=xlibOS.X2C_ftDisk);
  END;
  RETURN FALSE;
END IsRndFile;

PROCEDURE IsRndFileException(): BOOLEAN;
BEGIN
<* IF EXCEPTIONS THEN *>
  RETURN EXCEPTIONS.IsCurrentSource(source)
<* ELSE *>
  RETURN FALSE
<* END *>
END IsRndFileException;

PROCEDURE StartPos(cid: ChanId): FilePos;
  VAR p: FPOS;
BEGIN
  IF NOT IsRndFile(cid) THEN raise END;
  xrInt64.X2C_CARDTO64(p.rec,0);
  RETURN p.arr;
END StartPos;

PROCEDURE CurrentPos(cid: ChanId): FilePos;
  VAR x: xDevData.Object;
      p: FPOS;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  xDevData.CurrentPos(x,p.rec);
  RETURN p.arr;
END CurrentPos;

PROCEDURE EndPos(cid: ChanId): FilePos;
  VAR x: xDevData.Object;
      p: FPOS;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  xDevData.Length(x,p.rec);
  RETURN p.arr;
END EndPos;

PROCEDURE NewPos(cid: ChanId; n: INTEGER; size: CARDINAL; pos: FilePos): FilePos;
  VAR c,x: FPOS;
BEGIN
  IF NOT IsRndFile(cid) THEN raise END;
  IF (size = 0) OR (n = 0) THEN RETURN pos END;
  c.arr:=pos;
  xrInt64.X2C_MUL64(x.rec,n,size);
  IF xrInt64.X2C_ADD64(c.rec,c.rec,x.rec) THEN raise END;
  RETURN c.arr;
END NewPos;

PROCEDURE SetPos(cid: ChanId; pos: FilePos);
  VAR x: xDevData.Object;
      p: FPOS;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  p.arr:=pos;
  xDevData.SetPos(x,p.rec);
END SetPos;

PROCEDURE Close(VAR cid: ChanId);
  VAR x: IOLink.DeviceTablePtr;
      f: xDevData.DevData;
     cf: xlibOS.X2C_OSFHANDLE;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  f:=xDevData.GetDevData(x);
  cf:=f^.cf;
  xDevData.Close(x);
  xlibOS.X2C_fClose(cf);
  IOLink.UnMakeChan(did,cid);
END Close;

BEGIN
  ASSERT(SIZE(FilePos)=SIZE(xlibOS.X2C_FPOS));
  xDevData.GetDID(did);
<* IF EXCEPTIONS THEN *>
  EXCEPTIONS.AllocateSource(source);
<* END *>
END RndFile.
