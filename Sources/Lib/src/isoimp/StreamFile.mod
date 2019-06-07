(* Copyright (c) xTech 1993. All Rights Reserved. *)
(* Based on xDevData *)
IMPLEMENTATION MODULE StreamFile;

IMPORT  SYSTEM, IOLink, IOChan, ChanConsts, xDevData, xlibOS;

VAR did: IOLink.DeviceId;

PROCEDURE Open(VAR cid: ChanId; name: ARRAY OF CHAR;
                 flags: FlagSet; VAR res: OpenResults);

  CONST rw=read+write;

  VAR f: xlibOS.X2C_OSFHANDLE;
   tags: BITSET;
     fn: xDevData.FileName;
    exi: BOOLEAN;
BEGIN
  IF NOT (ChanConsts.writeFlag IN flags) THEN INCL(flags,ChanConsts.readFlag) END;
  IF ChanConsts.readFlag IN flags THEN INCL(flags,ChanConsts.oldFlag) END;
  IF NOT (ChanConsts.rawFlag IN flags) THEN INCL(flags,ChanConsts.textFlag) END;

  xDevData.MakeName(fn,name,res);
  IF res#ChanConsts.opened THEN RETURN END;

  IOLink.MakeChan(did,cid);
  IF cid=IOChan.InvalidChan() THEN
    xDevData.UnMakeName(fn);
    res:=ChanConsts.outOfChans;
    RETURN
  END;

  IF flags*rw=rw THEN tags:=xlibOS.X2C_fAccessRead+xlibOS.X2C_fAccessWrite;
  ELSIF ChanConsts.readFlag IN flags THEN
    tags:=xlibOS.X2C_fAccessRead
  ELSE
    exi:=xlibOS.X2C_Exists(fn^);
    IF exi THEN
      IF NOT (ChanConsts.oldFlag IN flags) THEN
        res:=ChanConsts.fileExists;
        xDevData.UnMakeName(fn);
        IOLink.UnMakeChan(did,cid);
        RETURN
      END;
      tags:=xlibOS.X2C_fAccessWrite;
    ELSE
      tags:=xlibOS.X2C_fAccessWrite+xlibOS.X2C_fModeNew;
    END;
  END;
  IF ChanConsts.textFlag IN flags THEN tags:=tags+xlibOS.X2C_fModeText END;
  IF ChanConsts.rawFlag IN flags  THEN tags:=tags+xlibOS.X2C_fModeRaw  END;

  res:=xlibOS.X2C_fOpen(f,fn^,tags);
  IF res#ChanConsts.opened THEN
    xDevData.UnMakeName(fn);
    IOLink.UnMakeChan(did,cid);
    RETURN
  END;

  xDevData.Open(IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,""),
                f,fn,flags,xDevData.bmDefault,res);
  IF res#ChanConsts.opened THEN
    xlibOS.X2C_fClose(f);
    xDevData.UnMakeName(fn);
    IOLink.UnMakeChan(did,cid);
  END;
END Open;

PROCEDURE IsStreamFile(cid: ChanId): BOOLEAN;
BEGIN
  RETURN IOLink.IsDevice(cid,did)
END IsStreamFile;

PROCEDURE Close(VAR cid: ChanId);
  VAR x: xDevData.Object;
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
  xDevData.GetDID(did);
END StreamFile.
