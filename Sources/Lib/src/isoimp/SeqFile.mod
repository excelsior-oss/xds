(* Copyright (c) xTech 1993. All Rights Reserved. *)
(* Based on xDevData *)
IMPLEMENTATION MODULE SeqFile;

IMPORT  SYSTEM, IOLink, IOChan, ChanConsts;
IMPORT  xDevData, xlibOS, xrInt64;

VAR did: IOLink.DeviceId;

PROCEDURE doOpenWrite(x: xDevData.Object; fn: xDevData.FileName;
                 flags: FlagSet; VAR res: OpenResults);
  VAR f: xlibOS.X2C_OSFHANDLE;
   tags: BITSET;
BEGIN
  IF NOT (ChanConsts.oldFlag IN flags) & (xlibOS.X2C_Exists(fn^)) THEN
    res:=ChanConsts.fileExists; RETURN
  END;
  INCL(flags,ChanConsts.writeFlag);
  IF NOT (ChanConsts.rawFlag IN flags) THEN INCL(flags,ChanConsts.textFlag) END;

  IF ChanConsts.readFlag IN flags THEN
       tags:=xlibOS.X2C_fAccessRead
  ELSE tags:={};
  END;
  IF ChanConsts.textFlag IN flags THEN tags:=tags+xlibOS.X2C_fModeText END;
  IF ChanConsts.rawFlag IN flags THEN tags:=tags+xlibOS.X2C_fModeRaw END;
  tags:=tags+xlibOS.X2C_fAccessWrite+xlibOS.X2C_fModeNew;

  res:=xlibOS.X2C_fOpen(f,fn^,tags);

  IF res#ChanConsts.opened THEN RETURN END;

  xDevData.Open(x,f,fn,flags,xDevData.bmFull,res);
  IF res#ChanConsts.opened THEN xlibOS.X2C_fClose(f) END;
END doOpenWrite;

PROCEDURE OpenWrite(VAR cid: ChanId;
                       name: ARRAY OF CHAR;
                      flags: FlagSet;
                    VAR res: OpenResults);
  VAR fn: xDevData.FileName;
       x: xDevData.Object;
BEGIN
  IF NOT (ChanConsts.oldFlag IN flags) & (xlibOS.X2C_Exists(name)) THEN
    res:=ChanConsts.fileExists; RETURN
  END;
  xDevData.MakeName(fn,name,res);
  IF res#ChanConsts.opened THEN RETURN END;

  IOLink.MakeChan(did,cid);
  IF cid=IOChan.InvalidChan() THEN res:=ChanConsts.outOfChans; RETURN END;

  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  doOpenWrite(x,fn,flags,res);

  IF res=ChanConsts.opened THEN
    xDevData.SetMode(x,FALSE);
  ELSE
    xDevData.UnMakeName(fn); IOLink.UnMakeChan(did,cid);
  END;
END OpenWrite;

PROCEDURE OpenAppend(VAR cid: ChanId; name: ARRAY OF CHAR;
                       flags: FlagSet; VAR res: OpenResults);
  VAR f: xlibOS.X2C_OSFHANDLE;
   tags: BITSET;
    pos: xlibOS.X2C_FPOS;
     fn: xDevData.FileName;
      x: xDevData.Object;
      r: INTEGER;
BEGIN
  IF NOT (ChanConsts.rawFlag IN flags) THEN
    INCL(flags,ChanConsts.textFlag)
  END;

  flags:=flags+ChanConsts.write+ChanConsts.old;

  xDevData.MakeName(fn,name,res);
  IF res#ChanConsts.opened THEN RETURN END;

  IOLink.MakeChan(did,cid);
  IF cid=IOChan.InvalidChan() THEN
    res:=ChanConsts.outOfChans;
    xDevData.UnMakeName(fn);
    RETURN
  END;

  IF ChanConsts.readFlag IN flags THEN
       tags:=xlibOS.X2C_fAccessRead;
  ELSE tags:={};
  END;
  IF ChanConsts.textFlag IN flags THEN tags:=tags+xlibOS.X2C_fModeText END;
  IF ChanConsts.rawFlag IN flags THEN tags:=tags+xlibOS.X2C_fModeRaw END;
  tags:=tags+xlibOS.X2C_fAccessWrite;
  IF NOT xlibOS.X2C_Exists(fn^) THEN tags:=tags+xlibOS.X2C_fModeNew END;

  res:=xlibOS.X2C_fOpen(f,fn^,tags);
  IF res#ChanConsts.opened THEN
    IOLink.UnMakeChan(did,cid); xDevData.UnMakeName(fn); RETURN
  END;
  xrInt64.X2C_CARDTO64(pos,0);
  r:=xlibOS.X2C_fSeek(f,pos,xlibOS.X2C_fSeekEnd);
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  IF r#0 THEN xDevData.SoftError(x,r) END;

  xDevData.Open(x,f,fn,flags,xDevData.bmFull,res);
  IF res=ChanConsts.opened THEN
    xDevData.SetMode(x,FALSE);
  ELSE
    xDevData.UnMakeName(fn); xlibOS.X2C_fClose(f); IOLink.UnMakeChan(did,cid);
  END;
END OpenAppend;

PROCEDURE OpenRead(VAR cid: ChanId; name: ARRAY OF CHAR;
                     flags: FlagSet; VAR res: OpenResults);
  VAR f: xlibOS.X2C_OSFHANDLE;
   tags: BITSET;
     fn: xDevData.FileName;
      x: xDevData.Object;
BEGIN
  IF NOT (ChanConsts.rawFlag IN flags) THEN
    INCL(flags,ChanConsts.textFlag)
  END;
  flags:=flags+ChanConsts.read+ChanConsts.old;

  xDevData.MakeName(fn,name,res);
  IF res#ChanConsts.opened THEN RETURN END;

  IOLink.MakeChan(did,cid);
  IF cid=IOChan.InvalidChan() THEN
    res:=ChanConsts.outOfChans;
    xDevData.UnMakeName(fn);
    RETURN
  END;

  IF ChanConsts.writeFlag IN flags THEN
       tags:=xlibOS.X2C_fAccessWrite
  ELSE tags:={};
  END;
  IF ChanConsts.textFlag IN flags THEN tags:=tags+xlibOS.X2C_fModeText END;
  IF ChanConsts.rawFlag  IN flags THEN tags:=tags+xlibOS.X2C_fModeRaw  END;
  tags:=tags+xlibOS.X2C_fAccessRead;

  res:=xlibOS.X2C_fOpen(f,fn^,tags);
  IF res#ChanConsts.opened THEN
    IOLink.UnMakeChan(did,cid); xDevData.UnMakeName(fn); RETURN
  END;

  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  xDevData.Open(x,f,fn,flags,xDevData.bmFull,res);

  IF res=ChanConsts.opened THEN
    xDevData.SetMode(x,TRUE);
  ELSE
    xDevData.UnMakeName(fn); IOLink.UnMakeChan(did,cid);
  END;
END OpenRead;

PROCEDURE IsSeqFile(cid: ChanId): BOOLEAN;
  VAR x: xDevData.Object;
     dd: xDevData.DevData;
BEGIN
  IF IOLink.IsDevice(cid,did) THEN
    x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
    dd:=SYSTEM.CAST(xDevData.DevData,x^.cd);
    RETURN (xlibOS.X2C_fGetFileType(dd^.cf)=xlibOS.X2C_ftDisk);
  END;
  RETURN FALSE;
END IsSeqFile;

PROCEDURE Reread(cid: ChanId);
  VAR x: xDevData.Object;
   null: xlibOS.X2C_FPOS;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  xrInt64.X2C_CARDTO64(null,0);
(*  IOChan.Flush(cid); *)
  xDevData.SetPos(x,null);
  xDevData.SetMode(x,TRUE);
END Reread;

PROCEDURE Rewrite(cid: ChanId);
  VAR f: xDevData.DevData;
      x: xDevData.Object;
    res: OpenResults;
     fn: xDevData.FileName;
  flags: FlagSet;
      r: INTEGER;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  f:=xDevData.GetDevData(x);
  flags:=f^.flags+ChanConsts.old;
  xDevData.MakeName(fn,f^.name^,res);
  IF res=ChanConsts.opened THEN
    xDevData.Close(x);
    r:=xlibOS.X2C_fClose(f^.cf);
    IF r#0 THEN xDevData.SoftError(x,r) END;
    doOpenWrite(x,fn,flags,res);
  END;
  IF res#ChanConsts.opened THEN
    xDevData.ForbidAll(x)
  ELSE
    xDevData.SetMode(x,FALSE);
  END;
END Rewrite;

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
  xDevData.GetDID(did);
END SeqFile.
