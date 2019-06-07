(* Copyright (c) 1998 XDS Ltd.  All Rights Reserved. *)
IMPLEMENTATION MODULE POSIXIOChan; (* Snowman 02.10.98 *)

IMPORT
  SYSTEM, IOChan, IOLink, ChanConsts, xDevData, xlibOS;

VAR did: IOLink.DeviceId;

PROCEDURE GetFilePtr(cid: IOChan.ChanId): SYSTEM.ADDRESS;
  VAR x: xDevData.Object;
      d: xDevData.DevData;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"POSIXIOChan.GetFilePtr");
  d:=x^.cd;
  RETURN SYSTEM.CAST(SYSTEM.ADDRESS,d^.cf);
END GetFilePtr;

PROCEDURE GetFileName(cid: IOChan.ChanId): xDevData.FileName;
  VAR x: xDevData.Object;
      d: xDevData.DevData;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"POSIXIOChan.GetFileName");
  d:=x^.cd;
  RETURN d^.name;
END GetFileName;

PROCEDURE MakeChannel(VAR cid: ChanId;
                         file: SYSTEM.ADDRESS;
                         name: ARRAY OF CHAR;
                        flags: FlagSet;
                      VAR res: OpenResults);
  VAR fn: xDevData.FileName;
BEGIN
  IOLink.MakeChan(did,cid);
  IF cid=IOChan.InvalidChan() THEN
    res:=ChanConsts.outOfChans; RETURN
  END;
  xDevData.MakeName(fn,name,res);
  IF res#ChanConsts.opened THEN
    IOLink.UnMakeChan(did,cid); RETURN
  END;
  xDevData.Open(IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,""),
                SYSTEM.CAST(xlibOS.X2C_OSFHANDLE,file),fn,flags,xDevData.bmDefault,res);
  IF res#ChanConsts.opened THEN
    IOLink.UnMakeChan(did,cid);
    xDevData.UnMakeName(fn);
  END;
END MakeChannel;

BEGIN
  xDevData.GetDID(did);
END POSIXIOChan.
