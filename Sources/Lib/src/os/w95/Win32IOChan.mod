(* Copyright (c) 1996 xTech Ltd, Russia. All Rights Reserved. *)
IMPLEMENTATION MODULE Win32IOChan; (* Hady. 01.08.96 15:28 *)

IMPORT
  SYSTEM, IOChan, IOLink, ChanConsts, xDevData, xlibOS, Windows;

VAR did: IOLink.DeviceId;

PROCEDURE GetSysHandle(cid: IOChan.ChanId): HANDLE;
  VAR x: xDevData.Object;
      d: xDevData.DevData;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"Win32IOChan.GetSysHandle");
  d:=x^.cd;
  RETURN SYSTEM.CAST(HANDLE,d^.cf);
END GetSysHandle;

PROCEDURE MakeChannel(VAR cid: ChanId;
                       handle: HANDLE;
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
                SYSTEM.CAST(xlibOS.X2C_OSFHANDLE,handle),fn,flags,xDevData.bmDefault,res);
  IF res#ChanConsts.opened THEN
    IOLink.UnMakeChan(did,cid);
    xDevData.UnMakeName(fn);
  END;
END MakeChannel;

PROCEDURE GetOpenAttrs(VAR data: XtdOpenAttrs);
  VAR a: xlibOS.X2C_FXATTRS;
BEGIN
  xlibOS.X2C_fGetXAttrs(a);
  WITH data DO
    shareMode:=SYSTEM.CAST(Windows.FILE_SHARE_MODE,a[0]);
    securityArrts:=SYSTEM.CAST(Windows.PSECURITY_ATTRIBUTES,a[1]);
    fileAttrs:=SYSTEM.CAST(Windows.DWORD,a[2]);
    fileFlags:=SYSTEM.CAST(Windows.FILE_ATTRIBUTE_SET,a[3]);
  END;
END GetOpenAttrs;

PROCEDURE SetOpenAttrs(data: XtdOpenAttrs);
  VAR a: xlibOS.X2C_FXATTRS;
BEGIN
  WITH data DO
    a[0]:=SYSTEM.CAST(SYSTEM.CARD32,shareMode);
    a[1]:=SYSTEM.CAST(SYSTEM.CARD32,securityArrts);
    a[2]:=SYSTEM.CAST(SYSTEM.CARD32,fileAttrs);
    a[3]:=SYSTEM.CAST(SYSTEM.CARD32,fileFlags);
  END;
  xlibOS.X2C_fSetXAttrs(a);
END SetOpenAttrs;

BEGIN
  xDevData.GetDID(did);
END Win32IOChan.
