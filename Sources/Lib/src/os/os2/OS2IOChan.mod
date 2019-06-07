(* Copyright (c) 1996 xTech Ltd, Russia. All Rights Reserved. *)

<*+M2EXTENSIONS *>

IMPLEMENTATION MODULE OS2IOChan; (* VitVit'n'Hady. 08.10.96 10:28 *)

IMPORT
  SYSTEM, IOChan, IOLink, ChanConsts, xDevData, xlibOS,
          O := OS2;

VAR did: IOLink.DeviceId;


PROCEDURE GetSysHandle(cid: IOChan.ChanId): HANDLE;
  VAR x: xDevData.Object;
      d: xDevData.DevData;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"OS2IOChan.GetSysHandle");
  d:=x^.cd;
  RETURN SYSTEM.CAST(HANDLE, d^.cf);
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
  IF (res#ChanConsts.opened) THEN
    IOLink.UnMakeChan(did,cid);
    xDevData.UnMakeName(fn);
  END;
END MakeChannel;


PROCEDURE GetOpenAttrs ( VAR data :XtdOpenAttrs );
VAR
  a :xlibOS.X2C_FXATTRS;
BEGIN
  xlibOS.X2C_fGetXAttrs (a);
  WITH data DO
    shareMode := a[0];
    fileAttrs := a[1];
  END;
END GetOpenAttrs;

PROCEDURE SetOpenAttrs ( data :XtdOpenAttrs );
VAR
  a :xlibOS.X2C_FXATTRS;
BEGIN
  WITH data DO
    a[0] := shareMode;
    a[1] := fileAttrs;
  END;
  xlibOS.X2C_fSetXAttrs (a);
END SetOpenAttrs;

BEGIN
  xDevData.GetDID(did);
END OS2IOChan.
