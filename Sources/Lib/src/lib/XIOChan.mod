(* Copyright (c) 1996 xTech Ltd, Russia. All Rights Reserved. *)
IMPLEMENTATION MODULE XIOChan; (* Hady. 01.08.96 13:57 *)

IMPORT  SYSTEM, xDevData, IOLink, xlibOS, IOChan;

VAR did: IOLink.DeviceId;

PROCEDURE Truncate(cid: ChanId);
  VAR x: xDevData.Object;
      d: xDevData.DevData;
    pos: xlibOS.X2C_FPOS;
    res: SYSTEM.int;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"XIOChan.Truncate");
  d:=x^.cd;
  xDevData.CurrentPos(x, pos);          (* fixup for Win32 in sight of internal bufferring: *)
  IOChan.Flush(cid);
  xlibOS.X2C_fSeek(d^.cf, pos, xlibOS.X2C_fSeekSet); (* force setting current file position *)
  res:=xlibOS.X2C_fChSize(d^.cf);                    (* so that fChSize work correctly!     *)
  IF res#0 THEN xDevData.SoftError(x,res) END;
END Truncate;

BEGIN
  xDevData.GetDID(did);
END XIOChan.
