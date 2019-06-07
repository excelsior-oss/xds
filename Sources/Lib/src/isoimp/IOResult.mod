(* Copyright (c) xTech 1993. All Rights Reserved. *)
IMPLEMENTATION MODULE IOResult;

IMPORT IOConsts, IOChan;

PROCEDURE ReadResult(cid: IOChan.ChanId): ReadResults;
BEGIN
  RETURN IOChan.ReadResult(cid)
END ReadResult;

END IOResult.

