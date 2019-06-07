(* Copyright (c) xTech 1993. All Rights Reserved. *)
IMPLEMENTATION MODULE IOChan;

IMPORT IOConsts, ChanConsts, SYSTEM, IOLink;

TYPE ChanId = IOLink.DeviceTablePtr;

VAR inv: ChanId;

PROCEDURE InvalidChan (): ChanId;
BEGIN RETURN inv
END InvalidChan;

PROCEDURE Look (cid: ChanId; VAR ch: CHAR; VAR res: IOConsts.ReadResults);
BEGIN
  cid^.doLook(cid,ch,res);
END Look;

PROCEDURE Skip(cid: ChanId);
BEGIN
  cid^.doSkip(cid)
END Skip;

PROCEDURE SkipLook(cid: ChanId; VAR ch: CHAR; VAR res: IOConsts.ReadResults);
BEGIN
  cid^.doSkipLook(cid,ch,res)
END SkipLook;

PROCEDURE WriteLn(cid: ChanId);
BEGIN
  cid^.doLnWrite(cid)
END WriteLn;

PROCEDURE TextRead(cid: ChanId; to: SYSTEM.ADDRESS; max: CARDINAL; VAR locs: CARDINAL);
BEGIN
  cid^.doTextRead(cid,to,max,locs)
END TextRead;

PROCEDURE TextWrite(cid: ChanId; from: SYSTEM.ADDRESS; len: CARDINAL);
BEGIN
  cid^.doTextWrite(cid,from,len)
END TextWrite;

PROCEDURE RawRead(cid: ChanId; to: SYSTEM.ADDRESS; max: CARDINAL; VAR locs: CARDINAL);
BEGIN
  cid^.doRawRead(cid,to,max,locs)
END RawRead;

PROCEDURE RawWrite(cid: ChanId; from: SYSTEM.ADDRESS; locs: CARDINAL);
BEGIN
  cid^.doRawWrite(cid,from,locs)
END RawWrite;

PROCEDURE GetName(cid: ChanId; VAR s: ARRAY OF CHAR);
BEGIN
  cid^.doGetName(cid,s)
END GetName;

PROCEDURE Reset(cid: ChanId);
BEGIN
  cid^.doReset(cid)
END Reset;

PROCEDURE Flush(cid: ChanId);
BEGIN
  cid^.doFlush(cid)
END Flush;

PROCEDURE SetReadResult (cid: ChanId; res: IOConsts.ReadResults);
BEGIN
  cid^.result:=res;
END SetReadResult;

PROCEDURE ReadResult (cid: ChanId): IOConsts.ReadResults;
BEGIN
  RETURN cid^.result
END ReadResult;

PROCEDURE CurrentFlags (cid: ChanId): ChanConsts.FlagSet;
BEGIN
  RETURN cid^.flags;
END CurrentFlags;

PROCEDURE IsChanException(): BOOLEAN;
BEGIN
  RETURN IOLink.IsIOException()
END IsChanException;

PROCEDURE ChanException(): ChanExceptions;
BEGIN
  RETURN IOLink.IOException()
END ChanException;

PROCEDURE DeviceError(cid: ChanId): DeviceErrNum;
BEGIN
  RETURN cid^.errNum
END DeviceError;

VAR did: IOLink.DeviceId;

BEGIN
  IOLink.AllocateDeviceId(did);
  inv:=NIL;
  IOLink.MakeChan(did,inv);
  IF inv=NIL THEN HALT END;
END IOChan.
