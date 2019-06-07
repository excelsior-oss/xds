(* Copyright (c) xTech 1993. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE IOLink;

IMPORT  SYSTEM, IOChan, IOConsts, ChanConsts, Strings;

<* IF NOT STORAGE THEN *>
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
<* END *>

<* IF EXCEPTIONS THEN *>  IMPORT EXCEPTIONS;
<* ELSE *>                IMPORT XRaise;
<* END *>

<* IF EXCEPTIONS THEN *>
  VAR source: EXCEPTIONS.ExceptionSource;
<* END *>

TYPE
  DeviceId = POINTER TO RECORD dummy: INTEGER END;

VAR
  inv: DeviceTable;

PROCEDURE raise(e: IOChan.ChanExceptions; name-: ARRAY OF CHAR);
  VAR s: ARRAY [0..128] OF CHAR;
BEGIN
  CASE e OF
    |IOChan.wrongDevice    : s:="IOException.wrongDevice"
    |IOChan.notAvailable   : s:="IOException.notAvailable"
    |IOChan.skipAtEnd      : s:="IOException.skipAtEnd"
    |IOChan.softDeviceError: s:="IOException.softDeviceError"
    |IOChan.hardDeviceError: s:="IOException.hardDeviceError"
    |IOChan.textParseError : s:="IOException.textParseError"
    |IOChan.notAChannel    : s:="IOException.notAChannel"
  END;
  IF name[0]#'' THEN
    Strings.Append(" ",s);
    Strings.Append(name,s);
  END;
  <* IF EXCEPTIONS THEN *>
      EXCEPTIONS.RAISE(source,ORD(e),s);
  <* ELSE *>
      XRaise.RAISE(XRaise.IOLink+ORD(e),s);
  <* END *>
END raise;

PROCEDURE AllocateDeviceId(VAR did: DeviceId);
BEGIN
  NEW(did);
END AllocateDeviceId;

PROCEDURE MakeChan(did: DeviceId; VAR cid: IOChan.ChanId);
  VAR x: DeviceTablePtr;
BEGIN
  NEW(x);
  IF x=NIL THEN cid:=IOChan.InvalidChan();
  ELSE
    x^:=inv; x^.did:=did;
    cid:=SYSTEM.CAST(IOChan.ChanId,x);
    x^.cid:=cid;
  END;
END MakeChan;

PROCEDURE UnMakeChan(did: DeviceId; VAR cid: IOChan.ChanId);
  VAR x: DeviceTablePtr;
BEGIN
  x:=SYSTEM.CAST(DeviceTablePtr,cid);
  IF (x=NIL) OR (did#x^.did) THEN
    raise(IOChan.wrongDevice,"IOLink.UnMakeChan"); RETURN
  END;
  DISPOSE(x);
  cid:=IOChan.InvalidChan();
END UnMakeChan;

PROCEDURE DeviceTablePtrValue(cid: IOChan.ChanId; did: DeviceId;
                                x: DevExceptionRange; s: ARRAY OF CHAR): DeviceTablePtr;
  VAR dt: DeviceTablePtr;
BEGIN
  dt:=SYSTEM.CAST(DeviceTablePtr,cid);
  IF (dt=NIL) OR (did#dt^.did) THEN
    raise(IOChan.wrongDevice,"IOLink.DeviceTablePtrValue"); RETURN NIL
  END;
  RETURN dt
END DeviceTablePtrValue;

PROCEDURE IsDevice(cid: IOChan.ChanId; did: DeviceId): BOOLEAN;
  VAR x: DeviceTablePtr;
BEGIN
  x:=SYSTEM.CAST(DeviceTablePtr,cid);
  RETURN (x#NIL) & (did=x^.did)
END IsDevice;

PROCEDURE RAISEdevException(cid: IOChan.ChanId; did: DeviceId;
                             e: DevExceptionRange; s: ARRAY OF CHAR);
  VAR x: DeviceTablePtr;
BEGIN
  x:=SYSTEM.CAST(DeviceTablePtr,cid);
  IF (x=NIL) OR (did#x^.did) THEN
    raise(IOChan.wrongDevice,"IOLink.RAISEdevException"); RETURN
  END;
(*!! s should be included in message *)
  raise(e,s);
END RAISEdevException;

PROCEDURE IOException(): IOChan.ChanExceptions;
BEGIN
<* IF EXCEPTIONS THEN *>
  IF EXCEPTIONS.IsCurrentSource(source) THEN
    RETURN VAL(IOChan.ChanExceptions,EXCEPTIONS.CurrentNumber(source))
  ELSE
    HALT; (* raise what? *)
  END;
<* ELSE *>
  HALT;
  RETURN IOChan.notAChannel
<* END *>
END IOException;

PROCEDURE IsIOException(): BOOLEAN;
BEGIN
<* IF EXCEPTIONS THEN *>
  RETURN EXCEPTIONS.IsCurrentSource(source)
<* ELSE *>
  RETURN FALSE
<* END *>
END IsIOException;

(*----------------------------------------------------------------*)

<*$< WOFF301+ *>
PROCEDURE look(x: DeviceTablePtr; VAR c: CHAR; VAR r: IOConsts.ReadResults);
BEGIN
  raise(IOChan.notAvailable,"Look");
END look;

PROCEDURE skip(x: DeviceTablePtr);
BEGIN
  raise(IOChan.notAvailable,"Skip");
END skip;

PROCEDURE read(x: DeviceTablePtr; a: SYSTEM.ADDRESS; max: CARDINAL; VAR n: CARDINAL);
BEGIN
  raise(IOChan.notAvailable,"Read");
END read;

PROCEDURE write(x: DeviceTablePtr; a: SYSTEM.ADDRESS; max: CARDINAL);
BEGIN
  raise(IOChan.notAvailable,"Write");
END write;

PROCEDURE name(x: DeviceTablePtr; VAR s: ARRAY OF CHAR);
BEGIN
  s[0]:=0C;
END name;

PROCEDURE dummy(x: DeviceTablePtr);
END dummy;
<*$>*>

BEGIN
  WITH inv DO
    cd:=NIL;
    NEW(did);
    cid:=NIL;
    result:=IOConsts.notKnown;
    errNum:=0;
    flags:=ChanConsts.FlagSet{};
    doLook:=look;
    doSkip:=skip;
    doSkipLook:=look;
    doTextRead:=read;
    doTextWrite:=write;
    doLnWrite:=skip;
    doRawRead:=read;
    doRawWrite:=write;
    doGetName:=name;
    doReset:=dummy;
    doFlush:=dummy;
    doFree :=dummy;
  END;
<* IF EXCEPTIONS THEN *>
  EXCEPTIONS.AllocateSource(source);
<* END *>
END IOLink.
