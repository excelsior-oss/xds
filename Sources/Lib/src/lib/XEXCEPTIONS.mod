(* Copyright (c) xTech 1997.  All Rights Reserved *)
IMPLEMENTATION MODULE XEXCEPTIONS; (* Hady. 20 Oct 1997 *)

IMPORT xmRTS, X2C, EXCEPTIONS, M2EXCEPTION, SYSTEM;

VAR
  rts, assert: EXCEPTIONS.ExceptionSource;

CONST
  Fault = VAL(EXCEPTIONS.ExceptionNumber,M2EXCEPTION.exException);

PROCEDURE RTSException(): RTSExceptions;
  VAR n: EXCEPTIONS.ExceptionNumber;
BEGIN
  IF NOT EXCEPTIONS.IsExceptionalExecution() THEN
    EXCEPTIONS.RAISE(rts,Fault,"Not an exceptional execution");
  END;
  IF NOT EXCEPTIONS.IsCurrentSource(rts) THEN
    EXCEPTIONS.RAISE(rts,Fault,"Not an RTS exception source");
  END;
  n:=EXCEPTIONS.CurrentNumber(rts);
  IF n <= ORD(MAX(M2EXCEPTION.M2Exceptions)) THEN
    EXCEPTIONS.RAISE(rts,Fault,"Not an RTS exception");
  END;
  (* Don't replace this IF by CASE *)
  IF    n = X2C.X2C_guardException    THEN RETURN typeGuardException;
  ELSIF n = X2C.X2C_noMemoryException THEN RETURN noMemoryException;
  ELSIF n = X2C.X2C_castError         THEN RETURN typeCastException;
  ELSIF n = X2C.X2C_UserBreak         THEN RETURN userBreakException;
  ELSIF n = X2C.X2C_unreachDLL        THEN RETURN unreachableDLLException;
  ELSE                                     RETURN internalRTSException;
  END;
END RTSException;

PROCEDURE IsRTSException(): BOOLEAN;
  VAR n: SYSTEM.CARD32;
BEGIN
  IF EXCEPTIONS.IsCurrentSource(rts) THEN
    n:=EXCEPTIONS.CurrentNumber(rts);
    RETURN n > ORD(MAX(M2EXCEPTION.M2Exceptions))
  END;
  RETURN FALSE
END IsRTSException;

PROCEDURE IsAssertException(): BOOLEAN;
BEGIN
  RETURN EXCEPTIONS.IsCurrentSource(assert);
END IsAssertException;

PROCEDURE AssertCode(): CARDINAL;
BEGIN
  IF NOT EXCEPTIONS.IsExceptionalExecution() THEN
    EXCEPTIONS.RAISE(rts,Fault,"Not an exceptional execution");
  END;
  IF NOT EXCEPTIONS.IsCurrentSource(assert) THEN
    EXCEPTIONS.RAISE(rts,Fault,"Not an ASSERT exception source");
  END;
  RETURN EXCEPTIONS.CurrentNumber(rts);
END AssertCode;

BEGIN
  rts:=SYSTEM.CAST(EXCEPTIONS.ExceptionSource,xmRTS.X2C_rtsSource);
  assert:=SYSTEM.CAST(EXCEPTIONS.ExceptionSource,xmRTS.X2C_assertSrc);
END XEXCEPTIONS.

