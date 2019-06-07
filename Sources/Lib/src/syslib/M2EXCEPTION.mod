(* Copyright (c) 1994 xTech Ltd, Russia. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE M2EXCEPTION;
(** Ned 21-Jan-94. *)
(** Sem 27-Sep-94. *)

IMPORT EXCEPTIONS, SYSTEM, xmRTS;

CONST
  Fault = VAL(EXCEPTIONS.ExceptionNumber,exException);

VAR
  Source : EXCEPTIONS.ExceptionSource;

PROCEDURE M2Exception(): M2Exceptions;
  VAR n: EXCEPTIONS.ExceptionNumber;
BEGIN
  IF NOT EXCEPTIONS.IsExceptionalExecution() THEN
    EXCEPTIONS.RAISE(Source,Fault,"No current exception");
  END;
  IF NOT EXCEPTIONS.IsCurrentSource(Source) THEN
    EXCEPTIONS.RAISE(Source,Fault,"Not Modula-2 exception source");
  END;
  n:=EXCEPTIONS.CurrentNumber(Source);
  IF n <= ORD(MAX(M2Exceptions)) THEN
    RETURN VAL(M2Exceptions,n)
  ELSE
    EXCEPTIONS.RAISE(Source,ORD(exException),"Not Modula-2 exception");
  END;
END M2Exception;

PROCEDURE IsM2Exception(): BOOLEAN;
  VAR n: SYSTEM.CARD32;
BEGIN
  IF EXCEPTIONS.IsCurrentSource(Source) THEN
    n:=EXCEPTIONS.CurrentNumber(Source);
    RETURN n <= ORD(MAX(M2Exceptions))
  END;
  RETURN FALSE
END IsM2Exception;

BEGIN
  Source:=SYSTEM.CAST(EXCEPTIONS.ExceptionSource,xmRTS.X2C_rtsSource);
END M2EXCEPTION.
