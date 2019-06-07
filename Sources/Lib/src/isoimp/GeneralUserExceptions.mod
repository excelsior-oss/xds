IMPLEMENTATION MODULE GeneralUserExceptions;

(* Provides facilities for general user-defined exceptions *)
IMPORT EXCEPTIONS;

CONST
  Fault = VAL(EXCEPTIONS.ExceptionNumber,disaster);

VAR
  Source : EXCEPTIONS.ExceptionSource;

PROCEDURE RaiseGeneralException (exception: GeneralExceptions; text: ARRAY OF CHAR);
BEGIN
  EXCEPTIONS.RAISE(Source,VAL(EXCEPTIONS.ExceptionNumber,exception),text);
END RaiseGeneralException;

PROCEDURE IsGeneralException (): BOOLEAN;
BEGIN
  RETURN EXCEPTIONS.IsCurrentSource(Source);
END IsGeneralException;

PROCEDURE GeneralException(): GeneralExceptions;
  VAR n: EXCEPTIONS.ExceptionNumber;
BEGIN
  IF NOT EXCEPTIONS.IsExceptionalExecution() THEN
    EXCEPTIONS.RAISE(Source,Fault,"No current exception");
  END;
  IF NOT EXCEPTIONS.IsCurrentSource(Source) THEN
    EXCEPTIONS.RAISE(Source,Fault,"Not general user exception source");
  END;
  n:=EXCEPTIONS.CurrentNumber(Source);
  RETURN VAL(GeneralExceptions,n)
END GeneralException;

BEGIN
  EXCEPTIONS.AllocateSource(Source);
END GeneralUserExceptions.
