(* Copyright (c) 1994 xTech Ltd, Russia. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
<*- GENDEBUG     *>
IMPLEMENTATION MODULE EXCEPTIONS;
(** Ned 20-Jan-94. *)
(** Sem 22-Mar-94. *)

IMPORT SYSTEM, xmRTS, M2EXCEPTION;

CONST
  st_exceptional=xmRTS.X2C_st_exceptional;
  exException = ORD(M2EXCEPTION.exException);

TYPE
  ExceptionSource = xmRTS.X2C_XSource;

PROCEDURE AllocateSource(VAR newSource: ExceptionSource);
  VAR a: SYSTEM.ADDRESS;
BEGIN
  xmRTS.X2C_ALLOCATE(a,SIZE(xmRTS.X2C_XSource_STR));
  IF a=NIL THEN
    RAISE(xmRTS.X2C_rtsSource,exException,
		"Exception source is not allocated");
  END;
  newSource:=a;
  newSource^.number:=0;
  newSource^.message[0]:=0C;
  <* IF multithread THEN *>
  xmRTS.X2C_NewMutex(newSource^.lock);
  <* END *>
END AllocateSource;

PROCEDURE RAISE(source  : ExceptionSource;
		number  : ExceptionNumber;
	        message-: ARRAY OF CHAR);
BEGIN
  <* IF multithread THEN *>
  xmRTS.X2C_LockMutex(source^.lock);
  <* END *>
  source^.number:=number;
  COPY(message,source^.message);
  xmRTS.X2C_doRaise(source);
END RAISE;

PROCEDURE handler(): xmRTS.X2C_XHandler;
  VAR current: xmRTS.X2C_Coroutine;
BEGIN
  current:=xmRTS.X2C_GetCurrent();
  RETURN current^.handler;
END handler;

PROCEDURE CurrentNumber(source: ExceptionSource): ExceptionNumber;
  VAR x: xmRTS.X2C_XHandler;
BEGIN
  x:=handler();
  IF (x=NIL) OR (x^.state # st_exceptional) THEN
    RAISE(xmRTS.X2C_rtsSource,exException,"No current exception");
  ELSIF x^.source=source THEN
    RETURN source^.number;
  ELSIF x^.source=xmRTS.X2C_rtsSource THEN
    RAISE(xmRTS.X2C_rtsSource,exException,"Not exception source");
  ELSE
    RAISE(xmRTS.X2C_rtsSource,exException,"Not current source");
  END;
END CurrentNumber;

PROCEDURE GetMessage(VAR text: ARRAY OF CHAR);
  VAR x: xmRTS.X2C_XHandler;
BEGIN
  x:=handler();
  IF (x#NIL) & (x^.state=st_exceptional) THEN
    COPY(x^.source^.message,text);
  ELSE
    text[0]:=0C;
  END;
END GetMessage;

PROCEDURE IsCurrentSource(source: ExceptionSource): BOOLEAN;
  VAR x: xmRTS.X2C_XHandler;
BEGIN
  x:=handler();
  RETURN (x#NIL) & (x^.state=st_exceptional) & (x^.source=source);
END IsCurrentSource;

PROCEDURE IsExceptionalExecution(): BOOLEAN;
  VAR x: xmRTS.X2C_XHandler;
BEGIN
  x:=handler();
  RETURN (x#NIL) & (x^.state=st_exceptional);
END IsExceptionalExecution;

END EXCEPTIONS.
