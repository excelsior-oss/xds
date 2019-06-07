(* Copyright (c) 1997 xTech Ltd, Russia. All Rights Reserved. *)
<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE CtrlC;

IMPORT xrnStart, X2C, xmRTS, EXCEPTIONS;

TYPE
  BH = xrnStart.BreakHandler;

VAR
  source :EXCEPTIONS.ExceptionSource; 

PROCEDURE SetBreakHandler(new :BreakHandler): BreakHandler;
BEGIN
  RETURN BreakHandler(xrnStart.X2C_SetBreakHandler(BH(new)));
END SetBreakHandler;

PROCEDURE IsCtrlCException() :BOOLEAN;
BEGIN
  RETURN EXCEPTIONS.IsCurrentSource(source) &
  (EXCEPTIONS.CurrentNumber(source)=EXCEPTIONS.ExceptionNumber(X2C.X2C_UserBreak));
END IsCtrlCException;

BEGIN
  source := EXCEPTIONS.ExceptionSource(xmRTS.X2C_rtsSource);
END CtrlC.
