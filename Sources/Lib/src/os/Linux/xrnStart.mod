(* Copyright (c) Excelsior LLC, 2002.  All Rights Reserved *)

<*- checkindex  *>
<*- checknil    *>
<*- checkdindex *>
<*- checktype   *>
<*- checkrange  *>
<*- checkset    *>
<*- checkdiv    *>
<*- checkproc   *>
<*- assert      *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xrnStart;

IMPORT
   SYSTEM
  ,xrtsOS
  ,xrnRTT
  ,xrHistory
  ,xmRTS
  ,X2C
  ,unistd;

CONST
  MAX_ARGS = 7FFFH;
  TAB = 11C;

TYPE
  ARGC  = SYSTEM.int;
  pCHAR = POINTER TO CHAR;
  ARGV  = POINTER TO ARRAY [0..MAX_ARGS] OF pCHAR;

  MAINTYPE = PROCEDURE ["C"] ( ARGC, SYSTEM.ADDRESS) :SYSTEM.CARD32;

VAR
  main :MAINTYPE;
  bHandler: BreakHandler;


PROCEDURE ["C"] X2C_SetBreakHandler(h: BreakHandler): BreakHandler;
  VAR o: BreakHandler;
BEGIN
  o:=bHandler;
  bHandler:=h;
  RETURN o;
END X2C_SetBreakHandler;

PROCEDURE ["C"] X2C_EstablishMain ( mainAdr :SYSTEM.ADDRESS );
BEGIN
  main := SYSTEM.CAST ( MAINTYPE, mainAdr );
END X2C_EstablishMain;


END xrnStart.

