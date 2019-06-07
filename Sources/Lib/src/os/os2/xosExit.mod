(* Copyright (c) Excelsior 1999-2002.  All Rights Reserved *)
(*
   implementation of termination functions for OS/2
*)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xosExit; (* Hady. 24.05.96 11:29 *)

IMPORT xmRTS,
       xrtsOS,
       xOS2,
       his := xrnShowHistory,
       M2EXCEPTION;

CONST atexitBufLen = 8;

VAR atexitBuf: ARRAY [0..atexitBufLen-1] OF xmRTS.X2C_EXIT_PROC;
    atexitCnt: CARDINAL;

PROCEDURE ["C"] X2C_iniexit;
BEGIN
  atexitCnt := 0;
END X2C_iniexit;

PROCEDURE ["C"] X2C_atexit(proc: xmRTS.X2C_EXIT_PROC);
BEGIN
  IF atexitCnt>=atexitBufLen THEN
    xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.coException));
  END;
  atexitBuf[atexitCnt]:=proc;
  INC(atexitCnt);
END X2C_atexit;

PROCEDURE ["C"] X2C_doexit(code: INTEGER);
  VAR p: xmRTS.X2C_EXIT_PROC;
BEGIN
  WHILE atexitCnt>0 DO
    DEC(atexitCnt);
    p:=atexitBuf[atexitCnt];
    p();
  END;
  his.X2C_EXIT_HISTORY ( NIL );
  xOS2.DosExit(xOS2.EXIT_PROCESS,code);
END X2C_doexit;

END xosExit.
