(* Copyright (c) Excelsior 1999-2002.  All Rights Reserved *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xosExit;


IMPORT xmRTS, X2C, stdlib;

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
    xmRTS.X2C_TRAP_F(ORD(X2C.X2C_internalError));
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

  stdlib.exit(code);
END X2C_doexit;


END xosExit.
