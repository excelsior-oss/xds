(* Copyright (c) Excelsior 1999-2002.  All Rights Reserved *)
(*
   implementation of termination functions for Windows95/NT
*)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xosExit; (* Hady. 24.05.96 11:29 *)

IMPORT  xmRTS, xWin32, M2EXCEPTION, xrtsOS;

CONST atexitBufLen = 8;

VAR atexitBuf: ARRAY [0..atexitBufLen-1] OF xmRTS.X2C_EXIT_PROC;
    atexitCnt: CARDINAL;
    alreadyExitting :BOOLEAN;

PROCEDURE ["C"] X2C_iniexit;
BEGIN
  atexitCnt := 0;
  alreadyExitting := FALSE;
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
  IF NOT alreadyExitting THEN
    alreadyExitting := TRUE;

    xmRTS.X2C_EXIT_PROFILER (TRUE);

    WHILE atexitCnt>0 DO
      DEC(atexitCnt);
      p:=atexitBuf[atexitCnt];
      p();
    END;
  END;

  xWin32.ExitProcess(code);
END X2C_doexit;

END xosExit.
