(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
(*
   implementation of environment access functions for OS/2
*)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xosEnv; (* Hady. 31.05.96 16:27 *)

IMPORT  SYSTEM, xmRTS, xOS2, X2C;

TYPE STR = POINTER TO ARRAY [0..7FFFH] OF CHAR;

PROCEDURE ["C"] X2C_EnvStringLength(name: xmRTS.X2C_pCHAR): SYSTEM.CARD32;
  VAR a: SYSTEM.ADDRESS; s: STR;
BEGIN
  IF xOS2.DosScanEnv(name,a) # 0 THEN RETURN 0 END;
  s:=SYSTEM.CAST(STR,a);
  RETURN LENGTH(s^);
END X2C_EnvStringLength;

PROCEDURE ["C"] X2C_EnvString(name: xmRTS.X2C_pCHAR; buf: xmRTS.X2C_pCHAR; blen: SYSTEM.CARD32);
  VAR a: SYSTEM.ADDRESS;
BEGIN
  IF (buf=NIL) OR (blen=0) THEN RETURN END;
  IF xOS2.DosScanEnv(name,a) # 0 THEN buf^:=0C
  ELSE
    xmRTS.X2C_COPY(a,7FFFH,buf,blen);
  END;
END X2C_EnvString;

CONST EmptyName = ARRAY OF CHAR{ 0C };

PROCEDURE ["C"] X2C_GetProgramName(): xmRTS.X2C_pCHAR;
BEGIN
  IF (X2C.X2C_argc = 0) THEN
    RETURN SYSTEM.CAST( xmRTS.X2C_pCHAR, SYSTEM.ADR(EmptyName) );
  ELSE
    RETURN SYSTEM.CAST( xmRTS.X2C_pCHAR, X2C.X2C_argv^ );
  END;
END X2C_GetProgramName;

END xosEnv.
