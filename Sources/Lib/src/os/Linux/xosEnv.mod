(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosEnv; (* Hady. 28.06.96 16:26 *)

IMPORT  SYSTEM, xmRTS, X2C, xrtsOS;
FROM stdlib IMPORT getenv;
FROM string IMPORT strlen;

<* -CHECKINDEX *>
TYPE PSTR = POINTER TO ARRAY [0..0] OF CHAR;

PROCEDURE ["C"] X2C_EnvStringLength(name: xmRTS.X2C_pCHAR): SYSTEM.CARD32;
  VAR p: xmRTS.X2C_pCHAR;
BEGIN
  p:=getenv(name);
  IF p=NIL THEN RETURN 0 END;
  RETURN strlen(p);
END X2C_EnvStringLength;

PROCEDURE ["C"] X2C_EnvString(name: xmRTS.X2C_pCHAR; buf: xmRTS.X2C_pCHAR; blen: SYSTEM.CARD32);
----
(*
  TYPE
    pac = POINTER TO ARRAY OF CHAR;
*)
----

  VAR p: xmRTS.X2C_pCHAR;
    f,t: PSTR;
      i: SYSTEM.CARD32;

----
(*
      ii:INTEGER;
      pp:pac;
      nn:xmRTS.X2C_pCHAR;
*)
----

BEGIN
  p:=getenv(name);
  IF p=NIL THEN buf^:=0C; RETURN END;

  ---------------
(*
  nn:=name;
  ii:=0;
  WHILE nn^#0C DO
    nn := SYSTEM.ADDADR(nn, 1);
    INC(ii);
  END;
  xrtsOS.X2C_StdOut("required env name: ", 19);
  xrtsOS.X2C_StdOut(name, ii);
  xrtsOS.X2C_StdOutN;
  xrtsOS.X2C_StdOutFlush;
  nn:=p;
  ii:=0;
  WHILE nn^#0C DO
    nn := SYSTEM.ADDADR(nn, 1);
    INC(ii);
  END;
  xrtsOS.X2C_StdOut("its value: ", 11);
  xrtsOS.X2C_StdOut(p, ii);
  xrtsOS.X2C_StdOutN;
  xrtsOS.X2C_StdOutFlush;
*)
  ---------------

  f:=PSTR(p);
  t:=PSTR(buf);
  i:=0;
  WHILE (i<blen) & (f^[i]#0C) DO
    t^[i]:=f^[i];
    INC(i);
  END;
  IF i<blen THEN t^[i]:=0C END;
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
