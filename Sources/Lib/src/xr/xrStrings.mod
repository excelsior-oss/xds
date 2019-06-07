<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<*- gendebug    *> (* Don enable! History would not work (SYSTEM.CODE) *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xrStrings;

IMPORT  SYSTEM, RTS:=xmRTS;
FROM SYSTEM    IMPORT  ADDADR, size_t, int;

PROCEDURE [2] X2C_CAP(x: CHAR): CHAR;
BEGIN
  IF (x >= 'a') & (x <= 'z') THEN x:=CHR(ORD(x)+ORD('A')-ORD('a')) END;
  RETURN x;
END X2C_CAP;

PROCEDURE [2] X2C_COPY(s: RTS.X2C_pVOID; s_len: size_t;
		   d: RTS.X2C_pVOID; d_len: size_t): RTS.X2C_pVOID;
  VAR x,y: RTS.X2C_pCHAR; i: size_t;
BEGIN
  x:=SYSTEM.CAST(RTS.X2C_pCHAR,s);
  y:=SYSTEM.CAST(RTS.X2C_pCHAR,d);
  IF s_len >= d_len THEN s_len:=d_len-1 END;
  i:=0;
  WHILE (i<s_len) & (x^#0C) DO
    y^:=x^;
    x:=ADDADR(x,SIZE(x^));
    y:=ADDADR(y,SIZE(y^));
    INC(i);
  END;
  y^:=0C;
  RETURN d;
END X2C_COPY;

<* IF (env_target="x86nt") OR (env_target="x86os2") OR (NATIVE_LIBRARY) THEN *>

(* Nothing *)

<* ELSE *>

PROCEDURE [2] X2C_LENGTH(s: RTS.X2C_pVOID; s_len: size_t): size_t;
  VAR x: RTS.X2C_pCHAR; i: size_t;
BEGIN
  x:=SYSTEM.CAST(RTS.X2C_pCHAR,s);
  i:=0;
  WHILE (i<s_len) & (x^#0C) DO
    x:=ADDADR(x,SIZE(x^));
    INC(i);
  END;
  RETURN i;
END X2C_LENGTH;

<* END *>

END xrStrings.
