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
IMPLEMENTATION MODULE xrStrCmp;

IMPORT  SYSTEM, RTS:=xmRTS;
FROM SYSTEM    IMPORT  ADDADR, size_t, int;

PROCEDURE [2] X2C_STRCMP_PROC(x: RTS.X2C_pVOID; alen: size_t;
			      y: RTS.X2C_pVOID; blen: size_t): int;
  VAR a,b: RTS.X2C_pCHAR; i,m: size_t;
BEGIN
  a:=SYSTEM.CAST(RTS.X2C_pCHAR,x);
  b:=SYSTEM.CAST(RTS.X2C_pCHAR,y);
  m:=alen;
  IF m > blen THEN m:=blen END;
  i:=0;
  WHILE (i<m) & (a^#0C) & (a^=b^) DO
    a:=ADDADR(a,SIZE(a^));
    b:=ADDADR(b,SIZE(b^));
    INC(i);
  END;
  IF i>=m THEN
    IF (i<alen) & (a^=0C) THEN alen:=i END;
    IF (i<blen) & (b^=0C) THEN blen:=i END;
    IF alen<blen THEN RETURN -1 END;
    IF alen>blen THEN RETURN  1 END;
    RETURN 0;
  END;
  IF a^<b^ THEN RETURN -1 END;
  IF a^>b^ THEN RETURN 1 END;
  RETURN 0;
END X2C_STRCMP_PROC;

END xrStrCmp.
