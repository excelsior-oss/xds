<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<*- gendebug    *> (* Don enable! History would not work (SYSTEM.CODE) *)
IMPLEMENTATION MODULE xrAReal;

IMPORT
	SYSTEM
	,X2C
	,M2EXCEPTION;

FROM SYSTEM    IMPORT  INT32, CARD32;
FROM SYSTEM    IMPORT  CODE;

PROCEDURE [2] X2C_ENTIER(x: LONGREAL): INT32;
  VAR i: INT32;
BEGIN
  IF (x<VAL(LONGREAL,MIN(INT32))) OR (x>VAL(LONGREAL,MAX(INT32))) THEN
    X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException));
  END;
  CODE("i=(long)x;");
  IF VAL(LONGREAL,i)>x THEN DEC(i) END;
  RETURN i;
END X2C_ENTIER;

PROCEDURE [2] X2C_TRUNCI(x: LONGREAL; min,max: INT32): INT32;
  VAR i: INT32;
BEGIN
  IF (x<VAL(LONGREAL,min)) OR (x>VAL(LONGREAL,max)) THEN
    X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException));
  END;
  CODE("i=(long)x;");
  IF x>0.0 THEN
    IF VAL(LONGREAL,i)>x THEN DEC(i) END;
  ELSE
    IF VAL(LONGREAL,i)<x THEN INC(i) END;
  END;
  RETURN i;
END X2C_TRUNCI;

PROCEDURE [2] X2C_TRUNCC(x: LONGREAL; min,max: CARD32): CARD32;
  VAR i: CARD32;
BEGIN
  IF (x<VAL(LONGREAL,min)) OR (x>VAL(LONGREAL,max)) THEN
    X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException));
  END;
  CODE("i=(unsigned long)x;");
  IF VAL(LONGREAL,i)>x THEN DEC(i) END;
  RETURN i;
END X2C_TRUNCC;

(* This function seems to be unused from anywhere.
             Hady, 28.05.96 13:37
PROCEDURE [2] X2C_VAL_REAL(x: LONGREAL): REAL;
  VAR i: REAL;
BEGIN
  IF (x<VAL(LONGREAL,MIN(REAL))) OR (x>VAL(LONGREAL,MAX(REAL))) THEN
    X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException));
  END;
  CODE("i=(X2C_REAL)x;");
  RETURN i;
END X2C_VAL_REAL;
*)

END xrAReal.
