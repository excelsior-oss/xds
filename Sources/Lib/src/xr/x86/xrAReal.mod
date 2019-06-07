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


(* some definitions of assembler funcs *)
PROCEDURE ["StdCall"] / X2C_longl(x: LONGREAL): INTEGER;
PROCEDURE ["StdCall"] / X2C_ulongl(x: LONGREAL): CARDINAL;


PROCEDURE [2] X2C_ENTIER(x: LONGREAL): INT32;
  VAR i: INT32;
BEGIN
  IF (x<VAL(LONGREAL,MIN(INT32))) OR (x>VAL(LONGREAL,MAX(INT32))) THEN
    X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException));
  END;
  i:=X2C_longl(x);
  IF VAL(LONGREAL,i)>x THEN DEC(i) END;
  RETURN i;
END X2C_ENTIER;

PROCEDURE [2] X2C_TRUNCI(x: LONGREAL; min,max: INT32): INT32;
  VAR i: INT32;
BEGIN
  IF (x<VAL(LONGREAL,min)-0.5) OR (x>VAL(LONGREAL,max)+0.5) THEN
    X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException));
  END;
  i:=X2C_longl(x);
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
  IF (x<VAL(LONGREAL,min)-0.5) OR (x>VAL(LONGREAL,max)+0.5) THEN
    X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException));
  END;
  i:=X2C_ulongl(x);
  IF VAL(LONGREAL,i)>x THEN DEC(i) END;
  RETURN i;
END X2C_TRUNCC;

END xrAReal.
