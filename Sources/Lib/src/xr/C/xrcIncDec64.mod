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
IMPLEMENTATION MODULE xrcIncDec64;

FROM SYSTEM    IMPORT  INT64,CARD64;
IMPORT  X2C, M2EXCEPTION;

PROCEDURE ["C"] X2C_INC64(VAR x: INT64; y,min,max: INT64): INT64;
  VAR i: INT64;
BEGIN
  IF y>0 THEN
    IF MAX(INT64)-y < x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  ELSE
    IF MIN(INT64)-y > x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  END;
  i:=x+y;
  IF (i<min) OR (i>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=i;
  RETURN x;
END X2C_INC64;

PROCEDURE ["C"] X2C_INCU64(VAR x: CARD64; y,min,max: CARD64): CARD64;
  VAR i: CARD64;
BEGIN
  IF MAX(CARD64)-y < x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  i:=x+y;
  IF (i<min) OR (i>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=i;
  RETURN x;
END X2C_INCU64;


PROCEDURE ["C"] X2C_DEC64(VAR x: INT64; y,min,max: INT64): INT64;
BEGIN
  IF y<0 THEN
    IF MAX(INT64)+y < x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  ELSE
    IF MIN(INT64)+y > x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  END;
  x:=x-y;
  IF (x<min) OR (x>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  RETURN x;
END X2C_DEC64;


PROCEDURE ["C"] X2C_DECU64(VAR x: CARD64; y,min,max: CARD64): CARD64;
  VAR i: CARD64;
BEGIN
  IF y>x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  i:=x-y;
  IF (i<min) OR (i>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=i;
  RETURN x;
END X2C_DECU64;

END xrcIncDec64.
