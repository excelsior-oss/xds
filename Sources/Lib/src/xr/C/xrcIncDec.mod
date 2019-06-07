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
IMPLEMENTATION MODULE xrcIncDec;

FROM SYSTEM    IMPORT  INT8,INT16,INT32,CARD8,CARD16,CARD32;
IMPORT  X2C, M2EXCEPTION;

PROCEDURE [2] X2C_INCC(VAR x: CHAR; y: CARD8; min,max: CHAR): CHAR;
  VAR i: INT16;
BEGIN
  i:=VAL(INT16,x)+VAL(INT16,y);
  IF (i<INT(ORD(min))) OR (i>INT(ORD(max))) THEN
    X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException))
  END;
  x:=CHR(i);
  RETURN x;
END X2C_INCC;

PROCEDURE [2] X2C_INCS(VAR x: INT8; y,min,max: INT8): INT8;
  VAR i: INT16;
BEGIN
  i:=VAL(INT16,x)+VAL(INT16,y);
  IF (i<min) OR (i>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=i;
  RETURN x;
END X2C_INCS;

PROCEDURE [2] X2C_INCI(VAR x: INT16; y,min,max: INT16): INT16;
  VAR i: INT32;
BEGIN
  i:=VAL(INT32,x)+VAL(INT32,y);
  IF (i<min) OR (i>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=i;
  RETURN x;
END X2C_INCI;

PROCEDURE [2] X2C_INC(VAR x: INT32; y,min,max: INT32): INT32;
  VAR i: INT32;
BEGIN
  IF y>0 THEN
    IF MAX(INT32)-y < x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  ELSE
    IF MIN(INT32)-y > x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  END;
  i:=x+y;
  IF (i<min) OR (i>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=i;
  RETURN x;
END X2C_INC;

PROCEDURE [2] X2C_INCUS(VAR x: CARD8; y,min,max: CARD8): CARD8;
  VAR i: CARD16;
BEGIN
  i:=VAL(CARD16,x)+VAL(CARD16,y);
  IF (i<min) OR (i>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=i;
  RETURN x;
END X2C_INCUS;

PROCEDURE [2] X2C_INCUI(VAR x: CARD16; y,min,max: CARD16): CARD16;
  VAR i: CARD32;
BEGIN
  i:=VAL(CARD32,x)+VAL(CARD32,y);
  IF (i<min) OR (i>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=i;
  RETURN x;
END X2C_INCUI;

PROCEDURE [2] X2C_INCU(VAR x: CARD32; y,min,max: CARD32): CARD32;
  VAR i: CARD32;
BEGIN
  IF MAX(CARD32)-y < x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  i:=x+y;
  IF (i<min) OR (i>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=i;
  RETURN x;
END X2C_INCU;

PROCEDURE [2] X2C_DECC(VAR x: CHAR; y: CARD8; min,max: CHAR): CHAR;
  VAR i: INT16;
BEGIN
  i:=VAL(INT16,x)-VAL(INT16,y);
  IF (i<INT(ORD(min))) OR (i>INT(ORD(max))) THEN
    X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException))
  END;
  x:=CHR(i);
  RETURN x;
END X2C_DECC;

PROCEDURE [2] X2C_DECS(VAR x: INT8; y,min,max: INT8): INT8;
  VAR i: INT16;
BEGIN
  i:=VAL(INT16,x)-VAL(INT16,y);
  IF (i<min) OR (i>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=i;
  RETURN x;
END X2C_DECS;

PROCEDURE [2] X2C_DECI(VAR x: INT16; y,min,max: INT16): INT16;
  VAR i: INT32;
BEGIN
  i:=VAL(INT32,x)-VAL(INT32,y);
  IF (i<min) OR (i>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=i;
  RETURN x;
END X2C_DECI;

PROCEDURE [2] X2C_DEC(VAR x: INT32; y,min,max: INT32): INT32;
BEGIN
  IF y<0 THEN
    IF MAX(INT32)+y < x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  ELSE
    IF MIN(INT32)+y > x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  END;
  x:=x-y;
  IF (x<min) OR (x>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  RETURN x;
END X2C_DEC;

PROCEDURE [2] X2C_DECUS(VAR x: CARD8; y,min,max: CARD8): CARD8;
BEGIN
  IF y>x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=x-y;
  IF (x<min) OR (x>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  RETURN x;
END X2C_DECUS;

PROCEDURE [2] X2C_DECUI(VAR x: CARD16; y,min,max: CARD16): CARD16;
BEGIN
  IF y>x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=x-y;
  IF (x<min) OR (x>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  RETURN x;
END X2C_DECUI;

PROCEDURE [2] X2C_DECU(VAR x: CARD32; y,min,max: CARD32): CARD32;
  VAR i: CARD32;
BEGIN
  IF y>x THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  i:=x-y;
  IF (i<min) OR (i>max) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  x:=i;
  RETURN x;
END X2C_DECU;

END xrcIncDec.
