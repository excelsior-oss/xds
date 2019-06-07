<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<*- gendebug    *> (* Don enable! History would not work (SYSTEM.CODE) *)
IMPLEMENTATION MODULE xrcArythmetics;

FROM SYSTEM    IMPORT  CODE, INT8, INT16, INT32;
IMPORT  X2C, M2EXCEPTION;

PROCEDURE ["C"] X2C_REM_F(a ["C"]: INT32; b ["C"]: INT32): INT32;
BEGIN
  IF b=0 THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.wholeDivException)) END;
  IF a>=0 THEN
    IF b>0 THEN
      CODE("return (a%b);");
    ELSE
      CODE("return (a%(-b));");
    END;
  ELSE
    IF b>0 THEN
      CODE("return (-((-a)%b));");
    ELSE
      CODE("return (-(-a)%(-b));");
    END;
  END;
  RETURN 0;
END X2C_REM_F;

PROCEDURE ["C"] X2C_QUO_F(a ["C"]: INT32; b ["C"]: INT32): INT32;
BEGIN
  IF b=0 THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.wholeDivException)) END;
  IF a>=0 THEN
    IF b>0 THEN
      CODE("return (a/b);");
    ELSE
      CODE("return (-(a/(-b)));");
    END;
  ELSE
    IF b>0 THEN
      CODE("return (-((-a)/b));");
    ELSE
      CODE("return ((-a)/(-b));");
    END;
  END;
  RETURN 0;
END X2C_QUO_F;

PROCEDURE ["C"] X2C_MOD_F(a ["C"]: INT32; b ["C"]: INT32): INT32;
  VAR c: INT32;
BEGIN
  IF b<=0 THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.wholeDivException)) END;
  CODE("c = (a % b);");
  IF (a<0) & (c<0) THEN
    c := c+b;
  END;
  RETURN c;
END X2C_MOD_F;

PROCEDURE ["C"] X2C_DIV_F(a ["C"]: INT32; b ["C"]: INT32): INT32;
  VAR c: INT32;
BEGIN
  IF b<=0 THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.wholeDivException)) END;
  CODE("c = (a/b);");
  IF (a<0) & (c*b>a) THEN c:=c-1 END;
  RETURN c;
END X2C_DIV_F;

PROCEDURE ["C"] X2C_DIVR_F(a ["C"]: REAL; b ["C"]: REAL): REAL;
BEGIN
  IF b=0.0 THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.realDivException)) 
  ELSE
    CODE("return (a/b);");
  END;
  RETURN 0.0;
END X2C_DIVR_F;

PROCEDURE ["C"] X2C_DIVL_F(a ["C"]: LONGREAL; b ["C"]: LONGREAL): LONGREAL;
BEGIN
  IF b=0.0 THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.realDivException)) 
  ELSE
    CODE("return (a/b);");
  END;
  RETURN 0.0;
END X2C_DIVL_F;


PROCEDURE ["C"] X2C_ABS_INT8 (x: INT8 ): INT8;
BEGIN
  IF x>=0 THEN RETURN x END;
  IF x=MIN(INT8) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  RETURN -x;
END X2C_ABS_INT8;

PROCEDURE ["C"] X2C_ABS_INT16(x: INT16): INT16;
BEGIN
  IF x>=0 THEN RETURN x END;
  IF x=MIN(INT16) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  RETURN -x;
END X2C_ABS_INT16;

PROCEDURE ["C"] X2C_ABS_INT32(x: INT32): INT32;
BEGIN
  IF x>=0 THEN RETURN x END;
  IF x=MIN(INT32) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  RETURN -x;
END X2C_ABS_INT32;

END xrcArythmetics.
