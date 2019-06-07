<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<*- gendebug    *> (* Don't enable! History would not work (SYSTEM.CODE) *)
IMPLEMENTATION MODULE xrcAri64;

FROM SYSTEM    IMPORT  CODE, INT64;
IMPORT  X2C, M2EXCEPTION;

PROCEDURE ["C"] X2C_REM64_F(a ["C"]: INT64; b ["C"]: INT64): INT64;
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
END X2C_REM64_F;

PROCEDURE ["C"] X2C_QUO64_F(a ["C"]: INT64; b ["C"]: INT64): INT64;
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
END X2C_QUO64_F;

PROCEDURE ["C"] X2C_MOD64_F(a ["C"]: INT64; b ["C"]: INT64): INT64;
  VAR c: INT64;
BEGIN
  IF b<=0 THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.wholeDivException)) END;
  CODE("c = (a % b);");
  IF (a<0) & (c<0) THEN
    c := c+b;
  END;
  RETURN c;
END X2C_MOD64_F;

PROCEDURE ["C"] X2C_DIV64_F(a ["C"]: INT64; b ["C"]: INT64): INT64;
  VAR c: INT64;
BEGIN
  IF b<=0 THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.wholeDivException)) END;
  CODE("c = (a/b);");
  IF (a<0) & (c*b>a) THEN c:=c-1 END;
  RETURN c;
END X2C_DIV64_F;

PROCEDURE ["C"] X2C_ABS_INT64(x: INT64): INT64;
BEGIN
  IF x>=0 THEN RETURN x END;
  IF x=MIN(INT64) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END; 
  RETURN -x;
END X2C_ABS_INT64;

END xrcAri64.
