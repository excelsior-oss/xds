(* Copyright (C) 1996 XTech LTD *)

<* +M2ADDTYPES *>

IMPLEMENTATION MODULE MATHLIB;

IMPORT SYSTEM;

IMPORT LongMath, xMath;

PROCEDURE Pow(X,Y : LONGREAL): LONGREAL;
BEGIN
  IF Y = 0.0 THEN
    RETURN 1.0;
  END;
  IF X >= 0.0 THEN
    RETURN LongMath.power(X,Y);
  ELSIF VAL(LONGREAL,VAL(INTEGER,Y)) = Y THEN
    RETURN IntPow (X,VAL(INTEGER,Y));
  ELSE 
    RETURN 0.0;
  END;
END Pow;

PROCEDURE SinH(A : LONGREAL) : LONGREAL;
BEGIN
  RETURN (LongMath.exp(A)-LongMath.exp(-A))/2.0;
END SinH;

PROCEDURE CosH(A : LONGREAL) : LONGREAL;
BEGIN
  RETURN (LongMath.exp(A)+LongMath.exp(-A))/2.0;
END CosH;

PROCEDURE TanH(A : LONGREAL) : LONGREAL;
VAR ex, emx :LONGREAL;
BEGIN
  ex  := LongMath.exp(A);
  emx := LongMath.exp(-A);
  RETURN (ex-emx)/(ex+emx);
END TanH;

PROCEDURE Log10(A : LONGREAL) : LONGREAL;
BEGIN
  RETURN LongMath.ln(A)/M_Ln10;
END Log10;


(*************************************************************************
Original TopSpeed comments from coremath.a
double _fmod(double X,double Y)
LONGDOUBLE _fmodl(LONGDOUBLE X,LONGDOUBLE Y)
Return: X % Y or 0 if Y is 0
Error: no error handling
**************************************************************************)
PROCEDURE Mod(X,Y : LONGREAL) :LONGREAL;
BEGIN
  IF Y = 0.0 THEN  RETURN 0.0  END;
  RETURN xMath.X2C_reml(X, Y);
END Mod;


PROCEDURE Rexp(VAR I : INTEGER;A : LONGREAL) :LONGREAL;
VAR e: SYSTEM.int; 
    m: LONGREAL;
BEGIN
  m := xMath.X2C_frexp(A,e);
  I := e;
  RETURN m;
END Rexp;

(* This is a specification of ATan2 :-)


PROCEDURE ATan2 ( x, y :LONGREAL ) :LONGREAL;
CONST
  delta = 1.0E-9;
VAR
  arct :LONGREAL;
BEGIN
  IF (ABS(x) < delta) & (ABS(y) >= delta) THEN
      IF ( y>0.0 ) THEN
        RETURN M_PiBy2
      ELSE
        RETURN -M_PiBy2
      END;
  END;
  IF (ABS(y) < delta) & (ABS(x) >= delta) & ( x<0.0 ) THEN
    RETURN M_Pi
  END;
  arct := LongMath.arctan(ABS(y)/ABS(x));
  IF ( x>0.0 ) THEN
    IF ( y>0.0 ) THEN
      RETURN arct
    ELSE
      RETURN -arct
    END;
  ELSIF ( y>0.0 ) THEN
       RETURN M_Pi-arct
     ELSE 
       RETURN arct-M_Pi
  END;
END ATan2;
*)

PROCEDURE ATan2 ( x, y :LONGREAL ) :LONGREAL;
BEGIN
  RETURN xMath.X2C_arctan2l(y,x);
END ATan2;

PROCEDURE IntPow(X : LONGREAL; K : INTEGER) : LONGREAL;
VAR
  res: LONGREAL;
  P : INTEGER;
BEGIN
  (* TS does not check that X <> 0.0, so do we *)
  IF K = 0 THEN RETURN 1.0 END;
  P := ABS(K);
  res := 1.0;
(* real overflow exception is involved by IntPow(1.7976931348623000E+308,1)
  WHILE P > 0 DO
    IF ODD(P) THEN res := res*X END;
    X := X*X;
    P := P DIV 2;
  END; 
*)
  LOOP
    IF ODD(P) THEN res := res*X END;
    P := P DIV 2;
    IF P = 0 THEN EXIT END;
    X := X*X;
  END;
  IF K < 0 THEN res := 1.0/res END;
  RETURN res;
END IntPow;

END MATHLIB.