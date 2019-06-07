(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<* +M2EXTENSIONS*>
IMPLEMENTATION MODULE xMathAds; (* Hady. 01.06.96 18:26 *)

IMPORT  math:=xMath;

PROCEDURE ["StdCall"] X2C_arcsin(x: REAL): REAL;
BEGIN
  RETURN math.X2C_arctan2 (x, math.X2C_sqrt(1-x*x))
END X2C_arcsin;

PROCEDURE ["StdCall"] X2C_arccos(x: REAL): REAL;
BEGIN
  RETURN math.X2C_arctan2 (math.X2C_sqrt(1-x*x), x)
END X2C_arccos;

PROCEDURE ["StdCall"] X2C_arcsinl(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_arctan2l (x, math.X2C_sqrtl(1-x*x))
END X2C_arcsinl;

PROCEDURE ["StdCall"] X2C_arccosl(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_arctan2l (math.X2C_sqrtl(1-x*x), x)
END X2C_arccosl;

PROCEDURE ["StdCall"] X2C_pow(base,exponent: REAL): REAL;
BEGIN
  RETURN math.X2C_exp(exponent*math.X2C_ln(base));
END X2C_pow;

PROCEDURE ["StdCall"] X2C_powl(base,exponent: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_expl(exponent*math.X2C_lnl(base));
END X2C_powl;

PROCEDURE ["StdCall"] X2C_sinh(x: REAL): REAL;
BEGIN
  RETURN 0.5*(math.X2C_exp(x)-math.X2C_exp(-x));
END X2C_sinh;

PROCEDURE ["StdCall"] X2C_sinhl(x: LONGREAL): LONGREAL;
BEGIN
  (* (exp(x)-exp(-x)) / 2 *)
  RETURN 0.5*(math.X2C_expl(x)-math.X2C_expl(-x));
END X2C_sinhl;

PROCEDURE ["StdCall"] X2C_cosh(x: REAL): REAL;
BEGIN
  (* (exp(x)+exp(-x)) / 2 *)
  RETURN 0.5*(math.X2C_exp(x)+math.X2C_exp(-x));
END X2C_cosh;

PROCEDURE ["StdCall"] X2C_coshl(x: LONGREAL): LONGREAL;
BEGIN
  (* (exp(x)+exp(-x)) / 2 *)
  RETURN 0.5*(math.X2C_expl(x)+math.X2C_expl(-x));
END X2C_coshl;

PROCEDURE ["StdCall"] X2C_tanh(x: REAL): REAL;
  VAR ep,em: REAL;
BEGIN
  ep:=math.X2C_exp(x);
  em:=math.X2C_exp(-x);
  RETURN (ep-em)/(ep+em);
END X2C_tanh;

PROCEDURE ["StdCall"] X2C_tanhl(x: LONGREAL): LONGREAL;
  VAR ep,em: LONGREAL;
BEGIN
  ep:=math.X2C_expl(x);
  em:=math.X2C_expl(-x);
  RETURN (ep-em)/(ep+em);
END X2C_tanhl;

PROCEDURE ["StdCall"] X2C_arcsinh(x: REAL): REAL;
BEGIN
  RETURN math.X2C_ln(x+math.X2C_sqrt(x*x+1))
END X2C_arcsinh;

PROCEDURE ["StdCall"] X2C_arcsinhl(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_lnl(x+math.X2C_sqrtl(x*x+1))
END X2C_arcsinhl;

PROCEDURE ["StdCall"] X2C_arccosh(x: REAL): REAL;
BEGIN
  RETURN math.X2C_ln(x+math.X2C_sqrt(x*x-1.0))
END X2C_arccosh;

PROCEDURE ["StdCall"] X2C_arccoshl(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_lnl(x+math.X2C_sqrtl(x*x-1.0))
END X2C_arccoshl;

PROCEDURE ["StdCall"] X2C_arctanh(x: REAL): REAL;
BEGIN
  RETURN 0.5*math.X2C_ln((1.0+x)/(1.0-x))
END X2C_arctanh;

PROCEDURE ["StdCall"] X2C_arctanhl(x: LONGREAL): LONGREAL;
BEGIN
  RETURN 0.5*math.X2C_lnl((1.0+x)/(1.0-x))
END X2C_arctanhl;

END xMathAds.
