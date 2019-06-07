(** Oakwood Oberon-2 Math library *)
(** Mathematical functions for the type LONGREAL *)
(** Copyright (c) xTech 1994,95. All Rights Reserved. *)
<*+ O2EXTENSIONS *>
<*+ O2NUMEXT *>
MODULE MathL;

IMPORT
<* IF EXCEPTIONS THEN *>  EXCEPTIONS,
<* ELSE *>                XRaise,
<* END *>
  math:=xMath,
  Strings;

CONST
  pi* = 3.14159265358979323846;
  e*  = 2.71828182845904523536;

<* IF EXCEPTIONS THEN *>
  VAR source: EXCEPTIONS.ExceptionSource;
<* END *>

PROCEDURE raise(n: INTEGER; s-: ARRAY OF CHAR);
  VAR m: ARRAY 80 OF CHAR;
BEGIN
  Strings.Concat("MathL.",s,m);
<* IF EXCEPTIONS THEN *>
  EXCEPTIONS.RAISE(source,n,m);
<* ELSE *>
  XRaise.RAISE(XRaise.RealMath,m);
<* END *>
END raise;

PROCEDURE sqrt*(x: LONGREAL): LONGREAL;
BEGIN
  IF x<0. THEN raise(0,"sqrt: negative argument") END;
  RETURN math.X2C_sqrtl(x)
END sqrt;

PROCEDURE exp*(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_expl(x)
END exp;

PROCEDURE ln*(x: LONGREAL): LONGREAL;
BEGIN
  IF x<=0. THEN raise(1,"ln: negative or zero argument") END;
  RETURN math.X2C_lnl(x)
END ln;

PROCEDURE sin*(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_sinl(x)
END sin;

PROCEDURE cos*(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_cosl(x)
END cos;

PROCEDURE tan*(x: LONGREAL): LONGREAL;
BEGIN
<* IF EXCEPTIONS THEN *>
  (* ODD miltiple of PI *)
<* END *>
  RETURN math.X2C_tanl(x)
END tan;

PROCEDURE arcsin*(x: LONGREAL): LONGREAL;
BEGIN
  IF ABS(x)>1. THEN raise(2,"arcsin: argument is not in range -1.0 .. 1.0") END;
  RETURN math.X2C_arcsinl(x)
END arcsin;

PROCEDURE arccos*(x: LONGREAL): LONGREAL;
BEGIN
  IF ABS(x)>1. THEN raise(3,"arccos: argument is not in range -1.0 .. 1.0") END;
  RETURN math.X2C_arccosl(x)
END arccos;

PROCEDURE arctan*(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_arctanl(x)
END arctan;

PROCEDURE power*(base, exponent: LONGREAL): LONGREAL;
BEGIN
  IF base<=0. THEN raise(4,"power: negative or zero base") END;
  RETURN math.X2C_powl(base,exponent)
END power;

(*-----------------------------------------------------------*)
(* The following functions are absent in ISO M2 Math library *)
(* or have different interface as "round".                   *)
(*-----------------------------------------------------------*)

PROCEDURE round*(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_floorl(x+0.5);
END round;

PROCEDURE log*(x,base: LONGREAL): LONGREAL;
BEGIN
  IF base<=0 THEN raise(5,"log: negative or zero base") END;
  IF x<=0 THEN raise(6,"log: negative or zero argument") END;
  RETURN math.X2C_lnl(x)/math.X2C_lnl(base)
END log;

PROCEDURE arctan2*(x,y: LONGREAL): LONGREAL;
BEGIN
  IF (x=0) & (y=0) THEN raise(7,"arctan2: zero arguments") END;
  RETURN math.X2C_arctan2l(y,x);
END arctan2;

PROCEDURE sinh*(x: LONGREAL): LONGREAL;
BEGIN
(* (exp(x)-exp(-x)) / 2 *)
  RETURN math.X2C_sinhl(x)
END sinh;

PROCEDURE cosh*(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_coshl(x)
END cosh;

PROCEDURE tanh*(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_tanhl(x)
END tanh;

PROCEDURE arcsinh*(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_arcsinhl(x);
END arcsinh;

PROCEDURE arccosh*(x: LONGREAL): LONGREAL;
BEGIN
  IF x<1 THEN raise(8,"arccosh: argument is less then 1.0") END;
  RETURN math.X2C_arccoshl(x);
END arccosh;

PROCEDURE arctanh*(x: LONGREAL): LONGREAL;
BEGIN
  IF (x<0) OR (x>=1) THEN raise(9,"arctanh: argument is not in range 0.0 .. 1.0") END;
  RETURN math.X2C_arctanhl(x);
END arctanh;

BEGIN
<* IF EXCEPTIONS THEN *>
  EXCEPTIONS.AllocateSource(source);
<* END *>
END MathL.
