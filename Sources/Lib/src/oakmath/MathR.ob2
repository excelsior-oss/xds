(** Oakwood Oberon-2 Math library *)
(** Mathematical functions for the type REAL *)
(** Copyright (c) xTech 1994,95. All Rights Reserved. *)
<*+ O2NUMEXT *>
<*+ O2EXTENSIONS *>
MODULE MathR;

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
  Strings.Concat("MathR.",s,m);
<* IF EXCEPTIONS THEN *>
  EXCEPTIONS.RAISE(source,n,m);
<* ELSE *>
  XRaise.RAISE(XRaise.RealMath,m);
<* END *>
END raise;

PROCEDURE sqrt*(x: REAL): REAL;
BEGIN
  IF x<0. THEN raise(0,"sqrt: negative argument") END;
  RETURN math.X2C_sqrt(x);
END sqrt;

PROCEDURE exp*(x: REAL): REAL;
BEGIN
  RETURN math.X2C_exp(x);
END exp;

PROCEDURE ln*(x: REAL): REAL;
BEGIN
  IF x<=0. THEN raise(1,"ln: negative or zero argument") END;
  RETURN math.X2C_ln(x);
END ln;

PROCEDURE sin*(x: REAL): REAL;
BEGIN
  RETURN math.X2C_sin(x);
END sin;

PROCEDURE cos*(x: REAL): REAL;
BEGIN
  RETURN math.X2C_cos(x);
END cos;

PROCEDURE tan*(x: REAL): REAL;
BEGIN
<* IF EXCEPTIONS THEN *>
  (* ODD miltiple of PI *)
<* END *>
  RETURN math.X2C_tan(x);
END tan;

PROCEDURE arcsin*(x: REAL): REAL;
BEGIN
  IF ABS(x)>1. THEN raise(2,"arcsin: argument is not in range -1.0 .. 1.0") END;
  RETURN math.X2C_arcsin(x);
END arcsin;

PROCEDURE arccos*(x: REAL): REAL;
BEGIN
  IF ABS(x)>1. THEN raise(3,"arccos: argument is not in range -1.0 .. 1.0") END;
  RETURN math.X2C_arccos(x)
END arccos;

PROCEDURE arctan*(x: REAL): REAL;
BEGIN
  RETURN math.X2C_arctan(x);
END arctan;

PROCEDURE power*(base, exponent: REAL): REAL;
BEGIN
  IF base<=0. THEN raise(4,"power: negative or zero base") END;
  RETURN math.X2C_pow(base,exponent)
END power;

(*-----------------------------------------------------------*)
(* The following functions are absent in ISO M2 Math library *)
(* or have different interface as "round".                   *)
(*-----------------------------------------------------------*)

PROCEDURE round*(x: REAL): REAL;
BEGIN
  RETURN math.X2C_floor(x+0.5);
END round;

PROCEDURE log*(x,base: REAL): REAL;
BEGIN
  IF base<=0 THEN raise(5,"log: negative or zero base") END;
  IF x<=0 THEN raise(6,"log: negative or zero argument") END;
  RETURN math.X2C_ln(x)/math.X2C_ln(base)
END log;

PROCEDURE arctan2*(x,y: REAL): REAL;
BEGIN
  IF (x=0) & (y=0) THEN raise(7,"arctan2: zero arguments") END;
  RETURN math.X2C_arctan2(y,x);
END arctan2;

PROCEDURE sinh*(x: REAL): REAL;
BEGIN
(* (exp(x)-exp(-x)) / 2 *)
  RETURN math.X2C_sinh(x);
END sinh;

PROCEDURE cosh*(x: REAL): REAL;
BEGIN
(* (exp(x)+exp(-x)) / 2 *)
  RETURN math.X2C_cosh(x);
END cosh;

PROCEDURE tanh*(x: REAL): REAL;
BEGIN
(* sinh(x)/cosh(x) *)
  RETURN math.X2C_tanh(x);
END tanh;

PROCEDURE arcsinh*(x: REAL): REAL;
BEGIN
  RETURN math.X2C_arcsinh(x);
END arcsinh;

PROCEDURE arccosh*(x: REAL): REAL;
BEGIN
  IF x<1 THEN raise(8,"arccosh: argument is less then 1.0") END;
  RETURN math.X2C_arccosh(x);
END arccosh;

PROCEDURE arctanh*(x: REAL): REAL;
BEGIN
  IF (x<0) OR (x>=1) THEN raise(9,"arctanh: argument is not in range 0.0 .. 1.0") END;
  RETURN math.X2C_arctanh(x);
END arctanh;

BEGIN
<* IF EXCEPTIONS THEN *>
  EXCEPTIONS.AllocateSource(source);
<* END *>
END MathR.
