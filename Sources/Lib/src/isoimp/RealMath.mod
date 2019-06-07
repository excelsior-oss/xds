(* Copyright (c) xTech 1993, 94. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE RealMath;

(* Modifications:
   22-Mar-94 Ned: merging implementations
*)

IMPORT Strings;

IMPORT  math:=xMath;

<* IF EXCEPTIONS THEN *>  IMPORT  EXCEPTIONS;
<* ELSE *>                IMPORT  XRaise;
<* END *>

<* IF EXCEPTIONS THEN *>
  VAR source: EXCEPTIONS.ExceptionSource;
<* END *>

PROCEDURE raise(n: CARDINAL; s-: ARRAY OF CHAR);
  VAR m: ARRAY [0..79] OF CHAR;
BEGIN
  Strings.Concat("RealMath.",s,m);
<* IF EXCEPTIONS THEN *>
  EXCEPTIONS.RAISE(source,n,m);
<* ELSE *>
  XRaise.RAISE(XRaise.RealMath,m);
<* END *>
END raise;

PROCEDURE sqrt(x: REAL): REAL;
BEGIN
  IF x<0. THEN raise(0,"sqrt: negative argument") END;
  RETURN math.X2C_sqrt(x);
END sqrt;

PROCEDURE exp(x: REAL): REAL;
BEGIN
  RETURN math.X2C_exp(x);
END exp;

PROCEDURE ln(x: REAL): REAL;
BEGIN
  IF x<=0. THEN raise(1,"ln: negative or zero argument") END;
  RETURN math.X2C_ln(x);
END ln;

PROCEDURE sin(x: REAL): REAL;
BEGIN
  RETURN math.X2C_sin(x);
END sin;

PROCEDURE cos(x: REAL): REAL;
BEGIN
  RETURN math.X2C_cos(x);
END cos;

PROCEDURE tan(x: REAL): REAL;
BEGIN
<* IF EXCEPTIONS THEN *>
  (* ODD miltiple of PI *)
<* END *>
  RETURN math.X2C_tan(x);
END tan;

PROCEDURE arcsin(x: REAL): REAL;
BEGIN
  IF ABS(x)>1. THEN raise(2,"arcsin: argument is not in range -1.0 .. 1.0") END;
  RETURN math.X2C_arcsin(x)
END arcsin;

PROCEDURE arccos(x: REAL): REAL;
BEGIN
  IF ABS(x)>1. THEN raise(3,"arccos: argument is not in range -1.0 .. 1.0") END;
  RETURN math.X2C_arccos(x)
END arccos;

PROCEDURE arctan(x: REAL): REAL;
BEGIN
  RETURN math.X2C_arctan(x);
END arctan;

PROCEDURE power(base, exponent: REAL): REAL;
BEGIN
  IF base<=0. THEN raise(4,"power: negative or zero base") END;
  RETURN math.X2C_pow(base,exponent)
END power;

PROCEDURE round(x: REAL): INTEGER;
  VAR y: LONGREAL;
BEGIN
  y:=math.X2C_floorl(LFLOAT(x));
  IF (y < LFLOAT(MIN(INTEGER))) OR (y > LFLOAT(MAX(INTEGER))) THEN
    raise(5,"round: integer overflow")
  END;
  RETURN INT(y)
END round;

PROCEDURE IsRMathException(): BOOLEAN;
BEGIN
<* IF EXCEPTIONS THEN *>
  RETURN EXCEPTIONS.IsCurrentSource(source)
<* ELSE *>
  RETURN FALSE
<* END *>
END IsRMathException;

BEGIN
<* IF EXCEPTIONS THEN *>
  EXCEPTIONS.AllocateSource(source);
<* END *>
END RealMath.
