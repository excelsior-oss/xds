(* Copyright (c) xTech 1993, 94. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE LongMath;

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
  Strings.Concat("LongMath.",s,m);
<* IF EXCEPTIONS THEN *>
  EXCEPTIONS.RAISE(source,n,m);
<* ELSE *>
  XRaise.RAISE(XRaise.RealMath,m);
<* END *>
END raise;

PROCEDURE sqrt(x: LONGREAL): LONGREAL;
BEGIN
  IF x<0. THEN raise(0,"sqrt: negative argument") END;
  RETURN math.X2C_sqrtl(x);
END sqrt;

PROCEDURE exp(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_expl(x)
END exp;

PROCEDURE ln(x: LONGREAL): LONGREAL;
BEGIN
  IF x<=0. THEN raise(1,"ln: negative or zero argument") END;
  RETURN math.X2C_lnl(x)
END ln;

PROCEDURE sin(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_sinl(x)
END sin;

PROCEDURE cos(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_cosl(x)
END cos;

PROCEDURE tan(x: LONGREAL): LONGREAL;
BEGIN
<* IF EXCEPTIONS THEN *>
  (* ODD miltiple of PI *)
<* END *>
  RETURN math.X2C_tanl(x)
END tan;

PROCEDURE arcsin(x: LONGREAL): LONGREAL;
BEGIN
  IF ABS(x)>1. THEN raise(2,"arcsin: argument is not in range -1.0 .. 1.0") END;
  RETURN math.X2C_arcsinl(x)
END arcsin;

PROCEDURE arccos(x: LONGREAL): LONGREAL;
BEGIN
  IF ABS(x)>1. THEN raise(3,"arccos: argument is not in range -1.0 .. 1.0") END;
  RETURN math.X2C_arccosl(x)
END arccos;

PROCEDURE arctan(x: LONGREAL): LONGREAL;
BEGIN
  RETURN math.X2C_arctanl(x)
END arctan;

PROCEDURE power(base, exponent: LONGREAL): LONGREAL;
BEGIN
  IF base<=0. THEN raise(4,"power: negative or zero base") END;
  RETURN math.X2C_powl(base,exponent)
END power;

PROCEDURE round(x: LONGREAL): INTEGER;
BEGIN
  x:=math.X2C_floorl(x);
  IF (x < LFLOAT(MIN(INTEGER))) OR (x > LFLOAT(MAX(INTEGER))) THEN
    raise(5,"round: integer overflow")
  END;
  RETURN INT(x)
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
END LongMath.
