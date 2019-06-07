IMPLEMENTATION MODULE MathLib0 ;
(* As defined in PIM *)

IMPORT  RealMath;

PROCEDURE sqrt(x: REAL): REAL;
BEGIN
  RETURN RealMath.sqrt(x)
END sqrt;

PROCEDURE exp(x: REAL): REAL;
BEGIN
  RETURN RealMath.exp(x)
END exp;

PROCEDURE ln(x: REAL): REAL;
BEGIN
  RETURN RealMath.ln(x)
END ln;

PROCEDURE sin(x: REAL): REAL;
BEGIN
  RETURN RealMath.sin(x)
END sin;

PROCEDURE cos(x: REAL): REAL;
BEGIN
  RETURN RealMath.cos(x)
END cos;

PROCEDURE arctan(x: REAL): REAL;
BEGIN
  RETURN RealMath.arctan(x)
END arctan;

PROCEDURE entier(x: REAL): INTEGER;
BEGIN
  RETURN INT(x)
END entier;

END MathLib0.
