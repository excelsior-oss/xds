(* Copyright (c) xTech 1994. All Rights Reserved. *)
IMPLEMENTATION MODULE LongComplexMath;

IMPORT  EXCEPTIONS;

(* ---- this part should be redefined when copy to LongComplexMath ---- *)

<*+ M2EXTENSIONS *>
IMPORT  math:=LongMath;
<*- M2EXTENSIONS *>

CONST name = "LongComplexMath.";

TYPE
  complex = LONGCOMPLEX;
  real    = LONGREAL;

(*----------------------------------------------------------------------*)

CONST pi = math.pi;

VAR source: EXCEPTIONS.ExceptionSource;

PROCEDURE abs (z: complex): real;
BEGIN
  RETURN math.sqrt( RE(z)*RE(z)+IM(z)*IM(z) )
END abs;

PROCEDURE arg (z: complex): real;
  VAR re,im: real;
BEGIN
  re:=RE(z);
  im:=IM(z);
  IF    re > 0. THEN
    RETURN math.arctan(im/re);
  ELSIF re < 0. THEN
    IF IM(z) >= 0. THEN RETURN  math.arctan(im/re) + pi;
    ELSE                RETURN  math.arctan(im/re) - pi;
    END;
  ELSIF im = 0. THEN
    EXCEPTIONS.RAISE(source,0,name+"arg: zero argument");
  ELSIF im > 0. THEN
    RETURN  pi / 2.0;
  ELSE
    RETURN -pi /2.0;
  END;
END arg;

PROCEDURE conj (z: complex): complex;
BEGIN
  RETURN CMPLX(RE(z),-IM(z))
END conj;

PROCEDURE power (base: complex; exponent: real): complex;
  VAR x, y: real;
BEGIN
  IF (base = zero) & (exponent > 0.) THEN RETURN zero END;
  x:=math.power(abs(base),exponent);
  y:=exponent*arg(base);
  RETURN CMPLX( x*math.cos(y), x*math.sin(y) )
END power;

PROCEDURE sqrt (z: complex): complex;
  VAR x, y: real;
BEGIN
  IF z=zero THEN RETURN z END;
  x:=math.sqrt(abs(z));
  y:=arg(z) / 2.;
  RETURN CMPLX( x*math.cos(y), x*math.sin(y) )
END sqrt;

PROCEDURE exp (z: complex): complex;
  VAR x: real;
BEGIN
  x:=math.exp(RE(z));
  RETURN CMPLX( x*math.cos(IM(z)), x*math.sin(IM(z)) )
END exp;

PROCEDURE ln (z: complex): complex;
BEGIN
  RETURN CMPLX( math.ln(abs(z)), arg(z))
END ln;

PROCEDURE sin (z: complex): complex;
  VAR eb, emb: real;
BEGIN
  eb:=math.exp(IM(z));
  emb:=1. / eb;
  RETURN CMPLX(
                     math.sin(RE(z))*(emb+eb)/2.,
                -0.5*math.cos(RE(z))*(emb-eb)
              )
END sin;

PROCEDURE cos (z: complex): complex;
  VAR eb, emb: real;
BEGIN
  eb:=math.exp(IM(z));
  emb:=1. / eb;
  RETURN CMPLX(
                     math.cos(RE(z))*(emb+eb)/2.,
                     math.sin(RE(z))*(emb-eb)/2.
              )
END cos;

PROCEDURE tan (z: complex): complex;
  VAR sin,cos,eb,emb,x,y: real;
BEGIN
  IF z = zero THEN RETURN zero END;
  sin := math.sin(RE(z));
  cos := math.cos(RE(z));
  eb  := math.exp(IM(z));
  emb := 1. / eb;
  x:=cos*(emb+eb);
  y:=sin*(emb-eb);
  x:=x*x + y*y;
  RETURN CMPLX( 4.*sin*cos/x, (eb - emb)*(emb + eb)/x );
END tan;

PROCEDURE arcsin (z: complex): complex;
  VAR c: complex; phi, rp: real;
BEGIN
  c:=CMPLX( 1. - RE(z)*RE(z) + IM(z)*IM(z),  -2.*RE(z)*IM(z) );
  IF c=zero THEN phi:=0.0 ELSE phi:= arg(c) / 2. END;
  rp:= math.sqrt(abs(c));
  c:=CMPLX( -IM(z) + rp*math.cos(phi), RE(z) + rp*math.sin(phi));
  RETURN CMPLX( arg(c), - math.ln(abs(c)) )
END arcsin;

PROCEDURE arccos (z: complex): complex;
  VAR c: complex; phi, rp: real;
BEGIN
  c:=CMPLX( 1. - RE(z)*RE(z) + IM(z)*IM(z),  -2.*RE(z)*IM(z) );
  IF c=zero THEN phi:=0.0 ELSE phi:= arg(c) / 2. END;
  rp:= math.sqrt(abs(c));
  c:=CMPLX( RE(z) - rp*math.sin(phi), IM(z) + rp*math.cos(phi));
  RETURN CMPLX( arg(c), - math.ln(abs(c)) )
END arccos;

PROCEDURE arctan (z: complex): complex;
  VAR opb, a2, den: real; c: complex;
BEGIN
  opb := 1. + IM(z);
  a2 := RE(z)*RE(z);
  den := opb*opb + a2;
  c:=CMPLX( ((1.-IM(z))*opb - a2) / den, 2.*RE(z)/den);
  RETURN CMPLX( arg(c)/2., -math.ln(abs(c))/2. )
END arctan;

PROCEDURE polarToComplex (abs, arg: real): complex;
BEGIN
  RETURN CMPLX( abs*math.cos(arg), abs*math.sin(arg) )
END polarToComplex;

PROCEDURE scalarMult (scalar: real; z: complex): complex;
BEGIN
  RETURN CMPLX( RE(z)*scalar, IM(z)*scalar )
END scalarMult;

PROCEDURE IsCMathException (): BOOLEAN;
BEGIN
  RETURN EXCEPTIONS.IsCurrentSource(source);
END IsCMathException;

BEGIN
  EXCEPTIONS.AllocateSource(source);
END LongComplexMath.

