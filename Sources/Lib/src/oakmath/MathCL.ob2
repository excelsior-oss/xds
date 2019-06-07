(** Oakwood Oberon-2 Math library *)
(** Copyright (c) xTech 1994. All Rights Reserved. *)
<*+ O2NUMEXT *>
<*+ O2EXTENSIONS *>
MODULE MathCL;

IMPORT
<* IF EXCEPTIONS THEN *>  EXCEPTIONS,
<* ELSE *>                XRaise,
<* END *>
  Strings,
  math:=MathL;

TYPE
  complex = LONGCOMPLEX;
  real    = LONGREAL;

(*----------------------------------------------------------------------*)

<* IF EXCEPTIONS THEN *>
  VAR source: EXCEPTIONS.ExceptionSource;
<* END *>

CONST
  i  = 1.i;
  zero = 0.i;

PROCEDURE raise(n: INTEGER; s-: ARRAY OF CHAR);
  VAR m: ARRAY 80 OF CHAR;
BEGIN
  Strings.Concat("MathCL.",s,m);
<* IF EXCEPTIONS THEN *>
  EXCEPTIONS.RAISE(source,n,m);
<* ELSE *>
  XRaise.RAISE(XRaise.LongMath,m);
<* END *>
END raise;

PROCEDURE abs*(z: complex): real;
BEGIN
  RETURN math.sqrt( RE(z)*RE(z)+IM(z)*IM(z) )
END abs;

PROCEDURE arg (z: complex): real;
BEGIN
  IF z = zero THEN RETURN 0.
  ELSE
    RETURN math.arctan2(RE(z),IM(z))
  END;
END arg;

PROCEDURE conj*(z: complex): complex;
BEGIN
  RETURN CMPLX(RE(z),-IM(z))
END conj;

PROCEDURE power*(base: complex; exponent: real): complex;
  VAR x, y: real;
BEGIN
  IF (base = zero) & (exponent > 0.) THEN RETURN zero END;
  x:=math.power(abs(base),exponent);
  y:=exponent*arg(base);
  RETURN CMPLX( x*math.cos(y), x*math.sin(y) )
END power;

PROCEDURE sqrt*(z: complex): complex;
  VAR x, y: real;
BEGIN
  x:=math.sqrt(abs(z));
  y:=arg(z) / 2.;
  RETURN CMPLX( x*math.cos(y), x*math.sin(y) )
END sqrt;

PROCEDURE exp*(z: complex): complex;
  VAR x: real;
BEGIN
  x:=math.exp(RE(z));
  RETURN CMPLX( x*math.cos(IM(z)), x*math.sin(IM(z)) )
END exp;

PROCEDURE ln*(z: complex): complex;
BEGIN
  RETURN CMPLX( math.ln(abs(z)), arg(z))
END ln;

PROCEDURE log*(z: complex; base: real): complex;
  VAR x: real;
BEGIN
  IF base<=0. THEN raise(0,"log: negative or zero base") END;
  x:=math.ln(base);
  RETURN CMPLX( math.ln(abs(z))/x, arg(z)/x)
END log;

PROCEDURE sin*(z: complex): complex;
  VAR x, y: LONGREAL;
BEGIN
  x:=math.exp(IM(z));
  y:=1. / x;
  RETURN CMPLX(
                     math.sin(RE(z))*(y+x)/2.,
                -0.5*math.cos(RE(z))*(y-x)
              )
END sin;

PROCEDURE cos*(z: complex): complex;
  VAR x, y: LONGREAL;
BEGIN
  x:=math.exp(IM(z));
  y:=1. / x;
  RETURN CMPLX(
                math.cos(RE(z))*(y+x)/2.,
                math.sin(RE(z))*(y-x)/2.
              )
END cos;

PROCEDURE tan*(z: complex): complex;
  VAR sin,cos,a,b: real; x,y: LONGREAL;
BEGIN
  IF z = zero THEN RETURN zero END;
  sin := math.sin(RE(z));
  cos := math.cos(RE(z));
  x   := math.exp(IM(z));
  y   := 1. / x;
  a:=cos*(x+y);
  b:=sin*(x-y);
  a:=a*a + b*b;
  RETURN CMPLX( 4.*sin*cos/a, ((x - y)*(x + y))/a );
END tan;

PROCEDURE arcsin*(z: complex): complex;
  VAR c: complex; phi, rp: real;
BEGIN
  ASSERT( ABS(RE(z)) <= 1.);
  c:=CMPLX( 1. - RE(z)*RE(z) + IM(z)*IM(z),  -2.*RE(z)*IM(z) );
  phi := arg(c) / 2.;
  rp := math.sqrt(abs(c));
  c:=CMPLX( -IM(z) + rp*math.cos(phi), RE(z) + rp*math.sin(phi));
  RETURN CMPLX( arg(c), - math.ln(abs(c)) )
END arcsin;

PROCEDURE arccos*(z: complex): complex;
  VAR c: complex; phi, rp: real;
BEGIN
  ASSERT( ABS(RE(z)) <= 1.);
  c:=CMPLX( 1. - RE(z)*RE(z) + IM(z)*IM(z),  -2.*RE(z)*IM(z) );
  phi := arg(c) / 2.;
  rp := math.sqrt(abs(c));
  c:=CMPLX( RE(z) - rp*math.sin(phi), IM(z) + rp*math.cos(phi));
  RETURN CMPLX( arg(c), - math.ln(abs(c)) )
END arccos;

PROCEDURE arctan*(z: complex): complex;
  VAR opb, a2, den: real; c: complex;
BEGIN
  opb := 1. + IM(z);
  a2 := RE(z)*RE(z);
  den := opb*opb + a2;
  c:=CMPLX( ((1.-IM(z))*opb - a2) / den, 2.*RE(z)/den);
  RETURN CMPLX( arg(c)/2., -math.ln(abs(c))/2. )
END arctan;

PROCEDURE arctan2*(zn,zd: complex): complex;
  VAR c: complex;
BEGIN
  ASSERT((zn # zero) OR (zd # zero));
  c:=(zd+i*zn)/(zd-i*zn);
  RETURN CMPLX(0.5*arg(c),-0.5*math.ln(abs(c)));
END arctan2;

PROCEDURE sinh*(z: complex): complex;
  VAR x,y: LONGREAL;
BEGIN
  x := math.exp(RE(z));
  y := 1. / x;
  RETURN CMPLX(
                math.cos(IM(z))*(x - y)/2.,
                math.sin(IM(z))*(x + y)/2.
              )
END sinh;

PROCEDURE cosh*(z: complex): complex;
  VAR x,y: LONGREAL;
BEGIN
  x := math.exp(RE(z));
  y := 1. / x;
  RETURN CMPLX(
                math.cos(IM(z))*(x + y)/2.,
                math.sin(IM(z))*(x - y)/2.
              )
END cosh;

PROCEDURE tanh*(z: complex): complex;
  VAR x,y: LONGREAL; sin,cos,minus,plus,div: real;
BEGIN
  x := math.exp(RE(z));
  y := 1. / x;
  minus:=x-y;
  plus :=x+y;
  sin:=math.sin(IM(z));
  cos:=math.cos(IM(z));
  div:=(cos*plus)**2 + (sin*minus)**2;
  RETURN CMPLX( minus*plus / div, 4.*sin*cos / div )
END tanh;

PROCEDURE arcsinh*(z: complex): complex;
BEGIN
  RETURN arcsin( CMPLX(-IM(z), RE(z)) ) * CMPLX(0., -1.);
END arcsinh;

PROCEDURE arccosh*(z: complex): complex;
BEGIN
  RETURN i * arccos(z)
END arccosh;

PROCEDURE arctanh*(z: complex): complex;
BEGIN
  RETURN arctan( CMPLX(-IM(z), RE(z)) ) * CMPLX(0., -1.);
END arctanh;

BEGIN
<* IF EXCEPTIONS THEN *>
  EXCEPTIONS.AllocateSource(source);
<* END *>
END MathCL.
