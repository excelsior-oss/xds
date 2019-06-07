<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<*- gendebug    *>
<* NEW INTRINSIC+ *>

IMPLEMENTATION MODULE xmRTSnat;

IMPORT xPOSIX;

FROM SYSTEM IMPORT INT32;

PROCEDURE [2]/ sin  (x: LONGREAL): LONGREAL;   (* Dummy intrinsic definitions *)
PROCEDURE [2]/ cos  (x: LONGREAL): LONGREAL;
PROCEDURE [2]/ sqrt (x: LONGREAL): LONGREAL;

PROCEDURE c_abs(re,im: LONGREAL): LONGREAL;
BEGIN
  RETURN sqrt(re*re+im*im);
END c_abs;

PROCEDURE c_arg(re,im: LONGREAL): LONGREAL;
BEGIN
  IF (re=0.0) & (im=0.0) THEN RETURN 0.0 END;
  RETURN xPOSIX.atan2(im,re);
END c_arg;

PROCEDURE [2] X2C__EXPCI(base: COMPLEX; ex: INT32): COMPLEX;
  VAR res: COMPLEX;
BEGIN
  IF (ex<0) OR (ex>8) THEN RETURN X2C__EXPCR(base,VAL(LONGREAL,ex)) END;
  res:=CMPLX(1.0,0.0);
  WHILE ex>0 DO res := res * base; DEC(ex) END;
  RETURN res;
END X2C__EXPCI;

PROCEDURE [2] X2C__EXPLI(base: LONGCOMPLEX; ex: INT32): LONGCOMPLEX;
  VAR res: LONGCOMPLEX;
BEGIN
  IF (ex<0) OR (ex>8) THEN RETURN X2C__EXPLR(base,VAL(LONGREAL,ex)) END;
  res:=CMPLX(1.0,0.0);
  WHILE ex>0 DO res := res * base; DEC(ex) END;
  RETURN res;
END X2C__EXPLI;

PROCEDURE [2] X2C__EXPCR(base: COMPLEX; ex: LONGREAL): COMPLEX;
  VAR x: REAL; y: LONGREAL;
BEGIN
  IF (RE(base)=0.0) & (IM(base)= 0.0) & (ex>0.0) THEN
    RETURN CMPLX(0.0,0.0);
  END;
  x:=FLOAT(xPOSIX.pow(c_abs(LFLOAT(RE(base)),LFLOAT(IM(base))),ex));
  y:=ex*c_arg(LFLOAT(RE(base)),LFLOAT(IM(base)));
  RETURN CMPLX(x*VAL(REAL,cos(y)),x*VAL(REAL,sin(y)));
END X2C__EXPCR;

PROCEDURE [2] X2C__EXPLR(base: LONGCOMPLEX; ex: LONGREAL): LONGCOMPLEX;
  VAR x,y: LONGREAL;
BEGIN
  IF (RE(base)=0.0) & (IM(base)= 0.0) & (ex>0.0) THEN
    RETURN CMPLX(0.0,0.0);
  END;
  x:=xPOSIX.pow(c_abs(RE(base),IM(base)),ex);
  y:=ex*c_arg(RE(base),IM(base));
  RETURN CMPLX(x*cos(y),x*sin(y));
END X2C__EXPLR;

END xmRTSnat.
