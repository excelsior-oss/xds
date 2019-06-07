<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<*- gendebug    *> (* Don enable! History would not work (SYSTEM.CODE) *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xrExponents;

FROM SYSTEM    IMPORT  INT32;
IMPORT
  RTS:=xmRTS
  ,math:=xPOSIX
  ;

PROCEDURE c_abs(re,im: LONGREAL): LONGREAL;
BEGIN
  RETURN math.sqrt(re*re+im*im);
END c_abs;

PROCEDURE c_arg(re,im: LONGREAL): LONGREAL;
BEGIN
  IF (re=0.0) & (im=0.0) THEN RETURN 0.0 END;
  RETURN math.atan2(im,re);
END c_arg;

PROCEDURE [2] X2C_EXPRI(base: LONGREAL; ex: INT32): LONGREAL;
  VAR res: LONGREAL;
BEGIN
  IF (ex<0) OR (ex>8) THEN
    RETURN math.pow(base,VAL(LONGREAL,ex))
  END;
  res:=1.0;
  WHILE ex>0 DO res:=res*base; DEC(ex) END;
  RETURN res;
END X2C_EXPRI;

PROCEDURE [2] X2C_EXPCI(base: COMPLEX; ex: INT32): COMPLEX;
  VAR res: COMPLEX;
BEGIN
  IF (ex<0) OR (ex>8) THEN RETURN X2C_EXPCR(base,VAL(LONGREAL,ex)) END;
  res:=CMPLX(1.0,0.0);
  WHILE ex>0 DO res:=RTS.CPLX_MUL(res,base); DEC(ex) END;
  RETURN res;
END X2C_EXPCI;

PROCEDURE [2] X2C_EXPLI(base: LONGCOMPLEX; ex: INT32): LONGCOMPLEX;
  VAR res: LONGCOMPLEX;
BEGIN
  IF (ex<0) OR (ex>8) THEN RETURN X2C_EXPLR(base,VAL(LONGREAL,ex)) END;
  res:=CMPLX(1.0,0.0);
  WHILE ex>0 DO res:=RTS.CPLX_LMUL(res,base); DEC(ex) END;
  RETURN res;
END X2C_EXPLI;

PROCEDURE [2] X2C_EXPCR(base: COMPLEX; ex: LONGREAL): COMPLEX;
  VAR x: REAL; y: LONGREAL;
BEGIN
  IF (RE(base)=0.0) & (IM(base)= 0.0) & (ex>0) THEN
    RETURN CMPLX(0.0,0.0);
  END;
  x:=math.pow(c_abs(RE(base),IM(base)),ex);
  y:=ex*c_arg(RE(base),IM(base));
  RETURN CMPLX(x*VAL(REAL,math.cos(y)),x*VAL(REAL,math.sin(y)));
END X2C_EXPCR;

PROCEDURE [2] X2C_EXPLR(base: LONGCOMPLEX; ex: LONGREAL): LONGCOMPLEX;
  VAR x,y: LONGREAL;
BEGIN
  IF (RE(base)=0.0) & (IM(base)= 0.0) & (ex>0) THEN
    RETURN CMPLX(0.0,0.0);
  END;
  x:=math.pow(c_abs(RE(base),IM(base)),ex);
  y:=ex*c_arg(RE(base),IM(base));
  RETURN CMPLX(x*math.cos(y),x*math.sin(y));
END X2C_EXPLR;

END xrExponents.
