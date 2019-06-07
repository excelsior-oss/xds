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
  ,M2EXCEPTION
  ;

PROCEDURE ["StdCall"] / X2C_sqrt(x: REAL): REAL;
PROCEDURE ["StdCall"] / X2C_sqrtl(x: LONGREAL): LONGREAL;
PROCEDURE ["StdCall"] / X2C_arctan2(y,x: REAL): REAL;
PROCEDURE ["StdCall"] / X2C_arctan2l(y,x: LONGREAL): LONGREAL;
PROCEDURE ["StdCall"] / X2C_exp(x: REAL): REAL;
PROCEDURE ["StdCall"] / X2C_expl(x: LONGREAL): LONGREAL;
PROCEDURE ["StdCall"] / X2C_ln(x: REAL): REAL;
PROCEDURE ["StdCall"] / X2C_lnl(x: LONGREAL): LONGREAL;
PROCEDURE ["StdCall"] / X2C_sin(x: REAL): REAL;
PROCEDURE ["StdCall"] / X2C_sinl(x: LONGREAL): LONGREAL;
PROCEDURE ["StdCall"] / X2C_cos(x: REAL): REAL;
PROCEDURE ["StdCall"] / X2C_cosl(x: LONGREAL): LONGREAL;
PROCEDURE ["StdCall"] / X2C_isWholeNum(x: LONGREAL): BOOLEAN;
PROCEDURE ["StdCall"] / X2C_isOdd(x: LONGREAL): BOOLEAN;

PROCEDURE c_absl(re,im: LONGREAL): LONGREAL;
BEGIN
  RETURN X2C_sqrtl(re*re+im*im);
END c_absl;

PROCEDURE c_argl(re,im: LONGREAL): LONGREAL;
BEGIN
  IF (re=0.0) & (im=0.0) THEN RETURN 0.0 END;
  RETURN X2C_arctan2l(im,re);
END c_argl;

PROCEDURE c_abs(re,im: REAL): REAL;
BEGIN
  RETURN X2C_sqrt(re*re+im*im);
END c_abs;

PROCEDURE c_arg(re,im: REAL): REAL;
BEGIN
  IF (re=0.0) & (im=0.0) THEN RETURN 0.0 END;
  RETURN X2C_arctan2(im,re);
END c_arg;

PROCEDURE [2] X2C_EXPRI(base: LONGREAL; ex: INT32): LONGREAL;
  VAR res: LONGREAL;
BEGIN
  IF (ex<0) OR (ex>8) THEN
    RETURN X2C_expl(VAL(LONGREAL,ex)*X2C_lnl(base));
  END;
  res:=1.0;
  WHILE ex>0 DO res:=res*base; DEC(ex) END;
  RETURN res;
END X2C_EXPRI;

PROCEDURE [2] X2C_EXPRR(base: LONGREAL; ex: LONGREAL): LONGREAL;
	VAR minus: BOOLEAN;
			res: LONGREAL;
BEGIN
  IF ex=0.0 THEN RETURN 1.0 END;
  IF (base<0.0) THEN
    IF NOT X2C_isWholeNum(ex) THEN
      RTS.X2C_TRAP_F(ORD(M2EXCEPTION.realValueException))
    END;
    minus:=X2C_isOdd(ex);
    res:=X2C_expl(ex*X2C_lnl(-base));
    IF minus THEN res:=-res END;
    RETURN res;
  ELSIF base=0.0 THEN RETURN 0.0
  END;
  RETURN X2C_expl(ex*X2C_lnl(base));
END X2C_EXPRR;

PROCEDURE [2] X2C__EXPCR(base: COMPLEX; ex: LONGREAL): COMPLEX;
  VAR x,y: REAL;
BEGIN
  IF (RE(base)=0.0) & (IM(base)= 0.0) & (ex>0) THEN
    RETURN CMPLX(0.0,0.0);
  END;
  x:=X2C_exp(ex*X2C_ln(c_abs(RE(base),IM(base))));
  y:=ex*c_arg(RE(base),IM(base));
  RETURN CMPLX(x*X2C_cos(y),x*X2C_sin(y));
END X2C__EXPCR;

PROCEDURE [2] X2C__EXPLR(base: LONGCOMPLEX; ex: LONGREAL): LONGCOMPLEX;
  VAR x,y: LONGREAL;
BEGIN
  IF (RE(base)=0.0) & (IM(base)= 0.0) & (ex>0) THEN
    RETURN CMPLX(0.0,0.0);
  END;
  x:=X2C_exp(ex*X2C_lnl(c_absl(RE(base),IM(base))));
  y:=ex*c_argl(RE(base),IM(base));
  RETURN CMPLX(x*X2C_cosl(y),x*X2C_sinl(y));
END X2C__EXPLR;

PROCEDURE [2] X2C__EXPCI(base: COMPLEX; ex: INTEGER): COMPLEX;
  VAR res: COMPLEX;
BEGIN
  IF (ex<0) OR (ex>8) THEN RETURN X2C__EXPCR(base,VAL(LONGREAL,ex)) END;
  res:=CMPLX(1.0,0.0);
  WHILE ex>0 DO res := res * base; DEC(ex) END;
  RETURN res;
END X2C__EXPCI;

PROCEDURE [2] X2C__EXPLI(base: LONGCOMPLEX; ex: INTEGER): LONGCOMPLEX;
  VAR res: LONGCOMPLEX;
BEGIN
  IF (ex<0) OR (ex>8) THEN RETURN X2C__EXPLR(base,VAL(LONGREAL,ex)) END;
  res:=CMPLX(1.0,0.0);
  WHILE ex>0 DO res := res * base; DEC(ex) END;
  RETURN res;
END X2C__EXPLI;

END xrExponents.
