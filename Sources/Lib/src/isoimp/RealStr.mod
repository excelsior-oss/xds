(* Copyright (c) xTech 1993. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
<* IF NOT __GEN_C__ THEN *>
  <*- PROCINLINE   *>
<* END *>
IMPLEMENTATION MODULE RealStr; (* Andrew Cadach Aug 1993 *)

(* Modifications:
        14-Mar-94 Ned:   error in to_fixed
        22-Sep-93 Andy:  visualization of special 80387 values added
        08-Sep-93 Ned:   to_fixed, write
        28-Feb-95 Sem:   reimplemented
*)

(* Implementation notes (Cadach):
   1. No underflow checks are performed.
      ISO says nothing about it.
   2. Approximate overflow checks in float math.
      Impossible to implement without "signal" and/or exact knowledge of
      hardware representation.
   3. Suppose that MAX(float) < 10^(MAX(CARDINAL)/2).
      It is true (at least right now for all platforms I know).
   4. ISO said nothing about IEEE infinum.
   5. ISO said nothing what to do if output string is not enough to keep
      the value with desired precision. I fill a string by '?'.
   6. ISO WG13 contain severe bugs because they do not take into account
      results of rounding: for example, 9.9999(9) must be output as 1.E1.
   7. ISO is really stupid sometimes (I implemented correctly :-)
      - "The decimal point shall not be included if there are no significant
	digits in the fractional part". Well, what about exponent - shall I
	output 1.0E-10 as 1E-10? Anyway, such output cannot be scanned back.
      - Float-point value (for input) MUST contain exponent part. I think it
	it simply crasy.
*)

IMPORT SYSTEM;
IMPORT ConvTypes, CharClass, XReal;

CONST
  PINF  = "+inf.";
  NINF  = "-inf.";
  NAN   = "NaN";
  INDEF = "indef.";

PROCEDURE IsRealSpecial(R: REAL; VAR s: ARRAY OF CHAR): BOOLEAN;
BEGIN
  IF (R#0.0) & (R/2.0=R) THEN
    IF R<0.0 THEN COPY(NINF,s) ELSE COPY(PINF,s) END;
    RETURN TRUE;
  END;
  RETURN FALSE;
END IsRealSpecial;

CONST
  EOS = 0C;
  MAXPOW = 32;

VAR
  max_digits: CARDINAL;
(* Find "digits" *)
(* it is necessary to prevent optimization - some C compilers really tough *)
(* it would be better to split it into different modules indeed *)

PROCEDURE store (VAR r: float; v: float);
BEGIN
  r := v;
END store;

PROCEDURE add (a, b: float): float;
 VAR r: float;
BEGIN
  store (r, a+b); RETURN r;
END add;

PROCEDURE div (a, b: float): float;
  VAR r: float;
BEGIN
  store (r, a/b); RETURN r;
END div;

PROCEDURE digits(): CARDINAL;
  VAR u: float;
BEGIN
  IF max_digits=0 THEN
    u:=0.1; max_digits:=1;
    LOOP
      IF (max_digits=32) OR (add(1.0,u)=1.0) THEN EXIT END;
      INC(max_digits);
      u:=div(u,10.0);
    END;
    DEC(max_digits);
  END;
  RETURN max_digits;
END digits;

PROCEDURE StrToReal (str: ARRAY OF CHAR; VAR real: float; VAR res: ConvResults);
  VAR
    s, i, e, n: CARDINAL;
    r, t, p: LONGREAL;
    c: CHAR;
    ovf, rovf, neg, rneg: BOOLEAN;

BEGIN
  (* spaces [-]  1[23] "." [456] ["E" ["-"] 1[23]] *)
  (* 0      0 1 1 2  2 2 3 3   3  3 4  4 5 5 6  6  *)
  (* finishing states are: 2, 3, 6 *)

  c := ' '; i := 0; e := 0;
  s := 0; r := 0.0; p := 1.0;
  neg := FALSE; rneg := FALSE; ovf := FALSE; rovf := FALSE;
  LOOP
    IF (c#EOS) & (i<=HIGH(str)) THEN c := str[i]; INC (i);
    ELSE c := EOS;
    END;
    IF CharClass.IsWhiteSpace(c) THEN c := ' ' END;
    CASE c OF
    |' ':
      IF s#0 THEN EXIT END;
    |'+', '-':
      IF s=0 THEN rneg:=c='-';
      ELSIF s=4 THEN neg:=c='-';
      ELSE EXIT
      END;
      INC (s);
    |'.':
      IF s#2 THEN EXIT END;
      INC (s);
    |'E':
      IF (s#2) & (s#3) THEN EXIT END;
      s:=4;
    |'0'..'9':
      n := ORD(c) - ORD('0');
      t := VAL(LONGREAL, n);
      CASE s OF
      |0..2:
        rovf := rovf OR (r >= (VAL(LONGREAL,MAX(REAL)) - t)/10.0);
        IF NOT rovf THEN r := r*10.0 + t; END;
        s := 2;
      |3:
        p := p / 10.0;
        IF NOT rovf THEN r := r + t*p; END;
      |4..6:
        ovf := ovf OR (e >= (MAX(CARDINAL) - n) DIV 10);
        IF NOT ovf THEN e := e*10 + n; END;
        s := 6;
      ELSE EXIT
      END;
    ELSE EXIT
    END;
  END;

  IF (s=2) OR (s=3) OR (s=6) THEN
    IF NOT ovf THEN t:= XReal.power10 (e, ovf) END;
    IF NOT ovf THEN
      IF neg THEN
        r := r / t;
      ELSE
        ovf := VAL(LONGREAL,MAX(REAL))/t <= r;
        IF NOT ovf THEN r := r*t END;
      END;
    END;
    IF ovf THEN
      IF NOT rovf & neg THEN
        r := 0.0
      ELSE
        IF rneg THEN r := MIN(float)
        ELSE r := MAX(float)
        END;
      END;
      res := ConvTypes.strOutOfRange
    ELSE
      IF rneg THEN r := -r; END;
      res := ConvTypes.strAllRight;
    END;
    IF c=EOS THEN real := VAL(REAL,r); RETURN END;
  END;

  IF (c=EOS) & (s=0) THEN res := ConvTypes.strEmpty; RETURN; END;
  res := ConvTypes.strWrongFormat;
END StrToReal;

PROCEDURE RealToFloat (real: float; sigFigs: CARDINAL; VAR str: ARRAY OF CHAR);
  VAR s: XReal.STR;
BEGIN
  IF IsRealSpecial(real,str) THEN RETURN END;
  XReal.to_float(real,sigFigs,1,digits(),'E',FALSE,TRUE,s);
  XReal.strcpy(s,str);
END RealToFloat;

PROCEDURE RealToEng (real: float; sigFigs: CARDINAL; VAR str: ARRAY OF CHAR);
  VAR s: XReal.STR;
BEGIN
  IF IsRealSpecial(real,str) THEN RETURN END;
  XReal.to_float(real,sigFigs,3,digits(),'E',FALSE,TRUE,s);
  XReal.strcpy(s,str);
END RealToEng;

PROCEDURE RealToFixed (real: float; place: INTEGER; VAR str: ARRAY OF CHAR);
  VAR s: XReal.STR;
BEGIN
  IF IsRealSpecial(real,str) THEN RETURN END;
  XReal.to_fixed(real,place,digits(),s);
  XReal.strcpy(s,str);
END RealToFixed;

PROCEDURE RealToStr (real: float; VAR str: ARRAY OF CHAR);
  VAR s: XReal.STR;
BEGIN
  IF IsRealSpecial(real,str) THEN RETURN END;
  XReal.to_any(real,digits(),s,LEN(str));
  XReal.strcpy(s,str);
END RealToStr;

BEGIN
  max_digits := 0;
END RealStr.
