(* Copyright (c) 1999-2003 Excelsior, LLC. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
<* IF NOT __GEN_C__ THEN *>
  <*- PROCINLINE   *>
<* END *>
IMPLEMENTATION MODULE LongStr; (* Andrew Cadach Aug 1993 *)

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

<* IF __GEN_C__ THEN *>
IMPORT xPOSIX, xrInt64;
<* END *>

<* IF __GEN_X86__ THEN *>
IMPORT xrInt64;
<* END *>

CONST PINF  = "+inf.";
      NINF  = "-inf.";
      NAN   = "NaN";
      INDEF = "indef.";

PROCEDURE IsRealSpecial(R: LONGREAL; VAR s: ARRAY OF CHAR): BOOLEAN;
BEGIN
  IF (R#0.0) & (R/2.0=R) THEN
    IF R<0.0 THEN COPY(NINF,s) ELSE COPY(PINF,s) END;
    RETURN TRUE;
  END;
  RETURN FALSE;
END IsRealSpecial;

CONST
  EOS = 0C;

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
    DEC(max_digits,2);
  END;
  RETURN max_digits;
END digits;

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

(*---------------------------------------------------------------------------
 * Implementation of StrToReal() divided for 3 cases:
 * 1) When we can rely on C
 * 2) When we have 10-byte floating-point numbers
 * 3) Otherwise.
 *
 * It is better to write one precise algorithm for all cases but it is harder 
 * that it seems.
 *)


<* IF __GEN_C__ OR __GEN_X86__ THEN *>

TYPE  arrCARD = ARRAY[0..9] OF CARDINAL;
      arrLLR   = ARRAY[0..8] OF LONGLONGREAL;   
      LongBits= ARRAY[0..1] OF CARDINAL;
      LongLongBits= RECORD  w1,w2: CARDINAL; hw3: SYSTEM.CARD16; END;


CONST maxPositiveExponent  =    308;
      maxNegativeExponent  =    -324;

--      positiveInfinityLB   =    LongBits{0H,07ff00000H};
--      negativeInfinityLB   =    LongBits{0H,0fff00000H};
      negativeZeroLB       =    LongBits{0H,080000000H};
      positiveZeroLB       =    LongBits{0H,0H};

      maxFloatLB           =    LongBits{0E0000000H,47EfffffH};
      minFloatLB           =    LongBits{0H,36A00000H};

      tenLLB         =          LongLongBits{0,2684354560,16386};       
      tenpowC = arrCARD{1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000};      

      minValHi = 494065645;
      minValLo = 841246544;
      maxValHi = 179769313;
      maxValLo = 486231570;

VAR   maxFloat, minFloat,       
--      positiveInfinity, negativeInfinity,
      negativeZero, positiveZero:               LONGREAL;
      tenpowLLR:                                arrLLR;


PROCEDURE ["StdCall"] / X2J_long2double(i: xrInt64.X2C_int64): LONGREAL;

PROCEDURE TenPower(i: CARDINAL): LONGLONGREAL;
VAR j:  INTEGER;
    x:  LONGLONGREAL;
--suppose we NEVER get incorrect input (0<=i<512)
BEGIN
  x:=1.0; j:=0;
  WHILE i>0 DO
     IF ODD(i) THEN x:=x*tenpowLLR[j] END;
     INC(j);
     i:=i DIV 2;
  END;
  RETURN x;
END TenPower;


PROCEDURE StrToReal(str-: ARRAY OF CHAR; VAR real: LONGREAL; VAR res: ConvResults);
VAR
  state, i, n1, n2, x1, x2, n: CARDINAL;
  ldvalue: LONGLONGREAL;
  exp1, exp2, diffexp: INTEGER;
  c: CHAR;
  expovf, eneg, mneg: BOOLEAN;
  val64,t64: xrInt64.X2C_int64;

  PROCEDURE TooSmall(): BOOLEAN;
  BEGIN
     IF exp2 < maxNegativeExponent THEN
         RETURN TRUE;
     ELSIF exp2 = maxNegativeExponent THEN
         IF n1 < 9 THEN
             RETURN (x1*tenpowC[9-n1]) < minValHi;
         ELSIF (*n1=9*) x1 <> minValHi THEN
             RETURN  x1 < minValHi;
         ELSE (*n1=9 && x1=minValHi*)
             RETURN (x2*tenpowC[9-n2]) < minValLo;
         END;
     ELSE RETURN FALSE
     END;
  END TooSmall;

  PROCEDURE TooBig(): BOOLEAN;
  BEGIN
     IF exp2 > maxPositiveExponent THEN
         RETURN TRUE;
     ELSIF exp2 = maxPositiveExponent THEN
         IF n1 < 9 THEN
             RETURN (x1*tenpowC[9-n1]) > maxValHi;
         ELSIF (*n1=9*) x1 <> maxValHi THEN
             RETURN  x1 < maxValHi;
         ELSE (*n1=9 && x1=maxValHi*)
             RETURN (x2*tenpowC[9-n2]) > maxValLo;
         END;
     ELSE RETURN FALSE
     END;
  END TooBig;

BEGIN
  (* spaces [-]  "0..0"1[23] ["."] [456] ["E"|"e" ["-"] 1[23]] *)
  (* 0      0 1 1 2    3  3 3  4  4   4  4 5      5 6 6 7  7  *)
  (*                           35 if only zeroes before . *)
  (* finishing states are: 2, 3, 4, 35, 7  *)

  c := ' '; i := 0; exp1 := -1; exp2:=0; x1:=0; x2:=0; n1:=0; n2:=0;
  state := 0;
  eneg := FALSE; mneg := FALSE; expovf := FALSE;
  LOOP
    IF (c#EOS) & (i<=HIGH(str)) THEN c := str[i]; INC (i);
    ELSE c := EOS;
    END;
    IF CharClass.IsWhiteSpace(c) THEN c := ' ' END;
    CASE c OF
    |' ':
      IF state#0 THEN EXIT END;
    |'+', '-':
      IF state=0 THEN mneg:=c='-';
      ELSIF state=5 THEN eneg:=c='-';
      ELSE EXIT
      END;
      INC (state);
    |'.':
      IF state<3 THEN
         state:=35
      ELSIF state=3 THEN
         state:=4
      ELSE EXIT
      END;
    |'E','e':
      IF (state#2) & (state#3) & (state#4) & (state#35) THEN EXIT END;
      state:=5;
    |'0'..'9':
      n := ORD(c) - ORD('0');
      CASE state OF
      |0..2:
        IF n=0 THEN
           state := 2;
        ELSE
           state:=3;
           x1:=n; n1:=1; exp1:=0;
        END
      |35:
        IF n=0 THEN
           DEC(exp1);
        ELSE
           state:=4;
           x1:=n; n1:=1;
        END
      |3..4:
        IF n1<9 THEN
           INC(n1); x1:=x1*10+n;
        ELSIF n2<9 THEN
           INC(n2); x2:=x2*10+n;
        END;
        IF state=3 THEN
           INC(exp1)
        END;
      |5..7:
        expovf := expovf OR (exp2 >= SYSTEM.CAST(INTEGER,(MAX(INTEGER)- 20 - n) DIV 10));
        IF NOT expovf THEN exp2 := exp2*10 + SYSTEM.CAST(INTEGER,n); END;
        state := 7;
      ELSE EXIT
      END;
    ELSE EXIT
    END;
  END;
  IF (state=2) OR (state=3) OR (state=4) OR (state=35) OR (state=7) THEN
      IF c#EOS THEN res := ConvTypes.strWrongFormat; RETURN END;

      IF n1=0 THEN
         res:=ConvTypes.strAllRight;
         IF mneg THEN real:=negativeZero ELSE real:=positiveZero END;
         RETURN
      END;

      IF eneg THEN exp2:=-exp2 END;

      IF(exp1>0) THEN
          expovf:= expovf OR (exp2 >= SYSTEM.CAST(INTEGER,MAX(INTEGER) - exp1))
      ELSE
          expovf:= expovf OR (exp2 <= SYSTEM.CAST(INTEGER,MIN(INTEGER) - exp1))
      END;
      IF NOT expovf THEN exp2:=exp2+exp1 END; --"real" exponent

      IF TooSmall() THEN
         res:=ConvTypes.strOutOfRange;
         IF mneg THEN real:=negativeZero ELSE real:=positiveZero END;
         RETURN;
      END;
      IF TooBig() THEN
         res:=ConvTypes.strOutOfRange;
--         IF mneg THEN real:=negativeInfinity ELSE real:=positiveInfinity END;
         IF mneg THEN real:=MIN(LONGREAL) ELSE real:=MAX(LONGREAL) END;
         RETURN;
      END;

      res:=ConvTypes.strAllRight;


 <* IF __GEN_X86__ THEN *> (* enclosed conditional compilation *)

      diffexp:=exp2-SYSTEM.CAST(INTEGER,n1+n2-1); -- exponent to be multiplied to

      xrInt64.X2C_MUL64(val64,tenpowC[n2],x1);
      xrInt64.X2C_CARDTO64(t64,x2);
      xrInt64.X2C_ADD64(val64,val64,t64);
      ldvalue:=X2J_long2double(val64);

      IF diffexp > 0 THEN  (* diffexp<=maxPositiveExponent  --always *)
           ldvalue:=ldvalue*TenPower(diffexp);
      ELSIF diffexp < 0 THEN
           ldvalue:=ldvalue / TenPower(-diffexp);
      END;
      IF mneg THEN ldvalue:=-ldvalue; END;
      real:=ldvalue;

 <* ELSE *> (* __GEN_C__ *)
      real := xPOSIX.atof(str);
 <* END *> (* end of enclosed conditional compilation *)

      RETURN;
  END;

  IF (c=EOS) & (state=0) THEN res := ConvTypes.strEmpty; RETURN; END;
  res := ConvTypes.strWrongFormat;
END StrToReal;

PROCEDURE init;
VAR     i: INTEGER;
        x: LONGLONGREAL;
BEGIN
--      positiveInfinity:=SYSTEM.CAST(LONGREAL,positiveInfinityLB);
--      negativeInfinity:=SYSTEM.CAST(LONGREAL,negativeInfinityLB);
      negativeZero:=SYSTEM.CAST(LONGREAL,negativeZeroLB);
      positiveZero:=SYSTEM.CAST(LONGREAL,positiveZeroLB);
      maxFloat:=SYSTEM.CAST(LONGREAL,maxFloatLB);
      minFloat:=SYSTEM.CAST(LONGREAL,minFloatLB);

--      x:=VAL(LONGLONGREAL,10.0);
      x:=SYSTEM.CAST(LONGLONGREAL,tenLLB);
      tenpowLLR[0]:=x;
      FOR i:=1 TO 8 DO
          x:=x*x;
          tenpowLLR[i]:=x;
      END;
END init;

<* ELSE *>
(* old implementation, precision is bad *)
PROCEDURE StrToReal(str-: ARRAY OF CHAR; VAR real: float; VAR res: ConvResults);
VAR
  s, i, e, n: CARDINAL;
  r, t, p: float;
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
      t := VAL(float, n);
      CASE s OF
      |0..2:
        rovf := rovf OR (r >= (MAX(float) - t)/10.0);
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
    IF NOT ovf THEN
      t:= XReal.power10 (e, ovf);
      IF neg THEN
        r := r / t;
      ELSE
        ovf := MAX(float)/t <= r;
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
    IF c=EOS THEN real := r; RETURN END;
  END;

  IF (c=EOS) & (s=0) THEN res := ConvTypes.strEmpty; RETURN; END;
  res := ConvTypes.strWrongFormat;
END StrToReal;

<* END *>


BEGIN
  max_digits := 0;

<* IF __GEN_X86__ OR __GEN__C__ THEN *>
  init();
<* END *>
  


END LongStr.
