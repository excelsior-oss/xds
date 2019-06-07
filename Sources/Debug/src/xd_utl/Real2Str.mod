IMPLEMENTATION MODULE Real2Str;

IMPORT xrnNaN;

<* PUSH *>
<* ALIGNMENT = "1" *>
TYPE
  LLR = ARRAY [0..2] OF CARDINAL;
<* POP *>

VAR
  MAX_LLR: LLR;

CONST
  MAXPOW = 20;
  
VAR
  pow    : ARRAY [0..MAXPOW-1] OF LONGLONGREAL; (* pow[i] = 10^(2^i), or -1.0 *)
  pow_max: CARDINAL; 
  pow2   : CARDINAL; (* max: 10^(pow2 = 2^pow_max) < MAX(float) *)


PROCEDURE pow_init;
VAR
  i: CARDINAL;
BEGIN
  (* init pow[i] = 10^(2^i), or -1.0 if overflow *)
  i := 1;
  pow[0] := 10.0;
  pow2 := 1;
  WHILE (i < MAXPOW) & (MAX_LONGLONGREAL/pow[i-1] > pow[i-1]) DO
    pow[i] := pow[i-1]*pow[i-1];
    INC (pow2, pow2);
    INC (i);
  END;
  pow_max := i-1;
  (* last must be -1.0 always *)
  WHILE i < MAXPOW DO
    pow[i] := -1.0;
    INC (i);
  END;
END pow_init;



PROCEDURE power10(e: CARDINAL; VAR oo: BOOLEAN): LONGLONGREAL;
VAR
  i  : CARDINAL;
  t  : LONGLONGREAL;
  ovf: BOOLEAN;
BEGIN
  i := 0;
  t := 1.0;
  ovf := oo;
  WHILE (e#0) AND NOT ovf DO
    IF ODD(e) THEN
      ovf := (i>pow_max) OR (MAX_LONGLONGREAL/pow[i] <= t);
      IF NOT ovf THEN t := t*pow[i]; END;
    END;
    INC (i);
    e := e DIV 2;
  END;
  oo := ovf;
  IF ovf THEN t := MAX_LONGLONGREAL; END;
  RETURN t;
END power10;



PROCEDURE exponent(VAR rr: LONGLONGREAL): INTEGER;
(*
   exponent(0.5) = 0,
   exponent(5.0) = 1, etc.
*)
VAR
  r,x: LONGLONGREAL;
  n,i: INTEGER;
  e  : INTEGER;
BEGIN
  ASSERT(rr>=0.0);
  n := pow2; i := pow_max; e := 0; r := rr;
  IF r >= 1.0 THEN
    IF r/2.0 = r THEN r := MAX_LONGLONGREAL; END;
    LOOP
      x := r / pow[i];
      IF x<0.1 THEN
        IF i=0 THEN 
          EXIT; 
        ELSE
          DEC(i);
          n := n DIV 2;
        END;
      ELSE
        r := x;
        INC(e, n);
      END;
    END;
  ELSIF (r < 0.1) AND (r*2.0 # r) THEN
    LOOP
      x := r*pow[i];
      ASSERT(x>0.0);
      IF x >= 1.0 THEN
        IF i = 0 THEN
          EXIT;
        ELSE
          DEC(i);
          n := n DIV 2;
        END;
      ELSE
        r := x;
        DEC(e, n);
      END;
    END;
  END;
  rr := r;
  IF rr >= 1.0 THEN
    rr := rr/10.0;
    INC(e); 
  END;
  RETURN e;
END exponent;


CONST 
  VALID   = "valid";
  INVALID = "invalid";
  PINF    = "+inf.";
  NINF    = "-inf.";
  NAN     = "NaN";

VAR
  temp: LONGLONGREAL;

PROCEDURE IsRealSpecial (R: LONGLONGREAL; VAR s: ARRAY OF CHAR): BOOLEAN;
BEGIN
  IF xrnNaN.X2C_is_NaNE (R) THEN
    -- it is a NaN (not a number)
    COPY(NAN, s);
    RETURN TRUE;
  ELSIF xrnNaN.X2C_is_infE (R) THEN
    -- it is a pos/neg infinity
    IF R < 0.0 THEN
      COPY(NINF, s);
    ELSE
      COPY(PINF, s);
    END;
    RETURN TRUE;
  ELSIF xrnNaN.X2C_is_aNE (R) THEN
    -- it is a number, but it may be wrong nevertheless...
    -- грязные танцы с волками: нужно как-нибудь заюзать
    -- переменную, так чтобы случилось исключение
    temp := R;
    IF temp < 0.0 THEN
      temp := -temp;
    ELSE
      temp := -temp;
    END;
    COPY (VALID, s);
    RETURN FALSE;
  ELSE
    -- it is a invalid number
    COPY(INVALID, s);
    RETURN TRUE;
  END;
EXCEPT
  -- it is a invalid number
  COPY (INVALID, s);
  RETURN TRUE;
END IsRealSpecial;


VAR
  d: CARDINAL;

PROCEDURE get_digit(VAR r: LONGLONGREAL): CHAR;
BEGIN
  ASSERT ((0.0 <= r) AND (r < 1.0));
  r := r*10.0;
  d := TRUNC (r);
   IF d = 10 THEN
    -- TRUNC иногда не корректно работает с типом LONGLONGREAL
    DEC(d);
  END;
  ASSERT( (0 <= d) AND (d < 10) );
  r := r-VAL (LONGLONGREAL, d);
  -- при r - "целом", после умножения может получится число "почти"
  -- правильное "целое", но меньше d! Например, для r=0.3 получим
  -- r=4000BFFFFFFFFFFFFFFE
  -- d=4000C000000000000000
  -- их разница отрицательная, поэтому r здесь равно 0.0
  IF r < 0.0 THEN
    r := 0.0;
  END;
  RETURN CHR (d+ORD('0'));
END get_digit;


PROCEDURE to_float (r: LONGLONGREAL;
                    digs, round, max: INTEGER;
                    exp: CHAR;
                    tail: BOOLEAN;
                    esig: BOOLEAN;
                    VAR str: ARRAY OF CHAR);
(*
  digs - number of significant digits
  max  - max number of significant digits
*)

  VAR
    ps: CARDINAL;

  PROCEDURE ch(c: CHAR);
  BEGIN
    IF ps<=HIGH(str) THEN str[ps] := c END;
    INC(ps);
  END ch;

  PROCEDURE cnum(n: CARDINAL);
  BEGIN
    IF n<10 THEN
      ch(CHR(n+ORD('0')));
    ELSE
      cnum(n DIV 10);
      ch(CHR(n MOD 10 +ORD('0')));
    END;
  END cnum;

  VAR
    k,l,d,pt,e,len: INTEGER;
    c: CHAR;
    ovr,nz: BOOLEAN;
BEGIN
  IF IsRealSpecial (r, str) THEN
    RETURN;
  END;

  IF round <= 0 THEN round := 1; END;

  ps := 0; len := 0; ovr := FALSE; nz := FALSE;

  IF r < 0.0 THEN ch('-'); r := -r END;
  e := exponent(r);

  d := max;
  IF (digs>0) & (digs<d) THEN d := digs END;
  (* d - max allowed digits *)

  (* round last digit *)
  IF r > 0.0 THEN
    r := r + 0.5/power10(d,ovr);
    IF r >= 1.0 THEN r := r / 10.0; INC(e) END;
  END;

  pt := 1; DEC(e);
  k := e MOD round;
  IF k#0 THEN pt := k+1; DEC(e,k) END;
  (* pt - number of digits before point *)

  ASSERT ((0.0 <= r) AND (r < 1.0));
  FOR k := 1 TO pt DO
    IF len<d THEN c := get_digit(r); INC(len);
    ELSE c := '0';
    END;
    IF nz THEN ch(c)
    ELSIF c#'0' THEN ch(c); nz := TRUE;
    ELSIF k=pt THEN ch(c);
    END;
    ASSERT(r<1.0);
  END;

  IF d>pt THEN
    k := ps+2; ch('.');
    FOR l := 1 TO d-pt DO
      c := get_digit(r); ch(c);
      IF c#'0' THEN k := ps; nz := TRUE END;
    END;
    IF NOT tail THEN ps := k END;
  END;

  IF nz (* Commented out by Hady. See BUG580 in PRS. & (e#0) *) THEN
    ch(exp);
    IF e<0 THEN ch('-'); e := -e ELSIF esig THEN ch('+') END;
    cnum(e);
  END;

  ASSERT(NOT ovr);
  IF ps<=HIGH(str) THEN str[ps] := 0C END;
END to_float;



PROCEDURE to_fixed(r: LONGLONGREAL;
                   place,max: INTEGER;
                   VAR str: ARRAY OF CHAR);
VAR
  ps: CARDINAL;

  PROCEDURE ch(c: CHAR);
  BEGIN
    IF ps <= HIGH(str) THEN str[ps] := c END;
    INC(ps);
  END ch;

VAR
  e,n,d,len: INTEGER;
  ovr: BOOLEAN;
BEGIN
  IF IsRealSpecial (r, str) THEN
    RETURN;
  END;

  COPY('?', str);
 
  ps := 0; len := 0; ovr := FALSE;
  IF r < 0.0 THEN ch('-'); r := -r END;

  IF r > 0.0 THEN
    e := exponent(r);
    (* round last digit *)
    IF place>=0 THEN n := e+place ELSE n := e+place+1 END;
    IF n>max THEN r := r+0.5/power10(max,ovr);
    ELSIF n>=0 THEN r := r+0.5/power10(n,ovr);
    END;
    IF r >= 1.0 THEN r := r / 10.0; INC(e) END;
  ELSE
    e := 0;
  END;
  d := max;
  IF e<=0 THEN
    ch('0');
    IF place>-1 THEN
      ch('.'); n := 0;
      WHILE (n<place) & (ps<HIGH(str)+1) DO
        IF (n<-e) OR (len>=d) THEN ch('0');
        ELSE ch(get_digit(r)); INC(len);
        END;
        INC(n);
      END;
    END;
  ELSIF -place-1 >= e THEN
    ch('0');
  ELSE
    n := 0;
    WHILE (n<e) & (ps<HIGH(str)+2) DO
      IF (n<e+place+1) & (len<d) THEN ch(get_digit(r)); INC(len);
      ELSE ch('0');
      END;
      INC(n);
    END;
    IF place>-1 THEN
      ch('.'); n := 0;
      WHILE (n<place) & (ps<HIGH(str)+1) DO
        IF len<d THEN ch(get_digit(r)); INC(len);
        ELSE ch('0');
        END;
        INC(n);
      END;
    END;
  END;
  ASSERT(NOT ovr);
  IF ps<=HIGH(str) THEN str[ps] := 0C END;
END to_fixed;




PROCEDURE to_any(r    : LONGLONGREAL;
                 len  : INTEGER;
                 max  : INTEGER;  (* max number of digits *)
                 VAR s: ARRAY OF CHAR);      (* result string *)

  VAR
    d,e: INTEGER;
    sg: INTEGER;
    f: LONGLONGREAL;
BEGIN
  IF IsRealSpecial (r, s) THEN
    RETURN;
  END;
  IF len <= 0 THEN
    RETURN;
  END;
  f := r;
  sg := 0;
  IF f < 0.0 THEN
    f := -f;
    sg := 1;
  END;
  e := exponent(f);
  IF max > len-sg THEN
    max := len-sg;
  END;
  IF (max = len-sg) AND (e+1 < max) THEN
    max := len-sg-1;
  END;
  FOR d := 0 TO max DO
    s[d] := get_digit (f);
  END;
  WHILE (max > 1) AND (s[max-1] = '0') AND (s[max] < '5') DO
    DEC (max);
  END;
  IF (e >= max) AND (sg+e <= len) THEN
    to_fixed (r, len-e-sg-1, max, s);
    IF INT(LENGTH(s)) <= len THEN
      RETURN;
    END;
  END;
  IF (e >= 0) AND (e < max) AND (sg+1+max <= len) THEN
    to_fixed (r, len-e-sg-1, max, s);
  ELSIF (e < 0) AND (sg+2+ABS(e)+max <= len) THEN
    to_fixed (r, len-sg-2, max, s);
  ELSE
    d := len;
    LOOP
      to_float (r, d, 1, max, 'E', FALSE, FALSE, s);
      IF (d <= 1) OR (INT(LENGTH(s)) <= len) THEN
        EXIT;
      END;
      DEC(d);
    END;
  END;
END to_any;


--PROCEDURE ["C"] / X2C_InitFPP4NaN;

BEGIN
  --X2C_InitFPP4NaN;
  MAX_LLR[0] := 0FFFFFFFFH;
  MAX_LLR[1] := 0FFFFFFFFH;
  MAX_LLR[2] := 07FFEH;
  MAX_LONGLONGREAL := LONGLONGREAL(MAX_LLR);
  pow_init;
END Real2Str.
