<* IF __GEN_X86__ THEN *>
<*   -NOPTRALIAS *>
<* END *>
IMPLEMENTATION MODULE LowLong;

<* +M2EXTENSIONS *>
IMPORT SYSTEM, EXCEPTIONS, math:=xMath;
<* -M2EXTENSIONS *>

VAR source: EXCEPTIONS.ExceptionSource;

PROCEDURE raise;
BEGIN
    EXCEPTIONS.RAISE (source, 0, "LowLong.lowException");
END raise;

PROCEDURE exponent (x: LONGREAL): INTEGER;
  (* Returns the exponent value of x *)
  VAR i: SYSTEM.int;
BEGIN
  x := math.X2C_frexp (x, i);
  RETURN i;
END exponent;

PROCEDURE fraction (x: LONGREAL): LONGREAL;
  (* Returns the significand (or significant part) of x *)
  VAR i: SYSTEM.int;
BEGIN
  RETURN math.X2C_frexp (x, i);
END fraction;

PROCEDURE sign (x: LONGREAL): LONGREAL;
  (* Returns the signum of x *)
BEGIN
  IF    x < 0.0 THEN RETURN -1.0
  ELSIF x = 0.0 THEN RETURN  0.0
  ELSE               RETURN  1.0
  END;
END sign;

PROCEDURE succ (x: LONGREAL): LONGREAL;
  (* Returns the next value of the type LONGREAL greater than x *)
BEGIN
  RETURN x + ulp (x)
END succ;

PROCEDURE ulp (x: LONGREAL): LONGREAL;
  (* Returns the value of a unit in the last place of x *)
  VAR pb: POINTER TO BITSET;
BEGIN
  pb := SYSTEM.ADR (x);
  INCL (pb^, 0);
  RETURN ABS (x - trunc (x, 51))
END ulp;

PROCEDURE pred (x: LONGREAL): LONGREAL;
  (* Returns the previous value of the type LONGREAL less than x *)
BEGIN
  RETURN x - ulp (x)
END pred;

PROCEDURE intpart (x: LONGREAL): LONGREAL;
  (* Returns the integer part of x *)
  VAR y: LONGREAL;
BEGIN
  x := math.X2C_modf (x, y);
  RETURN y;
END intpart;

PROCEDURE fractpart (x: LONGREAL): LONGREAL;
  (* Returns the fractional part of x *)
  VAR y: LONGREAL;
BEGIN
  RETURN math.X2C_modf (x, y);
END fractpart;

PROCEDURE scale (x: LONGREAL; n: INTEGER): LONGREAL;
  (* Returns the value of x * radix ** n *)
BEGIN
  RETURN math.X2C_ldexp (x, n);
END scale;

PROCEDURE trunc (x: LONGREAL; n: INTEGER): LONGREAL;
  (* Returns the value of the first n places of x *)
  VAR pb: POINTER TO ARRAY [0..1] OF BITSET;
BEGIN
  IF n <= 0 THEN raise END;
  IF n >= 52 THEN RETURN x END;
  pb := SYSTEM.ADR (x);
  IF n < (52 - 32) THEN
    pb^[0] := {};
    pb^[1] := pb^[1] - {0 .. 52 - 32 - n - 1};
  ELSE
    pb^[0] := pb^[0] - {0 .. 52 - n - 1};
  END;
  RETURN x
END trunc;

PROCEDURE round (x: LONGREAL; n: INTEGER): LONGREAL;
  (* Returns the value of x rounded to the first n places *)
BEGIN
  RETURN trunc (x, n);
END round;

PROCEDURE synthesize (n: INTEGER; x: LONGREAL): LONGREAL;
  (* Returns a value of the type LONGREAL constructed from the given expart and frapart *)
BEGIN
  RETURN math.X2C_ldexp (x, n);
END synthesize;

PROCEDURE setMode (m: Modes);
  (* Sets status flags appropriate to the underlying implementation of the type LONGREAL *)
  VAR r: CARDINAL;
BEGIN
  r := math.X2C_controlfp (SYSTEM.CAST (CARDINAL, m),
                      SYSTEM.CAST (CARDINAL, {0 .. nModes - 1}));
END setMode;

PROCEDURE currentMode (): Modes;
  (* Returns the current status flags in the form set by setMode *)
BEGIN
  RETURN SYSTEM.CAST (Modes, math.X2C_controlfp (0, 0));
END currentMode;

PROCEDURE IsLowException (): BOOLEAN;
  (* Returns TRUE if the current coroutine is in the exceptional execution state
     because of the raising of an exception in a routine from this module; otherwise
     returns FALSE.
  *)
BEGIN
  RETURN EXCEPTIONS.IsCurrentSource (source);
END IsLowException;

BEGIN
  EXCEPTIONS.AllocateSource (source);
END LowLong.
