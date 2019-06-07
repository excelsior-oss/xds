<* IF __GEN_X86__ THEN *>
<*   -NOPTRALIAS *>
<* END *>
IMPLEMENTATION MODULE LowReal;

<* +M2EXTENSIONS *>
IMPORT SYSTEM, EXCEPTIONS, math:=xMath;
<* -M2EXTENSIONS *>

VAR source: EXCEPTIONS.ExceptionSource;

PROCEDURE raise;
BEGIN
    EXCEPTIONS.RAISE (source, 0, "LowReal.lowException");
END raise;

PROCEDURE exponent (x: REAL): INTEGER;
  (* Returns the exponent value of x *)
  VAR i: SYSTEM.int;
      z: LONGREAL;
BEGIN
  z := math.X2C_frexp (LFLOAT (x), i);
  RETURN i;
END exponent;

PROCEDURE fraction (x: REAL): REAL;
  (* Returns the significand (or significant part) of x *)
  VAR i: SYSTEM.int;
BEGIN
  RETURN FLOAT (math.X2C_frexp (LFLOAT (x), i));
END fraction;

PROCEDURE sign (x: REAL): REAL;
  (* Returns the signum of x *)
BEGIN
  IF    x < 0.0 THEN RETURN -1.0
  ELSIF x = 0.0 THEN RETURN  0.0
  ELSE               RETURN  1.0
  END;
END sign;

PROCEDURE succ (x: REAL): REAL;
  (* Returns the next value of the type LONGREAL greater than x *)
BEGIN
  RETURN x + ulp (x)
END succ;

PROCEDURE ulp (x: REAL): REAL;
  (* Returns the value of a unit in the last place of x *)
  VAR pb: POINTER TO BITSET;
BEGIN
  pb := SYSTEM.ADR (x);
  INCL (pb^, 0);
  RETURN ABS (x - trunc (x, 22))
END ulp;

PROCEDURE pred (x: REAL): REAL;
  (* Returns the previous value of the type LONGREAL less than x *)
BEGIN
  RETURN x - ulp (x)
END pred;

PROCEDURE intpart (x: REAL): REAL;
  (* Returns the integer part of x *)
  VAR y, z: LONGREAL;
BEGIN
  z := math.X2C_modf (LFLOAT (x), y);
  RETURN FLOAT (y);
END intpart;

PROCEDURE fractpart (x: REAL): REAL;
  (* Returns the fractional part of x *)
  VAR y: LONGREAL;
BEGIN
  RETURN FLOAT (math.X2C_modf (LFLOAT (x), y));
END fractpart;

PROCEDURE scale (x: REAL; n: INTEGER): REAL;
  (* Returns the value of x * radix ** n *)
BEGIN
  RETURN FLOAT (math.X2C_ldexp (LFLOAT (x), n));
END scale;

PROCEDURE trunc (x: REAL; n: INTEGER): REAL;
  (* Returns the value of the first n places of x *)
  VAR pb: POINTER TO BITSET;
BEGIN
  IF n <= 0 THEN raise END;
  IF n >= 23 THEN RETURN x END;
  pb := SYSTEM.ADR (x);
  pb^ := pb^ - {0 .. 23 - n - 1};
  RETURN x
END trunc;

PROCEDURE round (x: REAL; n: INTEGER): REAL;
  (* Returns the value of x rounded to the first n places *)
BEGIN
  RETURN trunc (x, n);
END round;

PROCEDURE synthesize (n: INTEGER; x: REAL): REAL;
  (* Returns a value of the type LONGREAL constructed from the given expart and frapart *)
BEGIN
  RETURN FLOAT (math.X2C_ldexp (LFLOAT (x), n));
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
  RETURN EXCEPTIONS.IsCurrentSource (source)
END IsLowException;

BEGIN
  EXCEPTIONS.AllocateSource (source);
END LowReal.
