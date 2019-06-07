-- Операции над 64-битными целыми числами

IMPLEMENTATION MODULE Int64;

IMPORT sys := SYSTEM;


PROCEDURE ["C"] / X2J_ADD64 (a_low, a_high, b_low, b_high: sys.CARD32): sys.INT64;

PROCEDURE ["C"] / X2J_SUB64 (a_low, a_high, b_low, b_high: sys.CARD32): sys.INT64;

PROCEDURE ["C"] / X2J_MUL64 (a_low, a_high, b_low, b_high: sys.CARD32): sys.INT64;

PROCEDURE ["C"] / X2J_DIV64 (a_low, a_high, b_low, b_high: sys.CARD32): sys.INT64;


PROCEDURE ["C"] / X2J_LESS64 (a_low, a_high, b_low, b_high: sys.CARD32): BOOLEAN;

PROCEDURE ["C"] / X2J_GTR64 (a_low, a_high, b_low, b_high: sys.CARD32): BOOLEAN;



PROCEDURE int (low, high: CARDINAL): INT64;
VAR
  tmp: INT64;
BEGIN
  tmp.low := low;
  tmp.high := high;
  RETURN tmp;
END int;



PROCEDURE int2real (a: INT64): LONGREAL;
BEGIN
  RETURN VAL(LONGREAL,a.low) + VAL(LONGREAL,a.high) * (VAL(LONGREAL,MAX(CARDINAL))+1.0);
END int2real;



PROCEDURE add (a, b: INT64): INT64;
BEGIN
  RETURN sys.CAST (INT64, X2J_ADD64 (a.low, a.high, b.low, b.high));
END add;


PROCEDURE sub (a, b: INT64): INT64;
BEGIN
  RETURN sys.CAST (INT64, X2J_SUB64 (a.low, a.high, b.low, b.high));
END sub;


PROCEDURE mul (a, b: INT64): INT64;
BEGIN
  RETURN sys.CAST (INT64, X2J_MUL64 (a.low, a.high, b.low, b.high));
END mul;


PROCEDURE div (a, b: INT64): INT64;
BEGIN
  RETURN sys.CAST (INT64, X2J_DIV64 (a.low, a.high, b.low, b.high));
END div;



PROCEDURE lss (a, b: INT64): BOOLEAN;
BEGIN
  RETURN X2J_LESS64 (a.low, a.high, b.low, b.high);
END lss;


PROCEDURE gtr (a, b: INT64): BOOLEAN;
BEGIN
  RETURN X2J_GTR64 (a.low, a.high, b.low, b.high);
END gtr;


END Int64.

