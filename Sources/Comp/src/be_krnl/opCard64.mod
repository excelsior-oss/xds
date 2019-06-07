(*
  This module will not work with overflow checks - do not turn on!
*)

<*-COVERFLOW*>
<*-IOVERFLOW*>
<*+M2EXTENSIONS*>

IMPLEMENTATION MODULE opCard64;

IMPORT SYSTEM;

CONST
    N = 32;
    M = 80000000H;

PROCEDURE lo (x-: dword): word;
BEGIN
    RETURN x.lo;
END lo;

PROCEDURE lshift (x-: dword): dword;
BEGIN
    RETURN dword {x.lo + x.lo, x.hi + x.hi + VAL(word, x.lo >= M)};
END lshift;

PROCEDURE lshift_n (x: dword; n: int): dword;
BEGIN
    WHILE n > 0 DO
        x := lshift (x);
        DEC (n);
    END;
    RETURN x;
END lshift_n;

PROCEDURE u_rshift (x: dword): dword;
BEGIN
    x.lo := x.lo DIV 2;
    IF ODD (x.hi) THEN
        INC (x.lo, M);
    END;
    x.hi := x.hi DIV 2;
    RETURN x;
END u_rshift;

PROCEDURE add (x-, y-: dword): dword;
VAR z: dword;
BEGIN
    z.lo := x.lo + y.lo;
    z.hi := x.hi + y.hi + VAL(word, (z.lo < x.lo) OR (z.lo < y.lo));
    RETURN z;
END add;

PROCEDURE sub (x-, y-: dword): dword;
BEGIN
    RETURN dword { x.lo - y.lo, x.hi - y.hi - VAL(word, x.lo < y.lo)};
END sub;

PROCEDURE ge (a-, b-: dword): BOOLEAN;
BEGIN
    RETURN (a.hi > b.hi) OR (a.hi = b.hi) & (a.lo >= b.lo);
END ge;

PROCEDURE log2 (d: word): word;
VAR i, j: word;
BEGIN
    i := 0;
    j := 0;
    WHILE d > 1 DO
        IF ODD (d) THEN
            j := 1;
        END;
        d := d DIV 2;
        INC (i);
    END;
    RETURN i + j;
END log2;

PROCEDURE pow2 (n: int): dword;
VAR x: dword;
BEGIN
    ASSERT (n >= 0);
    ASSERT (n <= 63);
    IF n < N THEN
        x.lo := SYSTEM.CAST (word, {n});
        x.hi := 0;
    ELSE
        x.lo := 0;
        x.hi := SYSTEM.CAST (word, {n - N});
    END;
    RETURN x;
END pow2;

PROCEDURE is_pow2 (n: word): BOOLEAN;
BEGIN
    RETURN SYSTEM.CAST (BITSET, n) * SYSTEM.CAST (BITSET, n - 1) = {};
END is_pow2;

PROCEDURE div (x: dword; y: word): dword;
VAR r, z, t: dword;
    i:       int;
BEGIN
    z := dword { y, 0 };
    i := 0;
    LOOP
        IF z.hi >= M THEN
            EXIT;
        END;
        t := lshift (z);
        IF NOT ge (x, t) THEN
            EXIT;
        END;
        z := t;
        INC (i);
    END;
    r := dword { 0, 0 };
    t := dword { y, 0 };
    WHILE ge (x, t) DO
        IF ge (x, z) THEN
            r := add (r, pow2 (i));
            x := sub (x, z);
        END;
        z := u_rshift (z);
        DEC (i);
    END;
    RETURN r;
END div;

PROCEDURE labs (x: word): word;
BEGIN
    RETURN SYSTEM.CAST (word, ABS (SYSTEM.CAST (int, x)));
END labs;

PROCEDURE neg (x: word): word;
BEGIN
    RETURN SYSTEM.CAST (word, - (SYSTEM.CAST (int, x)));
END neg;

END opCard64.
