IMPLEMENTATION MODULE BitVect;

<*-IOVERFLOW   *>
<*-COVERFLOW   *>
<*-CHECKDINDEX *>
<*-CHECKINDEX  *>
<*-CHECKRANGE  *>
<*-CHECKNIL    *>
<*-CHECKDIV    *>
<*-CHECKPROC   *>
<*-CHECKSET    *>
<*-CHECKTYPE   *>
<* IF backend # "C" THEN *>
<*-GENFRAME    *>
<*+DOREORDER   *>
<* END *>

<*-GENDEBUG    *>
<* NEW o_in_bt+ *>

IMPORT lh := LocalHeap;
IMPORT xiEnv, SYSTEM;

PROCEDURE In (p: BitVector; e: INT): BOOLEAN;
<* IF TARGET_386 AND fast_bitvect_in THEN *>
<* NEW no_set_range_check+ *>
VAR foo:LongBitVector;
BEGIN
    foo := SYSTEM.CAST(LongBitVector,p);
    RETURN e IN foo^.v;
<* ELSE *>
  BEGIN
  <* IF backend = "C" THEN *>
      RETURN ({e MOD BitsPerSet} * p^.v [VAL (CARDINAL, e) / BitsPerSet]) # {};
  <* ELSE *>
      RETURN ({e MOD BitsPerSet} * p^.v [e DIV BitsPerSet]) # {};
  <* END *>
<* END *>
END In;

PROCEDURE Incl (p: BitVector; e: INT);
BEGIN
<* IF backend = "C" THEN *>
    INCL (p^.v [VAL (CARDINAL, e)  / BitsPerSet], e MOD BitsPerSet);
<* ELSE *>
    INCL (p^.v [e DIV BitsPerSet], e MOD BitsPerSet);
<* END *>
END Incl;

PROCEDURE Excl (p: BitVector; e: INT);
BEGIN
<* IF backend = "C" THEN *>
    EXCL (p^.v [VAL (CARDINAL, e)  / BitsPerSet], e MOD BitsPerSet);
<* ELSE *>
    EXCL (p^.v [e DIV BitsPerSet], e MOD BitsPerSet);
<* END *>
END Excl;

(*
  Fill bitvector by TRUE/FALSE
*)

PROCEDURE Fill (p: BitVector; filler: BOOLEAN; n: INT);
VAR i, l: INT;
BEGIN
<* IF backend = "C" THEN *>
    l := n / BitsPerSet;
<* ELSE *>
    l := n DIV BitsPerSet;
<* END *>
    IF filler THEN
        FOR i:=0 TO l-1 DO
            p^.v [i] := {0..BitsPerSet-1};
        END;
        IF n MOD BitsPerSet <> 0 THEN
            p^.v [l] := {0..n MOD BitsPerSet - 1};
        END;
    ELSE
        FOR i:=0 TO l-1 DO
            p^.v [i] := {};
        END;
        IF n MOD BitsPerSet <> 0 THEN
            p^.v [l] := {};
        END;
    END;
END Fill;

(*
  Create new bitvector
*)


PROCEDURE ERROR;
BEGIN
    xiEnv.errors^.Fault (xiEnv.null_pos, 950);
    HALT;
END ERROR;


PROCEDURE New_Ex (heap: lh.heapID; n: INT; filler: BOOLEAN): BitVector;
VAR p: BitVector;
    k: INT;
BEGIN
    IF n = 0 THEN
        RETURN NIL;
    END;
<* IF backend = "C" THEN *>
    k := (n + BitsPerSet - 1) / BitsPerSet;
<* ELSE *>
    k := (n + BitsPerSet - 1) DIV BitsPerSet;
<* END *>
    lh.ALLOCATE (heap, p, SIZE (INT) + k * SIZE (BITSET));
    IF p = NIL THEN ERROR END;
    p^.n := k;
    Fill (p, filler, n);
    RETURN p;
END New_Ex;

PROCEDURE New(n: INT; filler: BOOLEAN): BitVector;
BEGIN
  RETURN New_Ex(defaultHeap, n, filler);
END New;


(*
  Make new copy of source bitvector
*)

PROCEDURE MakeCopy (s: BitVector): BitVector;
VAR p: BitVector;
BEGIN
    IF s = NIL THEN
        RETURN NIL;
    END;
    lh.ALLOCATE (defaultHeap, p, SIZE (INT) + s^.n * SIZE (BITSET));
    p^.n := s^.n;
    Move (s, p);
    RETURN p;
END MakeCopy;

(*
  Dispose bitvector
*)

PROCEDURE Free_Ex (heap: lh.heapID; VAR p: BitVector);
BEGIN
  IF p <> NIL THEN
    lh.DEALLOCATE (heap, p, SIZE (INT) + p^.n * SIZE (BITSET));
  END;
END Free_Ex;

PROCEDURE Free (VAR p: BitVector);
BEGIN
  Free_Ex(defaultHeap, p);
END Free;

(*
  s := d
*)

PROCEDURE Move (s, d: BitVector);
VAR i: INT;
BEGIN
    FOR i:=0 TO d^.n - 1 DO
        d^.v [i] := s^.v [i];
    END;
END Move;

(*
  s := d (d shorter than s)
  THERE MAY BE GARABAGE AT UNUSED BITS OF d!
*)

PROCEDURE Shorten (s, d: BitVector);
VAR i: INT;
BEGIN
    FOR i:=0 TO d^.n - 1 DO
        d^.v [i] := s^.v [i];
    END;
END Shorten;

(*
  d := s
  return CHANGED
*)

PROCEDURE Assign (s, d: BitVector): BOOLEAN;
VAR i, n: INT;
BEGIN
    i := 0;
    n := d^.n;
    LOOP
        IF d^.v [i] <> s^.v [i] THEN
            EXIT;
        END;
        INC (i);
        IF i > n-1 THEN
            RETURN FALSE;
        END;
    END;
    FOR i:=i TO n-1 DO
        d^.v [i] := s^.v [i];
    END;
    RETURN TRUE;
END Assign;

(*
  Add new element(s) to bitvector
*)

PROCEDURE Realloc (VAR p: BitVector; n: INT);
VAR q: BitVector;
    i: INT;
BEGIN
    IF p = NIL THEN
        IF n <> 0 THEN
            p := New (n, FALSE);
        END;
<* IF backend = "C" THEN *>
    ELSIF p^.n <> (n + BitsPerSet - 1) / BitsPerSet THEN
<* ELSE *>
    ELSIF p^.n <> (n + BitsPerSet - 1) DIV BitsPerSet THEN
<* END *>
        q := New (n, FALSE);
        FOR i:=0 TO p^.n-1 DO
            q^.v [i] := p^.v [i];
        END;
        Free (p);
        p := q;
    END;
END Realloc;

(*
  r := p1 * p2
*)

PROCEDURE Intersect  (p1, p2, r: BitVector);
VAR i: INT;
BEGIN
    FOR i:=0 TO r^.n - 1 DO
        r^.v [i] := p1^.v [i] * p2^.v [i];
    END;
END Intersect;

(*
  RETURN p1 * p2 <> {}
*)

PROCEDURE Intersecting (p1, p2: BitVector): BOOLEAN;
VAR i: INT;
BEGIN
    FOR i:=0 TO p1^.n - 1 DO
        IF p1^.v [i] * p2^.v [i] <> {} THEN
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END Intersecting;

(*
  r := p1 + p2
*)

PROCEDURE Union (p1, p2, r: BitVector);
VAR i: INT;
BEGIN
    FOR i:=0 TO r^.n - 1 DO
        r^.v [i] := p1^.v [i] + p2^.v [i];
    END;
END Union;

(*
  d += s (assumes s <= d)
  return CHANGED
*)

PROCEDURE UnionAssign (d, s: BitVector): BOOLEAN;
VAR b: BOOLEAN;
    i: INT;
    r: BITSET;
BEGIN
    b := FALSE;
    FOR i:=0 TO d^.n - 1 DO
        r := d^.v [i] + s^.v [i];
        IF r <> d^.v [i] THEN
           d^.v [i] := r;
           b := TRUE;
        END;
    END;
    RETURN b;
END UnionAssign;

(*
  r := p1 - p2
*)

PROCEDURE Sub (p1, p2, r: BitVector);
VAR i: INT;
BEGIN
    FOR i:=0 TO r^.n - 1 DO
        r^.v [i] := p1^.v [i] - p2^.v [i];
    END;
END Sub;

(*
  p1 = p2 ?
*)

PROCEDURE Eq (p1, p2: BitVector): BOOLEAN;
VAR i: INT;
BEGIN
    FOR i:=0 TO p1^.n - 1 DO
        IF p1^.v [i] <> p2^.v [i] THEN
            RETURN FALSE;
        END;
    END;
    RETURN TRUE;
END Eq;

(*
  p1 <= p2 ?
*)

PROCEDURE Subset (p1, p2: BitVector): BOOLEAN;
VAR i: INT;
    r: BITSET;
BEGIN
    FOR i:=0 TO p1^.n - 1 DO
        r := p1^.v [i] + p2^.v [i];
        IF r <> p2^.v [i] THEN
            RETURN FALSE;
        END;
    END;
    RETURN TRUE;
END Subset;

PROCEDURE IsEmpty( v: BitVector ) : BOOLEAN;
VAR i:  INT;
BEGIN
    IF v = NIL THEN RETURN TRUE; END;
    FOR i := 0 TO v^.n-1 DO
      IF v^.v[i] # {} THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
END IsEmpty;

BEGIN
  lh.Create(defaultHeap);
END BitVect.
