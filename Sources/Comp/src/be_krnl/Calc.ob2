MODULE Calc;

IMPORT pc   := pcK,
       xiEnv,
       tune := opTune,
       ir_def,
       SYSTEM;


(* -------------------------------------------------------------------------- *)

TYPE    FLOAT          = ir_def.FLOAT;
        TypeType*      = ir_def.TypeType;
        SizeType*      = ir_def.SizeType;
        TypeTypeSet    = ir_def.TypeTypeSet;
        SizeTypeRange  = ir_def.SizeTypeRange;

CONST t_int*     = ir_def.t_int;
      t_unsign*  = ir_def.t_unsign;
      t_ref*     = ir_def.t_ref;
      t_float*   = ir_def.t_float;
      t_complex* = ir_def.t_complex;
      t_ZZ*      = ir_def.t_ZZ;
      t_RR*      = ir_def.t_RR;

(*

CONST
        un_not         *= pc.su_not;
        un_abs         *= pc.su_abs;
        un_neg         *= pc.su_neg;
        un_odd         *= pc.su_odd;
        un_cap         *= pc.su_cap;
        un_compl       *= pc.su_compl;
        un_entier      *= pc.su_entier;
        un_im          *= pc.su_im;
        un_re          *= pc.su_re;
        un_length      *= pc.su_length;

        bin_add        *= pc.sb_plus;
        bin_sub        *= pc.sb_minus;
        bin_mul        *= pc.sb_mul;
        bin_div        *= pc.sb_div;
        bin_mod        *= pc.sb_mod;
        bin_rem        *= pc.sb_rem;
        bin_dvd        *= pc.sb_slash;
        bin_pow        *= pc.sb_exp;
        bin_and        *= pc.sb_and;
        bin_or         *= pc.sb_or;
        bin_xor        *= pc.sb_xor;
        bin_andnot     *= pc.sb_bic;

        bin_equ        *= pc.sb_equ;
        bin_neq        *= pc.sb_neq;
        bin_lss        *= pc.sb_lss;
        bin_leq        *= pc.sb_leq;
        bin_gtr        *= pc.sb_gtr;
        bin_geq        *= pc.sb_geq;

        bin_in         *= pc.sb_in;
 *)
(* -------------------------------------------------------------------------- *)

CONST high1 = VAL (SYSTEM.CARD32, 80000000H);


CONST ValueArraySize = 65;

TYPE    VALUE          *= pc.VALUE;

(* -------------------------------------------------------------------------- *)

VAR
        overflow*       : BOOLEAN; -- is always FALSE in Java
        MaxCard8*,
        MaxCard16*,
        MaxCard32*      : SYSTEM.CARD32;
        cmpTmp,
        zzTmp,
        rrTmp,         
        FalseValue,
        TrueValue,
        RRZero,
        RROne,
        NilValue,

        MinusOne, Zero, One, Two : VALUE;

        IntegerValue,
        CardinalValue,
        ZZValue         : ARRAY ValueArraySize OF VALUE;

(* -------------------------------------------------------------------------- *)

PROCEDURE correctSigned(v: VALUE; sz: SizeType);
  VAR i8: SYSTEM.INT8;
     i16: SYSTEM.INT16;
     i32: SYSTEM.INT32;
BEGIN
  IF sz>SIZE(i16) THEN RETURN END;
  IF v.is_neg() THEN
    i32:=v.get_integer();
  ELSE
    i32:=SYSTEM.VAL(SYSTEM.INT32,v.get_cardinal());
  END;
  IF sz=2 THEN
    i16:=SYSTEM.VAL(SYSTEM.INT16,i32);
    i32:=VAL(SYSTEM.INT32,i16);
  ELSE
    i8:=SYSTEM.VAL(SYSTEM.INT8,i32);
    i32:=VAL(SYSTEM.INT32,i8);
  END;
  v.set_integer(i32);
END correctSigned;

PROCEDURE correctUnsigned(v: VALUE; sz: SizeType);
  VAR c8: SYSTEM.CARD8;
     c16: SYSTEM.CARD16;
     c32: SYSTEM.CARD32;
       i: SYSTEM.INT32;
BEGIN
  IF sz>SIZE(c16) THEN RETURN END;
  IF v.is_neg() THEN
    c32:=SYSTEM.VAL(SYSTEM.CARD32,v.get_integer());
  ELSE
    c32:=v.get_cardinal();
  END;
  IF sz=2 THEN
    c16:=SYSTEM.VAL(SYSTEM.CARD16,c32);
    c32:=VAL(SYSTEM.CARD32,c16);
  ELSE
    c8:=SYSTEM.VAL(SYSTEM.CARD8,c32);
    c32:=VAL(SYSTEM.CARD32,c8);
  END;
  i:=SYSTEM.VAL(SYSTEM.INT32,c32);
  v.set_integer(i);
END correctUnsigned;

PROCEDURE TypeStruct(t: TypeType; sz: SizeType): pc.STRUCT;
BEGIN
  CASE t OF
  | t_int:
    CASE sz OF
    | 1 : RETURN pc.shortint_type;
    | 2 : RETURN pc.integer_type;
    | 4 : RETURN pc.longint_type;
    | 8 : RETURN pc.longlongint_type;
    END;

  | t_unsign:
    CASE sz OF
    | 1 : RETURN pc.shortcard_type;
    | 2 : RETURN pc.cardinal_type;
    | 4 : RETURN pc.longcard_type;
    | 8 : RETURN pc.longlongcard_type;
    END;

  | t_float:
    CASE sz OF
    | tune.real_sz     : RETURN pc.real_type;
    | tune.longreal_sz : RETURN pc.longreal_type;
    ELSE
      ASSERT(sz = tune.IDB.ld_real_sz);
      RETURN pc.ld_real_type;
    END;

  | t_ref:
    ASSERT( sz = 4 );
    RETURN pc.AA_type;

  | t_ZZ:
    ASSERT( sz = 0 );
    RETURN pc.ZZ_type;

  | t_RR:
    ASSERT( sz = 0 );
    RETURN pc.RR_type;
  END;
END TypeStruct;

PROCEDURE TypeKind (t: pc.STRUCT): TypeType;
BEGIN
  CASE t.mode OF
    |pc.ty_shortcard
    ,pc.ty_cardinal
    ,pc.ty_longcard
    ,pc.ty_longlongcard
    ,pc.ty_uchar
                     : RETURN t_unsign;
    |pc.ty_shortint
    ,pc.ty_integer
    ,pc.ty_longint
    ,pc.ty_longlongint
                     : RETURN t_int
    |pc.ty_real
    ,pc.ty_longreal
    ,pc.ty_ld_real
                     : RETURN t_float
    |pc.ty_complex
    ,pc.ty_lcomplex
                     : RETURN t_complex
    |pc.ty_boolean   : RETURN t_unsign
    |pc.ty_char      : RETURN t_int;

    |pc.ty_pointer
    ,pc.ty_opaque
    ,pc.ty_AA
                     : RETURN t_ref

    |pc.ty_proctype  : RETURN t_ref
    |pc.ty_ZZ        : RETURN t_ZZ
  ELSE
    ASSERT(FALSE);
  END;
END TypeKind;

PROCEDURE Type*(v: VALUE): TypeType;
BEGIN
    RETURN TypeKind(v.get_type());
END Type;
(*
this code is never used in java
PROCEDURE TypeSize(t: pc.STRUCT): SizeType;
BEGIN
    CASE t.mode OF
    | pc.ty_shortint  : RETURN 1;
    | pc.ty_integer   : RETURN 2;
    | pc.ty_longint   : RETURN 4;
    | pc.ty_longlongint:RETURN 8;
    | pc.ty_shortcard : RETURN 1;
    | pc.ty_cardinal  : RETURN 2;
    | pc.ty_longcard  : RETURN 4;
    | pc.ty_real      : RETURN tune.real_sz;
    | pc.ty_longreal  : RETURN tune.longreal_sz;
    | pc.ty_ld_real   : RETURN tune.IDB.ld_real_sz;
    | pc.ty_ZZ        : RETURN 0;
    ELSE
      RETURN 4;
    END;
END TypeSize;

PROCEDURE Size*(v: VALUE): SizeType;
BEGIN
    RETURN TypeSize(v.get_type());
END Size;
*)
PROCEDURE BitSize( mode: pc.TY_MODE ) : LONGINT;
BEGIN
  CASE mode OF
  | pc.ty_shortint,
    pc.ty_shortcard,
    pc.ty_char    :  RETURN 8;
  | pc.ty_integer,
    pc.ty_cardinal:  RETURN 16;
  | pc.ty_longint,
    pc.ty_longcard:  RETURN 32;

  | pc.ty_longlongint,
    pc.ty_longlongcard,
    pc.ty_ZZ      :
                     RETURN 64;
  | pc.ty_uchar   :  RETURN 16;
  END;
END BitSize;


(* -------------------------------------------------------------------------- *)

PROCEDURE ^ NewValue(v: LONGINT; t: TypeType; sz: SizeType): VALUE;

PROCEDURE Cast* (v: VALUE; t: TypeType; sz: SizeType): VALUE;
VAR r: VALUE;
BEGIN
    CASE t OF
    | t_int,
      t_ref,
      t_unsign,
      t_ZZ    :     IF v.is_ZZ () THEN
                        RETURN v;
                    END;
                    r := pc.value.new (v.pos, pc.ZZ_type);
                    r.unary (pc.su_conv, v);
                    IF t=t_int THEN correctSigned(r,sz)
                    ELSE correctUnsigned(r,sz);
                    END;
                    RETURN r;
    | t_float,
      t_RR   :      IF v.is_RR () THEN
                        RETURN v;
                    END;
                    r := pc.value.new (v.pos, pc.RR_type);
                    r.unary (pc.su_conv, v);
                    RETURN r;
    END;
END Cast;

PROCEDURE SystemCast* (v: VALUE; ft,t: TypeType; fsz,sz: SizeType): VALUE;
VAR r: VALUE;
    real:REAL;
BEGIN
    CASE t OF
    | t_int,
      t_ref,
      t_unsign,
      t_ZZ    :     r := pc.value.new (v.pos, pc.ZZ_type);
                  <* IF TARGET_386 OR TARGET_MIPS THEN *> 
                    -- host and target should comply to IEEE 754 for binary floatig point
                    IF (ft = t_float) AND (fsz = 4) THEN
                      real := VAL(REAL, v.get_real());
                      r.set_integer(SYSTEM.VAL(LONGINT, real));
                    ELSE
                      r.unary (pc.su_cast, v);
                    END;
                  <* ELSE *>
                    ASSERT((ft # t_float) AND (ft # t_RR));
                    r.unary (pc.su_cast, v);
                  <* END *>
                    IF t=t_int THEN correctSigned(r,sz)
                    ELSE            correctUnsigned(r,sz);
                    END;
                    RETURN r;
    | t_float,
      t_RR   :      IF v.is_RR () THEN
                        RETURN v;
                    END;
                    r := pc.value.new (v.pos, pc.RR_type);
                    r.unary (pc.su_cast, v);
                    RETURN r;
    END;
END SystemCast;

(* -------------------------------------------------------------------------- *)

PROCEDURE ToInteger* (p: VALUE; l: SizeType): LONGINT;
VAR r: VALUE;
BEGIN
    r := Cast (p, t_int, l);
    RETURN r.get_integer ();
END ToInteger;

(* -------------------------------------------------------------------------- *)

PROCEDURE ToCardinal* (v: VALUE; l: SizeType): SYSTEM.CARD32;
VAR r: VALUE;
BEGIN
    r := Cast (v, t_unsign, l);
    IF (r = v) & v.is_neg() THEN  (* make a copy -- cast_ordinal can change r *)
       r := pc.value.new (v.pos, pc.ZZ_type);
       r.unary (pc.su_conv, v);
    END;
    r.cast_ordinal(pc.longcard_type);
    RETURN r.get_cardinal ();
END ToCardinal;

(* ------------- NewXXX functions - always create new value ----------------- *)

<* PUSH *>
<* WOFF301+ *>
PROCEDURE NewInteger* (t: LONGINT; sz: SizeType): VALUE;
VAR r: VALUE;
BEGIN
    r := pc.value.new (xiEnv.null_pos, pc.ZZ_type);
    r.set_integer (t);
    RETURN r;
END NewInteger;
<* POP *>

(* -------------------------------------------------------------------------- *)

PROCEDURE NewCardinal* (t: SYSTEM.CARD32; l: SizeType): VALUE;
VAR r: VALUE;
BEGIN
    IF t < high1 THEN
        RETURN NewInteger (t, l);
    END;
    r := NewInteger (t DIV 2, l);
    r.binary (pc.sb_mul, r, Two);
    IF ODD (t) THEN
        r.binary (pc.sb_plus, r, One);
    END;
    RETURN r;
END NewCardinal;

(* -------------------------------------------------------------------------- *)

PROCEDURE NewBoolean*(v: BOOLEAN; sz: SizeType): VALUE;
BEGIN
    RETURN NewCardinal(ORD(v), sz);
END NewBoolean;

(* -------------------------------------------------------------------------- *)

PROCEDURE NewSet* (p: SET; l: SizeType): VALUE;
BEGIN
    RETURN NewCardinal (SYSTEM.VAL (SYSTEM.CARD32, p * { 0 .. 8 * l - 1 }), l);
END NewSet;

(* -------------------------------------------------------------------------- *)

PROCEDURE NewReal* (t: FLOAT): VALUE;
VAR r: VALUE;
BEGIN
    r := pc.value.new (xiEnv.null_pos, pc.RR_type);
    r . set_real (t);
    RETURN r;
END NewReal;

(* -------------------------------------------------------------------------- *)

PROCEDURE NewNilPointer*(): VALUE;
VAR r: VALUE;
BEGIN
    r := pc.value.new (xiEnv.null_pos, pc.AA_type);
    RETURN r;
END NewNilPointer;

(* -------------------------------------------------------------------------- *)

PROCEDURE NewZZValue*(t: LONGINT): VALUE;
VAR r: VALUE;
BEGIN
    r := pc.value.new (xiEnv.null_pos, pc.ZZ_type);
    r.set_integer(t);
    RETURN r;
END NewZZValue;

(* -------------------------------------------------------------------------- *)

PROCEDURE NewRRValue*(t: FLOAT): VALUE;
VAR r: VALUE;
BEGIN
    r := pc.value.new (xiEnv.null_pos, pc.RR_type);
    r.set_real(t);
    RETURN r;
END NewRRValue;

(* -------------------------------------------------------------------------- *)

<* PUSH *>
<* WOFF301+ *>
PROCEDURE NewFloat* (t: FLOAT; sz: SizeType): VALUE;
BEGIN
  RETURN NewRRValue(t);
END NewFloat;
<* POP *>

(* -------------------------------------------------------------------------- *)

PROCEDURE NewValue*( x: LONGINT; ty: TypeType; sz: SizeType ): VALUE;
BEGIN
    CASE( ty ) OF
    |  t_int    : RETURN NewInteger(x, sz);
    |  t_unsign : RETURN NewCardinal(x, sz);
    |  t_float  : RETURN NewFloat(x, sz);
    |  t_ref    : ASSERT(x=0);  RETURN NewNilPointer();
    |  t_ZZ     : ASSERT(sz=0); RETURN NewZZValue(x);
    |  t_RR     : ASSERT(sz=0); RETURN NewRRValue(x);
    ELSE
    (*
    |  t_void
    |  t_complex
    |  t_arr
    |  t_rec
    |  t_flxarr
    *)
       ASSERT(FALSE, ORD(ty));
    END;
END NewValue;

(* ------------------ GetXXX() functions - don't create new values----------- *)

<* PUSH *>
<* WOFF301+ *>
PROCEDURE GetBoolean*(v: BOOLEAN; sz: SizeType): VALUE;
BEGIN
    IF v THEN
        RETURN TrueValue;
    ELSE
        RETURN FalseValue;
    END;
END GetBoolean;
<* POP *>

PROCEDURE GetInteger* (t: LONGINT; l: SizeType): VALUE;
BEGIN
    IF (t >=0 ) AND (t < ValueArraySize) THEN
       RETURN IntegerValue[t];
    ELSE
       RETURN NewInteger(t,l);
    END;
END GetInteger;

PROCEDURE GetCardinal* (t: SYSTEM.CARD32; l: SizeType): VALUE;
BEGIN
    IF t < ValueArraySize THEN
       RETURN CardinalValue[t];
    ELSE
       RETURN NewCardinal(t,l);
    END;
END GetCardinal;

PROCEDURE GetNilPointer*(): VALUE;
BEGIN
    RETURN NilValue;
END GetNilPointer;

PROCEDURE GetZZValue*(t: LONGINT): VALUE;
BEGIN
    IF (t >=0 ) AND (t < ValueArraySize) THEN
       RETURN ZZValue[t];
    ELSE
       RETURN NewZZValue(t);
    END;
END GetZZValue;

PROCEDURE GetRRValue*(t: FLOAT): VALUE;
BEGIN
    IF t = 0.0 THEN
        RETURN RRZero;
    ELSIF t = 1.1 THEN
        RETURN RROne;
    ELSE
        RETURN NewRRValue(t);
    END;
END GetRRValue;

<* PUSH *>
<* WOFF301+ *>
PROCEDURE GetFloat* (t: FLOAT; sz: SizeType): VALUE;
BEGIN
  RETURN GetRRValue(t);
END GetFloat;
<* POP *>

(* suxx
PROCEDURE GetSet* (p: SET; l: SizeType): VALUE;
BEGIN
    RETURN GetCardinal (SYSTEM.VAL (SYSTEM.CARD32, p * { 0 .. 8 * l - 1 }), l);
END GetSet;
*)

PROCEDURE GetValue*( x: LONGINT; ty: TypeType; sz: SizeType ): VALUE;
BEGIN
    CASE( ty ) OF
    |  t_int    : RETURN GetInteger(x, sz);
    |  t_unsign : RETURN GetCardinal(x, sz);
    |  t_float  : RETURN GetFloat(x, sz);
    |  t_ref    : ASSERT(x=0);  RETURN GetNilPointer();
    |  t_ZZ     : ASSERT(sz=0); RETURN GetZZValue(x);
    |  t_RR     : ASSERT(sz=0); RETURN GetRRValue(x);
    ELSE
    (*
    |  t_void
    |  t_complex
    |  t_arr
    |  t_rec
    |  t_flxarr
    *)
       ASSERT(FALSE, ORD(ty));
    END;
END GetValue;

(* -------------------------------------------------------------------------- *)

PROCEDURE MaxValue* (t: TypeType; s: SizeType): VALUE;
VAR tt: pc.STRUCT;
BEGIN
  tt := TypeStruct(t,s);
  RETURN tt.max;
END MaxValue;

(* -------------------------------------------------------------------------- *)

PROCEDURE MinValue* (t: TypeType; s: SizeType): VALUE;
VAR tt: pc.STRUCT;
BEGIN
  tt := TypeStruct(t,s);
  RETURN tt.min;
END MinValue;

(* -------------------------------------------------------------------------- *)

PROCEDURE RangeOk* (v: VALUE; tp: TypeType; sz: SizeType): BOOLEAN;
VAR r: VALUE;
BEGIN
    r := Cast (v, tp, sz);
    cmpTmp.binary (pc.sb_geq, r, MinValue (tp, sz));
    IF cmpTmp.is_zero () THEN
        RETURN FALSE;
    END;
    cmpTmp.binary (pc.sb_leq, r, MaxValue (tp, sz));
    IF cmpTmp.is_zero () THEN
        RETURN FALSE;
    END;
    RETURN TRUE;
END RangeOk;

(* -------------------------------------------------------------------------- *)

PROCEDURE CheckOverflow (v: VALUE; tp: TypeType; sz: SizeType): VALUE;
VAR c: SYSTEM.CARD32;
BEGIN
    IF sz = 0 THEN
      ASSERT( tp IN TypeTypeSet{ t_ZZ, t_RR });
      overflow := FALSE;
      RETURN v;
    END;

    overflow := NOT RangeOk (v, tp, sz);
    IF overflow & (tp <> t_float) THEN
        v.binary (pc.sb_and, v, MaxValue (t_unsign, sz));
        IF tp <> t_unsign THEN
            c := ToCardinal (v, sz);
            CASE sz OF
            | 1:    v.set_integer (SYSTEM.VAL (SHORTINT, c));
            | 2:    v.set_integer (SYSTEM.VAL (INTEGER,  c));
            | 4:    v.set_integer (SYSTEM.VAL (LONGINT,  c));
            END;
        END;
    END;
    RETURN v;
END CheckOverflow;

(* -------------------------------------------------------------------------- *)

PROCEDURE Unary* (op: pc.SUB_MODE; t: TypeType; s: SizeType; p: VALUE): VALUE;
VAR res: VALUE;
BEGIN
    CASE t OF
    | t_int,
      t_ref,
      t_unsign,
      t_ZZ    :     res := pc.value.new (p.pos, pc.ZZ_type);
    | t_float,
      t_RR    :     res := pc.value.new (p.pos, pc.RR_type);
    | t_complex:    res := pc.value.new (p.pos, pc.CC_type);
    END;
    IF (op # pc.su_re) AND (op # pc.su_im) THEN
        p := Cast(p, t, s);
    END;
    res.unary (op, p);
    RETURN CheckOverflow (res, t, s);
END Unary;

(* -------------------------------------------------------------------------- *)

PROCEDURE Binary* (op: pc.SUB_MODE; t: TypeType; s: SizeType; p1, p2: VALUE): VALUE;
VAR res, r: VALUE;
BEGIN
    CASE t OF
    | t_int,
      t_ref,
      t_unsign,
      t_ZZ   :     res := pc.value.new (p2.pos, pc.ZZ_type);
    | t_float,
      t_RR   :     res := pc.value.new (p2.pos, pc.RR_type);
    END;
    r := Cast (p2, t, s);
    IF ((op = pc.sb_div) OR (op = pc.sb_slash) OR (op = pc.sb_mod) OR (op = pc.sb_rem)) &
       r.is_zero ()
    THEN
        overflow := TRUE;
        RETURN NewInteger (0, s);
    ELSE
        overflow := FALSE;
        res.binary (op, Cast (p1, t, s), r);
        RETURN CheckOverflow (res, t, s);
    END;
END Binary;

(* -------------------------------------------------------------------------- *)

<* PUSH *>
<* WOFF301+ *>
PROCEDURE CompareValues* (op: pc.SUB_MODE; p1, p2: VALUE;
                          t: TypeType; s: SizeType;
                          strict: BOOLEAN): BOOLEAN;
BEGIN
    cmpTmp.binary (op, Cast (p1, t, s), Cast (p2, t, s));
    RETURN NOT cmpTmp.is_zero ();
END CompareValues;
<* POP *>

(* -------------------------------------------------------------------------- *)

PROCEDURE CompareWithInt* (op: pc.SUB_MODE; p1: VALUE; p2: LONGINT;
                           t: TypeType; l: SizeType): BOOLEAN;
BEGIN
    zzTmp.set_integer (p2);
    cmpTmp.binary (op, Cast (p1, t, l), zzTmp);
    RETURN NOT cmpTmp.is_zero ();
END CompareWithInt;

(* -------------------------------------------------------------------------- *)

<* PUSH *>
<* WOFF301+ *>
PROCEDURE CompareWithReal* (op: pc.SUB_MODE; p1: VALUE; p2: FLOAT;
                            l: SizeType; strict: BOOLEAN): BOOLEAN;
BEGIN
    rrTmp.set_real (p2);
    cmpTmp.binary (op, Cast (p1, t_float, l), rrTmp);
    RETURN NOT cmpTmp.is_zero ();
END CompareWithReal;
<* POP *>

(* -------------------------------------------------------------------------- *)

PROCEDURE IsZero* (p: VALUE; t: TypeType; s: SizeType): BOOLEAN;
VAR r: VALUE;
BEGIN
    r := Cast (p, t, s);
    RETURN r.is_zero ();
END IsZero;

(* -------------------------------------------------------------------------- *)

PROCEDURE IsOnes* (p: VALUE; tp: TypeType; sz: SizeType): BOOLEAN;
BEGIN
    IF tp = t_unsign THEN
        RETURN CompareValues (pc.sb_equ, Cast (p, tp, sz),
                              MaxValue (tp, sz), tp, sz, FALSE);
    ELSE
        RETURN CompareValues (pc.sb_equ, Cast (p, tp, sz),
                              MinusOne, tp, sz, FALSE);
    END;
END IsOnes;

(* -------------------------------------------------------------------------- *)

PROCEDURE IsNegative* (p: VALUE; l: SizeType): BOOLEAN;
VAR r: VALUE;
BEGIN
    r := Cast (p, t_int, l);
    RETURN r.is_neg ();
END IsNegative;

(* -------------------------------------------------------------------------- *)

PROCEDURE ToReal* (p: VALUE; l: SizeType): FLOAT;
VAR r: VALUE;
    x: FLOAT;
BEGIN
    r := Cast (p, t_float, l);
    x := VAL (FLOAT, r.get_real ());
    IF x = 0.0 THEN             -- Идиотизм, конечно, но нужно из-за -0.0
        RETURN 0.0;
    ELSE
        RETURN x;
    END;
END ToReal;

(* -------------------------------------------------------------------------- *)

PROCEDURE SetConstructor* (p1, p2: LONGINT; sz: SizeType): VALUE;
BEGIN
    RETURN NewSet ({ p1 .. p2 }, sz);
END SetConstructor;

(* -------------------------------------------------------------------------- *)

<* PUSH *>
<* WOFF301+ *>
PROCEDURE Val* (p: VALUE; ft, tt: TypeType; fl, tl: SizeType): VALUE;
VAR r: VALUE;
BEGIN
    overflow := FALSE;
    CASE tt OF
    | t_int,
      t_unsign,
      t_ref,
      t_ZZ :        r := pc.value.new (xiEnv.null_pos, pc.ZZ_type);
                    r.unary (pc.su_conv, p);
                    IF (ft = tt) THEN
                      IF tt=t_int THEN correctSigned(r,fl);
                      ELSE            correctUnsigned(r,fl);
                      END;
                    END;
    | t_float,
      t_RR   :      r := pc.value.new (xiEnv.null_pos, pc.RR_type);
                    r.unary (pc.su_conv, p);
    END;
    RETURN CheckOverflow (r, tt, tl);
END Val;
<* POP *>

(* -------------------------------------------------------------------------- *)

(*
  Сколько в слове установлено битов
*)

PROCEDURE NOnes* (p: VALUE; l: SizeType): INTEGER;
VAR n: INTEGER;
    c: SYSTEM.CARD32;
BEGIN
    c := ToCardinal (p, l);
    n := 0;
    WHILE c <> 0 DO
        IF ODD (c) THEN
            INC (n);
        END;
        c := c DIV 2;
    END;
    RETURN n;
END NOnes;

(* -------------------------------------------------------------------------- *)

PROCEDURE IsPowerOfTwo* (v: VALUE; t: TypeType; s: SizeType): INTEGER;
VAR i: LONGINT;
    c: SYSTEM.CARD32;
    j: INTEGER;
BEGIN
    IF (t = t_int) OR (t = t_ref) THEN
        i := ToInteger (v, s);
        FOR j:=0 TO 30 DO
            IF i = ASH (1, j) THEN
                RETURN j;
            END;
        END;
    ELSIF t = t_unsign THEN
        c := ToCardinal (v, s);
        FOR j:=0 TO 30 DO
            IF c = SYSTEM.VAL (SYSTEM.CARD32, ASH (1, j)) THEN
                RETURN j;
            END;
        END;
        IF c = high1 THEN
            RETURN 31;
        END;
    END;
    RETURN -1;
END IsPowerOfTwo;

(* -------------------------------------------------------------------------- *)

PROCEDURE IsNotPowerOfTwo* (v: VALUE; t: TypeType; s: SizeType): INTEGER;
VAR i: LONGINT;
    c: SYSTEM.CARD32;
    j: INTEGER;
BEGIN
    IF (t = t_int) OR (t = t_ref) THEN
        i := ToInteger (v, s);
        IF i = MIN (LONGINT) THEN
            RETURN -1;
        END;
        i := - i - 1;
        FOR j:=0 TO 30 DO
            IF i = ASH (1, j) THEN
                RETURN j;
            END;
        END;
    ELSIF t = t_unsign THEN
        c := MAX (SYSTEM.CARD32) - ToCardinal (v, s);
        FOR j:=0 TO 30 DO
            IF c = SYSTEM.VAL (SYSTEM.CARD32, ASH (1, j)) THEN
                RETURN j;
            END;
        END;
        IF c = high1 THEN
            RETURN 31;
        END;
    END;
    RETURN -1;
END IsNotPowerOfTwo;

(* -------------------------------------------------------------------------- *)

PROCEDURE Init*;
VAR  j : SYSTEM.INT32;
BEGIN
    overflow := FALSE;

    cmpTmp  := NewInteger(0,1);
    zzTmp   := NewZZValue(0);
    rrTmp   := NewRRValue(0);

    MaxCard8  := 255;
    MaxCard16 := 65535;
    MaxCard32 := VAL (SYSTEM.CARD32, 0FFFFFFFFH);

    FOR j := 0 TO ValueArraySize-1 DO
        IntegerValue [j] := NewInteger(j,4);
        CardinalValue[j] := NewCardinal(j,4);
        ZZValue[j]       := NewZZValue(j);
    END;

    FalseValue := NewBoolean(FALSE,1);
    TrueValue  := NewBoolean(TRUE, 1);

    RRZero := NewRRValue(0);
    RROne  := NewRRValue(1);

    NilValue := NewNilPointer();

    MinusOne := GetInteger(-1, 4);
    Zero     := GetInteger(0 , 4);
    One      := GetInteger(1 , 4);
    Two      := GetInteger(2 , 4);
END Init;

-------------- this part is used for opCode.code initialization only! --------

TYPE rr =  ARRAY OF CHAR;
CONST
<* IF TARGET_VAX THEN *>
    max_lr = 1.701411834604691E+38;
    min_lr = -max_lr;
    max_r  = 1.7014117E+38;
    min_r  = -max_r;
<* ELSIF TARGET_RISC & ((env_target="aix") OR (env_target="ppcaix")) THEN *>
    max_lr = rr{07fX,0efX,0ffX,0ffX,0ffX,0ffX,0ffX,0ffX};
    min_lr = rr{0ffX,0efX,0ffX,0ffX,0ffX,0ffX,0ffX,0ffX};
    max_r  = rr{047X,0efX,0ffX,0ffX,0e0X,000X,000X,000X};
    min_r  = rr{0c7X,0efX,0ffX,0ffX,0e0X,000X,000X,000X};
<* ELSE *>
    max_lr = rr{0ffX,0ffX,0ffX,0ffX,0ffX,0ffX,0efX,07fX};
    min_lr = rr{0ffX,0ffX,0ffX,0ffX,0ffX,0ffX,0efX,0ffX};
    max_r  = rr{000X,000X,000X,0e0X,0ffX,0ffX,0efX,047X};
    min_r  = rr{000X,000X,000X,0e0X,0ffX,0ffX,0efX,0c7X};
<* END *>

<* IF NOT TARGET_VAX THEN *>
PROCEDURE CopyFloat(s: ARRAY OF CHAR): LONGREAL;
(* necessary for platforms with obligatory memory alignment,
   such as PPC
 *)
  CONST size = 8;
  TYPE FPTR = POINTER TO ARRAY size OF CHAR;
  VAR r: LONGREAL;
      p: FPTR;
      i: INTEGER;
BEGIN
  r := 0.0;
  p:=SYSTEM.VAL(FPTR,SYSTEM.ADR(r));
  FOR i:=0 TO size-1 DO p^[i]:=s[i] END;
  RETURN r;
END CopyFloat;
<* END *>

CONST BoundedTypes = pc.NUMs + pc.ORDs +
                     pc.TY_SET{ pc.ty_loc, pc.ty_AA };

PROCEDURE SetMinValue*( t: pc.STRUCT; VAR v: pc.VALUE);

  PROCEDURE GetMinOrd( bits: LONGINT; sign: BOOLEAN; VAR v: pc.VALUE );
  BEGIN
    IF sign THEN
      CASE bits OF
      | 8  : v.set_integer( MIN(SHORTINT) );
      | 16 : v.set_integer( MIN(INTEGER)  );
      | 32 : v.set_integer( MIN(LONGINT)  );
      | 64 : v.set_integer( 0 ); -- we don't care about longlongint in O2/M2
      END;
    ELSE
      v.set_integer(0);
    END;
  END GetMinOrd;

BEGIN
  IF t.mode IN pc.TY_SET{ pc.ty_range, pc.ty_enum,
                               pc.ty_array_of, pc.ty_SS}
  THEN
    v.set_integer(0);
    RETURN;
  ELSIF NOT( t.mode IN BoundedTypes ) THEN
    v := NIL;
    RETURN;
  END;

  CASE t.mode OF
  | pc.ty_boolean  : v.set_integer(0);
<* IF NOFLOAT THEN *>
  | pc.ty_real,
  | pc.ty_longreal,
  | pc.ty_ld_real  : v := NIL;
<* ELSE *>
  | pc.ty_real     :
  <* IF TARGET_VAX THEN *>
    v.set_float( min_r );
  <* ELSE *>
    v.set_float( SYSTEM.VAL(REAL, CopyFloat(min_r) ) );
  <* END *>

  | pc.ty_longreal,
    pc.ty_ld_real  :
  <* IF TARGET_VAX THEN *>
    v.set_real( min_lr );
  <* ELSE *>
    v.set_real( CopyFloat(min_lr) );
  <* END *>

<* END *>
  ELSE
    IF t.mode IN pc.ORDs THEN
      GetMinOrd( BitSize(t.mode), t.mode IN pc.SIGNED_WHOLEs, v );
    ELSE
      v.set_integer(0);
    END;
  END;
  RETURN;
END SetMinValue;

PROCEDURE SetMaxValue*( t: pc.STRUCT; VAR v: pc.VALUE);

  PROCEDURE GetMaxOrd( bits: LONGINT; sign: BOOLEAN; VAR v: pc.VALUE );
  BEGIN
    IF sign THEN
      CASE bits OF
      | 8  : v.set_integer( MAX(SHORTINT) );
      | 16 : v.set_integer( MAX(INTEGER)  );
      | 32 : v.set_integer( MAX(LONGINT)  );
      | 64 : v.set_integer( 0 ); -- we don't care about longlongint in O2/M2
      END;
    ELSE
      CASE bits OF
      | 8  : v.set_integer( 255 );
      | 16 : v.set_integer( 65535 );
      | 32 : v.set_integer(0);
             ASSERT( pc.longint_type # NIL );
             v.binary(pc.sb_minus, pc.longint_type.max,
                                   pc.longint_type.min);
      END;
    END;
  END GetMaxOrd;

BEGIN
  IF t.mode IN pc.TY_SET{ pc.ty_range, pc.ty_enum,
                               pc.ty_array_of, pc.ty_SS }
  THEN
    v.set_integer(0);
    RETURN;
  ELSIF NOT( t.mode IN pc.BOUNDED_TYPES ) THEN
    v := NIL;
    RETURN;
  END;

  CASE t.mode OF
  | pc.ty_boolean  : v.set_integer(1);
<* IF NOFLOAT THEN *>
  | pc.ty_real,
    pc.ty_longreal,
    pc.ty_ld_real  : v := NIL;
<* ELSE *>
  | pc.ty_real     :
  <* IF TARGET_VAX THEN *>
    v.set_float( max_r );
  <* ELSE *>
    v.set_float( SYSTEM.VAL( REAL, CopyFloat(max_r) ) );
  <* END *>

  | pc.ty_longreal,
    pc.ty_ld_real  :
  <* IF TARGET_VAX THEN *>
    v.set_real( max_lr );
  <* ELSE *>
    v.set_real( CopyFloat(max_lr) );
  <* END *>

<* END *>
  ELSE
    IF t.mode IN pc.ORDs THEN
      GetMaxOrd( BitSize(t.mode), t.mode IN pc.SIGNED_WHOLEs, v );
    ELSE
      v.set_integer(0);
    END;
  END;
  RETURN;
END SetMaxValue;

(* -------------------------------------------------------------------------- *)

BEGIN
END Calc.
