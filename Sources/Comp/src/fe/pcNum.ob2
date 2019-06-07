(** Copyright (c) 1993,97 XDS Ltd, Russia. All Rights Reserved. *)
<*+O2NUMEXT*>
<*+O2ADDKWD*>
<*+WOFF312 *> (* loop is executed exactly once                  *)
<*+WOFF314 *> (* variable has compile time defined value here   *)
MODULE pcNum; (** Sem 22-Sep-93. *)

(* Modifications:
   23-Mar-96 Ned  method "int_value.cast_ordinal" is implemented.
   24-Mar-96 Ned  <*IF extvalue*> is deleted.
   26-Mar-96 Ned  get_cardinal is implemented.
   27-Mar-96 Ned  PRO0046: int_value.binary(sb_len) is implemented for strings.
                  packed_array.index_get - length of operand is used.
                  it can be in the range [0..type.len+1].
   27-Mar-96 Ned  new: flist - alwasy allocate place for tag.
*)

IMPORT
  (* do NOT import pcVis! *)
  pc :=pcK,
  xfs:=xiFiles,
  fmt:=xcStr,
  lio:=LongStr,
  wio:=WholeStr,
  env:=xiEnv,
  SYSTEM;

CONST
<* IF value96 THEN *>
  VAL_SIZE = 96; (* in bits *)
<* ELSE *>
  VAL_SIZE = 64; (* in bits *)
<* END *>

(*-------------------------------------------------------------------------*)

CONST
  SET_BITS = MAX(SET)+1;
  MAX_BIT  = SET_BITS-1;
  MAX_SET  = (VAL_SIZE-1) DIV SET_BITS;
  VAL_BITS = (MAX_SET+1)*SET_BITS;
  FULL_MASK = {0..MAX_BIT};

TYPE
  BUF    = ARRAY MAX_SET+1 OF SET;
  CBFB   = ARRAY VAL_SIZE DIV 16 OF SYSTEM.CARD16;
  CBF    = POINTER [1] TO CBFB;
  LCPTR  = POINTER [1] TO RECORD v: SYSTEM.CARD32 END;
  int_value = RECORD (pc.value_rec)
    radix: SHORTINT;
    val  : BUF;
  END;

VAR
  zero   : BUF;
  cbf_inx: ARRAY VAL_SIZE DIV 16 OF SHORTINT;

PROCEDURE ( VAR z: int_value ) correctUnsigned ();
BEGIN
  RETURN;
(* XDS v2.50 - It's temporary commented and reqired another implementation.
   This procedure is intriduced to avoid erorr "expression out of bounds" 
   for statement like "c2:= NOT( CARDINAL( 0 ) );" but in the same time 
   it is removes this error from statement like 
   "x := MIN(CARDINAL); INC(x, -1);".

  ASSERT(z.expr#NIL);
  IF z.expr.type.mode IN pc.UNSIGNED_WHOLEs THEN
    CASE pc.code.get_size(pc.su_bytes,z.expr.type) OF
    |1:    z.val[0]:=z.val[0]*{0,1,2,3,4,5,6,7};
           z.val[1]:={};
    |2:    z.val[0]:=z.val[0]*{0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
           z.val[1]:={};
    |4:    z.val[1]:={};
    ELSE
    END;
    z.val[MAX_SET]:={};
  END;
*)
END correctUnsigned;

PROCEDURE (VAR z: int_value) sgnext_32();
VAR i : INTEGER;
    m : SET;
BEGIN
  IF MAX_BIT IN z.val[0] THEN
    m := FULL_MASK;
  ELSE
    m := {};
  END;

  FOR i := 1 TO MAX_SET DO
    z.val[i] := m;
  END;
END sgnext_32;

PROCEDURE (VAR x: int_value) set_radix(radix: SHORTINT);
BEGIN
  x.radix := radix;
END set_radix;

PROCEDURE (VAR x: int_value) get_radix(): SHORTINT;
BEGIN
  RETURN x.radix;
END get_radix;

PROCEDURE (VAR x: int_value) ovr;
BEGIN
  env.errors.Error(x.pos,202);
  x.val:=zero;
END ovr;

PROCEDURE (VAR x: int_value) vptr(): CBF;
BEGIN
  RETURN SYSTEM.VAL(CBF,SYSTEM.M2ADR(x.val));
END vptr;

PROCEDURE (VAR x: int_value) lptr(): LCPTR;
BEGIN
  RETURN SYSTEM.VAL(LCPTR,SYSTEM.M2ADR(x.val));
END lptr;

PROCEDURE (VAR x: int_value) in(i: INTEGER): BOOLEAN;
BEGIN
  RETURN (i>=0) & (i<VAL_BITS) & ((i MOD SET_BITS) IN x.val[i DIV SET_BITS]);
END in;

PROCEDURE (VAR x: int_value) is_neg(): BOOLEAN;
BEGIN
  RETURN MAX_BIT IN x.val[MAX_SET];
END is_neg;

PROCEDURE (VAR x: int_value) is_zero(): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO MAX_SET DO IF x.val[i]#{} THEN RETURN FALSE END END;
  RETURN TRUE;
END is_zero;

PROCEDURE (VAR x: int_value) is_short(): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  FOR i:=1 TO MAX_SET DO IF x.val[i]#{} THEN RETURN FALSE END END;
  RETURN NOT (MAX_BIT IN x.val[0]);
END is_short;

PROCEDURE (VAR x: int_value) shift(n: INTEGER);
  VAR i,j: INTEGER; t: BUF;
BEGIN
  t:=zero;
  IF n>0 THEN
    IF n>VAL_BITS-2 THEN
      IF NOT x.is_zero() THEN x.ovr() END;
      RETURN;
    END;
    FOR i:=0 TO MAX_SET DO
      FOR j:=0 TO MAX_BIT DO
        IF ((i*SET_BITS+j)<(VAL_BITS-n)) & (j IN x.val[i]) THEN
          INCL(t[i+ (n+j) DIV SET_BITS],(n+j) MOD SET_BITS);
        END;
        IF ((i*SET_BITS+j)>=(VAL_BITS-n-1)) &
           ((j IN x.val[i])#(MAX_BIT IN x.val[MAX_SET])) THEN
          x.ovr();
          RETURN;
        END;
      END;
    END;
  ELSIF n<0 THEN
    FOR i:=0 TO MAX_SET DO
      FOR j:=0 TO MAX_BIT DO
        IF (i*SET_BITS+j)<(VAL_BITS+n) THEN
          IF ((j-n) MOD SET_BITS) IN x.val[i+(j-n) DIV SET_BITS] THEN
            INCL(t[i],j);
          END;
        ELSIF MAX_BIT IN x.val[MAX_SET] THEN
          INCL(t[i],j);
        END;
      END;
    END;
  ELSE RETURN;
  END;
  x.val:=t;
END shift;

PROCEDURE (VAR x: int_value) lshift(n: INTEGER);
  VAR i,j: INTEGER; t: BUF; b: LONGINT; r: BOOLEAN;
BEGIN
  FOR i:=0 TO MAX_SET DO
    FOR j:=0 TO MAX_BIT DO
      b:=LONG(i)*SET_BITS+j-n;
      IF (b<0) OR (b>=VAL_BITS) THEN
        r:=FALSE;
      ELSE
        r:=(b MOD SET_BITS) IN x.val[b DIV SET_BITS];
      END;
      IF r THEN INCL(t[i],j) ELSE EXCL(t[i],j) END;
    END;
  END;
  x.val:=t;
END lshift;

PROCEDURE (VAR z: int_value) i_plus(x,y: int_value);
(* do not mark x,y as read only! x.i_plus(x,y) will not work. *)
  VAR
    n: SYSTEM.CARD32;
    c: BOOLEAN;
    i: INTEGER;
    px,py,pz: CBF;
    j: SHORTINT;
    s: SET;
BEGIN
  px:=x.vptr();
  py:=y.vptr();
  pz:=z.vptr();
  c:=FALSE;
  FOR i:=0 TO LEN(px^)-1 DO
    j:=cbf_inx[i];
    n:=LONG(px[j])+LONG(py[j])+VAL(SYSTEM.CARD32,ORD(c));
    c:=n>=10000H;
    pz[j]:=SHORT(n MOD 10000H);
  END;
  s:=x.val[MAX_SET]/y.val[MAX_SET]/z.val[MAX_SET];
  IF c # (MAX_BIT IN s) THEN z.ovr() END;
END i_plus;

PROCEDURE (VAR z: int_value) i_minus(x,y: int_value);
  VAR
    n: SYSTEM.CARD32;
    c: BOOLEAN;
    i: INTEGER;
    px,py,pz: CBF;
    j: SHORTINT;
    s: SET;
BEGIN
  px:=x.vptr();
  py:=y.vptr();
  pz:=z.vptr();
  c:=TRUE;
  FOR i:=0 TO LEN(px^)-1 DO
    j:=cbf_inx[i];
    n:=LONG(px[j])+0FFFFH-LONG(py[j])+VAL(SYSTEM.CARD32,ORD(c));
    c:=n>=10000H;
    pz[j]:=SHORT(n MOD 10000H);
  END;
  s:=x.val[MAX_SET]/y.val[MAX_SET]/z.val[MAX_SET];
  IF c = (MAX_BIT IN s) THEN z.ovr() END;
END i_minus;

PROCEDURE (VAR z: int_value) i_or(x,y: int_value);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO MAX_SET DO
    z.val[i]:=x.val[i]+y.val[i];
  END;
END i_or;

PROCEDURE (VAR z: int_value) i_xor(x,y: int_value);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO MAX_SET DO
    z.val[i]:=x.val[i]/y.val[i];
  END;
END i_xor;

PROCEDURE (VAR z: int_value) i_and(x,y: int_value);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO MAX_SET DO
    z.val[i]:=x.val[i]*y.val[i];
  END;
END i_and;

PROCEDURE (VAR z: int_value) i_bic(x,y: int_value);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO MAX_SET DO
    z.val[i]:=x.val[i]-y.val[i];
  END;
END i_bic;

PROCEDURE (VAR z: int_value) i_neg;
  VAR t: int_value;
BEGIN
  t.pos:=z.pos;
  t.val:=zero;
  z.i_minus(t,z);
END i_neg;

PROCEDURE (VAR z: int_value) i_compl;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO MAX_SET DO
    z.val[i]:=-z.val[i];
  END;
END i_compl;

PROCEDURE (VAR z: int_value) i_mul(x,y: int_value);
  VAR ng: BOOLEAN; xp,i,j: INTEGER;
BEGIN
  ng:=y.is_neg();
  IF ng THEN y.i_neg END;
  z.val:=zero;
  xp:=0;
  FOR i:=0 TO MAX_SET DO
    FOR j:=0 TO MAX_BIT DO
      IF j IN y.val[i] THEN
        x.shift(i*SET_BITS+j-xp);
        z.i_plus(z,x);
        xp:=i*SET_BITS+j;
      END;
    END;
  END;
  IF ng THEN z.i_neg END;
END i_mul;

PROCEDURE (VAR z: int_value) i_slash(x,y: int_value; rem: BOOLEAN): BOOLEAN;
(* returns TRUE if (x MOD y) # 0 *)
  VAR sg: BOOLEAN; i,j: INTEGER; t: int_value; px,py,pz: LCPTR;
BEGIN
  sg:=x.is_neg();
  IF sg THEN x.i_neg END;
  IF y.is_neg() THEN y.i_neg; IF NOT rem THEN sg:=NOT sg END END;
  z.val:=zero;
  IF x.is_short() & y.is_short() THEN
    px:=x.lptr();
    py:=y.lptr();
    pz:=z.lptr();
    IF py.v=0 THEN z.ovr(); RETURN FALSE END;
    IF rem THEN pz.v:=px.v MOD py.v;
    ELSE pz.v:=px.v DIV py.v;
    END;
    IF sg THEN z.i_neg END;
    RETURN (px.v MOD py.v)#0;
  END;
  i:=MAX_SET; j:=MAX_BIT;
  WHILE NOT (j IN y.val[i]) DO
    DEC(j);
    IF j<0 THEN
      DEC(i); j:=MAX_BIT;
      IF i<0 THEN z.ovr(); RETURN FALSE END;
    END;
  END;
  i:=VAL_BITS-2-j-i*SET_BITS;
  y.shift(i);
  t.pos:=z.pos;
  LOOP
    t.i_minus(x,y);
    IF NOT t.is_neg() THEN
      x.val:=t.val;
      INCL(z.val[i DIV SET_BITS],i MOD SET_BITS);
    END;
    IF i=0 THEN EXIT END;
    y.shift(-1);
    DEC(i);
  END;
  IF rem THEN z.val:=x.val END;
  IF sg THEN z.i_neg END;
  RETURN NOT x.is_zero();
END i_slash;

PROCEDURE xorBits (VAR buf :BUF; n :LONGINT);
(* inverts n+1 low bits *)
VAR
  i, last :LONGINT;
BEGIN
  last := n DIV SET_BITS;
  FOR i:=0 TO last-1 DO
    buf[i] := buf[i] / FULL_MASK;
  END;
  IF (last <= MAX_SET) THEN
    buf[last] := buf[last] / {0..(n MOD SET_BITS)};
  END;
END xorBits;


PROCEDURE setBits (VAR buf :BUF; n :LONGINT);
(* set n+1 low bits to 1 *)
BEGIN
  buf := zero;
  xorBits (buf, n);
END setBits;

PROCEDURE (VAR z: int_value) get_min (n :LONGINT; signed :BOOLEAN);
BEGIN
  ASSERT((n>0) & (n <=VAL_SIZE));

  IF (signed) THEN
    setBits (z.val, n-2);
    xorBits (z.val, VAL_SIZE-1);
  ELSE
    z.val := zero;
  END;
END get_min;

PROCEDURE (VAR z: int_value) get_max (n :LONGINT; signed :BOOLEAN);
BEGIN
  ASSERT((n>0) & (n <=VAL_SIZE));

  IF (signed) THEN DEC(n) END;
  setBits (z.val, n-1);
END get_max;

PROCEDURE (VAR z: int_value) set_integer(x: LONGINT);
  VAR i: INTEGER; m: SET;
BEGIN
  (* написано в предположении BITS(SET)=32 !!! *)
  ASSERT(SET_BITS=32);
  z.val[0]:=SYSTEM.VAL(SET,x);
  z.sgnext_32();
END set_integer;

PROCEDURE (VAR z: int_value) get_integer(): LONGINT;
  VAR i: INTEGER; m: SET;
BEGIN
  (* написано в предположении BITS(SET)=32 !!! *)
  ASSERT(SET_BITS=32);
  IF MAX_BIT IN z.val[(VAL_SIZE DIV SET_BITS)-2] THEN m:=FULL_MASK ELSE m:={} END;
  FOR i:=(VAL_SIZE DIV SET_BITS)-1 TO MAX_SET DO
    IF z.val[i]#m THEN z.ovr(); RETURN 0 END;
  END;
  RETURN SYSTEM.VAL(LONGINT,z.val[0]);
END get_integer;

PROCEDURE (VAR z: int_value) get_cardinal(): SYSTEM.CARD32;
  VAR i: INTEGER;
BEGIN
  (* написано в предположении BITS(SET)=32 !!! *)
  ASSERT(SET_BITS=32);
  FOR i:=(VAL_SIZE DIV SET_BITS)-1 TO MAX_SET DO
    IF z.val[i]#{} THEN z.ovr(); RETURN 0 END;
  END;
  RETURN SYSTEM.VAL(SYSTEM.CARD32,z.val[0]);
END get_cardinal;

PROCEDURE (VAR z: int_value) cast_ordinal(to_type: pc.STRUCT);
  VAR i,bitsize,signset,signbit: LONGINT;
  to_signed:BOOLEAN;
BEGIN
  to_signed := to_type.signed();
  bitsize := pc.code.get_size (pc.su_bits, to_type);
  signset := (bitsize-1) DIV SET_BITS;
  signbit := (bitsize-1) MOD SET_BITS;
  IF to_signed AND (signbit IN z.val[signset]) THEN
    z.val[signset] := z.val[signset] + {signbit+1 .. SET_BITS-1};
    FOR i:=signset+1 TO MAX_SET DO
      z.val[i]:=FULL_MASK;
    END;
  ELSE
    z.val[signset] := z.val[signset] - {signbit+1 .. SET_BITS-1};
    FOR i:=signset+1 TO MAX_SET DO
      z.val[i]:={};
    END;
  END;
END cast_ordinal;

PROCEDURE (VAR z: int_value) get_NDWord(n :INTEGER): SYSTEM.CARD32;
BEGIN
  RETURN SYSTEM.VAL(SYSTEM.CARD32, z.val[n]);
END get_NDWord;

PROCEDURE (VAR z: int_value) set_NDWord(n :INTEGER; val :SYSTEM.CARD32);
BEGIN
    z.val[n]:=SYSTEM.VAL(SET,val);
END set_NDWord;

-- actually this operation is identical to right ariphmetic shift
-- to 32 positions.
-- this operation is applicable to INT64/CARD64 only!
PROCEDURE (VAR z: int_value) get_hiword();
BEGIN
  z.set_NDWord( 0, z.get_NDWord(1));
  z.sgnext_32();
END get_hiword;

(*-------------------------------------------------------------------------*)

TYPE
  real_value=RECORD (pc.value_rec)
    val: LONGREAL;
  END;

PROCEDURE (VAR x: real_value) ovr();
BEGIN
  env.errors.Error(x.pos,201);
  x.val:=0.0;
END ovr;

PROCEDURE (VAR x: real_value) CheckOvr();
BEGIN
  IF x.val = 1. THEN
    x.val := 1.;
  END;
EXCEPT
  x.ovr();
  RETURN;
END CheckOvr;

PROCEDURE (VAR x: real_value) is_neg(): BOOLEAN;
BEGIN
  RETURN x.val<0.0;
END is_neg;

PROCEDURE (VAR x: real_value) is_zero(): BOOLEAN;
BEGIN
  RETURN x.val=0.0;
END is_zero;


PROCEDURE (VAR x: real_value) is_aN(): BOOLEAN;
VAR
  setH :SET;
CONST
  fullExpo = {20..30};  -- see IEEE 754 fp standard
BEGIN
  SYSTEM.GET (SYSTEM.ADR(x.val)+4, setH);
  RETURN (setH*fullExpo # fullExpo);
END is_aN;


PROCEDURE (VAR z: real_value) rplus(x,y: real_value);
BEGIN
  IF (y.val>0.0) & (x.val>MAX(LONGREAL)-y.val) THEN z.ovr;
  ELSIF (y.val<0.0) & (x.val<MIN(LONGREAL)-y.val) THEN z.ovr;
  ELSE z.val:=x.val+y.val;
  END;
  z.CheckOvr();
END rplus;

PROCEDURE (VAR z: real_value) rminus(x,y: real_value);
BEGIN
  y.val:=-y.val;
  z.rplus(x,y);
END rminus;

PROCEDURE (VAR z: real_value) rmul(x,y: real_value);
BEGIN
  z.val:=x.val*y.val;
  z.CheckOvr();
END rmul;

PROCEDURE (VAR z: real_value) rdiv(x,y: real_value);
BEGIN
  IF y.val=0.0 THEN z.ovr;
  ELSE z.val:=x.val/y.val;
  END;
  z.CheckOvr();
END rdiv;

PROCEDURE (VAR z: real_value) ri_exp(x: real_value; y: int_value);
BEGIN
  z.val:=x.val**y.get_integer();
  z.CheckOvr();
END ri_exp;

PROCEDURE (VAR z: real_value) rr_exp(x,y: real_value);
BEGIN
  z.val:=x.val**y.val;
  z.CheckOvr();
END rr_exp;

PROCEDURE (VAR z: real_value) float(x: int_value);
BEGIN
  IF x.is_neg() THEN z.val:=x.get_integer();
  ELSE               z.val:=x.get_cardinal();
  END;
END float;

PROCEDURE (VAR z: int_value) truncate(x: real_value);
BEGIN
  IF x.val > VAL(LONGREAL, MAX(LONGINT)) THEN
    IF x.val > VAL(LONGREAL, MAX(SYSTEM.CARD32)) THEN
      z.ovr();
      RETURN
    END;
    z.set_integer(SYSTEM.VAL (LONGINT, VAL(SYSTEM.CARD32, x.val)));
    z.cast_ordinal (pc.longcard_type);
  ELSE 
    z.set_integer(VAL(LONGINT,x.val));
  END;
END truncate;

PROCEDURE (VAR z: int_value) entier(x: real_value);
BEGIN
  IF x.val > VAL(LONGREAL, MAX(LONGINT)) THEN
    z.truncate(x);
  ELSE
    z.set_integer(ENTIER(x.val));
  END;
END entier;

(*-------------------------------------------------------------------------*)

TYPE
  complex_value=RECORD (pc.value_rec)
    r: real_value;
    i: real_value;
  END;

PROCEDURE (VAR z: complex_value) cplus(x,y: complex_value);
BEGIN
  z.r.pos:=z.pos; z.i.pos:=z.pos;
  z.r.rplus(x.r,y.r);
  z.i.rplus(x.i,y.i);
END cplus;

PROCEDURE (VAR z: complex_value) cminus(x,y: complex_value);
BEGIN
  z.r.pos:=z.pos; z.i.pos:=z.pos;
  z.r.rminus(x.r,y.r);
  z.i.rminus(x.i,y.i);
END cminus;

PROCEDURE (VAR z: complex_value) cmul(x,y: complex_value);
  VAR t: real_value;
BEGIN
  t.pos:=x.pos;
  z.r.pos:=z.pos;
  z.i.pos:=z.pos;
  z.r.rmul(x.r,y.r);
  t.rmul(x.i,y.i);
  z.r.rminus(z.r,t);
  z.i.rmul(x.r,y.i);
  t.rmul(x.i,y.r);
  z.i.rplus(z.i,t);
END cmul;

PROCEDURE (VAR z: complex_value) cdiv(x,y: complex_value);
  VAR t,s: real_value;
BEGIN
  t.pos:=z.pos;
  s.pos:=z.pos;
  z.r.pos:=z.pos;
  z.i.pos:=z.pos;
  z.r.rmul(x.r,y.r);
  t.rmul(x.i,y.i);
  z.r.rplus(z.r,t);
  z.i.rmul(x.i,y.r);
  t.rmul(x.r,y.i);
  z.i.rminus(z.i,t);
  s.rmul(y.r,y.r);
  t.rmul(y.i,y.i);
  s.rplus(s,t);
  z.r.rdiv(z.r,s);
  z.i.rdiv(z.i,s);
END cdiv;

PROCEDURE (VAR x: complex_value) is_zero(): BOOLEAN;
BEGIN
  RETURN (x.r.val=0.0) & (x.i.val=0.0);
END is_zero;

PROCEDURE (VAR z: complex_value) ci_exp(x: complex_value; y: int_value);
  VAR r: LONGCOMPLEX;
BEGIN
  r:=CMPLX(x.r.val,x.i.val)**y.get_integer();
  z.r.val:=RE(r);
  z.i.val:=IM(r);
END ci_exp;

PROCEDURE (VAR z: complex_value) cr_exp(x: complex_value; y: real_value);
  VAR r: LONGCOMPLEX;
BEGIN
  r:=CMPLX(x.r.val,x.i.val)**y.val;
  z.r.val:=RE(r);
  z.i.val:=IM(r);
END cr_exp;

(*-------------------------------------------------------------------------*)

TYPE
  array_value = RECORD (pc.value_rec)
    arr: POINTER TO ARRAY OF pc.VALUE;
  END;
  packed_arr_value = RECORD (pc.value_rec)
    str: BOOLEAN;
    arr: pc.STRING;
  END;
  proc_value = RECORD (pc.value_rec)
    obj: pc.OBJECT;
  END;

(*-------------------------------------------------------------------------*)

TYPE
  IVALUE=POINTER TO int_value;
  RVALUE=POINTER TO real_value;
  CVALUE=POINTER TO complex_value;
  AVALUE=POINTER TO array_value;
  SVALUE=POINTER TO packed_arr_value;
  PVALUE=POINTER TO proc_value;

PROCEDURE (VAR x: int_value) new(ps: pc.TPOS; type: pc.STRUCT): pc.VALUE;

  PROCEDURE flist(f: pc.OBJECT): LONGINT;
    VAR n: pc.NODE; m,k,s: LONGINT;
  BEGIN
    s:=0;
    WHILE f#NIL DO
      IF f.mode=pc.ob_field THEN
        INC(s);
      ELSIF f.mode=pc.ob_header THEN
        (*!! IF f.val.obj#NIL THEN INC(s) END;*)
        INC(s); (* Ned: always allocate place for tag *)
        n:=f.val.l; m:=0;
        WHILE n#NIL DO
          ASSERT(n.mode=pc.nd_node);
          k:=flist(n.obj);
          IF k>m THEN m:=k END;
          n:=n.next;
        END;
        INC(s,m);
      ELSE
        ASSERT(FALSE);
      END;
      f:=f.next;
    END;
    RETURN s;
  END flist;

  PROCEDURE field_no(tt: pc.STRUCT): LONGINT;
    VAR s: LONGINT;
  BEGIN
    ASSERT(tt.mode=pc.ty_record);
    IF tt.base#NIL THEN s:=field_no(tt.base) ELSE s:=0 END;
    RETURN s+flist(tt.prof);
  END field_no;

  VAR
    i : IVALUE;
    r : RVALUE;
    c : CVALUE;
    a : AVALUE;
    s : SVALUE;
    p : PVALUE;
    v : pc.VALUE;

BEGIN
  IF x.is_ordinal(type) THEN
    NEW(i);
    i.val   := zero;
    i.radix := 10;
    v:=i;
  ELSE
    ASSERT(NOT type.is_ordinal());
    CASE type.mode OF
    |pc.ty_real,pc.ty_longreal,pc.ty_ld_real,pc.ty_RR:
      NEW(r);
      r.val:=0.0;
      v:=r;
    |pc.ty_proctype:
      NEW(p);
      p.obj:=NIL;
      v:=p;
    |pc.ty_complex,pc.ty_lcomplex,pc.ty_CC:
      NEW(c);
      c.r.val:=0.0;
      c.i.val:=0.0;
      v:=c;
    |pc.ty_array,pc.ty_set,pc.ty_SS:
      IF (type.mode IN pc.SETs) OR
         (type.base.mode IN pc.TY_SET{pc.ty_char,pc.ty_boolean,pc.ty_loc})
      THEN
        NEW(s);
        IF type.len > 0 THEN
          NEW(s.arr,type.len);
        END;
        s.str:=type.mode IN pc.ARRs;
        v:=s;
      ELSE
        NEW(a);
        NEW(a.arr,type.len);
        v:=a;
      END;
    |pc.ty_record:
      NEW(a);
      NEW(a.arr,field_no(type));
      v:=a;
    END;
  END;
  v.pos:=ps;
  v.expr:=NIL;
  RETURN v;
END new;

PROCEDURE (VAR x: int_value) is_ordinal(type: pc.STRUCT): BOOLEAN;
BEGIN
  RETURN (type.mode IN
    pc.ORDs + pc.PTRs +  pc.TY_SET{ pc.ty_loc, pc.ty_protection });
END is_ordinal;

PROCEDURE (VAR z: int_value) is_ZZ(): BOOLEAN;
BEGIN
  RETURN TRUE
END is_ZZ;

PROCEDURE (VAR z: real_value) is_RR(): BOOLEAN;
BEGIN
  RETURN TRUE
END is_RR;

(*------------------------- input / output ----------------------------*)

CONST
  TAG_INT  = 0;
  TAG_REAL = 1;
  TAG_CPLX = 2;
  TAG_ARR  = 3;
  TAG_PARR = 4;
  TAG_PROC = 5;

PROCEDURE (VAR x: int_value) out(f: xfs.SymFile; w: pc.WRO_PROC);
  VAR i: LONGINT;
BEGIN
  f.WriteInt(TAG_INT);
<* IF value96 THEN *>
  FOR i:=0 TO (LEN(x.val)-1)-1 DO f.WriteSet(x.val[i]) END;
<* ELSE *>
  FOR i:=0 TO LEN(x.val)-1 DO f.WriteSet(x.val[i]) END;
<* END *>
END out;

PROCEDURE (VAR x: real_value) out(f: xfs.SymFile; w: pc.WRO_PROC);
BEGIN
  f.WriteInt(TAG_REAL);
  f.WriteReal(x.val);
END out;

PROCEDURE (VAR x: proc_value) out(f: xfs.SymFile; w: pc.WRO_PROC);
BEGIN
  f.WriteInt(TAG_PROC);
  w(x.obj);
END out;

PROCEDURE (VAR x: complex_value) out(f: xfs.SymFile; w: pc.WRO_PROC);
BEGIN
  f.WriteInt(TAG_CPLX);
  f.WriteReal(x.r.val);
  f.WriteReal(x.i.val);
END out;

PROCEDURE (VAR x: packed_arr_value) out(f: xfs.SymFile; w: pc.WRO_PROC);
  VAR i: LONGINT;
BEGIN
  f.WriteInt(TAG_PARR);
  f.Write(CHR(x.str));
  f.WriteInt(LEN(x.arr^));
  FOR i:=0 TO LEN(x.arr^)-1 DO f.Write(x.arr[i]) END;
END out;

PROCEDURE (VAR x: array_value) out(f: xfs.SymFile; w: pc.WRO_PROC);
  VAR i, len: LONGINT;
BEGIN
  f.WriteInt(TAG_ARR);
  len := 0;
  LOOP
    IF (len = LEN(x.arr^)) OR (x.arr[len] = NIL) THEN EXIT END;
    INC(len);
  END;
  f.WriteInt(len);
  FOR i:=0 TO len-1 DO x.arr[i].out(f,w) END;
END out;

PROCEDURE (VAR x: int_value) inp(f: xfs.SymFile; rd: pc.RDO_PROC): pc.VALUE;
  VAR a,b: LONGINT; xx: CHAR;
      i: IVALUE; r: RVALUE; c: CVALUE; ar: AVALUE; pa: SVALUE; p: PVALUE;
      iro :pc.IROBJECT;
BEGIN
  f.ReadInt(a);
  CASE a OF
    |TAG_INT:
      NEW(i);
      i.pos:=env.null_pos;
<* IF value96 THEN *>
      FOR b:=0 TO (LEN(i.val)-1)-1 DO f.ReadSet(i.val[b]) END;
      IF 31 IN i.val[(LEN(i.val)-1)-1] 
      THEN i.val[LEN(i.val)-1] := -{}; ELSE i.val[LEN(i.val)-1] := {}; END;
<* ELSE *>
      FOR b:=0 TO LEN(i.val)-1 DO f.ReadSet(i.val[b]) END;
<* END *>
      RETURN i;
    |TAG_REAL:
      NEW(r);
      r.pos:=env.null_pos;
      f.ReadReal(r.val);
      RETURN r;
    |TAG_CPLX:
      NEW(c);
      c.pos:=env.null_pos;
      f.ReadReal(c.r.val);
      f.ReadReal(c.i.val);
      RETURN c;
    |TAG_PARR:
      NEW(pa);
      pa.pos:=env.null_pos;
      f.Read(xx); pa.str:=xx#0X;
      f.ReadInt(b);
      NEW(pa.arr,b);
      FOR b:=0 TO LEN(pa.arr^)-1 DO f.Read(xx); pa.arr[b]:=xx END;
      RETURN pa;
    |TAG_ARR:
      NEW(ar);
      ar.pos:=env.null_pos;
      f.ReadInt(b);
      ASSERT(b>0);
      NEW(ar.arr,b);
      FOR b:=0 TO LEN(ar.arr^)-1 DO ar.arr[b]:=x.inp(f,rd) END;
      RETURN ar;
    |TAG_PROC:
      NEW(p);
      iro := rd();
      IF (iro # NIL) THEN
        p.obj := iro(pc.OBJECT);
      ELSE
        p.obj := NIL
      END;
      RETURN p;
  END;
END inp;

(*----------------------------------------------------------------------*)

PROCEDURE (VAR z: int_value) unary(cop: pc.SUB_MODE; x: pc.VALUE);
  VAR i,l: LONGINT;
  TYPE intint = ARRAY 2 OF LONGINT;
BEGIN
  CASE cop OF
    |pc.su_abs   :
      z.val:=x(IVALUE).val;
      IF z.is_neg() THEN z.i_neg END;
    |pc.su_neg   :
      z.val:=x(IVALUE).val;
      z.i_neg;
    |pc.su_conv  :
      WITH x: IVALUE DO
          z.val:=x.val;
      |x: RVALUE DO
          z.truncate(x^);
      |x: SVALUE DO
          ASSERT(NOT x.str);
          ASSERT(LEN(x.arr^)<VAL_BITS);
          z.val:=zero;
          l:=LEN(x.arr^)-1;
          FOR i:=0 TO l DO
              IF x.arr[i]#0X THEN INCL(z.val[i DIV SET_BITS],i MOD SET_BITS) END;
          END;
      END;
    |pc.su_cast  :
      WITH x: IVALUE DO
        z.val:=x.val;
      |x: RVALUE DO
        z.set_NDWord(0,SYSTEM.VAL(intint,x.val)[0]);
        z.set_NDWord(1,SYSTEM.VAL(intint,x.val)[1]);
      |x: SVALUE DO
          ASSERT(NOT x.str);
          ASSERT(LEN(x.arr^)<VAL_BITS);
          z.val:=zero;
          l:=LEN(x.arr^)-1;
          FOR i:=0 TO l DO
              IF x.arr[i]#0X THEN INCL(z.val[i DIV SET_BITS],i MOD SET_BITS) END;
          END;
          (* the following is done to prevent const overflow in sequence
             of casts INT32 -> SET32 -> INCL(MAX_BIT) -> IN32
             Hady, Jun 2, 1998
          *)
          IF (l=MAX_BIT) & (MAX_BIT IN z.val[0]) THEN
              FOR i:=1 TO MAX_SET DO z.val[i]:=FULL_MASK END;
          END;
      END;
    | pc.su_entier:
      WITH x: IVALUE DO
        z.val:=x.val;
      |x: RVALUE DO
        z.entier(x^);
      END;
    |pc.su_odd   : z.set_integer(VAL(LONGINT,0 IN x(IVALUE).val[0]));
    |pc.su_not   : z.set_integer(VAL(LONGINT,x(IVALUE).is_zero()));
    |pc.su_cap   :
      i:=x(IVALUE).get_integer();
      IF (i>=ORD('a')) & (i<=ORD('z')) THEN i:=i-ORD('a')+ORD('A') END;
      z.set_integer(i);
    |pc.su_length:
      WITH x: SVALUE DO
        i:=0;
        WHILE (i<LEN(x.arr^)) & (x.arr^[i]#0X) DO INC(i) END;
        z.set_integer(i);
      END;
    |pc.su_compl:
      z.val:=x(IVALUE).val;
      z.i_compl;
    |pc.su_ptr2vptr:
      z.set_integer(x(IVALUE).get_integer() DIV 4);
    |pc.su_vptr2ptr:
      z.set_integer(x(IVALUE).get_integer() * 4);
    |pc.su_hiword:
      z.val:=x(IVALUE).val;
      z.get_hiword();
  END;
END unary;

PROCEDURE (VAR z: real_value) unary(cop: pc.SUB_MODE; x: pc.VALUE);
VAR foo: int_value;
BEGIN
  CASE cop OF
    |pc.su_abs  :
      z.val:=x(RVALUE).val;
      IF z.val<0.0 THEN z.val:=-z.val END;
    |pc.su_neg  :
      z.val:=-x(RVALUE).val;
    |pc.su_conv :
      WITH x: IVALUE DO
        z.float(x^);
      |x: RVALUE DO
        z.val:=x.val;

      (* added to permit SET->REAL casts *)
      (* htayod, 2 Oct 2000 *)
      |x: SVALUE DO
        foo.unary(cop, x);
        z.float(foo); 
      END;

    |pc.su_cast  :
      WITH x: RVALUE DO
        z.val:=x.val;
      END;
    |pc.su_re:
      z.val:=x(CVALUE).r.val;
    |pc.su_im:
      z.val:=x(CVALUE).i.val;
  END;
END unary;

PROCEDURE (VAR z: complex_value) unary(cop: pc.SUB_MODE; x: pc.VALUE);
BEGIN
  CASE cop OF
    |pc.su_neg  :
      WITH x: CVALUE DO
        z.r.val:=-x.r.val;
        z.i.val:=-x.i.val;
      END;
    |pc.su_conv :
      WITH x: IVALUE DO
        z.r.float(x^);
        z.i.val:=0.;
      |x: RVALUE DO
        z.r.val:=x.val;
        z.i.val:=0.;
      |x: CVALUE DO
        z.r.val:=x.r.val;
        z.i.val:=x.i.val;
      END;
    |pc.su_cast :
      WITH x: CVALUE DO
        z.r.val:=x.r.val;
        z.i.val:=x.i.val;
      END;
  END;
END unary;

PROCEDURE (VAR z: packed_arr_value) unary(cop: pc.SUB_MODE; x: pc.VALUE);
  VAR i, k, m: LONGINT;
BEGIN
  CASE cop OF
    |pc.su_compl:
      ASSERT(NOT z.str);
      FOR i:=0 TO LEN(z.arr^)-1 DO
        IF x(SVALUE).arr[i] # 0X
         THEN z.arr[i] := 0C;
         ELSE z.arr[i] := 1C;
        END;
      END;
    |pc.su_cast:
      WITH x: SVALUE DO               (* from another array - just copy *)
        ASSERT(z.str = x.str);
        FOR i:=0 TO LEN(z.arr^)-1 DO
          IF i>=LEN(x.arr^)
           THEN z.arr[i] := 0C;
           ELSE z.arr[i] := x.arr[i];
          END;
        END;
      | x: IVALUE DO                  (* from integer constant *)
        IF z.str THEN                     (* to array of char/loc -- WORD in TS *)
          FOR i:=0 TO LEN(z.arr^)-1 DO
            IF i DIV (SET_BITS DIV 8) < LEN(x.val) THEN
              IF i MOD (SET_BITS DIV 8) = 0 THEN
                m := SYSTEM.VAL(LONGINT, x.val[i DIV (SET_BITS DIV 8)]);
              END;
              z.arr[i] := CHR(m MOD 256);
              m := m DIV 256;
            ELSE
              z.arr[i] := 0C;
            END;
          END;
        ELSE                              (* to bitset *)
          FOR i:=0 TO LEN(z.arr^)-1 DO
            IF (i DIV SET_BITS < LEN(x.val)) & ((i MOD SET_BITS) IN x^.val[i DIV SET_BITS])
             THEN z.arr[i] := 1C;
             ELSE z.arr[i] := 0C;
            END;
          END;
        END;
      END;
  END;
END unary;

PROCEDURE copy_array_value(source: AVALUE; VAR dest: array_value);
VAR
  i: LONGINT;
BEGIN
  ASSERT(LEN(source.arr^)<=LEN(dest.arr^));
  FOR i:=0 TO LEN(dest.arr^)-1 DO
    IF i < LEN(source.arr^) THEN
      dest.arr[i] := source.arr[i];
    ELSE
      dest.arr[i] := NIL;
    END;
  END;
END copy_array_value;

PROCEDURE (VAR z: array_value) unary(cop: pc.SUB_MODE; x: pc.VALUE);
VAR i:INTEGER;
BEGIN
  CASE cop OF
    |pc.su_cast:
      WITH x: AVALUE DO
        copy_array_value(x(AVALUE), z);
      | x: IVALUE DO  (* for TS conversion rules *)
        z.arr[0]:=pc.value.new(x.pos,pc.ZZ_type);
        z.arr[0].set_integer(x.get_NDWord(0));
        IF LEN(z.arr^)>1 THEN
          z.arr[1]:=pc.value.new(x.pos,pc.ZZ_type);
          z.arr[1].set_integer(x.get_NDWord(1));
        END;
        FOR i := 2 TO LEN(z.arr^)-1 DO
          z.arr[i]:=pc.value.new(x.pos,pc.ZZ_type);
          IF x.is_neg() THEN
            z.arr[i].set_integer(-1);
          ELSE
            z.arr[i].set_integer(0);
          END;
        END;
      END;
  END;
END unary;

PROCEDURE (VAR z: proc_value) unary(cop: pc.SUB_MODE; x: pc.VALUE);
BEGIN
  CASE cop OF
    |pc.su_cast:
      WITH x: PVALUE DO
        z.obj:=x.obj;
      END;
  END;
END unary;

PROCEDURE strcmp(cop: pc.SUB_MODE; x-,y-: ARRAY OF CHAR): LONGINT;
(* строка может не кончаться символом 0X ! *)
  VAR i: LONGINT; r: BOOLEAN; cx,cy: CHAR;
BEGIN
  i:=0;
  LOOP
    IF i<LEN(x) THEN cx:=x[i] ELSE cx:=0X END;
    IF i<LEN(y) THEN cy:=y[i] ELSE cy:=0X END;
    IF (cx=0X) OR (cx#cy) THEN EXIT END;
    INC(i)
  END;
  CASE cop OF
    |pc.sb_equ: r:=cx= cy;
    |pc.sb_neq: r:=cx# cy;
    |pc.sb_gtr: r:=cx> cy;
    |pc.sb_geq: r:=cx>=cy;
    |pc.sb_lss: r:=cx< cy;
    |pc.sb_leq: r:=cx<=cy;
  END;
  IF r THEN RETURN 1 ELSE RETURN 0 END;
END strcmp;

PROCEDURE (VAR z: int_value) binary(cop: pc.SUB_MODE; x,y: pc.VALUE);
  VAR
    ti: int_value;
    i : LONGINT;
    l : LONGINT;
BEGIN
  ti.pos:=z.pos;
  CASE cop OF
    |pc.sb_equ     :
      WITH x: IVALUE DO
        ti.i_minus(x^,y(IVALUE)^);
        z.set_integer(VAL(LONGINT,ti.is_zero()));
      |x: RVALUE DO
        z.set_integer(VAL(LONGINT,x.val=y(RVALUE).val));
      |x: CVALUE DO
        z.set_integer(VAL(LONGINT,(x.r.val=y(CVALUE).r.val) &
                        (x.i.val=y(CVALUE).i.val)));
      |x: SVALUE DO
        IF x.str THEN
          l:=strcmp(cop,x.arr^,y(SVALUE).arr^);
        ELSE
          l:=1;
          FOR i:=0 TO LEN(x.arr^)-1 DO
            IF x.arr[i]#y(SVALUE).arr[i] THEN l:=0 END;
          END;
        END;
        z.set_integer(l);
      |x: AVALUE DO
        l:=1;
        FOR i:=0 TO LEN(x.arr^)-1 DO
          ti.binary(pc.sb_equ,x.arr[i],y(AVALUE).arr[i]);
          IF ti.is_zero() THEN l:=0 END;
        END;
        z.set_integer(l);
      |x: PVALUE DO
        WITH y: PVALUE DO
          z.set_integer(VAL(LONGINT,x.obj=y(PVALUE).obj));
        | y: IVALUE DO -- trying to compare proc_value and int_value
          ASSERT( y.is_zero() ); -- int_value must be 0 (NIL)
          z.set_integer(VAL(LONGINT, x.obj = NIL));
        END;
      END;
    |pc.sb_neq     :
      WITH x: IVALUE DO
        ti.i_minus(x^,y(IVALUE)^);
        z.set_integer(VAL(LONGINT,NOT ti.is_zero()));
      |x: RVALUE DO
        z.set_integer(VAL(LONGINT,x.val#y(RVALUE).val));
      |x: CVALUE DO
        z.set_integer(VAL(LONGINT,(x.r.val#y(CVALUE).r.val) OR
                        (x.i.val#y(CVALUE).i.val)));
      |x: SVALUE DO
        IF x.str THEN
          l:=strcmp(cop,x.arr^,y(SVALUE).arr^);
        ELSE
          l:=0;
          FOR i:=0 TO LEN(x.arr^)-1 DO
            IF x.arr[i]#y(SVALUE).arr[i] THEN l:=1 END;
          END;
        END;
        z.set_integer(l);
      |x: AVALUE DO
        l:=0;
        FOR i:=0 TO LEN(x.arr^)-1 DO
          ti.binary(pc.sb_equ,x.arr[i],y(AVALUE).arr[i]);
          IF ti.is_zero() THEN l:=1 END;
        END;
        z.set_integer(l);
      |x: PVALUE DO
        WITH y: PVALUE DO
          z.set_integer(VAL(LONGINT,x.obj#y(PVALUE).obj));
        | y: IVALUE DO -- trying to compare proc_value and int_value
          ASSERT( y.is_zero() ); -- int_value must be 0 (NIL)
          z.set_integer(VAL(LONGINT, x.obj # NIL));  
        END;
      END;
    |pc.sb_lss     :
      WITH x: IVALUE DO
        ti.i_minus(x^,y(IVALUE)^);
        z.set_integer(VAL(LONGINT,ti.is_neg()));
      |x: RVALUE DO
        z.set_integer(VAL(LONGINT,x.val<y(RVALUE).val));
      |x: SVALUE DO
        ASSERT(x.str);
        l:=strcmp(cop,x.arr^,y(SVALUE).arr^);
        z.set_integer(l);
      END;
    |pc.sb_leq     :
      WITH x: IVALUE DO
        ti.i_minus(y(IVALUE)^,x^);
        z.set_integer(VAL(LONGINT,NOT ti.is_neg()));
      |x: RVALUE DO
        z.set_integer(VAL(LONGINT,x.val<=y(RVALUE).val));
      |x: SVALUE DO
        IF x.str THEN
          l:=strcmp(cop,x.arr^,y(SVALUE).arr^);
        ELSE
          l:=1;
          FOR i:=0 TO LEN(x.arr^)-1 DO
            IF (x.arr[i]#0X) & (y(SVALUE).arr[i]=0X) THEN l:=0 END;
          END;
        END;
        z.set_integer(l);
      END;
    |pc.sb_gtr     :
      WITH x: IVALUE DO
        ti.i_minus(y(IVALUE)^,x^);
        z.set_integer(VAL(LONGINT,ti.is_neg()));
      |x: RVALUE DO
        z.set_integer(VAL(LONGINT,x.val>y(RVALUE).val));
      |x: SVALUE DO
        ASSERT(x.str);
        l:=strcmp(cop,x.arr^,y(SVALUE).arr^);
        z.set_integer(l);
      END;
    |pc.sb_geq     :
      WITH x: IVALUE DO
        ti.i_minus(x^,y(IVALUE)^);
        z.set_integer(VAL(LONGINT,NOT ti.is_neg()));
      |x: RVALUE DO
        z.set_integer(VAL(LONGINT,x.val>=y(RVALUE).val));
      |x: SVALUE DO
        IF x.str THEN
          l:=strcmp(cop,x.arr^,y(SVALUE).arr^);
        ELSE
          l:=1;
          FOR i:=0 TO LEN(x.arr^)-1 DO
            IF (y(SVALUE).arr[i]#0X) & (x.arr[i]=0X) THEN l:=0 END;
          END;
        END;
        z.set_integer(l);
      END;
    |pc.sb_in      :
      WITH y: SVALUE DO
        l:=x(IVALUE).get_integer();
        z.set_integer(VAL(LONGINT,
                (l>=0) & (l<LEN(y.arr^)) & (y.arr[l]#0X)));
      END;
    |pc.sb_mul     : z.i_mul(x(IVALUE)^,y(IVALUE)^);
    |pc.sb_div     :
      IF z.i_slash(x(IVALUE)^,y(IVALUE)^,FALSE) &
        (x(IVALUE).is_neg()#y(IVALUE).is_neg())
      THEN
        ti.set_integer(1);
        IF y(IVALUE).is_neg() THEN z.i_plus(z,ti)
        ELSE z.i_minus(z,ti)
        END;
      END;
    |pc.sb_mod     :
      IF z.i_slash(x(IVALUE)^,y(IVALUE)^,TRUE) &
        (x(IVALUE).is_neg()#y(IVALUE).is_neg()) THEN
        z.i_plus(z,y(IVALUE)^)
      END;
    |pc.sb_slash   : IF z.i_slash(x(IVALUE)^,y(IVALUE)^,FALSE) THEN END;
    |pc.sb_rem     : IF z.i_slash(x(IVALUE)^,y(IVALUE)^,TRUE) THEN END;
    |pc.sb_plus,
     pc.sb_addadr  : z.i_plus(x(IVALUE)^,y(IVALUE)^);
    |pc.sb_minus,
     pc.sb_subadr,
     pc.sb_difadr  : z.i_minus(x(IVALUE)^,y(IVALUE)^);
    |pc.sb_or      : z.i_or(x(IVALUE)^,y(IVALUE)^);
    |pc.sb_xor     : z.i_xor(x(IVALUE)^,y(IVALUE)^);
    |pc.sb_and     : z.i_and(x(IVALUE)^,y(IVALUE)^);
    |pc.sb_bic     : z.i_bic(x(IVALUE)^,y(IVALUE)^);
    |pc.sb_lsh     :
      l:=y(IVALUE).get_integer();
      IF (l<MIN(INTEGER)) OR (l>MAX(INTEGER)) THEN
        z.val:=zero;
      ELSE
        z.val:=x(IVALUE).val; z.lshift(SHORT(l));
      END;
    |pc.sb_ash     :
      l:=y(IVALUE).get_integer();
      IF (l<MIN(INTEGER)) OR (l>MAX(INTEGER)) THEN
        y(IVALUE).ovr(); z.val:=zero;
      ELSE
        z.val:=x(IVALUE).val; z.shift(SHORT(l));
      END;
    |pc.sb_shl     :
      l:=y(IVALUE).get_integer();
      IF (l<-127) OR (l>127) THEN
        y(IVALUE).ovr(); z.val:=zero;
      ELSE
        z.val:=x(IVALUE).val; z.shift(SHORT(l MOD 128));
      END;
    |pc.sb_shr     :
      l:=y(IVALUE).get_integer();
      IF (l<-127) OR (l>127) THEN
        y(IVALUE).ovr(); z.val:=zero;
      ELSE
        z.val:=x(IVALUE).val; z.shift(SHORT(-(l MOD 128)));
      END;
    |pc.sb_len :
      WITH x: SVALUE DO
        ASSERT(x.str);
        ASSERT(y.get_integer() = 0);
        z.set_integer(LEN(x.arr^));
      END;
  END;
END binary;

PROCEDURE (VAR z: real_value) binary(cop: pc.SUB_MODE; x,y: pc.VALUE);
BEGIN
  CASE cop OF
    |pc.sb_mul     : z.rmul(x(RVALUE)^,y(RVALUE)^);
    |pc.sb_slash   : z.rdiv(x(RVALUE)^,y(RVALUE)^);
    |pc.sb_plus    : z.rplus(x(RVALUE)^,y(RVALUE)^);
    |pc.sb_minus   : z.rminus(x(RVALUE)^,y(RVALUE)^);
    |pc.sb_exp     :
      WITH y: IVALUE DO z.ri_exp(x(RVALUE)^,y^);
          |y: RVALUE DO z.rr_exp(x(RVALUE)^,y^);
      END;
  END;
END binary;

PROCEDURE (VAR z: complex_value) binary(cop: pc.SUB_MODE; x,y: pc.VALUE);
BEGIN
  CASE cop OF
    |pc.sb_mul     : z.cmul(x(CVALUE)^,y(CVALUE)^);
    |pc.sb_slash   : z.cdiv(x(CVALUE)^,y(CVALUE)^);
    |pc.sb_plus    : z.cplus(x(CVALUE)^,y(CVALUE)^);
    |pc.sb_minus   : z.cminus(x(CVALUE)^,y(CVALUE)^);
    |pc.sb_cmplx   : z.r.val:=x(RVALUE).val;
                     z.i.val:=y(RVALUE).val;
    |pc.sb_exp     :
      WITH y: IVALUE DO z.ci_exp(x(CVALUE)^,y^);
          |y: RVALUE DO z.cr_exp(x(CVALUE)^,y^);
      END;
  END;
END binary;

PROCEDURE (VAR z: packed_arr_value) binary(cop: pc.SUB_MODE; x,y: pc.VALUE);
  VAR i,j: LONGINT; b: BOOLEAN; sx,sy: pc.STRING;
BEGIN
  IF cop=pc.sb_concat THEN
    ASSERT(z.str);
    sx:=x(SVALUE).arr; sy:=y(SVALUE).arr;
    NEW(z.arr,LEN(sx^)+LEN(sy^)-1);
    j:=0; i:=0;
    WHILE sx^[i]#"" DO z.arr^[j]:=sx^[i]; INC(i); INC(j) END;
    i:=0;
    WHILE sy^[i]#"" DO z.arr^[j]:=sy^[i]; INC(i); INC(j) END;
    z.arr^[j]:="";
    RETURN;
  END;
  IF cop=pc.sb_lsh THEN j:=y(IVALUE).get_integer() ELSE j:=0 END;
  FOR i:=0 TO LEN(z.arr^)-1 DO
    CASE cop OF
      |pc.sb_and: b := (x(SVALUE).arr[i]#0X) &  (y(SVALUE).arr[i]#0X);
      |pc.sb_or : b := (x(SVALUE).arr[i]#0X) OR (y(SVALUE).arr[i]#0X);
      |pc.sb_xor: b := x(SVALUE).arr[i] # y(SVALUE).arr[i];
      |pc.sb_bic: b := (x(SVALUE).arr[i]#0X) &  (y(SVALUE).arr[i]=0X);
      |pc.sb_lsh: IF (j=MIN(LONGINT)) OR
                     (ABS(j)>LEN(z.arr^)) OR
                     ((i-j)<0) OR
                     ((i-j)>=LEN(z.arr^))
                  THEN b:=FALSE;
                  ELSE b:=x(SVALUE).arr[i-j]#0X;
                  END;
    END;
    IF b THEN z.arr[i]:=1X ELSE z.arr[i]:=0X END;
  END;
END binary;

PROCEDURE (VAR z: real_value) set_integer(x: LONGINT);
BEGIN
  z.val:=x;
END set_integer;

PROCEDURE (VAR z: complex_value) set_integer(x: LONGINT);
BEGIN
  z.r.val:=x;
  z.i.val:=0.0;
END set_integer;

PROCEDURE (VAR z: real_value) set_real(x: LONGREAL);
BEGIN
  z.val:=x;
END set_real;

PROCEDURE (VAR z: real_value) get_integer(): LONGINT;
BEGIN
  RETURN ENTIER(z.val);
END get_integer;

PROCEDURE (VAR z: real_value) get_real(): LONGREAL;
BEGIN
  RETURN z.val;
END get_real;

PROCEDURE (VAR z: proc_value) get_object(): pc.OBJECT;
BEGIN
  RETURN z.obj;
END get_object;

PROCEDURE (VAR z: proc_value) set_object(x: pc.OBJECT);
BEGIN
  z.obj:=x;
END set_object;

PROCEDURE (VAR z: packed_arr_value) get_NDWord(n :INTEGER): SYSTEM.CARD32;
VAR r: pc.VALUE;
BEGIN
    r := pc.value.new (z.pos, pc.ZZ_type);
    r.unary (pc.su_conv, SYSTEM.REF(z));
    RETURN r.get_NDWord(n);
END get_NDWord;

PROCEDURE (VAR v: packed_arr_value) set_string(s-: ARRAY OF CHAR);
  VAR i,j: INTEGER;
BEGIN
  i:=0;
  WHILE (i<LEN(s)) & (s[i]#0X) DO INC(i) END;
  NEW(v.arr,i+1);
  FOR j:=0 TO i-1 DO v.arr^[j]:=s[j] END;
  v.arr^[i]:=0X;
END set_string;

PROCEDURE (VAR z: int_value) index_set(i: LONGINT; v: pc.VALUE);
BEGIN
  IF v.is_zero() THEN
    EXCL(z.val[i DIV SET_BITS],i MOD SET_BITS);
  ELSE
    INCL(z.val[i DIV SET_BITS],i MOD SET_BITS);
  END;
END index_set;

PROCEDURE (VAR z: packed_arr_value) index_set(i: LONGINT; v: pc.VALUE);
BEGIN
  z.arr[i]:=CHR(v.get_integer());
END index_set;

PROCEDURE (VAR z: array_value) index_set(i: LONGINT; v: pc.VALUE);
BEGIN
  z.arr[i]:=v; (* copy ???? *)
END index_set;

PROCEDURE (VAR z: int_value) index_get(i: LONGINT; v: pc.VALUE);
BEGIN
  WITH v: SVALUE DO
    IF (i<0) OR (i>=LEN(v.arr^)) THEN
      z.set_integer(0);
    ELSE
      z.set_integer(ORD(v.arr[i]));
    END;
  |v: AVALUE DO
    z:=v.arr[i](IVALUE)^;
  END;
END index_get;

PROCEDURE (VAR z: real_value) index_get(i: LONGINT; v: pc.VALUE);
BEGIN
  WITH v: AVALUE DO
    z:=v.arr[i](RVALUE)^;
  END;
END index_get;

PROCEDURE (VAR z: proc_value) index_get(i: LONGINT; v: pc.VALUE);
BEGIN
  WITH v: AVALUE DO
    z:=v.arr[i](PVALUE)^;
  END;
END index_get;

PROCEDURE (VAR z: complex_value) index_get(i: LONGINT; v: pc.VALUE);
BEGIN
  WITH v: AVALUE DO
    z:=v.arr[i](CVALUE)^;
  END;
END index_get;

PROCEDURE (VAR z: array_value) index_get(i: LONGINT; v: pc.VALUE);
BEGIN
  WITH v: AVALUE DO
    copy_array_value(v.arr[i](AVALUE), z);
  END;
END index_get;

PROCEDURE (VAR z: packed_arr_value) index_get(i: LONGINT; v: pc.VALUE);
  VAR a: SVALUE; j: LONGINT;
BEGIN
  WITH v: AVALUE DO
    a:=v.arr[i](SVALUE);
    IF a.str & (LEN(a.arr^) # LEN(z.arr^)) THEN
      (* actual length of string not equal to that of array type *)
      ASSERT(z.str);
      NEW(z.arr,LEN(a.arr^));
    END;
    ASSERT(LEN(a.arr^)=LEN(z.arr^));
    FOR j:=0 TO LEN(a.arr^)-1 DO z.arr[j]:=a.arr[j] END;
  END;
END index_get;

PROCEDURE strapp(VAR d: ARRAY OF CHAR; s-: ARRAY OF CHAR);
  VAR i,j: INTEGER;
BEGIN
  i:=0;
  WHILE (i<LEN(d)) & (d[i]#'') DO INC(i) END;
  j:=0;
  WHILE (j<LEN(s)) & (s[j]#'') & (i<LEN(d)-1) DO
    d[i]:=s[j]; INC(i); INC(j)
  END;
  d[i]:='';
END strapp;

PROCEDURE (VAR z: int_value) value_to_str(VAR s: ARRAY OF CHAR; f: pc.Lang);
  VAR
    zr,fs: BOOLEAN;
    bf: ARRAY 32 OF CHAR;
    x : int_value;
    i : INTEGER;
    p : LCPTR;
BEGIN
  x:=z; s[0]:=0X;
  IF x.is_neg() THEN x.i_neg; s[0]:="-"; s[1]:=0X END;
  IF x.is_short() THEN
    p:=x.lptr();
    wio.CardToStr(p.v,bf); strapp(s,bf); RETURN;
  END;

  IF f=pc.flag_c THEN strapp(s,"0x0") ELSE strapp(s,"0") END;
  zr:=TRUE;
  FOR i:=MAX_SET TO 0 BY -1 DO
    fs:=zr; zr:=zr & (x.val[i]={});
    IF NOT zr THEN
      IF fs THEN fmt.prn_txt(bf,"%X",x.val[i]);
      ELSE fmt.prn_txt(bf,"%.8X",x.val[i]);
      END;
      strapp(s,bf);
    END;
  END;
  IF f#pc.flag_c THEN strapp(s,"H") END;
END value_to_str;

PROCEDURE (VAR z: packed_arr_value) value_to_str(VAR s: ARRAY OF CHAR; f: pc.Lang);
  VAR i,j,k,l: LONGINT;
BEGIN
  ASSERT(NOT z.str & (f=pc.flag_c));
  s[0]:='0'; s[1]:='x'; l:=2;
  j:=(LEN(z.arr^)+3) DIV 4;
  FOR i:=j*4 TO 4 BY -4 DO
    k:=0;
    FOR j:=1 TO 4 DO
      k:=k*2;
      IF ((i-j)<LEN(z.arr^)) & (z.arr[i-j]#0X) THEN INC(k) END;
    END;
    IF k>=10 THEN s[l]:=CHR(k+ORD('A')-10);
    ELSE s[l]:=CHR(k+ORD('0'));
    END;
    IF (l>2) OR (s[l]#'0') THEN INC(l) END;
  END;
  IF l=2 THEN l:=1 END;
  s[l]:=0X;
END value_to_str;

PROCEDURE (VAR z: real_value) value_to_str(VAR s: ARRAY OF CHAR; f: pc.Lang);
  VAR t,i,j,zr,pt,ex: INTEGER; bf: ARRAY 32 OF CHAR; c: CHAR;
BEGIN
  t:=0;
  LOOP
    IF t=0 THEN lio.RealToStr(z.val,bf) ELSE lio.RealToFloat(z.val,0,bf) END;
    i:=0; j:=0; pt:=-1; ex:=-1; zr:=-1;
    LOOP
      IF i>=LEN(bf) THEN c:=0X ELSE c:=bf[i] END;
      CASE c OF
        |'E'     : IF zr>0 THEN j:=zr END;
                   IF pt<0 THEN s[j]:='.'; pt:=j; INC(j) END;
                   ex:=j; s[j]:='E'; INC(j);
        |'.'     : s[j]:='.'; pt:=j; INC(j); zr:=j;
        |'-','+' : s[j]:=bf[i]; INC(j);
        |'0'     : s[j]:=bf[i]; INC(j);
                   IF (j>2) & (pt=j-2) & (ex<0) THEN zr:=j END;
        |'1'..'9': s[j]:=bf[i]; INC(j);
                   IF (pt>=0) & (ex<0) THEN zr:=j END;
        |0X      : IF (zr>0) & (ex<0) THEN j:=zr END;
                   IF pt<0 THEN s[j]:='.'; INC(j) END;
                   s[j]:=0X; EXIT;
      END;
      INC(i);
    END;
    IF (t#0) OR (ex>=0) OR (j<=7) THEN EXIT END;
    INC(t);
  END;
END value_to_str;

PROCEDURE (VAR z: int_value) str_to_value(s-: ARRAY OF CHAR; f: pc.Lang);
  VAR dg,bs: int_value; i,j,e,ibs,iz: LONGINT;
BEGIN
  e:=LENGTH(s)-1;
  IF e<=7 THEN
    CASE s[e] OF
      |'H','X': ibs:=16; DEC(e);
      |'B','C': ibs:=8; DEC(e);
      |'I'    : ibs:=2; DEC(e);
    ELSE
      ibs:=10;
    END;
    i:=0; iz:=0;
    WHILE i<=e DO
      IF s[i]>='A' THEN j:=ORD(s[i])-ORD('A')+10;
      ELSE j:=ORD(s[i])-ORD('0');
      END;
      iz:=iz*ibs+j;
      INC(i);
    END;
    z.set_integer(iz);
    RETURN;
  END;
  CASE s[e] OF
    |'H','X': bs.set_integer(16); DEC(e);
    |'B','C': bs.set_integer(8);  DEC(e);
    |'I'    : bs.set_integer(2);  DEC(e);
  ELSE
    bs.set_integer(10);
  END;
  i:=0;
  z.val:=zero;
  WHILE i<=e DO
    IF s[i]>='A' THEN j:=ORD(s[i])-ORD('A')+10;
    ELSE j:=ORD(s[i])-ORD('0');
    END;
    dg.set_integer(j);
    z.i_mul(z,bs);
    z.i_plus(z,dg);
    INC(i);
  END;
END str_to_value;

PROCEDURE (VAR z: real_value) str_to_value(s: ARRAY OF CHAR; f: pc.Lang);
  VAR res: lio.ConvResults; i: LONGINT;
BEGIN
  i:=0;
  WHILE (s[i]#0X) & (s[i]#'i') DO
    IF (s[i]='D') OR (s[i]='e') THEN s[i]:='E' END;
    INC(i);
  END;
  s[i]:=0X;
  IF (i>0) & (f=pc.flag_c) & ((s[i-1]='F') OR (s[i-1]='f')) THEN
    s[i-1]:=0X;
  END;
  lio.StrToReal(s,z.val,res);
  IF res#lio.strAllRight THEN z.ovr; z.val:=0.0 END;
END str_to_value;

PROCEDURE (VAR z: complex_value) str_to_value(s-: ARRAY OF CHAR; f: pc.Lang);
BEGIN
  z.r.pos:=z.pos;
  z.r.val:=0.0;
  z.i.pos:=z.pos;
  z.i.str_to_value(s,f);
END str_to_value;

VAR min1: INTEGER; (* to avoid optimizations *)

PROCEDURE Set*;
  VAR
    i: INTEGER;
    p: IVALUE;
    l: int_value;
    f: CBF;
BEGIN
  ASSERT( SET_BITS = 32 );
  ASSERT(min1 DIV 2 = -1);
  ASSERT(SIZE(CBFB) = SIZE(BUF));
  FOR i:=0 TO MAX_SET DO zero[i]:={} END;
  f:=l.vptr();
  l.val[0]:={0,9,18,27};
  IF (f[0]=0804H) & (f[1]=0201H) THEN
    FOR i:=0 TO LEN(cbf_inx)-1 BY 2 DO
      cbf_inx[i+1]:=SHORT(i);
      cbf_inx[i]:=SHORT(i+1);
    END;
  ELSIF (f[0]=0201H) & (f[1]=0804H) THEN
    FOR i:=0 TO LEN(cbf_inx)-1 DO
      cbf_inx[i]:=SHORT(i);
    END;
  ELSE
    ASSERT(FALSE);
  END;
  NEW(p); pc.value:=p;
END Set;

BEGIN
  min1:=-1;
END pcNum.
