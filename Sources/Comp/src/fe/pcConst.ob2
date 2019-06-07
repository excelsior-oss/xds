(** Copyright (c) 1994,98 XDS Ltd, Russia. All Rights Reserved. *)
(** o2/m2 development system v2.0: IR transformer *)
MODULE pcConst;
<* IF ~DEFINED(STATANALYSIS)  THEN *> <* NEW STATANALYSIS- *>  <* END *>

(*Questions:
   @?1: Check for REALs in Assign
*)

(* Modifications:
   20-Oct-98 Vit @23: .use list is processed for ob_cproc too.
   16-Jul-98 Vit @22: Type name is ObjectUsage'd to process all names
   01-Jul-98 Vit @21: Generate all procedures in C target
   17-Jun-98 Vit @19: SetValue: CheckValueRange is called for constants,
                      and range check is eliminated / error reported
                 @20: su_conv(sb_cmplx(ty_CC),ty) -> sb_cmplx(ty)
   15-May-98 Vit @15: EnConsProp, EnStatOpt, EnAllProcs options introduced
                 to perform expr/stat optimizations on IR
                 (IR changes marked by IRMOD:partial or IRMOD!-general use)
                 @16: EnGoto merged to EnStatOpt
                 @17: symmetrical booleans comparison in GenBinary
                 @18: sb_cand/cor values map analysis improved
   11-May-98 Vit Tracing facilities inserted
                 @14 nd_var.val keeps ob_cons.val.val to meet @12
   20-Apr-98 Vit @11 su_min/su_max calculation eliminated for BE_C (Megos FS)
                 @12 values revolution (NODE->VALUE, not vs) done
                 @13 position setting for vIntTmp eliminated -- nafig nado
      Feb-98 Vit  Optimizations
             @1  EnAtariTC eliminated: obsolete
             @2  EnRealMinmax eliminated
             @3  PROCTYPEs nomore used
             @4  VALUES tags combined in a bitset; procedures for bitset
                 operations for DFA are implemented
             @5  naming improved to M2 style
             @6  X340 elimination is completed: TYPES control is eliminated;
                 'type' param in DefValue, ChkValue, ObjectUsage eliminated.
             @7  property 'any' eliminated: means val=NIL.
             @8  'dead' params of GenSequence, GenStatement, CaseVariant
                 turned to return values
             @9 GenValue~ -> GenExpr~ for more explicity
             @10 'fwd' param of GenStatement, Gensequence eliminated as not used.
   15-Feb-96 Ned  "Power2" is reimplemented. Bug (overflow) is fixed.
   19-Feb-96 Ned  "ProcIsInline" additional check that actual VAR parameter
                   for inlined function should be variable or field access.
   01-Mar-96 Ned  "ProcIsInline" additional check is removed, only
                   procedure with less than 5 variables/constants can
                   be inlined.
                  "InlineProc" variable of pointer type is
                   allocated for VAR parameter if the actual parameter is
                   not simple variable or field access.

                   "GenProc" restriction for inlined procedure is changed to:
                        <number of statements>*<number of calls>  <= 64
   13-Mar-96 Ned   Warning 340 is commented out (all places).
   23-Mar-96 Ned   GenExprUnary.su_cast: cast_ordinal is used.
   26-Mar-96 Ned   GenExprUnary.su_cast: cast_ordinal is called for
                   ordinal types only!
   27-Mar-96 Ned   PRO0046: GenExprUnary.su_conv: conversion
                   ty_SS -> ty_array is evaluated for all contexts except
                   procedure call. See also fix in GenCall to prevent this
                   evaluation.
   29-Mar-96 Ned   conversion (see above) is not done for assignment also.
   03-Apr-96 Ned   SplitTree: nd_lconv -> nd_lconv (not su_conv).
   15-Apr-96 Ned   constants of proctype with value NIL are invented.
                   GenExprUnary.su_conv: NIL -> proctype
   24-Apr-96 Ned   BUG0110 fixed: ValueUsage.flist - correct parsing of
                   a record constant.
   29-Apr-96 Ned   GenStatement.nd_goto - optimization is commented out.
                   See comment at the place.
   16-May-96 Ned   PRO0124: ttag_ & otag_ -> tmark & omark
   30-May-96 LAZ   inlining of a procedure with VAR oberon record parameter
                   is denied now (see "ProfOk" in "ProcIsInline")
   28-Oct-96 LAZ   function inlining implemented
   29-Oct-96 LAZ   ValsAppUsage was added in nd_repeat node processing
                   (see "GenStatement") to obtain correct write information

                   "ProcIsInline" limit of 5 variables/constants was
                   increased to 20 when function inlining is on
   20-Oct-97 LAZ   SplitTree: nd_lconv -> nd_lconv, but su_conv -> su_conv !!
*)

IMPORT
  SYSTEM
, pc  := pcK
, fmt := xcStr
, env := xiEnv
<* IF TARGET_IDB THEN *>
, model2
, IVERAS
<* END *>
<* IF DB_TRACE THEN *>
, dbg := pcVisIR
<* END *>
, pcS;

(* notes:
   1. Non-invariant transformations of VALUE extracted from the tree are
      forbidden! It may have other references to.
   2. nd_binary is assumed not to be a reference to an object (designator)
   3. Expression tree is not removed after const calculations: set value
      indicates that the tree is not necessary.
   4. All calculated values are considered safe: their expressions contain
      no side effects (ie. no run-time checks required) and may be eliminated.
*)

CONST
  ZZs      = pc.ORDs + pc.PADRs + pc.TY_SET{pc.ty_loc};
  NUMs     = ZZs + pc.REALs;
  CNUMs    = NUMs + pc.CPLXs;

  omark_used = pc.omark_aux20;  -- object is used in CurProc (included in .use list)
  omark_incl = pc.omark_aux21;
  omark_keep = pc.omark_aux22;  -- object is used, must be saved

  tmark_ingen   = pc.tmark_aux20;      -- GenProc started for this procedure
  tmark_genok   = pc.tmark_aux21;      -- GenProc completed for this procedure
  tmark_inlined = pc.tmark_aux22;      -- procedure has been inlined
  tmark_basetype= pc.tmark_aux23;      -- type has extensions
  tmark_inl_dis = pc.tmark_aux24;      -- procedure cant be inlined

  (* object usage tags (used to pass as 'tags:SET' param for some procs) *)
  U_RD = pc.UTAG_SET{pc.utag_read};   -- read
  U_WR = pc.UTAG_SET{pc.utag_write};  -- write
  U_CL = pc.UTAG_SET{pc.utag_call};   -- procedure call
  U_SZ = pc.UTAG_SET{};               -- get size or offs or length
  U_TP = pc.UTAG_SET{};               -- get dynamic type
  U_AD = pc.UTAG_SET{};               -- get address
  U_VP = U_WR;             -- var parameter

<* IF TARGET_MEGOS THEN *>
  ntag_lhs = 30;
<* END *>

VAR
  nestedThrowsCall:BOOLEAN;
  vIntTmp  : pc.VALUE;     -- temp.value, used as result for calcs on values
  vIntCmp  : pc.VALUE;     -- other! value to perform values comparison
  vIntOne  : pc.VALUE;     -- ZZ 1
  vIntZero : pc.VALUE;     -- ZZ 0
(*3
  IntOne   : pc.NODE;      -- nd_value 1
  IntZero  : pc.NODE;      -- nd_value 0
*)

  VoidType : pc.STRUCT;

  CurLevel : INTEGER;
  CurProc  : pc.STRUCT;
  CurMod   : pc.STRUCT;

  Restart  : BOOLEAN; (* if TRUE then retry requested *)

  INLINE_MAXWEIGHT  : LONGINT;
  INLINE_SMALLLIMIT : LONGINT;

(* --- Switches to dis/enable some analysis & optimizations ---
       in 'eval_module' (M) and 'eval_value/eval_const' (V)     *)

  EnReusageChk : BOOLEAN; -- temp.vars reusage check
                          --   M: = pc.code.en_tmpvar V: = FALSE
  EnUsageChk   : BOOLEAN; -- vars usage check
                          --   M: = TRUE              V: = FALSE
  EnStatOpt    : BOOLEAN; -- statements & control flow optimization
                          --   M: = Opt|"IRSTATOPT"   V: = FALSE
  EnConsProp   : BOOLEAN; -- values analysis, const propagation, exprs opt-n
                          --   M: = Opt|"IRCONSPROP"  V: = FALSE
  EnInline     : BOOLEAN; -- procedure inlining (upto INLINE_LEVEL)
                          --   M: = "PROCINLINE"      V: = FALSE
  EnAllProcs   : BOOLEAN; -- process all/used only procedures
                          --   = "IRALLPROCS"
  EnSL1Addr    : BOOLEAN; -- SL1 ADDR processing
                          --   = "STAT"
<* IF DB_TRACE THEN *>
  EnDebug      : BOOLEAN; -- tracing
                          --   = "IRVIS"
<* END *>


(*===================== Error & warnings diplay ========================*)


PROCEDURE Fault (  p : pc.TPOS         -- Fault output position
                ; no : INTEGER );      -- Fault number
(**
   Raise a Fault #'no'
*)
BEGIN
  env.errors.Fault (p, no);
END Fault;

-------------------------------------------------------------------------

PROCEDURE Err (  p : pc.TPOS           -- error output position
              ; no : INTEGER );        -- error number
(**
   Raise an error #'no'
*)
BEGIN
  env.errors.Error (p, no);
END Err;

-------------------------------------------------------------------------
VAR
  insideInline: BOOLEAN;

PROCEDURE Wrn ( pos : pc.TPOS           -- warning output position
              ;  no : INTEGER );        -- warning number
(**
   Raise a warning #'no'
*)
BEGIN
  IF pos.IsNull () THEN RETURN END;
  env.errors.Warning (pos, no);
END Wrn;

-------------------------------------------------------------------------

PROCEDURE WrnObj (  o : pc.OBJECT      -- #NIL, name and position
                 ; no : INTEGER );     -- warning number
(**
   Raise a warning #'no' by object's 'o' name and position
*)
BEGIN
  IF o.pos.IsNull () THEN RETURN END;
  env.errors.Warning (o.pos, no, o.name^);
END WrnObj;


(*======================= Miscellaneous ==========================*)

PROCEDURE EquationValue(name-: ARRAY OF CHAR) : LONGINT; --*FSA
VAR
  s: env.String;
  i: LONGINT;
BEGIN
  env.config.Equation(name,s);
  IF (s#NIL) & fmt.StrToInt(s^,i) THEN
    RETURN i;
  ELSE
    env.errors.Warning (env.null_pos, 176, s, name); -- 176 "%s" illegal value for equation %s
    RETURN 1;
  END;
END EquationValue;


PROCEDURE IsChar ( t : pc.STRUCT             -- #NIL
                 )   :           BOOLEAN;
(**
  Check if 't' is CHAR or range of CHAR
*)
BEGIN
  IF t.mode = pc.ty_range THEN
    t := t.base
  END;
  RETURN t.mode = pc.ty_char;
END IsChar;

-------------------------------------------------------------------------

PROCEDURE CmpValue (  op : pc.SUB_MODE
                   ; x,y : pc.VALUE
                   )     :          BOOLEAN;
(**
   Perform comparison operator 'op' on values 'x' and 'y'
*)
BEGIN
(*vIntCmp.pos := x.pos;         -- Vit: what for? *)
  vIntCmp.binary (op, x, y);
  RETURN ~vIntCmp.is_zero ();
END CmpValue;

--------------------------------------------------------------------------------
-- Check if value is in its type valid range.
-- Envokes mostly for type casting; for other operations checking is done
-- by methods .unary(), .binary()
PROCEDURE CheckValueRange ( n: pc.NODE ); --.val # NIL; .type is_ordinal or REAL
BEGIN
  ASSERT( n.mode = pc.nd_value (*3/ n.val # NIL *));

  IF ~n.type.is_ordinal () & ~(n.type.mode IN pc.REALs) -- not numeric
  OR (n.type.mode IN pc.TY_SET{pc.ty_ZZ, pc.ty_RR, pc.ty_CC})    -- literal const
  THEN
    RETURN
  END;

  IF CmpValue (pc.sb_lss, n.val, n.type.min(*3 .val*))
  OR CmpValue (pc.sb_gtr, n.val, n.type.max(*3 .val*))
  THEN
    Err (n.pos, 122);         -- "expression out of bounds"
  END;
  EXCL (n.tags, pc.ntag_chk_range);  -- range check done already
END CheckValueRange;

-------------------------------------------------------------------------

PROCEDURE IniNode (  n: pc.NODE
                  ;  p: pc.TPOS
                  ; md: pc.ND_MODE
                  ;  t: pc.STRUCT );
(**
   Initializes node 'n' with mode 'md', position 'p' and type 't'
*)
BEGIN
  n.tags:= pc.NTAG_SET{};
  n.mode:= md;
  n.sub := pc.su_none;
  n.type:= t;
  n.pos := p;
  n.obj := NIL;
  n.val := NIL;
  n.l   := NIL;
  n.r   := NIL;
  n.next:= NIL;
END IniNode;

-------------------------------------------------------------------------

PROCEDURE NewNode (  p: pc.TPOS
                  ; md: pc.ND_MODE
                  ;  t: pc.STRUCT
                  )   :            pc.NODE;
(**
   Create node with mode 'md', position 'p' and type 't'
*)
VAR n: pc.NODE;
BEGIN
  NEW (n);
  IniNode (n, p, md, t);
  RETURN n;
END NewNode;

-------------------------------------------------------------------------

PROCEDURE NewNodObj ( m: pc.ND_MODE          -- mode
                    ; o: pc.OBJECT           -- object to reference
                    ; p: pc.TPOS             -- position
                    )  :           pc.NODE;
(**
   Create node with mode 'm' to represent 'o'; position 'p'
*)
VAR n: pc.NODE;
BEGIN
  n := NewNode (p, m, o.type);
  n.obj := o;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN
    dbg.CallTraceInd (0, "NewNodObj: %X .obj=%X .mode=", n, o); dbg.Trace(dbg.VisNodMode [n.mode]);
  END;
<* END *>
  RETURN n;
END NewNodObj;

------------------------------------------------------------------------------

PROCEDURE CopyNode ( fr: pc.NODE
                   )   :          pc.NODE;
(**
   Make a copy of node 'fr'.
   All fields are copied; .pos = null_pos
*)
VAR n: pc.NODE;
BEGIN
  NEW (n);
  n.copy (fr);

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (0, "CopyNode: %X->%X", fr, n) END;
<* END *>
  RETURN n;
END CopyNode;

-------------------------------------------------------------------------

PROCEDURE SetValue ( n : pc.NODE       -- #NIL, .type#NIL
                   ; v : pc.VALUE      -- #NIL
                   );
(**
  Store value 'v' to node 'n'; in EnConsProp turn 'n' to 'nd_value'.
*)
BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (0, "SetValue: %X.val=%X", n, v) END;
<* END *>

  ASSERT( n.type # NIL );
  ASSERT( v # NIL );

(*3/end proc
  n.val := v;
  IF EnConsProp THEN
    n.mode := pc.nd_value -- replace expr by value in EnConsProp
  END;
*)
  IF EnConsProp THEN
--  ASSERT( v.pos.equ(n.pos) );
--  ASSERT( v.expr = NIL );
    NEW(v.expr);
    v.expr.copy(n);
    v.expr.next := NIL;
    v.expr.pos := n.pos;
    v.expr.end := n.end;
  END;
  v.correctUnsigned();
  n.mode:=pc.nd_value;    -- replace expr by value in EnConsProp
  n.sub:=pc.su_none;
  n.tags:=pc.NTAG_SET{};
  n.obj:=NIL;
  n.l:=NIL;
  n.r:=NIL;
  n.val:=v;
END SetValue;

-------------------------------------------------------------------------

PROCEDURE NewValue ( n: pc.NODE );
(**
   Creates an undefinite value and sets it to 'n'
*)
BEGIN
  SetValue (n, pc.value.new (n.pos, n.type) ); -- pos, type of node
END NewValue;

-------------------------------------------------------------------------

PROCEDURE EvalVal ( n : pc.NODE     -- #NIL, .type, # NIL
                  ; i : LONGINT );  -- value to set
(**
   Create value for 'i' and store it in node 'n'.
*)
VAR v: pc.VALUE; (*3/ *)
BEGIN
  v := pc.value.new(n.pos, n.type);
  (*3/  NewValue (n); *)
  v (*3/ n.val*).set_integer (i);
  SetValue (n, v ); (*3/ *)
  IF pc.ntag_chk_range IN n.tags THEN
    CheckValueRange (n);   -- @19
  END;
END EvalVal;

-------------------------------------------------------------------------

PROCEDURE EvalUn ( n : pc.NODE          -- #NIL, .type # NIL
                 )   :         BOOLEAN; --> T: if .l.val # NIL
(**
   If unary op in node 'n' has constant arg, calculate 'n's value.
   Value validation is performed with calculation.
*)
VAR v: pc.VALUE; (*3/ *)
BEGIN
  IF n.l.mode # pc.nd_value (*3/ n.l.val = NIL*) THEN
    RETURN FALSE
  END;
  v := pc.value.new(n.pos, n.type);
  (*3/  NewValue (n); *)
  v (*3/ n.val*).unary (n.sub, n.l.val);
  SetValue (n, v ); (*3/ *)
  IF pc.ntag_chk_range IN n.tags THEN
    CheckValueRange (n);   -- @19
  END;
  RETURN TRUE;
END EvalUn;

-------------------------------------------------------------------------

PROCEDURE EvalBin ( n : pc.NODE          -- #NIL, .type # NIL
                  )   :         BOOLEAN; --> T: if both .l.val .r.val # NIL
(**
   If binary op in node 'n' has constant args, calculate 'n's value.
   Value validation is performed with calculation.
*)
VAR v: pc.VALUE; (*3/ *)
BEGIN
  IF (n.l.mode # pc.nd_value(*3/n.l.val = NIL*))
  OR (n.r.mode # pc.nd_value(*3/n.r.val = NIL*)) THEN
    RETURN FALSE
  END;
  v := pc.value.new(n.pos, n.type);
  (*3/  NewValue (n); *)
  v (*3/ n.val*).binary (n.sub, n.l.val, n.r.val);
  SetValue (n, v ); (*3/ *)
  IF pc.ntag_chk_range IN n.tags THEN
    CheckValueRange (n);   -- @19
  END;
  RETURN TRUE;
END EvalBin;

-------------------------------------------------------------------------

PROCEDURE SetValueCast ( n : pc.NODE
                       ; v : pc.VALUE );
(**
   Copy 'v' to 'n's value with casting to its type
*)
VAR w: pc.VALUE;
BEGIN
(*3/end proc
  NewValue (n);
  n.val.unary (pc.su_cast, v);
  IF pc.ntag_chk_range IN n.tags THEN
    CheckValueRange (n);   -- @19
  END;
*)
  w := v;
  IF EnConsProp THEN
    w := pc.value.new(n.pos,n.type);
    w.unary (pc.su_cast, v); (* to copy value, see pcNum.unary(cast) *)
  END;
  SetValue (n, w);
END SetValueCast;

-------------------------------------------------------------------------

PROCEDURE Power2 (     n: pc.NODE           -- .val # NIL
                 ; VAR v: pc.VALUE          -- if T: 2^m-1
                 )      :          BOOLEAN;
(**
   If n represents 2^m, returns v=2^m-1 (to be treated as bit mask)
*)
VAR w: LONGINT;

BEGIN
  IF n.val.is_neg ()
  OR CmpValue (pc.sb_geq, n.val, pc.longint_type.max(*3 .val*))
  THEN
    RETURN FALSE;
  END;

  w := n.val.get_integer ();
  IF (w > 0)
   & (SYSTEM.VAL(SET, w-1) * SYSTEM.VAL(SET, w) = {})
  THEN
    v := pc.value.new (n.pos, n.type);
    v.set_integer (w-1);
    RETURN TRUE;
  END;

  RETURN FALSE;
END Power2;


(*================== EXPRESSIONS REUSABILITY PROCESSING ===================*)

CONST             -- Reusability modes:
  ru_prm    = 0;
  ru_assign = 1;
  ru_binary = 2;
  ru_this   = 3;
  ru_deref  = 4;

PROCEDURE MakeReusable ( VAR host: pc.NODE
                       ;     expr: pc.NODE
                       ;       md: SHORTINT    -- reusability mode
                       ;     lval: BOOLEAN
                       )         :         BOOLEAN; -- replacement done
(**
   Make 'expr' reusable (upto IsReusable predicate).
   If not yet, copy it to a temporary var, and replace 'host' by
   pair assign->use, if in statements list, or by <nd_replace(assign,use)>.
   (actual 'VAR host' is usually a link position in some node: changing it
   causes tree restructuring)
*)
  -----------------------------------------------

  PROCEDURE IsReusable (   n : pc.NODE     -- expression node to check
                       ;  lev: SHORTINT
                       ;   md: SHORTINT
                       ; lval: BOOLEAN
                       )     :          BOOLEAN;
  (**
     Check if expression has no side effect and is rather small
  *)

  VAR next_lev: SHORTINT;

  BEGIN
    IF ~EnReusageChk THEN
      RETURN TRUE
    END;

    CASE md OF
    | ru_prm:
       IF ~lval & (n.type.mode IN (pc.REALs+pc.CPLXs+pc.TY_SET{pc.ty_record}))
       OR (n.type.mode = pc.ty_pointer)
       OR (n.dynamic_type () # NIL)
       THEN
         RETURN TRUE;
       END;
    | ru_this:
       IF n.mode = pc.nd_call THEN
         RETURN FALSE
       END;
       IF n.dynamic_type () # NIL THEN
         RETURN TRUE
       END;
    | ru_binary
    , ru_assign:
       IF n.type.mode#pc.ty_array_of THEN
         RETURN TRUE
       END;
    | ru_deref:
    END;

    CASE n.mode OF
    | pc.nd_value
    , pc.nd_sequence
    , pc.nd_var:
       RETURN TRUE;

    | pc.nd_index:
       RETURN (n.l.type.mode = pc.ty_array_of)
            & IsReusable (n.l, lev, md, lval);

    | pc.nd_field
    , pc.nd_lconv:
       RETURN IsReusable (n.l, lev, md, lval);

    | pc.nd_deref:
       RETURN (lev = 0) & IsReusable (n.l, lev+1, ru_deref, FALSE);

    | pc.nd_guard:
       IF pc.ntag_chk_range IN n.tags
         THEN next_lev := 1
         ELSE next_lev := 0
       END;
       RETURN (lev = 0) & IsReusable (n.l, next_lev, md, lval);

    | pc.nd_unary:
       RETURN (n.sub = pc.su_conv)
            & IsReusable (n.l, lev, md, lval);
    ELSE
      RETURN FALSE;
    END;
  END IsReusable;

  ------------------------------------------

  PROCEDURE SplitTree (       n: pc.NODE    -- type(expr^)[i1]..[iN]
                      ;   VAR a: pc.NODE    -- => newvar:=copy(n)
                      ;   VAR b: pc.NODE    -- => nd_var(newvar)
                      ;    lval: BOOLEAN ); -- expr/desig
  (**
     Copy 'n', introducing a new var for expr.
     Indices are allowed for dynamic array expr only.
     All operations are optional.
  *)
  VAR  h: pc.STRUCT;
      ix: ARRAY 10 OF pc.NODE;   -- save dynamic array indices
      no: INTEGER;               -- counter of indices
     chk: BOOLEAN;
    dref: pc.NODE;
    conv: pc.NODE;
    gard: pc.NODE;
  BEGIN
  <* IF DB_TRACE THEN *>
    IF EnDebug THEN
      dbg.CallTraceInd (1, "[SplitTree: n=%X lval=%b", n, lval)
    END;
  <* END *>

    IF n.mode = pc.nd_replace THEN
      a := n.l;
      b := n.r;
  <* IF DB_TRACE THEN *>
      IF EnDebug THEN dbg.CallTraceInd (-1, "] => a=%X b=%X", a, b) END;
  <* END *>
      RETURN;
    END;
    chk := pc.ntag_chk_range IN n.tags;

    no := 0;         -- Save chain of indices nodes of dynamic array indexing
    WHILE (n.type.mode = pc.ty_array_of)
        & (n.mode = pc.nd_index)
        & (n.l.type.mode = pc.ty_array_of) DO
      ix[no] := n.r;
      n := n.l;
      INC (no);
    END;             -- n is array designator now

    gard := NIL;
    conv := NIL;
    dref := NIL;
    IF lval THEN
      IF (n.mode = pc.nd_lconv)     -- preserve conversion node, if any
      OR ((n.mode = pc.nd_unary)
       & (n.sub = pc.su_conv))
      THEN
        conv := n;
        n    := n.l;
      END;
      IF (n.mode = pc.nd_guard)     -- preserve guard node, if any  (* *FSA *)
      THEN
        gard := n;
        n    := n.l;
      END;
      IF n.mode = pc.nd_deref THEN  -- preserve dereference node, if any
        dref := n;
        n    := n.l;
      END;
      IF dref = NIL THEN            -- Intern.error
        Fault (n.pos, 142)
      END;
        (* !!! Vit: lval only with deref?! *)
    END;

    IF CurProc # NIL                -- create a = 'newvar := copy(n)' node
      THEN h := CurProc;
      ELSE h := CurMod;
    END;
    a := NewNodObj (pc.nd_assign,
                    pc.new_object_ex (n.type, h, pc.ob_var, FALSE),
                    n.pos);
    NEW (a.r); a.r^ := n^;

    b := NewNodObj (pc.nd_var,      -- create b = 'nd_var newvar' node
                    a.obj, a.pos);

    IF dref # NIL THEN              -- restore dereference, if was
      n   := NewNode (dref.pos, pc.nd_deref, dref.type);
      n.l := b;
      b   := n;
    END;

    IF gard # NIL THEN
      n   := NewNode (gard.pos, pc.nd_guard, gard.type);
      n.l := b;
      b   := n;
    END;

    IF conv # NIL THEN                -- restore conversion, if was
      n := NewNode (conv.pos, conv.mode, conv.type);   (* LAZ 20-10-97 *)
      IF conv.mode = pc.nd_unary THEN
        n.sub := pc.su_conv
      END;
      n.l := b;
      b := n;
    END;

    WHILE no > 0 DO                   -- restore all saved indexing
      DEC (no);
      n   := NewNode (b.pos, pc.nd_index, b.type.base);
      n.r := ix[no];
      n.l := b;
      b   := n;
    END;

    IF chk THEN INCL (b.tags, pc.ntag_chk_range) END;
  <* IF DB_TRACE THEN *>
    IF EnDebug THEN dbg.CallTraceInd (-1, "]SplitTree => a=%X b=%X", a, b) END;
  <* END *>
  END SplitTree;

  -----------------------------------------------

VAR a,b,n,x: pc.NODE;

BEGIN
  IF IsReusable (expr, 0, md, lval) THEN  -- goof already
    RETURN FALSE
  END;

  SplitTree (expr,              -- make a copy: 'a,b' = '<assign>,<use>'
             a, b, (*=>*)
             lval OR (expr.type.mode = pc.ty_array_of) );
  IF host.type.mode = pc.ty_void THEN -- in statements list
    a.next := host;                   -- replace 'host' by 'a->host'
    a.type := host.type;
    a.pos  := host.pos;
    host   := a;
  ELSE                                -- replace 'host' by '<nd_replace(a,host)>'
    NEW (n);
    n.copy (host);
    n.pos := host.pos;
    x := host.next;                   -- save from init
    IniNode (host, host.pos, pc.nd_replace, host.type);
    host.l := a;
    host.r := n;
    host.next := x;
  END;
  expr.copy (b);                      -- replace 'expr' by 'b'

  Restart := TRUE;
  RETURN TRUE;
END MakeReusable;

-------------------------------------------------------------------------

PROCEDURE RemoveExpr ( VAR n: pc.NODE );    -- expression  (* IRMOD *)
(**
   Eliminates expression that has no side effect.
   Applies to expressions which value is not used.
   (actual 'VAR n' is usually a link position in some node: changing it
   causes tree restructuring)
*)
  -----------------------------------------------

  PROCEDURE Remove ( VAR n: pc.NODE );
  (*
  *)
  BEGIN
    LOOP
      CASE n.mode OF
      | pc.nd_var, pc.nd_value :   (*!!! .val # NIL? *)
          n := NIL;
          EXIT;
      | pc.nd_field :
          n := n.l;
      | pc.nd_deref :
          IF pc.ntag_chk_range IN n.tags THEN
            EXIT
          END;
          n := n.l;
      | pc.nd_unary :
          IF (pc.ntag_chk_overflow IN n.tags)
          OR (pc.ntag_chk_range IN n.tags)
          THEN
            EXIT;
          END;
          n := n.l;
      | pc.nd_binary :
          IF (pc.ntag_chk_overflow IN n.tags)
          OR (pc.ntag_chk_range IN n.tags)
          OR (n.sub = pc.sb_pre_inc)
          OR (n.sub = pc.sb_pre_dec)
          OR (n.sub = pc.sb_post_inc)
          OR (n.sub = pc.sb_post_dec)
          OR (n.sub = pc.sb_cand)
          OR (n.sub = pc.sb_cor)
          THEN
            EXIT;
          END;
          Remove (n.r(*=>*));
          IF n.r = NIL THEN
            n := n.l;
          ELSE
            Remove (n.l(*=>*));
            IF n.l = NIL THEN
              n := n.r;
            ELSE
              n.mode := pc.nd_replace;
              n.sub := pc.su_none;
            END;
            EXIT;
          END;
      ELSE
        EXIT;
      END;
    END;
  END Remove;

  -----------------------------------------------

BEGIN
  Remove (n(*=>*));
END RemoveExpr;


(*=========================================================================*)

PROCEDURE ^ GenExpr ( n: pc.NODE
                    ; t: pc.STRUCT);
-----------------------------------------------------------------------------

PROCEDURE GetBase ( t : pc.STRUCT
                  ; n : pc.NODE               -- indexation level
                  )   :           pc.STRUCT;
VAR i: LONGINT;
BEGIN
  GenExpr (n, NIL);

  IF n.mode # pc.nd_value (*3/n.val = NIL*) THEN RETURN NIL END;

  i := n.val.get_integer();
  IF (i < 0)
  OR (i >= pc.code.max_dim)
  THEN
    Err (n.pos, 61);           -- dimension too large or neg
    RETURN NIL;
  END;

  WHILE (i > 0) & (t # NIL) & (t.mode IN pc.ARRs) DO
    DEC (i);
    t := t.base;
  END;

  IF  (i > 0)
  OR  (t = NIL)
  OR ~(t.mode IN pc.ARRs)
  THEN
    Err (n.pos, 50);  -- object is not array
    RETURN NIL;
  END;
  RETURN t;
END GetBase;


(*==================== MASKED SET OPERATIONS FOR DFA =======================*)


--------------------------------------------------------------------------------
-- Clear bits given by 'mask' in 'to'. Other bits not affected.
PROCEDURE BitClr ( VAR to: SET
                 ;   mask: SET );
BEGIN
  to := to - mask;
END BitClr;

--------------------------------------------------------------------------------
-- Set bits given by 'mask' in 'to'. Other bits not affected.
PROCEDURE BitSet ( VAR to: SET
                 ;   mask: SET );
BEGIN
  to := to + mask;
END BitSet;

--------------------------------------------------------------------------------
-- Incremental join (bitwise OR-ing) bit sets with respect to 'mask'.
-- Other bits not affected.
PROCEDURE BitJoin ( VAR to: SET
                  ;     fr: SET
                  ;   mask: SET );
BEGIN
  to := to + (fr * mask);
END BitJoin;

---------------------------------------------------------------------------------
-- Copy 'mask'ed bits of 'fr' to to'. Other bits not affected.
PROCEDURE BitCopy ( VAR to: SET
                  ;     fr: SET
                  ;   mask: SET );
BEGIN
  to := (to - mask) + (fr * mask);
END BitCopy;

--------------------------------------------------------------------------------
-- Check if any of 'mask' bits is set in 'fr'.
PROCEDURE BitChk (   fr: SET
                 ; mask: SET
                 )     :     BOOLEAN;
BEGIN
  RETURN fr * mask # {};
END BitChk;


(*============= VALUES MAPS SUPPORT FOR DATA FLOW ANALYSIS (DFA) ==============

  Value maps match var/varpar's list of current scope (proc/module).
  When performing DFA, they keep track of vars' constant values known for
  current program location (in terms of IR nodes) - to perform constants
  propagation, and also some boolean properties (tags) - to do NIL check, etc.
  Enlisted objects have .sno#0 - value map index.
*)

CONST           -- DFA properties tags (join operation is OR, or set +)
  v_nav = 0;    -- value not assigned yet (maybe, on some path)
  v_nil = 1;    -- check for NIL required (does not track actual NIL value,
                --   just reset along a path where check has been done already)
  v_wr  = 2;    -- DFA properties are changed

TYPE
  VALUES = POINTER TO ARRAY OF
    RECORD
      tags: SET;
      val : pc.VALUE;  -- keeps known single constant value; no, if NIL
      cnt : LONGINT;   -- statement id where set was changed
    END;

VAR
  CurProps: VALUES;    -- cur. properties map for var/varpars of cur.scope
  StatCnt : LONGINT;   -- statements & etc. counter for current proc/module body
  StatCnt1: LONGINT;   -- statements counter for current proc/module body

-------------------------------------------------------------------------
-- Is there a value for object 'o' in values map?
PROCEDURE IsInVals (o: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (CurProps # NIL)             -- non-empty map
       & (o.sno >= 0)                 -- 'o' has been enlisted into map
       & ((o.mode = pc.ob_var) OR     -- 'o' is proper var/varpar
          (o.mode = pc.ob_varpar))
       & (o.host = CurProc)           -- 'o' is in current scope
END IsInVals;

-------------------------------------------------------------------------

PROCEDURE UndefValue (o: pc.OBJECT);
(**
   Undefine var 'o' DFA settings in current values map.
   Envokes when entering (and, smtms, exiting) object scope.
   If object is ob_var, its value is assumed undefined.
*)
BEGIN
  IF ~IsInVals (o) THEN       -- 'o' is not a proper var
    RETURN
  END;
  IF o.mode = pc.ob_var
    THEN CurProps[o.sno].tags := {v_nav, v_nil, v_wr};
    ELSE CurProps[o.sno].tags :=       {v_nil, v_wr};
  END;
  CurProps[o.sno].val := NIL;              -- no known value
  CurProps[o.sno].cnt := StatCnt;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (0, "UndefValue: o=%X '%s'", o, o.name^) END;
<* END *>
END UndefValue;

--------------------------------------------------------------------------------
-- Store var 'o' value 'v' in values map.
-- Envokes in value definition nodes.
PROCEDURE DefValue ( o: pc.OBJECT             -- object to set value
                   ; v: pc.VALUE );           -- v=NIL means "any value"
BEGIN
  IF ~IsInVals (o) THEN                       -- 'o' is not a proper var
    RETURN
  END;
  IF ~(pc.otag_no_aliases IN o.tags) THEN     -- 'o' is smb's alias:
    v := NIL;                                 --   assume no known value
  END;
  IF v = NIL
    THEN CurProps[o.sno].tags := {v_wr, v_nil}-- v_nav dropped:
    ELSE CurProps[o.sno].tags := {v_wr};      --   some value is set alrdy
  END;
  CurProps[o.sno].val := v;
  CurProps[o.sno].cnt := StatCnt;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN
    dbg.CallTraceInd (0, "DefValue: o=%X '%s' = %X", o, o.name^, v)
  END;
<* END *>
END DefValue;


--------------------------------------------------------------------------------
-- Check if object's 'o' value is possibly undefined at usage position.
-- Envokes when using object's value.
PROCEDURE ChkValue ( p: pc.TPOS
                   ; o: pc.OBJECT );
BEGIN
  IF ~IsInVals (o) THEN                      -- not a proper var
    RETURN
  END;
  IF v_nav IN CurProps[o.sno].tags THEN      -- if value is N/Av here:
    EXCL (CurProps[o.sno].tags, v_nav);      -- suppress further checks
    IF NOT                                   -- give warning if not:
    (   (pc.omark_code IN CurProc.obj.marks) -- translating code proc
     OR (o.type.mode IN pc.COMPOUNDs)        -- compound type
     OR Restart                              -- reiteration on inlining
     OR p.IsNull ()                          -- usage position is given
(*   OR
     ( -- O2 pointer == NIL
       (o.type.mode = pc.ty_pointer) AND
       (o.flag      = env.flag_o2  ) AND
       (at.INIT_PTR IN at.COMP_MODE)
     )
*)
    ) THEN
      env.errors.Warning (p, 304, o.name^);  -- "possible use before definition"
                                             -- usage pos. here: cant use WrnObj!
  <* IF TARGET_IDB THEN *>
      IF env.InterViewMode THEN model2.warn_at_pos(p); END;
  <* END *>
    END;
  END;
END ChkValue;

-------------------------------------------------------------------------

PROCEDURE GetValue ( p: pc.TPOS              -- usage position
                   ; o: pc.OBJECT            -- object to get value
<* IF TARGET_IDB THEN *>
                   ; n : pc.NODE             -- node that will be mapped to database
<* END *>
                   )  :           pc.VALUE;  -- value or NIL
(**
   Get 'o's current constant value, if known, otherwise NIL.
   Warning is reported, if so.
*)
BEGIN
  IF Restart                                       -- no value in restart mode
  OR ~IsInVals (o)                                 -- not a proper var
  OR BitChk (CurProps[o.sno].tags, {v_nav, v_nil}) -- value not available
  OR (CurProps[o.sno].val = NIL)                   -- value is not known
  OR (CurProps[o.sno].cnt = StatCnt)               -- no value in def node
  THEN
    RETURN NIL;
  END;
  IF ~insideInline AND ~p.IsNull () THEN
    env.errors.Warning (p, 314, o.name^); -- "'o' has compile time defined value"
<* IF TARGET_IDB THEN *>
    IF env.InterViewMode THEN model2.warn_at_pos(p); END;
<* END *>
  END;                                    -- usage pos. here, cant use WrnObj!

<* IF DB_TRACE THEN *>
  IF EnDebug THEN
    dbg.CallTraceInd (0, "GetValue: o=%X '%s' = %X", o, o.name^, CurProps[o.sno].val)
  END;
<* END *>
  RETURN CurProps[o.sno].val;
END GetValue;

-------------------------------------------------------------------------

PROCEDURE ValsChkNil ( o: pc.OBJECT           -- pointer object to check
                     )  :           BOOLEAN;  -- T: NIL check required now
(**
  Check if NIL check for 'o' is required in current postition.
  Enovokes to check if a NIL-chk must be done in THIS node.
*)
BEGIN
  RETURN Restart                              -- code mb. reused when inlining
      OR ~IsInVals (o)                        -- not a proper var (won't matter)
      OR BitChk (CurProps[o.sno].tags, {v_nil, v_nav}) -- NILchk or N/Av tagged
      OR (CurProps[o.sno].cnt = StatCnt);     -- 'o' is assigned in this node
END ValsChkNil;

-------------------------------------------------------------------------

PROCEDURE ValsClrNil (o: pc.OBJECT);          -- pointer object
(**
   Initiate check-free path for 'o' Estarting NEXT to current node.
   Envokes when actual NIL check will be done for THIS node.
*)
BEGIN
  IF ~IsInVals (o) THEN RETURN END;          -- if proper var (won't matter)

  EXCL (CurProps[o.sno].tags, v_nil);      -- drop NIL chk tag
  INCL (CurProps[o.sno].tags, v_wr);       -- adjust property modification
  CurProps[o.sno].cnt := StatCnt;          --   position
END ValsClrNil;

-------------------------------------------------------------------------

PROCEDURE ValsAssign (     fr: VALUES
                     ; VAR to: VALUES );
(**
   Copy values map 'fr' into 'to'.
*)
VAR i: LONGINT;

BEGIN
  IF fr = NIL THEN RETURN END;
  IF to = NIL THEN
    NEW (to, LEN (fr^));
  END;

  ASSERT( LEN(fr^) = LEN(to^));
  FOR i := 0 TO LEN(fr^) - 1 DO
    to[i] := fr[i];
  END;
END ValsAssign;

-------------------------------------------------------------------------

PROCEDURE ValsCopy (     fr: VALUES
                   ; VAR to: VALUES );
(**
   Copy values map 'fr' to newly created 'to'.
*)
BEGIN
  IF fr = NIL THEN
    to := NIL;
  ELSE
    NEW (to, LEN (fr^));
    ValsAssign (fr, to);
  END;
END ValsCopy;

-------------------------------------------------------------------------

PROCEDURE ValsEqu ( x, y: VALUES
                  )    :        BOOLEAN;
(*
*)
VAR i: LONGINT;

BEGIN
  IF x = NIL THEN RETURN TRUE END;

  FOR i := 0 TO LEN (x^)-1 DO
    IF (x[i].tags # y[i].tags)
    OR((x[i].val = NIL) # (y[i].val = NIL))
    OR (x[i].val # NIL)
     & CmpValue (pc.sb_neq, x[i].val, y[i].val)
    THEN
      RETURN FALSE
    END
  END;
  RETURN TRUE;
END ValsEqu;

-------------------------------------------------------------------------

PROCEDURE ValsAppend ( fr, to: VALUES );
(**
   Join values maps to := fr & to.
   Envokes to accumulate data on merging branches in compound stmts.
*)

VAR i: LONGINT;

BEGIN
  IF fr = NIL THEN RETURN END;

  FOR i := 0 TO LEN (fr^)-1 DO
    BitJoin (to[i].tags,                -- OR-join fr & to branches
             fr[i].tags,
             {v_nav, v_nil, v_wr});
    IF v_wr IN fr[i].tags THEN
      to[i].cnt := fr[i].cnt;
    END;
    IF (fr[i].val = NIL)                -- join values
    OR (to[i].val = NIL)
    OR ~CmpValue (pc.sb_equ, fr[i].val, to[i].val)
    THEN
      to[i].val := NIL;
    END;
  END;
END ValsAppend;

-------------------------------------------------------------------------

PROCEDURE ValsReplace ( fr, to: VALUES );
(**
   Join values maps to := fr & to - for each var with modified properties.
   Replace values map to := fr - for each unmodified var.
*)

VAR i: LONGINT;

BEGIN
  IF fr = NIL THEN RETURN END;
  ASSERT( to # NIL );                      -- must =fr or be created before

  FOR i := 0 TO LEN (fr^)-1 DO
    IF v_wr IN to[i].tags THEN
      BitJoin (to[i].tags,
               fr[i].tags,
               {v_nav, v_nil});
      IF  (fr[i].val = NIL)                -- join values
      OR  (to[i].val = NIL)
      OR ~CmpValue (pc.sb_equ, fr[i].val, to[i].val)
      THEN
        to[i].val := NIL;
      END;
    ELSE
      ASSERT( (to[i].val = NIL) OR (to[i].val = fr[i].val) );
      BitCopy (to[i].tags,
               fr[i].tags,
               {v_nav, v_nil} );
      to[i].val := fr[i].val;
    END;
  END;
END ValsReplace;

-------------------------------------------------------------------------

PROCEDURE ValsAppUsage ( fr, to: VALUES );
(**
   Replace {v_wr, cnt} properties in 'to' from 'fr' for each modified var.
*)

VAR i: LONGINT;

BEGIN
  IF fr = NIL THEN
    RETURN
  END;
  FOR i := 0 TO LEN (fr^)-1 DO
    IF v_wr IN fr[i].tags THEN
      INCL (to[i].tags, v_wr);
      to[i].cnt := fr[i].cnt;
    END
  END
END ValsAppUsage;

-------------------------------------------------------------------------

PROCEDURE ValsWorst ( v: VALUES );
(**
   Set worst assumptions (lattice 0) for all vars: {v_nil, val}
*)

VAR i: LONGINT;

BEGIN
  IF v = NIL THEN
    RETURN
  END;
  FOR i := 0 TO LEN (v^)-1 DO
    INCL (v[i].tags, v_nil);
    v[i].val := NIL;
  END;
END ValsWorst;

-------------------------------------------------------------------------

PROCEDURE ValsWorstClrU ( v: VALUES );
(**
   Set worst assumptions (lattice 0) for all vars: {v_nil, val}.
   Also clear write info.
*)

VAR i: LONGINT;

BEGIN
  IF v = NIL THEN
    RETURN
  END;
  FOR i := 0 TO LEN (v^)-1 DO
    INCL (v[i].tags, v_nil);
    EXCL (v[i].tags, v_wr);
    v[i].val := NIL;
    v[i].cnt := -1;
  END;
END ValsWorstClrU;

-------------------------------------------------------------------------

PROCEDURE ValsWorstSetU ( v: VALUES );
(**
   Set worst assumptions (lattice 0) for all vars: {v_nil, val}.
   Also set write info as modified in cur.node (StatCnt).
*)

VAR i: LONGINT;

BEGIN
  IF v = NIL THEN
    RETURN
  END;
  FOR i := 0 TO LEN (v^) - 1 DO
    BitSet (v[i].tags, {v_nil, v_wr});
    v[i].val := NIL;
    v[i].cnt := StatCnt;
  END;
END ValsWorstSetU;

-------------------------------------------------------------------------

(* --- Uncomment for debugging: print current values set ---
PROCEDURE PrintVals ( msg-: ARRAY OF CHAR
                    ;   vl: VALUES
                    );
VAR i, ln: LONGINT;
     val: pc.VALUE;
       v: pc.OBJECT;
    prof: BOOLEAN;

BEGIN
  IF vl = NIL THEN
    dbg.Trace("---- PrintVals('%s') = NIL -------------\n", msg);
  ELSE
    ln := LEN(vl^);
    dbg.Trace("---- PrintVals('%s') = %X; len = %d ------------\n", msg, vl, ln);
    i := 0;
    WHILE i < ln DO
      v := CurProc.prof; prof := TRUE;
      LOOP
        IF v = NIL THEN
          IF prof THEN prof := FALSE; v := CurProc.mem;
          ELSE EXIT
          END;
        ELSE
          IF v.sno = i THEN
            dbg.Trace("'%s'", v.name^);
          END;
          v := v.next
        END
      END;
      val := vl[i].val;
      dbg.Trace("  nav = %d; zero = %d; any = %d; wr = %d; val = %X; cnt  = %d ",
                  v_nav IN vl[i].tags,
                  v_nil IN vl[i].tags,
                  v_wr  IN vl[i].tags,
                  val, vl[i].cnt);
      IF (val # NIL) & val.is_ZZ() THEN
        dbg.Trace(" VALUE = %d", val.get_integer());
      END;
      dbg.Trace("\n");
      INC(i);
    END;
    dbg.Trace("---- end PrintVals ------------\n");
  END;
END PrintVals;
--- *)


(*===========================================================================*)

PROCEDURE ^ GenNextProc (p: pc.OBJECT);


--------------------------------------------------------------------------------
PROCEDURE ObjectUsage (    p: pc.TPOS      -- object usage position
                      ;    o: pc.OBJECT    -- object
                      ; tags: pc.UTAG_SET          -- usage tags
                      ;  val: pc.VALUE );  -- const.value of 'o' or NIL
(*
*)
  -----------------------------------------------

  PROCEDURE Use;
  (**
     Add object to .use list of CurProc
  *)
  VAR u: pc.USAGE;
  BEGIN
    IF o.type = CurProc THEN
      RETURN
    END;
    INCL (o.marks, omark_used);
    NEW (u);
    u.obj  := o;
    u.tags := tags;
    u.next := CurProc.use;
    CurProc.use := u;
  END Use;

  -------------------------------

  PROCEDURE ChkAlloc;
    VAR h: pc.STRUCT;
  BEGIN
    h := CurProc;
    WHILE (h # NIL) & (h # o.host) DO
      h := h.obj.host;
    END;
    ASSERT( h # NIL);
    ASSERT( h.mode = pc.ty_proctype);
    ASSERT( h.obj.lev+1 = o.lev);
  END ChkAlloc;

  -------------------------------
<* IF TARGET_MEGOS THEN *> (* FS 26.2 *)

  -- Check if 'nm' match 'pt' to 'full' length
  PROCEDURE CmpNames (  nm-: ARRAY OF CHAR
                     ;  pt-: ARRAY OF CHAR
                     ; full: BOOLEAN
                     )     :          BOOLEAN;
  VAR i: INTEGER;
  BEGIN
    i := 0;
    LOOP
      IF pt[i] = 0X THEN RETURN ~full OR (nm[i] = 0X) END;
      IF nm[i] # pt[i] THEN RETURN FALSE END;
      INC(i);
    END;
  END CmpNames;

  PROCEDURE CopyStruct (to, fr: pc.STRUCT);
  (**
     Copy type structure
  *)
  BEGIN
    to.mode := fr.mode;
    to.mno  := fr.mno;
    to.flag := fr.flag;
    to.align:= fr.align;
    to.tags := fr.tags;
    to.marks:= fr.marks;
    to.base := fr.base;
    to.inx  := fr.inx;
    to.min  := fr.min;
    to.max  := fr.max;
    to.pos  := fr.pos;
    to.end  := fr.end;
    to.len  := fr.len;
    to.ext  := fr.ext;
  END CopyStruct;
<* END *>

VAR u: pc.USAGE;

BEGIN
  IF ~EnUsageChk THEN
    RETURN
  END;

  ASSERT( o.mode IN  pc.OB_Common );

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (0, "ObjectUsage: o=%X '%s' v=%X", o, o.name^, val) END;
<* END *>

  IF (o.type.obj # NIL) &                    -- @22: type name processing
     (o.type.obj # o) THEN
    ObjectUsage (p, o.type.obj, pc.UTAG_SET{}, NIL);
  END;

<* IF TARGET_MEGOS THEN *> (* FS 26 - string types convert to ty_SS *)
IF env.config.Option("GENDYNSTR") THEN
  IF (o.mode = pc.ob_type) THEN
    IF (o.type.mode = pc.ty_array)           -- "SYString#": ARRAY n OF CHAR
     & (o.type.base.mode = pc.ty_char)
     & CmpNames (o.name^,"SYString", FALSE)
    THEN
      o.type.mode := pc.ty_SS;
    ELSIF (o.type.mode = pc.ty_pointer)      -- "SYDynStr": PTR TO ARRAY n OF CHAR
        & (o.type.base.mode = pc.ty_array)
        & (o.type.base.base.mode = pc.ty_char)
        &  CmpNames (o.name^, "SYDynStr", TRUE)
    THEN
      CopyStruct (o.type, (*<-*) o.type.base);
      o.type.mode := pc.ty_SS;
      INCL (o.type.tags, pc.ttag_packed);    -- mark dynamic string
    ELSIF (o.type.mode = pc.ty_array_of)
        & (o.type.base.mode = pc.ty_char)    -- ARRAY OF CHAR(unnamed)
        & (o.type.base.obj = NIL)
    THEN
      o.type.mode := pc.ty_SS;
      INCL (o.type.tags, pc.ttag_packed);    -- mark dynamic string
    END;
  END;
END;
<* END *>

  IF o.mno = CurMod.mno THEN
    INCL (o.marks, omark_keep);
  END;
  IF o.mode IN pc.PROCs THEN
    GenNextProc (o);
  END;
  IF CurProc = NIL THEN
    RETURN
  END;

  IF pc.utag_read  IN tags THEN      -- read node:
    ChkValue (p, o);                 --   check undefinite value
  END;
  IF pc.utag_write IN tags THEN      -- write node:
    DefValue (o, val);               --   store value in CurProps
  END;

  IF omark_used IN o.marks THEN
    u := CurProc.use;
    WHILE u.obj # o DO
      u := u.next
    END;
    IF BitChk (SYSTEM.VAL(SET, u.tags), SYSTEM.VAL(SET, U_WR)) THEN
--      BitClr (SYSTEM.VAL(SET, tags), SYSTEM.VAL(SET, U_RD))
      tags := tags - U_RD
    END;
--    BitSet (SYSTEM.VAL(SET, u.tags), SYSTEM.VAL(SET, tags));
    u.tags := u.tags + tags;
  ELSIF o.mode IN pc.PROCs THEN
    IF (o.lev > 0) & (CurLevel > 1) THEN
      Use;
    END;
  ELSIF o.mode IN pc.VARs THEN
    IF (o.lev > 0) & (o.lev < CurLevel) THEN
      EXCL (o.tags, pc.otag_no_threat);
      Use;
      ChkAlloc ();
    END;
  END;
END ObjectUsage;


--------------------------------------------------------------------------------
PROCEDURE ValueUsage ( v: pc.VALUE
                     ; t: pc.STRUCT );

  -- 1 -- ValueUsage -----------------------------------------------------------
  PROCEDURE Field (      f: pc.OBJECT
                  ; VAR no: LONGINT );
  VAR w: pc.VALUE;
  BEGIN
    IF f.type.mode IN pc.TY_SET{pc.ty_proctype, pc.ty_array, pc.ty_record} THEN
      w := pc.value.new (v.pos, f.type);
      w.index_get (no, v);
      ValueUsage (w, f.type);
    END;
    INC (no);
  END Field;

  ------------------------------------------------------
  PROCEDURE SearchVariant ( VAR l: pc.NODE
                          ;     v: pc.VALUE );

  VAR e,m: pc.NODE;
  BEGIN
    e := NIL;
    LOOP
      IF l = NIL THEN
        EXIT
      END;
      ASSERT( l.mode = pc.nd_node );
      m := l.l;
      IF m = NIL THEN
        e := l
      END;
      LOOP
        IF m = NIL THEN EXIT END;
        IF m.mode = pc.nd_pair THEN
          IF CmpValue (pc.sb_geq, v, m(*3.l*).val)
           & CmpValue (pc.sb_leq, v, m.l(*3/.r*).val)
          THEN
             EXIT
          END;
        ELSE
          ASSERT( m.mode = pc.nd_value (*3/m.val # NIL*) );
          IF CmpValue (pc.sb_equ, v, m.val) THEN EXIT END;
        END;
        m := m.next;
      END;
      IF m # NIL THEN EXIT END;
      l := l.next;
    END;
    IF l = NIL THEN
      l := e
    END;
  END SearchVariant;

  ------------------------------------------------

  PROCEDURE Flist (      f: pc.OBJECT
                  ; VAR no: LONGINT);
  (*
  *)
    VAR n: pc.NODE;
        w: pc.VALUE;
  BEGIN
    WHILE f # NIL DO
      IF f.mode = pc.ob_field THEN
        Field (f, no);
      ELSIF f.mode = pc.ob_header THEN
        w := pc.value.new (v.pos, f.type);
        w.index_get (no, v);
        IF f.val.obj # NIL
          THEN Field (f.val.obj, no)
          ELSE INC (no)
        END;
        n := f.val.l;
        SearchVariant (n, w);
        Flist (n.obj, no);
      ELSE
        ASSERT( FALSE );
      END;
      f := f.next;
    END;
  END Flist;

  -------------------------------------------------

  PROCEDURE Record (     tt: pc.STRUCT
                   ; VAR no: LONGINT);
  (*
  *)
  BEGIN
    ASSERT( tt.mode = pc.ty_record);
    IF tt.base # NIL THEN
      Record (tt.base, no)
    END;
    Flist (tt.prof, no);
  END Record;

  -----------------------------------------------

VAR i: LONGINT;
    w: pc.VALUE;
    o: pc.OBJECT;

BEGIN
  CASE t.mode OF
    | pc.ty_proctype:
       o := v.get_object ();
       IF o # NIL THEN
         ObjectUsage (v.pos, o, U_RD, NIL);
       END;

    | pc.ty_array:
       IF t.base.mode IN pc.TY_SET{pc.ty_proctype, pc.ty_array, pc.ty_record} THEN
         w := pc.value.new (v.pos, t.base);
         FOR i := 0 TO t.len-1 DO
           w.index_get (i, v);
           ValueUsage (w, t.base);
         END;
       END;

    | pc.ty_record:
       i := 0;
       Record (t, i);
  ELSE
  END;
END ValueUsage;

-------------------------------------------------------------------------

(* try to sort expressions like
   const (OP) expr (OP) const ... *)
VAR
  trySortTreeLock : INTEGER;
PROCEDURE trySortTree(n : pc.NODE) : BOOLEAN;
VAR
  arr     : ARRAY 128 OF pc.NODE;
  arrF    : ARRAY 128 OF BOOLEAN;
  sub     : pc.SUB_MODE;
  total,i : INTEGER;
  consts  : INTEGER;
  b,bb    : BOOLEAN;
  nn      : pc.NODE;
  ----
  PROCEDURE is_val (nn: pc.NODE):BOOLEAN;
  BEGIN
    WHILE (nn # NIL) AND
          ( ((nn.mode = pc.nd_unary) AND ((nn.sub = pc.su_cast) OR (nn.sub = pc.su_conv))) OR
            (nn.mode = pc.nd_guard) OR
            (nn.mode = pc.nd_aggregate)
          )
    DO
      nn := nn.l;
    END;
    RETURN (nn # NIL) AND (nn.mode = pc.nd_value);
  END is_val;
  ----
  PROCEDURE app(n : pc.NODE);
  BEGIN
    IF (n.mode = pc.nd_binary) AND (n.sub = sub) THEN
      app(n.l);
      app(n.r);
    ELSE
      arr [total] := n;
      IF is_val(n) THEN
        INC(consts);
        arrF[total] := TRUE;
      ELSE
        arrF[total] := FALSE;
      END;
      INC(total);
    END;
  END app;
  ----
BEGIN ---- trySortTree()
  sub    := n.sub;
  total  := 0;
  consts := 0;
  app(n);
  IF (consts < 2) OR (total < 3) OR (total = consts) THEN
    RETURN FALSE;
  END;
  bb := FALSE;
  REPEAT;
    b := FALSE;
    FOR i:=0 TO total-2 DO
      IF (arrF[i] = TRUE) AND (arrF[i+1] = FALSE) THEN
        nn       := arr[i];
        arr[i]   := arr[i+1];
        arr[i+1] := nn;
        arrF[i]  := FALSE;
        arrF[i+1]:= TRUE;
        b  := TRUE;
        bb := TRUE;
      END;
    END;
  UNTIL ~b;
  IF (~bb) THEN RETURN FALSE; END;
  n.l := arr[0];
  FOR i:=1 TO total-2 DO
    nn     := CopyNode(n);
    nn.pos := n.pos;
    nn.l   := arr[i];
    n.r    := nn;
    n      := nn;
  END;
  nn.r := arr[total-1];
  RETURN TRUE;
END trySortTree;


--------------------------------------------------------------------------------
PROCEDURE ^ GenDesignator (      n: pc.NODE
                          ;   addr: BOOLEAN
                          ; u_tags: pc.UTAG_SET );


--------------------------------------------------------------------------------
-- Traverse binary expression
PROCEDURE GenExprBinary ( n: pc.NODE );  -- nd_binary
VAR   i: LONGINT;
    v,w: pc.VALUE;
      t: pc.STRUCT;
      l: pc.NODE;
   vals: VALUES;
BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN
    dbg.CallTraceInd (1, "(GenExprBinary: n=%X .sub=", n); dbg.VisNodSubMode (n);
  END;
<* END *>

  ASSERT( n.mode = pc.nd_binary);
  ASSERT( n.type.mode # pc.ty_void);

  CASE n.sub OF
  | pc.sb_high:         (* HIGH(n.l=designator); n.r=indexation level *)
      GenDesignator (n.l, FALSE, U_SZ);
      t := GetBase (n.l.type, n.r);              -- does GenExpr of n.r
      IF (t # NIL) & (t.mode IN pc.TY_SET{pc.ty_array, pc.ty_SS}) THEN
        SetValueCast (n, n.l.type.max(*3 .val*));
      END;

  | pc.sb_len:          (* LENGTH(n.l=designator); n.r=??? *)
      GenDesignator (n.l, FALSE, U_SZ);
      t := GetBase (n.l.type, n.r);              -- does GenExpr of n.r
      IF (t # NIL) & (t.mode IN pc.TY_SET{pc.ty_array, pc.ty_SS}) THEN
        EvalVal (n, t.len);
      END;

  | pc.sb_equ
  , pc.sb_neq:
      ASSERT( n.type.mode IN ZZs );

      GenExpr (n.l, NIL);
      GenExpr (n.r, NIL);

      -- reusability check:
      IF n.l.type.mode IN pc.ARRs THEN  -- strings
        IF MakeReusable (n(*=>*), n.l, ru_binary, FALSE)      (* IRMOD *)
        OR MakeReusable (n(*=>*), n.r, ru_binary, FALSE)
        THEN
          GenExpr (n, NIL);       -- re-parse modified node
       <* IF DB_TRACE THEN *>
          IF EnDebug THEN dbg.CallTraceInd (-1, ")GenExprBinary - reuse") END;
       <* END *>
          RETURN;
        END;
      ELSE
        ASSERT(   (n.l.type.mode = n.r.type.mode)      -- types must be of same mode
               OR (n.l.type.mode = pc.ty_proctype)     -- but ty_proc may compare
               OR (n.r.type.mode = pc.ty_proctype) );  -- to ty_AA (NIL)
      END;

      -- values calculation and optimizations:
      IF EvalBin (n) THEN                       -- propagate, if const args

      ELSIF (n.l.mode = pc.nd_proc)
          & (n.r.mode = pc.nd_proc)             -- procedures are eq
      THEN
        i := ORD (n.l.obj=n.r.obj);             -- if same
        IF n.sub = pc.sb_neq THEN
          i := 1-i
        END;
        EvalVal (n, i);

      ELSIF EnConsProp
          & (n.l.type.mode = pc.ty_boolean)     -- booleans comp:
      THEN
        IF n.r.mode = pc.nd_value (*3/n.r.val # NIL*) THEN
          IF n.r.val.is_zero() = (n.sub = pc.sb_neq) THEN
            IF (n.type = n.l.type) THEN
              n.copy (n.l);                     -- x=T, x#F -> x  (* IRMOD *)
            ELSE
              n.mode := pc.nd_unary;            -- --//--   -> BOOLEAN(x) (* IRMOD *)
              n.sub  := pc.su_conv;
              n.r    := NIL;
            END;
          ELSE
            n.mode := pc.nd_unary;              -- x#T, x=F -> ~x (* IRMOD *)
            n.sub  := pc.su_not;
            n.r    := NIL;
          END
        ELSIF n.l.mode = pc.nd_value (*3/n.l.val # NIL*) THEN  (*@17*)
          IF n.l.val.is_zero () = (n.sub = pc.sb_neq) THEN
            IF (n.type = n.r.type) THEN
              n.copy (n.r);                     -- T=y, F#y -> y  (* IRMOD *)
            ELSE
              n.mode := pc.nd_unary;            -- --//--   -> BOOLEAN(y) (* IRMOD *)
              n.sub  := pc.su_conv;
              n.l    := n.r;
              n.r    := NIL;
            END;
          ELSE
            n.mode := pc.nd_unary;              -- T#y, F=y -> ~y (* IRMOD *)
            n.sub  := pc.su_not;
            n.l    := n.r;
            n.r    := NIL;
          END
        END
      ELSIF (n.l.mode = pc.nd_unary) & (n.l.sub = pc.su_conv) -- 2 convs to
          & (n.r.mode = pc.nd_unary) & (n.r.sub = pc.su_conv) -- same ordinal
          & (n.l.l.type = n.r.l.type)                         -- type:
          & n.l.l.type.is_ordinal() & n.l.type.is_ordinal()
      THEN
        n.l.copy (n.l.l);                       -- drop conversions (* IRMOD! *)
        n.r.copy (n.r.l);
      END;

  | pc.sb_lss
  , pc.sb_leq
  , pc.sb_gtr
  , pc.sb_geq:
      ASSERT( n.l.type.mode IN (NUMs+pc.SETs+pc.ARRs+pc.TY_SET{pc.ty_protection}));

      GenExpr (n.l, NIL);
      GenExpr (n.r, NIL);

      -- reusability check:
      IF n.l.type.mode IN pc.ARRs THEN -- strings         (* IRMOD *)
        IF MakeReusable (n(*=>*), n.l, ru_binary, FALSE)
        OR MakeReusable (n(*=>*), n.r, ru_binary, FALSE)
        THEN
          GenExpr (n, NIL);       -- re-parse modified node
       <* IF DB_TRACE THEN *>
          IF EnDebug THEN dbg.CallTraceInd (-1, ")GenExprBinary - reuse") END;
       <* END *>
          RETURN;
        END;
      ELSE
        ASSERT( n.r.type.mode = n.l.type.mode );  -- type mode must match
      END;

      SYSTEM.EVAL(EvalBin (n));            -- propagate values, if const

  | pc.sb_in:
      ASSERT( n.type.mode IN ZZs );
      ASSERT( n.l.type.mode IN ZZs );
      ASSERT( n.r.type.mode IN pc.SETs );

      GenExpr (n.l, NIL);
      GenExpr (n.r, NIL);

      -- values calculation and optimizations:
      IF n.l.mode # pc.nd_value (*3/ n.l.val = NIL*) THEN
      ELSIF n.r.mode = pc.nd_value (*3/n.r.val # NIL*) THEN             -- set, num are const:
        vIntTmp.binary (pc.sb_minus, n.l.val, n.r.type.min(*3/.val*));
        v := pc.value.new(n.pos, n.type);
  (*3/  NewValue (n); *)
        v (*3/n.val*).binary (n.sub, vIntTmp, n.r.val);
        SetValue (n, v ); (*3/ *)
      ELSIF ~EnConsProp THEN
      ELSIF  CmpValue (pc.sb_lss, n.l.val, n.r.type.min(*3/.val*)) -- value out of
         OR  CmpValue (pc.sb_gtr, n.l.val, n.r.type.max(*3/.val*)) -- set range
      THEN                                 -- set value FALSE (* IRMOD *)
        n.mode := pc.nd_replace;           -- replace(set, num)
        n.sub  := pc.su_none;
        l      := n.l;
        n.l    := n.r;
        n.r    := l;
        l.type := n.type;                  -- num.val=FALSE, num.type=ty_bool
        l.val  := vIntZero;
        RemoveExpr (n.l(*=>*));            -- eliminate set
        IF n.l = NIL THEN n.copy (n.r) END;-- if noth left - repl.by num
      END;

  | pc.sb_div
  , pc.sb_mod
  , pc.sb_rem:
      ASSERT( n.type.mode IN ZZs);
      ASSERT( n.l.type.mode = n.type.mode );
      ASSERT( n.r.type.mode = n.type.mode );

      GenExpr (n.l, NIL);
      GenExpr (n.r, NIL);

      -- values calculation and optimizations:
      IF n.r.mode # pc.nd_value (*3/n.r.val = NIL*) THEN
      ELSIF (   (n.sub = pc.sb_div)
             OR (n.sub = pc.sb_mod))
          & n.r.val.is_neg ()
      THEN Err (n.pos, 107);          -- "shall not have a negative value"
      ELSIF n.r.val.is_zero () THEN
        Err (n.pos, 203);             -- "division by zero"
      ELSIF EvalBin (n) THEN          -- propagate values, if const
      ELSIF ~EnConsProp THEN
      ELSIF CmpValue (pc.sb_equ, vIntOne, n.r.val) THEN
        IF n.sub = pc.sb_div
          THEN n.copy (n.l);          -- x DIV 1 -> x  (* IRMOD *)
          ELSE SetValue (n, vIntZero);-- x MOD 1 -> 0  (* IRMOD *)
        END;
      ELSIF (n.sub = pc.sb_mod) & Power2 (n.r, v(*=>*)) THEN
        n.sub := pc.sb_and;           -- MOD 2^m = AND 2^m-1  (* IRMOD *)
        SetValue (n.r, v);
        v.expr:=NIL; (*3/ *)
        IF pc.ntag_chk_range IN n.r.tags THEN
          CheckValueRange (n.r);      -- @19
        END;
      END;

  | pc.sb_slash:
      ASSERT( n.type.mode IN CNUMs );
      ASSERT( n.l.type.mode = n.type.mode );
      ASSERT( n.r.type.mode = n.type.mode );

      GenExpr (n.l, NIL);
      GenExpr (n.r, NIL);

      -- values calculation and optimizations:
      IF n.r.mode # pc.nd_value (*3/n.r.val = NIL*) THEN
      ELSIF n.r.val.is_zero () THEN
        Err (n.pos, 203);             -- "division by zero"
      ELSIF EvalBin (n) THEN          -- propagate values, if const
      ELSIF ~EnConsProp THEN
      ELSIF (n.type.mode IN ZZs)
          & CmpValue (pc.sb_equ, n.r.val, vIntOne)
      THEN
        n.copy (n.l);                 -- x / 1 -> x   (* IRMOD *)
      END;

  | pc.sb_plus
  , pc.sb_minus
  , pc.sb_mul:
      ASSERT( n.type.mode IN CNUMs );
      ASSERT( n.l.type.mode = n.type.mode );
      ASSERT( n.r.type.mode = n.type.mode );

      GenExpr (n.l, NIL);
      GenExpr (n.r, NIL);

      SYSTEM.EVAL(EvalBin (n));               -- propagate values, if const

  | pc.sb_and
  , pc.sb_or
  , pc.sb_xor
  , pc.sb_bic:
      ASSERT( n.type.mode IN (pc.SETs+ZZs));  -- these ops defined for integers
      ASSERT( n.l.type.mode = n.type.mode);   -- that is necessary for some
      ASSERT( n.r.type.mode = n.type.mode);   -- useful optimizations

      IF (trySortTreeLock = 0)
         AND
         ((n.sub = pc.sb_and) OR (n.sub = pc.sb_or))
         AND
         (((n.l.mode = n.mode) AND (n.l.sub = n.sub)) OR
          ((n.r.mode = n.mode) AND (n.r.sub = n.sub)))
         AND
          trySortTree(n)
      THEN
        INC(trySortTreeLock);
        GenExpr(n, NIL);
        DEC(trySortTreeLock);
      ELSE
        GenExpr (n.l, NIL);
        GenExpr (n.r, NIL);
        SYSTEM.EVAL(EvalBin (n));             -- propagate values, if const
      END;

  | pc.sb_cand:
      GenExpr (n.l, NIL); ValsCopy (CurProps, vals);   -- join values maps
      GenExpr (n.r, NIL); ValsAppend (vals, CurProps); -- before&after 2nd arg

      -- values calculation and optimizations:
      IF ~EnConsProp THEN
      ELSIF n.l.mode = pc.nd_value (*3/n.l.val # NIL*) THEN
        IF n.l.val.is_zero ()
         THEN n.copy (n.l);(*3 ValsCopy (vals, CurProps);*) -- F & y -> F (*@18*)
         ELSE n.copy (n.r);                            -- T & y -> y
        END;
      ELSIF n.r.mode = pc.nd_value (*3/n.r.val # NIL*) THEN
        IF ~n.r.val.is_zero () THEN
          n.copy (n.l);               -- x & T -> x
        END;                          -- x & F not optimized
      END;                            -- (x m.h. side effect)

  | pc.sb_cor:
      GenExpr (n.l, NIL); ValsCopy (CurProps, vals);
      GenExpr (n.r, NIL); ValsAppend (vals, CurProps);

      -- values calculation and optimizations:
      IF ~EnConsProp THEN
      ELSIF n.l.mode = pc.nd_value (*3/n.l.val # NIL*) THEN                           (* IRMOD *)
        IF n.l.val.is_zero () THEN
          n.copy (n.r);               -- F ! y -> y
        ELSE
          n.copy (n.l);
          (*3 ValsCopy (vals, CurProps); *) -- T ! y -> T (*@18*)
        END;
      ELSIF n.r.mode = pc.nd_value (*3 n.r.val # NIL*) THEN
        IF n.r.val.is_zero () THEN
          n.copy (n.l);               -- x ! F -> x
        END;                          -- x ! T not optimized
      END;                            -- (x m.h. side effect)

  | pc.sb_ash
  , pc.sb_lsh
  , pc.sb_shl
  , pc.sb_shr:
      ASSERT( (n.type.mode = n.l.type.mode)
           OR n.l.type.is_equal(n.type.mode)    -- WES-10: XDS Wish list: 11-K: Fix F450 Subrange to ASH
            );  

      GenExpr (n.l, NIL);
      GenExpr (n.r, NIL);

      -- values calculation and optimizations:
      IF n.r.mode # pc.nd_value (*3/n.r.val = NIL*) THEN
      ELSIF EvalBin (n) THEN            -- propagate values, if const
      ELSIF ~EnConsProp THEN
      ELSIF n.r.val.is_zero () THEN
        n.copy (n.l);                   -- x sh 0 -> x  (* IRMOD *)
      END;

  | pc.sb_cmplx
  , pc.sb_difadr:
      GenExpr (n.l, NIL);
      GenExpr (n.r, NIL);

      SYSTEM.EVAL(EvalBin (n));                 -- propagate values, if const

  | pc.sb_addadr
  , pc.sb_subadr:
      GenExpr (n.l, NIL);
      GenExpr (n.r, NIL);

      -- values calculation and optimizations:
      IF EvalBin (n) THEN               -- propagate values, if const
      ELSIF ~EnConsProp THEN
      ELSIF (n.r.mode = pc.nd_value(*3/n.r.val # NIL*)) & n.r.val.is_zero () THEN
        n.copy (n.l);                   -- x +/- 0 -> x  (* IRMOD *)
      END;

  | pc.sb_rot:
      GenExpr (n.l, NIL);
      GenExpr (n.r, NIL);

  | pc.sb_pre_inc
  , pc.sb_pre_dec
  , pc.sb_post_inc
  , pc.sb_post_dec:
      IF n.l.mode = pc.nd_var
        THEN v := GetValue (n.l.pos, n.l.obj <*IF TARGET_IDB THEN *>, n <* END *> );
        ELSE v := NIL;
      END;
      GenDesignator (n.l, FALSE, U_RD+U_WR);
      GenExpr (n.r, NIL);

      -- values calculation and optimizations:
      IF (v # NIL) & (n.r.mode = pc.nd_value (*3/n.r.val # NIL*)) THEN       -- if simple var has value
        w := pc.value.new (n.l.pos, n.l.type);
        IF (n.sub = pc.sb_pre_inc)
        OR (n.sub = pc.sb_post_inc)
          THEN w.binary (pc.sb_plus,  v, n.r.val);
          ELSE w.binary (pc.sb_minus, v, n.r.val);
        END;
        DefValue (n.l.obj, w);                 -- update its value in CurProps
      END;

<* IF TARGET_MEGOS THEN *>
      IF n.l.type.mode IN pc.WHOLEs-{pc.ty_loc} THEN
        EXCL(n.tags, pc.ntag_chk_range);
      END;
<* END *>

  | pc.sb_concat:
      GenExpr (n.l, NIL);
      GenExpr (n.r, NIL);

      -- reusability check:
      IF MakeReusable (n(*=>*), n.l, ru_binary, FALSE)
      OR MakeReusable (n(*=>*), n.r, ru_binary, FALSE)
        THEN GenExpr (n, NIL);
        ELSE SYSTEM.EVAL(EvalBin (n));                 -- propagate values, if const
      END;

  | pc.sb_exp:
      ASSERT( n.type.mode IN (pc.REALs+pc.CPLXs)) ;

      GenExpr (n.l, NIL);
      GenExpr (n.r, NIL);

      SYSTEM.EVAL(EvalBin (n));                        -- propagate values, if const
  ELSE
    ASSERT( FALSE );
  END;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenExprBinary") END;
<* END *>
END GenExprBinary;

--------------------------------------------------------------------------------
-- Traverse unary expression node
PROCEDURE GenExprUnary ( n: pc.NODE );  -- nd_unary
VAR
  i: LONGINT;
  l: pc.NODE;
  v: pc.VALUE; (*3/ *)
BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN
    dbg.CallTraceInd (1, "(GenExprUnary: n=%X .sub=", n); dbg.VisNodSubMode (n);
  END;
<* END *>

  CASE n.sub OF
  | pc.su_bits
  , pc.su_bytes
  , pc.su_size
  , pc.su_words:
      i := pc.code.get_size (n.sub, n.l.type);
--    i := (n.l.type.num+7) DIV 8; -- BNRP, PR252: size inconsistency in ccCode *)
      GenDesignator(n.l,FALSE,U_SZ);
      IF i >= 0 THEN
        EvalVal (n, i);
      END;

  | pc.su_bit_offs
  , pc.su_byte_offs
  , pc.su_word_offs:
      i := pc.code.get_offs (n.sub, n.l.obj);
      GenDesignator (n.l, FALSE, U_SZ);
      IF i >= 0 THEN
        EvalVal (n, i);
      END;

  | pc.su_min:
(*3/
      IF n.l.type.min # NIL THEN
        n.pos := n.l.type.min.val.pos;   ( * need? save expr pos? * )
        SetValue (n, n.l.type.min.val);
      END;
*)
      IF (n.l.type.min # NIL) &
         ~ (n.l.type.mode IN pc.REALs)
      THEN
        n.val := n.l.type.min;
        n.l   := NIL;
        n.mode:= pc.nd_value;
      END;

  | pc.su_max:
(*3/
      IF (n.l.type.max # NIL) THEN
        n.pos := n.l.type.max.val.pos;   ( * need? save expr.pos? * )
        SetValue (n, n.l.type.max.val);
      END;
*)
      IF (n.l.type.max # NIL) &
         ~ (n.l.type.mode IN pc.REALs)
      THEN
        n.val := n.l.type.max;
        n.l   := NIL;
        n.mode:= pc.nd_value;
      END;

  | pc.su_is:
      GenDesignator (n.l, FALSE, U_TP);

  | pc.su_abs
  , pc.su_neg:
      GenExpr (n.l, NIL);
      SYSTEM.EVAL(EvalUn (n));                   -- propagate values, if const

  | pc.su_cc:
      GenExpr (n.l, NIL);

  | pc.su_adr
  , pc.su_adr_o2:
    IF (n.l.mode=pc.nd_sproc)&((n.l.sub=pc.sp_callerIPref)OR(n.l.sub=pc.sp_excepttable)) THEN
    ELSE
      GenDesignator (n.l, TRUE, U_AD);

      -- simplifications (significant for BEs)
      IF (n.l.mode = pc.nd_var)
       & (n.l.obj.mode = pc.ob_var) THEN (* ADR(var) *)
        IF n.l.obj.attr # NIL THEN
          n.copy (n.l.obj.attr(pc.NODE));          -- -> var          (* IRMOD *)
          GenExpr (n, NIL);
        ELSIF (*!*)EnSL1Addr(*!*)        -- SL1 var address calculation
            & (n.l.obj.attr # NIL)
        THEN
          ASSERT( n.l.obj.flag = pc.flag_sl1 );
          EvalVal (n, n.l.obj.attr(pc.NODE).l.val.get_integer () * 4);
        END;
      ELSIF (n.l.mode = pc.nd_deref)     (* ADR(p^) *)
          & (n.l.type.mode # pc.ty_array_of) -- p^ # PTR TO ARRAY OF!
      THEN
        n.sub := pc.su_conv;             -- ADR(p^) -> su_conv(p) (* IRMOD *)
        n.l   := n.l.l;
        GenExpr (n, NIL);                -- continue from modfied node
(*Vit: ADR(arr[const]) optimization to ADR(arr)+const*size eliminated
      ELSIF (n.l.mode = pc.nd_index)     (* ADR(arr[const]) *)
          & (n.l.r.mode = pc.nd_value (*3/ n.l.r.val # NIL*)) THEN
(*@13:  vIntTmp.pos := n.l.r.pos; *)
        vIntTmp.binary (pc.sb_minus, n.l.r.val, n.l.r.type.min(*3 .val*));
        i := vIntTmp.get_integer ();      -- i = index - LOW
        s := pc.code.get_size (pc.su_size, n.l.l.type.base);
        IF (i = 0) OR (s = 0) THEN
          n.l := n.l.l;
          GenExpr (n, NIL);              -- continue from modfied node
        ELSIF s > 0 THEN
          n.l.mode := pc.nd_unary;
          n.l.sub  := n.sub;
          n.l.r    := NIL;
          n.l.type := n.type;
          n.mode   := pc.nd_binary;
          n.sub    := pc.sb_addadr;
          n.r := NewNode (n.pos, pc.nd_value, pc.ZZ_type);
          EvalVal (n.r, i*s);
        END;
*)
      END;
    END;

  | pc.su_conv:
      IF n.type.mode IN pc.PADRs THEN -- for computers with 64-bit address
        l := n.l;
        WHILE (l.mode = pc.nd_unary)
            & (l.sub = pc.su_conv)
            & l.type.is_ordinal ()
        DO
          l := l.l;
        END;
        IF l.type.mode IN pc.PADRs THEN
          n.l := l
        END;
      END;

      IF (n.l.mode = pc.nd_binary)    -- @20: sb_cmplx with ty_CC
       & (n.l.sub  = pc.sb_cmplx)     -- casts to given type
       & (n.l.type.mode = pc.ty_CC)
      THEN
        n.l.type := n.type;
      END;

      GenExpr (n.l, n.type);

      -- values calculation and optimizations:
      IF n.l.mode = pc.nd_value (*3/ n.l.val # NIL*) THEN
        IF  n.type.is_ordinal() & n.l.type.is_ordinal()
        OR (n.type.mode IN pc.REALs) & (n.l.type.mode IN pc.REALs)
        THEN
          SetValueCast (n, n.l.val);
          CheckValueRange (n);
        ELSIF (n.type.mode IN pc.PTRs) & (n.l.type.mode IN pc.PTRs)
           OR (n.type.mode = pc.ty_proctype) & (n.l.type.mode = pc.ty_proctype)
           OR (n.type.mode IN pc.CPLXs) & (n.l.type.mode IN pc.CPLXs)
        THEN
          SetValueCast (n, n.l.val);
        ELSIF (n.type.mode = pc.ty_proctype) & (n.l.type.mode = pc.ty_AA) THEN
          NewValue (n);
          n.val.set_object (NIL);
        ELSIF n.type.is_ordinal () & (n.l.type.mode = pc.ty_SS) THEN
          v := pc.value.new(n.pos, n.type);
    (*3/  NewValue (n); *)
          v (*3/n.val*).index_get (0, n.l.val);
          SetValue (n, v ); (*3/ *)
          CheckValueRange (n);
        ELSIF (n.type.mode IN pc.REALs) & n.l.type.is_ordinal ()
           OR (n.type.is_ordinal () & (n.l.type.mode IN pc.REALs))
        THEN
          SYSTEM.EVAL(EvalUn (n));
        ELSIF (n.type.mode = pc.ty_array) & (n.l.type.mode = pc.ty_SS) THEN
          ASSERT( (IsChar (n.type.base) OR (n.type.base.mode=pc.ty_loc)) & IsChar (n.l.type.base));
                    SetValueCast (n, n.l.val);
(*<An4*)
        ELSIF pcS.TS_ext() & (n.type.mode = pc.ty_array) & (n.l.type.mode = pc.ty_ZZ) THEN
          ASSERT(n.type.base.mode=pc.ty_loc);
          SetValueCast (n, n.l.val);
(*An4>*)
        END;

      ELSIF n.type = n.l.type THEN
        n.copy (n.l);                    (* IRMOD *)

      ELSIF n.type.is_ordinal () & n.l.type.is_ordinal () THEN
        WHILE (n.l.mode = pc.nd_unary)  -- remove useless type conversion
            & (n.l.sub = pc.su_conv)    -- (done opt.mode-independently)
            &  n.l.l.type.is_ordinal()
            & (n.type.min # NIL)
            & ( (n.l.type.min(*3 .val*) = NIL)
           OR  CmpValue (pc.sb_geq, n.type.min(*3 .val*), n.l.type.min(*3 .val*)))
            & (n.type.max # NIL)
            & ( (n.l.type.max(*3 .val*) = NIL)
           OR  CmpValue (pc.sb_leq, n.type.max(*3 .val*), n.l.type.max(*3 .val*)))
            & EnConsProp (*3/ *)
        DO
          IF pc.ntag_chk_range IN n.l.tags THEN
            INCL (n.tags, pc.ntag_chk_range)
          END;
          n.l.copy (n.l.l);               (* IRMOD *)
        END;
        IF (n.l.type.mode = pc.ty_ZZ)
        OR (  n.type.mode = pc.ty_ZZ) THEN
          -- nothing for literals
        ELSIF CmpValue (pc.sb_lss, n.type.max(*3 .val*), n.l.type.min(*3 .val*))
           OR CmpValue (pc.sb_gtr, n.type.min(*3 .val*), n.l.type.max(*3 .val*))
        THEN
          Err (n.pos, 122);  -- expr out of bounds (* passed for BNRP *)
        ELSIF (pc.ntag_chk_range IN n.tags)
            & CmpValue (pc.sb_leq, n.type.min(*3 .val*), n.l.type.min(*3 .val*))
            & CmpValue (pc.sb_geq, n.type.max(*3 .val*), n.l.type.max(*3 .val*))
        THEN
          EXCL (n.tags, pc.ntag_chk_range);
        END;

      ELSIF (n.type.mode IN pc.PADRs)
          & (n.l.type.mode IN pc.PADRs)
      THEN
        WHILE (n.l.mode = pc.nd_unary)       -- remove useless type conversion
            & (n.l.sub = pc.su_conv)         -- (done opt.mode-independently)
            & (n.l.l.type.mode IN pc.PTRs)
            & EnConsProp (*3/ *)
        DO
          n.l.copy (n.l.l);               (* IRMOD *)
        END;
      END;

  | pc.su_cast:
      IF (n.type.mode IN pc.PADRs)
       & (n.l.type.mode IN pc.PADRs)
      THEN
        n.sub := pc.su_conv;
        GenExpr (n, NIL);
      ELSE
        GenExpr (n.l, NIL);

        -- values calculation and optimizations:
        IF n.l.mode = pc.nd_value (*3/ n.l.val # NIL*) THEN
          IF pc.value.is_ordinal (n.type)
           & pc.value.is_ordinal (n.l.type)
          THEN
            SetValueCast (n, n.l.val);
            (* cast_ordinal is called only for ordinal types!
               Note: value_is_ordinal returns TRUE for pointers, ty_AA, etc. *)
            IF n.type.is_ordinal() THEN
              n.val.cast_ordinal (n.type);
            END;
          ELSIF (n.type.mode IN pc.PTRs) & (n.l.type.mode IN pc.PTRs)
             OR (n.type.mode = pc.ty_set) & pc.value.is_ordinal (n.l.type)
          THEN
            SetValueCast (n, n.l.val)
(*<An4*)
          ELSIF pcS.TS_ext() & (n.type.mode = pc.ty_array) & (n.l.type.mode = pc.ty_ZZ) THEN
            SetValueCast (n, n.l.val);
(*An4>*)
          END;
        END;
      END;

  | pc.su_odd
  , pc.su_not
  , pc.su_cap
  , pc.su_compl
  , pc.su_entier
  , pc.su_length
  , pc.su_im
  , pc.su_re:
      GenExpr (n.l, NIL);
      SYSTEM.EVAL(EvalUn (n));               -- propagate values, if const

  | pc.su_vptr2ptr
  , pc.su_ptr2vptr:
      GenExpr (n.l, NIL);

      -- values calculation and optimizations:
      IF EvalUn (n) THEN
      ELSIF (n.l.mode = pc.nd_unary)
          & (n.l.sub  = pc.su_ptr2vptr)
          & (n.sub    = pc.su_vptr2ptr)
         OR (n.l.mode = pc.nd_unary)
          & (n.l.sub  = pc.su_vptr2ptr)
          & (n.sub    = pc.su_ptr2vptr)
      THEN
        IF n.l.l.type = n.type THEN
          n.copy (n.l.l);
        ELSE
          n.l   := n.l.l;
          n.sub := pc.su_conv;
          GenExpr (n, NIL);
        END;
      ELSIF (n.l.mode = pc.nd_unary)
          & ( (n.l.sub = pc.su_conv)
         OR (n.l.sub  = pc.su_cast) )
          & (n.l.l.type.mode IN pc.PADRs)
      THEN
        n.l := n.l.l;
      END;
  ELSE
    Fault (n.pos, 103); -- Int.error: value expected
  END;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenExprUnary") END;
<* END *>
END GenExprUnary;

-------------------------------------------------------------------------

PROCEDURE GenExprList ( VAR n: pc.NODE
                      ;     l: pc.NODE
                      )      :         BOOLEAN;
(**
   ('VAR n' actual is usually a link position in some node: changing it
   causes tree restructuring)
*)
VAR chng: BOOLEAN;

BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (1, "(GenExprList:") END;
<* END *>

  chng := FALSE;
  WHILE l # NIL DO
    GenExpr (l, NIL);
    IF MakeReusable (n(*=>*), l, ru_prm, FALSE) THEN
      chng := TRUE
    END;
    l := l.next;
  END;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenExprList => %b", chng) END;
<* END *>
  RETURN chng;
END GenExprList;


--------------------------------------------------------------------------------
-- Traverse set aggregate
PROCEDURE GenSetAggr ( n: pc.NODE );  -- nd_aggregate, type in SETs

  -- 1 -- GenSetAggr -----------------------------------------------------------
  PROCEDURE val (v: pc.NODE): LONGINT;
  BEGIN
  (*@13: vIntTmp.pos := n.pos; *)
    vIntTmp.binary (pc.sb_minus, v.val, n.type.min(*3 .val*));
    RETURN vIntTmp.get_integer ();
  END val;


-- 0 -- GenSetAggr -----------------------------------------------------------
VAR i,j,k: LONGINT;
       ok: BOOLEAN;
        l: pc.NODE;
        v: pc.VALUE; (*3/ *)
BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (1, "(GenSetAggr: n=%X", n) END;
<* END *>

  l := n.l; ok := TRUE;
  WHILE l # NIL DO
    IF l.mode = pc.nd_node THEN
      GenExpr (l.l, NIL);
      GenExpr (l.r, NIL);
      ok := ok & (l.l.mode = pc.nd_value (*3/ l.l.val # NIL)*))
               & (l.r.mode = pc.nd_value (*3/ l.r.val # NIL*));
    ELSE
      GenExpr (l, NIL);
      ok := ok & (l.mode = pc.nd_value (*3/ l.val # NIL*));
    END;
    l := l.next;
  END;

  IF ok & (env.errors.err_cnt <= 0) THEN
    v := pc.value.new(n.pos,n.type);
(*3/ NewValue (n); *)
    FOR i := 0 TO n.type.len-1 DO
      v (*3/ n.val*).index_set (i, vIntZero)
    END;
    l := n.l;
    WHILE l # NIL DO
      IF l.mode = pc.nd_node THEN
        i := val (l.l);
        j := val (l.r);
      ELSE
        i := val (l);
        j := i;
      END;
      FOR k := i TO j DO
        v (*3/n.val*).index_set (k, vIntOne)
      END;
      l := l.next;
    END;
    SetValue (n,v); (*3 / *)
  END;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenSetAggr") END;
<* END *>
END GenSetAggr;

-------------------------------------------------------------------------

PROCEDURE GenArrRecAggr ( n: pc.NODE );  -- nd_aggregate, ty_array/ty_record
(**
   Traverse array/record aggregate
*)
VAR i,j,k: LONGINT;
   ok,str: BOOLEAN;
        l: pc.NODE;
    v,w,u: pc.VALUE;  (*3/ v *)
        t: pc.STRUCT;
BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (1, "(GenArrRecAggr: n=%X", n) END;
<* END *>

  l  := n.l;
  ok := TRUE;                        -- accumulate "constant aggregate" flag
  WHILE l # NIL DO
    IF l.mode = pc.nd_node THEN
      GenExpr (l.l, NIL);
      GenExpr (l.r, NIL);
      ok := ok & (l.l.mode = pc.nd_value (*3/ l.l.val # NIL*))
               & (l.r.mode = pc.nd_value (*3/ l.r.val # NIL*));
    ELSE
      GenExpr (l, NIL);
      ok := ok & (l.mode = pc.nd_value (*3/ l.val # NIL*));
    END;
    l := l.next;
  END;

  IF ok & (env.errors.err_cnt <= 0) THEN
    (* Constant aggregate processing *)
    v := pc.value.new (n.pos, n.type); (*3/ NewValue (n); *)
    l := n.l;
    i := 0;
    WHILE l # NIL DO
      IF l.mode = pc.nd_node THEN
        j := l.r.val.get_integer ();
        w := l.l.val;
        t := l.l.type;
      ELSE
        j := 1;
        w := l.val;
        t := l.type;
      END;
      str := (n.type.mode IN pc.ARRs)
           & IsChar (n.type.base)
           & (t.mode = pc.ty_SS);
      WHILE j > 0 DO
        IF str THEN
          k := 0;
          LOOP
            IF k  >= t.len THEN EXIT END;
            u := pc.value.new (v (*3/n.val*).pos, n.type.base);
            u.index_get (k, w);
            IF u.is_zero () THEN EXIT END;
            v (*3/n.val*).index_set (i, u);
            INC (i);
            INC (k);
          END;
        ELSE
          v (*3/n.val*).index_set (i, w);
          INC (i);
        END;
        DEC (j);
      END;
      l := l.next;
    END;
    ASSERT(   (n.type.mode = pc.ty_record)
           OR (n.type.len = i) );
    SetValue (n, v);  (*3/ *)
  END;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenArrRecAggr") END;
<* END *>
END GenArrRecAggr;


(*==================== PROCEDURES INLINE SUBSTITUTION ======================*)

PROCEDURE Append ( VAR to : pc.NODE     -- stmt sequence to append to (mb NIL)
                 ;     nd : pc.NODE );  -- node (stmt seq) to add (mb NIL)
(**
  Append 'nd' to sequence 'to'.
  If 'to' ends with <nd_goto M> & 'nd' is <nd_label M>, remove goto.
*)
VAR n,l: pc.NODE;

BEGIN
  IF nd = NIL THEN                      -- nothing to add
    RETURN
  END;
  IF to = NIL THEN                      -- append to empty
    to := nd;
    RETURN;
  END;

  n := to; l := NIL;                    -- search l->n - 2 last elems
  WHILE n.next # NIL DO
    l := n;
    n := n.next;
  END;

  IF (n.mode = pc.nd_goto) & (n.l = nd) THEN (* IRMOD: eliminate goto to *)
    ASSERT( nd.mode = pc.nd_label );         (*  the following label *)
    IF l = NIL
      THEN to := nd;                    -- goto is 1st node
      ELSE l.next := nd;                --   - not 1st
    END;
  ELSE
    n.next := nd;                       -- no goto to next; just glue
  END;
END Append;

-------------------------------------------------------------------------

PROCEDURE ProcIsInline ( o   : pc.OBJECT;   -- proc.object
                         fExt: BOOLEAN
                       )     : BOOLEAN;
(*
*)

  -----------------------------------------------
  PROCEDURE MemOk (v: pc.OBJECT): BOOLEAN;
  (*
  *)
    VAR i: INTEGER;
  BEGIN
    IF pc.code.en_f_inline
     THEN i := 20;
     ELSE i := 5;
    END;
    WHILE v # NIL DO
      IF v.mode IN pc.OB_SET{pc.ob_var, pc.ob_cons} THEN
        DEC (i);
      ELSIF v.mode # pc.ob_type THEN
        RETURN FALSE;
      END;
      IF i < 0 THEN RETURN FALSE END;
      v := v.next;
    END;
    RETURN TRUE;
  END MemOk;

  -----------------------------------------------

  PROCEDURE ProfOk ( v: pc.OBJECT
                   )  :                 BOOLEAN;
  (*
  *)
  BEGIN
    WHILE v # NIL DO
      IF ~(v.mode IN pc.OB_SET{pc.ob_var, pc.ob_varpar}) THEN
        RETURN FALSE
      END;
      IF (v.mode      = pc.ob_varpar)
       & (v.type.mode = pc.ty_record)
       & (v.type.flag IN pc.OOP_langs)
      THEN
        RETURN FALSE
      END;
      IF v.type.mode = pc.ty_array_of THEN
        RETURN FALSE
      END;
      v := v.next;
    END;
    RETURN TRUE;
    END ProfOk;

  -----------------------------------------------
BEGIN
    IF ~(pc.ttag_neverinline IN o.type.tags)
       & ~(pc.xot_noninlinable IN o.xtags)
       & (o.val # NIL)
       & (o.mode IN pc.OB_SET{pc.ob_proc, pc.ob_xproc, pc.ob_lproc})
       & ( (o.type.base.mode = pc.ty_void) OR pc.code.en_f_inline)
       & ((tmark_genok IN o.type.marks) OR fExt)
       & ((MemOk  (o.type.mem) & ProfOk (o.type.prof)& ~(tmark_inl_dis IN o.type.marks)&EnInline) OR
           (pc.ttag_alwaysinline IN o.type.tags))
     THEN
       RETURN TRUE;
     ELSE
       INCL (o.xtags, pc.xot_noninlinable);
       RETURN FALSE;
    END;
END ProcIsInline;


--------------------------------------------------------------------------------
PROCEDURE InlineProc ( n: pc.NODE; fExt : BOOLEAN);      -- nd_call

VAR nodes : ARRAY 512 OF pc.NODE;
    ncnt  : INTEGER;
    vars  : ARRAY 4096 OF pc.OBJECT;
    vcnt  : INTEGER;
    proc  : pc.OBJECT;
    result: pc.OBJECT;

  ---------------------------------------

  PROCEDURE CreateNode ( fr: pc.NODE
                       )   :          pc.NODE;
  (**
     Make a copy of node 'fr' without tree-forming links
     All fields are copied; .pos = null_pos; .next, .l, .r = NIL;
  *)
  VAR n: pc.NODE;
  BEGIN
    n     := CopyNode (fr);
    n.pos := fr.pos;
    n.end := fr.end;
    n.l   := NIL;
    n.r   := NIL;
    INCL( n.tags, pc.ntag_constrinlined);
    RETURN n;
  END CreateNode;

  ---------------------------------------

  PROCEDURE RememberNode (a, b: pc.NODE);
  (*
  *)
  BEGIN
    nodes[ncnt] := a; INC (ncnt);
    nodes[ncnt] := b; INC (ncnt);
  END RememberNode;

  ---------------------------------------

  PROCEDURE ReplaceNode ( fr: pc.NODE
                        )   :         pc.NODE;
  (*
  *)
  VAR i: INTEGER;
      n: pc.NODE;
  BEGIN
    ASSERT( fr # NIL );
    i := 0;
    WHILE i < ncnt DO
      IF nodes[i] = fr THEN RETURN nodes[i+1] END;
      INC (i, 2);
    END;
    ASSERT( fr.mode = pc.nd_label );
    n := CreateNode (fr);
    RememberNode (fr, n);
    RETURN n;
  END ReplaceNode;

  ---------------------------------------

  PROCEDURE IsSimple ( ap: pc.NODE
                     )   :          BOOLEAN;
  (*
  *)
  BEGIN
    RETURN (ap.mode = pc.nd_var)
        OR (ap.mode = pc.nd_field)
         &  IsSimple (ap.l);
  END IsSimple;

  ---------------------------------------

  PROCEDURE CopyTree ( fr: pc.NODE
                     )   :         pc.NODE;
  (*
  *)
    PROCEDURE ReplaceVar ( v: pc.OBJECT
                         ; m: pc.NODE);
    (*
    *)

    VAR      i: INTEGER;
        l,k,mm: pc.NODE;
             o: pc.OBJECT;
    BEGIN
      IF v.host # n.obj.type THEN RETURN END;
      IF v.mode = pc.ob_varpar THEN
        l := n.r;
        o := n.obj.type.prof;
        WHILE o # v DO
          o := o.next;
          l := l.next;
        END;
        ASSERT( l # NIL );
        IF IsSimple (l) THEN
          k      := l.next;
          l.next := NIL;
          mm     := CopyTree (l);
          l.next := k;
          CASE m.mode OF
          | pc.nd_var:
              m.copy (mm);
          | pc.nd_assign
          , pc.nd_call:
              m.obj := NIL;
              m.l := mm;
          ELSE
            ASSERT( FALSE );
          END;
          RETURN;
        END;
      END;
      i := 0; (* FOR statement here is not work under X2 *)
      WHILE i < vcnt DO
        IF vars[i] = v THEN
          IF vars[i].mode # pc.ob_varpar THEN
            m.obj := vars[i+1];
          ELSE
            IF m.mode = pc.nd_var THEN
              NEW (k);
              k.copy (m);
              k.obj := vars[i+1];
              k.type:= k.obj.type;
              m.mode:= pc.nd_deref;
              m.obj := NIL;
              m.tags:= pc.NTAG_SET{};
              m.sub := pc.su_none;
              m.l   := k;
              m.r   := NIL;
            ELSIF (m.mode = pc.nd_assign) OR (m.mode = pc.nd_call) THEN
              k     := NewNodObj (pc.nd_var, vars[i+1], env.null_pos);
              mm    := NewNode (env.null_pos, pc.nd_deref, k.type.base);
              mm.l  := k;
              m.obj := NIL;
              m.l   := mm;
            ELSE
              m.obj := vars[i+1];
            END;
          END;
          RETURN
        END;
        INC (i);
      END;
      ASSERT( FALSE );
    END ReplaceVar;

    -------------------------
  VAR l,m,to: pc.NODE;

  BEGIN
    to := NIL; l := NIL;
    WHILE fr # NIL DO
      IF fr.mode = pc.nd_label
       THEN m := ReplaceNode (fr);
       ELSE m := CreateNode (fr);
      END;
      IF l = NIL
       THEN to := m
       ELSE l.next := m
      END;
      l := m;
      CASE fr.mode OF
      | pc.nd_var:
          ReplaceVar (fr.obj, m);
      | pc.nd_assign
      , pc.nd_call:
          IF fr.l = NIL
           THEN ReplaceVar (fr.obj, m);
           ELSE m.l := CopyTree (fr.l);
          END;
          m.r := CopyTree (fr.r);
      | pc.nd_activate
      , pc.nd_reraise
      , pc.nd_retry
      , pc.nd_exit:
          m.r := ReplaceNode (fr.r);
      | pc.nd_goto:
          m.l := ReplaceNode (fr.l);
      | pc.nd_loop
      , pc.nd_while
      , pc.nd_repeat:
          RememberNode (fr, m);
          m.l := CopyTree (fr.l);
          m.r := CopyTree (fr.r);
      | pc.nd_block:
          RememberNode (fr, m);
          m.r := CopyTree (fr.r);
      | pc.nd_return:
          IF fr.l # NIL THEN
            ASSERT( result # NIL);
            m.mode := pc.nd_assign;
            m.r    := CopyTree (fr.l);
            m.obj  := result;
            m.next := NewNode (env.null_pos, pc.nd_return, VoidType);
            m      := m.next;
            m.tags := l.tags;
            l.tags := pc.NTAG_SET{};
            l      := m;
          END;
          m.pos := env.null_pos;
          m.end := env.null_pos;
          m.r   := ReplaceNode (fr.r);
      | pc.nd_except
      , pc.nd_for:
          RememberNode (fr, m);
          ReplaceVar (fr.obj, m);
          m.l := CopyTree (fr.l);
          m.r := CopyTree (fr.r);
      | pc.nd_with
      , pc.nd_protect:
          ReplaceVar (fr.obj, m);
          m.l := CopyTree (fr.l);
          m.r := CopyTree (fr.r);
      ELSE
        m.l := CopyTree (fr.l);
        m.r := CopyTree (fr.r);
      END;
      fr := fr.next;
    END;
    RETURN to;
  END CopyTree;

  -----------------------------------------------

VAR m,l,a,x,y: pc.NODE;
          o,v: pc.OBJECT;
         h,vt: pc.STRUCT;

BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (1, "(InlineProc: n=%X", n) END;
<* END *>


  ASSERT( n.mode = pc.nd_call );

  proc := n.obj;
  ncnt := 0;
  vcnt := 0;
  a    := NIL;
  Restart := TRUE;

  IF (fExt) THEN env.errors.Warning (n.pos, 331, proc.name^);
  ELSE           env.errors.Warning (n.pos, 330, proc.name^); END;

  INCL (proc.type.marks, tmark_inlined);  -- inlining break
  IF CurProc = NIL                        -- current host
   THEN h := CurMod;
   ELSE h := CurProc;
  END;

  (* make variable for procedure result *)  -- LAZ
  IF n.type.mode = pc.ty_void
   THEN result := NIL;
   ELSE result := pc.new_object_ex (n.type, h, pc.ob_var, FALSE);
  END;

  o := proc.type.mem;
  WHILE o # NIL DO
    IF o.mode IN pc.OB_SET{pc.ob_var, pc.ob_cons} THEN
      v := pc.new_object_ex (o.type, h, o.mode, FALSE);
      IF (o.mode=pc.ob_cons) THEN
        v.val := o.val;
      ELSE
        ASSERT((o.val=NIL));
        v.val := NIL;
      END;
      v.attr:= o.attr;
<* IF TARGET_IDB THEN *>
      v.pos := o.pos; v.end := o.end;
<* END *>
      vars[vcnt] := o; INC (vcnt);
      vars[vcnt] := v; INC (vcnt);
    END;
    o := o.next;
  END;

  o := proc.type.prof;
  l := n.r;
  WHILE o # NIL DO
    IF o.mode = pc.ob_var THEN
      v := pc.new_object_ex (o.type, h, pc.ob_var, FALSE);
      EXCL (v.tags, pc.otag_valpar);
      v.val := NIL;
      v.attr:= o.attr;
<* IF TARGET_IDB THEN *>
      v.pos := o.pos; v.end := o.end;
<* END *>
      vars[vcnt] := o; INC (vcnt);
      vars[vcnt] := v; INC (vcnt);
      m     := NewNode (n.pos, pc.nd_assign, VoidType);
      INCL(m.tags, pc.ntag_constrinlined);
      m.obj := v;
      m.r   := CopyNode (l);
      Append (a(*=>*), m);
    ELSIF (o.mode = pc.ob_varpar) & ~IsSimple (l) THEN
      vt := pc.new_type (pc.ty_pointer);
      vt.base := o.type;
      v := pc.new_object_ex (vt, h, pc.ob_var, FALSE);
      ASSERT(o.val=NIL);
      v.val  := NIL;
      v.attr := o.attr;
<* IF TARGET_IDB THEN *>
      v.pos := o.pos; v.end := o.end;
<* END *>
      vars[vcnt] := o; INC (vcnt);
      vars[vcnt] := v; INC (vcnt);
    (* ADR(parameter) *)
      x := NewNode (env.null_pos, pc.nd_unary, vt (*!!!should be address*));
      INCL(x.tags, pc.ntag_constrinlined);
      x.sub := pc.su_adr;
      x.l   := CopyNode (l);
    (* lconv node: *)
      y   := NewNode (env.null_pos, pc.nd_lconv, vt);
      INCL(y.tags, pc.ntag_constrinlined);
      y.l := x;
    (* assign:  ptr := PTR_TYPE(ADR(parameter)) *)
      m := NewNode (n.pos, pc.nd_assign, VoidType);
      INCL(m.tags, pc.ntag_constrinlined);
      m.obj := v;
      m.r   := y;
      Append (a(*=>*), m);
    END;
    o := o.next;
    l := l.next;
  END;
  Append (a(*=>*), CopyTree (proc.val.r));

  IF result # NIL THEN
    n.mode := pc.nd_replace;
    n.l := NewNode (env.null_pos, pc.nd_block, VoidType);
    INCL(n.l.tags, pc.ntag_constrinlined);
    n.l.r := a;
    n.r := NewNodObj (pc.nd_var, result, env.null_pos);
  ELSE
    n.mode := pc.nd_block;
    n.tags := pc.NTAG_SET{};
    n.obj  := NIL;
    n.l    := NIL;
    n.r    := a;
  END;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")InlineProc") END;
<* END *>
END InlineProc;


(*===========================================================================*)

PROCEDURE CallForceWorst ( o: pc.OBJECT
                         )  :           BOOLEAN;
(**
   Assume only local vars of current proc in CurProps !
*)
BEGIN
  ASSERT( o.mode IN pc.PROCs);
  IF (pc.ttag_usage_ok IN o.type.tags)
  OR (CurProc = NIL)
  OR (o.lev <= CurProc.obj.lev)
  THEN
    RETURN FALSE;
  END;
  WHILE o.lev > CurProc.obj.lev DO
    o := o.host.obj
  END;
  RETURN o = CurProc.obj;
END CallForceWorst;

-------------------------------------------------------------------------

PROCEDURE IsSStoArrConv ( n: pc.NODE
                        )  :         BOOLEAN;
(**
   Is 'n' nd_unary.su_conv(ty_array->ty_SS) ?
*)
BEGIN
  RETURN (n.mode = pc.nd_unary)
       & (n.sub  = pc.su_conv)
       & (n.type.mode   = pc.ty_array)
       & (n.l.type.mode = pc.ty_SS)
       & IsChar (n.type.base)
END IsSStoArrConv;


--------------------------------------------------------------------------------
-- Traverse a proc/func call
-- ('VAR n' actual is usually a link position in some node: changing it
-- causes tree restructuring)
PROCEDURE GenCall ( VAR n: pc.NODE
                  )      :         BOOLEAN;  -- T: re-traverse from n rqrd

  -- 1 -- GenCall ---------------------------------------------------------------
  PROCEDURE SearchMethod ( t: pc.STRUCT
                         ; o: pc.NODE) ;
  VAR m,n: pc.OBJECT;
  BEGIN
    IF t.mode = pc.ty_pointer THEN
      t := t.base
    END;
    WHILE t # NIL DO
      ASSERT( t.mode = pc.ty_record );
      m := t.mem;
      WHILE m # NIL DO
        n := m;
        LOOP
          ASSERT( n.type.obj = n );
          IF o.obj = n THEN
            o.obj := m;
            o.type := o.obj.type;
            RETURN
          END;
          IF n.type.inx = NIL THEN EXIT END;
          n := n.type.inx.obj;
        END;
        m := m.next;
      END;
      t := t.base;
    END;
    ASSERT( FALSE );
  END SearchMethod;

  -- 1 -- GenCall ---------------------------------------------------------------
  PROCEDURE TypesEqu ( t1, t2: pc.STRUCT
                     )      :           BOOLEAN;
  VAR a,b: pc.OBJECT;
  BEGIN
    IF t1.mode # t2.mode THEN RETURN FALSE END;
    IF t1.flag # t2.flag THEN RETURN FALSE END;
    CASE t1.mode OF
    | pc.ty_shortcard, pc.ty_cardinal, pc.ty_longcard
    , pc.ty_shortint,  pc.ty_integer,  pc.ty_longint,   pc.ty_ZZ
    , pc.ty_real,      pc.ty_longreal, pc.ty_ld_real,   pc.ty_RR
    , pc.ty_complex,   pc.ty_lcomplex, pc.ty_CC
    , pc.ty_boolean,   pc.ty_char,     pc.ty_AA
    , pc.ty_loc,       pc.ty_module,   pc.ty_protection
    , pc.ty_void,      pc.ty_process:
        RETURN TRUE;
    | pc.ty_range
    , pc.ty_enum
    , pc.ty_opaque:
        RETURN t1 = t2;
    | pc.ty_pointer
    , pc.ty_set:
        RETURN TypesEqu (t1.base, t2.base);
    | pc.ty_proctype:
        a := t1.prof;
        b := t2.prof;
        WHILE (a # NIL) & (b # NIL) & TypesEqu (a.type, b.type) DO
          a := a.next; b := b.next;
        END;
        RETURN (a = NIL) & (b = NIL) & TypesEqu (t1.base, t2.base);
    | pc.ty_array
    , pc.ty_array_of:
        RETURN TypesEqu (t1.base, t2.base)
             & TypesEqu (t1.inx, t2.inx);
    | pc.ty_record:
        RETURN t1 = t2;
    | pc.ty_SS:
        RETURN t1.len = t2.len;
    END;
  END TypesEqu;

-- 0 -- GenCall ----------------------------------------------------------------
VAR l,p,c: pc.NODE;
   this,o: pc.OBJECT;
        u: pc.USAGE;
        t: pc.STRUCT;
      hbz: BOOLEAN;
     fExt: BOOLEAN;
BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (1, "(GenCall: n=%X", n) END;
<* END *>

(* -*FSA: condition break :)
  IF (n.obj   # NIL) AND (n.obj.name       # NIL) AND
           (Strings.Compare(n.obj.name^, "unlock") = Strings.equal) AND
     (CurProc # NIL) AND (CurProc.obj.name # NIL) AND
           (Strings.Compare(CurProc.obj.name^, "allocate_signals") = Strings.equal)
  THEN
    Wrn(n.pos,0);
  END;
 *)
  hbz := FALSE;
  IF n.l # NIL THEN
    (* here must be value, not designator *)
    IF n.l.mode # pc.nd_proc THEN
      GenExpr (n.l, NIL);
      WHILE (n.l.mode = pc.nd_unary)
          & (n.l.sub  = pc.su_conv)
          & (n.l.l.type.mode = pc.ty_proctype)
          & TypesEqu (n.l.type, n.l.l.type)
      DO n.l := n.l.l;
      END;
    END;
    IF (n.l.mode = pc.nd_method) & (CurMod # NIL) THEN
      (* try to convert method call into static call *)
      p := n.l;
      l := p.l;
      t := l.type;
      IF t.mode = pc.ty_pointer THEN
        t := t.base
      END;
      IF  (t.mno = CurMod.mno)
       & ~(tmark_basetype IN t.marks)
       & ~t.obj.is_public ()
        THEN t := l.type;
        ELSE t := l.dynamic_type ();
      END;
      IF t # NIL THEN
        SearchMethod (t, p);
        ASSERT( p.type = p.obj.type);
        IF l.type # p.type.prof.type THEN
          c   := NewNode (l.pos, pc.nd_guard, p.type.prof.type);
          c.l := l;
          l   := c;
        END;
        p.mode := pc.nd_proc;
        l.next := n.r;
        n.r    := l;
        p.l    := NIL;
      END;
    END;
    IF n.l.mode = pc.nd_proc THEN
      n.obj := n.l.obj;
      n.l   := NIL;
    ELSIF n.l.mode = pc.nd_value (*3/ n.l.val # NIL*) THEN
      n.obj := n.l.val.get_object ();
      n.l   := NIL;
      ASSERT( n.obj # NIL);
    END;
  END;
  l := n.r;
  u := NIL;
  IF n.l = NIL THEN
    ObjectUsage (n.pos, n.obj, U_CL, NIL); (* <- this must be done, even
                                               if procedure will be inlined!
                                               Else .use is invalid.
                                             *)
    IF (CurMod # NIL) THEN
      fExt := (n.obj.mno # CurMod.mno);
      IF ProcIsInline (n.obj,fExt) THEN
        InlineProc (n,fExt);
        ValsWorstSetU (CurProps);
        RETURN TRUE;
      END;
    END;
    nestedThrowsCall:=nestedThrowsCall OR (pc.ttag_throws IN n.obj.type.tags);
    o := n.obj.type.prof;
    u := n.obj.type.use;
    hbz := CallForceWorst (n.obj);
    IF (n.obj.val # NIL) & (pc.ntag_no_exit IN n.obj.val.tags) THEN
      INCL (n.tags, pc.ntag_no_exit);
    END;
  ELSIF n.l.mode = pc.nd_method THEN
    this := n.l.type.prof;
    nestedThrowsCall:=nestedThrowsCall OR (pc.ttag_throws IN n.l.type.tags);
    ObjectUsage (n.l.pos, n.l.obj, U_CL, NIL);
    p   := n.l.l;
    u   := n.l.obj.type.use;
    hbz := CallForceWorst (n.l.obj);
    ASSERT( p.type.mode = this.type.mode );
    IF this.mode = pc.ob_var
      THEN GenExpr (p, NIL);
      ELSE GenDesignator (p, FALSE, U_RD);
    END;
    IF MakeReusable (n(*=>*), p, ru_this, this.mode = pc.ob_varpar) THEN
    <* IF DB_TRACE THEN *>
      IF EnDebug THEN dbg.CallTraceInd (-1, ")GenCall reuse => TRUE") END;
    <* END *>
      RETURN TRUE;
    END;
    o := this.next;
  ELSE
    (* hbz for global vars !!!!! *)
    nestedThrowsCall:=nestedThrowsCall OR (pc.ttag_throws IN n.l.type.tags);
    o := n.l.type.prof;
  END;
  WHILE o # NIL DO
    ASSERT(   (o.mode = pc.ob_var)
           OR (o.mode = pc.ob_varpar)
           OR (o.mode = pc.ob_seq) );
    ASSERT( l # NIL );
    IF o.mode = pc.ob_varpar THEN
      GenDesignator (l, TRUE, U_VP);
<* IF TARGET_MEGOS THEN *>
    IF (l.mode = pc.nd_index) & (l.l.type.mode = pc.ty_SS) THEN -- FS 26.5
      INCL (l.tags, ntag_lhs);
    END;
<* END *>
    ELSIF IsSStoArrConv (l) THEN
      GenExpr (l.l, NIL);    -- do not calculate this conversion
    ELSE
      GenExpr (l, NIL);
    END;
    IF MakeReusable (n(*=>*), l, ru_prm, o.mode=pc.ob_varpar) THEN
    <* IF DB_TRACE THEN *>
      IF EnDebug THEN dbg.CallTraceInd (-1, ")GenCall reuse => TRUE") END;
    <* END *>
      RETURN TRUE;
    END;
    l := l.next;
    o := o.next;
  END;
  ASSERT( l = NIL );
  IF hbz THEN
    ValsWorstSetU (CurProps);
  ELSE
    WHILE u # NIL DO
      ObjectUsage (n.pos, u.obj, u.tags, NIL);
      u := u.next;
    END
  END;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenCall => n=%X", n) END;
<* END *>
  RETURN FALSE;
END GenCall;

--------------------------------------------------------------------------------
-- Traverse an indexation: agg[ind]
-- n.l - aggregate, n.r - index
-- In En???: range checking & calculation.
PROCEDURE GenIndex (     n: pc.NODE   -- nd_index
                   ; desig: BOOLEAN   -- value/designator position
                   ;  addr: BOOLEAN   -- T: ptr to a designated obj is created
                   ;  tags: pc.UTAG_SET );    -- usage tags
VAR v: pc.VALUE;
    l: pc.NODE;
    i: LONGINT;

BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (1, "(GenIndex: n=%X desig=%b addr=%b", n, desig, addr) END;
<* END *>

  ASSERT( n.l.type.inx.mode = n.r.type.mode );  -- index type check

  IF MakeReusable (n(*=>*), n.l, ru_prm, desig) THEN
    IF desig
      THEN GenDesignator (n, addr, tags);       --> l-value agg[ind]
      ELSE GenExpr (n, NIL);                    --> r-value agg[ind]
    END;
  <* IF DB_TRACE THEN *>
    IF EnDebug THEN dbg.CallTraceInd (-1, ")GenIndex - reuse") END;
  <* END *>
    RETURN;
  END;

  IF desig
    THEN GenDesignator (n.l, addr, tags);      --> l-value agg
    ELSE GenExpr (n.l, NIL);                   --> r-value agg
  END;
  GenExpr (n.r, NIL);                           --> ind

(*@ IF ~MOD THEN RETURN END; -- else do const.prop and values check *)

  IF n.r.mode = pc.nd_value (*3/ n.r.val # NIL*) THEN -- Ind is constant
    IF n.l.mode = pc.nd_value (*3/ n.l.val # NIL*) THEN -- Agg is constant
      vIntTmp.binary (pc.sb_minus, n.r.val, n.l.type.min(*3 .val*));
      v := pc.value.new(n.pos, n.type);
(*3/  NewValue (n); *)
      v (*3/n.val*).index_get (vIntTmp.get_integer (), n.l.val); -- [M] replace indexing by value
      SetValue (n, v ); (*3/ *)
    ELSE                                     -- Agg is non-constant:
      IF n.l.type.mode = pc.ty_array THEN      -- v := max.value / NIL
        v := n.l.type.max(*3.val*);
      ELSIF n.l.type.mode = pc.ty_SS THEN
        vIntTmp.set_integer (n.l.type.len-1);
        v := vIntTmp;
      ELSE
        v := NIL;
      END;
      IF CmpValue (pc.sb_lss, n.r.val, n.l.type.min(*3.val*))  -- check v in min..max
      OR
<* IF TARGET_MEGOS THEN *>
         ~( (n.l.type.mode = pc.ty_SS) &
            (pc.ttag_packed IN n.l.type.tags) ) &
<* END *>
         (v # NIL) &
         CmpValue (pc.sb_gtr, n.r.val, v)
      THEN
--        Err (n.pos, 122);                        -- "expr out of bounds"
      ELSIF n.l.mode = pc.nd_aggregate THEN    -- aggregate: calc 'agg[ind]'
        vIntTmp.binary (pc.sb_minus, n.r.val, n.l.type.min(*3 .val*)); -- = ind-min
        i := vIntTmp.get_integer ();
        l := n.l.l;
        WHILE i > 0 DO
          l := l.next;
          DEC (i)
        END;
        n.copy (l);                            -- [M] replace indexing by value
      ELSIF (v # NIL)
         OR CmpValue (pc.sb_equ, n.r.val, n.l.type.min(*3 .val*))
      THEN
        EXCL (n.tags, pc.ntag_chk_range);      -- mark range check done
      END;
    END;
  END;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenIndex") END;
<* END *>
END GenIndex;

-------------------------------------------------------------------------
-- Traverse a pointer dereference operation: ptr^
-- n.l - pointer
-- In En???: set checking flags, give warnings.
PROCEDURE GenDeref ( n: pc.NODE );    -- nd_deref
BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (1, "(GenDeref: n=%X", n) END;
<* END *>

  GenExpr (n.l, NIL);
<* IF TARGET_MEGOS THEN *>
  IF n.l.type.mode = pc.ty_SS THEN
    n.copy (n.l);
    RETURN;
  END;
<* END *>

  ASSERT( n.l.type.mode = pc.ty_pointer );

(*@ IF ~MOD THEN RETURN END; -- else do const.prop and values check *)

  CASE n.l.mode OF
  |pc.nd_guard:
    EXCL (n.tags, pc.ntag_chk_range);

  |pc.nd_var:
    IF ValsChkNil (n.l.obj)          -- if NIL chk for object required here,
      THEN ValsClrNil (n.l.obj);     -- then initiate check-free path
      ELSE EXCL (n.tags, pc.ntag_chk_range); -- if on check-free path, omit check
    END;

  |pc.nd_value:   (*3/ *)
    IF n.l.val.is_zero () THEN
      Wrn (n.pos, 315);                 -- "NIL dereference"
<* IF TARGET_IDB THEN *>
        IF env.InterViewMode THEN model2.nil_deref(n); END;
<* END *>
    END;
  ELSE
(*3/
    IF (n.l.val # NIL) & n.l.val.is_zero () THEN
      Wrn (n.pos, 315);                 -- "NIL dereference"
    END;
*)
  END;
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenDeref") END;
<* END *>
END GenDeref;

-------------------------------------------------------------------------

PROCEDURE GenGuard (     n: pc.NODE     -- nd_eguard/nd_guard
                   ; desig: BOOLEAN     -- value/designator position
                   ;  addr: BOOLEAN     -- T: ptr to a designated obj is created
                   ;  tags: pc.UTAG_SET );      -- usage tags
(**
   Traverse a record/ptr guard operation: lval(dtype)
   n.l - pointer/record, n.type - destination type
   Do type calculation and checks reducing
   (works in optimize-off modes, as generally useful)
*)
  PROCEDURE IsTypeGtr ( x, y: pc.STRUCT
                      )    :            BOOLEAN;
  (**
     Checks if 'y' is a supertype for 'x'.
     Applied for records or pointers to records
  *)
  BEGIN
    IF x.mode = pc.ty_pointer THEN      -- bypass pointers
      x := x.base;
      y := y.base;
    END;
    ASSERT( x.mode = pc.ty_record );    -- must be objects (records)
    ASSERT( y.mode = pc.ty_record );
    WHILE (x # NIL) & (x # y) DO
      x := x.base
    END;
    RETURN x = y;
  END IsTypeGtr;
  -----------------------------------------------

VAR t: pc.STRUCT;

BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (1, "(GenGuard: n=%X", n) END;
<* END *>

  IF desig
    THEN GenDesignator (n.l, addr, tags);       --> l-value rec(dtype)
    ELSE GenExpr (n.l, NIL);                    --> r-value rec(dtype)
  END;

  (* Do type calculations and reducing of checks and casting *)

  IF n.l.type.mode = pc.ty_pointer THEN
    ASSERT( n.l.type.base.mode = pc.ty_record );
  ELSIF n.l.mode = pc.nd_deref THEN
    ASSERT( n.l.type.mode = pc.ty_record );
  ELSE
    t := n.l.dynamic_type ();               -- get actual type of lval
    IF t = NIL THEN
      ASSERT( n.l.mode = pc.nd_var );
    ELSE
      IF t = n.type THEN
        IF t = n.l.type                         -- dest.type match actual:
          THEN n.copy (n.l);                    -- [M]: remove guard node
          ELSE EXCL (n.tags, pc.ntag_chk_range);-- else assume no NIL check
        END;
      ELSIF (n.mode = pc.nd_guard)
          & IsTypeGtr (t, n.type)          -- if guard to supertype
      THEN
        EXCL (n.tags, pc.ntag_chk_range);  --  assume no NIL check
      END;
    END;
  END;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenGuard") END;
<* END *>
END GenGuard;

-------------------------------------------------------------------------
PROCEDURE GenAssign ( VAR n,e: pc.NODE          -- nd_assign
                    )        :         BOOLEAN; -- T: re-analyse from 'n' rquired
(**
   Traverse an assignment stmt LHS := RHS.
   Either n.l - LHS designator xor n.obj - simple LHS var; n.r - RHS
   ('VAR n' actual is usually a link position in some node: changing it
   causes tree restructuring)
*)
BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (1, "(GenAssign: n=%X", n) END;
<* END *>

  IF n.l # NIL THEN                   -- LHS is lval designator?
    ASSERT( n.obj = NIL );            -- exclude object spec.
    GenDesignator (n.l, FALSE, U_WR); -->> Traverse LHS
    IF n.l.mode = pc.nd_var THEN      -- if simple var resulted,
      n.obj := n.l.obj;               -- move it to object position
      n.l   := NIL;
    END;
  END;

  IF IsSStoArrConv (n.r)
   THEN GenExpr (n.r.l, NIL);    -- do not calculate this conversion
   ELSE GenExpr (n.r, NIL);
  END;

  IF MakeReusable (n(*=>*), n.r, ru_assign, FALSE)  -- reusability check for
  OR (n.l # NIL) & MakeReusable (n(*=>*), n.l, ru_assign, TRUE) -- both sides
  THEN
  <* IF DB_TRACE THEN *>
    IF EnDebug THEN dbg.CallTraceInd (-1, ")GenAssign reuse => TRUE,n=%X", n) END;
  <* END *>
    e := n; (*3? *)
    RETURN TRUE;                 -- exit, as retraverse will do
  END;                           -- rest of analysis

<* IF TARGET_MEGOS THEN *>
  IF (n.l # NIL) &
     (n.l.mode = pc.nd_index) &
     (n.l.l.type.mode = pc.ty_SS) THEN -- FS 26.5
    INCL (n.l.tags, ntag_lhs);
  END;
<* END *>

(*@ IF ~MOD THEN RETURN FALSE END; -- else do const.prop and values check *)

  IF n.r.mode = pc.nd_value (*3/n.r.val # NIL*) THEN
    CheckValueRange (n.r);
  END;

  IF n.l = NIL THEN              -- Assignment to a simple var:
    IF n.r.mode = pc.nd_value (*3/ n.r.val # NIL *) THEN        -- RHS is a constant value
      ObjectUsage (n.pos, n.obj, U_WR, n.r.val);

    ELSIF (n.r.mode = pc.nd_unary)         -- RHS is a type conversion
        & (n.r.sub = pc.su_conv)
        & (n.type.mode = pc.ty_void)
    THEN
      ObjectUsage (n.pos, n.obj, U_WR, NIL);

    ELSE                                   -- RHS is arbitrary expression
      ObjectUsage (n.pos, n.obj, U_WR, NIL);

      IF (n.r.mode  = pc.nd_binary)        -- case var := var +- i :
       &((n.r.sub   = pc.sb_plus) OR (n.r.sub = pc.sb_minus))
       & (n.r.l.mode= pc.nd_var)
       & (n.r.l.obj = n.obj)
       &  n.obj.type.is_ordinal()
      THEN
        IF n.r.sub = pc.sb_plus            -- replace by (pre) INC/DEC(var,i)
          THEN n.r.sub := pc.sb_pre_inc;   -- (now in RHS)
          ELSE n.r.sub := pc.sb_pre_dec;
        END;
        IF pc.ntag_chk_overflow IN n.r.tags THEN -- preserve range check
          EXCL (n.r.tags, pc.ntag_chk_overflow);
          INCL (n.r.tags, pc.ntag_chk_range);
        END;
        IF n.type.mode = pc.ty_void THEN   -- if nd_assign is in stmt position
          n.mode := pc.nd_eval;            -- insert nd_eval, as inc/dec are
          n.l    := n.r;                   -- binary operations with result
          n.r    := NIL;
          n.obj  := NIL;
        ELSE                               -- if in expr.position,
          n.copy (n.r);                    -- just move to n
        END;
      END;
    END;
  END;
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenAssign => FALSE, n=%X", n) END;
<* END *>
  RETURN FALSE;
END GenAssign;


(*========================== EXPRESSIONS ANALYSIS ===========================*)

TYPE LOOPINF = POINTER TO LOOPINF_REC;

-----------------------------------------------------------------------------

PROCEDURE ^ GenSequence ( VAR n: pc.NODE
                        ;    li: LOOPINF
                        )      :         BOOLEAN;

-----------------------------------------------------------------------------

PROCEDURE FieldIdx ( recType: pc.STRUCT
                   ;    fobj: pc.OBJECT
                   )        :          LONGINT;
(*
  Returns field idx or -1 if not found (or variant record);
*)
VAR cancel: BOOLEAN;

  PROCEDURE record (     tt: pc.STRUCT
                   ; VAR no: LONGINT
                   )       :          BOOLEAN;
  VAR o: pc.OBJECT;
  BEGIN
    ASSERT( tt.mode = pc.ty_record );
    IF tt.base # NIL THEN
      Fault (fobj.pos, 108);   --FSA: bug in inherited-records constructor, CASE is not allowed
      (*
      IF    record (tt.base, no) THEN
        RETURN TRUE;
      ELSIF cancel THEN
        RETURN FALSE;
      END;
      *)
    END;

    o := tt.prof;
    WHILE o # NIL DO
      IF o.mode = pc.ob_field THEN
        IF o = fobj THEN RETURN TRUE END;
        INC(no);
      ELSE
        cancel := TRUE;
        RETURN FALSE;
      END;
      o := o.next;
    END;
    RETURN FALSE;
  END record;

VAR no: LONGINT;

BEGIN
  no     := 0;
  cancel := FALSE;
  IF record(recType, no) THEN
    RETURN no;
  ELSE
    RETURN -1;
  END;
END FieldIdx;


--------------------------------------------------------------------------------
-- 1st main analysis routine:
-- Traverse r-value; try to calculate it to a value
-- (this is its main difference from GenDesignator)
PROCEDURE GenExpr ( n: pc.NODE
                  ; t: pc.STRUCT );
VAR vals, val0: VALUES;
            e: pc.NODE;   (*3/ *)
            v: pc.VALUE;  (*3/ *)
            i: LONGINT;
BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN
    dbg.CallTraceInd (1, "(GenExpr: n=%X t=%X .mode=", n, t);
    dbg.Trace(dbg.VisNodMode [n.mode]);
  END;
<* END *>

  ASSERT( n # NIL );
  IF n.type = NIL THEN    -- Int.error: node=NIL
    Fault (n.pos, 106)
  END;
  ASSERT( n.type.mode < pc.ty_free ); -- type maybe void for inlined functions

  IF n.type.obj # NIL THEN
    n.type := n.type.obj.type
  END;

  CASE n.mode OF
  | pc.nd_value:
      ValueUsage (n.val, n.type);
      CheckValueRange (n);

  | pc.nd_var:
      ASSERT( (n.l = NIL) & (n.r = NIL) );
      ObjectUsage (n.pos, n.obj, U_RD, NIL);
      IF (n.obj.mode = pc.ob_cons)
       & EnConsProp (*3/ *) THEN
        IF n.obj.val.mode = pc.nd_value (*3/ n.obj.val.val # NIL*) THEN
          SetValueCast (n, n.obj.val.val);
        ELSE
          n.copy(n.obj.val);  (*3/ ELSE part: @12 *)
          GenExpr(n,t);
        END;
      ELSE
        n.val := GetValue (n.pos, n.obj<* IF TARGET_IDB THEN *>, n <* END *>);
        IF n.val # NIL THEN      (* Vit: maybe leave nd_var, but set .val *)
          n.mode := pc.nd_value; (*      and interpret it as value, if needed*)
          n.obj  := NIL;         (* (just a proposal to think later) *)
        END;
      END;

  | pc.nd_binary:
      GenExprBinary (n);

  | pc.nd_unary:
      GenExprUnary (n);

  | pc.nd_proc:
      ObjectUsage (n.pos, n.obj, U_RD, NIL);
      IF EnConsProp THEN    (*3/ do unconditionally *)
  v := pc.value.new(n.pos, n.type);
  (*3/  NewValue (n); *)                 -- Vit: what for???
        v (*3/n.val*).set_object (n.obj);
        SetValue (n, v ); (*3/ *)
      END;
      ASSERT( (n.l = NIL) & (n.r = NIL) );

  | pc.nd_method:
      ASSERT( n.obj.mode IN pc.PROCs );

  | pc.nd_type:
      ASSERT( (n.l = NIL) & (n.r = NIL) );

  | pc.nd_field: --*FSA
      GenExpr (n.l, NIL);
      IF (n.obj # NIL) AND (n.obj.mode = pc.ob_field) AND
         (n.l.mode = pc.nd_value) AND (n.l.type.mode = pc.ty_record) AND
         (n.l.val # NIL) AND (n.l.type.base = NIL)
      THEN
        i := FieldIdx (n.l.type, n.obj);
        IF (i >= 0) THEN
          v := pc.value.new (n.pos, n.obj.type);
          v.index_get (i, n.l.val);
          SetValue (n, v);
        END;
      END

  | pc.nd_lconv:
      GenExpr (n.l, NIL);
      IF (n.l.mode = pc.nd_value) AND (n.type.mode IN pc.WHOLEs) AND (n.l.type.mode IN pc.SETs) THEN
        v := pc.value.new(n.pos, n.type);
        IF n.type.mode IN pc.INTs THEN
          v.unary (pc.su_conv, n.l.val);
        ELSE -- IN CARDs
          v.unary (pc.su_cast, n.l.val);
        END;
        SetValue (n, v);
      END;

  | pc.nd_index:
      GenIndex (n, FALSE, FALSE, U_RD);

  | pc.nd_deref:
      GenDeref (n);

  | pc.nd_eguard
  , pc.nd_guard:
      GenGuard (n, FALSE, FALSE, pc.UTAG_SET{});

  | pc.nd_aggregate:
      IF n.type.mode IN pc.SETs THEN
        GenSetAggr (n);
      ELSIF n.type.mode IN pc.TY_SET{pc.ty_array, pc.ty_record} THEN
        GenArrRecAggr (n);
      ELSIF GenExprList (n(*=>*), n.l) THEN
        GenExpr (n, NIL);
      END;

  | pc.nd_sequence:
      IF GenExprList (n(*=>*), n.l) THEN
        GenExpr (n, NIL);
      END;

  | pc.nd_call:
      IF GenCall (n(*=>*)) THEN
        GenExpr (n, NIL);
      END;
      ASSERT( n.type.mode # pc.ty_void);

  | pc.nd_assign:
      e := n.next; (*3/? *)
      IF GenAssign (n(*=>*),e(*3/*)) THEN
        GenExpr (n, NIL);
      END;

  | pc.nd_replace :
      GenExpr (n.l, NIL);
      GenExpr (n.r, NIL);
      RemoveExpr (n.l(*=>*));                      (* IRMOD *)
      IF n.l = NIL THEN n.copy (n.r) END;          (* IRMOD *)

  | pc.nd_if:
      ASSERT( n.r.mode = pc.nd_node);
      ASSERT( n.type.mode = n.r.l.type.mode);
      ASSERT( n.type.mode = n.r.r.type.mode);
      GenExpr (n.l, NIL);
      IF EnStatOpt & (n.l.mode = pc.nd_value) (*3/ n.l.val # NIL*) THEN
        IF n.l.val.is_zero() THEN
          GenExpr (n.r.r, NIL);
          n.copy (n.r.r)                            (* IRMOD *)
        ELSE
          GenExpr (n.r.l, NIL);
          n.copy (n.r.l)                            (* IRMOD *)
        END;
      ELSE
        ValsCopy (CurProps, val0);
        GenExpr (n.r.l, NIL);
        vals := CurProps;
        CurProps := val0;
        GenExpr (n.r.r, NIL);
        ValsAppend (vals, CurProps);
      END;

  ELSE
    IF pc.code.en_f_inline
      THEN SYSTEM.EVAL(GenSequence (n(*=>*), NIL));
      ELSE Fault (n.pos, 103);    -- "Int.error: value expected"
    END;
  END;
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenExpr") END;
<* END *>
END GenExpr;

--------------------------------------------------------------------------------
-- 2nd main analysis routine:
-- Traverse l-value (designator); doesnt try to calculate it to a value
-- (this is its main difference from GenExpr)
PROCEDURE GenDesignator (    n: pc.NODE
                        ; addr: BOOLEAN  -- T: ptr to a designated obj is created
                        ; tags: pc.UTAG_SET );   --    with SYSTEM.ADR or by VAR-param
BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN
    dbg.CallTraceInd (1, "(GenDesignator: n=%X addr=%b .mode=", n, addr);
    dbg.Trace(dbg.VisNodMode [n.mode]);
  END;
<* END *>

  IF n.type = NIL THEN
    Fault (n.pos, 103);     -- "Int.error: value expected"
  END;
  IF n.type.obj # NIL THEN
    n.type := n.type.obj.type;
  END;

  IF n.mode = pc.nd_binary THEN -- possible in desig, if concatenate in C-call (bug523)
    GenExprBinary (n);
  END;

  CASE n.mode OF
  | pc.nd_var:
      ObjectUsage (n.pos, n.obj, tags, NIL);
      ASSERT( ~addr OR ~(pc.otag_no_aliases IN n.obj.tags) );

  | pc.nd_value:
      ASSERT( n.type.mode = pc.ty_SS );
      ASSERT( addr );

  | pc.nd_proc:
      ObjectUsage (n.pos, n.obj, tags, NIL);
      ASSERT( ~addr );

  | pc.nd_type: (* nothing *)

  | pc.nd_field:
      GenDesignator (n.l, addr, tags);

  | pc.nd_index:
      GenIndex (n, TRUE, addr, tags);

  | pc.nd_deref:
      GenDeref (n);

  | pc.nd_eguard
  , pc.nd_guard:
      GenGuard (n, TRUE, addr, tags);

  | pc.nd_lconv:
      GenDesignator (n.l, addr, tags);

  | pc.nd_replace:
      GenExpr (n.l, NIL);
      GenDesignator (n.r, addr, tags);
      RemoveExpr (n.l(*=>*));                  (* IRMOD *)
      IF n.l = NIL THEN n.copy (n.r) END;

  ELSE
    Fault (n.pos, 104);         -- "Int.error: designator expected"
  END;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenDesignator") END;
<* END *>
END GenDesignator;


(*========================== Statements ================================*)

TYPE
  LOOPINF_REC = RECORD
    prev : LOOPINF;
    node : pc.NODE;
    exits: ARRAY 4 OF pc.NODE; -- links to nd_exit nodes within cur.loop
    excnt: LONGINT;  -- >0: nodes sequence contains EXIT from a node loop,
                     -- (if node.mode=nd_loop & exit # NIL then control flow
                     --  may reach statement node.next)
    rtrn : BOOLEAN;  -- nodes sequence contains RETURN from block node
    esc  : BOOLEAN;  -- control flow some way exits node,
    vals : VALUES;   -- but not obligatory comes to node.next
  END;

--------------------------------------------------------------------------------

PROCEDURE ^ GenStatement ( VAR n,e: pc.NODE
                         ;      li: LOOPINF
                         );

--------------------------------------------------------------------------------

PROCEDURE GenSequence ( VAR n: pc.NODE
                      ;    li: LOOPINF
                      )      :         BOOLEAN; -- is dead?
(**
   Traverse a node sequence.
   ('VAR n' actual is usually a link position in some node: changing it
   causes tree restructuring)
*)

VAR t,e: pc.NODE;

BEGIN
  IF n = NIL THEN RETURN FALSE END;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (0, "GenSequence: n=%X", n) END;
<* END *>

  REPEAT
    GenStatement (n(*=>*), e(*=>*), li);
  UNTIL (n = NIL) OR (n # e);
  IF n = NIL THEN RETURN FALSE END;

  t := n;
  WHILE t.next # e DO
    t := t.next
  END;

  LOOP
    IF t.next = NIL THEN EXIT END;
    REPEAT
      GenStatement (t.next(*=>*), e(*=>*), li);
    UNTIL (t.next = NIL) OR (t.next # e);
    IF e = NIL THEN EXIT END;
    WHILE t.next # e DO
      t := t.next
    END;
  END;
  WHILE t.next # NIL DO
    t := t.next
  END;
  RETURN pc.ntag_no_exit IN t.tags;
END GenSequence;

-------------------------------------------------------------------------

PROCEDURE OpenDeadLoop ( VAR n: pc.NODE    -- nd_loop
                       ;    ln: LOOPINF );
(**
   Replace a loop by its body sequence.
*)
VAR l: pc.NODE;
    i: LONGINT;

BEGIN
  Wrn (n.pos, 312);                      -- "loop executed exactly once"

  IF ln.excnt > 0 THEN                   -- if there are internal EXITs:
    IF ln.excnt > LEN(ln.exits) THEN RETURN END; -- but too many, do nothing
    IF (n.next = NIL)                    -- if .next is not label,
    OR (n.next.mode # pc.nd_label)       -- create one
    THEN
      l := NewNode (env.null_pos, pc.nd_label, n.type);
      l.next := n.next;
      n.next := l;
    END;
    FOR i := 0 TO ln.excnt-1 DO          -- turn all internal nd_exits
      l := ln.exits[i];                  -- to nd_goto n.next
      l.mode := pc.nd_goto;
      l.l := n.next;
      l.r := NIL;
    END;
  END;

  Append (n.r (*=>*), n.next);
  n := n.r;
END OpenDeadLoop;

-------------------------------------------------------------------------

PROCEDURE SprocVarArg ( sp : pc.SUB_MODE; i: INTEGER          -- proc number, ??
                      )     :         BOOLEAN;
(*
*)
BEGIN
  CASE sp OF
  | pc.sp_new
  , pc.sp_sysnew
  , pc.sp_incl
  , pc.sp_excl:
      RETURN i = 0;
  | pc.sp_copy
  , pc.sp_get
  , pc.sp_getreg :
      RETURN i = 1;
  ELSE
      RETURN FALSE;
  END;
END SprocVarArg;

-------------------------------------------------------------------------

PROCEDURE GenBooleanValue (      n: pc.NODE
                          ; VAR vf: VALUES
                          );
(*
*)
VAR ve,vx: VALUES;

BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (1, "(GenBooleanValue: n=%X", n) END;
<* END *>

  INC (StatCnt);
  IF (n.mode = pc.nd_binary) & (n.sub = pc.sb_cand) THEN
    GenBooleanValue (n.l, vf);
    GenBooleanValue (n.r, ve);
    ValsAppend (ve, vf);
    IF ~EnConsProp THEN
    ELSIF n.l.mode = pc.nd_value (*3/ n.l.val # NIL*) THEN
      IF n.l.val.is_zero ()
        THEN n.copy (n.l);
        ELSE n.copy (n.r);
      END;
    ELSIF n.r.mode = pc.nd_value (*3/ n.r.val # NIL*) THEN
      IF ~n.r.val.is_zero () THEN
        n.copy (n.l);
      END;
    END;
  ELSIF (n.mode = pc.nd_binary) & (n.sub = pc.sb_cor) THEN
    GenBooleanValue (n.l, vx);
    ve := CurProps;
    CurProps := vx;
    GenBooleanValue (n.r, vf);
    ValsAppend (ve, CurProps);
    IF ~EnConsProp THEN
    ELSIF n.l.mode = pc.nd_value (*3/ n.l.val # NIL*) THEN
      IF ~n.l.val.is_zero ()
        THEN n.copy (n.l);
        ELSE n.copy (n.r);
      END;
    ELSIF n.r.mode = pc.nd_value (*3/ n.r.val # NIL*) THEN
      IF n.r.val.is_zero () THEN
        n.copy (n.l);
      END;
    END;
  ELSE
    GenExpr  (n, NIL);
    ValsCopy (CurProps, vf);
  END;
  INC (StatCnt);

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenBooleanValue") END;
<* END *>
END GenBooleanValue;

-------------------------------------------------------------------------

PROCEDURE GenStatement ( VAR n: pc.NODE    -- current stmt node
                       ; VAR e: pc.NODE    -- => node to continue from
                       ;    li: LOOPINF );
(**
   Traverse a statement.
   Nodes that are not available to control flow are never traversed.
   Such nodes are eliminated with warning. @: but in MOD mode?
   That is, all transformations may be done in one pass along the tree.
   ('VAR n' actual is usually a link position in some node: changing it
   causes tree restructuring)
*)
VAR ln: LOOPINF;

  -------------------------------------
  PROCEDURE SetLn;
  (*
  *)
  BEGIN
    ln.prev := li;
    ln.node := n;
    ln.excnt:= 0;
    ln.rtrn := FALSE;
    ln.esc  := FALSE;
    ln.vals := NIL;
  END SetLn;

  -------------------------------------
  PROCEDURE Remove ( VAR n: pc.NODE );  -- =>start, end.next=> of unreach.code
  (**
     Remove unreachable code starting from 'n'.
     (removing means just setting invalid modes)
     ('VAR n' actual is usually a link position in some node: changing it
     causes tree restructuring)
  *)
  VAR  s: pc.NODE;
      md: pc.ND_MODE;
  BEGIN
    IF n = NIL THEN RETURN END;
    s := n;                            -- skip unreachable code until:
    md := n.mode;
    WHILE (s # NIL)                                        -- end of branch
        & (s.mode # pc.nd_label)                           -- some label
        & ((s.mode # pc.nd_sproc) OR (s.sub # pc.sp_code)) -- built-in code
    DO
      s.mode := pc.nd_last;            -- mark as invalid node, just for sure
      s := s.next;
    END;
    IF ( s # n)                        -- warning, if not empty code
     & (md # pc.nd_ftrap)              --   or TRAP/ACTIVATE blocks
     & (md # pc.nd_wtrap)
     & (md # pc.nd_activate)
     & ~(pc.ntag_constrinlined IN n.tags)
    THEN
      Wrn (n.pos, 311);                -- "unreachable code"
  <* IF TARGET_IDB THEN *>
      IF env.InterViewMode THEN model2.warn_at_pos(n.pos); END;
  <* END *>
    END;
    n := s;                            -- return start of new branch
  END Remove;

  -------------------------------------
  PROCEDURE CaseVariant (   VAR n: pc.NODE
                        ;      li: LOOPINF
                        ;     vfr: VALUES
                        ; VAR vto: VALUES
                        )        :        BOOLEAN; -- is dead-end
  (**
     ('VAR n' actual is usually a link position in some node: changing it
      causes tree restructuring)
  *)
  BEGIN
    ValsAssign (vfr, CurProps);
    IF GenSequence (n(*=>*), li) THEN
      IF vto = NIL THEN ValsCopy (vfr, vto) END;
      RETURN TRUE;
    END;
    IF vto = NIL
      THEN ValsCopy   (CurProps, vto);  -- 1st : nothing to join with
      ELSE ValsAppend (CurProps, vto);  -- next: join with accumulator
    END;
    RETURN FALSE;
  END CaseVariant;
  -------------------------------------

  PROCEDURE has_sideeffect (n: pc.NODE): BOOLEAN;
  BEGIN
    LOOP
      IF (n = NIL) THEN RETURN FALSE END;
      CASE n.mode OF
      | pc.nd_assign
      , pc.nd_call
      , pc.nd_for:
          RETURN TRUE;
      | pc.nd_binary:
          IF (n.sub = pc.sb_pre_inc)
          OR (n.sub = pc.sb_pre_dec)
          OR (n.sub = pc.sb_post_inc)
          OR (n.sub = pc.sb_post_dec)
          THEN
            RETURN TRUE;
          ELSE
            IF has_sideeffect(n.l) OR has_sideeffect(n.r) THEN RETURN TRUE; END;
          END;
      | pc.nd_return:
          IF (has_sideeffect(n.l)) THEN RETURN TRUE; END;
      | pc.nd_exit:
      | pc.nd_goto:
      ELSE
        IF (has_sideeffect(n.l) OR has_sideeffect(n.r)) THEN RETURN TRUE; END;
      END;
      n := n.next;
    END;
  END has_sideeffect;
  -------------------------------------


VAR      l,m,k: pc.NODE;
             p: pc.NODE;
             i: LONGINT;
       nxt,stp: pc.VALUE;
    dead,dead1: BOOLEAN;
           lvl: BOOLEAN;
          ok,b: BOOLEAN;
         vals1: VALUES;
         vals2: VALUES;
         vals3: VALUES;
loc_insideInline: BOOLEAN;

BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN
    dbg.CallTraceInd (1, "(GenStatement n=%X .mode=", n); dbg.Trace(dbg.VisNodMode [n.mode]);
  END;
<* END *>

  loc_insideInline := insideInline;
  IF pc.ntag_constrinlined IN n.tags THEN
    insideInline:=TRUE;
  END;

  ASSERT( n.type.mode = pc.ty_void );
  INC (StatCnt);
  INC (StatCnt1);
  e := n.next;

  CASE n.mode OF
  | pc.nd_call:
      IF GenCall (n(*=>*)) THEN              -- maybe inlined!
        IF ~Restart THEN e := n END;         -- re-traverse needed (*what is n ELSE? *)
      ELSIF pc.ntag_no_exit IN n.tags THEN   -- no continuation:
        IF EnStatOpt THEN
          Remove (n.next(*=>*));             --   remove next stmts
        END;
        e := n.next;                         --   continue from label/NIL
      END;

  | pc.nd_assign:
      IF GenAssign (n(*=>*),e(*3/*)) THEN    -- if splitting done,
(*3     THEN e := n;                         --   re-traverse
    ( * ELSE e := n.next; *)
      END;

  | pc.nd_while: (* n.l - cond.expr, n.r - body *)
      NEW (ln);
      ValsCopy (CurProps, vals1);
      ValsWorstClrU (CurProps);
      ValsCopy (CurProps, vals2);
      LOOP                              -- Iteration loop by properties:
        GenExpr (n.l, NIL);
        IF EnStatOpt &
          (n.l.mode = pc.nd_value) (*3/n.l.val # NIL*) THEN -- termination by const.condition
          ASSERT( ValsEqu (CurProps, vals2) );-- (actually, mb on 1st pass only)
          CurProps := vals1;
          IF n.l.val.is_zero () THEN        -- WHILE FALSE:
            Remove (n.r(*=>*));             --   delete whole stmt
            n := n.next;                    -- next node becomes this
          ELSE                              -- WHILE TRUE:
            n.l := NIL;                     --   turn to LOOP
            n.mode := pc.nd_loop;
          END;
          e := n;                           -- re-traverse
          EXIT;
        END;

        SetLn;
        ValsCopy (CurProps, ln.vals);       -- process body and merges properties
        dead := GenSequence (n.r(*=>*), ln);
        IF dead                             --   no continuation
          THEN ValsAssign  (vals1, CurProps);
          ELSE ValsReplace (vals1, CurProps);
        END;

        IF ValsEqu (vals2, CurProps) THEN   -- term-n by prop-s stabilization
          IF dead & (ln.excnt = 0) THEN                 -- if no cont-n & exots,
            l   := NewNode (n.pos, pc.nd_node, n.type); -- convert to IF-stmt
            l.l := n.r;
            n.r := l;
            n.mode := pc.nd_if;
          END;
          CurProps := ln.vals;
          ValsAppUsage (vals1, CurProps);
          EXIT;
        END;
        ValsAssign (CurProps,  vals2);
      END;

  | pc.nd_repeat:
      NEW (ln);
      ValsCopy (CurProps, vals1);
      ValsWorstClrU (CurProps);
      ValsCopy (CurProps, vals2);
      ValsCopy (CurProps, vals3);
      LOOP
        SetLn;
        dead := GenSequence (n.l(*=>*), ln);
        IF dead THEN
          ValsAssign (vals1,  CurProps);
        ELSE
          GenExpr (n.r, NIL);
          ValsAssign  (CurProps,  vals3);
          ValsReplace (vals1, CurProps);
        END;
        IF ValsEqu (vals2, CurProps) THEN EXIT END;
        ValsAssign (CurProps, vals2);
      END;
      IF dead & (ln.excnt = 0) THEN
        Remove (n.r(*=>*));
        Remove (n.next(*=>*));
        Append (n.l(*=>*), n.next);
        e := n.next;
        n := n.l;
      ELSIF dead THEN
        CurProps := ln.vals;
        Remove (n.r(*=>*));
        n.mode := pc.nd_loop;
        n.r    := n.l;
        n.l    := NIL;
        OpenDeadLoop (n(*=>*), ln);
      ELSE
        CurProps := vals3;
        IF n.r.mode = pc.nd_value (*3/n.r.val # NIL*) THEN
          IF n.r.val.is_zero () THEN
            n.r := n.l;
            n.l := NIL;
            n.mode := pc.nd_loop;
            INCL (n.tags, pc.ntag_no_exit);
            IF EnStatOpt THEN
              Remove (n.next(*=>*));
            END;
            IF ~ln.esc THEN
              Wrn (n.pos, 310);  -- infinite loop
<* IF TARGET_IDB THEN *>
    IF env.InterViewMode THEN model2.warn_at_pos(n.pos); END;
<* END *>

            END;
            e := n.next;
          ELSE
            Wrn (n.pos, 312); -- loop executed once
            Append (n.l(*=>*), n.next);
            n := n.l;
          END;
        ELSIF n.r.mode = pc.nd_var THEN
          DefValue (n.r.obj, vIntOne);  -- store value 1 in CurProps
        END;
        ValsAppend (ln.vals, CurProps);
        ValsAppUsage (vals1, CurProps); --- LAZ
      END;

  | pc.nd_loop:
      NEW (ln);
      ValsCopy (CurProps, vals1);
      ValsWorstClrU (CurProps);
      ValsCopy (CurProps, vals2);
      LOOP
        SetLn;
        dead := GenSequence (n.r(*=>*), ln);
        IF dead
          THEN ValsAssign  (vals1, CurProps);
          ELSE ValsReplace (vals1, CurProps);
        END;
        IF ValsEqu (vals2, CurProps) THEN EXIT END;
        ValsAssign (CurProps, vals2);
      END;
      IF ~ln.esc THEN
        Wrn (n.pos, 310); -- infinite loop
<* IF TARGET_IDB THEN *>
    IF env.InterViewMode THEN model2.warn_at_pos(n.pos); END;
<* END *>
      END;
      IF ln.excnt = 0 THEN
        INCL (n.tags, pc.ntag_no_exit);
        IF EnStatOpt THEN
          Remove (n.next(*=>*));
        END;
        e := n.next;
      ELSE
        ASSERT( (CurProps # NIL) = (ln.vals # NIL) );
        CurProps := ln.vals;
        ValsAppUsage (vals1, CurProps);
      END;
      IF dead & EnStatOpt THEN
        OpenDeadLoop (n(*=>*), ln);
      END;

  | pc.nd_block:
      NEW (ln);
      SetLn;
      dead := GenSequence (n.r(*=>*), ln);
      IF dead & ~ln.rtrn THEN
        INCL (n.tags, pc.ntag_no_exit);
        IF EnStatOpt THEN
          Remove (n.next(*=>*));
        END;
        e := n.next;
      ELSIF dead THEN
        CurProps := ln.vals;
      ELSE
        IF ln.vals # NIL THEN
          ValsAppend (ln.vals, CurProps)
        END;
      END;

  | pc.nd_exit:
      ASSERT(   (n.r.mode = pc.nd_loop)
             OR (n.r.mode = pc.nd_while)
             OR (n.r.mode = pc.nd_repeat) );
      ASSERT( n.l = NIL );
      ln := li;
      LOOP
        ln.esc := TRUE;
        IF ln.node = n.r THEN
          EXIT
        END;
        ln := ln.prev;
      END;
      i := 0;
      LOOP
        IF i = LEN (ln.exits) THEN
          INC (ln.excnt);
          EXIT;
        ELSIF i = ln.excnt THEN
          ln.exits[i] := n;
          INC (ln.excnt);
          EXIT;
        ELSIF ln.exits[i] = n THEN
          EXIT;
        END;
        INC (i);
      END;
      IF ln.vals = NIL
        THEN ValsCopy (CurProps, ln.vals);
        ELSE ValsAppend (CurProps, ln.vals);
      END;
      INCL (n.tags, pc.ntag_no_exit);
      IF EnStatOpt THEN                   -- optimize IF with EXIT
        Remove (n.next(*=>*));
      END;
      e := n.next;

  | pc.nd_return:
      ln := li;
      LOOP
        ln.esc := TRUE;
        IF ln.node = n.r THEN
          ln.rtrn := TRUE;
          EXIT
        END;
        ln := ln.prev;
      END;
      IF n.l # NIL THEN
        GenExpr (n.l, NIL)
      END;
      IF ln.vals = NIL
        THEN ValsCopy (CurProps, ln.vals);
        ELSE ValsAppend (CurProps, ln.vals);
      END;
      INCL (n.tags, pc.ntag_no_exit);
      IF EnStatOpt THEN                   -- optimize IF with EXIT
        Remove (n.next(*=>*));
      END;
      e := n.next;

  | pc.nd_for:
      l := n.l;
      GenExpr (l.l, NIL);
      GenExpr (l.r, NIL);
      ObjectUsage (n.pos, n.obj, U_WR, NIL);
      IF EnStatOpt
       & (l.l.mode = pc.nd_value (*3/ l.l.val # NIL*))
       & (l.r.mode = pc.nd_value (*3/ l.r.val # NIL*))
      THEN
        stp := l.val;
        IF stp = NIL THEN
          stp := vIntOne
        END;
        nxt := pc.value.new (l.pos, n.obj.type);
        nxt.binary (pc.sb_plus, l.l.val, stp);
        IF   stp.is_neg ()
         &   CmpValue (pc.sb_lss, l.l.val, l.r.val)
        OR ~stp.is_neg ()
         &   CmpValue (pc.sb_gtr, l.l.val, l.r.val)
        THEN
          Remove (n.r(*=>*));
          Wrn (n.pos, 318);  -- redundant FOR stmt
          UndefValue (n.obj);
          n := n.next;
        ELSIF   stp.is_neg ()
            &   CmpValue (pc.sb_lss, nxt, l.r.val)
           OR ~stp.is_neg ()
            &   CmpValue (pc.sb_gtr, nxt, l.r.val)
        THEN
          Wrn (n.pos, 312);    -- loop executed once
          ASSERT( n.obj.mode = pc.ob_var );
          ASSERT( n.obj.attr = NIL );
          ObjectUsage (n.pos, n.obj, U_WR, n.l.l.val);
          SYSTEM.EVAL(GenSequence (n.r(*=>*), li));
          UndefValue (n.obj);
          Append (n.r(*=>*), n.next);
          n.mode := pc.nd_assign;
          n.next := n.r;
          n.r    := n.l.l;
          n.l    := NIL;
        ELSIF n.r = NIL THEN
          Wrn (n.pos, 318);   -- redundant FOR stamt
          UndefValue (n.obj);
          n := n.next;
        ELSE
          ValsWorst (CurProps);
          SYSTEM.EVAL(GenSequence (n.r(*=>*), li));
          UndefValue (n.obj);
        END;
      ELSE
        NEW (ln);
        ValsCopy (CurProps, vals1);
        ValsWorstClrU (CurProps);
        ValsCopy (CurProps, vals2);
        LOOP
          SetLn;
          dead := GenSequence (n.r(*=>*), ln);
          IF dead
            THEN ValsAssign  (vals1, CurProps);
            ELSE ValsReplace (vals1, CurProps);
          END;
          IF ValsEqu (vals2, CurProps) THEN
            EXIT
          END;
          ValsAssign (CurProps, vals2);
        END;
        ValsAppUsage (ln.vals, CurProps);
        ValsAppend (vals1, CurProps);
        UndefValue (n.obj);
      END;

  | pc.nd_with:
      INCL (n.obj.tags, pc.otag_with);
      GenDesignator (n.l, TRUE, U_AD);
      ObjectUsage (n.pos, n.obj, U_WR, NIL);
      IF GenSequence (n.r(*=>*), li) THEN    -- if dead
        INCL (n.tags, pc.ntag_no_exit);
        IF EnStatOpt THEN
          Remove (n.next(*=>*));
        END;
        e := n.next;
      END;

  | pc.nd_ftrap, pc.nd_wtrap:
      INCL (n.tags, pc.ntag_no_exit);
      IF EnStatOpt THEN
        Remove (n.next(*=>*));
      END;
      e := n.next;

  | pc.nd_if:
      ASSERT( n.r.mode = pc.nd_node );
      GenBooleanValue (n.l, vals1);
      (* vals1 - false branch entry *)
      (* CurProps - true branch entry *)
      IF EnStatOpt & (n.l.mode = pc.nd_value) (*3/n.l.val # NIL*) THEN
        i := n.l.val.get_integer ();
        IF i = 0 THEN
          (* eliminate IF FALSE THEN *)
          Remove (n.r.l(*=>*));
          Append (n.r.r(*=>*), n.next);
          n := n.r.r;
          e := n;
          CurProps := vals1;
        ELSE
          (* eliminate IF TRUE THEN *)
          Remove (n.r.r(*=>*));
          Append (n.r.l(*=>*), n.next);
          n := n.r.l;
          e := n;
        END;
      ELSE
        dead := GenSequence (n.r.l(*=>*), li);
        vals2 := CurProps;
        CurProps := vals1;
        (* vals2 - true branch exit *)
        dead1 := GenSequence (n.r.r(*=>*), li);
        IF dead THEN
          -- nothing
        ELSIF dead1 THEN
          CurProps := vals2;
        ELSE
          ValsAppend (vals2, CurProps);
        END;
        IF (n.r.l = NIL) & (n.r.r = NIL) THEN
          IF EnStatOpt THEN                   (* IRMOD *)
            n.mode := pc.nd_eval;
            n.r := NIL;
            e := n;
          END;
        ELSIF dead & dead1 THEN
          INCL (n.tags, pc.ntag_no_exit);
          IF EnStatOpt THEN
            Remove (n.next(*=>*));
          END;
          e := n.next;
        ELSIF dead THEN
          Append (n.r.r(*=>*), n.next);
          n.next := n.r.r;
          n.r.r  := NIL;
        END;
      END;

  | pc.nd_case:
      dead1 := TRUE;
      GenExpr (n.l, NIL);
      m := n.r;
      ASSERT(   (m.mode = pc.nd_casedo)
             OR (m.mode = pc.nd_caselse) );
      IF EnStatOpt & (n.l.mode = pc.nd_value) (*3/n.l.val # NIL*) THEN
        l  := m.l;
        k  := NIL;
        ok := FALSE;
        WHILE l # NIL DO
          ASSERT( l.mode = pc.nd_node );
          p := l.l;
          b := ok;
          WHILE ~ok & (p # NIL) DO
            ASSERT( p.mode = pc.nd_pair );
            IF CmpValue (pc.sb_leq, p(*3 .l*).val, n.l.val)
             & CmpValue (pc.sb_leq, n.l.val, p.l(*3/.r*).val)
            THEN
              k  := l.r;
              ok := TRUE;
            END;
            p := p.next;
          END;
          IF b = ok THEN Remove (l.r(*=>*)) END;
          l := l.next;
        END;
        IF m.mode = pc.nd_caselse THEN
          IF ~ok THEN
            k  := m.r;
            ok := TRUE
          ELSE
            Remove (m.r(*=>*))
          END;
        END;
        IF ~ok THEN Err (n.pos, 208) END; -- CASE stmt always fails
        Append (k(*=>*), n.next);
        n := k;
        e := n;
      ELSE
        ValsCopy (CurProps, vals1);
        vals2 := NIL;
        l := m.l;
        WHILE l # NIL DO
          ASSERT( l.mode = pc.nd_node );
          ASSERT( l.l.mode = pc.nd_pair );
          IF ~CaseVariant (l.r, li, vals1, vals2) THEN
            dead1 := FALSE
          END;
          l := l.next;
        END;
        IF m.mode = pc.nd_caselse THEN
          IF ~CaseVariant (m.r, li, vals1, vals2) THEN
            dead1 := FALSE
          END;
        END;
        CurProps := vals2;
        IF dead1 THEN
          INCL (n.tags, pc.ntag_no_exit);
          IF EnStatOpt THEN
            Remove (n.next(*=>*));
          END;
          e := n.next;
        END;
      END;

  | pc.nd_null:
      n := n.next;

  | pc.nd_eval:
      (* no result, so the node may be eliminated if no side effect *)
      GenExpr (n.l, NIL);
      RemoveExpr (n.l(*=>*));                  (* IRMOD *)
      IF n.l = NIL THEN
        n := n.next
      END;

  | pc.nd_goto:
      ASSERT( n.l.mode = pc.nd_label );
      INCL (n.tags, pc.ntag_no_exit);
      IF EnStatOpt THEN
        Remove (n.next(*=>*));
      END;
      e := n.next;

  | pc.nd_label:
      ValsWorstSetU (CurProps);

  | pc.nd_sproc:
      l := n.r;
      i := 0;
      WHILE l # NIL DO
        lvl := SprocVarArg (n.sub, SHORT(i));
        IF lvl
          THEN GenDesignator (l, FALSE, U_WR);
          ELSE GenExpr (l, NIL);
        END;
        IF MakeReusable(n(*=>*), l, ru_prm, lvl) THEN
          e := n;
      <* IF DB_TRACE THEN *>
          IF EnDebug THEN dbg.CallTraceInd (-1, ")GenStatement reuse => n=%X e=%X", n, e) END;
      <* END *>
          RETURN;
        END;
        l := l.next;
        INC (i);
      END;
      CASE n.sub OF
      | pc.sp_halt
      , pc.sp_abort
      , pc.sp_casetrap:
          INCL (n.tags, pc.ntag_no_exit);
          IF EnStatOpt THEN
            Remove (n.next(*=>*));
          END;
          e := n.next;
      | pc.sp_assert:
          IF (n.r.mode = pc.nd_value) AND (pc.ntag_chk_range IN n.tags) (*3/ n.r.val # NIL*) THEN
            IF n.r.val.is_zero () THEN
              INCL (n.tags, pc.ntag_chk_range);
              INCL (n.tags, pc.ntag_no_exit);
              IF EnStatOpt THEN
                Remove (n.next(*=>*));
              END;
              e := n.next;
            ELSE
              EXCL (n.tags, pc.ntag_chk_range);
            END;
          END;
          IF ~(pc.ntag_chk_range IN n.tags) THEN
            IF (has_sideeffect(n.r)) THEN Wrn(n.pos, 326); END;
            n := n.next
          END;
      | pc.sp_code:
          IF CurProc # NIL THEN
            INCL (CurProc.obj.marks, pc.omark_code)
          END;
          ln := li;
          WHILE ln # NIL DO
            ln.esc := TRUE;
            ln := ln.prev
          END;
          ValsWorstSetU (CurProps);
      | pc.sp_new
      , pc.sp_dispose:
          IF n.obj # NIL THEN
            ObjectUsage (n.pos, n.obj, U_CL, NIL);
          END;
      ELSE
      END;

  | pc.nd_finally:
      ASSERT( n.obj # NIL );
      ASSERT( n.obj.mode = pc.ob_xproc );
      ObjectUsage (n.pos, n.obj, U_RD, NIL);

  | pc.nd_except:
      ASSERT( n.sub = pc.su_none );
      ValsWorstSetU (CurProps);
      ObjectUsage (n.pos, n.obj, U_WR, NIL);
      dead := GenSequence (n.l(*=>*), li);
      ASSERT( (li = NIL) OR ~li.esc );
      ValsWorstSetU (CurProps);
      dead1 := GenSequence (n.r(*=>*), NIL);
      ValsWorstSetU (CurProps);
      IF dead & dead1 THEN
        INCL (n.tags, pc.ntag_no_exit);
        IF EnStatOpt THEN
          Remove (n.next(*=>*));
        END;
        e := n.next;
      END;
      IF n.l = NIL THEN
        n.sub := pc.su_bits; (* nd_reraise must be removed *)
        Remove (n.r(*=>*));  -- EnStatOpt?
        n := n.next;
      END;

  | pc.nd_protect:
      ObjectUsage (n.pos, n.obj, U_WR, NIL);
      dead := GenSequence (n.l(*=>*), li);
      ObjectUsage (n.pos, n.obj, U_RD, NIL);
      ASSERT( (li = NIL) OR ~li.esc );
      IF dead THEN
        INCL (n.tags, pc.ntag_no_exit);
        IF EnStatOpt THEN
          Remove (n.next(*=>*));
        END;
        e := n.next;
      END;
      IF n.l = NIL THEN
        n := n.next
      END;

  | pc.nd_reraise:
      ASSERT( n.r.mode = pc.nd_except );
      ASSERT( n.obj = NIL );
      IF n.r.sub # pc.su_none THEN
        n := n.next
      END;

  | pc.nd_activate:
      ASSERT( n.r.mode = pc.nd_except );
      ASSERT( n.obj = NIL );
      ASSERT( n.r.sub = pc.su_none );

  | pc.nd_retry:
      ASSERT( n.r.mode = pc.nd_except );
      ASSERT( n.obj = NIL );
      ASSERT( n.r.sub = pc.su_none );
      INCL (n.tags, pc.ntag_no_exit);
      IF EnStatOpt THEN
        Remove (n.next(*=>*));
      END;
      e := n.next;

  ELSE
    Fault (n.pos, 105);            -- "Int.error: statement expected"
  END;

  insideInline := loc_insideInline;
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenStatement => n=%X e=%X", n, e) END;
<* END *>
END GenStatement;


--------------------------------------------------------------------------------
-- ('VAR n' actual is usually a link position in some node: changing it
-- causes tree restructuring)
PROCEDURE GenBody ( VAR n: pc.NODE
                  )      :         BOOLEAN;   --> T if dead end
VAR    o: pc.OBJECT;
       i: INTEGER;
    dead: BOOLEAN;
BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (1, "(GenBody n=%X", n) END;
<* END *>

  IF (CurProc # NIL) & EnConsProp THEN (* Init values DFA here *)
    i := 0;
    o := CurProc.prof;
    WHILE o # NIL DO
      ASSERT( o.mno = CurProc.mno );
      ASSERT(    (o.mode # pc.ob_varpar)
             OR ~(pc.otag_no_aliases IN o.tags) );
      o.sno := i;
      INC (i);
      o := o.next;
    END;

    o := CurProc.mem;                 -- calc i - number of params
    WHILE o # NIL DO
      IF o.mode = pc.ob_var THEN
        o.sno := i;
        INC (i)
      END;
      o := o.next;
    END;
    IF i > 0                          -- create value map of proper size
      THEN NEW (CurProps, i);
      ELSE CurProps := NIL;
    END;

    i := 0;                           -- calc i - number of params and locals
    o := CurProc.prof;
    WHILE o # NIL DO                  -- ??? Eto ne lishnee?
      DefValue (o, NIL);              -- drop value
      EXCL (CurProps[i].tags, v_wr);
      CurProps[i].cnt := -1;
      INC (i);
      o := o.next;
    END;
    o := CurProc.mem;
    WHILE o # NIL DO
      ASSERT( o.mno = CurProc.mno );
      IF o.mode = pc.ob_var THEN
        ASSERT( o.sno = i );
        UndefValue (o);
        EXCL (CurProps[i].tags, v_wr);
        CurProps[i].cnt := -1;
        INC (i);
      END;
      o := o.next;
    END;
  END;

  StatCnt := 0;
  StatCnt1:= 0;
  dead := GenSequence (n(*=>*), NIL);
  CurProps := NIL;

<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenBody => %b n=%X", dead, n) END;
<* END *>
  RETURN dead;
END GenBody;

--------------------------------------------------------------------------------
PROCEDURE CloseProcUsage ( u: pc.USAGE
                         )  :          BOOLEAN;

  -- 1 -- CloseProcUsage -------------------------------------------------------
  PROCEDURE IsVisible (o: pc.OBJECT): BOOLEAN;
  VAR lo,lp: INTEGER;
          h: pc.STRUCT;
  BEGIN
    lo := o.lev;
    lp := CurProc.obj.lev;
    IF lo > lp THEN
      RETURN FALSE
    END;
    h := CurProc.obj.host;
    WHILE lo < lp DO
      h := h.obj.host;
      DEC (lp)
    END;
    RETURN h = o.host;
  END IsVisible;

-- 0 -- CloseProcUsage ---------------------------------------------------------
VAR r: BOOLEAN;

BEGIN
  r := TRUE;
  WHILE u # NIL DO
    ASSERT( u.obj.mno = CurMod.mno );
    IF ~(omark_used IN u.obj.marks) THEN
      IF (u.obj.mode IN pc.PROCs) OR IsVisible (u.obj) THEN
        ObjectUsage (env.null_pos, u.obj, u.tags, NIL);
      END;
    END;
    IF  (u.obj.mode IN pc.PROCs)
     &  (omark_used IN u.obj.marks)
     & ~(omark_incl IN u.obj.marks)
    THEN
      (* if current list is full (ttag_usage_ok), no need going here *)
      INCL (u.obj.marks, omark_incl);
      IF ~(pc.ttag_usage_ok IN u.obj.type.tags) THEN
        r := FALSE
      END;
      IF ~CloseProcUsage (u.obj.type.use) THEN
        r := FALSE
      END;
    END;
    u := u.next;
  END;
  RETURN r;
END CloseProcUsage;


--------------------------------------------------------------------------------
-- Traverse procedure object
PROCEDURE GenProc ( p: pc.OBJECT );
VAR     u: pc.USAGE;
    calls: INTEGER;
    savednestedThrowsCall:BOOLEAN;
BEGIN
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (1, "(GenProc p=%X", p) END;
<* END *>

  ASSERT( p.mode IN (pc.PROCs-pc.OB_SET{pc.ob_eproc}) );
  ASSERT( p.mno = CurMod.mno );
  ASSERT( p.val # NIL );
  ASSERT( p.mno = p.host.mno );
  ASSERT( ~(tmark_ingen IN p.type.marks) );
  ASSERT( ~(tmark_genok IN p.type.marks) );
  ASSERT( ~(pc.ttag_usage_ok IN p.type.tags) );
  ASSERT( (p.mode = pc.ob_cproc) OR (p.type.use = NIL) );

  savednestedThrowsCall:=nestedThrowsCall;
  nestedThrowsCall:=FALSE;
  CurLevel := p.lev+1;
  CurProc  := p.type;
  INCL (CurProc.marks, tmark_ingen);
  IF p.val.mode = pc.nd_aggregate THEN
    IF GenExprList (p.val.l(*=>*), p.val.l) THEN
      ASSERT( FALSE )
    END;
  ELSE   (*@23*)
    IF p.mode # pc.ob_cproc THEN
      IF GenBody (p.val(*=>*).r) THEN
        INCL (p.val.tags, pc.ntag_no_exit)
      END;
      calls := CurProc.obj.sno;
      IF (StatCnt1*calls > INLINE_MAXWEIGHT) AND (StatCnt1 > INLINE_SMALLLIMIT) THEN
        INCL (CurProc.marks, tmark_inl_dis);
      END;
    END;
    IF ~Restart & CloseProcUsage (CurProc.use) THEN
      INCL (CurProc.tags, pc.ttag_usage_ok);
    END;
    u := CurProc.use;
    WHILE u # NIL DO
      BitClr (SYSTEM.VAL(SET, u.obj.marks), {ORD(omark_used), ORD(omark_incl)});
      u := u.next;
    END;
  END;
  INCL (CurProc.marks, tmark_genok);
  IF nestedThrowsCall AND
  ~((pc.ttag_throws IN CurProc.tags)OR(pc.ttag_except IN CurProc.tags))THEN
  env.errors.Error(p.pos,450,"Procedure must have 'throws' or 'except' attribute");
  END;
  nestedThrowsCall:=savednestedThrowsCall;
  CurProc := NIL;
<* IF DB_TRACE THEN *>
  IF EnDebug THEN dbg.CallTraceInd (-1, ")GenProc") END;
<* END *>
END GenProc;

--------------------------------------------------------------------------------
PROCEDURE GenNextProc ( p: pc.OBJECT );
VAR sl: INTEGER;
    sp: pc.STRUCT;
    sv: VALUES;
    sc: LONGINT;
    s1: LONGINT;
     u: pc.USAGE;

BEGIN
  IF p.mno # CurMod.mno THEN
    RETURN
  END;
  IF tmark_genok IN p.type.marks THEN
    RETURN
  END;
  IF tmark_ingen IN p.type.marks THEN
    INCL (p.type.marks, tmark_inl_dis);
    RETURN
  END;
  IF ~(p.mode IN (pc.PROCs-pc.OB_SET{pc.ob_eproc})) THEN
    RETURN
  END;

  sl       := CurLevel;  CurLevel := MAX (INTEGER);
  sp       := CurProc;   CurProc  := NIL;
  sv       := CurProps;  CurProps := NIL;
  sc       := StatCnt;
  s1       := StatCnt1;
  IF sp # NIL THEN
    u := sp.use;
    WHILE u # NIL DO
      EXCL (u.obj.marks, omark_used);
      u := u.next
    END;
  END;
  GenProc (p);
  IF sp # NIL THEN
    u := sp.use;
    WHILE u # NIL DO
      INCL (u.obj.marks, omark_used);
      u := u.next
    END;
  END;
  StatCnt1 := s1;
  StatCnt  := sc;
  CurLevel := sl;
  CurProc  := sp;
  CurProps := sv;
END GenNextProc;

-------------------------------------------------------------------------

TYPE
  CONSTS    = POINTER TO const_rec;
  const_rec = RECORD (pc.const_rec) END;

-----------------------------------------------------------------------------

PROCEDURE (c: CONSTS) eval_value (n: pc.NODE);
(*
*)
BEGIN
  CurLevel := -1;
  CurProc  := NIL;
  CurMod   := NIL;
  EnReusageChk := FALSE;
  EnUsageChk   := FALSE;
  EnConsProp   := TRUE;    (* 3: FALSE? *)
  EnStatOpt    := FALSE;
  EnInline     := FALSE;
  EnAllProcs   := env.config.Option("IRALLPROCS");
  EnSL1Addr    := pc.code.name = "STAT";
  GenExpr (n, NIL);
END eval_value;

-------------------------------------------------------------------------

PROCEDURE DoNodes (n: pc.NODE);
(**
   drops otag_no_aliases for those objects, where su_adr is called
*)
  PROCEDURE clr (l: pc.NODE);
  (*
  *)
  BEGIN
    WHILE l.mode IN pc.LVALUEs DO
      CASE l.mode OF
      | pc.nd_index
      , pc.nd_field
      , pc.nd_guard
      , pc.nd_eguard
      , pc.nd_lconv  :
          l := l.l;
      | pc.nd_replace:
          l := l.r;
      | pc.nd_var    :
          EXCL (l.obj.tags, pc.otag_no_aliases);
          RETURN;
      | pc.nd_deref  :
          RETURN;
      END;
    END;
  END clr;
  --------------------------------

VAR o: pc.OBJECT;
    l: pc.NODE;

BEGIN
  WHILE n # NIL DO
    (* nd_sproc: VAR parameters may be not checked *)
    CASE n.mode OF
    | pc.nd_exit:
    | pc.nd_goto:
    | pc.nd_proc:
        INC (n.obj.sno);
        DoNodes (n.l); (* procedures tree *)
        DoNodes (n.r);
    | pc.nd_return:
        DoNodes (n.l);
    | pc.nd_activate:
    | pc.nd_reraise :
    | pc.nd_retry   :
    | pc.nd_with    :
        DoNodes (n.l);
        clr (n.l);
        DoNodes (n.r);
    | pc.nd_unary   :
        DoNodes (n.l);
        IF n.sub = pc.su_adr THEN
          clr (n.l)
        END;
        IF n.sub = pc.su_adr_o2 THEN
          clr (n.l)
        END;
    | pc.nd_call    :
        DoNodes (n.l);
        DoNodes (n.r);
        IF n.l = NIL THEN
          o := n.obj.type.prof;
          INC (n.obj.sno);
        ELSIF n.l.mode = pc.nd_method THEN
          o := n.l.type.prof;
          IF o.mode = pc.ob_varpar THEN
            clr (n.l.l)
          END;
          o := o.next;
        ELSE o := n.l.type.prof;
        END;
        l := n.r;
        WHILE o # NIL DO
          IF o.mode = pc.ob_varpar THEN
            clr (l)
          END;
          l := l.next;
          o := o.next;
        END;
    ELSE
      DoNodes (n.l);
      DoNodes (n.r);
    END;
    n := n.next;
  END;
END DoNodes;

-------------------------------------------------------------------------

TYPE
  INIOBJ  = RECORD (pc.RIDER) END;
  ALIASES = RECORD (pc.RIDER) END;
  GENOBJ  = RECORD (pc.RIDER) END;
  CLROBJ  = RECORD (pc.RIDER) END;
  USEWRN  = RECORD (pc.RIDER) END;

-------------------------------------------------------------------------

PROCEDURE ( VAR r: INIOBJ ) object ( o: pc.OBJECT );
(*
*)
BEGIN
  ASSERT( o.mno = CurMod.mno );
  ASSERT(  ~BitChk ( SYSTEM.VAL(SET, o.marks),
                    {ORD(omark_used), ORD(omark_keep), ORD(omark_incl), ORD(pc.omark_code)})
         OR Restart );
  (* can't check for tmark_basetype absense, as it is set at this pass only *)
  (* can't check for tmark_inlined, as it is saved between iterations *)
  ASSERT( ~BitChk (SYSTEM.VAL(SET, o.type.marks), {ORD(tmark_ingen), ORD(tmark_genok)}) );
  o.tags := o.tags - pc.OTAG_SET{pc.otag_no_aliases, pc.otag_no_threat};
  o.sno := -1;
  IF o.mode IN pc.OB_SET{pc.ob_proc, pc.ob_xproc, pc.ob_lproc} THEN
(*  IF o.type.obj # o THEN INCL (o.marks, omark_keep) END;  --BNRP PR228: ccc *)
    ASSERT( o.type.obj = o );
    o.type.use := NIL;
    EXCL (o.type.tags, pc.ttag_usage_ok);
  END;
  IF EnAllProcs THEN
    INCL (o.marks, omark_keep);
  END;
  IF o.is_public ()
  OR (o.host # NIL)
   & (   (o.host.obj.mode = pc.ob_eproc)
      OR (o.host.mode = pc.ty_record) )
  THEN
    INCL (o.marks, omark_keep);
  ELSIF o.mode = pc.ob_var THEN
    o.tags := o.tags + pc.OTAG_SET{pc.otag_no_aliases, pc.otag_no_threat};
  END;
  IF (o.mode = pc.ob_type)
   & (o.type.obj = o)
   & (o.type.mode = pc.ty_record)
   & (o.type.base # NIL)
   & (o.type.base.mno = CurMod.mno)
  THEN
    INCL (o.type.base.marks, tmark_basetype);
  END;
END object;


--------------------------------------------------------------------------------
PROCEDURE ( VAR r: ALIASES ) object ( o: pc.OBJECT );
BEGIN
  IF (o.mode = pc.ob_var) & (o.val # NIL)
  OR (o.mode = pc.ob_cons)
  OR (o.mode = pc.ob_module) THEN
    DoNodes (o.val);
  END;
END object;


--------------------------------------------------------------------------------
PROCEDURE ( VAR r: GENOBJ ) object ( o: pc.OBJECT );
BEGIN
(*  IF Restart THEN RETURN END; *)
  IF  (o.mode IN (pc.PROCs-pc.OB_SET{pc.ob_eproc}))
   &  (  EnAllProcs                    -- dont skip unused in C target
      OR (o.host.mode = pc.ty_record)  -- dont skip methods
      OR  o.is_public () )             -- dont skip externals
   & ~(tmark_genok IN o.type.marks)
  THEN
    GenProc (o);
  ELSIF (o.mode = pc.ob_var) & (o.val # NIL)
  OR    (o.mode = pc.ob_cons)
  THEN
    GenExpr (o.val, NIL);
  ELSIF (o.mode = pc.ob_var) & (o.attr # NIL)
  THEN
    GenExpr (o.attr(pc.NODE), NIL);
  ELSIF o.mode = pc.ob_module THEN
    IF GenBody (o.val.r(*=>*)) THEN
      INCL (o.val.tags, pc.ntag_no_exit)
    END;
  END;
END object;

--------------------------------------------------------------------------------
PROCEDURE ( VAR w: USEWRN ) object ( o: pc.OBJECT );

  -- 1 -- USEWRN::object -------------------------------------------------------
  PROCEDURE DoCloseProcUsage ();
  VAR
    u: pc.USAGE;
  BEGIN
    IF o.type.use = NIL THEN
      RETURN
    END;
    IF pc.ttag_usage_ok IN o.type.tags THEN
      RETURN
    END;
    CurLevel := o.lev+1;
    CurProc  := o.type;
    u := CurProc.use;
    WHILE u # NIL DO
      INCL (u.obj.marks, omark_used);
      u := u.next
    END;
    SYSTEM.EVAL(CloseProcUsage (CurProc.use));
    u := CurProc.use;
    WHILE u # NIL DO
      BitClr (SYSTEM.VAL(SET, u.obj.marks), {ORD(omark_used), ORD(omark_incl)});
      u := u.next;
    END;
    CurProc := NIL;
  END DoCloseProcUsage;

-- 0 -- USEWRN::object ---------------------------------------------------------
BEGIN
  ASSERT( ~BitChk (SYSTEM.VAL(SET, o.marks), {ORD(omark_used), ORD(omark_incl)}) );
  IF o.mode IN pc.OB_SET{pc.ob_proc, pc.ob_xproc, pc.ob_lproc} THEN
    DoCloseProcUsage;
  END;
  IF omark_keep IN o.marks THEN
    EXCL (o.marks, omark_keep);
    IF (pc.otag_secretvar IN o.tags) THEN
      env.errors.Error (o.pos, 307, o.name^);
    END;
  ELSIF (o.mode IN pc.OB_SET{pc.ob_varpar, pc.ob_seq})
     OR (pc.otag_valpar IN o.tags)
  THEN
    IF (pc.omark_code IN o.host.obj.marks)
    OR (o.host.obj.host.mode = pc.ty_record)&
              ((o=o.host.prof) OR (o.host.obj.val.r=NIL)OR
               (o.host.obj.val.mode = pc.nd_block )&
               (o.host.obj.val.r.r.sub=pc.sp_assert)&(o.host.obj.val.r.r.r.mode=pc.nd_value))
    THEN
    ELSE
      IF ~(pc.otag_secretvar IN o.tags) THEN
        WrnObj (o, 301);    -- param 'o' never used
<* IF TARGET_IDB THEN *>
        IF env.InterViewMode THEN model2.not_used(o); END;
<* END *>
      END;
    END;
  ELSIF o.mode = pc.ob_var THEN
    IF ~(pc.omark_code IN o.host.obj.marks)
     & ~(pc.omark_used_by_code IN o.marks)
    THEN
      WrnObj (o, 300);    -- object 'o' never used
<* IF TARGET_IDB THEN *>
     IF env.InterViewMode THEN model2.not_used(o); END;
<* END *>
      w.del := TRUE;
    END;
  ELSIF (o.mode IN pc.PROCs) THEN
    IF ~(pc.omark_used_by_code IN o.marks) THEN
      IF ~(tmark_inlined IN o.type.marks) THEN
        WrnObj (o, 303);  -- proc 'o' never used
      END;
      w.del := TRUE;
    END;
(* !!!!
  ELSIF o.mode = pc.ob_cons THEN
    Wrn (o.pos, 305); w.del := TRUE; -- constant never used
        can't track constants usage correctly because of eval_const, eval_value
*)
  END;
  EXCL (o.type.marks, tmark_inlined);
END object;

-------------------------------------------------------------------------

PROCEDURE ( VAR r: CLROBJ ) object ( o: pc.OBJECT );
(*
*)
BEGIN
  BitClr (SYSTEM.VAL(SET, o.marks), {ORD(omark_used), ORD(omark_incl), ORD(omark_keep)});
  IF (o.mode # pc.ob_var) & Restart THEN
    EXCL (o.marks, pc.omark_code)
  END;
  (* don't clear tmark_inlined - see USEWRN.object *)
  BitClr (SYSTEM.VAL(SET, o.type.marks), {ORD(tmark_ingen), ORD(tmark_genok), ORD(tmark_basetype)});
END object;

-------------------------------------------------------------------------

PROCEDURE ( c: CONSTS ) eval_module ( n: pc.NODE );
(*
*)
VAR ini : INIOBJ;
    ali : ALIASES;
    gen : GENOBJ;
    Wrn : USEWRN;
    clr : CLROBJ;
    rcnt: INTEGER;

BEGIN
  ASSERT( n.mode = pc.nd_module );

  insideInline := FALSE;
  CurMod       := n.type;
  EnConsProp   := pc.code.en_preopt OR env.config.Option ("IRCONSPROP");
  EnStatOpt    := pc.code.en_preopt OR env.config.Option ("IRSTATOPT");
  EnAllProcs   := env.config.Option("IRALLPROCS");
  EnReusageChk := pc.code.en_tmpvar;
  EnUsageChk   := TRUE;
  EnInline     := env.config.Option ("PROCINLINE");
  EnSL1Addr    := pc.code.name = "STAT";
  INLINE_MAXWEIGHT  := EquationValue("INLINE_MAXWEIGHT");
  INLINE_SMALLLIMIT := EquationValue("INLINE_SMALLLIMIT");
<* IF DB_TRACE THEN *>
  EnDebug     := env.config.Option ("ME_TRACE");
  dbg.Ini;
  IF EnDebug THEN dbg.Trace ("\n====== M-end IR optimization ======") END;
<* END *>

  rcnt    := 0;
  Restart := FALSE;
  REPEAT
    IF rcnt > 3 THEN
      EnInline := FALSE
    END;
    CurLevel := 0;
    CurProc  := NIL;
    (* ini object tags *)
    ini.object (n.type.obj);
    n.type.objects (ini);
    Restart := FALSE;
    (* clear otag_no_aliases for su_adr argument & var parameters *)
    ali.object (n.type.obj);
    n.type.objects (ali);
    (* do operators *)
    gen.object (n.type.obj);
    n.type.objects (gen);
    (* printing warnings for unused objects and their elimination *)
    IF ~Restart THEN
      n.type.objects (Wrn)
    END;
    (* extraneous tags clean-up *)
    clr.object (n.type.obj);
    n.type.objects (clr);
    INC (rcnt);
  UNTIL ~Restart;
  CurProc := NIL;
  CurMod  := NIL;
  EnReusageChk := FALSE;   -- Vit: what for???
  EnUsageChk   := FALSE;   --
END eval_module;

-------------------------------------------------------------------------

PROCEDURE ( c: CONSTS ) eval_const ( n: pc.NODE ) :  pc.VALUE;
(*
*)
BEGIN
  CurLevel := -1;
  CurProc  := NIL;
  CurMod   := NIL;
  EnStatOpt    := FALSE;
  EnConsProp   := pc.code.en_preopt OR env.config.Option ("IRCONSPROP");
  EnReusageChk := FALSE;
  EnUsageChk   := FALSE;
  EnInline     := FALSE;
  EnAllProcs   := env.config.Option("IRALLPROCS");
  EnSL1Addr    := pc.code.name = "STAT";
<* IF DB_TRACE THEN *>
  EnDebug      := env.config.Option ("ME_TRACE");
<* END *>

  GenExpr (n, NIL);

  IF n.val = NIL THEN
    Err (n.pos, 87); -- expr should be const
    RETURN pc.value.new (n.pos, n.type);
  ELSE
    RETURN n.val;
  END;
END eval_const;

-------------------------------------------------------------------------
PROCEDURE ( c: CONSTS ) ini;
(*
*)
  PROCEDURE NewIntValue ( n : LONGINT
                        )   :           pc.VALUE; -- number to create
  (*
  *)
  VAR val: pc.VALUE;
  BEGIN
    val := pc.value.new (env.null_pos, pc.ZZ_type);
    val.set_integer (n);
    RETURN val;
  END NewIntValue;

BEGIN
  vIntTmp := NewIntValue (0);                       -- useful values creation
  vIntCmp := NewIntValue (0);
  vIntZero:= NewIntValue (0);
  vIntOne := NewIntValue (1);
(*3
  IntZero := NewNode (env.null_pos, pc.nd_value, c.ZZ_type);
    SetValue (IntZero, vIntZero);
  IntOne  := NewNode (env.null_pos, pc.nd_value, c.ZZ_type);
    SetValue (IntOne,  vIntOne );
*)
  CurLevel     := -1;
  CurProc      := NIL;
  CurMod       := NIL;
  EnReusageChk := FALSE;
  EnUsageChk   := FALSE;
  EnSL1Addr    := FALSE;
<* IF DB_TRACE THEN *>
  EnDebug      := env.config.Option ("ME_TRACE");
<* END *>
END ini;

--------------------------------------------------------------------------------
PROCEDURE Set *;
VAR c: CONSTS;
BEGIN
  env.config.NewOption ("IRSTATOPT",  TRUE,  SYSTEM.VAL(env.CompilerOption,-1));
  env.config.NewOption ("IRCONSPROP", TRUE,  SYSTEM.VAL(env.CompilerOption,-1));
  env.config.NewOption ("IRALLPROCS", FALSE, SYSTEM.VAL(env.CompilerOption,-1));
  env.config.NewOption ("PROCINLINE", FALSE, SYSTEM.VAL(env.CompilerOption,-1));
  env.config.NewOption ("ME_TRACE",   FALSE, SYSTEM.VAL(env.CompilerOption,-1));
  env.config.NewEquation("INLINE_MAXWEIGHT");
  env.config.SetEquation("INLINE_MAXWEIGHT","64" );
  env.config.NewEquation("INLINE_SMALLLIMIT");
  env.config.SetEquation("INLINE_SMALLLIMIT","16" );
  NEW (c);
  pc.const := c;
  c.ini;
  VoidType := pc.new_type(pc.ty_void);
  trySortTreeLock := 0;
END Set;


END pcConst.
