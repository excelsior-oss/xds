(** Copyright (c) 1991,97 XDS Ltd, Russia. All Rights Reserved. *)
(** M2 parser *)
<* IF ~ DEFINED(COMPONENT_TESTCOVERAGE) THEN *> <* NEW COMPONENT_TESTCOVERAGE- *> <* END *>
MODULE pcM2; (** Ned 10-Nov-87. *)
             (** Ned 11-Aug-90. *)
             (** Ned 01-Feb-92. *)
             (** Sem 23-Sep-93. *)

(* Modifications:
   04-Mar-96 Ned  sysflag_expr: do nothing if errors
   14-Mar-96 Ned  BUG006
                  pcB.chk_threat is used in "for" & "o2_with_variant".
                  RO_tag is not set for control variables.
   14-Mar-96 Ned  BUG004
                  "AppendVariant" - empty variant is preserved in the record
                  "field_list" in "record_aggregate"
                        several small bugs are fixed, message improved
   15-Mar-96 Ned  BUG006 fix of previous fix. Statement seq should be
                  checked for all variables, not only for the threatened.
   23-Mar-96 Ned  procedure "pcB.cast" is used for cast: Type(expr).
                  Version v2.13.
   25-Mar-96 Ned  v2.14.
   27-Mar-96 Ned  Block: error 75 for function without BEGIN
   03-Apr-96 Ned  proc_head: wrong syntax fixed: ';)'
   04-Apr-96 Ned  constructor: disallow for non-extended Oberon.
                               skip_constructor if of the wrong type.
   11-Apr-96 Ned  o2_with_variant: do not check that with variable is
                  declared in the same scope. Check exported for pointers
                  only.
   15-Apr-96 Ned  constants of proctype with value NIL are invented.
                  Check in ProcCall on attempt to call NIL.
   14-May-96 Ned  ModuleDcl.import: import of renamed module.
   16-Jul-96 Ned  typesize instead of pack_types
*)

IMPORT SYSTEM;

IMPORT
  pc:=pcK, pcS, pcO, pcB, pcF, brs:=xcBrowse,
  <* IF db_trace THEN *> dbg := pcVisIR, <* END *>
  <* IF TARGET_386 THEN *>       asm := AsmX86,
  <* ELSIF TARGET_RISC THEN*>    asm := AsmRISC,
  <* END *>
  DStrings,
  xfs:=xiFiles, env:=xiEnv, xcStr;

<* IF TARGET_IDB THEN *>

IMPORT model2;
IMPORT IVERAS;

<* END *>

<* IF COMPONENT_TESTCOVERAGE THEN *>  IMPORT tcMain;  <* END *>

CONST
  err_incomp    = 30;  (* error number: err_incompatibility *)
  err_incomp_ex = 29;  (* error number: err_incompatibility %s %s *)

  omark_imported = pc.omark_aux18;
TYPE
  SP_ARGS = ARRAY 16 OF pc.NODE;

VAR
  cu      : pc.OBJECT;  (* compilation unit *)
  tbd     : pc.OBJECT;  (* type been declared *)
  pbd     : BOOLEAN;    (* pointer base type is being defined *)
  sy      : pcS.Symbol;

  src_time: xfs.Time;   (* last modification time of the source text file *)

  with     : ARRAY 64 OF pc.OBJECT;
  with_type: ARRAY 64 OF pc.STRUCT;
  with_cnt : INTEGER;

  protection: pc.VALUE;

  int_tmp : pc.VALUE;
  int_one : pc.VALUE;

  SP_ARGS_NIL: SP_ARGS;

CONST MAX_PROC_LEV = 64;

VAR proc_lev: INTEGER; (* level of nested procedures *)

CONST
  valid_size = {1,2,4}; (* valid values for enumsize and setsize *)

VAR
  enumsize : LONGINT;   (* minimum size of enum *)
  setsize  : LONGINT;   (* minimum size of set  *)

(*---------------------------------------------------------------*)
PROCEDURE ^ Expr(VAR e: pc.NODE);
PROCEDURE ^ type_definition(VAR t: pc.STRUCT; en_array_of: BOOLEAN);
PROCEDURE ^ sProcCall(VAR i: pc.NODE);
(*---------------------------------------------------------------*)

PROCEDURE cur_pos(VAR ps: pc.TPOS);
BEGIN
  ps:=pcS.txtpos;
END cur_pos;

PROCEDURE check_get(S: pcS.Symbol);
BEGIN
  IF sy#S THEN
    pcS.expc(S);
    IF (S#pcS.semic) OR (sy<pcS.and) OR (sy>pcS.with) THEN pcS.get(sy) END;
    IF S=sy THEN pcS.get(sy) END;
  ELSE
    pcS.get(sy);
  END;
END check_get;

(*----------------------------------------------------------------*)

PROCEDURE qualident(VAR v: pc.OBJECT; mod_allowed:= FALSE: BOOLEAN);
  VAR q: pc.OBJECT; cs: pc.STRUCT;
BEGIN
  IF sy#pcS.ident THEN pcS.expc(pcS.ident); v:=pcO.inv_obj; RETURN END;
  IF ~ pcS.oberon & (tbd#NIL) & (tbd.type#NIL) &
     (tbd.type.mode IN pc.TY_SET{pc.ty_proctype,pc.ty_pointer,pc.ty_opaque}) OR
     pcS.oberon & pbd
  THEN
    IF ~ pcO.try_vis(pcO.cur_scope,pcS.name,v) THEN
      (* see Clause 6.2, p.54 *)
      cs:=NIL;
      IF (tbd#NIL) & (pcO.cur_scope=tbd.type) THEN
        cs:=pcO.cur_scope; pcO.exit_scope;
      END;
      v := pcO.new_obj(pc.ob_type);
      pcO.set_name(v,pcS.name);
      v.type := pcO.new_type(pcO.ty_undef);
      pcO.dcl(pcO.cur_scope,v);
      pcO.alloc(v);
      INCL(v.tags,pcO.otag_forward);
      IF cs#NIL THEN pcO.enter_scope(cs) END;
      v.type.obj:=v;
    END;
  ELSE
    pcO.fnd_vis(pcO.cur_scope,pcS.name,v);
    ASSERT(v.mode IN pc.OB_Common);
  END;
  pcS.get(sy);
  LOOP
    q:=v;
    IF sy#pcS.period THEN EXIT;
    ELSIF q.mode=pc.ob_module THEN
      pcS.get(sy);
      IF sy#pcS.ident THEN pcS.expc(pcS.ident); v:=pcO.inv_obj;
      ELSE pcO.fnd_qua(q.type,pcS.name,v); pcS.get(sy);
      END;
    ELSIF q.mode=pc.ob_inv THEN
      pcS.get(sy);
      IF sy#pcS.ident THEN pcS.expc(pcS.ident) ELSE pcS.get(sy) END;
    ELSE EXIT
    END;
  END;
  IF (v.mode=pc.ob_module) & ~mod_allowed THEN
    pcS.err(27,v.name^); v:=pcO.inv_obj
  ELSIF (v=tbd) &
        ((tbd.type=NIL) OR
         ~( (tbd.type.mode IN pc.TY_SET{pc.ty_proctype,pc.ty_pointer}) OR
            (pbd & (tbd.type.mode = pc.ty_record) & (tbd.type.flag = pc.flag_o2))   -- LAZ
        ))
  THEN
    (* An identifier being declared as a type
       shall not be used in declaring that type,
       unless that type is a new pointer type or
       a new procedure type, p.57
      -- to permit to define a pointer to a record in the record declaration itself -- LAZ
    *)
    pcS.err(21,v.name^); v:=pcO.inv_obj;
  ELSIF (v.mode#pc.ob_type) & (v.mode#pc.ob_label) & (v.type=pcO.undef) THEN
    pcS.err(117,v.name^); v:=pcO.inv_obj;
  END;
  ASSERT( (v.type.mode#pc.ty_module) OR mod_allowed );
END qualident;

PROCEDURE sProcTypeCall(VAR proc: pc.OBJECT);
VAR
  n: INTEGER;
--  result: pc.NODE;
  a: SP_ARGS;
BEGIN
  n:=0;
  IF sy=pcS.lpar THEN
    pcS.get(sy);
    IF sy#pcS.rpar THEN
      LOOP
        Expr(a[n]);
        IF n<LEN(a)-1 THEN INC(n) END;
        IF sy=pcS.comma THEN pcS.get(sy) ELSE EXIT END;
      END;
    END;
    check_get(pcS.rpar);
  END;

--  CASE pcO.sproc_no(proc) OF
(*  |pcO.fn_key:
     pcO.new(result, pc.nd_inv);
     result.obj := proc;
     pcO.new(a[n],pc.nd_type);
     a[n].obj := tbd;
     pcF.gen_system_function(result, a, n, FALSE);
*)
--  ELSE
    pcS.err(31); proc:=pcO.inv_obj;
    RETURN
--  END;
--  proc := result.obj;
END sProcTypeCall;

PROCEDURE type_qualident(VAR v: pc.OBJECT; en_array_of: BOOLEAN);
BEGIN
  qualident(v);
  IF v.mode=pc.ob_type THEN
    IF (v.type.mode=pc.ty_opaque) &
       ~ pcO.def &
       (v.type.base#NIL) &
       (v.type.base.obj#NIL) (* хорошо бы убрать это условие !!!!! *)
    THEN
      v:=v.type.base.obj;
      ASSERT(v.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}
<* IF MCS THEN *>
                 +pc.TY_SET{pc.ty_longint,pc.ty_longcard}
<* END *>
      );
    END;
    IF ~ en_array_of THEN
      IF pcO.otag_forward IN v.tags THEN
        INCL(v.marks,pcO.omark_dis_array_of);
      ELSIF v.type.mode=pc.ty_array_of THEN
        pcS.err(46); v:=pcO.inv_obj;
      END;
    END;
  ELSIF v.mode=pc.ob_sproc THEN
    sProcTypeCall(v);
  ELSIF v.mode#pc.ob_inv THEN
    pcS.err(31); v:=pcO.inv_obj;
  END;
END type_qualident;

PROCEDURE o2_type_test(VAR n: pc.NODE; obj: pc.OBJECT; guard: BOOLEAN);
(* if guard=FALSE then gen check for node type is obj.type *)

  VAR lg: pc.STRUCT; (* lg = last guard type *)

  PROCEDURE GTT(t0,t1: pc.STRUCT);
  BEGIN
    ASSERT((t0.mode=pc.ty_record) & (t1.mode=pc.ty_record));
    IF t0#t1 THEN
      WHILE (t1#NIL) & (t1.obj#lg.obj) DO t1:=t1.base END;
      IF t1#NIL THEN
        IF guard THEN
          ASSERT(obj.type.flag=pc.flag_o2);
          pcB.guard(n,obj.type,TRUE);
        ELSIF n.type.mode=pc.ty_pointer THEN
          pcB.unary(n,pc.su_is,pcO.boolean);
          n.obj:=obj.type.base.obj;
          pcO.new(n.r,pc.nd_type);
          n.r.end  := pcS.txtpos;
          n.r.type := obj.type;
          n.r.obj  := obj;
        ELSE
          pcB.unary(n,pc.su_is,pcO.boolean);
          n.obj:=obj;
          pcO.new(n.r,pc.nd_type);
          n.r.type:=obj.type;
          n.r.obj:=obj;
        END;
      ELSE pcS.err(41);
      END
    ELSIF ~ guard THEN
      pcB.gen_usage(pcO.true,n);
      n.end := pcS.txtpos;
    END;
  END GTT;

BEGIN
  lg:=n.type;
  pcB.chk_designator(n);
  IF n.type.mode=pcO.ty_invtype THEN
  ELSIF obj.type.mode=pcO.ty_invtype THEN
  ELSIF ~ (n.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_record}) OR
        ~ (obj.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_record})
  THEN pcS.err(59);
  ELSIF n.type.mode#obj.type.mode THEN pcS.err(144);
  ELSIF (n.type.flag#pc.flag_o2) OR (obj.type.flag#pc.flag_o2) THEN pcS.err(71);
  ELSIF n.type.mode=pc.ty_pointer THEN
    IF (n.type.base.mode#pc.ty_record) OR
       (obj.type.base.mode#pc.ty_record)
    THEN pcS.err(62);
    ELSE
      lg:=lg.base;
      IF n.mode=pc.nd_guard THEN
        GTT(n.l.type.base, obj.type.base);
      ELSE
        GTT(n.type.base, obj.type.base);
      END;
      RETURN;
    END;
  ELSIF (n.type.mode=pc.ty_record) THEN
    IF n.mode=pc.nd_guard THEN
      IF (n.l.mode=pc.nd_var) & (n.l.obj.mode=pc.ob_varpar) THEN
        GTT(n.l.type, obj.type); RETURN;
      END;
    ELSIF (n.mode=pc.nd_var) & (n.obj.mode=pc.ob_varpar) THEN
      GTT(n.type, obj.type); RETURN;
    END;
    pcS.err(59);
  ELSE pcS.err(59);
  END;
  n.type:=pcO.invtype;
END o2_type_test;

PROCEDURE check_super_call(n: pc.NODE);
  VAR l: pc.NODE; m,p: pc.OBJECT; class: pc.STRUCT;
BEGIN
  ASSERT(n.mode = pc.nd_method);
  m:=n.obj;
  IF n.l.type.mode=pc.ty_pointer THEN class:=n.l.type.base.base;
  ELSE class:=n.l.type.base;
  END;
  IF (class=NIL) OR ~ pcO.try_qua(class,m.name^,n.obj) THEN pcS.err(58)
(*!!!! ????
  ELSIF m.type#pcO.cur_scope THEN pcS.err(68)
*)
  ELSE
    ASSERT(m.type.inx.obj=n.obj);
    p:=m.type.prof; l:=n.l;
    IF p.type.mode IN pc.TY_SET{pc.ty_record,pc.ty_pointer} THEN
      IF (l.mode#pc.nd_var) OR (l.obj#p) THEN pcS.err(57);
      ELSE pcB.guard(n.l,n.obj.type.prof.type,FALSE); n.sub:=pc.su_bits;
      END;
    ELSE
      (* error should be already reported *)
      ASSERT(env.errors.err_cnt > 0);
    END;
  END;
END check_super_call;

PROCEDURE extend_designator(VAR i: pc.NODE);
VAR
  ps: pc.TPOS;
  ex: pc.NODE;
  v : pc.OBJECT;
BEGIN
  LOOP
    ASSERT(i#NIL);
    ASSERT(i.type#NIL);
    ASSERT((i.type.mode#pc.ty_opaque) OR (i.type.obj#NIL));
    IF (i.type.mode=pc.ty_opaque) &
       (i.type.base=NIL) & (i.type.obj.mno=pc.cur_mod) & pcO.imp
    THEN
      pcS.err(96,i.type.obj.name^);
      i.type:=pcO.invtype;
    ELSIF (i.type.mode=pcO.ty_undef) AND (i.mode # pc.nd_label) THEN
      pcS.err(97,i.type.obj.name^);
      i.type:=pcO.invtype;
    END;
    IF sy=pcS.bar THEN
      IF i.mode=pc.nd_method THEN check_super_call(i); pcS.get(sy); EXIT END;
      pcB.gen_deref(i); pcS.get(sy);
    ELSIF sy=pcS.period THEN
      ps:=pcS.txtpos;
      IF pcS.oberon & (i.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) THEN
        pcB.gen_deref(i);
      END;
      pcS.get(sy);
      IF sy#pcS.ident THEN
        pcS.expc(pcS.ident); i.type:=pcO.invtype;
      ELSE
        pcB.gen_access(pcS.prevpos,i);
        pcS.get(sy);
        i.end := pcS.txtpos;
      END;
    ELSIF sy=pcS.lbr THEN
      ps:=pcS.txtpos;
      IF pcS.oberon & (i.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) THEN
        pcB.gen_deref(i);
      END;
      pcS.get(sy); Expr(ex); pcB.gen_index(ps,i,ex);
      IF sy=pcS.comma THEN sy:=pcS.lbr ELSE check_get(pcS.rbr) END;
      i.end := pcS.txtpos;
    ELSIF (sy=pcS.lpar) & pcS.oberon &
          (i.mode=pc.nd_sproc) & (pcO.sproc_no(i.obj)=pcO.fn_cast)
    THEN
      sProcCall(i);
      ASSERT(i.type#NIL);
    ELSIF (sy=pcS.lpar) & pcS.oberon &
          (i.type.mode#pcO.ty_invtype) &
          (i.type.mode#pc.ty_proctype)
    THEN
      pcB.chk_designator(i);
      pcS.get(sy); type_qualident(v,FALSE);
      o2_type_test(i,v,TRUE); check_get(pcS.rpar);
      ASSERT(i.type#NIL);
    ELSE
      EXIT
    END;
  END;
END extend_designator;

PROCEDURE designator(VAR i: pc.NODE);

  PROCEDURE try_m2_with(VAR v: pc.OBJECT): BOOLEAN;
    VAR t: pc.STRUCT; i: INTEGER;
  BEGIN
    IF pcS.oberon THEN RETURN FALSE END;
    FOR i:=with_cnt-1 TO 0 BY -1 DO
      t:=with[i].type;
      IF (t.mode=pc.ty_pointer) &
         (t.base.mode=pc.ty_record) &
         pcO.try_qua(t.base,pcS.name,v)
      THEN
        v:=with[i]; RETURN TRUE
      END;
    END;
    RETURN FALSE
  END try_m2_with;

  PROCEDURE try_o2_with(v: pc.OBJECT): pc.STRUCT;
    VAR i: INTEGER;
  BEGIN
    IF ~ pcS.oberon THEN RETURN v.type END;
    FOR i:=with_cnt-1 TO 0 BY -1 DO
      IF v=with[i] THEN RETURN with_type[i] END;
    END;
    RETURN v.type;
  END try_o2_with;

  VAR v: pc.OBJECT; ps: pc.TPOS; t: pc.STRUCT;
BEGIN
  ps := pcS.txtpos;
  IF sy#pcS.ident THEN pcS.expc(pcS.ident); Expr(i); RETURN END;
  IF try_m2_with(v) THEN
    pcO.new(i,pc.nd_var);
    i.pos := ps;
    i.end := pcS.txtpos;
    i.obj:=v;
    i.type:=v.type;
    pcB.gen_deref(i);
    EXCL(i.tags,pc.ntag_chk_range);
    pcB.gen_access(pcS.txtpos,i);
    pcS.get(sy);
  ELSE
    ps:=pcS.txtpos;
    qualident(v);
    t:=try_o2_with(v);
    pcB.gen_usage(v,i);
    i.pos := ps;
    i.end := pcS.txtpos;
    pcB.guard(i,t,FALSE);
    IF (v.mode = pc.ob_var) & (pcO.otag_varpar_nil IN v.tags) THEN
      pcB.gen_deref(i);
      i.end := pcS.txtpos;
   END;
  END;
  extend_designator(i);
END designator;

(*----------------------------------------------------------------*)

PROCEDURE boolexpr(VAR i: pc.NODE);
VAR
  s1,s2: ARRAY 70 OF CHAR;
BEGIN
  Expr(i);
  pcB.chk_value(i);
  IF i.mode = pc.nd_type THEN
    i.mode := pc.nd_inv;
  END;
  IF i.type.mode=pcO.ty_invtype THEN RETURN END;
  IF i.type.mode=pc.ty_boolean THEN RETURN END;
  IF pcB.type_string(s1, i.type) & pcB.type_string(s2, pcO.boolean) THEN
    pcS.error(i.pos,'e',err_incomp_ex,s1,s2);
  ELSE
    pcS.error(i.pos,'e',err_incomp);
  END;
  pcB.convert(i,pcO.invtype);
END boolexpr;

PROCEDURE new_value(ps: pc.TPOS; n: LONGINT): pc.VALUE;
  VAR v: pc.VALUE;
BEGIN
  v:=pc.value.new(ps,pcO.ZZ_type);
  v.set_integer(n);
  RETURN v;
END new_value;

PROCEDURE const_value(VAR v: pc.VALUE; n: pc.NODE; old_errors: INTEGER);
BEGIN
  IF  (n.mode = pc.nd_inv)
    OR (env.errors.err_cnt # old_errors)
    OR (n.type.mode = pc.ty_free)
  THEN
    v:=new_value(n.pos,0);
  ELSE
    v:=pc.const.eval_const(n);
  END;
END const_value;

PROCEDURE sProcCall(VAR i: pc.NODE);
  VAR
    n: INTEGER;
    a: SP_ARGS;
    des: BOOLEAN;
BEGIN
  a:=SP_ARGS_NIL; (* to force NIL checks *)
  n:=0;
  IF sy=pcS.lpar THEN
    pcS.get(sy);
    IF sy#pcS.rpar THEN
      LOOP
        Expr(a[n]);
        IF n<LEN(a)-1 THEN INC(n) END;
        IF sy=pcS.comma THEN pcS.get(sy) ELSE EXIT END;
      END;
    END;
    check_get(pcS.rpar);
  END;
  i.end := pcS.txtpos;
  des := pcS.oberon 
       & (i.mode=pc.nd_sproc) & (pcO.sproc_no(i.obj)=pcO.fn_cast) 
       & (n>1) & (a[1].mode # pc.nd_value);
  pcF.gen_system_function(i,a,n, des);
END sProcCall;

PROCEDURE parmlist(VAR ap: pc.NODE; fp: pc.OBJECT);
  VAR tl,sq,e,n: pc.NODE; ps: pc.TPOS;
BEGIN
  tl:=NIL; sq:=NIL; ps:=pcS.txtpos;
  IF sy=pcS.lpar THEN
    pcS.get(sy);
    IF sy#pcS.rpar THEN
      LOOP
        IF (sy#pcS.comma) & (sy#pcS.rpar) THEN
          Expr(e)
        ELSIF (fp#NIL) & (fp.val#NIL) THEN (* default parameter *)
          pcO.new(e,pc.nd_value);
          e.type:=fp.val.type;
          e.val:=fp.val.val;
        ELSIF fp#NIL THEN
          pcS.err(85);
          pcO.new(e,pc.nd_inv);
          e.type:=pcO.invtype;
        ELSE
          e:=NIL;
        END;
        IF fp=NIL THEN
          pcS.err(48);
        ELSIF (fp.mode=pc.ob_seq) & ((tl=NIL) OR (tl.mode#pc.nd_sequence)) &
              (e.mode=pc.nd_var) & (e.obj.mode=pc.ob_seq)
        THEN
          pcB.convert(e,fp.type);
          IF tl=NIL THEN ap:=e ELSE tl.next:=e END;
          tl:=e; fp:=fp.next;
        ELSIF fp.mode=pc.ob_seq THEN
          ASSERT(fp.next=NIL);
          IF (tl=NIL) OR (tl.mode#pc.nd_sequence) THEN
            pcO.new(n,pc.nd_sequence);
            n.end := pcS.txtpos;
            n.type:=fp.type;
            IF tl=NIL THEN ap:=n ELSE tl.next:=n END;
            tl:=n;
          END;
          pcB.chk_value(e);
          pcB.eval_value(e);
          IF fp.host.flag IN pc.LangSet{ pc.flag_c, pc.flag_p,
                                         pc.flag_stdcall,
                                         pc.flag_vmcall, pc.flag_lightcall,
                                         pc.flag_syscall, pc.flag_javacall }
          THEN
            IF e.type.mode=pc.ty_ZZ THEN
              IF (e.mode=pc.nd_value) & ~ e.val.is_neg() THEN
                pcB.convert(e,pcB.lit_value_type(e.val,e.type,pcO.cardinal));
              ELSIF e.mode=pc.nd_value THEN
                pcB.convert(e,pcB.lit_value_type(e.val,e.type,pcO.integer));
              ELSE
                pcB.convert(e,pcO.longcard);
              END;
            ELSIF e.type.mode=pc.ty_RR THEN
              IF e.mode=pc.nd_value THEN
                pcB.convert(e,pcB.lit_value_type(e.val,e.type,pcO.real));
              ELSE
                pcB.convert(e,pcO.longreal);
              END;
            ELSIF e.type.mode IN pc.REALs THEN
              pcB.convert(e,pcO.longreal)
            END;
          ELSIF e.type.mode IN pc.REALs THEN
            pcB.convert(e,pcO.longreal)
          ELSIF e.type.mode IN pc.INTs  THEN
            IF (e.type.mode#pc.ty_ZZ) OR (e.mode=pc.nd_value) & e.val.is_neg() THEN
              pcB.convert(e,pcO.longint);
            ELSE
              pcB.convert(e,pcO.longcard);
            END;
          ELSIF e.type.mode IN pc.SEQ_ARRs THEN
          ELSIF e.type.mode IN pc.SEQ_PTRs THEN
          ELSE
            pcB.convert(e,pcO.longcard);
          END;
          IF sq=NIL THEN tl.l:=e ELSE sq.next:=e END;
          sq:=e;
        ELSE
          pcB.parameter_compatibility(fp,e);
          IF tl=NIL THEN ap:=e ELSE tl.next:=e END;
          tl:=e; fp:=fp.next;
        END;
        IF sy=pcS.comma THEN pcS.get(sy) ELSE EXIT END;
      END;
    END;
    check_get(pcS.rpar);
  END;
  WHILE fp#NIL DO
    IF fp.mode=pc.ob_seq THEN
      IF (tl=NIL) OR (tl.mode#pc.nd_sequence) THEN
        pcO.new(n,pc.nd_sequence);
        n.end := pcS.txtpos;
        n.type:=fp.type;
        IF tl=NIL THEN ap:=n ELSE tl.next:=n END;
        tl:=n;
      END;
    ELSIF fp.val#NIL THEN
      pcO.new(e,fp.val.mode);
      e.copy(fp.val);
      IF tl=NIL THEN ap:=e ELSE tl.next:=e END;
      tl:=e;
    ELSE
      pcS.error(ps,'e',47);
      RETURN;
    END;
    fp:=fp.next;
  END;
END parmlist;

PROCEDURE skip_parms;
  VAR i: pc.NODE;
BEGIN
  IF sy#pcS.lpar THEN RETURN END;
  pcS.get(sy);
  LOOP
    IF (sy=pcS.rpar) OR (sy=pcS.end) THEN EXIT END;
    Expr(i);
    IF sy=pcS.comma THEN pcS.get(sy) ELSE EXIT END;
  END;
  IF sy=pcS.rpar THEN pcS.get(sy) END;
END skip_parms;

PROCEDURE ProcCall(n: pc.NODE; func: BOOLEAN);
  VAR proc: pc.NODE; e: INTEGER; c: pc.OBJECT;
BEGIN
  n.type:=pcO.invtype;
  proc:=n.l;
  IF proc.type.mode#pc.ty_proctype THEN
    IF proc.type.mode=pcO.ty_invtype THEN (* nothing *)
    ELSIF proc.mode=pc.nd_inv THEN (* nothing *)
    ELSIF proc.mode=pc.nd_proc THEN pcS.error(proc.pos,'e',55,proc.obj.name^)
    ELSE pcS.error(proc.pos,'e',156)
    END;
    skip_parms; RETURN
  END;
  IF (proc.mode = pc.nd_var) & (proc.obj.mode = pc.ob_cons) THEN
    c:=proc.obj;
    IF (c.val.mode = pc.nd_value) & (c.val.val.get_object()=NIL) THEN
      pcS.err(55,'NIL'); skip_parms; RETURN
    END;
  END;
  n.type:=proc.type.base;
  IF n.type.mode=pcO.ty_invtype THEN
  ELSIF (n.type.mode#pc.ty_void)=func THEN
  ELSIF ~ func & (proc.type.flag IN pc.CallResOptional) THEN
  ELSE
    IF func THEN e:=91 ELSE e:=90 END;
    pcS.error(n.pos,'e',e);
    n.type:=pcO.invtype;
  END;
  IF proc.mode=pc.nd_method THEN
    parmlist(n.r,proc.type.prof.next);
    IF proc.sub#pc.su_none THEN
      proc.mode:=pc.nd_proc; proc.sub:=pc.su_none; proc.l.next:=n.r;
      n.r:=proc.l; proc.l:=NIL;
    END;
  ELSE
    parmlist(n.r,proc.type.prof);
  END;
  IF proc.mode=pc.nd_proc THEN
    n.obj:=proc.obj; n.l:=NIL;
  ELSIF env.proc_check IN env.config.tags THEN
    INCL(n.tags,pc.ntag_chk_range);
  END;
END ProcCall;

(*----------------------------------------------------------------*)

PROCEDURE Expr(VAR e1: pc.NODE);

  PROCEDURE ^ constructor(VAR n: pc.NODE; type: pc.STRUCT);

  PROCEDURE set_constructor(VAR n: pc.NODE; type: pc.STRUCT);
  VAR
    a, b: pc.NODE;
    tail: pc.NODE;
    ps  : pc.TPOS;
  BEGIN
    pcO.new(n,pc.nd_aggregate); n.type:=type;
    IF env.set_check IN env.config.tags THEN
      INCL(n.tags,pc.ntag_chk_range);
    END;
    pcS.get(sy);
    tail:=NIL;
    IF sy#pcS.rbrace THEN
      LOOP
        ps := pcS.txtpos;
        Expr(a); pcB.chk_index(type.base,a,FALSE);
        IF sy=pcS.range  THEN
          pcS.get(sy);
          ps := pcS.txtpos;
          Expr(b); pcB.chk_index(type.base,b,FALSE);
          pcO.tie(n.l,tail,pc.nd_node);
          tail.pos := ps;
          tail.end := pcS.txtpos;
          tail.l:=a;
          tail.r:=b;
        ELSE
          pcO.app(n.l,tail,a);
        END;
        IF sy=pcS.rbrace THEN pcS.get(sy); EXIT END;
        IF sy=pcS.comma  THEN pcS.get(sy)
        ELSE pcS.expc(pcS.rbrace);  EXIT
        END
      END;
    ELSE pcS.get(sy)
    END;
    n.end := pcS.txtpos;
  END set_constructor;

  PROCEDURE repeated_structure_component(VAR n: pc.NODE;
                        VAR no: LONGINT; base: pc.STRUCT);
  VAR
    ex : pc.NODE;
    str: BOOLEAN;
    len: LONGINT;
    old_errors: INTEGER;
  BEGIN
    no:=1;
    IF sy=pcS.lbrace THEN constructor(ex,base);
    ELSE Expr(ex);
    END;
    str:=pcS.ext() & (base.mode = pc.ty_char)
                   & (ex.type.mode = pc.ty_SS)
                   & (ex.type.len > 2);
    IF str THEN
      len:=ex.type.len-1;
    ELSE
      pcB.assign_compatibility(pcS.txtpos,base,ex);
      len:=1;
    END;
    IF sy=pcS.by THEN
      old_errors := env.errors.err_cnt;
      pcO.new(n,pc.nd_node); n.l:=ex; n.type:=base;
      pcS.get(sy); Expr(ex);
      n.end := pcS.txtpos;
      IF ex.type.mode=pcO.ty_invtype THEN (* nothing *)
      ELSIF ~ pcB.is_whole(pcB.host_type(ex.type)) THEN
        pcB.err(ex,111);
      ELSIF env.errors.err_cnt=old_errors THEN
        pcB.eval_value(ex);
        IF ex.mode#pc.nd_value THEN
          pcS.err(87)
        ELSE
          no:=ex.val.get_integer();
          IF no<0 THEN no:=0; pcB.err(ex,107) END;
        END;
      END;
      n.r:=ex;
    ELSE
      n:=ex;
    END;
    no:=no*len; (* if str *)
  END repeated_structure_component;

  PROCEDURE array_constructed_value(VAR n: pc.NODE;
                        VAR no: LONGINT; base: pc.STRUCT);
    VAR ex: pc.NODE; tail: pc.NODE; len: LONGINT;
  BEGIN
    tail:=NIL; no:=0;
    pcO.new(n,pc.nd_aggregate); n.type:=pcO.invtype;
    IF sy#pcS.lbrace THEN pcS.expc(pcS.lbrace); RETURN END;
    pcS.get(sy);
    LOOP
      repeated_structure_component(ex,len,base);
      pcO.app(n.l,tail,ex); INC(no,len);
      IF sy=pcS.rbrace THEN pcS.get(sy); EXIT END;
      IF sy=pcS.comma  THEN pcS.get(sy)
      ELSE pcS.expc(pcS.rbrace); EXIT
      END
    END;
  END array_constructed_value;

  PROCEDURE open_array_constructor(VAR n: pc.NODE; base: pc.STRUCT);
    VAR no: LONGINT; type: pc.STRUCT;
  BEGIN
    IF base.mode IN pc.COMPOUNDs THEN pcS.err(64) END;
    array_constructed_value(n,no,base);
    type := pcO.new_type(pc.ty_array);
    pcO.set_type_base(type,base);
    type.inx := pcO.new_type(pc.ty_range);
    type.inx.base:=pcO.index;
    type.inx.min:=pcO.cardinal.min;
    type.inx.max:=new_value(pcS.txtpos,no-1);
    type.min:=type.inx.min;
    type.max:=type.inx.max;
    type.len:=no;
    n.type:=type;
    type.end := pcS.txtpos;
  END open_array_constructor;

  PROCEDURE array_constructor(VAR n: pc.NODE; type: pc.STRUCT);
    VAR no: LONGINT;
  BEGIN
    array_constructed_value(n,no,type.base);
    n.type:=type;
    IF no#type.len THEN pcS.err(110,type.len) END;
  END array_constructor;

  PROCEDURE search_variant(VAR l: pc.NODE; v: pc.VALUE);
    VAR e,m: pc.NODE;
  BEGIN
    e:=NIL;
    LOOP
      IF l=NIL THEN EXIT END;
      ASSERT(l.mode=pc.nd_node);
      m:=l.l;
      IF m=NIL THEN e:=l END;
      LOOP
        IF m=NIL THEN EXIT END;
        IF m.mode=pc.nd_pair THEN
          IF pcB.cmp_value(pc.sb_geq,v,m.val) &
             pcB.cmp_value(pc.sb_leq,v,m.l.val) THEN EXIT END;
        ELSE
          ASSERT(m.mode=pc.nd_value);
          IF pcB.cmp_value(pc.sb_equ,v,m.val) THEN EXIT END;
        END;
        m:=m.next;
      END;
      IF m#NIL THEN EXIT END;
      l:=l.next;
    END;
    IF l=NIL THEN l:=e END;
  END search_variant;

  PROCEDURE fields_list(f: pc.OBJECT; VAR n,tail: pc.NODE);
    PROCEDURE skip;
    BEGIN
      WHILE sy#pcS.rbrace DO pcS.get(sy) END;
    END skip;
    VAR ex: pc.NODE; l: pc.NODE;
  BEGIN
    IF (sy#pcS.rbrace) & (f#NIL) THEN
      LOOP
        IF sy=pcS.lbrace THEN constructor(ex,f.type);
        ELSE Expr(ex);
        END;
        pcB.assign_compatibility(pcS.txtpos,f.type,ex);
        IF f.mode=pc.ob_header THEN
          IF env.errors.err_cnt#0 THEN skip; RETURN END;
          pcB.eval_value(ex);
          IF ex.mode#pc.nd_value THEN pcB.err(ex,87); skip; RETURN END;
          l:=f.val.l;
          search_variant(l,ex.val);
          IF l=NIL THEN pcS.err(113); skip; RETURN END;
          pcO.app(n,tail,ex);
          IF l.obj#NIL THEN
            IF sy#pcS.comma THEN pcS.err(112,l.obj.name^); skip; RETURN END;
            pcS.get(sy);
            fields_list(l.obj,n,tail);
          END;
        ELSE
          pcO.app(n,tail,ex);
        END;
        f:=f.next;
        IF (sy#pcS.comma) OR (f=NIL) THEN EXIT END;
        pcS.get(sy)
      END;
    END;
    IF f#NIL THEN pcS.err(112,f.name^) END;
  END fields_list;

  PROCEDURE skip_constructor;
  BEGIN
    WHILE (sy#pcS.rbrace) & (sy#pcS.end) & (sy#pcS.begin) & (sy#pcS.semic) DO
      pcS.get(sy);
    END;
    check_get(pcS.rbrace);
  END skip_constructor;

  PROCEDURE record_constructor(VAR n: pc.NODE; type: pc.STRUCT);
    VAR tail: pc.NODE;
  BEGIN
    pcO.new(n,pc.nd_aggregate); n.type:=type;
    IF sy#pcS.lbrace THEN pcS.expc(pcS.lbrace);
    ELSE
      pcS.get(sy); tail:=NIL;
      fields_list(type.prof,n.l,tail);
      IF sy=pcS.rbrace THEN pcS.get(sy)
      ELSE
        pcS.err(129);
        skip_constructor;
      END;
    END;
  END record_constructor;

  PROCEDURE simple_constructor(VAR n: pc.NODE; type: pc.STRUCT);
  VAR prev_error_num : INTEGER;
  BEGIN
    IF sy#pcS.lbrace THEN pcS.expc(pcS.lbrace); RETURN END;
    pcS.get(sy);
    prev_error_num := env.errors.err_cnt;
    Expr(n);
    pcB.convert(n,type);
    check_get(pcS.rbrace);
    IF prev_error_num < env.errors.err_cnt THEN RETURN; END;
    pc.const.eval_value(n);
    IF n.mode#pc.nd_value THEN pcB.err(n,87); RETURN END;
  END simple_constructor;

  PROCEDURE constructor(VAR n: pc.NODE; type: pc.STRUCT);
  BEGIN
    IF type.mode IN pc.SETs THEN set_constructor(n,type);
    ELSIF (type.mode IN pc.ARRs + pc.TY_SET{pc.ty_record})
      OR (type.mode # pcO.ty_invtype)
    THEN
      IF pcS.oberon & ~ pcS.ext() THEN
        pcS.err(102,"(array or record or simple aggregate)")
      END;
      IF type.mode=pc.ty_array_of THEN open_array_constructor(n,type.base);
      ELSIF type.mode IN pc.ARRs THEN array_constructor(n,type);
      ELSIF type.mode=pc.ty_record THEN record_constructor(n,type);
      ELSE
        IF type.mode = pc.ty_range THEN
          type := type.base;
        END;
        IF type.mode IN pc.WHOLEs+pc.TY_SET{pc.ty_boolean,pc.ty_enum} THEN
          simple_constructor(n,type);
        END;
      END;
    ELSE
     -- ASSERT(type.mode = pcO.ty_invtype);
      pcO.new(n,pc.nd_aggregate); n.type:=pcO.invtype;
      ASSERT(sy = pcS.lbrace);
      skip_constructor;
    END;
  END constructor;

  PROCEDURE iden(VAR n: pc.NODE);
  VAR
    ex: pc.NODE;
    ps, start_pos: pc.TPOS;
  BEGIN
    start_pos := pcS.txtpos;
    designator(n);
    IF n.mode=pc.nd_sproc THEN
      sProcCall(n);
      IF n.type.mode=pc.ty_void THEN
        pcS.error(n.pos,'e',91); n.type:=pcO.invtype;
      ELSIF pcS.ext() (*& (sy = pcS.bar)*) THEN
        extend_designator(n);
      END;
    ELSIF sy=pcS.lpar THEN
      IF n.mode=pc.nd_type THEN
        ps:=pcS.txtpos;
<* IF ~MCS THEN *>
        IF NOT (pcS.ext() OR pcS.TS_ext())
         <* IF TARGET_VAX THEN *>      -- especially for our friends from K26
          OR (n.type.mode IN (pc.REALs+pc.CPLXs) )
         <* END *>
        THEN 
          pcS.err(102,"(obsolete type cast)") 
        END;
<* END *>
        pcS.get(sy); Expr(ex);
<* IF MCS THEN *>
        IF env.errors.err_cnt = 0 THEN
          pc.const.eval_value(ex);
          IF ex.mode = pc.nd_value THEN
            pcS.warn(340);
          END;
          IF (ex.type.mode = pc.ty_SS) & (ex.type.len > 2) THEN
            pcS.err(err_incomp);
          END;
        END;
<* END *>
        IF pcS.TS_ext() THEN
          IF sy = pcS.rpar THEN
            pcS.get(sy);
            IF NOT pcB.TS_TypeTransferFunction(ex, n.type) THEN
              pcS.err(102,"(not implemented yet)")
            END;
          ELSE
            pcS.get(sy);
            pcS.err(102,"(TopSpeed structured values - use curly braces instead of parentheses)")
          END;
        ELSE
          check_get(pcS.rpar);
          pcB.cast(ex,n.type,FALSE);
        END;
        n:=ex; n.pos:=ps;
        IF pcS.enh_deref & (sy = pcS.bar) THEN
          extend_designator(n);
        END;
      ELSE
        pcO.new(ex,pc.nd_call);
        ex.pos := start_pos;
        ex.l:=n; ex.type:=pcO.invtype; n:=ex;
        ProcCall(n,TRUE);
        ex.end := pcS.txtpos;
        IF pcS.ext() (*& (sy = pcS.bar)*) THEN
          extend_designator(n);
        END;
      END;
    ELSIF sy=pcS.lbrace THEN
      IF n.mode#pc.nd_type THEN pcB.err(n,31)
      ELSE
        constructor(n,n.type);
        n.end := pcS.txtpos;
      END;
    END;
  END iden;

  PROCEDURE Factor(VAR i: pc.NODE);
    VAR v: pc.OBJECT; ps: pc.TPOS; sav_err_cnt:INTEGER;
  BEGIN
    CASE sy OF
      |pcS.val_integer,
       pcS.val_real,pcS.val_long_real,
       pcS.val_string,pcS.val_char,
       pcS.val_cmplx,pcS.val_long_cmplx:
                    pcB.gen_literal(sy,i); pcS.get(sy);
                    i.end := pcS.txtpos;
      |pcS.lpar   : pcS.get(sy); Expr(i);
                    sav_err_cnt := env.errors.err_cnt;
                    check_get(pcS.rpar);
                    IF (sav_err_cnt # env.errors.err_cnt) THEN
                      i.mode := pc.nd_inv;
                    END;
      |pcS.ident  : iden(i);
      |pcS.lbrace : set_constructor(i,pcO.bitset);
      |pcS.not    : ps:=pcS.txtpos; pcS.get(sy); Factor(i);
                    pcB.gen_unary_operator(ps,i,pc.su_not);
      |pcS.array  : pcS.get(sy); check_get(pcS.of);
                    type_qualident(v,FALSE);
                    open_array_constructor(i,v.type);
    ELSE
      pcS.err(81);
      WHILE (sy#pcS.end) & (sy#pcS.rpar) & (sy#pcS.semic) DO pcS.get(sy) END;
      pcO.new(i,pc.nd_inv); i.type:=pcO.invtype;
    END;
  END Factor;

  PROCEDURE Exponent(VAR e1: pc.NODE);
    VAR e2: pc.NODE; ps: pc.TPOS;
  BEGIN
    Factor(e1);
    IF sy=pcS.exponent THEN
      ps:=pcS.txtpos; pcS.get(sy); Exponent(e2);
      pcB.gen_binary_operator(ps,e1,e2,pc.sb_exp);
    END;
  END Exponent;

  PROCEDURE Term(VAR e1: pc.NODE);
    VAR e2: pc.NODE; op: pc.SUB_MODE; ps: pc.TPOS;
  BEGIN
    Exponent(e1);
    LOOP
      ps:=pcS.txtpos;
      CASE sy OF
      | pcS.times  : op:=pc.sb_mul;
      | pcS.div    : op:=pc.sb_div;
      | pcS.mod    : op:=pc.sb_mod;
      | pcS.rem    : op:=pc.sb_rem;
      | pcS.slash  : op:=pc.sb_slash;
      | pcS.and    : op:=pc.sb_cand;
      | pcS.shift_l: op:=pc.sb_shl;
      | pcS.shift_r: op:=pc.sb_shr;
      ELSE RETURN
      END;
      pcS.get(sy);
      Exponent(e2);
      pcB.gen_binary_operator(ps,e1,e2,op);
    END;
  END Term;

  PROCEDURE Simple(VAR e1: pc.NODE);
    VAR e2: pc.NODE; ps: pc.TPOS;
  BEGIN
    IF    sy=pcS.plus  THEN
      ps:=pcS.txtpos; pcS.get(sy); Term(e1);
      pcB.gen_unary_operator(ps,e1,pc.sb_plus);
    ELSIF sy=pcS.minus THEN
      ps:=pcS.txtpos; pcS.get(sy); Term(e1);
      pcB.gen_unary_operator(ps,e1,pc.su_neg);
    ELSE
      Term(e1)
    END;
    LOOP
      CASE sy OF
        |pcS.plus:
          ps:=pcS.txtpos; pcS.get(sy); Term(e2);
          pcB.gen_binary_operator(ps,e1,e2,pc.sb_plus);
        |pcS.minus:
          ps:=pcS.txtpos; pcS.get(sy); Term(e2);
          pcB.gen_binary_operator(ps,e1,e2,pc.sb_minus);
        |pcS.or:
          ps:=pcS.txtpos; pcS.get(sy); Term(e2);
          pcB.gen_binary_operator(ps,e1,e2,pc.sb_cor);
      ELSE RETURN;
      END;
    END;
  END Simple;

VAR
  e2: pc.NODE;
  op: pc.SUB_MODE;
  o : pc.OBJECT;
  ps, start_pos: pc.TPOS;
BEGIN
  start_pos := pcS.txtpos;
  Simple(e1); ps:=pcS.txtpos;
  IF (sy>=pcS.equ) & (sy<=pcS.geq) THEN
    CASE sy OF
      |pcS.equ: op:=pc.sb_equ
      |pcS.neq: op:=pc.sb_neq
      |pcS.lss: op:=pc.sb_lss
      |pcS.gtr: op:=pc.sb_gtr
      |pcS.leq: op:=pc.sb_leq
      |pcS.geq: op:=pc.sb_geq
    END;
    pcS.get(sy); Simple(e2);
    pcB.gen_binary_operator(ps,e1,e2,op);
  ELSIF sy=pcS.in THEN
    pcS.get(sy); Simple(e2);
    IF e2.type.mode IN pc.SETs THEN
      pcB.chk_index(e2.type.base,e1,TRUE);
      pcB.chk_value(e2);
      pcB.binary(ps,e1,e2,pc.sb_in,pcO.boolean);
    ELSE
      pcS.err(30);
    END;
  ELSIF sy=pcS.is THEN
    pcS.get(sy); type_qualident(o,FALSE); o2_type_test(e1,o,FALSE);
  END;
  <* IF TARGET_IDB THEN*>
    IF env.InterViewMode AND NOT (e1.mode IN pc.LVALUEs) THEN
      e1.pos := start_pos;
      e1.end := pcS.txtpos;
    END;
  <* END *>
END Expr;

(*----------------------------------------------------------------*)

<* IF TARGET_386 THEN *>
VAR 
  asm_code: pc.OBJECT;

PROCEDURE new_asm_code(): pc.OBJECT;
  VAR type: pc.STRUCT;
    l,c: LONGINT;
    fnm: env.String;
    name: ARRAY 16 OF CHAR;
BEGIN
  type := pcO.new_type(pc.ty_proctype);
  type.base := pcO.void;
  asm_code := pcO.new_obj(pc.ob_cproc);
  type.pos.unpack(fnm,l,c);
  xcStr.prn_txt(name, "asm&&%d", l);
  pcO.set_name(asm_code,name);
  pcO.alloc(asm_code);
  asm_code.type := type; 
  type.obj := asm_code;
  asm_code.pos := type.pos;
  INCL(asm_code.marks, pc.omark_used_by_code);
  RETURN asm_code
END new_asm_code;

PROCEDURE end_asm_code(n: pc.NODE);
BEGIN
  asm_code.val := n;
  asm_code.end := pcS.txtpos;
  asm_code := NIL; 
END end_asm_code;

PROCEDURE mark_asm_usage(o: pc.OBJECT); -- object 'o' used in assembler code
  VAR u,v: pc.USAGE;
BEGIN
 INCL(o.marks, pc.omark_used_by_code);
 v := NIL;
 u := asm_code.type.use;
 LOOP
   IF u = NIL THEN
     NEW(u); 
     u.obj := o; 
     u.tags := pc.UTAG_SET{pc.utag_read, pc.utag_write};
     IF v = NIL THEN asm_code.type.use := u ELSE v.next := u END;
     EXIT
   END;
   IF u.obj = o THEN u.tags := u.tags+pc.UTAG_SET{pc.utag_read, pc.utag_write}; EXIT END;
   v := u;
   u := u.next;
 END;
END mark_asm_usage;

<* END *>

PROCEDURE StatSeq(VAR stat: pc.NODE; loop,block,except: pc.NODE);

  PROCEDURE m2_with(VAR node: pc.NODE);
    VAR v: pc.OBJECT; i: pc.NODE;
  BEGIN
    pcO.new(node,pc.nd_with);
    pcS.get(sy);
    Expr(i);
    pcB.chk_lvalue(i);
    v := pcO.new_obj(pc.ob_var);
    pcO.alloc(v);
    with[with_cnt]:=v; INC(with_cnt);
    IF (i.type.mode#pc.ty_record) & (i.type.mode#pcO.ty_invtype) THEN
      pcS.error(i.pos,'e',51);
    END;
    v.type := pcO.new_type(pc.ty_pointer);
    v.type.end := pcS.txtpos;
    v.type.base:=i.type;
    node.obj:=v; node.l:=i;
    check_get(pcS.do); StatSeq(node.r,loop,block,except); check_get(pcS.end);
    DEC(with_cnt); with[with_cnt]:=NIL;
  END m2_with;

  PROCEDURE goto(node: pc.NODE);
  VAR
    v: pc.OBJECT;
  BEGIN
    pcS.get(sy);
    IF sy#pcS.ident THEN
      pcS.err(0);
    ELSE
      pcO.fnd_vis(pcO.cur_scope,pcS.name,v);
      IF v.mode#pc.ob_inv THEN
        IF (v.mode # pc.ob_label) THEN pcS.err(288);
        ELSIF pcO.object_scope(v) # pcO.cur_scope THEN pcS.err(287);
        END;
        node.l := v.attr(pc.NODE);
      END;
      pcS.get(sy);
   END;
  END goto;

  PROCEDURE for(node: pc.NODE);
    VAR r: pc.NODE; v: pc.OBJECT; e1,e2: pc.NODE; ps1,ps2: pc.TPOS;
      err: BOOLEAN; (* something wrong with control variable *)
      old_errors: INTEGER;
      for_types: pc.TY_SET;
  BEGIN
    err:=TRUE;
    pcS.get(sy);
    IF sy#pcS.ident THEN
      pcS.err(7); v:=pcO.inv_obj;
    ELSE
      pcO.fnd_vis(pcO.cur_scope,pcS.name,v);
      IF v.mode#pc.ob_inv THEN
        IF pcO.object_scope(v)#pcO.cur_scope THEN pcS.err(128);
        ELSIF ~ (v.mode IN pc.VARs) THEN pcS.err(54);
        ELSIF (v.mode#pc.ob_var) OR (pc.otag_valpar IN v.tags) THEN pcS.err(145);
        ELSIF pcO.otag_exported IN v.tags THEN pcS.err(146);
        ELSIF pc.otag_RO IN v.tags THEN pcS.err(123)
        ELSIF (pc.otag_volatile IN v.tags) OR (pc.ttag_volatile IN v.type.tags)
          THEN pcS.err(163)
        ELSE
          IF pcS.oberon THEN
            IF pcS.ext() THEN
              for_types := pc.INTs + pc.TY_SET{pc.ty_enum, pc.ty_range};
            ELSE
              for_types := pc.INTs;
            END;
            IF ~ (v.type.mode IN for_types) THEN pcS.err(151); END;
          END;
          err:=FALSE;
        END;
        INCL(v.marks,pcO.omark_used_in_for);
      END;
      pcB.chk_ordinal(v.type);
      node.obj:=v;
      pcS.get(sy);
    END;
    ps1:=pcS.txtpos; check_get(pcS.becomes);
    pcO.new(r,pc.nd_node); node.l:=r;
    IF (env.range_check IN env.config.tags) & (v.type.mode=pc.ty_range) THEN
      INCL(node.tags,pc.ntag_chk_range);
    END;
    Expr(e1); ps2:=pcS.txtpos; check_get(pcS.to); Expr(e2);
    pcB.assign_compatibility(ps1,v.type,e1);
    pcB.expression_compatibility_2(ps2,v.type,e2);
    r.l:=e1; r.r:=e2;
    IF sy=pcS.by THEN
      old_errors := env.errors.err_cnt;
      pcS.get(sy); Expr(e1);
      pcB.type_in(e1.type,pc.WHOLEs);
      const_value(r.val,e1, old_errors);
      IF (env.errors.err_cnt=old_errors) & r.val.is_zero() THEN pcS.err(131) END;
    END;
    check_get(pcS.do);
    StatSeq(node.r,loop,block,except);
    check_get(pcS.end);
    IF ~ err & pcB.chk_threat(v,node.r) THEN pcS.error(ps1,"e",147) END;
  END for;

  PROCEDURE case(node: pc.NODE);

    PROCEDURE check_labels(a,b: pc.VALUE);
      VAR c,l: pc.NODE;
    BEGIN
      IF env.errors.err_cnt>0 THEN RETURN END;
      c:=node.r.l; (* case branches list *)
      WHILE c#NIL DO
        l:=c.l; (* labels list *)
        WHILE l#NIL DO
          ASSERT(l.mode=pc.nd_pair);
          IF pcB.cmp_value(pc.sb_leq,l.val,b) &
             pcB.cmp_value(pc.sb_geq,l.l.val,a)
          THEN
            pcS.err(126); RETURN
          END;
          l:=l.next;
        END;
        c:=c.next;
      END;
    END check_labels;

    PROCEDURE Variant(type: pc.STRUCT; casedo: pc.NODE;
                      VAR min,max: pc.VALUE);
    VAR
      e: pc.NODE;
      tail,n: pc.NODE;
      a,b: pc.VALUE;
      old_errors: INTEGER;
      start_pos,  end_pos:  pc.TPOS;
      start_pos2, end_pos2: pc.TPOS;
    BEGIN
      tail:=NIL;
      LOOP
        IF type.mode < pc.ty_free THEN
          old_errors := env.errors.err_cnt;
        ELSE
          ASSERT(env.errors.err_cnt > 0);
          old_errors := env.errors.err_cnt - 1;
        END;
        start_pos := pcS.txtpos;
        Expr(e); n:=e;
        end_pos   := pcS.txtpos;
        pcB.assign_compatibility(n.pos,type,n);
        const_value(a,n, old_errors);
        b:=a;
        IF sy=pcS.range THEN
          old_errors := env.errors.err_cnt;
          pcS.get(sy);
          start_pos2 := pcS.txtpos;
          Expr(e); n:=e;
          end_pos2   := pcS.txtpos;
          pcB.assign_compatibility(n.pos,type,n);
          const_value(b,n, old_errors);
          IF pcB.cmp_value(pc.sb_gtr,a,b) THEN pcS.err(124); b:=a END;
        ELSE
          start_pos2 := start_pos;
          end_pos2 := end_pos;
        END;
        IF min=NIL THEN min:=a;
        ELSIF pcB.cmp_value(pc.sb_lss,a,min) THEN min:=a;
        END;
        IF max=NIL THEN max:=b;
        ELSIF pcB.cmp_value(pc.sb_gtr,a,max) THEN max:=b;
        END;
        check_labels(a,b);
        pcO.tie(casedo.l,tail,pc.nd_pair);
        tail.val:=a; tail.type:=type;
        tail.pos := start_pos;
        tail.end := end_pos;
        pcO.new(tail.l,pc.nd_value);
        tail.l.val:=b;
        tail.l.pos := start_pos2;
        tail.l.end := end_pos2;
        tail.l.type:=type;
        IF sy=pcS.comma THEN pcS.get(sy) ELSE EXIT END;
      END;
      check_get(pcS.colon);
      StatSeq(casedo.r,loop,block,except);
      casedo.end := pcS.txtpos;
      IF (sy#pcS.else) & (sy#pcS.end) THEN check_get(pcS.sep) END;
    END Variant;

  VAR
    tail,else: pc.NODE;
    i: pc.NODE;
    min,max: pc.VALUE;
    base: pc.STRUCT;
    start_pos: pc.TPOS;
  BEGIN
    pcS.get(sy);
    Expr(i); node.l:=i;
    base:=i.type;
    pcB.chk_ordinal(base);
    IF base.mode IN pc.TY_SET{pc.ty_longlongint, pc.ty_longlongcard} THEN
      pcS.error(i.pos,'e',30);
--      pcB.type_incomp(ex.pos,pc.longcard_type, base);
    END;
    check_get(pcS.of);
    pcO.new(else,pc.nd_casedo);
    node.r:=else;
    tail:=NIL; min:=NIL; max:=NIL;
    start_pos := pcS.txtpos;
    WHILE (sy#pcS.end) & (sy#pcS.else) DO
      IF sy=pcS.sep THEN
        start_pos := pcS.txtpos;
        pcS.get(sy)
      ELSE
        pcO.tie(else.l,tail,pc.nd_node);
        tail.pos := start_pos;
        Variant(base,tail,min,max);
        start_pos := pcS.txtpos;
      END;
    END;
    IF sy=pcS.else THEN
      else.mode:=pc.nd_caselse;
      pcS.get(sy); StatSeq(else.r,loop,block,except);
      else.end := pcS.txtpos;
    ELSE pcS.warn(319);
    END;
    IF min#NIL THEN
      ASSERT(max#NIL);
      pcO.new(tail,pc.nd_pair);
      tail.val:=min;
      pcO.new(tail.l,pc.nd_value);
      tail.l.val:=max;
      else.next:=tail;
    END;
    check_get(pcS.end);
    node.r.end := pcS.txtpos;
  END case;

  PROCEDURE guard(VAR x: pc.NODE);
    VAR n,l: pc.NODE;
  BEGIN
    IF x.dynamic_type()=x.type THEN RETURN END;
    IF x.mode=pc.nd_deref THEN n:=x.l ELSE n:=x END;
    IF n.mode=pc.nd_guard THEN n:=n.l END;
    pcO.new(l,pc.nd_eguard);
    l.end := pcS.txtpos;
    IF env.guard_check IN env.config.tags THEN
      INCL(l.tags,pc.ntag_chk_range);
      IF x.mode=pc.nd_deref THEN EXCL(x.tags,pc.ntag_chk_range) END;
    END;
    IF x.mode=pc.nd_deref THEN l.type:=x.l.type ELSE l.type:=x.type END;
    l.l:=n; n:=l;
    IF x.mode=pc.nd_deref THEN x.l:=n ELSE x:=n END;
  END guard;

  PROCEDURE o2_with_variant(n: pc.NODE);
    VAR t,v,x: pc.OBJECT; w: INTEGER; l: pc.NODE; pos: pc.TPOS;
  BEGIN
    w:=with_cnt; with[w]:=pcO.inv_obj;
    designator(n.l); v:=NIL;
    l:=n.l;
    WHILE l.mode=pc.nd_guard DO
      (* We are inside another WITH *)
      l:=l.l;
    END;
    IF l.mode=pc.nd_var THEN
      x:=l.obj;
      IF x.mode=pc.ob_inv THEN (* nothing *)
      ELSIF ~ (x.mode IN pc.OB_SET{pc.ob_var,pc.ob_varpar}) THEN pcS.err(54);
      ELSE
        v:=x; with[w]:=v;
      END;
      IF x.type.mode = pc.ty_pointer THEN
        IF pcO.otag_exported IN x.tags THEN pcS.err(146);
        ELSIF x.mode=pc.ob_varpar THEN pcS.warn(317)
        END;
      END;
    ELSE
      pcB.err(l,54);
    END;
    pos:=pcS.txtpos;
    check_get(pcS.colon);
    type_qualident(t,FALSE);
    with_type[w]:=t.type;
    o2_type_test(n.l,t,FALSE);
    check_get(pcS.do);
    pcO.new(n.r,pc.nd_node);
    INC(with_cnt);
    n.r.pos := pcS.txtpos;
    StatSeq(n.r.l,loop,block,except);
    n.r.end := pcS.txtpos;
    DEC(with_cnt);
    IF (v#NIL) & (v.type.mode=pc.ty_pointer) & pcB.chk_threat(v,n.r.l) THEN
      pcS.error(pos,"e",147);
    END;
  END o2_with_variant;

  PROCEDURE o2_with(VAR x: pc.NODE);
    VAR n: pc.NODE;
  BEGIN
    pcO.new(x,pc.nd_if);
    n:=x; INCL(n.tags, pc.ntag_substitute);
    pcS.get(sy);
    LOOP
      o2_with_variant(n);
      IF sy#pcS.sep THEN EXIT END;
      pcS.get(sy);
      pcO.new(n.r.r,pc.nd_if);
      n.r.r.end := pcS.txtpos;
      n:=n.r.r;
      n.type:=pcO.void;
      INCL(n.tags, pc.ntag_substitute);
      INCL(n.tags, pc.ntag_elsif_node);
    END;
    IF sy=pcS.else THEN
      pcS.get(sy); StatSeq(n.r.r,loop,block,except);
    ELSIF env.guard_check IN env.config.tags THEN
      pcO.new(n.r.r,pc.nd_wtrap);
      n.r.r.end := pcS.txtpos;
      n.r.r.type:=pcO.void;
    END;
    check_get(pcS.end);
  END o2_with;


VAR
  n: pc.NODE;
  type: pc.STRUCT;

  PROCEDURE assignment(VAR x: pc.NODE; lval: pc.NODE);
  BEGIN
    pcO.new(x,pc.nd_assign);
    x.end := pcS.txtpos;
    IF (lval.type.mode=pc.ty_record) & (lval.type.flag=pc.flag_o2) &
       (env.guard_check IN env.config.tags)
    THEN
      guard(lval);
    END;
    n:=lval; type:=n.type;
    pcB.chk_lvalue(n);
    IF n.mode=pc.nd_var THEN x.obj:=n.obj; n:=NIL END;
    x.l:=n;
    pcS.get(sy);
    Expr(x.r);
    pcB.assign_compatibility(x.pos,type,x.r);
  END assignment;

  VAR
    x, i, tail: pc.NODE;
    branch, elsif : pc.NODE;
    start_pos, seq_start_pos, seq_end_pos: pc.TPOS;
    ex: pc.NODE;
    v: pc.OBJECT;

BEGIN
  tail:=NIL;
  LOOP
    x:=NIL;
    start_pos := pcS.txtpos;
    CASE sy OF
     |pcS.ident :
        designator(i);
        IF NOT (i.mode IN pc.NODE_SET{pc.nd_index, pc.nd_field}) THEN
          i.pos := start_pos;
        END;
        i.end := pcS.txtpos;
        IF sy=pcS.becomes THEN
          assignment(x, i);
        ELSIF sy=pcS.colon THEN
          IF i.mode # pc.nd_label THEN
            pcS.err(286);
          ELSE
            IF pcO.object_scope(i.obj) # pcO.cur_scope THEN
              pcS.error(i.pos, 'e', 287);
            END;
            x     := i;
            x.pos := start_pos;
            x.end := pcS.txtpos;
            start_pos := pcS.txtpos;
          END;
          pcS.get(sy);
        ELSIF sy=pcS.equ THEN
          pcS.expc(pcS.becomes);
          pcS.get(sy); Expr(i);
        ELSIF i.mode=pc.nd_sproc THEN
          sProcCall(i); x:=i;
          IF (x.type.mode#pcO.ty_invtype) & (x.type.mode#pc.ty_void) THEN
            pcS.err(90)
          END;
          IF (x.mode=pc.nd_sproc) &
             (x.sub=pc.sp_assert)
          THEN
            IF env.assert_check IN env.config.tags THEN
              INCL(x.tags,pc.ntag_chk_range);
            END;
            IF env.errors.err_cnt=0 THEN pc.const.eval_value(x.r);
            ELSIF pcS.oberon THEN block.sub:=pc.su_bits;
            END;
            IF (x.r.mode=pc.nd_value) &
               x.r.val.is_zero() &
               pcS.oberon
            THEN
              block.sub:=pc.su_bits;
              INCL(x.tags,pc.ntag_chk_range);
            END;
          ELSIF (x.mode=pc.nd_sproc) &
                (x.sub=pc.sp_halt) &
                pcS.oberon
          THEN block.sub:=pc.su_bits;
          END;
        ELSIF i.type.mode=pcO.ty_invtype THEN
          skip_parms;
        ELSIF i.mode=pc.nd_type THEN
          IF pcS.enh_deref & (sy = pcS.lpar)THEN
            pcS.get(sy); Expr(ex);
            check_get(pcS.rpar);
            IF pcS.TS_ext() THEN
              IF NOT pcB.TS_TypeTransferFunction(ex, i.type) THEN
                pcS.err(102,"(not implemented yet)")
              END;
            ELSE
              pcB.cast(ex,i.type,FALSE);
            END;
            IF sy = pcS.bar THEN
              extend_designator(ex);
              IF sy=pcS.becomes THEN
                assignment(x, ex);
              ELSE
                pcS.error(start_pos,'e',121,i.obj.name^);
                skip_parms;
              END;
            ELSE
              pcS.error(start_pos,'e',121,i.obj.name^);
              skip_parms;
            END;
          ELSE
            pcS.error(start_pos,'e',121,i.obj.name^);
            skip_parms;
          END;
        ELSIF i.type.mode = pc.ty_proctype THEN
          pcO.new(x,pc.nd_call);
          x.l:=i;
          x.pos:=start_pos;
          ProcCall(x,FALSE);
          x.end := pcS.txtpos;
          IF x.type.mode#pc.ty_void THEN
            pcO.new(i,pc.nd_eval);
            i.l:=x; i.type:=pcO.void;
            i.pos:=start_pos;
            i.end := pcS.txtpos;
            x:=i;
          END;
        ELSE
          pcS.expc(pcS.becomes);
          skip_parms;
        END;
     |pcS.if:
        pcO.new(x,pc.nd_if);
        pcO.new(x.r,pc.nd_node);
        x.r.end := pcS.txtpos;
        pcS.get(sy);
        n:=x;
        branch := x.r;
        elsif := NIL;
        LOOP
          boolexpr(n.l);
          seq_start_pos := pcS.txtpos;
          branch.pos := seq_start_pos;
          check_get(pcS.then);
          StatSeq(n.r.l,loop,block,except);
          seq_end_pos := pcS.txtpos;
          CASE sy OF
          | pcS.elsif:
            IF elsif # NIL THEN
              elsif.end := seq_end_pos;
            END;
            pcS.get(sy);
            pcO.new(elsif,pc.nd_if);
            pcO.new(elsif.r, pc.nd_node);
            n.r.r   := elsif;
            n       := elsif;
            n.type  := pcO.void;

            INCL(elsif.tags, pc.ntag_elsif_node);
            elsif.pos  := pcS.prevpos;
            branch.end := pcS.txtpos;
            branch     := elsif.r;
          ELSE
            IF elsif # NIL THEN
              elsif.end := seq_end_pos;
            END;
            IF sy=pcS.else THEN
              branch.pos := seq_end_pos;
              pcS.get(sy);
              StatSeq(n.r.r,loop,block,except);
              branch.end := pcS.txtpos;
            ELSE
              branch.end := seq_end_pos;
            END;
            EXIT;
          END;
        END;
        check_get(pcS.end);
        x.end := pcS.txtpos;
     |pcS.while:
        pcO.new(x,pc.nd_while);
        pcS.get(sy); boolexpr(x.l);
        check_get(pcS.do); StatSeq(x.r,loop,block,except);
        check_get(pcS.end);
     |pcS.repeat:
        pcO.new(x,pc.nd_repeat);
        pcS.get(sy); StatSeq(x.l,loop,block,except);
        check_get(pcS.until); boolexpr(x.r);
     |pcS.loop  :
        pcO.new(x,pc.nd_loop);
        pcS.get(sy); StatSeq(x.r,x,block,except);
        check_get(pcS.end);
     |pcS.exit  :
        IF loop=NIL THEN pcS.err(125) END;
        pcO.new(x,pc.nd_exit); x.r:=loop;
        pcS.get(sy);
        x.end := pcS.txtpos;
     |pcS.return:
        pcO.new(x,pc.nd_return); pcS.get(sy);
        x.r:=block; block.sub:=pc.su_bits;
        IF pcS.oberon &
           (pcO.cur_scope.mode#pc.ty_proctype) &
           ~ pcS.ext()
        THEN
          pcS.err(99);
        ELSE
          type:=pcO.cur_scope.base;
          IF type.mode#pc.ty_void THEN
            Expr(x.l); pcB.assign_compatibility(x.pos,type,x.l);
          END;
        END;
     |pcS.goto  : pcO.new(x,pc.nd_goto); goto(x);
     |pcS.for   : pcO.new(x,pc.nd_for);  for(x);
     |pcS.case  : pcO.new(x,pc.nd_case); case(x);
     |pcS.with  :
        IF pcS.oberon THEN o2_with(x) ELSE m2_with(x) END;
     |pcS.retry :
        IF except=NIL THEN pcS.err(149) END;
        pcO.new(x,pc.nd_retry); pcS.get(sy); x.r:=except;
  <* IF TARGET_386 THEN *>
     |pcS.asm   :
        pcO.new(x,pc.nd_sproc);
        x.sub := pc.sp_code;
        x.obj := new_asm_code();
        pcO.new(n,pc.nd_value);
        n.type := pcO.void;
        n.val := asm.Asm (mark_asm_usage);
        end_asm_code(n);
        pcS.get(sy);
        x.r := n;
  <* END *>
  <* IF MCS THEN *>
     |pcS.inline, pcS.setreg:
        pcO.fnd_vis(pcO.cur_scope, pcS.name, v);
        IF v.mode # pc.ob_inv THEN
          pcS.warn(342, pcS.name);
        END;
        asm.Ignore();
        pcS.get(sy);
(*
        pcO.new(x,pc.nd_sproc);
        x.sub := pc.sp_code;
        x.obj := v;
        pcS.get(sy);
        sProcCall(x);
        x:= NIL;
*)
  <* END *>
     |pcS.semic :
     |pcS.end,pcS.else,pcS.elsif,
      pcS.until,pcS.sep,pcS.finally,pcS.except: EXIT
    ELSE
      pcS.err(86); pcS.get(sy);
    END;
    IF x#NIL THEN x.type:=pcO.void; pcO.app(stat,tail,x) END;
    IF x # NIL THEN
      x.pos := start_pos;
      x.end := pcS.txtpos;
    END;
    CASE sy OF
     |pcS.end,pcS.else,pcS.elsif,
      pcS.until,pcS.sep,pcS.finally,pcS.except: EXIT
     |pcS.semic: pcS.get(sy);
    ELSE
      IF (x = NIL) OR (x.mode # pc.nd_label) THEN
        pcS.expc(pcS.semic);
      END;
    END;
  END;
END StatSeq;

(*----------------------------------------------------------------*)


PROCEDURE sysflag_expr( VAR flag: pc.Lang; maybeAttrs: BOOLEAN;
                        VAR exceptnode: pc.OBJECT; VAR wasDirect: BOOLEAN
                      ): pc.ProcAttrSet;
  VAR n: pc.Lang; m: LONGINT; nm: pc.NAME; v: pc.VALUE; i: pc.NODE;
      old_errors: INTEGER;
      VAR attrs:pc.ProcAttrSet;
BEGIN
  wasDirect:=FALSE;
  IF maybeAttrs THEN
    exceptnode:=NIL;
  END;
  attrs:= pc.ProcAttrSet{};
  old_errors := env.errors.err_cnt;
  LOOP
    Expr(i);
    const_value(v,i, old_errors);
    IF env.errors.err_cnt # old_errors THEN (* nothing *)
    ELSIF i.type.mode IN pc.WHOLEs THEN
      n:=SYSTEM.VAL(pc.Lang,v.get_integer());
      IF (VAL(SHORTINT,n)<0) OR (n>pc.code.max_sysflag) THEN pcS.err(150);
      ELSE flag:=n;
      END;
      wasDirect:=TRUE;
    ELSIF i.type.mode=pc.ty_SS THEN
      m:=0;
      LOOP
        IF (m=LEN(nm)-1) OR (m=i.type.len) THEN EXIT END;
        int_tmp.index_get(m,v);
        nm[m]:=CHR(int_tmp.get_integer());
        IF nm[m]=0X THEN EXIT END;
        INC(m);
      END;
      nm[m]:=0X;
      IF    nm="Oberon"    THEN flag:=pc.flag_o2;        wasDirect:=TRUE;
      ELSIF nm="Modula"    THEN flag:=pc.flag_m2;        wasDirect:=TRUE;
      ELSIF nm="C"         THEN flag:=pc.flag_c;         wasDirect:=TRUE;
      ELSIF nm="SL1"       THEN flag:=pc.flag_sl1;       wasDirect:=TRUE;
      ELSIF nm="Pascal"    THEN flag:=pc.flag_p;         wasDirect:=TRUE;
      ELSIF nm="BNRP"      THEN flag:=pc.flag_bnrp;      wasDirect:=TRUE;
      ELSIF nm="StdCall"   THEN flag:=pc.flag_stdcall;   wasDirect:=TRUE;
      ELSIF nm="VMCall"    THEN flag:=pc.flag_vmcall;    wasDirect:=TRUE;
      ELSIF nm="LightCall" THEN flag:=pc.flag_lightcall; wasDirect:=TRUE;
      ELSIF nm="JavaCall"  THEN flag:=pc.flag_javacall;  wasDirect:=TRUE;
      ELSIF nm="SysCall"   THEN flag:=pc.flag_syscall;   wasDirect:=TRUE;
      ELSIF (nm="alwaysinline") OR (nm="AlwaysInline") THEN
        IF ~maybeAttrs THEN
          pcS.err(err_incomp);
        ELSE
          INCL(attrs, pc.alwaysinline);
        END;
      ELSIF (nm="neverinline") OR (nm="NeverInline") THEN
        IF ~maybeAttrs THEN
          pcS.err(err_incomp);
        ELSE
          INCL(attrs, pc.neverinline);
        END;
      ELSIF (nm="strictcallconv") OR (nm="StrictCallConv") THEN
        IF ~maybeAttrs THEN
          pcS.err(err_incomp);
        ELSE
          INCL(attrs, pc.strictcallconv);
          INCL(attrs, pc.neverinline);
        END;
      ELSIF (nm="except") THEN
        IF ~maybeAttrs THEN
          pcS.err(err_incomp);
        ELSE
          INCL(attrs, pc.except);
          INCL(attrs, pc.neverinline);
        END;
      ELSIF (nm="throws") THEN
        IF ~maybeAttrs THEN
          pcS.err(err_incomp);
        ELSE
          INCL(attrs, pc.throws);
        END;
      ELSE pcS.err(150);
      END;
    ELSIF i.type.mode=pc.ty_proctype THEN
      IF (pc.except IN attrs) AND (exceptnode = NIL) THEN
        exceptnode := i.type.obj;
        INCL(exceptnode.marks, pc.omark_used_by_code);
       ELSE
        pcS.err(err_incomp);
      END;
    ELSE
      pcS.err(err_incomp);
    END;
    IF ~maybeAttrs OR (sy#pcS.comma) THEN
      EXIT;
    END;
    pcS.get(sy);
  END;
  RETURN attrs;
END sysflag_expr;

PROCEDURE check_sysflag_ex( VAR flag: pc.Lang; maybeAttrs: BOOLEAN;
                            VAR exceptnode: pc.OBJECT; VAR wasDirect: BOOLEAN
                           ): pc.ProcAttrSet;
VAR attrs:pc.ProcAttrSet;
BEGIN
  attrs := pc.ProcAttrSet{};
  IF cu#NIL THEN flag:=pcO.cur_scope.flag;
  ELSIF pcS.oberon THEN flag:=pc.flag_o2;
  ELSE flag:=pc.flag_m2;
  END;
  IF sy=pcS.lbr THEN
    pcS.get(sy);
    IF ~ pcO.system_imported & ~ pcS.ext() THEN
      pcS.err(102,"(direct language specification)")
    END;
    attrs := sysflag_expr(flag, maybeAttrs, exceptnode, wasDirect);
    check_get(pcS.rbr);
  ELSE
    wasDirect := FALSE;
  END;
  RETURN attrs;
END check_sysflag_ex;

PROCEDURE check_sysflag(VAR flag: pc.Lang);
VAR wasDirect: BOOLEAN;
    exceptnode: pc.OBJECT;
BEGIN
   SYSTEM.EVAL(check_sysflag_ex(flag, FALSE, exceptnode, wasDirect));
END check_sysflag;

PROCEDURE check_sysflag2(VAR flag: pc.Lang);
VAR wasDirect: BOOLEAN;
    exceptnode: pc.OBJECT;
BEGIN
   SYSTEM.EVAL(check_sysflag_ex(flag, FALSE, exceptnode, wasDirect));
   IF ~wasDirect THEN
     IF cu.type.flag = pc.flag_o2 THEN
       flag := pc.flag_o2;
     ELSE
       flag := pc.flag_m2;
     END;
   END;
END check_sysflag2;

PROCEDURE check_sysflag3(VAR flag: pc.Lang);
VAR wasDirect: BOOLEAN;
    exceptnode: pc.OBJECT;
BEGIN
   SYSTEM.EVAL(check_sysflag_ex(flag, FALSE, exceptnode, wasDirect));
   IF ~wasDirect THEN
       flag := cu.type.flag;
   END;
END check_sysflag3;

PROCEDURE ^const_expr(v: pc.NODE);
PROCEDURE check_var_addr(): pc.NODE;
VAR
  s1,s2: ARRAY 70 OF CHAR;
  e: pc.NODE;
BEGIN
  e:=NIL;
  IF sy=pcS.lbr THEN
    pcS.get(sy);
    IF sy#pcS.rbr THEN
      Expr(e);
      pcB.eval_value(e);
      const_expr(e);
      IF e.type # pcO.addr THEN
        IF pcB.type_string(s1, e.type) & pcB.type_string(s2, pcO.addr) THEN
          pcS.error(e.pos,'e',err_incomp_ex,s1,s2);
        ELSE
          pcS.error(e.pos,'e',err_incomp);
        END;
        e:=NIL;
      END;
    END;
    check_get(pcS.rbr);
  END;
  RETURN e;
END check_var_addr;

PROCEDURE check_protection(VAR s: pc.VALUE; VAR flag: pc.Lang);
  VAR i: pc.NODE;  old_errors: INTEGER; VAR exceptnode: pc.OBJECT;
      wasDirect:BOOLEAN;
BEGIN
  s:=protection;
  IF sy=pcS.lbr THEN
    pcS.get(sy);
    IF sy#pcS.rbr THEN
      old_errors := env.errors.err_cnt;
      Expr(i);
      pcB.assign_compatibility(pcS.txtpos,pcO.protection,i);
      const_value(protection,i, old_errors);
    END;
    check_get(pcS.rbr);
  END;
  IF sy=pcS.lbr THEN
    pcS.get(sy);
    IF ~ pcO.system_imported & ~ pcS.ext() THEN pcS.err(102,"") END;
    SYSTEM.EVAL(sysflag_expr(flag, FALSE, exceptnode, wasDirect));
    check_get(pcS.rbr);
  END;
END check_protection;

PROCEDURE check_export_mark(o: pc.OBJECT; m2_ro: BOOLEAN);
BEGIN
  IF pcS.oberon THEN
    IF sy=pcS.times THEN
      IF o.mode=pc.ob_inv THEN
      ELSIF (o.host # NIL) & (o.host.mode=pc.ty_record) THEN
        INCL(o.tags,pc.otag_public_f);
      ELSIF pcO.level=0 THEN
        INCL(o.tags,pcO.otag_exported);
      ELSE pcS.err(95);
      END;
      pcS.get(sy);
    ELSIF sy=pcS.minus THEN
      IF o.mode=pc.ob_inv THEN
      ELSIF pcO.level#0 THEN
        pcS.err(95);
      ELSIF o.mode=pc.ob_var THEN
        o.tags:=o.tags+pc.OTAG_SET{pc.otag_RO_public,pcO.otag_exported};
      ELSIF o.mode=pc.ob_field THEN
        o.tags:=o.tags+pc.OTAG_SET{pc.otag_RO_public,pc.otag_public_f};
      ELSE
        pcS.err(54);
      END;
      pcS.get(sy);
    END;
  ELSE
    IF (o.mode=pc.ob_field) & pcO.def THEN INCL(o.tags,pc.otag_public_f) END;
    IF m2_ro THEN
      (* check Modula-2 "READ ONLY" mark *)
      IF sy=pcS.minus THEN
        IF ~ pcS.ext() THEN pcS.err(102,"(read-only tag)") END;
        IF pcO.def THEN INCL(o.tags,pc.otag_RO_public)
        ELSE pcS.err(94)
        END;
        pcS.get(sy);
      END;
    ELSIF (sy=pcS.times) & pcS.ext() THEN
      IF o.mode=pc.ob_inv THEN
      ELSIF pcO.level=0 THEN INCL(o.tags,pcO.otag_exported);
      ELSE pcS.err(95);
      END;
      pcS.get(sy);
    END;
  END;
END check_export_mark;

PROCEDURE formal_type(VAR t: pc.STRUCT; proc: pc.STRUCT);
VAR
  o: pc.OBJECT;
  dim1,dim2,i: LONGINT;
  a: pc.STRUCT;
  ps: pc.TPOS;
BEGIN
  dim1:=0;
  ps := pcS.txtpos;
  WHILE sy=pcS.array DO
    pcS.get(sy); check_get(pcS.of); INC(dim1);
  END;
  type_qualident(o,TRUE); t:=o.type;
  IF t.mode=pc.ty_array_of THEN dim2:=t.len ELSE dim2:=0 END;
  i:=1;
  WHILE i<=dim1 DO
    a := pcO.new_type(pc.ty_array_of);
    a.pos := ps;
    a.end := pcS.txtpos;
    (* for "C" procedures ARRAY OF parameter has no length *)
    IF proc.flag IN pc.LangSet{ pc.flag_c, pc.flag_p,
                                pc.flag_stdcall, pc.flag_vmcall, pc.flag_lightcall,
                                pc.flag_syscall, pc.flag_javacall }
    THEN
      a.flag:=proc.flag;
    END;
    pcO.set_type_base(a,t);
    a.len:=i+dim2; t:=a; INC(i);
  END;
END formal_type;

PROCEDURE const_expr(v: pc.NODE);
(*
        приходится так сложно определять является ли выражение константным
        из-за выражений типа SIZE(REAL)+1, для которых back-end может
        отказаться сообщить значение SIZE(REAL)
*)
  PROCEDURE const(v: pc.NODE): BOOLEAN;
    VAR n: pc.NODE;
  BEGIN
    IF v.mode=pc.nd_value THEN RETURN TRUE END;
    IF v.mode=pc.nd_proc  THEN RETURN TRUE END;
    IF v.mode=pc.nd_unary THEN
      CASE v.sub OF
        |pc.su_bits,pc.su_bytes,pc.su_size,pc.su_min,pc.su_max:
          RETURN TRUE;
        |pc.su_abs,pc.su_neg,pc.su_cap,pc.su_odd,pc.su_not,pc.su_conv,
         pc.su_compl,pc.su_entier,pc.su_length,pc.su_im,pc.su_re:
          RETURN const(v.l);
      ELSE RETURN FALSE;
      END;
    END;
    IF v.mode=pc.nd_binary THEN
      CASE v.sub OF
        |pc.sb_equ,pc.sb_neq,pc.sb_lss,pc.sb_leq,pc.sb_gtr,pc.sb_geq,
         pc.sb_in,pc.sb_mul,pc.sb_div,pc.sb_mod,pc.sb_rem,pc.sb_slash,
         pc.sb_plus,pc.sb_minus,pc.sb_and,pc.sb_or,pc.sb_xor,pc.sb_bic,
         pc.sb_cand,pc.sb_cor,pc.sb_ash,pc.sb_lsh,pc.sb_rot,pc.sb_addadr,
         pc.sb_subadr,pc.sb_difadr,pc.sb_concat,pc.sb_cmplx,pc.sb_exp:
          RETURN const(v.l) & const(v.r);
      ELSE
        RETURN FALSE;
      END;
    END;
    IF v.mode=pc.nd_var THEN RETURN v.obj.mode=pc.ob_cons END;
    IF v.mode=pc.nd_lconv THEN RETURN const(v.l) END;
    IF v.mode=pc.nd_node THEN RETURN const(v.l) END;
    IF v.mode=pc.nd_index THEN RETURN const(v.l) & const(v.r) END;
    IF v.mode=pc.nd_field THEN RETURN const(v.l) END;
    IF v.mode=pc.nd_aggregate THEN
      n:=v.l;
      WHILE n#NIL DO
        IF ~ const(n) THEN RETURN FALSE END;
        n:=n.next;
      END;
      RETURN TRUE;
    END;
    RETURN FALSE;
  END const;
BEGIN
  IF env.errors.err_cnt#0 THEN RETURN END;
  pcB.eval_value(v);
  IF const(v) THEN RETURN END;
  pcS.error(v.pos,'e',87);
END const_expr;

PROCEDURE special_varpar(VAR mode: pc.OB_MODE; VAR tags: pc.OTAG_SET);
BEGIN
  mode:=pc.ob_varpar; tags:=pc.OTAG_SET{};
  IF sy=pcS.lbr THEN
    pcS.get(sy);
    IF ~ pcO.system_imported & ~ pcS.ext() THEN
      pcS.err(102,"(special kinds of parameters)")
    END;
    IF sy = pcS.ident THEN
      IF pcS.name = "NIL" THEN
        mode:=pc.ob_var;
        tags:=tags + pc.OTAG_SET{pcO.otag_varpar_nil,pc.otag_valpar};
      ELSE pcS.err(160)
      END;
      pcS.get(sy);
    ELSE pcS.expc(pcS.ident)
    END;
    check_get(pcS.rbr);
  END;
END special_varpar;

<* IF TARGET_IDB THEN *>
VAR
  TypeUse: pc.NODE;
<* END *>

PROCEDURE proc_head(type: pc.STRUCT; this: pc.OBJECT);
  VAR
    mode : pc.OB_MODE;
    tags : pc.OTAG_SET;
    l,p,o: pc.OBJECT;
    parm : pc.STRUCT;
    ptr  : pc.STRUCT;
    tail : pc.OBJECT;
    start_type: pc.TPOS;
    <* IF TARGET_IDB THEN *>
    type_use: pc.NODE;
    <* END *>
BEGIN
  ASSERT(type.prof=NIL);
  IF this#NIL THEN pcO.alloc_param(this,type,tail) END;
  type.base:=pcO.void;
  IF sy#pcS.lpar THEN RETURN END;
  pcS.get(sy);
  IF sy#pcS.rpar THEN
    LOOP
      tags:=pc.OTAG_SET{};
      IF    sy=pcS.var  THEN pcS.get(sy); special_varpar(mode,tags);
      ELSIF sy=pcS.seq  THEN EXIT
      ELSIF sy=pcS.rpar THEN pcS.expc(pcS.ident); EXIT
      ELSE mode:=pc.ob_var; tags:=pc.OTAG_SET{pc.otag_valpar};
      END;
      l:=NIL;
      LOOP
        IF sy=pcS.ident THEN
          o := pcO.new_obj(mode);
          pcO.set_name(o,pcS.name);
          pcO.alloc_param(o,type,tail);
          o.tags:=o.tags + tags;
          IF l=NIL THEN l:=o END;
          pcS.get(sy);
          o.end := pcS.txtpos;
          check_sysflag(o.flag);
          IF sy=pcS.minus THEN
            IF ~ pcS.ext() THEN pcS.err(102,"(read-only parameters)") END;
            IF pcO.def THEN pcS.err(93) END;
            IF mode=pc.ob_var THEN INCL(o.tags,pc.otag_RO)
            ELSE pcS.err(98)
            END;
            pcS.get(sy);
          END;
          IF (sy=pcS.neq) & pcS.ext()THEN
            INCL(o.tags,pc.otag_secretvar);
            pcS.get(sy);
          END;
          IF sy=pcS.becomes THEN
            IF ~ pcS.ext() THEN pcS.err(102,"(parameter value by default)") END;
            IF mode#pc.ob_var THEN pcS.err(98) END;
            pcS.get(sy);
            Expr(o.val);
          END;
        ELSE check_get(pcS.ident);
        END;
        IF    sy=pcS.comma THEN pcS.get(sy)
        ELSIF sy=pcS.ident THEN pcS.expc(pcS.comma)
        ELSE EXIT
        END
      END;
      check_get(pcS.colon);
      start_type := pcS.txtpos;
      IF pcS.oberon THEN
        type_definition(parm,TRUE);
        <* IF target_idb THEN *>
          type_use := TypeUse;
        <* END *>
      ELSE
        formal_type(parm,type);
        <* IF target_idb THEN *>
          pcO.new(type_use, pc.nd_type);
          type_use.pos := start_type;
          type_use.end := pcS.txtpos;
        <* END *>
      END;

      ptr:=NIL;
      WHILE l#NIL DO
        IF pcO.otag_varpar_nil IN l.tags THEN
          IF ptr = NIL THEN ptr := pcO.new_type(pc.ty_pointer); ptr.base:=parm END;
          l.type:=ptr;
        ELSE
          l.type:=parm;
        END;
        IF l.val#NIL THEN
          pcB.assign_compatibility(l.pos,parm,l.val);
          const_expr(l.val);
        END;
        <* IF TARGET_IDB THEN *>
        l.type_use := type_use;
        <* END *>
        l:=l.next
      END;
      IF sy = pcS.rpar THEN EXIT END;
      check_get(pcS.semic);
    END;
    IF sy=pcS.seq THEN
      IF ~ pcS.ext() THEN pcS.err(102,"(SEQ parameter)") END;
      pcS.get(sy);
      IF sy#pcS.ident THEN pcS.err(7)
      ELSE
        o := pcO.new_obj(pc.ob_seq);
        pcO.set_name(o,pcS.name);
        pcO.alloc_param(o,type,tail);
        (* o.tags:={pc.otag_RO}; ???? *)
        pcS.get(sy);
        check_sysflag(o.flag);
        IF (sy=pcS.neq) & pcS.ext() THEN
          INCL(o.tags,pc.otag_secretvar);
          pcS.get(sy);
        END;
        check_get(pcS.colon);
        o.type := pcO.new_type(pc.ty_array_of);
        IF pcS.oberon THEN type_definition(parm,FALSE)
        ELSE type_qualident(p,FALSE); parm:=p.type;
        END;
        IF (parm.mode=pc.ty_loc) OR
           (parm.mode=pc.ty_array) & (parm.base.mode=pc.ty_loc)
        THEN pcO.set_type_base(o.type,parm);
        ELSE pcS.err(49); o.type.base:=pcO.invtype
        END;
        o.type.len:=1;
      END;
    END;
  END;
  type.end:=pcS.txtpos;
  check_get(pcS.rpar);
  IF sy=pcS.colon THEN
    pcS.get(sy);
    start_type := pcS.txtpos;
    IF pcS.oberon THEN
      type_definition(parm,FALSE);
      <* IF TARGET_IDB THEN *>
        type_use := TypeUse;
      <* END *>
    ELSE
      type_qualident(o,FALSE); parm:=o.type;
      <* IF target_idb THEN *>
        pcO.new(type_use, pc.nd_type);
        type_use.pos := start_type;
        type_use.end := pcS.txtpos;
        type_use.obj := o;
      <* END *>
    END;
    <* IF TARGET_IDB THEN *>
      IF type.obj # NIL THEN
        IF type.obj.mode IN pc.PROCs THEN
          type.obj.type_use := type_use;
        ELSIF type.obj.mode = pc.ob_type THEN
        ELSE
          ASSERT(FALSE);
        END;
      END;
    <* END *>
    IF ~ (parm.mode IN pc.code.FRETs) & (parm.mode#pcO.ty_invtype) THEN
      pcS.err(43); parm:=pcO.invtype;
    ELSIF pcS.oberon & (parm.mode IN pc.TY_SET{pc.ty_array,pc.ty_record}) & ~ pcS.ext() THEN
      pcS.err(102,"(function result is array or record)");
    END;
    type.base:=parm;
  END;
END proc_head;

PROCEDURE prochead(type: pc.STRUCT; this: pc.OBJECT);
BEGIN
  pcO.enter_scope(type);
  proc_head(type,this);
  pcO.exit_scope;
END prochead;

TYPE RIDER=RECORD (pc.RIDER) src,dst: pc.STRUCT END;

PROCEDURE (VAR r: RIDER) object(o: pc.OBJECT);
  VAR t: pc.STRUCT;
BEGIN
  IF o.type=r.dst THEN o.type:=r.src END;
  ASSERT(o.host#r.dst);
  t:=o.type;
  LOOP
    (* t.inx можно не проверять, он обязательно ordinal *)
    ASSERT(t.inx#r.dst);
    IF t.base=r.dst THEN t.base:=r.src END;
    IF (t.base=NIL) OR (t.base.obj#NIL) THEN EXIT END;
    t:=t.base;
  END;
END object;

PROCEDURE copy_type(VAR dst: pc.STRUCT; src: pc.STRUCT);
  VAR r: RIDER;
BEGIN
  IF dst=src THEN RETURN END;
  ASSERT(dst.mode=pcO.ty_undef);
  r.src:=src; r.dst:=dst;
  cu.type.objects(r); (* итерируются лишние объекты !!!! *)
  dst:=src;
END copy_type;

PROCEDURE type_definition(VAR t: pc.STRUCT; en_array_of: BOOLEAN);

  VAR
    ps: pc.TPOS;

  PROCEDURE type_obj(s: pc.STRUCT);
  BEGIN
    pcO.new_anonim_obj(s.obj,pc.ob_type);
    s.obj.type:=s;
    pcO.alloc(s.obj);
    pcO.dcl(pcO.cur_scope,s.obj);
  END type_obj;

  PROCEDURE new_type(VAR t: pc.STRUCT; md: pc.TY_MODE);
  BEGIN
    IF (tbd=NIL) OR (tbd.type=NIL) OR (tbd.type#t) THEN
      t := pcO.new_type(md);
    ELSIF tbd.mode=pc.ob_inv THEN
      t := pcO.new_type(md); tbd.type:=t;
    ELSE
      ASSERT(t.obj=tbd);
      pcO.ini_type(t,md);
      t.obj:=tbd;
    END;
    IF (tbd#NIL) & (tbd.type=t) THEN t.obj:=tbd;
    ELSIF md IN pc.TY_SET{pc.ty_enum,pc.ty_record(*,pc.ty_array*)} THEN type_obj(t);
    END;
  END new_type;

  PROCEDURE enum(VAR t: pc.STRUCT);
    VAR o,tail: pc.OBJECT;
  BEGIN
    IF pcS.oberon & ~pcS.ext() THEN pcS.err(143) END;
    new_type(t,pc.ty_enum);
    pcS.get(sy);
    t.len:=0;
    t.max:=new_value(pcS.txtpos,0);
    IF sy=pcS.rpar THEN pcS.err(7); pcS.get(sy); RETURN END;
    LOOP
      IF sy=pcS.ident THEN
        o := pcO.new_obj(pc.ob_cons);
        pcO.set_name(o,pcS.name);
        pcO.alloc_enum(o,t,t.prof,tail);
        pcO.dcl(pcO.cur_scope,o);
        o.type:=t;
        o.flag:=cu.type.flag;
        IF (pcS.oberon) & (pcO.otag_exported IN t.obj.tags) THEN
          INCL(o.tags, pcO.otag_exported);
        END;
        pcO.new(o.val,pc.nd_value);
        o.val.type:=t;
        o.val.val:=new_value(pcS.txtpos,t.len);
        INC(t.len);
      END;
      check_get(pcS.ident);
      IF    sy=pcS.comma THEN pcS.get(sy)
      ELSIF sy=pcS.rpar  THEN EXIT
      ELSIF sy=pcS.ident THEN pcS.expc(pcS.comma)
      ELSE EXIT END
    END;
    t.max.set_integer(t.len-1);
    (* super-type must be unsigned!, see pcF..fn_inc *)
    ASSERT(enumsize IN valid_size);
    IF    (enumsize = 4) OR (t.len > 10000H) THEN t.base:=pcO.longcard
    ELSIF (enumsize = 2) OR (t.len > 100H)   THEN t.base:=pcO.cardinal
    ELSE t.base:=pcO.shortcard
    END;
    check_get(pcS.rpar);
    t.end := pcS.txtpos;
  END enum;

  PROCEDURE range(VAR t: pc.STRUCT; base: pc.STRUCT);
    VAR emin,emax: pc.NODE; min,max: pc.VALUE; ps: pc.TPOS;
        old_errors1, old_errors2: INTEGER;

  BEGIN
    IF pcS.oberon AND ~pcS.ext() THEN pcS.err(143) END;
    IF base#NIL THEN pcB.chk_ordinal(base) END;
    new_type(t,pc.ty_range);
    pcS.get(sy);
    old_errors1 := env.errors.err_cnt;
    Expr(emin);
    ps:=pcS.txtpos;
    check_get(pcS.range);
    old_errors2 := env.errors.err_cnt;
    Expr(emax);
    IF (base=NIL) &
       ((emin.type.mode=pc.ty_SS) OR (emax.type.mode=pc.ty_SS))
    THEN base:=pcO.char;
    END;
    IF base#NIL THEN
      pcB.assign_compatibility(emin.pos,base,emin);
      pcB.assign_compatibility(emax.pos,base,emax);
    ELSE
      pcB.expression_compatibility(ps,emin,emax);
    END;
    pcB.chk_ordinal(emin.type);
    pcB.chk_ordinal(emax.type);
    const_value(min,emin, old_errors1);
    const_value(max,emax, old_errors2);
    IF pcB.cmp_value(pc.sb_gtr,min,max) THEN pcS.err(124) END;
    IF base=NIL THEN
      IF ~ (emin.type.mode IN pc.WHOLEs) THEN
        base:=emin.type;
        ASSERT(base.mode#pc.ty_range);
      ELSIF min.is_neg() THEN
        IF ~ pcS.ext() THEN
          base:=pcO.m2_int;
        ELSIF pcB.cmp_value(pc.sb_gtr,max,pc.longint_type.max) THEN
          base:=pcO.longlongint;
        ELSIF pcB.cmp_value(pc.sb_gtr,max,pc.integer_type.max) THEN
          base:=pcO.longint;
        ELSIF pcB.cmp_value(pc.sb_gtr,max,pc.shortint_type.max) THEN
          base:=pcO.integer;
        ELSE
          base:=pcO.shortint;
        END;
      ELSIF ~ pcS.ext() THEN
        base:=pcO.m2_card;
      ELSIF pcB.cmp_value(pc.sb_gtr,max,pc.longcard_type.max) THEN
        base:=pcO.longlongcard;
      ELSIF pcB.cmp_value(pc.sb_gtr,max,pc.cardinal_type.max) THEN
        base:=pcO.longcard;
      ELSIF pcB.cmp_value(pc.sb_gtr,max,pc.shortcard_type.max) THEN
        base:=pcO.cardinal;
      ELSE
        base:=pcO.shortcard;
      END;
      IF pcB.cmp_value(pc.sb_lss,min,base.min) THEN
        pcS.error(emin.pos,'e',122);
      ELSIF pcB.cmp_value(pc.sb_gtr,max,base.max) THEN
        pcS.error(emax.pos,'e',122);
      END;
    END;
    IF base.mode=pc.ty_range THEN base:=base.base END;
    t.base := base;
    t.min  := min;
    t.max  := max;
    t.end  := pcS.txtpos;
    check_get(pcS.rbr);
  END range;

  PROCEDURE pointer(VAR t: pc.STRUCT);
  BEGIN
    ASSERT(~ pbd);
    new_type(t,pc.ty_pointer);
    pcS.get(sy);
    check_sysflag3(t.flag);
    check_get(pcS.to);
    IF sy=pcS.ident THEN pbd:=TRUE END;
    type_definition(t.base,TRUE);
    t.end := pcS.txtpos;
    pbd:=FALSE;
    IF (t.base.mode=pc.ty_array_of) & (t.base.obj=NIL) THEN
      type_obj(t.base);
    END;
    IF t.flag=pc.flag_o2 THEN INCL(t.tags,pc.ttag_has_o2_ptr) END;
    IF (t.flag=pc.flag_o2) &
       (t.base.mode#pcO.ty_undef) &
       (t.base.mode#pcO.ty_invtype) &
       ~ (t.base.mode IN (pc.ARRs+pc.TY_SET{pc.ty_record}))
    THEN pcS.err(60)
    END;
  END pointer;

  PROCEDURE record(VAR rec: pc.STRUCT);

    PROCEDURE ^ fields(VAR x,tail: pc.OBJECT);

    PROCEDURE SimpleFields(VAR head,tail: pc.OBJECT);
    VAR
      t: pc.STRUCT;
      l,o: pc.OBJECT;
    BEGIN
      l:=NIL;
      LOOP
        IF sy=pcS.ident THEN
          o := pcO.new_obj(pc.ob_field);
          pcO.set_name(o,pcS.name);
          pcO.dcl(rec,o);
          pcO.alloc_field(o,rec,head,tail);
          IF l=NIL THEN l:=o END;
          pcS.get(sy);
          o.end := pcS.txtpos;
          check_sysflag(o.flag);
          check_export_mark(o,TRUE);
        ELSE
          check_get(pcS.ident);
        END;
        IF    sy=pcS.comma THEN pcS.get(sy)
        ELSIF sy=pcS.ident THEN pcS.expc(pcS.comma)
        ELSE EXIT
        END
      END;
      check_get(pcS.colon);
      t:=NIL;       (* -- error with option INITPTR off *)
      type_definition(t,FALSE);

      IF (t.obj # NIL) & (pcO.otag_forward IN t.obj.tags) THEN pcS.err(117,t.obj.name^) END;
      IF (t.flag=pc.flag_o2) & (rec.flag#pc.flag_o2) THEN pcS.err(120) END;
      IF pc.ttag_has_o2_ptr IN t.tags THEN
        INCL(rec.tags,pc.ttag_has_o2_ptr);
      END;
      WHILE l#NIL DO
        <* IF TARGET_IDB THEN *>
          l.type_use := TypeUse;
        <* END *>
        l.type     := t;
        l          := l.next;
      END;
    END SimpleFields;

    PROCEDURE VariantPart(VAR head,tail: pc.OBJECT);

      PROCEDURE AppendVariant(tag: pc.OBJECT; vrt: pc.NODE);
        VAR y: pc.NODE;
      BEGIN
        y:=tag.val.l;
        IF y=NIL THEN
          tag.val.l:=vrt;
        ELSE
          WHILE y.next#NIL DO y:=y.next END;
          y.next:=vrt;
        END;
      END AppendVariant;

      PROCEDURE check_labels(tag: pc.OBJECT; a,b: pc.VALUE);
      VAR
        c,l: pc.NODE;
      BEGIN
        IF env.errors.err_cnt>0 THEN RETURN END;
        ASSERT(tag.mode = pc.ob_header);
        c:=tag.val.l; (* case branches list *)
        WHILE c#NIL DO
          ASSERT(c.mode=pc.nd_node);
          l:=c.l; (* labels list *)
          CASE l.mode OF
          | pc.nd_value:
            WHILE l#NIL DO
              IF pcB.cmp_value(pc.sb_leq,l.val,b) &
                 pcB.cmp_value(pc.sb_geq,l.val,a)
              THEN
                pcS.err(126); RETURN
              END;
              l:=l.next;
            END;
          | pc.nd_pair:
            IF pcB.cmp_value(pc.sb_leq,l.l.val,b) &
               pcB.cmp_value(pc.sb_geq,l.val,a)
            THEN
              pcS.err(126); RETURN
            END;
          END;
          c:=c.next;
        END;
      END check_labels;

      PROCEDURE VariantLabel(VAR n: pc.NODE; tag: pc.OBJECT);
        VAR a,b: pc.VALUE; e: pc.NODE;
            old_errors: INTEGER;
      BEGIN
        IF tag.type.mode # pcO.ty_invtype THEN
          old_errors := env.errors.err_cnt;
        ELSE
          old_errors := 0;
        END;
        Expr(n);
        pcB.chk_index(tag.type,n,FALSE);
        const_value(a,n, old_errors);
        b := a;
        IF sy=pcS.range THEN
          old_errors := env.errors.err_cnt;
          pcS.get(sy); Expr(e);
          pcB.chk_index(tag.type,e,FALSE);
          const_value(b,e, old_errors);
          IF pcB.cmp_value(pc.sb_gtr,a,b) THEN pcS.err(124) END;
          n.l:=e; n.mode:=pc.nd_pair;
        END;
        check_labels(tag, a,b);
      END VariantLabel;

      PROCEDURE Variant(tag: pc.OBJECT);
        VAR x,nt,v: pc.NODE; tail: pc.OBJECT;
      BEGIN
        pcO.new(x,pc.nd_node);
        x.type:=tag.type; tail:=NIL;
        IF sy=pcS.else THEN
          pcS.get(sy); fields(x.obj,tail);
        ELSE
          LOOP
            VariantLabel(v,tag);
            pcO.app(x.l,nt,v);
            IF sy=pcS.colon THEN EXIT
            ELSIF sy=pcS.comma THEN pcS.get(sy);
            ELSE check_get(pcS.comma); EXIT
            END
          END;
          pcS.get(sy); fields(x.obj,tail);
          IF (sy#pcS.else) & (sy#pcS.end) THEN check_get(pcS.sep) END;
        END;
        AppendVariant(tag,x);
      END Variant;

      VAR tag,o: pc.OBJECT;
    BEGIN
      IF rec.flag=pc.flag_o2 THEN pcS.err(119) END;
      (* вариантные записи в Обероне требуют особой поддержки
         для нормальной работы сборки мусора *)
      pcS.get(sy);
      tag := pcO.new_obj(pc.ob_header);
      pcO.new(tag.val,pc.nd_case);
      IF sy=pcS.colon THEN
        (* unnamed tag *)
      ELSIF sy#pcS.ident THEN
        pcS.err(7);
        WHILE sy#pcS.end DO pcS.get(sy) END;
        RETURN
      ELSE
        o := pcO.new_obj(pc.ob_field);
        pcO.set_name(o,pcS.name);
        pcO.alloc_field(o,rec,tag.val.obj,tag.val.obj);
        pcO.dcl(rec,o); pcS.get(sy);
        check_sysflag(o.flag);
        check_export_mark(o,TRUE);
        o.end := pcS.txtpos;
      END;
      check_get(pcS.colon);
      type_qualident(o,FALSE);
      pcB.chk_ordinal(o.type);
      tag.type:=o.type; tag.val.type:=o.type;
      IF tag.val.obj#NIL THEN tag.val.obj.type:=o.type END;
      check_get(pcS.of);
      WHILE sy#pcS.end DO
        IF sy=pcS.sep THEN pcS.get(sy) ELSE Variant(tag) END;
      END;
      pcS.get(sy);
      IF (tag.val.obj#NIL) OR (tag.val.l#NIL) THEN
        pcO.alloc_field(tag,rec,head,tail);
      END;
    END VariantPart;

    PROCEDURE fields(VAR head,tail: pc.OBJECT);
    BEGIN
      LOOP
        IF    sy=pcS.ident THEN SimpleFields(head,tail)
        ELSIF sy=pcS.case  THEN VariantPart (head,tail)
        ELSIF (sy=pcS.end) OR (sy=pcS.sep) OR (sy=pcS.else) THEN EXIT
        ELSIF sy#pcS.semic THEN
          pcS.expc(pcS.end);
          WHILE sy#pcS.end DO pcS.get(sy) END;
          EXIT
        END;
        IF (sy#pcS.end) & (sy#pcS.sep) & (sy#pcS.else) THEN
          check_get(pcS.semic)
        END;
      END;
    END fields;

    PROCEDURE base;
      VAR v: pc.OBJECT;
    BEGIN
      IF rec.flag # pc.flag_o2 THEN pcS.err(118) END;
      pcS.get(sy); type_qualident(v,TRUE);
      IF v.type.mode#pc.ty_record THEN
        pcS.err(51)
      ELSE
        rec.base:=v.type;
        IF v.type.flag=pc.flag_o2 THEN
          rec.len:=v.type.len+1;
          IF rec.len > pc.code.max_ext_lev THEN
            pcS.err(220,pc.code.max_ext_lev)
          END;
        END;
      END;
      check_get(pcS.rpar);
    END base;

    VAR tail: pc.OBJECT;
  BEGIN
    new_type(rec,pc.ty_record);
    pcS.get(sy);
    check_sysflag2(rec.flag);
    IF (sy=pcS.lpar) & (pcS.oberon OR pcS.ext()) THEN base END;
    tail:=NIL;
    fields(rec.prof,tail);
    check_get(pcS.end);
    rec.end:=pcS.txtpos;
  END record;

  PROCEDURE o2_proctype(VAR proc: pc.STRUCT);
  BEGIN
    new_type(proc,pc.ty_proctype);
    pcS.get(sy);
    proc.base:=pcO.void;
    check_sysflag(proc.flag);
    IF proc.flag=pc.flag_o2 THEN proc.flag:=pc.flag_m2 END;
    prochead(proc,NIL);
    proc.end := pcS.txtpos;
  END o2_proctype;

  PROCEDURE m2_proctype(VAR proc: pc.STRUCT);
    VAR mode: pc.OB_MODE; p,o: pc.OBJECT; tags: pc.OTAG_SET; ptr: pc.STRUCT;
  BEGIN
    new_type(proc,pc.ty_proctype);
    pcS.get(sy);
    proc.base:=pcO.void;
    check_sysflag(proc.flag);
    IF proc.flag=pc.flag_o2 THEN proc.flag:=pc.flag_m2 END;
    IF sy#pcS.lpar THEN RETURN END;
    pcS.get(sy);
    pcO.enter_scope(proc);
    IF sy#pcS.rpar THEN
      LOOP
        tags:=pc.OTAG_SET{};
        IF sy=pcS.var THEN pcS.get(sy); special_varpar(mode,tags);
        ELSIF sy=pcS.seq THEN EXIT
        ELSE mode:=pc.ob_var; tags:=pc.OTAG_SET{pc.otag_valpar};
        END;
        IF (sy=pcS.ident) OR (sy=pcS.array) THEN
          p := pcO.new_obj(mode);
          pcO.alloc(p);
          p.tags:=p.tags + tags;
          formal_type(p.type,proc);
          IF pcO.otag_varpar_nil IN p.tags THEN
            ptr := pcO.new_type(pc.ty_pointer);
            ptr.base:=p.type; p.type:=ptr;
          END;
        ELSE pcS.err(7);
        END;
        IF (sy=pcS.rpar) OR (sy=pcS.seq) THEN EXIT END;
        check_get(pcS.comma);
      END;
      IF sy=pcS.seq THEN
        IF ~ pcS.ext() THEN pcS.err(102,"(SEQ parameter)") END;
        pcS.get(sy);
        p := pcO.new_obj(pc.ob_seq);
        pcO.alloc(p);
        p.type := pcO.new_type(pc.ty_array_of);
        type_qualident(o,TRUE);
        IF (o.type.mode=pc.ty_loc) OR
           (o.type.mode=pc.ty_array) & (o.type.base.mode=pc.ty_loc)
        THEN pcO.set_type_base(p.type,o.type);
        ELSE pcS.err(49); p.type.base:=pcO.invtype
        END;
        p.type.len:=1;
      END;
    END;
    pcO.exit_scope();
    proc.prof:=proc.mem; proc.mem:=NIL;
    proc.end:=pcS.txtpos;
    check_get(pcS.rpar);
    IF sy=pcS.colon THEN
      pcS.get(sy);
      type_qualident(o,FALSE);
      IF ~ (o.type.mode IN pc.code.FRETs) &
             (o.type.mode#pcO.ty_undef) (* !!!!! forward type *)
      THEN
        pcS.err(43); proc.base:=pcO.invtype;
      ELSE
        proc.base:=o.type;
      END;
    END;
  END m2_proctype;

  PROCEDURE array(VAR t: pc.STRUCT);
    VAR n: pc.NODE; v: pc.VALUE;
        old_errors: INTEGER;
        node_one: pc.NODE;
  BEGIN
    old_errors := env.errors.err_cnt;
    pcS.get(sy);
    IF sy=pcS.of THEN
      IF ~ pcS.oberon & ~ pcS.ext() THEN pcS.err(102,"(ARRAY OF type)") END;
      IF ~ en_array_of THEN pcS.err(46) END;
      new_type(t,pc.ty_array_of);
      pcS.get(sy);
      type_definition(t.base,TRUE);
      IF (t.base.obj # NIL) & (pcO.otag_forward IN t.base.obj.tags) THEN pcS.err(117,t.base.obj.name^) END;
      t.end := pcS.txtpos;
      pcO.set_type_base(t,t.base);
      IF t.base.mode=pc.ty_array_of THEN t.len:=t.base.len+1
      ELSE t.len:=1
      END;
      IF t.len>pc.code.max_dim THEN pcS.err(219,pc.code.max_dim) END;
    ELSE
      new_type(t,pc.ty_array);
      t.pos := ps;
      IF pcS.oberon THEN
        old_errors := env.errors.err_cnt;
        Expr(n);
        IF (n.mode = pc.nd_type) & pcS.ext() THEN
          t.inx := n.type;
          pcB.chk_ordinal(t.inx);
        ELSE
          pcB.assign_compatibility(n.pos,pcO.index,n);
          pcB.chk_ordinal(n.type);
          const_value(v,n, old_errors);
          t.inx := pcO.new_type(pc.ty_range);
          t.inx.base:=pcO.index;
          t.inx.min:=pcO.cardinal.min;
          t.inx.max:=new_value(n.pos,0);
          t.inx.max.binary(pc.sb_minus,v,int_one);
          IF v.expr # NIL THEN
            pcO.new(node_one, pc.nd_value);
            node_one.type := pcO.shortint;
            node_one.val  := int_one;
            pcB.gen_binary_operator(ps,v.expr,node_one,pc.sb_minus);
            v.expr.end := pcS.txtpos;
            t.inx.max.expr := v.expr;
          END;
        END;
      ELSE
        type_definition(t.inx,FALSE);
        pcB.chk_ordinal(t.inx);
      END;
      IF sy=pcS.comma THEN sy:=pcS.array ELSE check_get(pcS.of) END;
      type_definition(t.base,FALSE);
      IF (t.base.obj # NIL) & (pcO.otag_forward IN t.base.obj.tags) THEN pcS.err(117,t.base.obj.name^) END;
      t.end := pcS.txtpos;
      pcO.set_type_base(t,t.base);
      t.min:=t.inx.min;
      t.max:=t.inx.max;
      int_tmp.pos:=pcS.txtpos;
      int_tmp.binary(pc.sb_minus,t.max,t.min);
      int_tmp.binary(pc.sb_plus,int_tmp,int_one);
      IF env.errors.err_cnt > old_errors THEN
        t.mode := pcO.ty_invtype;
      ELSIF pcB.cmp_value(pc.sb_gtr,int_tmp,pc.code.max_index) THEN
        pcS.err(206);
        t.len:=pc.code.max_index.get_integer();
      ELSE
        t.len:=int_tmp.get_integer();
      END;
      t.end:=pcS.txtpos;
    END;
    IF pc.ttag_has_o2_ptr IN t.base.tags THEN
      INCL(t.tags,pc.ttag_has_o2_ptr);
    END;
  END array;

  PROCEDURE set(VAR t: pc.STRUCT; packed: BOOLEAN);
    CONST MAX_SET_LEN = 1024*1024; (* 32 * 128 * 256 = 128 Kb *)
  VAR
    ps: pc.TPOS;
  BEGIN
    ps := pcS.txtpos;
    pcS.get(sy);
    IF pcS.oberon & ~ packed THEN t:=pcO.bitset; RETURN END;
    new_type(t,pc.ty_set);
    t.pos := ps;
    check_get(pcS.of);
    type_definition(t.base,FALSE);
    t.end := pcS.txtpos;
    pcB.chk_ordinal(t.base);
    t.min:=t.base.min;
    t.max:=t.base.max;
    int_tmp.pos:=pcS.txtpos;
    int_tmp.binary(pc.sb_minus,t.max,t.min);
    int_tmp.binary(pc.sb_plus,int_tmp,int_one);
    IF pcB.cmp_value(pc.sb_gtr,int_tmp,pc.code.max_index) THEN
      pcS.err(133,MAX_SET_LEN); t.len:=MAX_SET_LEN;
    ELSE
      t.len:=int_tmp.get_integer();
      IF ~env.config.Option("override_max_set_len") & (t.len > MAX_SET_LEN) THEN
        pcS.err(133,MAX_SET_LEN); t.len:=MAX_SET_LEN;
      END;
    END;

    ASSERT(setsize IN valid_size);
    IF t.len > 32 THEN (* nothing *)
    ELSIF (setsize = 4) OR (t.len > 16) THEN t.inx:=pcO.longcard;
    ELSIF (setsize = 2) OR (t.len > 8)  THEN t.inx:=pcO.cardinal;
    ELSE t.inx:=pcO.shortcard
    END;
  END set;

  VAR o: pc.OBJECT;
BEGIN
  ps := pcS.txtpos;
  o := NIL;
  CASE sy OF
   |pcS.ident    : type_qualident(o,en_array_of);
                   IF sy=pcS.lbr THEN
                     range(t,o.type)
                   ELSE t:=o.type
                   END;
   |pcS.lbr      : range(t,NIL)
   |pcS.lpar     : enum(t); ASSERT(t.obj#NIL);
   |pcS.procedure: IF pcS.oberon THEN o2_proctype(t) ELSE m2_proctype(t) END;
   |pcS.record   : record(t); ASSERT(t.obj#NIL);
   |pcS.array    : array(t);
   |pcS.set      : set(t, FALSE);
   |pcS.packedset: set(t, TRUE);
   |pcS.pointer  : pointer(t)
  ELSE pcS.err(83); t:=pcO.invtype;
  END;
  <* IF TARGET_IDB THEN *>
    pcO.new(TypeUse, pc.nd_type);
    TypeUse.pos := ps;
    TypeUse.end := pcS.txtpos;
    TypeUse.obj := o;
  <* END *>
END type_definition;

(*----------------------------------------------------------------*)

TYPE
  CHECK_LABEL = RECORD (pc.RIDER)
                END;

PROCEDURE (VAR c: CHECK_LABEL) object(o: pc.OBJECT);
BEGIN
  IF (o.mode = pc.ob_label) THEN
    IF NOT (pc.ntag_lbl_used IN o.attr(pc.NODE).tags) THEN
      pcS.error(o.pos, 'e', 285, o.name^);
      c.del := TRUE;
    END;
  END;
END object;

VAR
  check_label: CHECK_LABEL;
PROCEDURE Block(enter: pc.NODE);

  PROCEDURE LabelDcl;
    VAR o: pc.OBJECT;
        n: pc.NODE;
  BEGIN
    pcS.get(sy);
    WHILE sy=pcS.ident DO
      o := pcO.new_obj(pc.ob_label);
      pcO.set_name(o,pcS.name);
      pcO.alloc(o);
      o.type:=pcO.undef;
      check_sysflag(o.flag);
      check_export_mark(o,FALSE);
      pcO.dcl(pcO.cur_scope,o);
      pcO.new(n, pc.nd_label);
      o.attr := n;
      o.attr(pc.NODE).obj  := o;
      o.attr(pc.NODE).type := o.type;
      pcS.get(sy);
      CASE sy OF
      | pcS.semic: pcS.get(sy); RETURN;
      | pcS.comma: pcS.get(sy);
      | pcS.ident: pcS.expc(pcS.semic)
      ELSE
        check_get(pcS.semic); RETURN
      END;
    END;
  END LabelDcl;

  PROCEDURE ConstDcl;
  VAR
    o: pc.OBJECT;
    n: pc.NODE;
    name: pc.NAME;
  BEGIN
    pcS.get(sy);
    WHILE sy=pcS.ident DO
      name := pcS.name;
      o := pcO.new_obj(pc.ob_cons);
      pcO.set_name(o,pcS.name);
      o.type:=pcO.undef;
      pcS.get(sy);
      check_sysflag(o.flag);
      check_export_mark(o,FALSE);
      CASE sy OF
      | pcS.equ:
        pcO.alloc(o);
        pcS.get(sy);
        Expr(n);
        o.end := pcS.txtpos;
        o.val:=n;
        o.type:=n.type;
        pcO.dcl(pcO.cur_scope,o);
        const_expr(o.val);
      | pcS.alias:
        pcS.get(sy);
        qualident(o, TRUE);
        pcO.dcl_rename(pcO.cur_scope,o,name);
      ELSE
        pcS.expc(pcS.equ); RETURN
      END;
      IF    sy=pcS.semic THEN pcS.get(sy)
      ELSIF sy=pcS.ident THEN pcS.expc(pcS.semic)
      ELSE check_get(pcS.semic); RETURN
      END;
    END;
  END ConstDcl;

  PROCEDURE VarDcl;
  VAR
    l,o: pc.OBJECT;
    t  : pc.STRUCT;
    ps : pc.TPOS;
    e  : pc.NODE;
    val: pc.VALUE;
    old_errors: INTEGER;
    varaddr:pc.NODE;
  BEGIN
    pcS.get(sy);
    WHILE sy=pcS.ident DO
      l:=NIL;
      LOOP
        IF sy=pcS.ident THEN
          o := pcO.new_obj(pc.ob_var);
          IF pcO.volatile THEN INCL(o.tags,pc.otag_volatile) END;
          pcO.set_name(o,pcS.name);
          pcO.dcl(pcO.cur_scope,o);
          o.type:=pcO.undef;
          o.next:=l; l:=o; (* do not call pcO.alloc(o) here ! *)
          o.host:=pcO.cur_scope;
          pcS.get(sy);
          o.end := pcS.txtpos;
          o.attr:=check_var_addr();
          check_sysflag3(o.flag);
          check_export_mark(o,TRUE);
        ELSE check_get(pcS.ident);
        END;
        IF    sy=pcS.comma THEN pcS.get(sy)
        ELSIF sy=pcS.ident THEN pcS.expc(pcS.comma)
        ELSE EXIT
        END
      END;
      IF pcS.ext() AND (sy=pcS.becomes) AND (o.lev=0) THEN
          pcS.get(sy);
          old_errors := env.errors.err_cnt;
          Expr(o.val);
          const_expr(o.val);
--          IF ~const(o.val) THEN   pcS.error(v.pos,'e',87); END;
      END;
      check_get(pcS.colon);
      ASSERT(tbd=NIL);
      ps := pcS.txtpos;
      type_definition(t,FALSE);
      IF o.val#NIL THEN
        pcB.assign_compatibility(o.val.pos,t,o.val);
      END;
      WHILE l#NIL DO
        o:=l; l:=o.next; o.next:=NIL;
        o.type:=t;
        <* IF target_idb THEN *>
          o.type_use := TypeUse;
        <* END *>
        pcO.alloc(o);
      END;
      check_get(pcS.semic);
    END;
  END VarDcl;

  PROCEDURE TypeDcl;
    VAR
      new: BOOLEAN;
      fdt: pc.STRUCT;
      nul: pc.STRUCT; (* to ignore type definition in case of error *)
      l,c: LONGINT;
      fnm: env.String;
      ps : env.TPOS;
  BEGIN
    pcS.get(sy);
    WHILE sy=pcS.ident DO
      ps:=pcS.txtpos;
      new:=~ pcO.try_scp(pcO.cur_scope,pcS.name,tbd);
      IF new THEN
        tbd := pcO.new_obj(pc.ob_type);
        pcO.alloc(tbd);
        pcO.set_name(tbd,pcS.name);
      ELSIF tbd.mode=pc.ob_module THEN
        pcS.err(27,tbd.name^); tbd:=pcO.inv_obj
      END;
      pcS.get(sy);
      check_sysflag3(tbd.flag);
      check_export_mark(tbd,FALSE);
      IF sy=pcS.equ THEN
        IF new THEN
          pcO.dcl(pcO.cur_scope,tbd);
          pcS.get(sy);
          type_definition(tbd.type,TRUE);
          tbd.type.end := pcS.txtpos;
          ASSERT(tbd.type.obj#NIL);
        ELSE
          fdt:=tbd.type;
          IF tbd.mode#pc.ob_type THEN
            IF tbd.mode#pc.ob_inv THEN
              tbd.mode:=pc.ob_inv;
              pcS.error(ps,'e',31);
            END;
            pcS.get(sy);
            type_definition(fdt,TRUE);
          ELSIF (tbd.type.mode=pc.ty_opaque) & (tbd.type.base=NIL) THEN
            ASSERT(tbd.type.obj#NIL);
            IF pcO.def THEN pcS.err(93) END;
            IF pcO.cur_scope#cu.type THEN pcS.err(95) END;
            IF tbd.type.mno#pc.cur_mod THEN
              tbd.pos.unpack(fnm,l,c);
              pcS.err(22,tbd.name^,fnm^,l+1,c+1);
              pcS.get(sy);
              type_definition(nul,FALSE);
            ELSE
              pcS.get(sy);
              type_definition(tbd.type.base,FALSE);
              IF tbd.type.base.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}
<* IF MCS THEN *>
                 +pc.TY_SET{pc.ty_longint,pc.ty_longcard}
<* END *>
              THEN
                IF tbd.type.base.flag=pc.flag_o2 THEN pcS.err(141) END;
              ELSE
                pcS.err(52);
                tbd.type:=pcO.invtype; (* not tbd.type.base:= -> ASSERT *)
            END;
            END;
          ELSIF tbd.type.mode=pcO.ty_undef THEN
            (* forward type *)
            ASSERT(pcO.otag_forward IN tbd.tags);
            ASSERT(tbd.type.obj=tbd);
            pcS.get(sy); tbd.pos:=ps;
            type_definition(tbd.type,~ (pcO.omark_dis_array_of IN tbd.marks));
            IF tbd.type.obj=NIL THEN tbd.type.obj:=tbd END;
            IF tbd.type.mode=pc.ty_opaque THEN
              pcS.err(109); fdt.mode:=pcO.ty_invtype;
            ELSE
              copy_type(fdt,tbd.type);
            END;
            EXCL(tbd.tags,pcO.otag_forward);
            IF pcS.oberon &
              (tbd.type.mode=pc.ty_pointer) &
              (tbd.type.base.mode#pcO.ty_invtype) &
              ~ (tbd.type.base.mode IN (pc.ARRs+pc.TY_SET{pc.ty_record}))
            THEN pcS.err(60)
            END;
          ELSE
            tbd.mode:=pc.ob_inv;
            tbd.pos.unpack(fnm,l,c);
            pcS.err(22,tbd.name^,fnm^,l+1,c+1);
            pcS.get(sy);
            type_definition(fdt,TRUE);
          END;
        END;
      ELSIF (sy=pcS.semic) & pcO.def & new THEN
        tbd.type := pcO.new_type(pc.ty_opaque);
        tbd.type.obj:=tbd; pcO.dcl(pcO.cur_scope,tbd);
      ELSE
        pcS.expc(pcS.equ); tbd.type:=pcO.invtype;
      END;
      check_get(pcS.semic);
      tbd:=NIL;
      pbd:=FALSE;
    END;
  END TypeDcl;

  PROCEDURE CodeProc(p: pc.OBJECT);
    VAR n,tail: pc.NODE; ex: pc.NODE;
  BEGIN
    pcO.new(n,pc.nd_aggregate);
    n.obj:=p; n.type:=p.type; p.val:=n;
    tail:=NIL;
    IF sy#pcS.semic THEN
      LOOP
        Expr(ex); pcB.eval_value(ex);
        IF ex.mode#pc.nd_value THEN pcS.err(87) END;
        pcO.app(n.l,tail,ex);
        IF sy=pcS.comma THEN pcS.get(sy) ELSE EXIT END;
      END;
    END;
  END CodeProc;

  PROCEDURE ProcBody(start_pos: pc.TPOS; p: pc.OBJECT; en: BOOLEAN; VAR x: pc.NODE);
    VAR l: pc.OBJECT;
  BEGIN
    IF p.mode=pc.ob_cproc THEN
      IF pcO.otag_forward IN p.tags THEN pcS.err(23) END;
      CodeProc(p); check_get(pcS.semic);
    ELSE
      check_get(pcS.semic);
      IF sy=pcS.forward THEN
        IF pcO.def THEN pcS.err(93) END;
        INCL(p.tags,pcO.otag_forward);
        pcS.get(sy); check_get(pcS.semic);
      ELSIF en THEN
        EXCL(p.tags,pcO.otag_forward);
        l:=p.type.prof;
        WHILE l#NIL DO pcO.dcl(pcO.cur_scope,l); l:=l.next END;
        pcO.new(x,pc.nd_proc);
        x.obj:=p; x.type:=p.type; p.val:=x;
        Block(x);
        ASSERT(x.r.mode=pc.nd_finally);
        ASSERT(x.r.l=NIL);
        x.r:=x.r.next;
        --x.end := pcS.txtpos;
        check_get(pcS.semic);
      END;
    END;
    p.pos := start_pos;
    p.end := pcS.prevpos;
  END ProcBody;

  PROCEDURE Method(VAR this: pc.OBJECT; VAR class: pc.STRUCT);
    VAR mode: pc.OB_MODE; v,o: pc.OBJECT;
  BEGIN
    class:=pcO.invtype; this:=NIL;
    IF ~ pcS.oberon OR (sy#pcS.lpar) THEN RETURN END;
    IF pcO.level#0 THEN pcS.err(95) END;
    pcS.get(sy);
    IF sy=pcS.var THEN mode:=pc.ob_varpar; pcS.get(sy)
    ELSE mode:=pc.ob_var
    END;
    IF sy=pcS.ident THEN
      o := pcO.new_obj(mode); pcO.set_name(o,pcS.name);
      IF mode=pc.ob_var THEN INCL(o.tags,pc.otag_valpar) END;
      pcS.get(sy); check_get(pcS.colon);
      type_qualident(v,FALSE); o.type:=v.type;
      IF v.type.mode#pcO.ty_invtype THEN
        IF mode=pc.ob_varpar THEN
          IF v.type.mode=pc.ty_record THEN class:=v.type;
          ELSE pcS.err(51);
          END;
        ELSIF v.type.mode#pc.ty_pointer THEN pcS.err(59);
        ELSIF v.type.flag#pc.flag_o2 THEN pcS.err(71)
        ELSIF v.type.base.mode#pc.ty_record THEN pcS.err(62);
        ELSE class:=v.type.base;
        END;
        IF class.mode#pcO.ty_invtype THEN
          IF    class.mno#pc.cur_mod THEN pcS.err(65)
          ELSIF class.flag#pc.flag_o2 THEN pcS.err(71)
          ELSE this:=o;
          END;
        END;
      END;
    ELSE
      pcS.err(7);
      WHILE (sy#pcS.end) & (sy#pcS.rpar) DO pcS.get(sy) END;
    END;
    check_get(pcS.rpar);
    IF class.obj#NIL THEN class:=class.obj.type END;
  END Method;

  PROCEDURE check_method_redef(s,p: pc.OBJECT);
    VAR f0,f1: pc.OBJECT;
  BEGIN
    f0:=s.type.prof;
    f1:=p.type.prof;
    IF f0.type.mode#f1.type.mode THEN pcS.error(f1.pos,'e',69); RETURN END;
    IF ~ pcB.cmp_params(f0.next,f1.next,FALSE) THEN
      pcS.err(155,p.name^); RETURN
    END;
    IF s.type.base#p.type.base THEN pcS.err(44); RETURN END;
    IF (pc.otag_public_f IN s.tags) &
       (p.host.obj#NIL) &
       (pcO.otag_exported IN p.host.obj.tags) &
       ~ (pc.otag_public_f IN p.tags)
    THEN pcS.err(74); INCL(p.tags,pc.otag_public_f);
    END;
  END check_method_redef;

  PROCEDURE ProcDcl(start_pos: pc.TPOS; VAR x: pc.NODE);
    CONST FWD = pc.OB_SET{pc.ob_xproc,pc.ob_proc,pc.ob_lproc};
    VAR p,this,spr: pc.OBJECT; type,class: pc.STRUCT; mode: pc.OB_MODE;
        pre,body: BOOLEAN; sf,nf: pc.Lang; sv_prot: pc.VALUE;
        attrs : pc.ProcAttrSet;
  VAR exceptnode: pc.OBJECT;
      wasDirect: BOOLEAN;
  BEGIN
    ASSERT(cu#NIL);
    ASSERT(sy=pcS.procedure);
    x:=NIL;
    body:=TRUE;
    mode:=pc.ob_proc;
    pcS.get(sy);
    attrs := check_sysflag_ex(sf, TRUE, exceptnode, wasDirect);
    nf:=sf;
    (*
        nf - object (name) flag
        sf - procedure type flag
    *)
    IF sf=pc.flag_o2  THEN sf:=pc.flag_m2 END;
    IF sf=pc.flag_sl1 THEN sf:=pc.flag_m2 END;
    IF (sy=pcS.bar) & pcS.oberon THEN pcS.get(sy); body:=FALSE END;
    Method(this,class);
    IF    this#NIL     THEN mode:=pc.ob_xproc;
    ELSIF sy=pcS.minus THEN mode:=pc.ob_cproc; pcS.get(sy);
    ELSIF sy=pcS.plus  THEN pcS.err(140); pcS.get(sy);
    ELSIF sy=pcS.slash THEN IF NOT (sf IN pc.ExtProcsAllowed) THEN pcS.err(164); END; mode:=pc.ob_eproc; body:=FALSE; pcS.get(sy);
    ELSIF sy=pcS.times THEN pcS.get(sy);
    END;
    IF pcO.def THEN
      IF mode=pc.ob_proc THEN mode:=pc.ob_xproc END;
      IF mode=pc.ob_cproc THEN pcS.err(92) END;
      body:=FALSE;
    END;
    IF sy#pcS.ident THEN pcS.err(7); RETURN END;
    p := NIL;
    IF this#NIL THEN
      pre:=pcO.try_scp(class,pcS.name,p) &
           (p.mode IN FWD) & (pcO.otag_forward IN p.tags);
      IF pre OR ~ pcO.try_qua(class,pcS.name,spr) THEN spr:=NIL;
      ELSIF ~ (spr.mode IN pc.PROCs) THEN pcS.err(114,pcS.name); spr:=NIL;
      END;
    ELSE
      spr:=NIL;
      pre:=(NOT pcO.def) & pcO.try_scp(pcO.cur_scope,pcS.name,p) &
           (p.mode IN FWD) & (pcO.otag_forward IN p.tags);
    END;
    (* spr - перекрытый метод, NIL если такого нет *)
    type := pcO.new_type(pc.ty_proctype);
--    type.flag:=sf;
    IF pre THEN
      IF ~wasDirect THEN
        type.flag := p.type.flag;
      ELSE
        type.flag:=sf;
      END;
      p.pos:=pcS.txtpos;
      ASSERT(p.type.obj=p);
    ELSE
      IF ~pcO.def & ~wasDirect THEN
        sf := pc.flag_m2;
      END;
      type.flag:=sf;
      p := pcO.new_obj(mode); pcO.set_name(p,pcS.name);
      p.type:=type; p.flag:=sf; type.obj:=p;
      IF this=NIL THEN
        pcO.dcl(pcO.cur_scope,p); pcO.alloc(p);
      ELSE
        pcO.dcl(class,p); pcO.alloc_method(p,class);
        IF spr#NIL THEN p.type.inx:=spr.type END;
      END;
      IF p.mode IN FWD THEN INCL(p.tags,pcO.otag_forward) END;
    END;

    IF pc.alwaysinline IN attrs THEN
      IF pc.neverinline IN attrs THEN
        pcS.err(err_incomp);
      ELSIF ~env.config.Option("dontalwaysinline") THEN
        INCL(p.type.tags,pc.ttag_alwaysinline);
      END;
    END;
    IF pc.neverinline IN attrs THEN
      INCL(p.type.tags,pc.ttag_neverinline);
    END;
    IF pc.strictcallconv IN attrs THEN
      INCL(p.type.tags,pc.ttag_strictcallconv);
    END;
    IF pc.except IN attrs THEN
      INCL(p.type.tags,pc.ttag_except);
    END;
    IF pc.throws IN attrs THEN
      INCL(type.tags,pc.ttag_throws);
    END;
    p.attr:=exceptnode;

    pcS.get(sy);
--    p.flag:=nf;
    check_export_mark(p,FALSE);
    IF (this#NIL) & (pc.otag_public_f IN p.tags) THEN
      IF this.type.obj=NIL THEN
        pcS.error(p.pos,'e',67,"  ");
      ELSIF ~ (pcO.otag_exported IN this.type.obj.tags) THEN
        pcS.error(p.pos,'e',67,this.type.obj.name^);
      END;
    END;
    IF (pcO.otag_exported IN p.tags) & (p.mode=pc.ob_proc) THEN
      IF pcS.oberon THEN
        (* экспортируемые Оберон-процедуры должны обладать значением *)
        p.mode:=pc.ob_xproc;
      ELSIF ~ (pc.otag_public IN p.tags) THEN
        (* процедура объявлена с внешним связыванием в Modula-2 *)
        p.mode:=pc.ob_lproc;
        EXCL(p.tags,pcO.otag_exported);
      END;
    END;
    check_protection(sv_prot,p.flag);
    prochead(type,this);
    IF pre & ~ pcB.proc_types_cmp(p.type,type,TRUE) THEN
      pcB.proc_incomp(p.pos,p.name^,p.type,type);
    END;
    IF spr#NIL THEN check_method_redef(spr,p) END;
    INC(proc_lev);
    IF proc_lev > MAX_PROC_LEV THEN pcS.err(221,MAX_PROC_LEV) END;
    pcO.enter_scope(p.type); ProcBody(start_pos, p,body,x); pcO.exit_scope;
    DEC(proc_lev);
    protection:=sv_prot;
  END ProcDcl;

  PROCEDURE ModuleDcl(VAR mods: pc.NODE);

    VAR host_scope: pc.STRUCT;

    PROCEDURE import;
    VAR
      o,mod: pc.OBJECT;
      from: BOOLEAN;
    BEGIN
      WHILE (sy=pcS.import) OR (sy=pcS.from) DO
        from:=(sy=pcS.from); mod:=NIL;
        IF from THEN
          pcS.get(sy);
          IF sy=pcS.ident THEN
             pcO.fnd_vis(host_scope,pcS.name,mod); pcS.get(sy);
          ELSE
            check_get(pcS.ident); mod:=pcO.inv_obj;
          END;
        END;
        check_get(pcS.import);
        LOOP
          IF sy=pcS.ident THEN
            IF from THEN
              pcO.fnd_qua(mod.type,pcS.name,o);
              pcO.dcl_ref(pcO.cur_scope,o);
            ELSE
              pcO.fnd_vis(host_scope,pcS.name,o);
              IF pcS.name # o.name^ THEN (* renamed module *)
                ASSERT(o.mode = pc.ob_module);
                pcO.dcl_rename(pcO.cur_scope,o,pcS.name);
              ELSE
                pcO.dcl_ref(pcO.cur_scope,o);
              END;
            END;
            pcS.get(sy);
          ELSE
            check_get(pcS.ident);
          END;
          IF    sy=pcS.comma THEN pcS.get(sy)
          ELSIF sy=pcS.semic THEN EXIT
          ELSIF sy=pcS.ident THEN pcS.expc(pcS.comma)
          ELSE EXIT
          END;
        END;
        check_get(pcS.semic);
      END;
    END import;

    PROCEDURE export(e-: ARRAY OF CHAR; qua: BOOLEAN);
      VAR o,l: pc.OBJECT; s: pc.STRUCT;
    BEGIN
      IF qua THEN s:=NIL ELSE s:=host_scope END;
      pcO.dcl_exp(s,e,o);
      IF (o#NIL) & (o.mode=pc.ob_type) & (o.type.mode=pc.ty_enum) THEN
        l:=o.type.prof;
        WHILE l#NIL DO pcO.dcl_exp(s,l.name^,o); l:=l.next END;
      END;
    END export;

    VAR
      mod,exps  : pc.OBJECT;
      e,o       : pc.OBJECT;
      x         : pc.NODE;
      sv_prot   : pc.VALUE;
      qua       : BOOLEAN;

  BEGIN
    pcS.get(sy);
    mod := pcO.new_obj(pc.ob_module);
    IF sy#pcS.ident THEN pcS.expc(pcS.ident);
    ELSE pcO.set_name(mod,pcS.name);
    END;
    pcS.get(sy);
    check_protection(sv_prot,mod.flag);
    mod.type := pcO.new_type(pc.ty_module);
    mod.type.obj:=mod;
    mod.type.base:=pcO.void;
    check_get(pcS.semic);
    pcO.dcl(pcO.cur_scope,mod);
    (* локальные модули не должны заноситься в список объектов *)
    host_scope:=pcO.cur_scope;
    pcO.enter_scope(mod.type);
    import;
    exps:=NIL; qua:=FALSE;
    IF sy=pcS.export THEN
      pcS.get(sy);
      qua:=(sy=pcS.qualified);
      IF qua THEN pcS.get(sy) END;
      LOOP
        IF sy=pcS.ident THEN
          o := pcO.new_obj(pc.ob_header);
          pcO.set_name(o,pcS.name);
          o.next:=exps; exps:=o;
          pcS.get(sy);
        ELSE
          check_get(pcS.ident);
        END;
        IF    sy=pcS.comma THEN pcS.get(sy)
        ELSIF sy=pcS.semic THEN EXIT
        ELSIF sy=pcS.ident THEN pcS.expc(pcS.comma)
        ELSE EXIT
        END
      END;
      check_get(pcS.semic);
    END;
    pcO.new(x,pc.nd_module);
    x.obj:=mod; x.type:=x.obj.type;
    x.next:=mods; mods:=x;
    Block(x);
    x.end := pcS.txtpos;

    e:=exps;
    WHILE e#NIL DO export(e.name^,qua); e:=e.next END;

    pcO.exit_scope;
    check_get(pcS.semic);
    protection:=sv_prot;
  END ModuleDcl;

  PROCEDURE app_nodes(VAR d: pc.NODE; s: pc.NODE);
    VAR l: pc.NODE;
  BEGIN
    IF s=NIL THEN RETURN END;
    IF d=NIL THEN d:=s; RETURN END;
    l:=d;
    WHILE l.next#NIL DO l:=l.next END;
    l.next:=s;
  END app_nodes;

  PROCEDURE BlockBody(VAR n,trap: pc.NODE);

    (* if trp#NIL then except part block *)
    PROCEDURE block(VAR bl: pc.NODE; trp: pc.NODE);
    VAR
      l, tmp: pc.NODE;

    BEGIN
      pcO.new(bl,pc.nd_block);
      tmp := bl;
      bl.type:=pcO.void;
      StatSeq(bl.r,NIL,bl,trp);
      tmp.end := pcS.txtpos;
      IF (pcO.cur_scope.base.mode#pc.ty_void) & (trp=NIL) THEN
        pcO.new(l,pc.nd_ftrap);
        l.type:=pcO.void;
        app_nodes(bl.r,l);
        IF bl.sub=pc.su_none THEN pcS.err(75) END;
      ELSIF trp#NIL THEN
        pcO.new(l,pc.nd_activate);
        l.type:=pcO.void; l.r:=trap;
        cur_pos(l.end);
        app_nodes(bl.r,l);
      END;
      IF bl.sub=pc.su_none THEN bl:=bl.r ELSE bl.sub:=pc.su_none END;
    END block;

    VAR l: pc.NODE;
  BEGIN
    n:=NIL; trap:=NIL;
    ASSERT((sy=pcS.begin) OR (sy=pcS.finally));
    IF sy=pcS.finally THEN
      IF pcO.cur_scope.mode#pc.ty_module THEN pcS.err(148) END;
    END;
    IF pcO.def THEN pcS.err(93) END;
    pcS.get(sy);
    block(n,NIL);
    IF sy=pcS.except THEN
      pcO.new(trap,pc.nd_except);
      trap.obj := pcO.new_obj(pc.ob_var);
      pcO.set_name(trap.obj,"&&trap");
      trap.obj.type:=pcO.process;
      trap.obj.pos:=env.null_pos;
      pcO.alloc(trap.obj);
      trap.type:=pcO.void;
      trap.l:=n; n:=trap;
      pcS.get(sy);
      block(n.r,trap);
      cur_pos(trap.end);
    END;
    IF protection#NIL THEN
      pcO.new(l,pc.nd_protect);
      pcO.new(l.r,pc.nd_value);
      l.obj := pcO.new_obj(pc.ob_var);
      l.obj.type:=pcO.protection;
      l.obj.pos:=env.null_pos;
      pcO.alloc(l.obj);
      l.l:=n; l.r.val:=protection;
      l.type:=pcO.void; l.r.type:=pcO.protection;
      n:=l;
    END;
  END BlockBody;

  PROCEDURE calc_method_num(t: pc.STRUCT);
    VAR m,b: pc.OBJECT;
  BEGIN
    ASSERT(t.mode=pc.ty_record);
    IF pcO.ttag_num_valid IN t.tags THEN RETURN END;
    ASSERT(t.mno=pc.cur_mod);
    IF t.base#NIL THEN
      calc_method_num(t.base);
      t.num:=t.base.num;
    END;
    m:=t.mem;
    WHILE m#NIL DO
      ASSERT(m.mno=pc.cur_mod);
      IF (t.base#NIL) &
         pcO.try_qua(t.base,m.name^,b) &
         (m.type.inx#b.type)
      THEN
        m.type.inx:=b.type; pcS.error(b.pos,'e',72);
      END;
      IF m.type.inx#NIL THEN m.type.len:=m.type.inx.len;
      ELSE m.type.len:=t.num; INC(t.num);
      END;
      m:=m.next;
    END;
    INCL(t.tags,pcO.ttag_num_valid);
  END calc_method_num;

  PROCEDURE check_objects(o: pc.OBJECT);
  BEGIN
    WHILE o#NIL DO
      IF pcO.otag_forward IN o.tags THEN
        IF (o.mode IN pc.PROCs) & ~ pcO.def THEN
          pcS.error(o.pos,'e',89,o.name^)
        ELSIF (o.mode=pc.ob_type) & (o.type.mode#pc.ty_opaque) THEN
          pcS.error(o.pos,'e',97,o.name^)
        END;
      END;
      IF (o.mode=pc.ob_type) & (o.type.obj=o) THEN
        IF o.type.mode=pc.ty_enum THEN
          check_objects(o.type.prof);
        ELSIF o.type.mode=pc.ty_record THEN
          check_objects(o.type.mem);
          calc_method_num(o.type);
        END;
      END;
      ASSERT(o.next#o);
      o:=o.next;
    END;
  END check_objects;

  PROCEDURE app_reraise(VAR d: pc.NODE; t: pc.NODE);
    VAR x: pc.NODE;
  BEGIN
    IF t=NIL THEN RETURN END;
    pcO.new(x,pc.nd_reraise);
    x.type:=pcO.void;
    x.r:=t;
    app_nodes(d,x);
  END app_reraise;

  PROCEDURE insert_module (enter, mod: pc.NODE);
    (* mod - локальный модуль, enter - объемлющая конструкция *)
    VAR n: pc.NODE; o: pc.OBJECT;
  BEGIN
    (* объекты *)
    ASSERT(mod.type.prof = NIL);
    o := mod.type.mem;
    IF o # NIL THEN
      o.host  := enter.type;
    <* IF COMPONENT_TESTCOVERAGE THEN *>
      IF o.owner # NIL THEN
        mod.obj.next := o.owner(pc.OBJECT);
      END;	
      o.owner := mod.obj;
    <* END *>
      WHILE o.next # NIL DO
        o := o.next;
	o.host  := enter.type;
      <* IF COMPONENT_TESTCOVERAGE THEN *>
        o.owner := mod.obj;
      <* END *>
      END;
      o.next := enter.type.mem;
      enter.type.mem := mod.type.mem;
    END;
    (* операторы *)
    ASSERT(mod.r.mode = pc.nd_finally);
    ASSERT(enter.r.mode = pc.nd_finally);
  <* IF COMPONENT_TESTCOVERAGE THEN *>
    IF mod.r # NIL THEN
      IF mod.r.owner # NIL THEN
        mod.next := mod.r.owner(pc.NODE);
      END;	
      n       := mod.r;
      n.owner := mod;
      WHILE n.next # NIL DO
        n       := n.next;
        n.owner := mod;
      END;
      -- process FINALLY part of local module
      n := mod.r.l;
      WHILE n # NIL DO
        n.owner := mod;
        n       := n.next;
      END;
    END;
  <* END *>
    IF pcO.level >= 0 THEN
      -- BEGIN part of local module
      IF (enter.r.next # NIL) & (enter.r.next.mode = pc.nd_except) THEN
        app_nodes(mod.r.next, enter.r.next.l);
        enter.r.next.l := mod.r.next;
      ELSE
        app_nodes(mod.r.next, enter.r.next);
        enter.r.next := mod.r.next;
      END;
      -- FINALLY part of local module
      app_nodes(enter.r.l, mod.r.l);
    ELSE
      n := NIL;
      app_nodes(n, enter.r);
      enter.r := n;
    END;
    (* процедуры *)
    n := mod.l;
    IF n # NIL THEN
      WHILE n.next # NIL DO n := n.next END;
      n.next  := enter.l;
      enter.l := mod.l;
    END;
  END insert_module;

  VAR
    proc,x,mods,trap1,trap2: pc.NODE;
    proc_cnt: INTEGER;
BEGIN
  ASSERT(enter#NIL);
  proc:=NIL; mods:=NIL; proc_cnt:=0;
  LOOP
    IF pcS.oberon & (proc_cnt>0) & ~ pcS.ext() &
       ((sy=pcS.const) OR (sy=pcS.var) OR (sy=pcS.type))
    THEN pcS.err(100); proc_cnt:=MIN(INTEGER);
    END;
    CASE sy OF
     |pcS.label    : LabelDcl();
     |pcS.const    : ConstDcl();
     |pcS.var      : VarDcl();
     |pcS.type     : TypeDcl();
     |pcS.module   : IF pcO.def THEN pcS.err(93) END;
                     IF pcS.oberon THEN pcS.err(143) END;
                     ModuleDcl(mods);
     |pcS.procedure: INC(proc_cnt);
                     ProcDcl(pcS.txtpos, x);
                     IF x#NIL THEN
                       IF proc=NIL THEN enter.l:=x ELSE proc.next:=x END;
                       proc:=x;
                     END;
     |pcS.begin,pcS.end: EXIT
    ELSE
      pcS.err(82);
      REPEAT pcS.get(sy)
      UNTIL (sy = pcS.const) OR (sy = pcS.var) OR (sy = pcS.type)
         OR (sy = pcS.module) OR (sy = pcS.procedure)
         OR (sy = pcS.begin) OR (sy = pcS.end);
    END;
  END;
  check_objects(enter.type.mem);
  pcO.new(enter.r,pc.nd_finally);
  enter.r.r:=enter;  (* это нужно для правильного порождения имени
                                процедуры финализации *)
  enter.r.type:=pcO.void;
  enter.pos:=pcS.txtpos;
  IF sy=pcS.begin THEN
    IF (enter.mode = pc.nd_module) AND (cu.type.flag=pc.flag_c) THEN
      pcS.err(290);
    END;
    BlockBody(enter.r.next,trap1)
  ELSE
    IF pcO.cur_scope.base.mode#pc.ty_void THEN pcS.err(75) END;
    trap1:=NIL
  END;
  enter.r.pos := pcS.txtpos;
  IF sy=pcS.finally THEN BlockBody(enter.r.l,trap2) ELSE trap2:=NIL END;
  WHILE mods#NIL DO insert_module(enter,mods); mods:=mods.next END;
  pcO.correct_cur_tail;
  IF pcO.cur_scope.mode=pc.ty_proctype THEN
    app_nodes(enter.r.next,enter.r.l);
    enter.r.l:=NIL;
  END;
  app_reraise(enter.r.next,trap1);
  app_reraise(enter.r.l,trap2);
  check_get(pcS.end);
  enter.end := pcS.txtpos;
  enter.r.end := pcS.txtpos;
  IF sy#pcS.ident THEN
    pcS.err(7);
  ELSE
    IF pcS.name#pcO.cur_scope.obj.name^ THEN
      pcS.err(88,pcO.cur_scope.obj.name^);
    END;
    pcO.cur_scope.obj.end:=pcS.txtpos;
    pcS.get(sy);
  END;
  pcO.cur_scope.objects(check_label);
END Block;

(*----------------------------------------------------------------*)

PROCEDURE import;

  PROCEDURE incl_import(m: pc.OBJECT);
    VAR u: pc.USAGE;
  BEGIN
--  ASSERT(m.mode=pc.ob_module);  nomore necessary, as objects are also included
    IF m.mno<pc.ZEROMno THEN RETURN END;
    u:=cu.type.use;
    WHILE u#NIL DO
      IF u.obj=m THEN RETURN END;
      u:=u.next;
    END;
    NEW(u);
    u.obj:=m;
    u.tags:=pc.UTAG_SET{};
    u.next:=cu.type.use;
    cu.type.use:=u;
  END incl_import;

  VAR
    mod,o: pc.OBJECT;
    from, exit: BOOLEAN;
    nm: pc.NAME;
    start_pos, end_pos: pc.TPOS;
BEGIN
  pc.import.pos := pcS.txtpos;
  WHILE (sy=pcS.import) OR (sy=pcS.from) DO
    from:=(sy=pcS.from); mod:=NIL;
    IF from THEN
      pcS.get(sy);
      start_pos := pcS.txtpos;
      IF sy=pcS.ident THEN
        IF pcS.name=cu.name^ THEN pcS.err(24) END;
        pcO.inp_sym_file(pcS.name,FALSE,FALSE,mod);
        incl_import(mod);
        pcS.get(sy);
        pc.import.add(mod, start_pos, pcS.txtpos);
      ELSE
        check_get(pcS.ident); mod:=pcO.inv_obj;
      END;
    END;
    check_get(pcS.import);
    LOOP
      start_pos := pcS.txtpos;
      o := NIL;
      IF (sy=pcS.ident) <* IF MCS THEN *>
                         OR (sy = pcS.inline) OR (sy = pcS.setreg)
                        <* END *>
      THEN
        IF from THEN
          ASSERT(mod#NIL);
          ASSERT(mod.type#NIL);
          pcO.fnd_scp(mod.type,pcS.name,o);
          pcO.dcl_ref(pcO.cur_scope,o);
          incl_import(o);  -- for debugger
          pcS.get(sy);
        ELSE
          nm:=pcS.name;
          pcS.get(sy);
          mod:=NIL;
          IF (sy=pcS.becomes) THEN
            IF NOT (pcS.ext() OR pcS.oberon) THEN
              pcS.err(102,"(renaming in import)")
            END;
            pcS.get(sy);
            IF sy=pcS.ident THEN
              IF pcS.name=cu.name^ THEN pcS.err(24) END;
              pcO.inp_sym_file(pcS.name,FALSE,FALSE,mod);
              incl_import(mod);
              o := mod;
              pcO.dcl_rename(pcO.cur_scope,mod,nm);
            END;
            check_get(pcS.ident);
          ELSE
            IF pcS.name=cu.name^ THEN pcS.err(24) END;
            pcO.inp_sym_file(nm,FALSE,FALSE,mod);
            incl_import(mod);
            o := mod;
            pcO.dcl_ref(pcO.cur_scope,mod);
          END;
        END;
      ELSE check_get(pcS.ident);
      END;
(*<An1*)
      cur_pos(o.end);
      EXCL(o.marks, pcO.omark_tried);
      INCL(o.marks, omark_imported);
(*An1>*)
      exit := FALSE;
      end_pos := pcS.txtpos;
      IF ~( cu.type.flag IN pc.LangSet{pc.flag_o2,pc.flag_m2}) AND
          (mod.type.flag IN pc.LangSet{pc.flag_o2,pc.flag_m2}) AND
          (mod.type.mno > 0) THEN
        pcS.warn(341);
      END;
      IF    sy=pcS.comma THEN pcS.get(sy)
      ELSIF sy=pcS.semic THEN exit := TRUE;
      ELSIF sy=pcS.ident THEN pcS.expc(pcS.comma)
      ELSE exit := TRUE;
      END;
      IF o # NIL THEN pc.import.add(o, start_pos, end_pos); END;
      IF exit THEN
        EXIT;
      END;
    END;
    check_get(pcS.semic);
    pc.import.end := pcS.prevpos;
  END;
END import;

PROCEDURE check_publics(o: pc.OBJECT);
BEGIN
  WHILE o#NIL DO
    ASSERT(pc.otag_public IN o.tags);
    IF (o.mode IN pc.OB_SET{pc.ob_proc,pc.ob_xproc,pc.ob_lproc,pc.ob_cproc}) &
       (o.val=NIL)
    THEN
      pcS.error(o.pos,'e',89,o.name^);
    ELSIF (o.mode=pc.ob_type) &
          (o.type.mode=pc.ty_opaque) &
          (o.type.base=NIL) &
          (o.type.obj=o)
    THEN
      pcS.error(o.pos,'e',96,o.name^);
    END;
    o:=o.next;
  END;
END check_publics;

PROCEDURE move_publics;
  PROCEDURE mark_type(t: pc.STRUCT);
    PROCEDURE mark_obj(o: pc.OBJECT);
    BEGIN
      IF o.host=cu.type THEN INCL(o.tags,pc.otag_public) END;
      mark_type(o.type);
    END mark_obj;
    VAR o: pc.OBJECT;
  BEGIN
    IF t=NIL THEN RETURN END;
    IF pcO.ttag_public IN t.tags THEN RETURN END;
    IF t.mno#pc.cur_mod THEN RETURN END;
    INCL(t.tags,pcO.ttag_public);
    IF t.obj#NIL THEN mark_obj(t.obj) END;
    mark_type(t.inx);
    mark_type(t.base);
    o:=t.prof;
    WHILE o#NIL DO mark_obj(o); o:=o.next END;
  END mark_type;
  VAR fp,tp,o: pc.OBJECT; m: pc.STRUCT;
BEGIN
  m:=cu.type;
  ASSERT(m.prof=NIL);
  o:=m.mem;
  WHILE o#NIL DO
    IF pc.OTAG_SET{pcO.otag_exported,pc.otag_public}*o.tags#pc.OTAG_SET{} THEN
      INCL(o.tags,pc.otag_public);
      mark_type(o.type);
    END;
    o:=o.next;
  END;
  o:=m.mem; fp:=NIL; tp:=NIL;
  WHILE o#NIL DO
    IF pc.otag_public IN o.tags THEN
      IF fp=NIL THEN m.mem:=o.next ELSE fp.next:=o.next END;
      IF tp=NIL THEN m.prof:=o ELSE tp.next:=o END;
      tp:=o; o:=o.next; tp.next:=NIL;
    ELSE
      fp:=o; o:=o.next;
    END;
  END;
END move_publics;

PROCEDURE make_finally_procs(VAR n,h: pc.NODE);
  VAR l,t,x: pc.NODE; p: pc.OBJECT; nm: pc.NAME;
BEGIN
  l:=n; t:=NIL;
  REPEAT
    ASSERT(l.mode=pc.nd_finally);
    IF l.l=NIL THEN
      l:=l.next;
      IF t=NIL THEN n:=l ELSE t.next:=l END;
    ELSE
      p := pcO.new_obj(pc.ob_xproc);
      p.type := pcO.new_type(pc.ty_proctype);
     <* IF TARGET_C THEN *>
      xcStr.prn_txt(nm,"%s_FINALLY",l.r.obj.name^);
     <* ELSE *>
      xcStr.prn_txt(nm,"FINALLY");
     <* END *>
      l.r:=NIL;
      pcO.set_name(p,nm);
      p.pos := l.pos;
      p.end := l.end;
      p.type.pos:=l.pos;
      p.type.obj:=p;
      p.type.base:=pcO.void;
      pcO.alloc(p);
      pcO.new(x,pc.nd_proc);
      x.pos := l.pos;
      x.end := l.end;
      x.r:=l.l; l.l:=NIL;
      x.next:=h; h:=x;
      p.val:=x; x.obj:=p; x.type:=p.type;
      l.obj:=p; t:=l; l:=l.next;
    END;
  UNTIL (l=NIL) OR (l.mode#pc.nd_finally);
END make_finally_procs;

PROCEDURE parser;
  VAR flag: pc.Lang; o: pc.OBJECT; sv_prot: pc.VALUE; fnm: env.String;

(* <An1 *)
  PROCEDURE find_redundant_import;
  VAR mod:pc.USAGE;
      name:pc.STRING;
      obj:pc.OBJECT;
  BEGIN
    mod := cu.type.use;
    WHILE mod # NIL DO
      obj := mod.obj;
      IF mod.obj.type.mode=pc.ty_enum THEN
        obj := obj.type.obj;
      END;
      IF pc.OMARK_SET{omark_imported,pcO.omark_tried} * obj.marks = pc.OMARK_SET{omark_imported} THEN
        DStrings.Assign("",name);
        IF obj.host#NIL THEN
          DStrings.Append(obj.host.obj.name^,name);
          DStrings.Append(".",name);
        END;
        DStrings.Append(obj.name^,name);
        env.errors.Warning(obj.end,306,name^);
      END;
      mod := mod.next;
    END;
  END find_redundant_import;
(* An1> *)

BEGIN
  IF ~ pcS.oberon THEN
    IF sy=pcS.definition        THEN pcS.get(sy); pcO.def:=TRUE;
    ELSIF sy=pcS.implementation THEN pcS.get(sy); pcO.imp:=TRUE;
    END;
  END;
  IF sy#pcS.module THEN pcS.expc(pcS.module); RETURN END;
  pcS.get(sy);
  IF pcO.def THEN check_sysflag(flag);
  ELSIF pcS.oberon THEN flag:=pc.flag_o2;
  ELSE flag:=pc.flag_m2;
  END;
  IF sy#pcS.ident THEN pcS.expc(pcS.ident); RETURN END;

  NEW(env.info.module,LENGTH(pcS.name)+1);
  COPY(pcS.name,env.info.module^);
  env.config.SetEquation("MODULE",pcS.name);
  IF env.info.file # NIL THEN
    env.config.SetEquation("FILE",env.info.file^);
  END;

  env.config.Equation("RTSSYMFILE",fnm);
  IF (fnm#NIL) & (fnm[0]#0X) THEN pcO.inp_sym_file(fnm^,FALSE,FALSE,o) END;

  IF pcO.imp THEN
    pcO.inp_sym_file(env.info.module^,TRUE,FALSE,cu);
    pc.cur_mod:=cu.mno;
  ELSE
    IF pc.mod_cnt>=LEN(pc.mods^) THEN pcO.resize_mods END;
    pc.cur_mod:=pc.mod_cnt;
    pc.RefreshPrimitiveTypes();
    INC(pc.mod_cnt);
    cu := pcO.new_obj(pc.ob_module);
    pcO.set_name(cu,env.info.module^);
    cu.type := pcO.new_type(pc.ty_module);
    cu.flag := flag;
    cu.type.flag := flag;
    cu.type.obj:=cu;
    cu.type.len:=xfs.sys.VersionTag();
    cu.type.base:=pcO.void;
    pc.mods[pc.cur_mod]:=cu;
  END;
  env.info.mod_no:=pc.cur_mod;

  pcO.enter_scope(cu.type);
    IF pcS.TS_ext() & pcO.imp THEN
      pcO.dcl_ref(pcO.cur_scope, cu);
    END;
    pcS.get(sy);
    check_protection(sv_prot,cu.flag);
    check_get(pcS.semic);
    pcO.new(cu.val,pc.nd_module);
    cu.val.obj:=cu;
    cu.val.type:=cu.type;
    ASSERT(cu.val#NIL);
    import;
    ASSERT(cu.val#NIL);
    Block(cu.val);
    make_finally_procs(cu.val.r,cu.val.l);
    protection:=sv_prot;
    cu.val.end := pcS.txtpos;
  pcO.exit_scope;
  IF sy#pcS.period THEN pcS.expc(pcS.period) END;

(* <An1 *)
  find_redundant_import;
(* An1> *)

  IF pcO.def THEN
    ASSERT(cu.type.prof=NIL);
    cu.type.prof:=cu.type.mem;
    cu.type.mem:=NIL;
    o:=cu.type.prof;
    WHILE o#NIL DO INCL(o.tags,pc.otag_public); o:=o.next END;
  ELSE
    IF pcS.oberon THEN move_publics END;
    check_publics(cu.type.prof);
  END;
  IF env.errors.err_cnt=0 THEN pcS.check_cc_level END;
END parser;

PROCEDURE optimazer;
BEGIN
  env.info.compiler_phase(2);
  <* IF db_trace THEN *>
   IF env.config.Option ("FE_IRVIS") THEN
      dbg.Ini;
      dbg.VisIR (cu.val, "\n======= IR before M-end optimizations =======", cu.mno);
    END;
  <* END *>
  IF ~ pcO.def THEN pc.const.eval_module(cu.val) END;
  <* IF db_trace THEN *>
     IF env.config.Option ("ME_IRVIS") THEN
     dbg.VisIR (cu.val, "\n======= IR after M-end optimizations =======", cu.mno);
   END;
  <* END *>
END optimazer;

PROCEDURE compiler;
VAR list:pc.OBJECT;
    haveExceptProcs:BOOLEAN;
BEGIN
  parser;
  env.info.main:=~ (pcO.imp OR pcO.def OR pcS.oberon) OR env.config.Option("MAIN");
  IF env.errors.err_cnt#0 THEN RETURN END;

<* IF TARGET_IDB THEN *>
  IF env.InterViewMode THEN
    model2.process_module(cu.val);
  END;
<* END *>
<* IF COMPONENT_TESTCOVERAGE THEN *>
  tcMain.ProcessModule (env.info.file^, cu);
<* END *>

  haveExceptProcs:=FALSE;
  list := pc.mods[pc.cur_mod].type.prof;
  WHILE list#NIL DO
    haveExceptProcs:=haveExceptProcs OR (pc.ttag_except IN list.type.tags );
    list:=list.next;
  END;
  list := pc.mods[pc.cur_mod].type.mem;
  WHILE list#NIL DO
    haveExceptProcs:=haveExceptProcs OR (pc.ttag_except IN list.type.tags );
    list:=list.next;
  END;
  IF haveExceptProcs>(pc.otag_haveExceptTable IN pc.mods[pc.cur_mod].tags) THEN
    env.errors.Error(env.null_pos,450,"ExceptTable inconsistency");
  END;

  optimazer;
  IF (env.errors.err_cnt#0) THEN RETURN END;
  IF pcO.def OR pcS.oberon THEN
    env.info.compiler_phase(3);
    IF pcO.def THEN
      pc.code.allocate(pc.cur_mod,env.info.main,src_time);
    ELSE
      pc.code.allocate(pc.cur_mod,env.info.main,MIN(xfs.Time));
    END;
    IF env.errors.err_cnt#0 THEN RETURN END;
    env.info.compiler_phase(4);
    pcO.out_sym_file;
    IF env.errors.err_cnt#0 THEN RETURN END;
    IF pcS.oberon & env.config.Option("MAKEDEF") THEN
      brs.browse_cu(cu)
    END;
  END;
  env.info.compiler_phase(5);
  IF ~ pcO.def &
     (env.errors.err_cnt = 0) &
     env.info.en_b_end
  THEN
    (* Ned: check errors for DEMO version.
       compiler_phase can generate an error.
    *)
    pc.code.gen_code(pc.cur_mod,env.info.main);
  END;
END compiler;

PROCEDURE compile_X2(lang: pc.Lang);
BEGIN
  pcO.ini(lang,cur_pos,pcS.o2_num_ext,pcS.ext());
  pcB.ini;
  int_tmp:=new_value(env.null_pos,0);
  int_one:=new_value(env.null_pos,1);
  compiler;
  ASSERT(tbd=NIL);
END compile_X2;

PROCEDURE get_import(cu: pc.UNIT);

  PROCEDURE Append(name-: pc.NAME);
  BEGIN
    IF (name#"SYSTEM") & (name#pcO.COMPILER) THEN cu.Import(name) END;
  END Append;

  PROCEDURE check_id;
  BEGIN
    IF sy#pcS.ident THEN pcS.expc(pcS.ident) END;
    pcS.get(sy)
  END check_id;

  PROCEDURE import;
    VAR name: pc.NAME; from: BOOLEAN;
  BEGIN
    WHILE (sy=pcS.import) OR (sy=pcS.from) DO
      from:=(sy=pcS.from);
      IF from THEN
        pcS.get(sy);
        IF sy=pcS.ident THEN Append(pcS.name); pcS.get(sy);
        ELSE check_id;
        END;
      END;
      check_get(pcS.import);
      LOOP
        IF (sy=pcS.ident) <* IF MCS THEN *>
                            OR (sy = pcS.inline) OR (sy = pcS.setreg)
                          <* END *>
        THEN
          IF from THEN
            pcS.get(sy);
          ELSE
            name:=pcS.name;
            pcS.get(sy);
            IF (sy=pcS.becomes) THEN
              pcS.get(sy);
              IF sy=pcS.ident THEN Append(pcS.name) END;
              check_id;
            ELSE
              Append(name);
            END;
          END;
        ELSE
          check_id;
        END;
        IF    sy=pcS.comma THEN pcS.get(sy)
        ELSIF sy=pcS.semic THEN EXIT
        ELSIF sy=pcS.ident THEN pcS.expc(pcS.comma)
        ELSE EXIT
        END
      END;
      check_get(pcS.semic);
    END;
  END import;

  VAR k: pcS.Symbol; nm: pc.NAME;

BEGIN
  k:=sy;
  IF    sy=pcS.definition     THEN pcS.get(sy);
  ELSIF sy=pcS.implementation THEN pcS.get(sy);
  END;
  IF sy#pcS.module THEN pcS.expc(pcS.module); RETURN END;
  pcS.get(sy);
  IF sy=pcS.lbr THEN
    WHILE (sy#pcS.rbr) & (sy#pcS.semic) & (sy#pcS.end) DO pcS.get(sy) END;
    check_get(pcS.rbr);
  END;
  IF sy=pcS.ident THEN
    nm:=pcS.name; pcS.get(sy);
    IF    k=pcS.definition     THEN cu.Definition(nm);
    ELSIF k=pcS.implementation THEN cu.Implementation(nm); Append(nm);
    ELSE  cu.Module(nm);
    END;
    IF sy=pcS.lbr THEN
      WHILE (sy#pcS.rbr) & (sy#pcS.semic) & (sy#pcS.end) DO pcS.get(sy) END;
      check_get(pcS.rbr);
    END;
    check_get(pcS.semic);
    import;
  ELSE
    pcS.expc(pcS.ident);
  END;
END get_import;

TYPE
  PARS = POINTER TO RECORD (pc.pars_rec)
  END;

PROCEDURE (p: PARS) ini(src: xfs.File; tm: xfs.Time);
BEGIN
  src_time:=tm;
  env.info.compiler_phase(1);
  cu:=NIL;
  tbd:=NIL;
  pbd:=FALSE;
  with_cnt:=0;
  proc_lev:=0;
  protection:=NIL;
  pcS.ini(src(xfs.TextFile),p.lang=pc.flag_o2,sy);
END ini;

PROCEDURE (p: PARS) compile;
BEGIN
  compile_X2(p.lang);
END compile;

PROCEDURE (p: PARS) browser(nm: pc.STRING);
BEGIN
  pcO.ini(p.lang,cur_pos,TRUE,TRUE);
  pcB.ini;
  env.info.module:=nm;
  pcO.inp_sym_file(nm^,FALSE,TRUE,cu);
  brs.browse_cu(cu);
END browser;

PROCEDURE (p: PARS) chk_import(u: pc.UNIT);
BEGIN
  pcO.ini(p.lang,cur_pos,TRUE,TRUE);
  pcB.ini;
  INCL(env.config.tags,env.m2_ext);
  get_import(u);
END chk_import;

PROCEDURE (p: PARS) exi;
  VAR i: LONGINT;
BEGIN
  protection:=NIL;
  FOR i:=0 TO LEN(with)-1 DO with[i]:=NIL; with_type[i]:=NIL END;
  cu:=NIL;
  tbd:=NIL;
  int_tmp:=NIL;
  int_one:=NIL;
  pcB.exi;
  pcO.exi;
  pcS.exi;
END exi;

(*----------------------------------------------------*)

PROCEDURE SetAlign(VAR val: env.String);
  CONST valid = {0,1,2,4,8,16};
  VAR i: LONGINT;
BEGIN
  IF val = NIL THEN pcO.align:=0
  ELSIF xcStr.StrToInt(val^,i) & (i IN valid) THEN
    pcO.align:=VAL(SHORTINT,i);
  ELSE
    pcS.err(162,"ALIGNMENT",valid,val^);
  END;
END SetAlign;

<* IF MCS THEN *>
PROCEDURE SetAlign2 (val: BOOLEAN);
BEGIN
  IF val THEN  pcO.align:= 2;   END;
END SetAlign2;
<* END *>


PROCEDURE EnumSize(VAR val: env.String);
  VAR i: LONGINT;
BEGIN
  IF val = NIL THEN enumsize:=1
  ELSIF xcStr.StrToInt(val^,i) & (i IN valid_size) THEN
    enumsize:=i;
  ELSE
    pcS.err(162,"ENUMSIZE",valid_size,val^);
  END;
END EnumSize;

PROCEDURE SetSize(VAR val: env.String);
  VAR i: LONGINT;
BEGIN
  IF val = NIL THEN setsize:=1
  ELSIF xcStr.StrToInt(val^,i) & (i IN valid_size) THEN
    setsize:=i;
  ELSE
    pcS.err(162,"SETSIZE",valid_size,val^);
  END;
END SetSize;

PROCEDURE SetVolatile(val: BOOLEAN);
BEGIN
  pcO.volatile:=val;
END SetVolatile;

(*----------------------------------------------------*)

PROCEDURE Set*;
  VAR p: PARS; i: LONGINT;
BEGIN
  NEW(p); p.new(pc.flag_o2,"Oberon-2 v2.40");
  NEW(p); p.new(pc.flag_m2,"Modula-2 v2.40");
  pcS.DeclareOptions;
  FOR i:=0 TO LEN(SP_ARGS_NIL)-1 DO SP_ARGS_NIL[i]:=NIL END;
  env.config.NewEquation("RTSSYMFILE");
  env.config.NewActiveEquation("ENUMSIZE",EnumSize);
  env.config.NewActiveEquation("SETSIZE",SetSize);
  env.config.NewActiveEquation("ALIGNMENT",SetAlign);
  env.config.NewActiveOption("VOLATILE",FALSE,SetVolatile);
  env.config.NewOption("WOFF319", TRUE, SYSTEM.VAL(env.CompilerOption,-1));
  env.config.NewOption ("ME_IRVIS",   FALSE, SYSTEM.VAL(env.CompilerOption,-1));
  env.config.NewOption ("FE_IRVIS",   FALSE, SYSTEM.VAL(env.CompilerOption,-1));
<* IF MCS THEN *>
  env.config.NewActiveOption("ALIGNMENT2", FALSE, SetAlign2);
<* END *>
END Set;

END pcM2.
