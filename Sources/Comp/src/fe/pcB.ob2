(** Copyright (c) 1991,97 XDS Ltd, Russia. All Rights Reserved. *)
(** M2/O2 front-end: tree building and compatibility *)
MODULE pcB; (** Ned 29-Jun-91. *)
            (** Sem 22-Sep-93. *)

(* Modifications:
   03-Mar-96 Ned  chk_index: "ty_SS IN SET" is converted to "ty_char".
   14-Mar-96 Ned  BUG006: chk_threat is implemented
                  checked for all variables, called procedures only
                  if control variable is threatened.
   17-Mar-96 Ned  chk_call (chk_threat). Skip "this" for method.
   19-Mar-96 Ned  relaxed parameter compatibility for C, Pascal,
                  StdCall, SysCall procedures:
                  if actual and formal are pointer compare bases (=).
                  procedure "p_base_compatibility".
   23-Mar-96 Ned  procedure "cast" is added.
   29-Mar-96 Ned  check_assign: array of char := "..." only if extensions!
   03-Apr-96 Ned  assign_compatibility: relaxed assignment compatibility
                  for pointers of RELAXED types
   25-Apr-96 Ned  BUG0115: chk_threat - check for variable
                  BUG0113: addr^:=byte compatibility
                  BUG0105: BOOL8/BOOL32 assign. & expr. compatibility
*)

IMPORT
  <* IF DB_TRACE THEN *> dbg := pcVisIR, <* END *>
  pc:=pcK,
  pcS,
  pcO,
  fmt:=xcStr,
  env:=xiEnv;

CONST
  incomp*  = 30;

CONST (* relaxed type compatibility *)
  RELAXED = pc.LangSet{ pc.flag_c, pc.flag_p,
                        pc.flag_syscall, pc.flag_javacall,
                        pc.flag_stdcall, pc.flag_vmcall, pc.flag_lightcall };
  ttag_sig_unsig_compat = pc.ttag_aux29;
VAR
  zz_tmp : pc.VALUE;
  zz_zero: pc.VALUE;
  rr_tmp1: pc.VALUE;
  rr_tmp2: pc.VALUE;

(*----------------------------------------------------------------*)

VAR
  cmp_value_tmp: pc.VALUE;

PROCEDURE cmp_value*(op: pc.SUB_MODE; x,y: pc.VALUE): BOOLEAN;
BEGIN
  IF cmp_value_tmp=NIL THEN
    cmp_value_tmp:=pc.value.new(pcS.txtpos,pcO.boolean);
  ELSE
    cmp_value_tmp.pos:=pcS.txtpos;
  END;
  cmp_value_tmp.binary(op,x,y);
  RETURN ~ cmp_value_tmp.is_zero();
END cmp_value;

PROCEDURE eval_value*(v: pc.NODE);
BEGIN
  IF env.errors.err_cnt>0 THEN RETURN END;
  pc.const.eval_value(v);
END eval_value;

(*----------------------------------------------------------------*)

PROCEDURE is_whole*(t: pc.STRUCT): BOOLEAN;
BEGIN
  RETURN t.mode IN (pc.INTs+pc.CARDs);
END is_whole;

PROCEDURE is_char*(t: pc.STRUCT): BOOLEAN;
BEGIN
  WHILE t.mode=pc.ty_range DO t:=t.base END;
  RETURN (t.mode=pc.ty_char) OR (t.mode=pc.ty_SS) & (t.len<=2);
END is_char;

PROCEDURE is_SS_char(t: pc.STRUCT): BOOLEAN;
(* Returns TRUE, if t is ty_SS, compatible with char *)
BEGIN
  RETURN (t.mode = pc.ty_SS) & (t.len <= 2)
END is_SS_char;

PROCEDURE convert_SS_to_char(n: pc.NODE);
  VAR v,ss: pc.VALUE;
BEGIN
  ASSERT(is_SS_char(n.type));
  IF (n.mode = pc.nd_var) & (n.obj.mode = pc.ob_cons) THEN
    IF n.obj.val.mode # pc.nd_value THEN RETURN END;
    ss:=n.obj.val.val;
  ELSE
    ASSERT(n.mode = pc.nd_value);
    ss:=n.val;
  END;
  n.mode:=pc.nd_value;
  n.type:=pcO.char;
  v:=pc.value.new(ss.pos,pcO.char);
  zz_tmp.index_get(0,ss);
  v.set_integer(zz_tmp.get_integer());
  n.val:=v;
END convert_SS_to_char;

PROCEDURE is_scalar*(t: pc.STRUCT): BOOLEAN;
BEGIN
  RETURN (t.mode IN pc.REALs) OR t.is_ordinal() OR is_SS_char(t)
END is_scalar;

(*------------------- node constructors -------------------------*)

PROCEDURE ^ binary(ps: pc.TPOS; VAR e: pc.NODE; e1: pc.NODE;
                   op: pc.SUB_MODE; type: pc.STRUCT);

PROCEDURE ^ chk_value(n: pc.NODE);

PROCEDURE array_of_equ(a,b: pc.STRUCT): BOOLEAN;
BEGIN
  RETURN
        (a=b) OR
        (a.mode=pc.ty_array_of) & (b.mode=pc.ty_array_of) &
        (a.inx=b.inx) & array_of_equ(a.base,b.base);
END array_of_equ;

PROCEDURE convert*(VAR n: pc.NODE; type: pc.STRUCT);
(** create node nd_unary, su_conv *)
  VAR x: pc.NODE; t: pc.STRUCT;
BEGIN
  IF n.type.mode=pcO.ty_invtype THEN RETURN END;
  IF n.mode = pc.nd_type THEN
    env.errors.Error(n.pos, 85);
  END;
  IF n.type=type THEN RETURN END;
  IF (type.mode=pc.ty_array_of) & array_of_equ(type,n.type) THEN
    RETURN
  ELSIF (type.mode IN pc.CPLXs) & ~ (n.type.mode IN pc.CPLXs) THEN
    pcO.new(x,pc.nd_value);
    x.val:=pcO.cardinal.min;
    x.type:=pcO.ZZ_type;
    CASE type.mode OF
      |pc.ty_complex : t:=pcO.real;
      |pc.ty_lcomplex: t:=pcO.longreal;
      |pc.ty_CC      : t:=pcO.RR_type;
    END;
    convert(x,t); convert(n,t);
    binary(n.pos,n,x,pc.sb_cmplx,type);
    RETURN;
  END;
  IF (n.mode=pc.nd_unary) & (n.sub=pc.su_conv) THEN
    IF (n.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) &
       (type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque})
    THEN n.type:=type; RETURN;
    END;
  END;
  pcO.new(x,pc.nd_unary);
  IF (env.range_check IN env.config.tags) & type.is_ordinal() THEN
    INCL(x.tags,pc.ntag_chk_range);
  END;
  x.sub:=pc.su_conv;
  x.type:=type;
  x.l:=n;
(*<An4*)
  IF pcS.TS_ext() AND (n.type.mode=pc.ty_ZZ) AND (type.mode=pc.ty_array_of) THEN
    convert(x.l, pcO.longlongint);
  END;
(*An4>*)
  x.pos := n.pos;
  x.end := n.end;
  n:=x;
END convert;

PROCEDURE lconvert(VAR n: pc.NODE; type: pc.STRUCT);
(** create node nd_lconv *)
  VAR x: pc.NODE;
BEGIN
  IF n.type.mode=pcO.ty_invtype THEN RETURN END;
  ASSERT(n.mode#pc.nd_type);
  IF n.type=type THEN RETURN END;
  IF (type.mode=pc.ty_array_of) & array_of_equ(type,n.type) THEN
    RETURN
  END;
  IF n.mode=pc.nd_lconv THEN
    IF (n.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) &
       (type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque})
    THEN n.type:=type; RETURN;
    END;
  END;
  pcO.new(x,pc.nd_lconv);
  x.type:=type;
  x.l:=n;
  x.pos := n.pos;
  x.end := n.end;
  n:=x;
END lconvert;

PROCEDURE unary*(VAR i: pc.NODE; op: pc.SUB_MODE; type: pc.STRUCT);
(** create node nd_unary *)
  VAR x: pc.NODE;
BEGIN
  IF op=pc.su_conv THEN
    convert(i,type);
  ELSE
    pcO.new(x,pc.nd_unary);
    x.end := pcS.txtpos;
    x.sub:=op;
    x.type:=type;
    x.l:=i;
    i:=x;
  END;
END unary;

PROCEDURE CC_to_type(val: pc.VALUE): pc.STRUCT;
BEGIN
  rr_tmp1.unary(pc.su_re,val);
  rr_tmp2.unary(pc.su_im,val);
  IF cmp_value(pc.sb_leq,pcO.real.min,rr_tmp1) &
     cmp_value(pc.sb_geq,pcO.real.max,rr_tmp1) &
     cmp_value(pc.sb_leq,pcO.real.min,rr_tmp2) &
     cmp_value(pc.sb_geq,pcO.real.max,rr_tmp2)
  THEN RETURN pcO.complex
  END;
  RETURN pcO.lcomplex;
END CC_to_type;

PROCEDURE RR_to_type(val: pc.VALUE): pc.STRUCT;
BEGIN
  IF cmp_value(pc.sb_leq,pcO.real.min,val) &
     cmp_value(pc.sb_geq,pcO.real.max,val)
  THEN RETURN pcO.real
  END;
  RETURN pcO.longreal;
END RR_to_type;

PROCEDURE cast*(VAR n: pc.NODE; type: pc.STRUCT; desig: BOOLEAN);
(** Generates type(n) or SYSTEM.CAST(type,n) *)
BEGIN
  chk_value(n);
  IF n.type.mode IN pc.NUM_LITERALs THEN
    IF ~ ((*pcS.oberon OR *)pcS.ext())
<* IF MCS THEN *>
      AND FALSE
<* END *>
    THEN pcS.error(n.pos,'e',137);
    ELSE
      IF n.type.is_ordinal() # type.is_ordinal() THEN
        pcS.error(n.pos,'w',350);
      END;
      IF n.type.mode IN pc.TY_SET{pc.ty_RR,pc.ty_CC} THEN
        (* calculate minimal type *)
        eval_value(n);
        IF n.mode = pc.nd_value THEN
          IF n.type.mode = pc.ty_RR THEN n.type:=RR_to_type(n.val);
          ELSE                           n.type:=CC_to_type(n.val);
          END;
        END;
      END;
    END;
  ELSIF is_SS_char(n.type) & (type.is_ordinal() OR (type.mode = pc.ty_loc)) THEN
    convert_SS_to_char(n);
  END;
  unary(n,pc.su_cast,type);
  IF desig THEN n.mode:=pc.nd_lconv; n.sub:=pc.su_none END;
END cast;

PROCEDURE TS_TypeTransferFunction*(VAR n: pc.NODE; type: pc.STRUCT): BOOLEAN;
(** Generates type(n) in TopSpeed meaning *)
VAR
  from, to: pc.TY_MODE;
BEGIN
  from := n.type.mode;
  to   := type.mode;

  chk_value(n);
  IF ((from IN pc.ADRs+pc.TY_SET{pc.ty_proctype, pc.ty_opaque}) AND (to IN pc.ADRs+pc.TY_SET{pc.ty_proctype, pc.ty_opaque})) OR
     ((to = pc.ty_loc) AND (pc.code.get_size(pc.su_bytes, n.type) = 1)) OR
(*<An3*)
     ((to = pc.ty_loc) AND pcS.TS_ext()) OR
     ((from = pc.ty_loc) AND pcS.TS_ext()) OR
     ((to = pc.ty_array) AND pcS.TS_ext()) OR
     ((from = pc.ty_array) AND pcS.TS_ext()) OR
(*An3>*)
     (((from IN pc.WHOLEs) AND (to = pc.ty_array)) AND (pc.code.get_size(pc.su_bytes, n.type) = pc.code.get_size(pc.su_bytes, type))) OR
     (((to IN pc.WHOLEs) AND (from = pc.ty_set)) AND (pc.code.get_size(pc.su_bytes, n.type) = pc.code.get_size(pc.su_bytes, type))) OR
     (((from IN pc.WHOLEs) AND (to = pc.ty_set)) AND (pc.code.get_size(pc.su_bytes, n.type) = pc.code.get_size(pc.su_bytes, type))) OR

     ((to = pc.ty_array) AND (type.mno < pc.ZEROMno) AND (type.base.mode = pc.ty_loc) AND (pc.code.get_size(pc.su_bytes, n.type) = type.len)) OR
     ((from IN pc.ADRs) AND (to IN pc.WHOLEs)) OR
     ((to IN pc.ADRs) AND (pc.code.get_size(pc.su_bytes, n.type) = 4)) OR
     ((to = pc.ty_char) AND (from IN pc.ADRs)) OR
     ((from IN pc.ADRs) AND (to = pc.ty_real))
  THEN
    unary(n,pc.su_cast,type);
  ELSIF ((from IN pc.ORDs + pc.REALs) AND (to IN pc.ORDs + pc.REALs))
  THEN
    unary(n,pc.su_conv,type);
  ELSIF (n.type = type) THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
  RETURN TRUE;
END TS_TypeTransferFunction;

PROCEDURE binary*(ps: pc.TPOS; VAR e: pc.NODE; e1: pc.NODE;
                  op: pc.SUB_MODE; type: pc.STRUCT);
(** create node nd_binary *)
  VAR x: pc.NODE;
BEGIN
  pcO.new(x,pc.nd_binary);
  x.pos := ps;
  x.end := e1.end;
  x.sub:=op;
  x.l:=e;
  x.r:=e1;
  x.type:=type;
  e:=x;
END binary;

PROCEDURE weak*(n: pc.NODE);
(** set overflow check tags nd_binary,nd_unary according to options *)
  VAR w: BOOLEAN; tg: pc.NTAG;
BEGIN
  IF n.mode=pc.nd_value THEN RETURN END;
  tg:=pc.ntag_chk_overflow;
  IF n.type.mode IN (pc.REALs+pc.CPLXs) THEN
    w:=env.float_ovfl IN env.config.tags;
  ELSIF n.type.mode IN pc.CARDs THEN
    w:=(env.card_ovfl IN env.config.tags) AND ~(ttag_sig_unsig_compat IN n.type.tags);
  ELSIF n.type.mode IN pc.ADRs THEN
    w:=env.nil_check IN env.config.tags;
    tg:=pc.ntag_chk_range;
  ELSE
    w:=env.int_ovfl IN env.config.tags
  END;
  IF w THEN INCL(n.tags,tg) END;
END weak;

PROCEDURE guard*(VAR n: pc.NODE; type: pc.STRUCT; chk: BOOLEAN);
(** create node  nd_guard *)
  VAR x: pc.NODE;
BEGIN
  IF n.type=type THEN RETURN END;
  IF (n.mode#pc.nd_guard) OR (pc.ntag_chk_range IN n.tags) THEN
    pcO.new(x,pc.nd_guard);
    x.l:=n;
    x.pos := n.pos;
    x.end := n.end;
    n:=x;
  END;
  n.type:=type;
  IF chk & (env.guard_check IN env.config.tags) THEN
    INCL(n.tags,pc.ntag_chk_range);
  END;
END guard;

(*----------------------- errors  --------------------------------*)

PROCEDURE type_string*(VAR s: ARRAY OF CHAR; t: pc.STRUCT): BOOLEAN;
  VAR i: INTEGER;
  PROCEDURE app(txt: ARRAY OF CHAR);
    VAR j: INTEGER;
  BEGIN
    j:=0;
    WHILE (i<LEN(s)-1) & (j<LEN(txt)) & (txt[j]#0X) DO
      s[i]:=txt[j]; INC(i); INC(j);
    END;
    s[i]:=0X;
  END app;
  PROCEDURE valid_name(nm-: ARRAY OF CHAR): BOOLEAN;
    VAR ch: CHAR; i: INTEGER;
  BEGIN
    i:=0;
    WHILE nm[i]#0X DO
      ch:=nm[i];
      IF (ch>='0') & (ch<='9') & (i>0) OR
         (ch>='A') & (ch<='Z') OR
         (ch>='a') & (ch<='z') OR
         (ch='_')
      THEN (* ok *)
      ELSE RETURN FALSE;
      END;
      INC(i);
    END;
    RETURN i>0;
  END valid_name;
  VAR bf: ARRAY 80 OF CHAR; o: pc.OBJECT;
BEGIN
  s[0]:=0X; i:=0;
  LOOP
    IF (t.obj#NIL) &
       (t.obj.mno>=pc.ZEROMno) &
       (t.obj.mode=pc.ob_type) &
       valid_name(t.obj.name^)
    THEN
      app(pc.mods[t.obj.mno].name^);
      app(".");
      app(t.obj.name^);
      RETURN TRUE;
    END;
    CASE t.mode OF
      |pc.ty_shortcard: app("CARD8");       RETURN TRUE;
      |pc.ty_cardinal : app("CARD16");      RETURN TRUE;
      |pc.ty_longcard : app("CARD32");      RETURN TRUE;
      |pc.ty_shortint : app("INT8");        RETURN TRUE;
      |pc.ty_integer  : app("INT16");       RETURN TRUE;
      |pc.ty_longint  : app("INT32");       RETURN TRUE;
      |pc.ty_real     : app("REAL");        RETURN TRUE;
      |pc.ty_longreal : app("LONGREAL");    RETURN TRUE;
      |pc.ty_ld_real  : app("LONGLONGREAL"); RETURN TRUE;
      |pc.ty_complex  : app("COMPLEX");     RETURN TRUE;
      |pc.ty_lcomplex : app("LONGCOMPLEX"); RETURN TRUE;
      |pc.ty_boolean  : IF t.base.mode=pc.ty_longcard THEN
                          app("BOOL32");  RETURN TRUE;
                        ELSIF t.base.mode=pc.ty_cardinal THEN
                          app("BOOL16");  RETURN TRUE;
                        ELSE
                          app("BOOLEAN"); RETURN TRUE;
                        END;
      |pc.ty_char     : app("CHAR");        RETURN TRUE;
      |pc.ty_loc      : app("LOC");         RETURN TRUE;
      |pc.ty_AA       : app("NILTYPE");     RETURN TRUE;
      |pc.ty_ZZ       : app("whole constant (ZZ)"); RETURN TRUE;
      |pc.ty_RR       : app("real constant (RR)"); RETURN TRUE;
      |pc.ty_CC       : app("complex constant (CC)"); RETURN TRUE;
      |pc.ty_SS       : app("string constant (SS)"); RETURN TRUE;
      |pc.ty_pointer  : IF t=pcO.addr THEN
                          app("SYSTEM.ADDRESS");
                          RETURN TRUE;
                        END;
                        app("POINTER TO ");  t:=t.base;
      |pc.ty_range     : fmt.prn_txt(bf,"[%d .. %d]", t.min.get_integer(), t.max.get_integer());
                         app(bf);
                         RETURN TRUE;
      |pc.ty_set      : app("SET OF ");     t:=t.base;
      |pc.ty_array    : fmt.prn_txt(bf,"ARRAY %d OF ",t.len); app(bf);
                        t:=t.base;
      |pc.ty_array_of : fmt.prn_txt(bf,"OPEN ARRAY OF "); app(bf);
                        t:=t.base;
      |pc.ty_proctype :
                        app("PROCEDURE ");
                        CASE t.flag OF
                          |pc.flag_o2       : app("PROCEDURE [Oberon] ");
                          |pc.flag_m2       : app("PROCEDURE [Modula] ");
                          |pc.flag_c        : app("PROCEDURE [C] ");
                          |pc.flag_sl1      : app("PROCEDURE [SL1] ");
                          |pc.flag_p        : app("PROCEDURE [Pascal] ");
                          |pc.flag_javacall : app("PROCEDURE [JavaCall] ");
                          |pc.flag_syscall  : app("PROCEDURE [SysCall] ");
                          |pc.flag_stdcall  : app("PROCEDURE [StdCall] ");
                          |pc.flag_vmcall   : app("PROCEDURE [VMCall] ");
                          |pc.flag_lightcall: app("PROCEDURE [LightCall] ");
                        ELSE RETURN FALSE
                        END;
                        IF pc.ttag_throws IN t.tags THEN
                          app("[throws]");
                        END;
                        app(" (");
                        o:=t.prof;
                        WHILE o#NIL DO
                          IF o.mode=pc.ob_varpar THEN app("VAR ");
                          ELSIF o.mode=pc.ob_seq THEN app("SEQ ");
                          END;
                          IF ~ type_string(bf,o.type) THEN RETURN FALSE END;
                          app(bf);
                          o:=o.next;
                          IF o#NIL THEN app(",") END;
                        END;
                        app(")");
                        IF t.base.mode#pc.ty_void THEN
                          app(": ");
                          IF ~ type_string(bf,t.base) THEN RETURN FALSE END;
                          app(bf);
                        END;
                        RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END;
END type_string;

PROCEDURE err*(n: pc.NODE; no: INTEGER);
BEGIN
  IF (n.mode#pc.nd_inv) & (n.type.mode#pcO.ty_invtype) THEN
    pcS.err(no); n.mode:=pc.nd_inv; n.type:=pcO.invtype;
  END;
END err;

PROCEDURE type_incomp(ps: pc.TPOS; t1,t2: pc.STRUCT);
  VAR s1,s2: ARRAY 70 OF CHAR; l,s: INTEGER;
BEGIN
  (* !!!!! нужно различать виды совместимости типов:
        assign, expression, parameter
  *)
  IF t1.mode=pcO.ty_invtype THEN RETURN END;
  IF t2.mode=pcO.ty_invtype THEN RETURN END;
  IF (t1.mode=pc.ty_proctype) &
     (t2.mode=pc.ty_proctype) &
     (t1.flag#t2.flag)
  THEN l:=152; s:=153;
  ELSE l:=29; s:=30;
  END;
  IF type_string(s1,t1) & type_string(s2,t2) THEN
    pcS.error(ps,'e',l,s1,s2);
  ELSE
    pcS.error(ps,'e',s);
  END;
END type_incomp;

PROCEDURE proc_incomp*(ps: pc.TPOS; nm-: ARRAY OF CHAR; t1,t2: pc.STRUCT);
  VAR s1,s2: ARRAY 70 OF CHAR;
BEGIN
  IF t1.mode=pcO.ty_invtype THEN RETURN END;
  IF t2.mode=pcO.ty_invtype THEN RETURN END;
  IF type_string(s1,t1) & type_string(s2,t2) THEN
    pcS.error(ps,'e',154,nm,s1,s2);
  ELSE
    pcS.error(ps,'e',155,nm);
  END;
END proc_incomp;

(*----------------------------------------------------------------*)

PROCEDURE host_type*(t: pc.STRUCT): pc.STRUCT;
BEGIN
  IF t.mode=pc.ty_range THEN
      t:=t.base
  END;
  IF t.obj#NIL THEN
      t:=t.obj.type
  END;
  RETURN t;
END host_type;

PROCEDURE type_in*(t: pc.STRUCT; valid: pc.TY_SET);
BEGIN
  WHILE t.mode=pc.ty_range DO t:=t.base END;
  IF (t.mode#pcO.ty_invtype) & ~ (t.mode IN valid) THEN pcS.err(incomp) END;
END type_in;

PROCEDURE type_equ*(t: pc.STRUCT; mode: pc.TY_MODE);
BEGIN
  WHILE t.mode=pc.ty_range DO t:=t.base END;
  IF (t.mode#pcO.ty_invtype) & (t.mode#mode) THEN pcS.err(incomp) END;
END type_equ;

PROCEDURE chk_proc_value(r: pc.NODE);
(* приводит к процедурному значению *)

  PROCEDURE CheckObj(obj:pc.OBJECT);
  BEGIN
    CASE obj.mode OF
    | pc.ob_proc:
      IF obj.lev=0 THEN obj.mode:=pc.ob_xproc
      ELSE pcS.err(282,obj.name^);
      END;
    | pc.ob_cproc:
      pcS.err(283)
    ELSE
    END;
  END CheckObj;

BEGIN
  ASSERT(r.mode#pc.nd_type);
  ASSERT(r.type.mode=pc.ty_proctype);
  IF r.mode=pc.nd_proc THEN
    CheckObj(r.obj);
  ELSIF r.mode=pc.nd_method THEN pcS.err(281)
  ELSIF (r.mode=pc.nd_var) & (r.obj.mode=pc.ob_cons) & (r.obj.val#NIL)
       & (r.obj.val.val#NIL) & (r.obj.val.val.get_object() # NIL)
  THEN -- XDS-13 fix
    CheckObj(r.obj.val.val.get_object());
  END;
END chk_proc_value;

PROCEDURE chk_value*(n: pc.NODE);
BEGIN
  ASSERT(n.type#NIL);
  IF n.mode IN pc.NODE_SET
     {pc.nd_inv,pc.nd_var,pc.nd_proc,pc.nd_method,
      pc.nd_field,pc.nd_index,pc.nd_deref,pc.nd_eguard,
      pc.nd_guard,pc.nd_binary,pc.nd_unary,
      pc.nd_value,pc.nd_aggregate,pc.nd_sequence,
      pc.nd_call,pc.nd_lconv}
  THEN
    IF n.type.mode=pc.ty_proctype THEN chk_proc_value(n) END;
    IF (n.mode = pc.nd_field) AND (n.l.mode = pc.nd_type) THEN
      pcS.err(incomp);
    END;
  ELSIF n.type.mode=pcO.ty_invtype THEN
    (* nothing *)
  ELSE
    pcS.error(n.pos,'e',76);
    n.mode:=pc.nd_inv;
    n.type:=pcO.invtype;
  END;
END chk_value;

PROCEDURE is_lvalue(n: pc.NODE; VAR ro: BOOLEAN;check_designator:=FALSE:BOOLEAN): BOOLEAN;
BEGIN
  ro:=FALSE;
  IF n.type.mode=pcO.ty_invtype THEN RETURN TRUE END;
  CASE n.mode OF
    |pc.nd_inv   :
    |pc.nd_var   :
      IF (n.obj.mode=pc.ob_cons) OR
         (pc.otag_RO IN n.obj.tags) OR
         (n.obj.mno#pc.cur_mod) & (pc.otag_RO_public IN n.obj.tags)
      THEN
        ro:=TRUE; RETURN FALSE;
      END;
    |pc.nd_field :
      IF (n.obj.host.mno#pc.cur_mod) & (pc.otag_RO_public IN n.obj.tags) THEN
        ro:=TRUE; RETURN FALSE;
      END;
      RETURN is_lvalue(n.l,ro,check_designator);
    |pc.nd_index : RETURN is_lvalue(n.l,ro,check_designator);
    |pc.nd_deref : RETURN TRUE;
    |pc.nd_eguard: RETURN is_lvalue(n.l,ro,check_designator);
    |pc.nd_guard : RETURN is_lvalue(n.l,ro,check_designator);
    |pc.nd_lconv : RETURN is_lvalue(n.l,ro,check_designator);
    |pc.nd_call  : RETURN pcS.ext() & check_designator;
  ELSE
    ASSERT(~ (n.mode IN pc.LVALUEs));
    RETURN FALSE;
  END;
  RETURN TRUE;
END is_lvalue;

PROCEDURE chk_designator*(n: pc.NODE);
  VAR ro: BOOLEAN;
BEGIN
  IF ~ is_lvalue(n,ro,TRUE) & ~ ro THEN
    pcS.error(n.pos,'e',54);
    n.type:=pcO.invtype;
  END;
END chk_designator;

PROCEDURE chk_lvalue*(n: pc.NODE);
  VAR ro: BOOLEAN;
BEGIN
  IF ~ is_lvalue(n,ro) THEN
    IF ro THEN pcS.error(n.pos,'e',123)
    ELSE pcS.error(n.pos,'e',54); n.type:=pcO.invtype;
    END;
  END;
END chk_lvalue;

PROCEDURE is_entire_designator(n: pc.NODE): BOOLEAN;
BEGIN
  IF n.type.mode=pcO.ty_invtype THEN RETURN TRUE END;
  CASE n.mode OF
    |pc.nd_inv   :
    |pc.nd_var   : IF n.obj.mode=pc.ob_cons THEN RETURN FALSE END;
    |pc.nd_eguard:
    |pc.nd_guard : RETURN is_entire_designator(n.l);
    |pc.nd_field : RETURN pcS.TS_ext() OR pcS.ext();
  ELSE RETURN FALSE;
  END;
  RETURN TRUE;
END is_entire_designator;

PROCEDURE chk_entire_designator*(n: pc.NODE);
BEGIN
  IF ~ is_entire_designator(n) THEN
    pcS.error(n.pos,'e',139);
  END;
END chk_entire_designator;

PROCEDURE chk_ordinal*(VAR t: pc.STRUCT);
BEGIN
  IF t.mode=pcO.ty_invtype THEN RETURN END;
  IF t.is_ordinal() OR is_SS_char(t) THEN RETURN END;
  pcS.err(33); t:=pcO.invtype;
END chk_ordinal;

PROCEDURE lit_value_type*(val: pc.VALUE; fr,to: pc.STRUCT): pc.STRUCT;
BEGIN
  IF fr.mode IN pc.CPLXs THEN RETURN CC_to_type(val) END;
  IF fr.mode IN pc.REALs THEN RETURN RR_to_type(val) END;
  ASSERT(fr.mode IN pc.WHOLEs);
  IF ~ to.is_ordinal() OR to.signed() THEN
    IF to.is_ordinal() & to.signed() & (fr.mode = pc.ty_ZZ) & (val.get_radix() # 10) THEN
      val.cast_ordinal(to);
    END;
    IF cmp_value(pc.sb_leq,pcO.shortint.min,val) &
       cmp_value(pc.sb_geq,pcO.shortint.max,val)
    THEN RETURN pcO.shortint
    END;
    IF cmp_value(pc.sb_leq,pcO.integer.min,val) &
       cmp_value(pc.sb_geq,pcO.integer.max,val)
    THEN RETURN pcO.integer
    END;
    IF cmp_value(pc.sb_leq,pcO.longint.min,val) &
       cmp_value(pc.sb_geq,pcO.longint.max,val)
    THEN RETURN pcO.longint;
    END;
    (* Sergic bug 873*)
    IF cmp_value(pc.sb_leq,pcO.longlongint.min,val) &
       cmp_value(pc.sb_geq,pcO.longlongint.max,val)
    THEN RETURN pcO.longlongint;
    END;
  --  RETURN pcO.longlongint;
    RETURN pcO.longcard;
  END;
  IF val.is_neg() THEN RETURN pcO.ZZ_type END;
  IF cmp_value(pc.sb_geq,pcO.shortcard.max,val) THEN RETURN pcO.shortcard END;
  IF cmp_value(pc.sb_geq,pcO.cardinal.max,val) THEN RETURN pcO.cardinal END;
  RETURN pcO.longcard;
END lit_value_type;

PROCEDURE chk_index*(t: pc.STRUCT; VAR n: pc.NODE; in_set: BOOLEAN);
(** indexation, in set, case *)
  VAR t1,t2: pc.STRUCT; m1,m2: pc.TY_MODE;
BEGIN
  chk_value(n);
  t1:=t; t2:=n.type;
  IF t1.mode=pc.ty_range THEN t1:=t1.base END;
  IF t2.mode=pc.ty_range THEN t2:=t2.base END;
  m1:=t1.mode;
  m2:=t2.mode;
  IF t1=t2 THEN
  ELSIF (m1 IN pc.WHOLEs) & (m2 IN pc.WHOLEs) THEN
  ELSIF (m1 = pc.ty_boolean) & (m2 = pc.ty_boolean) THEN
  ELSIF (m1=pc.ty_char) & is_SS_char(t2) THEN
    IF in_set THEN convert_SS_to_char(n) END;
  ELSIF (m1=pcO.ty_invtype) OR (m2=pcO.ty_invtype) THEN
  ELSE pcS.error(n.pos,'e',incomp);
  END;
  (* do not convert "n" to "t" in "sb_in"! *)
  IF ~ in_set THEN
    convert(n,t);
  ELSIF n.type.mode=pc.ty_ZZ THEN
    eval_value(n);
    IF n.mode=pc.nd_value THEN
      convert(n,lit_value_type(n.val,n.type,pcO.longint));
    ELSE
      convert(n,pcO.longint);
    END;
  END;
END chk_index;

PROCEDURE ^ proc_types_cmp*(proc1,proc2: pc.STRUCT; copy: BOOLEAN): BOOLEAN;

PROCEDURE ^ assign_compatible(l,r: pc.STRUCT): BOOLEAN;


PROCEDURE cmp_params*(p1,p2: pc.OBJECT; copy: BOOLEAN): BOOLEAN;
  VAR t1,t2: pc.STRUCT;
BEGIN
  WHILE (p1#NIL) & (p2#NIL) DO
    t1:=p1.type; t2:=p2.type;
    IF p1.mode#p2.mode THEN RETURN FALSE END;
    IF (pcO.otag_varpar_nil IN p1.tags) # (pcO.otag_varpar_nil IN p2.tags) THEN
      RETURN FALSE
    END;
    IF pcO.otag_varpar_nil IN p1.tags THEN
      t1:=t1.base;
      t2:=t2.base;
    END;
    WHILE (t1.mode=pc.ty_array_of) & (t2.mode=pc.ty_array_of) DO
      t1:=t1.base; t2:=t2.base;
    END;
    IF (t1.mode=pc.ty_opaque) & (t1.base#NIL) THEN t1:=t1.base END;
    IF (t2.mode=pc.ty_opaque) & (t2.base#NIL) THEN t2:=t2.base END;
    IF t1.obj#NIL THEN t1:=t1.obj.type END;
    IF t2.obj#NIL THEN t2:=t2.obj.type END;
    IF t1#t2 THEN
      IF NOT pcS.TS_ext() THEN
        IF copy & ~ pcS.oberon THEN RETURN FALSE END;
      END;
      IF ~ proc_types_cmp(t1,t2,FALSE) THEN
        IF NOT pcS.TS_ext() THEN RETURN FALSE; END;
        IF NOT ( ((t1 = pcO.addr) AND (t2.mode IN pc.ADRs)) OR ((t2 = pcO.addr) AND (t1.mode IN pc.ADRs)) ) THEN RETURN FALSE END;
      END;
    END;
    IF copy THEN
      pcO.set_name(p1,p2.name^);
      p1.tags:=p1.tags+p2.tags*pc.OTAG_SET{pc.otag_RO,pc.otag_valpar};
      p1.pos:=p2.pos;
      p1.end:=p2.end;
      p1.flag:=p2.flag;
    END;
    p1:=p1.next;
    p2:=p2.next;
  END;
  RETURN p1=p2;
END cmp_params;

PROCEDURE proc_types_cmp*(proc1,proc2: pc.STRUCT; copy: BOOLEAN): BOOLEAN;
(* copy = TRUE если сравнение с предварительным описанием процедуры,
        при этом копируются имена и тэги параметров,
        для Модулы в этом случае требуется точное совпадение типов
        параметров.
        Копирование из proc2 в proc1.
*)
  VAR b1,b2: pc.STRUCT;
BEGIN
  IF (proc1.mode=pcO.ty_invtype) OR
     (proc2.mode=pcO.ty_invtype)
  THEN RETURN TRUE;
  END;
  IF copy THEN
    proc1.pos:=proc2.pos;
    proc1.end:=proc2.end;
  END;
  b1:=proc1.base;
  b2:=proc2.base;
  IF (b1#NIL) & (b1.mode=pc.ty_opaque) & (b1.base#NIL) THEN b1:=b1.base END;
  IF (b2#NIL) & (b2.mode=pc.ty_opaque) & (b2.base#NIL) THEN b2:=b2.base END;
  IF (b1#NIL) & (b1.obj#NIL) THEN b1:=b1.obj.type END;
  IF (b2#NIL) & (b2.obj#NIL) THEN b2:=b2.obj.type END;
  RETURN
    (proc1.mode=pc.ty_proctype) &
    (proc2.mode=pc.ty_proctype) &
    cmp_params(proc1.prof,proc2.prof,copy) &
    (b1=b2) &
    ((env.errors.err_cnt#0) OR (proc1.flag=proc2.flag)) &
    ((pc.ttag_throws IN proc1.tags)=(pc.ttag_throws IN proc2.tags))
    ;
END proc_types_cmp;

PROCEDURE type_inclusion(fr,to: pc.STRUCT): BOOLEAN;
  VAR m: pc.TY_SET;
BEGIN
  IF ~ (to.mode IN (pc.CPLXs+pc.REALs+pc.WHOLEs)) THEN RETURN FALSE END;
  IF fr.mode=to.mode THEN RETURN TRUE END;
  CASE to.mode OF
    |pc.ty_complex   : m:=pc.TY_SET{pc.ty_real}+pc.WHOLEs-pc.TY_SET{pc.ty_ZZ};
    |pc.ty_lcomplex  : m:=pc.TY_SET{pc.ty_complex}+pc.REALs+pc.WHOLEs-pc.TY_SET{pc.ty_ZZ,pc.ty_RR};
    |pc.ty_CC        : m:=pc.REALs+pc.WHOLEs;
    |pc.ty_ld_real   : m:=pc.TY_SET{pc.ty_real,pc.ty_longreal}+pc.WHOLEs-pc.TY_SET{pc.ty_ZZ};
    |pc.ty_longreal  : m:=pc.TY_SET{pc.ty_real}+pc.WHOLEs-pc.TY_SET{pc.ty_ZZ};
    |pc.ty_real      : m:=pc.WHOLEs-pc.TY_SET{pc.ty_ZZ};
    |pc.ty_RR        : m:=pc.TY_SET{pc.ty_ZZ};
    |pc.ty_longint   : m:=pc.TY_SET{pc.ty_integer,pc.ty_cardinal,pc.ty_shortint,pc.ty_shortcard};
    |pc.ty_longlongint:m:=pc.TY_SET{pc.ty_longint,pc.ty_integer,pc.ty_cardinal,pc.ty_shortint,pc.ty_shortcard};
    |pc.ty_integer   : m:=pc.TY_SET{pc.ty_shortint,pc.ty_shortcard};
    |pc.ty_longcard  : m:=pc.TY_SET{pc.ty_integer,pc.ty_cardinal,pc.ty_shortint,pc.ty_shortcard};
    |pc.ty_longlongcard:m:=pc.TY_SET{pc.ty_longint,pc.ty_integer,pc.ty_cardinal,pc.ty_shortint,pc.ty_shortcard,pc.ty_longcard};
    |pc.ty_cardinal  : m:=pc.TY_SET{pc.ty_shortint,pc.ty_shortcard};
  ELSE m:=pc.TY_SET{};
  END;
  RETURN fr.mode IN m;
END type_inclusion;

PROCEDURE type_extension(fr,to: pc.STRUCT): BOOLEAN;
BEGIN
  IF (fr.mode=pc.ty_pointer) & (to.mode=pc.ty_pointer) THEN
    RETURN (fr.flag=pc.flag_o2) &
           (to.flag=pc.flag_o2) &
           type_extension(fr.base,to.base);
  END;
  IF fr.obj#NIL THEN fr:=fr.obj.type END;
  IF to.obj#NIL THEN to:=to.obj.type END;
  IF (fr=to) & (fr.mode#pc.ty_array_of) THEN RETURN TRUE END;
  IF (fr.mode#pc.ty_record) OR (to.mode#pc.ty_record) THEN RETURN FALSE END;
  RETURN (to.base#NIL) & type_extension(fr,to.base);
END type_extension;

PROCEDURE assign_compatible(l,r: pc.STRUCT): BOOLEAN;

  VAR t1,t2,b1: pc.STRUCT;

  PROCEDURE type_sign_conversion(fr,to: pc.STRUCT): BOOLEAN;
  BEGIN
    CASE fr.mode OF
      |pc.ty_shortint : RETURN to.mode=pc.ty_shortcard;
      |pc.ty_integer  : RETURN to.mode=pc.ty_cardinal;
      |pc.ty_longint  : RETURN to.mode=pc.ty_longcard;
      |pc.ty_shortcard: RETURN to.mode=pc.ty_shortint;
      |pc.ty_cardinal : RETURN to.mode=pc.ty_integer;
      |pc.ty_longcard : RETURN to.mode=pc.ty_longint;
    ELSE RETURN FALSE;
    END;
  END type_sign_conversion;

  PROCEDURE chk(): BOOLEAN;
  BEGIN
    CASE b1.mode OF
     |pc.ty_shortint,pc.ty_integer,pc.ty_longint,pc.ty_longlongint,
      pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard,pc.ty_longlongcard:
       RETURN ~ pcS.oberon & (t2.mode IN pc.WHOLEs) OR
              pcS.oberon & type_inclusion(t2,b1) OR
              type_sign_conversion(t2,b1) OR
              pcS.oberon & (t2=pcO.addr) & (b1.mode=pc.ty_longint);
     |pc.ty_boolean:
        RETURN (* compatibility between different boolean types *)
         (t2.mode = pc.ty_boolean);
     |pc.ty_real,pc.ty_longreal,pc.ty_ld_real:
       RETURN
        ~ pcS.oberon & (t2.mode=pc.ty_RR) OR
        pcS.oberon & type_inclusion(t2,b1) OR
<* IF ~MCS THEN *>
        ~ pcS.oberon & pcS.ext() &
<* END *>
         (t2.mode IN pc.REALs);
     |pc.ty_complex,pc.ty_lcomplex:
       RETURN
        ~ pcS.oberon & (t2.mode=pc.ty_CC) OR
        pcS.oberon & type_inclusion(t2,b1);
     |pc.ty_pointer:
       RETURN
        (t2.mode=pc.ty_AA) OR
        (t2=pcO.addr) & (t1.flag#pc.flag_o2) OR
        (t1=pcO.addr) & (t2.mode=pc.ty_pointer) OR
        pcS.oberon & (t1=pcO.addr) & (t2.mode=pc.ty_longint);
     |pc.ty_opaque:
       RETURN
        (t2.mode=pc.ty_AA);
     |pc.ty_proctype:
       RETURN
        (pcS.oberon OR pcS.ext()) & (t2.mode=pc.ty_AA) OR
        (t2.obj#NIL) & (t2.obj.mode IN pc.PROCs) & proc_types_cmp(t1,t2,FALSE);
     |pc.ty_char:
       RETURN is_char(t2);
     |pc.ty_array:
       RETURN
        is_char(t1.base) & (t2.mode=pc.ty_SS) &
        (t2.len<=t1.len) & pcS.oberon OR
        is_char(t1.base) & (t2.mode=pc.ty_SS) &
        (t2.len-1<=t1.len) & ~ pcS.oberon OR
        pcS.ext() & (t1.base.mode=pc.ty_loc) &
        (t1.len=pc.code.get_size(pc.su_size,t2));
     |pc.ty_loc:
       RETURN
          (pcS.ext() OR pcS.oberon)
        & ~ (t2.mode IN pc.NUM_LITERALs+pc.TY_SET{pc.ty_SS})
        & (pc.code.get_size(pc.su_size,t2) = 1)

(*!! Ned: not important as extension and can lead to unpredicted results.
     |pc.ty_array_of:
       RETURN
        pcS.ext() & is_char(t1.base) & (t2.mode=pc.ty_SS);
*)
    ELSE RETURN FALSE;
    END;
  END chk;

BEGIN
  t1:=l; t2:=r;
  WHILE (t1.mode=pc.ty_opaque) & (t1.base#NIL) DO t1:=t1.base END;
  WHILE (t2.mode=pc.ty_opaque) & (t2.base#NIL) DO t2:=t2.base END;
  b1:=t1;
  IF b1.mode=pc.ty_range THEN b1:=b1.base END;
  IF t2.mode=pc.ty_range THEN t2:=t2.base END;

  IF b1.obj#NIL THEN b1:=b1.obj.type END;
  IF t1.obj#NIL THEN t1:=t1.obj.type END;
  IF t2.obj#NIL THEN t2:=t2.obj.type END;

  RETURN
     (t2.mode=pcO.ty_invtype) OR
     (t1.mode=pcO.ty_invtype) OR
     ((t1=t2) & (t1.mode#pc.ty_array_of)) OR
     ((b1=t2) & (b1.mode#pc.ty_array_of)) OR
     chk();
END assign_compatible;

PROCEDURE o2_const_assign(l: pc.STRUCT; VAR r: pc.NODE): BOOLEAN;
  VAR t: pc.STRUCT;
BEGIN
  t:=host_type(r.type);
  IF ~ (t.mode IN (pc.WHOLEs+pc.REALs+pc.CPLXs)) THEN RETURN FALSE END;
  IF l.mode = pc.ty_loc THEN (* see byte_const_assign *) RETURN FALSE END;
  IF env.errors.err_cnt>0 THEN RETURN TRUE END;

  eval_value(r);

  IF r.mode=pc.nd_value THEN
    r.type:=lit_value_type(r.val,t,l);
  END;
  RETURN assign_compatible(l,r.type);
END o2_const_assign;

PROCEDURE byte_const_assign(l: pc.STRUCT; r: pc.NODE): BOOLEAN;
BEGIN
  ASSERT(l.mode = pc.ty_loc);
  IF pc.code.bits_per_loc # 8 THEN RETURN FALSE END;
  IF r.type.is_ordinal() THEN
    eval_value(r);
    IF r.mode = pc.nd_value THEN
      IF r.val.is_neg() THEN
        IF cmp_value(pc.sb_leq,pcO.shortint.min,r.val) THEN
          r.type:=pcO.shortint; RETURN TRUE
        END;
      ELSIF cmp_value(pc.sb_geq,pcO.shortcard.max,r.val) THEN
        r.type:=pcO.shortcard; RETURN TRUE
      END;
    ELSE (* cannot evaluate *)
      RETURN (env.errors.err_cnt > 0)
    END;
  ELSIF is_SS_char(r.type) THEN
    convert_SS_to_char(r); RETURN TRUE
  END;
  RETURN FALSE
END byte_const_assign;

PROCEDURE TS_byte_assign(l: pc.STRUCT; r: pc.NODE): BOOLEAN;
BEGIN
  ASSERT(r.type.mode = pc.ty_loc);
  IF pc.code.bits_per_loc # 8 THEN RETURN FALSE END;
  IF pc.code.get_size(pc.su_bytes, l) = 1 THEN
    RETURN TRUE;
  END;
  RETURN FALSE
END TS_byte_assign;

PROCEDURE p_base_compatibility(t: pc.STRUCT; VAR n: pc.NODE): BOOLEAN;
(* relaxed compatibility for "C" pointers *)
  VAR e: pc.STRUCT;
BEGIN
  e:=n.type;
  ASSERT(t.mode=pc.ty_pointer);
  IF (e.mode#pc.ty_pointer) OR (t.base#e.base) THEN RETURN FALSE END;
  unary(n,pc.su_cast,t);
  RETURN TRUE;
END p_base_compatibility;

PROCEDURE assign_compatibility*(ps: pc.TPOS; l: pc.STRUCT; VAR r: pc.NODE);
BEGIN
  chk_value(r);
  IF assign_compatible(l,r.type) THEN
    IF r.type#l THEN convert(r,l) END;
  ELSIF pcS.oberon & type_extension(l,r.type) THEN
    guard(r,l,FALSE);
  ELSIF pcS.oberon & o2_const_assign(l,r) THEN
    convert(r,l);
  ELSIF (l.mode = pc.ty_loc) & byte_const_assign(l,r) THEN
    IF ~ pcS.ext() THEN
      pcS.error(ps,'e',102,"(extended compatibility with byte)")
    END;
    convert(r,l);
  ELSIF pcS.TS_ext() & (((r.type.mode = pc.ty_loc) & TS_byte_assign(l,r)) OR
                        ((r.type.mode = pc.ty_array) & (r.type.base.mode = pc.ty_loc) & (r.type.mno < pc.ZEROMno) & (pc.code.get_size(pc.su_bytes, l) = r.type.len)))
  THEN
    convert(r,l);
  ELSIF (l.mode = pc.ty_pointer)
      & (r.type.mode = pc.ty_pointer)
      & ((l.flag IN RELAXED) OR (r.type.flag IN RELAXED))
      & p_base_compatibility(l,r)
  THEN (* nothing *)
<* IF MCS THEN *>
  ELSIF pc.TY_SET{pc.ty_real,pc.ty_longreal} = pc.TY_SET{l.mode,r.type.mode} THEN
    convert(r,l);
<* END *>
  ELSE
    type_incomp(ps,l,r.type);
    convert(r,l);
  END;
END assign_compatibility;

PROCEDURE sys_parameter_compatible(f,a: pc.STRUCT): BOOLEAN;
  VAR sz: LONGINT;
BEGIN
  sz:=pc.code.get_size(pc.su_size,a);
  RETURN
    (f.mode=pc.ty_loc) & (sz=1) & (a.mode # pc.ty_SS) OR
    (f.mode=pc.ty_array) & (f.base.mode=pc.ty_loc) & (f.len=sz) OR
    (f.mode=pc.ty_array_of) & (f.base.mode=pc.ty_loc) &
        ~ (a.mode IN pc.NUM_LITERALs+pc.TY_SET{pc.ty_AA}) OR
    (f.mode=pc.ty_array_of) &
        (f.base.mode=pc.ty_array) &
        (f.base.base.mode=pc.ty_loc) &
        ((sz>=0) & (sz MOD f.base.len = 0) OR (f.base.len=1)) OR
    pcS.ext() & (f.mode=pc.ty_pointer) & (f.base.mode=pc.ty_loc) &
        (a.mode=pc.ty_pointer);
END sys_parameter_compatible;

PROCEDURE m2_var_parameter_compatible(f,a: pc.STRUCT): BOOLEAN;
  VAR t1,t2: pc.STRUCT;
BEGIN
  t1:=f; t2:=a;
  IF t1.obj#NIL THEN t1:=t1.obj.type END;
  IF t2.obj#NIL THEN t2:=t2.obj.type END;
  RETURN
        (t1.mode=pcO.ty_invtype) OR
        (t2.mode=pcO.ty_invtype) OR
        (t1=t2) OR
        (f.mode=pc.ty_array_of) & (a.mode IN pc.ARRs) &
                m2_var_parameter_compatible(f.base,a.base) OR
        sys_parameter_compatible(f,a);
END m2_var_parameter_compatible;

PROCEDURE var_parameter_compatible(f,a: pc.STRUCT): BOOLEAN;
  VAR t1,t2: pc.STRUCT;
BEGIN
  t1:=f; t2:=a;
  WHILE (t1.mode=pc.ty_opaque) & (t1.base#NIL) DO t1:=t1.base END;
  WHILE (t2.mode=pc.ty_opaque) & (t2.base#NIL) DO t2:=t2.base END;
  IF t1.obj#NIL THEN t1:=t1.obj.type END;
  IF t2.obj#NIL THEN t2:=t2.obj.type END;
  IF (t1.mode=pcO.ty_invtype) OR (t2.mode=pcO.ty_invtype) THEN
    RETURN TRUE;
  ELSIF (t1=t2) THEN
    RETURN TRUE;
  ELSIF (t1.mode=t2.mode) AND (t1.mode IN pc.NUMs) AND ((t1.mno < pc.ZEROMno) OR (t2.mno < pc.ZEROMno)) THEN
    RETURN TRUE;
  ELSIF (t1=pcO.addr) & (t2.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) THEN
    RETURN TRUE;
  ELSIF (f.mode=pc.ty_array_of) THEN
    IF (a.mode IN pc.ARRs) & m2_var_parameter_compatible(f.base,a.base) THEN
      RETURN TRUE;
    ELSIF (pcS.ext() OR pcS.TS_ext()) & (a = f.base) THEN
      RETURN TRUE;
    ELSIF (a.mode=pc.ty_pointer) & (f.flag IN RELAXED) & var_parameter_compatible(f.base,a.base) THEN
      RETURN TRUE;
    END;
  END;
  RETURN sys_parameter_compatible(f,a);
END var_parameter_compatible;

PROCEDURE val_parameter_compatible(f,a: pc.STRUCT): BOOLEAN;
BEGIN
  IF assign_compatible(f,a) OR sys_parameter_compatible(f,a) THEN
    RETURN TRUE;
  ELSIF (f.mode = pc.ty_array_of) THEN
    IF  (a.mode IN pc.ARRs) & var_parameter_compatible(f.base,a.base) THEN
      RETURN TRUE;
    ELSIF (a.mode=pc.ty_pointer) & (f.flag IN RELAXED) & var_parameter_compatible(f.base,a.base) THEN
      RETURN TRUE;
    ELSIF (pcS.ext() OR pcS.TS_ext()) & (a = f.base) THEN
      RETURN TRUE
    END;
  END;
  RETURN FALSE;
END val_parameter_compatible;

PROCEDURE c_arr_compatibility(t: pc.STRUCT; VAR n: pc.NODE): BOOLEAN;
(* relaxed compatibility for "C" procedures *)
  VAR b,e: pc.STRUCT; x: pc.NODE;
BEGIN
  e:=n.type;
  ASSERT(t.mode=pc.ty_pointer);
  IF (t.base.mode=pc.ty_char) & (e.mode=pc.ty_SS) THEN
    (* nothing *)
  ELSIF e.mode IN pc.TY_SET{pc.ty_array,pc.ty_array_of} THEN
    b:=e;
    WHILE b.mode=e.mode DO b:=b.base END;
    IF ~ var_parameter_compatible(t.base,b) THEN RETURN FALSE END;
  ELSIF (e.mode=pc.ty_pointer) &
        (e.base.mode IN pc.TY_SET{pc.ty_array,pc.ty_array_of})
  THEN
    b:=e.base;
    WHILE b.mode=e.base.mode DO b:=b.base END;
    IF ~ var_parameter_compatible(t.base,b) THEN RETURN FALSE END;
    IF e.base.mode=pc.ty_array_of THEN
      pcO.new(x,pc.nd_deref);
      IF env.nil_check IN env.config.tags THEN
        INCL(x.tags,pc.ntag_chk_range)
      END;
      x.type:=e.base;
      x.l:=n;
      n:=x;
    ELSE
      RETURN TRUE;
    END;
  ELSE
    RETURN FALSE;
  END;
  unary(n,pc.su_adr,pcO.addr);
  RETURN TRUE;
END c_arr_compatibility;

PROCEDURE p_rec_compatibility(t: pc.STRUCT; VAR n: pc.NODE): BOOLEAN;
(* relaxed compatibility for "C" procedures *)
  VAR e: pc.STRUCT;
BEGIN
  e:=n.type;
  ASSERT(t.mode=pc.ty_pointer);
  IF e.mode#pc.ty_record THEN RETURN FALSE END;
  IF t.base.mode#pc.ty_record THEN RETURN FALSE END;
  IF t.base#e THEN RETURN FALSE END;
  unary(n,pc.su_adr,pcO.addr);
  RETURN TRUE;
END p_rec_compatibility;

PROCEDURE parameter_compatible(p: pc.OBJECT; VAR n: pc.NODE): BOOLEAN;
BEGIN
  IF p.mode=pc.ob_var THEN
    IF pcO.otag_varpar_nil IN p.tags THEN
      IF var_parameter_compatible(p.type.base,n.type) THEN
        chk_lvalue(n);
        unary(n,pc.su_adr,pcO.addr);
        convert(n,p.type);
        RETURN TRUE
      ELSIF n.type.mode = pc.ty_AA THEN RETURN TRUE
      END;
      RETURN FALSE
    ELSIF val_parameter_compatible(p.type,n.type) THEN
      IF pcS.TS_ext() & (n.type.mode=pc.ty_SS) & (p.type.base.mode=pc.ty_char) &
         (n.type.len=2)(*It's enough, since last always zero *) &
         (n.mode=pc.nd_value)  THEN
        env.errors.Warning(n.pos,325);
      END;
      chk_value(n); convert(n,p.type);
      RETURN TRUE
    ELSIF (p.type.mode = pc.ty_array_of) & (pcS.ext() OR pcS.TS_ext()) THEN
      IF ((n.type.mode = pc.ty_SS) AND (p.type.base.mode = pc.ty_array) AND (p.type.base.base.mode = pc.ty_char)) THEN
        chk_value(n); convert(n,p.type.base);
        RETURN TRUE
      ELSIF (n.type.mode = pc.ty_ZZ) & (p.type.base.mode IN pc.WHOLEs) THEN
        chk_value(n); convert(n,p.type.base); convert(n,p.type);
        RETURN TRUE;
      ELSIF (n.type.mode = pc.ty_RR) & (p.type.base.mode IN pc.REALs) THEN
        chk_value(n); convert(n,p.type.base); convert(n,p.type);
        RETURN TRUE;
      ELSIF (n.type.mode = pc.ty_CC) & (p.type.base.mode IN pc.CPLXs) THEN
        chk_value(n); convert(n,p.type.base); convert(n,p.type);
        RETURN TRUE;
      ELSIF (n.type.mode = pc.ty_boolean) & (p.type.base.mode=pc.ty_boolean) THEN
        chk_value(n); convert(n,p.type.base); convert(n,p.type);
        RETURN TRUE;
      ELSIF (n.type.mode = pc.ty_ZZ)& assign_compatible(p.type.base,n.type) THEN
        chk_value(n); convert(n,p.type.base); convert(n,p.type);
        RETURN TRUE;
      ELSIF (n.type.mode = pc.ty_proctype) & assign_compatible(p.type.base,n.type) THEN
        chk_value(n); convert(n,p.type.base); convert(n,p.type);
        RETURN TRUE;
      ELSIF (n.type.mode = pc.ty_AA) & assign_compatible(p.type.base,n.type) THEN
        chk_value(n); convert(n,p.type.base); convert(n,p.type);
        RETURN TRUE;
      END;
    END;
    IF pcS.oberon THEN
      IF type_extension(p.type,n.type) THEN
        chk_value(n); guard(n,p.type,FALSE); RETURN TRUE
      ELSIF o2_const_assign(p.type,n) THEN
        chk_value(n); convert(n,p.type); RETURN TRUE
      END;
    END;
    IF (p.type.mode = pc.ty_loc) & byte_const_assign(p.type,n) THEN
      IF ~ pcS.ext() THEN
        pcS.error(n.pos,'e',102,"(extended compatibility with byte)")
      END;
      convert(n,p.type); RETURN TRUE
    END;
    IF (p.type.mode=pc.ty_pointer) &
       (p.host.flag IN RELAXED) &
       (c_arr_compatibility(p.type,n) OR
        p_rec_compatibility(p.type,n) OR
        p_base_compatibility(p.type,n))
    THEN
      convert(n,p.type); RETURN TRUE
    END;
  ELSIF p.mode=pc.ob_varpar THEN
    IF var_parameter_compatible(p.type,n.type)
      & ~( (p.host.flag IN pc.LangsWithOpenArrays)AND
           (n.mode=pc.nd_deref) AND
           (p.type.mode=pc.ty_array_of)
            AND ~(n.l.type.flag IN pc.LangsWithOpenArrays) )
    THEN
      chk_lvalue(n); lconvert(n,p.type); RETURN TRUE
    END;
    IF  pcS.oberon
      & (p.type.mode=pc.ty_record)
      & type_extension(p.type,n.type)
    THEN
      chk_lvalue(n); guard(n,p.type,FALSE); RETURN TRUE
    END;
  ELSE ASSERT(FALSE)
  END;
  RETURN FALSE
END parameter_compatible;

PROCEDURE parameter_compatibility*(p: pc.OBJECT; VAR n: pc.NODE);
BEGIN
  IF parameter_compatible(p,n) THEN (* nothing *)
  ELSIF p.mode#pc.ob_inv THEN
    type_incomp(n.pos,p.type,n.type);
    chk_value(n);
    convert(n,p.type);
  END;
END parameter_compatibility;

PROCEDURE compatibility_with_byte*(VAR n: pc.NODE);
BEGIN
  IF val_parameter_compatible(pcO.loc,n.type) THEN
  ELSIF byte_const_assign(pcO.loc,n) THEN
    IF ~ pcS.ext() THEN
      pcS.error(n.pos,'e',102,"(extended compatibility with byte)")
    END;
  ELSE
    type_incomp(n.pos,pcO.loc,n.type);
  END;
  convert(n,pcO.loc);
END compatibility_with_byte;

PROCEDURE expression_compatible(t1,t2: pc.STRUCT; VAR t: pc.STRUCT): BOOLEAN;
  PROCEDURE lit_cmp(a,b: pc.STRUCT): BOOLEAN;
  BEGIN
    IF pcS.oberon THEN
      CASE a.mode OF
        |pc.ty_ZZ: RETURN b.mode IN pc.TY_SET{pc.ty_ZZ,pc.ty_RR,pc.ty_CC};
        |pc.ty_RR: RETURN b.mode IN pc.TY_SET{pc.ty_RR,pc.ty_CC};
        |pc.ty_CC: RETURN b.mode IN pc.TY_SET{pc.ty_CC};
      END;
    ELSIF pcS.ext() THEN
      CASE a.mode OF
        |pc.ty_ZZ: RETURN b.mode IN (pc.WHOLEs+pc.REALs+pc.CPLXs);
        |pc.ty_RR: RETURN b.mode IN (pc.REALs+pc.CPLXs);
        |pc.ty_CC: RETURN b.mode IN pc.CPLXs;
      END;
    ELSE
      CASE a.mode OF
        |pc.ty_ZZ: RETURN b.mode IN pc.WHOLEs;
        |pc.ty_RR: RETURN b.mode IN pc.REALs;
        |pc.ty_CC: RETURN b.mode IN pc.CPLXs;
      END;
    END;
  END lit_cmp;
  PROCEDURE max_num_type(a,b: pc.STRUCT; VAR res: pc.STRUCT): BOOLEAN;
    CONST NUMs=pc.CNUMs-pc.TY_SET{pc.ty_ZZ,pc.ty_RR,pc.ty_CC};
  BEGIN
    res:=NIL;
    IF ~(a.mode IN NUMs) THEN RETURN FALSE END;
    IF ~(b.mode IN NUMs) THEN RETURN FALSE END;
    IF ~pcS.oberon THEN
      IF (a.mode IN pc.WHOLEs)#(b.mode IN pc.WHOLEs) THEN RETURN FALSE END;
      IF (a.mode IN pc.REALs )#(b.mode IN pc.REALs ) THEN RETURN FALSE END;
      IF (a.mode IN pc.CPLXs )#(b.mode IN pc.CPLXs ) THEN RETURN FALSE END;
    END;
    CASE a.mode OF
      |pc.ty_shortcard:
        IF    b.mode=pc.ty_shortint  THEN res:=pcO.integer;
        ELSE  res:=b;
        END;
      |pc.ty_cardinal:
        IF    b.mode=pc.ty_shortint  THEN res:=pcO.longint;
        ELSIF b.mode=pc.ty_integer   THEN res:=pcO.longint;
        ELSIF b.mode=pc.ty_shortcard THEN res:=pcO.cardinal;
        ELSE  res:=b;
        END;
      |pc.ty_longcard:
        IF    b.mode=pc.ty_shortint  THEN RETURN FALSE;
        ELSIF b.mode=pc.ty_integer   THEN RETURN FALSE;
        ELSIF b.mode=pc.ty_longint   THEN RETURN FALSE;
        ELSIF b.mode=pc.ty_shortcard THEN res:=pcO.longcard;
        ELSIF b.mode=pc.ty_cardinal  THEN res:=pcO.longcard;
        ELSE  res:=b;
        END;
      |pc.ty_shortint:
        IF    b.mode=pc.ty_shortcard THEN res:=pcO.integer;
        ELSIF b.mode=pc.ty_cardinal  THEN res:=pcO.longint;
        ELSIF b.mode=pc.ty_longcard  THEN RETURN FALSE;
        ELSE  res:=b;
        END;
      |pc.ty_integer:
        IF    b.mode=pc.ty_shortint  THEN res:=pcO.integer;
        ELSIF b.mode=pc.ty_shortcard THEN res:=pcO.integer;
        ELSIF b.mode=pc.ty_cardinal  THEN res:=pcO.longint;
        ELSIF b.mode=pc.ty_longcard  THEN RETURN FALSE;
        ELSE  res:=b;
        END;
      |pc.ty_longint:
        IF    b.mode=pc.ty_shortint  THEN res:=pcO.longint;
        ELSIF b.mode=pc.ty_integer   THEN res:=pcO.longint;
        ELSIF b.mode=pc.ty_longint   THEN res:=pcO.longint;
        ELSIF b.mode=pc.ty_shortcard THEN res:=pcO.longint;
        ELSIF b.mode=pc.ty_cardinal  THEN res:=pcO.longint;
        ELSIF b.mode=pc.ty_longcard  THEN RETURN FALSE;
        ELSE  res:=b;
        END;
      |pc.ty_longlongint:
        CASE b.mode OF
        | pc.ty_shortint,
          pc.ty_integer,
          pc.ty_longint,
          pc.ty_shortcard,
          pc.ty_longcard,
          pc.ty_cardinal:

            res:=pcO.longlongint;
        | pc.ty_longlongcard:
            RETURN FALSE;
        ELSE
          res:=b;
        END;
      |pc.ty_longlongcard:
        CASE b.mode OF
        |
          pc.ty_shortcard,
          pc.ty_cardinal,
          pc.ty_longcard:

            res:=pcO.longlongcard;
        | pc.ty_longlongint:
            RETURN FALSE;
        ELSE
          res:=b;
        END;
      |pc.ty_real:
        IF    b.mode IN pc.WHOLEs    THEN res:=a;
        ELSE  res:=b;
        END;
      |pc.ty_longreal:
        IF    b.mode IN pc.WHOLEs    THEN res:=a;
        ELSIF b.mode=pc.ty_real      THEN res:=pcO.longreal;
        ELSIF b.mode=pc.ty_complex   THEN res:=pcO.lcomplex;
        ELSE  res:=b;
        END;
      |pc.ty_ld_real:
        IF    b.mode=pc.ty_complex   THEN RETURN FALSE;
        ELSIF b.mode=pc.ty_lcomplex  THEN RETURN FALSE;
        ELSE  res:=a;
        END;
      |pc.ty_complex:
        IF    b.mode IN pc.WHOLEs    THEN res:=a;
        ELSIF b.mode=pc.ty_real      THEN res:=pcO.complex;
        ELSIF b.mode=pc.ty_longreal  THEN res:=pcO.lcomplex;
        ELSIF b.mode=pc.ty_ld_real   THEN RETURN FALSE;
        ELSE  res:=b;
        END;
      |pc.ty_lcomplex:
        IF    b.mode=pc.ty_ld_real   THEN RETURN FALSE;
        ELSE  res:=a;
        END;
    END;
    RETURN TRUE;
  END max_num_type;
  CONST
    CARDs = pc.CARDs-pc.TY_SET{pc.ty_ZZ};
    INTs  = pc.INTs -pc.TY_SET{pc.ty_ZZ};
    LITs  = pc.TY_SET{pc.ty_ZZ,pc.ty_RR,pc.ty_CC};
  VAR tt: pc.STRUCT;
BEGIN
  ASSERT(max_num_type(t1,t2,t)=max_num_type(t2,t1,tt));
  ASSERT((t=NIL) & (tt=NIL) OR (t.mode=tt.mode));
  t:=NIL;
  WHILE (t1.mode=pc.ty_opaque) & (t1.base#NIL) DO t1:=t1.base END;
  WHILE t1.mode=pc.ty_range DO t1:=t1.base END;
  IF t1.obj#NIL THEN t1:=t1.obj.type END;
  WHILE (t2.mode=pc.ty_opaque) & (t2.base#NIL) DO t2:=t2.base END;
  WHILE t2.mode=pc.ty_range DO t2:=t2.base END;
  IF t2.obj#NIL THEN t2:=t2.obj.type END;

  IF    t1.mode=pcO.ty_invtype THEN t:=t1;
  ELSIF t2.mode=pcO.ty_invtype THEN t:=t2;
  ELSIF t1=t2 THEN t:=t1;
  ELSIF (t1.mode IN LITs) & lit_cmp(t1,t2) THEN t:=t2;
  ELSIF (t2.mode IN LITs) & lit_cmp(t2,t1) THEN t:=t1;
  ELSIF (pcS.oberon OR pcS.ext()) & max_num_type(t1,t2,t) THEN (* nothing *)
  ELSIF (t2.mode=pc.ty_char) & is_char(t1) THEN t:=t2;
  ELSIF (t1.mode=pc.ty_char) & is_char(t2) THEN t:=t1;
  ELSIF (t1.mode=pc.ty_AA) & (t2.mode IN pc.ADRs) THEN t:=t2;
  ELSIF (t2.mode=pc.ty_AA) & (t1.mode IN pc.ADRs) THEN t:=t1;
  ELSIF (pcS.ext() OR pcS.TS_ext()) & (t1.mode=pc.ty_AA) & (t2.mode = pc.ty_opaque) THEN t:=t2;
  ELSIF (pcS.ext() OR pcS.TS_ext()) & (t2.mode=pc.ty_AA) & (t1.mode = pc.ty_opaque) THEN t:=t1;
  ELSIF (t1.mode IN CARDs) & (t2.mode IN CARDs) THEN
    IF t1.mode>t2.mode THEN t:=t1 ELSE t:=t2 END;
  ELSIF (t1.mode IN INTs) & (t2.mode IN INTs) THEN
    IF t1.mode>t2.mode THEN t:=t1 ELSE t:=t2 END;
  ELSIF (t1.mode=pc.ty_pointer) & type_extension(t1,t2) THEN t:=t1;
  ELSIF (t2.mode=pc.ty_pointer) & type_extension(t2,t1) THEN t:=t2;
  ELSIF (t1.mode=pc.ty_boolean) & (t2.mode=pc.ty_boolean) THEN
    t:=pcO.boolean; (* for BOOL8, BOOL32 compatibility *)
<* IF MCS THEN *>
  ELSIF
    ((t1.mode=pc.ty_longreal) & (t2.mode=pc.ty_real))
  THEN
    t:=t1;
  ELSIF
    ((t2.mode=pc.ty_longreal) & (t1.mode=pc.ty_real))
  THEN
    t:=t2;
  ELSIF
    ((t1.mode=pc.ty_shortcard)& (t2.mode=pc.ty_shortint)) OR
    ((t1.mode=pc.ty_cardinal) & (t2.mode=pc.ty_integer))  OR
    ((t1.mode=pc.ty_longcard) & (t2.mode=pc.ty_longint))
  THEN
    t:=t1;
    INCL(t.tags,ttag_sig_unsig_compat);
    pcS.warn(333);
  ELSIF
    ((t2.mode=pc.ty_shortcard)& (t1.mode=pc.ty_shortint)) OR
    ((t2.mode=pc.ty_cardinal) & (t1.mode=pc.ty_integer))  OR
    ((t2.mode=pc.ty_longcard) & (t1.mode=pc.ty_longint))
  THEN
    t:=t2;
    INCL(t.tags,ttag_sig_unsig_compat);
    pcS.warn(333);
<* END *>
  ELSE RETURN FALSE;
  END;
  ASSERT(t#NIL);
  IF pcS.oberon THEN RETURN TRUE END;
  tt:=t;
  IF t.mode IN pc.TY_SET{pc.ty_shortint,pc.ty_integer} THEN t:=pcO.m2_int;
  ELSIF t.mode IN pc.TY_SET{pc.ty_shortcard,pc.ty_cardinal} THEN t:=pcO.m2_card;
  END;
  IF ttag_sig_unsig_compat IN tt.tags THEN
    INCL(t.tags, ttag_sig_unsig_compat);
  END;
  RETURN TRUE;
END expression_compatible;

PROCEDURE o2_const_compatible(o1,o2: pc.NODE; VAR t: pc.STRUCT): BOOLEAN;
  PROCEDURE num_type(a: pc.STRUCT): pc.STRUCT;
  BEGIN
    IF a.mode IN pc.CPLXs  THEN RETURN pcO.CC_type END;
    IF a.mode IN pc.REALs  THEN RETURN pcO.RR_type END;
    ASSERT(a.mode IN pc.WHOLEs);
    RETURN pcO.ZZ_type;
  END num_type;
  VAR t1,t2: pc.STRUCT;

BEGIN
  t:=NIL;
  t1:=host_type(o1.type);
  t2:=host_type(o2.type);
  IF ~ (t1.mode IN (pc.WHOLEs+pc.REALs+pc.CPLXs)) THEN RETURN FALSE END;
  IF ~ (t2.mode IN (pc.WHOLEs+pc.REALs+pc.CPLXs)) THEN RETURN FALSE END;
  IF env.errors.err_cnt>0 THEN t:=pcO.invtype; RETURN TRUE END;
  eval_value(o1);
  eval_value(o2);
  IF (o1.mode=pc.nd_value) & (o2.mode=pc.nd_value) THEN
    o1.type:=num_type(t1);
    o2.type:=num_type(t2);
  ELSIF o1.mode=pc.nd_value THEN
    o1.type:=lit_value_type(o1.val,t1,t2);

  ELSIF o2.mode=pc.nd_value THEN
    o2.type:=lit_value_type(o2.val,t2,t1);
  ELSE RETURN FALSE
  END;
  RETURN expression_compatible(o1.type,o2.type,t);
END o2_const_compatible;

PROCEDURE o2_const_compatible_2(t: pc.STRUCT; o: pc.NODE; VAR tt: pc.STRUCT): BOOLEAN;
  VAR t1,t2: pc.STRUCT;
BEGIN
  tt:=NIL;
  t1:=host_type(t);
  t2:=host_type(o.type);
  IF ~ (t1.mode IN (pc.WHOLEs+pc.REALs+pc.CPLXs)) THEN RETURN FALSE END;
  IF ~ (t2.mode IN (pc.WHOLEs+pc.REALs+pc.CPLXs)) THEN RETURN FALSE END;
  IF env.errors.err_cnt>0 THEN tt:=pcO.invtype; RETURN TRUE END;
    eval_value(o);
  IF o.mode=pc.nd_value THEN
    o.type:=lit_value_type(o.val,t2,t1);
  ELSE RETURN FALSE
  END;
  RETURN expression_compatible(t,o.type,tt);
END o2_const_compatible_2;

PROCEDURE expression_compatibility*(ps: pc.TPOS; VAR o1,o2: pc.NODE);
  VAR t: pc.STRUCT;

  PROCEDURE isZZsizeCorrect (o: pc.NODE; t: pc.STRUCT):BOOLEAN;
  -- KMM-454 fix
    VAR  val: pc.VALUE;
  BEGIN
    IF (o.type.mode=pc.ty_ZZ) THEN
      IF o.mode = pc.nd_var THEN
        ASSERT(o.obj.mode=pc.ob_cons);
        IF o.obj.val.mode#pc.nd_value THEN
          RETURN TRUE;
        END;
        val:=o.obj.val.val;
      ELSE
        val:=o.val;
      END;
      IF (val#NIL) AND (t.mode IN pc.WHOLEs) AND
         ~(cmp_value(pc.sb_leq,t.min,val) &
           cmp_value(pc.sb_geq,t.max,val)) THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END isZZsizeCorrect;

BEGIN
  chk_value(o1);
  chk_value(o2);
  IF ~ expression_compatible(o1.type,o2.type,t) THEN
    IF pcS.oberon & o2_const_compatible(o1,o2,t) THEN
      (* nothing *)
    ELSE
      type_incomp(ps,o1.type,o2.type);
      t:=pcO.invtype;
    END;
  END;
  IF o1.type#t THEN
    IF ~isZZsizeCorrect(o1,t) THEN
      type_incomp(ps,o1.type,o2.type);
    END;
    convert(o1,t);
    IF ttag_sig_unsig_compat IN t.tags THEN
--      INCL(o1.tags,pc.ntag_chk_overflow);  -- MicroWare feature
      EXCL(o1.tags,pc.ntag_chk_range);
    END;
  END;
  IF o2.type#t THEN
    IF ~isZZsizeCorrect(o2,t) THEN
      type_incomp(ps,o1.type,o2.type);
    END;
    convert(o2,t);
    IF ttag_sig_unsig_compat IN t.tags THEN
--      INCL(o2.tags,pc.ntag_chk_overflow);  -- MicroWare feature
      EXCL(o2.tags,pc.ntag_chk_range);
    END;
  END;

END expression_compatibility;

PROCEDURE expression_compatibility_2*(ps: pc.TPOS; t: pc.STRUCT; VAR o: pc.NODE);
  VAR tt: pc.STRUCT;
BEGIN
  chk_value(o);
  IF ~ expression_compatible(t,o.type,tt) THEN
    IF pcS.oberon & o2_const_compatible_2(t,o,tt) THEN
      (* nothng *)
    ELSE
      type_incomp(ps,t,o.type);
    END;
  END;
  IF o.type#t THEN convert(o,t) END;
END expression_compatibility_2;

PROCEDURE is_structurally_identical(t1,t2: pc.STRUCT): BOOLEAN;
BEGIN
  IF t1.obj#NIL THEN t1:=t1.obj.type END;
  IF t2.obj#NIL THEN t2:=t2.obj.type END;
  IF ((t1.obj=NIL) OR (t1.obj.mode=pc.ob_type)) &
     ((t2.obj=NIL) OR (t2.obj.mode=pc.ob_type))
  THEN
    RETURN t1=t2;
  ELSE
    RETURN proc_types_cmp(t1,t2,FALSE);
  END;
END is_structurally_identical;

PROCEDURE Char2SS(n:pc.NODE):pc.NODE;
VAR ty:pc.STRUCT; v:pc.VALUE;
BEGIN
   ASSERT((n.mode=pc.nd_value)&(n.type.mode=pc.ty_char));
   v:=pcS.char2SS(n.val,n.pos);
   ty := pcO.new_type(pc.ty_SS);
   zz_tmp.unary(pc.su_length,v);
   ty.len:=zz_tmp.get_integer()+1;
   n.type:=ty;
   n.val:=v;
   RETURN n;
END Char2SS;

PROCEDURE gen_binary_operator*(ps: pc.TPOS; VAR e1: pc.NODE;
                    e2: pc.NODE; op: pc.SUB_MODE);
(** +,-,*,/,&,OR,=,#,<,>,<=,>=,** *)
  PROCEDURE IsCharConstant(n:pc.NODE):BOOLEAN;
  BEGIN RETURN (n.mode=pc.nd_value)&(n.type.mode=pc.ty_char)
  END IsCharConstant;

  VAR m: pc.TY_MODE; t: pc.STRUCT;
BEGIN
  CASE op OF
    |pc.sb_exp:
       chk_value(e1); chk_value(e2);
       IF e1.type.mode=pcO.ty_invtype THEN RETURN END;
       IF e2.type.mode=pcO.ty_invtype THEN RETURN END;
       IF e1.type.mode IN pc.WHOLEs THEN convert(e1,pcO.real);
       ELSIF e1.type.mode=pc.ty_RR THEN convert(e1,pcO.longreal);
       ELSIF e1.type.mode=pc.ty_CC THEN convert(e1,pcO.lcomplex);
       END;
       IF e2.type.mode IN pc.WHOLEs THEN convert(e2,pcO.longint);
       ELSIF e2.type.mode IN pc.REALs THEN convert(e2,pcO.longreal);
       END;
       IF (e1.type.mode IN pc.CNUMs) &
          (e2.type.mode IN pc.TY_SET{pc.ty_longint,pc.ty_longreal})
       THEN binary(ps,e1,e2,pc.sb_exp,e1.type); weak(e1);
       ELSE pcS.error(ps,'e',incomp); e1.type:=pcO.invtype;
       END;
    |pc.sb_slash:
       expression_compatibility(ps,e1,e2);
       t:=e1.type; m:=t.mode;
       IF m=pcO.ty_invtype THEN RETURN END;
       IF pcS.oberon & (m IN pc.WHOLEs) THEN
         convert(e1,pcO.real); convert(e2,pcO.real);
         t:=pcO.real; m:=t.mode;
       END;
       IF m IN pc.SETs THEN binary(ps,e1,e2,pc.sb_xor,t);
       ELSIF m IN pc.CNUMs THEN binary(ps,e1,e2,pc.sb_slash,t); weak(e1);
       ELSE pcS.error(ps,'e',incomp); e1.type:=pcO.invtype;
       END;
    |pc.sb_rem:
       expression_compatibility(ps,e1,e2);
       t:=e1.type; m:=t.mode;
       IF m=pcO.ty_invtype THEN RETURN END;
       IF m IN pc.WHOLEs THEN binary(ps,e1,e2,op,t); weak(e1);
       ELSE pcS.error(ps,'e',incomp); e1.type:=pcO.invtype;
       END;
    |pc.sb_mul:
       expression_compatibility(ps,e1,e2);
       t:=e1.type; m:=t.mode;
       IF m=pcO.ty_invtype THEN RETURN END;
       IF m IN pc.SETs THEN binary(ps,e1,e2,pc.sb_and,t);
       ELSIF m IN pc.CNUMs THEN binary(ps,e1,e2,pc.sb_mul,t); weak(e1);
       ELSE pcS.error(ps,'e',incomp); e1.type:=pcO.invtype;
       END;
    |pc.sb_div:
       expression_compatibility(ps,e1,e2);
       t:=e1.type; m:=t.mode;
       IF m=pcO.ty_invtype THEN RETURN END;
       IF m IN pc.WHOLEs THEN
         binary(ps,e1,e2,pc.sb_div,t); weak(e1);
         IF env.quo_check IN env.config.tags THEN
           INCL(e1.tags,pc.ntag_chk_range);
         END;
       ELSE pcS.error(ps,'e',incomp); e1.type:=pcO.invtype;
       END;
    |pc.sb_mod:
       expression_compatibility(ps,e1,e2);
       t:=e1.type; m:=t.mode;
       IF m=pcO.ty_invtype THEN RETURN END;
       IF m IN pc.WHOLEs THEN
         binary(ps,e1,e2,op,t); weak(e1);
         IF env.quo_check IN env.config.tags THEN
           INCL(e1.tags,pc.ntag_chk_range);
         END;
       ELSE pcS.error(ps,'e',incomp); e1.type:=pcO.invtype;
       END;
    |pc.sb_minus:
       IF pcS.ext() & (e1.type.mode IN pc.ADRs) THEN
         IF e2.type.mode IN pc.ADRs THEN
           chk_value(e1); chk_value(e2);
           binary(ps,e1,e2,pc.sb_difadr,pcO.difadr);
         ELSE
           chk_value(e1); chk_value(e2);
           type_in(e2.type,pc.WHOLEs);
           convert(e2,pcO.difadr);
           binary(ps,e1,e2,pc.sb_subadr,e1.type);
           weak(e1);
         END;
       ELSE
         expression_compatibility(ps,e1,e2);
         t:=e1.type; m:=t.mode;
         IF m=pcO.ty_invtype THEN RETURN END;
         IF m IN pc.SETs THEN binary(ps,e1,e2,pc.sb_bic,t);
         ELSIF m IN pc.CNUMs THEN binary(ps,e1,e2,pc.sb_minus,t); weak(e1);
         ELSE pcS.error(ps,'e',incomp); e1.type:=pcO.invtype;
         END;
       END;
    |pc.sb_plus:
       IF pcS.ext() & (e1.type.mode IN pc.ADRs) THEN
         chk_value(e1); chk_value(e2);
         type_in(e2.type,pc.WHOLEs);
         convert(e2,pcO.difadr);
         binary(ps,e1,e2,pc.sb_addadr,e1.type);
         weak(e1);
       ELSIF (e1.type.mode=pc.ty_SS) & (e2.type.mode=pc.ty_SS) THEN
         IF pcS.oberon & ~ pcS.ext() THEN
           pcS.error(ps,'e',102,"(string concatenation)");
         END;
         t := pcO.new_type(pc.ty_SS);
         t.len:=e1.type.len+e2.type.len-1;
         binary(ps,e1,e2,pc.sb_concat,t);
       ELSIF ((e1.type.mode=pc.ty_SS) & IsCharConstant(e2)) OR
             (IsCharConstant(e1) & (e2.type.mode=pc.ty_SS)) THEN
         IF pcS.oberon & ~ pcS.ext() THEN
           pcS.error(ps,'e',102,"(string concatenation)");
         END;
         IF IsCharConstant(e1) THEN e1:=Char2SS(e1); END;
         IF IsCharConstant(e2) THEN e2:=Char2SS(e2); END;
         t := pcO.new_type(pc.ty_SS);
         t.len:=e1.type.len+e2.type.len-1;
         binary(ps,e1,e2,pc.sb_concat,t);
       ELSE
         expression_compatibility(ps,e1,e2);
         t:=e1.type; m:=t.mode;
         IF m=pcO.ty_invtype THEN RETURN END;
         IF m IN pc.SETs THEN binary(ps,e1,e2,pc.sb_or,t);
         ELSIF m IN pc.CNUMs THEN binary(ps,e1,e2,pc.sb_plus,t); weak(e1);
         ELSE pcS.error(ps,'e',incomp); e1.type:=pcO.invtype;
         END;
       END;
    |pc.sb_shr, pc.sb_shl, pc.sb_sar:
      t:=e1.type; m:=t.mode;
      IF m=pcO.ty_invtype THEN RETURN END;
      IF (m IN pc.WHOLEs) AND (e2.type.mode IN pc.WHOLEs) THEN
        IF (e2.type.mode IN pc.INTs) THEN
          convert(e2,pcO.shortcard);
        END;
        binary(ps,e1,e2,op,t);
      ELSE
        pcS.error(ps,'e',incomp); e1.type:=pcO.invtype;
      END;

    |pc.sb_cand, pc.sb_cor:
       chk_value(e1); chk_value(e2);
       IF (pcS.TS_ext() OR pcS.ext()) AND (e1.type.mode IN pc.WHOLEs) THEN
         expression_compatibility(ps,e1,e2);
         CASE op OF
         | pc.sb_cand:
           binary(ps, e1, e2, pc.sb_and, e1.type);
         | pc.sb_cor:
           binary(ps, e1, e2, pc.sb_or,  e1.type);
         END;
       ELSE
         type_equ(e1.type, pc.ty_boolean);
         type_equ(e2.type, pc.ty_boolean);
         binary(ps, e1, e2, op, pcO.boolean);
       END;

    |pc.sb_equ,pc.sb_neq:
      IF (e1.type.mode=pc.ty_proctype) OR
         (e2.type.mode=pc.ty_proctype)
      THEN
        IF e1.type.mode=e2.type.mode THEN
          IF ~ is_structurally_identical(e1.type,e2.type) THEN
            type_incomp(ps,e1.type,e2.type);
            e1.type:=pcO.invtype;
          ELSE
            chk_value(e1); chk_value(e2);
            binary(ps,e1,e2,op,pcO.boolean);
          END;
        ELSIF (e1.type.mode=pc.ty_AA) OR (e2.type.mode=pc.ty_AA) THEN
          IF ~ pcS.ext() & ~ pcS.oberon THEN
            pcS.error(ps,'e',102,"(procedure comparision with NIL)")
          END;
          chk_value(e1); chk_value(e2);
          binary(ps,e1,e2,op,pcO.boolean);
        ELSE
          pcS.error(ps,'e',incomp);
          e1.type:=pcO.invtype;
        END;
      ELSIF (e1.type.mode IN (pc.ADRs+pc.TY_SET{pc.ty_opaque})) OR
            (e2.type.mode IN (pc.ADRs+pc.TY_SET{pc.ty_opaque}))
      THEN
        expression_compatibility(ps,e1,e2);
        binary(ps,e1,e2,op,pcO.boolean);
      ELSIF (e1.type.mode IN pc.ARRs) & is_char(e1.type.base) &
            (e2.type.mode IN pc.ARRs) & is_char(e2.type.base)
      THEN
        IF ~ pcS.ext() & ~ pcS.oberon THEN
          pcS.error(ps,'e',102,"(string comparision)")
        END;
        chk_value(e1); chk_value(e2);
        binary(ps,e1,e2,op,pcO.boolean);
      ELSE
        expression_compatibility(ps,e1,e2);
        t:=e1.type; m:=t.mode;
        IF m=pcO.ty_invtype THEN RETURN END;
        IF is_scalar(t) OR
           (m IN (pc.CPLXs+pc.SETs+pc.TY_SET{pc.ty_protection})) OR (m=pc.ty_loc)
        THEN binary(ps,e1,e2,op,pcO.boolean);
        ELSE pcS.error(ps,'e',incomp); e1.type:=pcO.invtype;
        END;
      END;
    |pc.sb_lss,pc.sb_gtr:
      IF (e1.type.mode IN pc.ARRs) & is_char(e1.type.base) &
         (e2.type.mode IN pc.ARRs) & is_char(e2.type.base)
      THEN
        IF ~ pcS.ext() & ~ pcS.oberon THEN
          pcS.error(ps,'e',102,"(string comparision)")
        END;
        chk_value(e1); chk_value(e2);
        binary(ps,e1,e2,op,pcO.boolean);
      ELSE
        expression_compatibility(ps,e1,e2);
        t:=e1.type; m:=t.mode;
        IF m=pcO.ty_invtype THEN RETURN END;
        IF is_scalar(t) THEN binary(ps,e1,e2,op,pcO.boolean);
        ELSE pcS.error(ps,'e',incomp); e1.type:=pcO.invtype;
        END;
      END;
    |pc.sb_leq,pc.sb_geq:
      IF (e1.type.mode IN pc.ARRs) & is_char(e1.type.base) &
         (e2.type.mode IN pc.ARRs) & is_char(e2.type.base)
      THEN
        IF ~ pcS.ext() & ~ pcS.oberon THEN
          pcS.error(ps,'e',102,"(string comparision)")
        END;
        chk_value(e1); chk_value(e2);
        binary(ps,e1,e2,op,pcO.boolean);
      ELSE
        expression_compatibility(ps,e1,e2);
        ASSERT(e1.type.mode=e2.type.mode);
        t:=e1.type; m:=t.mode;
        IF m=pcO.ty_invtype THEN RETURN END;
        IF is_scalar(t) OR
           (m IN pc.SETs) & ~ pcS.oberon OR
           (m=pc.ty_protection)
        THEN binary(ps,e1,e2,op,pcO.boolean);
        ELSE pcS.error(ps,'e',incomp); e1.type:=pcO.invtype;
        END;
      END;
  END;
END gen_binary_operator;

PROCEDURE gen_unary_operator*(ps: pc.TPOS; VAR e: pc.NODE; op: pc.SUB_MODE);
(** +, -, ~ *)
BEGIN
  chk_value(e);
  CASE op OF
    |pc.sb_plus:
      IF e.type.mode=pc.ty_range THEN convert(e,e.type.base) END;
      IF e.type.mode IN (pc.INTs+pc.CARDs+pc.REALs+pc.CPLXs) THEN
        (* ok *)
      ELSIF (pcS.oberon OR pcS.ext()) & (e.type.mode IN pc.SETs) THEN
        (* ok *)
      ELSIF e.type.mode#pcO.ty_invtype THEN
        pcS.error(ps,'e',incomp);
        e.type:=pcO.invtype;
      END;
    |pc.su_neg :
      IF e.type.mode=pc.ty_range THEN convert(e,e.type.base) END;
--      IF e.type.mode=pc.ty_longlongint THEN pcS.error(ps,'e',200) END;
      IF e.type.mode IN (pc.INTs+pc.REALs+pc.CPLXs) THEN
        unary(e,pc.su_neg,e.type); weak(e);
      ELSIF (pcS.oberon OR pcS.ext()) & (e.type.mode IN pc.SETs) THEN
        unary(e,pc.su_compl,e.type);
      ELSIF e.type.mode#pcO.ty_invtype THEN
        pcS.error(ps,'e',incomp);
        e.type:=pcO.invtype;
      END;
    |pc.su_not :
      IF e.type.mode=pc.ty_range THEN convert(e,e.type.base) END;
      IF (e.type.mode=pc.ty_boolean) THEN
        unary(e,pc.su_not,e.type);
      ELSIF ((pcS.ext() OR pcS.TS_ext()) AND (e.type.mode IN pc.WHOLEs)) THEN
        unary(e,pc.su_compl,e.type);
      ELSIF e.type.mode#pcO.ty_invtype THEN
        pcS.error(ps,'e',incomp);
        e.type:=pcO.invtype;
      END;
  END;
  e.pos := ps;
  e.end := pcS.txtpos;
END gen_unary_operator;

(*----------------------------------------------------------------*)

PROCEDURE gen_deref*(VAR n: pc.NODE);
  VAR type: pc.STRUCT; l: pc.NODE;
BEGIN
  IF (n.mode=pc.nd_type) & (n.type.base#NIL)THEN
    IF (pcS.TS_ext() OR pcS.ext()) AND (n.type.base.mode # pc.ty_void) THEN
      WHILE (n.type.mode=pc.ty_opaque) & (n.type.base#NIL) DO
        n.type := n.type.base;
      END;
      n.type := n.type.base;
    ELSE
      pcS.err(54);
      n.type:=pcO.invtype;
    END;
    RETURN;
  END;
  WHILE (n.type.mode=pc.ty_opaque) & (n.type.base#NIL) DO
    convert(n,n.type.base);
  END;
  type:=n.type;
  IF type.mode=pc.ty_pointer THEN
    IF type.base.mode=pcO.ty_undef THEN
      ASSERT(pcO.otag_forward IN type.base.obj.tags);
      pcS.err(97,type.base.obj.name^);
      EXCL(type.base.obj.tags,pcO.otag_forward);
      type.base:=pcO.invtype;
    END;
    type:=type.base;
  ELSE
    IF n.mode # pc.nd_inv THEN
      (* Don't use err() here, gen_deref may be called with sproc of invtype *)
      pcS.err(52);
    END;
    type:=pcO.invtype;
  END;
  l:=n;
  pcO.new(n,pc.nd_deref);
  n.end := pcS.txtpos;
  IF env.nil_check IN env.config.tags THEN
    INCL(n.tags,pc.ntag_chk_range)
  END;
  n.type:=type;
  n.l:=l;
  IF (l.type.mode=pc.ty_pointer) & (n.type.mode=pc.ty_array_of) THEN
    INCL(n.tags,pcO.ntag_dynarr);
  END;
END gen_deref;

PROCEDURE gen_index*(ps: pc.TPOS; VAR n: pc.NODE; ex: pc.NODE);
  VAR x: pc.NODE; check: BOOLEAN; f: pc.TY_MODE; inx: pc.STRUCT;
BEGIN
  chk_designator(n);
  f:=n.type.mode;
  IF f=pcO.ty_invtype THEN
    RETURN;
  ELSIF pcO.ntag_dynarr IN n.tags THEN
    ASSERT(f=pc.ty_array_of);
    check:=(env.dynamic_check IN env.config.tags);
    inx:=n.type.inx;
  ELSIF f IN pc.TY_SET{pc.ty_array,pc.ty_SS,pc.ty_array_of} THEN
    check:=(env.index_check IN env.config.tags) &
           ~ (n.type.flag IN pc.LangSet{pc.flag_c,pc.flag_sl1,pc.flag_p,
                        pc.flag_stdcall,pc.flag_syscall,pc.flag_javacall});
    inx:=n.type.inx;
  ELSE
    err(n,50);
    n.type:=pcO.invtype;
    RETURN;
  END;
  ASSERT(inx#NIL);
  ASSERT(ex.type#NIL);
  IF ex.type.mode IN pc.TY_SET{pc.ty_longlongint, pc.ty_longlongcard} THEN
    type_incomp(ex.pos,pc.longcard_type, ex.type);
  END;
  IF pcS.oberon THEN
    IF is_whole(host_type(ex.type)) THEN
      IF inx.mode = pc.ty_enum THEN
        assign_compatibility(ex.pos,inx,ex);
      END;
      convert(ex,inx);
    ELSE
      IF n.type.inx.flag # pc.flag_o2 THEN
        assign_compatibility(ex.pos,inx,ex);
      ELSE
        err(ex,30)
      END;
    END;
  ELSE
    assign_compatibility(ex.pos,inx,ex);
  END;
  pcO.new(x,pc.nd_index);
  x.type:=n.type.base;
  x.l:=n; x.r:=ex;
  x.pos := ps;
  x.end := pcS.txtpos;
  IF check THEN
    INCL(x.tags,pc.ntag_chk_range)
  ELSIF (x.r.mode=pc.nd_unary) & (x.r.sub=pc.su_conv) THEN
    EXCL(x.r.tags,pc.ntag_chk_range)
  END;
  n:=x;
  IF (n.type.mode=pc.ty_array_of) & (pcO.ntag_dynarr IN n.l.tags) THEN
    INCL(n.tags,pcO.ntag_dynarr);
  END;
END gen_index;

PROCEDURE gen_access*(ps: pc.TPOS; VAR n: pc.NODE);
  VAR f: pc.OBJECT; x: pc.NODE;
BEGIN
  IF (n.mode=pc.nd_type) & ~ pcS.TS_ext() & ~pcS.ext() THEN
    pcS.err(54);
    n.type:=pcO.invtype;
    RETURN;
  END;
  IF n.type.mode=pcO.ty_invtype THEN RETURN END;
  IF n.type.mode#pc.ty_record THEN err(n,51); n.type:=pcO.invtype; RETURN END;
  pcO.fnd_qua(n.type,pcS.name,f);
  IF f.mode=pc.ob_inv THEN n.type:=pcO.invtype; RETURN END;
  IF (f.mno#pc.cur_mod) & ~ (pc.otag_public_f IN f.tags) THEN
    pcS.err(116,f.name^)
  END;
  IF f.mode IN pc.PROCs THEN
    IF f.type.prof.type.mode=pc.ty_pointer THEN
      ASSERT(f.type.prof.type.flag=pc.flag_o2);
      IF n.mode#pc.nd_deref THEN pcS.err(68);
      ELSIF n.l.type.flag#pc.flag_o2 THEN pcS.err(71);
      ELSE n:=n.l;
      END;
    END;
    guard(n,f.type.prof.type,FALSE);
    pcO.new(x,pc.nd_method);
    x.l:=n;
    x.obj:=f;
    x.type:=f.type;
    ASSERT(f.mode=pc.ob_xproc);
  ELSE
    ASSERT((f.mode=pc.ob_field) OR (f.mode=pc.ob_inv));
    ASSERT(f.type#NIL);
    pcO.new(x,pc.nd_field);
    x.l:=n;
    x.obj:=f;
    x.type:=f.type;
  END;
  x.pos := ps;
  x.end := pcS.txtpos;
  n:=x;
END gen_access;

PROCEDURE gen_usage*(v: pc.OBJECT; VAR n: pc.NODE);
BEGIN
  pcO.new(n,pc.nd_inv);
  n.obj:=v;
  n.type:=v.type;
  IF    v.mode IN pc.VARs   THEN n.mode:=pc.nd_var;
  ELSIF v.mode=pc.ob_sproc  THEN n.mode:=pc.nd_sproc;
  ELSIF v.mode IN pc.PROCs  THEN n.mode:=pc.nd_proc;
  ELSIF v.mode=pc.ob_type   THEN n.mode:=pc.nd_type;
  ELSIF v.mode=pc.ob_cons   THEN n.mode:=pc.nd_var;
  ELSIF v.mode=pc.ob_inv    THEN n.mode:=pc.nd_inv;
  ELSIF v.mode=pc.ob_module THEN
    pcS.error(n.pos,'e',27,v.name^);
    n.mode := pc.nd_inv; n.type:=pcO.invtype;
  ELSIF v.mode=pc.ob_label  THEN
    IF pc.ntag_lbl_used IN v.attr(pc.NODE).tags THEN
      pcS.error(n.pos, 'e', 289, v.name^);
      n.mode:=pc.nd_label; n.obj:=v; n.type:=pcO.undef;
    ELSE
      n      := v.attr(pc.NODE);
      INCL(n.tags, pc.ntag_lbl_used);
    END;
  ELSE ASSERT(FALSE);
  END;
  ASSERT(n.type#NIL);
END gen_usage;

PROCEDURE gen_literal*(sy: pcS.Symbol; VAR n: pc.NODE);
  VAR type: pc.STRUCT;
BEGIN
  CASE sy OF
    |pcS.val_integer   : type:=pcO.ZZ_type;
    |pcS.val_real      : type:=pcO.RR_type;
    |pcS.val_long_real : type:=pcO.longreal;
    |pcS.val_cmplx     : type:=pcO.CC_type;
    |pcS.val_long_cmplx: type:=pcO.lcomplex;
    |pcS.val_char      : type:=pcO.char;
    |pcS.val_string    :
      type := pcO.new_type(pc.ty_SS);
      zz_tmp.unary(pc.su_length,pcS.val);
      type.len:=zz_tmp.get_integer()+1;
  END;
  pcO.new(n,pc.nd_value);
  n.type:=type;
  n.val := pcS.val;
  n.end := pcS.txtpos;
  IF pcS.val_hex THEN
    INCL(n.tags,pc.ntag_hex);
    ASSERT(type.mode IN pc.TY_SET{pc.ty_ZZ,pc.ty_char});
  END;
  pcS.val:=NIL;
END gen_literal;

(*--------------------------------------------------------*)

PROCEDURE chk_threat*(cv: pc.OBJECT; n: pc.NODE): BOOLEAN;
(* Returns TRUE, if control variable cv (FOR, WITH/O2)
   is treatened in statements "n".
   "cv" should be variable.
*)

  VAR procs: pc.USAGE; (* list of all marked procedures *)

  PROCEDURE threat_err(n: pc.NODE);
  BEGIN
    pcS.error(n.pos,'e',158,cv.name^);
  END threat_err;

  PROCEDURE chk_var(n: pc.NODE): BOOLEAN;
  BEGIN
    ASSERT(n#NIL);
    WHILE (n.mode = pc.nd_guard) OR (n.mode = pc.nd_lconv) DO n:=n.l END;
    IF (n.mode = pc.nd_var) & (n.obj = cv) THEN
      threat_err(n); RETURN TRUE
    END;
    RETURN FALSE
  END chk_var;

  PROCEDURE chk_list(n: pc.NODE): BOOLEAN;

    PROCEDURE chk_call(n: pc.NODE): BOOLEAN;
      VAR l: pc.NODE; procT: pc.STRUCT; f,proc: pc.OBJECT; u: pc.USAGE;
    BEGIN
      IF n.l#NIL THEN procT:=n.l.type ELSE procT:=n.obj.type END;
      ASSERT(procT.mode = pc.ty_proctype);
      f:=procT.prof;
      IF (n.l#NIL) & (n.l.mode = pc.nd_method) THEN (* skip this *)
        f:=f.next;
      END;
      l:=n.r;
      WHILE f#NIL DO
        IF (f.mode=pc.ob_varpar) & chk_var(l) THEN
          threat_err(l); RETURN TRUE
        END;
        f:=f.next;
        l:=l.next;
      END;
      proc:=n.obj;
      IF  (pcO.omark_threatened IN cv.marks) (* not necessary for non-threatened *)
        & (proc # NIL) & (proc.val # NIL)    (* call of normal procedure (i.e. non FORWARD) *)
        & (proc.mode IN pc.OB_SET{pc.ob_proc,pc.ob_xproc,pc.ob_lproc})
        & ~ (pcO.omark_threat_proc IN proc.marks)
        & ((proc.host = cv.host)             (* from the same block as "cv" *)
           OR (proc.lev > cv.lev))           (* or from nested blocks *)
      THEN
        INCL(proc.marks,pcO.omark_threat_proc);
        NEW(u);
        u.obj:=proc;
        u.next:=procs;
        procs:=u;
        IF chk_list(proc.val.r) THEN RETURN TRUE END;
      END;
      RETURN FALSE
    END chk_call;

    VAR down: BOOLEAN;
  BEGIN
    WHILE n # NIL DO
      down:=TRUE;
      CASE n.mode OF
       |pc.nd_for:
         IF n.obj = cv THEN threat_err(n); RETURN TRUE END;
       |pc.nd_assign:
         IF n.obj = cv THEN threat_err(n); RETURN TRUE END;
         IF (n.l#NIL) & chk_var(n.l) THEN RETURN TRUE END;
       |pc.nd_call  :
         IF chk_call(n) THEN RETURN TRUE END;
       |pc.nd_binary:
         CASE n.sub OF
         |pc.sb_pre_inc,pc.sb_pre_dec,pc.sb_post_inc,pc.sb_post_dec:
           IF chk_var(n.l) THEN RETURN TRUE END;
         ELSE
         END;
       |pc.nd_unary:
         IF (n.sub = pc.su_adr) OR (n.sub = pc.su_adr_o2) THEN
           IF chk_var(n.l) THEN RETURN TRUE END;
         END;
       |pc.nd_sproc:
         IF (n.sub = pc.sp_get) OR (n.sub = pc.sp_getreg) THEN
           IF chk_var(n.r.next) THEN RETURN TRUE END;
         ELSIF (n.sub = pc.sp_new)
            OR (n.sub = pc.sp_sysnew)
            OR (n.sub = pc.sp_dispose)
         THEN
           IF chk_var(n.r) THEN RETURN TRUE END;
         END;
       |pc.nd_pair,pc.nd_value,pc.nd_exit,pc.nd_return
       ,pc.nd_activate,pc.nd_reraise,pc.nd_retry: down:=FALSE;
      ELSE
      END;
      IF down & (chk_list(n.l) OR chk_list(n.r)) THEN RETURN TRUE END;
      n:=n.next;
    END;
    RETURN FALSE
  END chk_list;

  VAR res: BOOLEAN;
BEGIN
  ASSERT(cv.mode IN pc.VARs);
  ASSERT(cv.lev >= 0);
  IF env.errors.err_cnt # 0 THEN RETURN FALSE END;
  procs:=NIL;
  res:=chk_list(n);
  WHILE procs#NIL DO
    ASSERT(pcO.omark_threat_proc IN procs.obj.marks);
    EXCL(procs.obj.marks,pcO.omark_threat_proc);
    procs:=procs.next;
  END;
  RETURN res
END chk_threat;

(*--------------------------------------------------------*)

PROCEDURE ini*;
(** вызывать только после pcO.ini *)
BEGIN
  IF zz_tmp=NIL THEN
    zz_tmp:=pc.value.new(pcS.txtpos,pcO.integer);
    zz_zero:=pc.value.new(pcS.txtpos,pcO.integer);
    zz_zero.set_integer(0);
    rr_tmp1:=pc.value.new(pcS.txtpos,pcO.real);
    rr_tmp2:=pc.value.new(pcS.txtpos,pcO.real);
  END;
END ini;

PROCEDURE exi*;
BEGIN
END exi;

BEGIN
  zz_tmp:=NIL;
  zz_zero:=NIL;
  rr_tmp1:=NIL;
  rr_tmp2:=NIL;
  cmp_value_tmp:=NIL;
END pcB.
