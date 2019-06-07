(** Copyright (c) 1991,97 XDS Ltd, Russia. All Rights Reserved. *)
MODULE ccCode; (** Sem 22-Sep-93. *)

(**  Головной модуль генерации в С *)

(* Modifications:
   08/Feb/96 Ned  2.12  op_gendll in "gen_includes"
   08/Feb/96 Ned  2.12  back-end version 4.10
   16/Mar/96 Ned  2.12  back-end version 4.11
   17/Mar/96 Ned  2.12  some BNRP specific is removed: GENNOBITREF,
                        for, include files.
   17/Mar/96 Ned  2.12  Comments copying is improved.
   24-Mar-96 Ned  2.13  code.en_cnsexp is initialized.
*)

IMPORT
  pc :=pcK,
  cc :=ccK,
  nms:=ccN,
  out:=ccL,
  def:=ccDef,
  dcl:=ccDcl,
  cmt:=ccComments,
  xfs:=xiFiles,
  cce:=ccE,
  seg:=ccSeg,
  rec:=ccRec,
  env:=xiEnv,
       xcStr,
  <* IF pcvis THEN *> pcVis, <* END *>
  tim:=SysClock;

IMPORT SYSTEM;

TYPE
  STR = dcl.STR;

VAR
  zz_tmp     : pc.VALUE;
  zz_gcr     : pc.VALUE; (* is used in gen_case_range *)
  zz_abs_stp : pc.VALUE; (* is used in gen_statement_for *)
  zz_one     : pc.VALUE;

VAR default_alignment : SHORTINT;

PROCEDURE ^ gen_sequence(n: pc.NODE; lev: INTEGER;
			VAR dead_end: BOOLEAN;
			VAR return_end: BOOLEAN;
			no_return: BOOLEAN;
			do_return: BOOLEAN;
			loop: pc.NODE);


PROCEDURE new_object(VAR o: pc.OBJECT; mode: pc.OB_MODE; type: pc.STRUCT);
  (* создает объект в текущей области действия *)
  VAR h: pc.STRUCT;
BEGIN
  ASSERT(~dcl.gen_def);
  IF dcl.cur_proc#NIL THEN h:=dcl.cur_proc ELSE h:=dcl.cur_mod.type END;
  o:=pc.new_object_ex(type,h,mode,FALSE);
END new_object;

PROCEDURE cre_label(VAR o: pc.OBJECT; nm-: ARRAY OF CHAR);
BEGIN
  new_object(o,pc.ob_label,NIL);
  NEW(o.name,LENGTH(nm)+1);
  COPY(nm,o.name^);
  INCL(o.tags,cc.otag_declared);
END cre_label;

PROCEDURE gen_case_range(n: pc.NODE; lev: INTEGER);
  VAR v: pc.VALUE; l: pc.NODE;
BEGIN
  ASSERT(n.mode=pc.nd_pair);
  v:=n.l.val;
  l:=cce.val_node(n.val,TRUE);
  IF l#NIL THEN
    out.sp(lev);
    out.ws("case ");
    cce.gen_value(l,-1,{});
    out.wf(":");
    IF cc.op_comments THEN cmt.tail_comment(n.pos) END;
    out.wl;
    IF dcl.cmp_value(pc.sb_equ,n.val,v) THEN RETURN END;
    zz_gcr.binary(pc.sb_plus,n.val,zz_one);
  ELSE
    zz_gcr.unary(pc.su_conv,n.val);
  END;
  LOOP
    out.sp(lev);
    out.ws("case ");
    dcl.const_aggregate(zz_gcr,n.type,0,FALSE,FALSE);
    out.wf(":");
    IF cc.op_comments THEN cmt.tail_comment(n.pos) END;
    out.wl;
    IF dcl.cmp_value(pc.sb_equ,zz_gcr,v) THEN EXIT END;
    zz_gcr.binary(pc.sb_plus,zz_gcr,zz_one);
  END;
END gen_case_range;

PROCEDURE gen_braced_sequence(n: pc.NODE;
		lev: INTEGER;
		VAR dd: BOOLEAN; (* dead end *)
		nrt: BOOLEAN;  	(* do not gen return *)
		trt: BOOLEAN;   (* do gen return *)
		loop: pc.NODE);
  VAR ps: out.POS; rt: BOOLEAN;
BEGIN
  out.enter_statement(ps);
  out.wr("{"); out.wl;
  gen_sequence(n,lev,dd,rt,nrt,trt,loop);
  out.sp(lev-1);
  out.wr("}"); out.wl;
  out.pack_statement(ps);
END gen_braced_sequence;

PROCEDURE gen_statement_if(n: pc.NODE; lev: INTEGER;
			   nrt,trt: BOOLEAN; loop: pc.NODE);
  VAR dd: BOOLEAN;
BEGIN
  out.ws("if ("); cce.gen_value(n.l,-1,{dcl.BLN}); out.ws(") ");
  gen_braced_sequence(n.r.l,lev+1,dd,nrt,trt,loop);
  IF n.r.r=NIL THEN RETURN END;
  out.sp(lev); out.ws("else ");
  IF (n.r.r.mode=pc.nd_if) & (n.r.r.next=NIL) THEN
    gen_statement_if(n.r.r,lev,nrt,trt,loop);
  ELSE
    gen_braced_sequence(n.r.r,lev+1,dd,nrt,trt,loop);
  END;
END gen_statement_if;

PROCEDURE gen_statement_for(n: pc.NODE; lev: INTEGER; trt: BOOLEAN);

  VAR
    var_to,var_no,var_nm: STR;
    stp: pc.VALUE;
    assigned: BOOLEAN;
    l: pc.NODE;

  PROCEDURE gen_value_to;
    VAR str: STR;
  BEGIN
    IF n.obj.type.mode=pc.ty_char THEN
      nms.x2c(nms.nm_shortcard,str); out.wf("(%s)",str);
    END;
    IF var_to#"" THEN out.ws(var_to);
    ELSE cce.gen_value(l.r,13,{});
    END;
  END gen_value_to;

  PROCEDURE gen_var_nm;
    VAR str: STR;
  BEGIN
    IF n.obj.type.mode=pc.ty_char THEN
      nms.x2c(nms.nm_shortcard,str); out.wf("(%s)",str);
    END;
    out.ws(var_nm);
  END gen_var_nm;

  PROCEDURE gen_var_assign;
  BEGIN
    ASSERT(NOT assigned);
    out.wf("%s = ",var_nm);
    cce.gen_value(l.l,0,{});
    assigned:=TRUE;
  END gen_var_assign;

  PROCEDURE gen_pre_check(en_if: BOOLEAN): BOOLEAN;
    (* возвращает FALSE если цикл не исполняется ни разу *)
    VAR neg: BOOLEAN;
  BEGIN
    neg:=stp.is_neg();
    IF (l.r.mode=pc.nd_value) & (l.l.mode=pc.nd_value) THEN
      IF neg THEN RETURN dcl.cmp_value(pc.sb_geq,l.l.val,l.r.val);
      ELSE        RETURN dcl.cmp_value(pc.sb_leq,l.l.val,l.r.val);
      END;
    END;
    IF l.r.mode=pc.nd_value THEN
      IF neg THEN zz_tmp.binary(pc.sb_geq,n.obj.type.min,l.r.val);
      ELSE        zz_tmp.binary(pc.sb_leq,n.obj.type.max,l.r.val);
      END;
      IF NOT zz_tmp.is_zero() THEN RETURN TRUE END;
    END;
    IF NOT en_if THEN RETURN TRUE END;
    IF NOT assigned THEN
      out.sp(lev);
      gen_var_assign;
      out.wf(";\n");
    END;
    out.sp(lev); out.ws("if ("); gen_var_nm;
    IF neg THEN out.ws(">=") ELSE out.ws("<=") END;
    gen_value_to; out.ws(") ");
    RETURN TRUE;
  END gen_pre_check;

  PROCEDURE gen_var_inc(stp1: BOOLEAN);
    VAR str: STR;
  BEGIN
    IF pc.ntag_chk_range IN n.tags THEN
      nms.r_chk(n.obj.type,str);
      out.wf("%s = %s(%s",var_nm,str,var_nm);
      IF stp.is_neg() THEN out.ws("-") ELSE out.ws("+") END;
      dcl.const_aggregate(zz_abs_stp,n.obj.type,11,FALSE,FALSE);
      out.wr(',');
      dcl.const_aggregate(n.obj.type.min,n.obj.type,0,FALSE,FALSE);
      out.wr(',');
      dcl.const_aggregate(n.obj.type.max,n.obj.type,0,FALSE,FALSE);
      out.wr(")");
    ELSE
      out.ws(var_nm);
      IF stp1 THEN
	IF stp.is_neg() THEN out.ws("--") ELSE out.ws("++") END;
      ELSE
	IF stp.is_neg() THEN out.ws(" -= ") ELSE out.ws(" += ") END;
	dcl.const_aggregate(zz_abs_stp,n.obj.type,1,FALSE,FALSE);
      END;
    END;
  END gen_var_inc;

  VAR
    str: STR;
    dd,qq,rt: BOOLEAN;
    t: pc.STRUCT;
BEGIN
  l:=n.l; stp:=l.val; assigned:=FALSE;
  IF stp=NIL THEN stp:=zz_one END;
  zz_abs_stp.unary(pc.su_abs,stp);
  dcl.o_usage(n.obj,var_nm);

  var_to:="";
  IF l.r.mode#pc.nd_value THEN
    dcl.make_temp_var(n.obj.type,var_to);
    out.sp(lev); out.wf("%s = ",var_to);
    cce.gen_value(l.r,0,{}); out.wf(";\n");
  END;
  IF dcl.cmp_value(pc.sb_equ,zz_abs_stp,zz_one) THEN
    (* шаг равен +/- 1 *) (* или число повторений константа !!!!! *)
    qq:=(l.r.mode=pc.nd_value) &
        dcl.cmp_value(pc.sb_neq,l.r.val,n.obj.type.min) &
        dcl.cmp_value(pc.sb_neq,l.r.val,n.obj.type.max);
    IF NOT gen_pre_check(NOT qq) THEN RETURN END;
    IF out.col_ps=0 THEN out.sp(lev) END;
    out.ws("for (");
    IF NOT assigned THEN gen_var_assign END;
    out.wr(";");
    IF qq THEN
      out.wr(" "); gen_var_nm;
      IF stp.is_neg() THEN out.ws(">=") ELSE out.ws("<=") END;
      gen_value_to;
    END;
    out.ws("; ");
    gen_var_inc(TRUE); out.wf(") {\n");
    gen_sequence(n.r,lev+1,dd,rt,FALSE,trt,n);
    (* значения zz_* испорчены ! *)
    IF NOT qq THEN
      out.sp(lev+1); out.ws("if (");
      gen_var_nm; out.ws("=="); gen_value_to;
      out.wf(") break;\n");
    END;
  ELSE
    IF var_to="" THEN dcl.make_temp_var(n.obj.type,var_no);
    ELSE COPY(var_to,var_no);
    END;
    out.sp(lev); gen_var_assign; out.wf(";\n");
    IF NOT gen_pre_check(TRUE) THEN RETURN END;
    IF out.col_ps=0 THEN out.sp(lev) END;
    out.wf("for (%s = ",var_no);
    IF (l.l.mode=pc.nd_value) & (l.r.mode=pc.nd_value) THEN
      IF stp.is_neg() THEN zz_tmp.binary(pc.sb_minus,l.l.val,l.r.val);
      ELSE zz_tmp.binary(pc.sb_minus,l.r.val,l.l.val);
      END;
      zz_tmp.binary(pc.sb_div,zz_tmp,zz_abs_stp); (* число повторений *)
      dcl.const_aggregate(zz_tmp,n.obj.type,1,FALSE,FALSE);
    ELSE
      t:=n.obj.type.super_type();
      CASE t.mode OF
	|pc.ty_shortint,pc.ty_shortcard: nms.x2c(nms.nm_shortcard,str);
	|pc.ty_integer, pc.ty_cardinal : nms.x2c(nms.nm_cardinal,str);
	|pc.ty_longint, pc.ty_longcard : nms.x2c(nms.nm_longcard,str);
      END;
      out.wf("(%s)(",str);
      IF stp.is_neg() THEN
	gen_var_nm; out.wr("-"); gen_value_to;
      ELSE
	gen_value_to; out.wr("-"); gen_var_nm;
      END;
      out.ws(")/");
      dcl.const_aggregate(zz_abs_stp,n.obj.type,12,FALSE,FALSE);
    END;
    out.wf(";;) {\n");
    gen_sequence(n.r,lev+1,dd,rt,FALSE,trt,n);
    (* значения zz_* испорчены ! *)
    zz_abs_stp.unary(pc.su_abs,stp);
    out.sp(lev+1); out.wf("if (!%s) break;\n",var_no);
    out.sp(lev+1); out.wf("--%s;\n",var_no);
    out.sp(lev+1); gen_var_inc(FALSE); out.wf(";\n");
  END;
  out.sp(lev); out.wf("} "); dcl.out_comment("end for");
END gen_statement_for;

PROCEDURE gen_statement_return(	n: pc.NODE;
				lev: INTEGER;
                                VAR rt: BOOLEAN;
				nrt,trt: BOOLEAN);
(*
	nrt - оператор является последним в блоке: можно не генерить
		goto в операторе RETURN

	trt - по RETURN нужно выйти из процедуры оператором return,
		нет кода, который нужно исполнить после RETURN но до
		выхода из тела C-процедуры
*)
  VAR nm: STR;
BEGIN
  IF (dcl.cur_proc#NIL) & dcl.is_c_arr(dcl.cur_proc.base) THEN
    nms.x2c(nms.nm_memcpy,nm); out.sp(lev); out.ws(nm); out.wr("(");
    dcl.o_second_name(dcl.cur_proc.obj,0,"_ret",nm); out.wf("%s,",nm);
    cce.gen_value(n.l,0,{dcl.REF,dcl.ANY}); out.wr(',');
    dcl.gen_sizeof(dcl.cur_proc.base); out.wf(");\n");
    IF trt THEN
      rt:=TRUE;
      out.sp(lev); out.wf("return %s;\n",nm);
      RETURN;
    END;
  ELSIF trt THEN
    rt:=TRUE;
    out.sp(lev); out.ws("return");
    IF n.l#NIL THEN out.wr(' '); cce.gen_value(n.l,-1,{}) END;
    out.wf(";\n");
    RETURN;
  ELSIF n.l#NIL THEN
    ASSERT(dcl.cur_proc#NIL);
    dcl.o_second_name(dcl.cur_proc.obj,0,"_ret",nm);
    out.sp(lev); out.wf("%s = ",nm);
    cce.gen_value(n.l,1,{}); out.wf(";\n");
    def.ret_emul:=TRUE;
    rt:=FALSE;
  END;
  IF NOT nrt THEN
    (* генерим goto на конец блока *)
    IF n.r.obj=NIL THEN
      cre_label(n.r.obj,"label");
      n.r.obj.pos:=n.pos;
    END;
    dcl.o_usage(n.r.obj,nm);
    out.sp(lev); out.wf("goto %s;\n",nm);
    rt:=FALSE;
  END;
END gen_statement_return;

PROCEDURE gen_sproc(n: pc.NODE; lev: INTEGER; VAR nl: BOOLEAN);
  VAR l: pc.NODE; nm: STR; i: LONGINT; ch: CHAR;
BEGIN
  nl:=TRUE;
  IF n.sub#pc.sp_code THEN out.sp(lev) END;
  CASE n.sub OF
    |pc.sp_put:
      IF dcl.is_c_arr(n.r.next.type) THEN
	nms.x2c(nms.nm_memcpy,nm); out.wf("%s(",nm);
	cce.gen_value(n.r,0,{}); out.wr(',');
	cce.gen_value(n.r.next,0,{dcl.REF,dcl.ANY}); out.wr(',');
        cce.gen_size(n.r.next,0,0); out.wf(");");
      ELSE
	dcl.type_designator(nm,"*",n.r.next.type);
	out.wf("*(%s)",nm); cce.gen_value(n.r,13,{});
        out.ws(" = "); cce.gen_value(n.r.next,1,{}); out.wf(";");
      END;
    |pc.sp_get:
      IF dcl.is_c_arr(n.r.next.type) THEN
	nms.x2c(nms.nm_memcpy,nm); out.wf("%s(",nm);
	cce.gen_value(n.r.next,0,{dcl.REF,dcl.ANY,dcl.LVL}); out.wr(',');
	cce.gen_value(n.r,0,{}); out.wr(',');
        cce.gen_size(n.r.next,0,0); out.wf(");");
      ELSE
        cce.gen_value(n.r.next,1,{dcl.LVL}); out.ws(" = ");
	dcl.type_designator(nm,"*",n.r.next.type);
        out.wf("*(%s)",nm); cce.gen_value(n.r,13,{}); out.wf(";");
      END;
    |pc.sp_new,pc.sp_sysnew,pc.sp_dispose:
      cce.gen_call_storage(n); out.wf(";");
    |pc.sp_assert:
      IF (n.r.mode#pc.nd_value) OR (n.r.val.is_zero()) THEN
	IF n.r.mode#pc.nd_value THEN
	  out.ws("if ("); cce.gen_value(n.r,-1,{dcl.NEG,dcl.BLN});
	  out.ws(") ");
	END;
        nms.x2c(nms.nm_assert,nm); out.wf("%s(",nm);
        IF n.r.next # NIL THEN cce.gen_value(n.r.next,0,{});
        ELSE out.wr('0');
        END;
        out.ws(");");
      ELSE nl:=FALSE;
      END;
    |pc.sp_incl,pc.sp_excl:
      ASSERT(n.r.type.mode=pc.ty_set);
      IF NOT dcl.is_c_arr(n.r.type) THEN
	cce.gen_value(n.r,1,{dcl.LVL});
	IF n.sub=pc.sp_incl THEN out.ws(" |= ") ELSE out.ws(" &= ~") END;
	IF n.r.next.mode=pc.nd_value THEN
	  zz_tmp.binary(pc.sb_minus,n.r.next.val,n.r.type.min);
	  out.wf("0x%X",{zz_tmp.get_integer()});
          dcl.suffix(n.r.type); out.wf(";");
	ELSE
	  out.ws("(1"); dcl.suffix(n.r.type); out.ws("<<");
	  cce.gen_index_check(n.r.next,n.r,10,0,pc.ntag_chk_range IN n.tags);
          out.wf(");");
	END;
      ELSE
	IF n.sub=pc.sp_incl THEN out.ws("X2C_INCL(");
	ELSE out.ws("X2C_EXCL(");
	END;
	cce.gen_value(n.r,0,{dcl.LVL,dcl.BSA}); out.wr(',');
	l:=n.r.next;
	cce.gen_value_minus_min(l,0,n.r.type.min,TRUE);
        out.wf(",%d);",n.r.type.len);
      END;
    |pc.sp_code:
      ASSERT(n.r.mode=pc.nd_value);
      ch:=0X;
      IF cc.op_krc THEN
	i:=0;
	LOOP
	  IF i>n.r.type.len-2 THEN EXIT END;
	  zz_tmp.index_get(i,n.r.val);
	  ch:=CHR(zz_tmp.get_integer());
	  IF ch#' ' THEN EXIT END;
	  INC(i);
        END;
      END;
      IF ch#'#' THEN out.sp(lev) END;
      FOR i:=0 TO n.r.type.len-2 DO
	zz_tmp.index_get(i,n.r.val);
	out.wr(CHR(zz_tmp.get_integer()));
      END;
  ELSE
    nms.sproc_name(n.pos,n.sub,nm);
    out.wf("%s(",nm);
    l:=n.r;
    WHILE l#NIL DO
      IF l#n.r THEN out.wr(",") END;
      IF l.type.mode IN pc.ARRs THEN
	cce.gen_value(l,0,{dcl.BSA}); out.wr(',');
	cce.gen_len(l,0,l.type.inx,0);
      ELSE
	cce.gen_value(l,0,{});
      END;
      l:=l.next;
    END;
    out.wf(");");
  END
END gen_sproc;

PROCEDURE gen_statement(n: pc.NODE;
			lev: INTEGER;
                        VAR rt: BOOLEAN;
			nrt,trt: BOOLEAN;
			loop: pc.NODE);
(*
	nrt - оператор является последним в блоке: можно не генерить
		goto в операторе RETURN

	trt - по RETURN нужно выйти из процедуры оператором return,
		нет кода, который нужно исполнить после RETURN но до
		выхода из тела C-процедуры
*)
  VAR
    nm  : STR;
    buf : STR;
    l,m,k: pc.NODE;
    dd  : BOOLEAN;
    t   : pc.STRUCT;
    tmps: dcl.TMP_VAR;
    nl  : BOOLEAN;
BEGIN
  nl:=FALSE;
  dcl.enter_statement(tmps);
  IF cc.op_comments THEN cmt.next_comment(n.pos,lev) END;
  IF cc.op_lineno THEN dcl.out_line_no(n.pos,lev) END;
  IF (n.mode#pc.nd_block) &
     (n.mode#pc.nd_return) &
     (n.mode#pc.nd_sproc) &
     (n.mode#pc.nd_for)
  THEN
    (* nd_return и nd_block могут вырождаться в пустую строку! *)
    out.sp(lev);
  END;
  CASE n.mode OF
    |pc.nd_call:
      cce.gen_call(n,-1,{});
      out.wf(";");
      rt:=FALSE; nl:=TRUE;
    |pc.nd_assign:
      IF dcl.is_c_arr(n.r.type) THEN
        cce.gen_assign(n,-1,{dcl.REF,dcl.ANY});
      ELSE
        cce.gen_assign(n,-1,{});
      END;
      out.wf(";");
      rt:=FALSE; nl:=TRUE;
    |pc.nd_while:
      out.ws("while ("); cce.gen_value(n.l,-1,{dcl.BLN}); out.ws(") ");
      gen_braced_sequence(n.r,lev+1,dd,FALSE,trt,n);
      IF n.obj#NIL THEN
	dcl.o_usage(n.obj,nm);
        out.sp(lev);
	out.wf("%s:;\n",nm)
      END;
      rt:=FALSE;
    |pc.nd_repeat:
      out.wf("do {\n");
      gen_sequence(n.l,lev+1,dd,rt,FALSE,trt,n);
      out.sp(lev); out.ws("} while (");
      cce.gen_value(n.r,-1,{dcl.NEG,dcl.BLN});
      out.wf(");\n");
      IF n.obj#NIL THEN
	dcl.o_usage(n.obj,nm);
	out.sp(lev); out.wf("%s:;\n",nm)
      END;
      rt:=FALSE;
    |pc.nd_loop:
      out.ws("for (;;) ");
      gen_braced_sequence(n.r,lev+1,dd,FALSE,trt,n);
      IF n.obj#NIL THEN
	dcl.o_usage(n.obj,nm);
	out.sp(lev); out.wf("%s:;\n",nm)
      END;
      rt:=FALSE;
    |pc.nd_label:
      IF n.obj=NIL THEN
	cre_label(n.obj,"label");
	n.obj.pos:=n.pos;
      END;
      dcl.o_usage(n.obj,nm);
      out.wf("%s:;",nm);
      rt:=FALSE; nl:=TRUE;
    |pc.nd_block:
      IF NOT trt OR NOT nrt THEN
	gen_sequence(n.r,lev,dd,rt,TRUE,FALSE,loop);
	IF n.obj#NIL THEN
	  (* раз есть метка, значит на нее попадает управление *)
	  ASSERT(NOT (pc.ntag_no_exit IN n.tags));
	  dcl.o_usage(n.obj,nm);
	  out.sp(lev); out.wf("%s:;\n",nm);
          rt:=FALSE;
	END;
      ELSE
	gen_sequence(n.r,lev,dd,rt,TRUE,TRUE,loop);
	(* если блоку сказано trt=TRUE то у него нет выходной метки,
		поскольку не генериться goto
	*)
	ASSERT(n.obj=NIL);
	(* управление тем не менее может достичь текущей позиции C кода,
		например если в блоке вообще нет операторов RETURN,
		но это означает что процедура не возвращает результата
		и return можно не генерить
	*)
      END;
    |pc.nd_exit:
      IF n.r=loop THEN
        out.wf("break;");
      ELSE
        IF n.r.obj=NIL THEN
    	  cre_label(n.r.obj,"loop_exit");
	  n.r.obj.pos:=n.pos;
        END;
        dcl.o_usage(n.r.obj,nm);
        out.wf("goto %s;",nm);
      END;
      rt:=FALSE; nl:=TRUE;
    |pc.nd_goto:
      IF n.l.obj=NIL THEN
	cre_label(n.l.obj,"label");
	n.l.obj.pos:=n.pos;
      END;
      dcl.o_usage(n.l.obj,nm);
      out.wf("goto %s;",nm);
      rt:=FALSE; nl:=TRUE;
    |pc.nd_return:
      gen_statement_return(n,lev,rt,nrt,trt);
    |pc.nd_for:
      gen_statement_for(n,lev,trt);
      rt:=FALSE;
    |pc.nd_with:
      dcl.o_usage(n.obj,nm);
      IF pc.otag_with IN n.obj.tags THEN
        ASSERT(n.obj.type.mode=pc.ty_pointer);
        out.wf("{ "); dcl.out_comment("with"); out.sp(lev+1);
        IF cc.op_cpp THEN
          out.ff(buf,"&%s",nm);
          dcl.type_designator(nm,buf,n.obj.type.base);
          out.ws(nm); out.ws(" = ");
          cce.gen_value(n.l,1,{});
        ELSE
          dcl.type_designator(buf,nm,n.obj.type);
          out.ws(buf); out.ws(" = ");
          cce.gen_value(n.l,1,{dcl.REF});
        END;
        out.wf(";\n");
        gen_sequence(n.r,lev+1,dd,rt,nrt,trt,loop);
        out.sp(lev); out.wf("}\n");
      ELSE
        out.wf("%s = ",nm);
        cce.gen_value(n.l,1,{dcl.REF}); out.wf(";\n");
        gen_sequence(n.r,lev,dd,rt,nrt,trt,loop);
      END;
      ASSERT((pc.ntag_no_exit IN n.tags)=dd);
    |pc.nd_wtrap:
      nms.x2c(nms.nm_trap,nm);
      out.wf("%s(X2C_GUARD_TRAP);\n",nm);
      rt:=FALSE;
    |pc.nd_ftrap:
      nms.x2c(nms.nm_trap,nm);
      out.wf("%s(X2C_RETURN_TRAP);\n",nm);
      rt:=FALSE;
    |pc.nd_if:
      gen_statement_if(n,lev,nrt,trt,loop);
      rt:=FALSE;
    |pc.nd_case:
      out.ws("switch (");
      t:=n.l.type.super_type();
      IF t.mode=pc.ty_shortint THEN
	out.ws("(int)"); cce.gen_value(n.l,13,{});
      ELSIF t.mode=pc.ty_shortcard THEN
	out.ws("(unsigned)"); cce.gen_value(n.l,13,{});
      ELSE
	cce.gen_value(n.l,-1,{});
      END;
      out.wf(") {");
      IF cc.op_comments THEN cmt.tail_comment(n.l.pos) END;
      out.wl;
      m:=n.r;
      ASSERT((m.mode=pc.nd_casedo) OR (m.mode=pc.nd_caselse));
      l:=m.l;
      WHILE l#NIL DO
	ASSERT(l.mode=pc.nd_node);
	k:=l.l;
	REPEAT
	  ASSERT(k.mode=pc.nd_pair);
	  gen_case_range(k,lev);
	  k:=k.next;
	UNTIL k=NIL;
	(* nrt:=FALSE ! Иначе вместо выхода из блока по RETURN
		попадем в следующую альтернативу CASE
	*)
	gen_sequence(l.r,lev+1,dd,rt,FALSE,trt,n);
	IF NOT dd THEN out.sp(lev+1); out.wf("break;\n") END;
	l:=l.next;
      END;
      IF m.mode=pc.nd_caselse THEN
	(* пустой ELSE можно вообще не генерить *)
        IF m.r#NIL THEN
	  out.sp(lev); out.wf("default:;\n");
	  gen_sequence(m.r,lev+1,dd,rt,nrt,trt,n);
	  (* break генерить обязательно, иначе нужна ';' после метки *)
	  IF NOT dd THEN out.sp(lev+1); out.wf("break;\n") END;
        END;
      ELSE
	out.sp(lev); out.wf("default:\n"); dcl.out_line_no(n.pos,lev+1);
	nms.x2c(nms.nm_trap,nm); out.sp(lev+1); out.wf("%s(",nm);
	nms.x2c(nms.nm_case_trap,nm); out.wf("%s);\n",nm);
      END;
      out.sp(lev); out.wf("} "); dcl.out_comment("end switch");
      rt:=FALSE;
    |pc.nd_eval:
      IF dcl.is_c_arr(n.l.type) THEN
        cce.gen_value(n.l,-1,{dcl.REF,dcl.ANY});
      ELSE
        cce.gen_value(n.l,-1,{});
      END;
      out.wf(";");
      rt:=FALSE; nl:=TRUE;
    |pc.nd_sproc:
      gen_sproc(n,lev,nl);
      rt:=FALSE;
    |pc.nd_protect:
      dcl.o_usage(n.obj,nm);
      out.wf("X2C_PROTECT(&%s,",nm);
      dcl.const_aggregate(n.r.val,n.r.type,0,FALSE,FALSE);
      out.wf(");\n");
      gen_sequence(n.l,lev,dd,rt,FALSE,FALSE,loop);
      out.sp(lev); out.wf("X2C_PROTECT(&%s,%s);\n",nm,nm);
      rt:=FALSE;
    |pc.nd_except:
      dcl.o_usage(n.obj,nm);
      out.wf("if (X2C_XTRY(&%s)) {\n",nm);
      gen_sequence(n.l,lev+1,dd,rt,nrt,trt,loop);
      IF NOT dd THEN
	out.sp(lev+1); out.ws("X2C_XOFF();"); out.wl;
      END;
      out.sp(lev); out.wf("}\n");
      IF n.r#NIL THEN
        out.sp(lev); out.wf("else ");
        gen_braced_sequence(n.r,lev+1,dd,nrt,trt,loop);
      END;
      rt:=FALSE;
    |pc.nd_retry:
      out.wf("X2C_XRETRY();\n");
      rt:=FALSE;
    |pc.nd_finally:
      dcl.o_usage(n.obj,nm);
      out.wf("X2C_FINALLY(%s);\n",nm);
      rt:=FALSE;
    |pc.nd_reraise:
      out.wf("X2C_XREMOVE();\n");
      rt:=FALSE;
    |pc.nd_activate:
      out.wf("X2C_XON();\n");
      rt:=FALSE;
  ELSE
    env.errors.Error(n.pos,1012);
  END;
  IF nl THEN
    IF cc.op_comments THEN cmt.tail_comment(n.pos) END;
    out.wl;
  END;
  ASSERT(out.col_ps=0);
  dcl.exit_statement(tmps);
END gen_statement;

PROCEDURE gen_sequence(n: pc.NODE;
		lev: INTEGER;
		VAR dd: BOOLEAN;(* dead end *)
		VAR rt: BOOLEAN;(* last statement was "return" *)
		nrt: BOOLEAN;  	(* do not gen return *)
		trt: BOOLEAN;   (* do gen return *)
		loop: pc.NODE   (* for, do, while *) );
(*
	nrt - оператор является последним в блоке: можно не генерить
		goto в операторе RETURN

	trt - по RETURN нужно выйти из процедуры оператором return,
		нет кода, который нужно исполнить после RETURN но до
		выхода из тела C-процедуры

*)
BEGIN
  dd:=FALSE;
  rt:=FALSE;
  WHILE n#NIL DO
    gen_statement(n,lev,rt,(n.next=NIL) & nrt,trt,loop);
    dd:=pc.ntag_no_exit IN n.tags;
    n:=n.next;
  END;
END gen_sequence;

PROCEDURE gen_includes(imp: BOOLEAN; main: BOOLEAN);
  VAR ext: env.String; conv,enif: BOOLEAN;
  PROCEDURE gen(nm-: ARRAY OF CHAR; own,cstd: BOOLEAN);
    (* own - header file is generated by XDS *)
    VAR fnm: env.String; ch0,ch1: CHAR; s: ARRAY 4 OF CHAR;
  BEGIN
    IF NOT own & cstd THEN ch0:='<'; ch1:='>';
    ELSE ch0:='"'; ch1:='"';
    END;
    IF own & enif THEN out.wf("#ifndef %s_H_\n",nm) END;
    IF conv & own THEN
      xfs.sys.Create("",nm,ext^,fnm);
      xfs.sys.ConvertToTarget(fnm^,fnm);
      out.wf('#include %c%s%c\n',ch0,fnm^,ch1);
    ELSE
      IF ext[0]="." THEN s:="" ELSE s:="." END;
      out.wf('#include %c%s%s%s%c\n',ch0,nm,s,ext^,ch1);
    END;
    IF own & enif THEN out.wf("#endif\n") END;
  END gen;
  VAR nm: STR; i: pc.Mno; u: pc.USAGE;
BEGIN
  enif:=dcl.cur_mod.type.flag IN pc.LangSet{pc.flag_o2,pc.flag_m2,pc.flag_sl1};
  IF dcl.cur_mod.type.flag=pc.flag_o2 THEN main:=FALSE END;
  IF (dcl.cur_mod.type.flag=pc.flag_sl1) & (NOT imp OR main) THEN
    out.wf("#define X2C_SL1\n");
  END;
  IF imp & (dcl.cur_mod.type.flag IN pc.LangSet{pc.flag_o2,pc.flag_m2}) THEN
    IF cc.op_int16 THEN out.wf("#define X2C_int16\n");
    ELSE out.wf("#define X2C_int32\n");
    END;
    IF cc.op_debug THEN out.wf("#define X2C_DEBUG\n") END;
    IF cc.op_index16 THEN out.wf("#define X2C_index16\n");
    ELSE out.wf("#define X2C_index32\n");
    END;
  END;
  env.config.Equation("HEADER",ext);
  conv:=env.config.Option("CONVHDRNAME");
  IF NOT imp OR main THEN
    gen("X2C",TRUE,FALSE);
  END;
  IF imp & (cc.otag_headerfile IN dcl.cur_mod.tags) THEN
    dcl.o_usage(dcl.cur_mod,nm);
    gen(nm,cc.ttag_ownheader IN dcl.cur_mod.type.tags,
           cc.ttag_cstdlib   IN dcl.cur_mod.type.tags);
  END;
  IF imp THEN
    (* возможно это можно не делать, см. ccDef *)
    u:=dcl.cur_mod.type.use;
    WHILE u#NIL DO
      dcl.mod_usage[u.obj.mno]:=TRUE;
      u:=u.next;
    END;
  END;
  IF imp THEN
    dcl.o_usage(dcl.cur_mod,nm);
    out.wf("#define %s_C_\n",nm)
  END;
  FOR i:=pc.ZEROMno TO SYSTEM.PRED(LEN(dcl.mod_usage^)) DO
    IF dcl.mod_usage[i] & (i#dcl.cur_mod.mno) THEN
      IF cc.otag_headerfile IN pc.mods[i].tags THEN
	dcl.o_usage(pc.mods[i],nm);
        gen(nm,cc.ttag_ownheader IN pc.mods[i].type.tags,
               cc.ttag_cstdlib   IN pc.mods[i].type.tags);
      ELSIF imp THEN
	dcl.module_body_def(pc.mods[i],FALSE);
	out.wf(";\n");
      END;
    END;
  END;
  out.wl;
  (* generate in header file only *)
  IF ~imp & ~main & cc.op_gendll THEN (*Ned v2.12 *)
    dcl.o_usage(dcl.cur_mod,nm);
    out.wf("#undef X2C_DLL_TAG\n");
    out.wf("#ifdef X2C_GEN_DLL\n");
    out.wf("#ifdef %s_C_\n",nm);
    out.wf("#define X2C_DLL_TAG X2C_DLL_EXPORT\n");
    out.wf("#else\n");
    out.wf("#define X2C_DLL_TAG X2C_DLL_IMPORT\n");
    out.wf("#endif\n");
    out.wf("#else\n");
    out.wf("#define X2C_DLL_TAG\n");
    out.wf("#endif\n\n");
  END;
END gen_includes;

PROCEDURE gen_copyright(v-,name-: ARRAY OF CHAR; code: BOOLEAN);
  VAR s: env.String; l,b: STR; dt: tim.DateTime;
  CONST month="JanFebMarAprMayJunJulAugSepOctNovDec";
BEGIN
  env.config.Equation("COPYRIGHT",s);
  IF s#NIL THEN dcl.out_comment(s^) END;
  IF code & tim.CanGetClock() & env.config.Option("GENDATE") THEN
    (*	!!!! желательно писать в h-файл время записи сим-файла,
	текущее время в h-файл писать нельзя - не работает сравнение.
    *)
    tim.GetClock(dt);
    s:=pc.code.code_ext;
    IF s[0]="." THEN l:="" ELSE l:="." END;
    out.ff(b,'"@(#)%s%s%s %.3.*s %2d %2d:%02d:%02d %d"',
	name,l,s^,(dt.month-1)*3,month,dt.day,dt.hour,
	dt.minute,dt.second,dt.year);
    dcl.out_comment(b);
  END;
  IF    dcl.cur_mod.type.flag=pc.flag_o2   THEN l:="Oberon-2";
  ELSIF dcl.cur_mod.type.flag=pc.flag_sl1  THEN l:="SL1";
  ELSIF dcl.cur_mod.type.flag=pc.flag_m2   THEN l:="Modula-2";
  ELSIF dcl.cur_mod.type.flag=pc.flag_bnrp THEN l:="BNR Pascal";
  ELSE l:="";
  END;
  IF l#"" THEN
    out.ff(b,"Generated by XDS %s to %s translator",l,v);
    dcl.out_comment(b); out.wl;
  END;
END gen_copyright;

(** ------------------------------ CODE --------------------------------- *)

TYPE
  CODE = POINTER TO code_rec;
  code_rec = RECORD (pc.code_rec)
    inited: BOOLEAN;
  END;

PROCEDURE (c: CODE) inp_object(f: xfs.SymFile; o: pc.OBJECT; id: LONGINT);
BEGIN
  nms.inp_object(f,o);
END inp_object;

PROCEDURE (c: CODE) skip_object(f: xfs.SymFile; id: LONGINT);
BEGIN
  nms.skip_object(f);
END skip_object;

PROCEDURE (c: CODE) out_object(file: xfs.SymFile; o: pc.OBJECT );
BEGIN
  IF ( o.ext = NIL ) THEN
    file.WriteInt(0);
  ELSE
    o.ext.out(file);
  END;
END out_object;

PROCEDURE (c: CODE) inp_struct(f: xfs.SymFile; s: pc.STRUCT; id: LONGINT);
BEGIN
  nms.inp_struct(f,s);
END inp_struct;

PROCEDURE (c: CODE) skip_struct(f: xfs.SymFile; id: LONGINT);
BEGIN
  nms.skip_struct(f);
END skip_struct;

PROCEDURE (c: CODE) selected;
BEGIN
  env.config.SetOption("__GEN_C__",TRUE);
  env.config.Equation("CODE",c.code_ext);
  env.config.Equation("HEADER",c.head_ext);
END selected;

PROCEDURE bit_size( mode: pc.TY_MODE ) : LONGINT;
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
END bit_size;


PROCEDURE (c: CODE) set_min_value( t: pc.STRUCT; VAR v: pc.VALUE);
BEGIN
  IF t.mode IN pc.TY_SET{ pc.ty_range, pc.ty_enum,
                               pc.ty_array_of, pc.ty_SS,
                               pc.ty_ZZ }
  THEN
    v.set_integer(0);
    RETURN;
  ELSIF NOT( t.mode IN pc.BOUNDED_TYPES ) THEN
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
    v.set_real( MIN(REAL) );
  | pc.ty_longreal,
    pc.ty_ld_real  :
    v.set_real( MIN(LONGREAL) );
<* END *>
  ELSE
    IF t.mode IN pc.ORDs THEN
      v.get_min(bit_size(t.mode), t.mode IN pc.SIGNED_WHOLEs);
    ELSE
      v.set_integer(0);
    END;
  END;
  RETURN;
END set_min_value;

PROCEDURE (c: CODE) set_max_value( t: pc.STRUCT; VAR v: pc.VALUE);
BEGIN
  IF t.mode IN pc.TY_SET{ pc.ty_range, pc.ty_enum,
                               pc.ty_array_of, pc.ty_SS,
                               pc.ty_ZZ }
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
    v.set_real( MAX(REAL) );
  | pc.ty_longreal,
    pc.ty_ld_real  :
    v.set_real( MAX(LONGREAL) );
<* END *>
  ELSE
    IF t.mode IN pc.ORDs THEN
      v.get_max(bit_size(t.mode), t.mode IN pc.SIGNED_WHOLEs);
    ELSE
      v.set_integer(0);
    END;
  END;
  RETURN;
END set_max_value;

PROCEDURE (c: CODE) ini;
  VAR val : pc.STRING;  i : LONGINT;
BEGIN
  IF ~ c.inited THEN

    zz_tmp:=dcl.val(0);
    zz_gcr:=dcl.val(0);
    zz_abs_stp:=dcl.val(0);
    zz_one:=dcl.val(1);

    c.bits_per_loc :=8;
    IF env.config.Option("m2base16") THEN
      c.locs_per_word:= 2;
    ELSE
      c.locs_per_word:= 4;
    END;

    c.FRETs:=pc.REALs+pc.WHOLEs+pc.SETs+pc.CPLXs+
		pc.TY_SET{pc.ty_boolean,pc.ty_enum,pc.ty_range,pc.ty_char}+
		pc.TY_SET{pc.ty_array,pc.ty_loc}+
		pc.TY_SET{pc.ty_opaque,pc.ty_pointer}+
		pc.TY_SET{pc.ty_protection,pc.ty_proctype,pc.ty_record};

    c.max_dim:=8;
    c.max_ext_lev:=15;
    c.def_storage:=TRUE;
    c.en_tmpvar:=TRUE;
    c.en_f_inline:=FALSE;  (* do not inline functions in expressions -- LAZ *)
    c.max_sysflag:=pc.flag_syscall;

    env.config.Equation("ALIGNMENT", val);
    IF (val = NIL) THEN -- ALIGNMENT not set
      default_alignment := 1;
    ELSIF NOT(xcStr.StrToInt(val^,i) & (i IN {0,1,2,4,8,16})) THEN
      env.errors.Fault(env.null_pos,450,"Invalid ALIGNMENT value specified");
    ELSE
      IF i # 0 THEN
        default_alignment := VAL(SHORTINT, i);
      ELSE
        default_alignment := 1;
      END;
    END;
    c.inited:=TRUE;
  END;

  rec.WORD_BITS:=0;
  c.en_preopt:=~ env.config.Option("NOOPTIMIZE");

  cc.ini;
  dcl.ini;
  def.ini;
  cce.ini;
  nms.ini;
  seg.ini;

  c.int16:=cc.op_int16;
  c.index16:=cc.op_index16;
  c.address16:=cc.op_address16;
  IF c.index16 THEN c.max_index:=dcl.val(0FFFFH);
  ELSE c.max_index:=dcl.val(MAX(LONGINT));
  END;
END ini;

PROCEDURE (c: CODE) exi;
BEGIN
  seg.exi;
  nms.exi;
  cce.exi;
  def.exi;
  dcl.exi;
  cc.exi;
END exi;

PROCEDURE set_cu(m: pc.OBJECT);
BEGIN
  dcl.cur_mod:=m;
  IF dcl.cur_mod.type.flag=pc.flag_bnrp THEN
    rec.WORD_BITS:=16;
  ELSE
    rec.WORD_BITS:=32;
  END;
END set_cu;

PROCEDURE (c: CODE) gen_code(cu: pc.Mno; main: BOOLEAN);
  VAR nm: STR; fnm: env.String; i: pc.Mno; txt: out.STR;
BEGIN
  dcl.cur_proc:=NIL;
  set_cu(pc.mods[cu]);
  nms.cnt_en(dcl.cur_mod.type.flag=pc.flag_sl1);
  NEW(dcl.mod_usage,pc.mod_cnt);
  FOR i:=pc.ZEROMno TO SYSTEM.PRED(LEN(dcl.mod_usage^)) DO dcl.mod_usage[i]:=FALSE END;
  nms.do_sym_list;
  out.ini;
  cmt.ini;
  def.gen_definitions(main);
  out.gstr(txt);
  dcl.o_usage(dcl.cur_mod,nm);
  gen_copyright(c.vers,nm,TRUE);
  (* Must be after "gen_definitions", see "mod_usage" *)
  gen_includes(TRUE,main);
  IF dcl.cur_mod.type.flag=pc.flag_sl1 THEN
    out.wf("extern void %s_start() {}\n",dcl.cur_mod.name^);
  END;
  out.wstr(txt);
  IF dcl.cur_mod.type.flag=pc.flag_sl1 THEN
    out.wf("extern void %s_end() {}\n",dcl.cur_mod.name^);
  END;
  cmt.exi;
  out.save(nm,TRUE,0,fnm);
  out.exi;
END gen_code;

PROCEDURE (c: CODE) allocate(cu: pc.Mno; main: BOOLEAN; src_time: xfs.Time);
  VAR nm: STR; fnm: env.String; str: out.STR; i: pc.Mno;
BEGIN
  set_cu(pc.mods[cu]);
  IF env.config.Option("VERSIONKEY") THEN
    INCL(dcl.cur_mod.tags,cc.otag_versionkey);
  END;
  nms.cnt_en(dcl.cur_mod.type.flag=pc.flag_sl1);
  dcl.cur_proc:=NIL;
  nms.do_sym_list;
  IF NOT env.config.Option("NOINCLUDE") THEN
    out.ini;
    cmt.ini;
    NEW(dcl.mod_usage,pc.mod_cnt);
    FOR i:=pc.ZEROMno TO SYSTEM.PRED(LEN(dcl.mod_usage^)) DO dcl.mod_usage[i]:=FALSE END;
    def.gen_publics;
    out.gstr(str);
    dcl.o_usage(dcl.cur_mod,nm);
    gen_copyright(c.vers,nm,FALSE);
    out.wf("#ifndef %s_H_\n",nm);
    out.wf("#define %s_H_\n",nm);
    gen_includes(FALSE,FALSE);
    out.wstr(str);
    out.wl;
    out.wf("#endif /* %s_H_ */\n",nm);
    INCL(dcl.cur_mod.tags,cc.otag_headerfile);
    cmt.exi;
    IF NOT env.config.Option("NOHEADER") THEN
      out.save(nm,FALSE,src_time,fnm);
      INCL(dcl.cur_mod.type.tags,cc.ttag_ownheader);
    ELSE
      INCL(dcl.cur_mod.type.tags,cc.ttag_nocode);
      IF env.config.Option("CSTDLIB") THEN
        INCL(dcl.cur_mod.type.tags,cc.ttag_cstdlib);
      END;
      IF ~ (dcl.cur_mod.type.flag IN cc.c_like_flags) THEN
        env.errors.Warning(dcl.cur_mod.type.pos,351);
      END;
    END;
    IF env.config.Option("NOCODE") THEN
      INCL(dcl.cur_mod.type.tags,cc.ttag_nocode);
      IF ~ (dcl.cur_mod.type.flag IN cc.c_like_flags) THEN
	env.errors.Warning(dcl.cur_mod.type.pos,352);
      END;
    END;
    out.exi;
  END;
END allocate;

---------------------- SIZE & FIELDOFS routines ------------------------

PROCEDURE mk_align(VAR offs: LONGINT; align: SHORTINT; f : pc.OBJECT);
  VAR r: LONGINT;
BEGIN
  IF align > default_alignment THEN
    env.errors.Warning(f.pos, 325, f.name^, align, default_alignment);
  END;
  r := offs MOD align;
  IF r # 0 THEN INC(offs, align-r) END;
END mk_align;

PROCEDURE ^ bytes(t: pc.STRUCT; VAR sz : LONGINT; VAR align : SHORTINT);

VAR fld_OfsObject : pc.OBJECT;
    fld_found     : BOOLEAN;
    fld_Offset    : LONGINT;
    Size_Undefined: BOOLEAN;

PROCEDURE alloc_field(f : pc.OBJECT;
               VAR offs : LONGINT;
              VAR align : SHORTINT);
  VAR field_size: LONGINT; field_align: SHORTINT;
BEGIN
  IF ~cc.op_gensize THEN
    Size_Undefined := TRUE;
    RETURN;
  END;
  field_align := align;
  bytes(f.type, field_size, field_align);
  IF Size_Undefined THEN RETURN; END;
  IF field_align > align THEN
    env.errors.Warning(f.pos, 324, f.name^, field_align, align);
  ELSIF field_align < align THEN
    align := field_align;
  END;
  IF offs <= MAX(LONGINT) - field_align THEN
    mk_align(offs, field_align, f);
  ELSE
    Size_Undefined := TRUE;
    RETURN;
  END;
  IF f = fld_OfsObject THEN
    fld_found := TRUE;
    fld_Offset := offs;
  END;
  IF offs <= MAX(LONGINT) - field_size THEN
    INC(offs, field_size);
  ELSE
    Size_Undefined := TRUE;
    RETURN;
  END;
END alloc_field;

PROCEDURE alloc_flist (f : pc.OBJECT;
                VAR offs : LONGINT;
               VAR align : SHORTINT);
  VAR n : pc.NODE;
    mx_size, l_size : LONGINT;
    list_align, mx_align, l_align : SHORTINT;
BEGIN
  list_align := 1;
  WHILE f # NIL DO
    IF f.attr # NIL THEN
        mx_size := rec.Bytes(f);
        ASSERT(mx_size > 0);
        IF f = fld_OfsObject THEN
          fld_found := TRUE;
          fld_Offset := offs;
        END;
        INC(offs, mx_size);
    ELSIF f.mode = pc.ob_field THEN
      l_align := align;
      alloc_field(f, offs, l_align);
      IF Size_Undefined THEN RETURN; END;
      IF l_align > list_align THEN list_align := l_align END;
    ELSIF f.mode = pc.ob_field_bts THEN
      Size_Undefined := TRUE;
      RETURN;
    ELSIF f.mode = pc.ob_header THEN
      IF f.val.obj # NIL THEN
        l_align := align;
        alloc_field(f.val.obj, offs, l_align);
        IF Size_Undefined THEN RETURN; END;
        IF l_align > list_align THEN list_align := l_align END;
      END;
      mx_size := 0;
      mx_align := 1;
      n := f.val.l;
      WHILE n # NIL DO
        ASSERT(n^.mode = pc.nd_node);
        l_size := 0; l_align := align;
        alloc_flist(n.obj, l_size, l_align);
        IF Size_Undefined THEN RETURN; END;
        IF l_size > mx_size THEN mx_size := l_size END;
        IF l_align > mx_align THEN mx_align := l_align END;
        n := n.next;
      END;
      IF offs <= MAX(LONGINT) - mx_align THEN
        mk_align(offs, mx_align, f);
      ELSE
        Size_Undefined := TRUE;
        RETURN;
      END;

      IF offs <= MAX(LONGINT) - mx_size THEN
        INC(offs, mx_size);
      ELSE
        Size_Undefined := TRUE;
        RETURN;
      END;
      IF mx_align > list_align THEN list_align := mx_align END;
    ELSE
      ASSERT(FALSE); ---  invalid object mode
    END;
    f := f.next;
  END;
  align := list_align;
END alloc_flist;

PROCEDURE rec_size0 (t : pc.STRUCT; VAR size : LONGINT; VAR align : SHORTINT);
VAR
  base: pc.STRUCT;
  t_align, fl_align, b_align : SHORTINT;
BEGIN
(* -- IF pc.ttag_packed IN t.tags THEN ... END; *)
  size := 0;
  b_align := -1;
  base := t.base;
  t_align := t.align;
  IF t_align = 0 THEN t_align := align END;
  IF base # NIL THEN
    b_align := t_align;
    rec_size0(base, size, b_align);
    IF Size_Undefined THEN RETURN; END;
    ASSERT (size # 0);
  END;
  fl_align := t_align;
  alloc_flist(t.prof, size, fl_align);
  IF Size_Undefined THEN RETURN; END;
  IF (base # NIL) & (b_align > fl_align) THEN
    align := b_align;
  ELSE
    align := fl_align;
  END;
  IF size = 0 THEN size := 4 END;
END rec_size0;

PROCEDURE record_size (t : pc.STRUCT;
                VAR size : LONGINT; VAR align : SHORTINT);
BEGIN
  rec_size0(t, size, align);
  IF Size_Undefined THEN RETURN; END;
  ASSERT (size # 0);
  mk_align(size, align, t.obj);
END record_size;

PROCEDURE bytes(t: pc.STRUCT; VAR sz : LONGINT; VAR align : SHORTINT);
  VAR n_align, t_align : SHORTINT;
BEGIN
  IF NOT cc.op_gensize &
     (t.mode IN pc.TY_SET{pc.ty_real, pc.ty_longreal,
                          pc.ty_complex, pc.ty_lcomplex,
                          pc.ty_protection, pc.ty_opaque, pc.ty_pointer,
                          pc.ty_proctype})
  THEN
    Size_Undefined := TRUE;
    RETURN;
  END;
  t_align := t.align;
  IF t_align = 0 THEN t_align := align END;
  CASE t.mode OF
  | pc.ty_shortcard, pc.ty_shortint :
      sz      := 1;
      n_align := 1;
  | pc.ty_cardinal, pc.ty_integer :
      sz      := 2;
      n_align := 2;
  | pc.ty_longcard, pc.ty_longint :
      sz      := 4;
      n_align := 4;
  | pc.ty_longlongint, pc.ty_longlongcard :
      sz      := 8;
      n_align := 8;
  | pc.ty_real :
      sz      := 4;
      n_align := 4;
  | pc.ty_longreal :
      sz      := 8;
      n_align := 8;
  | pc.ty_complex :
      sz      := 8;
      n_align := 4;
  | pc.ty_lcomplex :
      sz      := 16;
      n_align := 8;
  | pc.ty_boolean, pc.ty_range, pc.ty_enum :
      n_align := t_align;
      bytes(t.base, sz, n_align);
  | pc.ty_protection :
      sz      := 2;
      n_align := 2;
  | pc.ty_char :
      sz      := 1;
      n_align := 1;
  | pc.ty_opaque, pc.ty_pointer :
      sz      := 4;
      n_align := 4;
  | pc.ty_set :
      IF t.inx#NIL THEN
        n_align := t_align;
        bytes(t.inx, sz, n_align);
      ELSIF dcl.is_c_arr(t) THEN
        sz      := (t.len+dcl.LSET_BITS-1) DIV dcl.LSET_BITS * 
                                                 (dcl.LSET_BITS DIV 8);
        n_align := VAL(SHORTINT, (dcl.LSET_BITS DIV 8));
      ELSIF t.len <= 8 THEN
        sz      := 1;
        n_align := 1;
      ELSIF t.len <= 16 THEN
        sz      := 2;
        n_align := 2;
      ELSE
        sz      := 4;
        n_align := 4;
      END;
  | pc.ty_proctype :
      sz      := 4;
      n_align := 4;
  | pc.ty_array :
      bytes(t.base, sz, align);
      IF Size_Undefined THEN RETURN; END;
      IF t.len <= MAX(LONGINT) DIV sz THEN
        sz := sz * t.len;
      ELSE
        Size_Undefined := TRUE;
      END;
      RETURN;
  | pc.ty_record :
      record_size(t, sz, align); RETURN
  | pc.ty_loc :
      sz      := 1;
      n_align := 1;
  | pc.ty_SS :
      sz      := t.len;
      n_align := 1;
  ELSE
    Size_Undefined := TRUE;
    RETURN;
  END;
  IF t_align > n_align THEN t_align := n_align END;
  IF t_align < align   THEN align   := t_align END;
END bytes;

PROCEDURE (c: CODE) get_offs(op: pc.SUB_MODE; o: pc.OBJECT): LONGINT;
  VAR sz: LONGINT; align : SHORTINT;
BEGIN
  ASSERT( o.mode=pc.ob_field );   --- Only for FIELDOFS
  ASSERT( o.host # NIL );
  IF cc.op_gencnsexp THEN RETURN -1; END;
  fld_found     := FALSE;
  fld_OfsObject := o;
  fld_Offset    := -1;
  Size_Undefined := FALSE;
  bytes(o.host, sz, align);
  IF Size_Undefined THEN RETURN -1; END;
  ASSERT(fld_found=TRUE);
  ASSERT(fld_Offset >= 0);
  CASE op OF
    | pc.su_bit_offs : RETURN fld_Offset*8;
    | pc.su_byte_offs: RETURN fld_Offset;
    | pc.su_word_offs: RETURN (fld_Offset+3) DIV 4;
  END;
END get_offs;

PROCEDURE (c: CODE) get_size(op: pc.SUB_MODE; t: pc.STRUCT): LONGINT;
  VAR sz: LONGINT; align : SHORTINT;
BEGIN
  align := default_alignment;
  Size_Undefined := FALSE;
  bytes(t, sz, align);
  IF Size_Undefined THEN RETURN -1; END;
  ASSERT(sz > 0);
  CASE op OF
    |pc.su_bits : RETURN sz*8;
    |pc.su_bytes: RETURN sz;
    |pc.su_size : RETURN sz;
    |pc.su_words: RETURN (sz+3) DIV 4;
  END;
END get_size;

----------------------------------------------------------------------

PROCEDURE Set*;
  VAR code: CODE;
BEGIN
  NEW(code);
  pc.code:=code;
  code.inited:=FALSE;
  code.sym_ident:=pc.sym_C;
  code.valid_idents:={pc.sym_C - pc.sym_base};

  COPY(cc.vers,code.vers);
  cc.DeclareOptions;
  code.selected;
END Set;

BEGIN
  zz_tmp:=NIL;
  zz_gcr:=NIL;
  zz_abs_stp:=NIL;
  zz_one:=NIL;
  def.gen_sequence:=gen_sequence;
END ccCode.
