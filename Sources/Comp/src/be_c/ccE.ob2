(** Copyright (c) 1993,97 XDS Ltd, Russia. All Rights Reserved. *)
(** Expressions *)
MODULE ccE; (** Sem 22-Sep-93. *)

(* Modifications:
   17/Mar/96 Ned  2.12  BNRP specific code is removed: GENNOBITREF,
                        bnrp_set_aggregate, set operations
   24-Mar-96 Ned  <*IF extvalue*> is deleted.
   24-Mar-96 Ned  gen_value_unary_conv: ty_SS -> ordinal (for GENCNSEXP)
   27-Mar-96 Ned  PRO0046: gen_assign.
                  is_string is used instead of rem_str_extension
   29-Mar-96 Ned  back to rem_str_extension (see fix in pcConst)
   09-Sep-99 AlexM SYSTEM.FIELDOFS support is added to ccE.ob2 & ccCode.ob2
*)

IMPORT
  pc :=pcK,
  cc :=ccK,
  nms:=ccN,
  out:=ccL,
  dcl:=ccDcl,
  xfs:=xiFiles,
  pcO,   (* patch, will go away in XDS 3.0 *)
  <* IF pcvis THEN *> pcVis, <* END *>
  env:=xiEnv,
  SYSTEM;

TYPE
  STR = dcl.STR;

VAR
  zz_tmp     : pc.VALUE;
  zz_128     : pc.VALUE;
  zz_m32     : pc.VALUE;
  zz_32      : pc.VALUE;
  zz_one     : pc.VALUE;
  zz_gvm     : pc.VALUE; (* is used in gen_value_minus_min *)


CONST
  len_avl=pc.TY_SET{pc.ty_array,pc.ty_SS,pc.ty_set};

PROCEDURE ^ gen_value(n: pc.NODE; p: INTEGER; md: SET);
PROCEDURE ^ gen_size(n: pc.NODE; dim: INTEGER; p: INTEGER);

PROCEDURE is_result_of_cptr_deref(l: pc.NODE): BOOLEAN;
BEGIN
  WHILE (l.mode = pc.nd_index) & (l.l.type.mode = pc.ty_array_of) DO l:= l.l END;
  RETURN (l.mode = pc.nd_deref) & (l.l.type.flag = pc.flag_c);
END is_result_of_cptr_deref;

PROCEDURE gen_type_cast(bs-: ARRAY OF CHAR; t: pc.STRUCT);
  VAR str: STR;
BEGIN
  dcl.type_designator(str,bs,t);
  out.wr('('); out.ws(str); out.wr(')');
END gen_type_cast;

PROCEDURE val_node*(v: pc.VALUE; const: BOOLEAN): pc.NODE;
  VAR l: pc.NODE;
BEGIN
  IF ~ cc.op_gencnsexp OR (v.expr=NIL) THEN RETURN NIL END;
  l:=v.expr;
(*
IF const THEN env.info.print("const:\n");
ELSE env.info.print("var:\n");
END;
pcVis.vis(l,0);
*)
  (* problems with ty_CC, ty_RR *)
  IF l.type.mode IN (pc.CPLXs+pc.REALs) THEN RETURN NIL END;

  IF const & (l.type.mode IN pc.TY_SET{pc.ty_array,pc.ty_record}) THEN
    RETURN NIL;
  END;

  IF const & (l.mode=pc.nd_unary) THEN
    IF l.sub=pc.su_conv THEN
      (* C++ not allows convertion to float in constant expression *)
      IF l.type.mode IN pc.REALs THEN RETURN NIL END;
      IF l.l.type.mode IN pc.REALs THEN RETURN NIL END;
      IF dcl.is_c_num(l.type) &
         dcl.is_c_num(l.l.type) &
         (l.l.type.mode#pc.ty_char)
      THEN
        RETURN l.l
      END;
      RETURN NIL;
    END;
    IF l.sub=pc.su_length THEN RETURN NIL END;
    IF l.sub=pc.su_abs    THEN RETURN NIL END;
  END;
  IF l.mode=pc.nd_aggregate THEN RETURN NIL END;

  (* indexation and field access are not constant expression (MSVC) *)
  IF const & (l.mode=pc.nd_index) THEN RETURN NIL END;
  IF const & (l.mode=pc.nd_field) THEN RETURN NIL END;

  IF l.mode=pc.nd_binary THEN
    CASE l.sub OF
      |pc.sb_concat:
        IF (l.l.mode#pc.nd_value) OR
           (l.r.mode#pc.nd_value) OR
           (l.l.val.node()#NIL) OR
           (l.r.val.node()#NIL) OR
           ~cc.op_cpp
        THEN RETURN NIL;
        END;
      |pc.sb_lsh:
        IF const THEN RETURN NIL END;
    ELSE
    END;
  END;
  RETURN l;
END val_node;

PROCEDURE get_sequence_len(n: pc.NODE): LONGINT;
  VAR l: pc.NODE; sz: LONGINT;
BEGIN
  l:=n.l; sz:=0;
  WHILE l#NIL DO
    IF l.type.mode IN pc.REALs THEN INC(sz,2)
    ELSIF (l.type.mode IN pc.SEQ_ARRs) & ~is_result_of_cptr_deref(l) THEN INC(sz,3)
    ELSE INC(sz,1)
    END;
    l:=l.next;
  END;
  RETURN sz;
END get_sequence_len;

PROCEDURE gen_a_chk(n: pc.NODE);
  VAR nm: STR;
BEGIN
  ASSERT(n.type.mode IN pc.ADRs);
  nms.x2c(nms.nm_a_chk,nm);
  out.ws(nm); out.wr('(');
  dcl.type_designator(nm,"",n.type);
  out.ws(nm); out.wr(',');
  WHILE (n.mode=pc.nd_unary) &
        (n.sub=pc.su_conv) &
        (n.l.type.mode IN pc.ADRs)
  DO n:=n.l;
  END;
  gen_value(n,0,{}); out.wr(')');
END gen_a_chk;

PROCEDURE gen_len_usage(ps: pc.TPOS; o: pc.OBJECT;
			dim: LONGINT; type: pc.STRUCT);
  VAR t: pc.STRUCT; nm: STR; i: LONGINT;
BEGIN
  t:=o.type;
  FOR i:=0 TO dim-1 DO ASSERT(t.mode IN pc.ARRs); t:=t.base END;
  IF t.mode IN len_avl THEN
    dcl.c_number(t.len,type,nm); out.ws(nm);
    RETURN;
  END;
  IF t.mode=pc.ty_array_of THEN
    ASSERT(o.type.mode=pc.ty_array_of);
    IF o.type.flag IN cc.c_like_flags THEN
      env.errors.Error(ps,1006,dim);
    ELSE
      dcl.o_second_name(o,SHORT(dim),"_len",nm);
      out.ws(nm);
    END;
    RETURN;
  END;
  env.errors.Error(ps,1006,dim);
END gen_len_usage;

PROCEDURE gen_len*(n: pc.NODE; dim: LONGINT; type: pc.STRUCT; p: INTEGER);
(* dim номер индексации, слева направо *)
  VAR t,f: pc.STRUCT; i,j: LONGINT; nm: STR;
BEGIN
  t:=n.type;
  FOR i:=0 TO dim-1 DO ASSERT(t.mode IN pc.ARRs); t:=t.base END;
  IF t.mode IN len_avl THEN
    dcl.c_number(t.len,type,nm); out.ws(nm);
    RETURN;
  END;
  IF t.mode=pc.ty_array_of THEN
    CASE n.mode OF
      |pc.nd_var:
        gen_len_usage(n.pos,n.obj,dim,type);
	RETURN;
      |pc.nd_deref:
        IF n.l.type.flag = pc.flag_c THEN env.errors.Error(n.pos,142) END;
        dcl.t_def(n.type);
        IF pc.ntag_chk_range IN n.tags THEN gen_a_chk(n.l);
        ELSE gen_value(n.l,14,{});
	END;
	nms.x2c(nms.nm_dynarr_len,nm);
	out.wf("->%s%d",nm,n.type.len-dim-1);
	RETURN;
      |pc.nd_index:
        gen_len(n.l,dim+1,type,p);
	RETURN;
      |pc.nd_unary,pc.nd_lconv:
	IF (n.mode=pc.nd_lconv) OR (n.sub=pc.su_conv) THEN
	  (* для выходного открытого массива для всех его измерений
	     кроме последнего должны быть соответствующие измерения
	     в операнде операции преобразования, длина по этим
	     измерениям совпадает с исходной длинной. Длина по
	     последнему измерению перевычисляется.
	  *)
	  f:=n.l.type;
	  FOR i:=0 TO dim-1 DO ASSERT(f.mode IN pc.ARRs); f:=f.base END;
	  IF (t.len>1) OR (f.mode IN pc.ARRs) & (f.base=t.base) THEN
            gen_len(n.l,dim,type,p);
          ELSIF (f.mode=pc.ty_array_of) OR ~cc.op_gensize THEN
	    IF p>=12 THEN out.wr('(') END;
	    gen_size(n.l,SHORT(dim),12);
	    out.wr('/');
	    dcl.gen_sizeof(t.base);
	    IF p>=12 THEN out.wr(')') END;
	  ELSE
	    i:=dcl.get_bytes(n.pos,f);
	    j:=dcl.get_bytes(n.pos,t.base);
	    dcl.c_number(i DIV j,type,nm); out.ws(nm);
	    IF (i MOD j)#0 THEN env.errors.Error(n.pos,1006,dim) END;
	  END;
	  RETURN;
	END;
      |pc.nd_sequence:
	IF p>=12 THEN out.wr('(') END;
	i:=get_sequence_len(n);
	IF i#1 THEN dcl.c_number(i,type,nm); out.ws(nm) END;
	IF i#0 THEN
	  IF i#1 THEN out.wr('*') END;
	  out.ws("sizeof(X2C_SEQ)");
	  j:=dcl.get_bytes(n.pos,n.type.base);
	  IF j#1 THEN
	    out.wr('/'); dcl.c_number(j,type,nm); out.ws(nm);
	  END;
	END;
	IF p>=12 THEN out.wr(')') END;
	RETURN;
    ELSE (* nothing *)
    END;
  END;
  env.errors.Error(n.pos,1006,dim);
END gen_len;

PROCEDURE gen_type_name(t: pc.STRUCT; p: INTEGER);
  VAR nm: STR;
BEGIN
  ASSERT(t.mode=pc.ty_record);
  ASSERT(t.flag=pc.flag_o2);
  IF p>13 THEN out.wr('(') ELSIF p=13 THEN out.wr(' ') END;
  dcl.o_second_name(t.obj,1,"_desc",nm);
  out.wr('&'); out.ws(nm);
  IF p>13 THEN out.wr(')') END;
END gen_type_name;

PROCEDURE gen_type(n: pc.NODE; p: INTEGER);
  VAR nm: STR; t: pc.STRUCT;
BEGIN
  t:=n.dynamic_type();
  IF t#NIL THEN gen_type_name(t,p); RETURN END;
  n:=n.dynamic_type_expr();
  IF n.type.mode=pc.ty_pointer THEN
    out.ws("X2C_GET_TD("); gen_value(n,0,{}); out.ws(")");
  ELSIF (n.mode=pc.nd_var) & (n.obj.mode=pc.ob_varpar) THEN
    dcl.o_second_name(n.obj,0,"_type",nm); out.ws(nm);
  ELSE
    ASSERT(FALSE);
  END;
END gen_type;

PROCEDURE gen_size_usage*(o: pc.OBJECT; dim: INTEGER; p: INTEGER);
  VAR nm: STR; t: pc.STRUCT; i: INTEGER; sz: LONGINT;
BEGIN
  IF p>=12 THEN out.wr('(') END;
  ASSERT(o.type.mode=pc.ty_array_of);
  t:=o.type; i:=0;
  WHILE i<dim DO ASSERT(t.mode IN pc.ARRs); t:=t.base; INC(i) END;
  WHILE t.mode=pc.ty_array_of DO
    dcl.o_second_name(o,i,"_len",nm);
    IF i>dim THEN out.wr('*') END;
    out.ws(nm); t:=t.base; INC(i);
  END;
  sz:=pc.code.get_size(pc.su_size,t);
  IF (sz#1) OR (i=dim) THEN
    IF i>dim THEN out.wr('*') END;
    dcl.gen_sizeof(t);
  END;
  IF p>=12 THEN out.wr(')') END;
END gen_size_usage;

PROCEDURE gen_size*(n: pc.NODE; dim: INTEGER; p: INTEGER);
  (* dim - количество индексаций *)
  VAR t: pc.STRUCT; i: INTEGER; nm: STR;
BEGIN
  t:=n.type;
  FOR i:=0 TO dim-1 DO ASSERT(t.mode IN pc.ARRs); t:=t.base END;
  IF t.mode#pc.ty_array_of THEN dcl.gen_sizeof(t); RETURN END;
  IF dim<n.type.len THEN
    CASE n.mode OF
      |pc.nd_deref:
	IF dim=0 THEN
	  IF p>=12 THEN out.wr('(') END;
          gen_len(n,0,pcO.size_t,12); out.wr('*'); gen_size(n,1,12);
	  IF p>=12 THEN out.wr(')') END;
	ELSE
	  dcl.t_def(n.type);
	  IF pc.ntag_chk_range IN n.tags THEN gen_a_chk(n.l);
	  ELSE gen_value(n.l,14,{});
	  END;
	  out.ws("->");
	  nms.x2c(nms.nm_dynarr_size,nm);
	  out.wf("%s%d",nm,n.type.len-dim);
	END;
	RETURN;
      |pc.nd_index:
	gen_size(n.l,dim+1,p);
	RETURN;
      |pc.nd_var:
	gen_size_usage(n.obj,dim,p);
	RETURN;
    ELSE (* nothing *)
    END;
  END;
  env.errors.Error(n.pos,1007,dim);
END gen_size;

PROCEDURE gen_field_ofs(n: pc.NODE);
  VAR s, fld_ofs : STR;
      sz : LONGINT;
BEGIN
  IF(n.mode # pc.nd_field) THEN
     env.errors.Error(n.pos,1008);
     RETURN;
  END;
  ASSERT(n.obj.mode = pc.ob_field);
  IF cc.op_gencnsexp THEN sz := -1;
  ELSE sz:=pc.code.get_offs(pc.su_bytes,n.obj);
  END;
  IF sz<0 THEN
    dcl.type_designator(s,"",n.obj.host);
    nms.x2c(nms.nm_field_ofs,fld_ofs);
    out.wf("%s(%s,%s)",fld_ofs, s, n.obj.name^);
  ELSE
    dcl.c_number(sz,pcO.size_t,s);
    out.ws(s);
  END;
END gen_field_ofs;

PROCEDURE gen_value_usage_conv(o: pc.OBJECT; p: INTEGER; adr,ref,lvl: BOOLEAN);
  VAR nm: STR;
BEGIN
  IF cc.otag_notype IN o.tags THEN
    IF p>13 THEN out.wr('(') END;
    IF ref THEN
      IF    adr THEN gen_type_cast("*",o.type);
      ELSIF lvl THEN out.wr("*"); gen_type_cast("*",o.type);
      ELSE           gen_type_cast("",o.type); out.wr("*");
      END;
    ELSE
      IF    adr THEN gen_type_cast("*",o.type); out.wr("&");
      ELSIF lvl THEN out.wr("*"); gen_type_cast("*",o.type); out.wr("&");
      ELSE           gen_type_cast("",o.type);
      END;
    END;
    dcl.o_usage(o,nm); out.ws(nm);
    IF p>13 THEN out.wr(')') END;
  ELSE
    IF adr THEN
      IF p>13 THEN out.wr('(')
      ELSIF p=13 THEN out.wr(' ')
      END;
      out.wr('&'); dcl.o_usage(o,nm); out.ws(nm);
      IF p>13 THEN out.wr(')') END;
    ELSE
      dcl.o_usage(o,nm); out.ws(nm);
    END;
  END;
END gen_value_usage_conv;

PROCEDURE gen_value_usage(o: pc.OBJECT; p: INTEGER; md: SET);
  VAR bf: STR;
BEGIN
  ASSERT(~(dcl.NEG IN md));
  IF cc.op_cpp & (pc.otag_with IN o.tags) THEN
    ASSERT(o.type.mode=pc.ty_pointer);
    ASSERT(~dcl.is_pointer(o,dcl.cur_proc));
    ASSERT(~(dcl.REF IN md));
    ASSERT(~(cc.otag_notype IN o.tags));
    IF p>13 THEN out.wr('(');
    ELSIF p=13 THEN out.wr(' ');
    END;
    out.wr('&'); dcl.o_usage(o,bf); out.ws(bf);
    IF p>13 THEN out.wr(')') END;
  ELSIF dcl.is_pointer(o,dcl.cur_proc) THEN
    IF dcl.REF IN md THEN
      gen_value_usage_conv(o,p,FALSE,TRUE,dcl.LVL IN md);
    ELSE
      IF p>13 THEN out.wr('(') ELSIF p=12 THEN out.wr(' ') END;
      out.wr('*'); gen_value_usage_conv(o,13,FALSE,TRUE,dcl.LVL IN md);
      IF p>13 THEN out.wr(')') END;
    END;
  ELSIF dcl.is_c_arr(o.type) THEN
    (* для C ARRs всегда ганерится BSA, ничего другого сгенерить нельзя *)
    ASSERT((dcl.REF IN md) & (dcl.ANY IN md) OR (dcl.BSA IN md));
    ASSERT(~(cc.otag_notype IN o.tags));
    dcl.o_usage(o,bf);
    IF (cc.otag_bitfield IN o.tags) & (dcl.REF IN md) THEN
      IF p>13 THEN out.wr('(') ELSIF p=13 THEN out.wr(' ') END;
      out.wr('&'); out.ws(bf);
      IF p>13 THEN out.wr(')') END;
    ELSE
      out.ws(bf);
    END;
  ELSIF dcl.REF IN md THEN
    gen_value_usage_conv(o,p,TRUE,FALSE,dcl.LVL IN md);
  ELSE
    gen_value_usage_conv(o,p,FALSE,FALSE,dcl.LVL IN md);
  END;
END gen_value_usage;

PROCEDURE gen_value_not_char(n: pc.NODE; p: INTEGER; md: SET);
BEGIN
  IF ~dcl.is_char(n.type) THEN
    gen_value(n,p,md);
  ELSIF (n.mode=pc.nd_value) & dcl.cmp_value(pc.sb_lss,n.val,zz_128) THEN
    gen_value(n,p,md);
  ELSE
    ASSERT(md*{dcl.REF,dcl.LVL}={});
    IF p>13 THEN out.wr('(') END;
    gen_type_cast("",n.type.super_type());
    gen_value(n,13,md);
    IF p>13 THEN out.wr(')') END;
  END;
END gen_value_not_char;

PROCEDURE gen_p_chk(n: pc.NODE);
  VAR nm: STR;
BEGIN
  ASSERT(n.type.mode IN pc.TY_SET{pc.ty_proctype});
  nms.x2c(nms.nm_p_chk,nm);
  out.ws(nm); out.wr('(');
  dcl.type_designator(nm,"",n.type);
  out.ws(nm); out.wr(',');
  WHILE (n.mode=pc.nd_unary) &
        (n.sub=pc.su_conv) &
        (n.l.type.mode IN pc.TY_SET{pc.ty_proctype})
  DO n:=n.l;
  END;
  gen_value(n,0,{}); out.wr(')');
END gen_p_chk;

PROCEDURE gen_call*(n: pc.NODE; p: INTEGER; md: SET);
  VAR
    t      : pc.STRUCT;
    rt,db  : BOOLEAN;
    tmp_nm : STR;
    l,ll   : pc.NODE;
    i,j    : LONGINT;
    nm     : STR;
    prm_buf: dcl.PRM_BUF;
    prm_cnt: INTEGER;
    fst    : BOOLEAN;
    this   : pc.NODE;
BEGIN
  IF n.l#NIL THEN t:=n.l.type ELSE t:=n.obj.type END;
  rt:=dcl.is_c_arr(t.base);
  (* rt=TRUE если результат возвращается через дополнительный параметр *)
  ASSERT((dcl.REF IN md) & (dcl.ANY IN md) OR (dcl.BSA IN md) OR ~rt);
  ASSERT(~(dcl.LVL IN md));
  IF (dcl.REF IN md) OR rt THEN
    dcl.make_temp_var(t.base,tmp_nm);
    IF ~rt THEN
      IF p>=0 THEN out.wr('(') END;
      out.wf("%s = ",tmp_nm);
    END;
  ELSE
    tmp_nm:="";
  END;
  db:=cc.op_debug;
  IF (n.l#NIL) & (n.l.mode#pc.nd_method) & (pc.ntag_chk_range IN n.tags) THEN
    db:=FALSE;
  END;
  IF db THEN out.ws("(X2C_SET_HINFO() ") END;
  this:=NIL;
  IF n.l=NIL THEN
    dcl.o_usage(n.obj,nm); out.ws(nm);
  ELSIF n.l.mode=pc.nd_method THEN
    this:=n.l.l;
    dcl.o_second_name(t.obj,1,"_",nm);
    out.wf("X2C_CALL(%s,",nm);
    gen_type(this,0);
    out.wf(",%d)",t.len);
  ELSIF pc.ntag_chk_range IN n.tags THEN
    gen_p_chk(n.l);
  ELSE
    gen_value(n.l,14,{});
  END;
  out.wr('(');
  dcl.func_params(prm_buf,prm_cnt,t);
  l:=n.r; ll:=NIL; fst:=TRUE; j:=-1;
  FOR i:=0 TO prm_cnt-1 DO
    CASE prm_buf[i].mode OF
      |dcl.pm_return :
        ASSERT(tmp_nm#"");
	IF fst THEN fst:=FALSE ELSE out.ws(", ") END;
	out.ws(tmp_nm);
      |dcl.pm_threat :
	IF fst THEN fst:=FALSE ELSE out.ws(", ") END;
	gen_value_usage(prm_buf[i].obj,0,prm_buf[i].ref); j:=0;
      |dcl.pm_param  :
	IF fst THEN fst:=FALSE ELSE out.ws(", ") END;
	IF this#NIL THEN ll:=this; this:=NIL ELSE ll:=l; l:=l.next END;
        IF cc.otag_notype IN prm_buf[i].obj.tags THEN
	  IF dcl.REF IN prm_buf[i].ref THEN
	    dcl.type_designator_notype(nm,"*",prm_buf[i].obj.type);
	  ELSIF dcl.LVL IN prm_buf[i].ref THEN
	    dcl.type_designator_notype(nm,"*",prm_buf[i].obj.type);
            out.wr('*'); INCL(prm_buf[i].ref,dcl.REF);
	  ELSE
	    dcl.type_designator_notype(nm,"",prm_buf[i].obj.type);
	  END;
	  out.wf("(%s)",nm);
	  gen_value(ll,13,prm_buf[i].ref);
(* слишком грязное решение
	ELSIF (t.flag=pc.flag_c) &
	      (ll.type.mode=pc.ty_pointer) &
	      (ll.type.base.mode=pc.ty_loc) &
              ~cc.op_cpp
	THEN
	  (* 	подавление контроля типов для рараметров типа ADDRESS,
		в C++ так делать нельзя!
	  *)
	  out.ws("(void * )");
	  gen_value(ll,13,prm_buf[i].ref);
*)
	ELSE
	  gen_value(ll,0,prm_buf[i].ref);
	END;
	j:=0;
      |dcl.pm_len    :
	ASSERT(j>=0);
        out.ws(", "); gen_len(ll,j,prm_buf[i].obj.type.inx,0); INC(j);
      |dcl.pm_frec   :
	IF fst THEN fst:=FALSE ELSE out.ws(", ") END;
	IF this#NIL THEN ll:=this; this:=NIL ELSE ll:=l; l:=l.next END;
	gen_value(ll,0,prm_buf[i].ref);
      |dcl.pm_type    :
	out.ws(", "); gen_type(ll,0);
      |dcl.pm_thr_len:
	IF fst THEN fst:=FALSE ELSE out.ws(", ") END;
	dcl.o_second_name(prm_buf[i].obj,SHORT(j),"_len",nm);
	out.ws(nm); INC(j);
      |dcl.pm_thr_type:
	IF fst THEN fst:=FALSE ELSE out.ws(", ") END;
	dcl.o_second_name(prm_buf[i].obj,0,"_type",nm);
	out.ws(nm);
      |dcl.pm_seq:
	ASSERT(l.next=NIL);
	ASSERT(l.type.mode=pc.ty_array_of);
	ll:=l.l;
	WHILE ll#NIL DO
	  IF fst THEN fst:=FALSE ELSE out.ws(", ") END;
	  IF dcl.is_c_arr(ll.type) THEN
	    gen_value(ll,0,{dcl.REF,dcl.ANY,dcl.LVL});
	  ELSE
	    gen_value_not_char(ll,0,{});
	  END;
	  ll:=ll.next;
	END;
    END;
  END;
  out.wr(")");
  IF db THEN out.wr(")") END;
  IF (dcl.REF IN md) & ~rt THEN
    out.wf(",&%s",tmp_nm);
    IF p>=0 THEN out.wr(')') END;
  END;
END gen_call;

PROCEDURE gen_call_storage*(n: pc.NODE);
  VAR
    ntype  : pc.STRUCT; (* pointer base type *)
    btype  : pc.STRUCT; (* dyn. array base type *)

  PROCEDURE prm_ptr(ptr: BOOLEAN);
    VAR nm: STR;
  BEGIN
    nms.x2c(nms.nm_AA,nm);
    IF ~ptr THEN out.wr('*') END;
    out.wr('('); out.ws(nm); out.ws('*)');
(*    IF ntype.mode=pc.ty_array_of THEN
      out.wr('&'); gen_value(n.r,13,{dcl.LVL});
    ELSE *)
      gen_value(n.r,13,{dcl.REF,dcl.ANY,dcl.LVL});
(*    END; *)
  END prm_ptr;

  PROCEDURE prm_size(l: pc.NODE);
  BEGIN
    IF l=NIL THEN dcl.gen_sizeof(btype);
    ELSE gen_value(l,0,{});
    END;
  END prm_size;

  PROCEDURE prm_dims(t: pc.STRUCT);
    VAR nm: STR;
  BEGIN
    ASSERT(ntype.mode=pc.ty_array_of);
    dcl.c_number(ntype.len,t,nm); out.ws(nm);
  END prm_dims;

  PROCEDURE prm_lens(l: pc.NODE; type: pc.STRUCT);
    VAR nm: STR; k: LONGINT;
  BEGIN
    IF type=NIL THEN type:=pcO.size_t END;
    dcl.make_temp_arr(type,ntype.len,nm);
    k:=0; out.wr("(");
    REPEAT
      out.wf("%s[%d] = ",nm,k); INC(k);
      gen_value(l,1,{}); l:=l.next; out.wr(",");
    UNTIL l=NIL;
    out.ws(nm);
    out.wr(')');
  END prm_lens;
(*
  PROCEDURE prm_lens_cptr(l: pc.NODE);
  BEGIN
    gen_value(l,1,{}); l:=l.next;
    WHILE l # NIL DO
      out.wr("+");
      gen_value(l,1,{}); l:=l.next;
    END;
  END prm_lens_cptr;
*)

  PROCEDURE prm_type(sys: BOOLEAN);
    VAR b: pc.STRUCT;
  BEGIN
    b:=btype;
    WHILE b.mode IN pc.ARRs DO b:=b.base END;
    IF (b.flag#pc.flag_o2) OR sys THEN out.ws("x2c_td_null");
    ELSIF b.mode=pc.ty_pointer THEN out.ws("x2c_td_ptr");
    ELSE gen_type_name(b,0);
    END;
  END prm_type;

  VAR
    l      : pc.NODE;
    nm     : STR;
    i,j,k  : INTEGER;
    prm_buf: dcl.PRM_BUF;
    prm_cnt: INTEGER;
    o2,new,sys: BOOLEAN;
    lt     : pc.STRUCT;
BEGIN
  l:=n.r; j:=0;
  lt:=l.type;
  IF lt.mode=pc.ty_opaque THEN lt:=lt.base END;
  ntype:=lt.base; btype:=ntype;
  WHILE btype.mode=pc.ty_array_of DO btype:=btype.base END;
  ASSERT(lt.mode=pc.ty_pointer);
  o2 :=lt.flag=pc.flag_o2;
  new:=(n.sub=pc.sp_new) OR (n.sub=pc.sp_sysnew);
  sys:=n.sub=pc.sp_sysnew;
  IF n.obj=NIL THEN
    IF ntype.mode=pc.ty_array_of THEN
      IF lt.flag=pc.flag_c THEN
        IF new THEN
          nms.x2c(nms.nm_dyncallocate,nm); out.wf("%s(",nm);
          prm_ptr(TRUE); out.wr(',');
          prm_size(NIL); out.wr(',');
          prm_lens(n.r.next,NIL); out.wr(',');
          prm_dims(pcO.size_t); out.wr(')');

          (*
          prm_ptr(TRUE); out.wf(',(');
          prm_lens_cptr(n.r.next); out.wf(')*');
          prm_size(NIL);
          out.wr(')');
          *)
        ELSE
          nms.x2c(nms.nm_dyncdeallocate,nm); out.wf("%s(",nm);
          prm_ptr(TRUE); out.wr(')');
        END;
      ELSE
        IF new THEN
          IF o2 THEN
            nms.x2c(nms.nm_new_open,nm); out.wf("%s(",nm);
            prm_type(FALSE); out.wr(',');
          ELSE
            nms.x2c(nms.nm_dynallocate,nm); out.wf("%s(",nm);
          END;
          prm_ptr(TRUE); out.wr(',');
          prm_size(NIL); out.wr(',');
          prm_lens(n.r.next,NIL); out.wr(',');
          prm_dims(pcO.size_t);
          IF o2 THEN
            IF sys THEN out.ws(",1") ELSE out.ws(",0") END;
          END;
          out.wr(')');
        ELSIF o2 THEN
          nms.x2c(nms.nm_dispose,nm); out.wf("%s(",nm);
          prm_ptr(TRUE); out.wr(')');
        ELSE
          nms.x2c(nms.nm_dyndeallocate,nm); out.wf("%s(",nm);
          prm_ptr(TRUE); out.wr(')');
        END;
      END;
    ELSIF new THEN
      IF o2 THEN
	nms.x2c(nms.nm_new,nm); out.wf("%s(",nm);
        prm_type(sys & (n.r.next#NIL)); out.wr(',');
	prm_ptr(TRUE); out.wr(',');
	prm_size(n.r.next);
	IF sys THEN out.ws(",1") ELSE out.ws(",0") END;
	out.wr(')');
      ELSE
	nms.x2c(nms.nm_allocate,nm); out.wf("%s(",nm);
	prm_ptr(TRUE); out.wr(',');
	prm_size(n.r.next); out.wr(')');
      END;
    ELSIF o2 THEN
      nms.x2c(nms.nm_dispose,nm); out.wf("%s(",nm);
      prm_ptr(TRUE); out.wr(')');
    ELSE
      nms.x2c(nms.nm_deallocate,nm); out.wf("%s(",nm);
      prm_ptr(TRUE); out.wr(')');
    END;
  ELSE
    dcl.o_usage(n.obj,nm); out.ws(nm); out.wr("(");
    dcl.func_params(prm_buf,prm_cnt,n.obj.type); k:=-1;
    FOR i:=0 TO prm_cnt-1 DO
      IF i>0 THEN out.ws(", ") END;
      CASE prm_buf[i].mode OF
      |dcl.pm_threat :
	gen_value_usage(prm_buf[i].obj,0,prm_buf[i].ref); k:=0;
      |dcl.pm_thr_len:
        ASSERT(k>=0);
	dcl.o_second_name(prm_buf[i].obj,k,"_len",nm);
	out.ws(nm); INC(k);
      |dcl.pm_param  :
	CASE j OF
	  |0: prm_ptr(dcl.REF IN prm_buf[i].ref);
	  |1: prm_size(NIL); l:=l.next;
	  |2: IF new THEN prm_lens(l,prm_buf[i].obj.type.inx);
	      ELSE prm_dims(prm_buf[i].obj.type);
	      END;
          |3: prm_dims(prm_buf[i].obj.type);
	END;
	INC(j);
      |dcl.pm_len   :
	ASSERT(j=3);
	prm_dims(prm_buf[i].obj.type.inx);
      END;
    END;
    out.wr(")");
  END;
END gen_call_storage;

PROCEDURE gen_value_minus_min*(m: pc.NODE;
		p  : INTEGER;
		v  : pc.VALUE;
		i32: BOOLEAN);
  VAR n: pc.NODE; t: pc.STRUCT; nm: STR;
BEGIN
  ASSERT(m.type.is_ordinal());
  n:=m;
  WHILE (n.mode=pc.nd_unary) & (n.sub=pc.su_conv) & n.l.type.is_ordinal() DO
    n:=n.l;
  END;
  IF ~i32 & cc.op_index16 THEN
    t:=n.type.super_type();
    IF t.mode IN pc.TY_SET{pc.ty_longint,pc.ty_longcard} THEN
      IF p>13 THEN out.wr('(') END;
      nms.x2c(nms.nm_index,nm); out.wf("(%s)",nm);
      gen_value_minus_min(n,13,v,TRUE);
      IF p>13 THEN out.wr(')') END;
      RETURN;
    END;
  END;
  IF v.is_zero() THEN
    gen_value_not_char(n,p,{});
  ELSIF n.mode=pc.nd_value THEN
    zz_gvm.binary(pc.sb_minus,n.val,v);
    dcl.const_aggregate(zz_gvm,m.type,p,FALSE,FALSE);
  ELSIF v.is_neg() THEN
    IF p>=11 THEN out.wr('(') END;
    gen_value_not_char(n,11,{}); out.wr('+');
    zz_gvm.unary(pc.su_neg,v);
    dcl.const_aggregate(zz_gvm,m.type,11,FALSE,FALSE);
    IF p>=11 THEN out.wr(')') END;
  ELSE
    IF p>=11 THEN out.wr('(') END;
    gen_value_not_char(n,11,{}); out.wr('-');
    dcl.const_aggregate(v,m.type,11,FALSE,FALSE);
    IF p>=11 THEN out.wr(')') END;
  END;
END gen_value_minus_min;

PROCEDURE gen_value_binary(n: pc.NODE; p: INTEGER; md: SET);

  PROCEDURE binary(p1: INTEGER; s-: ARRAY OF CHAR; p2: INTEGER);
    VAR b: BOOLEAN; m: SET;
  BEGIN
    b:=(p>p1) OR (p=p1) & (p IN {1..2,8..12});
    IF b & (dcl.PLS IN md) THEN
      IF (p=11) & (n.sub=pc.sb_plus) THEN b:=FALSE END;
      IF (p=12) & (n.sub=pc.sb_mul)  THEN b:=FALSE END;
    END;
    m:=md*{dcl.CHK};
    IF (n.sub=pc.sb_plus) OR (n.sub=pc.sb_mul) THEN INCL(m,dcl.PLS) END;
    IF b THEN out.wr('(') END;
    gen_value(n.l,p1,m);
    out.ws(s);
    gen_value(n.r,p2,m);
    IF b THEN out.wr(')') END;
  END binary;

  PROCEDURE binary_bln(p1: INTEGER; s-: ARRAY OF CHAR);
  BEGIN
    IF p>=p1 THEN out.wr('(') END;
    gen_value(n.l,p1,md*{dcl.CHK}+{dcl.BLN});
    out.ws(s);
    gen_value(n.r,p1,md*{dcl.CHK}+{dcl.BLN});
    IF p>=p1 THEN out.wr(')') END;
  END binary_bln;

  PROCEDURE binary_cplx;
    VAR nm: STR;
  BEGIN
    nms.cplx_func(n.l.type,n.sub,nm); out.wf("%s(",nm);
    gen_value(n.l,0,{}); out.wr(','); gen_value(n.r,0,{}); out.wr(')');
  END binary_cplx;

  PROCEDURE binary_lset(s-: ARRAY OF CHAR);
    VAR nm: STR;
  BEGIN
    dcl.make_temp_var(n.type,nm);
    out.wf("%s(%s,",s,nm);
    gen_value(n.l,0,{dcl.BSA});
    out.wr(',');
    gen_value(n.r,0,{dcl.BSA});
    out.wf(",%d)",(n.type.len+dcl.LSET_BITS-1) DIV dcl.LSET_BITS);
  END binary_lset;

  PROCEDURE wr_cmp_sign;
  BEGIN
    IF dcl.NEG IN md THEN
      CASE n.sub OF
	|pc.sb_lss: out.ws(">=");
	|pc.sb_leq: out.ws(">");
	|pc.sb_gtr: out.ws("<=");
	|pc.sb_geq: out.ws("<");
      END;
    ELSE
      CASE n.sub OF
	|pc.sb_lss: out.ws("<");
	|pc.sb_leq: out.ws("<=");
	|pc.sb_gtr: out.ws(">");
	|pc.sb_geq: out.ws(">=");
      END;
    END;
  END wr_cmp_sign;

  PROCEDURE chk_neg(): BOOLEAN;
  BEGIN
    IF ~(dcl.NEG IN md) THEN RETURN TRUE END;
    IF p>13 THEN out.wr('(') END;
    out.wr('!'); gen_value_binary(n,13,md-{dcl.NEG}+{dcl.BLN});
    IF p>13 THEN out.wr(')') END;
    RETURN FALSE;
  END chk_neg;

  PROCEDURE is_empty_str(n: pc.NODE): BOOLEAN;
  BEGIN
    IF n.mode#pc.nd_value THEN RETURN FALSE END;
    zz_tmp.index_get(0,n.val);
    RETURN zz_tmp.is_zero();
  END is_empty_str;

  VAR t: pc.STRUCT; nm: STR; b: BOOLEAN; l: pc.NODE; i: LONGINT;
BEGIN
  ASSERT(md-{dcl.BLN,dcl.NEG,dcl.BSA,dcl.CCH,dcl.CNS,dcl.CHK,dcl.PLS}={});
  ASSERT(~(dcl.NEG IN md) OR (n.type.mode=pc.ty_boolean));
  CASE n.sub OF
    |pc.sb_equ,pc.sb_neq:
      IF (dcl.BLN IN md) &
	 (n.r.mode=pc.nd_value) &
	 (n.r.type.is_ordinal() OR
          (n.r.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque})) &
	 n.r.val.is_zero() &
         ((dcl.NEG IN md) = (n.sub=pc.sb_equ))
      THEN
	gen_value(n.l,p,md-{dcl.NEG});
	RETURN;
      END;
      IF p>=8 THEN out.wr('(') END;
      IF (n.l.type.mode=pc.ty_set) & dcl.is_c_arr(n.l.type) THEN
	out.ws("X2C_SET_EQU(");
	gen_value(n.l,0,{dcl.BSA}); out.wr(',');
	gen_value(n.r,0,{dcl.BSA}); out.wf(",%d)",n.l.type.len);
	IF (n.sub=pc.sb_neq) # (dcl.NEG IN md) THEN out.ws("==0") END;
      ELSIF n.l.type.mode IN pc.CPLXs THEN
	binary_cplx;
	IF (n.sub=pc.sb_equ) # (dcl.NEG IN md) THEN out.ws("==0");
	ELSIF ~(dcl.BLN IN md) THEN out.ws("!=0");
	END;
      ELSIF (n.l.type.mode=pc.ty_record) OR dcl.is_c_arr(n.l.type) THEN
	IF (n.l.type.mode IN pc.ARRs) & dcl.is_char(n.l.type.base) THEN
          IF is_empty_str(n.l) THEN
	    gen_value(n.r,14,{dcl.BSA}); out.ws("[0]");
          ELSIF is_empty_str(n.r) THEN
	    gen_value(n.l,14,{dcl.BSA}); out.ws("[0]");
          ELSE
	    nms.x2c(nms.nm_strcmp,nm); out.wf("%s(",nm);
	    gen_value(n.l,0,{dcl.BSA}); out.wr(',');
            gen_len(n.l,0,pcO.size_t,0); out.wr(',');
	    gen_value(n.r,0,{dcl.BSA}); out.wr(',');
            gen_len(n.r,0,pcO.size_t,0); out.wr(')');
          END;
	ELSE
	  nms.x2c(nms.nm_memcmp,nm); out.wf("%s(",nm);
	  gen_value(n.l,0,{dcl.REF,dcl.ANY}); out.wr(',');
	  gen_value(n.r,0,{dcl.REF,dcl.ANY}); out.wr(',');
	  dcl.gen_sizeof(n.l.type); out.wr(')');
	END;
	IF (n.sub=pc.sb_equ) # (dcl.NEG IN md) THEN out.ws("==0");
	ELSIF ~(dcl.BLN IN md) THEN out.ws("!=0");
	END;
      ELSE
	gen_value(n.l,8,{});
	IF (n.sub=pc.sb_equ) # (dcl.NEG IN md) THEN out.ws("==")
	ELSE out.ws("!=")
	END;
	gen_value(n.r,8,{});
      END;
      IF p>=8 THEN out.wr(')') END;
    |pc.sb_lss,pc.sb_leq,pc.sb_gtr,pc.sb_geq:
      IF n.l.type.mode=pc.ty_set THEN
	IF ~chk_neg() THEN RETURN END;
        IF dcl.is_c_arr(n.l.type) THEN
	  out.ws("X2C_SET_LEQ(");
	  IF n.sub=pc.sb_leq THEN
	    gen_value(n.l,0,{dcl.BSA}); out.wr(','); gen_value(n.r,0,{dcl.BSA});
	  ELSE
	    ASSERT(n.sub=pc.sb_geq);
	    gen_value(n.r,0,{dcl.BSA}); out.wr(','); gen_value(n.l,0,{dcl.BSA});
	  END;
	  out.wf(",%d)",n.l.type.len);
	ELSE
	  IF p>13 THEN out.wr('(') END;
	  IF n.sub=pc.sb_leq THEN
	    out.ws("!("); gen_value(n.l,7,{});
	    out.ws("&~"); gen_value(n.r,13,{});
	  ELSE
	    ASSERT(n.sub=pc.sb_geq);
	    out.ws("!(~"); gen_value(n.l,13,{});
	    out.wr("&"); gen_value(n.r,7,{});
	  END;
	  out.wr(')');
	  IF p>13 THEN out.wr(')') END;
	END;
      ELSE
	IF p>=9 THEN out.wr('(') END;
	IF (n.l.type.mode=pc.ty_record) OR dcl.is_c_arr(n.l.type) THEN
	  IF (n.l.type.mode IN pc.ARRs) & dcl.is_char(n.l.type.base) THEN
	    nms.x2c(nms.nm_strcmp,nm); out.wf("%s(",nm);
	    gen_value(n.l,0,{dcl.BSA}); out.wr(',');
            gen_len(n.l,0,pcO.size_t,0); out.wr(',');
	    gen_value(n.r,0,{dcl.BSA}); out.wr(',');
            gen_len(n.r,0,pcO.size_t,0);
	  ELSE
	    nms.x2c(nms.nm_memcmp,nm); out.wf("%s(",nm);
	    gen_value(n.l,0,{dcl.REF,dcl.ANY}); out.wr(',');
	    gen_value(n.r,0,{dcl.REF,dcl.ANY}); out.wr(',');
	    dcl.gen_sizeof(n.l.type);
	  END;
	  out.wr(')'); wr_cmp_sign; out.wr('0');
	ELSE
	  gen_value_not_char(n.l,9,{});
	  wr_cmp_sign;
	  gen_value_not_char(n.r,9,{});
	END;
	IF p>=9 THEN out.wr(')') END;
      END;
    |pc.sb_plus,pc.sb_minus:
      IF n.l.type.mode IN pc.CPLXs THEN binary_cplx;
      ELSIF n.sub=pc.sb_plus THEN binary(11,"+",11);
      ELSE binary(11,"-",11);
      END;
    |pc.sb_mul:
      IF n.l.type.mode IN pc.CPLXs THEN binary_cplx;
      ELSE binary(12,"*",12);
      END;
    |pc.sb_and:
      IF dcl.is_c_arr(n.type) THEN binary_lset("X2C_AND");
      ELSE binary(7,"&",7);
      END;
    |pc.sb_xor:
      IF dcl.is_c_arr(n.type) THEN binary_lset("X2C_XOR");
      ELSE binary(6,"^",6);
      END;
    |pc.sb_or:
      IF dcl.is_c_arr(n.type) THEN binary_lset("X2C_OR");
      ELSE binary(5,"|",5);
      END;
    |pc.sb_bic:
      IF dcl.is_c_arr(n.type) THEN binary_lset("X2C_BIC");
      ELSE binary(7,"&~",13);
      END;
    |pc.sb_cand:
      IF chk_neg() THEN binary_bln(4," && ") END;
    |pc.sb_cor:
      IF chk_neg() THEN binary_bln(3," || ") END;
    |pc.sb_ash     :
      IF (n.r.mode=pc.nd_value) &
         dcl.cmp_value(pc.sb_gtr,n.r.val,zz_m32) &
         dcl.cmp_value(pc.sb_lss,n.r.val,zz_32)
      THEN
	IF p>=10 THEN out.wr('(') END;
	gen_value(n.l,10,md*{dcl.CHK});
	IF n.r.val.is_neg() THEN out.ws(">>") ELSE out.ws("<<") END;
	out.wf("%d",ABS(n.r.val.get_integer()));
	IF p>=10 THEN out.wr(')') END;
      ELSE
	nms.x2c(nms.nm_ash,nm); out.wf("%s(",nm);
	gen_value(n.l,0,{}); out.wr(',');
	gen_value(n.r,0,{}); out.wr(')');
      END;
    |pc.sb_pre_inc,pc.sb_pre_dec:
      IF chk_neg() THEN
	IF pc.ntag_chk_range IN n.tags THEN
	  nms.inc_dec(n.sub,n.type,nm);
	  out.ws(nm); out.wr('(');
	  gen_value(n.l,0,{dcl.LVL,dcl.REF}); out.wr(',');
	  gen_value(n.r,0,{}); out.wr(',');
	  dcl.const_aggregate(n.type.min,n.type,0,FALSE,FALSE); out.wr(',');
	  dcl.const_aggregate(n.type.max,n.type,0,FALSE,FALSE); out.wr(')');
	ELSIF (n.r.mode=pc.nd_value) & dcl.cmp_value(pc.sb_equ,n.r.val,zz_one) THEN
	  IF p>=13 THEN out.wr('(') END;
	  IF n.sub=pc.sb_pre_inc THEN out.ws('++') ELSE out.ws('--') END;
	  gen_value(n.l,13,{dcl.LVL});
	  IF p>=13 THEN out.wr(')') END;
	ELSE
	  IF p>=1 THEN out.wr('(') END;
	  gen_value(n.l,1,{dcl.LVL});
	  IF n.sub=pc.sb_pre_inc THEN out.ws(' += ') ELSE out.ws(' -= ') END;
	  gen_value(n.r,1,{});
	  IF p>=1 THEN out.wr(')') END;
	END;
      END;
    |pc.sb_addadr,pc.sb_subadr:
      gen_type_cast("",n.type);
      out.wr('(');
      gen_type_cast("",pc.AA_type);
      IF pc.ntag_chk_range IN n.tags THEN gen_a_chk(n.l);
      ELSE gen_value(n.l,11,{});
      END;
      (*
         hp9000 can not substruct unsigned long from pointer!,
	 so I have to cast expression to int
      *)
      IF n.sub=pc.sb_addadr
       THEN out.ws('+');
       ELSE out.ws('-');
      END;
      gen_type_cast("",pc.longint_type);
      gen_value(n.r,13,{});
      out.wr(')');
    |pc.sb_difadr:
      binary(11,'-',11);
    |pc.sb_rot:
      ASSERT(n.type.mode IN pc.SETs);
      IF dcl.is_c_arr(n.type) THEN
	nms.x2c(nms.nm_rot_lset,nm); out.wf("%s(",nm);
	dcl.make_temp_var(n.type,nm); out.wf("%s,",nm);
	gen_value(n.l,0,{dcl.BSA}); out.wf(",%d,",n.type.len);
	gen_value(n.r,0,{}); out.wr(')');
      ELSE
	nms.x2c(nms.nm_rot_set,nm); out.wf("%s(",nm);
	gen_value(n.l,0,{}); out.wf(",%d,",n.type.len);
	gen_value(n.r,0,{}); out.wr(')');
      END;
    |pc.sb_lsh	   :
      IF dcl.is_c_arr(n.type) THEN
        ASSERT(n.type.mode=pc.ty_set);
	nms.x2c(nms.nm_lsh_lset,nm); out.wf("%s(",nm);
	dcl.make_temp_var(n.type,nm); out.wf("%s,",nm);
	gen_value(n.l,0,{dcl.BSA}); out.wf(",%d,",n.type.len);
	gen_value(n.r,0,{}); out.wr(')');
      ELSE
        CASE n.type.mode OF
          |pc.ty_shortint,pc.ty_char,pc.ty_loc  : i:= 8;
          |pc.ty_integer                        : i:=16;
          |pc.ty_longint,pc.ty_ZZ               : i:=32;
          |pc.ty_set                            : i:=n.type.len;
        END;
        b:=n.type.is_ordinal() & n.type.signed();
	IF (p>13) & b THEN out.wr('(') END;
        IF b THEN gen_type_cast("",n.type) END;
	nms.x2c(nms.nm_lsh_set,nm); out.wf("%s(",nm);
	gen_value(n.l,0,{}); out.wf(",%d,",i);
	gen_value(n.r,0,{}); out.wr(')');
	IF (p>13) & b THEN out.wr(')') END;
      END;
    |pc.sb_high    :
      ASSERT(n.r.mode=pc.nd_value);
      t:=n.l.type;
      i:=n.r.val.get_integer();
      WHILE i>0 DO t:=t.base; DEC(i) END;
      IF t.mode=pc.ty_array THEN
        dcl.const_aggregate(t.max,t.inx,p,FALSE,FALSE);
      ELSE
        ASSERT(n.l.type.mode=pc.ty_array_of);
        IF p>=11 THEN out.wr('(') END;
        gen_len(n.l,n.r.val.get_integer(),n.type,11);
        out.ws("-1");
        IF p>=11 THEN out.wr(')') END;
      END;
    |pc.sb_len     :
      ASSERT(n.r.mode=pc.nd_value);
      gen_len(n.l,n.r.val.get_integer(),n.type,p);
    |pc.sb_in      :
      t:=n.r.type; b:=dcl.is_c_arr(t);
      IF ~b & (n.l.mode=pc.nd_value) THEN
	IF p>=8 THEN out.wr('(') END;
	zz_tmp.binary(pc.sb_minus,n.l.val,t.min);
	out.wf("(0x%X",{zz_tmp.get_integer()}); dcl.suffix(t);
	out.ws(" & "); gen_value(n.r,7,{}); out.wr(')');
	IF dcl.NEG IN md THEN out.ws("==0");
	ELSIF ~(dcl.BLN IN md) THEN out.ws("!=0");
	END;
	IF p>=8 THEN out.wr(')') END;
      ELSIF chk_neg() THEN
	IF b THEN nms.x2c(nms.nm_inl,nm) ELSE nms.x2c(nms.nm_in,nm) END;
	out.wf("%s(",nm);
	l:=n.l;
	IF dcl.is_long(l.type) THEN
	  gen_value_minus_min(l,0,t.min,TRUE);
	ELSE
	  nms.x2c(nms.nm_longint,nm);
	  out.wf("(%s)",nm);
	  gen_value_minus_min(l,13,t.min,TRUE);
	END;
	out.wf(",%d,",t.len);
	IF b THEN gen_value(n.r,0,{dcl.BSA}) ELSE gen_value(n.r,0,{}) END;
	out.wr(')');
      END;
    |pc.sb_slash,pc.sb_div,pc.sb_mod,pc.sb_rem:
      IF n.l.type.mode IN pc.CPLXs THEN
	binary_cplx;
      ELSE
	(* for ZZ-type '/' and '%' are used *)
	t:=NIL;
	IF n.type.is_ordinal() THEN
	  t:=n.type.super_type();
	  IF (t.mode IN pc.CARDs) OR
             (cc.op_cdiv & ((n.sub=pc.sb_div) OR (n.sub=pc.sb_mod)))
	  THEN
	    t:=NIL;
	  END;
	END;
	IF t#NIL THEN
	  IF t.mode#pc.ty_longint THEN
	    IF p>13 THEN out.wr('(') END;
	    gen_type_cast("",n.type);
	  END;
          IF (n.l.type.mode = pc.ty_longlongint) OR (n.r.type.mode = pc.ty_longlongint) THEN
             CASE n.sub OF
               |pc.sb_slash: nms.x2c(nms.nm_quo64,nm);
               |pc.sb_div  : nms.x2c(nms.nm_div64,nm);
               |pc.sb_rem  : nms.x2c(nms.nm_rem64,nm);
               |pc.sb_mod  : nms.x2c(nms.nm_mod64,nm);
             END;
          ELSE
             CASE n.sub OF
               |pc.sb_slash: nms.x2c(nms.nm_quo,nm);
               |pc.sb_div  : nms.x2c(nms.nm_div,nm);
               |pc.sb_rem  : nms.x2c(nms.nm_rem,nm);
               |pc.sb_mod  : nms.x2c(nms.nm_mod,nm);
             END;
          END;
	  out.wf("%s(",nm);
	  gen_value(n.l,0,{}); out.wr(',');
	  gen_value(n.r,0,{}); out.wr(')');
	  IF t.mode#pc.ty_longint THEN
	    IF p>13 THEN out.wr(')') END;
	  END;
	ELSE
	  IF (n.sub = pc.sb_slash) & ((n.type.mode = pc.ty_real) OR 
	                               (n.type.mode = pc.ty_longreal)) THEN
            IF p>=12 THEN out.wr('(') END;
            CASE n.type.mode OF
              |pc.ty_real    :  nms.x2c(nms.nm_divr, nm);
              |pc.ty_longreal:  nms.x2c(nms.nm_divl, nm);
            END;
            out.wf("%s(",nm);
            gen_value(n.l,0,{}); out.wr(',');
            gen_value(n.r,0,{}); out.wr(')');
            IF p>=12 THEN out.wr(')') END;
          ELSE
            IF p>=12 THEN out.wr('(') END;
            gen_value(n.l,12,{});
            IF (n.sub=pc.sb_slash) OR (n.sub=pc.sb_div) THEN out.wr('/');
            ELSE out.wr('%');
            END;
            gen_value(n.r,12,{});
            IF p>=12 THEN out.wr(')') END;
      	  END;
	END;
      END;
    |pc.sb_exp:
      IF n.type.mode=pc.ty_complex THEN
	IF n.r.type.mode IN pc.WHOLEs THEN out.ws("X2C_EXPCI(");
	ELSE out.ws("X2C_EXPCR("); ASSERT(n.r.type.mode IN pc.REALs);
	END;
      ELSIF n.type.mode IN pc.TY_SET{pc.ty_lcomplex,pc.ty_CC} THEN
	IF n.r.type.mode IN pc.WHOLEs THEN out.ws("X2C_EXPLI(");
	ELSE out.ws("X2C_EXPLR("); ASSERT(n.r.type.mode IN pc.REALs);
	END;
      ELSE
	ASSERT(n.l.type.mode IN pc.REALs);
	IF n.r.type.mode IN pc.WHOLEs THEN out.ws("X2C_EXPRI(");
	ELSE out.ws("X2C_EXPRR("); ASSERT(n.r.type.mode IN pc.REALs);
	END;
      END;
      gen_value(n.l,0,{}); out.wr(',');
      gen_value(n.r,0,{}); out.wr(')');
    |pc.sb_cmplx:
      dcl.make_temp_var(n.type,nm);
      out.wf("(%s.re = ",nm); gen_value(n.l,1,{});
      out.wf(",%s.im = ",nm); gen_value(n.r,1,{});
      out.wf(',%s)',nm);
    |pc.sb_concat:
      ASSERT(n.type.mode=pc.ty_SS);
      gen_value(n.l,0,md);
      gen_value(n.r,0,md);
  ELSE
    env.errors.Error(n.pos,1008);
  END;
END gen_value_binary;

PROCEDURE cmp_base_types(t1,t2: pc.STRUCT): BOOLEAN;
BEGIN
  IF t1.mode=pc.ty_pointer THEN t1:=t1.base;
  ELSE t1:=dcl.get_base_type(t1);
  END;
  IF t2.mode=pc.ty_pointer THEN t2:=t2.base;
  ELSE t2:=dcl.get_base_type(t2);
  END;
  RETURN (t1#NIL) & (t1=t2);
END cmp_base_types;

PROCEDURE gen_conv_adr(n: pc.NODE; p: INTEGER; md: SET);
BEGIN
  ASSERT(n.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque});
  ASSERT(n.l.mode=pc.nd_unary);
  ASSERT(n.l.sub=pc.su_adr);
  IF (n.type.mode=pc.ty_pointer) &
     dcl.is_c_arr(n.l.l.type) &
     dcl.c_types_equ(n.l.l.type,n.type,TRUE)
  THEN
    gen_value(n.l.l,p,{dcl.BSA,dcl.LVL}+md*{dcl.CCH});
  ELSIF (n.type.mode=pc.ty_pointer) &
         ~dcl.is_c_arr(n.l.l.type) &
         dcl.c_types_equ(n.l.l.type,n.type.base,FALSE)
  THEN
    gen_value(n.l.l,p,{dcl.REF,dcl.LVL}+md*{dcl.CCH});
  ELSE
    IF p>13 THEN out.wr('(') END;
    gen_type_cast("",n.type);
    gen_value(n.l.l,13,{dcl.REF,dcl.ANY,dcl.LVL}+md*{dcl.CCH});
    IF p>13 THEN out.wr(')') END;
  END;
END gen_conv_adr;

PROCEDURE gen_value_cast(n: pc.NODE; p: INTEGER; md: SET);
(* if types size don't match then additional bits are not defined *)

  PROCEDURE gen_temp_var(sz: LONGINT);
    (* assigns source expression into temporary variable with
       result type, generated text returns "void *" of temporary var
    *)
    VAR nm,tmp: STR;
  BEGIN
    IF dcl.LVL IN md THEN
      env.errors.Error(n.pos,1009);
    END;
    dcl.make_temp_var(n.type,tmp);
    nms.x2c(nms.nm_memcpy,nm); out.wf("%s(&%s,",nm,tmp);
    gen_value(n.l,0,{dcl.REF,dcl.ANY}); out.wr(',');
    dcl.c_number(sz,pcO.size_t,nm); out.wf("%s)",nm);
  END gen_temp_var;

  PROCEDURE is_whole_in_c(t: pc.STRUCT): BOOLEAN;
  BEGIN
    IF t.mode IN (pc.WHOLEs+
      pc.TY_SET{pc.ty_boolean,pc.ty_char,pc.ty_AA,pc.ty_range}+
      pc.TY_SET{pc.ty_enum,pc.ty_loc,pc.ty_protection})
    THEN RETURN TRUE;
    END;
    IF (t.mode=pc.ty_set) & (t.len<=32) THEN RETURN TRUE END;
    RETURN FALSE;
  END is_whole_in_c;

  VAR nm: STR; s1,s2: LONGINT;
BEGIN
  IF (md*{dcl.REF,dcl.LVL}={}) & (
       is_whole_in_c(n.type) &
       is_whole_in_c(n.l.type) OR
       (n.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) &
       (n.l.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) OR
       (n.type.mode=pc.ty_proctype) &
       (n.l.type.mode=pc.ty_proctype)
     )
  THEN
    (* generate C type cast *)
    ASSERT({dcl.BSA,dcl.NEG,dcl.ANY}*md={});
    IF (n.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) &
       (n.l.mode=pc.nd_unary) &
       (n.l.sub=pc.su_adr)
    THEN
      (* оптимизнем cast к char* *)
      gen_conv_adr(n,p,md);
    ELSE
      IF p>13 THEN out.wr('(') END;
      gen_type_cast("",n.type);
      gen_value_not_char(n.l,13,md*{dcl.CHK});
      IF p>13 THEN out.wr(')') END;
    END;
  ELSE
    IF (n.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) &
       (n.l.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) OR
       (n.type.mode=pc.ty_proctype) &
       (n.l.type.mode=pc.ty_proctype)
    THEN
      s1:=0; s2:=0;
      (* не нужно знать реальные размеры если известно что они равны *)
    ELSE
      s1:=pc.code.get_size(pc.su_size,n.l.type);
      s2:=pc.code.get_size(pc.su_size,n.type);
    END;
    IF n.type.mode=pc.ty_array_of THEN
      ASSERT((dcl.BSA IN md) OR (dcl.REF IN md) & (dcl.ANY IN md));
      IF (dcl.BSA IN md) &
         (n.type.flag IN cc.c_like_flags) &
	 (n.l.type.mode=pc.ty_pointer) &
	 cmp_base_types(n.type,n.l.type)
      THEN
	gen_value(n.l,p,{});
      ELSIF (dcl.ANY IN md) OR
	 (dcl.BSA IN md) &
	 dcl.is_c_arr(n.l.type) &
	 cmp_base_types(n.type,n.l.type)
      THEN
	gen_value(n.l,p,md);
      ELSE
	IF p>13 THEN out.wr('(') END;
	gen_type_cast("*",dcl.get_base_type(n.type));
	gen_value(n.l,13,{dcl.REF,dcl.ANY});
	IF p>13 THEN out.wr(')') END;
      END;
    ELSIF n.l.type.is_ordinal() &
	  (n.l.mode=pc.nd_value) &
	  n.l.val.is_zero()
    THEN
      ASSERT(~(dcl.LVL IN md));
      IF (p>13) & (md*{dcl.REF,dcl.ANY}#{dcl.REF,dcl.ANY}) THEN out.wr("(") END;
      IF ~(dcl.REF IN md) THEN out.wr("*") END;
      IF ~(dcl.ANY IN md) THEN gen_type_cast("*",n.type) END;
      dcl.make_temp_var(n.type,nm);
      out.wf("memset(&%s,0,",nm);
      dcl.gen_sizeof(n.type);
      out.wr(")");
      IF (p>13) & (md*{dcl.REF,dcl.ANY}#{dcl.REF,dcl.ANY}) THEN out.wr(")") END;
    ELSIF (s1<0) OR (s2<0) THEN
      env.errors.Warning(n.pos,350);
      IF (p>13) & ~(dcl.REF IN md) THEN out.wr('(') END;
      IF ~(dcl.REF IN md) THEN out.wr('*') END;
      out.ws("X2C_CAST(");
      gen_value(n.l,0,{dcl.REF,dcl.ANY}); out.wr(",");
      dcl.type_designator(nm,"",n.l.type);
      out.ws(nm); out.wr(",");
      dcl.type_designator(nm,"",n.type);
      out.ws(nm); out.wr(",");
      dcl.type_designator(nm,"*",n.type);
      out.ws(nm); out.wr(")");
      IF (p>13) & ~(dcl.REF IN md) THEN out.wr(')') END;
    ELSIF (dcl.REF IN md) & (dcl.ANY IN md) THEN
      IF s2>s1 THEN gen_temp_var(s1) ELSE gen_value(n.l,p,md) END;
    ELSE
      IF p>13 THEN out.wr('(') END;
      IF ~(dcl.REF IN md) THEN out.wr('*') END;
      gen_type_cast("*",n.type);
      IF s2>s1 THEN gen_temp_var(s1);
      ELSE gen_value(n.l,13,{dcl.REF,dcl.ANY});
      END;
      IF p>13 THEN out.wr(')') END;
    END;
  END;
END gen_value_cast;

PROCEDURE conv_is_cast(fr,to: pc.STRUCT): BOOLEAN;
  VAR sf,st: pc.STRUCT;
BEGIN
  IF dcl.is_c_arr(fr) & ((fr.mode#pc.ty_SS) OR (fr.len>2)) OR
     dcl.is_c_arr(to) OR
     (fr.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) &
     (to.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) OR
     (fr.mode=pc.ty_record) OR
     (to.mode=pc.ty_record) OR
     (fr.mode=pc.ty_proctype) & (to.mode=pc.ty_proctype)
  THEN
    RETURN TRUE;
  END;
  IF fr.is_ordinal() THEN sf:=fr.super_type() ELSE sf:=fr END;
  IF to.is_ordinal() THEN st:=to.super_type() ELSE st:=to END;
  RETURN
    (sf.mode IN pc.TY_SET{pc.ty_shortint,pc.ty_shortcard,pc.ty_loc}) &
    (st.mode=pc.ty_loc) OR
    (sf.mode=pc.ty_loc) &
    (st.mode IN pc.TY_SET{pc.ty_shortint,pc.ty_shortcard,pc.ty_loc});
END conv_is_cast;

PROCEDURE gen_value_unary_conv(n: pc.NODE; p: INTEGER; md: SET);
  PROCEDURE min(x,y: pc.VALUE): pc.VALUE;
  BEGIN
    zz_tmp.binary(pc.sb_lss,x,y);
    IF zz_tmp.is_zero() THEN RETURN y ELSE RETURN x END;
  END min;
  PROCEDURE max(x,y: pc.VALUE): pc.VALUE;
  BEGIN
    zz_tmp.binary(pc.sb_lss,x,y);
    IF zz_tmp.is_zero() THEN RETURN x ELSE RETURN y END;
  END max;
  VAR
    fn		: STR;
    sf,st	: pc.STRUCT;
    nf		: pc.NODE;
    vmin,vmax	: pc.VALUE;
    chk,cnv	: BOOLEAN;
BEGIN
  ASSERT(md*{dcl.NEG}={});
  sf:=n.l.type;
  st:=n.type;
  IF sf.mode=pc.ty_range THEN sf:=sf.base END;
  IF st.mode=pc.ty_range THEN st:=st.base END;
  IF (sf.mode IN pc.TY_SET{pc.ty_boolean,pc.ty_char,pc.ty_enum}) THEN
    sf:=sf.super_type();
  END;

  IF (sf.mode=pc.ty_ZZ) &
     (n.l.mode=pc.nd_unary) &
     ((n.l.sub=pc.su_size) OR (n.l.sub=pc.su_bytes))
  THEN
    sf:=pcO.size_t; n.l.type:=sf;
    ASSERT(n.l.type.max#NIL);
    ASSERT(n.l.type.min#NIL);
  END;

  IF (sf.mode IN pc.TY_SET{pc.ty_shortint,pc.ty_integer,pc.ty_longint,
		  pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard}) &
     (st.mode IN pc.TY_SET{pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard,
		  pc.ty_shortint,pc.ty_integer,pc.ty_longint,
		  pc.ty_boolean,pc.ty_char,pc.ty_enum})
  THEN (* 1 .. 4 *)
    ASSERT(n.l.type.max#NIL);
    ASSERT(n.l.type.min#NIL);
    IF ~(pc.ntag_chk_range IN n.tags) &
      dcl.c_types_equ(sf,st,TRUE)
    THEN
      gen_value(n.l,p,md);
    ELSE
      chk:=(pc.ntag_chk_range IN n.tags) & ~(dcl.CHK IN md);
      nf:=n.l;
      vmin:=max(n.type.min,nf.type.min);
      vmax:=min(n.type.max,nf.type.max);
      WHILE (nf.mode=pc.nd_unary) &
            (nf.sub=pc.su_conv) &
            (nf.l.type.mode IN
                 pc.TY_SET{pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard,
		  pc.ty_shortint,pc.ty_integer,pc.ty_longint,
		  pc.ty_boolean,pc.ty_char,pc.ty_enum})
      DO
        chk:=chk OR (pc.ntag_chk_range IN nf.tags) & ~(dcl.CHK IN md);
        nf:=nf.l;
        vmin:=max(vmin,nf.type.min);
        vmax:=min(vmax,nf.type.max);
        sf:=nf.type.super_type();
      END;
      cnv:=~dcl.c_types_equ(sf,st,TRUE);
      IF cnv THEN
        IF p>13 THEN out.wr('(') END;
        gen_type_cast("",n.type);
      END;
      IF chk THEN
        nms.r_chk(sf,fn); out.ws(fn); out.wr('(');
        gen_value_not_char(nf,0,md*{dcl.CHK}); out.wr(',');
        dcl.const_aggregate(vmin,sf,0,FALSE,FALSE);
        out.wr(',');
        dcl.const_aggregate(vmax,sf,0,FALSE,FALSE);
	out.wr(')');
      ELSIF cnv THEN
	gen_value_not_char(nf,13,md*{dcl.CHK});
      ELSE
	gen_value_not_char(nf,p,md*{dcl.CHK});
      END;
      IF cnv & (p>13) THEN out.wr(')') END;
    END;
  ELSIF (sf.mode IN pc.TY_SET{pc.ty_shortint,pc.ty_integer,pc.ty_longint,
		  pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard,
		  pc.ty_real,pc.ty_longreal,pc.ty_ld_real,pc.ty_RR}) &
	(st.mode IN pc.TY_SET{pc.ty_real,pc.ty_longreal,pc.ty_ld_real,pc.ty_RR}) OR
	(sf.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) &
	(st.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) OR
	(sf.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) &
	(st.mode IN pc.WHOLEs) OR
	(sf.mode IN pc.WHOLEs) &
	(st.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) OR
	(st.mode=pc.ty_proctype) &
	(sf.mode=pc.ty_proctype)
  THEN (* 5 *)
    IF (st.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) &
       (n.l.mode=pc.nd_unary) &
       (n.l.sub=pc.su_adr)
    THEN
      gen_conv_adr(n,p,md);
    ELSE
      IF (st.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) &
         ~(sf.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque})
      THEN
        env.errors.Warning(n.pos,350);
      END;
      IF p>13 THEN out.wr('(') END;
      gen_type_cast("",n.type);
      gen_value_not_char(n.l,13,md*{dcl.CHK});
      IF p>13 THEN out.wr(')') END;
    END;
  ELSIF (sf.mode IN pc.TY_SET{pc.ty_real,pc.ty_longreal,pc.ty_ld_real}) &
	(st.mode IN pc.TY_SET{pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard})
  THEN
    IF p>13 THEN out.wr('(') END;
    gen_type_cast("",n.type);
    nms.x2c(nms.nm_truncc,fn); out.wf("%s(",fn);
    gen_value(n.l,0,md*{dcl.CHK}); out.wr(',');
    dcl.const_aggregate(n.type.min,st,0,FALSE,FALSE); out.wr(',');
    dcl.const_aggregate(n.type.max,st,0,FALSE,FALSE); out.wr(')');
    IF p>13 THEN out.wr(')') END;
  ELSIF (sf.mode IN pc.TY_SET{pc.ty_real,pc.ty_longreal,pc.ty_ld_real}) &
	(st.mode IN pc.TY_SET{pc.ty_shortint,pc.ty_integer,pc.ty_longint})
  THEN
    IF p>13 THEN out.wr('(') END;
    gen_type_cast("",n.type);
    nms.x2c(nms.nm_trunci,fn);
    out.wf("%s(",fn);
    gen_value(n.l,0,md*{dcl.CHK}); out.wr(',');
    dcl.const_aggregate(n.type.min,st,0,FALSE,FALSE); out.wr(',');
    dcl.const_aggregate(n.type.max,st,0,FALSE,FALSE); out.wr(')');
    IF p>13 THEN out.wr(')') END;
  ELSIF (sf.mode=pc.ty_AA) &
	(st.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque,pc.ty_proctype})
  THEN
    out.wr('0');
  ELSIF
    (sf.mode=pc.ty_longcard) &
    (st.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque})
  THEN
    (* SYSTEM.MAKEADR *)
    IF p>13 THEN out.wr('(') END;
    gen_type_cast("",n.type);
    gen_value(n.l,13,md*{dcl.CHK});
    IF p>13 THEN out.wr(')') END;
  ELSIF (sf.mode=pc.ty_set) & ~dcl.is_c_arr(sf) &
	(st.mode IN pc.TY_SET{pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard,
		  pc.ty_shortint,pc.ty_integer,pc.ty_longint,
		  pc.ty_boolean,pc.ty_char,pc.ty_enum})
  THEN (* 9 *)
    IF p>13 THEN out.wr('(') END;
    gen_type_cast("",n.type);
    IF (pc.ntag_chk_range IN n.tags) & ~(dcl.CHK IN md) THEN
      (* Здесь генерится r_chk неправильного размера (size=4) !!! *)
      nms.r_chk(sf,fn); out.wf("%s(",fn);
      gen_value(n.l,0,md*{dcl.CHK}); out.wr(',');
      dcl.const_aggregate(n.type.min,sf,0,FALSE,FALSE); out.wr(',');
      dcl.const_aggregate(n.type.max,sf,0,FALSE,FALSE); out.wr(')');
    ELSE
      gen_value(n.l,13,md*{dcl.CHK});
    END;
    IF p>13 THEN out.wr(')') END;
  ELSIF (sf.mode=pc.ty_complex) & (st.mode=pc.ty_lcomplex) THEN
    out.ws("CPLX_L("); gen_value(n.l,0,md*{dcl.CHK}); out.wr(')');
  ELSIF conv_is_cast(sf,st) THEN
    gen_value_cast(n,p,md*{dcl.CHK});
  ELSIF (sf.mode=pc.ty_ZZ) &
        (st.mode IN pc.TY_SET{pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard,
		     pc.ty_shortint,pc.ty_integer,pc.ty_longint,
		     pc.ty_boolean,pc.ty_char,pc.ty_enum})
  THEN (* 1 .. 4 *)
    IF ~(pc.ntag_chk_range IN n.tags) OR (dcl.CHK IN md) THEN
      gen_value(n.l,p,md);
    ELSE
      IF p>13 THEN out.wr('(') END;
      gen_type_cast("",n.type);
      IF st.signed() THEN nms.x2c(nms.nm_r_chkl,fn);
      ELSE nms.x2c(nms.nm_r_chkul,fn);
      END;
      out.ws(fn); out.wr('(');
      gen_value(n.l,0,md*{dcl.CHK}); out.wr(',');
      dcl.const_aggregate(n.type.min,st,0,FALSE,FALSE);
      out.wr(',');
      dcl.const_aggregate(n.type.max,st,0,FALSE,FALSE);
      out.wr(')');
      IF p>13 THEN out.wr(')') END;
    END;
  ELSIF (sf.mode=pc.ty_SS) & st.is_ordinal() (*(st.mode IN pc.WHOLEs)*) THEN
    IF n.l.mode = pc.nd_var THEN
      ASSERT(n.l.obj.mode = pc.ob_cons);
      zz_tmp.index_get(0,n.l.obj.val.val);
    ELSE
      ASSERT(n.l.mode=pc.nd_value);
      zz_tmp.index_get(0,n.l.val);
    END;
    dcl.const_aggregate(zz_tmp,n.l.type.base,p,FALSE,FALSE);
  ELSE
    <* IF pcvis THEN *> pcVis.vis(n,0); <* END *>
    env.errors.Error(n.pos,1010);
  END;
END gen_value_unary_conv;

PROCEDURE gen_conv_vptr(n: pc.NODE; p: INTEGER; md: SET);
  VAR conv: BOOLEAN;
BEGIN
  ASSERT(n.mode=pc.nd_unary);
  ASSERT(md*{dcl.NEG}={});
  CASE n.sub OF
    |pc.su_ptr2vptr:
      conv:=(n.type.mode#pc.ty_pointer) OR
            ~(n.type.base.mode IN pc.TY_SET{pc.ty_loc,pc.ty_char});
      IF conv THEN
        IF p>13 THEN out.wr('(') END;
        gen_type_cast("",n.type);
      END;
      out.ws("VPTR(");
      gen_value(n.l,0,{});
      out.wr(')');
      IF conv THEN
        IF p>13 THEN out.wr(')') END;
      END;
    |pc.su_vptr2ptr:
      conv:=(n.type.mode#pc.ty_pointer) OR
            ~(n.type.base.mode IN pc.TY_SET{pc.ty_loc,pc.ty_char});
      IF conv THEN
        IF p>13 THEN out.wr('(') END;
        gen_type_cast("",n.type);
      END;
      out.ws("RPTR(");
      gen_value(n.l,0,{});
      out.wr(')');
      IF conv THEN
        IF p>13 THEN out.wr(')') END;
      END;
  END;
END gen_conv_vptr;

PROCEDURE gen_value_unary(n: pc.NODE; p: INTEGER; md: SET);

  PROCEDURE chk_neg(): BOOLEAN;
  BEGIN
    IF ~(dcl.NEG IN md) THEN RETURN TRUE END;
    IF p>13 THEN out.wr('(') END;
    out.wr('!'); gen_value_unary(n,13,md-{dcl.NEG}+{dcl.BLN});
    IF p>13 THEN out.wr(')') END;
    RETURN FALSE;
  END chk_neg;

  VAR nm: STR; i: LONGINT;
BEGIN
  ASSERT(md-{dcl.BLN,dcl.NEG,dcl.BSA,dcl.CCH,dcl.CNS,dcl.CHK,dcl.PLS}={});
  ASSERT(~(dcl.NEG IN md) OR (n.type.mode=pc.ty_boolean));
  CASE n.sub OF
    |pc.su_bits:
      IF p>=12 THEN out.wr('(') END;
      gen_size(n.l,0,12);
      out.ws("*8");
      IF p>=12 THEN out.wr(')') END;
    |pc.su_min:
      CASE n.l.type.mode OF
	|pc.ty_real    : out.ws("X2C_min_real");
	|pc.ty_longreal: out.ws("X2C_min_longreal");
	|pc.ty_ld_real : out.ws("X2C_min_longdouble");
      END;
    |pc.su_max:
      CASE n.l.type.mode OF
	|pc.ty_real    : out.ws("X2C_max_real");
	|pc.ty_longreal: out.ws("X2C_max_longreal");
	|pc.ty_ld_real : out.ws("X2C_max_longdouble");
      END;
    |pc.su_bytes,pc.su_size:
      gen_size(n.l,0,p);
    |pc.su_abs:
      nms.u_abs(n.type,pc.ntag_chk_overflow IN n.tags,nm);
      out.ws(nm); out.wr('(');
      gen_value(n.l,0,{}); out.wr(')');
    |pc.su_cap:
      nms.x2c(nms.nm_cap,nm); out.wf("%s(",nm);
      gen_value(n.l,0,{}); out.wr(')');
    |pc.su_neg:
      IF n.type.mode IN pc.CPLXs THEN
	IF n.type.mode=pc.ty_complex THEN nms.x2c(nms.nm_cplx_neg,nm);
	ELSE nms.x2c(nms.nm_cplx_lneg,nm);
	END;
	out.wf("%s(",nm); gen_value(n.l,0,{}); out.wr(')');
      ELSE
	IF p>13 THEN out.wr('(') ELSIF p=13 THEN out.wr(' ') END;
	out.ws('-'); gen_value(n.l,13,{});
	IF p>13 THEN out.wr(')') END;
      END;
    |pc.su_adr   :
      ASSERT(md*{dcl.NEG}={});
      IF p>13 THEN out.wr('(') END;
      nms.x2c(nms.nm_AA,nm); out.wf("(%s)",nm);
      gen_value(n.l,13,{dcl.REF,dcl.ANY,dcl.LVL}+md*{dcl.CCH});
      IF p>13 THEN out.wr(')') END;
    |pc.su_adr_o2:
      ASSERT(md*{dcl.NEG}={});
      IF p>13 THEN out.wr('(') END;
      nms.x2c(nms.nm_longint,nm); out.wf("(%s)",nm);
      gen_value(n.l,13,{dcl.REF,dcl.ANY,dcl.LVL});
      IF p>13 THEN out.wr(')') END;
    |pc.su_conv  :
      IF (dcl.BLN IN md) &
	 ~(pc.ntag_chk_range IN n.tags) &
	 n.type.is_ordinal() &
	 n.l.type.is_ordinal()
      THEN
	gen_value(n.l,p,md);
      ELSIF ~(pc.ntag_chk_range IN n.tags) &
            dcl.c_types_equ(n.l.type,n.type,TRUE)
      THEN
	gen_value(n.l,p,md);
      ELSIF chk_neg() THEN
        gen_value_unary_conv(n,p,md);
      END;
    |pc.su_odd:
      IF chk_neg() THEN
	IF p>13 THEN out.wr('(') END;
	IF ~(dcl.BLN IN md) THEN
	  nms.x2c(nms.nm_boolean,nm); out.wf("(%s)",nm);
	END;
	out.wr('('); gen_value(n.l,7,{}); out.ws("&1)");
	IF p>13 THEN out.wr(')') END;
      END;
    |pc.su_not:
      gen_value(n.l,p,md/{dcl.NEG});
    |pc.su_length:
      nms.x2c(nms.nm_length,nm); out.wf("%s(",nm);
      gen_value(n.l,0,{dcl.REF,dcl.ANY}); out.wr(',');
      gen_len(n.l,0,n.type,0); out.wr(')');
    |pc.su_compl :
      ASSERT(n.type.mode IN (pc.TY_SET{pc.ty_set}+pc.WHOLEs-pc.TY_SET{pc.ty_ZZ}));
      IF dcl.is_c_arr(n.type) THEN
	ASSERT(n.type.mode=pc.ty_set);
	dcl.make_temp_var(n.type,nm);
	out.wf("X2C_COMPLEMENT(%s,",nm);
	gen_value(n.l,0,{dcl.BSA});
	out.wf(",%d)",(n.type.len+dcl.LSET_BITS-1) DIV dcl.LSET_BITS);
      ELSE
	IF n.type.mode=pc.ty_set THEN
	  i:=n.type.len;
	ELSE
	  i:=dcl.get_bytes(n.pos,n.type)*8;
	END;
	IF i=32 THEN
	  (* Предпологаем, что промежуточный результат не превышает 32 бита *)
	  IF p>13 THEN out.wr('(') END;
	  out.wr("~"); gen_value(n.l,13,{});
	  IF p>13 THEN out.wr(')') END;
	ELSE
	  IF p>6 THEN out.wr('(') END;
	  gen_value(n.l,6,{});
	  out.wf("^0x%X",{0..i-1});
	  dcl.suffix(n.type);
	  IF p>6 THEN out.wr(')') END;
	END;
      END;
    |pc.su_re:
      gen_value(n.l,14,{});
      out.ws(".re");
    |pc.su_im:
      gen_value(n.l,14,{});
      out.ws(".im");
    |pc.su_entier:
      out.ws("X2C_ENTIER(");
      gen_value(n.l,0,{});
      out.wr(')');
    |pc.su_is:
      IF p>=8 THEN out.wr('(') END;
      gen_type(n.l,14);
      out.wf("->base[%d]",n.obj.type.len);
      IF dcl.NEG IN md THEN out.ws("!=") ELSE out.ws("==") END;
      gen_type_name(n.obj.type,8);
      IF p>=8 THEN out.wr(')') END;
    |pc.su_ptr2vptr,pc.su_vptr2ptr:
      gen_conv_vptr(n,p,md);
    |pc.su_bit_offs, pc.su_byte_offs, pc.su_word_offs:
      gen_field_ofs(n.l);
(*
    |pc.su_cc    : gen_value(n.l);
*)
  ELSE
    env.errors.Error(n.pos,1008);
  END;
END gen_value_unary;

PROCEDURE gen_value_or_address(n: pc.NODE; p: INTEGER; md: SET): BOOLEAN;
  VAR nm: STR;
BEGIN
  CASE n.mode OF
    |pc.nd_var      :
      IF dcl.is_pointer(n.obj,dcl.cur_proc) THEN
	gen_value_usage(n.obj,p,md);
	RETURN TRUE;
      END;
    |pc.nd_replace  :
      IF dcl.LVL IN md THEN
        ASSERT(dcl.REF IN md);
        gen_value(n,p,md);
        RETURN TRUE;
      END;
    |pc.nd_deref    :
      IF cc.op_cpp &
         (n.l.mode=pc.nd_var) &
         (pc.otag_with IN n.l.obj.tags)
      THEN
        dcl.o_name(n.l.obj,nm);
        out.ws(nm);
        RETURN FALSE;
      ELSE
        gen_value(n,p,md);
        RETURN TRUE;
      END;
  ELSE
  END;
  gen_value(n,p,md-{dcl.REF});
  RETURN FALSE;
END gen_value_or_address;

PROCEDURE gen_index_check*(n,l: pc.NODE; p,dim: INTEGER; en: BOOLEAN);
(* n - expression, l - array or set *)
  VAR nm: STR;
BEGIN
  IF en THEN
    IF (l.mode = pc.nd_deref) & (l.l.type.flag = pc.flag_c) THEN
      COPY("X2C_CHKSL",nm);
    ELSE
      nms.i_chk(l.type,nm);
    END;
    out.ws(nm); out.wr('(');
    p:=0;
  END;
  gen_value_minus_min(n,p,l.type.min,en);
  IF en THEN
    IF ~((l.mode = pc.nd_deref) & (l.l.type.flag = pc.flag_c)) THEN
      out.wr(',');
      gen_len(l,dim,pcO.size_t,0);
    END;
    out.wr(')');
  END;
END gen_index_check;

PROCEDURE gen_index(n: pc.NODE; p: INTEGER; md: SET);
  VAR
    l : pc.NODE;
    r : ARRAY 32 OF pc.NODE;
    no: INTEGER;
    i : INTEGER;
BEGIN
  ASSERT(md*{dcl.NEG,dcl.REF}={});
  l:=n.l; r[0]:=n.r; no:=1;
  WHILE (l.type.mode=pc.ty_array_of) &
	(l.mode=pc.nd_index) &
	(l.l.type.mode=pc.ty_array_of)
  DO
    r[no]:=l.r; INC(no); l:=l.l;
  END;

  IF (l.mode = pc.nd_deref) & (l.l.type.flag = pc.flag_c) THEN
    IF (n.type.mode=pc.ty_array_of) & (p>=11) THEN out.wr('(') END;
    gen_value(l,11,{dcl.BSA});

    out.wr('[');
    gen_index_check(r[no-1],l,0,0,
          (pc.ntag_chk_range IN n.tags) & ~(dcl.CHK IN md));
    out.wr(']');
    FOR i:=1 TO no-1 DO
      out.wr('[');
      gen_index_check(r[no-i-1],l,11,i,
          (pc.ntag_chk_range IN n.tags) & ~(dcl.CHK IN md));
      out.wr(']');
    END;

    IF (n.type.mode=pc.ty_array_of) & (p>=11) THEN out.wr(')') END;
  ELSE
    IF n.type.mode=pc.ty_array_of THEN
      IF p>=11 THEN out.wr('(') END;
      gen_value(l,11,{dcl.BSA}); out.ws("+(");
    ELSE
      gen_value(l,14,{dcl.BSA}); out.wr("[");
    END;

    FOR i:=1 TO no-1 DO out.wr('(') END;
    gen_index_check(r[no-1],l,0,0,
          (pc.ntag_chk_range IN n.tags) & ~(dcl.CHK IN md));
    FOR i:=1 TO no-1 DO
      out.ws(')*'); gen_len(l,i,l.type.inx,12); out.wr('+');
      gen_index_check(r[no-i-1],l,11,i,
          (pc.ntag_chk_range IN n.tags) & ~(dcl.CHK IN md));
    END;
    IF n.type.mode=pc.ty_array_of THEN
      out.wr(')');
      FOR i:=0 TO SHORT(n.type.len-1) DO
        out.wr('*'); gen_len(l,no+i,l.type.inx,12);
      END;
      IF p>=11 THEN out.wr(')') END;
    ELSE
      out.ws("]");
    END;
  END;
END gen_index;

PROCEDURE gen_lset_aggregate(n: pc.NODE; p: INTEGER);
  VAR l,f,t: pc.NODE; nm: STR;
BEGIN
  ASSERT(n.type.mode=pc.ty_set);
  ASSERT(dcl.is_c_arr(n.type));
  l:=n.l;
  dcl.make_temp_var(n.type,nm);
  IF l#NIL THEN
    out.wr('(');
  ELSE
    IF p>13 THEN out.wr('(') END;
    out.ws("(X2C_LSET)");
  END;
  out.wf("memset(%s,0,",nm); dcl.gen_sizeof(n.type); out.wr(')');
  IF l=NIL THEN
    IF p>13 THEN out.wr(')') END;
    RETURN;
  END;
  REPEAT
    out.wr(',');
    IF l.mode=pc.nd_node THEN
      f:=l.l; t:=l.r;
      out.wf("X2C_LONGSET(%s,",nm);
      gen_value_minus_min(f,0,n.type.min,TRUE);
      out.wr(',');
      gen_value_minus_min(t,0,n.type.min,TRUE);
      out.wf(",%d)",n.type.len);
    ELSE
      f:=l;
      out.wf("X2C_INCL(%s,",nm);
      gen_value_minus_min(f,0,n.type.min,TRUE);
      out.wf(",%d)",n.type.len);
    END;
    l:=l.next;
  UNTIL l=NIL;
  out.wr(')');
END gen_lset_aggregate;

PROCEDURE gen_set_aggregate(n: pc.NODE; p: INTEGER; md: SET);
  VAR l,f,t: pc.NODE; nm: STR;
BEGIN
  ASSERT(n.type.mode=pc.ty_set);
  IF dcl.is_c_arr(n.type) THEN gen_lset_aggregate(n,p); RETURN END;
  l:=n.l;
  IF l=NIL THEN out.wr('0'); RETURN END;
  IF p>5 THEN out.wr('(') END;
  REPEAT
    IF l#n.l THEN out.wr('|') END;
    IF l.mode=pc.nd_node THEN
      nms.x2c(nms.nm_set_range,nm); out.wf("%s(",nm);
      f:=l.l; t:=l.r;
      gen_value_minus_min(f,0,n.type.min,TRUE);
      out.wr(',');
      gen_value_minus_min(t,0,n.type.min,TRUE);
      out.wf(",%d)",n.type.len);
    ELSE
      out.wr("1"); dcl.suffix(n.type); out.ws("<<");
      gen_index_check(l,n,10,0,
      	(pc.ntag_chk_range IN n.tags) & ~(dcl.CHK IN md));
    END;
    l:=l.next;
  UNTIL l=NIL;
  IF p>5 THEN out.wr(')') END;
END gen_set_aggregate;

PROCEDURE gen_array_aggregate(n: pc.NODE);
  VAR l,v: pc.NODE; nm: STR; i,j: LONGINT;
BEGIN
  ASSERT(n.type.mode=pc.ty_array);
  l:=n.l;
  dcl.make_temp_var(n.type,nm);
  out.wr('(');
  i:=0;
  WHILE l#NIL DO
    IF l.mode=pc.nd_node THEN
      ASSERT(l.r.mode=pc.nd_value);
      j:=l.r.val.get_integer();
      v:=l.l;
    ELSE
      j:=1; v:=l;
    END;
    IF v.type.mode = pc.ty_array THEN
      WHILE j > 0 DO
        out.wf("memcpy(%s[%d], ",nm,i);
        gen_value(v,1,{});
        out.wf(", sizeof(%s[%d])), ", nm, i);
        INC(i); DEC(j);
      END;
    ELSE
      WHILE j>0 DO
        out.wf("%s[%d] = ",nm,i);
        INC(i); DEC(j);
      END;
      gen_value(v,1,{});
      out.ws(", ");
    END;
    l:=l.next;
  END;
  out.wf("%s)",nm);
  ASSERT(i=n.type.len);
END gen_array_aggregate;

PROCEDURE gen_value_sequence(n: pc.NODE; md: SET);
  VAR sz,sz0: LONGINT; nm,str: STR; l: pc.NODE;
BEGIN
  ASSERT(~(dcl.LVL IN md));
  sz0:=get_sequence_len(n);
  IF sz0=0 THEN out.wr('0'); RETURN END;
  dcl.make_temp_arr(dcl.SEQU_T,sz0,nm);
  out.wr('(');
  l:=n.l; sz:=0;
  WHILE l#NIL DO
    IF l.type.mode IN pc.REALs THEN
      nms.x2c(nms.nm_longreal,str);
      out.wf("*(%s*)&%s[%d] = ",str,nm,sz); INC(sz,2);
      gen_value(l,1,{});
    ELSIF l.type.mode IN pc.SEQ_ARRs THEN
      out.wf("%s[%d].adr = ",nm,sz); INC(sz);
      gen_value(l,1,{dcl.REF,dcl.ANY});
      IF ~is_result_of_cptr_deref(l) THEN
        out.wf(", %s[%d].val = 0, %s[%d].val = ",nm,sz,nm,sz+1); INC(sz,2);
        gen_size(l,0,11); out.ws("-1");
      END;
    ELSIF l.type.mode IN pc.SEQ_PTRs THEN
      out.wf("%s[%d].adr = ",nm,sz); INC(sz); gen_value(l,1,{});
    ELSE
      ASSERT(l.type.mode IN pc.TY_SET{pc.ty_longcard,pc.ty_longint});
      out.wf("%s[%d].val = ",nm,sz); INC(sz); gen_value(l,13,{});
    END;
    out.ws(", ");
    l:=l.next;
  END;
  ASSERT(sz=sz0);
  IF dcl.REF IN md THEN
    gen_type_cast("*",n.type);
  ELSIF dcl.BSA IN md THEN
    gen_type_cast("*",dcl.get_base_type(n.type));
  ELSE
    gen_type_cast("",n.type);
  END;
  out.ws(nm); out.wr(")");
END gen_value_sequence;

PROCEDURE gen_record_aggregate(n: pc.NODE);
  VAR nm: STR; l: pc.NODE;
  PROCEDURE gen_fld(f: pc.OBJECT);
    VAR fld: STR;
  BEGIN
    IF f.type.mode = pc.ty_array THEN
      out.ws ("memcpy(");
      dcl.o_usage (f, fld(*=>*));
      dcl.gen_field_path (f.ext (nms.INFO).nxt);
      out.wf ("%s.%s, ", nm, fld);
      gen_value (l, 1, {});
      out.wf (", sizeof(%s.%s))", nm, fld);
    ELSE
      out.wf("%s.",nm);
      dcl.o_usage(f,fld);
      dcl.gen_field_path(f.ext(nms.INFO).nxt);
      out.wf("%s = ",fld);
      gen_value(l,1,{});
    END;
    out.ws(", ");
    l:=l.next;
  END gen_fld;
  PROCEDURE gen_fld_seq(f: pc.OBJECT);
    VAR v: pc.NODE;
  BEGIN
    WHILE f#NIL DO
      IF f.mode=pc.ob_header THEN
        ASSERT(f.val.mode=pc.nd_case);
        ASSERT(l.mode=pc.nd_value);
        v:=f.val.l;
        dcl.search_record_variant(v,l.val);
        ASSERT(v#NIL);
        IF f.val.obj#NIL THEN gen_fld(f.val.obj) ELSE l:=l.next END;
        gen_fld_seq(v.obj);
      ELSE
        gen_fld(f);
      END;
      f:=f.next;
    END;
  END gen_fld_seq;
  PROCEDURE gen_level(t: pc.STRUCT);
  BEGIN
    IF t.base#NIL THEN gen_level(t.base) END;
    gen_fld_seq(t.prof);
  END gen_level;
BEGIN
  ASSERT(n.type.mode=pc.ty_record);
  dcl.make_temp_var(n.type,nm);
  out.wr('(');
  l:=n.l;
  gen_level(n.type);
  ASSERT(l=NIL);
  out.wf("%s)",nm);
END gen_record_aggregate;

PROCEDURE rem_str_extension(VAR n: pc.NODE): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN
  r:=	(n.mode=pc.nd_unary) &
	(n.sub=pc.su_conv) &
	(n.type.mode IN pc.ARRs) &
	(n.l.type.mode IN pc.ARRs) &
	dcl.is_char(n.type.base) &
	dcl.is_char(n.l.type.base);
  IF r THEN n:=n.l END;
  RETURN r;
END rem_str_extension;

PROCEDURE gen_assign*(n: pc.NODE; p: INTEGER; md: SET);
  VAR
    r,l	: pc.NODE;
    lt	: pc.STRUCT;
    nm  : STR;
    cv  : BOOLEAN;
    st  : BOOLEAN;
    zr  : BOOLEAN;
BEGIN
(* env.info.print("%{}\n",md); *)
  ASSERT(md-{dcl.REF,dcl.ANY,dcl.BLN} = {});
  IF n.obj#NIL THEN lt:=n.obj.type ELSE lt:=n.l.type END;
  r:=n.r; l:=n.l;
  IF dcl.is_c_arr(lt) THEN
    ASSERT((dcl.REF IN md) OR (dcl.BSA IN md));
    st:=rem_str_extension(r);
    IF n.type.mode=pc.ty_void THEN cv:=FALSE;
    ELSIF st THEN cv:=(dcl.REF IN md) & ~(dcl.ANY IN md);
    ELSE cv:=~(dcl.ANY IN md);
    END;
    IF (p>13) & cv THEN out.wr('(') END;
    IF cv THEN
      IF dcl.BSA IN md THEN
	gen_type_cast("*",dcl.get_base_type(n.type));
      ELSE
	gen_type_cast("*",n.type);
      END;
    END;
    IF st THEN
      zr:=(n.type.mode=pc.ty_void) & (r.mode=pc.nd_value);
      IF zr THEN
        zz_tmp.index_get(0,r.val);
        zr:=zz_tmp.is_zero();
      END;
      IF zr THEN
        ASSERT(p<0);
        IF n.obj#NIL THEN gen_value_usage(n.obj,14,{dcl.BSA,dcl.LVL});
        ELSE gen_value(l,14,{dcl.BSA,dcl.LVL});
        END;
        out.ws("[0] = 0");
      ELSE
        nms.x2c(nms.nm_strncpy,nm); out.wf("%s(",nm);
        IF n.obj#NIL THEN gen_value_usage(n.obj,0,{dcl.BSA,dcl.LVL});
        ELSE gen_value(l,0,{dcl.BSA,dcl.LVL});
        END;
        out.wr(',');
        gen_value(r,0,{dcl.BSA}); out.wr(',');
        IF n.obj#NIL THEN gen_len_usage(n.pos,n.obj,0,pcO.size_t);
        ELSE gen_len(l,0,pcO.size_t,0);
        END;
        out.wr(')');
      END;
    ELSE
      nms.x2c(nms.nm_memcpy,nm); out.wf("%s(",nm);
      IF n.obj#NIL THEN gen_value_usage(n.obj,0,{dcl.REF,dcl.ANY,dcl.LVL});
      ELSE gen_value(n.l,0,{dcl.REF,dcl.ANY,dcl.LVL});
      END;
      out.wr(','); gen_value(n.r,0,{dcl.REF,dcl.ANY}); out.wr(',');
      gen_size(r,0,0); out.wr(')');
    END;
    IF (p>13) & cv THEN out.wr(')') END;
  ELSIF (dcl.REF IN md) & (n.obj#NIL) THEN
    out.wr("(");
    gen_assign(n,1,{});
    out.wr(",");
    gen_value_usage(n.obj,1,md);
    out.wr(")");
  ELSIF dcl.REF IN md THEN
    dcl.make_temp_var(n.type,nm);
    out.wf("(%s = ",nm);
    gen_assign(n,1,md-{dcl.REF,dcl.ANY});
    out.wf(",&%s)",nm);
  ELSE
    IF p>=1 THEN out.wr('(') END;
    IF n.obj#NIL THEN
      gen_value_usage(n.obj,1,{dcl.LVL});
    ELSE
      gen_value(n.l,1,{dcl.LVL})
    END;
    out.ws(" = ");
    gen_value(n.r,1,{});
    IF p>=1 THEN out.wr(')') END;
  END;
END gen_assign;

PROCEDURE gen_value*(n: pc.NODE; p: INTEGER; md: SET);
  PROCEDURE chk_adr(): BOOLEAN;
  BEGIN
    IF ~(dcl.REF IN md) THEN RETURN FALSE END;
    IF dcl.is_c_arr(n.type) THEN
      ASSERT(dcl.ANY IN md);
      md:=md-{dcl.REF,dcl.ANY}+{dcl.BSA}; RETURN FALSE
    END;
    IF p>13 THEN out.wr('(') ELSIF p=13 THEN out.wr(' ') END;
    out.wr('&'); gen_value(n,13,md-{dcl.REF,dcl.ANY}+{dcl.LVL});
    IF p>13 THEN out.wr(')') END;
    RETURN TRUE;
  END chk_adr;
  PROCEDURE chk_vref(): BOOLEAN;
    VAR nm: STR;
  BEGIN
    IF ~(dcl.REF IN md) THEN RETURN FALSE END;
    IF dcl.is_c_arr(n.type) THEN RETURN chk_adr() END;
    IF dcl.LVL IN md THEN env.errors.Error(n.pos,1008) END;
    dcl.make_temp_var(n.type,nm);
    out.wf("(%s = ",nm);
    gen_value(n,1,md-{dcl.REF,dcl.ANY,dcl.LVL});
    out.wf(",&%s)",nm);
    RETURN TRUE;
  END chk_vref;
  PROCEDURE chk_neg(): BOOLEAN;
  BEGIN
    IF ~(dcl.NEG IN md) THEN RETURN FALSE END;
    ASSERT({dcl.LVL,dcl.REF}*md={});
    IF p>13 THEN out.wr('(') END;
    out.wr('!'); gen_value(n,13,md-{dcl.NEG}+{dcl.BLN});
    IF p>13 THEN out.wr(')') END;
    RETURN TRUE;
  END chk_neg;
  VAR
    l	: pc.NODE;
    b, c: BOOLEAN;
    nm	: pc.NAME;
    inf : nms.INFO;
    i   : LONGINT;
BEGIN
  CASE n.mode OF
    |pc.nd_var      :
      IF chk_neg() THEN RETURN END;
      gen_value_usage(n.obj,p,md);
    |pc.nd_proc     :
      IF chk_vref() THEN RETURN END;
      dcl.o_usage(n.obj,nm); out.ws(nm);
    |pc.nd_field    :
      dcl.o_usage(n.obj,nm); (* <- must be before next IF *)
      ASSERT(cc.otag_declared IN n.obj.tags);
      ASSERT(n.obj.mode IN pc.FIELDs);
      IF (dcl.REF IN md) &
         (cc.otag_bitfield IN n.obj.tags)
      THEN
        ASSERT(~(dcl.NEG IN md));
        IF p>13 THEN out.wr("(") END;
        IF ~(dcl.ANY IN md) THEN gen_type_cast("*",n.type) END;
	out.wr("&");
	IF gen_value_or_address(n.l,14,{dcl.REF}+md*{dcl.LVL}) THEN out.ws("->");
	ELSE out.wr(".");
	END;
        IF cc.otag_bitfield_nm IN n.obj.tags THEN
	  inf:=n.obj.ext(nms.INFO).nxt;
	  dcl.gen_field_path(inf.nxt);
	  out.ws(inf.name^);
        ELSE
          dcl.gen_field_path(n.obj.ext(nms.INFO).nxt);
          out.ws(nm);
        END;
        IF p>13 THEN out.wr(")") END;
      ELSIF (dcl.REF IN md) & (n.obj.attr#NIL) THEN
        ASSERT(~(dcl.NEG IN md));
        IF ~(dcl.ANY IN md) THEN
          IF p>13 THEN out.wr("(") END;
          gen_type_cast("*",n.type);
          gen_value(n,13,md+{dcl.ANY});
          IF p>13 THEN out.wr(")") END;
        ELSE
          IF p>=11 THEN out.wr('(') END;
          i:=n.obj.attr(pc.NODE).l.val.get_integer();
          out.ws("(long *)");
          gen_value(n.l,13,md);
          out.wf("+%d",i);
          IF p>=11 THEN out.wr(')') END;
        END;
      ELSIF chk_adr() THEN
        (* nothing *)
      ELSIF chk_neg() THEN
        (* nothing *)
      ELSE
        IF gen_value_or_address(n.l,14,{dcl.REF}+md*{dcl.LVL}) THEN out.ws("->");
        ELSE out.wr(".");
        END;
        dcl.gen_field_path(n.obj.ext(nms.INFO).nxt);
        out.ws(nm);
      END;
    |pc.nd_index    :
      IF chk_adr() THEN RETURN END;
      IF chk_neg() THEN RETURN END;
      gen_index(n,p,md);
    |pc.nd_deref    :
      IF chk_neg() THEN RETURN END;
      IF n.type.mode=pc.ty_array_of THEN
	dcl.t_def(n.l.type.base);
	IF chk_adr() THEN RETURN END;
	IF pc.ntag_chk_range IN n.tags THEN gen_a_chk(n.l);
	ELSE gen_value(n.l,14,{});
	END;
	ASSERT(dcl.BSA IN md);
        IF n.l.type.flag # pc.flag_c THEN
          nms.x2c(nms.nm_dynarr_addr,nm);
          out.ws("->"); out.ws(nm);
        END;
      ELSE
        c:= ~(dcl.REF IN md) & (env.config.Option("GENPTRTOARR") OR (n.type.mode # pc.ty_array));
        b:=(p>=13) & c;
	IF b THEN out.wr('(') END;
        IF c THEN
	  IF p=12 THEN out.wr(' ') END; (* avoid '/*' *)
          out.wr('*');
	END;
	IF pc.ntag_chk_range IN n.tags THEN gen_a_chk(n.l);
	ELSIF b THEN gen_value(n.l,13,{});
	ELSE gen_value(n.l,p,{});
	END;
	IF b THEN out.wr(')') END;
      END;
    |pc.nd_replace:
      IF chk_neg() THEN RETURN END;
      IF (dcl.LVL IN md) & ({dcl.REF,dcl.BSA}*md={}) THEN
	IF p>13 THEN out.wr('(') END;
	out.ws("*("); gen_value(n.l,0,{}); out.wr(',');
	gen_value(n.r,0,md+{dcl.REF}); out.wr(')');
	IF p>13 THEN out.wr(')') END;
      ELSE
	IF p>=0 THEN out.wr('(') END;
	gen_value(n.l,0,{}); out.wr(','); gen_value(n.r,0,md);
	IF p>=0 THEN out.wr(')') END;
      END;
    |pc.nd_binary:
      IF chk_vref() THEN RETURN END;
      gen_value_binary(n,p,md);
    |pc.nd_unary:
      IF (n.sub=pc.su_cast) OR
	 (n.sub=pc.su_conv) &
	 ~(pc.ntag_chk_range IN n.tags) &
	 conv_is_cast(n.l.type,n.type)
      THEN
	gen_value_cast(n,p,md);
      ELSE
	IF chk_vref() THEN RETURN END;
	gen_value_unary(n,p,md);
      END;
    |pc.nd_value:
      IF chk_vref() THEN RETURN END;
      IF chk_neg() THEN RETURN END;
      l:=val_node(n.val,dcl.CHK IN md);
      IF l#NIL THEN
        gen_value(l,p,md);
      ELSIF n.type.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque} THEN
         IF n.val.is_zero() THEN
           out.wr("0");
         ELSE
           IF p>13 THEN out.wr("(") END;
           gen_type_cast("",n.type);
           dcl.const_aggregate(n.val,n.type,13,dcl.CNS IN md,dcl.CCH IN md);
           IF p>13 THEN out.wr(")") END;
         END;
      ELSIF pc.ntag_hex IN n.tags THEN
        dcl.const_hex(n.val,n.type,p);
      ELSE
        dcl.const_aggregate(n.val,n.type,p,dcl.CNS IN md,dcl.CCH IN md);
      END;
    |pc.nd_call:
      IF chk_vref() THEN RETURN END;
      IF chk_neg() THEN RETURN END;
      gen_call(n,p,md);
    |pc.nd_assign:
      IF chk_neg() THEN RETURN END;
      gen_assign(n,p,md);
    |pc.nd_sequence:
      gen_value_sequence(n,md);
    |pc.nd_guard,pc.nd_eguard:
      IF (n.l.mode=pc.nd_deref) OR (n.type.mode=pc.ty_pointer) THEN
	ASSERT(n.type.flag=pc.flag_o2);
	IF p>13 THEN out.wr('(') END;
        IF n.l.mode=pc.nd_deref THEN
	  IF ~(dcl.REF IN md) THEN out.wr('*') ELSE EXCL(md,dcl.REF) END;
	  l:=n.l.l; gen_type_cast("*",n.type);
        ELSE
          l:=n.l;
          ASSERT(l.type.mode=pc.ty_pointer);
          IF (dcl.LVL IN md) & ~(dcl.REF IN md) THEN
            out.wr('*'); INCL(md,dcl.REF);
          END;
	  IF dcl.REF IN md THEN gen_type_cast("*",n.type);
	  ELSE gen_type_cast("",n.type);
          END;
	END;
        ASSERT(l.type.mode=pc.ty_pointer);
	IF pc.ntag_chk_range IN n.tags THEN
          IF {dcl.REF,dcl.LVL}*md#{} THEN
	    IF ~(dcl.REF IN md) THEN out.wr('*'); INCL(md,dcl.REF) END;
            out.wr('(');
            dcl.make_temp_ptr(l.type,nm);
            out.ws(nm); out.ws('=');
            gen_value(l,0,md); out.wr(',');
            IF n.mode=pc.nd_guard THEN out.ws("X2C_GUARDP(");
            ELSE out.ws("X2C_GUARDPE(");
            END;
            out.wr('*'); out.ws(nm); out.wr(',');
	    gen_type_name(n.type.base,0); out.ws('),');
            out.ws(nm); out.wr(')');
          ELSE
            IF n.mode=pc.nd_guard THEN out.ws("X2C_GUARDP(");
            ELSE out.ws("X2C_GUARDPE(");
            END;
            gen_value(l,0,md); out.wr(',');
	    gen_type_name(n.type.base,0); out.wr(')');
          END;
	ELSE
	  gen_value(l,13,md);
	END;
	IF p>13 THEN out.wr(')') END;
      ELSE
	IF p>13 THEN out.wr('(') END;
	IF ~(dcl.REF IN md) THEN out.wr('*') END;
        gen_type_cast("*",n.type);
	IF pc.ntag_chk_range IN n.tags THEN
	  ASSERT(n.l.mode=pc.nd_var);
	  ASSERT(n.l.obj.mode=pc.ob_varpar);
	  ASSERT(n.l.obj.type.mode=pc.ty_record);
	  IF n.mode=pc.nd_guard THEN out.ws("(X2C_GUARDV(");
	  ELSE out.ws("(X2C_GUARDVE(");
	  END;
	  gen_type(n.l,0); out.wr(',');
	  gen_type_name(n.type,0); out.ws("),");
          IF ~dcl.is_pointer(n.l.obj,dcl.cur_proc) THEN out.wr('&') END;
	  dcl.o_usage(n.l.obj,nm); out.wf("%s)",nm);
	ELSE
	  gen_value(n.l,13,{dcl.REF,dcl.ANY});
	END;
	IF p>13 THEN out.wr(')') END;
      END;
    |pc.nd_aggregate:
      IF chk_vref() THEN RETURN END;
      IF chk_neg() THEN RETURN END;
      IF n.type.mode=pc.ty_set THEN gen_set_aggregate(n,p,md);
      ELSIF n.type.mode=pc.ty_array THEN gen_array_aggregate(n);
      ELSIF n.type.mode=pc.ty_record THEN gen_record_aggregate(n);
      ELSE env.errors.Error(n.pos,1011);
      END;
    |pc.nd_prot:
      IF chk_vref() THEN RETURN END;
      out.ws("X2C_PROT()");
    |pc.nd_lconv:
      gen_value_cast(n,p,md);
    |pc.nd_if:
      IF p>=2 THEN out.wr('(') END;
      gen_value(n.l,2,{});
      out.ws(" ? ");
      gen_value(n.r.l,2,md);
      out.ws(" : ");
      gen_value(n.r.r,2,md);
      IF p>=2 THEN out.wr(')') END;
  ELSE
    env.errors.Error(n.pos,1008);
  END;
END gen_value;

PROCEDURE ini*;
BEGIN
  zz_gvm:=dcl.val(0);
  zz_tmp:=dcl.val(0);
  zz_128:=dcl.val(128);
  zz_m32:=dcl.val(-32);
  zz_32 :=dcl.val(32);
  zz_one:=dcl.val(1);
END ini;

PROCEDURE exi*;
BEGIN
  zz_gvm:=NIL;
  zz_tmp:=NIL;
  zz_128:=NIL;
  zz_m32:=NIL;
  zz_32:=NIL;
  zz_one:=NIL;
END exi;

BEGIN
  dcl.gen_value:=gen_value;
END ccE.
