(** Copyright (c) 1991,97 XDS Ltd, Russia. All Rights Reserved. *)
(** Definitions *)
MODULE ccDef;
(** Sem 22-Sep-93. *)

(* Modifications:
   08/Feb/96 Ned  2.12  op_gendll - X2C_DLL_TAG is generated instead of
                        X2C_IMPORT/EXPORT_FUNC/DATA for externals.
   17/Mar/96 Ned  2.12  Comments copying is improved.
   03/Apr/96 Ned  2.14  record_definition: process ccRec.m_dummy.
*)

IMPORT
  pc :=pcK,
  env:=xiEnv,
  xfs:=xiFiles,
  cc :=ccK,
  nms:=ccN,
  out:=ccL,
  dcl:=ccDcl,
  cmt:=ccComments,
  ccr:=ccRec,
  seg:=ccSeg,
  exp:=ccE,
  <* IF PCVIS THEN *> pcVis, <* END *>
  xcStr;

TYPE
  STR = dcl.STR;
  RecListPtr = POINTER TO RecList;
  RecList = RECORD
    rec : pc.STRUCT;
    next: RecListPtr;
  END;

VAR
  udf_txt        : out.STR; (* inserted at the end of procedure *)
  gen_main       : BOOLEAN;
  md_desc        : STR; (* current module type descriptor *)
  last_td        : pc.OBJECT; (* last defined type descriptor *)
  eq_glomark     : env.String;
  gen_size_usage*: PROCEDURE(o: pc.OBJECT; dim: INTEGER; p: INTEGER);
  ret_emul      *: BOOLEAN; (* TRUE если нужно сгенерить код для освобождения
                                локальных переменных динамического размера
                                или код удаления ловушки,
                                т.е. нельзя генерить return
                                в текущей процедуре
                            *)
  gen_sequence*  : PROCEDURE(n: pc.NODE; lev: INTEGER;
                        VAR dd: BOOLEAN;
                        VAR rt: BOOLEAN;
                        nrt,trt: BOOLEAN;
                        loop: pc.NODE);

  reclist        : RecListPtr;

  unnamed_anonymous_structs : BOOLEAN;

(*------------------------ Definitions -----------------------------------*)

PROCEDURE offset_definition(bs-,rec-: ARRAY OF CHAR; t: pc.STRUCT);
  PROCEDURE ofs(nm-: ARRAY OF CHAR);
  BEGIN
    out.sp(1);
    IF rec#"" THEN out.wf("X2C_OFS(%s,%s),\n",rec,nm);
    ELSE out.wf("&%s,\n",nm);
    END;
  END ofs;
  VAR bf0,bf1,nm: STR;
BEGIN
  ASSERT(t.flag=pc.flag_o2);
  IF t.mode=pc.ty_record THEN
    dcl.o_second_name(t.obj,1,"_desc",nm);
    out.sp(1); out.wf("X2C_OFS_REC,\n");
    out.sp(1); out.wf("&%s,\n",nm);
    ofs(bs);
  ELSIF t.mode=pc.ty_pointer THEN
    ofs(bs);
  ELSIF t.mode IN pc.ARRs THEN
    out.sp(1); out.wf("X2C_OFS_ARR,\n");
    COPY(bs,bf0); COPY(bs,bf1);
    WHILE t.mode IN pc.ARRs DO
      out.ff(nm,"[%d]",t.len-1);
      nms.strcat(bf1,nm);
      nms.strcat(bf0,"[0]");
      t:=t.base;
    END;
    ofs(bf1);
    offset_definition(bf0,rec,t);
  ELSE ASSERT(FALSE);
  END;
END offset_definition;

PROCEDURE out_str(s: ARRAY OF CHAR);
  VAR b: BOOLEAN;
BEGIN
  out.wr('"'); out.disable_lf(b);
  out.ws(s);
  out.wr('"'); out.restore_lf(b);
END out_str;

PROCEDURE is_o2_var(o: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (o.mode=pc.ob_var) &
         (o.type.flag=pc.flag_o2) &
         (o.type.mode IN (pc.TY_SET{pc.ty_record,pc.ty_pointer}+pc.ARRs));
END is_o2_var;

PROCEDURE ini_var_addr(l: pc.OBJECT);
(* генерит инициализацию указателей для переменных,
   у которых явно задан адрес
*)
  VAR nm: STR;
BEGIN
  WHILE l#NIL DO
    IF (l.mode=pc.ob_var) & (l.attr#NIL) THEN
      dcl.o_usage(l,nm);
      out.sp(1); out.wf("%s = ",nm);
      dcl.type_designator(nm,"*",l.type);
      out.wr('('); out.ws(nm); out.wr(')');
      exp.gen_value(l.attr(pc.NODE),13,{});
      out.wf(";\n");
    END;
    l:=l.next;
  END;
END ini_var_addr;

PROCEDURE sl1_glo_mark;
BEGIN
  IF eq_glomark=NIL THEN RETURN END;
  IF eq_glomark^="" THEN RETURN END;
  out.wf("/* %s */\n",eq_glomark^);
END sl1_glo_mark;

PROCEDURE out_type_bases(r: pc.STRUCT);
(* Static initialization *)
  VAR nm: STR; i: LONGINT;

  PROCEDURE out_base(r: pc.STRUCT);
  BEGIN
    IF (r=NIL) OR (r.flag # pc.flag_o2) THEN RETURN END;
    out_base(r.base);
    IF i>0 THEN out.ws(", ") END;
    ASSERT(r.mode=pc.ty_record);
    dcl.o_usage(r.obj,nm); (* force declaration *)
    dcl.o_second_name(r.obj,1,"_desc",nm); out.wf("&%s",nm);
    INC(i);
  END out_base;

  VAR x: RecListPtr;
BEGIN
  i:=0; out.ws("{ ");
  IF cc.op_gendll THEN
    NEW(x);  x.rec :=r;  x.next:=reclist;
    reclist:=x;

    (* to force declaration only *)
    WHILE (r#NIL) & (r.flag=pc.flag_o2) DO
      ASSERT(r.mode=pc.ty_record);
      dcl.o_usage(r.obj,nm);
      dcl.o_second_name(r.obj,1,"_desc",nm);
      r:=r.base;
    END;
  ELSE
    out_base(r);
  END;
  WHILE i <= pc.code.max_ext_lev DO
    IF i>0 THEN out.ws(", ") END;
    out.ws("0");
    INC(i)
  END;
  out.ws(" }");
END out_type_bases;

PROCEDURE init_bases(r: pc.STRUCT);
(* Dynamic initialization for GENDLL *)
  VAR nm,ds: STR;
BEGIN
  ASSERT(cc.op_gendll);
  dcl.o_second_name(r.obj,1,"_desc",ds);
  WHILE (r#NIL) & (r.flag=pc.flag_o2) DO
    ASSERT(r.mode=pc.ty_record);
    dcl.o_usage(r.obj,nm); (* force declaration *)
    dcl.o_second_name(r.obj,1,"_desc",nm);
    out.sp(1); out.wf("%s.base[%d] = &%s;\n",ds,r.len,nm);
    r:=r.base;
  END;
END init_bases;

PROCEDURE init_records_bases;
BEGIN
  WHILE reclist # NIL DO
    init_bases(reclist.rec);
    reclist:=reclist.next;
  END;
END init_records_bases;

PROCEDURE type_desc_definition(t: pc.STRUCT);
  VAR
    bf     : ARRAY 1024 OF pc.OBJECT;
    bf_cnt : LONGINT;
    type_nm: STR;
    md     : pc.OBJECT;

  PROCEDURE out_procs;
    VAR i,j: LONGINT; nm: STR;
  BEGIN
    j:=bf_cnt-1;
    IF j<0 THEN INC(j) END;
    FOR i:=0 TO j DO
      out.sp(1);
      IF bf[i]=NIL THEN out.ws("0");
      ELSE dcl.o_usage(bf[i],nm); out.wf("(X2C_PROC)%s",nm);
      END;
      IF i#j THEN out.wr(',') END;
      out.wl;
    END;
  END out_procs;

  PROCEDURE out_ptrs_rec(rec-: ARRAY OF CHAR; r: pc.STRUCT);
    VAR o: pc.OBJECT; nm: STR;
  BEGIN
    IF (r.base#NIL) & (r.base.flag=pc.flag_o2) THEN
      out_ptrs_rec(rec,r.base);
    END;
    o:=r.prof;
    WHILE o#NIL DO
      ASSERT(o.mode IN pc.FIELDs);
      IF (o.type.flag=pc.flag_o2) &
         (o.type.mode IN (pc.TY_SET{pc.ty_record,pc.ty_pointer}+pc.ARRs))
      THEN
        dcl.o_usage(o,nm);
        offset_definition(nm,rec,o.type);
      END;
      o:=o.next;
    END;
  END out_ptrs_rec;

  PROCEDURE out_ptrs(rec-: ARRAY OF CHAR);
  BEGIN
    out_ptrs_rec(rec,t);
    out.sp(1); out.wf("X2C_OFS_END\n");
  END out_ptrs;

  PROCEDURE get_procs(b: pc.STRUCT);
    VAR o: pc.OBJECT; i: LONGINT;
  BEGIN
    bf_cnt:=0;
    FOR i:=0 TO LEN(bf)-1 DO bf[i]:=NIL END;
    WHILE b#NIL DO
      o:=b.mem;
      WHILE o#NIL DO
        IF o.type.len>=bf_cnt THEN bf_cnt:=o.type.len+1 END;
        IF bf[o.type.len]=NIL THEN bf[o.type.len]:=o END;
        o:=o.next;
      END;
      b:=b.base;
    END;
  END get_procs;

  VAR ds,ps,of,ls: STR;
BEGIN
  md:=pc.mods[t.mno];
  IF md_desc="" THEN dcl.o_second_name(md,2,"_desc",md_desc) END;
  ASSERT(t.obj#NIL);
  ASSERT(t.mode=pc.ty_record);
  ASSERT(t.obj.mno=dcl.cur_mod.mno);
  ASSERT(t.flag=pc.flag_o2);
  IF NOT dcl.t_usage(t,type_nm,FALSE,FALSE) THEN ASSERT(FALSE) END;
  get_procs(t);
  dcl.o_second_name(t.obj,1,"_desc",ds);
  dcl.o_second_name(t.obj,2,"_offs",of);
  dcl.o_second_name(t.obj,3,"_proc",ps);
  out.wf("static X2C_PROC %s[] = {\n",ps); out_procs; out.wf("};\n");
  out.wf("static void * %s[] = {\n",of); out_ptrs(type_nm); out.wf("};\n");
  out.wf("struct X2C_TD_STR %s = {\n",ds);
  out.sp(1); out.wf("sizeof(%s), ",type_nm);
  out_str(t.obj.name^); out.wf(",\n");
  out.sp(1); out.wf("&%s, ",md_desc);
  IF last_td=NIL THEN
    out.ws("0, ");
  ELSE
    dcl.o_usage(last_td,ls);
    dcl.o_second_name(last_td,1,"_desc",ls);
    out.wf("&%s, ",ls);
  END;
  last_td:=t.obj;
  out.wf("%d, %d,\n",bf_cnt,t.len);
  out.sp(1); out_type_bases(t); out.wf(",\n");
  out.sp(1); out.wf("%s, %s, 0, 0, 0, &%s, 0x93678150l",ps,of,ds);
  IF NOT cc.op_krc THEN out.wr('u') END;
  out.wf("\n};\n");
END type_desc_definition;

PROCEDURE record_definition(t: pc.STRUCT);

  PROCEDURE normal_field(f: pc.OBJECT; lev: INTEGER; path: nms.INFO);
    VAR buf: STR;
  BEGIN
    IF cc.op_comments THEN cmt.next_comment(f.pos,lev) END;
    dcl.declarator(buf,f,FALSE);
    out.sp(lev); out.wf("%s;",buf);
    IF cc.op_comments THEN cmt.tail_comment(f.pos) END;
    out.wl;
    INCL(f.tags,cc.otag_declared);
    f.ext(nms.INFO).nxt:=path;
    (* nms.disable(t,fnm); ?!!!!! *)
  END normal_field;

  PROCEDURE bit_field(f: pc.OBJECT; lev: INTEGER; path: nms.INFO);
    VAR buf,bts: STR;
  BEGIN
    IF (f.type.mode IN pc.TY_SET{pc.ty_range,pc.ty_enum}) & (f.type.obj#NIL) THEN
      dcl.o_usage(f.type.obj,buf);
      out.ff(bts,"BITS_%s",buf);
    ELSIF f.type.mode=pc.ty_char THEN
      bts:="BITS_char";
    ELSIF f.type.mode=pc.ty_boolean THEN
      bts:="BITS_bool";
    ELSE
      out.ff(bts,"%d",f.attr(pc.NODE).val.get_integer());
    END;
    dcl.declarator(buf,f,FALSE);
    out.sp(lev); out.wf("%s : %s;",buf,bts);
    IF cc.op_comments THEN cmt.tail_comment(f.pos) END;
    out.wl;
    INCL(f.tags,cc.otag_declared);
    f.ext(nms.INFO).nxt:=path;
  END bit_field;

  PROCEDURE bit_field_or_filler(l: ccr.Node; lev,ofs: INTEGER; path: nms.INFO);
  BEGIN
    IF l.mode=ccr.m_filler THEN
      out.sp(lev); out.wf("int _%02d_%02d: %d;\n",l.offs,ofs,l.bits);
    ELSE
      bit_field(l.obj,lev,path);
    END;
  END bit_field_or_filler;

  PROCEDURE word_bck(l: ccr.Node; lev: INTEGER; path: nms.INFO);
    VAR ofs: INTEGER;
  BEGIN
    ASSERT(l#NIL);
    ofs:=ccr.WORD_BITS;
    REPEAT
      DEC(ofs,l.bits);
      bit_field_or_filler(l,lev,ofs,path);
      l:=l.next;
    UNTIL l=NIL;
  END word_bck;

  PROCEDURE word_fwd(l: ccr.Node; lev,ofs: INTEGER; path: nms.INFO);
  BEGIN
    IF l=NIL THEN RETURN END;
    DEC(ofs,l.bits);
    word_fwd(l.next,lev,ofs,path);
    bit_field_or_filler(l,lev,ofs,path);
  END word_fwd;

  PROCEDURE gen(n: ccr.Node; lev: INTEGER; path: nms.INFO);
    VAR nm: STR; l: ccr.Node; i: INTEGER; inf,inm: nms.INFO;
  BEGIN
    CASE n.mode OF
      |ccr.m_struct,ccr.m_union:
        out.sp(lev);
        IF n.mode=ccr.m_struct THEN
          out.ws("struct ");
        ELSE
          out.ws("union ");
        END;
        IF lev=0 THEN
          dcl.o_name(t.obj,nm);
          out.ws(nm); out.wr(' ');
          inf:=path;
          inm:=NIL;
        ELSIF unnamed_anonymous_structs OR
              ((n.mode=ccr.m_union) & cc.op_cpp) 
        THEN
          inf:=path;
          inm:=NIL;
        ELSE
          IF path=NIL THEN i:=1 ELSE i:=path.no+1 END;
          inf:=nms.make_name(nm,t,NIL,pc.ob_field,"_",i);
          inf.nxt:=path;
          inm:=inf;
        END;
        out.wf("{\n");
        l:=n.down;
        IF l = NIL THEN (* empty variant *)
          env.errors.Error (t.obj.pos, 1017);
        END;
        WHILE l#NIL DO gen(l,lev+1,inf); l:=l.next END;
        out.sp(lev); out.wr('}');
        IF inm#NIL THEN out.wr(' '); out.ws(inm.name^) END;
        out.wf(";\n");
      |ccr.m_dummy:
        nms.x2c(nms.nm_longint,nm);
        out.sp(lev); out.wf("%s _dummy_;\n",nm);
      |ccr.m_sl1:
      |ccr.m_field:
        (* word offset must be ignored *)
        normal_field(n.obj,lev,path);
      |ccr.m_filler:
        nms.x2c(nms.nm_longint,nm);
        out.sp(lev);
        IF n.size=1 THEN
          out.wf("%s _%d_;\n",nm,n.offs);
        ELSE
          out.wf("%s _%d_[%d];\n",nm,n.offs,n.size);
        END;
      |ccr.m_word:
        ASSERT(n.size=1);
        IF dcl.cur_mod.type.flag=pc.flag_bnrp THEN
          out.sp(lev); out.wf("// word %d\n",n.offs+1);
        ELSE
          out.sp(lev); INC(lev);
          out.ws("#ifdef X2C_XE"); out.wl;
        END;
        word_bck(n.down,lev,path);
        IF dcl.cur_mod.type.flag#pc.flag_bnrp THEN
          out.sp(lev-1); out.ws("#else"); out.wl;
          word_fwd(n.down,lev,ccr.WORD_BITS,path); DEC(lev);
          out.sp(lev); out.ws("#endif"); out.wl;
        END;
    END;
  END gen;

BEGIN
  out.wl;
  IF cc.op_comments THEN cmt.first_comment(t.pos,1,0) END;
  IF ~ (cc.otag_pub_defined IN t.obj.tags) THEN
    ccr.Record(t);
    gen(t.ext(nms.TREE).node(ccr.Node),0,NIL);
  END;
  IF ~ (cc.otag_pri_defined IN t.obj.tags) &
     ~ dcl.gen_def &
     (t.flag=pc.flag_o2)
  THEN
    type_desc_definition(t);
  END;
  IF cc.op_comments THEN cmt.last_comment(t.end,0) END;
  out.wl;
END record_definition;

PROCEDURE type_definition(o: pc.OBJECT);
BEGIN
  IF o.type.obj#o THEN
    (* nothing *)
  ELSIF o.type.mode=pc.ty_record THEN
    record_definition(o.type);
  END;
END type_definition;

PROCEDURE var_definition(o: pc.OBJECT);
  VAR buf: STR;
BEGIN
  IF dcl.gen_def THEN RETURN END;
  dcl.object_declaration(o);
  IF cc.otag_pri_defined IN o.tags THEN RETURN END;
  ASSERT(o.lev=0);
  ASSERT(o.mno=dcl.cur_mod.mno);
  IF ~cc.op_krc & (o.mode = pc.ob_var) & (pc.otag_volatile IN o.tags) THEN
    out.ws("volatile ");
  END;
  dcl.declarator(buf,o,FALSE);
  IF o.is_public() THEN
(*Ned v2.12    IF cc.op_gendll THEN out.ws("X2C_EXPORT_DATA ") END;*)
  ELSE
    out.ws("static ");
  END;
  IF o.mode=pc.ob_cons THEN
    out.wf("%s = ",buf);
    exp.gen_value(o.val,1,{dcl.CNS});
    out.wf(";\n");
  ELSIF o.attr#NIL THEN
    ASSERT(o.mode=pc.ob_var);
    out.wf("%s ",buf);
    dcl.var_addr_expression(o);
    INCL(o.tags,cc.otag_pri_defined);
    out.wf(";\n");
  ELSE
    out.wf("%s;\n",buf);
  END;
END var_definition;

PROCEDURE copy_func_params;
  VAR nm,buf,str: STR; l: pc.OBJECT;
BEGIN
  (* copy params *)
  IF dcl.cur_proc.flag IN pc.LangSet{pc.flag_o2,pc.flag_m2} THEN
    l:=dcl.cur_proc.prof;
    WHILE l#NIL DO
      IF dcl.is_c_arr(l.type) &
         (l.mode=pc.ob_var) &
         NOT (pc.otag_RO IN l.tags)
      THEN
        dcl.o_name(l,nm);
        IF l.type.mode=pc.ty_array_of THEN
          ret_emul:=TRUE;
          nms.x2c(nms.nm_alloc_param,str);
          out.sp(1); out.wf("%s((void **)&%s,",str,nm);
          exp.gen_size_usage(l,0,0);
          out.wf(");\n");
        ELSE
          dcl.type_designator(buf,"*",dcl.get_base_type(l.type));
          out.sp(1); out.wf("%s = (%s)",nm,buf);
          dcl.make_temp_var(l.type,buf);
          nms.x2c(nms.nm_memcpy,str);
          out.wf("%s(%s,%s,",str,buf,nm);
          dcl.gen_sizeof(l.type); out.wf(");\n");
        END;
      END;
      l:=l.next;
    END;
  END;
  (* ini local vars with a paticular address *)
  ini_var_addr(dcl.cur_proc.mem);
END copy_func_params;

PROCEDURE free_func_params;
  VAR nm,str: STR; l: pc.OBJECT;
BEGIN
  (* free params *)
  IF dcl.cur_proc.flag<pc.flag_c THEN
    l:=dcl.cur_proc.prof;
    WHILE l#NIL DO
      IF (l.type.mode=pc.ty_array_of) &
         (l.mode=pc.ob_var) &
         NOT (pc.otag_RO IN l.tags)
      THEN
        dcl.o_name(l,nm);
        nms.x2c(nms.nm_free_param,str);
        out.sp(1); out.wf("%s(%s);\n",str,nm);
      END;
      l:=l.next;
    END;
  END;
END free_func_params;

PROCEDURE clear_o2_vars(o: pc.OBJECT);
  VAR nm: STR;
BEGIN
  WHILE o#NIL DO
    IF is_o2_var(o) &
       (o.type.mode#pc.ty_pointer) &
       (pc.ttag_has_o2_ptr IN o.type.tags)
    THEN
      out.sp(1);
      dcl.o_usage(o,nm);
      out.ws("memset(");
      IF NOT dcl.is_c_arr(o.type) THEN out.wr('&') END;
      out.wf("%s,0,sizeof(%s));\n",nm,nm);
    END;
    o:=o.next;
  END;
END clear_o2_vars;

PROCEDURE func_definition(o: pc.OBJECT);

  PROCEDURE full_name(o: pc.OBJECT);
  BEGIN
    IF o.host#NIL THEN full_name(o.host.obj); out.wr('.') END;
    out.ws(o.name^);
  END full_name;

  VAR
    nm,str : STR;
    dd,rt,b: BOOLEAN;
    buf    : ARRAY 2048 OF CHAR;
    tmps   : dcl.TMP_VAR;
    v      : pc.OBJECT;

    s_loc_txt : out.STR;
    s_cur_proc: pc.STRUCT;
    s_tmp_vars: dcl.TMP_VAR;
    s_tmp_busy: dcl.TMP_VAR;
    s_ret_emul: BOOLEAN;

BEGIN
  ASSERT(o.mode IN (pc.PROCs-pc.OB_SET{pc.ob_eproc}));
  IF dcl.gen_def THEN RETURN END;

  s_loc_txt :=dcl.loc_txt;
  s_cur_proc:=dcl.cur_proc;
  s_tmp_vars:=dcl.tmp_vars;
  s_tmp_busy:=dcl.tmp_busy;
  s_ret_emul:=ret_emul;

  dcl.tmp_vars:=NIL;
  dcl.tmp_busy:=NIL;
  out.wl;
  IF cc.op_comments THEN cmt.first_comment(o.pos,1,0) END;
  dcl.out_line_no(o.pos,0);
  IF nms.func_is_extern(o) THEN
    sl1_glo_mark;
    IF NOT cc.op_krc THEN out.ws("extern ") END;
  ELSE
    out.ws("static ");
  END;
(*Ned v2.12
  IF cc.op_gendll & ((o.mode=pc.ob_xproc) OR nms.func_is_extern(o)) THEN
    out.ws("X2C_EXPORT_FUNC ");
  END;
*)
  dcl.declarator(str,o,FALSE);
  dcl.func_profile_definition(o.type,buf,TRUE,FALSE);
  out.wf("%s(%s)\n",str,buf);
  dcl.func_profile_definition(o.type,buf,TRUE,TRUE);
  out.ws(buf);
  IF cc.op_comments THEN cmt.last_comment(o.type.end,0) END;

  out.wf("{\n");
  dcl.cur_proc:=o.type;
  ASSERT(dcl.cur_proc#NIL);
  ret_emul:=FALSE;
  out.gstr(dcl.loc_txt);        (* is used in params copy *)
  out.gstr(udf_txt);
  v:=o.type.mem;
  WHILE v#NIL DO
    IF (v.mode=pc.ob_var) OR
       (v.mode=pc.ob_cons) & NOT (pc.otag_no_threat IN v.tags)
       (* если константы не описывать заранее то возникают проблемы
          с константами, которые описаны уровнем выше, чем уровень
          использования
       *)
    THEN
      dcl.object_declaration(v);
    END;
    v:=v.next;
  END;

  IF cc.op_comments THEN cmt.first_comment(o.val.pos,0,0) END;
  dcl.enter_statement(tmps);

  IF cc.op_debug OR cc.op_profile THEN
    ret_emul:=TRUE;
    dcl.out_line_no(o.val.pos,1);
    IF cc.op_profile THEN
      out.push;
      out.disable_lf(b);
      out.sp(1); out.wf('static struct X2C_Profile_STR X2C_PRF = { ');
      ASSERT(o.host#NIL);
      out.wr('"');
      full_name(o);
      out.ws('", 0, 0, 0, 0, 0 };');
      out.restore_lf(b);
      out.wl;
      out.append(dcl.loc_txt);
      out.pop;
      out.sp(1); out.wf("X2C_PROC_PRF(&X2C_PRF);\n");
    ELSE
      out.sp(1); out.wf("X2C_PROC_INP();\n");
    END;
  END;
  copy_func_params;     (* change value of ret_emul! *)

  (* statements *)
  gen_sequence(o.val.r,1,dd,rt,TRUE,NOT ret_emul,NIL);
  ASSERT(dcl.cur_proc#NIL);
  ASSERT(dcl.cur_proc.base#NIL);

  free_func_params;
  IF cc.op_debug OR cc.op_profile THEN
    dcl.out_line_no(o.end,1);
    out.sp(1); out.wf("X2C_PROC_OUT();\n");
  END;

  IF (dcl.cur_proc.base.mode#pc.ty_void) & NOT rt THEN
    (* We must generate return statement even if control flow
       never reach it. C compiler is not requared to do
       exectly the same control flow analisys as we do.
    *)
    IF dcl.is_c_arr(dcl.cur_proc.base) THEN
      dcl.o_second_name(dcl.cur_proc.obj,0,"_ret",nm);
      out.sp(1); out.wf("return %s;\n",nm);
    ELSE
      IF NOT ret_emul & dcl.is_c_num_or_adr(dcl.cur_proc.base) THEN
        nm:="0";
      ELSE
        (* create var for returning value *)
        out.push;
        dcl.o_second_name(dcl.cur_proc.obj,0,"_ret",nm);
        dcl.type_designator(buf,nm,dcl.cur_proc.base);
        out.sp(1); out.wf("%s;\n",buf);
        out.append(dcl.loc_txt);
        out.pop;
      END;
      out.sp(1); out.wf("return %s;\n",nm);
    END;
  END;
  out.push;
  clear_o2_vars(dcl.cur_proc.mem);
  out.append(dcl.loc_txt);
  out.pop;
  out.append(dcl.loc_txt);
  dcl.cur_proc:=NIL;
  out.wstr(dcl.loc_txt);
  IF cc.op_comments THEN cmt.next_comment(o.end,0) END;
  out.wstr(udf_txt);
  out.ws("} ");
  out.ff(str,"end %s()",o.name^);
  dcl.out_comment(str);
  IF cc.op_comments THEN cmt.last_comment(o.end,0) END;
  out.wl;

  dcl.exit_statement(tmps);

  dcl.tmp_vars:=NIL;
  ASSERT(dcl.tmp_busy=NIL);

  dcl.loc_txt :=s_loc_txt;
  dcl.cur_proc:=s_cur_proc;
  dcl.tmp_vars:=s_tmp_vars;
  dcl.tmp_busy:=s_tmp_busy;
  ret_emul:=s_ret_emul;
END func_definition;

PROCEDURE code_func_definition(o: pc.OBJECT);
  VAR ret: STR;
  PROCEDURE protocol(v: pc.VALUE);
    VAR
      ps : LONGINT;
      ch : CHAR;
      nm : STR;
      loc: STR;
    PROCEDURE get;
    BEGIN
      dcl.zz_tmp.index_get(ps,v);
      ch:=CHR(dcl.zz_tmp.get_integer());
      IF ch=0X THEN RETURN END;
      INC(ps);
    END get;
    PROCEDURE get_nm;
      VAR i: INTEGER;
    BEGIN
      WHILE ch=' ' DO get END;
      i:=0;
      WHILE (ch#':') & (ch#0X) & (ch#' ') DO
        nm[i]:=ch; INC(i); get;
      END;
      nm[i]:=0X;
    END get_nm;
    PROCEDURE get_colon;
    BEGIN
      WHILE ch=' ' DO get END;
      IF ch=':' THEN get; RETURN END;
    END get_colon;
    PROCEDURE get_register;
      VAR i: INTEGER;
    BEGIN
      WHILE ch=' ' DO get END;
      i:=0;
      WHILE (ch#',') & (ch#0X) & (ch#' ') & (ch#')') DO
        loc[i]:=ch; INC(i); get;
      END;
      loc[i]:=0X;
    END get_register;
    PROCEDURE get_location;
    BEGIN
      get_register;
    END get_location;
    VAR str: STR; l: pc.OBJECT;
  BEGIN
    ps:=0;
    get;
    LOOP
      get_nm;
      IF nm="" THEN RETURN END;
      IF nm="USE" THEN
        get_colon; WHILE (ch#')') & (ch#0X) DO get END; get;
      ELSIF nm="RETURN" THEN
        get_colon; get_register; ret:=loc;
      ELSE
        l:=o.type.prof;
        WHILE (l#NIL) & (l.name^#nm) DO l:=l.next END;
        IF l=NIL THEN
          env.errors.Error(v.pos,1001,nm);
          RETURN;
        END;
        get_colon; get_location; dcl.o_name(l,str);
        out.sp(1); out.wf("asm mov %s, DWORD PTR[%s];\n",loc,str);
      END;
    END;
  END protocol;
  VAR buf,str: STR; l: pc.NODE; i: LONGINT;
BEGIN
  IF dcl.gen_def THEN RETURN END;
  ASSERT(o.mode=pc.ob_cproc);
  out.wl;
  IF nms.func_is_extern(o) THEN
    IF NOT cc.op_krc THEN out.ws("extern ") END;
    IF cc.op_gendll THEN out.ws("X2C_DLL_TAG ") END; (*Ned v2.12*)
  ELSE
    out.ws("static ");
  END;
  dcl.declarator(str,o,FALSE);
  dcl.func_profile_definition(o.type,buf,TRUE,FALSE);
  out.wf(" %s(%s)\n",str,buf);
  dcl.func_profile_definition(o.type,buf,TRUE,TRUE);
  out.wf("%s{\n",buf);
  out.gstr(dcl.loc_txt);
  dcl.cur_proc:=o.type;
  l:=o.val.l; ret:="";
  IF (l#NIL) & (l.type.mode=pc.ty_SS) THEN
    protocol(l.val); l:=l.next;
  END;
  WHILE l#NIL DO
    ASSERT(l.mode=pc.nd_value);
    out.sp(1);
    IF l.type.mode=pc.ty_SS THEN
      FOR i:=0 TO l.type.len-1 DO
        dcl.zz_tmp.index_get(i,l.val);
        IF NOT dcl.zz_tmp.is_zero() THEN
          out.wr(CHR(dcl.zz_tmp.get_integer()));
        END;
      END;
      out.wl;
    ELSE
      out.wf("asm db 0x%02X;\n",l.val.get_integer());
    END;
    l:=l.next;
  END;
  IF ret#"" THEN
    dcl.make_temp_var(o.type.base,str);
    out.sp(1); out.wf("asm mov DWORD PTR [%s],%s;\n",str,ret);
    out.sp(1); out.wf("return %s;\n",str);
  END;
  dcl.cur_proc:=NIL;
  out.append(dcl.loc_txt);
  out.wstr(dcl.loc_txt);
  out.wf("}\n\n");
END code_func_definition;

PROCEDURE gen_size_checks(o: pc.OBJECT);
  VAR nm,fn: STR; sz: LONGINT;
BEGIN
  nms.x2c(nms.nm_assert,fn);
  WHILE o#NIL DO
    IF (o.mode=pc.ob_type) & (o.type.obj=o) &
       (cc.otag_declared IN o.tags) &
       NOT (o.type.mode IN pc.TY_SET{pc.ty_range,pc.ty_enum})
    THEN
      sz:=pc.code.get_size(pc.su_bytes,o.type);
      IF sz>=0 THEN
        IF NOT dcl.t_usage(o.type,nm,TRUE,FALSE) THEN ASSERT(FALSE) END;
        out.sp(1); out.wf("if (sizeof(%s)!=%d) %s(0);\n",nm,sz,fn);
      END;
    END;
    o:=o.next;
  END;
END gen_size_checks;

PROCEDURE chk_o2_vars(o: pc.OBJECT): BOOLEAN;
BEGIN
  WHILE o#NIL DO
    IF is_o2_var(o) THEN RETURN TRUE END;
    o:=o.next;
  END;
  RETURN FALSE;
END chk_o2_vars;

PROCEDURE module_desc_definition(m,types: pc.OBJECT);
  PROCEDURE list_offs(o: pc.OBJECT);
    VAR nm: STR;
  BEGIN
    WHILE o#NIL DO
      IF is_o2_var(o) THEN
        dcl.o_usage(o,nm);
        offset_definition(nm,"",o.type);
      END;
      o:=o.next;
    END;
  END list_offs;
  PROCEDURE list_cmds(o: pc.OBJECT; adr: BOOLEAN);
    VAR nm: STR;
  BEGIN
    WHILE o#NIL DO
      IF (o.flag IN pc.LangsAllowCommands) &
         (o.mode=pc.ob_xproc) &
         (o.type.prof=NIL) &
         (o.type.base.mode=pc.ty_void)
      THEN
        ASSERT(pc.otag_public IN o.tags);
        IF adr THEN dcl.o_usage(o,nm); out.ws(nm);
        ELSE out_str(o.name^);
        END;
        out.wr(',');
      END;
      o:=o.next;
    END;
  END list_cmds;
  VAR of,ls,cmd,cnm: STR;
BEGIN
  IF md_desc="" THEN dcl.o_second_name(m,2,"_desc",md_desc) END;
  dcl.o_second_name(m,3,"_offs",of);
  dcl.o_second_name(m,4,"_cmds",cmd);
  dcl.o_second_name(m,5,"_cnms",cnm);

  out.wl;
  (* адреса глобальных переменных - указателей *)
  out.wf("static void * %s[] = {\n",of);
  list_offs(m.type.prof);
  list_offs(m.type.mem);
  out.sp(1); out.wf("X2C_OFS_END\n};\n");

  (* адреса глобальных процедур - команд *)
  out.wf("static X2C_PROC %s[] = { ",cmd);
  list_cmds(m.type.prof,TRUE);
  out.wf("0 };\n");

  (* имена глобальных процедур - команд *)
  out.wf("static X2C_CHAR * %s[] = { ",cnm);
  list_cmds(m.type.prof,FALSE);
  out.wf("0 };\n");

  out.wf("struct X2C_MD_STR %s = {\n  0, 0, ",md_desc);
  out_str(dcl.cur_mod.name^);
  out.wf(",%s,%s,%s,",of,cmd,cnm);
  IF types=NIL THEN
    out.ws("0");
  ELSE
    dcl.o_second_name(types,1,"_desc",ls);
    out.wf("&%s",ls);
  END;
  out.wf("\n};\n");
END module_desc_definition;

PROCEDURE module_body_definition(main: BOOLEAN);
  VAR
    nm,ini      : STR;
    dd,rt,gc_auto: BOOLEAN;
    heap_lim,stk_lim,gc_thres: LONGINT;
    str         : env.String;
    u           : pc.USAGE;
    noinit      : BOOLEAN;
BEGIN
  IF dcl.gen_def THEN RETURN END;
  IF (dcl.cur_mod.type.flag=pc.flag_o2) OR
     (md_desc#"") OR
     chk_o2_vars(dcl.cur_mod.type.prof) OR
     chk_o2_vars(dcl.cur_mod.type.mem)
  THEN
    module_desc_definition(dcl.cur_mod,last_td);
  END;
  ASSERT(dcl.tmp_vars=NIL);
  dcl.cur_proc:=NIL;
  IF dcl.cur_mod.type.flag IN cc.c_like_flags THEN
    RETURN
  END;
  noinit:=env.config.Option("GENNOINIT");
  out.wl;
  IF ~ main THEN
    IF dcl.cur_mod.type.flag=pc.flag_sl1 THEN
      ASSERT(dcl.cur_mod.val.r=NIL);
      RETURN;
    END;
    sl1_glo_mark;
    dcl.module_body_def(dcl.cur_mod,TRUE); out.wf("\n{\n");
    out.gstr(dcl.loc_txt);

    (* initialization check *)
    IF ~ noinit THEN
      dcl.o_second_name(dcl.cur_mod,0,"_init",ini);
      out.sp(1); out.wf("static int %s = 0;\n",ini);
      dcl.out_line_no(dcl.cur_mod.val.pos,1);
      out.sp(1); out.wf("if (%s) return;\n",ini);
      dcl.out_line_no(dcl.cur_mod.val.pos,1);
      out.sp(1); out.wf("%s = 1;\n",ini);
    END;
  ELSE
    env.config.Equation("STACKLIMIT",str);
    IF (str=NIL) OR NOT xcStr.StrToInt(str^,stk_lim) THEN stk_lim:=-1 END;
    env.config.Equation("HEAPLIMIT",str);
    IF (str=NIL) OR NOT xcStr.StrToInt(str^,heap_lim) THEN heap_lim:=0 END;
    env.config.Equation("GCTHRESHOLD",str);
    IF (str=NIL) OR NOT xcStr.StrToInt(str^,gc_thres) THEN gc_thres:=0 END;
    gc_auto:=env.config.Option("GCAUTO");
    IF stk_lim>=0 THEN
      out.wf("X2C_STACK_LIMIT(%dl)\n",stk_lim);
    END;
    IF cc.op_krc THEN
      out.wf("int main(argc, argv)\n  int argc;\n  char **argv;\n{\n");
    ELSE
      out.wf("extern int main(int argc, char **argv)\n{\n");
    END;
    out.gstr(dcl.loc_txt);
    IF ~noinit THEN
      nms.x2c(nms.nm_rts_body,nm);
      dcl.out_line_no(dcl.cur_mod.val.pos,1);
      out.sp(1);
      out.wf("%s(&argc,argv,%d,%dl,%dl);\n",nm,gc_auto,gc_thres,heap_lim);
    END;
    ret_emul:=TRUE;
  END;

  IF cc.op_debug THEN
    dcl.out_line_no(dcl.cur_mod.val.pos,1);
    out.sp(1); out.wf("X2C_PROC_INP();\n");
  END;

  IF ~noinit THEN
    gen_size_checks(dcl.cur_mod.type.prof);
    gen_size_checks(dcl.cur_mod.type.mem);
    init_records_bases;
    (* import initialization *)
    u:=dcl.cur_mod.type.use;
    WHILE u#NIL DO
      IF (u.obj.mode = pc.ob_module) &
        ~ (u.obj.type.flag IN cc.c_like_flags + pc.LangSet{pc.flag_sl1})
      THEN
        dcl.module_body_name(u.obj,nm);
        dcl.out_line_no(dcl.cur_mod.val.pos,1);
        out.sp(1); out.wf("%s();\n",nm)
      END;
      u:=u.next;
    END;
  END;

  IF md_desc#"" THEN
    (* module descriptor *)
    out.sp(1); out.wf("X2C_MODULE(&%s);\n",md_desc);
  END;

(*global var address is generated as constant initializer
  ini_var_addr(dcl.cur_mod.type.prof);
  ini_var_addr(dcl.cur_mod.type.mem); *)

  IF cc.op_comments THEN cmt.first_comment(dcl.cur_mod.val.pos,0,0) END;
  (* statements *)
  gen_sequence(dcl.cur_mod.val.r,1,dd,rt,TRUE,FALSE,NIL);
  out.append(dcl.loc_txt);
  out.wstr(dcl.loc_txt);

  IF cc.op_comments THEN cmt.next_comment(dcl.cur_mod.end,0) END;
  IF cc.op_debug THEN
    dcl.out_line_no(dcl.cur_mod.end,1);
    out.sp(1); out.wf("X2C_PROC_OUT();\n");
  END;
  IF main THEN
    dcl.out_line_no(dcl.cur_mod.end,1);
    IF ~noinit THEN
      out.sp(1); out.wf("X2C_EXIT();\n");
    END;
    out.sp(1); out.wf("return 0;\n");
  END;
  out.wf("}\n");
  IF cc.op_comments THEN cmt.last_comment(dcl.cur_mod.end,0) END;
  out.wl;
  IF main THEN
    out.wf("X2C_MAIN_DEFINITION\n");
  END;
  dcl.tmp_vars:=NIL;
  <* IF ASSERT THEN *>
    IF dcl.tmp_busy#NIL THEN
      env.info.print("tmp_busy -> %s\n",dcl.tmp_busy.inf.name^);
    END;
  <* END *>
  ASSERT(dcl.tmp_busy=NIL);
END module_body_definition;

PROCEDURE glo_object_definition(o: pc.OBJECT);
  VAR s: seg.SEGMENT;
BEGIN
  IF cc.otag_pub_defined IN o.tags THEN
    IF dcl.gen_def THEN RETURN END;
    IF cc.otag_pri_defined IN o.tags THEN RETURN END;
  END;
  IF pc.OTAG_SET{cc.otag_defining,cc.otag_declaring}*o.tags#pc.OTAG_SET{} THEN
    env.errors.Error(o.pos,1002);
    ASSERT(o.mode=pc.ob_type);
    RETURN;
  END;
  IF o.type.obj=o THEN
    seg.enter(o.type.ext(nms.TREE).seg);
  ELSE
    seg.new_seg(s,o.pos,1);
    seg.enter(s);
  END;
  INCL(o.tags,cc.otag_defining);
  CASE o.mode OF
    |pc.ob_var,pc.ob_cons               : var_definition(o);
    |pc.ob_proc,pc.ob_xproc,pc.ob_lproc : func_definition(o);
    |pc.ob_cproc                        : code_func_definition(o);
    |pc.ob_type                         : type_definition(o);
    |pc.ob_module                       : module_body_definition(gen_main);
    |pc.ob_eproc                        : (* nothing *)
(*
ELSE
env.info.print("def %d ?\n",o.mode);
*)
  END;
  EXCL(o.tags,cc.otag_defining);
  INCL(o.tags,cc.otag_pub_defined);
  IF ~ dcl.gen_def THEN INCL(o.tags,cc.otag_pri_defined) END;
  seg.exit;
END glo_object_definition;

TYPE
  GENDEF = RECORD (pc.RIDER) END;
  INIOBJ = RECORD (pc.RIDER) END;

PROCEDURE no_segment(o: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN
    (o.mode IN pc.FIELDs+pc.OB_SET{pc.ob_label,pc.ob_header}) OR
    (o.mode IN pc.VARs) & (o.lev>0) OR
    ((o.host#NIL) & (o.host.mode=pc.ty_enum)) OR
    (pc.otag_with IN o.tags) OR
    (o.mode=pc.ob_cons) & (o.flag=pc.flag_sl1) &
       ~cc.op_gencnsexp & pc.code.en_preopt;
END no_segment;

PROCEDURE in_header(o: pc.OBJECT): BOOLEAN;
(* object declaration is in header file *)
BEGIN
  RETURN (o.mode#pc.ob_lproc) & o.is_public();
END in_header;

PROCEDURE (VAR r: INIOBJ) object(o: pc.OBJECT);
  VAR i: nms.INFO; n: nms.TREE;
BEGIN
  IF no_segment(o) THEN RETURN END;
  i:=dcl.o_info(o,-1,"");
  IF dcl.gen_def # in_header(o) THEN
    i.seg:=NIL;
  ELSE
    IF dcl.gen_def & (o.mode = pc.ob_module) THEN
      seg.new_seg(i.seg,o.end,0);
    ELSE
      seg.new_seg(i.seg,o.pos,0);
    END;
  END;
  IF o.type.obj=o THEN
    IF o.type.ext=NIL THEN
      nms.new_tree(n); o.type.ext:=n;
    ELSE
      n:=o.type.ext(nms.TREE);
    END;
    IF (o.mode IN (pc.PROCs+pc.OB_SET{pc.ob_module})) & (o.val#NIL) THEN
      seg.new_seg(n.seg,o.val.pos,1);
    ELSE
      seg.new_seg(n.seg,o.pos,1);
    END;
  END;
END object;

PROCEDURE (VAR r: GENDEF) object(o: pc.OBJECT);
BEGIN
  IF no_segment(o) THEN RETURN END;
  IF dcl.gen_def THEN
    IF ~ in_header(o) THEN RETURN END;
    dcl.object_declaration(o);
  END;
  glo_object_definition(o);
END object;

PROCEDURE gen_definitions*(main: BOOLEAN);
  VAR d: GENDEF; i: INIOBJ;
BEGIN
  dcl.gen_def:=FALSE;
  gen_main:=main;
  md_desc:="";
  out.gstr(dcl.glo_txt);
  dcl.cur_mod.type.objects(i);
  i.object(dcl.cur_mod);
  dcl.cur_mod.type.objects(d);
  d.object(dcl.cur_mod);
  IF md_desc#"" THEN
    out.ws("extern ");
    IF cc.op_gendll THEN out.ws("X2C_DLL_TAG ") END; (*Ned v2.12*)
    out.wf("struct X2C_MD_STR %s;\n",md_desc);
  END;
  out.wstr(dcl.glo_txt);
  IF cc.op_comments THEN cmt.out_all_comments(dcl.cur_mod) END;
  seg.write(cc.op_lineno);
END gen_definitions;

PROCEDURE gen_publics*;
  VAR d: GENDEF; i: INIOBJ;
BEGIN
  dcl.gen_def:=TRUE;
  md_desc:="";
  out.gstr(dcl.glo_txt);
  dcl.cur_mod.type.objects(i);
  i.object(dcl.cur_mod);
  dcl.cur_mod.type.objects(d);
  d.object(dcl.cur_mod);
  out.wstr(dcl.glo_txt);
  IF cc.op_comments & (dcl.cur_mod.type.flag=pc.flag_m2) THEN
    cmt.out_all_comments(dcl.cur_mod);
  END;
  seg.write(FALSE);
  ASSERT(md_desc="");
END gen_publics;

PROCEDURE exi*;
BEGIN
  last_td:=NIL;
  eq_glomark:=NIL;
  reclist:=NIL;
END exi;

PROCEDURE ini*;
BEGIN
  exi;
  env.config.Equation("SL1GLOBALMARK",eq_glomark);
  unnamed_anonymous_structs := env.config.Option("unnamed_anonymous_structs");
END ini;

BEGIN
  dcl.glo_object_def:=glo_object_definition;
END ccDef.
