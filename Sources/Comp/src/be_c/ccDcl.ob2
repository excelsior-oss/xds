(** Copyright (c) 1991,97 XDS Ltd, Russia. All Rights Reserved. *)
(** Declarations *)
MODULE ccDcl;
(** Sem 22-Sep-93. *)

(* Modifications:
   08/Feb/96 Ned  2.12  op_gendll - X2C_DLL_TAG is generated instead of
                        X2C_IMPORT/EXPORT_FUNC/DATA.
   15/Mar/96 Ned  2.12  X2C_PROCLASS is generated for M2/O2 procedures only.
                        For ["Pascal"], ["StdCall"], ["SysCall"] the
                        compiler always generates X2C_PASCAL, X2C_STDCALL,
                        X2C_SYSCALL respectively, regardless of the value
                        of GENPROCLASS option.
                        Procedures affected: module_body_def,
                          type_designator_notype, declarator, type_constructor
   17/Mar/96 Ned  2.12  almost all BNRP specific is removed.
   17/Mar/96 Ned  2.12  Comments copying is improved.
   23/Mar/96 Ned  2.13  glo_var_declaration: generate #define if op_gencnsexp.
   27/Mar/96 Ned  2.14  PRO0046: const_aggregate str: sb_len is used to get LEN.
   15-Apr-96 Ned        constants of proctype with value NIL are invented.
                        Fix in const_aggregate.
*)

IMPORT
  pc :=pcK,
  cc :=ccK,
  nms:=ccN,
  out:=ccL,
  cmt:=ccComments,
  env:=xiEnv,
  xfs:=xiFiles,
  ccr:=ccRec,
  seg:=ccSeg,
  pcO,   (* patch, will go away in XDS 3.0 *)
  <* IF PCVIS THEN *> pcVis, <* END *>
  xcStr;

(*
	Additional object names
ob_proc
	0 _ret  - переменная для возврата результата
	1 _type - имя типа (для методов)
ob_module
	0 _init
	1 _BEGIN
	2 _desc
	3 _offs
	4 _cmds
	5 _cnms
ob_field
	0..n    - путь для вариантных имен
ob_type
	1 _desc   - дескриптор типа
	2 _offs   - список оффсетов
	3 _proc   - список методов
ob_varpar
	0 _type
*)

TYPE
  STR* = ARRAY 256 OF CHAR;
  PRM_BUF* = ARRAY 256 OF RECORD
    mode*: SHORTINT;  	(* вид параметра: pm_*, см. CONST *)
    ref *: SET;    	(* мода генерации *)
    obj *: pc.OBJECT;
  END;
  TMP_VAR* = POINTER TO TMP_VAR_REC;
  TMP_VAR_REC = RECORD
    type: pc.STRUCT;
    len : LONGINT;      (* -1 normal, -2 pointer, else array *)
    inf-: nms.INFO;
    val : pc.VALUE;
    dcl : seg.SEGMENT;
    next: TMP_VAR;
  END;

CONST
  (* expression generation mode *)
  REF*  = 0;	(* генерить указатель вместо значения 			*)
  ANY*  = 1;    (* генерить указатель (или значение) произвольного С-типа *)
  LVL* 	= 2;    (* генерить l-value (или ссылку на него если REF) 	*)
  BSA*  = 3;    (* генерить правильный тип либо указатель на базовый тип *)
  CCH*  = 5;    (* строковые литералы генерить сишными 			*)
  NEG*  = 6;
  BLN*  = 7;	(* условное выражение, значение будет сравниваться с нулем *)
  CNS*  = 8;    (* выражение-инициализация: допустимы агрегаты 		*)
  CHK*  = 9;    (* constant expression  	 			*)
  CRF   =10;    (* reference                                            *)
  PLS*  =11;    (* addition or multiplication context                   *)

  (* function argument mode *)
  pm_return    *= 0;
  pm_threat    *= 1;
  pm_thr_len   *= 2;
  pm_thr_type  *= 3;
  pm_param     *= 4;
  pm_len       *= 5;
  pm_seq       *= 7; (* "..." argument in C function *)
  pm_frec      *= 8;
  pm_type      *= 9;

VAR
  zz_tmp       *: pc.VALUE;
  rr_tmp       *: pc.VALUE;
  cmp_value_tmp : pc.VALUE;
  max_longint   : pc.VALUE;
  zz_zero       : pc.VALUE;

  type_ZZ    	: pc.STRUCT;
  type_RR    	: pc.STRUCT;
  LSET_T       *: pc.STRUCT;
  SEQU_T       *: pc.STRUCT;
  LSET_BITS    *: INTEGER;

  glo_txt      *: out.STR;
  loc_txt      *: out.STR;

  cur_proc     *: pc.STRUCT;
  cns_vars   	: TMP_VAR;
  tmp_vars     *: TMP_VAR;
  tmp_busy     *: TMP_VAR;
  gen_def      *: BOOLEAN; (* TRUE если идет генерация DEFINITION *)

  cur_mod      *: pc.OBJECT; (* current module *)
  mod_usage    *: POINTER TO ARRAY OF BOOLEAN; (* must be included *)
  sl1_usage    *: BOOLEAN; (* slRTS must be included *)

  gen_value     *: PROCEDURE(n: pc.NODE; p: INTEGER; md: SET);
  glo_object_def*: PROCEDURE(o: pc.OBJECT);

PROCEDURE ^ object_declaration(o: pc.OBJECT);

PROCEDURE ^type_desig(
	VAR res	: ARRAY OF CHAR;
	ident-	: ARRAY OF CHAR;
	t	: pc.STRUCT;
	def	: BOOLEAN;
	prm     : BOOLEAN);

PROCEDURE cmp_value*(op: pc.SUB_MODE; x,y: pc.VALUE): BOOLEAN;
BEGIN
  IF cmp_value_tmp=NIL THEN
    cmp_value_tmp:=pc.value.new(env.null_pos,pc.ZZ_type);
  END;
  cmp_value_tmp.binary(op,x,y);
  RETURN ~cmp_value_tmp.is_zero();
END cmp_value;

PROCEDURE o_info*(o: pc.OBJECT; n: INTEGER; suffix-: ARRAY OF CHAR): nms.INFO;
  VAR i: nms.INFO;
BEGIN
  IF o.ext#NIL THEN
    i:=o.ext(nms.INFO);
    REPEAT
      IF i.no=n+1 THEN RETURN i END;
      i:=i.nxt;
    UNTIL i=NIL;
  END;
  ASSERT((o.mode=pc.ob_module) OR
         (o.mno=cur_mod.mno) OR
         (o.flag IN cc.c_like_flags) & (n<0) OR
	 (o.host.mode=pc.ty_proctype));
  IF ((o.mode=pc.ob_module) OR
      (o.flag IN cc.c_like_flags)) &
     (n<0)
  THEN
    i:=nms.make_info(o,-1,suffix);
  ELSE
    i:=nms.make_info(o,n+1,suffix);
  END;
  RETURN i;
END o_info;

PROCEDURE o_second_name*(
		o: pc.OBJECT;
		n: INTEGER;
		suffix-: ARRAY OF CHAR;
		VAR nm : ARRAY OF CHAR);

  VAR i: nms.INFO;
BEGIN
  IF o.mno<pc.ZEROMno THEN
    IF n>=0 THEN out.ff(nm,"X2C_%s%s%d",o.name^,suffix,n);
    ELSE out.ff(nm,"X2C_%s%s",o.name^,suffix);
    END;
    RETURN
  END;
  i:=o_info(o,n,suffix);
  COPY(i.name^,nm);
END o_second_name;

PROCEDURE o_name*(o: pc.OBJECT; VAR nm: ARRAY OF CHAR);
BEGIN
  o_second_name(o,-1,"",nm);
END o_name;

PROCEDURE o_usage*(o: pc.OBJECT; VAR nm: ARRAY OF CHAR);
(*
   Returns C object name, generate object declaration if requared.
   Name can be pointer to object (is_pointer(o)=TRUE),
   or reference to objec (is_reference(o)=TRUE)
*)
  VAR i: nms.INFO; n: nms.TREE;
BEGIN
  IF pc.otag_with IN o.tags THEN o_name(o,nm); RETURN END;
  IF (o.mode=pc.ob_module) OR (o.mno<pc.ZEROMno) THEN o_name(o,nm); RETURN END;
  mod_usage[o.mno]:=TRUE;
  IF o.mno#cur_mod.mno THEN o_name(o,nm); RETURN END;

  IF (o.mode IN pc.VARs) &
     (o.lev>0) &
     (o.lev<=cur_proc.obj.lev)
  THEN
    (* Intermidiate local variables don't requires declaration *)
    o_name(o,nm);
    (* Do not "import" intermidiate local variables *)
    RETURN;
  END;

  IF (o.mode = pc.ob_label) THEN
    (* Labels don't requires declaration *)
    o_name(o,nm);
    RETURN;
  END;

  IF (o.mode=pc.ob_proc) &
     (seg.segment#NIL) &
     seg.segment.pos.gtr(o.val.pos)
  THEN
    (* declaration is not requared *)
    (* the same can not be done for ob_type->ty_record! *)
    o_name(o,nm);
    n:=o.type.ext(nms.TREE);
    seg.import(n.seg);
    RETURN;
  END;

  IF cc.otag_declared IN o.tags THEN
    (* nothing *)
  ELSIF o.host.mode=pc.ty_enum THEN
    (* object is declared by type declaration *)
    object_declaration(o.host.obj);
    ASSERT(cc.otag_declared IN o.tags);
  ELSIF (o.host.mode=pc.ty_record) & ~ (o.mode IN pc.PROCs) THEN
    (* object is declared by type definition *)
    glo_object_def(o.host.obj);
    ASSERT(cc.otag_declared IN o.tags);
  ELSE
    object_declaration(o);
  END;
  i:=o_info(o,-1,"");
  seg.import(i.seg);
  COPY(i.name^,nm);
END o_usage;

PROCEDURE module_body_name*(m: pc.OBJECT; VAR nm: ARRAY OF CHAR);
  VAR bf: STR; i: INTEGER;
BEGIN
  ASSERT(m.mode=pc.ob_module);
  o_second_name(m,1,"_BEGIN",bf);
  IF ~(cc.otag_versionkey IN m.tags) THEN
    COPY(bf,nm); RETURN;
  END;
  i:=0;
  IF cc.op_index16   THEN i:=1 END;
  IF cc.op_address16 THEN INC(i,2) END;
  IF cc.op_int16     THEN INC(i,4) END;
  out.ff(nm,"%s_%c%.8X",bf,CHR(ORD('A')+i),m.type.len);
END module_body_name;

PROCEDURE module_body_def*(m: pc.OBJECT; def: BOOLEAN);
  VAR nm: STR;
BEGIN
  IF ~ cc.op_krc OR ~ def THEN out.ws("extern ") END;
  IF cc.op_gendll & ~def THEN out.ws("X2C_DLL_TAG ") END;
  out.ws("void ");
  IF cc.op_proclass THEN
    nms.get_proclass(m.type.flag,nm); out.ws(nm);
  END;
  module_body_name(m,nm);
  out.ws(nm); out.wr('(');
  IF ~cc.op_krc THEN out.ws("void") END;
  out.wr(')');
END module_body_def;

PROCEDURE is_char*(t: pc.STRUCT): BOOLEAN;
BEGIN
  IF t.mode=pc.ty_range THEN t:=t.base END;
  RETURN t.mode=pc.ty_char;
END is_char;

PROCEDURE is_long*(t: pc.STRUCT): BOOLEAN;
(** type size is more then 2 bytes *)
(* l must be generated in spite of 'target16',
   else EXCL (&=~) depends on size of int in C *)
BEGIN
  RETURN
    ~(pc.ttag_c_type IN t.tags) &
    (pc.code.get_size(pc.su_bytes,t) > 2);
END is_long;

PROCEDURE suffix*(t: pc.STRUCT);
BEGIN
  IF ~cc.op_krc THEN
    IF (t.mode IN pc.TY_SET{pc.ty_set,pc.ty_pointer,pc.ty_opaque}) OR
       ~t.signed()
    THEN
      out.wr('u');
    END;
  END;
  IF pc.ttag_c_type IN t.tags THEN RETURN END;
  IF pc.code.get_size(pc.su_bytes,t) > 2 THEN out.wr('l') END;
END suffix;

PROCEDURE is_c_arr*(t: pc.STRUCT): BOOLEAN;
BEGIN
  (* passed by reference in function params *)
  RETURN (t.mode IN pc.ARRs) & ~(pc.ttag_packed IN t.tags) OR
         (t.mode=pc.ty_set) & (t.len>32);
END is_c_arr;

PROCEDURE is_c_num*(t: pc.STRUCT): BOOLEAN;
BEGIN
  RETURN
    (t.mode IN (pc.NUMs+
    pc.TY_SET{pc.ty_boolean,pc.ty_char,
     pc.ty_range,pc.ty_enum,
     pc.ty_loc,pc.ty_protection})) OR
    (t.mode=pc.ty_set) & (t.len<=32);
END is_c_num;

PROCEDURE is_c_num_or_adr*(t: pc.STRUCT): BOOLEAN;
BEGIN
  RETURN
    (t.mode IN (pc.ADRs+pc.TY_SET{pc.ty_opaque,pc.ty_proctype})) OR
    is_c_num(t);
END is_c_num_or_adr;

PROCEDURE c_types_equ*(x,y: pc.STRUCT; fst: BOOLEAN): BOOLEAN;
  VAR px,py: pc.OBJECT;
BEGIN
  IF x.mode=pc.ty_range THEN x:=x.base END;
  IF y.mode=pc.ty_range THEN y:=y.base END;
  IF x.is_ordinal() THEN x:=x.super_type() END;
  IF y.is_ordinal() THEN y:=y.super_type() END;
  IF x.obj#NIL THEN x:=x.obj.type END;
  IF y.obj#NIL THEN y:=y.obj.type END;
  IF x=y THEN RETURN TRUE END;
  CASE x.mode OF
    |pc.ty_pointer:
      RETURN (y.mode=pc.ty_pointer) &
             c_types_equ(x.base,y.base,FALSE);
    |pc.ty_array:
      RETURN fst &
             (y.mode=pc.ty_pointer) &
             c_types_equ(x.base,y.base,FALSE) OR
	     (y.mode=pc.ty_array) &
	     (x.len=y.len) &
             c_types_equ(x.base,y.base,FALSE);
    |pc.ty_shortint,pc.ty_integer,pc.ty_longint,
     pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard,
     pc.ty_real,pc.ty_longreal,pc.ty_ld_real,
     pc.ty_complex,pc.ty_lcomplex,
     pc.ty_void:
      RETURN x.mode=y.mode;
    |pc.ty_proctype:
      IF x.flag#y.flag THEN RETURN FALSE END;
      IF ~c_types_equ(x.base,y.base,TRUE) THEN RETURN FALSE END;
      px:=x.prof;
      py:=y.prof;
      WHILE (px#NIL) & (py#NIL) DO
        IF cc.otag_notype IN px.tags THEN RETURN FALSE END;
        IF cc.otag_notype IN py.tags THEN RETURN FALSE END;
        IF ~c_types_equ(px.type, py.type, TRUE) THEN RETURN FALSE END;
        IF (pc.otag_RO IN (px.tags / py.tags))  THEN RETURN FALSE END;
        px:=px.next;
        py:=py.next;
      END;
      RETURN (px=NIL) & (py=NIL);
  ELSE
    RETURN FALSE;
  END;
END c_types_equ;

PROCEDURE out_line_no*(pos: env.TPOS; lev: INTEGER);
  VAR fnm: env.String; i,j: LONGINT;
BEGIN
  IF ~cc.op_lineno THEN RETURN END;
  IF gen_def THEN RETURN END;
  IF pos.IsNull() THEN RETURN END;
  pos.unpack(fnm,i,j);
  IF ~cc.op_krc THEN out.sp(lev) END;
  out.ws("#line ");
  out.wint(i+1);
  out.wl;
END out_line_no;

PROCEDURE out_comment*(s-: ARRAY OF CHAR);
BEGIN
  IF cc.op_cpp THEN out.ws("// ") ELSE out.ws("/* ") END;
  out.ws(s);
  IF ~ cc.op_cpp THEN out.ws(" */") END;
  out.wl;
END out_comment;

PROCEDURE std_type(t: pc.STRUCT; VAR nm: ARRAY OF CHAR): BOOLEAN;
(* if type is declared in X2C.h then returns TRUE and type name in 'nm' *)
BEGIN
  (* ty_range can have own name! *)
  IF t.mode=pc.ty_range THEN
    IF (t.obj#NIL) & (cur_mod.type.flag=pc.flag_bnrp) THEN RETURN FALSE END;
    t:=t.base;
  END;
  IF (pc.ttag_c_type IN t.tags) & (t.obj#NIL) THEN
    COPY(t.obj.name^,nm);
    RETURN TRUE;
  END;
  CASE t.mode OF
    |pc.ty_shortcard: nms.x2c(nms.nm_shortcard,nm);
    |pc.ty_cardinal : nms.x2c(nms.nm_cardinal,nm);
    |pc.ty_longcard : nms.x2c(nms.nm_longcard,nm);
    |pc.ty_shortint : nms.x2c(nms.nm_shortint,nm);
    |pc.ty_integer  : nms.x2c(nms.nm_integer,nm);
    |pc.ty_longint  : nms.x2c(nms.nm_longint,nm);
    |pc.ty_ZZ       : nms.x2c(nms.nm_longint,nm);
    |pc.ty_real     : nms.x2c(nms.nm_real,nm);
    |pc.ty_longreal : nms.x2c(nms.nm_longreal,nm);
    |pc.ty_ld_real  : nms.x2c(nms.nm_ld_real,nm);
    |pc.ty_RR       : nms.x2c(nms.nm_longreal,nm);
    |pc.ty_complex  : nms.x2c(nms.nm_complex,nm);
    |pc.ty_lcomplex : nms.x2c(nms.nm_lcomplex,nm);
    |pc.ty_CC       : nms.x2c(nms.nm_lcomplex,nm);
    |pc.ty_boolean  : IF t.base.mode=pc.ty_shortcard THEN
    			nms.x2c(nms.nm_boolean,nm);
                      ELSE
                        RETURN std_type(t.base,nm);
                      END;
    |pc.ty_char     : nms.x2c(nms.nm_char,nm);
    |pc.ty_AA       : nms.x2c(nms.nm_AA,nm);
    |pc.ty_loc      : nms.x2c(nms.nm_loc,nm);
    |pc.ty_protection:nms.x2c(nms.nm_protection,nm);
    |pc.ty_void     : nms.x2c(nms.nm_void,nm);
    |pc.ty_set      : IF is_c_arr(t) THEN RETURN FALSE END;
    		      IF t.inx#NIL THEN RETURN std_type(t.inx,nm) END;
		      CASE pc.code.get_size(pc.su_bytes,t) OF
			|1: nms.x2c(nms.nm_shortcard,nm);
			|2: nms.x2c(nms.nm_cardinal,nm);
			|4: nms.x2c(nms.nm_longcard,nm);
		      END;
    |pc.ty_process  : COPY("struct X2C_XHandler_STR",nm);
  ELSE
    IF (t.obj=NIL) OR (t.obj.mno>=pc.ZEROMno) THEN RETURN FALSE END;
    o_name(t.obj,nm);
  END;
  RETURN TRUE;
END std_type;

PROCEDURE t_def*(t: pc.STRUCT);
BEGIN
  IF (t.obj=NIL) OR (t.obj.mode#pc.ob_type) THEN RETURN END;
  t:=t.obj.type; (* this is requared for SL1 *)
  IF t.mno#cur_mod.mno THEN RETURN END;
  IF t.mode#pc.ty_record THEN RETURN END;
  IF ~ (cc.otag_pub_defined IN t.obj.tags) THEN glo_object_def(t.obj) END;
  IF t.ext=NIL THEN RETURN END;
  seg.import(t.ext(nms.TREE).seg);
END t_def;

PROCEDURE t_usage*(
	t: pc.STRUCT;
	VAR nm: ARRAY OF CHAR;
	def: BOOLEAN;
	ren: BOOLEAN): BOOLEAN;
(*
   returns type name, generates type declaration if requared.
   if def = TRUE then generate type definition.
   if ren = TRUE then type is used for rename: do not substitute
      base type insted of enum

   returns FALSE if type has not name
*)
  VAR bf: STR;
BEGIN
  IF std_type(t,nm) THEN RETURN TRUE END;
  IF (t.obj=NIL) OR (t.obj.mode#pc.ob_type) THEN RETURN FALSE END;
  IF def THEN t_def(t) END;
  t:=t.obj.type; (* this is requared for SL1 *)
  IF ~ren & (t.mode=pc.ty_enum) & (cur_mod.type.flag # pc.flag_bnrp) THEN --$$$
    mod_usage[t.mno]:=TRUE; (* ! *)
    (* для enum используем базовый тип с целью улучшения
       переносимости C-текстов, проблема в том, что
       sizeof(enum X) не определен
    *)
    RETURN t_usage(t.base,nm,def,FALSE);
  END;
  o_usage(t.obj,bf);
  ASSERT(t.obj.type=t);
  IF t.mode=pc.ty_record THEN ccr.Record(t) END;
  IF t.mode=pc.ty_enum THEN out.ff(nm,"enum %s",bf);
  ELSIF pc.ttag_no_struct IN t.tags THEN COPY(bf,nm);
  ELSIF cc.ttag_union IN t.tags THEN out.ff(nm,"union %s",bf);
  ELSIF t.mode=pc.ty_record   THEN out.ff(nm,"struct %s",bf);
  ELSIF t.mode=pc.ty_array_of THEN out.ff(nm,"struct %s",bf);
  ELSE COPY(bf,nm);
  END;
  RETURN TRUE;
END t_usage;

PROCEDURE type_designator*(
	VAR res	: ARRAY OF CHAR;
	ident-	: ARRAY OF CHAR;
	t	: pc.STRUCT);
BEGIN
  type_desig(res,ident,t,TRUE,FALSE);
END type_designator;

PROCEDURE type_designator_notype*(
	VAR res	: ARRAY OF CHAR;
	ident-	: ARRAY OF CHAR;
	t	: pc.STRUCT);
  VAR pcl: STR;
BEGIN
  CASE t.mode OF
    |pc.ty_proctype:
      IF t.flag IN cc.default_proclass THEN
        COPY("X2C_PROC",res);
        IF ident#"" THEN nms.strcat(res,' '); nms.strcat(res,ident) END;
      ELSIF t.flag IN cc.special_proclass THEN
        nms.get_proclass(t.flag,pcl);
        out.ff(res,"void (%s *%s)()",pcl,ident);
      ELSE
        out.ff(res,"void (*%s)()",ident);
      END;
    |pc.ty_pointer :
      out.ff(res,"void *%s",ident);
  END;
END type_designator_notype;

(*------------------ Temporary variables ---------------------------------*)

PROCEDURE new_temp_var(t: pc.STRUCT; len: LONGINT;
		       VAR nm: ARRAY OF CHAR);
  VAR v: TMP_VAR;
BEGIN
  NEW(v);
  v.type:=t;
  v.len:=len;
  v.inf:=nms.make_name(nm,cur_proc,NIL,pc.ob_var,"tmp",0);
  v.next:=tmp_busy;
  tmp_busy:=v;
END new_temp_var;

PROCEDURE search_temp_var(t: pc.STRUCT; len: LONGINT;
			VAR nm: ARRAY OF CHAR): BOOLEAN;
  VAR p,v: TMP_VAR;
BEGIN
  v:=tmp_vars; p:=NIL;
  IF len<0 THEN
    WHILE (v#NIL) & ((v.type#t) OR (v.len#len)) DO p:=v; v:=v.next END;
  ELSE
    WHILE (v#NIL) & ((v.type#t) OR (v.len<len)) DO p:=v; v:=v.next END;
  END;
  IF v=NIL THEN RETURN FALSE END;
  IF p=NIL THEN tmp_vars:=v.next ELSE p.next:=v.next END;
  v.next:=tmp_busy;
  tmp_busy:=v;
  COPY(v.inf.name^,nm);
  RETURN TRUE;
END search_temp_var;

PROCEDURE enter_statement*(VAR x: TMP_VAR);
BEGIN
  x:=tmp_busy;
  tmp_busy:=NIL;
END enter_statement;

PROCEDURE exit_statement*(x: TMP_VAR);
  VAR v: TMP_VAR;
BEGIN
  IF tmp_busy#NIL THEN
    v:=tmp_busy;
    WHILE v.next#NIL DO v:=v.next END;
    v.next:=tmp_vars;
    tmp_vars:=tmp_busy;
  END;
  tmp_busy:=x;
END exit_statement;

PROCEDURE make_temp_var*(t: pc.STRUCT; VAR nm: ARRAY OF CHAR);
  VAR buf: STR;
BEGIN
  IF search_temp_var(t,-1,nm) THEN RETURN END;
  new_temp_var(t,-1,nm);
  out.push;
  type_designator(buf,nm,t);
  out.sp(1); out.ws(buf); out.wr(";"); out.wl;
  out.append(loc_txt);
  out.pop;
END make_temp_var;

PROCEDURE make_temp_arr*(t: pc.STRUCT; len: LONGINT; VAR nm: ARRAY OF CHAR);
  VAR buf,res: STR;
BEGIN
  ASSERT(len>=0);
  IF search_temp_var(t,len,nm) THEN RETURN END;
  new_temp_var(t,len,nm);
  out.push;
  out.ff(buf,"%s[%d]",nm,len);
  out.sp(1);
  IF t=SEQU_T THEN
    nms.x2c(nms.nm_seq_type,res);
    out.wf("%s %s;\n",res,buf);
  ELSIF t=pcO.size_t THEN
    nms.x2c(nms.nm_index,res);
    out.wf("%s %s;\n",res,buf);
  ELSE
    type_designator(res,buf,t);
    out.wf("%s;\n",res);
  END;
  out.append(loc_txt);
  out.pop;
END make_temp_arr;

PROCEDURE make_temp_ptr*(t: pc.STRUCT; VAR nm: ARRAY OF CHAR);
  VAR buf,str: STR;
BEGIN
  IF search_temp_var(t,-2,nm) THEN RETURN END;
  new_temp_var(t,-2,nm);
  out.push;
  out.ff(str,"*%s",nm);
  type_designator(buf,str,t);
  out.sp(1); out.ws(buf); out.wr(";"); out.wl;
  out.append(loc_txt);
  out.pop;
END make_temp_ptr;

(*----------------------- Literals --------------------------------------*)

PROCEDURE c_number*(n: LONGINT; t: pc.STRUCT; VAR s: ARRAY OF CHAR);
BEGIN
  IF cur_mod.type.flag=pc.flag_bnrp THEN
    out.ff(s,"%d",n); RETURN;
  END;
  t:=t.super_type();
  IF is_long(t) THEN
    IF cc.op_krc OR t.signed() THEN out.ff(s,"%dl",n)
    ELSE out.ff(s,"%dul",n)
    END;
  ELSE
    IF cc.op_krc OR t.signed() THEN out.ff(s,"%d",n)
    ELSE out.ff(s,"%du",n)
    END;
  END;
END c_number;

PROCEDURE gen_field_path*(inf: nms.INFO);
BEGIN
  IF inf=NIL THEN RETURN END;
  gen_field_path(inf.nxt);
  out.ws(inf.name^); out.wr('.');
END gen_field_path;

PROCEDURE search_record_variant*(VAR l: pc.NODE; v: pc.VALUE);
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
	zz_tmp.binary(pc.sb_geq,v,m.val);
	IF ~zz_tmp.is_zero() THEN
	  zz_tmp.binary(pc.sb_leq,v,m.l.val);
          IF ~zz_tmp.is_zero() THEN EXIT END;
        END;
      ELSE
        ASSERT(m.mode=pc.nd_value);
        zz_tmp.binary(pc.sb_equ,v,m.val);
        IF ~zz_tmp.is_zero() THEN EXIT END;
      END;
      m:=m.next;
    END;
    IF m#NIL THEN EXIT END;
    l:=l.next;
  END;
  IF l=NIL THEN l:=e END;
END search_record_variant;

PROCEDURE gen_sizeof*(t: pc.STRUCT);
  VAR sz: LONGINT; nm: STR;
BEGIN
  ASSERT(t.mode#pc.ty_array_of);
  IF cc.op_gencnsexp THEN sz:=-1;
  ELSE sz:=pc.code.get_size(pc.su_bytes,t);
  END;
  IF sz<0 THEN
    type_designator(nm,"",t);
    out.wf("sizeof(%s)",nm);
  ELSE
    c_number(sz,pcO.size_t,nm);
    out.ws(nm);
  END;
END gen_sizeof;

PROCEDURE const_aggregate*(v: pc.VALUE; t: pc.STRUCT; p: INTEGER; def,cch: BOOLEAN);
(*
	def - aggregate in var definition, i.e. it can be { ... }
	cch - C string literal, do not double '\' char
*)

  VAR tmp: STR;

  PROCEDURE tmp_in(): BOOLEAN;
    (* RETURN TRUE если константа уже описана, при этом ее имя
	записывается в выходной поток, т.е. больше ничего генерить
	не нужно.
    *)
    VAR bf: STR; c: TMP_VAR;
  BEGIN
    tmp:="";
    IF def THEN RETURN FALSE END;
    c:=cns_vars;
    LOOP
      IF c=NIL THEN EXIT END;
      IF c.type=t THEN
	zz_tmp.binary(pc.sb_equ,c.val,v);
	IF ~zz_tmp.is_zero() THEN
	  out.ws(c.inf.name^);
          seg.import(c.dcl);
          RETURN TRUE;
	END;
      END;
      c:=c.next;
    END;
    NEW(c); c.next:=cns_vars; cns_vars:=c; c.type:=t; c.val:=v;
    c.inf:=nms.make_name(tmp,NIL,NIL,pc.ob_var,"_cnst",0);
    seg.new_seg(c.dcl,v.pos,0);
    seg.import(c.dcl);
    seg.enter(c.dcl);
    type_designator(bf,tmp,t);
    out.ws("static ");
    IF cc.op_cpp & is_c_num(t) THEN out.ws("const ") END;
    out.ws(bf);
    out.ws(" = ");
    RETURN FALSE;
  END tmp_in;

  PROCEDURE tmp_out;
  BEGIN
    IF def THEN RETURN END;
    out.wf(";\n");
    seg.exit;
    out.ws(tmp);
  END tmp_out;

  PROCEDURE enum;
    VAR l: pc.OBJECT; nm: STR;
  BEGIN
    l:=t.obj.type.prof;
    WHILE l#NIL DO
      zz_tmp.binary(pc.sb_equ,l.val.val,v);
      IF ~zz_tmp.is_zero() THEN o_usage(l,nm); out.ws(nm); RETURN END;
      l:=l.next;
    END;
    v.value_to_str(nm,pc.flag_c);
    out.ws(nm);
  END enum;

  PROCEDURE num;
    VAR str: STR; s: pc.STRUCT;
  BEGIN
    IF t.mode#pc.ty_ZZ THEN
      s:=t.super_type();
      ASSERT(s.mode IN pc.WHOLEs);
      IF s.mode=pc.ty_longint THEN
        zz_tmp.binary(pc.sb_equ,v,s.min);
        IF ~zz_tmp.is_zero() THEN out.ws("X2C_min_longint"); RETURN END;
        zz_tmp.binary(pc.sb_equ,v,s.max);
        IF ~zz_tmp.is_zero() THEN out.ws("X2C_max_longint"); RETURN END;
      ELSIF s.mode=pc.ty_longcard THEN
        zz_tmp.binary(pc.sb_equ,v,s.max);
        IF ~zz_tmp.is_zero() THEN out.ws("X2C_max_longcard"); RETURN END;
      END;
    END;
    IF (p>=11) & v.is_neg() THEN out.wr('(') END;
    v.value_to_str(str,pc.flag_c); out.ws(str); suffix(t);
    IF (p>=11) & v.is_neg() THEN out.wr(')') END;
  END num;

  PROCEDURE complex;
    VAR v1,v2: STR;
  BEGIN
    IF tmp_in() THEN RETURN END;
    rr_tmp.unary(pc.su_re,v); rr_tmp.value_to_str(v1,pc.flag_c);
    rr_tmp.unary(pc.su_im,v); rr_tmp.value_to_str(v2,pc.flag_c);
    out.wf("{%s,%s}",v1,v2);
    tmp_out;
  END complex;

(*
  PROCEDURE is_compound_type(t: pc.STRUCT): BOOLEAN;
  BEGIN
    RETURN (t.mode IN (pc.CPLXs+{pc.ty_record})) OR is_c_arr(t);
  END is_compound_type;
*)

(*Ned v2.12 - procedure is not used
  PROCEDURE array_agg;
    VAR nm,mm: STR; n: LONGINT; w: pc.VALUE;
  BEGIN
    ASSERT(t.mode=pc.ty_array);
    make_temp_var(t,nm);
    out.wr('(');
    FOR n:=0 TO t.len-1 DO
      (* Нужно создавать новый объект на каждый элемент массива,
	 он используется в глобальном списке констант
      *)
      w:=pc.value.new(v.pos,t.base);
      w.index_get(n,v);
      IF is_c_arr(t.base) THEN
	nms.x2c(nms.nm_memcpy,mm); out.wf("%s(",mm);
	out.wf("%s[%d],",nm,n);
	const_aggregate(w,t.base,0,FALSE,FALSE);
	out.wr(','); gen_sizeof(t.base); out.ws('), ');
      ELSE
	out.wf("%s[%d]=",nm,n);
	const_aggregate(w,t.base,1,FALSE,FALSE);
	out.ws(", ");
      END;
    END;
    out.wf("%s)",nm);
  END array_agg;
*)

  PROCEDURE arr;
    VAR i: LONGINT; v1: pc.VALUE;
  BEGIN
    ASSERT(t.mode#pc.ty_array_of);
    ASSERT(t.base#NIL);
(*    IF is_compound_type(t.base) THEN array_agg; RETURN END; *)
    IF tmp_in() THEN RETURN END;
    out.wr("{");
    FOR i:=0 TO t.len-1 DO
      IF i#0 THEN out.wr(",") END;
      (* We must create new object for each array element,
	 it is used in global list of constants
      *)
      v1:=pc.value.new(v.pos,t.base);
      v1.index_get(i,v);
      const_aggregate(v1,t.base,0,TRUE,FALSE);
    END;
    out.wr("}");
    tmp_out;
  END arr;

  PROCEDURE is_simple_record(): BOOLEAN;
  (* RETURN TRUE if record can be generated as C constant aggregate *)
    VAR f: pc.OBJECT;
  BEGIN
    IF t.base#NIL THEN RETURN FALSE END;
    f:=t.prof;
    WHILE f#NIL DO
      IF f.mode=pc.ob_header THEN RETURN FALSE END;
(* Ned 5-Aug-96  IF is_compound_type(f.type) THEN RETURN FALSE END; *)
      f:=f.next;
    END;
    RETURN TRUE;
  END is_simple_record;

  PROCEDURE record_agg;
    VAR nm: STR; n: LONGINT;
    PROCEDURE gen_fld(f: pc.OBJECT);
      VAR fld,mm: STR; w: pc.VALUE;
    BEGIN
      ASSERT(f.mode#pc.ob_header);
      w:=pc.value.new(v.pos,f.type);
      w.index_get(n,v);
      IF is_c_arr(f.type) THEN
	nms.x2c(nms.nm_memcpy,mm); out.wf("%s(%s.",mm,nm);
	o_usage(f,fld); gen_field_path(f.ext(nms.INFO).nxt);
	out.wf("%s,",fld); const_aggregate(w,f.type,0,FALSE,FALSE);
	out.wr(','); gen_sizeof(f.type); out.wr(')');
      ELSE
	out.wf("%s.",nm); o_usage(f,fld);
	gen_field_path(f.ext(nms.INFO).nxt); out.wf("%s = ",fld);
	const_aggregate(w,f.type,1,FALSE,FALSE);
      END;
      out.ws(", ");
      INC(n);
    END gen_fld;
    PROCEDURE gen_fld_seq(f: pc.OBJECT);
      VAR l: pc.NODE; w: pc.VALUE;
    BEGIN
      WHILE f#NIL DO
	IF f.mode=pc.ob_header THEN
	  ASSERT(f.val.mode=pc.nd_case);
	  l:=f.val.l;
	  w:=pc.value.new(v.pos,f.type);
	  w.index_get(n,v);
	  search_record_variant(l,w);
	  ASSERT(l#NIL);
	  IF f.val.obj#NIL THEN gen_fld(f.val.obj) ELSE INC(n) END;
	  gen_fld_seq(l.obj);
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
    ASSERT(t.mode=pc.ty_record);
    make_temp_var(t,nm);
    out.wr('(');
    n:=0;
    gen_level(t);
    out.wf("%s)",nm);
  END record_agg;

  PROCEDURE record;
    VAR i: LONGINT; f: pc.OBJECT; w: pc.VALUE;
  BEGIN
    IF ~is_simple_record() THEN record_agg(); RETURN END;
    IF tmp_in() THEN RETURN END;
    out.wr("{"); f:=t.prof;
    IF f=NIL THEN
      out.wr('0');
    ELSE
      i:=0;
      REPEAT
	IF i#0 THEN out.wr(",") END;
	ASSERT(f.mode#pc.ob_header);
	w:=pc.value.new(v.pos,f.type);
	w.index_get(i,v);
        const_aggregate(w,f.type,0,TRUE,FALSE);
	INC(i); f:=f.next;
      UNTIL f=NIL;
    END;
    out.wr("}");
    tmp_out;
  END record;

  PROCEDURE lset;
    VAR i,j: LONGINT; k: SET; fst: BOOLEAN;
  BEGIN
    IF tmp_in() THEN RETURN END;
    out.wr("{"); j:=0; k:={}; fst:=TRUE;
    FOR i:=0 TO t.len-1 DO
      zz_tmp.index_get(i,v);
      IF ~zz_tmp.is_zero() THEN INCL(k,j) END;
      INC(j);
      IF (j=LSET_BITS) OR (i=t.len-1) THEN
	IF fst THEN fst:=FALSE ELSE out.wr(",") END;
	out.wf("0x%.*X",LSET_BITS DIV 4,k);
	suffix(LSET_T);
	k:={}; j:=0;
      END;
    END;
    out.wr("}");
    tmp_out;
  END lset;

  PROCEDURE char(ch: CHAR; str0,cch0: BOOLEAN);
    PROCEDURE wr(c: CHAR);
    BEGIN
      IF str0 THEN out.wr(c) ELSE out.wrc(c) END;
    END wr;
    VAR bf: ARRAY 8 OF CHAR; i: INTEGER;
  BEGIN
    CASE ch OF
      |'"': out.wrc('\'); wr('"');
      |"'": out.wrc('\'); wr("'");
(* On Macintosh \r and \n designate somthing else
      |0DX: out.wrc('\'); wr("r");
      |0AX: out.wrc('\'); wr("n");
*)
      |'\': out.wrc('\'); IF ~cch0 THEN wr('\') END;
      |00X: out.wrc('\'); wr("0");
    ELSE
      IF (ch>=' ') & (ch<='~') THEN
	wr(ch);
      ELSE
	out.ff(bf,"\\%.3o",ch);
	ASSERT(LENGTH(bf)=4);
	FOR i:=0 TO 2 DO out.wrc(bf[i]) END;
	wr(bf[3]);
      END;
    END;
  END char;

  PROCEDURE str;
    VAR i,len: LONGINT; ch: CHAR; b: BOOLEAN;
  BEGIN
    zz_tmp.binary(pc.sb_len,v,zz_zero);
    len:=zz_tmp.get_integer();
    ASSERT(len <= t.len + 1); (* note: value may contain 0X in [t.len] *)
    zz_tmp.index_get(len-1,v);
    IF ~ zz_tmp.is_zero() THEN
      ASSERT(len = t.len);
      arr; RETURN
    END;
    out.disable_lf(b); out.wr('"');
    FOR i:=0 TO len-2 DO
      zz_tmp.index_get(i,v);
      ch:=CHR(zz_tmp.get_integer());
      char(ch,TRUE,cch);
    END;
    out.wr('"'); out.restore_lf(b);
  END str;

  VAR bf,be: STR; o: pc.OBJECT;
BEGIN
  IF t.mode=pc.ty_range THEN t:=t.base END;
  CASE t.mode OF
    |pc.ty_char,pc.ty_loc:
      IF v.is_zero() THEN
        out.wr('0')
      ELSE
        out.wrc("'"); char(CHR(v.get_integer()),FALSE,FALSE); out.wr("'");
      END;
    |pc.ty_enum:
      enum;
    |pc.ty_AA:
      out.wr('0');
    |pc.ty_pointer,pc.ty_opaque,pc.ty_protection:
      v.value_to_str(bf,pc.flag_c); out.ws(bf); suffix(t);
    |pc.ty_boolean:
      IF v.is_zero() THEN out.wr('0') ELSE out.wr('1') END;
    |pc.ty_array,pc.ty_SS:
      IF is_char(t.base) THEN str ELSE arr END;
    |pc.ty_real:
      v.value_to_str(bf, pc.flag_c);
      IF v.is_neg() THEN out.wf("(%sf)",bf) ELSE out.wf("%sf",bf) END;
    |pc.ty_longreal,pc.ty_ld_real,pc.ty_RR:
      v.value_to_str(bf,pc.flag_c);
      IF v.is_neg() THEN out.wf("(%s)",bf) ELSE out.ws(bf) END;
    |pc.ty_complex,pc.ty_lcomplex,pc.ty_CC:
      complex;
    |pc.ty_set :
      IF is_c_arr(t) THEN
	lset;
      ELSE
	v.value_to_str(bf,pc.flag_c); out.ws(bf); suffix(t);
      END;
    |pc.ty_record:
      record;
    |pc.ty_proctype:
      o:=v.get_object();
      IF o = NIL THEN
        out.wr('0');
      ELSE
        o_usage(o,bf);
        IF c_types_equ(t,o.type,TRUE) THEN
          out.ws(bf);
        ELSE
          IF p>13 THEN out.wr('(') END;
          type_designator(be,"",t);
          out.wr("("); out.ws(be); out.wr(')'); out.ws(bf);
          IF p>13 THEN out.wr(')') END;
        END;
      END;
  ELSE
    IF t.mode IN pc.WHOLEs THEN num;
    ELSE env.errors.Error(v.pos,1013);
    END;
  END;
END const_aggregate;

PROCEDURE const_hex*(v: pc.VALUE; t: pc.STRUCT; p: INTEGER);
  VAR i,j: LONGINT;
BEGIN
  zz_tmp.unary(pc.su_abs,v);
  IF cmp_value(pc.sb_gtr,zz_tmp,max_longint) THEN
    const_aggregate(v,t,p,FALSE,FALSE);
  ELSE
    i:=v.get_integer();
    IF (p>=11) & (i<0) THEN out.wr('(') END;
    IF i<0 THEN j:=ABS(i); out.wr('-') ELSE j:=i END;
    out.wf("0x%X",j); suffix(t); out.wr(' ');
    IF (p>=11) & (i<0) THEN out.wr(')') END;
  END;
END const_hex;

(*--------------------------------------------------------------------*)

PROCEDURE get_bytes*(ps: pc.TPOS; t: pc.STRUCT): LONGINT;
  VAR i: LONGINT; nm: STR;
BEGIN
  i:=pc.code.get_size(pc.su_bytes,t);
  IF i<0 THEN
    type_desig(nm,"",t,FALSE,FALSE);
    env.errors.Error(ps,1014,nm);
    i:=1;
  END;
  RETURN i;
END get_bytes;

PROCEDURE get_base_type*(t: pc.STRUCT): pc.STRUCT;
  (* для типов, которые в С являются массивами, выдает базовай тип,
     т.е. тип, с указателем на который даннй тип совместим *)
BEGIN
  ASSERT(is_c_arr(t));
  IF t.mode=pc.ty_array_of THEN
    REPEAT t:=t.base UNTIL t.mode#pc.ty_array_of;
    RETURN t;
  ELSIF t.mode IN pc.ARRs THEN
    (* !!!!!! а как насчет многомерных массивов ? *)
    RETURN t.base;
  ELSE
    ASSERT(t.mode=pc.ty_set);
    RETURN LSET_T;
  END;
END get_base_type;

PROCEDURE simple_type_desig(
	VAR res: ARRAY OF CHAR;
	t: pc.STRUCT;
	def: BOOLEAN);
BEGIN
  IF t_usage(t,res,def,FALSE) THEN RETURN END;
  IF t.mode=pc.ty_pointer THEN
    simple_type_desig(res,t.base,FALSE);
    nms.strcat(res," *");
  ELSIF t.mode=pc.ty_range THEN
    simple_type_desig(res,t.base,def);
  ELSE
    ASSERT(t.obj=NIL);
    t.obj:=pc.new_object_ex(t,cur_mod.type,pc.ob_type,gen_def);
    IF ~t_usage(t,res,def,FALSE) THEN ASSERT(FALSE) END;
  END;
END simple_type_desig;

PROCEDURE is_reference*(o: pc.OBJECT; scp: pc.STRUCT): BOOLEAN;
  (* RETURN TRUE if name in C is reference to object of type 'o.type' *)
  VAR u: pc.USAGE;
BEGIN

  IF ~(o.mode IN pc.OB_SET{pc.ob_varpar,pc.ob_var}) THEN RETURN FALSE END;
  IF cc.otag_reference IN o.tags           THEN RETURN TRUE  END;
  IF ~ cc.op_cpp                           THEN RETURN FALSE END;
  IF o.mno#cur_mod.mno                     THEN RETURN FALSE END;
  IF scp=NIL                               THEN RETURN FALSE END;

  IF (o.mode=pc.ob_varpar) & ~is_c_arr(o.type) &
     (o.host.flag IN pc.LangSet{pc.flag_o2,pc.flag_m2,pc.flag_sl1,pc.flag_bnrp})
  THEN
    INCL(o.tags,cc.otag_reference); RETURN TRUE;
  END;

  IF (o.lev=0) & (o.val#NIL) THEN
    INCL(o.tags,cc.otag_reference); RETURN TRUE;
  END;

  IF (o.lev>0) & (scp.obj#NIL) & (o.lev<=scp.obj.lev) & ~is_c_arr(o.type) THEN
    IF pc.otag_no_aliases IN o.tags THEN
      u:=scp.use;
      WHILE u.obj#o DO u:=u.next END;
      IF ~(pc.utag_write IN u.tags) THEN RETURN FALSE END;
    END;
    (* Do not use 'otag_reference' for scope dependent checks! *)
    RETURN TRUE;
  END;

  RETURN FALSE;
END is_reference;

PROCEDURE is_pointer*(o: pc.OBJECT; scp: pc.STRUCT): BOOLEAN;
  (* RETURN TRUE if name in C is pointer to object of type 'o.type' *)
  VAR u: pc.USAGE;
BEGIN
  IF cc.op_cpp & is_reference(o,scp) THEN RETURN FALSE END;
  IF o.mode=pc.ob_varpar THEN RETURN ~is_c_arr(o.type) END;
  IF o.mode#pc.ob_var THEN RETURN FALSE END;
  IF (o.attr#NIL) & ~(pc.otag_valpar IN o.tags) THEN RETURN TRUE END;
  IF (o.type.mode=pc.ty_record) &
     (pc.otag_valpar IN o.tags) &
     (o.host.flag=pc.flag_p)
  THEN RETURN TRUE;
  END;
  IF scp=NIL THEN RETURN FALSE END;
  IF (o.lev>0) & (scp.obj#NIL) & (o.lev<=scp.obj.lev) & ~is_c_arr(o.type) THEN
    IF pc.otag_no_aliases IN o.tags THEN
      u:=scp.use;
      WHILE u.obj#o DO u:=u.next END;
      IF ~(pc.utag_write IN u.tags) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END;
  RETURN FALSE;
END is_pointer;

PROCEDURE declarator*(VAR res: ARRAY OF CHAR; o: pc.OBJECT; ptr: BOOLEAN);
  VAR nm,buf,pcl: STR;
BEGIN
  IF o.mode IN pc.PROCs THEN
    IF cc.op_proclass OR (o.type.flag IN cc.special_proclass) THEN
      nms.get_proclass(o.type.flag,pcl)
    ELSE pcl:="";
    END;
    IF ptr THEN
      o_second_name(o,1,"_",buf);
      out.ff(nm,"(%s*%s)",pcl,buf);
    ELSE
      o_name(o,buf);
      out.ff(nm,"%s%s",pcl,buf);
    END;
    IF is_c_arr(o.type.base) THEN
      simple_type_desig(buf,get_base_type(o.type.base),TRUE);
      out.ff(res,"%s * %s",buf,nm);
    ELSE
      simple_type_desig(buf,o.type.base,TRUE);
      out.ff(res,"%s %s",buf,nm);
    END;
  ELSE
    o_name(o,nm);
    IF    is_reference(o,o.host) THEN out.ff(buf,"&%s",nm); nm:=buf;
    ELSIF is_pointer(o,o.host)   THEN out.ff(buf,"*%s",nm); nm:=buf;
    END;
    type_designator(res,nm,o.type);
  END;
END declarator;

PROCEDURE func_params*(
	VAR prm_buf: PRM_BUF;
	VAR prm_cnt: INTEGER;
	t: pc.STRUCT);

  VAR buf_errs: BOOLEAN;

  PROCEDURE buf_err;
  BEGIN
    IF buf_errs THEN RETURN END;
    env.errors.Error(t.pos,1015);
    buf_errs:=TRUE;
  END buf_err;

  PROCEDURE threat(o: pc.OBJECT);
    VAR nm: STR; i: LONGINT;
  BEGIN
    IF ~(o.mode IN pc.VARs) THEN RETURN END;
    IF prm_cnt>=LEN(prm_buf) THEN buf_err; RETURN END;
    prm_buf[prm_cnt].mode:=pm_threat;
    o_name(o,nm); nms.disable(t,nm);
    prm_buf[prm_cnt].obj:=o;
    prm_buf[prm_cnt].ref:={LVL};
    IF    is_reference(o,t) THEN INCL(prm_buf[prm_cnt].ref,CRF);
    ELSIF is_pointer(o,t)   THEN INCL(prm_buf[prm_cnt].ref,REF);
    ELSIF is_c_arr(o.type)  THEN INCL(prm_buf[prm_cnt].ref,BSA);
    END;
    (* если array_of то надо передать длины *)
    IF o.type.mode=pc.ty_array_of THEN
      FOR i:=0 TO o.type.len-1 DO
	INC(prm_cnt);
	IF prm_cnt>=LEN(prm_buf) THEN buf_err; RETURN END;
	prm_buf[prm_cnt].mode:=pm_thr_len;
	o_second_name(o,SHORT(i),"_len",nm); nms.disable(t,nm);
	prm_buf[prm_cnt].obj:=o;
	prm_buf[prm_cnt].ref:={};
      END;
    ELSIF (o.type.flag=pc.flag_o2) &
	  (o.type.mode=pc.ty_record) &
	  (o.mode=pc.ob_varpar)
    THEN
      INC(prm_cnt);
      IF prm_cnt>=LEN(prm_buf) THEN buf_err; RETURN END;
      prm_buf[prm_cnt].mode:=pm_thr_type;
      o_second_name(o,0,"_type",nm); nms.disable(t,nm);
      prm_buf[prm_cnt].obj:=o;
      prm_buf[prm_cnt].ref:={};
    END;
    INC(prm_cnt);
  END threat;

  PROCEDURE threatening(u: pc.USAGE);
  BEGIN
    WHILE u#NIL DO threat(u.obj); u:=u.next END;
  END threatening;

  VAR
    f    : pc.OBJECT;
    i    : INTEGER;
BEGIN
  buf_errs:=FALSE; prm_cnt:=0;
  IF is_c_arr(t.base) THEN
    ASSERT(t.base.mode#pc.ty_array_of);
    prm_buf[0].mode:=pm_return;
    prm_buf[0].obj:=t.obj;
    prm_buf[0].ref:={LVL};
    prm_cnt:=1;
  END;
  IF (t.obj#NIL) & (t.obj.mode#pc.ob_type) THEN threatening(t.use) END;
  f:=t.prof;
  WHILE f#NIL DO
    ASSERT(f.mode IN pc.VARs);
    IF prm_cnt>=LEN(prm_buf) THEN buf_err; RETURN END;
    prm_buf[prm_cnt].mode:=pm_param;
    prm_buf[prm_cnt].obj:=f;
    prm_buf[prm_cnt].ref:={};
    IF    is_reference(f,t) THEN INCL(prm_buf[prm_cnt].ref,CRF);
    ELSIF is_pointer(f,t)   THEN INCL(prm_buf[prm_cnt].ref,REF);
    ELSIF is_c_arr(f.type)  THEN INCL(prm_buf[prm_cnt].ref,BSA);
    END;
    IF f.mode=pc.ob_varpar THEN INCL(prm_buf[prm_cnt].ref,LVL) END;
    IF (f.type.mode=pc.ty_array_of) & ~(t.flag IN cc.c_like_flags) THEN
      FOR i:=0 TO SHORT(f.type.len-1) DO
	INC(prm_cnt);
	IF prm_cnt>=LEN(prm_buf) THEN buf_err; RETURN END;
	prm_buf[prm_cnt].mode:=pm_len;
	prm_buf[prm_cnt].obj:=f;
	prm_buf[prm_cnt].ref:={};
      END;
    ELSIF (f.mode=pc.ob_seq) &
          (t.flag IN pc.LangSet{pc.flag_c,pc.flag_syscall,pc.flag_stdcall})
    THEN
      prm_buf[prm_cnt].mode:=pm_seq;
    ELSIF (f.mode=pc.ob_varpar) &
	  (f.type.mode=pc.ty_record) &
	  (f.type.flag=pc.flag_o2)
    THEN
      prm_buf[prm_cnt].mode:=pm_frec;
      INC(prm_cnt);
      IF prm_cnt>=LEN(prm_buf) THEN buf_err; RETURN END;
      prm_buf[prm_cnt].mode:=pm_type;
      prm_buf[prm_cnt].obj:=f;
      prm_buf[prm_cnt].ref:={};
    ELSIF t.flag IN cc.c_like_flags THEN
      INCL(prm_buf[prm_cnt].ref,CCH);
    END;
    INC(prm_cnt);
    f:=f.next;
  END;
END func_params;

PROCEDURE func_profile_definition*(
		t: pc.STRUCT;
		VAR res: ARRAY OF CHAR;
		def,pdf: BOOLEAN);
(*
	def: TRUE если в профиле должны быть указаны имена параметров,
	     т.е. за профилем последует тело функции.
	pdf: FALSE собственно профиль
	     TRUE описание параметров (в стиле устаревшего С)
*)

  VAR
    nm,str : STR;
    buf    : STR;
    i,j    : INTEGER;
    prm_buf: PRM_BUF;
    prm_cnt: INTEGER;
BEGIN
  res[0]:=0X;
  IF ~def & cc.op_krc THEN RETURN END;
  IF pdf & ~cc.op_krc THEN RETURN END;
  func_params(prm_buf,prm_cnt,t);
  IF prm_cnt=0 THEN
    IF ~cc.op_krc THEN nms.strcat(res,"void") END;
    RETURN;
  END;
  j:=0;
  FOR i:=0 TO prm_cnt-1 DO
    IF cc.op_krc & (prm_buf[i].mode=pm_seq) THEN RETURN END;
    IF (i>0) & ~pdf THEN nms.strcat(res,", ") END;
    IF    REF IN prm_buf[i].ref THEN str:="* ";
    ELSIF CRF IN prm_buf[i].ref THEN str:="& ";
    ELSE str:=""
    END;
    IF def OR (cur_mod.type.flag=pc.flag_bnrp) THEN
      IF def & (prm_buf[i].mode IN {pm_param,pm_frec}) THEN
        INCL(prm_buf[i].obj.tags,cc.otag_declared)
      END;
      IF prm_buf[i].mode=pm_return THEN
	o_second_name(prm_buf[i].obj,0,"_ret",nm);
      ELSIF prm_buf[i].mode IN {pm_len,pm_thr_len} THEN
	o_second_name(prm_buf[i].obj,j,"_len",nm); INC(j);
      ELSIF prm_buf[i].mode IN {pm_type,pm_thr_type} THEN
	o_second_name(prm_buf[i].obj,0,"_type",nm); INC(j);
      ELSE
	o_name(prm_buf[i].obj,nm); j:=0;
      END;
      nms.strcat(str,nm);
    ELSE
      IF str[0]#0X THEN
        ASSERT(str[1]=' ');
        str[1]:=0X;
      END;
      nm:=""; (* assigment to suppress warning: used before definition *)
    END;
    IF cc.op_krc & ~pdf THEN
      COPY(nm,buf); (* только имя параметра *)
    ELSIF prm_buf[i].mode=pm_seq THEN
      buf:="...";
    ELSIF prm_buf[i].mode IN {pm_type,pm_thr_type} THEN
      out.ff(buf,"X2C_TD %s",str);
    ELSIF prm_buf[i].mode IN {pm_len,pm_thr_len} THEN
      type_designator(buf,str,prm_buf[i].obj.type.inx);
    ELSIF prm_buf[i].mode=pm_return THEN
      type_desig(buf,str,prm_buf[i].obj.type.base,FALSE,FALSE);
    ELSIF cc.otag_notype IN prm_buf[i].obj.tags THEN
      type_designator_notype(buf,str,prm_buf[i].obj.type);
    ELSE
      type_desig(buf,str,prm_buf[i].obj.type,FALSE,TRUE);
    END;
    IF cc.op_krc & pdf THEN
      out.ff(str,"  %s;\n",buf); nms.strcat(res,str);
    ELSE
      nms.strcat(res,buf);
    END;
  END;
END func_profile_definition;

PROCEDURE type_constructor(
	VAR res	: ARRAY OF CHAR;
	ident-	: ARRAY OF CHAR;
	in_prm  : BOOLEAN;
	t	: pc.STRUCT): BOOLEAN;

(* если может, порождает конструктор (не обозначение) типа *)

  VAR buf,prm,pcl: STR; l: LONGINT; rf: BOOLEAN;
BEGIN
  ASSERT(ident[0]#' ');
  rf:=(ident[0]='*') OR (ident[0]='&');
  CASE t.mode OF
    |pc.ty_range:
      type_desig(res,ident,t.base,TRUE,in_prm);
    |pc.ty_pointer:
      IF (t.base.mode = pc.ty_array) & ~env.config.Option("GENPTRTOARR") THEN
        (* Pointer to array is generated to pointer to array base *)
        IF ident#"" THEN out.ff(buf,"* %s",ident);
        ELSE buf:="*";
        END;
        type_desig(res,buf,t.base.base,FALSE,FALSE);
      ELSIF (t.base.mode = pc.ty_array_of) & (t.flag = pc.flag_c) THEN
        (* Pointer to open "C" array is generated to pointer to array base *)
        buf:= "*";
        t:= t.base.base; l:= 1;
        WHILE t.mode = pc.ty_array_of DO
          t:= t.base;
          buf[l]:= '*'; INC(l);
        END;
        buf[l]:= 0X;
        IF ident#'' THEN out.ff(buf, "%s %s", buf, ident) END;
        type_desig(res,buf,t,FALSE,FALSE);
      ELSE
        (* Pointer to array is generated to pointer to array *)
        IF ident#"" THEN out.ff(buf,"* %s",ident);
        ELSE buf:="*";
        END;
        type_desig(res,buf,t.base,FALSE,FALSE);
      END;
    |pc.ty_opaque:
      out.ff(res,"void *%s",ident);
    |pc.ty_set:
      IF ~is_c_arr(t) THEN RETURN FALSE END;
      l:=(t.len-1) DIV LSET_BITS +1;
      CASE LSET_BITS OF
        | 8: nms.x2c(nms.nm_shortcard,buf);
        |16: nms.x2c(nms.nm_cardinal,buf);
        |32: nms.x2c(nms.nm_longcard,buf);
      END;
      IF rf THEN out.ff(res,"%s (%s)[%d]",buf,ident,l);
      ELSE out.ff(res,"%s %s[%d]",buf,ident,l);
      END;
    |pc.ty_proctype:
      func_profile_definition(t,prm,FALSE,FALSE);
      IF cc.op_proclass OR (t.flag IN cc.special_proclass) THEN
        nms.get_proclass(t.flag,pcl)
      ELSE pcl:="";
      END;
      IF is_c_arr(t.base) THEN
        out.ff(buf,"* (%s *%s)(%s)",pcl,ident,prm);
        type_designator(res,buf,get_base_type(t.base));
      ELSE
        out.ff(buf,"(%s *%s)(%s)",pcl,ident,prm);
        type_designator(res,buf,t.base);
      END;
    |pc.ty_array,pc.ty_SS:
      IF rf THEN out.ff(buf,"(%s)[%d]",ident,t.len);
      ELSE out.ff(buf,"%s[%d]",ident,t.len);
      END;
      type_designator(res,buf,t.base);
    |pc.ty_array_of:
      IF rf THEN out.ff(buf,"(%s)[]",ident);
      ELSE out.ff(buf,"%s[]",ident);
      END;
      REPEAT t:=t.base UNTIL t.mode#pc.ty_array_of;
      type_designator(res,buf,t);
  ELSE
    RETURN FALSE;
  END;
  RETURN TRUE;
END type_constructor;

PROCEDURE type_desig*(
	VAR res	: ARRAY OF CHAR;
	ident-	: ARRAY OF CHAR;
	t	: pc.STRUCT;
	def	: BOOLEAN;
	prm     : BOOLEAN);

  VAR nm: STR;
BEGIN
  IF (~prm OR (t.mode#pc.ty_array_of)) & t_usage(t,nm,def,FALSE) THEN
    IF ident="" THEN COPY(nm,res);
    ELSE out.ff(res,"%s %s",nm,ident);
    END;
  ELSIF type_constructor(res,ident,prm,t) THEN
    (* nothing *)
  ELSE
    env.errors.Error(t.pos,1016);
    res[0]:='';
  END;
END type_desig;

(*--------------------------- Declarations ------------------------------*)

PROCEDURE var_addr_expression*(o: pc.OBJECT);
  VAR buf: STR; ref: BOOLEAN;
      o_attr :pc.NODE;
BEGIN
  ASSERT(o.mode=pc.ob_var);
  ASSERT(o.lev=0);
  ref:=is_reference(o,o.host);
  out.ws(" = ");
  o_attr := o.attr(pc.NODE);
  IF (o_attr.mode=pc.nd_unary) &
     (o_attr.sub=pc.su_adr) &
     c_types_equ(o.type,o_attr.l.type,TRUE)
  THEN
    IF ref THEN
      IF is_c_arr(o.type) THEN
        gen_value(o_attr.l,1,{CNS,CHK,LVL,BSA});
      ELSE
        gen_value(o_attr.l,1,{CNS,CHK,LVL});
      END;
    ELSE
      gen_value(o_attr.l,1,{CNS,CHK,REF});
    END;
  ELSE
    IF ref THEN out.wr('*') END;
    out.wr('(');
    type_designator(buf,"*",o.type);
    out.wf("%s)",buf);
    gen_value(o_attr,13,{CNS,CHK});
  END;
END var_addr_expression;

PROCEDURE glo_var_declaration(o: pc.OBJECT);
  VAR buf: STR; cns,b: BOOLEAN;
BEGIN
  INCL(o.tags,cc.otag_pub_defined);
  IF cc.op_comments THEN cmt.first_comment(o.pos,1,0) END;
  IF (o.mode=pc.ob_cons) &
     (o.type.mode=pc.ty_SS) &
     (o.type.len<=2) &
     (o.val.mode=pc.nd_value) &
     (o.flag=pc.flag_c)
  THEN
    o_name(o,buf);
    zz_tmp.index_get(0,o.val.val);
    out.disable_lf(b);
    out.wf("#define chr_%s ",buf);
    const_aggregate(zz_tmp,o.type.base,14,FALSE,FALSE);
    out.restore_lf(b); out.wl;
  END;
  IF (o.mode=pc.ob_cons) & (
          (o.type.mode IN pc.TY_SET{pc.ty_ZZ,pc.ty_RR,pc.ty_SS,pc.ty_AA})
       OR (o.val.mode#pc.nd_value)
       OR (* can be used in case or other "constant" positions *)
          (~ cc.op_cpp & o.is_public() & o.type.is_ordinal())
       OR ((o.type.mode=pc.ty_enum) & ~ cc.op_cpp)
       OR (o.type.mode=pc.ty_proctype) &
          (~ cc.op_cpp OR (o.type.flag=pc.flag_c))
          (* C function can be a #define macros *)
       OR (cc.op_gencnsexp & ~ cc.op_cpp
            & ((o.type.mode = pc.ty_char) OR
               (o.type.mode = pc.ty_set) & ~ is_c_arr(o.type)
              )
          )
        )
  THEN
    o_name(o,buf);
    out.disable_lf(b);
    out.ws("#define "); out.ws(buf); out.wr(' ');
    IF is_c_arr(o.type) THEN
      gen_value(o.val,14,{BSA});
    ELSE
      gen_value(o.val,14,{});
    END;
    out.restore_lf(b);
    out.wl;
    INCL(o.tags,cc.otag_pri_defined);
  ELSE
    cns:=cc.op_cpp & (o.mode=pc.ob_cons) & is_c_num(o.type);
    IF cns THEN
      out.ws("static const ");
      INCL(o.tags,cc.otag_pri_defined);
    ELSIF o.is_public() THEN
      IF ~ cc.op_krc THEN out.ws("extern ") END;
      IF cc.op_gendll THEN out.ws("X2C_DLL_TAG ") END;
    ELSE
      ASSERT(o.mno=cur_mod.mno);
      out.ws("static ");
      INCL(o.tags,cc.otag_pri_defined);
    END;
    IF ~ cc.op_krc & (o.mode = pc.ob_var) & (pc.otag_volatile IN o.tags) THEN
      out.ws("volatile ");
    END;
    (* 'const' has different meaning in C and C++ *)
    declarator(buf,o,FALSE);
    out.ws(buf);
    IF (cns OR ~ o.is_public()) THEN
      IF o.mode=pc.ob_cons THEN
        out.ws(" = ");
        IF is_c_arr(o.type) THEN
          gen_value(o.val,1,{CNS,CHK,BSA});
        ELSE
          gen_value(o.val,1,{CNS,CHK});
        END;
        INCL(o.tags,cc.otag_pri_defined);
      ELSIF o.attr#NIL THEN
        var_addr_expression(o);
        INCL(o.tags,cc.otag_pri_defined);
      END;
    END;
    out.wr(';');
    IF cc.op_comments THEN cmt.tail_comment(o.end) END;
    out.wl;
  END;
  IF cc.op_comments THEN cmt.last_comment(o.end,0) END;
  out.wl;
END glo_var_declaration;

PROCEDURE enum_declaration(t: pc.STRUCT);

  PROCEDURE line_no(ps-: env.TPOS): LONGINT;
    VAR fnm: env.String; l,c: LONGINT;
  BEGIN
    ps.unpack(fnm,l,c);
    RETURN l
  END line_no;

  VAR
    f,enums: pc.OBJECT;
    i      : nms.INFO;
    nm     : STR;
    cur    : LONGINT;
BEGIN
  ASSERT(t.obj.mno=cur_mod.mno);
  o_name(t.obj,nm);
  IF cc.op_comments THEN cmt.first_comment(t.pos,1,0) END;
  IF ~ cc.op_constenum THEN out.wf("enum %s {",nm) END;
  enums:=t.prof;
  t.prof:=NIL; (* clear enum list - to use values in glo_var_declaration *)
  f:=enums;
  WHILE f#NIL DO
    ASSERT(f.mode=pc.ob_cons);
    ASSERT(f.host=t);
    ASSERT(f.type=t);
    i:=o_info(f,-1,"");
    i.seg:=seg.segment;
    IF cc.op_constenum THEN
      glo_var_declaration(f);
    ELSE
      cur:=line_no(f.pos);
      out.ws(i.name^);
      IF f.next#NIL THEN out.ws(", ") ELSE out.ws('};') END;
      IF (f.next=NIL) OR (line_no(f.next.pos)#cur) THEN
        IF cc.op_comments THEN cmt.tail_comment(f.pos) END;
        out.wl;
        IF f.next#NIL THEN out.sp(1) END;
      END;
    END;
    INCL(f.tags,cc.otag_declared);
    INCL(f.tags,cc.otag_pub_defined);
    INCL(f.tags,cc.otag_pri_defined);
    f:=f.next;
  END;
  t.prof:=enums; (* restore enum list *)
  IF cc.op_comments THEN cmt.last_comment(t.end,0) END;
  IF ~ cc.op_constenum THEN
    out.wl;
    IF pc.ttag_no_struct IN t.tags THEN
      out.wf("typedef enum %s %s;\n",nm,nm);
    END;
  END;
  INCL(t.obj.tags,cc.otag_pub_defined);
  INCL(t.obj.tags,cc.otag_pri_defined);
END enum_declaration;

PROCEDURE array_of_declaration(t: pc.STRUCT);
  VAR buf,str,nm: STR; n: LONGINT; bs: pc.STRUCT;
BEGIN
  ASSERT(t.mode=pc.ty_array_of);
  o_name(t.obj,nm);
  out.wf("\nstruct %s {\n",nm);
  n:=t.len; bs:=t;
  WHILE n>0 DO bs:=bs.base; DEC(n) END;
  nms.x2c(nms.nm_dynarr_addr,buf);
  out.ff(str,"* %s",buf);
  type_desig(buf,str,bs,FALSE,FALSE);
  out.sp(1); out.wf("%s;\n",buf);
  nms.x2c(nms.nm_index,str);
  FOR n:=0 TO t.len-1 DO
    IF n#0 THEN
      nms.x2c(nms.nm_dynarr_size,buf);
      out.sp(1); out.wf("%s %s%d;\n",str,buf,n)
    END;
    nms.x2c(nms.nm_dynarr_len,buf);
    out.sp(1); out.wf("%s %s%d;\n",str,buf,n);
  END;
  out.wf("};\n");
  IF pc.ttag_no_struct IN t.tags THEN
    out.wf("typedef struct %s %s;\n",nm,nm);
  END;
  out.wl;
END array_of_declaration;

PROCEDURE type_declaration(o: pc.OBJECT);
(* предварительное объявление типа *)

  PROCEDURE recursion_in_parms(p: pc.OBJECT);
  BEGIN
    WHILE p#NIL DO
      IF (p.type.obj#NIL) & (cc.otag_declaring IN p.type.obj.tags) THEN
        INCL(p.tags,cc.otag_notype);
      END;
      p:=p.next;
    END;
  END recursion_in_parms;

  PROCEDURE out_typedef(buf-,nm-: STR);
  BEGIN
    IF cc.op_comments THEN cmt.first_comment(o.pos,1,0) END;
    IF nm = "" THEN
      out.wf("typedef %s;",buf);
    ELSE
      out.wf("typedef %s %s;",buf,nm);
    END;
    IF cc.op_comments THEN cmt.tail_comment(o.pos) END;
    out.wl;
    IF cc.op_comments THEN cmt.last_comment(o.pos,0) END;
    out.wl;
  END out_typedef;

  VAR nm,buf: STR;
BEGIN
  ASSERT(o.mode=pc.ob_type);
  ASSERT(o.type.obj#NIL);
  o_name(o,nm);
  IF o.type.obj#o THEN (* rename *)
    IF ~t_usage(o.type,buf,TRUE,TRUE) THEN ASSERT(FALSE) END;
    out_typedef(buf,nm);
    INCL(o.tags,cc.otag_pub_defined);
    INCL(o.tags,cc.otag_pri_defined);
  ELSIF o.type.mode=pc.ty_record THEN
    ccr.Record(o.type);
    IF cc.ttag_union IN o.type.tags THEN
      out.wf("union %s;\n",nm);
      IF pc.ttag_no_struct IN o.type.tags THEN
        out.wf("typedef union %s %s;\n",nm,nm);
      END;
    ELSE
      out.wf("struct %s;\n",nm);
      IF pc.ttag_no_struct IN o.type.tags THEN
        out.wf("typedef struct %s %s;\n",nm,nm);
      END;
    END;
    IF o.type.flag=pc.flag_o2 THEN
      o_second_name(o,1,"_desc",nm);
      out.ws("extern ");
      IF cc.op_gendll THEN out.ws("X2C_DLL_TAG ") END;
      out.wf("struct X2C_TD_STR %s;\n",nm);
    END;
    out.wl;
  ELSIF o.type.mode=pc.ty_enum THEN
    enum_declaration(o.type);
    out.wl;
  ELSIF o.type.mode=pc.ty_array_of THEN
    array_of_declaration(o.type);
  ELSIF std_type(o.type,buf) THEN
    out_typedef(buf,nm);
    INCL(o.tags,cc.otag_pub_defined);
    INCL(o.tags,cc.otag_pri_defined);
  ELSE
    IF o.type.mode=pc.ty_proctype THEN recursion_in_parms(o.type.prof) END;
    IF ~type_constructor(buf,nm,FALSE,o.type) THEN
      env.errors.Error(o.pos,1017);
      buf[0]:='';
    END;
    out_typedef(buf,"");
    INCL(o.tags,cc.otag_pub_defined);
    INCL(o.tags,cc.otag_pri_defined);
  END;
END type_declaration;

PROCEDURE loc_var_declaration(o: pc.OBJECT);
  VAR buf: STR;
BEGIN
  ASSERT(cur_proc#NIL);
  ASSERT(o.host=cur_proc);
  ASSERT(~(pc.otag_valpar IN o.tags));
  ASSERT(o.mode#pc.ob_varpar);
  IF cc.op_comments THEN cmt.first_comment(o.pos,1,1) END;
  out_line_no(o.pos,1);
  out.sp(1); declarator(buf,o,FALSE); out.ws(buf);
  IF (o.type.mode=pc.ty_pointer) & (o.type.flag=pc.flag_o2) THEN
    out.ws(" = 0");
  END;
  out.wr(";");
  IF cc.op_comments THEN cmt.tail_comment(o.pos) END;
  out.wl;
  IF cc.op_comments THEN cmt.last_comment(o.pos,1) END;
  INCL(o.tags,cc.otag_pub_defined);
  INCL(o.tags,cc.otag_pri_defined);
END loc_var_declaration;

PROCEDURE func_declaration(o: pc.OBJECT);
(* предварительное описание функции *)
  VAR
    buf: ARRAY 2048 OF CHAR; (* must be large *)
    nm : STR;
BEGIN
  IF (o.mode=pc.ob_eproc) & env.config.Option("NOEXTERN") THEN
    (* nothing *)
  ELSE
    IF cc.op_comments THEN cmt.first_comment(o.pos,1,0) END;
    IF nms.func_is_extern(o) THEN out.ws("extern ");
    ELSE out.ws("static ");
    END;
    IF cc.op_gendll & ((o.mode=pc.ob_xproc) OR nms.func_is_extern(o)) THEN
      out.ws("X2C_DLL_TAG ");
    END;
    func_profile_definition(o.type,buf,FALSE,FALSE);
    declarator(nm,o,FALSE); out.ws(nm);
    out.wr('('); out.ws(buf); out.ws(");"); out.wl;
    IF o.host.mode=pc.ty_record THEN
      out.ws("typedef ");
      declarator(nm,o,TRUE); out.ws(nm);
      out.wr('('); out.ws(buf); out.ws(");"); out.wl;
    END;
    IF cc.op_comments THEN cmt.last_comment(o.pos,0) END;
    out.wl;
  END;
  INCL(o.tags,cc.otag_pub_defined);
END func_declaration;

PROCEDURE object_declaration*(o: pc.OBJECT);
(* предварительное объявление объекта *)
  VAR i: nms.INFO; sv: TMP_VAR;
BEGIN
  IF cc.otag_declared IN o.tags THEN RETURN END;
  IF cc.otag_declaring IN o.tags THEN
    env.errors.Error(o.pos,1018);
    ASSERT(o.mode=pc.ob_type);
    RETURN;
  END;
  ASSERT(o.mno=cur_mod.mno);
  INCL(o.tags,cc.otag_declaring);
  sv:=tmp_busy; tmp_busy:=NIL;
  IF (o.mode=pc.ob_var) & (o.lev>0) THEN
    out.push;
  ELSE
    i:=o_info(o,-1,"");
    IF i.seg=NIL THEN
      ASSERT(gen_def);
      ASSERT(o.lev=0);
      (* declaring non public object in header file *)
      seg.new_seg(i.seg,o.pos,0);
    END;
    seg.enter(i.seg);
  END;
  CASE o.mode OF
    |pc.ob_var   : IF o.lev=0 THEN glo_var_declaration(o);
                   ELSE loc_var_declaration(o);
                   END;
    |pc.ob_cons  : glo_var_declaration(o);

    |pc.ob_proc,pc.ob_xproc,pc.ob_eproc,pc.ob_lproc,
     pc.ob_cproc : func_declaration(o);

    |pc.ob_type  : type_declaration(o);
    |pc.ob_module: IF ~(cur_mod.type.flag IN cc.c_like_flags+pc.LangSet{pc.flag_sl1}) THEN
                     out.wl;
                     module_body_def(cur_mod,FALSE);
                     out.wf(";\n\n");
                     INCL(o.tags,cc.otag_pub_defined);
                   END;
  ELSE
    (* o.mode=127 -> object is deleted! *)
    env.errors.Fault(o.pos,1019);
  END;
  IF (o.mode=pc.ob_var) & (o.lev>0) THEN
    out.append(loc_txt); out.pop;
  ELSE
    seg.exit;
  END;
  EXCL(o.tags,cc.otag_declaring);
  INCL(o.tags,cc.otag_declared);
  IF tmp_busy#NIL THEN
    env.errors.Error(o.pos,1019);
  END;
  tmp_busy:=sv;
END object_declaration;

PROCEDURE val*(x: LONGINT): pc.VALUE;
  VAR v: pc.VALUE;
BEGIN
  IF type_ZZ=NIL THEN
    NEW(type_ZZ);
    type_ZZ.mode:=pc.ty_ZZ;
    type_ZZ.pos:=env.null_pos;
  END;
  v:=pc.value.new(type_ZZ.pos,type_ZZ);
  v.set_integer(x);
  RETURN v;
END val;

PROCEDURE rval*(x: LONGREAL): pc.VALUE;
  VAR v: pc.VALUE;
BEGIN
  IF type_RR=NIL THEN
    NEW(type_RR);
    type_RR.mode:=pc.ty_RR;
    type_RR.pos:=env.null_pos;
  END;
  v:=pc.value.new(type_RR.pos,type_RR);
  v.set_real(x);
  RETURN v;
END rval;

PROCEDURE ini*;
BEGIN
  zz_tmp:=val(0);
  rr_tmp:=rval(0.0);
  IF max_longint=NIL THEN max_longint:=val(MAX(LONGINT)) END;
  IF zz_zero=NIL THEN zz_zero:=val(0) END;
  LSET_T := pc.new_type(pc.ty_longcard);
  SEQU_T := pc.new_type(pc.ty_void);
  
  LSET_BITS:=SHORT(pc.code.get_size(pc.su_bits,LSET_T));
  cns_vars:=NIL;
  tmp_vars:=NIL;
  tmp_busy:=NIL;
  mod_usage:=NIL;
  sl1_usage:=FALSE;
END ini;

PROCEDURE exi*;
BEGIN
  zz_tmp:=NIL;
  rr_tmp:=NIL;
  type_RR:=NIL;
  type_ZZ:=NIL;
  LSET_T:=NIL;
  SEQU_T:=NIL;
  out.gstr(glo_txt);
  out.gstr(glo_txt);
  out.gstr(loc_txt);
  cur_proc:=NIL;
  cns_vars:=NIL;
  tmp_vars:=NIL;
  tmp_busy:=NIL;
  cur_mod:=NIL;
  mod_usage:=NIL;
END exi;

BEGIN
  cmp_value_tmp:=NIL;
  max_longint:=NIL;
  zz_zero:=NIL;
END ccDcl.
