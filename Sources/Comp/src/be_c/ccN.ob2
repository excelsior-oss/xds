(** Copyright (c) 1991,97 XDS Ltd, Russia. All Rights Reserved. *)
(** C back-end: visibility, names, standard names, keywords *)
MODULE ccN; (** Sem 22-Sep-93. *)

(* Modifications:
   15/Feb/96 Ned  2.12  nm_proclass is deleted. Added:
                        procedure get_proclass, constants:
                        special_proclass, default_proclass.
   17/Mar/96 Ned  2.12  object and type tags are moved to ccK.
   07/Sep/99 AlexM      New standard keyword is added: X2C_FIELD_OFS
*)

IMPORT
  pc :=pcK,
  fmt:=xcStr,
  xfs:=xiFiles,
  env:=xiEnv,
  cc :=ccK,
  seg:=ccSeg,
<* IF DB_TRACE THEN *>
  dbg:=pcVisIR,
<* END *>
  xcStr;

(** Object  names *)

CONST
  INDEX_LEN = 256;

TYPE
  NAME* = POINTER TO name_rec; (* OBJECT extension *)
  RENM* = POINTER TO renm_rec; (* OBJECT extension *)
  INFO* = POINTER TO info_rec; (* OBJECT extension *)
  TREE* = POINTER TO tree_rec; (* STRUCT extension *)

  name_rec* = RECORD (pc.bext_rec)
    name*: pc.STRING;
    l    : NAME;
    r    : NAME;
    bal  : INTEGER;
  END;

  renm_rec* = RECORD (name_rec)
    renm : pc.STRING;
  END;

  info_rec* = RECORD (name_rec)
    obj* : pc.OBJECT;
    no*  : INTEGER;
    seg* : seg.SEGMENT;
    nxt* : INFO;
  END;

  tree_rec* = RECORD (pc.bext_rec)
    root : NAME;
    index: POINTER TO ARRAY INDEX_LEN OF NAME;
    node*: pc.BEXT;
    seg* : seg.SEGMENT; (* record type: definition segment      *)
  END;

CONST
(** name's indexes *)
  nm_shortcard	*= 0;
  nm_cardinal   *= 1;
  nm_longcard   *= 2;
  nm_shortint   *= 3;
  nm_integer    *= 4;
  nm_longint    *= 5;
  nm_real       *= 6;
  nm_longreal  	*= 7;
  nm_ld_real	*= 8;
  nm_complex   	*= 9;
  nm_lcomplex  	*=10;
  nm_boolean   	*=12;
  nm_char      	*=13;
  nm_AA        	*=14;
  nm_loc       	*=15;
  nm_protection	*=16;
  nm_void      	*=17;
  nm_a_chk     	*=21;
  nm_p_chk     	*=22;
  nm_dynarr_addr *=30;
  nm_dynarr_len  *=31;
  nm_dynarr_size *=32;
  nm_memcmp      *=33;
  nm_memcpy      *=34;
  nm_strcmp      *=35;
  nm_quo         *=36;
  nm_div         *=37;
  nm_rem         *=38;
  nm_mod         *=39;
  nm_cap         *=40;
  nm_in          *=41;  (* IN SET *)
  nm_inl         *=42;  (* IN LONG SET *)
  nm_alloc_param *=43;
  nm_free_param  *=44;
  nm_rts_body    *=45;
  nm_length      *=46;
  nm_set_range   *=47;
  nm_assert      *=48;
  nm_case_trap   *=49;
  nm_return_trap *=50;
  nm_trap        *=51;
  nm_seq_type    *=52;
  nm_rot_set     *=53;
  nm_rot_lset    *=54;
  nm_trunci      *=55;
  nm_truncc      *=56;
  nm_cplx_cmp    *=58;
  nm_cplx_add    *=59;
  nm_cplx_sub    *=60;
  nm_cplx_mul    *=61;
  nm_cplx_div    *=62;
  nm_cplx_lcmp   *=63;
  nm_cplx_ladd   *=64;
  nm_cplx_lsub   *=65;
  nm_cplx_lmul   *=66;
  nm_cplx_ldiv   *=67;
  nm_i_chk       *=68;
  nm_i_chkl      *=69;
  nm_r_chk       *=70;
  nm_r_chkl      *=71;
  nm_r_chku      *=72;
  nm_r_chkul     *=73;
  nm_halt        *=74;
  nm_abort       *=75;
  nm_move        *=76;
  nm_copy        *=77;
  nm_allocate    *=78;
  nm_deallocate  *=79;
  nm_dynallocate *=80;
  nm_dyndeallocate*=81;
  nm_new_open    *=82;
  nm_new    	 *=83;
  nm_index	 *=84;
  nm_dispose     *=85;
  nm_cplx_neg    *=86;
  nm_cplx_lneg   *=87;
  nm_strncpy     *=88;
  nm_lsh_set     *=89;
  nm_lsh_lset    *=90;
  nm_ash         *=91;
  nm_fill         =92;
  nm_dyncallocate*=93;
  nm_dyncdeallocate*=94;
  nm_field_ofs   *=95;
  nm_quo64       *=96;
  nm_div64       *=97;
  nm_rem64       *=98;
  nm_mod64       *=99;
  nm_divr        *=100;
  nm_divl        *=101;
  nm_Number       =102;  -- number of nm_ constant it's used in VAR "names" 

  sn_name        *= 0;
  sn_rec_td      *= 1;
  sn_rec_offs    *= 2;
  sn_rec_procs   *= 3;
  sn_mod_td      *= 4;
  sn_mod_offs    *= 5;
  sn_mod_procs   *= 6;
  sn_mod_cmds    *= 7;
  sn_mod_cnms    *= 8;
  sn_mod_begin   *= 9;
  sn_mod_init    *=10;
  sn_var_type    *=11;
  sn_func_ret    *=12;
  sn_meth_type   *=13;
  sn_case        *=14;
  sn_c_name      *=15;
  sn_arr_len     *=20;

VAR
  ctype      : ARRAY 256 OF BOOLEAN;
  capital    : ARRAY 256 OF CHAR;
  ident_len  : LONGINT;
  name_cnt   : LONGINT;
  name_cnt_en: BOOLEAN;
  sys_tree   : TREE;
  glo_tree   : TREE;
  sym_list   : INFO;
  names      : ARRAY nm_Number OF pc.STRING;

PROCEDURE func_is_extern*(o: pc.OBJECT): BOOLEAN;
BEGIN
  ASSERT(o.mode IN pc.PROCs);
  RETURN (o.mode=pc.ob_eproc) OR
	 (o.mode=pc.ob_lproc) OR
	 (pc.otag_public IN o.tags) OR
         (o.mode=pc.ob_xproc) & cc.op_gendll OR
	 (o.host.mode=pc.ty_record) & (pc.otag_public IN o.host.obj.tags);
END func_is_extern;

PROCEDURE strcat*(VAR d: ARRAY OF CHAR; s-: ARRAY OF CHAR);
  VAR i,j: LONGINT;
BEGIN
  i:=0;
  WHILE (i<LEN(d)) & (d[i]#'') DO INC(i) END;
  j:=0;
  WHILE (j<LEN(s)) & (s[j]#'') & (i<LEN(d)-1) DO
    d[i]:=s[j]; INC(i); INC(j)
  END;
  d[i]:='';
END strcat;

PROCEDURE strcmp(x-: ARRAY OF CHAR; y-: ARRAY OF CHAR): INTEGER;
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (x[i]#'') & (capital[ORD(x[i])]=capital[ORD(y[i])]) DO INC(i) END;
  IF capital[ORD(x[i])]>capital[ORD(y[i])] THEN RETURN  1 END;
  IF capital[ORD(x[i])]<capital[ORD(y[i])] THEN RETURN -1 END;
  RETURN 0;
END strcmp;

PROCEDURE app_bal(x: NAME; VAR p: NAME; VAR h: BOOLEAN): NAME;
  VAR p1,p2: NAME; y: NAME; r: LONGINT;
BEGIN
  IF p=NIL THEN
    p:=x; x.l:=NIL; x.r:=NIL; h:=TRUE; x.bal:=0; RETURN NIL;
  END;
  r:=strcmp(p.name^,x.name^);
  IF r>0 THEN
    y:=app_bal(x,p.l,h);
    IF h THEN
      CASE p.bal OF
	| 1: p.bal:=0; h:=FALSE;
	| 0: p.bal:=-1;
	|-1:
	  p1:=p.l;
	  IF p1.bal=-1 THEN
	    p.l:=p1.r; p1.r:=p; p.bal:=0; p:=p1;
	  ELSE
	    p2:=p1.r; p1.r:=p2.l; p2.l:=p1; p.l:=p2.r; p2.r:=p;
	    IF p2.bal=-1 THEN p.bal:=+1 ELSE p.bal:=0 END;
	    IF p2.bal=+1 THEN p1.bal:=-1 ELSE p1.bal:=0 END;
	    p:=p2;
	  END;
	  p.bal:=0; h:=FALSE;
      END;
    END;
    RETURN y;
  ELSIF r<0 THEN
    y:=app_bal(x,p.r,h);
    IF h THEN
      CASE p.bal OF
	|-1: p.bal:=0; h:=FALSE;
	| 0: p.bal:=+1;
	|+1:
	  p1:=p.r;
	  IF p1.bal=+1 THEN
	    p.r:=p1.l; p1.l:=p; p.bal:=0; p:=p1;
	  ELSE
	    p2:=p1.l; p1.l:=p2.r; p2.r:=p1; p.r:=p2.l; p2.l:=p;
	    IF p2.bal=+1 THEN p.bal:=-1 ELSE p.bal:=0 END;
	    IF p2.bal=-1 THEN p1.bal:=+1 ELSE p1.bal:=0 END;
	    p:=p2;
	  END;
	  p.bal:=0; h:=FALSE;
      END;
    END;
    RETURN y;
  ELSE
    h:=FALSE; RETURN p;
  END;
END app_bal;

PROCEDURE search(t: TREE; n-: ARRAY OF CHAR): NAME;
  VAR j: LONGINT; i: NAME;
BEGIN
  IF t=NIL THEN t:=glo_tree END;
  IF t.index#NIL THEN
    j:=ORD(n[0]);
    IF j#0 THEN j:=(j+ORD(n[1])) MOD INDEX_LEN END;
    i:=t.index[j];
  ELSE
    i:=t.root;
  END;
  LOOP
    IF i=NIL THEN RETURN NIL END;
    j:=strcmp(i.name^,n);
    IF j=0 THEN RETURN i END;
    IF j<0 THEN i:=i.r ELSE i:=i.l END;
  END;
END search;

PROCEDURE insert(tree: TREE; i: NAME): NAME;
  VAR j: LONGINT; h: BOOLEAN;
BEGIN
  ASSERT(i#NIL);
  IF tree=NIL THEN tree:=glo_tree END;
  i.l:=NIL; i.r:=NIL;
  IF tree.index#NIL THEN
    j:=ORD(i.name[0]);
    IF j#0 THEN j:=(j+ORD(i.name[1])) MOD INDEX_LEN END;
    RETURN app_bal(i,tree.index[j],h);
  ELSE
    RETURN app_bal(i,tree.root,h);
  END;
END insert;

PROCEDURE new_tree*(VAR t: TREE);
BEGIN
  NEW(t);
END new_tree;

PROCEDURE get_tree(h: pc.STRUCT; md: pc.OB_MODE): TREE;
  VAR l: TREE;
BEGIN
  IF h=NIL THEN RETURN glo_tree END;
  IF md IN (pc.PROCs+pc.OB_SET{pc.ob_type,pc.ob_cons}) THEN RETURN glo_tree END;
  IF h.mode=pc.ty_enum THEN
    ASSERT(h.obj.mode=pc.ob_type);
    h:=h.obj.host
  END;
  IF h.mode=pc.ty_module THEN RETURN glo_tree END;
  IF h.obj#NIL THEN h:=h.obj.type END;
  IF h.ext#NIL THEN RETURN h.ext(TREE) END;
  ASSERT((h.mode=pc.ty_record) OR (h.mode=pc.ty_proctype));
  new_tree(l);
  h.ext:=l;
  RETURN l;
END get_tree;

PROCEDURE disable*(h: pc.STRUCT; nm-: ARRAY OF CHAR);
  VAR t: TREE; i: NAME;
BEGIN
  t:=get_tree(h,pc.ob_var);
  NEW(i);
  NEW(i.name,LENGTH(nm)+1);
  COPY(nm,i.name^);
  i:=insert(t,i);
END disable;

PROCEDURE make_name*(
	VAR res: ARRAY OF CHAR;
	host   : pc.STRUCT;  (* область действия *)
	mod    : pc.STRUCT;  (* если # NIL то префикс - имя модуля *)
	mode   : pc.OB_MODE;    (* используется для определения С видимости *)
	from-  : ARRAY OF CHAR;  (* образец для имени *)
	no     : INTEGER     (* номер имени, если < 0 то имя сишное *)
	): INFO;

  PROCEDURE chk_name(VAR fr: ARRAY OF CHAR): BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    i:=0;
    LOOP
      CASE from[i] OF
	|0X: fr[i]:=0X; EXIT;
	|'.','$','#': fr[i]:='_';
      ELSE
	IF NOT ctype[ORD(from[i])] THEN
	  RETURN FALSE;
	ELSE
	  fr[i]:=from[i];
	END;
      END;
      INC(i);
    END;
    RETURN i>0;
  END chk_name;

  VAR
    fr,nm: pc.NAME;
    sf   : pc.NAME;
    n,m  : LONGINT;
    loc  : TREE;
    i    : INFO;
    j    : NAME;

BEGIN
  NEW(i);
  i.l:=NIL;
  i.r:=NIL;
  i.seg:=seg.segment;
  loc:=get_tree(host,mode);
  IF no>=0 THEN
    n:=0;
    IF mod#NIL THEN
      ASSERT(mod.mode=pc.ty_module);
      WHILE (mod.obj.name^[n]#0X) & (n<LEN(nm)-2) DO
	nm[n]:=mod.obj.name^[n]; INC(n);
      END;
      nm[n]:='_'; INC(n);
    END;
    IF ~chk_name(fr) THEN fr:="anonym" END;
    m:=0;
    WHILE (n<LEN(nm)-1) & (fr[m]#0X) DO
      nm[n]:=fr[m]; INC(n); INC(m);
    END;
    nm[n]:=0X;
    m:=-1;
    LOOP
      IF m>=0 THEN
        fmt.prn_txt(sf,"%d",m);
        fmt.prn_txt(res,"%.*s%s",ident_len-LENGTH(sf),nm,sf);
      ELSIF n<=ident_len THEN
        COPY(nm,res);
      ELSE
        fmt.prn_txt(res,"%.*s",ident_len,nm);
      END;
      IF mode IN pc.OB_SET{pc.ob_eproc,pc.ob_lproc,pc.ob_module} THEN EXIT END;
      j:=search(sys_tree,res);
      WHILE (j#NIL) & (j IS RENM) DO
        COPY(j(RENM).renm^,res);
        j:=search(sys_tree,res);
      END;
      IF (j#NIL) OR (search(loc,res)#NIL) THEN
        (* nothing *)
      ELSIF (loc=glo_tree) OR (search(glo_tree,res)=NIL) THEN
        EXIT
      END;
      IF (m>=10) & (name_cnt_en) THEN
        IF m>=name_cnt THEN name_cnt:=m+1 END;
        m:=name_cnt; INC(name_cnt);
      ELSE
        INC(m);
      END;
    END;
    i.no:=no;
    NEW(i.name,LENGTH(res)+1);
    COPY(res,i.name^);
    j:=insert(loc,i);
    ASSERT((j=NIL) OR (mode IN pc.OB_SET{pc.ob_eproc,pc.ob_lproc,pc.ob_module}));
  ELSE
    i.no:=0;
    COPY(from,res);
    NEW(i.name,LENGTH(res)+1);
    COPY(res,i.name^);
    (* C name can be equal reserved name *)
    j:=insert(loc,i);
  END;
  RETURN i;
END make_name;

PROCEDURE make_info*(
	o      : pc.OBJECT;
	no     : INTEGER;
	suffix-: ARRAY OF CHAR): INFO;

(* no = -2 : use suffix only *)
  PROCEDURE Check_X2C(nm : pc.STRING) : BOOLEAN;
  BEGIN
    RETURN (LENGTH(nm^) >= 4) & 
           (nm^[0] = 'X') & (nm^[1] = '2') & (nm^[2] = 'C') & (nm^[3] = '_');
  END Check_X2C;

  VAR m: pc.STRUCT; res,nm: ARRAY 128 OF CHAR; i: INFO; j: NAME;
BEGIN
  IF ~(o.flag IN pc.LangSet{pc.flag_o2,pc.flag_m2}) THEN
    m:=NIL;
  ELSIF o.mode IN pc.PROCs THEN
    IF func_is_extern(o) THEN m:=pc.mods[o.mno].type ELSE m:=NIL END;
  ELSIF
     (o.mode=pc.ob_cons) OR

     (pc.otag_public IN o.tags) OR

     (o.mode=pc.ob_type) &
     (o.type.mode=pc.ty_record) &
     (o.type.flag=pc.flag_o2) OR

     (o.host#NIL) &
     (o.host.mode=pc.ty_enum) &
     (pc.otag_public IN o.host.obj.tags)
  THEN
    m:=pc.mods[o.mno].type;
  ELSE
    m:=NIL;
  END;
  j:=search(sys_tree,o.name^);
  IF (j # NIL) & (no = -1) & ~Check_X2C(o.name) THEN
     no := 0;
  END;
  IF (o.name=NIL) OR (no=-2) THEN COPY(suffix,nm);
  ELSE fmt.prn_txt(nm,"%s%s",o.name^,suffix);
  END;
  IF ~(o.flag IN pc.LangSet{pc.flag_o2, pc.flag_m2})
   & env.config.Option("NOHEADER") THEN   -- non-M2/O2 objects with NOHEADER+
    no := -1;                             -- force no renaming
  END;
  i:=make_name(res,o.host,m,o.mode,nm,no);
  IF o.mode # pc.ob_module THEN 
    j:=search(sys_tree,i.name^);
    IF j # NIL THEN
      env.errors.Warning(o.pos, 354, o.name^);
    END;
  END;
  i.obj:=o;
  IF o.ext#NIL THEN i.nxt:=o.ext(INFO) END;
  o.ext:=i;
  RETURN i;
END make_info;

PROCEDURE (i: INFO) out*(f: xfs.SymFile);
  VAR j: INFO; n: LONGINT;
BEGIN
  IF i.obj.lev>0 THEN
    (* for field path info .obj = NIL !!!!! *)
    f.WriteInt(0);
  ELSE
    j:=i; n:=1;
    WHILE j.nxt#NIL DO j:=j.nxt; INC(n) END;
    f.WriteInt(n);
    j:=i;
    WHILE j#NIL DO
      f.WriteString(j.name^); f.WriteInt(j.no); j:=j.nxt;
    END;
  END;
END out;

PROCEDURE (t: TREE) out*(f: xfs.SymFile);
BEGIN
  (* nothing *)
END out;

PROCEDURE skip_object*(f: xfs.SymFile);
  VAR s: pc.NAME; i,j: LONGINT;
BEGIN
  f.ReadInt(i);
  WHILE i>0 DO
    f.ReadString(s); f.ReadInt(j); DEC(i);
  END;
END skip_object;

PROCEDURE inp_object*(f: xfs.SymFile; o: pc.OBJECT);
  VAR i: INFO; n,l: LONGINT; bf: ARRAY 256 OF CHAR;
BEGIN
  f.ReadInt(n);
  WHILE n>0 DO
    NEW(i);
    f.ReadString(bf);
    NEW(i.name,LENGTH(bf)+1);
    COPY(bf,i.name^);
    f.ReadInt(l);
    i.no:=SHORT(l);
    i.obj:=o;
    i.nxt:=sym_list;
    sym_list:=i;
    DEC(n);
  END;
END inp_object;

PROCEDURE skip_struct*(f: xfs.SymFile);
BEGIN
  (* nothing *)
END skip_struct;

PROCEDURE inp_struct*(f: xfs.SymFile; s: pc.STRUCT);
BEGIN
  (* nothing *)
END inp_struct;

PROCEDURE do_sym_list*;
  VAR
    j,i : INFO;
    n   : NAME;
    tree: TREE;
BEGIN
  WHILE sym_list#NIL DO
    i:=sym_list; sym_list:=i.nxt;
    IF i.obj.ext=NIL THEN i.nxt:=NIL ELSE i.nxt:=i.obj.ext(INFO) END;
    i.obj.ext:=i;
    tree:=get_tree(i.obj.host,i.obj.mode);
    n:=insert(tree,i);
    IF n#NIL THEN
      IF n IS INFO THEN j:=n(INFO) ELSE j:=NIL END;
      IF (i.obj.mode IN pc.FIELDs) & (i.no>=0) OR
	 (i.obj.mode=pc.ob_eproc) OR
	 (j#NIL) & (j.obj#NIL) & (j.obj.mode IN pc.OB_SET{pc.ob_eproc,pc.ob_module}) OR
         (i.obj.flag IN cc.c_like_flags) OR
         (j#NIL) & (j.obj#NIL) & (j.obj.flag IN cc.c_like_flags)
      THEN
	(* nothing *)
      ELSIF (j#NIL) & (j.obj#NIL) THEN
	env.errors.Error(i.obj.pos,1003,
	  pc.mods[i.obj.mno].name^,i.obj.name^,
	  pc.mods[j.obj.mno].name^,j.obj.name^);
      ELSE
	env.errors.Error(i.obj.pos,1004,
	  pc.mods[i.obj.mno].name^,i.obj.name^);
      END;
    END;
  END;
END do_sym_list;

PROCEDURE x2c*(n: INTEGER; VAR nm: ARRAY OF CHAR);
BEGIN
  COPY(names[n]^,nm);
END x2c;

PROCEDURE sproc_name*(ps: pc.TPOS; p: pc.SUB_MODE; VAR nm: ARRAY OF CHAR);
BEGIN
  CASE p OF
    |pc.sp_halt    : x2c(nm_halt,nm);
    |pc.sp_abort   : x2c(nm_abort,nm);
    |pc.sp_move    : x2c(nm_move,nm);
    |pc.sp_fill    : x2c(nm_fill,nm);
    |pc.sp_copy    : x2c(nm_copy,nm);
  ELSE
    env.errors.Error(ps,1005);
    nm[0]:=0X;
  END;
END sproc_name;

PROCEDURE get_proclass*(flag: pc.Lang; VAR nm: ARRAY OF CHAR);
BEGIN
  IF flag IN cc.default_proclass THEN COPY("X2C_PROCLASS ",nm)
  ELSIF flag IN cc.special_proclass THEN
    IF    flag = pc.flag_p       THEN COPY("X2C_PASCAL ",nm)
    ELSIF flag = pc.flag_stdcall THEN COPY("X2C_STDCALL ",nm)
    ELSIF flag = pc.flag_syscall THEN COPY("X2C_SYSCALL ",nm)
    ELSE ASSERT(FALSE)
    END;
  ELSE nm[0]:=0X;
  END;
END get_proclass;

PROCEDURE cplx_func*(t: pc.STRUCT; sb: pc.SUB_MODE; VAR nm: ARRAY OF CHAR);
BEGIN
  IF t.mode=pc.ty_complex THEN
    CASE sb OF
      |pc.sb_equ,pc.sb_neq: x2c(nm_cplx_cmp,nm);
      |pc.sb_plus : x2c(nm_cplx_add,nm);
      |pc.sb_minus: x2c(nm_cplx_sub,nm);
      |pc.sb_mul  : x2c(nm_cplx_mul,nm);
      |pc.sb_slash: x2c(nm_cplx_div,nm);
    END
  ELSE
    CASE sb OF
      |pc.sb_equ,pc.sb_neq: x2c(nm_cplx_lcmp,nm);
      |pc.sb_plus : x2c(nm_cplx_ladd,nm);
      |pc.sb_minus: x2c(nm_cplx_lsub,nm);
      |pc.sb_mul  : x2c(nm_cplx_lmul,nm);
      |pc.sb_slash: x2c(nm_cplx_ldiv,nm);
    END;
  END;
END cplx_func;

PROCEDURE i_chk*(arr: pc.STRUCT; VAR nm: ARRAY OF CHAR);
  VAR n: INTEGER; t: pc.STRUCT;
BEGIN
  ASSERT(arr.mode IN pc.TY_SET{pc.ty_array,pc.ty_array_of,pc.ty_SS,pc.ty_set});
  IF arr.mode=pc.ty_array_of THEN
    t:=arr.inx.super_type();
    IF t.mode IN pc.TY_SET{pc.ty_longint,pc.ty_longcard} THEN n:=nm_i_chkl;
    ELSE n:=nm_i_chk;
    END;
  ELSIF arr.len>=10000H THEN
    n:=nm_i_chkl;
  ELSE
    n:=nm_i_chk;
  END;
  x2c(n,nm);
END i_chk;

PROCEDURE r_chk*(t: pc.STRUCT; VAR nm: ARRAY OF CHAR);
BEGIN
  IF t.mode#pc.ty_set THEN t:=t.super_type() END; (* !!!! см. to.do *)
  CASE t.mode OF
    |pc.ty_shortint,pc.ty_integer  : x2c(nm_r_chk,nm);
    |pc.ty_longint                 : x2c(nm_r_chkl,nm);
    |pc.ty_shortcard,pc.ty_cardinal: x2c(nm_r_chku,nm);
    |pc.ty_longcard,pc.ty_set      : x2c(nm_r_chkul,nm);
  END;
END r_chk;

PROCEDURE inc_dec*(m: pc.SUB_MODE; t: pc.STRUCT; VAR s: ARRAY OF CHAR);
  VAR md: pc.TY_MODE;
BEGIN
  WHILE t.mode=pc.ty_range DO t:=t.base END;
  IF t.mode IN pc.TY_SET{pc.ty_char,pc.ty_boolean,pc.ty_loc} THEN
    md:=pc.ty_char;
  ELSE
    t:=t.super_type(); md:=t.mode;
  END;
  IF m=pc.sb_pre_inc THEN
    CASE md OF
      |pc.ty_char     : COPY("X2C_INCC",s);
      |pc.ty_shortint : COPY("X2C_INCS",s);
      |pc.ty_integer  : COPY("X2C_INCI",s);
      |pc.ty_longint  : COPY("X2C_INC",s);
      |pc.ty_shortcard: COPY("X2C_INCUS",s);
      |pc.ty_cardinal : COPY("X2C_INCUI",s);
      |pc.ty_longcard : COPY("X2C_INCU",s);
    END;
  ELSE
    CASE md OF
      |pc.ty_char     : COPY("X2C_DECC",s);
      |pc.ty_shortint : COPY("X2C_DECS",s);
      |pc.ty_integer  : COPY("X2C_DECI",s);
      |pc.ty_longint  : COPY("X2C_DEC",s);
      |pc.ty_shortcard: COPY("X2C_DECUS",s);
      |pc.ty_cardinal : COPY("X2C_DECUI",s);
      |pc.ty_longcard : COPY("X2C_DECU",s);
    END;
  END;
END inc_dec;

PROCEDURE s_chk*(t: pc.STRUCT; VAR s: ARRAY OF CHAR);
BEGIN
  t:=t.super_type();
  CASE t.mode OF
    |pc.ty_shortint,pc.ty_integer: COPY("X2C_CHKS",s);
    |pc.ty_longint               : COPY("X2C_CHKSL",s);
  END;
END s_chk;

PROCEDURE u_abs*(t: pc.STRUCT; chk: BOOLEAN; VAR s: ARRAY OF CHAR);
BEGIN
  IF NOT (t.mode IN pc.REALs) THEN t:=t.super_type() END;
  IF t.mode IN pc.REALs THEN
    CASE t.mode OF
      |pc.ty_real     : fmt.prn_txt(s,"(%s)fabs",names[nm_real]^);
      |pc.ty_longreal : COPY("fabs",s);
      |pc.ty_RR       : COPY("fabs",s);
      |pc.ty_ld_real  : COPY("fabsl",s);
    END;
  ELSIF chk THEN
    CASE t.mode OF
      |pc.ty_shortint : COPY("X2C_ABS_INT8",s);
      |pc.ty_integer  : COPY("X2C_ABS_INT16",s);
      |pc.ty_longint  : COPY("X2C_ABS_INT32",s);
      |pc.ty_longlongint : COPY("X2C_ABS_INT64",s);
      |pc.ty_ZZ       : COPY("X2C_ABS_INT32",s);
    END;
  ELSE
    CASE t.mode OF
      |pc.ty_shortint,pc.ty_integer  : COPY("abs",s);
      |pc.ty_longint, pc.ty_ZZ       : COPY("labs",s);
    END;
  END;
END u_abs;

PROCEDURE key(s-,t-: ARRAY OF CHAR);
  VAR i: NAME; j: RENM;
BEGIN
  IF t="" THEN
    NEW(i);
    NEW(i.name,LENGTH(s)+1);
    COPY(s,i.name^);
    i:=insert(sys_tree,i);
  ELSE
    NEW(j);
    NEW(j.name,LENGTH(s)+1);
    NEW(j.renm,LENGTH(t)+1);
    COPY(s,j.name^);
    COPY(t,j.renm^);
    i:=insert(sys_tree,j);
  END;
END key;

PROCEDURE read_keywords;
  VAR
    fnm    : env.String;
    f      : xfs.TextFile;
    s,nm,to: ARRAY 256 OF CHAR;
    i,j    : INTEGER;
BEGIN
  xfs.sys.SysLookup("kwd",fnm);
  xfs.text.Open(fnm^,FALSE);
  IF xfs.text.file=NIL THEN
    env.errors.Fault(env.null_pos,425,xfs.text.msg^);
  END;
  f:=xfs.text.file(xfs.TextFile);
  LOOP
    f.ReadString(s);
    IF f.readRes = xfs.allRight THEN
      i:=0;
      LOOP
	IF s[i]=0X THEN EXIT END;
	WHILE (s[i]>0X) & (s[i]<=' ') DO INC(i) END;
	IF s[i]='%' THEN EXIT END;
	j:=0;
	WHILE s[i]>' ' DO nm[j]:=s[i]; INC(i); INC(j) END;
	nm[j]:=0X;
	IF j>0 THEN
          to:="";
          IF s[i]='=' THEN
            INC(i); j:=0;
	    WHILE s[i]>' ' DO to[j]:=s[i]; INC(i); INC(j) END;
	    to[j]:=0X;
          END;
          key(nm,to);
        END;
      END;
    ELSIF f.readRes = xfs.endOfInput THEN
      EXIT
    END;
  END;
  f.Close;
END read_keywords;

PROCEDURE std(n: INTEGER; s-: ARRAY OF CHAR);
  VAR i: NAME;
BEGIN
  ASSERT(names[n]=NIL);
  NEW(i);
  NEW(i.name,LENGTH(s)+1);
  COPY(s,i.name^);
  names[n]:=i.name;
--  i:=insert(sys_tree,i);
END std;

PROCEDURE ini_sys_tree;
BEGIN
  IF sys_tree#NIL THEN RETURN END;
  new_tree(sys_tree);
  NEW(sys_tree.index);
  read_keywords;

  (* standard names *)
  IF env.config.Option("GENCTYPES") THEN
    std(nm_shortcard,"unsigned char");
    std(nm_shortint,"signed char");
    std(nm_cardinal,"unsigned short");
    std(nm_longcard,"unsigned long");
    std(nm_integer,"short");
    std(nm_longint,"long");
    std(nm_real,"float");
    std(nm_longreal,"double");
    std(nm_ld_real,"long double");
    std(nm_boolean,"char");
    std(nm_char,"char");
    std(nm_AA,"char *");
    std(nm_loc,"char");
    std(nm_protection,"short");
    std(nm_void,"void");
    std(nm_index,"size_t");
    key("short",  "");
    key("long",   "");
    key("float",  "");
    key("double", "");
    key("char",   "");
    key("void",   "");
    key("size_t", "");
  ELSE
    std(nm_shortcard,"X2C_CARD8");
    std(nm_cardinal,"X2C_CARD16");
    std(nm_longcard,"X2C_CARD32");
    std(nm_shortint,"X2C_INT8");
    std(nm_integer,"X2C_INT16");
    std(nm_longint,"X2C_INT32");
    std(nm_real,"X2C_REAL");
    std(nm_longreal,"X2C_LONGREAL");
    std(nm_ld_real,"X2C_LONGDOUBLE");
    std(nm_boolean,"X2C_BOOLEAN");
    std(nm_char,"X2C_CHAR");
    std(nm_AA,"X2C_ADDRESS");
    std(nm_loc,"X2C_LOC");
    std(nm_protection,"X2C_PROTECTION");
    std(nm_void,"void");
    std(nm_index,"X2C_INDEX");
    key("void", "");
  END;

  std(nm_complex,"X2C_COMPLEX");
  std(nm_lcomplex,"X2C_LONGCOMPLEX");
  std(nm_a_chk,"X2C_CHKNIL");
  std(nm_p_chk,"X2C_CHKPROC");
  std(nm_dynarr_addr,"Adr");
  std(nm_dynarr_len,"Len");
  std(nm_dynarr_size,"Size");
  std(nm_memcmp,"memcmp");
  key("memcmp", "");
  std(nm_memcpy,"memcpy");
  key("memcpy", "");
  std(nm_strcmp,"X2C_STRCMP");
  std(nm_strncpy,"strncpy");
  key("strncpy", "");
  std(nm_quo,"X2C_QUO");
  std(nm_div,"X2C_DIV");
  std(nm_rem,"X2C_REM");
  std(nm_mod,"X2C_MOD");
  std(nm_cap,"X2C_CAP");
  std(nm_in,"X2C_IN");
  std(nm_inl,"X2C_INL");
  std(nm_alloc_param,"X2C_PCOPY");
  std(nm_free_param,"X2C_PFREE");
  std(nm_rts_body,"X2C_BEGIN");
  std(nm_length,"X2C_LENGTH");
  std(nm_trap,"X2C_TRAP");
  std(nm_assert,"X2C_ASSERT");
  std(nm_case_trap,"X2C_CASE_TRAP");
  std(nm_return_trap,"X2C_RETURN_TRAP");
  std(nm_set_range,"X2C_SET");
  std(nm_seq_type,"X2C_SEQ");
  std(nm_rot_set,"X2C_ROT");
  std(nm_rot_lset,"X2C_ROTL");
  std(nm_lsh_set,"X2C_LSH");
  std(nm_lsh_lset,"X2C_LSHL");
  std(nm_ash,"X2C_ASH");
  std(nm_trunci,"X2C_TRUNCI");
  std(nm_truncc,"X2C_TRUNCC");
  std(nm_cplx_cmp,"CPLX_CMP");
  std(nm_cplx_add,"CPLX_ADD");
  std(nm_cplx_sub,"CPLX_SUB");
  std(nm_cplx_mul,"CPLX_MUL");
  std(nm_cplx_div,"CPLX_DIV");
  std(nm_cplx_lcmp,"CPLX_LCMP");
  std(nm_cplx_ladd,"CPLX_LADD");
  std(nm_cplx_lsub,"CPLX_LSUB");
  std(nm_cplx_lmul,"CPLX_LMUL");
  std(nm_cplx_ldiv,"CPLX_LDIV");
  std(nm_i_chk,"X2C_CHKINX");
  std(nm_i_chkl,"X2C_CHKINXL");
  std(nm_r_chk,"X2C_CHK");
  std(nm_r_chkl,"X2C_CHKL");
  std(nm_r_chku,"X2C_CHKU");
  std(nm_r_chkul,"X2C_CHKUL");
  std(nm_halt,"X2C_HALT");
  std(nm_abort,"X2C_ABORT");
  std(nm_move,"X2C_MOVE");
  std(nm_fill,"memset");
  key("memset", "");
  std(nm_copy,"X2C_COPY");
  std(nm_allocate,"X2C_ALLOCATE");
  std(nm_deallocate,"X2C_DEALLOCATE");
  std(nm_dynallocate,"X2C_DYNALLOCATE");
  std(nm_dyndeallocate,"X2C_DYNDEALLOCATE");
  std(nm_dyncallocate,"X2C_DYNCALLOCATE");
  std(nm_dyncdeallocate,"X2C_DYNCDEALLOCATE");
  std(nm_new_open,"X2C_NEW_OPEN");
  std(nm_new,"X2C_NEW");
  std(nm_dispose,"X2C_DISPOSE");
  std(nm_cplx_neg,"CPLX_NEG");
  std(nm_cplx_lneg,"CPLX_LNEG");
  std(nm_field_ofs,"X2C_FIELD_OFS");
  std(nm_quo64,"X2C_QUO64");
  std(nm_div64,"X2C_DIV64");
  std(nm_rem64,"X2C_REM64");
  std(nm_mod64,"X2C_MOD64");
  std(nm_divr,"X2C_DIVR");
  std(nm_divl,"X2C_DIVL");
END ini_sys_tree;

PROCEDURE cnt_en*(en: BOOLEAN);
BEGIN
  name_cnt_en:=en;
END cnt_en;

PROCEDURE ini*;
  VAR str: pc.STRING;
BEGIN
  sym_list:=NIL;
  new_tree(glo_tree);
  NEW(glo_tree.index);
  name_cnt:=0;
  name_cnt_en:=FALSE;
  ini_sys_tree;
  env.config.Equation("GENIDLEN",str);
  IF (str=NIL) OR NOT xcStr.StrToInt(str^,ident_len) THEN ident_len:=30;
  ELSIF ident_len<6 THEN ident_len:=6;
  ELSIF ident_len>=pc.name_size THEN ident_len:=pc.name_size-1;
  END;
END ini;

PROCEDURE exi*;
BEGIN
  glo_tree:=NIL;
  sym_list:=NIL;
END exi;

VAR i: INTEGER; c: CHAR;

BEGIN
  FOR i:=0 TO 0FFH DO
    c:=CHR(i);
    ctype[i]:=(c>='0') & (c<='9') OR
	      (c>='A') & (c<='Z') OR
	      (c>='a') & (c<='z') OR
	      (c='_');
    capital[i]:=CAP(c);
  END;
  ident_len:=8;
  sys_tree:=NIL;
END ccN.
