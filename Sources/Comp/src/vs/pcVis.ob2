(** Copyright (c) 1991,97 XDS Ltd, Russia. All Rights Reserved. *)
MODULE pcVis; (* Ned 04-Apr-91. (c) KRONOS *)
	      (* Sem 29-Sep-93. (c) xTech  *)

IMPORT
  SYSTEM,
  pcK,
  pcO,
  pcNum,
  env:=xiEnv,
  xfs:=xiFiles;

PROCEDURE print*(fmt: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN
  env.info.print(fmt,x);
END print;

PROCEDURE show_pos*(ps: env.TPOS);
  VAR fnm: env.String; l,col: LONGINT;
BEGIN
  IF ps.IsNull() THEN
    env.info.print("(null pos) ");
  ELSE
    ps.unpack(fnm,l,col);
    env.info.print("(%d,%d,%s) ",l,col,fnm^);
  END;
END show_pos;

PROCEDURE ss(s: ARRAY OF CHAR);
BEGIN
  print("%s",s);
END ss;

PROCEDURE wi(name: ARRAY OF CHAR; n: LONGINT);
BEGIN
  print("%s=%d ",name,n);
END wi;

PROCEDURE wl;
BEGIN
  print("\n");
END wl;

PROCEDURE type_mode(m: pcK.TY_MODE);
BEGIN
  CASE m OF
    |pcK.ty_shortint : ss("INT8 ");
    |pcK.ty_shortcard: ss("CARD8 ");
    |pcK.ty_integer  : ss("INT16 ");
    |pcK.ty_cardinal : ss("CARD16 ");
    |pcK.ty_longint  : ss("INT32 ");
    |pcK.ty_longcard : ss("CARD32 ");
    |pcK.ty_real     : ss("REAL ");
    |pcK.ty_longreal : ss("LREAL ");
    |pcK.ty_boolean  : ss("BOOLEAN ");
    |pcK.ty_char     : ss("CHAR ");
    |pcK.ty_loc      : ss("LOC ");
    |pcK.ty_range    : ss("RANGE ");
    |pcK.ty_enum     : ss("ENUM ");
    |pcK.ty_opaque   : ss("OPAQUE ");
    |pcK.ty_pointer  : ss("PTR ");
    |pcK.ty_set      : ss("SET ");
    |pcK.ty_proctype : ss("PROCTYPE ");
    |pcK.ty_array    : ss("ARRAY ");
    |pcK.ty_array_of : ss("ARR_OF ");
    |pcK.ty_record   : ss("RECORD ");
    |pcK.ty_module   : ss("MODULE ");
    |pcK.ty_AA	     : ss("AA-type ");
    |pcK.ty_SS	     : ss("SS-type ");
    |pcK.ty_ZZ       : ss("ZZ-type ");
    |pcK.ty_CC	     : ss("CC-type ");
    |pcK.ty_RR 	     : ss("RR-type ");
    |pcK.ty_void     : ss("VOID ");
  ELSE
    print("TMODE%d ",m);
  END;
END type_mode;

PROCEDURE type*(t: pcK.STRUCT);
BEGIN
  IF t=NIL THEN ss('type=NIL ');
  ELSE
    type_mode(t^.mode);
    CASE t^.mode OF
      |pcK.ty_range : type_mode(t^.base^.mode);
      |pcK.ty_enum  :
      |pcK.ty_record:
      |pcK.ty_array_of,pcK.ty_array,pcK.ty_SS: wi("len",t^.len);
    ELSE
    END;
    IF t^.obj#NIL THEN ss('"'); ss(t^.obj^.name^); ss('"') END;
    print(" flag=%d",t^.flag);
    print(" mno=%d",t^.mno);
    print(" tags=%{}",t^.tags);
    IF t^.align # 0 THEN 
      print(" align=%d",t^.align) 
    END;
  END;
END type;

PROCEDURE ^ obj*(o: pcK.OBJECT; lev: INTEGER);

PROCEDURE struct*(t: pcK.STRUCT; lev: INTEGER);
  PROCEDURE sobj(o: pcK.OBJECT; lev: INTEGER);
    VAR p: pcK.OBJECT; n: pcK.NODE;
  BEGIN
    obj(o,lev);
    IF (o.mode=pcK.ob_type) &
       (o.type.obj=o) &
       ((o.type.prof#NIL) OR (o.type.mem#NIL))
    THEN
      struct(o.type,lev+1);
    ELSIF o.mode=pcK.ob_header THEN
      IF o.val.obj#NIL THEN
        print("%.*cTAG",lev*2+2,' '); obj(o.val.obj,0);
      END;
      n:=o.val.l;
      WHILE n#NIL DO
        ASSERT(n.mode=pcK.nd_node);
        p:=n.obj;
        WHILE p#NIL DO
          sobj(p,lev+1);
          p:=p.next;
        END;
        print("%.*c-----\n",lev*2+2,' ');
(*
	m:=n.l;
	  WHILE m#NIL DO
	    IF m.l#NIL THEN
	      out_val(m.l.val,sym_max);
	      out_val(m.val,-1);
	    ELSE
	      out_val(m.val,sym_min);
	    END;
	    m:=m.next;
	  END;
*)
        n:=n.next;
      END;
    END;
  END sobj;
  VAR o: pcK.OBJECT;
BEGIN
  print('%.*cT: ',lev*2,' ');
  type(t);
  wl;
  IF t#NIL THEN
    print("%.*cpublic:\n",lev*2,' ');
    o:=t^.prof;
    WHILE o#NIL DO
      sobj(o,lev+1);
      o:=o^.next;
    END;
    print("%.*cprivate:\n",lev*2,' ');
    o:=t^.mem;
    WHILE o#NIL DO
      sobj(o,lev+1);
      o:=o^.next;
    END;
    print("%.*cend.\n",lev*2,' ');
  END;
END struct;

PROCEDURE adr(o: pcK.OBJECT);
BEGIN
  ss('[');
  wi('mno',ORD(o^.mno));
  wi(',lev',o^.lev);
  ss('] ');
END adr;

PROCEDURE tags(o: pcK.OBJECT);
  VAR i: LONGINT;
BEGIN
  IF o^.tags#pcK.OTAG_SET{} THEN
    ss('{');
    FOR i:=0 TO 31 DO
      IF VAL(pcK.OTAG, i) IN o^.tags THEN print("%d",i); ss(' ') END;
    END;
    ss('} ');
  END;
END tags;

PROCEDURE obj_mode(m: pcK.OB_MODE);
BEGIN
  CASE m OF
    |pcK.ob_cons   : ss('CONS ');
    |pcK.ob_var    : ss('VAR ');
    |pcK.ob_varpar : ss('VPAR ');
    |pcK.ob_seq    : ss('SEQ ');
    |pcK.ob_field  : ss('FIELD ');
    |pcK.ob_proc   : ss('PROC ');
    |pcK.ob_xproc  : ss('XPROC ');
    |pcK.ob_eproc  : ss('EPROC ');
    |pcK.ob_cproc  : ss('CPROC ');
    |pcK.ob_type   : ss('TYPE ');
    |pcK.ob_module : ss('MODULE ');
    |pcK.ob_header : ss('HEADER ');
  ELSE wi('mode',ORD(m));
  END;
END obj_mode;

PROCEDURE obj*(o: pcK.OBJECT; lev: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=1 TO lev DO ss('  ') END;
  ss('O: ');
  obj_mode(o^.mode);
  print('"%s" mno=%d, lev=%d, tags=%{}, TYPE: ',
  	o^.name^,o^.mno,o^.lev,o^.tags);
  type(o^.type);
  print("\n");
END obj;

PROCEDURE ^ vis*(n: pcK.NODE; lev: INTEGER);

CONST
  ORD_VAL =pcK.WHOLEs+pcK.TY_SET{pcK.ty_range,pcK.ty_enum,pcK.ty_boolean,pcK.ty_char};


PROCEDURE o_list(o: pcK.OBJECT; lev: INTEGER);
BEGIN
  WHILE o#NIL DO
    obj(o,lev);
    IF (o^.mode=pcK.ob_type) & (o^.type^.mode=pcK.ty_enum) THEN
      o_list(o^.type^.prof,lev+1);
    ELSIF o^.mode IN pcK.PROCs THEN
      o_list(o^.type^.mem,lev+1);
      vis(o^.val,lev+1);
    END;
    o:=o^.next;
  END;
END o_list;

PROCEDURE node(n: pcK.NODE; lev: INTEGER);

  PROCEDURE ntype;
  BEGIN
    IF n^.type=NIL THEN ss('type=NIL ');
    ELSE
      ss('T: '); type_mode(n^.type^.mode);
      IF n^.type^.base#NIL THEN type_mode(n^.type^.base^.mode) END;
      IF n^.type^.obj#NIL THEN
	ss('O: "'); ss(n^.type^.obj^.name^); ss('" ')
      END;
    END;
  END ntype;

  PROCEDURE nobj;
  BEGIN
    IF n^.obj=NIL THEN ss('obj=NIL ');
    ELSE
      ss('O: '); obj_mode(n^.obj^.mode); adr(n^.obj); tags(n^.obj);
      ss('"'); ss(n^.obj^.name^); ss('" ');
    END;
  END nobj;

  PROCEDURE sub_u;
  BEGIN
    CASE n^.sub OF
      |pcK.su_none : (* nothing *)
      |pcK.su_cast : ss('cast  ')
      |pcK.su_bits : ss('bits  ')
      |pcK.su_bytes: ss('bytes ')
      |pcK.su_size : ss('size  ')
      |pcK.su_length: ss('length ')
      |pcK.su_abs  : ss('abs   ')
      |pcK.su_adr  : ss('adr   ')
      |pcK.su_cc   : ss('cc    ')
      |pcK.su_cap  : ss('cap   ')
      |pcK.su_conv : ss('conv  ')
      |pcK.su_entier: ss('entier ')
      |pcK.su_odd  : ss('odd   ')
      |pcK.su_not  : ss('not   ')
      |pcK.su_is   : ss('is    '); nobj; ss("  ");
      |pcK.su_neg  : ss('neg ')
    ELSE wi('sub',ORD(n^.sub));
    END;
  END sub_u;

  PROCEDURE sub_b;
  BEGIN
    CASE n^.sub OF
      |pcK.su_none : (* nothing *)
      |pcK.sb_ash  : ss('ash   ')
      |pcK.sb_lsh  : ss('lsh   ')
      |pcK.sb_rot  : ss('rot   ')
      |pcK.sb_high : ss('high  ')
      |pcK.sb_len  : ss('len   ')
      |pcK.sb_equ  : ss('equ   ')
      |pcK.sb_neq  : ss('neq   ')
      |pcK.sb_lss  : ss('lss   ')
      |pcK.sb_leq  : ss('leq   ')
      |pcK.sb_gtr  : ss('gtr   ')
      |pcK.sb_geq  : ss('geq   ')
      |pcK.sb_in   : ss('in    ')
      |pcK.sb_mul  : ss('mul   ')
      |pcK.sb_div  : ss('div   ')
      |pcK.sb_slash: ss('slash ')
      |pcK.sb_mod  : ss('mod   ')
      |pcK.sb_rem  : ss('rem   ')
      |pcK.sb_plus : ss('plus  ')
      |pcK.sb_minus: ss('minus ')
      |pcK.sb_and  : ss('and   ')
      |pcK.sb_or   : ss('or    ')
      |pcK.sb_xor  : ss('xor   ')
      |pcK.sb_bic  : ss('bic   ')
      |pcK.sb_cand : ss('cand  ')
      |pcK.sb_cor  : ss('cor   ')
      |pcK.sb_addadr : ss('addadr ');
      |pcK.sb_subadr : ss('subadr ');
      |pcK.sb_difadr : ss('difadr ');
      |pcK.sb_bit  : ss('bit   ')
    ELSE wi('sub',ORD(n^.sub));
    END
  END sub_b;

  PROCEDURE ntag;
  BEGIN
    ss('{');
    IF pcK.ntag_chk_overflow IN n.tags THEN ss('chk_ovf') END;
    IF pcK.ntag_chk_range    IN n.tags THEN ss(' chk_range') END;
    IF pcK.ntag_no_exit      IN n.tags THEN ss(' no_exit') END;
    IF pcK.ntag_hex          IN n.tags THEN ss(' hex') END;
    ss('} ');
  END ntag;

  PROCEDURE value(val: pcK.NODE);
    VAR f: pcK.TY_MODE; buf: ARRAY 32 OF CHAR;
  BEGIN
    IF (val=NIL) OR (val^.val=NIL) OR (val^.type=NIL) THEN
      ss('value NIL'); RETURN
    END;
    f:=val^.type^.mode;
    IF f IN ORD_VAL THEN
      val.val.value_to_str(buf,pcK.flag_o2);
      print("val=%s ",buf);
(*      wi('val',val^.val^.get_integer())*)
    ELSE
      ss('value ???');
    END;
  END value;

  VAR i: INTEGER; l,c: LONGINT; fnm: pcK.STRING;

BEGIN
  FOR i:=1 TO lev DO ss('  ') END;
  CASE n^.mode OF
    |pcK.nd_inv      : ss('*INV*  ');  nobj;  ntag;
    |pcK.nd_var      : ss('VAR    ');  ntype; ntag; nobj
    |pcK.nd_field    : ss('FIELD  ');  ntype; ntag; nobj
    |pcK.nd_method   : ss('METHOD ');
		       IF n^.sub#pcK.su_none THEN ss('super ') END;
                       type(n^.type); ntag; nobj
    |pcK.nd_proc     : ss('PROC   ');  ntype; ntag; nobj
    |pcK.nd_type     : ss('TYPE   ');  ntype; ntag; nobj
    |pcK.nd_module   : ss('MODULE ');  ntype; ntag; nobj; wl;
		       o_list(n^.type^.mem,lev+1);
    |pcK.nd_index    : ss('INDEX  ');  ntype; ntag;
    |pcK.nd_binary   : ss('BINARY ');  sub_b; ntype; ntag;
    |pcK.nd_unary    : ss('UNARY  ');  sub_u; ntype; ntag;
    |pcK.nd_lconv    : ss('LCONV  ');  ntype;        ntag;
    |pcK.nd_deref    : ss('DEREF  ');  ntype;  ntag;
    |pcK.nd_guard    : ss('GUARD  ');  ntype;  ntag; nobj;
    |pcK.nd_eguard   : ss('EGUARD ');  ntype; ntag; nobj;
    |pcK.nd_value    : ss('VALUE  ');  ntype; ntag; value(n);
    |pcK.nd_aggregate: ss('AGGREG ');  ntype; ntag;
    |pcK.nd_sequence : ss('SEQU   ');  ntype; ntag;
    |pcK.nd_pair     : ss('PAIR   ');  ntag; value(n); value(n^.l);
    |pcK.nd_node     : ss('NODE   ');  ntag;
    |pcK.nd_while    : ss('WHILE  ');  ntag;
    |pcK.nd_repeat   : ss('REPEAT ');  ntag;
    |pcK.nd_loop     : ss('LOOP   ');  ntag;
    |pcK.nd_exit     : ss('EXIT   ');  ntag;
    |pcK.nd_return   : ss('RETURN ');  ntype; ntag;
    |pcK.nd_goto     : ss('GOTO   ');  ntag;
    |pcK.nd_for      : ss('FOR    ');  sub_u; ntag; nobj
    |pcK.nd_with     : ss('WITH   ');  ntag; nobj
    |pcK.nd_wtrap    : ss('WTRAP  ');  ntag;
    |pcK.nd_ftrap    : ss('FTRAP  ');  ntag;
    |pcK.nd_if       : ss('IF     ');  ntag;
    |pcK.nd_case     : ss('CASE   ');  ntag;
    |pcK.nd_caselse  : ss('CASELS ');  ntag;
    |pcK.nd_casedo   : ss('CASEDO ');  ntag;
    |pcK.nd_assign   : ss(':=     ');  ntag; nobj;
    |pcK.nd_call     : ss('CALL   ');  ntype; ntag;
		       IF n^.sub#pcK.su_none THEN wi("sub",ORD(n^.sub)) END;
		       nobj;
    |pcK.nd_eval     : ss('EVAL ');    ntag;
    |pcK.nd_finally  : ss('FINALLY '); ntag; nobj;
    |pcK.nd_block    : ss('BLOCK ');   ntag;
    |pcK.nd_except   : ss('EXCEPT ');  ntag; nobj;
    |pcK.nd_protect  : ss('PROTECT '); ntag; value(n);
    |pcK.nd_replace  : ss('REPLACE '); ntag;
    |pcK.nd_sproc    :
      CASE n^.sub OF
	|pcK.sp_incl      : ss('INCL   ');
	|pcK.sp_excl      : ss('EXCL   ');
	|pcK.sp_new       : ss('NEW    ');   nobj
	|pcK.sp_sysnew    : ss('SYSNEW ');   nobj
	|pcK.sp_dispose   : ss('DISPOSE ');  nobj
	|pcK.sp_halt      : ss('HALT   ');
	|pcK.sp_copy      : ss('COPY   ');
	|pcK.sp_move      : ss('MOVE   ');
	|pcK.sp_code      : ss('CODE   ');
	|pcK.sp_assert    : ss('ASSERT ');
	|pcK.sp_get       : ss('GET ');
	|pcK.sp_put       : ss('PUT ');
	|pcK.sp_getreg    : ss('GETREG');
	|pcK.sp_putreg    : ss('PUTREG');
      ELSE print("sproc %d ",n.sub);
      END;
      ntag;
    |pcK.nd_null     : ss('NULL '); ntag;
    |pcK.nd_label    : ss('LABEL '); ntag; nobj;
  ELSE wi('mode', ORD(n^.mode)); ntype; ntag; nobj;
  END;
  n.pos.unpack(fnm,l,c);
  print("[%d.%d]\n",l+1,c+1);
END node;

PROCEDURE vis*(n: pcK.NODE; lev: INTEGER);

  PROCEDURE _vis(n: pcK.NODE; lev: INTEGER);
  BEGIN
    WHILE n#NIL DO
      node(n,lev);
      IF (n^.mode=pcK.nd_return) THEN
        _vis(n^.l,lev+1);
      ELSIF
        (n^.mode#pcK.nd_pair) &
        (n^.mode#pcK.nd_value) &
        (n^.mode#pcK.nd_exit) &
        (n^.mode#pcK.nd_goto) &
        (n^.mode#pcK.nd_activate) &
        (n^.mode#pcK.nd_reraise) &
        (n^.mode#pcK.nd_retry)
      THEN
	_vis(n^.l,lev+1);
	_vis(n^.r,lev+1)
      END;
      n:=n^.next;
    END;
  END _vis;

BEGIN
  _vis(n,lev);
END vis;

PROCEDURE rd_obj(): pcK.IROBJECT;
BEGIN
  ASSERT(FALSE);
END rd_obj;

PROCEDURE vis_sym_file*(file: xfs.SymFile);

  VAR 
    ref_no: INTEGER;
    sym_ident: LONGINT;

  PROCEDURE inp_byte(VAR x: LONGINT);
    VAR n: SYSTEM.CARD8;
  BEGIN
    file.Read(n);
    x:=VAL(LONGINT,n);
  END inp_byte;

  PROCEDURE inp_int(): LONGINT;
    VAR i: LONGINT;
  BEGIN
    file.ReadInt(i);
    RETURN i;
  END inp_int;

  PROCEDURE inp_ref;
    VAR i: LONGINT; t: pcK.STRUCT;
  BEGIN
    i:=inp_int();
    IF i=0 THEN print("NIL "); RETURN END;
    IF (i>0) & (i<pcO.ref_first) THEN
      t:=pcO.std_types[i];
      IF t=NIL THEN print("NIL ")
      ELSIF t^.obj#NIL THEN print('T:%d "%s" ',i,t^.obj^.name^);
      ELSE print("T:%d ",i); type_mode(t^.mode);
      END;
    ELSE
      print("T:%d ",i);
    END;
  END inp_ref;

  PROCEDURE inp_str();
    VAR s: pcK.NAME;
  BEGIN
    file.ReadString(s);
    print('"%s" ',s);
  END inp_str;

  PROCEDURE inp_value(m: pcK.TY_MODE);
    VAR v: pcK.VALUE; l: LONGINT;
  BEGIN
    v:=pcK.value^.inp(file,rd_obj);
    IF m IN ORD_VAL THEN
      l:=v^.get_integer();
      print("VALUE %d ",l);
    ELSE
      print("VALUE ");
    END;
  END inp_value;

  PROCEDURE inp_object;
    VAR i: LONGINT; pos: env.TPOS;
  BEGIN
    print("O: ");
    obj_mode(VAL(pcK.OB_MODE,inp_int()));
    print("flag %d ",inp_int());
    file.ReadTPOS(pos); show_pos(pos);
    print("type "); inp_ref;
    print("name "); inp_str;
(*
    print("host "); inp_ref;
    print("mno "); inp_ref;
*)
    print("tags %{} ",inp_int());
    print("scp "); inp_ref;
    LOOP
      inp_byte(i);
      CASE i OF
	|pcO.sym_val     : inp_value(pcO.ty_undef);
        |pcO.sym_info    : pcK.code^.skip_object(file,sym_ident);
	|pcO.sym_ref     : print("REF ");
        |pcO.sym_end     : print(" *END object*\n"); EXIT;
      END;
    END;
  END inp_object;

  PROCEDURE inp_list;
    VAR i: LONGINT;
  BEGIN
    LOOP
      inp_byte(i);
      CASE i OF
        |pcO.sym_end   : print("*END list*\n"); EXIT;
	|pcO.sym_object: inp_object;
	|pcO.sym_tag   : print("TAG\n");
	  inp_ref;
	  LOOP
	    inp_byte(i);
	    CASE i OF
	      |pcO.sym_object: inp_object;
	      |pcO.sym_case  : print("CASE\n");
			       inp_list;
	      |pcO.sym_end   : print("END\n");
			       EXIT;
              |pcO.sym_min   : print("LABEL ");
                               inp_value(pcK.ty_integer); wl;
              |pcO.sym_max   : print("PAIR ");
                               inp_value(pcK.ty_integer);
                               inp_value(pcK.ty_integer); wl;
            ELSE
              env.info.print('%d (%x)\n',i,i); ASSERT(FALSE);
	    END;
	  END;
      END;
    END;
  END inp_list;

  PROCEDURE inp_struct;
    VAR i: LONGINT; m: pcK.TY_MODE;
  BEGIN
    print("T:%d ",ref_no); INC(ref_no);
    m:=VAL(pcK.TY_MODE,inp_int());
    type_mode(m);
    print("[%d] ",inp_int());
    print("mno "); inp_ref;
    print("base "); inp_ref;
    print("inx "); inp_ref;
    print("len %d ",inp_int());
    print("num %d ",inp_int());
    print("tags {} ",inp_int());
    print("\n");
    LOOP
      inp_byte(i);
      CASE i OF
	|pcO.sym_min   : print("MIN "); inp_value(m); wl;
	|pcO.sym_max   : print("MAX "); inp_value(m); wl;
	|pcO.sym_prof  : print("PROF\n"); inp_list;
	|pcO.sym_mem   : print("MEM\n"); inp_list;
        |pcO.sym_info  : pcK.code^.skip_struct(file,sym_ident);
	|pcO.sym_ref   : print("REF "); inp_ref; wl;
	|pcO.sym_incomp: print("INCOMP"); wl;
        |pcO.sym_align : inp_byte(i); print("ALIGN=%d\n",i);
        |pcO.sym_end   : print(" *END struct*\n"); EXIT;
      END;
    END;
  END inp_struct;

  PROCEDURE inp_hook;
    VAR i: LONGINT;
  BEGIN
    print("HOOK ");
    i:=inp_int();
    WHILE i>0 DO inp_str; DEC(i) END;
    inp_ref;
    print("\n");
  END inp_hook;

  PROCEDURE inp_all;
    VAR i: LONGINT;
  BEGIN
    ref_no:=pcO.ref_first;
    LOOP
      inp_byte(i);
      CASE i OF
	|pcO.sym_hook  : inp_hook;
	|pcO.sym_type  : inp_struct;
	|pcO.sym_object: inp_object;
        |pcO.sym_module: inp_ref;
        |pcO.sym_end   : print("*END all*\n"); EXIT;
      ELSE
        env.info.print('%d (%x)\n',i,i); ASSERT(FALSE);
      END;
    END;
  END inp_all;

BEGIN
  print("TAG    %d\n",inp_int());
  print("VERS   %d\n",inp_int());
  sym_ident:=inp_int();
  print("TARGET %d\n",sym_ident);
  inp_all;
END vis_sym_file;

END pcVis.
