(** Copyright (c) 1993,97 XDS Ltd, Russia. All Rights Reserved. *)
(** M2/O2 front-end: Standard functions *)
MODULE pcF; (** Ned 23-Jan-93. *)
	    (** Sem 18-Oct-93. *)

(* Modifications:
   14-Mar-96 Ned  warn316 is reported instead of error if
                  SYSTEM.GET, SYSTEM.PUT, SYSTEM.MOVE are used in
                  M2 without extensions.
   16-Mar-96 Ned  INCL,EXCL set.base.super_type is not called for invtype.
   19-Mar-96 Ned  SYSTEM.REF is added.
   23-Mar-96 Ned  pcB.cast is used for fn_cast.
   15-Apr-96 Ned  constants of proctype with value NIL are invented.
                  Fix in "check_profile".
   24-Apr-96 Ned  BUG0101 - PUT(,literal).
   05-Aug-96 Ned  INCL,EXCL - pcB.chk_index is used instead of
                  expression_compatibility
   12-Aug-96 Ned  INCL,EXCL - expression_compatibility is used again,
                  (See ISO).
*)

IMPORT
   pc:=pcK,
   pcS,
   pcO,
   pcB,
   env:=xiEnv,
<* IF pcVis THEN *> pcVis, <* END *>
   SYSTEM;

VAR max_dim: pc.VALUE; (* := pc.code.max_dim *)

PROCEDURE check_profile(VAR proc: pc.OBJECT; dyn: BOOLEAN;
			op: SHORTINT; VAR len_t: pc.STRUCT);

  CONST t_addr=0; t_size=1; t_arr=2; t_len=3;

  PROCEDURE check_param(VAR p: pc.OBJECT; mode: pc.OB_MODE; type: INTEGER);
    VAR ok: BOOLEAN; t: pc.STRUCT;
  BEGIN
    IF p=NIL THEN pcS.err(38,proc.name^); RETURN END;
    IF p.mode#mode THEN
      IF mode = pc.ob_varpar THEN
        pcS.err(39,proc.name^,"VAR",p.name^);
      ELSE
        pcS.err(39,proc.name^,"value",p.name^);
      END;
    ELSE
      CASE type OF
	|t_addr: ok:=p.type=pcO.addr;
	|t_size: t:=p.type;
	         IF t.mode=pc.ty_range THEN t:=t.base END;
	         ok:=t.mode IN pc.WHOLEs;
	|t_len : t:=p.type;
	         IF t.mode=pc.ty_range THEN t:=t.base END;
	         ok:=t.mode IN pc.WHOLEs;
		 IF ok THEN len_t:=p.type END;
	|t_arr : ok:=p.type.mode=pc.ty_array_of;
                 IF ok THEN
                   t:=p.type.base;
                   IF t.mode=pc.ty_range THEN t:=t.base END;
		   ok:=t.mode IN pc.WHOLEs;
                 END;
		 IF ok THEN len_t:=p.type.base END;
      END;
      IF NOT ok THEN pcS.err(40,proc.name^,p.name^) END;
    END;
    p:=p.next;
  END check_param;

  VAR p: pc.OBJECT; type: pc.STRUCT;
BEGIN
  IF (proc.mode=pc.ob_cons) & (proc.type.mode=pc.ty_proctype) THEN
    IF proc.val.mode=pc.nd_proc THEN
      proc:=proc.val.obj; ASSERT(proc#NIL);
    ELSIF proc.val.mode=pc.nd_value THEN
      proc:=proc.val.val.get_object();
    END;
  END;
  IF proc = NIL THEN pcS.err(55,'NIL')
  ELSIF proc.mode IN pc.PROCs THEN
    type:=proc.type;
    IF type.base.mode#pc.ty_void THEN pcS.err(37,proc.name^) END;
    p:=type.prof;
    check_param(p,pc.ob_varpar,t_addr);
    check_param(p,pc.ob_var,t_size);
    IF dyn THEN
      IF (op=pcO.fn_new) OR (op=pcO.fn_resize) THEN
	check_param(p,pc.ob_var,t_arr);
        IF proc.type.flag=pc.flag_c THEN
	  check_param(p,pc.ob_var,t_len);
        END;
      ELSE
	check_param(p,pc.ob_var,t_len);
      END;
    END;
    IF p#NIL THEN pcS.err(38,proc.name^) END;
  ELSIF proc.mode#pc.ob_inv THEN pcS.err(55,proc.name^)
  END;
END check_profile;

PROCEDURE not_defined(new: BOOLEAN; nm-: ARRAY OF CHAR);
  VAR n: INTEGER;
BEGIN
  IF NOT pc.code.def_storage OR NOT env.config.Option("STORAGE") THEN
    IF new THEN n:=35 ELSE n:=36 END;
    pcS.error(pcS.txtpos,'e',n,nm)
  END;
END not_defined;

PROCEDURE allocate(VAR o: pc.OBJECT; type: pc.STRUCT; VAR len_t: pc.STRUCT);
BEGIN
  len_t:=pcO.lens_type;
  o:=NIL;
  IF pcS.oberon OR (type.mode#pc.ty_pointer) OR (type.flag=pc.flag_o2) THEN
    (* nothing *)
  ELSIF type.base.mode=pc.ty_array_of THEN
    IF pcO.try_vis(pcO.cur_scope,"DYNALLOCATE",o) THEN
      check_profile(o,TRUE,pcO.fn_new,len_t);
    ELSE
      not_defined(TRUE,"DYNALLOCATE");
    END;
  ELSE
    IF pcO.try_vis(pcO.cur_scope,"ALLOCATE",o) THEN
      check_profile(o,FALSE,pcO.fn_new,len_t);
    ELSE
      not_defined(TRUE,"ALLOCATE");
    END;
  END;
END allocate;

PROCEDURE deallocate(n: pc.NODE);
  VAR type,len_t: pc.STRUCT;
BEGIN
  type:=n.r.type;
  WHILE (type.mode=pc.ty_opaque) & (type.base#NIL) DO
    type := type.base;
  END;
  IF pcS.oberon OR (type.mode#pc.ty_pointer) THEN RETURN END;
  IF type.base.mode=pc.ty_array_of THEN
    IF pcO.try_vis(pcO.cur_scope,"DYNDEALLOCATE",n.obj) THEN
      check_profile(n.obj,TRUE,pcO.fn_dispose,len_t);
    ELSE
      not_defined(FALSE,"DYNDEALLOCATE");
    END;
  ELSE
    IF pcO.try_vis(pcO.cur_scope,"DEALLOCATE",n.obj) THEN
      check_profile(n.obj,FALSE,pcO.fn_dispose,len_t);
    ELSE
      not_defined(FALSE,"DEALLOCATE");
    END;
  END;
END deallocate;

PROCEDURE gen_system_function*(VAR n: pc.NODE;
			       VAR a: ARRAY OF pc.NODE;
			       no   : INTEGER;
			       desig: BOOLEAN);

  CONST
    NUM_LITERALs = pc.TY_SET{pc.ty_ZZ,pc.ty_RR,pc.ty_CC}; (* size is undef *)
    ORDINALs     = pc.TY_SET{pc.ty_void};

  VAR
    fn: pc.OBJECT;
    ps: pc.TPOS;

  PROCEDURE chk_no(i: LONGINT): BOOLEAN;
    VAR e: INTEGER;
  BEGIN
    IF no=i THEN RETURN TRUE END;
    IF no < i THEN e:=47 ELSE e:=48 END;
    pcS.error(ps,'e',e);
    RETURN FALSE;
  END chk_no;

  PROCEDURE chk_val_mode(i: INTEGER; m: pc.TY_SET): BOOLEAN;
    VAR h: pc.STRUCT;
  BEGIN
    pcB.chk_value(a[i]);
    h:=pcB.host_type(a[i].type);
    IF h.mode=pcO.ty_invtype THEN RETURN FALSE END;
    IF (h.mode = pc.ty_opaque) AND (h.base # NIL) THEN
      h := h.base;
    END;
    IF m*ORDINALs#pc.TY_SET{} THEN
      m:=m-ORDINALs;
      IF h.is_ordinal() THEN RETURN TRUE END;
      IF (h.mode=pc.ty_SS) & (h.len<=2) THEN RETURN TRUE END;
    END;
    IF (h.mode IN m) OR
       (h.mode=pc.ty_SS) & (pc.ty_char IN m) & (h.len<=2)
    THEN RETURN TRUE;
    END;
    IF pcS.ext() THEN
      IF h.mode=pc.ty_set THEN
        RETURN TRUE;
      END;
    END;
    IF m=pc.TY_SET{pc.ty_pointer} THEN pcS.error(a[i].pos,'e',135);
    ELSIF m=pc.CARDs THEN pcS.error(a[i].pos,'e',134);
    ELSE pcS.error(a[i].pos,'e',30);
    END;
    RETURN FALSE;
  END chk_val_mode;

  PROCEDURE binary(sb: pc.SUB_MODE; t: pc.STRUCT);
  BEGIN
    n.type:=t; n.l:=a[0]; n.r:=a[1];
    n.mode:=pc.nd_binary; n.sub:=sb;
  END binary;

  PROCEDURE unary(su: pc.SUB_MODE; t: pc.STRUCT);
  BEGIN
    n:=a[0];
    pcB.unary(n,su,t);
  END unary;

  PROCEDURE sproc(i: INTEGER; sp: pc.SUB_MODE);
    VAR k: INTEGER;
  BEGIN
    n.type:=pcO.void;
    n.mode:=pc.nd_sproc;
    n.sub:=sp;
    IF i=0 THEN n.r:=NIL; RETURN END;
    n.r:=a[0];
    FOR k:=0 TO i-2 DO a[k].next:=a[k+1] END;
    a[i-1].next:=NIL;
  END sproc;

  PROCEDURE value(v: pc.VALUE; t: pc.STRUCT);
  BEGIN
    n.mode:=pc.nd_value;
    n.type:=t;
    n.val:=v;
  END value;

  PROCEDURE get_string(v: pc.VALUE; len: LONGINT; VAR s: env.String);
    VAR zz: pc.VALUE; i,c: LONGINT;
  BEGIN
    zz:=pc.value.new(v.pos,pcO.integer);
    NEW(s,len+1);
    FOR i:=0 TO len-1 DO
      zz.index_get(i,v);
      c:=zz.get_integer();
      s[i]:=CHR(c);
      IF c = 0 THEN RETURN END;
    END;
    s[len]:=0X;
  END get_string;

  PROCEDURE gen_function(i: LONGINT);
    VAR
      ro: BOOLEAN;
      m0,m1: pc.TY_MODE;
      foo: LONGINT;
      k: INTEGER;
      t0,t1,len_t: pc.STRUCT;
      sb: pc.SUB_MODE;
      md,md0: pc.TY_SET;
      j: LONGINT;
      o: pc.OBJECT;
      s,v: env.String;
      x: pc.NODE;
  BEGIN
    CASE i OF
    |pcO.fn_addadr:
      IF pcS.oberon THEN
        md0:=pc.TY_SET{pc.ty_pointer}+pc.WHOLEs;       md:=pc.WHOLEs;
      ELSE
        md0:=pc.TY_SET{pc.ty_pointer, pc.ty_opaque};   md:=pc.CARDs;
      END;
      IF chk_no(2) & chk_val_mode(0,md0) & chk_val_mode(1,md) THEN
	pcB.convert(a[0],pcO.addr);
	IF pc.code.address16 THEN pcB.convert(a[1],pcO.cardinal);
	ELSE pcB.convert(a[1],pcO.longcard);
	END;
	binary(pc.sb_addadr,pcO.addr);
	pcB.weak(n);
      END;
    |pcO.fn_subadr:
      IF pcS.oberon THEN
        md0:=pc.TY_SET{pc.ty_pointer}+pc.WHOLEs; md:=pc.WHOLEs;
      ELSE
        md0:=pc.TY_SET{pc.ty_pointer};           md:=pc.CARDs;
      END;
      IF chk_no(2) & chk_val_mode(0,md0) & chk_val_mode(1,md) THEN
	pcB.convert(a[0],pcO.addr);
	IF pc.code.address16 THEN pcB.convert(a[1],pcO.cardinal);
	ELSE pcB.convert(a[1],pcO.longcard);
	END;
	binary(pc.sb_subadr,pcO.addr);
	pcB.weak(n);
      END;
    |pcO.fn_difadr:
      IF chk_no(2) &
	 chk_val_mode(0,pc.TY_SET{pc.ty_pointer}) &
	 chk_val_mode(1,pc.TY_SET{pc.ty_pointer})
      THEN
	pcB.convert(a[0],pcO.addr);
	pcB.convert(a[1],pcO.addr);
	binary(pc.sb_difadr,pcO.difadr);
      END;
    |pcO.fn_difadrc:
      IF chk_no(2) &
	 chk_val_mode(0,pc.TY_SET{pc.ty_pointer}) &
	 chk_val_mode(1,pc.TY_SET{pc.ty_pointer})
      THEN
	pcB.convert(a[0],pcO.addr);
	pcB.convert(a[1],pcO.addr);
	binary(pc.sb_difadr,pcO.longcard);
      END;
    |pcO.fn_makeadr:
      IF chk_no(1) & chk_val_mode(0,pc.WHOLEs) THEN
	pcB.convert(a[0],pcO.longcard);
	pcB.convert(a[0],pcO.addr);
        n:=a[0];
      END;
    |pcO.fn_adr:
      IF chk_no(1) THEN
	IF pcS.ext() THEN pcB.chk_designator(a[0]);
	ELSE pcB.chk_lvalue(a[0]);
	END;
	unary(pc.su_adr,pcO.addr);
      END;
    |pcO.fn_adr_o2:
      IF chk_no(1) THEN
	IF pcS.ext() THEN pcB.chk_designator(a[0]);
	ELSE pcB.chk_lvalue(a[0]);
	END;
	unary(pc.su_adr,pcO.addr);
        pcB.convert(n,pcO.longint);
      END;
    |pcO.fn_ref:
      IF chk_no(1) THEN
        IF pcS.ext() THEN pcB.chk_designator(a[0]);
        ELSE pcB.chk_lvalue(a[0]);
        END;
        unary(pc.su_adr,pcO.addr);
        t0 := pcO.new_type(pc.ty_pointer);
        t0.base:=a[0].type;
        pcB.convert(n,t0);
      END;
    |pcO.fn_valid:
      IF chk_no(1) THEN
        pcB.chk_designator(a[0]);
        x:=a[0];
        IF   (x.mode = pc.nd_deref)
           & (x.l.mode = pc.nd_var)
           & (x.l.obj.mode = pc.ob_var)
           & (pcO.otag_varpar_nil IN x.l.obj.tags)
        THEN
          n.mode:=pc.nd_binary; n.sub:=pc.sb_neq;
          n.type:=pcO.boolean;
          n.l:=x.l;
          pcB.gen_usage(pcO.nil,x);
          pcB.convert(x,n.l.type);
          n.r:=x;
        ELSE
          pcS.error(a[0].pos,'e',161);
        END;
      END;
    |pcO.fn_rot:
      IF chk_no(2) &
	 chk_val_mode(0,pc.TY_SET{pc.ty_set}) &
	 chk_val_mode(1,pc.INTs)
      THEN
	pcB.convert(a[1],pcO.integer);
	binary(pc.sb_rot,a[0].type);
      END;
    |pcO.fn_lsh:
      IF chk_no(2) & chk_val_mode(1,pc.INTs) THEN
        IF pcS.oberon THEN
	  ro:=chk_val_mode(0,pc.TY_SET{pc.ty_set,pc.ty_char,pc.ty_loc}+pc.INTs);
        ELSE
	  ro:=chk_val_mode(0,pc.TY_SET{pc.ty_set});
        END;
        IF ro THEN binary(pc.sb_lsh,a[0].type) END;
      END;
    |pcO.fn_cast:
      IF chk_no(2) THEN
	IF a[0].mode#pc.nd_type THEN
	  pcS.error(a[0].pos,'e',136);
        ELSE
          pcB.cast(a[1],a[0].type,desig);
          n:=a[1];
	END;
      END;
    |pcO.fn_tsize:
      IF no<1 THEN
        pcS.error(ps,'e',47);
      ELSIF a[0].mode#pc.nd_type THEN
	pcS.error(a[0].pos,'e',136);
(*      ELSIF no>1 THEN
	pcS.error(ps,'e',200);  !!!!! *)
      ELSIF a[0].type.mode IN pc.NUM_LITERALs THEN
        pcS.error(a[1].pos,'e',137);
      ELSIF a[0].type.mode=pc.ty_array_of THEN
        pcS.error(a[1].pos,'e',78);
      ELSE
	unary(pc.su_size,pcO.ZZ_type);
      END;
      IF n.type.mode=pc.ty_ZZ THEN pcB.eval_value(n) END;
    |pcO.fn_dec,pcO.fn_inc:
      IF no>=1 THEN
	IF no=1 THEN
	  pcO.new(a[1],pc.nd_value);
	  a[1].pos:=ps;
	  a[1].val:=pc.value.new(ps,pcO.ZZ_type);
	  a[1].val.set_integer(1);
	  a[1].type:=pcO.ZZ_type;
	  INC(no);
	END;
	pcB.chk_lvalue(a[0]);
	IF a[0].type.mode=pcO.ty_invtype THEN
	  (* nothing *)
	ELSIF NOT a[0].type.is_ordinal() THEN
          pcS.error(a[0].pos,'e',33);
	ELSIF pcS.oberon & NOT (a[0].type.mode IN pc.INTs) THEN
	  pcS.error(a[0].pos,'e',151);
	ELSIF chk_no(2) & chk_val_mode(1,pc.WHOLEs) THEN
	  IF pcS.oberon &
	     NOT pcS.ext() &
	     (a[0].type.mode#a[1].type.mode) &
	     (a[1].type.mode#pc.ty_ZZ)
	  THEN pcS.error(a[1].pos,'e',30);
	  END;
	  pcB.convert(a[1],a[0].type.super_type());
	  IF a[0].type.mode IN pc.CARDs THEN
	    ro:=env.card_ovfl IN env.config.tags;
	  ELSIF a[0].type.mode IN pc.INTs THEN
	    ro:=env.int_ovfl IN env.config.tags;
          ELSE
	    ro:=env.range_check IN env.config.tags;
	  END;
	  IF i=pcO.fn_dec THEN
	    pcB.binary(ps,a[0],a[1],pc.sb_pre_dec,a[0].type);
          ELSE
	    pcB.binary(ps,a[0],a[1],pc.sb_pre_inc,a[0].type);
          END;
	  IF ro THEN INCL(a[0].tags,pc.ntag_chk_range) END;
	  n.l:=a[0];
	  n.mode:=pc.nd_eval;
	  n.type:=pcO.void;
	END;
      ELSIF chk_no(1) THEN
      END;
    |pcO.fn_dispose:
      IF no>1 THEN
	pcS.error(ps,'e',200);
      ELSIF chk_no(1) THEN
	pcB.chk_lvalue(a[0]);
	t0:=a[0].type;
	IF (t0.mode=pc.ty_opaque) & (t0.base#NIL) THEN t0:=t0.base END;
	IF (t0.mode#pc.ty_pointer)OR(t0.flag = pc.flag_o2) THEN
	  pcS.error(a[0].pos,'e',30);
	ELSE
          IF pcS.oberon AND ~pcS.ext() THEN
            pcS.err(102,"(DISPOSE function)");
          END;
	  sproc(1,pc.sp_dispose);
          ASSERT(t0.flag # pc.flag_o2);
	  deallocate(n);
	END;
      END;
    |pcO.fn_excl,pcO.fn_incl:
      IF chk_no(2) THEN
	pcB.chk_lvalue(a[0]);
	pcB.chk_value(a[1]);
	IF NOT (a[0].type.mode IN pc.SETs) THEN
	  pcS.error(a[0].pos,'e',30);
	ELSE
          IF pcS.oberon THEN
            IF pcS.ext() THEN
              pcB.expression_compatibility_2(a[1].pos,
                pcB.host_type(a[0].type.base),a[1]);
            ELSE
              IF chk_val_mode(1,pc.INTs) THEN END;
            END;
          ELSE
            pcB.assign_compatibility(a[1].pos,
              pcB.host_type(a[0].type.base),a[1]);
          END;
          (* super_type asserts on inv_type *)
          IF a[0].type.base.mode # pcO.ty_invtype THEN
            pcB.convert(a[1],a[0].type.base.super_type());
          END;
	  IF i=pcO.fn_excl THEN sproc(2,pc.sp_excl);
	  ELSE sproc(2,pc.sp_incl);
          END;
	  IF env.set_check IN env.config.tags THEN
	    INCL(n.tags,pc.ntag_chk_range);
	  END;
	END;
      END;
    |pcO.fn_halt:
      IF (no=1) & (pcS.oberon OR pcS.ext()) THEN
	IF chk_val_mode(0,pc.WHOLEs) THEN
	  pcB.convert(a[0],pcO.longcard);
	  sproc(1,pc.sp_halt);
	END;
      ELSIF chk_no(0) THEN
	sproc(0,pc.sp_abort);
      END;
    |pcO.fn_new:
      IF no>=1 THEN
	pcB.chk_lvalue(a[0]);
	t0:=a[0].type;
	IF (t0.mode=pc.ty_opaque) & (t0.base#NIL) THEN t0:=t0.base END;
	t1:=t0.base;
	IF t0.mode=pc.ty_pointer THEN
	  IF t1.mode#pc.ty_array_of THEN
	    IF chk_no(1) THEN
	      sproc(1,pc.sp_new);
	      allocate(n.obj,t0,len_t);
	    END;
	  ELSE
	    IF chk_no(t1.len+1) THEN
	      allocate(o,t0,len_t);
	      FOR k:=1 TO no-1 DO
		t0:=pcB.host_type(a[k].type);
		IF t0.mode IN pc.WHOLEs THEN pcB.convert(a[k],len_t);
		ELSE pcB.err(a[k],30);
		END;
	      END;
	      sproc(no,pc.sp_new); n.obj:=o;
	    END;
	  END;
	ELSIF t0.mode#pcO.ty_invtype THEN pcS.error(a[0].pos,'e',30);
	END;
      ELSIF chk_no(1) THEN
      END;
    |pcO.fn_abs:
      IF chk_no(1) & chk_val_mode(0,pc.INTs+pc.REALs) THEN
	unary(pc.su_abs,pcB.host_type(a[0].type)); pcB.weak(n);
      END;
    |pcO.fn_cap:
      IF chk_no(1) & chk_val_mode(0,pc.TY_SET{pc.ty_char}) THEN
	pcB.convert(a[0],pcO.char);
	unary(pc.su_cap,pcO.char);
      END;
    |pcO.fn_chr:
      IF pcS.ext() THEN md:=ORDINALs ELSE md:=pc.WHOLEs END;
      IF chk_no(1) & chk_val_mode(0,md) THEN
	pcB.convert(a[0],pcO.char);
        n:=a[0];
      END;
    |pcO.fn_cmplx:
      IF pcS.oberon THEN md:=pc.REALs+pc.WHOLEs;
      ELSE md:=pc.REALs;
      END;
      IF chk_no(2) & chk_val_mode(0,md) & chk_val_mode(1,md) THEN
	m0:=a[0].type.mode;
	m1:=a[1].type.mode;
	IF m0=pc.ty_ZZ THEN pcB.convert(a[0],pcO.RR_type); m0:=pc.ty_RR END;
	IF m1=pc.ty_ZZ THEN pcB.convert(a[1],pcO.RR_type); m1:=pc.ty_RR END;
	IF m0 IN pc.WHOLEs THEN pcB.convert(a[0],pcO.real); m0:=pc.ty_real END;
	IF m1 IN pc.WHOLEs THEN pcB.convert(a[1],pcO.real); m1:=pc.ty_real END;
	IF (m0=pc.ty_RR) & (m1=pc.ty_RR) THEN
	  binary(pc.sb_cmplx,pcO.CC_type);
	ELSIF (m0#pc.ty_RR) & (m1#pc.ty_RR) & (m0#m1) & NOT pcS.oberon THEN
	  pcS.error(ps,'e',30);
	ELSIF (m0=pc.ty_longreal) OR (m1=pc.ty_longreal) THEN
	  pcB.convert(a[0],pcO.longreal);
	  pcB.convert(a[1],pcO.longreal);
	  binary(pc.sb_cmplx,pcO.lcomplex);
	ELSE
	  pcB.convert(a[0],pcO.real);
	  pcB.convert(a[1],pcO.real);
	  binary(pc.sb_cmplx,pcO.complex);
	END;
      END;
    |pcO.fn_float:
      IF chk_no(1) & chk_val_mode(0,pc.REALs+pc.WHOLEs) THEN
	pcB.convert(a[0],pcO.real);
        n:=a[0];
      END;
    |pcO.fn_high:
      IF chk_no(1) THEN
	pcB.chk_designator(a[0]);
	IF pcS.ext() THEN
          IF NOT (a[0].type.mode IN pc.ARRs) THEN pcS.error(a[0].pos,'e',50) END;
        ELSIF a[0].type.mode#pc.ty_array_of THEN pcS.error(a[0].pos,'e',132)
	END;
	foo:=0;
	WHILE (a[0].mode=pc.nd_index) &
	      (a[0].type.mode=pc.ty_array_of) &
	      (a[0].l.type.mode=pc.ty_array_of)
	DO a[0]:=a[0].l; INC(foo);
	END;
	pcO.new(a[1],pc.nd_value);
	a[1].pos:=ps;
	a[1].val:=pc.value.new(ps,pcO.ZZ_type);
	a[1].val.set_integer(foo);
	a[1].type:=pcO.ZZ_type;
	IF a[0].type.mode=pc.ty_array_of THEN
	  binary(pc.sb_high,pcO.index);
    IF a[0].mode = pc.nd_deref THEN
      IF NOT (a[0].l.type.flag IN pc.LangsWithOpenArrays) THEN
        env.errors.Error (a[0].pos, 79); -- HIGH or LEN cannot be applied to a foreign open array
      END;
    ELSIF NOT (a[0].type.flag IN pc.LangsWithOpenArrays) THEN
      env.errors.Error (a[0].pos, 79); -- HIGH or LEN cannot be applied to a foreign open array
    END;
	ELSIF pcS.oberon THEN
	  binary(pc.sb_high,pcO.longint);
	ELSE
	  binary(pc.sb_high,pcO.ZZ_type);
	END;
      END;
    |pcO.fn_im:
      IF chk_no(1) & chk_val_mode(0,pc.CPLXs) THEN
	CASE a[0].type.mode OF
	  |pc.ty_CC      : unary(pc.su_im,pcO.RR_type);
	  |pc.ty_complex : unary(pc.su_im,pcO.real);
	  |pc.ty_lcomplex: unary(pc.su_im,pcO.longreal);
	END;
      END;
    |pcO.fn_int:
      md:=ORDINALs+pc.REALs;
      IF pcS.ext() THEN INCL(md,pc.ty_loc) END;
      IF chk_no(1) & chk_val_mode(0,md) THEN
        IF a[0].type.mode = pc.ty_loc THEN
          pcB.cast(a[0],pcO.shortint,FALSE);
        END;
        pcB.convert(a[0],pcO.m2_int);
        n:=a[0];
      END;
    |pcO.fn_length:
      IF chk_no(1) THEN
	pcB.chk_value(a[0]);
	t0:=a[0].type;
	IF (t0.mode=pc.ty_SS) OR
	   (t0.mode IN pc.ARRs) & (pcB.is_char(t0.base))
	THEN
	  IF pcS.oberon THEN unary(pc.su_length,pcO.longint);
	  ELSE unary(pc.su_length,pcO.index);
	  END;
	ELSIF t0.mode#pcO.ty_invtype THEN pcS.error(a[0].pos,'e',30);
	END;
      END;
    |pcO.fn_lfloat:
      IF chk_no(1) & chk_val_mode(0,pc.REALs+pc.WHOLEs) THEN
	pcB.convert(a[0],pcO.longreal);
        n:=a[0];
      END;
    |pcO.fn_max:
      IF chk_no(1) THEN
	t0:=a[0].type; t1:=pcB.host_type(t0);
	IF t0.mode=pcO.ty_invtype THEN
	ELSIF a[0].mode#pc.nd_type THEN
          pcS.error(a[0].pos,'e',136);
	ELSIF (pcS.oberon OR pcS.ext()) & (t0.mode IN pc.SETs) THEN
	  t1:=pcB.host_type(t0.base);
	  IF pcS.oberon THEN value(t0.max,pcO.integer);
	  ELSIF t1.mode IN pc.WHOLEs THEN value(t0.max,pcO.ZZ_type);
	  ELSE value(t0.max,t1);
	  END;
	ELSIF NOT pcB.is_scalar(t0) THEN
	  pcS.error(a[0].pos,'e',32);
	ELSIF t0.mode IN pc.REALs THEN
	  unary(pc.su_max,pcO.RR_type);
	  pcB.eval_value(n);
	  IF n.mode#pc.nd_value THEN n.type:=t1 END;
	ELSIF NOT pcS.oberon & (t1.mode IN pc.WHOLEs) THEN
	  value(t0.max,pcO.ZZ_type);
	ELSE
	  value(t0.max,t1);
	END;
      END;
    |pcO.fn_min:
      IF chk_no(1) THEN
	t0:=a[0].type; t1:=pcB.host_type(t0);
	IF t0.mode=pcO.ty_invtype THEN
	ELSIF a[0].mode#pc.nd_type THEN
          pcS.error(a[0].pos,'e',136);
	ELSIF pcS.ext() & (t0.mode IN pc.SETs) THEN
	  t1:=pcB.host_type(t0.base);
	  IF pcS.oberon THEN value(t0.min,pcO.integer);
	  ELSIF t1.mode IN pc.WHOLEs THEN value(t0.min,pcO.ZZ_type);
	  ELSE value(t0.min,t1);
	  END;
	ELSIF NOT pcB.is_scalar(t0) THEN
	  pcS.error(a[0].pos,'e',32);
	ELSIF t0.mode IN pc.REALs THEN
	  unary(pc.su_min,pcO.RR_type);
	  pcB.eval_value(n);
	  IF n.mode#pc.nd_value THEN n.type:=t1 END;
	ELSIF NOT pcS.oberon & (t1.mode IN pc.WHOLEs) THEN
	  value(t0.min,pcO.ZZ_type);
	ELSE
	  value(t0.min,t1);
	END;
      END;
    |pcO.fn_odd:
      IF chk_no(1) & chk_val_mode(0,pc.WHOLEs) THEN
	unary(pc.su_odd,pcO.boolean);
      END;
    |pcO.fn_ord:
      IF pcS.oberon THEN md:=pc.TY_SET{pc.ty_char} ELSE md:=pc.TY_SET{pc.ty_char}+ORDINALs END;
      IF pcS.ext()  THEN md:=md+pc.TY_SET{pc.ty_loc}+ORDINALs END;
      IF chk_no(1) & chk_val_mode(0,md) THEN
	IF pcS.oberon THEN
          IF a[0].type.mode = pc.ty_loc THEN
            pcB.cast(a[0],pcO.shortint,FALSE);
          END;
	  pcB.convert(a[0],pcO.integer);
        ELSIF a[0].type.mode IN (pc.NUM_LITERALs+pc.TY_SET{pc.ty_SS}) THEN
	  pcB.convert(a[0],pcO.ZZ_type);
	ELSE
          IF a[0].type.mode = pc.ty_loc THEN
            pcB.cast(a[0],pcO.shortcard,FALSE);
          END;
          pcB.convert(a[0],pcO.m2_card);
	END;
        n:=a[0];
      END;
    |pcO.fn_prot:
      IF chk_no(0) THEN
	n.mode:=pc.nd_prot;
	n.type:=pcO.protection;
      END;
    |pcO.fn_re:
      IF chk_no(1) & chk_val_mode(0,pc.CPLXs) THEN
	CASE a[0].type.mode OF
	  |pc.ty_CC      : unary(pc.su_re,pcO.RR_type);
	  |pc.ty_complex : unary(pc.su_re,pcO.real);
	  |pc.ty_lcomplex: unary(pc.su_re,pcO.longreal);
	END;
      END;
    |pcO.fn_size,pcO.fn_bytes,pcO.fn_bits:
      CASE i OF
	|pcO.fn_size : sb:=pc.su_size;
	|pcO.fn_bytes: sb:=pc.su_bytes;
	|pcO.fn_bits : sb:=pc.su_bits;
      END;
      IF chk_no(1) THEN
	t0:=a[0].type;
	IF t0.mode=pcO.ty_invtype THEN
	ELSIF a[0].mode=pc.nd_type THEN
	  unary(sb,pcO.ZZ_type);
        ELSIF pcS.oberon & NOT pcS.ext() THEN
          pcS.error(a[0].pos,'e',136);
	ELSIF t0.mode=pc.ty_array_of THEN
	  IF NOT pcS.ext() & NOT pcS.oberon THEN
	    pcS.error(a[0].pos,'e',78);
	  END;
	  pcB.chk_designator(a[0]);
	  IF pcS.oberon THEN unary(sb,pcO.longint);
	  ELSE unary(sb,pcO.index);
	  END;
        ELSIF t0.mode IN pc.NUM_LITERALs THEN
          pcS.error(a[0].pos,'e',137);
	ELSIF pcS.ext() THEN
	  pcB.chk_designator(a[0]);
	  unary(sb,pcO.ZZ_type);
	ELSE
	  pcB.chk_entire_designator(a[0]);
	  unary(sb,pcO.ZZ_type);
	END;
	IF n.type.mode=pc.ty_ZZ THEN
	  pcB.eval_value(n);
          IF (env.errors.err_cnt = 0) & (n.mode#pc.nd_value) THEN
	    IF pcS.oberon THEN pcB.convert(n,pcO.longint);
	    ELSE pcB.convert(n,pcO.longcard);
	    END;
	  END;
	END;
      END;
    |pcO.fn_trunc:
      IF chk_no(1) & chk_val_mode(0,pc.REALs) THEN
        pcB.convert(a[0],pcO.longcard);
        n:=a[0];
      END;
    |pcO.fn_val:
      md:=ORDINALs+pc.REALs;
      IF pcS.ext() THEN md:=ORDINALs+pc.REALs+pc.ADRs+pc.TY_SET{pc.ty_loc} END;
      IF chk_no(2) & chk_val_mode(1,md) THEN
        IF a[0].mode#pc.nd_type THEN pcS.error(a[0].pos,'e',136);
	ELSIF (NOT pcB.is_scalar(a[0].type)) & ~((pcS.ext()&(a[0].type.mode=pc.ty_set))) THEN
          pcS.error(a[0].pos,'e',32);
	ELSE
	  t1:=pcB.host_type(a[1].type);
	  t0:=pcB.host_type(a[0].type);
          IF t1.mode = pc.ty_loc THEN
            IF t0.mode IN pc.INTs THEN t1:=pcO.shortint
            ELSE t1:=pcO.shortcard
            END;
            pcB.cast(a[1],t1,FALSE);
          END;
	  IF (t1=t0) OR
	     (t1.mode IN (pc.WHOLEs+pc.REALs)) &
	     (t0.mode IN (pc.WHOLEs+pc.REALs)) OR
	     (t1.mode IN pc.WHOLEs) OR
	     (t0.mode IN pc.WHOLEs) OR
             (t0.mode = pc.ty_boolean) & (* conversion between bool8/bool32 *)
               (t1.mode = pc.ty_boolean) OR
             (t1.mode IN pc.ADRs) & (t0.mode IN pc.WHOLEs) OR
             ((pcS.ext()&(t0.mode=pc.ty_set)&(t1.mode=pc.ty_set)))
	  THEN
            pcB.convert(a[1],t0); n:=a[1];
	  ELSE pcS.error(ps,'e',34);
	  END;
	END;
      END;
    |pcO.fn_assert:
      IF (pcS.oberon OR pcS.M2_ext()) & (no=2) THEN
	IF chk_val_mode(0,pc.TY_SET{pc.ty_boolean}) &
	   chk_val_mode(1,pc.WHOLEs)
	THEN
	  pcB.convert(a[1],pcO.longcard);
	  sproc(2,pc.sp_assert);
        END;
      ELSIF chk_no(1) & (chk_val_mode(0,pc.TY_SET{pc.ty_boolean})) THEN
	sproc(1,pc.sp_assert);
      END;
    |pcO.fn_ash:
      IF chk_no(2) &
	 chk_val_mode(0,pc.WHOLEs) &
	 chk_val_mode(1,pc.INTs)
      THEN
	IF pcS.oberon THEN
	  pcB.convert(a[0],pcO.longint);
	  pcB.convert(a[1],pcO.longint);
	  binary(pc.sb_ash,pcO.longint);
	ELSIF (a[0].type.mode=pc.ty_ZZ) & (a[1].type.mode=pc.ty_ZZ) THEN
	  binary(pc.sb_ash,pcO.ZZ_type);
	ELSE
	  pcB.convert(a[1],pcO.longint);
	  binary(pc.sb_ash,pcB.host_type(a[0].type));
	END;
	pcB.weak(n);
      END;
    |pcO.fn_copy:
      IF chk_no(2) THEN
	pcB.chk_value(a[0]);
	pcB.chk_lvalue(a[1]);
	t0:=a[0].type;
	t1:=a[1].type;
	IF (t0.mode=pcO.ty_invtype) OR (t1.mode=pcO.ty_invtype) THEN
	ELSIF (t0.mode#pc.ty_SS) &
	      NOT ((t0.mode IN pc.ARRs) & pcB.is_char(t0.base))
	THEN pcS.error(a[0].pos,'e',30);
	ELSIF NOT ((t1.mode IN pc.ARRs) & pcB.is_char(t1.base)) THEN
	  pcS.error(a[1].pos,'e',30);
	ELSE sproc(2,pc.sp_copy);
	END;
      END;
    |pcO.fn_len:
      IF no IN {1,2} THEN
	IF (no=2) & chk_val_mode(1,pc.WHOLEs) THEN
	  pcB.eval_value(a[1]);
          IF a[1].mode#pc.nd_value THEN
            pcS.error(a[1].pos,'e',87); no:=1; j:=0;
          ELSE
            IF max_dim = NIL THEN
              max_dim:=pc.value.new(a[1].pos,pcO.longint);
              max_dim.set_integer(pc.code.max_dim);
            END;
            IF a[1].val.is_neg() OR pcB.cmp_value(pc.sb_gtr,a[1].val,max_dim) THEN
              pcS.error(a[1].pos,'e',61); no:=1; j:=0;
            ELSE
              j:=a[1].val.get_integer();
            END;
	  END;
	ELSE
          no:=1; j:=0;
	END;
	pcB.chk_designator(a[0]);
	WHILE (a[0].mode=pc.nd_index) &
	      (a[0].type.mode IN pc.ARRs) &
	      (a[0].l.type.mode IN pc.ARRs)
	DO a[0]:=a[0].l; INC(j);
	END;
	IF no<2 THEN
	  pcO.new(a[1],pc.nd_value);
          a[1].pos := ps;
          a[1].end := ps;
	  a[1].val := pc.value.new(ps,pcO.ZZ_type);
	END;
	a[1].val.set_integer(j);
	a[1].type:=pcO.ZZ_type;
	t0:=a[0].type;
        IF ~ (t0.mode IN pc.ARRs) THEN
          pcS.error(a[0].pos,'e',50);
        ELSE
          WHILE (j>0) & (t0.mode IN pc.ARRs) DO DEC(j); t0:=t0.base END;
          IF ~ (t0.mode IN pc.ARRs) THEN pcS.error(a[1].pos,'e',61) END;
        END;
	IF t0.mode=pc.ty_array_of THEN
	  binary(pc.sb_len,pcO.longint);
	ELSE
	  binary(pc.sb_len,pcO.ZZ_type);
	END;
      ELSIF chk_no(1) THEN (* nothing *)
      END;
    |pcO.fn_get:
      IF pcS.oberon THEN md:=pc.INTs ELSE md:=pc.TY_SET{pc.ty_pointer} END;
      IF NOT pcS.ext() & NOT pcS.oberon THEN
        pcS.error(ps,'w',316);
      END;
      IF chk_no(2) & chk_val_mode(0,md) THEN
	pcB.chk_lvalue(a[1]);
	pcB.convert(a[0],pcO.addr);
	sproc(2,pc.sp_get);
      END;
    |pcO.fn_put:
      IF pcS.oberon THEN md:=pc.INTs ELSE md:=pc.TY_SET{pc.ty_pointer} END;
      IF ~ pcS.ext() & ~ pcS.oberon THEN
        pcS.error(ps,'w',316);
      END;
      IF chk_no(2) & chk_val_mode(0,md) THEN
	pcB.chk_value(a[1]);
	pcB.convert(a[0],pcO.addr);
        IF a[1].type.mode IN pc.NUM_LITERALs THEN
          pcS.error(a[1].pos,'e',137)
        ELSIF a[1].type.mode = pc.ty_SS THEN
          pcS.error(a[1].pos,'e',138)
        END;
	sproc(2,pc.sp_put);
      END;
    |pcO.fn_move:
      IF pcS.oberon THEN md:=pc.TY_SET{pc.ty_pointer}+pc.WHOLEs;
      ELSE md:=pc.TY_SET{pc.ty_pointer};
      END;
      IF ~ pcS.ext() & ~ pcS.oberon THEN
        pcS.error(ps,'w',316);
      END;
      IF chk_no(3) &
	 chk_val_mode(0,md) &
	 chk_val_mode(1,md) &
	 chk_val_mode(2,pc.WHOLEs)
      THEN
	pcB.convert(a[0],pcO.addr);
	pcB.convert(a[1],pcO.addr);
	pcB.convert(a[2],pcO.index);
	sproc(3,pc.sp_move);
      END;
    |pcO.fn_fill:
      IF pcS.oberon THEN md:=pc.TY_SET{pc.ty_pointer}+pc.WHOLEs;
      ELSE md:=pc.TY_SET{pc.ty_pointer};
      END;
      IF ~ pcS.ext() & ~ pcS.oberon THEN
        pcS.error(ps,'w',316);
      END;
      IF chk_no(3) &
	 chk_val_mode(0,md) &
	 chk_val_mode(2,pc.WHOLEs)
      THEN
        pcB.chk_value(a[1]);
        pcB.compatibility_with_byte(a[1]);
	pcB.convert(a[0],pcO.addr);
	pcB.convert(a[2],pcO.index);
        sproc(3,pc.sp_fill);
      END;

    |pcO.fn_eval:
      IF chk_no(1) THEN
	n.l:=a[0];
	n.mode:=pc.nd_eval;
	n.type:=pcO.void;
      ELSE
        pcS.error(ps, 'e', 0);
      END;

    |pcO.fn_short:
      IF chk_no(1) &
	 chk_val_mode(0,pc.TY_SET{pc.ty_longint,pc.ty_longlongint,pc.ty_integer,pc.ty_longreal}+
			pc.TY_SET{pc.ty_longcard,pc.ty_longlongcard,pc.ty_cardinal})
      THEN
	t0:=pcB.host_type(a[0].type);
	CASE t0.mode OF
	  |pc.ty_integer  : t1:=pcO.shortint;
	  |pc.ty_longint  : t1:=pcO.integer;
	  |pc.ty_longlongint: t1:=pcO.longint;
	  |pc.ty_longreal : t1:=pcO.real;
	  |pc.ty_longcard : t1:=pcO.cardinal;
	  |pc.ty_longlongcard : t1:=pcO.longcard;
	  |pc.ty_cardinal : t1:=pcO.shortcard;
	END;
        pcB.convert(a[0],t1); n:=a[0];
      END;
    |pcO.fn_long:
      IF chk_no(1) &
	 chk_val_mode(0,pc.TY_SET{pc.ty_shortint,pc.ty_integer,pc.ty_real}+
			pc.TY_SET{pc.ty_shortcard,pc.ty_cardinal} +
                        pc.TY_SET{pc.ty_ZZ})
      THEN
	t0:=pcB.host_type(a[0].type);
        IF t0.mode = pc.ty_ZZ THEN
          t0 := pcB.lit_value_type( a[0].val, t0, pcO.longint);
        END;
	CASE t0.mode OF
	  |pc.ty_integer  : t1 := pcO.longint;
	  |pc.ty_shortint : t1 := pcO.integer;
	  |pc.ty_real     : t1 := pcO.longreal;
	  |pc.ty_shortcard: t1 := pcO.cardinal;
	  |pc.ty_cardinal : t1 := pcO.longcard;
          |pc.ty_ZZ       : t1 := a[0].type;    pcS.error(a[0].pos,'e',30);
	END;
        pcB.convert(a[0],t1); n:=a[0];
      END;
    |pcO.fn_entier:
      IF chk_no(1) & chk_val_mode(0,pc.REALs) THEN
	unary(pc.su_entier,pcO.longint); pcB.weak(n);
      END;
    |pcO.fn_code:
      IF chk_no(1) & chk_val_mode(0,pc.TY_SET{pc.ty_SS}) THEN
	sproc(1,pc.sp_code);
      END;
    |pcO.fn_sysnew:
      IF (no=1) OR (no=2) & chk_val_mode(1,pc.WHOLEs) THEN
	pcB.chk_lvalue(a[0]); t0:=a[0].type;
	IF (t0.mode=pc.ty_pointer) &
	   ( (no=2) OR (t0.base.mode#pc.ty_array_of) )
	THEN
	  sproc(no,pc.sp_sysnew);
	ELSIF t0.mode#pcO.ty_invtype THEN
	  pcS.error(a[0].pos,'e',30);
	END;
      ELSIF chk_no(2) THEN
      END;
    |pcO.fn_sysdispose:
      IF chk_no(1) THEN
	pcB.chk_lvalue(a[0]); t0:=a[0].type;
	IF t0.mode=pc.ty_pointer THEN
	  sproc(1,pc.sp_dispose);
	ELSIF t0.mode#pcO.ty_invtype THEN
	  pcS.error(a[0].pos,'e',30);
	END;
      END;
    |pcO.fn_target:
      IF chk_no(0) THEN
        env.config.Equation("ENV_TARGET",s);
        pcO.new(n,pc.nd_value);
        n.type := pcO.new_type(pc.ty_SS);
        n.type.len:=LENGTH(s^)+1;
        n.val:=pc.value.new(ps,n.type);
        n.val.set_string(s^);
      END;
    |pcO.fn_option:
      IF chk_no(1) & chk_val_mode(0,pc.TY_SET{pc.ty_SS}) THEN
        pcB.eval_value(a[0]);
        IF a[0].mode # pc.nd_value THEN pcB.err(a[0],87)
        ELSE
          get_string(a[0].val,a[0].type.len,s);
          k:=ORD(env.config.Option(s^));
          IF env.config.res # env.ok THEN pcS.err(320,s^)
          ELSE
            pcO.new(n,pc.nd_value);
            n.type:=pcO.boolean;
            n.val:=pc.value.new(ps,n.type);
            n.val.set_integer(k);
          END;
        END;
      END;
    |pcO.fn_equation:
      IF chk_no(1) & chk_val_mode(0,pc.TY_SET{pc.ty_SS}) THEN
        pcB.eval_value(a[0]);
        IF a[0].mode # pc.nd_value THEN pcB.err(a[0],87)
        ELSE
          get_string(a[0].val,a[0].type.len,s);
          env.config.Equation(s^,v);
          IF env.config.res # env.ok THEN pcS.err(322,s^);
          ELSE
            pcO.new(n,pc.nd_value);
            n.type := pcO.new_type(pc.ty_SS);
            n.val:=pc.value.new(ps,n.type);
            IF v = NIL THEN
              n.type.len:=1;
              n.val.set_string("");
            ELSE
              n.type.len:=LENGTH(v^)+1;
              n.val.set_string(v^);
            END;
          END;
        END;
      END;
    |pcO.fn_timestamp:
      IF chk_no(0) THEN
        env.config.Equation("ENV_TARGET",s);
        pcO.new(n,pc.nd_value);
        n.type := pcO.longcard;
        n.val := pc.value.new(ps, n.type);
        n.val.set_integer(SYSTEM.VAL(LONGINT, env.config.GetTimeStamp()));
        n.val.cast_ordinal(n.type);
      END;
    |pcO.fn_fldoffs:
      IF chk_no(1) THEN
	t0:=a[0].type;
	IF t0.mode=pcO.ty_invtype THEN
	ELSIF a[0].mode=pc.nd_field THEN
	  unary(pc.su_byte_offs,pcO.ZZ_type);
        END;
      END;
    |pcO.fn_pred, pcO.fn_succ:
      IF no>=1 THEN
	IF no=1 THEN
	  pcO.new(a[1],pc.nd_value);
	  a[1].pos:=ps;
	  a[1].val:=pc.value.new(ps,pcO.ZZ_type);
	  a[1].val.set_integer(1);
          a[1].type := a[0].type;
	  INC(no);
	END;
        pcB.convert(a[1],a[0].type);
	IF i=pcO.fn_succ THEN
          pcB.binary(ps,a[0],a[1],pc.sb_plus,a[0].type);
        ELSE
          pcB.binary(ps,a[0],a[1],pc.sb_minus,a[0].type);
        END;
        a[0].end := pcS.txtpos;
        n := a[0];
      ELSIF chk_no(1) THEN
      END;
  <* IF MCS THEN *>
    |pcO.fn_register:
      pcS.error(ps, 'w', 342, pcS.name);
      pcO.new(n, pc.nd_value);
      n.pos:=ps;
      n.val:=pc.value.new(ps, pcO.ZZ_type);
      n.val.set_integer(0);
      n.type := pcO.longcard;
    |pcO.fn_inline, pcO.fn_setreg:
      pcS.error(ps, 'e', 342, pcS.name);
  <* END *>

    |pcO.fn_callerIPref:
      IF chk_no(0) THEN
        pcO.new(n,pc.nd_sproc);
        n.sub:=pc.sp_callerIPref;
        n.type:=pcO.proctype0;
        pcB.unary(n,pc.su_adr,pcO.addr);
        INCL(pcO.cur_scope.obj.xtags,pc.xot_noninlinable);
      END;
    |pcO.fn_excepttable:
      IF chk_no(0) THEN
        pcO.new(n,pc.nd_sproc);
        n.sub:=pc.sp_excepttable;
        n.type:=pcO.proctype0;
        pcB.unary(n,pc.su_adr,pcO.addr);
        INCL(pc.mods[pc.cur_mod].tags,pc.otag_haveExceptTable);
      END;
    ELSE pcS.error(ps,'e',200);
    END;
  END gen_function;

BEGIN
  ps:=n.pos;
  fn:=n.obj;
  n.mode:=pc.nd_inv;
  n.type:=pcO.invtype;
  n.obj:=NIL;
  gen_function(pcO.sproc_no(fn));
END gen_system_function;

END pcF.
