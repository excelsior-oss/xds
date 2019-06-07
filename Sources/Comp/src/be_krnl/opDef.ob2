<* NEW db_def- *>
<* NEW db_code- *>
<* NEW db_procs- *>

MODULE opDef;

IMPORT
<* IF db_def OR db_code THEN *>  io := opIO,  <* END *>
  pc  := pcK,                   env := xiEnv,
  ir,                            gr := ControlGraph,
  pr  := opProcs,                cd := CodeDef,
  at  := opAttrs,              tune := opTune,
  std := opStd,
  opt := Options,
  Calc,
  ObjNames,
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  TOC,
  TOCData,
<* END *>
  SYSTEM;

(** --------------------------------------------------------------------- *)

TYPE INT = ir.INT;
  TMP_VAR* = POINTER TO TMP_VAR_REC;

  TMP_VAR_REC = RECORD
    loc  : ir.Local;
    next : TMP_VAR;
  END;

  CNST_VAR = POINTER TO CNST_VAR_REC;

  CNST_VAR_REC = RECORD
    c_const: BOOLEAN;     (* константа Сишная *)
    type: pc.STRUCT;
    obj : pc.OBJECT;
    next: CNST_VAR;
  END;

CONST  (* ---- моды генерации ---- *)
  REF*  = ir.REF;    (* требуется адрес *)
  LVL*  = ir.LVL;    (* требуется l-value (адрес или объект) *)
  NEG*  = ir.NEG;
  CC*   = ir.CC;    (* генерировать C-шные строковые константы *)

  VOLATILE* = ir.VOLATILE;  (* возвращаемый в arg признак - вместе c REF *)

  VPTR*     = ir.VPTR;  (* SL-1 virtual pointer - возвращаемый признак *)

CONST
  SHORTCARD_BITS* = 8;
  bitset_sz = tune.BITSET_LEN DIV SHORTCARD_BITS;
  PROFL_size = 6 * tune.addr_sz ;

VAR
  zz_tmp* : pc.VALUE;
  bool0*  : pc.VALUE;  (* константа false *)
  bool1*  : pc.VALUE;  (* константа true  *)
  zz_zero*: pc.VALUE;  (* константа 0 *)
  zz_one* : pc.VALUE;  (* константа 1 *)
  rr_zero*: pc.VALUE;  (* константа 1.0 *)

  SIZE_T*       : pc.STRUCT;
  LSET_T*       : pc.STRUCT;
  SEQU_T*       : pc.STRUCT;
  ADDR_T*       : pc.STRUCT;  (* LAZ *)
  SETOP_TABLE_T : pc.STRUCT;  (* LAZ *)
  CARD8_T*      : pc.STRUCT;  (* LAZ *)
  INDEX_T*      : pc.STRUCT;  (* LAZ *)
  TDESC_T       : pc.STRUCT;  (* LAZ *)
  MDESC_T       : pc.STRUCT;  (* LAZ *)
  VOID_T*       : pc.STRUCT;  (* LAZ *)

  PROFL_T       : pc.STRUCT;  (* LAZ *)

  md_desc*   : pc.OBJECT;  (* дескриптор текущего модуля  *)
  last_type* : pc.OBJECT;  (* последний из тех типов, для *)
                           (* которых нужен дескриптор    *)
  cns_vars   : CNST_VAR;
  cns_cnt    : INT;
  tmp_vars*  : TMP_VAR;
  tmp_busy*  : TMP_VAR;
  ret_emul*  : BOOLEAN;

  gen_value* : PROCEDURE (n: pc.NODE; md: ir.GenModeSet; VAR arg: ir.Arg);

<* IF db_def THEN *>
PROCEDURE note(str-: ARRAY OF CHAR; o: pc.OBJECT);
BEGIN
  IF o = NIL THEN io.print("%s(NIL)\n", str)
  ELSE
    IF (o.host= NIL) OR (o.host.obj = NIL) THEN
      io.print('%s("?.%s")\n', str, o.name^);
    ELSIF o.host.obj.name = NIL THEN
      io.print('%s("%s")\n', str, o.name^);
    ELSE
      io.print('%s("%s.%s")\n', str, o.host.obj.name^, o.name^);
    END;
  END;
END note;
<* END *>

(** ------------------------ S C O P E S -------------------------------- *)

CONST
  local* = ir.TmpScope;            (* -1 for local variablies in a procedure *)
                    (* scope for global and external module variablies >= 0  *)

PROCEDURE scope_id*(o: pc.OBJECT): ir.ScopeType;
BEGIN
--<* IF db_def THEN *> note("scope_id", o); <* END *>
  IF o.mno = tune.x2c_mno THEN
    RETURN ir.ZEROScope
  ELSIF (o.mno # at.curr_mno) OR (o.mode = pc.ob_module) THEN
    ASSERT (o.mno >= pc.ZEROMno);
    RETURN VAL(ir.ScopeType, SYSTEM.SUCC(o.mno))
  ELSE ASSERT(o = at.curr_proc);
    RETURN local
  END;
END scope_id;

(* --------- d a t a   t y p e   s i z e s ---------------------- *)
PROCEDURE err_type_size (t: pc.STRUCT);
VAR
  s  : ARRAY 256 OF CHAR;
  pos: pc.TPOS;
BEGIN
  IF (t.obj # NIL) & ObjNames.valid_name(t.obj.name) THEN
    COPY (t.obj.name^, s);
  ELSIF t.mode IN pc.ARRs THEN
    s := "an unnamed ARRAY inside some type definition";
  ELSIF t.mode = pc.ty_record THEN
    s := "an unnamed RECORD inside some type definition";
  ELSE
    s := "an unnamed type inside some type definition";
  END;
  pos := t.pos;
  IF pos.IsNull() AND (t.obj#NIL) THEN
    pos := t.obj.pos;
  END;
  env.errors.Error(pos,1014,s);
END err_type_size;

             (* actual_alignment(T) may differ from get_align(T), *)
             (* but always   actual_alignment(T) <= get_align(T)  *)
PROCEDURE get_align * (t: pc.STRUCT) : SHORTINT;
BEGIN
  IF t.align = 0 THEN
    RETURN at.default_alignment
  ELSE
    RETURN t.align
  END;
END get_align;

PROCEDURE mk_align(VAR offs: LONGINT; align: SHORTINT);
  VAR r: LONGINT;
BEGIN
  r := offs MOD align;
  IF r # 0 THEN INC(offs, align-r) END;
END mk_align;

<* IF TARGET_RISC OR TARGET_SPARC THEN *>

PROCEDURE mk_neg_align(VAR offs: LONGINT; align: SHORTINT);
  VAR r: LONGINT;
BEGIN
  r := offs MOD align;
  IF r # 0 THEN DEC(offs, r) END;
END mk_neg_align;

<* END *>

PROCEDURE set_size(t: pc.STRUCT; size: LONGINT);
  VAR ext: at.SIZE_EXT;
BEGIN
  ASSERT(NOT (at.tmark_processed IN t.marks));
  NEW(ext); ext.size := size;
  at.app_struct_attr(t, ext, at.a_size);
  INCL(t.marks, at.tmark_processed);
END set_size;

PROCEDURE ^ record_size (t: pc.STRUCT; VAR sz: LONGINT; VAR align: SHORTINT);
PROCEDURE ^ type_definition (o: pc.OBJECT);

PROCEDURE bytes(t : pc.STRUCT; VAR sz : LONGINT; VAR align : SHORTINT);
  VAR n_align, t_align : SHORTINT;
BEGIN
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
      sz      := tune.real_sz;
      n_align := tune.real_sz;
  | pc.ty_longreal :
      sz      := tune.longreal_sz;
      n_align := tune.longreal_sz;
  | pc.ty_ld_real :
      sz      := tune.IDB.ld_real_sz;
      n_align := tune.longreal_sz;   (*!?*)
  | pc.ty_complex :
      sz      := 2 * tune.real_sz;
      n_align := tune.real_sz;
  | pc.ty_lcomplex :
      sz      := 2 * tune.longreal_sz;
      n_align := tune.longreal_sz;
  | pc.ty_boolean, pc.ty_range, pc.ty_enum, pc.ty_protection :
      n_align := t_align;
      bytes(t.base, sz, n_align);
  | pc.ty_char :
      sz      := tune.char_sz;
      n_align := tune.char_sz;
  | pc.ty_opaque, pc.ty_pointer, pc.ty_AA :
      sz      := tune.addr_sz;
      n_align := tune.addr_sz;
  | pc.ty_set :
      IF t.inx#NIL THEN
        n_align := t_align;
        bytes(t.inx, sz, n_align);
      ELSIF t.len >= tune.BITSET_LEN THEN
        sz := ((t.len -1) DIV tune.BITSET_LEN +1)* bitset_sz;
        n_align := bitset_sz;
      ELSIF t.len <= 8 THEN
        sz      := 1;
        n_align := 1;
      ELSIF t.len <=16 THEN
        sz      := 2;
        n_align := 2;
      ELSE
        sz      := 4;
        n_align := 4;
      END;
  | pc.ty_proctype :
      sz      := tune.proc_sz;
      n_align := tune.proc_sz;
  | pc.ty_array :
      bytes(t.base, sz, align);
      IF sz > 0 THEN
        IF t.len <= MAX(LONGINT) DIV sz THEN
          sz := sz * t.len;
        ELSE
          sz := MAX(LONGINT)-1;
        END;
      END;
      RETURN
  | pc.ty_record :
      record_size(t, sz, align); RETURN
  | pc.ty_loc :
      sz      := 1;
      n_align := 1;
  | pc.ty_SS :
      sz      := tune.char_sz * t.len;
      n_align := tune.char_sz;
  | pc.ty_process :
      sz      := tune.process_size;
      n_align := 4;
  | pc.ty_array_of :
      bytes(t.base, sz, align);
      sz := -1;
      RETURN
  ELSE
    sz    := 0  (*sz := -1*);
    align := 1;
    RETURN
  END;
  IF t_align > n_align THEN t_align := n_align END;
  IF t_align < align   THEN align   := t_align END;
END bytes;

PROCEDURE type_size* (t: pc.STRUCT): LONGINT;
  VAR sz: LONGINT; align: SHORTINT;
BEGIN
  align := at.default_alignment;
  bytes(t, sz, align);
  RETURN sz
END type_size;

PROCEDURE type_align* (t: pc.STRUCT): SHORTINT;
  VAR sz: LONGINT; t_align : SHORTINT;
BEGIN
  t_align := t.align;
  IF t_align = 0 THEN t_align := at.default_alignment END;
  bytes(t, sz, t_align);
  RETURN t_align
END type_align;

PROCEDURE type_kind* (t: pc.STRUCT): ir.TypeType;
BEGIN
  CASE t.mode OF
    |pc.ty_shortcard
    ,pc.ty_cardinal
    ,pc.ty_longcard
    ,pc.ty_longlongcard
                     : RETURN ir.t_unsign
    |pc.ty_shortint
    ,pc.ty_integer
    ,pc.ty_longint
    ,pc.ty_longlongint
                     : RETURN ir.t_int
    |pc.ty_real
    ,pc.ty_longreal
    ,pc.ty_ld_real
                     : RETURN ir.t_float
    |pc.ty_complex
    ,pc.ty_lcomplex
                     : RETURN ir.t_complex
    |pc.ty_boolean   : RETURN tune.bool_ty
    |pc.ty_char      : RETURN tune.char_ty
    |pc.ty_range     : RETURN type_kind(t.base)
    |pc.ty_enum      : RETURN type_kind(t.base)
    |pc.ty_pointer
    ,pc.ty_opaque
    ,pc.ty_AA
                     : RETURN ir.t_ref
    |pc.ty_set       : IF t.len <= tune.BITSET_LEN_scalar THEN RETURN ir.t_unsign
                       ELSE                             RETURN ir.t_arr
                       END;
    |pc.ty_proctype  : RETURN ir.t_ref
    |pc.ty_array     : IF (t.base.mode = pc.ty_loc)
                        & ((t.len=2)OR(t.len=4)) THEN RETURN ir.t_unsign
                       ELSE                           RETURN ir.t_arr
                       END;
    |pc.ty_array_of
    ,pc.ty_SS
                     : RETURN ir.t_arr
    |pc.ty_record    : RETURN ir.t_rec
    |pc.ty_loc       : RETURN ir.t_unsign
    |pc.ty_protection: RETURN tune.protect_ty
    |pc.ty_void      : RETURN ir.t_void
    |pc.ty_process   : RETURN tune.process_ty;
  ELSE
     env.errors.Fault(t.pos, 961, t.mode);   --- invalid type mode (%d)
     RETURN ir.t_int
  END;
END type_kind;

PROCEDURE type_info* (t: pc.STRUCT; VAR ty: ir.TypeType; VAR sz: ir.SizeType);
BEGIN
 ty := type_kind(t);
 sz := VAL( ir.SizeType, type_size(t));
END type_info;

PROCEDURE index_type_info* (t: pc.STRUCT; VAR ty: ir.TypeType; VAR sz: ir.SizeType);
BEGIN
  IF t.mode = pc.ty_ZZ THEN
    ty := ir.t_int;
    sz := ir.BiggestIntSize;
  ELSE
    ty := type_kind(t);
    sz := VAL( ir.SizeType, type_size(t));
  END;
END index_type_info;

PROCEDURE re_im_sz*(t: pc.STRUCT): ir.SizeType;    (* размер соотв. float *)
BEGIN
  IF t.mode = pc.ty_complex THEN
    RETURN tune.real_sz
  END;
  ASSERT(t.mode =pc.ty_lcomplex);
  RETURN tune.longreal_sz;
END re_im_sz;

PROCEDURE re_im_offs*(t: pc.STRUCT;          (* комплексный тип *)
           VAR re_offs, im_offs: LONGINT);   (* могут зависеть от архитектуры *)
  VAR sz: ir.SizeType;
BEGIN
  IF t.mode = pc.ty_complex THEN
    sz := tune.real_sz
  ELSE ASSERT(t.mode = pc.ty_lcomplex);
    sz :=  tune.longreal_sz;
  END;
  re_offs := 0;
  im_offs := sz;
END re_im_offs;

PROCEDURE is_scalar*(t: pc.STRUCT): BOOLEAN;
   (* значение такого типа "напрямую" присваивается  *)
   (*  и возвращается как результат функции          *)
   (* в принципе, возможно включение сюда "коротких" массивов и записей *)
BEGIN
  IF (t.mode IN (pc.NUMs + pc.ADRs
              +pc.TY_SET{pc.ty_boolean, pc.ty_char, pc.ty_range, pc.ty_enum
               ,pc.ty_opaque, pc.ty_proctype, pc.ty_protection,
                pc.ty_longlongint,pc.ty_longlongcard})) OR (t.mode = pc.ty_loc)
  THEN
    RETURN TRUE
  ELSIF t.mode = pc.ty_set THEN
    RETURN (t.len <= tune.BITSET_LEN_scalar)
  ELSIF (t.mode = pc.ty_array) & (t.base.mode = pc.ty_loc) & (t.len = 4)
<* IF env_target="x86linux" THEN *> & (t.mno < 0) <* END *>
  THEN
    RETURN TRUE                           (* -- this type is SYSTEM.WORD *)
  ELSE
    RETURN FALSE
  END;
END is_scalar;

(*
PROCEDURE is_scalar_param*(t: pc.STRUCT): BOOLEAN;
   (* значение такого типа "напрямую" передается параметром-значением   *)
   (* в принципе, возможно включение сюда "коротких" массивов и записей *)
BEGIN
  RETURN t.mode IN (pc.CNUMs + pc.ADRs
                   +{pc.ty_boolean,pc.ty_char,pc.ty_range,    pc.ty_enum
                    ,pc.ty_opaque, pc.ty_set, pc.ty_proctype, pc.ty_loc,
                     pc.ty_protection});
END is_scalar_param;
*)

PROCEDURE mod_desc(): pc.OBJECT;
  VAR nm: pc.STRING;
BEGIN
  IF md_desc = NIL THEN
    nm := at.make_name("%s'desc", at.curr_mod.name^);
    md_desc := at.new_work_object(nm, MDESC_T, at.curr_mod.type, pc.ob_cons, FALSE);
   <* IF TARGET_RISC THEN *>
    TOC.Add(md_desc);
   <* END *>
  END;
  RETURN md_desc
END mod_desc;

<* IF TARGET_RISC OR TARGET_SPARC THEN *>
PROCEDURE register_module_base(mno: pc.Mno);
  VAR md, o: pc.OBJECT;
    pass: SHORTINT;
BEGIN
  md := pc.mods[mno];
  IF at.omark_allocated IN md.marks THEN RETURN END;
  o := md.type.prof;
  pass := 0;
  LOOP
    IF o = NIL THEN INC(pass);
      IF pass = 1 THEN ASSERT(mno=at.curr_mno);
        o := md.type.mem;
      ELSE ASSERT(pass = 2);
        o := at.work_objects;
      END
    END;
    IF ((o.mode = pc.ob_var) OR (o.mode = pc.ob_cons))
      & (at.omark_allocated IN o.marks)
      & (at.get_global_offset(o) = 0)
    THEN
      EXIT
    END;
    o := o.next;
  END;
  TOC.AddBase(o);
  INCL(md.marks, at.omark_allocated);
END register_module_base;
<* END *>

PROCEDURE work_variable(o: pc.OBJECT) : BOOLEAN;
BEGIN
  RETURN (o.mode = pc.ob_var) & NOT ObjNames.valid_name(o.name)
END work_variable;

<* IF env_target = "x86nt" THEN *>
  (*
    XDS feature to export variables from DLL (that's needed if executable
    format is PE
  *)
PROCEDURE from_other_DLL (o: pc.OBJECT) : BOOLEAN;
BEGIN
  RETURN ((o.mode=pc.ob_var) OR (o.mode=pc.ob_type)) & (o.lev = 0) &
         at.get_dllexported(o) & ~at.isSameDLL(o)
END from_other_DLL;

<* END *>

PROCEDURE make_local(o: pc.OBJECT);
  VAR l: ir.Local; a: at.ATTR_EXT;
    arg: ir.Arg;
    type: pc.STRUCT;
    ty: ir.TypeType; sz: LONGINT; align : SHORTINT;
    scope : ir.ScopeType;
    e_tag: ir.TagType; name: ir.VarNum; val: pc.VALUE; offs: LONGINT;
BEGIN
  IF (o.mode = pc.ob_var) & (o.attr # NIL) THEN
(*
 pcVis.print("----------- make_local: %s ----------------\n", o.name^);
 pcVis.vis(o.val, 1);
 pcVis.print("---------------------------------------------\n");
*)
    gen_value(o.attr(pc.NODE), ir.GenModeSet{}, arg);
    e_tag := at.BASE + arg.tag;
    CASE arg.tag OF
    | ir.y_NumConst:
       name := ir.UNDEFINED;
       val := arg.value;
       offs := 0;
    | ir.y_AddrConst:
       name := arg.name;
       val := arg.value;
       offs := arg.offset;
    | ir.y_ProcConst
    , ir.y_RealVar
    , ir.y_Variable:
       name := arg.name;
       val := NIL;
       offs := 0;
    ELSE
      env.errors.Error(o.attr(pc.NODE).pos, 200); --"Can not generate address of %s", o.name^);
      e_tag := at.BASE + ir.y_NumConst;
      name := ir.UNDEFINED;
      val := zz_zero;
      offs := 0;
    END;
    a := at.attr(o.ext, at.a_self);
    IF a # NIL THEN
      WITH a : at.INFO_EXT DO
        a.procno := at.curr_procno;
        a.e_tag := e_tag;
        a.name  := VAL(at.InfExtName, name);
        a.value := val;
        a.offs  := offs;
      END;
    ELSE at.app_info(o, at.a_self, e_tag, VAL(at.InfExtName, name), val, offs);
    END;
  ELSE
    type := o.type;
    align := at.default_alignment;
    bytes(type, sz, align);
    ty := type_kind(type);
    IF work_variable(o) THEN scope := local;
    ELSE scope := scope_id(o.host.obj)
    END;
    e_tag := ir.y_RealVar;
   <* IF env_target = "x86nt" THEN *>
    IF from_other_DLL (o) THEN
      e_tag := at.BASE + ir.y_RealVar;
      ty := tune.addr_ty;
      sz := tune.addr_sz;
      align := tune.addr_sz;
    END;
   <* END *>

    l := ir.AddLocal(o.name, scope, align);
    ir.Locals[l].Obj := o;
    ir.Locals[l].VarType := ty;
    ir.Locals[l].VarSize := sz;
    IF (e_tag = ir.y_RealVar)
      & ((pc.otag_volatile IN o.tags) OR (pc.ttag_volatile IN type.tags))
    THEN
      INCL(ir.Locals[l].Options, ir.o_Volatile)
    END;
    a := at.attr(o.ext, at.a_self);
    IF a # NIL THEN
      WITH a : at.INFO_EXT DO
        a.procno := at.curr_procno;
        ir.Locals[l].Offset := a.offs;
        a.e_tag := e_tag;
        a.name  := VAL(at.InfExtName, l);
        a.offs  := 0;
      END;
    ELSE at.app_info(o, at.a_self, e_tag, VAL(at.InfExtName, l), NIL, 0);
    END;
  <* IF TARGET_RISC OR TARGET_SPARC THEN *>
    IF ir.IsExternal(l) THEN
      IF at.omark_allocated IN o.marks THEN
        ir.Locals[l].Offset := at.get_global_offset(o);
        register_module_base(o.mno);
      ELSE TOC.Add(o);
      END;
    END;
  <* END *>
  END;
END make_local;

PROCEDURE use_temp*(o: pc.OBJECT; t: ir.VarNum);
BEGIN
  ASSERT(o.mode = pc.ob_var);
  at.app_info(o, at.a_self, ir.y_Variable, VAL(at.InfExtName, t), NIL, 0);
END use_temp;

PROCEDURE NewTriadeTS*(nparams: ir.INT; op: ir.Operation;
                                         t: pc.STRUCT): ir.TriadePtr;
  VAR ty: ir.TypeType; sz: ir.SizeType;
BEGIN
  type_info(t, ty, sz);
  RETURN ir.NewTriadeInit(nparams, op, ty, sz);
END NewTriadeTS;

(** --------------------------------------------------------------------- *)

PROCEDURE ^ object_declaration*(o: pc.OBJECT);

PROCEDURE ^ object_definition(o: pc.OBJECT);

PROCEDURE ^ const_aggregate*(v: pc.VALUE; t: pc.STRUCT; md: ir.GenModeSet;
                                                   VAR arg: ir.Arg);

(** ------------ P R O C   b a s e s --------------------------------- *)

PROCEDURE make_attr(o: pc.OBJECT; kind: SHORTINT): at.ATTR_EXT;
BEGIN
<* IF db_def THEN *>
  note("make_attr", o); io.print('     kind=%d\n', kind);
  IF at.otag_declared IN o.tags THEN io.print('declared\n') END;
<* END *>
  ASSERT(kind = at.a_self);
  IF NOT (at.otag_declared IN o.tags) THEN
    IF (o.mode # pc.ob_module)
      &(o.host.mode IN pc.TY_SET{pc.ty_enum,pc.ty_record})
      & NOT (o.mode IN pc.PROCs)
    THEN (* объект объявляется описанием его типа *)
      object_definition(o.host.obj);
      ASSERT(at.otag_declared IN o.tags);
    ELSE
      object_declaration(o);
    END;
  END;
  RETURN at.attr(o.ext, kind)
(* -- этого не бывает ??
  | a_size        (* размер типа *)                    (* size_ext *)
  | a_prot        (* номер прототипа *)                (* prot_ext *)
  | a_type:                                            (* inf_ext  *)
--- *)
END make_attr;


PROCEDURE is_reference (o: pc.OBJECT; scp: pc.OBJECT): BOOLEAN;
BEGIN
  IF (scp#NIL) & (o.lev>0) & (o.lev<=scp.lev) THEN
    IF (o.mode=pc.ob_varpar) THEN
       RETURN TRUE
     END;
    IF (o.mode=pc.ob_var) THEN
      IF (o.type.mode=pc.ty_array_of) THEN RETURN TRUE END;
      IF (pc.OTAG_SET{pc.otag_valpar, pc.otag_RO}*o.tags=pc.OTAG_SET{pc.otag_valpar, pc.otag_RO})
        & NOT is_scalar(o.type)
      THEN
        RETURN TRUE
      END;
    END;
  END;
  RETURN FALSE
END is_reference;


PROCEDURE deref*(pos-: pc.TPOS;
                   ty: ir.TypeType; sz: ir.SizeType;
                 opts: ir.OptionsSet;
              VAR arg: ir.Arg);
  VAR q: ir.TriadePtr;
BEGIN
  IF ty = ir.t_float THEN at.was_float_triade := TRUE END;
  IF VOLATILE IN arg.mode THEN INCL(opts, ir.o_Volatile) END;
  q := ir.NewTriadeInit(1, ir.o_loadr, ty, sz);
  q.Position := pos;
  q.Options := q.Options + opts;
  ir.ParmByArg(q.Params[0], arg);
  ir.GenResVar(q);
  gr.AppendTr(q);
  ir.MakeArgVar(arg, q.Name);
END deref;

PROCEDURE chk_adr*(tpos-: ir.TPOS;
                      md: ir.GenModeSet; type: pc.STRUCT;
                 VAR arg: ir.Arg);
  VAR ty: ir.TypeType; sz: ir.SizeType;
BEGIN
  IF pc.ttag_volatile IN type.tags THEN INCL(arg.mode, VOLATILE) END;
  IF    REF IN md THEN (* ничего *)
  ELSIF LVL IN md THEN INCL(arg.mode, REF);
  ELSE
    type_info(type, ty, sz);
    deref(tpos, ty, sz, ir.OptionsSet{}, arg);
  END;
END chk_adr;

PROCEDURE chk_volatile(o: pc.OBJECT; VAR arg: ir.Arg);
BEGIN
  IF (pc.otag_volatile IN o.tags) OR (pc.ttag_volatile IN o.type.tags) THEN
    INCL(arg.mode, VOLATILE);
  END;
END chk_volatile;

PROCEDURE add_offs*(tpos-: ir.TPOS; offs: LONGINT; VAR arg: ir.Arg);
  VAR q: ir.TriadePtr; volatile: BOOLEAN;
BEGIN
  IF offs # 0 THEN
    IF arg.tag = ir.y_AddrConst THEN
      arg.offset := arg.offset + offs;
    ELSE
      volatile := VOLATILE IN arg.mode;
      q := ir.NewTriadeInit(2, ir.o_add, tune.addr_ty, tune.addr_sz);
      q.Position := tpos;
      ir.ParmByArg(q.Params[0], arg);
      ir.MakeParNum(q.Params[1], Calc.NewInteger(offs, tune.addr_sz));
      ir.GenResVar(q);
      gr.AppendTr(q);
      ir.MakeArgVar(arg, q.Name);
      IF volatile THEN INCL(arg.mode, VOLATILE) END;
    END;
  END;
END add_offs;

PROCEDURE o_attr*(tpos-: ir.TPOS;
                      o: pc.OBJECT; kind: SHORTINT; md: ir.GenModeSet;
                VAR arg: ir.Arg);
  VAR type: pc.STRUCT;
    a: at.ATTR_EXT;  inf: at.INFO_EXT;
BEGIN
 (*
   если объект o является параметром - гибким массивом по значению,
   то возможны проблемы при доступе к нему из вложенной процедуры -
   необходимо еще одно разыменование
 *)
<* IF db_def THEN *>
  io.print("o_attr(o='%s', kind=%d, md=%x, arg)\n", o.name^, kind, md);
<* END *>
  a := at.attr(o.ext, kind);
  IF a = NIL THEN
    a   := make_attr(o, kind);
    inf := a(at.INFO_EXT);
  ELSE
    inf := a(at.INFO_EXT);
    IF inf.procno # at.curr_procno THEN
      IF (o.mode = pc.ob_var) OR (o.mode = pc.ob_cons)
         OR (o.mode = pc.ob_type)
      THEN
        ASSERT(kind = at.a_self);
        at.del_attr(o.ext, kind);
        EXCL(o.tags, at.otag_declared);
        a   := make_attr(o, kind);
        inf := a(at.INFO_EXT);
      END;
    END;
  END;
--  io.print("inf.e_tag = %d\n", inf.e_tag);
  IF inf.e_tag < at.BASE THEN
    CASE inf.e_tag OF
    | ir.y_NumConst
    , ir.y_RealConst:
        const_aggregate(inf.value, o.type, md, arg);
    | ir.y_AddrConst:
        IF inf.name = MAX(at.InfExtName) THEN
          ir.MakeArgAddrConst(arg, inf.value, inf.offs);
        ELSE
          ir.MakeArgAddr(arg, VAL(ir.VarNum, inf.name), inf.offs);
        END;
        chk_adr(tpos, md, o.type, arg);
    | ir.y_ProcConst:
        ASSERT(md=ir.GenModeSet{}); ir.MakeArgProc(arg, VAL(ir.ProcNum, inf.name));
    | ir.y_RealVar:
        IF REF IN md THEN ir.MakeArgAddr(arg, VAL(ir.VarNum, inf.name), 0);
        ELSE ir.MakeArgLocal(arg, VAL(ir.VarNum, inf.name),0);
        END;
    | ir.y_Variable:
        ASSERT(md=ir.GenModeSet{});
        ir.MakeArgVar(arg, VAL(ir.VarNum, inf.name));
    END;
  ELSE
    CASE (inf.e_tag - at.BASE) OF
    | ir.y_Nothing:
        ir.MakeArgAddr(arg, MAX(ir.Local), inf.offs);
        RETURN
    | ir.y_NumConst:
        ir.MakeArgNum(arg, inf.value);
        add_offs(tpos, inf.offs, arg);
    | ir.y_AddrConst:
        IF inf.name = MAX(at.InfExtName) THEN
          ir.MakeArgAddrConst(arg, inf.value, inf.offs);
        ELSE
          ir.MakeArgAddr(arg, VAL(ir.VarNum, inf.name), inf.offs);
        END;
    | ir.y_RealVar:
        ir.MakeArgLocal(arg, VAL(ir.VarNum, inf.name),0);
        add_offs(tpos, inf.offs, arg);
    | ir.y_Variable:
        ir.MakeArgVar(arg, VAL(ir.VarNum, inf.name));
(*  | y_RealConst, y_ComplexConst, y_ProcConst:  --  не бывает *)
    END;
    IF kind >= at.a_len THEN
      type := INDEX_T;
    ELSIF (kind >= at.a_base) OR (kind = at.a_type) THEN
      type := ADDR_T;
    ELSE
      type := o.type;
      IF is_reference(o, at.curr_proc) THEN
        <* IF db_def THEN *> io.print("is_reference(%s)\n", o.name^);<* END *>
        chk_volatile(o, arg);
        deref(tpos, tune.addr_ty, tune.addr_sz, ir.OptionsSet{}, arg)
     <* IF env_target = "x86nt" THEN *>
      ELSIF from_other_DLL(o) THEN
        chk_volatile(o, arg);   ---- !!!
     <* END *>
      END;
    END;
    chk_adr(tpos, md, type, arg);
  END;
END o_attr;

PROCEDURE o_usage*(tpos-: ir.TPOS; o: pc.OBJECT; md: ir.GenModeSet; VAR arg: ir.Arg);
BEGIN
  o_attr(tpos, o, at.a_self, md, arg);
END o_usage;

(* по N - номеру rts-процедуры создает триаду вызова и подвешивает
   к триаде 0-ой параметр - саму процедуру
*)
PROCEDURE RTS_call*(tpos-: ir.TPOS; N : INT; make_var: BOOLEAN) : ir.TriadePtr;
  VAR o : pc.OBJECT;
    arg: ir.Arg;
    q : ir.TriadePtr;
    proto_num: pr.ProtoNum;  proto: pr.Proto;
BEGIN
  o := std.Proc(N);
 <* IF TARGET_RISC OR TARGET_SPARC THEN *>
  --TOC.Add(o);
 <* END *>
  o_usage(tpos, o, ir.GenModeSet{}, arg);
  ASSERT(arg.tag = ir.y_ProcConst);
  proto_num := pr.ProcProtoNum(VAL(pr.ProcNum,arg.name));
  proto     := pr.ProtoList[proto_num];
  q := ir.NewTriadeInit(proto.npar+1, ir.o_call, proto.ret_type, proto.ret_size);
  q.Position := tpos;
  q.Prototype := proto_num;
  ir.ParmByArg(q.Params[0], arg);
  IF make_var AND (proto.ret_type # ir.t_void) THEN ir.GenResVar(q) END;
  RETURN q
END RTS_call;

PROCEDURE obj_offset * (o: pc.OBJECT): LONGINT;
  VAR a: at.ATTR_EXT;  inf: at.INFO_EXT;
BEGIN
  a := at.attr(o.ext, at.a_self);
  IF a = NIL THEN
    a := make_attr(o, at.a_self);
  END;
  inf := a(at.INFO_EXT);
  RETURN inf.offs
END obj_offset;

PROCEDURE is_char*(t: pc.STRUCT): BOOLEAN;
BEGIN
  IF t.mode = pc.ty_range THEN t := t.base END;
  RETURN (t.mode = pc.ty_char);
END is_char;

PROCEDURE t_def*(t: pc.STRUCT);
BEGIN
  IF (t.obj=NIL) OR (t.obj.mode#pc.ob_type) THEN RETURN END;
  IF (at.otag_declared IN t.obj.tags) &
     NOT (at.otag_undef IN t.obj.tags) THEN RETURN END;
  object_definition(t.obj);
  ASSERT(NOT (at.otag_undef IN t.obj.tags));
END t_def;

PROCEDURE c_number*(n: LONGINT; VAR arg: ir.Arg);
  VAR val: pc.VALUE;
BEGIN
  val := pc.value.new (env.null_pos, pc.ZZ_type);
  val.set_integer(n);
  ir.MakeArgNum(arg, val);
END c_number;

PROCEDURE put_procedure(p: pc.OBJECT);
BEGIN
  IF p = NIL THEN cd.put_nil_value;
  ELSE
 <* IF TARGET_RISC THEN *>
    object_declaration(p);
    cd.gen_fixup(pr.ProcDesc(p), 0, cd.fx_obj32);
 <* ELSE *>
    cd.gen_fixup(p, 0, cd.fx_obj32);
 <* END *>
  END;
END put_procedure;

PROCEDURE search_record_variant * (VAR l: pc.NODE; v: pc.VALUE);
  VAR e, m: pc.NODE;
BEGIN
  e := NIL;
  LOOP
    IF l = NIL THEN EXIT END;
    ASSERT(l.mode = pc.nd_node);
    m := l.l;
    IF m = NIL THEN e := l END;
    LOOP
      IF m = NIL THEN EXIT END;
      IF m.mode = pc.nd_pair THEN
        zz_tmp.binary(pc.sb_geq, v, m.val);
        IF NOT zz_tmp.is_zero() THEN
          zz_tmp.binary(pc.sb_leq, v, m.l.val);
          IF NOT zz_tmp.is_zero() THEN EXIT END;
        END;
      ELSE
        ASSERT(m.mode = pc.nd_value);
        zz_tmp.binary(pc.sb_equ, v, m.val);
        IF NOT zz_tmp.is_zero() THEN EXIT END;
      END;
      m := m.next;
    END;
    IF m # NIL THEN EXIT END;
    l := l.next;
  END;
  IF l = NIL THEN l := e END;
END search_record_variant;

PROCEDURE put_pad(align: SHORTINT; VAR sg_len: LONGINT);
BEGIN
  WHILE (sg_len MOD align) # 0 DO cd.GenByte(0X); INC(sg_len) END;
END put_pad;

PROCEDURE put_filling(sz: LONGINT; VAR sg_len: LONGINT);
BEGIN
  ASSERT(sz>0);
  WHILE sz >= 4 DO cd.GenLWord(0); INC(sg_len, 4); DEC(sz,4) END;
  WHILE sz > 0 DO cd.GenByte(0X); INC(sg_len); DEC(sz) END;
END put_filling;

PROCEDURE put_str(v: pc.VALUE; len: LONGINT; VAR sg_len: LONGINT);
  VAR i: LONGINT; ch: CHAR;
BEGIN
  cd.GenStartString;
  FOR i := 0 TO len - 1 DO
    zz_tmp.index_get(i, v);
    ch := CHR(zz_tmp.get_integer());
    cd.GenStringChar(ch);
  END;
  cd.GenEndString;
  sg_len := sg_len + len;
END put_str;

PROCEDURE ^ put_value(v: pc.VALUE; t: pc.STRUCT; c_c: BOOLEAN; VAR sg_len: LONGINT);

PROCEDURE put_array(v: pc.VALUE; t: pc.STRUCT; VAR sg_len: LONGINT);
  VAR i: LONGINT; v1: pc.VALUE;
    base: pc.STRUCT;  b_align: SHORTINT;
BEGIN
  ASSERT(t.mode IN pc.ARRs);
  base := t.base;
  b_align := type_align(base);
  v1 := pc.value.new(v.pos, base);
  put_pad(b_align, sg_len);
  FOR i:=0 TO t.len-1 DO
    v1.index_get(i, v);
    put_value(v1, base, FALSE, sg_len);
    put_pad(b_align, sg_len);
  END;
END put_array;

PROCEDURE put_C_str(v: pc.VALUE; t: pc.STRUCT; VAR sg_len: LONGINT);
  CONST NL  = 0AX;
        HT  = 09X;
        VT  = 0BX;
        BS  = 08X;
        CR  = 0DX;
        FF  = 0CX;
        BEL = 07X;
  VAR i: LONGINT;
    ch : CHAR; x : INT;
    bslash, oct, hex: BOOLEAN;
BEGIN
  ASSERT(t.mode = pc.ty_SS);
  cd.GenStartString;
  bslash := FALSE; oct := FALSE; hex := FALSE; x := 0;
  FOR i := 0 TO t.len-1 DO
    zz_tmp.index_get(i, v);
    ch := CHR(zz_tmp.get_integer());
    IF bslash THEN
      CASE ch OF
      |'n':      cd.GenStringChar(NL); INC(sg_len);
      |'t':      cd.GenStringChar(HT); INC(sg_len);
      |'v':      cd.GenStringChar(VT); INC(sg_len);
      |'b':      cd.GenStringChar(BS); INC(sg_len);
      |'r':      cd.GenStringChar(CR); INC(sg_len);
      |'f':      cd.GenStringChar(FF); INC(sg_len);
      |'a':      cd.GenStringChar(BEL); INC(sg_len);
      |'0'..'7': oct := TRUE; x := ORD(ch)-ORD('0');
      |"x":      hex := TRUE; x := 0;
      ELSE cd.GenStringChar(ch); INC(sg_len);
      END;
      bslash := FALSE;
    ELSIF hex THEN
      IF    ('0'<=ch)&(ch <= '9') THEN x := x * 10 + ORD(ch)-ORD('0');
      ELSIF ('A'<=ch)&(ch <= 'Z') THEN x := x * 10 + ORD(ch)-ORD('A');
      ELSIF ('a'<=ch)&(ch <= 'z') THEN x := x * 10 + ORD(ch)-ORD('a');
      ELSE cd.GenStringChar(CHR(x)); INC(sg_len); hex := FALSE;
      END;
      IF x > ORD(MAX(CHAR)) THEN x := x MOD (ORD(MAX(CHAR))+1) END;
    ELSIF oct THEN
      IF    ('0'<=ch)&(ch <= '7') THEN x := x * 8 + ORD(ch)-ORD('0');
      ELSE cd.GenStringChar(CHR(x)); INC(sg_len); oct := FALSE;
      END;
      IF x > ORD(MAX(CHAR)) THEN x := x MOD (ORD(MAX(CHAR))+1) END;
    ELSIF ch = '\' THEN bslash := TRUE;
    ELSE cd.GenStringChar(ch); INC(sg_len);
    END;
  END (*LOOP*);
  IF    bslash     THEN cd.GenStringChar('\'); INC(sg_len);
  ELSIF hex OR oct THEN cd.GenStringChar(CHR(x)); INC(sg_len);
  END;
  cd.GenEndString;
END put_C_str;

PROCEDURE put_record(v: pc.VALUE; t: pc.STRUCT; VAR sg_len: LONGINT);
  VAR n : LONGINT;
    start : LONGINT;

  PROCEDURE put_fld(f: pc.OBJECT);
    VAR w: pc.VALUE;
     offs, curr_offs : LONGINT;
  BEGIN
    ASSERT (f.mode # pc.ob_header);
    offs := obj_offset(f);
    curr_offs := sg_len - start;
    IF offs > curr_offs THEN   (* -- нужно выравнивание на начало поля *)
      put_filling (offs - curr_offs, sg_len)
    ELSE ASSERT (offs = curr_offs);
    END;
    w := pc.value.new(v.pos, f.type);
    w.index_get(n, v);
    put_value(w, f.type, FALSE, sg_len);
    INC(n);
  END put_fld;

  PROCEDURE put_fld_seq(f: pc.OBJECT);
    VAR l: pc.NODE; w: pc.VALUE;
  BEGIN
    WHILE f # NIL DO
      IF f.mode = pc.ob_header THEN
        ASSERT(f.val.mode = pc.nd_case);
        l := f.val.l;
        w := pc.value.new(v.pos, f.type);
        w.index_get(n, v);
        search_record_variant(l, w);
        ASSERT(l#NIL);
        IF f.val.obj # NIL THEN put_fld(f.val.obj) ELSE INC(n) END;
        put_fld_seq(l.obj);
      ELSE
        put_fld(f);
      END;
      f := f.next;
    END;
  END put_fld_seq;

  PROCEDURE put_level(t: pc.STRUCT);
  BEGIN
    IF t.base # NIL THEN put_level(t.base) END;
    put_fld_seq(t.prof);
  END put_level;

  VAR rem : LONGINT;

BEGIN
  n := 0;
  start := sg_len;
  put_level(t);
  rem := type_size(t) + start - sg_len;
  IF rem > 0 THEN
    put_filling(rem, sg_len);
  ELSE
    ASSERT (rem = 0)
  END;
END put_record;

PROCEDURE put_lset(v: pc.VALUE; t: pc.STRUCT; VAR sg_len: LONGINT);
  VAR i,j, k, len, el_sz: LONGINT; s: SET;
BEGIN
  len := t.len;
  k := type_size(t);
  IF k < bitset_sz THEN el_sz := k;
  ELSE                  el_sz := bitset_sz;
  END;
  i := 0; j := 0; s := {};
  WHILE k > 0 DO
    IF i < len THEN
      zz_tmp.index_get(i, v);
      IF NOT zz_tmp.is_zero() THEN INCL(s,j) END;
    END;
    INC(i);
    INC(j);
    IF j = el_sz*8 THEN
      CASE el_sz OF
      | 1: cd.GenByte (SYSTEM.VAL(SYSTEM.BYTE, s));
      | 2: cd.GenWord (SYSTEM.VAL(SYSTEM.INT16, s));
      | 4: cd.GenLWord (SYSTEM.VAL(SYSTEM.INT32, s));
      END;
      s := {}; j := 0;
      INC(sg_len, el_sz);
      DEC(k, el_sz);
    END;
  END;
END put_lset;

PROCEDURE put_value (v: pc.VALUE;
                     t: pc.STRUCT;
                   c_c: BOOLEAN;
            VAR sg_len: LONGINT);
VAR
  sz: ir.SizeType;
  ty: ir.TypeType;
BEGIN
--  io.print("------- put_value (cc = %d)\n", c_c);
  IF t.mode = pc.ty_range THEN t := t.base END;
  CASE t.mode OF
  | pc.ty_real, pc.ty_longreal, pc.ty_ld_real:
      sz := VAL(ir.SizeType, type_size(t));
      cd.put_float(v, sz);
      INC(sg_len, sz);
  | pc.ty_complex, pc.ty_lcomplex:
      sz := re_im_sz(t);
      (* -- порядок RE,IM может зависеть от архитектуры *)
      cd.put_float(Calc.Unary(pc.su_re, ir.t_float, sz, v), sz);
      INC(sg_len, sz);
      cd.put_float(Calc.Unary(pc.su_im, ir.t_float, sz, v), sz);
      INC(sg_len, sz);
  | pc.ty_shortcard, pc.ty_cardinal, pc.ty_longcard
  , pc.ty_shortint, pc.ty_integer, pc.ty_longint
  , pc.ty_longlongint, pc.ty_longlongcard
  , pc.ty_boolean
  , pc.ty_char
  , pc.ty_enum
  , pc.ty_opaque
  , pc.ty_pointer
  , pc.ty_loc
  , pc.ty_protection
  , pc.ty_AA:
      sz := VAL(ir.SizeType, type_size(t));
      cd.put_ordinal(v, type_kind(t), sz);
      INC(sg_len, sz);
  | pc.ty_array:
      IF is_char(t.base) THEN
        put_str(v, t.len, sg_len);
      ELSE
        put_array(v, t, sg_len);
      END;
  | pc.ty_SS:
      IF c_c THEN put_C_str(v, t, sg_len) ELSE put_str(v, t.len, sg_len) END;
  | pc.ty_record:
      put_record(v, t, sg_len);
  |pc.ty_set:
      put_lset(v, t, sg_len);
  |pc.ty_proctype:
      put_procedure(v.get_object());
      INC(sg_len, tune.proc_sz);
  |pc.ty_ZZ:
      IF v.is_neg() THEN
        ty := ir.t_int;
      ELSE
        ty := ir.t_unsign;
      END;
      cd.put_ordinal(v, ty, tune.index_sz);
      INC(sg_len, tune.index_sz);
  |pc.ty_RR:
      cd.put_float(v, tune.longreal_sz);
      INC(sg_len, tune.longreal_sz);
  |pc.ty_CC:
      cd.put_float(Calc.Unary(pc.su_re, ir.t_float, tune.longreal_sz, v), tune.longreal_sz);
      INC(sg_len, tune.longreal_sz);
      cd.put_float(Calc.Unary(pc.su_im, ir.t_float, tune.longreal_sz, v), tune.longreal_sz);
      INC(sg_len, tune.longreal_sz);
  ELSE
    env.errors.Fault(t.pos, 961, t.mode);   --- invalid type mode (%d)
  END;
END put_value;

(*
PROCEDURE eq(v1, v2: pc.VALUE; t: pc.STRUCT): BOOLEAN;
 VAR i: INT;
   w1, w2: pc.VALUE;
BEGIN
  IF (t.mode = pc.ty_array)&(t.base.mode=pc.ty_char) THEN
    w1 := pc.value.new(v1.pos, t.base);
    w2 := pc.value.new(v1.pos, t.base);
    i := 0;
    LOOP
      IF (i >= t.len) THEN EXIT END;
      w1.index_get(i, v1);
      w2.index_get(i, v2);
      zz_tmp.binary(pc.sb_equ, w1, w2);
      IF zz_tmp.is_zero() THEN RETURN FALSE END;
      INC(i);
    END;
    RETURN TRUE
  ELSE
    zz_tmp.binary(pc.sb_equ, v1, v2);
    RETURN (NOT zz_tmp.is_zero())
  END;
END eq;
*)

PROCEDURE const_aggregate*(v: pc.VALUE;    (* value *)
                           t: pc.STRUCT;   (* its type *)
                          md: ir.GenModeSet;         (* generation mode *)
                     VAR arg: ir.Arg);

  VAR c: CNST_VAR; tmp: pc.OBJECT;

  PROCEDURE in_tmp(new: cd.CODE_SEGM): BOOLEAN;
    VAR c_c : BOOLEAN;
  BEGIN
-- io.print("in_tmp; cnt = %d; v = %$X\n", cns_cnt, v);
    c_c := (CC IN md);
    c := cns_vars;
    LOOP
      IF c = NIL THEN EXIT END;
      IF (c.type = t) & (c.c_const = c_c)
        & cd.EqualSegments(cd.get_ready(c.obj), new)
      THEN
        o_usage(ir.NullPos, c.obj, md, arg);
        RETURN TRUE
      END;
      c := c.next;
    END;
    INC(cns_cnt);
    NEW(c);
    c.c_const := c_c;
    c.next := cns_vars;
    cns_vars := c;
    c.type := t;
    RETURN FALSE;
  END in_tmp;

  PROCEDURE aggregate;
    VAR old, sg: cd.CODE_SEGM; loc: ir.Local;
      sg_len: LONGINT; align: SHORTINT; sz: LONGINT;
      c_c: BOOLEAN;
  BEGIN
    IF (t.mode = pc.ty_ZZ) OR (t.mode = pc.ty_RR) THEN
      -- numeric constant does not have a defined storage size
      env.errors.Fault(v.pos, 137); -- never returns
    END;
    ASSERT(t.mode # pc.ty_array_of);
    cd.get_segm(old);
    cd.new_segm(sg); cd.set_segm(sg);
    sg_len := 0;
    -- c_c == TRUE signals that '\' is constant strings should
    -- be treaten by C rules
    c_c := ((CC IN md) AND (at.GenCStrings IN at.COMP_MODE)) OR (at.GenCStringsAlways IN at.COMP_MODE);
    put_value(v, t, c_c, sg_len);
    IF in_tmp(sg) THEN
      cd.set_segm(old); RETURN
    END;
    tmp := at.new_work_object(at.make_name("$c_%d",cns_cnt), t,
                                   at.curr_mod.type, pc.ob_cons, FALSE);
   <* IF TARGET_RISC OR TARGET_SPARC THEN *>
    TOC.Add(tmp);  -- to make all aggregates self-based
   <* END *>

    c.obj := tmp;
    cd.set_ready(tmp, sg);
    cd.set_segm(old);
    align := at.default_alignment;
    bytes(t, sz, align);
    loc := ir.AddLocal(tmp.name, scope_id(at.curr_mod), align);
    ir.Locals[loc].Obj := tmp;
    ir.Locals[loc].VarType := type_kind(t);
    ir.Locals[loc].VarSize := sz;
    at.app_info(tmp, at.a_self, ir.y_RealVar, VAL(at.InfExtName, loc), NIL, 0);
    IF REF IN md THEN
      ir.MakeArgAddr(arg, loc, 0);
    ELSE
      ir.MakeArgLocal(arg, loc, 0);
    END;
  END aggregate;

BEGIN (* --- c o n s t _ a g g r e g a t e --- *)
<* IF db_def THEN *>
  io.print("const_aggregate [ref=%d, c_c=%d]\n", REF IN md, CC IN md);
<* END *>
  ASSERT(NOT(LVL IN md));
  IF t.mode = pc.ty_range THEN t := t.base END;
  IF REF IN md THEN aggregate; RETURN END;
  CASE t.mode OF
  | pc.ty_real, pc.ty_longreal, pc.ty_ld_real:
      ir.MakeArgFloat(arg, v);
(*
  | pc.ty_complex, pc.ty_lcomplex:
      gi.MakeArgComplex(arg, v);
*)
  | pc.ty_shortcard, pc.ty_cardinal, pc.ty_longcard, pc.ty_longlongcard
  , pc.ty_shortint, pc.ty_integer, pc.ty_longint, pc.ty_longlongint
  , pc.ty_ZZ
  , pc.ty_boolean
  , pc.ty_char
  , pc.ty_enum
  , pc.ty_opaque
  , pc.ty_loc
  , pc.ty_protection:
      ir.MakeArgNum(arg, v);
  | pc.ty_pointer:
      ir.MakeArgNum(arg, v);
      IF pc.ttag_volatile IN t.base.tags THEN INCL(arg.mode, VOLATILE) END;
  | pc.ty_AA:
      ir.MakeArgNum(arg, tune.nil_val);
  | pc.ty_array, pc.ty_SS, pc.ty_record:
      aggregate;
  | pc.ty_set:
      IF t.len > tune.BITSET_LEN(*_scalar*) THEN aggregate;
      ELSE ir.MakeArgNum(arg, v);
      END;
  | pc.ty_proctype:
      tmp := v.get_object();
      IF tmp = NIL THEN
        ir.MakeArgNum(arg, tune.nil_val);
      ELSE o_usage(ir.NullPos, tmp, ir.GenModeSet{}, arg);
      END;
  ELSE
    env.errors.Fault (t.pos, 961, t.mode);   --- invalid type mode (%d)
  END;
END const_aggregate;

PROCEDURE get_bytes*(ps-: pc.TPOS; t: pc.STRUCT): LONGINT;
  VAR i: LONGINT;
BEGIN
  i := pc.code.get_size(pc.su_bytes, t);
  IF i<=0 THEN
    env.errors.Error(ps,1014,""); i := 1;
  END;
  RETURN i;
END get_bytes;

(*
PROCEDURE app_type(p: pc.OBJECT);
BEGIN

END app_type;

PROCEDURE app_len(p: pc.OBJECT; dim: INTEGER);
BEGIN

END app_len;
*)

PROCEDURE prototype* (t: pc.STRUCT) : pr.ProtoNum;
  CONST MAX_PAR = 125;

  VAR buf_errs: BOOLEAN;
    prm_cnt: INTEGER;
    prm_buf: ARRAY MAX_PAR OF pr.param;
    offset, last_offs: LONGINT;
    RtoL: BOOLEAN;

  PROCEDURE parameter(p_md   : pr.ParamMode;
                      p_ind  : SHORTINT;
                      p_type : ir.TypeType;
                      p_sz   : ir.SizeType;
                      p_where: pr.MemType;
                      p_offs : LONGINT);
    VAR ln: LONGINT;
  BEGIN
    IF buf_errs THEN RETURN
    ELSIF prm_cnt >= LEN(prm_buf) THEN
      env.errors.Error(t.pos, 222);
      buf_errs:=TRUE;
    ELSE
      prm_buf[prm_cnt].mode := p_md;
      prm_buf[prm_cnt].ind  := p_ind;
      prm_buf[prm_cnt].type := p_type;
      prm_buf[prm_cnt].size := p_sz;
      prm_buf[prm_cnt].where:= p_where;
      prm_buf[prm_cnt].offs := p_offs;
      IF (p_where = pr.STACK) & (p_sz > 0) THEN
        IF p_sz<4 THEN
          IF tune.BIG_END THEN       -- прижать к другому краю
            INC(prm_buf[prm_cnt].offs, 4 - p_sz);
          END;
          ln := 4;
          -- пока все параметры кладутся в стек push eax - длина всегда 4
        ELSIF (p_sz = 10) THEN ln := 12;  (* ld_real ?? *)
        ELSE ln := p_sz;
        END;
        IF ~RtoL THEN
          DEC(p_offs, ln);
          DEC(offset, ln);
          prm_buf[prm_cnt].offs := p_offs;
        ELSE
          INC(offset, ln)
        END;
        last_offs := p_offs;
      END;
      INC(prm_cnt);
    END;
  END parameter;

  VAR p: pc.OBJECT;   us: pc.USAGE;
    prt: pr.ProtoNum;  pext: at.PROT_EXT;  a: at.ATTR_EXT;
    prm_ptr: pr.params;
    proc_lev,i: INTEGER;
    o: pc.OBJECT;
    P: pr.Proto;
    ty: ir.TypeType;
    sz: ir.SizeType;

  PROCEDURE make_bases;
    VAR i: SHORTINT;
  BEGIN
    FOR i := 0 TO MAX(pr.Bases) DO
      IF i IN P.bases THEN
        parameter(pr.pm_base, i, tune.addr_ty, tune.addr_sz, pr.STACK, offset);
      END;
    END;
  END make_bases;

  PROCEDURE chk_array_of(p: pc.OBJECT): BOOLEAN;
    VAR ind: SHORTINT;
  BEGIN
    IF (p.type.mode # pc.ty_array_of)
      OR NOT (t.flag IN opt.LangsWithOpenArrays)
    THEN
      RETURN FALSE
    END;
    IF p.mode = pc.ob_varpar THEN ind := ORD(pr.by_ref);
    ELSE                          ind := ORD(pr.by_ROref);
    END;
    parameter(pr.pm_param, ind, tune.addr_ty, tune.addr_sz, pr.STACK, offset);
    FOR ind := 0 TO p.type.len - 1  DO
      parameter(pr.pm_len, ind, tune.index_ty, tune.index_sz, pr.STACK, offset);
    END;
    RETURN TRUE
  END chk_array_of;

BEGIN
  IF t.mode = pc.ty_module THEN
    IF (at.curr_mod.type = t) & at.main THEN
      IF at.GENDLL IN at.COMP_MODE THEN RETURN std.DLLMainProtoNum
      ELSE                              RETURN std.MainProtoNum
      END;
    ELSE RETURN std.ModuleProtoNum
    END;
  END;
  ASSERT(t.mode = pc.ty_proctype);
  a := at.attr(t.ext, at.a_prot);
  IF a # NIL THEN
    RETURN a(at.PROT_EXT).proto
  END;
<* IF db_def THEN *> note("prototype", t.obj); <* END *>
  buf_errs := FALSE;
  pr.NewProto(prt, P);
  P.lang_flag := t.flag;      (* -- временно !! *) (* почему временно? *)
  RtoL := t.flag IN opt.LangsWithPushingParams_RtoL;
  P.right_to_left := RtoL;
  prm_cnt := 0;
  offset := tune.PARAM_START;
  last_offs := offset;
  (* ------------ возвращаемое значение ------------ *)
  IF t.base.mode = pc.ty_void THEN
    P.ret_type := ir.t_void;
    P.ret_size := 0;
    P.rtn := FALSE;
    P.ext_rtn := FALSE;
  ELSIF is_scalar(t.base) THEN
    type_info(t.base, P.ret_type, P.ret_size);
    P.rtn := FALSE;
    P.ext_rtn := FALSE;
  ELSE
    P.ret_type := ir.t_void;
    P.ret_size := 0;
    P.rtn      := TRUE;
    IF (t.base.mode = pc.ty_array) OR (t.base.mode = pc.ty_set) THEN
      P.ext_rtn := FALSE;
    ELSE
      P.ext_rtn := TRUE;
    END;
    parameter(pr.pm_return, 0, tune.addr_ty, tune.addr_sz, pr.STACK, offset);
  END;
  (* ------------ базы охватывающих процедур ------------ *)
  P.nbase := 0; P.bases := pr.Bases{};
  us := t.use;
  IF us # NIL THEN (* иначе может не быть obj !! *)
    proc_lev := t.obj.lev;
    REPEAT
      o := us.obj;
     <* IF db_def THEN *>
      io.print("  %s.use: %s %{}\n", t.obj.name^, o.name^, us.tags);
     <* END *>
      IF o.mode IN pc.VARs THEN
        IF (proc_lev - o.lev) <= MAX(SET) THEN
          INCL(P.bases, (proc_lev - o.lev));
          INC(P.nbase);
        ELSE
          env.errors.Error(o.pos, 221, MAX(SET));
        END;
      END;
      us := us.next;
    UNTIL us = NIL;
    IF P.bases # {} THEN make_bases END;
  END;
  (* ------------ остальные параметры ------------ *)
  P.seq := FALSE;
  p := t.prof;
  WHILE p # NIL DO
    IF chk_array_of(p) THEN (* все уже сделано *)
    ELSE
      CASE p.mode OF
      | pc.ob_var:
           IF is_scalar(p.type) THEN
--           align := at.default_alignment
--           bytes(p.type, size, align);
--           sz := SHORT(SHORT(size));
--           ty := type_kind(p.type);
             type_info(p.type, ty, sz);
             parameter(pr.pm_param, ORD(pr.by_val), ty,  sz, pr.STACK, offset);

           ELSIF p.type.mode IN pc.CPLXs THEN
             sz := re_im_sz(p.type);
             parameter(pr.pm_re, ORD(pr.by_val), ir.t_float, sz, pr.STACK, offset);
             parameter(pr.pm_im, ORD(pr.by_val), ir.t_float, sz, pr.STACK, offset);
           ELSE
             parameter(pr.pm_param, ORD(pr.by_ROref), tune.addr_ty, tune.addr_sz, pr.STACK, offset);
           END;
      | pc.ob_varpar:
          IF (p.type.mode=pc.ty_record) & (p.type.flag IN opt.LangsWithTypedRecords) THEN
            parameter(pr.pm_formrec, ORD(pr.by_ref), tune.addr_ty, tune.addr_sz, pr.STACK, offset);
            parameter(pr.pm_type, ORD(pr.by_val), tune.addr_ty, tune.addr_sz, pr.STACK, offset);
          ELSE
            parameter(pr.pm_param, ORD(pr.by_ref), tune.addr_ty, tune.addr_sz, pr.STACK, offset);
          END;
      | pc.ob_seq:
          ASSERT (p.next = NIL);
          ASSERT (t.flag IN opt.LangsWithSEQParams);
          P.seq := TRUE;
          parameter(pr.pm_seq, 0, ir.t_flxarr, -1, pr.STACK, offset);
      END;
    END;
    p := p.next;
  END (* WHILE *);

  P.npar := prm_cnt;
  IF prm_cnt > 0 THEN
    NEW(prm_ptr, prm_cnt);
    FOR i := 0 TO prm_cnt-1 DO
      IF NOT RtoL & (last_offs # tune.PARAM_START) & (prm_buf[i].where = pr.STACK) THEN
        prm_buf[i].offs := prm_buf[i].offs - last_offs + tune.PARAM_START;
      END;
      prm_ptr[i] := prm_buf[i];
    END;
  ELSE prm_ptr := NIL;
  END;
  P.par := prm_ptr;
  IF P.ret_type # ir.t_void THEN
    P.where_res := pr.STACK;
    P.offs_res := offset;
  END;
  NEW(pext); pext.proto := prt;
  at.app_struct_attr(t, pext, at.a_prot);
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  pr.EvalProto(prt);
<* END *>
<* IF db_procs THEN *> pr.WrProto(prt); <* END *>
  RETURN prt
END prototype;

VAR temp_no : INTEGER;

PROCEDURE new_temp_var(ty: ir.TypeType; size: LONGINT): ir.Local;
  VAR v: TMP_VAR; l: ir.Local; al: SHORTINT;
BEGIN
  IF    size = 1 THEN al := 1;
  ELSIF size = 2 THEN al := 2;
  ELSE                al := 4;
  END;
  l := ir.AddLocal(at.make_name("$_%d",temp_no), local, al);
  ir.Locals[l].VarType := ty;
  ir.Locals[l].VarSize := size;
  INC(temp_no);

  NEW(v);
  v.loc := l;
  v.next := tmp_busy;
  tmp_busy := v;
  RETURN v.loc
END new_temp_var;

PROCEDURE search_temp_var(ty : ir.TypeType;
                         size: LONGINT;
                       VAR nm: ir.Local): BOOLEAN;
  VAR p,v: TMP_VAR;
BEGIN
  v := tmp_vars; p := NIL;
  LOOP
    IF v = NIL THEN RETURN FALSE END;
    IF (ir.Locals[v.loc].VarType = ty) & (ir.Locals[v.loc].VarSize = size) THEN
      EXIT
    END;
    p := v; v := v.next
  END;
  IF p = NIL THEN tmp_vars := v.next ELSE p.next := v.next END;
  v.next := tmp_busy;
  tmp_busy := v;
  nm := v.loc;
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
  IF tmp_busy # NIL THEN
    v := tmp_busy;
    WHILE v.next # NIL DO v := v.next END;
    v.next := tmp_vars;
    tmp_vars := tmp_busy;
  END;
  tmp_busy := x;
END exit_statement;

PROCEDURE make_temp_var*(ty: ir.TypeType; size: LONGINT; VAR nm: ir.Local);
BEGIN
  IF search_temp_var(ty, size, nm) THEN RETURN END;
  nm := new_temp_var(ty, size);
END make_temp_var;

PROCEDURE gen_sizeof*(tpos: ir.TPOS; t: pc.STRUCT; VAR arg: ir.Arg);
  VAR sz: LONGINT;
BEGIN
  ASSERT(t.mode # pc.ty_array_of);
  sz := get_bytes(tpos, t);
  c_number(sz, arg);
END gen_sizeof;

(*--------------------------- Declarations ------------------------------*)

(*
PROCEDURE func_is_extern(o: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (o.mode=pc.ob_eproc) OR
         (o.mode=pc.ob_lproc) OR
         (pc.otag_public IN o.tags) OR
         (o.host.mode=pc.ty_record) &
         (pc.otag_public IN o.host.obj.tags);
END func_is_extern;
*)

PROCEDURE gen_code_body(o: pc.OBJECT);
  VAR l: pc.NODE; n: INT;
    old, sg: cd.CODE_SEGM;
BEGIN
<* IF db_code THEN *>
  io.print("gen_code_body('%s')\n", o.name^);
<* END *>
  ASSERT(o.mode=pc.ob_cproc);
  IF at.omark_gen_ready IN o.marks THEN
    RETURN
  END;
  cd.get_segm(old);
  cd.new_segm(sg); cd.set_segm(sg);
  l := o.val.l;
  WHILE l # NIL DO
    IF l.mode = pc.nd_value THEN
      IF l.type.mode IN pc.WHOLEs THEN
        n := l.val.get_integer();
       <* IF db_code THEN *> io.print("code_elem = %X\n", n); <* END *>
      ELSE n := -1;
      END;
    ELSE n := -1;
    END;
    IF (n<00H) OR (n>0FFH) THEN
      env.errors.Error (l.pos, 921);   --- "invalid code procedure"
      n := 0;
    END;
    cd.GenByte(VAL(CHAR,n));
    l := l.next;
  END;
  cd.set_ready(o, sg);
  cd.set_segm(old);
END gen_code_body;

PROCEDURE func_declaration(o: pc.OBJECT);
(* предварительное описание функции *)
  VAR proc: pr.ProcNum;  prt: pr.ProtoNum;
BEGIN
<* IF db_def THEN *> note("func_declaration", o); <* END *>
  prt  := prototype(o.type);
  proc := pr.NewProc(o);
  IF o.mno # at.curr_mno THEN INCL(pr.ProcList[proc].tags, pr.external) END;
  IF pc.otag_public IN o.tags THEN
    INCL(pr.ProcList[proc].tags, pr.public);
  END;
  pr.ProcList[proc].name     := o.name;
  pr.ProcList[proc].proto_no := prt;
  at.app_info(o, at.a_self, ir.y_ProcConst, VAL(at.InfExtName, proc), NIL, 0);
  INCL(o.tags, at.otag_declared);
<* IF TARGET_RISC THEN *>
  IF at.ABI = at.PowerOpen THEN
    IF pr.IsExternal(proc) & (o.mno=at.curr_mno) THEN TOC.Add(o); END;
  END;
<* END *>
  IF o.mode = pc.ob_cproc THEN gen_code_body(o) END;
(*
  IF o.host.mode = pc.ty_record THEN
    (* возможно, еще надо обработать тип записи, *)
    (* если эта процедура - метод                *)
  END;
*)
END func_declaration;

PROCEDURE module_declaration(o: pc.OBJECT);
(* предварительное описание функции *)
  VAR proc: pr.ProcNum;  prt: pr.ProtoNum;
BEGIN
  prt  := prototype(o.type);
  proc := pr.NewProc(o);
  IF o.mno # at.curr_mno THEN INCL(pr.ProcList[proc].tags, pr.external) END;
  INCL(pr.ProcList[proc].tags, pr.public);
  pr.ProcList[proc].name     := o.name;
  pr.ProcList[proc].proto_no := prt;
  at.app_info(o, at.a_self, ir.y_ProcConst, VAL(at.InfExtName,proc), NIL, 0);
<* IF TARGET_RISC THEN *>
  IF o.mno=at.curr_mno THEN TOC.Add(o); END;
<* END *>
  INCL(o.tags, at.otag_declared);
END module_declaration;

PROCEDURE proc_num*(o: pc.OBJECT): pr.ProcNum;
  VAR a: at.ATTR_EXT;
BEGIN
  a := at.attr(o.ext, at.a_self);
  RETURN VAL(pr.ProcNum, a(at.INFO_EXT).name);
END proc_num;

PROCEDURE object_declaration*(o: pc.OBJECT);
  VAR arg: ir.Arg;  a: at.ATTR_EXT;
  VAR old, sg: cd.CODE_SEGM; sg_len: LONGINT;
BEGIN
  IF at.otag_declared IN o.tags THEN RETURN END;
<* IF db_def THEN *> note("object_declaration", o); <* END *>
  ASSERT(NOT (at.otag_declaring IN o.tags));
  INCL(o.tags,at.otag_declaring);
  CASE o.mode OF
  | pc.ob_var:
      IF (o.val # NIL) & (o.mno = at.curr_mno) &
        NOT (at.omark_gen_ready IN o.marks)
      THEN
        IF (o.val.mode = pc.nd_value) THEN
          cd.get_segm(old);
          cd.new_segm(sg); cd.set_segm(sg);
          sg_len := 0;
          put_value(o.val.val, o.type, FALSE, sg_len);
          cd.set_ready(o, sg);
          cd.set_segm(old);
(*   botva iz VAX
          ELSIF (o.val.mode=pc.nd_unary)AND(o.val.sub=pc.su_conv)
                AND(o.val.l.mode=pc.nd_value)
                AND(o.val.type.mode=pc.ty_pointer) THEN
          cd.get_segm(old);
          cd.new_segm(sg); cd.set_segm(sg);
          sg_len := 0;
          cd.GenLWord(0);
          cd.gen_fixup(o.val.l.val.get_object(), 0, cd.fx_obj32);
          cd.set_ready(o, sg);
          cd.set_segm(old);
*)
        ELSE
          ASSERT(FALSE);
        END;
      END;
      make_local(o);
  | pc.ob_cons:
       IF at.omark_gen_ready IN o.marks THEN make_local(o);
      ELSE
--        IF o.val.mode # pc.nd_value THEN
--          pcVis.vis(o.val, 0);
--        END;
        ASSERT(o.val.mode = pc.nd_value);
        const_aggregate(o.val.val, o.type, ir.GenModeSet{}, arg);
        a := at.attr(o.ext, at.a_self);
        IF a # NIL THEN
          WITH a : at.INFO_EXT DO
            a.procno := at.curr_procno;
            a.e_tag := arg.tag;
            a.name  := VAL(at.InfExtName, arg.name);
            a.value := arg.value;
            a.offs  := arg.offset;
          END;
        ELSE at.app_info(o, at.a_self, arg.tag, VAL(at.InfExtName,arg.name), arg.value, arg.offset);
        END;
      END;
  | pc.ob_proc,pc.ob_xproc,pc.ob_eproc,pc.ob_cproc,pc.ob_lproc:
      func_declaration(o);
  | pc.ob_type:
      type_definition(o);
  | pc.ob_module:
      module_declaration(o);
  ELSE
    env.errors.Fault (o.pos, 962, o.mode);    ---  invalid object mode (%d)
  END;
  EXCL(o.tags,at.otag_declaring);
  INCL(o.tags,at.otag_declared);
  INCL(o.marks, at.omark_used);
END object_declaration;

(*------------------------ Definitions -----------------------------------*)

PROCEDURE offset_definition(o: pc.OBJECT; rec: BOOLEAN);

  PROCEDURE ofs(fx_ofs: LONGINT);
    VAR arg: ir.Arg;
  BEGIN
    IF rec THEN
      o_usage(ir.NullPos, o, ir.GenModeSet{}, arg);
      ASSERT((arg.tag = ir.y_AddrConst) & (arg.name = MAX(ir.Local)));
      cd.GenLWord(arg.offset + fx_ofs + tune.X2C_BASE);
    ELSE
     <* IF TARGET_RISC OR TARGET_SPARC THEN *>
      IF o.mno = at.curr_mno THEN
        cd.gen_fixup(TOCData.ModuleStartObjects[o.mno],
                     at.get_global_offset(o),
                     cd.fx_obj32);
      ELSE
        cd.gen_fixup(o, fx_ofs, cd.fx_obj32);
      END;
     <* ELSE *>
      cd.gen_fixup(o, fx_ofs, cd.fx_obj32);
     <* END *>
    END;
  END ofs;

  VAR t: pc.STRUCT; arr_size, elm_size: LONGINT;
BEGIN
  t := o.type;
  LOOP
    ASSERT(t.flag IN opt.LangsWithTypeDescriptors);
    IF t.mode = pc.ty_record THEN
      cd.GenLWord(tune.X2C_OFS_REC);
      cd.gen_fixup(t.obj, 0, cd.fx_obj32);
      ofs(0); RETURN
    ELSIF t.mode = pc.ty_pointer THEN
      ofs(0); RETURN
    ELSIF t.mode IN pc.ARRs THEN
      cd.GenLWord(tune.X2C_OFS_ARR);
      arr_size := type_size(t);
      REPEAT t := t.base UNTIL NOT(t.mode IN pc.ARRs);
      elm_size := type_size(t);
      ofs(arr_size - elm_size);
       -- записываем смещение последнего элемента массива
       -- и уходим на второй виток для типа элемента
    ELSE ASSERT(FALSE);
    END;
  END;
END offset_definition;

PROCEDURE out_str*(s-: ARRAY OF CHAR): pc.OBJECT;
  VAR tmp : pc.OBJECT;
    old, sg: cd.CODE_SEGM;
    i: INT;   ch: CHAR;
BEGIN
  tmp := at.new_work_object(NIL, NIL, at.curr_mod.type, pc.ob_cons, FALSE);
  cd.get_segm(old);
  cd.new_segm(sg); cd.set_segm(sg);
  cd.GenStartString;
  i:=0;
  REPEAT
    ch := s[i];
    cd.GenStringChar(ch);
    INC(i);
  UNTIL ch = 0X;
  cd.GenEndString;
  cd.set_ready(tmp,sg);
  cd.set_segm(old);
  RETURN tmp
END out_str;

PROCEDURE type_desc_definition(t: pc.STRUCT);  (* строим дескриптор для типа t *)
  VAR
    bf     : ARRAY 1024 OF pc.OBJECT;
    bf_cnt : INT;
    bbf    : ARRAY 16 OF pc.STRUCT;

  PROCEDURE out_bases;
    VAR i: INT; r: pc.STRUCT;
  BEGIN
    FOR i := 0 TO LEN(bbf)-1 DO
      r := bbf[i];
      IF (r # NIL) & (r.flag IN opt.LangsWithTypeDescriptors) THEN
        ASSERT(r.mode=pc.ty_record);
        cd.gen_fixup(r.obj, 0, cd.fx_obj32);
      ELSE
        cd.put_nil_value;
      END;
    END;
  END out_bases;

  PROCEDURE out_procs;
    VAR i,j: INT;
  BEGIN
    j := bf_cnt - 1;
    IF j < 0 THEN INC(j) END;
    FOR i := 0 TO j DO put_procedure(bf[i]) END;
  END out_procs;

  PROCEDURE out_ptrs_rec(r: pc.STRUCT);
    VAR o: pc.OBJECT;
  BEGIN
    IF (r.base#NIL) & (r.base.flag IN opt.LangsWithTypeDescriptors) THEN
      out_ptrs_rec(r.base);
    END;
    o := r.prof;
    WHILE o # NIL DO
      ASSERT(o.mode IN pc.OB_SET{pc.ob_field,pc.ob_field_bts});
      IF (o.type.flag IN opt.LangsWithTypeDescriptors) &
         (o.type.mode IN (pc.TY_SET{pc.ty_record,pc.ty_pointer}+pc.ARRs))
      THEN
        offset_definition(o, TRUE);
      END;
      o := o.next;
    END;
  END out_ptrs_rec;

  PROCEDURE out_ptrs(t: pc.STRUCT);
  BEGIN
    out_ptrs_rec(t);
    cd.GenLWord(tune.X2C_OFS_END);
  END out_ptrs;

  PROCEDURE get_bases(b: pc.STRUCT);
    VAR i: LONGINT;
  BEGIN
    FOR i:=0 TO LEN(bbf)-1 DO bbf[i]:=NIL END;
    WHILE (b#NIL) & (b.flag IN opt.LangsWithTypeDescriptors) DO
      IF NOT (at.otag_declared IN b.obj.tags) THEN
        object_declaration(b.obj);
      END;
      bbf[b.len] := b; b := b.base;
    END;
  END get_bases;

  PROCEDURE get_procs(b: pc.STRUCT);
    VAR o: pc.OBJECT; i: LONGINT;
  BEGIN
    bf_cnt := 0;
    FOR i := 0 TO LEN(bf)-1 DO bf[i] := NIL END;
    WHILE b # NIL DO
      o := b.mem;
      WHILE o # NIL DO
        IF o.type.len >= bf_cnt THEN bf_cnt := o.type.len+1 END;
        IF bf[o.type.len] = NIL THEN bf[o.type.len] := o END;
        o := o.next;
      END;
      b := b.base;
    END;
  END get_procs;

  VAR old, new: cd.CODE_SEGM;
    md_type: pc.STRUCT;
    ps, of: pc.OBJECT;
    nm : pc.STRING;

CONST
  MAGIC = 93678150H;

BEGIN
  ASSERT(t.obj # NIL);
  ASSERT(t.mode = pc.ty_record);
  ASSERT(t.obj.mno = at.curr_mno);
  ASSERT(t.flag IN opt.LangsWithTypeDescriptors);
  md_type := at.curr_mod.type;

  get_procs(t);
  get_bases(t);

  nm := at.make_name("%s'proc", t.obj.name^);
  ps := at.new_work_object(nm, NIL, md_type, pc.ob_cons, FALSE);
  cd.get_segm(old);
  cd.new_segm(new); cd.set_segm(new);
  out_procs;
  cd.set_ready(ps, new);

  nm := at.make_name("%s'offs", t.obj.name^);
  of := at.new_work_object(nm, NIL, md_type, pc.ob_cons, FALSE);
  cd.new_segm(new); cd.set_segm(new);
  out_ptrs(t);
  cd.set_ready(of, new);

  cd.new_segm(new); cd.set_segm(new);              --  X2C_TD_STR = RECORD
  cd.GenLWord(type_size(t));                          -- size   : SYSTEM.size_t;
  cd.gen_fixup(out_str(t.obj.name^), 0, cd.fx_obj32); -- name   : POINTER TO CHAR;
  cd.gen_fixup(mod_desc(), 0, cd.fx_obj32);           -- module : X2C_MD;
  IF last_type = NIL THEN                             -- next   : X2C_TD;
    cd.put_nil_value;
  ELSE
    cd.gen_fixup(last_type, 0, cd.fx_obj32);
  END;
  last_type := t.obj;
  cd.GenWord(SHORT(bf_cnt));                          -- methods: SYSTEM.INT16;
  cd.GenWord(SHORT(t.len));                           -- level  : SYSTEM.INT16;
  out_bases;                                          -- base   : ARRAY [0..15] OF X2C_TD;
  cd.gen_fixup(ps, 0, cd.fx_obj32);                   -- proc   : POINTER TO PROC;
  cd.gen_fixup(of, 0, cd.fx_obj32);                   -- offs   : POINTER TO POINTER TO SYSTEM.void;
  cd.put_nil_value;                                   -- succ   : X2C_TD;
  cd.put_nil_value;                                   -- link   : X2C_LINK;
  cd.put_nil_value;                                   -- tail   : X2C_LINK;
  cd.gen_fixup(t.obj, 0, cd.fx_obj32);                -- self   : X2C_TD;
  cd.GenLWord(SYSTEM.VAL(LONGINT, MAGIC));            -- res    : SYSTEM.CARD32;
  cd.set_ready(t.obj, new);                        --  END;
  cd.set_segm(old);
END type_desc_definition;

PROCEDURE profiling_desc*(o: pc.OBJECT): pc.OBJECT;
  VAR dsc: pc.OBJECT;
    old, new: cd.CODE_SEGM;
    buffer: ARRAY 256 OF CHAR;
BEGIN
  ASSERT(o.mode IN pc.PROCs);
  ObjNames.makename(o, buffer);
  dsc := at.new_work_object(NIL, PROFL_T, at.curr_mod.type, pc.ob_cons, FALSE);
  cd.get_segm(old);
  cd.new_segm(new); cd.set_segm(new);
  cd.gen_fixup(out_str(buffer), 0, cd.fx_obj32);   -- name   : POINTER TO CHAR;
  cd.put_nil_value;                                -- all other 5 fields
  cd.put_nil_value;
  cd.put_nil_value;
  cd.put_nil_value;
  cd.put_nil_value;
  cd.set_ready(dsc, new);
  cd.set_segm(old);
  RETURN dsc
END profiling_desc;

PROCEDURE is_typed_var(o: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (o.mode = pc.ob_var) &
         (o.type.flag IN opt.LangsWithTypeDescriptors) &
         (o.type.mode IN (pc.TY_SET{pc.ty_record,pc.ty_pointer}+pc.ARRs));
END is_typed_var;

PROCEDURE module_desc_definition(m: pc.OBJECT);  (* сам модуль *)

  PROCEDURE list_offs(o: pc.OBJECT);
  BEGIN
    WHILE o#NIL DO
      IF is_typed_var(o) THEN offset_definition(o, FALSE) END;
      o:=o.next;
    END;
  END list_offs;

  PROCEDURE list_cmds(o: pc.OBJECT; adr: BOOLEAN);
    VAR wrk: pc.OBJECT;
  BEGIN
    WHILE o#NIL DO
      IF (o.flag IN opt.LangsAllowCommands) & 
         (o.mode = pc.ob_xproc) &
         (o.type.prof = NIL) &
         (o.type.base.mode = pc.ty_void)
      THEN
        ASSERT(pc.otag_public IN o.tags);
        IF adr THEN
          put_procedure(o);
        ELSE
          wrk := out_str(o.name^);
          cd.gen_fixup(wrk, 0, cd.fx_obj32);
        END;
      END;
      o := o.next;
    END;
  END list_cmds;

  VAR nm: pc.STRING;
    of, cmd, cnm: pc.OBJECT;
    old, new: cd.CODE_SEGM;
BEGIN
  cd.get_segm(old);

 (* адреса глобальных переменных - указателей *)
  nm := at.make_name("%s'offs", m.name^);
  of := at.new_work_object(nm, NIL, m.type, pc.ob_cons, FALSE);
  cd.new_segm(new); cd.set_segm(new);
  list_offs(m.type.prof);
  list_offs(m.type.mem);
  cd.GenLWord(tune.X2C_OFS_END);
  cd.set_ready(of, new);

 (* адреса глобальных процедур - команд *)
  nm := at.make_name("%s'cmds", m.name^);
  cmd := at.new_work_object(nm, NIL, m.type, pc.ob_cons, FALSE);
  cd.new_segm(new); cd.set_segm(new);
  IF at.o2_cmds IN at.COMP_MODE THEN
    list_cmds(m.type.prof, TRUE);
  END;
  cd.put_nil_value;
  cd.set_ready(cmd, new);

 (* имена глобальных процедур - команд *)
  IF at.o2_cmds IN at.COMP_MODE THEN
    nm := at.make_name("%s'cnms", m.name^);
    cnm := at.new_work_object(nm, NIL, m.type, pc.ob_cons, FALSE);
    cd.new_segm(new); cd.set_segm(new);
    list_cmds(m.type.prof, FALSE);
    cd.put_nil_value;
    cd.set_ready(cnm, new);
  ELSE
    cnm := cmd;  (* use the same NIL value *)
  END;

  cd.new_segm(new); cd.set_segm(new);           -- X2C_MD_STR = RECORD
  cd.put_nil_value;                               -- next  : X2C_MD;             (* next module descriptor               *)
  cd.put_nil_value;                               -- cnext : X2C_MD;             (* next module descriptor               *)
  cd.gen_fixup(out_str(m.name^), 0, cd.fx_obj32); -- name : POINTER TO CHAR;    (* module name                          *)
  cd.gen_fixup(of,  0, cd.fx_obj32);              -- offs : POINTER TO POINTER TO SYSTEM.void;(* global pointer offsets *)
  cd.gen_fixup(cmd, 0, cd.fx_obj32);              -- cmds : POINTER TO PROC;    (* commands                             *)
  cd.gen_fixup(cnm, 0, cd.fx_obj32);              -- cnms : POINTER TO POINTER TO CHAR;(* command names                 *)
  IF last_type = NIL THEN                         -- types: X2C_TD;             (* type descriptors                     *)
    cd.put_nil_value;
  ELSE
    cd.gen_fixup(last_type, 0, cd.fx_obj32);
  END;
  cd.set_ready(mod_desc(), new);                -- END;
  cd.set_segm(old);
END module_desc_definition;

PROCEDURE gen_type_descs*(md: pc.OBJECT);

  PROCEDURE list(o: pc.OBJECT);
  BEGIN
    WHILE o # NIL DO
      IF (o.mode = pc.ob_type) &
         (o.type.flag IN opt.LangsWithTypeDescriptors) &
         (o.type.mode = pc.ty_record) &
         (o.type.obj = o)
      THEN
        type_desc_definition(o.type);
        list(o.type.mem);
      ELSIF o.mode IN pc.PROCs THEN
        list(o.type.mem);
      END;
      o := o.next;
    END;
  END list;

  PROCEDURE chk_typed_vars(o: pc.OBJECT): BOOLEAN;
  BEGIN
    LOOP
      IF o = NIL         THEN RETURN FALSE END;
      IF is_typed_var(o) THEN RETURN TRUE  END;
      o := o.next;
    END;
  END chk_typed_vars;

BEGIN (* ----- g e n _ t y p e _ d e s c s -----*)
  list(md.type.prof);
  list(md.type.mem);   (* ?? возможно этого не нужно *)
  IF (md.type.flag IN opt.LangsWithTypeDescriptors) OR (md_desc # NIL) OR
     chk_typed_vars(md.type.prof) OR chk_typed_vars(md.type.mem)
  THEN
    module_desc_definition(md);
  END;
END gen_type_descs;

(** размещает поле, перевычисляя смещение, и выдает его выравнивание *)

PROCEDURE alloc_field(f : pc.OBJECT;
               VAR offs : LONGINT;
              VAR align : SHORTINT;
               cre_attr : BOOLEAN);
  VAR field_size: LONGINT; field_align: SHORTINT;
BEGIN
  field_align := align;
  bytes(f.type, field_size, field_align);
  ASSERT(field_size>=0);
  IF field_align > align THEN
    env.errors.Warning(f.pos, 324, f.name^, field_align, align);
  ELSIF field_align < align THEN
    align := field_align;
  END;
  IF offs <= MAX(LONGINT) - align THEN
    mk_align(offs, align);
  ELSE
    err_type_size(f.host);
  END;
  IF cre_attr THEN
    at.app_info(f, at.a_self, ir.y_Nothing+at.BASE, at.ZEROInfExtName, NIL, offs);
    INCL(f.tags, at.otag_declared);
  END;
  IF offs <= MAX(LONGINT) - field_size THEN
    INC(offs, field_size);
  ELSE
    err_type_size(f.host)
  END;
END alloc_field;

PROCEDURE bit_field(f: pc.OBJECT; offs: LONGINT);
--  VAR wrd_offs: LONGINT;
BEGIN
(*
  IF (f.attr = NIL) OR (f.attr.l = NIL) THEN
    wrd_offs := offs;    (* how to take in account alignment ??? *)
  ELSE
    wrd_offs := BYTES_PER_WORD * f.attr.l.val.get_integer();
  END;
*)
  at.app_info(f, at.a_self, ir.y_Nothing+at.BASE, at.ZEROInfExtName, NIL, offs);
  INCL(f.tags, at.otag_declared);
END bit_field;

(** размещает список полей, перевычисляя смещение, и выдает выравнивание *)

PROCEDURE alloc_flist (f : pc.OBJECT;
                VAR offs : LONGINT;
               VAR align : SHORTINT;
                cre_attr : BOOLEAN);
  VAR n : pc.NODE;
    mx_size, l_size, start : LONGINT;
    list_align, mx_align, l_align : SHORTINT;
BEGIN
  list_align := 1;
  WHILE f # NIL DO
    IF f.mode = pc.ob_field THEN
      l_align := align;
      alloc_field(f, offs, l_align, cre_attr);
      IF l_align > list_align THEN list_align := l_align END;
    ELSIF f.mode = pc.ob_field_bts THEN
      IF cre_attr THEN bit_field(f, offs) END;
    ELSIF f.mode = pc.ob_header THEN
      IF f.val.obj # NIL THEN
        l_align := align;
        alloc_field(f.val.obj, offs, l_align, cre_attr);
        IF l_align > list_align THEN list_align := l_align END;
      END;
      mx_size := 0;
      mx_align := 1;
      n := f.val.l;
      WHILE n # NIL DO
        ASSERT(n^.mode = pc.nd_node);
        l_size := 0; l_align := align;
        alloc_flist(n.obj, l_size, l_align, FALSE);
        IF l_size > mx_size THEN mx_size := l_size END;
        IF l_align > mx_align THEN mx_align := l_align END;
        n := n.next;
      END;
      IF offs <= MAX(LONGINT) - mx_align THEN
        mk_align(offs, mx_align);
      ELSE
        err_type_size(f.host)
      END;

      IF cre_attr THEN
        n  := f.val.l;
        start := offs;
        WHILE n # NIL DO
          l_align := align;
          alloc_flist(n.obj, offs, l_align, TRUE);
          offs := start;
          n := n.next;
        END;
      END;

      IF offs <= MAX(LONGINT) - mx_size THEN
        INC(offs, mx_size);
      ELSE
        err_type_size(f.host)
      END;
      IF mx_align > list_align THEN list_align := mx_align END;
    ELSE
      env.errors.Fault (f.pos, 962, f.mode);    ---  invalid object mode (%d)
    END;
    f := f.next;
  END;
  align := list_align;
END alloc_flist;

(*
   определяет размер записи без выравнивания и
   предполагая, что размер пустой записи = 0
*)

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
  END;
  fl_align := t_align;
  alloc_flist(t.prof, size, fl_align, FALSE);
  IF (base # NIL) & (b_align > fl_align) THEN
    align := b_align;
  ELSE
    align := fl_align;
  END;
END rec_size0;

PROCEDURE record_size (t : pc.STRUCT;
                VAR size : LONGINT; VAR align : SHORTINT);
BEGIN
  rec_size0(t, size, align);
  IF size = 0 THEN size := tune.empty_rec_size END;
  mk_align(size, align);
END record_size;

(* ----- R e c o r d   D e c l a r a t i o n ---------- *)

PROCEDURE declare_record(t: pc.STRUCT);
  VAR offs : LONGINT;
   t_align : SHORTINT;
BEGIN
<* IF db_def THEN *>
  note("declare_record", t.obj);
  io.print(" ttags=%X, otags =%X\n", t.tags, t.obj.tags);
<* END *>

  IF NOT (at.tmark_processed IN t.marks) THEN
    offs := 0;
    IF t.base # NIL THEN
      declare_record(t.base);
      rec_size0(t.base, offs, t_align);
    END;
    t_align := get_align(t);
    alloc_flist(t.prof, offs, t_align, TRUE);
    IF offs = 0 THEN offs := tune.empty_rec_size END;
    set_size(t, offs);
  END;
END declare_record;

PROCEDURE create_record*(o: pc.OBJECT);
  VAR loc: ir.Local;  a: at.ATTR_EXT;
    name: ir.NameType;
    ty: ir.TypeType; sz: LONGINT;
    align: SHORTINT;
    e_tag: ir.TagType;
BEGIN
  IF at.otag_created IN o.tags THEN
    a := at.attr(o.ext, at.a_self);
    IF a(at.INFO_EXT).procno = at.curr_procno THEN RETURN END;
    at.del_attr(o.ext, at.a_self);
  END;
  ASSERT(o.type.flag IN opt.LangsWithTypeDescriptors);
  declare_record(o.type);
  name := at.make_name("%s'desc", o.name^);

  e_tag := ir.y_RealVar;
  ty    := ir.t_rec;
  sz    := tune.TYP_DESC_size;
  align := at.default_alignment;

 <* IF env_target = "x86nt" THEN *>
  IF from_other_DLL (o) THEN
    e_tag := at.BASE + ir.y_RealVar;
    ty    := tune.addr_ty;
    sz    := tune.addr_sz;
    align := tune.addr_sz;
  END;
 <* END *>

  loc := ir.AddLocal(name, scope_id(pc.mods[o.mno]), align);
  ir.Locals[loc].Obj     := o;
  ir.Locals[loc].VarType := ty;
  ir.Locals[loc].VarSize := sz;
  at.app_info(o, at.a_self, e_tag, VAL(at.InfExtName,loc), NIL, 0);

 <* IF TARGET_RISC OR TARGET_SPARC THEN *>  TOC.Add(o);  <* END *>
  INCL(o.tags, at.otag_created);
END create_record;

PROCEDURE type_definition(o: pc.OBJECT);
BEGIN
  IF o.type.mode = pc.ty_record THEN declare_record(o.type);
  ELSIF o.type.mode = pc.ty_array_of THEN        -- dynarr_definition(o.type);
  ELSE ASSERT(NOT (at.otag_declared IN o.tags)); -- type_declaration(o);
  END;
END type_definition;

PROCEDURE code_func_definition(o: pc.OBJECT);
BEGIN
  ASSERT(o.mode=pc.ob_cproc);
END code_func_definition;

PROCEDURE object_definition(o: pc.OBJECT);
(* окончательное описание объекта *)
BEGIN
  IF pc.OTAG_SET{at.otag_defining,at.otag_declaring}*o.tags#pc.OTAG_SET{} THEN
    env.errors.Error(o.pos, 1018);
    EXCL(o.tags,at.otag_undef);
    ASSERT(o.mode=pc.ob_type);
    RETURN;
  END;
<* IF db_def THEN *> note("object_definition", o); <* END *>
  INCL(o.tags,at.otag_defining);
  CASE o.mode OF
    |pc.ob_var,pc.ob_cons:
       (* var_definition(o); *)
    |pc.ob_proc,pc.ob_xproc,pc.ob_eproc:
       (* func_definition(o);  *)
    |pc.ob_cproc: code_func_definition(o);
    |pc.ob_type: type_definition(o);
  ELSE
  END;
  EXCL(o.tags,at.otag_defining);
  EXCL(o.tags,at.otag_undef);
  INCL(o.tags,at.otag_declared);
END object_definition;

(* --------- A l l o c a t e   G l o b a l   V a r i a b l e s -------------- *)

<* IF TARGET_RISC OR TARGET_SPARC THEN *>
CONST
  MAX_OFFS =   7FFFH;
  MIN_OFFS = - 8000H;
  RESERVED =   200;    -- bytes for constants

TYPE
  one_global_info = RECORD
                      var  : pc.OBJECT;
                      size : LONGINT;
                    END;

  globals_info = RECORD
                   min_offs: LONGINT;
                   max_offs: LONGINT;
                   cnt: INT;
                   inf: POINTER TO ARRAY OF one_global_info;
                 END;

VAR GLOB : globals_info;

PROCEDURE add_info (v: pc.OBJECT; size: LONGINT);
  VAR new: globals_info; l, m, r, n: INT;
BEGIN
  n := GLOB.cnt;
  IF n >= LEN(GLOB.inf^) THEN
    NEW(new.inf, n*2);
    FOR l := 0 TO n-1 DO
      new.inf^[l] := GLOB.inf^[l];
    END;
    GLOB.inf := new.inf;
  END;
  l := 0; r := n;
  WHILE l < r DO
    m := (l+r) DIV 2;
    IF GLOB.inf[m].size >= size THEN r := m;
    ELSE                             l := m+1;
    END
  END;
  FOR m := n-1 TO r BY -1 DO
    GLOB.inf[m+1] := GLOB.inf[m]
  END;
  GLOB.inf^[r].var := v;
  GLOB.inf^[r].size := size;
  INC(GLOB.cnt);
END add_info;

PROCEDURE collect_glob_vars * (obj_list: pc.OBJECT);
  VAR o: pc.OBJECT; sz, offs: LONGINT;
BEGIN
  o := obj_list;
  WHILE o # NIL DO
    IF (o.lev = 0) & (o.mno = at.curr_mno) --  & (o.flag # pc.flag_c)
      & (o.mode = pc.ob_var) -- OR ((o.mode = pc.ob_cons) & NOT is_scalar(o.type)))
    THEN
      sz := type_size(o.type);
      IF at.omark_allocated IN o.marks THEN
        offs := at.get_global_offset(o);
        IF offs < GLOB.min_offs THEN
          GLOB.min_offs := offs;
        ELSIF offs >= GLOB.max_offs THEN
          GLOB.max_offs := offs + sz;
        END;
      ELSE
        add_info(o, sz);
      END;
    END;
    o := o.next;
  END;
END collect_glob_vars;

PROCEDURE alloc_glob_vars * ;
  VAR o: pc.OBJECT;
    offs, size: LONGINT;
    align: SHORTINT;
    --neg : BOOLEAN;
    i: INT;
BEGIN
  --neg := FALSE;
  FOR i := 0 TO GLOB.cnt - 1 DO
    o := GLOB.inf[i].var;
    align := at.default_alignment;
    bytes(o.type, size, align);
(*
    IF neg THEN
      offs := GLOB.min_offs;
      DEC(offs, GLOB.inf[i].size);
      mk_neg_align(offs, align);
      IF offs >= (MIN_OFFS + RESERVED) THEN
        at.set_global_offset(o, offs);
        GLOB.min_offs := offs;
      END;
    ELSE
*)
      offs := GLOB.max_offs;
      mk_align(offs, align);
      IF offs <= (MAX_OFFS - RESERVED) THEN
        at.set_global_offset(o, offs);
        GLOB.max_offs := (offs + GLOB.inf[i].size);
      END;
(*
    END;
*)
    IF offs = 0 THEN
      INCL(at.curr_mod.marks, at.omark_allocated);
      TOC.AddBase(o);
    END;
--    neg := NOT neg;    -- incorrect alignment if we have negative offsets,
                         -- we need more correct algorithm to assign offsets
  END;
END alloc_glob_vars;

PROCEDURE alloc_work_object * (o: pc.OBJECT);
(*
  VAR offs, size: LONGINT;
    align: SHORTINT;
*)
BEGIN
  TOC.Add(o);
(*
  align := at.default_alignment;
  bytes(o.type, size, align);
  offs := GLOB.max_offs;
  mk_align(offs, align);
  IF offs <= MAX_OFFS THEN
    at.set_global_offset(o, offs);
    GLOB.max_offs := (offs + size);
    IF offs = 0 THEN
      TOC.AddBase(o);
      INCL(at.curr_mod.marks, at.omark_allocated);
    END;
  ELSE
    TOC.Add(o);
  END;
*)
END alloc_work_object;

TYPE
  obj_info_rec = RECORD
    o: pc.OBJECT;
    offs: LONGINT;
  END;
  obj_info = POINTER TO
               ARRAY OF obj_info_rec;

VAR inf: obj_info;
   inf_no: INT;     --  общее число занесенных объектов
   based_no : INT;  --  из них - число базированных (остальные самобазированные)

PROCEDURE ini_glob_info;
BEGIN
  NEW(GLOB.inf, 32);
  GLOB.cnt := 0;
  GLOB.min_offs := 0;
  GLOB.max_offs := 0;
  inf_no := 0;
  based_no := 0;
END ini_glob_info;

PROCEDURE clear_glob_info*;  (* save min_offs, max_offs !!! *)
BEGIN
  GLOB.cnt := 0;
END clear_glob_info;

PROCEDURE realloc_inf;
  VAR new : obj_info; i: INT;
BEGIN
  IF inf = NIL THEN
    NEW(inf, 32);
    inf_no := 0;
    based_no := 0;
  ELSIF LEN(inf^) <= inf_no THEN
    NEW(new, LEN(inf^) * 2);
    FOR i := 0 TO LEN(inf^)-1 DO
      new[i] := inf[i];
    END;
    inf := new;
  END;
END realloc_inf;

PROCEDURE collect_self_based (o: pc.OBJECT);
BEGIN
  realloc_inf;
  inf[inf_no].o := o;
  inf[inf_no].offs := 0;
  INC(inf_no);
END collect_self_based;

PROCEDURE collect_object (o: pc.OBJECT);
  VAR i, j: INT; offs: LONGINT;
BEGIN
  realloc_inf;
  offs := at.get_global_offset(o);
  i := 0;
  WHILE (i < based_no) & (inf[i].offs < offs) DO
    INC(i);
  END;
  j := inf_no;
  WHILE j>i DO inf[j] := inf[j-1]; DEC(j) END;
  inf[i].o := o;
  inf[i].offs := offs;
  INC(based_no);
  INC(inf_no);
END collect_object;

PROCEDURE collect_final_info (obj_list: pc.OBJECT);
  VAR o: pc.OBJECT;
BEGIN
  o := obj_list;
  WHILE o # NIL DO
    IF  (o.mno = at.curr_mno) THEN
      CASE o.mode OF
      | pc.ob_var  :
         IF (o.lev = 0) THEN
           IF at.omark_allocated IN o.marks THEN
             collect_object(o)
           ELSE
             collect_self_based(o)
           END;
         END;
      | pc.ob_cons :
         IF (o.lev = 0)  THEN
           IF o.type = NIL THEN
             ASSERT((at.omark_gen_ready IN o.marks)
                    &~(at.omark_allocated IN o.marks));
             collect_self_based(o)
           ELSIF ~is_scalar(o.type) OR (o.type.mode IN pc.REALs)
             OR (at.omark_gen_ready IN o.marks)
           THEN
             --- constant in memory
             IF at.omark_allocated IN o.marks THEN
               collect_object(o)
             ELSE
               collect_self_based(o)
             END;
           END;
         END;
      | pc.ob_type : -- в список нужно внести
                     -- дескриптор типа - обероновской записи
         IF at.omark_gen_ready IN o.marks THEN
           ASSERT(o.type.mode = pc.ty_record);
           collect_self_based(o);
           collect_final_info(o.type.mem);
         END;
      | pc.ob_proc,
        pc.ob_xproc,
        pc.ob_lproc :
         collect_final_info(o.type.mem);
 (*

      | pc.ob_module : -- этого конечно не будет,
                       -- но в список нужно внести дескриптор модуля
  *)
      ELSE
      END;
    END;
    o := o.next;
  END;
END collect_final_info;

VAR get_no : INT;

PROCEDURE GetNextData( VAR o : pc.OBJECT;
                       VAR offs : LONGINT;
                       VAR align : SHORTINT;
                       VAR const : BOOLEAN);
  VAR sz: LONGINT;
BEGIN
  IF get_no >= inf_no THEN
    o := NIL;
    inf := NIL; -- to clear the memory
  ELSE
    o := inf[get_no].o;
    offs := inf[get_no].offs;
    const := (o.mode = pc.ob_cons) OR (o.mode = pc.ob_type);
    IF (o.type = NIL) OR (o.mode = pc.ob_type) THEN
      ASSERT((at.omark_gen_ready IN o.marks) & ~(at.omark_allocated IN o.marks));
      align := 4
    ELSE
      align := at.default_alignment;
      bytes(o.type, sz, align);
    END;
    INC(get_no);
  END;
END GetNextData;

PROCEDURE GetFirstData( VAR o : pc.OBJECT;
                        VAR offs : LONGINT;
                        VAR align : SHORTINT;
                        VAR const : BOOLEAN);
BEGIN
  collect_final_info(at.curr_mod.type.prof);
  collect_final_info(at.curr_mod.type.mem);
  collect_final_info(at.work_objects);
  get_no := 0;
  GetNextData(o, offs, align, const);
END GetFirstData;

PROCEDURE Obj4TOCRegister(o : pc.OBJECT) : pc.OBJECT;
BEGIN
<* IF TARGET_RISC THEN *>
  IF o.mode IN pc.PROCs THEN RETURN pr.ProcDesc(o) END;
<* END *>
  RETURN o;
END Obj4TOCRegister;

<* END *>

(* ------------ C o n s t a n t   V a l u e s ------------------------------- *)

PROCEDURE rval*(x: LONGREAL): pc.VALUE;
  VAR v: pc.VALUE;
BEGIN
  v:=pc.value.new(env.null_pos,pc.RR_type);
  v.set_real(x);
  RETURN v;
END rval;

PROCEDURE val*(x: LONGINT): pc.VALUE;
  VAR v: pc.VALUE;
BEGIN
  v := pc.value.new(env.null_pos, pc.ZZ_type);
  v.set_integer(x);
  RETURN v;
END val;

(*
CONST ValNo = 64;
VAR
  ValCnt : INT;
  VTable : ARRAY ValNo OF
             RECORD
               val: pc.VALUE;
               x:   LONGINT;
             END;

PROCEDURE val*(x: LONGINT): pc.VALUE;
  VAR v: pc.VALUE;
    l, m, r: INT;
BEGIN
  l := 0; r := ValCnt;                    -- search the table for a value "x"
  WHILE l < r DO
    m := (l+r) DIV 2;
    IF VTable[m].x >= x THEN r := m;
    ELSE                     l := m+1;
    END
  END;
  IF (r < ValCnt) & (VTable[r].x = x) THEN
    v := VTable[r].val                    -- found - return VALUE from the table
  ELSE
    v := ival(x);                         -- else create and insert in the table
    IF ValCnt < ValNo THEN
      FOR m := ValCnt-1 TO r BY -1 DO
        VTable[m+1] := VTable[m];
      END;
      VTable[r].val := v;
      VTable[r].x := x;
      INC(ValCnt);
    END;
  END;
  RETURN v
END val;
*)

PROCEDURE ini*;
BEGIN
--  ValCnt := 0;
  SIZE_T := pc.new_type(pc.ty_longcard);
  LSET_T := pc.new_type(pc.ty_longcard);
  INDEX_T := pc.new_type(pc.ty_longint);  (* надо, чтобы хар-ки совпадали *)
  ADDR_T  := pc.new_type(pc.ty_pointer);  (* с информацией в op_tune      *)
  CARD8_T := pc.new_type(pc.ty_shortcard);
  SETOP_TABLE_T := pc.new_type(pc.ty_array);
  MDESC_T := pc.new_type(pc.ty_record);
  set_size(MDESC_T, tune.MOD_DESC_size);
  TDESC_T := pc.new_type(pc.ty_record);
  set_size(TDESC_T, tune.TYP_DESC_size);
  PROFL_T := pc.new_type(pc.ty_record);
  set_size(PROFL_T, PROFL_size);
  PROFL_T.align := tune.addr_sz;
  VOID_T := pc.new_type(pc.ty_void);

  tune.nil_val := val(0); (* надо бы ему соответствующий тип указателя *)
  zz_tmp  := val(0);  -- this VALUE is used as a temporary variable
  zz_zero := val(0);
  zz_one  := val(1);
<*IF NOT NOFLOAT THEN *>
  rr_zero := rval(0);
<*END *>
  bool0   := val(0);  (* правильнее было бы завести их не с типом ZZ ?? *)
  bool1   := val(1);

  md_desc   := NIL;
  last_type := NIL;

  cns_vars:=NIL;
  cns_cnt := 0;
  tmp_vars:=NIL;
  tmp_busy:=NIL;
  temp_no := 0;

  cd.Init;
  std.Ini(ADDR_T, SETOP_TABLE_T);
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  ini_glob_info;
  TOC.Init;
<* END *>
END ini;

PROCEDURE exi*;
--  VAR i: INT;
BEGIN
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  GLOB.inf := NIL;
<* END *>
  std.Exi;
  cd.Exit;

  tune.nil_val := NIL;
  zz_tmp  := NIL;
  zz_one  := NIL;
  zz_zero := NIL;
  rr_zero := NIL;
  bool0   := NIL;
  bool1   := NIL;

  SIZE_T  := NIL;
  LSET_T  := NIL;
  SEQU_T  := NIL;
  ADDR_T  := NIL;
  INDEX_T := NIL;
  CARD8_T := NIL;
  SETOP_TABLE_T := NIL;
  MDESC_T := NIL;
  TDESC_T := NIL;
  VOID_T  := NIL;
  md_desc := NIL;
  last_type := NIL;
  at.curr_proc := NIL;
  at.curr_mod := NIL;

  cns_vars :=NIL;
  cns_cnt  := 0;
  tmp_vars :=NIL;
  tmp_busy :=NIL;
(*
  FOR i := 0 TO LEN(VTable)-1 DO
    VTable[i].val := NIL;
  END
*)
END exi;

BEGIN
--  ValCnt := 0;
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  at.alloc_work_object := alloc_work_object;
  TOCData.GetFirstData := GetFirstData;
  TOCData.GetNextData  := GetNextData;
  TOC.Obj4TOCRegister := Obj4TOCRegister;
<* END *>
END opDef.

