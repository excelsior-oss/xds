<* NEW statistics- *>  -- to obtain some statistics

<* IF ~ DEFINED(db_code)  THEN *> <* NEW db_code- *>  <* END *>
<* IF ~ DEFINED(db_procs)  THEN *> <* NEW db_procs- *>  <* END *>

MODULE opCode;

(** Головной модуль для подключения native-кодогенерации *)

IMPORT
  pc   := pcK,                                      ir,
  def  := opDef,                              gr := ControlGraph,
  ope  := opE,                                at := opAttrs,
  pr   := opProcs,                                  ssa,
  tune := opTune,                                   Optimize,
  std  := opStd,                                    Calc,
  xfs  := xiFiles,                                  Color,
  env  := xiEnv,                                    Modify,
          xcStr,                             cmd := CodeDef,
  str  := Strings,                                  PrepGen,
  opt  := Options,                            fc := CodeFace,
  nms  := ObjNames,                          reg := Registry,
  Analysis,
<* IF TARGET_IDB THEN *>
  RelManager,
<* END *>
  Polymorph; -- do not remove this import!!! It is not redundant!
<* IF TARGET_386 THEN *> IMPORT xProfRTS; <* END *>

<* IF DEFINED(OVERDYE) AND OVERDYE THEN *> -- FIXME
IMPORT od := OverDye;
<* END *>

<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  IMPORT TOC;
<* END *>

IMPORT
  Select := Iselect,
  KillDead,
<* IF db_code OR pcvis THEN *> io := opIO,     <* END *>
<* IF pcvis THEN *>            pcVis,          <* END *>
<* IF gen_qfile THEN *>        TestIO,         <* END *>
  SYSTEM;


IMPORT dbg := DbgFace;
IMPORT pcO;

TYPE INT = ir.INT;

<* IF pcvis THEN *>
CONST EQU_VIS_PROC = "VIS_PROC";
<* END *>

<* IF gen_qfile THEN *>
CONST EQU_QFILE_PROC = "QFILE_PROC";
<* END *>
  TYPE rr =  ARRAY OF CHAR;
  CONST
<* IF TARGET_VAX THEN *>
    max_lr = 1.701411834604691E+38;
    min_lr = -max_lr;
    max_r  = 1.7014117E+38;
    min_r  = -max_r;
<* ELSIF TARGET_RISC & ((env_target="aix") OR (env_target="ppcaix")) THEN *>
    max_lr = rr{07fX,0efX,0ffX,0ffX,0ffX,0ffX,0ffX,0ffX};
    min_lr = rr{0ffX,0efX,0ffX,0ffX,0ffX,0ffX,0ffX,0ffX};
    max_r  = rr{047X,0efX,0ffX,0ffX,0e0X,000X,000X,000X};
    min_r  = rr{0c7X,0efX,0ffX,0ffX,0e0X,000X,000X,000X};
<* ELSE *>
    max_lr = rr{0ffX,0ffX,0ffX,0ffX,0ffX,0ffX,0efX,07fX};
    min_lr = rr{0ffX,0ffX,0ffX,0ffX,0ffX,0ffX,0efX,0ffX};
    max_r  = rr{000X,000X,000X,0e0X,0ffX,0ffX,0efX,047X};
    min_r  = rr{000X,000X,000X,0e0X,0ffX,0ffX,0efX,0c7X};
<* END *>


<* IF db_code THEN *>

PROCEDURE PrintLocals ();
  VAR i: INT;
BEGIN
  io.needed := TRUE;
  IF (at.curr_proc # NIL) & (at.curr_proc.name # NIL) THEN
    io.print("\n-------- List of locals [%s] -------\n", at.curr_proc.name^);
  ELSE
    io.print("\n-------- List of locals [BEGIN] -------\n");
  END;
  FOR i := 0 TO ir.NLocals-1 DO
    io.print("Scope = %d Name='", ir.Locals[i].Scope);
    IF ir.Locals[i].Name # NIL THEN
      io.print("%s", ir.Locals[i]. Name^);
    END;
    io.print("' ");
    IF (ir.Locals[i].Obj # NIL) & (ir.Locals[i].Obj.name # NIL) THEN
      io.print(" Obj='%s' ", ir.Locals[i].Obj.name^);
    END;
    io.print(" Align=%d ", ir.Locals[i].Align);
    io.print(" Size=%d ", ir.Locals[i].VarSize);
    io.print(" Type=%d ", ir.Locals[i].VarType);
    io.print(" Offset=%d ", ir.Locals[i].Offset);
    IF ir.Locals[i].Addressed THEN io.print("Addressed") END;
    io.print("\n");
  END;
  io.print("-------------------------------\n");
END PrintLocals;

<* END *>


(* ------------- O p t i m i z a t i o n ---------------------------- *)
VAR
  try_block : BOOLEAN;     (* there is excepton handler in the current proc *)
  was_alloca: BOOLEAN;     (* was dynamic alloc on the stack *)

PROCEDURE MakeNewRet(VAR n: ir.Node;
                     VAR loc: ir.Local; ty: ir.TypeType; sz: ir.SizeType);
  VAR q: ir.TriadePtr;
    name : ir.NameType; al: SHORTINT;

BEGIN
  n := gr.NewNode();
  IF ty =ir.t_void THEN
    loc := ir.UNDEFINED;
    q := ir.NewTriadeInit (0, ir.o_ret, ir.t_void, 0);
  ELSE
    name := at.make_name("%s'ret", at.curr_proc.name^);
    IF at.default_alignment > ORD(sz) THEN al := VAL(SHORTINT, sz);
    ELSE                              al := at.default_alignment
    END;
    loc := ir.AddLocal(name, def.local, al);
    ir.Locals^[loc].VarType := ty;
    ir.Locals^[loc].VarSize := sz;
    q := ir.NewTriadeInit (1, ir.o_ret, ty, sz);
    ir.MakeParLocal(q.Params[0], loc)
  END;
  q.Position := at.curr_proc.val.end;
  ASSERT(NOT q.Position.IsNull() OR (pc.omark_used_by_code IN at.curr_proc.marks));
  gr.PutTriadeLast(q, n);
END MakeNewRet;

PROCEDURE ReplaceRet(n: ir.Node;       -- replace o_ret in this node
                     ret: ir.Node;     -- GOTO that node
                     loc: ir.Local);   -- use loc to store return value
  VAR q: ir.TriadePtr; tpos : ir.TPOS;
BEGIN
  q := ir.Nodes^[n].Last;
  ASSERT(q^.Op = ir.o_ret);
  IF q.ResType = ir.t_void THEN
    q.Op := ir.o_goto;
  ELSE
    tpos := q.Position;
    q.Op := ir.o_assign;
    q.Tag := ir.y_RealVar;
    q.Name := loc;
    q := ir.NewTriadeInit(0, ir.o_goto, ir.t_void, 0);
    q.Position := tpos;
    gr.PutTriadeLast(q, n);
  END;
  INCL(q.Options, ir.o_Silent);
  gr.NewArc(n, ret, FALSE);
END ReplaceRet;


PROCEDURE UnifyReturns;
  VAR n, first_ret, new_ret: ir.Node;
    loc: ir.Local;
    q: ir.TriadePtr;
BEGIN
  IF NOT (at.debug IN at.COMP_MODE) & (at.SPACE IN at.COMP_MODE) THEN
    RETURN
  END;
  first_ret := ir.UndefNode;
  new_ret   := ir.UndefNode;
  loc := ir.ZEROVarNum;
  n   := ir.ZERONode;
  LOOP
    IF n >= ir.Nnodes THEN EXIT END;
    IF ir.Nodes^[n].Alive & (ir.Nodes^[n].NOut = 0) & (n # new_ret) THEN
      q := ir.Nodes^[n].Last;
      IF q^.Op = ir.o_ret THEN
        IF first_ret = ir.UndefNode THEN first_ret := n;
        ELSE
          IF new_ret = ir.UndefNode THEN
            MakeNewRet(new_ret, loc, q.ResType, q.ResSize);
            ReplaceRet(first_ret, new_ret, loc);
          END;
          ReplaceRet(n, new_ret, loc);
        END;
      END;
    END;
    INC(n);
  END;
END UnifyReturns;

(*
  Create fake object for debugger
*)

PROCEDURE make_return_object (mode : pc.OB_MODE) : pc.OBJECT;
  VAR o: pc.OBJECT;
     nm: pc.STRING;
BEGIN
  nm := at.make_name("RETURN");
  o := at.new_work_object(nm, at.curr_proc.type.base, at.curr_proc.type, mode, FALSE);
  at.work_objects := o.next;
  o.next := at.curr_proc.type.prof;
  at.curr_proc.type.prof := o;
  RETURN o
END make_return_object;

PROCEDURE get_proc_ret_info (VAR di: ir.DebugInfo);
BEGIN
<* IF TARGET_VAX THEN *>    -- we should take that information from somewhere
  di.Location := ir.LocInReg;
  di.Value    := 0;
<* ELSE *>
  di.Location := ir.LocUndef;
  di.Value    := ir.UNKNOWN_OFFSET;
<* END *>
END get_proc_ret_info;

PROCEDURE put_result_info (p: pc.OBJECT);
  VAR
    a: at.ATTR_EXT;  inf: at.INFO_EXT;
    mode: pc.OB_MODE;  obj: pc.OBJECT;
    di: ir.DebugInfo; db: at.DBG_EXT;
BEGIN
  IF p.type.base.mode = pc.ty_void THEN RETURN END;
  di.Location := ir.LocUndef;
  a := at.attr(p.ext, at.a_rtn);
  IF a = NIL THEN
    mode := pc.ob_var;
    get_proc_ret_info (di)
  ELSE
    mode := pc.ob_varpar;
    inf := a(at.INFO_EXT);
    IF inf.e_tag = ir.y_RealVar THEN
      di := ir.Locals [VAL(ir.VarNum, inf.name)].Debug;
    END;
  END;
  IF di.Location # ir.LocUndef THEN
    obj := make_return_object (mode);
    NEW(db); NEW(db.list, 1);
    db.list[0].what := at.a_rtn;
    db.list[0].info := di;
    at.app_obj_attr (obj, db, at.a_dbg);
  END;
END put_result_info;

PROCEDURE put_var_info (o: pc.OBJECT);
  CONST
    MAX_ATTR_NO = 10;
  VAR
    at_arr: ARRAY MAX_ATTR_NO OF SHORTINT;
    di_arr: ARRAY MAX_ATTR_NO OF ir.DebugInfo;
    n: INT;

  PROCEDURE get_db_info (atr: SHORTINT);
    VAR a: at.ATTR_EXT;  inf: at.INFO_EXT; e_tag: ir.TagType;
  BEGIN
    a := at.attr(o.ext, atr);
    IF a # NIL THEN
      inf := a(at.INFO_EXT);
      e_tag := inf.e_tag;
      IF ((e_tag = ir.y_RealVar) OR (e_tag = (at.BASE + ir.y_RealVar)))
        & (ir.Locals [VAL(ir.VarNum, inf.name)].Debug.Location # ir.LocUndef)
      THEN
        at_arr [n] := atr;
        di_arr [n] := ir.Locals [VAL(ir.VarNum ,inf.name)].Debug;
        INC (n);
      END;
    END;
  END get_db_info;

  VAR db: at.DBG_EXT;  i: SHORTINT;
BEGIN
  IF ~nms.valid_name(o.name) THEN RETURN END;
  n := 0;
  get_db_info (at.a_self);
  IF n # 0 THEN
    IF o.type.mode = pc.ty_array_of THEN
      FOR i := 0 TO o.type.len - 1 DO
        get_db_info (at.a_len+i)
      END;
    ELSIF (o.type.mode = pc.ty_record) & (o.type.flag IN opt.LangsWithTypedRecords )
      & (o.mode = pc.ob_varpar)
    THEN
      get_db_info (at.a_type);
    ELSIF (o.type.mode IN pc.CPLXs)
      & (o.mode = pc.ob_var) & (pc.OTAG_SET{pc.otag_RO,pc.otag_valpar}*o.tags # pc.OTAG_SET{})
    THEN
      get_db_info (at.a_im);
    END;
    IF n > 0 THEN
      NEW(db); NEW(db.list, n);
      REPEAT
        DEC (n);
        db.list[n].what := at_arr[n];
        db.list[n].info := di_arr[n];
      UNTIL n = 0;
      at.app_obj_attr (o, db, at.a_dbg);
    END;
  END;
END put_var_info;

PROCEDURE put_param_info (p: pc.OBJECT);
BEGIN
  put_var_info (p);
END put_param_info;

PROCEDURE process_var_debug_info;
  VAR o: pc.OBJECT;
BEGIN
  put_result_info (at.curr_proc);
  o := at.curr_proc.type.prof;
  WHILE o # NIL DO
    put_param_info (o);
    o := o.next;
  END;
  o := at.curr_proc.type.mem;
  WHILE o # NIL DO
    IF o.mode  = pc.ob_var THEN
      put_var_info (o);
    END;
    o := o.next;
  END;
END process_var_debug_info;

VAR
  last_name_len : INT;     (* name length for the previous procedure *)

PROCEDURE Optim_Init;   (* первоначальная инициализация оптимизатора *)
BEGIN
  last_name_len := 0;
  Calc.Init;
  at.was_float_triade := FALSE;

<*IF gen_qfile THEN *>
    TestIO.Init();
<* END *>
END Optim_Init;

PROCEDURE Optim_Start;  (* начало новой процедуры *)
BEGIN
  try_block := FALSE;
  was_alloca:= FALSE;
  pr.MustFillFrame := FALSE;    (* no initialization for current proc's frame *)
  gr.NewGraph;
  ir.NewLocals;
  gr.Init;
END Optim_Start;

PROCEDURE must_be_frame(): BOOLEAN; (* create frame for current procedure *)
BEGIN
  RETURN was_alloca
      OR (at.use_frame_ptr IN at.COMP_MODE)
      OR (pc.omark_code IN at.curr_proc.marks)
END must_be_frame;

PROCEDURE WriteTest (id: CHAR; N-: ARRAY OF CHAR);
BEGIN
<* IF gen_qfile THEN *>
  TestIO.WriteTest (id, N);
<* END *>
END WriteTest;

PROCEDURE Optim_Do;  (* обработка созданного графа *)
  VAR nm: pc.STRING;
    s: env.String;
BEGIN
 <* IF DEFINED(OVERDYE) AND OVERDYE THEN *> -- FIXME
  od.ClearNodePosOrders;
 <* END *>

  IF at.curr_proc # NIL THEN
    nm := at.curr_proc.GetReadableName(FALSE);
  ELSE
    nm := at.curr_mod.GetReadableName(FALSE);
  END;
  IF env.shell.Active () THEN
    xcStr.dprn_txt (s,"Optimizing %s", nm^);
    env.shell.Comment  (s^);
    env.shell.Progress (0, 1);
  ELSIF (env.dc_progress IN env.decor) THEN
     env.info.print("\rOptimizing");
  END;

<* IF TARGET_IDB THEN *>
  IF env.InterViewMode THEN
    RETURN;
  END;
<* END *>

<* IF gen_qfile THEN *>
  TestIO.curr_proc_name := nm;
<* END *>

<* IF db_code THEN *>
  PrintLocals;
<* END *>

  WriteTest("1", "first (1)");
  gr.KillDead;
  gr.FindDominators;
  WriteTest("i", "initial (i)");
  UnifyReturns;
  WriteTest ("r", "after UnifyReturns (r)");

  IF env.errors.err_cnt # 0 THEN RETURN END;

  Optimize.Init();
  gr.KillDead;

  IF at.COMP_MODE * at.CompModeSet{at.debug, at.SPACE} = at.CompModeSet{} THEN
    Modify.UnrollFors;
  END;
  gr.FindDominators;
  WriteTest ("u", "after UnrollFors (u)");

  Optimize.OptimizeReturns;
  gr.FindLoops;
  WriteTest ("b", "before ConvertToSSA (b)");
  ssa.ConvertToSSA (try_block);
  WriteTest ("s", "after ConvertToSSA (s)");

<* IF TARGET_IDB THEN *>
  IF env.InterViewMode THEN
   -- RelManager.after_convert_to_SSA();
    RETURN;
  END;
<* END *>

  KillDead.KillDeadCode;
  IF NOT (at.nooptimize IN at.COMP_MODE) THEN
    Optimize.MachineLevelOptimizations := FALSE;
    WriteTest ("o", "Before OptimizeGraph (o)");
    Optimize.OptimizeGraph;
  END;
  WriteTest ("m", "Before Modify.Decompose (m)");
  Modify.Decompose;
  WriteTest ("d", "After Modify.Decompose (d)");
  IF NOT (at.nooptimize IN at.COMP_MODE) THEN
    Modify.OptimizeLoops;
    Optimize.OptimizeGraph;
  ELSE
    Optimize.NoOptimizeGraph;
  END;
  KillDead.KillDeadCode;
  gr.FindDominators;
  WriteTest ("O", "machine level optimizations (O)");
  Optimize.MachineLevelOptimizations := TRUE;
  IF NOT (at.nooptimize IN at.COMP_MODE) THEN
    Optimize.OptimizeGraph;
  ELSE
    Optimize.NoOptimizeGraph;
  END;
  Optimize.Done();
  WriteTest ("p", "before PrepGen (p)");
  PrepGen.Prepare;
  Modify.InsertNodes;
  WriteTest ("c", "before Color (c)");
  Color.ColorVariables;
  ir.MarkAllocatedLocals;   -- fills ir.allocatedLocals
  WriteTest ("f", "final (f)");

 <* IF DEFINED(OVERDYE) AND OVERDYE THEN *> -- FIXME
  IF at.DbgRefine IN at.COMP_MODE THEN
    -- Calculate node positions order...
    od.SetInitialNodePos;
    -- ...and reorder triades positions for each node
    od.SmoothTriadePositions;
   <* IF gen_qfile THEN *>
    TestIO.WriteNodeOrder (2, nm);
   <* END *>
  END;
 <* END *>

  IF ~env.shell.Active () & (env.dc_progress IN env.decor) THEN
     env.info.print("\rGenerating");
  END;

  Select.IDB.Generate(at.was_float_triade, must_be_frame(), was_alloca);
  ir.ClearAllocatedLocals;

  IF at.curr_proc.mode # pc.ob_module THEN -- do not process global variables
    process_var_debug_info;
  END;

  Color.NNonTempVars := ir.UNDEFINED;
  Color.RegAllocOk   := FALSE;

  Select.IDB.ClearAll;
  IF ~env.shell.Active () & (env.dc_progress IN env.decor) THEN
    env.info.print("\r");
  END;
END Optim_Do;

(** ------------- s t a n d a r d   p r o c e d u r e s ---------- *)

PROCEDURE gen_incl_excl(n: pc.NODE);
  VAR p: ir.TriadePtr;
    larg, rarg, arg, delt: ir.Arg;
    ty: ir.TypeType; sz: ir.SizeType;
    chk_opt: ir.OptionsSet;
    type: pc.STRUCT;
BEGIN
  IF pc.ntag_chk_range IN n.tags THEN chk_opt := ir.OptionsSet{ir.o_Range}
  ELSE                                chk_opt := ir.OptionsSet{}
  END;
  ope.gen_index_check(n.r.next, n.r, 0, chk_opt, rarg);
  type := n.r.type;
  IF  def.is_scalar(type) THEN
    ope.gen_value(n.r, ir.GenModeSet{def.LVL}, larg);
    def.type_info(type, ty, sz);
  ELSE
    ope.gen_value(n.r, ir.GenModeSet{def.REF}, larg);
    ir.MakeArgNum(arg, def.val(tune.BITSET_LEN));
    ope.gen_bin_op(n.pos, ir.o_div, rarg, arg, tune.index_ty, tune.index_sz, arg);
    ir.MakeArgNum(delt, def.val(tune.lset_sz));
    ope.gen_bin_op(n.pos, ir.o_mul, arg, delt, tune.index_ty, tune.index_sz, arg);
    ope.gen_bin_op(n.pos, ir.o_add, larg, arg, tune.addr_ty, tune.addr_sz, larg);
    INCL(larg.mode, def.REF);
    ir.MakeArgNum(arg, def.val(tune.BITSET_LEN));
    ope.gen_bin_op(n.pos, ir.o_mod, rarg, arg, tune.index_ty, tune.index_sz, rarg);
    ty := tune.lset_ty;
    sz := tune.lset_sz;
  END;
  IF n.sub = pc.sp_incl THEN
    p := ir.NewTriadeInit(2, ir.o_incl, ty, sz);
  ELSE
    p := ir.NewTriadeInit(2, ir.o_excl, ty, sz);
  END;
  p.Position := n.pos;
  ir.ParmByArg(p.Params[1], rarg);
  IF def.REF IN larg.mode THEN
    arg := larg;
    def.deref(n.pos, ty, sz, ir.OptionsSet{}, arg);
    ir.ParmByArg(p.Params[0], arg);
    ir.GenResVar(p);
    gr.AppendTr(p);
    ir.MakeArgVar(arg, p.Name);
    ope.gen_storer(n.pos, ty, sz, larg, arg);
  ELSE
    ir.ParmByArg(p.Params[0], larg);
    p.Tag  := ir.y_RealVar;
    p.Name := larg.name;
    gr.AppendTr(p);
  END;
END gen_incl_excl;

<* IF TARGET_386 THEN *>

PROCEDURE gen_embedded_code(n: pc.NODE);
  VAR
    q: ir.TriadePtr;
    arg : ir.Arg;
BEGIN
  def.o_usage(n.pos, n.obj, ir.GenModeSet{}, arg);
  q := ir.NewTriadeInit(1, ir.o_call, ir.t_void, 0);
  q.Position := n.pos;
  q.Prototype := std.AsmCodeProtoNum;
  ir.ParmByArg(q.Params[0], arg);
  gr.AppendTr(q);
END gen_embedded_code;

<* END *>

PROCEDURE gen_sproc(n: pc.NODE);
  VAR nr: pc.NODE;
    then_node, end_node: ir.Node;
    q: ir.TriadePtr;
    larg, arg: ir.Arg;
    type: pc.STRUCT;
    ty: ir.TypeType; sz: ir.SizeType;

  PROCEDURE min (a,b: SHORTINT): SHORTINT;
  BEGIN
    IF a < b THEN RETURN a ELSE RETURN b END;
  END min;

  PROCEDURE addr_align (n: pc.NODE): SHORTINT;
    VAR type : pc.STRUCT; a1, a2: SHORTINT;
  BEGIN
    IF (n.mode = pc.nd_unary) & (n.sub = pc.su_adr) OR (n.sub = pc.su_adr_o2) THEN
      a1 := def.type_align (n.l.type);
    ELSE
      a1 := 1;
    END;
    type := n.type;
    ASSERT (type.mode = pc.ty_pointer);
    a2 := def.type_align (type.base);
    IF a2 > a1 THEN
      RETURN a2
    ELSE
      RETURN a1
    END;
  END addr_align;

BEGIN
  CASE n.sub OF
  | pc.sp_put:
      nr := n.r;
      type := nr.next.type;
      ope.gen_value(nr, ir.GenModeSet{}, larg);
      IF def.is_scalar(type) THEN
        ope.gen_value(nr.next, ir.GenModeSet{}, arg);
        def.type_info(type, ty, sz);
        ope.gen_storer(n.pos, ty, sz, larg, arg);
      ELSE
        q := ir.NewTriadeInit(3, ir.o_copy, ir.t_void, 0);
        q.Position := n.pos;
        ir.ParmByArg(q.Params[1], larg);
        ope.gen_value(nr.next, ir.GenModeSet{def.REF}, arg);
        ir.ParmByArg(q.Params[0], arg);
        ope.gen_size(nr.next, 0, arg);
        ir.ParmByArg(q.Params[2], arg);
        q.NPar := min (addr_align (nr), def.type_align (type));
        gr.AppendTr(q);
      END;

  | pc.sp_get:
      nr := n.r;
      type := nr.next.type;
      ope.gen_value(nr, ir.GenModeSet{}, larg);
      IF def.is_scalar(type) THEN
        q := def.NewTriadeTS(1, ir.o_loadr, type);
        q.Position := n.pos;
        ir.ParmByArg(q.Params[0], larg);
        ope.gen_value(nr.next, ir.GenModeSet{def.LVL}, arg);
        IF def.REF IN arg.mode THEN
          ir.GenResVar(q);
          gr.AppendTr(q);
          ir.MakeArgVar(larg, q.Name);
          def.type_info(type, ty, sz);
          ope.gen_storer(n.pos, ty, sz, arg, larg);
        ELSE
          q.Tag  := ir.y_RealVar;
          q.Name := arg.name;
          gr.AppendTr(q);
        END;
      ELSE
        q := ir.NewTriadeInit(3, ir.o_copy, ir.t_void, 0);
        q.Position := n.pos;
        ir.ParmByArg(q.Params[0], larg);
        ope.gen_value(nr.next, ir.GenModeSet{def.REF}, arg);
        ir.ParmByArg(q.Params[1], arg);
        ope.gen_size(nr.next, 0, arg);
        ir.ParmByArg(q.Params[2], arg);
        q.NPar := min (addr_align (nr), def.type_align (type));
        gr.AppendTr(q);
      END;

  | pc.sp_new, pc.sp_sysnew, pc.sp_dispose (*, pc.sp_resize*) :
      ope.gen_call_storage(n);

  | pc.sp_assert:
      nr := n.r;
      IF nr.mode # pc.nd_value THEN
        end_node  := gr.NewNode();
        then_node := gr.NewNode();
        ope.gen_condition(nr, end_node, then_node);
        gr.currNode := then_node;
      ELSIF NOT nr.val.is_zero() THEN RETURN
      ELSE
        end_node := ir.UndefNode;
      END;
      IF nr.next # NIL THEN ope.gen_value(nr.next,ir.GenModeSet{},arg);
      ELSE ir.MakeArgNum(arg, def.val(tune.assertException));
      END;
      ope.gen_trap(arg, n.pos, TRUE);
      IF nr.mode # pc.nd_value THEN
        gr.currNode := end_node
      END;

  | pc.sp_incl, pc.sp_excl:
      gen_incl_excl(n);

  | pc.sp_halt:  (* HALT(n) *)
      ope.gen_value(n.r, ir.GenModeSet{}, arg);
      ope.gen_halt(n.pos, arg);

  | pc.sp_abort: (* HALT;   *)
      ir.MakeArgNum(arg, def.val(tune.ABORT_EXIT_CODE));
      ope.gen_halt(n.pos, arg);

  | pc.sp_move:
      nr := n.r;
      q := ir.NewTriadeInit(3, ir.o_copy, ir.t_void, 0);
      q.Position := n.pos;
      ope.gen_value(nr, ir.GenModeSet{}, arg);
      ir.ParmByArg(q.Params[0], arg);
      ope.gen_value(nr.next, ir.GenModeSet{}, arg);
      ir.ParmByArg(q.Params[1], arg);
      ope.gen_value(nr.next.next, ir.GenModeSet{}, arg);
      ir.ParmByArg(q.Params[2], arg);
      q.NPar := min (addr_align (nr), addr_align (nr.next));
      gr.AppendTr(q);

  | pc.sp_fill:
      q := def.RTS_call(n.pos, std.X2C_MEMSET, TRUE);
      ope.gen_value(n.r, ir.GenModeSet{}, arg);
      ir.ParmByArg(q.Params[1], arg);
      ope.gen_value(n.r.next, ir.GenModeSet{}, arg);
      ir.ParmByArg(q.Params[2], arg);
      ope.gen_value(n.r.next.next, ir.GenModeSet{}, arg);
      ir.ParmByArg(q.Params[3], arg);
      gr.AppendTr(q);

  | pc.sp_copy:
      ope.gen_value(n.r.next, ir.GenModeSet{def.REF}, arg);
      ope.gen_len(n.r.next, 0, def.SIZE_T, larg);
      ope.gen_copy(n.r, arg, larg, n.pos);

  | pc.sp_casetrap:
      ope.gen_trap_N(tune.caseSelectException, n.end);

<* IF TARGET_386 THEN *>
  | pc.sp_code:
      ASSERT ((n.r.mode=pc.nd_value) & (n.r.next=NIL));
      gen_embedded_code (n);
<* END *>

  ELSE
(* ------------------
?  sp_getreg*  = 94;
?  sp_putreg*  = 96;
?  sp_enter*   =100;
?  sp_leave*   =101;
-------------------- *)
    env.errors.Error(n.pos,142);    --- "Can not generate standard proc";
  END;
END gen_sproc;

(** ------------- labels & gotos ------------------------------------- *)
CONST
  ntag_exit_label  = pc.ntag_elsif_node;  (* есть выходная метка *)

TYPE
  label_rec = RECORD
                triade_node: ir.Node;
                tree_node  : pc.NODE;
              END;
  label_list = POINTER TO ARRAY OF label_rec;

VAR
  exit_list : label_list;
  exit_cnt  : INT;

PROCEDURE set_label(n: pc.NODE; nd: ir.Node);
  VAR len, i: INT;
    old: label_list;
BEGIN
  ASSERT(NOT (ntag_exit_label IN n.tags));
  len := LEN(exit_list^);
  IF len = exit_cnt THEN
    old := exit_list;
    NEW(exit_list, len*2);
    FOR i := 0 TO len-1 DO exit_list[i] := old[i] END;
  END;
  exit_list[exit_cnt].triade_node := nd;
  exit_list[exit_cnt].tree_node   := n;
  INC(exit_cnt);
  INCL(n.tags, ntag_exit_label);
END set_label;

PROCEDURE get_label(n: pc.NODE): ir.Node;
  VAR i: INT;
BEGIN
  IF NOT (ntag_exit_label IN n.tags) THEN RETURN ir.UndefNode END;
  i := 0;
  LOOP
    IF exit_list[i].tree_node = n THEN
      RETURN exit_list[i].triade_node
    END;
    INC(i);
  END;
END get_label;

PROCEDURE label(n: pc.NODE): ir.Node;
  VAR nd : ir.Node;
BEGIN
  IF ntag_exit_label IN n.tags THEN
    RETURN get_label(n)
  ELSE
    nd := gr.NewNode();
    set_label(n, nd);
    RETURN nd
  END;
END label;

PROCEDURE ini_labels;
BEGIN
  exit_cnt := 0;
END ini_labels;

(** ------------- statments ------------------------------------------ *)

VAR -- proc_end: ir.Node;
    ret_tmp: ir.Local;

PROCEDURE ^ gen_sequence(n: pc.NODE;
                    go_ret: BOOLEAN;  (* return - переход в конец блока *)
                      tail: BOOLEAN;  (* есть еще какие-то действия, которые надо сделать *)
              VAR dead_end: BOOLEAN);

PROCEDURE gen_alt(n: pc.NODE; go_ret, tail: BOOLEAN; end: ir.Node);
   VAR dd: BOOLEAN;
BEGIN
<* IF db_code THEN *>
  IF n # NIL THEN io.print_pos(n.pos) END;
  io.print(" => gen_alt( end = %d)\n",end);
<* END *>

  gen_sequence(n, go_ret, tail, dd);

  IF NOT dd THEN
    IF gr.currNode = ir.UndefNode THEN gr.StartNewNode END;
    gr.Goto (end);
  END;

<* IF db_code THEN *>
  io.print(" <= gen_alt( end = %d)\n",end);
<* END *>
END gen_alt;

PROCEDURE gen_statement_if(n: pc.NODE; go_ret, tail: BOOLEAN; end_node: ir.Node);
  VAR nn: pc.NODE;
    then_node, else_node: ir.Node;
BEGIN
  then_node := gr.NewNode();
  nn := n.r.r;
  IF nn = NIL THEN else_node := end_node ELSE else_node := gr.NewNode() END;
  ope.gen_condition(n.l, then_node, else_node);
  gr.currNode := then_node;
  gen_alt(n.r.l, go_ret, tail, end_node);
  IF nn # NIL THEN
    gr.currNode := else_node;
    gen_alt(nn, go_ret, tail, end_node);
  END;
END gen_statement_if;

PROCEDURE gen_eternal_value (n: pc.NODE; VAR arg: ir.Arg);
  VAR q: ir.TriadePtr; v: ir.Local;
BEGIN
  ASSERT(def.is_scalar(n.type));
  ope.gen_value(n, ir.GenModeSet{}, arg);
  IF arg.tag = ir.y_RealVar THEN
    v := arg.name;
    q := def.NewTriadeTS(1, ir.o_assign, n.type);
    q.Position := n.pos;
    ir.MakeParLocal(q.Params[0], v);
    ir.GenResVar(q);
    gr.AppendTr(q);
    ir.MakeArgVar(arg, q.Name);
  END;
END gen_eternal_value;

PROCEDURE gen_statement_for (n: pc.NODE; go_ret: BOOLEAN);
  VAR stp: pc.VALUE;
    dd: BOOLEAN;
    arg, ind_arg, larg, rarg : ir.Arg;
    p: ir.TriadePtr;
    header, body, end_for : ir.Node;
    type: pc.STRUCT;
    ty: ir.TypeType; sz: ir.SizeType;
    tmp: ir.Local;
BEGIN
  stp := n.l.val;
  IF stp = NIL THEN stp := def.zz_one END;
  type := n.obj.type;
  def.o_usage(n.pos, n.obj, ir.GenModeSet{def.LVL}, arg);
  IF def.REF IN arg.mode THEN
    def.type_info(type, ty, sz);
    def.make_temp_var(ty, sz, tmp);
    ir.MakeArgLocal(ind_arg, tmp, 0);
  ELSE
    ind_arg := arg;
  END;
  ope.gen_value(n.l.l, ir.GenModeSet{}, larg);
  gen_eternal_value(n.l.r, rarg);

  p := def.NewTriadeTS(3, ir.o_forstart, type);
  p.Position := n.pos;
  ir.ParmByArg(p.Params[0], larg);
  ir.ParmByArg(p.Params[1], rarg);
  ir.MakeParNum (p.Params[2], stp);
  ASSERT(ind_arg.tag = ir.y_RealVar);
  p.Tag  := ir.y_RealVar;
  p.Name := ind_arg.name;
  gr.AppendTr(p);
  header := gr.currNode;

  gr.StartNewNode();
  body := gr.currNode;
  end_for := gr.NewNode();
  gr.NewArc(header, body, TRUE);
  gr.NewArc(header, end_for, FALSE);

  IF def.REF IN arg.mode THEN
    def.type_info(type, ty, sz);
    ope.gen_storer(n.pos, ty, sz, arg, ind_arg)
  END;

  gen_sequence(n.r, go_ret, TRUE, dd);

  p := def.NewTriadeTS(3, ir.o_forcont, type);
  ASSERT(~n.end.IsNull() OR (pc.ntag_constrinlined IN n.tags));
  p.Position := n.end;
  ir.ParmByArg(p.Params[0], ind_arg);
  ir.ParmByArg(p.Params[1], rarg);
  ir.MakeParNum (p.Params[2], stp);
  ASSERT(ind_arg.tag = ir.y_RealVar);
  p.Tag  := ir.y_RealVar;
  p.Name := ind_arg.name;
  gr.AppendTr(p);

  gr.NewArc(gr.currNode, body, TRUE);
  gr.NewArc(gr.currNode, end_for, TRUE);
  gr.StartNode(end_for);
END gen_statement_for;

PROCEDURE num_cases(n : pc.NODE): INTEGER; (* число меток/пар меток *)
 VAR cnt : INTEGER; l,m: pc.NODE;
BEGIN
  ASSERT((n.mode=pc.nd_casedo) OR (n.mode=pc.nd_caselse));
  cnt := 0;
  l := n.l;
  WHILE l # NIL DO
    ASSERT(l.mode=pc.nd_node);
    m := l.l;
    WHILE m # NIL DO INC(cnt); m := m.next END;
    l := l.next;
  END;
  RETURN cnt
END num_cases;

PROCEDURE gen_statement_case(n: pc.NODE; go_ret, tail: BOOLEAN);
  VAR sz: ir.SizeType; ty: ir.TypeType;
    m,l,k: pc.NODE;
    case_node, end_node: ir.Node;
    q: ir.TriadePtr;  arg: ir.Arg;
    NUM, num, i, j: LONGINT;
    alts: POINTER TO ARRAY OF ir.Node;
    low, high: pc.VALUE;
    dd : BOOLEAN;
BEGIN
  ope.gen_value(n.l,ir.GenModeSet{},arg);
  def.type_info(n.l.type, ty, sz);
  m := n.r;
(*  min := m.next.val; *)

  NUM := num_cases(m);
  IF NUM = 0 THEN
    IF (m.mode=pc.nd_caselse) THEN
      gen_sequence(m.r, go_ret, tail, dd);
    ELSE
      ASSERT(~n.end.IsNull() OR (pc.ntag_constrinlined IN n.tags));
      ope.gen_trap_N(tune.caseSelectException, n.end);
    END;
    RETURN
  END;

  IF gr.currNode = ir.UndefNode THEN gr.StartNewNode END;
  case_node := gr.currNode;
  IF pc.ntag_no_exit IN n.tags THEN
    end_node := ir.UndefNode;
  ELSE
    end_node := gr.NewNode();
  END;
  NEW(alts, NUM);
  q := ir.NewTriadeInit(1+2*NUM, ir.o_case, ty, sz);
  q.Position := n.pos;
  ir.ParmByArg(q.Params[0], arg);
  gr.PutTriadeLast(q, case_node);
  num := 0;
  l:=m.l;
  WHILE l#NIL DO (* переребор альтернатив *)
    gr.StartNewNode();
    k:=l.l;
    WHILE k # NIL DO (* переребор списка меток у альтернативы *)
      low  := k.val;
      high := k.l.val;
      i := 0;
      WHILE (i < num) & ir.GT(low, q.Params[i*2+1].value, ty, sz, TRUE) DO
        INC (i);
      END;
      FOR j := num-1 TO i BY -1 DO
        alts[j+1] := alts[j];
        ir.CopyParam(q.Params[j*2+1], q.Params[(j+1)*2+1]);
        ir.CopyParam(q.Params[j*2+2], q.Params[(j+1)*2+2]);
      END;
      alts[i] := gr.currNode;
      ir.MakeParNum(q.Params[i*2+1], low);
      ir.MakeParNum(q.Params[i*2+2], high);
      q.Params[i*2+1].position := k.pos;
      q.Params[i*2+2].position := k.pos;
      INC(num);
      k:=k.next;
    END;
    gen_alt(l.r, go_ret, tail, end_node);
    l:=l.next;
  END;
  FOR i := 0 TO NUM-1 DO
    gr.NewArc(case_node, alts[i], TRUE);
  END;
  alts := NIL;
  gr.StartNewNode; (*else_node*);
  gr.NewArc(case_node, gr.currNode, m.mode=pc.nd_caselse);

  IF m.mode=pc.nd_caselse THEN
    gen_alt(m.r, go_ret, tail, end_node);
  ELSE
    ASSERT(~n.end.IsNull() OR (pc.ntag_constrinlined IN n.tags));
    ope.gen_trap_N(tune.caseSelectException, n.end);
  END;
  gr.currNode := end_node;
END gen_statement_case;

PROCEDURE make_ret_tmp(t: pc.STRUCT);
BEGIN
  IF ret_tmp = ir.UNDEFINED THEN
    ope.make_temp (t, ret_tmp)
  END;
END make_ret_tmp;

PROCEDURE gen_statement_return(n: pc.NODE; go_ret: BOOLEAN);
  VAR q, rtn: ir.TriadePtr;
    arg, re_arg, im_arg: ir.Arg;
    base: pc.STRUCT;
BEGIN
  ASSERT (n.r.mode = pc.nd_block);
  IF n.l = NIL THEN
    IF go_ret THEN gr.Goto (label(n.r));
    ELSE
      rtn := ir.NewTriadeInit (0, ir.o_ret, ir.t_void, 0);
      rtn.Position := n.pos;
      gr.AppendTr (rtn);
    END;
  ELSE
    base := n.l.type;     (* at.curr_proc.type.base;*)
    IF def.is_scalar(base) THEN
      ope.gen_value(n.l, ir.GenModeSet{}, arg);
      IF go_ret THEN
        make_ret_tmp(base);
        q := def.NewTriadeTS (1, ir.o_assign, base);
        q.Position := n.pos;
        ir.ParmByArg(q.Params[0], arg);
        q.Tag := ir.y_RealVar;
        q.Name := ret_tmp;
        gr.AppendTr (q);
        gr.Goto (label(n.r));
      ELSE
        rtn := def.NewTriadeTS (1, ir.o_ret, base);
        rtn.Position := n.pos;
        ir.ParmByArg(rtn.Params[0], arg);
        gr.AppendTr (rtn);
      END;
    ELSE
      IF base.mode IN pc.CPLXs THEN
        ope.gen_complex_value(n.pos, n.l, TRUE, TRUE, re_arg, im_arg);
        def.o_attr(n.pos, at.curr_proc, at.a_rtn, ir.GenModeSet{}, arg);
        ope.store_complex(n.pos, arg, re_arg, im_arg, base);
      ELSE (*  возвращаем структуру - надо сделать copy *)
          q := ir.NewTriadeInit (3, ir.o_copy, ir.t_void, 0);
          q.Position := n.pos;
          ope.gen_value(n.l, ir.GenModeSet{def.REF}, arg);
          ir.ParmByArg(q.Params[0], arg);
          def.o_attr(n.pos, at.curr_proc, at.a_rtn, ir.GenModeSet{}, arg);
          ir.ParmByArg(q.Params[1], arg);
          ir.MakeParNum(q.Params[2], def.val(def.type_size(base)));
          q.NPar := def.type_align (base);
          gr.AppendTr(q);
      END;
      IF go_ret THEN gr.Goto (label(n.r));
      ELSE
        rtn := ir.NewTriadeInit (0, ir.o_ret, ir.t_void, 0);
        rtn.Position := n.pos;
        gr.AppendTr(rtn);
      END;
    END;
  END;
END gen_statement_return;

PROCEDURE gen_except(n: pc.NODE; go_ret, tail: BOOLEAN);
  VAR p, q: ir.TriadePtr;
    if_node, then_node, else_node, end_node: ir.Node;
    arg: ir.Arg;
    dd : BOOLEAN;
    pos : ir.TPOS;
BEGIN
  try_block := TRUE;
--  IF n.l # NIL THEN pos := n.l.pos ELSE pos := n.pos END;
  pos := ir.NullPos;
  def.o_usage(pos, n.obj, ir.GenModeSet{def.REF}, arg);
  q := def.RTS_call(pos, std.X2C_XInitHandler, TRUE);
  ir.ParmByArg(q.Params[1], arg);
  gr.AppendTr(q);

  def.add_offs(pos, tune.process_buffer_offs, arg);
  q := def.RTS_call(pos, std.X2C_setjmp, TRUE);
  ir.ParmByArg(q.Params[1], arg);
  gr.AppendTr(q);

  p := ir.NewTriadeInit(2, ir.o_le, tune.index_ty, tune.index_sz);
  p.Position := pos;
  ir.MakeParNum(p.Params[0], def.val(2));
  ir.MakeParVar(p.Params[1], q.Name);
  gr.AppendTr(p);

  if_node := gr.currNode;
  gr.StartNewNode();
  then_node := gr.currNode;
  gen_sequence(n.l, go_ret, TRUE, dd);

  IF NOT dd THEN
    q := def.RTS_call(n.pos, std.X2C_XOFF, TRUE);
    gr.AppendTr(q);
  END;

  else_node := gr.NewNode();
  gr.NewArc(if_node, else_node, FALSE);
  gr.NewArc(if_node, then_node, FALSE);
  IF n.r = NIL THEN end_node := else_node;
  ELSE
    end_node := gr.NewNode();
    IF gr.currNode # ir.UndefNode THEN gr.Goto(end_node) END;
    gr.StartNode(else_node);
    gen_alt(n.r, go_ret, tail, end_node);
  END;
  gr.StartNode(end_node);
END gen_except;

PROCEDURE inlined_proc_in_expr(n: pc.NODE): BOOLEAN;
BEGIN
  IF n = NIL THEN RETURN FALSE
  ELSIF n.mode = pc.nd_block THEN RETURN TRUE
  ELSE RETURN inlined_proc_in_expr(n.l) OR inlined_proc_in_expr(n.r)
  END;
END inlined_proc_in_expr;

PROCEDURE gen_statement(n: pc.NODE; go_ret, tail: BOOLEAN);
VAR
  dd  : BOOLEAN;
  tmps: def.TMP_VAR;
  arg : ir.Arg;
  q   : ir.TriadePtr;
  i   : INT;
  start_node, end_node, body_node, nd : ir.Node;
  silentMode, convert: BOOLEAN;
  loc_insideInline: BOOLEAN;

BEGIN
  def.enter_statement(tmps);
  loc_insideInline := ir.insideInline;
  IF pc.ntag_constrinlined IN n.tags THEN
    ir.insideInline := TRUE;
  END;

  CASE n.mode OF
    |pc.nd_call:
      ope.gen_call(n, ir.GenModeSet{}, arg);

    |pc.nd_assign:
      arg.tag := ir.y_Nothing;
      ope.gen_value(n, ir.GenModeSet{}, arg);

    |pc.nd_while:
      convert := (at.convert_while IN at.COMP_MODE)
                & NOT inlined_proc_in_expr(n.l);
      start_node := ir.UndefNode;
      IF convert THEN
        ir.SetSilentMode ();
      ELSE
        gr.StartNewNode();
        start_node := gr.currNode;
      END;
      body_node := gr.NewNode();
      end_node  := label(n);                       -- next;
      ope.gen_condition(n.l, body_node, end_node);
      gr.currNode := body_node;
      IF convert THEN
        ir.SetNormalMode ();
        FOR i := 0 TO ir.Nodes[end_node].NIn-1 DO
         (* дуги, ведущие в вершину end_node в обход цикла *)
          gr.Arcs[ir.Nodes[end_node].InArcs[i]].Original := FALSE;
        END;
        gen_sequence(n.r, go_ret, TRUE, dd);
        IF NOT dd THEN
          ope.gen_condition(n.l, body_node, end_node);
        END;
      ELSE
        gen_alt(n.r, go_ret, TRUE, start_node);
      END;
      gr.currNode := end_node;

    |pc.nd_repeat:
      gr.StartNewNode();
      body_node := gr.currNode;
      gen_sequence(n.l, go_ret, TRUE, dd);
      IF ntag_exit_label IN n.tags THEN
        end_node := get_label(n);
      ELSE
        end_node := gr.NewNode();
      END;
      ope.gen_condition(n.r, end_node, body_node);
      gr.currNode := end_node;

    |pc.nd_loop:
      gr.StartNewNode();
      start_node := gr.currNode;
      gen_alt(n.r, go_ret, TRUE, start_node);
      IF ntag_exit_label IN n.tags THEN
        gr.StartNode(label(n));
      END;

    |pc.nd_exit:                   (* n.r - оператор, из которого выходим *)
      nd := gr.currNode;
      gr.Goto(label(n.r));
      ir.Nodes^[nd].Last.Position := n.pos;

    |pc.nd_if:
      IF pc.ntag_no_exit IN n.tags THEN
        end_node := ir.UndefNode;
      ELSE
        end_node := gr.NewNode();
      END;
      gen_statement_if(n, go_ret, tail, end_node);
      gr.currNode := end_node;

    |pc.nd_block:             (* оператор, из которого есть выход return'ом *)
      ASSERT (NOT (ntag_exit_label IN n.tags));
      gen_sequence(n.r, go_ret OR tail, tail, dd);
      IF ntag_exit_label IN n.tags THEN
        gr.StartNode(label(n));
      END;

    |pc.nd_return:                      (* в n.r - блок, из которого выходим; *)
      gen_statement_return(n, go_ret);  (* однако после этого блока могут     *)
      gr.FinishNode();                  (* быть еще исполняемые операторы     *)

    |pc.nd_for:
      gen_statement_for(n, go_ret);

    |pc.nd_case:
      gen_statement_case(n, go_ret, tail);

    |pc.nd_wtrap:
      ope.gen_trap_N(tune.guardException, n.pos);

    |pc.nd_ftrap:
      ope.gen_trap_N(tune.functionException, n.pos);

    |pc.nd_eval:
      IF n.l.type.mode IN pc.COMPOUNDs THEN
        ope.gen_value(n.l, ir.GenModeSet{def.REF}, arg);
      ELSE
        ope.gen_value(n.l, ir.GenModeSet{}, arg);
      END;
      IF (gr.lastTriade # NIL) & (gr.lastTriade.Op = ir.o_call) THEN
        INCL(gr.lastTriade.Options, ir.o_Silent);
      END;

    |pc.nd_with:
      ope.gen_value(n.l, ir.GenModeSet{def.REF}, arg);
      IF arg.tag # ir.y_Variable THEN
        q := ir.NewTriadeInit(1, ir.o_assign, tune.addr_ty, tune.addr_sz);
        q.Position := n.pos;
        ir.ParmByArg(q.Params[0], arg);
        ir.GenResVar(q);
        gr.AppendTr(q);
        ir.MakeArgVar(arg, q.Name);
      END;
      def.use_temp(n.obj, arg.name);
      gen_sequence(n.r, go_ret, tail, dd);

    |pc.nd_sproc:
      gen_sproc(n);

    |pc.nd_protect:
      q := def.RTS_call(n.pos, std.X2C_PROTECT, TRUE);
      def.o_usage(n.pos, n.obj, ir.GenModeSet{def.REF}, arg);
      ir.ParmByArg(q.Params[1], arg);
      ir.MakeParNum(q.Params[2], n.r.val);
      gr.AppendTr(q);

      gen_sequence(n.l, go_ret, TRUE, dd);

      q := def.RTS_call(ir.NullPos, std.X2C_PROTECT, TRUE);
      def.o_usage(n.pos, n.obj, ir.GenModeSet{def.REF}, arg);
      ir.ParmByArg(q.Params[1], arg);
      def.o_usage(n.pos, n.obj, ir.GenModeSet{}, arg);
      ir.ParmByArg(q.Params[2], arg);
      gr.AppendTr(q);

    |pc.nd_except:
      gen_except(n, go_ret, tail);

    |pc.nd_retry:
      q := def.RTS_call(n.pos, std.X2C_XRETRY, TRUE);
      gr.AppendTr(q);
      gr.Goto(ir.Node{1});

    |pc.nd_finally:
      def.o_usage(n.pos, n.obj, ir.GenModeSet{}, arg);
      q := def.RTS_call(n.pos, std.X2C_FINALLY, TRUE);
      ir.ParmByArg(q.Params[1], arg);
      gr.AppendTr(q);

    |pc.nd_reraise:
      q := def.RTS_call(n.pos, std.X2C_XREMOVE, TRUE);
      gr.AppendTr(q);

    |pc.nd_activate:
      q := def.RTS_call(n.pos, std.X2C_XON, TRUE);
      gr.AppendTr(q);

    |pc.nd_goto:
      ASSERT(n.l.mode = pc.nd_label);
      nd := gr.currNode;
      gr.Goto(label(n.l));
      ir.Nodes^[nd].Last.Position := n.pos;

    |pc.nd_label:
      gr.StartNode(label(n));

  ELSE
    env.errors.Error(n.pos,142);   --- "Can not generate statement"
  END;
(*
  IF ntag_exit_label IN n.tags THEN
    end_node := exit_label(n);
    IF end_node # gr.currNode THEN gr.StartNode(end_node) END;
  END;
*)
  def.exit_statement(tmps);
  ir.insideInline := loc_insideInline;
END gen_statement;

PROCEDURE gen_sequence(n: pc.NODE;
                  go_ret: BOOLEAN;   (* return - переход в конец блока *)
                    tail: BOOLEAN;   (* далее есть еще операторы *)
                  VAR dd: BOOLEAN);  (* dead end *)
BEGIN
  dd := FALSE;
  WHILE n # NIL DO
    gen_statement(n, go_ret, tail OR (n.next#NIL));
    dd:=pc.ntag_no_exit IN n.tags;
    n := n.next;
  END;
END gen_sequence;

PROCEDURE make_neg_align(VAR offs: LONGINT; l: INT);
BEGIN
  WHILE (offs MOD l) # 0 DO DEC(offs) END;
END make_neg_align;

PROCEDURE copied_param(v: pc.OBJECT): BOOLEAN;
BEGIN
  IF (v.mode # pc.ob_var) OR (pc.otag_RO IN v.tags) OR def.is_scalar(v.type)
  THEN
    RETURN FALSE
  ELSE
    RETURN TRUE
  END;
END copied_param;

<* IF TARGET_RISC THEN *>

PROCEDURE must_save_param (p: pc.OBJECT; i: INT) : BOOLEAN;
BEGIN
  RETURN (p.marks * pc.OMARK_SET{at.omark_nested_read, at.omark_nested_write} # pc.OMARK_SET{})
END must_save_param;

 -- PPC code generator places some parameters on registers, so we don't need
 -- to reserve space on the stack and all offsets must be recalculated.
 -- We implement right-to-left (RtoL) parameter passing order only

PROCEDURE RecalcParamOffsets(p: pc.OBJECT; Prt: pr.Proto);
  VAR
    i : INT;
    parm_offs, loc_offs: LONGINT;
    p_offs : LONGINT;
    p_sz   : ir.SizeType;
BEGIN
  parm_offs := tune.PARAM_START;
  loc_offs   := tune.LOCAL_START;
  FOR i := 0 TO Prt.npar - 1 DO
    p_sz := Prt.par[i].size;
    IF Prt.par[i].where = pr.STACK THEN
      p_offs := parm_offs;
      IF p_sz < 4 THEN
        IF tune.BIG_END THEN       -- прижать к другому краю
          INC(p_offs, 4 - p_sz);
        END;
        p_sz := 4; -- пока все параметры кладутся целыми словами, длина всегда 4
      END;
      INC (parm_offs, p_sz);
    ELSE
      IF must_save_param (p,i) THEN
        IF p_sz < 4 THEN
          DEC(loc_offs, 4);
          IF tune.BIG_END THEN       -- прижать к другому краю
            p_offs := loc_offs + 4 - p_sz;
          ELSE
            p_offs := loc_offs;
          END;
        ELSE
          DEC(loc_offs, 4);
          make_neg_align (loc_offs, p_sz);
          p_offs := loc_offs;
        END;
      ELSE
        p_offs := ir.UNKNOWN_OFFSET;
      END;
    END;
    Prt.par[i].offs := p_offs;
  END;
END RecalcParamOffsets;
<* END *>

PROCEDURE make_offsets(p: pc.OBJECT;
               VAR based: pc.OBJECT);  (* one of p's vars in memory *)
(*
 процедура уже должна быть занесена в таблицу и для нее построен прототип;
 надо приписать смещения для всех параметров процедуры и для тех переменных,
 которые используются вложенными процедурами
*)
  VAR ptype: pc.STRUCT;  v,vv: pc.OBJECT;  i: SHORTINT;
    proto: pr.Proto; prm: pr.params;  cnt: INTEGER;
    loc_offs, size: LONGINT;
    l: INT;
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
    offs: LONGINT;
<* END *>
BEGIN
  loc_offs := tune.IDB.MinLocalStart(p);
  based := NIL;
  ptype := p.type;
  v := ptype.prof; vv := NIL;
  proto := pr.ProtoList[def.prototype(ptype)];
<* IF TARGET_RISC THEN *>
  IF at.ABI # at.PowerOpen THEN
    RecalcParamOffsets(p, proto);
    FOR i := 0 TO proto.npar - 1 DO
      offs := proto.par[i].offs;
      IF (offs # ir.UNKNOWN_OFFSET) & (offs < loc_offs) THEN
        loc_offs := offs;
      END;
    END;
  END;
<* END *>
<* IF db_procs THEN *> pr.WrProto(def.prototype(ptype)); <* END *>
  prm := proto.par;
  FOR cnt := 0 TO proto.npar-1 DO
    CASE prm[cnt].mode OF
    | pr.pm_return:  (* ----- адрес переменной для возвращаемого значения *)
        at.app_info(p, at.a_rtn, ir.y_Nothing, VAL(at.InfExtName, ir.UNDEFINED), NIL, prm[cnt].offs);
    | pr.pm_base:    (* ----- база одной из охват. процедур *)
        i := VAL(SHORTINT, prm[cnt].ind);
        at.app_info(p, at.a_base+i, ir.y_Nothing, VAL(at.InfExtName, ir.UNDEFINED), NIL, prm[cnt].offs);
    | pr.pm_param:   (* ----- параметр *)
        IF at.must_have_fixed_address(v) THEN based := v END;
        IF copied_param(v) & NOT (v.type.mode = pc.ty_array_of) THEN
          IF at.must_have_fixed_address(v) THEN
            DEC(loc_offs, def.type_size(v.type));
            make_neg_align(loc_offs, 4);         -- выровнять на 4 ??
            at.app_info(v, at.a_self, ir.y_Nothing, VAL(at.InfExtName, ir.UNDEFINED), NIL, loc_offs);
          ELSE
            at.app_info(v, at.a_self, ir.y_Nothing, VAL(at.InfExtName, ir.UNDEFINED), NIL, ir.UNKNOWN_OFFSET);
          END;
        ELSE
          at.app_info(v, at.a_self, ir.y_Nothing, VAL(at.InfExtName, ir.UNDEFINED), NIL, prm[cnt].offs);
        END;
        vv := v;
        v := v.next;
    | pr.pm_len:     (* ----- длина параметра-гибкого массива по одному измер.*)
        i := VAL(SHORTINT, prm[cnt].ind);
        at.app_info(vv, at.a_len+i, ir.y_Nothing, VAL(at.InfExtName, ir.UNDEFINED), NIL, prm[cnt].offs);
    | pr.pm_formrec: (* ----- VAR параметр - обероновская запись *)
        IF at.must_have_fixed_address(v) THEN based := v END;
        at.app_info(v, at.a_self, ir.y_Nothing, VAL(at.InfExtName, ir.UNDEFINED), NIL, prm[cnt].offs);
        vv := v;
        v := v.next;
    | pr.pm_type:    (* ----- тип этой записи *)
        at.app_info(vv, at.a_type, ir.y_Nothing, VAL(at.InfExtName, ir.UNDEFINED), NIL, prm[cnt].offs);
    | pr.pm_re:
        IF at.must_have_fixed_address(v) THEN based := v END;
        at.app_info(v, at.a_self, ir.y_Nothing, VAL(at.InfExtName, ir.UNDEFINED), NIL, prm[cnt].offs);
        at.app_info(v, at.a_re, ir.y_Nothing, VAL(at.InfExtName, ir.UNDEFINED), NIL, prm[cnt].offs);
        vv := v;
        v := v.next;
    | pr.pm_im:
        at.app_info(vv, at.a_im, ir.y_Nothing, VAL(at.InfExtName, ir.UNDEFINED), NIL, prm[cnt].offs);
    | pr.pm_seq:     (* ----- C-последовательность параметров *)
        ASSERT(v.next = NIL);
        IF at.must_have_fixed_address(v) THEN based := v END;
        at.app_info(v, at.a_self, ir.y_Nothing, VAL(at.InfExtName, ir.UNDEFINED), NIL, prm[cnt].offs);
    END;
  END;
  v := ptype.mem;
  WHILE v # NIL DO
    IF (v.mode = pc.ob_var) & (v.attr = NIL) & at.must_have_fixed_address(v) THEN
     <* IF db_code THEN *> io.print('make_offs("%s")\n', v.name^); <* END *>
      based := v;
--    INCL(p.marks, at.omark_locals_used);
      size := def.type_size(v.type);
      DEC(loc_offs, size);
      IF    size = 1 THEN l := 1;
      ELSIF size = 2 THEN l := 2;
      ELSE  l := 4;
      END;
      make_neg_align(loc_offs, l);
      at.app_info(v, at.a_self, ir.y_Nothing, VAL(at.InfExtName, ir.UNDEFINED), NIL, loc_offs);
    END;
    v := v.next;
  END;
END make_offsets;

PROCEDURE change_info(o: pc.OBJECT;   (* у данного объекта заменить  *)
                  akind: SHORTINT;    (* информацию об этом атрибуте *)
                  e_tag: ir.TagType;
                   name: at.InfExtName);
  VAR a: at.ATTR_EXT;
BEGIN
  <* IF db_code THEN *> io.print("change_info: %s\n", o.name^); <* END *>
  a := at.attr(o.ext, akind);
  WITH a: at.INFO_EXT DO
    a.procno := at.curr_procno;
    a.e_tag := e_tag;
    a.name  := name;
  END
END change_info;

PROCEDURE change_offs(o: pc.OBJECT;   (* у данного объекта заменить  *)
                  akind: SHORTINT;    (* информацию об этом атрибуте *)
                   offs: LONGINT);
  VAR a: at.ATTR_EXT;
BEGIN
  <* IF db_code THEN *> io.print("change_offs: %s\n", o.name^); <* END *>
  a := at.attr(o.ext, akind);
  WITH a: at.INFO_EXT DO
    a.procno := at.curr_procno;
    a.offs := offs;
  END
END change_offs;


PROCEDURE alloc_open(tpos: pc.TPOS; sz_arg-: ir.Arg; VAR arg: ir.Arg);
  VAR q: ir.TriadePtr;
BEGIN
  was_alloca := TRUE;
  q := ir.NewTriadeInit(1, ir.o_alloca, tune.addr_ty, tune.addr_sz);
  q.Position := tpos;
  IF at.stack_checked IN at.COMP_MODE THEN
    INCL(q.Options, ir.o_Checked)
  END;
  ir.ParmByArg(q.Params[0], sz_arg);
  ir.GenResVar(q);
  gr.AppendTr(q);
  ir.MakeArgVar(arg, q.Name);
END alloc_open;

PROCEDURE copy_one_param (v    : pc.OBJECT;
                          tpos : ir.TPOS);
  VAR
    q: ir.TriadePtr;
    from_arg, to_arg, sz_arg: ir.Arg;
    open_arr: BOOLEAN; type: pc.STRUCT;
    offset: LONGINT;
BEGIN
<* IF db_code THEN *> io.print("copy_one_param(%s)\n", v.name^); <* END *>
    IF NOT copied_param(v) THEN RETURN END;
    offset := def.obj_offset(v);           (* to save offs for copied params *)
    change_offs(v, at.a_self, 0);
    type := v.type;
    open_arr := (type.mode = pc.ty_array_of);
    IF open_arr & NOT (at.curr_proc.type.flag IN opt.LangsWithOpenArrays) THEN
      RETURN
    END;
  (* надо сделать копию параметра *)
    def.o_usage(tpos, v, ir.GenModeSet{def.REF}, from_arg);
    IF open_arr THEN
      ope.gen_size_usage(tpos, v, 0, sz_arg);
      alloc_open(tpos, sz_arg, to_arg); (* выделить место под копию массива *)
    ELSE
      def.c_number(def.type_size(type), sz_arg);
      EXCL(v.tags, at.otag_declared);
      change_offs(v, at.a_self, offset); (* restore offset for copied params *)
      def.object_declaration(v);
      def.o_usage(tpos, v, ir.GenModeSet{def.REF}, to_arg);
    END;
    q := ir.NewTriadeInit(3, ir.o_copy, ir.t_void, 0);
    q.Position := tpos;
    INCL(q.Options, ir.o_Parameters);
    ir.ParmByArg(q.Params[0], from_arg);
    ir.ParmByArg(q.Params[1], to_arg);
    ir.ParmByArg(q.Params[2], sz_arg);
    q.NPar := def.type_align (type);
    gr.AppendTr(q);
    IF open_arr THEN
      ASSERT(from_arg.tag = ir.y_RealVar);
      q := ir.NewTriadeTInit(1, ir.o_assign, ir.y_RealVar, tune.addr_ty, tune.addr_sz);
      q.Position := tpos;
      ir.ParmByArg(q.Params[0], to_arg);
      q.Name := from_arg.name;  (*??*)
      gr.AppendTr(q);
      ir.Locals[from_arg.name].Obj := v;
    END;
END copy_one_param;

PROCEDURE copy_params(p: pc.OBJECT);
  VAR proto: pr.Proto;
--    RtoL: BOOLEAN;
    tpos : ir.TPOS;

  PROCEDURE copy_param(v: pc.OBJECT;    (* объект - формальный параметр *)
                       i: INTEGER);     (* номер параметра в прототипе   *)

  BEGIN
    IF i >= proto.npar THEN RETURN END;
    CASE proto.par[i].mode OF
    | pr.pm_return:  (* ----- адрес переменной для возвращаемого значения *)
        copy_param(v, i+1)
    | pr.pm_base:    (* ----- база одной из охват. процедур *)
        copy_param(v, i+1)
    | pr.pm_param:   (* ----- параметр *)
--      IF NOT RtoL THEN copy_param(v.next, i+1) END;

        LOOP
          INC(i);
          IF i >= proto.npar THEN EXIT END;
          IF proto.par[i].mode # pr.pm_len THEN EXIT END;
        END;
        copy_one_param(v, tpos);
--      IF RtoL THEN
        copy_param(v.next, i)
--      END;

    | pr.pm_len, pr.pm_type:
--      ASSERT(RtoL);
        copy_param(v, i+1);

    | pr.pm_formrec:
        ASSERT(proto.par[i+1].mode = pr.pm_type);
        copy_param(v.next, i+2);

    | pr.pm_re:
        copy_param(v.next, i+2);

    | pr.pm_seq:
        ASSERT(v.next=NIL);
        ASSERT(v.type.mode=pc.ty_array_of);
    END;
  END copy_param;

  VAR ptype: pc.STRUCT;

BEGIN
  <* IF db_code THEN *> io.print("copy_params: %s\n", p.name^); <* END *>
  ptype := p.type;
  proto := pr.ProtoList[def.prototype(ptype)];
  tpos := p.val.pos;
  -- Now we always copy 1st the leftmost parameter, then - the 2nd and so on.
  -- I suppose it's of no use to synchronize the order of copying with
  -- the order of passing
  -- RtoL := proto.right_to_left;
  copy_param(ptype.prof, 0);
END copy_params;

-------------------------------

PROCEDURE get_one_param(tpos- : ir.TPOS;
                        proto : pr.Proto; i : INTEGER; name : pc.STRING
                       ) : ir.Local;
  VAR q : ir.TriadePtr;
    loc : ir.Local;  al : SHORTINT;
    ty : ir.TypeType;
    sz : ir.SizeType;
--    offset : LONGINT;
BEGIN
<* IF db_code THEN *> io.print("   get_one_param: %s\n", name^); <* END *>
  ty     := proto.par[i].type;
  sz     := proto.par[i].size;
--  offset := proto.par[i].offs;

  IF    sz = 1 THEN al := 1;
  ELSIF sz = 2 THEN al := 2;
  ELSE              al := 4;
  END;
  loc := ir.AddLocal(name, def.local, al);
  ir.Locals^[loc].VarType := ty;
  ir.Locals^[loc].VarSize := sz;
  ir.Locals^[loc].Offset  := proto.par[i].offs;

  q := ir.NewTriadeInit(0, ir.o_getpar, ty, sz);
  q.Position := tpos;
  q.NPar := SHORT(i);
  q.Tag := ir.y_RealVar;
  q.Name := loc;
  gr.AppendTr(q);
  RETURN loc
END get_one_param;

PROCEDURE get_params(p: pc.OBJECT);
  VAR proto: pr.Proto;
    RtoL: BOOLEAN;
    tpos : ir.TPOS;

  PROCEDURE get_one(i : INTEGER; name : pc.STRING) : ir.Local;
  BEGIN
    RETURN get_one_param(tpos, proto, i, name)
  END get_one;

  PROCEDURE get_param(v: pc.OBJECT;    (* объект - формальный параметр *)
                      i: INTEGER);     (* номер параметра в прототипе   *)
    VAR j, k: INTEGER;
      pp: pc.OBJECT;
      loc, loc_len: ir.Local;
      nm : pc.STRING;
  BEGIN
    IF i>=proto.npar THEN RETURN END;
    CASE proto.par[i].mode OF
    | pr.pm_return:  (* ----- адрес переменной для возвращаемого значения *)
        IF NOT RtoL THEN get_param(v, i+1) END;
        nm := at.make_name("%s'rtn", p.name^);
        loc := get_one(i, nm);
        change_info(p, at.a_rtn, ir.y_RealVar, VAL(at.InfExtName, loc));
        IF RtoL THEN get_param(v, i+1) END;

    | pr.pm_base:    (* ----- база одной из охват. процедур *)
        IF NOT RtoL THEN get_param(v, i+1) END;
        j := VAL(SHORTINT, proto.par[i].ind);
        pp := p;
        FOR k := 0 TO j DO pp := pp.host.obj END;
        nm := at.make_name("%s'base", pp.name^);
        loc := get_one(i, nm);
        change_info(p, at.a_base+SHORT(j), ir.y_RealVar, VAL(at.InfExtName, loc));
        IF RtoL THEN get_param(v, i+1) END;

    | pr.pm_param:   (* ----- параметр *)
        IF NOT RtoL THEN get_param(v.next, i+1) END;
        CASE proto.par[i].ind OF
        | ORD(pr.by_val):
          loc := get_one(i, v.name);
          change_info(v, at.a_self, ir.y_RealVar, VAL(at.InfExtName, loc));
          ir.Locals[loc].Obj := v;
        | ORD(pr.by_ref), ORD(pr.by_ROref):
          nm := at.make_name("%s'ref", v.name^);
          loc := get_one(i, nm);
          change_info(v, at.a_self, at.BASE+ir.y_RealVar, VAL(at.InfExtName, loc));
          IF copied_param(v) THEN
            (* -- offset will be set by "copy_one_param"  *)
          ELSE
            change_offs(v, at.a_self, 0);
          END;
          ir.Locals[loc].Obj := v;
        END;
        j := 0;
        LOOP
          INC(i);
          IF i >= proto.npar THEN EXIT END;
          IF proto.par[i].mode # pr.pm_len THEN EXIT END;
          nm := at.make_name("%s'len%d", v.name^, j);
          loc_len := get_one(i, nm);
          change_info(v, at.a_len+SHORT(j), ir.y_RealVar, VAL(at.InfExtName, loc_len));
          ir.Locals[loc_len].Obj := v;
          INC(j);
        END;
        IF RtoL THEN get_param(v.next, i) END;

    | pr.pm_len, pr.pm_type:
        ASSERT(RtoL);
        get_param(v, i+1);

    | pr.pm_formrec:
        IF NOT RtoL THEN get_param(v.next, i+2) END;
        nm := at.make_name("%s'ref", v.name^);
        loc := get_one(i, nm);
        ir.Locals[loc].Obj := v;
        change_info(v, at.a_self, at.BASE+ir.y_RealVar, VAL(at.InfExtName, loc));
        change_offs(v, at.a_self, 0);
        ASSERT(proto.par[i+1].mode = pr.pm_type);
        nm := at.make_name("%s'type", v.name^);
        loc := get_one(i+1, nm);
        ir.Locals[loc].Obj := v;
        change_info(v, at.a_type, ir.y_RealVar, VAL(at.InfExtName, loc));
        IF RtoL THEN get_param(v.next, i+2) END;

    | pr.pm_re:
        IF NOT RtoL THEN get_param(v.next, i+2) END;
        nm := at.make_name("%s're", v.name^);
        loc := get_one(i, nm);
        change_info(v, at.a_re, ir.y_RealVar, VAL(at.InfExtName, loc));
        ir.Locals[loc].Obj := v;                         (* очень склизко !! *)
        change_info(v, at.a_self, ir.y_RealVar, VAL(at.InfExtName, loc));
                           (* at.BASE+ir.y_AddrConst -- ?? *)
        nm := at.make_name("%s'im", v.name^);
        loc := get_one(i+1, nm);
        ir.Locals[loc].Obj := v;
        change_info(v, at.a_im, ir.y_RealVar, VAL(at.InfExtName, loc));
        IF RtoL THEN get_param(v.next, i+2) END;

    | pr.pm_seq:
        ASSERT(v.next=NIL);
        ASSERT(v.type.mode=pc.ty_array_of);
        loc := get_one(i, v.name);
        change_info(v, at.a_self, ir.y_RealVar, VAL(at.InfExtName, loc));
    END;
  END get_param;

  VAR ptype: pc.STRUCT;
BEGIN
  <* IF db_code THEN *> io.print("get_params: %s\n", p.name^); <* END *>
  ptype := p.type;
  proto := pr.ProtoList[def.prototype(ptype)];
  RtoL := proto.right_to_left;
  tpos := p.val.pos;
  get_param(ptype.prof, 0);
  ope.CallerIPLocal := ir.UNDEFINED;
END get_params;

PROCEDURE alloc_threatened(p: pc.OBJECT);
  VAR v: pc.OBJECT;
BEGIN
  v := p.type.mem;
  WHILE v # NIL DO
    IF (v.mode = pc.ob_var) & (v.attr = NIL) & at.must_have_fixed_address(v) THEN
      EXCL(v.tags, at.otag_declared);
      def.object_declaration(v);
    END;
    v := v.next;
  END;
END alloc_threatened;

(*
PROCEDURE absolute_var(l: pc.OBJECT);
  VAR arg: ir.Arg;
BEGIN
  ASSERT ((l.mode = pc.ob_var) & (l.val # NIL));
  gen_value(l.val, {}, arg);
  CASE arg.tag OF
  | ir.y_NumConst:
     at.app_info(o, at.a_self, (at.BASE + ir.y_NumConst), ir.UNDEFINED, arg.value, 0);
     RETURN;
(*
  | ir.y_AddrConst:
  | ir.y_ProcConst:
  | ir.y_RealVar:
  | ir.y_Variable:
*)
  ELSE
  END;
  env.errors.PrintMsg(l.val.pos,'e',"Can not generate address of %s", l.name^);
END absolute_var;
*)

PROCEDURE make_usage(p: pc.OBJECT);
  VAR u: pc.USAGE; o: pc.OBJECT; arg: ir.Arg; j: SHORTINT;
BEGIN
-- вообще-то при "ленивом" подходе было бы достаточно
-- почистить признак "created" у объектов из этого списка

<* IF db_code THEN *> io.print("make_usage: %s\n", p.name^); <* END *>
  u := p.type.use;
  WHILE u # NIL DO
    o := u.obj;
    IF o.mode IN pc.VARs THEN
     <* IF db_code THEN *>
      io.print("      o = %s : %s %{}\n", o.host.obj.name^, o.name^, u.tags);
     <* END *>
      j := (p.lev-o.lev); ASSERT(j >=0 );
      def.o_attr(ir.NullPos, p, at.a_base+j, ir.GenModeSet{}, arg);
      change_info(o, at.a_self, at.BASE+ir.y_RealVar, VAL(at.InfExtName, arg.name));
      -- isn't it wrong ??
      -- array of having len defined at compile time?
      IF (o.type.mode = pc.ty_array_of) &
         (o.host.flag IN opt.LangsWithOpenArrays) THEN
        FOR j := 0 TO VAL(SHORTINT,o.type.len) - 1 DO
          change_info(o, at.a_len+j, at.BASE+ir.y_RealVar, VAL(at.InfExtName, arg.name));
        END;
      ELSIF (o.mode = pc.ob_varpar)
         & (o.type.mode=pc.ty_record) & (o.type.flag IN opt.LangsWithRTTI) THEN
          change_info(o, at.a_type, at.BASE+ir.y_RealVar, VAL(at.InfExtName, arg.name));
      END;
    END;                               (* здесь хорошо бы учесть и offset *)
    u := u.next;
  END;
END make_usage;

PROCEDURE make_mybase(p: pc.OBJECT;             (* based procedure *)
                      v: pc.OBJECT);            (* one of it's based vars *)
  VAR loc: ir.Local;
    q: ir.TriadePtr;
(*    arg: ir.Arg;     *)
BEGIN
<* IF db_code THEN *> io.print("make_mybase: %s\n", p.name^); <* END *>
(*
  loc := ir.AddLocal(at.make_name("%s'base", p.name^), def.local, 4);
  ir.Locals^[loc].VarType := tune.addr_ty;
  ir.Locals^[loc].VarSize := tune.addr_sz;

  def.o_usage(ir.NullPos, v, {def.REF}, arg);
  arg.offset := -ir.Locals[arg.name].Offset;
*)

  q := ir.NewTriadeInit(1, ir.o_assign, tune.addr_ty, tune.addr_sz);
  loc := at.loc_by_obj(v);
  ir.MakeParAddr(q.Params[0], loc, -ir.Locals[loc].Offset);
  ir.GenResVar(q);
  gr.AppendTr(q);
  at.app_info(p, at.a_mybase, ir.y_Variable, VAL(at.InfExtName, q.Name), NIL, 0);

(* другой вариант - без присваиваний:
  loc := at.loc_by_obj(v);
  at.app_info(p, at.a_mybase, at.BASE+ir.y_AddrConst, loc, NIL, -ir.Locals[loc].Offset);
*)
END make_mybase;

PROCEDURE init_o2_ptrs;                 (* result - in opProcs.MustFillFrame *)
  VAR o: pc.OBJECT; type: pc.STRUCT;
    q: ir.TriadePtr; arg : ir.Arg;
    o2_ptr : BOOLEAN;
BEGIN      (* может быть лучше потом пройтись по ir.Locals ?? *)
  o2_ptr := FALSE;
  o := at.curr_proc.type.mem;
  LOOP
    IF o = NIL THEN EXIT END;
    IF o.mode = pc.ob_var THEN
      type := o.type;
      IF pc.ttag_has_o2_ptr IN type.tags THEN
        IF type.mode # pc.ty_pointer THEN pr.MustFillFrame := TRUE; RETURN END;
        o2_ptr := TRUE;
      END;
    END;
    o := o.next;
  END;
  IF o2_ptr THEN
    o := at.curr_proc.type.mem;    (* (o2_ptr = TRUE) --> (o # NIL) *)
    REPEAT
      IF (o.mode = pc.ob_var) & (pc.ttag_has_o2_ptr IN o.type.tags) THEN
        ASSERT(o.type.mode = pc.ty_pointer);
        q := ir.NewTriadeTInit(1, ir.o_assign, ir.y_RealVar, tune.addr_ty, tune.addr_sz);
      (* --  don't put anything in q.Position to avoid debugger's jumps -- *)
        def.o_usage(ir.NullPos, o, ir.GenModeSet{def.LVL}, arg);
        ASSERT(arg.tag = ir.y_RealVar);
        q.Name := arg.name;
        ir.MakeParNum(q.Params[0], tune.nil_val);
        gr.AppendTr(q);
      END;
      o := o.next;
    UNTIL (o = NIL);
  END;
END init_o2_ptrs;

PROCEDURE clear(o: pc.OBJECT);
BEGIN
  WHILE o # NIL DO
    IF o.mode IN pc.VARs THEN
      at.del_attr(o.ext, at.a_self);
      EXCL(o.tags, at.otag_declared);
      (* надо ввести тег(и) для атрибутов *)
    END;
    o := o.next;
  END;
END clear;

PROCEDURE clear_attrs;
BEGIN
  clear(at.curr_mod.type.prof);
  clear(at.curr_mod.type.mem);
  clear(std.x2c.prof);
END clear_attrs;

TYPE iterated_proc = PROCEDURE (o: pc.OBJECT);

PROCEDURE proc_list(o: pc.OBJECT; prc: iterated_proc);
 VAR mt: pc.OBJECT;
BEGIN
  WHILE o # NIL DO
    IF o.mode IN pc.PROCs THEN prc(o);
    ELSIF (o.mode = pc.ob_type)
     & (o.type.mode=pc.ty_record)
     & (o.type.flag IN opt.OOP_Langs)
   THEN
     mt := o.type.mem;
     WHILE mt # NIL DO prc(mt); mt := mt.next END;
   END;
    o := o.next;
  END;
END proc_list;

VAR proc_cnt: INT;

PROCEDURE count_proc(o: pc.OBJECT);
BEGIN
  IF (o.mno = at.curr_mno)
    & (o.mode # pc.ob_eproc)
    & (o.mode # pc.ob_cproc)
  THEN
    proc_list(o.type.mem, count_proc);
    INC(proc_cnt);
  END;
END count_proc;

PROCEDURE NumberOfProcs(): INT;
BEGIN
  proc_cnt := 1;
  proc_list(at.curr_mod.type.mem, count_proc);
  proc_list(at.curr_mod.type.prof, count_proc);
  RETURN proc_cnt
END NumberOfProcs;


(* --- Calculation of procedures which variables used by nested procedures --- *)

PROCEDURE OneProcUsage(o: pc.OBJECT);
  VAR u: pc.USAGE; uobj: pc.OBJECT;
BEGIN
  IF (o.mno = at.curr_mno)
    & (o.mode # pc.ob_eproc)
    & (o.mode # pc.ob_cproc)
  THEN
    proc_list(o.type.mem, OneProcUsage);
    u := o.type.use;
    WHILE u # NIL DO
      uobj := u.obj;
      IF (uobj.mode IN pc.VARs) THEN
        IF pc.utag_read IN u.tags THEN
          INCL (uobj.marks, at.omark_nested_read);
          INCL (uobj.host.obj.marks, at.omark_nested_read);
        END;
--      IF pc.utag_write IN u.tags THEN
          INCL (uobj.marks, at.omark_nested_write);
          INCL (uobj.host.obj.marks, at.omark_nested_write);
--      END;
      END;
      u := u.next;
    END;
  END;
END OneProcUsage;

PROCEDURE CalculateUsedByNested;
BEGIN
  proc_list(at.curr_mod.type.mem, OneProcUsage);
  proc_list(at.curr_mod.type.prof, OneProcUsage);
END CalculateUsedByNested;

(*
PROCEDURE print_var_list(s-: ARRAY OF CHAR; o: pc.OBJECT);
BEGIN
  pcVis.print(" %s\n", s);
  IF o = NIL THEN
    pcVis.print(" NIL\n");
  ELSE
    REPEAT
      IF o.mode IN pc.VARs THEN
        pcVis.print(" %s (offs = %d)\n", o.name^, def.obj_offset(o))
      END;
      o := o.next;
    UNTIL o = NIL;
  END;
END print_var_list;
*)

<* IF pcvis THEN *>

PROCEDURE vis_proc(p: pc.OBJECT);
  VAR nm: pc.STRING;
BEGIN
  env.config.Equation(EQU_VIS_PROC, nm);
  IF (nm # NIL)&(nm^=p.name^) THEN
    pcVis.struct(p.type, 0);
    pcVis.vis(p.val.r, 0);
    io.needed := TRUE;
  ELSE
    io.needed := FALSE;
  END;
END vis_proc;

<* END *>

<* IF TARGET_386 THEN *>
----- procedures for profiler code generating -------

PROCEDURE raw_object (name-: ARRAY OF CHAR; m: pc.OB_MODE; size: LONGINT): pc.OBJECT;
VAR t: pc.STRUCT;
BEGIN
  t := pc.new_type( pc.ty_array );
  t.base := pcO.shortcard;
  t.len := size;
  RETURN at.new_work_object(at.make_name(name), t, at.curr_mod.type, m, FALSE);
END raw_object;

PROCEDURE set_raw_segm (o: pc.OBJECT; seg:cmd.CODE_SEGM);
BEGIN
  o.type.len := seg.code_len;
  cmd.set_ready(o, seg);
END set_raw_segm;

PROCEDURE gen_raw_loc (o: pc.OBJECT): ir.Local;
VAR loc: ir.Local;
BEGIN
  loc := ir.AddLocal( o.name, VAL(ir.ScopeType,SYSTEM.SUCC(at.curr_mno)),
                      at.default_alignment );
  ir.Locals[loc].Obj := o;
  RETURN loc;
END gen_raw_loc;

PROCEDURE gen_prof_procinfo(p: pc.OBJECT);
VAR old: cmd.CODE_SEGM;
    procname: ARRAY pc.name_size OF CHAR;
    i :INTEGER;
BEGIN
  IF at.profilingMode = xProfRTS.PROF_MODE_NONE THEN RETURN END;

  cmd.get_segm(old);
  cmd.set_segm(cmd.prof_info_segm);
  FOR i := 0 TO cmd.prof_info_elem_len-1 DO cmd.GenByte(0X); END;
  cmd.prof_info_loc := gen_raw_loc(cmd.prof_info);

  cmd.set_segm(cmd.prof_xref_segm);
  dbg.write_obj_name (p, procname);
  cmd.gen_fixup(def.out_str(procname), 0, cmd.fx_obj32);
  cmd.set_segm(old);
END gen_prof_procinfo;

PROCEDURE make_prof_objs();
BEGIN
  IF at.profilingMode = xProfRTS.PROF_MODE_NONE THEN RETURN END;

  cmd.prof_info := raw_object("profiler_info", pc.ob_var, 0);
  cmd.new_segm(cmd.prof_info_segm);
  cmd.prof_xref := raw_object("profiler_xref", pc.ob_cons, 0);
  cmd.new_segm(cmd.prof_xref_segm);
END make_prof_objs;

PROCEDURE gen_prof_modinfo(): pc.OBJECT;
VAR mobj: pc.OBJECT;
    mseg, old: cmd.CODE_SEGM;
BEGIN
  IF at.profilingMode = xProfRTS.PROF_MODE_NONE THEN RETURN NIL; END;
  gen_prof_procinfo(at.curr_mod);
  set_raw_segm(cmd.prof_info, cmd.prof_info_segm);
  set_raw_segm(cmd.prof_xref, cmd.prof_xref_segm);

  cmd.get_segm(old);
  mobj := raw_object("profiler_module", pc.ob_var, 0);
  cmd.new_segm(mseg);
  cmd.set_segm(mseg);
  cmd.gen_fixup(cmd.prof_info, 0, cmd.fx_obj32);        -- info
  cmd.gen_fixup(cmd.prof_xref, 0, cmd.fx_obj32);        -- xref
  cmd.GenLWord(ORD(at.curr_user_procno)+1);             -- procCount
  cmd.gen_fixup(def.out_str(at.curr_mod.name^), 0, cmd.fx_obj32); -- moduleName
  cmd.gen_fixup(at.curr_proc, 0, cmd.fx_obj32);         -- mainAdr
  cmd.put_nil_value;                                    -- next
  set_raw_segm(mobj, mseg);
  cmd.set_segm(old);
  RETURN mobj;
END gen_prof_modinfo;

PROCEDURE gen_prof_begin_module;
VAR
  q   :ir.TriadePtr;
BEGIN
  IF at.profilingMode = xProfRTS.PROF_MODE_NONE THEN RETURN END;

  q := def.RTS_call(env.null_pos, std.X2C_PROFILE_BEGIN_MODULE, TRUE);
  ir.MakeParAddr(q.Params[1], gen_raw_loc(gen_prof_modinfo()), 0);
  gr.AppendTr(q);
END gen_prof_begin_module;

PROCEDURE gen_INIT_PROFILER(pos-: ir.TPOS);
VAR
  q : ir.TriadePtr;
BEGIN
  IF at.profilingMode = xProfRTS.PROF_MODE_NONE THEN RETURN END;

  q := def.RTS_call(pos, std.X2C_INIT_PROFILER, TRUE);
  ir.MakeParNum(q.Params[1], Calc.GetInteger(ORD(at.profilingMode),4)); -- 4 = longint_sz
  gr.AppendTr(q);
END gen_INIT_PROFILER;

PROCEDURE except_attr_prepare;
BEGIN
  IF pc.ttag_except IN at.curr_proc.type.tags THEN
    IF at.curr_proc.attr # NIL THEN
      def.object_declaration(at.curr_proc.attr(pc.OBJECT));
    END;
  END;
END except_attr_prepare;

<* ELSE *>  -- TARGET_386

<* PUSH *>
<* WOFF301+ *>
PROCEDURE gen_prof_procinfo(p: pc.OBJECT);
BEGIN
END gen_prof_procinfo;

PROCEDURE make_prof_objs();
BEGIN
END make_prof_objs;

PROCEDURE gen_prof_begin_module;
BEGIN
END gen_prof_begin_module;

PROCEDURE gen_INIT_PROFILER(pos-: ir.TPOS);
BEGIN
END gen_INIT_PROFILER;

PROCEDURE except_attr_prepare;
BEGIN
END except_attr_prepare;
<* POP *>

<* END *>  -- TARGET_386
---------------------------------------

PROCEDURE prepare_excepttable_info();
BEGIN
  IF pc.otag_haveExceptTable IN pc.mods[pc.cur_mod].tags THEN
    cmd.excepttable := raw_object("excepttable", pc.ob_var, 0);
    cmd.new_segm(cmd.excepttable_segm);
  END;
END prepare_excepttable_info;

PROCEDURE gen_excepttable;
VAR
  old: cmd.CODE_SEGM;
BEGIN
  IF pc.otag_haveExceptTable IN pc.mods[pc.cur_mod].tags THEN
    cmd.get_segm(old);
    cmd.set_segm(cmd.excepttable_segm);
    cmd.put_nil_value;
    cmd.set_segm(old);
    set_raw_segm(cmd.excepttable, cmd.excepttable_segm);
  END;
END gen_excepttable;


PROCEDURE decor_write_triading(p: pc.OBJECT);
VAR
    k,name_len: INT;
    nm: pc.STRING;
BEGIN
  nm := p.GetReadableName(FALSE);
  name_len := LENGTH(nm^);
  IF ~env.shell.Active ()&(env.dc_progress IN env.decor) THEN
     k := last_name_len - name_len;
     IF k < 0 THEN k := 0 END;
     env.info.print("\rTriading   %s%.*c", nm^, k, CHR(32)); --fixme
  END;
  last_name_len := name_len;
END decor_write_triading;

PROCEDURE ^ gen_proc(o: pc.OBJECT);

PROCEDURE gen_proc_body(p: pc.OBJECT);
  VAR
    dd: BOOLEAN; tmps: def.TMP_VAR;
    based: pc.OBJECT;
    q: ir.TriadePtr;
    onlyProcName : pc.STRING;
    readableprocname: pc.STRING;
    fake1,fake2: ir.ParamPtr;
BEGIN
  IF at.omark_gen_ready IN p.marks THEN RETURN END; (* to avoid double processing *)
<* IF db_code THEN *>
  pcVis.print("\n----------------- opCode.gen_proc_body('%s')\n", p.name^);
<* END *>

  def.object_declaration(p);
  make_offsets(p, based);

  proc_list(p.type.mem, gen_proc);

  env.config.Equation("gen_only_proc", onlyProcName);
  readableprocname := p.GetReadableName(FALSE);
  IF (onlyProcName # NIL) & (onlyProcName^ # readableprocname^) THEN RETURN; END;

  decor_write_triading(p);
  Optim_Start;

  gen_prof_procinfo(p);

  gr.StartNewNode();
  ret_tmp := ir.UNDEFINED;

  at.curr_proc := p;
  at.curr_procno := def.proc_num(p);
  INC(at.curr_user_procno);

  ope.gen_modName(p^.pos, fake1,fake2);
  except_attr_prepare;

  def.enter_statement(tmps);

  ir.SetSilentMode ();

  get_params(p);                  (* принять параметры *)

(*
  pcVis.print("\n----------------- opCode.gen_proc('%s')\n", p.name^);
  print_var_list("prof:", p.type.prof);
  print_var_list("mem:", p.type.mem);
*)

<* IF pcvis THEN *>  vis_proc(p);  <* END *>

  alloc_threatened(p);

  make_usage(p);                  (* приписать атрибуты внешним переменным *)
  IF based # NIL THEN
    make_mybase(p, based);        (* завести переменную - базу процедуры *)
  END;
  copy_params(p);

  init_o2_ptrs;
  ir.SetNormalMode ();

  ini_labels;
  gen_sequence(p.val.r, FALSE, FALSE, dd);      (* тело процедуры *)
  ir.SetSilentMode;
  IF p.type.base.mode = pc.ty_void THEN
    q := ir.NewTriadeInit(0, ir.o_ret, ir.t_void, 0);
    q.Position := at.curr_proc.val.end;
    ASSERT(NOT q.Position.IsNull() OR (pc.omark_used_by_code IN at.curr_proc.marks));
    gr.AppendTr(q);
  ELSIF ret_tmp # ir.UNDEFINED THEN
    q := def.NewTriadeTS(1, ir.o_ret, p.type.base);
    q.Position := at.curr_proc.val.end;
    ASSERT(NOT q.Position.IsNull());
    ir.MakeParLocal(q.Params[0], ret_tmp);
    gr.AppendTr(q);
  END;
  ir.SetNormalMode;
  def.exit_statement(tmps);
  def.tmp_vars:=NIL;
  ASSERT(def.tmp_busy=NIL);
  Optim_Do;
  clear_attrs;
  IF pc.xot_noninlinable IN p.xtags THEN
    -- delete redundand procedure body
    p.val.r := NIL;
  END;
END gen_proc_body;

PROCEDURE gen_proc(o: pc.OBJECT);
BEGIN
  ASSERT(o.mode IN pc.PROCs);
  IF (o.mno = at.curr_mno) & (o.mode # pc.ob_eproc) & (o.mode # pc.ob_cproc) THEN
    gen_proc_body(o);
  END;
END gen_proc;

(*------------ G e n e r a t e   m o d u l e   b o d y -----------------------*)

PROCEDURE mod_ini_flag(): ir.Local;
  VAR old, sg: cmd.CODE_SEGM;
    o: pc.OBJECT; ty: ir.TypeType; sz: ir.SizeType;
    loc: ir.Local;
    name : pc.STRING;
BEGIN
  name := at.make_name("'init");
  o := at.new_work_object(name, def.CARD8_T, at.curr_mod.type, pc.ob_var, FALSE);

 <* IF TARGET_RISC OR TARGET_SPARC THEN *>
  def.alloc_work_object(o);
 <* END *>

  def.type_info(o.type, ty, sz);
  loc := ir.AddLocal(name, def.scope_id(at.curr_mod), at.default_alignment);
  ir.Locals[loc].Obj := o;
  ir.Locals[loc].VarType := ty;
  ir.Locals[loc].VarSize := sz;

  cmd.get_segm(old);   (* to make this variable pre-initialized by 0 *)
  cmd.new_segm(sg);
  cmd.set_segm(sg);
  cmd.GenByte(0X);
  cmd.set_ready(o, sg);
  cmd.set_segm(old);

  RETURN loc
END mod_ini_flag;

PROCEDURE gen_InitFPP(pos-: ir.TPOS);
  VAR q: ir.TriadePtr;
BEGIN
<* IF TARGET_VAX OR TARGET_68k THEN *>
  RETURN
<* ELSE *>
  q := def.RTS_call(pos, std.X2C_InitFPP, TRUE);
  gr.AppendTr(q);
<* END *>
END gen_InitFPP;

PROCEDURE set_stack_limit(L: LONGINT);
  VAR old, sg: cmd.CODE_SEGM;
    o: pc.OBJECT;
    name : pc.STRING;
BEGIN
  IF at.TARGET IN at.trg_SET{at.trg_NT, at.trg_OS2, at.trg_LINUX} THEN
    RETURN;
  END;
  cmd.get_segm(old);
  cmd.new_segm(sg);
  cmd.set_segm(sg);
  cmd.GenLWord(L);
  o := pc.new_object_ex(def.INDEX_T, at.curr_mod.type, pc.ob_cons, TRUE);
  CASE at.CC OF
  | at.SYMANTEC, at.NATIVE:
     name := at.make_name("_stack");
  | at.WATCOM:
     name := at.make_name("_x32_stack_size");
  | at.BORLAND:
     name := at.make_name("_stklen");
  ELSE
     name := at.make_name("_stklen"); (* ??? *)
  END;
  o.name := name;
  o.flag := pc.flag_c;

  cmd.set_ready(o, sg);
  cmd.set_segm(old);
END set_stack_limit;

PROCEDURE gen_INIT_HISTORY(pos-: ir.TPOS);
  VAR q : ir.TriadePtr;
BEGIN
  IF at.history IN at.COMP_MODE THEN
    q := def.RTS_call(pos, std.X2C_INIT_HISTORY, TRUE);
    gr.AppendTr(q);
  END;
END gen_INIT_HISTORY;


PROCEDURE get_heap_parameters;
  VAR str : env.String;
BEGIN
  env.config.Equation("HEAPLIMIT", str);
  IF (str = NIL) OR ~xcStr.StrToInt(str^, at.heap_lim) THEN
    at.heap_lim:=0;
  END;
  env.config.Equation("GCTHRESHOLD", str);
  IF (str = NIL) OR ~xcStr.StrToInt(str^, at.gc_thres) THEN
    at.gc_thres := 0;
  END;
  at.gc_auto := env.config.Option("GCAUTO");
END get_heap_parameters;

PROCEDURE gen_X2C_BEGIN(tpos-: ir.TPOS; proto: pr.Proto);
  VAR q : ir.TriadePtr;
    argc, argv: ir.Local;
    str : env.String;
BEGIN
  argc := get_one_param(tpos, proto, 0, at.make_name("main'argc"));
  argv := get_one_param(tpos, proto, 1, at.make_name("main'argv"));

  env.config.Equation("STACKLIMIT", str);
  IF (str = NIL) OR NOT xcStr.StrToInt(str^, at.stk_lim) THEN
    at.stk_lim := 0;
  ELSE
    IF (at.TARGET = at.trg_FLASHTEK) THEN
      at.stk_lim := 0; (* иначе - неприятности со знаком '=' в команд. строке *)
    END;
    set_stack_limit(at.stk_lim);
  END;

  get_heap_parameters;

  q := def.RTS_call(tpos, std.X2C_BEGIN, TRUE);
  ir.MakeParAddr(q.Params[1], argc, 0);    (* параметры от main *)
  ir.MakeParLocal(q.Params[2], argv);      (* параметры от main *)
  ir.MakeParNum(q.Params[3], def.val(ORD(at.gc_auto)));
  ir.MakeParNum(q.Params[4], def.val(at.gc_thres));
  ir.MakeParNum(q.Params[5], def.val(at.heap_lim));
  gr.AppendTr(q);
END gen_X2C_BEGIN;

PROCEDURE gen_X2C_BEGINDLL (tpos-: ir.TPOS; ret: ir.Node);
VAR
  q   :ir.TriadePtr;
  str :env.String;
  res :ir.VarNum;
  cont:ir.Node;
BEGIN
  env.config.Equation("STACKLIMIT", str);
  IF (str = NIL) OR NOT xcStr.StrToInt(str^, at.stk_lim) THEN
    at.stk_lim := 0;
  ELSE
    IF (at.TARGET = at.trg_FLASHTEK) THEN
      at.stk_lim := 0; (* иначе - неприятности со знаком '=' в команд. строке *)
    END;
    set_stack_limit(at.stk_lim);
  END;

  get_heap_parameters;

  q := def.RTS_call(tpos, std.X2C_BEGINDLL, TRUE);
  ir.MakeParNum(q.Params[1], def.val(ORD(at.gc_auto)));
  ir.MakeParNum(q.Params[2], def.val(at.gc_thres));
  ir.MakeParNum(q.Params[3], def.val(at.heap_lim));
  res := q.Name;
  gr.AppendTr(q);

  q := ir.NewTriadeInit(2, ir.o_eq, tune.bool_ty, tune.bool_sz);
  q.Position := tpos;
  ir.MakeParVar(q.Params[0], res);
  ir.MakeParNum(q.Params[1], def.zz_zero);
  gr.AppendTr(q);

  cont := gr.NewNode();
  gr.NewArc(gr.currNode, ret,  FALSE);
  gr.NewArc(gr.currNode, cont, FALSE);

  gr.StartNode(cont);
END gen_X2C_BEGINDLL;

PROCEDURE gen_check_flag(tpos-: ir.TPOS;
                         flag : ir.Local;
                         val  : pc.VALUE;
                  VAR YES, NO : ir.Node);
  VAR q : ir.TriadePtr;
BEGIN
  q := ir.NewTriadeInit(2, ir.o_eq,
                           ir.Locals[flag].VarType,
          VAL(ir.SizeType, ir.Locals[flag].VarSize));
  q.Position := tpos;
  ir.MakeParLocal(q.Params[0], flag);
  ir.MakeParNum(q.Params[1], val);
  gr.AppendTr(q);
  YES := gr.NewNode();
  NO  := gr.NewNode();
  gr.NewArc(gr.currNode, YES, FALSE);
  gr.NewArc(gr.currNode, NO, FALSE);
END gen_check_flag;

PROCEDURE gen_import_init (pos-: ir.TPOS);
  VAR q : ir.TriadePtr;
    arg : ir.Arg;
    u : pc.USAGE;  utype : pc.STRUCT;
BEGIN
 (* import initialization *)
  u := at.curr_mod.type.use;
  WHILE u # NIL DO
    IF u.obj.mode = pc.ob_module THEN
      utype := u.obj.type;
      IF (utype.flag IN opt.LangsWithModuleConstructors) THEN
        q := ir.NewTriadeInit(1, ir.o_call, ir.t_void, 0);
        q.Position := pos;
        q.Prototype := def.prototype(utype);
        def.o_usage(pos, u.obj, ir.GenModeSet{}, arg);
        ir.ParmByArg(q.Params[0], arg);
        gr.AppendTr(q);
      END;
    END;
    u := u.next;
  END;
END gen_import_init;

PROCEDURE gen_glovars(o: pc.OBJECT);
BEGIN
  WHILE o # NIL DO
    IF (o.mode = pc.ob_var) & (o.val # NIL)THEN
      def.object_declaration(o);
    END;
    o := o.next;
  END;
END gen_glovars;

PROCEDURE gen_module;
  VAR
    str : env.String;
   tmps : def.TMP_VAR;
    arg : ir.Arg;
      q : ir.TriadePtr;
   flag : ir.Local;
    dll : BOOLEAN;
   proto: pr.Proto;
   init, ret : ir.Node;
   begin_pos : ir.TPOS;  (* text position of module's BEGIN *)
   onlyProcName : pc.STRING;
   fake1,fake2:ir.ParamPtr;
BEGIN
<* IF pcvis THEN *>
  IF opt.pcvis THEN pcVis.vis(at.curr_mod.val, 0) END;
<* END *>
  IF env.shell.Active () THEN
    xcStr.dprn_txt (str,"Module length: %d lines", env.info.lines);
    env.shell.StartJob (str^, NumberOfProcs ());
  END;
  ope.modNames      :=NIL;
  ope.invalidModName:=NIL;
  at.UseFloatOps := FALSE;
  def.object_declaration(at.curr_mod);
  def.gen_type_descs(at.curr_mod);

  ope.skipChecknullOnGuard := env.config.Option("skipchecknullonguard");
  CalculateUsedByNested;

  make_prof_objs();
  prepare_excepttable_info();

  proc_list(at.curr_mod.type.mem, gen_proc);
  proc_list(at.curr_mod.type.prof, gen_proc);

  gen_excepttable;

  env.config.Equation( "gen_only_proc", onlyProcName);
  IF ((onlyProcName = NIL) OR (onlyProcName^ = at.curr_mod.name^))
     AND ((at.curr_mod.type.flag IN opt.LangsWithModuleConstructors) OR
           (at.profilingMode # xProfRTS.PROF_MODE_NONE))
  THEN
    at.curr_proc := at.curr_mod;
    at.curr_procno := def.proc_num(at.curr_mod);
    INC(at.curr_user_procno);
    begin_pos := at.curr_mod.val.pos;
    proto := pr.ProtoList[pr.ProcList[at.curr_procno].proto_no];
    dll := at.GENDLL IN at.COMP_MODE;
    ret_tmp := ir.UNDEFINED;

    Optim_Start;
    -- need to be placed between Optim_Start & Optim_Do cause creates ir.Local's
    gen_glovars(at.curr_mod.type.prof);

    decor_write_triading(at.curr_mod);
    gr.StartNewNode();
    IF at.main THEN
      IF ~env.config.Option ("NO_InitFPP") THEN
        gen_InitFPP(begin_pos);
      END;
      gen_INIT_HISTORY (begin_pos);
    END;
    ope.gen_modName(at.curr_mod^.pos, fake1,fake2);
    IF at.main & ~dll THEN
      gen_X2C_BEGIN (begin_pos, proto);
      gen_INIT_PROFILER (begin_pos);
      init := gr.currNode;
      ret := gr.NewNode ();
    ELSE
     (* initialization check *)
      flag := mod_ini_flag();
      gen_check_flag(begin_pos, flag, def.zz_zero, init, ret);
      gr.StartNode(init);
      q := ir.NewTriadeTInit(1, ir.o_assign, ir.y_RealVar,
         ir.Locals[flag].VarType, VAL(ir.SizeType, ir.Locals[flag].VarSize));
      q.Position := begin_pos;
      ir.MakeParNum(q.Params[0], def.zz_one);
      q.Name := flag;
      gr.AppendTr(q);

      IF dll & ( at.main OR ~(at.USEDLL IN at.COMP_MODE)) THEN
        gen_X2C_BEGINDLL (begin_pos, ret)
      END;
      (*
         X2C_BEGINDLL initializes the XDS library with ( X2C_GCAUTO, X2C_GCTHRESHOLD, X2C_HEAPLIMIT )
         It is different from X2C_BEGIN only in that neither argc nor argv is passed in

         One needs to generate X2C_BEGINDLL in either each BEGIN-part of all modules
         from DLL or its "dllmain" procedure because initialization of DLL ("dllmain")
         is invoked ( by the system ) before the control will be in the "main" procedure
         of EXE where a call to X2C_BEGIN is put too.
         ( Inside of "dllmain", the already initialized library may be need )

         X2C_BEGINDLL has the flag "already initialized" --   VitVit
      *)
    END;

    gen_prof_begin_module;

    gen_import_init (begin_pos);

    (* In code generated, the X2C_MODULE procedure MUST be after
       an initialization of modules imported ( XDS RT relies on it);
       otherwise problems with GC in DLL model will arise - VitVit *)

    IF def.md_desc # NIL THEN  (* module descriptor *)
      q := def.RTS_call(begin_pos, std.X2C_MODULE, TRUE);
      def.o_usage(begin_pos, def.md_desc, ir.GenModeSet{def.REF}, arg);
      ir.ParmByArg(q.Params[1], arg);
      gr.AppendTr(q);
    END;

    ini_labels;
    def.enter_statement(tmps);
    gen_alt(at.curr_mod.val.r, FALSE, FALSE, ret);  (* тело модуля *)
    def.exit_statement(tmps);
    def.tmp_vars := NIL;
    ASSERT(def.tmp_busy = NIL);

    gr.StartNode(ret);
    ir.SetSilentMode;
<* IF (TARGET_RISC OR TARGET_SPARC) THEN *>
    IF at.main & ~dll THEN
      q := def.RTS_call(at.curr_mod.end, std.X2C_EXIT, TRUE );
      gr.AppendTr(q);
    END;
<* END *>
--    gen_X2C_PROFILE_FINALLY;
    q := ir.NewTriadeInit(0, ir.o_ret, ir.t_void, 0);
    q.Position := at.curr_mod.end;
    gr.AppendTr(q);

    ir.SetNormalMode;
    Optim_Do;
  END;
  IF pc.xot_noninlinable IN at.curr_mod.xtags THEN
    -- delete redundand module body
    at.curr_mod.val.r := NIL;
  END;

  IF env.shell.Active () THEN
    env.shell.Comment  ("");
    env.shell.StartJob ("", 0);
  END;
END gen_module;

PROCEDURE wrn_obj(o: pc.OBJECT; no: INTEGER);
BEGIN
  env.errors.Warning(o.pos, no, o.name^);
END wrn_obj;

PROCEDURE check_objects(o: pc.OBJECT);
BEGIN
  WHILE o # NIL DO
    IF pc.OMARK_SET{at.omark_used, pc.omark_used_by_code} * o.marks # pc.OMARK_SET{} THEN
      EXCL(o.marks, at.omark_used);
      IF o.mode IN (pc.PROCs-pc.OB_SET{pc.ob_cproc}) THEN check_objects(o.type.mem) END;
    ELSIF o.pos.IsNull()
      OR (o.name = NIL) OR (o.name[0]=0X) OR (o.name[0]='&')
    THEN (* nothing *)
    ELSIF (o.mode = pc.ob_var) & ~(pc.otag_valpar IN o.tags) THEN
        IF nms.valid_name(o.name) THEN wrn_obj(o, 300) END;
    ELSIF o.mode IN pc.VARs   THEN wrn_obj(o, 301);
    ELSIF o.mode IN pc.PROCs  THEN wrn_obj(o, 303);
--  ELSIF o.mode = pc.ob_cons THEN env.errors.Warning(o.pos, 305);
    END;
    o:=o.next;
  END;
END check_objects;

(** ------------------------------ CODE --------------------------------- *)

PROCEDURE mark_intrinsic(o: pc.OBJECT);
  VAR type: pc.STRUCT; p: pc.OBJECT;
BEGIN
<* IF TARGET_VAX OR TARGET_68k OR TARGET_RISC OR TARGET_SPARC THEN *>
  RETURN
<* ELSE *>
  WHILE o # NIL DO
    IF o.mode IN pc.OB_SET{pc.ob_xproc, pc.ob_eproc, pc.ob_proc} THEN
      IF VAL(SHORTINT,ope.spec_op(o.name)) > 0 THEN
        type := o.type;
        IF type.base.mode IN pc.REALs THEN
           p := o.type.prof;
           IF (p # NIL) & (p.next = NIL) & (p.type.mode IN pc.REALs) THEN
             INCL(type.tags, pc.ttag_intrinsic);
           END;
        END;
      END;
    END;
    o := o.next;
  END;
<* END *>
END mark_intrinsic;

PROCEDURE clear_sys_modules;
  VAR i: pc.Mno;
     o: pc.OBJECT;
BEGIN
  IF pc.sys_mods = NIL THEN RETURN; END;
  FOR i := pc.ZEROMno TO SYSTEM.PRED( LEN(pc.sys_mods^) ) DO
    IF pc.sys_mods[i] # NIL THEN
      o := pc.sys_mods[i].type.prof;
      WHILE o # NIL DO
        o.ext := NIL;
        IF o.type <> NIL THEN
          o.type.ext := NIL;
          EXCL (o.type.marks, at.tmark_db_index);
        END;
        o := o^.next;
      END;
    END;
  END;
END clear_sys_modules;

PROCEDURE give_external_names;
---  VAR o: pc.OBJECT;
BEGIN

END give_external_names;

(* ----------------- c o d e   g e n e r a t i o n ------------------------ *)

TYPE
  CODE = POINTER TO RECORD (pc.code_rec) END;


PROCEDURE (c: CODE) clear_object(o :pc.OBJECT);
BEGIN
  at.clear_object (o);
END clear_object;

PROCEDURE (c: CODE) out_object(f: xfs.SymFile; o: pc.OBJECT );
BEGIN
  at.out_object(f, o );
END out_object;

PROCEDURE (c: CODE) inp_object(f: xfs.SymFile; o: pc.OBJECT; id: LONGINT);
BEGIN
  at.inp_object(f, o, id);
  (* затем почистим тэги -- по историческим причинам *)
  o.tags := o.tags - (pc.OTAG_SET{pc.otag_aux1 .. pc.otag_aux12} - pc.OTAG_SET{ at.otag_versionkey });
END inp_object;

PROCEDURE (c: CODE) skip_object(f: xfs.SymFile; id: LONGINT);
BEGIN
  at.skip_object(f, id);
END skip_object;

PROCEDURE (c: CODE) inp_struct(f: xfs.SymFile; s: pc.STRUCT; id: LONGINT);
BEGIN
END inp_struct;

PROCEDURE (c: CODE) skip_struct(f: xfs.SymFile; id: LONGINT);
BEGIN
END skip_struct;

PROCEDURE (c: CODE) selected;
BEGIN
  env.config.SetOption("__GEN_X86__", TRUE);
END selected;

<* IF NOT TARGET_VAX THEN *>
PROCEDURE copyFloat(s: ARRAY OF CHAR): LONGREAL;
(* necessary for platforms with obligatory memory alignment,
   such as PPC
 *)
  CONST size = 8;
  TYPE FPTR = POINTER TO ARRAY size OF CHAR;
  VAR r: LONGREAL;
      p: FPTR;
      i: INTEGER;
BEGIN
  r := 0.0;
  p:=SYSTEM.VAL(FPTR,SYSTEM.ADR(r));
  FOR i:=0 TO size-1 DO p^[i]:=s[i] END;
  RETURN r;
END copyFloat;
<* END *>

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

CONST BoundedTypes = pc.NUMs + pc.ORDs +
                     pc.TY_SET{ pc.ty_loc, pc.ty_AA };

PROCEDURE (c: CODE) set_min_value( t: pc.STRUCT; VAR v: pc.VALUE);

  PROCEDURE get_min_ord( bits: LONGINT; sign: BOOLEAN; VAR v: pc.VALUE );
  BEGIN
    IF sign THEN
      CASE bits OF
      | 8  : v.set_integer( MIN(SHORTINT) );
      | 16 : v.set_integer( MIN(INTEGER)  );
      | 32 : v.set_integer( MIN(LONGINT)  );
      | 64 : --v.set_integer( MIN(LONGINT) ); -- FIXME!
<* IF value96 THEN *>
             v.set_NDWord(0,0);
             v.set_NDWord(1,80000000H);
             v.set_NDWord(2,0FFFFFFFFH);
<* ELSE *>
             v.set_integer( MIN(LONGINT) ); -- FIXME!
<* END *>
      END;
    ELSE 
      v.set_integer(0);
    END;
  END get_min_ord;

BEGIN
  IF t.mode IN pc.TY_SET{ pc.ty_range, pc.ty_enum,
                               pc.ty_array_of, pc.ty_SS,
                               pc.ty_ZZ }
  THEN
    v.set_integer(0);
    RETURN;
  ELSIF NOT( t.mode IN BoundedTypes ) THEN
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
  <* IF TARGET_VAX THEN *>
    v.set_real( min_r );
  <* ELSE *>
    v.set_real( copyFloat(min_r) );
  <* END *>

  | pc.ty_longreal,
    pc.ty_ld_real  :
  <* IF TARGET_VAX THEN *>
    v.set_real( min_lr );
  <* ELSE *>
    v.set_real( copyFloat(min_lr) );
  <* END *>

<* END *>
  ELSE
    IF t.mode IN pc.ORDs THEN
      get_min_ord( bit_size(t.mode), t.mode IN pc.SIGNED_WHOLEs, v );
    ELSE
      v.set_integer(0);
    END;
  END;
  RETURN;
END set_min_value;

PROCEDURE (c: CODE) set_max_value( t: pc.STRUCT; VAR v: pc.VALUE);

  PROCEDURE get_max_ord( bits: LONGINT; sign: BOOLEAN; VAR v: pc.VALUE );
  BEGIN
    IF sign THEN
      CASE bits OF
      | 8  : v.set_integer( MAX(SHORTINT) );
      | 16 : v.set_integer( MAX(INTEGER)  );
      | 32 : v.set_integer( MAX(LONGINT)  );
      | 64 : --v.set_integer( MAX(LONGINT) ); -- FIXME!
<* IF value96 THEN *>
             v.set_NDWord(0,0FFFFFFFFH);
             v.set_NDWord(1,MAX(LONGINT));
             v.set_NDWord(2,0);
<* ELSE *>
             v.set_integer( MAX(LONGINT) ); -- FIXME!
<* END *>
      END;
    ELSE
      CASE bits OF
      | 8  : v.set_integer( 255 );
      | 16 : v.set_integer( 65535 );
      | 32 : v.set_integer(0);
             ASSERT( pc.longint_type # NIL );
             v.binary(pc.sb_minus, pc.longint_type.max,
                                   pc.longint_type.min);
      | 64 :
             v.set_integer(0);
             ASSERT( pc.longlongint_type # NIL );
             v.binary(pc.sb_minus, pc.longlongint_type.max,
                                   pc.longlongint_type.min);
      END;
    END;
  END get_max_ord;

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
  <* IF TARGET_VAX THEN *>
    v.set_real( max_r );
  <* ELSE *>
    v.set_real( copyFloat(max_r) );
  <* END *>

  | pc.ty_longreal,  
    pc.ty_ld_real  :
  <* IF TARGET_VAX THEN *>
    v.set_real( max_lr );
  <* ELSE *>
    v.set_real( copyFloat(max_lr) );
  <* END *>

<* END *>
  ELSE
    IF t.mode IN pc.ORDs THEN
      get_max_ord( bit_size(t.mode), t.mode IN pc.SIGNED_WHOLEs, v ); 
    ELSE
      v.set_integer(0);
    END;
  END;
  RETURN;
END set_max_value;


PROCEDURE (c: CODE) ini;

BEGIN
  c.bits_per_loc := 8;
  IF env.config.Option("m2base16") THEN
    c.locs_per_word:= 2;
  ELSE
    c.locs_per_word:= 4;
  END;
  c.FRETs:=pc.REALs + pc.WHOLEs + pc.CPLXs +
                pc.TY_SET{pc.ty_set} +
                pc.TY_SET{pc.ty_boolean,pc.ty_enum,pc.ty_range,pc.ty_char}+
                pc.TY_SET{pc.ty_array,pc.ty_loc}+
                pc.TY_SET{pc.ty_opaque,pc.ty_pointer}+
                pc.TY_SET{pc.ty_protection,pc.ty_proctype,pc.ty_record}+
                pc.TY_SET{pc.ty_longlongint, pc.ty_longlongcard };


  c.max_dim := 8;
  c.max_ext_lev := 15;
  c.def_storage := TRUE;
  c.max_sysflag := pc.flag_syscall;

  c.int16 := FALSE;
  c.index16 := FALSE;
  c.address16 := FALSE;
  c.max_index:=def.val(MAX(LONGINT));

  opt.get_target_options;

  def.ini;

  NEW(exit_list, 20);
  exit_cnt := 0;
  ret_tmp := ir.UNDEFINED;
<* IF statistics THEN *>
  ope.init_stat;
<* END *>
END ini;

PROCEDURE (c: CODE) exi;
BEGIN
<* IF statistics THEN *>
  ope.output_stat;
<* END *>
  clear_sys_modules;
  exit_list := NIL;
  def.exi;
END exi;

PROCEDURE (c: CODE) allocate (cu: pc.Mno; main: BOOLEAN; src_time: xfs.Time);
  VAR prof: pc.OBJECT;
BEGIN
  IF env.errors.err_cnt # 0 THEN RETURN END;

  at.curr_proc := NIL;
  at.curr_mno := cu;
  at.curr_mod := pc.mods[cu];
  at.main := main;

  prof := at.curr_mod.type.prof;
  IF env.config.Option("VERSIONKEY") THEN
    INCL(at.curr_mod.tags, at.otag_versionkey);
  END;
  IF env.config.Option("INTRINSIC") THEN
    mark_intrinsic(prof);
  END;

  give_external_names;

 <* IF TARGET_RISC OR TARGET_SPARC THEN *>
  def.collect_glob_vars(prof);
  def.alloc_glob_vars;
  def.clear_glob_info;
 <* END *>
END allocate;

(*
PROCEDURE vis_mods(mods-: pc.OBJECTS; cnt: INTEGER);
  VAR i: INTEGER; o: pc.OBJECT;
BEGIN
  FOR i := 0 TO cnt-1 DO
    o := mods[i];
    IF o = NIL THEN
      pcVis.print("NIL\n");
    ELSE
      pcVis.obj(o, 0);
      o := o.type.prof;
      WHILE o # NIL DO
        pcVis.obj(o, 1);
        o := o.next;
      END;
    END;
    pcVis.print("%.40c конец модуля %d\n", '-', i);
  END;
END vis_mods;
*)

PROCEDURE generate_obj_file;
VAR
  item: reg.ITEM;
BEGIN
  item := reg.SetFromEquation( opt.EQU_OBJFMT, opt.DefaultOBJFMT,
                               opt.objFormat, NIL );
  IF item = NIL THEN
    env.errors.Error(at.curr_mod.pos, 501);
    RETURN;
  END;

  WITH item: fc.FORM_OBJ DO
    item.generate;
  ELSE
    ASSERT(FALSE);
  END;
END generate_obj_file;

PROCEDURE (c: CODE) gen_code(cu: pc.Mno; main: BOOLEAN);
  VAR prof, mem : pc.OBJECT;
BEGIN
  at.curr_proc := NIL;
  at.curr_mno := cu;
  at.curr_mod := pc.mods[cu];
  at.main := main;

  prof := at.curr_mod.type.prof;
  mem  := at.curr_mod.type.mem;

  opt.get_target_options;
  opt.get_gencode_options;

  IF env.config.Option("INTRINSIC") THEN
    mark_intrinsic(prof);
    mark_intrinsic(mem);
  END;

  Select.InitOutput;

 <* IF TARGET_RISC OR TARGET_SPARC THEN *>
  TOC.Start;
  at.LocalTOC := at.new_work_object(at.make_name("%s_TOC",at.curr_mod.name^),
                                    NIL,
                                    at.curr_mod.type,
                                    pc.ob_cons,
                                    TRUE);
  def.collect_glob_vars(prof);
  def.collect_glob_vars(mem);
--  def.collect_glob_vars(at.work_objects); - эти объекты появятся только потом
  def.alloc_glob_vars;
  def.clear_glob_info;
 <* END *>

  Optim_Init;
  gen_module;
<* IF TARGET_IDB THEN *>
  IF env.InterViewMode THEN RETURN; END;
<* END *>

  IF env.errors.err_cnt # 0 THEN RETURN END;
  check_objects(at.curr_mod.type.mem);

 <* IF db_procs THEN *>
  pr.show_proc_list;
  pr.show_prototype_list;
 <* END *>

 <* IF TARGET_RISC OR TARGET_SPARC THEN *>
  TOC.Finish;
 <* END *>

  generate_obj_file;

END gen_code;

PROCEDURE (c: CODE) get_size(op: pc.SUB_MODE; t: pc.STRUCT): LONGINT;
  VAR sz: LONGINT;
BEGIN
  sz := def.type_size(t);
  CASE op OF
    |pc.su_bits : IF sz > 0 THEN sz := sz * 8 END;
    |pc.su_bytes:
    |pc.su_size :
  END;
  RETURN sz
END get_size;

PROCEDURE (c: CODE) get_offs(op: pc.SUB_MODE; o: pc.OBJECT): LONGINT;
VAR
  offs: LONGINT;
BEGIN
  offs := def.obj_offset(o);
  CASE op OF
  | pc.su_bit_offs : IF offs > 0 THEN offs := offs * 8 END;
  | pc.su_byte_offs:
  | pc.su_word_offs:
    IF NOT ODD(offs) THEN
      offs := -1;
    ELSE
      offs := offs DIV 2;
    END
  END;
  RETURN offs
END get_offs;


PROCEDURE (c: CODE) get_align(t: pc.STRUCT): SHORTINT;
BEGIN
  RETURN def.type_align(t)
END get_align;

PROCEDURE Set*;
  VAR code: CODE;
BEGIN
  ope.gen_sequence := gen_sequence;
  Color.WriteTest := WriteTest;
  Analysis.WriteTest := WriteTest;
  NEW(code);
  pc.code := code;
  code.sym_ident := (pc.sym_native + 1);
  code.valid_idents := { pc.sym_C          - pc.sym_base
                       , pc.sym_native     - pc.sym_base
                       , pc.sym_native + 1 - pc.sym_base };
  code.vers := at.CODE_VERSION;
  code.en_preopt := TRUE;
  code.en_tmpvar := TRUE;
  code.en_f_inline := TRUE;
  code.head_ext := NIL;
  code.code_ext := NIL;

  opt.InitOptsEqus;

--  exit_point := ir.UndefNode;
--  ir.NullPos := env.null_pos;
  opt.DefaultLevel := env.config.Level ();
  env.config.Save ();

END Set;

END opCode.
