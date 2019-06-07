<* NEW statistics- *>  -- collect some statistics on SL-1 source files
<* NEW db_e- *>
<* NEW db_procs- *>

MODULE opE;

IMPORT  pc := pcK,                         ir,
        def:= opDef,                       gr   := ControlGraph,
        pr := opProcs,                     tune := opTune,
        std:= opStd,                       Calc,
        at := opAttrs,                     opt  := Options,
        env:= xiEnv,                       ops  := opSample,
        str:= Strings,
<* IF db_e OR db_procs THEN *> io := opIO, <* END *>
        SYSTEM;
IMPORT xfs:=xiFiles,Printf;
IMPORT cmd:=CodeDef;
TYPE
  INT = ir.INT;

<* NEW setproc- *> (* использовать процедуры для операций со множествами *)


<* IF TARGET_SPARC THEN *>
CONST -- use shift instructions in bit operations
  rshift = ir.o_shr;
  lshift = ir.o_shl;
<* ELSE *>
CONST -- use rotate instructions in bit operations
  rshift = ir.o_ror;
  lshift = ir.o_rol;
<* END *>

CONST
  RANGEs = pc.TY_SET{pc.ty_range, pc.ty_boolean, pc.ty_enum};

CONST MAX_assign = 50;  

CONST
  sArc      =  ops.Arc ;      (* <Arc>,<номер узла> если у Arc номер узла < 0, *)
                                (* то это номер настоящий с минусом              *)
  sLabel    =  ops.Label;     (* <Label>,<номер узла>   *)
  sResult   =  ops.Result;    (* <Result>, <parameter>  *)
  sEnd      =  ops.End;      (* <End> -- конец шаблона *)

  sarg      =  ops.arg;       (* <Arg>,<номер аргумента> *)
  stmp      =  ops.tmp;       (* временная переменная *)
  snum      =  ops.num;       (* численная константа *)
  sloc      =  ops.loc;       (* номер локала *)
  sprc      =  ops.prc;       (* номер процедуры *)

  sOpt      =  ops.Opt;       (* пометка для опции триады (необяз.) *)
  sResType  =  ops.ResType;   (* пометка о типе результата (необяз.) *)

-- if FALSE (by default), then O2 "guard" statement checknils its argument
-- if TRUE, then the following code can be safely compiled:
--
-- a = foo();
-- b = a (MyType); -- no checknil here
-- if b = NIL THEN
-- ...
VAR skipChecknullOnGuard* : BOOLEAN; -- set in opCode.gen_module

<* IF db_e THEN *>

PROCEDURE print_arg(s-: ARRAY OF CHAR; arg-: gi.Arg);
BEGIN
  io.print("%s:(mode=%d, tag=%d, name=%d, offset=%d)\n",
             s, arg.mode, arg.tag, arg.name, arg.offset);
END print_arg;

<* END *>

<* IF statistics THEN *>

VAR 
  deref_cnt     : LONGINT; 
  read1bit_cnt  : LONGINT;  
  read2bit_cnt  : LONGINT;  
  read4bit_cnt  : LONGINT;  
  write1bit_cnt : LONGINT;  
  write2bit_cnt : LONGINT;  
  write4bit_cnt : LONGINT;  

PROCEDURE init_stat*; 
BEGIN
  deref_cnt := 0;
  read1bit_cnt  := 0;  
  read2bit_cnt  := 0;  
  read4bit_cnt  := 0;  
  write1bit_cnt := 0;  
  write2bit_cnt := 0;  
  write4bit_cnt := 0;  
END init_stat;

PROCEDURE output_stat*;
BEGIN
  env.info.print ("\nStatistics for module %s:\n" +
    "   DEREF = %d\n" +
    "   READ_1_BIT = %d\t\tWRITE_1_BIT = %d\n" +
    "   READ_2_BIT = %d\t\tWRITE_2_BIT = %d\n" +
    "   READ_4_BIT = %d\t\tWRITE_4_BIT = %d\n",
    at.curr_mod.name^,
    deref_cnt,
    read1bit_cnt,  write1bit_cnt,  
    read2bit_cnt,  write2bit_cnt,  
    read4bit_cnt,  write4bit_cnt);
END output_stat;

PROCEDURE count_bit_read (w: INT);
BEGIN
  CASE w OF
  | 1: INC (read1bit_cnt);
  | 2: INC (read2bit_cnt);
  | 4: INC (read4bit_cnt);
  END;
END count_bit_read;

PROCEDURE count_bit_write (w: INT);
BEGIN
  CASE w OF
  | 1: INC (write1bit_cnt);
  | 2: INC (write2bit_cnt);
  | 4: INC (write4bit_cnt);
  END;
END count_bit_write;

PROCEDURE count_deref (n : pc.NODE);
BEGIN
  ASSERT(n.mode = pc.nd_deref);
  IF (n.l.mode = pc.nd_var) & (n.l.obj = def.mem_adr) THEN RETURN END; 
  INC (deref_cnt);
END count_deref;

<* END *> 

PROCEDURE make_temp * (t: pc.STRUCT; VAR nm: ir.Local);
  VAR ty: ir.TypeType; size: LONGINT;
BEGIN
  ty := def.type_kind(t);
  size := def.type_size(t);
  def.make_temp_var(ty, size, nm);
END make_temp;

PROCEDURE mask_val(sz : ir.SizeType; l: LONGINT): pc.VALUE;
  VAR   s: SET;
    mask : pc.VALUE;
    MAX_BITS : INT;
BEGIN  (* l - число разрядов с 1-ей *)
  MAX_BITS := sz * def.SHORTCARD_BITS;
  ASSERT((0<l) AND (l <= MAX_BITS));
  IF l = MAX_BITS THEN
    mask := Calc.MaxValue(ir.t_unsign, sz);
  ELSE
    IF l < tune.BITSET_LEN THEN
      s := {0..l-1};
      mask := def.val(SYSTEM.VAL(LONGINT, s));
    ELSE
      mask := pc.value.new(env.null_pos, pc.ZZ_type);
      mask.set_NDWord(0, SYSTEM.VAL(SYSTEM.CARD32, -1));
      s := {0..l-1-tune.BITSET_LEN};
      mask.set_NDWord(1, SYSTEM.VAL(SYSTEM.CARD32, s));
    END;
  END;
  RETURN mask
END mask_val;

VAR modNames*      : POINTER TO ARRAY OF pc.OBJECT;
    invalidModName*: pc.OBJECT;

PROCEDURE gen_modName*(pos-: ir.TPOS; VAR p1, p2: ir.ParamPtr);
VAR dir,name,ext,fullName : pc.STRING;
    buf                   : ARRAY 64 OF CHAR;
    index, ln,col         : LONGINT;   
    module: pc.OBJECT;
    arg: ir.Arg;
BEGIN
  ir.modname_local := ir.UNDEFINED;
  IF modNames=NIL THEN
      NEW(modNames,env.getFnamesLen()+1); -- the last entry for 'unknown' fname
      invalidModName           := def.out_str('unknown file');
      invalidModName.type      := pc.new_type(pc.ty_array_of);
      invalidModName.type.base := pc.char_type;
      invalidModName.type.len  := LEN(buf);
  END;
  IF NOT pos.IsNull() THEN
      index:=env.getFnameIndex(pos);
      pos.unpack(fullName, ln, col); -- we need 'ln' always
      IF modNames[index]=NIL THEN
        xfs.sys.Get(fullName^, dir, name, ext);
        Printf.sprintf(buf, "%s.%s", name^, ext^);
        modNames[index] := def.out_str(buf);
        modNames[index].type := pc.new_type(pc.ty_array_of);
        modNames[index].type.base := pc.char_type;
        modNames[index].type.len := LEN(buf);
      END;
      module := modNames[index];
  ELSE
      ln     := -1;
      module := invalidModName;
<* IF MODE = "WORK" THEN *>
      env.info.print("\nUNKNOWN TRAP POSITION\n")
<* END *>
  END;
  def.o_usage(pos, module, ir.GenModeSet{def.REF}, arg);
  ir.modname_local := arg.name;
  IF p1#NIL THEN
    ir.ParmByArg(p1, arg);
  END;
  IF p2#NIL THEN
    ir.MakeParNum(p2, def.val(ln+1));
  END;
END gen_modName;

PROCEDURE gen_trap*(arg-: ir.Arg; tpos-: ir.TPOS; assert: BOOLEAN);
  VAR q: ir.TriadePtr;
BEGIN
  IF at.TRAP_proc IN at.COMP_MODE THEN
    q := def.RTS_call(tpos, std.X2C_TRAP_F, TRUE);
    ir.ParmByArg(q.Params[1], arg);
  ELSE
    q := ir.NewTriadeInit(3, ir.o_error, tune.index_ty, tune.index_sz);
    q.Position := tpos;
    ir.ParmByArg(q.Params[0], arg);
    gen_modName(tpos, q.Params[1], q.Params[2]);
  END;
  q.Options := q.Options + ir.OptionsSet{ir.o_Dangerous, ir.o_Silent};
  IF assert THEN 
    INCL(q.Options, ir.o_Assert);
  END;
  gr.AppendTr(q);
  gr.FinishNode;
END gen_trap;

PROCEDURE gen_trap_N*(num: LONGINT; tpos-: ir.TPOS);
  VAR arg: ir.Arg;
BEGIN
  CASE num OF
  | tune.functionException:    env.errors.Warning(tpos, 917);
  ELSE
  END;
  ir.MakeArgNum(arg, def.val(num));
  gen_trap(arg, tpos, FALSE);
END gen_trap_N;

PROCEDURE gen_conv_triade(tpos-: ir.TPOS;
                     op: ir.Operation;
                VAR arg: ir.Arg;                               (* что    *)
                from_ty: ir.TypeType; from_sz: ir.SizeType;    (* откуда *)
                to_ty  : ir.TypeType;   to_sz: ir.SizeType;    (* куда   *)
                 silent: BOOLEAN);
  VAR q: ir.TriadePtr;
BEGIN
  IF (from_ty = to_ty) & (from_sz = to_sz) THEN RETURN END;
  IF (to_ty = ir.t_float) OR (from_ty = ir.t_float) THEN
    at.was_float_triade := TRUE;
  END;
  q := ir.NewTriadeInit(1, op, to_ty, to_sz);
  q.Position := tpos;
  q.OpType := from_ty;
  q.OpSize := from_sz;
  IF silent THEN INCL(q.Options, ir.o_Silent); END;
  ir.ParmByArg(q.Params[0], arg);
  ir.GenResVar(q);
  gr.AppendTr(q);
  ir.MakeArgVar(arg, q.Name);
END gen_conv_triade;

PROCEDURE gen_conv(tpos-: ir.TPOS;
                VAR arg : ir.Arg;
                from_ty : ir.TypeType; from_sz: ir.SizeType;   
                to_ty   : ir.TypeType;   to_sz: ir.SizeType);  
BEGIN
  gen_conv_triade(tpos, ir.o_val, arg, from_ty, from_sz, to_ty, to_sz, TRUE);
END gen_conv;

PROCEDURE gen_conv_Ex(tpos-: ir.TPOS;
                VAR arg : ir.Arg;
                from_ty : ir.TypeType; from_sz: ir.SizeType;   
                to_ty   : ir.TypeType;   to_sz: ir.SizeType);  
VAR p : ir.TriadePtr;
BEGIN
  IF (arg.tag = ir.y_Variable) THEN
    p := ir.Vars[arg.name].Def;
    IF p.Op = ir.o_val THEN
      p.ResType := to_ty;
      p.ResSize := to_sz;
      RETURN;
    END;
  END;
  gen_conv(tpos, arg, from_ty, from_sz, to_ty, to_sz);
END gen_conv_Ex;

PROCEDURE gen_conversion (tpos-: ir.TPOS;
                       VAR arg : ir.Arg;
                  s_from, s_to : pc.STRUCT);
  VAR
    from_ty, to_ty: ir.TypeType;
    from_sz, to_sz: ir.SizeType;
BEGIN
  def.type_info (s_from, from_ty, from_sz);
  def.type_info (s_to, to_ty, to_sz);
  gen_conv_triade (tpos, ir.o_val, arg, from_ty, from_sz, to_ty, to_sz, TRUE);
END gen_conversion;

PROCEDURE gen_cast(tpos-: ir.TPOS;
                VAR arg : ir.Arg;                               (* что    *)
                from_ty : ir.TypeType; from_sz: ir.SizeType;    (* откуда *)
                to_ty   : ir.TypeType;   to_sz: ir.SizeType);   (* куда   *)
BEGIN
  gen_conv_triade(tpos, ir.o_cast, arg, from_ty, from_sz, to_ty, to_sz, FALSE);
END gen_cast;

PROCEDURE gen_neg(tpos-: ir.TPOS;
                     ty: ir.TypeType; sz: ir.SizeType;
                  check: BOOLEAN;
                VAR arg: ir.Arg);
  VAR q: ir.TriadePtr;
BEGIN
  q := ir.NewTriadeInit(1, ir.o_add, ty, sz);
  q.Position := tpos;
  IF check THEN
    INCL(q.Options, ir.o_Checked);
  END;
  ir.ParmByArg(q.Params[0], arg);
  ir.SetParamReverse(q.Params[0], TRUE);
  ir.GenResVar(q);
  gr.AppendTr(q);
  ir.MakeArgVar(arg, q.Name);  --  arg.operpos := tpos;
END gen_neg;

PROCEDURE gen_mult(tpos-: ir.TPOS;
                   larg-, rarg-: ir.Arg;
                   ty: ir.TypeType; sz: ir.SizeType;
                   VAR res: ir.Arg);
  VAR q: ir.TriadePtr; 
      v: pc.VALUE;
BEGIN
  IF (larg.tag = ir.y_NumConst) & (rarg.tag = ir.y_NumConst) THEN 
    v := Calc.Binary(pc.sb_mul, ty, sz, larg.value, rarg.value);
    IF NOT Calc.overflow THEN
      ir.MakeArgNum(res, v);
      RETURN
    END;
  END;
  q := ir.NewTriadeInit(2, ir.o_mul, ty, sz);
  q.Position := tpos;
  ir.ParmByArg(q.Params[0], larg);
  ir.ParmByArg(q.Params[1], rarg);
  ir.GenResVar(q);
  gr.AppendTr(q);
  ir.MakeArgVar(res, q.Name);
END gen_mult;

PROCEDURE gen_bin_op * (tpos-: ir.TPOS;
                        op: ir.Operation;
                        larg-, rarg: ir.Arg;
                        ty: ir.TypeType; sz: ir.SizeType;
                    VAR res: ir.Arg);
  VAR q: ir.TriadePtr; reverse: BOOLEAN;
BEGIN
  IF VAL(SHORTINT,op) > 0 THEN reverse := FALSE;
  ELSE ASSERT(VAL(SHORTINT,op) = -VAL(SHORTINT,ir.o_add)); op := ir.o_add;  reverse := TRUE;
    (* some trick to make subtruction - to introduce
       special flag in "arg" would be better
    *)
  END;
  IF (op = ir.o_add) & (rarg.tag = ir.y_NumConst) & rarg.value.is_neg() THEN
    reverse := NOT reverse;
    rarg.value := Calc.Unary(pc.su_neg, ty, sz, rarg.value);
  END;
  q := ir.NewTriadeInit(2, op, ty, sz);
  q.Position := tpos;
  ir.ParmByArg(q.Params[0], larg);
  ir.ParmByArg(q.Params[1], rarg);
  ir.SetParamReverse(q.Params[1], reverse);
  ir.GenResVar(q);
  gr.AppendTr(q);
  ir.MakeArgVar(res, q.Name);
END gen_bin_op;

PROCEDURE gen_bin_spec(tpos: ir.TPOS;
                  op: ir.Operation;
                  larg-: ir.Arg; res_ty: ir.TypeType; res_sz: ir.SizeType;
                  rarg-: ir.Arg;  op_ty: ir.TypeType;  op_sz: ir.SizeType
                  ): ir.VarNum;
  VAR q: ir.TriadePtr;
BEGIN
  q := ir.NewTriadeInit(2, op, res_ty, res_sz);
  q.Position := tpos;
  q.OpType := op_ty;
  q.OpSize := op_sz;
  ir.ParmByArg(q.Params[0], larg);
  ir.ParmByArg(q.Params[1], rarg);
  ir.GenResVar(q);
  gr.AppendTr(q);
  RETURN q.Name
END gen_bin_spec;

PROCEDURE gen_fi(tpos-: ir.TPOS;                    (* позиция           *)
                 n: INTEGER;                       (* число аргументов  *)
                ty: ir.TypeType; sz: ir.SizeType;  (* тип результата    *)
             args-: ARRAY OF ir.Arg;               (* массив аргументов *)
           VAR res: ir.Arg);                       (* результат         *)
  VAR q: ir.TriadePtr; i: INT;
BEGIN
  q := ir.NewTriadeInit(n, ir.o_fi, ty, sz);
  q.Position := tpos;
  FOR i := 0 TO n - 1 DO ir.ParmByArg(q.Params[i], args[i]) END;
  ir.GenResVar(q);
  gr.AppendTr(q);
  ir.MakeArgVar(res, q.Name);
END gen_fi;


PROCEDURE gen_check_nil(tpos-: ir.TPOS; arg-: ir.Arg; t: pc.STRUCT);
  VAR p: ir.TriadePtr;
BEGIN
  p := def.NewTriadeTS(1, ir.o_checknil, t);
  INCL(p.Options, ir.o_Dangerous);
  p.Position := tpos;
  ir.ParmByArg(p.Params[0], arg);
  gr.AppendTr(p);
END gen_check_nil;

PROCEDURE gen_storer * (tpos-: ir.TPOS;
                        ty: ir.TypeType; sz: ir.SizeType;
                        dest-, src-: ir.Arg);
  VAR q: ir.TriadePtr;
BEGIN
  q := ir.NewTriadeInit(2, ir.o_storer, ty, sz);
  q.Position := tpos;
  IF def.VOLATILE IN dest.mode THEN INCL(q.Options, ir.o_Volatile) END;
  ir.ParmByArg(q.Params[0], dest);
  ir.ParmByArg(q.Params[1], src);
  gr.AppendTr(q);
END gen_storer;

PROCEDURE gen_storer_loc(tpos-: ir.TPOS;
                     ty: ir.TypeType; sz: ir.SizeType;
                     v: ir.Local; offs: LONGINT;
                     src-: ir.Arg);
  VAR dest: ir.Arg;
BEGIN
  ir.MakeArgAddr(dest, v, offs);
  gen_storer(tpos, ty, sz, dest, src);
END gen_storer_loc;

PROCEDURE gen_bounds_check(tpos-: ir.TPOS;
                        min, max: pc.VALUE;
                    VAR arg     : ir.Arg;
                        NO      : ir.Node);
  VAR q: ir.TriadePtr;
    nn: ir.Node;  rarg, tmp_arg: ir.Arg;
BEGIN
  IF arg.tag = ir.y_NumConst THEN
    IF NOT( Calc.CompareValues( pc.sb_leq, min, arg.value,
                                tune.element_ty, tune.element_sz, TRUE ) &
            Calc.CompareValues( pc.sb_leq, arg.value, max,
                                tune.element_ty, tune.element_sz, TRUE ) )
    THEN
      gr.Goto(NO);
    END;
    IF NOT min.is_zero() THEN
       arg.value := Calc.Binary( pc.sb_minus, tune.element_ty, tune.element_sz,
                                 arg.value, min );
    END;
    RETURN;
  END;

  IF NOT min.is_zero() THEN
    ir.MakeArgNum(rarg, min);
    gen_bin_op(tpos,
              SYSTEM.VAL(ir.Operation,-VAL(SHORTINT,ir.o_add)), arg, rarg, tune.element_ty, tune.element_sz, arg);
    max := Calc.Binary(pc.sb_minus, tune.element_ty, tune.element_sz, max, min);
  END;
  -- this is necessary because we wanna check two conditions
  -- (arg>=0 && arg<=max) by one comparison.
  tmp_arg := arg;
  gen_conv_Ex(tpos, tmp_arg, tune.element_ty, tune.element_sz,
                            ir.t_unsign,     tune.element_sz);
  q := ir.NewTriadeInit(2, ir.o_le, ir.t_unsign(*!! see comment before *), tune.element_sz);
  q.Position := tpos;
  ir.ParmByArg(q.Params[0], tmp_arg );
  ir.MakeParNum(q.Params[1], max);
  gr.AppendTr(q);
  nn := gr.currNode;
  gr.StartNewNode();
  gr.NewArc(nn, gr.currNode, TRUE);
  gr.NewArc(nn, NO, FALSE);
END gen_bounds_check;

PROCEDURE gen_type_name(t: pc.STRUCT; VAR arg: ir.Arg);
BEGIN
  ASSERT((t.mode = pc.ty_record) & (t.flag IN pc.OOP_langs));
  def.create_record(t.obj);
  def.o_usage(ir.NullPos, t.obj, ir.GenModeSet{def.REF}, arg);
  def.add_offs(ir.NullPos, tune.SELF_offset, arg);
  def.deref(ir.NullPos, tune.addr_ty, tune.addr_sz, ir.OptionsSet{ir.o_Constant}, arg);
END gen_type_name;

PROCEDURE ^ gen_value(n: pc.NODE; md: ir.GenModeSet; VAR arg: ir.Arg);

PROCEDURE ^ gen_len*(n: pc.NODE;
                   dim: INTEGER; type: pc.STRUCT; VAR arg: ir.Arg);


PROCEDURE gen_type_ex(n: pc.NODE; this: ir.Arg; isThisEvaluated: BOOLEAN;
                      mdforthis:ir.GenModeSet; 
                      checkNull : BOOLEAN;
                      VAR arg: ir.Arg);
  VAR t: pc.STRUCT;
BEGIN
<* IF db_e THEN *>
  io.print_pos(n.pos); io.print(" gen_type(n.mode=%d)\n", n.mode);
<* END *>
  t := n.dynamic_type();
  IF t # NIL THEN (* тип известен статически *)
    <* IF db_e THEN *> io.print("        gen_type: статический тип"); <* END *>
    gen_type_name(t, arg);
  ELSE
    <* IF db_e THEN *> io.print("        gen_type: динамический тип"); <* END *>
    n := n.dynamic_type_expr();
    IF n.type.mode=pc.ty_pointer THEN
      IF isThisEvaluated THEN
        arg:=this;
      ELSE
        gen_value(n, ir.GenModeSet{}, arg);
      END;
      IF checkNull AND env.config.Option("CHECKNIL") THEN  (* it may be no node to get chk_tag *)
        gen_check_nil(n.pos, arg, n.type);
      END;
      IF def.REF IN mdforthis THEN
        def.deref(n.pos, tune.addr_ty, tune.addr_sz, ir.OptionsSet{}, arg);
      END;
      def.add_offs(n.pos, tune.TD_PTR_offset, arg);
      def.deref(n.pos, tune.addr_ty, tune.addr_sz, ir.OptionsSet{ir.o_Constant}, arg);
    ELSIF (n.mode=pc.nd_var) & (n.obj.mode=pc.ob_varpar) THEN
      def.o_attr(n.pos, n.obj, at.a_type, ir.GenModeSet{}, arg);
    ELSE ASSERT(FALSE);
    END;
  END;
END gen_type_ex;

PROCEDURE gen_type(n: pc.NODE; VAR arg: ir.Arg);
  VAR fakearg: ir.Arg;
BEGIN
  gen_type_ex(n,fakearg,FALSE,ir.GenModeSet{},TRUE,arg);
END gen_type;

PROCEDURE gen_is(n: pc.NODE);
  VAR q: ir.TriadePtr; arg, targ: ir.Arg;
BEGIN
  gen_type(n.l, arg);
  def.add_offs(n.pos, tune.BASEs_offset+tune.BASE_size*n.obj.type.len, arg);
  def.deref(n.pos, tune.addr_ty, tune.addr_sz, ir.OptionsSet{ir.o_Constant}, arg);

  gen_type_name(n.obj.type, targ);

  q := ir.NewTriadeInit(2, ir.o_eq, tune.addr_ty, tune.addr_sz);
  ir.ParmByArg(q.Params[0], arg);
  ir.ParmByArg(q.Params[1], targ);
  q.Position := n.pos;
  gr.AppendTr(q);
END gen_is;

PROCEDURE gen_strcmp(n: pc.NODE; op: ir.Operation): ir.TriadePtr;
  VAR q1,q2: ir.TriadePtr; arg: ir.Arg;
BEGIN
  q1 := def.RTS_call(n.pos, std.X2C_STRCMP_PROC, TRUE);
  gen_value(n.l, ir.GenModeSet{def.REF}, arg);
  ir.ParmByArg(q1.Params[1], arg);
  gen_len(n.l, 0, def.SIZE_T, arg);
  ir.ParmByArg(q1.Params[2], arg);
  gen_value(n.r, ir.GenModeSet{def.REF}, arg);
  ir.ParmByArg(q1.Params[3], arg);
  gen_len(n.r, 0, def.SIZE_T, arg);
  ir.ParmByArg(q1.Params[4], arg);
  gr.AppendTr(q1);
  q2 := ir.NewTriadeInit(2, op, ir.t_int, tune.index_sz);
  q2.Position := n.pos;
  ir.MakeParVar(q2.Params[0], q1.Name);
  ir.MakeParNum(q2.Params[1], def.zz_zero);
  gr.AppendTr(q2);
  RETURN q2
END gen_strcmp;

PROCEDURE gen_check (op:  ir.Operation;
                     chk: ir.OptionsSet;
                     larg-, rarg-: ir.Arg;
                     ty: ir.TypeType;
                     sz: ir.SizeType;
                     tpos: ir.TPOS);
  VAR q: ir.TriadePtr;
BEGIN
  q := ir.NewTriadeInit(2, op, ty, sz);
  q.Position := tpos;
  q.Options := q.Options + chk + ir.OptionsSet{ir.o_Dangerous};
  ir.ParmByArg(q.Params[0], larg);
  ir.ParmByArg(q.Params[1], rarg);
  gr.AppendTr(q);
END gen_check;

PROCEDURE ^ gen_index_check * (n: pc.NODE;  (* - expression *)
                               l: pc.NODE;  (* - array or set *)
                             dim: INTEGER;
                             chk: ir.OptionsSet;
                         VAR arg: ir.Arg);

PROCEDURE ^ gen_size(n: pc.NODE;
                   dim: INTEGER; VAR arg: ir.Arg);

PROCEDURE ^ gen_complex_value*(tpos: ir.TPOS;
                                  n: pc.NODE;
                             gen_re,
                             gen_im: BOOLEAN;
                          VAR re,im: ir.Arg);

CONST GENERAL_TYPEs = pc.TY_SET{pc.ty_ZZ, pc.ty_RR, pc.ty_CC};

PROCEDURE args_type(n: pc.NODE): pc.STRUCT;   (* тип аргументов бин.операции *)
  VAR type: pc.STRUCT;
BEGIN
  type := n.l.type;
  IF type.mode IN GENERAL_TYPEs THEN
    type := n.r.type;
    ASSERT(NOT (type.mode IN GENERAL_TYPEs));
  END;
  RETURN type
END args_type;


PROCEDURE gen_index_value(n: pc.NODE; VAR arg: ir.Arg);
  VAR ntype: pc.STRUCT;
    ty: ir.TypeType; sz: ir.SizeType;
BEGIN
  ntype := n.type;  ASSERT(def.is_scalar(ntype));
  def.index_type_info(ntype, ty, sz);
  gen_value(n, ir.GenModeSet{}, arg);
  IF (arg.tag # ir.y_NumConst)
    & ((ty # tune.index_ty) OR (sz # tune.index_sz))
  THEN
    gen_conversion (n.pos, arg, ntype, def.INDEX_T);
  END;
END gen_index_value;

PROCEDURE gen_bound_trap():ir.Node;
  VAR n: ir.Node;
      q: ir.TriadePtr;
BEGIN
  n:= gr.NewNode();
  q := ir.NewTriadeInit(3, ir.o_error, tune.index_ty, tune.index_sz);
  ir.MakeParNum(q.Params[0], Calc.GetValue(0, tune.index_ty, tune.index_sz));
  gen_modName(env.null_pos, q.Params[1], q.Params[2]);
  gr.PutTriadeFirst(q,n);
  RETURN n;
END gen_bound_trap;

PROCEDURE gen_in (n: pc.NODE; YES, NO: ir.Node);
  VAR q: ir.TriadePtr;
    larg, rarg, arg, delt: ir.Arg;
    type, base: pc.STRUCT;
    ty: ir.TypeType; sz: ir.SizeType;
    opts: ir.OptionsSet;
    scalar: BOOLEAN;
BEGIN
  type := n.r.type;
  scalar := def.is_scalar(type);
  base := type.base;
  gen_index_value(n.l, larg);
  IF ~env.config.Option("no_set_range_check") THEN
    gen_bounds_check(n.pos, base.min, base.max, larg, NO(*gen_bound_trap()*));
  END;
  IF scalar THEN
    gen_value(n.r, ir.GenModeSet{}, rarg);
    --ty := tune.index_ty;
    --sz := tune.index_sz;
    def.type_info(type, ty, sz);
  ELSIF
    type.len <= tune.BITSET_LEN_InInclExcl
  THEN
    gen_value(n.r, ir.GenModeSet{def.REF}, rarg);
    ty := ir.t_ref;
    sz := tune.addr_sz;
--    def.type_info(type, ty, sz);
--    def.deref(n.r.pos,ty,sz,ir.OptionsSet{},rarg);
  ELSE
    gen_value(n.r, ir.GenModeSet{def.REF}, rarg);
    IF def.VOLATILE IN rarg.mode THEN opts := ir.OptionsSet{ir.o_Volatile};
    ELSE                              opts := ir.OptionsSet{};
    END;
    ir.MakeArgNum(arg, def.val(tune.BITSET_LEN));
    gen_bin_op(n.pos, ir.o_div, larg, arg, tune.index_ty, tune.index_sz, arg);
    ir.MakeArgNum(delt, def.val(tune.lset_sz));
    gen_bin_op(n.pos, ir.o_mul, arg, delt, tune.index_ty, tune.index_sz, arg);
    gen_bin_op(n.pos, ir.o_add, rarg, arg, tune.addr_ty, tune.addr_sz, rarg);
    def.deref(n.pos, tune.lset_ty, tune.lset_sz, opts, rarg);
    ir.MakeArgNum(arg, def.val(tune.BITSET_LEN));
    gen_bin_op(n.pos, ir.o_mod, larg, arg, tune.index_ty, tune.index_sz, larg);
    ty := tune.lset_ty;
    sz := tune.lset_sz;
  END;
  q := ir.NewTriadeInit(2, ir.o_in, ty, sz);
  q.Position := n.pos;
  ir.ParmByArg(q.Params[0], larg);
  ir.ParmByArg(q.Params[1], rarg);
  gr.AppendTr(q);
  gr.NewArc(gr.currNode, YES, TRUE);
  gr.NewArc(gr.currNode, NO,  TRUE);
  gr.FinishNode;
END gen_in;

PROCEDURE gen_set_compare(n: pc.NODE; YES, NO: ir.Node);
  VAR q: ir.TriadePtr;
    op: ir.Operation;
    larg, rarg: ir.Arg;
    args: ARRAY 2 OF ir.Arg;
    a1_arg, b1_arg: ir.Arg;
    l, a, b : ir.Arg;
    l1, a1, b1, v: ir.VarNum;
    len, delt, mask: pc.VALUE;
    loop, cont: ir.Node;
    type: pc.STRUCT;
    lopt, ropt: ir.OptionsSet;
    h, words : LONGINT;
BEGIN
<* IF db_e THEN *>
  io.print_pos(n.pos); io.print("gen_set_compare\n");
<* END *>
  type := args_type(n);
  ASSERT((n.mode = pc.nd_binary) & (type.mode = pc.ty_set) & (NOT def.is_scalar(type)) );

  gen_value(n.r, ir.GenModeSet{def.REF}, rarg);
  IF def.VOLATILE IN rarg.mode THEN ropt := ir.OptionsSet{ir.o_Volatile} ELSE ropt := ir.OptionsSet{} END;
  gen_value(n.l, ir.GenModeSet{def.REF}, larg);
  IF def.VOLATILE IN larg.mode THEN lopt := ir.OptionsSet{ir.o_Volatile} ELSE lopt := ir.OptionsSet{} END;
  CASE n.sub OF
  | pc.sb_equ: op := ir.o_eq;
  | pc.sb_neq: op := ir.o_eq;    cont := YES; YES := NO; NO := cont;
  | pc.sb_leq: op := ir.o_leset;
  | pc.sb_geq: op := ir.o_leset; a := rarg; rarg := larg; larg := a;
  END;
  <* IF setproc THEN *>
    IF (op = ir.o_eq) THEN q := def.RTS_call(n.pos, std.X2C_SET_EQU);
    ELSE                   q := def.RTS_call(n.pos, std.X2C_SET_LEQ);
    END;
    ir.MakeParNum(q.Params[3], def.val(type.len));
    ir.ParmByArg(q.Params[2], rarg);
    ir.ParmByArg(q.Params[1], larg);
    gr.AppendTr(q);
  <* ELSE *>
    ir.SetSilentMode();
    delt := def.val(tune.lset_sz);
    h     := (type.len MOD tune.BITSET_LEN);
    words := def.type_size(type) DIV tune.lset_sz;
    IF h # 0 THEN
      DEC(words);
    END;
    len := def.val(words);
    ASSERT((words#1) OR (h#0));

    loop := gr.NewNode();                     (* loop: *)
    gr.StartNode(loop);

    ir.GenVar(ir.TEMPORARY, a1, NIL);
    ir.GenVar(ir.TEMPORARY, b1, NIL);
    ir.MakeArgVar(a1_arg, a1);
    ir.MakeArgVar(b1_arg, b1);

    IF words > 1 THEN
      ir.MakeArgNum(args[0], len);              (* l := fi(len-1, l1) *)
      ir.GenVar(ir.TEMPORARY, l1, NIL);
      ir.MakeArgVar(args[1], l1);
      gen_fi(n.pos, 2, tune.index_ty, tune.index_sz, args, l);

      args[0] := larg;                          (* a := fi(larg, a1)  *)
      ir.MakeArgVar(args[1], a1);
      gen_fi(n.pos, 2, tune.addr_ty, tune.addr_sz, args, a);

      args[0] := rarg;                          (* b := fi(rarg, b1)  *)
      ir.MakeArgVar(args[1], b1);
      gen_fi(n.pos, 2, tune.addr_ty, tune.addr_sz, args, b);

    ELSE
      l1 := ir.UNDEFINED;
      a := larg;
      b := rarg;
    END;


    q := ir.NewTriadeInit(2, ir.o_add, tune.addr_ty, tune.addr_sz);
    q.Position := n.pos;                      (* a1 := a + delt    *)
    ir.ParmByArg(q.Params[0], a);
    ir.MakeParNum(q.Params[1], delt);
    ir.SetDef(a1, q);
    gr.AppendTr(q);

    larg := a;                                (* t := *a           *)
    def.deref(n.pos, ir.t_unsign, tune.lset_sz, lopt, larg);

    q := ir.NewTriadeInit(2, ir.o_add, tune.addr_ty, tune.addr_sz);
    q.Position := n.pos;                      (* b1 := b + delt    *)
    ir.ParmByArg(q.Params[0], b);
    ir.MakeParNum(q.Params[1], delt);
    ir.SetDef(b1, q);
    gr.AppendTr(q);

    rarg := b;                                (* s := *b           *)
    def.deref(n.pos, tune.lset_ty, tune.lset_sz, ropt, rarg);

    q := ir.NewTriadeInit(2, op, tune.lset_ty, tune.lset_sz);
    q.Position := n.pos;                      (* if a = b then LL else NO *)
    ir.ParmByArg(q.Params[0], larg);
    ir.ParmByArg(q.Params[1], rarg);
    gr.AppendTr(q);
                                              (* LL:                  *)
    IF words > 1 THEN
      gr.StartNewNode();

      q := ir.NewTriadeInit(2, ir.o_add, tune.index_ty, tune.index_sz);
      q.Position := n.pos;                      (* l1 := l - 1      *)
      ir.ParmByArg(q.Params[0], l);
      ir.MakeParNum(q.Params[1], def.zz_one);
      ir.SetParamReverse(q.Params[1], TRUE);
      ir.SetDef(l1, q);
      gr.AppendTr(q);

      q := ir.NewTriadeInit(2, ir.o_eq, tune.index_ty, tune.index_sz);
      q.Position := n.pos;                      (* if l1 = 0 then LL1 else loop *)
      ir.MakeParVar(q.Params[0], l1);
      ir.MakeParNum(q.Params[1], def.zz_zero);
      gr.AppendTr(q);
    ELSE
      gr.StartNewNode();
    END;

    gr.NewArc(loop, gr.currNode, FALSE);
    gr.NewArc(loop, NO, TRUE);

    IF h = 0 THEN
      gr.NewArc(gr.currNode, YES, TRUE);
      gr.NewArc(gr.currNode, loop, FALSE);
      gr.FinishNode;
      ir.SetNormalMode();
      RETURN
    END;

    IF words > 1 THEN
      cont := gr.NewNode();                     (* LL1:               *)
      gr.NewArc(gr.currNode, cont, FALSE);
      gr.NewArc(gr.currNode, loop, FALSE);
      gr.StartNode(cont);
    END;

    mask :=  mask_val(tune.lset_sz, h);


    def.deref(n.pos, tune.lset_ty, tune.lset_sz, lopt, a1_arg); (* t1 := *a1          *)
    def.deref(n.pos, tune.lset_ty, tune.lset_sz, ropt, b1_arg); (* t2 := *b1          *)
    IF op = ir.o_eq THEN
      q := ir.NewTriadeInit(2, ir.o_xor, tune.lset_ty, tune.lset_sz);
      q.Position := n.pos;                      (* v := t1 xor s1     *)
      ir.ParmByArg(q.Params[0], a1_arg);
      ir.ParmByArg(q.Params[1], b1_arg);
      ir.GenResVar(q);
      gr.AppendTr(q);
      v := q.Name;

      q := ir.NewTriadeInit(2, ir.o_and, tune.lset_ty, tune.lset_sz);
      q.Position := n.pos;                      (* v1 := v and mask   *)
      ir.MakeParVar(q.Params[0], v);
      ir.MakeParNum(q.Params[1], mask);
      ir.GenResVar(q);
      gr.AppendTr(q);
      v := q.Name;

      q := ir.NewTriadeInit(2, ir.o_eq,  tune.lset_ty, tune.lset_sz);
      q.Position := n.pos;                      (* if v1 = 0 then YES else NO *)
      ir.MakeParVar(q.Params[0], v);
      ir.MakeParNum(q.Params[1], def.zz_zero);
      gr.AppendTr(q);
    ELSE
      q := ir.NewTriadeInit(2, ir.o_and, ir.t_unsign, tune.lset_sz);
      q.Position := n.pos;                      (* v := t1 and mask     *)
      ir.ParmByArg(q.Params[0], a1_arg);
      ir.MakeParNum(q.Params[1], mask);
      ir.GenResVar(q);
      gr.AppendTr(q);
      v := q.Name;

      q := ir.NewTriadeInit(2, ir.o_leset,  ir.t_unsign, tune.lset_sz);
      q.Position := n.pos;                      (* if v <= s1 then YES else NO *)
      ir.MakeParVar(q.Params[0], v);
      ir.ParmByArg(q.Params[1], b1_arg);
      gr.AppendTr(q);
    END;
    ir.SetNormalMode();
  <* END *>
  gr.NewArc(gr.currNode, YES, TRUE);
  gr.NewArc(gr.currNode, NO,  TRUE);
  gr.FinishNode;
<* IF db_e THEN *>
  io.print_pos(n.pos); io.print("gen_set_compare ---> \n");
<* END *>
END gen_set_compare;


PROCEDURE gen_condition*(n: pc.NODE;  YES, NO: ir.Node);
  VAR q: ir.TriadePtr;  arg0, arg1: ir.Arg;  nn: ir.Node;
    type: pc.STRUCT;
-- base: pc.STRUCT;


  PROCEDURE gen_comp0;
  BEGIN
  <* IF db_e THEN *> io.print("gen_comp0\n"); <* END *>
    gen_value(n, ir.GenModeSet{}, arg0);
    q := def.NewTriadeTS(2, ir.o_eq, n.type);
    q.Position := n.pos;
    ir.ParmByArg(q.Params[0], arg0);
    ir.MakeParNum(q.Params[1], def.bool0);
    gr.AppendTr(q);
    gr.NewArc(gr.currNode, NO,  TRUE);
    gr.NewArc(gr.currNode, YES, TRUE);
    gr.FinishNode;
  END gen_comp0;

  PROCEDURE gen_bin_compare(op: ir.Operation; sw_par, sw_arc: BOOLEAN);
  BEGIN
  <* IF db_e THEN *>
    io.print_pos(n.pos); io.print("gen_bin_compare\n");
  <* END *>
    IF type.mode IN (pc.REALs+pc.CPLXs) THEN
      at.was_float_triade := TRUE
    END;
    IF def.is_scalar(type) THEN
      gen_value(n.l, ir.GenModeSet{}, arg0);
      gen_value(n.r, ir.GenModeSet{}, arg1);
      q := def.NewTriadeTS(2, op, type);
      q.Position := n.pos;
      ir.ParmByArg(q.Params[0], arg0);
      ir.ParmByArg(q.Params[1], arg1);
      gr.AppendTr(q);
    ELSIF (type.mode IN pc.ARRs) & def.is_char(type.base) THEN
      q := gen_strcmp(n, op);
    ELSE ASSERT(FALSE);
    END;
    gr.NewArc(gr.currNode, YES, TRUE);
    gr.NewArc(gr.currNode, NO,  TRUE);
    IF sw_par THEN ir.SwapParams(q.Params[0], q.Params[1]) END;
    IF sw_arc THEN gr.ExchangeArcs(gr.currNode) END;
    gr.FinishNode;
  END gen_bin_compare;

  PROCEDURE gen_equal(neq : BOOLEAN);
  VAR
    arg1, arg1im: ir.Arg;
    arg2, arg2im: ir.Arg;
    tmp: ir.Node;
    sz: ir.SizeType;
  BEGIN
  <* IF db_e THEN *> io.print("gen_equal\n"); <* END *>
    IF def.is_scalar(type)
      OR ((type.mode IN pc.ARRs) & def.is_char(type.base))
    THEN
      gen_bin_compare(ir.o_eq, FALSE, neq);
    ELSIF type.mode IN pc.CPLXs THEN
      at.was_float_triade := TRUE;
      sz := def.re_im_sz(type);
      gen_complex_value(n.pos, n.l, TRUE, TRUE, arg1, arg1im);
      gen_complex_value(n.pos, n.r, TRUE, TRUE, arg2, arg2im);
      q := ir.NewTriadeInit(2, ir.o_eq, ir.t_float, sz);
      q.Position := n.pos;
      ir.ParmByArg(q.Params[0], arg1);
      ir.ParmByArg(q.Params[1], arg2);
      gr.AppendTr(q);
      tmp := gr.NewNode();
      gr.NewArc(gr.currNode, tmp, FALSE);
      IF neq THEN gr.NewArc(gr.currNode, YES, FALSE);
      ELSE        gr.NewArc(gr.currNode, NO,  FALSE);
      END;
      gr.FinishNode;
      gr.StartNode(tmp);
      q := ir.NewTriadeInit(2, ir.o_eq, ir.t_float, sz);
      q.Position := n.pos;
      ir.ParmByArg(q.Params[0], arg1im);
      ir.ParmByArg(q.Params[1], arg2im);
      gr.AppendTr(q);
      gr.NewArc(gr.currNode, YES, TRUE);
      gr.NewArc(gr.currNode, NO, TRUE);
      IF neq THEN gr.ExchangeArcs(gr.currNode) END;
      gr.FinishNode;
    ELSIF type.mode = pc.ty_set THEN
      gen_set_compare(n, YES, NO);
      RETURN
    ELSE
      q := def.RTS_call(n.pos, std.MEM_CMP, TRUE);
      gen_value(n.l, ir.GenModeSet{def.REF}, arg1);
      ir.ParmByArg(q.Params[1], arg1);
      gen_value(n.r, ir.GenModeSet{def.REF}, arg2);
      ir.ParmByArg(q.Params[2], arg2);
      def.gen_sizeof(n.pos, type, arg2);
      ir.ParmByArg(q.Params[3], arg2);
      gr.AppendTr(q);
      ir.MakeArgVar(arg1, q.Name);
      q := ir.NewTriadeInit(2, ir.o_eq, tune.index_ty, tune.index_sz);
      q.Position := n.pos;
      ir.ParmByArg(q.Params[0], arg1);
      ir.MakeParNum(q.Params[1], def.bool0);
      gr.AppendTr(q);
      gr.NewArc(gr.currNode, NO, TRUE);
      gr.NewArc(gr.currNode, YES, TRUE);
      IF neq THEN gr.ExchangeArcs(gr.currNode) END;
      gr.FinishNode;
    END;
  END gen_equal;

--VAR ty: ir.TypeType; sz: ir.SizeType;

BEGIN  (* - - - - g e n _ c o n d i t i o n - - - - *)
<* IF db_e THEN *>
  io.print_pos(n.pos); io.print("gen_condition(n.mode=%d)\n", n.mode);
<* END *>

  CASE n.mode OF
  | pc.nd_var, pc.nd_field, pc.nd_index
   ,pc.nd_deref, pc.nd_call, pc.nd_replace
   ,pc.nd_value:
        gen_comp0;

  | pc.nd_binary:
        type := args_type(n);
        CASE n.sub OF
        | pc.sb_equ, pc.sb_neq:
            gen_equal(n.sub = pc.sb_neq);
        | pc.sb_lss:
            gen_bin_compare(ir.o_le, TRUE, TRUE);
        | pc.sb_leq:
            IF (type.mode # pc.ty_set) THEN
              gen_bin_compare(ir.o_le, FALSE, FALSE);
            ELSIF def.is_scalar(type) THEN
              gen_bin_compare(ir.o_leset, FALSE, FALSE);
            ELSE gen_set_compare(n, YES, NO);
            END;
        | pc.sb_gtr:
            gen_bin_compare(ir.o_le, FALSE, TRUE);
        | pc.sb_geq:
            IF (type.mode # pc.ty_set) THEN
              gen_bin_compare(ir.o_le, TRUE, FALSE);
            ELSIF def.is_scalar(type) THEN
              gen_bin_compare(ir.o_leset, TRUE, FALSE);
            ELSE gen_set_compare(n, YES, NO);
            END;
        | pc.sb_in:
            gen_in(n, YES, NO);
        | pc.sb_cand:
            nn := gr.NewNode();
            gen_condition(n.l, nn, NO);
            gr.currNode := nn;
            gen_condition(n.r, YES, NO);
        | pc.sb_cor:
            nn := gr.NewNode();
            gen_condition(n.l, YES, nn);
            gr.currNode := nn;
            gen_condition(n.r, YES, NO);
    (*  | pc.sb_bit:    (* SYSTEM.BIT *)                                ?? *)
        END;
  | pc.nd_unary:
       CASE n.sub OF
       | pc.su_is:
           gen_is(n);
           gr.NewArc(gr.currNode, YES, TRUE);
           gr.NewArc(gr.currNode, NO,  TRUE);
           gr.FinishNode;
   (*  | pc.su_cc:   надо как следует разобраться - ?? *)
       | pc.su_conv ,pc.su_cast:
           gen_comp0;
       | pc.su_odd:
           gen_value(n.l, ir.GenModeSet{}, arg0);
           q := def.NewTriadeTS(1, ir.o_odd, n.l.type);
           q.Position := n.pos;
           ir.ParmByArg(q.Params[0], arg0);
           gr.AppendTr(q);
           gr.NewArc(gr.currNode, YES, TRUE);
           gr.NewArc(gr.currNode, NO,  TRUE);
           gr.FinishNode;
       | pc.su_not:
           gen_condition(n.l, NO, YES);
       END;
  END;
  ASSERT(gr.currNode = ir.UndefNode);
END gen_condition;

PROCEDURE gen_boolean_value(n: pc.NODE; neg: BOOLEAN; VAR arg: ir.Arg);
  VAR yes, no, cont: ir.Node; q: ir.TriadePtr;
BEGIN
  yes := gr.NewNode();
  no  := gr.NewNode();
  IF neg THEN gen_condition(n, no, yes);
  ELSE        gen_condition(n, yes, no);
  END;
  cont := gr.NewNode();
  gr.StartNode(yes);
  gr.Goto(cont);
  gr.StartNode(no);
  gr.Goto(cont);
  gr.StartNode(cont);
  q := def.NewTriadeTS(2, ir.o_fi, n.type);
  q.Position := n.pos;
  ir.MakeParNum(q.Params[0], def.bool1);   (* true  *)
  ir.MakeParNum(q.Params[1], def.bool0);   (* false *)
  ir.GenResVar(q);
  gr.AppendTr(q);
  ir.MakeArgVar(arg, q.Name);
END gen_boolean_value;

PROCEDURE sequence_size(n: pc.NODE): LONGINT;
  VAR l: pc.NODE; sz: LONGINT;
BEGIN
  l := n.l; sz := 0;
  WHILE l#NIL DO
    IF l.type.mode IN pc.REALs THEN
      INC(sz, tune.longreal_sz)
    ELSIF l.type.mode IN pc.SEQ_ARRs THEN
      INC(sz, tune.addr_sz + 2 * tune.index_sz)
    ELSE
      INC(sz, def.type_size(l.type))
    END;
    l := l.next;
  END;
  RETURN sz;
END sequence_size;

CONST len_avl=pc.TY_SET{pc.ty_array,pc.ty_SS,pc.ty_set};

PROCEDURE gen_len_usage(tpos-: pc.TPOS;
                            o: pc.OBJECT; dim: INTEGER;
                      VAR arg: ir.Arg);
  VAR t: pc.STRUCT; i: LONGINT;
    kind : SHORTINT; a: at.ATTR_EXT;
BEGIN
--  io.print("gen_len_usage('%s',dim=%d)\n", o.name^, dim);
  t:=o.type;
  FOR i:=0 TO dim-1 DO ASSERT(t.mode IN pc.ARRs); t:=t.base END;
  IF t.mode IN len_avl THEN
    def.c_number(t.len, arg); RETURN;
  END;
  IF t.mode=pc.ty_array_of THEN
    kind := at.a_len+SHORT(dim);
    a := at.attr(o.ext, kind);
    IF a # NIL THEN def.o_attr(tpos, o, kind, ir.GenModeSet{}, arg); RETURN END;
  END;
  env.errors.Error(tpos, 1006, dim);
  ir.MakeArgNum(arg, def.zz_one);
END gen_len_usage;

PROCEDURE gen_len*(n: pc.NODE; dim: INTEGER; type: pc.STRUCT;
                                                     VAR arg: ir.Arg);
(* dim номер индексации, слева направо *)
  VAR t,f: pc.STRUCT; i,j: LONGINT;
    larg: ir.Arg;
    kind : SHORTINT; a: at.ATTR_EXT;
BEGIN
  t:=n.type;
  FOR i:=0 TO dim-1 DO ASSERT(t.mode IN pc.ARRs); t:=t.base END;
  IF t.mode IN len_avl THEN
    IF (n.mode = pc.nd_unary) AND (n.l.mode = pc.nd_value) AND (n.l.type.mode = pc.ty_SS) AND (n.type.mode = pc.ty_array) AND (n.type.base.mode = pc.ty_char) THEN
      (* it's a case of passing string literal to formal type open array of char's array *)
      def.c_number(1, arg); RETURN
    ELSE
      def.c_number(t.len, arg); RETURN
    END;
  ELSIF t.mode IN pc.WHOLEs+pc.REALs THEN
    IF (n.mode = pc.nd_value) THEN
      (* it's a case of passing ZZ and RR value to formal type open array of wholes *)
      def.c_number(1, arg); RETURN
    END;
  END;
  IF t.mode=pc.ty_array_of THEN
    CASE n.mode OF
    | pc.nd_var:
        kind := at.a_len+SHORT(dim);
        a := at.attr(n.obj.ext, kind);
        IF a # NIL THEN def.o_attr(n.pos, n.obj, kind, ir.GenModeSet{}, arg); RETURN END;
    | pc.nd_deref:
        IF NOT (n.l.type.flag IN opt.LangsWithOpenArrays) THEN
          env.errors.Fault (n.pos, 1008);
          RETURN;
        END;
        def.t_def(n.type);
        gen_value(n.l, ir.GenModeSet{}, arg);
        IF pc.ntag_chk_range IN n.tags THEN
          gen_check_nil(n.pos, arg, n.l.type);
        END;
        def.add_offs(n.pos,
            tune.DYNARR_LENs_offset+(n.type.len-dim-1)*tune.index_sz*2, arg);
        def.deref(n.pos, tune.index_ty, tune.index_sz, ir.OptionsSet{ir.o_Constant}, arg);
        RETURN;
    | pc.nd_index:
        gen_len(n.l,dim+1,type,arg);
        RETURN;
    | pc.nd_unary, pc.nd_lconv:
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
            gen_len(n.l,dim,type, arg);
          ELSIF (f.mode=pc.ty_array_of) THEN
            gen_size(n.l,SHORT(dim), larg);
            def.gen_sizeof(n.pos, t.base, arg);
            gen_bin_op(n.pos, ir.o_div, larg, arg, tune.index_ty, tune.index_sz, arg);
          ELSE
            i:=def.get_bytes(n.pos,f);
            j:=def.get_bytes(n.pos,t.base);
            def.c_number(i DIV j, arg);
            IF (i MOD j) # 0 THEN
              env.errors.Error(n.pos, 1006, dim);
              ir.MakeArgNum(arg, def.zz_one);
            END;
          END;
          RETURN;
        END;
    | pc.nd_sequence:
        i := sequence_size(n) DIV def.get_bytes(n.pos, n.type.base);
        def.c_number(i, arg);
        RETURN;
    ELSE (* nothing *)
    END;
  END;
  env.errors.Error(n.pos, 1006, dim);
  ir.MakeArgNum(arg, def.zz_one);
END gen_len;

PROCEDURE gen_size_usage*(tpos-: ir.TPOS;
                              o: pc.OBJECT;
                            dim: INTEGER;
                        VAR arg: ir.Arg);
  VAR t: pc.STRUCT; i,j: INTEGER; sz: LONGINT;
    q: ir.TriadePtr; larg: ARRAY 32 OF ir.Arg;
BEGIN
  ASSERT(o.type.mode=pc.ty_array_of);
  t := o.type;
  i := 0; j := 0;
  WHILE i < dim DO ASSERT(t.mode IN pc.ARRs); t := t.base; INC(i) END;
  WHILE t.mode = pc.ty_array_of DO
    def.o_attr(tpos, o, at.a_len+SHORT(i), ir.GenModeSet{}, larg[j]);
    t := t.base;
    INC(i); INC(j);
  END;
  sz := def.type_size(t);
  IF (sz # 1) OR (i = dim) THEN
    def.gen_sizeof(o.pos, t, larg[j]);
    INC(j);
  END;
  IF j = 1 THEN arg := larg[0];
  ELSE
    q := ir.NewTriadeInit(j, ir.o_mul, tune.index_ty, tune.index_sz);
    q.Position := tpos;
    FOR i := 0 TO j - 1 DO ir.ParmByArg(q.Params[i], larg[i]) END;
    ir.GenResVar(q);
    gr.AppendTr(q);
    ir.MakeArgVar(arg, q.Name);
  END;
END gen_size_usage;

PROCEDURE gen_size*(n: pc.NODE; dim: INTEGER; VAR arg: ir.Arg);
  (* dim - количество индексаций *)
  VAR t: pc.STRUCT; i: INTEGER;
    larg: ir.Arg;
    a: at.ATTR_EXT;
BEGIN
  t := n.type;
  FOR i := 0 TO dim-1 DO ASSERT(t.mode IN pc.ARRs); t:=t.base END;
  IF t.mode # pc.ty_array_of THEN def.gen_sizeof(n.pos, t, arg); RETURN END;
  IF dim<n.type.len THEN
    CASE n.mode OF
    | pc.nd_deref:
        IF dim=0 THEN
          gen_len(n, 0, def.SIZE_T, larg);
          gen_size(n, 1, arg);
          gen_bin_op(n.pos, ir.o_mul, larg, arg, ir.t_unsign, 4, arg);
        ELSE
          def.t_def(n.type);
          gen_value(n.l, ir.GenModeSet{}, arg);
          IF pc.ntag_chk_range IN n.tags THEN
            gen_check_nil(n.pos, arg, n.l.type);
          END;
          def.add_offs(n.pos,
            tune.DYNARR_LENs_offset+(n.type.len-dim-1)*tune.index_sz*2+tune.index_sz
           ,arg);
          def.deref(n.pos, ir.t_unsign, tune.index_sz, ir.OptionsSet{ir.o_Constant}, arg);
        END;
        RETURN;
    | pc.nd_index:
        gen_size(n.l, dim+1, arg);
        RETURN;
    | pc.nd_var:
        a := at.attr(n.obj.ext, at.a_len+SHORT(dim));
        IF a # NIL THEN gen_size_usage(n.pos, n.obj, dim, arg); RETURN END;
    ELSE (* nothing *)
    END;
  END;
  env.errors.Error(n.pos, 1007, dim);
  ir.MakeArgNum(arg, def.zz_one);
END gen_size;

PROCEDURE rem_str_extension(VAR n: pc.NODE): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN
  r:=   (n.mode=pc.nd_unary) &  (n.sub=pc.su_conv) &
        (n.type.mode IN pc.ARRs) & (n.l.type.mode IN pc.ARRs) &
        def.is_char(n.type.base) & def.is_char(n.l.type.base);
  IF r THEN n:=n.l END;
  RETURN r;
END rem_str_extension;

PROCEDURE spec_op * (nm: pc.STRING): ir.Operation;
  VAR name: ARRAY 8 OF CHAR;
    op : ir.Operation;
BEGIN
  COPY(nm^, name);
  str.Capitalize(name);
  IF    name = "SIN"    THEN op := ir.o_sin;
  ELSIF name = "COS"    THEN op := ir.o_cos;
  ELSIF name = "TAN"    THEN op := ir.o_tan;
  ELSIF name = "ARCTAN" THEN op := ir.o_atan;
  ELSIF name = "EXP"    THEN op := ir.o_exp;
  ELSIF name = "SQRT"   THEN op := ir.o_sqrt;
  ELSIF name = "LN"     THEN op := ir.o_ln;
  ELSIF name = "LG"     THEN op := ir.o_lg;
  ELSE                     op := ir.o_invalid;
  END;
  RETURN op
END spec_op;


PROCEDURE special_call(n: pc.NODE; md: ir.GenModeSet; VAR arg: ir.Arg);
  VAR q: ir.TriadePtr; op: ir.Operation;
    ty: ir.TypeType; sz: ir.SizeType;
    i: INT;
    tmp: ir.Local;
    l: pc.NODE;
BEGIN
  op := spec_op(n.obj.name);  ASSERT( op # ir.o_invalid );
  INCL(n.obj.marks, at.omark_used);
  def.type_info(n.type, ty, sz);
  q := ir.NewTriadeInit(1, op, ty, sz);
  q.Position := n.pos;
  INCL(q.Options, ir.o_Dangerous);
  i := 0;
  l := n.r;
  WHILE l # NIL DO
    gen_value(l, ir.GenModeSet{}, arg);
    ir.ParmByArg(q.Params[i], arg);
    INC(i);
    l := l.next;
  END;
  IF (def.REF IN md) THEN
    make_temp(n.type, tmp);
    q.Tag := ir.y_RealVar;
    q.Name := tmp;
    ir.MakeArgLocal(arg, tmp, 0);
  ELSE
    ir.GenResVar(q);
    ir.MakeArgVar(arg, q.Name);
  END;
  gr.AppendTr(q);
END special_call;


PROCEDURE gen_halt * (pos-: ir.TPOS; arg-: ir.Arg);
  VAR q : ir.TriadePtr;
BEGIN
  q := ir.NewTriadeInit(1, ir.o_stop, ir.t_void, 0);
  q.Position := pos;
  INCL(q.Options, ir.o_Dangerous);
  ir.ParmByArg(q.Params[0], arg);
  gr.AppendTr(q);
  gr.FinishNode;
END gen_halt;

PROCEDURE chk_no_exit(n: pc.NODE);
  VAR arg: ir.Arg;
BEGIN
  IF pc.ntag_no_exit IN n.tags THEN
    ir.MakeArgNum(arg, def.val(tune.ABORT_EXIT_CODE));
    gen_halt(n.pos, arg)
  END;
END chk_no_exit;


PROCEDURE gen_call*(n: pc.NODE; md: ir.GenModeSet; VAR arg: ir.Arg);
  VAR
    proto:  pr.Proto;
    nst: SHORTINT;
    q : ir.TriadePtr;
    t : pc.STRUCT;
    tmp_nm : ir.Local;
    prm_cnt: INTEGER;
    arg2: ir.Arg;      (* type or im-part *)
    prt_no: pr.ProtoNum;
    RtoL : BOOLEAN;
    this: pc.NODE;
    cc : ir.GenModeSet;

  PROCEDURE count_seq(): INTEGER;
    VAR l,r: pc.NODE; cnt: INTEGER;
  BEGIN (* посчитать число параметров в последовательности *)
    r := n.r;
    WHILE r.next # NIL DO r := r.next END;
    ASSERT(r.mode= pc.nd_sequence);
    l := r.l; cnt := 0;
    WHILE l # NIL DO
(*     надо ли передавать длину массива ??
      IF l.type.mode IN pc.SEQ_ARRs THEN INC(cnt, 2);
      ELSE INC(cnt, 1);
      END;
*)
      INC(cnt, 1);
      l := l.next;
    END;
    RETURN cnt
  END count_seq;

  PROCEDURE gen_seq_el(l: pc.NODE; N: INTEGER);      (* номер в триаде *)
    VAR ty: ir.TypeType; sz: ir.SizeType;
        asgn: ir.TriadePtr;
  BEGIN
    IF l = NIL THEN RETURN END;
    IF RtoL THEN gen_seq_el(l.next, N+1) END;
    IF def.is_scalar(l.type) THEN
      gen_value(l, cc, arg);
      def.type_info(l.type, ty, sz);
    ELSE
      gen_value(l, cc+ir.GenModeSet{def.REF}, arg);
      ty := tune.addr_ty;
      sz := tune.addr_sz;
    END;
    IF sz < tune.seq_item_sz THEN
      gen_conv(l.pos, arg, ty, sz, ty, tune.seq_item_sz);
    -- JET-1253 fix {
    ELSIF (ty # ir.t_float) & (sz = 8) AND
          (arg.tag # ir.y_Variable) & (arg.tag # ir.y_RealVar)
    THEN -- passing longint const to seq arg
--      Printf.printf("\nJET-1253: XDSComp.opE.gen_call.gen_seq_el\n");
      asgn := ir.NewTriadeInit(1, ir.o_assign, ty, sz);
      asgn.Position := l.pos;
      ir.ParmByArg(asgn.Params[0], arg);
      ir.GenResVar(asgn);
      gr.AppendTr(asgn);
      ir.MakeArgVar(arg, asgn.Name);
    END;
    -- } JET-1253 fix
    ir.ParmByArg(q.Params[N], arg);
    IF NOT RtoL THEN gen_seq_el(l.next, N+1) END;
  END gen_seq_el;

  PROCEDURE gen_param(this, l: pc.NODE;    (* выражение - факт.параметр *)
                      i: INTEGER);         (* номер в прототипе *)

    VAR j : INTEGER; ll: pc.NODE;
  BEGIN
    IF i>=proto.npar THEN RETURN END;
    CASE proto.par[i].mode OF
    | pr.pm_return :
        IF RtoL THEN gen_param(this, l, i+1) END;
        ir.MakeParAddr(q.Params[i+1], tmp_nm, 0);
        IF NOT RtoL THEN gen_param(this, l, i+1) END;

    | pr.pm_base:    (* ----- база одной из охват. процедур *)
        IF RtoL THEN gen_param(this, l, i+1) END;
        j := nst + ORD(proto.par[i].ind);
        IF j >= 0 THEN (* место вызова глубже описания *)
          def.o_attr(ir.NullPos, at.curr_proc, at.a_base+SHORT(j), ir.GenModeSet{}, arg);
        ELSE (* вызов в охватывающей процедуре *)
          ASSERT(j = -1);
          def.o_attr(ir.NullPos, at.curr_proc, at.a_mybase, ir.GenModeSet{}, arg);
        END;
        ir.ParmByArg(q.Params[i+1], arg);
        IF NOT RtoL THEN gen_param(this, l, i+1) END;

    | pr.pm_param:
        IF this = NIL THEN ll := l.next ELSE ll := l; l := this END;
        IF RtoL THEN gen_param(NIL, ll, i+1) END;
        j := ORD(proto.par[i].ind);
        IF j = ORD(pr.by_val) THEN
          gen_value(l, cc, arg);
        ELSIF (cc # ir.GenModeSet{}) & (l.mode = pc.nd_unary) & (l.sub = pc.su_conv) &
           (l.type.mode = pc.ty_array_of) & (l.l.type.mode = pc.ty_pointer) &
           (l.type.base = l.l.type.base) & (l.type.base.mode # pc.ty_loc)
        THEN -- pointer to T is equivalent to array of T in C procedures
          gen_value (l.l, ir.GenModeSet{}, arg);
        ELSE
          gen_value(l, cc+ir.GenModeSet{def.REF}, arg);
        END;

        ir.ParmByArg(q.Params[i+1], arg);
        j := 0;
        LOOP
          IF i<(proto.npar-1) THEN INC(i) ELSE RETURN END;
          IF (proto.par[i].mode # pr.pm_len) THEN EXIT END;
          gen_len(l, j, def.SIZE_T, arg);
          ir.ParmByArg(q.Params[i+1], arg);
          INC(j);
        END;
        IF NOT RtoL THEN gen_param(NIL, ll, i) END;

    | pr.pm_len, pr.pm_type:
        ASSERT(RtoL);
        gen_param(this, l, i+1);

    | pr.pm_re:
        ASSERT((this = NIL)&(proto.par[i+1].mode = pr.pm_im));
        ll := l.next;
        IF RtoL THEN gen_param(NIL, ll, i+2) END;
        gen_complex_value(l.pos, l, TRUE, TRUE, arg, arg2);
        ir.ParmByArg(q.Params[i+1], arg);
        ir.ParmByArg(q.Params[i+2], arg2);
        IF NOT RtoL THEN gen_param(NIL, ll, i+2) END;

    | pr.pm_formrec:
        ASSERT(proto.par[i+1].mode = pr.pm_type);
        IF this = NIL THEN ll := l.next ELSE ll := l; l := this END;
        IF RtoL THEN gen_param(NIL, ll, i+2) END;
        gen_value(l, cc+ir.GenModeSet{def.REF}, arg);
        ir.ParmByArg(q.Params[i+1], arg);
        gen_type(l, arg2);
        ir.ParmByArg(q.Params[i+2], arg2);
        IF NOT RtoL THEN gen_param(NIL, ll, i+2) END;

    | pr.pm_seq:
        ASSERT((l.next=NIL) & (l.type.mode=pc.ty_array_of));
        gen_seq_el(l.l, i+1);
        RETURN
    END;
  END gen_param;

--  VAR ty: ir.TypeType; sz: ir.SizeType;
BEGIN
<* IF db_procs THEN *>
  io.print("-------------->gen_call\n");
<* END *>
  ASSERT(NOT (def.LVL IN md));
  IF n.l # NIL THEN t := n.l.type;
  ELSE t := n.obj.type;
    IF pc.ttag_intrinsic IN t.tags THEN
      special_call(n, md, arg);
      RETURN
    END;
  END;

  IF (t.flag IN opt.LangsWithCLikeStrings) THEN
    cc := ir.GenModeSet{def.CC};
  ELSE 
    cc := ir.GenModeSet{};
  END;
  prt_no := def.prototype(t);
  proto  := pr.ProtoList[prt_no];

  --  RtoL := proto.right_to_left;
  IF proto.lang_flag IN opt.LangsWithSpecifiedComputingParamsOrder THEN
    -- if the caller's language specifies the parameters calculating order,
    -- then we use it.
    RtoL := proto.lang_flag IN opt.LangsWithComputingParams_RtoL;
  ELSE
    -- else we synchronize order of calculating with order of passing
    -- this makes code more efficient.
    RtoL := proto.right_to_left;
  END;
  IF (def.REF IN md) OR proto.rtn THEN
    make_temp(t.base, tmp_nm);
  ELSE
    tmp_nm := MAX(ir.VarNum);
  END;

  prm_cnt := proto.npar;
  IF proto.seq THEN INC(prm_cnt, count_seq()-1) END;
  q := ir.NewTriadeInit(prm_cnt+1, ir.o_call, proto.ret_type, proto.ret_size);
  q.Prototype := prt_no;
  q.Position := n.pos;
  IF t.flag IN pc.CallResOptional THEN
    INCL(q.Options, ir.o_Silent);
  END;

  this := NIL;
  nst  := 0;
  IF n.l=NIL THEN
    nst := at.curr_proc.lev - n.obj.lev;
  ELSIF n.l.mode=pc.nd_method THEN
    this:=n.l.l;
  END;

  IF RtoL THEN gen_param(this, n.r, 0) END;
  IF n.l=NIL THEN
    def.o_usage(n.pos, n.obj, ir.GenModeSet{}, arg);
  ELSIF n.l.mode=pc.nd_method THEN
    ir.ArgByParm(arg,q.Params[1+ORD(proto.rtn)]);
    gen_type_ex(n.l.l, arg, TRUE, ir.GenModeSet{}, TRUE, arg);
    def.add_offs(n.pos, tune.PROC_offset, arg);
    def.deref(n.pos, tune.addr_ty, tune.addr_sz, ir.OptionsSet{ir.o_Constant}, arg);
    def.add_offs(n.pos, t.len * tune.proc_sz, arg);
--    def.type_info(n.l.type, ty, sz);
    def.deref(n.pos, tune.proc_ty, tune.proc_sz, ir.OptionsSet{ir.o_Constant}, arg);
  ELSE
    gen_value(n.l, ir.GenModeSet{}, arg);
    IF pc.ntag_chk_range IN n.tags THEN
      gen_check_nil(n.pos, arg, n.l.type)
    END;
  END;
  ir.ParmByArg(q.Params[0], arg);    (* procedure *)
  IF NOT RtoL THEN gen_param(this, n.r, 0) END;
  gr.AppendTr(q);

  IF def.REF IN md THEN
    IF NOT proto.rtn THEN (* а так бывает ?? LAZ*)
      q.Tag  := ir.y_RealVar;
      q.Name := tmp_nm;
    END;
    ir.MakeArgAddr(arg, tmp_nm, 0);
  ELSE
    IF q.ResType # ir.t_void THEN
      ir.GenResVar(q);
      ir.MakeArgVar(arg, q.Name);
    ELSE arg.tag := ir.y_Nothing;
    END;
  END;
<* IF db_procs THEN *>
  io.print("##########################\n");
  io.print("NodeNo = %d; tag=%d; Prototype=%d\n", q.NodeNo, q.Params[0].tag, q.Prototype);
  io.print("##########################\n");
  io.print("<----------------- gen_call\n");
<* END *>
  chk_no_exit(n);  -- to have o_stop after procedure call with no_exit tag
END gen_call;

PROCEDURE gen_call_storage*(n: pc.NODE);
  VAR
    ntype : pc.STRUCT; (* pointer base type *)
    btype : pc.STRUCT; (* dyn. array base type *)
    q: ir.TriadePtr;
    arg : ir.Arg;
    p_no: INTEGER;

  PROCEDURE prm_ptr;
  BEGIN
    gen_value(n.r, ir.GenModeSet{def.REF}, arg);
    ir.ParmByArg(q.Params[p_no], arg);
    INC(p_no);
  END prm_ptr;

  PROCEDURE prm_size(l: pc.NODE);
    VAR size: LONGINT;
  BEGIN
    IF l = NIL THEN
      size := def.type_size(btype);
      def.c_number(size, arg);
                                -- размер элемента с выравниванием от массива
    ELSE gen_value(l, ir.GenModeSet{}, arg);
    END;
    ir.ParmByArg(q.Params[p_no], arg);
    INC(p_no);
  END prm_size;

  PROCEDURE prm_dims();
  BEGIN
    ASSERT(ntype.mode=pc.ty_array_of);
    def.c_number(ntype.len, arg);
    ir.ParmByArg(q.Params[p_no], arg);
    INC(p_no);
  END prm_dims;

  PROCEDURE prm_lens(l: pc.NODE);
    VAR tmp: ir.Local; k: LONGINT;
  BEGIN
    --IF type = NIL THEN type := def.SIZE_T END;
    def.make_temp_var(ir.t_arr, tune.index_sz*ntype.len, tmp);
    k := 0;
    REPEAT
      gen_value(l, ir.GenModeSet{}, arg);
      gen_storer_loc(l.pos, tune.index_ty, tune.index_sz,
                                      tmp, k*tune.index_sz, arg);
      INC(k);
      l := l.next;
    UNTIL l = NIL;
    ASSERT(k = ntype.len);
    ir.MakeParAddr(q.Params[p_no], tmp, 0);
    INC(p_no);
  END prm_lens;

  PROCEDURE prm_type(sys: BOOLEAN);
   <* IF TARGET_386 THEN *>
    PROCEDURE check_indirect_var (VAR arg: ir.Arg);
    BEGIN
      IF (at.TARGET = at.trg_NT) & (at.USEDLL IN at.COMP_MODE) THEN 
        def.deref(n.pos, tune.addr_ty, tune.addr_sz, ir.OptionsSet{ir.o_Constant}, arg);
      END;
    END check_indirect_var;
   <* END *>
    VAR b: pc.STRUCT;
  BEGIN
    b := btype;
    WHILE b.mode IN pc.ARRs DO b := b.base END;
    IF NOT (b.flag IN opt.LangsWithGarbageCollector) OR sys THEN
      def.o_usage(ir.NullPos, std.x2c_td_null, ir.GenModeSet{}, arg);
     <* IF TARGET_386 THEN *>
      check_indirect_var (arg);
     <* END *>
    ELSIF b.mode = pc.ty_pointer THEN
      def.o_usage(ir.NullPos, std.x2c_td_ptr, ir.GenModeSet{}, arg);
     <* IF TARGET_386 THEN *> 
      check_indirect_var (arg); 
     <* END *>
    ELSE gen_type_name(b, arg);
    END;
    ir.ParmByArg(q.Params[p_no], arg);
    INC(p_no);
  END prm_type;

  PROCEDURE prm_flag(sys : BOOLEAN);
    VAR val : pc.VALUE;
  BEGIN
    IF sys THEN 
      val := def.bool1;
    ELSE 
      val := def.bool0;
    END;
    ir.MakeParNum(q.Params[p_no], val);
    INC(p_no);
  END prm_flag;

  VAR
    l      : pc.NODE;
    i,j,k  : INTEGER;
    prt_no : pr.ProtoNum;  proto: pr.Proto;  prms: pr.params;
    prm_cnt: INTEGER;
    o2,new,sys: BOOLEAN;
    lt     : pc.STRUCT;
BEGIN
  l := n.r;
  lt := l.type;
  IF lt.mode=pc.ty_opaque THEN lt:=lt.base END;
  ntype := lt.base;
  btype := ntype;
  WHILE btype.mode = pc.ty_array_of DO btype := btype.base END;
  ASSERT(lt.mode = pc.ty_pointer);
  o2 := (lt.flag=pc.flag_o2);
  new := (n.sub=pc.sp_new) OR (n.sub=pc.sp_sysnew);
  sys := (n.sub=pc.sp_sysnew);
(*------------- *)
  p_no := 1;
  IF n.obj = NIL THEN
    IF ntype.mode = pc.ty_array_of THEN
      IF new THEN
        IF o2 THEN
          q := def.RTS_call(n.pos, std.X2C_NEW_OPEN, TRUE);
          prm_type(FALSE);  -- always true type, not x2c_td_null
          prm_ptr;
          prm_size(NIL);
          prm_lens(n.r.next);
          prm_dims();
          prm_flag(sys);
        ELSIF lt.flag=pc.flag_m2 THEN
          q := def.RTS_call(n.pos, std.X2C_DYNALLOCATE, TRUE);
          prm_ptr;
          prm_size(NIL);
          prm_lens(n.r.next);
          prm_dims();
        ELSE -- (lt.flag=pc.flag_c) OR (lt.flag=pc.flag_syscall) OR (lt.flag=pc.flag_stdcall) THEN
          q := def.RTS_call(n.pos, std.X2C_DYNCALLOCATE, TRUE);
          prm_ptr;
          prm_size(NIL);
          prm_lens(n.r.next);
          prm_dims();
        END;
      ELSE
        IF o2 THEN
          q := def.RTS_call(n.pos, std.X2C_DISPOSE, TRUE);
        ELSIF lt.flag=pc.flag_m2 THEN
          q := def.RTS_call(n.pos, std.X2C_DYNDEALLOCATE, TRUE);
        ELSE
          q := def.RTS_call(n.pos, std.X2C_DYNCDEALLOCATE, TRUE);
        END;
        prm_ptr;
      END;
    ELSE
      IF new THEN
        IF o2 THEN
          q := def.RTS_call(n.pos, std.X2C_NEW, TRUE);
          prm_type(sys & (n.r.next#NIL));
          prm_ptr;
          prm_size(n.r.next);
          prm_flag(sys);
        ELSE
          q := def.RTS_call(n.pos, std.X2C_ALLOCATE, TRUE);
          prm_ptr;
          prm_size(n.r.next);
        END;
      ELSE
        IF o2 THEN
          q := def.RTS_call(n.pos, std.X2C_DISPOSE, TRUE);
        ELSE
          q := def.RTS_call(n.pos, std.X2C_DEALLOCATE, TRUE);
        END;
        prm_ptr;
      END;
    END;
  ELSE
    prt_no := def.prototype(n.obj.type);
    proto  := pr.ProtoList[prt_no];
    prm_cnt := proto.npar;
    prms := proto.par;
    q := ir.NewTriadeInit(prm_cnt+1, ir.o_call, proto.ret_type, proto.ret_size);
    q.Position := n.pos;
    q.Prototype := prt_no;
    def.o_usage(n.pos, n.obj, ir.GenModeSet{}, arg);
    ir.ParmByArg(q.Params[0], arg);    (* procedure *)
    j := 0;
    FOR i:=0 TO prm_cnt-1 DO
      CASE prms[i].mode OF
      |pr.pm_base:
        k := at.curr_proc.lev - n.obj.lev + ORD(prms[i].ind);
        IF k >= 0 THEN (* место вызова глубже описания *)
          def.o_attr(ir.NullPos, at.curr_proc, at.a_base+SHORT(k), ir.GenModeSet{}, arg);
        ELSE (* вызов в охватывающей процедуре *)
          ASSERT(k = -1);
          def.o_attr(ir.NullPos, at.curr_proc, at.a_mybase, ir.GenModeSet{}, arg);
        END;
        ir.ParmByArg(q.Params[p_no], arg);
        INC(p_no);

      |pr.pm_param  :
        CASE j OF
          |0: prm_ptr;
          |1: prm_size(NIL); l:=l.next;
          |2: IF new THEN prm_lens(l)
              ELSE prm_dims()  -- а это зачем? (при удалении - не нужно, может быть - для будущей реаллокации?)
              END;
        END;
        INC(j);
      |pr.pm_len   :
        ASSERT(j=3);
        prm_dims();
      END;
    END;
  END;
  gr.AppendTr(q);
(* ?? а как на счет resize ?? см opCode.gen_sproc *)
  chk_no_exit(n);  -- to have o_stop after procedure call with no_exit tag
END gen_call_storage;

PROCEDURE gen_checkS(op: ir.Operation;
                     chk: ir.OptionsSet;
                     larg-, rarg-: ir.Arg;
                     t: pc.STRUCT;
                     tpos: ir.TPOS);
  VAR ty: ir.TypeType; sz: ir.SizeType;
BEGIN
  def.type_info(t, ty, sz);
  gen_check(op, chk, larg, rarg, ty, sz, tpos);
END gen_checkS;

PROCEDURE gen_value_minus_min(n: pc.NODE;
                v  : pc.VALUE;
                chk: ir.OptionsSet;
                VAR arg: ir.Arg);
  VAR rarg: ir.Arg;
    type: pc.STRUCT;
    ty: ir.TypeType; sz: ir.SizeType;
BEGIN
  WHILE (n.mode=pc.nd_unary) & (n.sub=pc.su_conv) & n.l.type.is_ordinal() DO
    n:=n.l;
  END;
  type := n.type;
  ASSERT(type.is_ordinal());
  def.index_type_info(type, ty, sz);
  WHILE (n.mode=pc.nd_unary) & (n.sub=pc.su_conv) & n.l.type.is_ordinal() DO
    n:=n.l;
  END;
  gen_value(n,ir.GenModeSet{},arg);
  ir.MakeArgNum(rarg, v);
  IF (chk # ir.OptionsSet{}) & ir.GT(v, type.min, ty, sz, TRUE) THEN
    gen_check(ir.o_checklo, chk, arg, rarg, ty, sz, n.pos);
  END;
  IF NOT v.is_zero() THEN
    ir.MakeArgNum(rarg, v);
    gen_bin_op(n.pos, SYSTEM.VAL(ir.Operation,-VAL(SHORTINT,ir.o_add)), arg, rarg, ty, sz, arg);
  END;
END gen_value_minus_min;

PROCEDURE gen_inc_dec(n: pc.NODE; VAR arg: ir.Arg);
  VAR q: ir.TriadePtr;
    ty: ir.TypeType; sz: ir.SizeType;
    larg, rarg, tmparg: ir.Arg;
    st: pc.STRUCT;
    chk: BOOLEAN;
    pred, dec : BOOLEAN;
BEGIN
  chk := pc.ntag_chk_range IN n.tags;
  CASE n.sub OF
  | pc.sb_pre_inc  : pred := TRUE;  dec := FALSE;
  | pc.sb_pre_dec  : pred := TRUE;  dec := TRUE;
  | pc.sb_post_inc : pred := FALSE; dec := FALSE;
  | pc.sb_post_dec : pred := FALSE; dec := TRUE;
  END;
  def.type_info(n.type, ty, sz);
  q := ir.NewTriadeInit(2, ir.o_add, ty, sz);
  q.Position := n.pos;
  IF chk THEN INCL(q.Options, ir.o_Checked) END;
  gen_value(n.l, ir.GenModeSet{def.LVL}, larg);
  tmparg := larg;
  IF def.REF IN larg.mode THEN
    def.deref(n.pos, ty, sz, ir.OptionsSet{}, tmparg);
  END;
  IF pred THEN arg := tmparg END;
  ir.ParmByArg(q.Params[0], tmparg);
  gen_value(n.r, ir.GenModeSet{}, tmparg);
  ir.ParmByArg(q.Params[1], tmparg);
  ir.SetParamReverse(q.Params[1], dec);
  gr.AppendTr(q);
  chk := chk & (n.type.mode IN RANGEs);
  IF chk OR (def.REF IN larg.mode) THEN
    ir.GenResVar(q);
    ir.MakeArgVar(tmparg, q.Name);
    IF chk THEN
      st := n.type.super_type();
      IF NOT ir.EQ(n.type.min, st.min, ty, sz, TRUE) THEN
        ir.MakeArgNum(rarg, n.type.min);
        gen_check(ir.o_checklo, ir.OptionsSet{ir.o_Range}, tmparg, rarg, ty, sz, n.pos);
      END;
      IF NOT ir.EQ(n.type.max, st.max, ty, sz, TRUE) THEN
        ir.MakeArgNum(rarg, n.type.max);
        gen_check(ir.o_checklo, ir.OptionsSet{ir.o_Range}, rarg, tmparg, ty, sz, n.pos);
      END;
    END;
    IF def.REF IN larg.mode THEN
      gen_storer(n.pos, ty, sz, larg, tmparg);
    ELSE
      q := ir.NewTriadeTInit(1, ir.o_assign, larg.tag, ty, sz);
      q.Position := n.pos;
      q.Name := larg.name;
      ir.ParmByArg(q.Params[0], tmparg);
      gr.AppendTr(q);
    END;
    IF NOT pred THEN arg := tmparg END;
  ELSE
    q.Tag  := larg.tag;
    q.Name := larg.name;
    IF NOT pred THEN arg := larg END;
  END;
END gen_inc_dec;


PROCEDURE gen_float_bin(tpos: ir.TPOS;
                          op: ir.Operation;
                 larg-,rarg-: ir.Arg;
                      res_sz: ir.SizeType): ir.TriadePtr;
  VAR q: ir.TriadePtr;
BEGIN
  at.was_float_triade := TRUE;
  q := ir.NewTriadeInit(2, op, ir.t_float, res_sz);
  q.Position := tpos;
  INCL(q.Options, ir.o_Dangerous);
  INCL(q.Options, ir.o_Checked);
  ir.ParmByArg(q.Params[0], larg);
  ir.ParmByArg(q.Params[1], rarg);
  ir.GenResVar(q);
  gr.AppendTr(q);
  RETURN q
END gen_float_bin;

PROCEDURE complex_expr_err(n: pc.NODE; VAR re,im: ir.Arg);
BEGIN
  env.errors.Error(n.pos, 1008);
  ir.MakeArgNum(re, def.zz_one);
  ir.MakeArgNum(im, def.zz_one);
END complex_expr_err;

PROCEDURE gen_complex_binary(tpos: ir.TPOS;
                             n   : pc.NODE;
                       VAR re,im : ir.Arg);
  VAR  q: ir.TriadePtr;
    lre, lim, rre, rim, t1, t2, t3: ir.Arg;
    RE_offs, IM_offs: LONGINT;   (* могут зависеть от архитектуры *)
    sz: ir.SizeType;
    loc : ir.Local;
BEGIN
<* IF db_e THEN *>
  io.print_pos(n.pos);
  io.print("gen_complex_binary(n.sub=%d)\n", n.sub);
<* END *>
  sz := def.re_im_sz(n.type);
  IF n.sub = pc.sb_cmplx THEN
    gen_value(n.l, ir.GenModeSet{}, re);
    gen_value(n.r, ir.GenModeSet{}, im);
  ELSIF n.sub = pc.sb_exp THEN
    at.was_float_triade := TRUE;
    IF n.type.mode = pc.ty_complex THEN
      sz := tune.real_sz;
      IF n.r.type.mode IN pc.WHOLEs THEN
        q := def.RTS_call(n.pos, std.X2C_EXPCI, TRUE);
      ELSE
        ASSERT(n.r.type.mode IN pc.REALs);
        q := def.RTS_call(n.pos, std.X2C_EXPCR, TRUE);
      END;
    ELSE
      ASSERT(n.type.mode IN pc.TY_SET{pc.ty_lcomplex,pc.ty_CC});
      sz := tune.longreal_sz;
      IF n.r.type.mode IN pc.WHOLEs THEN
        q := def.RTS_call(n.pos, std.X2C_EXPLI, TRUE);
      ELSE
        ASSERT(n.r.type.mode IN pc.REALs);
        q := def.RTS_call(n.pos, std.X2C_EXPLR, TRUE);
      END;
    END;
    def.make_temp_var(ir.t_rec, 2*sz, loc);
    ir.MakeParAddr(q.Params[1], loc, 0);
    gen_complex_value(n.pos, n.l, TRUE, TRUE, lre, lim);
    gen_value(n.r, ir.GenModeSet{}, rre);
    ir.ParmByArg(q.Params[2], lre);
    ir.ParmByArg(q.Params[3], lim);
    ir.ParmByArg(q.Params[4], rre);
    gr.AppendTr(q);
    def.re_im_offs(n.type, RE_offs, IM_offs);
    ir.MakeArgAddr(re, loc, RE_offs);         -- loc is a temporary variable !!!
    def.deref(tpos, ir.t_float, sz, ir.OptionsSet{}, re);
    ir.MakeArgAddr(im, loc, IM_offs);
    def.deref(tpos, ir.t_float, sz, ir.OptionsSet{}, im);
  ELSIF (n.sub = pc.sb_pre_inc) OR (n.sub = pc.sb_pre_dec) THEN
    def.re_im_offs(n.type, RE_offs, IM_offs);
    gen_value(n.l, ir.GenModeSet{def.REF}, t1);
    lre := t1;
    def.add_offs(tpos, RE_offs, lre);
    def.deref(tpos, ir.t_float, sz, ir.OptionsSet{}, lre);
    lim := t1;
    def.add_offs(tpos, IM_offs, lim);
    def.deref(tpos, ir.t_float, sz, ir.OptionsSet{}, lim);
    gen_complex_value(n.pos, n.r, TRUE, TRUE, rre, rim);
    q := gen_float_bin(n.pos, ir.o_add, lre, rre, sz);
    ir.MakeArgVar(re, q.Name);
    IF n.sub = pc.sb_pre_dec THEN
      ir.SetParamReverse(q.Params[1], TRUE);
    END;
    lre := t1;
    def.add_offs(n.pos, RE_offs, lre);
    gen_storer(n.pos, ir.t_float, sz, lre, re);
    q := gen_float_bin(n.pos, ir.o_add, lim, rim, sz);
    ir.MakeArgVar(im, q.Name);
    IF n.sub = pc.sb_pre_dec THEN
      ir.SetParamReverse(q.Params[1], TRUE);
    END;
    lim := t1;
    def.add_offs(n.pos, IM_offs, lim);
    gen_storer(n.pos, ir.t_float, sz, lim, im);
  ELSE
    gen_complex_value(tpos, n.l, TRUE, TRUE, lre, lim);
    gen_complex_value(n.pos, n.r, TRUE, TRUE, rre, rim);
    CASE n.sub OF
    | pc.sb_mul:
        q := gen_float_bin(n.pos, ir.o_mul, lre, rre, sz);
        ir.MakeArgVar(t1, q.Name);
        q := gen_float_bin(n.pos, ir.o_mul, lim, rim, sz);
        ir.MakeArgVar(t2, q.Name);
        q := gen_float_bin(n.pos, ir.o_add, t1, t2, sz);
        ir.SetParamReverse(q.Params[1], TRUE);
        ir.MakeArgVar(re, q.Name); -- re.parpos :=  n.pos;  re.operpos :=  tpos;

        q := gen_float_bin(n.pos, ir.o_mul, lre, rim, sz);
        ir.MakeArgVar(t1, q.Name);
        q := gen_float_bin(n.pos, ir.o_mul, lim, rre, sz);
        ir.MakeArgVar(t2, q.Name);
        q := gen_float_bin(n.pos, ir.o_add, t1, t2, sz);
        ir.MakeArgVar(im, q.Name); -- im.parpos :=  n.pos;  im.operpos :=  tpos;

    | pc.sb_slash:
        q := gen_float_bin(n.pos, ir.o_mul, rre, rre, sz);
        ir.MakeArgVar(t1, q.Name);
        q := gen_float_bin(n.pos, ir.o_mul, rim, rim, sz);
        ir.MakeArgVar(t2, q.Name);
        q := gen_float_bin(n.pos, ir.o_add, t1, t2, sz);
        ir.MakeArgVar(t3, q.Name);

        q := gen_float_bin(n.pos, ir.o_mul, lre, rre, sz);
        ir.MakeArgVar(t1, q.Name);
        q := gen_float_bin(n.pos, ir.o_mul, lim, rim, sz);
        ir.MakeArgVar(t2, q.Name);
        q := gen_float_bin(n.pos, ir.o_add, t1, t2, sz);
        ir.MakeArgVar(re, q.Name);

        q := gen_float_bin(n.pos, ir.o_dvd, re, t3, sz);
        ir.MakeArgVar(re, q.Name); -- re.parpos :=  n.pos;  re.operpos :=  tpos;

        q := gen_float_bin(n.pos, ir.o_mul, lim, rre, sz);
        ir.MakeArgVar(t1, q.Name);
        q := gen_float_bin(n.pos, ir.o_mul, lre, rim, sz);
        ir.MakeArgVar(t2, q.Name);
        q := gen_float_bin(n.pos, ir.o_add, t1, t2, sz);
        ir.SetParamReverse(q.Params[1], TRUE);
        ir.MakeArgVar(im, q.Name);

        q := gen_float_bin(n.pos, ir.o_dvd, im, t3, sz);
        ir.MakeArgVar(im, q.Name); -- im.parpos :=  n.pos;  im.operpos :=  tpos;

    | pc.sb_plus:
        q := gen_float_bin(n.pos, ir.o_add, lre, rre, sz);
        ir.MakeArgVar(re, q.Name); -- re.parpos :=  n.pos;  re.operpos :=  tpos;

        q := gen_float_bin(n.pos, ir.o_add, lim, rim, sz);
        ir.MakeArgVar(im, q.Name); -- im.parpos :=  n.pos;  im.operpos :=  tpos;

    | pc.sb_minus:
        q := gen_float_bin(n.pos, ir.o_add, lre, rre, sz);
        ir.SetParamReverse(q.Params[1], TRUE);
        ir.MakeArgVar(re, q.Name); -- re.parpos :=  n.pos;  re.operpos :=  tpos;

        q := gen_float_bin(n.pos, ir.o_add, lim, rim, sz);
        ir.SetParamReverse(q.Params[1], TRUE);
        ir.MakeArgVar(im, q.Name); -- im.parpos :=  n.pos;  im.operpos :=  tpos;

    ELSE complex_expr_err(n, re, im);
    END;
  END;
END gen_complex_binary;

PROCEDURE gen_complex_unary(n: pc.NODE; VAR re,im: ir.Arg);
  VAR arg: ir.Arg;
    sz, from_sz: ir.SizeType;             (* размер соответствующего float *)
    RE_offs, IM_offs: LONGINT;            (* могут зависеть от архитектуры *)
    from, type : pc.STRUCT;
    from_ty: ir.TypeType;
BEGIN
  type := n.type;
  sz := def.re_im_sz(type);
  def.re_im_offs(type, RE_offs, IM_offs);
  CASE n.sub OF
  | pc.su_neg:
     at.was_float_triade := TRUE;
     gen_complex_value(n.pos, n.l, TRUE, TRUE, re, im);
     gen_neg(n.pos, ir.t_float, sz, FALSE, re);
     gen_neg(n.pos, ir.t_float, sz, FALSE, im);
  | pc.su_conv:
     from := n.l.type;
     IF from.mode IN pc.CPLXs THEN
       from_sz := def.re_im_sz(from);
       gen_complex_value(n.pos, n.l, TRUE, TRUE, re, im);
       gen_conv(n.pos, re, ir.t_float, from_sz, ir.t_float, sz);
       gen_conv(n.pos, im, ir.t_float, from_sz, ir.t_float, sz);
     ELSIF from.mode IN pc.NUMs THEN
       def.type_info(from, from_ty, from_sz);
       gen_value(n.l, ir.GenModeSet{}, re);
       IF (from_ty # ir.t_float) OR (from_sz#sz) THEN
         gen_conv(n.pos, re, from_ty, from_sz, ir.t_float, sz);
       END;
       ir.MakeArgFloat(im, def.rr_zero);
     ELSE complex_expr_err(n, re, im);
     END;
  | pc.su_cast:
     gen_value(n, ir.GenModeSet{def.REF}, arg);
     re := arg;
     def.add_offs(n.pos, RE_offs, re);
     def.deref(n.pos, ir.t_float, sz, ir.OptionsSet{}, re);
     im := arg;
     def.add_offs(n.pos, IM_offs, im);
     def.deref(n.pos, ir.t_float, sz, ir.OptionsSet{}, im);
  ELSE complex_expr_err(n, re, im);
  END;
END gen_complex_unary;

PROCEDURE store_complex*(tpos : ir.TPOS;
                         arg- : ir.Arg;        (* адрес получателя *)
                      re, im- : ir.Arg;        (* составные части значения *)
                         type : pc.STRUCT);
  VAR sz: ir.SizeType;
    RE_offs, IM_offs: LONGINT;
    larg: ir.Arg;
BEGIN
  at.was_float_triade := TRUE;
  sz := def.re_im_sz(type);
  def.re_im_offs(type, RE_offs, IM_offs);
  larg := arg;
  def.add_offs(tpos, RE_offs, larg);
  gen_storer(tpos, ir.t_float, sz, larg, re);
  larg := arg;
  def.add_offs(tpos, IM_offs, larg);
  gen_storer(tpos, ir.t_float, sz, larg, im);
END store_complex;

(*
PROCEDURE not_ref (o: pc.OBJECT) : BOOLEAN;
BEGIN
  RETURN (o.lev = 0) OR
      ((o.lev > at.curr_proc.lev) & (o.mode # pc.ob_varpar))
END not_ref;
*)

PROCEDURE not_ref (o: pc.OBJECT) : BOOLEAN;
BEGIN
  RETURN ((o.lev > at.curr_proc.lev)            (* value parameters only *)
        & (o.mode = pc.ob_var)
        & (pc.otag_valpar IN o.tags))
END not_ref;

PROCEDURE gen_complex_value*(tpos: ir.TPOS;
                             n:    pc.NODE;
                           gen_re,
                           gen_im: BOOLEAN;
                        VAR re,im: ir.Arg);
  VAR arg: ir.Arg;
    RE_offs, IM_offs: LONGINT;   (* могут зависеть от архитектуры *)
    sz: ir.SizeType;             (* размер соответствующего float *)
    type: pc.STRUCT;
    v : pc.VALUE;
    o: pc.OBJECT;
BEGIN
<* IF db_e THEN *>
  io.print_pos(n.pos);
  io.print("gen_complex_value(n.mode=%d)\n", n.mode);
<* END *>
  CASE n.mode OF
  | pc.nd_var, pc.nd_field, pc.nd_index, pc.nd_deref, pc.nd_call:
    IF n.mode = pc.nd_var THEN
      o := n.obj;
      IF not_ref (o) THEN
        IF gen_re THEN def.o_attr (n.pos, o, at.a_re, ir.GenModeSet{}, re); END;
        IF gen_im THEN def.o_attr (n.pos, o, at.a_im, ir.GenModeSet{}, im); END;
        RETURN
      END;
    END;
    sz := def.re_im_sz(n.type);
    def.re_im_offs(n.type, RE_offs, IM_offs);
    gen_value(n, ir.GenModeSet{def.REF}, arg);
    IF gen_re THEN
      re := arg;
      def.add_offs(tpos, RE_offs, re); def.deref(tpos, ir.t_float, sz, ir.OptionsSet{}, re);
    END;
    IF gen_im THEN
      im := arg;
      def.add_offs(tpos, IM_offs, im); def.deref(tpos, ir.t_float, sz, ir.OptionsSet{}, im);
    END;
  | pc.nd_binary:
     gen_complex_binary(tpos, n, re, im);
  | pc.nd_unary:
     gen_complex_unary(n, re, im);
  | pc.nd_value:
     IF gen_re THEN
       v := pc.value.new(tpos, pc.RR_type);
       v.unary(pc.su_re, n.val);
       ir.MakeArgFloat(re, v);
     END;
     IF gen_im THEN
       v := pc.value.new(tpos, pc.RR_type);
       v.unary(pc.su_im, n.val);
       ir.MakeArgFloat(im, v);
     END;
  | pc.nd_assign:
     -- we do generate both re & im here
     gen_complex_value(n.pos, n.r, TRUE, TRUE, re, im);
     IF n.obj # NIL THEN
       o := n.obj;
       type := n.obj.type;
       IF not_ref(o) THEN
         sz := def.re_im_sz(type);
         def.o_attr(n.pos, o, at.a_im, ir.GenModeSet{def.REF}, arg);
         gen_storer(n.pos, ir.t_float, sz, arg, im);
         def.o_attr(n.pos, o, at.a_re, ir.GenModeSet{def.REF}, arg);
         gen_storer(n.pos, ir.t_float, sz, arg, re);
       ELSE
         def.o_usage(n.pos, n.obj, ir.GenModeSet{def.REF}, arg);
         store_complex(n.pos, arg, re, im, type);
       END;
     ELSE
       type := n.l.type;
       gen_value(n.l, ir.GenModeSet{def.REF}, arg);
       store_complex(n.pos, arg, re, im, type);
     END;
  | pc.nd_replace:
     gen_value(n.l, ir.GenModeSet{}, arg);
     gen_complex_value(n.pos, n.r, gen_re, gen_im, re, im);
  ELSE complex_expr_err(n, re, im);
  END;
END gen_complex_value;

(* note that for all o_shift triades the first argument
   has type/size resType/resSize, and the second argument
   has type/size opType/opSize.
   So, the type of first argument is type of result,
   and type of first argument is NOT opType.
*)
PROCEDURE gen_shift(n: pc.NODE; VAR arg: ir.Arg);
 VAR rarg, tmp_arg: ir.Arg;
   q: ir.TriadePtr;
   op: ir.Operation;
   then_node, else_node, end_node: ir.Node;
   ty, rty: ir.TypeType;
   sz, rsz: ir.SizeType;
   res, res1: ir.VarNum;
   res_l: ir.Local;
   nr: pc.NODE; v: pc.VALUE;
   len: INT;
   op_left, op_right: ir.Operation;
   cnst, rarg_not_needed : BOOLEAN;
BEGIN
  CASE n.sub OF
  | pc.sb_ash:
      op_left := ir.o_shl;
      op_right := ir.o_sar;
  | pc.sb_rot:
      op_left := ir.o_rol;
      op_right := ir.o_ror;
  | pc.sb_lsh:
      op_left := ir.o_shl;
      op_right := ir.o_shr;
  | pc.sb_shl:
      op_left := ir.o_shl;
      op_right := ir.o_shl;
  | pc.sb_shr:
      op_left := ir.o_shr;
      op_right := ir.o_shr;
  END;
  CASE n.type.mode OF
  | pc.ty_set:
    len := n.type.len;
  | pc.ty_ZZ:
    len := 32;
  ELSE
    len := 8 * def.type_size(n.type);
  END;
  nr := n.r;

  WHILE (nr.mode = pc.nd_unary) & (nr.sub = pc.su_conv) &
        nr.l.type.is_ordinal()
  DO
    nr := nr.l;
  END;

  def.index_type_info(nr.type, rty, rsz);

<* IF TARGET_VAX THEN *>
  gen_value(nr, ir.GenModeSet{}, rarg);
  IF (rty # tune.index_ty) OR (rsz # tune.index_sz) THEN
    gen_conv(n.pos, rarg, rty, rsz, tune.index_ty, tune.index_sz);
  END;

  IF (len = 32) OR ((n.sub # pc.sb_rot) & ((len = 8) OR (len = 16)) ) THEN
    def.type_info(n.l.type, ty, sz);
    gen_value(n.l, ir.GenModeSet{}, arg);
    IF n.r.mode = pc.nd_value THEN
      v := n.r.val;
      IF v.is_zero() THEN RETURN END;
      v := Calc.Binary(pc.sb_rem, rty, rsz, v, def.val(sz*8));
    END;
    res := gen_bin_spec(n.pos, op_left, arg, ty, sz, rarg, rty, rsz);
    ir.MakeArgVar(arg, res);
    RETURN

<* ELSE *>

  cnst := (n.r.mode = pc.nd_value);
  rarg_not_needed :=  ( (len = 8) OR (len = 16) OR (len = 32) ) AND
                      cnst; 

  IF ~rarg_not_needed THEN
    gen_value(nr, ir.GenModeSet{}, rarg);
    IF (rty # tune.index_ty) OR (rsz # tune.index_sz) THEN
      gen_conv(n.pos, rarg, rty, rsz, tune.index_ty, tune.index_sz);
    END;
  END;
  rty := tune.index_ty;
  rsz := tune.index_sz;

  IF (len = 8) OR (len = 16) OR (len = 32) THEN
    IF (n.l.mode = pc.nd_value) & (n.l.type.mode = pc.ty_ZZ) THEN
      sz := 4;
      IF n.l.val.is_neg() THEN
        ty := ir.t_int;
      ELSE
        ty := ir.t_unsign;
      END;
    ELSE
      def.type_info(n.l.type, ty, sz);
    END;
    gen_value(n.l, ir.GenModeSet{}, arg);
    IF cnst OR (rty = ir.t_unsign) OR (n.sub =pc.sb_shl) OR (n.sub =pc.sb_shr)
    THEN (* staticaly known direction *)
      IF NOT cnst THEN
        op := op_left;
      ELSE v := n.r.val;
        IF v.is_zero() THEN RETURN END;
        IF NOT (env.ts_ext IN env.config.tags) THEN
          v := Calc.Binary(pc.sb_rem, rty, rsz, v, def.val(sz*8));
        END;
        IF v.is_neg() THEN
          ir.MakeArgNum(rarg, Calc.Unary(pc.su_neg, rty, rsz, v));
          op := op_right;
        ELSE
          ir.MakeArgNum(rarg, v);
          op := op_left;
        END;
      END;
      IF (n.sub =pc.sb_shl) OR (n.sub =pc.sb_shr) OR (n.sub =pc.sb_sar) THEN
        ir.MakeArgNum( tmp_arg, def.zz_zero );
        IF cnst THEN
          gen_check(ir.o_checklo, ir.OptionsSet{ ir.o_Range }, rarg, tmp_arg,
                    rty, rsz, n.pos);
        ELSE
          gen_check(ir.o_checklo, ir.OptionsSet{ ir.o_Range }, rarg, tmp_arg,
                    tune.index_ty, tune.index_sz, n.pos);
        END;
      END;
      res := gen_bin_spec(n.pos, op, arg, ty, sz, rarg, rty, rsz);
      ir.MakeArgVar(arg, res);
      RETURN
    ELSE
      (* there we cannot have sb_shl or sb_shr *)
      (* so we must handle both directions     *)
      ir.SetSilentMode();
      q := ir.NewTriadeInit(2, ir.o_le, rty, rsz);
      q.Position := n.pos;
      ir.MakeParNum(q.Params[0], def.zz_zero);
      ir.ParmByArg(q.Params[1], rarg);
      gr.AppendTr(q);
      then_node := gr.NewNode();
      gr.NewArc(gr.currNode, then_node, FALSE);
      else_node := gr.NewNode();
      gr.NewArc(gr.currNode, else_node, FALSE);
      end_node := gr.NewNode();

      gr.StartNode(then_node);
      res := gen_bin_spec(n.pos, op_left, arg, ty, sz, rarg, rty, rsz);
      gr.Goto(end_node);

      gr.StartNode(else_node);
      gen_neg(n.pos, rty, rsz, FALSE, rarg);
      res1 := gen_bin_spec(n.pos, op_right, arg, ty, sz, rarg, rty, rsz);
      gr.Goto(end_node);

      gr.StartNode(end_node);
      q := ir.NewTriadeInit(2, ir.o_fi, ty, sz);
      q.Position := n.pos;
      ir.MakeParVar(q.Params[0], res);
      ir.MakeParVar(q.Params[1], res1);
      ir.GenResVar(q);
      gr.AppendTr(q);
      ir.MakeArgVar(arg, q.Name);
      ir.SetNormalMode();
    END;
<* END *>

  -- ELSIF (n.l.type.mode = pc.ty_set) & def.is_scalar(n.l.type) THEN
  -- There is scalar that must be passed by Addr (lion)
  ELSIF (n.l.type.mode = pc.ty_set) & (len < tune.BITSET_LEN) THEN
    IF (n.sub =pc.sb_shl) OR (n.sub =pc.sb_shr) OR (n.sub =pc.sb_sar) THEN
        ir.MakeArgNum( tmp_arg, def.zz_zero );
        gen_check(ir.o_checklo, ir.OptionsSet{ ir.o_Range }, rarg, tmp_arg,
                  tune.index_ty, tune.index_sz, n.pos);
    END;
    def.type_info(n.l.type, ty, sz);
    gen_value(n.l, ir.GenModeSet{}, arg);
    gen_conv (n.pos, arg, ty, sz, ir.t_unsign, tune.BITSET_LEN DIV 8);
    IF op_right = ir.o_ror THEN q := def.RTS_call(n.pos, std.X2C_ROT, TRUE);
    ELSE                        q := def.RTS_call(n.pos, std.X2C_LSH, TRUE);
    END;
    ir.ParmByArg(q.Params[1], arg);
    ir.MakeParNum(q.Params[2], def.val(len));
    ir.ParmByArg(q.Params[3], rarg);
    gr.AppendTr(q);
    ir.MakeArgVar(arg, q.Name);

    IF sz # tune.BITSET_LEN DIV 8 THEN
      q.ResSize := sz;
      q.OpSize := sz;
--      gen_conv(n.pos, arg, ty, sz, ir.t_unsign, tune.BITSET_LEN DIV 8);
    END;

  ELSE
    IF (n.sub =pc.sb_shl) OR (n.sub =pc.sb_shr) OR (n.sub =pc.sb_sar) THEN
        ir.MakeArgNum( tmp_arg, def.zz_zero );
        gen_check(ir.o_checklo, ir.OptionsSet{ ir.o_Range }, rarg, tmp_arg,
                  tune.index_ty, tune.index_sz, n.pos);
    END;
    gen_value(n.l, ir.GenModeSet{def.REF}, arg);
    IF op_right = ir.o_ror THEN q := def.RTS_call(n.pos, std.X2C_ROTL, TRUE);
    ELSE                        q := def.RTS_call(n.pos, std.X2C_LSHL, TRUE);
    END;
    make_temp(n.l.type, res_l);
    ir.MakeParAddr(q.Params[1], res_l, 0);
    ir.ParmByArg(q.Params[2], arg);
    ir.MakeParNum(q.Params[3], def.val(len));
    ir.ParmByArg(q.Params[4], rarg);
    gr.AppendTr(q);
    IF def.is_scalar(n.l.type) THEN
      ir.MakeArgLocal(arg, res_l, 0);
    ELSE
      ir.MakeArgAddr(arg, res_l, 0);
    END;
  END;
END gen_shift;

CONST
  lset_binary_sample = ARRAY OF LONGINT {
  sLabel, 0,
  VAL(SHORTINT,ir.o_fi),    2, VAL(SHORTINT,tune.index_ty), tune.index_sz,  sarg, 0, stmp, 0, ORD(ir.y_Variable), 1,
  VAL(SHORTINT,ir.o_fi),    2, VAL(SHORTINT, tune.addr_ty),  tune.addr_sz,  sarg, 1, stmp, 2, ORD(ir.y_Variable), 3,
  VAL(SHORTINT,ir.o_fi),    2, VAL(SHORTINT, tune.addr_ty),  tune.addr_sz,  sarg, 2, stmp, 4, ORD(ir.y_Variable), 5,
  VAL(SHORTINT,ir.o_fi),    2, VAL(SHORTINT, tune.addr_ty),  tune.addr_sz,  sarg, 3, stmp, 6, ORD(ir.y_Variable), 7,
  VAL(SHORTINT,ir.o_loadr), 1, VAL(SHORTINT, tune.lset_ty),  tune.lset_sz,  stmp, 5,            ORD(ir.y_Variable), 8,
  VAL(SHORTINT,ir.o_add),   2, VAL(SHORTINT, tune.addr_ty),  tune.addr_sz,  stmp, 5, snum, 4, ORD(ir.y_Variable), 4,
  VAL(SHORTINT,ir.o_loadr), 1, VAL(SHORTINT, tune.lset_ty),  tune.lset_sz,  stmp, 7,            ORD(ir.y_Variable), 9,
  (* !!*) -1,               2, VAL(SHORTINT, tune.lset_ty),  tune.lset_sz,  stmp, 9, stmp, 8, ORD(ir.y_Variable), 10,
  VAL(SHORTINT,ir.o_add),   2, VAL(SHORTINT, tune.addr_ty),  tune.addr_sz,  stmp, 7, snum, 4, ORD(ir.y_Variable), 6,
  VAL(SHORTINT,ir.o_storer),2, VAL(SHORTINT, tune.lset_ty),  tune.lset_sz,  stmp, 3, stmp,10, ORD(ir.y_Nothing),
  VAL(SHORTINT,ir.o_add),   2, VAL(SHORTINT, tune.addr_ty),  tune.addr_sz,  stmp, 3, snum, 4, ORD(ir.y_Variable), 2,
  VAL(SHORTINT,ir.o_add),   2, VAL(SHORTINT,tune.index_ty), tune.index_sz,  stmp, 1, snum,-1, ORD(ir.y_Variable), 0,
  VAL(SHORTINT,ir.o_eq),    2, VAL(SHORTINT,tune.index_ty), tune.index_sz,  stmp, 0, snum, 0, ORD(ir.y_Nothing),
  sArc, 1,
  sArc, 0,
  sLabel, 1
  };              --   "while (length) { length--; *c++ = *a++ & *b++; }"

CONST op_place = 68;

PROCEDURE gen_lset_binary(n: pc.NODE; loc: ir.Local);
                                (* loc - переменная-результат *)
VAR
 <* IF setproc THEN *>
  q: ir.TriadePtr;
  N : INT;
 <* END *>
  larg, rarg: ir.Arg;
  args : ARRAY 4 OF ir.Arg;
  len : pc.VALUE;
  op : ir.Operation;
  sample: ARRAY LEN(lset_binary_sample) OF LONGINT;  i: INT;
BEGIN
--  io.print("lset_binary\n");
  ASSERT((n.type.mode = pc.ty_set) & NOT def.is_scalar(n.type));
  len := def.val((n.type.len+tune.BITSET_LEN-1) DIV tune.BITSET_LEN);
  gen_value(n.r, ir.GenModeSet{def.REF}, rarg);
  gen_value(n.l, ir.GenModeSet{def.REF}, larg);
  <* IF setproc THEN *>
    CASE n.sub OF
    | pc.sb_and: N := std.X2C_AND;
    | pc.sb_xor: N := std.X2C_XOR;
    | pc.sb_or:  N := std.X2C_OR;
    | pc.sb_bic: N := std.X2C_BIC;
    END;
    q := def.RTS_call(n.pos, N);
    ir.MakeParNum(q.Params[4], len);
    ir.ParmByArg(q.Params[3], rarg);
    ir.ParmByArg(q.Params[2], larg);
    ir.MakeParAddr(q.Params[1], loc, 0);
    gr.AppendTr(q);
  <* ELSE *>
    FOR i := 0 TO LEN(lset_binary_sample)-1 DO
      sample[i] := lset_binary_sample[i];
    END;
    CASE n.sub OF
    | pc.sb_and: op := ir.o_and;
    | pc.sb_xor: op := ir.o_xor;
    | pc.sb_or:  op := ir.o_or;
    | pc.sb_bic: op := ir.o_andnot;
    END;
    sample[op_place] := VAL(SHORTINT,op);
    ir.MakeArgNum(args[0], len);
    ir.MakeArgAddr(args[1], loc, 0);
    args[2] := rarg;
    args[3] := larg;
    ir.SetSilentMode();
    ops.gen_sample(n.pos, sample, args);
    ir.SetNormalMode();
  <* END *>
END gen_lset_binary;

PROCEDURE chk_ref(n: pc.NODE; md: ir.GenModeSet; VAR arg: ir.Arg);
  VAR q : ir.TriadePtr;
BEGIN
  IF def.REF IN md THEN
    q := def.NewTriadeTS(1, ir.o_assign, n.type);
    q.Position := n.pos;
    q.Tag := ir.y_RealVar;
    make_temp(n.type, q.Name);
    ir.ParmByArg(q.Params[0], arg);
    gr.AppendTr(q);
    ir.MakeArgAddr(arg, q.Name, 0);
  END;
END chk_ref;

PROCEDURE chk_neg(n:pc.NODE; md: ir.GenModeSet; VAR arg: ir.Arg);
  VAR q: ir.TriadePtr;
BEGIN
  IF def.NEG IN md THEN
    q := def.NewTriadeTS(1, ir.o_not, n.type);
    q.Position := n.pos;
    ir.ParmByArg(q.Params[0], arg);
    ir.GenResVar(q);
    gr.AppendTr(q);
    ir.MakeArgVar(arg, q.Name);
  END;
END chk_neg;

PROCEDURE expression_error(tpos-: ir.TPOS; VAR arg: ir.Arg);
BEGIN
  env.errors.Error(tpos, 1008);
  ir.MakeArgNum(arg, def.zz_one);
END expression_error;

PROCEDURE gen_value_binary(n: pc.NODE; md: ir.GenModeSet; VAR arg: ir.Arg);

 VAR larg, rarg: ir.Arg;
   p, q: ir.TriadePtr;
   loc: ir.Local;
   ty: ir.TypeType; sz: ir.SizeType;
 -- yes, no, cont: ir.Node;

  PROCEDURE binary;
    VAR op: ir.Operation;
      rev, chk, dng, rmvb: BOOLEAN;
      zarg: ir.Arg;
  BEGIN
    gen_value(n.l, ir.GenModeSet{}, larg);
    gen_value(n.r, ir.GenModeSet{}, rarg);

    rev := FALSE;
    chk := (pc.ntag_chk_overflow IN n.tags);   -- & (n.type.mode IN pc.WHOLEs);
    IF n.type.mode IN pc.REALs THEN
      dng := TRUE;
      at.was_float_triade := TRUE;
    ELSE dng := FALSE;
    END;
    rmvb := FALSE;
    CASE n.sub OF
    | pc.sb_mul   : op := ir.o_mul;
    | pc.sb_plus  : op := ir.o_add;
    | pc.sb_minus : op := ir.o_add; rev := TRUE;
    | pc.sb_and   : op := ir.o_and;   chk := FALSE; dng := FALSE;
    | pc.sb_xor   : op := ir.o_xor;   chk := FALSE; dng := FALSE;
    | pc.sb_or    : op := ir.o_or;    chk := FALSE; dng := FALSE;
    | pc.sb_bic   : op := ir.o_andnot;chk := FALSE; dng := FALSE;
    | pc.sb_addadr: op := ir.o_add;
    | pc.sb_subadr: op := ir.o_add; rev := TRUE;
    | pc.sb_difadr: op := ir.o_add; rev := TRUE;
    | pc.sb_slash : op := ir.o_dvd;                 dng := TRUE;
    | pc.sb_rem   : op := ir.o_rem;                 dng := TRUE;
    | pc.sb_div, pc.sb_mod:
        IF n.sub = pc.sb_div THEN  op := ir.o_div;
        ELSE                       op := ir.o_mod;
        END;
        dng := TRUE;
        IF (pc.ntag_chk_range IN n.tags) THEN
          def.index_type_info(n.r.type, ty, sz);
          IF n.r.mode = pc.nd_value THEN
            dng := FALSE;
          ELSIF ty = ir.t_int THEN
            ir.MakeArgNum(zarg, def.zz_zero);
            gen_check(ir.o_checkhi, ir.OptionsSet{ir.o_Division}, zarg, rarg, ty, sz, n.pos);
            dng := FALSE;
          END;
        ELSE
          rmvb := TRUE;
        END;
    END;
    p := def.NewTriadeTS(2, op, n.type);
    p.Position := n.pos;
    ir.ParmByArg(p.Params[0], larg);
    ir.ParmByArg(p.Params[1], rarg);
    IF rev  THEN ir.SetParamReverse(p.Params[1], TRUE); END;
    IF dng  THEN INCL(p.Options, ir.o_Dangerous) END;
    IF chk  THEN INCL(p.Options, ir.o_Checked)   END;
    IF rmvb THEN INCL(p.Options, ir.o_Removable) END;
    ir.GenResVar(p);
    gr.AppendTr(p);
    ir.MakeArgVar(arg, p.Name);
    chk_ref(n, md, arg);
  END binary;

BEGIN
<* IF db_e THEN *>
  io.print_pos(n.pos);
  io.print(" gen_value_binary(n.sub=%d, mode=%X)\n", n.sub, md);
<* END *>
  ASSERT(NOT (def.LVL IN md));
  CASE n.sub OF
  | pc.sb_equ, pc.sb_neq, pc.sb_lss, pc.sb_leq, pc.sb_gtr, pc.sb_geq,
    pc.sb_cand, pc.sb_cor:
      gen_boolean_value(n, def.NEG IN md, arg);
      chk_ref(n, md, arg);

  | pc.sb_plus ,pc.sb_minus ,pc.sb_mul,
    pc.sb_addadr, pc.sb_subadr, pc.sb_difadr,
    pc.sb_slash, pc.sb_div, pc.sb_mod, pc.sb_rem:
      ASSERT(n.type.mode#pc.ty_ZZ);
      binary();

  | pc.sb_and, pc.sb_xor, pc.sb_or, pc.sb_bic:
      IF def.is_scalar(n.type) THEN binary();
      ELSE
        ASSERT(def.REF IN md);
        make_temp(n.type, loc);
        gen_lset_binary(n, loc);
        ir.MakeArgAddr(arg, loc, 0);
      END;

  | pc.sb_pre_inc, pc.sb_pre_dec
  , pc.sb_post_inc, pc.sb_post_dec:
      gen_inc_dec(n, arg);
      chk_ref(n, md, arg);

  | pc.sb_ash, pc.sb_rot, pc.sb_lsh, pc.sb_shl, pc.sb_shr:
      gen_shift(n, arg);
      IF def.is_scalar(n.type) THEN
        chk_ref(n, md, arg);
      END;

  | pc.sb_high    :
      ASSERT(n.r.mode=pc.nd_value);
      gen_len(n.l, SHORT(n.r.val.get_integer()), n.type, arg);
      ir.MakeArgNum(rarg, def.zz_one);
      gen_bin_op(n.pos, SYSTEM.VAL(ir.Operation,-VAL(SHORTINT,ir.o_add)), arg, rarg, tune.index_ty, tune.index_sz, arg);
      chk_ref(n, md, arg);

  | pc.sb_len     :
      ASSERT(n.r.mode=pc.nd_value);
      gen_len(n.l, SHORT(n.r.val.get_integer()), n.type, arg);
      chk_ref(n, md, arg);

  | pc.sb_in      :
      gen_boolean_value(n, def.NEG IN md, arg);
      chk_ref(n, md, arg);

  | pc.sb_exp:
      ASSERT(n.l.type.mode IN pc.REALs);
      IF n.r.type.mode IN pc.WHOLEs THEN
        q := def.RTS_call(n.pos, std.X2C_EXPRI, TRUE);
      ELSE
        ASSERT(n.r.type.mode IN pc.REALs);
        q := def.RTS_call(n.pos, std.X2C_EXPRR, TRUE);
      END;
      gen_value(n.l, ir.GenModeSet{}, arg);
      IF (n.l.type.mode # pc.ty_longreal) THEN
        def.type_info(n.l.type, ty, sz);
        gen_conv(n.pos, arg, ty, sz, ir.t_float, tune.longreal_sz);
      END;
      ir.ParmByArg(q.Params[1], arg);
      gen_value(n.r, ir.GenModeSet{}, arg);
      ir.ParmByArg(q.Params[2], arg);
      gr.AppendTr(q);
      ir.MakeArgVar(arg, q.Name);
      IF (n.type.mode # pc.ty_longreal) THEN
        def.type_info(n.type, ty, sz);
        gen_conv(n.pos, arg, ir.t_float, tune.longreal_sz, ty, sz);
      END;
      chk_ref(n, md, arg);
  ELSE expression_error(n.pos, arg);
  END;
END gen_value_binary;


PROCEDURE gen_value_cast(n: pc.NODE; md: ir.GenModeSet; VAR arg: ir.Arg);
(* если размеры типов не совпадают, то значения лишних байтов не определены *)

  PROCEDURE gen_temp_var(sz: LONGINT);
    (* прописывает исходное выражения во временную
       переменную и возвращает ее адрес                    *)
    VAR tmp: ir.Local; q: ir.TriadePtr; rarg:ir.Arg;
  BEGIN
    IF def.LVL IN md THEN
      env.errors.Error(n.pos, 1009);
    END;

    make_temp(n.type, tmp);
    ir.MakeArgAddr(arg, tmp, 0);

    q := ir.NewTriadeInit(3, ir.o_copy, ir.t_void, 0);
    q.Position := n.pos;
    gen_value(n.l, ir.GenModeSet{def.REF}, rarg);
    ir.ParmByArg(q.Params[0], rarg);
    ir.ParmByArg(q.Params[1], arg);
    ir.MakeParNum(q.Params[2], def.val(sz));
    q.NPar := def.type_align (n.type);
    gr.AppendTr(q);
  END gen_temp_var;

  VAR s1, s2 : LONGINT;
    to, from : pc.STRUCT;
    ty, from_ty : ir.TypeType;
    sz, from_sz : ir.SizeType;
BEGIN
<* IF db_e THEN *>
  io.print_pos(n.pos); io.print("gen_value_cast(md = %d)\n", md);
<* END *>

  to := n.type;
  from := n.l.type;
  IF (md*ir.GenModeSet{def.REF,def.LVL}=ir.GenModeSet{})
    & def.is_scalar(to) & def.is_scalar(from)
  THEN
    gen_value(n.l, md, arg);
    def.index_type_info(to, ty, sz);
    def.index_type_info(from, from_ty, from_sz);
    gen_cast(n.pos, arg, from_ty, from_sz, ty, sz);
  ELSIF (to.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) &
       (from.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) OR
       (to.mode=pc.ty_proctype) &
       (from.mode=pc.ty_proctype)
  THEN
    gen_value(n.l, md, arg);
  ELSE
    s1 := pc.code.get_size(pc.su_size, from);
    s2 := pc.code.get_size(pc.su_size, to);
--    io.print("s1 = %d; s2 = %d\n", s1, s2);
    IF to.mode = pc.ty_array_of THEN
      ASSERT(def.REF IN md);
      gen_value(n.l, md, arg);
    ELSIF (s1<=0) OR (s2<=0) THEN
      IF md * ir.GenModeSet{def.REF,def.LVL} # ir.GenModeSet{} THEN
        env.errors.Error(n.pos, 350);
        gen_value(n.l, md, arg);
      ELSE
        env.errors.Error(n.pos, 1010);
        ir.MakeArgNum(arg, def.zz_one);
      END;
    ELSE
      IF    s2 > s1 THEN gen_temp_var(s1)
--      ELSIF s2 = s1 THEN gen_value(n.l, md, arg);
      ELSE
        gen_value(n.l, md+ir.GenModeSet{def.REF}, arg);
        IF NOT (def.REF IN md) THEN
          def.index_type_info(to, ty, sz);
          IF pc.ttag_volatile IN to.tags THEN
            INCL(arg.mode, def.VOLATILE);
          END;
          IF def.LVL IN md THEN
            INCL(arg.mode, def.REF);
          ELSE
            def.deref(n.pos, ty, sz, ir.OptionsSet{}, arg);
          END;
        END;
      END;
    END;
  END;
END gen_value_cast;

CONST CASTed = pc.ARRs + pc.TY_SET{pc.ty_record, pc.ty_set};

PROCEDURE conv_is_cast(fr, to: pc.STRUCT): BOOLEAN;
  VAR sf, st: pc.STRUCT;
BEGIN
  IF (fr.mode IN CASTed) OR (to.mode IN CASTed) OR
     (fr.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) &
     (to.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque}) OR
     (fr.mode=pc.ty_proctype) & (to.mode=pc.ty_proctype)
  THEN
    RETURN TRUE;
  END;
  IF fr.is_ordinal() THEN sf := fr.super_type() ELSE sf := fr END;
  IF to.is_ordinal() THEN st := to.super_type() ELSE st := to END;
  RETURN
    ( (sf.mode IN pc.WHOLEs) OR (sf.mode = pc.ty_loc)) &
    (st.mode=pc.ty_loc) OR
    (sf.mode=pc.ty_loc) &
    (st.mode IN pc.WHOLEs);
END conv_is_cast;

PROCEDURE gen_value_unary_conv(n: pc.NODE; VAR arg: ir.Arg);

(*
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
*)

  VAR sf,st: pc.STRUCT; rarg : ir.Arg;
    ty, to_ty: ir.TypeType; sz, to_sz: ir.SizeType;
--    val: pc.VALUE;

BEGIN
<* IF db_e THEN *>
  io.print_pos(n.pos); io.print("gen_value_unary_conv\n");
<* END *>
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
    sf := def.SIZE_T; n.l.type := sf;
  END;

  IF (sf.mode IN pc.TY_SET{pc.ty_shortint,pc.ty_integer,pc.ty_longint,pc.ty_longlongint,
                  pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard,pc.ty_longlongcard}) &
     (st.mode IN pc.TY_SET{pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard,pc.ty_longlongcard,
                  pc.ty_shortint,pc.ty_integer,pc.ty_longint,pc.ty_longlongint,
                  pc.ty_boolean,pc.ty_char,pc.ty_enum})
  THEN (* 1 .. 4 *)
    gen_value(n.l, ir.GenModeSet{}, arg);
    def.type_info(n.l.type, ty, sz);
    IF pc.ntag_chk_range IN n.tags THEN
      IF ir.GT(n.type.min, n.l.type.min, ty, sz, FALSE) THEN
        ir.MakeArgNum(rarg, n.type.min);
        gen_checkS(ir.o_checklo, ir.OptionsSet{ir.o_Range}, arg, rarg, n.l.type, n.pos);
      END;
      IF ir.GT(n.l.type.max, n.type.max, ty, sz, TRUE) THEN
        ir.MakeArgNum(rarg, n.type.max);
        gen_checkS(ir.o_checklo, ir.OptionsSet{ir.o_Range}, rarg, arg, n.l.type, n.pos);
      END;
    END;
    gen_conversion (n.pos, arg, sf, st);

  ELSIF (sf.mode IN pc.TY_SET{pc.ty_shortint,pc.ty_integer,pc.ty_longint,pc.ty_longlongint,
                  pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard,pc.ty_longlongcard,
                  pc.ty_real,pc.ty_longreal, pc.ty_ld_real}) &
        (st.mode IN pc.REALs) OR
        (sf.mode IN (pc.TY_SET{pc.ty_pointer,pc.ty_opaque, pc.ty_proctype}+pc.WHOLEs)) &
        (st.mode IN (pc.TY_SET{pc.ty_pointer,pc.ty_opaque, pc.ty_proctype}+pc.WHOLEs))
  THEN (* 5 *)
    gen_value(n.l,ir.GenModeSet{},arg);
    IF sf.mode # pc.ty_ZZ THEN
      gen_conversion (n.pos, arg, sf, st);
    END;
  ELSIF (sf.mode IN pc.REALs) & (st.mode IN pc.WHOLEs)
  THEN
    gen_value(n.l, ir.GenModeSet{}, arg);
    gen_conversion (n.pos, arg, sf, st);
  ELSIF (sf.mode=pc.ty_AA) &
        (st.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque,pc.ty_proctype})
  THEN
    ir.MakeArgNum(arg, tune.nil_val);
  ELSIF
    (sf.mode IN pc.TY_SET{pc.ty_longcard,pc.ty_longint,pc.ty_longlongcard,pc.ty_longlongint, pc.ty_ZZ})       (* ?! LAZ *)
    & (st.mode IN pc.TY_SET{pc.ty_pointer,pc.ty_opaque, pc.ty_proctype})
  THEN (* SYSTEM.MAKEADR *)
    gen_value(n.l,ir.GenModeSet{},arg);
  ELSIF ((sf.mode=pc.ty_set) & def.is_scalar(sf)) &
        (st.mode IN pc.TY_SET{pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard,pc.ty_longlongcard,
                  pc.ty_shortint,pc.ty_integer,pc.ty_longint,pc.ty_longlongint,
                  pc.ty_boolean,pc.ty_char,pc.ty_enum})
  THEN (* 9 *)
    gen_value(n.l, ir.GenModeSet{}, arg);
    def.type_info(sf, ty, sz);
    def.type_info(st, to_ty, to_sz);
    gen_cast (n.pos, arg, ty, sz, to_ty, to_sz);

(*
    IF pc.ntag_chk_range IN n.tags THEN                           (*??*)
      ir.MakeArgNum(arg, n.type.min);  ---- sf
      ir.MakeArgNum(arg, n.type.max);  ---- sf
    END;
*)
  ELSIF conv_is_cast(sf, st) THEN
    gen_value_cast(n, ir.GenModeSet{}, arg);
  ELSE
    expression_error(n.pos, arg);
  END;
END gen_value_unary_conv;

PROCEDURE gen_value_unary(n: pc.NODE; md: ir.GenModeSet; VAR arg: ir.Arg);
  VAR q: ir.TriadePtr;
    dummy: ir.Arg;

  PROCEDURE gen_set_compl;
    VAR len, l: INT;
      nm: ir.Local;
  BEGIN
    ASSERT(n.type.mode=pc.ty_set);
    len := n.type.len;
    l := def.type_size(n.type);
    IF def.is_scalar(n.type) THEN
      gen_value(n.l, ir.GenModeSet{}, arg);
      q := ir.NewTriadeInit(2, ir.o_xor, ir.t_unsign, VAL(ir.SizeType, l));
      q.Position := n.pos;
      ir.ParmByArg(q.Params[0], arg);
      ir.MakeParNum(q.Params[1], mask_val(VAL(ir.SizeType, l), len));
      ir.GenResVar(q);
      gr.AppendTr(q);
      ir.MakeArgVar(arg, q.Name);
    ELSE
      make_temp(n.type, nm);
      gen_value(n.l, ir.GenModeSet{def.REF}, arg);
      q := def.RTS_call(n.pos, std.X2C_COMPLEMENT, TRUE);
      ir.MakeParAddr(q.Params[1], nm, 0);
      ir.ParmByArg(q.Params[2], arg);
      ir.MakeParNum(q.Params[3], def.val((l+3) DIV 4));
      gr.AppendTr(q);
      ASSERT(def.REF IN md);
      ir.MakeArgAddr(arg, nm, 0);
    END;
  END gen_set_compl;

VAR
  ty: ir.TypeType;
  sz: ir.SizeType;
  l : SHORTINT;


BEGIN
<* IF db_e THEN *>
  io.print_pos(n.pos); io.print(" gen_value_unary(md = %d)\n", md);
<* END *>
  ASSERT(NOT (def.LVL IN md));
  CASE n.sub OF
  | pc.su_min:
      CASE n.l.type.mode OF
        |pc.ty_real    : ir.MakeArgFloat(arg, pc.real_type.min);
        |pc.ty_longreal: ir.MakeArgFloat(arg, pc.longreal_type.min);
        |pc.ty_ld_real : env.errors.Error(n.pos, 201);
                     --  ir.MakeArgFloat(arg, min_ldreal);
                         ir.MakeArgFloat(arg, pc.ld_real_type.min);
      END;
      chk_ref(n, md, arg);
  | pc.su_max:
      CASE n.l.type.mode OF
        |pc.ty_real    : ir.MakeArgFloat(arg, pc.real_type.max);
        |pc.ty_longreal: ir.MakeArgFloat(arg, pc.longreal_type.max);
        |pc.ty_ld_real : env.errors.Error(n.pos, 201);
                      -- ir.MakeArgFloat(arg, max_ldreal);
                         ir.MakeArgFloat(arg, pc.ld_real_type.max);
      END;
      chk_ref(n, md, arg);
  | pc.su_bits:
      gen_size(n.l, 0, arg);
      q := def.NewTriadeTS(2, ir.o_mul, n.type);
      q.Position := n.pos;
      ir.ParmByArg(q.Params[0], arg);
      ir.MakeParNum(q.Params[1], def.val(8));
      ir.GenResVar(q);
      gr.AppendTr(q);
      ir.MakeArgVar(arg, q.Name);
      chk_ref(n, md, arg);
  | pc.su_bytes, pc.su_size:
      gen_size(n.l, 0, arg);
      chk_ref(n, md, arg);
  | pc.su_abs:
      gen_value(n.l, ir.GenModeSet{}, arg);
      IF n.type.mode IN pc.CARDs THEN RETURN END;
      q := def.NewTriadeTS(1, ir.o_abs, n.type);
      q.Position := n.pos;
      IF (n.type.mode IN pc.INTs) & (pc.ntag_chk_overflow IN n.tags) THEN
        INCL(q.Options, ir.o_Checked);
      END;
      ir.ParmByArg(q.Params[0], arg);
      ir.GenResVar(q);
      gr.AppendTr(q);
      ir.MakeArgVar(arg, q.Name);
      chk_ref(n, md, arg);
  | pc.su_cap:
      gen_value(n.l, ir.GenModeSet{}, arg);
      IF at.CAP_proc IN at.COMP_MODE THEN
        q := def.RTS_call(n.pos, std.X2C_CAP, TRUE);
        ir.ParmByArg(q.Params[1], arg);
      ELSE
        q := def.NewTriadeTS(1, ir.o_cap, n.type);  -- может быть триаду o_cap
        q.Position := n.pos;                        --  следует изжить
        ir.ParmByArg(q.Params[0], arg);
        ir.GenResVar(q);
      END;
      gr.AppendTr(q);
      ir.MakeArgVar(arg, q.Name);
      chk_ref(n, md, arg);
  | pc.su_neg:
      gen_value(n.l, ir.GenModeSet{}, arg);
      def.type_info (n.type, ty, sz);
      gen_neg(n.pos, ty, sz, (pc.ntag_chk_overflow IN n.tags), arg);
      chk_ref(n, md, arg);
  | pc.su_adr, pc.su_adr_o2:
      IF n.l.type.mode IN pc.TY_SET{pc.ty_ZZ, pc.ty_RR, pc.ty_CC} THEN
        -- address of a numerical literal
        env.errors.Error(n.l.pos, 123);
        ir.MakeArgNum(arg, def.zz_one);
      ELSE 
        gen_value(n.l, md*ir.GenModeSet{def.CC}+ir.GenModeSet{def.REF}, arg);
      END;
      chk_ref(n, md, arg);
  | pc.su_conv:
      gen_value_unary_conv(n, arg);
      chk_ref(n, md, arg);
  | pc.su_odd:
      gen_boolean_value(n, def.NEG IN md, arg);
      chk_ref(n, md, arg);
      RETURN
  | pc.su_not:
      gen_value(n.l, md/ir.GenModeSet{def.NEG}, arg);
      chk_ref(n, md, arg);
      RETURN
  | pc.su_compl :
      IF n.type.mode IN pc.WHOLEs THEN
        l := SHORT(SHORT(def.type_size(n.type)));
        IF l=0 THEN -- ZZ_type
          l:=4;
        END;
        CASE l OF
        | 1, 2, 4:
          gen_value(n.l, md, arg);
          chk_ref(n, md, arg);
          q := ir.NewTriadeInit(2, ir.o_xor, ir.t_unsign, l);
          q.Position := n.pos;
          ir.ParmByArg(q.Params[0], arg);
          ir.MakeParNum(q.Params[1], mask_val(l, 8*l));
          ir.GenResVar(q);
          gr.AppendTr(q);
          ir.MakeArgVar(arg, q.Name);
        END;
      ELSE
        gen_set_compl;
      END;
  | pc.su_length:
      q := def.RTS_call(n.pos, std.X2C_LENGTH, TRUE);
      gen_value(n.l, md*ir.GenModeSet{def.CC}+ir.GenModeSet{def.REF}, arg);
      ir.ParmByArg(q.Params[1], arg);
      gen_len(n.l, 0, n.type, arg);
      ir.ParmByArg(q.Params[2], arg);
      gr.AppendTr(q);
      ir.MakeArgVar(arg, q.Name);
      chk_ref(n, md, arg);
  | pc.su_entier:
      at.was_float_triade := TRUE;
      gen_value(n.l, ir.GenModeSet{}, arg);
      IF (n.l.type.mode # pc.ty_longreal) THEN
        q := ir.NewTriadeInit(1, ir.o_val, ir.t_float, 8);
        q.Position := n.pos;
        def.type_info(n.l.type, q.OpType, q.OpSize);
        ir.ParmByArg(q.Params[0], arg);
        ir.GenResVar(q);
        gr.AppendTr(q);
        ir.MakeArgVar(arg, q.Name);
      END;
      q := def.RTS_call(n.pos, std.X2C_ENTIER, TRUE);
      ir.ParmByArg(q.Params[1], arg);
      gr.AppendTr(q);
      ir.MakeArgVar(arg, q.Name);
      chk_ref(n, md, arg);
  | pc.su_re:
      gen_complex_value(n.pos, n.l, TRUE, FALSE, arg, dummy);
      chk_ref(n, md, arg);
  | pc.su_im:
      gen_complex_value(n.pos, n.l, FALSE, TRUE, dummy, arg);
      chk_ref(n, md, arg);
(*
    |pc.su_cc    : gen_value(n.l);
*)

  | pc.su_is:
      gen_boolean_value(n, def.NEG IN md, arg);
      chk_ref(n, md, arg);
      RETURN
  ELSE
    expression_error(n.pos, arg);
  END;
  chk_neg(n, md, arg);
END gen_value_unary;

PROCEDURE gen_index_check*(n: pc.NODE;  (* - expression *)
                           l: pc.NODE;  (* - array or set *)
                           dim: INTEGER;
                           chk: ir.OptionsSet;
                       VAR arg: ir.Arg);
  VAR ntype, ltype: pc.STRUCT; low, high: pc.VALUE;
      rarg, tmp_arg: ir.Arg;  ty: ir.TypeType; sz: ir.SizeType;
      q: ir.TriadePtr;
BEGIN
(* LAZ - временно *)
  WHILE (n.mode = pc.nd_unary) & (n.sub = pc.su_conv)
    & n.l.type.is_ordinal()
  DO
    n := n.l
  END;
(*                *)
  ltype := l.type;
  ntype := n.type;  ASSERT(ntype.is_ordinal());
  def.index_type_info(ntype, ty, sz);
  gen_index_value(n, arg);

  IF (ltype.mode = pc.ty_array_of)
    AND (((l.mode = pc.nd_deref) AND NOT (l.l.type.flag IN opt.LangsWithOpenArrays))
         OR NOT (ltype.flag IN opt.LangsWithOpenArrays))
  THEN  chk := ir.OptionsSet{}
  END;

  IF (ltype.mode = pc.ty_array_of) THEN
    IF chk # ir.OptionsSet{} THEN
      IF (arg.tag # ir.y_NumConst) OR (NOT arg.value.is_zero()) THEN
        gen_len(l, dim, def.INDEX_T, rarg);
        -- this is necessary because we wanna check two conditions
        -- (arg>=0 && arg<=max) by one comparison.
        tmp_arg := arg;
        gen_conv_Ex(n.pos, tmp_arg, tune.index_ty, tune.index_sz,
                             ir.t_unsign, tune.index_sz);
        gen_check(ir.o_checkhi, chk, tmp_arg, rarg, ir.t_unsign(*!!*), tune.index_sz, n.pos);
      END;
    END;
  ELSE
    IF ltype.mode = pc.ty_SS THEN
      low := def.zz_zero;
      high :=def.val(ltype.len-1);
    ELSE
      low  := ltype.min;
      high := ltype.max;
    END;
   <* IF db_e THEN *>
    io.print("  low = %d, high = %d\n", low, high);
   <* END *>
    IF low.is_zero() THEN
      IF chk # ir.OptionsSet{} THEN
        ir.MakeArgNum(rarg, high);
        -- this is necessary because we wanna check two conditions
        -- (arg>=0 && arg<=rarg) by one comparison.
        tmp_arg := arg;
        gen_conv_Ex(n.pos, tmp_arg, tune.index_ty, tune.index_sz,
                                    ir.t_unsign,   tune.index_sz);
        gen_check(ir.o_checklo, chk, rarg, tmp_arg, ir.t_unsign(*!!*), tune.index_sz, n.pos);
      END;
    ELSE
      IF chk # ir.OptionsSet{} THEN
        IF ir.GT(low, ntype.min, ty, sz, TRUE) THEN
          ir.MakeArgNum(rarg, low);
          gen_checkS(ir.o_checklo, chk, arg, rarg, def.INDEX_T, n.pos);
        END;
        IF ir.GT(ntype.max, high, ty, sz, TRUE) THEN
          ir.MakeArgNum(rarg, high);
          gen_checkS(ir.o_checklo, chk, rarg, arg, def.INDEX_T, n.pos);
        END;
      END;
      q := def.NewTriadeTS(2, ir.o_add, def.INDEX_T);
      q.Position := n.pos;
      ir.ParmByArg(q.Params[0], arg);

      IF low.is_neg() THEN
        low := Calc.Unary(pc.su_neg, ir.t_int, tune.index_sz, low);
        ir.MakeParNum(q.Params[1], low);
      ELSE
        ir.MakeParNum_Ex(q.Params[1], ir.y_NumConst, low, TRUE);
      END;
      ir.GenResVar(q);
      gr.AppendTr(q);
      ir.MakeArgVar(arg, q.Name);
    END;
  END;
END gen_index_check;

PROCEDURE gen_index(n: pc.NODE; VAR arg: ir.Arg);
  VAR                               (* всегда выдает адрес !! *)
    l : pc.NODE;
    r   : ARRAY 32 OF pc.NODE;
    chkd: ARRAY 32 OF ir.OptionsSet;
    no: INTEGER;
    i : INTEGER;
    volatile: BOOLEAN;
    p: ir.TriadePtr; 
    larg, rarg: ir.Arg;
BEGIN
  no := 0; l := n; volatile := FALSE;
  REPEAT
    IF pc.ttag_volatile IN l.type.tags THEN volatile := TRUE END;
    r[no] := l.r;
    IF pc.ntag_chk_range IN l.tags THEN chkd[no] := ir.OptionsSet{ir.o_Index};
    ELSE                                chkd[no] := ir.OptionsSet{};
    END;
    INC(no);
    l := l.l;
  UNTIL (l.type.mode # pc.ty_array_of) OR
        (l.mode # pc.nd_index) OR
        (l.l.type.mode # pc.ty_array_of);

  gen_value(l, ir.GenModeSet{def.REF}, larg);
  IF def.VOLATILE IN larg.mode THEN volatile := TRUE END;

  p := ir.NewTriadeInit(no + 1, ir.o_add, tune.addr_ty, tune.addr_sz);
  p.Position := n.pos;
  ir.ParmByArg(p.Params[0], larg);
  i := 1;
  REPEAT
    gen_index_check(r[no-i], l, i-1, chkd[no-i], larg);
    gen_size(l, i, rarg);
    IF (rarg.tag = ir.y_NumConst)
      & ir.EQ(rarg.value, def.zz_one, tune.index_ty, tune.index_sz, TRUE)
    THEN
      ir.ParmByArg(p.Params[i], larg);
    ELSE
      gen_mult(r[no-i].pos, larg, rarg, tune.index_ty, tune.index_sz, arg);
      ir.ParmByArg(p.Params[i], arg);
    END;
    INC(i);
  UNTIL (i > no);
  ir.GenResVar(p);
  gr.AppendTr(p);
  ir.MakeArgVar(arg, p.Name);
  IF volatile THEN INCL(arg.mode, def.VOLATILE) END;
END gen_index;

PROCEDURE gen_lset_aggregate(n: pc.NODE; chk_opt: ir.OptionsSet;
                         VAR arg: ir.Arg);
  VAR nl, ll: pc.NODE;
    type: pc.STRUCT;
    ty: ir.TypeType; sz: ir.SizeType;
    tmp: ir.Local;

  VAR q: ir.TriadePtr;
    args: ARRAY 2 OF ir.Arg;
    l, a, a1, b, b1, d, v, r1: ir.Arg;
    l1, l2, r, r2, s, t: ir.VarNum;
    len: pc.VALUE;
    loop, cont: ir.Node;
    size : LONGINT;
BEGIN
<* IF db_e THEN *>
  io.print_pos(n.pos); io.print(" gen_lset_aggregate\n");
<* END *>
  ASSERT(NOT def.is_scalar(n.type));
  size := def.type_size(n.type);
  ASSERT(size MOD tune.lset_sz = 0);
  len := def.val(size DIV tune.lset_sz);

  make_temp(n.type, tmp);

  ir.SetSilentMode();
  loop := gr.NewNode();                                (*  loop:              *)
  gr.StartNode(loop);

  ir.MakeArgNum(args[0], len);
  ir.GenVar(ir.TEMPORARY, l1, NIL);                    (*  l := fi(len, l1)   *)
  ir.MakeArgVar(args[1], l1);
  gen_fi(n.pos, 2, tune.index_ty, tune.index_sz, args, l);

  q := ir.NewTriadeInit(2, ir.o_add, tune.index_ty, tune.index_sz);
  q.Position := n.pos;                                 (* l1 := l - 1         *)
  ir.ParmByArg(q.Params[0], l);
  ir.MakeParNum(q.Params[1], def.zz_one);
  ir.SetParamReverse(q.Params[1], TRUE);
  ir.SetDef(l1, q);
  gr.AppendTr(q);

  q := ir.NewTriadeInit(2, ir.o_mul, tune.index_ty, tune.index_sz);
  q.Position := n.pos;                                 (* l2 := l1 * 4       *)
  ir.MakeParVar(q.Params[0], l1);
  ir.MakeParNum(q.Params[1], def.val(tune.lset_sz));
  ir.GenResVar(q);
  gr.AppendTr(q);
  l2 := q.Name;
  q := ir.NewTriadeInit(2, ir.o_add, tune.addr_ty, tune.addr_sz);
  q.Position := n.pos;                                  (* t := (&loc) + l2   *)
  ir.MakeParAddr(q.Params[0], tmp, 0);
  ir.MakeParVar(q.Params[1], l2);
  ir.GenResVar(q);
  gr.AppendTr(q);

  ir.MakeArgVar(a, q.Name);               (* *t := 0            *)
  ir.MakeArgNum(b, def.zz_zero);
  gen_storer(n.pos, ir.t_unsign, tune.lset_sz, a, b);

  q := ir.NewTriadeInit(2, ir.o_eq, tune.index_ty, tune.index_sz);
  q.Position := n.pos;                       (* if l1 = 0 then LL1 else loop *)
  ir.MakeParVar(q.Params[0], l1);
  ir.MakeParNum(q.Params[1], def.zz_zero);
  gr.AppendTr(q);

  cont := gr.NewNode();
  gr.NewArc(gr.currNode, cont, FALSE);
  gr.NewArc(gr.currNode, gr.currNode, FALSE);

  gr.StartNode(cont);
  nl := n.l;
  WHILE nl # NIL DO
    IF nl.mode = pc.nd_node THEN
      ll := nl.l;
(*    WHILE (ll.mode = pc.nd_unary) & (ll.sub = pc.su_conv) DO ll := ll.l END;*)
      type := ll.type;
      gen_value_minus_min(ll, n.type.min, chk_opt, a);
      def.index_type_info(type, ty, sz);
      IF (ty # tune.index_ty) OR (sz#tune.index_sz) THEN
        gen_conversion (n.pos, a, type, def.INDEX_T);
      END;
      ll := nl.r;
(*    WHILE (ll.mode = pc.nd_unary) & (ll.sub = pc.su_conv) DO ll := ll.l END;*)
      gen_value_minus_min(ll, n.type.min, chk_opt, b);
      type := ll.type;
      def.index_type_info(type, ty, sz);
      IF (ty # tune.index_ty) OR (sz#tune.index_sz) THEN
        gen_conversion (ll.pos, b, type, def.INDEX_T);
      END;
      <* IF setproc THEN *>
        q := def.RTS_call(n.pos, std.X2C_LONGSET);
        ir.MakeParAddr(q.Params[1], tmp, 0);
        ir.ParmByArg(q.Params[2], a);
        ir.ParmByArg(q.Params[3], b);
        ir.MakeParNum(q.Params[4], def.val(size));
        gr.AppendTr(q);
      <* ELSE *>
        gen_check (ir.o_checklo, ir.OptionsSet{ir.o_Range}, b, a, tune.index_ty, tune.index_sz, nl.pos);

        ir.MakeArgNum(d, def.val(tune.BITSET_LEN));
        gen_bin_op(nl.pos, ir.o_div, a, d, tune.index_ty, tune.index_sz, a1);
        ir.MakeArgNum(d, def.val(tune.lset_sz));
        gen_bin_op(nl.pos, ir.o_mul, a1, d, tune.index_ty, tune.index_sz, a1);
        q := ir.NewTriadeInit(2, ir.o_add, tune.addr_ty, tune.addr_sz);
        q.Position := nl.pos;
        ir.MakeParAddr(q.Params[0], tmp, 0);
        ir.ParmByArg(q.Params[1], a1);
        ir.GenResVar(q);
        gr.AppendTr(q);
        r := q.Name;

        ir.MakeArgNum(d, def.val(tune.BITSET_LEN));
        gen_bin_op(nl.pos, ir.o_div, b, d, tune.index_ty, tune.index_sz, b1);
        ir.MakeArgNum(d, def.val(tune.lset_sz));
        gen_bin_op(nl.pos, ir.o_mul, b1, d, tune.index_ty, tune.index_sz, b1);
        q := ir.NewTriadeInit(2, ir.o_add, tune.addr_ty, tune.addr_sz);
        q.Position := nl.pos;
        ir.MakeParAddr(q.Params[0], tmp, 0);
        ir.ParmByArg(q.Params[1], b1);
        ir.GenResVar(q);
        gr.AppendTr(q);
        s := q.Name;

        ir.MakeArgNum(d, def.val(tune.BITSET_LEN));
        gen_bin_op(nl.pos, ir.o_mod, a, d, tune.index_ty, tune.index_sz, a);
        q := ir.NewTriadeInit(1, ir.o_hiset, tune.lset_ty, tune.lset_sz);
        q.Position := nl.pos;
        ir.ParmByArg(q.Params[0], a);
        ir.GenResVar(q);
        gr.AppendTr(q);
        t := q.Name;

        loop := gr.NewNode();
        gr.StartNode(loop);

        ir.MakeArgVar(args[0], r);                (* r1 := fi(r, r2)   *)
        ir.GenVar(ir.TEMPORARY, r2, NIL);
        ir.MakeArgVar(args[1], r2);
        gen_fi(n.pos, 2, tune.addr_ty, tune.addr_sz, args, r1);

        ir.MakeArgVar(args[0], t);                (* d := fi(t, mm)   *)
        ir.MakeArgNum(args[1], Calc.MaxValue(ir.t_unsign, tune.lset_sz));
        gen_fi(n.pos, 2, tune.lset_ty, tune.lset_sz, args, d);

        q := ir.NewTriadeInit(2, ir.o_eq, tune.addr_ty, tune.addr_sz);
        q.Position := n.pos;
        ir.ParmByArg(q.Params[0], r1);
        ir.MakeParVar(q.Params[1], s);
        gr.AppendTr(q);

        gr.StartNewNode();
        cont := gr.NewNode();
        gr.NewArc(loop, cont, FALSE);
        gr.NewArc(loop, gr.currNode, FALSE);

        v := r1;
        def.deref(nl.pos, tune.lset_ty, tune.lset_sz, ir.OptionsSet{}, v);

        gen_bin_op(nl.pos, ir.o_or, v, d, tune.lset_ty, tune.lset_sz, v);

        gen_storer(nl.pos, tune.lset_ty, tune.lset_sz, r1, v);

        q := ir.NewTriadeInit(2, ir.o_add, tune.addr_ty, tune.addr_sz);
        q.Position := n.pos;
        ir.ParmByArg(q.Params[0], r1);
        ir.MakeParNum(q.Params[1], def.val(tune.lset_sz));
        ir.SetDef(r2, q);
        gr.AppendTr(q);
        gr.Goto(loop);

        gr.StartNode(cont);

        ir.MakeArgNum(v, def.val(tune.BITSET_LEN));
        gen_bin_op(nl.pos, ir.o_mod, b, v, tune.index_ty, tune.index_sz, b);

        q := ir.NewTriadeInit(1, ir.o_loset, tune.lset_ty, tune.lset_sz);
        q.Position := nl.pos;
        ir.ParmByArg(q.Params[0], b);
        ir.GenResVar(q);
        gr.AppendTr(q);

        ir.MakeArgVar(v, q.Name);
        gen_bin_op(nl.pos, ir.o_and, d, v, tune.lset_ty, tune.lset_sz, d);

        v := r1;
        def.deref(nl.pos, tune.lset_ty, tune.lset_sz, ir.OptionsSet{}, v);

        gen_bin_op(nl.pos, ir.o_or, v, d, tune.lset_ty, tune.lset_sz, d);

        gen_storer(nl.pos, tune.lset_ty, tune.lset_sz, r1, d);
      <* END *>
    ELSE
      gen_index_check(nl, n, 0, chk_opt, a);
      <* IF setproc THEN *>
        q := def.RTS_call(nl.pos, std.X2C_INCL);
        ir.MakeParAddr(q.Params[1], tmp, 0);
        ir.ParmByArg(q.Params[2], a);
        ir.MakeParNum(q.Params[3], def.val(n.type.len));
      <* ELSE *>
        ir.MakeArgNum(d, def.val(tune.BITSET_LEN));
        gen_bin_op(nl.pos, ir.o_div, a, d, tune.index_ty, tune.index_sz, arg);
        ir.MakeArgNum(d, def.val(tune.lset_sz));
        gen_bin_op(nl.pos, ir.o_mul, arg, d, tune.index_ty, tune.index_sz, arg);
        ir.MakeArgAddr(b, tmp, 0);
        gen_bin_op(nl.pos, ir.o_add, b, arg, tune.addr_ty, tune.addr_sz, b);
        arg := b;
        def.deref(nl.pos, tune.lset_ty, tune.lset_sz, ir.OptionsSet{}, arg);
        ir.MakeArgNum(d, def.val(tune.BITSET_LEN));
        gen_bin_op(nl.pos, ir.o_mod, a, d, tune.index_ty, tune.index_sz, a);
        q := ir.NewTriadeInit(2, ir.o_incl, tune.lset_ty, tune.lset_sz);
        q.Position := nl.pos;
        ir.ParmByArg(q.Params[0], arg);
        ir.ParmByArg(q.Params[1], a);
        ir.GenResVar(q);
        gr.AppendTr(q);
        ir.MakeArgVar(arg, q.Name);
        gen_storer(nl.pos, tune.lset_ty, tune.lset_sz, b, arg);
      <* END *>
    END;
    nl := nl.next;
  END;
  ir.SetNormalMode();
  ir.MakeArgAddr(arg, tmp, 0);
END gen_lset_aggregate;

-- le (src, 32) ? left : end:
-- left:  t0 = hiset(src)
--        goto end;
--
-- end:   dst = fi(t0, 0);
PROCEDURE MakeHiset(src: ir.Arg; t: pc.STRUCT; pos: ir.TPOS; VAR dst: ir.Arg);
VAR le, hiset, fi: ir.TriadePtr;
    orig, left, end: ir.Node;
    ty : ir.TypeType;
    sz : ir.SizeType;
BEGIN
    def.type_info(t, ty, sz);

    le := ir.NewTriadeInit(2, ir.o_le, tune.index_ty, tune.index_sz);
    ir.ParmByArg(le.Params[0], src);
    ir.MakeParNum(le.Params[1], Calc.GetValue(31, tune.index_ty, tune.index_sz));
    gr.AppendTr(le);
    gr.FinishNode();
    orig := le.NodeNo;

    end := gr.NewNode();
    left := gr.NewNode();
    gr.StartNode(left);

    gr.NewArc(orig, left, FALSE);

    hiset := ir.NewTriadeInit(1, ir.o_hiset, ty, sz);
    hiset.Position := pos;
    ir.ParmByArg(hiset.Params[0], src);
    ir.GenResVar(hiset);
    gr.AppendTr(hiset);
    gr.Goto(end); -- creates 1st arc to end
    gr.NewArc(orig, end,  FALSE);

    gr.StartNode(end);
    fi := ir.NewTriadeInit(2, ir.o_fi, ty, sz);
    ir.MakeParVar(fi.Params[0], hiset.Name);
    ir.MakeParNum(fi.Params[1], Calc.GetValue(0, ty, sz));
    ir.GenResVar(fi);
    gr.AppendTr(fi);

    ir.MakeArgVar(dst, fi.Name);
END MakeHiset;

-- le (src, 32) ? left : end:
-- left:  t0 = loset(src)
--        goto end;
--
-- end:   dst = fi(t0, 0xffffffff);
PROCEDURE MakeLoset(src: ir.Arg; t: pc.STRUCT; pos: ir.TPOS; VAR dst: ir.Arg);
VAR le, loset, fi: ir.TriadePtr;
    orig, left, end: ir.Node;
    ty : ir.TypeType;
    sz : ir.SizeType;
BEGIN
    def.type_info(t, ty, sz);

    le := ir.NewTriadeInit(2, ir.o_le, tune.index_ty, tune.index_sz);
    ir.ParmByArg(le.Params[0], src);
    ir.MakeParNum(le.Params[1], Calc.GetValue(31, tune.index_ty, tune.index_sz));
    gr.AppendTr(le);
    gr.FinishNode();
    orig := le.NodeNo;

    end := gr.NewNode();
    left := gr.NewNode();
    gr.StartNode(left);

    gr.NewArc(orig, left, FALSE);

    loset := ir.NewTriadeInit(1, ir.o_loset, ty, sz);
    loset.Position := pos;
    ir.ParmByArg(loset.Params[0], src);
    ir.GenResVar(loset);
    gr.AppendTr(loset);
    gr.Goto(end); -- creates 1st arc to end
    gr.NewArc(orig, end,  FALSE);

    gr.StartNode(end);
    fi := ir.NewTriadeInit(2, ir.o_fi, ty, sz);
    ir.MakeParVar(fi.Params[0], loset.Name);
    ir.MakeParNum(fi.Params[1], Calc.MaxValue(ty, sz));
    ir.GenResVar(fi);
    gr.AppendTr(fi);

    ir.MakeArgVar(dst, fi.Name);
END MakeLoset;

PROCEDURE gen_set_aggregate(n: pc.NODE; VAR arg: ir.Arg);
  VAR l,ll: pc.NODE; q: ir.TriadePtr; larg, rarg: ir.Arg;
    type: pc.STRUCT; ty: ir.TypeType; sz: ir.SizeType;
    chk_opt: ir.OptionsSet;
BEGIN
<* IF db_e THEN *>
  io.print_pos(n.pos); io.print(" gen_set_aggregate\n");
<* END *>
  ASSERT(n.type.mode = pc.ty_set);
  IF pc.ntag_chk_range IN n.tags THEN chk_opt := ir.OptionsSet{ir.o_Range};
  ELSE                                chk_opt := ir.OptionsSet{};
  END;
  IF NOT def.is_scalar(n.type) THEN
    gen_lset_aggregate(n, chk_opt, arg); RETURN
  END;
  ir.MakeArgNum(arg, def.zz_zero);
  l := n.l;
  WHILE l # NIL DO
    IF l.mode = pc.nd_node THEN
      ll := l.l;
      WHILE (ll.mode = pc.nd_unary) & (ll.sub = pc.su_conv) DO ll := ll.l END;
      type := ll.type;
      gen_value_minus_min(ll, n.type.min, chk_opt, larg);
      def.index_type_info(type, ty, sz);
      IF (ty # tune.index_ty) OR (sz#tune.index_sz) THEN
        gen_conversion (ll.pos, larg, type, def.INDEX_T);
      END;
      MakeHiset(larg, n.type, ll.pos, larg);

      ll := l.r;
      WHILE (ll.mode = pc.nd_unary) & (ll.sub = pc.su_conv) DO ll := ll.l END;
      gen_value_minus_min(ll, n.type.min, chk_opt, rarg);
      type := ll.type;
      def.index_type_info(type, ty, sz);
      IF (ty # tune.index_ty) OR (sz#tune.index_sz) THEN
        gen_conversion (ll.pos, rarg, type, def.INDEX_T);
      END;
      MakeLoset(rarg, n.type, ll.pos, rarg);

      q := def.NewTriadeTS(2, ir.o_and, n.type);
      q.Position := ll.pos;
      ir.ParmByArg(q.Params[0], larg);
      ir.ParmByArg(q.Params[1], rarg);
      ir.GenResVar(q);
      gr.AppendTr(q);
      ir.MakeArgVar(rarg, q.Name);

      IF l = n.l THEN arg := rarg;
      ELSE
        q := def.NewTriadeTS(2, ir.o_or, n.type);
        q.Position := l.pos;
        ir.ParmByArg(q.Params[0], arg);
        ir.ParmByArg(q.Params[1], rarg);
        ir.GenResVar(q);
        gr.AppendTr(q);
        ir.MakeArgVar(arg, q.Name);
      END;
    ELSE
      gen_index_check(l, n, 0, chk_opt, rarg);
      q := def.NewTriadeTS(2, ir.o_incl, n.type);
      q.Position := n.pos;
      ir.ParmByArg(q.Params[0], arg);
      ir.ParmByArg(q.Params[1], rarg);
      ir.GenResVar(q);
      gr.AppendTr(q);
      ir.MakeArgVar(arg, q.Name);
    END;
    l := l.next;
  END;
END gen_set_aggregate;

PROCEDURE gen_array_aggregate(n: pc.NODE; ares-: ir.Arg);
  VAR l,v: pc.NODE; i,j: LONGINT;
    larg, rarg: ir.Arg;
    q: ir.TriadePtr;
    el_type : pc.STRUCT;
    el_len  : pc.VALUE;
    ty: ir.TypeType;   size: LONGINT;
    storer: BOOLEAN;
BEGIN
  ASSERT(n.type.mode=pc.ty_array);
  el_type := n.type.base;
  storer := def.is_scalar(el_type);
  ty := def.type_kind(el_type);
  size := def.type_size(el_type);
  IF storer THEN
    el_len := NIL;
  ELSE
    el_len := def.val(size)
  END;
  l := n.l;
  i := 0;
  WHILE l#NIL DO
    IF l.mode = pc.nd_node THEN
      ASSERT(l.r.mode = pc.nd_value);
      j:=l.r.val.get_integer();
      v:=l.l;
    ELSE
      j := 1; v := l;
    END;
    IF storer THEN gen_value(v, ir.GenModeSet{}, rarg);
    ELSE    gen_value(v, ir.GenModeSet{def.REF}, rarg);
    END;
    WHILE j > 0 DO
      larg := ares;
      def.add_offs(v.pos, i*size, larg);
      IF storer THEN
        gen_storer(v.pos, ty, VAL(ir.SizeType,size), larg, rarg);
      ELSE
        q := ir.NewTriadeInit(3, ir.o_copy, ir.t_void, 0);
        q.Position := v.pos;
        ir.ParmByArg(q.Params[0], rarg);
        ir.ParmByArg(q.Params[1], larg);
        ir.MakeParNum(q.Params[2], el_len);
        q.NPar := def.type_align (el_type);
        gr.AppendTr(q);
      END;
      INC(i); DEC(j);
    END;
    l := l.next;
  END;
  ASSERT(i = n.type.len);
END gen_array_aggregate;

PROCEDURE gen_record_aggregate(n: pc.NODE;   --
                           ares-: ir.Arg);   -- адрес результата

  VAR l: pc.NODE;

  PROCEDURE gen_fld(f: pc.OBJECT);
    VAR larg, rarg: ir.Arg;
      q : ir.TriadePtr;
      ty: ir.TypeType; sz: ir.SizeType;
      type: pc.STRUCT; offs: LONGINT;
  BEGIN
    type := f.type;
    def.o_usage(l.pos, f, ir.GenModeSet{}, larg);
    offs := larg.offset;
    larg := ares;
    def.add_offs(l.pos, offs, larg);
    IF def.is_scalar(f.type) THEN
      def.type_info(f.type, ty, sz);
      gen_value(l, ir.GenModeSet{}, rarg);
      gen_storer(l.pos, ty, sz, larg, rarg);
    ELSE
      gen_value(l, ir.GenModeSet{def.REF}, rarg);
      q := ir.NewTriadeInit(3, ir.o_copy, ir.t_void, 0);
      q.Position := l.pos;
      ir.ParmByArg(q.Params[0], rarg);  (* source *)
      ir.ParmByArg(q.Params[1], larg);  (* destination *)
      ir.MakeParNum(q.Params[2], def.val(def.type_size(type)));
      q.NPar := def.type_align (type);
      gr.AppendTr(q);
    END;
    l := l.next;
  END gen_fld;

  PROCEDURE gen_fld_seq(f: pc.OBJECT);
    VAR v: pc.NODE;
  BEGIN
    WHILE f # NIL DO
      IF f.mode = pc.ob_header THEN
        ASSERT(f.val.mode = pc.nd_case);
        ASSERT(l.mode = pc.nd_value);
        v := f.val.l;
        def.search_record_variant(v,l.val);
        ASSERT(v#NIL);
        IF f.val.obj # NIL THEN gen_fld(f.val.obj) ELSE l := l.next END;
        gen_fld_seq(v.obj);
      ELSE
        gen_fld(f);
      END;
      f := f.next;
    END;
  END gen_fld_seq;

  PROCEDURE gen_level(t: pc.STRUCT);
  BEGIN
    IF t.base # NIL THEN gen_level(t.base) END;
    gen_fld_seq(t.prof);
  END gen_level;

BEGIN
  ASSERT(n.type.mode = pc.ty_record);
  l := n.l;
  gen_level(n.type);
  ASSERT(l = NIL);
END gen_record_aggregate;

PROCEDURE gen_value_sequence(n: pc.NODE; md: ir.GenModeSet; VAR arg: ir.Arg);
  VAR size, offs: LONGINT;
    q: ir.TriadePtr;
    l: pc.NODE;
    loc: ir.Local;
    fr_ty: ir.TypeType; fr_sz: ir.SizeType;
BEGIN
<* IF db_e THEN *>
  io.print_pos(n.pos);
  io.print("gen_value_sequence(md= %d)\n", md);
<* END *>
  ASSERT((def.REF IN md) & NOT(def.LVL IN md));
  size := sequence_size(n);
  IF size = 0 THEN ir.MakeArgNum(arg, tune.nil_val); RETURN END;
  def.make_temp_var(ir.t_arr, size, loc);
  l := n.l; offs := 0;
  WHILE l#NIL DO
    IF l.type.mode IN pc.REALs THEN
      at.was_float_triade := TRUE;
      gen_value(l, ir.GenModeSet{}, arg);
      IF l.type.mode # pc.ty_longreal THEN
        def.type_info(l.type, fr_ty, fr_sz);
        gen_conv(l.pos, arg, fr_ty, fr_sz, ir.t_float, tune.longreal_sz);
      END;
      gen_storer_loc(l.pos, ir.t_float, tune.longreal_sz, loc, offs, arg);
      INC(offs, tune.longreal_sz);
    ELSIF l.type.mode IN pc.SEQ_ARRs THEN
      gen_value(l, md*ir.GenModeSet{def.CC}+ir.GenModeSet{def.REF}, arg);
      gen_storer_loc(l.pos, tune.addr_ty, tune.addr_sz, loc, offs, arg);
      INC(offs, tune.addr_sz);

--    out.wf(",%s[%d].val=0",nm,sz);
      ir.MakeArgNum(arg, def.zz_zero);
      gen_storer_loc(l.pos, tune.index_ty, tune.index_sz, loc, offs, arg);
      INC(offs, tune.index_sz);

--    out.wf(",%s[%d].val=",nm,sz+1);
--    out.ws("-1");
      gen_size(l,0,arg);
      q := ir.NewTriadeInit(2, ir.o_add, tune.index_ty, tune.index_sz);
      q.Position := l.pos;
      ir.ParmByArg(q.Params[0], arg);
      ir.MakeParNum(q.Params[1], def.zz_one);
      ir.SetParamReverse(q.Params[1], TRUE);
      ir.GenResVar(q);
      gr.AppendTr(q);
      ir.MakeArgVar(arg, q.Name);
      gen_storer_loc(l.pos, tune.index_ty, tune.index_sz, loc, offs, arg);
      INC(offs, tune.index_sz);
    ELSE
      ASSERT(l.type.mode IN (pc.SEQ_PTRs+pc.TY_SET{pc.ty_longcard,pc.ty_longint}));
      gen_value(l, ir.GenModeSet{}, arg);
      def.type_info(l.type, fr_ty, fr_sz);
      gen_storer_loc(l.pos, fr_ty, fr_sz, loc, offs, arg);
      INC(offs, fr_sz);
    END;
    l:=l.next;
  END;
--  io.print("offs = %d, size = %d", offs, size);
  ASSERT(offs = size);
  ir.MakeArgAddr(arg, loc, 0);
END gen_value_sequence;

CONST
  (* min_len(x,v)  ; arg0 = &x; arg1 = xlen; arg2 = &v; arg3 = vlen *)
  (*     результат - в последней триаде                             *)

  min_len_sample = ARRAY OF LONGINT {
  VAL(SHORTINT,ir.o_le),    2, VAL(SHORTINT,tune.index_ty), tune.index_sz,  sarg, 1, sarg, 3, ORD(ir.y_Nothing),
  sArc, 0,
  sArc, 0,

  sLabel, 0,
  VAL(SHORTINT,ir.o_fi),    2, VAL(SHORTINT,tune.index_ty), tune.index_sz,  sarg, 1, sarg, 3, ORD(ir.y_Variable), 0
  };

CONST
  (* COPY(x,v)  x --> v; arg0 = &x; arg1 = xlen; arg2 = &v; arg3 = vlen *)

  copy_sample = ARRAY OF LONGINT {

  sLabel, 0,
  VAL(SHORTINT,ir.o_fi),    2, VAL(SHORTINT,tune.index_ty), tune.index_sz,  snum, 0, stmp, 1, ORD(ir.y_Variable), 0,
  VAL(SHORTINT,ir.o_le),    2, VAL(SHORTINT,tune.index_ty), tune.index_sz,  sarg, 3, stmp, 0, ORD(ir.y_Nothing),
  sArc, 5,
  sArc, 1,

  sLabel, 1,
  VAL(SHORTINT,ir.o_add),   2, VAL(SHORTINT, tune.addr_ty),  tune.addr_sz,  sarg, 2, stmp, 0, ORD(ir.y_Variable), 2,
  VAL(SHORTINT,ir.o_le),    2, VAL(SHORTINT,tune.index_ty), tune.index_sz,  sarg, 1, stmp, 0, ORD(ir.y_Nothing),
  sArc, 2,
  sArc, 3,

  sLabel, 2,
  VAL(SHORTINT,ir.o_storer),2,  VAL(SHORTINT,tune.char_ty),  tune.char_sz,  stmp, 2, snum, 0, ORD(ir.y_Nothing),
  VAL(SHORTINT,ir.o_goto),  0,  VAL(SHORTINT,   ir.t_void),             0,                        ORD(ir.y_Nothing),
  sArc, 5,

  sLabel, 3,
  VAL(SHORTINT,ir.o_add),   2,  VAL(SHORTINT,tune.addr_ty),  tune.addr_sz,  sarg, 0, stmp, 0, ORD(ir.y_Variable), 3,
  VAL(SHORTINT,ir.o_loadr), 1,  VAL(SHORTINT,tune.char_ty),  tune.char_sz,  stmp, 3,            ORD(ir.y_Variable), 4,
  VAL(SHORTINT,ir.o_storer),2,  VAL(SHORTINT,tune.char_ty),  tune.char_sz,  stmp, 2, stmp, 4, ORD(ir.y_Nothing),
  VAL(SHORTINT,ir.o_eq),    2,  VAL(SHORTINT,tune.char_ty),  tune.char_sz,  stmp, 4, snum, 0, ORD(ir.y_Nothing),
  sArc, 5,
  sArc, 4,

  sLabel, 4,
  VAL(SHORTINT,ir.o_add),   2,  VAL(SHORTINT,tune.addr_ty),  tune.addr_sz,  stmp, 0, snum, tune.char_sz, ORD(ir.y_Variable), 1,
  VAL(SHORTINT,ir.o_goto),  0,  VAL(SHORTINT,   ir.t_void),             0,                        ORD(ir.y_Nothing),
  sArc, 0,

  sLabel, 5
  };

PROCEDURE gen_copy*(from: pc.NODE; v-, v_len-: ir.Arg; tpos: ir.TPOS);
  VAR                                 (* COPY  X -> V *)
    q: ir.TriadePtr;
    args: ARRAY 4 OF ir.Arg;
    len : pc.VALUE;
BEGIN
  gen_value(from, ir.GenModeSet{def.REF}, args[0]);
  args[2] := v;
  args[3] := v_len;
  IF from.mode = pc.nd_value THEN
    len := def.val(0);
    len.unary(pc.su_length, from.val);
    len.binary(pc.sb_plus, len, def.zz_one);
    ir.MakeArgNum(args[1], len);
    IF v_len.tag = ir.y_NumConst THEN
      IF ir.GT(len, v_len.value, tune.index_ty, tune.index_sz, TRUE ) THEN
        ir.MakeArgNum(args[1], v_len.value);
      END;
    ELSE
      ir.SetSilentMode();
      ops.gen_sample(tpos, min_len_sample, args);
      ir.SetNormalMode();
      ir.MakeArgVar(args[1], ir.Nodes[gr.currNode].Last.Name);
    END;
    q := ir.NewTriadeInit(3, ir.o_copy, ir.t_void, 0);
    q.Position := tpos;
    ir.ParmByArg(q.Params[0], args[0]);   (* source *)
    ir.ParmByArg(q.Params[1], v);         (* destination *)
    ir.ParmByArg(q.Params[2], args[1]);   (* size *)
    q.NPar := def.type_align (from.type); 
    gr.AppendTr(q);
  ELSE
    gen_len(from, 0, def.SIZE_T, args[1]);
    IF at.copystr_proc IN at.COMP_MODE THEN
     (* ----- реализовать вызовом ----- *)
      q := def.RTS_call(tpos, std.X2C_COPY, TRUE);
      ir.ParmByArg(q.Params[1], args[0]);
      ir.ParmByArg(q.Params[2], args[1]);
      ir.ParmByArg(q.Params[3], v);
      ir.ParmByArg(q.Params[4], v_len);
      gr.AppendTr(q);
    ELSE
     (* -------- явно разворачивать в цикл ------- *)
      ir.SetSilentMode();
      ops.gen_sample(tpos, copy_sample, args);
      ir.SetNormalMode();
    END;
  END;
END gen_copy;

VAR CallerIPLocal*: ir.Local;

PROCEDURE gen_caller_ip_adr(VAR arg: ir.Arg);
BEGIN
  IF CallerIPLocal = ir.UNDEFINED THEN
    CallerIPLocal := ir.AddLocal(at.make_name("$CallerIP"), def.local, 4);
    ir.Locals^[CallerIPLocal].VarType := tune.addr_ty;
    ir.Locals^[CallerIPLocal].VarSize := tune.addr_sz;
    ir.Locals^[CallerIPLocal].Offset  := tune.PARAM_START;
    INCL(ir.Locals[CallerIPLocal].Options, ir.o_Constant);
    INCL(ir.Locals[CallerIPLocal].Options, ir.o_Parameters);
  END;
  ir.MakeArgAddr(arg, CallerIPLocal, -4);
END gen_caller_ip_adr;

PROCEDURE conditional_expression(n: pc.NODE; md: ir.GenModeSet; VAR arg: ir.Arg);
  VAR
    then_node, else_node, fi_node: ir.Node;
    ty: ir.TypeType; sz: ir.SizeType;      (* тип результата    *)
    args: ARRAY 2 OF ir.Arg;               (* аргументы *)
    lvl: BOOLEAN;
BEGIN
  ASSERT(n.type.mode # pc.ty_void);
  then_node := gr.NewNode();
  else_node := gr.NewNode();
  fi_node := gr.NewNode();

  lvl := FALSE;
  IF md * ir.GenModeSet{def.REF, def.LVL} = ir.GenModeSet{} THEN
    def.type_info(n.type, ty, sz);
  ELSE
    ty := tune.addr_ty;
    sz := tune.addr_sz;
    IF def.LVL IN md THEN
      INCL(md, def.REF);
      EXCL(md, def.LVL);
      lvl := TRUE;
    END;
  END;

  gen_condition(n.l, then_node, else_node);

  gr.currNode := then_node;
  gen_value(n.r.l, md, args[0]);
  gr.Goto(fi_node);

  gr.currNode := else_node;
  gen_value(n.r.r, md, args[1]);
  gr.Goto(fi_node);

  gr.currNode := fi_node;
  gen_fi(n.pos, 2, ty, sz, args, arg);
  IF lvl THEN INCL(arg.mode, def.REF) END;
END conditional_expression;

PROCEDURE with_temp(o: pc.OBJECT): BOOLEAN;
BEGIN (* переменная сгенерирована транслятором для M2-оператора WITH *)
  RETURN ((o.mode = pc.ob_var)&(o.name[0] = ""))
END with_temp;

VAR gen_sequence*: PROCEDURE(n: pc.NODE;
                        go_ret: BOOLEAN;   (* return - переход в конец блока *)
                          tail: BOOLEAN;   (* далее есть еще операторы *)
                        VAR dd: BOOLEAN);  (* dead end *)

PROCEDURE gen_value*(n: pc.NODE; md: ir.GenModeSet; VAR arg: ir.Arg);
  VAR  q: ir.TriadePtr;
    type: pc.STRUCT;
    ty: ir.TypeType; sz: ir.SizeType;
    larg,rarg: ir.Arg; ofs: LONGINT;
    check_node, err_node, cont_node: ir.Node;
    tmp: ir.Local;
    dd: BOOLEAN;
BEGIN  (* ---- g e n _ v a l u e ----- *)
<* IF db_e THEN *>
  io.print_pos(n.pos); io.print(" gen_value(n = %d; md = %d)\n", n, md);
<* END *>
  CASE n.mode OF
    |pc.nd_var:
       def.o_usage(n.pos, n.obj, md - ir.GenModeSet{def.NEG}, arg);  -- print_arg("nd_var", arg);
       chk_neg(n, md, arg);

    |pc.nd_proc:
       def.o_usage(n.pos, n.obj, ir.GenModeSet{}, arg);
       chk_ref(n, md, arg);

    |pc.nd_field:
       IF n.obj.mode = pc.ob_field THEN
         ofs := def.obj_offset(n.obj);
         gen_value(n.l, md*ir.GenModeSet{def.CC}+ir.GenModeSet{def.REF}, arg);
         def.add_offs(n.pos, ofs, arg);
         def.chk_adr(n.pos, md, n.type, arg);
       ELSE
         ASSERT(FALSE);
       END;
       chk_neg(n, md, arg);

    |pc.nd_index:
       ASSERT((md*ir.GenModeSet{def.REF, def.LVL} # ir.GenModeSet{}) OR def.is_scalar(n.type));
       gen_index(n, arg);
       def.chk_adr(n.pos, md, n.type, arg);
       chk_neg(n, md, arg);

    |pc.nd_deref:
      <* IF statistics THEN *>
       count_deref (n);
      <* END *> 
       gen_value(n.l,ir.GenModeSet{},arg);
       IF (pc.ntag_chk_range IN n.tags)
         AND ( (n.l.mode#pc.nd_var) OR NOT with_temp(n.l.obj))
       THEN
         gen_check_nil(n.pos, arg, n.l.type)
       END;
       IF (n.type.mode=pc.ty_array_of) AND (n.l.type.flag IN opt.LangsWithOpenArrays) THEN
         def.add_offs(n.pos, tune.DYNARR_ADDR_offset, arg);
         def.type_info(n.l.type, ty, sz);
         def.deref(n.pos, ty, sz, ir.OptionsSet{ir.o_Constant}, arg);
       END;
       def.chk_adr(n.pos, md, n.type, arg);
       chk_neg(n, md, arg);

    |pc.nd_replace:
       IF n.l.type.mode IN pc.COMPOUNDs THEN
         gen_value(n.l, ir.GenModeSet{def.REF}, arg);
       ELSE
         gen_value(n.l, ir.GenModeSet{}, arg);
       END;
       gen_value(n.r,md,arg);

    |pc.nd_binary:
       type := n.type;
       IF type.mode IN pc.CPLXs THEN
         gen_complex_binary(n.pos, n, larg, rarg);
         IF def.REF IN md THEN
           make_temp (type, tmp);
           ir.MakeArgAddr (arg, tmp, 0);
           store_complex(n.pos, arg, larg, rarg, type);
         END;
       ELSE
         gen_value_binary(n, md, arg);
       END;

    |pc.nd_unary:
       IF (n.sub=pc.su_cast) OR
         (n.sub=pc.su_conv)
         & NOT (pc.ntag_chk_range IN n.tags)
         & conv_is_cast(n.l.type,n.type)
       THEN
         gen_value_cast(n, md - ir.GenModeSet{def.NEG}, arg);
         chk_neg(n, md, arg);
       ELSE
         gen_value_unary(n, md, arg);
       END;

    |pc.nd_value:
       def.const_aggregate(n.val, n.type, md, arg);

    |pc.nd_call:
       gen_call(n, md - ir.GenModeSet{def.NEG}, arg);
       chk_neg(n, md, arg);

    |pc.nd_assign:
--       ASSERT(md={});

       -- to split a very long sequence of assignments 
       -- we create a new node after 'MAX_assign' assignments
       INC(gr.assign_cnt);
       IF gr.assign_cnt > MAX_assign THEN
         gr.StartNewNode;
       END;

       IF n.obj # NIL THEN type := n.obj.type;           -- pcVis.obj(n.obj, 1);
       ELSE type := n.l.type
       END;
       IF rem_str_extension(n.r) THEN
         IF n.obj # NIL THEN
           def.o_usage(n.pos, n.obj, ir.GenModeSet{def.REF,def.LVL}, arg);
           gen_len_usage(n.pos, n.obj, 0, larg);
         ELSE
           gen_value(n.l, ir.GenModeSet{def.REF,def.LVL}, arg);
           gen_len(n.l, 0, def.SIZE_T, larg);
         END;
         gen_copy(n.r, arg, larg, n.pos);
       ELSIF type.mode IN pc.CPLXs THEN
         gen_complex_value(n.pos, n, TRUE, TRUE, arg, larg);
       ELSIF NOT def.is_scalar(type) THEN
         q := ir.NewTriadeInit(3, ir.o_copy, ir.t_void, 0);
         q.Position := n.pos;
         IF n.obj # NIL THEN def.o_usage(n.pos, n.obj, ir.GenModeSet{def.REF}, arg);
         ELSE                gen_value(n.l, ir.GenModeSet{def.REF}, arg);
         END;
         ir.ParmByArg(q.Params[1], arg);  (* destination *)
         gen_value(n.r, ir.GenModeSet{def.REF}, rarg);
         ir.ParmByArg(q.Params[0], rarg);  (* source *)
         gen_size(n.r, 0, rarg);
         ir.ParmByArg(q.Params[2], rarg);
         q.NPar := def.type_align (type);
         gr.AppendTr(q);
       ELSE
         gen_value(n.r, ir.GenModeSet{}, rarg);
         arg := rarg;   (* rarg value may be changed in pointer conversions *)
         IF n.obj # NIL THEN def.o_usage(n.pos, n.obj, ir.GenModeSet{def.LVL}, larg);
         ELSE                gen_value(n.l, ir.GenModeSet{def.LVL}, larg)
         END;
--       print_arg("larg", larg);  print_arg("rarg",  rarg);
         IF (def.REF IN larg.mode) OR
            (larg.tag = ir.y_AddrConst)
         THEN
           def.type_info(type, ty, sz);
           gen_storer(n.pos, ty, sz, larg, rarg);
         ELSE
           ASSERT(larg.tag = ir.y_RealVar);
           IF (rarg.tag = ir.y_Variable) &
              (ir.Vars[rarg.name].Use = NIL) &
              (ir.Vars[rarg.name].Def.Op = ir.o_call)
           THEN
             q := ir.Vars[rarg.name].Def;
             ir.RemoveVar(q.Name);
             q.Tag := ir.y_RealVar;
             q.Name := larg.name;
             ir.MakeArgLocal(arg, q.Name, 0);
           ELSE
             q := def.NewTriadeTS(1, ir.o_assign, type);
             q.Position := n.pos;
             q.Tag  := ir.y_RealVar;
             q.Name := larg.name;
             ir.ParmByArg(q.Params[0], rarg);
             gr.AppendTr(q);
           END;
         END;
       END;

    |pc.nd_sequence:
       ASSERT(def.REF IN md);
       gen_value_sequence(n, md, arg);

    |pc.nd_guard, pc.nd_eguard:             --   io.print("gen_guard/eguard\n");
       gen_value(n.l, md, arg);
       IF pc.ntag_chk_range IN n.tags THEN
         type := n.type;
         IF type.mode = pc.ty_pointer THEN
           ASSERT( type.flag IN opt.LangsWithRTTI );
           type := type.base;
         END;
         IF skipChecknullOnGuard THEN
             check_node := gr.NewNode();
             err_node := gr.NewNode();
             cont_node := gr.NewNode();
             
             q := ir.NewTriadeInit(2, ir.o_eq, tune.addr_ty, tune.addr_sz);
             q.Position := n.pos;
             ir.ParmByArg(q.Params[0], arg);
             ir.MakeParNil(q.Params[1]);
             gr.AppendTr(q);
             gr.NewArc(gr.currNode, cont_node, TRUE);
             gr.NewArc(gr.currNode, check_node, FALSE);
             gr.StartNode(check_node);
             gen_type_ex(n.l, arg, TRUE, md, FALSE, larg);
         ELSE
             err_node := gr.NewNode();
             cont_node := gr.NewNode();
             IF env.config.Option("gentypeold") THEN
                 gen_type(n.l, larg);
             ELSE
                 gen_type_ex(n.l, arg, TRUE, md, TRUE, larg);
             END;
         END;

         IF n.mode = pc.nd_guard THEN
           def.add_offs(n.pos, tune.BASEs_offset+tune.BASE_size*type.len, larg);
           def.deref(n.pos, tune.addr_ty, tune.addr_sz, ir.OptionsSet{ir.o_Constant}, larg);
         END;
         q := ir.NewTriadeInit(2, ir.o_eq, tune.addr_ty, tune.addr_sz);
         q.Position := n.pos;
         ir.ParmByArg(q.Params[0], larg);
         gen_type_name(type, larg);
         ir.ParmByArg(q.Params[1], larg);
         gr.AppendTr(q);

         gr.NewArc(gr.currNode, cont_node, TRUE);
         gr.NewArc(gr.currNode, err_node, FALSE);
         gr.StartNode(err_node);
         gen_trap_N(tune.guardException, n.pos);
         gr.StartNode(cont_node);
       END;

    |pc.nd_aggregate:
      IF n.type.mode = pc.ty_set THEN
        gen_set_aggregate(n, arg);
        RETURN
      ELSIF n.type.mode = pc.ty_array  THEN
        make_temp(n.type, tmp);
        ir.MakeArgAddr(arg, tmp, 0);
        gen_array_aggregate(n, arg);
      ELSIF n.type.mode = pc.ty_record THEN
        make_temp(n.type, tmp);
        ir.MakeArgAddr(arg, tmp, 0);
        gen_record_aggregate(n, arg);
      ELSE env.errors.Error(n.pos, 1011);
        ir.MakeArgNum(arg, def.zz_one);
        RETURN
      END;
      IF NOT (def.REF IN md) THEN
        ir.MakeArgLocal(arg, tmp, 0);
      END;

    |pc.nd_lconv:
      gen_value_cast(n, md, arg);

    |pc.nd_sproc:
      IF n.sub = pc.sp_callerIPref THEN
        ASSERT(def.REF IN md);
        gen_caller_ip_adr(arg);
      ELSIF n.sub = pc.sp_excepttable THEN
        ASSERT(def.REF IN md);
        def.o_usage(n.pos, cmd.excepttable, ir.GenModeSet{def.REF}, arg);
      ELSE
        gen_sequence(n, TRUE, TRUE, dd);
        arg.tag := ir.y_Nothing;  -- to prevent incorrect 'arg' usage
      END;

  ELSE
    IF (n.mode = pc.nd_if) & (n.type.mode # pc.ty_void) THEN
      conditional_expression(n, md, arg);
    ELSE
      gen_sequence(n, TRUE, TRUE, dd);
      arg.tag := ir.y_Nothing;  -- to prevent incorrect 'arg' usage

--    expression_error(n.pos, arg);

    END;
  END;
END gen_value;

BEGIN
--  def.gen_size_usage := gen_size_usage;
  def.gen_value := gen_value;
 <* IF statistics THEN *>
  init_stat;
 <* END *> 
END opE.

(* ------------------------------ из процедуры gen_in:

--  io.print("gen_condition: base.mode = %d\n", base.mode);
  ASSERT(base.mode IN {pc.ty_range, pc.ty_enum, pc.ty_char});
  IF base.min.is_zero() THEN
    ir.MakeArgNum(arg1, base.max);
  ELSE
    q := def.NewTriadeTS(2, ir.o_add, n.l.type);
    ir.GenResVar(q);
    ir.ParmByArg(q.Params[0], arg0);
    ir.MakeParNum(q.Params[1], base.min);
    q.Params[1].reverse := TRUE;
    ir.MakeArgVar(arg0, q.Name);
    gr.AppendTr(q);
    ir.MakeArgNum(arg1, Calc.Binary(pc.sb_minus, ty, sz, base.max, base.min));
  END;
  gen_check(ir.o_checklo, arg1, arg0, ir.t_unsign(*!!*), tune.index_sz, n.pos);
------------------------------------------------- *)

(* ------------------------------- из процедуры gen_binary:

  | pc.sb_cand:
      yes := gr.NewNode();
      no  := gr.NewNode();
      gen_condition(n.l, yes, no);
      gr.currNode := yes;
      gen_value(n.r,{},rarg);

      IF ir.Nodes[no].NIn>1 THEN
        cont  := gr.NewNode();
        gr.Goto(cont);
        gr.StartNode(no);
      ELSE cont := no;
      END;
      gr.Goto(cont);
      gr.StartNode(cont);
      p := def.NewTriadeTS(2, ir.o_fi, n.type);
      ir.MakeParNum(p.Params[0], def.bool0);
      ir.ParmByArg(p.Params[1], rarg);
      ir.GenResVar(p);
      gr.AppendTr(p);
      IF def.NEG IN md THEN ir.SwapParams(p.Params[0], p.Params[1]) END;
      ir.MakeArgVar(arg, p.Name);

    |pc.sb_cor:
      no  := gr.NewNode();
      yes := gr.NewNode();
      gen_condition(n.l, yes, no);
      gr.currNode := no;
      gen_value(n.r,{},rarg);
      no := gr.currNode;
      ir.MakeGoto(no);

      IF ir.Nodes[yes].NIn>1 THEN
        cont  := gr.NewNode();
        gr.StartNode(yes);
        gr.Goto(cont);
      ELSE cont := yes;
      END;
      gr.StartNode(cont);
      gr.NewArc(no, cont, FALSE);
      p := def.NewTriadeTS(2, ir.o_fi, n.type);
      ir.MakeParNum(p.Params[0], def.bool1);
      ir.ParmByArg(p.Params[1], rarg);
      ir.GenResVar(p);
      gr.AppendTr(p);
      IF def.NEG IN md THEN ir.SwapParams(p.Params[0], p.Params[1]) END;
      ir.MakeArgVar(arg, p.Name);
---------------------------------------------------- *)

