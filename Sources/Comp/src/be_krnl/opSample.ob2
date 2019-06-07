MODULE opSample;

(* tools for generation triade sequences *)

IMPORT SYSTEM,
       ir,
       gr := ControlGraph,
       Calc,
       tune := opTune,
       pc   := pcK;

TYPE INT = ir.INT;

TYPE Arg = ir.Arg;

CONST
  max_label_no = 20;           (* максимальное количество узлов *)
  max_tmp_no = 30;             (* максимальное количество временных *)

CONST
  max_op_code = VAL(SHORTINT,MAX(ir.Operation)); (* максимальный код операции в триадах *)

  Arc*     =  max_op_code + 1 ; (* <Arc>,<номер узла> если у Arc номер узла < 0, *)
                                (* то это номер настоящий с минусом              *)
  Label*   =  Arc + 1;          (* <Label>,<номер узла>   *)
  Result*  =  Label + 1;        (* <Result>, <parameter>  *)     
  End*     =  Result + 1;       (* <End> -- конец шаблона *)

  arg*     =  -max_op_code;         (* <Arg>,<номер аргумента> *)
  tmp*     =  arg - 1;         (* временная переменная *)
  num*     =  arg - 2;         (* численная константа *)
  loc*     =  arg - 3;         (* номер локала *)
  prc*     =  arg - 4;         (* номер процедуры *)

  Opt*     =  arg - 5;         (* пометка для опции триады (необяз.) *)
  ResType* =  arg - 6;         (* пометка о типе результата (необяз.) *)

PROCEDURE gen_sample_res * (tpos-: ir.TPOS;
                          sample-: ARRAY OF LONGINT;
                            args-: ARRAY OF Arg;
                      VAR res_arg: Arg);
  VAR n: INT;
    labs    : ARRAY max_label_no OF ir.Node;
    tmp_vars: ARRAY max_tmp_no OF ir.VarNum;
    q: ir.TriadePtr;

  PROCEDURE get(): LONGINT;
    VAR res: LONGINT;
  BEGIN
    res := sample[n]; INC(n);
    RETURN res
  END get;

  PROCEDURE get_param_arg(VAR prm_arg: Arg);
    VAR kind, v: INT;
  BEGIN
    kind := get();
    v := get();
--  io.print(" param_arg(kind=%d,v=%d) ", kind, v);
    CASE kind OF
    | arg:
       prm_arg := args[v];
    | tmp:
       IF tmp_vars[v] = ir.UNDEFINED THEN
         ir.GenVar(ir.TEMPORARY, tmp_vars[v], NIL);
       END;
       ir.MakeArgVar(prm_arg, tmp_vars[v]);
    | num:
       ir.MakeArgNum(prm_arg, Calc.GetInteger(v, q.OpSize));
    | loc:
       ir.MakeArgLocal(prm_arg, VAL(ir.VarNum, v), 0);
    | prc:
       ir.MakeArgProc(prm_arg, VAL(ir.VarNum, v));
    END;
  END get_param_arg;

  PROCEDURE get_param(i : INT);
    VAR arg: Arg;
  BEGIN
--  io.print("param(%d) ", i);
    get_param_arg(arg);
    ir.ParmByArg(q.Params[i], arg);
  END get_param;

  PROCEDURE gen_triade(op: ir.Operation);
    VAR k: INT;
      s: LONGINT;
      arn: INT; ty: ir.TypeType; sz : ir.SizeType;
      neg : BOOLEAN;  (* sign for the LAST parameter of o_add *)
  BEGIN
--  io.print("get_triade(op=%d, arn=%d, ty=%d, sz=%d) ",
--                              op, get(), get(), get());   DEC(n, 3);
    arn := get();
    ty  := VAL(ir.TypeType,get());
    sz  := VAL(ir.SizeType, get());
    IF VAL(SHORTINT,op) < 0 THEN ASSERT(VAL(SHORTINT,op) = -VAL(SHORTINT,ir.o_add));
      op := ir.o_add;
      neg := TRUE;
    ELSE
      neg := FALSE;
    END;
    q := ir.NewTriadeInit(arn, op, ty, sz);
    q.Position := tpos;
    FOR k := 0 TO arn - 1 DO get_param(k) END;
    LOOP
      s := get();
      IF s # Opt THEN EXIT END;
--    io.print(" Option = %d ", get()); DEC(n);
      INCL(q.Options, VAL(ir.Option,get()));
    END;
    IF s = ResType THEN (* --- get_res_type --- *)
--    io.print("ResType = %d, ResSize = %d ", get(), get()); DEC(n,2);
      q.ResType := VAL(ir.TypeType,get());
      q.ResSize := VAL(ir.SizeType,get());
      s := get()
    END;
   (* --- get_result --- *)
    q.Tag := VAL(ir.TagType,s);
    IF VAL(ir.TagType,s) = ir.y_RealVar THEN
--    io.print("RealVar = %d\n ", get()); DEC(n);
      q.Name := VAL(ir.VarNum,get());
    ELSIF VAL(ir.TagType,s) = ir.y_Variable THEN
--    io.print("Variable = %d\n ", get()); DEC(n);
      s := get();
      IF tmp_vars[s] = ir.UNDEFINED THEN
        ir.GenVar(ir.TEMPORARY, tmp_vars[s], q);
      END;
      ir.SetDef(tmp_vars[s], q);
    ELSE ASSERT( VAL(ir.TagType,s) = ir.y_Nothing);
--    io.print("Nothing\n ");
    END;
    IF (op = ir.o_add) & neg THEN
      ir.SetParamReverse(q.Params[arn-1], TRUE);
    END;
    gr.AppendTr(q);
  END gen_triade;

  VAR i: INT; s: LONGINT; lab: ir.Node;
BEGIN
  q := NIL;
  FOR i := 0 TO max_tmp_no-1 DO tmp_vars[i] := ir.UNDEFINED END;
  FOR i := 0 TO max_label_no-1 DO labs[i] := ir.UndefNode END;
  n := 0;
  LOOP
    IF n >= LEN(sample) THEN EXIT END;
    s := get();
    IF ((0 <= s) & (s <= max_op_code)) OR (s = -VAL(SHORTINT,ir.o_add)) THEN
      gen_triade(VAL(ir.Operation,s));
    ELSIF s = End THEN
--    io.print("End\n");
      EXIT
    ELSIF s = Result THEN
--    io.print("Result = "); 
      get_param_arg(res_arg);
    ELSIF s = Arc THEN
--    io.print("Arc --> %d\n", get()); DEC(n);
      s := get();
      IF s < 0 THEN
        lab := VAL(ir.Node, -s);
      ELSE
        lab := labs[s];
        IF lab = ir.UndefNode THEN
          lab := gr.NewNode();
          labs[s] := lab;
        END;
      END;
      gr.NewArc(gr.currNode, lab, FALSE);
    ELSE ASSERT(s = Label);
--    io.print("Label : %d\n", get()); DEC(n);
      s := get();
      IF labs[s] = ir.UndefNode THEN labs[s] := gr.NewNode() END;
      gr.StartNode(labs[s]);
    END;
  END;
END gen_sample_res;

PROCEDURE gen_sample*(tpos-: ir.TPOS;
                    sample-: ARRAY OF LONGINT;
                      args-: ARRAY OF Arg);
  VAR dummy_arg: Arg;
BEGIN
  dummy_arg.tag := ir.y_Nothing;
  gen_sample_res(tpos, sample, args, dummy_arg);
  ASSERT(dummy_arg.tag = ir.y_Nothing);
END gen_sample;

END opSample.

