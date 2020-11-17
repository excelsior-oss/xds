--------------------------------------------------------------------------------
--                          Excelsior LLVM Compiler
--                         (c) 2020, Excelsior, LLC.
--
-- Module:   llvmCode
-- Mission:  Main module of LLVM code generator 
-- Authors:  Lvov Konstantin
-- Created:  05-Mar-2020
--------------------------------------------------------------------------------
MODULE llvmCode;

FROM SYSTEM IMPORT PRED, SUCC;

IMPORT sys := SYSTEM;

IMPORT DStrings;
IMPORT fmt := FormStr;

IMPORT  pc := pcK;
IMPORT env := xiEnv;

IMPORT ir;
IMPORT Calc;
IMPORT opStd;
IMPORT prc := opProcs;

IMPORT nms := llvmNames;
IMPORT def := llvmDefs;
IMPORT  cs := llvmCodeSegment;
IMPORT  ei := llvmEmitInstr;

IMPORT tune := opTune;

--------------------------------------------------------------------------------
VAR
  LastVarNum: ir.VarNum;
  
  -- ir.Locals loaded into SSA variables 
  LocalsInVars: POINTER TO ARRAY OF ir.VarNum;  -- INDEXED BY "ir.Local"   

VAR
  DbgMode: BOOLEAN;

--------------------------------------------------------------------------------
PROCEDURE NewVarNum (): ir.VarNum;
BEGIN
  INC(LastVarNum);
  RETURN LastVarNum;
END NewVarNum;

--------------------------------------------------------------------------------
PROCEDURE NewArgVar (var_num: ir.VarNum; type-: ARRAY OF CHAR; align: LONGINT): ei.Arg; 
VAR var_name: nms.String;
BEGIN 
  nms.GetVarName(var_name, var_num);  
  RETURN ei.NewArg(var_name, type, align);
END NewArgVar; 
  
--------------------------------------------------------------------------------
PROCEDURE NewTempVar (type-: ARRAY OF CHAR; align: LONGINT): ei.Arg;
BEGIN
  RETURN NewArgVar(NewVarNum(), type, align);
END NewTempVar;

--------------------------------------------------------------------------------
PROCEDURE NewArgLocal (local_no: ir.Local; as_ptr:=FALSE: BOOLEAN): ei.Arg;
VAR local_name: nms.String;
    type_name: nms.String; 
BEGIN
  nms.GetLocalTypeName(type_name, local_no);
  IF as_ptr THEN
    nms.GetPointerTo(type_name, type_name);
  END;
  nms.GetLocalName(local_name, local_no);
  RETURN ei.NewArg(local_name, type_name, ir.Locals[local_no].Align); 
END NewArgLocal;

--------------------------------------------------------------------------------
PROCEDURE IsProcEntryNode (node_no: ir.Node): BOOLEAN;
BEGIN
  RETURN (node_no = ir.ZERONode);
END IsProcEntryNode; 

--------------------------------------------------------------------------------
PROCEDURE HasCallReturnValue (tr: ir.TriadePtr): BOOLEAN;
BEGIN
  RETURN (tr.ResType # ir.t_void);
END HasCallReturnValue; 

--------------------------------------------------------------------------------
PROCEDURE IsTriadeParamReference (p: ir.ParamPtr): BOOLEAN;
VAR type_name: nms.String;
BEGIN
  CASE p.tag OF
  | ir.y_NumConst:
      IF p.triade.Op = ir.o_call THEN
        nms.GetProcParamType(type_name, p.triade.Prototype, p.protoparnum);  
      ELSE
        nms.GetParamTypeName(type_name, p);
      END;
      RETURN nms.IsPointer(type_name);
  | ir.y_AddrConst:  
      RETURN TRUE;
  | ir.y_RealVar:    
      RETURN ir.Locals[p.name].VarType = ir.t_ref;
  | ir.y_Variable:
      RETURN ir.Vars[p.name].Def.ResType = ir.t_ref;
  ELSE
      RETURN FALSE;
  END;
END IsTriadeParamReference;

--------------------------------------------------------------------------------
PROCEDURE IsTriadeParamSingle (p: ir.ParamPtr): BOOLEAN;
VAR proto: prc.Proto;
    size: ir.SizeType;
BEGIN
  ASSERT( p.tag = ir.y_RealConst );
  IF p.triade.Op = ir.o_call THEN
    proto := prc.ProtoList[p.triade.Prototype];
    size := proto.par[p.protoparnum].size;
  ELSE
    size := p.triade.OpSize;  
  END;
  RETURN size = 4;
END IsTriadeParamSingle;

--------------------------------------------------------------------------------
PROCEDURE IsTriadeParamOpenArray (p: ir.ParamPtr): BOOLEAN; 
BEGIN
  CASE p.tag OF
  | ir.y_RealVar
  , ir.y_AddrConst:
      RETURN (ir.o_Parameters IN ir.Locals[p.name].Options)
           & (ir.Locals[p.name].Obj # NIL)
           & (ir.Locals[p.name].Obj.type.mode = pc.ty_array_of);
  ELSE
      RETURN FALSE;
  END;   
END IsTriadeParamOpenArray;

--------------------------------------------------------------------------------
PROCEDURE IsTriadeParamArray (p: ir.ParamPtr): BOOLEAN; 
BEGIN
  IF p.type # NIL THEN
    RETURN p.type.mode IN pc.ARRs; 
  END;
  CASE p.tag OF
  | ir.y_RealVar
  , ir.y_AddrConst:
      RETURN (ir.Locals[p.name].Obj # NIL)
           & (ir.Locals[p.name].Obj.type.mode IN pc.ARRs);
  ELSE
      RETURN FALSE;
  END;   
END IsTriadeParamArray;

--------------------------------------------------------------------------------
PROCEDURE IsTriadeParamAllocaRes (p: ir.ParamPtr): BOOLEAN; 
BEGIN
  RETURN (p.tag = ir.y_Variable) 
       & (ir.Vars[p.name].Def.Op = ir.o_alloca);  
END IsTriadeParamAllocaRes; 


--------------------------------------------------------------------------------
PROCEDURE GetProcObj (p: ir.ParamPtr): pc.OBJECT;
BEGIN
  CASE p.tag OF
  | ir.y_ProcConst:
      RETURN prc.ProcList[p.name].obj;
  | ir.y_RealVar:
      RETURN ir.Locals[p.name].Obj;
  END;
END GetProcObj;  

--------------------------------------------------------------------------------
PROCEDURE GetNodeLableName (VAR label_name: ARRAY OF CHAR; node_no: ir.Node);
VAR label: def.LabelName;
BEGIN
  cs.GetNodeLable(label, node_no);
  fmt.print(label_name, "%%%s", label);
END GetNodeLableName;

--------------------------------------------------------------------------------
PROCEDURE GetZeroIndexList (count: LONGINT): ei.ArgList;
VAR i: LONGINT;
    idx_list: ei.ArgList;
    idx_type_name: nms.String;
BEGIN
  NEW(idx_list, count);
  nms.GetIndexTypeName(idx_type_name);
  FOR i := 0 TO count-1 DO
    idx_list[i] := ei.NewArg("0", idx_type_name, 0);
  END;  
  RETURN idx_list
END GetZeroIndexList;


--------------------------------------------------------------------------------
PROCEDURE ClearLoadInfo (); 
VAR i: ir.Local;
BEGIN 
  FOR i := ir.ZEROLocal TO PRED(ir.NLocals) DO
    LocalsInVars[i] := ir.UNDEFINED;
  END; 
END ClearLoadInfo; 
  
--------------------------------------------------------------------------------
PROCEDURE InitLoadInfo (); 
BEGIN 
  NEW(LocalsInVars, ir.NLocals);
  ClearLoadInfo();
  LastVarNum := ir.NVars;
END InitLoadInfo; 


--------------------------------------------------------------------------------
PROCEDURE GenLoadLocal (local_no: ir.Local): ei.Arg;
VAR var_num: ir.VarNum;
    res, local: ei.Arg;
BEGIN
  local := NewArgLocal(local_no);
  
  IF LocalsInVars[local_no] # ir.UNDEFINED THEN
    var_num := LocalsInVars[local_no];
    res := NewArgVar(var_num, local.type^, 0);
  ELSE  
    var_num := NewVarNum();
    res := NewArgVar(var_num, local.type^, local.align);

    ei.EmitLoad(res, local);
    LocalsInVars[local_no] := var_num;
  END;
  RETURN res;
END GenLoadLocal;

--------------------------------------------------------------------------------
PROCEDURE GenStoreInLocal (local_no: ir.Local; arg: ei.Arg);
VAR local: ei.Arg;
BEGIN
  local := NewArgLocal(local_no);
  ei.EmitStore(local, arg);
  LocalsInVars[local_no] := ir.UNDEFINED;
END GenStoreInLocal;  


--------------------------------------------------------------------------------
TYPE
  TriadeRes = POINTER TO TriadeResDesc;
  TriadeResDesc = RECORD
    arg: ei.Arg;
    need_store_in_local: BOOLEAN;
    local_no: ir.Local;
  END;   

--------------------------------------------------------------------------------
PROCEDURE GetTriadeRes(tr: ir.TriadePtr): TriadeRes;
VAR res: TriadeRes;
    var_num: ir.VarNum;
    type_name: nms.String;
    align: LONGINT;
BEGIN
  NEW(res);
  CASE tr.Tag OF
  | ir.y_Nothing:
      res.local_no := ir.UndefLocal;
      res.need_store_in_local := FALSE;
      nms.GetTypeName(type_name, pc.void_type);
      res.arg := ei.NewArg("", type_name, 0);
     
  | ir.y_RealVar:    
      res.local_no := tr.Name;
      res.need_store_in_local := TRUE;

      nms.GetLocalTypeName(type_name, res.local_no);
      res.arg := NewTempVar(type_name, ir.Locals[res.local_no].Align);
      
  | ir.y_Variable:
      res.local_no := ir.UndefLocal;
      res.need_store_in_local := FALSE;

      var_num := tr.Name;
      IF ir.Vars[var_num].LocalNo # ir.UndefLocal THEN
        nms.GetLocalTypeName(type_name, ir.Vars[var_num].LocalNo);
        align := ir.Locals[ir.Vars[var_num].LocalNo].Align;
      ELSIF tr.type # NIL THEN  
        nms.GetTypeName(type_name, tr.type);
        align := 0;
      ELSE  
        nms.GetTagTypeName(type_name, tr.ResType, tr.ResSize);
        align := 0;
      END;   
      res.arg := NewArgVar(var_num, type_name, align);
  END;
  RETURN res;
END GetTriadeRes;

--------------------------------------------------------------------------------
PROCEDURE GenCastPtrToInt (arg: ei.Arg): ei.Arg; 
VAR res: ei.Arg;
    res_type_name: nms.String;
BEGIN 
  nms.GetTypeName(res_type_name, pc.longcard_type);
  res := NewTempVar(res_type_name, arg.align);
  ei.EmitPtrToInt(res, arg);
  RETURN ei.NewArg(res.name^, res.type^, arg.align);
END GenCastPtrToInt; 

--------------------------------------------------------------------------------
PROCEDURE GenElementPtr_AddrConst (p: ir.ParamPtr): ei.Arg;
VAR res_type_name: nms.String;
    align: LONGINT;
    res, t1, t2, offset, local: ei.Arg;
BEGIN 
  ASSERT( p.tag = ir.y_AddrConst );
  local := NewArgLocal(p.name, TRUE);

  IF (p.offset = 0) THEN
    -- address of the local itself 
    RETURN local;
  END;
    
  nms.GetParamTypeName(res_type_name, p);
  nms.GetPointerTo(res_type_name, res_type_name);
    
  t1 := GenCastPtrToInt(local);  
  offset := ei.NewArgValue(p.offset, t1.type^);    
  t2 := NewTempVar(t1.type^, t1.align);
  ei.EmitAdd(t2, t1, offset);

  align := nms.GetValueAlignment(local.align + p.offset, tune.max_alignment);
  res := NewTempVar(res_type_name, align);
  ei.EmitIntToPtr(res, t2);
  RETURN res;
END GenElementPtr_AddrConst;

--------------------------------------------------------------------------------
PROCEDURE GenTriadeParam (p: ir.ParamPtr): ei.Arg;
VAR tr: ir.TriadePtr;
    name: nms.String;
    type_name: nms.String;
    var_num: ir.VarNum;
    align: LONGINT;
    res: ei.Arg; 
BEGIN
  CASE p.tag OF
  | ir.y_NumConst:
      IF p.triade.Op = ir.o_call THEN
        nms.GetProcParamType(type_name, p.triade.Prototype, p.protoparnum);  
      ELSE
        nms.GetParamTypeName(type_name, p);
      END;
      IF p.value.is_zero() & IsTriadeParamReference(p) THEN
        name := nms.Null;
      ELSE  
        nms.ValueToStr(name, p.value);
      END;
      res := ei.NewArg(name, type_name, 0);
      
  | ir.y_RealConst:
      IF p.triade.Op = ir.o_call THEN
        nms.GetProcParamType(type_name, p.triade.Prototype, p.protoparnum);  
      ELSE
        nms.GetParamTypeName(type_name, p);
      END;
      nms.RealValueToStr(name, p.value, IsTriadeParamSingle(p));
      res := ei.NewArg(name, type_name, 0);
      
  | ir.y_RealVar:    
      res := GenLoadLocal(p.name);
  
  | ir.y_AddrConst:
      res := GenElementPtr_AddrConst(p);

  | ir.y_Variable:
      var_num := p.name;
      tr := ir.Vars[var_num].Def;
      IF ir.Vars[var_num].LocalNo # ir.UndefLocal THEN
        nms.GetLocalTypeName(type_name, ir.Vars[var_num].LocalNo);
        IF tr.ResType = ir.t_ref THEN
          nms.GetPointerTo(type_name, type_name);
        END;
        align := ir.Locals[ir.Vars[var_num].LocalNo].Align;
      ELSIF tr.type # NIL THEN
        nms.GetTypeName(type_name, tr.type);
        align := tr.type.align;
      ELSE    
        nms.GetTagTypeName(type_name, tr.ResType, tr.ResSize);
        align := 0;
      END;   
      res := NewArgVar(var_num, type_name, align);
      
  | ir.y_ProcConst:
      nms.GetObjName(name, prc.ProcList[p.name].obj);
      nms.GetTypeName(type_name, prc.ProcList[p.name].obj.type);
      align := nms.GetTypeAlign(prc.ProcList[p.name].obj.type, FALSE);
      res := ei.NewArg(name, type_name, align);
  END;
  RETURN res;
END GenTriadeParam;


--------------------------------------------------------------------------------
-- Generates ir.o_ret: 'RETURN [x]' 
PROCEDURE GenReturn (tr: ir.TriadePtr);
VAR type_name: nms.String; 
    value: ei.Arg;
BEGIN
  ASSERT( tr.Op = ir.o_ret );
  ASSERT( tr.Params = NIL );
  nms.GetTagTypeName(type_name, tr.ResType, tr.ResSize);
  value := ei.NewArg("", type_name, 0);
  ei.EmitReturn(value);
END GenReturn;  

--------------------------------------------------------------------------------
-- Generates ir.o_retfun: 'RETURN x' - return value from function 
PROCEDURE GenReturnVal (tr: ir.TriadePtr);
VAR type_name: nms.String; 
    value: ei.Arg;
BEGIN
  ASSERT( tr.Op = ir.o_retfun );
  ASSERT( tr.Params # NIL );
  nms.GetTagTypeName(type_name, tr.ResType, tr.ResSize);
  value := GenTriadeParam(tr.Params[0]); 
  ei.EmitReturn(value);
END GenReturnVal;  

--------------------------------------------------------------------------------
-- Generates ir.o_goto: 'GOTO arc1'
PROCEDURE GenGoto (tr: ir.TriadePtr);
VAR label_name: def.LabelName;
BEGIN
  ASSERT( tr.Op = ir.o_goto );
  cs.GetNodeLable(label_name, ir.Nodes[tr.NodeNo].Out[0]);
  ei.EmitBranch(label_name);
END GenGoto;  

--------------------------------------------------------------------------------
-- Generates ir.o_eq: 'IF x = y  THEN arc1 ELSE arc2'
PROCEDURE GenBranchEQ (tr: ir.TriadePtr);
VAR res, x, y: ei.Arg;
    iftrue, iffalse: def.LabelName;
BEGIN
  ASSERT( tr.Op = ir.o_eq );
  res := NewTempVar(nms.BoolT, 0);
  x := GenTriadeParam(tr.Params[0]); 
  y := GenTriadeParam(tr.Params[1]);
  IF tr.OpType = ir.t_float THEN
    ei.EmitFCmp(ei.fcnd_oeq, res, x, y);
  ELSE  
    ei.EmitICmp(ei.cnd_eq, res, x, y);
  END;
  cs.GetNodeLable(iftrue,  ir.Nodes[tr.NodeNo].Out[0]); 
  cs.GetNodeLable(iffalse, ir.Nodes[tr.NodeNo].Out[1]); 
  ei.EmitCondBranch(res, iftrue, iffalse);
END GenBranchEQ;

--------------------------------------------------------------------------------
-- Generates ir.o_eq: 'IF x <= y THEN arc1 ELSE arc2'
PROCEDURE GenBranchLE (tr: ir.TriadePtr);
VAR res, x, y: ei.Arg;
    cond: ei.ICmpCond;
    iftrue, iffalse: def.LabelName;
BEGIN
  ASSERT( tr.Op = ir.o_le );
  res := NewTempVar(nms.BoolT, 0);
  x := GenTriadeParam(tr.Params[0]); 
  y := GenTriadeParam(tr.Params[1]);
  IF tr.OpType = ir.t_float THEN
    ei.EmitFCmp(ei.fcnd_ole, res, x, y);
  ELSE  
    IF tr.OpType = ir.t_int THEN  cond := ei.cnd_sle;
    ELSE                          cond := ei.cnd_ule;
    END;  
    ei.EmitICmp(cond, res, x, y);
  END;
  cs.GetNodeLable(iftrue,  ir.Nodes[tr.NodeNo].Out[0]); 
  cs.GetNodeLable(iffalse, ir.Nodes[tr.NodeNo].Out[1]); 
  ei.EmitCondBranch(res, iftrue, iffalse);
END GenBranchLE;

--------------------------------------------------------------------------------
-- Generates ir.o_incl: 'IF x IN y THEN arc1 ELSE arc2'
PROCEDURE GenBranchIn (tr: ir.TriadePtr); 
VAR x, y: ei.Arg;
    res, t1, t2, one: ei.Arg;
    iftrue, iffalse: def.LabelName;
BEGIN
  ASSERT( tr.Op = ir.o_in );
  x := GenTriadeParam(tr.Params[0]); 
  y := GenTriadeParam(tr.Params[1]);
  -- shift 'x' of 'y' to zero position
  t1 := NewTempVar(y.type^, 0);
  ei.EmitLshr(t1, y, x);
  -- clean all bits except bit in the zero position
  one := ei.NewArgValue(1, y.type^);
  t2 := NewTempVar(y.type^, 0);
  ei.EmitAnd(t2, t1, one);
  -- cast result to the type suitable for the branch instruction
  res := NewTempVar(nms.BoolT, 0);
  ei.EmitTruncForBranch(res, t2);
  -- emit branch  
  cs.GetNodeLable(iftrue,  ir.Nodes[tr.NodeNo].Out[0]); 
  cs.GetNodeLable(iffalse, ir.Nodes[tr.NodeNo].Out[1]); 
  ei.EmitCondBranch(res, iftrue, iffalse);
END GenBranchIn;  

--------------------------------------------------------------------------------
-- Generates ir.o_case: 'CASE x OF y, z: arc1;  t, w: arc2; ... ELSE arcn'
PROCEDURE GenCase (tr: ir.TriadePtr); 
VAR x: ei.Arg;
    i, varinats_num: LONGINT;
    case_label, else_label: def.LabelName;
    case_value: nms.String;
    start_value, end_value: pc.VALUE;
    case_list: ei.CaseVariant;
BEGIN
  ASSERT( tr.Op = ir.o_case );
  varinats_num := LEN(tr.Params^) DIV 2;

  -- CASE selector
  x := GenTriadeParam(tr.Params[0]); 

  -- CASE else part  
  ASSERT( (varinats_num + 1) = ir.Nodes[tr.NodeNo].NOut );  -- else part is present 
  cs.GetNodeLable(else_label, ir.Nodes[tr.NodeNo].Out[ir.Nodes[tr.NodeNo].NOut - 1]);
  
  -- CASE variants
  case_list := NIL;
  FOR i := varinats_num - 1 TO 0 BY -1 DO
    cs.GetNodeLable(case_label, ir.Nodes[tr.NodeNo].Out[i]);
    start_value := tr.Params[(2 * i) + 1].value; 
    end_value   := tr.Params[(2 * i) + 2].value;
    WHILE Calc.CompareValues(pc.sb_gtr, end_value, start_value, tr.OpType, tr.OpSize, TRUE) DO
      nms.ValueToStr(case_value, end_value);
      case_list := ei.NewCaseVariant(case_value, case_label, case_list);
      end_value := Calc.Binary(pc.sb_minus, tr.OpType, tr.OpSize, end_value, Calc.One);
    END;
    nms.ValueToStr(case_value, start_value);
    case_list := ei.NewCaseVariant(case_value, case_label, case_list);
  END;
  
  ei.EmitSwitch(x, else_label, case_list)
END GenCase;


--------------------------------------------------------------------------------
-- Generates ir.o_getpar: 'r = GETPAR (NPar)'
PROCEDURE GenGetPar (tr: ir.TriadePtr);
VAR local_no: ir.Local; 
    local, param: ei.Arg;
    param_name: nms.String;
BEGIN
  IF tr.Tag = ir.y_RealVar THEN
    local_no := tr.Name;
    local := NewArgLocal(local_no);
    nms.GetProcParamName(param_name, tr.NPar);
    param := ei.NewArg(param_name, local.type^, 0);
    ei.EmitStore(local, param);
    LocalsInVars[local_no] := ir.UNDEFINED;
  ELSE
    ASSERT( FALSE );  
  END;
END GenGetPar;  

--------------------------------------------------------------------------------
-- Generates ir.o_assign: 'r = x'
PROCEDURE GenAssign (tr: ir.TriadePtr);
VAR res: TriadeRes;
    x, tmp: ei.Arg; 
BEGIN
  ASSERT( tr.Op = ir.o_assign );
  res := GetTriadeRes(tr); 
  x := GenTriadeParam(tr.Params[0]);
  IF  IsTriadeParamAllocaRes(tr.Params[0]) THEN
    tmp := NewTempVar(res.arg.type^, res.arg.align);
    ei.EmitBitCast(tmp, x);
    x := tmp;
  END;
  CASE tr.Tag OF
  | ir.y_RealVar:    
      GenStoreInLocal(tr.Name, x);
  | ir.y_Variable:
      ei.EmitMove(res.arg, x);    
  END;
END GenAssign;

--------------------------------------------------------------------------------
-- Generates integer ir.o_add: 'r = x + y + z + ...'
PROCEDURE GenAdd (tr: ir.TriadePtr);
VAR res: TriadeRes;
    arg1, arg2, arg3: ei.Arg;
    args_type_name: nms.String;
    res_type_name: nms.String;
BEGIN
  ASSERT( (tr.Op = ir.o_add) & (tr.ResType # ir.t_float) );
  res  := GetTriadeRes(tr); 
  arg1 := GenTriadeParam(tr.Params[0]); 
  IF LEN(tr.Params^) = 1 THEN
    ei.EmitNeg(res.arg, arg1);

  ELSIF tr.ResType = ir.t_ref THEN
    IF (tr.Params[1].tag = ir.y_NumConst) & tr.Params[1].value.is_zero() THEN
      ei.EmitGetElemetPrt(res.arg, arg1, GetZeroIndexList(2));
    ELSE  
      nms.GetTypeName(args_type_name, pc.longcard_type);

      IF IsTriadeParamReference(tr.Params[0]) THEN
        arg1 := GenCastPtrToInt(arg1);
      END;
      arg2 := GenTriadeParam(tr.Params[1]);
      IF IsTriadeParamReference(tr.Params[1]) THEN
        arg2 := GenCastPtrToInt(arg2);
      END;
      arg3 := NewTempVar(args_type_name, 0);
      IF tr.Params[1].reverse THEN
        ei.EmitSub(arg3, arg1, arg2);
      ELSE   
        ei.EmitAdd(arg3, arg1, arg2);
      END;
      IF tr.type # NIL THEN
        nms.GetPointerTo(res_type_name, res.arg.type^);
        res.arg := ei.NewArg(res.arg.name^, res_type_name, res.arg.align);
      END;   
      ei.EmitIntToPtr(res.arg, arg3);
    END;

  ELSE
    arg2 := GenTriadeParam(tr.Params[1]);
    IF tr.Params[1].reverse THEN
      ei.EmitSub(res.arg, arg1, arg2);
    ELSE   
      ei.EmitAdd(res.arg, arg1, arg2);
    END;
  END;    
  IF res.need_store_in_local THEN
    GenStoreInLocal(res.local_no, res.arg);
  END;
END GenAdd;

--------------------------------------------------------------------------------
-- Generates floating-point ir.o_add: 'r = x + y + z + ...'
PROCEDURE GenFAdd (tr: ir.TriadePtr);
VAR res: TriadeRes;
    x, y: ei.Arg;
BEGIN
  ASSERT( (tr.Op = ir.o_add) & (tr.ResType = ir.t_float) );
  res := GetTriadeRes(tr); 
  x := GenTriadeParam(tr.Params[0]); 
  IF LEN(tr.Params^) = 1 THEN
    ei.EmitFNeg(res.arg, x);
  ELSE  
    y := GenTriadeParam(tr.Params[1]);
    IF tr.Params[1].reverse THEN
      ei.EmitFSub(res.arg, x, y);
    ELSE   
      ei.EmitFAdd(res.arg, x, y);
    END;
  END;
  IF res.need_store_in_local THEN
    GenStoreInLocal(res.local_no, res.arg);
  END;
END GenFAdd;

--------------------------------------------------------------------------------
-- Generates integer ir.o_sub: 'r = x - y'
PROCEDURE GenSub (tr: ir.TriadePtr);
VAR res: TriadeRes;
    x, y: ei.Arg;
BEGIN
  ASSERT( tr.Op = ir.o_sub );
  res := GetTriadeRes(tr); 
  x := GenTriadeParam(tr.Params[0]); 
  y := GenTriadeParam(tr.Params[1]);
  IF tr.ResType = ir.t_float THEN
    ei.EmitFSub(res.arg, x, y);
  ELSE
    ei.EmitSub(res.arg, x, y);
  END;      
  IF res.need_store_in_local THEN
    GenStoreInLocal(res.local_no, res.arg);
  END;
END GenSub; 
  
--------------------------------------------------------------------------------
-- Generates integer ir.o_neg: 'r = - x'
PROCEDURE GenNeg (tr: ir.TriadePtr);
VAR res: TriadeRes;
    x: ei.Arg;
BEGIN
  ASSERT( tr.Op = ir.o_neg );
  res := GetTriadeRes(tr); 
  x := GenTriadeParam(tr.Params[0]); 
  IF tr.ResType = ir.t_float THEN
    ei.EmitFNeg(res.arg, x);
  ELSE
    ei.EmitNeg(res.arg, x);
  END;      
END GenNeg; 

--------------------------------------------------------------------------------
-- Generates ir.o_mul: 'r = x + y + z + ...'
PROCEDURE GenMul (tr: ir.TriadePtr);
VAR res: TriadeRes;
    x, y: ei.Arg;
BEGIN
  ASSERT( tr.Op = ir.o_mul );
  res  := GetTriadeRes(tr); 
  x := GenTriadeParam(tr.Params[0]); 
  y := GenTriadeParam(tr.Params[1]); 
  IF tr.ResType = ir.t_float THEN
    ei.EmitFMul(res.arg, x, y);
  ELSE  
    ei.EmitMul(res.arg, x, y);
  END;
  IF res.need_store_in_local THEN
    GenStoreInLocal(res.local_no, res.arg);
  END;
END GenMul;

--------------------------------------------------------------------------------
-- Generates ir.o_div: 'r = x DIV y'
PROCEDURE GenDiv (tr: ir.TriadePtr);
VAR res: TriadeRes;
    x, y: ei.Arg;
    t1, t2, t3, t4, shift_bits: ei.Arg;
BEGIN
  ASSERT( (tr.Op = ir.o_div) & (tr.ResType # ir.t_unsign) );
  res := GetTriadeRes(tr); 
  x := GenTriadeParam(tr.Params[0]); 
  y := GenTriadeParam(tr.Params[1]);

  t1 := NewTempVar(res.arg.type^, 0);
  ei.EmitSDiv(t1, x, y);
  -- get the reminder
  t2 := NewTempVar(res.arg.type^, 0);
  ei.EmitMul(t2, t1, y);
  t3 := NewTempVar(res.arg.type^, 0);
  ei.EmitSub(t3, x, t2);
  -- get sign of the reminder
  shift_bits := ei.NewArgValue((tr.ResSize * nms.BITS_IN_BYTE) - 1, res.arg.type^);
  t4 := NewTempVar(res.arg.type^, 0);
  ei.EmitAshr(t4, t3, shift_bits);
  -- adjust the quotient 
  ei.EmitAdd(res.arg, t1, t4);
  IF res.need_store_in_local THEN
    GenStoreInLocal(res.local_no, res.arg);
  END;
END GenDiv;  

--------------------------------------------------------------------------------
-- Generates ir.o_dvd: 'r = x / y'
PROCEDURE GenDvd (tr: ir.TriadePtr);
VAR res: TriadeRes;
    x, y: ei.Arg;
BEGIN
  ASSERT( (tr.Op = ir.o_dvd) OR (tr.Op = ir.o_div) );
  res := GetTriadeRes(tr); 
  x := GenTriadeParam(tr.Params[0]); 
  y := GenTriadeParam(tr.Params[1]);
  IF tr.ResType = ir.t_unsign THEN
    ei.EmitUDiv(res.arg, x, y);
  ELSE  
    ei.EmitSDiv(res.arg, x, y);
  END;
  IF res.need_store_in_local THEN
    GenStoreInLocal(res.local_no, res.arg);
  END;
END GenDvd;

--------------------------------------------------------------------------------
-- Generates ir.o_div: 'r = x REM y'
PROCEDURE GenRem (tr: ir.TriadePtr);
VAR res: TriadeRes;
    x, y: ei.Arg;
BEGIN
  ASSERT( (tr.Op = ir.o_rem) OR (tr.Op = ir.o_mod) );
  res := GetTriadeRes(tr); 
  x := GenTriadeParam(tr.Params[0]); 
  y := GenTriadeParam(tr.Params[1]);
  IF tr.ResType = ir.t_unsign THEN
    ei.EmitURem(res.arg, x, y);
  ELSE  
    ei.EmitSRem(res.arg, x, y);
  END;
  IF res.need_store_in_local THEN
    GenStoreInLocal(res.local_no, res.arg);
  END;
END GenRem;

--------------------------------------------------------------------------------
-- Generates ir.o_div: 'r = x MOD y'
PROCEDURE GenMod (tr: ir.TriadePtr);
VAR res: TriadeRes;
    x, y: ei.Arg;
    t1, t2, t3, shift_bits: ei.Arg;
BEGIN
  ASSERT( (tr.Op = ir.o_mod) & (tr.ResType # ir.t_unsign) );
  res := GetTriadeRes(tr); 
  x := GenTriadeParam(tr.Params[0]); 
  y := GenTriadeParam(tr.Params[1]);

  t1 := NewTempVar(res.arg.type^, 0);
  ei.EmitSRem(t1, x, y);
  -- get sign of the reminder
  shift_bits := ei.NewArgValue((tr.ResSize * nms.BITS_IN_BYTE) - 1, res.arg.type^);
  t2 := NewTempVar(res.arg.type^, 0);
  ei.EmitAshr(t2, t1, shift_bits);
  -- inverse sign of the divider
  t3 := NewTempVar(res.arg.type^, 0);
  ei.EmitAnd(t3, t2, y);
  
  -- adjust the reminder 
  ei.EmitAdd(res.arg, t1, t3);
  IF res.need_store_in_local THEN
    GenStoreInLocal(res.local_no, res.arg);
  END;
END GenMod;

--------------------------------------------------------------------------------
-- Generates ir.o_div: 'r = x DIV y'
PROCEDURE GenFDvd (tr: ir.TriadePtr);
VAR res: TriadeRes;
    x, y: ei.Arg;
BEGIN
  ASSERT( tr.Op = ir.o_dvd );
  res  := GetTriadeRes(tr); 
  x := GenTriadeParam(tr.Params[0]); 
  y := GenTriadeParam(tr.Params[1]); 
  ei.EmitFDiv(res.arg, x, y);
  IF res.need_store_in_local THEN
    GenStoreInLocal(res.local_no, res.arg);
  END;
END GenFDvd;

--------------------------------------------------------------------------------
PROCEDURE GetProcArgList (tr: ir.TriadePtr): ei.ArgList;

  -- 1 -- GetProcArgList -------------------------------------------------------
  PROCEDURE isFormalParamOpenArray(proto: prc.Proto; i: LONGINT): BOOLEAN;
  BEGIN
    IF (i < proto.npar) THEN
      IF (proto.par[i].mode = prc.pm_param) & (proto.par[i].type = ir.t_ref) THEN
        IF (proto.par[i].obj # NIL) THEN
          RETURN (proto.par[i].obj.type.mode = pc.ty_array_of);
        ELSE  
          RETURN IsTriadeParamArray(tr.Params[i+1]);
        END;
      END;        
    END;
    RETURN FALSE;
  END isFormalParamOpenArray;  

-- 0 -- GetProcArgList ---------------------------------------------------------
VAR i: LONGINT;
    tname: nms.String; 
    proto: prc.Proto;
    arg: ei.Arg;
    arg_list: ei.ArgList;
BEGIN
  IF (LEN(tr.Params^) < 2) THEN
    RETURN NIL;  -- procedure without parameters
  END;

  NEW(arg_list, LEN(tr.Params^)-1);
  FOR i := 1 TO LEN(tr.Params^)-1 DO
    arg_list[i-1] := GenTriadeParam(tr.Params[i]);
  END;   

  proto := prc.ProtoList[tr.Prototype];
  FOR i := 0 TO LEN(arg_list^)-1 DO
    IF isFormalParamOpenArray(proto, i) THEN
      IF (proto.par[i].obj # NIL) THEN
        nms.GetTypeName(tname, proto.par[i].obj.type);
      ELSE  
        nms.GetTagTypeName(tname, proto.par[i].type, proto.par[i].size);
      END;
      arg := NewTempVar(tname, 0);
      ei.EmitBitCast(arg, arg_list[i]);
      arg_list[i] := arg;
    END;
  END;  

  RETURN arg_list;
END GetProcArgList;

--------------------------------------------------------------------------------
-- Generates ir.o_call: '[ r = ] CALL x (y, z, ... )'
PROCEDURE GenCall (tr: ir.TriadePtr);
VAR obj: pc.OBJECT;
    res: TriadeRes;
    t1, proc_var: ei.Arg;
    proc_name: nms.String;
    ret_type_name: nms.String;
    arg_list: ei.ArgList;
BEGIN
  ASSERT( tr.Op = ir.o_call );
  ASSERT( LEN(tr.Params^) > 0 );  
  obj := GetProcObj(tr.Params[0]);

  nms.GetProcReturnTypeName(ret_type_name, obj);
  arg_list := GetProcArgList(tr);
  
  CASE tr.Params[0].tag OF
  | ir.y_ProcConst:
      nms.GetObjName(proc_name, obj);
  | ir.y_RealVar:
      proc_var := GenTriadeParam(tr.Params[0]); 
      COPY(proc_var.name^, proc_name);
  END;    
  
  IF HasCallReturnValue(tr) THEN
    res := GetTriadeRes(tr); 
    res.arg := ei.NewArg(res.arg.name^, ret_type_name, res.arg.align);
    ei.EmitProcCall(res.arg, proc_name, arg_list);
    IF res.need_store_in_local THEN
      GenStoreInLocal(res.local_no, res.arg);
    END;  
  ELSE
    t1 := ei.NewArg("", ret_type_name, 0);
    ei.EmitProcCall(t1, proc_name, arg_list)  
  END;
  ClearLoadInfo();
END GenCall;  

--------------------------------------------------------------------------------
-- Generates ir.o_stor: 'HALT (x)' 
PROCEDURE GenHalt (tr: ir.TriadePtr);
VAR obj: pc.OBJECT;
    res: TriadeRes;
    exit_code: ei.Arg;
    arg_list: ei.ArgList;
    proc_name: nms.String;
BEGIN
  ASSERT( tr.Op = ir.o_stop );
  res := GetTriadeRes(tr);
  exit_code := GenTriadeParam(tr.Params[0]); 

  obj := opStd.Proc(opStd.X2C_HALT);
  nms.GetObjName(proc_name, obj);
  
  NEW(arg_list, 1);
  arg_list[0] := exit_code;
  ei.EmitProcCall(res.arg, proc_name, arg_list);
  ei.EmitUnreachable();  
END GenHalt;  

--------------------------------------------------------------------------------
-- Generates ir.o_error: 'RAISE (x)' 
PROCEDURE GenError (tr: ir.TriadePtr);
VAR obj: pc.OBJECT;
    res: TriadeRes;
    trap_no: ei.Arg;
    arg_list: ei.ArgList;
    proc_name: nms.String;
BEGIN
  ASSERT( tr.Op = ir.o_error );
  res := GetTriadeRes(tr);
  trap_no := GenTriadeParam(tr.Params[0]); 

  obj := opStd.Proc(opStd.X2C_TRAP_F);
  nms.GetObjName(proc_name, obj);
  
  NEW(arg_list, 1);
  arg_list[0] := trap_no;
  ei.EmitProcCall(res.arg, proc_name, arg_list);
  ei.EmitUnreachable();  
END GenError;  


--------------------------------------------------------------------------------
-- Generates ir.o_storer: '* x = y'
PROCEDURE GenStorer (tr: ir.TriadePtr);
VAR arg1, arg2: ei.Arg;
BEGIN
  ASSERT( tr.Op = ir.o_storer );
  arg1 := GenTriadeParam(tr.Params[0]); 
  arg2 := GenTriadeParam(tr.Params[1]);
  ei.EmitStore(arg1, arg2);
END GenStorer;

--------------------------------------------------------------------------------
-- Generates ir.o_loadr: 'r = * x'
PROCEDURE GenLoadr (tr: ir.TriadePtr);
VAR arg: ei.Arg;
    res: TriadeRes;
BEGIN
  ASSERT( tr.Op = ir.o_loadr );
  res := GetTriadeRes(tr);
  arg := GenTriadeParam(tr.Params[0]); 
  ei.EmitLoad(res.arg, arg);
END GenLoadr;


--------------------------------------------------------------------------------
-- Generates ir.o_fi: 'r = fi (x, y, ...)'
PROCEDURE GenFi (tr: ir.TriadePtr);
VAR i, in_node_num: LONGINT;
    res: TriadeRes;
    arg: ei.Arg;
    pred_list: ei.ArgList;
    node_label: def.LabelName;
BEGIN
  ASSERT( tr.Op = ir.o_fi );
  in_node_num := ir.Nodes[tr.NodeNo].NIn; 
  res := GetTriadeRes(tr);
  NEW(pred_list, in_node_num);
  FOR i := 0 TO in_node_num-1 DO
    arg := GenTriadeParam(tr.Params[i]);
--    GetNodeLableName(node_label, ir.Nodes[tr.NodeNo].In[i]);
    cs.GetNodeLable(node_label, ir.Nodes[tr.NodeNo].In[i]);
    pred_list[i] := ei.NewArg(arg.name^, node_label, arg.align);
  END;
  ei.EmitPhi(res.arg, pred_list); 
END GenFi;

--------------------------------------------------------------------------------
PROCEDURE GetElementPtrIndexList (tr: ir.TriadePtr): ei.ArgList;
VAR i, j: LONGINT;
    idx_list: ei.ArgList;
    idx_type_name: nms.String;
BEGIN
  nms.GetIndexTypeName(idx_type_name);
  IF IsTriadeParamOpenArray(tr.Params[0]) THEN
    NEW(idx_list, LEN(tr.Params^)-1);
    j := 0;
  ELSE
    NEW(idx_list, LEN(tr.Params^));
    idx_list[0] := ei.NewArg("0", idx_type_name, 0);
    j := 1;
  END;
  FOR i := 1 TO LEN(tr.Params^)-1 DO
    idx_list[j] := GenTriadeParam(tr.Params[i]);
    INC(j);
  END;   
  RETURN idx_list;
END GetElementPtrIndexList;

--------------------------------------------------------------------------------
-- Generates ir.o_getelementptr: 'r = x [index1, index2, ...]'
PROCEDURE GenGetElementPtr (tr: ir.TriadePtr); 
VAR res: TriadeRes;
    item: ei.Arg;
    idx_list: ei.ArgList;
BEGIN 
  ASSERT( tr.Op = ir.o_getelementptr );
  res := GetTriadeRes(tr);
  item := GenTriadeParam(tr.Params[0]);
  idx_list := GetElementPtrIndexList(tr);
  ei.EmitGetElemetPrt(res.arg, item, idx_list);
END GenGetElementPtr;  

--------------------------------------------------------------------------------
-- Generates ir.o_incl: 'r = INCL (a, b)' and 'r = EXCL (a, b)'
PROCEDURE GenInclExcl (tr: ir.TriadePtr); 
VAR res: TriadeRes;
    a, b: ei.Arg;
    t1, t2, one: ei.Arg;
BEGIN 
  ASSERT( tr.Op = ir.o_incl );
  res := GetTriadeRes(tr);
  a := GenTriadeParam(tr.Params[0]);
  b := GenTriadeParam(tr.Params[1]);
  
  one := ei.NewArgValue(1, a.type^);
  t1 := NewTempVar(a.type^, 0);
  ei.EmitShl(t1, one, b);  -- сдвигаем '1' влево на 'b'
  CASE tr.Op OF
  | ir.o_incl:  -- INCL(a, b) = a | (1 << b)
      ei.EmitOr(res.arg, a, t1);
      
  | ir.o_excl:  -- EXCL(a, b) = a & ~(1 << b)
      t2 := NewTempVar(a.type^, 0);
      ei.EmitNeg(t2, t1);
      ei.EmitAnd(res.arg, a, t2);
  END;
  IF res.need_store_in_local THEN
    GenStoreInLocal(res.local_no, res.arg);
  END;
END GenInclExcl;  


--------------------------------------------------------------------------------
-- Generates integer ir.o_and: 'r = x AND y AND z AND ...'
--                   ir.o_or:  'r = x OR y OR z OR ...'
--                   ir.o_xor: 'r = x XOR y XOR z XOR ...'
PROCEDURE GenBitwiseLogicalOp (tr: ir.TriadePtr);
VAR res: TriadeRes;
    x, y: ei.Arg;
BEGIN
  res := GetTriadeRes(tr); 
  x := GenTriadeParam(tr.Params[0]); 
  y := GenTriadeParam(tr.Params[1]);
  CASE tr.Op OF
  | ir.o_add: ei.EmitAnd(res.arg, x, y);
  | ir.o_or:  ei.EmitOr(res.arg,  x, y);
  | ir.o_xor: ei.EmitXor(res.arg, x, y);
  END;
END GenBitwiseLogicalOp; 

--------------------------------------------------------------------------------
-- Generates ir.o_not: 'r = NOT x  (for BOOL only 0 <->1 )'
PROCEDURE GenLogicalNot (tr: ir.TriadePtr); 
VAR res: TriadeRes;
    x, one: ei.Arg;
BEGIN 
  ASSERT( tr.Op = ir.o_not );
  res := GetTriadeRes(tr); 
  x := GenTriadeParam(tr.Params[0]); 
  one := ei.NewArgValue(1, res.arg.type^);
  ei.EmitXor(res.arg, x, one);  
END GenLogicalNot;  

--------------------------------------------------------------------------------
-- Generates ir.o_val: 'r = VAL ((OpType, OpSize) x)'
PROCEDURE GenCastVal (tr: ir.TriadePtr); 
VAR res: TriadeRes;
    x: ei.Arg;
BEGIN 
  ASSERT( (tr.Op = ir.o_val) OR (tr.Op = ir.o_cast) );
  res := GetTriadeRes(tr);
  x := GenTriadeParam(tr.Params[0]);
  
  IF res.arg.type^ = x.type^ THEN
    ei.EmitMove(res.arg, x);
    
  ELSIF (tr.ResType # ir.t_float) & (tr.OpType # ir.t_float) THEN     
    -- int -> int
    IF tr.ResSize <= tr.OpSize THEN
      ei.EmitTrunc(res.arg, x);
    ELSIF tr.OpType = ir.t_int THEN
      ei.EmitSExt(res.arg, x);
    ELSE    
      ei.EmitZExt(res.arg, x);
    END;     
      
  ELSIF (tr.ResType = ir.t_float) & (tr.OpType # ir.t_float) THEN
    -- int -> float   
    CASE tr.Op OF
    | ir.o_val:    
        IF tr.OpType = ir.t_int THEN
          ei.EmitSIToFP(res.arg, x);
        ELSE  
          ei.EmitUIToFP(res.arg, x);
        END;
    | ir.o_cast:
        ei.EmitBitCast(res.arg, x);
    END;
          
  ELSIF (tr.ResType # ir.t_float) & (tr.OpType = ir.t_float) THEN
    -- float -> int   
    CASE tr.Op OF
    | ir.o_val:    
        IF tr.ResType = ir.t_int THEN
          ei.EmitFPToSI(res.arg, x);
        ELSE  
          ei.EmitFPToUI(res.arg, x);
        END;
    | ir.o_cast:    
        ei.EmitBitCast(res.arg, x);
    END;
      
  ELSE  
    -- float -> float   
    IF tr.ResSize <= tr.OpSize THEN
      ei.EmitFPTrunc(res.arg, x);
    ELSE  
      ei.EmitFPExt(res.arg, x);
    END;
  END;
  
  IF res.need_store_in_local THEN
    GenStoreInLocal(res.local_no, res.arg);
  END;  
END GenCastVal;  


--------------------------------------------------------------------------------
-- Generates ir.o_copy: 'COPY: * x -> * y (z bytes)'
PROCEDURE GenCopyByConst (tr: ir.TriadePtr);
VAR x, y: ei.Arg;
    len: sys.CARD32;
    cnt_word: LONGINT;
    cnt_byte: LONGINT;
    idx1, idx2, one: ei.Arg; 
    x1, y1: ei.Arg;
    item, ptr_item_x, ptr_item_y: ei.Arg; 
    item2, ptr_item_x2, ptr_item_y2: ei.Arg;
    cmp_res: ei.Arg;
    index_type: nms.String; 
    item_type, ptr_item_type: nms.String;  
    item2_type, ptr_item2_type: nms.String;  
    phi_arg_list, gep_arg_list: ei.ArgList;
    sg_node, sg_body, sg_node_continue: cs.Segment;
BEGIN
  ASSERT( (tr.Op = ir.o_copy) & (tr.Params[2].tag = ir.y_NumConst) );
  x := GenTriadeParam(tr.Params[0]);
  y := GenTriadeParam(tr.Params[1]); 
  len := nms.Par2Cardinal(tr.Params[2]);

  IF len <= 16 THEN
    -- load value
    nms.GetPointerBase(item_type, x.type^);
    item := NewTempVar(item_type, x.align);
    ei.EmitLoad(item, x);
    -- store value
    ei.EmitStore(y, item);
  ELSE
    cnt_word := len DIV 4;
    cnt_byte := len MOD 4;

    nms.GetPointerTo(ptr_item_type, nms.WordT);
    x1 := NewTempVar(ptr_item_type, x.align);
    y1 := NewTempVar(ptr_item_type, y.align);
    ptr_item_x := NewTempVar(ptr_item_type, x.align);
    item := NewTempVar(nms.WordT, x.align);
    ptr_item_y := NewTempVar(ptr_item_type, y.align);
        
    nms.GetIndexTypeName(index_type);
    idx1 := NewTempVar(index_type, 4);
    idx2 := NewTempVar(index_type, 4);
    cmp_res := NewTempVar(nms.BoolT, 0);
  
    -- Copies bytes starting from the first: 'y[0] := x[0]' ... 'y[z-1] := x[z-1]'
    -- node:
    --     br body
    -- body:
    --     idx1 = phi [cnt_word, node], [idx2, body]
    --     itm = load x + idx1
    --     store itm, y + idx1
    --     idx2 = add idx1, 1
    --     br idx2 < cnt_word, body, node_rest
    -- node_rest:
    sg_node := cs.GetSegment();
    cs.EnterNewSegment();
    sg_body := cs.GetSegment();
    cs.EnterNewSegment();
    sg_node_continue := cs.GetSegment();

    -- node:
    cs.SetSegment(sg_node);
    ei.EmitBitCast(x1, x);
    ei.EmitBitCast(y1, y);
    ei.EmitBranch(sg_body.label);

    -- body:
    cs.SetSegment(sg_body);
    NEW(phi_arg_list, 2);
    phi_arg_list[0] := ei.NewArg("0", sg_node.label, 0);
    phi_arg_list[1] := ei.NewArg(idx2.name^, sg_body.label, idx2.align);
    ei.EmitPhi(idx1, phi_arg_list);
    
    NEW(gep_arg_list, 1);
    gep_arg_list[0] := ei.NewArg(idx1.name^, idx1.type^, idx1.align);
    ei.EmitGetElemetPrt(ptr_item_x, x1, gep_arg_list);
    ei.EmitLoad(item, ptr_item_x);
    
    ei.EmitGetElemetPrt(ptr_item_y, y1, gep_arg_list);
    ei.EmitStore(ptr_item_y, item);

    one := ei.NewArgValue(1, idx2.type^);
    ei.EmitAdd(idx2, idx1, one);
    ei.EmitICmp(ei.cnd_slt, cmp_res, idx2, ei.NewArgValue(cnt_word, idx2.type^));
    ei.EmitCondBranch(cmp_res, sg_body.label, sg_node_continue.label);
    
    -- node_rest:
    cs.SetSegment(sg_node_continue);
    IF cnt_byte > 0 THEN
      ptr_item_x := NewTempVar(ptr_item_type, x.align);
      ptr_item_y := NewTempVar(ptr_item_type, y.align);

      CASE cnt_byte OF
      | 1: COPY("i8",  item2_type);
      | 2: COPY("i16", item2_type);
      | 3: COPY("i24", item2_type);
      END;
      item2 := NewTempVar(item2_type, x.align);
      
      nms.GetPointerTo(ptr_item2_type, item2_type);
      ptr_item_x2 := NewTempVar(ptr_item2_type, x.align);
      ptr_item_y2 := NewTempVar(ptr_item2_type, y.align);

      NEW(gep_arg_list, 1);
      gep_arg_list[0] := ei.NewArgValue(cnt_word, index_type);
      ei.EmitGetElemetPrt(ptr_item_x, x1, gep_arg_list);
      ei.EmitBitCast(ptr_item_x2, ptr_item_x);
      ei.EmitLoad(item2, ptr_item_x2);

      ei.EmitGetElemetPrt(ptr_item_y, y1, gep_arg_list);
      ei.EmitBitCast(ptr_item_y2, ptr_item_y);
      ei.EmitStore(ptr_item_y2, item2);
    END; 
  END;    
END GenCopyByConst;

--------------------------------------------------------------------------------
-- Generates ir.o_copy: 'COPY: * x -> * y (z bytes)'
-- Copies bytes starting from the last: 'y[z-1] := x[z-1]' ... 'y[0] := x[0]'
-- node:
--     br test
-- body:
--     itm = load x + idx2
--     store itm, y + idx2
-- test: 
--     idx1 = phi [z, node], [idx2, body]
--     idx2 = sub idx1, 1
--     br idx2 >= 0, body, node_rest
-- node_rest:
PROCEDURE GenCopyByVar (tr: ir.TriadePtr);
VAR x, y, z: ei.Arg;
    idx1, idx2, zero, one: ei.Arg; 
    cmp_res, x1, ptr_itm_x, ptr_itm_y, itm: ei.Arg; 
    type: nms.String;
    gep_arg_list: ei.ArgList;
    phi_arg_list: ei.ArgList;
    sg_node, sg_body, sg_test, sg_node_continue: cs.Segment;
BEGIN
  ASSERT( tr.Op = ir.o_copy );
  x := GenTriadeParam(tr.Params[0]);
  y := GenTriadeParam(tr.Params[1]); 
  z := GenTriadeParam(tr.Params[2]);

  nms.GetPointerTo(type, nms.ByteT);
  x1 := NewTempVar(type, x.align);
  ptr_itm_x  := NewTempVar(type, x.align);
  itm := NewTempVar(nms.ByteT, x.align);
  ptr_itm_y := NewTempVar(type, y.align);
  
  idx1 := NewTempVar(z.type^, 4);
  idx2 := NewTempVar(z.type^, 4);
  cmp_res := NewTempVar(nms.BoolT, 0);
  
  -- allocate segments
  sg_node := cs.GetSegment();
  cs.EnterNewSegment();
  sg_body := cs.GetSegment();
  cs.EnterNewSegment();
  sg_test := cs.GetSegment();
  cs.EnterNewSegment();
  sg_node_continue := cs.GetSegment();

  -- node:
  cs.SetSegment(sg_node);
  ei.EmitBitCast(x1, x);
  ei.EmitBranch(sg_test.label);
  
  -- body:
  cs.SetSegment(sg_body);
  NEW(gep_arg_list, 1);
  gep_arg_list[0] := ei.NewArg(idx2.name^, idx2.type^, idx2.align);
  ei.EmitGetElemetPrt(ptr_itm_x, x1, gep_arg_list);
  ei.EmitLoad(itm, ptr_itm_x);
  
  ei.EmitGetElemetPrt(ptr_itm_y, y, gep_arg_list);
  ei.EmitStore(ptr_itm_y, itm);
  ei.EmitBranch(sg_test.label);
  
  -- test:
  cs.SetSegment(sg_test);
  NEW(phi_arg_list, 2);
  phi_arg_list[0] := ei.NewArg(z.name^, sg_node.label, z.align);
  phi_arg_list[1] := ei.NewArg(idx2.name^, sg_body.label, idx2.align);
  ei.EmitPhi(idx1, phi_arg_list);
  one := ei.NewArgValue(1, idx1.type^);
  ei.EmitSub(idx2, idx1, one);
  zero := ei.NewArgValue(0, idx2.type^);
  ei.EmitICmp(ei.cnd_sge, cmp_res, idx2, zero);
  ei.EmitCondBranch(cmp_res, sg_body.label, sg_node_continue.label);
  
  -- node_rest:
  cs.SetSegment(sg_node_continue);
END GenCopyByVar;

--------------------------------------------------------------------------------
-- Generates ir.o_alloca: 'r = alloca (x)'
PROCEDURE GenAlloca (tr: ir.TriadePtr); 
VAR res: TriadeRes;
    x: ei.Arg;
BEGIN 
  ASSERT( tr.Op = ir.o_alloca );
  res := GetTriadeRes(tr);
  x := GenTriadeParam(tr.Params[0]);
  ei.EmitArrayAllocate(res.arg, x);
  ei.EmitDbgLocalAddr(res.local_no, res.arg);
END GenAlloca;  

--------------------------------------------------------------------------------
PROCEDURE GenAllocateLocal (local_no: ir.Local); 
VAR local: ei.Arg;  
BEGIN
  local := NewArgLocal(local_no); 
  ei.EmitAllocate(local);
  ei.EmitDbgLocalAddr(local_no, NewArgLocal(local_no, TRUE));
END GenAllocateLocal; 

--------------------------------------------------------------------------------
PROCEDURE AllocStackMemory ();
VAR i: ir.Local;
BEGIN
  FOR i := ir.ZEROLocal TO PRED(ir.NLocals) DO
    IF ~ir.IsExternal(i) THEN
      GenAllocateLocal(i);
    END;
  END;
END AllocStackMemory;  


--------------------------------------------------------------------------------
PROCEDURE GenTriade (tr: ir.TriadePtr); 
BEGIN
  CASE tr.Op OF
  | ir.o_getpar:         GenGetPar(tr);
  | ir.o_alloca:         GenAlloca(tr); 
  
  | ir.o_not:            GenLogicalNot(tr);
  | ir.o_and
  , ir.o_or
  , ir.o_xor:            GenBitwiseLogicalOp(tr);

  | ir.o_assign:         GenAssign(tr);
  | ir.o_add:            IF tr.ResType = ir.t_float THEN GenFAdd(tr);
                         ELSE                            GenAdd(tr);
                         END;
  | ir.o_sub:            GenSub(tr);
  | ir.o_neg:            GenNeg(tr);

  | ir.o_mul:            GenMul(tr);
  | ir.o_div:            IF tr.ResType = ir.t_unsign THEN GenDvd(tr);
                         ELSE                             GenDiv(tr);                       
                         END;   
  | ir.o_dvd:            IF tr.ResType = ir.t_float THEN GenFDvd(tr);
                         ELSE                            GenDvd(tr);
                         END;
  | ir.o_rem:            GenRem(tr);   
  | ir.o_mod:            IF tr.ResType = ir.t_unsign THEN GenRem(tr);
                         ELSE                             GenMod(tr);                       
                         END;          
  
  | ir.o_storer:         GenStorer(tr);
  | ir.o_loadr:          GenLoadr(tr);
  
  | ir.o_copy:           IF tr.Params[2].tag = ir.y_NumConst THEN GenCopyByConst(tr);
                         ELSE                                     GenCopyByVar(tr);
                         END; 
  
  | ir.o_call:           GenCall(tr);
  | ir.o_ret:            GenReturn(tr);
  | ir.o_retfun:         GenReturnVal(tr);
  
  | ir.o_stop:           GenHalt(tr);
  | ir.o_error:          GenError(tr);
  
  | ir.o_goto:           GenGoto(tr);
  | ir.o_eq:             GenBranchEQ(tr);
  | ir.o_le:             GenBranchLE(tr);
  
  | ir.o_case:           GenCase(tr);
  
  | ir.o_fi:             GenFi(tr);
  | ir.o_getelementptr:  GenGetElementPtr(tr);
  
  | ir.o_incl        
  , ir.o_excl:           GenInclExcl(tr);
  | ir.o_in:             GenBranchIn(tr);
  
  | ir.o_val
  , ir.o_cast:           GenCastVal(tr) 
  ELSE
  END; 
END GenTriade; 

--------------------------------------------------------------------------------
PROCEDURE GenNode (node_no: ir.Node);
VAR i: LONGINT;
    tr: ir.TriadePtr;
    comment: DStrings.String;
BEGIN
  cs.EnterNewSegment(node_no);
  ClearLoadInfo();

  IF IsProcEntryNode(node_no) THEN
    AllocStackMemory();
  END;

  i  := 0;
  tr := ir.Nodes[node_no].First;
  WHILE tr # NIL DO
    cs.SetSourcePos(tr.Position); 
    IF DbgMode THEN
      comment := nms.GetTriadeDbgStr(i, tr);
      ei.EmitComment(comment^);
    END;
    GenTriade(tr);
    INC(i);
    tr := tr.Next;
  END; 
END GenNode;

--------------------------------------------------------------------------------
PROCEDURE Generate *(); 
VAR i: ir.Node;
BEGIN
  DbgMode := env.config.Option("LLVM_DBG");
  cs.Init();
  InitLoadInfo(); 

  FOR i := ir.ZERONode TO PRED(ir.Nnodes) DO 
    IF ir.Nodes[i].Alive THEN
      GenNode(i);
    END;   
  END;

  cs.EndGenProc();
  LocalsInVars := NIL;
END Generate; 

END llvmCode.