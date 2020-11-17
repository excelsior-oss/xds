--------------------------------------------------------------------------------
--                          Excelsior LLVM Compiler
--                          (c) 2020, Excelsior, LLC.
-- Module:   llvmEmitInstr
-- Mission:  LLVM Instruction Emitter
-- Synonym:  ei
-- Authors:  Lvov Konstantin
-- Created:  11-Mar-2020
--------------------------------------------------------------------------------
MODULE llvmEmitInstr;

IMPORT DStrings;
IMPORT fmt := FormStr;

IMPORT ir;
IMPORT fc := CodeFace;

IMPORT  cs := llvmCodeSegment;
IMPORT nms := llvmNames;
IMPORT formLLVM;
IMPORT dbgLLVM;

--------------------------------------------------------------------------------
TYPE
  String *= DStrings.String;

  Arg *= POINTER TO ArgDesc;
  ArgDesc *= RECORD
    name -: String;
    type -: String;
    align-: LONGINT;
  END;
    
  ArgList *= POINTER TO ARRAY OF Arg;
  
  ArgListStr = ARRAY 1024 OF CHAR;

--------------------------------------------------------------------------------
PROCEDURE NewArg *(name-, type-: ARRAY OF CHAR; align: LONGINT): Arg;
VAR arg: Arg;
BEGIN
  NEW(arg);
  DStrings.Assign(name, arg.name);
  DStrings.Assign(type, arg.type);
  arg.align := align;
  RETURN arg;
END NewArg;  

--------------------------------------------------------------------------------
PROCEDURE NewArgValue *(value: LONGINT; type-: ARRAY OF CHAR): Arg;
VAR str: nms.String;
BEGIN
  nms.IntToStr(str, value);
  RETURN NewArg(str, type, 0)
END NewArgValue;  


--------------------------------------------------------------------------------
-- Emits comment in LLVM assembler.
PROCEDURE EmitComment *(str-: ARRAY OF CHAR); 
BEGIN 
  cs.GenInstr(";%s", str);
END EmitComment; 


--------------------------------------------------------------------------------
-- Emits instruction to allocate memory on the stack frame of the currently executing function. 
-- '<result> = alloca [inalloca] <type>, align <alignment>' 
PROCEDURE EmitAllocate *(res: Arg);
BEGIN
  cs.GenInstr("%s = alloca %s, align %d", res.name^, res.type^, res.align);
END EmitAllocate;  

--------------------------------------------------------------------------------
-- Emits instruction to allocate memory on the stack frame of the currently executing function. 
-- '<result> = alloca [inalloca] <type>, <ty> <NumElements>, align <alignment>' 
PROCEDURE EmitArrayAllocate *(res, num: Arg);
VAR item_type: nms.String;
BEGIN
  nms.GetPointerBase(item_type, res.type^);
  IF res.align # 0 THEN
    cs.GenInstr("%s = alloca %s, %s %s, align %d", res.name^, item_type, num.type^, num.name^, res.align);
  ELSE  
    cs.GenInstr("%s = alloca %s, %s %s", res.name^, item_type, num.type^, num.name^);
  END;
END EmitArrayAllocate;  


--------------------------------------------------------------------------------
-- Emits instruction to calculate the sum of its two integer operands. 
-- '<result> = add <ty> <op1>, <op2>' 
PROCEDURE EmitAdd *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = add %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitAdd;  

--------------------------------------------------------------------------------
-- Emits instruction to calculate the sum of its two floating-point operands. 
-- '<result> = fadd <ty> <op1>, <op2>' 
PROCEDURE EmitFAdd *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = fadd %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitFAdd;  

--------------------------------------------------------------------------------
-- Emits instruction to calculate the difference of its two integer operands. 
-- '<result> = sub <ty> <op1>, <op2>' 
PROCEDURE EmitSub *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = sub %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitSub;  

--------------------------------------------------------------------------------
-- Emits instruction to calculate the difference of its two floating-point operands. 
-- '<result> = fsub <ty> <op1>, <op2>' 
PROCEDURE EmitFSub *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = fsub %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitFSub;  

--------------------------------------------------------------------------------
-- Emits instruction to calculate the negation of its integer operand. 
PROCEDURE EmitNeg *(res, op: Arg);
VAR t0: Arg;
BEGIN
  t0 := NewArgValue(0, res.type^);
  EmitSub(res, t0, op);
END EmitNeg;  

--------------------------------------------------------------------------------
-- Emits instruction to calculate the negation of its floating-point operand.
-- '<result> = fneg <ty> <op1>' 
PROCEDURE EmitFNeg *(res, op: Arg);
BEGIN
  cs.GenInstr("%s = fneg %s %s", res.name^, res.type^, op.name^);
END EmitFNeg;  

--------------------------------------------------------------------------------
-- Emits instruction to calculate the integer product of the two operands.
-- '<result> = fmul <ty> <op1>, <op2>'
PROCEDURE EmitMul *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = mul %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitMul;  

--------------------------------------------------------------------------------
-- Emits instruction to calculate the floating-point product of the two operands.
-- '<result> = fmul <ty> <op1>, <op2>'
PROCEDURE EmitFMul *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = fmul %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitFMul;  

--------------------------------------------------------------------------------
-- Emits instruction to calculate the floating-point quotient of the two operands.
-- '<result> = fdiv <ty> <op1>, <op2>'
PROCEDURE EmitFDiv *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = fdiv %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitFDiv;  

--------------------------------------------------------------------------------
-- Emits instruction to calculate the floating-point remainder from the division
-- of its two operands.
-- '<result> = frem <ty> <op1>, <op2>'
PROCEDURE EmitFRem *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = frem %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitFRem;  


--------------------------------------------------------------------------------
-- Emits instruction to calculate the signed integer quotient of its two operands
-- rounded towards zero. 
-- '<result> = sdiv <ty> <op1>, <op2>'
PROCEDURE EmitSDiv *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = sdiv %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitSDiv;  

--------------------------------------------------------------------------------
-- Emits instruction to calculate the unsigned integer quotient of its two operands. 
-- '<result> = udiv <ty> <op1>, <op2>'
PROCEDURE EmitUDiv *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = udiv %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitUDiv;  

--------------------------------------------------------------------------------
-- Emits instruction to calculate remainder from the signed division of its two operands. 
-- '<result> = srem <ty> <op1>, <op2>'
PROCEDURE EmitSRem *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = srem %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitSRem;  

--------------------------------------------------------------------------------
-- Emits instruction to calculate the remainder from the unsigned division of its two arguments. 
-- '<result> = urem <ty> <op1>, <op2>'
PROCEDURE EmitURem *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = urem %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitURem;  


--------------------------------------------------------------------------------
TYPE
  ICmpCond *= (
    cnd_eq  -- equal
  , cnd_ne  -- not equal
  , cnd_ugt -- unsigned greater than
  , cnd_uge -- unsigned greater or equal
  , cnd_ult -- unsigned less than
  , cnd_ule -- unsigned less or equal
  , cnd_sgt -- signed greater than
  , cnd_sge -- signed greater or equal
  , cnd_slt -- signed less than
  , cnd_sle -- signed less or equal
  );
  
TYPE  
  ICmpCondNameArray = ARRAY ICmpCond OF ARRAY 4 OF CHAR;  
  
CONST
  ICmpCondName = ICmpCondNameArray {
    "eq"  -- equal
  , "ne"  -- not equal
  , "ugt" -- unsigned greater than
  , "uge" -- unsigned greater or equal
  , "ult" -- unsigned less than
  , "ule" -- unsigned less or equal
  , "sgt" -- signed greater than
  , "sge" -- signed greater or equal
  , "slt" -- signed less than
  , "sle" -- signed less or equal
  };  
  
--------------------------------------------------------------------------------
-- Emits comparison of its integer operands.
-- '<result> = icmp <cond> <ty> <op1>, <op2>' 
PROCEDURE EmitICmp *(cond: ICmpCond; res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = icmp %s %s %s, %s", res.name^, ICmpCondName[cond], op1.type^, op1.name^, op2.name^);
END EmitICmp;  

--------------------------------------------------------------------------------
TYPE
  FCmpCond *= (
    fcnd_false -- no comparison, always returns false
  , fcnd_oeq   -- ordered and equal
  , fcnd_ogt   -- ordered and greater than
  , fcnd_oge   -- ordered and greater than or equal
  , fcnd_olt   -- ordered and less than
  , fcnd_ole   -- ordered and less than or equal
  , fcnd_one   -- ordered and not equal
  , fcnd_ord   -- ordered (no nans)
  , fcnd_ueq   -- unordered or equal
  , fcnd_ugt   -- unordered or greater than
  , fcnd_uge   -- unordered or greater than or equal
  , fcnd_ult   -- unordered or less than
  , fcnd_ule   -- unordered or less than or equal
  , fcnd_une   -- unordered or not equal
  , fcnd_uno   -- unordered (either nans)
  , fcnd_true  -- no comparison, always returns true  
  );  

TYPE  
  FCmpCondNameArray = ARRAY FCmpCond OF ARRAY 8 OF CHAR;  
  
CONST
  FCmpCondName = FCmpCondNameArray {
    "false" -- no comparison, always returns false
  , "oeq"   -- ordered and equal
  , "ogt"   -- ordered and greater than
  , "oge"   -- ordered and greater than or equal
  , "olt"   -- ordered and less than
  , "ole"   -- ordered and less than or equal
  , "one"   -- ordered and not equal
  , "ord"   -- ordered (no nans)
  , "ueq"   -- unordered or equal
  , "ugt"   -- unordered or greater than
  , "uge"   -- unordered or greater than or equal
  , "ult"   -- unordered or less than
  , "ule"   -- unordered or less than or equal
  , "une"   -- unordered or not equal
  , "uno"   -- unordered (either nans)
  , "true"  -- no comparison, always returns true  
  };  

--------------------------------------------------------------------------------
-- Emits comparison of its floating-point operands. 
-- '<result> = fcmp <cond> <ty> <op1>, <op2>'
PROCEDURE EmitFCmp *(cond: FCmpCond; res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = fcmp %s %s %s, %s", res.name^, FCmpCondName[cond], op1.type^, op1.name^, op2.name^);
END EmitFCmp;  


--------------------------------------------------------------------------------
-- Emits unconditional branch 'br label <dest>' 
-- @param[in]: dest - target label to pass control       
PROCEDURE EmitBranch *(dest-: ARRAY OF CHAR);
BEGIN
  cs.GenInstr("br label %%%s", dest);
END EmitBranch;  

--------------------------------------------------------------------------------
-- Emits conditional branch 
-- 'br i1 <cond>, label <iftrue>, label <iffalse>'
-- @param[in]: cond - a single 'i1' value       
-- @param[in]: iftrue - target label to pass control if 'cond' is true       
-- @param[in]: iffalse - target label to pass control if 'cond' is false       
PROCEDURE EmitCondBranch *(cond: Arg; iftrue-, iffalse-: ARRAY OF CHAR);
BEGIN
  cs.GenInstr("br i1 %%%s, label %%%s, label %%%s", cond.name^, iftrue, iffalse);
END EmitCondBranch;  


--------------------------------------------------------------------------------
-- Emits instruction to write to memory. 
-- 'store [volatile] <ty> <value>, <ty>* <pointer>[, align <alignment>]' 
PROCEDURE EmitStore *(ptr, value: Arg);
BEGIN
  IF ptr.align = 0 THEN
    cs.GenInstr("store %s %s, %s* %s", value.type^, value.name^, value.type^, ptr.name^);
  ELSE  
    cs.GenInstr("store %s %s, %s* %s, align %d", value.type^, value.name^, value.type^, ptr.name^, ptr.align);
  END;
END EmitStore;  

--------------------------------------------------------------------------------
-- Emits instruction to read from memory.
-- '<result> = load [volatile] <ty>, <ty>* <pointer>[, align <alignment>]' 
PROCEDURE EmitLoad *(res, ptr: Arg);
BEGIN
  IF ptr.align = 0 THEN
    cs.GenInstr("%s = load %s, %s* %s", res.name^, res.type^, res.type^, ptr.name^);
  ELSE  
    cs.GenInstr("%s = load %s, %s* %s, align %d", res.name^, res.type^, res.type^, ptr.name^, ptr.align);
  END;  
END EmitLoad;  


--------------------------------------------------------------------------------
-- Emits instruction to call function.
-- '<result> = call <ty> <fnptrval> (<function args>)'
PROCEDURE EmitProcCall *(res: Arg; name-: ARRAY OF CHAR; args: ArgList);
VAR i: LONGINT;
    args_str: ArgListStr; 
BEGIN
  args_str := "";
  IF args # NIL THEN
    FOR i := 0 TO LEN(args^)-1 DO
      IF i # 0 THEN fmt.append(args_str, ", ")  END;
      fmt.append(args_str, "%s %s",  args[i].type^, args[i].name^);
    END;
  END;
  IF res.name^ = "" THEN
    cs.GenInstr("call %s %s(%s)", res.type^, name, args_str);
  ELSE
    cs.GenInstr("%s = call %s %s(%s)", res.name^, res.type^, name, args_str);
  END;
END EmitProcCall;  

--------------------------------------------------------------------------------
-- Emits instruction to  return control flow (and optionally a value) 
-- from a function back to the caller.
-- 'ret <type> <value>'
PROCEDURE EmitReturn *(value: Arg);
BEGIN  
  cs.GenInstr("ret %s %s", value.type^, value.name^);
END EmitReturn;  


--------------------------------------------------------------------------------
-- Emits instruction to convert the pointer to the integer type 'ty2'. 
-- '<result> = ptrtoint <ty> <value> to <ty2>'
PROCEDURE EmitPtrToInt *(res, value: Arg);
BEGIN
  cs.GenInstr("%s = ptrtoint %s %s to %s", res.name^, value.type^, value.name^, res.type^);
END EmitPtrToInt;  

--------------------------------------------------------------------------------
-- Emits instruction to convert an integer 'value' to a pointer type, 'ty2'. 
-- <result> = inttoptr <ty> <value> to <ty2>
PROCEDURE EmitIntToPtr *(res, value: Arg);
BEGIN
  cs.GenInstr("%s = inttoptr %s %s to %s", res.name^, value.type^, value.name^, res.type^);
END EmitIntToPtr;  

--------------------------------------------------------------------------------
-- Emits instruction to truncate its operand to the type 'ty2'. Both types must be of integer types.
-- <result> = trunc <ty> <value> to <ty2>
PROCEDURE EmitTrunc *(res, value: Arg);
BEGIN
  cs.GenInstr("%s = trunc %s %s to %s", res.name^, value.type^, value.name^, res.type^);
END EmitTrunc;  

--------------------------------------------------------------------------------
-- Emits instruction to truncate its operand to the type requited for the branch instruction. 
-- The type must be of integer types.
-- <result> = trunc <ty> <value>
PROCEDURE EmitTruncForBranch *(res, value: Arg);
BEGIN
  EmitTrunc(res, value);
END EmitTruncForBranch;  

--------------------------------------------------------------------------------
-- Emits instruction to truncate 'value' to the type 'ty2'. Both types must 
-- be of floating-point types. The size of value must be larger than the size of ty2.
-- <result> = fptrunc <ty> <value> to <ty2>
PROCEDURE EmitFPTrunc *(res, value: Arg);
BEGIN
  cs.GenInstr("%s = fptrunc %s %s to %s", res.name^, value.type^, value.name^, res.type^);
END EmitFPTrunc;  

--------------------------------------------------------------------------------
-- Emits instruction to zero extends 'value' to type 'ty2'. 
-- <result> = zext <ty> <value> to <ty2>
PROCEDURE EmitZExt *(res, value: Arg);
BEGIN
  cs.GenInstr("%s = zext %s %s to %s", res.name^, value.type^, value.name^, res.type^);
END EmitZExt;  

--------------------------------------------------------------------------------
-- Emits instruction to sign extends 'value' to type 'ty2'. 
-- <result> = sext <ty> <value> to <ty2>
PROCEDURE EmitSExt *(res, value: Arg);
BEGIN
  cs.GenInstr("%s = sext %s %s to %s", res.name^, value.type^, value.name^, res.type^);
END EmitSExt;  

--------------------------------------------------------------------------------
-- Emits instruction to extend a floating-point 'value' to a larger floating-point value.
-- The source type must be smaller than the destination type.
-- <result> = fpext <ty> <value> to <ty2>
PROCEDURE EmitFPExt *(res, value: Arg);
BEGIN
  cs.GenInstr("%s = fpext %s %s to %s", res.name^, value.type^, value.name^, res.type^);
END EmitFPExt;  

--------------------------------------------------------------------------------
-- Emits instruction to convert a floating-point 'value' to its unsigned integer
-- equivalent of type 'ty2'.
-- <result> = fptoui <ty> <value> to <ty2>
PROCEDURE EmitFPToUI *(res, value: Arg);
BEGIN
  cs.GenInstr("%s = fptoui %s %s to %s", res.name^, value.type^, value.name^, res.type^);
END EmitFPToUI;  

--------------------------------------------------------------------------------
-- Emits instruction to convert a floating-point 'value' to its signed integer
-- equivalent of type 'ty2'.
-- <result> = fptosi <ty> <value> to <ty2>
PROCEDURE EmitFPToSI *(res, value: Arg);
BEGIN
  cs.GenInstr("%s = fptosi %s %s to %s", res.name^, value.type^, value.name^, res.type^);
END EmitFPToSI;  

--------------------------------------------------------------------------------
-- Emits instruction to convert a unsigned 'value' to floating-point type 'ty2'.
-- <result> = uitofp <ty> <value> to <ty2>
PROCEDURE EmitUIToFP *(res, value: Arg);
BEGIN
  cs.GenInstr("%s = uitofp %s %s to %s", res.name^, value.type^, value.name^, res.type^);
END EmitUIToFP;  

--------------------------------------------------------------------------------
-- Emits instruction to convert a signed 'value' to floating-point type 'ty2'.
-- <result> = sitofp <ty> <value> to <ty2>
PROCEDURE EmitSIToFP *(res, value: Arg);
BEGIN
  cs.GenInstr("%s = sitofp %s %s to %s", res.name^, value.type^, value.name^, res.type^);
END EmitSIToFP;  

--------------------------------------------------------------------------------
-- Emits instruction to convert 'value' to type 'ty2' without changing any bits. 
-- <result> = bitcast <ty> <value> to <ty2>
PROCEDURE EmitBitCast *(res, value: Arg);
BEGIN
  cs.GenInstr("%s = bitcast %s %s to %s", res.name^, value.type^, value.name^, res.type^);
END EmitBitCast;  


--------------------------------------------------------------------------------
-- Emits instruction to get the address of a subelement of an aggregate data structure.
-- '<result> = getelementptr <ty>, <ty>* <ptrval>{, [inrange] <ty> <idx>}*
PROCEDURE EmitGetElemetPrt *(res, prtval: Arg; idx: ArgList);
VAR i: LONGINT;
    idx_str: ArgListStr; 
    base_type: nms.String;
BEGIN
  idx_str := "";
  FOR i := 0 TO LEN(idx^)-1 DO
    IF i # 0 THEN fmt.append(idx_str, ", ")  END;
    fmt.append(idx_str, "%s %s",  idx[i].type^, idx[i].name^);
  END;
  nms.GetPointerBase(base_type, prtval.type^);
  cs.GenInstr("%s = getelementptr %s, %s* %s, %s", res.name^, base_type, base_type, prtval.name^, idx_str);
END EmitGetElemetPrt;  


--------------------------------------------------------------------------------
-- Emits SSA phi function.
-- '<result> = phi <ty> [ <val0>, <label0>], ...'
PROCEDURE EmitPhi *(res: Arg; preds: ArgList);
VAR i: LONGINT;
    preds_str: ArgListStr; 
BEGIN
  preds_str := "";
  FOR i := 0 TO LEN(preds^)-1 DO
    IF i # 0 THEN fmt.append(preds_str, ", ")  END;
    fmt.append(preds_str, "[%s, %%%s]",  preds[i].name^, preds[i].type^);
  END;
  cs.GenInstr("%s = phi %s %s", res.name^, res.type^, preds_str);
END EmitPhi;


--------------------------------------------------------------------------------
-- Emits instruction to shift the first operand to the left a specified number of bits.
-- '<result> = shl <ty> <op1>, <op2>'
PROCEDURE EmitShl *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = shl %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitShl; 

--------------------------------------------------------------------------------
-- Emits instruction to logical shift the first operand to 
-- the right a specified number of bits with zero fill.
-- '<result> = lshr <ty> <op1>, <op2>'
PROCEDURE EmitLshr *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = lshr %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitLshr; 

--------------------------------------------------------------------------------
-- Emits instruction to arithmetic shift the first operand to 
-- the right a specified number of bits with zero fill.
-- '<result> = lshr <ty> <op1>, <op2>'
PROCEDURE EmitAshr *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = ashr %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitAshr; 

--------------------------------------------------------------------------------
-- Emits instruction to bitwise logical and of its two operands.
-- '<result> = and <ty> <op1>, <op2> '
PROCEDURE EmitAnd *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = and %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitAnd; 

--------------------------------------------------------------------------------
-- Emits instruction to bitwise logical inclusive or of its two operands.
-- '<result> = or <ty> <op1>, <op2> '
PROCEDURE EmitOr *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = or %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitOr; 

--------------------------------------------------------------------------------
-- Emits instruction to bitwise logical exclusive or of its two operands.
-- '<result> = xor <ty> <op1>, <op2> '
PROCEDURE EmitXor *(res, op1, op2: Arg);
BEGIN
  cs.GenInstr("%s = xor %s %s, %s", res.name^, res.type^, op1.name^, op2.name^);
END EmitXor; 

--------------------------------------------------------------------------------
-- Emits instruction to move its integer type operand.
-- '<result> = or <ty> <op1>, <op2>'
PROCEDURE EmitMove *(res, op1: Arg);
BEGIN
  EmitOr(res, op1, op1);
END EmitMove; 


--------------------------------------------------------------------------------
TYPE
  CaseVariant *= POINTER TO CaseVariantDesc;
  CaseVariantDesc *= RECORD
    value -: String;  -- case variant 
    label -: String;  -- case variant body
    next  -: CaseVariant;
  END;

--------------------------------------------------------------------------------
PROCEDURE NewCaseVariant *(value-, label-: ARRAY OF CHAR; next: CaseVariant): CaseVariant;
VAR case_var: CaseVariant;
BEGIN
  NEW(case_var);
  DStrings.Assign(value, case_var.value);
  DStrings.Assign(label, case_var.label);
  case_var.next := next;
  RETURN case_var;
END NewCaseVariant;  

--------------------------------------------------------------------------------
-- Emits the 'switch' instruction is used to transfer control flow to one of 
-- several different places.
-- 'switch <intty> <value>, label <defaultdest> [ <intty> <val>, label <dest> ... ]'
PROCEDURE EmitSwitch *(value: Arg; default-: ARRAY OF CHAR; case_list: CaseVariant);
VAR instr: String;
    str: ARRAY 1024 OF CHAR;
BEGIN
  fmt.print(str, "switch %s %s, label %%%s [\n", value.type^, value.name^, default);
  DStrings.Assign(str, instr);
  WHILE case_list # NIL DO
    fmt.print( str, formLLVM.CODE_INDENT + "  %s %s, label %%%s\n"
             , value.type^, case_list.value^, case_list.label^ );
    DStrings.Append(str, instr);
    case_list := case_list.next;
  END;
  DStrings.Append(formLLVM.CODE_INDENT + "]", instr);
  cs.GenInstrExt(instr^);
END EmitSwitch;


--------------------------------------------------------------------------------
-- Emits indication that the code after a no-return function cannot be reached.
-- 'unreachable'
PROCEDURE EmitUnreachable *();
BEGIN
  cs.GenInstr("unreachable");
END EmitUnreachable;


--------------------------------------------------------------------------------
-- Emits intrinsic call to provide information about a local element.
-- 'call void @llvm.dbg.addr(metadata <ty> <value>, metadata <desc>, metadata <expr>)'
PROCEDURE EmitDbgLocalAddr *(local_no: ir.Local; addr: Arg); 
BEGIN
  IF fc.GenDebug() & (local_no # ir.UndefLocal) 
  & NOT(ir.o_ParameterLen IN ir.Locals[local_no].Options)
  & (ir.Locals[local_no].Obj # NIL)
  & ir.Locals[local_no].Obj.is_local()
  & dbgLLVM.IsTypeSupported(ir.Locals[local_no].Obj.type) 
  THEN 
    cs.GenDbgCall(local_no, dbgLLVM.FORMAT_LocalVarAddr, addr.type^, addr.name^);
  END;  
END EmitDbgLocalAddr; 


END llvmEmitInstr.