--------------------------------------------------------------------------------
--                          Excelsior LLVM Compiler
--                         (c) 2020, Excelsior, LLC.
-- Module:   llvmNames
-- Mission:  LLVM names generator 
-- Synonym:  nms
-- Authors:  Lvov Konstantin
-- Created:  10-Mar-2020
--------------------------------------------------------------------------------
MODULE llvmNames;

IMPORT sys := SYSTEM;

IMPORT Strings;
IMPORT DStrings;
IMPORT fmt := FormStr;

IMPORT pcO;
IMPORT pc := pcK;

IMPORT ir;
IMPORT Calc;
IMPORT opDef;
IMPORT TestIO;
IMPORT ObjNames;
IMPORT prc := opProcs;

IMPORT llvmDefs;

--------------------------------------------------------------------------------
CONST 
  BITS_IN_BYTE *= 8;

TYPE
  String *= ARRAY 256 OF CHAR;

--------------------------------------------------------------------------------
CONST
  ByteSize *= "8";
  ByteT    *= "i" + ByteSize;

  WordSize *= "32";
  WordT    *= "i" + WordSize;
  
  BoolSize *= "1";
  BoolT    *= "i" + BoolSize;

  Null     *= "null";
    
--------------------------------------------------------------------------------
PROCEDURE IsProc *(obj: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (obj.mode IN pc.PROCs) OR (obj.mode = pc.ob_module);
END IsProc;

--------------------------------------------------------------------------------
PROCEDURE IsModuleEntry *(obj: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (obj.mode = pc.ob_module);
END IsModuleEntry;


--------------------------------------------------------------------------------
PROCEDURE isProcParamReference *(proto: prc.Proto; parm_no: ir.ProtoParNumber): BOOLEAN;
CONST BY_REF = sys.SET32{ORD(prc.by_ref), ORD(prc.by_ROref)}; 
BEGIN
  RETURN (proto.par[parm_no].mode = prc.pm_param)
       & (proto.par[parm_no].ind IN BY_REF); 
END isProcParamReference;

--------------------------------------------------------------------------------
-- Does the local correspond to the parameter used to pass the length of an open array parameter. 
PROCEDURE isLocalParamLen (local_no: ir.Local): BOOLEAN;
BEGIN
  RETURN (ir.o_ParameterLen IN ir.Locals[local_no].Options);
END isLocalParamLen;

--------------------------------------------------------------------------------
PROCEDURE isLocalParamReference (local_no: ir.Local): BOOLEAN;
VAR obj: pc.OBJECT;
BEGIN
  obj := ir.Locals[local_no].Obj;
  RETURN (ir.o_Parameters IN ir.Locals[local_no].Options)
       & NOT isLocalParamLen(local_no)
       & (obj.type.mode # pc.ty_array_of) 
       & (  (obj.mode = pc.ob_varpar) 
         OR ((ir.Locals[local_no].VarType = ir.t_ref) & ~(obj.type.mode IN pc.ADRs))
         );
END isLocalParamReference;


--------------------------------------------------------------------------------
PROCEDURE GetProcPrototype *(type: pc.STRUCT): prc.Proto; 
VAR proto: prc.Proto;
    proto_num: prc.ProtoNum;
BEGIN
  proto := prc.ProcProto(type);
  IF proto = NIL THEN
    proto_num := opDef.prototype(type);
    proto := prc.ProtoList[proto_num];
  END;
  RETURN proto;
END GetProcPrototype; 

--------------------------------------------------------------------------------
-- Определить естественное выравнивание значения. 
-- @param[in] value - значение для которого необходимо определить естественное выравнивание.
-- @param[in] max_align - верхняя граница для естественного выравнивания.
-- @return естественное выравнивание. 
PROCEDURE GetValueAlignment *(value: LONGINT; max_align: ir.AlignType): ir.AlignType; 
VAR align: ir.AlignType;
BEGIN
  align := max_align;
  WHILE (value MOD align) # 0 DO
    align := align DIV 2;
  END;
  RETURN align;
END GetValueAlignment; 

--------------------------------------------------------------------------------
PROCEDURE GetObjName *(VAR name: ARRAY OF CHAR; obj: pc.OBJECT);
VAR obj_name: String;
BEGIN
  ObjNames.makename(obj, obj_name);
  IF (obj.is_global() OR (obj.mode IN pc.PROCs)) & (obj.mode # pc.ob_type) THEN
    fmt.print(name, "@%s", obj_name);
  ELSE  
    fmt.print(name, "%%%s", obj_name);
  END;
END GetObjName;

--------------------------------------------------------------------------------
PROCEDURE GetVarName *(VAR name: ARRAY OF CHAR; var_num: ir.VarNum); 
BEGIN
  ASSERT( var_num # ir.UNDEFINED ); 
  fmt.print(name, "%%tmp.%d", var_num);
END GetVarName; 

--------------------------------------------------------------------------------
PROCEDURE GetLocalName *(VAR name: ARRAY OF CHAR; local_no: ir.Local);
VAR obj: pc.OBJECT;
    local_name: String;
BEGIN
  obj := ir.Locals[local_no].Obj;
  IF (obj # NIL) & ~(ir.o_Parameters IN ir.Locals[local_no].Options) THEN
    GetObjName(name, obj);
  ELSE
    COPY(ir.Locals[local_no].Name^, local_name);
    ObjNames.replace_non_alpha(local_name);
    fmt.print(name, "%%%s", local_name);
  END;   
END GetLocalName;  

--------------------------------------------------------------------------------
PROCEDURE IsPointer *(type-: ARRAY OF CHAR): BOOLEAN; 
VAR i: LONGINT;
BEGIN
  i := LENGTH(type) - 1;
  RETURN type[i] = "*";
END IsPointer; 

--------------------------------------------------------------------------------
PROCEDURE GetPointerTo *(VAR ptr_type: ARRAY OF CHAR; type-: ARRAY OF CHAR);
BEGIN
  fmt.print(ptr_type, "%s*", type);    
END GetPointerTo;  

--------------------------------------------------------------------------------
PROCEDURE GetPointerBase *(VAR base_type: ARRAY OF CHAR; type-: ARRAY OF CHAR);
VAR i: LONGINT;
BEGIN
  i := LENGTH(type) - 1;
  ASSERT( type[i] = "*" );
  fmt.print(base_type, "%s", type);
  Strings.Delete(base_type, i, 1);    
END GetPointerBase;  


--------------------------------------------------------------------------------
PROCEDURE TagSizeToType *(type: ir.TypeType; size: LONGINT): pc.STRUCT; 
BEGIN 
  CASE type OF
  | ir.t_void:
      RETURN pcO.void;
  | ir.t_ref:
      RETURN pcO.addr;
  | ir.t_int:
      CASE size OF
      | 1: RETURN pcO.shortint;
      | 2: RETURN pcO.integer;
      | 4: RETURN pcO.longint;
      | 8: RETURN pcO.longlongint;
      ELSE
      END;
  | ir.t_unsign:
      CASE size OF
      | 1: RETURN pcO.shortcard;
      | 2: RETURN pcO.cardinal;
      | 4: RETURN pcO.longcard;
      | 8: RETURN pcO.longlongcard;
      ELSE
      END;
  | ir.t_float:
      CASE size OF
      | 4: RETURN pcO.real;
      | 8: RETURN pcO.longreal;
      ELSE RETURN pcO.ld_real;
      END;
  | ir.t_complex:    
      CASE size OF
      | 4: RETURN pcO.complex;
      | 8: RETURN pcO.lcomplex;
      END;
  ELSE
      RETURN pcO.invtype;     
  END;
  RETURN pcO.invtype;
END TagSizeToType; 


--------------------------------------------------------------------------------
PROCEDURE GetTagTypeName *(VAR name: ARRAY OF CHAR; type: ir.TypeType; size: LONGINT);
BEGIN
  CASE type OF
  | ir.t_void:    
      COPY("void", name);
  
  | ir.t_int
  , ir.t_unsign:  
      fmt.print(name, "i%d", size * BITS_IN_BYTE);
  
  | ir.t_ref:
      GetPointerTo(name, ByteT);     
  
  | ir.t_float:  
      CASE size OF
      | 4: COPY("float", name);
      | 8: COPY("double", name);
      END; 
  END;
END GetTagTypeName;

--------------------------------------------------------------------------------
PROCEDURE ^ GetTypeName (VAR name: ARRAY OF CHAR; type: pc.STRUCT);

PROCEDURE GetProcTypeName *(VAR name: ARRAY OF CHAR; type: pc.STRUCT);
VAR i: LONGINT;
    proto: prc.Proto;
    arg_type_name: String;
BEGIN
  IF (type.obj # NIL) & (type.obj.mode = pc.ob_type) THEN
    GetObjName(name, type.obj);
    RETURN;
  END;
  
  proto := GetProcPrototype(type);
  ASSERT( proto # NIL );

  IF proto.ret_type = ir.t_void THEN
    fmt.print(name, "void (");
  ELSE  
    GetTypeName(arg_type_name, type.base);                           
    fmt.print(name, "%s (", arg_type_name);
  END;
  
  FOR i := 0 TO proto.npar - 1 DO
    IF proto.par[i].obj # NIL THEN
      GetTypeName(arg_type_name, proto.par[i].obj.type);
    ELSE  
      GetTagTypeName(arg_type_name, proto.par[i].type, proto.par[i].size);
    END;
    IF i = 0 THEN
      fmt.append(name, arg_type_name);
    ELSE  
      fmt.append(name, ", %s", arg_type_name);
    END;
  END;

  fmt.append(name, ")*");
END GetProcTypeName; 

--------------------------------------------------------------------------------
PROCEDURE GetTypeName *(VAR name: ARRAY OF CHAR; type: pc.STRUCT);
VAR base_type_name: String;
BEGIN
  CASE type.mode OF
  | pc.ty_void:       
      COPY("void", name);
  
  | pc.ty_shortcard, pc.ty_shortint, pc.ty_char, pc.ty_loc:    
      COPY("i8", name);
  
  | pc.ty_cardinal, pc.ty_integer:
      COPY("i16", name);
  
  | pc.ty_longcard, pc.ty_longint:
      COPY("i32", name);
  
  | pc.ty_longlongint, pc.ty_longlongcard:
      COPY("i64", name);
  
  | pc.ty_real:
      COPY("float", name);
      
  | pc.ty_longreal:
      COPY("double", name);
  
  | pc.ty_boolean, pc.ty_range, pc.ty_enum, pc.ty_protection :
      GetTypeName(name, type.base);

  | pc.ty_record:
      GetObjName(name, type.obj);

  | pc.ty_SS:
      GetTypeName(base_type_name, type.base);                           
      fmt.print(name, "[%d x %s]", type.len, base_type_name);
  
  | pc.ty_array:
      IF (type.base.mode = pc.ty_loc) & (type.len = 2) THEN 
        COPY("i16", name);
      ELSIF (type.base.mode = pc.ty_loc) & (type.len = 4) THEN 
        COPY("i32", name);
      ELSIF type.obj # NIL THEN
        GetObjName(name, type.obj);
      ELSE
        GetTypeName(base_type_name, type.base);                           
        fmt.print(name, "[%d x %s]", type.len, base_type_name);
      END;
      
  | pc.ty_array_of:    
      GetTypeName(base_type_name, type.base);                           
      fmt.print(name, "%s*", base_type_name);
      
  | pc.ty_pointer
  , pc.ty_AA:
      GetTypeName(base_type_name, type.base);                           
      fmt.print(name, "%s*", base_type_name);
  
  | pc.ty_set:    
      IF type.inx # NIL THEN
        GetTypeName(name, type.inx);
      ELSIF type.len <= 8 THEN
        COPY("i8", name);
      ELSIF type.len <= 16 THEN
        COPY("i16", name);
      ELSIF type.len <= 32 THEN 
        COPY("i32", name);
--      ELSIF type.len <= tune.BITSET_LEN_scalar THEN  
--        COPY("i64", name);
      ELSE                             
        ASSERT( FALSE );  -- long set is not supported
      END;
      
  | pc.ty_proctype:
      GetProcTypeName(name, type);
        
  END;
END GetTypeName;  

--------------------------------------------------------------------------------
PROCEDURE GetIndexTypeName *(VAR name: ARRAY OF CHAR);
BEGIN
  GetTypeName(name, pc.longint_type);
END GetIndexTypeName;  

--------------------------------------------------------------------------------
PROCEDURE GetLocalTypeName *(VAR name: ARRAY OF CHAR; local_no: ir.Local);
VAR obj: pc.OBJECT;
BEGIN
  obj := ir.Locals[local_no].Obj;
  IF (obj # NIL) & (obj.type # NIL) & NOT isLocalParamLen(local_no) THEN
    GetTypeName(name, obj.type);
    IF isLocalParamReference(local_no) THEN
      GetPointerTo(name, name);
    END; 
  ELSE
    GetTagTypeName(name, ir.Locals[local_no].VarType, ir.Locals[local_no].VarSize);
  END;
END GetLocalTypeName;  

--------------------------------------------------------------------------------
PROCEDURE GetParamTypeName *(VAR name: ARRAY OF CHAR; p: ir.ParamPtr);
BEGIN
  IF p.type # NIL THEN
    GetTypeName(name, p.type);
    RETURN;
  END; 
  
  CASE p.triade.Op OF
  | ir.o_loadr: 
      IF p.paramnumber = 0 THEN
        GetTagTypeName(name, p.triade.ResType, p.triade.ResSize);
        RETURN; 
      END;
  
  | ir.o_storer: 
      IF p.paramnumber = 0 THEN
        GetTagTypeName(name, p.triade.OpType, p.triade.OpSize); 
        RETURN; 
      END;
  
  | ir.o_getelementptr: 
      IF p.paramnumber > 0 THEN
        GetIndexTypeName(name); 
        RETURN; 
      END;
  ELSE
  END;
  GetTagTypeName(name, ir.ParamType(p.triade, p.paramnumber), ir.ParamSize(p.triade, p.paramnumber));
END GetParamTypeName;


--------------------------------------------------------------------------------
PROCEDURE GetTypeAlign *(type: pc.STRUCT; bySize: BOOLEAN): LONGINT;
BEGIN
  RETURN opDef.type_align(type);
END GetTypeAlign;

--------------------------------------------------------------------------------
PROCEDURE GetLocalAlign *(local_no: ir.Local): LONGINT;
BEGIN
  RETURN ir.Locals[local_no].Align;
END GetLocalAlign;  

--------------------------------------------------------------------------------
PROCEDURE GetProcReturnTypeName *(VAR name: ARRAY OF CHAR; obj: pc.OBJECT);
VAR proto: prc.Proto;
BEGIN
  IF (obj.type = NIL) OR IsModuleEntry(obj) THEN
    proto := prc.ProtoList[prc.ProcProtoNum(prc.ProcNumByObj(obj))];
    GetTagTypeName(name, proto.ret_type, proto.ret_size);
  ELSE
    GetTypeName(name, obj.type.base);
  END;
END GetProcReturnTypeName;  

--------------------------------------------------------------------------------
PROCEDURE GetProcParamType *( VAR name: ARRAY OF CHAR
                            ; proto_no: prc.ProtoNum
                            ; param_no: ir.ProtoParNumber );
VAR proto: prc.Proto;
BEGIN
  proto := prc.ProtoList[proto_no];
  IF (proto.par[param_no].obj # NIL) & (proto.par[param_no].obj.type # NIL) THEN
    GetTypeName(name, proto.par[param_no].obj.type);  
  ELSE  
    GetTagTypeName(name, proto.par[param_no].type, proto.par[param_no].size);  
  END;
END GetProcParamType;  


--------------------------------------------------------------------------------
PROCEDURE GetProcParamName *(VAR name: ARRAY OF CHAR; param_no: LONGINT);
BEGIN
  fmt.print(name, "%%%d", param_no);  
END GetProcParamName;  


--------------------------------------------------------------------------------
PROCEDURE ValueToHexString (VAR str: ARRAY OF CHAR; value: ARRAY OF sys.BYTE);
VAR i: LONGINT;
BEGIN 
  fmt.print(str, "0x");
  FOR i := LEN(value)-1  TO  0  BY -1 DO
    fmt.append(str, "%.2x", sys.VAL(sys.CARD8, value[i]));
  END;    
END ValueToHexString;

--------------------------------------------------------------------------------
PROCEDURE RealToLongReal(v: REAL): LONGREAL;
BEGIN 
  RETURN VAL(LONGREAL, v);
END RealToLongReal;

--------------------------------------------------------------------------------
PROCEDURE RealValueToStr *(VAR str: ARRAY OF CHAR; value: pc.VALUE; single: BOOLEAN); 
VAR lr: LONGREAL;
BEGIN 
  lr := value.get_real();
  IF single THEN
    lr := RealToLongReal(VAL(REAL, lr));  -- convert REAL to LLVM format 
  END;
  ValueToHexString(str, lr); 
END RealValueToStr; 

--------------------------------------------------------------------------------
PROCEDURE ValueToStr *(VAR str: ARRAY OF CHAR; value: pc.VALUE); 
BEGIN 
  value.value_to_str(str, pc.flag_m2);
END ValueToStr; 

--------------------------------------------------------------------------------
PROCEDURE IntToStr *(VAR str: ARRAY OF CHAR; value: LONGINT); 
BEGIN 
  fmt.print(str, "%d", value);  
END IntToStr; 

--------------------------------------------------------------------------------
PROCEDURE CardToStr *(VAR str: ARRAY OF CHAR; value: sys.CARD32); 
BEGIN 
  fmt.print(str, "%u", value);  
END CardToStr; 

--------------------------------------------------------------------------------
PROCEDURE GetTriadeDbgStr *(i: LONGINT; tr: ir.TriadePtr): DStrings.String;
VAR res, triade_info: DStrings.String;
    str: ARRAY 32 OF CHAR;
BEGIN
  fmt.print(str, " %d.%d", tr.NodeNo, i);
  DStrings.Assign(str, res);
  triade_info := TestIO.GetTriadeInfo(tr);
  DStrings.Append(triade_info^, res);
  RETURN res;
END GetTriadeDbgStr;

--------------------------------------------------------------------------------
PROCEDURE Par2Cardinal *(p: ir.ParamPtr): sys.CARD32;
VAR sz: ir.SizeType;
BEGIN
  ASSERT( p.tag = ir.y_NumConst );
  sz := ir.ParamSize(p.triade, p.paramnumber);
  RETURN Calc.ToCardinal(p.value, sz);
END Par2Cardinal;

--------------------------------------------------------------------------------
PROCEDURE Par2Integer *(p: ir.ParamPtr) : LONGINT;
VAR sz: ir.SizeType;
BEGIN
  ASSERT( p.tag = ir.y_NumConst );
  sz := ir.ParamSize(p.triade, p.paramnumber);
  RETURN Calc.ToInteger(p.value, sz);
END Par2Integer;


--------------------------------------------------------------------------------
BEGIN
  llvmDefs.GetTypeName    := GetTypeName; 
  llvmDefs.GetTagTypeName := GetTagTypeName;
END llvmNames.