--------------------------------------------------------------------------------
--                      Excelsior XDS Test Coverage System
--                          (c) 2015, Excelsior Ltd.
-- Module:   tcObjs
-- Mission:  Test Coverage Object Operations.  
-- Synonym:  tco
-- Authors:  Lvov Konstantin
-- Created:  11-Dec-2002
--
-- It is used to hide 'pcO' and other front-end modules.
--------------------------------------------------------------------------------
<* IF NOT DEFINED(dbg_tcs) THEN *>  <* NEW dbg_tcs- *> <* END *>
<* IF NOT DEFINED(TARGET_VAX_OLD) THEN *>  <* NEW TARGET_VAX_OLD- *> <* END *>
<* +o2addkwd *>

MODULE tcObjs;

<* IF    TARGET_386  THEN *> FROM  xrTCSx86   IMPORT  TestConditionType;
<* ELSIF TARGET_PPC   THEN *> FROM  xrTCSppc   IMPORT  TestConditionType;
<* ELSIF TARGET_MIPS THEN *> FROM  xrTCSmips  IMPORT  TestConditionType;
<* ELSIF TARGET_VAX  THEN *> FROM  xrTCSvax   IMPORT  TestConditionType;
<* ELSIF TARGET_SPARC THEN *> FROM  xrTCSsparc IMPORT  TestConditionType;
<* END *>

IMPORT  pc := pcK,             pcF,               sys := SYSTEM
     ,  pcO,                   pcB,               env := xiEnv
     ,  fe := tcFEproxy,       xcStr,             tcc := tcConfig
     ,  pcS
<* IF    TARGET_386  THEN *>,  tc := xrTCSx86
<* ELSIF TARGET_PPC   THEN *>,  tc := xrTCSppc
<* ELSIF TARGET_MIPS THEN *>,  tc := xrTCSmips
<* ELSIF TARGET_VAX  THEN *>,  tc := xrTCSvax
<* ELSIF TARGET_SPARC THEN *>,  tc := xrTCSsparc
<* END *>
     ;

IMPORT DStrings;
IMPORT xfs := xiFiles;

FROM  pcO     IMPORT  ZZ_type;
<* IF TARGET_VAX_OLD THEN *>
FROM  pcO     IMPORT  cur_mod;
<* ELSE *>
FROM  pcK     IMPORT  cur_mod;
<* END *>

--------------------------------------------------------------------------------
TYPE
  INT = sys.INT32;

  TSourceRefs * = POINTER TO ARRAY OF tc.TSourceRef;

  TModuleList * = POINTER TO TModuleDesc;
  TModuleDesc * = RECORD
    name *: DStrings.String;
    next *: TModuleList;
  END;


CONST
   NAME_OF_RegistryModule         = "RegistryModule";
   NAME_OF_RegistryModuleDynamic  = "RegistryModuleDynamic";

   NAME_OF_IncreaseCounter    = "IncreaseCounter";
   NAME_OF_IncreaseCounterExt = "IncreaseCounterExt";

   NAME_OF_IncreaseConditionCounter = "IncreaseConditionCounter";

   NAME_OF_InitIterationLocal       = "InitIterationLocal";
   NAME_OF_InitIterationCounter     = "InitIterationCounter";
   NAME_OF_IncreaseIterationCounter = "IncreaseIterationCounter";

   NAME_OF_IncreaseRelationCounter         = "IncreaseRelationCounter";
   NAME_OF_IncreaseRelationCounterSigned   = "IncreaseRelationCounterSigned";
   NAME_OF_IncreaseRelationCounterUnsigned = "IncreaseRelationCounterUnsigned";

   NAME_OF_InitRecursionDepth     = "InitRecursionDepth";
   NAME_OF_IncreaseRecursionDepth = "IncreaseRecursionDepth";
   NAME_OF_DecreaseRecursionDepth = "DecreaseRecursionDepth";

   NAME_OF_DecreaseVariableValue  = "DecreaseVariableValue";

VAR
  ObjSYSModule  * : pc.OBJECT;
  ObjRTSModule  * : pc.OBJECT;

  ObjModuleInfo * : pc.OBJECT;
  ObjModuleName * : pc.OBJECT;

  ObjProcedureNames * : pc.OBJECT;

  ObjSourceRefs * : pc.OBJECT;
  ObjCounters   * : pc.OBJECT;
  ObjCountersDyn* : pc.OBJECT;

  ModuleModifyTime * : tc.TTime;
  CRCSum           * : tc.TCRCSum;

--------------------------------------------------------------------------------
PROCEDURE FindObject (module: pc.OBJECT; name-: ARRAY OF CHAR): pc.OBJECT;
VAR obj: pc.OBJECT;
    found: BOOLEAN;
BEGIN
  IF module = NIL THEN
    found := pcO.try_vis(pcO.cur_scope, name, obj);
  ELSE
    found := pcO.try_vis (module.type, name, obj);
  END;

  IF found THEN
    RETURN obj;
  ELSE
    RETURN NIL;
  END;
END FindObject;

--------------------------------------------------------------------------------
PROCEDURE GetObject (module: pc.OBJECT; name-: ARRAY OF CHAR): pc.OBJECT;
VAR obj: pc.OBJECT;
BEGIN
  obj := NIL;

  IF module = NIL THEN
    pcO.fnd_vis(pcO.cur_scope, name, obj);
  ELSE
    pcO.fnd_qua (module.type, name, obj);
  END;

  RETURN obj;
END GetObject;


--------------------------------------------------------------------------------
TYPE   String * = env.String;

PROCEDURE MakeUniqueName * (name: ARRAY OF CHAR): String;
VAR uname: String;
    obj: pc.OBJECT;
    cnt: INTEGER;
BEGIN
  xcStr.dprn_txt (uname, "%s", name);
  cnt := 0;
  WHILE pcO.try_vis (pcO.cur_scope, uname^, obj) DO
    xcStr.dprn_txt (uname, "%s%d", name, cnt);
    INC(cnt);
  END;
  RETURN uname;
END MakeUniqueName;

--------------------------------------------------------------------------------
PROCEDURE MakeBeginName * (doAppendBegin: BOOLEAN): String;
VAR retval: String;
    dir, name, ext: String;
    dllname: String;
BEGIN
  DStrings.Assign("X2C_TCS_", retval);
  env.config.Equation("dllname", dllname);
  IF (dllname # NIL) AND (dllname[0] # 0X) THEN
    DStrings.Append(dllname^, retval);
  ELSIF (tcc.ProjectName # NIL) THEN
    xfs.sys.Get(tcc.ProjectName^, dir, name, ext);
    DStrings.Append(name^, retval);
  ELSIF (tcc.MainModuleName # NIL) THEN
    DStrings.Append(tcc.MainModuleName^, retval);
  END;

<* IF TARGET_VAX THEN *>
  IF LENGTH(retval^) > 31 - LENGTH("_$RODATA") THEN
    retval[LENGTH(retval^)-LENGTH("_$RODATA")] := 0C;
  END;
<* END *> -- IF TARGET_VAX

  IF doAppendBegin THEN
    DStrings.Append("_BEGIN",retval);
  END;

  RETURN retval;
END MakeBeginName;


--------------------------------------------------------------------------------
--                          Create IR Objects
--------------------------------------------------------------------------------
PROCEDURE NewObj (mode: pc.OB_MODE; name-: ARRAY OF CHAR): pc.OBJECT;
VAR obj: pc.OBJECT;
BEGIN
  obj := fe.new_obj (mode);
  pcO.set_name (obj, name);
  obj.flag := pc.flag_m2;

  obj.pos := env.null_pos;
  obj.end := env.null_pos;

  RETURN obj;
END NewObj;


--------------------------------------------------------------------------------
PROCEDURE NewType (mode: pc.TY_MODE): pc.STRUCT;
VAR type: pc.STRUCT;
BEGIN
  type := fe.new_type (mode);
  type.pos := env.null_pos;
  RETURN type;
END NewType;


--------------------------------------------------------------------------------
PROCEDURE NewNode (mode: pc.ND_MODE): pc.NODE;
VAR node: pc.NODE;
BEGIN
  pcO.new (node, mode);
  node.pos := env.null_pos;
  RETURN node;
END NewNode;

--------------------------------------------------------------------------------
PROCEDURE NewEqual (node: pc.NODE): pc.NODE;
VAR
  equal: pc.NODE;
BEGIN
  equal := NewNode(pc.nd_binary);
  equal.pos := node.pos;
  equal.end := node.end;
  equal.sub := pc.sb_equ;
  equal.l := node.l;
  equal.r := node.r;
  equal.type := node.type;
  RETURN equal;
END NewEqual;

--------------------------------------------------------------------------------
--                        Create Programm Objects
--------------------------------------------------------------------------------
VAR type_string: pc.STRUCT;  -- reflection from pcS

PROCEDURE NewConst_String * ( name-:ARRAY OF CHAR
                            ; str-: ARRAY OF CHAR ): pc.OBJECT;
VAR obj:  pc.OBJECT;
    node: pc.NODE;
    type: pc.STRUCT;
    val:  pc.VALUE;
BEGIN
  obj := NewObj (pc.ob_cons, name);

  type_string.len := LENGTH(str) + 1;
  val := pc.value.new (env.null_pos, type_string);
  val.set_string (str);

  type := NewType (pc.ty_SS);
  type.len := type_string.len;

  pcO.new (node, pc.nd_value);
  node.type := type;
  node.val  := val;
  node.pos  := env.null_pos;

  obj.val  := node;
  obj.type := node.type;

  pcO.alloc (obj);
  pcO.dcl (pcO.cur_scope, obj);
  RETURN obj;
END NewConst_String;


--------------------------------------------------------------------------------
PROCEDURE NewConst_RTSEnumeration (item_name-: ARRAY OF CHAR): pc.NODE;
VAR node: pc.NODE;
    obj:  pc.OBJECT;
BEGIN
  obj := GetObject (ObjRTSModule, item_name);
  pcB.gen_usage (obj, node);
  RETURN node;
END NewConst_RTSEnumeration;


--------------------------------------------------------------------------------
PROCEDURE NewConst_RTSInteger (tname-: ARRAY OF CHAR; value: tc.TIndex): pc.NODE;
VAR node: pc.NODE;
    type: pc.STRUCT;
    obj:  pc.OBJECT;
    val:  pc.VALUE;
BEGIN
  obj := GetObject (ObjRTSModule, tname);
  pcB.gen_usage (obj, node);
  type := node.type;

  val := pc.value.new (env.null_pos, ZZ_type);
  val.set_integer (value);

  pcO.new (node, pc.nd_value);
  node.type := ZZ_type;
  node.val  := val;
  node.pos  := env.null_pos;

  pcB.convert (node, type);

  RETURN node;
END NewConst_RTSInteger;

--------------------------------------------------------------------------------
PROCEDURE NewConst_RTSCardinal (tname-: ARRAY OF CHAR; value: tc.TCard32): pc.NODE;
VAR node: pc.NODE;
    type: pc.STRUCT;
    obj:  pc.OBJECT;
    val:  pc.VALUE;
    option_m2extensions: BOOLEAN;
    option_o2extensions: BOOLEAN;
BEGIN
  obj := GetObject (ObjRTSModule, tname);
  pcB.gen_usage (obj, node);
  type := node.type;

  val := pc.value.new (env.null_pos, ZZ_type);
  val.set_integer (sys.VAL(sys.INT32,value));

  pcO.new (node, pc.nd_value);
  node.type := ZZ_type;
  node.val  := val;
  node.pos  := env.null_pos;

  option_m2extensions := env.config.Option("M2EXTENSIONS");
  option_o2extensions := env.config.Option("O2EXTENSIONS");
  env.config.SetOption("M2EXTENSIONS", TRUE);
  env.config.SetOption("O2EXTENSIONS", TRUE);

  pcB.cast (node, type, FALSE);

  env.config.SetOption("M2EXTENSIONS", option_m2extensions);
  env.config.SetOption("O2EXTENSIONS", option_o2extensions);

  RETURN node;
END NewConst_RTSCardinal;

(*
--------------------------------------------------------------------------------
PROCEDURE NewConst_RTSProc ( tname-: ARRAY OF CHAR
                           ; type:pc.STRUCT
                           ; external: BOOLEAN ): pc.NODE;
VAR node: pc.NODE;
    obj: pc.OBJECT;
BEGIN
  pcO.new (node, pc.nd_value);
  node.type := type;
  node.val := pc.value.new (env.null_pos, pcO.proctype0);
  IF external THEN
    obj := NewObj (pc.ob_eproc, tname);
    obj.mno := pc.cur_mod;
  ELSE
    obj := NewObj (pc.ob_proc, tname);
    obj.mno := -1;
  END;
  obj.flag := pc.flag_c;
  obj.host := NewType (pc.ty_module);
  obj.host.obj := NewObj (pc.ob_module, tname);
  obj.type := type;
  node.val.set_object(obj);

  IF external THEN
    pcO.dcl(pcO.cur_scope, obj);
    pcO.alloc(obj);
  END;
  RETURN node;
END NewConst_RTSProc;
*)

--------------------------------------------------------------------------------
PROCEDURE NewVariable * ( name-: ARRAY OF CHAR
                        ; tmodule: pc.OBJECT
                        ; tname-: ARRAY OF CHAR ): pc.OBJECT;
VAR obj:  pc.OBJECT;
    tobj: pc.OBJECT;
BEGIN
  obj := NewObj (pc.ob_var, name);
  obj.host := pcO.cur_scope;

  tobj := GetObject (tmodule, tname);
  obj.type := tobj.type;

  pcO.alloc (obj);
  pcO.dcl (pcO.cur_scope, obj);

  INCL(obj.marks, pc.omark_used_by_code);
<* IF NOT (TARGET_VAX OR TARGET_C OR dbg_tcs) THEN *>
  INCL(obj.marks, pc.omark_no_debug);
<* END *>

  RETURN obj;
END NewVariable;
--------------------------------------------------------------------------------
PROCEDURE NewVariablePtr * ( name-: ARRAY OF CHAR
                          ; tname-: ARRAY OF CHAR ): pc.OBJECT;
VAR obj:  pc.OBJECT;
    tobj: pc.OBJECT;
BEGIN
  obj := NewObj (pc.ob_var, name);
  obj.host := pcO.cur_scope;

  tobj := GetObject (NIL, tname);
  obj.type := NewType (pc.ty_pointer);
  obj.type.base := tobj.type;

  pcO.alloc (obj);
  pcO.dcl (pcO.cur_scope, obj);

  INCL(obj.marks, pc.omark_used_by_code);
<* IF NOT (TARGET_VAX OR TARGET_C OR dbg_tcs) THEN *>
  INCL(obj.marks, pc.omark_no_debug);
<* END *>

  RETURN obj;
END NewVariablePtr;
--------------------------------------------------------------------------------
PROCEDURE NewGlobalVariable * ( name-: ARRAY OF CHAR
                              ; tmodule: pc.OBJECT
                              ; tname-: ARRAY OF CHAR ): pc.OBJECT;
VAR obj:  pc.OBJECT;
    cur_scope: pc.STRUCT;
BEGIN
  cur_scope := pcO.cur_scope;
  pcO.exit_scope ();

  pcO.enter_scope (pc.mods[cur_scope.mno].type);

  obj := NewVariable(name, tmodule, tname);
  obj.lev := 0;

  pcO.exit_scope ();
  pcO.enter_scope (cur_scope);

<* IF NOT TARGET_VAX AND NOT dbg_tcs THEN *>
  INCL(obj.marks, pc.omark_no_debug);
<* END *>

  RETURN obj;
END NewGlobalVariable;


--------------------------------------------------------------------------------
PROCEDURE New_ModuleInfoPtr (): pc.NODE;
VAR node: pc.NODE;
    tnode: pc.NODE;
    ref_arg: ARRAY 1 OF pc.NODE;
    obj:  pc.OBJECT;
BEGIN
  obj := GetObject (ObjSYSModule, "REF");
  pcB.gen_usage (obj, node);

  pcB.gen_usage (ObjModuleInfo, ref_arg[0]);
  pcF.gen_system_function (node, ref_arg, 1, FALSE);

  obj := GetObject (ObjRTSModule, "TModuleInfoPtr");
  pcB.gen_usage (obj, tnode);

  pcB.cast (node, tnode.type, FALSE);

  RETURN node;
END New_ModuleInfoPtr;


--------------------------------------------------------------------------------
-- Массив.
--      mode    ty_array
--      min     -> минимальное значение индекса  <----|
--      max     -> максимальное значение индекса <--| |
--      base    -> базовый тип                      | |
--      inx     -> тип индекса                      | |
--                      max     --------------------| |
--                      min     ----------------------|
--------------------------------------------------------------------------------
PROCEDURE AddArrayType * ( name-: ARRAY OF CHAR
                         ; len: tc.TIndex
                         ; base_tname-: ARRAY OF CHAR );
VAR obj, base_obj:  pc.OBJECT;
    type: pc.STRUCT;
    min, max: pc.VALUE;
BEGIN
  obj  := NewObj (pc.ob_type, name);
  type := NewType (pc.ty_array);
  type.obj := obj;

  -- array index
  min := pc.value.new (env.null_pos, ZZ_type);
  min.set_integer (0);

  max := pc.value.new (env.null_pos, ZZ_type);
  max.set_integer (len-1);

  type.inx := NewType (pc.ty_range);
  type.inx.base := pcO.longcard;
  type.inx.min  := min;
  type.inx.max  := max;
  type.inx.end  := env.null_pos;

  -- array base type
  base_obj := GetObject (ObjRTSModule, base_tname);
  type.base := base_obj.type;
  type.end  := env.null_pos;

  pcO.set_type_base (type, type.base);
  type.min := type.inx.min;
  type.max := type.inx.max;
  type.len := len;

  obj.type := type;

  pcO.alloc (obj);
  pcO.dcl (pcO.cur_scope, obj);
END AddArrayType;
--------------------------------------------------------------------------------
TYPE
  TestConditionTypeNamesArray = ARRAY TestConditionType OF
                                ARRAY 32 OF CHAR;

CONST
  TestConditionTypeNames = TestConditionTypeNamesArray {
    "tc_Version"
  , "tc_Targets"
  , "tc_File"
  , "tc_Empty"
  -- C1 criterion: compilation unit 
  , "tc_C1_Procedure"
  , "tc_C1_Module"
  , "tc_C1_Finally"

  -- C1 criterion: IF statement 
  , "tc_C1_IF"             
  , "tc_C1_IF_true"        
  , "tc_C1_IF_false"       

  , "tc_C1_IF_elsif"       
  , "tc_C1_IF_elsif_true"  
  , "tc_C1_IF_elsif_false" 

  , "tc_Info_IF_else"        

  , "tc_C1_IF_end"         
  , "tc_Info_IF_end"

  -- C1 criterion: CASE statement 
  , "tc_C1_Case"
  , "tc_C1_Case_else"
  , "tc_C1_Case_end"
  , "tc_Info_Case_end"

  -- C1 criterion: Oberon WHIT statement 
  , "tc_C1_OberonWith"
  , "tc_C1_OberonWith_else"
  , "tc_C1_OberonWith_end"
  , "tc_Info_OberonWith_end"

  -- C1 criterion: WHILE statement
  , "tc_C1_While_true"
  , "tc_C1_While_false"
  , "tc_C1_While_end"
  , "tc_Info_While_end"

  -- C criterion: WHILE statement
  , "tc_C_While_itr_0"
  , "tc_C_While_itr_1"
  , "tc_C_While_itr_N"

  -- C1 criterion: REPEAT statement
  , "tc_C1_Repeat_true"
  , "tc_C1_Repeat_false"
  , "tc_C1_Repeat_end"
  , "tc_Info_Repeat_end"

  -- C criterion: REPEAT statement
  , "tc_Info_Repeat_itr_0"
  , "tc_C_Repeat_itr_1"
  , "tc_C_Repeat_itr_N"

  -- C1 criterion: FOR statement
  , "tc_C1_For"
  , "tc_C1_For_end"
  , "tc_Info_For_end"

  -- C criterion: FOR statement
  , "tc_Info_For_itr_0"
  , "tc_Info_For_itr_1"
  , "tc_C_For_itr_0"
  , "tc_C_For_itr_1"
  , "tc_C_For_itr_N"

  -- C1 criterion: LOOP statement
  , "tc_C1_Loop"
  , "tc_C1_Loop_end"
  , "tc_Info_Loop_end"

  -- C criterion: LOOP statement
  , "tc_Info_Loop_itr_0"
  , "tc_C_Loop_itr_1"
  , "tc_C_Loop_itr_N"

  , "tc_C_Expr_true"
  , "tc_C_Expr_false"
  , "tc_C_Expr_equal"
  , "tc_C_Case_else"
  , "tc_C1_Except"

  , "tc_C_Recur_0"
  , "tc_C_Recur_1"
  , "tc_C_Recur_N"
  
  };

--------------------------------------------------------------------------------
PROCEDURE NewConst_SourceRefs * ( name-: ARRAY OF CHAR
                                ; srcrefs: TSourceRefs
                                ; base_tname-: ARRAY OF CHAR ): pc.OBJECT;

    -- 1 -- NewConst_Fixups ----------------------------------------------------
    PROCEDURE NewRecordConst ( type: pc.STRUCT
                             ; index: tc.TIndex ): pc.NODE;
    VAR node: pc.NODE;
        tail: pc.NODE;
        expr: pc.NODE;
        field: pc.OBJECT;

        -- 2 -- NewRecordConst -------------------------------------------------
        PROCEDURE AddField ();
        BEGIN
          pcB.assign_compatibility (env.null_pos, field.type, expr);
          pcO.app (node.l, tail, expr);
          field := field.next;
        END AddField;

    -- 1 -- NewRecordConst -----------------------------------------------------
    VAR  obj:  pc.OBJECT;
    BEGIN
      node := NewNode (pc.nd_aggregate);
      node.type := type;

      field := type.prof;
      tail := NIL;

      -- proc: TIndex;
      expr := NewNode (pc.nd_value);
      expr.type := ZZ_type;
      expr.val  := pc.value.new (env.null_pos, ZZ_type);
      expr.val.set_integer (srcrefs[index].proc);
      AddField ();

      -- line: TSourceLine;
      expr := NewNode (pc.nd_value);
      expr.type := ZZ_type;
      expr.val  := pc.value.new (env.null_pos, ZZ_type);
      expr.val.set_integer (srcrefs[index].line);
      AddField ();

      -- col:  TSourceCol;
      expr := NewNode (pc.nd_value);
      expr.type := ZZ_type;
      expr.val  := pc.value.new (env.null_pos, ZZ_type);
      expr.val.set_integer (srcrefs[index].col);
      AddField ();

      -- end_line: TSourceLine;
      expr := NewNode (pc.nd_value);
      expr.type := ZZ_type;
      expr.val  := pc.value.new (env.null_pos, ZZ_type);
      expr.val.set_integer (srcrefs[index].end_line);
      AddField ();

      -- end_col:  TSourceCol;
      expr := NewNode (pc.nd_value);
      expr.type := ZZ_type;
      expr.val  := pc.value.new (env.null_pos, ZZ_type);
      expr.val.set_integer (srcrefs[index].end_col);
      AddField ();

      -- type: TestCriterionType;
      obj := GetObject (ObjRTSModule, TestConditionTypeNames[srcrefs[index].type]);
      pcB.gen_usage (obj, expr);
      AddField ();

      RETURN node;
    END NewRecordConst;

-- 0 -- NewConst_Fixups --------------------------------------------------------
VAR obj, base_obj:  pc.OBJECT;
    node: pc.NODE;
    tail: pc.NODE;
    expr: pc.NODE;
    type: pc.STRUCT;
    i: INT;
BEGIN
  obj  := NewObj (pc.ob_cons, name);
  base_obj := GetObject (NIL, base_tname);
  type := base_obj.type;

  node := NewNode (pc.nd_aggregate);
  node.type := type;

  tail := NIL;
  FOR i:= 0 TO type.len-1 DO
    expr := NewRecordConst (type.base, i);
    pcB.assign_compatibility (env.null_pos, type.base, expr);
    pcO.app (node.l, tail, expr);
  END;

  obj.val  := node;
  obj.type := node.type;

  pcO.alloc (obj);
  pcO.dcl (pcO.cur_scope, obj);
  RETURN obj;
END NewConst_SourceRefs;


--------------------------------------------------------------------------------
PROCEDURE NewProcedure * ( name-: ARRAY OF CHAR
                         ; host: pc.STRUCT
                         ; flag: fe.Lang
                         ): pc.OBJECT;
VAR proc: pc.OBJECT;
    type: pc.STRUCT;
BEGIN
  proc := fe.new_obj(pc.ob_proc);
  pcO.set_name(proc, name);
  proc.flag := flag;
  proc.host := host;
  proc.mno  := host.mno;

  type := fe.new_type(pc.ty_proctype);
  type.base := pcO.void;
  type.obj  := proc;
  type.flag := flag;
  type.pos  := env.null_pos;

  proc.type := type;

  pcO.new(proc.val, pc.nd_proc);
  proc.val.obj := proc;

  INCL(proc.marks, pc.omark_used_by_code); -- to preserve from wiping
  INCL(proc.tags,  pc.otag_public);        -- to preserve from wiping
<* IF NOT TARGET_VAX AND NOT dbg_tcs THEN *>
  INCL(proc.marks, pc.omark_no_debug);
<* END *>

  RETURN proc;
END NewProcedure;

--------------------------------------------------------------------------------
PROCEDURE NewExternalProcedure ( name-: ARRAY OF CHAR
                               ; host: pc.STRUCT
                               ; flag: fe.Lang ): pc.OBJECT;
VAR proc: pc.OBJECT;
    type: pc.STRUCT;
BEGIN
  proc := fe.new_obj(pc.ob_eproc);
  pcO.set_name(proc, name);
  proc.flag := flag;
  proc.host := host;
  proc.mno  := host.mno;
  proc.lev  := 0;

  type := fe.new_type(pc.ty_proctype);
  type.mno  := host.mno;
  type.base := pcO.void;
  type.obj  := proc;
  type.flag := flag;
  type.pos  := env.null_pos;

  proc.type := type;

  pcO.dcl(host, proc);

  INCL(proc.marks, pc.omark_used_by_code); -- to preserve from wiping
<* IF NOT TARGET_VAX AND NOT dbg_tcs THEN *>
  INCL(proc.marks, pc.omark_no_debug);
<* END *>

  RETURN proc;
END NewExternalProcedure;


--------------------------------------------------------------------------------
PROCEDURE Get_ExternalProcedure ( name-: ARRAY OF CHAR
                                ; host: pc.STRUCT
                                ; flag: fe.Lang ): pc.OBJECT;
VAR proc: pc.OBJECT;
BEGIN
  proc := FindObject(host.obj, name);
  IF (proc # NIL) & (proc.mode # pc.ob_inv) THEN
    ASSERT( proc.mode = pc.ob_eproc );
  ELSE
    proc := NewExternalProcedure(name, host, flag);
  END;

  RETURN proc;
END Get_ExternalProcedure;

--------------------------------------------------------------------------------
PROCEDURE NewNode_ExternalProcedure ( tname-: ARRAY OF CHAR
                                    ; type: pc.STRUCT ): pc.NODE;
VAR node: pc.NODE;
    obj: pc.OBJECT;
BEGIN
  ASSERT( type = pcO.proctype0 );

  pcO.new (node, pc.nd_value);
  node.type := type;
  node.val := pc.value.new (env.null_pos, pcO.proctype0);

  obj := Get_ExternalProcedure(tname, pc.mods[cur_mod].type, pc.flag_c);

  node.val.set_object(obj);

  RETURN node;
END NewNode_ExternalProcedure;



--------------------------------------------------------------------------------
--                      Abstract Procedure Calls
--------------------------------------------------------------------------------
TYPE
  TArgumentIndex = INTEGER;

TYPE
  TProcedureCall = RECORD
  END;

<* PUSH *> <* +WOFF301 *> -- parameter "%s" is never used
PROCEDURE (VAR this: TProcedureCall) getName (VAR name: ARRAY OF CHAR);
BEGIN
  ASSERT(FALSE);
END getName;

PROCEDURE (VAR this: TProcedureCall) getObject (): pc.OBJECT;
BEGIN
  ASSERT(FALSE);
END getObject;

PROCEDURE (VAR this: TProcedureCall) getArgument (i: TArgumentIndex): pc.NODE;
BEGIN
  -- arguments are numbered from 1
  ASSERT(FALSE);
END getArgument;
<* POP *>

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TProcedureCall) finalize (pcall: pc.NODE);
BEGIN
  pcall.obj := pcall.l.obj;
  pcall.l   := NIL;
END finalize;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TProcedureCall) NewCallNode (): pc.NODE;
VAR obj: pc.OBJECT;
    proc, pcall: pc.NODE;
BEGIN
  obj := this.getObject();

  pcB.gen_usage (obj, proc);
  pcO.new (pcall, pc.nd_call);

  pcall.pos  := env.null_pos;
  pcall.l    := proc;
  pcall.type := proc.type.base;

  RETURN pcall;
END NewCallNode;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TProcedureCall) createCall (): pc.NODE;
VAR pcall:  pc.NODE;
    fparam: pc.OBJECT; -- formal procedure parameters
    ptail:  pc.NODE;   -- tail of actual parameters list

    -- 1 -- TProcedureCall.create ----------------------------------------------
    PROCEDURE AddParameter (arg: pc.NODE);
    BEGIN
      pcB.parameter_compatibility (fparam, arg);
      IF ptail = NIL THEN  pcall.r    := arg;
      ELSE                 ptail.next := arg;
      END;
      ptail  := arg;
      fparam := fparam.next;
    END AddParameter;

-- 0 -- TProcedureCall.create --------------------------------------------------
VAR arg: pc.NODE;
    i: TArgumentIndex;
BEGIN
  pcall := this.NewCallNode ();

  -- create arguments
  fparam := pcall.l.type.prof;
  ptail  := NIL;

<* PUSH *> <* +WOFF314 *> -- variable "i" has compile time defined value here
  i := 1;
  arg := this.getArgument(i);
<* POP *>
  WHILE (arg # NIL) DO
    AddParameter (arg);
    INC(i);
    arg := this.getArgument(i);
  END;

  this.finalize(pcall);

  RETURN pcall;
END createCall;

--------------------------------------------------------------------------------
--                  Create New External Procedure Call
--------------------------------------------------------------------------------
TYPE
  TTCSModuleBeginCall * = RECORD (TProcedureCall)
    host-: pc.STRUCT;
  END;

PROCEDURE (VAR this: TTCSModuleBeginCall) NewCall * (host: pc.STRUCT): pc.NODE;
VAR pcall:  pc.NODE;
BEGIN
  this.host := host;
  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TTCSModuleBeginCall) getName (VAR name: ARRAY OF CHAR);
BEGIN
  COPY(MakeBeginName(TRUE)^, name);
END getName;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TTCSModuleBeginCall) getObject (): pc.OBJECT;
VAR name: ARRAY 256 OF CHAR;
    obj: pc.OBJECT;
BEGIN
  this.getName(name);
  obj := Get_ExternalProcedure(name, this.host, pc.flag_c);
  RETURN obj;
END getObject;

--------------------------------------------------------------------------------
<* PUSH *> <* +WOFF301 *> -- parameter "%s" is never used
PROCEDURE (VAR this: TTCSModuleBeginCall) getArgument (i: TArgumentIndex): pc.NODE;
BEGIN
  RETURN NIL;
END getArgument;
<* POP *>


--------------------------------------------------------------------------------
--                    Create New RTS Procedure Calls
--------------------------------------------------------------------------------
VAR
  SaveIncompleteRTSCall *: PROCEDURE ( pcall: pc.NODE
                                     ; UseHeavyRTSCall: BOOLEAN );

--------------------------------------------------------------------------------
--          abstract call of procedure with incoplete argument list
--------------------------------------------------------------------------------
TYPE
  TIncompleteRTSCall = RECORD (TProcedureCall)
  END;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncompleteRTSCall) getObject (): pc.OBJECT;
VAR name: ARRAY 256 OF CHAR;
    obj: pc.OBJECT;
BEGIN
  this.getName(name);
  obj := GetObject (ObjRTSModule, name);
  RETURN obj;
END getObject;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncompleteRTSCall) finalize (pcall: pc.NODE);
BEGIN
  SaveIncompleteRTSCall (pcall, FALSE (*UseHeavyRTSCall*));
END finalize;

--------------------------------------------------------------------------------
--             Finalyzed call of abstract procedure
--------------------------------------------------------------------------------
TYPE
  TCompleteRTSCall = RECORD (TIncompleteRTSCall)
  END;

PROCEDURE (VAR this: TCompleteRTSCall) finalize (pcall: pc.NODE);
BEGIN
  pcall.obj := pcall.l.obj;
  pcall.l   := NIL;
END finalize;


--------------------------------------------------------------------------------
--  PROCEDURE RegistryModule ( module:   TModuleInfoPtr
--                           ; modtime:  TTime
--                           ; modname:  ARRAY OF CHAR
--                           ; procnames: ARRAY OF CHAR
--                           ; srcrefs:  ARRAY OF TSourceRef
--                           ; counters: ARRAY OF TCounter
--                           );
--------------------------------------------------------------------------------
TYPE
  TRegistryModule * = RECORD (TCompleteRTSCall)
    dynamic : BOOLEAN;
  END;

PROCEDURE (VAR this: TRegistryModule) SetDynamicMode * (dynamic : BOOLEAN);
BEGIN
  this.dynamic := dynamic;
END SetDynamicMode;

PROCEDURE (VAR this: TRegistryModule) NewCall * (): pc.NODE;
VAR pcall:  pc.NODE;
BEGIN
  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TRegistryModule) getName (VAR name: ARRAY OF CHAR);
BEGIN
  IF (this.dynamic) THEN
    COPY(NAME_OF_RegistryModuleDynamic, name);
  ELSE
    COPY(NAME_OF_RegistryModule, name);
  END;
END getName;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TRegistryModule) getArgument (i: TArgumentIndex): pc.NODE;
VAR arg: pc.NODE;
BEGIN
  CASE i OF
  | 1: -- ( module: ModuleInfoPtr
      arg := New_ModuleInfoPtr();
  | 2: -- ; modtime:  TTime
      arg := NewConst_RTSCardinal ("TTime", ModuleModifyTime);
  | 3: -- ; crcsum:    TCRCSum
      arg := NewConst_RTSCardinal ("TCRCSum", CRCSum);
  | 4: -- ; modname: ARRAY OF CHAR
      pcB.gen_usage (ObjModuleName, arg);
  | 5: -- ; procnames: ARRAY OF CHAR
      pcB.gen_usage (ObjProcedureNames, arg);
  | 6: -- ; srcrefs:  ARRAY OF TSourceRef
      pcB.gen_usage (ObjSourceRefs, arg);
  | 7: -- ; counters: ARRAY OF TCounter )
      IF (this.dynamic) THEN
        pcB.gen_usage (ObjCountersDyn, arg);
      ELSE
        pcB.gen_usage (ObjCounters, arg);
      END;
  ELSE
      arg := NIL;
  END;
  RETURN arg;
END getArgument;


--------------------------------------------------------------------------------
--  PROCEDURE CallRegistryModule ( modulename: ARRAY OF CHAR );
--------------------------------------------------------------------------------
TYPE
  TCallRegistryModule * = RECORD (TCompleteRTSCall)
    modulename: DStrings.String;
  END;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TCallRegistryModule) NewCall * (modulename-: ARRAY OF CHAR): pc.NODE;
VAR pcall:  pc.NODE;
BEGIN
  DStrings.Assign(modulename, this.modulename);
  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TCallRegistryModule) getName (VAR name: ARRAY OF CHAR);
BEGIN
  COPY(this.modulename^, name);
END getName;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TCallRegistryModule) getObject (): pc.OBJECT;
VAR modulename: ARRAY 256 OF CHAR;
    procname: DStrings.String;
    obj: pc.OBJECT;
BEGIN
  this.getName(modulename);
  DStrings.Assign(modulename, procname);
  DStrings.Append("_X2C_TCS_INIT", procname);

  obj := NewObj(pc.ob_proc, procname^);
  obj.flag := pc.flag_c;
  obj.host := NewType (pc.ty_module);
  obj.host.obj := NewObj (pc.ob_module, modulename);
  obj.type := NewType (pc.ty_proctype);
  obj.type.base := NewType (pc.ty_void);

  RETURN obj;
END getObject;

--------------------------------------------------------------------------------
<* PUSH *> <* +WOFF301 *> -- parameter "%s" is never used
PROCEDURE (VAR this: TCallRegistryModule) getArgument (i: TArgumentIndex): pc.NODE;
BEGIN
  RETURN NIL;
END getArgument;
<* POP *>

--------------------------------------------------------------------------------
--  PROCEDURE InitIterationLocal ( VAR local_counter: TCounter
--                               );
--------------------------------------------------------------------------------
(*
TYPE
  TInitIterationLocal * = RECORD (TCompleteRTSCall)
    local_counter -: pc.OBJECT;
  END;

PROCEDURE (VAR this: TInitIterationLocal) NewCall * ( local_counter: pc.OBJECT
                                                    ): pc.NODE;
VAR pcall:  pc.NODE;
BEGIN
  this.local_counter := local_counter;

  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TInitIterationLocal) getName (VAR name: ARRAY OF CHAR);
BEGIN
  COPY(NAME_OF_InitIterationLocal, name);
END getName;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TInitIterationLocal) getArgument (i: TArgumentIndex): pc.NODE;
VAR arg: pc.NODE;
BEGIN
  CASE i OF
  | 1: -- (VAR local_counter: TCounter);
      pcB.gen_usage (this.local_counter, arg);
  ELSE
      arg := NIL;
  END;
  RETURN arg;
END getArgument;
*)

--------------------------------------------------------------------------------
--  PROCEDURE InitIterationCounter ( index: TIndex
--                                 ; VAR local_counter: TCounter
--                                 ; VAR counters: ARRAY OF TCounter
--                                 );
--------------------------------------------------------------------------------
TYPE
  TInitIterationCounter * = RECORD (TIncompleteRTSCall)
    index_0       -: tc.TIndex;
    local_counter -: pc.OBJECT;
  END;

PROCEDURE (VAR this: TInitIterationCounter) NewCall * ( index_0: tc.TIndex
                                                      ; local_counter: pc.OBJECT
                                                      ): pc.NODE;
VAR pcall:  pc.NODE;
BEGIN
  this.index_0       := index_0;
  this.local_counter := local_counter;

  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TInitIterationCounter) getName (VAR name: ARRAY OF CHAR);
BEGIN
  COPY(NAME_OF_InitIterationCounter, name);
END getName;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TInitIterationCounter) getArgument (i: TArgumentIndex): pc.NODE;
VAR arg: pc.NODE;
BEGIN
  CASE i OF
  | 1: -- ( index_0: TIndex
      arg := NewConst_RTSInteger ("TIndex", this.index_0);
  | 2:  -- ; VAR local_counter: TCounter
      pcB.gen_usage (this.local_counter, arg);
  ELSE
      arg := NIL;
  END;
  RETURN arg;
END getArgument;



--------------------------------------------------------------------------------
--  PROCEDURE IncreaseIterationCounter ( index: TIndex
--                                     ; VAR local_counter: TCounter
--                                     ; VAR counters: ARRAY OF TCounter
--                                     );
--------------------------------------------------------------------------------
TYPE
  TIncreaseIterationCounter * = RECORD (TIncompleteRTSCall)
    index_0        -: tc.TIndex;
    index_1        -: tc.TIndex;
    index_N        -: tc.TIndex;
    local_counter  -: pc.OBJECT;
  END;

PROCEDURE (VAR this: TIncreaseIterationCounter) NewCall * ( index_0, index_1, index_N: tc.TIndex
                                                          ; local_counter: pc.OBJECT
                                                          ): pc.NODE;
VAR pcall:  pc.NODE;
BEGIN
  this.index_0       := index_0;
  this.index_1       := index_1;
  this.index_N       := index_N;
  this.local_counter := local_counter;

  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseIterationCounter) getName (VAR name: ARRAY OF CHAR);
BEGIN
  COPY(NAME_OF_IncreaseIterationCounter, name);
END getName;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseIterationCounter) getArgument (i: TArgumentIndex): pc.NODE;
VAR arg: pc.NODE;
BEGIN
  CASE i OF
  | 1: -- ( index_0: TIndex
      arg := NewConst_RTSInteger ("TIndex", this.index_0);
  | 2: -- ; index_1: TIndex
      arg := NewConst_RTSInteger ("TIndex", this.index_1);
  | 3: -- ; index_N: TIndex
      arg := NewConst_RTSInteger ("TIndex", this.index_N);
  | 4:  -- ; VAR local_counter: TCounter
      pcB.gen_usage (this.local_counter, arg);
  ELSE
      arg := NIL;
  END;
  RETURN arg;
END getArgument;

--------------------------------------------------------------------------------
-- PROCEDURE IncreaseRecursionCounter ( index_0: TIndex
--                                    ; index_1: TIndex
--                                    ; index_N: TIndex
--                                    ; VAR depth: TCounter
--                                    ; VAR was_return: BOOLEAN
--                                    ; VAR counters: ARRAY OF TCounter
--                                    );
--------------------------------------------------------------------------------
TYPE
  TIncreaseRecursionDepth * = RECORD (TIncompleteRTSCall)
    index_0    -: tc.TIndex;
    index_1    -: tc.TIndex;
    index_N    -: tc.TIndex;
    depth      -: pc.OBJECT;
    was_return -: pc.OBJECT;
  END;

PROCEDURE (VAR this: TIncreaseRecursionDepth) NewCall * ( index_0, index_1, index_N: tc.TIndex
                                                        ; depth: pc.OBJECT
                                                        ; was_return: pc.OBJECT
                                                        ): pc.NODE;
VAR pcall:  pc.NODE;
BEGIN
  this.index_0    := index_0;
  this.index_1    := index_1;
  this.index_N    := index_N;
  this.depth      := depth;
  this.was_return := was_return;

  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseRecursionDepth) getName (VAR name: ARRAY OF CHAR);
BEGIN
  COPY(NAME_OF_IncreaseRecursionDepth, name);
END getName;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseRecursionDepth) getArgument (i: TArgumentIndex): pc.NODE;
VAR arg: pc.NODE;
BEGIN
  CASE i OF
  | 1: -- ( index_0: TIndex
      arg := NewConst_RTSInteger ("TIndex", this.index_0);
  | 2: -- ; index_1: TIndex
      arg := NewConst_RTSInteger ("TIndex", this.index_1);
  | 3: -- ; index_N: TIndex
      arg := NewConst_RTSInteger ("TIndex", this.index_N);
  | 4: -- ; VAR depth: TCounter
      pcB.gen_usage (this.depth, arg);
  | 5: -- ; VAR was_return: BOOLEAN
      pcB.gen_usage (this.was_return, arg);
  ELSE
      arg := NIL;
  END;
  RETURN arg;
END getArgument;

--------------------------------------------------------------------------------
--  PROCEDURE IncreaseCounter ( index: TIndex
--                            ; VAR counters: ARRAY OF TCounter
--                            );
--------------------------------------------------------------------------------
TYPE
  TIncreaseCounter * = RECORD (TIncompleteRTSCall)
    index -: tc.TIndex;
    heavy -: BOOLEAN;
  END;

PROCEDURE (VAR this: TIncreaseCounter) NewCall * ( index: tc.TIndex
                                                 ; heavy: BOOLEAN ): pc.NODE;
VAR pcall:  pc.NODE;
BEGIN
  this.index := index;
  this.heavy := heavy;

  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseCounter) getName (VAR name: ARRAY OF CHAR);
BEGIN
  IF this.heavy THEN
    COPY(NAME_OF_IncreaseCounterExt, name);
  ELSE
    COPY(NAME_OF_IncreaseCounter, name);
  END;
END getName;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseCounter) getArgument (i: TArgumentIndex): pc.NODE;
VAR arg: pc.NODE;
BEGIN
  CASE i OF
  | 1: -- ( index: TIndex
      arg := NewConst_RTSInteger ("TIndex", this.index);
  ELSE
      arg := NIL;
  END;
  RETURN arg;
END getArgument;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseCounter) finalize (pcall: pc.NODE);
BEGIN
  SaveIncompleteRTSCall (pcall, this.heavy);
END finalize;


--------------------------------------------------------------------------------
-- PROCEDURE DecreaseVariableValue (VAR value: TCounter);
--------------------------------------------------------------------------------
TYPE
  TDecreaseVariableValue * = RECORD (TCompleteRTSCall)
    variable: pc.OBJECT;
  END;

PROCEDURE (VAR this: TDecreaseVariableValue) NewCall * (variable: pc.OBJECT): pc.NODE;
VAR
  pcall:  pc.NODE;
BEGIN
  this.variable := variable;
  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TDecreaseVariableValue) getName (VAR name: ARRAY OF CHAR);
BEGIN
  COPY(NAME_OF_DecreaseVariableValue, name);
END getName;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TDecreaseVariableValue) getArgument (i: TArgumentIndex): pc.NODE;
VAR arg: pc.NODE;
BEGIN
  CASE i OF
  | 1: -- variable: pc.OBJECT;
      pcB.gen_usage (this.variable, arg);
  ELSE
      arg := NIL;
  END;
  RETURN arg;
END getArgument;


--------------------------------------------------------------------------------
-- PROCEDURE InitRecursionDepth ( VAR depth : TCounter
--                              ; VAR was_return: BOOLEAN );
--------------------------------------------------------------------------------
TYPE
  TInitRecursionDepth * = RECORD (TCompleteRTSCall)
    depth      -: pc.OBJECT;
    was_return -: pc.OBJECT;
  END;

PROCEDURE (VAR this: TInitRecursionDepth) NewCall * ( depth: pc.OBJECT
                                                    ; was_return: pc.OBJECT
                                                    ): pc.NODE;
VAR
  pcall:  pc.NODE;
BEGIN
  this.depth := depth;
  this.was_return := was_return;
  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TInitRecursionDepth) getName (VAR name: ARRAY OF CHAR);
BEGIN
  COPY(NAME_OF_InitRecursionDepth, name);
END getName;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TInitRecursionDepth) getArgument (i: TArgumentIndex): pc.NODE;
VAR arg: pc.NODE;
BEGIN
  CASE i OF
  | 1: -- ( VAR depth : TCounter
      pcB.gen_usage (this.depth, arg);
  | 2: -- ; VAR was_return: BOOLEAN
      pcB.gen_usage (this.was_return, arg);
  ELSE
      arg := NIL;
  END;
  RETURN arg;
END getArgument;

--------------------------------------------------------------------------------
-- PROCEDURE DecreaseRecursionDepth ( VAR depth: TCounter
--                                  ; VAR was_return: BOOLEAN
--                                  );
--------------------------------------------------------------------------------
TYPE
  TDecreaseRecursionDepth * = RECORD (TCompleteRTSCall)
    depth      -: pc.OBJECT;
    was_return -: pc.OBJECT;
  END;

PROCEDURE (VAR this: TDecreaseRecursionDepth) NewCall * ( depth: pc.OBJECT
                                                        ; was_return: pc.OBJECT
                                                        ): pc.NODE;
VAR
  pcall: pc.NODE;
BEGIN
  this.depth := depth;
  this.was_return := was_return;
  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TDecreaseRecursionDepth) getName (VAR name: ARRAY OF CHAR);
BEGIN
  COPY(NAME_OF_DecreaseRecursionDepth, name);
END getName;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TDecreaseRecursionDepth) getArgument (i: TArgumentIndex): pc.NODE;
VAR arg: pc.NODE;
BEGIN
  CASE i OF
  | 1: -- ( VAR depth: TCounter;
      pcB.gen_usage (this.depth, arg);
  | 2: -- ; VAR was_return: BOOLEAN
      pcB.gen_usage (this.was_return, arg);
  ELSE
      arg := NIL;
  END;
  RETURN arg;
END getArgument;

--------------------------------------------------------------------------------
--  PROCEDURE IncreaseConditionCounter ( condition: BOOLEAN
--                                     ; index_true: TIndex
--                                     ; index_false: TIndex
--                                     ; VAR counters: ARRAY OF TCounter
--                                     ): BOOLEAN;
--------------------------------------------------------------------------------
TYPE
  TIncreaseConditionCounter * = RECORD (TIncompleteRTSCall)
    condition   -: pc.NODE;
    index_true  -: tc.TIndex;
    index_false -: tc.TIndex;
  END;

PROCEDURE (VAR this: TIncreaseConditionCounter) NewCall * ( condition: pc.NODE
                                                          ; index_true: tc.TIndex
                                                          ; index_false: tc.TIndex
                                                          ): pc.NODE;
VAR pcall:  pc.NODE;
BEGIN
  this.condition   := condition;
  this.index_true  := index_true;
  this.index_false := index_false;

  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseConditionCounter) getName (VAR name: ARRAY OF CHAR);
BEGIN
  COPY(NAME_OF_IncreaseConditionCounter, name);
END getName;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseConditionCounter) getArgument (i: TArgumentIndex): pc.NODE;
VAR arg: pc.NODE;
BEGIN
  CASE i OF
  | 1: -- ; condition: BOOLEAN
      arg := this.condition;
  | 2: -- ; index_true: TIndex
      arg := NewConst_RTSInteger ("TIndex", this.index_true);
  | 3: -- ; index_false: TIndex
      arg := NewConst_RTSInteger ("TIndex", this.index_false);
  ELSE
      arg := NIL;
  END;
  RETURN arg;
END getArgument;


--------------------------------------------------------------------------------
--  PROCEDURE IncreaseRelationCounter ( condition: BOOLEAN
--                                     ; equals: BOOLEAN
--                                     ; index: TIndex
--                                     ; VAR counters: ARRAY OF TCounter
--                                     ): BOOLEAN;
--------------------------------------------------------------------------------
TYPE
  TIncreaseRelationCounter * = RECORD (TIncompleteRTSCall)
    condition- : pc.NODE;
    index-     : tc.TIndex;
  END;

PROCEDURE (VAR this: TIncreaseRelationCounter) NewCall * ( condition: pc.NODE
                                                         ; index: tc.TIndex
                                                         ): pc.NODE;
VAR pcall:  pc.NODE;
BEGIN
  this.condition := condition;
  this.index  := index;

  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseRelationCounter) getName (VAR name: ARRAY OF CHAR);
BEGIN
  COPY(NAME_OF_IncreaseRelationCounter, name);
END getName;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseRelationCounter) getArgument (i: TArgumentIndex): pc.NODE;
VAR arg: pc.NODE;
BEGIN
  CASE i OF
  | 1: -- ; condition: BOOLEAN
      arg := this.condition;
  | 2: -- ; equals: BOOLEAN
      arg := NewEqual (this.condition);
  | 3: -- ; index: TIndex
      arg := NewConst_RTSInteger ("TIndex", this.index);
  ELSE
      arg := NIL;
  END;
  RETURN arg;
END getArgument;


--------------------------------------------------------------------------------
--  PROCEDURE IncreaseRelationCounter<Type> ( relation: TRelation
--                                          ; arg_left: T<Type>Argument
--                                          ; arg_right: T<Type>Argument
--                                          ; index: TIndex
--                                          ; VAR counters: ARRAY OF TCounter
--                                          ): BOOLEAN;
--------------------------------------------------------------------------------
TYPE
  TIncreaseRelationCounterWithSideEffects * = RECORD (TIncompleteRTSCall)
    condition- : pc.NODE;
    index-     : tc.TIndex;
  END;

PROCEDURE (VAR this: TIncreaseRelationCounterWithSideEffects)
          NewCall * ( condition: pc.NODE
                    ; index: tc.TIndex
                    ): pc.NODE;
VAR pcall:  pc.NODE;
BEGIN
  this.condition := condition;
  this.index  := index;

  pcall := this.createCall ();
  RETURN pcall;
END NewCall;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseRelationCounterWithSideEffects) getArgument (i: TArgumentIndex): pc.NODE;
VAR arg: pc.NODE;
BEGIN
  CASE i OF
  | 1: -- ; relation: TRelation
      CASE this.condition.sub OF
      | pc.sb_gtr:
          arg := NewConst_RTSEnumeration ("rel_GreaterThen");
      | pc.sb_geq:
          arg := NewConst_RTSEnumeration ("rel_GreaterEqual");
      | pc.sb_lss:
          arg := NewConst_RTSEnumeration ("rel_LessThen");
      | pc.sb_leq:
          arg := NewConst_RTSEnumeration ("rel_LessEqual");
      END;
  | 2: -- ; arg_left: TUnsignedArgument
      arg := this.condition.l;
  | 3: -- ; arg_right: TUnsignedArgument
      arg := this.condition.r;
  | 4: -- ; index: TIndex
      arg := NewConst_RTSInteger ("TIndex", this.index);
  ELSE
      arg := NIL;
  END;
  RETURN arg;
END getArgument;

--------------------------------------------------------------------------------
--  PROCEDURE IncreaseRelationCounterSigned ( relation: TRelation
--                                          ; arg_left: TSignedArgument
--                                          ; arg_right: TSignedArgument
--                                          ; index: TIndex
--                                          ; VAR counters: ARRAY OF TCounter
--                                          ): BOOLEAN;
--------------------------------------------------------------------------------
TYPE
  TIncreaseRelationCounterSigned * = RECORD (TIncreaseRelationCounterWithSideEffects)
  END;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseRelationCounterSigned) getName (VAR name: ARRAY OF CHAR);
BEGIN
  COPY(NAME_OF_IncreaseRelationCounterSigned, name);
END getName;

--------------------------------------------------------------------------------
--  PROCEDURE IncreaseRelationCounterUnsigned ( relation: TRelation
--                                            ; arg_left: TUnsignedArgument
--                                            ; arg_right: TUnsignedArgument
--                                            ; index: TIndex
--                                            ; VAR counters: ARRAY OF TCounter
--                                            ): BOOLEAN;
--------------------------------------------------------------------------------
TYPE
  TIncreaseRelationCounterUnsigned * = RECORD (TIncreaseRelationCounterWithSideEffects)
  END;

--------------------------------------------------------------------------------
PROCEDURE (VAR this: TIncreaseRelationCounterUnsigned) getName (VAR name: ARRAY OF CHAR);
BEGIN
  COPY(NAME_OF_IncreaseRelationCounterUnsigned, name);
END getName;



--------------------------------------------------------------------------------
PROCEDURE CompleteRTSCall * (pcall: pc.NODE; heavy: BOOLEAN; dynamic : BOOLEAN);
VAR fparam: pc.OBJECT; -- formal procedure parameters
    ptail:  pc.NODE;   -- tail of actual parameters list

    -- 1 -- NewRTSCall_IncreaseCounter -----------------------------------------
    PROCEDURE AddParameter (arg: pc.NODE);
    BEGIN
      pcB.parameter_compatibility (fparam, arg);
      IF ptail = NIL THEN  pcall.r    := arg;
      ELSE                 ptail.next := arg;
      END;
      ptail  := arg;
      fparam := fparam.next;
    END AddParameter;

-- 0 -- NewRTSCall_IncreaseCounter ---------------------------------------------
VAR arg: pc.NODE;
BEGIN
  -- skip already generated arguments
  IF pcall.r = NIL THEN
    fparam := pcall.l.type.prof;
    ptail  := NIL;
  ELSE
    fparam := pcall.l.type.prof.next;
    ptail  := pcall.r;
    WHILE ptail.next # NIL DO
      ptail := ptail.next;
      fparam := fparam.next;
    END;
  END;

  IF heavy THEN
    -- add ADR(X2C_TCS_<prjname>_BEGIN) to call parameters
    arg := NewNode_ExternalProcedure (MakeBeginName(TRUE)^, fparam.type);
    AddParameter (arg);
  END;
  -- ; VAR counters: ARRAY OF TCounter
  IF (dynamic) THEN
    pcB.gen_usage (ObjCountersDyn, arg);
    pcB.gen_deref(arg);
  ELSE
    pcB.gen_usage (ObjCounters, arg);
  END;

  AddParameter (arg);

  pcall.obj := pcall.l.obj;
  pcall.l   := NIL;
END CompleteRTSCall;


--------------------------------------------------------------------------------
--                  Create nd_sproc call
--------------------------------------------------------------------------------
PROCEDURE CreateSprocCall * (sproc : pc.SUB_MODE): pc.NODE;
VAR n : pc.NODE;
BEGIN
  n      := NewNode(pc.nd_sproc);
  n.type := NewType(pc.ty_void);
  n.sub  := sproc;
  n.pos  := env.null_pos;
  n.end  := env.null_pos;
  RETURN n;
END CreateSprocCall;



--------------------------------------------------------------------------------
PROCEDURE cur_pos(VAR ps: pc.TPOS);
BEGIN
  ps := env.null_pos;
END cur_pos;

--------------------------------------------------------------------------------
PROCEDURE NewTCSModule * (modules: TModuleList);
VAR option_K26: BOOLEAN;
    option_GENDEBUG: BOOLEAN;
    option_LINENO: BOOLEAN;
   <* IF NOT TARGET_MIPS THEN *>
    option_GENHISTORY: BOOLEAN;
   <* END *>

    -- 1 -- NewTCSModule -------------------------------------------------------
    PROCEDURE setOption();
    BEGIN
      option_K26      := env.config.Option("K26");
      option_GENDEBUG := env.config.Option("GENDEBUG");
      option_LINENO   := env.config.Option("LINENO");
      env.config.SetOption("K26", FALSE);
      env.config.SetOption("GENDEBUG", FALSE);
      env.config.SetOption("LINENO", FALSE);
    <* IF NOT TARGET_MIPS THEN *>
      option_GENHISTORY  := env.config.Option("GENHISTORY");
      env.config.SetOption("GENHISTORY", FALSE);
    <* END *>
    END setOption;

    -- 1 -- NewTCSModule -------------------------------------------------------
    PROCEDURE restoreOption();
    BEGIN
      env.config.SetOption("K26", option_K26);
      env.config.SetOption("GENDEBUG", option_GENDEBUG);
      env.config.SetOption("LINENO", option_LINENO);
    <* IF NOT TARGET_MIPS THEN *>
      env.config.SetOption("GENHISTORY", option_GENHISTORY);
    <* END *>
    END restoreOption;

CONST lang = pc.flag_m2;
VAR cu: pc.OBJECT;
    modulename: DStrings.String;
    call: TCallRegistryModule;
    node : pc.NODE;
  <* IF TARGET_MIPS THEN *>
    str: DStrings.String;
  <* END *>
-- 0 -- NewTCSModule -----------------------------------------------------------
BEGIN
  pc.code.ini();
  pcO.ini(lang, cur_pos, pcS.o2_num_ext, pcS.ext());
  pcB.ini;

  IF (pc.mod_cnt = 0) THEN
    NEW(pc.mods, 1);
  ELSIF pc.mod_cnt >= LEN(pc.mods^) THEN
    pcO.resize_mods();
  END;

  cur_mod := pc.mod_cnt;
  INC(pc.mod_cnt);
  fe.RefreshPrimitiveTypes();

  modulename := MakeBeginName(FALSE);
  env.config.SetEquation(tcc.EQU_TCSMODULE, modulename^);
  cu := fe.new_obj(pc.ob_module);
  INCL(cu.marks, pc.omark_used_by_code);
<* IF NOT TARGET_VAX AND NOT dbg_tcs THEN *>
  INCL(cu.marks, pc.omark_no_debug);
<* END *>
  pcO.set_name(cu, modulename^);
  cu.type := fe.new_type(pc.ty_module);
  cu.type.obj := cu;
  cu.flag := lang;
  cu.type.flag := lang;
  cu.type.len := xfs.sys.VersionTag();
  cu.type.base := pcO.void;

  cu.mno := cur_mod;
  cu.type.mno := cur_mod;
  pc.mods[cur_mod] := cu;

  pcO.enter_scope(cu.type);
  pcO.new(cu.val, pc.nd_module);
  cu.val.obj := cu;
  cu.val.type := cu.type;
  pcO.new(cu.val.r, pc.nd_block);

  env.info.print("\n");
  WHILE modules # NIL DO
    env.info.print('TCS processed "%s"\n', modules.name^);
    node := call.NewCall (modules.name^);
    node.obj.mno := -1;
    node.next := cu.val.r.r;
    cu.val.r.r := node;
    modules := modules.next;
  END;
  env.info.print("\n");

  -- create begin flag check
  cu.type.mem := fe.new_obj(pc.ob_var);
  cu.type.mem.host := cu.type;
  INCL(cu.type.mem.marks, pc.omark_used_by_code);
  pcO.set_name(cu.type.mem, "begflag");
  cu.type.mem.type := pcO.boolean;

(*
                    NODE nd_if  [A5CCB0][test.mod:6:3]
                      type: [A344B0] ty_void
                      next: [A5CDF0] nd_assign
                         l: [A5CD30] nd_var
                         r: [A5CCF0] nd_node
                        ----
                        NODE nd_var  [A5CD30][test.mod:6:6]
                          type: [A781D0] ty_boolean
                           obj: [A37AB0] begflag
                        ----
                        NODE nd_node  [A5CCF0][test.mod:8:3]
                             l: [A5CD70] nd_return
                            ----
                            NODE nd_return  [A5CD70][test.mod:7:5]
                              type: [A344B0] ty_void
                                 r: [A5CC70] nd_block
                    ----
                    NODE nd_assign  [A5CDF0][test.mod:9:3]
                      type: [A344B0] ty_void
                       obj: [A37AB0] begflag
                         r: [A5CE30] nd_var
                        ----
                        NODE nd_var  [A5CE30][test.mod:9:14]
                          type: [A781D0] ty_boolean
                           obj: [A35590] TRUE
*)
  -- create assign
  pcO.new(node, pc.nd_assign);
  node.type := pcO.void;
  node.obj  := cu.type.mem;
  pcO.new(node.r, pc.nd_value);
  node.r.type := pcO.boolean;
  node.r.val  := pc.value.new(env.null_pos,pcO.boolean);
  node.next   := cu.val.r.r;
  cu.val.r.r  := node;

  -- create if
  pcO.new(node, pc.nd_if);
  node.type := pcO.void;
  pcO.new(node.l, pc.nd_var);
  node.l.type := pcO.boolean;
  node.l.obj  := cu.type.mem;
  pcO.new(node.r, pc.nd_node);
  pcO.new(node.r.l, pc.nd_return);
  node.r.l.r := cu.val.r;

  node.next := cu.val.r.r;
  cu.val.r.r := node;

(*
<* IF TARGET_MIPS THEN *>
  env.config.Equation (env.EQU_MAINNAME, str);

  IF (str # NIL) AND (str^ # "") AND ~env.config.Option(env.OPT_GHS) THEN
    cu.type.mem.next := NewProcedure("main", cu.type, pc.flag_c);
    cu.type.mem.next.type.flag := lang;

    pcO.new(cu.type.mem.next.val, pc.nd_block);

    pcO.new(cu.type.mem.next.val.r, pc.nd_call);
    cu.type.mem.next.val.r.obj := Get_ExternalProcedure(str^, cu.type, pc.flag_syscall);
    cu.type.mem.next.val.r.obj.type.flag := lang;
    cu.type.mem.next.val.r.obj.mno := -1;
  END;
<* END *>
*)

  setOption();

  pcO.exit_scope;

  cu.type.prof := cu.type.mem;
  cu.type.mem := NIL;

  pc.code.gen_code(cur_mod, FALSE);

  restoreOption();

  DEC(pc.mod_cnt);
  pc.code.exi();
  pcB.exi;
  pcO.exi;
END NewTCSModule;


--------------------------------------------------------------------------------
PROCEDURE Reset * ();
BEGIN
  ObjSYSModule  := NIL;
  ObjRTSModule  := NIL;

  ObjModuleInfo := NIL;
  ObjModuleName := NIL;

  ObjSourceRefs := NIL;
  ObjCounters   := NIL;
  ObjCountersDyn:= NIL;

  ModuleModifyTime := 0;
END Reset;


--------------------------------------------------------------------------------
BEGIN
  -- cut and paste from pcS
  NEW(type_string);      type_string.mode := pc.ty_SS;
  NEW(type_string.base); type_string.base.mode := pc.ty_char;
  type_string.len := 0;
END tcObjs.