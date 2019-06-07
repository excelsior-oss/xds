--------------------------------------------------------------------------------
--                      Excelsior XDS Test Coverage System
--                          (c) 2015, Excelsior Ltd.
-- Module:   tcInstr
-- Mission:  Code Instrumentation for Test Coverage.  
-- Synonym:  tci
-- Authors:  Lvov Konstantin
-- Created:  11-Dec-2002
--------------------------------------------------------------------------------
<* +o2addkwd *>
MODULE tcInstr;

<* IF    TARGET_386  THEN *> FROM  xrTCSx86   IMPORT  TestConditionType;
<* ELSIF TARGET_PPC  THEN *>  FROM  xrTCSppc   IMPORT  TestConditionType;
<* ELSIF TARGET_MIPS THEN *> FROM  xrTCSmips  IMPORT  TestConditionType;
<* ELSIF TARGET_VAX  THEN *> FROM  xrTCSvax   IMPORT  TestConditionType;
<* ELSIF TARGET_SPARC THEN *> FROM  xrTCSsparc IMPORT  TestConditionType;
<* END *>

IMPORT  env := xiEnv,          pc   := pcK
     ,  tco := tcObjs,         fe   := tcFEproxy
     ,  sys := SYSTEM,         dstr := DStrings
     ,  pcO,                   tcLib
<* IF    TARGET_386  THEN *>,  tc := xrTCSx86
<* ELSIF TARGET_PPC   THEN *>,  tc := xrTCSppc
<* ELSIF TARGET_MIPS THEN *>,  tc := xrTCSmips
<* ELSIF TARGET_VAX  THEN *>,  tc := xrTCSvax
<* ELSIF TARGET_SPARC THEN *>,  tc := xrTCSsparc
<* END *>
     ;

--------------------------------------------------------------------------------
TYPE
  INT = sys.INT32;

  TRTSCall = RECORD
    pcall: pc.NODE;
    heavy: BOOLEAN;
  END;
  TRTSCalls = POINTER TO ARRAY OF TRTSCall;

  TPocedureDescIndex   = INT;
  TPocedureDescCounter = TPocedureDescIndex;

  TPocedureDesc = RECORD
    host: TPocedureDescIndex;
    name: dstr.String;
  END;

  TPocedureDescList   = POINTER TO ARRAY OF TPocedureDesc;
  TProcNames  = POINTER TO ARRAY OF CHAR;

  TProcedures = RECORD
    proc:     tc.TIndex;
    current:  TPocedureDescIndex;
    count:    TPocedureDescCounter;
    list:     TPocedureDescList;
  END;

CONST
  UNDEFINED_INDEX = tc.TIndex{-1};

VAR
  -- test condition counters number
  TestCount - : tc.TCounter;
  -- test condition source code reference
  SourceRefs: tco.TSourceRefs;

  Modules - : tco.TModuleList;
  Procedures: TProcedures;

  RTSCallCount - : tc.TCounter;
  RTSCalls       : TRTSCalls;


--------------------------------------------------------------------------------
PROCEDURE MakeTestCondition ( type: TestConditionType
                            ; pos-, end_pos-: pc.TPOS ): tc.TIndex;

    -- 1 -- MakeTestCondition --------------------------------------------------
    PROCEDURE  AdjustSourceRefsStorage ();
    VAR tmp: tco.TSourceRefs;
        len, i: INT;
    BEGIN
      IF SourceRefs = NIL THEN
        NEW(SourceRefs, 5000);
      ELSIF TestCount >= LEN(SourceRefs^) THEN
        tmp := SourceRefs;
        len := LEN(tmp^);
        NEW(SourceRefs, len*2);
        FOR i := 0 TO len-1 DO
          SourceRefs[i] := tmp[i];
        END;
      END;
    END AdjustSourceRefsStorage;


-- 0 -- MakeTestCondition ------------------------------------------------------
VAR fname: env.String;
    line, end_line: tc.TSourceLine;
    col, end_col: tc.TSourceCol;
    index: tc.TIndex;
BEGIN
  pos.unpack (fname, line, col);
  end_pos.unpack (fname, end_line, end_col);

  AdjustSourceRefsStorage ();
  index := TestCount;
  INC(TestCount);

  SourceRefs[index].proc     := Procedures.proc;
  SourceRefs[index].line     := line+1;
  SourceRefs[index].col      := col+1;
  SourceRefs[index].end_line := end_line+1;
  SourceRefs[index].end_col  := end_col+1;
  SourceRefs[index].type     := type;

  RETURN index;
END MakeTestCondition;



--------------------------------------------------------------------------------
PROCEDURE SaveIncompleteRTSCall ( pcall: pc.NODE
                                ; UseHeavyRTSCall: BOOLEAN );

    -- 1 --  SaveIncompleteRTSCall ---------------------------------------------
    PROCEDURE  AdjustRTSCallsStorage ();
    VAR tmp: TRTSCalls;
        len, i: INT;
    BEGIN
      IF RTSCalls = NIL THEN
        NEW(RTSCalls, 5000);
      ELSIF RTSCallCount >= LEN(RTSCalls^) THEN
        tmp := RTSCalls;
        len := LEN(tmp^);
        NEW(RTSCalls, len*2);
        FOR i := 0 TO len-1 DO
          RTSCalls[i] := tmp[i];
        END;
      END;
    END AdjustRTSCallsStorage;


-- 0 --  SaveIncompleteRTSCall -------------------------------------------------
BEGIN
  IF pcall = NIL THEN RETURN END;

  AdjustRTSCallsStorage ();
  RTSCalls[RTSCallCount].pcall := pcall;
  RTSCalls[RTSCallCount].heavy := UseHeavyRTSCall;
  INC(RTSCallCount);
END SaveIncompleteRTSCall;


--------------------------------------------------------------------------------
PROCEDURE setCurrentProcedure (index: TPocedureDescIndex);
VAR i: TPocedureDescIndex;
    proc: tc.TIndex;
BEGIN
  Procedures.current := index;
  IF index = UNDEFINED_INDEX THEN
    Procedures.proc := UNDEFINED_INDEX;
  ELSE
    proc := 0;
    FOR i := 0 TO index-1 DO
      INC(proc, LENGTH(Procedures.list[i].name^)+1);
    END;
    Procedures.proc := proc;
  END;
END setCurrentProcedure;

--------------------------------------------------------------------------------
PROCEDURE EnterProcedure * (proc-: pc.OBJECT);

    -- 1 -- EnterProcedure -----------------------------------------------------
    PROCEDURE AdjustProceduresStorage ();
    VAR tmp: TPocedureDescList;
        len: TPocedureDescCounter;
        i: TPocedureDescIndex;
    BEGIN
      IF Procedures.list = NIL THEN
        NEW(Procedures.list, 100);
      ELSIF Procedures.count >= LEN(Procedures.list^) THEN
        tmp := Procedures.list;
        len := LEN(tmp^);
        NEW(Procedures.list, len*2);
        FOR i := 0 TO len-1 DO
          Procedures.list[i] := tmp[i];
        END;
      END;
    END AdjustProceduresStorage;

-- 0 -- EnterProcedure ---------------------------------------------------------
VAR name: dstr.String;
    obj: pc.OBJECT;
BEGIN
  AdjustProceduresStorage();
  dstr.Assign("", name);
  IF proc.owner # NIL THEN
    obj := proc.owner(pc.OBJECT);
    WHILE obj # NIL DO
      dstr.Append(obj.name^, name);
      dstr.Append(".", name);
      obj := obj.next;
    END;
  END;
  CASE proc.mode OF
  | pc.ob_module:
      IF proc # pcO.cur_scope.obj THEN
        dstr.Append(proc.name^, name);
        dstr.Append(".", name);
      END;
      dstr.Append("BEGIN", name);
  | pc.ob_xproc:
      IF proc.host.mode = pc.ty_record THEN
        IF proc.host.obj.name[0] # "$" THEN
          dstr.Append(proc.host.obj.name^, name);
        ELSE
          dstr.Append(proc.type.prof.type.obj.name^, name);
        END;
        dstr.Append("::", name);
        dstr.Append(proc.name^, name);
      ELSE
        dstr.Append(proc.name^, name);
      END;
  ELSE
      dstr.Append(proc.name^, name);
  END;
  IF (Procedures.current = UNDEFINED_INDEX)
  OR (pcO.cur_scope.mode = pc.ty_module)
  THEN
    dstr.Assign(name^, Procedures.list[Procedures.count].name);
  ELSE
    dstr.Assign( Procedures.list[Procedures.current].name^
               , Procedures.list[Procedures.count].name );
    dstr.Append(".", Procedures.list[Procedures.count].name);
    dstr.Append(name^, Procedures.list[Procedures.count].name);
  END;
  Procedures.list[Procedures.count].host := Procedures.current;
  setCurrentProcedure(Procedures.count);
  INC(Procedures.count);
END EnterProcedure;

--------------------------------------------------------------------------------
PROCEDURE ExitProcedure * ();
BEGIN
  setCurrentProcedure(Procedures.list[Procedures.current].host);
END ExitProcedure;

--------------------------------------------------------------------------------
PROCEDURE getProcedureNames (): TProcNames;
VAR i: TPocedureDescIndex;
    j: INT;
    k: tc.TIndex;
    len: TPocedureDescCounter;
    name: dstr.String;
    proc_names: TProcNames;
BEGIN
  ASSERT( Procedures.current = UNDEFINED_INDEX );
  len := 0;
  FOR i := 0 TO Procedures.count-1 DO
    INC(len, LENGTH(Procedures.list[i].name^)+1);
  END;
  k := 0;
  IF len = 0 THEN
    NEW(proc_names, 1);
  ELSE
    NEW(proc_names, len+1);
    FOR i := 0 TO Procedures.count-1 DO
      name := Procedures.list[i].name;
      FOR j := 0 TO LENGTH(name^)-1 DO
         proc_names[k] := name[j];
         INC(k);
      END;
      proc_names[k] := tc.PROCNAME_SEPARATOR;
      INC(k);
    END;
  END;
  proc_names[k] := 0C;
  RETURN proc_names;
END getProcedureNames;


--------------------------------------------------------------------------------
-- добавляет модуль в список инструментированных модулей
PROCEDURE AddInstrumentedModules * (name-: ARRAY OF CHAR);
VAR module: tco.TModuleList;
BEGIN
  -- checks duplicates
  module := Modules;
  WHILE module # NIL DO
    IF module.name^ = name THEN
      RETURN;
    END;
    module := module.next;
  END;
  NEW(module);
  module.next := Modules;
  dstr.Assign(name, module.name);
  Modules := module;
END AddInstrumentedModules;

--------------------------------------------------------------------------------
-- удалить модуль из списока инструментированных модулей
PROCEDURE RemoveInstrumentedModules * (name-: ARRAY OF CHAR);
VAR module: tco.TModuleList;
BEGIN
  -- checks duplicates
  IF Modules = NIL THEN
    RETURN;
  ELSIF Modules.name^ = name THEN
    Modules := Modules.next;
  ELSE
    module := Modules;
    WHILE module.next # NIL DO
      IF module.next.name^ = name THEN
        module.next := module.next.next;
        RETURN;
      END;
      module := module.next;
    END;
  END;
END RemoveInstrumentedModules;

--------------------------------------------------------------------------------
PROCEDURE CreateTCSModule * ();
BEGIN
  IF Modules = NIL THEN
    RETURN;
  END;
  tco.NewTCSModule(Modules);
END CreateTCSModule;


--------------------------------------------------------------------------------
PROCEDURE FinalizeRTSCalls * (dynamic : BOOLEAN);
VAR i: INT;
BEGIN
  IF RTSCalls # NIL THEN
    FOR i := 0 TO RTSCallCount-1 DO
      tco.CompleteRTSCall ( RTSCalls[i].pcall
                          , RTSCalls[i].heavy
                          , dynamic);
    END;
  END;
END FinalizeRTSCalls;


--------------------------------------------------------------------------------
PROCEDURE Create_Procedure * ( name-: ARRAY OF CHAR
                             ; host: pc.STRUCT
                             ; flag: fe.Lang
                             ; pos-, end_pos-: pc.TPOS ): pc.OBJECT;
VAR proc: pc.OBJECT;
BEGIN
  proc := tco.NewProcedure(name, host, flag);
  proc.val.pos := pos;
  proc.val.end := end_pos;
  RETURN proc;
END Create_Procedure;


--------------------------------------------------------------------------------
PROCEDURE Create_TCSModuleBeginCall * (pos-, end_pos-: pc.TPOS): pc.NODE;
VAR pcall: pc.NODE;
    call: tco.TTCSModuleBeginCall;
BEGIN
  pcall := call.NewCall(pcO.module_xds.type);
  pcall.pos := pos;
  pcall.end := end_pos;
  RETURN pcall;
END Create_TCSModuleBeginCall;


--------------------------------------------------------------------------------
-- Привязка к исходному коду без всяких счетчиков
PROCEDURE Create_Anchor * ( type: TestConditionType
                          ; pos-, end_pos-: pc.TPOS );
BEGIN
  sys.EVAL( MakeTestCondition (type, pos, end_pos) );
END Create_Anchor;


--------------------------------------------------------------------------------
-- Счетчик, начало последовательности некоторых операторов
PROCEDURE Create_Counter * ( type: TestConditionType
                           ; pos-, end_pos-: pc.TPOS
                           ; UseHeavyRTSCall:=FALSE: BOOLEAN ): pc.NODE;
VAR pcall: pc.NODE;
    index: tc.TIndex;
    call: tco.TIncreaseCounter;
BEGIN
  index := MakeTestCondition (type, pos, end_pos);
  pcall := call.NewCall(index, UseHeavyRTSCall);
  pcall.pos := pos;
  pcall.end := end_pos;
--  SaveIncompleteRTSCall (pcall, UseHeavyRTSCall);
  RETURN pcall;
END Create_Counter;

--------------------------------------------------------------------------------
-- Счетчик условий, сколько раз было TRUE, сколько раз FALSE
PROCEDURE Create_ConditonCounter * ( type_true, type_false: TestConditionType
                                   ; condition: pc.NODE
                                   ; pos-, end_pos-: pc.TPOS
                                   ; false_pos-, false_end_pos-: pc.TPOS ): pc.NODE;
VAR pcall: pc.NODE;
    index_true, index_false: tc.TIndex;
    call: tco.TIncreaseConditionCounter;
BEGIN
  index_true  := MakeTestCondition (type_true, pos, end_pos);
  index_false := MakeTestCondition (type_false, false_pos, false_end_pos);
  pcall := call.NewCall (condition, index_true, index_false);
  pcall.pos := pos;
  pcall.end := end_pos;

--  SaveIncompleteRTSCall (pcall, FALSE (*UseHeavyRTSCall*));
  RETURN pcall;
END Create_ConditonCounter;

--------------------------------------------------------------------------------
-- Счетчик условий, сколько раз было TRUE, сколько раз FALSE
PROCEDURE Create_RelationCounter * ( condition: pc.NODE
                                   ; pos-, end_pos-: pc.TPOS ): pc.NODE;

    -- 1 -- Create_RelationCounter ---------------------------------------------
    PROCEDURE isSigned (node: pc.NODE): BOOLEAN;
    BEGIN
      RETURN (node.type.mode IN pc.INTs);
    END isSigned;

    -- 1 -- Create_RelationCounter ---------------------------------------------
    PROCEDURE isUnsigned (node: pc.NODE): BOOLEAN;
    BEGIN
      RETURN (node.type.mode # pc.ty_ZZ)
           & (node.type.mode IN pc.CARDs);
    END isUnsigned;

    -- 1 -- Create_RelationCounter ---------------------------------------------
    PROCEDURE hasSideEffects (node: pc.NODE): BOOLEAN;
    BEGIN
      RETURN (node # NIL)
           & (  (node.mode = pc.nd_call)
             OR hasSideEffects(node.l)
             OR hasSideEffects(node.r)
             );
    END hasSideEffects;

-- 0 -- Create_RelationCounter -------------------------------------------------
VAR pcall: pc.NODE;
    index: tc.TIndex;
    call: tco.TIncreaseRelationCounter;
    call_signed: tco.TIncreaseRelationCounterSigned;
    call_unsigned: tco.TIncreaseRelationCounterUnsigned;
BEGIN
  index := MakeTestCondition (tc_C_Expr_equal, pos, end_pos);

  IF hasSideEffects(condition) THEN
    IF isUnsigned(condition.l) OR isUnsigned(condition.r) THEN
      pcall := call_unsigned.NewCall (condition, index);
     ELSIF isSigned(condition.l) OR isSigned(condition.r) THEN
      pcall := call_signed.NewCall (condition, index);
    ELSE
      ASSERT(FALSE);
    END;
  ELSE
    pcall := call.NewCall (condition, index);
  END;

  pcall.pos := pos;
  pcall.end := end_pos;
  RETURN pcall;
END Create_RelationCounter;

--------------------------------------------------------------------------------
-- Локальная переменная для подсчтета количество последовательных итераций цикла
-- и счетчики для подсчтета количество последовательных итераций цикла
PROCEDURE Create_IterationCounter * ( type_0: TestConditionType
                                    ; VAR local_counter: pc.OBJECT
                                    ; pos-, end_pos-: pc.TPOS
                                    ; VAR pcall_init, pcall: pc.NODE
                                    );
VAR obj_name: tco.String;
    type_1, type_N: TestConditionType;
    index_0, index_1, index_N: tc.TIndex;
    call_init: tco.TInitIterationCounter;
    call: tco.TIncreaseIterationCounter;
BEGIN
  -- create iteration local counter
  CASE type_0 OF
  | tc_C_While_itr_0:
      type_1   := tc_C_While_itr_1;
      type_N   := tc_C_While_itr_N;
      obj_name := tco.MakeUniqueName ("X2C_TCS_itr_while");
  | tc_Info_Repeat_itr_0:
      type_1   := tc_C_Repeat_itr_1;
      type_N   := tc_C_Repeat_itr_N;
      obj_name := tco.MakeUniqueName ("X2C_TCS_itr_repeat");
  | tc_C_For_itr_0
  , tc_Info_For_itr_0:
      type_1   := tc_C_For_itr_1;
      type_N   := tc_C_For_itr_N;
      obj_name := tco.MakeUniqueName ("X2C_TCS_itr_for");
  | tc_Info_For_itr_1:
      type_0   := tc_Info_For_itr_0;
      type_1   := tc_Info_For_itr_1;
      type_N   := tc_C_For_itr_N;
      obj_name := tco.MakeUniqueName ("X2C_TCS_itr_for");
  | tc_Info_Loop_itr_0:
      type_1   := tc_C_Loop_itr_1;
      type_N   := tc_C_Loop_itr_N;
      obj_name := tco.MakeUniqueName ("X2C_TCS_itr_loop");
  END;
  local_counter := tco.NewVariable ( obj_name^
                                   , tco.ObjRTSModule
                                   , "TCounter" );

  index_0 := MakeTestCondition (type_0, pos, end_pos);
  index_1 := MakeTestCondition (type_1, pos, end_pos);
  index_N := MakeTestCondition (type_N, pos, end_pos);

  pcall_init := call_init.NewCall(index_0, local_counter);
  pcall_init.pos := pos;
  pcall_init.end := end_pos;

  pcall     := call.NewCall(index_0, index_1, index_N, local_counter);
  pcall.pos := pos;
  pcall.end := end_pos;
END Create_IterationCounter;

--------------------------------------------------------------------------------
PROCEDURE Create_DecreaseVariableValue * ( pos-, end_pos-: pc.TPOS
                                         ; counter: pc.OBJECT ): pc.NODE;
VAR
  dec_call: tco.TDecreaseVariableValue;
  pcall: pc.NODE;
BEGIN
  pcall := dec_call.NewCall(counter);
  pcall.pos := pos;
  pcall.end := end_pos;
  RETURN pcall;
END Create_DecreaseVariableValue;

--------------------------------------------------------------------------------
PROCEDURE Create_DecreaseRecursionDepth * ( pos-, end_pos-: pc.TPOS
                                          ; counter: pc.OBJECT
                                          ; return_flag: pc.OBJECT ): pc.NODE;
VAR
  dec_call: tco.TDecreaseRecursionDepth;
  pcall: pc.NODE;
BEGIN
  pcall := dec_call.NewCall(counter, return_flag);
  pcall.pos := pos;
  pcall.end := end_pos;
  RETURN pcall;
END Create_DecreaseRecursionDepth;

--------------------------------------------------------------------------------
PROCEDURE Create_RecursionCounter * ( pos-, end_pos-: pc.TPOS
                                    ; VAR counter: pc.OBJECT
                                    ; VAR return_flag: pc.OBJECT
                                    ; VAR pcall_init, pcall_inc: pc.NODE
                                    );
VAR
  obj_name: tco.String;
  index_0, index_1, index_N: tc.TIndex;
  call_init: tco.TInitRecursionDepth;
  inc_call: tco.TIncreaseRecursionDepth;
BEGIN
  -- create global recursion depth counter
  obj_name := tco.MakeUniqueName ("X2C_TCS_recur");
  counter := tco.NewGlobalVariable ( obj_name^
                                   , tco.ObjRTSModule
                                   , "TCounter" );

  -- create global recursion return flag
  obj_name := tco.MakeUniqueName ("X2C_TCS_recur_return");
  return_flag := tco.NewGlobalVariable ( obj_name^
                                       , NIL
                                       , "BOOLEAN" );

  index_0 := MakeTestCondition (tc_C_Recur_0, pos, end_pos);
  index_1 := MakeTestCondition (tc_C_Recur_1, pos, end_pos);
  index_N := MakeTestCondition (tc_C_Recur_N, pos, end_pos);

  pcall_init := call_init.NewCall(counter, return_flag);
  pcall_init.pos := pos;
  pcall_init.end := end_pos;

  pcall_inc := inc_call.NewCall(index_0, index_1, index_N, counter, return_flag);
  pcall_inc.pos := pos;
  pcall_inc.end := end_pos;
END Create_RecursionCounter;

--------------------------------------------------------------------------------
PROCEDURE Create_CaseTrap * ( pos-, end_pos-: pc.TPOS): pc.NODE;
VAR
  n : pc.NODE;
BEGIN
  n := tco.CreateSprocCall(pc.sp_casetrap);
  n.pos  := pos;
  n.end  := end_pos;
  RETURN n;
END Create_CaseTrap;
--------------------------------------------------------------------------------
--                         Module Registration
--------------------------------------------------------------------------------

PROCEDURE AddModuleRegistration * (VAR ph: pc.NODE; dynamic : BOOLEAN);
VAR rts_call: pc.NODE;
    call: tco.TRegistryModule;
BEGIN
  call.SetDynamicMode(dynamic);
  rts_call := call.NewCall ();
  rts_call.next := ph;
  ph := rts_call;
END AddModuleRegistration;


--------------------------------------------------------------------------------
--                 Add Global Constants and Variables
--------------------------------------------------------------------------------

TYPE   TTime * = tc.TTime;

--------------------------------------------------------------------------------
PROCEDURE ModuleModifyTime * (time: TTime);
BEGIN
  tco.ModuleModifyTime := time;
END ModuleModifyTime;

--------------------------------------------------------------------------------
PROCEDURE CRCSum * (crcsum: sys.CARD32);
BEGIN
  tco.CRCSum := sys.VAL(tc.TCRCSum, crcsum);
END CRCSum;

--------------------------------------------------------------------------------
PROCEDURE AddGlobalConst_ModuleName * (fname: ARRAY OF CHAR);
VAR obj_name: tco.String;
    str: dstr.String;
BEGIN
  obj_name := tco.MakeUniqueName ("X2C_TC_ModName");
  dstr.Assign(fname, str);
  tcLib.ReplaceAllSlashes(str^);
  tco.ObjModuleName := tco.NewConst_String (obj_name^, str^);
END AddGlobalConst_ModuleName;


--------------------------------------------------------------------------------
PROCEDURE AddGlobalVar_ModuleInfo * ();
VAR obj_name: tco.String;
BEGIN
  obj_name := tco.MakeUniqueName ("X2C_TC_ModuleInfo");
  tco.ObjModuleInfo := tco.NewVariable ( obj_name^
                                       , tco.ObjRTSModule
                                       , "TModuleInfo" );
END AddGlobalVar_ModuleInfo;


--------------------------------------------------------------------------------
PROCEDURE AddGlobalSourceRefsAndCounters * (dynamic: BOOLEAN);
VAR obj_name, type_name: tco.String;
    proc_names: TProcNames;
BEGIN
  -- create Test Condition Counters
  type_name := tco.MakeUniqueName ("X2C_TC_TCounters");
  tco.AddArrayType (type_name^, TestCount, "TCounter");
  IF (dynamic) THEN
    obj_name := tco.MakeUniqueName ("X2C_TC_Counters_Dyn");
    tco.ObjCountersDyn := tco.NewVariablePtr (obj_name^, type_name^);
  ELSE
    obj_name := tco.MakeUniqueName ("X2C_TC_Counters");
    tco.ObjCounters := tco.NewVariable (obj_name^, NIL, type_name^);
  END;

  -- create Source Code Refrences
  type_name := tco.MakeUniqueName ("X2C_TC_TSourceRefs");
  tco.AddArrayType (type_name^, TestCount, "TSourceRef");

  obj_name := tco.MakeUniqueName ("X2C_TC_SourceRefs");
  tco.ObjSourceRefs := tco.NewConst_SourceRefs (obj_name^, SourceRefs, type_name^);

  -- create Procedure Names
  proc_names := getProcedureNames();
  obj_name := tco.MakeUniqueName ("X2C_TC_ProcNames");
  tco.ObjProcedureNames := tco.NewConst_String (obj_name^, proc_names^);
END AddGlobalSourceRefsAndCounters;


--------------------------------------------------------------------------------
--                     Extend Module Import Section
--------------------------------------------------------------------------------

PROCEDURE ImportModule (cu: pc.OBJECT; modname-: ARRAY OF CHAR): pc.OBJECT;
VAR new_mod: pc.OBJECT;
    import: pc.USAGE;
BEGIN
  pcO.inp_sym_file (modname, FALSE, FALSE, new_mod);
  IF new_mod.mno < fe.ZEROMno THEN RETURN NIL END;

  -- check module in import list
  import := cu.type.use;
  WHILE import # NIL DO
    IF import.obj = new_mod THEN RETURN new_mod END;
    import := import.next;
  END;

  -- add module in import list
  NEW(import);
  import.obj  := new_mod;
  import.tags := fe.UTAG_SET{};
  import.next := cu.type.use;
  cu.type.use := import;

  pcO.dcl_ref (cu.type, new_mod);
  RETURN new_mod;
END ImportModule;


--------------------------------------------------------------------------------
PROCEDURE ExtendModuleImport * (cu: pc.OBJECT);
<* IF    TARGET_386  THEN *> CONST RTS_MODULE_NAME = "xrTCSx86";
<* ELSIF TARGET_PPC   THEN *> CONST RTS_MODULE_NAME = "xrTCSppc";
<* ELSIF TARGET_MIPS THEN *> CONST RTS_MODULE_NAME = "xrTCSmips";
<* ELSIF TARGET_SPARC THEN *> CONST RTS_MODULE_NAME = "xrTCSsparc";
<* ELSIF TARGET_VAX  THEN *> CONST RTS_MODULE_NAME = "xrTCSvax";
<* END *>
BEGIN
  tco.ObjRTSModule := ImportModule (cu, RTS_MODULE_NAME);
  pcO.inp_sym_file ("SYSTEM", FALSE, FALSE, tco.ObjSYSModule);
END ExtendModuleImport;


--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

PROCEDURE Reset * ();
VAR i: INT;
BEGIN
  tco.Reset ();

  TestCount := 0;

  Procedures.count   := 0;
  Procedures.current := UNDEFINED_INDEX;
  Procedures.list    := NIL;

  IF RTSCalls # NIL THEN
    FOR i := 0 TO RTSCallCount-1 DO
      RTSCalls[i].pcall := NIL;
    END;
  END;
  RTSCallCount := 0;
END Reset;


--------------------------------------------------------------------------------
BEGIN
  SourceRefs := NIL;
  RTSCalls   := NIL;
  tco.SaveIncompleteRTSCall := SaveIncompleteRTSCall;
END tcInstr.