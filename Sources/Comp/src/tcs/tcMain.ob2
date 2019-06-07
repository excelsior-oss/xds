--------------------------------------------------------------------------------
--                      Excelsior XDS Test Coverage System
--                          (c) 2015, Excelsior Ltd.
-- Module:   tcMain
-- Mission:  Main module of programm instrumentation for Test Coverage. 
-- Authors:  Lvov Konstantin
-- Created:  06-Dec-2002
--
-- Performs programm tree traversal.
-- First process expression, then create test condition for statement, 
-- after that process statement and finaly add instrumented code which 
-- created on step 2 to expression.
--
-- It's a part of Excelsior XDS O2/M2 compiler. 
-- Set option '-target_testcoverage:+' to build in this tool into compiler.
--------------------------------------------------------------------------------
<* +o2addkwd *>
MODULE tcMain;

<* IF    TARGET_386  THEN *> FROM  xrTCSx86   IMPORT  TestConditionType;
<* ELSIF TARGET_PPC   THEN *> FROM  xrTCSppc   IMPORT  TestConditionType;
<* ELSIF TARGET_MIPS THEN *> FROM  xrTCSmips  IMPORT  TestConditionType;
<* ELSIF TARGET_VAX  THEN *> FROM  xrTCSvax   IMPORT  TestConditionType;
<* ELSIF TARGET_SPARC THEN *> FROM  xrTCSsparc IMPORT  TestConditionType;
<* END *>

IMPORT  sys := SYSTEM,       env := xiEnv,             pcO
     ,  tci := tcInstr,      tcc := tcConfig,          fe := tcFEproxy
     ,  xfs := xiFiles,      pc  := pcK,               pcMarker
     ;

VAR UseModuleConstructor: BOOLEAN;
    ModuleBudyExist: BOOLEAN;

--------------------------------------------------------------------------------
PROCEDURE SkipInstrumentations (pos-: pc.TPOS): BOOLEAN;
BEGIN
  RETURN NOT env.config.OptionAt (pos, env.OPT_TESTCOVERAGE);
END SkipInstrumentations;

--------------------------------------------------------------------------------
PROCEDURE SetInstrCodePos (icode: pc.NODE; pos: pc.TPOS);
BEGIN
  WHILE icode # NIL DO
    icode.pos := pos;
    icode.end := pos;
    icode := icode.next;
  END;
END SetInstrCodePos;

--------------------------------------------------------------------------------
PROCEDURE AppendNode (VAR dest: pc.NODE; src: pc.NODE);
VAR n: pc.NODE;
BEGIN
  IF src = NIL THEN  RETURN END;

  IF dest = NIL THEN
    dest := src;
  ELSE
    n := dest;
    WHILE n.next # NIL DO
      n := n.next;
    END;
    n.next := src;
  END;
END AppendNode;

--------------------------------------------------------------------------------
PROCEDURE InsertInstrCode_right (node: pc.NODE; icode: pc.NODE);
BEGIN
  IF icode = NIL THEN  RETURN END;

  SetInstrCodePos(icode, node.pos);
  AppendNode(icode, node.r);
  node.r := icode;
END InsertInstrCode_right;

--------------------------------------------------------------------------------
PROCEDURE InsertInstrCode_left (node: pc.NODE; icode: pc.NODE);
BEGIN
  IF icode = NIL THEN  RETURN END;

  SetInstrCodePos(icode, node.pos);
  AppendNode(icode, node.l);
  node.l := icode;
END InsertInstrCode_left;

--------------------------------------------------------------------------------
PROCEDURE InsertInstrCode_next (node: pc.NODE; icode: pc.NODE);
BEGIN
  IF icode = NIL THEN  RETURN END;

  SetInstrCodePos(icode, node.end);
  AppendNode(icode, node.next);
  node.next := icode;
END InsertInstrCode_next;


--------------------------------------------------------------------------------
--                       Expression Process
--------------------------------------------------------------------------------

PROCEDURE ^ ProcessExpression_right (stmt: pc.NODE; root:=FALSE: BOOLEAN);
PROCEDURE ^ ProcessExpression_left  (stmt: pc.NODE; root:=FALSE: BOOLEAN);

--------------------------------------------------------------------------------
TYPE
  TExpressionChild = (echLeft, echRight, echNext);

  TExpression = RECORD
    root:   BOOLEAN;          -- root of expression, parent is a statemet
    parent: pc.NODE;
    node:   pc.NODE;          -- current expression
    child:  TExpressionChild; -- "node" is left or right son of "parent"
  END;

--------------------------------------------------------------------------------
PROCEDURE (VAR expr: TExpression) Assign_left (parent: pc.NODE);
BEGIN
  expr.root   := FALSE;
  expr.parent := parent;
  expr.node   := parent.l;
  expr.child  := echLeft;
END Assign_left;

--------------------------------------------------------------------------------
PROCEDURE (VAR expr: TExpression) Assign_right (parent: pc.NODE);
BEGIN
  expr.root   := FALSE;
  expr.parent := parent;
  expr.node   := parent.r;
  expr.child  := echRight;
END Assign_right;

--------------------------------------------------------------------------------
PROCEDURE (VAR expr: TExpression) Assign_next (parent: pc.NODE);
BEGIN
  expr.root   := FALSE;
  expr.parent := parent;
  expr.node   := parent.next;
  expr.child  := echNext;
END Assign_next;

--------------------------------------------------------------------------------
PROCEDURE (VAR expr: TExpression) ReplaceByInstrCode_top (icode: pc.NODE);
BEGIN
  IF icode = NIL THEN  RETURN END;

  ASSERT( expr.parent # NIL );
  CASE expr.child OF
  | echLeft:
       expr.parent.l    := icode;
  | echRight:
       expr.parent.r    := icode;
  | echNext:
       expr.parent.next := icode;
  END;
  expr.node  := icode;
END ReplaceByInstrCode_top;


--------------------------------------------------------------------------------
--                       Expression Traversal
--------------------------------------------------------------------------------

PROCEDURE (VAR expr: TExpression) ProcessBoolean ();

   -- 1 -- ProcessBoolean ------------------------------------------------------
   PROCEDURE getStartPos (n: pc.NODE): pc.TPOS;
   BEGIN
     WHILE n.mode = pc.nd_binary DO
       n := n.l;
     END;
     RETURN n.pos;
   END getStartPos;

   -- 1 -- ProcessBoolean ------------------------------------------------------
   PROCEDURE isComplex (n: pc.NODE): BOOLEAN;
   BEGIN
     IF (n.mode = pc.nd_unary) AND (n.sub = pc.su_not) THEN
       n := n.l;
     END;
     RETURN (n.mode = pc.nd_binary)
        AND (  (n.sub = pc.sb_and)
            OR (n.sub = pc.sb_or)
            OR (n.sub = pc.sb_cor)
            OR (n.sub = pc.sb_cand)
            )
        ;
   END isComplex;

   -- 1 -- ProcessBoolean ------------------------------------------------------
   PROCEDURE isWholeRelation (n: pc.NODE): BOOLEAN;
   BEGIN
     IF (n.mode = pc.nd_binary) THEN
       CASE n.sub OF
       | pc.sb_gtr, pc.sb_geq, pc.sb_lss, pc.sb_leq:
         RETURN (n.l.type.mode IN pc.WHOLEs)
              & (n.r.type.mode IN pc.WHOLEs);
       ELSE
         RETURN FALSE;
       END;
     END;
     RETURN FALSE;
   END isWholeRelation;

-- 0 -- ProcessBoolean ---------------------------------------------------------
VAR
  node, icode, restoreNext: pc.NODE;
BEGIN
  node := expr.node;
  IF node = NIL THEN RETURN END;

  ASSERT(node.type.mode = pc.ty_boolean);

  IF isComplex(node) THEN
    ProcessExpression_left(node);
    ProcessExpression_right(node);
    RETURN;
  END;

  restoreNext := node.next; -- tci.Create_Conditon..() могут портить next

  IF isWholeRelation(node) THEN
    icode := tci.Create_RelationCounter(node, getStartPos(node), node.r.end);
    expr.ReplaceByInstrCode_top(icode);
  END;

  IF NOT expr.root THEN
    icode := tci.Create_ConditonCounter( tc_C_Expr_true, tc_C_Expr_false
                                       , expr.node
                                       , getStartPos(node), node.end
                                       , getStartPos(node), node.end );
    expr.ReplaceByInstrCode_top(icode);
  END;

  expr.node.next := restoreNext;
END ProcessBoolean;


--------------------------------------------------------------------------------
PROCEDURE (VAR expr: TExpression) Process ();
VAR
  node: pc.NODE;
BEGIN
  node := expr.node;

  IF (node = NIL) THEN
    RETURN;
  ELSIF (node.type.mode = pc.ty_boolean) THEN
    expr.ProcessBoolean();
  END;
END Process;

--------------------------------------------------------------------------------
PROCEDURE ProcessExpression_right (node: pc.NODE; root:=FALSE: BOOLEAN);
VAR expr: TExpression;
BEGIN
  IF node = NIL THEN RETURN END;

  expr.Assign_right (node);
  expr.root := root;
  expr.Process ();
END ProcessExpression_right;

--------------------------------------------------------------------------------
PROCEDURE ProcessExpression_left (node: pc.NODE; root:=FALSE: BOOLEAN);
VAR expr: TExpression;
BEGIN
  IF node = NIL THEN RETURN END;

  expr.Assign_left (node);
  expr.root := root;
  expr.Process ();
END ProcessExpression_left;

--------------------------------------------------------------------------------
PROCEDURE ProcessExpression_next (node: pc.NODE; root:=FALSE: BOOLEAN);
VAR expr: TExpression;
BEGIN
  IF node = NIL THEN RETURN END;

  expr.Assign_next (node);
  expr.root := root;
  expr.Process ();
END ProcessExpression_next;



--------------------------------------------------------------------------------
--                       Statement Process
--------------------------------------------------------------------------------

PROCEDURE ^ ProcessStatement_right (stmt: pc.NODE);
PROCEDURE ^ ProcessStatement_left  (stmt: pc.NODE);

--------------------------------------------------------------------------------
TYPE
  TStatementChild = (schLeft, schNext, schRight);

  TStatement = RECORD
    parent: pc.NODE;
    node:   pc.NODE;         -- current statement
    child:  TStatementChild; -- "node" is left, right or next son of "parent"
    icode:  pc.NODE;         -- InstrCode to insert into "node.next"
  END;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Assign_next ( parent: pc.NODE
                                             ; full_mode:= FALSE: BOOLEAN );
VAR next: pc.NODE;
BEGIN
  next := parent.next;
  IF full_mode AND (stmt.icode # NIL) THEN
    parent := stmt.icode;
    WHILE parent.next # NIL DO
      parent := parent.next;
    END;
    InsertInstrCode_next (stmt.node, stmt.icode);
  END;
  stmt.parent := parent;
  stmt.node   := next;
  stmt.child  := schNext;
  stmt.icode  := NIL;
END Assign_next;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Assign_left (parent: pc.NODE);
BEGIN
  stmt.parent := parent;
  stmt.node   := parent.l;
  stmt.child  := schLeft;
  stmt.icode  := NIL;
END Assign_left;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Assign_right (parent: pc.NODE);
BEGIN
  stmt.parent := parent;
  stmt.node   := parent.r;
  stmt.child  := schRight;
  stmt.icode  := NIL;
END Assign_right;


--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) InsertInstrCode_left (icode: pc.NODE);
BEGIN
  IF icode = NIL THEN  RETURN END;

  InsertInstrCode_left (stmt.node, icode);
END InsertInstrCode_left;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) InsertInstrCode_right (icode: pc.NODE);
BEGIN
  IF icode = NIL THEN  RETURN END;

  InsertInstrCode_right (stmt.node, icode);
END InsertInstrCode_right;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) InsertInstrCode_top (icode: pc.NODE);
BEGIN
  IF icode = NIL THEN  RETURN END;

  ASSERT( stmt.parent # NIL );
  CASE stmt.child OF
  | schLeft:
       InsertInstrCode_left (stmt.parent, icode);
  | schRight:
       InsertInstrCode_right (stmt.parent, icode);
  | schNext:
       InsertInstrCode_next (stmt.parent, icode);
  END;
END InsertInstrCode_top;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) InsertInstrCode_next (icode: pc.NODE);
BEGIN
  IF icode = NIL THEN  RETURN END;

  AppendNode (stmt.icode, icode);
END InsertInstrCode_next;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) ReplaceByInstrCode_left (icode: pc.NODE);
BEGIN
  IF icode = NIL THEN  RETURN END;

  SetInstrCodePos(icode, stmt.node.l.pos);
  stmt.node.l := icode;
END ReplaceByInstrCode_left;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) ReplaceByInstrCode_right (icode: pc.NODE);
BEGIN
  IF icode = NIL THEN  RETURN END;

  SetInstrCodePos(icode, stmt.node.r.pos);
  stmt.node.r := icode;
END ReplaceByInstrCode_right;



--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) ProcessCondition_left ( type_true, type_false: TestConditionType
                                                       ; pos, end_pos: pc.TPOS
                                                       ; false_pos, false_end_pos: pc.TPOS
                                                       );
VAR icode: pc.NODE;
BEGIN
  IF SkipInstrumentations(stmt.node.pos) THEN RETURN END;

  ProcessExpression_left (stmt.node, TRUE);

  icode := tci.Create_ConditonCounter ( type_true, type_false
                                      , stmt.node.l
                                      , pos, end_pos, false_pos, false_end_pos );
  stmt.ReplaceByInstrCode_left (icode);
END ProcessCondition_left;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) ProcessCondition_right ( type_true, type_false: TestConditionType
                                                        ; pos, end_pos: pc.TPOS
                                                        ; false_pos, false_end_pos: pc.TPOS
                                                        );
VAR icode: pc.NODE;
BEGIN
  IF SkipInstrumentations(stmt.node.pos) THEN RETURN END;

  ProcessExpression_right (stmt.node, TRUE);

  icode := tci.Create_ConditonCounter ( type_true, type_false
                                      , stmt.node.r
                                      , pos, end_pos, false_pos, false_end_pos );
  stmt.ReplaceByInstrCode_right (icode);
END ProcessCondition_right;


--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) ProcessEnd (type: TestConditionType);

    -- 1 -- ProcessEnd ---------------------------------------------------------
    PROCEDURE getEndPos (n: pc.NODE): pc.TPOS;
    BEGIN
      WHILE (n.next # NIL) AND (n.owner = n.next.owner) DO
        CASE n.next.mode OF
        | pc.nd_if
        , pc.nd_while
        , pc.nd_repeat
        , pc.nd_loop
        , pc.nd_for
        , pc.nd_case
        , pc.nd_except
        , pc.nd_with
        , pc.nd_block
        , pc.nd_protect:
            RETURN n.next.pos;
        | pc.nd_wtrap
        , pc.nd_ftrap
        , pc.nd_activate:
            RETURN n.end;
        | pc.nd_return:
            RETURN n.next.end;
        ELSE
        END;
        n := n.next;
      END;
      RETURN n.end;
    END getEndPos;

-- 0 -- ProcessEnd -------------------------------------------------------------
CONST ProcessedStatement = pc.NODE_SET{ pc.nd_if
                                      , pc.nd_while
                                      , pc.nd_repeat
                                      , pc.nd_loop
                                      , pc.nd_for
                                      , pc.nd_case
                                      , pc.nd_except
                                      , pc.nd_with
                                      , pc.nd_block
                                      , pc.nd_protect
                                      , pc.nd_wtrap
                                      , pc.nd_ftrap };
VAR icode: pc.NODE;
BEGIN
  IF SkipInstrumentations(stmt.node.pos) THEN RETURN END;

  IF (stmt.node.next = NIL)
  OR (stmt.node.next.mode IN ProcessedStatement)
  OR (stmt.node.owner # stmt.node.next.owner)
  THEN
    CASE type OF
    | tc_C1_IF_end:          type := tc_Info_IF_end;
    | tc_C1_Case_end:        type := tc_Info_Case_end;
    | tc_C1_OberonWith_end:  type := tc_Info_OberonWith_end;
    | tc_C1_For_end:         type := tc_Info_For_end;
    | tc_C1_Loop_end:        type := tc_Info_Loop_end;
    | tc_C1_While_end:       type := tc_Info_While_end;
    | tc_C1_Repeat_end:      type := tc_Info_Repeat_end;
    ELSE
    END;
  END;

  icode := tci.Create_Counter (type, stmt.node.end, getEndPos(stmt.node));
  stmt.InsertInstrCode_next (icode);
END ProcessEnd;

--------------------------------------------------------------------------------
-- возвращает код инструментации для тела цикла
PROCEDURE (VAR stmt: TStatement) ProcessLoop ( type: TestConditionType
                                             ; pos, end_pos: pc.TPOS
                                             ): pc.NODE;
VAR local_counter: pc.OBJECT;
    icode_init, icode: pc.NODE;
BEGIN
  IF SkipInstrumentations(stmt.node.pos) THEN RETURN NIL END;

  tci.Create_IterationCounter ( type, local_counter
                              , pos, end_pos
                              , icode_init, icode );
  stmt.InsertInstrCode_top (icode_init);
  RETURN icode;
END ProcessLoop;


--------------------------------------------------------------------------------
-- Присваивание.
--        mode    nd_assign
--        obj     -> если левая часть - именованная переменная, то ссылка на нее
--        l       -> ссылка на lvalue, если obj = NIL
--        r       -> rvalue
--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Process_Assign ();
BEGIN
  IF stmt.node = NIL THEN RETURN END;
  ASSERT( stmt.node.mode = pc.nd_assign );

--  IF stmt.node.r.type # pc.ty_boolean THEN RETURN END;
  CASE stmt.node.r.type.mode OF
  | pc.ty_boolean:
      ProcessExpression_right (stmt.node, TRUE);
  ELSE
  END;

END Process_Assign;


--------------------------------------------------------------------------------
-- Вызов процедуры.
--        mode    nd_call
--        obj     -> если вызов не виртуальный, то вызываемая процедура
--        l       -> виртуальный вызов или NIL
--        r       -> список аргументов
--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Process_Call ();
VAR
  n : pc.NODE;
BEGIN
  IF stmt.node = NIL THEN RETURN END;
  ASSERT( stmt.node.mode = pc.nd_call );

  IF (stmt.node.r # NIL) THEN
    ProcessExpression_right (stmt.node, TRUE);
    n := stmt.node.r;
    WHILE (n.next # NIL) DO
      IF (n.next.type.mode = pc.ty_boolean) THEN
        ProcessExpression_next (n, TRUE);
      END;
      n := n.next;
    END;
  END;
END Process_Call;


--------------------------------------------------------------------------------
-- nd_sproc
--        mode    nd_sproc
--        sub     sp_*
--        l       -> виртуальный вызов или NIL
--        r       -> список аргументов
--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Process_Sproc ();
BEGIN
  IF stmt.node = NIL THEN RETURN END;
  ASSERT( stmt.node.mode = pc.nd_sproc );

  IF (stmt.node.sub = pc.sp_assert) THEN
    ASSERT((stmt.node.r # NIL) AND (stmt.node.r.type.mode = pc.ty_boolean));
    ProcessExpression_right (stmt.node, TRUE);
  END;
END Process_Sproc;


--------------------------------------------------------------------------------
-- Оператор Oberon2 WITH
--    Представляется как
--        IF ... IS ... THEN
--        ELSIF ... IS ... THEN
--                ...
--        ELSE
--        END
--    Если ELSE-части нет, то она все равно создается - там паявляется wtrap
--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) MakeInstrCode_C1_OberonWITH (): pc.NODE;
BEGIN
  IF SkipInstrumentations(stmt.node.pos) THEN RETURN NIL END;

  RETURN tci.Create_Counter ( tc_C1_OberonWith
                                  , stmt.node.pos, stmt.node.r.end);
END MakeInstrCode_C1_OberonWITH;


--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) MakeInstrCode_C1_OberonWITH_ELSE ( end_pos-: pc.TPOS
                                                                  ): pc.NODE;
BEGIN
  IF SkipInstrumentations(stmt.node.r.end) THEN RETURN NIL END;

  RETURN tci.Create_Counter ( tc_C1_OberonWith_else
                            , stmt.node.r.end, end_pos );
END MakeInstrCode_C1_OberonWITH_ELSE;


--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Process_OberonWITH ();

    -- 1 -- Process_OberonWITH -------------------------------------------------
    PROCEDURE Is_Another_WITH_Branch (stmt: pc.NODE): BOOLEAN;
    BEGIN
      RETURN (stmt # NIL)           AND
             (stmt.mode = pc.nd_if) AND
             (pc.ntag_elsif_node IN stmt.tags);
    END Is_Another_WITH_Branch;

    -- 1 -- Process_OberonWITH -------------------------------------------------
    PROCEDURE Is_ELSE_Part (stmt: pc.NODE): BOOLEAN;
    BEGIN
      RETURN (stmt = NIL) OR (stmt.mode # pc.nd_wtrap);
    END Is_ELSE_Part;

-- 0 -- Process_OberonWITH -----------------------------------------------------
VAR branch: TStatement;
    icode: pc.NODE;
    end_pos: pc.TPOS;
BEGIN
  IF stmt.node = NIL THEN RETURN END;
  ASSERT( stmt.node.mode = pc.nd_if );
  end_pos := stmt.node.end;

  branch := stmt;
  LOOP
    IF (branch.node.r = NIL) THEN  EXIT  END;

    -- process WITH-branch body
    icode := branch.MakeInstrCode_C1_OberonWITH ();
    ProcessStatement_left (branch.node.r);
    InsertInstrCode_left (branch.node.r, icode);

    IF Is_Another_WITH_Branch (branch.node.r.r) THEN
      -- get next branch of WITH statement
      branch.Assign_right (branch.node.r);
    ELSIF Is_ELSE_Part (branch.node.r.r) THEN
      -- process ELSE-part of WITH statement
      icode := branch.MakeInstrCode_C1_OberonWITH_ELSE (end_pos);
      ProcessStatement_right (branch.node.r);
      InsertInstrCode_right (branch.node.r, icode);
      EXIT;
    ELSE
      EXIT;
    END;
  END;

  -- process END statement
  stmt.ProcessEnd (tc_C1_OberonWith_end);
END Process_OberonWITH;


--------------------------------------------------------------------------------
-- Оператор IF
--      mode    nd_if
--      l       -> условное выражение
--      r       -> NODE
--                      mode    nd_node
--                      l       -> TRUE-часть
--                      r       -> ELSE-часть
-- В случае ELSIF ELSE-часть явялется такой же конструкцией.
--------------------------------------------------------------------------------
PROCEDURE Is_IF_ELSE_Part (stmt: pc.NODE): BOOLEAN;
BEGIN
  RETURN (stmt.mode # pc.nd_if) OR
         NOT (pc.ntag_elsif_node IN stmt.tags);
END Is_IF_ELSE_Part;

PROCEDURE (VAR stmt: TStatement) ProcessCondition_IF_ELSIF ( type_true: TestConditionType
                                                           ; type_false: TestConditionType
                                                           );
VAR end_pos: pc.TPOS;
    false_end_pos: pc.TPOS;
BEGIN
  IF SkipInstrumentations(stmt.node.pos) THEN RETURN END;

  IF stmt.node.r.r = NIL THEN
    -- 'if' statement without 'elsif' and 'else' parts
    end_pos := stmt.node.end;
--  ELSIF stmt.node.r.r.mode = pc.nd_if THEN
  ELSIF Is_IF_ELSE_Part(stmt.node.r.r) THEN
    -- 'if ... else ' statement
    end_pos := stmt.node.r.pos;
  ELSE
    -- 'if' statement with 'elsif' part
    end_pos := stmt.node.r.r.pos;
  END;
  IF stmt.node.r.l = NIL THEN
    false_end_pos := end_pos;
  ELSE
    false_end_pos := stmt.node.r.l.pos;
  END;
  stmt.ProcessCondition_left ( type_true, type_false
                             , stmt.node.pos, end_pos
                             , stmt.node.pos, false_end_pos );
END ProcessCondition_IF_ELSIF;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) MakeInstrCode_IF_ELSE (): pc.NODE;
BEGIN
  IF SkipInstrumentations(stmt.node.r.pos) THEN RETURN NIL END;

  RETURN tci.Create_Counter (tc_Info_IF_else, stmt.node.r.pos, stmt.node.r.end);
END MakeInstrCode_IF_ELSE;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Process_IF ();

    -- 1 -- Process_IF ---------------------------------------------------------
    PROCEDURE Is_OberonWITH (stmt: pc.NODE): BOOLEAN;
    BEGIN
      RETURN (stmt.mode = pc.nd_if) AND
             (pc.ntag_substitute(*o2_with*) IN stmt.tags);
    END Is_OberonWITH;

-- 0 -- Process_IF -------------------------------------------------------------
VAR branch: TStatement;
    icode: pc.NODE;
BEGIN
  IF stmt.node = NIL THEN RETURN END;
  ASSERT( stmt.node.mode = pc.nd_if );
  IF Is_OberonWITH (stmt.node) THEN
    stmt.Process_OberonWITH ();
    RETURN;
  END;

  stmt.ProcessCondition_IF_ELSIF (tc_C1_IF_true, tc_C1_IF_false);

  -- process TRUE-part
  IF stmt.node.r.l # NIL THEN
    ProcessStatement_left (stmt.node.r);
  END;

  -- process ELSE-part
  branch := stmt;
  LOOP
    IF (branch.node.r.r = NIL) THEN
      EXIT;
    ELSIF Is_IF_ELSE_Part (branch.node.r.r) THEN
      -- process ELSE-part
      icode := branch.MakeInstrCode_IF_ELSE ();
      ProcessStatement_right (branch.node.r);
      InsertInstrCode_right (branch.node.r, icode);
      EXIT;
    ELSE
      -- process ELSIF-part
      branch.Assign_right (branch.node.r);
      ASSERT( branch.node.mode = pc.nd_if );
      branch.ProcessCondition_IF_ELSIF (tc_C1_IF_elsif_true, tc_C1_IF_elsif_false);
      ProcessStatement_left (branch.node.r);
    END;
  END;

  -- process END statement
  stmt.ProcessEnd (tc_C1_IF_end);
END Process_IF;


--------------------------------------------------------------------------------
-- WHILE
--      mode    nd_while
--      l       -> условное выражение
--      r       -> тело
--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Process_WHILE ();
VAR icode: pc.NODE;
    do_end_pos: pc.TPOS;
BEGIN
  IF stmt.node = NIL THEN RETURN END;
  ASSERT( stmt.node.mode = pc.nd_while );

  IF stmt.node.r # NIL THEN
    do_end_pos := stmt.node.r.pos;
  ELSE
    do_end_pos := stmt.node.end;
  END;
  stmt.ProcessCondition_left ( tc_C1_While_true, tc_C1_While_false
                             , stmt.node.pos, stmt.node.end
                             , stmt.node.pos, do_end_pos );

  icode := stmt.ProcessLoop ( tc_C_While_itr_0
                            , stmt.node.pos, do_end_pos );

  ProcessStatement_right (stmt.node);

  stmt.InsertInstrCode_right (icode);

  -- process END statement
  stmt.ProcessEnd (tc_C1_While_end);
END Process_WHILE;


--------------------------------------------------------------------------------
-- REPEAT
--      mode    nd_repeat
--      r       -> условное выражение
--      l       -> тело
--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Process_REPEAT ();
VAR icode: pc.NODE;
    until_pos: pc.TPOS;
    n: pc.NODE;
BEGIN
  IF stmt.node = NIL THEN RETURN END;
  ASSERT( stmt.node.mode = pc.nd_repeat );

  IF stmt.node.l # NIL THEN
    n := stmt.node.l;
    WHILE n.next # NIL DO
      n := n.next;
    END;
    until_pos := n.end;
  ELSE
    until_pos := stmt.node.pos;
  END;
  stmt.ProcessCondition_right ( tc_C1_Repeat_true, tc_C1_Repeat_false
                              , stmt.node.pos, stmt.node.end
                              , until_pos, stmt.node.end );

  icode := stmt.ProcessLoop ( tc_Info_Repeat_itr_0
                            , until_pos, stmt.node.end );

  ProcessStatement_left (stmt.node);

  stmt.InsertInstrCode_left (icode);

  -- process END statement
  stmt.ProcessEnd (tc_C1_Repeat_end);
END Process_REPEAT;


--------------------------------------------------------------------------------
-- LOOP
--      mode    nd_loop
--      r       -> тело
--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) MakeInstrCode_C1_LOOP (): pc.NODE;
BEGIN
  IF SkipInstrumentations(stmt.node.pos) THEN RETURN NIL END;

  RETURN tci.Create_Counter (tc_C1_Loop, stmt.node.pos, stmt.node.end);
END MakeInstrCode_C1_LOOP;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Process_LOOP ();

    -- 1 -- Process_LOOP -------------------------------------------------------
    PROCEDURE hasExit (n: pc.NODE): BOOLEAN;
    BEGIN
      IF (n = NIL) THEN
        RETURN FALSE;
      END;

      CASE n.mode OF
      | pc.nd_return:
          RETURN FALSE;
      | pc.nd_exit:
          RETURN TRUE;
      | pc.nd_call:
          RETURN hasExit(n.next);
      ELSE
          RETURN hasExit(n.l)
              OR hasExit(n.r)
              OR hasExit(n.next);
      END;
    END hasExit;

-- 0 -- Process_LOOP -----------------------------------------------------------
VAR icode: pc.NODE;
    icode2: pc.NODE;
    body_pos: pc.TPOS;
BEGIN
  IF stmt.node = NIL THEN RETURN END;
  ASSERT( stmt.node.mode = pc.nd_loop );

  IF stmt.node.r # NIL THEN
    body_pos := stmt.node.r.pos;
  ELSE
    body_pos := stmt.node.end;
  END;

  icode  := stmt.MakeInstrCode_C1_LOOP ();
  icode2 := stmt.ProcessLoop ( tc_Info_Loop_itr_0
                             , stmt.node.pos, body_pos );
  ProcessStatement_right (stmt.node);

  stmt.InsertInstrCode_right (icode2);
  stmt.InsertInstrCode_right (icode);

  -- process END statement
  IF hasExit(stmt.node.r) THEN
    stmt.ProcessEnd (tc_C1_Loop_end);
  END;
END Process_LOOP;


--------------------------------------------------------------------------------
-- FOR
--      mode    nd_for
--      obj     -> переменная цикла
--      l       -> NODE
--                      mode    nd_node
--                      l       -> 'from' expression
--                      r       -> 'to'   expression
--                      val     -> 'by'   value
--      r       -> тело
--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) MakeInstrCode_C1_FOR (): pc.NODE;
BEGIN
  IF SkipInstrumentations(stmt.node.pos) THEN RETURN NIL END;

  RETURN tci.Create_Counter (tc_C1_For, stmt.node.pos, stmt.node.end);
END MakeInstrCode_C1_FOR;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Process_FOR ();

  -- 1 -- Process_FOR ----------------------------------------------------------
  PROCEDURE isValue(n: pc.NODE): BOOLEAN;
  BEGIN
    CASE n.mode OF
    | pc.nd_value:
        RETURN TRUE;
    | pc.nd_var:
        RETURN (n.obj # NIL) AND (n.obj.mode = pc.ob_cons);
    | pc.nd_unary
    , pc.nd_lconv:
        RETURN isValue(n.l);
    ELSE
        pc.const.eval_value(n);
        RETURN n.mode = pc.nd_value;
    END;
  END isValue;

  -- 1 -- Process_FOR ----------------------------------------------------------
  PROCEDURE isConstantFor(n: pc.NODE): BOOLEAN;
  BEGIN
    RETURN isValue(n.l.l) AND isValue(n.l.r);
  END isConstantFor;

  -- 1 -- Process_FOR ----------------------------------------------------------
  PROCEDURE getConstantForType(n: pc.NODE): TestConditionType;
  VAR val_from, val_to: pc.VALUE;
      val_type: pc.STRUCT;
      i_from, i_to, i_tmp: LONGINT;
      c_from, c_to, c_tmp: sys.CARD32;
      type: TestConditionType;
  BEGIN
    val_from := pc.const.eval_const(n.l.l);
    val_to   := pc.const.eval_const(n.l.r);
    val_type := n.obj.type;
    WHILE val_type.mode = pc.ty_range DO val_type := val_type.base END;
    CASE val_type.mode OF
    | pc.ty_shortcard
    , pc.ty_cardinal
    , pc.ty_longcard
  <* IF NOT TARGET_VAX THEN *>
    , pc.ty_longlongcard
  <* END *>
    , pc.ty_uchar
    , pc.ty_char
    , pc.ty_enum:
        c_from := val_from.get_cardinal();
        c_to   := val_to.get_cardinal();
        IF c_to < c_from THEN
          c_tmp  := c_to;
          c_to   := c_from;
          c_from := c_tmp;
        END;
        IF (c_to - c_from) > 0 THEN
          type := tc_Info_For_itr_1;
        ELSE
          type := tc_Info_For_itr_0;
        END;
    | pc.ty_shortint
    , pc.ty_integer
    , pc.ty_longint
    , pc.ty_longlongint:
        i_from := val_from.get_integer();
        i_to   := val_to.get_integer();
        IF i_to < i_from THEN
          i_tmp  := i_to;
          i_to   := i_from;
          i_from := i_tmp;
        END;
        IF ABS(i_from - i_to) > 1 THEN
          type := tc_Info_For_itr_1;
        ELSE
          type := tc_Info_For_itr_0;
        END;
    END;
    RETURN type;
  END getConstantForType;

-- 0 -- Process_FOR ------------------------------------------------------------
VAR icode: pc.NODE;
    icode2: pc.NODE;
    type: TestConditionType;
    do_end_pos: pc.TPOS;
BEGIN
  IF stmt.node = NIL THEN RETURN END;
  ASSERT( stmt.node.mode = pc.nd_for );

  IF isConstantFor(stmt.node) THEN
    type := getConstantForType(stmt.node);
  ELSE
    type := tc_C_For_itr_0;
  END;                  

  IF stmt.node.r # NIL THEN
    do_end_pos := stmt.node.r.pos;
  ELSE
    do_end_pos := stmt.node.end;
  END;

  icode  := stmt.MakeInstrCode_C1_FOR ();
  icode2 := stmt.ProcessLoop ( type
                             , stmt.node.pos, do_end_pos );

  ProcessStatement_right (stmt.node);

  stmt.InsertInstrCode_right (icode2);
  stmt.InsertInstrCode_right (icode);

  -- process END statement
  stmt.ProcessEnd (tc_C1_For_end);

END Process_FOR;


--------------------------------------------------------------------------------
-- CASE
--      mode    nd_case
--      l       -> выражение-селектор
--      r       -> NODE
--                      mode    nd_casedo (nd_caselse, если есть ELSE-часть)
--                      r       -> ELSE-часть (если есть)
--                      next    -> NODE
--                              До 3.0 было так
--                                      mode    nd_pair
--                                      val     -> minimum
--                                      l       -> NODE
--                                                      mode    nd_value
--                                                      val     -> maximum
--                              В 3.0 стало так
--                                      mode    nd_pair
--                                      l       -> NODE
--                                                      mode    nd_value
--                                                      val     -> minimum
--                                      r       -> NODE
--                                                      mode    nd_value
--                                                      val     -> maximum
--                      l       -> список вариантов
--                              Узел списка.
--                                      mode    nd_node
--                                      next    -> следующий
--                                      r       -> тело
--                                      l       -> список диапазонов
--                                              Узел списка.
--                                                      mode    nd_pair
--                                                      type    -> тип селектора
--                                                      До 3.0 было так
--                                                      val     -> minimum
--                                                      l       -> NODE
--                                                                      mode    nd_value
--                                                                      val     -> maximum
--                                                                      type    -> тип селектора
--                                                      В 3.0 стало так
--                                                      l       -> minimum (выражение)
--                                                      r       -> maximum (выражение)
--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) MakeInstrCode_C1_CASE (): pc.NODE;
BEGIN
  IF SkipInstrumentations(stmt.node.pos) THEN RETURN NIL END;

  RETURN tci.Create_Counter (tc_C1_Case, stmt.node.pos, stmt.node.end);
END MakeInstrCode_C1_CASE;

--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) MakeInstrCode_C1_CASE_ELSE (): pc.NODE;
VAR pos: pc.TPOS;
    case_branch: pc.NODE;
BEGIN
  IF stmt.node.l # NIL THEN
    case_branch := stmt.node.l;
    WHILE case_branch.next # NIL DO
      case_branch := case_branch.next;
    END;
    pos := case_branch.end;
  ELSE
    pos := stmt.node.pos;
  END;

  IF SkipInstrumentations(pos) THEN RETURN NIL END;

  RETURN tci.Create_Counter (tc_C1_Case_else, pos, stmt.node.end);
END MakeInstrCode_C1_CASE_ELSE;


--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Process_CASE ();
VAR icode: pc.NODE;

    -- 1 -- Process_CASE -------------------------------------------------------
    PROCEDURE Process_CASE_BRANCH (branch-: TStatement);
    BEGIN
      icode := branch.MakeInstrCode_C1_CASE ();
      ProcessStatement_right (branch.node);
      branch.InsertInstrCode_right (icode);
    END Process_CASE_BRANCH;

-- 0 -- Process_CASE -----------------------------------------------------------
VAR
  branch: TStatement;
  case: pc.NODE;
BEGIN
  IF stmt.node = NIL THEN RETURN END;
  ASSERT( stmt.node.mode = pc.nd_case );

  -- process CASE-branches
  branch.Assign_left (stmt.node.r);
  WHILE branch.node # NIL DO
    Process_CASE_BRANCH (branch);
    branch.Assign_next (branch.node);
  END;

  -- process ELSE-part
  branch.Assign_right (stmt.node);
  IF (stmt.node.r.mode = pc.nd_caselse) THEN
    icode := branch.MakeInstrCode_C1_CASE_ELSE ();
    ProcessStatement_right (branch.node);
    branch.InsertInstrCode_right (icode);
  ELSE
    case := stmt.node.r;
    ASSERT(case.mode = pc.nd_casedo);
    icode := tci.Create_Counter (tc_C_Case_else, stmt.node.pos, stmt.node.l.pos);
    ASSERT(icode.next = NIL);
    icode.next := tci.Create_CaseTrap (stmt.node.pos, stmt.node.l.pos);
    ASSERT(case.r = NIL);
    InsertInstrCode_right(case, icode);
    case.mode := pc.nd_caselse;
  END;

  -- process END statement
  stmt.ProcessEnd (tc_C1_Case_end);
END Process_CASE;


--------------------------------------------------------------------------------
-- EXCEPT блок.
--      mode    nd_except
--      type    -> void
--      next    -> следующий
--      obj     -> OBJECT
--                      mode    ob_var
--                      name    "trap"
--                      type    -> pcO.process
--      r       -> список операторов EXCEPT части
--      l       -> операторы BEGIN или FINALLY части (см. Блок операторов)
--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) Process_EXCEPT ();
VAR
  icode: pc.NODE;
BEGIN
  IF stmt.node = NIL THEN RETURN END;
  ASSERT( stmt.node.mode = pc.nd_except );

  icode := tci.Create_Counter (tc_C1_Except, stmt.node.pos, stmt.node.end);

  ProcessStatement_left  (stmt.node);
  ProcessStatement_right (stmt.node);

  stmt.InsertInstrCode_right(icode);
END Process_EXCEPT;


--------------------------------------------------------------------------------

VAR
  InitRecursionDepth, IncRecursionDepth: pc.NODE;
  RecursionDepth: pc.OBJECT;
  RecursionReturnFlag: pc.OBJECT;

PROCEDURE (VAR stmt: TStatement) Process_RETURN ();
VAR
  icode: pc.NODE;
BEGIN
  IF RecursionDepth = NIL THEN RETURN END;
  icode := tci.Create_DecreaseRecursionDepth( stmt.node.pos, stmt.node.end
                                            , RecursionDepth, RecursionReturnFlag );
  stmt.InsertInstrCode_top(icode);
END Process_RETURN;

--------------------------------------------------------------------------------
--                       Statement Traversal
--------------------------------------------------------------------------------

PROCEDURE (VAR stmt: TStatement) Process ();
BEGIN
  IF stmt.node = NIL THEN RETURN END;

  CASE stmt.node.mode OF
  | pc.nd_assign:  stmt.Process_Assign ();
  | pc.nd_call:    stmt.Process_Call   ();
  | pc.nd_sproc:   stmt.Process_Sproc  ();
  | pc.nd_if:      stmt.Process_IF     ();
  | pc.nd_while:   stmt.Process_WHILE  ();
  | pc.nd_repeat:  stmt.Process_REPEAT ();
  | pc.nd_loop:    stmt.Process_LOOP   ();
  | pc.nd_for:     stmt.Process_FOR    ();
  | pc.nd_case:    stmt.Process_CASE   ();
  | pc.nd_except:  stmt.Process_EXCEPT ();
  | pc.nd_with,
    pc.nd_block:   ProcessStatement_right (stmt.node);
  | pc.nd_protect: ProcessStatement_left  (stmt.node);
  | pc.nd_return:  stmt.Process_RETURN ();
  ELSE
  END;
END Process;


--------------------------------------------------------------------------------
PROCEDURE (VAR stmt: TStatement) ProcessList ();

    -- 1 -- ProcessList --------------------------------------------------------
    PROCEDURE MakeInstrCode_LocalModuleEntry ( module: pc.NODE
                                             ; finally: BOOLEAN ): pc.NODE;
    BEGIN
      IF module # NIL THEN
        IF finally THEN
          ASSERT( module.r.mode = pc.nd_finally );
          RETURN tci.Create_Counter (tc_C1_Finally, module.r.pos, module.r.end);
        ELSE
          RETURN tci.Create_Counter (tc_C1_Module, module.pos, module.end);
        END;
      END;
      RETURN NIL;                               
    END MakeInstrCode_LocalModuleEntry;

    -- 1 -- ProcessList --------------------------------------------------------
    PROCEDURE getOwner (node: pc.NODE): pc.NODE;
    BEGIN
      IF node.owner # NIL THEN
        RETURN node.owner(pc.NODE);
      END;
      RETURN NIL;
    END getOwner;

-- 0 -- ProcessList ------------------------------------------------------------
VAR owner: pc.NODE;
    icode: pc.NODE;
    entred: BOOLEAN;
    finally: BOOLEAN;
    host_proc: BOOLEAN;
BEGIN
  host_proc := stmt.parent.mode = pc.nd_proc;
  finally := host_proc & (stmt.parent.obj.name^ = "FINALLY");
  owner   := getOwner(stmt.parent);
  entred  := FALSE;

  IF (stmt.parent.mode # pc.nd_proc) AND (stmt.parent.mode # pc.nd_module) THEN
    WHILE stmt.node # NIL DO
      stmt.Process ();
      stmt.Assign_next (stmt.node, TRUE (* process 'stmt.icode' *));
    END;

  ELSE
    -- it is module BEGIN or FINALLY
    WHILE stmt.node # NIL DO
      IF stmt.node.owner # owner THEN
        IF entred THEN
          tci.ExitProcedure();
          entred := FALSE;
        END;
        owner := getOwner(stmt.node);
        IF owner # NIL THEN
          tci.EnterProcedure(owner.obj);
          icode := MakeInstrCode_LocalModuleEntry (owner, finally);
          stmt.InsertInstrCode_top(icode);
          entred := TRUE;
        ELSE
          finally := host_proc;
        END;
      END;
      stmt.Process ();
      stmt.Assign_next (stmt.node, TRUE (* process 'stmt.icode' *));
    END;
    IF (owner # NIL) AND entred THEN
      tci.ExitProcedure();
    END;
  END;

END ProcessList;


--------------------------------------------------------------------------------
PROCEDURE ProcessStatement_right (node: pc.NODE);
VAR stmt: TStatement;
BEGIN
  IF node = NIL THEN RETURN END;

  stmt.Assign_right (node);
  stmt.ProcessList ();
END ProcessStatement_right;


--------------------------------------------------------------------------------
PROCEDURE  ProcessStatement_left  (node: pc.NODE);
VAR stmt: TStatement;
BEGIN
  IF node = NIL THEN RETURN END;

  stmt.Assign_left (node);
  stmt.ProcessList ();
END ProcessStatement_left;


--------------------------------------------------------------------------------
--                       Procedure Traversal
--------------------------------------------------------------------------------

PROCEDURE ^ ProcessProc_list (procs: pc.NODE);


--------------------------------------------------------------------------------
-- 0) Блок операторов (представляет BEGIN или FINALLY части).
--    a) если в списке операторов есть RETURN, ASSERT или HALT
--         mode    nd_block
--         type    -> void
--         next    -> следующий
--         r       -> список операторов
--    b) если их нет, то это просто список операторов (тот, что висит на ссылке r)
--
-- 1) EXCEPT блок.
--      mode    nd_except
--         type    -> void
--         next    -> следующий
--         obj     -> OBJECT
--                         mode    ob_var
--                         name    "trap"
--                         type    -> pcO.process
--      r       -> список операторов EXCEPT части
--      l       -> операторы BEGIN или FINALLY части (см. Блок операторов)
--
-- 2) PROTECT блок.
--         mode    nd_protect
--         type    -> void
--         next    -> следующий
--         l       -> на ЕХCEPT блок или блок операторов
--         r       -> NODE
--                         mode    nd_value
--                         type    -> pcO.protection
--                         val     -> protection number
--         obj     -> OBJECT
--                         mode    ob_var
--                         type    -> pcO.protection
--------------------------------------------------------------------------------
PROCEDURE InsertInstrCode_BeginProc (proc: pc.NODE; icode: pc.NODE);
CONST ProcBlocks = pc.NODE_SET {pc.nd_block, pc.nd_except, pc.nd_protect};
BEGIN
  IF (proc.r # NIL) AND (proc.r.mode IN ProcBlocks) THEN
    proc := proc.r;
  END;

  CASE proc.mode OF
  | pc.nd_except
  , pc.nd_protect:
      IF (proc.l # NIL) AND (proc.l.mode = pc.nd_block) THEN
        InsertInstrCode_right (proc.l, icode);
      ELSE
        InsertInstrCode_left (proc, icode);
      END;
  ELSE
    InsertInstrCode_right (proc, icode);
  END;
END InsertInstrCode_BeginProc;


--------------------------------------------------------------------------------
-- Процедура.                  <-------------------+---------------------------|
--      mode    ob_?proc                           |                           |
--      type    -> на STRUCT                       |          <--------------| |
--                      mode    ty_proctype        |                         | |
--                      obj     -------------------|                         | |
--                      len     если метод, то его номер                     | |
--                      base    -> тип возвращаемого значения                | |
--                      prof    -> список параметров (см. Процедурный тип),  | |
--                                 для метода первый параметр - this         | |
--                      mem     -> локалы                                    | |
--                      inx     -> для метода ссылается на такой же STRUCT   | |
--                                 от перекрытого метода                     | |
--                                                                           | |
--      val     -> на NODE                                                   | |   
--                      mode    nd_proc                                      | |   
--                      type    ---------------------------------------------| |
--                      obj     -----------------------------------------------|
--                      l       -> список локальных процедур 
--                                 (точно таких же NODE)
--                      r       -> тело - разного вида блоки (Блок операторов,
--                                 EXCEPT блок, PROTECT блок) провязаные по next,
--                                 причем в конце списка висит NODE
--                                      mode    nd_reraise
--                                      r       -> указвает на EXCEPT блок, если
--                                                 же его нет, то = NIL
--
--------------------------------------------------------------------------------
PROCEDURE MakeInstrCode_ProcedureEntry ( proc: pc.NODE
                                       ; UseHeavyRTSCall: BOOLEAN): pc.NODE;
VAR type: TestConditionType;
    node: pc.NODE;
BEGIN
  IF SkipInstrumentations(proc.obj.pos) THEN RETURN NIL END;

  IF (proc.obj.name^ = "FINALLY") THEN   type := tc_C1_Finally;
  ELSE                                   type := tc_C1_Procedure
  END;
  IF UseHeavyRTSCall THEN
    node      := tci.Create_TCSModuleBeginCall (proc.pos, proc.pos);
    node.next := tci.Create_Counter (type, proc.obj.pos, proc.obj.end);
  ELSE
    node      := tci.Create_Counter (type, proc.obj.pos, proc.obj.end);
  END;
  RETURN node;
END MakeInstrCode_ProcedureEntry;

--------------------------------------------------------------------------------
PROCEDURE isRecursive (proc: pc.NODE): BOOLEAN;

  PROCEDURE check (n: pc.NODE): BOOLEAN;
  VAR call: pc.OBJECT;
      val: pc.NODE;
      res: BOOLEAN;
  BEGIN
    IF (n = NIL) THEN
      RETURN FALSE;
    END;

    CASE n.mode OF
    | pc.nd_proc:
        RETURN check(n.r);
    | pc.nd_return:
        RETURN check(n.l);
    | pc.nd_exit
    , pc.nd_value
    , pc.nd_activate
    , pc.nd_reraise
    , pc.nd_retry:
        RETURN FALSE;
    | pc.nd_call:
        IF (n.obj # NIL) THEN
          call := n.obj;
        ELSE
          ASSERT(n.l # NIL);
          call := n.l.obj;
          IF call = NIL THEN
            RETURN FALSE;
          END;
        END;
        IF (call  = proc.obj) THEN
          RETURN TRUE;
        ELSIF (call.val # NIL) THEN
          val := call.val;
          call.val := NIL;      
          res := check(val);
          call.val := val;
          RETURN res;
        ELSE
          RETURN check(n.next);
        END;
    ELSE
        RETURN check(n.l)
            OR check(n.r)
            OR check(n.next);
    END;
  END check;

BEGIN
  RETURN check(proc.r);
END isRecursive;


PROCEDURE MakeInstrCode_Recursion (proc: pc.NODE): BOOLEAN;
VAR
  n: pc.NODE;
BEGIN
  IF NOT isRecursive(proc) THEN
    RETURN FALSE;
  END;

  tci.Create_RecursionCounter( proc.pos, proc.end
                             , RecursionDepth, RecursionReturnFlag
                             , InitRecursionDepth, IncRecursionDepth);

  IF (proc.r # NIL) THEN
    n := proc.r;
    IF (n.mode = pc.nd_block) THEN
      n := n.r;
    END;
    WHILE (n.next # NIL) DO
      n := n.next;
    END;
    IF (n.mode # pc.nd_return) THEN
      n.next := tci.Create_DecreaseRecursionDepth( n.pos, n.end
                                                 , RecursionDepth, RecursionReturnFlag );
    END;
  END;

  RETURN TRUE;
END MakeInstrCode_Recursion;

--------------------------------------------------------------------------------

PROCEDURE ProcessProc (proc: pc.NODE);

    -- 1 -- ProcessProc --------------------------------------------------------
    PROCEDURE hasAsmCode (n: pc.NODE): BOOLEAN;
    BEGIN
      IF (n = NIL) THEN
        RETURN FALSE;
      END;

      CASE n.mode OF
      | pc.nd_sproc:
          CASE n.sub OF
          | pc.sp_code:   RETURN  TRUE;
          ELSE            RETURN  FALSE;
          END;
      | pc.nd_if
      , pc.nd_while
      , pc.nd_loop
      , pc.nd_for
      , pc.nd_case
      , pc.nd_with
      , pc.nd_block:
          RETURN hasAsmCode(n.r)
              OR hasAsmCode(n.next);
      | pc.nd_repeat
      , pc.nd_protect:
          RETURN hasAsmCode(n.l)
              OR hasAsmCode(n.next);
      | pc.nd_return
      , pc.nd_exit
      , pc.nd_pair
      , pc.nd_value
      , pc.nd_activate
      , pc.nd_reraise
      , pc.nd_retry:
          RETURN FALSE;
      | pc.nd_call:
          RETURN hasAsmCode(n.next);
      ELSE
          RETURN hasAsmCode(n.l)
              OR hasAsmCode(n.r)
              OR hasAsmCode(n.next);
      END;
    END hasAsmCode;

    -- 1 -- ProcessProc --------------------------------------------------------
    PROCEDURE Is_Nested_Proc (): BOOLEAN;
    BEGIN
      RETURN proc.obj.lev > 0;
    END Is_Nested_Proc;

-- 0 -- ProcessProc ------------------------------------------------------------
VAR useHeavyRTSCall: BOOLEAN;
    icode: pc.NODE;
    is_recursive: BOOLEAN;
BEGIN
  IF (proc = NIL) THEN
    RETURN;
  ELSIF hasAsmCode(proc.r) THEN
    env.errors.Warning (proc.obj.pos, 1026, proc.obj.name^);
    RETURN;
  END;
  
  pcO.enter_scope (proc.type);
  tci.EnterProcedure(proc.obj);

  -- process local procedures
  ProcessProc_list(proc.l);

  -- process procedure body
<* IF TARGET_MIPS OR TARGET_VAX THEN *>
  useHeavyRTSCall := NOT Is_Nested_Proc();
<* ELSE *>
  useHeavyRTSCall := NOT Is_Nested_Proc() AND
                     (pc.otag_public IN proc.obj.tags) AND
                     ( NOT UseModuleConstructor  OR
                       NOT (proc.type.flag IN fe.LangsWithModuleConstructors) OR
                       NOT ModuleBudyExist
                     );
<* END *>

  icode := MakeInstrCode_ProcedureEntry (proc, useHeavyRTSCall);

  is_recursive := MakeInstrCode_Recursion(proc);

  ProcessStatement_right (proc);

  IF is_recursive THEN
    InsertInstrCode_BeginProc (proc, IncRecursionDepth);
    -- TODO: insert in X2C_TCS_INIT
--    InsertInstrCode_BeginProc (proc.obj.host.obj.val, InitRecursionDepth);
  END;

  InsertInstrCode_BeginProc (proc, icode);

  InitRecursionDepth  := NIL;
  IncRecursionDepth   := NIL;
  RecursionDepth      := NIL;
  RecursionReturnFlag := NIL;

  tci.ExitProcedure();
  pcO.exit_scope();
END ProcessProc;


--------------------------------------------------------------------------------
PROCEDURE ProcessProc_list (procs: pc.NODE);
BEGIN
  WHILE procs # NIL DO
    ProcessProc(procs);
    procs := procs.next;
  END;
END ProcessProc_list;


--------------------------------------------------------------------------------
--                          Module Traversal
--------------------------------------------------------------------------------

PROCEDURE MakeInstrCode_ModuleEntry (module: pc.NODE): pc.NODE;
VAR node: pc.NODE;
BEGIN
  node      := tci.Create_TCSModuleBeginCall (module.pos, module.pos);
  node.next := tci.Create_Counter (tc_C1_Module, module.pos, module.end);
  RETURN node;
END MakeInstrCode_ModuleEntry;


--------------------------------------------------------------------------------
PROCEDURE ProcessModuleBody (module: pc.NODE);
VAR icode: pc.NODE;
BEGIN
   IF NOT UseModuleConstructor THEN  RETURN END;

  -- process module body
  tci.EnterProcedure(module.obj);
  icode := MakeInstrCode_ModuleEntry (module);

  ProcessStatement_right (module);

  InsertInstrCode_BeginProc (module, icode);
  tci.ExitProcedure();
END ProcessModuleBody;

--------------------------------------------------------------------------------
PROCEDURE InsertTCSModuleBeginCall (module: pc.NODE);
VAR icode: pc.NODE;
BEGIN
  icode := tci.Create_TCSModuleBeginCall (fe.NullPos, fe.NullPos);
  InsertInstrCode_BeginProc (module, icode);
END InsertTCSModuleBeginCall;

--------------------------------------------------------------------------------
PROCEDURE BeginTraversal ();
VAR time: tci.TTime;
    crcsum:   sys.CARD32;
    crcsum16: sys.CARD16;
    done: BOOLEAN;
BEGIN
  xfs.sys.ModifyTime (env.info.file^, time, done);
  ASSERT( done );
  tci.ModuleModifyTime (time);

  -- To use CRC32:
  --ASSERT( pcMarker.GetModCrc32(crcsum) );

  -- To use CRC16:
  ASSERT( pcMarker.GetCurrentModuleCrc16(crcsum16) );
  crcsum := crcsum16;

  tci.CRCSum (crcsum);

  tci.AddGlobalVar_ModuleInfo ();
END BeginTraversal;

--------------------------------------------------------------------------------
PROCEDURE AddProcedure_X2C_TCS_INIT (cu: pc.OBJECT; dynamic : BOOLEAN);
VAR proc: pc.OBJECT;
BEGIN
  proc := tci.Create_Procedure( "X2C_TCS_INIT"
                              , cu.type, pc.flag_m2
                              , fe.NullPos, fe.NullPos);

  proc.val.next := cu.val.l;
  cu.val.l := proc.val;
  proc.next:= cu.type.mem;
  cu.type.mem := proc;

  tci.AddModuleRegistration(proc.val.r, dynamic);
END AddProcedure_X2C_TCS_INIT;

--------------------------------------------------------------------------------
PROCEDURE EndTraversal (file-: ARRAY OF CHAR; cu: pc.OBJECT);
VAR dynamic: BOOLEAN;
BEGIN
  dynamic := tcc.DynamicModuleRegistration();

  IF tci.TestCount = 0 THEN
    tci.Create_Anchor(tc_Empty, cu.pos, cu.end);
  END;

  tci.AddGlobalConst_ModuleName (file);
  tci.AddGlobalSourceRefsAndCounters (dynamic);
  AddProcedure_X2C_TCS_INIT(cu, dynamic);
  tci.FinalizeRTSCalls (dynamic);
END EndTraversal;


--------------------------------------------------------------------------------
PROCEDURE ProcessModule * (file-: ARRAY OF CHAR; cu: pc.OBJECT);
BEGIN
  IF pcO.def THEN  RETURN;  END;

  IF (cu.flag = pc.flag_o2) AND env.info.main THEN
    tcc.SetMainModuleName(cu.name^);
  END;
  IF tcc.IgnoreModule(file) THEN
    pcO.enter_scope (cu.type);
    tci.RemoveInstrumentedModules(cu.name^);
    IF env.info.main AND (tci.Modules # NIL) THEN
      InsertTCSModuleBeginCall(cu.val);
    END;
    pcO.exit_scope();
    RETURN;
  ELSE
    tci.AddInstrumentedModules(cu.name^);
  END;

  UseModuleConstructor := (cu.flag IN fe.LangsWithModuleConstructors)
                      <* IF TARGET_MIPS OR TARGET_SPARC OR TARGET_VAX OR TARGET_PPC THEN *>
                        & NOT (pc.otag_nomodinit IN cu.tags)
                      <* END *>
                       ;
  ModuleBudyExist := cu.val.r # NIL;

  tci.Reset();
  tci.ExtendModuleImport (cu);

  pcO.enter_scope (cu.type);
  BeginTraversal ();

  ProcessProc_list (cu.val.l);
  ProcessModuleBody (cu.val);

  EndTraversal (file, cu);
  pcO.exit_scope();
END ProcessModule;


--------------------------------------------------------------------------------
PROCEDURE CreateTCSModule * ();
BEGIN
  tci.CreateTCSModule();
  tcc.Reset();
END CreateTCSModule;

--------------------------------------------------------------------------------
PROCEDURE AddModule * (file-: ARRAY OF CHAR; name-: ARRAY OF CHAR; isMain: BOOLEAN);
BEGIN
  IF isMain THEN
    tcc.SetMainModuleName(name);
  END;
  IF (pcO.def OR ~pcO.imp) AND NOT tcc.IgnoreModule(file) THEN
    tci.AddInstrumentedModules(name);
    tcc.TestcoverageActivated := TRUE;
  END;
END AddModule;

--------------------------------------------------------------------------------
PROCEDURE Init * (projectName: tcc.String);
BEGIN
  tcc.SetProjectName(projectName);
  env.config.SetEquation("TABSTOP","1");
END Init;

END tcMain.
