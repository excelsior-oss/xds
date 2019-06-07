<* Storage+ *>

IMPLEMENTATION MODULE Breaks;

IMPORT sys := SYSTEM;

IMPORT xStr;
IMPORT exp := Expr;

IMPORT mem := Exe_Mem;
IMPORT exe := ExeMain;

IMPORT kmem:= Krn_Mem;

IMPORT tls := DI_Tools;
IMPORT dt  := DI_Types;



(* Сравнение двух остановов, если "равны" вернет TRUE *)
PROCEDURE CompareBreaks (break1, break2: BREAK): BOOLEAN;
BEGIN
  RETURN (break1.Active = break2.Active) AND
         (break1.Sticky = break2.Sticky) AND
         (break1.Pass   = break2.Pass) AND
         (break1.Index  = break2.Index) AND
         (break1.Owner  = break2.Owner);
END CompareBreaks;


(* Сравнение двух точек останова, если "равны" вернет TRUE *)
PROCEDURE CompareBreakpoints (breakpoint1, breakpoint2: BREAKPOINT): BOOLEAN;
BEGIN
  RETURN CompareBreaks(breakpoint1.Break, breakpoint2.Break) AND
                      (breakpoint1.Pos.ComN = breakpoint2.Pos.ComN) AND
                      (breakpoint1.Pos.ModN = breakpoint2.Pos.ModN) AND
                      (breakpoint1.Line  = breakpoint2.Line) AND
                      (breakpoint1.Addr  = breakpoint2.Addr) AND
                      (breakpoint1.Kind  = breakpoint2.Kind) AND
                      ((breakpoint1.Condition = NIL) AND (breakpoint2.Condition = NIL) OR
                      ((breakpoint1.Condition # NIL) AND (breakpoint2.Condition # NIL) AND
                      (breakpoint1.Condition^ = breakpoint2.Condition^)));
END CompareBreakpoints;


CONST
  HN = 16;

VAR
  HasModuleCount: CARDINAL; (* Количество точек останова, имеющий модуль *)


PROCEDURE Add_Breakpoint (BP: BREAKPOINT; replace: BOOLEAN; VAR index: CARDINAL): BOOLEAN;
VAR
  tmp: PBREAKPOINT;
  pos,
  i  : CARDINAL;

BEGIN
  IF NOT mem.IsAddrFromExecutableSeg(BP.Addr) THEN RETURN FALSE; END;
  WITH Breakpoints DO
    IF Breakpoints = NIL THEN
      NEW(Breakpoints, HN);
      IF Breakpoints = NIL THEN RETURN FALSE; END;
      IF NOT exe.SetBreakPoint(BP.Addr, index) THEN RETURN FALSE END;
      BP.Break.Index := index;
      Breakpoints^[0] := BP;
      free := 1;
      IF (BP.Pos.ModN = 0) OR (BP.Line = 0) THEN
        HasModuleCount := 0;
      ELSE
        HasModuleCount := 1;
      END;
    ELSE
      IF free > HIGH(Breakpoints^) THEN
        NEW(tmp, HIGH(Breakpoints^)+1 + HN);
        IF tmp = NIL THEN RETURN FALSE; END;
        sys.MOVE(sys.ADR(Breakpoints^), sys.ADR(tmp^),
                 (HIGH(Breakpoints^)+1)*SIZE(BREAKPOINT));
        DISPOSE(Breakpoints);
        Breakpoints := tmp;
      END;
      IF free > 0 THEN
        <* PUSH *>
        <* WOFF312+ *>
        LOOP
          IF (BP.Pos.ModN # 0) AND (BP.Line # 0) THEN
            IF HasModuleCount = 0 THEN
              pos := 0;
              EXIT;
            ELSE
              FOR i := 0 TO HasModuleCount-1 DO
                WITH Breakpoints^[i] DO
                  IF BP.Pos.ComN = Pos.ComN THEN
                    IF BP.Pos.ModN = Pos.ModN THEN
                      IF BP.Line = Line THEN
                        IF replace THEN
                          IF BP.Break.Active THEN
                            IF NOT exe.UpdateBreakPoint(Break.Index) THEN RETURN FALSE; END;
                          END;
                          index := Break.Index;
                          BP.Break.Index := index;
                          Breakpoints^[i] := BP;
                          RETURN TRUE;
                        ELSE
                          pos := i;
                          REPEAT
                            INC(pos);
                          UNTIL (pos = free) OR
                                (BP.Pos.ModN # Breakpoints^[pos].Pos.ModN) OR
                                (BP.Line  # Breakpoints^[pos].Line);
                          EXIT;
                        END;
                      ELSIF (BP.Line < Line) THEN
                        pos := i;
                        EXIT;
                      END;
                    ELSIF BP.Pos.ModN < Pos.ModN THEN
                      pos := i;
                      EXIT;
                    END;
                  ELSIF BP.Pos.ComN < Pos.ComN THEN
                    pos := i;
                    EXIT;
                  END;
                END;
              END;
              pos := HasModuleCount;
              EXIT;
            END;
          ELSE
            FOR i := HasModuleCount TO free-1 DO
              WITH Breakpoints^[i] DO
                IF BP.Addr = Addr THEN
                  IF replace THEN
                    IF BP.Break.Active THEN
                      IF NOT exe.UpdateBreakPoint(Break.Index) THEN RETURN FALSE; END;
                    END;
                    index := Break.Index;
                    BP.Break.Index := index;
                    Breakpoints^[i] := BP;
                    RETURN TRUE;
                  ELSE
                    pos := i;
                    REPEAT
                      INC(pos);
                    UNTIL (pos = free) OR (BP.Addr # Breakpoints^[pos].Addr);
                    EXIT;
                  END;
                ELSIF BP.Addr < Addr THEN
                  pos := i;
                  EXIT;
                END;
              END;
            END;
            pos := free;
            EXIT;
          END;
        END;
        IF NOT exe.SetBreakPoint(BP.Addr, index) THEN RETURN FALSE END;
        BP.Break.Index := index;
        <* POP *>
        IF pos <= HasModuleCount THEN INC(HasModuleCount); END;
        IF pos # free THEN
          FOR i := free TO pos+1 BY -1 DO
            Breakpoints^[i] := Breakpoints^[i-1];
          END;
        END;
        Breakpoints^[pos] := BP;
        INC(free);
      ELSE
        IF NOT exe.SetBreakPoint(BP.Addr, index) THEN RETURN FALSE; END;
        BP.Break.Index := index;
        Breakpoints^[0] := BP;
        free := 1;
        IF tls.IsPosValid(BP.Pos) AND (BP.Line # 0) THEN
          HasModuleCount := 1;
        ELSE
          HasModuleCount := 0;
        END;
      END;
    END;
  END;
  RETURN TRUE;
END Add_Breakpoint;


PROCEDURE Delete_Breakpoint (BreakPos: CARDINAL): BOOLEAN;
VAR
  index : CARDINAL;
BEGIN
  WITH Breakpoints DO
    IF BreakPos >= free THEN RETURN FALSE; END;
    WITH Breakpoints^[BreakPos] DO
      index := Break.Index;
      IF Condition # NIL THEN xStr.dealloc_str(Condition); END;
      IF tls.IsPosValid(Pos) AND (Line # 0) THEN
        ASSERT(HasModuleCount # 0);
        DEC(HasModuleCount);
      END;
    END;
    IF BreakPos < free-1 THEN
      sys.MOVE(sys.ADR(Breakpoints^[BreakPos+1]), sys.ADR(Breakpoints^[BreakPos]),
              (free-1-BreakPos)*SIZE(BREAKPOINT));
    END;
    DEC(free);
  END;
<* PUSH *>
<* WOFF900+ *>
<* WOFF903+ *>
  IF (index # 0) AND exe.RemoveBreakPoint(index) THEN END;
<* POP *>
  RETURN TRUE;
END Delete_Breakpoint;


PROCEDURE DeleteAll_Breakpoint (): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 1 TO Breakpoints.free DO
    IF NOT Delete_Breakpoint(0) THEN RETURN FALSE; END;
  END;
  RETURN TRUE;
END DeleteAll_Breakpoint;


(* Update 0xCC instruction in program code *)
PROCEDURE Refresh_Breakpoints;
VAR
  i: CARDINAL;
BEGIN
  WITH Breakpoints DO
    IF free > 0 THEN
      FOR i := 0 TO free-1 DO
        WITH Breakpoints^[i] DO
          IF Break.Active AND (Break.Index > 0) THEN
            Break.Active := exe.UpdateBreakPoint(Break.Index);
          END;  
          Break.Pass := 0;
          IF init_value # 0 THEN
            Break.Pass := init_value;
          END;
        END;
      END;
    END;
  END;
END Refresh_Breakpoints;


PROCEDURE Switch_Breakpoint (active: BOOLEAN; BreakPos: CARDINAL): BOOLEAN;
BEGIN
  WITH Breakpoints DO
    IF BreakPos >= free THEN RETURN FALSE; END;
    WITH Breakpoints^[BreakPos].Break DO
      IF active THEN
        IF NOT exe.UpdateBreakPoint(Index) THEN RETURN FALSE; END;
        Active := TRUE;
      ELSE
        IF NOT exe.ClearBreakPoint(Index) THEN RETURN FALSE; END;
        Active := FALSE;
      END;
    END;
  END;
  RETURN TRUE;
END Switch_Breakpoint;


PROCEDURE Enable_Breakpoint (BreakPos: CARDINAL): BOOLEAN;
BEGIN
  RETURN Switch_Breakpoint (TRUE, BreakPos);
END Enable_Breakpoint;


PROCEDURE Disable_Breakpoint (BreakPos: CARDINAL): BOOLEAN;
BEGIN
  RETURN Switch_Breakpoint (FALSE, BreakPos);
END Disable_Breakpoint;


PROCEDURE FindByIndex_Breakpoint (Index: CARDINAL; VAR BreakPos: CARDINAL): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  WITH Breakpoints DO
    IF (Breakpoints#NIL) AND (free # 0) THEN
      FOR i := 0 TO free-1 DO
        IF Breakpoints^[i].Break.Index = Index THEN
          BreakPos := i;
          RETURN TRUE;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END FindByIndex_Breakpoint;


PROCEDURE Add_AccessBreak (VAR AB: ACCESS_BREAK): BOOLEAN;
VAR
  tmp: PACCESS_BREAK;

BEGIN
  WITH AccessBreaks DO
    IF AccessBreaks = NIL THEN
      NEW(AccessBreaks, HN);
      IF AccessBreaks = NIL THEN RETURN FALSE; END;
    ELSIF free > HIGH(AccessBreaks^) THEN
      NEW(tmp, HN+HIGH(AccessBreaks^)+1);
      IF tmp = NIL THEN RETURN FALSE; END;
      sys.MOVE(sys.ADR(AccessBreaks^[0]), sys.ADR(tmp^[0]), (HIGH(AccessBreaks^)+1)*SIZE(AB));
      DISPOSE(AccessBreaks);
      AccessBreaks := tmp;
    END;
    WITH AB DO
      WITH Access_Data DO

<* IF TARGET_VAX THEN *>

        CASE Access_ID OF
        | Register: IF NOT kmem.SetRegTrace(Access_Type, Reg_No, Break.Index) THEN RETURN FALSE; END;
        | Memory  : IF NOT kmem.SetTrace(Access_Type, Location, Len, Break.Index) THEN RETURN FALSE; END;
        | Port    :
        ELSE
          ASSERT(FALSE);
        END;

<* ELSE *>

        ASSERT( Access_ID = Memory );
        IF NOT kmem.SetTrace(Access_Type, Location, Len, Break.Index) THEN
           WITH Break DO
             Active := FALSE;
             Index := 0;
           END;
        END;

<* END *>

      END;
    END;
    AccessBreaks^[free] := AB;
    INC(free);
  END;
  RETURN TRUE;
END Add_AccessBreak;


PROCEDURE Delete_AccessBreak (BreakPos: CARDINAL): BOOLEAN;
BEGIN
  WITH AccessBreaks DO
    IF BreakPos >= free THEN RETURN FALSE; END;
    WITH AccessBreaks^[BreakPos] DO
      WITH Access_Data DO

<* IF TARGET_VAX THEN *>

        CASE Access_ID OF
        | Register: IF (Break.Index > 0) AND NOT kmem.RemoveRegTrace(Break.Index) THEN RETURN FALSE; END;
        | Memory  : IF (Break.Index > 0) AND NOT kmem.RemoveTrace(Break.Index) THEN RETURN FALSE; END;
        | Port    :
        ELSE
          ASSERT(FALSE);
        END;

<* ELSE *>

        ASSERT( Access_ID = Memory );
        IF (Break.Index > 0) AND NOT kmem.RemoveTrace(Break.Index) THEN RETURN FALSE; END;

<* END *>
      END;
    END;
    IF BreakPos < free-1 THEN
      sys.MOVE(sys.ADR(AccessBreaks^[BreakPos+1]), sys.ADR(AccessBreaks^[BreakPos]),
              (free-1-BreakPos)*SIZE(ACCESS_BREAK));
    END;
    DEC(free);
  END;
  RETURN TRUE;
END Delete_AccessBreak;


PROCEDURE DeleteAll_AccessBreak;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 1 TO AccessBreaks.free DO
<* PUSH *>
<* WOFF903+ *>
    IF NOT Delete_AccessBreak(0) THEN END;
<* POP *>
  END;
END DeleteAll_AccessBreak;


PROCEDURE FindByIndex_AccessBreak (Index: CARDINAL; VAR BreakPos: CARDINAL): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  WITH AccessBreaks DO
    IF (AccessBreaks#NIL) AND (free # 0)  THEN
      FOR i := 0 TO free-1 DO
        IF AccessBreaks^[i].Break.Index = Index THEN
          BreakPos := i;
          RETURN TRUE;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END FindByIndex_AccessBreak;


PROCEDURE Clear_AccessBreak;

<* IF TARGET_VAX THEN *>
BEGIN
<*ELSE *>
VAR
  i: CARDINAL;
BEGIN
  WITH AccessBreaks DO
    IF (AccessBreaks#NIL) AND (free # 0)  THEN
      FOR i := 0 TO free-1 DO
        WITH AccessBreaks^[i] DO
          WITH Access_Data DO
            ASSERT( Access_ID = Memory );
            IF Break.Active THEN
              ASSERT(kmem.RemoveTrace (Break.Index));
              -- если Break.Active = TRUE, то после рестарта в процедуре
              -- Refresh такие точки останова будет востанновлены
            END;
          END;
        END;
      END;
    END;
  END;
<* END *>
END Clear_AccessBreak;


PROCEDURE Refresh_AccessBreak;

<* IF TARGET_VAX THEN *>
BEGIN
<*ELSE *>
VAR
  i: CARDINAL;
BEGIN
  WITH AccessBreaks DO
    IF (AccessBreaks#NIL) AND (free # 0)  THEN
      FOR i := 0 TO free-1 DO
        WITH AccessBreaks^[i] DO
          WITH Access_Data DO
            ASSERT( Access_ID = Memory );
            IF Break.Active THEN
              Break.Active := kmem.SetTrace(Access_Type, Location, Len, Break.Index);
            END;
            IF NOT mem.Get(Location, sys.ADR(Prev_value[0]), Len) THEN
              sys.FILL(sys.ADR(Prev_value[0]), 0, Len);
            END;
            Prev_value_long := NIL;
          END;
        END;
      END;
    END;
  END;
<* END *>
END Refresh_AccessBreak;




PROCEDURE Add_ConditionBreak (CB: CONDITION_BREAK): BOOLEAN;
VAR
  tmp: PCONDITION_BREAK;

BEGIN
  WITH ConditionBreaks DO
    IF ConditionBreaks = NIL THEN
      NEW(ConditionBreaks, HN);
      IF ConditionBreaks = NIL THEN RETURN FALSE; END;
    ELSIF free > HIGH(ConditionBreaks^) THEN
      NEW(tmp, HN+HIGH(ConditionBreaks^)+1);
      IF tmp = NIL THEN RETURN FALSE; END;
      sys.MOVE(sys.ADR(ConditionBreaks^[0]), sys.ADR(tmp^[0]), (HIGH(ConditionBreaks^)+1)*SIZE(CB));
      DISPOSE(ConditionBreaks);
      ConditionBreaks := tmp;
    END;
    ConditionBreaks^[free] := CB;
    INC(free);
  END;
  RETURN TRUE;
END Add_ConditionBreak;


PROCEDURE Delete_ConditionBreak (BreakPos: CARDINAL): BOOLEAN;
BEGIN
  WITH ConditionBreaks DO
    IF (ConditionBreaks = NIL) OR (free <= BreakPos) THEN RETURN FALSE; END;
    WITH ConditionBreaks^[BreakPos] DO
      IF Expr # NIL THEN xStr.dealloc_str(Expr); END;
    END;
    IF BreakPos < free-1 THEN
      sys.MOVE(sys.ADR(ConditionBreaks^[BreakPos+1]),
               sys.ADR(ConditionBreaks^[BreakPos]),
               SIZE(CONDITION_BREAK)*(free-BreakPos));
    END;
    DEC(free);
  END;
  RETURN TRUE;
END Delete_ConditionBreak;


PROCEDURE DeleteAll_ConditionBreak (): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 1 TO ConditionBreaks.free DO
    IF NOT Delete_ConditionBreak(0) THEN RETURN FALSE; END;
  END;
  RETURN TRUE;
END DeleteAll_ConditionBreak;


PROCEDURE NeedToStop (com: dt.ComNo; mod: dt.ModNo; VAR BreakPos: CARDINAL): BOOLEAN;
VAR
  i: CARDINAL;
  expr: exp.ExprRes;
BEGIN
  WITH ConditionBreaks DO
    IF ConditionBreaks = NIL THEN RETURN FALSE; END;
    FOR i := BreakPos+1 TO free DO
      WITH ConditionBreaks^[i-1] DO
        IF Break.Active AND (Break.Pass = 0) THEN
          exp.CalcExpr (com, mod, Expr^, expr);
          IF (exp.error = 0) AND exp.dfn THEN
            WITH expr DO
              IF (sort = exp.BOOLval) AND b_val THEN
                BreakPos := i-1;
                RETURN TRUE;
              END;
            END;
          END;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END NeedToStop;


(* Проверяет необходимость включения пошагового исполнения *)
PROCEDURE NeedToStepMode (): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  WITH ConditionBreaks DO
    IF (ConditionBreaks = NIL) OR (free = 0) THEN RETURN FALSE; END;
    FOR i := 0 TO free-1 DO
      WITH ConditionBreaks^[i] DO
        IF Break.Active THEN RETURN TRUE; END;
      END;
    END;
  END;
  RETURN FALSE;
END NeedToStepMode;


<* PUSH *>
<* WOFF903+ *>

PROCEDURE ClearBreaks;
VAR
  i: CARDINAL;
BEGIN
  WITH Breakpoints DO
    IF (Breakpoints # NIL) AND (free # 0) THEN
      FOR i := 0 TO free-1 DO
        IF Delete_Breakpoint (i) THEN END;
        WITH Breakpoints^[i] DO
          IF Condition # NIL THEN xStr.dealloc_str(Condition); END;
        END;
      END;
      DISPOSE(Breakpoints);
    END;
  END;
  Breakpoints := BREAKPOINTS{ NIL, 0};

  WITH AccessBreaks DO
    IF AccessBreaks # NIL THEN
      FOR i := 0 TO HIGH(AccessBreaks^) DO
        IF Delete_AccessBreak (i) THEN END;
        WITH AccessBreaks^[i].Access_Data DO
          IF (Access_ID = Memory) AND (Prev_value_long # NIL) THEN
            xStr.dealloc_str(Prev_value_long);
          END;
        END;
      END;
      DISPOSE(AccessBreaks);
    END;
  END;
  AccessBreaks := ACCESS_BREAKS{ NIL, 0};

  WITH ConditionBreaks DO
    IF ConditionBreaks # NIL THEN
      FOR i := 0 TO HIGH(ConditionBreaks^) DO
        IF Delete_ConditionBreak (i) THEN END;
        WITH ConditionBreaks^[i] DO
          IF Expr # NIL THEN xStr.dealloc_str(Expr); END;
        END;
      END;
      DISPOSE(ConditionBreaks);
    END;
  END;
  ConditionBreaks := CONDITION_BREAKS{ NIL, 0};

END ClearBreaks;

<* POP *>



BEGIN
  Breakpoints     := BREAKPOINTS{ NIL, 0};        (* Точки останова      *)
  AccessBreaks    := ACCESS_BREAKS{ NIL, 0};      (* Остановы по доступу *)
  ConditionBreaks := CONDITION_BREAKS{ NIL, 0};   (* Условные остановы   *)
END Breaks.
