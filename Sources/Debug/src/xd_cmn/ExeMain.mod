IMPLEMENTATION MODULE ExeMain;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT eve := Events;
IMPORT erc := ExeReact;
IMPORT mem := Exe_Mem;

IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;

IMPORT kt  := KrnTypes;
IMPORT kex := KrnExec;
IMPORT ind := KrnIndex;
IMPORT kpr := Krn_Prog;
IMPORT dsm := Krn_Dasm;

IMPORT xs  := xStr;
IMPORT opt := Options;

IMPORT xi  := xdRTS;

<* IF DEST_XDS THEN *>
IMPORT dbg := Krn_Dbg;
<* END *>




(* По номеру регистра выдать его имя *)
PROCEDURE GetRegName (RegNo: CARDINAL; VAR RegName: ARRAY OF CHAR): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO kt.RegsNum-1 DO
    IF kt.Registers[i].reg_no = RegNo THEN
      COPY(kt.Registers[i].name, RegName);
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END GetRegName;



VAR
  stop_exec : BOOLEAN;
  stop_queue: BOOLEAN;
  step_mode : BOOLEAN;


PROCEDURE StopExec;
BEGIN
  stop_exec := TRUE;
 <* IF TARGET_x86 THEN *>
  dbg.SwitchToDebugger;
 <* END *>
END StopExec;


PROCEDURE StopQueueProc;
BEGIN
  stop_queue := TRUE;
END StopQueueProc;


(* Включение/выключение покомандного исполнения *)
PROCEDURE TurnStepModeOn;
BEGIN
  step_mode := TRUE;
END TurnStepModeOn;


PROCEDURE TurnStepModeOff;
BEGIN
  step_mode := FALSE;
END TurnStepModeOff;





(* Продолжить исполнение программы с текущего значения счетчика комманд       *)
(* Исполнение продолжается до возникновения события из множества Stop_Events, *)
(* после чего происходит обработка очереди событий и исполнение отлаживаемой  *)
(* программы продолжается. Для останова исполнения программы следует          *)
(* использовать StopExec                                                      *)
PROCEDURE Go;

  MODULE set_stop_exec;
  IMPORT stop_queue, stop_exec, kpr;
  BEGIN
    kpr.ProgramExecuting := TRUE;
    stop_queue := FALSE;
  FINALLY
    kpr.ProgramExecuting := FALSE;
    stop_exec := FALSE;
  END set_stop_exec;

VAR
  ip: kt.ADDRESS;
  i : CARDINAL;
  go_mode : kpr.GO_MODE;
  StepMade: BOOLEAN;
  Flag    : BOOLEAN;

BEGIN
  IF erc.NextReaction # NIL THEN              (* Реакции выполнены не все  *)
    erc.Raise (erc.NextReaction);
    IF erc.IsStopReact() THEN RETURN; END;    (* Реакция прервала цепочку  *)
    IF stop_queue THEN RETURN END;
  END;

  REPEAT

    IF NOT stop_exec THEN
    (*
     Сделать все реакции активными, независиио от того
     все собтытия обработаны, или нет.
     Очередь может быть не пуста в первом цикле - 
     эти события тоже надо обрабатывать !
    *)
      erc.Ready;
    END;

    IF NOT stop_exec AND eve.QueueIsEmpty() THEN 
         (* Все события обработаны    *)
      StepMade := FALSE;
      ip := mem.GetIP();
      IF WasBreakpoint = ip THEN
        Flag := FALSE;
        IF IsTmpBreakPoint(ip) THEN
          ClearTmpBreakPoint();
          Flag := TRUE;
        ELSIF IsBreakPoint (ip, i) THEN
          ASSERT(ClearBreakPoint(i));
          Flag := TRUE;
        END;
        IF Flag THEN
          go_mode.mode := kpr.SingleStep;
          go_mode.add_step := step_mode;
          kpr.Execute(go_mode);
          ASSERT(SetAbsBreakPoint(ip));
          StepMade := TRUE;
        END;
      END;
      WasBreakpoint := 0;
      IF step_mode THEN
        IF NOT StepMade THEN
          go_mode.mode := kpr.SingleStep;
          go_mode.add_step := TRUE;
          kpr.Execute(go_mode);
        END;
      ELSIF eve.QueueIsEmpty() THEN
        go_mode.mode := kpr.Go;
        kpr.Execute(go_mode);
      END;
    END;
    mem.GetCaches();
    WHILE NOT eve.QueueIsEmpty() DO
      eve.GetEvent;                                       -- Выбрать элемент из очереди
      erc.Raise (erc.FirstReaction (eve.LastEvent.Event)); -- цепочка реакций на событие
      IF erc.IsStopReact() THEN
        RETURN;
      END;
      IF stop_queue THEN
        RETURN;
      END;
    END;
  UNTIL stop_exec;
END Go;



(* Обрабатывает очередь событий, пока она не пуста или какая-либо реакция *)
(* не останавливает обработку очереди                                     *)
PROCEDURE ProcessEvents;
BEGIN
  -- Разрешить обработку очереди
  stop_queue := FALSE;
  IF erc.NextReaction # NIL THEN
    -- Довыполнить реакции в текущей цепочке
    erc.Raise (erc.NextReaction);
    IF erc.IsStopReact() THEN
      -- Прервать исполнение цепочки реаций
      RETURN;
    END;
  END;
  WHILE NOT stop_queue AND NOT eve.QueueIsEmpty() AND NOT erc.IsStopReact() DO
    erc.Ready;
    eve.GetEvent; -- Выбрать элемент из очереди
    erc.Raise (erc.FirstReaction (eve.LastEvent.Event)); -- Цепочка реакций на событие
  END;
END ProcessEvents;




PROCEDURE SkipToMain;

  MODULE reset_option;

  IMPORT ProgramSkipToMainEntry;

  BEGIN
    ProgramSkipToMainEntry := TRUE;
  FINALLY
    ProgramSkipToMainEntry := FALSE;
  END reset_option;

BEGIN
 IF opt.StopImmediately THEN RETURN;END;
  erc.Ready;
  IF JumpToMainEntry THEN
    JumpToMainEntry := FALSE;
    IF kex.ProgramMainEntryFound () THEN
      sys.EVAL (SetTmpBreakPoint (kex.ProgInfo.MainEntry));
    END;
    Go;
    IF kex.ProgramMainEntryFound () THEN
      RemoveTmpBreakPoint ();
    END;
    RETURN;
  ELSIF JumpToProgramEntry THEN
    JumpToProgramEntry := FALSE;
    IF kex.ProgramStartupEntryFound () THEN
      sys.EVAL (SetTmpBreakPoint (kex.ProgInfo.StartupEntry));
    END;
    Go;
    IF kex.ProgramStartupEntryFound () THEN
      RemoveTmpBreakPoint ();
    END;
    RETURN;
  END;
  IF NOT eve.QueueIsEmpty () THEN
    StopExec ();
    Go;
  END;
END SkipToMain;



PROCEDURE Restart (): BOOLEAN;
BEGIN
  IF NOT kex.Loaded THEN
    RETURN FALSE;
  END;
  UnloadProgram;
  IF NOT kpr.RestartProgram () THEN
    RETURN FALSE;
  END;
  kex.ProgramContextOk := TRUE;
  TurnStepModeOff;
  mem.GetCaches();
  SkipToMain;
  kex.Loaded := TRUE;
 <* IF TARGET_x86 THEN *>
  dbg.StartSmartSelector;
 <* END *>
  RETURN TRUE;
END Restart;


PROCEDURE UnloadProgram;
BEGIN
  RemoveTmpBreakPoint();
  kpr.UnloadProgram;
  kex.UnloadProgram;
  tls.ClearComponents(FALSE);
  eve.ClearQueue;
  kex.Loaded := FALSE;
  kex.ProgramContextOk := FALSE;
  WasBreakpoint := 0;
END UnloadProgram;



VAR
  CodeBreakInterrupt: kt.BREAK_CODE;


(* Установка/снятие точки останова по адресу *)
PROCEDURE SetBreakPoint (addr: kt.ADDRESS; VAR Index: CARDINAL): BOOLEAN;
VAR
  I: ind.INDEX_REC;
BEGIN
  I := ind.NEW_INDEX;
  I.Access := ind.EXECUTE;
  I.Addr := addr;
  IF NOT mem.Get(addr, sys.ADR(I.OpCode), kt.BreakLen) THEN RETURN FALSE; END;
  IF NOT SetAbsBreakPoint(addr) THEN RETURN FALSE; END;
  Index := ind.AddIndex(I);
  RETURN TRUE;
END SetBreakPoint;


PROCEDURE RemoveBreakPoint (VAR index: CARDINAL): BOOLEAN;
VAR
  tmp: CARDINAL;
  I: ind.INDEX_REC;
BEGIN
  IF NOT ind.DelIndex (index, I) THEN
    RETURN FALSE;
  END;
  ASSERT (I.Access = ind.EXECUTE);
  tmp := index;
  index := ind.Invalid_Index;
  IF ind.Find (tmp) THEN
    RETURN TRUE;
  ELSE
    RETURN mem.Put (I.Addr, sys.ADR(I.OpCode), kt.BreakLen);
  END;
END RemoveBreakPoint;


PROCEDURE IsBreakPoint (addr: kt.ADDRESS; VAR index: CARDINAL): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  WITH ind.Index DO
    IF Free > 0 THEN
      FOR i := 0 TO Free-1 DO
        WITH Index^[i] DO
          IF Busy AND (Access = ind.EXECUTE) AND (Addr = addr) THEN
            index := i+1;
            RETURN TRUE;
          END;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END IsBreakPoint;


PROCEDURE BreakPointOpCode (index: CARDINAL; VAR opcode: CARDINAL): BOOLEAN;
BEGIN
  WITH ind.Index DO
    IF (Index = NIL) OR (index = 0) OR (index > Free) THEN RETURN FALSE; END;
    WITH Index^[index-1] DO
      IF Busy AND (Access = ind.EXECUTE) THEN
        opcode := 0;
        sys.MOVE(sys.ADR(OpCode), sys.ADR(opcode), kt.BreakLen);
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    END;
  END;
END BreakPointOpCode;


PROCEDURE ClearBreakPoint (index: CARDINAL): BOOLEAN;
VAR
  opcode: CARDINAL;
BEGIN
  RETURN BreakPointOpCode (index, opcode) AND mem.Put(ind.Index.Index^[index-1].Addr, sys.ADR(opcode), kt.BreakLen);
END ClearBreakPoint;


PROCEDURE UpdateBreakPoint (index: CARDINAL): BOOLEAN;
BEGIN
  WITH ind.Index DO
    IF (Index = NIL) OR (index = 0) OR (index > Free) THEN RETURN FALSE; END;
    WITH Index^[index-1] DO
      RETURN Busy AND (Access = ind.EXECUTE) AND SetAbsBreakPoint(Addr);
    END;
  END;
END UpdateBreakPoint;




TYPE
  TMP_BREAKPOINT = RECORD
                     Addr   : kt.ADDRESS;
                     OpCode : kt.BREAK_CODE;
                     Present: BOOLEAN;
                   END;

VAR
  TmpBreakpoint: TMP_BREAKPOINT;


PROCEDURE SetTmpBreakPoint (addr: kt.ADDRESS): BOOLEAN;
BEGIN
  RemoveTmpBreakPoint;
  WITH TmpBreakpoint DO
    Addr := addr;
    IF NOT mem.Get(addr, sys.ADR(OpCode), kt.BreakLen) THEN RETURN FALSE; END;
    IF NOT SetAbsBreakPoint(addr) THEN RETURN FALSE; END;
    Present := TRUE;
  END;
  RETURN TRUE;
END SetTmpBreakPoint;


PROCEDURE RemoveTmpBreakPoint;
BEGIN
  ClearTmpBreakPoint ();
  TmpBreakpoint.Addr := 0;
  TmpBreakpoint.OpCode := kt.BREAK_CODE {0};
  TmpBreakpoint.Present := FALSE;
END RemoveTmpBreakPoint;


PROCEDURE ClearTmpBreakPoint;
BEGIN
  WITH TmpBreakpoint DO
    IF Present THEN
      -- не проверяем результат исполнения, так как возможна
      -- ситуация, когда временная точка останова устанавливалась
      -- в компоненту, которая была позднее отгружена
      sys.EVAL (mem.Put (Addr, sys.ADR(OpCode), kt.BreakLen));
    END;
  END;
END ClearTmpBreakPoint;


PROCEDURE IsTmpBreakPoint (addr: kt.ADDRESS): BOOLEAN;
BEGIN
  WITH TmpBreakpoint DO
    RETURN Present AND (addr = Addr);
  END;
END IsTmpBreakPoint;


PROCEDURE UpdateTmpBreakPoint (): BOOLEAN;
BEGIN
  WITH TmpBreakpoint DO
    RETURN Present AND SetAbsBreakPoint(Addr);
  END;
END UpdateTmpBreakPoint;


PROCEDURE SetAbsBreakPoint (addr: kt.ADDRESS): BOOLEAN;
BEGIN
  RETURN mem.Put(addr, sys.ADR(CodeBreakInterrupt), kt.BreakLen);
END SetAbsBreakPoint;




<* PUSH *>
<* WOFF301+ *>
PROCEDURE ResolveAddr(target, addr: kt.ADDRESS; VAR str: ARRAY OF CHAR): BOOLEAN;
VAR
  com : CARDINAL;
  mod : CARDINAL;
  proc: dt.OBJECT;
  name: xs.txt_ptr;
  obj : xs.String;

  new_addr: kt.ADDRESS;

<* IF TARGET_x86 THEN *>
  tmp: kt.ADDRESS;
<* END *>

BEGIN
  IF opt.DisasmMode THEN
    new_addr := addr;
   <* IF TARGET_x86 THEN *>
    tmp := dsm.IsJmpForDll (addr);
    IF tmp # 0 THEN
      new_addr := tmp;
    END;
   <* END *>
    IF tls.FindModByAddr (new_addr, com, mod) THEN
      proc := tls.FindLabelByAddr (com, mod, new_addr);
      IF tls.IsObjectValid(proc) THEN
        ASSERT(tls.ModName(com, mod, name));
        fmt.append(str, "%s.", name^);
        tls.ObjectName (proc, obj);
        fmt.append(str, "%s", obj);
        RETURN TRUE;
      END;
    END;
    IF tls.FindPublicByAddr(new_addr, TRUE, com, name) THEN
      fmt.append(str, "%s", name^);
      RETURN TRUE;
    END;
  END;
  fmt.append(str, "%$8X", addr);
  RETURN FALSE;
END ResolveAddr;


VAR
  ESP, BOTTOM: kt.ADDRESS;


PROCEDURE ResolveEA (target, addr: kt.ADDRESS; len: CARDINAL; VAR str: ARRAY OF CHAR);

TYPE
  GetNameResults = (resolved, not_found, unknown);


  PROCEDURE GetName (addr: kt.ADDRESS; VAR str: ARRAY OF CHAR; VAR code: BOOLEAN; VAR com: dt.ComNo): GetNameResults;
  VAR
    mod: dt.ModNo;
    com_name: xs.txt_ptr;
    obj: CARDINAL;
    object: dt.OBJECT;
    name: xs.txt_ptr;
    var : xs.String;
    start: kt.ADDRESS;
  BEGIN
    IF NOT tls.FindComObjByAddr (addr, com, obj) THEN
      RETURN unknown;
    END;
    code := (kt.execute IN dt.Components.Components^[com].EI.Objects^[obj].Attributes);
    IF code THEN
      IF tls.FindModInCompByAddr (com, addr, mod) THEN
        object := tls.FindProcByAddr (com, mod, addr);
      ELSE
        object := dt.Invalid_Object;
      END;
    ELSE
      object := tls.FindVarByAddr (com, addr, FALSE);
    END;
    IF tls.IsObjectValid(object) THEN
      IF tls.ModName(com, tls.ObjectMod(object), name) THEN
        fmt.append (str, "%s.", name^);
      END;
      tls.ObjectName(object, var);
      fmt.append(str, "%s", var);
      ASSERT (tls.ObjectAddr(object, start));
      IF start # addr THEN
        fmt.append (str, "+0x%X", addr-start);
      END;
    ELSIF tls.FindPublicByAddr (addr, FALSE, com, name) THEN
      ASSERT (tls.ComName (com, com_name));
      fmt.append (str, "%s.%s", com_name^, name^);
      ASSERT (tls.FindPublicByNameInCom (com, name^, start));
      IF start < addr THEN
        fmt.append (str, "+0x%X", addr-start);
      ELSIF start > addr THEN
        fmt.append (str, "-0x%X", start-addr);
      END;
    ELSE
      RETURN not_found;
    END;
    RETURN resolved;
  END GetName;


<* WOFF304+ *>
  PROCEDURE contents;
  VAR
    data32, data32_2: sys.CARD32;
    data16: sys.CARD16;
    data8 : sys.CARD8;
    tmp   : xs.String;
    code  : BOOLEAN;
    com   : dt.ComNo;
  BEGIN
    CASE len OF
    | 1:
      IF mem.Get(addr, sys.ADR(data8), len) THEN
        fmt.append(str, "=%$2X", data8);
      ELSE
        fmt.append(str, "=??");
      END;
    | 2:
      IF mem.Get(addr, sys.ADR(data16), len) THEN
        fmt.append(str, "=%$4X", data16);
      ELSE
        fmt.append(str, "=????");
      END;
    | 4:
      IF mem.Get(addr, sys.ADR(data32), len) THEN
        tmp:="";
        IF GetName (data32, tmp, code, com) = resolved THEN
          fmt.append(str, "=%s", tmp);
        ELSE
          fmt.append(str, "=%$8X", data32);
        END;
      ELSE
        fmt.append(str, "=????????");
      END;
    | 6:
      IF mem.Get(addr, sys.ADR(data32), 4) & mem.Get(addr+4, sys.ADR(data16), 2) THEN
        fmt.append(str, "=%$4X:%$8X", data16, data32);
      ELSE
        fmt.append(str, "=????:????????");
      END;
    | 8:
      IF mem.Get(addr, sys.ADR(data32), 4) & mem.Get(addr+4, sys.ADR(data32_2), 4) THEN
        fmt.append(str, "=%$8X%$8X", data32_2, data32);
      ELSE
        fmt.append(str, "=????????????????");
      END;
    | 10:
      IF mem.Get(addr, sys.ADR(data32), 4) & mem.Get(addr+4, sys.ADR(data32_2), 4) & mem.Get(addr+8, sys.ADR(data16), 2) THEN
        fmt.append(str, "=%$4X%$8X%$8X", data16, data32_2, data32);
      ELSE
        fmt.append(str, "=????????????????????");
      END;
    ELSE
    END;
  END contents;

<* POP *>

VAR
  code: BOOLEAN;
  com : CARDINAL;
  size, esp, bottom: CARDINAL;
  access: kt.ATTRIBS;
  com_name: xs.txt_ptr;

BEGIN
  IF opt.DisasmMode THEN
    esp := mem.GetSP();
    IF esp # ESP THEN
      IF mem.GetSegmentInfo(esp, BOTTOM, size, access) THEN;
        INC(BOTTOM, size);
        ESP := esp;
      ELSE
        RETURN;
      END;
    END;
    bottom := BOTTOM;
    IF (addr >= esp) AND (addr <= bottom) THEN
      fmt.append(str, "SS:%$8X", addr);
      contents;
    ELSE
      CASE GetName (addr, str, code, com) OF
      | unknown:
        fmt.append(str, ":%$8X", addr);
      | not_found:
        ASSERT (tls.ComName (com, com_name));
        IF code THEN
          fmt.append (str, "CS[%s]:%$8X ", com_name^, addr);
        ELSE
          fmt.append (str, "DS[%s]:%$8X", com_name^, addr);
        END;
      | resolved:
        -- адреса уже превратились в имена
      END;
      contents;
    END;
  END;
END ResolveEA;

VAR
  cache: ARRAY [0..63] OF sys.CARD8;

(* Дисассемблирование команды *)
PROCEDURE DisasmInstr (addr: kt.ADDRESS; curr: BOOLEAN; VAR asm, info: ARRAY OF CHAR; VAR len: CARDINAL): BOOLEAN;
VAR
  res : BOOLEAN;
  i   : CARDINAL;
  code: ARRAY [0..3] OF CHAR;
  s, t: xs.String;

BEGIN
  res := dsm.Disasm(addr, curr, asm, info, len);
  IF NOT res THEN
    COPY ('?????', asm);
    COPY ('', info);
  END;
  IF opt.Code THEN
    IF res THEN
      ASSERT(mem.Get (addr, sys.ADR(cache), len));
      s := '';
      FOR i := 0 TO len-1 DO
        fmt.print (code, "%$2X ", cache[i]);
        xs.Append (code, s);
      END;
    ELSIF mem.Get (addr, sys.ADR(cache), 1) THEN
      fmt.print (s, "%$2X ", cache[0]);
    ELSE
      COPY ('?????', s);
    END;
    fmt.print(t, "│%-32s│ %s", s, asm);
    COPY(t, asm);
  END;
  RETURN res;
END DisasmInstr;


CONST
  InvalidScopeName = '???';

BEGIN
  stop_exec := FALSE;
  ExecAddr  := 0;
  ExecComp  := dt.Invalid_Component;
  ExecMod   := 0;
  ExecLine  := 0;
  ExecScope := dt.Invalid_Object;
  ESP    := 0;
  BOTTOM := 0;
  CodeBreakInterrupt := kt.CodeBreakInterrupt;
  TmpBreakpoint      := TMP_BREAKPOINT{0, kt.BREAK_CODE{0 BY kt.BreakLen}, FALSE};
  dsm.ResolveAddr := ResolveAddr;
  dsm.ResolveEA   := ResolveEA;
  Stop_Events := EVENTS_SET{ eve.InternalError  (* Исключительная ситуация в отладчике *)
                           , eve.Exception      (* Исключительная ситуация в программе *)
                           , eve.BreakpointHit  (* Точка останова                      *)
                           , eve.SingleStep     (* Исполнена одна команда              *)
                           , eve.Call           (* Выполнена инструкция CALL           *)
                           , eve.Return         (* Выполнена инструкция RET            *)
                           , eve.MemoryAccess   (* Достпуп к памяти                    *)
                          <* IF DEST_K26 THEN *>
                           , eve.RegisterAccess (* Достпуп к регистрам                 *)
                           , eve.DeviceAccess   (* Обращение к устройству              *)
                          <* END *>
                           };
  ProgramSkipToMainEntry := FALSE;
  JumpToMainEntry := TRUE;
  JumpToProgramEntry := FALSE;
END ExeMain.
