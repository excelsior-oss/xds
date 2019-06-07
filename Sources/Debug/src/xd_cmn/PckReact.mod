<* Storage + *>

IMPLEMENTATION MODULE PckReact;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT xs  := xStr;
IMPORT opt := Options;
IMPORT msg := MsgNo;
IMPORT pro := Protocol;
IMPORT fil := File;

IMPORT mem := Exe_Mem;
IMPORT erc := ExeReact;
IMPORT exe := ExeMain;

IMPORT brk := Breaks;
IMPORT eve := Events;
IMPORT exp := Expr;
IMPORT stk := CallStk;

IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;

IMPORT kt  := KrnTypes;
IMPORT kmem:= Krn_Mem;
IMPORT kex := KrnExec;

IMPORT pt  := PckTypes;
IMPORT bas := PckBase;

IMPORT lst := Lists;
IMPORT xcp := XCPT_Msg;

<* IF DEST_K26 THEN *>

IMPORT mdl := Model;

<* END *>


(* Проверка, нужен ли режим Step *)
PROCEDURE NeedToStepMode (): BOOLEAN;
BEGIN
  RETURN bas.CheckMode(pt.Trace) OR brk.NeedToStepMode();
END NeedToStepMode;



(* Реакция на исключительную ситуацию в программе *)
PROCEDURE ExceptionReaction (data: erc.DATA); FORWARD;


PROCEDURE SetDefaultExceptionReaction;
BEGIN
  IF erc.FindReaction (erc.FirstReaction (eve.Exception), ExceptionReaction) = NIL THEN
    erc.AddAction (eve.Exception, NIL, ExceptionReaction);
  END;
END SetDefaultExceptionReaction;


(* Удаление останова по номеру *)
PROCEDURE DelBreak (N: CARDINAL) : BOOLEAN;
VAR
  React   : erc.PREACTION;
  BreakPos: CARDINAL;
  Event   : eve.EVENT_TYPE;
  name    : xs.String;
BEGIN
  IF FindBreak (N, Event, React, BreakPos) THEN
    ASSERT(lst.GetBreakName (name, N));
    lst.DelBreak (name);
    CASE Event OF
    | eve.Exception:
      erc.RemAction (eve.Exception, React);
      SetDefaultExceptionReaction;
    | eve.Call, eve.Return, eve.CompCreated, eve.CompDestroyed:
      erc.RemAction (Event, React);
    | eve.SingleStep:
      ASSERT (brk.Delete_ConditionBreak(BreakPos));
    | eve.BreakpointHit:
      ASSERT (brk.Delete_Breakpoint(BreakPos));
    | eve.MemoryAccess:
      ASSERT (brk.Delete_AccessBreak(BreakPos));
   <* IF DEST_K26 THEN *>
    | eve.RegisterAccess:
      ASSERT (brk.Delete_AccessBreak(BreakPos));
    | eve.DeviceAccess:
      ASSERT (brk.Delete_AccessBreak(BreakPos));
   <* END *>
    END;
    WHILE DelBreak (N) DO
      -- все такие точки нужно удалить
    END;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END DelBreak;


(* Поиск реакции по номеру *)
PROCEDURE FindBreak (        N   : CARDINAL;
                     VAR Event   : eve.EVENT_TYPE;
                     VAR React   : erc.PREACTION;
                     VAR BreakPos: CARDINAL): BOOLEAN;
VAR
  pexcept: PEXCEPTION;
  pcall  : PCALL;
  preturn: PRETURN;
  pcomp  : PCOMP;
  i      : CARDINAL;
BEGIN
  Event := eve.Exception;
  React := erc.FirstReaction (Event);
  LOOP
    IF React = NIL THEN EXIT; END;
    WITH React^ DO
      pexcept := PEXCEPTION(data);
      IF pexcept # NIL THEN
        IF (do = ExceptionReaction) AND (pexcept^ .Description.Number = N) THEN RETURN TRUE; END;
      END;
    END;
    React := React^.next;
  END;

  Event := eve.Call;
  React := erc.FirstReaction (Event);
  LOOP
    IF React = NIL THEN EXIT; END;
    WITH React^ DO
      pcall := PCALL(data);
      IF pcall # NIL THEN
        IF (do = Call) AND (pcall^.Description.Number = N) THEN RETURN TRUE; END;
      END;
    END;
    React := React^.next;
  END;

  Event := eve.Return;
  React := erc.FirstReaction (Event);
  LOOP
    IF React = NIL THEN EXIT; END;
    WITH React^ DO
      preturn := PRETURN(data);
      IF preturn # NIL THEN
        IF (do = Return) AND (preturn^.Description.Number = N) THEN RETURN TRUE; END;
      END;
    END;
    React := React^.next;
  END;

  Event := eve.CompCreated;
  React := erc.FirstReaction (Event);
  LOOP
    IF React = NIL THEN EXIT; END;
    WITH React^ DO
      pcomp := PCOMP(data);
      IF pcomp # NIL THEN
        IF (do = CompProcessing) AND (pcomp^.Description.Number = N) THEN RETURN TRUE; END;
      END;
    END;
    React := React^.next;
  END;

  Event := eve.CompDestroyed;
  React := erc.FirstReaction (Event);
  LOOP
    IF React = NIL THEN EXIT; END;
    WITH React^ DO
      pcomp := PCOMP(data);
      IF pcomp # NIL THEN
        IF (do = CompProcessing) AND (pcomp^.Description.Number = N) THEN RETURN TRUE; END;
      END;
    END;
    React := React^.next;
  END;

  Event := eve.BreakpointHit;
  WITH brk.Breakpoints DO
    FOR i := 1 TO free DO
      WITH Breakpoints^[i-1].Break DO
        IF (Owner = brk.Paket) AND (BreakDesc.Number = N) THEN
          BreakPos := i-1;
          RETURN TRUE;
        END;
      END;
    END;
  END;

  Event := eve.SingleStep;
  WITH brk.ConditionBreaks DO
    FOR i := 1 TO free DO
      WITH ConditionBreaks^[i-1].Break DO
        IF (Owner = brk.Paket) AND (BreakDesc.Number = N) THEN
          BreakPos := i-1;
          RETURN TRUE;
        END;
      END;
    END;
  END;

  WITH brk.AccessBreaks DO
    FOR i := 1 TO free DO
      WITH AccessBreaks^[i-1] DO
        WITH Break DO
          IF (Owner = brk.Paket) AND (BreakDesc.Number = N) THEN
            CASE Access_Data.Access_ID OF
            | brk.Memory  : Event := eve.MemoryAccess;

<* IF DEST_K26 THEN *>
            | brk.Register: Event := eve.RegisterAccess;
            | brk.Port    : Event := eve.MemoryAccess;
<* END *>
            END;
            BreakPos := i-1;
            RETURN TRUE;
          END;
        END;
      END;
    END;
  END;

  RETURN FALSE;
END FindBreak;



VAR
  (* Предназначены для отображения трассы программы *)
  CurrentCom : CARDINAL;
  CurrentMod : CARDINAL;
  CurrentLine: CARDINAL;

(* Печать трассы исполнения программы *)
PROCEDURE PrintTraceExecuting (data: erc.DATA);
VAR
  mssg, buf: xs.String;
  com_name : xs.String;
  mod_ptr: xs.txt_ptr;
  com_ptr: xs.txt_ptr;
  com : CARDINAL;
  mod : CARDINAL;
  line: CARDINAL;
  len : CARDINAL;
BEGIN
  IF NOT opt.in_dialog AND bas.CheckMode(pt.Trace) THEN
    ASSERT(data = erc.DATA_NIL);
    ASSERT(eve.LastEvent.Event = eve.SingleStep);
    CASE pt.TraceMode OF
    | pt.Code :
      mssg[0] := 0C;
      IF NOT exe.DisasmInstr (eve.LastEvent.IP, TRUE, mssg, buf, len) THEN COPY('?????', mssg); END;
      pro.WriteMsgNo(msg.Desassembler_command, pro.to_screen, pro.to_file, eve.LastEvent.IP, mssg);

    | pt.Source, pt.Mix:
      IF tls.SourceByAddr (eve.LastEvent.IP, com, mod, line) THEN
        IF com # CurrentCom THEN
          ASSERT( tls.ComName (com, com_ptr) );
          COPY(com_ptr^, com_name);
          com_name[LENGTH(com_name)+1] := 0C;
          com_name[LENGTH(com_name)] := '.';
          CurrentCom := com;
          CurrentMod := MAX(CARDINAL);
          CurrentLine := MAX(CARDINAL);
        ELSE
          com_name := '';
        END;
        IF mod # CurrentMod THEN
          ASSERT( tls.ModName (com, mod, mod_ptr) );
          COPY(mod_ptr^, mssg);
          fmt.print(buf, "     │----- %s%s", com_name, mssg);
          pro.WriteMsg(buf, pro.to_screen, pro.to_file);
          CurrentMod := mod;
          CurrentLine := MAX(CARDINAL);
        END;
        IF line # CurrentLine THEN
          mod_ptr := tls.GetSourceLine (com, mod, line-1);
          COPY(mod_ptr^, mssg);
          fmt.print(buf, "     │%5u   %s", line, mssg);
          pro.WriteMsg(buf, pro.to_screen, pro.to_file);
          CurrentLine := line;
        END;
        IF pt.TraceMode = pt.Mix THEN
          mssg[0] := 0C;
          IF NOT exe.DisasmInstr(eve.LastEvent.IP, TRUE, mssg, buf, len) THEN COPY('?????', mssg); END;
          pro.WriteMsgNo(msg.Desassembler_command, pro.to_screen, pro.to_file, eve.LastEvent.IP, mssg);
        END;
      END;
    END;
  END;
END PrintTraceExecuting;


(* Стандартные действия при переходе в пакет *)
PROCEDURE BreakProgram (Desc: pt.BREAK_DESCRIPTION; print: BOOLEAN; text-: ARRAY OF CHAR);
VAR
  com: dt.ComNo;
  mod: dt.ModNo;
  BreakName: xs.String;
BEGIN
  -- Остановить программу и обработку реакций
  erc.StopReact;
  exe.StopExec;

  -- Сбросить стек
  stk.ResetCallStack;

  -- Перейти в пакет на метку
  pt.CurrPaket := Desc.Paket_No;
  pt.Pakets[pt.CurrPaket].LineNum := Desc.Line_No;

  -- Установить текущие компоненту и модуль
  IF tls.FindComponentByAddr (eve.LastEvent.IP, com) THEN
    pt.ActiveComponent := com;
    IF tls.FindModInCompByAddr (com, eve.LastEvent.IP, mod) THEN
      pt.ActiveModule := mod;
    END;
  END;

  -- Если печать определена
  IF print THEN
    ASSERT(lst.GetBreakName (BreakName, Desc.Number));
    pro.WriteMsgNo (msg.Break, pro.to_screen, pro.to_file, BreakName, eve.LastEvent.IP, text);
  END;
END BreakProgram;


(* Поиск реакции на исключительную ситуацию в программе *)
(* идентификатору исключительной ситуации               *)
PROCEDURE FindException (Exception_ID: eve.EXCEPTION_ID): BOOLEAN;
VAR
  preact: erc.PREACTION;
  pdata : PEXCEPTION;
BEGIN
  preact := erc.FirstReaction (eve.Exception);
  LOOP
    IF preact = NIL THEN EXIT; END;
    WITH preact^ DO
      pdata := PEXCEPTION(data);
      IF pdata # NIL THEN
        IF pdata^.Exception_ID = Exception_ID THEN RETURN TRUE; END;
      END;
    END;
    preact := preact^.next;
  END;
  RETURN FALSE;
END FindException;


(* Реакция на исключительную ситуацию в программе *)
PROCEDURE ExceptionReaction (data: erc.DATA);
VAR
  pdata: PEXCEPTION;
  text : xs.String;
  m    : xs.String;
  xcpt : xs.String;
  print: BOOLEAN;
  paket: CARDINAL;
  line : CARDINAL;

BEGIN
  IF NOT opt.in_dialog THEN
    ASSERT(eve.LastEvent.Event = eve.Exception);
    IF eve.QueryEvent (eve.SingleStep) THEN
      ASSERT(eve.AddEvent(eve.LastEvent));
      erc.CancelReact;
      RETURN;
    END;
    pdata := PEXCEPTION(data);
    IF pdata # NIL THEN
      IF pdata^.Exception_ID # eve.LastEvent.Exception_ID THEN RETURN; END;
    END;
    print := FALSE;
    text  := '';
    CASE eve.LastEvent.Exception_ID OF
    | eve.OutOfMemory     : (* Доступ по адресу вне диапазона адресов *)
      IF bas.CheckMode(pt.OutMem) OR (pdata = NIL) THEN
        print := TRUE;
        pro.GetMsg(msg.BreakOutMem, m);
        fmt.print(text, m, CARDINAL(eve.LastEvent.XCPT_INFO_1));
      END;
    | eve.ProgramException: (* Программное прерывание                 *)
      IF bas.CheckMode(pt.ProgInt) OR (pdata = NIL) THEN
        print := TRUE;
        WITH eve.LastEvent DO
          IF CARDINAL(XCPT_INFO_1) = 0 THEN
            IF exe.ProgramSkipToMainEntry THEN
              pro.GetMsg (msg.ExceptionAtStartup, m);
            ELSE
              pro.GetMsg (msg.ProgramFinished, m);
            END;
            fmt.print (text, m);
          ELSE
            pro.GetMsg(msg.BreakProgInt, m);
            xcp.Get_XCPT_Msg(CARDINAL(XCPT_INFO_1), CARDINAL(XCPT_INFO_4), xcpt);
            fmt.print (text, m, xcpt, CARDINAL(XCPT_INFO_2));
          END;
        END;
      END;
    | eve.WriteProtected  : (* Запись в защищенную область памяти     *)
      IF bas.CheckMode(pt.WrProt) OR (pdata = NIL) THEN
        print := TRUE;
        pro.GetMsg(msg.BreakWrProt, m);
        xcp.Get_XCPT_Msg(CARDINAL(eve.LastEvent.XCPT_INFO_4), 0, xcpt);
        fmt.print(text, m, xcpt, CARDINAL(eve.LastEvent.XCPT_INFO_1));
      END;
    | eve.UserException   : (* Исполнение прервано пользователем      *)
      IF bas.CheckMode(pt.User) OR (pdata = NIL) THEN
        pro.GetMsg(msg.User_React_Default,text);
      END;
    END;
    IF pdata = NIL THEN
      pro.WriteMsgNo (msg.Last_address, pro.to_screen, pro.to_file, eve.LastEvent.IP, text);
      IF bas.CheckMode (pt.GoSuccess) THEN
        WHILE lst.IsGOSUBCall() DO
          lst.PopCall(paket,line);
        END;
        lst.PopCall(paket, line);
        pt.CurrPaket := paket;
        pt.Pakets[pt.CurrPaket].LineNum := line;
        bas.ModeOff (pt.GoSuccess);
      END;
      bas.ModeOff (pt.RestartDone);
      exe.StopExec;
      kex.ProgramContextOk := FALSE;
      exe.StopQueueProc;
      eve.ClearQueue;
      erc.StopReact;
      erc.CancelReact;
    ELSE
      BreakProgram (pdata^.Description, print, text);
    END;
  END;
END ExceptionReaction;


(* Реакция на исключительную ситуацию в программе *)
PROCEDURE SetExceptionReaction (data: erc.DATA);
VAR
  preact, tmp: erc.PREACTION;
BEGIN
  preact := erc.FirstReaction (eve.Exception);
  LOOP
    IF preact = NIL THEN EXIT; END;
    WITH preact^ DO
      tmp := next;
      IF (do = ExceptionReaction) AND (data = erc.DATA_NIL) THEN
        erc.RemAction(eve.Exception, preact);
      END;
    END;
    preact := tmp;
  END;
  erc.AddAction (eve.Exception, data, ExceptionReaction);
  erc.AddAction (eve.Exception, NIL, ExceptionReaction);
END SetExceptionReaction;


(* Реакция на исполнение команды Call *)
PROCEDURE Call (data: erc.DATA);
VAR
  pcall: PCALL;
  m    ,
  text : xs.String;
  print: BOOLEAN;
  com  : CARDINAL;
  mod  : CARDINAL;
  obj  : dt.OBJECT;
  name : xs.String;
BEGIN
  IF NOT opt.in_dialog THEN
    IF eve.QueryEvent (eve.SingleStep) THEN
      ASSERT(eve.AddEvent(eve.LastEvent));
      erc.CancelReact;
      RETURN;
    END;
    pcall := PCALL(data);
    ASSERT(pcall#NIL);
    IF (pcall^.Exactly) THEN
      IF pcall^.Address # eve.LastEvent.CallAddr THEN RETURN; END;
    END;
    text := '';
    print := bas.CheckMode(pt.Call);
    IF print THEN
      name := '';
      IF tls.FindModByAddr (eve.LastEvent.CallAddr, com, mod) THEN
        obj := tls.FindProcByAddr (com, mod, eve.LastEvent.CallAddr);
        IF tls.IsObjectValid(obj) THEN
          tls.ObjectName (obj, name);
        END;  
      END;  
      pro.GetMsg(msg.BreakCallByAddr, m);
      fmt.print(text, m, name, eve.LastEvent.CallAddr);
    END;
    BreakProgram(pcall^.Description, print, text);
  END;
END Call;


(* Реакция на исполнение команды Ret *)
PROCEDURE Return (data: erc.DATA);
VAR
  pret : PRETURN;
  text ,
  m    : xs.String;
  print: BOOLEAN;
  com  : CARDINAL;
  mod  : CARDINAL;
  obj  : dt.OBJECT;
  name : xs.String;
BEGIN
  IF NOT opt.in_dialog THEN
    IF eve.QueryEvent (eve.SingleStep) THEN
      ASSERT(eve.AddEvent(eve.LastEvent));
      erc.CancelReact;
      RETURN;
    END;
    pret := PRETURN(data);
    ASSERT(pret#NIL);
    CASE eve.LastEvent.Event OF
    | eve.Call:
      INC(pret^.Count);
      RETURN;
    | eve.Return:
      IF pret^.Count # 0 THEN
        DEC(pret^.Count);
        RETURN;
      END;  
    ELSE
      ASSERT(FALSE);
    END;  
    text  := '';
    print := bas.CheckMode(pt.Ret);
    IF print THEN
      name := '';
      IF tls.FindModByAddr (eve.LastEvent.IP, com, mod) THEN
        obj := tls.FindProcByAddr (com, mod, eve.LastEvent.IP);
        IF tls.IsObjectValid(obj) THEN
          tls.ObjectName (obj, name);
        END;
      END;   
      pro.GetMsg(msg.BreakRet, m);
      fmt.print(text, m, name, eve.LastEvent.ReturnAddr);
    END;
    BreakProgram(pret^.Description, print, text);
    ASSERT(DelBreak(pret^.Description.Number));
  END;
END Return;

VAR  pcomp: PCOMP;

PROCEDURE CompProcessing (data: erc.DATA);
VAR
  m    : xs.String;
  text : xs.String;
  name : xs.String;
  tmp  : xs.txt_ptr;
  print: BOOLEAN;
  com  : dt.ComNo;
BEGIN
  IF NOT opt.in_dialog THEN
    IF eve.QueryEvent (eve.SingleStep) THEN
      ASSERT(eve.AddEvent(eve.LastEvent));
      erc.CancelReact;
      RETURN;
    END;
    pcomp := PCOMP(data);
    ASSERT(pcomp#NIL);
    CASE pcomp^.Action OF
    | pt.CompCreated:
      IF NOT eve.LastEvent.Stopable THEN RETURN; END;
      COPY(eve.LastEvent.Component.short_name, name);
      IF pcomp^.CompName # "" THEN
        xs.Uppercase (name);
        IF pcomp^.CompName # name THEN -- <name>.dll ?
          fil.RemoveExtension(name);
          IF pcomp^.CompName # name THEN -- <name> ?
            xs.Append ("$", name);
            IF pcomp^.CompName # name THEN -- <name>$ ?
              RETURN;
            END;
          END;
        END;
      END;
    | pt.CompDestroyed:
      IF tls.FindComponentByHandle (eve.LastEvent.Handle, com) THEN
        ASSERT(tls.ComName (com, tmp));
        COPY(tmp^, name);
        IF pcomp^.CompName # "" THEN
          IF pcomp^.CompName # tmp^ THEN -- <name>$ ?
            name[LENGTH(name)-1] := 0C;;
            IF pcomp^.CompName # name THEN -- <name> ?
              ASSERT(tls.ComFullName (com, tmp));
              fil.ExtractFileName (tmp^, name);
              IF pcomp^.CompName # name THEN -- <name>.dll ?
                RETURN;
              END;
            END;
          END;
        END;
      ELSIF pcomp^.CompName = "" THEN
        COPY("<unknown>", name);
      ELSE
        RETURN;
      END;
    END;
    text := '';
    print := bas.CheckMode (pcomp^.Action);
    IF print THEN
      CASE pcomp^.Action OF
      | pt.CompCreated:
        pro.GetMsg (msg.BreakCompCreated, m);
      | pt.CompDestroyed:
        pro.GetMsg (msg.BreakCompDestroyed, m);
      END;
      fmt.print (text, m, name);
    END;
    BreakProgram(pcomp^.Description, print, text);
  END;
END CompProcessing;


PROCEDURE CheckBreakInCompDestroyed (data: erc.DATA);
VAR
  i   : CARDINAL;
  com1: dt.ComNo;
  com2: dt.ComNo;
  name: xs.txt_ptr;
  tmp : xs.String;
  need: BOOLEAN;
BEGIN
  IF opt.in_dialog THEN RETURN; END;
  ASSERT(eve.LastEvent.Event = eve.CompDestroyed);
  ASSERT(data=erc.DATA(NIL));
  IF tls.FindComponentByHandle (eve.LastEvent.Handle, com1) THEN
    ASSERT(tls.ComName (com1, name));
    COPY(name^, tmp);
    WITH brk.Breakpoints DO
      IF free # 0 THEN
        need := TRUE;
        FOR i := 0 TO free-1 DO
          WITH Breakpoints^[i] DO
            IF Break.Active AND tls.FindComponentByAddr (Addr, com2) AND (com1 = com2) THEN
              IF need THEN
                pro.WriteMsgNo (msg.CompUnload_BreakDisasbled, pro.to_screen, pro.to_file, tmp);
                need := FALSE;
              END;
              Break.Active := FALSE;
            END;
          END;
        END;
      END;
    END;
  END;
END CheckBreakInCompDestroyed;


<* PUSH *>
<* WOFF301+ *>

(* Реакция на исключительную ситуацию в отладчике *)
PROCEDURE InternalError (data: erc.DATA);
BEGIN
  ASSERT(eve.LastEvent.Event = eve.InternalError);
  ASSERT(FALSE, eve.LastEvent.ErrorNo);
END InternalError;


(* Условный останов *)
PROCEDURE CheckCondition (data: erc.DATA);
VAR
  text    ,
  m       : xs.String;
  print   : BOOLEAN;
  result  : BOOLEAN;
  BreakPos: CARDINAL;
  com     : CARDINAL;
  mod     : CARDINAL;
BEGIN
  IF NOT opt.in_dialog THEN
    ASSERT(eve.LastEvent.Event = eve.SingleStep);
    BreakPos := 0;
    IF NOT tls.FindModByAddr (eve.LastEvent.IP, com, mod) THEN
      com := 0;
      mod := 0;
    END;  
    WHILE brk.NeedToStop (com, mod, BreakPos) DO
      WITH brk.ConditionBreaks.ConditionBreaks^[BreakPos] DO
        WITH Break DO
          IF (Owner=brk.Paket) AND Active THEN
            IF exp.GetRelation (com, mod, Expr^, result) AND result THEN
              (* Результат получен и выражение истинно *)
              IF Pass = 0 THEN
                text  := '';
                print := bas.CheckMode(pt.If);
                IF print THEN
                  pro.GetMsg(msg.BreakIf, m);
                  fmt.print(text, m, Expr^);
                END;
                BreakProgram(BreakDesc, print, text);
                IF NOT Sticky THEN
                  ASSERT(brk.Delete_Breakpoint(BreakPos));
                  ASSERT(DelBreak(BreakDesc.Number));
                ELSE
                  INC(BreakPos);
                END;
              ELSE
                DEC(Pass);
              END;
            END;
          END;
        END;
      END;
    END;
  END;
END CheckCondition;



(* Останов по адресу *)
PROCEDURE ProcessingBreakpoints (data: erc.DATA);
VAR
  text, m   : xs.String;
  print     : BOOLEAN;
  result    : BOOLEAN;
  BreakPos  : CARDINAL;
  NeedToStop: BOOLEAN;
BEGIN
  IF NOT opt.in_dialog THEN
    ASSERT(eve.LastEvent.Event = eve.BreakpointHit);
    IF eve.QueryEvent (eve.SingleStep) THEN
      ASSERT(eve.AddEvent(eve.LastEvent));
      erc.CancelReact;
      RETURN;
    END;
    IF brk.FindByIndex_Breakpoint(eve.LastEvent.BreakpointInd, BreakPos) THEN
      WITH brk.Breakpoints.Breakpoints^[BreakPos] DO
        ASSERT(eve.LastEvent.IP=Addr);
        WITH Break DO
          IF (Owner=brk.Paket) AND Active THEN
            CASE Kind OF
            | brk.watchpoint:
              -- nothing to do
            | brk.counter:
              INC(Pass);
            | brk.normal:
              IF Pass > 0 THEN
                DEC(Pass);
              ELSE
                IF Condition = NIL THEN
                  NeedToStop := TRUE;
                ELSE
                  NeedToStop := exp.GetRelation (Pos.ComN, Pos.ModN, Condition^, result) AND result;
                END;
                IF NeedToStop THEN
                  IF bas.CheckMode(pt.Address) THEN
                    print := TRUE;
                    pro.GetMsg(msg.BreakAddress, m);
                    fmt.print(text, m);
                  ELSE
                    print := FALSE;
                    text  := '';
                  END;
                  BreakProgram (BreakDesc, print, text);
                  IF NOT Sticky THEN
                    ASSERT(brk.Delete_Breakpoint(BreakPos));
                    ASSERT(DelBreak(BreakDesc.Number));
                  END;
                END;
              END;
            END;
          END;
        END;
      END;
    ELSIF eve.LastEvent.BreakpointInd = exe.TmpBreakpointIndex THEN
      exe.RemoveTmpBreakPoint();
      exe.StopExec;
    END;  
  END;
END ProcessingBreakpoints;


(* Останов по доступу к памяти *)
PROCEDURE MemoryAccess (data: erc.DATA);
VAR
  text, m   : xs.String;
  print     : BOOLEAN;
  BreakPos  : CARDINAL;
  was, is   : xs.String;
  i, byte   : CARDINAL;
  s         : ARRAY [0..2] OF CHAR;
  NeedToStop: BOOLEAN;
  access_len: CARDINAL;
  access_loc: kt.ADDRESS;


BEGIN
  IF NOT opt.in_dialog THEN
    ASSERT(eve.LastEvent.Event = eve.MemoryAccess);
    IF eve.QueryEvent (eve.SingleStep) THEN
      ASSERT(eve.AddEvent(eve.LastEvent));
      erc.CancelReact;
      RETURN;
    END;
    ASSERT(brk.FindByIndex_AccessBreak(eve.LastEvent.MemAccess_Ind, BreakPos));
    WITH brk.AccessBreaks.AccessBreaks^[BreakPos] DO
      WITH Access_Data DO
        ASSERT(Access_ID = brk.Memory);

<* IF DEST_K26 THEN *>

        WITH eve.LastEvent DO
          ASSERT(MemAccess_Type = Access_Type);
          ASSERT( ((Location<=MemAccess_Addr)&(MemAccess_Addr<=Location+Len-1)) OR
                  ((Location<=MemAccess_Addr+MemAccess_Len-1) AND
                   (MemAccess_Addr+MemAccess_Len-1<=Location+Len-1)));

          access_len := MemAccess_Len ;
          access_loc := MemAccess_Addr ;
        END;

<* ELSE *>

        access_len := Len ;
        access_loc := Location ;

<* IF env_target = 'x86os2' THEN *>

        ASSERT( kmem.SetTrace(Access_Type, Location, Len, Break.Index) );

<* END *>

<* END *>

        WITH Break DO
          IF (Owner=brk.Paket) AND Active THEN
            IF Pass > 0 THEN
              DEC(Pass);
              NeedToStop := FALSE;
            ELSE
              NeedToStop := TRUE;
            END;
            IF NeedToStop THEN
              text  := '';
              CASE Access_Type OF
              | eve.Read:
                  print := bas.CheckMode(pt.Read);
                  IF print THEN
                    is  := '';
                    FOR i := access_len-1 TO 0 BY -1 DO
                      byte := 0;
                      ASSERT(mem.Get(access_loc+i, sys.ADR(byte), 1));
                      fmt.print(s, "%$2X", byte);
                      xs.Append(s, is);
                    END;
                    pro.GetMsg(msg.BreakRead, m);
                    fmt.print(text, m, access_loc, access_len, is);
                  END;

              | eve.Write:
                  print := bas.CheckMode(pt.Write);
                  IF print THEN
<* WOFF304+ *>
                    was := '';
                    is  := '';
                    IF Len <= brk.PREV_VALUE_LENGTH THEN
                      FOR i := Len-1 TO 0 BY -1 DO
                        byte := VAL(CARDINAL, Prev_value[i]);
                        fmt.print(s, "%$2X", byte);
                        xs.Append(s, was);
                        ASSERT(mem.Get(access_loc+i, sys.ADR(Prev_value[i]), 1));
                        byte := VAL(CARDINAL, Prev_value[i]);
                        fmt.print(s, "%$2X", byte);
                        xs.Append(s, is);
                      END;
                    ELSE
                      FOR i := Len-1 TO 0 BY -1 DO
                        byte := VAL(CARDINAL, Prev_value_long^[i]);
                        fmt.print(s, "%$2X", byte);
                        xs.Append(s, was);
                        ASSERT(mem.Get(access_loc+i, sys.ADR(Prev_value_long^[i]), 1));
                        byte := VAL(CARDINAL, Prev_value_long^[i]);
                        fmt.print(s, "%$2X", byte);
                        xs.Append(s, is);
                      END;
                    END;
<* WOFF304- *>
                    pro.GetMsg(msg.BreakWrite, m);
                    fmt.print(text, m, access_loc, access_loc+access_len-1,
                                      access_loc, access_len, was, is);
                  END;
              END;
              BreakProgram(BreakDesc, print, text);
              IF NOT Sticky THEN
                ASSERT(brk.Delete_AccessBreak(BreakPos));
                ASSERT(DelBreak(BreakDesc.Number));
              END;
            END;
          END;
        END;
    END;
    END;
  END;
END MemoryAccess;


(* Поиск реакции на доступ *)
PROCEDURE FindAccess (Access: brk.ACCESS_BREAK): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  WITH brk.AccessBreaks DO
    FOR i := 1 TO free DO
      WITH AccessBreaks^[i-1] DO
        IF Access_Type = Access.Access_Type THEN
          WITH Access_Data DO
            IF Access_ID = Access.Access_Data.Access_ID THEN
              CASE Access_ID OF
              | brk.Memory  : IF (Location = Access.Access_Data.Location) AND
                                 (Len = Access.Access_Data.Len) THEN RETURN TRUE; END;
<* IF DEST_K26 THEN *>
              | brk.Register: IF Reg_No = Access.Access_Data.Reg_No THEN RETURN TRUE; END;
              | brk.Port    : IF (Dev_No = Access.Access_Data.Dev_No) AND
                                 (Port_No = Access.Access_Data.Port_No) THEN RETURN TRUE; END;
<* END *>
              END;
            END;
          END;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END FindAccess;



<* IF DEST_K26 THEN *>

(* Останов по доступу к регистрам *)
PROCEDURE RegisterAccess (data: erc.DATA);
VAR
  text, m   : xs.String;
  print     : BOOLEAN;
  BreakPos  : CARDINAL;
  was, is   : xs.String;
  i         : CARDINAL;
  pbyte     : POINTER TO sys.CARD8;
  s         : ARRAY [0..2] OF CHAR;
  RegValue  : kt.REG_VALUE;
  RegName   : xs.String;
  NeedToStop: BOOLEAN;
BEGIN
  IF NOT opt.in_dialog THEN
    ASSERT(eve.LastEvent.Event = eve.RegisterAccess);
    IF eve.QueryEvent (eve.SingleStep) THEN
      ASSERT(eve.AddEvent(eve.LastEvent));
      erc.CancelReact;
      RETURN;
    END;
    ASSERT(brk.FindByIndex_AccessBreak(eve.LastEvent.RegAccess_Ind, BreakPos));
    WITH brk.AccessBreaks.AccessBreaks^[BreakPos] DO
      WITH Access_Data DO
        ASSERT(Access_ID = brk.Register);
        WITH eve.LastEvent DO
          ASSERT(RegAccess_Type = Access_Type);
          ASSERT(RegAccess_RegNo = Reg_No);
          WITH Break DO
            IF (Owner=brk.Paket) AND Active THEN
              IF Pass > 0 THEN
                DEC(Pass);
                NeedToStop := FALSE;
              ELSE
                NeedToStop := TRUE;
              END;
              IF NeedToStop THEN
                text  := '';
                CASE RegAccess_Type OF
                | eve.Read  :
                    print := bas.CheckMode(pt.Read);
                    IF print THEN
                      ASSERT(mem.GetReg(Reg_No, RegValue));
                      is  := '';
                      FOR i := SIZE(kt.REG_VALUE)-1 TO 0 BY -1 DO
                        pbyte := sys.ADDADR(sys.ADR(RegValue), i);
                        fmt.print(s, "%$2X", pbyte^);
                        xs.Append(s, is);
                      END;
                      pro.GetMsg(msg.BreakReadReg, m);
                      ASSERT(exe.GetRegName(RegAccess_RegNo, RegName));
                      fmt.print(text, m, RegName, RegAccess_Len, is);
                    END;
                | eve.Write :
                    print := bas.CheckMode(pt.Write);
<* WOFF304+ *>
                    IF print THEN
                      ASSERT(mem.GetReg(Reg_No, RegValue));
                      was := '';
                      is  := '';
                      FOR i := SIZE(kt.REG_VALUE)-1 TO 0 BY -1 DO
                        pbyte := sys.ADDADR(sys.ADR(Prev_Reg_value), i);
                        fmt.print(s, "%$2X", pbyte^);
                        xs.Append(s, was);
                        pbyte := sys.ADDADR(sys.ADR(RegValue), i);
                        fmt.print(s, "%$2X", pbyte^);
                        xs.Append(s, is);
                      END;
                      sys.MOVE(sys.ADR(RegValue), sys.ADR(Prev_Reg_value), SIZE(kt.REG_VALUE));
<* WOFF304- *>
                      pro.GetMsg(msg.BreakWriteReg, m);
                      ASSERT(exe.GetRegName(RegAccess_RegNo, RegName));
                      fmt.print(text, m, RegName, RegAccess_Len, was, is);
                    END;
                END;
                BreakProgram(BreakDesc, print, text);
                IF NOT Sticky THEN
                  ASSERT(brk.Delete_AccessBreak(BreakPos));
                  ASSERT(DelBreak(BreakDesc.Number));
                END;
              END;
            END;
          END;
        END;
      END;
    END;
  END;
END RegisterAccess;


(* Реакция на обращение к порту *)
PROCEDURE DeviceAccess (data: erc.DATA);
VAR
  text    : xs.String;
  m       : xs.String;
  Dev_Name: xs.String;
  Reg_Name: xs.String;
  Action  : xs.String;
  print   : BOOLEAN;
  BreakPos: CARDINAL;
  Address : kt.ADDRESS;

BEGIN
  IF NOT opt.in_dialog THEN
    ASSERT(eve.LastEvent.Event = eve.DeviceAccess);
    IF eve.QueryEvent (eve.SingleStep) THEN
      ASSERT(eve.AddEvent(eve.LastEvent));
      erc.CancelReact;
      RETURN;
    END;
    print := bas.CheckMode(pt.IO);
    WITH brk.AccessBreaks DO
      IF free = 0 THEN RETURN; END;
      BreakPos := 0;
      WHILE BreakPos < free DO
        WITH AccessBreaks^[BreakPos] DO
          WITH Break DO
            IF (Owner=brk.Paket) AND Active THEN
              WITH eve.LastEvent DO
                WITH Access_Data DO
                  IF (Access_ID=brk.Port) AND (DevAccess_DevNo = Dev_No) THEN
                    ASSERT(Access_Type = eve.ReadWrite);
                    IF (DevAccess_RegNo = Port_No) OR (Port_No = MAX(CARDINAL)) THEN
                      IF Pass = 0 THEN
                        text := '';
                        IF print THEN
                          mdl.DeviceName(DevAccess_DevNo, Dev_Name);
                          mdl.RegisterName(DevAccess_DevNo, DevAccess_RegNo, Reg_Name);
                          mdl.RegisterAddress(DevAccess_DevNo, DevAccess_RegNo, Address);
                          pro.GetMsg(msg.BreakIO, m);
                          IF DevAccess_Type = eve.Read THEN
                            pro.GetMsg(msg.IO_Read, Action);
                          ELSE
                            pro.GetMsg(msg.IO_Write, Action);
                          END;
                          fmt.print(text, m, Dev_Name, Reg_Name, Action, Address, DevAccess_Len);
                        END;
                        BreakProgram(BreakDesc, print, text);
                        IF NOT Sticky THEN
                          ASSERT(brk.Delete_Breakpoint(BreakPos));
                          ASSERT(DelBreak(BreakDesc.Number));
                        END;
                      ELSE
                        DEC(Pass);
                      END;
                    END;
                  END;
                END;
              END;
            END;
          END;
        END;
        INC(BreakPos);
      END;
    END;
  END;
END DeviceAccess;

<* END *>

<* POP *> -- PUSH WOFF301+


BEGIN
  erc.Ini ();

  CurrentCom  := MAX(CARDINAL);
  CurrentMod  := MAX(CARDINAL);
  CurrentLine := MAX(CARDINAL);

  erc.AddAction (eve.InternalError,  NIL, InternalError);
  erc.AddActionSecond (eve.Exception,      NIL, ExceptionReaction);
  erc.AddActionSecond (eve.BreakpointHit,  NIL, ProcessingBreakpoints);
  erc.AddActionSecond (eve.SingleStep,     NIL, CheckCondition);
  erc.AddActionSecond (eve.MemoryAccess,   NIL, MemoryAccess);

  erc.InsActionFirst (eve.SingleStep,     NIL, PrintTraceExecuting);
  erc.InsActionFirst (eve.CompDestroyed,  NIL, CheckBreakInCompDestroyed);

<* IF DEST_K26 THEN *>
  erc.AddActionSecond (eve.RegisterAccess, NIL, RegisterAccess);
  erc.AddActionSecond (eve.DeviceAccess,   NIL, DeviceAccess);
<* END *>

END PckReact.

