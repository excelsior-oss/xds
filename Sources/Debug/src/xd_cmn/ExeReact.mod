<* Storage + *>

IMPLEMENTATION MODULE ExeReact;

IMPORT sys := SYSTEM;

IMPORT xs  := xStr;
IMPORT opt := Options;
IMPORT fil := File;

IMPORT dri := DI_Read;
IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;
IMPORT bld := DI_Build;

IMPORT eve := Events;
IMPORT mem := Exe_Mem;
IMPORT exe := ExeMain;

IMPORT kprg:= Krn_Prog;
IMPORT kt  := KrnTypes;
IMPORT ind := KrnIndex;
IMPORT kexe:= KrnExec;

IMPORT xcpt := XCPT_Msg;

IMPORT exp := Expr;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;


<* IF DEST_XDS AND (TARGET_OS = "WINNT") THEN *>

IMPORT win := Windows;

<* END *>


TYPE
  DATA = POINTER TO ARRAY OF sys.BYTE;



VAR
  (* Списки реакций для всех событий, возникающих при интерпретации *)
  ReactArr: AllReactions;

  stop_react: BOOLEAN;


(* Делает реакцию активной *)
PROCEDURE ReadyReaction (preact: PREACTION);
BEGIN
  ASSERT (preact # NIL);
  preact^.ready := TRUE;
END ReadyReaction;


(* Делает все реакции активными *)
PROCEDURE Ready;
VAR
  ev    : eve.EVENT_TYPE; 
  preact: PREACTION;
BEGIN
  FOR ev := MIN(eve.EVENT_TYPE) TO MAX(eve.EVENT_TYPE) DO
    IF ReactArr[ev] # NIL THEN
      preact := ReactArr[ev];
      REPEAT
        ReadyReaction (preact);
        preact := preact^.next;
      UNTIL preact = NIL;   
    END;
  END;
END Ready;


(* Возбуждает реакции, начиная с указанной *)
PROCEDURE Raise (preact: PREACTION);
BEGIN
  stop_react := FALSE;
  IF preact = NIL THEN RETURN; END;
  LOOP
    WITH preact^ DO
      NextReaction := next;
      IF ready THEN do(data); END;
    END;  
    IF stop_react THEN EXIT; END;
    preact := NextReaction;
    IF preact = NIL THEN EXIT; END;
  END;
END Raise;


(* Прекратить выполнение последующих реакций *)
PROCEDURE StopReact;
BEGIN
  stop_react := TRUE;
END StopReact;

PROCEDURE IsStopReact () : BOOLEAN;
BEGIN
  RETURN stop_react;
END IsStopReact;


PROCEDURE CancelReact;
(* Прекратить выполнение цепочки реакций на текущее событие *)
BEGIN
  NextReaction := NIL;
END CancelReact;



(* Процедуры для работы с реакциями на события *)


(* Добавляет еще одну реакцию на событие *)
PROCEDURE AddAction (ev: eve.EVENT_TYPE; data: DATA; do: DO_PROC);
VAR
  new, preact: PREACTION;
BEGIN
  NEW(new);
  ASSERT(new#NIL);
  new^.ready := FALSE;
  new^.do := do;
  new^.data := data;
  new^.next := NIL;
  new^.ready := FALSE;
  IF ReactArr[ev] = NIL THEN
    ReactArr[ev] := new;
  ELSE
    preact := ReactArr[ev];
    WHILE (preact^.next # NIL) DO preact := preact^.next; END;
    preact^.next := new;
  END;
END AddAction;

(* Вставляет новую реакцию newR перед уже имевшейся oldR *)
PROCEDURE InsAction(ev: eve.EVENT_TYPE; data: DATA; do: DO_PROC; old: PREACTION);
VAR
  new, act0, act1: PREACTION;
BEGIN
  NEW(new);
  ASSERT(new#NIL);
  new^.ready := FALSE;
  new^.do := do;
  new^.data := data;
  new^.ready := FALSE;
  act0 := ReactArr[ev];
  IF (act0 = NIL) OR (act0 = old) THEN
    ReactArr[ev] := new;
    new^.next := act0;
  ELSE
    LOOP
      act1 := act0^.next;
      IF (act1 = NIL) OR (act1 = old) THEN EXIT END;
      act0 := act1;
    END;
    act0^.next := new;
    new^.next := act1;
  END;
END InsAction;


(* Вставляет новую реакцию первой перед уже имеющимися *)
PROCEDURE InsActionFirst (ev: eve.EVENT_TYPE; data: DATA; do: DO_PROC);
BEGIN
   InsAction (ev, data, do, FirstReaction (ev));
END InsActionFirst;


(* Добавляет еще одну реакцию на событие, пропустив первую, если есть *)
PROCEDURE AddActionSecond (ev: eve.EVENT_TYPE; data: DATA; do: DO_PROC);
VAR
  reaction: PREACTION;
BEGIN
  reaction := FirstReaction (ev);
  IF (reaction # NIL) AND (reaction^.next # NIL) THEN
    InsAction (ev, data, do, reaction^.next);
  ELSE
    AddAction (ev, data, do);
  END;
END AddActionSecond;



(* Удаляет реакцию на событие *)
PROCEDURE RemAction(ev: eve.EVENT_TYPE; R: PREACTION);
VAR
 act0: PREACTION;
BEGIN
  ASSERT( R # NIL );
  IF ReactArr[ev] = R THEN
    ReactArr[ev] := R^.next;
  ELSE
    act0 := ReactArr[ev];
    LOOP
      IF (act0 = NIL) THEN EXIT END;
      IF (act0^.next = R) THEN act0^.next := R^.next; EXIT; END;
      act0 := act0^.next;
    END;
  END;
  DISPOSE(R);
END RemAction;


(* Первая реакция на событие *)
PROCEDURE FirstReaction (ev: eve.EVENT_TYPE): PREACTION;
BEGIN
  RETURN ReactArr[ev];
END FirstReaction;


(* Поиск реакции do начиная с first*)
PROCEDURE FindReaction (first: PREACTION; do: DO_PROC): PREACTION;
BEGIN
  LOOP
    IF first = NIL    THEN EXIT; END;
    IF first^.do = do THEN RETURN first; END;
    first := first^.next;
  END;
  RETURN NIL;
END FindReaction;



PROCEDURE DoAddress(data: DATA);
VAR
  i     : CARDINAL;
  event1: eve.EVENT;
  event2: eve.EVENT;
BEGIN
  ASSERT (data = NIL);
  WITH ind.Index DO
    IF eve.LastEvent.BreakpointInd = 0 THEN
      event1 := eve.LastEvent;
      IF exe.IsTmpBreakPoint(eve.LastEvent.IP) THEN
        event1.BreakpointInd := exe.TmpBreakpointIndex;
        ASSERT(mem.SetReg(kt.EIP, event1.IP));
      END;
      IF Free > 0 THEN
        event2 := event1;
        FOR i := 0 TO Free-1 DO
          WITH Index^[i] DO
            IF Busy AND (Access = ind.EXECUTE) AND (Addr = eve.LastEvent.IP) THEN
              IF event2.BreakpointInd = 0 THEN
                event2.BreakpointInd := i+1;
                ASSERT(mem.SetReg(kt.EIP, event2.IP));
                event1 := event2;
              ELSE
                event2.BreakpointInd := i+1;
                ASSERT(eve.AddEvent(event2));
              END;
            END;
          END;
        END;
      END;
      eve.LastEvent := event1;
    END;
  END;
  IF eve.LastEvent.BreakpointInd = 0 THEN
    event1 := eve.LastEvent;
    event1.Event := eve.Exception;
    event1.Exception_ID := eve.ProgramException;
    event1.XCPT_INFO_1 := CARDINAL(xcpt.UNKNOWN_BREAKPOINT);
    event1.XCPT_INFO_2 := event1.IP;
    event1.XCPT_INFO_3 := CARDINAL(1);
    ASSERT (eve.AddEvent(event1));
    CancelReact();
  ELSE
    exe.WasBreakpoint := eve.LastEvent.IP;
  END;
END DoAddress;


VAR
  LastCompName: exp.ExprRes;
  LastCompPath: exp.ExprRes;

PROCEDURE DoCompCreated (data: DATA);
VAR
  component: dt.COMPONENT;
  cname : xs.String;
  com: dt.ComNo;
  export: kt.EXPORTS;
  i     : CARDINAL;
  add   : BOOLEAN;
  public: dt.PUBLIC;

<* IF DEST_XDS AND (TARGET_OS = "WINNT") THEN *>
TYPE
  MAPSTRUCT = RECORD
                lpFile  : win.PVOID;
                hMapFile: win.HANDLE;
                raw     : dt.RAW_DEBUG_INFO;
              END;
VAR
  MapStruct: MAPSTRUCT;
<* END *>

BEGIN
  ASSERT(data = NIL);
  ASSERT(eve.LastEvent.Event = eve.CompCreated);
  IF eve.LastEvent.IP = kt.ADDRESS(NIL) THEN
    eve.LastEvent.IP := mem.GetIP();
  END;
  component:= dt.EmptyComponent;
  WITH component DO
    fil.ExtractFileName (eve.LastEvent.Component.short_name, cname);
    fil.RemoveExtension (cname);
    xs.Append ('$', cname);
    IsLoadTime:= NOT kexe.Loaded;
    EI := eve.LastEvent.Component;
    DI := dt.EmptyDebugInfo;
    raw := NIL;
  END;

  COPY (eve.LastEvent.Component.full_name, LastCompPath.string);
  COPY (cname, LastCompName.string);

  IF NOT kexe.Loaded THEN
    IF tls.FindComponentByName (cname, com) THEN
      RETURN;
    END;
  END;

  bld.AddComponent(component);
  WITH dt.Components DO
    WITH Components^[Count-1] DO
      Name:= bld.AddName( cname);
      IF dri.CheckDebugInfoVersion(EI) THEN
       <* IF DEST_XDS AND (TARGET_OS = "WINNT") THEN *>
        IF opt.RemoteMode THEN
          -- поскольку невозможно смапировать файл на удаленной машине
          NEW (raw, EI.DebugInfoSize);
          IF kprg.GetDebugInfo(EI, sys.ADR(raw^)) THEN
            sys.EVAL (dri.ProcessDebugInfo(Count-1, Components^[Count-1]));
          END;
          IF raw # NIL THEN
            DISPOSE (raw);
          END;
        ELSE
          raw := NIL;
          IF kprg.GetDebugInfo(EI, sys.ADR(MapStruct)) THEN
            raw:= MapStruct.raw;
            sys.EVAL (dri.ProcessDebugInfo(Count-1, Components^[Count-1]));
          END;
          IF raw # NIL THEN
            win.UnmapViewOfFile (MapStruct.lpFile);
            win.CloseHandle (MapStruct.hMapFile);
          END;
        END;
       <* ELSE *>
        NEW (raw, EI.DebugInfoSize);
        IF kprg.GetDebugInfo(EI, sys.ADR(raw^)) THEN
          sys.EVAL (dri.ProcessDebugInfo(Count-1, Components^[Count-1]));
        END;
        IF raw # NIL THEN
          DISPOSE (raw);
        END;
       <* END *>
        raw := NIL;
      ELSE
        IF kprg.ReadExport (EI.full_name, export) THEN
          FOR i := 0 TO HIGH(export^) DO
            WITH export^[i] DO
              add := (obj > 0) AND (obj <= EI.N_Objects) AND
                     (offset <= MAX(CARDINAL)-EI.Objects^[obj-1].Begin);
              <* IF TARGET_x86 THEN *>
              add := add AND (kt.ATTRIBS{kt.bit_32} <= EI.Objects^[obj-1].Attributes);
              <* END *>
              IF add THEN
                public.code := kt.execute IN EI.Objects^[obj-1].Attributes;
                public.addr := offset + EI.Objects^[obj-1].Begin;
                public.len  := 0;
                public.name := bld.AddName(name);
                bld.AddPublic(Count-1, public);
              END;
            END;
          END;
          DISPOSE(export);
          <* IF DEFINED (xd_debug) & xd_debug THEN *>
          IF opt.Debug(opt.Load) THEN bld.ShowDebugInfo (Count-1); END;
          <* END *>
        END;
      END;
    END;
  END;
END DoCompCreated;


PROCEDURE DoCompDestroyed (data: DATA);
VAR
  com : dt.ComNo;
  name: xs.txt_ptr;
BEGIN
  ASSERT (data = NIL);
  ASSERT(eve.LastEvent.Event = eve.CompDestroyed);
  IF eve.LastEvent.IP = kt.ADDRESS(NIL) THEN
    eve.LastEvent.IP := mem.GetIP();
  END;
  IF tls.FindComponentByHandle (eve.LastEvent.Handle, com) THEN
   <* IF TARGET_OS = "WINNT" THEN *>
    win.CloseHandle(win.HANDLE(dt.Components.Components^[com].EI.hFile));
   <* END *>
    ASSERT (tls.ComName (com, name));
    COPY (name^, LastCompName.string);
    ASSERT (tls.ComFullName (com, name));
    COPY (name^, LastCompPath.string);
    ASSERT (tls.RemoveComponent(eve.LastEvent.Handle));
  ELSE
    eve.LastEvent.Handle := 0;
  END;
END DoCompDestroyed;


VAR
  XCPT_NUMBER: exp.ExprRes;
  XCPT_ADDR  : exp.ExprRes;
  ERROR_LEVEL: exp.ExprRes;

PROCEDURE GetExceptionInfo (data: DATA);
BEGIN
  WITH eve.LastEvent DO
    ASSERT(Event = eve.Exception);
    ASSERT(sys.ADDRESS(data) = NIL);
    XCPT_NUMBER.value := CARDINAL(XCPT_INFO_1);
    XCPT_ADDR.value := CARDINAL(XCPT_INFO_2);
    IF XCPT_NUMBER.value = 0 THEN
      ERROR_LEVEL.value := CARDINAL(XCPT_INFO_4);
    END;
  END;
END GetExceptionInfo;


VAR
  Initialized: BOOLEAN;

PROCEDURE Ini;
BEGIN
  IF NOT Initialized THEN
    NextReaction := NIL;
    DATA_NIL := DATA (NIL);
    ReactArr := AllReactions { NIL BY HIGH(ReactArr)+1 };

    InsActionFirst (eve.BreakpointHit, NIL, DoAddress);
    InsActionFirst (eve.CompCreated,   NIL, DoCompCreated);
    AddAction (eve.CompDestroyed, NIL, DoCompDestroyed);

    --- Get exception information ---
    XCPT_NUMBER.type := dt.st_hex;
    XCPT_NUMBER.sort := exp.WHOLEval;
    XCPT_NUMBER.value := 0;
    ASSERT (exp.AddIdentExprVar ('XCPT_NUMBER', sys.ADR(XCPT_NUMBER)));

    XCPT_ADDR.type := dt.st_hex;
    XCPT_ADDR.sort := exp.WHOLEval;
    XCPT_ADDR.value := 0;
    ASSERT (exp.AddIdentExprVar ('XCPT_ADDR', sys.ADR(XCPT_ADDR)));

    ERROR_LEVEL.type := dt.st_original;
    ERROR_LEVEL.sort := exp.WHOLEval;
    ERROR_LEVEL.value := 0;
    ASSERT (exp.AddIdentExprVar ('ERRORLEVEL', sys.ADR(ERROR_LEVEL)));

    InsActionFirst (eve.Exception, NIL, GetExceptionInfo);

    LastCompName.type := dt.st_original;
    LastCompName.sort := exp.STRINGval;
    LastCompName.string := "";
    ASSERT (exp.AddIdentExprVar ('LASTCOMPNAME', sys.ADR(LastCompName)));

    LastCompPath.type := dt.st_original;
    LastCompPath.sort := exp.STRINGval;
    LastCompPath.string := "";
    ASSERT (exp.AddIdentExprVar ('LASTCOMPPATH', sys.ADR(LastCompPath)));

    Initialized := TRUE;
  END;
END Ini;


BEGIN
  Ini();
END ExeReact.

