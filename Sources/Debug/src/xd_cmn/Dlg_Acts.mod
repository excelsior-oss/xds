IMPLEMENTATION MODULE Dlg_Acts;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT key := Keys;
IMPORT dt  := DI_Types;

VAR
  QueryResult: BOOLEAN;

PROCEDURE ExecuteAction (action: ACTION; mode: ACTION_MODE);
BEGIN
  sys.EVAL(Action[action](action, mode));
END ExecuteAction;

PROCEDURE QueryAction (action: ACTION): BOOLEAN;
BEGIN
  RETURN Action[action](action, mode_check);
END QueryAction;

PROCEDURE ClearQuery;
BEGIN
  QueryResult := FALSE;
END ClearQuery;

PROCEDURE CheckQuery (): BOOLEAN;
BEGIN
  RETURN QueryResult;
END CheckQuery;

PROCEDURE ConfirmQuery;
BEGIN
  QueryResult := TRUE;
END ConfirmQuery;

PROCEDURE ConfirmQueryByCond (condition: BOOLEAN);
BEGIN
  QueryResult := condition;
END ConfirmQueryByCond;


PROCEDURE IniIcon (action: ACTION; icon: ICON);
BEGIN
  COPY(icon, Icons[action]);
END IniIcon;

TYPE
  SHORTCUTS = ARRAY [0..255] OF ACTION;

PROCEDURE IniAction(action: ACTION; proc: ACTION_PROC);
BEGIN
  Action[action] := proc;
END IniAction;

VAR
  PrimaryShortcut: ARRAY ACTION OF key.KEY;

PROCEDURE GetPrimaryShortCut(action: ACTION): key.KEY;
BEGIN
  RETURN PrimaryShortcut[action];
END GetPrimaryShortCut;

VAR
  ShortCut1, ShortCut2: SHORTCUTS;


(* По кнопке вернет действие *)
PROCEDURE GetActionByKey (k: key.KEY; VAR action: ACTION): BOOLEAN;
VAR
  ShortCut: POINTER TO SHORTCUTS;
BEGIN
  IF k <= 0FFH THEN
    ShortCut := sys.ADR(ShortCut1);
  ELSIF (00100H <= k) AND (k <= 0FF00H) THEN
    ShortCut := sys.ADR(ShortCut2);
    k := k DIV 0100H;
  ELSE
    RETURN FALSE;
  END;
  IF (ShortCut^[k] # None) AND (ShortCut^[k] # Reserved) THEN
    action := ShortCut^[k];
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END GetActionByKey;


(* По имени вернет действие *)
PROCEDURE GetActionByName (name-: ARRAY OF CHAR; VAR action: ACTION): BOOLEAN;
VAR
  i: ACTION;
BEGIN
  FOR i := MIN(ACTION) TO MAX(ACTION) DO
    IF ActionName[i] = name THEN
      action := i;
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END GetActionByName;



PROCEDURE IniShortCut(k: CARDINAL; action: ACTION);
BEGIN
  IF k > 256 THEN
    IF ShortCut2[k DIV 256] # Reserved THEN
      ShortCut2[k DIV 256] := action;
    END;
  ELSE
    IF ShortCut1[k] # Reserved THEN
      ShortCut1[k] := action;
    END;
  END;
  IF PrimaryShortcut[action] = 0 THEN
    PrimaryShortcut[action] := k;
  END;
END IniShortCut;


PROCEDURE ExecShortCut (Key: key.KEY): BOOLEAN;
VAR
  act: ACTION;
BEGIN
  IF Key > 256 THEN
    act := ShortCut2[Key DIV 256];
  ELSE
    act := ShortCut1[Key];
  END;
  CASE act OF
  | None, Reserved:
    RETURN FALSE;
  ELSE
    IF Action[act](act, mode_loud) THEN RETURN TRUE; END;
  END;
  RETURN TRUE;
END ExecShortCut;


PROCEDURE DeleteLinksForAction (action: ACTION);
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO 255 DO
    IF ShortCut1[i] = action THEN ShortCut1[i] := None; END;
    IF ShortCut2[i] = action THEN ShortCut2[i] := None; END;
  END;
  PrimaryShortcut[action] := 0;
  PrimaryShortcut[Delete] := key.Del;
  PrimaryShortcut[DelWatch] := key.Del;
END DeleteLinksForAction;



(* Удаляет связи кнопок с действиями *)
PROCEDURE DeleteLinks;
VAR
  i: CARDINAL;
  j: ACTION;
BEGIN
  FOR i := 0 TO 255 DO
    ShortCut1[i] := None;
    ShortCut2[i] := None;
  END;

  FOR j := MIN(ACTION) TO MAX(ACTION) DO PrimaryShortcut[j] := 0; END;

  PrimaryShortcut[Delete]   := key.Del;
  PrimaryShortcut[DelWatch] := key.Del;

  IniShortCut(key.Del,      Reserved);
  IniShortCut(key.CtrlEnter,Reserved);
  IniShortCut(key.Enter,    Reserved);
  IniShortCut(key.Esc,      Reserved);
  IniShortCut(key.CtrlUp,   Reserved);
  IniShortCut(key.CtrlDown, Reserved);
  IniShortCut(key.CtrlLeft, Reserved);
  IniShortCut(key.CtrlRight,Reserved);
  IniShortCut(key.Up,       Reserved);
  IniShortCut(key.Down,     Reserved);
  IniShortCut(key.Left,     Reserved);
  IniShortCut(key.Right,    Reserved);
  IniShortCut(key.PgUp,     Reserved);
  IniShortCut(key.PgDn,     Reserved);
  IniShortCut(key.CtrlPgUp, Reserved);
  IniShortCut(key.CtrlPgDn, Reserved);
  IniShortCut(key.CtrlHome, Reserved);
  IniShortCut(key.CtrlEnd,  Reserved);
  IniShortCut(key.End,      Reserved);
  IniShortCut(key.Home,     Reserved);
END DeleteLinks;


PROCEDURE DummyAction (action: ACTION; mode: ACTION_MODE): BOOLEAN;
BEGIN
  RETURN FALSE;
END DummyAction;


VAR
  GroupShowList: CONTEXT_LIST;
  GroupTypeList: CONTEXT_LIST;
  GroupDataList: CONTEXT_LIST;

  icon: ICON;

BEGIN
  Action := ACTIONS{ DummyAction BY ORD(MAX(ACTION))+1 };

  ChangeTypesID := dt.st_original;

  GroupShowList[0] := CONTEXT{ toggler, 'Binary',  NIL};
  GroupShowList[1] := CONTEXT{ toggler, 'Octal',   NIL};
  GroupShowList[2] := CONTEXT{ toggler, 'Decimal', NIL};
  GroupShowList[3] := CONTEXT{ toggler, 'Signed',  NIL};
  GroupShowList[4] := CONTEXT{ toggler, 'Hexadec', NIL};
  GroupShowList[5] := EMPTY_CONTEXT;

  GroupShow := CONTEXT{ radio, 'Show',  NIL, sys.ADR(GroupShowList) };

  GroupTypeList[0] := CONTEXT{ toggler, 'Cardinal', NIL };
  GroupTypeList[1] := CONTEXT{ toggler, 'Integer',  NIL };
  GroupTypeList[2] := CONTEXT{ toggler, 'Char',     NIL };
  GroupTypeList[3] := CONTEXT{ toggler, 'Boolean',  NIL };
  GroupTypeList[4] := CONTEXT{ toggler, 'Address',  NIL };
  GroupTypeList[5] := CONTEXT{ toggler, 'Real',     NIL };
  GroupTypeList[6] := CONTEXT{ toggler, 'String',   NIL };
  GroupTypeList[7] := EMPTY_CONTEXT;

  GroupType := CONTEXT{ radio, 'Type', NIL, sys.ADR(GroupTypeList) };

  GroupDataList[0] := CONTEXT{ toggler, 'Byte',          NIL };
  GroupDataList[1] := CONTEXT{ toggler, 'Word',          NIL };
  GroupDataList[2] := CONTEXT{ toggler, 'DWord',         NIL };
  GroupDataList[3] := CONTEXT{ toggler, '16:16 Address', NIL };
  GroupDataList[4] := CONTEXT{ toggler, '16:32 Address', NIL };
  GroupDataList[5] := CONTEXT{ toggler, 'Long Real',     NIL };
  GroupDataList[6] := CONTEXT{ toggler, 'DLong Real',    NIL };
  GroupDataList[7] := EMPTY_CONTEXT;

  GroupData := CONTEXT{ radio, 'Data', NIL, sys.ADR(GroupDataList) };


  DeleteLinks;

  IniShortCut(key.F10,    Pulldown);
  IniShortCut(key.CtrlT,  MainWindow);

  IniShortCut(key.F9,     Sticky);
  IniShortCut(key.F8,     OneTime);
  IniShortCut(key.CtrlF9, D_Sticky);
  IniShortCut(key.AltF9,  ExprPoint);
  IniShortCut(key.CtrlF8, D_OneTime);
  IniShortCut(ORD('.'),   Counter);
  IniShortCut(key.CtrlB,  ViewAll);
  IniShortCut(key.Plus,   Enable);
  IniShortCut(key.Minus,  Disable);
  IniShortCut(key.CtrlF2, SaveAsBatch);
  IniShortCut(key.CtrlF3, RestoreFromBatch);

  IniShortCut(key.CtrlM,  Modules);
  IniShortCut(key.CtrlL,  Procs);

  IniShortCut(key.CtrlF,    Find);
  IniShortCut(key.CtrlN,    FindNext);
  IniShortCut(key.CtrlUp,   PrevProc);
  IniShortCut(key.CtrlDown, NextProc);
  IniShortCut(key.CtrlG,    GotoLine);
  IniShortCut(key.CtrlA,    GotoAddr);
  IniShortCut(key.CtrlH,    GotoExec);

  IniShortCut(key.CtrlIns, AddWatch);
  IniShortCut(key.CtrlK,   Watches);

  IniShortCut(key.F3,    Load);
  IniShortCut(key.AltX,  Quit);
  IniShortCut(key.AltX,  ReturnToBatch);
  IniShortCut(key.AltQ,  Halt);
  IniShortCut(key.CtrlE, Refresh);
  IniShortCut(ORD('/'),  Show_scr);

 <* IF DEST_XDS THEN *>
  IniShortCut(ORD('\'),  Show_wnd);
 <* END *>

  IniShortCut(key.F1,     Help);

  IniShortCut(key.F5,     Run);
  IniShortCut(key.F7,     Into);
  IniShortCut(key.CtrlF7, Over);
  IniShortCut(key.Space,  Over);
  IniShortCut(key.F4,     Skip);
  IniShortCut(key.CtrlF5, Animation);
  IniShortCut(key.CtrlF6, UptoRet);
  IniShortCut(key.CtrlX,  Restart);

  IniShortCut(key.Alt1, SourceMode);
  IniShortCut(key.Alt2, AssemblyMode);
  IniShortCut(key.Alt3, MixMode);

 <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
  IniShortCut(key.Alt0, prf_var0);
  IniShortCut(key.Alt9, prf_var9);
  IniShortCut(key.Alt8, prf_var8);
  IniShortCut(key.Alt7, prf_var7);
  IniShortCut(key.Alt6, prf_var6);
 <* END *>

  IniShortCut(key.CtrlV,  ModuleVars);
  IniShortCut(key.CtrlD,  Dump);
  IniShortCut(key.CtrlR,  Registers);
  IniShortCut(ORD('?'),   Examine);

  IniShortCut(key.CtrlM, Modules);
  IniShortCut(key.CtrlX, Restart);

  IniShortCut(key.CtrlF10, ContextMenu);

  fmt.print (icon, "{%c}", CHR(31C));
  IniIcon (Into,    icon);
  fmt.print (icon, "{}%c", CHR(32C));
  IniIcon (Over,    icon);
  fmt.print (icon, "%c{}", CHR(32C));
  IniIcon (Skip,    icon);
  fmt.print (icon, "{%c}", CHR(30C));
  IniIcon (UptoRet, icon);
  fmt.print (icon, "%c%c", CHR(30C),CHR(31C));
  IniIcon (Restart, icon);

 <* IF DEST_XDS THEN *>
  IniShortCut(key.Ins,  Access);
 <* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME
  IniShortCut(key.AltF10,  Controls);
  IniShortCut(key.AltF2,   Log);
 <* END *>
 <* ELSIF DEST_K26 THEN *>
  IniShortCut(key.Ins,    ConditionBreak);
  IniShortCut(key.F6,     UptoCall);
  IniShortCut(key.CtrlF3, LoadTableDevises);
  IniShortCut(key.CtrlY,  TableDevises);
  IniShortCut(key.Ins,    ConditionBreak);
 <* END *>
END Dlg_Acts.
