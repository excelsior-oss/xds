IMPLEMENTATION MODULE Dlg_Vars;

IMPORT dt := DI_Types;

FROM DlgTypes IMPORT MESSAGE, HWND, Invalid_H;

VAR
  SaveMode: MAINMODE;

PROCEDURE RestoreMainMode;
BEGIN
  IF MainMode = need_disasm THEN
    MainMode := SaveMode;
  END;
END RestoreMainMode;

PROCEDURE SetMainMode(mode: MAINMODE);
BEGIN
  IF MainMode = need_disasm THEN
    MainMode := SaveMode;
  END;
  IF mode = need_disasm THEN
    SaveMode := MainMode;
  END;
  MainMode := mode;
END SetMainMode;


BEGIN
  MainMode := source;
  SaveMode := source;
  VarName     := '';
  PrimeFile   := '';
  expr        := '';
  DumpAddrStr := '';
  FirstTime   := TRUE;
  N_Call       := 0;
  MainPulldown := Invalid_H;
  Delay := 25;
  LocalScope := dt.Invalid_Object;
  LocalScopeLevel := MAX(CARDINAL);
  F_Init_Wnds := 0;
END Dlg_Vars.

