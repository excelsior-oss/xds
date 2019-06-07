IMPLEMENTATION MODULE Dlg_TD;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT xs  := xStr;

IMPORT kt  := KrnTypes;

IMPORT exp := Expr;

IMPORT crt := CRT;
IMPORT std := Dlg_Std;
IMPORT eve := DlgEvent;
IMPORT win := Dlg_Win;
IMPORT act := Dlg_Acts;
IMPORT mod := DlgMods;

IMPORT mem := Exe_Mem;

-- FIX ME
IMPORT xj  := xjRTS;


PROCEDURE LocatorTD (hwnd: crt.HWND; i: CARDINAL; VAR line: ARRAY OF CHAR);


  PROCEDURE GetName (addr: sys.ADDRESS; VAR name: ARRAY OF CHAR): BOOLEAN;
  VAR
    i: CARDINAL;
    a: kt.ADDRESS;
  BEGIN
    a := kt.ADDRESS(addr);
    i := 0;
    REPEAT
      IF NOT mem.Get (a+i, sys.ADR(name[i]), 1) THEN
        RETURN FALSE;
      END;
      INC(i);
    UNTIL (i > HIGH(name)) OR (name[i-1] = '');
    RETURN TRUE;
  END GetName;


VAR
  plist: std.PLIST;
  paddr: POINTER TO kt.ADDRESS;
-- FIX ME
  name : xj.X2C_OFFSET;
  pname: xs.String;
-- FIX ME
  jsync: xj.X2C_JSYNC_TYPE;
-- FIX ME
  kind : xj.TDKinds;
  rtNum: sys.CARD16;
  begin: PROC;
-- FIX ME
  clinit: xj.X2J_clinitMethod;
-- FIX ME
  clinit_ee: xj.X2C_ExecEnv;
-- FIX ME
  initialized: sys.BOOL32;
--  log2size: sys.size_t;
-- FIX ME
  typeCode: xj.primTCodes;
-- FIX ME
  typesTable: xj.X2C_PARR_TD;
-- FIX ME
  metaInfo: xj.reflectionTD;
  modifiers: BITSET;
  hash: INTEGER;

BEGIN
  plist := win.GetAMPtr(hwnd);
  paddr := sys.ADDRESS (plist^.ext);
  CASE i OF
  | 0 : -- jsync         :X2C_JSYNC_TYPE;
    IF mem.Get (paddr^+sys.FIELDOFS(xj.X2C_TD_STR.jsync), sys.ADR (jsync), SIZE (jsync)) THEN
      fmt.print (line, "%-12s 0x%$8X", "jsync", jsync);
    ELSE
      fmt.print (line, "%-12s ???", "jsync");
    END;
  | 1 : -- kind          :TDKinds;              (* tag                                   *)
    IF mem.Get (paddr^+sys.FIELDOFS(xj.X2C_TD_STR.kind), sys.ADR (kind), SIZE (kind)) THEN
      fmt.print (line, "%-12s %d", "kind", kind);
    ELSE
      fmt.print (line, "%-12s ???", "kind");
    END;
  | 2 : -- rtNum         :CARD16;               (* class # at run-time *)
    IF mem.Get (paddr^+sys.FIELDOFS(xj.X2C_TD_STR.rtNum), sys.ADR (rtNum), SIZE (rtNum)) THEN
      fmt.print (line, "%-12s %d", "rtNum", rtNum);
    ELSE
      fmt.print (line, "%-12s ???", "rtNum");
    END;
  | 3 : -- begin         :PROC;
    IF mem.Get (paddr^+sys.FIELDOFS(xj.X2C_TD_STR.begin), sys.ADR (begin), SIZE (begin)) THEN
      fmt.print (line, "%-12s 0x%$8X", ".begin", begin);
    ELSE
      fmt.print (line, "%-12s ???", ".begin");
    END;
  | 4 : -- dimnum         :size_t;
--    IF mem.Get (paddr^+sys.FIELDOFS(xj.X2C_TD_STR.log2size), sys.ADR (log2size), SIZE (log2size)) THEN
--      fmt.print (line, "%-12s %d", ".log2size", log2size);
--    ELSE
--      fmt.print (line, "%-12s ???", ".log2size");
    END;
  | 5 : -- clinit
    IF mem.Get (paddr^+sys.FIELDOFS(xj.X2C_TD_STR.clinit), sys.ADR (clinit), SIZE (clinit)) THEN
      fmt.print (line, "%-12s 0x%$8X", "clinit", clinit);
    ELSE
      fmt.print (line, "%-12s ???", "clinit");
    END;
  | 6 : -- typesTable    :X2C_PARR_TD;          (* TDTable of component this TD belong to *)
    IF mem.Get (paddr^+sys.FIELDOFS(xj.X2C_TD_STR.typesTable), sys.ADR (typesTable), SIZE (typesTable)) THEN
      fmt.print (line, "%-12s 0x%$8X", "typesTable", typesTable);
    ELSE
      fmt.print (line, "%-12s ???", "typesTable");
    END;
  | 7 : -- metaInfo      :reflectionTD;         (* Second part of TD *)
    IF mem.Get (paddr^+sys.FIELDOFS(xj.X2C_TD_STR.metaInfo), sys.ADR (metaInfo), SIZE (metaInfo)) THEN
      fmt.print (line, "%-12s 0x%$8X", "metaInfo", metaInfo);
    ELSE
      fmt.print (line, "%-12s ???", "metaInfo");
    END;
  | 8 : -- modifiers     :BITSET;
    IF mem.Get (paddr^+sys.FIELDOFS(xj.X2C_TD_STR.modifiers), sys.ADR (modifiers), SIZE (modifiers)) THEN
      fmt.print (line, "%-12s %{}", "modifiers", modifiers);
    ELSE
      fmt.print (line, "%-12s ???", "modifiers");
    END;
  | 9 : -- hash          :INTEGER;              (* hashCode of type name                 *)
    IF mem.Get (paddr^+sys.FIELDOFS(xj.X2C_TD_STR.hash), sys.ADR (hash), SIZE (hash)) THEN
      fmt.print (line, "%-12s 0x%$8X", "hash", hash);
    ELSE
      fmt.print (line, "%-12s ???", "hash");
    END;
  | 10 : -- name          :X2C_OFFSET;           (*pCHAR*) (* type name                    *)
    IF mem.Get (paddr^+sys.FIELDOFS(xj.X2C_TD_STR.name), sys.ADR (name), SIZE (name)) THEN
      sys.FILL (sys.ADR(pname), 0C, SIZE(pname));
      IF (name > 0) AND GetName (sys.ADDADR(sys.ADDRESS (paddr^), VAL (CARDINAL, name)), pname) THEN
        fmt.print (line, '%-12s "%s"', "name", pname);
      ELSE
        fmt.print (line, '%-12s "%s" ???', "name", pname);
      END;
    ELSE
      fmt.print (line, "%-12s ???", "name");
    END;
  ELSE
    COPY ("", line);
  END;
END LocatorTD;


PROCEDURE HandlerTD (hwnd: win.HWND; msg: eve.MSG);
VAR
  p   : std.PLIST;
  size: crt.SZ;

  PROCEDURE write_line (num: CARDINAL);
  VAR
    line: xs.String;
  BEGIN
    LocatorTD (hwnd, num, line);
    WITH p^ DO
      crt.SetPos(2, num-frame+1);
      crt.WrStrFromPos (hwnd, line, Colors^[crt.List_Line], pos);
    END;
  END write_line;

VAR
  i, len: CARDINAL;
  last  : CARDINAL;

BEGIN
  p := win.GetAMPtr(hwnd);
  size := win.GetWindowSize(hwnd);
  CASE msg.ID OF
  | eve.Redraw, eve.Paint:
    IF (msg.par # 3) AND (msg.par # 4) THEN
      crt.FillWindow(hwnd, ' ', p^.Colors^[crt.List_Background])
    END;
    WITH size DO len := y2 - y1 - 1; END;
    WITH p^ DO
      IF N > 0 THEN
        CASE msg.par OF
        | 3:
          IF hwnd = win.ActiveWindow THEN
            write_line(curr);
            crt.Lite(hwnd, curr-frame+1 , 1, Colors^[crt.List_CurrentLine]);
          END;
        | 4:
          IF hwnd = win.ActiveWindow THEN
            crt.Lite(hwnd, curr-frame+1 , 1, Colors^[crt.List_Background]);
            write_line(curr);
          END;
        ELSE
          last := std.Min(frame+len-1, N-1);
          FOR i:= frame TO last DO
            write_line(i)
          END;
          IF hwnd = win.ActiveWindow THEN
            crt.Lite(hwnd, curr-frame+1 , 1, Colors^[crt.List_CurrentLine]);
          END;
        END;
      END;
    END;
    std.ListBox (hwnd, msg);

  | eve.QueryAction:
    CASE std.GetAction (msg.par) OF
    | act.ViewTD:
      act.ConfirmQuery ();
    ELSE
      std.ListBox (hwnd, msg);
    END;

  ELSE
    std.ListBox(hwnd, msg);
  END;
END HandlerTD;


VAR
  TDHwnd: win.HWND;
  ActiveTDHwnd: win.HWND;


PROCEDURE InitTDWindow (TDAddr: kt.ADDRESS);
VAR
  plist : std.PLIST;
  size  : crt.SZ;
  paddr : POINTER TO kt.ADDRESS;
  header: xs.String;
BEGIN
  IF TDHwnd = win.Invalid_H THEN
    TDHwnd := win.RegisterWindow (HandlerTD, SIZE(std.LIST)+SIZE(TDAddr));
  ELSE
    TDHwnd := win.FindClosed (win.Invalid_H, HandlerTD);
    IF TDHwnd = win.Invalid_H THEN
      TDHwnd := win.RegisterWindow (HandlerTD, SIZE(std.LIST)+SIZE(TDAddr));
    END;
  END;
  ASSERT (TDHwnd # win.Invalid_H);
  win.UnHide (TDHwnd);

  size := crt.SZ {0, 1, crt.Xmax-1, crt.Ymax-1};
  win.SetWindowSize (TDHwnd, size);
  win.SetMovable (TDHwnd);
  win.SetResizable (TDHwnd, TRUE, TRUE);
  win.SetSwitchable (TDHwnd);
  fmt.print (header, "%s: 0x%$8X", act.ActionName[act.ViewTD], TDAddr);
  win.SetHeaderByStr (TDHwnd, header);
  plist := win.GetAMPtr (TDHwnd);
  plist^ := std.EmptyList;
  WITH plist^ DO
    N          := 64;
    Colors     := sys.ADR (crt.List);
    Frame      := crt.Double;
    locator    := LocatorTD;
    actions[0] := act.EMPTY_CONTEXT;
    ext        := sys.ADDADR (sys.ADR(plist^), SIZE(std.LIST));
    paddr      := sys.ADDRESS (ext);
  END;

  paddr^ := TDAddr;

  ActiveTDHwnd := TDHwnd;
  eve.AddToTail (TDHwnd, eve.Rise, 0);
END InitTDWindow;


VAR
  TDAddrStr: xs.String;

PROCEDURE InputTDAddr (hwnd: win.HWND): BOOLEAN;
VAR
  TDAddr: kt.ADDRESS;
BEGIN
  IF NOT exp.GetAddress (mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, TDAddrStr, TDAddr) THEN
    std.SetErrorNo (exp.error);
    RETURN FALSE;
  END;
  IF hwnd # win.Invalid_H THEN
    eve.AddToTail (hwnd, eve.Hide, 0);
  END;
  InitTDWindow (TDAddr);
  eve.Flush;
  RETURN TRUE;
END InputTDAddr;


PROCEDURE InitTDList (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.ViewTD);
  IF mode # act.mode_check THEN
    std.OpenUniversalDialogByStr ("Input address of TD", InputTDAddr, TDAddrStr);
  END;
  RETURN TRUE;
END InitTDList;


BEGIN
  TDHwnd := win.Invalid_H;
  ActiveTDHwnd := win.Invalid_H;
  act.IniAction (act.ViewTD, InitTDList);
END Dlg_TD.
