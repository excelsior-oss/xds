IMPLEMENTATION MODULE Dlg_ODS;

IMPORT sys := SYSTEM;

IMPORT win := Dlg_Win;
IMPORT eve := DlgEvent;
IMPORT act := Dlg_Acts;
IMPORT std := Dlg_Std;
IMPORT crt := CRT;

IMPORT kexe:= KrnExec;

IMPORT xs := xStr;
IMPORT od := OutDebug;


VAR
  OutputDebugStringsWindow: win.HWND;


PROCEDURE OutputDebugStringsHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p   : std.PLIST;
  size: crt.SZ;

  PROCEDURE write_line (num: CARDINAL);
  VAR
    buf: xs.String;
  BEGIN
    od.GetString (num, buf);
    WITH p^ DO
      crt.SetPos(2, num-frame + 1);
      crt.WrStrFromPos (hwnd, buf, Colors^[crt.List_Line], pos);
    END;
  END write_line;

VAR
  i, len: CARDINAL;
  last  : CARDINAL;
BEGIN
  p := win.GetAMPtr (hwnd);
  len := od.StringsNo ();
  size := win.GetWindowSize(hwnd);
  IF p^.N < len THEN
    p^.N := len;
    p^.curr := p^.N-1;
    std.Normalize (size, p^.curr, p^.frame, p^.N);
  ELSIF p^.N > len THEN
    p^.N := len;
    p^.frame := 0;
    p^.curr  := 0;
  END;
  CASE msg.ID OF
  | eve.Redraw, eve.Paint:
    IF (msg.par # 3) AND (msg.par # 4) THEN
      crt.FillWindow(hwnd, ' ', p^.Colors^[crt.List_Background])
    END;
    WITH size DO
      len := y2-y1-1;
    END;
    WITH p^ DO
      IF N > 0 THEN
        CASE msg.par OF
        | 3:
          IF hwnd = win.ActiveWindow THEN
            write_line(curr);
            crt.Lite (hwnd, curr-frame+1 , 1, Colors^[crt.List_CurrentLine]);
          END;
        | 4:
          IF hwnd = win.ActiveWindow THEN
            crt.Lite (hwnd, curr-frame+1 , 1, Colors^[crt.List_Background]);
            write_line(curr);
          END;
        ELSE
          last := std.Min (frame+len-1, N-1);
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
    IF act.ACTION(msg.par MOD 100H) = act.OutputDebugStrings THEN
      act.ConfirmQueryByCond (kexe.Loaded);
    ELSE
      std.ListBox (hwnd, msg);
    END;
  ELSE
    std.ListBox(hwnd, msg);
  END;
END OutputDebugStringsHandler;



PROCEDURE InitOutputDebugStrings (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  p   : std.PLIST;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.OutputDebugStrings);
  IF NOT kexe.Loaded THEN
    RETURN FALSE;
  END;
  IF OutputDebugStringsWindow = win.Invalid_H THEN
    OutputDebugStringsWindow := win.RegisterWindow (OutputDebugStringsHandler, SIZE(p^));
    ASSERT(OutputDebugStringsWindow # win.Invalid_H);
    WITH size DO
      x2 := crt.Xmax-1;
      y2 := crt.Ymax-1;
      x1 := 0;
      y1 := y2-10;
    END;
    win.SetWindowSize (OutputDebugStringsWindow, size);
    win.SetMovable (OutputDebugStringsWindow);
    win.SetResizable (OutputDebugStringsWindow, TRUE, TRUE);
    win.SetSwitchable (OutputDebugStringsWindow);
    win.SetHeaderByStr (OutputDebugStringsWindow, act.ActionName[act.OutputDebugStrings]);
    p := win.GetAMPtr (OutputDebugStringsWindow);
    p^ := std.EmptyList;
    WITH p^ DO
      Colors     := sys.ADR(crt.List);
      Frame      := crt.Double;
      locator    := NIL;
      actions[0] := act.EMPTY_CONTEXT;
    END;
  END;
  IF mode # act.mode_check THEN
    eve.AddToTail(OutputDebugStringsWindow, eve.Rise, 0);
  END;
  RETURN std.QueryAction (OutputDebugStringsWindow, act.OutputDebugStrings);
END InitOutputDebugStrings;


BEGIN
  OutputDebugStringsWindow := win.Invalid_H;
  act.IniAction (act.OutputDebugStrings, InitOutputDebugStrings);
END Dlg_ODS.