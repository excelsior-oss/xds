<* storage+ *>

IMPLEMENTATION MODULE DlgExcpt;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT xs := xStr;

IMPORT kt  := KrnTypes;

IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;

IMPORT win := Dlg_Win;
IMPORT eve := DlgEvent;
IMPORT act := Dlg_Acts;
IMPORT std := Dlg_Std;
IMPORT mod := DlgMods;
IMPORT crt := CRT;
IMPORT key := Keys;




TYPE
  TEXCEPTION = RECORD
                 Address: kt.ADDRESS;   -- Адрес исключительной ситуации
                 Message: xs.STRING;    -- Сообщение
               END;

  PAEXCEPTION = POINTER TO ARRAY OF TEXCEPTION;
  THISTORY = RECORD
               History : PAEXCEPTION;
               Count: CARDINAL;
             END;

VAR
  History: THISTORY;

PROCEDURE ClearExceptionsHistory ();
VAR
  i: CARDINAL;
BEGIN
  WITH History DO
    IF Count > 0 THEN
      FOR i := 0 TO Count-1 DO
        xs.dealloc_str (History^[i].Message);
      END;
      Count := 0;
    END;
  END;
END ClearExceptionsHistory;



PROCEDURE PutExceptionInfo (addr: kt.ADDRESS; message: ARRAY OF CHAR);
VAR
  tmp: PAEXCEPTION;
BEGIN
  WITH History DO
    IF History = NIL THEN
      NEW (History, 32);
      ASSERT (History # NIL);
      Count := 0;
    ELSIF Count > HIGH(History^) THEN
      NEW (tmp, 2*(HIGH(History^)+1));
      ASSERT (tmp # NIL);
      sys.MOVE (sys.ADR(History^), sys.ADR(tmp^), SIZE(History^));
      DISPOSE (History);
      History := tmp;
    END;
    WITH History^[Count] DO
      Address := addr;
      xs.alloc_from (Message, message);
    END;
    INC(Count);
  END;
END PutExceptionInfo;


PROCEDURE ExceptionsHistoryHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p   : std.PLIST;
  size: crt.SZ;

  PROCEDURE write_line (num: CARDINAL);
  VAR
    buf: xs.String;
  BEGIN
    WITH History DO
      WITH History^[Count-1-num] DO
        fmt.print (buf, "%s", Message^);
      END;
    END;
    WITH p^ DO
      crt.SetPos (2, num-frame + 1);
      crt.WrStrFromPos (hwnd, buf, Colors^[crt.List_Line], pos);
    END;
  END write_line;

VAR
  i, len: CARDINAL;
  last  : CARDINAL;
  comno : dt.ComNo;
  modno : dt.ModNo;

BEGIN
  size := win.GetWindowSize(hwnd);
  p := win.GetAMPtr (hwnd);
  WITH p^ DO
    IF N # History.Count THEN
      N := History.Count;
      frame := 0;
      curr := 0;
      std.Normalize (size, curr, frame, N);
    END;
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
    IF std.GetAction (msg.par) = act.ExceptionsHistory THEN
      act.ConfirmQuery ();
    ELSE
      std.ListBox (hwnd, msg);
    END;

  | eve.QueryKbHit:
    CASE msg.par OF
    | key.Enter:
      WITH History DO
        act.ConfirmQueryByCond ((p^.N > 0) AND tls.FindModByAddr (History^[Count-1-p^.curr].Address, comno, modno));
      END;
    ELSE
      std.ListBox (hwnd, msg);
    END;

  | eve.KbHit:
    CASE msg.par OF
    | key.Enter:
      WITH History DO
        IF NOT mod.SetNewPosByAddr (History^[Count-1-p^.curr].Address) THEN
          crt.Beep ();
        END;
      END;
      eve.AddToTail (hwnd, eve.Redraw, 0);
    ELSE
      std.ListBox (hwnd, msg);
    END;

  ELSE
    std.ListBox(hwnd, msg);
  END;
END ExceptionsHistoryHandler;



VAR
  ExceptionsHistoryWindow: win.HWND;

PROCEDURE InitExceptionsHistory (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  p   : std.PLIST;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.ExceptionsHistory);
  IF ExceptionsHistoryWindow = win.Invalid_H THEN
    ExceptionsHistoryWindow := win.RegisterWindow (ExceptionsHistoryHandler, SIZE(p^));
    ASSERT(ExceptionsHistoryWindow # win.Invalid_H);
    WITH size DO
      x2 := crt.Xmax-1;
      y2 := crt.Ymax-1;
      x1 := 0;
      y1 := y2-10;
    END;
    win.SetWindowSize (ExceptionsHistoryWindow, size);
    win.SetMovable (ExceptionsHistoryWindow);
    win.SetResizable (ExceptionsHistoryWindow, TRUE, TRUE);
    win.SetSwitchable (ExceptionsHistoryWindow);
    win.SetHeaderByStr (ExceptionsHistoryWindow, act.ActionName[act.ExceptionsHistory]);
    p := win.GetAMPtr (ExceptionsHistoryWindow);
    p^ := std.EmptyList;
    WITH p^ DO
      Colors     := sys.ADR(crt.List);
      Frame      := crt.Double;
      locator    := NIL;
      actions[0] := act.CONTEXT {act.push_key, "Go to...", key.Enter};
      actions[1] := act.EMPTY_CONTEXT;
    END;
  END;
  IF mode # act.mode_check THEN
    eve.AddToTail(ExceptionsHistoryWindow, eve.Rise, 0);
  END;
  RETURN std.QueryAction (ExceptionsHistoryWindow, act.ExceptionsHistory);
END InitExceptionsHistory;


BEGIN
  History := THISTORY {NIL, 0};
  ExceptionsHistoryWindow := win.Invalid_H;
  act.IniAction (act.ExceptionsHistory, InitExceptionsHistory);
END DlgExcpt.