<* Storage+ *>

IMPLEMENTATION MODULE Publics;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT crt := CRT;
IMPORT xs  := xStr;

IMPORT key := Keys;

IMPORT dv  := Dlg_Vars;
IMPORT act := Dlg_Acts;
IMPORT win := Dlg_Win;
IMPORT eve := DlgEvent;
IMPORT mod := DlgMods;

IMPORT std := Dlg_Std;

IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;

IMPORT kex := KrnExec;



PROCEDURE PublicsHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p: std.PLIST;

  PROCEDURE write_line(num: CARDINAL);
  VAR
    line : xs.txt_ptr;
  BEGIN
    WITH p^ DO
      line := tls.GetPublicName(mod.Curr^.Pos.ComN, num);
      crt.SetPos(2, num - frame + 1);
      crt.WrStrFromPos(hwnd, line^, Colors^[crt.List_Line], pos);
    END;
  END write_line;

VAR
  size   : crt.SZ;
  i, len : CARDINAL;
  last   : CARDINAL;
  public : dt.PUBLIC;
  name   : xs.txt_ptr;

BEGIN
  p := win.GetAMPtr(hwnd);
  CASE msg.ID OF
  | eve.Redraw, eve.Paint:
    size := win.GetWindowSize(hwnd);
    IF ( msg.par # 3) AND (msg.par # 4) THEN
      crt.FillWindow(hwnd, ' ', p^.Colors^[crt.List_Background])
    END;
    WITH size DO len := y2 - y1 - 1; END;
    WITH p^ DO
      IF kex.Loaded AND (mod.Curr^.Pos.ComN # dt.Invalid_Component) THEN
        N := tls.PublicsNo(mod.Curr^.Pos.ComN);
      ELSE
        N := 0;
      END;
      IF frame > N THEN
        frame := N;
      END;
      IF curr > N THEN
        curr := N;
      END;
      IF N > 0 THEN
        CASE msg.par OF
        | 3:
          IF hwnd = win.ActiveWindow THEN
            write_line(curr);
            crt.Lite(hwnd, curr - frame + 1 , 1, Colors^[crt.List_CurrentLine]);
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
            crt.Lite(hwnd, curr - frame + 1 , 1, Colors^[crt.List_CurrentLine]);
          END;
        END;
      END;
    END;
    std.ListBox (hwnd, msg);

  | eve.KbHit:
    WITH p^ DO
      CASE msg.par OF
      | key.Enter:
        IF (N > 0) THEN
          public := tls.GetPublic (mod.Curr^.Pos.ComN, p^.curr);
          IF public.code THEN
            IF mod.SetNewPosByAddr(public.addr) THEN
              eve.AddToTail(std.Wnds[std.MainWindow].hwnd, eve.Rise, 0);
            ELSE
              crt.Beep;
            END;
          ELSE
            name := tls.GetName (public.name);
            fmt.print(dv.DumpAddrStr,'%s', name^);
            act.ExecuteAction (act.Dump, act.mode_silent);
            RETURN;
          END;
        ELSE
          crt.Beep;
        END;
      ELSE
        std.ListBox (hwnd, msg);
      END;
    END;
  ELSE
    std.ListBox (hwnd, msg);
  END;
END PublicsHandler;


<* PUSH *>
<* WOFF301+ *>
PROCEDURE lctrPublic (hwnd: crt.HWND; i: CARDINAL; VAR str: ARRAY OF CHAR);
<* POP *>
VAR
  pname: xs.txt_ptr;
BEGIN
  pname := tls.GetPublicName (mod.Curr^.Pos.ComN, i);
  COPY(pname^, str);
END lctrPublic;


PROCEDURE InitPublicsWindow (show: BOOLEAN);
VAR
  p: std.PLIST;
BEGIN
  WITH std.Wnds[std.PublicsWindow] DO
    IF hwnd = win.Invalid_H THEN
      hwnd := win.RegisterWindow(PublicsHandler, SIZE(std.LIST));
      ASSERT(hwnd # win.Invalid_H);

      win.SetWindowSize(hwnd, size);

      win.SetMovable(hwnd);
      win.SetResizable(hwnd, TRUE, TRUE);
      win.SetSwitchable(hwnd);

      win.SetHeaderByStr(hwnd, 'Publics');
      p := win.GetAMPtr(hwnd);
      p^ := std.EmptyList;
      WITH p^ DO
        locator    := lctrPublic;
        Colors     := sys.ADR(crt.List);
        actions[0] := act.EMPTY_CONTEXT;
      END;
    END;
    IF show THEN eve.AddToTail(hwnd, eve.Rise, 0); END;
  END;
END InitPublicsWindow;


PROCEDURE PublicsAction (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Publics);
  IF tls.PublicsNo (mod.Curr^.Pos.ComN) = 0 THEN
    RETURN FALSE;
  END;
  IF mode # act.mode_check THEN
    InitPublicsWindow (TRUE);
  END;
  RETURN TRUE;
END PublicsAction;




BEGIN
  std.Wnds[std.PublicsWindow].init    := InitPublicsWindow;
  act.IniAction(act.Publics, PublicsAction);
END Publics.
