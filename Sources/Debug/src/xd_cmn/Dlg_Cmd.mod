-- ‚ë¯®«­¥­¨¥ ª®¬ ­¤ ¯ ª¥â­®£® ®â« ¤ç¨ª  ¨§ ¤¨ «®£®¢®£®

IMPLEMENTATION MODULE Dlg_Cmd;

IMPORT win := Dlg_Win;

VAR
  CalcDialog   : win.HWND;


VAR
  ExprStr: std.MESSAGE;
  ExprNum: CARDINAL;
  ExprRad: CARDINAL;

<* WOFF301+ *>
PROCEDURE EvalExpr(hwnd: win.HWND): BOOLEAN;
VAR
  res: exp.ExprRes;
  p: std.PDIALOG;
BEGIN
  p := win.GetAMPtr(CalcDialog);
  WITH p^ DO
    exp.CalcExpr (mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, ExprStr, res);
    WITH Lines^[ExprRad] DO
      CASE res.sort OF
      | exp.WHOLEval, exp.CARDval, exp.INTval:
        state := std.d_enabled;
        CASE ractive OF
        | 0: res.type := dt.st_hex;
        | 1: IF res.sort = exp.INTval THEN
               res.type := dt.st_signed;
             ELSE
               res.type := dt.st_unsigned;
             END;
        | 2: res.type := dt.st_oct;
        | 3: res.type := dt.st_bin;
        END;
      ELSE
        state := std.d_disabled;
      END;
    END;
    WITH Lines^[ExprNum] DO
      IF exp.error # 0 THEN
        pro.GetMsg(exp.error, str);
        RETURN TRUE;
      END;
      IF NOT exp.dfn THEN
        pro.GetMsg(mes.Undefined_Expression, str);
        RETURN TRUE;
      END;
      exp.Res2Str (res, str);
    END;
  END;
  eve.AddToTail (CalcDialog, eve.Redraw, 0);
  RETURN TRUE;
END EvalExpr;
<* WOFF301- *>


PROCEDURE UpdateExpr;
BEGIN
  ASSERT(EvalExpr (CalcDialog));
END UpdateExpr;


PROCEDURE CalcExpr (action: act.ACTION; check: BOOLEAN): BOOLEAN;
TYPE
  LINE = std.LINE;

VAR
  Lines: std.PLINES;
  Radio: std.PRADIO;
  p    : std.PDIALOG;
  size : crt.SZ;
  zzz  : std.MESSAGE;
BEGIN
  ASSERT(action = act.Evaluate);
  IF NOT check THEN
    IF CalcDialog = win.Invalid_H THEN
      pro.GetMsg(mes.InputExpression, zzz);

      NEW (Radio, 4);
      Radio^[0] := std.RADIO{ 3, 8, 'Hex', 1, key.AltH };
      Radio^[1] := std.RADIO{12, 8, 'Dec', 1, key.AltD };
      Radio^[2] := std.RADIO{21, 8, 'Oct', 1, key.AltO };
      Radio^[3] := std.RADIO{30, 8, 'Bin', 1, key.AltB };

      ExprNum := 9;
      ExprRad := 10;

      NEW (Lines, 11);
      Lines^[ 0] := LINE{ 2, 2, std.msg     , zzz , std.d_enabled};
      Lines^[ 1] := LINE{ 2, 3, std.msg     , '³' , std.d_enabled};
      Lines^[ 2] := LINE{ 4, 3, std.edit_str, sys.ADR(ExprStr), 35, std.d_enabled};
      Lines^[ 3] := LINE{40, 3, std.msg     , '³' , std.d_enabled};
      Lines^[ 4] := LINE{ 2, 4, std.msg     , '³' , std.d_enabled};
      Lines^[ 5] := LINE{40, 4, std.msg     , '³' , std.d_enabled};
      Lines^[ 6] := LINE{ 2, 5, std.msg     , '³' , std.d_enabled};
      Lines^[ 7] := LINE{40, 5, std.msg     , '³' , std.d_enabled};
      Lines^[ 8] := LINE{ 2, 6, std.msg     , 'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ' , std.d_enabled};

      Lines^[ExprNum] := LINE{ 4, 5, std.msg   , '???' , std.d_enabled};
      Lines^[ExprRad] := LINE{ 2, 8, std.radio , '' , 0, 0, 0, 0, Radio, UpdateExpr, std.d_enabled};

      CalcDialog := win.RegisterWindow(std.DialogProc,SIZE(std.DIALOG));
      ASSERT(CalcDialog # win.Invalid_H);
      win.SetModal(CalcDialog);
      win.SetMovable(CalcDialog);
      win.SetHeaderByStr(CalcDialog, act.ActionName[act.Evaluate]);

      size.x1 := 18; size.y1 := 7;
      size.x2 := 60; size.y2 := 19;
      win.SetWindowSize(CalcDialog,size);

      p := win.GetAMPtr(CalcDialog);
      p^.curr     := 2;
      IF (std.ErrorMsg = win.Invalid_H) THEN std.InitErrorMsg; END;
      p^.on_error := std.ErrorMsg;
      p^.Lines    := Lines;
      p^.action   := EvalExpr;
    END;
    p := win.GetAMPtr(CalcDialog);
    IF NOT opt.WholeHex THEN p^.Lines^[ExprRad].ractive := 1; END;
    eve.AddToTail(CalcDialog, eve.Rise, 0);
  END;
  RETURN TRUE;
END CalcExpr;

BEGIN
  CalcDialog := win.Invalid_H;
  act.IniAction (act.Evaluate, CalcExpr);
END Dlg_Cmd.
