<* Storage+ *>
IMPLEMENTATION MODULE DlgBreak;

IMPORT sys := SYSTEM;
IMPORT wstr:= WholeStr;

IMPORT std := Dlg_Std;

IMPORT crt := CRT;
IMPORT eve := Events;
IMPORT exp := Expr;
IMPORT fmt := FormStr;
IMPORT key := Keys;
IMPORT mes := MsgNo;
IMPORT pro := Protocol;
IMPORT xs  := xStr;
IMPORT txt := Texts;
IMPORT opt := Options;
IMPORT fil := File;

IMPORT brk := Breaks;

IMPORT mem := Exe_Mem;
IMPORT exe := ExeMain;

FROM Breaks IMPORT Breakpoints;

IMPORT dv  := Dlg_Vars;
IMPORT act := Dlg_Acts;
IMPORT dsm := Dlg_Dasm;
IMPORT deve:= DlgEvent;
IMPORT mod := DlgMods;
IMPORT win := Dlg_Win;
IMPORT men := Dlg_Menu;
IMPORT brw := DlgBrows;
IMPORT dlt := DlgTypes;

IMPORT kexe:= KrnExec;
IMPORT kt  := KrnTypes;
IMPORT kmem:= Krn_Mem;

IMPORT tls := DI_Tools;
IMPORT dt  := DI_Types;

<* IF xd_batch_included THEN *>

IMPORT pt  := PckTypes;
IMPORT bas := PckBase;

<* IF DEST_K26 THEN *>
IMPORT mdl := Model;
<* END *>

<* END *>




VAR
  CondDialog   : win.HWND;


(* =============================================================== *)
(*            Непосредственная работа с остановами                 *)
(* =============================================================== *)

PROCEDURE FindBreakPos(com, mod, ln: CARDINAL): CARDINAL;
VAR
  i: CARDINAL;
BEGIN
  WITH brk.Breakpoints DO
    IF free > 0 THEN
      FOR i := 0 TO free-1 DO
        WITH Breakpoints^[i] DO
          IF Pos.ComN = com THEN
            IF Pos.ModN = mod THEN
              IF Line = ln + 1 THEN
                RETURN i;
              ELSIF Line > ln+1 THEN
                RETURN MAX(CARDINAL);
              END;
            ELSIF Pos.ModN > mod THEN
              RETURN MAX(CARDINAL);
            END;
          ELSIF Pos.ComN > com THEN
            RETURN MAX(CARDINAL)
          END;
        END;
      END;
    END;
  END;
  RETURN MAX(CARDINAL);
END FindBreakPos;

PROCEDURE FindBreakPosByAddr(addr: kt.ADDRESS): CARDINAL;
VAR
  i: CARDINAL;
BEGIN
  WITH brk.Breakpoints DO
    IF free > 0 THEN
      FOR i := 0 TO free-1 DO
        WITH Breakpoints^[i] DO
          IF (addr = Addr) THEN
            RETURN i;
          END;
        END;
      END;
    END;
  END;
  RETURN MAX(CARDINAL);
END FindBreakPosByAddr;

(* =============================================================== *)
(*            Взаимодействие с диалогом                            *)
(* =============================================================== *)

PROCEDURE RefreshBreaks;
BEGIN
  deve.AddToTail(std.Wnds[std.MainWindow].hwnd, deve.Redraw, 0);
  WITH std.Wnds[std.BrkWindow] DO
    IF (hwnd # win.Invalid_H) AND (win.Visible(hwnd)) THEN
      deve.AddToTail(hwnd, deve.Redraw, 0);
    END;
  END;
END RefreshBreaks;


<* PUSH *>
<* WOFF903+ *>

PROCEDURE DeletePoint (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  addr: kt.ADDRESS;
  p2: std.PLIST;
  w : win.HWND;
  size: crt.SZ;
  N, N1, N2 : CARDINAL;
  ind: CARDINAL;
BEGIN
  ASSERT(action = act.Delete);
  N1 := brk.Breakpoints.free;
  N2 := N1 + brk.AccessBreaks.free;
  N  := N2 + brk.ConditionBreaks.free;
  w := men.SkipMenus();
  IF N = 0 THEN
    IF mode # act.mode_check THEN
      crt.Beep;
    END;
    RETURN FALSE;
  END;
  IF w = std.Wnds[std.MainWindow].hwnd THEN
    IF N1 = 0 THEN
      IF mode # act.mode_check THEN crt.Beep; END;
      RETURN FALSE;
    END;
    CASE dv.MainMode OF
    | dv.source:
      WITH mod.Curr^ DO
        ind := FindBreakPos(Pos.ComN, Pos.ModN, curr);
        IF ind = MAX(CARDINAL) THEN
          IF mode # act.mode_check THEN crt.Beep; END;
          RETURN FALSE;
        END;
        IF mode # act.mode_check THEN IF brk.Delete_Breakpoint(ind) THEN END; END;
      END;
    ELSE
      IF NOT dsm.GetAddr(dsm.D_curr, addr) THEN
        IF mode # act.mode_check THEN crt.Beep; END;
        RETURN FALSE;
      END;
      ind := FindBreakPosByAddr(addr);
      IF ind = MAX(CARDINAL) THEN
        IF mode # act.mode_check THEN crt.Beep; END;
        RETURN FALSE;
      END;
      IF mode # act.mode_check THEN IF brk.Delete_Breakpoint(ind) THEN END; END;
    END;
  ELSIF w = std.Wnds[std.BrkWindow].hwnd THEN
    IF mode # act.mode_check THEN
      p2 := win.GetAMPtr(std.Wnds[std.BrkWindow].hwnd);
      size := win.GetWindowSize(std.Wnds[std.BrkWindow].hwnd);
      WITH p2^ DO
        IF curr < N1 THEN
          IF brk.Delete_Breakpoint(curr) THEN END;
        ELSIF curr < N2 THEN
          ASSERT(brk.Delete_AccessBreak(curr-N1));
        ELSE
          ASSERT(brk.Delete_ConditionBreak(curr-N2));
        END;
      END;
    END;
  ELSE
    IF mode # act.mode_check THEN crt.Beep; END;
    RETURN FALSE;
  END;
  IF mode # act.mode_check THEN
    IF (std.Wnds[std.BrkWindow].hwnd # win.Invalid_H) AND
       (win.Visible(std.Wnds[std.BrkWindow].hwnd)) THEN
      p2 := win.GetAMPtr(std.Wnds[std.BrkWindow].hwnd);
      size := win.GetWindowSize(std.Wnds[std.BrkWindow].hwnd);
      IF (p2^.curr > 0) AND (p2^.curr+1 = N) THEN
        DEC(p2^.curr);
      END;
      std.Normalize(size,p2^.curr, p2^.frame, N);
    END;
    RefreshBreaks;
  END;
  RETURN TRUE;
END DeletePoint;


PROCEDURE ObtainBreakPoint (VAR bp: brk.BREAKPOINT): BOOLEAN;
VAR
  p: std.PLIST;
  w: win.HWND;
BEGIN
  w := men.SkipMenus();
  IF w = std.Wnds[std.MainWindow].hwnd THEN
    WITH mod.Curr^ DO
      bp := brk.EmptyBREAKPOINT;
      bp.Break.Owner := brk.Dialog;
      CASE dv.MainMode OF
      | dv.source:
        bp.Pos.ComN := Pos.ComN;
        bp.Pos.ModN := Pos.ModN;
        bp.Line  := curr + 1;
        IF NOT tls.AddrBySource(bp.Pos.ComN, bp.Pos.ModN, bp.Line, bp.Addr) THEN RETURN FALSE; END;
      ELSE
        bp.Line  := 0;
        IF NOT dsm.GetAddr(dsm.D_curr, bp.Addr) THEN RETURN FALSE; END;
        IF NOT tls.FindModByAddr(bp.Addr, bp.Pos.ComN, bp.Pos.ModN) THEN END;
      END;
    END;
  ELSIF w = std.Wnds[std.BrkWindow].hwnd THEN
    WITH brk.Breakpoints DO
      p := win.GetAMPtr(std.Wnds[std.BrkWindow].hwnd);
      IF (free = 0) OR (p^.curr >= free) THEN RETURN FALSE; END;
      bp := Breakpoints^[p^.curr];
    END;
  ELSE
    RETURN FALSE;
  END;
  RETURN TRUE;
END ObtainBreakPoint;

<* POP *>



VAR
  count: std.MESSAGE;


PROCEDURE CountBreak (action: std.D_ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  p   : std.PLIST;
  ind : CARDINAL;
  w   : win.HWND;
  bp  : brk.BREAKPOINT;
  addr: kt.ADDRESS;
BEGIN
  w := men.SkipMenus();
  IF NOT ObtainBreakPoint(bp) THEN
    IF mode # act.mode_check THEN crt.Beep; END;
    RETURN FALSE;
  END;
  IF mode # act.mode_check THEN
    IF w = std.Wnds[std.MainWindow].hwnd THEN
      WITH mod.Curr^ DO
        IF dv.MainMode = dv.source THEN
          ind := FindBreakPos(Pos.ComN, Pos.ModN, curr);
        ELSE
          ASSERT(dsm.GetAddr(dsm.D_curr, addr));
          ind := FindBreakPosByAddr(addr);
        END;
      END;
    ELSIF w = std.Wnds[std.BrkWindow].hwnd THEN
      IF brk.Breakpoints.free = 0 THEN RETURN FALSE; END;
      p := win.GetAMPtr(std.Wnds[std.BrkWindow].hwnd);
      IF p^.curr > brk.Breakpoints.free THEN
        crt.Beep;
        RETURN FALSE;
      END;
      ind := p^.curr;
    ELSE
      RETURN FALSE;
    END;
    IF ind = MAX(CARDINAL) THEN
      count := '';
    ELSE
      WITH brk.Breakpoints.Breakpoints^[ind] DO
        IF (Kind = brk.counter) AND (Break.Pass > 0) THEN
          fmt.print(count, '%d', Break.Pass-1);
        ELSE
          fmt.print(count, '%d', Break.Pass);
        END;
      END;
    END;
    std.OpenUniversalDialog(mes.dlg_BreakpointDelay, action, count);
  END;
  RETURN TRUE;
END CountBreak;


PROCEDURE PermCount(hwnd: win.HWND): BOOLEAN;
VAR
  bp  : brk.BREAKPOINT;
  res : wstr.ConvResults;
  tmp : CARDINAL;
BEGIN
  wstr.StrToCard(count, tmp, res);
  IF res # wstr.strAllRight THEN
    std.SetErrorMsgNo(mes.ExpectedIntConstant);
    RETURN FALSE;
  ELSE
    IF NOT ObtainBreakPoint(bp) THEN crt.Beep; RETURN FALSE END;
    bp.Break.Sticky := TRUE;
    bp.Break.Active := TRUE;
    bp.Break.Pass   := tmp;
    bp.Kind   := brk.normal;
    bp.init_value := tmp;
    IF NOT brk.Add_Breakpoint(bp, TRUE, tmp) THEN RETURN FALSE; END;
    RefreshBreaks;
    deve.AddToTail(hwnd, deve.Hide, 0);
    deve.Flush;
    RETURN TRUE;
  END;
END PermCount;

PROCEDURE OTCount(hwnd: win.HWND): BOOLEAN;
VAR
  bp  : brk.BREAKPOINT;
  res : wstr.ConvResults;
  tmp : CARDINAL;
BEGIN
  wstr.StrToCard(count, tmp, res);
  IF res # wstr.strAllRight THEN
    std.SetErrorMsgNo(mes.ExpectedIntConstant);
    RETURN FALSE;
  ELSE
    IF NOT ObtainBreakPoint(bp) THEN crt.Beep; END;
    bp.Break.Sticky := FALSE;
    bp.Break.Active := TRUE;
    bp.Break.Pass   := tmp;
    bp.Kind    := brk.normal;
    bp.init_value := tmp;
    IF NOT brk.Add_Breakpoint(bp, TRUE, tmp) THEN RETURN FALSE; END;
    RefreshBreaks;
    deve.AddToTail(hwnd, deve.Hide, 0);
    deve.Flush;
    RETURN TRUE;
  END;
END OTCount;


PROCEDURE SetDelayedBreak(action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  CASE action OF
  | act.D_Sticky : RETURN CountBreak (PermCount, mode);
  | act.D_OneTime: RETURN CountBreak (OTCount, mode);
  ELSE
    ASSERT(FALSE);
  END;
END SetDelayedBreak;


PROCEDURE SetBreak (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  bp: brk.BREAKPOINT;
  index: CARDINAL;
BEGIN
  IF NOT ObtainBreakPoint(bp) THEN
    IF mode # act.mode_check THEN crt.Beep; END;
    RETURN FALSE;
  END;
  IF mode # act.mode_check THEN
    bp.Break.Owner  := brk.Dialog;
    bp.Break.Active := TRUE;
    bp.Break.Pass   := 0;
    bp.Kind         := brk.normal;
    CASE action OF
    | act.OneTime:
      bp.Break.Sticky := FALSE;
    | act.Sticky:
      bp.Break.Sticky := TRUE;
    | act.Counter:
      bp.Break.Sticky := TRUE;
      bp.Kind         := brk.counter;
    | act.Watchpoint:
      bp.Break.Sticky := TRUE;
      bp.Kind         := brk.watchpoint;
    ELSE
      ASSERT(FALSE);
    END;
    IF NOT brk.Add_Breakpoint(bp, TRUE, index) THEN crt.Beep; RETURN FALSE; END;
    RefreshBreaks;
  END;
  RETURN TRUE;
END SetBreak;


VAR
  BCondStr: std.MESSAGE;

PROCEDURE SetCondBreak(hwnd: win.HWND):BOOLEAN;
VAR
  res  : exp.ExprRes;
  bp   : brk.BREAKPOINT;
  index: CARDINAL;
  zzz  : xs.String;
BEGIN
  exp.CalcExpr(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, BCondStr, res);
  IF exp.error = 0 THEN
    IF res.sort # exp.BOOLval THEN
      std.SetErrorMsgNo(mes.ExpectedBooleanValue);
      RETURN FALSE;
    END;
  ELSIF NOT exp.dfn THEN
    pro.GetMsg(exp.error, zzz);
    std.NotifyNo(mes.WrongExprInCurrentContext, zzz);
  ELSE
    std.NotifyNo(exp.error);
  END;
  IF NOT ObtainBreakPoint(bp) THEN crt.Beep; RETURN FALSE; END;
  bp.Break.Active := TRUE;
  bp.Break.Sticky := TRUE;
  bp.Break.Pass   := 0;
  bp.Kind := brk.normal;
  xs.alloc_from(bp.Condition, BCondStr);

  IF NOT brk.Add_Breakpoint(bp, TRUE, index) THEN RETURN FALSE; END;
  RefreshBreaks;
  deve.AddToTail(hwnd, deve.Hide, 0);
  deve.Flush;
  RETURN TRUE;
END SetCondBreak;


PROCEDURE BreakByCond (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
TYPE
  LINE = std.LINE;
VAR
  ind  : CARDINAL;
  p2   : std.PLIST;
  w : win.HWND;
  bp: brk.BREAKPOINT;
  addr: kt.ADDRESS;
BEGIN
  ASSERT(action = act.ExprPoint);
  IF NOT ObtainBreakPoint(bp) THEN
    IF mode # act.mode_check THEN crt.Beep; END;
    RETURN FALSE;
  END;
  IF mode # act.mode_check THEN
    w := men.SkipMenus();
    IF w = std.Wnds[std.MainWindow].hwnd THEN
      WITH mod.Curr^ DO
        IF dv.MainMode = dv.source THEN
          ind := FindBreakPos(Pos.ComN, Pos.ModN, curr);
        ELSE
          ASSERT(dsm.GetAddr(dsm.D_curr, addr));
          ind := FindBreakPosByAddr(addr);
        END;
      END;
    ELSIF w = std.Wnds[std.BrkWindow].hwnd THEN
      p2 := win.GetAMPtr(std.Wnds[std.BrkWindow].hwnd);
      IF p2^.curr >= brk.Breakpoints.free THEN RETURN FALSE; END;
      ind := p2^.curr;
    ELSE
      RETURN FALSE;
    END;
    IF ind <> MAX(CARDINAL) THEN
      WITH brk.Breakpoints.Breakpoints^[ind] DO
        IF Condition # NIL THEN
          COPY(Condition^, BCondStr);
        ELSE
          BCondStr := '';
        END;
      END;
    END;
    std.OpenUniversalDialog(mes.dlg_ExpressionBreakpoint, SetCondBreak, BCondStr);
  END;
  RETURN TRUE;
END BreakByCond;



VAR
  condition, address: std.MESSAGE;

TYPE
  LENS=ARRAY [0..4] OF CARDINAL;
CONST
  Lens = LENS{ 1, 2, 4, 8, 16};

PROCEDURE Dummy;
END Dummy;



<* IF DEST_K26 THEN *>

PROCEDURE Toggle;
VAR
  p: std.PDIALOG;
BEGIN
  p := win.GetAMPtr(CondDialog);
  WITH p^ DO
    IF curr = 0 THEN
      WITH Lines^[0] DO
        IF ractive = 2 THEN
          p^.Lines^[1].state := std.d_disabled;
          p^.Lines^[3].state := std.d_disabled;
          p^.Lines^[5].state := std.d_enabled;
        ELSE
          p^.Lines^[1].state := std.d_enabled;
          p^.Lines^[5].state := std.d_disabled;
          p^.Lines^[3].state := std.d_enabled;
        END;
      END;
    END;
  END;
END Toggle;

PROCEDURE AddCondBreak(hwnd: win.HWND): BOOLEAN;
VAR
  CB  : brk.CONDITION_BREAK;
  AB  : brk.ACCESS_BREAK;
  p   : std.PDIALOG;
  res : BOOLEAN;
  R   : CARDINAL;
  Addr: kt.ADDRESS;
BEGIN
  p := win.GetAMPtr(CondDialog);
  IF p^.Lines^[0].ractive = 2 THEN
    IF LENGTH(condition) = 0 THEN
      std.SetErrorNo(mes.EmptyCondition);
      RETURN FALSE;
    END;
    IF NOT exp.GetRelation (mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, condition, res) THEN
  (*    std.SetErrorNo(exp.error);
      RETURN FALSE;
  *)
      std.Notify('В текущем контексте выражение неопределено');
    END;
    CB := brk.EmptyCONDITION_BREAK;
    CB.Break.Owner  := brk.Dialog;
    CB.Break.Active := TRUE;
    CB.Break.Sticky := TRUE;
    CB.Break.Pass   := 0;
    xs.alloc_from(CB.Expr, condition);
    ASSERT(brk.Add_ConditionBreak(CB));
    RefreshBreaks;
  ELSE
    IF NOT exp.GetAddrOrReg(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, address, Addr, R) THEN
      std.SetErrorNo(exp.error);
      RETURN FALSE;
    END;
    --AB              := brk.EmptyCONDITION_BREAK;
    AB.Break.Owner  := brk.Dialog;
    AB.Break.Active := TRUE;
    AB.Break.Sticky := TRUE;
    AB.Break.Pass   := 0;
    IF p^.Lines^[0].ractive = 0 THEN
      AB.Access_Type := eve.Write;
    ELSIF p^.Lines^[0].ractive = 1 THEN
      AB.Access_Type := eve.Read;
    END;
    WITH AB.Access_Data DO
      IF (R = MAX(CARDINAL)) THEN
        Access_ID := brk.Memory;
        Location  := Addr;
        Len  := Lens[p^.Lines^[1].ractive];
      ELSE
        Access_ID := brk.Register;
        Reg_No := R;
        ASSERT(mem.GetReg(Reg_No, Prev_Reg_value));
      END;
      IF NOT brk.Add_AccessBreak(AB) THEN
        std.SetErrorNo(mes.Incorrect_address);
        RETURN FALSE;
      END;
      RefreshBreaks;
    END;
  END;
  deve.AddToTail(hwnd, deve.Hide, 0);
  deve.Flush;
  RETURN TRUE;
END AddCondBreak;

PROCEDURE Cond (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
TYPE
  LINE = std.LINE;
  RADIO = std.RADIO;
VAR
  p: std.PDIALOG;
  Lines : std.PLINES;
  size: crt.SZ;
  Radio1: std.PRADIO;
  Radio2: std.PRADIO;

BEGIN
  ASSERT( (action = act.ConditionBreak) );
  IF mode # act.mode_check THEN
    IF CondDialog = win.Invalid_H THEN
      NEW(Lines,6);

      NEW(Radio1, 3);
      Radio1^[0] := RADIO{ 4, 4, 'По записи' , 4, key.AltP };
      Radio1^[1] := RADIO{ 4, 5, 'По чтению' , 4, key.AltX };
      Radio1^[2] := RADIO{ 4, 6, 'По условию', 4, key.AltE };

      NEW(Radio2, 4);
      Radio2^[0] := RADIO{ 26, 4, 'Байт'      , 2, key.AltF };
      Radio2^[1] := RADIO{ 26, 5, 'Слово'     , 1, key.AltC };
      Radio2^[2] := RADIO{ 26, 6, 'Дв. слово' , 1, key.AltL };
      Radio2^[3] := RADIO{ 26, 7, 'Тетpаслово', 1, key.AltN };

      Lines^[0] := LINE{ 2, 2, std.radio, ' Тип останова ' , 18, 5, 0, 0,  Radio1,  Toggle, std.d_enabled};
      Lines^[1] := LINE{ 24, 2, std.radio, ' Длина участка ' , 19, 6, 0, 0, Radio2, Dummy, std.d_enabled};
      Lines^[2] := LINE{ 2, 9, std.msg, 'Адpес:', std.d_enabled };
      Lines^[3] := LINE{ 8, 9, std.edit_str, sys.ADR(address), 14, std.d_enabled };
      Lines^[4] := LINE{ 2, 11, std.msg, 'Условие:', std.d_disabled };
      Lines^[5] := LINE{ 10, 11, std.edit_str, sys.ADR(condition),   32, std.d_disabled };


      CondDialog := win.RegisterWindow(std.DialogProc, SIZE(std.DIALOG));
      ASSERT(CondDialog # win.Invalid_H);
      win.SetModal(CondDialog);

      size.x1 := 14; size.y1 := 4;
      size.x2 := 60; size.y2 := 19;
      win.SetWindowSize(CondDialog,size);

      p := std.PDIALOG(win.GetAMPtr(CondDialog));
      p^.curr     := 0;
      p^.on_error := std.ErrorMsg;
      p^.Lines    := Lines;
      p^.action   := AddCondBreak;
    END;
    deve.AddToTail(CondDialog, deve.Rise, 0);
  END;
  RETURN TRUE;
END Cond;

<* ELSE *>

VAR
  Curr  : CARDINAL;
  Dialog: BOOLEAN;

PROCEDURE CreateNewAccessBreak(access_type: eve.ACCESS_TYPE; Addr: kt.ADDRESS; len: CARDINAL): BOOLEAN;
VAR
  AB  : brk.ACCESS_BREAK;
BEGIN
  AB              := brk.EmptyACCESS_BREAK;
  AB.Break.Owner  := brk.Dialog;
  AB.Break.Active := TRUE;
  AB.Break.Sticky := TRUE;
  AB.Break.Pass   := 0;
  AB.Access_Type  := access_type;
  WITH AB.Access_Data DO
    Access_ID := brk.Memory;
    Location  := Addr;
    Len       := len;
<* IF TARGET_x86 THEN *>
    IF MAX(CARDINAL)-Addr <= Len THEN
      std.SetErrorMsg(" Incorrect range of addresses");
      Dialog := TRUE;
      RETURN FALSE;
    END;

    IF (Addr MOD Len) # 0 THEN
      IF Len = 2 THEN
        std.SetErrorMsg("Watchpoint address must be aligned on word boundary");
      ELSE
        std.SetErrorMsg("Watchpoint address must be aligned on dword boundary");
      END;
      Dialog := TRUE;
      RETURN FALSE;
    END;
<* END *>
    IF NOT mem.Get(Location, sys.ADR(Prev_value[0]), Len) THEN
      sys.FILL(sys.ADR(Prev_value[0]), 0, 4);
    END;
    Prev_value_long := NIL;

    IF NOT brk.Add_AccessBreak(AB) THEN
      std.SetErrorMsgNo (mes.SomeInternalDebuggerError);
      Dialog := TRUE;
      RETURN FALSE;
    END;
<* IF DEST_XDS THEN *>
    IF NOT AB.Break.Active THEN
      std.NotifyNo(mes.HardwareAccessBreakLimit);
    END;
<* END *>

    RefreshBreaks;
  END;
  Dialog := FALSE;
  RETURN TRUE;
END CreateNewAccessBreak;

PROCEDURE AddAccessBreak (hwnd: win.HWND): BOOLEAN;
VAR
  p   : std.PDIALOG;
  Access_Type: eve.ACCESS_TYPE;
  Addr: kt.ADDRESS;
  Len : CARDINAL;
BEGIN
  p := win.GetAMPtr(CondDialog);
  IF NOT exp.GetAddress(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, address, Addr) THEN
    std.SetErrorNo(exp.error);
    Dialog := TRUE;
    RETURN FALSE;
  END;
  IF p^.Lines^[0].ractive = 0 THEN
    Access_Type := eve.Write;
  ELSIF p^.Lines^[0].ractive = 1 THEN
    Access_Type := eve.Read;
  ELSE
    Access_Type := eve.Nothing;
  END;
  Len := Lens[p^.Lines^[1].ractive];

  IF CreateNewAccessBreak(Access_Type, Addr, Len) THEN
    IF hwnd # win.Invalid_H THEN
      deve.AddToTail(hwnd, deve.Hide, 0);
      deve.Flush;
    END;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END AddAccessBreak;

PROCEDURE AddConditionBreak(hwnd: win.HWND): BOOLEAN;
VAR
  CB  : brk.CONDITION_BREAK;
  res : BOOLEAN;
BEGIN
  IF LENGTH(condition) = 0 THEN
    std.SetErrorNo(mes.EmptyCondition);
    RETURN FALSE;
  END;
  IF NOT exp.GetRelation(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, condition, res) THEN
(*    std.SetErrorNo(exp.error);
    RETURN FALSE;
*)
    std.NotifyNo(mes.WrongExprInCurrentContext, condition);
  END;
  IF Curr = MAX(CARDINAL) THEN
    CB := brk.EmptyCONDITION_BREAK;
    CB.Break.Owner  := brk.Dialog;
    CB.Break.Active := TRUE;
    CB.Break.Sticky := TRUE;
    CB.Break.Pass   := 0;
    xs.alloc_from(CB.Expr, condition);
    ASSERT(brk.Add_ConditionBreak(CB));
  ELSE
    WITH brk.ConditionBreaks.ConditionBreaks^[Curr] DO
      xs.dealloc_str(Expr);
      xs.alloc_from(Expr, condition);
    END;
  END;
  RefreshBreaks;
  deve.AddToTail(hwnd, deve.Hide, 0);
  deve.Flush;
  RETURN TRUE;
END AddConditionBreak;


PROCEDURE ConditionBreak (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Condition);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    Curr := MAX(CARDINAL);
    std.OpenUniversalDialog(mes.InputCondition, AddConditionBreak, condition);
  END;
  RETURN TRUE;
END ConditionBreak;


PROCEDURE InitAccess;
TYPE
  LINE = std.LINE;
  RADIO = std.RADIO;
VAR
  p: std.PDIALOG;
  Lines : std.PLINES;
  size: crt.SZ;
  Radio1: std.PRADIO;
  Radio2: std.PRADIO;

BEGIN
  Curr := MAX(CARDINAL);
  IF CondDialog = win.Invalid_H THEN

    NEW(Radio1, 2);
    Radio1^[0] := RADIO{ 4, 4, 'Write Access'      , 1, key.AltW };
    Radio1^[1] := RADIO{ 4, 5, 'Read/Write Access' , 1, key.AltR };

    NEW(Radio2, 3);
    Radio2^[0] := RADIO{ 30, 4, 'Byte'  , 1, key.AltB };
    Radio2^[1] := RADIO{ 30, 5, 'Word'  , 2, key.AltO };
    Radio2^[2] := RADIO{ 30, 6, 'DWord' , 1, key.AltD };

    NEW(Lines,4);
    Lines^[0] := LINE{ 2, 2, std.radio, ' Access kind ' , 24, 5, 0, 0,  Radio1, Dummy, std.d_enabled};
    Lines^[1] := LINE{ 28, 2, std.radio, ' Length ' , 12, 5, 0, 0, Radio2, Dummy, std.d_enabled};
    Lines^[2] := LINE{ 3, 9, std.msg, 'Location', std.d_enabled };
    Lines^[3] := LINE{ 12, 9, std.edit_str, sys.ADR(address), 28, std.d_enabled };

    CondDialog := win.RegisterWindow(std.DialogProc, SIZE(std.DIALOG));
    ASSERT(CondDialog # win.Invalid_H);
    size.x1 := 18; size.y1 := 5;
    size.x2 := 60; size.y2 := 18;
    win.SetWindowSize(CondDialog,size);
    win.SetModal (CondDialog);
    win.SetMovable (CondDialog);
    win.SetHeaderByStr (CondDialog, act.ActionName[act.Access]);
    p := win.GetAMPtr(CondDialog);
    p^.curr     := 0;
    p^.on_error := std.ErrorMsg;
    p^.Lines    := Lines;
    p^.action   := AddAccessBreak;
  END;
END InitAccess;


PROCEDURE SetAccessAttr (location-: ARRAY OF CHAR; len: CARDINAL);
VAR
  p : std.PDIALOG;
BEGIN
  InitAccess;
  COPY(location, address);
  p := win.GetAMPtr(CondDialog);
  WITH p^ DO
    Lines^[0].ractive := 0;
    CASE len OF
    | 2 : Lines^[1].ractive := 1;
    | 4 : Lines^[1].ractive := 2;
    ELSE
      Lines^[1].ractive := 0;
    END;
  END;
END SetAccessAttr;


PROCEDURE AccessBreak (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Access);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  InitAccess;
  IF mode = act.mode_silent THEN
    IF AddAccessBreak (win.Invalid_H) THEN
      deve.AddToTail (win.ActiveWindow, deve.Redraw, 0);
    ELSE
      deve.AddToTail (std.ErrorMsg, deve.Rise, 0);
      deve.Flush;
      IF Dialog THEN
        deve.AddToTail (CondDialog, deve.Rise, 0);
      END;
    END;
  ELSIF mode # act.mode_check THEN
    deve.AddToTail (CondDialog, deve.Rise, 0);
  END;
  RETURN TRUE;
END AccessBreak;

<* END *>



PROCEDURE Enable_Disable (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  w   : win.HWND;
  ind : CARDINAL;
  addr: kt.ADDRESS;
BEGIN
  w := men.SkipMenus();
  IF w # std.Wnds[std.MainWindow].hwnd THEN RETURN FALSE; END;
  WITH mod.Curr^ DO
    IF dv.MainMode = dv.source THEN
      ind := FindBreakPos(Pos.ComN, Pos.ModN, curr);
    ELSE
      ASSERT(dsm.GetAddr(dsm.D_curr, addr));
      ind := FindBreakPosByAddr(addr);
    END;
    IF ind = MAX(CARDINAL) THEN
      RETURN FALSE;
    ELSE
      WITH brk.Breakpoints.Breakpoints^[ind].Break DO
        CASE action OF
        | act.Enable : IF mode = act.mode_check THEN
                         RETURN NOT Active;
                       ELSE
                         RefreshBreaks;
                         RETURN brk.Enable_Breakpoint(ind);
                       END;
        | act.Disable: IF mode = act.mode_check THEN
                         RETURN Active;
                       ELSE
                         RefreshBreaks;
                         RETURN brk.Disable_Breakpoint(ind);
                       END;
        ELSE
          ASSERT(FALSE);
        END;
      END;
    END;
  END;
END Enable_Disable;



PROCEDURE EnableDisableAllPoints (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  r: BOOLEAN;

  PROCEDURE Breakpoints (): BOOLEAN;
  VAR
    i: CARDINAL;
    r: BOOLEAN;
  BEGIN
    WITH brk.Breakpoints DO
      IF free = 0 THEN RETURN FALSE; END;
      IF mode = act.mode_check THEN
        r := FALSE;
        FOR i := 0 TO free-1 DO
          CASE action OF
          | act.EnableAll:
            r := r OR NOT Breakpoints^[i].Break.Active;
          | act.DisableAll:
            r := r OR Breakpoints^[i].Break.Active;
          END;
        END;
        RETURN r;
      ELSE
        RefreshBreaks;
        FOR i := 0 TO free-1 DO
          WITH Breakpoints^[i] DO
            CASE action OF
            | act.EnableAll:
              IF NOT Break.Active THEN
                IF brk.Enable_Breakpoint(i) THEN
                  Break.Active := TRUE;
                END;
              END;
            | act.DisableAll:
              IF Break.Active THEN
                IF brk.Disable_Breakpoint(i) THEN
                  Break.Active := FALSE;
                END;
              END;
            END;
          END;
        END;
        RETURN TRUE;
      END;
    END;
  END Breakpoints;


  PROCEDURE Accesss (): BOOLEAN;
  VAR
    i: CARDINAL;
    r: BOOLEAN;
  BEGIN
    WITH brk.AccessBreaks DO
      IF free = 0 THEN RETURN FALSE; END;
      IF mode = act.mode_check THEN
        r := FALSE;
        FOR i := 0 TO free-1 DO
          CASE action OF
          | act.EnableAll:
            r := r OR NOT AccessBreaks^[i].Break.Active;
          | act.DisableAll:
            r := r OR AccessBreaks^[i].Break.Active;
          END;
        END;
        RETURN r;
      ELSE
        RefreshBreaks;
        FOR i := 0 TO free-1 DO
          WITH AccessBreaks^[i] DO
            CASE action OF
            | act.EnableAll:
              IF NOT Break.Active THEN
                CASE Access_Data.Access_ID OF
                | brk.Memory:
                  IF kmem.SetTrace (Access_Type, Access_Data.Location, Access_Data.Len, Break.Index) THEN
                    AccessBreaks^[i].Break.Active := TRUE;
                  END;
               <* IF TARGET_VAX THEN *>
                | brk.Register:
                  IF kmem.SetRegTrace (Access_Type, Access_Data.Reg_No, Break.Index) THEN
                    AccessBreaks^[i].Break.Active := TRUE;
                  END;
                | brk.Port:
               <* END *>
                END;
              END;
            | act.DisableAll:
              IF Break.Active THEN
                CASE Access_Data.Access_ID OF
                | brk.Memory:
                  IF kmem.RemoveTrace (Break.Index) THEN
                    AccessBreaks^[i].Break.Active := FALSE;
                  END;
               <* IF TARGET_VAX THEN *>
                | brk.Register:
                  IF kmem.RemoveRegTrace (Break.Index) THEN
                    AccessBreaks^[i].Break.Active := FALSE;
                  END;
                | brk.Port:
               <* END *>
                END;
              END;
            END;
          END;
        END;
        RETURN TRUE;
      END;
    END;
  END Accesss;


  PROCEDURE ConditionBreaks (): BOOLEAN;
  VAR
    i: CARDINAL;
    r: BOOLEAN;
  BEGIN
    WITH brk.ConditionBreaks DO
      IF free = 0 THEN RETURN FALSE; END;
      IF mode = act.mode_check THEN
        r := FALSE;
        FOR i := 0 TO free-1 DO
          CASE action OF
          | act.EnableAll:
            r := r OR NOT ConditionBreaks^[i].Break.Active;
          | act.DisableAll:
            r := r OR ConditionBreaks^[i].Break.Active;
          END;
        END;
        RETURN r;
      ELSE
        RefreshBreaks;
        FOR i := 0 TO free-1 DO
          WITH ConditionBreaks^[i] DO
            CASE action OF
            | act.EnableAll:
              Break.Active := TRUE;
            | act.DisableAll:
              Break.Active := FALSE;
            END;
          END;
        END;
        RETURN TRUE;
      END;
    END;
  END ConditionBreaks;


BEGIN
  ASSERT((action = act.EnableAll) OR (action = act.DisableAll));
  IF mode = act.mode_check THEN
    RETURN Breakpoints() OR Accesss() OR ConditionBreaks();
  ELSE
    r := Breakpoints();
    r := Accesss() AND r;
    r := ConditionBreaks() AND r;
    RETURN r;
  END;
END EnableDisableAllPoints;


PROCEDURE EraseAllPoints (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.EraseAll);
  IF (brk.Breakpoints.free = 0) AND (brk.AccessBreaks.free = 0) AND (brk.ConditionBreaks.free = 0) THEN
    RETURN FALSE;
  END;
  IF mode # act.mode_check THEN
<* PUSH *>
<* WOFF903+ *>
    IF brk.DeleteAll_Breakpoint() THEN END;
<* POP *>
    brk.DeleteAll_AccessBreak();
    ASSERT(brk.DeleteAll_ConditionBreak());
    deve.AddToTail(std.Wnds[std.MainWindow].hwnd, deve.Redraw, 0);
    WITH std.Wnds[std.BrkWindow] DO
      IF (hwnd # win.Invalid_H) AND (win.Visible(hwnd)) THEN
        deve.AddToTail(hwnd, deve.Redraw, 0);
      END;
    END;
  END;
  RETURN TRUE;
END EraseAllPoints;



PROCEDURE BreaksHandler (hwnd: win.HWND; msg: deve.MSG);
VAR
  p    : std.PLIST;
  buf  : xs.String;
  attr : crt.ATTR;
  cnt,len: CARDINAL;
  N1, N2 : CARDINAL;

  PROCEDURE write_line(num: CARDINAL);
  VAR
    str: xs.String;
    l  : CARDINAL;
    mn : xs.txt_ptr;

    <* IF DEST_K26 THEN *>
    zzz: xs.String;
    <* END *>

  BEGIN
    WITH p^ DO
      l    := num - frame  + 1;
      attr := Colors^[crt.List_Line];
      crt.SetPos(10, l);
      crt.WrChar(hwnd, '|', attr);
      crt.SetPos(12, l);
      IF (num < N1)  THEN
        WITH brk.Breakpoints.Breakpoints^[num] DO
          IF tls.IsPosValid(Pos) AND tls.ModName(Pos.ComN, Pos.ModN, mn) THEN
            fmt.print(str, '%-8s', mn^);
          ELSE
            COPY('?????', str);
          END;
          crt.SetPos(2, l);
          crt.WrStr(hwnd, str, attr);
          IF Break.Active THEN
            attr := crt.Attr(crt.Fg(crt.Src[crt.Src_Break]), crt.Bg(attr));
          ELSE
            attr := crt.Attr(crt.Fg(crt.Src[crt.Src_BreakDisabled]), crt.Bg(attr));
          END;
          IF Break.Pass > 0 THEN
            cnt := Break.Pass;
            crt.SetPos(11, l);
            CASE cnt OF
            | 0..9999:
              fmt.print(str, '%4u', cnt);
              crt.WrStr(hwnd, str, attr);
            ELSE
              crt.WrStr(hwnd, '****', attr);
            END;
          END;
          crt.SetPos(15, l);
          IF Break.Sticky THEN
            CASE Kind OF
            | brk.counter:
              crt.WrChar(hwnd, '.', attr);
            | brk.watchpoint:
              crt.WrChar(hwnd, CHAR(9), attr);
            | brk.normal:
              IF Condition # NIL THEN
                crt.WrChar(hwnd, '?', attr);
              ELSE
                crt.WrChar(hwnd, CHAR(4), attr);
              END;
            END;
          ELSE
            crt.WrChar(hwnd, CHAR(16), attr);
          END;
          crt.SetPos(17, l);
          IF Break.Active THEN
            attr := crt.Attr(crt.Fg(crt.Src[crt.Src_Code]), crt.Bg(attr));
          ELSE
            attr := crt.Attr(crt.Fg(crt.Src[crt.Src_NoCode]), crt.Bg(attr));
          END;
          IF Line # 0 THEN
            mn := tls.GetSourceLine(Pos.ComN, Pos.ModN, Line-1);
            crt.WrStrFromPos(hwnd, mn^, attr, pos);
          ELSIF exe.DisasmInstr(Addr, FALSE, buf, str, len) THEN
            fmt.print(str, '%$8X %s', Addr, buf);
            crt.WrStrFromPos(hwnd, str, attr, pos);
          ELSE
            COPY('Memory is not accessible', str);
            crt.WrStrFromPos(hwnd, str, attr, pos);
          END;
        END;
      ELSIF (num < N2) THEN
        WITH brk.AccessBreaks.AccessBreaks^[num-N1] DO
          IF eve.Read = Access_Type THEN

           <* IF DEST_K26 THEN *>
            CASE Access_Data.Access_ID OF
            | brk.Memory:
              pro.GetMsg(mes.BreakLineReadMemory, zzz);
              fmt.print(buf, zzz , Access_Data.Location, Access_Data.Location+Access_Data.Len-1);
            | brk.Register:
              pro.GetMsg(mes.BreakLineReadRegister, zzz);
              fmt.print(buf, zzz, kt.Registers[Access_Data.Reg_No].name);
            END;
            <* ELSE*>
            ASSERT( Access_Data.Access_ID = brk.Memory );
            pro.GetMsg(mes.BreakLineReadMemory, str);
            fmt.print(buf, str, Access_Data.Location, Access_Data.Location+Access_Data.Len-1);
            <* END *>

          ELSE

            <* IF DEST_K26 THEN *>
            CASE Access_Data.Access_ID OF
            | brk.Memory:
              pro.GetMsg(mes.BreakLineWriteMemory, zzz);
              fmt.print(buf, zzz, Access_Data.Location, Access_Data.Location+Access_Data.Len-1);
            | brk.Register:
              pro.GetMsg(mes.BreakLineWriteRegister, zzz);
              fmt.print(buf, zzz, kt.Registers[Access_Data.Reg_No].name);
            END;
            <* ELSE *>

            ASSERT( Access_Data.Access_ID = brk.Memory );
            pro.GetMsg(mes.BreakLineWriteMemory, str);
            fmt.print(buf, str, Access_Data.Location, Access_Data.Location+Access_Data.Len-1);
            <* END *>

          END;

          IF NOT Break.Active THEN
            attr := crt.Attr(crt.Fg(crt.Src[crt.Src_BreakDisabled]), crt.Bg(attr));;
          END;
          crt.WrStrFromPos(hwnd, buf, attr, pos);
        END;
      ELSIF (num < N) THEN
        WITH brk.ConditionBreaks.ConditionBreaks^[num - N2] DO
          pro.GetMsg(mes.BreakLineRelation, str);
          fmt.print(buf, str, Expr^);
          IF NOT Break.Active THEN
            attr := crt.Attr(crt.Fg(crt.Src[crt.Src_BreakDisabled]), crt.Bg(attr));;
          END;
          crt.WrStrFromPos(hwnd, buf, attr, pos);
        END;
      END;
    END;
  END write_line;

VAR
  size    : crt.SZ;
  i, z    : CARDINAL;
  last    : CARDINAL;
  action  : act.ACTION;

<* IF DEST_XDS THEN *>
  location: xs.String;
<* END *>

BEGIN
  N1 := 0;
  N2 := 0;
  p := win.GetAMPtr(hwnd);
  CASE msg.ID OF
  | deve.Redraw, deve.Paint:
    size := win.GetWindowSize(hwnd);
    IF ( msg.par # 3) AND (msg.par # 4) THEN
      crt.FillWindow(hwnd, ' ', p^.Colors^[crt.List_Background])
    END;
    WITH size DO len := y2 - y1 - 1; END;
    WITH p^ DO
      IF kexe.Loaded THEN
        N1 := brk.Breakpoints.free;
        N2 := N1 + brk.AccessBreaks.free;
        N  := N2 + brk.ConditionBreaks.free;
      ELSE
        N  := 0;
      END;
      IF N > 0 THEN
        CASE msg.par OF
        | 3:
          IF hwnd = win.ActiveWindow THEN
            attr := crt.Attr(crt.Fg(Colors^[crt.List_Line]), crt.Bg(crt.List[crt.List_CurrentLine]));
            write_line(curr);
            crt.Hilite(hwnd, curr - frame + 1 , 1, crt.Bg(Colors^[crt.List_CurrentLine]));
          END;
        | 4:
          IF hwnd = win.ActiveWindow THEN
            crt.Lite(hwnd, curr-frame+1 , 1, Colors^[crt.List_Background]);
            attr := Colors^[crt.List_Line];
            write_line(curr);
          END;
        ELSE
          last := std.Min(frame+len-1, N-1);
          FOR i:= frame TO last DO
            IF (i = curr) AND (hwnd = win.ActiveWindow) THEN
              attr := crt.Attr(crt.Fg(Colors^[crt.List_Line]), crt.Bg(crt.List[crt.List_CurrentLine]))
            ELSE
              attr := Colors^[crt.List_Line];
            END;
            write_line(i)
          END;
          IF (hwnd = win.ActiveWindow) THEN
            crt.Hilite(hwnd, curr - frame + 1 , 1, crt.Bg(Colors^[crt.List_CurrentLine]));
          END;
        END;
      END;
    END;
    std.ListBox(hwnd, msg);

  | deve.DoAction, deve.QueryAction:
    WITH p^ DO
      IF kexe.Loaded THEN
        N1 := brk.Breakpoints.free;
        N2 := N1 + brk.AccessBreaks.free;
        N  := N2 + brk.ConditionBreaks.free;
      ELSE
        N := 0;
      END;
      action := act.ACTION(msg.par MOD 100H);
      CASE action OF
      | act.Enable, act.Disable:
        IF N > 0 THEN
          IF curr < N1 THEN
            WITH brk.Breakpoints.Breakpoints^[curr] DO
              WITH Break DO
                IF msg.ID = deve.DoAction THEN
                  IF NOT Active AND (act.ACTION(msg.par MOD 100H) = act.Enable) THEN
                    IF NOT brk.Enable_Breakpoint(curr) THEN crt.Beep; END;
                  ELSIF Active AND (act.ACTION(msg.par MOD 100H) = act.Disable) THEN
                    IF NOT brk.Disable_Breakpoint(curr) THEN crt.Beep; END;
                  ELSE  
                    crt.Beep;
                  END;
                ELSE
                  IF (action = act.Disable) THEN
                    act.ConfirmQueryByCond(Active AND mem.Get(Addr, sys.ADR(z), 1));
                  ELSE
                    act.ConfirmQueryByCond(NOT Active);
                  END;
                END;  
              END;
            END;
          ELSIF curr < N2 THEN
            WITH brk.AccessBreaks.AccessBreaks^[curr-N1] DO
              IF (action = act.Disable) THEN
                act.ConfirmQueryByCond(Break.Active);
              ELSE
                act.ConfirmQueryByCond(NOT Break.Active);
              END;
              IF msg.ID = deve.DoAction THEN
                IF act.ACTION(msg.par MOD 100H) = act.Enable THEN
                  IF NOT Break.Active THEN
                    ASSERT(Access_Data.Access_ID = brk.Memory);
                    IF kmem.SetTrace(Access_Type, Access_Data.Location, Access_Data.Len, Break.Index) THEN
                      Break.Active := TRUE;
                    ELSE
                      crt.Beep;
                    END;
                  END;
                ELSE
                  IF Break.Active THEN
                    Break.Active := FALSE;
                    ASSERT(kmem.RemoveTrace(Break.Index));
                  END;
                END;
              END;
            END;
          ELSE
            WITH brk.ConditionBreaks.ConditionBreaks^[curr-N2].Break DO
              IF (action = act.Disable) THEN
                act.ConfirmQueryByCond(Active);
              ELSE
                act.ConfirmQueryByCond(NOT Active);
              END;
              IF msg.ID = deve.DoAction THEN
                Active := (act.ACTION(msg.par MOD 100H) = act.Enable);
              END;
            END;
          END;
          IF msg.ID = deve.DoAction THEN
            deve.AddToTail(hwnd, deve.Redraw,  0);
            deve.AddToTail(std.Wnds[std.MainWindow].hwnd, deve.Redraw,  0);
          END;
        END;
      ELSE
        std.ListBox (hwnd, msg);
      END;
    END;

  | deve.KbHit, deve.QueryKbHit:
    WITH p^ DO
      IF kexe.Loaded THEN
        N1 := brk.Breakpoints.free;
        N2 := N1 + brk.AccessBreaks.free;
        N  := N2 + brk.ConditionBreaks.free;
      ELSE
        N := 0;
      END;
      CASE msg.par OF
      | key.Del:
        IF msg.ID = deve.QueryKbHit THEN
          act.ConfirmQueryByCond(act.QueryAction(act.Delete));
        ELSE
          act.ExecuteAction (act.Delete, act.mode_silent);
        END;

     <* IF DEST_XDS THEN *>
      | key.Enter, key.AltEnter:
        IF N = 0 THEN RETURN; END;
        IF curr < N1 THEN
          WITH brk.Breakpoints.Breakpoints^[curr] DO
            IF NOT mem.Get (Addr, sys.ADR(z), 1) THEN RETURN; END;
            IF msg.ID = deve.QueryKbHit THEN
              IF msg.par = key.Enter THEN
                act.ConfirmQuery;
              END;
              RETURN;
            END;
            IF mod.SetNewPosByAddr (Addr) THEN;
              deve.AddToTail (std.Wnds[std.MainWindow].hwnd, deve.Rise, 0);
            ELSE
              crt.Beep;
            END;
          END;
        ELSIF curr < N2 THEN
          IF msg.ID = deve.QueryKbHit THEN
            IF msg.par = key.AltEnter THEN
              act.ConfirmQuery;
            END;
            RETURN;
          END;
          Curr := curr - N1;
          WITH brk.AccessBreaks.AccessBreaks^[Curr] DO
            fmt.print (location, exp.Fmt_ADDRval, Access_Data.Location);
            SetAccessAttr (location, Access_Data.Len);
          END;
          deve.AddToTail (CondDialog, deve.Rise, 0);
        ELSIF curr < N THEN
          IF msg.ID = deve.QueryKbHit THEN
            IF msg.par = key.AltEnter THEN
              act.ConfirmQuery;
            END;
            RETURN;
          END;
          Curr := curr - N2;
          COPY (brk.ConditionBreaks.ConditionBreaks^[Curr].Expr^, condition);
          std.OpenUniversalDialog(mes.InputCondition, AddConditionBreak, condition);
        END;
<* END *>
      ELSE
        std.ListBox (hwnd, msg);
      END;

    END;

  ELSE
    std.ListBox (hwnd, msg);
  END;
END BreaksHandler;


PROCEDURE InitWindow(show: BOOLEAN);
VAR
  p: std.PLIST;
BEGIN
  WITH std.Wnds[std.BrkWindow] DO
    IF hwnd = win.Invalid_H THEN
      hwnd := win.RegisterWindow(BreaksHandler,SIZE(std.LIST));
      ASSERT(hwnd # win.Invalid_H);

      win.SetWindowSize(hwnd,size);

      win.SetMovable(hwnd);
      win.SetResizable(hwnd, TRUE, TRUE);
      win.SetSwitchable(hwnd);
      win.SetHeader(hwnd,  mes.Breaks);

      p := win.GetAMPtr(hwnd);
      p^ := std.EmptyList;
      WITH p^ DO
        Colors    := sys.ADR(crt.List);
        Frame     := crt.Double;
        locator   := NIL;
        actions[0]:= act.CONTEXT{ act.do_action, act.Enable };
        actions[1]:= act.CONTEXT{ act.do_action, act.Disable };
        actions[2]:= act.CONTEXT{ act.do_action, act.Delete };
        actions[3]:= act.CONTEXT{ act.separate };
        actions[4]:= act.CONTEXT{ act.push_key, 'Go to', key.Enter };
        actions[5]:= act.CONTEXT{ act.push_key, 'Modify', key.AltEnter };
        actions[6]:= act.EMPTY_CONTEXT;
      END;
    END;
    IF show THEN deve.AddToTail(hwnd, deve.Rise, 0); END;
  END;
END InitWindow;


PROCEDURE InitList (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.ViewAll);
  IF mode # act.mode_check THEN
    InitWindow(TRUE);
  END;
  RETURN TRUE;
END InitList;


VAR
  TmpCom : dt.ComNo;
  TmpMod : dt.ModNo;
  BreakNo: CARDINAL;


PROCEDURE WriteAccessBreak (out: dlt.OUT_PROC; Access_Break: brk.ACCESS_BREAK; new: BOOLEAN);
VAR
  name: xs.txt_ptr;
  com : dt.ComNo;
  mod : dt.ModNo;
  s   : xs.String;
  tmp : xs.String;
  addr: kt.ADDRESS;
  obj : dt.OBJECT;
  var : BOOLEAN;
  tmp2: xs.txt_ptr;
BEGIN
  IF new THEN
    TmpCom := dt.Invalid_Component;
    TmpMod := dt.Invalid_Module;
  END;
  WITH Access_Break DO
    WITH Access_Data DO
      CASE Access_ID OF
      | brk.Memory:
        fmt.print (s, '0x%$8X, %i', Location, Len);
        IF tls.FindComponentByAddr (Location, com) THEN
          var := FALSE;
          obj := tls.FindVarByAddr (com, Location, FALSE);
          IF tls.IsObjectValid (obj) THEN
            var := TRUE;
          ELSE
            IF tls.FindModInCompByAddr (com, Location, mod) THEN
              obj := tls.FindProcByAddr (com, mod, Location);
            ELSE
              obj := dt.Invalid_Object;
            END;
          END;
          IF tls.IsObjectValid (obj) THEN
            mod := tls.ObjectMod (obj);
            IF (TmpCom # com) OR (TmpMod # mod) THEN
              ASSERT(tls.ComName(com, name));
              out ('  MODULE %s', name^);
              TmpCom := com;
              ASSERT(tls.ModName(com, mod, name));
              out (', %s\n', name^);
              TmpMod := mod;
            END;
            ASSERT(tls.ObjectAddr (obj, addr));
            tls.ObjectName (obj, tmp);
            IF var THEN
              IF Location = addr THEN
                 fmt.print (s, 'ADR(%s), %i', tmp, Len);
              ELSE
                fmt.print (s, 'ADR(%s)+%i, %i', tmp, Location-addr, Len);
              END;
            ELSE
              IF Location = addr THEN
                 fmt.print (s, '%s, %i', tmp, Len);
              ELSE
                fmt.print (s, '%s+%i, %i', tmp, Location-addr, Len);
              END;
            END;
          ELSIF tls.FindPublicByAddrInCom (com, Location, FALSE, name) THEN
            IF TmpCom # com THEN
              ASSERT(tls.ComName(com, tmp2));
              out ('  MODULE %s\n', tmp2^);
              TmpCom := com;
              TmpMod := dt.Invalid_Module;
            END;
            ASSERT(tls.FindPublicByNameInCom (com, name^, addr));
            IF Location = addr THEN
              fmt.print (s, '%s, %i', name^, Len);
            ELSE
              fmt.print (s, '%s+%i, %i', name^, Location-addr, Len);
            END;
          END;
        END;
        INC(BreakNo);
        CASE Access_Type OF
        | eve.Read, eve.ReadWrite:
          out ('  BREAK a%-2i, READ,  %-14s', BreakNo, s);
        | eve.Write:
          out ('  BREAK a%-2i, WRITE, %-14s', BreakNo, s);
        END;
     <* IF TARGET_VAX THEN *>
      | brk.Register:
        ASSERT(exe.GetRegName (Reg_No, s));
        INC(BreakNo);
        CASE Access_Type OF
        | eve.Read, eve.ReadWrite:
          out ('  BREAK a%-2i, READ,  @%s', BreakNo, s);
        | eve.Write:
          out ('  BREAK a%-2i, WRITE, @%s', BreakNo, s);
        END;
      | brk.Port:
        IF bas.CheckMode(pt.KodSuccess) THEN
          INC(BreakNo);
          out ('  BREAK a%-2i, IO', BreakNo);
          mdl.DeviceName (Dev_No, s);
          out (' %s,', s);
          mdl.RegisterName (Dev_No, Port_No, s);
          out (' %s,', s);
        END;
     <* END *>
      END;
      out (', %s\n', dlt.Switch_To_Dialog);
    END;
  END;
END WriteAccessBreak;


PROCEDURE WriteBreaks (out: dlt.OUT_PROC);
VAR
  i   : CARDINAL;
  name: xs.txt_ptr;
  TmpCom: dt.ComNo;
  TmpMod: dt.ComNo;
  str, proc_name: xs.txt_ptr;
  proc: dt.OBJECT;
  TmpProc: dt.OBJECT;
  start, end: kt.ADDRESS;
BEGIN
  WITH brk.Breakpoints DO
    IF free > 0 THEN
      TmpCom := dt.Invalid_Component;
      TmpMod := dt.Invalid_Module;
      TmpProc := dt.Invalid_Object;
      FOR i := 0 TO free-1 DO
        WITH Breakpoints^[i] DO
          IF tls.IsPosValid (Pos) AND (Line # 0) THEN
            IF TmpCom # Pos.ComN THEN
              ASSERT (tls.ComName(Pos.ComN, name));
              out ('  MODULE %s\n', name^);
              TmpCom := Pos.ComN;
              TmpMod := dt.Invalid_Module;
            END;
            IF TmpMod # Pos.ModN THEN
              ASSERT (tls.ModName(Pos.ComN, Pos.ModN, name));
              out ('  MODULE %s\n', name^);
              TmpMod := Pos.ModN;
            END;
            ASSERT (tls.ModName(Pos.ComN, Pos.ModN, name));
            proc := tls.FindProcByAddr (Pos.ComN, Pos.ModN, Addr);
            IF NOT tls.IsObjectValid (proc) OR tls.IsObjectValid (tls.ObjectParentScope(proc)) THEN
              out ('  BREAK %s_%i, LINE, %s, %i', name^, i+1, name^, Line);
            ELSE
              tls.Object_pName(proc, proc_name);
              tls.ProcAttr (proc, start, end);
              IF Addr < start THEN
                out ('  BREAK %s_%s_Begin, PROC, %s, %s', name^, proc_name^, proc_name^, pt.ProcedureParts[pt.Prologue]);
              ELSIF Addr >= end THEN
                out ('  BREAK %s_%s_End, PROC, %s, %s', name^, proc_name^, proc_name^, pt.ProcedureParts[pt.Epilogue]);
              ELSE
                IF NOT tls.EqualObjects (proc, TmpProc) THEN
                  out ('; Procedure %s\n', proc_name^);
                END;
                ASSERT (tls.ModName(Pos.ComN, Pos.ModN, name));
                out ('  BREAK %s_%i, LINE, %s, %i', name^, i+1, name^, Line);
              END;
            END;
            TmpProc := proc;
            str := tls.GetSourceLine(Pos.ComN, Pos.ModN, Line-1);
          ELSE
            out ('  BREAK b%-2i, ADDR, 0x%$8X     ', i+1, Addr);
            str := NIL;
          END;
          CASE Kind OF
          | brk.counter:
            out (', '+pt.Counter);
          | brk.watchpoint:
            out (', '+pt.Watchpoint);
          | brk.normal:
            out (', %s', dlt.Switch_To_Dialog);
            IF NOT Break.Sticky THEN
              out (', '+pt.Delayed);
            END;
            IF init_value # 0 THEN
              out (', %d', init_value);
            END;
            IF Condition # NIL THEN
              out (', '+pt.Condition+', %s', Condition^);
            END;
          END;
          IF str # NIL THEN
            out ('; %s', str^);
          END;
          out ('\n');
        END;
      END;
      out ('\n');
    END;
  END;
  WITH brk.AccessBreaks DO
    IF free > 0 THEN
      FOR i := 0 TO free-1 DO
        WriteAccessBreak (out, AccessBreaks^[i], i=0);
      END;
      out ('\n');
    END;
  END;
  WITH brk.ConditionBreaks DO
    IF free > 0 THEN
      FOR i := 0 TO free-1 DO
        WITH ConditionBreaks^[i] DO
          out ('  BREAK c%-2i, COND, %-15s, %s\n', i+1, Expr^, dlt.Switch_To_Dialog);
        END;
      END;
      out ('\n');
    END;
  END;
END WriteBreaks;



BEGIN
  TmpCom := dt.Invalid_Component;
  TmpMod := dt.Invalid_Module;
  BreakNo := 0;

  std.Wnds[std.BrkWindow].init    := InitWindow;

  CondDialog  := win.Invalid_H;

  act.IniAction(act.Delete,        DeletePoint);
  act.IniAction(act.Enable,        Enable_Disable);
  act.IniAction(act.Disable,       Enable_Disable);
  act.IniAction(act.OneTime,       SetBreak);
  act.IniAction(act.Sticky,        SetBreak);
  act.IniAction(act.Counter,       SetBreak);
  act.IniAction(act.Watchpoint,    SetBreak);
  act.IniAction(act.D_Sticky,      SetDelayedBreak);
  act.IniAction(act.D_OneTime,     SetDelayedBreak);
  act.IniAction(act.ViewAll,       InitList);
  act.IniAction(act.ExprPoint,     BreakByCond);
  act.IniAction(act.DisableAll,    EnableDisableAllPoints);
  act.IniAction(act.EnableAll,     EnableDisableAllPoints);
  act.IniAction(act.EraseAll,      EraseAllPoints);
 <* IF DEST_K26 THEN *>
  act.IniAction(act.ConditionBreak, Cond);
 <* ELSE *>
  act.IniAction(act.Access,    AccessBreak);
  act.IniAction(act.Condition, ConditionBreak);
 <* END *>
END DlgBreak.
