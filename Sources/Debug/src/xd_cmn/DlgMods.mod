<* Storage+ *>
IMPLEMENTATION MODULE DlgMods;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT kt  := KrnTypes;
IMPORT kexe:= KrnExec;

IMPORT exe := ExeMain;
IMPORT mem := Exe_Mem;

IMPORT opt := Options;
IMPORT key := Keys;
IMPORT pro := Protocol;
IMPORT mes := MsgNo;
IMPORT xs  := xStr;

IMPORT exp := Expr;

IMPORT crt := CRT;
IMPORT dlg := Dialog;
IMPORT eve := DlgEvent;
IMPORT puw := DlgPopup;
IMPORT win := Dlg_Win;
IMPORT dsm := Dlg_Dasm;
IMPORT act := Dlg_Acts;
IMPORT std := Dlg_Std;
IMPORT dv  := Dlg_Vars;


IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;

IMPORT nm  := Names;



VAR
  ZeroModule: MOD_REC;

CONST
  EmptyModule = MOD_REC{ dt.Invalid_Position, enabled, 0, 0, 0, 0, 0, 0, 0, 0, 0};

TYPE
  KEY = POINTER TO ARRAY OF CARDINAL;

  PMODS = POINTER TO ARRAY OF MOD_REC;

  COMP = RECORD
           CompNo    : CARDINAL;
           curr_mod  : CARDINAL;
           curr_addr : CARDINAL;
           Modules   : PMODS;
           mod_curr, mod_frame: CARDINAL;
           pub_curr, pub_frame: CARDINAL;
           K_DebugInfo, K_All: KEY;
         END;

  COMPS = POINTER TO ARRAY OF COMP;
  COMPONENTS = RECORD
                 Count: CARDINAL;
                 Comps: COMPS;
               END;

CONST
  EmptyComp = COMP{ dt.Invalid_Component, dt.Invalid_Module, 0, NIL, 0, 0, 0, 0, NIL, NIL};

VAR
  Key: KEY;
  Components: COMPONENTS;

  Curr_ComNo, Curr_ModNo: CARDINAL;
  Curr_ComNo_int, Curr_ModNo_int: CARDINAL;


(* возвращает номер компоненты если для нее была построена структура *)
PROCEDURE IniModules(com: CARDINAL);
CONST
  NC = 64;
VAR
  i, j, N: CARDINAL;
BEGIN
  WITH Components DO
    IF Comps = NIL THEN
      NEW(Comps, NC);
      Count := 0;
    ELSIF Count = HIGH(Comps^) THEN
      ASSERT(FALSE);
    END;
    Comps^[Count] := EmptyComp;
    WITH Comps^[Count] DO
      CompNo := com;

      N := 0;
      FOR i := 1 TO dt.Components.Components^[com].DI.LastModule DO
        IF tls.ModHaveDebugInfo(com, i) THEN
          INC(N);
        END;
      END;

      IF N # 0 THEN
        NEW(K_DebugInfo, N);
      ELSE
        K_DebugInfo := NIL;
      END;

      N := dt.Components.Components^[com].DI.LastModule;
      IF N # 0 THEN
        NEW(K_All, N);
        NEW(Modules, N);
        j := 0;
        FOR i := 0 TO N-1 DO
          Modules^[i] := EmptyModule;
          WITH Modules^[i] DO
            Pos.ComN := com;
            Pos.ModN := dt.Components.Components^[com].DI.KModules^[i] + 1;
            IF tls.ModHaveDebugInfo (Pos.ComN, Pos.ModN) THEN
              N := MAX(CARDINAL);
              K_DebugInfo^[j] := i;
              INC(j);
            END;
            K_All^[i] := i;
          END;
        END;
        IF ~ opt.ShowAllModules & (K_DebugInfo # NIL) THEN
          Key := K_DebugInfo;
        ELSE
          Key := K_All;
        END;
      ELSE
        NEW(Modules, 1);
        NEW(K_All,   1);
        K_All^[0]   := 0;
        Modules^[0] := ZeroModule;
        Modules^[0].Pos.ComN := com;
        Key := K_All;
      END;
    END;
    INC(Count);
  END;
END IniModules;

PROCEDURE FindComponent(com: CARDINAL): CARDINAL;
VAR
  i: CARDINAL;
BEGIN
  WITH Components DO
    FOR i := 1 TO Count DO
      IF Comps^[i-1].CompNo = com THEN
        RETURN i-1;
      END;
    END;
  END;
  RETURN dt.Invalid_Component;
END FindComponent;

PROCEDURE FindModInComp(com, mod: CARDINAL): CARDINAL;
VAR
  i: CARDINAL;
BEGIN
  WITH Components.Comps^[com] DO
    FOR i := 0 TO HIGH(K_All^) DO
      IF Modules^[K_All^[i]].Pos.ModN = mod THEN
        RETURN K_All^[i];
      END;
    END;
  END;
  RETURN MAX(CARDINAL);
END FindModInComp;

PROCEDURE SaveForMod(mod_int: CARDINAL);
VAR
  p: std.PLIST;
BEGIN
  WITH Components.Comps^[Curr_ComNo_int].Modules^[mod_int] DO
    IF std.Wnds[std.LblWindow].hwnd # win.Invalid_H THEN
      p := win.GetAMPtr(std.Wnds[std.LblWindow].hwnd);
      l_curr  := p^.curr;
      l_frame := p^.frame;
    ELSE
      l_curr  := 0;
      l_frame := 0;
    END;
    IF std.Wnds[std.ModuleVarWindow].hwnd # win.Invalid_H THEN
      p := win.GetAMPtr(std.Wnds[std.ModuleVarWindow].hwnd);
      v_curr  := p^.curr;
      v_frame := p^.frame;
    ELSE
      v_curr  := 0;
      v_frame := 0;
    END;
    IF dv.MainMode # dv.source THEN
      ASSERT(dsm.GetAddr(dsm.D_curr, dis_addr));
    ELSE
      IF NOT tls.AddrBySource(Pos.ComN, Pos.ModN, curr+1, dis_addr) THEN
        dis_addr := 0;
      END;
    END;
  END;
END SaveForMod;

PROCEDURE SetNewModule(m: CARDINAL);
  PROCEDURE Restore;
  VAR
    p: std.PLIST;
  BEGIN
    WITH Curr^ DO
      IF std.Wnds[std.LblWindow].hwnd # win.Invalid_H THEN
        p := win.GetAMPtr(std.Wnds[std.LblWindow].hwnd);
        p^.curr  := l_curr;
        p^.frame := l_frame;
      END;
      IF std.Wnds[std.ModuleVarWindow].hwnd # win.Invalid_H THEN
        p := win.GetAMPtr(std.Wnds[std.ModuleVarWindow].hwnd);
        p^.curr  := v_curr;
        p^.frame := v_frame;
      END;
    END;
  END Restore;

VAR
  p : std.PLIST;
(*
  size: crt.SZ;
  new_mod_inx: CARDINAL;
*)

BEGIN
  IF m = Curr_ModNo THEN RETURN END;
  IF Curr_ModNo # dt.Invalid_Module THEN
    SaveForMod(Curr_ModNo_int);
  END;
  ASSERT(Curr_ComNo_int # dt.Invalid_Component);
  IF m # dt.Invalid_Module THEN
    Curr_ModNo_int  := FindModInComp(Curr_ComNo_int, m);
    IF Curr_ModNo_int = MAX(CARDINAL) THEN
      Curr_ModNo_int := 0;
    END;
    Curr := sys.ADR(Components.Comps^[Curr_ComNo_int].Modules^[Curr_ModNo_int]);
    Curr_ModNo := Curr^.Pos.ModN;
    Restore;
  ELSE
    Curr_ModNo     := dt.Invalid_Module;
    Curr_ModNo_int := MAX(CARDINAL);
    Curr := sys.ADR(ZeroModule);
  END;
  IF std.Wnds[std.ModWindow].hwnd # win.Invalid_H THEN
    p := win.GetAMPtr(std.Wnds[std.ModWindow].hwnd);
    WITH p^ DO
      frame := 0;
      curr := 0;
(*
      IF (Curr_ComNo_int # dt.Invalid_Component) & (Curr_ModNo_int # dt.Invalid_Module) THEN
        new_mod_inx := FindModInComp(Curr_ComNo_int, Curr_ModNo);
      ELSE
        new_mod_inx := MAX(CARDINAL);
      END;
      IF new_mod_inx = MAX(CARDINAL) THEN
        curr := 0;
      ELSE
        curr := new_mod_inx;
      END;
      size := win.GetWindowSize (std.Wnds[std.ModWindow].hwnd);
      std.Normalize (size, curr, frame, HIGH(Key^)+1);
*)
    END;
  END;
END SetNewModule;

PROCEDURE SetNewComp(com: CARDINAL);

  PROCEDURE SaveForComp(i: CARDINAL);
  VAR
    p   : std.PLIST;
  BEGIN
    WITH Components.Comps^[i] DO
      curr_mod := Curr^.Pos.ModN;
      WITH std.Wnds[std.ModWindow] DO
        IF hwnd # win.Invalid_H THEN
          p := win.GetAMPtr(hwnd);
          WITH p^ DO
            mod_curr  := curr;
            mod_frame := frame;
          END;
        END;
      END;
    <* IF DEST_XDS THEN *>
      WITH std.Wnds[std.PublicsWindow] DO
        IF hwnd # win.Invalid_H THEN
          p := win.GetAMPtr(hwnd);
          WITH p^ DO
            pub_curr  := curr;
            pub_frame := frame;
          END;
        END;
      END;
    <* END *>
      IF dv.MainMode # dv.source THEN
        IF NOT dsm.GetAddr(dsm.D_curr, curr_addr) THEN
          curr_addr := 0;
        END;
      END;
    END;
  END SaveForComp;

  PROCEDURE RestoreForComp(i: CARDINAL);
  VAR
    p   : std.PLIST;
  BEGIN
    WITH Components.Comps^[i] DO
      WITH std.Wnds[std.ModWindow] DO
        IF hwnd # win.Invalid_H THEN
          p := win.GetAMPtr(hwnd);
          WITH p^ DO
            curr  := mod_curr;
            frame := mod_frame;
          END;
        END;
      END;
    <* IF DEST_XDS THEN *>
      WITH std.Wnds[std.PublicsWindow] DO
        IF hwnd # win.Invalid_H THEN
          p := win.GetAMPtr(hwnd);
          WITH p^ DO
            curr  := pub_curr;
            frame := pub_frame;
          END;
        END;
      END;
    <* END *>
    END;
  END RestoreForComp;

BEGIN
  ASSERT(com # dt.Invalid_Component);
  IF com = Curr_ComNo THEN
    RETURN;
  END;
  IF Curr_ComNo_int # dt.Invalid_Component THEN
    SaveForComp(Curr_ComNo_int);
    IF Curr_ModNo_int # MAX(CARDINAL) THEN
      SaveForMod(Curr_ModNo_int);
      SetNewModule(dt.Invalid_Module);
    END;
  END;
  Curr_ComNo_int := FindComponent(com);
  IF Curr_ComNo_int = dt.Invalid_Component THEN
    IniModules(com);
    Curr_ComNo_int := Components.Count - 1;
  END;
  WITH Components.Comps^[Curr_ComNo_int] DO
    Curr_ComNo := com;
    ASSERT(CompNo = com);
    IF ~ opt.ShowAllModules & (K_DebugInfo # NIL) THEN
      Key := K_DebugInfo;
    ELSE
      Key := K_All;
    END;
  END;
  RestoreForComp(Curr_ComNo_int);
END SetNewComp;

(* Устанавливает текущую линию в текущем модуле и позициониpует frame так, чтобы
   ее было видно на экpане *)

PROCEDURE SetCurrLine(ln: CARDINAL);
VAR
  size : crt.SZ;
  len  : CARDINAL;
BEGIN
  IF ln = MAX(CARDINAL) THEN RETURN END;
  size := win.GetWindowSize(std.Wnds[std.MainWindow].hwnd);
  WITH size DO
    len := y2 - y1;
  END;
  WITH Curr^ DO
    IF N = MAX(CARDINAL) THEN
      N := tls.LastSourceLine(Pos.ComN, Pos.ModN);
      IF N = 0 THEN
        N := tls.LastLineHasCode(Pos.ComN, Pos.ModN);
      END;
    END;
    ASSERT(N#MAX(CARDINAL));
    IF (ln < frame) OR (ln > frame+len-1) THEN
      IF ln > 3 THEN
        IF len <= 3 THEN
          frame := ln;
        ELSE
          frame := ln-3;
        END;
        IF frame + len > N THEN
          IF N < len THEN
           frame := 0;
          ELSE
            frame := N - len  + 1;
          END;
        END;
      ELSE
        frame := 0;
      END;
    END;
    curr  := ln;
  END;
  eve.AddToTail(std.Wnds[std.MainWindow].hwnd, eve.Redraw, 0 );
END SetCurrLine;

VAR
  SaveMode: dv.MAINMODE;

PROCEDURE SetNewPos(com, m, ln: CARDINAL);
BEGIN
  ASSERT(dv.MainMode = dv.source);
  SetNewComp(com);
  SetNewModule(m);
  SetCurrLine(ln);
  eve.AddToTail(eve.AllWindows, eve.Redraw, 1);
END SetNewPos;

PROCEDURE SetNewPosByAddrInComp(com: CARDINAL; addr: kt.ADDRESS): BOOLEAN;
VAR
  size  : crt.SZ;
  len   : CARDINAL;
  save_mod, m, ln : CARDINAL;
BEGIN
  dsm.SaveCurrentState;

  dv.RestoreMainMode;
  SaveMode := dv.MainMode;
  save_mod := Curr^.Pos.ModN;

  IF tls.FindModInCompByAddr(com, addr, m) THEN
    SetNewModule(m);
    IF tls.SourceByAddrInMod(com, m, addr, ln) THEN
      IF (dv.MainMode = dv.source) THEN
        SetCurrLine(ln-1);
        RETURN TRUE;
      END;
      -- сохранить информацию о строке
      Curr^.curr := ln-1;
    ELSE
      dv.SetMainMode(dv.need_disasm);
    END
  ELSE
    SetNewModule(dt.Invalid_Module);
    dv.SetMainMode(dv.need_disasm);
  END;
  ASSERT(dv.MainMode # dv.source);
  CASE dv.MainMode OF
  | dv.disasm       : dsm.SetDisasmMode(dsm.D);
  | dv.need_disasm  : dsm.SetDisasmMode(dsm.D);
  | dv.disasm_first : dsm.SetDisasmMode(dsm.DS);
  END;
  size := win.GetWindowSize(std.Wnds[std.MainWindow].hwnd);
  len := size.y2 - size.y1;
  IF NOT dsm.GetPosByAddr(addr, len, dsm.D_curr) THEN
    dv.SetMainMode(SaveMode);
    IF dv.MainMode = dv.source THEN
      SetNewModule(save_mod);
    ELSE
      dsm.RestoreCurrentState;
    END;
    eve.AddToTail(eve.AllWindows, eve.Redraw, 1);
    RETURN FALSE;
  END;
  RETURN TRUE;
END SetNewPosByAddrInComp;

PROCEDURE SetNewPosByAddrNowhere(addr: kt.ADDRESS): BOOLEAN;
VAR
  size     : crt.SZ;
  len      : CARDINAL;
  save_mod : CARDINAL;
BEGIN
  dsm.SaveCurrentState;

  dv.RestoreMainMode;
  SaveMode := dv.MainMode;
  save_mod := Curr^.Pos.ModN;

  SetNewModule(dt.Invalid_Module);
  dv.SetMainMode(dv.need_disasm);
  dsm.SetDisasmMode(dsm.D);
  size := win.GetWindowSize(std.Wnds[std.MainWindow].hwnd);
  len := size.y2 - size.y1;
  IF NOT dsm.GetPosByAddr(addr, len, dsm.D_curr) THEN
    IF save_mod # dt.Invalid_Module THEN
      dv.SetMainMode(SaveMode);
      IF dv.MainMode = dv.source THEN
        SetNewModule(save_mod);
      ELSE
        dsm.RestoreCurrentState;
      END;
    ELSE
      dsm.RestoreCurrentState;
    END;
    RETURN FALSE;
  END;
  RETURN TRUE;
END SetNewPosByAddrNowhere;


PROCEDURE SetNewPosByAddr(addr: kt.ADDRESS): BOOLEAN;
VAR
  com: CARDINAL;
BEGIN
 eve.AddToTail(eve.AllWindows, eve.Redraw, 1);
 IF tls.FindComponentByAddr(addr, com) THEN
    SetNewComp(com);
    RETURN SetNewPosByAddrInComp(com, addr);
  ELSE
    RETURN SetNewPosByAddrNowhere(addr);
  END;
END SetNewPosByAddr;


VAR
  Section_Window : win.HWND;




PROCEDURE SectionsHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p, p2 : std.PLIST;

  PROCEDURE write_line(num: CARDINAL);
  VAR
    l,LEN: CARDINAL;
    buf  : xs.String;
    addr : kt.ADDRESS;
    access: kt.ATTRIBS;
  BEGIN
    WITH p^ DO
      l := num - p^.frame + 1;
      WITH Components.Comps^[Curr_ComNo_int].Modules^[p2^.curr] DO
        ASSERT(tls.SegmentName(Pos.ComN, Pos.ModN, num, buf));
        ASSERT(tls.GetSegmentInfo(Pos.ComN, Pos.ModN, num, addr, LEN));
      END;
      crt.SetPos(2, l);
      fmt.append(buf, ' %$8X', addr);
      fmt.append(buf, ' %$8X', LEN);

      IF mem.GetSegmentInfo(addr,addr,LEN, access) THEN
        IF kt.execute IN access THEN
          xs.Append(' EXECUTE',buf);
        END;
        IF kt.read IN access THEN
          xs.Append(' READ',buf);
        END;
        IF kt.write IN access THEN
          xs.Append(' WRITE',buf);
        END;
        crt.WrStrFromPos(hwnd, buf, Colors^[crt.List_Line], pos);
      END;
    END;
  END write_line;

VAR
  size  : crt.SZ;
  i, len: CARDINAL;
  last  : CARDINAL;

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
      p2 := win.GetAMPtr(std.Wnds[std.ModWindow].hwnd);
      WITH Components.Comps^[Curr_ComNo_int].Modules^[p2^.curr] DO
        p^.N := tls.SegmentsNo(Pos.ComN, Pos.ModN);
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
    CASE msg.par OF
    | key.Esc:
      eve.AddToTail(hwnd, eve.KbHit, key.CtrlF4);
    ELSE
      std.ListBox (hwnd, msg);
    END;
  ELSE
    std.ListBox (hwnd, msg);
  END;
END SectionsHandler;


PROCEDURE InitSectionWindow(size: crt.SZ; show: BOOLEAN);
VAR
  p, p2 : std.PLIST;
  header, fmt_str: ARRAY [0..32] OF CHAR;
  ModName: xs.txt_ptr;
BEGIN
    IF Section_Window = win.Invalid_H THEN
      Section_Window := win.RegisterWindow(SectionsHandler,SIZE(std.LIST));
      ASSERT(Section_Window # win.Invalid_H);

      win.SetWindowSize(Section_Window, size);

      win.SetMovable(Section_Window);
      win.SetResizable(Section_Window, FALSE, TRUE);
      win.SetModal(Section_Window);

      p := win.GetAMPtr(Section_Window);
      p^ := std.EmptyList;
      WITH p^ DO
        Colors    := sys.ADR(crt.List);
      END;
    ELSE
      p := win.GetAMPtr(Section_Window);
      p^.curr  := 0;
      p^.frame := 0;
    END;
    p2 := win.GetAMPtr(std.Wnds[std.ModWindow].hwnd);
    pro.GetMsg(mes.Segments_, fmt_str);
    WITH Components.Comps^[Curr_ComNo_int].Modules^[p2^.curr] DO
      ASSERT(tls.ModName(Pos.ComN, Pos.ModN, ModName));
    END;
    fmt.print(header, fmt_str, ModName^);
    win.SetHeaderByStr(Section_Window, header);
    IF show THEN eve.AddToTail(Section_Window, eve.Rise, 0); END;
END InitSectionWindow;

PROCEDURE InitSectionList;
VAR
  size : crt.SZ;
BEGIN
  size.x1 := 10;  size.y1 := 14;
  size.x2 := 68;  size.y2 := 24;
  InitSectionWindow(size, TRUE);
END InitSectionList;


CONST
  ContextItem_ShowCode        = 0;
  ContextItem_RereadDebugInfo = 4;

PROCEDURE ModulesHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p: std.PLIST;

  PROCEDURE write_line(num: CARDINAL);
  VAR
    ModName: xs.txt_ptr;
  BEGIN
    WITH Components.Comps^[Curr_ComNo_int].Modules^[Key^[num]] DO
      IF (Pos.ComN = exe.ExecComp) AND (Pos.ModN = exe.ExecMod) THEN
        crt.SetPos (1, num-p^.frame + 1);
        crt.WrChar (hwnd, CHAR(16), crt.Attr(crt.BringColor(crt.Bg(crt.Src[crt.Src_ExecCursor])), crt.Bg(p^.Colors^[crt.List_Line])));
      ELSIF (Pos.ComN = Curr^.Pos.ComN) AND (Pos.ModN = Curr^.Pos.ModN) THEN
        crt.SetPos (1, num-p^.frame + 1);
        crt.WrChar (hwnd, CHAR(16), crt.Attr(crt.BringColor(crt.Bg(crt.Src[crt.Src_UserCursor])), crt.Bg(p^.Colors^[crt.List_Line])));
      END;
      crt.SetPos(2, num - p^.frame + 1);
--      IF tls.ModName(Pos.ComN, Pos.ModN, ModName) THEN
--        IF state = enabled THEN
--          crt.WrStr(hwnd, ModName^, p^.Colors^[crt.List_Line]);
--        ELSE
--          crt.WrStr(hwnd, ModName^, crt.Modules_Inactive);
--        END;
      IF tls.ModName (Pos.ComN, Pos.ModN, ModName) THEN
        IF state = enabled THEN
          crt.WrStr(hwnd, ModName^, p^.Colors^[crt.List_Line]);
        ELSE
          crt.WrStr(hwnd, ModName^, crt.Modules_Inactive);
        END;
      ELSE
        ASSERT(num = 0)
      END;
    END;
  END write_line;

VAR
  size  : crt.SZ;
  i, len: CARDINAL;
  last  : CARDINAL;
  main  : BOOLEAN;
  item  : CARDINAL;
  k     : key.KEY;

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
      IF kexe.Loaded AND (Key # NIL) THEN
        N := HIGH(Key^)+1;
      ELSE
        N := 0;
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

  | eve.QueryAction:
    CASE act.ACTION(msg.par MOD 100H) OF
    | act.Modules:
      IF kexe.Loaded AND (Key # NIL) THEN
        act.ConfirmQueryByCond((HIGH(Key^)+1)#0);
        RETURN;
      END;
    | act.ModuleVars:
      IF kexe.Loaded THEN
        WITH Components.Comps^[Curr_ComNo_int].Modules^[Key^[p^.curr]] DO
          IF tls.IsPosValid (Pos) THEN
            act.ConfirmQueryByCond(tls.VarsNo(Pos.ComN, Pos.ModN)#0);
            RETURN;
          END;
        END;
      END;
    | act.ContextMenu:
      act.ConfirmQuery;
    ELSE
      std.ListBox (hwnd, msg);
    END;

  | eve.DoAction:
    CASE std.GetAction (msg.par) OF
    | act.ModuleVars:
      IF kexe.Loaded THEN
        WITH Components.Comps^[Curr_ComNo_int].Modules^[Key^[p^.curr]] DO
          IF tls.IsPosValid (Pos) AND (tls.VarsNo(Pos.ComN, Pos.ModN) # 0) THEN
            dlg.InitSpecialVariableWindow (Pos);
            RETURN;
          END;
        END;
      END;
    | act.ContextMenu:
      size := win.GetWindowSize(hwnd);
      WITH size DO
        puw.PopupWindow(x1+(x2-x1) DIV 2, y1+(y2-y1) DIV 2, hwnd, p^.actions);
      END;
    ELSE
      ASSERT (key.GetKeyByName ('.', k));
      IF std.GetKey (msg.par) = k THEN
        eve.AddToTail (hwnd, eve.KbHit, k);
        RETURN;
      END;
    END;
    std.ListBox (hwnd, msg);

  | eve.QueryItem:
    std.GetItem (msg.par, main, item);
    ASSERT(main);
    CASE item OF
    | ContextItem_ShowCode:
      act.ConfirmQuery;
    | ContextItem_RereadDebugInfo:
      act.ConfirmQueryByCond (Curr_ComNo # dt.Invalid_Component);
    ELSE
      std.ListBox (hwnd, msg);
    END;

  | eve.ContextItem:
    std.GetItem (msg.par, main, item);
    ASSERT(main);
    CASE item OF
    | ContextItem_ShowCode:
      eve.AddToTail (hwnd, eve.KbHit, key.Enter);
      eve.Flush;
    | ContextItem_RereadDebugInfo:
      -- reset debug info processed flag
      tls.ClearDebugProcessed (Curr_ComNo);
      -- reset source file information
      WITH Components DO
        IF Comps # NIL THEN
          WITH Comps^[Curr_ComNo] DO
            IF Modules # NIL THEN
              FOR i := 0 TO HIGH(Modules^) DO
                Modules^[i].N := MAX(CARDINAL);
              END;
            END;
          END;
        END;
      END;
      eve.AddToTail (hwnd, eve.Redraw, 0);
    ELSE
      std.ListBox (hwnd, msg);
    END;

  | eve.KbHit:
    WITH p^ DO
      CASE msg.par OF
      | key.Enter:
        IF N = 0 THEN
          RETURN;
        END;
        p := win.GetAMPtr(hwnd);
        IF tls.SegmentsNo (Curr_ComNo, Components.Comps^[Curr_ComNo_int].Modules^[Key^[p^.curr]].Pos.ModN) = 0 THEN
          crt.Beep;
          RETURN;
        END;
        dv.RestoreMainMode;
        SetNewModule (Components.Comps^[Curr_ComNo_int].Modules^[Key^[p^.curr]].Pos.ModN);

        IF NOT tls.CheckDebugInfoForModule (Curr^.Pos.ComN, Curr^.Pos.ModN) OR
           NOT tls.ModHaveDebugInfo (Curr^.Pos.ComN, Curr^.Pos.ModN)
        THEN
          dv.SetMainMode (dv.need_disasm);
        END;

        IF (dv.MainMode # dv.source) THEN
          WITH Curr^ DO
            IF dis_addr = 0 THEN
              dis_addr := dt.Components.Components^[Pos.ComN].DI.Modules^[Pos.ModN-1].Segments^[0].Begin;
            END;
            ASSERT (SetNewPosByAddr(dis_addr));
          END;
        END;
        eve.AddToTail (eve.AllWindows, eve.Redraw, 1);
        eve.AddToTail (std.Wnds[std.MainWindow].hwnd, eve.Rise, 0);

      | key.CtrlEnter:
        IF N = 0 THEN RETURN; END;
        InitSectionList;
      ELSE
        std.ListBox (hwnd, msg);
      END;
    END;
  ELSE
    std.ListBox (hwnd, msg);
  END;
END ModulesHandler;


<* PUSH *>
<* WOFF301+ *>
PROCEDURE lctrModules (hwnd: crt.HWND; i: CARDINAL; VAR str: ARRAY OF CHAR);
<* POP *>
VAR
  ModName: xs.txt_ptr;
BEGIN
  WITH Components.Comps^[Curr_ComNo_int].Modules^[Key^[i]] DO
    IF tls.ModName(Pos.ComN, Pos.ModN, ModName) THEN
      COPY (ModName^, str);
    ELSE
      COPY ('', str);
    END;
  END;
END lctrModules;


PROCEDURE tglShowAllModules (check: BOOLEAN): BOOLEAN;
BEGIN
  IF check THEN
    RETURN std.tglOption (opt.ShowAllModules, TRUE);
  ELSE
    ASSERT(std.tglOption (opt.ShowAllModules, FALSE));
    RefreshModulesKey;
    RETURN TRUE;
  END;
END tglShowAllModules;


PROCEDURE InitWindow(show: BOOLEAN);
VAR
  p    : std.PLIST;
BEGIN
  WITH std.Wnds[std.ModWindow] DO
    IF hwnd = win.Invalid_H THEN
      hwnd := win.RegisterWindow(ModulesHandler,SIZE(std.LIST));
      ASSERT(hwnd # win.Invalid_H);

      win.SetWindowSize(hwnd, size);

      win.SetMovable(hwnd);
      win.SetResizable(hwnd, TRUE, TRUE);
      win.SetSwitchable(hwnd);

      win.SetHeader(hwnd, mes.Modules_);
      p := win.GetAMPtr(hwnd);
      p^ := std.EmptyList;
      WITH p^ DO
        locator    := lctrModules;
        Colors     := sys.ADR(crt.List);
        actions[ContextItem_ShowCode] := act.CONTEXT{ act.context_item, 'Show code'};
        actions[1] := act.CONTEXT{ act.do_action, act.ModuleVars };
        actions[2] := act.CONTEXT{ act.separate };
        actions[3] := act.CONTEXT{ act.toggler, 'Show all', tglShowAllModules };
        actions[ContextItem_RereadDebugInfo] := act.CONTEXT{ act.context_item, 'Reread debug info' };
        actions[5] := act.EMPTY_CONTEXT;
      END;
    END;
    IF show THEN eve.AddToTail(hwnd, eve.Rise, 0); END;
  END;
END InitWindow;


PROCEDURE InitList(action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Modules);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  InitWindow (mode # act.mode_check);
  IF mode = act.mode_check THEN
    RETURN std.QueryAction(std.Wnds[std.ModWindow].hwnd, act.Modules);
  ELSE
    RETURN TRUE;
  END;
END InitList;


PROCEDURE ResetTryRead;
VAR
  i,j: CARDINAL;
BEGIN
  WITH Components DO
    IF Comps # NIL THEN
      FOR i := 1 TO Count DO
        WITH Comps^[i-1] DO
          IF Modules # NIL THEN
            FOR j := 0 TO HIGH(Modules^) DO
              WITH Modules^[j] DO
                tls.ResetModTryRead(Pos.ComN, Pos.ModN);
                IF Pos.ModN = exe.ExecMod THEN
                  pos   := 0;
                  N     := MAX( CARDINAL);
                ELSE
                  pos   := 0;
                  N     := MAX( CARDINAL);
                END;
              END;
            END;
          END;
        END;
      END;
    END;
  END;
END ResetTryRead;




PROCEDURE RefreshModulesKey;
VAR
  p: std.PLIST;
BEGIN
  WITH Components DO
    IF (Comps # NIL) AND (Count # 0) THEN
      WITH Components.Comps^[Curr_ComNo_int] DO
        IF (opt.ShowAllModules) OR (K_DebugInfo = NIL) THEN
          Key := K_All;
        ELSE
          Key := K_DebugInfo;
        END;
        IF std.Wnds[std.ModWindow].hwnd # win.Invalid_H THEN
          p := win.GetAMPtr(std.Wnds[std.ModWindow].hwnd);
          p^.curr  := 0;
          p^.frame := 0;
          eve.AddToTail(std.Wnds[std.ModWindow].hwnd, eve.Redraw, 0);
        END;
      END;
    END;
  END;
END RefreshModulesKey;


PROCEDURE ClearModules;
VAR
  i: CARDINAL;
BEGIN
  WITH Components DO
    IF Comps # NIL THEN
      FOR i := 1 TO Count DO
        WITH Comps^[i-1] DO
          DISPOSE(Modules);
          IF K_DebugInfo # NIL THEN DISPOSE(K_DebugInfo); END;
          IF K_All # NIL THEN DISPOSE(K_All); END;
         END;
      END;
      DISPOSE(Comps);
      Count := 0;
    END;
  END;
  Key := NIL;
  Curr := sys.ADR(ZeroModule);

  Curr_ComNo     := dt.Invalid_Component;
  Curr_ComNo_int := dt.Invalid_Component;

  Curr_ModNo     := dt.Invalid_Module;
  Curr_ModNo_int := dt.Invalid_Module;
END ClearModules;


VAR
  ShowFullComponentName: BOOLEAN;


PROCEDURE tglShowFullComponentName (check: BOOLEAN): BOOLEAN;
BEGIN
  RETURN std.tglOption (ShowFullComponentName, check);
END tglShowFullComponentName;


<* PUSH *>
<* WOFF301+ *>
PROCEDURE lctrComponents (hwnd: crt.HWND; i: CARDINAL; VAR str: ARRAY OF CHAR);
<* POP *>
BEGIN
  IF ShowFullComponentName THEN
    COPY(dt.Components.Components^[i].EI.full_name, str);
  ELSE
    COPY(dt.Components.Components^[i].EI.short_name, str);
  END;
END lctrComponents;


PROCEDURE ComponentsHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p: std.PLIST;

  PROCEDURE write_line(num: CARDINAL);
  TYPE
    STR = xs.String;
  CONST
    Empty = ''+0C;
  VAR
    str  : STR;
    pstr : POINTER TO STR;
  BEGIN
    WITH p^ DO
      IF num = exe.ExecComp THEN
        crt.SetPos (1, num-p^.frame + 1);
        crt.WrChar (hwnd, CHAR(16), crt.Attr(crt.BringColor(crt.Bg(crt.Src[crt.Src_ExecCursor])), crt.Bg(p^.Colors^[crt.List_Line])));
      ELSIF num = Curr^.Pos.ComN THEN
        crt.SetPos (1, num-p^.frame + 1);
        crt.WrChar (hwnd, CHAR(16), crt.Attr(crt.BringColor(crt.Bg(crt.Src[crt.Src_UserCursor])), crt.Bg(p^.Colors^[crt.List_Line])));
      END;
      lctrComponents (hwnd, num, str);
      crt.SetPos(2, num - p^.frame + 1);
      IF p^.pos <= LENGTH(str) THEN
        pstr := sys.ADR(str[p^.pos]);
      ELSE
        pstr := sys.ADR(Empty);
      END;
      crt.WrStrFromPos(hwnd, pstr^, p^.Colors^[crt.List_Line], pos);
    END;
  END write_line;

VAR
  size   : crt.SZ;
  i, len : CARDINAL;
  last   : CARDINAL;

BEGIN
  p := win.GetAMPtr(hwnd);
  CASE msg.ID OF
  | eve.Redraw, eve.Paint:
    size := win.GetWindowSize(hwnd);
    IF (msg.par # 3) AND (msg.par # 4) THEN
      crt.FillWindow(hwnd, ' ', p^.Colors^[crt.List_Background])
    END;
    WITH size DO len := y2 - y1 - 1; END;
    WITH p^ DO
      IF kexe.Loaded THEN
        N := dt.Components.Count;
      ELSE
        N := 0;
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
        IF N = 0 THEN RETURN; END;
        p := win.GetAMPtr(hwnd);
        SetNewComp(p^.curr);
        dv.RestoreMainMode;
        WITH Components.Comps^[Curr_ComNo_int] DO
          IF curr_mod # dt.Invalid_Module THEN
            SetNewModule(curr_mod);
          ELSE
            SetNewModule(1);
          END;
          -- AVY: first param in SetNewPosByAddrInComp is component
          -- number in DI_Types numeration, not internal
          IF curr_addr # 0 THEN
            ASSERT(SetNewPosByAddrInComp(Curr_ComNo(*_int*), curr_addr));
          ELSE
            WITH dt.Components.Components^[Curr_ComNo].EI DO
              IF (N_Objects <= Code_Object) OR
                NOT SetNewPosByAddrInComp (Curr_ComNo(*_int*), Objects^[Code_Object].Begin)
              THEN
                crt.Beep;
                RETURN;
              END;
            END;
          END;
        END;
        eve.AddToTail(eve.AllWindows, eve.Redraw, 0);
        eve.AddToTail(std.Wnds[std.MainWindow].hwnd, eve.Rise, 0);
      ELSE
        std.ListBox(hwnd, msg);
      END;
    END;

  ELSE
    std.ListBox(hwnd, msg);
  END;
END ComponentsHandler;



PROCEDURE InitComponentsWindow (show: BOOLEAN);
VAR
  p: std.PLIST;
BEGIN
  WITH std.Wnds[std.ComponentsWindow] DO
    IF hwnd = win.Invalid_H THEN
      hwnd := win.RegisterWindow(ComponentsHandler, SIZE(std.LIST));
      ASSERT(hwnd # win.Invalid_H);

      win.SetWindowSize(hwnd, size);

      win.SetMovable(hwnd);
      win.SetResizable(hwnd, TRUE, TRUE);
      win.SetSwitchable(hwnd);

      win.SetHeaderByStr(hwnd, act.ActionName[act.Components]);

      p := win.GetAMPtr(hwnd);
      p^ := std.EmptyList;
      WITH p^ DO
        locator   := lctrComponents;
        actions[0] := act.CONTEXT{ act.toggler, 'Show full name', tglShowFullComponentName };
        actions[1] := act.EMPTY_CONTEXT;
        Colors    := sys.ADR(crt.List);
      END;
    END;
    IF show THEN eve.AddToTail(hwnd, eve.Rise, 0); END;
  END;
END InitComponentsWindow;


PROCEDURE ComponentsAction (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Components);
  IF dt.Components.Count = 0 THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    InitComponentsWindow(TRUE);
  END;
  RETURN TRUE;
END ComponentsAction;


BEGIN
  Curr_ComNo     := dt.Invalid_Component;
  Curr_ComNo_int := dt.Invalid_Component;

  Curr_ModNo     := dt.Invalid_Module;
  Curr_ModNo_int := dt.Invalid_Module;

  std.Wnds[std.ModWindow].init := InitWindow;
  std.Wnds[std.ComponentsWindow].init  := InitComponentsWindow;
  
  Components := COMPONENTS{0, NIL};
  ZeroModule := MOD_REC{ dt.Invalid_Position, enabled, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  Key     := NIL;
  Curr := sys.ADR(ZeroModule);
  Section_Window := win.Invalid_H;
  SaveMode := dv.need_disasm;

  ShowFullComponentName := FALSE;

  act.IniAction(act.Modules, InitList);
  act.IniAction(act.Components, ComponentsAction);
END DlgMods.
