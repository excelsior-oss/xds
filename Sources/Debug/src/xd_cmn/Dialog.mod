<* Storage+ *>
IMPLEMENTATION MODULE Dialog;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;
IMPORT wstr:= WholeStr;
IMPORT st  := Strings;
IMPORT arg := ProgEnv;
IMPORT com := COMPILER;
IMPORT tcn := TimeConv;
IMPORT ioc := IOChan;
IMPORT seq := SeqFile;
IMPORT tio := TextIO;


IMPORT xdt := XD_Title;

IMPORT dv  := Dlg_Vars;
IMPORT men := Dlg_Menu;
IMPORT dmm := Dlg_Mem;
IMPORT dex := Dlg_Exec;
IMPORT win := Dlg_Win;
IMPORT act := Dlg_Acts;
IMPORT dsm := Dlg_Dasm;
IMPORT puw := DlgPopup;
IMPORT cfg := DlgCfg;
IMPORT eve := DlgEvent;
IMPORT dbrk:= DlgBreak;
IMPORT brw := DlgBrows;
IMPORT mod := DlgMods;
IMPORT std := Dlg_Std;
IMPORT dw  := DlgWatch;
IMPORT dlt := DlgTypes;

IMPORT brk := Breaks;
IMPORT crt := CRT;
IMPORT exe := ExeMain;
IMPORT exp := Expr;
IMPORT evn := Events;

IMPORT mes := MsgNo;
IMPORT key := Keys;
IMPORT opt := Options;
IMPORT pro := Protocol;
IMPORT xs  := xStr;
IMPORT lst := Lists;

IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;

IMPORT stk := CallStk;

IMPORT kt  := KrnTypes;
IMPORT mem := Exe_Mem;
IMPORT kexe:= KrnExec;

IMPORT fil := File;
IMPORT red := RedFile;

IMPORT nm  := Names;

<* IF xd_batch_included THEN *>

IMPORT pt  := PckTypes;
IMPORT bas := PckBase;

<* IF DEST_K26 THEN *>
IMPORT mdl := Model;
<* END *>

<* END *>


<* IF DEST_XDS THEN *>

IMPORT trd := Threads;

<* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME
IMPORT dl  := Dlg_Log;
<* END *>

<* END *>


<* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
IMPORT                              (* CHERN *)
  wbr := Windbrk,
 -- prf := Asmprof,
    prv := Prf_Var,
 -- lpr := Lineprof,
  bls := Blist_new,
  tim := Timeprof;
                                    (*  end CHERN *)
<* END *>



<* IF TARGET_OS = 'WINNT' THEN *>

IMPORT WIN := Windows;

<* ELSIF TARGET_OS = 'OS2' THEN *>

IMPORT OS2;

PROCEDURE StartViewer (HelpFile: ARRAY OF CHAR; VAR ErrorStr: ARRAY OF CHAR): CARDINAL;

CONST
  VIEWER = 'VIEW.EXE';

VAR
  PATH       : OS2.PCSZ;
  PPATH      : xs.txt_ptr;
  StartData  : OS2.STARTDATA;
  ViewerPath : xs.String;
  ViewerPID  : OS2.PID;
  ViewerSID  : OS2.ULONG;
  Result     : CARDINAL;

BEGIN
  OS2.DosScanEnv ("PATH", PATH);
  PPATH := sys.CAST(xs.txt_ptr, PATH);
  Result := OS2.DosSearchPath (OS2.SEARCH_IGNORENETERRS+OS2.SEARCH_CUR_DIRECTORY,
                               PPATH^, VIEWER, ViewerPath, SIZE(ViewerPath));
  IF Result # 0 THEN
    COPY('Not found by path', ErrorStr);
    RETURN Result;
  END;

  WITH StartData DO
    Length        := SIZE(StartData);
    Related       := OS2.SSF_RELATED_CHILD; --INDEPENDENT;
    FgBg          := OS2.SSF_FGBG_BACK;
    TraceOpt      := OS2.SSF_TRACEOPT_NONE;
    PgmTitle      := NIL;
    PgmName       := sys.ADR(ViewerPath);
    PgmInputs     := sys.ADR(HelpFile);
    TermQ         := NIL;
    Environment   := NIL;
    InheritOpt    := OS2.SSF_INHERTOPT_SHELL;
    SessionType   := OS2.SSF_TYPE_DEFAULT;
    IconFile      := NIL;
    PgmHandle     := 0;
    PgmControl    := 0;
    InitXPos      := 0;
    InitYPos      := 0;
    InitXSize     := 0;
    InitYSize     := 0;
    Reserved      := 0;
    ObjectBuffer  := sys.ADR(ErrorStr);
    ObjectBuffLen := SIZE(ErrorStr);
  END;

  RETURN OS2.DosStartSession (StartData, ViewerSID, ViewerPID);
END StartViewer;

<* END *>

<* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
VAR
  prf_Var_Ind : CARDINAL;    (*       индекс текущeй проф переменной SCHERN         *)
<* END *>


PROCEDURE Help (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  pname: xs.String;
  helpname: xs.String;

 <* IF env_target = 'x86os2' THEN *>
  ErrorStr: xs.String;
 <* END *>

BEGIN
  ASSERT(action = act.Help);
  arg.ProgramName(pname);
  fil.ExtractFileName(pname, helpname);
  fil.RemoveExtension(helpname);
  IF mode # act.mode_check THEN
<* IF env_target = 'x86nt' THEN *>
    xs.Append(".hlp", helpname);
    IF NOT WIN.WinHelp (NIL, helpname, WIN.HELP_FINDER, 0) THEN
      std.ErrorNo(mes.HelpFileNotFoundAtStartup);
      RETURN FALSE;
    END;
<* ELSE *>
    xs.Append(".inf", helpname);
    IF StartViewer (helpname, ErrorStr) # 0 THEN
      std.ErrorNo(mes.HelpFileNotFoundAtStartup);
      RETURN FALSE;
    END;
<* END *>
  END;
  RETURN TRUE;
END Help;


VAR
  CalcDialog   : win.HWND;
  SearchDialog : win.HWND;

VAR
  SearchStr  : xs.String; (* строка, котоpая будет искаться *)
  win2search : win.HWND; (* окно в котоpом будет осуществляться поиск *)
  found      : BOOLEAN;  (* Была ли найдена строчка соответсвующим окном в своем контексте *)




PROCEDURE ProceduresHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p : std.PLIST;
  pl: dt.OBJECT;

  PROCEDURE write_line(num: CARDINAL);
  VAR
    Name: xs.String;
    buf : xs.String;
  BEGIN
    WITH p^ DO
      WITH mod.Curr^.Pos DO
        pl := tls.GetLabel(ComN, ModN, num);
      END;
      ASSERT(tls.IsObjectValid(pl));
      nm.ObjectNameGetAndCorrect (pl, Name);
      fmt.print(buf, '%s', Name);
      crt.SetPos(2, num - frame + 1);
      crt.WrStrFromPos(hwnd, buf, Colors^[crt.List_Line], pos);
    END;
  END write_line;

VAR
  size  : crt.SZ;
  i, len: CARDINAL;
  last  : CARDINAL;
  addr  : kt.ADDRESS;

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
      IF kexe.Loaded AND tls.IsPosValid(mod.Curr^.Pos) THEN
        N := tls.LabelsNo(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN);
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
    IF std.GetAction (msg.par) = act.Procs THEN
      act.ConfirmQueryByCond (kexe.Loaded);
--      IF kexe.Loaded AND tls.IsPosValid(mod.Curr^.Pos) THEN
--        act.ConfirmQueryByCond(tls.LabelsNo(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN)#0);
--      END;
    ELSE
      std.ListBox (hwnd, msg);
    END;

  | eve.KbHit:
    WITH p^ DO
      CASE msg.par OF
      | key.Enter:
        IF N = 0 THEN RETURN; END;
        pl  := tls.GetLabel(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, curr);
        ASSERT( tls.ObjectAddr(pl, addr));
        <* IF TARGET_VAX THEN *>
        INC(addr, 2); -- Смещение из-за маски входа
        <* END *>
        ASSERT(mod.SetNewPosByAddr(addr));
        eve.AddToTail(hwnd, eve.Redraw, 0);
        eve.AddToTail(std.Wnds[std.MainWindow].hwnd, eve.Rise, 0);
      ELSE
        std.ListBox(hwnd, msg);
      END;
    END;
  ELSE
    std.ListBox(hwnd, msg);
  END;
END ProceduresHandler;



CONST
  ContextItem_Pointed = 4;
  ContextItem_Type    = 5;


PROCEDURE ChangeVariable (hwnd: win.HWND; var: exp.ExprRes): BOOLEAN; FORWARD;



--------------------------------------------------------------------------------

PROCEDURE lctrVariables (hwnd: crt.HWND; i: CARDINAL; VAR str: ARRAY OF CHAR);
VAR
  p   : std.PLIST;
  pext: std.PLIST_EXT_VARIABLE;
  pv  : dt.OBJECT;
  name: xs.String;
BEGIN
  p := win.GetAMPtr(hwnd);
  pext := std.PLIST_EXT_VARIABLE(p^.ext);
  IF pext^.Auto THEN
    pext^.Pos := mod.Curr^.Pos;
    IF pext^.Globals THEN
      pext^.Pos.ModN := dt.Fake_Module;
    END;
  END;
  pv := tls.GetVar(pext^.Pos.ComN, pext^.Pos.ModN, i);
  ASSERT(tls.IsObjectValid(pv));
  tls.ObjectName (pv, name);
  COPY(name, str);
END lctrVariables;



PROCEDURE VariablesHandler (hwnd: win.HWND; msg: eve.MSG);

VAR
  p      : std.PLIST;
  pext   : std.PLIST_EXT_VARIABLE;
  size   : crt.SZ;

  PROCEDURE InitData;
  VAR
    s1, s2: xs.String;
    name: xs.txt_ptr;
  BEGIN
    p := win.GetAMPtr(hwnd);
    p^.N := 0;
    pext := std.PLIST_EXT_VARIABLE(p^.ext);
    IF pext^.Auto THEN
      pext^.Pos := mod.Curr^.Pos;
      IF pext^.Globals THEN
        pext^.Pos.ModN := dt.Fake_Module;
      END;
    END;
    size := win.GetWindowSize(hwnd);
    pro.GetMsg (mes.GlobalVariables, s1);
    IF kexe.Loaded AND tls.IsPosValid (pext^.Pos) THEN
      p^.N := tls.VarsNo(pext^.Pos.ComN, pext^.Pos.ModN);
      IF tls.ModName(pext^.Pos.ComN, pext^.Pos.ModN, name) THEN
        IF pext^.Auto THEN
          fmt.print (s2, "%s (Auto: %s)", s1, name^);
        ELSE
          fmt.print (s2, "%s (%s)", s1, name^);
        END;
        win.SetHeaderByStr(hwnd, s2);
      ELSE
        win.SetHeaderByStr(hwnd, s1);
      END;
    ELSE
      p^.N := 0;
      win.SetHeaderByStr(hwnd, s1);
    END;
  END InitData;


  PROCEDURE GetRes (i: CARDINAL; VAR pv: dt.OBJECT; VAR res: exp.ExprRes);
  BEGIN
    pv := tls.GetVar (pext^.Pos.ComN, pext^.Pos.ModN, i);
    res.sort := exp.Variable;
    ASSERT(tls.ObjectAddr(pv, res.location));
    ASSERT(tls.ObjectType(pv, res.var_type));
    ASSERT(tls.ObjectSymTypeID(pv, res.type));
  END GetRes;


VAR
  VarName: xs.String;
  ModName: xs.txt_ptr;

  PROCEDURE write_line(num: CARDINAL);
  VAR
    obj: dt.OBJECT;
    str: xs.String;
    res: exp.ExprRes;
    buf: xs.String;
    tag: dt.TYPE_TAG;
  BEGIN
    WITH p^ DO
      GetRes (num, obj, res);
      tls.ObjectName (obj, VarName);
      crt.SetPos(2, num-frame + 1);
      ASSERT(tls.TypeTag(res.var_type, tag));
      IF opt.DisplayDerefencePointer AND (tag = dt.Pointer) THEN
        res.type := dt.st_dereference;
      END;
      exp.Res2Str (res, str);
      fmt.print(buf, '%-10s %s', VarName, str);
     <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
      IF prv.MyVar(??mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN,num) THEN         (* CHERN *)
        crt.WrStrFromPos(hwnd, buf,  Colors^[crt.List_Line],pos);
        crt.Lite(hwnd,num-frame+1, 1, crt.Attr(crt.Yellow, crt.Bg(Colors^[crt.List_Line])));
        crt.Hilite(hwnd,num-frame+1,1,crt.LightMagenta);
      ELSE                                                          (*  end CHERN *)
        crt.WrStrFromPos(hwnd, buf, Colors^[crt.List_Line], pos);
      END;
     <* ELSE *>
      crt.WrStrFromPos(hwnd, buf, Colors^[crt.List_Line], pos);
     <* END *>
    END;
  END write_line;


VAR
  i, len  : CARDINAL;
  last    : CARDINAL;
  obj     : dt.OBJECT;
  obj_tag : dt.TYPE_TAG;
  var     : exp.ExprRes;
  subtype : dt.PTYPE;
  type_tag: dt.TYPE_TAG;
  x, y    : CARDINAL;
  buf     : xs.String;
  name    : xs.String;
  main    : BOOLEAN;
  item    : CARDINAL;
  addr    : kt.ADDRESS;


CONST
  ContextItem_ViewAs = ContextItem_Type+1;


BEGIN
  InitData;
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
           <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
            IF prv.MyVar(??mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN,curr) THEN         (* CHERN *)
              crt.Lite(hwnd,curr-frame+1, 1, crt.Attr(crt.Yellow, crt.Bg(Colors^[crt.List_CurrentLine])));
            ELSE                                                             (* end CHERN *)
             crt.Lite(hwnd, curr - frame + 1 , 1, Colors^[crt.List_CurrentLine]);
            END;
           <* ELSE *>
            crt.Lite(hwnd, curr-frame+1 , 1, Colors^[crt.List_CurrentLine]);
           <* END *>
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
           <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
            IF prv.MyVar(??mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN,curr) THEN         (* CHERN *)
              crt.Lite(hwnd,curr-frame+1, 1, crt.Attr(crt.Yellow, crt.Bg(Colors^[crt.List_CurrentLine])));
            ELSE                                                             (* end CHERN *)
             crt.Lite(hwnd, curr - frame + 1 , 1, Colors^[crt.List_CurrentLine]);
            END;
           <* ELSE *>
            crt.Lite(hwnd, curr-frame+1 , 1, Colors^[crt.List_CurrentLine]);
           <* END *>
          END;
        END;
      END;
    END;
    std.ListBox(hwnd, msg);

  | eve.QueryItem:
    IF p^.N > 0 THEN
      std.GetItem (msg.par, main, item);
      IF main THEN
        CASE item OF
        | ContextItem_Pointed:
          GetRes (p^.curr, obj, var);
          ASSERT(tls.TypeTag (var.var_type, type_tag));
          act.ConfirmQueryByCond (type_tag = dt.Pointer);
        | ContextItem_ViewAs:
          act.ConfirmQueryByCond (lst.EquNamesNo() > 0);
        END;
      ELSE
        act.ConfirmQueryByCond (item < lst.EquNamesNo());
      END;
    END;

  | eve.ContextItem:
    std.GetItem (msg.par, main, item);
    IF main THEN
      CASE item OF
      | ContextItem_Pointed:
        GetRes (p^.curr, obj, var);
        exp.ObjectFullName (obj, buf);
        COPY(buf, dv.DumpAddrStr);
        act.ExecuteAction (act.Dump, act.mode_silent);
      | ContextItem_ViewAs:
        ASSERT(FALSE);
      END;
    ELSE
      lst.GetName (item, name);
      lst.GetEquName (name, buf);
      obj := tls.GetVar(pext^.Pos.ComN, pext^.Pos.ModN, p^.curr);
      exp.ObjectFullName(obj, name);
      fmt.print (dv.VarName, "%s(%s)", buf, name);
      act.ExecuteAction (act.Examine, act.mode_silent);
    END;

  | eve.QueryAction:
    CASE std.GetAction (msg.par) OF
    | act.GlobalVars:
      act.ConfirmQueryByCond (kexe.Loaded);
--      IF kexe.Loaded AND pext^.Globals AND tls.IsComValid(pext^.Pos.ComN) THEN
--        act.ConfirmQueryByCond(tls.VarsNo(pext^.Pos.ComN, pext^.Pos.ModN)#0);
--      END;
    | act.ModuleVars:
      act.ConfirmQueryByCond (kexe.Loaded);
--      IF kexe.Loaded AND NOT pext^.Globals AND tls.IsPosValid (pext^.Pos) THEN
--        act.ConfirmQueryByCond(tls.VarsNo(pext^.Pos.ComN, pext^.Pos.ModN)#0);
--      END;
    | act.ChangeTypes:
      IF p^.N = 0 THEN RETURN; END;
      GetRes(p^.curr, obj, var);
      IF NOT exp.Var2Value(var, var) THEN
        crt.Beep;
        RETURN;
      END;
      CASE var.sort OF
      | exp.CARDval, exp.INTval, exp.WHOLEval:
        act.ConfirmQuery;
      | exp.Variable:
        ASSERT(tls.TypeTag(var.var_type, type_tag));
        IF type_tag IN dt.TYPE_TAG_SET{dt.Pointer, dt.Int, dt.Card} THEN
          act.ConfirmQuery();
        END;
      ELSE
      END;
    ELSE
      std.ListBox (hwnd, msg);
    END;

  | eve.DoAction:
    WITH p^ DO
      CASE std.GetAction (msg.par) OF
      | act.ChangeTypes:
        obj := tls.GetVar(pext^.Pos.ComN, pext^.Pos.ModN, curr);
        ASSERT(tls.SetObjectSymTypeID(obj, act.ChangeTypesID));
      | act.ContextMenu:
        eve.AddToTail(hwnd, eve.R_Mouse, msg.par);
      | act.Dump:
        IF N # 0 THEN
          obj := tls.GetVar(pext^.Pos.ComN, pext^.Pos.ModN, curr);
          ASSERT(tls.ObjectAddr(obj, addr));
          fmt.print (dv.DumpAddrStr, exp.Fmt_ADDRval, addr);
          act.ExecuteAction (act.Dump, act.mode_silent);
          RETURN;
        END;
      | act.AddWatch:
        IF N # 0 THEN
          obj := tls.GetVar(pext^.Pos.ComN, pext^.Pos.ModN, curr);
          exp.ObjectFullName(obj, dv.expr);
          act.ExecuteAction (act.AddWatch, act.mode_silent);
          RETURN;
        END;
      | act.Examine:
        IF N # 0 THEN
          obj := tls.GetVar(pext^.Pos.ComN, pext^.Pos.ModN, curr);
          exp.ObjectFullName(obj, dv.VarName);
          act.ExecuteAction (act.Examine, act.mode_silent);
          RETURN;
        END;
     <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
      | act.prf_var0:
(*                                                              SCHERN              *)
--          prv.Ind_Var:= curr;
--          act.ExecuteAction (act.prf_var0);
          IF prv.AddVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN, curr) THEN  (* включаем переменную в профилирование*)
            eve.AddToTail(hwnd, eve.Redraw, 0);
          END;
      | act.prf_var9:
          prv.DelVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN, curr);
          eve.AddToTail(hwnd, eve.Redraw, 0);
      | act.prf_var8:
          prv.AddAllVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN);
          (* включаем все переменные модуля в профилирование*)
          eve.AddToTail(hwnd, eve.Redraw, 0);
      | act.prf_var7:
          prv.DelAllVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN);
          (* исключаем все переменные модуля из профилирование*)
          eve.AddToTail(hwnd, eve.Redraw, 0);
     | act.prf_var6:
          wbr.ShowCountVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN, curr);

(*                                                                  end SCHERN        *)
    <* END *>

     <* IF DEST_XDS THEN *>
      | act.Access:
        IF N # 0 THEN
          obj := tls.GetVar(pext^.Pos.ComN, pext^.Pos.ModN, curr);
          ASSERT(tls.ObjectSize(obj, len));
          exp.ObjectFullName(obj, buf);
          xs.Append(')', buf);
          xs.Insert('ADR(', 0, buf);
          dbrk.SetAccessAttr(buf, len);
          act.ExecuteAction (act.Access, act.mode_silent);
          RETURN;
        END;
     <* END *>
      ELSE
      END;
      std.ListBox (hwnd, msg);
    END;

  | eve.R_Mouse:
    ASSERT(win.GetMouse(msg, x, y));
    size := win.GetWindowSize(hwnd);
    IF NOT std.CheckFrame(size, x, y) THEN
      eve.AddToTail(hwnd, eve.Mouse_Pressed, msg.par);
      eve.Flush;
      p^.actions[ContextItem_Type].tenable := act.TYPES_SET{};
      p^.actions[ContextItem_ViewAs] := act.EMPTY_CONTEXT;
      IF p^.N > 0 THEN
        GetRes(p^.curr, obj, var);
        IF NOT exp.Var2Value(var, var) THEN RETURN; END;
        CASE var.sort OF
        | exp.CARDval, exp.INTval, exp.WHOLEval:
          p^.actions[ContextItem_Type].tenable := act.TYPES_SET{dt.st_original, dt.st_unsigned, dt.st_signed, dt.st_hex, dt.st_oct, dt.st_bin };
        | exp.Variable:
          ASSERT(tls.TypeTag(var.var_type, type_tag));
          IF type_tag = dt.Pointer THEN
            p^.actions[ContextItem_Type].tenable := act.TYPES_SET{dt.st_original,dt.st_dereference};
          ELSIF type_tag = dt.Int THEN
            p^.actions[ContextItem_Type].tenable := act.TYPES_SET{dt.st_original, dt.st_unsigned, dt.st_signed, dt.st_hex, dt.st_bin };
          END;
        ELSE
        END;
      END;
      IF p^.N # 0 THEN
        IF std.MakeSubmenuViewAs (p^.actions[ContextItem_ViewAs]) THEN
          p^.actions[ContextItem_ViewAs+1] := act.EMPTY_CONTEXT;
        END;
      END;
      puw.PopupWindow(x, y, hwnd, p^.actions);
    END;

  | eve.KbHit:
    WITH p^ DO
      CASE msg.par OF
     <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
(*                                     SCHERN              *)
      |key.Alt0:
          IF prv.AddVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN, curr) THEN  (* включаем переменную в профилирование*)
            eve.AddToTail(hwnd, eve.Redraw, 0);
          END;
      | key.Alt9:
          prv.DelVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN, curr);
          eve.AddToTail(hwnd, eve.Redraw, 0);

      | key.Alt8 :
          prv.AddAllVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN);
          (* включаем все переменные модуля в профилирование*)
          eve.AddToTail(hwnd, eve.Redraw, 0);

      | key.Alt7 :
          prv.DelAllVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN);
          (* исключаем все переменные модуля из профилирование*)
          eve.AddToTail(hwnd, eve.Redraw, 0);

      | key.Alt6 :
          wbr.ShowCountVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN, curr);

(*                                         end   SCHERN        *)
     <* END *>
      | key.Enter, key.CtrlEnter:
        IF N = 0 THEN RETURN; END;
        GetRes(curr, obj, var);
       <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
        prf_Var_Ind := curr;                                        (* CHERN *)
       <* END *>
        tls.ObjectName (obj, VarName);
        ASSERT(tls.TypeTag(var.var_type, obj_tag));
        IF tls.ModName(pext^.Pos.ComN, pext^.Pos.ModN, ModName) THEN
          fmt.print (dv.VarName, '%s.%s', ModName^, VarName);
        ELSE
          fmt.print (dv.VarName, '%s', VarName);
        END;
        IF obj_tag = dt.Pointer THEN
          tls.SubType(var.var_type, subtype);
          ASSERT(tls.TypeTag(subtype, obj_tag));
          IF obj_tag = dt.Procedure THEN
            var.var_type := subtype;
          ELSIF (msg.par = key.Enter) THEN
            IF NOT exp.Dereference(var, var) THEN
              std.ErrorNo (mes.NilPointerDereference);
              RETURN;
            END;
            xs.Append ("^", dv.VarName);
          END;
        END;
        IF NOT ChangeVariable (win.Invalid_H, var) THEN
          eve.AddToTail (std.ErrorMsg, eve.Rise, 0);
        END;
      ELSE
        std.ListBox(hwnd, msg);
      END;
    END;
  ELSE
    std.ListBox(hwnd, msg);
  END;
END VariablesHandler;


PROCEDURE OpenNewVariableWindow (VAR hwnd: win.HWND; size: crt.SZ; show: BOOLEAN);
VAR
  p: std.PLIST;
BEGIN
  IF hwnd = win.Invalid_H THEN
    hwnd := win.RegisterWindow (VariablesHandler, SIZE(std.LIST)+SIZE(std.LIST_EXT_VARIABLE));
    ASSERT(hwnd # win.Invalid_H);
    win.SetWindowSize(hwnd, size);
    win.SetMovable(hwnd);
    win.SetResizable(hwnd, TRUE, TRUE);
    win.SetSwitchable(hwnd);
    p := win.GetAMPtr(hwnd);
    p^ := std.EmptyList;
    WITH p^ DO
      Colors     := sys.ADR(crt.List);
      Frame      := crt.Double;
      locator    := NIL;
      actions[0] := act.CONTEXT{ act.do_action, act.Examine  };
     <* IF DEST_K26 THEN *>
      actions[01] := act.CONTEXT{ act.do_action, act.ConditionBreak };
     <* ELSIF DEST_XDS THEN *>
      actions[01] := act.CONTEXT{ act.do_action, act.Access   };
     <* END *>
      actions[02] := act.CONTEXT{ act.do_action, act.AddWatch };
      actions[03] := act.CONTEXT{ act.do_action, act.Dump };
      actions[04] := act.CONTEXT{ act.context_item, "Pointed memory" };
      actions[ContextItem_Type] := act.CONTEXT{ act.types, act.TYPES_SET{} };
      actions[06] := act.EMPTY_CONTEXT;
     <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
      actions[06] := act.CONTEXT{ act.do_action, act.prf_var0 };      (* SCHERN *)
      actions[07] := act.CONTEXT{ act.do_action, act.prf_var9 };      (* SCHERN *)
      actions[08] := act.CONTEXT{ act.do_action, act.prf_var8 };      (* SCHERN *)
      actions[09] := act.CONTEXT{ act.do_action, act.prf_var7 };      (* SCHERN *)
      actions[10] := act.CONTEXT{ act.do_action, act.prf_var6 };      (* SCHERN *)
      actions[11] := act.EMPTY_CONTEXT;                               (* SCHERN *)
     <* END *>
      ext := sys.ADDADR(sys.ADR(p^), SIZE(std.LIST)); -- Module variables
    END;
  END;
  IF show THEN
    eve.AddToTail(hwnd, eve.Rise, 0);
  END;
END OpenNewVariableWindow;



PROCEDURE InitSpecialVariableWindow (Pos: dt.POS);
VAR
  SpecVarWinHwnd: win.HWND;
  p: std.PLIST;
  pext: std.PLIST_EXT_VARIABLE;
  size: crt.SZ;
BEGIN
  ASSERT(tls.IsPosValid(Pos));
  SpecVarWinHwnd := win.Invalid_H;
  LOOP
    SpecVarWinHwnd := win.FindClosed (SpecVarWinHwnd, VariablesHandler);
    IF SpecVarWinHwnd = win.Invalid_H THEN EXIT; END;
    IF (SpecVarWinHwnd # std.Wnds[std.ModuleVarWindow].hwnd) AND
       (SpecVarWinHwnd # std.Wnds[std.GlobalVarWindow].hwnd)
    THEN
      EXIT;
    END;
  END;
  size := crt.SZ{ crt.Xmax-32, crt.Ymax-10, crt.Xmax-1, crt.Ymax-1};
  OpenNewVariableWindow (SpecVarWinHwnd, size, TRUE);
  win.UnHide (SpecVarWinHwnd);
  win.SetHeader(SpecVarWinHwnd, mes.ModuleVariables);
  p := win.GetAMPtr(SpecVarWinHwnd);
  p^.locator := lctrVariables;
  pext := std.PLIST_EXT_VARIABLE(p^.ext);
  pext^.Auto := FALSE;
  pext^.Globals := FALSE;
  pext^.Pos := Pos;
END InitSpecialVariableWindow;


PROCEDURE InitVarWindow(show: BOOLEAN);
VAR
  p: std.PLIST;
  pext: std.PLIST_EXT_VARIABLE;
BEGIN
  WITH std.Wnds[std.ModuleVarWindow] DO
    OpenNewVariableWindow (hwnd, size, show);
    win.SetHeader(hwnd, mes.ModuleVariables);
    p := win.GetAMPtr(hwnd);
    p^.locator := lctrVariables;
    pext := std.PLIST_EXT_VARIABLE(p^.ext);
    pext^.Auto := TRUE;
    pext^.Globals := FALSE;
    pext^.Pos := dt.Invalid_Position;
  END;
END InitVarWindow;


PROCEDURE InitVarsList (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.ModuleVars);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  InitVarWindow (mode # act.mode_check);
  IF mode = act.mode_check THEN
    RETURN std.QueryAction(std.Wnds[std.ModuleVarWindow].hwnd, act.ModuleVars);
  ELSE
    RETURN TRUE;
  END;
END InitVarsList;


PROCEDURE InitGlobalVarWindow (show: BOOLEAN);
VAR
  p: std.PLIST;
  pext: std.PLIST_EXT_VARIABLE;
BEGIN
  WITH std.Wnds[std.GlobalVarWindow] DO
    OpenNewVariableWindow (hwnd, size, show);
    win.SetHeader(hwnd, mes.GlobalVariables);
    p := win.GetAMPtr(hwnd);
    p^.locator := lctrVariables;
    pext := std.PLIST_EXT_VARIABLE(p^.ext);
    pext^.Auto := TRUE;
    pext^.Globals := TRUE;
    pext^.Pos := dt.Invalid_Position;
  END;
END InitGlobalVarWindow;


PROCEDURE InitGlobalVarList (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.GlobalVars);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  InitGlobalVarWindow (mode # act.mode_check);
  IF mode = act.mode_check THEN
    RETURN std.QueryAction(std.Wnds[std.GlobalVarWindow].hwnd, act.GlobalVars);
  ELSE
    RETURN TRUE;
  END;
END InitGlobalVarList;

--------------------------------------------------------------------------------




-- по текущему контексту возвращает число строк
PROCEDURE CountLocalObjects (): CARDINAL;
VAR
  scope: dt.OBJECT;
  params: CARDINAL;
  locals: CARDINAL;
  n: CARDINAL;
BEGIN
  -- обход всех объемлющих блоков
  scope := dv.LocalScope;
  n := 0;
  REPEAT
    params := tls.ParamVarsNo (scope);
    locals := tls.LocalVarsNo (scope);
    -- число собственных локальных обьектов
    INC(n, params+locals);
    -- строка-разделитель с именем контекста
    INC(n);
    scope := tls.ObjectParentScope(scope);
  UNTIL tls.EqualObjects (scope, dt.Invalid_Object);
  -- строку-разделитель для текущего контекста не надо добавлять
  DEC(n);
  RETURN n;
END CountLocalObjects;


-- по номеру строки возвращает контекст и обьект,
-- при этом если строка - разделитель, то вернет FALSE
PROCEDURE GetLineLocalWindow (N, i: CARDINAL; VAR scope, obj: dt.OBJECT; VAR separate: BOOLEAN): BOOLEAN;
VAR
  params: CARDINAL;
  locals: CARDINAL;
BEGIN
  IF i < N THEN
    -- обход всех объемлющих блоков
    scope := dv.LocalScope;
    separate := FALSE;
    LOOP
      params := tls.ParamVarsNo (scope);
      locals := tls.LocalVarsNo (scope);
      N := params + locals;
      IF i < params THEN
        -- параметры
        obj := tls.GetParamVar (scope, i);
        EXIT;
      ELSIF i < N THEN
        -- локалы
        obj := tls.GetLocalVar (scope, i-params);
        EXIT;
      ELSE
        scope := tls.ObjectParentScope(scope);
        IF tls.EqualObjects (scope, dt.Invalid_Object) THEN
          RETURN FALSE;
        END;
        IF i = N THEN
          -- строка-разделитель с именем контекста
          separate := TRUE;
          EXIT;
        END;
        -- обьект из охватывающего контекста
        DEC(i, N+1);
      END;
    END;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END GetLineLocalWindow;


PROCEDURE lctrLocalVariables (hwnd: crt.HWND; i: CARDINAL; VAR str: ARRAY OF CHAR);
VAR
  p       : std.PLIST;
  scope   : dt.OBJECT;
  obj     : dt.OBJECT;
  name    : xs.String;
  separate: BOOLEAN;
BEGIN
  COPY('', str);
  p := win.GetAMPtr(hwnd);
  IF kexe.Loaded THEN
    IF GetLineLocalWindow (p^.N, i, scope, obj, separate) THEN
      IF separate THEN
        nm.ObjectNameGetAndCorrect (scope, name);
        fmt.print (str, '- %s ──────────────────────────────────────────────────────────────────────────', name);
      ELSE
        tls.ObjectName (obj, name);
        COPY (name, str);
      END;
    END;
  END;
END lctrLocalVariables;


CONST
  Local_ContextItem_GoUpCall     = 7;
  Local_ContextItem_GoDownCall   = 8;
  Local_ContextItem_ShowLocation = 10;


VAR
  optionShowLocation: BOOLEAN;


PROCEDURE tglShowLocation (check: BOOLEAN): BOOLEAN;
BEGIN
  RETURN std.tglOption (optionShowLocation, check);
END tglShowLocation;


PROCEDURE LocalVariablesHandler (hwnd: win.HWND; msg: eve.MSG);

-- Замечание по терминологии, используемой в комментариях
-- exe.ExecScope - исполняемый контекст
-- dv.LocalScope - отображаемый контекст
-- scope - текущий контекст
-- при этом dv.LocalScope может не являтся охватывающим для
-- exe.ExecScope, или же совпадать с ним
-- при этом scope всегда является охватывающим для dv.LocalScope,
-- или же совпадает с ним
-- exe.ExecScope <= dv.LocalScope <= scope

VAR
  p   : std.PLIST;
  body: BOOLEAN;


  PROCEDURE InitData;
  VAR
    s1, s2: xs.String;
    name  : xs.String;
  BEGIN
    WITH p^ DO
      IF kexe.Loaded & tls.IsObjectValid (dv.LocalScope) THEN
        N := CountLocalObjects();
        IF tls.IsObjectValid (exe.ExecScope) THEN
          body := tls.AddrInProcBody (exe.ExecScope, exe.ExecAddr);
        ELSE
          body := FALSE;
        END;
      ELSE
        N := 0;
        body := FALSE;
      END;
      pro.GetMsg (mes.LocalVariables, s1);
      IF N = 0 THEN
        win.SetHeaderByStr(hwnd, s1);
      ELSE
        nm.ObjectNameGetAndCorrect (dv.LocalScope, name);
        fmt.print (s2, "%s (%s)", s1, name);
        win.SetHeaderByStr(hwnd, s2);
      END;
    END;
  END InitData;

  -- 1 ---- LocalVariablesHandler ----------------------------------------------
  PROCEDURE GetRes (scope, obj: dt.OBJECT; VAR res: exp.ExprRes): BOOLEAN;
  VAR
    obj_tag    : dt.SYM_TAG;
    obj_type   : dt.PTYPE;
    reg_no     : CARDINAL;
    reg_val    : kt.REG_VALUE;
    ref        : BOOLEAN;
    level      : CARDINAL;
    sym_id     : dt.SYM_TYPE_ID;
    top_call   : BOOLEAN;
   <* IF DEST_XDS THEN *>
    tmp        : kt.ADDRESS;
   <* END *>
  BEGIN
    ASSERT(tls.ObjectTag(obj, obj_tag));
    ASSERT(tls.ObjectSymTypeID (obj, sym_id));
    ASSERT(tls.ObjectType (obj, obj_type));
    CASE obj_tag OF
    | dt.Sy_Register :
      ASSERT(tls.GetLocalObject_Reg (obj, reg_no, ref));
      ASSERT(mem.GetReg (reg_no, reg_val));
      IF ref THEN
        res.sort     := exp.Variable;
        res.var_type := obj_type;
        res.location := kt.ADDRESS(reg_val);
        res.type     := sym_id;
      ELSE
        res.sort     := exp.Register;
        res.reg_type := obj_type;
        res.reg_no   := reg_no;
        res.type     := sym_id;
      END;
    | dt.Sy_Relative :
      top_call := tls.EqualObjects (exe.ExecScope, scope) AND (dv.LocalScopeLevel = 0);
      IF top_call AND tls.ProcHasFrame (exe.ExecScope) AND NOT body THEN
        -- текущий и исполняемый контекст совпали, но
        -- еще не инициализирована база текущего контекста
        RETURN FALSE;
      END;
      ASSERT (tls.GetLocalObject_Reg (obj, reg_no, ref));
      IF reg_no = kt.FRAME_REG THEN
        -- переменная относительно кадра (FRAME_REG), а не произвольного регистра
       <* IF DEST_XDS THEN *>
        stk.ScanCallStack;
       <* END *>
        -- поискать текущий в стеке вызовов
        IF NOT stk.GetObjectLevelInCallStack (dv.LocalScopeLevel, scope, level) THEN
          RETURN FALSE;
        END;
        -- достать базу текущего контекста из стека
        IF NOT stk.GetFrame (level, reg_val) THEN
          RETURN FALSE;
        END;
      ELSIF top_call THEN
        -- текущий и исполняемый контекст совпали
        -- при этом база обьекта не является регистром кадра
        ASSERT (mem.GetReg (reg_no, reg_val));
      ELSE
        -- текущий и исполняемый контекст не совпали, получить
        -- значение регистра уже нельзя
        RETURN FALSE;
      END;
      ASSERT (tls.GetLocalObject_Addr (obj, reg_val, res.location));
      res.sort := exp.Variable;
      res.var_type := obj_type;
      res.type := sym_id;
      IF ref THEN
       <* IF DEST_XDS THEN *>
        IF opt.AutoDetectActualType THEN
          tmp := res.location;
          INC(res.location, 4);
          sys.EVAL(exp.CheckActualType (res, FALSE));
          res.location := tmp;
        END;
       <* END *>
        IF NOT mem.Get (res.location, sys.ADR(res.location), 4) THEN
          RETURN FALSE;
        END;
      ELSE
       <* IF DEST_XDS THEN *>
        IF opt.AutoDetectActualType THEN
          exp.GetActualType (res);
        END;
       <* END *>
      END;
    | dt.Sy_Var:
      res.sort := exp.Variable;
      res.var_type := obj_type;
      ASSERT(tls.ObjectAddr (obj, res.location));
      res.type := sym_id;
    END;
    RETURN TRUE;
  END GetRes;

VAR
  size: crt.SZ;

  PROCEDURE write_line (num: CARDINAL);
  VAR
    res     : exp.ExprRes;
    str     : xs.String;
    buf     : xs.String;
    name    : xs.String;
    scope   : dt.OBJECT;
    obj     : dt.OBJECT;
    obj_tag : dt.SYM_TAG;
    type_tag: dt.TYPE_TAG;
    obj_type: dt.PTYPE;
    separate: BOOLEAN;
    offs    : CARDINAL;
    regname : xs.String;
    reg_no  : CARDINAL;
    reg_val : kt.REG_VALUE;
    ref     : BOOLEAN;
    location: xs.String;
    obj_attr: dt.SYM_ATTRIB;
  BEGIN
    IF GetLineLocalWindow (p^.N, num, scope, obj, separate) THEN
      IF separate THEN
        tls.ObjectName (scope, name);
        fmt.print (buf, '─ %s ', name);
        crt.SetPos (1, num-p^.frame + 1);
        crt.WrStrFromPos (hwnd, buf, p^.Colors^[crt.List_Line], p^.pos);
        offs := LENGTH(buf)+1;
        IF offs < size.x2 THEN
          crt.SetPos (offs, num-p^.frame + 1);
          crt.WrNChar (hwnd, size.x2-offs, '─', p^.Colors^[crt.List_Line]);
        END;
      ELSE
        IF body OR NOT tls.EqualObjects(dv.LocalScope, exe.ExecScope) THEN
          COPY('???', str);
        ELSE
          COPY('.', str);
        END;
        ASSERT(tls.ObjectSymTypeID(obj, res.type));
        ASSERT(tls.ObjectTag(obj, obj_tag));
        location := "";

        ASSERT(tls.ObjectAttr(obj, obj_attr));
        IF (dt.SA_Param IN obj_attr) THEN
          crt.SetPos (1, num-p^.frame + 1);
          crt.WrStr (hwnd, '>', crt.Attr(crt.Yellow, crt.Bg(p^.Colors^[crt.List_Line])));
        END;

        CASE obj_tag OF
        | dt.Sy_Register:
          crt.SetPos (1, num-p^.frame + 1);
          crt.WrStr (hwnd, '!', crt.Attr(crt.LightRed, crt.Bg(p^.Colors^[crt.List_Line])));
          IF GetRes (scope, obj, res) THEN
            ASSERT(tls.ObjectType (obj, obj_type));
            ASSERT(tls.TypeTag(obj_type, type_tag));
            WHILE type_tag = dt.Range DO
              tls.SubType(obj_type, obj_type);
              ASSERT(tls.TypeTag(obj_type, type_tag));
            END;
            IF opt.DisplayDerefencePointer AND (type_tag = dt.Pointer) THEN
              res.type := dt.st_dereference;
            END;
            exp.Res2Str (res, str);
            IF NOT tls.EqualObjects (exe.ExecScope, scope) THEN
              xs.Append (" ?", str);
            END;
            IF optionShowLocation THEN
              ASSERT (tls.GetLocalObject_Reg (obj, reg_no, ref));
              ASSERT (exe.GetRegName (reg_no, location));
            END;
          END;
        | dt.Sy_Relative, dt.Sy_Var:
          IF GetRes (scope, obj, res) THEN
            ASSERT(tls.TypeTag(res.var_type, type_tag));
            IF opt.DisplayDerefencePointer AND (type_tag = dt.Pointer) THEN
              res.type := dt.st_dereference;
            END;
            exp.Res2Str (res, str);
            IF optionShowLocation THEN
              IF obj_tag = dt.Sy_Relative THEN
                ASSERT (tls.GetLocalObject_Reg (obj, reg_no, ref));
                IF reg_no = kt.FRAME_REG THEN
                  -- FIX ME: dynamic frame pointer here!
                  COPY ("ESP", regname);
                  reg_val := mem.GetSP ();
                ELSE
                  ASSERT (exe.GetRegName (reg_no, regname));
                  ASSERT (mem.GetReg (reg_no, reg_val));
                END;
                IF reg_val < res.location THEN
                  fmt.print (buf, "%s (%s+0x%$2X)", exp.Fmt_ADDRval);
                  fmt.print (location, buf, res.location, regname, res.location-reg_val);
                ELSIF reg_val > res.location THEN
                  fmt.print (buf, "%s (%s-0x%$2X)", exp.Fmt_ADDRval);
                  fmt.print (location, buf, res.location, regname, reg_val-res.location);
                ELSE
                  fmt.print (buf, "%s (%s)", exp.Fmt_ADDRval);
                  fmt.print (location, buf, res.location, regname);
                END;
              ELSE
                fmt.print (location, exp.Fmt_ADDRval, res.location)
              END;
            END;
          END;
        END;
        tls.ObjectName (obj, name);
        IF optionShowLocation AND (location # "") THEN
          fmt.print (buf, '%-21s: %-10s %s', location, name, str);
        ELSE
          fmt.print (buf, '%-10s %s', name, str);
        END;
        crt.SetPos (2, num-p^.frame + 1);
        crt.WrStrFromPos (hwnd, buf, p^.Colors^[crt.List_Line], p^.pos);
      END;
    END;
  END write_line;


VAR
  i, len  : CARDINAL;
  last    : CARDINAL;
  x, y    : CARDINAL;
  subtype : dt.PTYPE;
  scope   : dt.OBJECT;
  obj     : dt.OBJECT;
  res     : exp.ExprRes;
  format  : xs.String;
  obj_tag : dt.SYM_TAG;
  type_tag: dt.TYPE_TAG;
  reg_no  : CARDINAL;
  reg_val : kt.REG_VALUE;
  ref     : BOOLEAN;
  buf     : xs.String;
  name    : xs.String;
  separate: BOOLEAN;
  equ     : xs.String;
  main    : BOOLEAN;
  item    : CARDINAL;

CONST
  ContextItem_ViewAs = Local_ContextItem_ShowLocation+1+1; -- prev+separator+item


  PROCEDURE ShowContext;
  BEGIN
    p^.actions[ContextItem_Type].tenable := act.TYPES_SET{};
    p^.actions[ContextItem_ViewAs-1] := act.EMPTY_CONTEXT;
    IF GetLineLocalWindow (p^.N, p^.curr, scope, obj, separate) AND NOT separate THEN
      IF GetRes (scope, obj, res) THEN
        IF NOT exp.Var2Value(res, res) THEN RETURN; END;
        CASE res.sort OF
        | exp.CARDval, exp.INTval, exp.WHOLEval:
          p^.actions[ContextItem_Type].tenable := act.TYPES_SET{dt.st_original, dt.st_unsigned, dt.st_signed, dt.st_hex, dt.st_oct, dt.st_bin };
        | exp.Variable:
          ASSERT(tls.TypeTag(res.var_type, type_tag));
          IF type_tag = dt.Pointer THEN
            p^.actions[ContextItem_Type].tenable := act.TYPES_SET{dt.st_original,dt.st_dereference};
          ELSIF type_tag = dt.Int THEN
            p^.actions[ContextItem_Type].tenable := act.TYPES_SET{dt.st_original, dt.st_unsigned, dt.st_signed, dt.st_hex, dt.st_bin };
          END;
        ELSE
        END;
        IF p^.N # 0 THEN
          IF std.MakeSubmenuViewAs (p^.actions[ContextItem_ViewAs]) THEN
            p^.actions[ContextItem_ViewAs-1] := act.CONTEXT{ act.separate };
            p^.actions[ContextItem_ViewAs+1] := act.EMPTY_CONTEXT;
          END;
        END;
      END;
    END;
  END ShowContext;


BEGIN
  p := win.GetAMPtr(hwnd);
  size := win.GetWindowSize(hwnd);
  InitData;
  CASE msg.ID OF
  | eve.Redraw, eve.Paint:
    IF (msg.par # 3) AND (msg.par # 4) THEN
      crt.FillWindow(hwnd, ' ', p^.Colors^[crt.List_Background])
    END;
    WITH size DO len := y2-y1-1; END;
    WITH p^ DO
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
    std.ListBox(hwnd, msg);

  | eve.DoAction:
    WITH p^ DO
      IF GetLineLocalWindow (N, curr, scope, obj, separate) AND NOT separate THEN
        CASE std.GetAction (msg.par) OF
        | act.ContextMenu:
          ShowContext;
        | act.ChangeTypes:
          ASSERT(tls.SetObjectSymTypeID(obj, act.ChangeTypesID));
          RETURN;
        | act.Dump:
          IF GetRes (scope, obj, res) AND (res.sort = exp.Variable) THEN
            fmt.print (dv.DumpAddrStr, exp.Fmt_ADDRval, res.location);
            act.ExecuteAction (act.Dump, act.mode_silent);
          ELSE
            act.ExecuteAction (act.Dump, act.mode_loud);
          END;
          RETURN;
        | act.AddWatch:
          exp.ObjectFullName (obj, dv.expr);
          act.ExecuteAction (act.AddWatch, act.mode_silent);
          RETURN;
        | act.Examine:
          exp.ObjectFullName (obj, dv.VarName);
          act.ExecuteAction (act.Examine, act.mode_silent);
          RETURN;
       <* IF DEST_XDS THEN *>
        | act.Access:
          IF GetRes (scope, obj, res) AND (res.sort = exp.Variable) THEN
            fmt.print (buf, exp.Fmt_ADDRval, res.location);
            ASSERT(tls.ObjectType(obj, subtype));
            dbrk.SetAccessAttr(buf, tls.TypeSize(subtype));
            act.ExecuteAction (act.Access, act.mode_silent);
          ELSE
            act.ExecuteAction (act.Access, act.mode_loud);
          END;
          RETURN;
       <* END *>
        ELSE
        END;
      END;
      std.ListBox (hwnd, msg);
    END;

  | eve.QueryAction:
    CASE std.GetAction (msg.par) OF
    | act.LocalVars:
      act.ConfirmQueryByCond (kexe.Loaded);
      -- IF kexe.Loaded & tls.IsObjectValid(exe.ExecScope) THEN
      --   act.ConfirmQueryByCond((tls.ParamVarsNo(exe.ExecScope)#0) OR (tls.LocalVarsNo(exe.ExecScope)#0));
      -- END;
    | act.ChangeTypes:
      IF GetLineLocalWindow (p^.N, p^.curr, scope, obj, separate) AND NOT separate THEN
        IF GetRes (scope, obj, res) AND exp.Var2Value(res, res) THEN
          CASE res.sort OF
          | exp.CARDval, exp.INTval, exp.WHOLEval:
            act.ConfirmQuery;
          | exp.Variable:
            ASSERT(tls.TypeTag(res.var_type, type_tag));
            act.ConfirmQueryByCond (type_tag IN dt.TYPE_TAG_SET {dt.Pointer, dt.Int, dt.Card});
          ELSE
          END;
        END;
      END;
    | act.Examine:
      -- prepare variable name...
      IF GetLineLocalWindow (p^.N, p^.curr, scope, obj, separate) AND NOT separate THEN
        exp.ObjectFullName (obj, dv.VarName);
      END;
      -- ...and all action by default handler!!!
      std.ListBox (hwnd, msg);
    | act.Dump:
      act.ConfirmQueryByCond ((p^.N > 0) AND GetRes (scope, obj, res) AND (res.sort = exp.Variable));
   <* IF DEST_XDS THEN *>
    | act.Access:
      act.ConfirmQueryByCond ((p^.N > 0) AND GetRes (scope, obj, res) AND (res.sort = exp.Variable));
   <* END *>
    ELSE
      std.ListBox (hwnd, msg);
    END;

  | eve.QueryItem:
    std.GetItem (msg.par, main, item);
    IF main THEN
      IF GetLineLocalWindow (p^.N, p^.curr, scope, obj, separate) AND NOT separate THEN
        CASE item OF
        | ContextItem_Pointed:
          ASSERT(tls.ObjectType(obj, res.var_type));
          ASSERT(tls.TypeTag (res.var_type, type_tag));
          act.ConfirmQueryByCond (type_tag = dt.Pointer);
        | ContextItem_ViewAs:
          ASSERT(tls.ObjectTag(obj, obj_tag));
          act.ConfirmQueryByCond ((obj_tag # dt.Sy_Register) AND (lst.EquNamesNo() > 0));
        END;
      END;
    ELSE
      act.ConfirmQueryByCond (item < lst.EquNamesNo());
    END;

  | eve.ContextItem:
    IF GetLineLocalWindow (p^.N, p^.curr, scope, obj, separate) AND NOT separate THEN
      std.GetItem (msg.par, main, item);
      IF main THEN
        CASE item OF
        | ContextItem_Pointed:
          IF GetRes (scope, obj, res) THEN
            IF res.sort = exp.Variable THEN
              fmt.print (buf, "ADDRESS<%s>", exp.Fmt_ADDRval);
              fmt.print (dv.DumpAddrStr, buf, res.location);
            ELSE
              ASSERT (res.sort = exp.Register);
              ASSERT (exe.GetRegName (res.reg_no, buf));
              fmt.print (dv.DumpAddrStr, "@%s", buf);
            END;
            act.ExecuteAction (act.Dump, act.mode_silent);
          END;
        | ContextItem_ViewAs:
          ASSERT(FALSE);
        END;
      ELSE
        lst.GetName (item, equ);
        lst.GetEquName (equ, buf);
        exp.ObjectFullName (obj, name);
        fmt.print (dv.VarName, "%s(%s)", buf, name);
        act.ExecuteAction (act.Examine, act.mode_silent);
      END;
    END;

  | eve.R_Mouse:
    ASSERT(win.GetMouse(msg, x, y));
    IF NOT std.CheckFrame(size, x, y) THEN
      eve.AddToTail(hwnd, eve.Mouse_Pressed, msg.par);
      eve.Flush;
      ShowContext;
      puw.PopupWindow(x, y, hwnd, p^.actions);
    END;

  | eve.QueryKbHit:
    CASE msg.par OF
    | key.CtrlDown:
      act.ConfirmQueryByCond (kexe.Loaded AND (dv.LocalScopeLevel+1 < stk.CallTop()));
    | key.CtrlUp:
      act.ConfirmQueryByCond (kexe.Loaded AND (dv.LocalScopeLevel > 0));
    ELSE
      std.ListBox(hwnd, msg);
    END;

  | eve.KbHit:
    WITH p^ DO
      CASE msg.par OF
      | key.CtrlDown:
        IF dex.GoDownCallStack() THEN
          eve.AddToTailIfValid (std.Wnds[std.CallWindow].hwnd, eve.Redraw, 0);
          eve.AddToTail (hwnd, eve.Redraw, 0);
        END;

      | key.CtrlUp:
        IF dex.GoUpCallStack() THEN
          eve.AddToTailIfValid (std.Wnds[std.CallWindow].hwnd, eve.Redraw, 0);
          eve.AddToTail (hwnd, eve.Redraw, 0);
        END;

      | key.Enter, key.CtrlEnter:
        IF NOT GetLineLocalWindow (N, curr, scope, obj, separate) OR
           separate OR NOT GetRes(scope, obj, res)
        THEN
          crt.Beep;
          RETURN;
        END;
        ASSERT(tls.ObjectTag (obj, obj_tag));
        CASE obj_tag OF
        | dt.Sy_Relative, dt.Sy_Var:
          ASSERT(tls.TypeTag (res.var_type, type_tag));
          COPY('%s', format);
          IF type_tag = dt.Pointer THEN
            tls.SubType(res.var_type, subtype);
            ASSERT(tls.TypeTag(subtype, type_tag));
            IF type_tag = dt.Procedure THEN
              res.var_type := subtype;
            ELSIF (msg.par = key.Enter) THEN
              IF NOT exp.Dereference(res, res) THEN
                std.ErrorNo (mes.NilPointerDereference);
                RETURN;
              END;
              xs.Append ('^', format);
            END;
          END;
          exp.ObjectFullName (obj, buf);
          fmt.print(dv.VarName, format, buf);
          IF NOT ChangeVariable(win.Invalid_H, res) THEN
            eve.AddToTail (std.ErrorMsg, eve.Rise, 0);
          END;

        | dt.Sy_Register:
          IF msg.par # key.Enter THEN
            crt.Beep;
            RETURN;
          END;
          ASSERT(tls.ObjectType(obj, res.var_type));
          ASSERT(tls.TypeTag(res.var_type, type_tag));
          ASSERT(tls.GetLocalObject_Reg(obj, reg_no, ref));
          IF (type_tag # dt.Pointer) AND NOT ref THEN
            crt.Beep;
            RETURN;
          END;
          ASSERT(mem.GetReg(reg_no, reg_val));
          res.sort := exp.Variable;
          ASSERT(tls.ObjectSymTypeID(obj, res.type));
          res.location := kt.ADDRESS(reg_val);
          COPY('%s', format);
          tls.SubType(res.var_type, res.var_type);
          ASSERT(tls.TypeTag(res.var_type, type_tag));
          IF type_tag = dt.Array_of THEN
            res.arr_desc := res.location;
            IF NOT exp.ArrayOf_Desc2Loc (res) THEN
              std.ErrorNo (mes.InvalidArrayDesc);
              RETURN;
            END;
          END;
         <* IF DEST_XDS THEN *>
          IF opt.AutoDetectActualType THEN
            exp.GetActualType (res);
          END;
         <* END *>
          IF type_tag # dt.Procedure THEN
            xs.Append ('^', format);
          END;
          exp.ObjectFullName (obj, buf);
          fmt.print(dv.VarName, format, buf);
          IF NOT ChangeVariable(win.Invalid_H, res) THEN
            eve.AddToTail(std.ErrorMsg, eve.Rise, 0);
          END;
        END;
      ELSE
        std.ListBox(hwnd, msg);
      END;
    END;
  ELSE
    std.ListBox(hwnd, msg);
  END;
END LocalVariablesHandler;


<* PUSH *>
<* WOFF301+ *>
PROCEDURE lctrLabels (hwnd: crt.HWND; i: CARDINAL; VAR str: ARRAY OF CHAR);
<* POP *>
VAR
  object: dt.OBJECT;
BEGIN
  object := tls.GetLabel(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, i);
  ASSERT(tls.IsObjectValid(object));
  nm.ObjectNameGetAndCorrect (object, str);
END lctrLabels;




PROCEDURE NextLabel (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  w   : win.HWND;
  i   : CARDINAL;
  ps  : kt.ADDRESS;
BEGIN
  ASSERT(action = act.NextProc);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  IF dv.MainMode # dv.source THEN RETURN FALSE; END;
  w := men.SkipMenus();
  IF w # std.Wnds[std.MainWindow].hwnd THEN RETURN FALSE; END;
  WITH mod.Curr^ DO
    IF mode # act.mode_check THEN
      FOR i := curr+1 TO N DO
        IF tls.AddrBySource(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, i+1, ps) THEN
          <* IF TARGET_VAX THEN *>
          IF ps > 2 THEN DEC(ps, 2); END; -- Смещение из-за маски входа
          <* END *>
          IF tls.IsLabelOnAddr(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, ps) THEN
            mod.SetCurrLine(i);
            RETURN TRUE;
          END;
        END;
      END;
    END;
  END;
  RETURN TRUE;
END NextLabel;


PROCEDURE PrevLabel (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  w   : win.HWND;
  i   : CARDINAL;
  ps : kt.ADDRESS;
BEGIN
  ASSERT(action = act.PrevProc);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  IF dv.MainMode # dv.source THEN RETURN FALSE; END;
  w := men.SkipMenus();
  IF w # std.Wnds[std.MainWindow].hwnd THEN RETURN FALSE; END;
  WITH mod.Curr^ DO
    IF curr = 0 THEN RETURN FALSE; END;
    IF mode # act.mode_check THEN
      FOR i := curr-1 TO 0 BY -1 DO
        IF tls.AddrBySource(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, i+1, ps) THEN
          <* IF TARGET_VAX THEN *>
          IF ps > 2 THEN DEC(ps, 2); END;
          <* END *>
          IF tls.IsLabelOnAddr(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, ps) THEN
            mod.SetCurrLine(i);
            RETURN TRUE;
          END;
        END;
      END;
    END;
  END;
  RETURN TRUE;
END PrevLabel;


VAR
  StrNum, AddrNum: std.MESSAGE; (* For search line by number and position by address *)


PROCEDURE GoToStr(hwnd: win.HWND):BOOLEAN;
VAR
  res: wstr.ConvResults;
  line: CARDINAL;
BEGIN
  wstr.StrToCard(StrNum, line, res);
  IF res # wstr.strAllRight THEN
    std.SetErrorMsgNo(mes.Incorrect_line);
    RETURN FALSE;
  END;
  IF (line = 0) OR (line > mod.Curr^.N) THEN
    std.SetErrorMsgNo(mes.ErrorLineOutOfBounds);
    RETURN FALSE
  END;
  mod.SetCurrLine(line - 1);
  eve.AddToTail(hwnd, eve.Hide, 0);
  eve.Flush;
  RETURN TRUE;
END GoToStr;

PROCEDURE GoToStrByAddr(hwnd: win.HWND):BOOLEAN;
VAR
  addr   : kt.ADDRESS;
BEGIN
  IF NOT exp.GetAddress(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, AddrNum, addr) THEN
    std.SetErrorMsgNo(mes.Expected_address);
    RETURN FALSE;
  END;
  IF NOT mod.SetNewPosByAddr(addr) THEN
    std.SetErrorMsgNo(mes.Cant_resolve);
    RETURN FALSE;
  END;
  eve.AddToTail(hwnd, eve.Hide, 0);
  eve.Flush;
  RETURN TRUE;
END GoToStrByAddr;


PROCEDURE StrByNum (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  w: win.HWND;
BEGIN
  ASSERT(action = act.GotoLine);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  IF dv.MainMode # dv.source THEN RETURN FALSE; END;
  w := men.SkipMenus();
  IF w # std.Wnds[std.MainWindow].hwnd THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    std.OpenUniversalDialog(mes.dlg_EnterLine, GoToStr, StrNum);
  END;
  RETURN TRUE;
END StrByNum;


PROCEDURE StrByAddr (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  w     : win.HWND;
BEGIN
  ASSERT(action = act.GotoAddr);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  w := men.SkipMenus();
  IF w # std.Wnds[std.MainWindow].hwnd THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    std.OpenUniversalDialog(mes.InputAddress, GoToStrByAddr, AddrNum);
  END;
  RETURN TRUE;
END StrByAddr;


PROCEDURE Go_Exec (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN; FORWARD;


PROCEDURE SetHeaderMainWindow;
VAR
  header: xs.String;
BEGIN
  WITH std.Wnds[std.MainWindow] DO
    IF hwnd # win.Invalid_H THEN
      CASE dv.MainMode OF
      | dv.source:
        COPY(act.ActionName[act.SourceMode], header);
      | dv.disasm, dv.need_disasm:
        COPY(act.ActionName[act.AssemblyMode], header);
      | dv.disasm_first:
        COPY(act.ActionName[act.MixMode], header);
      END;
      win.SetHeaderByStr(hwnd, header);
    END;
  END;
END SetHeaderMainWindow;


CONST
  Invalid_Name = '????????';


VAR
  selected_word: xs.String;


PROCEDURE tglCodeHighlight (check: BOOLEAN): BOOLEAN;
BEGIN
  RETURN std.tglOption (opt.CodeHilight, check);
END tglCodeHighlight;


PROCEDURE tglDisasmMode (check: BOOLEAN): BOOLEAN;
BEGIN
  RETURN std.tglOption (opt.DisasmMode, check);
END tglDisasmMode;


PROCEDURE tglCodeDump (check: BOOLEAN): BOOLEAN;
BEGIN
  RETURN std.tglOption (opt.Code, check);
END tglCodeDump;


<* IF DEST_XDS THEN *>

PROCEDURE tglStripFullPath (check: BOOLEAN): BOOLEAN;
BEGIN
  IF check THEN
    RETURN std.tglOption (opt.StripPathFromFullName, TRUE);
  ELSE
    ASSERT(std.tglOption (opt.StripPathFromFullName, FALSE));
    red.InitRedirection();
    mod.ResetTryRead;
    RETURN TRUE;
  END;
END tglStripFullPath;

PROCEDURE tglStripPartialPath (check: BOOLEAN): BOOLEAN;
BEGIN
  IF check THEN
    RETURN std.tglOption (opt.StripPathFromPartialName, TRUE);
  ELSE
    ASSERT(std.tglOption (opt.StripPathFromPartialName, FALSE));
    red.InitRedirection();
    mod.ResetTryRead;
    RETURN TRUE;
  END;
END tglStripPartialPath;

<* END *>


PROCEDURE ScrollAreaProc (hwnd: win.HWND; msg: eve.MSG);

VAR
  MainWindowContextList: act.CONTEXT_LIST;

CONST
  Watch  = 9;


  PROCEDURE SetMainWindowContextList;
  VAR
    name : act.CONTEXT_NAME;
    b_pos: CARDINAL;
    act1 : act.ACTION;
    act2 : act.ACTION;
    addr : kt.ADDRESS;
  BEGIN
    CASE dv.MainMode OF
    | dv.source:
      b_pos := dbrk.FindBreakPos (mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, mod.Curr^.curr);
    | dv.disasm, dv.need_disasm, dv.disasm_first:
      IF dsm.GetAddr (dsm.D_curr, addr) THEN
        b_pos := dbrk.FindBreakPosByAddr (addr);
      ELSE
        b_pos := MAX(CARDINAL);
      END;
    END;
    IF b_pos # MAX(CARDINAL) THEN
       IF brk.Breakpoints.Breakpoints^[b_pos].Kind = brk.counter THEN
         act1 := act.D_Sticky;
       ELSE
         act1 := act.Delete;
       END;
       IF brk.Breakpoints.Breakpoints^[b_pos].Break.Active THEN
         act2 := act.Disable;
       ELSE
         act2 := act.Enable;
       END;
    ELSE
      act1 := act.Sticky;
      act2 := act.Counter;
    END;
    CASE dv.MainMode OF
    | dv.source:
      MainWindowContextList[00] := act.CONTEXT{ act.do_action, act.MixMode };
      MainWindowContextList[01] := act.CONTEXT{ act.do_action, act.AssemblyMode };
      MainWindowContextList[02] := act.CONTEXT{ act.separate };
      MainWindowContextList[03] := act.CONTEXT{ act.do_action, act.GotoExec };
      MainWindowContextList[04] := act.CONTEXT{ act.do_action, act.Skip };
      MainWindowContextList[05] := act.CONTEXT{ act.separate };
      MainWindowContextList[06] := act.CONTEXT{ act.do_action, act1 };
      MainWindowContextList[07] := act.CONTEXT{ act.do_action, act2 };
      MainWindowContextList[08] := act.CONTEXT{ act.separate };
      IF selected_word # '' THEN
        IF LENGTH(selected_word) <= 15 THEN
          fmt.print(name, 'Watch "%s"', selected_word);
        ELSE
          fmt.print(name, 'Watch "%.15s..."', selected_word);
        END;
      ELSE
        name := act.ActionName[act.AddWatch];
      END;
      MainWindowContextList[Watch] := act.CONTEXT{ act.context_item, name };
      MainWindowContextList[10] := act.CONTEXT{ act.do_action, act.Find };
      MainWindowContextList[11] := act.CONTEXT{ act.do_action, act.GotoLine };
      MainWindowContextList[12] := act.CONTEXT{ act.do_action, act.GotoAddr };
      MainWindowContextList[13] := act.CONTEXT{ act.separate };
      MainWindowContextList[14] := act.CONTEXT{ act.toggler, 'Code highlight', tglCodeHighlight };
      MainWindowContextList[15] := act.EMPTY_CONTEXT;
     <* IF DEST_XDS THEN *>
      MainWindowContextList[15] := act.CONTEXT{ act.toggler, 'Strip full path', tglStripFullPath };
      MainWindowContextList[16] := act.CONTEXT{ act.toggler, 'Strip partial path', tglStripPartialPath };
      MainWindowContextList[17] := act.EMPTY_CONTEXT;
     <* END *>
    | dv.need_disasm, dv.disasm:
      MainWindowContextList[00] := act.CONTEXT{ act.do_action, act.SourceMode };
      MainWindowContextList[01] := act.CONTEXT{ act.do_action, act.MixMode };
      MainWindowContextList[02] := act.CONTEXT{ act.separate };
      MainWindowContextList[03] := act.CONTEXT{ act.do_action, act.GotoExec };
      MainWindowContextList[04] := act.CONTEXT{ act.do_action, act.Skip };
      MainWindowContextList[05] := act.CONTEXT{ act.separate };
      MainWindowContextList[06] := act.CONTEXT{ act.do_action, act1 };
      MainWindowContextList[07] := act.CONTEXT{ act.do_action, act2 };
      MainWindowContextList[08] := act.CONTEXT{ act.separate };
      MainWindowContextList[09] := act.CONTEXT{ act.toggler, 'Full disasm mode', tglDisasmMode };
      MainWindowContextList[10] := act.CONTEXT{ act.toggler, 'Code dump', tglCodeDump };
      MainWindowContextList[11] := act.EMPTY_CONTEXT;
    | dv.disasm_first:
      MainWindowContextList[00] := act.CONTEXT{ act.do_action, act.SourceMode };
      MainWindowContextList[01] := act.CONTEXT{ act.do_action, act.AssemblyMode };
      MainWindowContextList[02] := act.CONTEXT{ act.separate };
      MainWindowContextList[03] := act.CONTEXT{ act.do_action, act.GotoExec };
      MainWindowContextList[04] := act.CONTEXT{ act.do_action, act.Skip };
      MainWindowContextList[05] := act.CONTEXT{ act.separate };
      MainWindowContextList[06] := act.CONTEXT{ act.do_action, act1 };
      MainWindowContextList[07] := act.CONTEXT{ act.do_action, act2 };
      MainWindowContextList[08] := act.CONTEXT{ act.separate };
      MainWindowContextList[09] := act.CONTEXT{ act.toggler, 'Full disasm mode', tglDisasmMode};
      MainWindowContextList[10] := act.CONTEXT{ act.toggler, 'Code dump', tglCodeDump };
      MainWindowContextList[11] := act.EMPTY_CONTEXT;
    <* IF DEST_XDS THEN *>
      MainWindowContextList[11] := act.CONTEXT{ act.toggler, 'Strip full path', tglStripFullPath };
      MainWindowContextList[12] := act.CONTEXT{ act.toggler, 'Strip partial path', tglStripPartialPath };
      MainWindowContextList[13] := act.EMPTY_CONTEXT;
    <* END *>
    END;
  END SetMainWindowContextList;

  PROCEDURE DisplayBreak(b_pos, i: CARDINAL);
  VAR
    cnt: CARDINAL;
    attr: crt.ATTR;
  BEGIN
    IF b_pos # MAX(CARDINAL) THEN
      WITH brk.Breakpoints.Breakpoints^[b_pos] DO
        IF Break.Active THEN
          attr := crt.Src[crt.Src_Break];
        ELSE
          attr := crt.Src[crt.Src_BreakDisabled];
        END;
        IF Break.Pass > 0 THEN
          cnt := Break.Pass;
          crt.SetPos(0,i);
          CASE cnt OF
          | 0..9:
            crt.WrChar(hwnd, CHAR((cnt MOD 10) + ORD('0')), attr);
          ELSE
            crt.WrChar(hwnd, '*', attr);
          END;
        END;
        crt.SetPos(1,i);
        IF (Break.Sticky) THEN
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
      END;
    END;
  END DisplayBreak;

CONST
  Empty = '';

VAR
  size : crt.SZ;
  str  : xs.txt_ptr;
  b_pos: CARDINAL;
  ind  : CARDINAL;
  attr : crt.ATTR;
  len  : CARDINAL;
  buf  : xs.String;
  POS  : CARDINAL;
  is_found: BOOLEAN;



  PROCEDURE Disasm (curr_addr: kt.ADDRESS);

  CONST
    unknown = '???';

  VAR
    i           : CARDINAL;
    com         : dt.ComNo;
    m           : dt.ModNo;
    mn          : xs.txt_ptr;
    name        : xs.String;
    str         : xs.txt_ptr;
    str1        : xs.String;
    addr        : kt.ADDRESS;
    scope       : dt.OBJECT;
    exec_cursor : CARDINAL;
    mod_name    : xs.String;

  BEGIN
    ASSERT (dsm.GetAddr (dsm.D_curr, curr_addr));

    IF tls.FindModByAddr (curr_addr, com, m) THEN
      ASSERT (tls.ModName (com, m, mn));
      COPY (mn^, mod_name);
    ELSE
      IF NOT tls.FindComponentByAddr(curr_addr, com) THEN
        COPY (unknown, mod_name);
      ELSE
        ASSERT(tls.ComName(com, mn));
        COPY (mn^, mod_name);
      END;
    END;

    scope := tls.FindProcByAddr(com, m, curr_addr);
    IF tls.IsObjectValid(scope) THEN
      nm.ObjectNameGetAndCorrect (scope, name);
      ASSERT(tls.ObjectAddr(scope, addr));
      IF curr_addr > addr THEN
        fmt.print(str1, '%s.%s + 0%XH', mod_name, name, curr_addr-addr);
      ELSE
        fmt.print(str1, '%s.%s', mod_name, name);
      END;
    ELSIF tls.FindPublicByAddr(curr_addr, FALSE, com, str) THEN
      ASSERT(tls.FindPublicByNameInCom(com, str^, addr));
      IF curr_addr > addr THEN
        fmt.print(str1, '%s.%s + 0%XH', mod_name, str^, curr_addr-addr);
      ELSE
        fmt.print(str1, '%s.%s', mod_name, str^);
      END;
    ELSE
      fmt.print(str1, '%s', mod_name);
    END;

    IF hwnd = win.ActiveWindow THEN
      attr := crt.Src[crt.Src_ActiveHeader]
    ELSE
      attr := crt.Src[crt.Src_Header]
    END;
    crt.SetPos(1,0);
    crt.WrStr(hwnd, str1, attr);
    crt.Lite(hwnd, 0, 0, attr);
    exec_cursor := 0;
    FOR i := 1 TO len DO
      IF dsm.Dasm(dsm.D_frame + i - 1, mod.Curr^.pos+size.x2-size.x1-1, str1) THEN
        attr := crt.Src[crt.Src_Code];
      ELSE
        attr := crt.Src[crt.Src_Disasm];
        IF dsm.GetAddr(dsm.D_frame + i - 1, curr_addr) THEN
          IF exe.ExecAddr = curr_addr THEN exec_cursor := i; END;
          DisplayBreak(dbrk.FindBreakPosByAddr(curr_addr), i);
          IF opt.CallHilight AND stk.IsAddrInCallStack (curr_addr) THEN
            attr := crt.Src[crt.Src_CallCursor];
          END;
        END;
      END;
      IF mod.Curr^.pos >= LENGTH(str1) THEN
        str := sys.ADR(Empty);
      ELSE
        str := sys.ADR(str1[mod.Curr^.pos]);
      END;
      crt.SetPos(2, i);
      crt.WrStr(hwnd, str^, attr);
      crt.Lite(hwnd, i, 2, attr);
    END;
    crt.Lite(hwnd, dsm.D_curr-dsm.D_frame+1, 2, crt.Src[crt.Src_UserCursor]);
    IF exec_cursor # 0 THEN
      crt.Lite(hwnd, exec_cursor, 2, crt.Src[crt.Src_ExecCursor]);
    END;
  END Disasm;



  PROCEDURE Source;
  VAR
    i    : CARDINAL;
    addr : kt.ADDRESS;
    tmp  : xs.String;
    name : xs.String;
    tmp2 : ARRAY [0..8] OF CHAR;
    scope: dt.OBJECT;
    str  : xs.txt_ptr;
    mname: xs.String;
   <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
    un,ln_b,ln_e : CARDINAL;
   <* END *>

  CONST
    EmptyStr = '';

(*
  VAR
    center, delta: CARDINAL;
*)

  BEGIN
    WITH mod.Curr^ DO
(*
      -- позиционирование курсора в середину окна
      IF (exe.ExecComp = Pos.ComN) AND (exe.ExecMod = Pos.ModN) AND (exe.ExecLine >= frame) AND (exe.ExecLine <= frame+len-1) THEN
        center := frame+(len DIV 2)-1;
        IF center < exe.ExecLine THEN
          delta := exe.ExecLine-center;
          INC (frame, delta);
        ELSE
          delta := center-exe.ExecLine;
          DEC (frame, delta);
        END;
      END;
*)
      IF tls.AddrBySource(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, curr + 1, addr) THEN
        scope := tls.FindProcByAddr(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, addr);
        fmt.print(tmp2, '%$8X', addr);;
      ELSE
        scope := dt.Invalid_Object;
        COPY('????????', tmp2);
      END;
      IF tls.ModName (mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, str) THEN
        COPY (str^, mname);
      ELSE
        COPY (Invalid_Name, mname);
      END;
      IF tls.IsObjectValid (scope) THEN
        nm.ObjectNameGetAndCorrect (scope, name);
        fmt.print (tmp, '%s.%s', mname, name);
      ELSE
        fmt.print(tmp, '%s', mname);
      END;
      fmt.print(buf, '%-29.29s | L%$4u | A%s', tmp, curr+1, tmp2);
      IF hwnd = win.ActiveWindow THEN
        attr := crt.Src[crt.Src_ActiveHeader]
      ELSE
        attr := crt.Src[crt.Src_Header]
      END;
      crt.SetPos(1,0);
      crt.WrStr(hwnd, buf, attr);
      crt.Lite(hwnd, 0, 0, attr);

      IF (msg.par # 3) AND (msg.par # 4) THEN
        FOR i := 1 TO len DO
          IF  frame+i-1 < N THEN
            str := tls.GetSourceLine(Pos.ComN, Pos.ModN, frame+i-1);
          ELSE
            str := sys.ADR(EmptyStr);
          END;
          <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
          (* проверка на точки останова профилировщика                          СHERN*)
          IF bls.isMyLine(frame+i,Pos.ComN,Pos.ModN) THEN
             IF tim.InstByLine(Pos.ComN,frame+i,Pos.ModN) THEN
               attr := crt.Attr(crt.Red,crt.LightGreen);
               crt.SetPos(2,i);
               IF tim.GetBeg(Pos.ComN,ln_b,un) AND tim.GetEnd(Pos.ComN,ln_e) THEN
                IF Pos.ModN=un THEN
                 IF (frame+i = ln_b) OR
                    (frame+i = ln_e) THEN
                        (*      crt.SetPos(2,i);   *)
                              crt.WrChar(hwnd, '#', attr);
                 ELSE
                      crt.WrChar(hwnd, ' ', attr);
                 END;
                END;
               END;
             ELSE
                         attr := crt.Attr(crt.Red,crt.LightGray);(*crt.Src[crt.Src_Break];*)
                         crt.SetPos(2,i);
                         crt.WrChar(hwnd, '!', attr);
             END;
          END;                                                     (* CHERN *)
         <* END *>
          b_pos := dbrk.FindBreakPos(Pos.ComN, Pos.ModN, frame+i-1);
          DisplayBreak(b_pos, i);
         <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
          crt.SetPos(3,i);
         <* ELSE *>
          crt.SetPos(2,i);
         <* END *>
          attr := crt.Src[crt.Src_Code];
          IF opt.CodeHilight AND NOT tls.AddrBySource(Pos.ComN, Pos.ModN, frame+i, addr) THEN
            attr := crt.Src[crt.Src_NoCode];
          END;
          IF pos >= LENGTH(str^) THEN
            str := sys.ADR(Empty);
          ELSE
            str := sys.ADR(str^[pos]);
          END;
          crt.WrStr(hwnd, str^, attr);
          IF opt.CallHilight AND stk.IsLineInCallStack (Pos.ComN, Pos.ModN, frame+i) THEN
            attr := crt.Src[crt.Src_CallCursor];
            crt.Lite(hwnd, i, 2, attr);
          END;
        END;
      END;
      IF (curr >= frame) AND (curr <= frame+len-1) THEN
        IF msg.par = 4 THEN
          attr := crt.Src[crt.Src_Code];
          IF opt.CodeHilight AND NOT tls.AddrBySource(Pos.ComN, Pos.ModN, curr+1, addr) THEN
            attr := crt.Src[crt.Src_NoCode];
          END;
          IF opt.CallHilight AND stk.IsLineInCallStack (Pos.ComN, Pos.ModN, curr+1) THEN
            attr := crt.Src[crt.Src_CallCursor];
          END;
         <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
          crt.Lite(hwnd, curr-frame+1, 3, attr);                        (*  SCHERN *)
        ELSE
          crt.Lite(hwnd, curr-frame+1, 3, crt.Src[crt.Src_UserCursor]);
         <* ELSE *>
          crt.Lite(hwnd, curr-frame+1, 2, attr);
        ELSE
          crt.Lite(hwnd, curr-frame+1, 2, crt.Src[crt.Src_UserCursor]);
         <* END *>
        END;
      END;
      IF (exe.ExecComp = Pos.ComN) AND (exe.ExecMod = Pos.ModN) AND (exe.ExecLine >= frame) AND (exe.ExecLine <= frame+len-1) THEN
        crt.Lite (hwnd, exe.ExecLine - frame+1, 2, crt.Src[crt.Src_ExecCursor]);
      END;
    END;
  END Source;


  PROCEDURE MalevichSquare;

  CONST
    unknown = '???';

   <* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME
    line1 = "Черный квадрат";
    line2 = "Малевич";
   <* ELSE *>
    line1 = "Instruction pointer is out";
    line2 = "of known executeble segments";

    PROCEDURE write_line (w, y: CARDINAL; line: ARRAY OF CHAR);
    VAR
      l, x: CARDINAL;
    BEGIN
      l := LENGTH(line);
      IF w > l THEN
        x := (w-l) DIV 2;
      ELSE
        x := 0;
      END;
      crt.SetPos (x, y);
      crt.WrStr (hwnd, line, attr);
      crt.Lite (hwnd, y, x, attr);
    END write_line;

  VAR
    RegName: xs.String;
    header : xs.String;
    l, y   : CARDINAL;

   <* END *>

  VAR
    w, x   : CARDINAL;

  BEGIN
   <* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME
    attr := crt.Src[crt.Src_Disasm];
    crt.SetPos (0, 0);
    crt.WrStr (hwnd, line1, attr);
    crt.Lite (hwnd, 0, 0, attr);
    w := size.x2-size.x1;
    IF w > LENGTH(line2) THEN
      x := w-LENGTH(line2)+1;
    ELSE
      x := 0;
    END;
    crt.SetPos (x, len);
    crt.WrStr (hwnd, line2, attr);
    crt.Lite (hwnd, len, x, attr);
   <* ELSE *>
    ASSERT(exe.GetRegName (kt.EIP, RegName));
    fmt.print (header, '??? %s=%$8X', RegName, mem.GetIP());
    IF hwnd = win.ActiveWindow THEN
      attr := crt.Src[crt.Src_ActiveHeader]
    ELSE
      attr := crt.Src[crt.Src_Header]
    END;
    crt.SetPos(1,0);
    crt.WrStr(hwnd, header, attr);
    crt.Lite(hwnd, 0, 0, attr);
    attr := crt.Attr (crt.LightRed, crt.Bg(crt.Src[crt.Src_Disasm]));
    w := size.x2-size.x1;
    y := len DIV 2;
    write_line (w, y, line1);
    INC (y);
    write_line (w, y, line2);
   <* END *>
  END MalevichSquare;



VAR
  i         : CARDINAL;
  fmt_str   : ARRAY [0..50] OF CHAR;
  x, y      : CARDINAL;
  buffer    : xs.String;
  address   : kt.ADDRESS;
  com, m, ln: CARDINAL;
  action    : act.ACTION;
  start, end: CARDINAL;
  main      : BOOLEAN;
  item      : CARDINAL;
BEGIN
  IF kexe.Loaded THEN
    WITH mod.Curr^ DO
      IF N = MAX(CARDINAL) THEN
        N := tls.LastSourceLine(Pos.ComN, Pos.ModN);
        i:= tls.LastLineHasCode (Pos.ComN, Pos.ModN);
        IF i > N THEN
          N := i;
        END;
        IF (N <= curr) OR (N <= frame) THEN
          curr  := 0;
          frame := 0;
          size := win.GetWindowSize(hwnd);
          std.Normalize(size, curr, frame, N);
        END;
      END;
    END;
  END;
  CASE msg.ID OF
  | eve.Mouse_Pressed, eve.Mouse_Dbl, eve.Mouse_Moved:
    size := win.GetWindowSize(hwnd);
    ASSERT(win.GetMouse(msg, x, y));
    IF std.CheckFrame(size, x, y) THEN
      std.DefaultProc(hwnd, msg);
    ELSE
      ASSERT(win.GetRelMouse(hwnd, msg, x, y));
      CASE dv.MainMode OF
      | dv.source:
          WITH mod.Curr^ DO
            IF frame + y-1 <= N THEN
              curr := frame + (y-1);
              eve.AddToTail(hwnd, eve.Redraw, 0);
              CASE msg.ID OF
              | eve.Mouse_Moved:
              | eve.Mouse_Dbl:
                IF NOT (x <= 1) THEN
                  eve.AddToTail(hwnd, eve.DoAction, ORD(act.Skip));
                END;
              ELSE
                IF x <= 1 THEN
                  b_pos := dbrk.FindBreakPos(Pos.ComN, Pos.ModN, frame+y-1);
                  IF b_pos # MAX(CARDINAL) THEN
                    IF brk.Breakpoints.Breakpoints^[b_pos].Break.Active THEN
                      act.ExecuteAction (act.Disable, act.mode_silent);
                    ELSE
                      act.ExecuteAction (act.Delete, act.mode_silent);
                    END;
                  ELSE
                    act.ExecuteAction (act.Sticky, act.mode_silent);
                  END;
                END;
              END;
            END;
          END;
      | dv.disasm, dv.need_disasm, dv.disasm_first:
          IF dsm.GetAddr(dsm.D_frame+y-1, address) THEN
            dsm.D_curr := dsm.D_frame+y-1;
            eve.AddToTail(hwnd, eve.Redraw, 0);
            CASE msg.ID OF
            | eve.Mouse_Moved:
            | eve.Mouse_Dbl:
              eve.AddToTail(hwnd, eve.DoAction, ORD(act.Skip));
            ELSE
              IF x <= 1 THEN
                b_pos := dbrk.FindBreakPosByAddr(address);
                IF b_pos # MAX(CARDINAL) THEN
                  IF brk.Breakpoints.Breakpoints^[b_pos].Break.Active THEN
                    act.ExecuteAction (act.Disable, act.mode_silent);
                  ELSE
                    act.ExecuteAction (act.Delete, act.mode_silent);
                  END;
                ELSE
                  act.ExecuteAction (act.Sticky, act.mode_silent);
                END;
              ELSIF x <= 9 THEN
                fmt.print(AddrNum, exp.Fmt_ADDRval, address);
                act.ExecuteAction (act.GotoAddr, act.mode_silent);
              END;
            END;
          END;
      END;
    END;

  | eve.R_Mouse_Pressed:
    ASSERT(win.GetMouse(msg, i, y));
    size := win.GetWindowSize(hwnd);
    IF NOT std.CheckFrame(size, i, y) THEN
      x := i;
      IF x - size.x1 <= 9 THEN INC(x, 10); END;
      msg.par := x*10000H + y;
      eve.AddToTail(hwnd, eve.Mouse_Pressed, msg.par);
      eve.Flush;
      crt.LitePart(hwnd, y - size.y1, i - size.x1, i - size.x1+1, crt.Attr(crt.Black, crt.Yellow));
      crt.UpdateRect(size);
    END;

  | eve.R_Mouse:
    ASSERT(win.GetMouse(msg, x, y));
    size := win.GetWindowSize(hwnd);
    IF NOT std.CheckFrame(size, x, y) THEN
      IF crt.FindWord(x, y, start, end, selected_word) THEN
        crt.LitePart(hwnd, y - size.y1, start - size.x1, end - size.x1+1, crt.Attr(crt.Black, crt.Yellow));
        crt.UpdateRect(size);
      END;
      SetMainWindowContextList;
      puw.PopupWindow(x, y, hwnd, MainWindowContextList);
    END;

  | eve.Rise:
    SetHeaderMainWindow;
    std.DefaultProc(hwnd, msg);

  | eve.Paint, eve.Redraw:
    size := win.GetWindowSize(hwnd);
    IF (msg.par # 3) AND (msg.par # 4) THEN
      crt.FillWindow(hwnd,' ', crt.Src[crt.Src_Background]);
    END;
    IF kexe.Loaded THEN
      WITH mod.Curr^ DO
        WITH size DO len := y2-y1; END;
        CASE dv.MainMode OF
        | dv.source:
          Source;
          ind := dbrk.FindBreakPos(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, curr);
        | dv.disasm
        , dv.disasm_first
        , dv.need_disasm:
          IF dsm.GetAddr (dsm.D_curr, address) THEN
            dsm.ShiftFrame(0, size.y2-size.y1);
            ind := dbrk.FindBreakPosByAddr (address);
            Disasm (address);
          ELSE
            MalevichSquare;
            ind := MAX(CARDINAL);
          END;
        END;
        IF hwnd = win.ActiveWindow THEN
          attr := crt.Src[crt.Src_ActiveHeader]
        ELSE
          attr := crt.Src[crt.Src_Header]
        END;
        crt.SetPos(51,0);
        IF size.x2-size.x1 > 50 THEN
          IF ind # MAX(CARDINAL) THEN
            crt.WrNChar(hwnd, size.x2-size.x1-50, ' ', attr);
            WITH brk.Breakpoints.Breakpoints^[ind] DO
              CASE Kind OF
              | brk.counter:
                pro.GetMsg(mes.hdr_Counter, fmt_str);
                fmt.print(buf, fmt_str, Break.Pass);
                crt.WrStr(hwnd, buf, attr);
                crt.Lite(hwnd, 0, 0, attr);
              | brk.watchpoint:
                fmt.print(buf, '| %s', act.ActionName[act.Watchpoint]);
                crt.WrStr(hwnd, buf, attr);
                crt.Lite(hwnd, 0, 0, attr);
              | brk.normal:
                IF Condition # NIL THEN
                  pro.GetMsg(mes.hdr_Condition, fmt_str);
                  fmt.print(buf, fmt_str, Condition^);
                  crt.WrStr(hwnd, buf, attr);
                  crt.Lite(hwnd, 0, 0, attr);
                ELSIF Break.Pass # 0 THEN
                  pro.GetMsg(mes.hdr_Pass_Left, fmt_str);
                  fmt.print(buf, fmt_str, Break.Pass);
                  crt.WrStr(hwnd, buf, attr);
                  crt.Lite(hwnd, 0, 0, attr);
                END;
              END;
            END;
          END;
        END;
      END;
    END;
    IF msg.ID = eve.Redraw THEN crt.UpdateRect(size) END;

  | eve.Search:
    IF kexe.Loaded THEN
      WITH mod.Curr^ DO
        IF N = 0 THEN RETURN; END;
        IF msg.par = 0 THEN
          FOR i:= curr+1 TO N-1 DO
            str := tls.GetSourceLine(Pos.ComN, Pos.ModN, i);
            COPY(str^, buffer);
            xs.Uppercase(buffer);
            st.FindNext(SearchStr, buffer, 0, is_found, POS);
            IF is_found THEN
              mod.SetCurrLine(i);
              found := TRUE;
              eve.AddToTail(hwnd, eve.Redraw, 0);
              RETURN;
            END;
          END;
        ELSE
          IF curr > 0 THEN
            FOR i:= curr-1 TO 0 BY -1 DO
              str := tls.GetSourceLine(Pos.ComN, Pos.ModN, i);
              st.FindNext(SearchStr, str^, 0, is_found, POS);
              IF is_found THEN
                mod.SetCurrLine(i);
                found := TRUE;
                eve.AddToTail(hwnd, eve.Redraw, 0);
                RETURN;
              END;
            END;
          END;
        END;
      END;
    END;

 | eve.DoAction, eve.QueryAction:
    action := std.GetAction (msg.par);
    CASE action OF
    | act.SourceMode:
      IF NOT kexe.Loaded THEN RETURN; END;
      CASE dv.MainMode OF
      | dv.source:
        RETURN;
      | dv.need_disasm, dv.disasm, dv.disasm_first:
        IF NOT dsm.GetAddr (dsm.D_curr, address) OR
           NOT tls.SourceByAddr (address, com, m, ln)
        THEN
          IF msg.ID = eve.DoAction THEN
            crt.Beep;
          END;
          RETURN;
        END;
      END;
      IF msg.ID = eve.QueryAction THEN
        act.ConfirmQuery;
      ELSE
        mod.Curr^.pos := 0;
        dv.SetMainMode(dv.source);
        mod.SetNewPos(com, m, ln-1);
        SetHeaderMainWindow;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      END;

    | act.AssemblyMode, act.MixMode:
      IF NOT kexe.Loaded THEN
        RETURN;
      END;
      address := 0;
      CASE dv.MainMode OF
      | dv.source:
        IF NOT tls.AddrBySource(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, mod.Curr^.curr+1, address) THEN
          -- try to find nearest line below
          i:= 1;
          LOOP
            IF i > 10 THEN
              -- not found from the current lines to 10 ones down
              IF msg.ID = eve.DoAction THEN
                crt.Beep;
              END;
              RETURN;
            END;
            IF tls.AddrBySource(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, mod.Curr^.curr+1+i, address) THEN
              EXIT;
            ELSE
             INC(i);
            END;
          END;
        END;
      | dv.need_disasm:
        IF msg.ID = eve.DoAction THEN
          crt.Beep;
        END;
        RETURN;
      | dv.disasm:
        IF action = act.AssemblyMode THEN
          RETURN;
        END;
        IF msg.ID = eve.DoAction THEN
          IF NOT dsm.GetAddr (dsm.D_curr, address) THEN
            crt.Beep;
            RETURN;
          END;
        END;
      | dv.disasm_first:
        IF action = act.MixMode THEN RETURN; END;
        IF msg.ID = eve.DoAction THEN
          IF NOT dsm.GetAddr (dsm.D_curr, address) THEN
            crt.Beep;
            RETURN;
          END;
        END;
      END;
      IF msg.ID = eve.QueryAction THEN
        act.ConfirmQuery;
      ELSE
        mod.Curr^.pos := 0;
        size := win.GetWindowSize(hwnd);
        IF action = act.AssemblyMode THEN
          dv.SetMainMode(dv.disasm);
          dsm.SetDisasmMode(dsm.D);
          ASSERT(dsm.SetNewPos(address, size.y2-size.y1));
        ELSE
          dsm.SaveCurrentState;
          dsm.SetDisasmMode(dsm.DS);
          IF dsm.SetNewPos(address, size.y2-size.y1) THEN
            dv.SetMainMode(dv.disasm_first);
          ELSE
            dsm.RestoreCurrentState;
            crt.Beep;
          END
        END;
        SetHeaderMainWindow;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      END;

    | act.ContextMenu:
      IF msg.ID = eve.QueryAction THEN
        act.ConfirmQuery;
      ELSE
        SetMainWindowContextList;
        size := win.GetWindowSize(hwnd);
        WITH size DO
          puw.PopupWindow(x1+(x2-x1) DIV 2, y1+(y2-y1) DIV 2, hwnd, MainWindowContextList);
        END;
      END;

    ELSE
      std.DefaultProc(hwnd,msg);
    END;

  | eve.QueryItem:
    std.GetItem (msg.par, main, item);
    ASSERT(main);
    ASSERT(item = Watch);
    act.ConfirmQueryByCond (dv.MainMode = dv.source);

  | eve.ContextItem:
    std.GetItem (msg.par, main, item);
    ASSERT(main);
    ASSERT(item = Watch);
    IF selected_word # '' THEN
      COPY (selected_word, dv.expr);
      act.ExecuteAction (act.AddWatch, act.mode_silent);
    ELSE
      act.ExecuteAction (act.AddWatch, act.mode_loud);
    END;

 | eve.KbHit, eve.QueryKbHit:
    CASE msg.par OF
    | key.Left:
      IF NOT kexe.Loaded THEN RETURN END;
      IF msg.ID = eve.QueryKbHit THEN
        act.ConfirmQuery;
      ELSE
        WITH mod.Curr^ DO
          IF pos > 0 THEN
            DEC(pos)
          END;
          eve.AddToTail(hwnd, eve.Redraw, 0);
        END;
      END;

    | key.Right:
      IF NOT kexe.Loaded THEN RETURN END;
      IF msg.ID = eve.QueryKbHit THEN
        act.ConfirmQuery;
      ELSE
        WITH mod.Curr^ DO
          INC(pos);
          eve.AddToTail(hwnd, eve.Redraw, 0);
        END;
      END;

    | key.Home:
      IF NOT kexe.Loaded THEN RETURN END;
      IF dv.MainMode # dv.source THEN RETURN END;
      IF msg.ID = eve.QueryKbHit THEN
        act.ConfirmQuery;
      ELSE
        WITH mod.Curr^ DO
          pos := 0;
          eve.AddToTail(hwnd, eve.Redraw, 0);
        END;
      END;

    | key.End:
      IF NOT kexe.Loaded THEN RETURN END;
      IF dv.MainMode # dv.source THEN RETURN END;
      IF msg.ID = eve.QueryKbHit THEN
        act.ConfirmQuery;
      ELSE
        WITH mod.Curr^ DO
          size := win.GetWindowSize(hwnd);
          len  := size.x2-size.x1-2;
          str := tls.GetSourceLine(Pos.ComN, Pos.ModN, curr);
          IF LENGTH(str^) > len THEN
            pos := LENGTH(str^) - len;
          ELSE
            pos := 0;
          END;
          eve.AddToTail(hwnd, eve.Redraw, 0);
        END;
      END;

    | key.Up, key.Down, key.PgUp, key.PgDn,
      key.CtrlHome, key.CtrlEnd, key.CtrlPgUp, key.CtrlPgDn :

      IF NOT kexe.Loaded THEN RETURN END;
      IF msg.ID = eve.QueryKbHit THEN
        act.ConfirmQuery;
      ELSE
        size := win.GetWindowSize(hwnd);
        WITH size DO
          len := size.y2 - size.y1;
          CASE dv.MainMode OF
          | dv.source:
            WITH mod.Curr^ DO
              eve.AddToTail(hwnd, eve.Paint,  4);
              eve.Flush;
              IF std.Shift (msg.par, N, y2-y1 , curr, frame) THEN
                eve.AddToTail(hwnd, eve.Redraw,  3);
              ELSE
                eve.AddToTail(hwnd, eve.Redraw,  0);
              END;
            END;
          | dv.disasm, dv.need_disasm:
            IF dsm.GetAddr (dsm.D_curr, address) THEN
              CASE msg.par OF
              | key.Down:
                IF dsm.D_curr = dsm.D_frame + len - 1 THEN
                  dsm.ShiftFrame (1, len);
                ELSIF dsm.GetAddr(dsm.D_curr+1, address) THEN
                  INC(dsm.D_curr);
                END;
              | key.Up:
                IF dsm.D_curr = dsm.D_frame THEN
                  dsm.ShiftFrame (-1, len);
                ELSE
                  DEC(dsm.D_curr);
                END;
              | key.PgDn:
                dsm.ShiftFrame (len, len);
              | key.PgUp:
                dsm.ShiftFrame (-VAL(INTEGER, len), len);
              ELSE
              END;
              eve.AddToTail(hwnd, eve.Redraw,  0);
            END;
          | dv.disasm_first:
            IF dsm.GetAddr (dsm.D_curr, address) THEN
              CASE msg.par OF
              | key.Down:
                IF (dsm.D_curr = dsm.D_frame+len-1) OR
                   ((dsm.D_curr = dsm.D_frame+len-2) AND NOT dsm.GetAddr(dsm.D_curr+1, address)) THEN
                  dsm.ShiftFrame(1, len);
                ELSIF dsm.GetAddr(dsm.D_curr+1, address) THEN
                  INC(dsm.D_curr);
                ELSIF (dsm.D_curr < dsm.D_frame+len-2) AND dsm.GetAddr(dsm.D_curr+2, address) THEN
                  INC(dsm.D_curr,2);
                END;
              | key.Up:
                IF (dsm.D_curr = dsm.D_frame) OR
                   ((dsm.D_curr = dsm.D_frame+1) AND NOT dsm.GetAddr(dsm.D_frame, address)) THEN
                  dsm.ShiftFrame(-1, len);
                ELSIF dsm.GetAddr(dsm.D_curr-1, address) THEN
                  DEC(dsm.D_curr);
                ELSIF (dsm.D_curr > dsm.D_frame+1) AND dsm.GetAddr(dsm.D_curr-2, address) THEN
                  DEC(dsm.D_curr,2);
                END;
              | key.PgDn:
                dsm.ShiftFrame (len, len);
              | key.PgUp:
                dsm.ShiftFrame (-VAL(INTEGER, len), len);
              ELSE
              END;
              eve.AddToTail(hwnd, eve.Redraw,  0);
            END;
          ELSE
          END;
        END;
      END;

    | key.CtrlW:
      IF msg.ID = eve.QueryKbHit THEN
        act.ConfirmQuery;
      ELSE
        std.Move_Resize_kbd (hwnd, 0, 0);
        CASE dv.MainMode OF
        | dv.source:
          IF kexe.Loaded THEN
            mod.SetCurrLine(mod.Curr^.curr);
          END;
        | dv.disasm:
          dsm.D_curr := dsm.D_frame;
        | dv.disasm_first:
          dsm.D_curr := dsm.D_frame;
          IF NOT dsm.GetAddr(dsm.D_curr, address) THEN
            INC(dsm.D_curr);
          END;
        ELSE
        END;
      END;

    | key.CtrlUp:
      IF msg.ID = eve.QueryKbHit THEN
        act.ConfirmQueryByCond(act.QueryAction(act.PrevProc));
      ELSE
        act.ExecuteAction (act.PrevProc, act.mode_silent);
      END;

    | key.CtrlDown:
      IF msg.ID = eve.QueryKbHit THEN
        act.ConfirmQueryByCond(act.QueryAction(act.NextProc));
      ELSE
        act.ExecuteAction (act.NextProc, act.mode_silent);
      END;

    | key.Del:
      IF msg.ID = eve.QueryKbHit THEN
        act.ConfirmQueryByCond(act.QueryAction(act.Delete));
      ELSE
        act.ExecuteAction (act.Delete, act.mode_silent);
      END;

    | key.CtrlF4:

    ELSE
      std.DefaultProc(hwnd,msg);
    END;
  ELSE
    std.DefaultProc(hwnd,msg);
  END;
END ScrollAreaProc;


PROCEDURE MainWindowStateAction (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT (action IN act.ACTION_SET {act.SourceMode, act.AssemblyMode, act.MixMode});
  WITH std.Wnds[std.MainWindow] DO
    IF hwnd = win.Invalid_H THEN
      RETURN act.DummyAction (action, mode);
    ELSIF mode = act.mode_check THEN
      RETURN std.QueryAction (hwnd, action);
    ELSE
      RETURN std.DoAction (hwnd, action);
    END;
  END;
END MainWindowStateAction;



VAR
  DlgExitCode: exp.ExprRes; -- WHOLEval, на самом деле value: act.ACTION;


PROCEDURE Halt (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Halt);
  IF mode # act.mode_check THEN
    DlgExitCode.value := ORD(action);
    crt.Exi;
   <* IF DEST_XDS THEN *>
   <* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME
    dl.SaveToLog (action);
   <* END *>
   <* END *>
    HALT (0);
  END;
  RETURN TRUE;
END Halt;


PROCEDURE Exit (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT((action = act.Quit) OR (action = act.ReturnToBatch));
  IF mode # act.mode_check THEN
    DlgExitCode.value := ORD(action);
    IF kexe.Loaded AND opt.DialogMode THEN
      exe.UnloadProgram;
    END;
    eve.AddToTail(win.Invalid_H, eve.Quit, 0);
   <* IF DEST_XDS THEN *>
   <* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME
    dl.SaveToLog (action);
   <* END *>
   <* END *>
  END;
  RETURN TRUE;
END Exit;


PROCEDURE Quit (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Quit);
  RETURN Exit (action, mode);
END Quit;


PROCEDURE ReturnToBatch (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.ReturnToBatch);
  RETURN Exit (action, mode);
END ReturnToBatch;


PROCEDURE DeleteBlanks (VAR s: ARRAY OF CHAR);
VAR
  e: CARDINAL;
BEGIN
  WHILE s[0] = ' ' DO
    st.Delete (s, 0, 1);
  END;
  e := LENGTH(s);
  WHILE (e > 0) AND (s[e-1] = ' ') DO
    st.Delete (s, e-1, 1);
    e := LENGTH(s);
  END;
END DeleteBlanks;


VAR
  ProgramName: xs.String;
  ProgramArgs: xs.String;

PROCEDURE LoadSource(hwnd: win.HWND): BOOLEAN;
VAR
  IP, tmp : kt.ADDRESS;
  access: kt.ATTRIBS;
  name  : xs.String;
BEGIN
  IF kexe.Loaded THEN
    dex.RemoveRTShandler;
    dmm.ClearRegisters;
    brk.ClearBreaks;
    exe.UnloadProgram;
    tls.ClearComponents(TRUE);
    mod.ClearModules;
    dex.ResetCallStack;
    dv.N_Call := 0;
  END;

  DeleteBlanks (ProgramName);

  COPY(ProgramName, opt.prog_name);
  COPY(ProgramArgs, opt.prog_args);

  IF opt.prog_name = '' THEN
    std.SetErrorMsgNo(mes.Expected_program_name);
    RETURN FALSE;
  END;

  fil.ExtractFileName (opt.prog_name,  name);
  fil.RemoveExtension(name);
  cfg.LoadConfig (name);
  IF dex.Load (opt.prog_name) AND kexe.Loaded THEN
    eve.AddToTail(hwnd, eve.Hide, 0);
    eve.Flush;
    IP := mem.GetIP ();
    IF mem.GetSegmentInfo (IP, tmp, tmp, access) THEN
      dex.PointExecLine (IP);
      eve.AddToTail(std.Wnds[std.MainWindow].hwnd, eve.Rise, 0);
    END;
    IF win.Visible(std.Wnds[std.RegWindow].hwnd) THEN
      dmm.IniRegisters;
    END;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END LoadSource;


PROCEDURE DelWatch (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  p: std.PLIST;
  i: CARDINAL;
BEGIN
  ASSERT(action = act.DelWatch);
  WITH std.Wnds[std.WatchWindow] DO
    IF men.SkipMenus() # hwnd THEN RETURN FALSE; END;
    p := win.GetAMPtr(hwnd);
    WITH p^ DO
      IF dw.Watches.count = 0 THEN RETURN FALSE; END;
      IF mode # act.mode_check THEN
        FOR i := curr TO dw.Watches.count-1 DO
          dw.Watches.Watch^[i] := dw.Watches.Watch^[i+1];
        END;
        DEC(dw.Watches.count);
        eve.AddToTail(hwnd, eve.Redraw, 0);
        IF (curr=N-1) AND (curr # 0) THEN DEC(curr); END;
       END;
    END;
  END;
  RETURN TRUE;
END DelWatch;


PROCEDURE DelAllWatches (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.DelAllWatches);
  IF dw.Watches.count = 0 THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    dw.Watches.count := 0;
    IF std.Wnds[std.WatchWindow].hwnd # win.Invalid_H THEN
      eve.AddToTail(std.Wnds[std.WatchWindow].hwnd, eve.Redraw, 0);
    END;
  END;
  RETURN TRUE;
END DelAllWatches;


CONST
 ContextWatch_PMemory = 3;
 ContextWatch_Edit = 8;

PROCEDURE InitWatchList (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN; FORWARD;

PROCEDURE NormalizeWatches; FORWARD;


PROCEDURE AddWatch(hwnd: win.HWND): BOOLEAN;
VAR
  previous: win.HWND;
BEGIN
  IF dw.NewWatch() THEN
    dw.RecalcWatches;
    IF hwnd # win.Invalid_H THEN
      eve.AddToTail(hwnd, eve.Hide, 0);
      eve.Flush;
    END;
    IF (std.Wnds[std.WatchWindow].hwnd = win.Invalid_H) THEN
      IF NOT InitWatchList (act.Watches, act.mode_loud) THEN
        RETURN FALSE;
      END;
    ELSE
      NormalizeWatches;
      IF win.ActiveWindow = std.Wnds[std.WatchWindow].hwnd THEN
        eve.AddToTail(std.Wnds[std.WatchWindow].hwnd, eve.Redraw, 0);
      ELSE
        previous := win.ActiveWindow;
        eve.AddToTail(std.Wnds[std.WatchWindow].hwnd, eve.Rise, 0);
        IF previous # win.Invalid_H THEN
          eve.AddToTail(previous, eve.Rise, 0);
        END;
      END;
    END;
    IF dw.Watches.Watch^[dw.WatchPos].dfn AND (dw.Watches.Watch^[dw.WatchPos].error # 0) THEN
      std.SetErrorNo(dw.Watches.Watch^[dw.WatchPos].error);
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END AddWatch;


PROCEDURE WatchesHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p    : std.PLIST;

  PROCEDURE write_line(num: CARDINAL; VAR lite: BOOLEAN);
  VAR
    buf, value  : xs.String;
    l: CARDINAL;
    type_tag: dt.TYPE_TAG;
  BEGIN
    WITH p^ DO
      l := num-frame + 1;
      WITH dw.Watches.Watch^[num] DO
        lite := FALSE;
        IF error = 0 THEN
          IF dfn THEN
            IF res.sort = exp.Variable THEN
              ASSERT(tls.TypeTag(res.var_type, type_tag));
              IF opt.DisplayDerefencePointer AND (type_tag = dt.Pointer) THEN
                res.type := dt.st_dereference;
              END;
            END;
            exp.Res2Str(res, value);
            lite := NOT exp.CompareRes(res, res_);
          ELSE
            COPY('???', value);
          END;
        ELSE
          pro.GetMsg(error, value);
        END;
        fmt.print(buf, '%-10s %s', expr^, value);
        crt.SetPos(2, l);
        IF lite THEN
          crt.WrStrFromPos(hwnd, buf, crt.Attr(crt.LightRed, crt.Bg(Colors^[crt.List_Line])), pos);
        ELSE
          crt.WrStrFromPos(hwnd, buf, Colors^[crt.List_Line], pos);
        END;
      END;
    END;
  END write_line;

VAR
  size   : crt.SZ; 
  i, len ,item: CARDINAL;
  last ,x,y  : CARDINAL;
  var    : exp.ExprRes;
  var_tag: dt.TYPE_TAG;
  subtype : dt.PTYPE;
  type_tag: dt.TYPE_TAG;
  main: BOOLEAN;
  lite: BOOLEAN;

 <* IF DEST_XDS THEN *>
  buf: xs.String;
 <* END *>



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
      IF kexe.Loaded THEN
        N := dw.Watches.count;
      ELSE
        N := 0;
      END;
      IF N > 0 THEN
        CASE msg.par OF
        | 3:
          IF hwnd = win.ActiveWindow THEN
            write_line (curr, lite);
            IF lite THEN
              crt.Lite (hwnd, curr - frame + 1 , 1, crt.Attr(crt.LightRed, crt.Bg(Colors^[crt.List_CurrentLine])));
            ELSE
              crt.Lite (hwnd, curr - frame + 1 , 1, Colors^[crt.List_CurrentLine]);
            END;
          END;
        | 4:
          IF hwnd = win.ActiveWindow THEN
            crt.Lite (hwnd, curr-frame+1 , 1, Colors^[crt.List_Background]);
            write_line (curr, lite);
          END;
        ELSE
          last := std.Min(frame+len-1, N-1);
          FOR i:= frame TO last DO
            write_line (i, lite)
          END;
          IF hwnd = win.ActiveWindow THEN
            crt.Lite (hwnd, curr - frame + 1 , 1, Colors^[crt.List_CurrentLine]);
          END;
        END;
      END;
    END;
    std.ListBox(hwnd, msg);

 | eve.QueryItem:
    IF p^.N > 0 THEN
      std.GetItem (msg.par, main, item);
      IF main THEN
        CASE item OF

        | ContextWatch_PMemory: -- Pointed memory
	  var := dw.Watches.Watch^[p^.curr].res;
	  IF var.sort IN exp.SORTS {exp.WHOLEval, exp.CARDval, exp.Address } THEN
	    act.ConfirmQueryByCond (dw.Watches.Watch^[p^.curr].dfn AND (dw.Watches.Watch^[p^.curr].error=0));
	  ELSIF (var.sort = exp.INTval) AND (INTEGER(var.value) > 0) THEN
	    act.ConfirmQueryByCond (dw.Watches.Watch^[p^.curr].dfn AND (dw.Watches.Watch^[p^.curr].error=0));
	  ELSIF var.sort = exp.Variable THEN
            ASSERT (tls.TypeTag (var.var_type, type_tag));
            act.ConfirmQueryByCond (dw.Watches.Watch^[p^.curr].dfn AND (dw.Watches.Watch^[p^.curr].error=0) AND (type_tag = dt.Pointer));
	  END;

        | ContextWatch_Edit:
           act.ConfirmQuery();
        ELSE
        END;
      END;
    END;

 | eve.ContextItem:
    IF p^.N > 0 THEN
      std.GetItem (msg.par, main, item);
      IF main THEN
        CASE item OF
        | ContextWatch_PMemory: -- Pointed memory
	  var := dw.Watches.Watch^[p^.curr].res;
	  IF var.sort IN exp.SORTS {exp.WHOLEval, exp.CARDval, exp.INTval, exp.Address} THEN
	     CASE var.sort OF
	     | exp.WHOLEval, exp.CARDval:
	       fmt.print (dv.DumpAddrStr, exp.Fmt_ADDRval, var.value);
	     | exp.INTval:
	       IF INTEGER(var.value) > 0 THEN
                 fmt.print (dv.DumpAddrStr, exp.Fmt_ADDRval, INTEGER(var.value));
	       ELSE
	         RETURN;
	       END;	 	
	     | exp.Address:
               fmt.print (dv.DumpAddrStr, exp.Fmt_ADDRval, var.address);
	     END;
             act.ExecuteAction (act.Dump, act.mode_silent);
	  ELSIF var.sort = exp.Variable THEN
	    WITH dw.Watches.Watch^[p^.curr] DO
	      IF (error = 0) AND dfn THEN
                COPY (dw.Watches.Watch^[p^.curr].expr^, dv.DumpAddrStr);
                act.ExecuteAction (act.Dump, act.mode_silent);
              END;
	    END;     		
	  END;

        | ContextWatch_Edit:
            COPY(dw.Watches.Watch^[p^.curr].expr^, dv.expr);
            dw.WatchPos := p^.curr;
            std.OpenUniversalDialog(mes.dlg_EnterWatch, AddWatch, dv.expr);
        ELSE
        END;
      END;
    END;

  | eve.QueryAction:
    CASE std.GetAction (msg.par) OF
     | act.ChangeTypes:
      IF p^.N = 0 THEN RETURN; END;
      var := dw.Watches.Watch^[p^.curr].res;
      CASE var.sort OF
      | exp.CARDval, exp.INTval, exp.WHOLEval:
        act.ConfirmQueryByCond (dw.Watches.Watch^[p^.curr].dfn AND (dw.Watches.Watch^[p^.curr].error=0));
      | exp.Variable:
        ASSERT(tls.TypeTag(var.var_type, type_tag));
        IF type_tag IN dt.TYPE_TAG_SET{dt.Pointer, dt.Int, dt.Card} THEN
          act.ConfirmQueryByCond (dw.Watches.Watch^[p^.curr].dfn AND (dw.Watches.Watch^[p^.curr].error=0));
        END;
      ELSE
      END;

     |<* IF DEST_XDS THEN *>
       act.Access,
      <* END *>
       act.Dump:
       IF (p^.N # 0) AND (dw.Watches.Watch^[p^.curr].dfn) AND (dw.Watches.Watch^[p^.curr].error = 0) THEN
         exp.GetLeftValue(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, dw.Watches.Watch^[p^.curr].expr^, var);
         act.ConfirmQueryByCond (dw.Watches.Watch^[p^.curr].dfn AND (dw.Watches.Watch^[p^.curr].error=0) AND (var.sort = exp.Variable));
       END;

    ELSE
      std.ListBox (hwnd, msg);
    END;

  | eve.DoAction:
    WITH p^ DO
      CASE std.GetAction (msg.par) OF
      | act.ChangeTypes:
        dw.Watches.Watch^[curr].res.type := act.ChangeTypesID;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      | act.Examine:
        eve.AddToTail (hwnd, eve.KbHit, key.Enter);
        RETURN;
      | act.Dump:
        IF (N # 0) AND (dw.Watches.Watch^[p^.curr].dfn) AND (dw.Watches.Watch^[p^.curr].error = 0) THEN
          exp.GetLeftValue(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, dw.Watches.Watch^[p^.curr].expr^, var);
	        fmt.print (dv.DumpAddrStr, exp.Fmt_ADDRval, var.location);
          act.ExecuteAction (act.Dump, act.mode_silent);
          RETURN;
        END;

     <* IF DEST_XDS THEN *>
      | act.Access:
        IF (N # 0) AND (dw.Watches.Watch^[p^.curr].dfn) AND (dw.Watches.Watch^[p^.curr].error = 0) THEN
          exp.GetLeftValue(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, dw.Watches.Watch^[p^.curr].expr^ , var);
          len := tls.TypeSize(var.var_type);
          COPY(dw.Watches.Watch^[p^.curr].expr^, buf);
          xs.Append(')', buf);
          xs.Insert('ADR(', 0, buf);
          dbrk.SetAccessAttr(buf, len);
          act.ExecuteAction (act.Access, act.mode_silent);
          RETURN;
        END;	
     <* END *>

      ELSE
      END;
      std.ListBox (hwnd, msg);
    END;

  | eve.R_Mouse:
    ASSERT(win.GetMouse(msg, x, y));
    size := win.GetWindowSize(hwnd);
    IF NOT std.CheckFrame(size, x, y) THEN
      eve.AddToTail(hwnd, eve.Mouse_Pressed, msg.par);
      eve.Flush;
      p^.actions[4].tenable := act.TYPES_SET{};
      IF (p^.N # 0) AND (dw.Watches.Watch^[p^.curr].dfn) AND (dw.Watches.Watch^[p^.curr].error = 0) THEN
        exp.GetLeftValue(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, dw.Watches.Watch^[p^.curr].expr^ , var);
        IF NOT exp.Var2Value(var, var) THEN
          RETURN;
        END;
        CASE var.sort OF
        | exp.CARDval, exp.INTval, exp.WHOLEval:
          p^.actions[4].tenable := act.TYPES_SET{dt.st_original, dt.st_unsigned, dt.st_signed, dt.st_hex, dt.st_oct, dt.st_bin };
        | exp.Variable:
          ASSERT(tls.TypeTag(var.var_type, type_tag));
          IF type_tag = dt.Pointer THEN
            p^.actions[4].tenable := act.TYPES_SET{dt.st_original,dt.st_dereference};
          ELSIF type_tag = dt.Int THEN
            p^.actions[4].tenable := act.TYPES_SET{dt.st_original, dt.st_unsigned, dt.st_signed, dt.st_hex, dt.st_bin };
          END;
        ELSE
        END;
      END;
      puw.PopupWindow(x, y, hwnd, p^.actions);
    END;


  | eve.KbHit:
    WITH p^ DO
      CASE msg.par OF
      | key.Del:
        IF N = 0 THEN crt.Beep; RETURN; END;
        act.ExecuteAction (act.DelWatch, act.mode_silent);

      | key.Enter, key.CtrlEnter:
        IF N = 0 THEN RETURN; END;
        exp.GetLeftValue(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, dw.Watches.Watch^[p^.curr].expr^ , var);
        IF (exp.error=0) AND exp.dfn THEN
          CASE var.sort OF
          | exp.Variable:
            ASSERT(tls.TypeTag(var.var_type, var_tag));
            COPY(dw.Watches.Watch^[p^.curr].expr^, dv.VarName);
            IF var_tag = dt.Pointer THEN
              tls.SubType(var.var_type, subtype);
              ASSERT(tls.TypeTag(subtype, var_tag));
              IF var_tag = dt.Procedure THEN
                var.var_type := subtype;
              ELSIF (msg.par = key.Enter) THEN
                IF NOT exp.Dereference(var, var) THEN
                  std.ErrorNo (mes.NilPointerDereference);
                  RETURN;
                END;
                xs.Append('^', dv.VarName);
              END;
            END;
            IF NOT ChangeVariable (win.Invalid_H, var) THEN
              eve.AddToTail (std.ErrorMsg, eve.Rise, 0);
            END;
            RETURN;
          | exp.Register:
            COPY (dw.Watches.Watch^[p^.curr].expr^, dv.VarName);
            IF NOT ChangeVariable (win.Invalid_H, var) THEN
               eve.AddToTail (std.ErrorMsg, eve.Rise, 0);
            END;
            RETURN;
          ELSE
          END;
        END;
        crt.Beep;
      ELSE
        std.ListBox(hwnd, msg);
      END;
    END;
  ELSE
    std.ListBox(hwnd, msg);
  END;
END WatchesHandler;




PROCEDURE InitWatchWindow (show: BOOLEAN); FORWARD;

PROCEDURE WatchExpr (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
	
  ASSERT(action = act.AddWatch);
  IF (std.Wnds[std.WatchWindow].hwnd = win.Invalid_H) THEN
      InitWatchWindow (FALSE);
  END;

  IF mode = act.mode_silent THEN
    dw.WatchPos := MAX(CARDINAL);
    IF AddWatch (win.Invalid_H) THEN
      IF NOT dw.Watches.Watch^[dw.WatchPos].dfn THEN
        std.NotifyNo(mes.WrongExprInCurrentContext,dw.Watches.Watch^[dw.WatchPos].expr^);
      END;
      eve.AddToTail (win.ActiveWindow, eve.Redraw, 0);
    ELSE
      eve.AddToTail (std.ErrorMsg, eve.Rise, 0);
      eve.Flush;
      std.OpenUniversalDialog(mes.dlg_EnterWatch, AddWatch, dv.expr);
      eve.AddToTail (std.Wnds[std.WatchWindow].hwnd, eve.Redraw, 0);
    END;
  ELSIF mode # act.mode_check THEN
    dw.WatchPos := MAX(CARDINAL);
    std.OpenUniversalDialog(mes.dlg_EnterWatch, AddWatch, dv.expr);
  END;
  RETURN TRUE;
END WatchExpr;


PROCEDURE Refresh(action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Refresh);
  IF mode # act.mode_check THEN
    crt.Refresh;
    crt.Update;
  END;
  RETURN TRUE;
END Refresh;


VAR
  LoadProgramDialog: win.HWND;


PROCEDURE UpdateProgramName;
VAR
  msg: eve.MSG;
  p  : std.PDIALOG;
BEGIN
  msg.hwnd := LoadProgramDialog;
  msg.ID := eve.Rise;
  msg.par := 0;
  p := std.PDIALOG(win.GetAMPtr(LoadProgramDialog));
  WITH p^ DO
    curr := 2;
    WITH Lines^[curr] DO
      ASSERT(sort = std.edit_str);
      COPY(ProgramName, e_str^);
      std.LineEditor(LoadProgramDialog, msg, 0, 0, e_str, len, TRUE);
    END;
  END;
END UpdateProgramName;


PROCEDURE BrowseProgramName;
BEGIN
  IF ProgramName = '' THEN fmt.print(ProgramName, '*.%s', kt.prg_file_ext); END;
  brw.Browse(act.ActionName[act.Load], ProgramName, UpdateProgramName);
END BrowseProgramName;


PROCEDURE Open (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;

TYPE
  LINE = std.LINE;

VAR
  Lines: std.PLINES;
  p    : std.PDIALOG;
  size : crt.SZ;
BEGIN
  ASSERT(action = act.Load);
  IF mode # act.mode_check THEN
    IF LoadProgramDialog = win.Invalid_H THEN
     <* IF DEST_K26 THEN *>
      NEW(Lines,6);
      Lines^[ 0] := LINE{ 2, 2, std.msg     , '┌── Введите имя программы ─┐'            , std.d_enabled};
      Lines^[ 1] := LINE{ 2, 3, std.msg     , '│'                                       , std.d_enabled};
      Lines^[ 2] := LINE{ 4, 3, std.edit_str, sys.ADR(ProgramName), 24                , std.d_enabled};
      Lines^[ 3] := LINE{29, 3, std.msg     ,                               '│'         , std.d_enabled};
      Lines^[ 4] := LINE{ 2, 4, std.msg     , '└──────────────────────────┘'            , std.d_enabled};
      Lines^[ 5] := LINE{31, 3, std.button  , 'Выбор', BrowseProgramName               , 1, key.AltD, std.d_enabled};
      size.x1 := 18; size.y1 := 8;
      size.x2 := 60; size.y2 := 16;
     <* ELSIF DEST_XDS THEN *>
      NEW(Lines,11);
      Lines^[ 0] := LINE{ 2, 2, std.msg     , '┌── Program name ──────────┐'            , std.d_enabled};
      Lines^[ 1] := LINE{ 2, 3, std.msg     , '│'                                       , std.d_enabled};
      Lines^[ 2] := LINE{ 4, 3, std.edit_str, sys.ADR(ProgramName), 24                , std.d_enabled};
      Lines^[ 3] := LINE{29, 3, std.msg     ,                               '│'         , std.d_enabled};
      Lines^[ 4] := LINE{ 2, 4, std.msg     , '└──────────────────────────┘'            , std.d_enabled};
      Lines^[ 5] := LINE{31, 3, std.button  , 'Browse', BrowseProgramName               , 1, key.AltB, std.d_enabled};
      Lines^[ 6] := LINE{ 2, 5, std.msg     , '┌── Arguments ────────────────────────┐' , std.d_enabled};
      Lines^[ 7] := LINE{ 2, 6, std.msg     , '│'                                       , std.d_enabled};
      Lines^[ 8] := LINE{ 4, 6, std.edit_str, sys.ADR(ProgramArgs), 35                , std.d_enabled};
      Lines^[ 9] := LINE{40, 6, std.msg     ,                                       '│' , std.d_enabled};
      Lines^[10] := LINE{ 2, 7, std.msg     , '└─────────────────────────────────────┘' , std.d_enabled};
      size.x1 := 18; size.y1 := 7;
      size.x2 := 60; size.y2 := 18;
     <* END *>

      LoadProgramDialog := win.RegisterWindow(std.DialogProc,SIZE(std.DIALOG));
      ASSERT(LoadProgramDialog # win.Invalid_H);
      win.SetModal(LoadProgramDialog);
      win.SetMovable(LoadProgramDialog);
      win.SetHeaderByStr(LoadProgramDialog, act.ActionName[act.Load]);
      win.SetWindowSize(LoadProgramDialog,size);

      p := std.PDIALOG(win.GetAMPtr(LoadProgramDialog));
      p^.curr     := 2;
      p^.on_error := win.Invalid_H;
      p^.Lines    := Lines;
      p^.action   := LoadSource;
    END;
    COPY(opt.prog_name, ProgramName);
    COPY(opt.prog_args, ProgramArgs);
    eve.AddToTail(LoadProgramDialog, eve.Rise, 0);
  END;
  RETURN TRUE;
END Open;


VAR
  Res2Change: exp.ExprRes;

PROCEDURE ChangeVarValue(hwnd: win.HWND): BOOLEAN;
VAR
  new_value: exp.ExprRes;
BEGIN
  exp.CalcExpr(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, std.NewValue, new_value);
  IF (exp.error # 0) THEN
    std.SetErrorNo(exp.error);
--    exp.Res2Str(res1, NewValue);
    RETURN FALSE;
  END;
  IF NOT exp.dfn THEN
    std.SetErrorNo(mes.Undefined_Expression);
--    exp.Res2Str(res1, NewValue);
    RETURN FALSE;
  END;
  IF NOT exp.Assign(Res2Change, new_value) THEN
    std.SetErrorNo(exp.error);
--    exp.Res2Str(res1, NewValue);
    RETURN FALSE;
  END;
  dw.RecalcWatches;
  dmm.RefreshRegs;
  eve.AddToTail(hwnd, eve.Hide, 0);
  eve.Flush;
  eve.AddToTail(eve.AllWindows, eve.Redraw, 1);
  RETURN TRUE;
END ChangeVarValue;



CONST
  -- Максимальная вложенность для структур
  MaxDepth = 64;


TYPE
  -- Текущее состояние окна структурных переменных
  STRUCT_STATE = RECORD
                   curr, frame: CARDINAL;
                   struct     : exp.ExprRes;
                   design     : dt.TYPE_TAG;
                   N          : CARDINAL;
                   StructName : xs.String;
                   arr_st_id  : dt.SYM_TYPE_ID;
                 END;

TYPE
  STRUCT_STATES = ARRAY [0..MaxDepth] OF STRUCT_STATE;

  STRUCT = RECORD
             ListData     : std.LIST;
             Depth        : CARDINAL;
             AddrInHeader : BOOLEAN;
             Structs      : STRUCT_STATES;
           END;

  PSTRUCT = POINTER TO STRUCT;


PROCEDURE StructListLocator (hwnd: crt.HWND; current_line: CARDINAL; VAR str: ARRAY OF CHAR);
VAR
  p    : PSTRUCT;
  name : xs.txt_ptr;
  buf  : xs.String;
  type : dt.PTYPE;
  field: dt.TYPE_RECORD_FIELD;
BEGIN
  p := PSTRUCT(win.GetAMPtr(hwnd));
  WITH p^ DO
    WITH Structs[Depth] DO
      IF (current_line < N) AND tls.IsTypeValid (struct.var_type) THEN
        CASE design OF
        | dt.Array, dt.Array_of, dt.OpenArray:
          tls.ArrayIndexType (struct.var_type, type);
          exp.i2Str (type, current_line, buf);
          fmt.print (str, '[%s]', buf);

        | dt.Set :
          tls.SubType (struct.var_type, type);
          exp.i2Str (type, current_line, str);

        | dt.Record, dt.Class:
          tls.Field (struct.var_type, current_line+1, field);
          name := tls.GetName (field.FieldName);
          COPY(name^, str);
        END;
      ELSE
        COPY('', str);
      END;
    END;
  END;
END StructListLocator;



VAR
  -- Окно структурных переменных
  StructWindow: win.HWND;

-- Обработчик окна структурных переменных
PROCEDURE StructList(hwnd: win.HWND; msg: eve.MSG); FORWARD;


VAR
  x_direct: BOOLEAN;
  y_direct: BOOLEAN;

PROCEDURE FixUpSize (VAR size: crt.SZ);
BEGIN
  WITH size DO
    IF x_direct THEN
      IF x1 > 0 THEN
        DEC(x1);
        DEC(x2);
      ELSIF x2 < crt.Xmax-1 THEN
        INC(x1);
        INC(x2);
        x_direct := FALSE;
      END;
    ELSE
      IF x2 < crt.Xmax-1 THEN
        INC(x1);
        INC(x2);
      ELSIF x1 > 0 THEN
        DEC(x1);
        DEC(x2);
        x_direct := TRUE;
      END;
    END;
    IF y_direct THEN
      IF y1 > 1 THEN
        DEC(y1);
        DEC(y2);
      ELSIF y2 < crt.Ymax-1 THEN
        INC(y1);
        INC(y2);
        y_direct := FALSE;
      END;
    ELSE
      IF y2 < crt.Ymax-1 THEN
        INC(y1);
        INC(y2);
      ELSIF y1 > 1 THEN
        DEC(y1);
        DEC(y2);
        y_direct := TRUE;
      END;
    END;
  END;
END FixUpSize;


-- Переключает опцию, открывать ли новое окно при инициализации
-- окна структурных переменных из списка переменных
PROCEDURE tglUseSingleStructWindow (check: BOOLEAN): BOOLEAN;
BEGIN
  RETURN std.tglOption (opt.UseSingleStructureWindow, check);
END tglUseSingleStructWindow;


CONST
 <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
  ContextItem_OpenNewWindow = 7;
 <* ELSE *>
  ContextItem_OpenNewWindow = 7;
 <* IF DEST_XDS THEN *>
  ContextItem_AddrInHeader  = 9;
  ContextItem_AutoDetectActualType = 10;
  ContextItem_GetActualType = 12;
 <* END *>
 <* END *>


VAR
  OpenNewWindowMode : BOOLEAN; -- Режим открытия нового окна
  OpenNewWindowCheck: BOOLEAN; -- Режим проверки возможности открытия


PROCEDURE OpenStructWindow (size: crt.SZ; single_window: BOOLEAN);
VAR
  p   : PSTRUCT;
  init: BOOLEAN;
BEGIN
  IF StructWindow = win.Invalid_H THEN
    StructWindow := win.RegisterWindow (StructList, SIZE(STRUCT));
    init := TRUE;
  ELSIF NOT single_window THEN
    StructWindow := win.FindClosed (win.Invalid_H, StructList);
    init := StructWindow = win.Invalid_H;
    IF init THEN
      StructWindow := win.RegisterWindow (StructList, SIZE(STRUCT));
    END;
  ELSE
    init := FALSE;
  END;
  ASSERT(StructWindow # win.Invalid_H);
  IF init THEN
    FixUpSize (size);
    win.SetWindowSize (StructWindow, size);
    win.SetHeader (StructWindow, mes.StructVar);
    win.SetSwitchable (StructWindow);
    win.SetMovable (StructWindow);
    win.SetResizable (StructWindow, TRUE, TRUE);
  END;
  p := PSTRUCT(win.GetAMPtr(StructWindow));
  WITH p^ DO
    Depth        := 0;
    AddrInHeader := FALSE;
    WITH Structs[Depth] DO;
      curr := 0;
      frame := 0;
      N := 0;
      design := dt.T_Void;
      struct.sort := exp.Empty;
      StructName := '';
      arr_st_id := dt.st_original;
    END;
    WITH ListData DO
      N := 0;
      curr := 0;
      frame := 0;
      pos := 0;
      Frame := crt.Double;
      IF init THEN
        Colors := sys.ADR(crt.StructAttr);
        locator := StructListLocator;
        actions[0] := act.CONTEXT{ act.do_action, act.Examine  };
       <* IF DEST_K26 THEN *>
        actions[01] := act.CONTEXT{ act.separate };
       <* ELSIF DEST_XDS THEN *>
        actions[01] := act.CONTEXT{ act.do_action, act.Access   };
       <* END *>
        actions[02] := act.CONTEXT{ act.do_action, act.AddWatch };
        actions[03] := act.CONTEXT{ act.do_action, act.Dump };
        actions[04] := act.CONTEXT{ act.context_item, "Pointed memory" };
        actions[ContextItem_Type] := act.CONTEXT{ act.types, act.TYPES_SET{} };
        actions[06] := act.CONTEXT{ act.separate };
        actions[ContextItem_OpenNewWindow] := act.CONTEXT{ act.context_item, 'Open in new window' };
        actions[08] := act.CONTEXT{ act.toggler, 'Use single window', tglUseSingleStructWindow };
        actions[09] := act.EMPTY_CONTEXT;
       <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
        actions[09] := act.CONTEXT{ act.do_action, act.prf_var0 };      (* SCHERN *)
        actions[10] := act.CONTEXT{ act.do_action, act.prf_var9 };      (* SCHERN *)
        actions[11] := act.CONTEXT{ act.do_action, act.prf_var6 };      (* SCHERN *)
        actions[12]:= act.EMPTY_CONTEXT;
       <* ELSE *>
       <* IF DEST_XDS THEN *>
        actions[ContextItem_AddrInHeader] := act.CONTEXT{ act.context_item, 'Show address' };
        actions[10] := act.EMPTY_CONTEXT;
       <* END *>
       <* END *>
        ext := NIL;
      END;
    END;
  END;
END OpenStructWindow;



<* IF DEST_XDS THEN *>
-- Переключает опцию авто-определения настоящего типа обьекта
PROCEDURE tlgAutoDetectActualType (check: BOOLEAN): BOOLEAN;
BEGIN
  RETURN std.tglOption (opt.AutoDetectActualType, check);
END tlgAutoDetectActualType;
<* END *>


PROCEDURE StructList (hwnd: win.HWND; msg: eve.MSG);
VAR
  p: PSTRUCT;

  PROCEDURE UpdateList;
  BEGIN
    WITH p^ DO
      WITH Structs[Depth] DO
        ListData.N     := N;
        ListData.frame := frame;
        ListData.curr  := curr;
      END;
    END
  END UpdateList;


  PROCEDURE UpdateStructs;
  BEGIN
    WITH p^ DO
      WITH Structs[Depth] DO
        frame := ListData.frame;
        curr  := ListData.curr;
      END;
    END;
  END UpdateStructs;


VAR
  AccessLen: CARDINAL;

  PROCEDURE GetCurrName (VAR curr_name: ARRAY OF CHAR; VAR pointer: BOOLEAN);
  VAR
    ptype: dt.PTYPE;
    str  : xs.String;
    name : xs.txt_ptr;
    ttag : dt.TYPE_TAG;
    field: dt.TYPE_RECORD_FIELD;
  BEGIN
    AccessLen := 1;
    pointer := FALSE;
    WITH p^ DO
      WITH Structs[Depth] DO
        IF tls.IsTypeValid(struct.var_type) THEN
          CASE design OF
          | dt.Array, dt.Array_of, dt.OpenArray:
            tls.ArrayIndexType(struct.var_type, ptype);
            exp.i2Str(ptype, curr, str);
            fmt.print(curr_name, '%s[%s]', StructName, str);
            tls.SubType(struct.var_type, ptype);
            ASSERT(tls.TypeTag(ptype, ttag));
            pointer := ttag = dt.Pointer;

          | dt.Set :
            COPY(StructName, curr_name);

          | dt.Record, dt.Class:
            IF Structs[Depth].N # 0 THEN
              tls.Field (struct.var_type, curr+1, field);
              name := tls.GetName (field.FieldName);
              fmt.print(curr_name, '%s.%s', StructName, name^);
              ptype := field.FieldType;
              ASSERT(tls.TypeTag(ptype, ttag));
              pointer := ttag = dt.Pointer;
              AccessLen := tls.TypeSize (ptype);
              INC(AccessLen, ORD(AccessLen = 0));
            ELSE
              COPY('', curr_name);
            END;
          END;
        ELSE
          COPY('???', curr_name);
        END;
      END;
    END;
  END GetCurrName;


VAR
  size: crt.SZ;
  attr: crt.ATTR;
  tag : dt.TYPE_TAG;
  bit : BOOLEAN;
  name: xs.txt_ptr;
  len : CARDINAL;
  i, last   : CARDINAL;
  min, max  : CARDINAL;
  arr_size  : CARDINAL;
  x, y      : CARDINAL;
  Struct    : STRUCT_STATE;
  pointed   : dt.PTYPE;
  index_type: dt.PTYPE;
  res, res2: exp.ExprRes;
  str1, str2, buf: xs.String;

<* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
     ai : ARRAY[0..64] OF CARDINAL;           (* SCHERN *)
     depth : CARDINAL;                              (*        *)
(*                                                    SCHERN     *)
PROCEDURE Add_Struct_var(p: PSTRUCT);
VAR ar_ind : prv.AR_EL;
   i , depth : CARDINAL;
BEGIN
      depth:= p^.Depth;
     FOR i := 0 TO depth DO
       ar_ind[i].ind:= p^.Structs[i].curr+1;
       ar_ind[i].tag:= p^.Structs[i].design;
       ar_ind[i].type := p^.Structs[i].struct.var_type;
     END;
  WITH p^ DO
    WITH Structs[Depth] DO
        CASE design OF
        | dt.Array, dt.Array_of, dt.OpenArray:
            tls.ArrayIndexType (struct.var_type, index_type);
            item.sort := exp.Variable;
            item.type := struct.type;
            tls.SubType (struct.var_type, item.var_type);
            exp.i2Str (index_type, curr, buf);
--          fmt.print (str1, '[%s]', buf);
            IF design = dt.OpenArray THEN
              IF exp.OpenArray_Size (struct, arr_size) THEN
                item.location := struct.location + curr*arr_size;
              ELSE
                item.location := 0;
              END;
            ELSE
              item.location := struct.location + curr*tls.TypeSize(item.var_type);
            END;
--          exp.Res2Str (item, str2);
(*        | dt.Set :
          tls.SubType (struct.var_type, type);
          exp.i2Str (type, current_line, str);
*)
        | dt.Record, dt.Class:
            item.sort := exp.Variable;
            item.type ;= struct.type;
            tls.Field(struct.var_type, curr+1, name, item.var_type, offs);
            item.location := struct.location + offs;
--           IF prv.AddFildRecVar(mod.Curr^.ComNo,mod.Curr^.ModNo,prf_Var_Ind,depth,struct.var_type,ar_ind,item) THEN
--              eve.AddToTail(hwnd,eve.Redraw,0);
--           END;
        END;
    END;
  END;
       IF prv.AddIndVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN,prf_Var_Ind,depth,ar_ind,item) THEN
           eve.AddToTail(hwnd,eve.Redraw,0);
       END;
END Add_Struct_var;

PROCEDURE MyIndVar(): BOOLEAN;
BEGIN
 IF mod.Curr^.Pos.ComN#dt.Invalid_Component THEN
   RETURN prv.MyIndVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN,prf_Var_Ind,depth,ai);
 ELSE RETURN FALSE;
 END;
END MyIndVar;
(*                                                        end SCHERN *)

<* END *>

  PROCEDURE get_current_location (VAR location: kt.ADDRESS): BOOLEAN;
  VAR
    field: dt.TYPE_RECORD_FIELD;
    type : dt.PTYPE;
    size : CARDINAL;
  BEGIN
    WITH p^ DO
      WITH Structs[Depth] DO
        IF (curr < N) AND tls.IsTypeValid(struct.var_type) THEN
          CASE design OF
          | dt.Array, dt.Array_of, dt.OpenArray:
           <* PUSH *>
           <* IOVERFLOW- *>
           <* COVERFLOW- *>
            tls.SubType (struct.var_type, type);
            CASE design OF
            | dt.Array:
              location := struct.location + curr*tls.TypeSize(type);
            | dt.OpenArray:
              IF exp.OpenArray_Size (struct, size) THEN
                location := struct.location + curr*size;
              ELSE
                location := 0;
              END;
            | dt.Array_of:
              IF exp.ArrayOf_Size (struct, size) THEN
                location := struct.location + curr*size;
              ELSE
                location := 0;
              END;
            END;
           <* POP *>
            IF location < struct.location THEN
              location := 0;
            END;

          | dt.Set :
            location := struct.location + (curr DIV 8);

          | dt.Record, dt.Class:
            tls.Field(struct.var_type, curr+1, field);
            location := struct.location + field.FieldOffs;
          END;
          RETURN TRUE;
        ELSE
          RETURN FALSE;
        END;
      END;
    END;
  END get_current_location;


  PROCEDURE write_line (current_line: CARDINAL);
  VAR
    tag  : dt.TYPE_TAG;
    field: dt.TYPE_RECORD_FIELD;
    item : exp.ExprRes;
  BEGIN
    <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
     ai[depth]:=current_line+1;
    <* END *>
    WITH p^ DO
      WITH Structs[Depth] DO
        IF (current_line < N) AND tls.IsTypeValid(struct.var_type) THEN
          CASE design OF
          | dt.Array, dt.Array_of, dt.OpenArray:
           <* PUSH *>
           <* IOVERFLOW- *>
           <* COVERFLOW- *>
            tls.ArrayIndexType (struct.var_type, index_type);
            item.sort := exp.Variable;
            tls.SubType (struct.var_type, item.var_type);
            exp.i2Str (index_type, current_line, buf);
            fmt.print (str1, '[%s]', buf);
            CASE design OF
            | dt.Array:
              item.location := struct.location + current_line*tls.TypeSize(item.var_type);
            | dt.OpenArray:
              IF exp.OpenArray_Size (struct, arr_size) THEN
                item.location := struct.location + current_line*arr_size;
              ELSE
                item.location := 0;
              END;
            | dt.Array_of:
              IF exp.ArrayOf_Size (struct, arr_size) THEN
                item.arr_desc := struct.arr_desc;
                item.location := struct.location + current_line*arr_size;
              ELSE
                item.location := 0;
              END;
            END;
           <* POP *>
            IF item.location < struct.location THEN
              item.location := 0;
            END;
            ASSERT(tls.TypeTag(item.var_type, tag));
            IF (tag = dt.Pointer) AND opt.DisplayDerefencePointer THEN
              item.type := dt.st_dereference;
            ELSE
              item.type := arr_st_id;
            END;
            exp.Res2Str (item, str2);

          | dt.Set :
            tls.SubType (struct.var_type, index_type);
            exp.i2Str (index_type, current_line, str1);
            IF exp.CheckBit(struct, current_line, bit) THEN
              IF bit THEN
                COPY('+', str2);
              ELSE
                COPY('-', str2);
              END;
            ELSE
              COPY('?', str2);
            END;

          | dt.Record, dt.Class:
            item.sort := exp.Variable;
            tls.Field(struct.var_type, current_line+1, field);
            item.var_type :=  field.FieldType;
            name := tls.GetName (field.FieldName);
            COPY(name^, str1);
            item.location := struct.location + field.FieldOffs;
            ASSERT(tls.TypeTag(item.var_type, tag));
            IF (tag = dt.Pointer) AND opt.DisplayDerefencePointer THEN
              item.type := dt.st_dereference;
            ELSE
              item.type := field.FieldSTID;
            END;
            exp.Res2Str (item, str2);
          END;
        ELSE
          COPY ("???", str1);
          COPY ("", str2);
        END;
      END;
      fmt.print (buf, '%-10s %s', str1, str2);
      WITH ListData DO
        crt.SetPos (2, current_line-frame+1);
       <*IF SCHERN_K26 THEN *>
        (*              SCHERN  *)
        IF MyIndVar() THEN
         crt.WrStrFromPos(hwnd, buf, attr, pos);
         attr:= crt.Attr(crt.Yellow, crt.LightMagenta);
         crt.LitePart (hwnd, current_line-frame+1, 1, size.x2-size.x1, attr);
        ELSE                                                          (*  end CHERN *)
          attr := crt.StructAttr[crt.Struct_Line];
          crt.WrStrFromPos(hwnd, buf, attr, pos);
          IF ODD(current_line) THEN
            attr := crt.StructAttr[crt.Struct_AlternateLine];
          END;
          crt.LitePart (hwnd, current_line-frame+1, 1, size.x2-size.x1, attr);
         END;                                                 (*  SCHERN *)
       <* ELSE *>
        attr := crt.StructAttr[crt.Struct_Line];
        crt.WrStrFromPos(hwnd, buf, attr, pos);
        IF ODD(current_line) THEN
          attr := crt.StructAttr[crt.Struct_AlternateLine];
        END;
        crt.LitePart (hwnd, current_line-frame+1, 1, size.x2-size.x1, attr);
       <* END *>
      END;
    END;
  END write_line;


  PROCEDURE PrepareContextMenu;
  VAR
    type_tag: dt.TYPE_TAG;
    field: dt.TYPE_RECORD_FIELD;
  BEGIN
    WITH p^ DO
      WITH Structs[Depth] DO
        WITH ListData DO
          actions[ContextItem_Type].tenable := act.TYPES_SET{};
         <* IF DEST_XDS THEN *>
          actions[ContextItem_AutoDetectActualType] := act.EMPTY_CONTEXT;
          actions[ContextItem_GetActualType] := act.EMPTY_CONTEXT;
         <* END *>
          IF NOT tls.IsTypeValid (struct.var_type) THEN
            RETURN;
          END;
          CASE design OF
          | dt.Array, dt.Array_of, dt.OpenArray:
            res.sort := exp.Variable;
            tls.SubType(struct.var_type, res.var_type);
            CASE design OF
            | dt.Array_of:
              IF NOT exp.ArrayOf_Size (struct, arr_size) THEN
                RETURN;
              END;
            | dt.OpenArray:
              IF NOT exp.OpenArray_Size (struct, arr_size) THEN
                RETURN;
              END;
            | dt.Array:
              arr_size := tls.TypeSize (res.var_type);
            END;
            res.location := struct.location + arr_size*Structs[Depth].curr;

          | dt.Record, dt.Class:
           <* IF DEST_XDS THEN *>
            IF design = dt.Class THEN
              -- для класса возможно определение настоящего типа
              actions[ContextItem_AutoDetectActualType+0] := act.CONTEXT{ act.separate };
              actions[ContextItem_AutoDetectActualType+1] := act.CONTEXT{ act.toggler, 'Auto-detect actual type', tlgAutoDetectActualType };
              actions[ContextItem_AutoDetectActualType+3] := act.EMPTY_CONTEXT;
              IF NOT opt.AutoDetectActualType THEN
                -- если автоопределение не включено, можно сделать выбором из меню
                actions[ContextItem_GetActualType+0] := act.CONTEXT{ act.separate };
                actions[ContextItem_GetActualType+1] := act.CONTEXT{ act.context_item, 'Get actual type'};
                actions[ContextItem_GetActualType+2] := act.EMPTY_CONTEXT;
              END;
            END;
           <* END *>
            IF N = 0 THEN
              RETURN;
            END;
            res.sort := exp.Variable;
            tls.Field (struct.var_type, Structs[Depth].curr+1, field);
            res.var_type :=  field.FieldType;
            res.location := struct.location + field.FieldOffs;

          | dt.Set:
            RETURN;
          END;

          IF exp.Var2Value(res, res) THEN
            CASE res.sort OF
            | exp.CARDval, exp.INTval, exp.WHOLEval:
              actions[ContextItem_Type].tenable := act.TYPES_SET{dt.st_original, dt.st_unsigned, dt.st_signed, dt.st_hex, dt.st_oct, dt.st_bin };
            | exp.Variable:
              ASSERT(tls.TypeTag(res.var_type, type_tag));
              IF type_tag = dt.Pointer THEN
                actions[ContextItem_Type].tenable := act.TYPES_SET{dt.st_original,dt.st_dereference};
              ELSIF type_tag = dt.Int THEN
                actions[ContextItem_Type].tenable := act.TYPES_SET{dt.st_original, dt.st_unsigned, dt.st_signed, dt.st_hex, dt.st_bin };
              END;
            ELSE
            END;
          END;
        END;
      END;
    END;
  END PrepareContextMenu;


VAR
  field  : dt.TYPE_RECORD_FIELD;
  main   : BOOLEAN;
  item   : CARDINAL;
  pointer: BOOLEAN;
  address: kt.ADDRESS;
  prev   : exp.ExprRes;

BEGIN
  p := PSTRUCT(win.GetAMPtr(hwnd));
  fmt.print (buf, "Var '%s'",p^.Structs[p^.Depth].StructName);
  win.SetHeaderByStr (hwnd, buf);
  UpdateList;
 <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
  (* SCHERN *)
  depth:= p^.Depth;
  FOR i := 0 TO depth DO
    ai[i]:= p^.Structs[i].curr+1;
  END;                     (*                         end SCHERN *)
 <* END *>
  size := win.GetWindowSize(hwnd);
  CASE msg.ID OF
  | eve.Rise:
    WITH p^ DO
      COPY(Structs[0].StructName, dv.VarName);
    END;
    std.ListBox (hwnd, msg);
    UpdateStructs;

  | eve.Hide:
    WITH p^ DO
      COPY(Structs[0].StructName, dv.VarName);
    END;
    std.ListBox (hwnd, msg);
    UpdateStructs;

  | eve.Paint, eve.Redraw:
    IF (msg.par # 3 ) AND (msg.par#4) THEN
      crt.FillWindow(hwnd, ' ', crt.StructAttr[crt.Struct_Background]);
    END;
    WITH size DO
      len := y2 - y1 - 1;
    END;
    WITH p^ DO
      WITH ListData DO
        IF N > 0 THEN
          CASE msg.par OF
          | 3:
            IF hwnd = win.ActiveWindow THEN
              write_line(curr);
             <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
              ai[depth]:=curr+1;                                          (*              SCHERN  *)
              IF MyIndVar() THEN
               crt.Lite(hwnd, curr-frame+1, 1,crt.Attr(crt.Yellow, crt.Bg(Colors^[crt.List_CurrentLine])));
              ELSE                                             (* end SCHERN   *)
               crt.Lite(hwnd, curr-frame+1, 1,Colors^[crt.List_CurrentLine]);
              END;
            <* ELSE *>
              crt.Lite(hwnd, curr-frame+1, 1, Colors^[crt.List_CurrentLine]);
            <* END *>
            END;
          | 4:
            IF hwnd = win.ActiveWindow THEN
              crt.Lite(hwnd, curr-frame+1, 1, Colors^[crt.List_Background]);
              write_line(curr);
            END;
          ELSE
            last := std.Min(frame+len-1, N-1);
            FOR i := frame TO last DO
              write_line(i)
            END;
            FOR i := last+1 TO frame+len-1 DO
              IF ODD(i) THEN
                attr := crt.StructAttr[crt.Struct_AlternateLine];
              ELSE
                attr := crt.StructAttr[crt.Struct_Line];
              END;
              crt.LitePart (hwnd, i-frame+1, 1, size.x2-size.x1, attr);
            END;
            IF hwnd = win.ActiveWindow THEN
            <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
             ai[depth]:=curr+1;                                             (*              SCHERN  *)
             IF MyIndVar() THEN
              crt.Lite(hwnd, curr-frame+1, 1,crt.Attr(crt.Yellow, crt.Bg(Colors^[crt.List_CurrentLine])));
             ELSE                                             (* end SCHERN   *)
              crt.Lite(hwnd, curr - frame + 1 , 1, Colors^[crt.List_CurrentLine]);
             END;
            <* ELSE *>
             crt.Lite(hwnd, curr - frame + 1 , 1, Colors^[crt.List_CurrentLine]);
            <* END *>
            END;
          END;
        END;
      END;
      WITH Structs[Depth] DO
        IF AddrInHeader THEN
          fmt.print (str1, exp.Fmt_ADDRval, struct.location);
          xs.Append (': ', str1);
        ELSE
          COPY ("", str1);
        END;
        fmt.append (str1, '%s', StructName);
        IF design = dt.Class THEN
          ASSERT(tls.TypeName(struct.var_type, name));
          fmt.print (buf, ' (%s)', name^);
          xs.Append (buf, str1);
        END;
      END;
    END;

    win.GetHeader (hwnd, str2);
    win.SetHeaderByStr (hwnd, str1);
    std.ListBox (hwnd, msg);
    UpdateStructs;
    win.SetHeaderByStr (hwnd, str2);

  | eve.KbHit:
    CASE msg.par OF
    | key.Esc:
      WITH p^ DO
        IF Depth = 0 THEN
          eve.AddToTail(hwnd, eve.Hide, 0);
        ELSE
          DEC(Depth);
          eve.AddToTail(hwnd, eve.Redraw,  0);
        END;
      END;
   <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
    | key.Alt5:
        Add_Struct_var(p);                                   (* SCHERN *)
   <* END *>
    | key.Enter, key.CtrlEnter:
      buf := '';
      WITH p^ DO
        res := Structs[Depth].struct;
        IF NOT tls.IsTypeValid(res.var_type) THEN
          IF NOT OpenNewWindowCheck THEN
            crt.Beep;
          END;
          RETURN;
        END;
        CASE Structs[Depth].design OF
        | dt.Array_of:
          tls.ArrayIndexType(res.var_type, index_type);
          IF NOT exp.ArrayOf_Size(res, arr_size) THEN
            IF NOT OpenNewWindowCheck THEN
              std.ErrorNo (mes.InvalidArrayDesc);
            END;
            RETURN;
          END;
          tls.SubType(res.var_type, res.var_type);
          res.location := Structs[Depth].struct.location+arr_size*Structs[Depth].curr;
          exp.i2Str(index_type, Structs[Depth].curr, str1);
          fmt.print(buf, '%s[%s]', Structs[Depth].StructName, str1);

        | dt.OpenArray:
          tls.ArrayIndexType(res.var_type, index_type);
          IF NOT exp.OpenArray_Size(res, arr_size) THEN
            IF NOT OpenNewWindowCheck THEN crt.Beep; END;
            RETURN;
          END;
          tls.SubType(res.var_type, res.var_type);
          res.location := Structs[Depth].struct.location+arr_size*Structs[Depth].curr;
          exp.i2Str(index_type, Structs[Depth].curr, str1);
          fmt.print(buf, '%s[%s]', Structs[Depth].StructName, str1);

        | dt.Array:
          tls.ArrayIndexType(res.var_type, index_type);
          tls.SubType(res.var_type, res.var_type);
          res.location := Structs[Depth].struct.location + Structs[Depth].curr * tls.TypeSize(res.var_type);
          exp.i2Str(index_type, Structs[Depth].curr, str1);
          fmt.print(buf, '%s[%s]', Structs[Depth].StructName, str1);

        | dt.Record, dt.Class:
          IF Structs[Depth].N # 0 THEN
            tls.Field (res.var_type, Structs[Depth].curr+1, field);
            res.var_type :=  field.FieldType;
            res.location := Structs[Depth].struct.location+field.FieldOffs;
            name := tls.GetName (field.FieldName);
            fmt.print(buf, '%s.%s', Structs[Depth].StructName, name^);
          ELSIF NOT OpenNewWindowCheck THEN
            crt.Beep;
            RETURN;
          END;

        | dt.Set:
          IF NOT OpenNewWindowCheck THEN
            IF exp.ToggleBit(res, Structs[Depth].curr) THEN
              dw.RecalcWatches;
              eve.AddToTail(eve.AllWindows, eve.Redraw, 0);
            ELSE
              crt.Beep;
            END;
          END;
          RETURN;
        END;
      END;
      Res2Change := res;
      res2 := res;
      ASSERT(tls.TypeTag(res.var_type, tag));
      IF NOT (tag IN dt.TYPE_TAG_SET{dt.Array, dt.Array_of, dt.OpenArray, dt.Record, dt.Class, dt.Set, dt.Pointer}) THEN
        IF NOT OpenNewWindowCheck THEN
          IF NOT exp.Var2Value(res, res2) THEN
            crt.Beep;
            RETURN;
          END;
          COPY(buf, dv.VarName);
          exp.Res2Str(res2, std.NewValue);
          std.IniInputDialog(ChangeVarValue, TRUE, buf);
        END;
        RETURN;
      ELSE
        IF tag = dt.Pointer THEN
          IF msg.par = key.CtrlEnter THEN
            IF NOT OpenNewWindowCheck THEN
              exp.Res2Str(res, std.NewValue);
              std.IniInputDialog(ChangeVarValue, TRUE, buf);
            END;
            RETURN;
          ELSE
            LOOP
              tls.SubType (res.var_type, pointed);
              ASSERT (tls.TypeTag(pointed, tag));
              IF tag # dt.Pointer THEN
                EXIT;
              END;
              prev := res;
              IF exp.Dereference (res, res) THEN
                xs.Append ('^', buf);
              ELSE
                res := prev;
                EXIT;
              END;
            END;
            IF tag IN dt.TYPE_TAG_SET{dt.Array, dt.Array_of, dt.OpenArray, dt.Record, dt.Class, dt.Set} THEN
              IF exp.Dereference(res, res) THEN
                xs.Append('^', buf);
              ELSE
                IF NOT OpenNewWindowCheck THEN
                  std.ErrorNo (mes.NilPointerDereference);
                END;
                RETURN;
              END;
            ELSE
              IF NOT OpenNewWindowCheck THEN
                IF tag = dt.Procedure THEN
                  res.var_type := pointed;
                ELSIF NOT exp.Dereference(res, res) THEN
                  std.ErrorNo (mes.NilPointerDereference);
                  RETURN;
                END;
                exp.Res2Str(res, std.NewValue);
                Res2Change := res;
                std.IniInputDialog(ChangeVarValue, TRUE, buf);
              END;
              RETURN;
            END;
          END;
        END;
        -- Заполнение нового состояния структуры
        WITH Struct DO
          curr := 0;
          frame := 0;
          struct := res;
          ASSERT (tls.TypeTag(struct.var_type, design));
          arr_st_id := dt.st_original;
          COPY(buf, StructName);
          CASE design OF
          | dt.Array_of:
            IF NOT exp.ArrayOf_Len (struct, N) THEN
              IF NOT OpenNewWindowCheck THEN
                std.ErrorNo (mes.InvalidArrayDesc);
              END;
              RETURN;
            END;
          | dt.OpenArray:
            IF NOT exp.OpenArray_Len (struct, N) THEN
              IF NOT OpenNewWindowCheck THEN crt.Beep; END;
              RETURN;
            END;
          | dt.Set:
            tls.Index(struct.var_type, min, max);
            N := max - min + 1;
          | dt.Class:
(*
           <* IF DEST_XDS THEN *>
            IF (p^.Depth = 0) AND opt.AutoDetectActualType THEN
              exp.GetActualType (struct);
            END;
           <* END *>
*)
            N := tls.TypeLen (struct.var_type);
          ELSE
            N := tls.TypeLen (struct.var_type);
          END;
        END;
        IF OpenNewWindowCheck THEN
          act.ConfirmQuery;
        ELSE
          IF OpenNewWindowMode THEN
            OpenStructWindow (size, FALSE); -- Multiple structure windows
            p := PSTRUCT(win.GetAMPtr(StructWindow));
            eve.AddToTail(StructWindow, eve.Rise, 0);
(*
            size := win.GetWindowSize(StructWindow);
            size.y2 := size.y1+Struct.N+1;
            IF size.y2 >= crt.Ymax THEN size.y2 := crt.Ymax-1; END;
            win.SetWindowSize (StructWindow, size);
*)
          ELSE
            WITH p^ DO
              IF Depth = MaxDepth THEN
                crt.Beep;
                RETURN;
              END;
              INC(Depth);
            END;
            eve.AddToTail(hwnd, eve.Redraw, 0);
          END;
          WITH p^ DO
            Structs[Depth] := Struct;
          END;
        END;
      END;

    ELSE
      std.ListBox (hwnd, msg);
      UpdateStructs;
    END;

  | eve.QueryAction:
    CASE std.GetAction (msg.par) OF
    | act.Examine,
      act.AddWatch,
     <* IF DEST_XDS THEN *>
      act.Access,
     <* END *>
      act.ContextMenu,
      act.Dump:
      act.ConfirmQuery;
    | act.ChangeTypes:
      act.ConfirmQueryByCond(p^.ListData.actions[ContextItem_Type].tenable # act.TYPES_SET{});
    ELSE
      std.ListBox (hwnd, msg);
      UpdateStructs;
    END;

  | eve.DoAction:
    CASE std.GetAction (msg.par) OF
    | act.Examine:
      GetCurrName (str1, pointer);
      COPY(str1, dv.VarName);
      act.ExecuteAction (act.Examine, act.mode_silent);
    <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
     | act.prf_var0:
(*                                                              SCHERN              *)
          Add_Struct_var(p);
     | act.prf_var9:
           depth:= p^.Depth;
           FOR i := 0 TO depth DO
           ai[i]:= p^.Structs[i].curr+1;
           END;                     (*                          *)
         IF prv.DelIndVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN,prf_Var_Ind,depth,ai) THEN
          eve.AddToTail(hwnd, eve.Redraw, 0);
         END;
   | act.prf_var6:
           depth:= p^.Depth;
           FOR i := 0 TO depth DO
           ai[i]:= p^.Structs[i].curr+1;
           END;                     (*                         end SCHERN *)
      wbr.ShowCountIndVar(mod.Curr^.Pos.ComN,mod.Curr^.Pos.ModN,prf_Var_Ind,depth,ai);

(*                                                                  end SCHERN        *)
   <* END *>
    | act.Dump:
      IF get_current_location (address) THEN
        fmt.print(dv.DumpAddrStr, exp.Fmt_ADDRval, address);
        act.ExecuteAction (act.Dump, act.mode_silent);
      ELSE
        act.ExecuteAction (act.Dump, act.mode_loud);
      END;
    | act.AddWatch:
      GetCurrName (str1, pointer);
      fmt.print (dv.expr,'%s', str1);
      act.ExecuteAction (act.AddWatch, act.mode_silent);
   <* IF DEST_XDS THEN *>
    | act.Access:
      GetCurrName (str1, pointer);
      IF get_current_location (address) THEN
        fmt.print (buf, exp.Fmt_ADDRval, address);
        dbrk.SetAccessAttr (buf, AccessLen);
        act.ExecuteAction (act.Access, act.mode_silent);
      ELSE
        COPY ("", buf);
        dbrk.SetAccessAttr (buf, AccessLen);
        act.ExecuteAction (act.Access, act.mode_loud);
      END;
    <* END *>
    | act.ContextMenu:
      PrepareContextMenu;
      std.ListBox (hwnd, msg);
      UpdateStructs;
    | act.ChangeTypes:
      WITH p^ DO
        WITH Structs[Depth] DO
          IF (N = 0) OR NOT tls.IsTypeValid (struct.var_type) THEN RETURN; END;
          IF ListData.actions[ContextItem_Type].tenable = act.TYPES_SET{} THEN RETURN; END;
          CASE design OF
          | dt.Array, dt.Array_of, dt.OpenArray:
            arr_st_id := act.ChangeTypesID;
          | dt.Record, dt.Class:
            tls.SetFieldSTID (struct.var_type, curr+1, act.ChangeTypesID);
          | dt.Set:
          END;
        END;
      END;
    ELSE
      std.ListBox (hwnd, msg);
      UpdateStructs;
    END;

  | eve.QueryItem:
    std.GetItem (msg.par, main, item);
    ASSERT(main);
    CASE item OF
    | ContextItem_Pointed:
      GetCurrName (str1, pointer);
      act.ConfirmQueryByCond (pointer AND exp.GetAddress (mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, str1, address) AND (address # 0));
    | ContextItem_OpenNewWindow:
      OpenNewWindowMode := TRUE;
      OpenNewWindowCheck := TRUE;
      eve.AddToTail (hwnd, eve.KbHit, key.Enter);
      eve.Flush;
      OpenNewWindowMode := FALSE;
      OpenNewWindowCheck := FALSE;
   <* IF DEST_XDS THEN *>
    | ContextItem_AddrInHeader:
      act.ConfirmQuery();
    | ContextItem_GetActualType+1: -- так как нужно учесть разделитель
      ASSERT(p^.Structs[p^.Depth].design = dt.Class);
      ASSERT(NOT opt.AutoDetectActualType);
      act.ConfirmQuery;
   <* END *>
    END;

  | eve.ContextItem:
    std.GetItem (msg.par, main, item);
    ASSERT(main);
    CASE item OF
    | ContextItem_Pointed:
      GetCurrName (str1, pointer);
      ASSERT(pointer AND exp.GetAddress (mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, str1, address) AND (address # 0));
      fmt.print (dv.DumpAddrStr, exp.Fmt_ADDRval, address);
      act.ExecuteAction (act.Dump, act.mode_silent);
    | ContextItem_OpenNewWindow:
      OpenNewWindowMode := TRUE;
      OpenNewWindowCheck := FALSE;
      eve.AddToTail (hwnd, eve.KbHit, key.Enter);
      eve.Flush;
      OpenNewWindowMode := FALSE;
   <* IF DEST_XDS THEN *>
    | ContextItem_AddrInHeader:
      p^.AddrInHeader := NOT p^.AddrInHeader;
      IF p^.AddrInHeader THEN
        p^.ListData.actions[ContextItem_AddrInHeader] := act.CONTEXT{ act.context_item, 'Hide address' };
      ELSE
        p^.ListData.actions[ContextItem_AddrInHeader] := act.CONTEXT{ act.context_item, 'Show address' };
      END;
    | ContextItem_GetActualType+1:
      WITH p^ DO
        WITH Structs[Depth] DO
          ASSERT(NOT opt.AutoDetectActualType AND (design = dt.Class));
          exp.GetActualType (struct);
          N := tls.TypeLen (struct.var_type);
          frame := 0;
          curr := 0;
          eve.AddToTail (hwnd, eve.Redraw, 0);
        END;
      END;
   <* END *>
    END;

  | eve.R_Mouse_Pressed:
    ASSERT(win.GetMouse(msg, x, y));
    IF NOT std.CheckFrame(size, x, y) THEN
      eve.AddToTail(hwnd, eve.Mouse_Pressed, msg.par);
      eve.Flush;
    END;
    PrepareContextMenu;
    std.ListBox (hwnd, msg);
    UpdateStructs;

  ELSE
    std.ListBox (hwnd,msg);
    UpdateStructs;
  END;
END StructList;


PROCEDURE StructVar (VarName-: ARRAY OF CHAR; res: exp.ExprRes);
VAR
  p   : PSTRUCT;
  tag : dt.TYPE_TAG;
  min : CARDINAL;
  max : CARDINAL;
  tmp : xs.String;
--  size: crt.SZ;
BEGIN
  COPY(VarName, tmp);
  ASSERT(res.sort = exp.Variable);
  ASSERT(tls.TypeTag(res.var_type, tag));
  CASE tag OF
  | dt.Array, dt.Array_of, dt.OpenArray, dt.Record, dt.Class, dt.Set:
    IF (StructWindow # win.Invalid_H) AND opt.UseSingleStructureWindow THEN
      eve.AddToTail (StructWindow, eve.Hide, 0);
      eve.Flush;
    END;
    -- Создание окна структурных переменных
    OpenStructWindow (crt.SZ{ crt.Xmax-30, 1, crt.Xmax-1, 15}, opt.UseSingleStructureWindow);
    -- Инициализация окна структурных переменных
    p := PSTRUCT(win.GetAMPtr(StructWindow));
    WITH p^ DO
      WITH Structs[Depth] DO;
        design := tag;
        arr_st_id := dt.st_original;
        struct := res;
        COPY(tmp, StructName);
        CASE design OF
        | dt.Array_of:
          IF exp.ArrayOf_Len (struct, N) THEN
            IF N = 0 THEN
              std.Error ("Zero length array");
              RETURN;
            END;
          ELSE
            std.ErrorNo (mes.InvalidArrayDesc);
            RETURN;
          END;
        | dt.OpenArray:
          IF NOT exp.OpenArray_Len (struct, N) THEN
            crt.Beep;
            RETURN;
          END;
        | dt.Set:
          tls.Index (struct.var_type, min, max);
          N := max - min + 1;
        | dt.Class:
         <* IF DEST_XDS THEN *>
          IF opt.AutoDetectActualType THEN
            exp.GetActualType (struct);
          END;
         <* END *>
          N := tls.TypeLen (struct.var_type);
        ELSE
          N := tls.TypeLen (struct.var_type);
        END;
(*
        size := win.GetWindowSize(StructWindow);
        size.y2 := size.y1+N+1;
        IF size.y2 >= crt.Ymax THEN size.y2 := crt.Ymax-1; END;
        win.SetWindowSize (StructWindow, size);
*)
      END;
    END;
    eve.AddToTail(StructWindow, eve.Rise, 0);
  ELSE
  END;
END StructVar;

PROCEDURE ChangeVariable (hwnd: win.HWND; var: exp.ExprRes): BOOLEAN;
VAR
  res2 : exp.ExprRes;
  tag : dt.TYPE_TAG;
BEGIN
  IF (var.sort = exp.Register) THEN
    res2 := var;
    exp.Res2Str (res2, std.NewValue);
    IF hwnd # win.Invalid_H THEN
      eve.AddToTail (hwnd, eve.Hide, 0);
      eve.Flush;
    END;
    Res2Change := var;
    std.IniInputDialog (ChangeVarValue, TRUE, dv.VarName);
    RETURN TRUE;
  END;
  IF (var.sort # exp.Variable) THEN
    std.SetErrorNo (mes.Expected_var);
    RETURN FALSE;
  END;
  ASSERT(tls.TypeTag(var.var_type, tag));
  res2 := var;
  IF NOT exp.Var2Value(var, res2) THEN
    IF (exp.error # 0) THEN
      std.SetErrorNo(exp.error);
    END;
    RETURN FALSE;
  END;
  IF (res2.sort # exp.Variable) OR (tag = dt.Pointer) OR (tag = dt.Enum) OR (tag = dt.Procedure) THEN
    exp.Res2Str(res2, std.NewValue);
    IF hwnd # win.Invalid_H THEN
      eve.AddToTail(hwnd, eve.Hide, 0); eve.Flush;
    END;
    Res2Change := var;
    std.IniInputDialog(ChangeVarValue, TRUE, dv.VarName);
  ELSE
    CASE tag OF
    | dt.Array, dt.Array_of, dt.OpenArray, dt.Record, dt.Class, dt.Set:
(*
      IF NOT exp.CheckRegionValid(var) THEN
        std.SetErrorNo(mes.Incorrect_address);
        RETURN FALSE;
      END;
*)
      IF hwnd # win.Invalid_H THEN
        eve.AddToTail(hwnd, eve.Hide, 0);
        eve.Flush;
      END;
      StructVar (dv.VarName, var);
    | dt.Complex:
      std.SetErrorMsg ('Variable of complex type is not supported yet.');
      RETURN FALSE;
    ELSE
      std.SetErrorMsg ('Variable of that type is not supported yet.');
      RETURN FALSE;
    END;
  END;
  RETURN TRUE;
END ChangeVariable;

PROCEDURE ChooseVar (hwnd: win.HWND): BOOLEAN;
VAR
  res: exp.ExprRes;
BEGIN
  exp.GetLeftValue (mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, dv.VarName, res);
  IF (exp.error # 0) THEN
    std.SetErrorNo (exp.error);
    RETURN FALSE;
  END;
  IF NOT exp.dfn THEN
    std.SetErrorNo (mes.Undefined_Expression);
    RETURN FALSE;
  END;
  RETURN ChangeVariable (hwnd, res);
END ChooseVar;


PROCEDURE Variable (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Examine);
  IF NOT kexe.Loaded THEN
    RETURN FALSE;
  END;
  IF mode = act.mode_loud THEN
    std.OpenUniversalDialog (mes.dlg_EnterVariable, ChooseVar, dv.VarName);
  ELSIF mode = act.mode_silent THEN
    IF NOT ChooseVar (win.Invalid_H) THEN
      eve.AddToTail (std.ErrorMsg, eve.Rise, 0);
    END;
  END;
  RETURN TRUE;
END Variable;


VAR
  ExprString: std.MESSAGE;
  ExprStrNum: CARDINAL;
  ExprResNum: CARDINAL;
  ExprRadNum: CARDINAL;

<* WOFF301+ *>
PROCEDURE EvalExpr(hwnd: win.HWND): BOOLEAN;
VAR
  res: exp.ExprRes;
  p: std.PDIALOG;
  i: CARDINAL;
BEGIN
  p := win.GetAMPtr(CalcDialog);
  WITH p^ DO
    exp.CalcExpr (mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, ExprString, res);
    WITH Lines^[ExprRadNum] DO
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
    WITH Lines^[ExprResNum] DO
      IF exp.error = 0 THEN
        exp.Res2Str (res, str);
      ELSE
        pro.GetMsg(exp.error, str);
        curr := ExprStrNum;
        eve.AddToTail (CalcDialog, eve.KbHit, key.Home);
        FOR i := 1 TO exp.GetLastErrorPos() DO
          eve.AddToTail (CalcDialog, eve.KbHit, key.Right);
        END;
        Lines^[ExprRadNum].state := std.d_disabled;
        RETURN TRUE;
      END;
    END;
    IF curr = ExprRadNum THEN
      curr := ExprStrNum;
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


PROCEDURE CalcExpr (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
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
  IF mode # act.mode_check THEN
    IF CalcDialog = win.Invalid_H THEN
      pro.GetMsg(mes.InputExpression, zzz);

      NEW (Radio, 4);
      Radio^[0] := std.RADIO{ 3, 8, 'Hex', 1, key.AltH };
      Radio^[1] := std.RADIO{12, 8, 'Dec', 1, key.AltD };
      Radio^[2] := std.RADIO{21, 8, 'Oct', 1, key.AltO };
      Radio^[3] := std.RADIO{30, 8, 'Bin', 1, key.AltB };

      ExprStrNum := 2;
      ExprResNum := 9;
      ExprRadNum := 10;

      NEW (Lines, 11);
      Lines^[ 0] := LINE{ 2, 2, std.msg     , zzz , std.d_enabled};
      Lines^[ 1] := LINE{ 2, 3, std.msg     , '│' , std.d_enabled};
      Lines^[ExprStrNum] := LINE{ 4, 3, std.edit_str, sys.ADR(ExprString), 35, std.d_enabled};
      Lines^[ 3] := LINE{40, 3, std.msg     , '│' , std.d_enabled};
      Lines^[ 4] := LINE{ 2, 4, std.msg     , '│' , std.d_enabled};
      Lines^[ 5] := LINE{40, 4, std.msg     , '│' , std.d_enabled};
      Lines^[ 6] := LINE{ 2, 5, std.msg     , '│' , std.d_enabled};
      Lines^[ 7] := LINE{40, 5, std.msg     , '│' , std.d_enabled};
      Lines^[ 8] := LINE{ 2, 6, std.msg     , '└─────────────────────────────────────┘' , std.d_enabled};

      Lines^[ExprResNum] := LINE{ 4, 5, std.msg   , '???' , std.d_enabled};
      Lines^[ExprRadNum] := LINE{ 2, 8, std.radio , '' , 0, 0, 0, 0, Radio, UpdateExpr, std.d_enabled};

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
    IF NOT opt.WholeHex THEN
      p^.Lines^[ExprRadNum].ractive := 1;
    END;
    eve.AddToTail(CalcDialog, eve.Rise, 0);
  END;
  RETURN TRUE;
END CalcExpr;


PROCEDURE InitLblWindow(show: BOOLEAN);
VAR
  p    : std.PLIST;
BEGIN
  WITH std.Wnds[std.LblWindow] DO
    IF hwnd = win.Invalid_H THEN
      hwnd := win.RegisterWindow (ProceduresHandler, SIZE(std.LIST));
      ASSERT(hwnd # win.Invalid_H);

      win.SetMovable(hwnd);
      win.SetResizable(hwnd, TRUE, TRUE);
      win.SetSwitchable(hwnd);

      win.SetWindowSize(hwnd, size);

      p := win.GetAMPtr(hwnd);
      win.SetHeader(hwnd, mes.Procedures);
      p^ := std.EmptyList;
      WITH p^ DO
        Colors     := sys.ADR(crt.List);
        locator    := lctrLabels;
        actions[0] := act.CONTEXT{ act.push_key, 'Go to', key.Enter };
        actions[1] := act.CONTEXT{ act.push_key, 'Break', key.None };
        actions[2] := act.CONTEXT{ act.push_key, 'Clear', key.None };
        actions[3] := act.CONTEXT{ act.push_key, 'Break all', key.None };
        actions[4] := act.CONTEXT{ act.push_key, 'Clear all', key.None };
        actions[5] := act.EMPTY_CONTEXT;
      END;
    END;
    IF show THEN eve.AddToTail(std.Wnds[std.LblWindow].hwnd, eve.Rise, 0); END;
  END;
END InitLblWindow;


PROCEDURE InitLabelsList (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Procs);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  InitLblWindow (mode # act.mode_check);
  IF mode = act.mode_check THEN
    RETURN std.QueryAction(std.Wnds[std.LblWindow].hwnd, act.Procs);
  ELSE
    RETURN TRUE;
  END;
END InitLabelsList;

<* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
 PROCEDURE prf_var9 (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
RETURN TRUE;
END prf_var9;

PROCEDURE prf_var0 (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
RETURN TRUE;
END prf_var0;

PROCEDURE prf_var8 (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
RETURN TRUE;                                                               (*  SCHERN *)
END prf_var8;

PROCEDURE prf_var7 (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
RETURN TRUE;
END prf_var7;

PROCEDURE prf_var6 (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
RETURN TRUE;
END prf_var6;

<* END *>


PROCEDURE InitLocalVarWindow(show: BOOLEAN);
VAR
  p: std.PLIST;
  s: act.NAME;
BEGIN
  WITH std.Wnds[std.LocalVarWindow] DO
    IF hwnd = win.Invalid_H THEN
      hwnd := win.RegisterWindow (LocalVariablesHandler, SIZE(std.LIST));
      ASSERT(hwnd # win.Invalid_H);

      win.SetWindowSize(hwnd, size);

      win.SetMovable(hwnd);
      win.SetResizable(hwnd, TRUE, TRUE);
      win.SetSwitchable(hwnd);
      win.SetHeader(hwnd, mes.LocalVariables);

      p := win.GetAMPtr(hwnd);
      p^ := std.EmptyList;
      WITH p^ DO
        Colors     := sys.ADR(crt.List);
        Frame      := crt.Double;
        locator    := lctrLocalVariables;
        actions[0] := act.CONTEXT{ act.do_action, act.Examine  };
       <* IF DEST_K26 THEN *>
        actions[1] := act.CONTEXT{ act.do_action, act.ConditionBreak };
       <* ELSIF DEST_XDS THEN *>
        actions[1] := act.CONTEXT{ act.do_action, act.Access   };
       <* END *>
        actions[2] := act.CONTEXT{ act.do_action, act.AddWatch };
        actions[3] := act.CONTEXT{ act.do_action, act.Dump };
        actions[4] := act.CONTEXT{ act.context_item, "Pointed memory" };
        actions[ContextItem_Type] := act.CONTEXT{ act.types, act.TYPES_SET{} };
        actions[6] := act.CONTEXT{ act.separate };
        fmt.print (s, "%c go up", CHR(30));
        actions[Local_ContextItem_GoUpCall] := act.CONTEXT{ act.push_key, s, key.CtrlUp };
        fmt.print (s, "%c go down", CHR(31));
        actions[Local_ContextItem_GoDownCall] := act.CONTEXT{ act.push_key, s, key.CtrlDown };
        actions[Local_ContextItem_ShowLocation-1] := act.CONTEXT{ act.separate };
        actions[Local_ContextItem_ShowLocation] := act.CONTEXT{ act.toggler, 'Show location', tglShowLocation };
        actions[Local_ContextItem_ShowLocation+1] := act.EMPTY_CONTEXT;
      END;
    END;
    IF show THEN eve.AddToTail(hwnd, eve.Rise, 0); END;
  END;
END InitLocalVarWindow;


PROCEDURE InitLocalVarsList(action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.LocalVars);
  IF NOT (kexe.Loaded AND tls.IsObjectValid(exe.ExecScope)) THEN RETURN FALSE; END;
  InitLocalVarWindow (mode # act.mode_check);
  IF mode = act.mode_check THEN
    RETURN std.QueryAction(std.Wnds[std.LocalVarWindow].hwnd, act.LocalVars);
  ELSE
    RETURN TRUE;
  END;
END InitLocalVarsList;


PROCEDURE NormalizeWatches;
VAR
  p : std.PLIST;
  sz: crt.SZ;
BEGIN
  WITH std.Wnds[std.WatchWindow] DO
    IF hwnd # win.Invalid_H THEN
      p := win.GetAMPtr(std.Wnds[std.WatchWindow].hwnd);
      WITH p^ DO
        IF N > 0 THEN
          curr  := N-1;
          sz := win.GetWindowSize (hwnd);
          std.Normalize (sz, curr, frame, N);
        END;
      END;
    END;
  END;
END NormalizeWatches;



PROCEDURE InitWatchWindow (show: BOOLEAN);
VAR
  p    : std.PLIST;
BEGIN
  WITH std.Wnds[std.WatchWindow] DO
    IF hwnd = win.Invalid_H THEN
      hwnd := win.RegisterWindow(WatchesHandler,SIZE(std.LIST));
      ASSERT(hwnd # win.Invalid_H);

      win.SetWindowSize(hwnd, size);

      win.SetMovable(hwnd);
      win.SetResizable(hwnd, TRUE, TRUE);
      win.SetSwitchable(hwnd);
      win.SetHeader(hwnd, mes.Watches);

      p := win.GetAMPtr(hwnd);
      p^ := std.EmptyList;
      WITH p^ DO
        Colors     := sys.ADR(crt.List);
        locator    := NIL;
        actions[0] := act.CONTEXT{ act.do_action, act.Examine  };
       <* IF DEST_K26 THEN *>
        actions[1] := act.CONTEXT{ act.do_action, act.ConditionBreak };
       <* ELSIF DEST_XDS THEN *>
        actions[1] := act.CONTEXT{ act.do_action, act.Access   };
       <* END *>
        actions[2] := act.CONTEXT{ act.do_action, act.Dump };
        actions[ContextWatch_PMemory] := act.CONTEXT{ act.context_item, "Pointed memory" };
        actions[4] := act.CONTEXT{ act.types, act.TYPES_SET{}};
        actions[5] := act.CONTEXT{ act.separate };
       
        actions[6] := act.CONTEXT{ act.do_action, act.AddWatch };
        actions[7] := act.CONTEXT{ act.do_action, act.DelWatch };
        actions[ContextWatch_Edit] := act.CONTEXT{ act.context_item, "Edit watch" };
        actions[9] := act.EMPTY_CONTEXT;
      END;
    ELSE
      p := win.GetAMPtr(std.Wnds[std.WatchWindow].hwnd);
      p^.curr  := 0;
      p^.frame := 0;
    END;
    IF show THEN eve.AddToTail(hwnd, eve.Rise, 0); END;
  END;
END InitWatchWindow;


PROCEDURE InitWatchList (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Watches);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    InitWatchWindow(TRUE);
  END;
  RETURN TRUE;
END InitWatchList;


<* IF DEST_XDS THEN *>

PROCEDURE ThreadsHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p    : std.PLIST;
  buf  : xs.String;

  PROCEDURE write_line(num: CARDINAL);
  VAR
    l   : CARDINAL;
  BEGIN
    WITH p^ DO
      l := num-frame + 1;
      trd.GetThreadDescription (num, buf);
      crt.SetPos(2, l);
      crt.WrStr(hwnd, buf, Colors^[crt.List_Line]);
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
    IF ( msg.par # 3) AND (msg.par # 4) THEN
      crt.FillWindow(hwnd, ' ', p^.Colors^[crt.List_Background])
    END;
    WITH size DO len := y2 - y1 - 1; END;
    WITH p^ DO
      IF kexe.Loaded  THEN
        N := trd.NThreads ();
      ELSE
        N := 0;
      END;
      IF (curr > N) OR (frame > N) THEN curr := 0; frame := 0; END;
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
    std.ListBox(hwnd, msg);

  | eve.QueryAction:
    IF std.GetAction (msg.par) IN act.ACTION_SET {act.Disable, act.Enable} THEN
      act.ConfirmQueryByCond (p^.N > 0);
    ELSE
      std.ListBox(hwnd, msg);
    END;

  | eve.DoAction:
    WITH p^ DO
      CASE std.GetAction (msg.par) OF
      | act.Disable:
        IF N = 0 THEN RETURN END;
        IF NOT trd.SuspendThread (curr) THEN
          crt.Beep;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      | act.Enable:
        IF N = 0 THEN RETURN END;
        IF NOT trd.ResumeThread (curr) THEN
          crt.Beep;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      ELSE
        std.DefaultProc(hwnd, msg);
      END;
    END;

  | eve.QueryKbHit:
    CASE msg.par OF
    | key.Enter
    , key.Plus
    , key.Minus:
      act.ConfirmQueryByCond (p^.N > 0);
    ELSE
      std.ListBox(hwnd, msg);
    END;

  | eve.KbHit:
    WITH p^ DO
      CASE msg.par OF
      | key.Enter:
        IF NOT trd.SwitchToThread (curr) THEN
          crt.Beep;
        END;
        mem.GetCaches;
        dmm.RefreshRegs;
        dex.PointExecLine(mem.GetIP());
      | key.Minus:
        IF N = 0 THEN RETURN END;
        IF NOT trd.SuspendThread (curr) THEN
          crt.Beep;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      | key.Plus:
        IF N = 0 THEN RETURN END;
        IF NOT trd.ResumeThread (curr) THEN
          crt.Beep;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      ELSE
        std.ListBox(hwnd, msg);
      END;
    END;

  ELSE
    std.ListBox(hwnd, msg);
  END;
END ThreadsHandler;

PROCEDURE InitThreadsWindow (show: BOOLEAN);
VAR
  p    : std.PLIST;
BEGIN
  WITH std.Wnds[std.ThreadsWindow] DO
    IF hwnd = win.Invalid_H THEN
      hwnd := win.RegisterWindow(ThreadsHandler,SIZE(std.LIST));
      ASSERT(hwnd # win.Invalid_H);

      win.SetWindowSize (hwnd, size);

      win.SetMovable (hwnd);
      win.SetResizable (hwnd, TRUE, TRUE);
      win.SetSwitchable (hwnd);

      win.SetHeaderByStr (hwnd, 'Threads');
      p := win.GetAMPtr (hwnd);
      p^ := std.EmptyList;
      WITH p^ DO
        Colors     := sys.ADR(crt.List);
        actions[0] := act.CONTEXT{ act.push_key, 'Switch to', key.Enter };
        actions[1] := act.CONTEXT{ act.push_key, 'Suspend', key.Plus };
        actions[2] := act.CONTEXT{ act.push_key, 'Resume', key.Minus };
        actions[3] := act.EMPTY_CONTEXT;
      END;
    END;
    IF show THEN eve.AddToTail(hwnd, eve.Rise, 0); END;
  END;
END InitThreadsWindow;


PROCEDURE InitThreadsList (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Threads);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    InitThreadsWindow(TRUE);
  END;
  RETURN TRUE;
END InitThreadsList;

<* END *>


PROCEDURE DoSearch(hwnd: win.HWND): BOOLEAN;
VAR
  hndl: win.WND_PROC;
  msg : eve.MSG;
BEGIN
  std.SetErrorMsgNo(mes.LineNotFound);
  IF win2search = win.Invalid_H THEN RETURN FALSE END;
  found := FALSE;
  xs.Uppercase(SearchStr);
  hndl  := win.GetHandler(win2search);
  WITH msg DO
    ID := eve.Search;
    hwnd := win2search;
    par := 0;
  END;
  hndl(msg.hwnd, msg);
  IF found AND (hwnd # win.Invalid_H) THEN
    eve.AddToTail(hwnd, eve.Hide, 0);
    eve.Flush;
  END;
  RETURN found;
END DoSearch;


PROCEDURE SearchPrev (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  hndl: win.WND_PROC;
  msg : eve.MSG;
  w : win.HWND;
BEGIN
  ASSERT(action = act.FindPrev);
  IF win2search = win.Invalid_H THEN RETURN FALSE; END;
  w := men.SkipMenus();
  IF NOT win.IsSearchable(w) OR (w # win2search) OR (SearchStr='') THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    std.SetErrorMsgNo(mes.LineNotFound);
    found := FALSE;
    hndl  := win.GetHandler(win2search);
    WITH msg DO
      ID := eve.Search;
      hwnd := win2search;
      par := 1;
    END;
    hndl(msg.hwnd, msg);
    IF NOT found THEN
      eve.AddToTail(std.ErrorMsg, eve.Rise, 0);
    END;
  END;
  RETURN TRUE;
END SearchPrev;


PROCEDURE SearchNext (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  w: win.HWND;
BEGIN
  ASSERT(action = act.FindNext);
  IF win2search = win.Invalid_H THEN RETURN FALSE; END;
  w := men.SkipMenus();
  IF NOT win.IsSearchable(w) OR (w # win2search) OR (SearchStr='') THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    IF NOT DoSearch(win.Invalid_H) THEN
      eve.AddToTail(std.ErrorMsg, eve.Rise, 0);
    END;
  END;
  RETURN TRUE;
END SearchNext;


PROCEDURE SearchFirst (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  w : win.HWND;
BEGIN
  ASSERT(action = act.Find);
  IF dv.MainMode = dv.source THEN
    win2search := win.Invalid_H;
    w := men.SkipMenus();
    IF NOT win.IsSearchable(w) THEN RETURN FALSE; END;
    win2search := w;
    IF mode # act.mode_check THEN
      std.OpenUniversalDialog(mes.InputLineNo, DoSearch, SearchStr);
    END;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END SearchFirst;


PROCEDURE Go_Exec (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  result  : BOOLEAN;
  address : kt.ADDRESS;
  access  : kt.ATTRIBS;
BEGIN
  ASSERT(action = act.GotoExec);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  IF mode = act.mode_check THEN
    RETURN mem.GetSegmentInfo (exe.ExecAddr, address, address, access);
  ELSE
    result := mod.SetNewPosByAddr (exe.ExecAddr);
    dv.LocalScope := exe.ExecScope;
    dv.LocalScopeLevel := 0;
    RETURN result;
  END;
END Go_Exec;


<* PUSH *>
<* WOFF301+ *>
PROCEDURE InitMainWindow(show: BOOLEAN);
<* POP *>
BEGIN
  WITH std.Wnds[std.MainWindow] DO
    IF hwnd = win.Invalid_H THEN
      hwnd := win.RegisterWindow(ScrollAreaProc, 0);
      ASSERT(hwnd # win.Invalid_H);
      win.SetMovable(hwnd);
      win.SetResizable(hwnd, TRUE, TRUE);
      win.SetSwitchable(hwnd);
      win.SetSearchable(hwnd);
      win.SetWindowSize(hwnd, size);
      SetHeaderMainWindow;
    END;
  END;
END InitMainWindow;


PROCEDURE MainWindowRise(action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.MainWindow);
  IF NOT kexe.Loaded THEN
    RETURN FALSE;
  END;
  IF mode # act.mode_check THEN
    WITH std.Wnds[std.MainWindow] DO
      ASSERT(hwnd # win.Invalid_H);
      eve.AddToTail(hwnd, eve.Rise, 0);
    END;
  END;
  RETURN TRUE;
END MainWindowRise;


VAR
  AboutDialog: win.HWND;

PROCEDURE About (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  d   : tcn.DateTime;
  p   : std.PPMSG;
  str : xs.String;
BEGIN
  ASSERT(action = act.About);
  IF mode # act.mode_check THEN
    IF AboutDialog = win.Invalid_H THEN
      AboutDialog := win.RegisterWindow (std.MsgProc, SIZE(std.MSG_REC));
      ASSERT(AboutDialog # win.Invalid_H);
      win.SetWindowSize (AboutDialog, crt.SZ {1, 1, 40, 9});
      win.SetModal (AboutDialog);
      win.SetHeader (AboutDialog, mes.AboutHeader);
      p := win.GetAMPtr(AboutDialog);
      p^.Colors := sys.ADR (crt.Info);
      std.InitMessage (AboutDialog, 8);
      tcn.unpack (d, com.TIMESTAMP);
      fmt.print ( str, "%s\n\n(c) %s\n\nVersion %s [build %$2d.%$2d.%4d]"
                , xdt.PRODUCT, xdt.COPYRIGHT, xdt.VERSION, d.day, d.month, d.year);
      std.SetMessage (AboutDialog, str);
      std.CenterWindow (AboutDialog);
      std.CenterMessage (win.GetWindowSize (AboutDialog), p^.Msg);
    END;
    eve.AddToTail(AboutDialog, eve.Rise, 0);
    eve.Flush;
  END;
  RETURN TRUE;
END About;


VAR
  BatchFileName: xs.String;

  file: ioc.ChanId;
  open: BOOLEAN;
  res : seq.OpenResults;


PROCEDURE out (f: ARRAY OF CHAR; SEQ arg: sys.BYTE);
VAR
  s: xs.String;
BEGIN
  fmt.print (s, f, arg);
  tio.WriteString (file, s);
END out;


PROCEDURE CreateFile;

  PROCEDURE WriteAliases (out: dlt.OUT_PROC);
  VAR
    i: CARDINAL;
    name, equ: xs.String;
  BEGIN
    FOR i := 1 TO lst.EquNamesNo() DO
      lst.GetName (i-1, name);
      lst.GetEquName (name, equ);
      out ("  ALIAS %s, S, %s\n", name, equ);
    END;
    out ("\n");
  END WriteAliases;


VAR
  prog_name: xs.String;
BEGIN
  seq.OpenWrite (file, BatchFileName, seq.write+seq.text+seq.old, res);
  open := res = seq.opened;
  IF open THEN
    out ("; %s, Version %s\n", xdt.PRODUCT, xdt.VERSION);
    out ("; (c) %s\n;\n", xdt.COPYRIGHT);
    out ('; XDS Debugger control file.\n');
    out ('; This is an automatically generated file.\n');
    out ('; It can be edited by the user and executed under the debugger.\n\n');
    out ('; Load the program\n\n');
    COPY(opt.prog_name, prog_name);
    fil.RemoveExtension (prog_name);
    out ('  LOAD %s %s\n\n', prog_name, opt.prog_args);
    out ('; Establish breaks\n\n');
    dbrk.WriteBreaks (out);
    out ('; Set watches\n\n');
    dw.WriteWatches (out);
    out ('; Assign aliases\n\n');
    WriteAliases (out);
    out ('; Switch to the dialog mode\n\n');
    out ('  DIALOG\n');
    out ('  QUIT\n\n');
    out ('; Comment two lines above to initiate the program execution\n\n');
    out ('; Initiate program execution\n');
    out ('\n');
    out ('  START\n');
    out ('  QUIT\n');
    out ('\n');
    out ('; Upon the breakpoint hit, switch to the dialog mode\n');
    out ('\n');
    out ('%s\n', dlt.Switch_To_Dialog);
    out ('  DIALOG\n');
    out ('  STOP\n');
    seq.Close (file);
  ELSE
    std.Error ('Error open file');
  END;
END CreateFile;


PROCEDURE SaveAsBatch (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action=act.SaveAsBatch);
  IF mode # act.mode_check THEN
    COPY(opt.prog_name, BatchFileName);
    fil.RemoveExtension (BatchFileName);
    fil.AddExtension (BatchFileName, kt.pkt_file_ext);
    brw.Browse(act.ActionName[act.SaveAsBatch], BatchFileName, CreateFile);
    RETURN TRUE;
  END;
  RETURN TRUE;
END SaveAsBatch;


<* PUSH *>
<* WOFF301+ *>
PROCEDURE RestoreFromBatch (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
<* POP *>
BEGIN
  ASSERT(action=act.RestoreFromBatch);
  RETURN FALSE;
END RestoreFromBatch;


<* IF DEST_XDS THEN *>

<* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME

CONST
  ControlAction = act.ACTION_GROUP{ act.Restart, act.Into, act.Over, act.Skip, act.UptoRet };

PROCEDURE ControlHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  size: crt.SZ;
  s   : xs.String;
  x, y: CARDINAL;
  a   : act.ACTION;
  l   : CARDINAL;
BEGIN
  size := win.GetWindowSize(hwnd);
  CASE msg.ID OF
  | eve.Redraw, eve.Paint:
    crt.FillWindow (hwnd, ' ', crt.PopupWindow[crt.Popup_Background]);
    l := 1;
    FOR a := MIN(act.ACTION) TO MAX(act.ACTION) DO
      IF a IN ControlAction THEN
        crt.SetPos(l, 0);
        fmt.print (s, " %s ", act.Icons[a]);
        crt.WrStr (hwnd, s, crt.PopupWindow[crt.Popup_Line]);
        x := LENGTH(s);
        crt.SetPos(l+x, 0);
        fmt.print (s, "%c", CHR(263C));
        crt.WrStr (hwnd, s, crt.PopupWindow[crt.Popup_Line]);
        INC(l, x+1);
      END;
    END;
    crt.DrawControls (hwnd, size, crt.PopupWindow[crt.Popup_Line], dlt.WND_CTRL_SET{dlt.WinCtrl_Close, dlt.WinCtrl_Move});
    IF msg.ID = eve.Redraw THEN
      crt.UpdateRect (size);
    END;

  | eve.Mouse_Pressed:
    ASSERT(win.GetRelMouse(hwnd, msg, x, y));
    CASE crt.GetControl (size, x, y, dlt.WND_CTRL_SET{dlt.WinCtrl_Close, dlt.WinCtrl_Move}) OF
    | dlt.WinCtrl_Close:
      eve.AddToTail(hwnd, eve.Hide, 0);
    | dlt.WinCtrl_Move:
      std.DefaultProc (hwnd, msg);
    ELSE
      l := 1;
      FOR a := MIN(act.ACTION) TO MAX(act.ACTION) DO
        IF a IN ControlAction THEN
          fmt.print (s, " %s ", act.Icons[a]);
          y := LENGTH(s);
          IF (l <= x) AND (x < l+y) THEN
            IF std.QueryAction (hwnd, a) THEN
              std.DefaultProc (hwnd, eve.Make (hwnd, eve.DoAction, CARDINAL(a)));
              RETURN;
            END;
          END;
          INC(l, y+1);
        END;
      END;
    END;

  | eve.QueryAction:
    IF std.GetAction (msg.par) = act.Controls THEN
      act.ConfirmQuery;
    ELSE
      std.DefaultProc (hwnd, msg);
    END;

  ELSE
    std.DefaultProc (hwnd, msg);
  END;
END ControlHandler;


VAR
  ControlWindow: win.HWND;


PROCEDURE InitControls (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  size: crt.SZ;
BEGIN
  ASSERT(action = act.Controls);
  IF ControlWindow = win.Invalid_H THEN
    ControlWindow := win.RegisterWindow (ControlHandler, 0);
    ASSERT(ControlWindow # win.Invalid_H);
    WITH size DO
      x2 := crt.Xmax-1; y2 := crt.Ymax-1;
      x1 := x2-37;      y1 := y2;
    END;
    win.SetWindowSize (ControlWindow, size);
    win.SetMovable (ControlWindow);
    win.SetSwitchable (ControlWindow);
    win.SetHeaderByStr (ControlWindow, act.ActionName[act.Controls]);
  END;
  IF mode # act.mode_check THEN
    eve.AddToTail(ControlWindow, eve.Rise, 0);
  END;
  RETURN std.QueryAction (ControlWindow, act.Controls);
END InitControls;

<* END *>


VAR
  AliasesWindow: win.HWND;


PROCEDURE AliasesHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p   : std.PLIST;
  size: crt.SZ;

  PROCEDURE init;
  BEGIN
    p := win.GetAMPtr(hwnd);
    size := win.GetWindowSize(hwnd);
    p^.N := lst.EquNamesNo();
  END init;


  PROCEDURE write_line (num: CARDINAL);
  VAR
    buf: xs.String;
    name1: xs.String;
    name2: xs.String;
  BEGIN
    lst.GetName (num, name1);
    lst.GetEquName (name1, name2);
    fmt.print(buf, '%-12s %s', name1, name2);
    WITH p^ DO
      crt.SetPos(2, num-frame + 1);
      crt.WrStrFromPos(hwnd, buf, Colors^[crt.List_Line], pos);
    END;
  END write_line;

VAR
  i, len: CARDINAL;
  last  : CARDINAL;

BEGIN
  init;
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
    std.ListBox(hwnd, msg);

  | eve.QueryAction:
    IF std.GetAction (msg.par) = act.Aliases THEN
      act.ConfirmQueryByCond (p^.N > 0);
    ELSE
      std.ListBox (hwnd, msg);
    END;

  ELSE
    std.ListBox(hwnd, msg);
  END;
END AliasesHandler;



PROCEDURE InitAliases (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  p   : std.PLIST;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.Aliases);
  IF AliasesWindow = win.Invalid_H THEN
    AliasesWindow := win.RegisterWindow (AliasesHandler, SIZE(p^));
    ASSERT(AliasesWindow # win.Invalid_H);
    WITH size DO
      x2 := crt.Xmax-1; y2 := crt.Ymax-1;
      x1 := 0;          y1 := y2-10;
    END;
    win.SetWindowSize (AliasesWindow, size);
    win.SetMovable (AliasesWindow);
    win.SetResizable (AliasesWindow, TRUE, TRUE);
    win.SetSwitchable (AliasesWindow);
    win.SetHeader (AliasesWindow, mes.Aliases);
    p := win.GetAMPtr (AliasesWindow);
    p^ := std.EmptyList;
    WITH p^ DO
      Colors     := sys.ADR(crt.List);
      Frame      := crt.Double;
      locator    := NIL;
      actions[0] := act.EMPTY_CONTEXT;
      ext := sys.ADDADR(sys.ADR(p^), SIZE(p^));
    END;
  END;
  IF mode # act.mode_check THEN
    eve.AddToTail(AliasesWindow, eve.Rise, 0);
  END;
  RETURN std.QueryAction (AliasesWindow, act.Aliases);
END InitAliases;



TYPE
  PLIST_EXT_TYPES = POINTER TO LIST_EXT_TYPES;

  LIST_EXT_TYPES  = RECORD
                      inside: BOOLEAN;
                    END;

VAR
  ModuleTypesWindow: win.HWND;
  TypeAlias: std.MESSAGE;



PROCEDURE ModuleTypesWindowLocator (hwnd: crt.HWND; i: CARDINAL; VAR str: ARRAY OF CHAR);
VAR
  type: dt.PTYPE;
  name: xs.txt_ptr;
BEGIN
  ASSERT(hwnd # win.Invalid_H);
  IF kexe.Loaded AND tls.IsPosValid (mod.Curr^.Pos) THEN
    type := tls.GetType (mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, i);
    ASSERT(tls.TypeName (type, name));
    COPY(name^, str);
  ELSE
    COPY('???', str);
  END;
END ModuleTypesWindowLocator;



PROCEDURE SetTypeAlias (hwnd: win.HWND): BOOLEAN;
VAR
  type: xs.String;
  tmp : xs.String;
  p   : std.PLIST;
  comn: xs.txt_ptr;
  modn: xs.txt_ptr;
BEGIN
  IF NOT exp.CheckName (TypeAlias, TRUE) THEN
    std.SetErrorMsgNo (mes.Incorrect_equiv_name);
    RETURN FALSE;
  END;
  p := win.GetAMPtr (ModuleTypesWindow);
  ModuleTypesWindowLocator (ModuleTypesWindow, p^.curr, type);
  IF kexe.Loaded AND tls.IsPosValid (mod.Curr^.Pos) THEN
    ASSERT(tls.ComName (mod.Curr^.Pos.ComN, comn));
    ASSERT(tls.ModName (mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, modn));
    fmt.print (tmp, "%s.%s.%s", comn^, modn^, type);
    COPY(tmp, type);
  END;
  lst.PutEquName (TypeAlias, type);
  eve.AddToTailIfValid (AliasesWindow, eve.Redraw, 0);
  eve.AddToTail(hwnd, eve.Hide, 0);
  eve.Flush;
  RETURN TRUE;
END SetTypeAlias;



PROCEDURE ModuleTypesHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p   : std.PLIST;
  pext: PLIST_EXT_TYPES;
  size: crt.SZ;
  pos : dt.POS;


  PROCEDURE init;
  BEGIN
    p := win.GetAMPtr(hwnd);
    p^.N := 0;
    pext := PLIST_EXT_TYPES(p^.ext);
    size := win.GetWindowSize(hwnd);
    IF kexe.Loaded THEN
      pos := mod.Curr^.Pos;
      IF tls.IsPosValid (pos) THEN
        p^.N := tls.TypesNo (pos.ComN, pos.ModN);
      END;
    END;
    IF (p^.N <= p^.curr) OR (p^.N <= p^.frame) THEN
      p^.frame := 0;
      p^.curr  := 0;
    END;
  END init;


VAR
  type : dt.PTYPE;
  name : xs.txt_ptr;
  tag  : dt.TYPE_TAG;
  ident: xs.String;


  PROCEDURE TypeInfo (i: CARDINAL);
  BEGIN
    type := tls.GetType (pos.ComN, pos.ModN, i);
    ASSERT(tls.TypeTag (type, tag));
    ASSERT(tls.TypeName (type, name));
    tls.TypeTagName (tag, ident);
  END TypeInfo;


  PROCEDURE write_line (num: CARDINAL);
  VAR
    buf: xs.String;
  BEGIN
    TypeInfo (num);
    fmt.print(buf, '%-32s %s' , name^, ident);
    WITH p^ DO
      crt.SetPos(2, num-frame + 1);
      crt.WrStrFromPos(hwnd, buf, Colors^[crt.List_Line], pos);
    END;
  END write_line;

VAR
  i, len: CARDINAL;
  last  : CARDINAL;
  main  : BOOLEAN;
  item  : CARDINAL;

BEGIN
  init;
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
    std.ListBox(hwnd, msg);

  | eve.QueryAction:
    IF std.GetAction (msg.par) = act.ModuleTypes THEN
      act.ConfirmQueryByCond (kexe.Loaded);
--      act.ConfirmQueryByCond (kexe.Loaded AND (p^.N > 0));
    ELSE
      std.ListBox (hwnd, msg);
    END;

  | eve.QueryItem:
    std.GetItem (msg.par, main, item);
    ASSERT(main);
    act.ConfirmQueryByCond (kexe.Loaded AND (p^.N > 0) AND (item = 0));

  | eve.ContextItem:
    std.GetItem (msg.par, main, item);
    ASSERT(main);
    ASSERT(kexe.Loaded AND (p^.N > 0) AND (item = 0));
    TypeInfo (p^.curr);
    COPY(name^, TypeAlias);
    std.OpenUniversalDialog (mes.dlg_EnterTypeAlias, SetTypeAlias, TypeAlias);

  ELSE
    std.ListBox(hwnd, msg);
  END;
END ModuleTypesHandler;



PROCEDURE InitModuleTypes (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  p   : std.PLIST;
  pext: PLIST_EXT_TYPES;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.ModuleTypes);
  IF NOT kexe.Loaded THEN
    RETURN FALSE;
  END;
  IF ModuleTypesWindow = win.Invalid_H THEN
    ModuleTypesWindow := win.RegisterWindow (ModuleTypesHandler, SIZE(p^)+SIZE(pext^));
    ASSERT(ModuleTypesWindow # win.Invalid_H);
    WITH size DO
      x2 := crt.Xmax-1; y2 := crt.Ymax-1;
      x1 := 0;          y1 := y2-10;
    END;
    win.SetWindowSize (ModuleTypesWindow, size);
    win.SetMovable (ModuleTypesWindow);
    win.SetResizable (ModuleTypesWindow, TRUE, TRUE);
    win.SetSwitchable (ModuleTypesWindow);
    win.SetHeader (ModuleTypesWindow, mes.ModuleTypes_);
    p := win.GetAMPtr (ModuleTypesWindow);
    p^ := std.EmptyList;
    WITH p^ DO
      Colors     := sys.ADR(crt.List);
      Frame      := crt.Double;
      locator    := ModuleTypesWindowLocator;
      actions[0] := act.CONTEXT{ act.context_item, "Add to users types" };
      actions[1] := act.EMPTY_CONTEXT;
      ext := sys.ADDADR(sys.ADR(p^), SIZE(p^));
    END;
    pext := PLIST_EXT_TYPES(p^.ext);
    pext^.inside := FALSE;
  END;
  IF mode # act.mode_check THEN
    eve.AddToTail(ModuleTypesWindow, eve.Rise, 0);
  END;
  RETURN std.QueryAction (ModuleTypesWindow, act.ModuleTypes);
END InitModuleTypes;

<* END *>



BEGIN
  StrNum  := '';
  AddrNum := '';
  x_direct := FALSE;
  y_direct := FALSE;
  OpenNewWindowMode  := FALSE; -- Режим открытия нового окна
  OpenNewWindowCheck := FALSE; -- Режим проверки возможности открытия

  std.Wnds[std.MainWindow].init      := InitMainWindow;
  std.Wnds[std.LblWindow].init       := InitLblWindow;
  std.Wnds[std.GlobalVarWindow].init := InitGlobalVarWindow;
  std.Wnds[std.ModuleVarWindow].init := InitVarWindow;
  std.Wnds[std.LocalVarWindow].init  := InitLocalVarWindow;
  std.Wnds[std.WatchWindow].init     := InitWatchWindow;

  win2search := win.Invalid_H;

  CalcDialog        := win.Invalid_H;
  SearchDialog      := win.Invalid_H;
  StructWindow      := win.Invalid_H;
  LoadProgramDialog := win.Invalid_H;
  AboutDialog       := win.Invalid_H;

  DlgExitCode.type := dt.st_original;
  DlgExitCode.sort := exp.WHOLEval;
  DlgExitCode.value := 0;
  ASSERT (exp.AddIdentExprVar ("DLGEXITCODE", sys.ADR(DlgExitCode)));
  ASSERT (exp.AddIdentExprConst ("DLGEXITCODEHALT", exp.ExprRes{ dt.st_original, exp.WHOLEval, ORD(act.Halt) }));
  ASSERT (exp.AddIdentExprConst ("DLGEXITCODEQUIT", exp.ExprRes{ dt.st_original, exp.WHOLEval, ORD(act.Quit) }));
  ASSERT (exp.AddIdentExprConst ("DLGEXITCODERETURN", exp.ExprRes{ dt.st_original, exp.WHOLEval, ORD(act.ReturnToBatch) }));

  act.IniAction(act.Load,             Open);
  act.IniAction(act.Refresh,          Refresh);
  act.IniAction(act.SaveAsBatch,      SaveAsBatch);
  act.IniAction(act.RestoreFromBatch, RestoreFromBatch);
  act.IniAction(act.ReturnToBatch,    ReturnToBatch);
  act.IniAction(act.Quit,             Quit);
  act.IniAction(act.Halt,             Halt);

  act.IniAction(act.SourceMode,   MainWindowStateAction);
  act.IniAction(act.AssemblyMode, MainWindowStateAction);
  act.IniAction(act.MixMode,      MainWindowStateAction);

  act.IniAction(act.GlobalVars, InitGlobalVarList);
  act.IniAction(act.ModuleVars, InitVarsList);
  act.IniAction(act.LocalVars,  InitLocalVarsList);
  act.IniAction(act.Procs,      InitLabelsList);
  act.IniAction(act.Examine,    Variable);
  act.IniAction(act.Evaluate,   CalcExpr);

  act.IniAction(act.AddWatch,      WatchExpr);
  act.IniAction(act.DelWatch,      DelWatch);
  act.IniAction(act.Watches,       InitWatchList);
  act.IniAction(act.DelAllWatches, DelAllWatches);

  act.IniAction(act.Find,     SearchFirst);
  act.IniAction(act.FindNext, SearchNext);
  act.IniAction(act.FindPrev, SearchPrev);
  act.IniAction(act.NextProc, NextLabel);
  act.IniAction(act.PrevProc, PrevLabel);
  act.IniAction(act.GotoExec, Go_Exec);
  act.IniAction(act.GotoLine, StrByNum);
  act.IniAction(act.GotoAddr, StrByAddr);

  act.IniAction(act.MainWindow,   MainWindowRise);

  act.IniAction(act.Help, Help);
  act.IniAction(act.About, About);

 <* IF DEFINED(SCHERN_K26) AND SCHERN_K26 THEN *>
  act.IniAction(act.prf_var0,prf_var0);                   (* SCHERN  *)
  act.IniAction(act.prf_var9,prf_var9);                   (* SCHERN  *)
  act.IniAction(act.prf_var8,prf_var8);                   (* SCHERN  *)
  act.IniAction(act.prf_var7,prf_var7);                   (* SCHERN  *)
  act.IniAction(act.prf_var6,prf_var6);                   (* SCHERN  *)
 <* END *>

 <* IF DEST_XDS THEN *>
  std.Wnds[std.ThreadsWindow].init  := InitThreadsWindow;
  act.IniAction(act.Threads, InitThreadsList);
  AliasesWindow := win.Invalid_H;
  act.IniAction(act.Aliases, InitAliases);
  ModuleTypesWindow := win.Invalid_H;
  act.IniAction(act.ModuleTypes, InitModuleTypes);
 <* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME
  ControlWindow := win.Invalid_H;
  act.IniAction(act.Controls, InitControls);
 <* END *>
 <* END *>
END Dialog.

