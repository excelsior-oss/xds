<* Storage+ *>
IMPLEMENTATION MODULE Dlg_Exec;

IMPORT std := Dlg_Std;

IMPORT sys := SYSTEM;

IMPORT fmt := FormStr;

IMPORT fil := File;
IMPORT exp := Expr;

IMPORT exe := ExeMain;

IMPORT crt := CRT;
IMPORT key := Keys;

IMPORT brk := Breaks;

IMPORT eve := DlgEvent;
IMPORT mod := DlgMods;
IMPORT win := Dlg_Win;
IMPORT dv  := Dlg_Vars;
IMPORT dw  := DlgWatch;
IMPORT act := Dlg_Acts;
IMPORT dsm := Dlg_Dasm;
IMPORT dmn := Dlg_Menu;

IMPORT kt   := KrnTypes;
IMPORT kexe := KrnExec;
IMPORT kdsm := Krn_Dasm;

IMPORT tls := DI_Tools;
IMPORT dt  := DI_Types;

IMPORT xs  := xStr;
IMPORT mes := MsgNo;
IMPORT pro := Protocol;
IMPORT opt := Options;

IMPORT mem := Exe_Mem;
IMPORT dmm := Dlg_Mem;

IMPORT stk := CallStk;

IMPORT nm  := Names;

IMPORT con := Console;

IMPORT xi  := xdRTS;

<* IF DEST_K26 THEN *>

IMPORT kpr := Krn_Prog;
IMPORT int := IntVMain;

<* ELSIF DEST_XDS THEN *>

IMPORT de  := DlgExcpt;

IMPORT thr := Threads;

<* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME
IMPORT dl  := Dlg_Log;
<* END *>

<* END *>


PROCEDURE ResetCall;
VAR
  p: std.PLIST;
BEGIN
  IF std.Wnds[std.CallWindow].hwnd # win.Invalid_H THEN
    p := win.GetAMPtr(std.Wnds[std.CallWindow].hwnd);
    p^.curr  := 0;
    p^.frame := 0;
  END;
END ResetCall;


PROCEDURE ResetCallStack;
BEGIN
  stk.ResetCallStack;
  ResetCall;
END ResetCallStack;



PROCEDURE ResetLocalVarWindow;
VAR
  p: std.PLIST;
BEGIN
  IF std.Wnds[std.LocalVarWindow].hwnd # win.Invalid_H THEN
    p := win.GetAMPtr(std.Wnds[std.LocalVarWindow].hwnd);
    p^.curr  := 0;
    p^.frame := 0;
  END;
END ResetLocalVarWindow;



PROCEDURE SetLocalScope (curr: CARDINAL; swicth_to_source: BOOLEAN): BOOLEAN;

  PROCEDURE reset_locals (curr: CARDINAL; obj: dt.OBJECT);
  BEGIN
    dv.LocalScopeLevel := curr;
    dv.LocalScope := obj;
    ResetLocalVarWindow;
  END reset_locals;

VAR
  obj   : dt.OBJECT;
  addr  : kt.ADDRESS;
  call  : stk.CALL;
 <* IF DEST_K26 THEN *>
  com_no: dt.ComNo;
  mod_no: dt.ModNo;
  N     : CARDINAL;
 <* END *>

BEGIN
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
 <* IF DEST_K26 THEN *>
  IF curr = 0 THEN
    addr := exe.ExecAddr;
    IF tls.FindModByAddr (addr, com_no, mod_no) THEN
      obj := tls.FindProcByAddr (com_no, mod_no, addr);
    ELSE
      obj := dt.Invalid_Object;
    END;
  ELSE
    N := stk.CallTop();
    INC (N);
    IF N < curr-1 THEN RETURN FALSE; END;
    stk.GetCall (N-curr-1, call);
    WITH call DO
      addr := call_addr;
      obj  := Object;
    END;
  END;
 <* ELSIF DEST_XDS THEN *>
  stk.GetCall (curr, call);
  WITH call DO
    addr := call_addr;
    obj  := Object;
  END;
 <* END *>
  IF swicth_to_source THEN
    IF mod.SetNewPosByAddr (addr) THEN
      reset_locals (curr, obj);
      eve.AddToTail(std.Wnds[std.MainWindow].hwnd, eve.Rise, 0);
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  ELSE
    IF tls.EqualObjects (obj, dt.Invalid_Object) THEN
      RETURN FALSE;
    ELSE
      reset_locals (curr, obj);
      RETURN TRUE;
    END;
  END;
END SetLocalScope;


PROCEDURE GoUpCallStack (): BOOLEAN;
BEGIN
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  RETURN (dv.LocalScopeLevel > 0) AND SetLocalScope (dv.LocalScopeLevel-1, FALSE);
END GoUpCallStack;


PROCEDURE GoDownCallStack (): BOOLEAN;
BEGIN
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  RETURN (dv.LocalScopeLevel+1 < stk.CallTop()) AND SetLocalScope (dv.LocalScopeLevel+1, FALSE);
END GoDownCallStack;



PROCEDURE CallsHandler (hwnd: win.HWND; msg: eve.MSG);

VAR
  p    : std.PLIST;
  buf  : xs.String;

  PROCEDURE write_line(num: CARDINAL);
  VAR
    mname: xs.string;
    call : stk.CALL;
   <* IF DEST_K26 THEN *>
    name : xs.String;
   <* END *>
  BEGIN
    WITH p^ DO
      IF num = 0 THEN
        crt.SetPos (1, num-p^.frame + 1);
        crt.WrChar (hwnd, CHAR(16), crt.Attr(crt.BringColor(crt.Bg(crt.Src[crt.Src_ExecCursor])), crt.Bg(p^.Colors^[crt.List_Line])));
      ELSIF num = dv.LocalScopeLevel THEN
        crt.SetPos (1, num-p^.frame + 1);
        crt.WrChar (hwnd, CHAR(16), crt.Attr(crt.BringColor(crt.Bg(crt.Src[crt.Src_CallCursor])), crt.Bg(p^.Colors^[crt.List_Line])));
      END;
      crt.SetPos(2, num-frame + 1);
     <* IF DEST_XDS THEN *>
      ASSERT(num < stk.CallTop());
      stk.GetCall (num, call);
      WITH call DO
     <* ELSIF DEST_K26 THEN *>
      IF num = 0 THEN
        IF tls.EqualObjects (exe.ExecScope, dt.Invalid_Object) THEN
          fmt.print (buf, exp.Fmt_ADDRval, exe.ExecAddr);
        ELSE
          ASSERT (tls.ModName (com, mod, mname));
          nm.ObjectNameGetAndCorrect (exe.ExecScope, name);
          fmt.print (buf, '%s.%s', mname^, name);
        END;
      ELSE
      stk.GetCall (N-num-1, call);
      WITH call DO
     <* END *>
        IF Name # "" THEN
          IF (com # dt.Invalid_Component) AND (mod # dt.Invalid_Module) THEN
            ASSERT (tls.ModName (com, mod, mname));
            fmt.print (buf, '%s.%s', mname^, Name);
          ELSE
            COPY (Name, buf);
          END;
        ELSE
          fmt.print (buf, exp.Fmt_ADDRval, call_addr);
        END;
      END; -- WITH call ...
     <* IF DEST_K26 THEN *>
      END; -- IF num = 0 ...
     <* END *>
      crt.WrStrFromPos(hwnd, buf, Colors^[crt.List_Line], pos);
    END;
  END write_line;

VAR
  size  : crt.SZ;
  i, len: CARDINAL;
  last  : CARDINAL;
  zzz   : xs.String;

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
        N := stk.CallTop();
       <* IF DEST_K26 THEN *>
        INC(N);
       <* END *>
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
        pro.GetMsg(mes.CallStack, zzz);
        fmt.print(zzz, '%s: %u', zzz, N);
        win.SetHeaderByStr(hwnd, zzz);
      ELSE
        win.SetHeader(hwnd, mes.CallStack);
      END;
    END;
    std.ListBox (hwnd, msg);

  | eve.KbHit:
    WITH p^ DO
      CASE msg.par OF
      | key.Enter:
        IF N = 0 THEN RETURN END;
        IF NOT SetLocalScope (curr, TRUE) THEN
          crt.Beep;
        END;
      ELSE
        std.ListBox (hwnd, msg);
      END;
    END;
  ELSE
    std.ListBox (hwnd, msg);
  END;
END CallsHandler;


PROCEDURE InitCallWindow(show: BOOLEAN);
VAR
  p: std.PLIST;
BEGIN
  WITH std.Wnds[std.CallWindow] DO
    IF hwnd = win.Invalid_H THEN
      hwnd := win.RegisterWindow(CallsHandler,SIZE(std.LIST));
      ASSERT(hwnd # win.Invalid_H);
      win.SetHeader(hwnd, mes.CallStack);
      win.SetMovable(hwnd);
      win.SetResizable(hwnd, TRUE, TRUE);
      win.SetSwitchable(hwnd);
      win.SetWindowSize(hwnd, size);
      p  := win.GetAMPtr(hwnd);
      p^ := std.EmptyList;
      WITH p^ DO
        Colors     := sys.ADR(crt.List);
        actions[0] := act.CONTEXT{ act.push_key, 'Go to', key.Enter };
        actions[1] := act.CONTEXT{ act.do_action, act.OneTime };
        actions[2] := act.EMPTY_CONTEXT;
        curr  := 0;
        frame := 0;
      END;
    END;
    IF show THEN eve.AddToTail(hwnd, eve.Rise, 0); END;
  END;
END InitCallWindow;


PROCEDURE InitCallList (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  p: std.PLIST;
 <* IF DEST_XDS THEN *>
  N: CARDINAL;
 <* END *>
BEGIN
  ASSERT(action = act.CallStack);
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    InitCallWindow(TRUE);
    p := win.GetAMPtr(std.Wnds[std.CallWindow].hwnd);
   <* IF DEST_XDS THEN *>
    N := stk.CallTop();
    IF N = 0 THEN
      stk.ScanCallStack;
    END;
    IF (N <= p^.curr) OR (N <= p^.frame) THEN
      p^.curr  := 0;
      p^.frame := 0;
    END;
   <* ELSIF DEST_K26 THEN *>
    p^.curr  := 0;
    p^.frame := 0;
   <* END *>
  END;
  RETURN TRUE;
END InitCallList;


CONST
  Invalid_Name = '????????';

PROCEDURE PosExec (addr: kt.ADDRESS): BOOLEAN;
BEGIN
  IF NOT mod.SetNewPosByAddr(addr) THEN RETURN FALSE; END;
  PointExecLine(addr);
  RETURN TRUE;
END PosExec;


PROCEDURE PointExecLine(PC: kt.ADDRESS);
VAR
  scope: dt.OBJECT;
BEGIN
 <* IF DEST_XDS THEN *>
  ResetCallStack;
  WITH std.Wnds[std.CallWindow] DO
    IF (hwnd # win.Invalid_H) AND (win.Visible(hwnd)) THEN
      stk.ScanCallStack;
    END;
  END;
 <* END *>
  IF kexe.ProgramContextOk THEN
    exe.RemoveTmpBreakPoint();
  END;
  scope := exe.ExecScope;
  IF mod.SetNewPosByAddr(PC) THEN
    exe.ExecAddr  := PC;
    exe.ExecMod   := mod.Curr^.Pos.ModN;
    exe.ExecComp  := mod.Curr^.Pos.ComN;
    exe.ExecLine  := mod.Curr^.curr;
    exe.ExecScope := tls.FindProcByAddr(exe.ExecComp, exe.ExecMod, exe.ExecAddr);
  ELSE
    exe.ExecAddr  := 0;
    exe.ExecComp  := dt.Invalid_Component;
    exe.ExecMod   := dt.Invalid_Module;
    exe.ExecLine  := 0;
    exe.ExecScope := dt.Invalid_Object;
    std.ErrorNo(mes.IncorrectIP);
    eve.AddToTail(eve.AllWindows, eve.Redraw, 0);
  END;
  dv.LocalScope := exe.ExecScope;
  dv.LocalScopeLevel := 0;
  IF NOT tls.EqualObjects(scope, exe.ExecScope) THEN
    ResetLocalVarWindow;
  END;
  IF kexe.ProgramContextOk THEN
    dmm.RefreshRegs;
    dmm.ResetStack;
  END;
  -- если в окне Watch были локальные переменные, то нужно
  -- сначала обновить стек вызовов, что уже будет сделано выше
  dw.RecalcWatches;
END PointExecLine;


PROCEDURE SetRTShandler;
VAR
  i   : CARDINAL;
  com : dt.ComNo;
  addr: kt.ADDRESS;
BEGIN
  FOR i := 0 TO HIGH(Traps) DO
    WITH Traps[i] DO
      IF Enabled THEN
        Ready := tls.FindPublicByName (Name, com, addr) AND exe.SetBreakPoint (addr, Inx);
      END;
    END;
  END;
END SetRTShandler;


PROCEDURE RemoveRTShandler;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO HIGH(Traps) DO
    WITH Traps[i] DO
      IF Ready THEN
        ASSERT(exe.RemoveBreakPoint(Inx));
        Ready := FALSE;
      END;
    END;
  END;
END RemoveRTShandler;


PROCEDURE CheckTrap (no, inx: CARDINAL): BOOLEAN;
BEGIN
  ASSERT (no <= HIGH(Traps));
  RETURN Traps[no].Ready AND (Traps [no].Inx = inx);
END CheckTrap;



PROCEDURE Load(filename-: ARRAY OF CHAR) : BOOLEAN;
VAR
  rc  : CARDINAL;
  tmp : BOOLEAN;
BEGIN
  kexe.Loaded := FALSE;
  tmp := kexe.Loaded;
  ExecKind := UpToAddr;
  rc := kexe.LoadProgram(filename, opt.prog_args);
  ExecKind := Stopped;
  SetRTShandler;
 <* IF DEST_XDS THEN *>
 <* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME
  dl.SaveToLog (act.Load);
 <* END *>
 <* END *>
  IF rc = 0 THEN
    IF NOT tmp THEN
      COPY(filename, dv.PrimeFile);
    END;
    RETURN TRUE;
  ELSIF (rc = mes.WrongDebugInfo) OR (rc = mes.ErrorCheckDebugInfo) THEN
    IF NOT tmp THEN
      COPY(filename, dv.PrimeFile);
    END;
    std.ErrorNo (rc);
    RETURN TRUE;
  ELSE
    std.ErrorNo (rc);
    RETURN FALSE;
  END;
END Load;


<* IF DEST_K26 THEN *>

VAR
  RestartAddrStr: std.MESSAGE;

PROCEDURE RestartAtAddr (hwnd: win.HWND): BOOLEAN;
VAR
  addr: kt.ADDRESS;
BEGIN
  IF NOT exp.GetAddress (mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, RestartAddrStr, addr) THEN
    std.SetErrorNo (exp.error);
    RETURN FALSE;
  END;
  ResetCallStack;
  dv.SetMainMode(dv.source);
  dmm.ClearRegsState;
  IF NOT int.ChangeProgramEntryPoint (addr) THEN
    std.SetErrorMsgNo (mes.IncorrectEntryPoint);
    RETURN FALSE;
  END;
  kexe.ProgramContextOk := TRUE;
  ExecKind := Stopped;
  PointExecLine(mem.GetIP());
  eve.AddToTail(hwnd, eve.Hide, 0);
  eve.Flush;
  RETURN TRUE;
END RestartAtAddr;


PROCEDURE Restart (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  IF NOT kexe.Loaded THEN RETURN FALSE; END;
  CASE action OF
  | act.Restart:
    IF mode # act.mode_check THEN
      RemoveRTShandler;
      kexe.ProgramContextOk := TRUE;
      ResetCallStack;
      dv.SetMainMode(dv.source);
      dmm.ClearRegsState;
      IF NOT kpr.RestartProgram() THEN
        crt.Beep;
        RETURN FALSE;
      END;
      exe.TurnStepModeOff;
      -- Иначе не сработает реакция (см. Load)
      ExecKind := UpToAddr;
      -- а теперь нужно выполнить программу, иначе
      -- мы не уйдем с точки входа после пускалки
      exe.JumpToMainEntry := TRUE;    -- остановиться, если есть отладочная информация
      exe.JumpToProgramEntry := TRUE; -- остановиться, если есть точка входа
      exe.SkipToMain;
      ExecKind := Stopped;
      PointExecLine(mem.GetIP());
      SetRTShandler;
    END;
  | act.RestartAtAddress:
    IF mode # act.mode_check THEN
      std.OpenUniversalDialog (mes.dlg_EnterRestartAddr, RestartAtAddr, RestartAddrStr);
    END;
  END;
  RETURN TRUE;
END Restart;

PROCEDURE DlgNeedStep(): BOOLEAN;
BEGIN
  CASE ExecKind OF
  | InTo, Step, Trace, SeekingforRet:
    RETURN TRUE;
  ELSE
    RETURN brk.NeedToStepMode();
  END;
END DlgNeedStep;

<* ELSIF DEST_XDS THEN *>


PROCEDURE JumpToMain (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;

  MODULE GuardOptions;

  IMPORT opt;

  VAR
    SaveStopImmediately : BOOLEAN;

  BEGIN
    SaveStopImmediately := opt.StopImmediately;
  FINALLY
    opt.StopImmediately := SaveStopImmediately;
  END GuardOptions;

VAR
  res: BOOLEAN;
BEGIN
  ASSERT (action = act.JumpToMain);
  res := kexe.Loaded AND (xi.XDInterfacePresent () OR kexe.ProgramMainEntryFound ()) AND exe.JumpToMainEntry;
  IF (mode # act.mode_check) AND res THEN
    ExecKind:= UpToAddr;
    opt.StopImmediately := FALSE;
    exe.SkipToMain;
    ExecKind:= Stopped;
    PointExecLine (mem.GetIP ());
  END;
  RETURN res;
END JumpToMain;


PROCEDURE Restart (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;

  MODULE GuardOptions;

  IMPORT opt;

  VAR
    SaveStopImmediately : BOOLEAN;

  BEGIN
    SaveStopImmediately := opt.StopImmediately;
  FINALLY
    opt.StopImmediately := SaveStopImmediately;
  END GuardOptions;
  

VAR 
  MainPassed: BOOLEAN;
BEGIN
  MainPassed :=  FALSE;
  opt.StopImmediately := FALSE;
  IF NOT kexe.Loaded THEN
    RETURN FALSE;
  END;
  IF mode = act.mode_check THEN
    CASE action OF
    | act.Restart:
      -- закомментарено, поскольку тогда действие Restart недоступно без отладочной информации
      -- RETURN xi.XDInterfacePresent () OR kexe.ProgramMainEntryFound ();
      RETURN TRUE;
    | act.RestartAtEntryPoint:
      RETURN kexe.ProgramStartupEntryFound ();
    | act.RestartAtStartup:
      RETURN TRUE;
    END;
  ELSE
    LOOP
      CASE action OF
      | act.Restart:
        IF xi.XDInterfacePresent () OR kexe.ProgramMainEntryFound () THEN
          exe.JumpToMainEntry := TRUE;
          exe.JumpToProgramEntry := FALSE;
          MainPassed := TRUE;
          EXIT;
        ELSE
          action := act.RestartAtEntryPoint;
        END;
      | act.RestartAtEntryPoint:
        IF kexe.ProgramStartupEntryFound () THEN
          exe.JumpToMainEntry := FALSE;
          exe.JumpToProgramEntry  := TRUE;
          EXIT;
        ELSE
          action := act.RestartAtStartup;
        END;
      | act.RestartAtStartup:
        exe.JumpToMainEntry := FALSE;
        exe.JumpToProgramEntry  := FALSE;
        EXIT;
      END;
    END;
    ResetCallStack;
    thr.ClearThreads;
    dv.SetMainMode(dv.source);
    ExecKind := UpToAddr;
    brk.Clear_AccessBreak;
    RemoveRTShandler;
    de.ClearExceptionsHistory ();
    IF NOT exe.Restart() THEN
      crt.Beep;
      ExecKind := Stopped;
      RETURN FALSE;
    END;
    IF NOT MainPassed THEN
      exe.JumpToMainEntry := TRUE;
    END;
    ExecKind := Stopped;
    PointExecLine (mem.GetIP());
    brk.Refresh_AccessBreak;
    brk.Refresh_Breakpoints;
    SetRTShandler;
    eve.AddToTail (std.Wnds[std.MainWindow].hwnd, eve.Rise, 0);
   <* IF DEST_XDS THEN *>
   <* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME
    dl.SaveToLog (action);
   <* END *>
   <* END *>
    RETURN TRUE;
  END;
END Restart;

PROCEDURE DlgNeedStep(): BOOLEAN;
BEGIN
  CASE ExecKind OF
  | InTo, Trace, SeekingforRet:
    RETURN TRUE;
  ELSE
    RETURN brk.NeedToStepMode();
  END;
END DlgNeedStep;

<* END *>

CONST
  MaxCheckers = 16;

TYPE
  PROC_ARR = ARRAY [0..MaxCheckers-1] OF CHECK_PROC;

VAR
  NeedStepProcs: PROC_ARR;


PROCEDURE AddStepCheck(check: CHECK_PROC);
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO HIGH(NeedStepProcs) DO
    IF NeedStepProcs[i] = NIL THEN
      NeedStepProcs[i] := check;
      RETURN;
    END;
  END;
  ASSERT(FALSE);
END AddStepCheck;


VAR
  TmpAddr: kt.ADDRESS;

PROCEDURE DoGo (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;

  MODULE Signal;
  IMPORT ExecKind, Stopped, crt, dmn, con;
  BEGIN
    dmn.DebuggerStateIndicator (TRUE);
    con.Flush ();
  FINALLY
    IF ExecKind # Stopped THEN
      crt.Beep;
    END;
    dmn.DebuggerStateIndicator (FALSE);
  END Signal;

VAR
  i: CARDINAL;
  pc: CARDINAL;
  len: CARDINAL;

 <* IF DEST_XDS THEN *>
  Start, ReturnPoint: CARDINAL;
  tmp: xs.String;
 <* END *>

BEGIN
  IF NOT (kexe.ProgramContextOk AND kexe.Loaded) THEN
    IF mode # act.mode_check THEN crt.Beep; END;
    RETURN FALSE;
  END;
  IF mode # act.mode_check THEN
    exe.TurnStepModeOff;
    dmm.ClearRegsState;
    IF ProcBeginIndex # 0 THEN
      ASSERT(exe.RemoveBreakPoint(ProcBeginIndex));
      ProcBeginIndex := 0;
    END;
  END;
  CASE action OF
  | act.Run:
    IF mode = act.mode_check THEN RETURN TRUE; END;
    ExecKind := GO;

  | act.Into:
    IF mode = act.mode_check THEN RETURN TRUE; END;
    WasCall := FALSE;
    IF opt.SkipDisasm THEN
      pc := mem.GetIP();
      len := kdsm.IsCall(pc);
      IF len # 0 THEN
        WasCall       := TRUE;
        AddrAfterCall := pc+len;
      END;
    END;
    ExecKind := InTo;

  | act.Over:
    IF mode = act.mode_check THEN RETURN TRUE; END;
    dv.N_Call := stk.CallTop();
   <* IF DEST_XDS THEN *>
    pc := mem.GetIP();
    len := kdsm.IsCall(pc);
    IF len # 0 THEN
      StepSP := mem.GetSP();
      IF NOT exe.SetTmpBreakPoint(pc + len) THEN RETURN FALSE; END;
    ELSE
      exe.TurnStepModeOn;
    END;
   <* END *>
    ExecKind := Step;

<* IF DEST_K26 THEN *>
  | act.UptoCall:
    IF mode = act.mode_check THEN RETURN TRUE; END;
    ExecKind := UpToCall;

  | act.UptoRet:
    IF mode = act.mode_check THEN RETURN TRUE; END;
    dv.N_Call := stk.CallTop();
    ExecKind := UpToReturn;

<* END *>

<* IF DEST_XDS THEN *>
  | act.UptoRet:
      IF tls.IsObjectValid(exe.ExecScope) THEN
        IF mode = act.mode_check THEN RETURN TRUE; END;
        ExecKind := UpToReturn;
        tls.ProcAttr (exe.ExecScope, Start, ReturnPoint);
        ASSERT(tls.ObjectAddr(exe.ExecScope, Start));

        ASSERT(tls.ObjectSize(exe.ExecScope, len));
        pc := ReturnPoint;
        LOOP
          IF kdsm.IsRet (pc) THEN
            ReturnPoint := pc;
            EXIT;
          END;
          IF NOT kdsm.Disasm (pc, FALSE, tmp, tmp, i) THEN
            EXIT;
          END;
          INC (pc, i);
          IF pc >= Start+len THEN
            EXIT;
          END;
        END;

        IF exe.ExecAddr < ReturnPoint THEN
          IF NOT exe.SetTmpBreakPoint(ReturnPoint) THEN RETURN FALSE; END;
          IF Start = exe.ExecAddr THEN
            UptoRetCount := 0;
          ELSE
            UptoRetCount := 1;
          END;
          IF NOT exe.SetBreakPoint(Start, ProcBeginIndex) THEN RETURN FALSE; END;
          exe.Go;
        END;
        pc := mem.GetIP();
        IF (pc=Start) OR (pc=ReturnPoint) THEN
          IF NOT DoGo (act.Into, act.mode_loud) THEN RETURN FALSE; END;
        ELSE
          ExecKind := Stopped;
        END;
        RETURN TRUE;
      ELSE
        IF mode = act.mode_check THEN RETURN TRUE; END;
        ExecKind := SeekingforRet;
      END;

  | act.UptoEpilog:
      IF NOT tls.IsObjectValid(exe.ExecScope) THEN RETURN FALSE; END;
      IF mode = act.mode_check THEN RETURN TRUE; END;
      tls.ProcAttr(exe.ExecScope, Start, ReturnPoint);
      IF exe.ExecAddr >= ReturnPoint THEN RETURN FALSE; END;
      ExecKind := UpToReturn;
      ASSERT(tls.ObjectAddr(exe.ExecScope, Start));
      IF NOT exe.SetTmpBreakPoint(ReturnPoint) THEN RETURN FALSE; END;
      IF Start = exe.ExecAddr THEN
        UptoRetCount := 0;
      ELSE
        UptoRetCount := 1;
      END;
      IF NOT exe.SetBreakPoint(Start, ProcBeginIndex) THEN RETURN FALSE; END;

<* END *>


  | act.Animation:
    IF mode = act.mode_check THEN RETURN TRUE; END;
    WasCall := FALSE;
    IF opt.SkipDisasm THEN
      pc := mem.GetIP();
      len := kdsm.IsCall(pc);
      IF len # 0 THEN
        WasCall       := TRUE;
        AddrAfterCall := pc+len;
      END;
    END;
    ExecKind := Trace;
    eve.Flush;

  | act.Skip :
    IF dv.MainMode = dv.source THEN
      WITH mod.Curr^ DO
        IF NOT tls.AddrBySource(Pos.ComN, Pos.ModN, curr+1, pc) THEN
          IF mode # act.mode_check THEN crt.Beep; END;
          RETURN FALSE;
        END;
      END;
    ELSE
      IF NOT dsm.GetAddr(dsm.D_curr, pc) THEN
        IF mode # act.mode_check THEN crt.Beep; END;
        RETURN FALSE;
      END;
    END;
    IF mode = act.mode_check THEN RETURN TRUE; END;
    TmpAddr  := pc;
    ExecKind := UpToCurs;
    ASSERT(exe.SetTmpBreakPoint(TmpAddr));

  | act.UptoAddr:
    IF mode = act.mode_check THEN RETURN TRUE; END;
    IF mem.IsAddrFromExecutableSeg(TmpAddr) AND exe.SetTmpBreakPoint(TmpAddr) THEN
      ExecKind := UpToAddr;
    ELSE
      crt.Beep;
      RETURN FALSE;
    END;
  ELSE
    ASSERT(FALSE);
  END;

 <* WOFF312+ *>
  LOOP
    FOR i := 0 TO HIGH(NeedStepProcs) DO
      IF NeedStepProcs[i] # NIL THEN
        IF NeedStepProcs[i]() THEN
          exe.TurnStepModeOn;
          EXIT;
        END;
      ELSE
        EXIT
      END;
    END;
    EXIT;
  END;
 <* WOFF312- *>
 <* IF DEST_XDS THEN *>
 <* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME
  dl.SaveToLog (action);
 <* END *>
 <* END *>
  exe.Go;
  ExecKind := Stopped;
  RETURN TRUE;
END DoGo;

VAR
  AddrStr: std.MESSAGE;

PROCEDURE DoUptoAddr(hwnd: win.HWND): BOOLEAN;
VAR
  addr: kt.ADDRESS;
BEGIN
  IF NOT exp.GetAddress(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, AddrStr, addr) THEN
    std.SetErrorNo(exp.error);
    RETURN FALSE;
  END;
  IF (NOT exp.dfn) THEN
    std.SetErrorMsgNo(mes.Undefined_Expression);
    RETURN FALSE;
  END;
  TmpAddr := addr;
  IF NOT DoGo (act.UptoAddr, act.mode_loud) THEN
    std.SetErrorMsgNo(mes.Cant_resolve);
    RETURN FALSE;
  END;
  IF ExecKind = Stopped THEN
    eve.AddToTail(hwnd, eve.Hide, 0);
    eve.Flush;
    RETURN TRUE;
  ELSE
    ExecKind := Stopped;
    RETURN FALSE;
  END;
END DoUptoAddr;


PROCEDURE UptoAddr (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.UptoAddr);
  IF NOT kexe.ProgramContextOk OR NOT kexe.Loaded THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    std.OpenUniversalDialog(mes.dlg_EnterUptoAddr, DoUptoAddr, AddrStr);
  END;
  RETURN TRUE;
END UptoAddr;

VAR i:INTEGER;
BEGIN
  std.Wnds[std.CallWindow].init := InitCallWindow;
  TmpAddr := 0;
  ExecKind := Stopped;

  NeedStepProcs := PROC_ARR {NIL BY MaxCheckers};
  AddStepCheck (DlgNeedStep);

 <* IF DEST_K26 THEN *>
  act.IniAction(act.UptoCall,            DoGo);
  act.IniAction(act.RestartAtAddress,    Restart);
 <* ELSE *>
  act.IniAction(act.UptoEpilog,          DoGo);
  act.IniAction(act.RestartAtStartup,    Restart);
  act.IniAction(act.RestartAtEntryPoint, Restart);
  act.IniAction(act.JumpToMain,          JumpToMain);
 <* END *>

  act.IniAction(act.CallStack,           InitCallList);
  act.IniAction(act.Run,                 DoGo);
  act.IniAction(act.Into,                DoGo);
  act.IniAction(act.Over,                DoGo);
  act.IniAction(act.Animation,           DoGo);
  act.IniAction(act.Skip,                DoGo);
  act.IniAction(act.UptoRet,             DoGo);
  act.IniAction(act.UptoAddr,            UptoAddr);
  act.IniAction(act.Restart,             Restart);

  FOR i := LongjmpNo TO LAST_EXCEPTION DO
    Traps[i].Ready   := FALSE;
    Traps[i].Enabled := TRUE;
    Traps[i].Inx     := MAX(CARDINAL);
  END;

  Traps[LongjmpNo].Name  := "X2C_longjmp";
  Traps[TrapFException].Name  := "X2C_TRAP_F";
  Traps[TrapGException].Name  := "X2C_TRAP_G";
  Traps[AssertFException].Name  := "X2C_ASSERT_F";
  Traps[AbortException].Name  := "X2C_ABORT";
  Traps[JavaLangRaisedException].Name    := "X2J_THROW";
  Traps[JavaLangUncaughtException].Name  := "java_lang_ThreadGroup@uncaughtException";
  Traps[JavaLangRedundantMethod].Name    := "X2J_RedundantMethod";
  Traps[JavaLangUnsatisfiedLink].Name    := "X2J_UnsatisfiedLink";
  Traps[AssertFCException].Name  := "X2C_ASSERT_FC";
  Traps[TrapRangeException].Name  := "X2C_TRAP_RANGE";
  Traps[TrapNilException].Name  := "X2C_TRAP_NIL";
  Traps[TrapOverflException].Name  := "X2C_TRAP_OVERFL";
  Traps[TrapDivException].Name  := "X2C_TRAP_DIV";
  Traps[TrapIndexException].Name  := "X2C_TRAP_INDEX";

  Traps[Trap_X2C_TRAP_FC      ].Name  := "X2C_TRAP_FC";
  Traps[Trap_X2C_TRAP_OVERFL_C].Name  := "X2C_TRAP_OVERFL_C";
  Traps[Trap_X2C_TRAP_RANGE_C ].Name  := "X2C_TRAP_RANGE_C";
  Traps[Trap_X2C_TRAP_INDEX_C ].Name  := "X2C_TRAP_INDEX_C";
  Traps[Trap_X2C_TRAP_NIL_C   ].Name  := "X2C_TRAP_NIL_C";
  Traps[Trap_X2C_TRAP_DIV_C   ].Name  := "X2C_TRAP_DIV_C";

  Traps[TrapHandler].Name  := "TrapHandler";
END Dlg_Exec.
