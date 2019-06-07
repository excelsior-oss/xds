<* Storage+ *>
IMPLEMENTATION MODULE DlgReact;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT xcp := XCPT_Msg;
IMPORT eve := Events;
IMPORT key := Keys;
IMPORT opt := Options;
IMPORT exp := Expr;
IMPORT brk := Breaks;
IMPORT xs  := xStr;

IMPORT stk := CallStk;

IMPORT erc := ExeReact;
IMPORT exe := ExeMain;
IMPORT mem := Exe_Mem;

FROM ExeReact IMPORT DATA;

IMPORT kexe := KrnExec;
IMPORT kdsm := Krn_Dasm;
IMPORT kmem := Krn_Mem;
IMPORT kt   := KrnTypes;
IMPORT dbg  := Krn_Dbg;
IMPORT ki   := KrnIndex;

IMPORT pro := Protocol;
IMPORT mes := MsgNo;

IMPORT deve:= DlgEvent;
IMPORT win := Dlg_Win;
IMPORT crt := CRT;
IMPORT dex := Dlg_Exec;
IMPORT std := Dlg_Std;
IMPORT dv  := Dlg_Vars;
IMPORT dmm := Dlg_Mem;
IMPORT act := Dlg_Acts;
IMPORT mod := DlgMods;

IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;

IMPORT xi  := xdRTS;
IMPORT rts := XDInterface;

<* IF DEST_XDS THEN *>

IMPORT de  := DlgExcpt;

<* END *>



CONST
  Notify = std.Notify;
  Error  = std.Error;

VAR
  pc: CARDINAL;

<* WOFF301+ *>

<* IF DEST_K26 THEN *>

VAR
  DontStop: BOOLEAN;
  Counter : CARDINAL;
  CallAddr: kt.ADDRESS;

(* Реакция на останов по заданному адресу *)
PROCEDURE DoAddress_VAX (data: DATA);
VAR
  i : CARDINAL;
  res: BOOLEAN;
  NeedToStop: BOOLEAN;
  savemode: dex.EXEC_KIND;
BEGIN
  IF opt.in_dialog THEN
    exe.WasBreakpoint := eve.LastEvent.IP;
    NeedToStop := FALSE;
    IF brk.FindByIndex_Breakpoint (eve.LastEvent.BreakpointInd, i) THEN
      WITH brk.Breakpoints.Breakpoints^[i] DO
        IF Break.Active THEN
          erc.CancelReact;
          CASE Kind OF
          | brk.counter:
            INC(Break.Pass);
            RETURN;
          | brk.watchpoint:
            savemode := dex.ExecKind;
            dex.PointExecLine(eve.LastEvent.IP);
            dex.ExecKind := savemode;
            deve.Flush;
          | brk.normal:
            IF Break.Pass = 0 THEN
              IF dex.ExecKind # dex.InTo THEN
                NeedToStop := TRUE;
              END;
              IF NeedToStop AND (Condition # NIL) THEN
                NeedToStop := exp.GetRelation (Pos.ComN, Pos.ModN, Condition^, res) AND res;
              END;
              IF NeedToStop AND NOT Break.Sticky THEN
                ASSERT(brk.Delete_Breakpoint(i));
              END;
            ELSE
              DEC(Break.Pass);
            END;
          END;
        END;
      END;
    ELSE
      IF (dex.ExecKind = dex.UpToAddr) OR (dex.ExecKind = dex.UpToCurs) THEN
        IF eve.LastEvent.BreakpointInd = exe.TmpBreakpointIndex THEN
          NeedToStop := TRUE;
        END;
      END;
    END;
    IF NeedToStop THEN
      dex.PointExecLine(eve.LastEvent.IP);
      Counter  := 0;
      DontStop := FALSE;
      CallAddr := 0;
      exe.StopExec;
      erc.CancelReact;
    END;
  END;
END DoAddress_VAX;

PROCEDURE DoStep_VAX (data: DATA);
VAR
  com, m,line: CARDINAL;
  pos : CARDINAL;
  NeedToStop: BOOLEAN;
  buf : ARRAY [0..74] OF CHAR;
  fmt_str: ARRAY [0..79] OF CHAR;
BEGIN
  IF opt.in_dialog THEN
    erc.CancelReact;
    NeedToStop := FALSE;
    IF brk.ConditionBreaks.free > 0 THEN
      pc := mem.GetIP();
      IF NOT tls.FindModByAddr(pc, com, m) THEN com := 0; m := 0; END;
      pos := 0;
      WHILE brk.NeedToStop(com, m, pos) DO
        pro.GetMsg(mes.BreakNotification, fmt_str);
        fmt.print(buf, fmt_str, brk.ConditionBreaks.ConditionBreaks^[pos].Expr^);
        Notify(buf);
        NeedToStop := TRUE;
        INC(pos);
      END;
      IF NeedToStop THEN
        dex.PointExecLine(pc);
      END;
    END;
    CASE dex.ExecKind OF
    | dex.Trace:
      IF key.KeyPressed() THEN
        NeedToStop := key.GetKey() = key.Esc;
      END;
      pc := mem.GetIP();
      IF (dv.MainMode = dv.source) &
         (tls.SourceByAddr(pc, com, m, line) & (exe.ExecComp = com) AND (exe.ExecMod = m) AND (exe.ExecLine = line-1)) OR
         DontStop THEN
         RETURN;
      END;
      dex.PointExecLine(pc);
      dex.ExecKind := dex.Trace;
      deve.Flush;
      dmm.ClearRegsState;
      key.Delay(dv.Delay);
    | dex.InTo, dex.Step:
      pc := mem.GetIP();
      IF NOT NeedToStop THEN
        IF (dv.MainMode = dv.source) THEN
          IF tls.SourceByAddr(pc, com, m, line) THEN
            IF ((exe.ExecComp # com) OR (exe.ExecMod # m) OR (exe.ExecLine # line-1)) THEN
              NeedToStop := TRUE;
            END;
          ELSE
            NeedToStop := ~DontStop;
          END;
        ELSE
          NeedToStop := TRUE;
        END;
        IF (dex.ExecKind = dex.Step) AND NeedToStop THEN
          NeedToStop := (dv.N_Call >= stk.CallTop())
        END;
      END;
    ELSE
    END;
    IF NeedToStop THEN
      exe.TurnStepModeOff;
      dex.ExecKind := dex.Stopped;
      pc := mem.GetIP();
      dex.PointExecLine(pc);
      exe.StopExec;
    END;
  END;
END DoStep_VAX;

PROCEDURE DoException_VAX(data: DATA);
VAR
  buf, zzz, xcpt_msg: xs.String;
  notify: BOOLEAN;
BEGIN
  IF opt.in_dialog THEN
    pc := eve.LastEvent.IP;
    kexe.ProgramContextOk := FALSE;
    dex.PointExecLine(pc);
    exe.StopExec;
    exe.StopQueueProc;
    notify := FALSE;
    CASE eve.LastEvent.Exception_ID OF
    | eve.OutOfMemory:
      pro.GetMsg(mes.BreakOutMem, zzz);
      fmt.print(buf, zzz, CARDINAL(eve.LastEvent.XCPT_INFO_1));
    | eve.WriteProtected:
      pro.GetMsg(mes.BreakWrProt, zzz);
      xcp.Get_XCPT_Msg(CARDINAL(eve.LastEvent.XCPT_INFO_4), 0, xcpt_msg);
      fmt.print(buf, zzz, xcpt_msg, CARDINAL(eve.LastEvent.XCPT_INFO_1));
    | eve.UserException:
      kexe.ProgramContextOk := TRUE;
      --exe.TurnStopModeOff;
      pro.GetMsg(mes.User_React_Default, buf);
      Notify(buf);
      exe.StopExec;
      RETURN;
    | eve.ProgramException:
      WITH eve.LastEvent DO
        IF CARDINAL(XCPT_INFO_1) = 0 THEN
          IF exe.ProgramSkipToMainEntry THEN
            pro.GetMsg (mes.ExceptionAtStartup, buf);
          ELSE
            pro.GetMsg (mes.ProgramFinished, buf);
          END;
          notify := TRUE;
        ELSE
          pro.GetMsg (mes.BreakProgInt, zzz);
          xcp.Get_XCPT_Msg (CARDINAL(eve.LastEvent.XCPT_INFO_1), CARDINAL(eve.LastEvent.XCPT_INFO_4), xcpt_msg);
          fmt.print (buf, zzz, xcpt_msg, CARDINAL(eve.LastEvent.XCPT_INFO_2));
        END;
      END;
    END;
    IF notify THEN
      Notify(buf);
    ELSE
      Error(buf);
    END;
    exe.StopExec;
    exe.StopQueueProc;
    eve.ClearQueue;
    erc.CancelReact;
  END;
END DoException_VAX;

<* ELSE *>



VAR
  AfterJavaExceptionStop: BOOLEAN;

TYPE
  JESTATE = ( jes_Examine
            , jes_Continue
            , jes_ShowObject
            );

  JESTATES = SET OF JESTATE;

  PJEXPT = POINTER TO RECORD
             message : xs.STRING;
             obj_expr: xs.STRING;
             state   : JESTATE;
           END;

  JEBUTTON = RECORD
               x: CARDINAL;
               s: xs.String;
             END;

  JEBUTTONS = ARRAY JESTATE OF JEBUTTON;


CONST
  JEButtons = JEBUTTONS { JEBUTTON { 3, '[ Examine ]'}
                        , JEBUTTON {16, '[ Continue ]'}
                        , JEBUTTON {30, '[ Show thrown object ]'}
                        };


PROCEDURE JavaExceptionHandler (hwnd: win.HWND; msg: deve.MSG);

  MODULE SetRestoreOptions;

  IMPORT opt;

  VAR
    save_opt1: BOOLEAN;
  BEGIN
    save_opt1 := opt.UseSingleStructureWindow;
    opt.UseSingleStructureWindow := FALSE;
  FINALLY
    opt.UseSingleStructureWindow := save_opt1;
  END SetRestoreOptions;


VAR
  size : crt.SZ;
  x, y : CARDINAL;
  attr : crt.ATTR;
  pje  : PJEXPT;
  state: JESTATE;
  bline: CARDINAL;


  PROCEDURE set_attr (draw_state: JESTATE);
  BEGIN
    IF pje^.state = draw_state THEN
      attr := crt.Error[crt.Msg_Button];
    ELSE
      attr := crt.Error[crt.Msg_Frame];
    END;
  END set_attr;


BEGIN
  pje := win.GetAMPtr (hwnd);
  size := win.GetWindowSize(hwnd);
  IF size.x2-size.x1+1 <= LENGTH(pje^.message^) THEN
    size.x1 := 1;
    size.x2 := crt.Xmax-2;
    win.SetWindowSize (hwnd, size);
  END;
  bline := size.y2-size.y1-2;
  CASE msg.ID OF
  | deve.Rise:
    deve.ModalRedraw (hwnd);
    IF win.ActiveWindow # hwnd THEN
      deve.AddToTail(win.ActiveWindow, deve.Redraw, 0);
    END;
    win.Rise (hwnd);
    crt.RiseWindow (hwnd);

  | deve.Redraw, deve.Paint:
    IF size.x2-size.x1+1 <= LENGTH(pje^.message^) THEN
      crt.SetPos (1, 3);
    ELSE
      crt.SetPos (((size.x2-size.x1+1)-LENGTH(pje^.message^)) DIV 2, 3);
    END;
    crt.FillWindow(hwnd, ' ', crt.Error[crt.Msg_Frame]);
    crt.WrStr (hwnd, pje^.message^, crt.Error[crt.Msg_Message]);
    FOR state := MIN(JESTATE) TO MAX(JESTATE) DO
      set_attr (state);
      crt.SetPos (JEButtons[state].x, bline);
      crt.WrStr (hwnd, JEButtons[state].s, attr);
    END;
    crt.DrawFrame(hwnd, size, crt.Double, crt.Error[crt.Msg_Frame]);
    crt.DrawHeader(hwnd, size, crt.Error[crt.Msg_Header]);
    IF msg.ID = deve.Redraw THEN
      crt.UpdateRect(size);
    END;
    deve.CheckWaitForEvent (hwnd, msg.par);

  | deve.KbHit:
    CASE msg.par OF
    | key.Enter:
      deve.AddToTail (hwnd, deve.Hide, 0);
      CASE pje^.state OF
      | jes_Examine:
        AfterJavaExceptionStop := TRUE;
      | jes_Continue:
        AfterJavaExceptionStop := FALSE;
      | jes_ShowObject:
        AfterJavaExceptionStop := TRUE;
        IF pje^.obj_expr^ # "" THEN
          COPY (pje^.obj_expr^, dv.VarName);
          act.ExecuteAction (act.Examine, act.mode_silent);
        END;
      END;
    | key.Tab, key.Right:
      IF pje^.state = MAX(JESTATE) THEN
        pje^.state := MIN(JESTATE);
      ELSE
        pje^.state := VAL (JESTATE, ORD(pje^.state)+1);
      END;
      deve.ModalRedraw (hwnd);
    | key.Left:
      IF pje^.state = MIN(JESTATE) THEN
        pje^.state := MAX(JESTATE);
      ELSE
        pje^.state := VAL (JESTATE, ORD(pje^.state)-1);
      END;
      deve.ModalRedraw (hwnd);
    ELSE
      crt.Beep;
      deve.ModalRedraw (hwnd);
    END;

  | deve.Mouse_Pressed:
    ASSERT(win.GetRelMouse(hwnd, msg, x, y));
    IF y = bline THEN
      FOR state := MIN(JESTATE) TO MAX(JESTATE) DO
        IF (JEButtons[state].x <= x) AND (x <= JEButtons[state].x+LENGTH(JEButtons[state].s)) THEN
          pje^.state := state;
        END;
      END;
    END;
    deve.ModalRedraw (hwnd);

  | deve.Mouse_Released:
    ASSERT(win.GetRelMouse(hwnd, msg, x, y));
    IF y = bline THEN
      FOR state := MIN(JESTATE) TO MAX(JESTATE) DO
        IF (JEButtons[state].x <= x) AND (x <= JEButtons[state].x+LENGTH(JEButtons[state].s)-1) THEN
          IF pje^.state = state THEN
            deve.AddToTail (hwnd, deve.KbHit, key.Enter);
            RETURN;
          END;
        END;
      END;
    END;
    deve.ModalRedraw (hwnd);

  | deve.Mouse_Moved
  , deve.Mouse_Dbl
  , deve.R_Mouse_Pressed
  , deve.R_Mouse:
    deve.ModalRedraw (hwnd);

  ELSE
    std.DefaultProc(hwnd,msg);
  END;
END JavaExceptionHandler;


VAR
  hJavaException: win.HWND;


PROCEDURE OpenJavaExceptionDialog (message-, obj_expr-: ARRAY OF CHAR);
VAR
  pje: PJEXPT;
BEGIN
  IF hJavaException = win.Invalid_H THEN
    hJavaException := win.RegisterWindow (JavaExceptionHandler, SIZE(pje^));
    ASSERT (hJavaException # win.Invalid_H);
    pje := win.GetAMPtr (hJavaException);
    pje^.message := NIL;
    pje^.obj_expr := NIL;
    win.SetWindowSize (hJavaException, crt.SZ {13, 8, 67, 16});
    std.CenterWindow (hJavaException);
    win.SetModal (hJavaException);
    win.SetHeaderByStr (hJavaException, 'Java exception');
  END;
  pje := win.GetAMPtr (hJavaException);
  IF pje^.message <> NIL THEN
    xs.dealloc_str (pje^.message);
  ELSE
    pje^.state := jes_Examine;
  END;
  xs.alloc_from (pje^.message, message);
  IF pje^.obj_expr <> NIL THEN
    xs.dealloc_str (pje^.obj_expr);
  END;
  xs.alloc_from (pje^.obj_expr, obj_expr);
  deve.AddToHead (hJavaException, deve.Rise, 0);
END OpenJavaExceptionDialog;


VAR
  Stop: BOOLEAN;

PROCEDURE ExceptionSpecialHandler(hwnd: win.HWND; msg: deve.MSG);
VAR
  size: crt.SZ;
  x, y: CARDINAL;
  attr: crt.ATTR;
  p   : std.PPMSG;
  i   : CARDINAL;
  pos1: CARDINAL;
  pos2: CARDINAL;
BEGIN
  p := win.GetAMPtr (hwnd);

  CASE msg.ID OF
  | deve.Rise:
    IF win.ActiveWindow # hwnd THEN
      deve.AddToTail(win.ActiveWindow, deve.Redraw, 0);
    END;
    deve.ModalRedraw (hwnd);
    win.Rise(hwnd);
    crt.RiseWindow(hwnd);


  | deve.Redraw, deve.Paint:
    crt.FillWindow(hwnd, ' ', p^.Colors^[crt.Msg_Frame]);

    FOR i := 0 TO HIGH (p^.Msg^) DO
      WITH p^.Msg^[i] DO
        crt.SetPos (x, y);
        crt.WrStr (hwnd, message, p^.Colors^[crt.Msg_Message]);
      END;
    END;

    size := win.GetWindowSize(hwnd);
    WITH size DO
      IF Stop THEN
        attr := p^.Colors^[crt.Msg_Button];
      ELSE
        attr := p^.Colors^[crt.Msg_Frame];
      END;
      pos1 := (x2-x1) DIV 2 -LENGTH('[ Examine ]')-1;
      crt.SetPos(pos1, y2-y1-2);
      crt.WrStr(hwnd,'[ Examine ]', attr);
      IF NOT Stop THEN
        attr := p^.Colors^[crt.Msg_Button];
      ELSE
        attr := p^.Colors^[crt.Msg_Frame];
      END;
      pos2 := (x2-x1) DIV 2 + 2;
      crt.SetPos(pos2, y2-y1-2);
      crt.WrStr(hwnd,'[ Continue ]', attr);
      crt.DrawFrame(hwnd, size, crt.Double, p^.Colors^[crt.Msg_Frame]);
      crt.DrawHeader(hwnd, size, p^.Colors^[crt.Msg_Header]);
    END;
    IF msg.ID = deve.Redraw THEN crt.UpdateRect(size); END;
    deve.CheckWaitForEvent (hwnd, msg.par);

  | deve.KbHit:
    CASE msg.par OF
    | key.Enter:
      deve.AddToTail (hwnd, deve.Hide, 0);
    | key.Tab, key.Left, key.Right:
      Stop := NOT Stop;
      deve.AddToTail(hwnd, deve.Redraw, deve.par_RedrawInFlush);
    ELSE
      crt.Beep;
      deve.AddToTail(hwnd, deve.Redraw, deve.par_RedrawInFlush);
    END;

  | deve.Mouse_Pressed:
    IF deve.InFlush THEN
      deve.WaitForEvent;
    END;

  | deve.Mouse_Released:
    ASSERT(win.GetRelMouse(hwnd, msg, x, y));
    size := win.GetWindowSize(hwnd);
    IF y = size.y2-size.y1-2 THEN
      pos1 := (size.x2-size.x1) DIV 2 -LENGTH('[ Examine ]')-1;
      pos2 := (size.x2-size.x1) DIV 2 + 2;
      IF (pos1 <= x) AND (x <= pos1+LENGTH('[ Examine ]')-1) THEN
        Stop := TRUE;
        deve.AddToTail(hwnd, deve.Hide, 0);
      ELSIF (pos2 <= x) AND (x <= pos2+LENGTH('[ Continue ]')-1) THEN
        Stop := FALSE;
        deve.AddToTail(hwnd, deve.Hide, 0);
      END;
    ELSE
      deve.AddToTail(hwnd, deve.Redraw, 1);
    END;

  ELSE
    std.DefaultProc(hwnd,msg);
  END;
END ExceptionSpecialHandler;


VAR
  AskToStop: win.HWND;


PROCEDURE RiseExceptionSpecial (message-: ARRAY OF CHAR);
VAR
  p: std.PPMSG;
  buf1, buf2: xs.String;
BEGIN
  IF AskToStop = win.Invalid_H THEN
    AskToStop := win.RegisterWindow (ExceptionSpecialHandler, SIZE(std.MSG_REC));
    ASSERT(AskToStop # win.Invalid_H);
    win.SetWindowSize (AskToStop, crt.SZ {13, 7, 67, 10});
    std.CenterWindow (AskToStop);
    win.SetModal (AskToStop);
    win.SetHeaderByStr (AskToStop, 'Program Exception');
    p := win.GetAMPtr (AskToStop);
    p^.Colors := sys.ADR (crt.Error);
    std.InitMessage (AskToStop, 8);
  END;
  p := win.GetAMPtr (AskToStop);
  IF message = "" THEN
    xcp.Get_XCPT_Msg (CARDINAL(eve.LastEvent.XCPT_INFO_1), CARDINAL(eve.LastEvent.XCPT_INFO_4), buf1);
    fmt.print(buf2, "Program has raised exception '%s'", buf1);
    std.SetMessage (AskToStop, buf2);
  ELSE
    std.SetMessage (AskToStop, message);
  END;
  std.CenterWindow (AskToStop);
  std.CenterMessage (win.GetWindowSize (AskToStop), p^.Msg);
  deve.AddToTail (AskToStop, deve.Rise, 0);
END RiseExceptionSpecial;


VAR
  WasLastTime, LastTime: BOOLEAN;


PROCEDURE DoAddress_x86 (data: DATA);
VAR
  i  : CARDINAL;
  res: BOOLEAN;
  NeedToStop: BOOLEAN;
  com, m, line, len: CARDINAL;
  savemode: dex.EXEC_KIND;
  pc: kt.ADDRESS;
  tmp_addr: kt.ADDRESS;
  pub_name: xs.txt_ptr;
  expt_name: xs.String;
  expt_num: CARDINAL;
  obj: dt.OBJECT;
  class_name: xs.txt_ptr;
  mod_name: xs.txt_ptr;
  tmp_str: xs.String;
  type: dt.PTYPE;
  user_code_found: BOOLEAN;
BEGIN
  IF opt.in_dialog THEN
    NeedToStop := FALSE;
    exe.WasBreakpoint := eve.LastEvent.IP;
    IF brk.FindByIndex_Breakpoint (eve.LastEvent.BreakpointInd, i) THEN
      WITH brk.Breakpoints.Breakpoints^[i] DO
        IF Break.Active THEN
          CASE Kind OF
          | brk.counter:
            INC(Break.Pass);
            RETURN;
          | brk.watchpoint:
            savemode := dex.ExecKind;
            dex.PointExecLine(eve.LastEvent.IP);
            dex.ExecKind := savemode;
            deve.Flush;

          | brk.normal:
            IF Break.Pass = 0 THEN
              IF (dex.ExecKind # dex.InTo) THEN
                NeedToStop := TRUE;
              END;
              IF NeedToStop AND (Condition # NIL) THEN
                NeedToStop := exp.GetRelation (Pos.ComN, Pos.ModN, Condition^, res) AND res;
              END;
              IF NeedToStop AND NOT Break.Sticky THEN
                ASSERT(brk.Delete_Breakpoint(i));
              END;
            ELSE
              DEC(Break.Pass);
            END;
          END;
        END;
        IF NeedToStop THEN
          dex.PointExecLine(eve.LastEvent.IP);
          exe.StopExec;
        END;
      END;
    ELSIF eve.LastEvent.BreakpointInd = exe.TmpBreakpointIndex THEN
     CASE dex.ExecKind OF
      | dex.UpToReturn:
        IF dex.UptoRetCount <= 1 THEN
          exe.RemoveTmpBreakPoint();
          ASSERT(exe.RemoveBreakPoint(dex.ProcBeginIndex));
          dex.ProcBeginIndex := 0;
          dex.PointExecLine(mem.GetIP());
          exe.StopExec;
          RETURN;
        ELSE
          DEC(dex.UptoRetCount);
        END;
      | dex.Trace:
        pc := mem.GetIP();
        dex.PointExecLine(pc);
        dex.ExecKind := dex.Trace;
        deve.Flush;
        dmm.ClearRegsState;
        exe.TurnStepModeOn;
        dex.WasCall := FALSE;
        IF opt.SkipDisasm THEN
          len := kdsm.IsCall(pc);
          IF len # 0 THEN
            dex.WasCall       := TRUE;
            dex.AddrAfterCall := pc+len;
          END;
        END;
        key.Delay(dv.Delay);
        RETURN;
      | dex.InTo:
        pc := mem.GetIP();
        exe.RemoveTmpBreakPoint();

        IF (dv.MainMode = dv.source) THEN
          IF tls.SourceByAddr(pc, com, m, line) THEN
            IF ((exe.ExecComp # com) OR (exe.ExecMod # m) OR (exe.ExecLine # line-1)) THEN
              dex.PointExecLine(mem.GetIP());
              exe.StopExec;
              RETURN;
            END;
          END;
        ELSE
          dex.PointExecLine(mem.GetIP());
          exe.StopExec;
          RETURN;
        END;
        exe.TurnStepModeOn;
        RETURN;
      | dex.UpToAddr, dex.UpToCurs:
        exe.RemoveTmpBreakPoint();
        dex.PointExecLine(mem.GetIP());
        exe.StopExec;
        RETURN;
      | dex.SeekingforRet:
        exe.RemoveTmpBreakPoint();
        pc := mem.GetIP();
        len := kdsm.IsCall(pc);
        IF len # 0 THEN
          dex.StepSP := mem.GetSP();
          ASSERT(exe.SetTmpBreakPoint(pc + len));
          RETURN;
        ELSE
          exe.TurnStepModeOn;
          RETURN;
        END;

      | dex.Step:
        IF mem.GetSP() >= dex.StepSP THEN
          exe.RemoveTmpBreakPoint();
          pc := mem.GetIP();
          IF (dv.MainMode = dv.source) THEN
            IF tls.SourceByAddr(pc, com, m, line) AND 
               ((exe.ExecComp # com) OR (exe.ExecMod # m) OR (exe.ExecLine # line-1)) THEN
              dex.PointExecLine(mem.GetIP());
              exe.StopExec;
              RETURN;
            ELSE
              len := kdsm.IsCall(pc);
              IF len # 0 THEN
                dex.StepSP := mem.GetSP();
                ASSERT(exe.SetTmpBreakPoint(pc + len));
                RETURN;
              ELSE
                exe.TurnStepModeOn;
                RETURN;
              END;
            END;
          ELSE
            dex.PointExecLine(mem.GetIP());
            exe.StopExec;
            RETURN;
          END;
        END;
      END;
    ELSIF dex.Traps[dex.LongjmpNo].Ready AND (dex.Traps[dex.LongjmpNo].Inx = eve.LastEvent.BreakpointInd) THEN
      CASE dex.ExecKind OF
      | dex.Step, dex.InTo, dex.Trace, dex.GO:
        tmp_addr := 0;
        IF mem.Get(mem.GetSP(), sys.ADR(tmp_addr), 4) THEN
          IF tls.FindPublicByAddr (tmp_addr, FALSE, com, pub_name) THEN
            COPY(pub_name^, expt_name);
            xs.Uppercase(expt_name);
            IF expt_name = dex.LongjmpCall THEN
              stk.ResetCallStack;
              stk.ScanCallStack;
              user_code_found := stk.SkipProcWithoutDebugInfo (pc, obj);
              IF NOT user_code_found THEN
                pc := eve.LastEvent.IP;
              END;
              de.PutExceptionInfo (pc, "Exception was raised here");
              IF opt.ShowSoftwareException THEN
                dbg.SwitchToDebugger;
                dex.PointExecLine(pc);
                eve.LastEvent.XCPT_INFO_1 := CARDINAL(xcp.GENERAL_EXCEPTION);
                eve.LastEvent.XCPT_INFO_4 := CARDINAL(0);
                RiseExceptionSpecial ("");
                deve.Flush;
                IF Stop THEN
                  IF user_code_found THEN
                    exe.StopExec();
                  ELSE
                    exe.TurnStepModeOn;
                    dex.ExecKind := dex.SeekingforRet;
                    LastTime := FALSE;
                  END;
                END;
              END;
            END;
          END;
        END;
      ELSE
      END;
      RETURN;
    ELSIF dex.CheckTrap (dex.JavaLangRaisedException, eve.LastEvent.BreakpointInd) THEN
      stk.ResetCallStack;
      stk.ScanCallStack;
      IF NOT stk.UnrollStack (1, pc, obj) THEN
        IF NOT stk.SkipProcWithoutDebugInfo (pc, obj) THEN
          pc := eve.LastEvent.IP;
        END;
      END;
      tmp_addr := 0;
      ASSERT (mem.Get (mem.GetSP()+4, sys.ADR(tmp_addr), 4)); -- первый параметр по смещению +4
      -- now tmp_addr is pointer to object
      IF exp.ROT_xjRTS_Java (tmp_addr, TRUE, type) AND NOT tls.IsTypePrimitive (type) AND tls.IsTypeValid (type) THEN
        com := tls.TypeCom (type);
        ASSERT (tls.ComName (com, pub_name));
        ASSERT (tls.ModName (com, tls.TypeMod(type), mod_name));
        ASSERT (tls.TypeName (type, class_name));
        fmt.print (tmp_str, "%s.%s.%s<CARD32<@esp+4>>", pub_name^, mod_name^, class_name^);
        fmt.print (expt_name, "Thrown exception: class %s", class_name^);
      ELSE
        COPY ("", tmp_str);
        COPY ("Thrown exception", expt_name);
      END;
      de.PutExceptionInfo (pc, expt_name);
      IF opt.ShowSoftwareException THEN
        dbg.SwitchToDebugger;
        dex.PointExecLine (pc);
        OpenJavaExceptionDialog (expt_name, tmp_str);
        deve.Flush;
        IF AfterJavaExceptionStop THEN
          exe.StopExec;
        END;
      END;
      RETURN;
    ELSIF dex.CheckTrap (dex.JavaLangUncaughtException, eve.LastEvent.BreakpointInd) OR
          dex.CheckTrap (dex.JavaLangRedundantMethod, eve.LastEvent.BreakpointInd) OR
          dex.CheckTrap (dex.JavaLangUnsatisfiedLink, eve.LastEvent.BreakpointInd)
    THEN
      exe.StopExec;
      dbg.SwitchToDebugger;
      stk.ResetCallStack;
      stk.ScanCallStack;
      IF NOT stk.SkipProcWithoutDebugInfo (pc, obj) THEN
        pc := eve.LastEvent.IP;
      END;
      IF dex.CheckTrap (dex.JavaLangUncaughtException, eve.LastEvent.BreakpointInd) THEN
        de.PutExceptionInfo (pc, "Java uncaught exception");
        dex.PointExecLine (pc);
        std.Error ("Java uncaught exception");
      ELSIF dex.CheckTrap (dex.JavaLangRedundantMethod, eve.LastEvent.BreakpointInd) THEN
        de.PutExceptionInfo (pc, "Redundant method has been called: wrong 'smart' linking");
        dex.PointExecLine (pc);
        std.Error ("Redundant method have been called: wrong 'smart' linking");
      ELSIF dex.CheckTrap (dex.JavaLangUnsatisfiedLink, eve.LastEvent.BreakpointInd) THEN
        de.PutExceptionInfo (pc, "Unsatisfied Link Error");
        dex.PointExecLine (pc);
        std.Error ("Unsatisfied Link Error");
      END;
      RETURN;
    ELSIF dex.ProcBeginIndex = eve.LastEvent.BreakpointInd THEN
      INC(dex.UptoRetCount);
    ELSIF MAX(CARDINAL) = eve.LastEvent.BreakpointInd THEN
      dex.PointExecLine(mem.GetIP());
      exe.StopExec;
    (*
    ELSIF MAX(CARDINAL) = eve.LastEvent.BreakpointInd THEN

      FOR i := 0 TO HIGH(dt.DI.ProcBegin^) DO
        WITH dt.DI.ProcBegin^[i] DO
          ASSERT(mem.Put(ProcBegin, sys.ADR(OpCode), 1));
        END;
      END;
      dex.PointExecLine(mem.GetIP());
      exe.StopExec;
    *)
    ELSE
      FOR i := 0 TO HIGH(dex.Traps) DO
        WITH dex.Traps[i] DO
          IF Ready AND (Inx = eve.LastEvent.BreakpointInd) THEN
            IF (opt.ShowSoftwareException) OR 
               (eve.LastEvent.BreakpointInd <= dex.Traps[dex.LAST_FATAL_EXCEPTION].Inx)
            THEN
              exe.StopExec;
              stk.ResetCallStack;
              stk.ScanCallStack;
              pc := eve.LastEvent.IP;
              sys.EVAL (stk.SkipProcWithoutDebugInfo (pc, obj));
              dex.PointExecLine (pc);
              expt_num := 0;
              IF NOT mem.Get (mem.GetSP()+4, sys.ADR(expt_num), 4) THEN
                expt_num := xcp.GENERAL_EXCEPTION;
              END;
              xcp.Get_RTS_XCPT_Msg (expt_num, expt_name);
              pub_name := pro.Get (mes.RTS_exception);
              fmt.print (tmp_str, pub_name^, expt_num, expt_name);
              de.PutExceptionInfo (pc, tmp_str);
              std.Error (tmp_str);
            END;
            RETURN;
          END;
        END;
      END;
    END;
  END;
END DoAddress_x86;



PROCEDURE DoStep_x86 (data: DATA);
VAR
  com, m, line: CARDINAL;
  pos : CARDINAL;
  NeedToStop: BOOLEAN;
  buf : ARRAY [0..74] OF CHAR;
  fmt_str: ARRAY [0..79] OF CHAR;
  len: CARDINAL;
BEGIN
  IF opt.in_dialog THEN
    erc.CancelReact;
    NeedToStop := FALSE;
    IF brk.ConditionBreaks.free > 0 THEN
      pc := mem.GetIP();
      IF NOT tls.FindModByAddr(pc, com, m) THEN
        m := dt.Invalid_Module;
      END;
      pos := 0;
      WHILE brk.NeedToStop(com, m, pos) DO
        IF NOT NeedToStop THEN
          NeedToStop := TRUE;
          dex.PointExecLine(pc);
        END;
        pro.GetMsg(mes.BreakNotification, fmt_str);
        fmt.print(buf, fmt_str, brk.ConditionBreaks.ConditionBreaks^[pos].Expr^);
        Notify(buf);
        INC(pos);
      END;
    END;
    CASE dex.ExecKind OF
    | dex.UpToCurs:
    | dex.GO:
      IF key.KeyPressed() THEN
        NeedToStop := key.GetKey() = key.Esc;
      END;
    | dex.Trace:
      IF key.KeyPressed() THEN
        NeedToStop := key.GetKey() = key.Esc;
      END;
      pc := mem.GetIP();
      IF (dv.MainMode = dv.source) THEN
        IF tls.SourceByAddr(pc, com, m, line) THEN
          dex.WasCall := FALSE;
          IF (exe.ExecComp # com) OR (exe.ExecMod # m) OR (exe.ExecLine # line-1) THEN
            IF  opt.ShowModuleWithoutSource OR tls.ModHaveSource(com, m) THEN
              dex.PointExecLine(pc);
              dex.ExecKind := dex.Trace;
              deve.Flush;
              dmm.ClearRegsState;
              key.Delay(dv.Delay);
            END;
          END;
        ELSE
          IF opt.SkipDisasm THEN
            IF dex.WasCall THEN
              exe.TurnStepModeOff;
              ASSERT(exe.SetTmpBreakPoint(dex.AddrAfterCall));
              dex.WasCall := FALSE;
            END;
          ELSE
            dex.PointExecLine(pc);
            dex.ExecKind := dex.Trace;
            deve.Flush;
            dmm.ClearRegsState;
            key.Delay(dv.Delay);
          END;
        END;
      ELSE
        dex.PointExecLine(pc);
        dex.ExecKind := dex.Trace;
        deve.Flush;
        dmm.ClearRegsState;
        key.Delay(dv.Delay);
      END;
      IF NOT dex.WasCall AND opt.SkipDisasm THEN
        len := kdsm.IsCall(pc);
        IF len # 0 THEN
          dex.WasCall       := TRUE;
          dex.AddrAfterCall := pc+len;
        END;
      END;
    | dex.SeekingforRet:
      pc := mem.GetIP();
      WasLastTime := LastTime;
      LastTime := kdsm.IsRet(pc);
      IF WasLastTime THEN
        exe.StopExec;
        exe.TurnStepModeOff;
        dbg.SwitchToDebugger;
        dex.ExecKind := dex.Stopped;
        dex.PointExecLine(pc);
        RETURN;
      ELSIF NOT LastTime THEN
        len := kdsm.IsCall(pc);
        IF len # 0 THEN
          exe.RemoveTmpBreakPoint();
          dex.StepSP := mem.GetSP();
          ASSERT(exe.SetTmpBreakPoint(pc + len));
          exe.TurnStepModeOff;
          RETURN;
        END;
      END;
    | dex.Step:
      pc := mem.GetIP();
      IF NOT NeedToStop THEN
        IF (dv.MainMode = dv.source) THEN
          IF tls.SourceByAddr(pc, com, m, line) THEN
            IF ((exe.ExecComp # com) OR (exe.ExecMod # m) OR (exe.ExecLine # line-1)) THEN
              NeedToStop := opt.ShowModuleWithoutSource OR tls.ModHaveSource(com, m);
            END;
          END;
          IF NOT NeedToStop THEN
            len := kdsm.IsCall(pc);
            IF len # 0 THEN
              dex.StepSP := mem.GetSP();
              ASSERT(exe.SetTmpBreakPoint(pc + len));
              exe.TurnStepModeOff;
              RETURN;
            END;
          END;
        ELSE
          NeedToStop := TRUE;
        END;
      END;
    | dex.InTo:
      pc  := mem.GetIP();
      IF NOT NeedToStop THEN
        IF (dv.MainMode = dv.source) THEN
          IF tls.SourceByAddr(pc, com, m, line) THEN
            dex.WasCall := FALSE;
            IF (exe.ExecComp # com) OR (exe.ExecMod # m) OR (exe.ExecLine # line-1) THEN
              IF opt.ShowModuleWithoutSource OR tls.ModHaveSource(com, m) THEN
                 NeedToStop:= TRUE;
              ELSE
                 dex.ExecKind:= dex.Step;
              END;
            END;
          ELSE
            IF opt.SkipDisasm THEN
              IF kdsm.IsJmpForDll (pc) = 0 THEN
                IF dex.WasCall THEN
                  exe.TurnStepModeOff;
                  ASSERT(exe.SetTmpBreakPoint(dex.AddrAfterCall));
                  dex.WasCall := FALSE;
                ELSE
                  NeedToStop := FALSE;
                END;
              END;
            ELSE
              NeedToStop := TRUE;
            END;
          END;
        ELSE
          NeedToStop := TRUE;
        END;
      END;
    END;
    IF NOT dex.WasCall AND (opt.SkipDisasm OR NOT opt.ShowModuleWithoutSource) THEN
      len := kdsm.IsCall(pc);
      IF len # 0 THEN
        dex.WasCall       := TRUE;
        dex.AddrAfterCall := pc+len;
      END;
    END;
    IF NeedToStop THEN
      exe.TurnStepModeOff;
      dex.ExecKind := dex.Stopped;
      pc := mem.GetIP();
      dex.PointExecLine(pc);
      exe.StopExec;
    END;
  END;
END DoStep_x86;


<* END *>

PROCEDURE DoMemoryAccess(data: DATA);
VAR
  text    ,
  m       : xs.String;
  BreakPos: CARDINAL;
  was, is : xs.String;
  i, byte : CARDINAL;
  s       : ARRAY [0..2] OF CHAR;

  access_loc: kt.ADDRESS;
  access_len: CARDINAL;

BEGIN
  IF opt.in_dialog THEN
    erc.CancelReact;
    ASSERT (eve.LastEvent.Event = eve.MemoryAccess);
    IF NOT brk.FindByIndex_AccessBreak(eve.LastEvent.MemAccess_Ind, BreakPos) THEN
      dex.PointExecLine (mem.GetIP());
      exe.StopExec;
      Notify ("Read/Write memory access detected.\n\nNOTE: The debugger cannot recognize this access break among user's ones.");
      RETURN;
    END;
    WITH brk.AccessBreaks.AccessBreaks^[BreakPos] DO
      WITH Access_Data DO
        ASSERT(Access_ID = brk.Memory);
<* PUSH *>
<* WOFF900+ *>
        WITH eve.LastEvent DO
<* POP *>

<* IF DEST_K26 THEN *>
          ASSERT(MemAccess_Type = Access_Type);
          ASSERT( ((Location<=MemAccess_Addr)&(MemAccess_Addr<=Location+Len-1)) OR
                  ((Location<=MemAccess_Addr+MemAccess_Len-1) AND
                   (MemAccess_Addr+MemAccess_Len-1<=Location+Len-1)));
          access_loc := MemAccess_Addr;
          access_len := MemAccess_Len;
<* ELSE *>
          access_loc := Location;
          access_len := Len;
  <* IF env_target = 'x86os2' THEN *>
          ASSERT( kmem.SetTrace(Access_Type, Location, Len, Break.Index) );
   <* END *>
<* END *>

          WITH Break DO
            ASSERT(Active);
            IF Pass > 0 THEN DEC(Pass); RETURN; END;
            text  := '';
            IF eve.Read = Access_Type THEN
                is  := '';
                FOR i := access_len-1 TO 0 BY -1 DO
                  byte := 0;
                  ASSERT(mem.Get(access_loc+i, sys.ADR(byte), 1));
                  fmt.print(s, "%$2X", byte);
                  xs.Append(s, is);
                END;
                pro.GetMsg(mes.BreakRead, m);
                fmt.print(text, m, access_loc, access_len, is);
                pc := mem.GetIP();
                dex.PointExecLine(pc);
                exe.StopExec;
                Notify(text);
            ELSIF eve.Write = Access_Type THEN
<* WOFF304+ *>
                was := '';
                is  := '';
                IF Len <= brk.PREV_VALUE_LENGTH THEN
                  FOR i := Len-1 TO 0 BY -1 DO
                    byte := VAL(CARDINAL, Prev_value[i]);
                    fmt.print(s, "%$2X", byte);
                    xs.Append(s, was);
                    ASSERT(mem.Get(access_loc+i, sys.ADR(Prev_value[i]), 1));
                    byte := VAL(CARDINAL, Prev_value[i]);
                    fmt.print(s, "%$2X", byte);
                    xs.Append(s, is);
                  END;
                ELSE
                  FOR i := Len-1 TO 0 BY -1 DO
                    byte := VAL(CARDINAL, Prev_value_long^[i]);
                    fmt.print(s, "%$2X", byte);
                    xs.Append(s, was);
                    ASSERT(mem.Get(access_loc+i, sys.ADR(Prev_value_long^[i]), 1));
                    byte := VAL(CARDINAL, Prev_value_long^[i]);
                    fmt.print(s, "%$2X", byte);
                    xs.Append(s, is);
                  END;
                END;
<* WOFF304- *>
                pro.GetMsg(mes.BreakWrite, m);
                fmt.print(text, m, access_loc, access_loc+access_len-1,
                                   access_loc, access_len, was, is);
                pc := mem.GetIP();
                dex.PointExecLine(pc);
                exe.StopExec;
                Notify(text);
            ELSE
              ASSERT(FALSE);
            END;
            IF NOT Sticky THEN
              ASSERT(brk.Delete_Breakpoint(BreakPos));
            END;
          END;
        END;
      END;
    END;
  END;
END DoMemoryAccess;


PROCEDURE DoCompDestroyed (data: DATA);
VAR
  i   : CARDINAL;
  com1: dt.ComNo;
  com2: dt.ComNo;
  name: xs.txt_ptr;
  tmp : xs.String;
  need: BOOLEAN;
  p   : std.PLIST;
BEGIN
  ASSERT(eve.LastEvent.Event = eve.CompDestroyed);
  IF tls.FindComponentByHandle (eve.LastEvent.Handle, com1) THEN
    ASSERT(tls.ComName (com1, name));
    COPY(name^, tmp);
    WITH brk.Breakpoints DO
      IF free # 0 THEN
        need := TRUE;
        FOR i := 0 TO free-1 DO
          WITH Breakpoints^[i] DO
            IF Break.Active AND tls.FindComponentByAddr (Addr, com2) AND (com1 = com2) THEN
              IF need THEN
                std.NotifyNo (mes.CompUnload_BreakDisasbled, tmp);
                need := FALSE;
              END;
              Break.Active := FALSE;
            END;
          END;
        END;
      END;
    END;
  END;
  IF std.Wnds[std.ComponentsWindow].hwnd # win.Invalid_H THEN
    p := win.GetAMPtr(std.Wnds[std.ComponentsWindow].hwnd);
    p^.curr := 0;
    p^.frame := 0;
  END;
END DoCompDestroyed;


<* IF DEST_K26 THEN *>

PROCEDURE DoRegisterAccess(data: DATA);
VAR
  text    : xs.String;
  BreakPos: CARDINAL;
  was, is : xs.String;
  i       : CARDINAL;
  pbyte   : POINTER TO sys.CARD8;
  s       : ARRAY [0..2] OF CHAR;
  RegNo   : CARDINAL;
  RegValue: kt.REG_VALUE;
  RegName : xs.String;
BEGIN
  IF opt.in_dialog THEN
    pc := mem.GetIP();
    erc.CancelReact;
    ASSERT(eve.LastEvent.Event = eve.RegisterAccess);
    IF dmm.FindRegisterByIndex(eve.LastEvent.RegAccess_Ind, RegNo) THEN
      dmm.ChangeReg(RegNo, eve.LastEvent.RegAccess_Type);
      RETURN;
    END;
    ASSERT(brk.FindByIndex_AccessBreak(eve.LastEvent.RegAccess_Ind, BreakPos));
    WITH brk.AccessBreaks.AccessBreaks^[BreakPos] DO
      WITH Access_Data DO
        ASSERT(Access_ID = brk.Register);
        WITH eve.LastEvent DO
          ASSERT(RegAccess_Type = Access_Type);
          ASSERT(RegAccess_RegNo = Reg_No);
          WITH Break DO
            IF Active THEN
              IF Pass > 0 THEN DEC(Pass); RETURN; END;
              text  := '';
              CASE RegAccess_Type OF
              | eve.Read  :
                  ASSERT(mem.GetReg(Reg_No, RegValue));
                  is  := '';
                  FOR i := SIZE(kt.REG_VALUE)-1 TO 0 BY -1 DO
                    pbyte := sys.ADDADR(sys.ADR(RegValue), i);
                    fmt.print(s, "%$2X", pbyte^);
                    xs.Append(s, is);
                  END;
                  ASSERT(exe.GetRegName(RegAccess_RegNo, RegName));
                  dex.PointExecLine(pc);
                  std.NotifyNo(mes.BreakReadReg, RegName, RegAccess_Len, is);
              | eve.Write :
<* WOFF304+ *>
                  ASSERT(mem.GetReg(Reg_No, RegValue));
                  was := '';
                  is  := '';
                  FOR i := SIZE(kt.REG_VALUE)-1 TO 0 BY -1 DO
                    pbyte := sys.ADDADR(sys.ADR(Prev_Reg_value), i);
                    fmt.print(s, "%$2X", pbyte^);
                    xs.Append(s, was);
                    pbyte := sys.ADDADR(sys.ADR(RegValue), i);
                    fmt.print(s, "%$2X", pbyte^);
                    xs.Append(s, is);
                  END;
                 sys.MOVE(sys.ADR(RegValue), sys.ADR(Prev_Reg_value), SIZE(kt.REG_VALUE));
<* WOFF304- *>
                 ASSERT(exe.GetRegName(RegAccess_RegNo, RegName));
                 dex.PointExecLine(pc);
                 std.NotifyNo(mes.BreakWriteReg, RegName, RegAccess_Len, was, is);
              END;
              exe.StopExec;
              IF NOT Sticky THEN
                ASSERT(brk.Delete_Breakpoint(BreakPos));
              END;
            END;
          END;
        END;
      END;
    END;
  END;
END DoRegisterAccess;


(* Строит стек вызовов / и может быть реакция на выполнение до вызова *)
PROCEDURE DoCall(data: DATA);
VAR
  com, m, line: CARDINAL;
BEGIN
  IF opt.in_dialog THEN
    ASSERT(eve.LastEvent.Event = eve.Call);
    stk.PushCall(eve.LastEvent.IP, mem.GetFrame());
    dex.ResetCall;
    erc.CancelReact;
    CASE dex.ExecKind OF
    | dex.UpToCall:
      pc := mem.GetIP();
      dex.PointExecLine(pc);
      exe.StopExec;
    | dex.InTo, dex.Trace:
      IF opt.SkipDisasm THEN
        IF CallAddr = 0 THEN
          IF ~tls.SourceByAddr(pc, com, m, line) THEN
            Counter := 1;
            DontStop := TRUE;
          ELSE
            dex.AddrAfterCall := 0;
          END;
        ELSIF eve.LastEvent.CallAddr = CallAddr THEN
          INC(Counter);
        END;
(*
      ELSIF NOT opt.ShowModuleWithoutSource THEN
        ???
*)
      END;
    ELSE
    END;
  END;
END DoCall;


(* Выпихивает один элемент стека вызовов / и может быть реакция на выполнение до возрата *)
PROCEDURE DoRet(data: DATA);
BEGIN
  IF opt.in_dialog THEN
    stk.PopCall();
    dex.ResetCall;
    erc.CancelReact;
    CASE dex.ExecKind OF
    | dex.UpToReturn:
      IF dv.N_Call >= stk.CallTop()+1 THEN
        pc := mem.GetIP();
        dex.PointExecLine(pc);
        exe.StopExec;
      END;
    | dex.InTo, dex.Trace:
      IF opt.SkipDisasm THEN
        IF dex.AddrAfterCall = eve.LastEvent.ReturnAddr THEN
          DEC(Counter);
          IF Counter = 0 THEN
            exe.StopExec;
            DontStop := FALSE;
            CallAddr := 0;
          END;
        END;
(*
      ELSIF NOT opt.ShowModuleWithoutSource THEN
        ???
*)
      END;
    ELSE
    END;
  END;
END DoRet;

<* END *>

<* IF DEST_XDS THEN *>

PROCEDURE DoException_x86(data: DATA);
VAR
  msg: xs.String;
  tmp: xs.txt_ptr;
  buf: xs.String;
BEGIN
  IF opt.in_dialog THEN
    CASE eve.LastEvent.Exception_ID OF
    | eve.OutOfMemory:
      exe.StopExec;
      tmp := pro.Get (mes.BreakOutMem);
      fmt.print (msg, tmp^, CARDINAL(eve.LastEvent.XCPT_INFO_1));
      de.PutExceptionInfo (eve.LastEvent.IP, msg);
      dex.PointExecLine (eve.LastEvent.IP);
      std.ErrorNo (mes.BreakOutMem, CARDINAL(eve.LastEvent.XCPT_INFO_1));
    | eve.WriteProtected:
      exe.StopExec;
      tmp := pro.Get (mes.BreakWrProt);
      fmt.print (msg, tmp^, CARDINAL(eve.LastEvent.XCPT_INFO_1));
      de.PutExceptionInfo (eve.LastEvent.IP, msg);
      dex.PointExecLine(eve.LastEvent.IP);
      std.ErrorNo(mes.BreakWrProt, CARDINAL(eve.LastEvent.XCPT_INFO_1));
    | eve.UserException:
      exe.StopExec;
      tmp := pro.Get (mes.User_React_Default);
      fmt.print (msg, tmp^, CARDINAL(eve.LastEvent.XCPT_INFO_1));
      de.PutExceptionInfo (eve.LastEvent.IP, msg);
      dex.PointExecLine(eve.LastEvent.IP);
      std.ErrorNo(mes.User_React_Default, CARDINAL(eve.LastEvent.XCPT_INFO_1));
      kexe.ProgramContextOk := TRUE;
    | eve.ProgramException:
      WITH eve.LastEvent DO
        IF CARDINAL(XCPT_INFO_3) = 0 THEN
          kexe.ProgramContextOk := FALSE;
          erc.CancelReact;
          exe.StopQueueProc;
          exe.StopExec;
          dex.PointExecLine(eve.LastEvent.IP);
          IF CARDINAL(XCPT_INFO_1) = 0 THEN
            IF exe.ProgramSkipToMainEntry THEN
              pro.GetMsg (mes.ExceptionAtStartup, msg);
              de.PutExceptionInfo (eve.LastEvent.IP, msg);
              std.Error (msg);
            ELSE
              std.InformationNo (mes.ProgramFinished);
            END;
          ELSE
            xcp.Get_XCPT_Msg (CARDINAL(XCPT_INFO_1), CARDINAL(XCPT_INFO_4), msg);
            tmp := pro.Get (mes.BreakProgInt);
            fmt.print (buf, tmp^, msg, CARDINAL(eve.LastEvent.XCPT_INFO_2));
            de.PutExceptionInfo (eve.LastEvent.IP, buf);
            std.ErrorNo (mes.BreakProgInt, msg, CARDINAL(eve.LastEvent.XCPT_INFO_2));
          END;
        ELSE
          -- или пришли после первой нотификации, или исключительная ситауция не может быть обработана
          dbg.SwitchToDebugger;
          dex.PointExecLine(eve.LastEvent.IP);
          xcp.Get_XCPT_Msg (CARDINAL(eve.LastEvent.XCPT_INFO_1), CARDINAL(eve.LastEvent.XCPT_INFO_4), msg);
          IF BOOLEAN(eve.LastEvent.XCPT_INFO_3) THEN
            fmt.print (buf, "First-chance exception '%s' at 0x%$8X", msg, eve.LastEvent.IP);
          ELSE
            fmt.print (buf, "Program exception '%s' at 0x%$8X", msg, eve.LastEvent.IP);
          END;
          de.PutExceptionInfo (eve.LastEvent.IP, buf);
          RiseExceptionSpecial ("");
          deve.Flush;
          IF Stop THEN
            exe.StopExec;
          END;
        END;
      END;
    END;
  END;
END DoException_x86;

<* END *>




 -- returns TRUE if exception is processed
PROCEDURE ExceptionHandler (type: xi.HandlerType; IP: kt.ADDRESS; msg-: ARRAY OF CHAR; SEQ args: sys.BYTE): BOOLEAN;
VAR
  obj: dt.OBJECT;
  user_code_IP: kt.ADDRESS;
  message: xs.String;
  buf1, buf2: xs.String;
BEGIN
  IF NOT opt.in_dialog THEN
    RETURN FALSE;
  END;
  dbg.SwitchToDebugger;
  dex.PointExecLine (IP);
  fmt.print (message, msg, args);
  user_code_IP := IP;
  stk.ResetCallStack;
  stk.ScanCallStack;
  IF stk.SkipProcWithoutDebugInfo (user_code_IP, obj) AND
     mod.SetNewPosByAddr (user_code_IP)
  THEN
    de.PutExceptionInfo (user_code_IP, message);
  ELSE
    de.PutExceptionInfo (IP, message);
  END;
  IF exe.ProgramSkipToMainEntry THEN
    pro.GetMsg (mes.ExceptionAtStartup, buf1);
    fmt.print (buf2, "%s\n\nNote: %s", message, buf1);
    COPY (buf2, message);
  END;
  CASE type OF
  | rts.htException: -- The user can choose either to stop or to continue execution
    RiseExceptionSpecial (message);
    deve.Flush;
    RETURN Stop;
  | rts.htTrap:      -- A program is unconditionally stopped
    std.ErrorArg (msg, args);
    RETURN TRUE;
  | rts.htMessage:   -- The debugger shows a message
    std.InformationArg (msg, args);
    RETURN FALSE;
  END;
END ExceptionHandler;


PROCEDURE EnableBreakpointsOnComponentLoading (data: erc.DATA);
VAR
  i: CARDINAL;
  com_by_addr: dt.ComNo;
  com_by_handle: dt.ComNo;
BEGIN
  ASSERT (data = NIL);
  ASSERT (eve.LastEvent.Event = eve.CompCreated);
  WITH brk.Breakpoints DO
    IF free = 0 THEN
      RETURN;
    END;
    FOR i := 0 TO free-1 DO
      WITH Breakpoints^[i] DO
        IF tls.FindComponentByAddr (Addr, com_by_addr) 
          AND tls.FindComponentByHandle (eve.LastEvent.Component.Handle, com_by_handle)
          AND (com_by_addr = com_by_handle)
        THEN
          sys.EVAL (brk.Enable_Breakpoint (i));
        END;
      END;
    END;
  END;
END EnableBreakpointsOnComponentLoading;


<* WOFF301- *>

VAR
  SaveBreak: brk.PBREAKPOINT;
  OldHandler: xi.HANDLER;


PROCEDURE Ini;
VAR
  i, index: CARDINAL;
BEGIN
  OldHandler := xi.SetHandler (ExceptionHandler);

  erc.InsActionFirst (eve.MemoryAccess, NIL, DoMemoryAccess);
  erc.InsActionFirst (eve.CompDestroyed, NIL, DoCompDestroyed);
  erc.AddAction (eve.CompCreated, NIL, EnableBreakpointsOnComponentLoading);

<* IF DEST_K26 THEN *>
  erc.InsAction (eve.Call, NIL, DoCall, erc.FirstReaction (eve.Call));
  erc.InsAction (eve.Return, NIL, DoRet, erc.FirstReaction (eve.Return));
  erc.InsAction (eve.SingleStep, NIL, DoStep_VAX, erc.FirstReaction (eve.SingleStep));
  erc.InsAction (eve.RegisterAccess, NIL, DoRegisterAccess, erc.FirstReaction (eve.RegisterAccess));
  erc.AddActionSecond (eve.BreakpointHit, NIL, DoAddress_VAX);
  erc.AddActionSecond (eve.Exception, NIL, DoException_VAX);
<* ELSE *>
  erc.InsAction (eve.SingleStep, NIL, DoStep_x86, erc.FirstReaction (eve.SingleStep));
  erc.AddActionSecond (eve.BreakpointHit, NIL, DoAddress_x86);
  erc.AddActionSecond (eve.Exception, NIL, DoException_x86);
<* END *>

  IF SaveBreak # NIL THEN
    FOR i := 0 TO HIGH(SaveBreak^) DO
      IF SaveBreak^[i].Break.Owner = brk.Dialog THEN
        ASSERT(brk.Add_Breakpoint(SaveBreak^[i], TRUE, index));
      END;
    END;
    DISPOSE(SaveBreak);
  END;
END Ini;



PROCEDURE Exi;
  
  PROCEDURE RemAction (event: eve.EVENT_TYPE; do: erc.DO_PROC);
  BEGIN
    erc.RemAction (event, erc.FindReaction (erc.FirstReaction (event), do));
  END RemAction;  
 

VAR
  i : CARDINAL;
BEGIN
  RemAction (eve.MemoryAccess,  DoMemoryAccess);
  RemAction (eve.CompDestroyed, DoCompDestroyed);

<* IF DEST_K26 THEN *>
  RemAction (eve.Call,           DoCall);
  RemAction (eve.Return,         DoRet);
  RemAction (eve.BreakpointHit,  DoAddress_VAX);
  RemAction (eve.SingleStep,     DoStep_VAX);
  RemAction (eve.Exception,      DoException_VAX);
  RemAction (eve.RegisterAccess, DoRegisterAccess);
<* ELSE *>
  RemAction (eve.BreakpointHit,  DoAddress_x86);
  RemAction (eve.SingleStep,     DoStep_x86);
  RemAction (eve.Exception,      DoException_x86);
<* END *>

  IF kexe.ProgramContextOk THEN
    WITH brk.Breakpoints DO
      IF free > 0 THEN
        NEW(SaveBreak, free);
        sys.MOVE(sys.ADR(brk.Breakpoints.Breakpoints^), sys.ADR(SaveBreak^),
                 free*SIZE(brk.BREAKPOINT));
        FOR i := free - 1 TO 0 BY -1 DO
          IF Breakpoints^[i].Break.Owner = brk.Dialog THEN
            ASSERT(brk.Delete_Breakpoint(i));
          END;
        END;
      END;
    END;
  END;

  OldHandler := xi.SetHandler (OldHandler);
END Exi;



<* IF TARGET_OS = "WINNT" THEN *>

CONST
  ExitFunctionName = "ExitProcess";

TYPE
  SET_EXIT_HANDLER_DATA = RECORD
                            comHandle: CARDINAL;
                            BreakInx: CARDINAL;
                          END;

  PSET_EXIT_HANDLER_DATA = POINTER TO SET_EXIT_HANDLER_DATA;


PROCEDURE SetExitHandler (data: erc.DATA);
VAR
  ComNo: dt.ComNo;
  entry: kt.ADDRESS;
  p: PSET_EXIT_HANDLER_DATA;
BEGIN
  ASSERT (data # NIL);
  ASSERT (eve.LastEvent.Event = eve.CompCreated);
  p := PSET_EXIT_HANDLER_DATA (data);
  IF NOT tls.FindComponentByHandle (eve.LastEvent.Component.Handle, ComNo) THEN
    RETURN;
  END;
  IF NOT tls.FindPublicByNameInCom (ComNo, ExitFunctionName, entry) THEN
    RETURN;
  END;
  IF exe.SetBreakPoint (entry, p^.BreakInx) THEN
    p^.comHandle := eve.LastEvent.Component.Handle;
  ELSE
    p^.comHandle := 0;
  END;
END SetExitHandler;



PROCEDURE RemoveExitHandler (data: erc.DATA);
VAR
  p: PSET_EXIT_HANDLER_DATA;
BEGIN
  ASSERT (data # NIL);
  ASSERT (eve.LastEvent.Event = eve.CompDestroyed);
  p := PSET_EXIT_HANDLER_DATA (data);
  IF p^.comHandle = eve.LastEvent.Component.Handle THEN
    sys.EVAL (exe.RemoveBreakPoint (p^.BreakInx));
    p^.comHandle := 0;
  END;
END RemoveExitHandler;



PROCEDURE ExitHandler (data: erc.DATA);
VAR
  p: PSET_EXIT_HANDLER_DATA;
BEGIN
  IF NOT opt.in_dialog THEN
    RETURN;
  END;
  ASSERT (data # NIL);
  ASSERT (eve.LastEvent.Event = eve.BreakpointHit);
  p := PSET_EXIT_HANDLER_DATA (data);
  IF p^.BreakInx = eve.LastEvent.BreakpointInd THEN
    exe.StopExec;
    dex.PointExecLine (mem.GetIP ());
    std.Information ("Program is about to finish");
  END;
END ExitHandler;


<* END *>


<* IF TARGET_OS = "WINNT" THEN *>
VAR
  p: PSET_EXIT_HANDLER_DATA;
<* END *>

BEGIN
  erc.Ini ();

 <* IF TARGET_OS = "WINNT" THEN *>
  NEW (p);
  p^.BreakInx := ki.Invalid_Index;
  erc.AddAction (eve.CompCreated,   erc.DATA(p), SetExitHandler);
  erc.AddAction (eve.CompDestroyed, erc.DATA(p), RemoveExitHandler);
  erc.AddAction (eve.BreakpointHit, erc.DATA(p), ExitHandler);
 <* END *>

 <* IF DEST_XDS THEN *>
  hJavaException := win.Invalid_H;
  AskToStop := win.Invalid_H;
  LastTime := FALSE;
 <* ELSIF DEST_K26 THEN *>
  DontStop := FALSE;
  Counter  := 0;
  CallAddr := 0;
 <* END *>

  SaveBreak := NIL;
  pc := MAX(CARDINAL);
END DlgReact.
