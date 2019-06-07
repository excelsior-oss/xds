IMPLEMENTATION MODULE Dlg_Log;

IMPORT sys := SYSTEM;
IMPORT ioc := IOChan;
IMPORT seq := SeqFile;
IMPORT tio := TextIO;
IMPORT fmt := FormStr;

IMPORT txt := Texts;
IMPORT xs  := xStr;
IMPORT opt := Options;
IMPORT fil := File;

IMPORT kt  := KrnTypes;

IMPORT crt := CRT;
IMPORT act := Dlg_Acts;
IMPORT brw := DlgBrows;
IMPORT std := Dlg_Std;
IMPORT win := Dlg_Win;
IMPORT eve := DlgEvent;


VAR
  log: txt.TEXT;


PROCEDURE Write (s: ARRAY OF CHAR; SEQ args: sys.BYTE);
BEGIN
  txt.AddLine (log, s, args);
END Write;


PROCEDURE WriteLn (s: ARRAY OF CHAR; SEQ args: sys.BYTE);
BEGIN
  Write (s, args);
  txt.AddLine (log, "");
END WriteLn;


PROCEDURE New;
BEGIN
  txt.Close (log);
  txt.New (log);
  WriteLn ("; XDS Debugger Log file");
END New;


VAR
  BatchFileName: xs.String;


PROCEDURE CreateFile;
VAR
  file: ioc.ChanId;
  open: BOOLEAN;
  res : seq.OpenResults;
  s   : xs.txt_ptr;
  i   : CARDINAL;
BEGIN
  seq.OpenWrite (file, BatchFileName, seq.write+seq.text+seq.old, res);
  open := res = seq.opened;
  IF open THEN
    txt.SetName (log, BatchFileName);
    FOR i := 0 TO txt.LastLine(log)-1 DO
      txt.GetLine (log, i, s);
      tio.WriteString (file, s^);
      tio.WriteLn (file);
    END;
    seq.Close (file);
  ELSE
    std.Error ('Error open file');
  END;
END CreateFile;


PROCEDURE SaveToFile;
BEGIN
  COPY(opt.prog_name, BatchFileName);
  fil.RemoveExtension (BatchFileName);
  xs.Append ("_log", BatchFileName);
  fil.AddExtension (BatchFileName, kt.pkt_file_ext);
  brw.Browse ("Save log to batch", BatchFileName, CreateFile);
END SaveToFile;


PROCEDURE LocatorLog (hwnd: crt.HWND; i: CARDINAL; VAR str: ARRAY OF CHAR);
BEGIN
  ASSERT(hwnd # win.Invalid_H);
  fmt.print (str, "%i", i+1);
END LocatorLog;


PROCEDURE HandlerLog (hwnd: win.HWND; msg: eve.MSG);
VAR
  p   : std.PLIST;
  size: crt.SZ;

  PROCEDURE write_line (num: CARDINAL);
  VAR
    line: xs.txt_ptr;
  BEGIN
    WITH p^ DO
      txt.GetLine (log, num, line);
      crt.SetPos(2, num-frame + 1);
      crt.WrStrFromPos (hwnd, line^, Colors^[crt.List_Line], pos);
    END;
  END write_line;

VAR
  i, len: CARDINAL;
  last  : CARDINAL;
  x, y  : CARDINAL;
  N     : CARDINAL;

BEGIN
  size := win.GetWindowSize(hwnd);
  N := txt.LastLine(log);
  p := win.GetAMPtr(hwnd);
  IF N # p^.N THEN
    p^.N := N;
    p^.curr := N-1;
    std.Normalize (size, p^.curr, p^.frame, p^.N);
  END;
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
    std.ListBox(hwnd, msg);

  | eve.QueryAction:
    CASE act.ACTION(msg.par MOD 100H) OF
    | act.Log:
      act.ConfirmQuery;
    ELSE
      std.ListBox (hwnd, msg);
    END;
(*
  | eve.R_Mouse:
    ASSERT(win.GetMouse(msg, x, y));
    size := win.GetWindowSize(hwnd);
    IF NOT std.CheckFrame(size, x, y) THEN
      eve.AddToTail(hwnd, eve.Mouse_Pressed, msg.par);
      eve.Flush;
      ???
      puw.PopupWindow(x, y, hwnd, p^.actions);
    END;
*)
  | eve.KbHit:
    CASE msg.par OF
    ELSE
      std.ListBox(hwnd, msg);
    END;
  ELSE
    std.ListBox(hwnd, msg);
  END;
END HandlerLog;


VAR
  LogHwnd: win.HWND;

PROCEDURE InitLogWindow (show: BOOLEAN);
VAR
  p   : std.PLIST;
  size: crt.SZ;
BEGIN
  IF LogHwnd = win.Invalid_H THEN
    LogHwnd := win.RegisterWindow (HandlerLog, SIZE(std.LIST));
    ASSERT(LogHwnd # win.Invalid_H);
    WITH size DO
      x2 := crt.Xmax-1; y2 := crt.Ymax-1;
      x1 := 0;          y1 := y2-5;
    END;
    win.SetWindowSize (LogHwnd, size);
    win.SetMovable (LogHwnd);
    win.SetResizable (LogHwnd, TRUE, TRUE);
    win.SetSwitchable (LogHwnd);
    win.SetHeaderByStr (LogHwnd, act.ActionName[act.Log]);
    p := win.GetAMPtr (LogHwnd);
    p^ := std.EmptyList;
    WITH p^ DO
      Colors     := sys.ADR(crt.List);
      locator    := LocatorLog;
      ext        := sys.ADDADR(sys.ADR(p^), SIZE(std.LIST));
    END;
  END;
  IF show THEN
    eve.AddToTail (LogHwnd, eve.Rise, 0);
  END;
END InitLogWindow;


PROCEDURE InitLogList (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Log);
  IF NOT opt.DialogMode THEN
    RETURN FALSE;
  END;
  InitLogWindow (mode # act.mode_check);
  IF mode = act.mode_check THEN
    RETURN std.QueryAction (LogHwnd, act.Log);
  ELSE
    RETURN TRUE;
  END;
END InitLogList;


CONST
  LABEL = 'LABEL';


VAR
  ProgramStarted: BOOLEAN;
  LabelCount    : CARDINAL;


PROCEDURE SaveToLog (action: act.ACTION; SEQ args: sys.BYTE);

  PROCEDURE next;
  BEGIN
    INC(LabelCount);
  END next;

  PROCEDURE label;
  BEGIN
    Write ('%s%i', LABEL, LabelCount);
  END label;

  PROCEDURE goto;
  BEGIN
    Write ('  GOTO %s%i', LABEL, LabelCount);
  END goto;


BEGIN
  Write ("; %s", act.ActionName[action]);
  CASE action OF
  | act.Load:
    WriteLn ('  LOAD "%s" %s', opt.prog_name, opt.prog_args);
  | act.Quit:
    WriteLn ('  QUIT');
  | act.Halt:
    WriteLn ('  QUIT');
  | act.Run:
    ProgramStarted := TRUE;
    WriteLn ('  START');
    label;
    next;
  | act.Restart:
    IF ProgramStarted THEN
      next;
      goto;
      Write ('  STOP');
      label;
      ProgramStarted := FALSE;
    END;
    WriteLn ('  RESTART');
(*
  | act.Animation:
    Write   ('  MODE TRACE+');
    WriteLn ('  START');
  -- dlg_exec.DoGo;
  | act.Into:
  | act.Over:
  | act.UptoRet:
  | act.UptoEpilog:
  | act.Skip :
  | act.UptoAddr:
  -- dlg_exec.DoGo;

  | act.Step into
  | act.Step over
  | act.Step out
  | act.Run to cursor
  | act.Run to epilogue
  | act.Run until address
  | act.Restart at startup

  | act.Sticky breakpoint
  | act.Breakpoint
  | act.Delayed sticky breakpoint
  | act.Delayed breakpoint
  | act.Expression breakpoint
  | act.Pass counter
  | act.Watchpoint
  | act.Access break
  | act.Condition break
  | act.Disable
  | act.Enable
  | act.Delete
  | act.Disable all breaks
  | act.Enable all breaks
  | act.Erase all breaks

  | act.Source
  | act.Disassembly
  | act.Mixed
  | act.Call stack
  | act.Components
  | act.Modules
  | act.Procedures
  | act.Publics
  | act.Threads
  | act.Examine
  | act.Evaluate expression
  | act.Global variables
  | act.Module variables
  | act.Local variables
  | act.Module types
  | act.Memory dump
  | act.Stack
  | act.Registers
  | act.Float registers
  | act.Write watch
  | act.Del watch
  | act.Show watch window
  | act.Del all watches
  | act.Aliases
*)
  ELSE
  END;
END SaveToLog;



BEGIN
  log := txt.nil;
  New;
  ProgramStarted := FALSE;
  LabelCount := 1;
  LogHwnd := win.Invalid_H;
  act.IniAction (act.Log, InitLogList);
END Dlg_Log.