IMPLEMENTATION MODULE Krn_Dbg;

IMPORT win := Windows;
IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT opt := Options;
IMPORT xs  := xStr;


VAR
  My_PID : CARDINAL;
  H_Title: ARRAY [0..255] OF CHAR;

<* PUSH *>
<* WOFF301+ *>
PROCEDURE [win.CALLBACK] Find_MyHWND(hwnd: win.HWND; par: win.LPARAM): win.BOOL;
<* POP *>
VAR
  txt: ARRAY [0..255] OF CHAR;
BEGIN
  txt[0]:=0C;
  win.GetWindowText(hwnd, txt, SIZE(txt));
  IF txt # H_Title THEN 
    RETURN TRUE
  ELSE
    My_HWND := hwnd;
    RETURN FALSE;
  END;
END Find_MyHWND;


<* PUSH *>
<* WOFF301+ *>
PROCEDURE [win.CALLBACK] Iter_Debuggee_HWND(hwnd: win.HWND; par: win.LPARAM): win.BOOL;
<* POP *>
VAR
  pid: CARDINAL;
BEGIN
  pid := 0;
  win.GetWindowThreadProcessId(hwnd, pid);
  IF pid # Id_Process THEN
    RETURN TRUE;
  ELSE
    Debuggee_HWND := hwnd;
    RETURN FALSE;
  END;
END Iter_Debuggee_HWND;


PROCEDURE Find_Debuggee_HWND();
BEGIN
  Debuggee_HWND := win.HWND(MAX(CARDINAL));
  win.EnumWindows(Iter_Debuggee_HWND, 0);
END Find_Debuggee_HWND;


VAR
  OS_ver: win.OSVERSIONINFO;

  in_debugger: BOOLEAN;

PROCEDURE StartSmartSelector;
BEGIN
  -- только в диалоге, но не при удаленной отладке
--  IF opt.in_dialog AND NOT opt.KernelInRemoteMode THEN
--  END;
END StartSmartSelector;


PROCEDURE StopSmartSelector;
BEGIN
  -- только в диалоге, но не при удаленной отладке
--  IF opt.in_dialog AND NOT opt.KernelInRemoteMode THEN
--  END;
END StopSmartSelector;


PROCEDURE SwitchToDebuggee;
BEGIN
  IF opt.in_dialog AND NOT opt.KernelInRemoteMode AND in_debugger THEN
    IF Debuggee_HWND = win.HWND(MAX(CARDINAL)) THEN
      Find_Debuggee_HWND;
    END;
    IF Debuggee_HWND # win.HWND(MAX(CARDINAL)) THEN
      win.SetForegroundWindow(Debuggee_HWND);
      in_debugger := FALSE;
    END;
  END;
END SwitchToDebuggee;


PROCEDURE SwitchToDebugger;
BEGIN
  IF opt.in_dialog AND (My_HWND # win.HWND(MAX(CARDINAL))) AND NOT opt.KernelInRemoteMode THEN
    win.SetForegroundWindow (My_HWND);
    in_debugger := TRUE;
  END;
END SwitchToDebugger;


PROCEDURE SetTitle (title-: ARRAY OF CHAR);
BEGIN
  win.SetConsoleTitle (title);
END SetTitle;

VAR
  SelfTitle: xs.String;

BEGIN
  SelfTitle := "XD Debugger for Win32";

  My_HWND := win.HWND(MAX(CARDINAL));
  Debuggee_HWND := My_HWND;
  
  My_PID := win.GetCurrentProcessId();
  
  fmt.print(H_Title, '%s: %x', SelfTitle, My_PID);

  win.SetConsoleTitle(H_Title);
  win.Sleep(40);
  win.EnumWindows(Find_MyHWND, 0);
  win.SetConsoleTitle (SelfTitle);

  OS_ver.dwOSVersionInfoSize := SIZE(OS_ver);
  IF NOT win.GetVersionEx(OS_ver) THEN
    ASSERT(FALSE, win.GetLastError());
  END;
  WITH OS_ver DO
    CASE dwPlatformId OF
    | win.VER_PLATFORM_WIN32_WINDOWS:
      opt.TargetSystem := opt.win95;
    | win.VER_PLATFORM_WIN32_NT:
      opt.TargetSystem := opt.win_nt;
    END;
  END;
  in_debugger := TRUE;
END Krn_Dbg.
