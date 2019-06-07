MODULE XD_Demon;


IMPORT sys := SYSTEM;
IMPORT arg := ProgEnv;
IMPORT fmt := FormStr;
IMPORT fn  := FileName;

IMPORT win := Windows;

IMPORT Printf;


IMPORT xs  := xStr;
IMPORT opt := Options;

IMPORT med := Media;
IMPORT rmt := RemTypes;


VAR
  Channel: rmt.TRN_CHANNEL;
  My_HWND: win.HWND;



PROCEDURE Help;
BEGIN
  Printf.printf ("Usage:   xd_lnch [ ('-'|'/') options ]\n");
  Printf.printf ('\nOptions: R=<transport>       use transport\n');
  HALT (0);
END Help;


PROCEDURE Init;
BEGIN
  Channel := rmt.INVALID_CHANNEL;
  My_HWND := win.HWND (MAX(CARDINAL));
END Init;


CONST
  My_Title = "XDS Remote Debugger";

VAR
  Tmp_My_Title: xs.String;

<* PUSH *>
<* WOFF301+ *>
PROCEDURE [win.CALLBACK] Catch_MyHWND (hwnd: win.HWND; par: win.LPARAM): win.BOOL;
<* POP *>
VAR
  txt: xs.String;
BEGIN
  txt[0] := 0C;
  win.GetWindowText (hwnd, txt, SIZE(txt));
  IF txt # Tmp_My_Title THEN
    RETURN TRUE;
  ELSE
    My_HWND := hwnd;
    RETURN FALSE;
  END;
END Catch_MyHWND;



PROCEDURE Find_MyHWND;
BEGIN
  My_HWND := win.HWND(MAX(CARDINAL));
  fmt.print (Tmp_My_Title, "%s (%x)", My_Title, win.GetCurrentProcessId());
  win.SetConsoleTitle (Tmp_My_Title);
  win.Sleep (40);
  win.EnumWindows (Catch_MyHWND, 0);
  ASSERT (My_HWND # win.HWND(MAX(CARDINAL)));
  win.SetConsoleTitle (My_Title);
END Find_MyHWND;



PROCEDURE Options (s-: ARRAY OF CHAR): BOOLEAN;
VAR
  ErrorInOptions: BOOLEAN;

  pos  : CARDINAL;
  found: BOOLEAN;
BEGIN
  CASE CAP(s[1]) OF
  |'R': ErrorInOptions := TRUE;
        IF s[2] = '=' THEN
         <* PUSH *>
         <* WOFF903+ *>
          pos := xs.CharPos (s, ' ', found);
         <* POP *>
          IF NOT found THEN
            xs.Extract (s, 3, MAX(CARDINAL), opt.RemoteTransport);
            ErrorInOptions := FALSE;
          END;
        END;
  ELSE
    ErrorInOptions := TRUE;
  END;
  RETURN NOT ErrorInOptions;
END Options;


PROCEDURE ParseCommandLine;
VAR
  i, k: CARDINAL;
  a: xs.String;
BEGIN
   k := arg.ArgNumber();
   IF k = 0 THEN Help; END;
   i := 0;
   LOOP                                   (* Разбор опций *)
     IF (i = k) THEN EXIT; END;
     arg.GetArg(i, a);
     IF (a[0] = '/') OR (a[0] = '-') THEN (* Опции начинаются с '-' или '/' *)
       IF NOT Options (a) THEN Help; END;
     ELSE
       EXIT;
     END;
     INC(i);
   END;
END ParseCommandLine;


PROCEDURE Listening;
VAR
  rc: CARDINAL;
BEGIN
  Printf.printf ('\nWait, listening...');
  rc := med.MasterListen (opt.RemotePort, Channel);
  IF rc # 0 THEN
    Printf.printf ('\nError %u', rc);
    HALT (rc);
  END;
  ASSERT (Channel # rmt.INVALID_CHANNEL);
END Listening;




PROCEDURE StartServer;
VAR
  progname, path: xs.String;
  Title, Cmd_Line: xs.String;
  startup_info   : win.STARTUPINFO;
  pi: win.PROCESS_INFORMATION;
BEGIN
  arg.ProgramName (progname);
  fn.GetDir (progname, path);
  fmt.print (Cmd_Line, "%s\xd_srv.exe /r=%s /d /c=%$8X", path, opt.RemoteTransport, Channel);

  COPY ("XDS Remote Server", Title);

  WITH startup_info DO
    cb              := SIZE(startup_info);
    lpReserved      := NIL;
    lpDesktop       := NIL;
    lpTitle         := sys.ADR (Title);
    dwFlags         := win.STARTF_SET{};
    cbReserved2     := 0;
    lpReserved2     := NIL;
  END;

  IF NOT win.CreateProcess ( NIL                       -- address of module name
                           , sys.ADR(Cmd_Line)         -- address of command line
                           , NIL                       -- address of process security attributes
                           , NIL                       -- address of thread security attributes
                           , TRUE                      -- new process inherits handles
                           , win.NORMAL_PRIORITY_CLASS -- creation flags
--                           + win.CREATE_NEW_CONSOLE+win.CREATE_NEW_PROCESS_GROUP
                           , NIL                       -- address of new environment block
                           , NIL                       -- address of current directory name
                           , startup_info              -- address of STARTUPINFO
                           , pi)                       -- address of PROCESS_INFORMATION
  THEN
    Printf.printf ('\nCannot create process.');
  END;

(*
  IF NOT win.DuplicateHandle ( win.HANDLE (My_HWND)
                             , win.HANDLE (Channel)
                             , pi.hProcess
                             , NIL
                             , win.ACCESS_MASK {}
                             , TRUE
                             , win.DUPLICATE_SAME_ACCESS
                             )
  THEN
    Printf.printf ('\nCannot create process.');
  END;
*)

  win.CloseHandle (win.HANDLE (Channel));
END StartServer;


BEGIN
  Find_MyHWND;
  Init;
  ParseCommandLine;
  LOOP
    Listening;
    StartServer;
  END;
END XD_Demon.
