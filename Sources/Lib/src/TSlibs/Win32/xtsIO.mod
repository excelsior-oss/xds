(* Copyright (C)1996-99 XDS Ltd. *)
(* IO : Win32 *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

<*-IOVERFLOW    *>
<*-COVERFLOW    *>
<*-CHECKINDEX   *>
<*-CHECKRANGE   *>
<*-CHECKNIL     *>

IMPLEMENTATION MODULE xtsIO;

IMPORT SYSTEM, W:=Windows;

PROCEDURE SetConMode(set : BOOLEAN; VAR tmp : SYSTEM.CARD32);
VAR
  hCon        : W.HANDLE;
  conOldState : W.CONSOLE_MODE_SET;
BEGIN
  hCon := W.GetStdHandle(W.STD_INPUT_HANDLE);
  IF (set) THEN
    W.GetConsoleMode(hCon, conOldState);
    W.SetConsoleMode(hCon, conOldState + W.ENABLE_LINE_INPUT + W.ENABLE_ECHO_INPUT + W.ENABLE_PROCESSED_INPUT);
    tmp := SYSTEM.CARD32(conOldState);
  ELSE
    W.SetConsoleMode(hCon, W.CONSOLE_MODE_SET(tmp));
  END;
END SetConMode;

BEGIN
END xtsIO.






