
<* Storage+ *>
IMPLEMENTATION MODULE Console;

IMPORT WIN := Windows;

IMPORT sys := SYSTEM;

IMPORT key := Keys;

FROM Printf IMPORT printf;

VAR
  KbdHndl: WIN.HANDLE;

  move_allowed: BOOLEAN;
  butt1, butt2: BOOLEAN;


  Scr: POINTER TO ARRAY OF WIN.CHAR_INFO;

  ScrHndl: WIN.HANDLE;
  SavHndl: WIN.HANDLE;

  Xmax, Ymax: CARDINAL;

  NeedToRestore, Dbg_Console: BOOLEAN;


PROCEDURE ConvertToInternal(k: WIN.KEY_EVENT_RECORD; VAR int_k: CARDINAL): BOOLEAN;
  TYPE
    CONV = ARRAY [0..255] OF CARDINAL;
  CONST

    ALT = CONV
          (* --0--  --1--  --2--   --3--    --4--   --5--   --6--   --7--   --8--    --9--   --A--  --B--   --C--   --D--   --E--  --F-- *)
  (* 0 *) {      0,      0,   key.Alt1,   key.Alt2,   key.Alt3,   key.Alt4,   key.Alt5,   key.Alt6,   key.Alt7,   key.Alt8,   key.Alt9,   key.Alt0,        0,      0,      0,      0,
  (* 1 *)     key.AltQ,   key.AltW,   key.AltE,   key.AltR,   key.AltT,   key.AltY,   key.AltU,   key.AltI,   key.AltO,   key.AltP,      0,      0, key.AltEnter,     0,   key.AltA,   key.AltS,
  (* 2 *)     key.AltD,   key.AltF,   key.AltG,   key.AltH,   key.AltJ,   key.AltK,   key.AltL,      0,      0,      0,      0,key.AltBSlash,   key.AltZ,   key.AltX,   key.AltC,   key.AltV,
  (* 3 *)     key.AltB,   key.AltN,   key.AltM,      0,      0,      0,      0,      0,      0,      0,      0,  key.AltF1,    key.AltF2,  key.AltF3,  key.AltF4,  key.AltF5,
  (* 4 *)    key.AltF6,  key.AltF7,  key.AltF8,  key.AltF9, key.AltF10,      0,      0,key.AltHome,  key.AltUp,key.AltPgUp,      0,key.AltLeft,        0,key.AltRight,     0, key.AltEnd,
  (* 5 *)  key.AltDown,key.AltPgDn, key.AltIns, key.AltDel,      0,      0,      0, key.AltF11, key.AltF12,      0,      0,      0,        0,      0,      0,      0,
  (* 6 *)        0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,        0,      0,      0,      0,
  (* 7 *)        0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,        0,      0,      0,      0,
  (* 8 *)        0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,        0,      0,      0,      0,
  (* 9 *)        0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,        0,      0,      0,      0,
  (* A *)        0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,        0,      0,      0,      0,
  (* B *)        0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,        0,      0,      0,      0,
  (* C *)        0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,        0,      0,      0,      0,
  (* D *)        0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,        0,      0,      0,      0,
  (* E *)        0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,        0,      0,      0,      0,
  (* F *)        0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,      0,        0,      0,      0,      0 };


    CTRL = CONV
          (*  --0--     --1--    --2--    --3--    --4--    --5--     --6--    --7--    --8--     --9--    --A--     --B--     --C--      --D--   --E--    --F-- *)
  (* 0 *) {       0,        0,       0,       0,       0,       0,        0,       0,       0,        0,       0,        0,        0,         0,      0, key.CtrlTab,
  (* 1 *)     key.CtrlQ,    key.CtrlW,   key.CtrlE,   key.CtrlR,   key.CtrlT,   key.CtrlY,    key.CtrlU,   key.CtrlI,   key.CtrlO,    key.CtrlP,       0,        0,key.CtrlEnter,         0,  key.CtrlA,   key.CtrlB,
  (* 2 *)     key.CtrlD,    key.CtrlF,   key.CtrlG,   key.CtrlH,   key.CtrlJ,   key.CtrlK,    key.CtrlL,       0,       0,        0,       0,key.CtrlBSlash,   key.CtrlZ,     key.CtrlX,  key.CtrlC,   key.CtrlV,
  (* 3 *)     key.CtrlB,    key.CtrlN,   key.CtrlM,       0,       0,       0,        0,       0,       0,key.CtrlSpace,       0,   key.CtrlF1,   key.CtrlF2,    key.CtrlF3, key.CtrlF4,  key.CtrlF5,
  (* 4 *)    key.CtrlF6,   key.CtrlF7,  key.CtrlF8,  key.CtrlF9, key.CtrlF10,       0,        0,key.CtrlHome,  key.CtrlUp, key.CtrlPgUp,       0, key.CtrlLeft,        0, key.CtrlRight,      0, key.CtrlEnd,
  (* 5 *)  key.CtrlDown, key.CtrlPgDn, key.CtrlIns, key.CtrlDel,       0,       0,        0, key.CtrlF11, key.CtrlF12,        0,       0,        0,        0,         0,      0,       0,
  (* 6 *)         0,        0,       0,       0,       0,       0,        0,       0,       0,        0,       0,        0,        0,         0,      0,       0,
  (* 7 *)         0,        0,       0,       0,       0,       0,        0,       0,       0,        0,       0,        0,        0,         0,      0,       0,
  (* 8 *)         0,        0,       0,       0,       0,       0,        0,       0,       0,        0,       0,        0,        0,         0,      0,       0,
  (* 9 *)         0,        0,       0,       0,       0,       0,        0,       0,       0,        0,       0,        0,        0,         0,      0,       0,
  (* A *)         0,        0,       0,       0,       0,       0,        0,       0,       0,        0,       0,        0,        0,         0,      0,       0,
  (* B *)         0,        0,       0,       0,       0,       0,        0,       0,       0,        0,       0,        0,        0,         0,      0,       0,
  (* C *)         0,        0,       0,       0,       0,       0,        0,       0,       0,        0,       0,        0,        0,         0,      0,       0,
  (* D *)         0,        0,       0,       0,       0,       0,        0,       0,       0,        0,       0,        0,        0,         0,      0,       0,
  (* E *)         0,        0,       0,       0,       0,       0,        0,       0,       0,        0,       0,        0,        0,         0,      0,       0,
  (* F *)         0,        0,       0,       0,       0,       0,        0,       0,       0,        0,       0,        0,        0,         0,      0,       0 };
BEGIN
  WITH k DO
(*
    io.WriteString('-------------------------');
    io.WriteLn;
    io.WriteString('KeyCode=');
    io.WriteHex(wVirtualKeyCode, 0);
    io.WriteLn;
    io.WriteString('ScanCode=');
    io.WriteHex(wVirtualScanCode, 0);
    io.WriteLn;
    io.WriteString('AsciiCode=');
    io.WriteHex(ORD(AsciiChar), 0);
    io.WriteLn;
    io.WriteString('Controls: ');
    IF WIN.ENHANCED__KEY IN dwControlKeyState       THEN io.WriteString('ENH_KEY ');    END;
    IF WIN.SHIFT__PRESSED IN dwControlKeyState      THEN io.WriteString('SHIFT ');      END;
    IF WIN.RIGHT__CTRL_PRESSED IN dwControlKeyState THEN io.WriteString('R_CONTROL ');  END;
    IF WIN.LEFT__CTRL_PRESSED IN dwControlKeyState  THEN io.WriteString('L_CONTROL ');  END;
    IF WIN.LEFT__key.Alt_PRESSED IN dwControlKeyState   THEN io.WriteString('L_key.Alt ');      END;
    IF WIN.RIGHT__key.Alt_PRESSED IN dwControlKeyState  THEN io.WriteString('R_key.Alt ');      END;
    io.WriteLn;
    io.WriteString('-------------------------');
    io.WriteLn;
*)
    dwControlKeyState := dwControlKeyState - WIN.NUMLOCK_ON - WIN.CAPSLOCK_ON - WIN.SCROLLLOCK_ON;
    IF (dwControlKeyState = WIN.CONTROLKEYSTATE_SET{}) THEN
      IF ORD(uChar.AsciiChar) # 0  THEN
        CASE wVirtualScanCode OF
        | 1CH:
          int_k := key.Enter;
        | 0EH:
          int_k := key.BackSpace;
        ELSE
          int_k := ORD(uChar.AsciiChar);
        END;
      ELSIF wVirtualScanCode = 1 THEN
        int_k := key.Esc;
      ELSE
        int_k := wVirtualScanCode*256;
      END;
      RETURN TRUE;
    ELSIF (WIN.SHIFT_PRESSED = dwControlKeyState) THEN
      IF (ORD(uChar.AsciiChar) # 0) AND (wVirtualScanCode # 42) THEN
        int_k := ORD(uChar.AsciiChar);
        RETURN TRUE;
      END;
    ELSIF (WIN.ENHANCED_KEY = dwControlKeyState) THEN
      IF ORD(uChar.AsciiChar) = 0  THEN
        int_k := wVirtualScanCode*256; RETURN TRUE;
      ELSIF wVirtualScanCode = 1CH THEN
        int_k := key.Enter; RETURN TRUE;
      END;
    ELSIF (WIN.ENHANCED_KEY+WIN.RIGHT_CTRL_PRESSED = dwControlKeyState) OR
          (WIN.ENHANCED_KEY+WIN.LEFT_CTRL_PRESSED  = dwControlKeyState) OR
          (WIN.RIGHT_CTRL_PRESSED                  = dwControlKeyState) OR
          (WIN.LEFT_CTRL_PRESSED                   = dwControlKeyState) THEN
      IF (wVirtualScanCode # 1DH) THEN
        int_k := CTRL[VAL(sys.CARD8,wVirtualScanCode)]; RETURN TRUE;
      END;
    ELSIF (WIN.ENHANCED_KEY+WIN.RIGHT_ALT_PRESSED = dwControlKeyState) OR
          (WIN.ENHANCED_KEY+WIN.LEFT_ALT_PRESSED  = dwControlKeyState) OR
          (WIN.RIGHT_ALT_PRESSED                  = dwControlKeyState) OR
          (WIN.LEFT_ALT_PRESSED                   = dwControlKeyState) THEN
      IF (wVirtualScanCode # 38H) THEN
        int_k := ALT[VAL(sys.CARD8,wVirtualScanCode)]; RETURN TRUE;
      END;
    ELSE
      IF ORD(uChar.AsciiChar) # 0  THEN
        int_k := ORD(uChar.AsciiChar); RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END ConvertToInternal;


PROCEDURE GetEvent(VAR event: EVENT);

  PROCEDURE ProcessMouseEvent(MouEvent: WIN.MOUSE_EVENT_RECORD): BOOLEAN;

    PROCEDURE AddEvent(action: MOU_ACTION);
    VAR
      tmp: WIN.SHORT;

    BEGIN
      WITH MouEvent.dwMousePosition DO
        IF X < 0 THEN X := 0; END;
        tmp := VAL(WIN.SHORT, Xmax)-1;
        IF X > tmp THEN X := tmp; END;
        IF Y < 0 THEN Y := 0; END;
        tmp := VAL(WIN.SHORT, Ymax)-1;
        IF Y > tmp THEN Y := tmp; END;
        event.x := X;
        event.y := Y;
      END;
      event.kind   := MouseEvent;
      event.action := action;
    END AddEvent;

  BEGIN
    WITH MouEvent DO
      IF (WIN.MOUSE__MOVED IN dwEventFlags) THEN
        IF move_allowed THEN
          AddEvent(Mou_Moved);
        ELSE
          RETURN FALSE;
        END;
      ELSE
        IF WIN.FROM__LEFT_1ST_BUTTON_PRESSED IN dwButtonState THEN
          IF WIN.DOUBLE__CLICK IN dwEventFlags THEN
            AddEvent(Mou_L_DoubleClick);
          ELSE
            butt1 := TRUE;
            AddEvent(Mou_L_Pressed);
            move_allowed := TRUE;
          END;
        ELSIF WIN.RIGHTMOST__BUTTON_PRESSED IN dwButtonState THEN
          butt2 := TRUE;
          AddEvent(Mou_R_Pressed);
        ELSE
          IF butt1 THEN
            butt1 := FALSE;
            move_allowed := FALSE;
            AddEvent(Mou_L_Released);
          ELSIF butt2 THEN
            butt2 := FALSE;
            AddEvent(Mou_R_Released);
          ELSE
            RETURN FALSE;
          END;
        END;
      END;
    END;
    RETURN TRUE;
  END ProcessMouseEvent;

VAR
  console_event: ARRAY [0..0] OF WIN.INPUT_RECORD;
  num  : CARDINAL;
BEGIN
  LOOP
    IF WIN.ReadConsoleInput(KbdHndl, console_event, 1, num) THEN
      ASSERT(num > 0);
      event.kind := None;
      CASE console_event[0].EventType OF
      | WIN.KEY_EVENT:
        IF console_event[0].Event.KeyEvent.bKeyDown THEN
          IF ConvertToInternal(console_event[0].Event.KeyEvent, event.key_code) THEN
            event.kind := KeyEvent;
            EXIT;
          END;
        END;
      | WIN.MOUSE_EVENT:
        IF ProcessMouseEvent(console_event[0].Event.MouseEvent) THEN
          ASSERT(event.kind # None);
          EXIT;
        END;
      | WIN.FOCUS_EVENT, WIN.MENU_EVENT:
      ELSE
        ASSERT(FALSE);
      END;
    ELSE
      printf ("Unexpected ReadConsoleInput error, error code %d.\n", WIN.GetLastError());
      HALT (1);
    END;
  END;
END GetEvent;


PROCEDURE QueryEvent(kind: EVENT_KIND): BOOLEAN;
VAR
  event  : ARRAY [0..0] OF WIN.INPUT_RECORD;
  num    : CARDINAL;
  int_key: CARDINAL;
BEGIN
  CASE kind OF
  | KeyEvent:
    num := 0;
    WIN.PeekConsoleInput(KbdHndl, event, 1, num);
    IF (num > 0) THEN
      WITH event[0] DO
        IF EventType = WIN.KEY_EVENT THEN
          IF Event.KeyEvent.bKeyDown & ConvertToInternal(Event.KeyEvent, int_key) THEN
            RETURN TRUE;
          END;
        END;
      END;
      WIN.ReadConsoleInput(KbdHndl, event, 1, num);
    END;
    RETURN FALSE;
  END;
END QueryEvent;


PROCEDURE Min(a,b: CARDINAL): CARDINAL;
BEGIN
  IF a > b THEN RETURN b ELSE RETURN a END;
END Min;

PROCEDURE IniConsole(VAR X, Y: CARDINAL): RESULT;
VAR
  ConScrBuffInfo: WIN.CONSOLE_SCREEN_BUFFER_INFO;
  Size          : WIN.COORD;
  srect         : WIN.SMALL_RECT;
BEGIN
  SavHndl := WIN.GetStdHandle(WIN.STD_OUTPUT_HANDLE);
  ScrHndl := WIN.CreateConsoleScreenBuffer(WIN.GENERIC_WRITE+WIN.GENERIC_READ, WIN.FILE_SHARE_MODE{}, NIL, WIN.CONSOLE_TEXTMODE_BUFFER, NIL);

  move_allowed := FALSE;

  IF WIN.GetConsoleScreenBufferInfo (SavHndl, ConScrBuffInfo) THEN
    WITH ConScrBuffInfo.dwMaximumWindowSize DO
      Xmax := X;
      Ymax := Y;
    END;
  ELSE
    IF X = 0 THEN
      Xmax := 80;
    ELSE
      Xmax := X;
    END;
    IF Y = 0 THEN
      Ymax := 25;
    ELSE
      Ymax := Y;
    END;
    SavHndl := NIL;
  END;

  WIN.SetConsoleMode(ScrHndl, WIN.ENABLE_MOUSE_INPUT);
  WIN.SetConsoleActiveScreenBuffer(ScrHndl);
  NeedToRestore := TRUE;
  IF (X # 0) AND (X # Xmax) THEN
    Xmax := X;
  END;
  IF (Y # 0) AND (Y # Ymax) THEN
    Ymax := Y;
  END;
  Size.X := Xmax; Size.Y := Ymax;
  IF WIN.SetConsoleScreenBufferSize(ScrHndl, Size) THEN
    Size := WIN.GetLargestConsoleWindowSize(ScrHndl);
    IF (Size.X = 0) AND (Size.Y = 0) THEN
      ASSERT(FALSE, WIN.GetLastError());
    END;
    WITH srect DO
      Left := 0; Right  := Min(Xmax - 1, Size.X - 1);
      Top  := 0; Bottom := Min(Ymax - 1, Size.Y - 1);
    END;
    IF NOT WIN.SetConsoleWindowInfo(ScrHndl, TRUE, srect) THEN
      ASSERT(FALSE, WIN.GetLastError());
    END;
  END;
  Dbg_Console := TRUE;

  X := Xmax; Y := Ymax;
  NEW(Scr, Xmax*Ymax);

  RETURN Ok;
END IniConsole;

PROCEDURE CloseConsole();
BEGIN
  IF SavHndl # NIL THEN
    WIN.SetConsoleActiveScreenBuffer(SavHndl);
  END;
  WIN.CloseHandle(ScrHndl);
  NeedToRestore := FALSE;
END CloseConsole;

<* PUSH *>
<* WOFF301+ *>
PROCEDURE ToggleConsoles(screen: LOGICAL_SCREEN);
<* POP *>
BEGIN
  IF Dbg_Console THEN
    IF SavHndl # NIL THEN
      WIN.SetConsoleActiveScreenBuffer(SavHndl);
    END;
  ELSE
    WIN.SetConsoleActiveScreenBuffer(ScrHndl);
  END;
  Dbg_Console := NOT Dbg_Console;
END ToggleConsoles;

PROCEDURE OutSymbol(x,y: CARDINAL; ch: CHAR; attr: ATTR);
VAR
  c  : WIN.COORD;
  num: CARDINAL;
  atr: ARRAY [0..0] OF WIN.CHAR_ATTRIBUTES_SET;
  chr: ARRAY [0..0] OF CHAR;
BEGIN
  c.X := x;
  c.Y := y;
  atr [0] := sys.CAST(WIN.CHAR_ATTRIBUTES_SET, attr);
  chr [0] := ch;
  WIN.WriteConsoleOutputCharacter(ScrHndl, chr, 1, c, num);
  WIN.WriteConsoleOutputAttribute(ScrHndl, atr, 1, c, num);
END OutSymbol;

PROCEDURE OutRect(screen: LOGICAL_SCREEN; x1, y1, x2, y2: CARDINAL);
VAR
  c1, c2 : WIN.COORD;
  r      : WIN.SMALL_RECT;

  pos, y, x: CARDINAL;
BEGIN
  FOR y := y1 TO y2 DO
    pos := y*Xmax+x1;
    FOR x := x1 TO x2 DO
      Scr^[pos].Char.AsciiChar  := screen^[pos].ch;
      Scr^[pos].Attributes := sys.CAST(WIN.CHAR_ATTRIBUTES_SET, screen^[pos].attr);
      INC(pos);
    END;
  END;

  WITH r DO
    Left := x1; Top    := y1;
    Right:= x2; Bottom := y2;
  END;
  c1.X := Xmax;  c1.Y := Ymax;
  c2.X := r.Left; c2.Y := r.Top;
  ASSERT(WIN.WriteConsoleOutput(ScrHndl, Scr^, c1, c2, r));
END OutRect;

PROCEDURE Flush();
BEGIN
END Flush;

PROCEDURE Beep;
BEGIN
  WIN.Beep(500,  20);
  WIN.Beep(1000, 150);
END Beep;

PROCEDURE CursorOff;
VAR
  ci: WIN.CONSOLE_CURSOR_INFO;
BEGIN
  WIN.GetConsoleCursorInfo(ScrHndl, ci);
  ci.bVisible := FALSE;
  WIN.SetConsoleCursorInfo(ScrHndl, ci);
  IF SavHndl # NIL THEN
    WIN.GetConsoleCursorInfo(SavHndl, ci);
    ci.bVisible := FALSE;
    WIN.SetConsoleCursorInfo(SavHndl, ci);
  END;
END CursorOff;

PROCEDURE CursorOn;
VAR
  ci: WIN.CONSOLE_CURSOR_INFO;
BEGIN
  WIN.GetConsoleCursorInfo(ScrHndl, ci);
  ci.bVisible := TRUE;
  WIN.SetConsoleCursorInfo(ScrHndl, ci);
  IF SavHndl # NIL THEN
    WIN.GetConsoleCursorInfo(SavHndl, ci);
    ci.bVisible := TRUE;
    WIN.SetConsoleCursorInfo(SavHndl, ci);
  END;
END CursorOn;

PROCEDURE SetCurPos(x,y: CARDINAL);
VAR
  cp: WIN.COORD;
BEGIN
  cp.X := x;
  cp.Y := y;
  WIN.SetConsoleCursorPosition(ScrHndl, cp);
END SetCurPos;

PROCEDURE CursorState(): BOOLEAN;
VAR
  ci: WIN.CONSOLE_CURSOR_INFO;
BEGIN
  WIN.GetConsoleCursorInfo(ScrHndl, ci);
  RETURN ci.bVisible;
END CursorState;

(* Delay d fraction second (1 fraction = 1/100 second) *)
PROCEDURE Delay (d: CARDINAL);
BEGIN
  WIN.Sleep (d*10);
END Delay;


TYPE
  HANDLE = POINTER TO RECORD END;

BEGIN
  SavHndl := NIL;
  KbdHndl := WIN.GetStdHandle(WIN.STD_INPUT_HANDLE);
  WIN.SetConsoleMode(KbdHndl, WIN.ENABLE_PROCESSED_INPUT+WIN.ENABLE_MOUSE_INPUT);
FINALLY
  IF NeedToRestore THEN
    CloseConsole;
  END;
END Console.
