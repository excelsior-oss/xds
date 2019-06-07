IMPLEMENTATION MODULE Console;

FROM Storage IMPORT ALLOCATE;

IMPORT sys := SYSTEM;

IMPORT key := Keys;
IMPORT utl := Krn_Dbg;

IMPORT OS2;

<* PUSH *> <* VOLATILE+ *>
VAR
  dbl_time: CARDINAL;
  Event   : EVENT;
<* POP *>

  EventHappened  : OS2.HEV;
  mouse_processed: OS2.HEV;
  FillEvent      : OS2.HMTX; (* выставляется, когда очередь готова принять что-то от клав. или мыши *)

  hmou   : OS2.HMOU;
  mask   : OS2.USHORT;
  MH_tid : OS2.TID;    (* Thread ID for mouse handler *)

<* PUSH *>
<* WOFF301+ *>
PROCEDURE ["SysCall"] MouseHandler(par: OS2.ULONG);
<* POP *>

  PROCEDURE AddEvent(event: OS2.MOUEVENTINFO; action: MOU_ACTION);
  VAR
    count  : CARDINAL;
  BEGIN
    OS2.DosRequestMutexSem(FillEvent, OS2.SEM_INDEFINITE_WAIT);
    WITH event DO
      Event.kind   := MouseEvent;
      Event.action := action;
      Event.x      := col;
      Event.y      := row;
    END;

    OS2.DosPostEventSem(EventHappened);
    OS2.DosReleaseMutexSem(FillEvent);

    OS2.DosWaitEventSem(mouse_processed, OS2.SEM_INDEFINITE_WAIT);
    OS2.DosResetEventSem(mouse_processed, count);
  END AddEvent;

VAR
  FirstClick  : BOOLEAN;
  butt1, butt2: BOOLEAN;
  too_long    : BOOLEAN;

  wait  : OS2.USHORT;
  event : OS2.MOUEVENTINFO;

  tm    : OS2.ULONG;
  old_col, old_row: OS2.SHORT;

BEGIN
  wait    := OS2.MOU_WAIT;
<* PUSH *>
<* WOFF310+ *>
  old_row := MAX(OS2.SHORT);
  old_col := MAX(OS2.SHORT);
  butt1 := FALSE; butt2 := FALSE;
  FirstClick := FALSE;
  tm := 0;
  LOOP
    ASSERT(OS2.MouReadEventQue(event, wait, hmou) = 0);
    WITH event DO
      CASE fs OF
      | OS2.MOUSE_BN1_DOWN:
        butt1 := TRUE;
        AddEvent(event, Mou_L_Pressed);
        too_long := (time-tm > dbl_time);
        IF NOT FirstClick OR too_long THEN
          tm := time; old_col := col; old_row := row;
          IF too_long THEN
            FirstClick := FALSE;
          END;
        END;
      | OS2.MOUSE_BN2_DOWN:
        butt2 := TRUE;
        AddEvent(event, Mou_R_Pressed);
      | OS2.MOUSE_MOTION_WITH_BN1_DOWN:
        AddEvent(event, Mou_Moved);
      ELSE
        IF (butt1) AND NOT (2 IN sys.SET16(fs)) THEN
          IF FirstClick & (time-tm <= dbl_time) & (row=old_row) & (col = old_col) THEN
            AddEvent(event, Mou_L_DoubleClick);
          ELSE
            AddEvent(event, Mou_L_Released);
          END;
          FirstClick := NOT FirstClick;
          butt1 := FALSE;
        END;
        IF (butt2) AND NOT (4 IN sys.SET16(fs)) THEN
          butt2 := FALSE;
          AddEvent(event, Mou_R_Released);
        END;
      END;
    END;
  END;
<* POP *>
END MouseHandler;

VAR
  KH_tid: CARDINAL;
  Key_Empty, Key_Taken: OS2.HEV;

PROCEDURE GetKey_OS(): CARDINAL;
  VAR
    kb: OS2.KBDKEYINFO;
    k: CARDINAL;
BEGIN
  OS2.KbdCharIn(kb, OS2.IO_WAIT, 0);
  IF kb.chChar = CHR(224) THEN
    k := 0
  ELSE
    k := ORD(kb.chChar);
  END;
  CASE kb.chScan OF
  | CHR(1CH), CHR(0E0H):
    CASE k OF
    | key.CtrlM : k := key.Enter;
    | key.CtrlJ : k := key.CtrlEnter;
    END;
  | CHR(0EH):
    k := key.BackSpace;
  ELSE
  END;
  IF k = 0 THEN
    k := ORD(kb.chScan)*100H;
  END;
  RETURN k;
END GetKey_OS;


<* PUSH *>
<* WOFF301+ *>
PROCEDURE ["SysCall"] KeyHandler(par: OS2.ULONG);
<* POP *>
VAR
  count: OS2.ULONG;
  Key  : CARDINAL;
BEGIN
<* PUSH *>
<* WOFF310+ *>
  LOOP
    OS2.DosWaitEventSem (Key_Empty, OS2.SEM_INDEFINITE_WAIT);
    OS2.DosResetEventSem(Key_Empty, count);
    OS2.DosResetEventSem(Key_Taken, count);
    Key := GetKey_OS();
    OS2.DosPostEventSem(Key_Taken);
    OS2.DosRequestMutexSem(FillEvent,     OS2.SEM_INDEFINITE_WAIT);
    Event.kind     := KeyEvent;
    Event.key_code := Key;
    OS2.DosPostEventSem(EventHappened);
    OS2.DosReleaseMutexSem(FillEvent);
  END;
<* POP *>
END KeyHandler;

PROCEDURE GetEvent(VAR event: EVENT);
VAR
  count: CARDINAL;
BEGIN
  OS2.DosReleaseMutexSem(FillEvent);
  OS2.DosWaitEventSem   (EventHappened, OS2.SEM_INDEFINITE_WAIT);
  OS2.DosResetEventSem  (EventHappened, count);
  OS2.DosRequestMutexSem(FillEvent,     OS2.SEM_INDEFINITE_WAIT);
  event := Event;
  Event.kind := None;
  CASE event.kind OF
  | KeyEvent:
    OS2.DosPostEventSem(Key_Empty);
  | MouseEvent:
    OS2.DosPostEventSem(mouse_processed);
  ELSE
    ASSERT(FALSE, CARDINAL(event.kind));
  END;
END GetEvent;


VAR
  KbdThread_Started: BOOLEAN;

PROCEDURE QueryEvent(kind: EVENT_KIND): BOOLEAN;
VAR
  count: CARDINAL;
BEGIN
  CASE kind OF
  | KeyEvent:
    IF KbdThread_Started THEN
      OS2.DosQueryEventSem(Key_Taken, count);
      RETURN count # 0;
    ELSE
      RETURN FALSE;
    END;
  END;
END QueryEvent;

VAR
  Save_vioModeInfo: OS2.VIOMODEINFO;
  Saved : BOOLEAN;
  Buffer: sys.ADDRESS;
  Xmax, Ymax: CARDINAL;
  Xcur, Ycur: sys.CARD16;
  Scr: POINTER TO ARRAY [0..0FFFFH] OF CELL;
  SaveScreen: sys.ADDRESS;
  BufferSize: CARDINAL;
  Dbg_Console: BOOLEAN;

PROCEDURE IniConsole(VAR X, Y: CARDINAL): RESULT;
VAR
  vioModeInfo: OS2.VIOMODEINFO;
  swp: OS2.SWP;
  new_cx, new_cy: CARDINAL;
  N : sys.CARD16;
  tmp: sys.ADDRESS;
BEGIN
  OS2.VioGetCurPos(Ycur, Xcur, 0);
  vioModeInfo.cb := SIZE(OS2.VIOMODEINFO);
  OS2.VioGetMode(vioModeInfo, 0);
  WITH vioModeInfo DO
    Xmax  := col; Ymax  := row;
  END;

  BufferSize := Xmax * Ymax * 2;

  IF Buffer = NIL THEN
    ALLOCATE(Buffer, 20000H);
  END;
  IF (CARDINAL(Buffer) MOD 10000H) # 0 THEN
    tmp := sys.ADDRESS(((CARDINAL(Buffer) DIV 10000H) + 1)*10000H);
  ELSE
    tmp := Buffer;
  END;
  Scr         := tmp; tmp := sys.ADDADR(tmp, BufferSize);
  SaveScreen  := tmp;

  N := BufferSize;
  OS2.VioReadCellStr(SaveScreen^, N, 0, 0, 0);
  ASSERT(N=BufferSize);

  Saved := FALSE;
  IF X = 0 THEN X := Xmax END;
  IF Y = 0 THEN Y := Ymax END;

  IF ( X < 80 ) OR ( Y < 25 ) THEN RETURN WrongParams; END;

  IF (X # Xmax) OR (Y # Ymax) THEN
    Saved := TRUE;
    Save_vioModeInfo := vioModeInfo;
    WITH vioModeInfo DO
      col := X;
      row := Y;
      buf_length  := col*row*2;
      full_length := buf_length;
    END;
    OS2.WinQueryWindowPos(utl.MyHwnd, swp);
    WITH Save_vioModeInfo DO
      new_cx := ORD(swp.cx DIV col) * X + ORD(swp.cx MOD col);
      new_cy := ORD(swp.cy DIV row) * Y + ORD(swp.cy MOD row);
      OS2.WinSetWindowPos(utl.MyHwnd, 0, 0, 0, new_cx, new_cy, OS2.SWP_SIZE+OS2.SWP_HIDE);
    END;
    IF OS2.VioSetMode(vioModeInfo, 0) # 0 THEN
      Saved := FALSE;
      RETURN Error;
    END;
    OS2.WinSetWindowPos(utl.MyHwnd, 0, 0, 0, new_cx, new_cy, OS2.SWP_SIZE+OS2.SWP_SHOW);
    OS2.WinInvalidateRect(utl.MyHwnd, NIL, TRUE);
    OS2.WinUpdateWindow(utl.MyHwnd);
  END;
  Xmax := X; Ymax := Y;

  OS2.DosCreateEventSem(NIL, Key_Empty,   0, TRUE);
  OS2.DosCreateEventSem(NIL, Key_Taken,   0, FALSE);

  OS2.DosCreateThread(KH_tid, KeyHandler, 0, 0, 32000);
  KbdThread_Started := TRUE;

  OS2.MouOpen(NIL,  hmou);
  mask := OS2.MOUSE_BN1_DOWN + OS2.MOUSE_MOTION_WITH_BN1_DOWN + OS2.MOUSE_BN2_DOWN;
  OS2.MouSetEventMask(mask, hmou);

  OS2.DosCreateEventSem(NIL, EventHappened,   0, FALSE);
  OS2.DosCreateEventSem(NIL, mouse_processed, 0, TRUE);
  OS2.DosCreateMutexSem(NIL, FillEvent, 0, TRUE);

  OS2.DosCreateThread(MH_tid, MouseHandler, 0, 0, 0FFFFH);


  Dbg_Console := TRUE;
  RETURN Ok;
END IniConsole;

PROCEDURE CloseConsole();
VAR
  vioModeInfo: OS2.VIOMODEINFO;
BEGIN
  OS2.DosKillThread(MH_tid);

  OS2.DosCloseEventSem(EventHappened);
  OS2.DosCloseEventSem(mouse_processed);
  OS2.DosCloseMutexSem(FillEvent);

  OS2.MouClose(hmou);

  OS2.DosKillThread(KH_tid);
  KbdThread_Started := TRUE;
  OS2.DosCloseEventSem(Key_Empty);
  OS2.DosCloseEventSem(Key_Taken);

  IF Saved THEN
    vioModeInfo := Save_vioModeInfo;
    OS2.VioSetMode(vioModeInfo, 0);
    Saved := FALSE;
  END;
  OS2.VioWrtCellStr(SaveScreen^, BufferSize, 0, 0, 0);
  SetCurPos(Xcur, Ycur);
END CloseConsole;

PROCEDURE ToggleConsoles(screen: LOGICAL_SCREEN);
VAR
  N: sys.CARD16;
BEGIN
  IF Dbg_Console THEN
    OS2.VioWrtCellStr(SaveScreen^, BufferSize, 0, 0, 0);
  ELSE
    N := BufferSize;
    OS2.VioReadCellStr(SaveScreen^, N, 0, 0, 0);
    ASSERT(N=BufferSize);
    sys.MOVE(sys.ADR(screen^), sys.ADR(Scr^), SIZE(screen^));
    OS2.VioWrtCellStr(Scr^, BufferSize, 0, 0, 0);
  END;
  Dbg_Console := NOT Dbg_Console;
END ToggleConsoles;

PROCEDURE OutSymbol(x,y: CARDINAL; ch: CHAR; attr: ATTR);
VAR
  cell: CELL;
BEGIN
  cell.ch   := ch;
  cell.attr := attr;
  OS2.VioWrtNCell(cell, 1, y, x, 0);
END OutSymbol;

<* PUSH *>
<* CHECKNIL- *> <* CHECKINDEX- *> <* CHECKDINDEX- *>

PROCEDURE OutRect(screen: LOGICAL_SCREEN; x1, y1, x2, y2: CARDINAL);
VAR
  y: CARDINAL;
  pos, len: CARDINAL;
BEGIN
  IF (x1 = 0) & (x2 = Xmax - 1) THEN
    pos := y1*Xmax;
    len := Xmax * 2 * (y2 - y1 + 1);
    sys.MOVE(sys.ADR(screen^[pos]), sys.ADR(Scr^[pos]), len);
    OS2.VioWrtCellStr(Scr^[y1*Xmax], len, y1, x1, 0)
  ELSE
    pos := y1*Xmax+x1;
    len := (x2-x1+1)*2;

    FOR y:=y1 TO y2 DO
      sys.MOVE(sys.ADR(screen^[pos]), sys.ADR(Scr^[pos]), len);
      OS2.VioWrtCellStr(Scr^[pos], len, y, x1, 0);
      INC(pos, Xmax);
    END;
  END;
END OutRect;

<* POP *>

PROCEDURE Flush();
BEGIN
END Flush;


PROCEDURE Beep;
BEGIN
  OS2.DosBeep(500,  20);
  OS2.DosBeep(1000, 150);
END Beep;

PROCEDURE CursorOff;
VAR
  vc: OS2.VIOCURSORINFO;
BEGIN
  OS2.VioGetCurType(vc, 0);
  IF vc.attr # 0FFFFH THEN
    vc.attr := 0FFFFH;
    OS2.VioSetCurType(vc, 0);
  END;
END CursorOff;

PROCEDURE CursorOn;
VAR
  vc: OS2.VIOCURSORINFO;
BEGIN
  OS2.VioGetCurType(vc, 0);
  IF vc.attr # 0 THEN
    vc.attr := 0;
    OS2.VioSetCurType(vc, 0);
  END;
END CursorOn;

PROCEDURE SetCurPos(x,y: CARDINAL);
BEGIN
  OS2.VioSetCurPos(y, x, 0);
END SetCurPos;

PROCEDURE CursorState(): BOOLEAN;
VAR
  vc: OS2.VIOCURSORINFO;
BEGIN
  OS2.VioGetCurType(vc, 0);
  RETURN vc.attr # 0FFFFH;
END CursorState;


(* Delay d fraction second (1 fraction = 1/100 second) *)
PROCEDURE Delay (d: CARDINAL);
BEGIN
  OS2.DosSleep (d*10);
END Delay;

BEGIN
  KbdThread_Started := FALSE;
  SaveScreen := NIL;

  dbl_time := OS2.WinQuerySysValue(OS2.HWND_DESKTOP, OS2.SV_DBLCLKTIME);
  ASSERT(dbl_time # 0);

FINALLY
  IF Saved THEN
    CloseConsole();
  END;
END Console.
