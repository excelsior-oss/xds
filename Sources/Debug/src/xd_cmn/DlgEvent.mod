<* Storage+ *>
IMPLEMENTATION MODULE DlgEvent;

IMPORT sys := SYSTEM;

IMPORT key := Keys;
IMPORT win := Dlg_Win;
IMPORT crt := CRT;
IMPORT act := Dlg_Acts;

IMPORT con := Console;

VAR
  Queue: ARRAY [0..1023] OF MSG;
  Head, Tail: CARDINAL;

PROCEDURE AddToHead(hwnd: HWND; ID: EVENT; par: CARDINAL);
BEGIN
  IF Head = 0 THEN Head := HIGH(Queue) ELSE DEC(Head) END;
  Queue[Head].hwnd := hwnd;
  Queue[Head].ID   := ID;
  Queue[Head].par  := par;
END AddToHead;

PROCEDURE AddToTail(hwnd: HWND; ID: EVENT; par: CARDINAL);
BEGIN
  Queue[Tail].hwnd := hwnd;
  Queue[Tail].ID   := ID;
  Queue[Tail].par  := par;
  IF Tail = HIGH(Queue) THEN Tail := 0 ELSE INC(Tail) END;
END AddToTail;


PROCEDURE AddToHeadIfValid (hwnd: HWND; ID: EVENT; par: CARDINAL);
BEGIN
  IF hwnd # win.Invalid_H THEN
    AddToHead (hwnd, ID, par);
  END;
END AddToHeadIfValid;


PROCEDURE AddToTailIfValid (hwnd: HWND; ID: EVENT; par: CARDINAL);
BEGIN
  IF hwnd # win.Invalid_H THEN
    AddToTail (hwnd, ID, par);
  END;
END AddToTailIfValid;


PROCEDURE Make (_hwnd: HWND; _ID: EVENT; _par: CARDINAL): MSG;
VAR
  tmp: MSG;
BEGIN
  WITH tmp DO
    hwnd := _hwnd;
    ID := _ID;
    par := _par;
  END;
  RETURN tmp;
END Make;


PROCEDURE GetEvent(): MSG;
VAR
  msg : MSG;
BEGIN
  IF Head = Tail THEN RETURN Empty END;
  msg := Queue[Head];
  IF Head = HIGH(Queue) THEN Head := 0 ELSE INC(Head) END;
  RETURN msg;
END GetEvent;

VAR
  WindowForMouse: HWND;

PROCEDURE WaitForEvent;
VAR
  action : act.ACTION;
  event  : con.EVENT;
  hwnd, pair: HWND;
  Kind   : EVENT;
  trusted, moved, add_event: BOOLEAN;
BEGIN
  LOOP
    con.GetEvent(event);
    CASE event.kind OF
    | con.KeyEvent:
      IF NOT win.IsModal(win.ActiveWindow) & act.GetActionByKey(event.key_code, action) THEN
        AddToTail(win.ActiveWindow, DoAction, event.key_code*100H+ORD(action));
      ELSE
        AddToTail(win.ActiveWindow, KbHit, event.key_code);
      END;
      RETURN;
    | con.MouseEvent:

      hwnd := crt.FindWindow(event.x, event.y);
      CASE event.action OF
      | con.Mou_L_Pressed:     Kind := Mouse_Pressed;
      | con.Mou_L_Released:    Kind := Mouse_Released;
      | con.Mou_L_DoubleClick: Kind := Mouse_Dbl;
      | con.Mou_R_Pressed:     Kind := R_Mouse_Pressed;
      | con.Mou_R_Released:    Kind := R_Mouse;
      | con.Mou_Moved:         Kind := Mouse_Moved;
      ELSE
        ASSERT(FALSE, ORD(event.action));
      END;
      CASE Kind OF
      | Mouse_Pressed, R_Mouse_Pressed:
        WindowForMouse := hwnd;
      ELSE
      END;
      IF (hwnd # win.Invalid_H) OR (win.GetPair(win.ActiveWindow) = AllWindows) THEN
        trusted := FALSE;
        IF (hwnd # win.Invalid_H) & (hwnd # win.ActiveWindow) THEN
          moved   := Kind = Mouse_Moved;
          pair := win.GetPair(win.ActiveWindow);
          trusted := (hwnd = pair) OR (pair = AllWindows);
          add_event := trusted;
          IF NOT win.IsModal(win.ActiveWindow) THEN
            IF NOT moved AND NOT trusted THEN
              CASE Kind OF
              | Mouse_Released, R_Mouse:
                IF WindowForMouse # hwnd THEN
                  RETURN;
                END;
              ELSE
              END;
              AddToTail(win.ActiveWindow, Defocus, hwnd);
              AddToTail(hwnd, Rise, 0);
              add_event := TRUE;
            END;
          END;
        ELSE
          add_event := TRUE;
        END;
        IF add_event THEN
          CASE Kind OF
          | Mouse_Released, R_Mouse:
            pair := win.GetPair(win.ActiveWindow);
            IF trusted OR (WindowForMouse = hwnd) OR (WindowForMouse = pair) OR (pair = AllWindows) THEN
              AddToTail(hwnd, Kind, event.x*10000H+event.y);
              RETURN;
            END;
          ELSE
            AddToTail(hwnd, Kind, event.x*10000H+event.y);
            RETURN;
          END;
        END;
      END;
    ELSE
      ASSERT(FALSE);
    END;
  END;
END WaitForEvent;

PROCEDURE Flush;
VAR
  Msg: MSG;
  tmp: BOOLEAN;
BEGIN
  tmp := InFlush;
  InFlush := TRUE;
  LOOP
    Msg := GetEvent();
    IF Msg.ID = None THEN
      EXIT
    END;
    IF NOT win.WinDispatchMsg(Msg) THEN ASSERT(FALSE) END;
  END;
  InFlush := tmp;
END Flush;

PROCEDURE MainLoop;
VAR
  msg  : MSG;
BEGIN
  LOOP
    InFlush := FALSE;
    LOOP
      msg := GetEvent();
      IF msg.ID = None THEN EXIT END;
      IF NOT win.WinDispatchMsg(msg) THEN RETURN END;
    END;
    WaitForEvent;
  END;
END MainLoop;


PROCEDURE ModalRedraw (hwnd: HWND);
BEGIN
  IF InFlush THEN
    AddToTail (hwnd, Redraw, par_RedrawInFlush);
  ELSE
    AddToTail (hwnd, Redraw, par_RedrawNormal);
  END;
END ModalRedraw;


PROCEDURE CheckWaitForEvent (hwnd: HWND; par: CARDINAL);
BEGIN
  IF InFlush AND (hwnd = win.ActiveWindow) AND (par = par_RedrawInFlush) THEN
    WaitForEvent;
  END;
END CheckWaitForEvent;


BEGIN
  Head := 0; Tail := 0;
  Empty := MSG {0, None, 0};
END DlgEvent.
