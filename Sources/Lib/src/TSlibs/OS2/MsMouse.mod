(* Copyright (C) 1998 XDS Ltd. *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

<*-IOVERFLOW    *>
<*-COVERFLOW    *>
<*-CHECKINDEX   *>
<*-CHECKRANGE   *>
<*-CHECKNIL     *>

IMPLEMENTATION MODULE MsMouse;

IMPORT SYSTEM, GComm := xtsGComm, O := OS2;

FROM SYSTEM IMPORT ADDRESS;


TYPE
  HITINFO = RECORD
    xPos  : INTEGER;
    yPos  : INTEGER;
    count : CARDINAL;
  END;
  PHITINFO = POINTER TO HITINFO;

 MASK_ENUM = (
   MASK__MOVE,  -- bit 0 = mouse movement           (01H)
   MASK__LDOWN, -- bit 1 = left button pressed      (02H)
   MASK__LUP,   -- bit 2 = left button released     (04H)
   MASK__RDOWN, -- bit 3 = right button pressed     (08H)
   MASK__RUP,   -- bit 4 = right button released    (10H)
   MASK__MDOWN, -- bit 5 = center button pressed    (20H)
   MASK__MUP);  -- bit 6 = center button released   (40H)

 MASK = SET OF MASK_ENUM;


--::::::::::GLOBALS:::::::::--

VAR
  -- Mouse handler changes it:
  btnLdown : BOOLEAN;
  btnRdown : BOOLEAN;
  btnMdown : BOOLEAN;
  xPos     : INTEGER;
  yPos     : INTEGER;
  aPressInfo   : ARRAY [0..2] OF HITINFO;
  aReleaseInfo : ARRAY [0..2] OF HITINFO;
  --
  lastMotion   : MsMotion;
  IntFunc      : MouseFuncType;
  IntFuncMask  : CARDINAL;
  mouTid       : O.TID;
  hsemMou      : O.HEV;



PROCEDURE Reset(): INTEGER;
VAR n: INTEGER;
BEGIN
  n := O.WinQuerySysValue(O.HWND_DESKTOP, O.SV_CMOUSEBUTTONS);
  IF (n=0) THEN RETURN MAX(INTEGER);
  ELSE          RETURN n;
  END;
END Reset;



PROCEDURE Cursor(Mode: BOOLEAN);
BEGIN
END Cursor;



PROCEDURE GetStatus(VAR mp: MsData);
BEGIN
  mp.row := yPos;
  mp.col := xPos;
  mp.left_pressed   := btnLdown;
  mp.right_pressed  := btnRdown;
  mp.middle_pressed := btnMdown;
END GetStatus;



PROCEDURE SetPosition(Row, Col: INTEGER);
BEGIN
  RETURN;
END SetPosition;



PROCEDURE GetPress(Button: INTEGER; VAR mp: MsData);
BEGIN
  IF (Button<0) OR (Button>2) THEN RETURN; END;
  WITH aPressInfo[Button] DO
    mp.row     := yPos;
    mp.col     := xPos;
    mp.actions := count;
    count      := 0;
  END;
END GetPress;



PROCEDURE GetRelease(Button: INTEGER; VAR mp: MsData);
BEGIN
  IF (Button<0) OR (Button>2) THEN RETURN; END;
  WITH aReleaseInfo[Button] DO
    mp.row     := yPos;
    mp.col     := xPos;
    mp.actions := count;
    count      := 0;
  END;
END GetRelease;



PROCEDURE SetRange(mp: MsRange);
BEGIN
END SetRange;



PROCEDURE SetGraphCursor(VAR mp: MsGraphcur);
BEGIN
END SetGraphCursor;



PROCEDURE SetTextCursor(mp: MsTextCur);
BEGIN
END SetTextCursor;



PROCEDURE GetMotion(VAR mp: MsMotion);
BEGIN
    mp.        horiz := xPos - lastMotion.horiz;
    lastMotion.horiz := xPos;
    mp.        vert  := yPos - lastMotion.vert;
    lastMotion.vert  := yPos;
END GetMotion;



PROCEDURE SetInterrupt(VAR mp: MsInterrupt);
BEGIN
  IntFunc     := mp.IntFunc;
  IntFuncMask := mp.mask;
END SetInterrupt;



PROCEDURE SwapInterrupt(VAR mp: MsInterrupt);
VAR
  ifunc : MouseFuncType;
  imask : CARDINAL;
BEGIN
  ifunc := IntFunc;
  imask := IntFuncMask;
  IntFunc     := mp.IntFunc;
  IntFuncMask := mp.mask;
  mp.IntFunc := ifunc;
  mp.mask    := imask;
END SwapInterrupt;



PROCEDURE LightPen(Mode: BOOLEAN);
BEGIN
END LightPen;



PROCEDURE SetMickeys(Vert, Horiz: INTEGER);
BEGIN
END SetMickeys;



PROCEDURE UpdateScreen(x1, y1, x2, y2: INTEGER);
BEGIN
END UpdateScreen;



PROCEDURE SetDouble(Threshold: INTEGER);
END SetDouble;



PROCEDURE DriverSize(): CARDINAL;
BEGIN
  RETURN 4;
END DriverSize;



PROCEDURE SaveDriver(Buffer: ADDRESS);
BEGIN
END SaveDriver;



PROCEDURE RestoreDriver(Buffer: ADDRESS);
BEGIN
END RestoreDriver;



PROCEDURE GetSensitivity(VAR mp: MsSense);
BEGIN
  mp.v_speed   := 1;
  mp.h_speed   := 1;
  mp.threshold := 1;
END GetSensitivity;



PROCEDURE SetSensitivity(mp: MsSense);
BEGIN
END SetSensitivity;



PROCEDURE SetPage(Page: CARDINAL);
BEGIN
END SetPage;



PROCEDURE GetPage(): CARDINAL;
BEGIN
  RETURN 1;
END GetPage;

--======================== Event handler =================================--

PROCEDURE mouse_proc( ulMsg : LONGCARD; m1, m2 : O.MPARAM);
VAR
  mask:  MASK;
  phit:  PHITINFO;
  btns:  CARDINAL;
  c   :  CARDINAL;
BEGIN
  WHILE (O.DosWaitEventSem(hsemMou, 300 ) = O.ERROR_TIMEOUT) DO
    O.DosSleep(0);
  END;
  O.DosResetEventSem (hsemMou, c);

  mask := MASK(0);
  phit := NIL;
  CASE (ulMsg) OF
  | O.WM_MOUSEMOVE:
      mask   := MASK{MASK__MOVE};

  | O.WM_BUTTON1DOWN,
    O.WM_BUTTON1DBLCLK:
      mask   := MASK{MASK__LDOWN};
      phit   := SYSTEM.ADR(aPressInfo[_MS_LEFT]);

  | O.WM_BUTTON1UP:
      mask   := MASK{MASK__LUP};
      phit   := SYSTEM.ADR(aReleaseInfo[_MS_LEFT]);

  | O.WM_BUTTON2DOWN,
    O.WM_BUTTON2DBLCLK:
      mask   := MASK{MASK__RDOWN};
      phit   := SYSTEM.ADR(aPressInfo[_MS_RIGHT]);

  | O.WM_BUTTON2UP:
      mask   := MASK{MASK__RUP};
      phit   := SYSTEM.ADR(aReleaseInfo[_MS_RIGHT]);

  | O.WM_BUTTON3DOWN,
    O.WM_BUTTON3DBLCLK:
      mask   := MASK{MASK__MDOWN};
      phit   := SYSTEM.ADR(aPressInfo[_MS_MIDDLE]);

  | O.WM_BUTTON3UP:
      mask   := MASK{MASK__MUP};
      phit   := SYSTEM.ADR(aReleaseInfo[_MS_MIDDLE]);

  ELSE
    O.DosPostEventSem (hsemMou);
    RETURN;
  END;

  xPos       := INTEGER(SYSTEM.INT16(m1));
  yPos       := INTEGER(GComm.mouseWinHeight) - INTEGER(SYSTEM.INT16(LONGCARD(m1) DIV 10000H));
  IF (phit # NIL) THEN
    phit^.xPos  := xPos;
    phit^.yPos  := yPos;
    IF (phit^.count < MAX(CARDINAL)) THEN INC(phit^.count); END;
  END;
  btnLdown := LONGCARD(O.WinGetKeyState(O.HWND_DESKTOP, O.VK_BUTTON1)) >= 8000H;
  btnRdown := LONGCARD(O.WinGetKeyState(O.HWND_DESKTOP, O.VK_BUTTON2)) >= 8000H;
  btnMdown := LONGCARD(O.WinGetKeyState(O.HWND_DESKTOP, O.VK_BUTTON3)) >= 8000H;

  IF (IntFunc # NIL) AND ((mask * MASK(IntFuncMask)) # MASK(0)) THEN
    btns := 0;
    IF btnLdown THEN   btns := 1;      END;
    IF btnRdown THEN   btns := btns+2; END;
    IF btnMdown THEN   btns := btns+4; END;
    IntFunc(CARDINAL(mask),btns,xPos,yPos);
  END;

  O.DosPostEventSem (hsemMou);
END mouse_proc;



(* Handle Graph' mouse events *)
PROCEDURE ["C"] mouseProc( ulMsg : LONGCARD; m1, m2 : O.MPARAM);
BEGIN
  IF GComm.mouseGraph THEN
     mouse_proc(ulMsg,m1,m2);
  END;
END mouseProc;




(* Waiting for VIO mouse events *)
PROCEDURE [O.EXPENTRY] mouseThread(ul: O.ULONG);
VAR
  mouev    : O.MOUEVENTINFO;
  hMou     : O.HMOU;
  m1       : O.MPARAM;
  fL,fR,fM : BOOLEAN;
  fwait    : O.USHORT;

BEGIN
  O.DosSleep(500);
  GComm.mouseProc := mouseProc; -- again...
  O.MouOpen(NIL,hMou);
  LOOP
    fwait := 1;
    IF (0 = O.MouReadEventQue(mouev, fwait, hMou)) AND NOT GComm.mouseGraph THEN
      fL := LONGCARD(O.WinGetKeyState(O.HWND_DESKTOP, O.VK_BUTTON1)) >= 8000H;
      fR := LONGCARD(O.WinGetKeyState(O.HWND_DESKTOP, O.VK_BUTTON2)) >= 8000H;
      fM := LONGCARD(O.WinGetKeyState(O.HWND_DESKTOP, O.VK_BUTTON3)) >= 8000H;
      m1 := O.MPARAM(LONGCARD(INTEGER(GComm.mouseWinHeight) - INTEGER(mouev.row)) * 10000H + LONGCARD(mouev.col));

      IF (btnLdown # fL) THEN
        IF (fL) THEN  mouse_proc(O.WM_BUTTON1DOWN, m1, NIL);
        ELSE          mouse_proc(O.WM_BUTTON1UP,   m1, NIL);
        END;
      END;
      IF (btnRdown # fR) THEN
        IF (fR) THEN  mouse_proc(O.WM_BUTTON2DOWN, m1, NIL);
        ELSE          mouse_proc(O.WM_BUTTON2UP,   m1, NIL);
        END;
      END;
      IF (btnMdown # fM) THEN
        IF (fM) THEN  mouse_proc(O.WM_BUTTON3DOWN, m1, NIL);
        ELSE          mouse_proc(O.WM_BUTTON3UP,   m1, NIL);
        END;
      END;
      IF (xPos # INTEGER(mouev.col)) OR (yPos # INTEGER(mouev.row)) THEN
        mouse_proc(O.WM_MOUSEMOVE, m1, NIL);
      END;
    END;
    O.DosSleep(1);
  END;
END mouseThread;


PROCEDURE InitMouse();
BEGIN
  btnLdown        := FALSE;
  btnRdown        := FALSE;
  btnMdown        := FALSE;
  IntFunc         := NIL;
  IntFuncMask     := 0;
  GComm.mouseProc := mouseProc;
  IF (mouTid = 0) THEN
    IF (O.DosCreateEventSem (NIL, hsemMou, 0, TRUE) = O.NO_ERROR) THEN
      O.DosCreateThread (mouTid, mouseThread, 0, O.CREATE_READY, 0FFFFH);
    END;
  END;
END InitMouse;

BEGIN
  mouTid           := 0;
  GComm.mouseGraph := FALSE;
  InitMouse();
END MsMouse.

