(* Copyright (C) 1998 XDS Ltd. *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

<*-IOVERFLOW    *>
<*-COVERFLOW    *>
<*-CHECKINDEX   *>
<*-CHECKRANGE   *>
<*-CHECKNIL     *>

IMPLEMENTATION MODULE MsMouse;

IMPORT SYSTEM, xtsIGraph, xtsEvQue, W:=Windows;

FROM SYSTEM   IMPORT ADDRESS, INT16;
FROM xtsEvQue IMPORT FLSET, MOUSE_EVT, L_PRESSED, R_PRESSED, M_PRESSED;



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
  fwKeys : W.WPARAM;
  xPos   : INTEGER;
  yPos   : INTEGER;
  aPressInfo   : ARRAY [0..2] OF HITINFO;
  aReleaseInfo : ARRAY [0..2] OF HITINFO;
  --
  lastMotion   : MsMotion;
  IntFunc      : MouseFuncType;
  IntFuncMask  : CARDINAL;
  hMouSem      : W.HANDLE;


PROCEDURE waitMouB();
BEGIN
  W.WaitForSingleObject (hMouSem, W.INFINITE);
  W.ResetEvent (hMouSem);
END waitMouB;


PROCEDURE releaseMou();
BEGIN
  W.SetEvent (hMouSem);
END releaseMou;




PROCEDURE Reset(): INTEGER;
VAR n: INTEGER;
BEGIN
  n := W.GetSystemMetrics(W.SM_CMOUSEBUTTONS);
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
  mp.left_pressed   := W.MK__LBUTTON IN W.MK_SET(fwKeys);
  mp.right_pressed  := W.MK__RBUTTON IN W.MK_SET(fwKeys);
  mp.middle_pressed := W.MK__MBUTTON IN W.MK_SET(fwKeys);
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
  waitMouB();
  IntFunc     := mp.IntFunc;
  IntFuncMask := mp.mask;
  releaseMou();
END SetInterrupt;



PROCEDURE SwapInterrupt(VAR mp: MsInterrupt);
VAR
  ifunc : MouseFuncType;
  imask : CARDINAL;
BEGIN
  waitMouB();
  ifunc := IntFunc;
  imask := IntFuncMask;
  IntFunc     := mp.IntFunc;
  IntFuncMask := mp.mask;
  mp.IntFunc := ifunc;
  mp.mask    := imask;
  releaseMou();
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



PROCEDURE InitMouse();
BEGIN
END InitMouse;



--======================== Event handler =================================--


PROCEDURE ["StdCall"] evtHandler( hWnd :W.HWND; msg :W.UINT;
                      wParam :W.WPARAM; lParam :W.LPARAM) :W.LRESULT;
VAR
  mask:  MASK;
  phit:  PHITINFO;
  btns:  CARDINAL;
BEGIN
  IF NOT (xtsIGraph.fGrMode) THEN RETURN 0; END;

  mask := MASK(0);
  phit := NIL;
  CASE (msg) OF
  | W.WM_MOUSEMOVE:
      mask   := MASK{MASK__MOVE};

  | W.WM_LBUTTONDBLCLK,
    W.WM_LBUTTONDOWN:
      mask   := MASK{MASK__LDOWN};
      phit   := SYSTEM.ADR(aPressInfo[_MS_LEFT]);

  | W.WM_LBUTTONUP:
      mask   := MASK{MASK__LUP};
      phit   := SYSTEM.ADR(aReleaseInfo[_MS_LEFT]);

  | W.WM_RBUTTONDBLCLK,
    W.WM_RBUTTONDOWN:
      mask   := MASK{MASK__RDOWN};
      phit   := SYSTEM.ADR(aPressInfo[_MS_RIGHT]);

  | W.WM_RBUTTONUP:
      mask   := MASK{MASK__RUP};
      phit   := SYSTEM.ADR(aReleaseInfo[_MS_RIGHT]);

  | W.WM_MBUTTONDBLCLK,
    W.WM_MBUTTONDOWN:
      mask   := MASK{MASK__MDOWN};
      phit   := SYSTEM.ADR(aPressInfo[_MS_MIDDLE]);

  | W.WM_MBUTTONUP:
      mask   := MASK{MASK__MUP};
      phit   := SYSTEM.ADR(aReleaseInfo[_MS_MIDDLE]);

  ELSE
    RETURN 0;
  END;

  waitMouB();

  IF (xPos # W.LOWORD(lParam)) OR (yPos # W.HIWORD(lParam)) OR (phit # NIL) THEN
    xPos       := W.LOWORD(lParam);
    yPos       := W.HIWORD(lParam);
    IF (phit # NIL) THEN
      phit^.xPos  := W.LOWORD(lParam);
      phit^.yPos  := W.HIWORD(lParam);
      INC(phit^.count);
    END;
    fwKeys     := wParam;

    IF (IntFunc # NIL) AND ((mask * MASK(IntFuncMask)) # MASK(0)) THEN
      btns := 0;
      IF (W.MK__LBUTTON IN W.MK_SET(fwKeys)) THEN btns := 1;      END;
      IF (W.MK__RBUTTON IN W.MK_SET(fwKeys)) THEN btns := btns+2; END;
      IF (W.MK__MBUTTON IN W.MK_SET(fwKeys)) THEN btns := btns+4; END;
      IntFunc(CARDINAL(mask),btns,xPos,yPos);
    END;
  END;

  releaseMou();

  RETURN 0;
END evtHandler;


PROCEDURE conEvtHandler(mouEv : MOUSE_EVT) : BOOLEAN;

(*FLSET = SET OF ( L_PRESSED, R_PRESSED, M_PRESSED );

  MOUSE_EVT = RECORD
    x  : LONGINT;
    y  : LONGINT;
    fl : FLSET;
  END;*)

VAR
  mask:  MASK;
  phit:  PHITINFO;
  btns:  CARDINAL;
BEGIN
  IF (xtsIGraph.fGrMode) THEN RETURN FALSE; END;

  waitMouB();

  mask := MASK(0);

  IF (L_PRESSED IN mouEv.fl) # (W.MK__LBUTTON IN W.MK_SET(fwKeys)) THEN
     IF (L_PRESSED IN mouEv.fl) THEN
       INCL(mask, MASK__LDOWN);
       phit := SYSTEM.ADR(aPressInfo[_MS_LEFT]);
     ELSE
       INCL(mask, MASK__LUP);
       phit := SYSTEM.ADR(aReleaseInfo[_MS_LEFT]);
     END;
     phit^.xPos  := mouEv.x;
     phit^.yPos  := mouEv.y;
     INC(phit^.count);
  END;

  IF (R_PRESSED IN mouEv.fl) # (W.MK__RBUTTON IN W.MK_SET(fwKeys)) THEN
     IF (R_PRESSED IN mouEv.fl) THEN
       INCL(mask, MASK__RDOWN);
       phit := SYSTEM.ADR(aPressInfo[_MS_RIGHT]);
     ELSE
       INCL(mask, MASK__LUP);
       phit := SYSTEM.ADR(aReleaseInfo[_MS_RIGHT]);
     END;
     phit^.xPos  := mouEv.x;
     phit^.yPos  := mouEv.y;
     INC(phit^.count);
  END;

  IF (M_PRESSED IN mouEv.fl) # (W.MK__MBUTTON IN W.MK_SET(fwKeys)) THEN
     IF (M_PRESSED IN mouEv.fl) THEN
       INCL(mask, MASK__MDOWN);
       phit := SYSTEM.ADR(aPressInfo[_MS_MIDDLE]);
     ELSE
       INCL(mask, MASK__MUP);
       phit := SYSTEM.ADR(aReleaseInfo[_MS_MIDDLE]);
     END;
     phit^.xPos  := mouEv.x;
     phit^.yPos  := mouEv.y;
     INC(phit^.count);
  END;

  IF (xPos # mouEv.x) OR (yPos # mouEv.y) THEN
    xPos := mouEv.x;
    yPos := mouEv.y;
    INCL(mask, MASK__MOVE);
  END;

  btns   := 0;
  fwKeys := 0;
  IF (L_PRESSED IN mouEv.fl) THEN
    fwKeys := W.WPARAM( W.MK_SET(fwKeys) + W.MK_SET{W.MK__LBUTTON});
    btns   := btns + 1;
  END;
  IF (R_PRESSED IN mouEv.fl) THEN
    fwKeys := W.WPARAM( W.MK_SET(fwKeys) + W.MK_SET{W.MK__RBUTTON});
    btns   := btns + 2;
  END;
  IF (M_PRESSED IN mouEv.fl) THEN
    fwKeys := W.WPARAM( W.MK_SET(fwKeys) + W.MK_SET{W.MK__MBUTTON});
    btns   := btns + 4;
  END;

  IF (IntFunc # NIL) AND ((mask * MASK(IntFuncMask)) # MASK(0)) THEN
    IntFunc(CARDINAL(mask),btns,xPos,yPos);
  END;

  releaseMou();

  RETURN TRUE;
END conEvtHandler;


BEGIN
  IntFunc        := NIL;
  IntFuncMask    := 0;
  xtsIGraph.proc := evtHandler;

  hMouSem := W.CreateEvent ( NIL, TRUE, TRUE,  NIL ); -- posted
  IF (hMouSem = NIL) THEN
    W.MessageBox ( NIL, "Can't create event semaphore.", "MsMouse: ERRER", W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND);
  END;

  xtsEvQue.AddMouseHandler(conEvtHandler);
END MsMouse.
