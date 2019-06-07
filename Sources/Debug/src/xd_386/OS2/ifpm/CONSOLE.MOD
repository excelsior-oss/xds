(*  *FSA - Dec 1998 *)

IMPLEMENTATION MODULE Console;

<* +STORAGE   *>
<* -IOVERFLOW *>
<* -COVERFLOW *>


IMPORT SYSTEM, Strings, Keys, OutMon;
IMPORT O   := OS2;
IMPORT BMP := conBmp;
IMPORT opt := Options;
IMPORT kt  := KrnTypes;
IMPORT utl := Krn_Dbg;

FROM Storage IMPORT ALLOCATE;
FROM SYSTEM  IMPORT ADR, SET16, SET32;
FROM Krn_Dbg IMPORT Program_stopped, Program_started_2;



TYPE
  COLOR = ( Black,    Blue,         Green,      Cyan,
            Red,      Magenta,      Brown,      LightGray,
            DarkGray, LightBlue,    LightGreen, LightCyan,
            LightRed, LightMagenta, Yellow,     White);
  CLR_TABLETYPE = ARRAY [0..15] OF INTEGER;

<* PUSH *>
<* +WOFF324 *>
  PROFILE_RECORD = RECORD
    fAttrs      : O.FATTRS;
    xLeft, yTop : INTEGER;
  END;
<* POP *>


CONST
  CONSOLECLASSNAME = "XDConsole";
  RES_MAIN         = 10;
  ITEM_FONT        = 20;
  MM_START         = O.WM_USER+0;
  MM_CHANGEFONT    = O.WM_USER+1;
  MM_FLUSH         = O.WM_USER+2;
  CLR_TABLE        = CLR_TABLETYPE { O.CLR_BLACK,    O.CLR_DARKBLUE, O.CLR_DARKGREEN, O.CLR_DARKCYAN,
                                     O.CLR_DARKRED,  O.CLR_DARKPINK, O.CLR_BROWN,     O.CLR_PALEGRAY,
                                     O.CLR_DARKGRAY, O.CLR_BLUE,     O.CLR_GREEN,     O.CLR_CYAN,
                                     O.CLR_RED,      O.CLR_PINK,     O.CLR_YELLOW,    O.CLR_WHITE};


VAR
(*--- Globals ---*)
  hevReady         : O.HEV;
  hevKilled        : O.HEV;
  win_tid          : O.TID;  (* Console window thread ID *)
  hack_tid         : O.TID;  (* PMHack window thread ID *)

(* Console window statics *)
  hwndConsole      : O.HWND;
  hwndConsoleFrame : O.HWND;
  hpsConsole       : O.HPS;
  hpsBmp           : O.HPS;
  xWin,  yWin      : CARDINAL; -- pixels
  xCell, yCell     : CARDINAL;
  yDescender       : INTEGER;
  hSwitch          : O.HSWITCH;
  fcdata           : O.FRAMECDATA;
  hMsgQ            : O.HMQ;
  hAB              : O.HAB;
  qMsg             : O.QMSG;
  fModalState      : BOOLEAN;
  fAttrs           : O.FATTRS;
  cursorON         : BOOLEAN;
  xCurs, yCurs     : CARDINAL;
  --Text buffer:
  xSize, ySize     : CARDINAL; -- cells
  lScreen          : LOGICAL_SCREEN;
  cell             : CELL;
  --Events:
  hevSetModal      : O.HEV;
  hevEventTaken    : O.HEV;
  hevProcessing    : O.HEV;
  hevFlushing      : O.HEV;
  eventMouse       : EVENT;
  eventKey         : EVENT;

  xLeftIni         : INTEGER;
  yTopIni          : INTEGER;

PROCEDURE FontDlg():BOOLEAN;
-- cahnges global fAttrs
VAR
  fontDlg     : O.FONTDLG;
  szFamily    : ARRAY [0..100] OF CHAR;
  fontMetrics : O.FONTMETRICS;
BEGIN
  SYSTEM.FILL (ADR(fontDlg), 0, SIZE(O.FONTDLG));
  O.GpiQueryFontMetrics (hpsBmp, SIZE(O.FONTMETRICS), fontMetrics);
  Strings.Assign(fontMetrics.szFamilyname, szFamily);

  WITH fontDlg DO
    cbSize         := SIZE(O.FONTDLG);
    hpsScreen      := hpsBmp;
    pszFamilyname  := ADR(szFamily);
    usFamilyBufLen := SIZE(szFamily);
    fxPointSize    := 80000H;
    fl             := O.FNTS_CENTER+O.FNTS_INITFROMFATTRS+O.FNTS_BITMAPONLY+O.FNTS_FIXEDWIDTHONLY;
    flType         := fontMetrics.fsType;
    clrFore        := O.CLR_BLACK;
    clrBack        := O.CLR_WHITE;
    sNominalPointSize := fontMetrics.sNominalPointSize;
    usWeight          := fontMetrics.usWeightClass;
    usWidth           := fontMetrics.usWidthClass;
  END;
  fontDlg.fAttrs := fAttrs;

  O.WinFontDlg(O.HWND_DESKTOP,hwndConsole,fontDlg);
  IF (fontDlg.lReturn = O.DID_OK) THEN
    fAttrs := fontDlg.fAttrs;
    RETURN TRUE;
  END;
  RETURN FALSE;
END FontDlg;



PROCEDURE getCell(fAttrs : O.FATTRS);
VAR
  fm   : O.FONTMETRICS;
  ptl  : O.POINTL;
  aptl : ARRAY [0..2] OF O.POINTL;
BEGIN
  IF O.GpiCreateLogFont(hpsConsole,NIL,1,fAttrs) # O.GPI_ERROR THEN
    O.GpiSetCharSet(hpsConsole,1);
  END;

  O.GpiQueryFontMetrics(hpsConsole,SIZE(fm),fm);
  ptl.x := 0; ptl.y := 0;
  O.GpiQueryCharStringPosAt(hpsConsole, ptl, 0, 2, "WW", NIL, aptl);

  xCell      := aptl[1].x-aptl[0].x;
  yCell      := fm.lMaxBaselineExt+fm.lExternalLeading;
  yDescender := fm.lMaxDescender;

  O.GpiSetCharSet(hpsConsole,O.LCID_DEFAULT);
END getCell;



PROCEDURE wrProfile();
VAR
  prfRec : PROFILE_RECORD;
  swp    : O.SWP;
BEGIN
  IF (hwndConsole # 0) THEN
    prfRec.fAttrs := fAttrs;
    O.WinQueryWindowPos(hwndConsoleFrame, swp);
    IF (SET32(swp.fl) * SET32(O.SWP_MINIMIZE) # SET32{}) THEN
      O.WinSetWindowPos  (hwndConsoleFrame,0,0,0,0,0,O.SWP_RESTORE);
      O.WinQueryWindowPos(hwndConsoleFrame, swp);
    END;

    IF (SET32(swp.fl) * SET32(O.SWP_MAXIMIZE) # SET32{}) THEN
      prfRec.xLeft := O.WinQueryWindowUShort(hwndConsoleFrame, O.QWS_XRESTORE);
      prfRec.yTop  := O.WinQueryWindowUShort(hwndConsoleFrame, O.QWS_YRESTORE) +
                      O.WinQueryWindowUShort(hwndConsoleFrame, O.QWS_CYRESTORE);
    ELSE
      prfRec.xLeft := swp.x;
      prfRec.yTop  := swp.y + swp.cy;
    END;

    O.PrfWriteProfileData(O.HINI_USERPROFILE,"XDSDbg","WinPosition",ADR(prfRec), SIZE(PROFILE_RECORD));
  END;
END wrProfile;



PROCEDURE rdProfile();
VAR
  prfRec : PROFILE_RECORD;
  l      : CARDINAL;
BEGIN
  l := SIZE(PROFILE_RECORD);
  IF (NOT O.PrfQueryProfileData(O.HINI_USERPROFILE,"XDSDbg","WinPosition",ADR(prfRec),l)
     OR (l # SIZE(PROFILE_RECORD))) THEN
    RETURN;
  END;

  fAttrs := prfRec.fAttrs;

  xLeftIni := prfRec.xLeft;
  yTopIni  := prfRec.yTop;

END rdProfile;



(* --- Draw2Bmp ----------------------------------------------*)
(*                                                            *)
(* Action: Draw text rectangle into the bitmap                *)
(*                                                            *)
(*   col    : left column                                     *)
(*   line   : upper line                                      *)
(*   ccol   : columns                                         *)
(*   cline  : lines                                           *)
(*                                                            *)
PROCEDURE  Draw2Bmp(col, line, ccol, cline: CARDINAL);
VAR
  c, dc, l  : CARDINAL;
  attr      : ATTR;
  ptl       : O.POINTL;
  rcl       : O.RECTL;
  pstr      : POINTER TO ARRAY OF CHAR;
BEGIN

  IF (col >= xSize) OR (line >= ySize) OR (ccol=0) OR (cline=0) THEN RETURN; END;
  IF (col+ccol)   > xSize  THEN ccol  := xSize-col;  END;
  IF (line+cline) > ySize  THEN cline := ySize-line; END;
  NEW (pstr, ccol+1);


  FOR l:=line TO line+cline-1 DO
    rcl.yTop    := yWin - l*yCell;
    rcl.yBottom := yWin - l*yCell - yCell;
    ptl.y       := rcl.yBottom + yDescender + 1;
    c           := 0;
    WHILE (c < ccol) DO
      dc        := 0;
      cell      := lScreen^[xSize*l + col + c];
      attr      := cell.attr;
      REPEAT
        pstr^[dc] := lScreen^[xSize*l + col + c + dc].ch;
        INC(dc);
      UNTIL (c+dc >= ccol) OR (lScreen^[xSize*l + col + c + dc].attr # attr);
      ptl.x      := (col+c)*xCell;
      rcl.xLeft  := ptl.x;
      rcl.xRight := ptl.x + INTEGER(dc*xCell) - 1;

      O.GpiSetColor       (hpsBmp, CLR_TABLE[attr MOD 10H]);
      O.GpiSetBackColor   (hpsBmp, CLR_TABLE[attr DIV 10H]);
      O.GpiCharStringPosAt(hpsBmp, ptl, rcl, O.CHS_OPAQUE, dc, pstr^, NIL);
      c := c+dc;
    END;
  END;

  DISPOSE(pstr);
END Draw2Bmp;

PROCEDURE PaintBlock(col, line, ccol, cline: CARDINAL);
VAR rcl : O.RECTL;
BEGIN
  rcl.yTop    := yWin - line*yCell + 1;
  rcl.yBottom := yWin - line*yCell - cline*yCell;
  rcl.xLeft   := col*xCell;
  rcl.xRight  := rcl.xLeft + INTEGER(ccol*xCell);

  O.WinInvalidateRect(hwndConsole,rcl,FALSE);
END PaintBlock;



PROCEDURE SHORT1FROMMP(m : O.MPARAM):SYSTEM.CARD16;
BEGIN
  RETURN SYSTEM.CARD16(m);
END SHORT1FROMMP;

PROCEDURE SHORT2FROMMP(m : O.MPARAM):SYSTEM.CARD16;
BEGIN
  RETURN SYSTEM.CARD16(SYSTEM.CARD32(m) DIV 10000H);
END SHORT2FROMMP;



PROCEDURE [O.EXPENTRY] wpConsole(hWnd :O.HWND; ulMsg  :CARDINAL; m1, m2 :O.MPARAM) :O.MRESULT;

CONST
  SZITEM = "~Font...";
VAR
  swp         : O.SWP;
  rcl         : O.RECTL;
  hps         : O.HPS;
  x,y         : INTEGER;
  aptl        : ARRAY [0..2] OF O.POINTL;
  fsflags     : SET16;
  scan        : SYSTEM.CARD8;
  char, virt  : SYSTEM.CARD16;
  key_code    : CARDINAL;
  hMenu       : O.HWND;
  mi          : O.MENUITEM;

BEGIN

  CASE ulMsg OF
  |O.WM_CREATE:
    O.WinDefWindowProc(hWnd,ulMsg,m1,m2);
    hpsBmp := BMP.NewBitmap(10, 10, hAB);

  |MM_START:
    hpsConsole := O.WinGetPS(hWnd);
    WITH fAttrs DO
      usRecordLength  := SIZE(O.FATTRS);
      fsSelection     := 0;
      lMatch          := 82;
      Strings.Assign("System VIO",szFacename);
      idRegistry      := 0;
      usCodePage      := 0;
      lMaxBaselineExt := 14;
      lAveCharWidth   := 8;
      fsType          := 0;
      fsFontUse       := O.FATTR_FONTUSE_NOMIX;
    END;
    rdProfile();
    IF (xLeftIni # -1000) THEN
      O.WinSetWindowPos(hwndConsoleFrame,0,xLeftIni,yTopIni-200,400,200,O.SWP_MOVE+O.SWP_SIZE+O.SWP_SHOW);
    ELSE
      O.WinSetWindowPos(hwndConsoleFrame,0,100,300,400,200,O.SWP_MOVE+O.SWP_SIZE+O.SWP_SHOW);
    END;
    O.WinSendMsg(hWnd,MM_CHANGEFONT,NIL,NIL);

    mi.hwndSubMenu := 0;
    hMenu := O.WinWindowFromID(hwndConsoleFrame, O.FID_SYSMENU);
    O.WinSendMsg(hMenu,O.MM_QUERYITEM,O.MPARAM(O.SC_SYSMENU),O.MPARAM(ADR(mi)));
    hMenu := mi.hwndSubMenu;
    IF (hMenu # 0) THEN
      mi.iPosition   := 0;
      mi.afStyle     := O.MIS_TEXT;
      mi.afAttribute := 0;
      mi.id          := ITEM_FONT;
      mi.hwndSubMenu := 0;
      mi.hItem       := 0;
      O.WinSendMsg(hMenu, O.MM_INSERTITEM, O.MPARAM(ADR(mi)), O.MPARAM(ADR(SZITEM)));
      O.WinSendMsg(hMenu,O.MM_SETITEMATTR, O.MPFROM2SHORT(O.SHORT(O.SC_CLOSE),1), O.MPFROM2SHORT(O.MIA_DISABLED, O.MIA_DISABLED));
    END;

    IF (hevReady # 0) THEN O.DosPostEventSem(hevReady); END;

  |MM_CHANGEFONT:
    -- m1 = NIL => set fAttrs font and window pos.
    -- m1 # NIL => font dialog
    IF (m1 # NIL) THEN
      IF NOT FontDlg() THEN RETURN NIL; END;
    END;

    getCell(fAttrs);

    O.WinQueryWindowPos(hwndConsoleFrame,swp);
    x := xCell*xSize + CARDINAL( 2*O.WinQuerySysValue(O.HWND_DESKTOP,O.SV_CXBORDER));
    y := yCell*ySize + CARDINAL( 2*O.WinQuerySysValue(O.HWND_DESKTOP,O.SV_CYBORDER) +
                                   O.WinQuerySysValue(O.HWND_DESKTOP,O.SV_CYTITLEBAR));
    O.WinSetWindowPos(hwndConsoleFrame, 0, swp.x, swp.y+swp.cy-y, x, y, O.SWP_MOVE+O.SWP_SIZE);
    O.WinInvalidateRect(hwndConsole,NIL,FALSE);

  |MM_FLUSH:
    O.WinUpdateWindow(hWnd);
    O.DosPostEventSem(hevFlushing);

  |O.WM_SIZE:
    O.WinQueryWindowPos(hWnd,swp);
    xWin   := swp.cx;
    yWin   := swp.cy;
    hpsBmp := BMP.NewBitmap(xWin, yWin, hAB);
    IF O.GpiCreateLogFont(hpsBmp,NIL,1,fAttrs) # O.GPI_ERROR THEN
      O.GpiSetCharSet(hpsBmp,1);
    END;
    Draw2Bmp(0,0,xSize,ySize);
    O.WinInvalidateRect(hWnd,NIL,FALSE);

  |O.WM_PAINT:
    hps := O.WinBeginPaint(hWnd, 0, rcl );

    aptl[0].x := rcl.xLeft;  aptl[0].y := rcl.yBottom;   -- targ:   left bottom
    aptl[1].x := rcl.xRight; aptl[1].y := rcl.yTop;      --         right top
    aptl[2].x := rcl.xLeft;  aptl[2].y := rcl.yBottom;   -- source: left bottom
    O.GpiBitBlt(hps, hpsBmp, 3, aptl, O.ROP_SRCCOPY, O.BBO_IGNORE);

    O.WinEndPaint(hps);

  | O.WM_FOCUSCHANGE:
    IF (SHORT1FROMMP(m2) # 0) THEN
      IF (cursorON) THEN
        CursorOn();
      END;
    ELSE
      O.WinDestroyCursor(hWnd);
    END;
    RETURN O.WinDefWindowProc(hWnd,O.WM_FOCUSCHANGE,m1,m2);


  | O.WM_BUTTON1DOWN
  , O.WM_BUTTON2DOWN
  , O.WM_BUTTON1UP
  , O.WM_BUTTON2UP
  , O.WM_BUTTON1DBLCLK
  , O.WM_MOUSEMOVE :
    IF O.WinQueryFocus(O.HWND_DESKTOP) = hWnd THEN
      IF (ulMsg = O.WM_MOUSEMOVE) THEN                                           -- try to ignore it...
        IF (NOT (15 IN SET32(O.WinGetKeyState(O.HWND_DESKTOP,O.VK_BUTTON1)))) OR  -- LButton not pressed
           (((eventMouse.kind # None) AND (eventMouse.action # Mou_Moved)))        -- don't overwrite bitton actions
        THEN
          RETURN O.WinDefWindowProc(hWnd,ulMsg,m1,m2);
        END;
      END;
      O.DosWaitEventSem (hevProcessing, O.SEM_INDEFINITE_WAIT);
      WITH eventMouse DO
        CASE ulMsg OF
        | O.WM_BUTTON1DOWN   :  action := Mou_L_Pressed;
        | O.WM_BUTTON2DOWN   :  action := Mou_R_Pressed;
        | O.WM_BUTTON1UP     :  action := Mou_L_Released;
        | O.WM_BUTTON2UP     :  action := Mou_R_Released;
        | O.WM_BUTTON1DBLCLK :  action := Mou_L_DoubleClick;
        | O.WM_MOUSEMOVE     :  action := Mou_Moved;
        END;
        x :=             SHORT1FROMMP(m1) DIV xCell;
        y := ySize - 1 - SHORT2FROMMP(m1) DIV yCell;
        IF (x>=xSize) THEN x := xSize-1; END;
        IF (y>=ySize) THEN y := ySize-1; END;
        kind := MouseEvent;
      END;
      O.DosPostEventSem(hevEventTaken);
    END;
    RETURN O.WinDefWindowProc(hWnd,ulMsg,m1,m2);

  | O.WM_HELP:
    RETURN O.WinSendMsg(hWnd,O.WM_CHAR,O.MPARAM(3b010006h),O.MPARAM(00203b00h)); -- F1 key

  | O.WM_CHAR:
    fsflags := SET16(SHORT1FROMMP(m1));
    IF (eventKey.kind = None) AND  NOT (SET16(O.KC_KEYUP) * fsflags # SET16{}) THEN
      char := SHORT1FROMMP(m2);
      virt := SHORT2FROMMP(m2);
      scan := SYSTEM.CARD8(SYSTEM.CARD32(m1) DIV 1000000H);
      IF (SET16(O.KC_VIRTUALKEY) * fsflags # SET16{}) AND
         ((virt = O.VK_SHIFT)  OR
          (virt = O.VK_ALT  )  OR
          (virt = O.VK_CTRL )) THEN
        RETURN O.MRESULT(FALSE);
      END;
      fsflags := fsflags * SET16(O.KC_CHAR + O.KC_SCANCODE + O.KC_VIRTUALKEY + O.KC_SHIFT + O.KC_ALT + O.KC_CTRL);

      key_code := 0;

      IF (fsflags - SET16(O.KC_ALT + O.KC_CTRL) = SET16(O.KC_SCANCODE + O.KC_VIRTUALKEY)) THEN
        CASE virt OF
        | O.VK_ESC :
          key_code := Keys.Esc;

        | O.VK_ENTER
        , O.VK_NEWLINE :
          IF (SET16(O.KC_CTRL) * fsflags # SET16{}) THEN key_code := Keys.CtrlEnter;
          ELSE                                           key_code := Keys.Enter; END;

        | O.VK_SPACE :
          IF (SET16(O.KC_CTRL) * fsflags # SET16{}) THEN key_code := Keys.CtrlSpace;
          ELSE                                           key_code := Keys.Space; END;

        | O.VK_TAB :
          IF (SET16(O.KC_CTRL) * fsflags # SET16{}) THEN key_code := Keys.CtrlTab;
          ELSE                                           key_code := Keys.Tab; END;

        ELSE
          key_code := (char DIV 100H) * 100H;

        END;

      ELSIF (SET16(O.KC_CHAR) * fsflags # SET16{}) THEN
        IF virt = O.VK_BACKSPACE THEN
          key_code := Keys.BackSpace;
        ELSIF (virt = O.VK_ENTER) OR (virt = O.VK_NEWLINE) THEN
          key_code := Keys.Enter;
        ELSE
          key_code := char MOD 100H;
        END;

      ELSIF (fsflags = SET16(O.KC_SCANCODE + O.KC_ALT)) THEN
        IF (scan >= 2) AND (scan <= 0bh) THEN
          key_code := (scan+76h) * 100h;
        ELSE
          key_code := scan * 100H;
        END;

      ELSIF (fsflags = SET16(O.KC_SCANCODE + O.KC_CTRL)) THEN
        IF    (scan = 2bh)  THEN  key_code := Keys.CtrlBSlash;
        ELSIF (char >= 60h) THEN  key_code := char - 60h; END;
      END;

      IF (key_code # 0) THEN
        O.DosWaitEventSem (hevProcessing, O.SEM_INDEFINITE_WAIT);
        eventKey.key_code := key_code;
        eventKey.kind     := KeyEvent;
        O.DosPostEventSem(hevEventTaken);
      END;
    END;

  |O.WM_COMMAND:
    CASE O.ULONGFROMMP(m1) OF
    |ITEM_FONT:
      O.WinSendMsg(hWnd,MM_CHANGEFONT,O.MPARAM(1),NIL);
    ELSE
      RETURN O.WinDefWindowProc(hWnd,ulMsg,m1,m2);
    END;

  | O.WM_CLOSE:
    wrProfile();
    hwndConsole      := 0;
    hwndConsoleFrame := 0;
    RETURN O.WinDefWindowProc(hWnd,ulMsg,m1,m2);

  ELSE
    RETURN O.WinDefWindowProc(hWnd,ulMsg,m1,m2);
  END; (* CASE*)

  RETURN O.MRESULT(FALSE);
END wpConsole;


PROCEDURE ["SysCall"] WinThread(par: O.ULONG);

  PROCEDURE kill();
  BEGIN
    IF (hSwitch          # 0) THEN O.WinRemoveSwitchEntry(hSwitch);          END;
    IF (hwndConsole      # 0) THEN O.WinDestroyWindow    (hwndConsole);      END;
    IF (hwndConsoleFrame # 0) THEN O.WinDestroyWindow    (hwndConsoleFrame); END;
    IF (hMsgQ            # 0) THEN O.WinDestroyMsgQueue  (hMsgQ);            END;
    IF (hAB              # 0) THEN O.WinTerminate        (hAB);              END;
    hwndConsole      := 0;
    hwndConsoleFrame := 0;
    IF (hevKilled # 0) THEN O.DosPostEventSem(hevKilled); END;
    O.DosExit(O.EXIT_THREAD, 0);
  END kill;

VAR SwData  :O.SWCNTRL;
BEGIN
  hAB   := O.WinInitialize(0);
  hMsgQ := O.WinCreateMsgQueue(hAB,32);
  IF (hMsgQ = 0) THEN kill(); RETURN; END;

  IF NOT O.WinRegisterClass(hAB
                           ,CONSOLECLASSNAME
                           ,wpConsole
                           ,O.CS_SIZEREDRAW+O.CS_CLIPCHILDREN
                           ,4) THEN
    kill(); RETURN;
  END;

  fcdata.cb := SIZE(O.FRAMECDATA);
  fcdata.hmodResources := 0;
  fcdata.idResources   := RES_MAIN;
  fcdata.flCreateFlags := O.FCF_TITLEBAR + O.FCF_SYSMENU + O.FCF_MINMAX + O.FCF_ICON +
                          O.FCF_BORDER + O.FCF_SHELLPOSITION + O.FCF_NOBYTEALIGN;

  hwndConsoleFrame := O.WinCreateWindow(O.HWND_DESKTOP,      (* parent window          *)
                                        O.WC_FRAME,          (* class name             *)
                                        "XD",                (* window text            *)
                                        0,                   (* window style           *)
                                        0,0,                 (* position (x,y)         *)
                                        0,0,                 (* size (width,height)    *)
                                        0,                   (* owner window           *)
                                        O.HWND_BOTTOM,       (* sibling window         *)
                                        0,                   (* window id              *)
                                        ADR(fcdata),         (* control data           *)
                                        NIL);                (* presentation parms     *)

  hwndConsole := O.WinCreateWindow(     hwndConsoleFrame,    (* parent window          *)
                                        CONSOLECLASSNAME,    (* class name             *)
                                        NIL,                 (* window text            *)
                                        0,                   (* window style           *)
                                        0, 0,                (* position (x,y)         *)
                                        0, 0,                (* size (width,height)    *)
                                        hwndConsoleFrame,    (* owner window           *)
                                        O.HWND_TOP,          (* sibling window         *)
                                        O.FID_CLIENT,        (* window id              *)
                                        NIL,                 (* control data           *)
                                        NIL);                (* presentation parms     *)

  IF hwndConsole = 0 THEN kill(); RETURN; END;

  (* Add switch entry to the system Window List *)
  WITH SwData DO
    hwnd            := hwndConsoleFrame;
    hwndIcon        := 0;
    hprog           := 0;
    idProcess       := 0;
    idSession       := 0;
    uchVisibility   := O.SWL_VISIBLE;
    fbJump          := O.SWL_JUMPABLE;
    szSwtitle       := "XDS Debugger";
  END;
  hSwitch := O.WinAddSwitchEntry(SwData);

  O.WinPostMsg(hwndConsole, MM_START, NIL, NIL);

  (* Main message loop *)
  WHILE O.WinGetMsg(hAB, qMsg, 0, 0, 0) DO
    O.WinDispatchMsg(hAB, qMsg);
  END;

  kill();

END WinThread;


PROCEDURE IniConsole(VAR X, Y: CARDINAL): RESULT;
VAR i: CARDINAL;
BEGIN
  IF (X = 0) OR (Y = 0) THEN X := 80; Y := 25; END;
  IF ( X < 80 ) OR ( Y < 25 ) OR (X > 200) OR (Y > 200) THEN RETURN WrongParams; END;

  xSize := X;
  ySize := Y;

  IF (lScreen # NIL) THEN DISPOSE(lScreen); END;
  NEW (lScreen, X*Y);
  FOR i:=0 TO X*Y-1 DO
    lScreen^[i].ch   := ' ';
    lScreen^[i].attr := 0;
  END;

  O.DosCreateEventSem(NIL, hevReady,   0, FALSE);
  O.DosCreateEventSem(NIL, hevKilled,  0, FALSE);
  O.DosCreateThread(win_tid, WinThread, 0, 0, 0FFFFH);
  O.DosWaitEventSem(hevReady,5000);
  O.DosCloseEventSem(hevReady);
  hevReady := 0;

  IF (hwndConsole = 0) THEN
    RETURN Error;
  ELSE
    IF (fModalState) THEN
      O.WinSetSysModalWindow(O.HWND_DESKTOP,hwndConsoleFrame);
    ELSE
      O.WinSetWindowPos(hwndConsoleFrame,O.HWND_TOP,0,0,0,0,O.SWP_SHOW+O.SWP_ACTIVATE+O.SWP_ZORDER);
    END;
    IF (OutMon.hFrame # 0) THEN  O.WinShowWindow(OutMon.hFrame,FALSE); END;
    RETURN Ok;
  END;

END IniConsole;



PROCEDURE CloseConsole();
BEGIN
  O.DosPostEventSem (hevEventTaken);
  O.DosCloseEventSem(hevEventTaken);
  O.DosPostEventSem (hevProcessing);
  O.DosCloseEventSem(hevProcessing);
  IF (hwndConsoleFrame # 0) THEN
    O.WinPostMsg(hwndConsoleFrame,O.WM_CLOSE, NIL, NIL);
  END;
  O.DosWaitEventSem(hevKilled,5000);
  O.DosCloseEventSem(hevKilled);
  hevKilled := 0;

  IF (OutMon.hFrame # 0) THEN
    O.WinShowWindow(OutMon.hFrame,TRUE);
  END;
END CloseConsole;



PROCEDURE OutSymbol(x,y: CARDINAL; ch: CHAR; attr: ATTR);
BEGIN
  IF (x >= xSize) OR (y >= ySize) THEN RETURN; END;
  lScreen^[xSize*y + x].ch   := ch;
  lScreen^[xSize*y + x].attr := attr;
  Draw2Bmp  (x,y,1,1);
  PaintBlock(x,y,1,1);
END OutSymbol;



PROCEDURE OutRect(screen: LOGICAL_SCREEN; x1, y1, x2, y2: CARDINAL);
VAR x,y: CARDINAL;
BEGIN
  IF (x1 >= xSize) OR (y1 >= ySize) THEN RETURN; END;
  IF (x2 >= xSize) THEN x2 := xSize-1; END;
  IF (y2 >= ySize) THEN y2 := ySize-1; END;
  IF (x2<x1) OR (y2<y1) THEN RETURN; END;

  FOR x:=x1 TO x2 DO
    FOR y:=y1 TO y2 DO
      lScreen^[xSize*y + x] := screen^[xSize*y + x];
    END;
  END;
  Draw2Bmp  (x1, y1, x2-x1+1, y2-y1+1);
  PaintBlock(x1, y1, x2-x1+1, y2-y1+1);
END OutRect;


PROCEDURE Flush();
VAR  ul  : O.ULONG;
BEGIN
  O.DosResetEventSem(hevFlushing, ul);
  O.WinPostMsg(hwndConsole,MM_FLUSH,NIL,NIL);
  O.DosWaitEventSem (hevFlushing, O.SEM_INDEFINITE_WAIT);
END Flush;

PROCEDURE CursorOn;
VAR rcl : O.RECTL;
BEGIN
  IF (O.WinQueryFocus(O.HWND_DESKTOP) # hwndConsole) THEN
    O.WinDestroyCursor(hwndConsole);
  ELSE
    rcl.xLeft := 0; rcl.yBottom := 0; rcl.xRight := 7FFFH; rcl.yTop := 7FFFH;
    O.WinCreateCursor(hwndConsole, xCurs * xCell, yWin - yCurs*yCell - yCell, 2, yCell, O.CURSOR_FLASH, rcl);
    O.WinShowCursor(hwndConsole, TRUE);
  END;
  cursorON := TRUE;
END CursorOn;



PROCEDURE CursorOff;
BEGIN
  O.WinDestroyCursor(hwndConsole);
  cursorON := FALSE;
END CursorOff;



PROCEDURE CursorState(): BOOLEAN;
BEGIN
  RETURN cursorON;
END CursorState;



PROCEDURE SetCurPos(x,y: CARDINAL);
BEGIN
  xCurs := x;
  yCurs := y;
  CursorOn();
END SetCurPos;



PROCEDURE Beep;
BEGIN
  O.DosBeep(500,  20);
  O.DosBeep(1000, 150);
END Beep;


PROCEDURE ToggleConsoles(screen: LOGICAL_SCREEN);
(*-- Not used in PM mode --*)
BEGIN
END ToggleConsoles;


(* ==========================  События консоли  ====================================== *)
(* ==========================  События консоли  ====================================== *)
(* ==========================  События консоли  ====================================== *)


PROCEDURE GetEvent(VAR event: EVENT);
VAR
  count: CARDINAL;
BEGIN
  O.DosPostEventSem(hevSetModal);
  O.DosWaitEventSem (hevEventTaken, O.SEM_INDEFINITE_WAIT);
  O.DosResetEventSem(hevEventTaken, count);

  O.DosResetEventSem(hevProcessing, count);
  IF (eventMouse.kind # None) THEN
    event           := eventMouse;
    eventMouse.kind := None;
  ELSIF (eventKey.kind # None) THEN
    event         := eventKey;
    eventKey.kind := None;
  ELSE
    ASSERT(FALSE);
  END;
  O.DosPostEventSem(hevProcessing);
END GetEvent;


PROCEDURE QueryEvent(kind: EVENT_KIND): BOOLEAN;
BEGIN
  O.DosPostEventSem(hevSetModal);
  CASE kind OF
  | KeyEvent:   RETURN eventKey.kind   # None;
  | MouseEvent: RETURN eventMouse.kind # None;
  ELSE          RETURN FALSE;
  END;
END QueryEvent;



PROCEDURE ["SysCall"] PMHack(dummy: O.ULONG);
VAR
  ul  : O.ULONG;
  hFr : O.HWND;
BEGIN
  LOOP
    O.DosWaitEventSem (hevSetModal, O.SEM_INDEFINITE_WAIT);
    O.DosResetEventSem(hevSetModal, ul);
    -- Set modal mode
    fModalState := TRUE;
    IF (hwndConsoleFrame # 0) THEN
       O.WinSetSysModalWindow(O.HWND_DESKTOP,hwndConsoleFrame);
    END;
--    O.DosBeep(200,50);

    O.DosWaitEventSem(Program_started_2, O.SEM_INDEFINITE_WAIT);
    -- Set normal mode
    fModalState := FALSE;
    IF (hwndConsoleFrame # 0) THEN
       O.WinSetSysModalWindow(O.HWND_DESKTOP,0);
    END;
--    O.DosBeep(1000,50);

  END;
END PMHack;


PROCEDURE Start_PM_Hack(): BOOLEAN;
BEGIN
  utl.StartSmartSelector;
  RETURN O.DosCreateThread(hack_tid,
                           PMHack,
                           0,
                           O.CREATE_READY+O.STACK_COMMITTED,
                           0FFFFH) = O.NO_ERROR;
END Start_PM_Hack;


(* Delay d fraction second (1 fraction = 1/100 second) *)
PROCEDURE Delay (d: CARDINAL);
BEGIN
  O.DosSleep (d*10);
END Delay;


BEGIN
  hevReady         := 0;
  hevKilled        := 0;
  hwndConsole      := 0;
  hwndConsoleFrame := 0;
  hpsBmp           := 0;
  hSwitch          := 0;
  hMsgQ            := 0;
  hAB              := 0;
  fModalState      := FALSE;
  lScreen          := NIL;
  cursorON         := FALSE;
  xCurs            := 0;
  yCurs            := 0;
  hevSetModal      := 0;
  hevEventTaken    := 0;
  hevProcessing    := 0;
  eventMouse.kind  := None;
  eventKey.kind    := None;
  xLeftIni         := -1000;
  O.DosCreateEventSem(NIL, hevSetModal,     0, FALSE);
  O.DosCreateEventSem(NIL, hevEventTaken,   0, FALSE);
  O.DosCreateEventSem(NIL, hevProcessing,   0, TRUE);
  O.DosCreateEventSem(NIL, hevFlushing,     0, TRUE);

END Console.


