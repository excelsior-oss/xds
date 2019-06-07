
(* Copyright (C)1996,98 XDS Ltd. Russia *)

<* +M2ADDTYPES   *>
<* +M2EXTENSIONS *>

<* -IOVERFLOW    *>
<* -COVERFLOW    *>
<* -CHECKINDEX   *>
<* -CHECKRANGE   *>
<* -CHECKNIL     *>

IMPLEMENTATION MODULE Graph;

IMPORT SYSTEM,
       W    := Windows,
       Strings, LM := LongMath,
       xtsIGraph,
       GComm := xtsGComm;

FROM SYSTEM IMPORT ADR, ADDRESS, FILL, CARD8;

FROM Windows IMPORT LOWORD, HIWORD;


CONST
  MAINCLASSNAME = "GraphWnd";


VAR
  GObj          :GComm.tGObj;  -- "shared memory"

  persistentHDC :W.HDC;

  xLeftC, yTopC :CARDINAL;  -- redo !!!!!!!!!!!!!

  consoleHandle  :W.HWND;
  mainWndHandle  :W.HWND;

  hasInitialized :BOOLEAN;
  hasExited      :BOOLEAN;

------------------------------------ Semaphores

VAR
  hservReadySem,
  hclReadySem,
  restartSem,
  hScrMutex    :W.HANDLE;


---- client interface


PROCEDURE waitServerReady();
BEGIN
  W.WaitForSingleObject (hservReadySem, W.INFINITE);
END waitServerReady;


PROCEDURE waitServerReadyB();
BEGIN
  W.WaitForSingleObject (hservReadySem, W.INFINITE);
  W.ResetEvent (hservReadySem);
END waitServerReadyB;


PROCEDURE releaseServerReady();
BEGIN
  W.SetEvent (hservReadySem);
END releaseServerReady;


-- server interface

PROCEDURE waitClientReadyB();
BEGIN
  W.WaitForSingleObject (hclReadySem, W.INFINITE); -- autoevent
END waitClientReadyB;


(* this procedure signals to the server to start *)
PROCEDURE releaseClientReady();
BEGIN
  W.SetEvent ( hclReadySem );  -- go, go...
END releaseClientReady;



-- screen access

PROCEDURE waitScreen;
BEGIN
  W.WaitForSingleObject ( hScrMutex, W.INFINITE );
END waitScreen;

PROCEDURE releaseScreen;
BEGIN
  W.ReleaseMutex( hScrMutex );
END releaseScreen;

-----------------------------------------------

(* status module vars *)

VAR
  BackColor    :LONGCARD;
  LineStyle    :CARDINAL;
  SFillMask    :LONGINT;
  FillMask     :FillMaskType;
  isStdMask    :BOOLEAN;

(* text proc vars *)
  curStatus    :BOOLEAN;
  wrapMode     :BOOLEAN;
  curPos       :TextCoords;
  r1W, c1W,
  r2W, c2W     :LONGCARD;  -- current window
  txtColor     :LONGCARD;

  fontXd,                 -- font size
  fontYd,
  fontDescend :LONGCARD;


PROCEDURE myMIN (a, b :LONGCARD) :LONGCARD;
BEGIN
  IF (a > b) THEN RETURN b; ELSE RETURN a; END;
END myMIN;

PROCEDURE myMAX (a, b :LONGCARD) :LONGCARD;
BEGIN
  IF (a < b) THEN RETURN b; ELSE RETURN a; END;
END myMAX;

PROCEDURE Adjust ( VAR obj :LONGCARD; min, max :LONGCARD );
BEGIN
  IF ( obj < min )
    THEN  obj := min;
    ELSIF ( obj > max )
      THEN obj := max;
  END;
END Adjust;


----------------------------------------------------------------------------------

MODULE BitMap;

IMPORT W,
       Width, Depth;


EXPORT QUALIFIED hdcM,
                 Create, Delete, Draw, DrawRect,
                 GainAccess, Relinquish;

VAR
  hdcM      :W.HDC;
  hbm       :W.HBITMAP;
  hbmpMutex :W.HANDLE;


(*
   creates of the screen bitmap and its DC
    WARNING : Create can be issued only after initialization the Depth/Width vars
*)
PROCEDURE Create (hdc :W.HDC);
BEGIN
  hdcM := W.CreateCompatibleDC (hdc);

  hbm := W.CreateCompatibleBitmap (hdc, Width, Depth);

  W.SelectObject ( hdcM, hbm );
END Create;

PROCEDURE GainAccess;
BEGIN
  W.WaitForSingleObject ( hbmpMutex, W.INFINITE );
END GainAccess;

PROCEDURE Relinquish;
BEGIN
  W.ReleaseMutex( hbmpMutex );
END Relinquish;


PROCEDURE Draw( hdc :W.HDC );
BEGIN
  GainAccess;
  W.BitBlt ( hdc,         -- handle to destination device context
             0, 0,        -- x/y coordinate of destination rectangle's upper-left corner
             Width, Depth,
             hdcM,        -- handle to source device context
             0, 0,        -- x/y coordinate of source rectangle's upper-left corner
             W.SRCCOPY    -- raster operation code
           );
  Relinquish;
END Draw;


PROCEDURE DrawRect ( hdc :W.HDC; x, y, w, h :CARDINAL );
BEGIN
  GainAccess;
  W.BitBlt ( hdc,         -- handle to destination device context
             x, y,        -- x/y coordinate of destination rectangle's upper-left corner
             w, h,
             hdcM,        -- handle to source device context
             x, y,        -- x/y coordinate of source rectangle's upper-left corner
             W.SRCCOPY    -- raster operation code
           );
  Relinquish;
END DrawRect;


PROCEDURE Delete();
BEGIN
  W.DeleteObject (hbm);
  W.DeleteDC (hdcM);
END Delete;

BEGIN
  hbmpMutex := W.CreateMutex (NIL, FALSE, NIL);
FINALLY
  W.CloseHandle (hbmpMutex);
END BitMap;



MODULE copyBmp;

IMPORT W;

EXPORT QUALIFIED Init, Exit, Paste, Copy, Delete,
                 hdcMem;


VAR
  hdcMem      :W.HDC;
  hDefaultBmp :W.HBITMAP;

PROCEDURE Copy (hdc :W.HDC; area :W.RECT) :W.HBITMAP;
(* ASSERT : area is ordered *)
VAR
  hb, hbt :W.HBITMAP;
  width,
  depth   :CARDINAL;
BEGIN
  WITH area DO
    width := right-left+1;
    depth := bottom-top+1;
  END;

  hb := W.CreateCompatibleBitmap ( hdc, width, depth );
  W.SetBitmapDimensionEx ( hb, width, depth, NIL );


  hbt := W.SelectObject (hdcMem, hb);
  IF (hDefaultBmp = NIL) THEN hDefaultBmp := hbt END;

  W.BitBlt ( hdcMem,       -- handle to destination device context
             0, 0,         -- x/y coordinate of destination rectangle's upper-left corner
             width, depth,
             hdc,          -- handle to source device context
             area.left,
             area.top,     -- x/y coordinate of source rectangle's upper-left corner
             W.SRCCOPY     -- raster operation code
           );

  RETURN hb;
END Copy;


PROCEDURE Paste (hdc :W.HDC; corn :W.SIZEL; hb :W.HBITMAP; mode :LONGCARD);
VAR
  sz  :W.SIZEL;
BEGIN
  W.GetBitmapDimensionEx (hb, sz);

  W.SelectObject (hdcMem, hb);

  W.BitBlt ( hdc,          -- handle to destination device context
             corn.cx,
             corn.cy,      -- x/y coordinate of destination rectangle's upper-left corner
             sz.cx, sz.cy,
             hdcMem,       -- handle to source device context
             0, 0,         -- x/y coordinate of source rectangle's upper-left corner
             mode          -- raster operation code
           );
END Paste;


PROCEDURE Delete (hb :W.HBITMAP);
BEGIN
  W.SelectObject (hdcMem, hDefaultBmp);
  W.DeleteObject (hb);
END Delete;


PROCEDURE Init (hdc :W.HDC);
BEGIN
  hdcMem := W.CreateCompatibleDC (hdc);
END Init;

PROCEDURE Exit();
BEGIN
  W.DeleteDC (hdcMem);
END Exit;

BEGIN
  hDefaultBmp := NIL;
END copyBmp;




MODULE RGB;

IMPORT W,
       GComm;

FROM SYSTEM IMPORT ADR, SHIFT;

EXPORT QUALIFIED numColors, MapAllCol;

VAR
  numColors :CARDINAL;

(* obtain screen capabilities *)
PROCEDURE GetSCaps();
VAR
  hdc     :W.HDC;
  planes,
  bitcnt  :INTEGER;
BEGIN
  hdc := W.CreateDC ("DISPLAY", NIL, NIL, NIL );
  bitcnt := W.GetDeviceCaps (hdc, W.BITSPIXEL );
  planes := W.GetDeviceCaps (hdc, W.PLANES );
  W.DeleteDC ( hdc );
  numColors := CARDINAL( SHIFT( BITSET(1), bitcnt*planes) );
END GetSCaps;


PROCEDURE MapAllCol ( hdc :W.HDC;  rgb- :ARRAY OF LONGCARD; nc2Map :LONGCARD );
TYPE
  PalT = RECORD
           CASE : BOOLEAN OF
           | TRUE :
             logpal        :W.LOGPALETTE;
           | FALSE :
             palVersion    :W.WORD;
             palNumEntries :W.WORD;
             pal           :ARRAY [0..GComm._mxNColor-1] OF W.PALETTEENTRY;
           END;
         END;
VAR
  palS :PalT;
  hpal :W.HPALETTE;
  i    :CARDINAL;
BEGIN
  WITH palS DO
    palVersion    := 300H;
    palNumEntries := HIGH (rgb)+1;
    FOR i:=0 TO palNumEntries-1 DO
      WITH pal[i] DO
        peFlags := W.PC_SET {};                       -- contains RGB-colours
        peRed   := W.GetBValue (W.COLORREF(rgb[i]));
        peGreen := W.GetGValue (W.COLORREF(rgb[i]));  -- input is 0RGB (not 0BGR)
        peBlue  := W.GetRValue (W.COLORREF(rgb[i]));
      END;
    END;
  END;
  hpal := W.CreatePalette ( palS.logpal );
  W.DeleteObject ( W.HGDIOBJ(W.SelectPalette ( hdc, hpal, TRUE )) );  -- temp !!
  W.RealizePalette ( hdc );
END MapAllCol;


BEGIN
  GetSCaps();
END RGB;



(*-----------------------------------------  Draw object ----------------------------------------*)



MODULE Tools;

IMPORT W;

FROM SYSTEM IMPORT ADR;

(* import from Graph *)
IMPORT PATSYM_SOLID;

EXPORT QUALIFIED SelectColor, ReplaceDefault,
                 SetLineStyle, SetStdBrushStyle, SetBrushStyle;


VAR                      -- to create indirect
  curPen   :W.LOGPEN;
  curBrush :W.LOGBRUSH;


VAR
  dflBrush     :W.HBRUSH;
  dflPen       :W.HPEN;
  brushCreated :W.HBRUSH;
  penCreated   :W.HPEN;


VAR
  isStdBS  :BOOLEAN;

  brushStyle :W.BS_ENUM;
  penStyle   :W.PS_SET;

PROCEDURE SetStdBrushStyle ( bs :LONGINT );
BEGIN
  isStdBS := TRUE;
  IF ( bs = PATSYM_SOLID ) THEN
    brushStyle := W.BS_SOLID;
  ELSE
    brushStyle       := W.BS_HATCHED;
    curBrush.lbHatch := bs;
  END;
END SetStdBrushStyle;


VAR
  prevPattBmp :W.HBITMAP;

PROCEDURE SetBrushStyle ( bh :W.HBITMAP );
BEGIN
  isStdBS := FALSE;
  IF ( prevPattBmp # NIL )
    THEN W.DeleteObject ( prevPattBmp )
  END;

  prevPattBmp := bh;
  curBrush.lbHatch := W.LONG(bh);
END SetBrushStyle;


PROCEDURE SetLineStyle ( ls :LONGINT );
BEGIN
  penStyle := W.PS_SET(ls);
END SetLineStyle;


PROCEDURE SelectColor ( hdc :W.HDC; c :W.COLORREF; fillMode :BOOLEAN );
BEGIN
  IF (fillMode) THEN
    curPen.lopnStyle := W.PS_SOLID;
    IF (isStdBS) THEN
      curBrush.lbStyle := brushStyle
    ELSE
      W.SetTextColor ( hdc, c);
      curBrush.lbStyle := W.BS_PATTERN;
    END;
  ELSE
    curPen.lopnStyle := penStyle;
    curBrush.lbStyle := W.BS_HOLLOW;
  END;

  curPen.lopnColor := c;
  penCreated := W.CreatePenIndirect ( curPen );
  dflPen := W.SelectObject ( hdc, penCreated );

  curBrush.lbColor := c;
  brushCreated     := W.CreateBrushIndirect ( curBrush );
  dflBrush := W.SelectObject (hdc, brushCreated);
END SelectColor;


PROCEDURE ReplaceDefault ( hdc :W.HDC );
BEGIN
  W.SelectObject ( hdc, dflBrush );
  W.DeleteObject ( brushCreated );

  W.SelectObject ( hdc, dflPen );
  W.DeleteObject ( penCreated );
END ReplaceDefault;


BEGIN
  isStdBS     := TRUE;
  prevPattBmp := NIL;
  brushStyle  := W.BS_SOLID;
  penStyle    := W.PS_SOLID;

  curPen.lopnWidth := W.POINT{1,0};    -- one pixel width
END Tools;


VAR
  currentHRGN :W.HRGN;

PROCEDURE DrawObj ( hdc :W.HDC );
VAR
  cp, bkr :W.SIZEL;
  i       :CARDINAL;
  c       :ARRAY [0..0] OF CHAR;
BEGIN

  WITH GObj DO
  CASE obj OF
    |GComm.setHatch:    Tools.SetStdBrushStyle ( htStyle );

    |GComm.setLnStyle:  Tools.SetLineStyle ( lnStyle );



    |GComm.setBkC:    W.SetBkColor ( hdc, W.PALETTEINDEX (bkColor) );

    |GComm.setBkMix:  W.SetBkMode ( hdc, bkMix );

    |GComm.mapAllcol: RGB.MapAllCol ( persistentHDC, curPalette, numc2map );
                      RGB.MapAllCol ( BitMap.hdcM, curPalette, numc2map );
                      RGB.MapAllCol ( copyBmp.hdcMem, curPalette, numc2map );

    |GComm.plot:      W.SetPixelV ( hdc, pointX, pointY, W.PALETTEINDEX (pColor) );

    |GComm.point:     --W.SetPixel ( persistentHDC, 0, 0, W.PALETTEINDEX (0) );

    |GComm.rect:
                     Tools.SelectColor ( hdc, W.PALETTEINDEX (rColor), rFill );

                     WITH rRect DO
                       W.Rectangle ( hdc, left, top, right+1, bottom+1 )
                     END;

                     Tools.ReplaceDefault ( hdc );



    | GComm.line:
                     IF ( lineX1 <> lineX2 ) OR
                        ( lineY1 <> lineY2 ) THEN
                       Tools.SelectColor ( hdc, W.PALETTEINDEX (lineColor ), FALSE );
                       W.MoveToEx ( hdc, lineX1, lineY1, NIL );
                       W.LineTo   ( hdc, lineX2, lineY2 );
                     END;
                     W.SetPixelV( hdc, lineX2, lineY2, W.PALETTEINDEX(lineColor) );

                     Tools.ReplaceDefault ( hdc );

    | GComm.ellip:
                     Tools.SelectColor ( hdc,  W.PALETTEINDEX (eColor ), eFill );

                     IF (eFill) THEN
                       W.Ellipse( hdc, eX1,eY1, eX2, eY2 );
                     ELSE
                       W.Arc ( hdc, eX1,eY1, eX2, eY2, 0, 0, 0, 0 );
                     END;

                     Tools.ReplaceDefault ( hdc );

    | GComm.poly:    Tools.SelectColor ( hdc,  W.PALETTEINDEX ( polyColor ), polyFill );

                     IF (polyFill) THEN
                       W.Polygon ( hdc, polyNodes, polyN );
                     ELSE
                       W.Polyline ( hdc, polyNodes, polyN );
                     END;

                     Tools.ReplaceDefault ( hdc );

    | GComm.flood:   Tools.SelectColor ( hdc,  W.PALETTEINDEX ( floodColor ), TRUE );

                     W.FloodFill ( hdc, seedpX, seedpY, W.PALETTEINDEX ( boundColor ) );

                     Tools.ReplaceDefault ( hdc );


    | GComm.clip:
                     IF ( currentHRGN # NIL) THEN
                       W.DeleteObject ( currentHRGN );
                     END;
                     currentHRGN := W.CreateRectRgnIndirect ( region );
                     W.SelectObject ( hdc, currentHRGN );
                     W.SelectClipRgn ( hdc, currentHRGN );

    |GComm.cancelClip: W.SelectClipRgn ( hdc, NIL );
                       IF ( currentHRGN # NIL) THEN
                         W.DeleteObject ( currentHRGN )
                       END;
                       currentHRGN := NIL;


    | GComm.arc:    Tools.SelectColor ( hdc,  W.PALETTEINDEX ( arcColor ), FALSE );
                    --W.SetArcDirection( hdc, W.AD_CLOCKWISE );

                     W.Arc ( hdc, arcR.left, arcR.top,  arcR.right, arcR.bottom,
                             arcXStart, arcYStart, arcXEnd, arcYEnd );
                     Tools.ReplaceDefault ( hdc );


    | GComm.pie:    Tools.SelectColor ( hdc,  W.PALETTEINDEX ( pieColor ), pieFill );
                     W.Pie ( hdc, pieR.left, pieR.top, pieR.right, pieR.bottom,
                             pieXStart, pieYStart, pieXEnd, arcYEnd );
                    Tools.ReplaceDefault ( hdc );


    | GComm.getimg:  rimgHandle := copyBmp.Copy ( hdc, imgArea );

    | GComm.putimg:  copyBmp.Paste ( hdc, imgLCorn, imgHandle, imgMode );

    | GComm.delimg:  copyBmp.Delete (delImgHandle);


    |GComm.rtext:
                    W.SetTextColor ( hdc, W.PALETTEINDEX ( rtxColor ) );
                    W.TextOut ( hdc, rtxPos.cx, rtxPos.cy, rtxText, LENGTH (rtxText) );

    |GComm.cursor:  W.InvertRect ( W.HDC(hdc), cursorArea);

    |GComm.text    : cp  := txPos;
                     bkr := txPos;

                     Tools.SelectColor ( hdc, W.PALETTEINDEX (txBgr), TRUE );
                     WITH bkr DO
                       W.Rectangle ( hdc, bkr.cx, bkr.cy,
                                          bkr.cx+INT(txFontXd*LENGTH(txText))-1,
                                          bkr.cy+INT(txFontYd+txfontDescend) );
                     END;
                     Tools.ReplaceDefault ( hdc );

                     W.SetTextColor ( hdc, W.PALETTEINDEX ( txColor ) );

                     FOR i:=0 TO LENGTH(txText)-1 DO
                       c [0] := txText [i];
                       W.TextOut ( hdc, cp.cx, cp.cy, c, 1 );
                       INC (cp.cx, txFontXd );
                     END;

    | ELSE ;
  END;
  END;  (* WITH *)
END DrawObj;


PROCEDURE drawOnce();
BEGIN
  DrawObj (BitMap.hdcM);
END drawOnce;

VAR
  autoUpdateMode :BOOLEAN;

PROCEDURE drawTwice();
BEGIN
  DrawObj (BitMap.hdcM);
  IF (autoUpdateMode) THEN
    DrawObj (persistentHDC);
  END;
END drawTwice;

PROCEDURE Update();
BEGIN
  BitMap.Draw (persistentHDC);
END Update;


PROCEDURE UpdateRect (x1, y1, x2, y2 :LONGCARD);
VAR
  xx1, xx2, yy1, yy2 :CARDINAL;
BEGIN
  xx1 := myMIN (x1, x2);
  xx2 := myMAX (x1, x2);
  yy1 := myMIN (y1, y2);
  yy2 := myMAX (y1, y2);

  BitMap.DrawRect (persistentHDC, xx1, yy1, xx2-xx1+1, yy2-yy1+1);
END UpdateRect; 


VAR
  isStartAct     :BOOLEAN;
  servThrHandle  :W.HANDLE;

PROCEDURE InitStdPalette; FORWARD;


VAR
  GWindowControlKeyState :W.CONTROLKEYSTATE_SET;

PROCEDURE [W.CALLBACK] MainWinProc ( hWnd   :W.HWND;
                                     msg    :W.UINT;
                                     wParam :W.WPARAM;
                                     lParam :W.LPARAM
                                   ) :W.LRESULT;
VAR
  dc        :W.HDC;
  ps        :W.PAINTSTRUCT;
  ignore    :CARDINAL;

-----
  ConInpRec :ARRAY [0..0] OF W.INPUT_RECORD;

  PROCEDURE makeInputEvent (buttonDown :BOOLEAN);
  VAR
    code       :CARDINAL;
    enhanced   :BOOLEAN;
    virtCode   :CARDINAL;
  BEGIN
    code      := (lParam DIV 10000H);
    virtCode   := code MOD 100H;
    enhanced   := (virtCode > 0FFH);

    IF (enhanced) THEN
      INCL(GWindowControlKeyState, W.ENHANCED__KEY);
    ELSE
      EXCL(GWindowControlKeyState, W.ENHANCED__KEY);
    END;

    ConInpRec[0].EventType := W.KEY_EVENT;
    WITH ConInpRec[0].Event.KeyEvent DO
      bKeyDown          := buttonDown;
      wRepeatCount      := 1;
      wVirtualKeyCode   := 0; -- !!!!
      wVirtualScanCode  := virtCode MOD 100;
      uChar.UnicodeChar := 0;
      dwControlKeyState := GWindowControlKeyState;
    END;

    (* make control key state *)
    CASE (virtCode) OF
      |38H  :     IF (buttonDown) THEN
                    INCL(GWindowControlKeyState, W.LEFT__ALT_PRESSED);
                  ELSE
                    EXCL(GWindowControlKeyState, W.LEFT__ALT_PRESSED);
                  END;

      |138H :     IF (buttonDown) THEN
                    INCL(GWindowControlKeyState, W.RIGHT__ALT_PRESSED);
                  ELSE
                    EXCL(GWindowControlKeyState, W.RIGHT__ALT_PRESSED);
                  END;
      |1DH  :     IF (buttonDown) THEN
                    INCL(GWindowControlKeyState, W.LEFT__CTRL_PRESSED);
                  ELSE
                    EXCL(GWindowControlKeyState, W.LEFT__CTRL_PRESSED);
                  END;

      |11DH :     IF (buttonDown) THEN
                    INCL(GWindowControlKeyState, W.RIGHT__CTRL_PRESSED);
                  ELSE
                    EXCL(GWindowControlKeyState, W.RIGHT__CTRL_PRESSED);
                  END;

      |2AH, 36H : IF (buttonDown) THEN
                    INCL(GWindowControlKeyState, W.SHIFT__PRESSED);
                  ELSE
                    EXCL(GWindowControlKeyState, W.SHIFT__PRESSED);
                  END;
    ELSE
    END;
  END makeInputEvent;
----

BEGIN
    IF (xtsIGraph.proc # NIL) THEN
      xtsIGraph.proc (hWnd, msg, wParam, lParam)
    END;
    CASE msg OF

    | W.WM_ACTIVATE:
        GWindowControlKeyState := W.CONTROLKEYSTATE_SET{};

    | W.WM_PAINT:
        dc := W.BeginPaint ( hWnd, ps );
        waitScreen;
          BitMap.Draw (persistentHDC);
        releaseScreen;

        W.EndPaint (hWnd, ps);
        RETURN 0;

    |W.WM_QUERYNEWPALETTE:
        W.RealizePalette (persistentHDC);
        RETURN 0;

    | W.WM_CLOSE:
        RETURN 0;

    | W.WM_KEYUP, W.WM_SYSKEYUP:

      makeInputEvent (FALSE);
      W.WriteConsoleInput (xtsIGraph.hConsoleInput,
                           ConInpRec,
                           1,
                           ignore);
      RETURN 0;

    | W.WM_KEYDOWN, W.WM_SYSKEYDOWN:

      makeInputEvent (TRUE);
      W.WriteConsoleInput (xtsIGraph.hConsoleInput,
                           ConInpRec,
                           1,
                           ignore);
      RETURN 0;
    ELSE
    END;
    RETURN W.DefWindowProc (hWnd, msg, wParam, lParam);
END MainWinProc;

------

PROCEDURE RegisterWindowClass (hInst :W.HINSTANCE);
VAR
  wc  :W.WNDCLASS;
BEGIN
  wc.style         := W.CS_BYTEALIGNCLIENT + W.CS_OWNDC;  --W.CS_CLASSDC;
  wc.lpfnWndProc   := MainWinProc;
  wc.cbClsExtra    := 0;
  wc.cbWndExtra    := 0;
  wc.hInstance     := hInst;
  wc.hIcon         := W.LoadIcon ( NIL, W.IDI_APPLICATION );
  wc.hCursor       := W.LoadCursor (NIL, W.RESOURCESTR(VAL (INTEGER, W.IDC_ARROW)) );
  wc.hbrBackground := W.CreateSolidBrush (W.RGB (0,0,0));  --HBRUSH(W.COLOR_BACKGROUND);

  wc.lpszMenuName  := NIL;
  wc.lpszClassName := ADR (MAINCLASSNAME);
  IF (W.RegisterClass (wc) = 0) THEN
    W.MessageBox (NIL, "Win class was not registered", NIL,
                  W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND);
    HALT;
  END;
END RegisterWindowClass;


-----

(*
  to comment !!!!!!!!!!!!!
  MODIFIES  mainWndHandle :W.HWND;
*)

PROCEDURE ["StdCall"] MsgLoopThreadProc (p :W.PVOID) :W.DWORD;

  PROCEDURE CreateWindow();
  VAR
    actualW, actualH :CARDINAL;
  BEGIN
    actualW := Width + CARDINAL(2*W.GetSystemMetrics(W.SM_CXBORDER));
    actualH := Depth +
               CARDINAL(2*W.GetSystemMetrics(W.SM_CYBORDER) +
                        W.GetSystemMetrics(W.SM_CYCAPTION));

    mainWndHandle := W.CreateWindowEx ( W.WS_EX_SET{}, -- W.WS_EX_TOPMOST
                                        MAINCLASSNAME,
                                        "Graph",
                                        W.WS_OVERLAPPED + W.WS_BORDER +
                                        W.WS_SYSMENU + W.WS_MINIMIZEBOX + W.WS_VISIBLE,
                                        xLeftC, yTopC,
                                        actualW, actualH,
                                        NIL,
                                        NIL,
                                        W.GetModuleHandle(NIL),  -- handle of mine
                                        NIL);

    IF (mainWndHandle = NIL) THEN
      W.MessageBox (NIL, "Couldn't create main window", NIL,
                    W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND);
      HALT;
    END;

    persistentHDC := W.GetDC (mainWndHandle);
    BitMap.Create (persistentHDC);
    copyBmp.Init  (persistentHDC);
  END CreateWindow;

  PROCEDURE DestroyWindow();
  BEGIN
    W.DestroyWindow (mainWndHandle);
    BitMap.Delete ();
    copyBmp.Exit  ();
  END DestroyWindow;

VAR
  msg :W.MSG;
BEGIN
  isStartAct := TRUE;

  LOOP
    CreateWindow();
    W.SetEvent (restartSem);             -- signal that window created

    WHILE W.GetMessage (msg, NIL, 0, 0) DO
      W.TranslateMessage (msg);
      W.DispatchMessage  (msg);
    END;

    DestroyWindow();

    W.SetEvent (restartSem);             -- signal that window is destroyed

    W.WaitForSingleObject (restartSem, W.INFINITE); -- autoevent
  END;
  RETURN 0;
END MsgLoopThreadProc;



----------------------------------------------------------------------------------

(*////////////////////// parms setting ////////////////////////////*)

PROCEDURE SetLinestyle (style :CARDINAL);
TYPE
  lnStyleArrayT = ARRAY [0..LNSTYLE_INVISIBLE] OF W.PS_SET;
CONST
  lnStyleArray = lnStyleArrayT{W.PS_SOLID,W.PS_DOT,W.PS_DASH,
                               W.PS_DASHDOT,W.PS_DASHDOTDOT,W.PS_NULL};
BEGIN
  LineStyle := style;
  WITH GObj DO
    lnStyle := CARDINAL(lnStyleArray[style]);
    obj     := GComm.setLnStyle;
  END;
  drawOnce();
END SetLinestyle;


PROCEDURE GetLinestyle() :CARDINAL; BEGIN RETURN LineStyle END GetLinestyle;


PROCEDURE SetStdFillMask ( mask :LONGINT );
BEGIN
  isStdMask := TRUE;
  SFillMask := mask;
  WITH GObj DO
    htStyle := mask;
    obj     := GComm.setHatch;
  END;
  drawOnce();
END SetStdFillMask;


PROCEDURE GetStdFillMask() :LONGINT; BEGIN RETURN SFillMask; END GetStdFillMask;


PROCEDURE SetFillMask ( mask- :FillMaskType );
VAR
  pattern :ARRAY[0..7] OF SYSTEM.CARD16;
  i       :CARDINAL;
BEGIN
  isStdMask := FALSE;
  FillMask  := mask;
  FOR i := 0 TO 7 DO
    pattern[i] := VAL (CARDINAL, 0FFH-mask[i]);
  END;
  Tools.SetBrushStyle ( W.CreateBitmap ( 8, 8, 1, 1, pattern ) );
END SetFillMask;

PROCEDURE GetFillMask ( VAR mask :FillMaskType );
BEGIN
  mask := FillMask;
END GetFillMask;


PROCEDURE SetBkColor ( color :LONGCARD) :LONGCARD;
VAR
  c :LONGCARD;
BEGIN
  c         := BackColor;
  BackColor := color;

  WITH GObj DO
    bkColor := color;
    obj     := GComm.setBkC;
  END;
  drawTwice();

  RETURN  c;
END SetBkColor;


PROCEDURE GetBkColor() :LONGCARD; BEGIN RETURN BackColor; END GetBkColor;


PROCEDURE SetBkMix ( isOpaque :BOOLEAN );
BEGIN
  WITH GObj DO
    IF isOpaque
      THEN bkMix := W.OPAQUE;
      ELSE bkMix := W.TRANSPARENT;
    END;
    obj := GComm.setBkMix;
  END;
  drawTwice();
END SetBkMix;



PROCEDURE myRect (x0, y0, x1, y1 :LONGCARD; color :LONGCARD; fill :BOOLEAN);
BEGIN
  IF ( x0=x1 ) & ( y0=y1 ) THEN
    WITH GObj DO
      pointX := x0;
      pointY := y0;
      pColor := color;
       obj    := GComm.plot;
    END;
  ELSE
    WITH GObj DO
      WITH rRect DO
        left   := myMIN (x0, x1);
        top    := myMIN (y0, y1);
        right  := myMAX (x0, x1);
        bottom := myMAX (y0, y1);
      END;
      rColor := color;
      rFill  := fill;
      obj    := GComm.rect;
    END;
  END;
  drawTwice();
END myRect;


(*//////////////////////////// text cursor ////////////////////////////*)


PROCEDURE mySolidRect (x0, y0, x1, y1 :LONGCARD; Color :LONGCARD; Fill :BOOLEAN);
VAR
  s      :LONGINT;
  stdWas :BOOLEAN;
BEGIN
  stdWas := isStdMask;
  s := GetStdFillMask();
  SetStdFillMask( PATSYM_SOLID );
  myRect ( x0, y0, x1, y1, Color, Fill );
  IF stdWas
    THEN SetStdFillMask( s );
    ELSE SetFillMask( FillMask );
  END;
END mySolidRect;



PROCEDURE ToggleCursor();
BEGIN
  WITH GObj DO
    WITH cursorArea DO
      left   := (curPos.col-1) * fontXd;
      right  := (curPos.col) * fontXd -1;
      top    := (curPos.row-1) * fontYd;
      bottom := (curPos.row) * fontYd -1;
    END;
    obj    := GComm.cursor;
  END;
  drawTwice();
END ToggleCursor;


VAR
  curDisplayed :BOOLEAN;

PROCEDURE CursorOff();
BEGIN
  IF (curDisplayed) THEN
    ToggleCursor();
    curDisplayed := FALSE;
  END;
END CursorOff;

PROCEDURE CursorOn();
BEGIN
  IF NOT curDisplayed THEN
    ToggleCursor();
    curDisplayed := TRUE;
  END;
END CursorOn;


PROCEDURE DisplayCursor ( Toggle :BOOLEAN) :BOOLEAN;
VAR
  b :BOOLEAN;
BEGIN
  b        := curStatus;
  curStatus := Toggle;
  IF Toggle THEN
    CursorOn();
  ELSE
    CursorOff();
  END;
  RETURN b;
END DisplayCursor;


PROCEDURE DisplayCurs ( Toggle :BOOLEAN);
VAR
  b :BOOLEAN;
BEGIN
  b := DisplayCursor ( Toggle );
END DisplayCurs;


PROCEDURE SetClipRgn ( x0, y0, x1, y1 :LONGCARD );
BEGIN
  WITH GObj DO
    region.left   := myMIN (x0, x1);
    region.bottom := myMIN (y0, y1);
    region.right  := myMAX (x0, x1)+1;
    region.top    := myMAX (y0, y1)+1;
    obj           := GComm.clip;
  END;
  drawTwice();
END SetClipRgn;

PROCEDURE CancelClipRgn ();
BEGIN
  GObj.obj := GComm.cancelClip;
  drawTwice();
END CancelClipRgn;


(*//////////////////////////// graph primitives /////////////////////////////////*)


PROCEDURE ClearScreen (Area :CARDINAL);
VAR
  ignore :TextCoords;
BEGIN
  CursorOff();

  CASE Area OF
    | _GCLEARSCREEN : mySolidRect (0, 0, Width-1, Depth-1, BackColor, TRUE);
    | _GWINDOW      : mySolidRect ( (c1W-1)*fontXd, (r1W-1)*fontYd,
                                   (c2W)*fontXd, (r2W)*fontYd + fontDescend,
                                   BackColor, TRUE );
  ELSE;
  END;

  ignore := SetTextPosition (1, 1);
  DisplayCurs (curStatus);
END ClearScreen;



PROCEDURE Plot (x, y :LONGCARD; Color :LONGCARD);
BEGIN
  CursorOff();
  WITH GObj DO
    pointX := x;
    pointY := y;
    pColor := Color;
    obj    := GComm.plot;
  END;
  drawTwice();
  DisplayCurs (curStatus);
END Plot;


PROCEDURE Point ( x, y :LONGCARD ) :LONGCARD;
BEGIN
  RETURN 0;
END Point;


PROCEDURE Line ( x1, y1, x2, y2 :LONGCARD; color: LONGCARD);
BEGIN
  CursorOff();
  WITH GObj DO
    lineX1 := x1;
    lineY1 := y1;
    lineX2 := x2;
    lineY2 := y2;
    lineColor   := color;
    obj    := GComm.line;
  END;
  drawTwice();
  DisplayCurs (curStatus);
END Line;


PROCEDURE HLine(x, y, x2 :LONGCARD; FillColor: LONGCARD);
BEGIN
  Line (x, y, x2, y, FillColor);
END HLine;


PROCEDURE Rectangle (x0, y0, x1, y1 :LONGCARD; color :LONGCARD; fill :BOOLEAN);
BEGIN
  CursorOff();
  myRect ( x0, y0, x1, y1, color, fill );
  DisplayCurs (curStatus);
END Rectangle;


<* PUSH *>
<* COVERFLOW - *>

PROCEDURE Ellipse ( x0, y0, a, b, c :LONGCARD; fill :BOOLEAN  );
BEGIN
  CursorOff();

  IF ( a = 0 ) OR ( b = 0 ) THEN
    WITH GObj DO
      lineX1:=x0-a;
      lineY1:=y0-b;
      lineX2:=x0+a;
      lineY2:=y0+b;
      lineColor   := c;
      obj    := GComm.line;
    END;
  ELSE
    WITH GObj DO
      eX1 := x0-a;
      eY1 := y0-b;
      eX2 := x0+a;
      eY2 := y0+b;
      eColor := c;
      eFill  := fill;
      obj    := GComm.ellip;
    END;
  END;
  drawTwice();
  DisplayCurs (curStatus);
END Ellipse;

<* POP *>


PROCEDURE Disc (x0, y0, r, c: LONGCARD);
BEGIN
  Ellipse ( x0, y0, r, r, c, TRUE );
END Disc;


PROCEDURE Circle (x0, y0, r, c :LONGCARD);
BEGIN
  Ellipse ( x0, y0, r, r, c, FALSE );
END Circle;



PROCEDURE Polygon (n :LONGCARD; px-, py- :ARRAY OF LONGCARD; FillColor :LONGCARD; Fill :BOOLEAN);
VAR
  i :LONGCARD;
BEGIN
  IF (n-1 > HIGH(px)) OR (n-1 > HIGH(py)) OR (n > GComm._mxPolyNodes) THEN RETURN; END;

  CursorOff();
  WITH GObj DO
    polyFill   := Fill;
    polyColor  := FillColor;
    polyN      := n;
    FOR i:=0 TO n-1 DO
      polyNodes[i].x := px[i];
      polyNodes[i].y := py[i];
    END;
    obj        := GComm.poly;
  END;
  drawTwice();

  DisplayCurs (curStatus);
END Polygon;


PROCEDURE Cube (top :BOOLEAN; x1, y1, x2, y2, depth :LONGCARD; Color :LONGCARD; Fill:BOOLEAN);
VAR
  px, py  :ARRAY [0..3] OF LONGCARD;
  height  :LONGCARD;
BEGIN
   height := y2-y1;
   px[0]  := x2;
   py[0]  := y2;
   px[1]  := x2 + depth;
   py[1]  := y2 - (depth DIV 2);
   px[2]  :=px[1];
   py[2]  :=py[1]-height;
   px[3]  :=px[0];
   py[3]  :=py[0]-height;
   Polygon (4, px, py, Color, Fill);
   IF top
    THEN px[0] := x1;
         py[0] := y1;
         px[1] := x1 + depth;
         py[1]:=y1 - (depth DIV 2);
         DEC( px[2] );
         DEC( px[3] );
         Polygon(4, px, py, Color, Fill );
   END;
   Rectangle (x1, y1, x2, y2, Color, Fill);
END Cube;


PROCEDURE FloodFill ( x, y :LONGCARD; color, boundary :LONGCARD );
BEGIN
  CursorOff();
  WITH GObj DO
    seedpX     := x;
    seedpY     := y;
    floodColor := color;
    boundColor := boundary;
    obj        := GComm.flood;
  END;
  drawTwice();

  DisplayCurs (curStatus);
END FloodFill;


PROCEDURE Arc (x0, y0, a, b, x3, y3, x4, y4: CARDINAL; Color: CARDINAL);
BEGIN
  CursorOff();
  WITH GObj DO
    WITH arcR DO
      left   := x0 - a;
      top    := y0 - b;
      right  := x0 + a;
      bottom := y0 + b;
    END;

    arcXStart := x3;
    arcYStart := y3;
    arcXEnd   := x4;
    arcYEnd   := y4;

    arcColor      := Color;
    obj           := GComm.arc;
  END;
  drawTwice();

  DisplayCurs (curStatus);
END Arc;


PROCEDURE Pie (x0, y0, a, b, x3, y3, x4, y4 :CARDINAL; Colr :CARDINAL; Fill :BOOLEAN);
BEGIN
  CursorOff();
  WITH GObj DO
    WITH pieR DO
      left   := x0 - a;
      top    := y0 - b;
      right  := x0 + a;
      bottom := y0 + b;
    END;

    pieXStart := x3;
    pieYStart := y3;
    pieXEnd   := x4;
    pieYEnd   := y4;

    pieColor      := Colr;
    pieFill       := Fill;
    obj           := GComm.pie;
  END;
  drawTwice();

  DisplayCurs (curStatus);
END Pie;


PROCEDURE Arc_a ( x0, y0, x1, y1 :LONGCARD; startAngle, sweepAngle :LONGREAL; Color :LONGCARD );
VAR
  radSA, radEA     :LONGREAL;
  arcCntX, arcCntY :INTEGER;
BEGIN
  CursorOff();
  WITH GObj DO
    WITH arcR DO
      left   := myMIN (x0, x1);
      bottom := myMAX (y0, y1);
      right  := myMAX (x0, x1);
      top    := myMIN (y0, y1);

      arcCntX := ( right - left ) DIV 2;
      arcCntY := ( bottom - top ) DIV 2;
    END;


    radSA := startAngle*LM.pi/180.0;
    radEA := (startAngle+sweepAngle)*LM.pi/180.0;

    arcXStart := VAL (INTEGER, 1000.0 * LM.cos (radSA) ) + arcCntX;
    arcYStart := VAL (INTEGER, -1000.0 * LM.sin (radSA) ) + arcCntY;
    arcXEnd   := VAL (INTEGER, 1000.0 * LM.cos (radEA) ) + arcCntX;
    arcYEnd   := VAL (INTEGER, -1000.0 * LM.sin (radEA) ) + arcCntY;

    arcColor      := Color;
    obj           := GComm.arc;
  END;
  drawTwice();

  DisplayCurs (curStatus);
END Arc_a;

PROCEDURE Pie_a ( x0, y0, x1, y1 :LONGCARD; startAngle, sweepAngle :LONGREAL;
                color :LONGCARD; fill :BOOLEAN );
VAR
  radSA, radEA     :LONGREAL;
  arcCntX, arcCntY :INTEGER;
BEGIN
  CursorOff();
  WITH GObj DO
    WITH pieR DO
      left   := myMIN (x0, x1);
      bottom := myMAX (y0, y1);
      right  := myMAX (x0, x1);
      top    := myMIN (y0, y1);

      arcCntX := (right+left) DIV 2;
      arcCntY := (bottom+top) DIV 2;
    END;

    radSA := startAngle*LM.pi/180.0;
    radEA := (startAngle+sweepAngle)*LM.pi/180.0;

    pieXStart := VAL (INTEGER, 1000.0 * LM.cos (radSA) ) + arcCntX;
    pieYStart := VAL (INTEGER, -1000.0 * LM.sin (radSA) ) + arcCntY;
    pieXEnd   := VAL (INTEGER, 1000.0 * LM.cos (radEA) ) + arcCntX;
    pieYEnd   := VAL (INTEGER, -1000.0 * LM.sin (radEA) ) + arcCntY;

    pieColor      := color;
    pieFill       := fill;
    obj           := GComm.pie;
  END;
  drawTwice();

  DisplayCurs (curStatus);
END Pie_a;


(*/////////////////////////// Bitmap operations //////////////////////////////*)


PROCEDURE GetImage  (x1, y1, x2, y2 :LONGCARD; VAR handle :HBITMAP);
BEGIN
  Adjust ( x1, 0, Width-1 );
  Adjust ( x2, 0, Width-1 );
  Adjust ( y1, 0, Depth-1 );
  Adjust ( y2, 0, Depth-1 );

  WITH GObj DO
    WITH imgArea DO
      left   := myMIN (x1,x2);
      top    := myMIN (y1,y2);
      right  := myMAX (x1,x2);
      bottom := myMAX (y1,y2);
    END;
    obj    := GComm.getimg;
  END;
  drawOnce();

  handle := HBITMAP(GObj.rimgHandle);
END GetImage;


PROCEDURE PutImage  (x, y :LONGCARD; hbm :HBITMAP; action :LONGCARD);
TYPE
  actArrayT = ARRAY [0.._GXOR] OF CARDINAL;
CONST
  actArray = actArrayT{W.SRCPAINT, W.SRCAND, W.SRCERASE, W.SRCCOPY, W.SRCINVERT};
BEGIN
  CursorOff();
  WITH GObj DO
    imgLCorn.cx := x;
    imgLCorn.cy := y;
    imgHandle   := W.HBITMAP(hbm);
    imgMode     := actArray[action];
    obj         := GComm.putimg;
  END;
  drawTwice();
  DisplayCurs (curStatus);
END PutImage;


PROCEDURE DelImage ( hbm :HBITMAP );
BEGIN
  WITH GObj DO
    delImgHandle := W.HBITMAP(hbm);
    obj          := GComm.delimg;
  END;
  drawOnce();
END DelImage;

PROCEDURE ImageSize (x1, y1, x2, y2: CARDINAL) :CARDINAL;
BEGIN
  RETURN 0;
END ImageSize;


(*///////////////////////////// Palette operations ////////////////////////////*)


PROCEDURE RemapAllPalette ( colArray- :ARRAY OF LONGCARD ) :LONGCARD;
VAR
  n :CARDINAL;
BEGIN
  n := HIGH(colArray)+1;
  IF ( n > NumPColor ) THEN RETURN MAX(LONGCARD) END;
  n := myMIN ( GComm._mxNColor, n );

  WITH GObj DO
    SYSTEM.MOVE ( ADR (colArray), ADR (curPalette), n*SIZE (LONGCARD) );
    numc2map := n;
    obj := GComm.mapAllcol;
  END;
  drawOnce();
  RETURN 0;
END RemapAllPalette;



PROCEDURE InitStdPalette;
TYPE
  colors = ARRAY [0..15] OF LONGCARD;
CONST
  ColArr = colors {_BLACK,    _BLUE,         _GREEN,       _CYAN,
                   _RED,      _MAGENTA,      _BROWN,       _WHITE,
                   _GRAY,     _LIGHTBLUE,    _LIGHTGREEN,  _LIGHTCYAN,
                   _LIGHTRED, _LIGHTMAGENTA, _LIGHTYELLOW, _BRIGHTWHITE};
VAR
  rc :CARDINAL;
BEGIN
  rc := RemapAllPalette ( ColArr );
END InitStdPalette;


PROCEDURE RemapPalette (palItem :CARDINAL; color :LONGCARD) :LONGCARD;
VAR
  r :LONGINT;
BEGIN
  IF (palItem > NumPColor-1)
    THEN RETURN MAX(LONGCARD);
  END;

  WITH GObj DO
    r := curPalette[palItem];
    curPalette[palItem] := color;

    obj := GComm.mapAllcol;
    drawOnce();
  END;

  RETURN  VAL(LONGCARD, r);
END RemapPalette;


(*///////////////////////////////// text procedures ////////////////////////////////*)


PROCEDURE GetTextColor() :LONGCARD; BEGIN RETURN txtColor; END GetTextColor;

PROCEDURE SetTextColor (Color :LONGCARD) :LONGCARD;
VAR
  c :LONGCARD;
BEGIN
  c        := txtColor;
  txtColor := Color;
  RETURN c;
END SetTextColor;


PROCEDURE GetTextPosition() :TextCoords; BEGIN RETURN curPos; END GetTextPosition;

PROCEDURE GetTextColumn() :LONGCARD; BEGIN RETURN curPos.col; END GetTextColumn;
PROCEDURE GetTextRow()    :LONGCARD; BEGIN RETURN curPos.row; END GetTextRow;


PROCEDURE SetTextPosition (row, col :LONGCARD) :TextCoords;
VAR
  oldPos :TextCoords;
BEGIN
  Adjust ( col, c1W, c2W);
  Adjust ( row, r1W, r2W);

  oldPos := curPos;
  CursorOff();
  curPos.col := col;
  curPos.row := row;
  DisplayCurs (curStatus);
  RETURN oldPos;
END SetTextPosition;

PROCEDURE SetTextWindow ( r1, c1, r2, c2  :LONGCARD );
VAR
  ignore :TextCoords;
BEGIN
  Adjust ( c1, 1, Columns);
  Adjust ( c2, 1, Columns);
  Adjust ( r1, 1, Rows );
  Adjust ( r2, 1, Rows );

  c1W := myMIN (c1, c2);
  r1W := myMIN (r1, r2);
  c2W := myMAX (c1, c2);
  r2W := myMAX (r1, r2);

  ignore := SetTextPosition ( r1W, c1W );
END SetTextWindow;



PROCEDURE Wrapon ( Opt :BOOLEAN) :BOOLEAN;
VAR
  b :BOOLEAN;
BEGIN
  b := wrapMode;
  wrapMode := Opt;
  RETURN b;
END Wrapon;




PROCEDURE RawOutText (x1, y1, color :LONGCARD; Text- :ARRAY OF CHAR );
BEGIN
  CursorOff();
  WITH GObj DO
    rtxPos.cx  := x1;
    rtxPos.cy  := y1;
    rtxColor   := color;
    Strings.Assign ( Text, rtxText );
    obj        := GComm.rtext;  -- go, go...
  END;
  drawTwice();

  DisplayCurs (curStatus);
END RawOutText;




PROCEDURE OutText ( Text- :ARRAY OF CHAR);

PROCEDURE outTextPiece ( Text- :ARRAY OF CHAR);
BEGIN
  WITH GObj DO
    txPos.cx := (curPos.col-1) * fontXd;
    txPos.cy := (curPos.row-1) * fontYd;
    txFontXd := fontXd;
    txFontYd := fontYd;
    txfontDescend := fontDescend;
    txColor  := txtColor;
    txBgr    := BackColor;
    Strings.Assign ( Text, txText );
    obj        := GComm.text;
  END;
  drawTwice();
END outTextPiece;

PROCEDURE ["C"] advCur ( n :LONGCARD ) :BOOLEAN;
VAR
  xs, ys :LONGCARD;
BEGIN
  xs := ( curPos.col-1 + n ) MOD (c2W - c1W +1);
  ys := ( curPos.col-1 + n ) DIV (c2W - c1W +1);

  curPos.col := c1W + xs;
  IF  ( INT(curPos.row) + INT(ys) > INT(Rows) )
    THEN curPos.row := Rows;
         RETURN FALSE
    ELSE INC (curPos.row, ys );
         RETURN TRUE;
  END;
END advCur;

VAR
  i, lRmStr,
  lRmLine, lt :CARDINAL;
  dText       :ARRAY [0..GComm._mxTextChars] OF CHAR;
BEGIN
  CursorOff();
  lt := LENGTH (Text);

  IF  NOT wrapMode
    THEN lRmLine := (c2W - curPos.col) + 1;
         IF (lt > lRmLine)
           THEN lRmStr := lRmLine;
           ELSE lRmStr := lt;
         END;
         Strings.Extract ( Text,  0, lRmStr, dText );
         outTextPiece ( dText );
         advCur ( lRmStr );
    ELSE
     i := 0;
     LOOP
       IF ( lt <= 0 ) THEN EXIT; END;
       lRmLine := (c2W - curPos.col) + 1;
       IF (lt > lRmLine)
         THEN lRmStr := lRmLine;
         ELSE lRmStr := lt;
       END;
       Strings.Extract ( Text,  i, lRmStr, dText );
       outTextPiece ( dText );
       IF NOT advCur ( lRmStr ) THEN EXIT; END;
       DEC ( lt, lRmStr );
       INC ( i,  lRmStr );
    END;
  END;
  DisplayCurs (curStatus);
END OutText;


PROCEDURE GraphMode();
BEGIN
  xtsIGraph.fGrMode := TRUE;                --*FSA
  W.SetForegroundWindow(mainWndHandle);     --
END GraphMode;

PROCEDURE TextMode();
BEGIN
  xtsIGraph.fGrMode := FALSE;               --*FSA
  W.SetForegroundWindow(consoleHandle);     --
END TextMode;

PROCEDURE SetActivePage (Page :CARDINAL) :CARDINAL;
BEGIN
  RETURN 0;
END SetActivePage;

PROCEDURE SetVisualPage (Page :CARDINAL) :CARDINAL;
BEGIN
  RETURN 0;
END SetVisualPage;

PROCEDURE SetGraphWindowTitle (text- :ARRAY OF CHAR);
BEGIN
  W.SetWindowText (mainWndHandle, text);
END SetGraphWindowTitle;


PROCEDURE SetVideoMode (Mode :CARDINAL): BOOLEAN;
TYPE
  szArrayT = ARRAY [0.._MRES256COLOR-_MRES4COLOR] OF CARDINAL;
CONST
  cxArray = szArrayT{320,320,640,13,0,0,0,0,720,320,640,640,640,640,640,320};
  cyArray = szArrayT{200,200,200,13,0,0,0,0,348,200,200,350,350,480,480,200};
VAR
  cx, cy :CARDINAL;
BEGIN
  IF (Mode <= _TEXTC80) OR (Mode = _TEXTMONO) THEN RETURN TRUE END; -- text mode

  IF (Mode = _DEFAULTMODE) THEN
    TextMode();                       --*FSA
    RETURN TRUE;
  ELSE
    DEC (Mode, _MRES4COLOR);
    cx := cxArray[Mode];
    cy := cyArray[Mode];
  END;
  RETURN Init (50,50, cx, cy);
END SetVideoMode;


PROCEDURE GetVideoConfig (VAR V :VideoConfig);
BEGIN
  FILL (ADR(V), 0, SIZE(VideoConfig));
  IF ~hasInitialized THEN RETURN END;

  WITH V DO
    numxpixels    := Width;
    numypixels    := Depth;
    numtextcols   := Columns;
    numtextrows   := Rows;
    numcolors     := NumPColor;
    bitsperpixel  := 0;         -- normally, no need in the value
    numvideopages := 1;
    mode          := _VGA;
    adapter       := _COLOR;
    --monitor
    --memory
  END;
END GetVideoConfig;




PROCEDURE SetUpdateMode (auto :BOOLEAN);
BEGIN
  autoUpdateMode := auto;
END SetUpdateMode;

------------------------------------------------------------

(* defining window handle of console *)

CONST
  CONSOLE_TITLE = "__XDS console@@@###___joppa";

<* +VOLATILE *>
VAR
  __consoleHandle   :W.HWND;
<* -VOLATILE *>

PROCEDURE ["StdCall"] EnumTProc (hwnd :W.HWND; mypid :W.LPARAM) :SYSTEM.BOOL32;
VAR
  s        :ARRAY[0..255] OF CHAR;
  pid, tid :CARDINAL;
BEGIN
   tid := W.GetWindowThreadProcessId (hwnd, pid);
   W.GetWindowText (hwnd, s, 256);

   --IF (pid = CARDINAL(mypid)) THEN -- it's a subject to research !!!!!

     IF (s = CONSOLE_TITLE) THEN
       __consoleHandle   := hwnd;
       RETURN FALSE;
     END;
   --END;
   RETURN TRUE;
END EnumTProc;


PROCEDURE GetMyConsoleHandle() :W.HWND;
VAR
  ch    :W.HWND;
  s     :ARRAY [0..255] OF CHAR;
  mypid :CARDINAL;
BEGIN
  W.GetConsoleTitle (s, 256);

  IF NOT W.SetConsoleTitle (CONSOLE_TITLE) THEN RETURN NIL END;

  __consoleHandle := NIL;

  mypid := W.GetCurrentProcessId();
  WHILE (__consoleHandle = NIL) DO
    W.EnumWindows (EnumTProc, mypid);
  END;

  W.SetConsoleTitle (s); -- restore a previous title

  RETURN __consoleHandle;
END GetMyConsoleHandle;


VAR
  gsTID :CARDINAL;
  sBufL :CARDINAL;

PROCEDURE Init (xlc, ytc, xd, yd :LONGCARD ) :BOOLEAN;
VAR
  gsTH         :W.HANDLE;
  SearchResult :ARRAY [0..255] OF CHAR;
  si           :W.STARTUPINFO;
  pi           :W.PROCESS_INFORMATION;  -- process ID saved to here (pi.dwProcessId)
  c            :CARDINAL;

  ignore       :CARDINAL;

  tMetrics     :W.TEXTMETRIC;
  hf           :W.HFONT;

BEGIN
  Width  := xd;
  Depth  := yd;
  xLeftC := xlc;
  yTopC  := ytc;

  (* if the module was alreary initialized, bring up the message box *)
  IF ~hasInitialized THEN
    hasInitialized := TRUE;

    consoleHandle := GetMyConsoleHandle();

    RegisterWindowClass (W.GetModuleHandle(NIL));

    (* create blocked event semaphore "client Ready" ( drawing thread hangs on it) *)
    hclReadySem := W.CreateEvent (NIL, FALSE, FALSE, NIL);
    IF ( hclReadySem = NIL )
      THEN W.MessageBox ( NIL, "Event not created", "Graph",
                          W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND );
           RETURN FALSE;
    END;

    (* create blocked event semaphore "server Ready" ( drawing thread hangs on it) *)
    hservReadySem := W.CreateEvent ( NIL, TRUE, FALSE, NIL );
    IF ( hservReadySem = NIL )
     THEN W.MessageBox ( NIL, "Event not created", "Graph",
                         W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND );
          RETURN FALSE;
     END;

    restartSem := W.CreateEvent ( NIL, FALSE, FALSE, NIL );  -- autoevent
    IF (restartSem = NIL)
     THEN W.MessageBox ( NIL, "Event not created", "Graph",
                         W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND );
          RETURN FALSE;
     END;

    (* create unowned mutex semaphore "screen resource" *)
    hScrMutex := W.CreateMutex ( NIL, FALSE, NIL );
    IF (hScrMutex = NIL) THEN
      W.MessageBox (NIL, "Mutex not created", "Graph",
                    W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND );
      RETURN FALSE;
    END;


    gsTH := W.CreateThread ( NIL,                -- security attrs
                             8192,               -- initial thread stack size, in bytes
                             MsgLoopThreadProc,  -- pointer to thread function
                             NIL,                -- argument for new thread
                             W.CREATE_SET {},    -- creation flags (start immediately)
                             gsTID               -- pointer to returned thread identifier
                           );

    IF (gsTH = NIL) THEN
      W.MessageBox ( NIL,
      "Graph server not run",
      "Graph",
      W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND);
      RETURN FALSE;
    END;

    (* wait until window not created *)
    W.WaitForSingleObject (restartSem, W.INFINITE);

  ELSE
    IF ~hasExited THEN Exit END;
    W.SetEvent (restartSem);

    (* wait until window not created *)
    W.WaitForSingleObject (restartSem, W.INFINITE);
  END;


  xtsIGraph.fGrMode := TRUE; --*FSA

  NumPColor := RGB.numColors;

  InitStdPalette;

  SetLinestyle    (LNSTYLE_SOLID);
  SetBkMix        (_OPAQUE);
  c := SetBkColor (_clrBLACK);
  ClearScreen     (_GCLEARSCREEN);

  W.GetTextMetrics (persistentHDC, tMetrics);
  hf := W.CreateFont(tMetrics.tmHeight, 0, 0, 0, W.FW_DONTCARE, FALSE, FALSE, FALSE,
                     W.ANSI_CHARSET, W.OUT_DEFAULT_PRECIS, W.CLIP_DEFAULT_PRECIS,
                     W.DEFAULT_QUALITY, W.FIXED_PITCH+W.FF_DONTCARE, "Fixedsys" );
  IF (hf # NIL) THEN
    W.SelectObject(BitMap.hdcM,   hf);
    W.SelectObject(persistentHDC, hf);
    W.GetTextMetrics (persistentHDC, tMetrics);
  END;

  WITH tMetrics DO
    fontYd      := tmHeight;
    fontDescend := tmDescent;
    fontXd      := tmMaxCharWidth;
  END;

  Rows    := (Depth - fontDescend) DIV fontYd;
  Columns := Width DIV fontXd;

  SetTextWindow (1, 1, Rows, Columns);

  hasExited := FALSE;

  RETURN TRUE;
END Init;


PROCEDURE Exit;
BEGIN
  IF hasInitialized THEN
    W.PostThreadMessage (gsTID, W.WM_QUIT, 0, 0);
    W.WaitForSingleObject (restartSem, W.INFINITE); -- wait until destroyed
    hasExited := TRUE;
  END;
END Exit;


BEGIN --------------------------------------------------------------------------
  hasInitialized := FALSE;
  hasExited      := FALSE;
  autoUpdateMode := TRUE;

  PATSYM_DENSE1         := ORD(W.HS_DIAGCROSS);
  PATSYM_DENSE2         := ORD(W.HS_CROSS);
  PATSYM_VERT           := ORD(W.HS_VERTICAL);
  PATSYM_HORIZ          := ORD(W.HS_HORIZONTAL);
  PATSYM_DIAG1          := ORD(W.HS_BDIAGONAL);
  PATSYM_DIAG2          := ORD(W.HS_FDIAGONAL);
  PATSYM_SOLID          := ORD(W.HS_SOLID);

  wrapMode     := _GWRAPOFF;
  txtColor     := _clrBRIGHTWHITE;
  curStatus    := FALSE;
  curDisplayed := FALSE;
  currentHRGN  := NIL;

  LineStyle    := LNSTYLE_SOLID;
  SFillMask    := PATSYM_SOLID;
  FILL ( ADR(FillMask), 0, SIZE(FillMask) );
  isStdMask    := TRUE;
FINALLY
  W.CloseHandle (hservReadySem);
  W.CloseHandle (hclReadySem);
  W.CloseHandle (hScrMutex);
  W.CloseHandle (restartSem);

  --IF hasInitialized THEN Exit() END;
END Graph.
