(* Copyright (C) 1998 xTech Ltd *)

<* +M2ADDTYPES   *>
<* +M2EXTENSIONS *>

<* -IOVERFLOW    *>
<* -COVERFLOW    *>
<* -CHECKINDEX   *>
<* -CHECKRANGE   *>
<* -CHECKNIL     *>

IMPLEMENTATION MODULE VTerm (* based on Graph *);

IMPORT SYSTEM,
       Strings,
       W     := Windows,
       LM    := LongMath,
       GComm := vterm_c,
       TW    := vterm_t,
       Processes,
       SeqFile,
       FileSys,
       StdChans,
       ProgEnv,
       IOResult,
       IOChan,
       IOConsts,
       TextIO,
       ChanConsts;

FROM SYSTEM IMPORT ADR, ADDRESS, FILL, CARD8;

FROM Windows IMPORT LOWORD, HIWORD;

CONST
  WM_SERVERSTART = W.WM_USER +1000;
  finished = " - Finished";

CONST
  MAINCLASSNAME  = "GraphWnd";
  FRAMECLASSNAME = "FrameWnd"; -- Frame window
  CM_FIRSTUSER   = 24346;
  CM_EXIT        = CM_FIRSTUSER;
  CM_LOADINPUT   = CM_FIRSTUSER + 1;
  CM_SAVEINPUT   = CM_FIRSTUSER + 2;

VAR
  GObj     :GComm.tGObj;  -- "shared memory"

  perstHDC :W.HDC;
  FrameWnd :W.HWND;
  GraphWnd :W.HWND;
  TWWnd    :W.HWND;

  dtID     :CARDINAL;
  isInit   :BOOLEAN;
  hmenu,hmenuFile : W.HMENU;
  Exiting : BOOLEAN;
  exename : ARRAY [1..100] OF CHAR;

------------------------------------ Semaphores

TYPE
     PCHAR = POINTER TO CHAR;
     AOC = ARRAY OF CHAR;
     PAOC= POINTER TO AOC;


VAR
  hservReadySem,
  hclReadySem,
  hScrMutex    :W.HANDLE;


---- client interface

PROCEDURE CopyFile ( a,b: ARRAY OF CHAR );
VAR
  acid,bcid:  IOChan.ChanId;
  res:        SeqFile.OpenResults;
  locsRead:   CARDINAL;
  ch:         CHAR;
BEGIN
  SeqFile.OpenRead ( bcid, b, ChanConsts.read, res);
  SeqFile.OpenWrite( acid, a, ChanConsts.write+ChanConsts.old, res );
  WHILE IOResult.ReadResult(bcid) # IOConsts.endOfInput DO
    TextIO.ReadChar ( bcid, ch );
    IF IOResult.ReadResult(bcid) = IOConsts.endOfLine THEN
      TextIO.SkipLine(bcid);
      TextIO.ReadChar ( bcid, ch );
    END;
    TextIO.WriteChar( acid, ch );
--      IOChan.TextRead (bcid, SYSTEM.ADR(ch), 1, locsRead );
--      IOChan.TextWrite(acid, SYSTEM.ADR(ch), locsRead);

  END;
  SeqFile.Close( acid );
  SeqFile.Close( bcid );

END CopyFile;

PROCEDURE waitServerReady();
BEGIN
  W.WaitForSingleObject ( hservReadySem, W.INFINITE );
END waitServerReady;


PROCEDURE waitServerReadyB();
BEGIN
  W.WaitForSingleObject ( hservReadySem, W.INFINITE );
  W.ResetEvent ( hservReadySem );
END waitServerReadyB;


PROCEDURE releaseServerReady();
BEGIN
  W.SetEvent ( hservReadySem );
END releaseServerReady;


-- server interface

PROCEDURE waitClientReadyB();
BEGIN
  W.WaitForSingleObject ( hclReadySem, W.INFINITE );
  W.ResetEvent ( hclReadySem );
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
  LineStyle    :LONGINT;
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
                 Create, Draw,
                 GainAccess, Relinquish;

VAR
  hdcM      :W.HDC;
  hbm       :W.HBITMAP;
  hbmpMutex :W.HANDLE;


(*
   creates of the screen bitmap and its DC
    WARNING : Create can be issued only after initialization the Depth/Width vars
*)
PROCEDURE Create ( hdc :W.HDC );
BEGIN
  hdcM := W.CreateCompatibleDC ( hdc );

  hbm := W.CreateCompatibleBitmap ( hdc, Width, Depth );

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

BEGIN
  hbmpMutex := W.CreateMutex ( NIL, FALSE, NIL );
FINALLY
  W.DeleteObject ( hbm );
  W.DeleteDC ( hdcM );
  W.CloseHandle ( hbmpMutex );
END BitMap;



MODULE copyBmp;

IMPORT W;

EXPORT QUALIFIED Init, Paste, Copy,
                 hdcMem;


VAR
  hdcMem :W.HDC;


PROCEDURE Copy ( hdc :W.HDC; area :W.RECT ) :W.HBITMAP;
(* ASSERT : area is ordered *)
VAR
  hb      :W.HBITMAP;
  width,
  depth :CARDINAL;
BEGIN
  WITH area DO
    width := right-left+1;
    depth := bottom-top+1; 
  END;  

  hb := W.CreateCompatibleBitmap ( hdc, width, depth );
  W.SetBitmapDimensionEx ( hb, width, depth, NIL );

  W.SelectObject ( hdcMem, hb );

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


PROCEDURE Paste ( hdc :W.HDC; corn :W.SIZEL; hb :W.HBITMAP; mode :LONGCARD );
VAR
  sz :W.SIZEL;
BEGIN
  W.GetBitmapDimensionEx ( hb, sz );

  W.SelectObject ( hdcMem, hb );

  W.BitBlt ( hdc,          -- handle to destination device context
             corn.cx,
             corn.cy,      -- x/y coordinate of destination rectangle's upper-left corner
             sz.cx, sz.cy,
             hdcMem,       -- handle to source device context
             0, 0,         -- x/y coordinate of source rectangle's upper-left corner
             mode          -- raster operation code
           );
END Paste;


PROCEDURE Init ( hdc :W.HDC );
BEGIN
  hdcMem := W.CreateCompatibleDC ( hdc );
END Init;

BEGIN
FINALLY
  W.DeleteDC ( hdcMem );
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
  IF ( bs = PATSYM_SOLID )
    THEN brushStyle := W.BS_SOLID;
    ELSE brushStyle := W.BS_HATCHED;
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
  IF (fillMode)
    THEN curPen.lopnStyle := W.PS_NULL;
         IF (isStdBS)
           THEN curBrush.lbStyle := brushStyle;
           ELSE W.SetTextColor ( hdc, c);
                curBrush.lbStyle := W.BS_PATTERN;
         END;       
    ELSE curPen.lopnStyle := penStyle;
         curBrush.lbStyle := W.BS_HOLLOW;
  END;
               
  curPen.lopnColor := c;
  penCreated := W.CreatePenIndirect ( curPen );
  dflPen := W.SelectObject ( hdc, penCreated );
  
  curBrush.lbColor := c;
  brushCreated     := W.CreateBrushIndirect ( curBrush );
  dflBrush := W.SelectObject ( hdc, brushCreated );
  
END SelectColor;


PROCEDURE ReplaceDefault ( hdc :W.HDC );
BEGIN
  --W.DeleteObject ( W.SelectObject ( hdc, dflBrush ) );
  --W.DeleteObject ( W.SelectObject ( hdc, dflPen ) );
  
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


(*
    The ServerDraw procedure actually works in the second thread when a job is,
    othewise it hangs on the "client ready" semaphore
*)

(*   Server interaction scheme :
 *   -------------------------
 *
 *    0. wait for client readiness ( until hclReadySem semaphore is became posted )
      1. block client readiness
 *    2. capture the shared memory (it is may be expected by client process to wait the mutex after )
 *    3. use the memory to draw (possibly write a output result in it)
 *    4. release the shared memory
 *)

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
  
    |GComm.init:        -- create screen bitmap --
                      perstHDC := W.GetDC (GraphWnd);

                      BitMap.Create ( perstHDC );
                      copyBmp.Init  ( perstHDC );

  
    |GComm.setHatch:    Tools.SetStdBrushStyle ( htStyle );

    |GComm.setLnStyle:  Tools.SetLineStyle ( lnStyle );



    |GComm.setBkC:    W.SetBkColor ( hdc, W.PALETTEINDEX (bkColor) );

    |GComm.setBkMix:  W.SetBkMode ( hdc, bkMix );
  
    |GComm.mapAllcol: RGB.MapAllCol ( perstHDC, curPalette, numc2map );
                      RGB.MapAllCol ( BitMap.hdcM, curPalette, numc2map );
                      RGB.MapAllCol ( copyBmp.hdcMem, curPalette, numc2map );

    |GComm.plot:      W.SetPixel ( hdc, pointX, pointY, W.PALETTEINDEX (pColor) );
   
    |GComm.point:     retColor := W.GetPixel ( hdc, ppointX, ppointY );

    |GComm.rect:
                     Tools.SelectColor ( hdc, W.PALETTEINDEX (rColor), rFill );
                     
                     WITH rRect DO           
                       W.Rectangle ( hdc, left, top, right, bottom )
                     END;
                     
                     Tools.ReplaceDefault ( hdc );



    | GComm.line:    Tools.SelectColor ( hdc, W.PALETTEINDEX (lineColor ), FALSE );
    
                     W.MoveToEx ( hdc, lineX1, lineY1, NIL );
                     W.LineTo   ( hdc, lineX2, lineY2 );
                     
                     Tools.ReplaceDefault ( hdc );

    | GComm.ellip:
                     Tools.SelectColor ( hdc,  W.PALETTEINDEX (eColor ), eFill );

                     IF (eFill)        
                       THEN W.Ellipse( hdc, eX1,eY1, eX2, eY2 );
                       ELSE W.Arc ( hdc, eX1,eY1, eX2, eY2,
                                    0, 0, 0, 0 );
                     END;

                     Tools.ReplaceDefault ( hdc );
                     
    | GComm.poly:    Tools.SelectColor ( hdc,  W.PALETTEINDEX ( polyColor ), polyFill );
    
                     IF polyFill
                       THEN W.Polygon ( hdc, polyNodes, polyN );
                       ELSE W.Polyline ( hdc, polyNodes, polyN );
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

    | GComm.delimg:  W.DeleteObject ( delImgHandle );


    |GComm.rtext: 
                    W.SetTextColor ( hdc, W.PALETTEINDEX ( rtxColor ) );
                    W.TextOut ( hdc, rtxPos.cx, rtxPos.cy, rtxText, LENGTH (rtxText) );

    |GComm.cursor:  W.InvertRect ( W.HDC(hdc), cursorArea); 
   
    | GComm.text   : cp  := txPos;
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



PROCEDURE ["StdCall"] ServerDraw ( p :W.PVOID ) :W.DWORD;
VAR
  dc :W.HDC;
BEGIN
(*
   this  releaseServerReady() tells Graph.Init that server is ready
*)
  releaseServerReady();
  LOOP
    waitClientReadyB();
    IF ( GComm.rmodeB <= GObj.obj ) AND (GObj.obj <= GComm.rmodeE)  --  read mode of interaction
      THEN  DrawObj ( BitMap.hdcM );
      ELSIF ( GComm.singleCallB <= GObj.obj ) AND (GObj.obj <= GComm.singleCallE)
           THEN DrawObj ( BitMap.hdcM );
         
           ELSE
  

                BitMap.GainAccess;
                DrawObj ( BitMap.hdcM );
                BitMap.Relinquish;

                waitScreen;
                --dc := W.GetDC (GraphWnd);
                DrawObj ( perstHDC );
                releaseScreen;
    END;

    releaseServerReady();
  END; (* LOOP *)
  RETURN 0;
END ServerDraw;



VAR
  isStartAct     :BOOLEAN;
  servThrHandle  :W.HANDLE;

PROCEDURE InitStdPalette; FORWARD;

(* Graph window procedure *)
PROCEDURE [W.CALLBACK] MainWinProc ( hWnd :W.HWND; msg :W.UINT;
                                     wParam :W.WPARAM; lParam :W.LPARAM) :W.LRESULT;
VAR
  dc   :W.HDC;
  ps   :W.PAINTSTRUCT;

BEGIN
    CASE msg OF
    | W.WM_CREATE:

        -- create & start drawing thread --
      servThrHandle := W.CreateThread ( NIL, 18192, ServerDraw, NIL,
                                        W.CREATE_SUSPENDED, dtID );

      W.SetThreadPriority ( servThrHandle, W.THREAD_PRIORITY_ABOVE_NORMAL );
      
      W.PostMessage ( hWnd, WM_SERVERSTART, 0, 0 );
      RETURN 0;
      
    | WM_SERVERSTART:
      W.ResumeThread ( servThrHandle );
      RETURN 0;

     
    | W.WM_PAINT:
        dc := W.BeginPaint ( hWnd, ps );
        waitScreen;
        BitMap.Draw ( perstHDC );
        releaseScreen;

        W.EndPaint (hWnd, ps);
        RETURN 0;
        
    |W.WM_QUERYNEWPALETTE:
        W.RealizePalette ( perstHDC );
        RETURN 0;
    | W.WM_CLOSE:
        W.PostQuitMessage (0);
    | W.WM_LBUTTONDOWN:
        RETURN 0;
    | W.WM_LBUTTONUP:
        RETURN 0;
    | W.WM_MOUSEMOVE:
        RETURN 0;
    | W.WM_RBUTTONDOWN:
        RETURN 0;

    ELSE ;
    END;
    RETURN W.DefWindowProc (hWnd, msg, wParam, lParam);
END MainWinProc;

(* Frame window procedure *)
PROCEDURE [W.CALLBACK] FrameWinProc ( hWnd   :W.HWND;  msg    :W.UINT;
                                     wParam :W.WPARAM; lParam :W.LPARAM) : W.LRESULT;
CONST
  FILENAMEBUFFERSIZE = 300;
  openDialogName = "LOAD Standard Input From File" ;
  saveDialogName = "SAVE Standard Input To File" ;
  defExt = ".sif";
  filter = ARRAY OF CHAR {"Standard Input (*.sif)",CHR(0),"*.sif",CHR(0)};
VAR
  dc   :W.HDC;
  ps   :W.PAINTSTRUCT;
  userfile: W.OPENFILENAME;
  res : SeqFile.OpenResults;
  resbool : BOOLEAN;
  fileNameBuffer : ARRAY [ 0..FILENAMEBUFFERSIZE ] OF CHAR;
BEGIN
  CASE msg OF
  | W.WM_PAINT:
      dc := W.BeginPaint ( hWnd, ps );
      W.EndPaint (hWnd, ps);
      RETURN 0;
  | W.WM_SETFOCUS:
      W.SetFocus(TWWnd);
      RETURN 0;
  | W.WM_DESTROY:
      Exiting:=TRUE;
      Exit;
      W.PostQuitMessage(0);
      RETURN 0;
  | W.WM_COMMAND:
        CASE wParam OF
        |  CM_EXIT:
             Exiting:=TRUE;
             Exit;
             W.PostQuitMessage(0);
             RETURN 0;
        |  CM_LOADINPUT:
           fileNameBuffer[0]:=CHR(0);
           userfile.lStructSize     := SIZE( W.OPENFILENAME );
       userfile.hwndOwner       := hWnd;
           userfile.hInstance       := NIL;
       userfile.lpstrFilter     := SYSTEM.ADR( filter );
       userfile.lpstrCustomFilter   := NIL;
       userfile.nMaxCustFilter  := 0;
       userfile.nFilterIndex    := 0;
           userfile.lpstrFile       := SYSTEM.ADR( fileNameBuffer );
           userfile.nMaxFile        := FILENAMEBUFFERSIZE;
           userfile.lpstrFileTitle  := NIL;
           userfile.nMaxFileTitle   := 0;
           userfile.lpstrInitialDir := NIL;
           userfile.lpstrTitle      := SYSTEM.ADR( openDialogName );
           userfile.Flags       := W.OFN_EXPLORER+W.OFN_FILEMUSTEXIST+
                       W.OFN_LONGNAMES+W.OFN_PATHMUSTEXIST+W.OFN_HIDEREADONLY;
       userfile.nFileOffset     := 0;
       userfile.nFileExtension  := 0;
       userfile.lpstrDefExt     := SYSTEM.ADR( defExt );
       userfile.lCustData       := 0;
       userfile.lpfnHook        := NIL;
       userfile.lpTemplateName  := NIL;

           IF W.GetOpenFileName( userfile ) THEN
             TW.readingFromFile:=TRUE;
               SeqFile.OpenRead ( TW.inputFile,
                    fileNameBuffer,
                    ChanConsts.read,res);
           W.SetEvent(TW.hsemCharReady);

           END;

        |  CM_SAVEINPUT:
           fileNameBuffer[0]:=CHR(0);
           userfile.lStructSize     := SIZE( W.OPENFILENAME );
       userfile.hwndOwner       := hWnd;
           userfile.hInstance       := NIL;
       userfile.lpstrFilter     := SYSTEM.ADR( filter );
       userfile.lpstrCustomFilter   := NIL;
       userfile.nMaxCustFilter  := 0;
       userfile.nFilterIndex    := 0;
           userfile.lpstrFile       := SYSTEM.ADR( fileNameBuffer );
           userfile.nMaxFile        := FILENAMEBUFFERSIZE;
           userfile.lpstrFileTitle  := NIL;
           userfile.nMaxFileTitle   := 0;
           userfile.lpstrInitialDir := NIL;
           userfile.lpstrTitle      := SYSTEM.ADR( saveDialogName );
           userfile.Flags       := W.OFN_EXPLORER+
                       W.OFN_LONGNAMES+W.OFN_PATHMUSTEXIST+W.OFN_HIDEREADONLY;
       userfile.nFileOffset     := 0;
       userfile.nFileExtension  := 0;
       userfile.lpstrDefExt     := SYSTEM.ADR( defExt );
       userfile.lCustData       := 0;
       userfile.lpfnHook        := NIL;
       userfile.lpTemplateName  := NIL;

           IF W.GetSaveFileName( userfile ) THEN
               SeqFile.Close ( TW.outputFile );
         CopyFile( fileNameBuffer, TW.TEMPSIFFILENAME );
--               FileSys.Remove( fileNameBuffer, resbool );
--               FileSys.Rename( TW.TEMPSIFFILENAME, fileNameBuffer, resbool );
           SeqFile.OpenWrite ( TW.outputFile, TW.TEMPSIFFILENAME,
             ChanConsts.write+ChanConsts.old, res );
           END;

        END;
  ELSE ;
  END;
  RETURN W.DefWindowProc (hWnd, msg, wParam, lParam);
END FrameWinProc;

------

VAR 
  xLeftC, yTopC :CARDINAL;

PROCEDURE [W.CALLBACK] WinMain ( hInst,
                                 hPrevInst :W.HINSTANCE;
                                 CmdLine   :W.PSTR;
                                 nShow     :INTEGER) :INTEGER;
VAR
  wc      :W.WNDCLASS;
  msg     :W.MSG;
  cxF,cyF :LONGCARD; -- farme window sizes
  pcommline : PCHAR;
  i: INTEGER;
TYPE
     AOC = ARRAY OF CHAR;
     PAOC= POINTER TO AOC;

BEGIN
   
  IF ( hPrevInst # NIL ) THEN RETURN 0 END;

 (* Register FRAMECLASSNAME class *)
  wc.style         := W.CS_BYTEALIGNCLIENT;
  wc.lpfnWndProc   := FrameWinProc;
  wc.cbClsExtra    := 0;
  wc.cbWndExtra    := 0;
  wc.hInstance     := hInst;
  wc.hIcon         := W.LoadIcon ( NIL, W.IDI_APPLICATION );
  wc.hCursor       := W.LoadCursor (NIL, W.RESOURCESTR(VAL (INTEGER, W.IDC_ARROW)) );
  wc.hbrBackground := W.CreateSolidBrush (W.RGB (0,0,0));

  wc.lpszMenuName  := NIL;
  wc.lpszClassName := ADR (FRAMECLASSNAME);
  wc.hbrBackground := W.CreateSolidBrush (W.RGB (80H,80H,80H));
  IF (W.RegisterClass (wc) = 0)
    THEN W.MessageBox (NIL, "Frame window class was not registered", NIL,
                       W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND);
         RETURN 0;
  END;

 (* Register MAINCLASSNAME class (graph. window) *)
  wc.style         := W.CS_BYTEALIGNWINDOW + W.CS_OWNDC;
  wc.lpfnWndProc   := MainWinProc;
  wc.lpszClassName := ADR (MAINCLASSNAME);
  IF (W.RegisterClass (wc) = 0)
    THEN W.MessageBox (NIL, "Graph window class was not registered", NIL,
                       W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND);
         RETURN 0;
  END;

 (* Register TW.TWCLASSNAME class *)
  IF (NOT TW.pre_init(hInst)) THEN RETURN 0; END;

  cxF := Width + LONGCARD(W.GetSystemMetrics(W.SM_CXBORDER))*2;
  cyF := Depth + TWDepth + LONGCARD(W.GetSystemMetrics(W.SM_CYBORDER))*2
                         + LONGCARD(W.GetSystemMetrics(W.SM_CYSIZE));

      hmenu := W.CreateMenu( );
--      W.SetMenu( hWnd, hmenu );
      hmenuFile := W.CreatePopupMenu( );
      W.AppendMenu( hmenuFile, W.MF_ENABLED+W.MF_STRING, CM_LOADINPUT, "&Load input" );
      W.AppendMenu( hmenuFile, W.MF_ENABLED+W.MF_STRING, CM_SAVEINPUT, "&Save input" );
      W.AppendMenu( hmenuFile, W.MF_SEPARATOR, 0 , NIL );
      W.AppendMenu( hmenuFile, W.MF_ENABLED+W.MF_STRING, CM_EXIT, "E&xit" );

      W.AppendMenu( hmenu, W.MF_ENABLED+W.MF_POPUP, VAL(INTEGER,hmenuFile), "&File" );
--      W.DrawMenuBar( hWnd );
  
--  pcommline:=W.GetCommandLine();

--  PCharToStringCopy( pcommline , exename );
  ProgEnv.ProgramName( exename );
  i:=0;
  REPEAT
    INC(i);
  UNTIL exename[i]=0C;
  REPEAT
    DEC(i);
  UNTIL (exename[i]="\") OR (i<0);
  Strings.Delete( exename, 0, i ); 
  FrameWnd := W.CreateWindowEx ( W.WS_EX_SET{},
                            FRAMECLASSNAME,
                            exename,
                            W.WS_OVERLAPPED + W.WS_BORDER +
                            W.WS_SYSMENU + W.WS_MINIMIZEBOX + W.WS_VISIBLE,
                            xLeftC, yTopC,
                            cxF, cyF,
                            NIL,
                            hmenu,
                            hInst,
                            NIL);
  TWWnd    := W.CreateWindow (TW.TWCLASSNAME,
                            "",
                            W.WS_CHILD + W.WS_VISIBLE,
                            0, Depth,
                            Width, TWDepth,
                            FrameWnd,
                            NIL,
                            hInst,
                            NIL);
  GraphWnd := W.CreateWindow (MAINCLASSNAME,
                            "",
                            W.WS_CHILD + W.WS_VISIBLE,
                            0, 0,
                            Width, Depth,
                            FrameWnd,
                            NIL,
                            hInst,
                            NIL);

  IF (FrameWnd=NIL) OR (TWWnd=NIL) OR (GraphWnd=NIL) THEN
    W.MessageBox ( NIL, "Couldn't create window(s)", NIL,
                   W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND);
    IF (FrameWnd # NIL) THEN
      W.DestroyWindow(FrameWnd);
    END;
    RETURN 0;
  END;

  WHILE W.GetMessage (msg, NIL, 0, 0) DO
    W.TranslateMessage (msg);
    W.DispatchMessage  (msg);
  END;
  RETURN 0;
END WinMain;


PROCEDURE thrProc ( );
(*PROCEDURE ["StdCall"] thrProc ( p :W.PVOID ) :W.DWORD;*)
BEGIN
  WinMain ( W.GetModuleHandle (NIL), NIL, NIL, 0);
  isInit := FALSE;
  HALT(0);
--  RETURN 0;
END thrProc;



----------------------------------------------------------------------------------

(*////////////////////// parms setting ////////////////////////////*)

PROCEDURE SetLineStyle ( style :LONGINT );
BEGIN
  waitServerReadyB();
  LineStyle := style;
  WITH GObj DO
    lnStyle := style;
    obj     := GComm.setLnStyle;
  END;
  releaseClientReady();
END SetLineStyle;


PROCEDURE GetLineStyle() :LONGINT; BEGIN RETURN LineStyle END GetLineStyle;


PROCEDURE SetStdFillMask ( mask :LONGINT );
BEGIN
  isStdMask := TRUE;
  SFillMask := mask;
  waitServerReadyB();
  WITH GObj DO
    htStyle := mask;
    obj     := GComm.setHatch;
  END;
  releaseClientReady();
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
  waitServerReady();   
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

  waitServerReadyB();
  WITH GObj DO
    bkColor := color;
    obj     := GComm.setBkC;
  END;
  releaseClientReady();
  
  RETURN  c;
END SetBkColor;


PROCEDURE GetBkColor() :LONGCARD; BEGIN RETURN BackColor; END GetBkColor;


PROCEDURE SetBkMix ( isOpaque :BOOLEAN );
BEGIN
  waitServerReadyB();
  WITH GObj DO
    IF isOpaque
      THEN bkMix := W.OPAQUE;
      ELSE bkMix := W.TRANSPARENT;
    END;  
    obj := GComm.setBkMix;
  END;
  releaseClientReady();
END SetBkMix;



PROCEDURE myRect (x0, y0, x1, y1 :LONGCARD; color :LONGCARD; fill :BOOLEAN);
BEGIN
  waitServerReadyB();
  WITH GObj DO
    rRect.left   := x0;
    rRect.top    := y0;
    rRect.right  := x1;
    rRect.bottom := y1;
    rColor := color;
    rFill  := fill;
    obj    := GComm.rect;
  END;
  releaseClientReady();
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
  waitServerReadyB();
  WITH GObj DO
    WITH cursorArea DO
      left   := (curPos.col-1) * fontXd;
      right  := (curPos.col) * fontXd -1;
      top    := (curPos.row-1) * fontYd;
      bottom := (curPos.row) * fontYd -1;
    END;  
    obj    := GComm.cursor;
  END;
  releaseClientReady();
END ToggleCursor;


VAR
  curDisplayed :BOOLEAN;

PROCEDURE CursorOff();
BEGIN
  IF curDisplayed
    THEN ToggleCursor();
         curDisplayed := FALSE;
  END;
END CursorOff;

PROCEDURE CursorOn();
BEGIN
  IF NOT curDisplayed
    THEN ToggleCursor();
         curDisplayed := TRUE;
  END;
END CursorOn;


PROCEDURE DisplayCursor ( Toggle :BOOLEAN) :BOOLEAN;
VAR
  b :BOOLEAN;
BEGIN
  b        := curStatus;
  curStatus := Toggle;
  IF Toggle
    THEN CursorOn();
    ELSE CursorOff();
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
  waitServerReadyB();
  WITH GObj DO
    region.left   := myMIN (x0, x1);
    region.bottom := myMIN (y0, y1);
    region.right  := myMAX (x0, x1)+1;
    region.top    := myMAX (y0, y1)+1;
    obj           := GComm.clip;
  END;
  releaseClientReady();
END SetClipRgn;

PROCEDURE CancelClipRgn ();
BEGIN
  waitServerReadyB();
    GObj.obj := GComm.cancelClip;
  releaseClientReady();
END CancelClipRgn;

 
(*//////////////////////////// graph primitives /////////////////////////////////*)


PROCEDURE ClearScreen ( Area :CARDINAL );
BEGIN
  CursorOff();
  CASE Area OF
    | _GCLEARSCREEN : mySolidRect (0, 0, Width-1, Depth-1, BackColor, TRUE);
    | _GWINDOW      : mySolidRect ( (c1W-1)*fontXd, (r1W-1)*fontYd,
                                   (c2W)*fontXd, (r2W)*fontYd + fontDescend,
                                   BackColor, TRUE );
    | ELSE;
  END;

  DisplayCurs (curStatus);
END ClearScreen;



PROCEDURE Plot ( x, y :LONGCARD; Color :LONGCARD );
BEGIN
  CursorOff();
  waitServerReadyB();
  WITH GObj DO
    pointX := x;
    pointY := y;
    pColor := Color;
    obj    := GComm.plot;
  END;
  releaseClientReady();
  DisplayCurs (curStatus);
END Plot;

PROCEDURE Point ( x, y :LONGCARD ) :LONGCARD;
BEGIN
  CursorOff();
  waitServerReadyB();
  WITH GObj DO
    ppointX:= x;
    ppointY:= y;
    obj    := GComm.point;
  END;
  releaseClientReady();
  DisplayCurs (curStatus);

  RETURN GObj.retColor;
END Point;


PROCEDURE Line ( x1, y1, x2, y2 :LONGCARD; color: LONGCARD);
BEGIN
  CursorOff();
  waitServerReadyB();
  WITH GObj DO
    lineX1 := x1;
    lineY1 := y1;
    lineX2 := x2;
    lineY2 := y2;
    lineColor   := color;
    obj    := GComm.line;
  END;
  releaseClientReady();
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
  waitServerReadyB();
  WITH GObj DO
    eX1 := x0-a;
    eY1 := y0-b;
    eX2 := x0+a;
    eY2 := y0+b;
    eColor := c;
    eFill  := fill;
    obj    := GComm.ellip;
  END;
  releaseClientReady();   -- go, go...

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
  waitServerReadyB();
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
  releaseClientReady();

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
  waitServerReadyB();
  WITH GObj DO
    seedpX     := x;
    seedpY     := y;
    floodColor := color;
    boundColor := boundary;
    obj        := GComm.flood;  -- go, go...
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END FloodFill;


PROCEDURE Arc ( x0, y0, x1, y1 :LONGCARD; startAngle, sweepAngle :LONGREAL; Color :LONGCARD );
VAR
  radSA, radEA     :LONGREAL;
  arcCntX, arcCntY :INTEGER;
BEGIN
  CursorOff();
  waitServerReadyB();
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
    obj           := GComm.arc;  -- go, go...
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END Arc;

PROCEDURE Pie ( x0, y0, x1, y1 :LONGCARD; startAngle, sweepAngle :LONGREAL;
                color :LONGCARD; fill :BOOLEAN );
VAR
  radSA, radEA     :LONGREAL;
  arcCntX, arcCntY :INTEGER;
BEGIN
  CursorOff();
  waitServerReadyB();
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
    obj           := GComm.pie;  -- go, go...
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END Pie;


(*/////////////////////////// Bitmap operations //////////////////////////////*)


PROCEDURE GetImage  (x1, y1, x2, y2 :LONGCARD; VAR handle :HBITMAP);
BEGIN
  Adjust ( x1, 0, Width-1 );
  Adjust ( x2, 0, Width-1 );
  Adjust ( y1, 0, Depth-1 );
  Adjust ( y2, 0, Depth-1 );

  waitServerReadyB();
  WITH GObj DO
    WITH imgArea DO
      left   := myMIN (x1,x2);
      top    := myMIN (y1,y2);
      right  := myMAX (x1,x2)+1;
      bottom := myMAX (y1,y2)+1;
    END;  
    obj    := GComm.getimg;
  END;
  releaseClientReady();

  waitServerReady();
  handle := HBITMAP(GObj.rimgHandle);

END GetImage;


PROCEDURE PutImage  (x, y :LONGCARD; hbm :HBITMAP; action :LONGCARD);
BEGIN
  CursorOff();
  waitServerReadyB();
  WITH GObj DO
    imgLCorn.cx := x;
    imgLCorn.cy := y;
    imgHandle  := W.HBITMAP(hbm);
    imgMode    := action;
    obj        := GComm.putimg;
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END PutImage;


PROCEDURE DelImage ( hbm :HBITMAP );
BEGIN
  waitServerReadyB();
  WITH GObj DO
    delImgHandle := W.HBITMAP(hbm);
    obj          := GComm.delimg;
  END;
  releaseClientReady();
END DelImage;



(*///////////////////////////// Palette operations ////////////////////////////*)


PROCEDURE RemapAllPalette ( colArray- :ARRAY OF LONGCARD ) :LONGCARD;
VAR
  n :CARDINAL;
BEGIN
  n := HIGH(colArray)+1;
  IF ( n > NumPColor ) THEN RETURN MAX(LONGCARD) END;
  n := myMIN ( GComm._mxNColor, n );

  waitServerReadyB();
  WITH GObj DO
    SYSTEM.MOVE ( ADR (colArray), ADR (curPalette), n*SIZE (LONGCARD) );
    numc2map := n;
    obj := GComm.mapAllcol;
  END;
  releaseClientReady();
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
    
    waitServerReadyB();
    obj := GComm.mapAllcol;
    releaseClientReady();
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


PROCEDURE SetTextPosition ( col, row :LONGCARD);
BEGIN
  Adjust ( col, c1W, c2W);
  Adjust ( row, r1W, r2W);

  CursorOff();
  curPos.col := col;
  curPos.row := row;
  DisplayCurs (curStatus);
END SetTextPosition;

PROCEDURE SetTextWindow ( c1, r1, c2, r2  :LONGCARD );
BEGIN
  Adjust ( c1, 1, Columns);
  Adjust ( c2, 1, Columns);
  Adjust ( r1, 1, Rows );
  Adjust ( r2, 1, Rows );

  c1W := myMIN (c1, c2);
  r1W := myMIN (r1, r2);
  c2W := myMAX (c1, c2);
  r2W := myMAX (r1, r2);

  SetTextPosition ( c1W, r1W );
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
  waitServerReadyB();
  WITH GObj DO
    rtxPos.cx   := x1;
    rtxPos.cy   := y1;
    rtxColor   := color;
    Strings.Assign ( Text, rtxText );
    obj        := GComm.rtext;  -- go, go...
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END RawOutText;




PROCEDURE OutText ( Text- :ARRAY OF CHAR);

PROCEDURE outTextPiece ( Text- :ARRAY OF CHAR);
BEGIN
  waitServerReadyB();
  WITH GObj DO
    txPos.cx  := (curPos.col-1) * fontXd;
    txPos.cy  := (curPos.row-1) * fontYd;
    txFontXd := fontXd;
    txFontYd := fontYd;
    txfontDescend := fontDescend;
    txColor  := txtColor;
    txBgr    := BackColor;
    Strings.Assign ( Text, txText );
    obj        := GComm.text;
  END;
  releaseClientReady();
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




VAR
(*  gsTH  :W.HANDLE;*)
  gsTH: Processes.ProcessId;
  sBufL :CARDINAL;

PROCEDURE Init (xlc, ytc, xd, yd, ydTW :LONGCARD ) :BOOLEAN;
VAR
  SearchResult :ARRAY [0..255] OF CHAR;
  si       :W.STARTUPINFO;
  pi       :W.PROCESS_INFORMATION;  -- process ID saved to here (pi.dwProcessId)
  tID      :CARDINAL;
  c        :CARDINAL;
  
  tMetrics :W.TEXTMETRIC;
   
BEGIN
  (* if the module was alreary initialized, bring up the message box *)
  IF isInit
    THEN W.MessageBox ( NIL, "Graph window was already created", "Graph",
                        W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND);
         RETURN FALSE;
  END;
  isInit     := TRUE;

  Width   := xd;
  Depth   := yd;
  xLeftC  := xlc;
  yTopC   := ytc;
  TWDepth := ydTW;

 (* create blocked event semaphore "client Ready" ( drawing thread hangs on it) *)
  hclReadySem := W.CreateEvent ( NIL, TRUE, FALSE, NIL );
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

 (* create unowned mutex semaphore "screen resource" *)
 hScrMutex := W.CreateMutex ( NIL, FALSE, NIL );
 IF ( hScrMutex = NIL )
  THEN W.MessageBox ( NIL, "Mutex not created", "Graph",
                      W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND );
       RETURN FALSE;
  END;

  Processes.Start (     thrProc,
            10000,
                        0,
                        NIL,
                        gsTH
                  );

(*  gsTH := W.CreateThread ( NIL,             -- security attrs
                           8192,            -- initial thread stack size, in bytes
                           thrProc,         -- pointer to thread function
                           NIL,             -- argument for new thread
                           W.CREATE_SET {}, -- creation flags (start immediately )
                           tID              -- pointer to returned thread identifier
                         );
*)


(* IF ( gsTH=NL)  *)
  IF ( gsTH = SYSTEM.CAST(Processes.ProcessId,0 ))  
    THEN W.MessageBox ( NIL,
                       "Graph server not run",
                       "Graph",
                        W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND);
         RETURN FALSE;
  END;
  --waitServerReady();
  
  waitServerReadyB();
    GObj.obj := GComm.init;
  releaseClientReady();

  NumPColor := RGB.numColors;

  InitStdPalette;

  SetLineStyle ( LNSTYLE_SOLID );
  SetBkMix ( _OPAQUE );
  c := SetBkColor (_clrBLACK );
  ClearScreen ( _GCLEARSCREEN );   
   

  W.GetTextMetrics ( perstHDC, tMetrics );
  
  WITH tMetrics DO
    fontYd      := tmHeight;
    fontDescend := tmDescent;
    fontXd      := tmMaxCharWidth;
  END;

  Rows    := (Depth - fontDescend) DIV fontYd;
  Columns :=  Width DIV fontXd;

  SetTextWindow ( 1, 1, Columns, Rows );


  RETURN TRUE;
END Init;


PROCEDURE Exit;
VAR resbool: BOOLEAN;
BEGIN
  SeqFile.Close( TW.outputFile );
  FileSys.Remove( TW.TEMPSIFFILENAME , resbool );
  IF isInit
    THEN isInit := FALSE;

--         W.TerminateThread ( gsTH, 0 );
  END;
END Exit;


PROCEDURE GetKey (): CHAR;
BEGIN
    RETURN TW.get_key();
END GetKey;

PROCEDURE KeyPressed (): BOOLEAN;
BEGIN
    RETURN TW.is_pressed();
END KeyPressed;



PROCEDURE Delay(t :LONGCARD);
BEGIN
  W.Sleep(t);
END Delay;




BEGIN -----------------------------------------------------------------------------------
  isInit       := FALSE;

  LNSTYLE_SOLID         := LONGINT(W.PS_SOLID);
  LNSTYLE_DOT           := LONGINT(W.PS_DOT);
  LNSTYLE_SHORTDASH     := LONGINT(W.PS_DASH);
  LNSTYLE_DASHDOT       := LONGINT(W.PS_DASHDOT);
  LNSTYLE_DASHDOUBLEDOT := LONGINT(W.PS_DASHDOTDOT);
  LNSTYLE_INVISIBLE     := LONGINT(W.PS_NULL);

  PATSYM_DENSE1         := ORD(W.HS_DIAGCROSS);
  PATSYM_DENSE2         := ORD(W.HS_CROSS);
  PATSYM_VERT           := ORD(W.HS_VERTICAL);
  PATSYM_HORIZ          := ORD(W.HS_HORIZONTAL);
  PATSYM_DIAG1          := ORD(W.HS_BDIAGONAL);
  PATSYM_DIAG2          := ORD(W.HS_FDIAGONAL);
  PATSYM_SOLID          := ORD(W.HS_SOLID);

  _GPSET    := W.SRCCOPY;
  _GPRESET  := W.SRCERASE;
  _GAND     := W.SRCAND;
  _GOR      := W.SRCPAINT;
  _GXOR     := W.SRCINVERT;

  wrapMode     := _GWRAPOFF;
  txtColor     := _clrBRIGHTWHITE;
  curStatus    := FALSE;
  curDisplayed := FALSE;
  currentHRGN  := NIL;      

  LineStyle    := LNSTYLE_SOLID;
  SFillMask    := PATSYM_SOLID;
  FILL ( ADR(FillMask), 0, SIZE(FillMask) );
  isStdMask    := TRUE;
  Exiting   := FALSE;
  
FINALLY
  W.CloseHandle ( hservReadySem );
  W.CloseHandle ( hclReadySem );
  W.CloseHandle ( hScrMutex );

  W.EnableMenuItem( hmenuFile, CM_LOADINPUT, W.MF_BYCOMMAND+W.MF_DISABLED+W.MF_GRAYED );
  Strings.Append( finished, exename );
  W.SetWindowText( FrameWnd, SYSTEM.CAST( PCHAR, SYSTEM.ADR(exename) ) );
  WHILE NOT Exiting DO END;

  Exit();

END VTerm.


