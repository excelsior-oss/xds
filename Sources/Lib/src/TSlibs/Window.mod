(* Copyrigth (C) 1996,99 XDS. All rights reserved. *)

<* +M2EXTENSIONS *>
<* +M2ADDTYPES   *>

IMPLEMENTATION MODULE Window;

IMPORT SYSTEM, Storage,
       IOChan, IOLink,
       ChanConsts, StdChans,
       Str, IO, Lib,
       ConM   := xtsConM, 
       WB     := xtsWB,
       iterXY := xtsiterXY,
       zOrder := xtszOrder;

<* IF multithread THEN *>
  IMPORT  Threads;
<* END *>

FROM SYSTEM IMPORT ADR, ADDRESS, FILL, MOVE;

FROM xtsiterXY IMPORT myMIN, myMAX;


CONST
  wndNMax      = zOrder.wndNMax;
  HWND_DESKTOP = zOrder.HWND_DESKTOP;
TYPE
  WinType      = ADDRESS;
  HWND         = zOrder.HWND;
  ATTR         = ConM.ATTR;

---------------------------------------------------------------------------------------------

<* IF multithread THEN *>

VAR 
  Key: Threads.Key;

PROCEDURE getCurW() :HWND;
BEGIN
  RETURN HWND( Threads.GetKeyValue(Key));
END getCurW;

PROCEDURE setCurW( hwnd :HWND );
BEGIN
  Threads.SetKeyValue(Key,ADDRESS(hwnd));
END setCurW;

<* ELSE *>

VAR
  CurW :HWND;

PROCEDURE getCurW() :HWND;
BEGIN
  RETURN CurW;
END getCurW;

PROCEDURE setCurW( hwnd :HWND );
BEGIN
  CurW := hwnd;
END setCurW;


<* END *>



---------------------------------- M a p e r -------------------------------------------------

MODULE Maper;
(* map functionality *)

IMPORT ConM, WB, iterXY, zOrder;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM SYSTEM IMPORT ADDRESS, ADR, FILL, CARD16;

<* IF multithread THEN *>
  IMPORT  Threads;
<* END *>


IMPORT WinDef, p2WDescriptor,
       AbsCoord, RelCoord,
       PaletteRange, PaletteDef,
       TitleStr, TitleMode,

       setCurW, getCurW;


----------------

EXPORT QUALIFIED wndTab,
                 Init, Exit,
                 Allocate, Deallocate,
                 getWDescriptor,
                 At, ShowCursor,
                 MapUpdateWnd, MapUpdateAll, MapUpdateRect,
                 <* IF multithread THEN *>
                 GainAccess, Relinquish,
                 <* END *>
                 RebuildRect, RebuildAll;

----------------

CONST
  wndNMax       = zOrder.wndNMax;
  HWND_DESKTOP  = zOrder.HWND_DESKTOP;
TYPE
  HWND          = zOrder.HWND;

  PBUFF         = WB.PBUFF;

TYPE
  CELL = ConM.CELL;
  ATTR = ConM.ATTR;
  SYM  = ConM.SYM;


  WNDRec = RECORD
             (*
                 the first part returned by the GetWDescriptor proc (with cast)

                  its offsets MUST COINCIDE with the WinDescriptor record ones
              *)

             WDef               :WinDef;               -- see Window.Info
             XA,YA,XB,YB        :AbsCoord;             -- inner window pane
             OWidth,ODepth      :CARDINAL;             -- outer window
             Width, Depth       :CARDINAL;             -- inner pane
             pTitle             :POINTER TO TitleStr;
             tMode              :TitleMode;
             CurrentX, CurrentY :RelCoord;
             isPalette          :BOOLEAN;
             palAttr            :PaletteDef;
             curPalColor        :PaletteRange;
             usrInfo            :ADDRESS;          -- window word

             x1, y1,
             x2, y2      :CARDINAL;                -- outer co-ordinates
             title       :TitleStr;
             myBuff      :WB.PWB;                  -- M2 inheritance is so...
             <* IF multithread THEN *>
             mySem       :Threads.Mutex;           -- window owner's semaphore
             <* END *>
           END;



  PMAP    = POINTER TO ARRAY [0..0FFFFH] OF HWND;
  PWNDRec = POINTER TO WNDRec;

-----------------

VAR
  pMap   :PMAP;


  wndTab :ARRAY [0..wndNMax] OF PWNDRec;

------------------------------------------------ Semaphores

<* IF multithread THEN *>

VAR
  semMap: Threads.Mutex;

PROCEDURE GainAccess;
BEGIN
  Threads.LockMutex ( semMap );
END GainAccess;

PROCEDURE Relinquish;
BEGIN
  Threads.UnlockMutex  ( semMap );
END Relinquish;

<* END *>

---------

PROCEDURE getWDescriptor ( hwnd :HWND ) :p2WDescriptor;
BEGIN
  WITH wndTab[hwnd]^ DO
    WITH myBuff^ DO
      CurrentX := curX;
      CurrentY := curY;
      XA       := inX1;
      YA       := inY1;
      XB       := inX2;
      YB       := inY2;
      OWidth   := xd;
      ODepth   := yd;
      Width    := inX2-inX1+1;
      Depth    := inY2-inY1+1;
    END;
    pTitle := ADR ( title );
  END;

  RETURN p2WDescriptor(wndTab[hwnd]);
END getWDescriptor;


PROCEDURE At ( absX, absY :CARDINAL ) :HWND;
BEGIN
  RETURN pMap^[absY*ConM.Columns + absX];
END At;


PROCEDURE ShowCursor();

  PROCEDURE isCursorShown (w :HWND; VAR absCurX, absCurY :CARDINAL) :BOOLEAN;
  BEGIN
    IF (w = HWND_DESKTOP) THEN RETURN FALSE END;
    WITH wndTab[w]^ DO
      absCurX := x1 + myBuff^.curX;
      absCurY := y1 + myBuff^.curY;
      RETURN WDef.CursorOn & ~WDef.Hidden & (At(absCurX, absCurY) = w) &
             (absCurX < ConM.Columns) & (absCurY < ConM.Rows);
    END;
  END isCursorShown;

VAR
  absCurX, absCurY :CARDINAL;
  gCurX, gCurY     :CARD16;
  curWndAt, curW   :HWND;
BEGIN
  curW := getCurW();

  IF (curW = HWND_DESKTOP) THEN RETURN END;

  IF isCursorShown(curW, absCurX, absCurY) THEN
    ConM.SetCurPos (absCurX, absCurY);
    ConM.CursorOn;
  ELSE
    (* should we make the cursor off? *)
    ConM.GetCurPos(gCurX, gCurY);    -- get current cursor position
    curWndAt := At(VAL(CARDINAL, gCurX), VAL(CARDINAL, gCurY));
    IF NOT isCursorShown(curWndAt, absCurX, absCurY) THEN
      ConM.CursorOff;
    END;
  END;
END ShowCursor;


------------------------------------------------ Maping

(* private for the mapping procedures *)
PROCEDURE getCell ( hwnd :HWND; xScr, yScr :CARDINAL; VAR cell :CELL );
VAR
  p :PBUFF;
  i :CARDINAL;
BEGIN
  IF ( hwnd = HWND_DESKTOP ) THEN
    cell := CELL{ SYM(' '), ConM.MakeAttr( ConM.Blue, ConM.Black ) };
  ELSE
    WITH wndTab[hwnd]^ DO
      <* IF multithread THEN *> WB.GainAccess ( myBuff ); <* END *>
      ASSERT ((xScr >= x1) AND (yScr >= y1));
      WB.GetIndex ( myBuff, xScr-x1, yScr-y1, i );
      WB.GetWinBuf ( myBuff, p );
      cell := p^[i];
      <* IF multithread THEN *> WB.Relinquish ( myBuff ); <* END *>
    END;
  END;
END getCell;


PROCEDURE MapRect ( x1A, y1A, x2A, y2A :CARDINAL );
VAR
  cell :CELL;
  it   :iterXY.iterCL;
BEGIN
  <* IF multithread THEN *>
     GainAccess;
     ConM.GainAccess;
  <* END *>
  iterXY.init ( ConM.Columns, ConM.Rows, x1A, y1A, x2A, y2A, it );
  WHILE ( NOT iterXY.isEnd(it) ) DO
    getCell ( pMap^[it.i], it.x, it.y, cell );
    ConM.pScr^[it.i] := cell;
    iterXY.inc(it);
  END;
  <* IF multithread THEN *>
    Relinquish;
    ConM.Relinquish;
  <* END *>
END MapRect;


PROCEDURE MapAll;
BEGIN
  MapRect ( 0, 0, ConM.Columns-1, ConM.Rows-1 );
END MapAll;


PROCEDURE MapWnd ( hwnd :HWND );
VAR
  cell :CELL;
  it   :iterXY.iterCL;
BEGIN
  <* IF multithread THEN *>
     GainAccess;
     ConM.GainAccess;
  <* END *>
  WITH wndTab[hwnd]^ DO
    iterXY.init ( ConM.Columns, ConM.Rows, x1, y1, x2, y2, it );
  END;
  WHILE ( NOT iterXY.isEnd(it) ) DO
    IF ( hwnd = pMap^[it.i] )
      THEN getCell ( hwnd, it.x, it.y, cell );
           ConM.pScr^[it.i] := cell;
    END;
    iterXY.inc(it);
  END;
  <* IF multithread THEN *>
    Relinquish;
    ConM.Relinquish;
  <* END *>
END MapWnd;

------------------------------------------------ Rebuilding

PROCEDURE RebuildRect ( x1A, y1A, x2A, y2A :CARDINAL );
VAR
  zw :CARDINAL;
  it :iterXY.iterCL;
BEGIN
  <* IF multithread THEN *>
     GainAccess;
     zOrder.GainAccess;
  <* END *>

  iterXY.init ( ConM.Columns, ConM.Rows, x1A, y1A, x2A, y2A, it );
  WHILE ( NOT iterXY.isEnd(it) ) DO
    zw := zOrder.top-1;
    LOOP
      IF ( zw=0 ) THEN EXIT END;
      WITH wndTab[zOrder.list[zw]]^ DO
        IF (NOT WDef.Hidden) AND (it.y>=y1) AND (it.y<=y2) AND
                                 (it.x>=x1) AND (it.x<=x2)
          THEN EXIT
        END;
      END;
      DEC (zw);
    END;
    pMap^[it.i] := zOrder.list[zw];
    iterXY.inc(it);
  END;
  <* IF multithread THEN *>
     Relinquish;
     zOrder.Relinquish;
  <* END *>
END RebuildRect;

PROCEDURE RebuildAll;
BEGIN
  RebuildRect ( 0, 0, ConM.Columns-1, ConM.Rows-1 );
END RebuildAll;

----------------

PROCEDURE MapUpdateRect ( x1, y1, x2, y2 :CARDINAL );
BEGIN
  MapRect ( x1, y1, x2, y2 );
  ConM.UpdateRect ( x1, y1, x2, y2 );
  ShowCursor;
END MapUpdateRect;

PROCEDURE MapUpdateAll;
BEGIN
  MapAll;
  ConM.UpdateAll;
  ShowCursor;
END MapUpdateAll;

PROCEDURE MapUpdateWnd ( hwnd :HWND );
BEGIN
  MapWnd ( hwnd );
  WITH wndTab[hwnd]^ DO
    ConM.UpdateRect ( x1, y1, x2, y2 );
  END;
  ShowCursor;
END MapUpdateWnd;

-----------------

PROCEDURE Allocate ( hwnd :HWND ) :BOOLEAN;
BEGIN
  IF (wndTab[hwnd] # NIL) THEN RETURN FALSE END;
  ALLOCATE ( wndTab[hwnd], SIZE (WNDRec) );
  RETURN (wndTab[hwnd] # NIL);
END Allocate;



PROCEDURE Deallocate ( hwnd :HWND );
BEGIN
  DEALLOCATE ( wndTab[hwnd], SIZE (WNDRec) );
END Deallocate;


----------------

(*!!!!!!!! this proc must be called ONLY after ConM initialization !!!!!!!!!!*)
PROCEDURE Init;
VAR
  i :CARDINAL;
BEGIN
  <* IF multithread THEN *>  Threads.CreateMutex ( semMap ); <* END *>

  FILL ( ADR (wndTab), 0, SIZE (wndTab) );
  ALLOCATE( pMap,  ConM.scrLength*SIZE(HWND) );
  ASSERT (pMap # NIL);

  FOR i:=0 TO wndNMax DO
    wndTab[i] := NIL;
  END;
END Init;

PROCEDURE Exit;
BEGIN
  DEALLOCATE( pMap,  ConM.scrLength*SIZE(HWND) );
  <* IF multithread THEN *> Threads.DeleteMutex ( semMap ); <* END *>
END Exit;

END Maper;



--------------------------------------------------------------------------------------------

(* Private *)

PROCEDURE checkHwndM ( W :WinType; VAR iH :HWND ) :BOOLEAN;
BEGIN
  iH := CARDINAL(W);

  (* Is there a window with such number? *)
  IF ( iH # HWND_DESKTOP ) AND ( iH <= wndNMax ) AND ( Maper.wndTab[iH] # NIL ) THEN
    <* IF multithread THEN *>
      Threads.LockMutex ( Maper.wndTab[iH]^.mySem );
    <* END *>
    RETURN TRUE;
  ELSE
    RETURN FALSE
  END;
END checkHwndM;


PROCEDURE checkHwnd ( W :WinType; VAR iH :HWND ) :BOOLEAN;
BEGIN
  iH := CARDINAL(W);

  (* Is there a window with such number? *)
  RETURN ( iH # HWND_DESKTOP ) AND ( iH <= wndNMax ) AND ( Maper.wndTab[iH] # NIL );
END checkHwnd;


PROCEDURE getPalAttr ( pal- :PaletteDef; n :PaletteRange ) :ATTR;
VAR
  colors :PaletteColorDef;
BEGIN
  colors := pal[n];
  RETURN ConM.MakeAttr ( ConM.COLOR(colors.Fore), ConM.COLOR(colors.Back) );
END getPalAttr;


PROCEDURE getAttr ( hwnd :HWND; frame :BOOLEAN ) :ATTR;
VAR
  attr :ATTR;
BEGIN
  WITH Maper.wndTab[hwnd]^ DO
    IF isPalette THEN
      IF frame
        THEN attr := getPalAttr ( palAttr, FramePaletteColor );
        ELSE attr := getPalAttr ( palAttr, curPalColor );
      END;
    ELSIF frame
      THEN attr := ConM.MakeAttr ( ConM.COLOR(WDef.FrameFore), ConM.COLOR(WDef.FrameBack) );
      ELSE attr := ConM.MakeAttr ( ConM.COLOR(WDef.Foreground), ConM.COLOR(WDef.Background) );
    END;
  END;
  RETURN attr;
END getAttr;


PROCEDURE ordCoords ( VAR x1, y1, x2, y2 :CARDINAL );
VAR
  t :CARDINAL;
BEGIN
    t  := x1;
    x1 := myMIN ( x1, x2 );
    x2 := myMAX ( t,  x2 );
    t  := y1;
    y1 := myMIN ( y1, y2 );
    y2 := myMAX ( t,  y2 );
END ordCoords;

PROCEDURE setWndPosition ( hwnd :HWND; X1, Y1, X2, Y2 :CARDINAL );
BEGIN
  ASSERT ( (X1<=X2) & (Y1<=Y2) );
  WITH Maper.wndTab[hwnd]^ DO
    x1 := X1;
    x2 := X2;
    y1 := Y1;
    y2 := Y2;
  END;
END setWndPosition;


PROCEDURE iSetTitle( hwndInUse :HWND; Title- :ARRAY OF CHAR; Mode :TitleMode);
FORWARD;

PROCEDURE initBuffer ( hwnd :HWND );
BEGIN
  WITH Maper.wndTab[hwnd]^ DO
    WB.Fill ( myBuff, ' ', getAttr (hwnd, FALSE) );
    IF WDef.FrameOn THEN
      (* draw frame & title according with WDef fields *)
      iSetTitle ( hwnd, title, tMode );
    END;
  END;
END initBuffer;

--------------------------------------------------------------------------------------------
TYPE
  PPaletteDef = POINTER TO PaletteDef;

PROCEDURE iOpen ( wd :WinDef; isPal :BOOLEAN; pPal :PPaletteDef ) :WinType;
VAR
  ti       :CARDINAL;
  wXD, wYD :CARDINAL;
BEGIN
  ti := HWND_DESKTOP+1;
  WHILE ( Maper.wndTab[ti] # NIL ) DO
    INC (ti);
    IF (ti > wndNMax) THEN RETURN NIL END;
  END;

  IF NOT Maper.Allocate ( ti ) THEN RETURN NIL END;

  WITH Maper.wndTab[ti]^ DO
    <* IF multithread THEN *>
       Threads.CreateMutex ( mySem );
       Threads.LockMutex  ( mySem );
    <* END *>
    ordCoords ( wd.X1, wd.Y1, wd.X2, wd.Y2 );
    setWndPosition ( ti, wd.X1, wd.Y1, wd.X2, wd.Y2 );
    tMode       := NoTitle;
    WDef        := wd;
    isPalette   := isPal;
    IF isPal THEN
      curPalColor := NormalPaletteColor;
      palAttr     := pPal^;
    END;

    wXD := x2-x1+1;
    wYD := y2-y1+1;

    (* Is there a client area? *)
    IF wd.FrameOn AND ( (wXD<3) OR (wYD<3) ) THEN RETURN NIL END;

    IF NOT WB.Constr ( wXD, wYD, wd.WrapOn, wd.FrameOn, myBuff ) THEN RETURN NIL END;
    initBuffer ( ti );
    zOrder.PutOnTop ( ti );

    Use ( ADDRESS(ti) );     -- make the window current (BEFORE MapUpdate !!!!!)
    IF NOT wd.Hidden
      THEN Maper.RebuildRect ( x1, y1, x2, y2 );
           Maper.MapUpdateRect ( x1, y1, x2, y2 );
    END;

    <* IF multithread THEN *> Threads.UnlockMutex ( mySem ); <* END *>
  END;   (* WITH *)
  RETURN ADDRESS(ti);
END iOpen;



PROCEDURE Open ( wd :WinDef ) :WinType;
BEGIN
  RETURN iOpen ( wd, FALSE, NIL );
END Open;


PROCEDURE Info ( W :WinType; VAR wd :WinDef );
VAR
  hwndInUse :HWND;
BEGIN
  IF NOT checkHwndM ( W, hwndInUse )
    THEN FILL( ADR (wd), 0, SIZE(wd) );
         RETURN;
  END;

  WITH Maper.wndTab[hwndInUse]^ DO
    wd := WDef;
    wd.X1 := x1;
    wd.Y1 := y1;
    wd.X2 := x2;
    wd.Y2 := y2;
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END Info;


PROCEDURE Close( VAR W :WinType );
VAR
  hwndInUse :HWND;
BEGIN
  IF NOT checkHwnd ( W, hwndInUse ) OR (hwndInUse=CARDINAL(FullScreen)) THEN RETURN END;

  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>
       Threads.LockMutex  ( mySem );
       Threads.UnlockMutex   ( mySem );
       Threads.DeleteMutex ( mySem );
    <* END *>

    zOrder.Kill (hwndInUse);
    Maper.RebuildRect ( x1, y1, x2, y2 );
    IF NOT WDef.Hidden THEN Maper.MapUpdateRect ( x1, y1, x2, y2 ) END;
    WB.Destr ( myBuff );
  END;

  Maper.Deallocate ( hwndInUse );

  IF (hwndInUse = getCurW())
    THEN Use (Top());
  END;

  W := NIL;
END Close;



PROCEDURE Use ( W :WinType );
VAR
  hwndInUse :HWND;
BEGIN
  IF NOT checkHwndM ( W, hwndInUse ) THEN RETURN END;
  setCurW ( hwndInUse );
  Maper.ShowCursor;
  <* IF multithread THEN *>  Threads.UnlockMutex ( Maper.wndTab[hwndInUse]^.mySem ); <* END *>
END Use;



PROCEDURE Used() :WinType; BEGIN RETURN ADDRESS(getCurW()) END Used;



PROCEDURE Top() :WinType;
VAR
  t :WinType;
BEGIN
  <* IF multithread THEN *> zOrder.GainAccess; <* END *>
  t := ADDRESS(zOrder.list[zOrder.top-1]);
  <* IF multithread THEN *> zOrder.Relinquish; <* END *>
  RETURN t;
END Top;



PROCEDURE Hide ( W :WinType );
VAR
  hwndInUse :HWND;
BEGIN
  IF NOT checkHwndM ( W, hwndInUse ) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    IF NOT WDef.Hidden
      THEN WDef.Hidden := TRUE;
           Maper.RebuildRect ( x1, y1, x2, y2 );
           Maper.MapUpdateRect ( x1, y1, x2, y2 );
    END;
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END Hide;



PROCEDURE Show ( W :WinType );
VAR
  hwndInUse :HWND;
BEGIN
  IF NOT checkHwndM ( W, hwndInUse ) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    WDef.Hidden := FALSE;
    Maper.RebuildRect ( x1, y1, x2, y2 );
    Maper.MapUpdateWnd (hwndInUse);
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END Show;




PROCEDURE PutOnTop ( W :WinType );
VAR
  hwndInUse :HWND;
BEGIN
  IF NOT checkHwndM ( W, hwndInUse ) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    zOrder.PutOnTop ( hwndInUse );
    WDef.Hidden := FALSE;
    Maper.RebuildRect ( x1, y1, x2, y2 );
    Maper.MapUpdateRect ( x1, y1, x2, y2 );
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END PutOnTop;




PROCEDURE PutBeneath ( W, WA :WinType );
VAR
  hwA, hwndInUse :HWND;
BEGIN
  IF NOT checkHwnd ( WA, hwA ) THEN RETURN END;        -- don't capture, just get HWND
  IF NOT checkHwndM (W, hwndInUse ) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    zOrder.PutBeneath ( hwndInUse, hwA );
    Maper.RebuildRect ( x1, y1, x2, y2 );
    Maper.MapUpdateRect ( x1, y1, x2, y2 );
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END PutBeneath;




PROCEDURE iSetTitle( hwndInUse :HWND; Title- :ARRAY OF CHAR; Mode :TitleMode);
VAR
  tM, tL :CARDINAL;
  tX, tY :CARDINAL;
BEGIN
  WITH Maper.wndTab[hwndInUse]^ DO
    IF NOT WDef.FrameOn THEN RETURN END;

    Str.Copy ( title, Title ); -- <-
    tMode := Mode;
    WITH myBuff^ DO
      WB.DrawFrame ( myBuff, WDef.FrameDef, getAttr ( hwndInUse, TRUE) );
      IF ( Mode # NoTitle )
        THEN tM := ORD (Mode) - ORD (LeftUpperTitle);
             tL := myMIN ( LENGTH (Title), xd-2 );
             tY := (tM DIV 3)*(yd-1);
             CASE (tM MOD 3) OF
               |0 : tX := 1;
               |1 : tX := (xd-tL) DIV 2;  -- always >=1 (see above)
               |2 : tX := xd-tL-1;
             END;
             WB.RawWrite ( myBuff, tX, tY, Title, 0, tL, getAttr ( hwndInUse, TRUE) );
      END;
      Maper.MapUpdateRect ( x1, y1, x2, y1 );
      Maper.MapUpdateRect ( x1, y2, x2, y2 );
    END;
  END;
END iSetTitle;

PROCEDURE SetTitle ( W :WinType; Title- :ARRAY OF CHAR; Mode :TitleMode);
VAR
  hwndInUse :HWND;
BEGIN
  IF NOT checkHwndM ( W, hwndInUse ) THEN RETURN END;
  iSetTitle ( hwndInUse, Title, Mode );
  <* IF multithread THEN *>  Threads.UnlockMutex ( Maper.wndTab[hwndInUse]^.mySem ); <* END *>
END SetTitle;



PROCEDURE SetFrame( W :WinType; Frame- :FrameStr; Fore, Back :Color);
VAR
  hwndInUse :HWND;
BEGIN
  IF NOT checkHwndM ( W, hwndInUse ) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    WITH WDef DO
      IF NOT FrameOn THEN RETURN END;
      FrameDef  := Frame;
      FrameFore := Fore;
      FrameBack := Back;
    END;
    SetTitle ( W, title, tMode );
    Maper.MapUpdateWnd (hwndInUse);
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END SetFrame;



PROCEDURE TextColor ( c :Color );
VAR
  hwndInUse :HWND;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    WDef.Foreground := c;
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END TextColor;



PROCEDURE TextBackground ( c :Color );
VAR
  hwndInUse :HWND;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    WDef.Background := c;
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END TextBackground;



PROCEDURE At ( x, y :AbsCoord) :WinType;
BEGIN
  IF (x < ConM.Columns) AND (y < ConM.Rows)
    THEN RETURN ADDRESS(Maper.At ( x, y ))
    ELSE RETURN NIL
  END;
END At;


PROCEDURE ObscuredAt ( W :WinType; x, y :RelCoord) :BOOLEAN;
VAR
  hwndInUse :HWND;
  b         :BOOLEAN;
BEGIN
  IF NOT checkHwndM ( W, hwndInUse ) THEN RETURN FALSE END;
  WITH Maper.wndTab[hwndInUse]^ DO
    WB.clipXY ( myBuff, x, y, FALSE );
    b := NOT (At ( x1+x, y1+y ) = W );
    <* IF multithread THEN *> Threads.UnlockMutex ( mySem ); <* END *>
  END;
  RETURN b;
END ObscuredAt;



PROCEDURE GotoXY ( x, y :RelCoord );
VAR
  hwndInUse :HWND;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;

  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    WB.clipXY ( myBuff, x, y, FALSE );
    myBuff^.curX := x;
    myBuff^.curY := y;
    Maper.ShowCursor;
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END GotoXY;




PROCEDURE WhereX() :RelCoord;
VAR
  hwndInUse :HWND;
  c         :RelCoord;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN 0 END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    c := myBuff^.curX;
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
  RETURN c;
END WhereX;


PROCEDURE WhereY() :RelCoord;
VAR
  hwndInUse :HWND;
  c         :RelCoord;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN 0 END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    c := myBuff^.curY;
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
  RETURN c;
END WhereY;


PROCEDURE ConvertCoords ( W :WinType; X, Y :RelCoord; VAR X0, Y0 :AbsCoord );
VAR
  hwndInUse :HWND;
BEGIN
  IF NOT checkHwndM ( W, hwndInUse ) THEN RETURN END;

  WITH Maper.wndTab[hwndInUse]^ DO
    WB.clipXY ( myBuff, X, Y, FALSE );
    X0 := x1+X;
    Y0 := y1+Y;
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END ConvertCoords;



PROCEDURE Clear;
VAR
  hwndInUse :HWND;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    WB.Fill ( myBuff, ' ', getAttr (hwndInUse, FALSE) );
    IF NOT WDef.Hidden
      THEN Maper.MapUpdateWnd ( hwndInUse );
    END;
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END Clear;


PROCEDURE CursorOn;
VAR
  hwndInUse :HWND;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    WDef.CursorOn := TRUE;
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
  Maper.ShowCursor;
END CursorOn;


PROCEDURE CursorOff;
VAR
  hwndInUse :HWND;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    WDef.CursorOn := FALSE;
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
  ConM.CursorOff;
END CursorOff;


PROCEDURE SetWrap ( on :BOOLEAN );
VAR
  hwndInUse :HWND;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    WDef.WrapOn      := on;
    WB.SetWrap (myBuff, on, WDef.FrameOn );
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END SetWrap;


PROCEDURE DelLine;
VAR
  hwndInUse :HWND;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    WB.LScrollUp ( myBuff, myBuff^.curY, getAttr (hwndInUse, FALSE) );
    Maper.MapUpdateWnd ( hwndInUse );
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END DelLine;



PROCEDURE InsLine;
VAR
  hwndInUse :HWND;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    WB.LScrollDown ( myBuff, myBuff^.curY, getAttr (hwndInUse, FALSE) );
    Maper.MapUpdateWnd ( hwndInUse );
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END InsLine;



PROCEDURE ClrEol;
VAR
  hwndInUse :HWND;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    WITH Maper.wndTab[hwndInUse]^.myBuff^ DO
      WB.RawWriteNChar ( Maper.wndTab[hwndInUse]^.myBuff, curX, curY,
                         inX2-curX+1, ' ', getAttr (hwndInUse, FALSE) );

    END;
    Maper.MapUpdateRect ( x1, y1+myBuff^.curY, x2, y1+myBuff^.curY );
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END ClrEol;



PROCEDURE DirectWrite( x,y :RelCoord; A :ADDRESS; len :CARDINAL);
TYPE
  p2C = POINTER TO ARRAY [0..0FFFFH] OF CHAR;
VAR
  p         :p2C;
  hwndInUse :HWND;
  xd        :CARDINAL;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;
  p := p2C (A);
  WITH Maper.wndTab[hwndInUse]^ DO
  <* IF multithread THEN *> Threads.LockMutex (mySem); <* END *>

    WB.clipXY ( myBuff, x, y, FALSE );
    WB.RawWrite ( myBuff, x, y, p^, 0, len, getAttr (hwndInUse, FALSE) );

    xd := myBuff^.xd;
    IF (xd-x <= len) THEN
      Maper.MapUpdateRect ( x1+x,
                            y1+y,
                            x1+x+len-1,
                            y1+y );
    ELSE
      Maper.MapUpdateRect ( x1,
                            y1+y,
                            x1+xd-1,
                            y1+ y + ((x+len+xd-1) DIV xd) - 1 );
    END;
  <* IF multithread THEN *> Threads.UnlockMutex (mySem); <* END *>
  END;
END DirectWrite;



PROCEDURE Change ( W :WinType; X1, Y1, X2, Y2 :AbsCoord );
VAR
  wXD, wYD       :CARDINAL;
  wcaX, wcaY     :CARDINAL;
  pOldwb         :WB.PWB;
  pSbuff, pDbuff :WB.PBUFF;
  hwndInUse      :HWND;
  si, di         :iterXY.iterCL;
  enclX1, enclY1,
  enclX2, enclY2 :CARDINAL;
BEGIN
  IF NOT checkHwndM ( W, hwndInUse ) THEN RETURN END; -- captures mutex
  ordCoords ( X1, Y1, X2, Y2 );
  wXD := X2-X1+1;
  wYD := Y2-Y1+1;

  WITH Maper.wndTab[hwndInUse]^ DO
    enclX1 := myMIN ( x1, X1);
    enclY1 := myMIN ( y1, Y1);
    enclX2 := myMAX ( x2, X2);
    enclY2 := myMAX ( y2, Y2);
  <* IF multithread THEN *> Maper.GainAccess; <* END *>
  LOOP
    (* Is there a client area? *)
    IF WDef.FrameOn AND ( (wXD<3) OR (wYD<3) ) THEN EXIT END;

    IF (myBuff^.xd # wXD) OR (myBuff^.yd # wYD) THEN
     pOldwb := myBuff;
     IF NOT WB.Constr ( wXD, wYD, WDef.WrapOn, WDef.FrameOn, myBuff ) THEN
        myBuff := pOldwb;
        EXIT;
     END;
     setWndPosition ( hwndInUse, X1, Y1, X2, Y2 );

     <* IF multithread THEN *> WB.GainAccess ( myBuff ); <* END *>
     initBuffer ( hwndInUse );

     WITH myBuff^ DO
       curX := pOldwb^.curX;
       curY := pOldwb^.curY;
       WB.clipXY ( myBuff, curX, curY, TRUE );

       wcaX := myMIN ( inX2-inX1, pOldwb^.inX2-pOldwb^.inX1 );
       wcaY := myMIN ( inY2-inY1, pOldwb^.inY2-pOldwb^.inY1 );
       iterXY.init ( xd, yd, inX1, inY1, inX1+wcaX, inY1+wcaY, di );
     END;
     WITH pOldwb^ DO
       iterXY.init ( xd, yd, inX1, inY1, inX1+wcaX, inY1+wcaY, si );
     END;

     WB.GetWinBuf ( pOldwb, pSbuff );
     WB.GetWinBuf ( myBuff, pDbuff );

     WHILE ( NOT iterXY.isEnd(si) ) DO
       pDbuff^[di.i] := pSbuff^[si.i];
       iterXY.inc(si);
       iterXY.inc(di);
     END;
     <* IF multithread THEN *> WB.Relinquish ( myBuff ); <* END *>

     WB.Destr ( pOldwb );
    ELSE
      setWndPosition ( hwndInUse, X1, Y1, X2, Y2 );
    END;

    IF NOT WDef.Hidden THEN
      Maper.RebuildRect ( enclX1, enclY1, enclX2, enclY2 );
      Maper.MapUpdateRect ( enclX1, enclY1, enclX2, enclY2 );
    END;
    EXIT;
  END; (* LOOP *)
  <* IF multithread THEN *>
    Maper.Relinquish;
    Threads.UnlockMutex ( mySem );
  <* END *>
  END;
END Change;



PROCEDURE SnapShot;
VAR
  wcaX, wcaY :CARDINAL;
  pDbuff     :WB.PBUFF;
  si, di     :iterXY.iterCL;
BEGIN
  IF (getCurW()=HWND_DESKTOP) THEN RETURN END;
  WITH Maper.wndTab[getCurW()]^ DO
    IF isPalette OR NOT WDef.Hidden THEN RETURN END;

   <* IF multithread THEN *> WB.GainAccess ( myBuff ); <* END *>
    WITH myBuff^ DO
      wcaX := myMIN ( ConM.Columns-1, inX2-inX1 );
      wcaY := myMIN ( ConM.Rows-1, inY2-inY1 );
      iterXY.init ( ConM.Columns, ConM.Rows, 0, 0, wcaX, wcaY, si );
      iterXY.init ( xd, yd, inX1, inY1, inX1+wcaX, inY1+wcaY, di );
    END;

    WB.GetWinBuf ( myBuff, pDbuff );

    WHILE ( NOT iterXY.isEnd(si) ) DO
      pDbuff^[di.i] := ConM.pScr^[si.i];
      iterXY.inc(si);
      iterXY.inc(di);
    END;
    <* IF multithread THEN *> WB.Relinquish ( myBuff ); <* END *>

  END;
END SnapShot;



----------------------------- P a l e t t e   w i n d o w s -----------------------------


PROCEDURE PaletteOpen ( WD :WinDef; Pal- :PaletteDef ) :WinType;
BEGIN
  RETURN iOpen ( WD, TRUE, PPaletteDef (ADR (Pal) ) );
END PaletteOpen;


PROCEDURE SetPalette ( W :WinType; newPal- :PaletteDef );
VAR
  oldPal    :PaletteDef;
  pDbuff    :WB.PBUFF;
  i         :CARDINAL;
  attr      :ATTR;
  hwndInUse :HWND;
  it        :iterXY.iterCL;
BEGIN
  IF NOT checkHwndM ( W, hwndInUse ) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    IF NOT isPalette THEN RETURN END;
    oldPal  := palAttr;

    <* IF multithread THEN *> WB.GainAccess ( myBuff ); <* END *>
    WB.GetWinBuf ( myBuff, pDbuff );
    WITH myBuff^ DO
      iterXY.init ( xd, yd, 0, 0, xd-1, yd-1, it );
    END;
    WHILE ( NOT iterXY.isEnd(it) ) DO
      attr := pDbuff^[it.i].Attributes;
      i := MIN (PaletteRange);
      LOOP
        IF (i>MAX (PaletteRange)) THEN EXIT END;
        IF ( ConM.CompareAttrs(attr, getPalAttr(oldPal,i)) )
          THEN pDbuff^[it.i].Attributes := getPalAttr ( newPal, i );
               EXIT;
        END;
        INC (i);
      END;
      iterXY.inc(it);
    END;
    <* IF multithread THEN *> WB.Relinquish ( myBuff ); <* END *>

    palAttr := newPal;
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
  Maper.MapUpdateWnd ( hwndInUse );
END SetPalette;


PROCEDURE PaletteColor() :PaletteRange;
BEGIN
  IF (getCurW()=HWND_DESKTOP) THEN RETURN 0 END;
  WITH Maper.wndTab[getCurW()]^ DO
    IF isPalette
      THEN RETURN curPalColor;
      ELSE RETURN 0
    END;
  END;
END PaletteColor;


PROCEDURE SetPaletteColor ( pc :PaletteRange );
VAR
  hwndInUse :HWND;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    curPalColor := pc;
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END SetPaletteColor;



PROCEDURE PaletteColorUsed ( W :WinType; pc :PaletteRange ) :BOOLEAN;
VAR
  attr      :ATTR;
  pDbuff    :WB.PBUFF;
  hwndInUse :HWND;
  it        :iterXY.iterCL;
BEGIN
  IF NOT checkHwnd ( W, hwndInUse ) THEN RETURN FALSE END;
  WITH Maper.wndTab[hwndInUse]^ DO
    IF NOT isPalette THEN RETURN FALSE END;
    attr := getPalAttr ( palAttr, pc );

    <* IF multithread THEN *> WB.GainAccess ( myBuff ); <* END *>
    WB.GetWinBuf ( myBuff, pDbuff );
    WITH myBuff^ DO
      iterXY.init ( xd, yd, 0, 0, xd-1, yd-1, it );
    END;

    WHILE ( NOT iterXY.isEnd(it) ) DO
      IF ( ConM.CompareAttrs(attr, pDbuff^[it.i].Attributes) )
        THEN <* IF multithread THEN *> WB.Relinquish ( myBuff ); <* END *>
             RETURN TRUE;
      END;
      iterXY.inc(it);
    END;
    <* IF multithread THEN *> WB.Relinquish ( myBuff ); <* END *>
  END;

  RETURN FALSE;
END PaletteColorUsed;


----------------------------------------------------------------------------------------


PROCEDURE RdBufferLn ( W :WinType; X,Y :RelCoord; Dest :ADDRESS; Len :CARDINAL );
VAR
  pbuff :WB.PBUFF;
  bi    :CARDINAL;
  hwndInUse :HWND;
BEGIN
  IF NOT checkHwnd ( W, hwndInUse ) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *> WB.GainAccess ( myBuff ); <* END *>
    WB.clipXY    ( myBuff, X, Y, FALSE );
    WB.GetIndex  ( myBuff, X, Y, bi );
    WB.GetWinBuf ( myBuff, pbuff );
    MOVE ( ADR (pbuff^[bi]), Dest, SIZE (ConM.CELL)*Len );
    <* IF multithread THEN *> WB.Relinquish ( myBuff ); <* END *>
  END;
END RdBufferLn;




PROCEDURE WrBufferLn ( W :WinType; X,Y :RelCoord; Src :ADDRESS; Len :CARDINAL );
VAR
  pbuff     :WB.PBUFF;
  bi, biEnd :CARDINAL;
  hwndInUse :HWND;
BEGIN
  IF NOT checkHwnd ( W, hwndInUse ) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *> WB.GainAccess ( myBuff ); <* END *>

    WB.clipXY    ( myBuff, X, Y, FALSE );
    WB.GetIndex  ( myBuff, X, Y, bi );
    WB.GetIndex  ( myBuff, myBuff^.xd-1, myBuff^.yd-1, biEnd );
    WB.GetWinBuf ( myBuff, pbuff );

    Len := myMIN ( Len, biEnd-bi+1);
    MOVE (  Src, ADR (pbuff^[bi]), SIZE (ConM.CELL)*Len );
    Maper.MapUpdateWnd ( hwndInUse );
    <* IF multithread THEN *> WB.Relinquish ( myBuff ); <* END *>
  END;
END WrBufferLn;





--------------------------- Redirected input/output  --------------------------------------


PROCEDURE iWriteOut ( S- :ARRAY OF CHAR; maxStrL :CARDINAL );

CONST
  RCZ_CTRLS = {7,8,10,12,13}; -- recognized control symbols

VAR
  attr :ATTR;
  pwb  :WB.PWB;

  PROCEDURE  WriteControl ( C :CHAR );
  BEGIN
    WITH pwb^ DO
      CASE C OF
        | CHR(12) : Clear;
                    GotoXY ( inX1, inY1 );
        | CHR(13):  GotoXY ( inX1, curY );
        | CHR(10) : GotoXY ( inX1, curY );
                    IF (curY=inY2)
                      THEN WB.LScrollUp ( pwb, inY1, attr )
                      ELSE INC ( curY );
                    END;
        | CHR(8)  : IF (curX>inX1)
                      THEN DEC ( curX );
                           WB.WrChar ( pwb, ' ', attr );
                           DEC ( curX );
                    END;
        | CHR(7)  : Lib.Speaker (1000,300);
        ELSE
         WB.WrChar ( pwb, C, attr );
      END;
    END;
  END WriteControl;

VAR
  begPos, endNPos :CARDINAL;
  rx0,ry0         :CARDINAL;
  wasScroll       :BOOLEAN;
  hwndInUse       :HWND;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;

  attr := getAttr ( hwndInUse, FALSE );
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *>  Threads.LockMutex ( mySem ); <* END *>
    pwb  := myBuff;

    begPos  := 0;
    endNPos := 0;

    WHILE (endNPos < maxStrL) AND ~(ORD(S[endNPos]) IN RCZ_CTRLS) DO INC (endNPos) END;

    IF (endNPos = maxStrL) THEN
      (* in this case we can optimize window updating *)
      rx0 := pwb^.curX;
      ry0 := pwb^.curY;
      wasScroll := WB.WrStrSlice ( pwb, S, begPos, maxStrL, attr );
      IF (wasScroll) THEN
        Maper.MapUpdateWnd ( hwndInUse );
      ELSIF ry0=pwb^.curY THEN
        Maper.MapUpdateRect ( x1+rx0,       (* within single line *)
                              y1+ry0,
                              x1+pwb^.curX,
                              y1+ry0 );
      ELSE ASSERT(ry0<pwb^.curY);           (* multiple lines wrapped *)
        Maper.MapUpdateRect ( x1,
                              y1+ry0,
                              x2,
                              y1+pwb^.curY );
      END;                    
    ELSE
      WHILE (endNPos < maxStrL) DO
        wasScroll := WB.WrStrSlice ( pwb, S, begPos, endNPos-begPos, attr );

        WHILE (endNPos < maxStrL) AND (ORD(S[endNPos]) IN RCZ_CTRLS) DO
          WriteControl (S[endNPos]);
          INC (endNPos);
        END;
        begPos := endNPos;
        
        WHILE (endNPos < maxStrL) AND ~(ORD(S[endNPos]) IN RCZ_CTRLS) DO INC (endNPos) END;
      END;
      Maper.MapUpdateWnd ( hwndInUse ); (* cannot optimize *)
    END;

    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END iWriteOut;

PROCEDURE WriteOut ( S- :ARRAY OF CHAR );
BEGIN
  iWriteOut ( S, LENGTH (S) );
END WriteOut;


PROCEDURE RawWriteOut ( S- :ARRAY OF CHAR; len :CARDINAL );
VAR
  hwndInUse :HWND;
  rx0,ry0   :CARDINAL;
  wasScroll :BOOLEAN;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;
  WITH Maper.wndTab[hwndInUse]^ DO
    <* IF multithread THEN *> Threads.LockMutex ( mySem ); <* END *>

      (* in this case we can optimize window updating *)
      rx0 := myBuff^.curX;
      ry0 := myBuff^.curY;
      wasScroll := WB.WrStrSlice ( myBuff, S, 0, len, getAttr ( hwndInUse, FALSE ) );
      IF (wasScroll) THEN
        Maper.MapUpdateWnd ( hwndInUse );
      ELSIF ry0=myBuff^.curY THEN
        Maper.MapUpdateRect ( x1+rx0,          (* within single line *)
                              y1+ry0,
                              x1+myBuff^.curX,
                              y1+ry0 );
      ELSE ASSERT(ry0<myBuff^.curY);           (* multiple lines wrapped *)
        Maper.MapUpdateRect ( x1,
                              y1+ry0,
                              x2,
                              y1+myBuff^.curY );
      END;                    
    <* IF multithread THEN *>  Threads.UnlockMutex ( mySem ); <* END *>
  END;
END RawWriteOut;


PROCEDURE InputStr ( VAR S :ARRAY OF CHAR );
TYPE
  a1Char = ARRAY [0..0] OF CHAR;
VAR
  ins      :BOOLEAN;  -- insert flag
  k        :CHAR;
  sX, sY,
  p, sH    :CARDINAL;
  curState :BOOLEAN;
  hwndInUse:HWND;
BEGIN
  hwndInUse := getCurW();
  IF (hwndInUse=HWND_DESKTOP) THEN RETURN END;

  WITH Maper.wndTab[hwndInUse]^ DO
    sX  := WhereX();
    sY  := WhereY();
    WB.clipXY (myBuff, sX, sY, TRUE);
    p   := 0;
    ins := TRUE;
    sH  := myMIN ( HIGH (S), myBuff^.inX2-sX );

    LOOP
      p := myMIN ( p, sH );
      Str.Slice ( S, S, 0, myMIN ( LENGTH (S), sH+1) );
      curState := WDef.CursorOn;
      CursorOff;
      GotoXY ( sX, sY );
      IO.WrStr (S); ClrEol;
      GotoXY ( sX+p, sY );
      IF curState THEN CursorOn END;

      k := IO.RdCharDirect();
      IF k = 0C THEN                (* Extended character *)
        CASE IO.RdCharDirect() OF
        | CHR(75) : k := CHR(19) ;   (* LeftArr  -> ^S *)
        | CHR(77) : k := CHR(4)  ;   (* RightArr -> ^D *)
        | CHR(71) : k := CHR(1)  ;   (* Home     -> ^A *)
        | CHR(79) : k := CHR(6)  ;   (* End      -> ^F *)
        | CHR(83) : k := CHR(7)  ;   (* Del      -> ^G *)
        | CHR(82) : k := CHR(22) ;   (* Ins      -> ^V *)
        ELSE
        END ;
      END ;
      CASE k OF
      | ' '..'~' : IF ins THEN
                     Str.Insert ( S, a1Char(k), p );
                   ELSIF ( p=sH ) THEN 
                     Str.Append ( S, a1Char(k) );
                   ELSE
                     S[p] := k;
                   END;
                   INC(p);
      | CHR(1)   : p := 0;                    (* Home    *)
      | CHR(6)   : p := sH;                   (* End     *)
      | CHR(19)  : IF (p>0) THEN DEC(p) END;  (* Left    *)
      | CHR(4)   : INC (p);                   (* Right   *)
      | CHR(7)   : Str.Delete (S,p,1);        (* Del     *)
      | CHR(8)   : IF (p>0) THEN              (* BackSpace *)
                     DEC (p);
                     Str.Delete (S,p,1);
                   END;
      | CHR(22)  : ins := NOT ins;            (* Toggle Ins/Ovr *)
      | CHR(13)  : RETURN;                    (* Enter *)
      ELSE
      END ;
    END ;
  END;
END InputStr;


PROCEDURE GetWDescriptor ( W :WinType ) :p2WDescriptor;
VAR
  hwndInUse :HWND;
  p         :p2WDescriptor;
BEGIN
  IF NOT checkHwndM ( W, hwndInUse ) THEN RETURN NIL END;

  p := Maper.getWDescriptor ( hwndInUse );
  <* IF multithread THEN *> Threads.UnlockMutex ( Maper.wndTab[hwndInUse]^.mySem ); <* END *>
  RETURN p;
END GetWDescriptor;


--------------------------------- ISO channel --------------------------------------------

TYPE
  pAr = POINTER TO ARRAY [0..0FFFFH] OF CHAR;

VAR
  did :IOLink.DeviceId;
  cid :IOChan.ChanId;

PROCEDURE ISOrawWrite ( pDT :IOLink.DeviceTablePtr; src :ADDRESS; len :CARDINAL );
VAR
  p :pAr;
BEGIN
  p := pAr(src);
  RawWriteOut ( p^, len );
END ISOrawWrite;


PROCEDURE ISOtextRead ( pDT :IOLink.DeviceTablePtr; src :ADDRESS;
                        maxChars :CARDINAL; VAR charsRead :CARDINAL );
VAR
  p   :pAr;
  str :ARRAY [0..512] OF CHAR;
BEGIN
  str := "";
  InputStr ( str );
  charsRead := myMIN ( LENGTH (str), maxChars );
  p := pAr(src);
  Str.Slice ( p^, str, 0, charsRead );
END ISOtextRead;


PROCEDURE ISOtextWrite ( pDT :IOLink.DeviceTablePtr; src :ADDRESS; len :CARDINAL );
VAR
  p   :pAr;
BEGIN
  p := pAr(src);
  iWriteOut ( p^, len );
END ISOtextWrite;


PROCEDURE ISOWrLn ( pDT :IOLink.DeviceTablePtr );
VAR
  fuflo :BOOLEAN;
BEGIN
  IF (getCurW()=HWND_DESKTOP) THEN RETURN END;
  fuflo := WB.WrLn ( Maper.wndTab[getCurW()]^.myBuff, getAttr ( getCurW(), FALSE ) );
  Maper.MapUpdateWnd ( getCurW() );
END ISOWrLn;



PROCEDURE MakeChannel;
VAR
  pt  :IOLink.DeviceTablePtr;
BEGIN
  IOLink.AllocateDeviceId ( did );
  IOLink.MakeChan ( did, cid );
  IF ( cid = IOChan.InvalidChan() ) THEN RETURN END;

  pt := IOLink.DeviceTablePtrValue ( cid, did, IOChan.notAvailable, "" );

  pt^.flags       := ChanConsts.text + ChanConsts.raw + ChanConsts.write + ChanConsts.raw;
  pt^.doRawWrite  := ISOrawWrite;
  pt^.doTextRead  := ISOtextRead;
  pt^.doTextWrite := ISOtextWrite;
  pt^.doLnWrite   := ISOWrLn;

END MakeChannel;


PROCEDURE UnMakeChannel();
BEGIN
  IOLink.UnMakeChan (did,cid);
  IOLink.AllocateDeviceId ( did );
END UnMakeChannel;


------------------------------------------------------------------------------------------

PROCEDURE closeAll();
VAR
  i :CARDINAL;
BEGIN
  FOR i := 1 TO wndNMax DO
    IF ( Maper.wndTab[i] # NIL )
      THEN <* IF multithread THEN *> Threads.DeleteMutex( Maper.wndTab[i]^.mySem ); <* END *>
             WB.Destr ( Maper.wndTab[i]^.myBuff );
             Maper.Deallocate ( i );
    END;
  END;
END closeAll;


VAR
  oldTSIn  :IO.RdStrType;
  oldTSOut :IO.WrStrType;
  oldISOR,
  oldISOW  :IOChan.ChanId;
BEGIN
<* IF multithread THEN *>
  Key := Threads.CreateKey();
<* END *>

  (* TS IO redirecting *)
  oldTSIn  := IO.RdStrRedirect;
  oldTSOut := IO.WrStrRedirect;
  IO.RdStrRedirect := InputStr;
  IO.WrStrRedirect := WriteOut;

  CurrentScreenWidth := ConM.Columns;
  CurrentScreenDepth := ConM.Rows;
  setCurW ( HWND_DESKTOP );

---------

  (* ISO redirecting *)
  oldISOR := StdChans.StdInChan();
  oldISOW := StdChans.StdOutChan();
  MakeChannel;
  StdChans.SetInChan  ( cid );
  StdChans.SetOutChan ( cid );

---------

  Maper.Init;
  Maper.RebuildAll;
  Maper.MapUpdateAll;

  FullScreen := Open ( FullScreenDef );
  setCurW ( CARDINAL(FullScreen) );
FINALLY
  closeAll();
  Maper.Exit;

  IO.RdStrRedirect := oldTSIn;
  IO.WrStrRedirect := oldTSOut;

  StdChans.SetInChan  ( oldISOR );
  StdChans.SetOutChan ( oldISOW );
  UnMakeChannel;
<* IF multithread THEN *>
  Threads.DeleteKey(Key);
<* END *>
END Window.
