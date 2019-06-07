(* Copyright (C) 1999-2000 Excelsior *)
(*  03.01.1999 - FSA: mouse events *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

<* -IOVERFLOW    *>
<* -COVERFLOW    *>
<* -CHECKINDEX   *>
<* -CHECKRANGE   *>
<* -CHECKNIL     *>

IMPLEMENTATION MODULE Graph;

IMPORT O := OS2,
       Strings,
       WholeStr,
       IO,
       xtsEvQue,
       GComm := xtsGComm,
       M     :=  MATHLIB;


FROM SYSTEM IMPORT ADR, FILL, ADDRESS;

VAR
  pShMem    :ADDRESS;       -- - these vars always contain the same address
  pGObj     :GComm.ptrGObj; -- /
  isInit    :BOOLEAN;


(* this module create message queue to enable bring up message boxes *)
MODULE Anchor;

IMPORT O;

VAR
  hAB   :O.HAB;
  hMsgQ :O.HMQ;

BEGIN
  hAB := O.WinInitialize(0);
  IF hAB = O.NULLHANDLE THEN HALT; END;
  hMsgQ := O.WinCreateMsgQueue(hAB, 0);
FINALLY
  O.WinDestroyMsgQueue(hMsgQ);
  O.WinTerminate(hAB);
END Anchor;

(* This module retrieves process information *)
MODULE ProcessInfo;
IMPORT O, Strings, WholeStr;
EXPORT szPID, hSw;

VAR
  ppib: O.PPIB;
  ptib: O.PTIB;
  hSw : O.HSWITCH;
  szPID: ARRAY [0..63] OF CHAR;
BEGIN
  IF O.DosGetInfoBlocks(ptib,ppib) = O.NO_ERROR THEN
    WholeStr.CardToStr(ppib^.pib_ulpid, szPID);
    IF ((ppib^.pib_ultype = 2) OR (ppib^.pib_ultype = 0)) THEN
      (* VIO OR Full screen session *)
      hSw := O.WinQuerySwitchHandle(0, ppib^.pib_ulpid);
    ELSE 
      hSw := O.NULLHANDLE;
    END;
  END;
END ProcessInfo;


------------------------------------ Semaphores


VAR
  hservReadySem,
  hservMouseSem,
  hclReadySem    :O.HEV;
  hTermQ         :O.HQUEUE;

PROCEDURE Error(sz : ARRAY OF CHAR);
BEGIN
  IO.WrStr("Graph: ");
  IO.WrStr(sz);
  IO.WrLn();
  O.WinMessageBox(O.HWND_DESKTOP, 0, sz, "Graph", 0,
                  O.MB_ERROR + O.MB_OK + O.MB_MOVEABLE);
END Error;


PROCEDURE waitServerReady();
BEGIN
  WHILE (O.DosWaitEventSem(hservReadySem, 300 ) = O.ERROR_TIMEOUT) DO
    O.DosSleep(0);
  END;
END waitServerReady;


PROCEDURE waitServerReadyB();
VAR
  c :CARDINAL;
BEGIN
  WHILE (O.DosWaitEventSem(hservReadySem, 300 ) = O.ERROR_TIMEOUT) DO
    O.DosSleep(0);
  END;
  O.DosResetEventSem (hservReadySem, c);
END waitServerReadyB;


(* this procedure signals to the server to start *)
PROCEDURE releaseClientReady();
BEGIN
  O.DosPostEventSem (hclReadySem);   -- go, go...
END releaseClientReady;

-----------------------------------------------

(* status module vars *)

VAR
  BkColor      :LONGCARD;
  LineStyle    :CARDINAL;
  SFillMask    :LONGINT;
  FillMask     :FillMaskType;
  isStdMask    :BOOLEAN;

(* text proc vars *)
  curStatus   :BOOLEAN;
  wrapMode    :BOOLEAN;
  curPos      :TextCoords;
  r1W, c1W,
  r2W, c2W    :LONGCARD;  -- current window
  txtColor    :LONGCARD;

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

PROCEDURE wcFlip (VAR y :LONGCARD);
BEGIN
  y := Depth-1 - y;
END wcFlip;


------------------------------------------------------------------

(*
    Scheme of a client interaction:
    ------------------------------

   0. capture the shared memory
   1. set input params
   2. release shared memory
   3. release "client ready" semaphore ( signals to server to draw )
   ---
         for input/output tasks only:

   4. wait until the shared memory is captured
   5. capture the shared memory
   6. read result returned
   7. release shared memory

*)

(*////////////////////// parms setting ////////////////////////////*)

PROCEDURE SetLinestyle (Style :CARDINAL);
TYPE  LNSTYLESARR = ARRAY [0..5] OF LONGINT;
CONST LnStylesArr = LNSTYLESARR {O.LINETYPE_SOLID,   O.LINETYPE_DOT,           O.LINETYPE_SHORTDASH,
                                 O.LINETYPE_DASHDOT, O.LINETYPE_DASHDOUBLEDOT, O.LINETYPE_INVISIBLE };
BEGIN
  LineStyle := Style;
  waitServerReadyB();
  WITH pGObj^ DO
    lnStyle := LnStylesArr[LineStyle];
    obj     := GComm.lnType;
  END;
  releaseClientReady();
END SetLinestyle;

PROCEDURE GetLinestyle () :CARDINAL;
BEGIN
  RETURN LineStyle;
END GetLinestyle;

PROCEDURE SetStdFillMask (Mask :LONGINT);
BEGIN
  isStdMask := TRUE;
  SFillMask := Mask;
  waitServerReadyB();
  WITH pGObj^ DO
    ptType := Mask;
    obj    := GComm.setSpat;
  END;
  releaseClientReady();
END SetStdFillMask;

PROCEDURE GetStdFillMask() :LONGINT; BEGIN RETURN SFillMask; END GetStdFillMask;


PROCEDURE SetFillMask ( Mask- :FillMaskType );
VAR
  i :CARDINAL;
BEGIN
  isStdMask := FALSE;
  FillMask  := Mask;
  waitServerReadyB();
  WITH pGObj^ DO
    FOR i:=0 TO 7 DO alPatt[i] := VAL (LONGCARD, Mask[i]); END;
    obj := GComm.setpat;
  END;
  releaseClientReady();
END SetFillMask;

PROCEDURE GetFillMask ( VAR Mask :FillMaskType );
BEGIN
  Mask := FillMask;
END GetFillMask;


PROCEDURE SetBkColor (Color :LONGCARD) :LONGCARD;
VAR
  c :LONGCARD;
BEGIN
  c       := BkColor;
  BkColor := Color;

  waitServerReadyB();
  WITH pGObj^ DO
    BgrColor := BkColor;
    obj   := GComm.setBkC;
  END;
  releaseClientReady();

  RETURN  c;
END SetBkColor;

PROCEDURE GetBkColor() :LONGCARD; BEGIN RETURN BkColor; END GetBkColor;


PROCEDURE SetBkMix ( isOpaque :BOOLEAN );
BEGIN
  waitServerReadyB();
  WITH pGObj^ DO
    IF isOpaque
      THEN mixMode := O.BM_OVERPAINT;
      ELSE mixMode := O.BM_LEAVEALONE;
    END;
    obj := GComm.setBkMix;
  END;
  releaseClientReady();
END SetBkMix;


PROCEDURE SetClipRgn ( x0, y0, x1, y1 :LONGCARD );
BEGIN
  wcFlip( y0 );
  wcFlip( y1 );
  waitServerReadyB();
  WITH pGObj^ DO
    region.xLeft   := myMIN (x0, x1);
    region.yBottom := myMIN (y0, y1);
    region.xRight  := myMAX (x0, x1);
    region.yTop    := myMAX (y0, y1);
    obj   := GComm.clip;
  END;
  releaseClientReady();

END SetClipRgn;

PROCEDURE CancelClipRgn ();
BEGIN
  waitServerReadyB();
  pGObj^.obj := GComm.cancelClip;
  releaseClientReady();
END CancelClipRgn;



(*///////////////////////////// Palette operations ////////////////////////////*)

(*
CONST
  maxNColor = GComm._mxNColor;


PROCEDURE SetColorMode ( mode :BOOLEAN );
BEGIN
  IF (mode = _GCOLOR_INDEX) AND ( NumPColor > maxNColor)
    THEN mode := _GCOLOR_RGB;    -- In this case the RGB mode is only available
  END;
  ColorMode := mode;
  waitServerReadyB();
  WITH pGObj^ DO
    tMode := mode;
    obj   := GComm.rgbmode;
  END;
  releaseClientReady();
END SetColorMode;
*)


PROCEDURE InitStdPalette();
BEGIN
  waitServerReadyB();
    pGObj^.obj := GComm.initPal;
  releaseClientReady();
END InitStdPalette;

PROCEDURE RemapPalette (palItem :CARDINAL; Color :LONGCARD) :LONGCARD;
VAR
  r :LONGINT;
BEGIN
  IF (palItem > NumPColor-1)
    THEN RETURN MAX(LONGCARD);
  END;

  waitServerReadyB();
  WITH pGObj^ DO
    colInd   := palItem;
    col2BSet := VAL (LONGINT, Color);
    obj      := GComm.map1col;
  END;
  releaseClientReady();

  waitServerReady();
  r := pGObj^.prevColor;

  RETURN  VAL(LONGCARD, r);
END RemapPalette;


PROCEDURE RemapAllPalette ( colArray- :ARRAY OF LONGCARD ) :LONGCARD;
VAR
  i, n :CARDINAL;
BEGIN
  n := HIGH(colArray)+1;
  IF ( n > NumPColor ) THEN RETURN MAX(LONGCARD) END;
  n := myMIN ( GComm._mxNColor, n );

  waitServerReadyB();
  WITH pGObj^ DO
    FOR i:=0 TO n-1 DO
      curPalette[i] := VAL (LONGINT, colArray[i]);
    END;
    numc2map := n;
    obj := GComm.mapAllcol;
    --curPalette[255] := VAL ( LONGINT, 0FFFFFFH );
  END;
  releaseClientReady();
  RETURN 0;
END RemapAllPalette;


(*//////////////////////////// text cursor ////////////////////////////*)

PROCEDURE myRect (x0, y0, x1, y1 :LONGCARD; Color :LONGCARD; Fill :BOOLEAN);
BEGIN
  waitServerReadyB();
  WITH pGObj^ DO
    lnCorn0.x := x0; lnCorn0.y := y0;
    lnCorn1.x := x1; lnCorn1.y := y1;
    lnColor := Color;
    lnFill  := Fill;
    obj   := GComm.rect;
  END;
  releaseClientReady();
END myRect;

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
  WITH pGObj^ DO
    mixMode := O.FM_XOR;
    obj     := GComm.setmix;
  END;
  releaseClientReady();

  mySolidRect ( (curPos.col-1) * fontXd, (curPos.row-1) * fontYd,
              (curPos.col) * fontXd -1, (curPos.row) * fontYd -1,
               _BRIGHTWHITE, TRUE );

  waitServerReadyB();
  WITH pGObj^ DO
    mixMode := O.FM_OVERPAINT;
    obj     := GComm.setmix;
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


(*//////////////////////////// graph primitives /////////////////////////////////*)


PROCEDURE ClearScreen ( Area :CARDINAL );
BEGIN
  CursorOff();
  CASE Area OF
    | _GCLEARSCREEN : mySolidRect (0, 0, Width-1, Depth-1, BkColor, TRUE);
    | _GWINDOW      : mySolidRect ( (c1W-1)*fontXd, (r1W-1)*fontYd - fontDescend,
                                   (c2W)*fontXd, (r2W)*fontYd - fontDescend,
                                   BkColor, TRUE );
    | ELSE;
  END;

  DisplayCurs (curStatus);
END ClearScreen;



PROCEDURE Plot ( x, y :LONGCARD; Color :LONGCARD );
BEGIN
  CursorOff();
  waitServerReadyB();
  WITH pGObj^ DO
    spot.x  := x;
    spot.y  := y;
    pColor := Color;
    obj    := GComm.plot;
  END;
  releaseClientReady();
  DisplayCurs (curStatus);
END Plot;


PROCEDURE Point ( x, y :LONGCARD ) :LONGCARD;
BEGIN
  waitServerReadyB();
  WITH pGObj^ DO
    spot.x  := x;
    spot.y  := y;
    obj    := GComm.point;
  END;
  releaseClientReady();     -- go, go...

  waitServerReady();
  RETURN pGObj^.pColor;
END Point;


PROCEDURE Line ( x0, y0, x1, y1 :LONGCARD; Color: LONGCARD);
BEGIN
  CursorOff();
  waitServerReadyB();
  WITH pGObj^ DO
    lnCorn0.x := x0; lnCorn0.y := y0;
    lnCorn1.x := x1; lnCorn1.y := y1;
    lnColor := Color;
    obj   := GComm.line;
  END;
  releaseClientReady();
  DisplayCurs (curStatus);
END Line;

PROCEDURE HLine(x, y, x2 :LONGCARD; FillColor: LONGCARD);
BEGIN
  Line (x, y, x2, y, FillColor);
END HLine;

PROCEDURE Rectangle (x0, y0, x1, y1 :LONGCARD; Color :LONGCARD; Fill :BOOLEAN);
BEGIN
  CursorOff();
  myRect ( x0, y0, x1, y1, Color, Fill );
  DisplayCurs (curStatus);
END Rectangle;


PROCEDURE Circle (x0, y0, r, c :LONGCARD);
(* centre x0,y0; radius r *)
BEGIN
  CursorOff();
  waitServerReadyB();
  WITH pGObj^ DO
    discCenter.x  := x0;
    discCenter.y  := y0;
    discRad       := r;
    discColor     := c;
    discFill      := FALSE;
    obj           := GComm.disc;
  END;
  releaseClientReady();
  DisplayCurs (curStatus);
END Circle;


PROCEDURE Disc (x0, y0, r, c: LONGCARD);
(* filled circle, centre x0,y0; radius r *)
BEGIN
  CursorOff();
  waitServerReadyB();
  WITH pGObj^ DO
    discCenter.x  := x0;
    discCenter.y  := y0;
    discRad       := r;
    discColor     := c;
    discFill      := TRUE;
    obj           := GComm.disc;
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END Disc;


PROCEDURE Ellipse ( x0, y0, a, b, c :LONGCARD; fill :BOOLEAN  );
BEGIN
  CursorOff();
  waitServerReadyB();
  WITH pGObj^ DO
    eCenter.x  := x0;
    eCenter.y  := y0;
    eARad      := a;
    eBRad      := b;
    eColor     := c;
    elFill     := fill;
    obj        := GComm.ellip;  -- go, go...
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END Ellipse;


PROCEDURE Polygon (n :LONGCARD; px-, py- :ARRAY OF LONGCARD; FillColor :LONGCARD; Fill :BOOLEAN);
VAR
  i :LONGCARD;
BEGIN
  IF (n-1 > HIGH(px)) OR (n-1 > HIGH(py)) OR (n > GComm._mxPolyNodes) THEN RETURN; END;

  CursorOff();
  waitServerReadyB();
  WITH pGObj^ DO
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


PROCEDURE FloodFill ( x, y :LONGCARD; Color, Boundary :LONGCARD );
BEGIN
  CursorOff();
  waitServerReadyB();
  WITH pGObj^ DO
    seedp.x    := x;
    seedp.y    := y;
    floodColor := Color;
    boundColor := Boundary;
    obj        := GComm.flood;  -- go, go...
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END FloodFill;



PROCEDURE makeFix ( a :LONGREAL) :LONGINT;
VAR
  r   :LONGREAL;
  i,c :LONGCARD;
BEGIN
  IF (a<0)     THEN a := 0.0;   END;
  IF (a>360.0) THEN a := 360.0; END;
  i := TRUNC(a);
  r := a - LFLOAT(i);
  c := ( TRUNC(r*1000000)*10000H ) DIV 1000000;  -- denom of the fraction c/10000H
  RETURN VAL ( LONGINT, i*10000H + c );
END makeFix;


PROCEDURE Arc_a ( x1, y1, x2, y2 :LONGCARD; startAngle, sweepAngle :LONGREAL; Color :LONGCARD );
BEGIN
  CursorOff();
  waitServerReadyB();
  WITH pGObj^ DO
    arcARad       := ABS (LONGINT(x1-x2)) DIV 2;
    arcBRad       := ABS (LONGINT(y1-y2)) DIV 2;
    arcCenter.x   := myMIN (x1, x2) + arcARad;
    arcCenter.y   := myMIN (y1, y2) + arcBRad;
    arcStartAngle := makeFix (startAngle);
    arcSweetAngle := makeFix (sweepAngle);
    arcColor      := Color;
    obj           := GComm.arc;  -- go, go...
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END Arc_a;


PROCEDURE Arc(x1, y1, a, b, x3, y3, x4, y4: CARDINAL; Color: CARDINAL);
VAR
  vx1, vx2, vy1, vy2 : LONGREAL;
  l1,l2 : LONGREAL;
BEGIN
  vx1 := (VAL(LONGREAL, x3) - VAL(LONGREAL, x1)) * VAL(LONGREAL, b);
  vx2 := (VAL(LONGREAL, x4) - VAL(LONGREAL, x1)) * VAL(LONGREAL, b);
  vy1 := (VAL(LONGREAL, y1) - VAL(LONGREAL, y3)) * VAL(LONGREAL, a);
  vy2 := (VAL(LONGREAL, y1) - VAL(LONGREAL, y4)) * VAL(LONGREAL, a);

  l1 := M.Sqrt(vx1*vx1 + vy1*vy1); -- length (v1)
  IF (l1 < 0.00000000000000001) THEN RETURN; END;
  l1 := M.ACos(vx1 / l1);
  IF (vy1 < 0) THEN l1 := 2*M.M_Pi - l1; END;
  l1 := l1 * 180.0 / M.M_Pi;

  l2 := M.Sqrt(vx2*vx2 + vy2*vy2); -- length (v2)
  IF (l2 < 0.00000000000000001) THEN RETURN; END;
  l2 := M.ACos(vx2 / l2);
  IF (vy2 < 0) THEN l2 := 2*M.M_Pi - l2; END;
  l2 := l2 * 180.0 / M.M_Pi;

  CursorOff();
  waitServerReadyB();

  WITH pGObj^ DO
    arcARad       := a;
    arcBRad       := b;
    arcCenter.x   := x1;
    arcCenter.y   := y1;
    arcStartAngle := makeFix (l1);
    IF (l2-l1+0.1 < 0.0) THEN  arcSweetAngle := makeFix (l2-l1 + 360.1);
    ELSE                       arcSweetAngle := makeFix (l2-l1+0.1);   END;
    arcColor      := Color;
    obj           := GComm.arc;  -- go, go...
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END Arc;


PROCEDURE Pie_a ( x1, y1, x2, y2 :LONGCARD; startAngle, sweepAngle :LONGREAL;
                Color :LONGCARD; Fill :BOOLEAN );
BEGIN
  CursorOff();
  waitServerReadyB();
  WITH pGObj^ DO
    pieARad       := ABS (LONGINT(x1-x2)) DIV 2;
    pieBRad       := ABS (LONGINT(y1-y2)) DIV 2;
    pieCenter.x   := myMIN (x1, x2) + pieARad;
    pieCenter.y   := myMIN (y1, y2) + pieBRad;
    pieStartAngle := makeFix (startAngle);
    pieSweetAngle := makeFix (sweepAngle);
    pieFill       := Fill;
    pieColor      := Color;
    obj           := GComm.pie;  -- go, go...
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END Pie_a;

PROCEDURE Pie(x1, y1, a, b, x3, y3, x4, y4: CARDINAL; Colr: CARDINAL; Fill: BOOLEAN);
VAR
  vx1, vx2, vy1, vy2 : LONGREAL;
  l1,l2 : LONGREAL;
BEGIN
  vx1 := (VAL(LONGREAL, x3) - VAL(LONGREAL, x1)) * VAL(LONGREAL, b);
  vx2 := (VAL(LONGREAL, x4) - VAL(LONGREAL, x1)) * VAL(LONGREAL, b);
  vy1 := (VAL(LONGREAL, y1) - VAL(LONGREAL, y3)) * VAL(LONGREAL, a);
  vy2 := (VAL(LONGREAL, y1) - VAL(LONGREAL, y4)) * VAL(LONGREAL, a);

  l1 := M.Sqrt(vx1*vx1 + vy1*vy1); -- length (v1)
  IF (l1 < 0.00000000000000001) THEN RETURN; END;
  l1 := M.ACos(vx1 / l1);
  IF (vy1 < 0) THEN l1 := 2*M.M_Pi - l1; END;
  l1 := l1 * 180.0 / M.M_Pi;

  l2 := M.Sqrt(vx2*vx2 + vy2*vy2); -- length (v2)
  IF (l2 < 0.00000000000000001) THEN RETURN; END;
  l2 := M.ACos(vx2 / l2);
  IF (vy2 < 0) THEN l2 := 2*M.M_Pi - l2; END;
  l2 := l2 * 180.0 / M.M_Pi;

  CursorOff();
  waitServerReadyB();
  WITH pGObj^ DO
    pieARad       := a;
    pieBRad       := b;
    pieCenter.x   := x1;
    pieCenter.y   := y1;
    pieStartAngle := makeFix (l1);
    IF (l2-l1+0.1 < 0.0) THEN  pieSweetAngle := makeFix (l2-l1 + 360.1);
    ELSE                       pieSweetAngle := makeFix (l2-l1+0.1);   END;
    pieFill       := Fill;
    pieColor      := Colr;
    obj           := GComm.pie;  -- go, go...
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END Pie;


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
VAR  ignore :TextCoords;
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


PROCEDURE OutText ( Text- :ARRAY OF CHAR);

PROCEDURE outTextPiece ( Text- :ARRAY OF CHAR);
BEGIN
  waitServerReadyB();
  WITH pGObj^ DO
    txPos.x  := (curPos.col-1) * fontXd;
    txPos.y  := (curPos.row-1) * fontYd - fontDescend;
    txFontXd := fontXd;
    txFontYd := fontYd;
    txfontDescend := fontDescend;
    txColor  := txtColor;
    txBgr    := BkColor;
    Strings.Assign ( Text, txText );
    obj        := GComm.text;  -- go, go...
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


PROCEDURE RawOutText (x1, y1, Color :LONGCARD; Text- :ARRAY OF CHAR );
BEGIN
  CursorOff();
  waitServerReadyB();
  WITH pGObj^ DO
    rtxPos.x   := x1;
    rtxPos.y   := y1;
    rtxColor   := Color;
    Strings.Assign ( Text, rtxText );
    obj        := GComm.rtext;  -- go, go...
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END RawOutText;

PROCEDURE SetGraphWindowTitle (text :ARRAY OF CHAR);
BEGIN
  (* Sory, not implemented. *)
END SetGraphWindowTitle;

PROCEDURE GraphMode(); --*FSA
BEGIN
  waitServerReadyB();
  WITH pGObj^ DO
    obj    := GComm.setfocus;
  END;
  releaseClientReady();
  waitServerReady();
  GComm.mouseGraph := TRUE;
END GraphMode;

PROCEDURE TextMode();
BEGIN
  IF hSw <> O.NULLHANDLE THEN O.WinSwitchToProgram(hSw) END;
  GComm.mouseGraph := FALSE;
END TextMode;

PROCEDURE SetActivePage (Page :CARDINAL) :CARDINAL; --*FSA
BEGIN
  RETURN 0;
END SetActivePage;

PROCEDURE SetVisualPage (Page :CARDINAL) :CARDINAL; --*FSA
BEGIN
  RETURN 0;
END SetVisualPage;


PROCEDURE SetVideoMode (Mode :CARDINAL): BOOLEAN; --*FSA
TYPE
  szArrayT = ARRAY [0.._MRES256COLOR-_MRES4COLOR] OF CARDINAL;
CONST
  cxArray = szArrayT{320,320,640,13,0,0,0,0,720,320,640,640,640,640,640,320};
  cyArray = szArrayT{200,200,200,13,0,0,0,0,348,200,200,350,350,480,480,200};
VAR
  cx, cy :CARDINAL;
BEGIN
  IF (Mode <= _TEXTC80) OR (Mode = _TEXTMONO) OR (Mode > _MRES256COLOR) THEN
    TextMode();
    RETURN TRUE
  END;

  cx := cxArray[Mode - _MRES4COLOR];
  cy := cyArray[Mode - _MRES4COLOR];

  RETURN Init (50,50, cx, cy);
END SetVideoMode;


PROCEDURE GetVideoConfig (VAR V :VideoConfig);
BEGIN
  FILL (ADR(V), 0, SIZE(VideoConfig));
  IF ~isInit THEN RETURN END;

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

(*/////////////////////////// Bitmap operations //////////////////////////////*)


PROCEDURE GetImage  (x1, y1, x2, y2 :LONGCARD; VAR handle :HBITMAP);
BEGIN
  wcFlip (y1);
  wcFlip (y2);

  Adjust ( x1, 0, Width-1 );
  Adjust ( x2, 0, Width-1 );
  Adjust ( y1, 0, Depth-1 );
  Adjust ( y2, 0, Depth-1 );

  waitServerReadyB();
  WITH pGObj^ DO
    imgArea [0].x := myMIN (x1,x2);
    imgArea [0].y := myMIN (y1,y2);
    imgArea [1].x := myMAX (x1,x2)+1;
    imgArea [1].y := myMAX (y1,y2)+1;
    obj    := GComm.getimg;
  END;
  releaseClientReady();

  waitServerReady();
  handle := pGObj^.rimgHandle;

END GetImage;

PROCEDURE PutImage  (x, y :LONGCARD; hbm :HBITMAP; Action :LONGCARD);
TYPE  ACTARR = ARRAY [0..4] OF LONGCARD;
CONST ActArr = ACTARR { O.ROP_SRCCOPY,  O.ROP_NOTSRCCOPY, O.ROP_SRCAND,
                        O.ROP_SRCPAINT, O.ROP_SRCINVERT};
BEGIN
(*
   actually coords of left upper corner are passed as (x,y), whereas GPI draws a bitmap
   at left lower corner - recalculate it in the server
*)
  wcFlip (y);
  CursorOff();
  waitServerReadyB();
  WITH pGObj^ DO
    imgLCorn.x := x;
    imgLCorn.y := y;
    imgHandle  := hbm;
    imgMode    := ActArr[Action];
    obj        := GComm.putimg;
  END;
  releaseClientReady();

  DisplayCurs (curStatus);
END PutImage;


PROCEDURE DelImage ( hbm :HBITMAP );
BEGIN
  waitServerReadyB();
  WITH pGObj^ DO
    delImgHandle := hbm;
    obj          := GComm.delimg;
  END;
  releaseClientReady();
END DelImage;

PROCEDURE ImageSize(x1, y1, x2, y2: CARDINAL) :CARDINAL;
BEGIN
  RETURN 0;
END ImageSize;

(*----------------------------------------------------------------------------------*)

VAR
  mouTid :O.TID;

  isMemAlc   :LONGCARD;
  rc         :LONGCARD;
  SData      :O.STARTDATA;
  GSpid      :O.PID;
  GSessID    :LONGCARD;
  achObjBuf  :ARRAY [0..255] OF CHAR;

  fInMouse   : BOOLEAN;

PROCEDURE [O.EXPENTRY] mouseThread (param :CARDINAL);
(* FSA *)
VAR
  c : CARDINAL;
BEGIN
  LOOP
    WHILE (O.DosWaitEventSem(hservMouseSem, 300 ) = O.ERROR_TIMEOUT) DO
      O.DosSleep(0);
    END;
    fInMouse := TRUE;
    O.DosResetEventSem (hservMouseSem, c);
    IF NOT isInit THEN O.DosExit(O.EXIT_THREAD,0); END;
    IF (GComm.mouseProc # NIL) THEN
      GComm.mouseProc(pGObj^.mouseMsg, pGObj^.mouseMp1, pGObj^.mouseMp2);
    END;
  END;
END mouseThread;


PROCEDURE Resize (xLeft, yTop, xd, yd :LONGCARD ) :BOOLEAN;
BEGIN
  Width                := xd;
  Depth                := yd;
  GComm.mouseWinHeight := yd;

  CursorOff();
  waitServerReadyB();
  pGObj^.xd      := xd;
  pGObj^.yd      := yd;
  pGObj^.xLeft   := xLeft;
  pGObj^.yBottom := yTop;
  pGObj^.obj     := GComm.resize;
  releaseClientReady();
  DisplayCurs (curStatus);

  ClearScreen ( _GCLEARSCREEN );

  Rows    := (Depth - fontDescend) DIV fontYd;
  Columns :=  Width DIV fontXd;

  SetTextWindow ( 1, 1, Rows, Columns);

  RETURN TRUE;
END Resize;



PROCEDURE Init (xLeft, yTop, xd, yd :LONGCARD ) :BOOLEAN;
VAR
  SearchResult :ARRAY [0..255] OF CHAR;
  szName : ARRAY [0..63] OF CHAR;
  szTermQName : ARRAY [0..63] OF CHAR;
CONST
  gExec  ["C"] = "TSGServ2.exe";
  PTitle ["C"] = "TS Graph Library";

PROCEDURE makeName(prefix: ARRAY OF CHAR);
BEGIN
  Strings.Concat(prefix,szPID,szName);
END makeName;

BEGIN
  IF (isInit) THEN RETURN Resize(xLeft, yTop, xd, yd); END;

  isInit := TRUE;

  Width                := xd;
  Depth                := yd;
  GComm.mouseWinHeight := yd;

  makeName(GComm.clReadySemName);
  IF O.DosCreateEventSem ( szName, hclReadySem, O.DC_SEM_SHARED, FALSE) # O.NO_ERROR THEN
    Error("Event semaphore not created");
    RETURN FALSE
  END;
  makeName(GComm.servReadySemName);
  IF O.DosCreateEventSem ( szName, hservReadySem, O.DC_SEM_SHARED, FALSE) # O.NO_ERROR THEN
    Error("Event semaphore not created");
    RETURN FALSE
  END;
  makeName(GComm.mouseActionSemName);
  IF O.DosCreateEventSem ( szName, hservMouseSem, O.DC_SEM_SHARED, FALSE) # O.NO_ERROR THEN
    Error("Event semaphore not created");
    RETURN FALSE
  END;

  (* allocate shared memory *)
  makeName(GComm.shMemName);
  isMemAlc := O.DosAllocSharedMem ( pShMem,                        -- to object pointer
                                    szName,                        -- object name
                                    SIZE (GComm.tGObj),            -- size of object
                                    O.PAG_COMMIT + O.PAG_WRITE + O.PAG_READ );
  IF isMemAlc # O.NO_ERROR THEN
    Error("Graph server already exists");
    RETURN FALSE;
  END;

  makeName(GComm.wmcharPipeName);
  xtsEvQue.RestartPipe(szName);

  pGObj := GComm.ptrGObj (pShMem);
  pGObj^.obj     := GComm.init;
  pGObj^.xd      := xd;
  pGObj^.yd      := yd;
  pGObj^.xLeft   := xLeft;
  pGObj^.yBottom := yTop;


  (* create session *)
  hTermQ := 0;
  makeName("\queues\TSGRAPHTERMQ_");
  COPY(szName,szTermQName);
  IF (O.DosCreateQueue(hTermQ,O.QUE_FIFO,szTermQName) <> O.NO_ERROR) THEN
    Error("Queue not created");
    RETURN FALSE;
  END;
  FILL ( ADR (SData), 0, SIZE (SData) );
  SData.Length   := SIZE (O.STARTDATA);
  SData.Related  := O.SSF_RELATED_CHILD;       -- start an child session
  SData.FgBg     := O.SSF_FGBG_BACK;           -- start session in background
  SData.TraceOpt := O.SSF_TRACEOPT_NONE;       -- No trace


  rc := O.DosSearchPath ( O.SEARCH_ENVIRONMENT + O.SEARCH_IGNORENETERRS
                          (*+ O.SEARCH_CUR_DIRECTORY,   -- temporary*),
                          "PATH",
                          gExec,
                          SearchResult,
                          SIZE (SearchResult) );

  IF (rc # O.NO_ERROR) THEN
    Error("Graph server exec file not found");
    RETURN FALSE;
  END;


  SData.PgmName   := ADR ( SearchResult );
  SData.PgmInputs := ADR ( szPID );
  SData.PgmTitle  := ADR ( PTitle );
  SData.TermQ     := ADR ( szTermQName );     -- Termination queue name
  SData.Environment := NIL;                   -- No environment string
  SData.InheritOpt := O.SSF_INHERTOPT_SHELL;  -- Inherit shell's env
  SData.SessionType := O.SSF_TYPE_PM;         -- PM session
  SData.IconFile := NIL;                      -- No icon association
  SData.PgmHandle := 0;

  SData.PgmControl    := O.SSF_CONTROL_VISIBLE;
  SData.ObjectBuffer  := ADR (achObjBuf);     -- Contains info if DosStartSession fails
  SData.ObjectBuffLen := SIZE(achObjBuf);


  rc := O.DosStartSession(SData, GSessID, GSpid);  -- Start the Graph session

  IF (rc # O.NO_ERROR) THEN
    Error("Graph server not run");
    RETURN FALSE;
  END;

  waitServerReady();
  IF (pGObj^.obj # GComm.ok ) THEN RETURN FALSE END;

  WITH pGObj^ DO
    NumPColor   := numColors;
    fontYd      := fontMetricYd;
    fontDescend := fontMetricDescender;
    fontXd      := fontMetricXd;
  END;

  rc := SetBkColor ( _clrBLACK );
  SetBkMix ( _OPAQUE );
  ClearScreen ( _GCLEARSCREEN );

  Rows    := (Depth - fontDescend) DIV fontYd;
  Columns :=  Width DIV fontXd;

  SetTextWindow ( 1, 1, Rows, Columns );

  fInMouse := FALSE;
  O.DosCreateThread (mouTid, mouseThread, 0, O.CREATE_READY, 3fffh);

  GraphMode();

  RETURN TRUE;
END Init;


PROCEDURE Exit;
VAR
  l : LONGCARD;
BEGIN
  GComm.mouseGraph := FALSE;
  IF isInit THEN
    isInit := FALSE;
    O.DosPostEventSem(hservMouseSem);
    O.DosStopSession ( O.STOP_SESSION_SPECIFIED, GSessID);
    REPEAT
      O.DosSleep(1);
      O.DosQueryQueue(hTermQ,l);
    UNTIL (l#0);
    O.DosCloseQueue(hTermQ);
    xtsEvQue.RestartPipe("");    -- to make it ready to new connection..
    IF (hclReadySem    # 0)    THEN O.DosCloseEventSem(hclReadySem);   hclReadySem    := 0; END;
    IF (hservReadySem  # 0)    THEN O.DosCloseEventSem(hservReadySem); hservReadySem  := 0; END;
    IF (hservMouseSem  # 0)    THEN O.DosCloseEventSem(hservMouseSem); hservMouseSem  := 0; END;
    IF (isMemAlc = O.NO_ERROR) THEN O.DosFreeMem(pShMem);              isMemAlc       := O.NO_ERROR+1; END;
  END;
END Exit;



PROCEDURE Update();
BEGIN
  (* Used in Win-32*)
END Update;

PROCEDURE UpdateRect ( x1, y1, x2, y2 :LONGCARD );
BEGIN
  (* Used in Win-32*)
END UpdateRect;

PROCEDURE SetUpdateMode (auto :BOOLEAN);
BEGIN
  (* Used in Win-32*)
END SetUpdateMode;


BEGIN -----------------------------------------------------------------------------------
  isInit        := FALSE;
  isMemAlc      := O.NO_ERROR+1;
  hclReadySem   := 0;
  hservReadySem := 0;
  hservMouseSem := 0;

  PATSYM_DENSE1         := O.PATSYM_DENSE1;
  PATSYM_DENSE2         := O.PATSYM_DENSE2;
  PATSYM_VERT           := O.PATSYM_VERT;
  PATSYM_HORIZ          := O.PATSYM_HORIZ;
  PATSYM_DIAG1          := O.PATSYM_DIAG1;
  PATSYM_DIAG2          := O.PATSYM_DIAG2;
  PATSYM_SOLID          := O.PATSYM_SOLID;


  wrapMode     := _GWRAPOFF;
  txtColor     := _BRIGHTWHITE;
  curStatus    := FALSE;
  curDisplayed := FALSE;

  LineStyle    := LNSTYLE_SOLID;
  SFillMask    := PATSYM_SOLID;
  FILL ( ADR(FillMask), 0, SIZE(FillMask) );
  isStdMask    := TRUE;

  GComm.mouseProc    := NIL;
  GComm.mouseGraph   := FALSE;

FINALLY
  IF isInit THEN Exit(); END;

  IF isMemAlc = O.NO_ERROR
    THEN O.DosFreeMem ( pShMem );
  END;

END Graph.

