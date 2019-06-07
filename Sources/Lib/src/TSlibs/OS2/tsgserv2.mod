(* Copyright (C) 1999-2000 Excelsior *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

MODULE TSGServ2;

IMPORT O := OS2, SYSTEM,
       Strings,
       FormStr,
       ProgEnv,
       GComm := xtsGComm;

FROM SYSTEM IMPORT ADR, ADDRESS, FILL, CARD8;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;


CONST
  RES_MAIN = 17;
  MY_PATT  = 18;

  WM_START    = O.WM_USER +1000;
  WM_UPDATE   = O.WM_USER +1001;
  WM_setfocus = O.WM_USER +1002;
  WM_resize   = O.WM_USER +1003;

CONST
  MAINCLASSNAME = "GraphWnd";

TYPE
  STRING = ARRAY [0..79] OF CHAR;

VAR
  ss      :STRING;                           -- to test only !!!!!!!!!!!!!!!!
  rc      :LONGCARD;

  hMainFrame,
  hMainClient :O.HWND;
  hpsw        :O.HPS;
  hChPipe     : O.HPIPE;
  perstHps    :O.HPS;
  tid         :O.TID;


  pShMem  :ADDRESS;                -- - these vars always contain the same address
  pGObj   :POINTER TO GComm.tGObj; -- /

  xd, yd, xLeft, yBottom  :LONGCARD;  -- window parms

(*
  this module must be the first in the module order because
  it makes other modules be able bring up a message box
*)
MODULE Anchor;
IMPORT O, FormStr;
EXPORT hAB, Error;

PROCEDURE Error(s-: ARRAY OF CHAR; rc: O.APIRET);
VAR
  msg: ARRAY [0..511] OF CHAR;
BEGIN
  FormStr.print(msg,"%s. Error code was %d",s,rc);
  O.WinMessageBox(O.HWND_DESKTOP, 0,
                  msg, "Graph", 0,
                  O.MB_ERROR + O.MB_OK + O.MB_MOVEABLE);
END Error;

VAR
  hAB     :O.HAB;
  hMsgQ   :O.HMQ;
BEGIN
  hAB := O.WinInitialize ( 0 );
  IF hAB = O.NULLHANDLE THEN HALT; END;

  hMsgQ := O.WinCreateMsgQueue(hAB,32);

FINALLY
  O.WinDestroyMsgQueue(hMsgQ);
  O.WinTerminate(hAB);
END Anchor;


----------------------- Semaphores

VAR
  hScrMutex     :O.HMTX;
  hservReadySem,
  hclReadySem,
  hservMouseSem :O.HEV;


PROCEDURE waitClientReady();
VAR
  c :LONGCARD;
BEGIN
  WHILE (O.DosWaitEventSem(hclReadySem, 300 ) = O.ERROR_TIMEOUT) DO
    O.DosSleep(0);
  END;
  O.DosResetEventSem (hclReadySem, c);
END waitClientReady;


PROCEDURE releaseServerReady();
BEGIN
  O.DosPostEventSem (hservReadySem);
END releaseServerReady;



(*-------------------------- Bitmap support ----------------------------*)

MODULE RGB;

IMPORT O;
IMPORT ADR, ADDRESS;
IMPORT Error;

EXPORT QUALIFIED numColors, planes, bitcnt, yRasterPels,
                 InitPal, Map1Col, MapAllCol;

VAR
  numColors,
  planes,
  bitcnt,
  yRasterPels :LONGCARD;

(* obtain screen capabilities *)
PROCEDURE GetSCaps();
VAR
  alArray :ARRAY [0..O.CAPS_COLOR_BITCOUNT] OF O.LONG;
  hdc     :O.HDC;
  hps     :O.HPS;

BEGIN
  hps := O.WinGetScreenPS( O.HWND_DESKTOP );
  hdc := O.GpiQueryDevice( hps );
  O.WinReleasePS( hps );

  O.DevQueryCaps( hdc,
                  O.CAPS_FAMILY,           -- number of first item
                  O.CAPS_COLOR_BITCOUNT+1, -- count of items
                  alArray);

  planes      := alArray [O.CAPS_COLOR_PLANES];
  bitcnt      := alArray [O.CAPS_COLOR_BITCOUNT];
  numColors   := alArray [O.CAPS_COLORS];
  yRasterPels := alArray [O.CAPS_HEIGHT];
END GetSCaps;


PROCEDURE ["C"] Map1Col (hps :O.HPS; rgb :LONGINT; colnum :LONGCARD ) :LONGINT;
VAR
  prevA :ARRAY[0..0] OF LONGINT;
  prev  :LONGINT;
BEGIN
  O.GpiQueryLogColorTable( hps, 0, colnum, 1, prevA );
  prev := prevA[0];
  prevA[0] := rgb;

  O.GpiCreateLogColorTable( hps, 0, O.LCOLF_CONSECRGB, colnum, 1, prevA);
  RETURN prev;
END Map1Col;


PROCEDURE MapAllCol ( hps :O.HPS;  rgb- :ARRAY OF LONGINT; nc2Map :LONGCARD );
BEGIN
  O.GpiCreateLogColorTable( hps, 0, O.LCOLF_CONSECRGB, 0, nc2Map, rgb );
END MapAllCol;


PROCEDURE InitPal ( hps :O.HPS );
TYPE
  colors = ARRAY [0..15] OF LONGINT;
CONST
  ColArr = colors { 0000000H, 00000B0H, 000B000H, 0008080H,
                    0B00000H, 0800080H, 0B06000H, 0C0C0C0H,
                    0808080H, 00000FFH, 000FF00H, 000FFFFH,
                    0FF0000H, 0FF00FFH, 0FFFF00H, 0FFFFFFH };
BEGIN
  MapAllCol ( hps, ColArr, 16 );
END InitPal;


BEGIN
  GetSCaps();
END RGB;


MODULE Translate;

IMPORT O, SYSTEM,
       FILL, ADR,
       hAB, Error, ss, FormStr;

EXPORT QUALIFIED Flip, Initial;

VAR
  matr :O.MATRIXLF;

PROCEDURE Flip ( hps :O.HPS );
VAR
  rc :BOOLEAN;
BEGIN
  rc := O.GpiSetModelTransformMatrix( hps, 9, matr, O.TRANSFORM_REPLACE);
  IF NOT rc THEN
    Error("Translate failure",O.WinGetLastError(hAB));
  END;
END Flip;


PROCEDURE Initial( depth :LONGCARD );
BEGIN
  FILL (ADR (matr), 0, SIZE (matr) );
  matr.fxM11 := 10000H;
  matr.fxM22 := O.FIXED (0FFFF0000H);
  matr.lM31  := 0;
  matr.lM32  := depth - 1;
  matr.lM33  := 1;

END Initial;

END Translate;


MODULE BitMap;

IMPORT O,
       RGB,
       FormStr, ss,
       ADDRESS, ADR, FILL,
       hAB, Error,
       xd, yd;
EXPORT QUALIFIED hpsM, hbm,
                 Create, Draw, Resize;

VAR
  hbm     :O.HBITMAP;
  hpsM    :O.HPS;    -- pres space associates with a memory device context (exported)
  hdcM    :O.HDC;

  aPt     :ARRAY[0..3] OF O.POINTL;


(* creatin' of a bitmap and a pres space *)
PROCEDURE Create ();
VAR
  bmih :O.BITMAPINFOHEADER2;

VAR
  dop   :O.DEVOPENSTRUC;
  sizl  :O.SIZEL;
  rc    :LONGCARD;
CONST
  drvName ["C"] = "Display";

BEGIN
  aPt[0].x := 0;  aPt[0].y := 0;
  aPt[1].x := xd; aPt[1].y := yd;
  aPt[2].x := 0;  aPt[2].y := 0;
  aPt[3].x := xd; aPt[3].y := yd;

  -- use same page size as device --
  sizl.cx := 0;
  sizl.cy := 0;

  FILL ( ADR (dop), 0, SIZE (dop) );
  dop.pszLogAddress := NIL;
  dop.pszDriverName := ADR ( drvName );

  -- create memory device context --
  hdcM := O.DevOpenDC(hAB, O.OD_MEMORY, "*", 4, ADR(dop), O.NULLHANDLE);

  -- create a presentation space associated with the context --
  hpsM := O.GpiCreatePS (hAB, hdcM, sizl, O.GPIA_ASSOC + O.PU_PELS);


  IF (hpsM = O.GPI_ERROR) THEN
    Error("GpiCreatePS (mem) failure",O.WinGetLastError(hAB));
  END;

  FILL ( ADR(bmih),0,  SIZE (O.BITMAPINFOHEADER2));
  bmih.cbFix       := SIZE ( O.BITMAPINFOHEADER2 );
  bmih.cx          := xd;
  bmih.cy          := yd;
  bmih.cPlanes     := RGB.planes;
  bmih.cBitCount   := RGB.bitcnt;

  hbm := O.GpiCreateBitmap (hpsM, bmih, 0, NIL (*leskoff*), NIL );

  IF (hbm = O.GPI_ERROR) THEN
    Error("Screen bitmap was not created",O.WinGetLastError(hAB));
    HALT;
  END;

  rc := O.GpiSetBitmap(hpsM, hbm);
  IF (rc = O.HBM_ERROR) THEN
    Error("SetBitmap failure (create)",O.WinGetLastError(hAB));
  END;

END Create;


PROCEDURE Resize ();
VAR
  bmih :O.BITMAPINFOHEADER2;
  rc   :LONGCARD;
BEGIN
  aPt[1].x := xd; aPt[1].y := yd;
  aPt[3].x := xd; aPt[3].y := yd;

  FILL ( ADR(bmih),0,  SIZE (O.BITMAPINFOHEADER2));
  bmih.cbFix       := SIZE ( O.BITMAPINFOHEADER2 );
  bmih.cx          := xd;
  bmih.cy          := yd;
  bmih.cPlanes     := RGB.planes;
  bmih.cBitCount   := RGB.bitcnt;

  hbm := O.GpiCreateBitmap (hpsM, bmih, 0, NIL (*leskoff*), NIL );

  IF (hbm = O.GPI_ERROR) THEN
    Error("Screen bitmap was not created",O.WinGetLastError(hAB));
    HALT;
  END;

  rc := O.GpiSetBitmap(hpsM, hbm);
  IF (rc = O.HBM_ERROR) THEN
    Error("SetBitmap failure (create)",O.WinGetLastError(hAB));
    HALT;
  ELSIF rc # 0 THEN
    O.GpiDeleteBitmap(O.HBITMAP(rc));
  END;
END Resize;


PROCEDURE Draw( hps :O.HPS );
VAR
  rci :LONGINT;
BEGIN
  rci := O.GpiBitBlt(hps, hpsM, 4, aPt, O.ROP_SRCCOPY, O.BBO_IGNORE);
END Draw;

BEGIN
FINALLY
  O.GpiDeleteBitmap ( hbm );
  O.DevCloseDC(hdcM);
  O.GpiDestroyPS( hpsM );
END BitMap;



MODULE copyBmp;

IMPORT O, RGB,
       hAB, Error,
       ADDRESS, ADR, FILL,
       ALLOCATE, DEALLOCATE,
       xd, yd,
       FormStr, ss;

EXPORT QUALIFIED Paste, Copy, hpsMem;

VAR
  hpsMem  :O.HPS;
  hdcMem  :O.HDC;

  bmih   :O.BITMAPINFOHEADER2;
  pbmi   :O.PBITMAPINFO2;
  szBmi  :LONGCARD;

(* creatin' of a pres space *)

PROCEDURE Init();
VAR
  dop   :O.DEVOPENSTRUC;
  sizl  :O.SIZEL;
  bmiadr :ADDRESS;              -- POINTER TO O.BITMAPINFO2

CONST
  drvName ["C"] = "Display";

BEGIN
  -- use same page size as device --
  sizl.cx := 0;
  sizl.cy := 0;

  FILL ( ADR (dop), 0, SIZE (dop) );
  dop.pszLogAddress := NIL;
  dop.pszDriverName := ADR ( drvName );

  -- create memory device context --
  hdcMem := O.DevOpenDC(hAB, O.OD_MEMORY, "*", 4, ADR(dop), O.NULLHANDLE);

  -- create a presentation space associated with the context --
  hpsMem := O.GpiCreatePS (hAB, hdcMem, sizl, O.GPIA_ASSOC + O.PU_PELS);
  --O.GpiCreateLogColorTable( hpsMem, 0, O.LCOLF_RGB, 0, 0, LESKOFF);  -- RGB mode on

  FILL ( ADR(bmih),0,  SIZE (O.BITMAPINFOHEADER2));
  bmih.cbFix       := SIZE ( O.BITMAPINFOHEADER2 );
  bmih.cPlanes     := RGB.planes;
  bmih.cBitCount   := RGB.bitcnt;

  szBmi := SIZE (O.BITMAPINFO2) + RGB.numColors * SIZE (O.RGB2);
  ALLOCATE ( bmiadr, szBmi );
  pbmi := O.PBITMAPINFO2 (bmiadr);
  FILL( pbmi, 0, SIZE (O.BITMAPINFO2) );
  pbmi^.cbFix     := SIZE(O.BITMAPINFO2)-SIZE(O.RGB2);
  pbmi^.cPlanes   := RGB.planes;
  pbmi^.cBitCount := RGB.bitcnt;
END Init;


PROCEDURE setSz ( xsz, ysz :LONGCARD );
BEGIN
  pbmi^.cx := xsz;
  pbmi^.cy := ysz;
  bmih.cx  := xsz;
  bmih.cy  := ysz;
END setSz;

(*
  LESKOFF :ARRAY [0..0] OF LONGINT;
BEGIN
  IF (m)
    THEN O.GpiCreateLogColorTable( hps, 0, O.LCOLF_RGB, 0, 0, LESKOFF);  -- RGB mode on
    ELSE O.GpiCreateLogColorTable( hps, O.LCOL_RESET, O.LCOLF_INDRGB, 0, 0, LESKOFF);
*)

PROCEDURE Copy ( hps :O.HPS; area- :ARRAY OF O.POINTL ) :O.HBITMAP;
VAR
  hb      :O.HBITMAP;
  myArea  :ARRAY [0..3] OF O.POINTL;
  sz      :O.SIZEL;
BEGIN
  myArea[1].x := area[1].x-area[0].x;
  myArea[1].y := area[1].y-area[0].y;

  setSz ( myArea[1].x+1, myArea[1].y+1 );
  sz.cx := myArea[1].x+1;
  sz.cy := myArea[1].y+1;

  hb := O.GpiCreateBitmap ( hpsMem, bmih, 0, NIL (*leskoff*), pbmi^ );
  O.GpiSetBitmapDimension ( hb, sz );

  O.GpiSetBitmap( hpsMem, hb );
  myArea[0].x := 0;
  myArea[0].y := 0;
  myArea[2]   := area[0];
  myArea[3]   := area[1];

  O.GpiBitBlt ( hpsMem, hps, 4, myArea, O.ROP_SRCCOPY, O.BBO_IGNORE);

  RETURN hb;
END Copy;


PROCEDURE Paste ( hps :O.HPS; corn :O.POINTL; hb :O.HBITMAP; mode :LONGCARD );
VAR
  myArea  :ARRAY [0..2] OF O.POINTL;
  sz :O.SIZEL;
BEGIN
  O.GpiQueryBitmapDimension ( hb, sz );

  DEC (corn.y, sz.cy-1); -- because left upper coord's been passed
  myArea[0] := corn;
  myArea[1].x := myArea[0].x + sz.cx-1;
  myArea[1].y := myArea[0].y + sz.cy-1;
  myArea[2].x := 0;
  myArea[2].y := 0;

  O.GpiSetBitmap ( hpsMem, hb );

  O.GpiBitBlt ( hps, hpsMem, 3, myArea, mode, O.BBO_IGNORE);

END Paste;


BEGIN
  Init();
FINALLY
  DEALLOCATE ( pbmi, szBmi );
  O.DevCloseDC(hdcMem);
  O.GpiDestroyPS( hpsMem );
END copyBmp;


MODULE pattBmp;

IMPORT O, BitMap, perstHps ;

FROM SYSTEM IMPORT ADDRESS, ADR, FILL;

EXPORT QUALIFIED Set, unSet, CreateB;

VAR
  hbmm        :O.HBITMAP;
  bmp2        :O.BITMAPINFOHEADER2;
  bmp2C       :O.BITMAPINFO2;

VAR
  lcidCustom :LONGINT;  -- Bit map tag

VAR
  setYet :BOOLEAN;

PROCEDURE unSet ( hps :O.HPS );
BEGIN
  IF (setYet)
    THEN O.GpiSetPatternSet (hps, O.LCID_DEFAULT );
         O.GpiDeleteSetId ( hps, lcidCustom );
  END;
END unSet;

PROCEDURE Set ( hps :O.HPS );
BEGIN
  O.GpiSetBitmapId( hps, hbmm, lcidCustom );  -- assign ID to bitmap
  O.GpiSetPatternSet (hps, lcidCustom );      -- create a singlton pattern set with ID
  setYet := TRUE;
END Set;

PROCEDURE CreateB ( hps :O.HPS; pbuff :ADDRESS );
BEGIN
  IF (setYet) THEN O.GpiDeleteBitmap ( hbmm ) END;

  hbmm := O.GpiCreateBitmap ( hps, bmp2, O.CBM_INIT, pbuff, bmp2C );
END CreateB;


BEGIN
  FILL (ADR(bmp2), 0, SIZE(bmp2));
  bmp2.cbFix     := SIZE(O.BITMAPINFOHEADER2);
  bmp2.cx        := 8;
  bmp2.cy        := 8;
  bmp2.cPlanes   := 1;
  bmp2.cBitCount := 1;

  FILL (ADR(bmp2C), 0, SIZE(bmp2C));
  WITH bmp2C DO
    cbFix     := bmp2.cbFix;
    cx        := 8;
    cy        := 8;
    cPlanes   := 1;
    cBitCount := 1;
  END;

  lcidCustom := 1;
  setYet     := FALSE;

END pattBmp;




(*-----------------------------------------  Draw object ----------------------------------------*)

(*
    The ServerDraw procedure actully works in the second thread when a job is,
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
  hrgn :O.HRGN;

PROCEDURE doResize();
BEGIN
  xd       := pGObj^.xd;
  yd       := pGObj^.yd;
  xLeft    := pGObj^.xLeft;
  yBottom  := RGB.yRasterPels-1 - pGObj^.yBottom - yd;  -- Win32 l.u. corner

  O.GpiSetClipRegion (perstHps, O.NULLHANDLE, NIL );
  O.GpiDestroyRegion (perstHps, hrgn );
  O.GpiSetClipRegion (BitMap.hpsM, O.NULLHANDLE, NIL );
  O.GpiDestroyRegion (BitMap.hpsM, hrgn );
  hrgn := 0;

  BitMap.Resize();

  Translate.Initial (yd);
  Translate.Flip ( perstHps );
  Translate.Flip ( BitMap.hpsM );
  Translate.Flip ( copyBmp.hpsMem );

  O.WinPostMsg(hMainClient, WM_resize, NIL, NIL);

END doResize;


PROCEDURE DrawObj (hps :O.HPS );
VAR
  arcp     :O.ARCPARAMS;

  ozzy     :ARRAY [0..0] OF O.RECTL;
  polar    :ARRAY [0..0] OF O.POLYGON;

  zztop    :ARRAY [0..0] OF CHAR;
  i        :CARDINAL;
  cp,
  bkr      :O.POINTL;


BEGIN
  arcp.lR := 0;
  arcp.lS := 0;

  WITH pGObj^ DO
  CASE obj OF
    | GComm.clip  :  ozzy[0] := region;
                     O.GpiSetClipRegion(hps, O.NULLHANDLE, NIL );
                     O.GpiDestroyRegion (hps, hrgn );
                     hrgn := O.GpiCreateRegion (hps, 1, ozzy);
                     O.GpiSetClipRegion(hps, hrgn, NIL );

    | GComm.cancelClip : O.GpiSetClipRegion(hps, O.NULLHANDLE, NIL );
                         O.GpiDestroyRegion (hps, hrgn );

    | GComm.initPal  : RGB.InitPal ( hps );
                       RGB.InitPal ( copyBmp.hpsMem );

    | GComm.mapAllcol: RGB.MapAllCol ( hps, curPalette, numc2map );
                       RGB.MapAllCol ( copyBmp.hpsMem, curPalette, numc2map );

    | GComm.map1col:   prevColor := RGB.Map1Col ( hps, col2BSet, colInd );

    | GComm.setmix:    O.GpiSetMix ( hps, mixMode );

    | GComm.setBkMix: O.GpiSetBackMix ( hps, mixMode );

    | GComm.setBkC:   O.GpiSetBackColor ( hps, BgrColor );

    | GComm.lnType:   O.GpiSetLineType ( hps, lnStyle );

    | GComm.setSpat:  O.GpiSetPatternSet ( hps, O.LCID_DEFAULT );
                      O.GpiSetPattern    ( hps, ptType );

    | GComm.setpat:  pattBmp.unSet ( perstHps );
                     pattBmp.unSet ( hps );

                     pattBmp.CreateB ( hps, ADR(alPatt) );

                     pattBmp.Set ( perstHps );
                     pattBmp.Set ( hps );


    | GComm.point :  pColor := O.GpiQueryPel ( hps, spot );

    | GComm.getimg : rimgHandle := copyBmp.Copy ( hps, imgArea );

    | GComm.rect  :  O.GpiSetColor( hps, lnColor );
                     O.GpiSetCurrentPosition( hps, lnCorn0 );
                     IF lnFill
                       THEN O.GpiBox ( hps, O.DRO_FILL, lnCorn1, 0, 0);
                       ELSE O.GpiBox ( hps, O.DRO_OUTLINE, lnCorn1, 0, 0);
                     END;
    | GComm.plot  :  O.GpiSetColor ( hps, pColor    );
                     O.GpiSetPel   ( hps, spot );

    | GComm.disc  :  O.GpiSetColor ( hps, discColor );
                     O.GpiSetCurrentPosition ( hps, discCenter );
                     arcp.lP := discRad;
                     arcp.lQ := discRad;

                     O.GpiSetArcParams( hps, arcp);

                     IF discFill
                       THEN O.GpiFullArc ( hps, O.DRO_FILL, 10000H );
                       ELSE O.GpiFullArc ( hps, O.DRO_OUTLINE, 10000H );
                     END;

    | GComm.line  :  O.GpiSetColor( hps, lnColor );
                     O.GpiSetCurrentPosition( hps, lnCorn0 );
                     O.GpiLine ( hps, lnCorn1);

    | GComm.ellip :  O.GpiSetColor ( hps, eColor );
                     O.GpiSetCurrentPosition ( hps, eCenter );
                     arcp.lP := eARad;
                     arcp.lQ := eBRad;

                     O.GpiSetArcParams( hps, arcp );

                     IF elFill
                       THEN O.GpiFullArc ( hps, O.DRO_FILL, 10000H );
                       ELSE O.GpiFullArc ( hps, O.DRO_OUTLINE, 10000H );
                     END;

    | GComm.poly   : O.GpiSetColor ( hps, polyColor );
                     O.GpiSetCurrentPosition ( hps, polyNodes [polyN-1] );

                     IF polyFill
                       THEN polar[0].ulPoints := polyN ;
                            polar[0].aPointl  := O.PPOINTL (ADR(polyNodes));
                            O.GpiPolygons ( hps, 1, polar, O.POLYGON_BOUNDARY, O.POLYGON_INCL );
                       ELSE
                            O.GpiPolyLine ( hps, polyN, polyNodes );
                     END;

    | GComm.flood  : O.GpiSetColor ( hps, floodColor );
                     O.GpiSetCurrentPosition ( hps, seedp );
                     O.GpiFloodFill (hps, O.FF_BOUNDARY, boundColor);

    | GComm.arc    : O.GpiSetColor ( hps, arcColor );
                     arcp.lP := arcARad;
                     arcp.lQ := -VAL( LONGINT, arcBRad);
                     O.GpiSetArcParams( hps, arcp );

                     O.GpiSetLineType( hps, O.LINETYPE_INVISIBLE );
                     O.GpiPartialArc ( hps, arcCenter, 10000H, arcStartAngle, 0);
                     O.GpiSetLineType( hps, O.LINETYPE_SOLID );
                     O.GpiPartialArc ( hps, arcCenter, 10000H, arcStartAngle, arcSweetAngle);

    | GComm.pie    : O.GpiSetColor ( hps, pieColor );
                     arcp.lP := pieARad;
                     arcp.lQ := -VAL( LONGINT, pieBRad);
                     O.GpiSetArcParams( hps, arcp );

                     IF pieFill
                       THEN O.GpiBeginPath (hps, 1);
                            O.GpiMove ( hps, pieCenter );
                            O.GpiPartialArc ( hps, pieCenter, 10000H, pieStartAngle, pieSweetAngle);
                            O.GpiLine ( hps, pieCenter );
                            O.GpiEndPath ( hps );
                            O.GpiFillPath(hps, 1, O.FPATH_ALTERNATE);

                       ELSE O.GpiSetCurrentPosition ( hps, pieCenter );
                            O.GpiPartialArc ( hps, pieCenter, 10000H, pieStartAngle, pieSweetAngle);
                            O.GpiLine ( hps, pieCenter );
                     END;

    | GComm.text   : cp  := txPos;

                     bkr := txPos;
                     DEC (bkr.y, txfontDescend);
                     O.GpiSetColor ( hps, txBgr );
                     O.GpiSetCurrentPosition ( hps, bkr );
                     INC (bkr.y, txFontYd);
                     INC (bkr.x, txFontXd*LENGTH(txText)-1);
                     O.GpiBox ( hps, O.DRO_FILL, bkr, 0, 0);

                     O.GpiSetColor ( hps, txColor );
                     FOR i:=0 TO LENGTH(txText)-1 DO
                       zztop[0] := txText[i];
                       O.GpiCharStringAt (hps, cp, 1, zztop );
                       INC (cp.x, txFontXd );
                     END;

    | GComm.rtext  : O.GpiSetColor ( hps, rtxColor );

                     O.GpiCharStringAt ( hps, rtxPos, LENGTH (rtxText), rtxText);

    | GComm.putimg   : copyBmp.Paste ( hps, imgLCorn, imgHandle, imgMode );

    | GComm.delimg   : O.GpiDeleteBitmap ( delImgHandle );

    | GComm.setfocus : O.WinPostMsg(hMainClient, WM_setfocus, NIL, NIL);

    | GComm.resize   : doResize();

    | ELSE ;
  END;
  END;  (* WITH *)
END DrawObj;



PROCEDURE [O.EXPENTRY] ServerDraw (c :CARDINAL);
BEGIN
(*
    this  releaseServerReady() tells Graph.Init that share mem's been took,
                                                the output results are wrote
                                                and server's ready
*)
  releaseServerReady();
  LOOP
    waitClientReady();

    IF ( GComm.rmodeB <= pGObj^.obj ) AND (pGObj^.obj <= GComm.rmodeE)  --  read mode of interaction
      THEN DrawObj ( BitMap.hpsM );
           IF (pGObj^.obj = GComm.map1col)   -- !!!!!!!!! so dirty
             THEN RGB.Map1Col ( copyBmp.hpsMem,
                  pGObj^.col2BSet, pGObj^.colInd );
                  RGB.Map1Col ( perstHps, pGObj^.col2BSet, pGObj^.colInd );
           END;
      ELSE
           --O.DosRequestMutexSem( hScrMutex, O.SEM_INDEFINITE_WAIT);
           DrawObj ( perstHps );
           --O.DosReleaseMutexSem( hScrMutex );
           DrawObj ( BitMap.hpsM );

           IF (pGObj^.obj = GComm.cancelClip) THEN hrgn := 0; END;

     END;

     releaseServerReady();
  END; (* LOOP *)

END ServerDraw;


PROCEDURE mouseAction(ulMsg :LONGCARD; m1, m2 :O.MPARAM);
BEGIN
  pGObj^.mouseMsg := ulMsg;
  pGObj^.mouseMp1 := m1;
  pGObj^.mouseMp2 := m2;
  O.DosPostEventSem(hservMouseSem);
END mouseAction;

PROCEDURE [O.EXPENTRY] wpMainClient(hWnd   :O.HWND;
                                    ulMsg  :LONGCARD;
                                    m1, m2 :O.MPARAM) :O.MRESULT;
TYPE
  CHPIPE_MSG = RECORD
    cb    : SYSTEM.CARD16;
    m1, m2: O.MPARAM;
  END;

VAR
  swp        :O.SWP;
  gpiHps     :O.HPS;
  pipemsg    :CHPIPE_MSG;
  ul         :O.ULONG;

BEGIN
  O.WinQueryWindowPos(hWnd, swp);

  CASE ulMsg OF
   |O.WM_CREATE:
      O.WinDefWindowProc(hWnd,ulMsg,m1,m2);
      O.WinSetWindowPos(hMainFrame, O.HWND_TOP,
                        xLeft, yBottom,
                        xd + LONGCARD (O.WinQuerySysValue( O.HWND_DESKTOP, O.SV_CXBORDER)*2),
                        yd + LONGCARD (O.WinQuerySysValue( O.HWND_DESKTOP, O.SV_CYTITLEBAR)) +
                        LONGCARD (O.WinQuerySysValue( O.HWND_DESKTOP, O.SV_CYBORDER)*2),
                        O.SWP_SIZE + O.SWP_ZORDER + O.SWP_MOVE + O.SWP_SHOW );

      -- create screen bitmap --
      perstHps := O.WinGetPS (hWnd);

      BitMap.Create();
      Translate.Initial (yd);
      Translate.Flip ( perstHps );
      Translate.Flip ( BitMap.hpsM );
      Translate.Flip ( copyBmp.hpsMem );
      RGB.InitPal ( perstHps );
      RGB.InitPal ( BitMap.hpsM );
      RGB.InitPal ( copyBmp.hpsMem );


   |O.WM_PAINT:
      hpsw := O.WinBeginPaint(hWnd, 0, NIL );

      --O.DosRequestMutexSem( hScrMutex, O.SEM_INDEFINITE_WAIT);
      gpiHps := O.WinGetPS (hWnd);
      BitMap.Draw ( gpiHps );
      O.WinReleasePS ( gpiHps );
      --O.DosReleaseMutexSem( hScrMutex );
      O.WinEndPaint( hpsw );

   |  WM_START:
      O.DosCreateThread ( tid, ServerDraw, 0, O.CREATE_READY, 3fffh);
      O.DosSetPriority  (O.PRTYS_THREAD, O.PRTYC_REGULAR, 5, tid );
      --O.DosResumeThread ( tid )

   | WM_setfocus :                               --*FSA
      O.WinSetActiveWindow(O.HWND_DESKTOP,hWnd);
      O.WinSetFocus       (O.HWND_DESKTOP,hWnd);

   | WM_resize :                                 --*FSA
     O.WinSetWindowPos(hMainFrame, 0, xLeft, yBottom,
                        xd + LONGCARD (O.WinQuerySysValue( O.HWND_DESKTOP, O.SV_CXBORDER)*2),
                        yd + LONGCARD (O.WinQuerySysValue( O.HWND_DESKTOP, O.SV_CYTITLEBAR)) +
                        LONGCARD (O.WinQuerySysValue( O.HWND_DESKTOP, O.SV_CYBORDER)*2),
                        O.SWP_SIZE + O.SWP_MOVE + O.SWP_SHOW);
     O.WinInvalidateRect(hWnd, NIL, FALSE);

   |O.WM_CHAR:
     IF (hChPipe # 0) THEN
       pipemsg.cb := SIZE(pipemsg);
       pipemsg.m1 := m1;
       pipemsg.m2 := m2;
       O.DosWrite(hChPipe, ADR(pipemsg), SIZE(pipemsg), ul);
     END;
     RETURN O.WinDefWindowProc(hWnd,ulMsg,m1,m2);

   |O.WM_BUTTON1DOWN, O.WM_BUTTON2DOWN:
     IF O.WinQueryFocus(O.HWND_DESKTOP) # hWnd
      THEN RETURN O.WinDefWindowProc(hWnd,ulMsg,m1,m2);
     END;
     mouseAction(ulMsg,m1,m2);
     (*
      ELSIF (O.WinOpenClipbrd(hAB) # FALSE)
           THEN        O.WinMessageBox(O.HWND_DESKTOP, 0,
                      "I'm gonna copy to clip",
                      "Graph", 0,
                       O.MB_OK + O.MB_MOVEABLE);

                IF NOT O.WinSetClipbrdData(hAB, BitMap.hb, O.CF_BITMAP,O.CFI_HANDLE)
                 THEN
                  FormStr.print(ss, "SetClip failure %x",
                                O.WinGetLastError( hAB ) );
                                     O.WinMessageBox(O.HWND_DESKTOP, 0,
                                                     ss,
                                                     "Graph", 0,
                                                     O.MB_OK + O.MB_MOVEABLE);
                END;
                O.WinCloseClipbrd(hAB);
      *)
   | O.WM_MOUSEMOVE,
     O.WM_BUTTON1UP,
     O.WM_BUTTON1DBLCLK,
     O.WM_BUTTON2UP,
     O.WM_BUTTON2DBLCLK,
     O.WM_BUTTON3DOWN,
     O.WM_BUTTON3UP,
     O.WM_BUTTON3DBLCLK:
     IF O.WinQueryFocus(O.HWND_DESKTOP) = hWnd THEN
       mouseAction(ulMsg,m1,m2);
     END;
     RETURN O.WinDefWindowProc(hWnd,ulMsg,m1,m2);

   | O.WM_CLOSE : O.WinReleasePS ( perstHps );
                  RETURN O.WinDefWindowProc(hWnd,ulMsg,m1,m2);

   | ELSE RETURN O.WinDefWindowProc(hWnd,ulMsg,m1,m2);
  END; (* CASE*)
  RETURN O.MRESULT(FALSE);

END wpMainClient;


VAR
  szPID    :ARRAY [0..15] OF CHAR;
  szName   :ARRAY [0..63] OF CHAR;
  SwData   :O.SWCNTRL;
  hSwitch  :O.HSWITCH;
  qMsg     :O.QMSG;
  fcdata   :O.FRAMECDATA;
  ulAction :O.ULONG;

  fm       :O.FONTMETRICS;
  i        :LONGCARD;
  widthTab :ARRAY [0..255] OF O.LONG;

BEGIN

  GComm.mouseProc := NIL;
  IF ProgEnv.ArgNumber() = 0 THEN HALT END;
  ProgEnv.GetArg(0,szPID);

  rc := O.DosCreateMutexSem(NIL, hScrMutex, 0, FALSE);
  IF rc <> O.NO_ERROR THEN
    Error("Could not create mutex sem",rc);
    HALT
  END;

  hclReadySem := 0;
  Strings.Concat(GComm.clReadySemName,szPID,szName);
  rc := O.DosOpenEventSem( szName, hclReadySem );
  IF rc <> O.NO_ERROR THEN
    Error("Could not open event sem C",rc);
    HALT
  END;

  hservReadySem := 0;
  Strings.Concat(GComm.servReadySemName,szPID,szName);
  rc := O.DosOpenEventSem( szName, hservReadySem );
  IF rc <> O.NO_ERROR THEN
    Error("Could not open event sem S",rc);
    HALT
  END;

  hservMouseSem := 0;
  Strings.Concat(GComm.mouseActionSemName,szPID,szName);
  rc := O.DosOpenEventSem( szName, hservMouseSem );
  IF rc <> O.NO_ERROR THEN
    Error("Could not open event sem M",rc);
    HALT
  END;

  hChPipe := 0;
  Strings.Concat(GComm.wmcharPipeName,szPID,szName);
  rc := O.DosOpen(szName, hChPipe, ulAction, 0, O.FILE_NORMAL,
               O.FILE_OPEN, O.OPEN_ACCESS_WRITEONLY+O.OPEN_SHARE_DENYNONE, NIL);
  IF rc <> O.NO_ERROR THEN
    Error("Could not open pipe",rc);
    HALT
  END;

  IF NOT O.WinRegisterClass( hAB, MAINCLASSNAME, wpMainClient,
                             O.CS_SIZEREDRAW + O.CS_CLIPCHILDREN, 0) THEN
    Error("Window class was not registred",O.WinGetLastError(hAB));
    HALT;
  END;


  (* get window parms from sharemem *)
  Strings.Concat(GComm.shMemName,szPID,szName);
  rc :=  O.DosGetNamedSharedMem(pShMem,                -- pointer is to return there
                                szName,                -- Name of shared memory
                                O.PAG_READ + O.PAG_WRITE );
  IF ( rc # O.NO_ERROR ) THEN
    Error("Shared memory was not allocated",rc);
    HALT;
  END;

  pGObj            := GComm.ptrGObj (pShMem);

  pGObj^.obj       := GComm.ok;
  xd               := pGObj^.xd;
  yd               := pGObj^.yd;
  xLeft            := pGObj^.xLeft;
  yBottom          := RGB.yRasterPels-1 - pGObj^.yBottom - yd;  -- Win32 l.u. corner
  pGObj^.numColors := RGB.numColors;

  -- obtain font size & calc a max number of rows and columns

  WITH pGObj^ DO
    O.GpiQueryFontMetrics(O.WinGetScreenPS(O.HWND_DESKTOP), SIZE(O.FONTMETRICS), fm);
    fontMetricYd        := fm.lMaxBaselineExt + fm.lExternalLeading + 1;
    fontMetricDescender := fm.lMaxDescender;

    fontMetricXd := 0;
    FILL ( ADR (widthTab), 0, SIZE (widthTab) );
    O.GpiQueryWidthTable  (O.WinGetScreenPS(O.HWND_DESKTOP), 0, 256, widthTab );
    FOR i := 0 TO 255 DO
      IF (LONGCARD(widthTab[i]) > fontMetricXd) THEN fontMetricXd := LONGCARD(widthTab[i]) END;
    END;
  END;

  (* init globals *)

  hrgn := O.NULLHANDLE;


  fcdata.cb := SIZE(O.FRAMECDATA);

  fcdata.flCreateFlags := O.FCF_SYSMENU + O.FCF_TITLEBAR + O.FCF_ICON + O.FCF_BORDER +
                          O.FCF_MINBUTTON  + O.FCF_SHELLPOSITION;
  fcdata.hmodResources := 0;
  fcdata.idResources   := RES_MAIN;


  hMainFrame := O.WinCreateWindow(O.HWND_DESKTOP, (* parent window          *)
                   O.WC_FRAME,                    (* class name             *)
                   "Graph",                       (* window text            *)
                   0,                             (* window style           *)
                   0,0,                           (* position (x,y)         *)
                   0,0,                           (* size (width,height)    *)
                   0,                             (* owner window           *)
                   O.HWND_TOP,                    (* sibling window         *)
                   0,                             (* window id              *)
                   ADR(fcdata),                   (* control data           *)
                   NIL);                          (* presentation parms     *)


  hMainClient := O.WinCreateWindow(hMainFrame,    (* parent window          *)
                  MAINCLASSNAME,                  (* class name             *)
                  NIL,                            (* window text            *)
                  0,                              (* window style           *)
                  0, 0,                           (* position (x,y)         *)
                  0, 0,                           (* size (width,height)    *)
                  hMainFrame,                     (* owner window           *)
                  O.HWND_TOP,                     (* sibling window         *)
                  O.FID_CLIENT,                   (* window id              *)
                  NIL,                            (* control data           *)
                  NIL);                           (* presentation parms     *)

  IF hMainClient = 0 THEN
    Error("Failed to create client window",O.WinGetLastError(hAB));
    HALT;
  END;

  (* Add switch entry to the system Window List *)
  WITH SwData DO
    hwnd            := hMainFrame;
    hwndIcon        := 0;
    hprog           := 0;
    idProcess       := 0;
    idSession       := 0;
    uchVisibility   := O.SWL_VISIBLE;
    fbJump          := O.SWL_JUMPABLE;
    szSwtitle       := "Graph Library";
  END;
  hSwitch := O.WinAddSwitchEntry(SwData);


  O.WinPostMsg(hMainClient, WM_START, NIL, NIL);

  (* Main message proc loop *)
  WHILE ( O.WinGetMsg( hAB, qMsg, 0, 0, 0 ) ) DO
     O.WinDispatchMsg( hAB, qMsg );
  END;

  (* Release all resources *)
  O.WinRemoveSwitchEntry(hSwitch);
  O.WinDestroyWindow(hMainFrame);

  O.DosCloseMutexSem ( hScrMutex );

FINALLY
  O.DosBeep(1000,1000);
  O.DosBeep(1000,1000);
  O.DosBeep(1000,1000);
END TSGServ2.

