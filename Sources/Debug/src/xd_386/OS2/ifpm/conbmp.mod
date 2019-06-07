IMPLEMENTATION MODULE conBmp;

IMPORT SYSTEM, FormStr, O:=OS2;
FROM Storage IMPORT ALLOCATE;
FROM SYSTEM IMPORT ADR, ADDRESS, FILL;


(*==================== R G B ====================*)

MODULE RGB;

IMPORT O;
IMPORT ADR, ADDRESS;

EXPORT QUALIFIED numColors, planes, bitcnt, yRasterPels;

VAR
  numColors,
  planes,
  bitcnt,
  yRasterPels :CARDINAL;


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

BEGIN
  GetSCaps();
END RGB;

(*==========================================================*)


    -- Globals: --
VAR
  hbm          : O.HBITMAP;
  hpsM         : O.HPS;
  hdcM         : O.HDC;
  cxBmp, cyBmp : CARDINAL;



PROCEDURE DestroyAll();
BEGIN
  IF (hbm  # 0) THEN O.GpiDeleteBitmap (hbm);  END;
  IF (hdcM # 0) THEN O.DevCloseDC      (hdcM); END;
  IF (hpsM # 0) THEN O.GpiDestroyPS    (hpsM); END;
  hbm     := 0;
  hpsM    := 0;
  hdcM    := 0;
END DestroyAll;


(* --- NewBitmap -----------------------------------------------
   --- Creates new bitmap and returns it's pres. space       ---
*)
PROCEDURE NewBitmap(cx, cy : CARDINAL; hAB : O.HAB) : O.HPS;
CONST
  drvName ["C"] = "Display";
VAR
  bmih     : O.BITMAPINFOHEADER2;
  dop      : O.DEVOPENSTRUC;
  sizl     : O.SIZEL;
  aPt      : ARRAY[0..3] OF O.POINTL;
  rcl      : O.RECTL;
BEGIN
  IF (cx=0) THEN cx := 1; END;
  IF (cy=0) THEN cy := 1; END;
  IF (hbm # 0) AND (cx = cxBmp) AND (cy = cyBmp) THEN RETURN hpsM; END;

  DestroyAll();

  aPt[0].x := 0;  aPt[0].y := 0;
  aPt[1].x := cx; aPt[1].y := cy;
  aPt[2].x := 0;  aPt[2].y := 0;
  aPt[3].x := cx; aPt[3].y := cy;

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

  FILL ( ADR(bmih),0,  SIZE (O.BITMAPINFOHEADER2));
  bmih.cbFix       := SIZE ( O.BITMAPINFOHEADER2 );
  bmih.cx          := cx;
  bmih.cy          := cy;
  bmih.cPlanes     := RGB.planes;
  bmih.cBitCount   := RGB.bitcnt;

  hbm := O.GpiCreateBitmap (hpsM, bmih, 0, NIL, NIL );

  O.GpiSetBitmap(hpsM, hbm);

  cxBmp := cx; cyBmp := cy;

  rcl.xLeft  := 0;      rcl.yBottom := 0;
  rcl.xRight := cxBmp;  rcl.yTop    := cyBmp;
  O.WinFillRect(hpsM, rcl, O.CLR_DARKGRAY);

  RETURN hpsM;

END NewBitmap;


BEGIN
  hbm     := 0;
  hpsM    := 0;
  hdcM    := 0;

FINALLY
  DestroyAll();
END conBmp.


