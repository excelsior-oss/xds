
<* +M2ADDTYPES   *>
<* +M2EXTENSIONS *>
<* IF NOT DEFINED(DEBUG) THEN *> <* NEW DEBUG+ *> <* END *>

IMPLEMENTATION MODULE xtsGraph;

IMPORT   SYSTEM, 
         vga, 
         vgagl, 
         Strings,
         LM:=LongMath,
	 xtsMsMouse,
	 Graph;

FROM Serv    IMPORT min, max, orderCoords, adjust; 
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM Graph   IMPORT HBITMAP, ImageSize;


(*--------------------------------------------------------------------------
-- Global function: GetImage
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE GetImage (x1, y1, x2, y2: CARDINAL; VAR handle: HBITMAP);
VAR
  img: image_info_ptr;
BEGIN
  orderCoords (x1, y1, x2, y2);

  adjust ( x1, 0, cur_mode_info^.width-1  );
  adjust ( x2, 0, cur_mode_info^.width-1  );
  adjust ( y1, 0, cur_mode_info^.height-1 );
  adjust ( y2, 0, cur_mode_info^.height-1 );

  ALLOCATE (img, SIZE (image_info));
  IF img # NIL THEN
    ALLOCATE (img^.ptr, ImageSize (x1, y1, x2, y2));
    IF img^.ptr # NIL THEN
      img^.w := x2-x1+1;
      img^.h := y2-y1+1;
      vgagl.gl_getbox (x1, y1, img^.w, img^.h, img^.ptr);
    ELSE
      DEALLOCATE (img, SIZE (image_info));
      img := NIL;
    END;
  END;
  handle := SYSTEM.CAST (HBITMAP, img);
END GetImage;


(*--------------------------------------------------------------------------
-- Global function: PutImage
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE PutImage (x1, y1: CARDINAL; hbm: HBITMAP);
VAR
  img                  :image_info_ptr;
  x2,y2                :CARDINAL; 
  nx1, ny1, nx2, ny2   :CARDINAL;
BEGIN
  img := SYSTEM.CAST (image_info_ptr, hbm);

  IF img = NIL THEN RETURN; END;
  
  x2 := x1+img^.w-1;
  y2 := y1+img^.h-1;

  IF ((x2 < cur_clip_region.x1) OR (x1 > cur_clip_region.x2) OR 
      (y2 < cur_clip_region.y1) OR (y1 > cur_clip_region.y2)) THEN
    RETURN
  END;
  (* ASSERT: intersection of clipping area & (x1,y1,x2,y2) is NOT empty *)

  nx1 := max (x1, cur_clip_region.x1);
  ny1 := max (y1, cur_clip_region.y1);
  nx2 := min (x2, cur_clip_region.x2);
  ny2 := min (y2, cur_clip_region.y2);

  IF (nx2-nx1+1 = img^.w) AND ( ny2-ny1+1 = img^.h) 
  THEN  vgagl.gl_putbox     (nx1, ny1, img^.w, img^.h, img^.ptr );
  ELSE  vgagl.gl_putboxpart (nx1, ny1, nx2-nx1+1, ny2-ny1+1, img^.w, img^.h, img^.ptr, nx1-x1, ny1-y1);END;
  
END PutImage;

(*--------------------------------------------------------------------------
-- Global function: GetImage
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE ImageSize_C (x1, y1, x2, y2: INTEGER) :INTEGER;
BEGIN
  IF is16 THEN
    RETURN (ABS (x2 - x1)+1) *
           (ABS (y2 - y1)+1);
  
  ELSE
    RETURN (ABS (x2 - x1)+1) *
           (ABS (y2 - y1)+1) *
           (cur_mode_info^.bytesperpixel);
  END;	 
END ImageSize_C;

PROCEDURE GetImage_C (x1, y1, x2, y2: INTEGER; VAR handle: HBITMAP);
VAR
  img: image_info_ptr;
BEGIN
  ALLOCATE (img, SIZE (image_info));
  IF img # NIL THEN
    ALLOCATE (img^.ptr, ImageSize_C (x1, y1, x2, y2));
    IF img^.ptr # NIL THEN
      img^.w := x2-x1+1;
      img^.h := y2-y1+1;
      vgagl.gl_getbox (x1, y1, img^.w, img^.h, img^.ptr);
    ELSE
      DEALLOCATE (img, SIZE (image_info));
      img := NIL;
    END;
  END;
  handle := SYSTEM.CAST (HBITMAP, img);
END GetImage_C;


(*--------------------------------------------------------------------------
-- Global function: PutImage
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE PutImage_C (x1, y1: INTEGER; hbm: HBITMAP);
VAR
  img                  :image_info_ptr;
BEGIN
  img := SYSTEM.CAST (image_info_ptr, hbm);

  IF img # NIL THEN
    vgagl.gl_putbox     ( x1, y1, img^.w, img^.h, img^.ptr );
  END;
END PutImage_C;


(*///////////////////////////// Palette operations ////////////////////////////*)

(*--------------------------------------------------------------------------
-- Global function: RemapPalette
-- Description:
--------------------------------------------------------------------------*)

PROCEDURE RemapPalette (palItem: CARDINAL; Color: LONGCARD): LONGCARD;
VAR
  red, green , blue  : INTEGER;
  old                : LONGCARD;
BEGIN
  IF palItem > CARDINAL (cur_mode_info^.colors - 1) THEN 
      RETURN MAX(CARDINAL) 
  END;

  vga.vga_getpalette( palItem , red , green , blue);
  
  old    := LONGCARD(SYSTEM.SHIFT(BITSET(red),16)) + LONGCARD( SYSTEM.SHIFT(BITSET(green),8)) + LONGCARD(blue);
  
  blue   :=  INTEGER( BITSET(Color)*BITSET(0FFH) );
  green  :=  INTEGER( SYSTEM.SHIFT( BITSET(Color), -8)*BITSET(0FFH)  ); 
  red    :=  INTEGER( SYSTEM.SHIFT( BITSET(Color), -16)*BITSET(0FFH) ); 

  vga.vga_setpalette( palItem , red , green , blue );
  RETURN old;
END RemapPalette;


(*--------------------------------------------------------------------------
-- Global function: RemapAllPalette
-- Description:
--------------------------------------------------------------------------*)

PROCEDURE RemapAllPalette_N (colArray: ARRAY OF LONGCARD);
VAR
  i,ignore : CARDINAL;
  
BEGIN
  FOR i:=0 TO HIGH( colArray ) DO
      ignore := RemapPalette( i , colArray[i] );
  END;
END RemapAllPalette_N;

PROCEDURE RemapAllPalette (colArray: ARRAY OF LONGCARD): LONGCARD;
BEGIN
  IF HIGH(colArray) # CARDINAL (cur_mode_info^.colors - 1) THEN 
      RETURN MAX(CARDINAL) 
  END;
  RemapAllPalette_N( colArray);

  RETURN 0;
END RemapAllPalette;

(*--------------------------------------------------------------------------
-- Global function: InitStdPalette
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE InitStdPalette;
TYPE
  colors = ARRAY [0..15] OF LONGCARD;
CONST
  colArray = colors {Graph._BLACK,    Graph._BLUE,         Graph._GREEN,       Graph._CYAN,
                     Graph._RED,      Graph._MAGENTA,      Graph._BROWN,       Graph._WHITE,
                     Graph._GRAY,     Graph._LIGHTBLUE,    Graph._LIGHTGREEN,  Graph._LIGHTCYAN,
                     Graph._LIGHTRED, Graph._LIGHTMAGENTA, Graph._LIGHTYELLOW, Graph._BRIGHTWHITE};
BEGIN
    RemapAllPalette_N(colArray);
END InitStdPalette;



END xtsGraph.

