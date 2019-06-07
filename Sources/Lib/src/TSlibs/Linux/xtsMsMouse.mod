<* +M2EXTENSIONS *>
<* +M2ADDTYPES *>

IMPLEMENTATION MODULE xtsMsMouse;

 IMPORT MsMouse, 
        Graph,
        vga,
        SYSTEM,
        vgagl,
        xtsGraph, 
        gpm;


FROM Storage IMPORT ALLOCATE;

PROCEDURE Gpm_DrawPointer( x,y :SYSTEM.CARD16; fd: SYSTEM.int  );
BEGIN
   gpm._gpm_buf[2-1] := 2; 
   gpm._gpm_arg^[0] := x+gpm.gpm_zerobased;
   gpm._gpm_arg^[2] := gpm._gpm_arg^[0];
   gpm._gpm_arg^[1] := y+gpm.gpm_zerobased; 
   gpm._gpm_arg^[3] := gpm._gpm_arg^[1];
   gpm._gpm_arg^[4] := 3; 
   gpm.ioctl( fd, gpm.TIOCLINUX, SYSTEM.ADR(gpm._gpm_buf[2-1]) );
END Gpm_DrawPointer;

PROCEDURE hideCursor();
BEGIN
  IF (whetherCursorOn) THEN
    IF (showCursorCount = 0) THEN
      xtsGraph.PutImage_C( cur_pos.col, cur_pos.row, under_cursor);
    END;
    DEC (showCursorCount);
  END;
END hideCursor;

TYPE
  image_info_ptr = POINTER TO image_info;
  image_info = RECORD
    w, h: CARDINAL;
    ptr: SYSTEM.ADDRESS;
  END;

PROCEDURE showCursor();
VAR
  img                  :image_info_ptr;

BEGIN
  IF (whetherCursorOn) THEN
    INC (showCursorCount);
    IF (showCursorCount = 0) THEN
       Graph.DelImage( under_cursor );
       xtsGraph.GetImage_C( cur_pos.col, cur_pos.row, cur_pos.col+16,cur_pos.row+16, under_cursor);
       img := SYSTEM.CAST (image_info_ptr, cur_cursor);
       vgagl.gl_putboxmask( cur_pos.col, cur_pos.row, 16, 16, img^.ptr );
    END; 
  END;
END showCursor;

PROCEDURE SetGraphCursor(VAR mp: MsMouse.MsGraphcur);
VAR
  set       :BITSET;
(*  backscreenptr      :vgagl.GraphicsContextPtr;
  backscreen         :vgagl.GraphicsContext;

  config    :Graph.VideoConfig;
*)  color     :CARDINAL;
  i,j       :CARDINAL;

  temp_image  :Graph.HBITMAP;
  temp_col    :LONGCARD;
BEGIN
  hideCursor;
(*  Graph.GetVideoConfig( config );
  stdio.printf( " %d %d ", config.numxpixels, config.numypixels);

  vgagl.gl_setcontext( backscreen );
*)

  Graph.GetImage( 0,0,15,15, temp_image );
  temp_col := Graph.RemapPalette( Graph._clrBLACK, Graph._BLACK );
  vgagl.gl_fillbox(0,0,15,15,0);

  color := vga.vga_white();

  FOR i:=0 TO 15 DO
    set := SYSTEM.CAST( BITSET, mp.cursor_mask[i] );
    FOR j := 0 TO 15 DO 
      IF j IN set THEN
        vgagl.gl_setpixel ( 15-j, i, color );
      END;      
    END;
  END;

  xtsGraph.GetImage( 0,0,15,15, cur_cursor );
  temp_col := xtsGraph.RemapPalette( Graph._clrBLACK, temp_col );
  xtsGraph.PutImage( 0, 0, temp_image);

  hot_spot.row := mp.row;
  hot_spot.col := mp.col;

  showCursor;
END SetGraphCursor;

(* ---------------------  Text mouse --------------------------------- *)


VAR 
   conn     :gpm.H2D_PtrGpm_Connect;

PROCEDURE InitTextMouse;
BEGIN

(*   initialization for text mode *)
   conn^.eventMask   := gpm.GPM_MOVE+gpm.GPM_UP+gpm.GPM_DOWN;
   conn^.defaultMask := 0;
   conn^.minMod      := 0;
   conn^.maxMod      := 0;
                                        
   gpm.Gpm_Open ( conn, -1 );

   countForShowText := 0;
   IF xtsGraph.isText THEN
      Gpm_DrawPointer( cur_pos.col, cur_pos.row, gpm.gpm_consolefd );
   END;  
   
END InitTextMouse;

PROCEDURE showTextCursor;
BEGIN
  IF countForShowText < 0 THEN
     INC( countForShowText ); 
     IF countForShowText = 0 THEN
         Gpm_DrawPointer( cur_pos.col, cur_pos.row, gpm.gpm_consolefd );
     END;
  END;            
END showTextCursor;

PROCEDURE hideTextCursor;
BEGIN
 IF countForShowText = 0 THEN
--     gpm.Gpm_DrawPointer( 200, 200, gpm.gpm_consolefd );
     Gpm_DrawPointer( 200, 200, gpm.gpm_consolefd );                     
 END;
 DEC( countForShowText );
END hideTextCursor;

BEGIN
  under_cursor
    := 0;
  whetherCursorOn := FALSE;
  ALLOCATE( conn , SIZE(gpm.Gpm_Connect) );
  ALLOCATE( event, SIZE(gpm.Gpm_Event) );
END xtsMsMouse.