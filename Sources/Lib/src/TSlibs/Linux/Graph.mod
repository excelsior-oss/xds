(* Copyright (c) 1999 JSC XDS. All rights reserved. *)
(* $RCSfile: Graph.mod,v $ $Revision: 6 $ $Date: 8/31/01 4:47p $ *)

<* +M2ADDTYPES   *>
<* +M2EXTENSIONS *>
<* IF NOT DEFINED(DEBUG) THEN *> <* NEW DEBUG+ *> <* END *>

IMPLEMENTATION MODULE Graph;

(*
    Title:   Implementation of svgalib interface to TopSpeed Graph mode
    Created: Mon  Feb 22 14:17:50 1999
    Author:  Igor L. Ziryanov <ego@xds.ru>
*)

IMPORT   SYSTEM, 
         vga, 
         vgagl, 
         vgamouse,
         Strings,
         LM:=LongMath,
         xtsMsMouse,
         xtsGraph;

FROM xtsGraph IMPORT cur_clip_region,_Region,image_info_ptr,
                     image_info, cur_mode_info, is16, isText;

FROM Serv     IMPORT min, max, orderCoords, adjust,
                    lScanHLine,scanHLine,rScanHLine;

FROM Storage  IMPORT ALLOCATE, DEALLOCATE;


--------------------------------------------------------------------------
-- Global variables
--------------------------------------------------------------------------
VAR
  cur_video_mode   : CARDINAL;
  cur_line_style   : LONGCARD;
  cur_std_fill_mask: LONGINT;
  cur_fill_mask    : FillMaskType;
  cur_bk_color     : LONGCARD;
  cur_text_color   : LONGCARD;
  cur_text_coords  : TextCoords;
  cur_wraping      : BOOLEAN;
  cur_cursor_toggle: BOOLEAN;
  cur_text_window  : _Region;

  cur_mode         : INTEGER;
  auto_refresh     : BOOLEAN;
  
  cur_isOpaque     : BOOLEAN;
  
VAR
  GPar_unpackedStyle :ARRAY[0..24] OF BOOLEAN;
  unpackedMask       :ARRAY [0..7] OF ARRAY [0..7] OF BOOLEAN;
  backscreenptr      :vgagl.GraphicsContextPtr;
  physicalscreenptr  :vgagl.GraphicsContextPtr;
  backscreen         :vgagl.GraphicsContext;
  physicalscreen     :vgagl.GraphicsContext;

PROCEDURE Update;
BEGIN
END Update;
   

(*///////////////////////////////////////////////////////////////////////*)
(*//// Local functions //////////////////////////////////////////////////*)
(*///////////////////////////////////////////////////////////////////////*)


(*--------------------------------------------------------------------------
-- Local function: error
-- Description: prints error message and bail out the program
--------------------------------------------------------------------------
PROCEDURE error (msg: ARRAY OF CHAR);
BEGIN
  stdio.fprintf (stdio.stderr^, 'Graph error: %s', msg);
  HALT (1);
END error;*)


(*///////////////////////////////////////////////////////////////////////*)
(*//// Global functions /////////////////////////////////////////////////*)
(*///////////////////////////////////////////////////////////////////////*)

PROCEDURE Refresh;
BEGIN
 IF is16 AND auto_refresh THEN 
    vgagl.gl_copyscreen( physicalscreen );
 END;    
END Refresh;


--------------------------------------------------------------------------
-- Global function: GraphMode
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE GraphMode;
BEGIN
  IF vga.vga_setmode (cur_mode) = 0 THEN
    IF is16 THEN
       vgagl.gl_setcontext( backscreen );
       vgagl.gl_copyscreen( physicalscreen );
    ELSE
       vgagl.gl_setcontextvga( cur_mode );
    END;
    cur_mode_info := vga.vga_getmodeinfo (cur_mode);
    WITH cur_clip_region DO
      x1 := 0; y2 := 0;
      x2 := cur_mode_info^.width - 1;
      y2 := cur_mode_info^.height - 1;
    END;
    isText := FALSE;
  END;
END GraphMode;


(*--------------------------------------------------------------------------
-- Global function: TextMode
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE TextMode;
BEGIN
  isText := TRUE;
  vga.vga_setmode (vga.TEXT);
END TextMode;



(*--------------------------------------------------------------------------
-- Global function: SetActivePage
-- Description: a terminator
--------------------------------------------------------------------------*)
PROCEDURE SetActivePage (Page :CARDINAL) :CARDINAL;
BEGIN
  RETURN 0;
END SetActivePage;


(*--------------------------------------------------------------------------
-- Global function: SetVisualPage
-- Description:.a terminator
--------------------------------------------------------------------------*)
PROCEDURE SetVisualPage (Page :CARDINAL) :CARDINAL;
BEGIN
  RETURN 0;
END SetVisualPage;


(*--------------------------------------------------------------------------
-- Global function: Init
-- Description: Initializes graphics mode maximaly close to given dimension
--              with maximum colors
-- On error: returns FALSE
-- Arguments: Coordinates of graphics window
--------------------------------------------------------------------------*)

CONST
  FONT_WIDTH  = 8;
  FONT_HEIGHT = 16;

PROCEDURE Text_Init;
BEGIN
    Columns := cur_mode_info^.width DIV FONT_WIDTH;
    Rows    := cur_mode_info^.width DIV FONT_HEIGHT;
    vgagl.gl_setfont( 8, 8, vgagl.gl_font8x8);
    vgagl.gl_setwritemode(vgagl.FONT_COMPRESSED + vgagl.WRITEMODE_OVERWRITE);
    vgagl.gl_setfontcolors(0, 15 );

    SetTextWindow( 1, 1, Rows, Columns);
END Text_Init;

PROCEDURE Init (xLeft, yTop, xd, yd: LONGCARD; CountColors: Colors): BOOLEAN;
TYPE
  video_modes_set =  ARRAY [0..5] OF INTEGER;

CONST
  set_320x200   = video_modes_set {vga.G320x200x16M32, vga.G320x200x16M,
                                   vga.G320x200x64K, vga.G320x200x32K,
                                   vga.G320x200x256, vga.G320x200x16};
  set_640x480   = video_modes_set {vga.G640x480x16M32, vga.G640x480x64K,
                                   vga.G640x480x32K, vga.G640x480x16M,
                                   vga.G640x480x256, vga.G640x480x16};
  set_800x600   = video_modes_set {vga.G800x600x16M32, vga.G800x600x16M,
                                   vga.G800x600x64K, vga.G800x600x32K,
                                   vga.G800x600x256, vga.G800x600x16};
  set_1024x768  = video_modes_set {vga.G1024x768x16M32, vga.G1024x768x16M,
                                   vga.G1024x768x64K, vga.G1024x768x32K,
                                   vga.G1024x768x256, vga.G1024x768x16};
  set_1152x864  = video_modes_set {vga.G1152x864x16M32, vga.G1152x864x16M,
                                   vga.G1152x864x64K, vga.G1152x864x32K,
                                   vga.G1152x864x256, vga.G1152x864x16};
  set_1280x1024 = video_modes_set { vga.G1280x1024x16M32, vga.G1280x1024x16M,
                                    vga.G1280x1024x64K, vga.G1280x1024x32K,
                                    vga.G1280x1024x256, vga.G1280x1024x16};
  set_1600x1200 = video_modes_set {vga.G1600x1200x16M32, vga.G1600x1200x16M,
                                   vga.G1600x1200x64K, vga.G1600x1200x32K,
                                   vga.G1600x1200x256, vga.G1600x1200x16};

PROCEDURE check_modes_set (modes_set: video_modes_set): INTEGER;
VAR
  i: INTEGER;
BEGIN

 IF CountColors=_NONECOLOR THEN
  FOR i := 0 TO 5 DO
    IF vga.vga_hasmode (modes_set [i]) # 0 THEN
      IF i = 5 THEN is16 := TRUE; END;
      RETURN i;
    END;
  END;
 ELSE 
   CASE CountColors OF
    |_16COLOR     : is16 := TRUE; RETURN 5;
    |_256COLOR    : RETURN 4;
    |_32KCOLOR    : RETURN 3;
    |_64KCOLOR    : RETURN 2;
    |_16MCOLOR    : RETURN 1;
    |_16M32COLOR  : RETURN 0;
   END;
 END;

 RETURN -1;
END check_modes_set;

PROCEDURE near (a, b, c: INTEGER): INTEGER;
BEGIN
  IF (a - b) > ((c - b) DIV 2) THEN
    RETURN c;
  ELSE
    RETURN b;
  END;
END near;

VAR
  mode, j        :INTEGER;
  ignore         :LONGCARD;
BEGIN
  IF xd <= 320 THEN xd := 320;
  ELSIF xd <= 360 THEN xd := near (xd, 320, 360);
  ELSIF xd <= 640 THEN xd := near (xd, 360, 640);
  ELSIF xd <= 720 THEN xd := near (xd, 640, 720);
  ELSIF xd <= 800 THEN xd := near (xd, 720, 800);
  ELSIF xd <= 1024 THEN xd := near (xd, 800, 1024);
  ELSIF xd <= 1152 THEN xd := near (xd, 1024, 1152);
  ELSIF xd <= 1280 THEN xd := near (xd, 1152, 1280);
  ELSE xd := near (xd, 1280, 1600); END;

  IF yd <= 200 THEN yd := 200;
  ELSIF yd <= 240 THEN yd := near (yd, 200, 240);
  ELSIF yd <= 348 THEN yd := near (yd, 240, 348);
  ELSIF yd <= 350 THEN yd := near (yd, 348, 350);
  ELSIF yd <= 400 THEN yd := near (yd, 350, 400);
  ELSIF yd <= 480 THEN yd := near (yd, 400, 480);
  ELSIF yd <= 600 THEN yd := near (yd, 480, 600);
  ELSIF yd <= 768 THEN yd := near (yd, 600, 768);
  ELSIF yd <= 864 THEN yd := near (yd, 768, 864);
  ELSIF yd <= 1024 THEN yd := near (yd, 864, 1024);
  ELSE yd := near (yd, 1024, 1200); END;

  is16 := FALSE;
 
  LOOP
     CASE xd OF
       |320:
        IF yd = 200 THEN
          j := check_modes_set (set_320x200);
          IF j >= 0 THEN mode := set_320x200 [j]; EXIT;
          ELSE mode := vga.TEXT; EXIT; END;
        ELSIF yd = 240 THEN
          IF vga.vga_hasmode (vga.G320x240x256) = 0 THEN yd := 200;
          ELSE mode := vga.G320x240x256; j := 4; EXIT; END;
        ELSIF yd = 400 THEN
          IF vga.vga_hasmode (vga.G320x400x256) = 0 THEN yd := 240;
          ELSE mode := vga.G320x400x256; j := 4; EXIT; END;
        ELSE yd := 400  END;
      |360:
        IF vga.vga_hasmode (vga.G360x480x256) = 0 THEN xd := 320; yd := 400;
        ELSE mode := vga.G360x480x256; j := 4; EXIT; END;
      |640:
        IF yd = 200 THEN
          IF vga.vga_hasmode (vga.G640x200x16) = 0 THEN xd := 360; yd := 480;
          ELSE mode := vga.G640x200x16; j := 5; EXIT; END;
        ELSIF yd = 350 THEN
          IF vga.vga_hasmode (vga.G640x350x16) = 0 THEN yd := 200;
          ELSE mode := vga.G640x350x16; j := 5; EXIT; END;
        ELSIF yd = 480 THEN
          j := check_modes_set (set_640x480);
          IF j >= 0 THEN mode := set_640x480 [j]; EXIT;
          ELSE
            IF vga.vga_hasmode (vga.G640x480x2) = 0 THEN yd := 350;
            ELSE mode := vga.G640x480x2; j := -1; EXIT; END;
          END;
        ELSE yd := 480 END;
      |720:
        IF vga.vga_hasmode (vga.G720x348x2) = 0 THEN xd := 640; yd := 480;
        ELSE mode := vga.G720x348x2; j := -1; EXIT; END;
      |800:
          j := check_modes_set (set_800x600);
          IF j >= 0 THEN mode := set_800x600 [j]; EXIT;
          ELSE xd := 720; yd := 348; END;
      |1024:
          j := check_modes_set (set_1024x768);
          IF j >= 0 THEN mode := set_1024x768 [j]; EXIT;
          ELSE xd := 800; yd := 600; END;
      |1152:
          j := check_modes_set (set_1152x864);
          IF j >= 0 THEN mode := set_1152x864 [j]; EXIT;
          ELSE xd := 800; yd := 600; END;
      |1280:
          j := check_modes_set (set_1280x1024);
          IF j >= 0 THEN mode := set_1280x1024 [j]; EXIT;
          ELSE xd := 1152; yd := 864; END;
      |1600:
          j := check_modes_set (set_1600x1200);
          IF j >= 0 THEN mode := set_1600x1200 [j]; EXIT;
          ELSE xd := 1280; yd := 1024; END;
      ELSE
        xd := 1600; yd := 1024;
    END (* case xd *);
  END (* loop *);

  IF mode # vga.TEXT THEN
    isText := FALSE;
    CASE j OF
      |0: cur_video_mode := _MRES256COLOR;
      |1: cur_video_mode := _MRES256COLOR;
      |2: cur_video_mode := _MRES256COLOR;
      |3: cur_video_mode := _MRES256COLOR;
      |4: cur_video_mode := _MRES256COLOR;
      |5:
        IF mode = vga.G320x200x16 THEN
          cur_video_mode := _MRES16COLOR;
        ELSIF mode = vga.G640x200x16 THEN
          cur_video_mode := _HRES16COLOR;
        ELSIF mode = vga.G640x350x16 THEN
          cur_video_mode := _ERESCOLOR;
        ELSE
          cur_video_mode := _VRES16COLOR;
        END;
      ELSE
        IF mode # vga.G720x348x2 THEN cur_video_mode := _VRES2COLOR;
        ELSE cur_video_mode := _HERCMONO; END;
    END (* case j *);
    cur_mode := mode;

    vga.vga_init;
    IF is16 THEN 
       vgagl.gl_setcontextvgavirtual( cur_mode );
       backscreenptr := vgagl.gl_allocatecontext();
       backscreen    := backscreenptr^;
       vgagl.gl_getcontext(backscreen);

       vga.vga_setmode( cur_mode );
       vgagl.gl_setcontextvga(cur_mode);        
       physicalscreenptr := vgagl.gl_allocatecontext();
       physicalscreen    := physicalscreenptr^;
       vgagl.gl_getcontext(physicalscreen);
    END;    


    SetLinestyle(0);
    SetStdFillMask( PATSYM_SOLID );
    ignore := SetBkColor (_clrBLACK);
    SetBkMix( _OPAQUE );
    GraphMode;
    ClearScreen(0);
    Text_Init();
    RETURN TRUE;
  END;
  RETURN FALSE;
END Init;


(*--------------------------------------------------------------------------
-- Global function: SetVideoMode
-- Description: it is just Init with default size
--------------------------------------------------------------------------*)
PROCEDURE SetVideoMode (Mode :CARDINAL): BOOLEAN;
VAR
  mode               :INTEGER;

BEGIN
  is16 := FALSE;
  isText := FALSE;
  CASE Mode OF
    |_MRES4COLOR:   mode := vga.G320x200x16; is16:=TRUE;  -- 320x200, 4 colors
    |_MRESNOCOLOR:  mode := vga.G320x200x16; is16:=TRUE;  -- 320x200, 4 grey
    |_HRESBW:       mode := vga.G640x200x16; is16:=TRUE;  -- 640x200, BW
    |_HERCMONO:     mode := vga.G720x348x2;               -- 720x348, BW for HGC
    |_MRES16COLOR:  mode := vga.G320x200x16; is16:=TRUE;  -- 320x200, 16 colors
    |_HRES16COLOR:  mode := vga.G640x200x16; is16:=TRUE;  -- 640x200, 16 colors
    |_ERESNOCOLOR:  mode := vga.G640x350x16; is16:=TRUE;  -- 640x350, BW
    |_ERESCOLOR:    mode := vga.G640x350x16; is16:=TRUE;  -- 640x350, 4 or 16 colors
    |_VRES2COLOR:   mode := vga.G640x480x2;               -- 640x480, BW
    |_VRES16COLOR:  mode := vga.G640x480x16; is16:=TRUE;  -- 640x480, 16 colors
    |_MRES256COLOR: mode := vga.G320x200x256;             -- 320x200, 256 colors
    ELSE
      isText := TRUE;
      mode := vga.TEXT;
  END (* case Mode *);
  IF vga.vga_hasmode (mode) # 0 THEN
    IF vga.vga_setmode (mode) = 0 THEN
      IF is16 THEN 
         vgagl.gl_freecontext( backscreen );
         vgagl.gl_freecontext( physicalscreen );         
         vga.vga_setmode( cur_mode );
         vgagl.gl_setcontextvga(cur_mode);      
         physicalscreenptr := vgagl.gl_allocatecontext();
         physicalscreen    := physicalscreenptr^;
         vgagl.gl_getcontext(physicalscreen);

         vgagl.gl_setcontextvgavirtual( cur_mode );
         backscreenptr := vgagl.gl_allocatecontext();
         backscreen    := backscreenptr^;
         vgagl.gl_setcontext( backscreen );
      ELSE
         vgagl.gl_setcontextvga( cur_mode );
      END;
      cur_mode_info := vga.vga_getmodeinfo (mode);
      WITH cur_clip_region DO
        x1 := 0; y2 := 0;
        x2 := cur_mode_info^.width - 1;
        y2 := cur_mode_info^.height - 1;
      END;
      cur_mode := mode;
      cur_video_mode := Mode;
      Text_Init;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END;
END SetVideoMode;


(*--------------------------------------------------------------------------
-- Global function: GetVideoConfig
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE GetVideoConfig (VAR V :VideoConfig);
BEGIN
  V.numxpixels    := cur_mode_info^.width;                  -- pixels on X axis
  V.numypixels    := cur_mode_info^.height;                 -- pixels on Y axis
  V.numtextcols   := cur_mode_info^.width DIV FONT_WIDTH;   -- text columns available
  V.numtextrows   := cur_mode_info^.width DIV FONT_HEIGHT;  -- text rows available
  V.numcolors     := cur_mode_info^.colors;                 -- actual colors
  V.bitsperpixel  := cur_mode_info^.bytesperpixel * 8;      -- bits per pixel
  V.numvideopages := 1;                                     -- available video pages
  V.mode          := cur_video_mode;                        -- current video mode
  V.adapter       := _VGA;                                  -- active display adapter
  V.monitor       := _ENHCOLOR;                             -- active display monitor
  V.memory        := cur_mode_info^.memory;                 -- adapter video memory in K bytes
END GetVideoConfig;


(*--------------------------------------------------------------------------
-- Global function: Exit
-- Description: set text mode and exit
--------------------------------------------------------------------------*)
PROCEDURE Exit;
BEGIN
  vga.vga_setmode (vga.TEXT);
--  HALT (0);
END Exit;


(*--------------------------------------------------------------------------
-- Global function: ClearScreen
-- Description: Clears a screen
--------------------------------------------------------------------------*)
PROCEDURE ClearScreen (Area: CARDINAL);
BEGIN
  xtsMsMouse.hideCursor;   
  CASE Area OF
    | _GCLEARSCREEN : vgagl.gl_clearscreen (cur_bk_color);
    | _GWINDOW      : vgagl.gl_fillbox( (cur_text_window.x1-1)*FONT_WIDTH, (cur_text_window.y1-1)*FONT_HEIGHT,
                                   (cur_text_window.x2-cur_text_window.x1+1)*FONT_WIDTH, (cur_text_window.y2-cur_text_window.y1+1)*FONT_HEIGHT,
                                   cur_bk_color );
  ELSE;
  END;
  SetTextWindow( 1, 1,  cur_mode_info^.height DIV FONT_HEIGHT, cur_mode_info^.width DIV FONT_WIDTH);
  xtsMsMouse.showCursor; 
  Refresh;
END ClearScreen;


(*--------------------------------------------------------------------------
-- Global function: SetLineStyle
-- Description:     Sets predefined line drawing style
--------------------------------------------------------------------------*)
PROCEDURE XSetLinestyle (Style :LONGCARD);
VAR
  i :CARDINAL;
BEGIN
  cur_line_style := Style;

  FOR i:=0 TO 24 DO
    GPar_unpackedStyle[i] := ((Style MOD 2) = 1);
    Style := Style DIV 2;
  END;
END XSetLinestyle;

PROCEDURE SetLinestyle (Style: LONGCARD);
BEGIN
  CASE Style OF
    | LNSTYLE_SOLID :         XSetLinestyle( 00FFFFFH );
    | LNSTYLE_DOT :           XSetLinestyle( 01C71C7H );
    | LNSTYLE_SHORTDASH :     XSetLinestyle( 003FFFFH );
    | LNSTYLE_DASHDOT :       XSetLinestyle( 00381FFH );
    | LNSTYLE_DASHDOUBLEDOT : XSetLinestyle( 01C71FFH );
    | LNSTYLE_INVISIBLE :     XSetLinestyle( 0000000H );
    ELSE                      XSetLinestyle( 00FFFFFH );
 END;
END SetLinestyle;


(*--------------------------------------------------------------------------
-- Global function: GetLineStyle
-- Description:     Gets predefined line drawing style
--------------------------------------------------------------------------*)
PROCEDURE GetLinestyle (): LONGCARD;
BEGIN
  RETURN cur_line_style;
END GetLinestyle;


(*--------------------------------------------------------------------------
-- Global function: SetFillMask
-- Description:     Sets up predefined fill mask
--------------------------------------------------------------------------*)
PROCEDURE SetFillMask (Mask: FillMaskType);
VAR
  i, j :CARDINAL;
  m    :SYSTEM.CARD8;
BEGIN
  cur_fill_mask := Mask;
  FOR j:=0 TO 7 DO
    m := Mask[j];
    FOR i:=0 TO 7 DO
      unpackedMask[i,j] := (m MOD 2)=1;
      m := m DIV 2;
    END;
  END;
END SetFillMask;


(*--------------------------------------------------------------------------
-- Global function: GetFillMask
-- Description:     Gets up predefined fill mask
-- Returns:         Current predefined fill mask
--------------------------------------------------------------------------*)
PROCEDURE GetFillMask (VAR Mask: FillMaskType);
BEGIN
  Mask := cur_fill_mask;
END GetFillMask;


(*--------------------------------------------------------------------------
-- Global function: GetStdFillMask
-- Description:     Gets up standard fill mask
-- Returns:         Current standard fill mask
--------------------------------------------------------------------------*)
PROCEDURE GetStdFillMask (): LONGINT;
BEGIN
  RETURN cur_std_fill_mask;
END GetStdFillMask;


(*--------------------------------------------------------------------------
-- Global function: SetStdFillMask
-- Description:     Sets up standard fill mask
--------------------------------------------------------------------------*)

PROCEDURE SetStdFillMask ( Mask  :LONGINT );
VAR
  mask         :FillMaskType;
  i            :CARDINAL;
BEGIN
 CASE Mask OF
   | 0 :                       (*PATSYM_DENSE1 *)
             mask[0] :=  081H;
             mask[1] :=  042H;
             mask[2] :=  024H;
             mask[3] :=  018H;
             mask[4] :=  018H;
             mask[5] :=  024H;
             mask[6] :=  042H;
             mask[7] :=  081H;
   | 1 :                       (* PATSYM_DENSE2 *)
             mask[0] :=  0FFH;
             FOR i:=1 TO 6 DO 
               mask[i] :=  001H;
             END;
             mask[7] :=  000H;
   | 2 :                       (* PATSYM_VERT  *)
             FOR i:=0 TO 7 DO 
               mask[i] :=  001H;
             END;
   | 3 :                       (* PATSYM_HORIZ *)
             mask[0] := 0FFH;
             FOR i:=1 TO 7 DO 
               mask[i] :=  0H;
             END;
   | 4 :                       (* PATSYM_DIAG1 *)
             mask[0] :=  080H;
             mask[1] :=  040H;
             mask[2] :=  020H;
             mask[3] :=  010H;
             mask[4] :=  008H;
             mask[5] :=  004H;
             mask[6] :=  002H;
             mask[7] :=  001H;
   | 5 :                       (* PATSYM_DIAG2 *)
             mask[0] :=  010H;
             mask[1] :=  020H;
             mask[2] :=  040H;
             mask[3] :=  080H;
             mask[4] :=  001H;
             mask[5] :=  002H;
             mask[6] :=  004H;
             mask[7] :=  008H;
   | 6 :                       (* PATSYM_SOLID *)
             FOR i:=0 TO 7 DO 
               mask[i] :=  0FFH;
             END;
   ELSE                        (* PATSYM_SOLID *)
             FOR i:=0 TO 7 DO 
               mask[i] :=  0FFH;
             END;
  END;
  SetFillMask( mask ); 
  cur_std_fill_mask := Mask;
END SetStdFillMask;


(*--------------------------------------------------------------------------
-- Global function: SetBkColor
-- Description:     Sets the current background color
-- Returns:         Current backgroud color
--------------------------------------------------------------------------*)
PROCEDURE SetBkColor (Color: LONGCARD): LONGCARD;
VAR
  prev, ignore  : LONGCARD;
BEGIN
  prev  := cur_bk_color;
  cur_bk_color := Color;
  ignore:=SetTextColor( cur_text_color );
  RETURN prev;
END SetBkColor;


(*--------------------------------------------------------------------------
-- Global function: GetBkColor
-- Description:     Gets the current background color
-- Returns:         Current backgroud color
--------------------------------------------------------------------------*)
PROCEDURE GetBkColor (): LONGCARD;
BEGIN
  RETURN cur_bk_color;
END GetBkColor;


(*--------------------------------------------------------------------------
-- Global function: SetBkMix
-- Description: 
--------------------------------------------------------------------------*)
PROCEDURE SetBkMix (isOpaque: BOOLEAN);
BEGIN
  cur_isOpaque  := isOpaque;
END SetBkMix;


(*/////////////////////////////// graphic primitives /////////////////////////*)

(*--------------------------------------------------------------------------
-- Global function: Plot
-- Description: Set a single pixel to given color
--------------------------------------------------------------------------*)
PROCEDURE Plot_Not_Clip (x, y: LONGCARD; Color: LONGCARD);
BEGIN
  vgagl.gl_setpixel (x, y, Color);
END Plot_Not_Clip;

PROCEDURE Plot_R (x, y: LONGINT; Color: LONGCARD);
BEGIN
  IF (x<=SYSTEM.CAST( LONGINT, cur_clip_region.x2))&
     (x>=SYSTEM.CAST( LONGINT, cur_clip_region.x1))&
     (y<=SYSTEM.CAST( LONGINT, cur_clip_region.y2))&
     (y>=SYSTEM.CAST( LONGINT, cur_clip_region.y1)) THEN
         Plot_Not_Clip (x, y, Color);
  END;
END Plot_R;

PROCEDURE Plot (x, y: LONGCARD; Color: LONGCARD);
BEGIN
  xtsMsMouse.hideCursor;   
  Plot_R(x,y , Color);
  xtsMsMouse.showCursor; 
  Refresh;
END Plot;


(*--------------------------------------------------------------------------
-- Global function: Point
-- Description: Read a pixel of the screen
--------------------------------------------------------------------------*)
PROCEDURE Point (x, y: LONGCARD): LONGCARD;
BEGIN
  RETURN vga.vga_getpixel (x, y);
END Point;


(*--------------------------------------------------------------------------
-- Global function: HLine
-- Description: Draw a horizontal line of given color (used for filling)
--------------------------------------------------------------------------*)
PROCEDURE HLine_Not_Clip_M (x1, y, x2: CARDINAL; FillColor: CARDINAL);
VAR
  i      :CARDINAL;
BEGIN
 FOR i:=x1 TO x2 DO
     IF unpackedMask[y MOD 8, i MOD 8] THEN
          Plot_Not_Clip(i,y, FillColor); 
     ELSE
          IF cur_isOpaque THEN
             Plot_Not_Clip(i,y, cur_bk_color); 
          END;
     END;            
 END;
END HLine_Not_Clip_M;

(*-------------------------------------------------------------------------*)

PROCEDURE HLine_R (x1, y, x2: CARDINAL; FillColor: CARDINAL);
VAR
  xb, xe, i :CARDINAL;
BEGIN
  xb := min (x1, x2);
  xe := max (x1, x2);

  IF (xb <= cur_clip_region.x2) & (xe >= cur_clip_region.x1) & (cur_clip_region.y1 <= y)  & (y <= cur_clip_region.y2) THEN
    (* line (or its part) is seen *)

    xb := max (cur_clip_region.x1, xb);
    xe := min (cur_clip_region.x2, xe);

 
    FOR i:=xb TO xe DO
      Plot_Not_Clip(i,y, FillColor); 
    END;

  END;
END HLine_R;

PROCEDURE HLine (x1, y, x2: CARDINAL; FillColor: CARDINAL);
BEGIN
   xtsMsMouse.hideCursor;   
   HLine_R( x1,y,x2, FillColor );
   xtsMsMouse.showCursor; 
   Refresh;
END HLine;


PROCEDURE HLine_M (x1, y, x2: CARDINAL; FillColor: CARDINAL);
VAR
  xb, xe :CARDINAL;
BEGIN
  xb := min (x1, x2);
  xe := max (x1, x2);

  IF (xb <= cur_clip_region.x2) & (xe >= cur_clip_region.x1) & (cur_clip_region.y1 <= y)  & (y <= cur_clip_region.y2) THEN
    (* line (or its part) is seen *)

    xb := max (cur_clip_region.x1, xb);
    xe := min (cur_clip_region.x2, xe);

    HLine_Not_Clip_M( xb , y , xe , FillColor);
  END;
END HLine_M;


(*--------------------------------------------------------------------------
-- Global function: Line
-- Description: Draw an arbitrary line in given color
--------------------------------------------------------------------------*)
PROCEDURE Line_R (x1, y1, x2, y2: LONGCARD; Color: LONGCARD);
VAR
  a, b        :INTEGER;
  x           :CARDINAL;
  iy          :INTEGER;

  deltaY      :INTEGER;
  addX, addY  :INTEGER;
  approxError :INTEGER;
  m1, m2, m3  :INTEGER;
BEGIN
  a := ABS(INTEGER(x2)-INTEGER(x1));
  b := ABS(INTEGER(y2)-INTEGER(y1));

  addX := b;
  addY := -a;

  IF (x2 < x1) THEN -- swap points
    x  := y2;
    y2 := y1;
    y1 := x;
    --------
    x  := x2;
    x2 := x1;
  ELSE
    x := x1;
  END;

  IF (y2 < y1) THEN
    deltaY := -1;
  ELSE
    deltaY := 1;
  END;
  iy := y1;

  approxError := 0;

  LOOP
    IF (GPar_unpackedStyle[x MOD 24]) THEN
      Plot_R (x, CARDINAL(iy), Color )
    END;

    IF (x = x2) & (CARDINAL(iy) = y2) THEN EXIT END;

    m1 := ABS(approxError-addX);
    m2 := ABS(approxError-addY);
    m3 := ABS(approxError-addX-addY);

    IF (m1>=m2) OR (m1>=m3) THEN
      INC(iy, deltaY);
      DEC(approxError, addY);
    END;

    IF (m2>=m1) OR (m2>=m3) THEN
      INC (x);
      DEC(approxError, addX);
    END;
  END;
END Line_R;

PROCEDURE Line (x1, y1, x2, y2: LONGCARD; Color: LONGCARD);
BEGIN
  xtsMsMouse.hideCursor;   
  Line_R( x1, y1 , x2 , y2 , Color);
  xtsMsMouse.showCursor; 
  Refresh; 
END Line;

PROCEDURE iHLine (x1, x2 :INTEGER; y :INTEGER; color :CARDINAL);
BEGIN
  IF (y < 0) OR (x2 < 0) THEN RETURN END;
  IF (x1 < 0) THEN x1 := 0 END;
  HLine_M (CARDINAL(x1), CARDINAL(x2), CARDINAL(y), color);
END iHLine;


(*--------------------------------------------------------------------------
-- Global function: Ellipse
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE Ellipse (x0, y0: LONGCARD;   -- center
                   a, b  : LONGCARD;   -- semi-axes
                   c     : LONGCARD;   -- color
                   fill  : BOOLEAN);   -- whether filled
VAR
  a2, b2           :INTEGER;
  ix0, iy0, ix, iy :INTEGER;
  addX, addY       :INTEGER;
  approxError      :INTEGER;
  m1, m2, m3       :INTEGER;
BEGIN
  xtsMsMouse.hideCursor;   
  a2 := a*a;
  b2 := b*b;
  addX := b2;
  addY := a2*(1 - 2*INTEGER(b));

  a2 := a2*2;
  b2 := b2*2;

  ix0 := x0;
  iy0 := y0;
  ix  := 0;
  iy  := b;
 
--  stdio.printf( "\n a-b %d  %d %d %d \n", x0, y0, a, b);

  approxError := 0;
  LOOP
    IF (fill) THEN
      iHLine (ix0-ix, iy0+iy,ix0+ix, c);
      iHLine (ix0-ix, iy0-iy,ix0+ix, c);
    ELSE
--      stdio.printf( " %d  %d %d %d \n", ix0-ix, ix0+ix, iy0+iy, iy0-iy);
      Plot_R(ix0-ix, iy0+iy, c);
      Plot_R(ix0+ix, iy0+iy, c);
      Plot_R(ix0-ix, iy0-iy, c);
      Plot_R(ix0+ix, iy0-iy, c);
    END;

    IF (iy = 0) & (ix = INTEGER(a)) THEN EXIT END;

    m1 := ABS(approxError-addX);
    m2 := ABS(approxError-addY);
    m3 := ABS(approxError-addX-addY);

    IF (m1>=m2) OR (m1>=m3) THEN
      DEC(iy);
      DEC(approxError, addY);
      INC(addY, a2);
    END;

    IF (m2>=m1) OR (m2>=m3) THEN
      INC (ix);
      DEC(approxError, addX);
      INC(addX, b2);
    END;
  END;
  xtsMsMouse.showCursor; 
  Refresh;
END Ellipse;


(*--------------------------------------------------------------------------
-- Global function: Disc
-- Description: filled circle, centre x0,y0; radius r
--------------------------------------------------------------------------*)
PROCEDURE Disc (x0, y0, r, c: LONGCARD);
BEGIN
  Ellipse ( x0, y0, r, r, c, TRUE );
END Disc;


(*--------------------------------------------------------------------------
-- Global function: Circle
-- Description: centre x0,y0; radius r
--------------------------------------------------------------------------*)
PROCEDURE Circle (x0, y0, r, c: LONGCARD);
BEGIN
  Ellipse ( x0, y0, r, r, c, FALSE );
END Circle;


(*--------------------------------------------------------------------------
-- Global function: Rectangle
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE Rectangle (x0, y0, x1, y1: LONGCARD; -- coordinates
                     Color         : LONGCARD; -- rectangle color
                     Fill          : BOOLEAN); -- rectangle filling toggle
VAR
  c: INTEGER;
  i: CARDINAL;
BEGIN
  xtsMsMouse.hideCursor;   
  c := Color;
  IF Fill THEN
    IF (x1>cur_clip_region.x2)  THEN x1 := cur_clip_region.x2; END;
    IF (x0<cur_clip_region.x1)  THEN x0 := cur_clip_region.x1; END;
    IF (y1>cur_clip_region.y2)  THEN y1 := cur_clip_region.y2; END;
    IF (y0<cur_clip_region.y1)  THEN y0 := cur_clip_region.y1; END;
    FOR i := y0 TO y1 DO
      HLine_Not_Clip_M (x0, i, x1, c);
    END;
  ELSE
    HLine_R (x0, y0, x1, c);
    HLine_R (x0, y1, x1, c);
    Line_R (x0, y0, x0, y1, c);
    Line_R (x1, y0, x1, y1, c);
  END;
  xtsMsMouse.showCursor; 
  Refresh;
END Rectangle;


(*--------------------------------------------------------------------------
-- Global function: SetClipRgn
-- Description: enable clipping to given region
--------------------------------------------------------------------------*)
PROCEDURE SetClipRgn (ax1, ay1, ax2, ay2: LONGCARD);
BEGIN
  orderCoords (ax1, ay1, ax2, ay2);

  IF ( ax1>SYSTEM.CAST(LONGCARD, cur_mode_info^.width) ) AND 
     ( ay1>SYSTEM.CAST(LONGCARD, cur_mode_info^.height) ) THEN
    adjust (ax2, 0, cur_mode_info^.width  ); 
    adjust (ay2, 0, cur_mode_info^.height );

    WITH cur_clip_region DO
        x1 := ax1; y1 := ay1;
        x2 := ax2; y2 := ay2;
    END;
  ELSE
    (* wrong region was passed *)
  
  END;
END SetClipRgn;


(*--------------------------------------------------------------------------
-- Global function: CancelClipRgn
-- Description: disable clipping region
--------------------------------------------------------------------------*)
PROCEDURE CancelClipRgn;
BEGIN
  WITH cur_clip_region DO
    x1 := 0; y1 := 0;
    x2 := cur_mode_info^.width - 1;
    y2 := cur_mode_info^.height - 1;
  END;
END CancelClipRgn;


(*--------------------------------------------------------------------------
-- Global function: Polygon
-- Description: polygon of n points
--------------------------------------------------------------------------*)

CONST
  MaxPts = 20;
VAR
  xord :ARRAY [0..MaxPts] OF CARDINAL;
  x    :ARRAY [0..MaxPts] OF CARDINAL;

PROCEDURE QuickSort(l,r: INTEGER);
VAR
  i,j,temp :INTEGER;
  key      :CARDINAL;
BEGIN
  WHILE ( l < r ) DO
    i := l; j := r; key := x[xord[j]];
    REPEAT
      WHILE ( i < j ) AND ( x[xord[i]] <= key ) DO i := i + 1 END;
      WHILE ( i < j ) AND ( key <= x[xord[j]] ) DO j := j - 1 END;
      IF i < j THEN
        temp := xord[i]; xord[i] := xord[j]; xord[j] := temp;
      END;
    UNTIL ( i >= j );
    temp := xord[i]; xord[i] := xord[r]; xord[r] := temp;
    IF (i-l < r-i) THEN
      QuickSort( l, i-1 ); l := i+1;
    ELSE
      QuickSort( i+1, r ); r := i-1;
    END;
  END;

END QuickSort;


PROCEDURE Polygon( n      :CARDINAL;
                   px, py :ARRAY OF CARDINAL;
                   color  :CARDINAL;
                   fill   :BOOLEAN);
VAR
  y, miny, maxy, x0, y0, x1, y1    :INTEGER;
  temp, i, edge, next_edge, active :INTEGER;
  e                                :ARRAY [0..MaxPts] OF INTEGER;
  plotl, plotr                     :INTEGER;
  --Mask                             :CARDINAL;
BEGIN
  xtsMsMouse.hideCursor;   
  IF (n > MaxPts) THEN RETURN END;
  i:=0;
  WHILE (i < INTEGER(n)) DO
    IF (i < INTEGER(n-1)) THEN
      Line_R (px[i], py[i], px[i+1], py[i+1], color);
    ELSE
      Line_R (px[i], py[i], px[0], py[0], color);
    END;
    INC(i);
  END;

  IF (~fill) THEN
    RETURN;
  END;

  miny:=py[0];                      (* find extremal y points *)
  maxy:=miny;
  i:=0;
  WHILE i < INTEGER(n) DO
    IF INTEGER(py[i]) < miny THEN
        miny:=py[i];
    END;
    IF INTEGER(py[i]) > maxy THEN
        maxy:=py[i];
    END;
    INC(i);
  END;
  y:=miny;
  WHILE y <= maxy DO
      active:=-1;
      edge:= 0;
      WHILE edge < INTEGER(n) DO
          IF edge = INTEGER(n-1) THEN
              next_edge:=0;
          ELSE
              next_edge:=edge+1;
          END;
          x0:=px[edge];
          y0:=py[edge];
          x1:=px[next_edge];
          y1:=py[next_edge];
          IF y0 > y1 THEN
              temp:=x0;
              x0:=x1;
              x1:=temp;
              temp:=y0;
              y0:=y1;
              y1:=temp;
          END;
          IF y = y0 THEN
              e[edge]:=0;
              x[edge]:=x0;
          ELSIF (y0 <= y) AND (y <= y1) THEN
              IF x1 >= x0 THEN (* x increases with y *)
                  INC(e[edge], (2*(x1-x0)));
                  WHILE e[edge] > (y1-y0) DO
                      DEC(e[edge], (2*(y1-y0)));
                      INC(x[edge]);
                  END;
              ELSE (* x decreases with y *)
                  INC(e[edge], (2*(x0-x1)));
                  WHILE e[edge] > (y1-y0) DO
                      DEC(e[edge], (2*(y1-y0)));
                      DEC(x[edge]);
                  END;
              END;
              INC(active);
              xord[active]:=edge;
          END;
          INC(edge);
      END;
      QuickSort(0, active);
      i:=0;
      WHILE i < active DO
          plotl:=x[xord[i]]+1;
          plotr:=x[xord[i+1]]-1;
          IF (plotr >= plotl) THEN
            HLine_R (plotl, y, plotr, color);
          END;
          INC(i, 2);
      END;
      INC(y);
  END; (* for y = .. *)
  xtsMsMouse.showCursor; 
  Refresh;
END Polygon;


(*--------------------------------------------------------------------------
-- Global function: Cube
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE Cube (top                  : BOOLEAN;  -- top switch
                x1, y1, x2, y2, depth: LONGCARD; -- cube parameters
                Color                : LONGCARD; -- fill color
                Fill                 : BOOLEAN); -- fill switch
VAR
  px, py  :ARRAY [0..3] OF LONGCARD;
  height  :LONGCARD;
BEGIN
  orderCoords (x1, y1, x2, y2);

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
  IF (top) THEN
    px[0] := x1;
    py[0] := y1;
    px[1] := x1 + depth;
    py[1]:=y1 - (depth DIV 2);
    DEC( px[2] );
    DEC( px[3] );
    Polygon(4, px, py, Color, Fill );
  END;
  Rectangle (x1, y1, x2, y2, Color, Fill);
END Cube;


(*--------------------------------------------------------------------------
-- Global function: FloodFill
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE FloodFill (x, y,                -- flood fill start coordinates
                     color,               -- flood fill color
                     Boundary: LONGCARD); -- boundary color
CONST
  deadProc = 0;    -- kill code
  numProc  = 4096;

VAR
  xl, xr, nxl, nxr :CARDINAL;
  xt               :CARDINAL;
  iy               :INTEGER;
  direction        :INTEGER;

  --------------------

  (* PSEUDOMODULE Q; *)

  VAR
    q_xl, q_xr   :ARRAY [0..numProc-1] OF CARDINAL;
    q_y,  q_dir  :ARRAY [0..numProc-1] OF INTEGER;
    qHead, qTail :CARDINAL;
    numOfRecs    :CARDINAL; -- # of elements put in queue

  PROCEDURE Q_init();
  BEGIN
    qHead     := 0;
    qTail     := 0;
    numOfRecs := 0;
  END Q_init;

  PROCEDURE Q_put (xl, xr :CARDINAL; y :INTEGER; dir :INTEGER);
  BEGIN
    ASSERT (numOfRecs < numProc);
    q_xl  [qHead]  := xl;
    q_xr  [qHead]  := xr;
    q_y   [qHead]  := y;
    q_dir [qHead]  := dir;
    qHead          := (qHead+1) MOD numProc;
    INC (numOfRecs);
  END Q_put;

  PROCEDURE Q_next();
  BEGIN
    ASSERT (numOfRecs > 0);
    qTail := (qTail+1) MOD numProc;
    DEC (numOfRecs);
  END Q_next;

  PROCEDURE Q_isEmpty() :BOOLEAN;
  BEGIN
    RETURN (numOfRecs = 0);
  END Q_isEmpty;

  (* END Q *)
  ----------------------------------

  PROCEDURE DrawAndScan();

    (*
       Out: IF (searchOnly) THEN
              Is the process in the queue?
            ELSE
              Is the process alive?
       Act: kill all queue clones of the current process
    *)
    PROCEDURE killThemAll( xl, xr        :CARDINAL;
                           iy, direction :INTEGER;
                           searchOnly    :BOOLEAN
                          ) :BOOLEAN;
    VAR
      p     :INTEGER;
      i     :CARDINAL;
      alive :BOOLEAN;
    BEGIN
      alive := TRUE;
      (* iterates all the queue *)
      p := qTail;
      FOR i:=1 TO numOfRecs DO
        IF (q_y[p] = iy) &                 -- process is on the current line
           (q_dir[p] # deadProc) &         -- it is alive
           (q_xr[p]=xr) & (q_xl[p] = xl)
            THEN
            IF (searchOnly) & (q_dir[p] = direction) THEN
              RETURN TRUE;
            ELSE
              alive := alive & (q_dir[p] = direction);
              q_dir[p] := deadProc         -- kill it
            END;
        END;
        p := (p+1) MOD numProc;
      END;
      IF (searchOnly) THEN
        RETURN FALSE;
      ELSE
        RETURN alive;
      END;
    END killThemAll;

    PROCEDURE search4Holes (xl, xr :CARDINAL; y :INTEGER; direction :INTEGER);
    VAR
      curX       :CARDINAL;  -- current X iterated in [xl..xr]
      expandLine :BOOLEAN;
      startX     :CARDINAL;  -- left edge of an expanding line
    BEGIN
      curX := xl;

      expandLine := (Point(curX, y) # Boundary);
      IF (expandLine) THEN startX := curX END;

      WHILE (curX <= xr) DO
        IF (Point(curX, y) # Boundary) THEN
          IF (~expandLine) THEN
            startX     := curX;
            expandLine := TRUE;
          END;
        ELSE
          IF (expandLine) THEN
            Q_put (startX, curX-1, y, direction);
            expandLine := FALSE;
          END;
        END;
        INC (curX);
      END;

      IF (expandLine) THEN
        Q_put (startX, xr, y, direction);
      END;
    END search4Holes;

  BEGIN
    HLine_R (xl, iy, xr,  color); 

    IF ((iy+direction) < INTEGER(cur_clip_region.y1)) OR
       ((iy+direction) > INTEGER(cur_clip_region.y2)) THEN
      RETURN
    END;

    (* check for process enlargement *)
    nxl := xl;
    nxr := xr;
    IF (Point(nxl, iy+direction) # Boundary) THEN
      lScanHLine (nxl, iy+direction, Boundary);
    END;
    IF (Point(nxr, iy+direction) # Boundary) THEN
      rScanHLine (nxr, iy+direction, Boundary);
    END;

    (* left enlargment happens *)
    IF (nxl < xl) THEN
       xt := nxl;
       scanHLine (nxl, xt, iy+direction, Boundary);
       IF ~killThemAll (nxl, xt, iy+direction, direction, TRUE) THEN
         search4Holes  (nxl, xl-1, iy, -direction)
       END;
    END;

    (* right enlargment happens *)
    IF (nxr > xr) THEN
      xt := nxr;
      scanHLine (xt, nxr, iy+direction, Boundary);
      IF ~killThemAll (xt, nxr, iy+direction, direction, TRUE) THEN
        search4Holes  (xr+1, nxr, iy, -direction)
      END;
    END;

    (* NOTE: this search should be performed only after above ones *)
    IF killThemAll(xl, xr, iy, direction, FALSE) THEN
      search4Holes  (nxl, nxr, iy+direction, direction);
    END;
  END DrawAndScan;

BEGIN
  xtsMsMouse.hideCursor;   
  IF (Boundary > CARDINAL (cur_mode_info^.colors - 1)) THEN
    Boundary := cur_mode_info^.colors - 1
  END;

  IF (x > cur_clip_region.x2) OR (x < cur_clip_region.x1) THEN RETURN END;
  IF (y > cur_clip_region.y2) OR (y < cur_clip_region.y1) THEN RETURN END;
  IF (Point(x, y) = Boundary) THEN RETURN END;

  Q_init();
  xl := x;
  xr := x;
  iy := y;

  scanHLine (xl, xr, iy, Boundary);

  direction := -1; DrawAndScan();
  direction := +1; DrawAndScan();

  LOOP
    (* get first alive process *)
    REPEAT
      IF Q_isEmpty() THEN Refresh; xtsMsMouse.showCursor; RETURN END;

      direction := q_dir[qTail];
      IF (direction # deadProc) THEN
        xr := q_xr[qTail];
        xl := q_xl[qTail];
        iy := q_y [qTail];
      END;
      Q_next();
    UNTIL (direction # deadProc);

    DrawAndScan();
  END; (* LOOP *)
END FloodFill;

(*--------------------------------------------------------------------------
-- Global function: StackFill
-- Description:
--------------------------------------------------------------------------*)


PROCEDURE StackFill (x, y :CARDINAL; Color :CARDINAL; Boundary :CARDINAL);

VAR
  xl, xr :CARDINAL;

  PROCEDURE fillHalf (direction :INTEGER);
  VAR
    nxl, nxr :CARDINAL;
    ny       :INTEGER;
  BEGIN
    nxl := INTEGER(xl);
    nxr := INTEGER(xr);
    ny  := INTEGER(y);

    LOOP
      INC(ny, direction);
      IF (ny > INTEGER(cur_clip_region.y2)) OR (ny < INTEGER(cur_clip_region.y1)) THEN
        RETURN
      END;

      WHILE (nxl <= nxr) & (Point(nxl, CARDINAL(ny)) = Boundary) DO
        INC(nxl)
      END;

      IF (nxl > nxr) THEN RETURN END;

      nxr := nxl;
      scanHLine (nxl, nxr, ny, Boundary);
      iHLine (nxl, nxr, ny, Color);
    END;
  END fillHalf;

BEGIN
  xtsMsMouse.hideCursor;   
  IF (Boundary > SYSTEM.CAST(LONGCARD, cur_mode_info^.colors-1)) THEN 
       Boundary := SYSTEM.CAST(LONGCARD, cur_mode_info^.colors-1)
  END;

  IF (cur_clip_region.x1 <= x) & (x <= cur_clip_region.x2) &
     (cur_clip_region.y1 <= y) & (y <= cur_clip_region.y2) &
     (Point(x, y) # Boundary) THEN

    xr := x;
    xl := x;
    scanHLine (xl, xr, y, Boundary);
    HLine_R (xl, xr, y, Color);

    fillHalf (+1);
    fillHalf (-1);
  END;
  xtsMsMouse.showCursor; 
  Refresh;
END StackFill;


(*--------------------------------------------------------------------------
-- Global function: Arc
-- Description:
--------------------------------------------------------------------------*)
TYPE
  qModes = (
            none,    -- no plots in the quarter
            edge,    -- starts/ends with the quarter's edge
            vector,  -- starts with the "v" vector/ends with the "w" vector
            twin     -- include 3 full quarters & two parts of the quarter
           );

  qDirT  = ARRAY [0..3] OF INTEGER;

VAR
  v1, v2, w1, w2     :INTEGER;
  vMult, wMult       :INTEGER;
  vdx, vdy, wdx, wdy :INTEGER;

  qBMode, qEMode     :ARRAY [0..3] OF qModes;
  bQ, eQ, q          :CARDINAL;

CONST
  xQDir = qDirT{+1,-1,-1,+1};
  yQDir = qDirT{-1,-1,+1,+1};

  PROCEDURE getQ (x, y :INTEGER) :CARDINAL;
  TYPE
    arqNT = ARRAY[0..3] OF CARDINAL;
  CONST
    arqN = arqNT{2,1,3,0};
  BEGIN
    RETURN arqN[ORD(x>0)*2+ORD(y>0)];
  END getQ;

  PROCEDURE fillQs (what :qModes);
  VAR
    i :CARDINAL;
  BEGIN
    FOR i:=0 TO 3 DO qBMode[i] := what; qEMode[i] := what END;
  END fillQs;

PROCEDURE Arc (x0, y0,           -- 1-st coordinates
               a, b,             -- arc half axes
               x3, y3,           -- 3-rd coordinates
               x4, y4,           -- 4-th coordinates
               color: CARDINAL); -- arc color

VAR
  a2, b2             :INTEGER;
  ix0, iy0, ix, iy   :INTEGER;
  addX, addY         :INTEGER;
  approxError        :INTEGER;
  m1, m2, m3         :INTEGER;

  PROCEDURE arcPlot(dx, dy :INTEGER; q :CARDINAL);
  BEGIN
    IF (qBMode[q] = none) THEN
      ASSERT (qEMode[q] = none);
      RETURN;
    END;

    IF (qBMode[q] = vector) OR (qBMode[q] = twin) THEN
      ASSERT(vMult = -v2*dx+v1*dy);
    END;
    IF (qEMode[q] = vector) OR (qEMode[q] = twin) THEN
      ASSERT(wMult = -w2*dx+w1*dy);
    END;

    IF (qBMode[q] = twin) THEN
      IF (wMult >= 0) OR (vMult <= 0) THEN
        Plot_R(ix0+dx, iy0+dy, color);
      END;
    ELSE
      IF ((qBMode[q] = edge) OR (vMult <=0)) &
         ((qEMode[q] = edge) OR (wMult >=0))  THEN
        Plot_R(ix0+dx, iy0+dy, color);
      END;
    END;
  END arcPlot;


BEGIN
  xtsMsMouse.hideCursor;   
  IF (x3*y4-x4*y3) = 0   THEN
      Ellipse( x0,y0,a,b, color, FALSE);
      RETURN;      
  END;

  v1 := INTEGER(x3)-INTEGER(x0); v2 := INTEGER(y3)-INTEGER(y0);
  w1 := INTEGER(x4)-INTEGER(x0); w2 := INTEGER(y4)-INTEGER(y0);

  a2   := a*a;
  b2   := b*b;
  addX := b2;
  addY := a2*(1 - 2*INTEGER(b));
  a2   := a2*2;
  b2   := b2*2;

  ix0 := x0;
  iy0 := y0;
  ix  := 0;
  iy  := b;

  fillQs (none);
  bQ := getQ (v1,v2);
  eQ := getQ (w1,w2);

  IF (bQ = eQ) & (-w2*v1+w1*v2 < 0) THEN
    fillQs (edge);
    qBMode[bQ] := twin;
    qEMode[bQ] := twin;
  ELSE
    q := bQ;
    LOOP
      qBMode[q] := edge;
      qEMode[q] := edge;
      IF (q = eQ) THEN EXIT END;

      IF (q = 0) THEN q := 3 ELSE q := q - 1 END;
    END;
    qBMode[bQ] := vector;
    qEMode[eQ] := vector;
  END;

  vMult := -yQDir[bQ]*v1*iy;
  vdx   := -v2*xQDir[bQ];
  vdy   :=  v1*yQDir[bQ];

  wMult := -yQDir[eQ]*w1*iy;
  wdx   := -w2*xQDir[eQ];
  wdy   :=  w1*yQDir[eQ];

  approxError := 0;
  LOOP
    arcPlot(+ix, +iy, 0);
    arcPlot(-ix, +iy, 1);
    arcPlot(-ix, -iy, 2);
    arcPlot(+ix, -iy, 3);

    IF (iy = 0) & (ix = INTEGER(a)) THEN EXIT END;

    m1 := ABS(approxError-addX);
    m2 := ABS(approxError-addY);
    m3 := ABS(approxError-addX-addY);

    IF (m1>=m2) OR (m1>=m3) THEN
      DEC(iy);
      DEC(approxError, addY);
      INC(addY, a2);
      INC(vMult, vdy);
      INC(wMult, wdy);
    END;

    IF (m2>=m1) OR (m2>=m3) THEN
      INC (ix);
      DEC(approxError, addX);
      INC(addX, b2);
      INC(vMult, vdx);
      INC(wMult, wdx);
    END;
  END;
  xtsMsMouse.showCursor; 
  Refresh;
END Arc;


(*--------------------------------------------------------------------------
-- Global function: Pie
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE Pie (x0, y0,           -- 1-st coordinates
               a, b,             -- pie half axes
               ax3, ay3,           -- 3-rd coordinates
               ax4, ay4,           -- 4-th coordinates
               color: CARDINAL;  -- pie color
               Fill: BOOLEAN);   -- fill indicator
VAR

  a2, b2             :INTEGER;
  ix0, iy0, ix, iy   :INTEGER;
  addX, addY         :INTEGER;
  approxError        :INTEGER;
  m1, m2, m3         :INTEGER;

  x3,x4,y3,y4        :CARDINAL;

  px, py             :ARRAY [0..3] OF INTEGER;
  e,x                :ARRAY [0..1] OF ARRAY [0..1] OF INTEGER;
  active             :ARRAY [0..1] OF INTEGER;
  
  i                  :CARDINAL;
  oldiy              :INTEGER;

  xord               :ARRAY [0..1] OF CARDINAL;

    PROCEDURE GetVec( x0,y0, a, b, vx, vy: INTEGER; VAR parx,pary : CARDINAL);
    VAR
      flag                   :[0..3];
      res, last_difference   :INTEGER; 
      qx,qy,px,py            :INTEGER;

    BEGIN
         a2 := a*a;
         b2 := b*b;
         addX := b2;
         addY := a2*(1 - 2*INTEGER(b));
       
         a2 := a2*2;
         b2 := b2*2;
       
         ix0 := x0;
         iy0 := y0;
         ix  := 0;
         iy  := b;

         qx := vx-x0;
         qy := vy-y0;

         IF qy>=0 THEN
            IF qx >= 0 THEN
                flag := 0;
            ELSE flag := 1; END;
         ELSE
            IF qx >= 0 THEN
                flag := 2;
            ELSE flag := 3; END;
         END;

         approxError := 0;
         last_difference := MAX (INTEGER);
         LOOP
       
           CASE flag OF
             | 0: res := ix*qy - iy*qx; px:=x0+ix; py:=y0+iy; 
             | 1: res :=-ix*qy - iy*qx; px:=x0-ix; py:=y0+iy;
             | 2: res := ix*qy + iy*qx; px:=x0+ix; py:=y0-iy;
             | 3: res :=-ix*qy + iy*qx; px:=x0-ix; py:=y0-iy;
            ELSE ;
           END;

           IF res < 0 THEN
              res:=-res;
           END;
           IF res > last_difference THEN
               RETURN 
           END;

           last_difference:=res;
           parx:=px;
           pary:=py;

           IF (iy = 0) & (ix = INTEGER(a)) THEN EXIT END;
       
           m1 := ABS(approxError-addX);
           m2 := ABS(approxError-addY);
           m3 := ABS(approxError-addX-addY);
       
           IF (m1>=m2) OR (m1>=m3) THEN
             DEC(iy);
             DEC(approxError, addY);
             INC(addY, a2);
           END;
       
           IF (m2>=m1) OR (m2>=m3) THEN
             INC (ix);
             DEC(approxError, addX);
             INC(addX, b2);
           END;
         END;

    END GetVec;

  PROCEDURE arcPlot(dx, dy :INTEGER; q :CARDINAL):BOOLEAN;
  BEGIN
    IF (qBMode[q] = none) THEN
      ASSERT (qEMode[q] = none);
      RETURN FALSE;
    END;

    IF (qBMode[q] = vector) OR (qBMode[q] = twin) THEN
      ASSERT(vMult = -v2*dx+v1*dy);
    END;
    IF (qEMode[q] = vector) OR (qEMode[q] = twin) THEN
      ASSERT(wMult = -w2*dx+w1*dy);
    END;

    IF (qBMode[q] = twin) THEN
      IF (wMult >= 0) OR (vMult <= 0) THEN
          RETURN TRUE;
      END;
    ELSE
      IF ((qBMode[q] = edge) OR (vMult <=0)) &
         ((qEMode[q] = edge) OR (wMult >=0))  THEN
          RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END arcPlot;

BEGIN
  xtsMsMouse.hideCursor;   

  GetVec( x0, y0, a, b, ax3,ay3,x3,y3 );
  GetVec( x0, y0, a, b, ax4,ay4,x4,y4 );

  IF (x3=x4) & (y3=y4) THEN
    Ellipse( x0, y0, a, b, color, Fill);
    RETURN;
  END;

  IF y3>y0 THEN
     px[0] := x0;
 py[0] := y0;
     px[2] := x3; py[2] := y3;
  ELSE
     px[0] := x3; py[0] := y3;
     px[2] := x0; py[2] := y0;
  END;

  IF y4>y0 THEN
     px[1] := x0; py[1] := y0;
     px[3] := x4; py[3] := y4;

  ELSE
     px[1] := x4;
 py[1] := y4;
     px[3] := x0; py[3] := y0;
  END;

  v1 := INTEGER(x3)-INTEGER(x0); v2 := INTEGER(y3)-INTEGER(y0);
  w1 := INTEGER(x4)-INTEGER(x0); w2 := INTEGER(y4)-INTEGER(y0);

  a2   := a*a;
  b2   := b*b;
  addX := b2;
  addY := a2*(1 - 2*INTEGER(b));
  a2   := a2*2;
  b2   := b2*2;

  ix0 := x0;
  iy0 := y0;
  ix  := 0;
  iy  := b;

  fillQs (none);
  bQ := getQ (v1,v2);
  eQ := getQ (w1,w2);

  IF (bQ = eQ) & (-w2*v1+w1*v2 < 0) THEN
    fillQs (edge);
    qBMode[bQ] := twin;
    qEMode[bQ] := twin;
  ELSE
    q := bQ;
    LOOP
      qBMode[q] := edge;
      qEMode[q] := edge;
      IF (q = eQ) THEN EXIT END;

      IF (q = 0) THEN q := 3 ELSE q := q - 1 END;
    END;
    qBMode[bQ] := vector;
    qEMode[eQ] := vector;
  END;

  vMult := -yQDir[bQ]*v1*iy;
  vdx   := -v2*xQDir[bQ];
  vdy   :=  v1*yQDir[bQ];

  wMult := -yQDir[eQ]*w1*iy;
  wdx   := -w2*xQDir[eQ];
  wdy   :=  w1*yQDir[eQ];

  approxError := 0;

  oldiy := -1;

  LOOP
    IF oldiy # iy THEN 

    active[0] := -1;
    active[1] := -1;

    FOR i:=0 TO 1 DO 
      IF iy0-iy = py[i] THEN
          e[0,i] := 0;
          x[0,i] := px[i];
          INC( active[0] );
          xord[active[0]] := i;
      ELSIF (iy0-iy>py[i]) AND (iy0-iy<=py[i+2]) THEN
          IF  px[i+2]>=px[i] THEN  (* x increases with y *)
             INC( e[0,i], 2*(px[i+2]-px[i]) );
             WHILE e[0,i]>(py[i+2]-py[i]) DO
                DEC( e[0,i], 2*(py[i+2]-py[i]) );
                INC( x[0,i] );
             END;
          ELSE  (* x decreases with y *)
             INC( e[0,i], 2*(px[i]-px[i+2]) );
             WHILE e[0,i]>(py[i+2]-py[i]) DO
                DEC( e[0,i], 2*(py[i+2]-py[i]) );
                DEC( x[0,i] );
             END;
          END;
          INC( active[0] );
          xord[active[0]] := i;
      END;
    END; (* for 1 to 2 *)

    CASE active[0] OF
     |-1 : 
           IF arcPlot(-ix, -iy, 2) THEN
                iHLine( ix0-ix, ix0+ix, iy0-iy,  color );
           END;
     |0 : 
          IF xord[0]=1 THEN
                iHLine( x[0,1], ix0+ix, iy0-iy,color);
          ELSE
                iHLine( ix0-ix, x[0,0], iy0-iy,color);
          END;
     |1 :
           IF x[0,0] < x[0,1] THEN
             iHLine( ix0-ix, x[0,0], iy0-iy, color);
             iHLine( x[0,1], ix0+ix, iy0-iy, color);
           ELSE
             iHLine( x[0,0], x[0,1], iy0-iy, color);
           END;
     ELSE;
    END;


    FOR i:=0 TO 1 DO 
      IF iy0+iy = py[i+2] THEN
          e[1,i] := 0;
          x[1,i] := px[i+2];
          INC( active[1] );
          xord[active[1]] := i;
      ELSIF (iy0+iy>py[i]) AND (iy0+iy<=py[i+2]) THEN
          IF  px[i+2]>=px[i] THEN  (* x increases with y *)
             INC( e[1,i], 2*(px[i+2]-px[i]) );
             WHILE e[1,i]>(py[i+2]-py[i]) DO
                DEC( e[1,i], 2*(py[i+2]-py[i]) );
                DEC( x[1,i] );
             END;
          ELSE  (* x decreases with y *)
             INC( e[1,i], 2*(px[i]-px[i+2]) );
             WHILE e[1,i]>(py[i+2]-py[i]) DO
                DEC( e[1,i], 2*(py[i+2]-py[i]) );
                INC( x[1,i] );
             END;
          END;
          INC( active[1] );
          xord[active[1]] := i;
      END;
    END; (* for 1 to 2 *)

    CASE active[1] OF
     |-1 : 
           IF arcPlot(-ix, +iy, 1) THEN
               iHLine( ix0-ix, ix0+ix, iy0+iy,  color );
           END;
     |0 : 
          IF xord[0]=0 THEN
                iHLine( x[1,0], ix0+ix, iy0+iy,color);
          ELSE
                iHLine( ix0-ix, x[1,1], iy0+iy,color);
          END;
     |1 :
           IF x[1,0] > x[1,1] THEN
             iHLine( ix0-ix, x[1,1], iy0+iy, color);
             iHLine( x[1,0], ix0+ix, iy0+iy, color);
           ELSE
             iHLine( x[1,1], x[1,0], iy0+iy, color);
           END;
     ELSE;
    END;
   END;

    IF (iy = 0) & (ix = INTEGER(a)) THEN EXIT END;

    m1 := ABS(approxError-addX);
    m2 := ABS(approxError-addY);
    m3 := ABS(approxError-addX-addY);

    oldiy := iy;
    IF (m1>=m2) OR (m1>=m3) THEN
      DEC(iy);
      DEC(approxError, addY);
      INC(addY, a2);
      INC(vMult, vdy);
      INC(wMult, wdy);
    END;

    IF (m2>=m1) OR (m2>=m3) THEN
      INC (ix);
      DEC(approxError, addX);
      INC(addX, b2);
      INC(vMult, vdx);
      INC(wMult, wdx);
    END;

    IF (iy = 0) & (ix = INTEGER(a)) THEN EXIT END;

  END;
  IF (py[0] < iy0 )AND(py[1] < iy0 ) THEN
     IF px[0] < px[1] THEN
          iHLine( ix0-INTEGER(a), ix0+INTEGER(a), iy0, color);
     END;
  ELSIF (py[0] > iy0 )AND(py[1] > iy0 ) THEN
     IF px[0] > px[1] THEN
          iHLine( ix0-INTEGER(a), ix0+INTEGER(a), iy0, color);
     END;
  ELSE
     IF py[0] < iy0 THEN
         iHLine( ix0-INTEGER(a), ix0, iy0, color );
     ELSE
         iHLine( ix0, ix0+INTEGER(a), iy0, color );
     END;
  END;
  xtsMsMouse.showCursor; 
  Refresh;
END Pie;


(*////////////////////////// text procedures ////////////////////////////*)

(*--------------------------------------------------------------------------
-- Global function: SetTextPosition
-- Description: get current text position
-- Returns: current text pointer
--------------------------------------------------------------------------*)
PROCEDURE SetTextPosition (row, col :LONGCARD) :TextCoords;
VAR
  prev: TextCoords;
BEGIN

  adjust ( col, cur_text_window.x1, cur_text_window.x2);
  adjust ( row, cur_text_window.y1, cur_text_window.y2);

  prev := cur_text_coords;
  cur_text_coords.row := row+cur_text_window.y1-1;
  cur_text_coords.col := col+cur_text_window.x1-1;
  RETURN prev;
END SetTextPosition;


(*--------------------------------------------------------------------------
-- Global function: GetTextPosition
-- Description: get current text position
-- Returns: current text pointer
--------------------------------------------------------------------------*)
PROCEDURE GetTextPosition (): TextCoords;
VAR
  ret  : TextCoords;
BEGIN
  ret.row := cur_text_coords.row-cur_text_window.y1+1;
  ret.col := cur_text_coords.col-cur_text_window.x1+1;
  RETURN ret;
END GetTextPosition;


(*--------------------------------------------------------------------------
-- Global function: GetTextColumn
-- Description: get current text column
-- Returns: current text column
--------------------------------------------------------------------------*)
PROCEDURE GetTextColumn (): LONGCARD;
BEGIN
  RETURN cur_text_coords.col-cur_text_window.x1+1;
END GetTextColumn;


(*--------------------------------------------------------------------------
-- Global function: GetTextRow
-- Description: get current text row
-- Returns: current text row
--------------------------------------------------------------------------*)
PROCEDURE GetTextRow (): LONGCARD;
BEGIN
  RETURN cur_text_coords.row-cur_text_window.y1+1;
END GetTextRow;


(*--------------------------------------------------------------------------
-- Global function: GetTextColor
-- Description: get current text color
-- Returns: current text color
--------------------------------------------------------------------------*)
PROCEDURE GetTextColor(): LONGCARD;
BEGIN
  RETURN cur_text_color;
END GetTextColor;


(*--------------------------------------------------------------------------
-- Global function: SetTextColor
-- Description: set current text color
-- Returns: previous text color value
--------------------------------------------------------------------------*)
PROCEDURE SetTextColor (Color: LONGCARD): LONGCARD;
VAR
  prev: LONGCARD;
BEGIN
  prev := cur_text_color;
  vgagl.gl_setfontcolors( cur_bk_color , Color );
  cur_text_color := Color;
  RETURN prev;
END SetTextColor;


(*--------------------------------------------------------------------------
-- Global function: SetTextWindow
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE SetTextWindow (r1, c1, r2, c2: LONGCARD);
VAR
  ignore     :TextCoords; 
BEGIN

 adjust ( c1, 1, cur_mode_info^.width DIV FONT_WIDTH );
 adjust ( c2, 1, cur_mode_info^.width DIV FONT_WIDTH );
 adjust ( r1, 1, cur_mode_info^.width DIV FONT_HEIGHT );
 adjust ( r2, 1, cur_mode_info^.width DIV FONT_HEIGHT );

 WITH cur_text_window DO
   x1 := min( c1 ,c2 )-1;
   x2 := max( c1 ,c2 )-1;
   y1 := min( r1, r2 )-1;
   y2 := max( r1, r2 )-1;

   Columns := x2-x1;
   Rows    := y2-y1;
 END;
 ignore := SetTextPosition( 1,1 );
END SetTextWindow;


(*--------------------------------------------------------------------------
-- Global function: Wrapon
-- Description: set TextWindow`s wrapping
-- Returns: previous value
--------------------------------------------------------------------------*)
PROCEDURE Wrapon (Opt: BOOLEAN): BOOLEAN;
VAR
  prev: BOOLEAN;
BEGIN
  prev := cur_wraping;
  cur_wraping := Opt;
  RETURN prev;
END Wrapon;


(*--------------------------------------------------------------------------
-- Global function: OutText
-- Description:
--------------------------------------------------------------------------*)

PROCEDURE OutText ( Text- :ARRAY OF CHAR);
VAR
    c: CHAR;
    n, h: CARDINAL;
BEGIN
    xtsMsMouse.hideCursor;   
    n:=0;
    h:=HIGH(Text);
    LOOP
        IF n > h THEN EXIT END;
        c:= Text[n];
        INC(n);
        IF c = CHAR(0) THEN EXIT END;
        IF c = CHAR(0AH) THEN
            IF cur_text_coords.row < cur_text_window.y2 THEN
                INC(cur_text_coords.row);
            ELSE
--                    Scroll
                  vgagl.gl_copybox( (cur_text_window.x1-1)*FONT_WIDTH , (cur_text_window.y1+1)*FONT_HEIGHT, 
                                    (cur_text_window.x2-cur_text_window.x1+2)*FONT_WIDTH,
                                    (cur_text_window.y2-cur_text_window.y1)*FONT_HEIGHT,
                                    (cur_text_window.x1-1)*FONT_WIDTH , (cur_text_window.y1)*FONT_HEIGHT);

                  vgagl.gl_fillbox( cur_text_window.x1 , cur_text_window.y2, 
                                    cur_text_window.x2-cur_text_window.x1,1, cur_bk_color);     

            END;
            cur_text_coords.col:=cur_text_window.x1;
--        ELSIF c = CHAR(0DH) THEN
--            cur_text_coords.col:=cur_text_window.x1;
        ELSE
            vgagl.gl_printf(cur_text_coords.col*FONT_WIDTH, cur_text_coords.row*FONT_HEIGHT, "%c",c );
            IF cur_text_coords.col = cur_text_window.x2 THEN
                cur_text_coords.col := cur_text_window.x1;
                IF cur_text_coords.row < cur_text_window.y2 THEN
                    INC(cur_text_coords.row);
                ELSE
--             Scroll
                  vgagl.gl_copybox( (cur_text_window.x1-1)*FONT_WIDTH , (cur_text_window.y1+1)*FONT_HEIGHT, 
                                    (cur_text_window.x2-cur_text_window.x1+2)*FONT_WIDTH,
                                    (cur_text_window.y2-cur_text_window.y1)*FONT_HEIGHT,
                                    (cur_text_window.x1-1)*FONT_WIDTH , (cur_text_window.y1)*FONT_HEIGHT);

                  vgagl.gl_fillbox( cur_text_window.x1 , cur_text_window.y2, 
                                    cur_text_window.x2-cur_text_window.x1,1, cur_bk_color);     
                END;
                IF cur_wraping = _GWRAPOFF THEN EXIT END;
            ELSE
                INC( cur_text_coords.col );
            END;
        END;
    END;
    xtsMsMouse.showCursor; 
    Refresh;
END OutText;


(*/////////////////////////// Bitmap operations //////////////////////////////*)


(*--------------------------------------------------------------------------
-- Additional types for handle images
--------------------------------------------------------------------------*)

(*--------------------------------------------------------------------------
-- Global function: GetImage
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE GetImage (x1, y1, x2, y2: CARDINAL; VAR handle: HBITMAP);
BEGIN
 xtsGraph.GetImage( x1, y1, x2, y2, handle );
END GetImage;

PROCEDURE PutImage (x1, y1: LONGCARD; hbm: HBITMAP; Action: LONGCARD);
BEGIN
  xtsMsMouse.hideCursor;   
  xtsGraph.PutImage (x1, y1, hbm);
  xtsMsMouse.showCursor; 
  Refresh;
END PutImage;


(*--------------------------------------------------------------------------
-- Global function: DelImage
-- Description: removes existed bitmap handle
--------------------------------------------------------------------------*)
PROCEDURE DelImage (hbm: HBITMAP);
VAR
  img: image_info_ptr;
BEGIN
  img := SYSTEM.CAST (image_info_ptr, hbm);
  IF img # NIL THEN
    DEALLOCATE (img^.ptr, ImageSize (0, 0, img^.w, img^.h));
    DEALLOCATE (img, SIZE (image_info));
  END;
END DelImage;


(*--------------------------------------------------------------------------
-- Global function: ImageSize
-- Description: gets image size
-- Return: necessory bytes for image
--------------------------------------------------------------------------*)
PROCEDURE ImageSize (x1, y1, x2, y2: CARDINAL) :CARDINAL;
BEGIN
  IF is16 THEN
    RETURN VAL(CARDINAL, ABS (VAL (INTEGER, x2) - VAL (INTEGER, x1))+1) *
           VAL(CARDINAL, ABS (VAL (INTEGER, y2) - VAL (INTEGER, y1))+1);
  
  ELSE
    RETURN VAL(CARDINAL, ABS (VAL (INTEGER, x2) - VAL (INTEGER, x1))+1) *
           VAL(CARDINAL, ABS (VAL (INTEGER, y2) - VAL (INTEGER, y1))+1) *
           VAL (CARDINAL, cur_mode_info^.bytesperpixel);
  END;   
END ImageSize;


(*///////////////////////////// Palette operations ////////////////////////////*)

(*--------------------------------------------------------------------------
-- Global function: RemapPalette
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE RemapPalette (palItem: CARDINAL; Color: LONGCARD): LONGCARD;
VAR
  old                : LONGCARD;
BEGIN
  old:=xtsGraph.RemapPalette( palItem, Color );
  Refresh;  
  RETURN old;
END RemapPalette;


(*--------------------------------------------------------------------------
-- Global function: RemapAllPalette
-- Description:
--------------------------------------------------------------------------*)

PROCEDURE RemapAllPalette (colArray: ARRAY OF LONGCARD): LONGCARD;
VAR
  old    :LONGCARD;
BEGIN
  old := xtsGraph.RemapAllPalette (colArray);
  Refresh;
  RETURN old;
END RemapAllPalette;

(*--------------------------------------------------------------------------
-- Global function: InitStdPalette
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE InitStdPalette;
BEGIN
    xtsGraph.InitStdPalette;
    Refresh;
END InitStdPalette;


(*//////////////////// Additional (XDS-specific) operations ////////////////////*)

(*--------------------------------------------------------------------------
-- Global function: RawOutText
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE RawOutText (x1, y1, Color: LONGCARD; Text-: ARRAY OF CHAR);
VAR
  col       :CARDINAL;
BEGIN
   xtsMsMouse.hideCursor;   
   col:=SetTextColor( Color );
   vgagl.gl_printf(x1, y1, "%s", Text );
   Color:=SetTextColor( col );
   xtsMsMouse.showCursor; 
   Refresh;
END RawOutText;


(*--------------------------------------------------------------------------
-- Global function: SetGraphWindowTitle
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE SetGraphWindowTitle (text :ARRAY OF CHAR);
BEGIN
END SetGraphWindowTitle;


(*--------------------------------------------------------------------------
-- Global function: Arc_a
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE Arc_a (x1, y1, x2, y2 :LONGCARD; startAngle, sweepAngle :LONGREAL; Color :LONGCARD);
VAR
  radSA, radEA     :LONGREAL;
  arcCntX, arcCntY :INTEGER;
  arcXStart,arcYStart,
  arcXEnd,arcYEnd     : CARDINAL;
  x0,y0               : INTEGER;
BEGIN
   xtsMsMouse.hideCursor;   
   IF x2>x1 THEN
     arcCntX := (x2-x1) DIV 2;
     x0 := INTEGER(x1)+arcCntX;
   ELSE
     arcCntX := (x1-x2) DIV 2;
     x0 := INTEGER(x2)+arcCntX;     
   END;     

   IF x2>x1 THEN
     arcCntY := (y2-y1) DIV 2;
     y0 := INTEGER(y1)+arcCntY;
   ELSE
     arcCntY := (y1-y2) DIV 2;
     y0 := INTEGER(y2)+arcCntY;     
   END;     

   radSA := startAngle*LM.pi/180.0;
   radEA := (startAngle+sweepAngle)*LM.pi/180.0;

   arcXStart := VAL (INTEGER, 1000.0 * LM.cos (radSA) ) + arcCntX;
   arcYStart := VAL (INTEGER, -1000.0 * LM.sin (radSA) ) + arcCntY;
   arcXEnd   := VAL (INTEGER, 1000.0 * LM.cos (radEA) ) + arcCntX;
   arcYEnd   := VAL (INTEGER, -1000.0 * LM.sin (radEA) ) + arcCntY;

   Arc(x0, y0, arcCntX, arcCntY, arcXStart, arcYStart, arcXEnd, arcYEnd , Color);
END Arc_a;


(*--------------------------------------------------------------------------
-- Global function: Pie_a
-- Description:
--------------------------------------------------------------------------*)
PROCEDURE Pie_a (x1, y1, x2, y2 :LONGCARD; startAngle, sweepAngle :LONGREAL;
                Color :CARDINAL; Fill :BOOLEAN);
VAR
  radSA, radEA     :LONGREAL;
  arcCntX, arcCntY :INTEGER;
  arcXStart,arcYStart,
  arcXEnd,arcYEnd     :INTEGER;
  x0,y0               :CARDINAL;  
BEGIN
   xtsMsMouse.hideCursor;   
   IF x2>x1 THEN
     arcCntX := (x2-x1) DIV 2;
     x0 := INTEGER(x1)+arcCntX;
   ELSE
     arcCntX := (x1-x2) DIV 2;
     x0 := INTEGER(x2)+arcCntX;     
   END;     

   IF x2>x1 THEN
     arcCntY := (y2-y1) DIV 2;
     y0 := INTEGER(y1)+arcCntY;
   ELSE
     arcCntY := (y1-y2) DIV 2;
     y0 := INTEGER(y2)+arcCntY;     
   END;     

   radSA := startAngle*LM.pi/180.0;
   radEA := (startAngle+sweepAngle)*LM.pi/180.0;

   arcXStart := VAL (INTEGER, 1000.0 * LM.cos (radSA) ) + arcCntX;
   arcYStart := VAL (INTEGER, -1000.0 * LM.sin (radSA) ) + arcCntY;
   arcXEnd   := VAL (INTEGER, 1000.0 * LM.cos (radEA) ) + arcCntX;
   arcYEnd   := VAL (INTEGER, -1000.0 * LM.sin (radEA) ) + arcCntY;

   Pie(x0, y0, arcCntX, arcCntY, arcXStart, arcYStart, arcXEnd, arcYEnd , Color, Fill);
END Pie_a;


--------------------------------------------------------------------------
-- Initialization section of the module
--------------------------------------------------------------------------
BEGIN
  (* Module Graph Initialisation Code *)
  PATSYM_DENSE1 := 0;
  PATSYM_DENSE2 := 1;
  PATSYM_VERT  := 2;
  PATSYM_HORIZ := 3;
  PATSYM_DIAG1 := 4;
  PATSYM_DIAG2 := 5;
  PATSYM_SOLID := 6;

  auto_refresh      := TRUE;
  cur_video_mode    := _DEFAULTMODE;
  cur_text_coords   := TextCoords {0, 0};
  cur_wraping       := FALSE;
  cur_cursor_toggle := FALSE;
  isText := TRUE;
END Graph.

(*
 * $Log: /XDSlib/src/TSlibs/Linux/Graph.mod $
(*  *)
(* 6     8/31/01 4:47p Kit *)
(* The input arguments (col,row) were changed to  (row,col) for *)
(* SetTextPosition and SetTextWindow procedures *)
(*  *)
(* 5     9/04/00 4:39p Adb *)
(*  *)
(* 6     9/04/00 4:27p Adb *)
(*  *)
(* 4     2/03/00 10:40p Adb *)
 * Revision 1.1  1999/09/14 13:03:35  ego
 * First unstable version of TopSpeed Graph, BiosIO and MsMouse modules.
 * And grf extension.
 *
 *)