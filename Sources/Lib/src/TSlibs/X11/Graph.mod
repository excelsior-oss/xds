(* Copyright (C) 1997 XDS Team. All rights reserved. *)
(* $RCSfile: Graph.mod,v $ $Revision: 0.2 $ $Date: 1997/10/13 06:05:01 $ *)

<* +M2ADDTYPES   *>
<* +M2EXTENSIONS *>
<* IF NOT DEFINED(DEBUG) THEN *> <* NEW DEBUG+ *> <* END *>

IMPLEMENTATION MODULE Graph;

(*
    Title: 	Implementation of X11 interface to TopSpeed Graph mode
    Created:	Thu Oct  2 17:21:04 1997
    Author: 	Igor L. Ziryanov
		<ego@pluto.xTech.RU>
*)

IMPORT SYSTEM, io:=stdio, stdlib, signal, X, Xlib, Xutil;

FROM SYSTEM IMPORT CAST, ADDRESS, ADR;
FROM types IMPORT pid_t;
FROM unistd IMPORT fork, pipe, close, read, write, pipe_t;
FROM stdlib IMPORT exit;

CONST
  CLIENT = 0;
  SERVER = 1;
  ERROR = -1;

  MSG_ERR  = -1;
  MSG_OK   = 1;
  MSG_EXIT = 2;

  MAXBUF = 1024;

  ID_ClearScreen = CARDINAL(1);
  ID_SetLineStyle = CARDINAL(2);
  ID_GetLineStyle = CARDINAL(3);
  ID_SetStdFillMask = CARDINAL(4);
  ID_GetStdFillMask = CARDINAL(5);
  ID_SetFillMask = CARDINAL(6);
  ID_GetFillMask = CARDINAL(7);
  ID_SetBkColor = CARDINAL(8);
  ID_GetBkColor = CARDINAL(9);
  ID_SetBkMix = CARDINAL(10);
  ID_Plot = CARDINAL(11);
  ID_Point = CARDINAL(12);
  ID_HLine = CARDINAL(13);
  ID_Line = CARDINAL(14);
  ID_Ellipse = CARDINAL(15);
  ID_Disc = CARDINAL(16);
  ID_Circle = CARDINAL(17);
  ID_Rectangle = CARDINAL(18);
  ID_SetClipRgn = CARDINAL(19);
  ID_CancelClipRgn = CARDINAL(20);
  ID_Polygon = CARDINAL(21);
  ID_Cube = CARDINAL(22);
  ID_FloodFill = CARDINAL(23);
  ID_Arc = CARDINAL(24);
  ID_Pie = CARDINAL(25);
  ID_RawOutText = CARDINAL(26);
  ID_SetTextPosition = CARDINAL(27);
  ID_GetTextPosition = CARDINAL(28);
  ID_GetTextColumn = CARDINAL(29);
  ID_GetTextRow = CARDINAL(30);
  ID_SetTextColor = CARDINAL(31);
  ID_GetTextColor = CARDINAL(32);
  ID_SetTextWindow = CARDINAL(33);
  ID_Wrapon = CARDINAL(34);
  ID_DisplayCursor = CARDINAL(35);
  ID_OutText = CARDINAL(36);
  ID_GetImage = CARDINAL(37);
  ID_PutImage = CARDINAL(38);
  ID_DelImage = CARDINAL(39);
  ID_InitStdPalette = CARDINAL(40);
  ID_RemapPalette = CARDINAL(41);
  ID_RemapAllPalette = CARDINAL(42);

  SIGDRAW = signal.SIGUSR1;

TYPE
  (* Graphical rectangle *)
  Rect = RECORD
    s, f: GraphCoords;
  END;

  (* Text window rectangle *)
  Wind = RECORD
    s, f: TextCoords;
  END;

  (* font description *)
  Font = RECORD
    Xd, Yd, desc: LONGCARD;
  END;

  Buffer = ARRAY [0..MAXBUF] OF INTEGER;

VAR
  isInit	:BOOLEAN;	(* initialization indicator *)
  curRect	:Rect;		(* current Graph window *)
  curClip	:Rect;		(* current Clip Region *)
  bkCol		:LONGCARD;	(* current background color *)
  linStyle	:LONGINT;	(* current line style *)
  fillStd	:LONGINT;	(* current standard fill mask *)
  fillMask	:FillMaskType;	(* current fill mask *)
  isStdMask	:BOOLEAN;	(* standard fill mask indicator *)
  bkMix		:BOOLEAN;	(* Transparent/Opaque indicator *)

  curStat	:BOOLEAN;	(* current status *)
  wrapMode	:BOOLEAN;	(* wrap mode indicator *)
  curWin	:Wind;		(* current text window *)
  curPos	:TextCoords;	(* current position *)
  curTextCol	:LONGCARD;	(* current text color *)
  fntSet	:Font;		(* current font settings *)

  dpy		:Xlib.PtrDisplay;
  dpyName, host	:Xlib.PChar;
  gc, fill_gc	:Xlib.GC;
  xrect		:Xlib.XRectangle;
  scr		:Xlib.PtrScreen;
  page		:X.Pixmap;
  window	:X.Window;
  drawable	:X.Drawable;
  scrNum	:INTEGER;
  region	:Xutil.Region;
  pid		:pid_t;
  socket	:pipe_t;
  buff		:Buffer;

--------------------------------------------------------------------------
--------------------------------------------------------------------------
------------ I M P L E M E N T A T I O N   S E C T I O N -----------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------

PROCEDURE _ClearScreen (Area: CARDINAL);
BEGIN
  CASE Area OF
    |_GCLEARSCREEN:
    |_GVIEWPORT:
    |_GWINDOW:
    ELSE;
  END (* case Area *);
END _ClearScreen;

PROCEDURE _SetStdFillMask (Mask: LONGINT);
BEGIN
END _SetStdFillMask;

PROCEDURE _SetBkMix (bkMix: BOOLEAN);
BEGIN
END _SetBkMix;

PROCEDURE _Plot (x, y: LONGCARD; Color: LONGCARD);
BEGIN
END _Plot;

PROCEDURE _Point (x, y: LONGCARD): LONGCARD;
VAR
  color: LONGCARD;
BEGIN
  color := bkCol;
  RETURN color;
END _Point;

PROCEDURE _HLine (x, y, x2: LONGCARD; FillColor: LONGCARD);
BEGIN
END _HLine;

PROCEDURE _Line (x1, y1, x2, y2: LONGCARD; Color: LONGCARD);
BEGIN
END _Line;

PROCEDURE _Ellipse (x0, y0, a, b: LONGCARD; Color: LONGCARD; Fill: BOOLEAN);
BEGIN
END _Ellipse;

PROCEDURE _Rectangle (x1, y1, x2, y2: LONGCARD; Color: LONGCARD; Fill: BOOLEAN);
BEGIN
END _Rectangle;

PROCEDURE _SetClipRgn (x1, y1, x2, y2: LONGCARD);
BEGIN
END _SetClipRgn;

PROCEDURE _CancelClipRgn;
BEGIN
END _CancelClipRgn;

PROCEDURE _Polygon;
BEGIN
END _Polygon;

PROCEDURE _Cube;
BEGIN
END _Cube;

PROCEDURE _FloodFill;
BEGIN
END _FloodFill;

PROCEDURE _Arc;
BEGIN
END _Arc;

PROCEDURE _Pie;
BEGIN
END _Pie;

PROCEDURE _RawOutText;
BEGIN
END _RawOutText;

PROCEDURE _OutText;
BEGIN
END _OutText;

PROCEDURE _GetImage;
BEGIN
END _GetImage;

PROCEDURE _PutImage;
BEGIN
END _PutImage;

PROCEDURE _DelImage;
BEGIN
END _DelImage;

PROCEDURE _InitStdPalette;
BEGIN
END _InitStdPalette;

PROCEDURE _RemapPalette;
BEGIN
END _RemapPalette;

PROCEDURE _RemapAllPalette;
BEGIN
END _RemapAllPalette;

(* wrap out standard read/write *)
PROCEDURE _read (fildes: INTEGER; data: SYSTEM.ADDRESS; size: INTEGER);
BEGIN
  IF read (fildes, data, size) < 0 THEN
    io.perror ('read');
    exit (1);
  END (* read error *);
END _read;

PROCEDURE _write (fildes: INTEGER; data: SYSTEM.ADDRESS; size: INTEGER);
BEGIN
  IF write (fildes, data, size) < 0 THEN
    io.perror ('write');
    exit (1);
  END (* write error *);
END _write;

PROCEDURE _draw_recv ();
VAR
  result: INTEGER;
  card_arg: CARDINAL;	(* CARDINAL arguments *)
  long_arg: LONGINT;	(* LONGINT arguments *)
  bool_arg: BOOLEAN;	(* BOOLEAN arguments *)
  tcoords:  TextCoords;
  wcoords:  Wind;
BEGIN
  _read (socket [CLIENT], ADR (result), SIZE(INTEGER));
  CASE result OF
    |ID_ClearScreen:
      _read (socket [CLIENT], ADR (card_arg), SIZE (CARDINAL));
      _ClearScreen (card_arg);
    |ID_SetLineStyle:
      _read (socket [CLIENT], ADR (linStyle), SIZE (LONGINT));
    |ID_GetLineStyle:
      _write (socket [CLIENT], ADR (linStyle), SIZE (LONGINT));
    |ID_SetStdFillMask:
      _read (socket [CLIENT], ADR (long_arg), SIZE (LONGINT));
      _SetStdFillMask (long_arg);
    |ID_GetStdFillMask:
      _write (socket [CLIENT], ADR (fillStd), SIZE (LONGINT));
    |ID_SetFillMask:
      _read (socket [CLIENT], ADR (fillMask), SIZE (FillMaskType));
    |ID_GetFillMask:
      _write (socket [CLIENT], ADR (fillMask), SIZE (FillMaskType));
    |ID_SetBkColor:
      _read (socket [CLIENT], ADR (bkCol), SIZE (CARDINAL));
    |ID_GetBkColor:
      _write (socket [CLIENT], ADR (bkCol), SIZE (LONGINT));
    |ID_SetBkMix:
      _read (socket [CLIENT], ADR (bool_arg), SIZE (BOOLEAN));
      _SetBkMix (bool_arg);
    |ID_Plot:
      _read (socket [CLIENT], ADR (tcoords), SIZE (TextCoords));
      _read (socket [CLIENT], ADR (card_arg), SIZE (CARDINAL));
      _Plot (tcoords.row, tcoords.col, card_arg);
    |ID_Point:
      _read (socket [CLIENT], ADR (tcoords), SIZE (TextCoords));
      card_arg := _Point (tcoords.row, tcoords.col);
      _write (socket [CLIENT], ADR (card_arg), SIZE (CARDINAL));
    |ID_HLine:
      _read (socket [CLIENT], ADR (wcoords), SIZE (Wind));
      _HLine (wcoords.s.row, wcoords.s.col, wcoords.s.row, wcoords.s.col);
    |ID_Line:
      _read (socket [CLIENT], ADR (wcoords), SIZE (Wind));
      _read (socket [CLIENT], ADR (card_arg), SIZE (CARDINAL));
      _Line (wcoords.s.row, wcoords.s.col,
             wcoords.s.row, wcoords.s.col, card_arg);
    |ID_Ellipse:
      _read (socket [CLIENT], ADR (wcoords), SIZE (Wind));
      _read (socket [CLIENT], ADR (card_arg), SIZE (CARDINAL));
      _read (socket [CLIENT], ADR (bool_arg), SIZE (BOOLEAN));
      _Ellipse (wcoords.s.row, wcoords.s.col, wcoords.s.row, wcoords.s.col,
                card_arg, bool_arg);
    |ID_Disc:
      _read (socket [CLIENT], ADR (wcoords), SIZE (Wind));
      _Ellipse (wcoords.s.row, wcoords.s.col, wcoords.s.row, wcoords.s.row,
                wcoords.s.col, TRUE);
    |ID_Circle:
      _read (socket [CLIENT], ADR (wcoords), SIZE (Wind));
      _Ellipse (wcoords.s.row, wcoords.s.col, wcoords.s.row, wcoords.s.row,
                wcoords.s.col, FALSE);
    |ID_Rectangle:
      _read (socket [CLIENT], ADR (wcoords), SIZE (Wind));
      _read (socket [CLIENT], ADR (card_arg), SIZE (CARDINAL));
      _read (socket [CLIENT], ADR (bool_arg), SIZE (BOOLEAN));
      _Rectangle (wcoords.s.row, wcoords.s.col, wcoords.s.row, wcoords.s.col,
                  card_arg, bool_arg);
    |ID_SetClipRgn:
      _read (socket [CLIENT], ADR (wcoords), SIZE (Wind));
      _SetClipRgn (wcoords.s.row, wcoords.s.col, wcoords.s.row, wcoords.s.col);
    |ID_CancelClipRgn: _CancelClipRgn;
    |ID_Polygon:       _Polygon;
    |ID_Cube:          _Cube;
    |ID_FloodFill:     _FloodFill;
    |ID_Arc:           _Arc;
    |ID_Pie:           _Pie;
    |ID_RawOutText:    _RawOutText;
    |ID_SetTextPosition:
      _read (socket [CLIENT], ADR (curPos), SIZE (TextCoords));
    |ID_GetTextPosition:
      _write (socket [CLIENT], ADR (curPos), SIZE (TextCoords));
    |ID_GetTextColumn:
      _write (socket [CLIENT], ADR (curPos.col), SIZE (CARDINAL));
    |ID_GetTextRow:
      _write (socket [CLIENT], ADR (curPos.row), SIZE (CARDINAL));
    |ID_SetTextColor:
      _read (socket [CLIENT], ADR (curTextCol), SIZE (CARDINAL));
    |ID_GetTextColor:
      _write (socket [CLIENT], ADR (curTextCol), SIZE (CARDINAL));
    |ID_SetTextWindow:
      _read (socket [CLIENT], ADR (curWin), SIZE (Wind));
    |ID_Wrapon:
      _read (socket [CLIENT], ADR (wrapMode), SIZE (BOOLEAN));
    |ID_DisplayCursor:
      _read (socket [CLIENT], ADR (curStat), SIZE (BOOLEAN));
    |ID_OutText:         _OutText;
    |ID_GetImage:        _GetImage;
    |ID_PutImage:        _PutImage;
    |ID_DelImage:        _DelImage;
    |ID_InitStdPalette:  _InitStdPalette;
    |ID_RemapPalette:    _RemapPalette;
    |ID_RemapAllPalette: _RemapAllPalette;
    ELSE
  END (* case result *);
END _draw_recv;

PROCEDURE _startx (xLeft, yTop, xd, yd: LONGCARD);
VAR
  cmap: X.Colormap;
  color: Xlib.XColor;
  event: Xlib.XEvent;
  values: Xlib.XGCValues;
BEGIN
  (* get display to connect *)
  host := CAST (Xlib.PChar, stdlib.getenv ('DISPLAY'));
  IF host = NIL THEN
    io.fprintf (io.stderr^, 'Unknown DISPLAY\n');
    (* Notificate server *)
    IF write (socket [CLIENT], ADR (MSG_ERR), SIZE (INTEGER)) < 0 THEN
      io.perror ("write");
    END (* write error *);
    exit (1);
  END (* if host = NIL *);

  (* try to connect to X server *)
  dpy := Xlib.XOpenDisplay (host^);
  IF dpy = NIL THEN
    io.fprintf (io.stderr^, 'Unable to connect to server %s\n', host);
    (* Notificate server *)
    IF write (socket [CLIENT], ADR (MSG_ERR), SIZE (INTEGER)) < 0 THEN
      io.perror ("write");
    END (* write error *);
    exit (1);
  END (* if dpy = NIL *);

<* IF DEBUG THEN *>
  Xlib.XSynchronize (dpy, TRUE);
<* END *>

  scrNum := Xlib.DefaultScreen (dpy);
  scr    := Xlib.DefaultScreenOfDisplay (dpy);

  (* Create an simple window *)
  window := Xlib.XCreateSimpleWindow(dpy, Xlib.RootWindow(dpy, scrNum),
                                     xLeft, yTop, xd, yd, 1,
                                     Xlib.WhitePixel(dpy, scrNum),
                                     Xlib.BlackPixel(dpy, scrNum));
  (* Get input exposure, keyboard, button & notification events *)
  Xlib.XSelectInput (dpy, window, X.ExposureMask + X.KeyPressMask +
                                  X.ButtonPressMask + X.StructureNotifyMask);
  (* Display the window *)
  Xlib.XMapWindow (dpy, window);

  curRect.s.xcoord := xLeft;
  curRect.s.ycoord := yTop;
  isInit := TRUE;
  (* Notificate server *)
  IF write (socket [CLIENT], ADR (MSG_OK), SIZE (INTEGER)) < 0 THEN
    io.perror ("write");
    exit (1);
  END (* write error *);

  LOOP
    Xlib.XNextEvent (dpy, event);
    IF event.type = X.Expose THEN
      REPEAT UNTIL Xlib.XCheckMaskEvent (dpy, X.ExposureMask, event) # TRUE;
      EXIT;
    END (* if event.type = Xlib.Exposure *);
  END (* loop *);

END _startx;


--------------------------------------------------------------------------
-- Global function: Init
-- Description: Starts a graphics handler and creates connectivity
--              of handler and master process
-- On error: returns FALSE
-- Arguments: Coordinates of graphics handler window
--------------------------------------------------------------------------
PROCEDURE Init (xLeft, yTop, xd, yd: LONGCARD): BOOLEAN;
VAR
  result: INTEGER;
BEGIN
  IF pipe (socket) < 0 THEN
    io.perror ('pipe');
    RETURN FALSE;
  END (* if pipe (socket) < 0 *);
  signal.signal (signal.SIGINT, signal.SIG_IGN);
  signal.signal (signal.SIGHUP, signal.SIG_IGN);
  signal.signal (signal.SIGTERM,
                 CAST(signal.sighandler_t, Exit));
  (* FORK AHEAD!!! %-) *)
  pid := fork ();
  CASE pid OF
    |ERROR:	(* on error *)
      io.perror ('fork');
      RETURN FALSE;
    |CLIENT:	(* returned 0 meens CHILD *)
      (* be paranoid *)
      IF close (socket [SERVER]) < 0 THEN
      	io.perror ('client: close');
        exit (1);
      END (* if close (socket [SERVER]) < 0 *);
      signal.signal (SIGDRAW,
                     CAST (signal.sighandler_t, _draw_recv));
      _startx (xLeft, yTop, xd, yd);
      exit (0);
    ELSE	(* returned CHILD`s process ident to PARENT *)
      (* be paranoid *)
      IF close (socket [CLIENT]) < 0 THEN
      	io.perror ('server: close');
        RETURN FALSE;
      END (* if close (socket [CLIENT]) < 0 *);
      read (socket [SERVER], ADR (result), SIZE(INTEGER));
      IF result # MSG_OK THEN
  	isInit := FALSE;
      ELSE
  	isInit := TRUE;
      END (* result # MSG_OK *);
  END (* case pid *);
  RETURN isInit;
END Init;


--------------------------------------------------------------------------
-- Global function: Exit
-- Description: Closes connectivity of handler and master process and
--              Destroys a graphics handler
--------------------------------------------------------------------------
PROCEDURE Exit;
BEGIN
  isInit := FALSE;
END Exit;


--------------------------------------------------------------------------
-- Global function: ClearScreen
-- Description: Clears a screen
--------------------------------------------------------------------------
PROCEDURE ClearScreen (Area: CARDINAL);
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_ClearScreen), SIZE (ID_ClearScreen));
  _write (socket [SERVER], ADR (Area), SIZE (CARDINAL));
END ClearScreen;


--------------------------------------------------------------------------
-- Global function: SetLineStyle
-- Description:     Sets predefined line drawing style
--------------------------------------------------------------------------
PROCEDURE SetLineStyle ( Style: LONGINT );
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_SetLineStyle), SIZE (ID_SetLineStyle));
END SetLineStyle;


--------------------------------------------------------------------------
-- Global function: GetLineStyle
-- Description:
--------------------------------------------------------------------------
PROCEDURE GetLineStyle (): LONGINT;
VAR
  Style: LONGINT;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_GetLineStyle), SIZE (ID_GetLineStyle));
  _read (socket [SERVER], ADR (Style), SIZE (LONGINT));
  RETURN Style;
END GetLineStyle;


--------------------------------------------------------------------------
-- Global function: SetStdFillMask
-- Description:
--------------------------------------------------------------------------
PROCEDURE SetStdFillMask ( Mask: LONGINT);
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_SetStdFillMask), SIZE (ID_SetStdFillMask));
END SetStdFillMask;


--------------------------------------------------------------------------
-- Global function: GetStdFillMask
-- Description:
--------------------------------------------------------------------------
PROCEDURE GetStdFillMask (): LONGINT;
VAR
  Mask: LONGINT;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_GetStdFillMask), SIZE (ID_GetStdFillMask));
  _read (socket [SERVER], ADR (Mask), SIZE (LONGINT));
  RETURN Mask;
END GetStdFillMask;


--------------------------------------------------------------------------
-- Global function: SetFillMask
-- Description:
--------------------------------------------------------------------------
PROCEDURE SetFillMask ( Mask: FillMaskType );
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_SetFillMask), SIZE (ID_SetFillMask));
END SetFillMask;


--------------------------------------------------------------------------
-- Global function: GetFillMask
-- Description:
--------------------------------------------------------------------------
PROCEDURE GetFillMask ( VAR Mask: FillMaskType );
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_GetFillMask), SIZE (ID_GetFillMask));
  _read (socket [SERVER], ADR (Mask), SIZE (FillMaskType));
END GetFillMask;


--------------------------------------------------------------------------
-- Global function: SetBkColor
-- Description:
--------------------------------------------------------------------------
PROCEDURE SetBkColor ( Color: LONGCARD ): LONGCARD;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_SetBkColor), SIZE (ID_SetBkColor));
  _write (socket [SERVER], ADR (Color), SIZE (LONGCARD));
  RETURN Color;
END SetBkColor;


--------------------------------------------------------------------------
-- Global function: GetBkColor
-- Description:
--------------------------------------------------------------------------
PROCEDURE GetBkColor(): LONGCARD;
VAR
  Color: LONGCARD;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_GetBkColor), SIZE (ID_GetBkColor));
  _read (socket [SERVER], ADR (Color), SIZE (LONGINT));
  RETURN Color;
END GetBkColor;


--------------------------------------------------------------------------
-- Global function: SetBkMix
-- Description:
--------------------------------------------------------------------------
PROCEDURE SetBkMix ( isOpaque: BOOLEAN );
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_SetBkMix), SIZE (ID_SetBkMix));
END SetBkMix;

(*/////////////////////////////// graphic primitives /////////////////////////*)


--------------------------------------------------------------------------
-- Global function: Plot
-- Description: Set a single pixel to given color
--------------------------------------------------------------------------
PROCEDURE Plot ( x, y: LONGCARD; Color: LONGCARD );
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_Plot), SIZE (ID_Plot));
END Plot;


--------------------------------------------------------------------------
-- Global function: Point
-- Description: Read a pixel of the screen
--------------------------------------------------------------------------
PROCEDURE Point (x, y: LONGCARD): LONGCARD;
VAR
  Color: LONGCARD;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_Point), SIZE (ID_Point));
  _read (socket [SERVER], ADR (Color), SIZE (LONGCARD));
  RETURN Color;
END Point;


--------------------------------------------------------------------------
-- Global function: HLine
-- Description: Draw a horizontal line of given color (used for filling)
--------------------------------------------------------------------------
PROCEDURE HLine (x,y,x2: CARDINAL; FillColor: CARDINAL);
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_HLine), SIZE (ID_HLine));
END HLine;


--------------------------------------------------------------------------
-- Global function: Line
-- Description: Draw an arbitrary line in given color
--------------------------------------------------------------------------
PROCEDURE Line ( x1, y1, x2, y2: LONGCARD; Color: LONGCARD);
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_Line), SIZE (ID_Line));
END Line;


--------------------------------------------------------------------------
-- Global function: Ellipse
-- Description:
--------------------------------------------------------------------------
PROCEDURE Ellipse ( x0,y0:LONGCARD;   -- center
                    a,b : LONGCARD;   -- semi-axes
                    c   : LONGCARD;   -- color
                    fill: BOOLEAN );  -- whether filled
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_Ellipse), SIZE (ID_Ellipse));
END Ellipse;


--------------------------------------------------------------------------
-- Global function: Disc
-- Description: filled circle, centre x0,y0; radius r
--------------------------------------------------------------------------
PROCEDURE Disc (x0, y0, r, c: LONGCARD);
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_Disc), SIZE (ID_Disc));
END Disc;


--------------------------------------------------------------------------
-- Global function: Circle
-- Description: centre x0,y0; radius r
--------------------------------------------------------------------------
PROCEDURE Circle (x0, y0, r, c: LONGCARD);
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_Circle), SIZE (ID_Circle));
END Circle;


--------------------------------------------------------------------------
-- Global function: Rectangle
-- Description:
--------------------------------------------------------------------------
PROCEDURE Rectangle (x0, y0, x1, y1: LONGCARD; Color: LONGCARD; Fill: BOOLEAN);
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_Rectangle), SIZE (ID_Rectangle));
END Rectangle;


--------------------------------------------------------------------------
-- Global function: SetClipRgn
-- Description:
--------------------------------------------------------------------------
PROCEDURE SetClipRgn ( x1, y1, x2, y2: LONGCARD );
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_SetClipRgn), SIZE (ID_SetClipRgn));
END SetClipRgn;


--------------------------------------------------------------------------
-- Global function: CancelClipRgn
-- Description:
--------------------------------------------------------------------------
PROCEDURE CancelClipRgn;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_CancelClipRgn), SIZE (ID_CancelClipRgn));
END CancelClipRgn;


--------------------------------------------------------------------------
-- Global function: Polygon
-- Description:
--------------------------------------------------------------------------
PROCEDURE Polygon (n: LONGCARD; px, py: ARRAY OF LONGCARD; FillColor: LONGCARD; Fill: BOOLEAN);
(* polygon of n points *)
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_Polygon), SIZE (ID_Polygon));
END Polygon;


--------------------------------------------------------------------------
-- Global function: Cube
-- Description:
--------------------------------------------------------------------------
PROCEDURE Cube (top: BOOLEAN; x1, y1, x2, y2, depth: LONGCARD; Color: LONGCARD; Fill:BOOLEAN);
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_Cube), SIZE (ID_Cube));
END Cube;


--------------------------------------------------------------------------
-- Global function: FloodFill
-- Description:
--------------------------------------------------------------------------
PROCEDURE FloodFill ( x, y: LONGCARD; Color, Boundary: LONGCARD );
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_FloodFill), SIZE (ID_FloodFill));
END FloodFill;


--------------------------------------------------------------------------
-- Global function: Arc
-- Description:
--------------------------------------------------------------------------
PROCEDURE Arc ( x1, y1, x2, y2: LONGCARD; startAngle, sweepAngle: LONGREAL; Color: LONGCARD );
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_Arc), SIZE (ID_Arc));
END Arc;


--------------------------------------------------------------------------
-- Global function: Pie
-- Description:
--------------------------------------------------------------------------
PROCEDURE Pie ( x1, y1, x2, y2: LONGCARD; startAngle, sweepAngle: LONGREAL;
                Color: CARDINAL; Fill: BOOLEAN);
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_Pie), SIZE (ID_Pie));
END Pie;

(*////////////////////////// text procedures ////////////////////////////*)


--------------------------------------------------------------------------
-- Global function: RawOutText
-- Description:
--------------------------------------------------------------------------
PROCEDURE RawOutText (x1, y1, Color: LONGCARD; Text: ARRAY OF CHAR );
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_RawOutText), SIZE (ID_RawOutText));
END RawOutText;


--------------------------------------------------------------------------
-- Global function: SetTextPosition
-- Description:
--------------------------------------------------------------------------
PROCEDURE SetTextPosition ( col, row: LONGCARD);
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_SetTextPosition), SIZE (ID_SetTextPosition));
END SetTextPosition;


--------------------------------------------------------------------------
-- Global function: GetTextPosition
-- Description:
--------------------------------------------------------------------------
PROCEDURE GetTextPosition (): TextCoords;
VAR
  Coords: TextCoords;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_GetTextPosition), SIZE (ID_GetTextPosition));
  _read (socket [SERVER], ADR (Coords), SIZE (TextCoords));
  RETURN Coords;
END GetTextPosition;


--------------------------------------------------------------------------
-- Global function: GetTextColumn
-- Description:
--------------------------------------------------------------------------
PROCEDURE GetTextColumn (): LONGCARD;
VAR
  Col: LONGCARD;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_GetTextColumn), SIZE (ID_GetTextColumn));
  _read (socket [SERVER], ADR (Col), SIZE (LONGCARD));
  RETURN Col;
END GetTextColumn;


--------------------------------------------------------------------------
-- Global function: GetTextRow
-- Description:
--------------------------------------------------------------------------
PROCEDURE GetTextRow (): LONGCARD;
VAR
  Row: LONGCARD;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_GetTextRow), SIZE (ID_GetTextRow));
  _read (socket [SERVER], ADR (Row), SIZE (LONGCARD));
  RETURN Row;
END GetTextRow;


--------------------------------------------------------------------------
-- Global function: GetTextColor
-- Description:
--------------------------------------------------------------------------
PROCEDURE GetTextColor(): LONGCARD;
VAR
  Color: LONGCARD;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_GetTextColor), SIZE (ID_GetTextColor));
  _read (socket [SERVER], ADR (Color), SIZE (LONGCARD));
  RETURN Color;
END GetTextColor;


--------------------------------------------------------------------------
-- Global function: SetTextColor
-- Description:
--------------------------------------------------------------------------
PROCEDURE SetTextColor (Color: LONGCARD): LONGCARD;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_SetTextColor), SIZE (ID_SetTextColor));
  _write (socket [SERVER], ADR (Color), SIZE (LONGCARD));
  RETURN Color;
END SetTextColor;


--------------------------------------------------------------------------
-- Global function: SetTextWindow
-- Description:
--------------------------------------------------------------------------
PROCEDURE SetTextWindow (c1, r1, c2, r2: LONGCARD);
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_SetTextWindow), SIZE (ID_SetTextWindow));
END SetTextWindow;


--------------------------------------------------------------------------
-- Global function: Wrapon
-- Description:
--------------------------------------------------------------------------
PROCEDURE Wrapon ( Opt: BOOLEAN): BOOLEAN;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_Wrapon), SIZE (ID_Wrapon));
  _write (socket [SERVER], ADR (Opt), SIZE (BOOLEAN));
  RETURN Opt;
END Wrapon;


--------------------------------------------------------------------------
-- Global function: DisplayCursor
-- Description:
--------------------------------------------------------------------------
PROCEDURE DisplayCursor ( Toggle: BOOLEAN): BOOLEAN;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_DisplayCursor), SIZE (ID_DisplayCursor));
  _write (socket [SERVER], ADR (Toggle), SIZE (BOOLEAN));
  RETURN Toggle;
END DisplayCursor;


--------------------------------------------------------------------------
-- Global function: OutText
-- Description:
--------------------------------------------------------------------------
PROCEDURE OutText ( Text: ARRAY OF CHAR);
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_OutText), SIZE (ID_OutText));
END OutText;

(*/////////////////////////// Bitmap operations //////////////////////////////*)


--------------------------------------------------------------------------
-- Global function: GetImage
-- Description:
--------------------------------------------------------------------------
PROCEDURE GetImage (x1, y1, x2, y2: LONGCARD; VAR handle: HBITMAP );
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_GetImage), SIZE (ID_GetImage));
END GetImage;


--------------------------------------------------------------------------
-- Global function: PutImage
-- Description:
--------------------------------------------------------------------------
PROCEDURE PutImage (x, y: LONGCARD; hbm: HBITMAP; Action: LONGCARD);
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_PutImage), SIZE (ID_PutImage));
END PutImage;


--------------------------------------------------------------------------
-- Global function: DelImage
-- Description:
--------------------------------------------------------------------------
PROCEDURE DelImage ( hbm: HBITMAP );
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_DelImage), SIZE (ID_DelImage));
END DelImage;

(*///////////////////////////// Palette operations ////////////////////////////*)


--------------------------------------------------------------------------
-- Global function: InitStdPalette
-- Description:
--------------------------------------------------------------------------
PROCEDURE InitStdPalette;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_InitStdPalette), SIZE (ID_InitStdPalette));
END InitStdPalette;


--------------------------------------------------------------------------
-- Global function: RemapPalette
-- Description:
--------------------------------------------------------------------------
PROCEDURE RemapPalette (palItem: CARDINAL; Color: LONGCARD): LONGCARD;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_RemapPalette), SIZE (ID_RemapPalette));
  _write (socket [SERVER], ADR (Color), SIZE (CARDINAL));
  RETURN Color;
END RemapPalette;


--------------------------------------------------------------------------
-- Global function: RemapAllPalette
-- Description:
--------------------------------------------------------------------------
PROCEDURE RemapAllPalette (colArray: ARRAY OF LONGCARD): LONGCARD;
VAR
  n: CARDINAL;
BEGIN
  signal.kill (pid, SIGDRAW);
  _write (socket [SERVER], ADR (ID_RemapAllPalette), SIZE (ID_RemapAllPalette));
  n := HIGH (colArray);
  _write (socket [SERVER], ADR (n), SIZE (CARDINAL));
  _write (socket [SERVER], ADR (colArray), SIZE (LONGCARD) * n);
  RETURN colArray [0];
END RemapAllPalette;


--------------------------------------------------------------------------
-- Initialization section of the module
--------------------------------------------------------------------------
BEGIN
  (* Module Graph Initialisation Code *)
  isInit := FALSE;
  bkCol := _BLACK;
END Graph.
