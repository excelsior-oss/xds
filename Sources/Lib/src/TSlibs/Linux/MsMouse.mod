(* Copyright (c) 1999 JSC XDS. All rights reserved. *)
(* $RCSfile: MsMouse.mod,v $ $Revision: 5 $ $Date: 2/03/00 10:40p $ *)

<*+ M2EXTENSIONS*>
<*+ M2ADDTYPES*>

IMPLEMENTATION MODULE MsMouse;

IMPORT vgamouse, 
       vga, 
       Processes,
       Graph,
       SYSTEM,
       vgagl,
       xtsMsMouse,
       xtsGraph,
       gpm,
       Threads;

FROM SYSTEM IMPORT ADDRESS;
FROM Storage IMPORT ALLOCATE;

TYPE
  MsPos  = RECORD
    row : INTEGER;
    col : INTEGER;
  END;     

VAR
   cur_pos_motion      :MsPos;
   
   action             :ARRAY [0..1] OF ARRAY [0..2] OF CARDINAL;
    (*  0  are used for "press"
        1  are used for "release"   *)                      
   button              :ARRAY [0..2] OF BOOLEAN;

CONST
   gr_cur = GraphcurMask{
                0180H,01C0H,
                01E0H,01F0H,
                01F8H,01FCH,
                01FEH,01FFH,
                01FFH,01FFH,
                01FCH,01DEH,
                
019EH,000FH,
                000FH,0007H
  };


PROCEDURE load_cur_pos;
BEGIN
  xtsMsMouse.cur_pos.col := INTEGER(vgamouse.mouse_getx())+xtsMsMouse.hot_spot.col;
  xtsMsMouse.cur_pos.row := INTEGER(vgamouse.mouse_gety())+xtsMsMouse.hot_spot.row;
END load_cur_pos;

PROCEDURE Reset(): INTEGER;
BEGIN
  vgamouse.mouse_close;
  vgamouse.mouse_init("/dev/mouse", vga.vga_getmousetype(), 150 );
  xtsMsMouse.hideCursor;
  xtsMsMouse.whetherCursorOn := FALSE;
  xtsMsMouse.showCursorCount := -1;
  gpm.Gpm_Close;
  InitMouse;
  RETURN 0;
END Reset;

PROCEDURE Cursor(Mode: BOOLEAN);
BEGIN
  CASE Mode OF
   | _MS_SHOW : 
                IF xtsGraph.isText THEN
                  xtsMsMouse.showTextCursor;
                ELSE
		  xtsMsMouse.showCursor;
		END; 		
   | _MS_HIDE : 
                IF xtsGraph.isText THEN
                  xtsMsMouse.hideTextCursor;
                ELSE
		  xtsMsMouse.hideCursor;
		END; 		
  END;
END Cursor;

PROCEDURE DriverSize(): CARDINAL;
BEGIN
  RETURN 0;
END DriverSize;

PROCEDURE GetMotion(VAR mp: MsMotion);
VAR
  arow, acol     :INTEGER;
BEGIN
  arow := xtsMsMouse.cur_pos.row;
  acol := xtsMsMouse.cur_pos.col;
  WITH mp DO
    vert  := arow-cur_pos_motion.row;
    horiz := acol-cur_pos_motion.col;
  END;
  WITH cur_pos_motion DO
     row := arow;
     col := acol;   
  END;
END GetMotion;

PROCEDURE GetPage(): CARDINAL;
BEGIN
  RETURN 0;
END GetPage;

PROCEDURE GetPress(Button: INTEGER; VAR mp: MsData);
BEGIN
  WITH mp DO
    left_pressed    :=  button[2];
    right_pressed   :=  button[0];
    middle_pressed  :=  button[1];

    CASE Button OF
      |0: actions :=  action[0, 2]; action[0, 2] := 0;
      |1: actions :=  action[0, 0]; action[0, 0] := 0;
      |2: actions :=  action[0, 1]; action[0, 1] := 0;
    END;

    row := xtsMsMouse.cur_pos.row;
    col := xtsMsMouse.cur_pos.col;
  END;
END GetPress;

PROCEDURE GetRelease(Button: INTEGER; VAR mp: MsData);
BEGIN
  WITH mp DO
    left_pressed    :=  button[2];
    right_pressed   :=  button[0];
    middle_pressed  :=  button[1];

    CASE Button OF
      |0: actions :=  action[1, 2];action[1, 2] := 0;
      |1: actions :=  action[1, 0];action[1, 0] := 0;
      |2: actions :=  action[1, 1];action[1, 1] := 0;
    END;

    row := xtsMsMouse.cur_pos.row;
    col := xtsMsMouse.cur_pos.col;
  END;
END GetRelease;

PROCEDURE GetSensitivity(VAR mp: MsSense);
BEGIN
END GetSensitivity;

PROCEDURE GetStatus(VAR mp: MsData);
BEGIN
  vgamouse.mouse_update;
  WITH mp DO
    left_pressed    :=  button[2];
    right_pressed   :=  button[0];
    middle_pressed  :=  button[1];

    actions := 0;
    row := xtsMsMouse.cur_pos.row;
    col := xtsMsMouse.cur_pos.col;
  END;
END GetStatus;

PROCEDURE LightPen(Mode: BOOLEAN);
BEGIN
END LightPen;

PROCEDURE RestoreDriver(Buffer: ADDRESS);
BEGIN
END RestoreDriver;

PROCEDURE SaveDriver(Buffer: ADDRESS);
BEGIN
END SaveDriver;

PROCEDURE SetDouble(Threshold: INTEGER);
BEGIN
END SetDouble;

PROCEDURE SetGraphCursor(VAR mp: MsGraphcur);
BEGIN
    xtsMsMouse.SetGraphCursor( mp );
END SetGraphCursor;

PROCEDURE SetInterrupt(VAR mp: MsInterrupt);
BEGIN
--   vgamouse.mouse_seteventhandler( mp.);
END SetInterrupt;

PROCEDURE SwapInterrupt(VAR mp: MsInterrupt);
BEGIN
END SwapInterrupt;


PROCEDURE SetMickeys(Vert, Horiz: INTEGER);
BEGIN
END SetMickeys;

PROCEDURE SetPage(Page: CARDINAL);
BEGIN
END SetPage;

PROCEDURE SetPosition(Row, Col: INTEGER);
BEGIN
  vgamouse.mouse_setposition (Row, Col);
END SetPosition;

PROCEDURE SetRange(mp: MsRange);
BEGIN
  WITH mp DO
    vgamouse.mouse_setxrange (min_col, max_col);
    vgamouse.mouse_setyrange (min_row, max_row);
  END;
END SetRange;

PROCEDURE SetSensitivity(mp: MsSense);
BEGIN
END SetSensitivity;

PROCEDURE SetTextCursor(mp: MsTextCur);
BEGIN
END SetTextCursor;

PROCEDURE UpdateScreen(x1, y1, x2, y2: INTEGER);
BEGIN
END UpdateScreen;

PROCEDURE Update;
VAR
   s         :BITSET;
   i         :CARDINAL;
BEGIN
   (*
      left_pressed    =  button[2];
      right_pressed   =  button[0];
      middle_pressed  =  button[1];
   *)

  IF xtsGraph.isText THEN
    gpm.Gpm_GetEvent( xtsMsMouse.event );  
    
    xtsMsMouse.cur_pos.col := xtsMsMouse.event^.x;
    xtsMsMouse.cur_pos.row := xtsMsMouse.event^.y;

    xtsMsMouse.Gpm_DrawPointer( xtsMsMouse.cur_pos.col, xtsMsMouse.cur_pos.row, gpm.gpm_consolefd );
    
    FOR i := 0 TO 2 DO
       IF i IN BITSET( xtsMsMouse.event^.buttons ) THEN
           button[i] := TRUE;
           INC(action[0,i]);
       ELSE
           IF button[i] = TRUE THEN
              button[i] := FALSE;
              INC( action[1,i]);
           END;
       END;
    END;

  ELSE
    vgamouse.mouse_waitforupdate;
    xtsMsMouse.hideCursor;
    load_cur_pos;
    xtsMsMouse.showCursor;
    Graph.Refresh;
  
    s := vgamouse.mouse_getbutton ();
    FOR i := 0 TO 2 DO
       IF i IN s THEN
           button[i] := TRUE;
           INC(action[0,i]);
       ELSE
           IF button[i] = TRUE THEN
              button[i] := FALSE;
              INC( action[1,i]);
           END;
       END;
    END;
 END;
END Update;

PROCEDURE updateThread;
BEGIN
 LOOP
   Update;
 END;
END updateThread;


PROCEDURE InitMouse;
VAR
  cur      :MsGraphcur;
  i        :CARDINAL;
  thread   :Threads.Thread;

BEGIN
   xtsMsMouse.InitTextMouse;

(*   initialization for graph mode *)
   vgamouse.mouse_init("/dev/mouse", vga.vga_getmousetype(), 150 );
   
   cur.row := 0;
   cur.col := 0; 
   cur.cursor_mask := gr_cur;
   SetGraphCursor( cur );

   cur_pos_motion.row := 0;
   cur_pos_motion.col := 0;

   FOR i:= 0 TO 2 DO
       button[i] := FALSE;
       action[0,i] :=0;
       action[1,i] :=0;
   END;

   vgagl.gl_setclippingwindow( 0,0, 640,480);
   vgagl.gl_enableclipping;
   
   load_cur_pos;

   xtsMsMouse.whetherCursorOn := TRUE;
   xtsMsMouse.showCursorCount := -1;
   xtsMsMouse.showCursor;
   
   Threads.CreateThread( thread, updateThread, 0, NIL, 99, FALSE );

END InitMouse;

END MsMouse.

(*
 * $Log: /XDSlib/src/TSlibs/Linux/MsMouse.mod $
(*  *)
(* 5     2/03/00 10:40p Adb *)
 * Revision 1.1  1999/09/14 13:03:36  ego
 * First unstable version of TopSpeed Graph, BiosIO and MsMouse modules.
 * And grf extension.
 *
 *)