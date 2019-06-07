 (* Copyrigth (C) 1996,99 XDS Ltd *)
 (* xtsConM : Win32 *)

<* +M2EXTENSIONS *>
<* +M2ADDTYPES   *>

IMPLEMENTATION MODULE xtsConM; (* Shell *)

IMPORT SYSTEM, C:=h2d_curses, BiosIO;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM SYSTEM IMPORT ADR, CARD16, CARD8, int, INT16;

FROM xtsiterXY IMPORT myMIN, myMAX;

<* IF multithread THEN *>
  IMPORT Threads;
<* END *>



------------------------------------------------------------------------------------------
CONST
    A_BOLD = 8192;
    A_ALTCHARSET = 400000H;

VAR
    ScrHndl :C.H2D_PtrWINDOW;
    ts2nc: ARRAY [0..7] OF INTEGER;
    nc2ts: ARRAY [0..7] OF COLOR;
    NCColors: ARRAY [0..255] OF ATTR;


------------------------------------------------------------------------------------------


PROCEDURE MakeAttr (fg, bg :COLOR) :ATTR;
BEGIN
    RETURN NCColors[ CARD8(bg)*16+CARD8(fg)];
END MakeAttr;


PROCEDURE GetFg( attr :ATTR ) :COLOR;
BEGIN
    IF (attr.Other & A_BOLD)#0 THEN
        RETURN VAL(COLOR, CARD8( nc2ts[attr.Color MOD 8] )+8);
    ELSE    
        RETURN nc2ts[attr.Color MOD 8];
    END;
END GetFg;


PROCEDURE GetBg( attr :ATTR ) :COLOR;
BEGIN
    RETURN nc2ts[ (attr.Color DIV 16) & 7 ];
END GetBg;


-------------------------------------------------- Semaphore

<* IF multithread THEN *>
VAR
    semScr :Threads.Mutex;
<* END *>

PROCEDURE GainAccess;
BEGIN
<* IF multithread THEN *>
    Threads.LockMutex ( semScr );
<* END *>
END GainAccess;

PROCEDURE Relinquish;
BEGIN
<* IF multithread THEN *>
    Threads.UnlockMutex ( semScr );
<* END *>
END Relinquish;


-------------------------------------------------- Updating

PROCEDURE UpdateRect ( x1A, y1A, x2A, y2A :CARDINAL );
VAR
    x, y, len, ind: int;
--    p: C.H2D_Ptrchtype;
    c: C.chtype;
    attr: SYSTEM.int;
    iy, ix: CARDINAL;
BEGIN
    <* IF multithread THEN *> GainAccess; <* END *>
--    x := SYSTEM.CAST(int, x1A);
--    len := SYSTEM.CAST(int, x2A-x1A+1);
    FOR iy:=y1A TO y2A DO
        y := SYSTEM.CAST(int, iy);
(*
        p := SYSTEM.CAST(C.H2D_Ptrchtype, ADR(pScr^[iy*Columns+x1A]));
	C.mvwaddchnstr(ScrHndl, y, x, p, len);
*)
	FOR ix:=x1A TO x2A DO
            x := SYSTEM.CAST(int, ix);
            ind := iy*Columns+ix;
            c := (SYSTEM.CAST(C.chtype, pScr^[ind]));
            IF (c & 128)#0 THEN
                c := c OR C.chtype(A_ALTCHARSET);
            END;
	    C.mvwaddch(ScrHndl, y, x, c);
	END;
    END;
    C.attrset(C.A_NORMAL);
    C.wrefresh(ScrHndl);

    <* IF multithread THEN *> Relinquish; <* END *>
END UpdateRect;


PROCEDURE UpdateAll;
BEGIN
    UpdateRect ( 0, 0, Columns-1, Rows-1 );
END UpdateAll;

-------------------------------------------------------------

PROCEDURE SaveScreen;
BEGIN
--    C.savetty();

END SaveScreen;


PROCEDURE RestoreScreen;
BEGIN
--    C.resetty();
--    C.reset_shell_mode();
END RestoreScreen;


--------------------------------------------------------- Cursor

PROCEDURE SetCurPos ( x,y :CARD16 );
BEGIN
    C.wmove(ScrHndl, y, x); -- cast??
END SetCurPos;


PROCEDURE GetCurPos ( VAR x,y :CARD16 );
BEGIN
    y := VAL( CARD16, ScrHndl^._cury );
    x := VAL( CARD16, ScrHndl^._curx );
END GetCurPos;

PROCEDURE CursorOn;
BEGIN
    C.curs_set(1); -- visible
END CursorOn;


PROCEDURE CursorOff;
BEGIN
    C.curs_set(0); -- invisible
END CursorOff;


-------------------------------------------------------------

PROCEDURE CompareAttrs(attr1, attr2: ATTR): BOOLEAN;
BEGIN
    RETURN (attr1.Color=attr2.Color) AND (attr1.Other=attr2.Other);
END CompareAttrs;

-------------------------------------------------------------
PROCEDURE InitColors();
VAR
    i, bfg, fg, bg: INT16;
BEGIN
    ASSERT(C.COLOR_PAIRS=64);

    ts2nc[CARD8(Black)] := C.COLOR_BLACK;
    ts2nc[CARD8(Blue)] := C.COLOR_BLUE;
    ts2nc[CARD8(Green)] := C.COLOR_GREEN;
    ts2nc[CARD8(Cyan)] := C.COLOR_CYAN;
    ts2nc[CARD8(Red)] := C.COLOR_RED;
    ts2nc[CARD8(Magenta)] := C.COLOR_MAGENTA;
    ts2nc[CARD8(Brown)] := C.COLOR_YELLOW;
    ts2nc[CARD8(LightGray)] := C.COLOR_WHITE;

    nc2ts[C.COLOR_BLACK] := Black;
    nc2ts[C.COLOR_BLUE] := Blue;
    nc2ts[C.COLOR_GREEN] := Green;
    nc2ts[C.COLOR_CYAN] := Cyan;
    nc2ts[C.COLOR_RED] := Red;
    nc2ts[C.COLOR_MAGENTA] := Magenta;
    nc2ts[C.COLOR_YELLOW] := Brown;
    nc2ts[C.COLOR_WHITE] := LightGray;

    FOR i:=0 TO 127 DO
        fg := (i MOD 16) & 7;
        bg := (i DIV 16) & 7;
        NCColors[i].Color := (ts2nc[bg]*8 + ts2nc[fg]);
        IF (i & 8)#0 THEN
            NCColors[i].Other := A_BOLD;
        ELSE
            NCColors[i].Other := 0;
        END;
        NCColors[i+128] := NCColors[i];
    END;

    FOR i:=0 TO C.COLOR_PAIRS DO
	C.init_pair(i, i MOD C.COLORS, i / C.COLORS);
    END;

END InitColors;

VAR
  Xc, Yc: CARD16;

BEGIN
<* IF multithread THEN *> Threads.CreateMutex ( semScr ); <* END *>

    ScrHndl := C.initscr();
    C.cbreak();
    C.noecho();
    C.start_color();
    C.def_prog_mode();

    InitColors();
    
    C.wrefresh(ScrHndl);
    Columns := ScrHndl^._maxx+1;
    Rows    := ScrHndl^._maxy+1;

    scrLength := Columns*Rows;

    CursorOff;


    ALLOCATE(pScr, scrLength*SIZE(CELL));
    ASSERT(pScr#NIL);

    SaveScreen;

FINALLY
<* IF NOT mydebug THEN *>
    RestoreScreen;
<* END *>

    DEALLOCATE(pScr, scrLength*SIZE(CELL));

    C.noraw();
    C.cbreak();
    C.noecho();
    C.scrollok(C.stdscr, 1);
    C.idlok(C.stdscr, 1);
    C.keypad(C.stdscr, 1);    
    C.reset_prog_mode();
    
    C.clear();
    C.refresh();


    CursorOn;
    C.endwin();

<* IF multithread THEN *> Threads.DeleteMutex ( semScr ); <* END *>

END xtsConM.
