 (* Copyrigth (C) 1996,99 XDS Ltd *)
 (* xtsConM : Win32 *)

<* +M2EXTENSIONS *>
<* +M2ADDTYPES   *>

IMPLEMENTATION MODULE xtsConM; (* VitVit, *FSA *)

IMPORT SYSTEM, W := Windows;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM SYSTEM IMPORT ADR, CARD16, CARD8;

FROM xtsiterXY IMPORT myMIN, myMAX;

<* IF multithread THEN *>
  IMPORT Threads;
<* END *>



------------------------------------------------------------------------------------------

VAR
  ScrHndl :W.HANDLE;
TYPE
  wPBUFF = POINTER TO ARRAY [0..0FFFFH] OF W.CHAR_INFO;

------------------------------------------------------------------------------------------


PROCEDURE MakeAttr (fg, bg :COLOR) :ATTR;
BEGIN
  RETURN VAL ( ATTR, CARD8(bg)*16 + CARD8(fg) );
END MakeAttr;


PROCEDURE GetFg( attr :ATTR ) :COLOR;
BEGIN
  RETURN VAL(COLOR, CARD8(attr) MOD 16);
END GetFg;


PROCEDURE GetBg( attr :ATTR ) :COLOR;
BEGIN
  RETURN VAL(COLOR, CARD8(attr) DIV 16);
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
  c1, c2 :W.COORD;
  r      :W.SMALL_RECT;
  p      :wPBUFF;
BEGIN
  <* IF multithread THEN *> GainAccess; <* END *>

  WITH r DO
    Left   := x1A;
    Top    := y1A;
    Right  := myMIN (x2A, Columns-1);
    Bottom := myMIN (y2A, Rows-1);
  END;
  c1.X := Columns;
  c1.Y := Rows;
  c2.X := r.Left;
  c2.Y := r.Top;
  p := wPBUFF (pScr);
  W.WriteConsoleOutput ( ScrHndl, p^, c1, c2, r );

  <* IF multithread THEN *> Relinquish; <* END *>
END UpdateRect;


PROCEDURE UpdateAll;
VAR
  c1, c2 :W.COORD;
  r      :W.SMALL_RECT;
BEGIN
   UpdateRect ( 0, 0, Columns-1, Rows-1 );
END UpdateAll;

-------------------------------------------------------------

VAR
  pScrSaved :PBUFF;

PROCEDURE SaveScreen;
BEGIN
END SaveScreen;


PROCEDURE RestoreScreen;
BEGIN
  W.SetConsoleActiveScreenBuffer( W.GetStdHandle(W.STD_OUTPUT_HANDLE) );
END RestoreScreen;


--------------------------------------------------------- Cursor

PROCEDURE SetCurPos ( x,y :CARD16 );
VAR
  c: W.COORD;
BEGIN
  c.X := x;
  c.Y := y;
  W.SetConsoleCursorPosition ( ScrHndl, c );
END SetCurPos;

VAR
  ConScrBuffInfo :W.CONSOLE_SCREEN_BUFFER_INFO;

PROCEDURE GetCurPos ( VAR x,y :CARD16 );
VAR
  cp: W.COORD;
BEGIN
  W.GetConsoleScreenBufferInfo ( ScrHndl, ConScrBuffInfo );
  x := ConScrBuffInfo.dwCursorPosition.X;
  y := ConScrBuffInfo.dwCursorPosition.Y;
END GetCurPos;

PROCEDURE CursorOn;
VAR
  ci :W.CONSOLE_CURSOR_INFO;
BEGIN
  W.GetConsoleCursorInfo (ScrHndl, ci);
  ci.bVisible := TRUE;
  ci.dwSize   := 20;
  W.SetConsoleCursorInfo (ScrHndl, ci);
END CursorOn;


PROCEDURE CursorOff;
VAR
  ci :W.CONSOLE_CURSOR_INFO;
BEGIN
  W.GetConsoleCursorInfo ( ScrHndl, ci );
  ci.bVisible := FALSE;
  W.SetConsoleCursorInfo ( ScrHndl, ci );
END CursorOff;


-------------------------------------------------------------

PROCEDURE CompareAttrs(attr1, attr2: ATTR): BOOLEAN;
BEGIN
    RETURN (attr1 = attr2);
END CompareAttrs;

-------------------------------------------------------------

VAR
  Xc, Yc: CARD16;

BEGIN
<* IF multithread THEN *> Threads.CreateMutex ( semScr ); <* END *>

  ScrHndl := W.CreateConsoleScreenBuffer (W.GENERIC_WRITE+W.GENERIC_READ,
                                          W.FILE_SHARE_MODE{},
                                          NIL,
                                          W.CONSOLE_TEXTMODE_BUFFER,
                                          NIL);
  W.SetConsoleActiveScreenBuffer(ScrHndl);

  IF NOT W.GetConsoleScreenBufferInfo ( ScrHndl, ConScrBuffInfo ) THEN 
    ASSERT(FALSE,W.GetLastError());
  END;
  WITH ConScrBuffInfo.srWindow DO
    Columns := Right-Left+1;
    Rows    := Bottom-Top+1;
  END;

  scrLength := Columns*Rows;

  CursorOff;

  ALLOCATE( pScr,      scrLength*SIZE(CELL) );
  ALLOCATE( pScrSaved, scrLength*SIZE(CELL) );
  ASSERT ( (pScr # NIL) AND ( pScrSaved # NIL ) );

  SaveScreen;

FINALLY
<* IF NOT mydebug THEN *>
   RestoreScreen;
<* END *>

  DEALLOCATE( pScr,      scrLength*SIZE(CELL) );
  DEALLOCATE( pScrSaved, scrLength*SIZE(CELL) );

  W.CloseHandle ( ScrHndl );
  CursorOn;

<* IF multithread THEN *> Threads.DeleteMutex ( semScr ); <* END *>

END xtsConM.
