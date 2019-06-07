 (* Copyrigth (C) 1996,99 XDS Ltd *)
 (* xtsConM : OS/2 *)


<* +M2EXTENSIONS *>
<* +M2ADDTYPES   *>

IMPLEMENTATION MODULE xtsConM; (* VitVit, *FSA *)

IMPORT SYSTEM, O := OS2;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM SYSTEM IMPORT ADR, CARD16, CARD8;

FROM xtsiterXY IMPORT myMIN, myMAX;


<* IF multithread THEN *>
  IMPORT Threads;
<* END *>



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
  semScr : Threads.Mutex;
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
  p  :PBUFF;
  iy :CARDINAL;
BEGIN
  <* IF multithread THEN *> GainAccess; <* END *>

  FOR iy:=y1A TO y2A DO
    p := SYSTEM.CAST(PBUFF, ADR(pScr^[iy*Columns+x1A]));
    O.VioWrtCellStr ( p^, (x2A-x1A+1)*SIZE(CELL), iy, x1A, 0);
  END;

  <* IF multithread THEN *> Relinquish; <* END *>
END UpdateRect;


PROCEDURE UpdateAll;
BEGIN
  O.VioWrtCellStr ( pScr^, scrLength*SIZE(CELL), 0, 0, 0);
END UpdateAll;

-------------------------------------------------------------

VAR
  pScrSaved :PBUFF;

PROCEDURE SaveScreen;
VAR
  N :CARD16;
BEGIN
  N := scrLength*SIZE(CELL);
  O.VioReadCellStr(pScrSaved^, N, 0, 0, 0);
END SaveScreen;


PROCEDURE RestoreScreen;
VAR
  N :CARD16;
BEGIN
  N := scrLength*SIZE(CELL);
  O.VioWrtCellStr(pScrSaved^, N, 0, 0, 0);
END RestoreScreen;


--------------------------------------------------------- Cursor

PROCEDURE SetCurPos ( x,y :CARD16 );
BEGIN
  O.VioSetCurPos(y, x, 0);
END SetCurPos;

PROCEDURE GetCurPos ( VAR x,y :CARD16 );
BEGIN
  O.VioGetCurPos(y, x, 0);
END GetCurPos;

PROCEDURE CursorOn;
VAR
  vc :O.VIOCURSORINFO;
BEGIN
  O.VioGetCurType ( vc, 0 );
  IF ( vc.attr # 0 )
    THEN vc.attr := 0;
         O.VioSetCurType ( vc, 0 );
  END;
END CursorOn;


PROCEDURE CursorOff;
VAR
  vc :O.VIOCURSORINFO;
BEGIN
  O.VioGetCurType(vc, 0);
  IF vc.attr # 0FFFFH
   THEN vc.attr := 0FFFFH;
        O.VioSetCurType(vc, 0);
  END;
END CursorOff;


-------------------------------------------------------------

PROCEDURE CompareAttrs(attr1, attr2: ATTR): BOOLEAN;
BEGIN
    RETURN (attr1 = attr2);
END CompareAttrs;

-------------------------------------------------------------


VAR
  vioModeInfo :O.VIOMODEINFO;
  Xc, Yc: CARD16;
BEGIN
<* IF multithread THEN *> Threads.CreateMutex ( semScr ); <* END *>

  GetCurPos ( Xc, Yc );
  vioModeInfo.cb := SIZE ( O.VIOMODEINFO );
  O.VioGetMode ( vioModeInfo, 0);
  Columns := vioModeInfo.col;
  Rows    := vioModeInfo.row;

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

  SetCurPos ( Xc, Yc );
  CursorOn;

<* IF multithread THEN *> Threads.DeleteMutex ( semScr ); <* END *>

END xtsConM.
