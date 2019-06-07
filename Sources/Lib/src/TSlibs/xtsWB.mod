(* Copyrigth (C) 1996,99 XDS Ltd *)

<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE xtsWB; (* VitVit *)


IMPORT SYSTEM,
       iterXY := xtsiterXY;

FROM SYSTEM IMPORT ADR, MOVE;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM xtsiterXY IMPORT myMIN, myMAX;

FROM xtsConM IMPORT SYM;

<* IF multithread THEN *>
  IMPORT Threads;
<* END *>

-------------------------------------------------- Semaphore


PROCEDURE GainAccess ( pwb :PWB );
BEGIN
<* IF multithread THEN *>
  Threads.LockMutex ( pwb^.semBuf );
<* END *>
END GainAccess;

PROCEDURE Relinquish ( pwb :PWB );
BEGIN
<* IF multithread THEN *>
  Threads.UnlockMutex ( pwb^.semBuf );
<* END *>
END Relinquish;



PROCEDURE GetWinBuf ( pwb :PWB; VAR pBuff :PBUFF );
BEGIN
  pBuff := SYSTEM.CAST ( PBUFF, ADR(pwb^.winBuff) );
END GetWinBuf;


PROCEDURE GetIndex ( pwb :PWB; xRel, yRel :CARDINAL; VAR i :CARDINAL );
BEGIN
  WITH pwb^ DO
    ASSERT ( (xRel < xd) AND (yRel < yd) );
    i := yRel*xd + xRel;
  END;
END GetIndex;


PROCEDURE RawWriteNChar ( pwb :PWB; relX, relY :CARDINAL; N :CARDINAL; c :CHAR; attr :ATTR );
VAR
  pbuff :PBUFF;
  bi    :CARDINAL;
BEGIN
  GainAccess ( pwb );
  GetWinBuf ( pwb, pbuff );
  GetIndex ( pwb, relX, relY, bi );
  WITH pwb^ DO
    WHILE (N>0) DO
      pbuff^[bi] := CELL{ SYM(c), attr };
      INC (bi);
      DEC (N);
    END;
  END;
  Relinquish ( pwb );
END RawWriteNChar;


(* assert : Any window has a client area, i.e. xd>2 & yd>2 in the case of a frame window *)

PROCEDURE LScrollUp ( pwb :PWB; line :CARDINAL; attr :ATTR );
VAR
  pbuff :PBUFF;
  bi    :CARDINAL;
BEGIN
  GainAccess ( pwb );
  WITH pwb^ DO
    IF (pwb^.yd >1) AND (inY2-inY1 > 0) THEN
      GetWinBuf ( pwb, pbuff );
      GetIndex ( pwb, 0, line, bi );
      MOVE ( ADR(pbuff^[bi+xd]), ADR(pbuff^[bi]), (inY2-line)*xd*SIZE(CELL) );
    END;
    RawWriteNChar ( pwb, inX1, inY2, inX2-inX1+1, ' ', attr );
  END;
  Relinquish ( pwb );
END LScrollUp;


PROCEDURE LScrollDown ( pwb :PWB; line :CARDINAL; attr :ATTR );
VAR
  pbuff  :PBUFF;
  bi     :CARDINAL;
  l2Move :CARDINAL;
BEGIN
  GainAccess ( pwb );
  WITH pwb^ DO
    IF (pwb^.yd >1) AND (inY2-inY1 > 0) THEN
      GetWinBuf ( pwb, pbuff );
      GetIndex ( pwb, 0, inY2, bi );
      l2Move := inY2-line;
      WHILE (l2Move >0 ) DO
        MOVE ( ADR(pbuff^[bi-xd]), ADR(pbuff^[bi]), xd*SIZE(CELL) );
        DEC (bi, xd);
        DEC (l2Move);
      END;
    END;
    RawWriteNChar ( pwb, inX1, line, inX2-inX1+1, ' ', attr );
  END;
  Relinquish ( pwb );
END LScrollDown;


PROCEDURE SetWrap ( pwb :PWB; on, hasfr :BOOLEAN );
BEGIN
  GainAccess ( pwb );
  WITH pwb^ DO
    wrapMode := on;
    IF (hasfr) THEN
      inX1 := 1;
      inY1 := 1;
      inX2 := xd-2;
      inY2 := yd-2;
    ELSE
      inX1 := 0;
      inY1 := 0;
      inX2 := xd-1;
      inY2 := yd-1;
    END;
  END;
  Relinquish ( pwb );
END SetWrap;


PROCEDURE Fill ( pwb :PWB; c :CHAR; attr :ATTR );
VAR
  pbuff :PBUFF;
  it    :iterXY.iterCL;
BEGIN
  GainAccess ( pwb );

  GetWinBuf ( pwb, pbuff );

  WITH pwb^ DO
    iterXY.init ( xd, yd, inX1, inY1, inX2, inY2, it );
    WHILE ( NOT iterXY.isEnd (it) ) DO
      pbuff^[it.i] := CELL{ SYM(c),attr};
      iterXY.inc(it);
    END;
  END;
  Relinquish ( pwb );
END Fill;


PROCEDURE RawWrite ( pwb :PWB; relX, relY :CARDINAL;
                     source- :ARRAY OF CHAR;
                     beg, length :CARDINAL;
                     attr :ATTR );
VAR
  pbuff :PBUFF;
  i, bi :CARDINAL;
BEGIN
  GainAccess ( pwb );
  GetWinBuf ( pwb, pbuff );
  GetIndex ( pwb, relX, relY, bi );
  WITH pwb^ DO
    FOR i:= beg TO beg+length-1 DO
      pbuff^[bi] := CELL{ SYM(source[i]),attr};
      INC (bi);
    END;
  END;
  Relinquish ( pwb );
END RawWrite;


PROCEDURE clipXY ( pwb :PWB; VAR x, y :CARDINAL; frm: BOOLEAN );
BEGIN
  GainAccess ( pwb );
  WITH pwb^ DO
    IF frm THEN
      x := myMIN ( myMAX (inX1, x), inX2);  -- framed clipping
      y := myMIN ( myMAX (inY1, y), inY2);
    ELSE
      x := myMIN ( x, xd-1);                -- non-framed clipping
      y := myMIN ( y, yd-1);
    END;
(*! Vit: this is wrong as in TS it is possible to access frame! 
    wrapMode has nothing with the clipping here; frame is important!
    IF (wrapMode) THEN
      x := myMIN ( myMAX (inX1, x), inX2);
      y := myMIN ( myMAX (inY1, y), inY2);
    ELSE
      x := myMIN ( x, inX2);
      y := myMIN ( y, inY2);
    END;
!*)
  END;
  Relinquish ( pwb );
END clipXY;


PROCEDURE WrLn ( pwb :PWB; attr :ATTR ) :BOOLEAN;
VAR
  wasScroll :BOOLEAN;
BEGIN
  GainAccess ( pwb );
  WITH pwb^ DO
    IF (curY < inY2) THEN
      INC (curY);
      wasScroll := FALSE;
    ELSE
      LScrollUp ( pwb, inY1, attr );
      wasScroll := TRUE;
    END;
    curX := inX1;
  END;
  Relinquish ( pwb );
  RETURN wasScroll;
END WrLn;


PROCEDURE WrStrSlice ( pwb    :PWB;
                       str-   :ARRAY OF CHAR;
           i, len :CARDINAL;
           attr   :ATTR
                     ) :BOOLEAN;
VAR
  lRmStr, lRmLine :CARDINAL;
  wasScroll       :BOOLEAN;
BEGIN
  GainAccess ( pwb );
  wasScroll := FALSE;
  WITH pwb^ DO
    IF NOT wrapMode THEN
      IF (inX2<curX) THEN
        len := 1;         -- write onto the right border
      ELSE  
        len := myMIN ( inX2-curX+1, len );
      END;  
    END;

    (* invariant : len>=0 *)
    WHILE (len>0) DO
      lRmLine := inX2-curX+1;
      IF (len > lRmLine)
        THEN lRmStr := lRmLine;
        ELSE lRmStr := len;
      END;
      RawWrite ( pwb, curX, curY, str, i, lRmStr, attr);
      DEC ( len, lRmStr );
      INC ( i,  lRmStr );
      (* advance cursor *)
      -- ASSERT (curX+lRmStr<=inX2+1);
      INC (curX, lRmStr);
      IF (curX>inX2) THEN
        IF (wrapMode) THEN
    IF WrLn ( pwb, attr ) THEN wasScroll := TRUE END;
        ELSE  
    IF (curX>xd-1) THEN
            curX := xd-1       -- write onto the right border
    ELSE
            curX := inX2  
    END;
        END;
      END;
    END; (* WHILE *)
  END; (* WITH *)
  Relinquish ( pwb );
  RETURN wasScroll;
END WrStrSlice;


TYPE
  a1Char = ARRAY [0..0] OF CHAR;

PROCEDURE WrChar ( pwb :PWB; c :CHAR; attr: ATTR );
VAR
  fuflo :BOOLEAN;
BEGIN
  fuflo := WrStrSlice ( pwb, a1Char(c), 0, 1, attr );
END WrChar;


PROCEDURE DrawFrame ( pwb :PWB; fs- :ARRAY OF CHAR; attr :ATTR );
VAR
  i  :CARDINAL;
  pi :CARDINAL;
  pbuff  :PBUFF;

PROCEDURE WrN ( c :CHAR; N :CARDINAL );
BEGIN
  WHILE ( N > 0 ) DO
    pbuff^[pi] := CELL{ SYM(c), attr };
    INC (pi);
    DEC (N);
  END;
END WrN;

BEGIN
  GainAccess ( pwb );
  ASSERT ( HIGH(fs) > 6 );
  GetWinBuf ( pwb, pbuff );
  pi := 0;
  WITH pwb^ DO
    pbuff^[pi] := CELL { SYM(fs[0]), attr };
    INC (pi);
    WrN ( fs[1], xd-2 );
    pbuff^[pi] := CELL { SYM(fs[2]), attr };
    INC (pi);

    FOR i :=1 TO yd-2 DO
      pbuff^[pi] := CELL { SYM(fs[3]), attr };
      INC ( pi, xd-1 );
      pbuff^[pi] := CELL { SYM(fs[4]), attr };
      INC (pi);
    END;

    pbuff^[pi] := CELL { SYM(fs[5]), attr };
    INC (pi);
    WrN ( fs[6], xd-2 );
    pbuff^[pi] := CELL { SYM(fs[7]), attr };
  END;
  Relinquish ( pwb );
END DrawFrame;



------------------------------------------------------------ constr/destructor

PROCEDURE Constr ( x, y :CARDINAL; wrapM, hasFr :BOOLEAN; VAR pwb :PWB ) :BOOLEAN;
BEGIN
  ALLOCATE ( pwb, SIZE(WBRec)+ x*y*SIZE(CELL) );
  IF ( pwb=NIL ) THEN RETURN FALSE END;

  SYSTEM.FILL( pwb, 0, SIZE(WBRec) );
  WITH pwb^ DO
    xd       := x;    (* these may not be changed  *)
    yd       := y;    (*  (see Destr)              *)

    <* IF multithread THEN *>  Threads.CreateMutex ( semBuf ); <* END *>
    SetWrap (pwb, wrapM, hasFr );  -- uses xd,yd & mutex !!!!!!

    IF (hasFr) THEN
      curX := 1;
      curY := 1;
    END;

  END;

  RETURN TRUE;
END Constr;

PROCEDURE Destr ( VAR pwb :PWB );
BEGIN
  GainAccess ( pwb );
  Relinquish ( pwb );
  <* IF multithread THEN *>  Threads.DeleteMutex ( pwb^.semBuf ); <* END *>
  DEALLOCATE ( pwb, SIZE(WBRec) + pwb^.xd*pwb^.yd*SIZE(CELL) );
  pwb := NIL;
END Destr;

END xtsWB.
