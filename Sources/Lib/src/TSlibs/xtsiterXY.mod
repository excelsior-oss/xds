(* Copyrigth (C) 1996 xTech Ltd *)

<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE xtsiterXY; (* VitVit *)


PROCEDURE myMIN ( a,b :CARDINAL ) :CARDINAL;
BEGIN
  IF a < b THEN RETURN a ELSE RETURN b END;
END myMIN;


PROCEDURE myMAX ( a,b :CARDINAL ) :CARDINAL;
BEGIN
  IF a > b THEN RETURN a ELSE RETURN b END;
END myMAX;


PROCEDURE init ( xd, yd, x1A, y1A, x2A, y2A :CARDINAL; VAR obj :iterCL );


BEGIN
  ASSERT( (x1A<=x2A) & (y1A<=y2A));
  WITH obj DO
    xLim := myMIN ( x2A+1, xd )-1;
    yLim := myMIN ( y2A+1, yd );
    iD   := xd-xLim+x1A;
    xBeg := x1A;

    x  := x1A;
    y  := y1A;
    i  := xd*y1A + x1A;
  END
END init;

PROCEDURE inc ( VAR obj :iterCL );
BEGIN
  WITH obj DO
    IF (x = xLim) THEN
      x := xBeg;
      INC (y);
      INC (i , iD);
    ELSE
      INC (x);
      INC (i);
    END;
  END;
END inc;


PROCEDURE isEnd ( VAR obj :iterCL ) :BOOLEAN;
BEGIN
  WITH obj DO
    RETURN (y >= yLim) OR (xBeg>xLim);
  END;
END isEnd;

END xtsiterXY.

