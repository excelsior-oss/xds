<* +M2ADDTYPES   *>
<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE Serv;

 FROM xtsGraph IMPORT cur_clip_region;
 FROM Graph IMPORT Point;

PROCEDURE lScanHLine (VAR x :CARDINAL; y :INTEGER; backColor :CARDINAL);
BEGIN
  WHILE (x > cur_clip_region.x1) & (Point (x, y) # backColor) DO DEC (x) END;
END lScanHLine;


PROCEDURE rScanHLine (VAR x :CARDINAL; y :INTEGER; backColor :CARDINAL);
BEGIN
  WHILE (x < cur_clip_region.x2) & (Point (x, y) # backColor) DO INC (x) END;
END rScanHLine;


PROCEDURE scanHLine (VAR x1, x2 :CARDINAL; y :INTEGER; backColor :CARDINAL);
BEGIN
  IF (cur_clip_region.x1 <= x1) & (x2 <= cur_clip_region.x2) &
     (y >= 0) & (cur_clip_region.y1 <= CARDINAL(y)) &
     (CARDINAL(y) <= cur_clip_region.y2) THEN
    WHILE (x1 > cur_clip_region.x1) & (Point (x1, y) # backColor) DO DEC (x1) END;
    WHILE (x2 < cur_clip_region.x2) & (Point (x2, y) # backColor) DO INC (x2) END;
  END;
END scanHLine;

PROCEDURE min (a, b :CARDINAL) :CARDINAL;
BEGIN
  IF (a > b) THEN RETURN b; ELSE RETURN a; END;
END min;


PROCEDURE max (a, b :CARDINAL) :CARDINAL;
BEGIN
  IF (a < b) THEN RETURN b; ELSE RETURN a; END;
END max;


PROCEDURE orderCoords (VAR x0, y0, x1, y1 :CARDINAL);
VAR
  x0t, y0t, x1t, y1t :CARDINAL;
BEGIN
  x0t := x0;
  y0t := y0;
  x1t := x1;
  y1t := y1;

  x0 := min (x0t, x1t);
  y0 := min (y0t, y1t);
  x1 := max (x0t, x1t);
  y1 := max (y0t, y1t);
END orderCoords;


PROCEDURE adjust (VAR obj :CARDINAL; min, max :CARDINAL);
BEGIN
  IF (obj < min)THEN
    obj := min;
  ELSIF (obj > max) THEN
    obj := max;
  END;
END adjust;

END Serv.