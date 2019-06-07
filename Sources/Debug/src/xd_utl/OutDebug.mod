-- Store output debug strings

<* storage+ *>

IMPLEMENTATION MODULE OutDebug;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT xs  := xStr;



TYPE
  PASTRING = POINTER TO ARRAY OF xs.STRING;

  STRINGS = RECORD
              Strings: PASTRING;
              Count  : CARDINAL;
            END;

VAR
  Strings: STRINGS;


PROCEDURE AddOutputDebugString (s-: ARRAY OF CHAR; SEQ arg: sys.BYTE);
VAR
  buf: ARRAY [0..1023] OF CHAR;
  tmp: PASTRING;
BEGIN
  fmt.print (buf, s, arg);
  WITH Strings DO
    IF Strings = NIL THEN
      NEW (Strings, 8);
      ASSERT (Strings#NIL);
      Count := 0;
    ELSIF Count > HIGH(Strings^) THEN
      NEW(tmp, 2*(HIGH(Strings^)+1));
      ASSERT (tmp#NIL);
      sys.MOVE (sys.ADR(Strings^), sys.ADR(tmp^), SIZE(Strings^));
      DISPOSE (Strings);
      Strings := tmp;
    END;
    xs.alloc_from (Strings^[Count], buf);
    INC(Count);
  END;
END AddOutputDebugString;


PROCEDURE Clear ();
VAR
  i: CARDINAL;
BEGIN
  WITH Strings DO
    IF Count > 0 THEN
      FOR i := 0 TO Count-1 DO
        xs.dealloc_str (Strings^[i]);
      END;
      Count := 0;
    END;
  END;
END Clear;


PROCEDURE StringsNo (): CARDINAL;
BEGIN
  RETURN Strings.Count;
END StringsNo;


PROCEDURE GetString (i: CARDINAL; VAR s: ARRAY OF CHAR);
BEGIN
  COPY (Strings.Strings^[i]^, s);
END GetString;

BEGIN
  Strings := STRINGS {NIL, 0};
END OutDebug.

