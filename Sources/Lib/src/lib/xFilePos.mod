IMPLEMENTATION MODULE xFilePos; (* Hady. 25.06.96 18:20 *)

IMPORT  xrInt64;

TYPE
  FPOS = RECORD
    CASE : BOOLEAN OF
      |TRUE : rec: xrInt64.X2C_int64;
      |FALSE: arr: FilePos;
    END;
  END;

PROCEDURE CardToPos(VAR P: FilePos; c: CARDINAL);
  VAR p: FPOS;
BEGIN
  xrInt64.X2C_CARDTO64(p.rec,c);
  P:=p.arr;
END CardToPos;

PROCEDURE IntToPos(VAR P: FilePos; i: INTEGER);
  VAR p: FPOS;
BEGIN
  xrInt64.X2C_INTTO64(p.rec,i);
  P:=p.arr;
END IntToPos;

PROCEDURE PosToInt(VAR i: INTEGER; P: FilePos): BOOLEAN;
  VAR p: FPOS;
BEGIN
  p.arr:=P;
  RETURN NOT xrInt64.X2C_64TOINT(i,p.rec);
END PosToInt;

PROCEDURE PosToCard(VAR c: CARDINAL; P: FilePos): BOOLEAN;
  VAR p: FPOS;
BEGIN
  p.arr:=P;
  RETURN NOT xrInt64.X2C_64TOCARD(c,p.rec);
END PosToCard;

END xFilePos.
