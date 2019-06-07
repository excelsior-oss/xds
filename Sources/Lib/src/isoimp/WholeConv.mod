(* (c) xTech 1993. All Rights Reserved. *)
IMPLEMENTATION MODULE WholeConv;

IMPORT ConvTypes, CharClass, WholeStr;

<* IF EXCEPTIONS THEN *>   IMPORT  EXCEPTIONS;
<* ELSE *>                 IMPORT  XRaise;
<* END *>

<* IF EXCEPTIONS THEN *>
  VAR source: EXCEPTIONS.ExceptionSource;
<* END *>

CONST ok = ConvTypes.strAllRight;

TYPE
  ScanState = ConvTypes.ScanState;
  ScanClass = ConvTypes.ScanClass;

PROCEDURE raise;
BEGIN
  <* IF EXCEPTIONS THEN *>
    EXCEPTIONS.RAISE(source,0,"WholeConv.Exception");
  <* ELSE *>
    XRaise.RAISE(XRaise.WholeConv,"WholeConv.Exception");
  <* END *>
END raise;

PROCEDURE Inv(ch: CHAR; VAR class: ScanClass; VAR next: ScanState);
BEGIN
  ch:=ch; class:=ConvTypes.invalid; next:=Inv;
END Inv;

PROCEDURE W(ch: CHAR; VAR class: ScanClass; VAR next: ScanState);
BEGIN
  IF CharClass.IsNumeric(ch) THEN
    class:=ConvTypes.valid; next:=W;
  ELSE
    class:=ConvTypes.terminator; next:=Inv;
  END;
END W;

PROCEDURE S(ch: CHAR; VAR class: ScanClass; VAR next: ScanState);
BEGIN
  IF CharClass.IsNumeric(ch) THEN
    class:=ConvTypes.valid; next:=W;
  ELSE
    class:=ConvTypes.invalid; next:=Inv;
  END;
END S;

PROCEDURE ScanInt(ch: CHAR; VAR class: ScanClass; VAR next: ScanState);
BEGIN
  IF CharClass.IsWhiteSpace(ch) THEN
    class:=ConvTypes.padding; next:=ScanInt;
  ELSIF (ch='-') OR (ch='+') THEN
    class:=ConvTypes.valid; next:=S;
  ELSIF CharClass.IsNumeric(ch) THEN
    class:=ConvTypes.valid; next:=W;
  ELSE
    class:=ConvTypes.invalid; next:=Inv;
  END;
END ScanInt;

PROCEDURE FormatInt(s: ARRAY OF CHAR): ConvResults;
  VAR n: INTEGER; res: ConvResults;
BEGIN
  WholeStr.StrToInt(s,n,res);
  RETURN res
END FormatInt;

PROCEDURE ValueInt(s: ARRAY OF CHAR): INTEGER;
  VAR n: INTEGER; res: ConvResults;
BEGIN
  n:=0;
  WholeStr.StrToInt(s,n,res);
  IF res # ok THEN raise END;
  RETURN n
END ValueInt;

PROCEDURE LengthInt(int: INTEGER): CARDINAL;
  VAR n: CARDINAL;
BEGIN
  n:=0;
  IF int<0 THEN
    n:=1;
    IF int=MIN(INTEGER) THEN int:=MAX(INTEGER) ELSE int:=-int END;
  END;
  REPEAT
    INC(n); int:=int DIV 10;
  UNTIL int = 0;
  RETURN n
END LengthInt;

PROCEDURE ScanCard(ch: CHAR; VAR class: ScanClass; VAR next: ScanState);
BEGIN
  IF CharClass.IsWhiteSpace(ch) THEN
    class:=ConvTypes.padding; next:=ScanCard;
  ELSIF CharClass.IsNumeric(ch) THEN
    class:=ConvTypes.valid; next:=W;
  ELSE
    class:=ConvTypes.invalid; next:=Inv;
  END;
END ScanCard;

PROCEDURE FormatCard(s: ARRAY OF CHAR): ConvResults;
  VAR n: CARDINAL; res: ConvResults;
BEGIN
  WholeStr.StrToCard(s,n,res);
  RETURN res
END FormatCard;

PROCEDURE ValueCard(s: ARRAY OF CHAR): CARDINAL;
  VAR n: CARDINAL; res: ConvResults;
BEGIN
  n:=0;
  WholeStr.StrToCard(s,n,res);
  IF res # ok THEN raise END;
  RETURN n
END ValueCard;

PROCEDURE LengthCard(card: CARDINAL): CARDINAL;
  VAR n: CARDINAL;
BEGIN
  n:=0;
  REPEAT
    INC(n); card:=card DIV 10;
  UNTIL card = 0;
  RETURN n
END LengthCard;

PROCEDURE IsWholeConvException (): BOOLEAN;
BEGIN
<* IF EXCEPTIONS THEN *>
  RETURN EXCEPTIONS.IsCurrentSource(source)
<* ELSE *>
  RETURN FALSE
<* END *>
END IsWholeConvException;

BEGIN
<* IF EXCEPTIONS THEN *>
  EXCEPTIONS.AllocateSource(source);
<* END *>
END WholeConv.

