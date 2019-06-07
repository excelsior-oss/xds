(* Copyright (c) xTech 1993. All Rights Reserved. *)
IMPLEMENTATION MODULE WholeIO;

IMPORT IOChan, TextIO, WholeStr, IOConsts, WholeConv, ConvTypes;

CONST ok = IOConsts.allRight;

PROCEDURE ReadInt(cid: IOChan.ChanId; VAR int: INTEGER);
  VAR state: ConvTypes.ScanState;
      class: ConvTypes.ScanClass;
      res: IOConsts.ReadResults;
      ch: CHAR;
      neg,ovfl,read: BOOLEAN;
      n,ord: CARDINAL;
BEGIN
  neg:=FALSE; ovfl:=FALSE; read:=FALSE;
  n:=0;
  state:=WholeConv.ScanInt;
  LOOP
    IOChan.Look(cid,ch,res);
    IF res#ok THEN ch:=0C END;
    state(ch,class,state);
    CASE class OF
    |ConvTypes.padding:
      IOChan.Skip(cid); read:=TRUE;
    |ConvTypes.valid:
      IOChan.Skip(cid); read:=TRUE;
      IF (ch='-') OR (ch='+') THEN neg:=(ch='-')
      ELSIF NOT ovfl THEN
        ord:=ORD(ch)-ORD('0');
        IF n > (MAX(CARDINAL) - ord) DIV 10 THEN
          ovfl:=TRUE;
          IF neg THEN int:=MIN(INTEGER) ELSE int:=MAX(INTEGER) END;
        ELSE n:=n*10+ord
        END;
      END;
    |ConvTypes.invalid:
      IF read OR (res=ok) THEN IOChan.SetReadResult(cid,IOConsts.wrongFormat) END;
      RETURN
    |ConvTypes.terminator:
      EXIT
    END;
  END;
  res:=ok;
  IF ovfl THEN res:=IOConsts.outOfRange;
  ELSIF neg THEN
    IF n>VAL(CARDINAL,MAX(INTEGER))+1 THEN res:=IOConsts.outOfRange
    ELSIF n = VAL(CARDINAL,MAX(INTEGER))+1 THEN int:=MIN(INTEGER)
    ELSE int:=-INT(n)
    END;
  ELSIF n>VAL(CARDINAL,MAX(INTEGER)) THEN res:=IOConsts.outOfRange
  ELSE int:=INT(n)
  END;
  IOChan.SetReadResult(cid,res);
END ReadInt;

PROCEDURE ReadCard(cid: IOChan.ChanId; VAR card: CARDINAL);
  VAR state: ConvTypes.ScanState;
      class: ConvTypes.ScanClass;
      res: IOConsts.ReadResults;
      ch: CHAR;
      ovfl,read: BOOLEAN;
      n,ord: CARDINAL;
BEGIN
  ovfl:=FALSE; read:=FALSE;
  n:=0;
  state:=WholeConv.ScanCard;
  LOOP
    IOChan.Look(cid,ch,res);
    IF res#ok THEN ch:=0C END;
    state(ch,class,state);
    CASE class OF
    |ConvTypes.padding:
       IOChan.Skip(cid); read:=TRUE;
    |ConvTypes.valid:
       IOChan.Skip(cid); read:=TRUE;
       IF NOT ovfl THEN
         ord:=ORD(ch)-ORD('0');
         IF n > (MAX(CARDINAL) - ord) DIV 10 THEN
           ovfl:=TRUE; card:=MAX(CARDINAL);
         ELSE n:=n*10+ord
         END;
       END;
    |ConvTypes.invalid:
      IF read OR (res=ok) THEN IOChan.SetReadResult(cid,IOConsts.wrongFormat) END; RETURN
    |ConvTypes.terminator:
       EXIT
    END;
  END;
  res:=ok;
  IF ovfl THEN res:=IOConsts.outOfRange;
  ELSE card:=n; res:=ok
  END;
  IOChan.SetReadResult(cid,res);
END ReadCard;

PROCEDURE WriteInt(cid: IOChan.ChanId; int: INTEGER; width: CARDINAL);
  VAR a: ARRAY [0..31] OF CHAR; len: CARDINAL;
BEGIN
  WholeStr.IntToStr(int,a);
  len:=LENGTH(a);
  IF width=0 THEN TextIO.WriteChar(cid,' ')
  ELSE
    WHILE width>len DO TextIO.WriteChar(cid,' '); DEC(width) END;
  END;
  TextIO.WriteString(cid,a);
END WriteInt;

PROCEDURE WriteCard(cid: IOChan.ChanId; card: CARDINAL; width: CARDINAL);
  VAR a: ARRAY [0..31] OF CHAR; len: CARDINAL;
BEGIN
  WholeStr.CardToStr(card,a);
  len:=LENGTH(a);
  IF width=0 THEN TextIO.WriteChar(cid,' ')
  ELSE
    WHILE width>len DO TextIO.WriteChar(cid,' '); DEC(width) END;
  END;
  TextIO.WriteString(cid,a);
END WriteCard;

END WholeIO.

