(* Copyright (c) xTech 1993. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE TextIO;

IMPORT SYSTEM, IOChan, IOConsts, CharClass;

CONST ok = IOConsts.allRight;

PROCEDURE ReadChar(cid: IOChan.ChanId; VAR ch: CHAR);
  VAR n: CARDINAL;
BEGIN
  ch:=0C;
  IOChan.TextRead(cid,SYSTEM.ADR(ch),1,n);
END ReadChar;

PROCEDURE ReadRestLine(cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
  VAR n: CARDINAL; res: IOConsts.ReadResults; ch: CHAR;
BEGIN
  IOChan.TextRead(cid,SYSTEM.ADR(s),HIGH(s)+1,n);
  res:=IOChan.ReadResult(cid);
  IF n<=HIGH(s) THEN s[n]:=0C
  ELSIF res=ok THEN
    REPEAT
      IOChan.TextRead(cid,SYSTEM.ADR(ch),1,n);
    UNTIL IOChan.ReadResult(cid)#ok;
    IOChan.SetReadResult(cid,IOConsts.outOfRange);
  END;
END ReadRestLine;

PROCEDURE ReadString(cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
  VAR n: CARDINAL;
BEGIN
  IOChan.TextRead(cid,SYSTEM.ADR(s),HIGH(s)+1,n);
  IF n<=HIGH(s) THEN s[n]:=0C END;
END ReadString;

PROCEDURE ReadToken(cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
  VAR res: IOConsts.ReadResults; ch: CHAR; i: CARDINAL; valid: BOOLEAN;
BEGIN
  REPEAT
    IOChan.Look(cid,ch,res);
    IF (res=ok) THEN
      valid:=NOT CharClass.IsWhiteSpace(ch);
      IOChan.Skip(cid);
    END;
  UNTIL ( (res#ok) OR valid );

  (* ASSUME : IF res=ok THEN ch = the first char of token ( for Hady ) *)

  i:=0;
  WHILE (res=ok) & valid & (i<=HIGH(s)) DO
    s[i]:=ch; INC(i);
    IOChan.Look(cid,ch,res);
    valid:=NOT CharClass.IsWhiteSpace(ch);
    IF (res=ok) & valid THEN IOChan.Skip(cid) END;
  END;
  IF i<=HIGH(s) THEN
    s[i]:=0C;
    IF i>0 THEN IOChan.SetReadResult(cid,ok) END;
    RETURN
  ELSIF (res=ok) & valid THEN
    IOChan.SetReadResult(cid,IOConsts.outOfRange);
    RETURN
  END;
  IOChan.SetReadResult(cid,res);
END ReadToken;

PROCEDURE SkipLine(cid: IOChan.ChanId);
  VAR res: IOConsts.ReadResults; ch: CHAR;
BEGIN
  REPEAT
    ReadChar(cid,ch);
    res:=IOChan.ReadResult(cid);
  UNTIL res # ok;
  IF res=IOConsts.endOfLine THEN IOChan.Skip(cid) END;
END SkipLine;

PROCEDURE WriteChar(cid: IOChan.ChanId; ch: CHAR);
  VAR a: ARRAY [0..0] OF CHAR;
BEGIN
  a[0]:=ch;
  IOChan.TextWrite(cid,SYSTEM.ADR(a),1);
END WriteChar;

PROCEDURE WriteLn(cid: IOChan.ChanId);
BEGIN
  IOChan.WriteLn(cid);
END WriteLn;

PROCEDURE WriteString(cid: IOChan.ChanId; s-: ARRAY OF CHAR);
BEGIN
  IOChan.TextWrite(cid,SYSTEM.ADR(s),LENGTH(s));
END WriteString;

END TextIO.

