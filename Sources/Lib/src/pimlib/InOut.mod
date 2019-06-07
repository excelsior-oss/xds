(* Copyright (c) xTech 1992,96. All Rights Reserved *)
(* Implemantation on the base of ISO library *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE InOut; (* Ned 20-Sep-93 *)

IMPORT  STextIO, SWholeIO, WholeStr, SIOResult, StdChans, StreamFile, Strings;

TYPE ChanId = StreamFile.ChanId;

VAR
  res: SIOResult.ReadResults;
  inp: ChanId;
  out: ChanId;

PROCEDURE Open(prompt,defext: ARRAY OF CHAR; flags: StreamFile.FlagSet;
                                    VAR cid: ChanId);
  VAR name: ARRAY [0..127] OF CHAR;
      ores: StreamFile.OpenResults;
      i: CARDINAL;
BEGIN
  Done:=FALSE;
  IF StdChans.InChan()=StdChans.StdInChan() THEN
    STextIO.WriteString(prompt);
    ReadString(name);
    STextIO.WriteLn;
    i:=LENGTH(name);
    IF (i>0) & (name[i-1]=".") THEN
      IF NOT Strings.CanAppendAll(LENGTH(defext),name) THEN RETURN END;
      Strings.Append(defext,name);
    END;
    StreamFile.Open(cid,name,flags,ores);
    Done:=(ores=StreamFile.opened);
  END;
END Open;

PROCEDURE OpenInput(defext: ARRAY OF CHAR);
  VAR i: ChanId;
BEGIN
  Open("Input> ",defext,StreamFile.read,i);
  IF Done THEN inp:=i; StdChans.SetInChan(inp) END;
END OpenInput;

PROCEDURE OpenOutput( defext: ARRAY OF CHAR );
  VAR o: ChanId;
BEGIN
  Open("Output> ",defext,StreamFile.write+StreamFile.old,o);
  IF Done THEN out:=o; StdChans.SetOutChan(out) END;
END OpenOutput;

PROCEDURE CloseInput;
BEGIN
  Done:=FALSE;
  IF inp#StdChans.StdInChan() THEN
    IF inp=StdChans.InChan() THEN
      StdChans.SetInChan(StdChans.StdInChan());
    END;
    StreamFile.Close(inp);
    inp:=StdChans.StdInChan();
    Done:=TRUE;
  END;
END CloseInput;

PROCEDURE CloseOutput;
BEGIN
  Done:=FALSE;
  IF out#StdChans.StdOutChan() THEN
    IF out=StdChans.OutChan() THEN
      StdChans.SetOutChan(StdChans.StdOutChan());
    END;
    StreamFile.Close(out);
    out:=StdChans.StdOutChan();
    Done:=TRUE;
  END;
END CloseOutput;

(*----------------------------------------------------------------*)

PROCEDURE Read(VAR v: CHAR);
BEGIN
  STextIO.ReadChar(v);
  res:=SIOResult.ReadResult();
  Done:=(res # SIOResult.endOfInput);
  IF Done THEN
    IF res=SIOResult.endOfLine THEN STextIO.SkipLine; v:=EOL END;
  ELSE
    v:=0C;
  END;
END Read;

PROCEDURE ReadString(VAR s: ARRAY OF CHAR);
  VAR c: CHAR; i: CARDINAL;
BEGIN
  Read(c);
  WHILE Done & (c=" ") DO Read(c) END;
  i:=0;
  WHILE Done & (c>" ") DO
    IF i<HIGH(s) THEN s[i]:=c; INC(i) END;
    Read(c);
  END;
  termCH:=c;
  s[i]:=0C;
  Done:=(i>0);
END ReadString;

PROCEDURE ReadCard(VAR v: CARDINAL);
  VAR s: ARRAY [0..63] OF CHAR;
      r: WholeStr.ConvResults;
BEGIN
  ReadString(s);
  Done:=(SIOResult.ReadResult() = SIOResult.allRight);
  IF Done THEN
    WholeStr.StrToCard(s,v,r);
    Done:=(r = WholeStr.strAllRight);
  END;
END ReadCard;

PROCEDURE ReadInt(VAR v : INTEGER);
  VAR s: ARRAY [0..63] OF CHAR;
      r: WholeStr.ConvResults;
BEGIN
  ReadString(s);
  Done:=(SIOResult.ReadResult() = SIOResult.allRight);
  IF Done THEN
    WholeStr.StrToInt(s,v,r);
    Done:=(r = WholeStr.strAllRight);
  END;
END ReadInt;

PROCEDURE Write(c: CHAR);
BEGIN
  STextIO.WriteChar(c);
END Write;

PROCEDURE WriteLn;
BEGIN
  STextIO.WriteLn;
END WriteLn;

PROCEDURE WriteString(s-: ARRAY OF CHAR);
BEGIN
  STextIO.WriteString(s);
END WriteString;

PROCEDURE WriteCard(v: CARDINAL ; w: CARDINAL);
BEGIN
  SWholeIO.WriteCard(v,w);
END WriteCard;

PROCEDURE WriteInt(v: INTEGER; w: CARDINAL);
BEGIN
  SWholeIO.WriteInt(v,w);
END WriteInt;

PROCEDURE WriteBase(v,w,base: CARDINAL);
  VAR i,d: CARDINAL;
      a: ARRAY [0..10] OF CHAR;
BEGIN
  i:=0;
  REPEAT
    d:=v MOD base;
    IF d<10 THEN a[i]:=CHR(d+ORD("0")) ELSE a[i]:=CHR(d+ORD("A")-10) END;
    v := v DIV base;
    INC(i);
  UNTIL v = 0;
  WHILE w>i DO STextIO.WriteChar(" "); DEC(w) END;
  REPEAT DEC(i); STextIO.WriteChar(a[i]) UNTIL i = 0;
END WriteBase;

PROCEDURE WriteOct(v: CARDINAL; w: CARDINAL);
BEGIN
  WriteBase(v,w,8);
END WriteOct;

PROCEDURE WriteHex(v: CARDINAL; w: CARDINAL);
BEGIN
  WriteBase(v,w,16);
END WriteHex;

BEGIN
  inp:=StdChans.StdInChan();
  out:=StdChans.StdOutChan();
END InOut.
