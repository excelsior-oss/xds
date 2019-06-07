(* Copyright (c) xTech 1993. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE STextIO;

IMPORT  TextIO, StdChans;

PROCEDURE ReadChar(VAR ch: CHAR);
BEGIN
  TextIO.ReadChar(StdChans.InChan(),ch);
END ReadChar;

PROCEDURE ReadRestLine(VAR s: ARRAY OF CHAR);
BEGIN
  TextIO.ReadRestLine(StdChans.InChan(),s);
END ReadRestLine;

PROCEDURE ReadString(VAR s: ARRAY OF CHAR);
BEGIN
  TextIO.ReadString(StdChans.InChan(),s);
END ReadString;

PROCEDURE ReadToken(VAR s: ARRAY OF CHAR);
BEGIN
  TextIO.ReadToken(StdChans.InChan(),s);
END ReadToken;

PROCEDURE SkipLine;
BEGIN
  TextIO.SkipLine(StdChans.InChan());
END SkipLine;

PROCEDURE WriteChar(ch: CHAR);
BEGIN
  TextIO.WriteChar(StdChans.OutChan(),ch);
END WriteChar;

PROCEDURE WriteLn;
BEGIN
  TextIO.WriteLn(StdChans.OutChan());
END WriteLn;

PROCEDURE WriteString(s-: ARRAY OF CHAR);
BEGIN
  TextIO.WriteString(StdChans.OutChan(),s);
END WriteString;

END STextIO.

