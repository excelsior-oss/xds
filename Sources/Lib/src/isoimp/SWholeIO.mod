(* Copyright (c) xTech 1993. All Rights Reserved. *)
IMPLEMENTATION MODULE SWholeIO;

IMPORT  WholeIO, StdChans;

PROCEDURE ReadInt(VAR int: INTEGER);
BEGIN
  WholeIO.ReadInt(StdChans.InChan(),int);
END ReadInt;

PROCEDURE WriteInt(int: INTEGER; width: CARDINAL);
BEGIN
  WholeIO.WriteInt(StdChans.OutChan(),int,width);
END WriteInt;

PROCEDURE ReadCard(VAR card: CARDINAL);
BEGIN
  WholeIO.ReadCard(StdChans.InChan(),card);
END ReadCard;

PROCEDURE WriteCard(card: CARDINAL; width: CARDINAL);
BEGIN
  WholeIO.WriteCard(StdChans.OutChan(),card,width);
END WriteCard;

END SWholeIO.

