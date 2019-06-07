(* Copyright (c) xTech 1993. All Rights Reserved. *)
IMPLEMENTATION MODULE SRawIO;

IMPORT SYSTEM, IOChan, StdChans, RawIO;

PROCEDURE Read(VAR to: ARRAY OF SYSTEM.LOC);
BEGIN
  RawIO.Read(StdChans.InChan(),to);
END Read;

PROCEDURE Write(from: ARRAY OF SYSTEM.LOC);
BEGIN
  RawIO.Write(StdChans.OutChan(),from);
END Write;

END SRawIO.
