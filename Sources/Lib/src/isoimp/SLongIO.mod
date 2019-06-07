(* Copyright (c) xTech 1993. All Rights Reserved. *)
IMPLEMENTATION MODULE SLongIO;

IMPORT LongIO, StdChans;

TYPE float = LONGREAL;

PROCEDURE ReadReal (VAR real: float);
BEGIN
  LongIO.ReadReal (StdChans.InChan(), real);
END ReadReal;

PROCEDURE WriteFloat (real: float; sigFigs: CARDINAL; width: CARDINAL);
BEGIN
  LongIO.WriteFloat (StdChans.OutChan(), real, sigFigs, width);
END WriteFloat;

PROCEDURE WriteEng (real: float; sigFigs: CARDINAL; width: CARDINAL);
BEGIN
  LongIO.WriteEng (StdChans.OutChan(), real, sigFigs, width);
END WriteEng;

PROCEDURE WriteFixed (real: float; place: INTEGER; width: CARDINAL);
BEGIN
  LongIO.WriteFixed (StdChans.OutChan(), real, place, width);
END WriteFixed;

PROCEDURE WriteReal (real: float; width: CARDINAL);
BEGIN
  LongIO.WriteReal (StdChans.OutChan(), real, width);
END WriteReal;

BEGIN
END SLongIO.
