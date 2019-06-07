(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
(* Implemantation on the base of ISO library *)
IMPLEMENTATION MODULE LongInOut;

IMPORT SLongIO, LongStr, InOut;

PROCEDURE ReadReal(VAR v: LONGREAL);
  VAR s: ARRAY [0..63] OF CHAR;
      r: LongStr.ConvResults;
BEGIN
  InOut.ReadString(s);
  Done:=InOut.Done;
  IF Done THEN
    LongStr.StrToReal(s,v,r);
    Done:=(r = LongStr.strAllRight);
  END;
END ReadReal;

PROCEDURE WriteReal(r: LONGREAL; w: CARDINAL);
BEGIN
  SLongIO.WriteReal(r,w);
END WriteReal;

END LongInOut.
