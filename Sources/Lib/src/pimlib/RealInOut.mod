(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
(* Implemantation on the base of ISO library *)
IMPLEMENTATION MODULE RealInOut;

IMPORT SRealIO, RealStr, InOut;

PROCEDURE ReadReal(VAR v: REAL);
  VAR s: ARRAY [0..63] OF CHAR;
      r: RealStr.ConvResults;
BEGIN
  InOut.ReadString(s);
  Done:=InOut.Done;
  IF Done THEN
    RealStr.StrToReal(s,v,r);
    Done:=(r = RealStr.strAllRight);
  END;
END ReadReal;

PROCEDURE WriteReal(r: REAL; w: CARDINAL);
BEGIN
  SRealIO.WriteReal(r,w);
END WriteReal;

END RealInOut.
