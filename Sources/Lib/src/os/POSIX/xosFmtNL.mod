(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosFmtNL; (* Hady. 31.05.96 12:57 *)

IMPORT  SYSTEM;
IMPORT xPOSIX;

PROCEDURE ["C"] X2C_StdOutN();
BEGIN
  SYSTEM.CODE('printf("\n");');
END X2C_StdOutN;

END xosFmtNL.
