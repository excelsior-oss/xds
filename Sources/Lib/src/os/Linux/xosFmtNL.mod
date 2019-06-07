(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosFmtNL; (* Hady. 31.05.96 12:57 *)

IMPORT xrtsOS;

PROCEDURE ["C"] X2C_StdOutN();
BEGIN
  xrtsOS.X2C_StdOut(""+12C, 1);
  -- it's right in linux
  -- but possible incorrect on other POSIX systems
END X2C_StdOutN;

END xosFmtNL.
