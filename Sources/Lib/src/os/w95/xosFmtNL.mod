(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
(*
   implementation of NewLN function for Windows95/NT
*)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosFmtNL; (* Hady. 31.05.96 12:57 *)

IMPORT  xrtsOS;

PROCEDURE ["C"] X2C_StdOutN(); (* NewLine *)
BEGIN
  xrtsOS.X2C_StdOut(""+15C+12C,2);
END X2C_StdOutN;

END xosFmtNL.
