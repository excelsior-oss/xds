(* Copyright (c) 2003 Excelsior, Russia. All Rights Reserved. *)

IMPLEMENTATION MODULE POSIXProcesses; (* Jek, 07.02.2003 *)

IMPORT SYSTEM, stat, x2cLib;

PROCEDURE MakeSource(fdes: INTEGER): Sources;
VAR
  buf : stat.stat_t;
BEGIN
  IF x2cLib.fstat(fdes, buf) < 0 THEN
    RETURN 0
  END;
  RETURN SYSTEM.CAST(Sources, fdes);
END MakeSource;

END POSIXProcesses.
