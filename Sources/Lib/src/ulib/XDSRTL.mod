(* Copyright (c) XDS Ltd. 1998.  All Rights Reserved *)
<*+M2EXTENSIONS*>
IMPLEMENTATION MODULE XDSRTL;

IMPORT SYSTEM;

IMPORT xmRTS;

PROCEDURE ["C"] XDSRTL_Init (argc:        PINT;
                             argv:        PPCHAR;
                             gcauto:      INTEGER;
                             gcthreshold: INTEGER;
                             heaplimit:   INTEGER);
BEGIN
  xmRTS.X2C_BEGIN (argc^, SYSTEM.CAST(SYSTEM.ADDRESS,argv), gcauto, gcthreshold, heaplimit);
END XDSRTL_Init;

PROCEDURE ["C"] XDSRTL_Exit;
BEGIN
  xmRTS.X2C_EXIT;
END XDSRTL_Exit;

END XDSRTL.
