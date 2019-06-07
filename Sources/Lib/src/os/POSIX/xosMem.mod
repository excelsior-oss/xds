(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosMem; (* Hady. 30.05.96 15:32 *)

IMPORT  SYSTEM;
IMPORT  xPOSIX;

PROCEDURE ["C"] X2C_AllocMem(size: CARDINAL): SYSTEM.ADDRESS;
BEGIN
  RETURN xPOSIX.malloc(size)
END X2C_AllocMem;

PROCEDURE ["C"] X2C_InitMem();
BEGIN
END X2C_InitMem;

PROCEDURE ["C"] X2C_GetAvailablePhysicalMemory () :CARDINAL;
BEGIN
  RETURN MAX(CARDINAL);
END X2C_GetAvailablePhysicalMemory;

END xosMem.
