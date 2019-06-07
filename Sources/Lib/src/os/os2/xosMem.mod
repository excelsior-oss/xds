(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
(*
  XDS RTS. Small memory manager.
*)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosMem; (* Hady 30.05.96 15:34 *)

IMPORT  SYSTEM, xOS2;

CONST PAGE = 4096;

VAR
  size: CARDINAL;
  adr: SYSTEM.ADDRESS;

PROCEDURE newPage;
BEGIN
  IF xOS2.DosAllocMem(adr, PAGE,
            xOS2.PAG_COMMIT + xOS2.PAG_READ + xOS2.PAG_WRITE) # 0
  THEN
    size := 0; adr := NIL;
  ELSE
    size := PAGE;
  END;
END newPage;

PROCEDURE ["C"] X2C_AllocMem(sz: CARDINAL): SYSTEM.ADDRESS;
  VAR a: SYSTEM.ADDRESS;
BEGIN
  IF sz > size THEN newPage END;
  IF sz > size THEN RETURN NIL END;
  a := adr;
  adr := SYSTEM.ADDADR(adr, sz); size := size - sz;
  RETURN a;
END X2C_AllocMem;

PROCEDURE ["C"] X2C_InitMem();
BEGIN
  size := 0; adr := NIL;
END X2C_InitMem;


PROCEDURE ["C"] X2C_GetAvailablePhysicalMemory () :CARDINAL;
BEGIN
  RETURN MAX(CARDINAL);
END X2C_GetAvailablePhysicalMemory;

END xosMem.
