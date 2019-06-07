(* Copyright (c) Excelsior, 2003. All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosMem; (* Hady. 30.05.96 15:32 *)

(* Jek: X2C_GetAvailablePhysicalMemory() implemented. *)
(* Jek: X2C_AllocMem changed. *)

IMPORT  SYSTEM;
IMPORT  xrnProc;
IMPORT  xrInt64;
IMPORT  xosMalloc;


PROCEDURE ["C"] X2C_AllocMem(size: CARDINAL): SYSTEM.ADDRESS;
BEGIN
  RETURN xosMalloc.X2C_malloc(size)
END X2C_AllocMem;


PROCEDURE ["C"] X2C_InitMem();
BEGIN
END X2C_InitMem;


PROCEDURE ["C"] X2C_GetAvailablePhysicalMemory () :CARDINAL;
VAR
  mem      :xrnProc.GlobalMemInfo;
  amount32 :CARDINAL;
  amount   :xrInt64.X2C_int64;
  max      :xrInt64.X2C_int64;
BEGIN
  xrnProc.meminfo(mem);
  amount := SYSTEM.CAST(xrInt64.X2C_int64, mem.freePhys + mem.buffers + mem.cached);
  xrInt64.X2C_CARDTO64(max, MAX(CARDINAL)-1);
  (* meminfo returns kilobytes *)
  IF (xrInt64.X2C_CMP64(amount, max) > 0) OR  (* amount*K > amount > max *)
     (xrInt64.X2C_64TOCARD(amount32, amount)) (* overflow *)
  THEN
    RETURN MAX(CARDINAL)-1
  END;
  xrInt64.X2C_MUL64(amount, 1024, amount32);
  IF (xrInt64.X2C_CMP64(amount, max) > 0) THEN (* amount > max *)
    RETURN MAX(CARDINAL)-1
  ELSE
    RETURN CARDINAL(amount);
  END;
END X2C_GetAvailablePhysicalMemory;


PROCEDURE ["C"] X2C_SetSystemDependentHeapLimit (VAR HeapLimit :CARDINAL);
BEGIN
  HeapLimit := (1*1024 + 128)*1024*1024;  -- 1Gb + 128Mb
END X2C_SetSystemDependentHeapLimit;


END xosMem.
