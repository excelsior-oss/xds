(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
(*
  XDS RTS. Small memory manager.
*)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosMem; (* Hady 30.05.96 15:34 *)

IMPORT SYSTEM, xWin32, Windows;

PROCEDURE ["C"] X2C_AllocMem(size: CARDINAL): SYSTEM.ADDRESS;
BEGIN
  RETURN xWin32.HeapAlloc(xWin32.GetProcessHeap(),xWin32.HEAP_ZERO_MEMORY,size);
END X2C_AllocMem;

PROCEDURE ["C"] X2C_InitMem();
BEGIN
  (* Nothing to initialize *)
END X2C_InitMem;

PROCEDURE ["C"] X2C_GetAvailablePhysicalMemory () :CARDINAL;
VAR memStatus :Windows.MEMORYSTATUS;
BEGIN
  Windows.GlobalMemoryStatus (memStatus);
  RETURN memStatus.dwAvailPhys;
END X2C_GetAvailablePhysicalMemory;

PROCEDURE ["C"] X2C_SetSystemDependentHeapLimit (VAR HeapLimit :CARDINAL);
VAR verInfo :Windows.OSVERSIONINFO;
BEGIN
  verInfo.dwOSVersionInfoSize := SIZE(verInfo);
  Windows.GetVersionEx (verInfo);
  CASE verInfo.dwPlatformId OF
  |Windows.VER_PLATFORM_WIN32_WINDOWS: HeapLimit := 20000000H;  (* 512 Mb *)
  |Windows.VER_PLATFORM_WIN32_NT:      HeapLimit := (1*1024 + 128)*1024*1024;  -- 1Gb + 128Mb
  ELSE
    HeapLimit := 20000000H;  (* 512 Mb *)
  END;
END X2C_SetSystemDependentHeapLimit;

END xosMem.
