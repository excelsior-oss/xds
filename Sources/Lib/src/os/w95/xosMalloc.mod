(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
(*
   implementation of memory allocation functions for Windows95/NT
*)
IMPLEMENTATION MODULE xosMalloc; (* VitVit'n'Hady. 30.05.96 15:43 *)

IMPORT  SYSTEM, xWin32;

VAR prsHeap: xWin32.HANDLE;

PROCEDURE ["C"] X2C_malloc(size: CARDINAL): SYSTEM.ADDRESS;
BEGIN
  RETURN xWin32.HeapAlloc(prsHeap,{},size);
END X2C_malloc;

PROCEDURE ["C"] X2C_free(adr: SYSTEM.ADDRESS; size: CARDINAL);
BEGIN
  IF xWin32.HeapFree(prsHeap,{},adr) THEN END;
END X2C_free;


PROCEDURE ["C"] X2C_gmalloc ( size :CARDINAL ) :SYSTEM.ADDRESS;
BEGIN
  RETURN xWin32.HeapAlloc(xWin32.GetProcessHeap(),xWin32.HEAP_ZERO_MEMORY,size);
END X2C_gmalloc;

PROCEDURE ["C"] X2C_gfree ( adr :SYSTEM.ADDRESS );
BEGIN
  IF xWin32.HeapFree(xWin32.GetProcessHeap(),{},adr) THEN END;
END X2C_gfree;

VAR canDestroyHeap :BOOLEAN;

PROCEDURE ["C"] X2C_InitHeap( heap :CARDINAL; isIncr :BOOLEAN );
BEGIN
  prsHeap:=xWin32.HeapCreate(xWin32.HEAP_NO_SERIALIZE,4096,0);
  IF prsHeap=0 THEN
    prsHeap:=xWin32.GetProcessHeap();
    canDestroyHeap := FALSE;
  ELSE
    canDestroyHeap := TRUE;
  END;
END X2C_InitHeap;

PROCEDURE ["C"] X2C_DestroyHeap ();
BEGIN
  IF canDestroyHeap THEN
    xWin32.HeapDestroy (prsHeap);
  END;
END X2C_DestroyHeap;

END xosMalloc.
