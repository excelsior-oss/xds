(* Copyright (c) xTech 1992,97.  All Rights Reserved *)
(*
   implementation of memory allocation functions for OS/2
*)

<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE xosMalloc; (* VitVit'n'Hady'n'Laz. 30.06.97 15:43 *)

IMPORT SYSTEM, xOS2;

VAR
  prsHeap :SYSTEM.ADDRESS;

PROCEDURE ["C"] X2C_malloc (size: CARDINAL) :SYSTEM.ADDRESS;
  VAR res :SYSTEM.ADDRESS;
BEGIN
  IF (prsHeap # NIL) & (xOS2.DosSubAllocMem (prsHeap, res, size)=0) THEN
    RETURN res
  ELSE
    RETURN NIL
  END;
END X2C_malloc;

PROCEDURE ["C"] X2C_free(a: SYSTEM.ADDRESS; size: CARDINAL);
BEGIN
  IF xOS2.DosSubFreeMem(prsHeap, a, size) # 0 THEN (* ??? *) END;
END X2C_free;

PROCEDURE ["C"] X2C_InitHeap( heap :CARDINAL; isIncr :BOOLEAN );
BEGIN
  (* ASSUME : if isIncr, the heap value passed is greater than
     that value set before - VitVit *)
  IF (isIncr) THEN
    IF (prsHeap # NIL) &
       (xOS2.DosSubSetMem (prsHeap,
                           xOS2.DOSSUB_GROW+
                           xOS2.DOSSUB_SPARSE_OBJ+
                           xOS2.DOSSUB_SERIALIZE,
                           heap+64) # 0)
    THEN
      prsHeap := NIL;  (* if init error, prevent any further memory allocations *)
    END;
  ELSE
    IF (heap <= 0) THEN
      heap:=4*1024*1024;      (* 4MB by default *)
    END;
    IF (xOS2.DosAllocMem ( prsHeap,
                           heap+64,                (* OS/2 heap overhead *)
                           xOS2.PAG_READ+xOS2.PAG_WRITE
                          ) # 0) OR
       (xOS2.DosSubSetMem (prsHeap,
                           xOS2.DOSSUB_INIT+
                           xOS2.DOSSUB_SPARSE_OBJ+
                           xOS2.DOSSUB_SERIALIZE,
                           heap+64) # 0)
    THEN
      prsHeap := NIL;  (* if init error, prevent any further memory allocations *)
    END;
  END;
END X2C_InitHeap;


PROCEDURE ["C"] X2C_gmalloc ( size :CARDINAL ) :SYSTEM.ADDRESS;
VAR
  p :SYSTEM.ADDRESS;
BEGIN
  xOS2.DosAllocMem (p, size, xOS2.PAG_READ + xOS2.PAG_WRITE + xOS2.PAG_COMMIT );
  RETURN p;
END X2C_gmalloc;

PROCEDURE ["C"] X2C_gfree ( p :SYSTEM.ADDRESS );
BEGIN
  xOS2.DosFreeMem ( p );
END X2C_gfree;

PROCEDURE ["C"] X2C_DestroyHeap ();
BEGIN
  (* not implemented *)
END X2C_DestroyHeap;

END xosMalloc.
