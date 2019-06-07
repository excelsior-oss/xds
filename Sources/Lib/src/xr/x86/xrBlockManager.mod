(* Copyright (c) XDS Ltd. 1999. All Rights Reserved *)

<* +M2EXTENSIONS *>

(* this module provides functionality for low-level highly efficient
   memory management: heap with equal size objects. Size of objects
   is a multiple of the physical memory page size on underlying hardware
 *)


IMPLEMENTATION MODULE xrBlockManager; (* VitVit *)

IMPORT SYSTEM, xosMalloc, xosBalls, xosMem;

FROM SYSTEM IMPORT ADDRESS;

CONST
  bitsPerSet   = MAX(BITSET)+1;
  bytesPerSet  = bitsPerSet DIV 8;

  busySet      = {0..MAX(BITSET)};

VAR
  heapBase  :ADDRESS;
  bSize     :CARDINAL;


----------------------- BitMap of non-commited pages -----------------------

VAR
  map         :POINTER TO ARRAY [0..0FFFFFFH] OF BITSET;
  nSets       :CARDINAL;  -- # of elements in map
  hint4Search :CARDINAL;  -- linear search starts with the index


PROCEDURE initMap (numbersets :CARDINAL);
VAR
  i :CARDINAL;
BEGIN
  nSets := numbersets;
  map   := xosMalloc.X2C_malloc(nSets * bytesPerSet);

  FOR i := 0 TO nSets-1 DO
    map^[i] := {};
  END;
  hint4Search := 0;
END initMap;


PROCEDURE findBlock_markBusy() :ADDRESS;
VAR
  i :CARDINAL;
BEGIN
  ASSERT (heapBase # NIL);

  i := hint4Search;

  WHILE (map^[i] = busySet) DO
    INC (i);
    IF (i = nSets) THEN RETURN NIL END;
  END;
  hint4Search := i;

  ASSERT(map^[hint4Search] # busySet);
  i := 0;
  WHILE (i IN map^[hint4Search]) DO INC (i) END;
  INCL (map^[hint4Search], i);

  RETURN heapBase + (hint4Search*bitsPerSet+i)*bSize;
END findBlock_markBusy;


PROCEDURE markBlockFree (offset :CARDINAL);
VAR
  i :CARDINAL;
BEGIN
  offset  := offset DIV bSize;
  i       := offset DIV bitsPerSet;
  EXCL (map^[i], offset MOD bitsPerSet);
  IF (i < hint4Search) THEN hint4Search := i END;
END markBlockFree;

-----------------------------------------------------

PROCEDURE init (blockSize :CARDINAL) :BOOLEAN;
VAR
  nBlocks :CARDINAL;
  nSets   :CARDINAL;
BEGIN
  <* IF ((env_target = "x86nt") OR (env_target = "x86linux")) THEN *>
  xosMem.X2C_SetSystemDependentHeapLimit ( (* VAR *) heapLimit);
  LOOP
  <* END *>

  bSize := blockSize;

  nBlocks := (heapLimit + bSize -1) DIV bSize;
  nSets   := (nBlocks+bitsPerSet-1) DIV bitsPerSet;

  heapBase := xosBalls.X2C_initBalls (nSets*bitsPerSet, blockSize);

  <* IF ((env_target = "x86nt") OR (env_target = "x86linux")) THEN *>
    IF heapBase # NIL THEN EXIT; END;

    IF (heapLimit <= heapLimitDelta) THEN
      RETURN FALSE;
    END;
    heapLimit := heapLimit - heapLimitDelta;
  END;
  <* END *>

  IF (heapBase = NIL) THEN
    RETURN FALSE;
  END;

  -- range for heap is reserved successfully
  initMap (nSets);

  RETURN TRUE;
END init;


PROCEDURE alloc() :ADDRESS;
VAR
  adr, alloc_adr :ADDRESS;
BEGIN
  adr := findBlock_markBusy();

  IF (adr = NIL) THEN
    RETURN NIL;
  END;

  alloc_adr := xosBalls.X2C_allocBlock (adr);
  ASSERT ((alloc_adr = NIL) OR (alloc_adr = adr));

  RETURN alloc_adr;
END alloc;
 

PROCEDURE free (adr :ADDRESS);
BEGIN
  ASSERT (CARDINAL(adr) >= CARDINAL(heapBase));
  markBlockFree (SYSTEM.DIFADRC(adr, heapBase));

  xosBalls.X2C_freeBlock (adr);
END free;


PROCEDURE exit ();
BEGIN
  xosBalls.X2C_freeAll ();
END exit;

END xrBlockManager.
