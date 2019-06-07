(* Copyright (c) XDS Ltd. 1999. All Rights Reserved *)

<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE xosBalls; (* VitVit *)

IMPORT SYSTEM;

FROM SYSTEM IMPORT ADDRESS;

IMPORT W := Windows;


CONST
  hardPageSize = 4096;

VAR
  bSize     :CARDINAL;  -- block size (sholud be a multiple of hardPageSize)

  heapBase  :ADDRESS;


PROCEDURE ["C"] X2C_initBalls (nBlocks, blockSize :CARDINAL) :ADDRESS;
BEGIN
  ASSERT ((blockSize MOD hardPageSize) = 0);
  bSize    := blockSize;

  heapBase := W.VirtualAlloc (NIL,
                              nBlocks*bSize,
                              W.MEM_RESERVE,
                              W.PAGE_READWRITE);

  RETURN heapBase;
END X2C_initBalls;


PROCEDURE ["C"] X2C_allocBlock(adr :ADDRESS) :ADDRESS;
BEGIN
  RETURN W.VirtualAlloc (adr,
                         bSize,
                         W.MEM_COMMIT,
                         W.PAGE_READWRITE);
END X2C_allocBlock;

 
PROCEDURE ["C"] X2C_freeBlock (adr :ADDRESS);
BEGIN
  ASSERT(CARDINAL(adr) MOD hardPageSize = 0);
  W.VirtualFree (adr,
                 bSize,
                 W.MEM_DECOMMIT);
END X2C_freeBlock;

PROCEDURE ["C"] X2C_freeAll ();
BEGIN
  W.VirtualFree (heapBase,
                 0,
                 W.MEM_RELEASE);

END X2C_freeAll;

END xosBalls.
