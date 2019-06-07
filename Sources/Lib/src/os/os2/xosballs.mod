(* Copyright (c) XDS Ltd. 1999. All Rights Reserved *)

<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE xosBalls; (* VitVit *)

IMPORT SYSTEM;

FROM SYSTEM IMPORT ADDRESS;

IMPORT OS2;

CONST
  hardPageSize = 4096;

VAR
  bSize     :CARDINAL;  -- block size (should be a multiple of hardPageSize)

PROCEDURE ["C"] X2C_initBalls (nBlocks, blockSize :CARDINAL) :ADDRESS;
VAR
  heapBase :ADDRESS;
BEGIN
  ASSERT ((blockSize MOD hardPageSize) = 0);
  bSize := blockSize;
  IF OS2.DosAllocMem(heapBase,
                     nBlocks*bSize,
                     OS2.PAG_READ+OS2.PAG_WRITE) <> OS2.NO_ERROR THEN
    heapBase := NIL
  END;
  RETURN heapBase;
END X2C_initBalls;


PROCEDURE ["C"] X2C_allocBlock(adr :ADDRESS) :ADDRESS;
VAR
  rc :OS2.APIRET;
BEGIN
  rc := OS2.DosSetMem(adr,
                      bSize,
                      OS2.PAG_COMMIT+OS2.PAG_READ+OS2.PAG_WRITE);

  IF (rc # OS2.NO_ERROR) THEN adr := NIL END;
  
  RETURN adr;
END X2C_allocBlock;

 
PROCEDURE ["C"] X2C_freeBlock (adr :ADDRESS);
BEGIN
  ASSERT(CARDINAL(adr) MOD hardPageSize = 0);
  OS2.DosSetMem(adr,
                bSize,
                OS2.PAG_DECOMMIT)
END X2C_freeBlock;

PROCEDURE ["C"] X2C_freeAll ();
BEGIN
  (* not implemented *)
END X2C_freeAll;

END xosBalls.
