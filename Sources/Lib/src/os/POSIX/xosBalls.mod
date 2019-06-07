(* Copyright (c) XDS Ltd. 1999. All Rights Reserved *)
<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE xosBalls; (* Shell *)

IMPORT 
    SYSTEM,
    xrtsOS,
    xrnMman;

FROM SYSTEM IMPORT ADDRESS;


CONST
    hardPageSize = 4096;

VAR
    bSize     :CARDINAL;  -- block size (sholud be a multiple of hardPageSize)
    heapBase  :ADDRESS;
    numBlocks :CARDINAL;  -- the number of blocks that initially mapped


PROCEDURE ["C"] X2C_initBalls (nBlocks, blockSize :CARDINAL) :ADDRESS;
BEGIN
    ASSERT( (blockSize MOD hardPageSize) = 0, 301);
    bSize := blockSize;

    heapBase := xrnMman.mmap(NIL, 
                             nBlocks*bSize, 
                             xrnMman.PROT_NONE,
                             xrnMman.MAP_ANON OR xrnMman.MAP_PRIVATE, 
                             -1, 
                             0);
    ASSERT( heapBase # ADDRESS(-1), 302 );			     
--    xrtsOS.X2C_StdOut("#RTS: X2C_initBalls #",21);
    RETURN heapBase;
END X2C_initBalls;


PROCEDURE ["C"] X2C_allocBlock(adr :ADDRESS) :ADDRESS;
BEGIN
    IF xrnMman.mprotect(adr, 
			bSize, 
			xrnMman.PROT_READ OR xrnMman.PROT_WRITE) # 0 THEN
 
	ASSERT( FALSE, 303 );
    END;
--    xrtsOS.X2C_StdOut("#RTS: X2C_allocBlock #",22);
    RETURN adr;
END X2C_allocBlock;

 
PROCEDURE ["C"] X2C_freeBlock (adr :ADDRESS);
BEGIN
    ASSERT( CARDINAL(adr) MOD hardPageSize = 0, 304);
    IF xrnMman.mprotect(adr, 
			bSize, 
			xrnMman.PROT_NONE) # 0 THEN
	ASSERT( FALSE, 305 );
    END;
--    xrtsOS.X2C_StdOut("#RTS: X2C_freeBlock #",21);
END X2C_freeBlock;


PROCEDURE ["C"] X2C_freeAll ();
BEGIN
    IF xrnMman.munmap(heapBase, numBlocks*bSize) # 0 THEN
	ASSERT( FALSE, 306 );
    END;
--    xrtsOS.X2C_StdOut("#RTS: X2C_freeAll #",19);
END X2C_freeAll;



END xosBalls.
