(* Copyright (c) XDS 1992,96,99.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosMalloc; (* Hady. 30.05.96 15:43 *)

IMPORT  SYSTEM;
IMPORT  xPOSIX;

PROCEDURE ["C"] X2C_malloc(size: CARDINAL): SYSTEM.ADDRESS;
BEGIN
  RETURN xPOSIX.malloc(size);
END X2C_malloc;

PROCEDURE ["C"] X2C_free(adr: SYSTEM.ADDRESS; size: CARDINAL);
BEGIN
  xPOSIX.free(adr)
END X2C_free;

PROCEDURE ["C"] X2C_gmalloc ( size :CARDINAL ) :SYSTEM.ADDRESS;
BEGIN
  RETURN xPOSIX.malloc(size);
END X2C_gmalloc;

PROCEDURE ["C"] X2C_gfree ( p :SYSTEM.ADDRESS );
BEGIN
  xPOSIX.free(p)
END X2C_gfree;


PROCEDURE ["C"] X2C_InitHeap ( limit :CARDINAL; isIncr :BOOLEAN );
BEGIN
END X2C_InitHeap;

PROCEDURE ["C"] X2C_ZEROMEM*(adr: SYSTEM.ADDRESS; qsize: CARDINAL);
(* Fills qsize*4 bytes of memory residing at adr with zeroes *)
BEGIN
  xPOSIX.memset(adr,0,qsize*4)
END X2C_ZEROMEM;

PROCEDURE ["C"] X2C_DestroyHeap ();
BEGIN
  (* not implemented *)
END X2C_DestroyHeap;

END xosMalloc.

