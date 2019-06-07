(* Copyright (c) XDS Ltd. 1999. All Rights Reserved *)

<* +M2EXTENSIONS *>

(* this module provides functionality for low-level highly efficient
   memory management: heap with equal size objects. Size of objects
   is a multiple of the physical memory page size on underlying hardware
 *)


IMPLEMENTATION MODULE xrBlockManager; (* VitVit *)

IMPORT SYSTEM, xosMalloc, xosBalls;

FROM SYSTEM IMPORT ADDRESS;

(*
 This is a simple & portable but low-effective implementation
*)

VAR
  bSize :CARDINAL;
   
PROCEDURE init (blockSize :CARDINAL) :BOOLEAN;
BEGIN
  bSize := blockSize;
  RETURN TRUE;
END init;

PROCEDURE alloc() :ADDRESS;
BEGIN
  RETURN xosMalloc.X2C_malloc(bSize);
END alloc;
 
PROCEDURE free (adr :ADDRESS);
BEGIN
  xosMalloc.X2C_free (adr, bSize);
END free;

PROCEDURE exit ();
BEGIN
  (* not implemented *)
END exit;

END xrBlockManager.
