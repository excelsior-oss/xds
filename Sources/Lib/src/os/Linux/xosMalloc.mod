(* Copyright (c) XDS 1992,96,99.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosMalloc; (* Hady. 30.05.96 15:43 *)
                                 (* Jek. 5.11.2003 *)

IMPORT  SYSTEM;
IMPORT  stdlib, string;

TYPE
  BlockList = POINTER TO BlockList_DESC;

  BlockList_DESC = RECORD
    next, prev :BlockList;
  END;


VAR
  listHead_desc :BlockList_DESC;
  listHead      :BlockList;

  
CONST
  ListNodeSize = SIZE(BlockList_DESC);


PROCEDURE ["C"] X2C_malloc(size: CARDINAL): SYSTEM.ADDRESS;
BEGIN
  RETURN X2C_gmalloc(size);
END X2C_malloc;


PROCEDURE ["C"] X2C_free(adr: SYSTEM.ADDRESS; size: CARDINAL);
BEGIN
  X2C_gfree(adr);
END X2C_free;


PROCEDURE ["C"] X2C_gmalloc ( size :CARDINAL ) :SYSTEM.ADDRESS;
VAR
  block :BlockList;
BEGIN
  block := stdlib.malloc(size+ListNodeSize);
  block^.next := listHead^.next;
  block^.prev := listHead;
  listHead^.next^.prev := block;
  listHead^.next := block;
  RETURN SYSTEM.ADDADR (block, ListNodeSize);
END X2C_gmalloc;


PROCEDURE ["C"] X2C_gfree ( p :SYSTEM.ADDRESS );
VAR
  block :BlockList;
BEGIN
  block := SYSTEM.SUBADR(p, ListNodeSize);
  ASSERT((block # NIL) AND (block # listHead));
  block^.next^.prev := block^.prev;
  block^.prev^.next := block^.next;
  stdlib.free(block);
END X2C_gfree;


PROCEDURE ["C"] X2C_InitHeap ( limit :CARDINAL; isIncr :BOOLEAN );
BEGIN
END X2C_InitHeap;


PROCEDURE freeAll ();
VAR
  block, dying :BlockList;
BEGIN
  block := listHead^.next;

  WHILE block # listHead DO
    dying := block;
    block := block^.next;
    stdlib.free(dying);
  END;

  listHead^.next := listHead;
  listHead^.prev := listHead;
END freeAll;


PROCEDURE ["C"] X2C_DestroyHeap ();
BEGIN
  freeAll;
END X2C_DestroyHeap;


PROCEDURE ["C"] xosMalloc_init();
BEGIN
  listHead := SYSTEM.ADR(listHead_desc);
  listHead^.next := listHead;
  listHead^.prev := listHead;
END xosMalloc_init;


END xosMalloc.

