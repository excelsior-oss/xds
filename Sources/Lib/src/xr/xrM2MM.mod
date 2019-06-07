(* Copyright (c) 1994,1996 xTech Ltd, Russia. All Rights Reserved. *)
(* Memory Manager. Modula-2 specific operations. *)

<* NEW ADB-      *>   (* allocate *)
<* NEW SDB-      *>   (* sweep *)
<* NEW MDB-      *>   (* mark  *)
<* NEW STAT-     *>   (* statistics *)
<* NEW FULLSTAT- *>   (* full statistics (STAT should be set) *)

(*----------------------------------------------------------------*)
<* IF XMM_DEBUG THEN *>
  <*+ checkindex   *>
  <*+ checkdindex  *>
  <*+ checknil     *>
  <*+ ioverflow    *>
  <*+ coverflow    *>
  <*+ assert       *>
<* ELSE *>
  <*- checkindex   *>
  <*- checkdindex  *>
  <*- checknil     *>
  <*- ioverflow    *>
  <*- coverflow    *>
  <*- assert       *>
  <*- ADB          *>
  <*- SDB          *>
  <*- MDB          *>
<* END *>

<*- checkrange   *>
<*- checkset     *>
<*- checkproc    *>
<*- ioverflow    *>
<*- coverflow    *>
<*- foverflow    *>
<*+ M2EXTENSIONS *>
<*+ M2ADDTYPES   *>
<*- M2BASE16     *>
<* IF __GEN_C__ THEN *>
<*- GENCTYPES    *>
<* END *>
<*+ WOFF310      *>

IMPLEMENTATION MODULE xrM2MM; (*  08-14-95 03:17pm *)

IMPORT
  SYSTEM
  ,xrMM
  ,xmRTS
  ;
<* IF XMM_DEBUG THEN *>
  IMPORT fmt:=FormStr;
  IMPORT  xrtsOS;
<* END *>

<* IF XMM_DEBUG THEN *>

  PROCEDURE db(f-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
        VAR buf: ARRAY [0..999] OF CHAR;
  BEGIN
    fmt.print(buf,f,x);
    xrtsOS.X2C_StdOut(buf,LENGTH(buf));
  END db;

<* END *>


TYPE
  ADDRESS = SYSTEM.ADDRESS;

CONST
  LINK_SZ  = SIZE(xmRTS.X2C_LINK_STR);
  GAP_SIZE = xrMM.GAP_LEN*SIZE(CARDINAL);

PROCEDURE [2] X2C_ALLOCATE(VAR a: ADDRESS; size: xrMM.SIZE_T);
(* Modula-2 "standard" allocate (explicit deallocation) *)
  VAR l: xmRTS.X2C_LINK;
BEGIN
  a:=NIL;
  IF size=0 THEN RETURN END;
  xrMM.allocate(l, size, FALSE);
  IF l=NIL THEN RETURN END;
  INCL(l^.tags,xrMM._expbit);
  l^.td:=xmRTS.x2c_td_null;
  a:=SYSTEM.ADDADR(l,LINK_SZ);
  IF (l^.tags*xrMM.BOFS_MASK = {}) THEN  -- large object
    INC (xrMM.expusedmem, xrMM.getLargeObjBlock(l)^.fsum); -- non-aligned size of piece
  ELSE
    INC (xrMM.expusedmem, xrMM.getHpObjSize(l));
  END;
<*IF ADB THEN*> db('ALLOCATE(%05x,%d) l.size=%x\n',a,size,l^.size); <*END*>
END X2C_ALLOCATE;

PROCEDURE ["C"] X2C_DEALLOCATE(VAR a: ADDRESS);
  VAR l: xmRTS.X2C_LINK; x: xrMM.Block; sz: CARDINAL;
BEGIN
  IF a = NIL THEN RETURN END;
  l:=SYSTEM.SUBADR(a,LINK_SZ);
  IF (l^.td^.res#xrMM.MAGIC_T) THEN xmRTS.X2C_ASSERT_F(100) END;
  IF xrMM._expbit IN l^.tags THEN
    IF (l^.tags*xrMM.BOFS_MASK = {}) THEN  -- LARGE block
      x := xrMM.getLargeObjBlock (l);
      ASSERT(x^.root = xrMM.LARGE);
      IF (x^.magic # xrMM.MAGIC_B) THEN xmRTS.X2C_ASSERT_F(101) END;
      sz:=x^.fsum; (* 20/6/96 non-aligned size of piece *)
      xrMM.free_block(x);
    ELSE
      sz:=xrMM.getHpObjSize(l);
      xrMM.dealloc(l)
    END;
    DEC(xrMM.expusedmem, sz);
    DEC(xmRTS.X2C_usedmem,sz);
    DEC(xmRTS.X2C_objects);
  ELSE
    EXCL(l^.tags,xrMM._sysbit);
  END;
  a:=NIL;
END X2C_DEALLOCATE;

PROCEDURE ["C"] X2C_DYNALLOCATE(VAR a: ADDRESS;
                       size : xrMM.SIZE_T;
                       lens-: ARRAY OF xmRTS.X2C_LENS_TYPE;
                       dims : xrMM.SIZE_T);
  VAR desc: xrMM.Dynarr;
BEGIN
  a:=NIL;
  ASSERT(dims <= xrMM.MaxDim,102);
  X2C_ALLOCATE(desc,xrMM.DynarrDescSize(dims));
  IF desc # NIL THEN
    xrMM.X2C_InitDesc(desc,size,lens,dims);
    X2C_ALLOCATE(desc^.a,size);
    IF desc^.a = NIL THEN X2C_DEALLOCATE(desc) ELSE a:=desc END;
  END;
END X2C_DYNALLOCATE;

PROCEDURE [2] X2C_DYNDEALLOCATE(VAR a: ADDRESS);
  VAR d: xrMM.Dynarr;
BEGIN
  IF a # NIL THEN
    d:=a;
    X2C_DEALLOCATE(d^.a);
    X2C_DEALLOCATE(a);
  END;
END X2C_DYNDEALLOCATE;

PROCEDURE [2] get_addr(    dim      : xrMM.SIZE_T; 
                       VAR free_addr: SYSTEM.ADDRESS; 
                           size     : xrMM.SIZE_T;
                           lens-    : ARRAY OF xmRTS.X2C_LENS_TYPE;
                           dims     : xrMM.SIZE_T): SYSTEM.ADDRESS;
TYPE PTR_TO_ADDR = POINTER TO SYSTEM.ADDRESS;
VAR i: CARDINAL;
    addr: SYSTEM.ADDRESS;
    tmp_ptr: PTR_TO_ADDR;
BEGIN
  addr:= free_addr;
  IF (dim + 1) = dims THEN
    free_addr:= SYSTEM.ADDADR(free_addr, size * lens[dim]);
  ELSE
    free_addr:= SYSTEM.ADDADR(free_addr, lens[dim] * 4);
    FOR i:= 0 TO lens[dim] - 1 DO
      tmp_ptr:= SYSTEM.CAST(PTR_TO_ADDR, SYSTEM.ADDADR(addr, i * 4));
      tmp_ptr^:= get_addr(dim + 1, free_addr, size, lens, dims); 
    END;
  END;
  RETURN addr;
END get_addr;


PROCEDURE [2] X2C_DYNCALLOCATE(VAR a: SYSTEM.ADDRESS;
                         size   : xrMM.SIZE_T;
                         lens-  : ARRAY OF xmRTS.X2C_LENS_TYPE;
                         dims   : xrMM.SIZE_T);
VAR i: CARDINAL;
    full_size: xrMM.SIZE_T;
    free_addr, addr: SYSTEM.ADDRESS;
BEGIN
  full_size:= size * lens[dims-1];
  FOR i:= 2 TO dims DO full_size:= (full_size + 4) * lens[dims-i] END;
  X2C_ALLOCATE(a, full_size);
  IF a # NIL THEN
    free_addr:= a;
    addr:= get_addr(0, free_addr, size, lens, dims);
    ASSERT((a = addr) AND (free_addr = SYSTEM.ADDADR(a, full_size)));
  END;
END X2C_DYNCALLOCATE;

PROCEDURE [2] X2C_DYNCDEALLOCATE(VAR a: SYSTEM.ADDRESS);
BEGIN
  IF a # NIL THEN X2C_DEALLOCATE(a) END;
END X2C_DYNCDEALLOCATE;


END xrM2MM.
