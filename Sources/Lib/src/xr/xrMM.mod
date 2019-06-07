(* Copyright (c) 1994,1999 XDS Ltd, Russia. All Rights Reserved. *)
(* Memory Manager. Basic operations. *)

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
<* ELSE *>
  <*- checkindex   *>
  <*- checkdindex  *>
  <*- checknil     *>
  <*- ioverflow    *>
  <*- coverflow    *>
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
  <*+ GENSIZE      *>
<* ELSIF __GEN_X86__ THEN *>
  <* -SPACE        *>
  <* +PROCINLINE   *>
<* END *>
<*+ WOFF310      *>

IMPLEMENTATION MODULE xrMM; (* VitVit'n'Hady *)

IMPORT
  SYSTEM,
  RTS := xmRTS,
  xrBlockManager,
  xrDTree,
  xosMalloc,
  X2C,
  xrtsOS;

<* IF MULTITHREAD THEN *>
  IMPORT xosThreads;
<* END *>

<* IF XMM_DEBUG THEN *>
   IMPORT fmt:=FormStr;
<* END *>

FROM xmRTS IMPORT
              X2C_busymem, X2C_usedmem, X2C_maxmem, X2C_threshold, X2C_objects,
              X2C_LINK;

FROM SYSTEM IMPORT ADDRESS, ADR, ADDADR, SUBADR, SHIFT;

CONST
  AdrLss = X2C.X2C_adr_lss;
  AdrGtr = X2C.X2C_adr_gtr;

CONST
  GAP_SIZE = GAP_LEN*SIZE(CARDINAL);

  MaxSmall  = Max*LINK_SZ;
  MinLarge  = NormalBlockSize - (Max+1)*LINK_SZ;

TYPE
  ADDR_REF = POINTER TO ADDRESS;

CONST
  malloc = xrtsOS.X2C_malloc;
  free   = xrtsOS.X2C_free;

<* IF MULTITHREAD THEN *>
VAR
  lock_a: xosThreads.X2C_Mutex;
<* END *>

PROCEDURE HeapLock();
BEGIN
<* IF MULTITHREAD THEN *>
  xosThreads.X2C_EnterMutex (lock_a);
<* END *>
END HeapLock;


PROCEDURE HeapUnlock();
BEGIN
<* IF MULTITHREAD THEN *>
  xosThreads.X2C_ExitMutex (lock_a);
<* END *>
END HeapUnlock;

PROCEDURE  Align(size: CARDINAL): CARDINAL;
  VAR i: CARDINAL;
BEGIN
  i:=(size+(LINK_SZ+LINK_SZ-1)) DIV LINK_SZ;
  RETURN i*LINK_SZ;
END Align;

PROCEDURE SetNil ( a :ADDRESS; size :SIZE_T);
BEGIN
  RTS.X2C_ZEROMEM (a, ((size+3) DIV 4));
END SetNil;


<* IF XMM_DEBUG & (ADB OR SDB OR MDB) THEN *>

  PROCEDURE db(f-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
    VAR buf: ARRAY [0..999] OF CHAR;
  BEGIN
    fmt.sprintf (buf, f, x);
    xrtsOS.X2C_StdOut(buf,LENGTH(buf));
  END db;

<* END *>

PROCEDURE  SetGap(a: SYSTEM.ADDRESS);
  VAR p: POINTER TO ARRAY [0..GAP_LEN-1] OF CARDINAL; i: CARDINAL;
BEGIN
  p:=a;
  FOR i:=0 TO GAP_LEN-1 DO p^[i]:=MAGIC_G END;
END SetGap;

(*
   For better performance, the compiler generates code reading from memory with double
   word addresing mode even though the value being read is 1 byte in size. It
   may cause the access violation hardware exception at run-time in the case when
   the value being read lays in the end of a physical memory page. The solution
   (or workaround) implemented is to remain some padding in the end of each heap
   block.
*)

-----

(* for SMALL & NORMAL objects only!!! *)
PROCEDURE getHpObjSize (obj :RTS.X2C_LINK) :CARDINAL;
BEGIN
  RETURN CARDINAL(BITSET(obj^.size)*SZ_MASK);
END getHpObjSize;

-----

PROCEDURE stampObjAsFree (obj :RTS.X2C_LINK; size :CARDINAL);
BEGIN
  (* clean all tags, set size & "_free" tag *)

  obj^.size := CARDINAL(obj^.tags*BOFS_MASK+{_free}) + size;
END stampObjAsFree;

-----

PROCEDURE getFirstBlockAdr (b :Block) :ADDRESS;
BEGIN
  RETURN b + MEMORY_OFS;
END getFirstBlockAdr;


PROCEDURE getLastBlockAdr1 (b :Block) :ADDRESS;
BEGIN
  RETURN b + MEMORY_OFS + b^.size;
END getLastBlockAdr1;


PROCEDURE getLargeObjBlock (largeObj :RTS.X2C_LINK) :Block;
BEGIN
  RETURN Block(largeObj - MEMORY_OFS);
END getLargeObjBlock;

CONST
  BOFS_BIT = 21-3;

(* returns block descriptor for object *)
PROCEDURE getObjBlock (obj :RTS.X2C_LINK) :Block;
VAR
  ofs :CARDINAL;
BEGIN
  ofs := CARDINAL( SHIFT(BITSET(obj^.size)*BOFS_MASK, -BOFS_BIT) );
  RETURN Block(obj - ofs);
END getObjBlock;

(* set bit fields for object size & block offset *)
PROCEDURE setObjOfsLen (VAR obj :RTS.X2C_LINK; size, ofs :CARDINAL);
BEGIN
  ASSERT (ofs MOD LINK_SZ = 0);

  obj^.size := CARDINAL( SHIFT(BITSET(ofs),+BOFS_BIT) ) + size;
END setObjOfsLen;


(*  cuts the required object from the beginning

    returns a remainder object
 *)
PROCEDURE splitFreeObj (obj :RTS.X2C_LINK; size :CARDINAL) :RTS.X2C_LINK;
VAR
  rem     :RTS.X2C_LINK;   -- remainder object
  remOfs  :CARDINAL;
  remSize :CARDINAL;
BEGIN
  ASSERT (size MOD LINK_SZ = 0);
  ASSERT (_free IN obj^.tags);

  rem := obj + size;
  remOfs  := CARDINAL(obj^.tags*(-SZ_MASK)) + CARDINAL( SHIFT(BITSET(size),+BOFS_BIT) );
  remSize := getHpObjSize(obj) - size;
--
  rem^.size := remOfs + remSize;

  DEC (obj^.size, remSize); -- set "size" as the old object size

  RETURN rem;
END splitFreeObj;


(*----------------------------------------------------------------*)

PROCEDURE  NewBlock(root: SHORTCARD; VAR b: Block; size: CARDINAL);

  PROCEDURE rt_alloc (size :CARDINAL) :ADDRESS;
  BEGIN
    IF (size = BlockSize) THEN
      RETURN xrBlockManager.alloc()
    ELSE
      RETURN malloc (size);
    END;
  END rt_alloc;

VAR
  msz :CARDINAL;
  m   :SYSTEM.ADDRESS;
BEGIN
  msz := size+BLOCK_SZ;

  <* IF XMM_DEBUG THEN *> INC(msz,GAP_SIZE*2); <* END *>

  IF (X2C_busymem + msz > X2C_maxmem) THEN
    m := NIL;
  ELSE
    m := rt_alloc(msz);
  END;
--
  IF (m = NIL) THEN b := NIL; RETURN END;

  <* IF XMM_DEBUG THEN *>
    SetGap(m); SetGap(SYSTEM.ADDADR(m,GAP_SIZE+BLOCK_SZ+size));
    b:=SYSTEM.ADDADR(m,GAP_SIZE);
    SetNil (b, BLOCK_SZ+size);
    INC(blockno);
  <* ELSE *>
    b:=m;
  <* END *>
  IF (blk_min=NIL) OR AdrLss(m,blk_min) THEN blk_min := m END;
  m := SYSTEM.ADDADR (m, msz);
  IF (blk_max=NIL) OR AdrGtr(m,blk_max) THEN blk_max := m END;

  b^.size  :=size;
  b^.fsum  :=size;
  b^.list  :=NIL;
  b^.root  :=root;
  IF (root IN {NORMAL,LARGE}) THEN
    b^.mem := b + size;      -- empty pool (all free objects are in free list)
    (* it's not used - just for safety *)
  ELSE
    b^.mem := getFirstBlockAdr(b);  -- free raw memory pool
  END;
  b^.magic:=MAGIC_B;

  INC(X2C_busymem,b^.size+BLOCK_SZ);
(* tie block *)
  b^.next:=f_blocks[root]^.next;
  b^.prev:=f_blocks[root];
  b^.prev^.next:=b;
  b^.next^.prev:=b;
END NewBlock;

PROCEDURE  MakeBusy(b: Block);
BEGIN
  ASSERT (b^.magic = MAGIC_B, 101);
  IF (b^.list # NIL) THEN RTS.X2C_ASSERT_F(102) END;
  (* untie block *)
  b^.next^.prev:=b^.prev;
  b^.prev^.next:=b^.next;
  (* tie block *)
  b^.next:=b_blocks[b^.root]^.next;
  b^.prev:=b_blocks[b^.root];
  b^.prev^.next:=b;
  b^.next^.prev:=b;
END MakeBusy;

PROCEDURE  MakeFree(b: Block);
BEGIN
  ASSERT(b^.magic = MAGIC_B, 103);
  ASSERT( b^.list # NIL, 104);

  (**** untie block ****)
  b^.next^.prev:=b^.prev;
  b^.prev^.next:=b^.next;

  (**** tie block ****)
  b^.next:=f_blocks[b^.root]^.next;
  b^.prev:=f_blocks[b^.root];
  b^.prev^.next:=b;
  b^.next^.prev:=b;
END MakeFree;

--------------------------------------------------------------------------------

(* PSEUDOMODULE foManager; *)


(* Free object manager *)

(* DEFINITION *)

  PROCEDURE foManager_add (obj :X2C_LINK); FORWARD;
  (* includes NORMAL size "obj" in the free object pool *)


  PROCEDURE foManager_del (obj :X2C_LINK); FORWARD;
  (* excludes NORMAL size "obj" from the free object pool

     NOTE: intended for merging purposes *)


  PROCEDURE foManager_get (size :CARDINAL) :X2C_LINK; FORWARD;
  (* gets object with required NORMAL size

     NOTE: it may cause object splitting or new block allocation *)

  PROCEDURE foManager_init (); FORWARD;


(* IMPLEMENTATION (* VitVit *) *)


CONST
  POOL_SIZE = BlockSize DIV LINK_SZ;

TYPE
  freeObj     = POINTER TO freeObjDesc;

  freeObjDesc = RECORD
                      link       :RTS.X2C_LINK_STR; -- for compatibility with busy object
                      prev, next :freeObj;          -- cut off from the list to merge

                      (* free object memory is following here *)
                    END;

VAR
  dummy_fo :ARRAY [0..POOL_SIZE-1] OF freeObjDesc;
  pool     :ARRAY [0..POOL_SIZE-1] OF freeObj;      -- pool of free objects with NORMAL size

---------------------

PROCEDURE getRootBySize (size :CARDINAL) :CARDINAL;
BEGIN
  RETURN (size-1) DIV LINK_SZ;
END getRootBySize;


PROCEDURE getRoot (obj :X2C_LINK) :CARDINAL;
BEGIN
  RETURN getRootBySize(getHpObjSize(obj));
END getRoot;

---------------------

PROCEDURE extractFromPool (root :CARDINAL) :X2C_LINK;
VAR
  obj :freeObj;
BEGIN
  obj := pool[root]^.next; ASSERT (obj # pool[root] (*not empty*), 1998);

  obj^.next^.prev := obj^.prev;
  obj^.prev^.next := obj^.next;

  RETURN X2C_LINK(obj);
END extractFromPool;


---------------------

(* cuts from the beginning of "obj" & adds a remainder object to the pool *)

PROCEDURE splitNonPooledFreeObj (obj :X2C_LINK; size :CARDINAL);
BEGIN
  foManager_add ( splitFreeObj((*VAR*) obj, size) );
END splitNonPooledFreeObj;


PROCEDURE splitNewBlock (size :CARDINAL) :X2C_LINK;
VAR
  f   :X2C_LINK;
  x   :Block;
BEGIN
  NewBlock (NORMAL, (*VAR*) x, NormalBlockSize);
  IF (x = NIL) THEN
    f := NIL;
  ELSE
    f := getFirstBlockAdr(x);
    setObjOfsLen ((*VAR*) f, NormalBlockSize, SYSTEM.DIFADRC(f,x));
    INCL (f^.tags, _free);

    splitNonPooledFreeObj((*VAR*) f, size);
  END;

  RETURN f;
END splitNewBlock;


---------------------


PROCEDURE foManager_add (obj :X2C_LINK);
VAR
  fo   :freeObj;
  root :CARDINAL;
BEGIN
  ASSERT (_free IN obj^.tags, 1999);
  fo := freeObj(obj);

  root := getRoot (obj);

  fo^.next       := pool[root]^.next;
  fo^.prev       := pool[root];
  fo^.prev^.next := fo;
  fo^.next^.prev := fo;

  xrDTree.ins(root);

END foManager_add;


PROCEDURE foManager_del (obj :X2C_LINK);
VAR
  fo   :freeObj;
BEGIN
  ASSERT (_free IN obj^.tags, 2000);
  fo := freeObj(obj);

  fo^.next^.prev := fo^.prev;
  fo^.prev^.next := fo^.next;

  xrDTree.del (getRoot(obj));

END foManager_del;


(*

PROCEDURE foManager_check_integrity_for_root (root: CARDINAL): BOOLEAN;
VAR
  obj  : freeObj;  -- current object
  count: CARDINAL; -- count of objects in the current objects' chain
BEGIN
  count := 0;
  obj := pool[root];
  IF obj # NIL THEN
    REPEAT
      INC (count);
      obj := obj^.next;
    UNTIL obj = pool[root];
    obj := pool[root];
    REPEAT
      DEC (count);
      obj := obj^.prev;
    UNTIL obj = pool[root];
  END;
  RETURN count = 0;
END foManager_check_integrity_for_root;


PROCEDURE foManager_check_integrity ();
VAR
  root: CARDINAL; -- current root 
BEGIN
  FOR root := 0 TO HIGH (pool) DO
    ASSERT (foManager_check_integrity_for_root (root), 0AE050474H);
  END;
END foManager_check_integrity;

*)


CONST
  LDV_Snowman_pad = 3;

PROCEDURE foManager_get (size :CARDINAL) :X2C_LINK;
VAR
  root   :CARDINAL;
  g_root :CARDINAL;     -- greater root
  obj    :X2C_LINK;
BEGIN
--  foManager_check_integrity ();

  root := getRootBySize(size);

  IF (pool[root]^.next = pool[root] (*empty list*)) THEN
    g_root := xrDTree.find (root);

    IF (g_root = xrDTree.ROOT_NOT_FOUND) THEN
      obj := splitNewBlock (size);        -- adds a remainder object to the pool
    ELSE
      (* element is already deleted from distribution tree by "find" *)
      ASSERT ( (root < g_root) & (g_root < xrDTree.numOfLeaves), 2002);

      obj := extractFromPool(g_root);
      IF (g_root - root > LDV_Snowman_pad) THEN    -- splits the object &
        splitNonPooledFreeObj ((*VAR*) obj, size); -- adds remainder to the pool
      END;
    END;
  ELSE
    obj := extractFromPool(root);
    xrDTree.del (root);
  END;

--  foManager_check_integrity ();

  RETURN obj;
END foManager_get;

-------

PROCEDURE foManager_init();
VAR
  i :CARDINAL;
BEGIN
  xrDTree.init();
  ASSERT(POOL_SIZE = xrDTree.numOfLeaves);

  FOR i:=0 TO POOL_SIZE-1 DO
    dummy_fo[i].prev := ADR (dummy_fo[i]);
    dummy_fo[i].next := dummy_fo[i].prev;

    pool[i] := ADR(dummy_fo[i]);
  END;

  ASSERT ( SIZE(freeObjDesc) DIV LINK_SZ <= LDV_Snowman_pad);
END foManager_init;


(* END foManager *)


---------------------------------- ALLOCATOR -----------------------------------

PROCEDURE small_alloc(root: SHORTCARD; VAR l: RTS.X2C_LINK;
                      VAR z: Block; size: CARDINAL);
VAR
  p, f :RTS.X2C_LINK;
  i    :CARDINAL;
  x, y :Block;
BEGIN
  ASSERT(size = root * LINK_SZ,105);
  ASSERT (root IN {1..Max});
  y:=f_blocks[root]; x:=y^.next;
  IF (x = y) THEN
    ASSERT(parts[root]*size <= SmallBlockSize,107);
    NewBlock(root, x, SmallBlockSize);
    IF (x = NIL) THEN z := NIL; l := NIL; RETURN END;
    ASSERT(x^.magic=MAGIC_B,108);
  END;
  ASSERT(x^.magic = MAGIC_B, 109);

  IF (x^.list = NIL) THEN   -- take an object from free raw memory pool
    l      := x^.mem;
    x^.mem := l + size;
    ASSERT (CARDINAL(x^.mem) <= CARDINAL(x + MEMORY_OFS + SmallBlockSize)); 
  ELSE                      -- take an object from free list
    l       := x^.list;
    x^.list := l^.next;

    ASSERT (_free IN l^.tags, 1091);
    EXCL (l^.tags, _free);
  END;

  DEC(x^.fsum, size);

  IF (x^.fsum < size) & (x^.list = NIL) THEN
    MakeBusy(x);
  END;

  ASSERT(x^.magic=MAGIC_B,110);
  z:=x;
  <*IF ADB THEN*> db('small(%05x,%d)\n',l,size); <*END*>
END small_alloc;


PROCEDURE large_alloc(VAR l: RTS.X2C_LINK; VAR x: Block; size: CARDINAL);
BEGIN
  ASSERT(size MOD LINK_SZ = 0);

  NewBlock (LARGE, x, size);
  IF x=NIL THEN
    l:=NIL
  ELSE
    l       := getFirstBlockAdr(x);
    l^.tags := {};
    <*IF ADB THEN*> db('large(%05x,%d)\n',l,size); <*END*>
  END;
END large_alloc;

PROCEDURE  allocate(VAR l: RTS.X2C_LINK; size: CARDINAL; clean :BOOLEAN);

VAR
  sz :CARDINAL;

  PROCEDURE Collect;
  BEGIN
    IF NOT GCAUTO THEN RETURN END;
    (*
       GCInvoker scack scanning mechanism.

       Procedures: xrMM.mod       allocate$Collect,  X2C_COLLECT
                   LocalHead.mod  allocBlock$Collect
                   xrO2MM.mod     MarkStack

       GCInvoker is a thread (coroutine) that implicitly invoked GC
       For such thread we can scan stack up to frame of 'allocate'
       to omit scanning of locals of 'allocate'.

       Note: In every place where GC is invoked we should set GCInvoker
             to zero or set GCInvoker to RTS.X2C_GetCurrent().
             In latter case StackEnd4GC should be set.

       Problem: if there is a register with reference that wasn't saved in
       the upper frame and saved only in frame of 'allocate', then
       the memory location on the stack where this register is saved
       won't be scanned.
       
       That is why this machinery is commented out.
    *)
    GCInvoker   := NIL; -- RTS.X2C_GetCurrent();
    StackEnd4GC := NIL; -- ADR (l);
    SizeToAlloc := sz;;
    COLLECTOR;
  END Collect;

  VAR
    b      :Block;
    n      :CARDINAL;
    tryCnt :CARDINAL;

  <* IF XMM_DEBUG THEN *> x: Block; <* END *>
BEGIN
  (* DO NOT change the lock/unlock order to escape *)
  HeapLock();
    sz := Align(size);
    IF (sz <= MaxSmall) THEN   (* small block *)
      n := sz DIV LINK_SZ;

      tryCnt := 0;
      LOOP
        small_alloc (n, l, b, sz);

        IF (l # NIL) THEN EXIT; END;

        IF (tryCnt = 2) THEN
          HeapUnlock();
          RETURN
        END;

        DoDefrag := (tryCnt = 1);
        Collect;

        INC (tryCnt);
      END;

      (* put size & containing block offset *)
      setObjOfsLen ((*VAR*) l, sz, SYSTEM.DIFADRC(l,b));

    ELSIF (sz > MinLarge) THEN (* large block *)

      tryCnt := 0;
      LOOP
        large_alloc(l,b,sz);

        IF (l # NIL) THEN EXIT; END;

        IF (tryCnt = 2) THEN
          HeapUnlock();
          RETURN
        END;

        DoDefrag := (tryCnt = 1);
        Collect;

        INC (tryCnt);
      END;

      b^.fsum := sz;
      l^.tags := {};
    ELSE

      tryCnt := 0;
      LOOP
        l := foManager_get (sz); --normal_alloc(l, sz);

        IF (l # NIL) THEN EXIT; END;

        IF (tryCnt = 2) THEN
          HeapUnlock();
          RETURN
        END;

        DoDefrag := (tryCnt = 1);
        Collect;

        INC (tryCnt);
      END;

      sz := getHpObjSize(l);

    END;
    IF (clean) THEN RTS.X2C_ZEROMEM (l+LINK_SZ, (sz-LINK_SZ) DIV 4) END;

    EXCL (l^.tags, _free);
    l^.td := RTS.x2c_td_null;
    INCL(l^.tags,_expbit); (* Hady, 28/11/96: Workaround to multithread problem *)
    INC(X2C_usedmem, sz);
    INC(X2C_objects);
  HeapUnlock();
END allocate;

(*--------------------------------------------------------------------------*)

PROCEDURE _free_block(x: Block);

  PROCEDURE rt_free (adr :ADDRESS; size :CARDINAL);
  BEGIN
    IF (size = BlockSize) THEN
      xrBlockManager.free(adr)
    ELSE
      free (adr, size)
    END;
  END rt_free;

(* root never points to x - dummy-block exists *)
VAR
  msz       :CARDINAL;
  blockSize :CARDINAL;
BEGIN
  <*IF SDB THEN*> db('free_block(%x)\n',x); <*END*>

  ASSERT(x^.magic = MAGIC_B, 119);
  ASSERT(x # f_blocks[x^.root], 120);
  ASSERT(x # b_blocks[x^.root], 121);

  blockSize := x^.size;
  x^.next^.prev := x^.prev;
  x^.prev^.next := x^.next;

  DEC(X2C_busymem, blockSize+BLOCK_SZ);
  <* IF XMM_DEBUG THEN *>
  DEC(blockno);

  msz := blockSize + BLOCK_SZ + GAP_SIZE * 2;
  SYSTEM.FILL (SYSTEM.SUBADR(x,GAP_SIZE), 0, msz);
  rt_free( SYSTEM.SUBADR(x,GAP_SIZE), msz);

  <* ELSE *>
  rt_free(x, blockSize + BLOCK_SZ);
  <* END *>
END _free_block;


PROCEDURE  free_block(x: Block);
(* root never points to x - dummy-block exists *)
BEGIN
  HeapLock();
    _free_block(x);
  HeapUnlock();
END free_block;


PROCEDURE dealloc(objd: RTS.X2C_LINK);
VAR
  x       :Block;
  size    :CARDINAL;
  _1stObj :RTS.X2C_LINK;
  _2ndObj :RTS.X2C_LINK;
  rObj    :RTS.X2C_LINK;
  eom     :CARDINAL;
BEGIN
  size := getHpObjSize(objd);

  (* gets object's block *)
  x := getObjBlock(objd);
    ASSERT(x^.magic = MAGIC_B, 122);
  <*IF ADB THEN*> db('dealloc: %x %d\n',objd,size); <*END*>

  HeapLock();

  IF (x^.root = NORMAL) THEN
    _1stObj := getFirstBlockAdr (x);
    _2ndObj := _1stObj + getHpObjSize(_1stObj);

    IF (objd = _2ndObj) & (_free IN _1stObj^.tags) THEN
      foManager_del (_1stObj);
      INC (_1stObj^.size, size);   -- merge 1st with 2nd object being deallocated
      objd := _1stObj;
    END;

    (* make right merge *)
    eom := CARDINAL(getLastBlockAdr1(x));
    LOOP
      rObj := objd + getHpObjSize (objd);
      IF (CARDINAL(rObj) >= eom) OR ~(_free IN rObj^.tags) THEN
        ASSERT (CARDINAL(objd) <= eom);
        EXIT;
      END;
      INC (objd^.size, getHpObjSize(rObj));
      foManager_del (rObj);
    END;

    IF (getHpObjSize(objd) = NormalBlockSize) THEN
      _free_block(x);
    ELSE
      stampObjAsFree(objd, getHpObjSize(objd));
      foManager_add (objd);
    END;
  ELSIF (x^.root =  LARGE) THEN
    _free_block(x);
  ELSE                      -- SMALL object
    INC(x^.fsum, size);

    IF (x^.fsum = x^.size) THEN
       _free_block(x)
    ELSE
      objd^.next := x^.list;
      x^.list := objd;
      stampObjAsFree(objd, getHpObjSize(objd));
    END;
  END;

  HeapUnlock();
END dealloc;

(*--------------------------------------------------------------------------*)

PROCEDURE CheckOneBlock (head :Block);
VAR
  i, sz   :CARDINAL;
  f, l, n :RTS.X2C_LINK;
  e       :ADDRESS;
  p       :POINTER TO ARRAY [0..GAP_LEN-1] OF CARDINAL;
BEGIN
<* IF XMM_DEBUG THEN *>
  ASSERT(head^.magic = MAGIC_B,127);

  p := SYSTEM.SUBADR(head,GAP_SIZE);
  FOR i:=0 TO GAP_LEN-1 DO
    ASSERT(p^[i]=MAGIC_G,131);
  END;
  p := getLastBlockAdr1(head);
  FOR i:=0 TO GAP_LEN-1 DO
    ASSERT(p^[i]=MAGIC_G,132);
  END;

  IF (head^.root = NORMAL) THEN
    f := head^.list;             (* list of free objects *)
    l := getFirstBlockAdr(head);
    e := l + head^.size;
    ASSERT(AdrLss(l,e),133);

    WHILE AdrLss(l,e) DO
      ASSERT((f=NIL) OR (f^.next=NIL) OR AdrLss(f,f^.next),134);
      sz := getHpObjSize(l);
      ASSERT((l=f) OR (l^.td^.res = MAGIC_T),136);
      (* sz includes LINK_SZ! *)
      IF (l = f) THEN f:=f^.next END;
      l := ADDADR(l, sz);
    END;
  ELSIF (head^.root # LARGE) THEN  (* block of small objects *)

    f := head^.list;         (* list of free parts *)
    WHILE (f # NIL) DO
      ASSERT(_free IN  f^.tags);
      f := f^.next;
    END;

    l := ADDADR(head, BLOCK_SZ);
    ASSERT (NOT AdrGtr(l, head^.mem));

    WHILE AdrLss(l, head^.mem) DO
      sz := getHpObjSize(l);
      ASSERT(sz = LINK_SZ*head^.root, 135);
      ASSERT((_free IN  l^.tags) OR (l^.td^.res = MAGIC_T),136);
      (* sz includes LINK_SZ! *)
      l := ADDADR(l, sz);
    END;
  END;
<* END *>
END CheckOneBlock;

PROCEDURE CheckHeap();
VAR
  i  :CARDINAL;
  b  :Block;
  no :CARDINAL;
BEGIN
  ASSERT (LARGE = Max+1);
  no := 0;

  (* check free blocks,
     NOTE: LARGE block cannot be free *)
  FOR i:=0 TO LARGE DO
    b := f_blocks[i]^.next; -- skip dummy block
    WHILE (b # f_blocks[i]) DO
      IF (i # LARGE) THEN
        CheckOneBlock (b);
      END;
      b := b^.next;
      INC (no);
    END;

    b := b_blocks[i]^.next;
    WHILE (b # b_blocks[i]) DO
      CheckOneBlock (b);
      b := b^.next;
      INC (no);
    END;
  END;
  ASSERT(no = blockno, 137);
END CheckHeap;


PROCEDURE ["C"] X2C_InitDesc(d: Dynarr; VAR size: SIZE_T;
                       lens-: ARRAY OF RTS.X2C_LENS_TYPE; dims: SIZE_T);
  VAR i: CARDINAL;
BEGIN
  d^.n[(dims-1)*2] := lens[0];
  FOR i:=1 TO dims-1 DO
    size := size*lens[dims-i];
    d^.n[i*2-1] := size;
    d^.n[(dims-i)*2-2] := lens[i];
  END;
  size := size*lens[0];
END X2C_InitDesc;

PROCEDURE  DynarrDescSize(i: SIZE_T): SIZE_T;
  TYPE D = RECORD a: ADDRESS; n: ARRAY [0..0] OF SIZE_T END;
BEGIN
  ASSERT(i < MaxDim,124);
  RETURN SIZE(D)+i*2*SIZE(SIZE_T);
END DynarrDescSize;


PROCEDURE stampAsNonConservative (obj :SYSTEM.ADDRESS);
VAR l :RTS.X2C_LINK;
BEGIN
  l := SYSTEM.SUBADR (obj, LINK_SZ);
  IF (l^.tags*BOFS_MASK = {}) THEN -- large object
    INCL (l^.tags, _notstack);
  END;
END stampAsNonConservative;

(*----------------------------------------------------------------*)

PROCEDURE ["C"] DummyCollector;
BEGIN
  (* Does nothing, You see? *)
END DummyCollector;

VAR
  null_offs: ARRAY [0..0] OF ADDRESS;
  ptr_offs : ARRAY [0..1] OF ADDRESS;
  mod_desc : RTS.X2C_MD_STR;

PROCEDURE  ini_type_desc(VAR x: RTS.X2C_TD;
                           nm-: ARRAY OF CHAR;
                                    sz: CARDINAL; offs: ADDRESS);
        TYPE str = POINTER TO ARRAY [0..9999] OF CHAR;
  VAR i: INTEGER; len: SIZE_T; X: str;
BEGIN
  x:=SYSTEM.CAST(RTS.X2C_TD,malloc(SIZE(x^)));
  IF x=NIL THEN RTS.X2C_TRAP_F(X2C.X2C_noMemoryException) END;
  x^.offs:=offs;
  x^.size:=sz;
  len:=LENGTH(nm)+1;
  x^.name:=SYSTEM.CAST(RTS.X2C_pCHAR,malloc(len));
        X:=SYSTEM.CAST(str,x^.name);
  IF X=NIL THEN RTS.X2C_TRAP_F(X2C.X2C_noMemoryException) END;
  COPY(nm,X^);
  x^.module:=SYSTEM.ADR(mod_desc);
  x^.methods:=0;
  x^.level:=0;
  FOR i:=0 TO HIGH(x^.base) DO x^.base[i]:=NIL END;
  x^.proc:=NIL;
  x^.succ:=NIL;
  x^.link:=NIL;
  x^.tail:=NIL;
  x^.res:=MAGIC_T;
  x^.next:=mod_desc.types;
  mod_desc.types:=x;
END ini_type_desc;


VAR BlocksHeap: ARRAY [0..(Max+1)*2+1] OF BlockDesc;

PROCEDURE ["C"] / Printf_BEGIN();

PROCEDURE ["C"] X2C_GC_INIT(auto: BOOLEAN; thres,heap: SYSTEM.CARD32) :BOOLEAN;
  VAR i,j: CARDINAL; x: Block;
BEGIN

  (* first, check that NIL = 0; used at least in SetNil *)
  x:=SYSTEM.ADR(i); (* just none-NIL value *)
  SYSTEM.FILL(SYSTEM.ADR(x),SYSTEM.CAST(SYSTEM.BYTE,0C),SIZE(x));
  IF x#NIL THEN
    RTS.X2C_ASSERT_F(100);
  END;

  FloatingHeaplimit := FALSE;

  IF heap = 0 THEN
    FloatingHeaplimit := TRUE;
    heap := InitialFloatingMaxMem;
  END;

  xrtsOS.X2C_InitHeap(heap, FALSE);

  IF NOT xrBlockManager.init (BlockSize) THEN
    RTS.XDSLIB_INITIALIZATION_FAILED := TRUE;
    RETURN FALSE;
  END;

  IF (heap > 0) & (CARDINAL(heap) > xrBlockManager.heapLimit) THEN
    heap := xrBlockManager.heapLimit;
  END;

  foManager_init();

  <* IF MULTITHREAD THEN  *>
    xosThreads.X2C_CreateMutex (lock_a);
  <* END *>

(*  RTS.X2C_INIT_RTS; *)
  O2MM_init     := FALSE;
  COLLECTOR     := DummyCollector;
  DoDefrag      := FALSE;
  GCAUTO        := auto;
  GCTHRESCNT    := 0;

  anchorTracing := FALSE;
  anchorWeightThreshold := 0;

  heapTracing := FALSE;
  heapTracingThreshold := 0;

  X2C_AlwaysDefrag := FALSE;

  IF (thres <= 0) THEN
    RTS.X2C_threshold:=MAX(CARDINAL);
  ELSE
     RTS.X2C_threshold:=thres;
  END;
--
  IF (heap <= 0) THEN
    RTS.X2C_maxmem:=MAX(CARDINAL);
  ELSE
    RTS.X2C_maxmem:=heap;
  END;
  (* check enviroment *)

  (* ASSERT NOTE: folowing ASSERTs are constant! let them be *)
  ASSERT(SIZE(INTEGER)=4);
  ASSERT(BLOCK_SZ >= SIZE(BlockDesc));
  ASSERT(SmallBlockSize MOD LINK_SZ = 0);
--  ASSERT(SmallBlockSize + BLOCK_SZ <= 100000000H DIV SMUL);
--  ASSERT((SmallBlockSize + BLOCK_SZ) DIV LINK_SZ <= SMUL DIV OMUL);  -- offset can be put in 11 bits
  ASSERT(NormalBlockSize MOD LINK_SZ = 0);
--  ASSERT(NormalBlockSize + BLOCK_SZ <= 100000000H DIV SMUL);
--  ASSERT((NormalBlockSize + BLOCK_SZ) DIV LINK_SZ <= SMUL DIV OMUL); -- offset can be put in 11 bits

  RTS.X2C_MODULES:=NIL;
  ptr_offs[0]:=X2C.X2C_BASE;
  ptr_offs[1]:=X2C.X2C_OFS_END;
  null_offs[0]:=X2C.X2C_OFS_END;
  mod_desc.types:=NIL;
  ini_type_desc(RTS.x2c_td_ptr,"$PTR",SIZE(ADDRESS),SYSTEM.ADR(ptr_offs));
  ini_type_desc(RTS.x2c_td_null,"$NULL",1,SYSTEM.ADR(null_offs));
  (*-----*)
  j:=0;
  FOR i:=0 TO Max+1 DO
    x:=SYSTEM.CAST(Block,SYSTEM.ADR(BlocksHeap[i*2]));
    x^.prev:=x; x^.next:=x;
    x^.size:=0; x^.root:=i;
    x^.fsum:=0; x^.list:=NIL;
    x^.magic:=0;
    f_blocks[i]:=x;
    x:=SYSTEM.CAST(Block,SYSTEM.ADR(BlocksHeap[i*2+1]));
    x^.prev:=x; x^.next:=x;
    x^.size:=0; x^.root:=i;
    x^.fsum:=0; x^.list:=NIL;
    x^.magic:=0;               -- intentionally # MAGIC_B
    b_blocks[i]:=x;
  END;

  FOR i:=1 TO Max DO
    parts[i] := SmallBlockSize DIV (i*LINK_SZ);
  END;

  <* IF XMM_DEBUG THEN *>
    blockno:=0;
    blk_min:=NIL;
    blk_max:=NIL;
    cur_td:=NIL;
  <* END *>
  RTS.X2C_objects   := 0;
  RTS.X2C_usedmem   := 0;
  RTS.X2C_busymem   := 0;
  RTS.X2C_busylheap := 0;

--  Printf_BEGIN();      -- to print GC anchor trace

  RETURN TRUE;
END X2C_GC_INIT;


PROCEDURE ["C"] X2C_GC_INCREASE (auto :BOOLEAN; thres, heap :SYSTEM.CARD32);
BEGIN
(* ASSUME: GCAUTO, X2C_threshold, X2C_maxmem are already initialized *)

  IF heap = 0 THEN
    FloatingHeaplimit := TRUE;
    heap := InitialFloatingMaxMem;
  END;

  IF (heap > 0) & (CARDINAL(heap) > xrBlockManager.heapLimit) THEN
    heap := xrBlockManager.heapLimit;
  END;

  IF (thres <= 0) THEN
    RTS.X2C_threshold := MAX(CARDINAL)
  ELSIF ( RTS.X2C_threshold < CARDINAL(thres) ) THEN
    RTS.X2C_threshold := thres;
  END;

  GCAUTO := auto OR GCAUTO;

  IF (heap > 0) & (RTS.X2C_maxmem < CARDINAL(heap)) THEN
    (* heap increasing process is SOUND :
       if the heap was initialized as default (0), it is unincreasible
       because of X2C_maxmem is MAX(CARDINAL) in the case; otherwise
       X2C_maxmem contains its value - VitVit *)

    RTS.X2C_maxmem := heap;
    xrtsOS.X2C_InitHeap (heap, TRUE)
  END;

END X2C_GC_INCREASE;

PROCEDURE ["C"] X2C_COLLECT;
BEGIN
  HeapLock();
    GCInvoker   := NIL;
    StackEnd4GC := NIL;
    SizeToAlloc := 0;
    COLLECTOR;
  HeapUnlock();
END X2C_COLLECT;

PROCEDURE ["C"] X2C_MemoryManager_exit ();
BEGIN
  xrBlockManager.exit ();
  xosMalloc.X2C_DestroyHeap ();
END X2C_MemoryManager_exit;

END xrMM.