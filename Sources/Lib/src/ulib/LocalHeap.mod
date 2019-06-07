(* Copyright (c) XDS Ltd 1996,99. All Rights Reserved. *)

<*+ M2EXTENSIONS *>

<*-IOVERFLOW   *>
<*-COVERFLOW   *>
<*-CHECKDINDEX *>
<*-CHECKINDEX  *>
<*-CHECKRANGE  *>
<*-CHECKNIL    *>
<*-CHECKDIV    *>
<*-CHECKPROC   *>
<*-CHECKSET    *>
<*-CHECKTYPE   *>

<* IF __GEN_C__ THEN *>
  <* +GENSIZE *>
<* END *>

<* IF NOT DEFINED (CHECK_LOCALHEAP) THEN *>
  <* NEW CHECK_LOCALHEAP- *>
<* END *>

IMPLEMENTATION MODULE LocalHeap;

IMPORT SYSTEM, Storage, xrBlockManager, xrMM;

<* IF CHECK_LOCALHEAP THEN *>
IMPORT Printf;
<* END *>

FROM SYSTEM IMPORT ADDRESS, INT32, ADR, ADDADR, GET, CARD8;

FROM xmRTS IMPORT X2C_busymem, X2C_maxmem, X2C_busylheap, X2C_usedlheap;

TYPE
  ALLOC_WORD = INT32;

CONST
  ALIGN     = SIZE (ALLOC_WORD);

  PADDING   = ALIGN;
(*
   For better performance, the compiler generates code reading from memory with double
   word addresing mode even though the value being read is 1 byte in size. It
   may cause the access violation hardware exception at run-time in the case when
   the value being read lays in the end of a physical memory page. The solution
   (or workaround) implemented is to remain some padding in the end of each heap
   block.
*)

  BlockSize = ( xrMM.BlockSize - SIZE (ADDRESS)
                               - SIZE (INTEGER)
                               - PADDING        ) DIV ALIGN;
  MaxFree   = 128;

TYPE
    FreePtr     = POINTER TO FreeRecord;
    FreeRecord  = RECORD
                    next: FreePtr;
                  END;

    BlockPtr    = POINTER TO BlockRecord;
    BlockRecord = RECORD
                     next: BlockPtr;
                     n:    INTEGER;
                     v:    ARRAY [0..BlockSize-1] OF ALLOC_WORD;
                  END;



TYPE
  heapID = POINTER TO RECORD
             Used    :INTEGER;   -- used space of this local heap
             Split   :BOOLEAN;                          -- was there spilitting?
             Headers :ARRAY [0..MaxFree-1] OF FreePtr;  -- splitted free parts

             Current :BlockPtr;  -- current block with free space
             NFree   :INTEGER;   -- free space in the current block

             Large   :BlockPtr;  -- list of busy large blocks
           END;



<* IF CHECK_LOCALHEAP THEN *>

VAR
  FreeBlocks :BlockPtr;

PROCEDURE checkFreeSpace (a :ADDRESS; size :CARDINAL);
VAR b :CARD8;
    i :CARDINAL;
BEGIN
  FOR i := 0 TO size-1 DO
    GET (a + i, b);
    ASSERT (b = 0FEH);
  END;
END checkFreeSpace;

PROCEDURE setFreeSpace (a :ADDRESS; size :CARDINAL);
BEGIN
  SYSTEM.FILL (a, 0FEH, size);
END setFreeSpace;

<* END *>

--------------------------------------------------------------------------------

PROCEDURE allocBlock() :ADDRESS;

  PROCEDURE Collect;
  BEGIN
    IF NOT xrMM.GCAUTO THEN RETURN END;
    xrMM.HeapUnlock();        -- !!!!
    xrMM.GCInvoker   := NIL;
    xrMM.StackEnd4GC := NIL;
    xrMM.SizeToAlloc := xrMM.BlockSize;
    xrMM.COLLECTOR;
    xrMM.HeapLock();
  END Collect;

  PROCEDURE alloc() :ADDRESS;
  VAR
    p :ADDRESS;
  BEGIN
    IF (X2C_busymem + xrMM.BlockSize > X2C_maxmem) THEN
      p := NIL;
    ELSE
      p := xrBlockManager.alloc();
      INC (X2C_busymem, xrMM.BlockSize);
      INC (X2C_busylheap, xrMM.BlockSize);
    END;

    RETURN p;
  END alloc;

VAR
  p      :ADDRESS;
  tryCnt :CARDINAL;
BEGIN
  xrMM.HeapLock();

  <* IF CHECK_LOCALHEAP THEN *>
  IF (FreeBlocks # NIL) THEN
    p := FreeBlocks;
    FreeBlocks := FreeBlocks^.next;
    checkFreeSpace (ADR(BlockPtr(p)^.v), BlockSize*ALIGN);

    xrMM.HeapUnlock();
    RETURN p;
  END;
  <* END *>

  tryCnt := 0;
  LOOP
    p := alloc();

    IF (p # NIL) OR (tryCnt = 2) THEN EXIT END;

    xrMM.DoDefrag := (tryCnt = 1); (* defragmentation*)
    Collect ();

    INC (tryCnt);
  END;

  <* IF CHECK_LOCALHEAP THEN *>
  IF (p # NIL) THEN
    setFreeSpace (p, xrMM.BlockSize);
  END;
  <* END *>

  xrMM.HeapUnlock();

  RETURN p;
END allocBlock;


PROCEDURE freeBlock (adr :ADDRESS; destroy :BOOLEAN);
BEGIN
  <* IF CHECK_LOCALHEAP THEN *>
  IF NOT destroy THEN
    xrMM.HeapLock();

    setFreeSpace (adr, xrMM.BlockSize);

    BlockPtr(adr)^.next := FreeBlocks;
    FreeBlocks := adr;

    checkFreeSpace (ADR(BlockPtr(adr)^.v), BlockSize*ALIGN);

    xrMM.HeapUnlock();

    RETURN;
  END;
  <* END *>

  DEC (X2C_busymem, xrMM.BlockSize);
  DEC (X2C_busylheap, xrMM.BlockSize);
  xrBlockManager.free(adr);
END freeBlock;

--------------------------------------------------------------------------------

<* IF CHECK_LOCALHEAP THEN *>

PROCEDURE checkNotFree (h :heapID; p :ADDRESS; root :INTEGER);
  PROCEDURE overlap (p1 :ADDRESS; r1 :INTEGER; p2 :ADDRESS; r2 :INTEGER) :BOOLEAN;
  BEGIN
    IF p1 = p2 THEN RETURN TRUE; END;
    IF CARDINAL (p1) < CARDINAL (p2) THEN
      RETURN (CARDINAL (p1) + CARDINAL(r1)*ALIGN) > CARDINAL (p2);
    ELSE
      RETURN (CARDINAL (p2) + CARDINAL(r2)*ALIGN) > CARDINAL (p1);
    END;
  END overlap;

VAR f :FreePtr;
    i :INTEGER;
BEGIN
  WITH h^ DO
    FOR i := 0 TO MaxFree-1 DO
      f := Headers [i];
      WHILE (f # NIL) DO
        IF overlap (f, i, p, root) THEN
          Printf.printf ("Trying to free %XH (size %d), overlapping with %XH (size %d)\n", p, root*ALIGN, f, i*ALIGN);
          ASSERT (FALSE);
        END;
        f := f^.next;
      END;
    END;
  END;
END checkNotFree;

<* END *>

--------------------------------------------------------------------------------

PROCEDURE ALLOCATE (h :heapID; VAR p: ADDRESS; n: INTEGER);


VAR
  q: BlockPtr;
  r: FreePtr;
  i: INTEGER;
  w: BOOLEAN;
BEGIN
  WITH h^ DO
    ASSERT (n > 0);
    n := (n + ALIGN - 1) DIV ALIGN;
    IF (n < MaxFree) & (Headers [n] <> NIL) THEN
        p := Headers [n];
        Headers [n] := Headers [n]^.next;
        INC (Used, n);
        INC (X2C_usedlheap, n*ALIGN);
        <* IF CHECK_LOCALHEAP THEN *>
        checkFreeSpace (p + SIZE(FreeRecord), n*ALIGN - SIZE(FreeRecord));
        <* END *>
    ELSIF n <= NFree THEN
        DEC (NFree, n);
        p := ADR (Current^.v[NFree]);
        INC (Used, n);
        INC (X2C_usedlheap, n*ALIGN);
        <* IF CHECK_LOCALHEAP THEN *>
        checkFreeSpace (p, n*ALIGN);
        <* END *>
    ELSIF n > BlockSize THEN
        Storage.ALLOCATE (q, SIZE (BlockPtr) + SIZE (INTEGER) +
                             VAL (CARDINAL, n) * ALIGN);         -- LARGE block
        IF (q = NIL) THEN
          p := NIL; RETURN;
        END;
        q^.n := n;
        q^.next := Large;
        Large   := q;
        p := ADR (q^.v [0]);
    ELSE
        w := FALSE;
        LOOP
            IF (n >= MaxFree) OR w OR NOT Split THEN
                q := allocBlock();
                IF (q # NIL) THEN
                    q^.n    := BlockSize;
                    q^.next := Current;
                    Current := q;
                    NFree   := BlockSize - n;
                    p       := ADR (q^.v [NFree]);
                    Split   := FALSE;
                    INC (Used, n);
                    INC (X2C_usedlheap, n*ALIGN);
                    RETURN;
                ELSIF n >= MaxFree THEN
                    p := NIL; RETURN;
                END;
            END;
            FOR i:=n+1 TO MaxFree-1 DO
                IF Headers [i] <> NIL THEN
                    p := Headers [i];
                    Headers [i] := Headers [i]^.next;
                    r := ADDADR (p, VAL (CARDINAL, n * ALIGN));
                    <* IF CHECK_LOCALHEAP THEN *>
                      checkNotFree (h, r, i-n);
                      checkFreeSpace (p + SIZE(FreeRecord), n*ALIGN - SIZE(FreeRecord));
                    <* END *>
                    r^.next := Headers [i - n];
                    Headers [i - n] := r;
                    Split := TRUE;
                    INC (Used, n);
                    INC (X2C_usedlheap, n*ALIGN);
                    RETURN;
                END;
            END;
            IF w OR NOT Split THEN
              p := NIL; RETURN;
            END;
            Split := TRUE;
            w     := TRUE;
        END;
    END;
  END;
END ALLOCATE;

--------------------------------------------------------------------------------

PROCEDURE DEALLOCATE (h :heapID; VAR p: ADDRESS; n: INTEGER);
VAR
  q :FreePtr;
BEGIN
  WITH h^ DO
    ASSERT (n > 0);
    n := (n + ALIGN - 1) DIV ALIGN;
    <* IF CHECK_LOCALHEAP THEN *>
      checkNotFree (h, p, n);
      setFreeSpace (p, n*ALIGN);
    <* END *>
    IF n < MaxFree THEN
        q := p;
        q^.next := Headers [n];
        Headers [n] := q;
    END;
    DEC (Used, n);
    DEC (X2C_usedlheap, n*ALIGN);
    p := NIL;
  END;
END DEALLOCATE;

--------------------------------------------------------------------------------

PROCEDURE DEALLOCATE_ALL (h :heapID);
VAR
  i    :INTEGER;
  p, q :BlockPtr;
BEGIN
  WITH h^ DO
    (* free standard blocks *)
    p := Current^.next;
    WHILE (p # NIL) DO
      q := p^.next;
      freeBlock(p, FALSE);
      p := q;
    END;
    NFree := Current^.n;
    Current^.next := NIL;
    <* IF CHECK_LOCALHEAP THEN *>
    setFreeSpace (ADR(Current^.v), BlockSize*ALIGN);
    <* END *>

    (* free large blocks *)
    WHILE (Large # NIL) DO
      q := Large^.next;
      Storage.DEALLOCATE (Large, SIZE (BlockPtr) + SIZE (INTEGER) +
                                 VAL (CARDINAL, Large^.n) * ALIGN);
      Large := q;
    END;

    FOR i:=0 TO MaxFree-1 DO
      Headers [i] := NIL;
    END;
    DEC (X2C_usedlheap, Used*ALIGN);
    Used  := 0;
    Split := FALSE;
  END;
END DEALLOCATE_ALL;

--------------------------------------------------------------------------------
PROCEDURE Create(VAR h :heapID);
VAR
  i :INTEGER;
BEGIN
  ASSERT ( xrMM.BlockSize = SIZE(BlockRecord) + PADDING );

  Storage.ALLOCATE (h, SIZE(h^));
  WITH h^ DO
    FOR i:=0 TO MaxFree-1 DO
        Headers [i] := NIL;
    END;
    Current       := allocBlock();
    Current^.n    := BlockSize;
    Current^.next := NIL;
    NFree         := BlockSize;
    Split         := FALSE;
    Used          := 0;

    Large         := NIL;
  END;
END Create;

--------------------------------------------------------------------------------

PROCEDURE Destroy (h :heapID);
BEGIN
  DEALLOCATE_ALL(h);
  freeBlock (h^.Current, TRUE);

  Storage.DEALLOCATE (h, SIZE(h^));
END Destroy;
END LocalHeap.
