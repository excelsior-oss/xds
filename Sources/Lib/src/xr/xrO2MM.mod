
(* Copyright (c) 2005 Excelsior, Russia. All Rights Reserved. *)
(* Memory Manager. Oberon-2 specific operations. *)

<* NEW ADB-      *>   (* allocate *)
<* NEW SDB-      *>   (* sweep *)
<* NEW MDB-      *>   (* mark  *)
<* NEW STAT-     *>   (* statistics *)
<* NEW FULLSTAT- *>   (* full statistics (STAT should be set) *)

<* IF NOT DEFINED(GC_IN_COROUTINE) THEN *>

(* In JET, we must not use coroutines for GC, because manual stack 
 * switching mechanism confuses POSIX thread library (under Linux, of 
 * course).
 *
 * In short, the thread library uses its own special frames in each
 * thread's stack. When GC performs stack switch, these frames became
 * lost for the thread library.
 *)

<* NEW GC_IN_COROUTINE+ *>
<* END *>

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
<* ELSE *>
  <* +PROCINLINE   *>
<* END *>
<*+ WOFF310      *>
IMPLEMENTATION MODULE xrO2MM;

IMPORT
  SYSTEM,
  xrMM,
  TERMINATION,
  rts := xmRTS,
  xosMalloc,
  xosMem,
  xrBlockManager,
  X2C,
<* IF NOT GC_IN_COROUTINE THEN *>
  xrnCoroutines,
<* END *>
<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>
  xosThreads, xosFmtIO, xWin32,
<* END *>
  p:=Printf, xrtsOS;

<* IF env_target = "x86nt" THEN *>
  IMPORT xrPEfmt;
<* END *>

<* IF GC_IN_COROUTINE THEN *>
  FROM COROUTINES IMPORT COROUTINE, NEWCOROUTINE, TRANSFER;
<* END *>

FROM SYSTEM IMPORT ADR, ADDADR, SUBADR, CAST, MOVE, GET;

<* IF STAT THEN *>
  IMPORT stdio;
<* END *>

IMPORT TimeConv;

TYPE
  ADDRESS   = SYSTEM.ADDRESS;
  INT16     = SYSTEM.INT16;
  CARD32    = SYSTEM.CARD32;
  ADDR_REF  = POINTER TO ADDRESS;
  SIZE_T    = xrMM.SIZE_T;
  SIZE_P    = POINTER TO SIZE_T;

  Module    = rts.X2C_MD;
  Type      = rts.X2C_TD;
  Link      = rts.X2C_LINK;
  Block     = xrMM.Block;
  Destruct  = POINTER TO DestructDesc;

  DestructDesc = RECORD
    proc : rts.X2C_DPROC;
    adr  : ADDRESS;
    next : Destruct;
  END;

TYPE
  OFFS = POINTER TO ARRAY [0..9999] OF ADDRESS;

CONST
  LINK_SZ  = SIZE(rts.X2C_LINK_STR);
  GAP_SIZE = xrMM.GAP_LEN*SIZE(CARDINAL);

CONST
  AdrLss = X2C.X2C_adr_lss;
  AdrGtr = X2C.X2C_adr_gtr;

<* IF STAT THEN *>
VAR moved_objects: CARDINAL;
<* END *>

<* IF XMM_DEBUG & (ADB OR SDB OR MDB) THEN *>

  PROCEDURE db(f-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
        VAR buf: ARRAY [0..999] OF CHAR;
  BEGIN
    fmt.print(buf,f,x);
    xrtsOS.X2C_StdOut(buf,LENGTH(buf));
  END db;

<* END *>


VAR
  dynarrs : ARRAY [0..xrMM.MaxDim-1] OF Type;
  destruct: Destruct;

  TAG_END     : ADDRESS;
  TAG_ARR     : ADDRESS;
  TAG_REC     : ADDRESS;

  ADR_ALIGMENT: CARDINAL;   (* address value memory aligment *)

<* IF GC_IN_COROUTINE THEN *>
  CollectPrs  : COROUTINE;
<* END *>

<* IF multithread THEN *>
VAR
  destLock: rts.X2C_MUTEX;
  modLock : rts.X2C_MUTEX;
<* END *>

VAR
  dyn_offs : ARRAY [0..1] OF ADDRESS;
  prs_wsp  : ARRAY [0..17999+rts.X2C_HIS_LEN*12] OF CHAR;
(*prs_wsp  : ARRAY [0..7999+SIZE(rts.X2C_Coroutine_STR)] OF CHAR;*)
  (* usually 5000 bytes are enough, but it looks that
     GC does not work on HP9000 with the stack size
     less than 8000 bytes.
     The stack is also used for destructor calls!
  *)

PROCEDURE InsufficientMemory;
BEGIN
  <* IF STAT THEN *> stdio.printf("No memory exception...\n"); <* END *>
  X2C.X2C_TRAP(X2C.X2C_noMemoryException);
END InsufficientMemory;

---------------

(* not imported from xrMM for better performance
  (cause lack of inter-modular inlining in M2/O2) *)

CONST
  SZ_MASK = {3..13}; -- small/normal object size

(* for SMALL & NORMAL objects only!!! *)
PROCEDURE getHpObjSize (obj :rts.X2C_LINK) :CARDINAL;
BEGIN
  RETURN CARDINAL(BITSET(obj^.size)*SZ_MASK);
END getHpObjSize;

---------------

PROCEDURE stampObjAsFree (obj :rts.X2C_LINK; size :CARDINAL);
BEGIN
  (* clean all tags, set size & "_free" tag *)

  obj^.size := CARDINAL(obj^.tags*xrMM.BOFS_MASK+{xrMM._free}) + size;
END stampObjAsFree;


PROCEDURE getRootBySize (size :CARDINAL) :CARDINAL;
BEGIN
  RETURN (size-1) DIV LINK_SZ;
END getRootBySize;

<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>

--------------- Allocation profile

CONST
  MegaByte = 1024*1024;
  AllocProfile_MAX = 16384;

TYPE
  AllocProfile_Rec = RECORD
    info   :ADDRESS;
    sizeMB :CARDINAL;
    sizeB  :CARDINAL;
  END;

VAR
  AllocProfile_mlock    :xosThreads.X2C_Mutex;
  AllocProfile_enabled  :BOOLEAN;

  AllocProfile_types    :ARRAY [0..AllocProfile_MAX-1] OF AllocProfile_Rec;
  AllocProfile_NTypes   :INTEGER;
  AllocProfile_lastType :INTEGER;

  AllocProfile_sites    :ARRAY [0..AllocProfile_MAX-1] OF AllocProfile_Rec;
  AllocProfile_NSites   :INTEGER;
  AllocProfile_lastSite :INTEGER;

PROCEDURE AllocProfile_init ();
BEGIN
  xosThreads.X2C_CreateMutex(AllocProfile_mlock);
  AllocProfile_enabled := TRUE;
END AllocProfile_init;

PROCEDURE AllocProfile_rec (VAR data :ARRAY OF AllocProfile_Rec;
                            VAR last :INTEGER;
                            VAR num  :INTEGER;
                            info :ADDRESS; size :SIZE_T);
VAR i  :INTEGER;
    sz :CARDINAL;
BEGIN
  IF (data[last].info = info) THEN
    i := last;
  ELSE
    i := 0;
    LOOP
      IF (i = num) THEN
        ASSERT (i < AllocProfile_MAX, 919192);
        data[i].info   := info;
        data[i].sizeB  := 0;
        data[i].sizeMB := 0;
        INC (num);
        EXIT;
      END;
      IF (data[i].info = info) THEN
        EXIT;
      END;
      INC (i);
    END;
  END;

  sz := data[i].sizeB + size;
  INC (data[i].sizeMB, sz DIV MegaByte);
  data[i].sizeB := sz MOD MegaByte;

  last := i;
END AllocProfile_rec;

PROCEDURE AllocProfile_lock();
BEGIN
  xosThreads.X2C_EnterMutex (AllocProfile_mlock);
END AllocProfile_lock;

PROCEDURE AllocProfile_unlock();
BEGIN
  xosThreads.X2C_ExitMutex (AllocProfile_mlock);
END AllocProfile_unlock;

PROCEDURE AllocProfile_setEnabled(b :BOOLEAN);
BEGIN
  AllocProfile_enabled := b;
END AllocProfile_setEnabled;


PROCEDURE AllocProfile_allocated (type :Type;
                                  size :SIZE_T;
                                  callerIpRef :ADDR_REF);
BEGIN
  IF NOT AllocProfile_enabled THEN RETURN; END;

  AllocProfile_lock();

  AllocProfile_rec (AllocProfile_types,
                    AllocProfile_lastType,
                    AllocProfile_NTypes,
                    type, size);

  AllocProfile_rec (AllocProfile_sites,
                    AllocProfile_lastSite,
                    AllocProfile_NSites,
                    callerIpRef^, size);

  AllocProfile_unlock();
END AllocProfile_allocated;

PROCEDURE AllocProfile_print ();
VAR i :INTEGER;
BEGIN
  xosFmtIO.X2C_StdOutS ("---------------------------------- Sites:\n", 0);
  FOR i := 0 TO AllocProfile_NSites-1 DO
    IF (AllocProfile_sites[i].sizeMB # 0) THEN
      xosFmtIO.X2C_StdOutD (AllocProfile_sites[i].sizeMB, 15);
      xosFmtIO.X2C_StdOutS ("  ", 0);
      xosFmtIO.X2C_StdOutH (CARDINAL(AllocProfile_sites[i].info), 10);
      xosFmtIO.X2C_StdOutS ("\n", 0);
    END;
  END;
  xosFmtIO.X2C_StdOutS ("---------------------------------- Types:\n", 0);
  FOR i := 0 TO AllocProfile_NTypes-1 DO
    IF (AllocProfile_types[i].sizeMB # 0) THEN
      xosFmtIO.X2C_StdOutD (AllocProfile_types[i].sizeMB, 15);
      xosFmtIO.X2C_StdOutS ("  ", 0);
      xosFmtIO.X2C_StdOutS (Type(AllocProfile_types[i].info)^.name, 50);
      xosFmtIO.X2C_StdOutS ("\n", 0);
    END;
  END;
  xosFmtIO.X2C_StdOutS ("----------------------------------\n", 0);
END AllocProfile_print;

<* END *>

---------------


PROCEDURE ["C"] X2C_NEW(type: Type; VAR a: ADDRESS; size: SIZE_T; sys: BOOLEAN);
(* Oberon-2 standard allocate. *)
  VAR l: Link;
BEGIN
  IF size=0 THEN a:=NIL; RETURN END;
  ASSERT(size MOD type^.size = 0,100);
  xrMM.allocate(l,size, (type # rts.x2c_td_null));
  IF l=NIL THEN InsufficientMemory END;

<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>
  AllocProfile_allocated (type, size, SYSTEM.CALLERIPREF);
<* END *>

  l^.td:=type;
  a:=SYSTEM.ADDADR(l,LINK_SZ);
  <* IF NOT XMM_EXPLICIT THEN *>
    EXCL(l^.tags,xrMM._expbit); (* Hady, 28/11/96: Workaround to multithread problem *)
    IF sys THEN INCL(l^.tags, xrMM._sysbit) END;
  <* ELSE*>
     ASSERT(xrMM._expbit IN l^.tags);
  <* END *>

  <*IF ADB THEN*>
    db('NEW(%x): size=%d tags=%{} type=%x\n',a,size,l^.tags*xrMM.all_tags,l^.td);
  <*END*>
END X2C_NEW;


PROCEDURE ["C"] X2C_NEW_OPEN(
                   type : Type;
                   VAR a: ADDRESS;
                   size : SIZE_T;
                   lens-: ARRAY OF rts.X2C_LENS_TYPE;
                   dims : SIZE_T;
                   sys  : BOOLEAN
                   );
  VAR dtype: Type; desc : xrMM.Dynarr;
BEGIN
<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>
  AllocProfile_lock();
<* END *>

  a:=NIL;
  ASSERT(dims <= xrMM.MaxDim,101);
  dtype:=dynarrs[dims-1];

<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>
  AllocProfile_setEnabled(FALSE);
<* END *>

  X2C_NEW(dtype,desc,dtype^.size,sys);

<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>
  AllocProfile_setEnabled(TRUE);
  AllocProfile_allocated (dtype, dtype^.size, SYSTEM.CALLERIPREF);
<* END *>

  xrMM.X2C_InitDesc(desc,size,lens,dims);

<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>
  AllocProfile_setEnabled(FALSE);
<* END *>

  X2C_NEW(type,desc^.a,size,sys);

<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>
  AllocProfile_setEnabled(TRUE);
  AllocProfile_allocated (type, size, SYSTEM.CALLERIPREF);
<* END *>

  a:=desc;

<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>
  AllocProfile_unlock();
<* END *>
END X2C_NEW_OPEN;


PROCEDURE ["C"] X2C_DISPOSE(VAR a: SYSTEM.ADDRESS);
VAR
  l       :rts.X2C_LINK;
  i       :CARDINAL;
  desc    :xrMM.Dynarr;
  d, prev :Destruct;
BEGIN
  IF (a = NIL) THEN RETURN END;
  l := SYSTEM.SUBADR(a,LINK_SZ);
  IF (l^.td^.res#xrMM.MAGIC_T) THEN rts.X2C_ASSERT_F(100) END;
  i := 0;
  WHILE (i < xrMM.MaxDim) & (l^.td # dynarrs[i]) DO INC(i) END;

  <* IF XMM_EXPLICIT THEN *>
    IF (i < xrMM.MaxDim) THEN (* this is a dynarray *)
      desc := a;
      l:=SYSTEM.SUBADR(desc^.a,LINK_SZ);
      IF (l^.td^.res#xrMM.MAGIC_T) THEN rts.X2C_ASSERT_F(101) END;
      rts.X2C_DEALLOCATE(desc^.a);
    ELSIF (xrMM._unmovable IN l^.tags) THEN -- call a destructor
      ASSERT (destruct # NIL);
      d    := destruct;
      prev := NIL;
      WHILE (d^.adr # a) DO
        prev := d;
        d    := d^.next;
        ASSERT (d # NIL);  -- "a" object should have a destructor
      END;
      d^.proc (a);
      IF (prev = NIL) THEN
        destruct := d^.next;
      ELSE
        prev^.next := d^.next
      END;
    END;
    rts.X2C_DEALLOCATE (a);
  <* ELSE *>
    EXCL(l^.tags,xrMM._sysbit);
    IF i<xrMM.MaxDim THEN (* this is a dynarray *)
      desc:=SYSTEM.CAST(xrMM.Dynarr,a);
      l:=SYSTEM.SUBADR(desc^.a,LINK_SZ);
      IF (l^.td^.res#xrMM.MAGIC_T) THEN rts.X2C_ASSERT_F(101) END;
      EXCL(l^.tags,xrMM._sysbit);
    END;
  <* END *>
  l:=NIL;
  a:=NIL;
END X2C_DISPOSE;

PROCEDURE [2] X2C_DESTRUCTOR(a: ADDRESS; p: rts.X2C_DPROC);
  VAR d: Destruct; ln: Link;
BEGIN
  IF a=NIL THEN rts.X2C_ASSERT_F(102) END;
  IF p=NIL THEN rts.X2C_ASSERT_F(103) END;
  ln:=SYSTEM.SUBADR(a,LINK_SZ);
  <* IF XMM_EXPLICIT THEN *>
    IF (xrMM._sysbit IN ln^.tags) THEN rts.X2C_ASSERT_F(104) END;
  <* ELSE *>
    IF ln^.tags*{xrMM._expbit,xrMM._sysbit}#{} THEN rts.X2C_ASSERT_F(104) END;
  <* END *>
  IF ln^.td^.res#xrMM.MAGIC_T THEN rts.X2C_ASSERT_F(105) END;
  X2C_NEW (rts.x2c_td_null, d, SIZE (Destruct) + LINK_SZ, FALSE);
  d^.adr:=a;
  d^.proc:=p;
  <* IF multithread THEN *> rts.X2C_LockMutex(destLock); <* END *>
    d^.next:=destruct;
    destruct:=d;
  <* IF multithread THEN *> rts.X2C_FreeMutex(destLock); <* END *>
END X2C_DESTRUCTOR;

PROCEDURE FinalOne(d: Destruct);
BEGIN
  d^.proc(d^.adr);
EXCEPT
  RETURN
END FinalOne;

PROCEDURE FinalAll;
  VAR n: Destruct;
BEGIN
  WHILE destruct#NIL DO
    n:=destruct^.next;
    FinalOne(destruct);
    destruct:=n;
  END;
END FinalAll;

(*----------------------------------------------------------------*)
(*                      GARBAGE COLLECTOR                         *)
(*----------------------------------------------------------------*)

VAR marked,tail: Type;


PROCEDURE getObjectSize (l :Link) :CARDINAL;
VAR sz :CARDINAL;
    b  :Block;
BEGIN
  sz := getHpObjSize (l);
  IF (sz = 0) THEN
    b   := xrMM.getLargeObjBlock(l);
    ASSERT(b^.root = xrMM.LARGE);
    RETURN b^.size;
  ELSE
    RETURN sz;
  END;
END getObjectSize;


----------------------------- Anchor Tracing -----------------------------

CONST
  Kb = 1024;

VAR
  countingClosureInProgress :BOOLEAN;
  anchorWeight              :CARDINAL;
  totalWeightOfSmallAnchors :CARDINAL;
  printBuffer               :ARRAY [0..1023] OF CHAR;
  hasModuleAnchors          :BOOLEAN;
  hasDestructorAnchors      :BOOLEAN;
  hasStackAnchors           :BOOLEAN;

TYPE
  pSTR = POINTER TO ARRAY [0..1023] OF CHAR;

PROCEDURE AppendObject(ref: ADDR_REF); FORWARD;
PROCEDURE Closure; FORWARD;


PROCEDURE printAnchorTraceCaption();
BEGIN
  p.sprintf (printBuffer,
             "\n*********************** GC ANCHORS TRACE ***********************\n");
  xrtsOS.X2C_StdOutS (printBuffer, 0);

  p.sprintf (printBuffer, " %8d Kb <- EXPLICITLY ALLOCATED\n", xrMM.expusedmem DIV Kb);
  xrtsOS.X2C_StdOutS (printBuffer, 0);
END printAnchorTraceCaption;


PROCEDURE printModuleAnchorWeight (anchorType- :ARRAY OF CHAR;
                                   adr         :ADDRESS;
                                   moduleName  :rts.X2C_pCHAR);
BEGIN
  Closure();
  IF (anchorWeight >= xrMM.anchorWeightThreshold) THEN
    IF NOT hasModuleAnchors THEN
      hasModuleAnchors := TRUE;
      p.sprintf (printBuffer, "\n\n");
      xrtsOS.X2C_StdOutS (printBuffer, 0);
    END;

    p.sprintf (printBuffer, " %8d Kb <- %s %s <A:%x>\n", 
               anchorWeight DIV Kb, pSTR(moduleName)^, anchorType, adr);

    xrtsOS.X2C_StdOutS (printBuffer, 0);
  ELSE
    INC (totalWeightOfSmallAnchors, anchorWeight);
  END;
END printModuleAnchorWeight;


PROCEDURE printDestructorAnchorWeight (obj :ADDRESS);
VAR
  type :Type;
BEGIN
  Closure();
  IF (anchorWeight >= xrMM.anchorWeightThreshold) THEN
    IF NOT hasDestructorAnchors THEN
      hasDestructorAnchors := TRUE;
      p.sprintf (printBuffer, "\n\n");
      xrtsOS.X2C_StdOutS (printBuffer, 0);
    END;

    type := Link(SUBADR(obj, LINK_SZ))^.td;
    IF (type^.module # NIL) & (type^.module^.name # NIL) THEN
      p.sprintf (printBuffer, " %8d Kb <- DESTRUCTOR OF OBJ %xH (type %s.%s)\n", 
                 anchorWeight DIV Kb, obj, pSTR(type^.module^.name)^, pSTR(type^.name)^);
    ELSE
      p.sprintf (printBuffer, " %8d Kb <- DESTRUCTOR OF OBJ %xH (type %s)\n", 
                 anchorWeight DIV Kb, obj, pSTR(type^.name)^);
    END;

    xrtsOS.X2C_StdOutS (printBuffer, 0);
  ELSE
    INC (totalWeightOfSmallAnchors, anchorWeight);
  END;
END printDestructorAnchorWeight;


PROCEDURE printStackAnchorWeight (obj :ADDRESS);
VAR
  type :Type;
BEGIN
  Closure();
  IF (anchorWeight >= xrMM.anchorWeightThreshold) THEN
    IF NOT hasStackAnchors THEN
      hasStackAnchors := TRUE;
      p.sprintf (printBuffer, "\n\n");
      xrtsOS.X2C_StdOutS (printBuffer, 0);
    END;

    type := Link(SUBADR(obj, LINK_SZ))^.td;
    IF (type^.module # NIL) & (type^.module^.name # NIL) THEN
      p.sprintf (printBuffer, " %8d Kb <- STACK REF TO %xH (type %s.%s)\n", 
                 anchorWeight DIV Kb, obj, pSTR(type^.module^.name)^, pSTR(type^.name)^);
    ELSE
      p.sprintf (printBuffer, " %8d Kb <- STACK REF TO %xH (type %s)\n", 
                 anchorWeight DIV Kb, obj, pSTR(type^.name)^);
    END;

    xrtsOS.X2C_StdOutS (printBuffer, 0);
  ELSE
    INC (totalWeightOfSmallAnchors, anchorWeight);
  END;
END printStackAnchorWeight;


PROCEDURE printSmallAnchorsWeight ();
BEGIN
  IF (totalWeightOfSmallAnchors > 0) THEN
    p.sprintf (printBuffer, "\n %8d Kb <- Total size of small (< %d bytes) anchors\n",
               totalWeightOfSmallAnchors DIV Kb, xrMM.anchorWeightThreshold);

    xrtsOS.X2C_StdOutS (printBuffer, 0);
  END;
END printSmallAnchorsWeight;


PROCEDURE printLargeObjects ();
VAR
  b, fb :Block;
  l :Link;
  o :ADDRESS;
BEGIN
  countingClosureInProgress := TRUE;

  p.sprintf (printBuffer, "\nHANGING LARGE OBJECTS:\n");
  xrtsOS.X2C_StdOutS (printBuffer, 0);

  fb := xrMM.f_blocks [xrMM.LARGE];
  b  := fb^.next;
  WHILE (b # fb) DO
    l := xrMM.getFirstBlockAdr (b);
    o := SYSTEM.ADDADR (l, LINK_SZ);

    anchorWeight := 0;
    AppendObject (ADR(o));
    Closure();

    IF (l^.td^.module # NIL) &
       (l^.td^.module^.name # NIL)
    THEN
      p.sprintf (printBuffer, " %8d Kb <- LARGE OBJ %xH (type:%s.%s, size:%d)\n",
                 anchorWeight DIV Kb, o, pSTR(l^.td^.module^.name)^, pSTR(l^.td^.name)^, b^.size);
    ELSE
      p.sprintf (printBuffer, " %8d Kb <- LARGE OBJ %xH (type:%s, size:%d)\n",
                 anchorWeight DIV Kb, o, pSTR(l^.td^.name)^, b^.size);
    END;

    xrtsOS.X2C_StdOutS (printBuffer, 0);

    b := b^.next;
  END;

  countingClosureInProgress := FALSE;
END printLargeObjects;


----------------------------- Heap Tracing -----------------------------

TYPE
  HeapIterator_P = PROCEDURE ["Modula"] (Link);

PROCEDURE iterateHeap (callback :HeapIterator_P);

  CONST
     liveObjTags = {xrMM._sysbit, xrMM._expbit, xrMM._markbit};

  PROCEDURE iterateBlocks (VAR blocks: ARRAY OF Block);
  VAR i :CARDINAL; root,x :Block;
      l :Link; sz, size :CARDINAL;
  BEGIN
    root:=blocks[xrMM.NORMAL]; x:=root^.next;
    WHILE x#root DO
      l  := xrMM.getFirstBlockAdr(x);
      sz := 0;
      LOOP
        IF (liveObjTags * l^.tags # {}) THEN
          ASSERT (l^.td # NIL);
          callback(l);
        END;
        size := getHpObjSize (l);
        ASSERT (size # 0);

        l := ADDADR(l, size);
        INC (sz, size);
        IF sz >= xrMM.NormalBlockSize THEN EXIT; END;
      END;
      x:=x^.next;
    END;

    FOR i:=1 TO xrMM.Max DO
      root:=blocks[i]; x:=root^.next;
      size := i*LINK_SZ;
      WHILE x#root DO
        l := xrMM.getFirstBlockAdr(x);
        WHILE AdrLss(l, x^.mem) DO
          IF (liveObjTags * l^.tags # {}) THEN
            callback(l);
          END;
          l := ADDADR(l, size);
        END;
        x:=x^.next;
      END;
    END;

    root:=blocks[xrMM.LARGE]; x:=root^.next;
    WHILE x#root DO
      l := xrMM.getFirstBlockAdr(x);
      IF (liveObjTags * l^.tags # {}) THEN
        callback(l);
      END;
      x:=x^.next;
    END;
  END iterateBlocks;

BEGIN
  iterateBlocks (xrMM.f_blocks);
  iterateBlocks (xrMM.b_blocks);
END iterateHeap;



CONST
  HeapProfile_MAX = 16384;

TYPE
  HeapProfile_Rec = RECORD
    type :Type;
    size :CARDINAL;
  END;

VAR
  HeapProfile_types    :ARRAY [0..HeapProfile_MAX-1] OF HeapProfile_Rec;
  HeapProfile_NTypes   :INTEGER;
  HeapProfile_lastType :INTEGER;


PROCEDURE HeapProfile_reset();
BEGIN
  HeapProfile_NTypes := 0;
  HeapProfile_lastType := 0;

  HeapProfile_types[HeapProfile_lastType].type := NIL;
  HeapProfile_types[HeapProfile_lastType].size := 0;
END HeapProfile_reset;


PROCEDURE HeapProfile_recordObject (l :Link);
VAR
  type :Type;
  size :SIZE_T;
  i    :INTEGER;
  sz   :CARDINAL;
BEGIN
  type := l^.td;
  size := getObjectSize(l);

  IF (HeapProfile_types[HeapProfile_lastType].type = type) THEN
    i := HeapProfile_lastType;
  ELSE
    i := 0;
    LOOP
      IF (i = HeapProfile_NTypes) THEN
        ASSERT (i < HeapProfile_MAX, 919192);
        HeapProfile_types[i].type := type;
        HeapProfile_types[i].size := 0;
        INC (HeapProfile_NTypes);
        EXIT;
      END;

      IF (HeapProfile_types[i].type = type) THEN
        EXIT;
      END;

      INC (i);
    END;
  END;

  INC (HeapProfile_types[i].size, size);
  HeapProfile_lastType := i;
END HeapProfile_recordObject;


PROCEDURE HeapProfile_print ();
VAR
  i :INTEGER;
  otherSize :CARDINAL;
BEGIN
  p.sprintf (printBuffer, "\n\nHEAP TRACE:\n");
  xrtsOS.X2C_StdOutS (printBuffer, 0);

  otherSize := 0;

  FOR i := 0 TO HeapProfile_NTypes-1 DO
    IF (HeapProfile_types[i].size >= xrMM.heapTracingThreshold) THEN
      IF (HeapProfile_types[i].type^.module # NIL) &
         (HeapProfile_types[i].type^.module^.name # NIL)
      THEN
        p.sprintf (printBuffer, " %8d Kb - %s.%s\n", HeapProfile_types[i].size DIV Kb, pSTR(HeapProfile_types[i].type^.module^.name)^, pSTR(HeapProfile_types[i].type^.name)^);
      ELSE
        p.sprintf (printBuffer, " %8d Kb - %s\n", HeapProfile_types[i].size DIV Kb, pSTR(HeapProfile_types[i].type^.name)^);
      END;
      xrtsOS.X2C_StdOutS (printBuffer, 0);
    ELSE
      INC (otherSize, HeapProfile_types[i].size);
    END;
  END;

  IF (otherSize > 0) THEN
    p.sprintf (printBuffer, " %8d Kb - OTHER (< %d bytes)\n", otherSize DIV Kb, xrMM.heapTracingThreshold);
    xrtsOS.X2C_StdOutS (printBuffer, 0);
  END;
END HeapProfile_print;


PROCEDURE printHeapTrace();
BEGIN
  HeapProfile_reset();

  iterateHeap(HeapProfile_recordObject);

  HeapProfile_print();
END printHeapTrace;


------------------------------------------------------------------------

PROCEDURE AppendObject(ref: ADDR_REF);
(* ref - address of pointer *)
  VAR l: Link; t: Type; a: ADDRESS;
BEGIN
  ASSERT(ref # NIL,106);
  a:=ref^;
  IF a = NIL THEN RETURN END;
  ASSERT(AdrGtr(a,xrMM.blk_min),107);
  ASSERT(AdrLss(a,xrMM.blk_max),108);
  l:=SYSTEM.SUBADR(a,LINK_SZ);
  IF xrMM._moved IN l^.tags THEN
    l:=l^.next;
    a:=SYSTEM.ADDADR(l,LINK_SZ);
    ref^:=a;
  END;
  <*IF MDB THEN*> db('Append(%x) l.size=%x\n',a,l^.size); <*END*>

  IF l^.tags*{xrMM._markbit,xrMM._expbit} = {} THEN
    INCL(l^.tags,xrMM._markbit);

    IF countingClosureInProgress THEN
      INC (anchorWeight, getObjectSize (l));
    END;

    t:=l^.td;
    IF t^.res#xrMM.MAGIC_T THEN rts.X2C_ASSERT_F(109) END;
    IF (t # rts.x2c_td_null) THEN
      IF t^.link#NIL THEN
        t^.tail^.td:=Type(l); (* == t^.tail^.next:=l *)
      ELSE
        t^.link:=l;
        IF marked = NIL THEN marked:=t ELSE tail^.succ:=t END;
        tail:=t; t^.succ:=NIL;
      END;
      t^.tail:=l;
      l^.next:=NIL;
    END;
  END;
END AppendObject;

PROCEDURE ScanRecord(base: ADDRESS; desc: OFFS);
  VAR
    type: Type;
    last,cur,x: ADDRESS;
    i: CARDINAL;
BEGIN
  ASSERT(base#NIL,110);
  <*IF MDB THEN*> db('ScanRec(%x)\n',base); <*END*>
  x:=desc^[0]; i:=1;
  WHILE x # TAG_END DO
    <*IF MDB THEN*> db('rec: ofs=%x\n',x); <*END*>
    IF x = TAG_ARR THEN
      last:=SYSTEM.ADDADR(base,SYSTEM.DIFADRC(desc^[i],X2C.X2C_BASE)); INC(i);
      x:=desc^[i]; INC(i);
      ASSERT(x # TAG_ARR,111);
      IF x = TAG_REC THEN
        type:=desc^[i]; INC(i);
        IF (type^.res#xrMM.MAGIC_T) THEN rts.X2C_ASSERT_F(112) END;
        cur :=SYSTEM.ADDADR(base,SYSTEM.DIFADRC(desc^[i],X2C.X2C_BASE)); INC(i);
        LOOP
          ScanRecord(cur,OFFS(type^.offs));
          IF cur = last THEN EXIT END;
          cur := SYSTEM.ADDADR(cur,type^.size);
        END;
      ELSE
        cur :=SYSTEM.ADDADR(base,SYSTEM.DIFADRC(x,X2C.X2C_BASE));
        LOOP
          AppendObject(cur);
          IF cur = last THEN EXIT END;
          cur := SYSTEM.ADDADR(cur,SIZE(ADDRESS));
        END;
      END;
    ELSIF x = TAG_REC THEN
      type:=desc^[i]; INC(i);
      IF type^.res#xrMM.MAGIC_T THEN rts.X2C_ASSERT_F(113) END;
      ScanRecord(SYSTEM.ADDADR(base,SYSTEM.DIFADRC(desc^[i],X2C.X2C_BASE)),OFFS(type^.offs));
      INC(i);
    ELSE
      <*IF XMM_DEBUG THEN*> xrMM.cur_offs:=SYSTEM.DIFADRC(x,X2C.X2C_BASE); <*END*>
      AppendObject(SYSTEM.ADDADR(base,SYSTEM.DIFADRC(x,X2C.X2C_BASE)));
    END;
    x:=desc^[i]; INC(i)
  END;
END ScanRecord;


PROCEDURE MarkDestructors(): Destruct;
  VAR d,r,l,m: Destruct; ln, d_link: Link;
BEGIN
  IF xrMM.anchorTracing THEN
    countingClosureInProgress := TRUE;
    hasDestructorAnchors := FALSE;
  END;

  d:=destruct; l:=NIL; r:=NIL;
  WHILE d#NIL DO
    d_link := SYSTEM.SUBADR (d, LINK_SZ);
    INCL (d_link^.tags, xrMM._markbit);

    ln:=SYSTEM.SUBADR(d^.adr,LINK_SZ);
    IF ln^.tags*{xrMM._expbit,xrMM._sysbit}#{} THEN rts.X2C_ASSERT_F(119) END;

    IF xrMM._markbit IN ln^.tags THEN
      l:=d; d:=d^.next;
    ELSE
      m:=d; d:=d^.next;
      IF l=NIL THEN destruct:=d ELSE l^.next:=d END;
      m^.next:=r; r:=m;

      anchorWeight := 0;
      AppendObject(SYSTEM.ADR(m^.adr));
      IF xrMM.anchorTracing THEN
        printDestructorAnchorWeight (m^.adr);
      END;
    END;
  END;

  IF xrMM.anchorTracing THEN
    countingClosureInProgress := FALSE;
  END;
  RETURN r;
END MarkDestructors;


PROCEDURE AdjustRef (ref :ADDR_REF);
VAR l :Link;
BEGIN
  IF ref^ = NIL THEN RETURN END;
  l := SYSTEM.SUBADR (ref^, LINK_SZ);
  IF xrMM._moved IN l^.tags THEN
    ref^ := SYSTEM.ADDADR (l^.next, LINK_SZ);
  END;
END AdjustRef;


PROCEDURE AdjustRTStructures ();
VAR d :Destruct;
    l :Link;
BEGIN
  IF destruct # NIL THEN
    (* Adjust destructor list *)
    AdjustRef (ADR (destruct));
    d := destruct;
    WHILE d # NIL DO
      AdjustRef (ADR (d^.next));
      AdjustRef (ADR (d^.adr));
      d := d^.next;
    END;
  END;
END AdjustRTStructures;


PROCEDURE SortPtrs(VAR buf: ARRAY OF ADDRESS; l,r: INTEGER);
  VAR i,j: INTEGER; x,w: ADDRESS;
BEGIN
  i:=l; j:=r; x:=buf[(l+r) DIV 2];
  REPEAT
    WHILE AdrLss(buf[i],x) DO INC(i) END;
    WHILE AdrGtr(buf[j],x) DO DEC(j) END;
    IF i<=j THEN
      w:=buf[i]; buf[i]:=buf[j]; buf[j]:=w;
      INC(i); DEC(j);
    END;
  UNTIL i>j;
  IF l<j THEN SortPtrs(buf,l,j) END;
  IF i<r THEN SortPtrs(buf,i,r) END;
END SortPtrs;

PROCEDURE SortBlocks(blocks-: ARRAY OF Block;
                VAR head: Block; VAR min,max: ADDRESS);

  PROCEDURE Sort(VAR buf: ARRAY OF ADDRESS; no: CARDINAL);
    VAR l,h,b: Block; i: CARDINAL; be: ADDRESS;
  BEGIN
    SortPtrs(buf,0,no-1);
    h:=NIL; l:=head;
    FOR i:=0 TO no-1 DO
      b:=buf[i];
      WHILE (l#NIL) & AdrLss(l,b) DO h:=l; l:=l^.snxt END;
      IF h=NIL THEN head:=b ELSE h^.snxt:=b END;
      h:=b; b^.snxt:=l;
    END;
    l  := buf[no-1];
    be := xrMM.getLastBlockAdr1(l)-1;
    IF (min=NIL) OR AdrLss(buf[0],min) THEN min:=buf[0] END;
    IF (max=NIL) OR AdrGtr(be,max) THEN max:=be END;
  END Sort;

  VAR e,b: Block; i,j: CARDINAL; buf: ARRAY [0..255] OF ADDRESS;
BEGIN
  j:=0;
  FOR i:=0 TO HIGH(blocks) DO
    b:=blocks[i]^.next; (* skip dummy block *)
    e:=blocks[i];
    WHILE b#e DO
      IF j>HIGH(buf) THEN Sort(buf,j); j:=0 END;
      buf[j]:=b; INC(j); b:=b^.next;
    END;
  END;
  IF j>0 THEN Sort(buf,j); j:=0 END;
END SortBlocks;

PROCEDURE CheckPtrs(head: Block; VAR buf: ARRAY OF ADDRESS; no: CARDINAL);
(*
        "head" - sorted by address list of all blocks (.snext)
        "buf"  - array of potential pointers obtained from the stack
*)
VAR
  obj   :ADDRESS;
  end   :ADDRESS;  -- end address of block's memory+1
  i,sz  :CARDINAL;
  l,n   :Link;
BEGIN
  ASSERT(no > 0,120);
  SortPtrs(buf,0,no-1);
  i:=0;
  IF (head^.magic#xrMM.MAGIC_B) THEN rts.X2C_ASSERT_F(121) END;
  IF head^.root IN {xrMM.NORMAL,xrMM.LARGE} THEN
    end := xrMM.getLastBlockAdr1(head);
  ELSE
    end := head^.mem;
  END;
  LOOP
    ASSERT((i=no-1) OR NOT AdrGtr(buf[i],buf[i+1]),122);
    IF AdrLss(buf[i], xrMM.getFirstBlockAdr(head)+LINK_SZ) THEN
      (* buf[i] is not a pointer *)
      INC(i);
      IF i=no THEN EXIT END;
    ELSIF NOT AdrLss(buf[i],end) THEN -- there are no pointers in the blocks
      head:=head^.snxt;
      IF head=NIL THEN EXIT END;
      IF (head^.magic#xrMM.MAGIC_B) THEN rts.X2C_ASSERT_F(123) END;
      IF head^.root IN {xrMM.NORMAL,xrMM.LARGE} THEN
        end := xrMM.getLastBlockAdr1(head);
      ELSE
        end := head^.mem;
      END;
    ELSIF head^.root=xrMM.LARGE THEN
      l := xrMM.getFirstBlockAdr(head);
      obj := SYSTEM.ADDADR (l, LINK_SZ);
      IF NOT (xrMM._notstack IN l^.tags) THEN
        anchorWeight := 0;
        AppendObject(SYSTEM.ADR(obj));
        INCL(l^.tags,xrMM._stack);
        IF xrMM.anchorTracing THEN
          printStackAnchorWeight(obj);
        END;
      END;
      INC(i);
      IF i=no THEN EXIT END;
    ELSE
      l := xrMM.getFirstBlockAdr(head);  -- gets first object of the block

      IF (head^.root # xrMM.NORMAL) THEN -- SMALL block
        sz := head^.root*LINK_SZ;
        LOOP
          LOOP
            ASSERT(AdrLss(l, end),124);
            ASSERT((xrMM._free IN  l^.tags) OR
                   (l^.tags*{xrMM._markbit,xrMM._moved}#{}) OR
                   (l^.td^.res = xrMM.MAGIC_T),125);
            n := ADDADR(l, sz);
            IF AdrLss(buf[i], n) THEN EXIT END;
            l := n;
          END;
          obj := ADDADR(l, LINK_SZ);
          -- Note: moved objects can be "referenced" from the stack due to
          -- scanning of stack/registers of GC thread (which is not frozen)
          IF ({xrMM._free, xrMM._moved} * l^.tags = {}) & (NOT AdrLss(buf[i],obj)) THEN
            anchorWeight := 0;
            AppendObject(SYSTEM.ADR(obj));
            INCL(l^.tags, xrMM._stack);
            IF xrMM.anchorTracing THEN
              printStackAnchorWeight(obj);
            END;
          END;
          IF (i+1<no) & AdrLss(buf[i+1],end) THEN INC(i) ELSE EXIT END;
        END;
      ELSE
        LOOP
          LOOP
            ASSERT(AdrLss(l,end),124);
            sz := getHpObjSize(l);
            ASSERT((xrMM._free IN l^.tags) OR
                   (l^.tags*{xrMM._markbit,xrMM._moved}#{}) OR
                   (l^.td^.res = xrMM.MAGIC_T),125);
            (* sz includes LINK_SZ! *)
            n:=SYSTEM.ADDADR(l,sz);
            IF AdrLss(buf[i],n) THEN EXIT END;
            l:=n;
          END;
          obj := ADDADR(l, LINK_SZ);
          -- Note: moved objects can be "referenced" from the stack due to
          -- scanning of stack/registers of GC thread (which is not frozen)
          IF ({xrMM._free, xrMM._moved} * l^.tags = {}) & (NOT AdrLss(buf[i],obj)) THEN
            anchorWeight := 0;
            AppendObject(SYSTEM.ADR(obj));
            INCL(l^.tags,xrMM._stack);
            IF xrMM.anchorTracing THEN
              printStackAnchorWeight(obj);
            END;
          END;
          IF (i+1<no) & AdrLss(buf[i+1], end) THEN INC(i) ELSE EXIT END;
        END;
      END;
      INC(i);
      IF (i = no) THEN EXIT END;
    END;
  END;
END CheckPtrs;

PROCEDURE ScanStack(head: Block; min,max: ADDRESS; fr,to: ADDR_REF);
(*
        "to" points to the first byte of free place on the stack!
        "head" - sorted by address list of all blocks (.snext)
*)
  VAR buf: ARRAY [0..255] OF ADDRESS; r: ADDR_REF; i: CARDINAL;
BEGIN
  IF SIZE(ADDRESS)>ADR_ALIGMENT THEN
    to:=SYSTEM.SUBADR(to,SIZE(ADDRESS)-ADR_ALIGMENT);
  ELSE
    to:=SYSTEM.SUBADR(to,ADR_ALIGMENT-SIZE(ADDRESS));
  END;
  WHILE NOT AdrGtr(fr,to) DO
    i:=0;
    LOOP
      r:=fr;
      fr:=SYSTEM.ADDADR(fr,ADR_ALIGMENT);
      IF NOT AdrLss(r^,min) &
         NOT AdrGtr(r^,max) THEN
        buf[i]:=r^; INC(i);
        IF i>HIGH(buf) THEN EXIT END;
      END;
      IF AdrGtr(fr,to) THEN EXIT END;
    END;
    IF i>0 THEN CheckPtrs(head,buf,i) END;
  END;
END ScanStack;

<* IF XMM_DEBUG THEN *>
  PROCEDURE CheckBlockList(head: Block; min,max: ADDRESS);
  VAR
    i, no, sz :CARDINAL;
    f, l, n   :rts.X2C_LINK;
    e         :ADDRESS;
    p         :POINTER TO ARRAY [0..xrMM.GAP_LEN-1] OF CARDINAL;

  BEGIN
    no := 0;
    WHILE (head # NIL) DO
      INC(no);
      ASSERT((head^.snxt=NIL) OR AdrLss(head,head^.snxt),128);
      ASSERT(NOT AdrGtr(min,head),129);
      ASSERT (NOT AdrLss(max, xrMM.getLastBlockAdr1(head)-1), 130);
      ASSERT(head^.magic = xrMM.MAGIC_B,127);

      p := SYSTEM.SUBADR(head,GAP_SIZE);
      FOR i:=0 TO xrMM.GAP_LEN-1 DO
        ASSERT(p^[i]=xrMM.MAGIC_G,131);
      END;
      p := xrMM.getLastBlockAdr1(head);
      FOR i:=0 TO xrMM.GAP_LEN-1 DO
        ASSERT(p^[i]=xrMM.MAGIC_G,132);
      END;

      IF (head^.root = xrMM.NORMAL) THEN
        f := head^.list;             (* list of free objects *)
        l := xrMM.getFirstBlockAdr (head);
        e := l + head^.size;
        ASSERT(AdrLss(l,e),133);

        WHILE AdrLss(l,e) DO
          ASSERT((f=NIL) OR (f^.next=NIL) OR AdrLss(f,f^.next),134);
          sz := getHpObjSize(l);
          ASSERT((l=f) OR
                 (l^.tags*{xrMM._markbit,xrMM._moved}#{}) OR
                 (l^.td^.res = xrMM.MAGIC_T),136);
          (* sz includes LINK_SZ! *)
          IF (l = f) THEN f:=f^.next END;
          l := ADDADR(l, sz);
        END;
      ELSIF (head^.root # xrMM.LARGE) THEN  (* block of small objects *)

        f := head^.list;         (* list of free parts *)
        WHILE (f # NIL) DO
          ASSERT(xrMM._free IN  f^.tags);
          f := f^.next;
        END;

        l := xrMM.getFirstBlockAdr(head);
        ASSERT (NOT AdrGtr(l, head^.mem));

        WHILE AdrLss(l, head^.mem) DO
          sz := getHpObjSize(l);
          ASSERT(sz = LINK_SZ*head^.root, 135);
          ASSERT((xrMM._free IN  l^.tags) OR
                 (l^.tags*{xrMM._markbit,xrMM._moved}#{}) OR
                 (l^.td^.res = xrMM.MAGIC_T),136);
          (* sz includes LINK_SZ! *)
          l := ADDADR(l, sz);
        END;
      END;

      head := head^.snxt;
    END;
    ASSERT(no = xrMM.blockno,137);
  END CheckBlockList;
<* END *>

PROCEDURE MarkStack;
  VAR
    min,max: ADDRESS;
    fr,to,a: ADDR_REF;
    head: Block;
    c,e: rts.X2C_Coroutine;
BEGIN
  IF xrMM.anchorTracing THEN
    countingClosureInProgress := TRUE;
    hasStackAnchors := FALSE;
  END;

  head:=NIL; min:=NIL; max:=NIL;
  SortBlocks(xrMM.f_blocks,head,min,max);
  SortBlocks(xrMM.b_blocks,head,min,max);
  <*IF XMM_DEBUG THEN*> CheckBlockList(head,min,max); <*END*>
  IF head#NIL THEN
    e:=rts.X2C_GetCurrent(); c:=e^.fwd;
    <* IF GC_IN_COROUTINE THEN *>
    ASSERT(c#e, 137);
    <* END *>
    LOOP
      <* IF GC_IN_COROUTINE THEN *>
        (* Do not scan GC' coroutine *)
        IF c = e THEN
          EXIT
        END;
      <* END *>
      IF c = xrMM.GCInvoker THEN
        fr:=c^.stk_start;
        to:=xrMM.StackEnd4GC;
      ELSE
        fr:=c^.stk_start;
        to:=c^.stk_end;
      END;
      IF to=NIL THEN to:=fr END; (* workaround: coroutine is not started but registered *)
      IF AdrLss(to,fr) THEN a:=to; to:=fr; fr:=a END;
      ScanStack(head,min,max,fr,to);
      IF (c^.reg_dsize > SIZE(SYSTEM.ADDRESS)) & (c # xrMM.GCInvoker) THEN
        fr:=SYSTEM.ADR(c^.reg_dump);
        to:=SYSTEM.ADDADR(fr,c^.reg_dsize);
        ScanStack(head,min,max,fr,to);
      END;
      <* IF NOT GC_IN_COROUTINE THEN *>
        IF c = e THEN
          EXIT
        END;
      <* END *>
      c:=c^.fwd;
    END;
  END;

  IF xrMM.anchorTracing THEN
    countingClosureInProgress := FALSE;
  END;
END MarkStack;

PROCEDURE MarkObject(x: Link; type: Type);
  VAR n,len: CARDINAL; a: ADDRESS; b: Block;
BEGIN
  ASSERT(type^.res = xrMM.MAGIC_T, 138);

  IF (x^.tags*xrMM.BOFS_MASK = {}) THEN -- LARGE block
    b   := xrMM.getLargeObjBlock(x);
    ASSERT(b^.root = xrMM.LARGE);
    len := b^.fsum;
  ELSE
    len := getHpObjSize(x);
  END;

  len := (len-LINK_SZ) DIV type^.size;
  a := SYSTEM.ADDADR(x, LINK_SZ);
  n := 0;
  WHILE (n < len) DO
    <*IF MDB THEN*> db('scan: %x l=%x size=%x type=%x\n',a,x,x^.size,type); <*END*>
    <*IF XMM_DEBUG THEN*> xrMM.cur_td:=type; <*END*>
    ScanRecord (a, OFFS(type^.offs));
    INC(n);
    a := a + type^.size;
  END;
END MarkObject;


PROCEDURE Closure;
  VAR l,x: Link;
BEGIN
  WHILE marked#NIL DO
    l:=Link(marked^.link);
    WHILE l # NIL DO
      MarkObject(l,marked);
      x:=l;
      l:=l^.next;    (* must be done after MarkObject *)
      x^.td:=marked; (* must be done after l:=l^.next *)
    END;
    marked^.link:=NIL;
    marked:=marked^.succ;
  END;
END Closure;


PROCEDURE MarkModules;
VAR
  l            :Module;
  desc         :OFFS;
  type         :Type;
  x, last, cur :ADDRESS;
  i            :CARDINAL;
  anchorBase   :ADDRESS;
BEGIN
  IF xrMM.anchorTracing THEN
    printAnchorTraceCaption();
    countingClosureInProgress := TRUE;
  END;

  l := rts.X2C_MODULES;
  WHILE (l # NIL) DO
    desc:=OFFS (l^.offs);
    x:=desc^[0]; i:=1;
    hasModuleAnchors := FALSE;

    WHILE (x # TAG_END)  DO
      IF x = TAG_REC THEN
        type:=desc^[i]; INC(i);
        IF type^.res#xrMM.MAGIC_T THEN rts.X2C_ASSERT_F(114) END;

        anchorBase := desc^[i];
        anchorWeight := 0;
        ScanRecord (anchorBase, OFFS(type^.offs)); INC(i);

        IF xrMM.anchorTracing THEN
          printModuleAnchorWeight ("RECORD ", anchorBase, l^.name);
        END;
      ELSIF x = TAG_ARR THEN
        last:=desc^[i]; INC(i);
        x:=desc^[i]; INC(i);
        ASSERT(x # TAG_ARR,115);
        IF x = TAG_REC THEN
          type:=desc^[i]; INC(i);
          IF (type^.res#xrMM.MAGIC_T) THEN rts.X2C_ASSERT_F(116) END;
          cur:=desc^[i]; INC(i);
          anchorBase := cur; 
          anchorWeight := 0;
          LOOP
            <*IF XMM_DEBUG THEN*> xrMM.cur_td:=type; <*END*>
            ScanRecord(cur,OFFS(type^.offs));
            IF cur = last THEN EXIT END;
            cur := SYSTEM.ADDADR(cur,type^.size);
          END;
          IF xrMM.anchorTracing THEN
            printModuleAnchorWeight ("ARRAY OF RECORDs", anchorBase, l^.name);
          END;
        ELSE
          cur := x;
          anchorWeight := 0;
          LOOP
            ASSERT(cur#NIL,117);
            AppendObject(cur);
            IF cur = last THEN EXIT END;
            cur := SYSTEM.ADDADR(cur,SIZE(ADDRESS));
          END;

          IF xrMM.anchorTracing THEN
            printModuleAnchorWeight ("ARRAY OF POINTERs", x, l^.name);
          END;
        END;
      ELSE (* just a pointer *)

        ASSERT(x#NIL,118);
        anchorWeight := 0;
        AppendObject(x);

        IF xrMM.anchorTracing THEN
          printModuleAnchorWeight ("POINTER", x, l^.name);
        END;
      END;
      x := desc^[i];
      INC(i);
    END;
    l:=l^.next;
  END;

  IF xrMM.anchorTracing THEN
    countingClosureInProgress := FALSE;
  END;
END MarkModules;


PROCEDURE normal_scan(x: Block; defrag :BOOLEAN);
TYPE
  scanerStates = (ss_noneFO,     -- none free object encountered
                  ss_savedFO,    -- there was a free object encountered
                  ss_accumFO);   -- accumulating adjoning free objects
VAR
  state          :scanerStates;
  savedFO        :Link;
  savedFO_inPool :BOOLEAN;
  mergedObjSize  :CARDINAL;

  l              :Link;          -- current in scanning
  l_inPool       :BOOLEAN;

  size  :CARDINAL;  -- of scanned part
  sz    :CARDINAL;
BEGIN
  <*IF SDB THEN*> db('normal_scan(%0x)\n',x); <*END*>

  size  :=0;
  state := ss_noneFO;

  l := xrMM.getFirstBlockAdr(x);
  x^.fixed := FALSE;
  x^.fsum  := xrMM.NormalBlockSize;
  LOOP
    ASSERT (l^.tags*xrMM.BOFS_MASK # {}, 1381);
    sz:=getHpObjSize(l);
    l_inPool := (xrMM._free IN l^.tags);

    IF NOT l_inPool THEN
      <*IF SDB THEN*> db('%05x: l.size=%x tags=%{} type=%x\n',l,l^.size,l^.tags*xrMM.all_tags,l^.td); <*END*>

      ASSERT((xrMM._moved IN l^.tags) OR (l^.td^.res = xrMM.MAGIC_T),140);

      IF l^.tags*{xrMM._sysbit,xrMM._expbit,xrMM._markbit} = {} THEN (* free the piece *)
        ASSERT(NOT (xrMM._stack IN l^.tags),141);

        <*IF XMM_DEBUG THEN*> xrMM.SetNil(SYSTEM.ADDADR(l,LINK_SZ),sz-LINK_SZ); <*END*>
        DEC(rts.X2C_usedmem, sz);
        DEC(rts.X2C_objects);

        stampObjAsFree (l, sz);
      ELSE
        DEC (x^.fsum, sz);
        (* wash out marking *)
        IF (defrag) THEN
          IF l^.tags*{xrMM._stack,xrMM._sysbit,xrMM._expbit,xrMM._unmovable}#{} THEN
            x^.fixed:=TRUE
          END;
          EXCL(l^.tags, xrMM._markbit);
        ELSE
          l^.tags:=l^.tags-{xrMM._markbit,xrMM._stack};
        END;
        INC (rts.X2C_normalused, sz);
      END;
    END;

--    IF (xrMM._free IN l^.tags) & NOT (l_inPool) THEN xrMM.foManager_add(l) END; -- !!!!!!!!!!TEMP

    IF (xrMM._free IN l^.tags) THEN   -- was free or just made free
      CASE (state) OF
        |ss_noneFO  : savedFO        := l;
                      savedFO_inPool := l_inPool;
                      state          := ss_savedFO;

        |ss_savedFO, ss_accumFO:

                      IF (state = ss_savedFO) THEN (* it's 1st merge *)
                        IF (savedFO_inPool) THEN
                          xrMM.foManager_del(savedFO);   -- remove from pool
                        END;
                      END;

                      IF (l_inPool) THEN
                        xrMM.foManager_del(l);   -- remove from pool
                      END;

                      mergedObjSize := sz + getHpObjSize(savedFO);

                      IF (mergedObjSize = xrMM.NormalBlockSize) THEN
                        xrMM.free_block(x);
                        RETURN;
                      END;

                      stampObjAsFree (savedFO, mergedObjSize);
                      state := ss_accumFO;
      END;
    ELSE
      IF (state = ss_accumFO) OR
         ( (state = ss_savedFO) & NOT savedFO_inPool) THEN
         xrMM.foManager_add(savedFO);
      END;
      state := ss_noneFO;
    END;

    INC(size,sz);
    IF (size >= xrMM.NormalBlockSize) THEN      -- end of the block
      ASSERT(size = xrMM.NormalBlockSize, 142);

      IF (state = ss_accumFO) OR
         ( (state = ss_savedFO) & NOT savedFO_inPool) THEN
         xrMM.foManager_add(savedFO);
      END;
      INC (rts.X2C_normalbusy, xrMM.BlockSize);

      EXIT;
    END;
    l:=SYSTEM.ADDADR(l,sz);
  END;
END normal_scan;

VAR
   smallBlocksTotal       :CARDINAL;
   smallBlocksStackFixed  :CARDINAL;
   smallBlocksSysbitFixed :CARDINAL;
   smallBlocksExpbitFixed :CARDINAL;

(* Inlined from xrMM FOR DEBUGGING ASSERT 144 ONLY *)

CONST
  BOFS_BIT = 21-3;

PROCEDURE getObjBlock (obj :Link) :Block;
VAR
  ofs :CARDINAL;
BEGIN
  ofs := CARDINAL( SYSTEM.SHIFT(BITSET(obj^.size)*xrMM.BOFS_MASK, -BOFS_BIT) );
  RETURN Block(obj - ofs);
END getObjBlock;
(* ----------------------------- *)

PROCEDURE small_scan (x :Block; root :CARDINAL; busy, defrag :BOOLEAN);
VAR
  l    :Link;
  size :CARDINAL;
BEGIN
  (* NOTE: free list of a SMALL block is unordered *)
  size := root*LINK_SZ;
  <*IF SDB THEN*> db('small_scan(%x,%d)\n',x,size); <*END*>
  l := xrMM.getFirstBlockAdr(x);
  x^.fixed := FALSE;
  x^.fsum  := xrMM.SmallBlockSize;
  WHILE AdrLss(l, x^.mem) DO
    ASSERT (getHpObjSize(l) = size, 1579);
    IF ~(xrMM._free IN  l^.tags) THEN
      IF l^.tags*{xrMM._sysbit,xrMM._expbit,xrMM._markbit} = {} THEN
        (* free piece *)
        ASSERT(NOT (xrMM._stack IN l^.tags),143);

        <* IF ASSERT THEN *>
        IF (NOT (xrMM._moved IN l^.tags)) & (l^.td^.res # xrMM.MAGIC_T) THEN
          p.printf ("\nASSERT 144: l = %X\n", l);
          p.printf ("\nASSERT 144: l^.tags = %X  l^.td = %X\n", l^.tags, l^.td);
          p.printf ("\nASSERT 144: b(l)=%X  x=%X  x^.magic=%X x^.root=%X  x^.fixed=%X  x^.mem=%X x^.fsum=%X\n", getObjBlock (l), x, x^.magic, x^.root, x^.fixed, x^.mem, x^.fsum);
          p.printf ("\nASSERT 144: l^.next^.tags = %X  l^.next^.td = %X l^.next^.td^.magic = %X\n", l^.next^.tags, l^.next^.td, l^.next^.td^.res);
          p.printf ("\nASSERT 144: b(l^.next)=%X  root=%X  fixed=%X  mem=%X fsum=%X\n", getObjBlock (l^.next), getObjBlock (l^.next)^.root, getObjBlock (l^.next)^.fixed, getObjBlock (l^.next)^.mem, getObjBlock (l^.next)^.fsum);
        END;
        <* END *>
        ASSERT((xrMM._moved IN l^.tags) OR (l^.td^.res = xrMM.MAGIC_T),144);

        <*IF XMM_DEBUG THEN*> xrMM.SetNil(SYSTEM.ADDADR(l,LINK_SZ),size-LINK_SZ); <*END*>
        DEC(rts.X2C_usedmem,getHpObjSize(l));
        DEC(rts.X2C_objects);
        l^.next := x^.list;
        x^.list := l;
        stampObjAsFree(l, size);
      ELSE
        (* wash out marking *)
        ASSERT(NOT (xrMM._moved IN l^.tags),145);
        ASSERT(l^.td^.res = xrMM.MAGIC_T, 146);
        IF (defrag) THEN
          IF l^.tags*{xrMM._stack,xrMM._sysbit,xrMM._expbit,xrMM._unmovable}#{} THEN
            x^.fixed:=TRUE
          END;
          EXCL(l^.tags, xrMM._markbit);
        ELSE
          l^.tags:=l^.tags-{xrMM._markbit,xrMM._stack};
        END;
        DEC (x^.fsum, size);
        INC (rts.X2C_smallused, size);
      END;
    END;
    l := ADDADR(l, size);
  END;

  ASSERT (x^.fsum <= xrMM.SmallBlockSize);
  IF (xrMM.SmallBlockSize - x^.fsum) < size THEN xrMM.free_block(x); RETURN END;

  IF busy & (x^.list # NIL) THEN xrMM.MakeFree(x) END;
  INC (rts.X2C_smallbusy, xrMM.SmallBlockSize);
  INC (smallBlocksTotal);
END small_scan;

PROCEDURE large_scan(x: Block);
  VAR l: Link;
BEGIN
  <*IF SDB THEN*> db('large_scan(%x)\n',x); <*END*>
  l := xrMM.getFirstBlockAdr(x);
  <*IF SDB THEN*> db('l^.size = %x size= %d tags=%{}\n',l^.size,x^.fsum,l^.tags*xrMM.all_tags); <*END*>
  ASSERT(NOT (xrMM._moved IN l^.tags),147);
  IF (l^.td^.res#xrMM.MAGIC_T) THEN rts.X2C_ASSERT_F(148) END;
  IF l^.tags*{xrMM._sysbit,xrMM._expbit,xrMM._markbit} = {} THEN
    ASSERT(NOT (xrMM._stack IN l^.tags),149);
    <*IF XMM_DEBUG THEN*> xrMM.SetNil(SYSTEM.ADDADR(l,LINK_SZ),x^.size); <*END*>
    DEC(rts.X2C_usedmem,x^.fsum);
    DEC(rts.X2C_objects);
    xrMM.free_block(x);
  ELSE
    l^.tags:=l^.tags-{xrMM._markbit,xrMM._stack};
    INC (rts.X2C_largebusy, x^.fsum);
  END;
END large_scan;

PROCEDURE Sweep(VAR blocks: ARRAY OF Block; busy,defrag: BOOLEAN);
  VAR i: CARDINAL; root,x,n: Block;
BEGIN
  root:=blocks[xrMM.NORMAL]; x:=root^.next;
  WHILE x#root DO n:=x; x:=x^.next; normal_scan(n, defrag) END;

  FOR i:=1 TO xrMM.Max DO
    root:=blocks[i]; x:=root^.next;
    WHILE x#root DO n:=x; x:=x^.next; small_scan(n,i,busy,defrag) END;
  END;

  root:=blocks[xrMM.LARGE]; x:=root^.next;
  WHILE x#root DO n:=x; x:=x^.next; large_scan(n) END;
END Sweep;

PROCEDURE DefragSort(b: Block);
  PROCEDURE lss(x,y: Block): BOOLEAN;
  BEGIN
    IF y^.fixed THEN RETURN FALSE END;
    RETURN x^.fsum<=y^.fsum;
  END lss;
  VAR s,i,j: Block;
BEGIN
  s:=NIL;
  WHILE b^.next#b DO
    i:=b^.next; b^.next:=i^.next;
    IF i^.fixed OR (s=NIL) OR lss(i,s) THEN
      i^.next:=s; s:=i;
    ELSE
      j:=s;
      WHILE (j^.next#NIL) & NOT lss(i,j^.next) DO j:=j^.next END;
      i^.next:=j^.next; j^.next:=i;
    END;
  END;
  b^.next:=s; j:=b;
  WHILE s#NIL DO s^.prev:=j; j:=s; s:=s^.next END;
  b^.prev:=j; j^.next:=b;
END DefragSort;

VAR SmallBlocksReleased  :CARDINAL;
    SmallBlocksCompleted :CARDINAL;

PROCEDURE Defragment(b: Block);
  VAR first,last: Block; n,l: Link; size: CARDINAL;
BEGIN
  ASSERT (b^.root IN {1..xrMM.Max});
  size  := b^.root*LINK_SZ;
  first := b^.next;
  last  := b^.prev;
  INC (SmallBlocksCompleted);
  WHILE (first # last) & NOT last^.fixed DO
    ASSERT(first^.magic = xrMM.MAGIC_B, 150);
    ASSERT(last^.magic  = xrMM.MAGIC_B, 151);
    l := xrMM.getFirstBlockAdr(last);

    WHILE AdrLss(l, last^.mem) DO
      IF l^.tags*{xrMM._free} = {} THEN
        ASSERT (l^.tags*{xrMM._sysbit, xrMM._expbit, xrMM._stack, xrMM._moved} = {}, 155);
        ASSERT (l^.td^.res = xrMM.MAGIC_T, 152);
        --ASSERT(first^.list # NIL,153);

        ASSERT (first^.fsum >= size);
        IF (first^.list = NIL) THEN  -- take an object from free raw memory pool
          n           := first^.mem;
          first^.mem  := n + size;
        ELSE                         -- take an object from free list
          n           := first^.list;
          first^.list := n^.next;
        END;
        DEC(first^.fsum, size);

        INC(rts.X2C_usedmem,getHpObjSize(l));
        INC(rts.X2C_objects);
        <* IF STAT THEN *> INC(moved_objects); <* END *>
        SYSTEM.MOVE(l,n,size);
        INCL (l^.tags,xrMM._moved);
        EXCL (l^.tags,xrMM._markbit);
        l^.next := n;              -- save address of copy
        INC (last^.fsum, size);

        IF (first^.fsum < size) THEN
          ASSERT (first^.list = NIL);
          xrMM.MakeBusy(first);
          first:=b^.next; 
          INC (SmallBlocksCompleted);
          (* gets next as MakeBusy removes a block from the list *)
        END;
        IF (first = last) THEN RETURN END;
        ASSERT(first^.magic = xrMM.MAGIC_B, 154);
      END;
      l := SYSTEM.ADDADR(l, size);
    END;

    ASSERT (last^.fsum <= xrMM.SmallBlockSize);
    ASSERT (xrMM.SmallBlockSize - last^.fsum < size, 190); -- this block will be sweeped
    INC (SmallBlocksReleased);
    last:=last^.prev;
  END;
END Defragment;


--------------------- Normal Blocks Defragmenter 

PROCEDURE MoveObject (from, to :Link; fromB, toB :Block; size :CARDINAL);
BEGIN
  ASSERT (fromB^.fsum > size);

  SYSTEM.MOVE (from, to, size);
  xrMM.setObjOfsLen (to, size, SYSTEM.DIFADRC(to, toB));

  from^.next := to;              -- save address of copy
  INCL (from^.tags,xrMM._moved);
  EXCL (from^.tags,xrMM._markbit);

  INC (fromB^.fsum, size);
  DEC (toB^.fsum, size);

  INC (rts.X2C_usedmem, size);
END MoveObject;



VAR
  normalBlocksRest      :CARDINAL;
  normalBlocksReleased  :CARDINAL;
  normalBlocksMoved     :CARDINAL;


PROCEDURE DefragmentNormalBlocks(blockList :Block);
CONST
  MOVING_RATE  = 50; --
VAR
  b, newb :Block;
  ib      :Block;
  freemem :CARDINAL;
  bmem    :CARDINAL;
  brem    :CARDINAL;
  rate    :CARDINAL;

  l, nl :Link;
  size  :CARDINAL;
  sz    :CARDINAL;

  X2C_maxmem_saved :CARDINAL;


  PROCEDURE getNormalBusyMem (b :Block) :CARDINAL;
  BEGIN
    RETURN xrMM.NormalBlockSize - b^.fsum;
  END getNormalBusyMem;


  PROCEDURE obtainNewBlock() :BOOLEAN;

  (* MODIFIES: newb, freemem, nl *)

  BEGIN
    xrMM.NewBlock (xrMM.NORMAL, newb, xrMM.NormalBlockSize);

    IF (newb = NIL) THEN
      -- the OS refused to allocate a new block, terminate defragmentation
      RETURN FALSE;
    END;

    nl      := xrMM.getFirstBlockAdr (newb);
    freemem := xrMM.NormalBlockSize;
    RETURN TRUE;
  END obtainNewBlock;


  PROCEDURE completeNewBlock();

  (* USES: nl, freemem *)

  BEGIN
    IF (freemem > 0) THEN          -- add remainder to FO pool
      xrMM.setObjOfsLen (nl, freemem, SYSTEM.DIFADRC(nl, newb));
      stampObjAsFree (nl, freemem);
      xrMM.foManager_add (nl);
    END;             

    ASSERT (newb^.fsum = freemem, 14049);
  END completeNewBlock;

BEGIN
  X2C_maxmem_saved := rts.X2C_maxmem;
  rts.X2C_maxmem   := MAX(CARDINAL);

  b := blockList;

  normalBlocksRest      := 0;
  normalBlocksReleased  := 0;
  normalBlocksMoved     := 1;

  IF NOT obtainNewBlock() THEN
    rts.X2C_maxmem := X2C_maxmem_saved;
    RETURN;
  END;

  ib := b^.next;

  WHILE (ib # b) DO

    bmem := getNormalBusyMem (ib);
    ASSERT (bmem <= xrMM.NormalBlockSize, 14050);

    IF (bmem = 0) THEN
      ASSERT (NOT ib^.fixed, 14051);
    ELSE
      rate := (bmem * 100) DIV xrMM.NormalBlockSize;

      IF NOT ib^.fixed & (rate < MOVING_RATE) THEN  -- move object from this block
        size := 0;
        l    := xrMM.getFirstBlockAdr(ib);

        REPEAT
          sz := getHpObjSize(l);

          IF NOT (xrMM._free IN l^.tags) THEN

            ASSERT (l^.tags*{xrMM._sysbit, xrMM._expbit, xrMM._stack, xrMM._unmovable} = {}, 14066);

            IF (sz > freemem) THEN
              INC (normalBlocksMoved);

              completeNewBlock();

              IF NOT obtainNewBlock() THEN
                rts.X2C_maxmem := X2C_maxmem_saved;
                RETURN;
              END;
            END;

            MoveObject (l, nl, ib, newb, sz);
            nl := ADDADR (nl, sz);

            freemem := freemem - sz;
          END;

          INC (size, sz);
          l := ADDADR (l, sz);
        UNTIL (size >= xrMM.NormalBlockSize);

        INC (normalBlocksReleased);
      ELSE
        INC (normalBlocksRest);
      END;
    END;
    ib := ib^.next;
  END;

  completeNewBlock();

  rts.X2C_maxmem := X2C_maxmem_saved;
END DefragmentNormalBlocks;


<* IF STAT THEN *>

PROCEDURE Stat(msg-: ARRAY OF CHAR);
<* IF FULLSTAT THEN *>
  VAR i,j,no,bl,fr,size: CARDINAL; x,y: Block;
<* END *>
BEGIN
<* IF FULLSTAT THEN *>
  FOR i:=1 TO xrMM.Max DO
    size:=i*LINK_SZ;
    no:=0; fr:=0; bl:=0;
    y:=xrMM.f_blocks[i]; x:=y^.next;
    WHILE x # y DO
      j:=x^.fsum DIV size;
      INC(fr,j); INC(no,xrMM.parts[i]-j);
      x:=x^.next;
      INC(bl);
    END;
    y:=xrMM.b_blocks[i]; x:=y^.next;
    WHILE x # y DO
      ASSERT(x^.fsum < size,156);
      INC(no,xrMM.parts[i]);
      x:=x^.next;
      INC(bl);
    END;
    stdio.printf("%6d: %3d blocks, busy %6d objects, free %6d objects\n",
        size,bl,no,fr);
  END;
  no:=0; size:=0;
  y:=xrMM.f_blocks[xrMM.NORMAL]; x:=y^.next;
  WHILE x # y DO
    INC(size,x^.fsum); INC(no);
    x:=x^.next;
  END;
  y:=xrMM.b_blocks[xrMM.NORMAL]; x:=y^.next;
  WHILE x # y DO
    ASSERT(x^.fsum=0,157);
    INC(no);
    x:=x^.next;
  END;
  stdio.printf("NORMAL: %5d blocks, %6d free bytes\n",no,size);
  y:=xrMM.f_blocks[xrMM.LARGE]; x:=y^.next; no:=0; size:=0;
  WHILE x # y DO
    INC(size,x^.size); INC(no);
    x:=x^.next;
  END;
  stdio.printf("LARGE : %5d blocks, total %6d bytes\n",no,size);
  <* END *>
  stdio.printf("X2C_COLLECT, %s: %dK busy mem, %dK in objects\n",
                msg, rts.X2C_busymem DIV 1024, rts.X2C_usedmem DIV 1024);
  stdio.fflush(stdio.stdout^);
END Stat;

<* END *>

CONST
  FloatingHeaplimitFreeReserve = 4*1024*1024; -- size of free memory that 
                                              -- will be reserved anyway during
                                              -- increasing heaplimit

PROCEDURE cleanMarkbits ();

  PROCEDURE cleanBlocks (VAR blocks: ARRAY OF Block);
  VAR i :CARDINAL; root,x :Block;
      l :Link; sz, size :CARDINAL;
  BEGIN
    root:=blocks[xrMM.NORMAL]; x:=root^.next;
    WHILE x#root DO
      l  := xrMM.getFirstBlockAdr(x);
      sz := 0;
      LOOP
        EXCL(l^.tags, xrMM._markbit);
        size := getHpObjSize (l);
        IF (size = 0) THEN EXIT; END;

        l := ADDADR(l, size);
        INC (sz, size);
        IF sz >= xrMM.NormalBlockSize THEN EXIT; END;
      END;
      x:=x^.next;
    END;

    FOR i:=1 TO xrMM.Max DO
      root:=blocks[i]; x:=root^.next;
      size := i*LINK_SZ;
      WHILE x#root DO
        l := xrMM.getFirstBlockAdr(x);
        WHILE AdrLss(l, x^.mem) DO
          EXCL(l^.tags, xrMM._markbit);
          l := ADDADR(l, size);
        END;
        x:=x^.next;
      END;
    END;

    root:=blocks[xrMM.LARGE]; x:=root^.next;
    WHILE x#root DO
      l := xrMM.getFirstBlockAdr(x);
      EXCL(l^.tags, xrMM._markbit);
      x:=x^.next;
    END;
  END cleanBlocks;

BEGIN
  cleanBlocks (xrMM.f_blocks);
  cleanBlocks (xrMM.b_blocks);
END cleanMarkbits;



VAR
  MutatorStartTime :CARDINAL;
  GCStartTime      :CARDINAL;

CONST
  DefaultMaxGCTimePercent = 100; -- %


PROCEDURE TruncToCard(x :REAL): CARD32;
BEGIN
  IF x < 0.5 THEN
    RETURN 0;
  ELSIF x > VAL(REAL, MAX(CARD32))-0.5 THEN
    RETURN MAX(CARD32);
  ELSE
    RETURN VAL(CARD32, x);
  END;
END TruncToCard;


PROCEDURE COLLECT;
  PROCEDURE do(defrag: BOOLEAN);
    VAR d,l: Destruct;
  BEGIN
    marked:=NIL;
    tail:=NIL;
    MarkModules;   -- should be before MarkStack (for anchor tracing)

    IF xrMM.GCAUTO THEN MarkStack END;
    Closure;
    d:=MarkDestructors();
    IF d#NIL THEN Closure END;

    rts.X2C_smallused := 0;
    rts.X2C_smallbusy := 0;
    rts.X2C_normalused := 0;
    rts.X2C_normalbusy := 0;
    rts.X2C_largebusy  := 0;

    IF xrMM.anchorTracing THEN
      printSmallAnchorsWeight();
    END;

    IF xrMM.heapTracing THEN
      printHeapTrace(); -- after mark/closure, but before sweep
    END;

    Sweep(xrMM.f_blocks,FALSE,defrag);
    Sweep(xrMM.b_blocks,TRUE,defrag);

    IF xrMM.anchorTracing THEN
      printLargeObjects (); -- after sweep
      cleanMarkbits ();
    END;

    WHILE d#NIL DO
      l:=d; d:=d^.next;
      l^.proc(l^.adr);
    END;
  END do;

  VAR defrag: BOOLEAN; i: INTEGER;
      maxmaxmem, freePhysMem, minmaxmem :CARDINAL;
      curTime :CARDINAL;
      gcTrash :BOOLEAN;
BEGIN
  rts.X2C_PrepareToGC;
<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>
  p.printf ("BEFORE GC: usedmem = %d K, busymem = %d K, maxmem = %d K\n",
            rts.X2C_usedmem DIV 1024, rts.X2C_busymem DIV 1024, rts.X2C_maxmem DIV 1024);
<* END *>

  curTime := TimeConv.millisecs();
  IF (curTime >= MutatorStartTime) THEN
    MutatorPeriod := curTime - MutatorStartTime;
  END;
  GCStartTime := curTime;

  defrag := xrMM.DoDefrag OR xrMM.X2C_AlwaysDefrag;
  xrMM.DoDefrag:=FALSE;
  smallBlocksTotal       := 0;
  smallBlocksStackFixed  := 0;
  smallBlocksSysbitFixed := 0;
  smallBlocksExpbitFixed := 0;
  do(defrag);
  <* IF STAT THEN *> Stat('exit '); <* END *>
  IF defrag THEN
    <* IF STAT THEN *> moved_objects:=0; <* END *>
    SmallBlocksCompleted := 0;
    SmallBlocksReleased  := 0;
    FOR i:=1 TO xrMM.Max DO
      DefragSort(xrMM.f_blocks[i]);
      Defragment(xrMM.f_blocks[i]);
    END;
    DefragmentNormalBlocks (xrMM.f_blocks [xrMM.NORMAL]);

    AdjustRTStructures ();

    do(FALSE);
    <* IF STAT THEN *>
      stdio.printf('moved_objects=%d\n',moved_objects);
      Stat('defrag');
    <* END *>
  END;

  curTime := TimeConv.millisecs();
  IF (curTime >= GCStartTime) THEN
    GCPeriod := curTime - GCStartTime;
  END;
  MutatorStartTime := curTime;

<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>
  p.printf ("AFTER GC: usedmem = %d K, busymem = %d K, maxmem = %d K\n",
            rts.X2C_usedmem DIV 1024, rts.X2C_busymem DIV 1024, rts.X2C_maxmem DIV 1024);
<* END *>

  IF (MutatorPeriod # 0) & ((MAX(CARDINAL) DIV MutatorPeriod) < rts.X2C_MaxGCTimePercent) THEN
    -- overflow protection, less precise but w/o risk of overflow
    gcTrash := ((GCPeriod DIV rts.X2C_MaxGCTimePercent) > (MutatorPeriod DIV 100));
  ELSE
    gcTrash := (GCPeriod > ((rts.X2C_MaxGCTimePercent*MutatorPeriod) DIV 100));
  END;

  IF xrMM.FloatingHeaplimit THEN
    --p.printf ("@XDSLIB: Adaptive heaplimit computation\n");

    (* 285% of used memory, e.g. used memory is 35% of new heaplimit *)
    rts.X2C_maxmem := TruncToCard(VAL(REAL, rts.X2C_busymem + rts.X2C_busylheap + xrMM.SizeToAlloc) * 2.85);
    --p.printf ("@XDSLIB: Initial maxmem: %x (busy %x, size req. %x)\n", rts.X2C_maxmem, rts.X2C_busymem + rts.X2C_busylheap, xrMM.SizeToAlloc);

    (* 1st limit: heapLimit + size of large blocks *)
    maxmaxmem := xrBlockManager.heapLimit + rts.X2C_largebusy;
    IF rts.X2C_maxmem > maxmaxmem THEN
      rts.X2C_maxmem := maxmaxmem;
      --p.printf ("@XDSLIB: 1st limit hit: reserve limit: %x, largebusy: %x, maxmem: %x\n", xrBlockManager.heapLimit, rts.X2C_largebusy, maxmaxmem);
    END;

    freePhysMem := xosMem.X2C_GetAvailablePhysicalMemory ();
    --p.printf ("@XDSLIB: Free physical: %x\n", freePhysMem);
    IF freePhysMem # MAX(CARDINAL) THEN
      (* 2nd limit: at least 25% of physical memory should be free *)
      maxmaxmem := TruncToCard((VAL(REAL, freePhysMem) + VAL(REAL, rts.X2C_busymem)) * 0.75);

      (* however, if we are unable to allocate any more with new limit,
         but we still have free memory, we should use all free memory *)
      IF (rts.X2C_busymem + xrMM.SizeToAlloc) > maxmaxmem THEN
        (* Avoid overfloat, if rts.X2C_busymem+freePhysMem > MAX(CARDINAL) *)
        IF rts.X2C_busymem <= (MAX(CARDINAL) - freePhysMem) THEN 
          maxmaxmem := rts.X2C_busymem + freePhysMem;
        ELSE
          maxmaxmem := MAX(CARDINAL)
        END;

        (* we should reserve some free memory anyway,
           otherwise out of memory will never happen and
           program will go to swap *)
        IF maxmaxmem > FloatingHeaplimitFreeReserve*2 THEN
          maxmaxmem := maxmaxmem - FloatingHeaplimitFreeReserve;
        END;
      END;

      IF rts.X2C_maxmem > maxmaxmem THEN
        rts.X2C_maxmem := maxmaxmem;
        --p.printf ("@XDSLIB: 2nd limit hit: maxmem=%x\n", maxmaxmem);
      END;

      IF gcTrash THEN
        minmaxmem := VAL(CARD32, ((VAL(REAL, freePhysMem) + VAL(REAL, rts.X2C_busymem)) * 0.60));
        IF rts.X2C_maxmem < minmaxmem THEN
          rts.X2C_maxmem := minmaxmem;
          gcTrash := FALSE;
        END;
      END;
    END;
    
    (* we should hold at least already allocated memory *)
    IF rts.X2C_maxmem < rts.X2C_busymem THEN
      rts.X2C_maxmem := rts.X2C_busymem;
    END;

    (* we should hold at least MinFloatingMaxMem *)
    IF rts.X2C_maxmem < xrMM.MinFloatingMaxMem THEN
      rts.X2C_maxmem := xrMM.MinFloatingMaxMem;
    END;
  END;

  rts.X2C_GCThrashWarning := gcTrash;

  IF rts.X2C_threshold < rts.X2C_maxmem THEN
    IF rts.X2C_busymem < rts.X2C_threshold / 3 * 2 THEN
      xrMM.GCTHRESCNT:=rts.X2C_threshold - rts.X2C_busymem;
    ELSE
      xrMM.GCTHRESCNT:=rts.X2C_threshold / 3;
    END;
  END;

<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>
  AllocProfile_print();

  p.printf ("AFTER ADAPTIVE HL: usedmem = %d K, busymem = %d K, maxmem = %d K\n\n",
            rts.X2C_usedmem DIV 1024, rts.X2C_busymem DIV 1024, rts.X2C_maxmem DIV 1024);

  p.printf ("GC Took: %d ms\n", curTime - GCStartTime);
<* END *>

  rts.X2C_FreeAfterGC;
END COLLECT;

<* IF GC_IN_COROUTINE THEN *>

PROCEDURE COLLECT_LOOP;
BEGIN
  LOOP
    COLLECT;
    TRANSFER(CollectPrs,CollectPrs);
  END;
END COLLECT_LOOP;

<* END *>

PROCEDURE [2] X2C_COLLECTOR_PROC;
BEGIN
  <* IF STAT THEN *>
    stdio.printf("X2C_COLLECT, enter: %d (%d)\n"
                ,rts.X2C_busymem, rts.X2C_usedmem);
  <* END *>

<* IF GC_IN_COROUTINE THEN *>
  TRANSFER(CollectPrs,CollectPrs);
<* ELSE *>
  xrnCoroutines.X2C_SYNC_STATE;
  COLLECT;
<* END *>
END X2C_COLLECTOR_PROC;

PROCEDURE init_dynarrs;
  TYPE D = RECORD a: ADDRESS; n: ARRAY [0..0] OF SIZE_T END;
  VAR i: CARDINAL;
BEGIN
  dyn_offs[0]:=X2C.X2C_BASE;
  dyn_offs[1]:=X2C.X2C_OFS_END;
  FOR i:=0 TO xrMM.MaxDim-1 DO
    xrMM.ini_type_desc(dynarrs[i],"$DYNARR",
    SIZE(D)+i*2*SIZE(SIZE_T),SYSTEM.ADR(dyn_offs));
  END;
END init_dynarrs;



(* Stub for DLL unloading *)

VAR
  EmptyName   :ADDRESS;

PROCEDURE dllAssert;
BEGIN
  rts.X2C_TRAP_F(X2C.X2C_unreachDLL)
END dllAssert;

(* ---------------------- *)

<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>
PROCEDURE ["C"] / X2C_StdOutInit(h: xWin32.HANDLE); 
<* END *>

PROCEDURE X2C_INIT_O2MM;
BEGIN
  ASSERT (SZ_MASK = xrMM.SZ_MASK);
  <* IF multithread THEN *>
    rts.X2C_NewMutex(destLock);
    rts.X2C_NewMutex(modLock);
  <* END *>
  destruct:=NIL;
  ADR_ALIGMENT:=X2C.X2C_adr_aligment;
  TAG_END:=X2C.X2C_OFS_END;
  TAG_ARR:=X2C.X2C_OFS_ARR;
  TAG_REC:=X2C.X2C_OFS_REC;
  init_dynarrs;
<* IF GC_IN_COROUTINE THEN *>
  NEWCOROUTINE(COLLECT_LOOP,SYSTEM.ADR(prs_wsp),SIZE(prs_wsp),CollectPrs);
<* END *>
  <* IF NOT XMM_EXPLICIT THEN *>
    xrMM.COLLECTOR:=X2C_COLLECTOR_PROC;
  <* END *>
  xrMM.O2MM_init:=TRUE;
  rts.X2C_FINALEXE ( FinalAll );

  EmptyName   := NIL;  (* to patch type names of unloaded DLLs etc. *)

<* IF DEFINED(ALLOC_PROFILE) & ALLOC_PROFILE THEN *>
  AllocProfile_init();
  X2C_StdOutInit(xWin32.GetStdHandle(xWin32.STD_ERROR_HANDLE));
<* END *>

  rts.X2C_MaxGCTimePercent := DefaultMaxGCTimePercent;
  MutatorStartTime := TimeConv.millisecs();
END X2C_INIT_O2MM;



(*////////////////// GC data patching for DLL //////////////////////*)


<* IF env_target = "x86nt" THEN *>

VAR
  idBegin, idEnd :CARDINAL;

PROCEDURE getIDataExtent( hmod :ADDRESS);
TYPE
  PEHeader          = xrPEfmt.FileHeader;
  PEOptionalHeader  = xrPEfmt.OptionalHeader;
VAR
  pH    :POINTER TO PEHeader;
  pOH   :POINTER TO PEOptionalHeader;
BEGIN
  GET( hmod+60, pH );                -- PE header offset
  pH   := hmod+CAST(CARDINAL,pH)+4;  -- skip PE signature
  pOH  := ADDADR( pH, SIZE(PEHeader) );
  WITH pOH^.DataDir[xrPEfmt.DirEntryImport] DO
    idBegin := CARDINAL(hmod)+virtAddr;
    idEnd   := idBegin+size;
  END;
END getIDataExtent;
<* END *>

PROCEDURE patchTD ( VAR pTD :ADDRESS );
VAR
  p :Type;
BEGIN
  <* IF env_target = "x86nt" THEN *>
  IF (idBegin <= CARDINAL(pTD)) & (CARDINAL(pTD) < idEnd) THEN
    (* it is an indirect reference - handle it *)
    GET( pTD, p );
  ELSE
    p := pTD;
  END;
  <* ELSE *>
    p := pTD;
  <* END *>
  pTD := p^.self;
END patchTD;

PROCEDURE patchOffsScript ( pOfs :OFFS );
VAR
  i :CARDINAL;
  a :ADDRESS;
BEGIN
  i := 0;
  LOOP
    a := pOfs^[i];
    IF ( a = X2C.X2C_OFS_END ) THEN
      EXIT
    ELSIF ( a = X2C.X2C_OFS_REC) THEN
      INC(i);
      patchTD(pOfs^[i]);
    END;
    INC(i);
  END;
END patchOffsScript;


PROCEDURE ["C"] X2C_MODULEXE ( md :Module; hmod :ADDRESS );

VAR
  pTD, curTD :Type;
  i          :INT16;

BEGIN
  IF ~xrMM.O2MM_init THEN X2C_INIT_O2MM END;
  <* IF multithread THEN *> rts.X2C_LockMutex(modLock); <* END *>
  md^.next:=rts.X2C_MODULES;
  rts.X2C_MODULES := md;

  (*               patch ancestor tables & GC data
                   -------------------------------
     ASSUME: all ancestors (from a DLL) of this type are already copied.
     We may assume it because of X2C_MODULE is always called
     after initialization of modules imported *)


  <* IF env_target = "x86nt" THEN *> getIDataExtent( hmod ); <* END *>

  (* patch module global var script *)
  patchOffsScript ( OFFS(md^.offs) );

  curTD := md^.types;
  WHILE ( curTD # NIL ) DO
    WITH curTD^ DO
      (* patch ancestor table *)
      FOR i:=0 TO level DO
        patchTD ( base[i] );
      END;
      (* patch TD's field offset script *)
      patchOffsScript ( OFFS(offs) );
    END;
    curTD := curTD^.next;
  END;
  <* IF multithread THEN *> rts.X2C_FreeMutex(modLock); <* END *>
END X2C_MODULEXE;

---------------------------------


PROCEDURE getOfsScriptLen ( td :Type ) :CARDINAL;
TYPE
  p2A = POINTER TO ADDRESS;
VAR
  l :CARDINAL;
  p :p2A;
BEGIN
  l := SIZE(ADDRESS);
  p := p2A(td^.offs);
  WHILE ( p^ # X2C.X2C_OFS_END ) DO
    INC (l, SIZE(ADDRESS));
    p := p + SIZE(ADDRESS);
  END;
  RETURN l;
END getOfsScriptLen;



PROCEDURE ["C"] X2C_MODULEDLL (VAR component :Module;
                                          md :Module;
                                        hmod :ADDRESS );

VAR
  cmroom, room  :CARDINAL;

  hostM         :Module;
  curTD, destTD :Type;
  p             :OFFS;
------
VAR
  heapadr :ADDRESS;

  PROCEDURE heapCopy ( from :ADDRESS; size :CARDINAL );
  BEGIN
    MOVE ( from, heapadr, size );
    heapadr := ADDADR ( heapadr, size );
  END heapCopy;
------

BEGIN
  IF ~xrMM.O2MM_init THEN X2C_INIT_O2MM END;
  room   := SIZE(rts.X2C_MD_STR);
  curTD  := md^.types;
  WHILE ( curTD # NIL ) DO
    INC ( room, SIZE(rts.X2C_TD_STR)   +
                getOfsScriptLen(curTD) +
                VAL(CARDINAL,curTD^.methods )*SIZE(PROC) );
    curTD := curTD^.next;
  END;

  heapadr := xosMalloc.X2C_gmalloc ( room ); (* !!!! system bit *)

  (* copy MD *)
  hostM := heapadr;
  heapCopy (md, SIZE(rts.X2C_MD_STR) );

  curTD  := md^.types;
  destTD := NIL;

  WHILE ( curTD # NIL ) DO
    IF (destTD = NIL ) THEN
      hostM^.types := heapadr
    ELSE
      destTD^.next := heapadr
    END;

    WITH curTD^ DO
      (* copy TD *)
      destTD := CAST (Type, heapadr);
      self := destTD;
      heapCopy ( curTD, SIZE(rts.X2C_TD_STR) );
      destTD^.module := hostM;

      (* copy TD's field offset script *)
      destTD^.offs       := heapadr;
      heapCopy ( offs, getOfsScriptLen(curTD) );

      (* copy method table *)
      IF ( methods # 0 ) THEN
         destTD^.proc := heapadr;
         heapCopy ( proc, VAL(CARDINAL,methods)*SIZE(PROC) );
      END;
    END;

    curTD := curTD^.next;
  END;

  (* to bind in the component module list *)
  hostM^.cnext  := component;
  component := hostM;

  (* to bind in the GC module list & patch offsets *)
  X2C_MODULEXE ( hostM, hmod );
END X2C_MODULEDLL;



PROCEDURE ["C"] X2C_DISABLE_COMPONENT ( component :Module );
VAR
  curMD :Module;
  curTD :Type;
  i     :INT16;
  p     :POINTER TO PROC;
BEGIN
  IF ~TERMINATION.IsTerminating() & ~TERMINATION.HasHalted() THEN
     (* load-time DLL unloading has occured - patch type info *)
     curMD := component;
     WHILE ( curMD # NIL ) DO

       (* patch MD *)
       WITH curMD^ DO
         name := ADR (EmptyName);
         offs := ADR (TAG_END);
         cmds := NIL;
         cnms := ADR (EmptyName);
         curTD := types;
       END;

       (* patch all of its TD *)
       WHILE (curTD # NIL) DO
         WITH curTD^ DO
           name := ADR (EmptyName);
           p    := proc;
           FOR i:=1 TO methods DO
             p^ := dllAssert;
             p := p + SIZE(p^);
           END;
         END;
         curTD := curTD^.next;
       END;

       curMD := curMD^.cnext;
     END;
  END;
END X2C_DISABLE_COMPONENT;

END xrO2MM.
