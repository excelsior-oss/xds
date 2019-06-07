(* Copyright (C) 1996 XTech LTD *)
 
<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>
<*-IOVERFLOW    *>
<*-COVERFLOW    *>
<*-CHECKINDEX   *>
<*-CHECKRANGE   *>
<*-CHECKNIL     *>

IMPLEMENTATION MODULE ShtHeap ;

IMPORT SYSTEM, EXCEPTIONS,
       Strings;
FROM SYSTEM IMPORT ADDRESS, ADR, ADDADR, SUBADR,
                   TSIZE, BOOL32, LOC;

CONST
 HpMagic = 0DEADFACEH;
 PMagic  = 0ACE0FACEH;

TYPE
(* Piece control block *)

  pPCB = POINTER TO PCB;

  PCB = RECORD
          magic         :LONGCARD;
          isFree        :BOOL32;
          length        :Size;
          prevP, nextP  :pPCB;
          memory        :ARRAY[0..Align-1] OF LOC; (* dummy field - this is beginning of an actual memory piece *)
        END;

(* min unit of memory which can be allocated is Align byte as large *)

CONST
  lPCB  = TSIZE(PCB);
  ltPCB = lPCB - Align;

TYPE
(* Heap control block *)

 pHCB = POINTER TO HCB;

 HCB = RECORD
         magic   :LONGCARD;
         bFMList :pPCB;
         eFMList :pPCB;     (* to obtain the largest piece quickly *)
         total   :LONGCARD; (* to obtain total free memory         *)
         limit   :ADDRESS;  (* addr of the first byte out of the heap location *)
       END;

CONST
  lHCB = TSIZE(HCB);


VAR
  source :EXCEPTIONS.ExceptionSource;

PROCEDURE RAISE (n :CARDINAL; s- :ARRAY OF CHAR);
  VAR m: ARRAY [0..79] OF CHAR;
BEGIN
  Strings.Concat("ShtHeap.",s,m);
  EXCEPTIONS.RAISE(source,n,m);
END RAISE;

PROCEDURE IsShtHeapException (): BOOLEAN;
BEGIN
  RETURN EXCEPTIONS.IsCurrentSource(source)
END IsShtHeapException;

(*----------------------------------- Heap object ----------------------------------*)

(*
   There are three states of heap pieces : free    - piece in free list
                                           commit  - piece is commited by call Free
                                           hanging - this piece is neither free nor
                                                     commited (such piece exists only
                                                     where a library procedure
                                                     being had the control )

   At most one hanging piece can exist at any span of time

   Free list is supposed to be sorted in the order of piece size increasing

   Free list is empty <=> bFMList = NIL => eFMList is undefined
*)



VAR
  pHEAP :pHCB;

PROCEDURE IncpPCB(p :ADDRESS; n :LONGCARD) :pPCB;
BEGIN
  RETURN pPCB(ADDADR(p, n));
END IncpPCB;

(* create hanging piece *)
PROCEDURE CreateHPiece( P :ADDRESS; sz :Size ) :pPCB;
VAR pp :pPCB;
BEGIN
  pp := pPCB(P);
  WITH pp^ DO
    magic  := PMagic;
    isFree := BOOL32 (TRUE);
    nextP  := NIL;
    prevP  := NIL;
    length := sz;
  END;
  RETURN pp;
END CreateHPiece;


(* make free piece at pP to be hanging *)
PROCEDURE DelFPiece (pP :pPCB);
BEGIN
  WITH pP^ DO
    IF prevP # NIL
      THEN prevP^.nextP := nextP;
    END;
    IF nextP # NIL
      THEN nextP^.prevP := prevP;
    END;

    IF pP = pHEAP^.bFMList
      THEN pHEAP^.bFMList := nextP;
    END;
    IF pP = pHEAP^.eFMList
      THEN pHEAP^.eFMList := prevP;
    END;
  END;
END DelFPiece;


(*
   Utilize pieces which are the next and/or previous
   to the piece at pObjP in the heap memory location
*)

PROCEDURE MergeHPiece (VAR pObjP :pPCB);
VAR
  pP, pPrevP, pNextP :pPCB;
BEGIN
   pPrevP := NIL;
   pP     := IncpPCB( pHEAP, lHCB );  (* get the pointer to the first piece *)

   (* find the previous piece in memory -> pPrevP *)
   WHILE pP # pObjP DO
     pPrevP := pP;
     pP := IncpPCB(pP, ltPCB + pP^.length);
     IF LONGCARD(pP)>=LONGCARD(pHEAP^.limit)
       THEN RAISE(17,'Free: Argument is not pointer to heap or heap integrity violation');
     END;
   END;

   IF (pPrevP # NIL) AND (pPrevP^.isFree)
    THEN DelFPiece(pPrevP);
         INC(pPrevP^.length, pObjP^.length + ltPCB);
         INC(pHEAP^.total, ltPCB); (* Additionally free memory from header *)
         pObjP := pPrevP;
   END;
   pNextP := IncpPCB(pObjP, ltPCB + pObjP^.length);
   IF (LONGCARD(pNextP)<LONGCARD(pHEAP^.limit)) AND (pNextP^.isFree)
     THEN DelFPiece(pNextP);
          INC(pObjP^.length, pNextP^.length + ltPCB);
          INC(pHEAP^.total, ltPCB); (* Additionally free memory from header *)
   END;
END MergeHPiece;


(* make the hanging piece at pfp to be free *)

PROCEDURE InsHPiece (pfp :pPCB);
VAR
  pP, pPrevP    :pPCB;
  sz            :Size;
BEGIN
  pfp^.isFree := TRUE;
  WITH pHEAP^ DO
    sz     := pfp^.length;
    pP     := bFMList;
    pPrevP := NIL;

    WHILE (pP # NIL) AND (pP^.length < sz) DO
      pPrevP := pP;
      pP     := pP^.nextP;
    END;

    (* insert pfp between pPrevP and pP *)
    pfp^.prevP := pPrevP;
    pfp^.nextP := pP;

    IF pPrevP = NIL
      THEN bFMList := pfp;
      ELSE pPrevP^.nextP := pfp;
    END;

    IF pP = NIL
      THEN eFMList := pfp;
      ELSE pP^.prevP := pfp;
    END;
  END;
END InsHPiece;


(*---------------------------------------------------------------------------------*)

(* These procedures must set the variable pHEAP for correct working of heap object procedures *)

PROCEDURE Initialize (H :ADDRESS; sz :Size);
BEGIN
  sz := ((sz+Align-1) DIV Align) * Align;
  IF sz < lHCB + lPCB
    THEN RAISE (0,'Initialize: Size Too Small');
  END;

  pHEAP := pHCB (H);
  WITH pHEAP^ DO
    magic   := HpMagic;
    bFMList := NIL;
    limit   := ADDADR(H, sz);

    DEC (sz, ltPCB + lHCB);
    total   := sz;
    InsHPiece( CreateHPiece( ADDADR(H, lHCB), sz) );
  END;
END Initialize;


PROCEDURE Allocate ( H: ADDRESS; VAR P: ADDRESS; sz: Size );
VAR
  pP   :pPCB;
  rest :Size;
BEGIN

  pHEAP := pHCB( H );
  IF pHEAP^.magic # HpMagic THEN RAISE(0,'Allocate:  Argument is not pointer to heap'); END;
  IF Debug THEN Test ( H ); END;

  sz := ((sz+Align-1) DIV Align) * Align;
  IF sz = 0 THEN RAISE (0,'Allocate: Size Too Small'); END;

  pP := pHEAP^.bFMList;
  WHILE (pP # NIL) AND (pP^.length < sz)  DO  pP := pP^.nextP; END; 
  IF pP = NIL
   THEN IF Check
         THEN RAISE (0,'Allocate: Out Of Memory');
         ELSE P := NIL;
              RETURN;
        END;
  END;

  DelFPiece (pP);

  (* truncate free piece at pP (if any) *)
  IF pP^.length >= Align + sz + ltPCB
    THEN 
         rest := pP^.length - (sz + ltPCB);
         pP^.length := rest;
         InsHPiece (pP);

         (* create new piece *)
         pP := IncpPCB(pP, ltPCB + rest);
         pP := CreateHPiece (pP, sz);

         DEC (pHEAP^.total, ltPCB + sz);

    ELSE (* else whole piece is used *)
         DEC (pHEAP^.total, pP^.length);
  END;
  

  pP^.isFree := FALSE;

  P := ADR(pP^.memory);
  IF Clear THEN SYSTEM.FILL( P, 0, pP^.length ); END;

END Allocate;


PROCEDURE Free (H :ADDRESS; VAR P :ADDRESS; sz :Size);
VAR
  pP :pPCB;
BEGIN

  pHEAP := pHCB( H );
  pP    := pPCB( SUBADR(P, ltPCB) );
  IF pHEAP^.magic # HpMagic
    THEN RAISE(0,'Free: The first argument is not pointer to heap');
  END;
  IF pP^.magic # PMagic
    THEN RAISE(0,'Free: The second argument is not pointer to block');
  END;

  (* check that given size and piece length are not differ much *)
  IF (pP^.length < sz) OR (pP^.length >= sz + 2*Align + ltPCB) 
    THEN RAISE(0,'Free: Invalid block size');
  END;

(*  IF pP^.length # sz
    THEN RAISE(0,'Free: Invalid block size');
  END;*)

  IF Debug THEN Test (H); END;

  INC (pHEAP^.total, pP^.length);
  MergeHPiece ( pP );
  InsHPiece   ( pP );
  P := NIL;

END Free;


PROCEDURE Largest ( H :ADDRESS ): Size;
BEGIN
  pHEAP := pHCB( H );
  IF pHEAP^.magic # HpMagic THEN RAISE(0,'Largest: Argument is not pointer to heap'); END;
  IF Debug THEN Test (H); END;
  IF pHEAP^.eFMList = NIL
      THEN RETURN 0;
      ELSE RETURN pHEAP^.eFMList^.length;
  END;
END Largest;


PROCEDURE Total ( H :ADDRESS ) :Size;
BEGIN
  pHEAP := pHCB( H );
  WITH pHEAP^ DO
    IF magic # HpMagic THEN RAISE(0,'Total: Argument is not pointer to heap'); END;
    IF Debug THEN Test (H); END;
    IF bFMList = NIL
       THEN RETURN 0;
       ELSE RETURN total;
    END;
  END;
END Total;


(*
PROCEDURE Increase (H :ADDRESS; sz :Size);
VAR
  pP :pPCB;
BEGIN
  pHEAP := pHCB( H );
  IF pHEAP^.magic # HpMagic THEN RAISE(0,'Increase: Argument is not pointer to heap'); END;
  IF Debug THEN Test ( H ); END;

  sz := ((sz+Align-1) DIV Align) * Align;
  IF sz < lPCB THEN RAISE (0,'Increase: Size Too Small'); END;

-- Lib.RunTimeError(CoreSig._FatalErrorPos(), 9BH, 'ShtHeap.Increase : Size Too Large');

  pP           := pPCB(pHEAP^.limit);
  pHEAP^.limit := ADDADR(pHEAP^.limit, sz);

  pP := CreateHPiece (pP, sz - ltPCB);
  MergeHPiece (pP);
  InsHPiece (pP);
END Increase;
*)


PROCEDURE Test ( H: ADDRESS );
VAR
  pP, pPrevP :pPCB;
BEGIN
  pHEAP := pHCB( H );
  IF pHEAP^.magic # HpMagic THEN RAISE (0,'Test: Argument is not pointer to heap'); END;

  (* Required size of piece header (ltPCB) be aligned too!!! *)
  IF pHEAP^.total MOD Align # 0 THEN RAISE (0, 'Test: Non-aligned heap size'); END;

  pPrevP := NIL;
  pP := pHEAP^.bFMList;

  WHILE (pP # NIL) DO
    IF LONGCARD(pP) + pP^.length + ltPCB > LONGCARD(pHEAP^.limit)
     THEN  RAISE (0, 'Test: Free block out of range');
     ELSIF LONGCARD (pP) MOD Align # 0 THEN
           RAISE (0, 'Test: Non-aligned free block');
     ELSIF pP^.length MOD Align # 0 THEN
           RAISE (0, 'Test: Non-aligned free block size');
     ELSIF (pPrevP # NIL ) AND (pPrevP^.length > pP^.length) THEN
           RAISE (0, 'Test: Unordered free list');
    END;
    pPrevP := pP;
    pP     := pP^.nextP;
  END;

  pP     := IncpPCB( pHEAP, lHCB );               (* get the pointer to the first piece *)
  WHILE LONGCARD(pP) < LONGCARD(pHEAP^.limit) DO
    IF pP^.magic # PMagic
      THEN RAISE(17,'Test: Heap integrity violation');
    END;
    pP := IncpPCB(pP, ltPCB + pP^.length);
  END;


(*
  ELSIF (G # B) AND (Offset ( G ) + G^.Siz = Offset ( F )) THEN
      Lib.RunTimeError(CoreSig._FatalErrorPos(), 9FH,  "ShtHeap.Test: Unjoined free list" );
  ELSIF (G # B) AND (Offset ( G ) + G^.Siz > Offset ( F )) THEN
      Lib.RunTimeError(CoreSig._FatalErrorPos(), 9FH,  "ShtHeap.Test: Overlapping free list" );
*)

END Test;


BEGIN
  EXCEPTIONS.AllocateSource(source);

  Clear := FALSE;
  Check := TRUE;
  Debug := FALSE;
END ShtHeap.



