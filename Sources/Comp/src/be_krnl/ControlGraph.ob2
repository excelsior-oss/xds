<* IF DEFINED(MeDebugMode) AND MeDebugMode THEN *>
  <* procinline- *>
  <* noregvars+  *>
<* ELSE *>
  <* procinline+ *>
<* END *>
MODULE ControlGraph;

IMPORT ir;
IMPORT BitVect;
IMPORT env := xiEnv;
IMPORT SYSTEM;
IMPORT Calc;
IMPORT bv := BitVect;
IMPORT lh := LocalHeap;
<* IF ~ NODEBUG THEN *>
IMPORT opIO;
<* END *>
IMPORT opProcs;
IMPORT Printf;

TYPE BitVector = bv.BitVector;
TYPE
        INT        = ir.INT;
        TriadePtr  = ir.TriadePtr;
        ParamPtr   = ir.ParamPtr;
        VarNum     = ir.VarNum;
        Node       = ir.Node;
        TSNode     = Node;
        Arc        = INTEGER;
        Loop       = Node;
        Matrix     = ir.Matrix;
        Vector     = ir.Vector;

        NodeArray     = ir.NodeArray;
        TSNodeArray   = ir.TSNodeArray;
        NodeNodeArray = ir.NodeNodeArray;
        ArcArray      = ir.ArcArray;

TYPE    NodeType      = ir.NodeType;

        ArcPriority  *= LONGINT;

CONST
  arcNotImportant    *= -1;
  arcNormal          *= 0;
  arcImportant       *= 1;

TYPE
        ArcType   *= RECORD
                        f          *: Node;
                        t          *: Node;
                        LoopNo     *: Loop;
                        Original   *: BOOLEAN;
                        Priority   *: ArcPriority;
                     END;

        ArcRefs   *= POINTER TO ARRAY OF ArcType;

        LoopType *= RECORD
                        Preheader  *: Node;
                        BackEdge   *: Arc;
                        Body       *: BitVect.BitVector;
                        Exits      *: BitVect.BitVector;
                    END;

        LoopRefs  *= POINTER TO ARRAY OF LoopType;

     VAR
       Narcs*            : Arc;
       Arcs*             : ArcRefs;

       LoopsOk*          : BOOLEAN;
       NLoops*           : Loop;
       LoopList*         : LoopRefs;
       NoKillFlag        : BOOLEAN; -- used in loops construction

       IsDominators*     : BOOLEAN;
       IsaDominators*    : BOOLEAN;
       IsDominatorsTree* : BOOLEAN;
       IsOrder*          : BOOLEAN;

       IsDF*             : BOOLEAN;
       IsRDF*            : BOOLEAN;
       dfSet             : ir.BitMatrix;
       dfLen*            : ir.VectorNode;
       dfArray*          : ir.MatrixNode;
       Stop*             : Node;
       HasStop*          : BOOLEAN;

       IsPDominators*    : BOOLEAN;
       IsPDominatorsTree*: BOOLEAN;

CONST UndefLoop = ir.UndefLoop;

<* IF TARGET_386 AND fast_bitvect_in THEN *>
<* PUSH *>
<* NEW no_set_range_check+ *>
<*-CHECKNIL    *>
<*-CHECKDIV    *>
<*-CHECKPROC   *>
<*-CHECKSET    *>
<*-CHECKINDEX  *>
<*-CHECKDINDEX  *>
<*-CHECKRANGE  *>
<*-CHECKNIL    *>

PROCEDURE BitVect_In (p: BitVect.BitVector; e: INT): BOOLEAN;
<* NEW override_max_set_len+ *>
VAR foo:BitVect.LongBitVector;
BEGIN
    foo := SYSTEM.VAL(BitVect.LongBitVector,p);
    RETURN e IN foo^.v;
END BitVect_In;


PROCEDURE BitVect_Incl (p: BitVect.BitVector; e: INT);
BEGIN
    INCL (p^.v [e DIV BitVect.BitsPerSet], e MOD BitVect.BitsPerSet);
END BitVect_Incl;
<* POP *>

<* ELSE *>

PROCEDURE BitVect_In (p: BitVect.BitVector; e: INT): BOOLEAN;
BEGIN
    RETURN BitVect.In(p, e);
END BitVect_In;


PROCEDURE BitVect_Incl (p: BitVect.BitVector; e: INT);
BEGIN
    BitVect.Incl(p, e);
END BitVect_Incl;

<* END *>

(* -------------------------------------------------------------------------- *)

(*
  Вставить триаду q перед триадой p
*)
PROCEDURE InsertTriade* (q, p: TriadePtr);
BEGIN
    IF p^.Prev = NIL THEN
        ir.Nodes^[p^.NodeNo].First := q;
    ELSE
        p^.Prev^.Next := q;
    END;
    q^.Next   := p;
    q^.Prev   := p^.Prev;
    p^.Prev   := q;
    q^.NodeNo := p^.NodeNo;
END InsertTriade;

(*
  Вставить триаду q после триады p
*)
PROCEDURE PutAfterTriade* (q, p : TriadePtr);
BEGIN
     IF p^.Next = NIL THEN
        ir.Nodes^[p^.NodeNo].Last := q;
     ELSE
         p^.Next^.Prev := q;
     END;
     q^.Prev   := p;
     q^.Next   := p^.Next;
     p^.Next   := q;
     q^.NodeNo := p^.NodeNo;
END PutAfterTriade;

(*
  Вставить триаду q в начало линейного участка n
*)
PROCEDURE PutTriadeFirst* (q: TriadePtr; n: Node);
BEGIN
    q^.Next   := ir.Nodes^[n].First;
    q^.Prev   := NIL;
    q^.NodeNo := n;
    ir.Nodes^[n].First := q;
    IF q^.Next <> NIL THEN
        q^.Next^.Prev := q;
    ELSE
        ir.Nodes^[n].Last := q;
    END;
END PutTriadeFirst;

(*
  Вставить триаду q в конец линейного участка n
*)
PROCEDURE PutTriadeLast* (q: TriadePtr; n: Node);
BEGIN
    q^.Prev   := ir.Nodes^[n].Last;
    q^.Next   := NIL;
    q^.NodeNo := n;
    ir.Nodes^[n].Last := q;
    IF q^.Prev <> NIL THEN
        q^.Prev^.Next := q;
    ELSE
        ir.Nodes^[n].First := q;
    END;
END PutTriadeLast;

(*
  Удалить триаду p из списка
*)
PROCEDURE DeleteTriade* (p: TriadePtr);
BEGIN
    IF p^.Next = NIL THEN
        ir.Nodes^[p^.NodeNo].Last := p^.Prev;
    ELSE
        p^.Next^.Prev := p^.Prev;
    END;
    IF p^.Prev = NIL THEN
        ir.Nodes^[p^.NodeNo].First := p^.Next;
    ELSE
        p^.Prev^.Next := p^.Next;
    END;
    p^.NodeNo := MAX (Node);
END DeleteTriade;

(*
  Уничтожить триаду (НО НЕ ЕЕ РЕЗУЛЬТАТ!)
*)
PROCEDURE KillTriade* (p: TriadePtr);
BEGIN
    DeleteTriade (p);
    ir.KillParams   (p);
    BitVect.Free (p^.Read);
    BitVect.Free (p^.Write);
END KillTriade;

PROCEDURE KillTriade_Ex*(p: TriadePtr): TriadePtr;
VAR
  q: TriadePtr;
BEGIN
    q := p^.Next;
    KillTriade(p);
    RETURN q;
END KillTriade_Ex;

(*
  Создать триаду безусловного перехода
*)
PROCEDURE MakeGoto* (n: Node);
VAR q: TriadePtr;
BEGIN
    q          := ir.NewTriade (0);
    q^.Tag     := ir.y_Nothing;
    q^.OpType  := ir.t_void;
    q^.OpSize  := 0;
    q^.ResType := ir.t_void;
    q^.ResSize := 0;
    q^.Op      := ir.o_goto;
    PutTriadeLast (q, n);
END MakeGoto;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE TryAddWarning*(q: TriadePtr; wrn_no: INTEGER);
BEGIN
  IF q.Position.IsNull() OR (ir.o_Silent IN q.Options) THEN
    RETURN
  END;
  env.errors.Warning (q^.Position, wrn_no);
END TryAddWarning;

PROCEDURE TryAddWarning_Ex*(q: TriadePtr; wrn_no: INTEGER): BOOLEAN;
BEGIN
  IF q.Position.IsNull() OR (ir.o_Silent IN q.Options) THEN
    RETURN FALSE;
  END;
  env.errors.Warning (q^.Position, wrn_no);
  RETURN TRUE;
END TryAddWarning_Ex;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(*
  Уничтожить все триады до конца линейного участка
*)
PROCEDURE KillTriades* (q: TriadePtr; doAssert:=FALSE: BOOLEAN);
VAR
  was_warning: BOOLEAN;
BEGIN
    was_warning := FALSE;
    WHILE q <> NIL DO
        IF (q^.Op <> ir.o_getpar) & (q^.Op <> ir.o_error) & (q^.Op <> ir.o_forcont) & (q^.Op <> ir.o_goto) &
           NOT was_warning THEN
          was_warning := TryAddWarning_Ex(q, 311);
          ASSERT(~doAssert);
        END;
        IF q^.Tag = ir.y_Variable THEN
            ir.RemoveVar (q^.Name);
        END;
        q := KillTriade_Ex (q);
    END;
END KillTriades;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE^ TestDeleteFi (q: TriadePtr): TriadePtr;
PROCEDURE^ NewArc* (n1, n2: Node; original:BOOLEAN; priority:=arcNormal: ArcPriority);
PROCEDURE^ UpdateLoops (n: Node);
PROCEDURE^ KillLoops* (lp : Loop);

-- JET-1253 fix {
PROCEDURE LoosingParamSize(call: ir.TriadePtr; i: INTEGER; dst: ir.ParamPtr): BOOLEAN;
VAR seq, long, nosz: BOOLEAN;
BEGIN
    ASSERT(call.Op = ir.o_call);
    ASSERT(call.Params[i].tag = ir.y_Variable);

    seq := (i > 0) AND
           ( (opProcs.ProtoList[call.Prototype].npar <= i-1) OR
             (opProcs.ProtoList[call.Prototype].par[i-1].mode = opProcs.pm_seq) );

    long := (ir.Vars[ call.Params[i].name ].Def.ResType # ir.t_float) AND
            (ir.Vars[ call.Params[i].name ].Def.ResSize = 8);

    nosz := (dst.tag # ir.y_Variable);

    RETURN seq & long & nosz;
END LoosingParamSize;

PROCEDURE ExpandParams(tr: ir.TriadePtr; pos: INT);
VAR i : INT;
    params: ir.ParamArray;
BEGIN
  params := ir.NewParams(LEN(tr.Params^)+1, tr);
  FOR i := 0 TO LEN(tr.Params^)-1 DO
    IF i <= pos THEN
        ir.MoveParam(tr.Params[i], params[i]);
    ELSE
        ir.MoveParam(tr.Params[i], params[i+1]);
    END;
  END;
  tr.Params := params;
END ExpandParams;

PROCEDURE CopyParamSpecial(dst, src: ir.ParamPtr);
VAR tr: ir.TriadePtr;
    i: INTEGER;
BEGIN
    tr := src.triade;
    i := src.paramnumber;
    ASSERT(LoosingParamSize(tr, i, dst));
    ASSERT(dst.tag = ir.y_NumConst);

--    Printf.printf("\nJET-1253: XDSComp.ControlGraph.CopyParamSpecial\n");
    ir.MakeParNum(tr.Params[i],   Calc.GetValue(dst.value.get_NDWord(0), ir.t_unsign, 4));
    ExpandParams(tr, i);
    ir.MakeParNum(tr.Params[i+1], Calc.GetValue(dst.value.get_NDWord(1), ir.t_unsign, 4));
END CopyParamSpecial;
-- } JET-1253 fix

(*
  Заменить все вхождения переменной s на параметр d
*)
PROCEDURE RepParams* (s: ir.VarNum; d: ir.ParamPtr);
VAR
  p: ir.ParamPtr;
  t: ir.TriadePtr;
BEGIN
    IF d^.tag = ir.y_Variable THEN
        IF ir.Vars^[d^.name].LocalNo = ir.TEMPORARY THEN
            ir.Vars^[d^.name].LocalNo := ir.Vars^[s].LocalNo;
        END;
    END;
    LOOP
        p := ir.FirstUse (s);
        IF (p = NIL) OR ir.EqParams( d, p,
                            ir.Vars[s].Def.ResType, ir.Vars[s].Def.ResSize ) THEN
            RETURN;
        END;
        t := p^.triade;
        ir.RemoveUse(p);
        IF (t.Op = ir.o_call) AND LoosingParamSize(t, p.paramnumber, d) THEN
            CopyParamSpecial(d, p); -- JET-1253 fix
        ELSE
            ir.CopyParam (d, p);
        END;
        IF t^.Op = ir.o_fi THEN
            t^.Op := MAX (ir.Operation);
            RepParams (s, d);
            t^.Op := ir.o_fi;
            SYSTEM.EVAL(TestDeleteFi (t));
            RETURN;
        ELSIF (t^.Op = ir.o_store) & (d^.tag = ir.y_Nothing) THEN
            KillTriade (t);
        END;
    END;
END RepParams;

(*
  Заменить все вхождения переменной s на параметр d
*)
PROCEDURE ReplaceByParam* (p: TriadePtr; d: ParamPtr);
BEGIN
    RepParams (p^.Name, d);
    -- this condition is always TRUE for live variables
    IF ir.Vars^[p^.Name].Next = ir.UNDEFINED THEN
        ir.RemoveVar (p^.Name);
        KillTriade (p);
    END;
END ReplaceByParam;

(*
  Удалить явно избыточные fi-функции
*)
PROCEDURE TestDeleteFi* (q: TriadePtr): TriadePtr;
VAR i, j: INT;
       r: ParamPtr;
       v: VarNum;
BEGIN
    v := q^.Name;
    i := 0;
    LOOP
        IF (q^.Params^[i].tag <> ir.y_Variable) OR (q^.Params^[i].name <> v) THEN
            r := q^.Params^[i];
            EXIT;
        END;
        INC (i);
        IF i >= LEN (q^.Params^) THEN
            ir.NewParamPtr (r);
            ir.MakeParNothing(r);
            EXIT;
        END;
    END;
    FOR j:=i+1 TO LEN(q^.Params^)-1 DO
        IF ((q^.Params^[j].tag  <> ir.y_Variable) OR
            (q^.Params^[j].name <> v))
           &
            NOT ir.EqParams (q^.Params^[j], r, q^.ResType, q^.ResSize)
        THEN
            RETURN q^.Next;
        END;
    END;
    q^.Op := MAX (ir.Operation);
    RepParams (v, r);
    ir.RemoveVar (v);
    RETURN KillTriade_Ex (q);
END TestDeleteFi;

(*----------------------------------------------------------------------------*)
(*----------------------------------------------------------------------------*)

(*
  Удалить у триады один параметр
*)
PROCEDURE DeleteParameter* (q: TriadePtr; k: INT): TriadePtr;
VAR i, j: INT;
       s: TriadePtr;
       r: ir.ParamArray;
BEGIN
    IF LEN (q^.Params^) = 1 THEN
        s := q^.Next;
        ReplaceByParam (q, q^.Params^[0]);
        RETURN s;
    ELSIF (LEN (q^.Params^) = 2) & NOT q^.Params^[1-k].reverse THEN
        s := q^.Next;
        ReplaceByParam (q, q^.Params^[1-k]);
        RETURN s;
    ELSE
        r := ir.NewParams (LEN (q^.Params^) - 1, q);
        j := 0;
        FOR i:=0 TO LEN(q^.Params^)-1 DO
            IF i <> k THEN
                ir.CopyParamWithRev (q^.Params^[i], r^[j]);
                INC (j);
            END;
            IF q^.Params^[i].tag = ir.y_Variable THEN
                ir.RemoveUse(q^.Params^[i]);
            END;
        END;
        q^.Params := r;
        RETURN q;
    END;
END DeleteParameter;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                            Node operations                                 *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GraphChanged*;
BEGIN
    IsDF              := FALSE;
    IsRDF             := FALSE;
    IsOrder           := FALSE;
    IsDominators      := FALSE;
    IsaDominators     := FALSE;
    IsPDominators     := FALSE;
    IsDominatorsTree  := FALSE;
    IsPDominatorsTree := FALSE;
END GraphChanged;

(*
  Создать новый узел
*)
PROCEDURE NewNode* (): Node;
VAR i, n: Node;
       q: ir.NodeRefs;
BEGIN
    n := 0;
    LOOP
        IF n >= ir.Nnodes THEN
            IF n = LEN (ir.Nodes^) THEN
                NEW (q, LEN (ir.Nodes^) * 2);
                FOR i:=0 TO SYSTEM.PRED(n) DO
                    q^[i] := ir.Nodes^[i];
                END;
                ir.Nodes := q;
            END;
            INC (ir.Nnodes);
            EXIT;
        END;
        IF NOT ir.Nodes^[n].Alive THEN
            EXIT;
        END;
        INC (n);
    END;
    GraphChanged;
    ir.Nodes^[n].First       := NIL;
    ir.Nodes^[n].Last        := NIL;
    ir.Nodes^[n].NIn         := 0;
    ir.Nodes^[n].NOut        := 0;
    ir.Nodes^[n].Alive       := TRUE;
    ir.Nodes^[n].TopNumber   := MAX (TSNode);
    ir.Nodes^[n].Dominators  := NIL;
    ir.Nodes^[n].aDominators := NIL;
    ir.Nodes^[n].Nesting     := 0;
    ir.Nodes^[n].LoopNo      := ir.UndefLoop;
    ir.Nodes^[n].IndirectEnter := FALSE;
    ir.Nodes^[n].IsPreheaderOf := ir.UndefLoop;
    NEW (ir.Nodes^[n].InArcs,  2);
    NEW (ir.Nodes^[n].OutArcs, 2);
    NEW (ir.Nodes^[n].In,      2);
    NEW (ir.Nodes^[n].Out,     2);
    UpdateLoops (n);
    RETURN n;
END NewNode;

(*----------------------- gen_ir legacy --------------------------------------*)
(*----------------------- creation of triades in currNode --------------------*)

-- to split very long sequence of assignments we create
-- a new node after 'MAX_assign' assignments

VAR
  currNode*   : Node;
  lastTriade* : TriadePtr;
  assign_cnt* : INT;

-- returns FALSE if this triade must be the last one in node
PROCEDURE StartNewNode* ();
  VAR old: Node; last: TriadePtr;
BEGIN
  IF currNode = ir.UndefNode THEN currNode := NewNode();
  ELSIF (currNode = 0) OR
        (ir.Nodes[currNode].First # NIL) OR
        (ir.Nodes[currNode].NOut # 0)
    (* OR (ir.ir.Nodes[currNode].NIn # 0) *)
  THEN
    old := currNode;
    currNode := NewNode();
    last := ir.Nodes[old].Last;
    IF (last = NIL) OR
       ( NOT(ir.isLast IN ir.OpProperties[last.Op]) &
         NOT(ir.o_NoReturn IN last.Options) &
         NOT(ir.o_IsLast IN last.Options)
       )
    THEN
      MakeGoto(old);
      NewArc(old, currNode, TRUE)
    END;
  END;
  lastTriade := NIL;
  assign_cnt := 0;
END StartNewNode;

PROCEDURE StartNode* (new: Node);
BEGIN
  IF (currNode # ir.UndefNode) & (currNode # new) THEN
    IF (ir.Nodes[currNode].First = NIL) OR
       ( NOT(ir.isLast IN ir.OpProperties[ir.Nodes[currNode].Last.Op]) &
         ~(ir.o_NoReturn IN ir.Nodes[currNode].Last.Options)
       )
    THEN
      MakeGoto(currNode);
      NewArc(currNode, new, TRUE)
    END;
  END;
  currNode := new;
  lastTriade := NIL;
  assign_cnt := 0;
END StartNode;

PROCEDURE FinishNode* ;
BEGIN
  currNode := ir.UndefNode;
  lastTriade := NIL;
END FinishNode;

PROCEDURE AppendTr* (t: ir.TriadePtr);
BEGIN
  IF currNode = ir.UndefNode THEN currNode := NewNode() END;
  PutTriadeLast(t, currNode);
  lastTriade := t;
END AppendTr;

PROCEDURE Goto* (nd: ir.Node);
BEGIN
  IF currNode # ir.UndefNode THEN
    MakeGoto(currNode);
    NewArc(currNode, nd, TRUE);
    currNode := ir.UndefNode;        (* FinishNode; *)
  ELSE
    env.info.print("\nINTERNAL ERROR: Goto base lack\n");
    ASSERT(FALSE);
  END;
END Goto;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                    Topological sort / dominators                           *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

(*
  Собственно процедуры топологической сортировки
*)
PROCEDURE Search (n: Node);
VAR i: INT;
BEGIN
    ir.Nodes^[n].Processed := TRUE;
--    FOR i:=0 TO ir.Nodes^[n].NOut-1 DO
    FOR i:=ir.Nodes^[n].NOut-1 TO 0 BY -1 DO
        IF NOT ir.Nodes^[ir.Nodes^[n].Out^[i]].Processed THEN
            Search (ir.Nodes^[n].Out^[i]);
        END;
    END;
    DEC (ir.StartOrder);
    ir.Nodes^[n].TopNumber := ir.StartOrder;
    ir.Order^[ir.StartOrder]  := n;
END Search;

PROCEDURE TopSort*;
VAR i: Node;
BEGIN
    IF IsOrder THEN
        RETURN;
    END;
    IF (ir.Order = NIL) OR (LEN (ir.Order^) <> VAL(TSNode, ir.Nnodes)) THEN
        NEW (ir.Order, ir.Nnodes);
    END;
    FOR i:=0 TO SYSTEM.PRED(ir.Nnodes) DO
        ir.Nodes^[i].Processed := FALSE;
    END;
    ir.StartOrder := VAL(TSNode, ir.Nnodes);
    Search (0);
    IsOrder := TRUE;
END TopSort;

(* -------------------------------------------------------------------------- *)

(*
  Найти доминаторы в графе управления - итеративный алгоритм
*)

PROCEDURE FindDominators*;
VAR changed: BOOLEAN;
    j:    INT;
    i:    TSNode;
    n,sourceNode:       Node;
    new:  BitVect.BitVector;

BEGIN
    IF IsDominators THEN
        RETURN;
    END;
    -- Printf.printf("building fominators\n");
    TopSort;
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        BitVect.Realloc (ir.Nodes^[n].Dominators, ir.Nnodes);
        BitVect.Fill    (ir.Nodes^[n].Dominators, TRUE, ir.Nnodes);
    END;
    BitVect.Fill (ir.Nodes^[0].Dominators, FALSE, ir.Nnodes);
    BitVect_Incl (ir.Nodes^[0].Dominators, 0);
    new := BitVect.New (ir.Nnodes, TRUE);
    REPEAT
        changed := FALSE;
        FOR i:=SYSTEM.SUCC(ir.StartOrder) TO SYSTEM.PRED(LEN(ir.Order^)) DO
            n := ir.Order^[i];
            BitVect.Fill (new, TRUE, ir.Nnodes);
            FOR j:=0 TO ir.Nodes^[n].NIn-1 DO
                sourceNode := ir.Nodes^[n].In^[j];
                BitVect.Intersect (ir.Nodes^[sourceNode].Dominators,
                                 new, new);
            END;
            BitVect_Incl (new, ORD(n));
            changed := BitVect.Assign (new,ir.Nodes^[n].Dominators) OR changed;
        END;
    UNTIL NOT changed;
    BitVect.Free (new);
    IsDominators  := TRUE;
    IsPDominators := FALSE;
END FindDominators;

(* ------------------- TRUE if node p dominates over node q ----------------- *)

PROCEDURE Dominates* (p, q: Node): BOOLEAN;
BEGIN
    ASSERT (IsDominators);
    RETURN BitVect_In (ir.Nodes^[q].Dominators, ORD(p));
END Dominates;

(* ----------------- TRUE if triade p dominates over triade q --------------- *)

PROCEDURE DominatesTriade* (p, q: TriadePtr): BOOLEAN;
VAR s: TriadePtr;
BEGIN
--    ASSERT (IsDominators);
    IF p^.NodeNo = q^.NodeNo THEN
        IF (p.Next = NIL) OR (q.Prev = NIL) THEN
          RETURN FALSE;
        END;
        IF (p.Prev = NIL) OR (q.Next = NIL) THEN
          RETURN TRUE;
        END;
          
        s := p;
        LOOP
            s := s^.Next;
            IF s = NIL THEN
                RETURN FALSE;
            END;
            IF s = q THEN
                RETURN TRUE;
            END;
        END;
    ELSE
        RETURN BitVect_In (ir.Nodes^[q^.NodeNo].Dominators, ORD(p^.NodeNo));
    END;
END DominatesTriade;


PROCEDURE DominatesTriadeApprox*(p, q: TriadePtr): BOOLEAN;
BEGIN
    RETURN DominatesTriade(p, q);
END DominatesTriadeApprox;

(* -------------------------------------------------------------------------- *)

(*
  Найти дуги-доминаторы в графе управления - итеративный алгоритм
*)

PROCEDURE FindArcsDominators*;
VAR changed: BOOLEAN;
    j:    INT;
    i:    TSNode;
    n:       Node;
    new:     BitVect.BitVector;
    b:       BOOLEAN;
BEGIN
    IF IsaDominators THEN
        RETURN;
    END;
    -- Printf.printf("building a_fominators\n");
    TopSort;
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        BitVect.Realloc (ir.Nodes^[n].aDominators, ORD(Narcs));
        BitVect.Fill    (ir.Nodes^[n].aDominators, TRUE, ORD(Narcs));
    END;
    BitVect.Fill (ir.Nodes^[0].aDominators, FALSE, ORD(Narcs));
    new := BitVect.New (ORD(Narcs), FALSE);
    REPEAT
        changed := FALSE;
        FOR i:=SYSTEM.SUCC(ir.StartOrder) TO SYSTEM.PRED(LEN(ir.Order^)) DO
            n := ir.Order^[i];
            IF ir.Nodes^[n].NIn = 1 THEN
                BitVect.Move (ir.Nodes^[ir.Nodes^[n].In^[0]].aDominators,
                              ir.Nodes^[n].aDominators);
                BitVect_Incl (ir.Nodes^[n].aDominators, ORD(ir.Nodes^[n].InArcs^[0]));
            ELSE
                BitVect.Move (ir.Nodes^[ir.Nodes^[n].In^[0]].aDominators, new);
                BitVect_Incl (new, ORD(ir.Nodes^[n].InArcs^[0]));
                FOR j:=1 TO ir.Nodes^[n].NIn-1 DO
                    b := BitVect_In (new, ORD(ir.Nodes^[n].InArcs^[j]));
                    BitVect.Intersect (ir.Nodes^[ir.Nodes^[n].In^[j]].aDominators,
                                       new, new);
                    IF b THEN
                        BitVect_Incl (new, ORD(ir.Nodes^[n].InArcs^[j]));
                    END;
                END;
                changed := BitVect.Assign (new, ir.Nodes^[n].aDominators)
                           OR changed;
            END;
        END;
    UNTIL NOT changed;
    BitVect.Free (new);
    IsaDominators := TRUE;
END FindArcsDominators;

(* ------------------- TRUE if arc a dominates over node q ------------------ *)

PROCEDURE aDominates* (a: Arc; q: Node): BOOLEAN;
BEGIN
    ASSERT (IsaDominators);
    RETURN BitVect_In (ir.Nodes^[q].aDominators, ORD(a));
END aDominates;

(* -------------------------------------------------------------------------- *)

(*
  Построить дерево доминаторов
*)

PROCEDURE MakeDominatorsTree*;
VAR --j: INT;
    i, j: TSNode;
    n, d: Node;
BEGIN
    IF IsDominatorsTree THEN
        RETURN;
    END;
    FindDominators;
    ir.Nodes^[0].IDom     := ir.UndefNode;
    ir.Nodes^[0].DomChild := ir.UndefNode;
    ir.Nodes^[0].DomLink  := ir.UndefNode;
    FOR i:=SYSTEM.SUCC(ir.StartOrder) TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        j := SYSTEM.PRED(i);
        LOOP
            d := ir.Order^[j];
            IF BitVect_In (ir.Nodes^[n].Dominators, ORD(d)) THEN
                EXIT;
            END;
            DEC (j);
        END;
        ir.Nodes^[n].IDom     := d;
        ir.Nodes^[n].DomChild := ir.UndefNode;
        ir.Nodes^[n].DomLink  := ir.Nodes^[d].DomChild;
        ir.Nodes^[d].DomChild := n;
    END;
    IsDominatorsTree  := TRUE;
    IsPDominatorsTree := FALSE;
END MakeDominatorsTree;

(* -------------------------------------------------------------------------- *)

(*
  Отметить все достижимые в Reverse CFG узлы
*)

PROCEDURE SearchBack (n: Node; v: BitVect.BitVector);
VAR i: INT;
BEGIN
    BitVect_Incl (v, ORD(n));
    FOR i:=0 TO ir.Nodes^[n].NIn-1 DO
        IF NOT BitVect_In (v, ORD(ir.Nodes^[n].In^[i])) THEN
            SearchBack (ir.Nodes^[n].In^[i], v);
        END;
    END;
END SearchBack;

(* -------------------------------------------------------------------------- *)

(*
  Найти постдоминаторы в графе управления - итеративный алгоритм
*)

PROCEDURE FindPDominators*;
VAR changed: BOOLEAN;
    j:    INT;
    i:  TSNode;
    n:       Node;
    new:     BitVect.BitVector;
BEGIN
    IF IsPDominators THEN
        RETURN;
    END;
    TopSort;
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        BitVect.Realloc (ir.Nodes^[n].Dominators, ir.Nnodes);
        BitVect.Fill    (ir.Nodes^[n].Dominators, TRUE, ir.Nnodes);
    END;
    BitVect.Fill (ir.Nodes^[Stop].Dominators, FALSE, ir.Nnodes);
    BitVect_Incl (ir.Nodes^[Stop].Dominators, ORD(Stop));
    new := BitVect.New (ir.Nnodes, FALSE);
    REPEAT
        changed := FALSE;
        FOR i:=SYSTEM.PRED(LEN(ir.Order^)) TO ir.StartOrder BY -1 DO
            n := ir.Order^[i];
            IF ir.Nodes^[n].NOut = 1 THEN
                BitVect.Move (ir.Nodes^[ir.Nodes^[n].Out^[0]].Dominators,
                              ir.Nodes^[n].Dominators);
                BitVect_Incl (ir.Nodes^[n].Dominators, ORD(n));
            ELSIF n <> Stop THEN
                BitVect.Move (ir.Nodes^[ir.Nodes^[n].Out^[0]].Dominators, new);
                FOR j:=1 TO ir.Nodes^[n].NOut-1 DO
                    BitVect.Intersect (ir.Nodes^[ir.Nodes^[n].Out^[j]].Dominators,
                                       new, new);
                END;
                BitVect_Incl (new, ORD(n));
                changed := BitVect.Assign (new,ir.Nodes^[n].Dominators) OR changed;
            END;
        END;
    UNTIL NOT changed;
    BitVect.Free (new);
    IsPDominators := TRUE;
    IsDominators  := FALSE;
END FindPDominators;

(* -------------------------------------------------------------------------- *)

(*
  Построить дерево постдоминаторов
*)

PROCEDURE MakePDominatorsTree*;
VAR i, j:    TSNode;
    n, d, e: Node;
    Changed: BOOLEAN;
BEGIN
    IF IsPDominatorsTree THEN
        RETURN;
    END;
    FindPDominators;
    ir.Nodes^[Stop].IDom     := ir.UndefNode;
    ir.Nodes^[Stop].DomChild := ir.UndefNode;
    ir.Nodes^[Stop].DomLink  := ir.UndefNode;
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        ir.Nodes^[ir.Order^[i]].DomChild := ir.UndefNode;
    END;
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        IF n <> Stop THEN
            d := Stop;
            REPEAT
                Changed := FALSE;
                FOR j:=SYSTEM.PRED(LEN(ir.Order^)) TO ir.StartOrder BY -1 DO
                    e := ir.Order^[j];
                    IF (e <> n) & BitVect_In (ir.Nodes^[n].Dominators, ORD(e)) &
                       (e <> d) & BitVect_In (ir.Nodes^[e].Dominators, ORD(d))
                    THEN
                        d       := e;
                        Changed := TRUE;
                    END;
                END;
            UNTIL NOT Changed;
            ir.Nodes^[n].IDom     := d;
            ir.Nodes^[n].DomLink  := ir.Nodes^[d].DomChild;
            ir.Nodes^[d].DomChild := n;
        END;
    END;
    IsPDominatorsTree := TRUE;
    IsDominatorsTree  := FALSE;
END MakePDominatorsTree;

(* -------------------------------------------------------------------------- *)

(*
  Создать новую битовую матрицу
*)

PROCEDURE NewBitMatrix (): ir.BitMatrix;
VAR m: ir.BitMatrix;
    i: Node;
BEGIN
    NEW (m, ir.Nnodes);
    FOR i:=0 TO SYSTEM.PRED(ir.Nnodes) DO
        IF ir.Nodes^[i].Alive THEN
            m^[i] := BitVect.New (ir.Nnodes, FALSE);
        ELSE
            m^[i] := NIL;
        END;
    END;
    RETURN m;
END NewBitMatrix;

(* -------------------------------------------------------------------------- *)

(*
  Освободить битовую матрицу
*)

PROCEDURE FreeBitMatrix* (VAR m: ir.BitMatrix);
VAR i: INT;
BEGIN
    IF m <> NIL THEN
        FOR i:=0 TO LEN(m^)-1 DO
            BitVect.Free (m^[i]);
        END;
        m := NIL;
    END;
END FreeBitMatrix;

(* -------------------------------------------------------------------------- *)

(*
  Создать новую матрицу узлов
*)

PROCEDURE NewMatrix (VAR m: Matrix; VAR l: Vector);
VAR i: Node;
BEGIN
    IF (m = NIL) OR (VAL(Node, LEN (m^)) < ir.Nnodes) THEN
        NEW (m, ir.Nnodes);
    END;
    IF (l = NIL) OR (VAL(Node, LEN (l^)) < ir.Nnodes) THEN
        NEW (l, ir.Nnodes);
    END;
    FOR i:=0 TO SYSTEM.PRED(ir.Nnodes) DO
        IF ir.Nodes^[i].Alive THEN
            IF (m^[i] = NIL) OR (VAL(Node, LEN (m^[i]^)) < ir.Nnodes) THEN
                NEW (m^[i], ir.Nnodes);
            END;
        ELSE
            m^[i] := NIL;
        END;
        l^[i] := 0;
    END;
END NewMatrix;

(* -------------------------------------------------------------------------- *)

(*
  Посчитать dominance frontier
*)

PROCEDURE DFStep (x: Node);
VAR z, y: Node;
    i, l: INT;
       p: NodeArray;
       v: BitVect.BitVector;
BEGIN
    z := ir.Nodes^[x].DomChild;
    WHILE z <> ir.UndefNode DO
        DFStep (z);
        z := ir.Nodes^[z].DomLink;
    END;
    p := dfArray^[x];
    v := dfSet^  [x];
    l := 0;
    FOR i:=0 TO ir.Nodes^[x].NOut-1 DO
        y := ir.Nodes^[x].Out^[i];
        IF (ir.Nodes^[y].IDom <> x) & NOT BitVect_In (v, ORD(y)) THEN
            BitVect_Incl (v, ORD(y));
            p^[l] := y;
            INC (l);
        END;
    END;
    z := ir.Nodes^[x].DomChild;
    WHILE z <> ir.UndefNode DO
        FOR i:=0 TO dfLen^[z]-1 DO
            y := dfArray^[z]^[i];
            IF (ir.Nodes^[y].IDom <> x) & NOT BitVect_In (v, ORD(y)) THEN
                BitVect_Incl (v, ORD(y));
                p^[l] := y;
                INC (l);
            END;
        END;
        z := ir.Nodes^[z].DomLink;
    END;
    dfLen^[x] := l;
END DFStep;

(* -------------------------------------------------------------------------- *)

PROCEDURE FindDF*;
BEGIN
    IF IsDF THEN
        RETURN;
    END;
    MakeDominatorsTree;
    dfSet := NewBitMatrix ();
    NewMatrix (SYSTEM.VAL(Matrix, dfArray), SYSTEM.VAL(Vector, dfLen));
    DFStep (0);
    FreeBitMatrix (dfSet);
    IsDF  := TRUE;
    IsRDF := FALSE;
END FindDF;

(* -------------------------------------------------------------------------- *)

(*
  Посчитать dominance frontier в RDF
*)

PROCEDURE RDFStep (x: Node);
VAR z, y: Node;
    i, l: INT;
       p: NodeArray;
       v: BitVect.BitVector;
BEGIN
    z := ir.Nodes^[x].DomChild;
    WHILE z <> ir.UndefNode DO
        RDFStep (z);
        z := ir.Nodes^[z].DomLink;
    END;
    p := dfArray^[x];
    v := dfSet^  [x];
    l := 0;
    FOR i:=0 TO ir.Nodes^[x].NIn-1 DO
        y := ir.Nodes^[x].In^[i];
        IF ( (ir.Nodes^[y].IDom <> x) OR
             (ir.Nodes[y].NOut # 1)
           ) &
           NOT BitVect_In (v, ORD(y))
        THEN
            BitVect_Incl (v, ORD(y));
            p^[l] := y;
            INC (l);
        END;
    END;
    z := ir.Nodes^[x].DomChild;
    WHILE z <> ir.UndefNode DO
        FOR i:=0 TO dfLen^[z]-1 DO
            y := dfArray^[z]^[i];
            IF (ir.Nodes^[y].IDom <> x) & NOT BitVect_In (v, ORD(y)) THEN
                BitVect_Incl (v, ORD(y));
                p^[l] := y;
                INC (l);
            END;
        END;
        z := ir.Nodes^[z].DomLink;
    END;
    dfLen^[x] := l;
END RDFStep;

(* -------------------------------------------------------------------------- *)

PROCEDURE FindRDF*;
BEGIN
    IF IsRDF THEN
        RETURN;
    END;
    MakePDominatorsTree;
    dfSet := NewBitMatrix ();
    NewMatrix (SYSTEM.VAL(Matrix, dfArray), SYSTEM.VAL(Vector, dfLen));
    RDFStep (Stop);
    FreeBitMatrix (dfSet);
    IsRDF := TRUE;
    IsDF  := FALSE;
END FindRDF;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                            Arcs operations                                 *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

(*
  Найти порядковый номер дуги a во входных дугах
*)

PROCEDURE FindInArc* (a: Arc): INT;
VAR i: INT;
BEGIN
    FOR i:=0 TO ir.Nodes^[Arcs^[a].t].NIn-1 DO
        IF ir.Nodes^[Arcs^[a].t].InArcs^[i] = a THEN
            RETURN i;
        END;
    END;
END FindInArc;

(* -------------------------------------------------------------------------- *)

(*
  Найти порядковый номер дуги a в выходных дугах
*)

PROCEDURE FindOutArc* (a: Arc): INT;
VAR i: INT;
BEGIN
    FOR i:=0 TO ir.Nodes^[Arcs^[a].f].NOut-1 DO
        IF ir.Nodes^[Arcs^[a].f].OutArcs^[i] = a THEN
            RETURN i;
        END;
    END;
END FindOutArc;

(* -------------------------------------------------------------------------- *)

(*
  Обменять две выходные дуги местами
*)

PROCEDURE ExchangeArcs* (n: Node);
VAR a: Arc;
    k: Node;
BEGIN
    ASSERT (ir.Nodes^[n].NOut = 2);
    a                     := ir.Nodes^[n].OutArcs^[0];
    ir.Nodes^[n].OutArcs^[0] := ir.Nodes^[n].OutArcs^[1];
    ir.Nodes^[n].OutArcs^[1] := a;
    k                     := ir.Nodes^[n].Out^[0];
    ir.Nodes^[n].Out^[0]     := ir.Nodes^[n].Out^[1];
    ir.Nodes^[n].Out^[1]     := k;
END ExchangeArcs;

(* -------------------------------------------------------------------------- *)

(*
  Добавить пару дуга/узел в массивы InArcs и In или OutArcs и Out
*)

PROCEDURE AddNodeArc* (VAR p: ArcArray;   a: Arc;
                       VAR r: NodeArray; nd: Node; VAR n: INT);
VAR i: INT;
    q: ArcArray;
    s: NodeArray;
BEGIN
    IF n = LEN (p^) THEN
        NEW (q, LEN (p^) * 2);
        FOR i:=0 TO n-1 DO
            q^[i] := p^[i];
        END;
        p := q;
    END;
    p^[n] := a;
    IF n = LEN (r^) THEN
        NEW (s, LEN (r^) * 2);
        FOR i:=0 TO n-1 DO
            s^[i] := r^[i];
        END;
        r := s;
    END;
    r^[n] := nd;
    INC (n);
END AddNodeArc;

(* -------------------------------------------------------------------------- *)

(*
  Провести новую дугу из n1 в n2
*)

PROCEDURE GetNewArc (n1, n2: Node; original: BOOLEAN; priority := arcNormal: ArcPriority): Arc;
VAR a, i: Arc;
    q: ArcRefs;
BEGIN
    ASSERT (n2 <> 0);
    a := 0;
    LOOP
        IF a >= Narcs THEN
            IF a = LEN (Arcs^) THEN
                NEW (q, LEN (Arcs^) * 2);
                FOR i:= 0 TO SYSTEM.PRED(a) DO
                    q^[i] := Arcs^[i];
                END;
                Arcs := q;
            END;
            INC (Narcs);
            EXIT;
        END;
        IF Arcs^[a].t = 0 THEN
            EXIT;
        END;
        INC (a);
    END;
    Arcs^[a].f        := n1;
    Arcs^[a].t        := n2;
    Arcs^[a].LoopNo   := ir.UndefLoop;
    Arcs^[a].Original := original;
    Arcs^[a].Priority := priority;

    AddNodeArc (ir.Nodes^[n1].OutArcs, a, ir.Nodes^[n1].Out, n2, ir.Nodes^[n1].NOut);
    AddNodeArc (ir.Nodes^[n2].InArcs,  a, ir.Nodes^[n2].In,  n1, ir.Nodes^[n2].NIn);
    KillLoops (ir.Nodes^[n1].LoopNo);
    KillLoops (ir.Nodes^[n2].LoopNo);
    GraphChanged;
    RETURN a;
END GetNewArc;

(* -------------------------------------------------------------------------- *)

(*
  Провести новую дугу из n1 в n2
*)

PROCEDURE NewArc* (n1, n2: Node; original : BOOLEAN; priority := arcNormal: ArcPriority);
VAR a: ir.Arc;
BEGIN
    a := GetNewArc (n1, n2, original);
    Arcs[a].Priority := priority;
END NewArc;

(* -------------------------------------------------------------------------- *)

(*
  Убрать дугу из списка входных дуг
*)

PROCEDURE KillInArc* (a: Arc);
VAR j: INT;
    n: Node;
    p: TriadePtr;
BEGIN
    n := Arcs^[a].t;
    j := FindInArc (a);
    p := ir.Nodes^[n].First;
    WHILE (p <> NIL) & (p^.Op = ir.o_fi) DO
        IF LEN (p^.Params^) = ir.Nodes^[n].NIn THEN
            SYSTEM.EVAL(DeleteParameter (p, j));
            p := ir.Nodes^[n].First;
        ELSE
            p := p^.Next;
        END;
    END;
    FOR j:=j TO ir.Nodes^[n].NIn-2 DO
        ir.Nodes^[n].In^     [j] := ir.Nodes^[n].In^     [j+1];
        ir.Nodes^[n].InArcs^ [j] := ir.Nodes^[n].InArcs^ [j+1];
    END;
    DEC (ir.Nodes^[n].NIn);
END KillInArc;

(* -------------------------------------------------------------------------- *)

(*
  Убрать дугу из списка выходных дуг
*)

PROCEDURE KillOutArc* (a : Arc);
VAR j: INT;
    n: Node;
BEGIN
    n := Arcs^[a].f;
    FOR j:=FindOutArc (a) TO ir.Nodes^[n].NOut-2 DO
        ir.Nodes^[n].Out^     [j] := ir.Nodes^[n].Out^     [j+1];
        ir.Nodes^[n].OutArcs^ [j] := ir.Nodes^[n].OutArcs^ [j+1];
    END;
    DEC (ir.Nodes^[n].NOut);
END KillOutArc;

(* -------------------------------------------------------------------------- *)

(*
  Отметить дугу как свободную
*)

PROCEDURE FreeArc (a: Arc);
BEGIN
    Arcs^[a].f := 0;
    Arcs^[a].t := 0;
END FreeArc;

(* -------------------------------------------------------------------------- *)

(*
  Уничтожить дугу
*)

PROCEDURE KillArc_Ex* (a: Arc; graphChanged: BOOLEAN);
BEGIN
    KillLoops  (ir.Nodes^[Arcs^[a].f].LoopNo);
    KillLoops  (ir.Nodes^[Arcs^[a].t].LoopNo);
    KillInArc  (a);
    KillOutArc (a);
    FreeArc    (a);
    IF graphChanged THEN
      GraphChanged();
    END;
END KillArc_Ex;

PROCEDURE KillArc* (a: Arc);
BEGIN
    KillArc_Ex(a, TRUE);
END KillArc;

(* -------------------------------------------------------------------------- *)

(*
  Retarget all incoming into s arcs into d
*)

PROCEDURE RetargetAllArcs* (s, d: Node);
VAR i, j: INT;
    a:    Arc;
BEGIN
    KillLoops (ir.Nodes^[s].LoopNo);
    KillLoops (ir.Nodes^[d].LoopNo);
    FOR i:=0 TO ir.Nodes^[s].NIn-1 DO
        a := ir.Nodes^[s].InArcs^[i];
        KillLoops (ir.Nodes^[Arcs^[a].f].LoopNo);
        j := FindOutArc (a);
        Arcs^[a].t := d;
        AddNodeArc (ir.Nodes^[d].InArcs,a,ir.Nodes^[d].In,Arcs^[a].f,ir.Nodes^[d].NIn);
        ir.Nodes^[Arcs^[a].f].Out^[j] := d;
    END;
    ir.Nodes^[s].NIn := 0;
END RetargetAllArcs;

PROCEDURE RetargetAllOutgoingArcs*(from, to : ir.Node);
VAR i, j: INT;
    a:    Arc;
BEGIN
    KillLoops (ir.Nodes^[from].LoopNo);
    KillLoops (ir.Nodes^[to].LoopNo);
    FOR i:=0 TO ir.Nodes^[from].NOut-1 DO
        a := ir.Nodes^[from].OutArcs^[i];
        KillLoops (ir.Nodes^[Arcs^[a].t].LoopNo);
        j := FindInArc (a);
        Arcs^[a].f := to;
        AddNodeArc (ir.Nodes^[to].OutArcs, a,
                    ir.Nodes^[to].Out, Arcs^[a].t,
                    ir.Nodes^[to].NOut);
        ir.Nodes^[ Arcs^[a].t ].In^[j] := to;
    END;
    ir.Nodes^[from].NOut := 0;
END RetargetAllOutgoingArcs;

(* -------------------------------------------------------------------------- *)

(*
  Создать новый узел в середине дуги a
*)

PROCEDURE SplitArc* (a: Arc): Node;
VAR z:       Arc;
    f, t, n: Node;
    i:       INT;
BEGIN
    n := NewNode ();
    MakeGoto (n);
    f := Arcs^[a].f;
    t := Arcs^[a].t;
    ir.Nodes^[n].Last^.Position := ir.Nodes^[f].Last^.Position;
    z := GetNewArc (n, n, FALSE);
    i := FindInArc (a);
    ir.Nodes^[t].InArcs^[i] := z;
    ir.Nodes^[t].In^    [i] := n;
    ir.Nodes^[f].Out^[FindOutArc (a)] := n;
    ir.Nodes^[n].InArcs^[0] := a;
    ir.Nodes^[n].In^    [0] := f;
    ir.Nodes^[n].Out^   [0] := t;
    Arcs^[a].t := n;
    Arcs^[z].t := t;
    KillLoops (ir.Nodes^[f].LoopNo);
    KillLoops (ir.Nodes^[t].LoopNo);
    RETURN n;
END SplitArc;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                            Loops operations                                *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

(*
  Принадлежит ли узел циклу?
*)

PROCEDURE NodeInLoop* (n: Node; i: Loop): BOOLEAN;
BEGIN
    RETURN (LoopList^[i].Body <> NIL)
      & BitVect_In (LoopList^[i].Body, ORD(n));
END NodeInLoop;

PROCEDURE IsAddressedInLoop*(local: ir.Local; loop: Loop): BOOLEAN;
VAR o: ir.Node;
    p: TriadePtr;
    i: INT;
BEGIN
    IF loop = UndefLoop THEN
      RETURN ir.Locals[local].Addressed;
    END;
    FOR o:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
      IF NodeInLoop(ir.Order[o],loop) THEN
        p := ir.Nodes[ir.Order[o]].First;
        WHILE p # NIL DO
          IF p.Params # NIL THEN
            FOR i:=0 TO LEN (p^.Params^)-1 DO
              IF p^.Params^[i].tag = ir.y_AddrConst THEN
                  RETURN TRUE;
              END;
            END;
          END;
          p := p.Next;
        END;
      END;
    END;
    RETURN FALSE;
END IsAddressedInLoop;

(* ---------------------- TRUE if lp1 IN lp2 -------------------------------- *)

PROCEDURE LoopInLoop* (lp1, lp2 : Loop): BOOLEAN;
BEGIN
    IF (lp1 = UndefLoop) THEN
        RETURN FALSE;
    ELSIF (lp2 = UndefLoop) THEN
        RETURN TRUE;
    END;
    RETURN NodeInLoop (LoopList^[lp1].Preheader, lp2);
END LoopInLoop;

(* -------------------------------------------------------------------------- *)

(*
  Уничтожить цикл
*)

PROCEDURE KillLoop* (lp : Loop);
VAR n: Node;
BEGIN
    IF NoKillFlag THEN
        RETURN;
    END;
    IF lp <> UndefLoop THEN
        LoopsOk := FALSE;
        Arcs^[LoopList^[lp].BackEdge].LoopNo := UndefLoop;
        BitVect.Free (LoopList^[lp].Body);
        BitVect.Free (LoopList^[lp].Exits);
        FOR n:=Node{ 1 } TO SYSTEM.PRED(ir.Nnodes) DO
            IF ir.Nodes^[n].Alive & (ir.Nodes^[n].LoopNo = lp) THEN
                ir.Nodes^[n].LoopNo := UndefLoop;
            END;
        END;
        IF LoopList[lp].Preheader # UndefLoop THEN
          ir.Nodes[LoopList[lp].Preheader].IsPreheaderOf := UndefLoop;
        END;
    END;
END KillLoop;

(* -------------------------------------------------------------------------- *)

(*
  Уничтожить цикл и все объемлющие
*)

PROCEDURE KillLoops* (lp : Loop);
VAR l: Loop;
BEGIN
      IF NoKillFlag THEN
          RETURN;
      END;
      WHILE (lp <> UndefLoop) &
            (Arcs^[LoopList^[lp].BackEdge].LoopNo <> UndefLoop)
      DO
            l := ir.Nodes^[LoopList^[lp].Preheader].LoopNo;
            KillLoop (lp);
            lp := l;
      END;
END KillLoops;

(* -------------------------------------------------------------------------- *)

(*
  В таблице циклов найти свободный элемент
*)

PROCEDURE NewLoop (): Loop;
VAR lp, i: Loop;
     q: LoopRefs;
BEGIN
    lp := 0;
    LOOP
        IF lp >= NLoops THEN
            IF lp = LEN (LoopList^) THEN
                NEW (q, LEN (LoopList^) * 2);
                FOR i:=0 TO SYSTEM.PRED(lp) DO
                    q^[i] := LoopList^[i];
                END;
                LoopList := q;
            END;
            INC (NLoops);
            EXIT;
        END;
        IF LoopList^[lp].Body = NIL THEN
            EXIT;
        END;
        INC (lp);
    END;
    LoopList^[lp].Body  := BitVect.New (ir.Nnodes, FALSE);
    LoopList^[lp].Exits := NIL;
    RETURN lp;
END NewLoop;

(* -------------------------------------------------------------------------- *)

(*
  По дуге (у которой t доминирует над f) создать новый цикл
*)

PROCEDURE MakeLoop (arc: Arc): Loop;
VAR Stack : NodeArray;
    SP, i : INT;
    lp    : Loop;
    nd    : Node;

    PROCEDURE Insert (nd: Node);
    BEGIN
        IF NOT BitVect_In (LoopList^[lp].Body, ORD(nd)) THEN
            BitVect_Incl (LoopList^[lp].Body, ORD(nd));
            Stack^[SP] := nd;
            INC (SP);
        END;
    END Insert;

BEGIN
    lp := NewLoop ();
    Arcs^[arc].LoopNo := lp;
    LoopList^[lp].BackEdge := arc;
    NEW (Stack, ir.Nnodes);
    SP := 0;
    BitVect_Incl (LoopList^[lp].Body, ORD(Arcs^[arc].t));
    Insert (Arcs^[arc].f);
    WHILE SP <> 0 DO
        DEC (SP);
        nd := Stack^[SP];
        FOR i:=0 TO ir.Nodes^[nd].NIn-1 DO
            Insert (ir.Nodes^[nd].In^[i]);
        END;
    END;
    RETURN lp;
END MakeLoop;

(* -------------------------------------------------------------------------- *)

(*
  После создания нового узла подправить таблицу циклов
*)

PROCEDURE UpdateLoops (n: Node);
VAR lp: Loop;
BEGIN
    FOR lp:=0 TO SYSTEM.PRED(NLoops) DO
        IF LoopList^[lp].Body <> NIL THEN
            BitVect.Realloc (LoopList^[lp].Body,  ir.Nnodes);
            BitVect.Excl    (LoopList^[lp].Body,  ORD(n));
        END;
        IF LoopList^[lp].Exits <> NIL THEN
            BitVect.Realloc (LoopList^[lp].Exits, ir.Nnodes);
            BitVect.Excl    (LoopList^[lp].Exits, ORD(n));
        END;
    END;
END UpdateLoops;

(* -------------------------------------------------------------------------- *)

(*
  После создания preheader'а поправить fi-функции
*)

PROCEDURE CorrectFi (head, pre: Node; a: INT);
VAR q, t: TriadePtr;
    i, k: INT;
       r: ir.ParamArray;
BEGIN
    q := ir.Nodes^[head].First;
    WHILE q^.Op = ir.o_fi DO
        r := ir.NewParams     (2, q);
        t := ir.NewTriadeLike (q, LEN (q^.Params^) - 1);
        PutTriadeFirst (t, pre);
        ir.GenVar (ir.Vars^[q^.Name].LocalNo, t^.Name, t);
        k := 0;
        FOR i:=0 TO LEN(q^.Params^)-1 DO
            IF i = a THEN
                ir.CopyParam (q^.Params^[i], r^[0]);
            ELSE
                ir.CopyParam (q^.Params^[i], t^.Params^[k]);
                INC (k);
            END;
            IF q^.Params^[i].tag = ir.y_Variable THEN
                ir.RemoveUse(q^.Params^[i]);
            END;
        END;
        ir.MakeParVar (r^[1], t^.Name);
        q^.Params := r;
        q := q^.Next;
    END;
    q := ir.Nodes^[head].First;
    WHILE q^.Op = ir.o_fi DO
        q := TestDeleteFi (q);
    END;
    q := ir.Nodes^[pre].First;
    WHILE q^.Op = ir.o_fi DO
        q := TestDeleteFi (q);
    END;
END CorrectFi;

(* -------------------------------------------------------------------------- *)

(*
  Создать циклу preheader
*)

PROCEDURE FindNextPosition*(p:TriadePtr):ir.TPOS;
BEGIN
    WHILE p#NIL DO
        IF NOT p^.Position.IsNull () THEN
            RETURN p^.Position;
        END;
        p := p^.Next;
    END;
    RETURN ir.NullPos;
END FindNextPosition;

PROCEDURE MakePreheader (lp: Loop);
VAR pre, head, n : Node;
    back, a      : Arc;
    i, j, Narc   : INT;
    k            : Loop;
BEGIN
    back := LoopList^[lp].BackEdge;
    head := Arcs^[back].t;
    IF (ir.Nodes^[head].NIn = 2) THEN                      (* Maybe there is
                                                           preheader ? *)
        IF ir.Nodes^[head].InArcs^[0] <> back THEN
            n := ir.Nodes^[head].In^[0];
            IF ir.Nodes^[n].NOut = 1 THEN
                LoopList^[lp].Preheader := n;
                RETURN;
            END;
        ELSE (* ir.Nodes^[head].InArcs^[1] <> back *)
            n := ir.Nodes^[head].In^[1];
            IF ir.Nodes^[n].NOut = 1 THEN
                LoopList^[lp].Preheader := n;
                RETURN;
            END;
        END;
    END;

    pre := NewNode ();                                  (* No, create one *)
    LoopList^[lp].Preheader := pre;
                                                        (* Update dominators *)

    ir.Nodes^[pre].Dominators := BitVect.New (ir.Nnodes, FALSE);
    FOR n:=0 TO SYSTEM.PRED(ir.Nnodes) DO
        IF ir.Nodes^[n].Alive THEN
            BitVect.Realloc (ir.Nodes^[n].Dominators, ir.Nnodes);
            BitVect.Excl    (ir.Nodes^[n].Dominators, ORD(pre));
        END;
    END;
    BitVect.Move (ir.Nodes^[head].Dominators, ir.Nodes^[pre].Dominators);
    BitVect.Excl (ir.Nodes^[pre].Dominators, ORD(head));
    BitVect_Incl (ir.Nodes^[pre].Dominators, ORD(pre));
    IsDominators  := TRUE;
    IsPDominators := FALSE;
    FOR n:=Node{ 1 } TO SYSTEM.PRED(ir.Nnodes) DO
        IF ir.Nodes^[n].Alive & Dominates (head, n) THEN
            BitVect_Incl (ir.Nodes^[n].Dominators, ORD(pre));
        END;
    END;

    FOR k:=0 TO NLoops-1 DO                          (* Update loops *)
        IF NodeInLoop (head, k) & (k <> lp) THEN
            BitVect_Incl (LoopList^[k].Body, ORD(pre));
        END;
    END;

    Narc := -1;
    FOR i:=0 TO ir.Nodes^[head].NIn-1 DO           (* Retarget incoming into
                                                   head arcs to pre *)
        a := ir.Nodes^[head].InArcs^[i];
        IF (a = back) THEN
            Narc := i;
        ELSE
            j := FindOutArc (a);
            Arcs^[a].t := pre;
            AddNodeArc (ir.Nodes^[pre].InArcs, a,
                        ir.Nodes^[pre].In, Arcs^[a].f, ir.Nodes^[pre].NIn);
            ir.Nodes^[Arcs^[a].f].Out^[j] := pre;
        END;
    END;

    ir.Nodes^[head].NIn        := 1;                       (* Make incoming
                                                           into head arcs  *)
    ir.Nodes^[head].InArcs^[0] := back;
    ir.Nodes^[head].In^[0]     := Arcs^[back].f;
    NoKillFlag := TRUE;
    NewArc (pre, head, FALSE);
    NoKillFlag := FALSE;
    IsDominators  := TRUE;
    IsPDominators := FALSE;

    MakeGoto (pre);
--    ir.Nodes[pre].Last.Position := FindNextPosition(ir.Nodes[head].First);
    CorrectFi (head, pre, Narc);
END MakePreheader;

(* -------------------------------------------------------------------------- *)

(*
  Построить множество выходных узлов цикла
*)

PROCEDURE MakeExits (lp: Loop);
VAR    i: INT;
       o: TSNode;
       n: Node;
BEGIN
    IF LoopList^[lp].Exits <> NIL THEN
        RETURN;
    END;
    LoopList^[lp].Exits := BitVect.New (ir.Nnodes, FALSE);
    FOR o := SYSTEM.SUCC(ir.Nodes^[LoopList^[lp].Preheader].TopNumber) TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^ [o];
        IF NodeInLoop (n, lp) THEN
            i := 0;
            LOOP
                IF NOT NodeInLoop (ir.Nodes^[n].Out^[i], lp) THEN
                    BitVect_Incl (LoopList^[lp].Exits, ORD(n));
                    EXIT;
                END;
                INC (i);
                IF i >= ir.Nodes^[n].NOut THEN
                    EXIT;
                END;
            END;
        END;
    END;
END MakeExits;

(* -------------------------------------------------------------------------- *)

(*
  Для всех узлов посчитать их вложенность в циклы
*)

PROCEDURE CalcNesting;
VAR o: TSNode;
    n: Node;
    j: Loop;
BEGIN
    FOR o:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        ir.Nodes^[ir.Order^[o]].Nesting := 0;
    END;
    FOR j:=0 TO NLoops-1 DO
        FOR o:=SYSTEM.SUCC(ir.Nodes^[LoopList^[j].Preheader].TopNumber) TO SYSTEM.PRED(LEN(ir.Order^)) DO
            n := ir.Order^[o];
            IF NodeInLoop (n, j) THEN
                INC (ir.Nodes^[n].Nesting);
            END;
        END;
    END;
END CalcNesting;

(* -------------------------------------------------------------------------- *)

(*
  Отсортировать таблицу циклов так, чтобы более вложенные были в ее начале
*)

PROCEDURE SortLoops;
VAR i, j, ind: Loop;
    t:         LoopType;
BEGIN
    FOR i:=0 TO SYSTEM.PRED(NLoops,2) DO
        ind := i;
        FOR j:=SYSTEM.SUCC(i) TO NLoops-1 DO
            IF (LoopList^[i].Body = NIL) THEN
               IF (LoopList^[j].Body <> NIL) THEN
                  ind := j;
               END;
            ELSIF (LoopList^[j].Body <> NIL) &
                  (ir.Nodes^[LoopList^[ j ].Preheader].Nesting >
                   ir.Nodes^[LoopList^[ind].Preheader].Nesting
                  )
            THEN
                ind := j;
            END;
        END;
        IF ind <> i THEN
            t              := LoopList^[i];
            LoopList^[i]   := LoopList^[ind];
            LoopList^[ind] := t;
            Arcs^[LoopList^[i].  BackEdge].LoopNo := i;
            Arcs^[LoopList^[ind].BackEdge].LoopNo := ind;
        END;
    END;
    i := NLoops-1;
    WHILE (i >= 0) & (LoopList^[i].Body = NIL) DO
          DEC (i);
    END;
    NLoops := i+1;
END SortLoops;

(* -------------------------------------------------------------------------- *)

(*
  В каждый узел написать номер самого вложенного содержащего его цикла
*)

PROCEDURE MakeLoopNo;
VAR o:  TSNode;
    i:  Node;
    lp: Loop;
BEGIN
    FOR o:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        ir.Nodes^[ir.Order^[o]].LoopNo := UndefLoop;
    END;
    IF NLoops > 0 THEN
        FOR o:=SYSTEM.SUCC(ir.StartOrder) TO SYSTEM.PRED(LEN(ir.Order^)) DO
            i := ir.Order^[o];
            lp := 0;
            LOOP
                IF NodeInLoop (i, lp) THEN
                    ir.Nodes^[i].LoopNo := lp;
                    EXIT;
                END;
                INC (lp);
                IF lp = NLoops THEN
                    EXIT;
                END;
            END;
        END;
    END;
END MakeLoopNo;

(* -------------------------------------------------------------------------- *)

(*
  Построить цикловую структуру программы
*)

PROCEDURE FindLoops*;
VAR i: Arc;
    l: Loop;
    n: Node;
BEGIN
    FindDominators;
    IF LoopsOk THEN
       RETURN;
    END;
    FOR n := 0 TO ir.Nnodes-1 DO
      ir.Nodes[n].IsPreheaderOf := UndefLoop;
    END;
    i := 0;
    WHILE i < Narcs DO
        IF (Arcs^[i].t <> 0) &
           Dominates (Arcs^[i].t, Arcs^[i].f) &
           (Arcs^[i].LoopNo = UndefLoop)
        THEN
            MakePreheader (MakeLoop (i));
            i := 0;
        ELSE
            INC (i);
        END;
    END;
    TopSort;
    CalcNesting;
    SortLoops;
    FOR l:=0 TO NLoops-1 DO
        MakeExits (l);
        ir.Nodes[LoopList[l].Preheader].IsPreheaderOf := l;
    END;
    MakeLoopNo;
    LoopsOk := TRUE;
END FindLoops;

(* ----------------------- Kill unreachable code ---------------------------- *)
(* -------------------------------------------------------------------------- *)

(*
  Уничтожить узел
*)
PROCEDURE KillNode* (n: Node; gr_changed: BOOLEAN; doAssert:=FALSE: BOOLEAN);
VAR i: INT;
    a: Arc;
    l: Loop;
BEGIN
    IF ir.Nodes^[n].Alive THEN
        l := 0;
        LOOP
           IF l >= NLoops THEN
              EXIT;
           END;
           IF (LoopList^[l].Body <> NIL) & (n = LoopList^[l].Preheader) THEN
              KillLoop (l);
              EXIT;
           END;
           INC (l);
        END;
        KillLoop    (ir.Nodes^[n].LoopNo);
        KillTriades (ir.Nodes^[n].First, doAssert);
        FOR i:=0 TO ir.Nodes^[n].NOut-1 DO
            KillInArc (ir.Nodes^[n].OutArcs^[i]);
        END;
        FOR i:=0 TO ir.Nodes^[n].NIn-1 DO
            a := ir.Nodes^[n].InArcs^[i];
            KillOutArc (a);
            FreeArc    (a);
        END;
        FOR i:=0 TO ir.Nodes^[n].NOut-1 DO
            FreeArc (ir.Nodes^[n].OutArcs^[i]);
        END;
        ir.Nodes^[n].NIn        := 0;
        ir.Nodes^[n].NOut       := 0;
        ir.Nodes^[n].Alive      := FALSE;
        ir.Nodes^[n].Dominators := NIL;
        IF HasStop & (n = Stop) THEN
            HasStop := FALSE;
        END;
    END;
    IF gr_changed THEN GraphChanged(); END;
END KillNode;

(*
  Уничтожить явно недостижимые узлы
*)

PROCEDURE KillDead*(doAssert:=FALSE: BOOLEAN);
VAR i: Node;
BEGIN
    IsOrder := FALSE;
    TopSort;
    FOR i := Node{ 1 } TO SYSTEM.PRED(ir.Nnodes) DO
        IF NOT ir.Nodes^[i].Processed & ir.Nodes^[i].Alive THEN
            KillNode (i, TRUE, doAssert);
        END;
    END;
END KillDead;

(* --------------------- check triade representation for--------------------- *)
(* ----------------------errors in type correspondence ---------------------- *)

PROCEDURE CheckNodesAndArcs;
VAR i, j, nin, nout: LONGINT;
    m, n: Node;
    a : Arc;
BEGIN
  TopSort();
  FOR i := ir.StartOrder TO LEN(ir.Order^)-1 DO
    n := ir.Order[i];
    nin  := ir.Nodes[n].NIn;
    nout := ir.Nodes[n].NOut;
    IF (LEN(ir.Nodes[n].In^)     < nin) OR
       (LEN(ir.Nodes[n].InArcs^) < nin) OR
       (LEN(ir.Nodes[n].Out^)     < nout) OR
       (LEN(ir.Nodes[n].OutArcs^) < nout) 
    THEN
      env.info.print("\nWrong ir.Nodes[%d]\n", n);
      ASSERT(FALSE);
    END;
    FOR j := 0 TO nin-1 DO
      a := ir.Nodes[n].InArcs[j];
      IF Arcs[a].t # n THEN
        env.info.print("\nWrong ir.Nodes[%d] or ir.Arcs[%d]\n", n, a);
        ASSERT(FALSE);
      END;
      m := ir.Nodes[n].In[j];
      IF ir.Nodes[m].Out[ FindOutArc(a) ] # n THEN
        env.info.print("\nWrong connection bv nodes %d, %d and arc %d\n", n, m, a);
        ASSERT(FALSE);
      END;  
    END;
    FOR j := 0 TO nout-1 DO
      a := ir.Nodes[n].OutArcs[j];
      IF Arcs[a].f # n THEN
        env.info.print("\nWrong ir.Nodes[%d] or ir.Arcs[%d]\n", n, a);
        ASSERT(FALSE);
      END;
      m := ir.Nodes[n].Out[j];
      IF ir.Nodes[m].In[ FindInArc(a) ] # n THEN
        env.info.print("\nWrong connection bv nodes %d, %d and arc %d\n", n, m, a);
        ASSERT(FALSE);
      END;
    END;
  END;
END CheckNodesAndArcs;

<* IF MODE="WORK" THEN *>
PROCEDURE GetTriadeCoord*(tr: TriadePtr): INT;
VAR i: INT;
BEGIN
    i := -1;
    WHILE tr # NIL DO
        tr := tr.Prev;
        INC(i);
    END;
    RETURN i;
END GetTriadeCoord;

<* END *>

PROCEDURE CheckCorrectness*();
BEGIN
    CheckNodesAndArcs;
END CheckCorrectness;

(*----------------------------------------------------------------------------*)
(*----------------------------------------------------------------------------*)

(*
  Создать уникальный "финишный" узел
*)

PROCEDURE MakeStop*;
VAR n: Node;
    i: TSNode;
    v: BitVect.BitVector;
BEGIN
    IF NOT HasStop THEN
        Stop    := NewNode ();
        IsOrder := FALSE;
        HasStop := TRUE;
    END;
    FOR n:=0 TO SYSTEM.PRED(ir.Nnodes) DO
        IF (n <> Stop) & ir.Nodes^[n].Alive & (ir.Nodes^[n].NOut = 0) THEN
            NewArc (n, Stop, FALSE);
        END;
    END;
    TopSort;
    v := BitVect.New (ir.Nnodes, FALSE);
    SearchBack (Stop, v);
    FOR i:=SYSTEM.PRED(LEN(ir.Order^)) TO ir.StartOrder BY -1 DO
        n := ir.Order^[i];
        IF (n <> Stop) AND NOT BitVect_In (v, ORD(n)) THEN
            NewArc     (n, Stop, FALSE);
            SearchBack (n, v);
        END;
    END;
    BitVect.Free (v);
END MakeStop;

PROCEDURE KillStop*;
BEGIN
    IF HasStop THEN
        KillNode (Stop, TRUE);
    END;
END KillStop;

(* moves from "from" to "to" (inclusive) before "before" *)
PROCEDURE MoveTriadesBefore*(from, to, before: ir.TriadePtr);
VAR a: ir.TriadePtr;
BEGIN
  IF from = NIL THEN RETURN; END;
  REPEAT
    a := from.Next;
    DeleteTriade(from);
    InsertTriade(from, before); -- put from before "before"
    IF from = to THEN RETURN; END;
    from := a;
  UNTIL from = NIL;
END MoveTriadesBefore;

(* moves from "from" to "to" (inclusive) to the start of "node" *)
PROCEDURE MoveTriadesToStart(from, to: ir.TriadePtr; node: ir.Node);
VAR a: ir.TriadePtr;
    is_first: BOOLEAN;
    before: ir.TriadePtr;
BEGIN
  IF from = NIL THEN RETURN; END;
  is_first := TRUE;
  before   := NIL;
  REPEAT
    a := from.Next;
    DeleteTriade(from);
    IF is_first THEN
      PutTriadeFirst(from, node);
      is_first := FALSE;
    ELSE
      PutAfterTriade(from, before);
    END;
    IF from = to THEN RETURN; END;
    before := from;
    from := a;
  UNTIL from = NIL;
END MoveTriadesToStart;

-- makes copy of the parameter
-- asserting that the triade creates no var
PROCEDURE CopyTriade*(p: TriadePtr): TriadePtr;
VAR q: TriadePtr;
    i: LONGINT;
BEGIN
  ASSERT(p.Tag = ir.y_Nothing);
  q := ir.NewTriadeLike (p, LEN (p^.Params^));
  FOR i:=0 TO LEN (p^.Params^)-1 DO
    ir.CopyParamWithRev (p^.Params^[i], q^.Params^[i]);
  END;
  RETURN q;
END CopyTriade;

(*
  Скопировать диапазон триад - от первой (включительно) до второй
  (исключительно), заменяя все вхождения локала l на имя v
*)

PROCEDURE CopyTriades*(p, last, ins: ir.TriadePtr;
                       l: ir.Local; v: ir.VarNum);
VAR q: TriadePtr;
    i, j: INT;
    vars, vars1: POINTER TO ARRAY OF RECORD s, d: VarNum END;
    u: VarNum;
    N: INT;
BEGIN
    NEW(vars, 100);
    N := 0;
    WHILE p <> last DO
        IF p^.Params = NIL THEN
            q  := ir.NewTriadeLike (p, 0);
        ELSE
            q  := ir.NewTriadeLike (p, LEN (p^.Params^));
            FOR i:=0 TO LEN (p^.Params^)-1 DO
                IF p^.Params^[i].tag = ir.y_Variable THEN
                    u := p^.Params^[i].name;
                    j := 0;
                    LOOP
                        IF j = N THEN
                            EXIT;
                        ELSIF vars[j].s = u THEN
                            u := vars[j].d;
                            EXIT;
                        END;
                        INC (j);
                    END;
                    ir.MakeParVar_Ex (q^.Params^[i], u, TRUE, p^.Params^[i].reverse);
                ELSE
                    ir.CopyParamWithRev (p^.Params^[i], q^.Params^[i]);
                    IF (q^.Params^[i].tag  = ir.y_RealVar) &
                       (q^.Params^[i].name = l)
                    THEN
                       ir.MakeParVar_Ex (q^.Params^[i], v, TRUE, q^.Params^[i].reverse);
                    END;
                END;
            END;
        END;
        IF p^.Tag = ir.y_Variable THEN
            ir.GenVar (ir.TEMPORARY, q^.Name, q);
            IF N >= LEN(vars^) THEN
              NEW(vars1, LEN(vars^)*2);
              FOR i := 0 TO N-1 DO
                vars1[i] := vars[i];
              END;
               vars := vars1;
            END;
            vars[N].s := p^.Name;
            vars[N].d := q^.Name;
            INC (N);
        ELSE
            q^.Name := p^.Name;
        END;
        INCL (q^.Options, ir.o_Silent);
        InsertTriade (q, ins);
        p := p^.Next;
    END;
END CopyTriades;

-- returns the second (new-created) part of the former node
PROCEDURE SplitNodeAfter*(p: ir.TriadePtr; make_goto: BOOLEAN): ir.Node;
VAR current, cont: ir.Node;
BEGIN
  current := p.NodeNo;
  cont := NewNode();
  RetargetAllOutgoingArcs(current, cont);
  MoveTriadesToStart(p.Next, ir.Nodes[current].Last, cont);
  IF make_goto THEN
    MakeGoto(current);
    NewArc(current, cont, TRUE);
  END;
  RETURN cont;
END SplitNodeAfter;

PROCEDURE MakeNodeSilent*(n: ir.Node);
VAR tr: ir.TriadePtr;
BEGIN
  tr := ir.Nodes[n].First;
  WHILE tr # NIL DO
    INCL(tr.Options,ir.o_Silent);
    tr := tr.Next;
  END;
END MakeNodeSilent;


PROCEDURE Init*;
BEGIN
  currNode := ir.UndefNode;
END Init;

(*
  "Опустошить" граф
*)

PROCEDURE NewGraph*;
BEGIN
<* IF MODE = "WORK" THEN *>
    NEW (ir.Nodes, 200);
    NEW (Arcs,  200);
    NEW (LoopList, 50);
<* ELSE *>
    NEW (ir.Nodes, 10);
    NEW (Arcs,  10);
    NEW (LoopList, 5);
<* END *>
    ir.Order   := NIL;
    ir.Nnodes  := 0;
    Narcs   := 0;
    NLoops  := 0;
    GraphChanged;
    LoopsOk := FALSE;
    HasStop := FALSE;

    ir.SetNormalMode;
END NewGraph;

BEGIN
    NoKillFlag := FALSE;
    ir.SetNormalMode;
    dfArray := NIL;
    dfSet   := NIL;
    dfLen   := NIL;
END ControlGraph.