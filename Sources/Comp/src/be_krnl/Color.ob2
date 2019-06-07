<* -procinline *>
<* inline_maxweight="200" *>
<* -werr300 *>
MODULE Color;

(*
  Недоделки:
  - когда заводим новый локал - 1) align, 2) Lo, Hi
  - в DoAllocation: при обновлении Profit выгода всегда 1
  - при обработке fi-функций в CalcProfit: выгода всегда равна 1
  - Traverse перетащить бы в ir
*)

IMPORT ir,
       gr := ControlGraph,
       BitVect,
       Calc,
       opTune,
       Optimize,
       at := opAttrs,
       prc := opProcs,
       pc := pcK;
IMPORT xiEnv;
<* IF ~ nodebug THEN *>
IMPORT opIO;
IMPORT opAttrs;
<* END *>
IMPORT SYSTEM;
(* ---------------------------- Various types ------------------------------- *)

TYPE
        INT         = ir.INT;
        Local       = ir.Local;
        VarNum      = ir.VarNum;
        VarNumArray = ir.VarNumArray;
        Node        = ir.Node;
        TSNode      = ir.TSNode;
        NodeArray   = ir.NodeArray;
        Arc         = ir.Arc;
        ParamPtr    = ir.ParamPtr;
        ParamArray  = ir.ParamArray;
        TriadePtr   = ir.TriadePtr;
        SizeType    = ir.SizeType;
        Loop        = ir.Loop;
        BitVector   = BitVect.BitVector;
        BitMatrix   = ir.BitMatrix;

        GenOrdNode*= ir.Node;
(* -------------------------------------------------------------------------- *)

VAR
        NLocals*       : Local;
        NNonTempVars*  : VarNum;
        HookedVars*    : ir.BitMatrixVar;
        LiveAtTop*     : ir.BitMatrixNode;
        LiveAtBottom*  : ir.BitMatrixNode;
        LiveAtTopArray*: POINTER TO ARRAY OF POINTER TO ARRAY OF ir.VarNum;

<* +procinline *>
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
(*                                                                            *)
(*                              Sort uselists                                 *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

(*
  Отсортировать использования переменных - все использования
  внутри одного узла будут в списке use подряд
*)

PROCEDURE SortUses;
VAR j: ir.VarNum;
    k: INT;
    i: TSNode;
    p:    TriadePtr;
BEGIN
    FOR j:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
        ir.Vars^[j].Use := NIL;
    END;
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        p := ir.Nodes^[ir.Order^[i]].First;
        REPEAT
            IF p^.Params <> NIL THEN
                FOR k:=0 TO LEN(p^.Params^)-1 DO
                    IF p^.Params^[k].tag = ir.y_Variable THEN
                        ir.AddUse (p^.Params^[k]);
                    END;
                END;
            END;
            p := p^.Next;
        UNTIL p = NIL;
    END;
END SortUses;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                              Sort variables                                *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

(*
  Должны ли мы выделить память под переменную или это может сделать селектор?
*)

PROCEDURE NonTemporary (v: VarNum): BOOLEAN;
VAR p: TriadePtr;
    q: ParamPtr;
BEGIN
    IF at.nooptimize IN at.COMP_MODE THEN
        IF ir.Vars [v].LocalNo <> ir.TEMPORARY THEN
            RETURN TRUE;
        END;
    END;
    p := ir.Vars^[v].Def;
    IF (p^.Op = ir.o_fi)
--
    OR (p^.Op = ir.o_load)
    OR (p^.Op = ir.o_getpar)
    OR (p^.Op = ir.o_loadr) & (p^.Params^[0].tag = ir.y_AddrConst)
--
    THEN
        RETURN TRUE;
    END;
    q := ir.Vars^[v].Use;
    WHILE q <> NIL DO
        IF (q^.triade^.Op = ir.o_fi)
--
           OR (q^.triade^.Op = ir.o_store)&(ir.popt_LastUse IN q.options)
           OR (q^.triade^.Op = ir.o_storer) &
              (q^.triade^.Params^[0].tag = ir.y_AddrConst)
--
        THEN
            RETURN TRUE;
        END;
        q := q^.next;
    END;
    RETURN FALSE;
END NonTemporary;

(* -------------------------------------------------------------------------- *)

(*
  Отсортировать переменные - те, под которые мы должны
  выделить память, переместить в начала массива переменных
*)

PROCEDURE SortVars;
VAR v: VarNum;
BEGIN
    NNonTempVars := ir.ZEROVarNum;
    FOR v:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
        IF NonTemporary (v) THEN
            IF v <> NNonTempVars THEN
                ir.SwapVars (v, NNonTempVars);
            END;
            INC (NNonTempVars);
        END;
    END;
END SortVars;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                    Calculate liveness of variables                         *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

(*
  Посчитать живость переменных в конце линейного участка
*)

PROCEDURE EnterNode (n: Node; bv: BitVector);
VAR i, k: INT;
       p: TriadePtr;
BEGIN
    IF ir.Nodes^[n].NOut = 0 THEN
        BitVect.Fill (bv, FALSE, ir.NVars);
    ELSE
        BitVect.Move (LiveAtTop^[ir.Nodes^[n].Out^[0]], bv);
        FOR i:=1 TO ir.Nodes^[n].NOut-1 DO
            BitVect.Union (bv, LiveAtTop^[ir.Nodes^[n].Out^[i]], bv);
        END;
        FOR i:=0 TO ir.Nodes^[n].NOut-1 DO
            p := ir.Nodes^[ir.Nodes^[n].Out^[i]].First;
            WHILE p^.Op = ir.o_fi DO
                BitVect.Excl (bv, ORD(p^.Name));
                p := p^.Next;
            END;
        END;
        FOR i:=0 TO ir.Nodes^[n].NOut-1 DO
            p := ir.Nodes^[ir.Nodes^[n].Out^[i]].First;
            k := gr.FindInArc (ir.Nodes^[n].OutArcs^[i]);

            WHILE p^.Op = ir.o_fi DO
                p := p^.Next;
            END;

            p := p^.Prev;
            WHILE p # NIL DO
                IF p^.Params^[k].tag = ir.y_Variable THEN
                    IF ~ BitVect_In(bv, ORD(p^.Params^[k].name)) THEN
                      INCL(p^.Params^[k].options, ir.popt_LastUse)
                    END;
                    BitVect_Incl (bv, ORD(p^.Params^[k].name));
                END;
                p := p^.Prev;
            END;
        END;
    END;
END EnterNode;

(* -------------------------------------------------------------------------- *)

(*
  Обновить живость переменных при проходе снизу вверх одной триады
*)

PROCEDURE ProcessTriade (p: TriadePtr; bv: BitVector);
VAR i: INT;
BEGIN
    IF p^.Tag = ir.y_Variable THEN
        BitVect.Excl (bv, ORD(p^.Name));
    END;
    IF p^.Params <> NIL THEN
        FOR i:=0 TO LEN(p^.Params^)-1 DO
            IF p^.Params^[i].tag  = ir.y_Variable THEN
                IF ~ BitVect_In(bv, ORD(p^.Params^[i].name)) THEN
                  INCL(p^.Params^[i].options, ir.popt_LastUse)
                END;
                BitVect_Incl (bv, ORD(p^.Params^[i].name));
            END;
        END;
    END;
END ProcessTriade;

(* -------------------------------------------------------------------------- *)

(*
  Посчитать живость переменных - обычный итеративный алгоритм:
  Обходим граф сзаду наперед, пока не сойдется:
    - в конце узла живы все, кто жив в начале одного из его потомков
    - потом идем сзаду наперед по триадам этого узла:
        - встретили use - значит переменная жива,
        - встретили def - значит переменная померла
  (при этом аргументы fi-функций учитываются в соответствующем
   предшественнике данного узла)
*)

PROCEDURE CalcLiveness (bv: BitVector);
VAR i:       TSNode;
    p:       TriadePtr;
    n:       Node;
    Changed: BOOLEAN;
    v: VarNum;
    count: VarNum;
BEGIN
    NEW (LiveAtTopArray,    ir.Nnodes);
    NEW (LiveAtTop,    ir.Nnodes);
    NEW (LiveAtBottom, ir.Nnodes);
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        LiveAtTop^   [n] := BitVect.New (ir.NVars, FALSE);
        LiveAtBottom^[n] := BitVect.New (ir.NVars, FALSE);
    END;
    REPEAT
        Changed := FALSE;
        FOR i:=SYSTEM.PRED(LEN(ir.Order^)) TO ir.StartOrder BY -1 DO
            n := ir.Order^[i];
            EnterNode (n, bv);
            Changed := BitVect.UnionAssign (LiveAtBottom^[n], bv) OR Changed;
            p := ir.Nodes^[n].Last;
            REPEAT
                ProcessTriade (p, bv);
                p := p^.Prev;
            UNTIL (p = NIL) OR (p^.Op = ir.o_fi);
            Changed := BitVect.UnionAssign (LiveAtTop^[n], bv) OR Changed;
        END;
    UNTIL NOT Changed;

    FOR i:=SYSTEM.PRED(LEN(ir.Order^)) TO ir.StartOrder BY -1 DO
      n := ir.Order^[i];
      count := 0;
      FOR v := ir.ZEROVarNum TO ir.NVars-1 DO
        IF BitVect_In(LiveAtTop[n],v) THEN
          INC(count);
        END;
      END;
      IF count = 0 THEN
        LiveAtTopArray[n]:= NIL
      ELSE

        NEW(LiveAtTopArray[n],count);
        FOR v := ir.ZEROVarNum TO ir.NVars-1 DO
          IF BitVect_In(LiveAtTop[n],v) THEN
            DEC(count);
            LiveAtTopArray[n][count] := v;
          END;
        END;
      END;
    END;
END CalcLiveness;

(* -------------------------------------------------------------------------- *)

(*
  Подправить живость переменных, если переменная v стала использоваться в узле n
  (не в fi-функции!)
*)

<* IF TARGET_RISC THEN *>
PROCEDURE FindLocInFMUL;

    PROCEDURE UsedInAddSub1Only(v : ir.VarNum) : BOOLEAN;
    VAR q: ir.ParamPtr; p : ir.TriadePtr;
    BEGIN
        q := ir.Vars^[v].Use;
        WHILE q <> NIL DO
            p := q^.triade;
            CASE p^.Op OF
            | ir.o_sub:
                IF q.paramnumber <> 0 THEN RETURN FALSE; END;
            | ir.o_add:
            ELSE
                RETURN FALSE;
            END;
            IF (p.Params[0].name = p.Params[1].name) OR
               -- is it p.Op(arg1, arg2)  and  arg1 = arg2
               (ir.o_LocInFMUL IN ir.Vars[(q.paramnumber+1) MOD 2].Options)
               -- will be p.Op(fmul, fmul)
            THEN
                RETURN FALSE;
            END;
            q := q^.next;
        END;
        RETURN TRUE;
    END UsedInAddSub1Only;

VAR v : ir.VarNum;
    p : ir.TriadePtr;
BEGIN
    FOR v := ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
       p := ir.Vars[v].Def;
       IF (p <> NIL) & (p.ResType = ir.t_float) & (p.Op = ir.o_mul) &
          UsedInAddSub1Only(v)
       THEN
           INCL(ir.Vars^[v].Options, ir.o_LocInFMUL);
       END;
    END;
END FindLocInFMUL;
<* END *>
(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                        Find hooked variables                               *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

PROCEDURE CalcHooked;
VAR i, j, k: VarNum;
    t: TSNode;
    n:    Node;
    bv:   BitVector;
    p:    TriadePtr;
BEGIN
    IF ir.NVars <> ir.ZEROVarNum THEN
        bv := BitVect.New (ir.NVars, FALSE);
        CalcLiveness (bv);
        NEW (HookedVars, ir.NVars);
        FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
            HookedVars^[i] := BitVect.New (ir.NVars, FALSE);
        END;
        FOR t:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
            n := ir.Order^[t];
            EnterNode (n, bv);
            p := ir.Nodes^[n].Last;
            REPEAT
                IF p^.Tag = ir.y_Variable THEN
                    BitVect.Union (bv, HookedVars^[p^.Name], HookedVars^[p^.Name]);
                    FOR k := ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                       IF BitVect_In(bv, k) THEN
                        BitVect_Incl(HookedVars^[k],p^.Name);
                      END;
                    END;
                END;
---------------
                IF p^.Op = ir.o_call THEN
                    FOR j:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                        IF BitVect_In(bv, ORD(j)) &
                           ((p^.Tag <> ir.y_Variable) OR (p^.Name <> j))
                        THEN
                            INCL (ir.Vars^[j].Options, ir.o_LiveAtCall);
                        END;
                    END;
                ELSIF p^.Op = ir.o_copy THEN
                    FOR j:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                        IF BitVect_In(bv, ORD(j)) THEN
                            INCL (ir.Vars^[j].Options, ir.o_LiveAtCopy);
                        END;
                    END;
                ELSIF ((p^.Op = ir.o_eq) OR (p^.Op = ir.o_le)) &
                      (p^.ResType = ir.t_float)
                THEN
                    FOR j:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                        IF BitVect_In(bv, ORD(j)) THEN
                            INCL (ir.Vars^[j].Options, ir.o_LiveAtFCom);
                        END;
                    END;
                ELSIF (p^.Op = ir.o_mul) & (p^.ResType = ir.t_unsign) &
                      (p^.Options * ir.OptionsSet{ ir.o_Checked } <> ir.OptionsSet{}) OR
                      (p^.Op = ir.o_div) OR (p^.Op = ir.o_dvd) OR
                      (p^.Op = ir.o_mod) OR (p^.Op = ir.o_rem) OR
                      (p^.Op = ir.o_mulh)
                THEN
                    FOR j:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                        IF BitVect_In(bv, ORD(j)) THEN
                            INCL (ir.Vars^[j].Options, ir.o_LiveAtMulDiv);
                        END;
                    END;
                END;
-----------------------
                ProcessTriade (p, bv);
                IF p^.Op = ir.o_cmpswap THEN
                    FOR j:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                        IF BitVect_In(bv, ORD(j)) THEN
                            INCL (ir.Vars^[j].Options, ir.o_LiveAtCmpSwap);
                        END;
                    END;
                END;
                p := p^.Prev;
            UNTIL (p = NIL) OR (p^.Op = ir.o_fi);
            WHILE p <> NIL DO
                BitVect.Union (bv, HookedVars^[p^.Name], HookedVars^[p^.Name]);
                FOR k := ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                  IF BitVect_In(bv,k) THEN
                    BitVect_Incl(HookedVars^[k],p^.Name);
                  END;
                END;
                p := p^.Prev;
            END;
        END;
        BitVect.Free (bv);
    END;
<* IF TARGET_RISC THEN *>
    FindLocInFMUL;
<* END *>
(* Symmetric Check
    FOR k:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
      FOR k2:=ir.ZEROVarNum TO SYSTEM.PRED(k) DO
        ASSERT( BitVect_In (HookedVars^[k], k2) = BitVect_In (HookedVars^[k2], k));
      END;
    END;
*)
END CalcHooked;

(* -------------------------------------------------------------------------- *)

(*
  Зацеплены ли две переменные?
*)

PROCEDURE AreHooked* (v1, v2: VarNum): BOOLEAN;
BEGIN
    RETURN BitVect_In(HookedVars^[v1], ORD(v2));--'cos nowit is symmetric!   OR BitVect_In (HookedVars^[v2], ORD(v1));
END AreHooked;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                     Распределение памяти - описания                        *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

TYPE  CostType  = INT;

CONST
   INVCost    = CostType{-1};
   ZEROCost   = CostType{ 0};
   UNITYCost  = CostType{ 1};
   MaxProfit = 4;

(*
  Собственно таблица выгод; для каждой переменной хранятся те локалы, с
  которыми ее выгодно совместить (но не более чем MaxProfit таковых).
*)

VAR Profits*: POINTER TO ARRAY OF RECORD
                                    N: INT;
                                    local: ARRAY MaxProfit OF RECORD
                                                            local:  Local;
                                                            offset: LONGINT;
                                                            length: SizeType;
                                                            cost:   CostType;
                                                          END;
                                 END;
(*
  Собственно таблица размещения переменных
*)

    Allocation*: POINTER TO ARRAY OF RECORD
                                         Location *: Local;
                                         Offset   *: LONGINT;
                                         Cluster  *: INTEGER;
                                     END;
(*
  Таблица кластеров (т.е. переменных с одинаковым размещением)
*)

TYPE
    ClusterNum *= INT;
    Cluster = RECORD
                  N *: INTEGER;
                  v *: VarNumArray;
                  aaa *: SHORTINT;     -- Место размещения-
                                     -- заполняется не
                                     -- здесь
               END;

VAR NClusters* : ClusterNum;
    Clusters*  : POINTER TO ARRAY OF Cluster;

(*
  Специальные примочки для более быстрого обхода графа
*)

VAR Traverse: POINTER TO ARRAY OF INT;
    CurrentTraverse: INT;

(* -------------------------------------------------------------------------- *)
<* IF ~ nodebug THEN *>

PROCEDURE PrintLocal (l: ir.Local);
BEGIN
    IF ir.Locals[l].Name <> NIL THEN
        opIO.print ("%s", ir.Locals[l].Name^);
    ELSE
        opIO.print ("tmp%d", l);
    END;
END PrintLocal;

(* -------------------------------------------------------------------------- *)

PROCEDURE PrintVar (r: ir.VarNum);
BEGIN
    IF ir.Vars^[r].LocalNo = ir.TEMPORARY THEN
        opIO.print ("t%d",r);
    ELSE
        PrintLocal (ir.Vars^[r].LocalNo);
        opIO.print ("_%d",r);
    END;
END PrintVar;

PROCEDURE PrintProfits;
VAR v: VarNum;
    i: INT;
BEGIN
    opIO.print("\nCluster profits for %s\n", opAttrs.curr_proc.name^);
    FOR v:=ir.ZEROVarNum TO SYSTEM.PRED(NNonTempVars) DO
      IF Allocation^[v].Location = ir.UNDEFINED THEN
        PrintVar (v);
        opIO.print ("\t- ");
        FOR i:=0 TO Profits^[v].N-1 DO
            PrintLocal (Profits^[v].local[i].local);
            IF Profits^[v].local[i].offset # 0 THEN
              opIO.print ("+%d", Profits^[v].local[i].offset);
            END;
            opIO.print (": %d; ", Profits^[v].local[i].cost);
        END;
        opIO.print ("\n");
      END;
    END;
END PrintProfits;

<* END *>
(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                     Построить таблицу кластеров                            *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FindCluster (i: ir.VarNum): INT;
VAR j: ClusterNum;
    k: INT;
    u:    VarNumArray;
BEGIN
    FOR j:=0 TO NClusters-1 DO
        IF (Allocation^[i].Location = Allocation^[Clusters^[j].v^[0]].Location)
         & (Allocation^[i].Offset   = Allocation^[Clusters^[j].v^[0]].Offset)
        THEN
            IF Clusters^[j].N = LEN (Clusters^[j].v^) THEN
                NEW (u, Clusters^[j].N + 4);
                FOR k:=0 TO Clusters^[j].N-1 DO
                    u^[k] := Clusters^[j].v^[k];
                END;
                Clusters^[j].v := u;
            END;
            Clusters^[j].v^[Clusters^[j].N] := i;
            INC (Clusters^[j].N);
            RETURN j;
        END;
    END;
    NEW (Clusters^[NClusters].v, 4);
    Clusters^[NClusters].v^[0] := i;
    Clusters^[NClusters].N     := 1;
    INC (NClusters);
    RETURN NClusters - 1;
END FindCluster;

(* -------------------------------------------------------------------------- *)

(*
  1. Посчитать кластера
  2. Про-or-ить Options у всех переменных в кластере
  3. Про-or-ить зацепленности у всех переменных в кластере
*)

<* PUSH *>
<*-CHECKNIL    *>
<*-CHECKDIV    *>
<*-CHECKPROC   *>
<*-CHECKSET    *>
<*-CHECKINDEX  *>
<*-CHECKDINDEX  *>
<*-CHECKRANGE  *>
<*-CHECKNIL    *>
PROCEDURE CalcClusters;
VAR i: VarNum;
    c: ClusterNum;
    j: INT;
    v: VarNum;
    o:    ir.VarOptionsSet;

    PROCEDURE RecalcHookedWithCluster (vars :VarNumArray; N: INTEGER; j: VarNum);
    VAR i, k: INT;
        v: ir.VarNum;
    BEGIN
        FOR i:=0 TO N-1 DO
            v := vars^[i];
            IF BitVect_In(HookedVars^[v], ORD(j))
--              oR BitVect_In (HookedVars^[j], ORD(v)) --WE'RE SYMMETRIC!!!
            THEN
                FOR k:=0 TO N-1 DO
                    v := vars^[k];
                    BitVect_Incl (HookedVars^[j], ORD(v));
                    BitVect_Incl (HookedVars^[v], ORD(j));
                END;
                RETURN;
            END;
        END;
    END RecalcHookedWithCluster;

BEGIN
    NEW (Clusters, NNonTempVars);
    FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(NNonTempVars) DO
        Allocation^[i].Cluster := VAL (INTEGER, FindCluster (i));
    END;
    FOR c:=0 TO NClusters-1 DO
        o := ir.VarOptionsSet{};
        FOR j:=0 TO Clusters^[c].N-1 DO
            o := o + ir.Vars^[Clusters^[c].v^[j]].Options;
        END;
        FOR j:=0 TO Clusters^[c].N-1 DO
            ir.Vars^[Clusters^[c].v^[j]].Options := o;
        END;
          FOR v:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
              RecalcHookedWithCluster (Clusters^[c].v,Clusters^[c].N, v);
          END;
    END;
END CalcClusters;
<*POP*>

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                     Распределение памяти - процедуры                       *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

(*
  Увеличить выгоду от размещения переменной в конкретном локале;
  в переменную ProfitNo положить номер варианта в profits^[v] (или -1)
*)

VAR ProfitNo: INT;

PROCEDURE IncProfit (v: VarNum; l: Local; o: LONGINT; s: SizeType; c: CostType);
VAR i: INT;
BEGIN
    FOR i:=0 TO Profits^[v].N-1 DO
        IF (Profits^[v].local[i].local  = l) &
           (Profits^[v].local[i].offset = o) &
           (Profits^[v].local[i].length = s)
        THEN
            ProfitNo := i;
            INC (Profits^[v].local[i].cost, c);
            RETURN;
        END;
    END;
    IF Profits^[v].N = MaxProfit THEN
        ProfitNo := -1;
        RETURN;
    END;
    i := Profits^[v].N;
    ProfitNo := i;
    INC (Profits^[v].N);
    Profits^[v].local[i].local  := l;
    Profits^[v].local[i].offset := o;
    Profits^[v].local[i].length := s;
    Profits^[v].local[i].cost   := c;
END IncProfit;

(* -------------------------------------------------------------------------- *)

(*
  Выделить память под таблицу выгод, пройти по графу и заполнить
  ее начальными значениями (без учета fi-функций); выгода есть от
  совмещения результата load, loadr с локалом, откуда читается, а
  также от совмещения параметра store, storer с локалом, куда пишется.
  Учитывается вложенность циклов.
  Кроме того, инициализировать массив, используемый потом для обхода графа.
*)

PROCEDURE InitProfits;
VAR i: VarNum;
    n: Node;
    t: TSNode;
    p: TriadePtr;
    c: CostType;
BEGIN
    NEW (Profits,    NNonTempVars);
    NEW (Allocation, NNonTempVars);
    FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(NNonTempVars) DO
        Profits^   [i].N        := 0;
        Allocation^[i].Location := ir.UNDEFINED;
    END;
    FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(NNonTempVars) DO
        IF ir.Vars^[i].LocalNo <> ir.TEMPORARY THEN
            IncProfit (i, ir.Vars^[i].LocalNo, 0,
                       ir.Vars^[i].Def^.ResSize, ZEROCost);
        END;
    END;
    FOR t:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[t];
        c := VAL(CostType,ir.Nodes^[n].Nesting + 1);
        p := ir.Nodes^[n].First;
        REPEAT
            CASE p^.Op OF
            | ir.o_getpar:
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
                IF (p^.Name < NNonTempVars) &
                   (prc.WhereParam(prc.ProcList[at.curr_procno].proto_no,
                                       p^.NPar)
                    = prc.STACK)
                            THEN
                                IncProfit (p^.Name,
                                           prc.ParamLoc (p^.NPar), 0,
                                           p^.ResSize, c);
                            END;
<* ELSE *>
                            IF p^.Name < NNonTempVars THEN
                                IncProfit (p^.Name,
                                           prc.ParamLoc (p^.NPar), 0,
                                           p^.ResSize, c);
                            END;
<* END *>
            | ir.o_load:    IF p^.Name < NNonTempVars THEN
                                IncProfit (p^.Name, p^.Params^[0].name, 0,
                                           p^.ResSize, c);
                            END;
            | ir.o_store:   IF (p^.Params^[0].tag = ir.y_Variable) &
                               (p^.Params^[0].name < NNonTempVars)
                            THEN
                                IncProfit (p^.Params^[0].name, p^.Name, 0,
                                           p^.ResSize, c);
                            END;
            | ir.o_loadr:   IF (p^.Name < NNonTempVars)             &
                               (p^.Params^[0].tag = ir.y_AddrConst) &
                               (p^.Params^[0].name <> MAX (Local))
                            THEN
                                IncProfit (p^.Name, p^.Params^[0].name,
                                           p^.Params^[0].offset, p^.ResSize, c);
                            END;
            | ir.o_storer:  IF (p^.Params^[1].tag = ir.y_Variable)       &
                               (p^.Params^[1].name < NNonTempVars) &
                               (p^.Params^[0].tag = ir.y_AddrConst)      &
                               (p^.Params^[0].name <> MAX (Local))
                            THEN
                                IncProfit (p^.Params^[1].name,
                                           p^.Params^[0].name,
                                           p^.Params^[0].offset,
                                           p^.ResSize, c);
                            END;
            ELSE
            END;
            p := p^.Next;
        UNTIL p = NIL;
    END;
    CurrentTraverse := 0;
END InitProfits;

(* -------------------------------------------------------------------------- *)

(*
    l IN p^.Write; check special cases - does it write in l + o (s bytes)?
*)

PROCEDURE Writes (p: TriadePtr; l: Local; o: LONGINT;
                  s: SizeType; v: VarNum): BOOLEAN;
BEGIN
    CASE p^.Op OF
    | ir.o_store:   IF (p^.Name = l) & (o = 0) & (s = p^.ResSize) &
                       (p^.Params^[0].tag  = ir.y_Variable)       &
                       (p^.Params^[0].name = v)
                    THEN
                        RETURN FALSE;
                    END;
    | ir.o_storer:  IF (p^.Params^[0].tag  = ir.y_AddrConst) &
                       (p^.Params^[0].name = l)
                    THEN
                        RETURN NOT ((o + ORD(s) <= p^.Params^[0].offset) OR
                                    (p^.Params^[0].offset + ORD(p^.ResSize) <= o))
                               & ((p^.Params^[1].tag  <> ir.y_Variable) OR
                                  (p^.Params^[1].name <> v)             OR
                                  (p^.Params^[0].offset <> o)           OR
                                  (p^.ResSize <> s));
                    END;

    | ir.o_cmpswap: (* quite identical to o_storer, but 2nd argument instead of 1st one *)
                    IF (p^.Params^[0].tag  = ir.y_AddrConst) &
                       (p^.Params^[0].name = l)
                    THEN
                        RETURN NOT ((o + ORD(s) <= p^.Params^[0].offset) OR
                                    (p^.Params^[0].offset + ORD(p^.ResSize) <= o))
                               & ((p^.Params^[2].tag  <> ir.y_Variable) OR
                                  (p^.Params^[2].name <> v)             OR
                                  (p^.Params^[0].offset <> o)           OR
                                  (p^.ResSize <> s));
                    END;

    | ir.o_copy:    IF (p^.Params^[1].tag  = ir.y_AddrConst) &
                       (p^.Params^[1].name = l)              &
                       ((o + ORD(s) <= p^.Params^[1].offset) OR
                        (p^.Params^[2].tag = ir.y_NumConst) &
                        (p^.Params^[1].offset +
                         Calc.ToInteger (p^.Params^[2].value,
                                         opTune.addr_sz) <= o))
                    THEN
                        RETURN FALSE;
                    END;
    ELSE
    END;
    RETURN TRUE;
END Writes;

(* -------------------------------------------------------------------------- *)

CONST CHUNK_SIZE = 128;

VAR Chunk:       ARRAY CHUNK_SIZE OF VarNum;
    NChunk:      INT;
    Vect:        BitVector;
    NToAllocate: INT;

(*
  Возможно ли разместить очередную переменную в данном локале,
  начиная с данного смещения?
  Надо проверить:
  - Она не зацеплена с переменными, уже размещенными в данном локале,
  - Во все время жизни переменной в этот локал никто не пишет
    (точнее говоря, в него пишут только эту переменную),
  - Если переменная родилась не путем загрузки из этого локала
    (т.е. не в load, loadr или в fi-функции, у которой все параметры
    уже размещены в этом локале), то до конца графа (или до первой
    записи в этот локал) из этого локала никто не читает.
  - Если из локала читают, а переменная родилась в Ф-функции, то пытаемся
    разместить ее параметры там же.
*)

PROCEDURE CanAllocate (n: INT; l: Local; o: LONGINT; s: SizeType): BOOLEAN;
VAR u, v: VarNum;
    i: INT;
    p: TriadePtr;

    (*
      Are u and v in the same memory location?
    *)

    PROCEDURE SameLocation (u: VarNum): BOOLEAN;
    BEGIN
        RETURN BitVect_In(Vect, ORD(u)) OR (Allocation^[u].Location  = l) &
                                       (Allocation^[u].Offset    = o) &
                                       (ir.Vars^[u].Def^.ResSize = s);
    END SameLocation;

    (*
      Return last triade in node that use v
    *)

    PROCEDURE LastUse (n: Node): TriadePtr;
    VAR r: ParamPtr;
    BEGIN
        IF BitVect_In (LiveAtBottom^[n], ORD(v)) THEN
            RETURN NIL;
        ELSE
            r := ir.Vars^[v].Use;
            WHILE r^.triade^.NodeNo <> n DO
                r := r^.next;
            END;
            RETURN r^.triade;
        END;
    END LastUse;

    (*
      Обход графа с проверкой, не пишет ли кто-нибудь в l - один шаг
    *)

    PROCEDURE WritesStep (p: TriadePtr; n: Node): BOOLEAN;
    VAR i: INT;
        m: Node;
        q: TriadePtr;
    BEGIN
        IF p <> NIL THEN
            WHILE p^.Op = ir.o_fi DO
                p := p^.Next;
            END;
        END;
        q := LastUse (n);
        WHILE p <> q DO
            IF (p^.Write <> NIL) &
               BitVect_In (p^.Write, ORD(l)) &
               Writes (p, l, o, s, v)
            THEN
                RETURN TRUE;
            END;
            p := p^.Next;
        END;
        IF q = NIL THEN
            FOR i:=0 TO ir.Nodes^[n].NOut-1 DO
                m := ir.Nodes^[n].Out^[i];
                IF (Traverse^[m] <> CurrentTraverse) &
                   BitVect_In (LiveAtTop^[m], ORD(v))
                THEN
                    Traverse^[m] := CurrentTraverse;
                    IF WritesStep (ir.Nodes^[m].First, m) THEN
                        RETURN TRUE;
                    END;
                END;
            END;
        END;
        RETURN FALSE;
    END WritesStep;

    (*
      l IN p^.Read; check special cases - does it read in l + o (s bytes)?
    *)

    PROCEDURE Reads (p: TriadePtr): BOOLEAN;
    BEGIN
        CASE p^.Op OF
        | ir.o_loadr:   IF (p^.Params^[0].tag  = ir.y_AddrConst) &
                           (p^.Params^[0].name = l)
                        THEN
                            RETURN NOT ((o + ORD(s) <= p^.Params^[0].offset) OR
                                        (p^.Params^[0].offset + ORD(p^.ResSize) <= o));
                        END;

        | ir.o_cmpswap: (* quite identical to o_loadr *)
                        IF (p^.Params^[0].tag  = ir.y_AddrConst) &
                           (p^.Params^[0].name = l)
                        THEN
                            RETURN NOT ((o + ORD(s) <= p^.Params^[0].offset) OR
                                        (p^.Params^[0].offset + ORD(p^.ResSize) <= o));
                        END;

        | ir.o_copy:    IF (p^.Params^[0].tag  = ir.y_AddrConst) &
                           (p^.Params^[0].name = l)              &
                           ((o + ORD(s) <= p^.Params^[0].offset) OR
                            (p^.Params^[2].tag = ir.y_NumConst) &
                            (p^.Params^[0].offset +
                             Calc.ToInteger (p^.Params^[2].value,
                                             opTune.addr_sz) <= o))
                        THEN
                            RETURN FALSE;
                        END;
        ELSE
        END;
        RETURN TRUE;
    END Reads;

    (*
      graph traverse - one step: can anybody read from l?
    *)

    PROCEDURE ReadsStep (p: TriadePtr; n: Node): BOOLEAN;
    VAR i: INT;
        m: Node;
    BEGIN
        WHILE p <> NIL DO
            IF (p^.Write <> NIL) & (p^.ResSize = s) THEN
                IF p^.Op = ir.o_store THEN
                    IF (p^.Name = l) & (o = 0) THEN
                        RETURN FALSE;
                    END;
                ELSIF p^.Op = ir.o_storer THEN
                    IF (p^.Params^[0].tag    = ir.y_AddrConst) &
                       (p^.Params^[0].name   = l)              &
                       (p^.Params^[0].offset = o)
                    THEN
                        RETURN FALSE;
                    END;
                END;
            END;
            IF (p^.Read <> NIL) &
               BitVect_In(p^.Read, ORD(l)) & Reads (p) THEN
                RETURN TRUE;
            END;
            IF (p^.Tag = ir.y_Variable) & (p^.Name < NNonTempVars) &
               (p^.Op <> ir.o_fi) & SameLocation (p^.Name)
            THEN
                RETURN FALSE;
            END;
            p := p^.Next;
        END;
        FOR i:=0 TO ir.Nodes^[n].NOut-1 DO
            m := ir.Nodes^[n].Out^[i];
            IF Traverse^[m] <> CurrentTraverse THEN
                Traverse^[m] := CurrentTraverse;
                IF ReadsStep (ir.Nodes^[m].First, m) THEN
                    RETURN TRUE;
                END;
            END;
        END;
        RETURN FALSE;
    END ReadsStep;

    (*
      Is p^.Name initially in the same location we want it allocate to?
    *)

    PROCEDURE LoadedFromSameMemory (p: TriadePtr): BOOLEAN;
    VAR i: INT;
        u: VarNum;
    BEGIN
        IF p^.ResSize <> s THEN
            RETURN FALSE;
        END;
        CASE p^.Op OF
        | ir.o_getpar:
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
                        RETURN (prc.WhereParam(prc.ProcList[at.curr_procno].proto_no,
                                               p^.NPar)
                                = prc.STACK) &
                               (prc.ParamLoc (p^.NPar) = l) & (o = 0);
<* ELSE *>
                        RETURN (prc.ParamLoc (p^.NPar) = l) & (o = 0);
<* END *>
        | ir.o_load:    RETURN (p^.Params^[0].name = l) & (o = 0);
        | ir.o_loadr:   RETURN (p^.Params^[0].tag = ir.y_AddrConst) &
                               (p^.Params^[0].name = l) &
                               (p^.Params^[0].offset = o);
        | ir.o_fi:      FOR i:=0 TO LEN (p^.Params^)-1 DO
                            IF (p^.Params^[i].tag <> ir.y_Variable) THEN
                                RETURN FALSE;
                            END;
                            u := p^.Params^[i].name;
                            IF NOT BitVect_In(Vect, ORD(u)) &
                               (Allocation^[u].Location <> ir.UNDEFINED) &
                               ((Allocation^[u].Location <> l) OR
                                (Allocation^[u].Offset    <> o) OR
                                (ir.Vars^[u].Def^.ResSize <> s))
                            THEN
                                RETURN FALSE;
                            END;
                        END;
                        FOR i:=0 TO LEN (p^.Params^)-1 DO
                            u := p^.Params^[i].name;
                            IF NOT BitVect_In(Vect, ORD(u)) &
                               (Allocation^[u].Location = ir.UNDEFINED)
                            THEN
                                  IF NChunk = CHUNK_SIZE THEN
                                      RETURN FALSE;
                                  END;
  <* IF ~nodebug THEN *>
                                  IF opIO.needed THEN
                                    opIO.printLabel();
                                    opIO.print ("Add to chunk ");
                                    ir.PrintVar(u);
                                    opIO.print ("\n");
                                  END;
  <* END *>
                                  Chunk [NChunk] := u;
                                  INC (NChunk);
                                  BitVect_Incl (Vect, ORD(u));
                                END;
                        END;
                        RETURN TRUE;
        | ELSE
            RETURN FALSE;
        END;
    END LoadedFromSameMemory;

(*
  Собственно процедура определения возможности размещения переменной в локале
*)
VAR scope : ir.ScopeType;
<* IF ~nodebug THEN *>
  VAR index: INT;
<* END *>
BEGIN
    v := Chunk [n];
<* IF ~nodebug THEN *>
    IF opIO.needed THEN
      opIO.print ("==== Color.CanAllocate ");
      ir.PrintVar(v);
      opIO.print (" to ");
      ir.PrintLocal(l);
      opIO.print (" + %d sz=%d.", o, s);
      IF NChunk > 1 THEN
      opIO.print (" Chunk rest:");
      FOR index := 1 TO NChunk-1 DO
          opIO.print(" ");
          ir.PrintVar(Chunk[index]);
        END;
      END;
      opIO.print("\n");
    END;
<* END *>
    FOR u:=ir.ZEROVarNum TO SYSTEM.PRED(NNonTempVars) DO
        IF (Allocation^[u].Location = l) &
           ( BitVect_In (HookedVars^[v], ORD(u)) OR
             BitVect_In (HookedVars^[u], ORD(v))) &
           NOT ((Allocation^[u].Offset + ORD(ir.Vars^[u].Def^.ResSize) <= o) OR
                (o + ORD(s) <= Allocation^[u].Offset))
        THEN
            RETURN FALSE;
        END;
      <* IF TARGET_RISC THEN *>
        IF ( (ir.Vars^[u].Def^.ResType = ir.t_float) &
             (ir.Vars^[v].Def^.ResType IN ir.WholeTypes)
           ) OR
           ( (ir.Vars^[v].Def^.ResType = ir.t_float) &
             (ir.Vars^[u].Def^.ResType IN ir.WholeTypes)
           )
        THEN -- RISC has different set of register for integer and float arithmetic
            RETURN FALSE;
        END;
      <* END *>
    END;
    FOR i:=0 TO n-1 DO
        u := Chunk [i];
        IF BitVect_In(HookedVars^[v], ORD(u)) OR
           BitVect_In(HookedVars^[u], ORD(v)) THEN
            RETURN FALSE;
        END;
    END;
    IF l >= NLocals THEN
<* IF ~nodebug THEN *>
      IF opIO.needed THEN
        opIO.print ("Yes - New local\n");
      END;
<* END *>
      RETURN TRUE;
    END;
--fixme        RETURN FALSE;
    p := ir.Vars^[v].Def;
    INC (CurrentTraverse);
    IF (ir.Vars^[v].Use <> NIL) & WritesStep (p^.Next, p^.NodeNo) THEN
        RETURN FALSE;
    END;
    IF ir.Vars[v].LocalNo = ir.TEMPORARY THEN
      scope := ir.TmpScope;
    ELSE
      scope := ir.Locals[ir.Vars[v].LocalNo].Scope;
    END;
    IF (scope = ir.Locals[l].Scope)&
        LoadedFromSameMemory (p) THEN
<* IF ~nodebug THEN *>
      IF opIO.needed THEN
        opIO.print ("Yes - Loaded from same memory\n");
      END;
<* END *>
        RETURN TRUE;
    END;
    INC (CurrentTraverse);
    IF ReadsStep (p^.Next, p^.NodeNo) THEN
      RETURN FALSE;
    END;
    IF scope # ir.Locals[l].Scope THEN
      RETURN FALSE;
    END;
    RETURN TRUE;
END CanAllocate;

(* -------------------------------------------------------------------------- *)

(*
  Разместить одну переменную; при этом могут измениться выгоды у других
*)

PROCEDURE DoAllocation (v: VarNum; l: Local; o: LONGINT; s: SizeType);
VAR p: TriadePtr;
    r: ParamPtr;

    PROCEDURE TryIncProfit (b, e: INT);
    VAR i: INT;
    BEGIN
        FOR i:=b TO e DO
            IF (p^.Params^[i].tag = ir.y_Variable) &
                (Allocation^[p^.Params^[i].name].Location = ir.UNDEFINED) &
                NOT BitVect_In(Vect, ORD(p^.Params^[i].name))
            THEN
                IncProfit (p^.Params^[i].name, l, o, s, UNITYCost);
            END;
        END;
    END TryIncProfit;

BEGIN
<* IF ~ nodebug THEN *>
    IF opIO.needed THEN
        opIO.print("    Variable ");
        PrintVar (v);
        opIO.print(" allocated to ");
        PrintLocal (l);
        opIO.print(" + %d\n", o);
    END;
<* END *>
    Allocation^[v].Location := l;
    Allocation^[v].Offset   := o;
    DEC (NToAllocate);
    p := ir.Vars^[v].Def;
    CASE p^.Op OF
    | ir.o_fi:      TryIncProfit (0, LEN(p^.Params^)-1);
    | ir.o_move_eq,
      ir.o_move_le: TryIncProfit (2, 3);
    ELSE
    END;
    r := ir.Vars^[v].Use;
    WHILE r <> NIL DO
        p := r^.triade;
        IF ((p^.Op = ir.o_fi) OR
            ((p^.Op = ir.o_move_eq) OR (p^.Op = ir.o_move_le)) &
            (r^.paramnumber >= 2))
         & (Allocation^[p^.Name].Location = ir.UNDEFINED)
         & NOT BitVect_In(Vect, ORD(p^.Name))
        THEN
            IncProfit (p^.Name, l, o, s, UNITYCost);
        END;
        r := r^.next;
    END;
END DoAllocation;

(* -------------------------------------------------------------------------- *)

<* IF TARGET_RISC THEN *>
PROCEDURE AllocateForCtrs;
VAR v: ir.VarNum;
    l: ir.Local;
BEGIN
    l := ir.UNDEFINED;
    FOR v:=ir.ZEROVarNum TO SYSTEM.PRED(NNonTempVars) DO
        IF ir.o_ForCtr IN ir.Vars[v].Options THEN
            IF l = ir.UNDEFINED THEN
                l := ir.AddLocal (NIL, ir.TmpScope, 4);
                ir.Locals^[l].VarSize := 4;
                ir.Locals^[l].VarType := ir.t_int;
            END;
            Allocation^[v].Location := l;
            Allocation^[v].Offset   := 0;
            DEC (NToAllocate);
        END;
    END;
END AllocateForCtrs;
<* END *>

(* -------------------------------------------------------------------------- *)

(*
  Найти наилучший вариант и разместить переменные
*)

PROCEDURE AllocateVariable;
VAR u, v, w: VarNum;
    i, j, n: INT;
    o:       LONGINT;
    l:       Local;
    s:       SizeType;
    c:       CostType;
BEGIN
    w := ir.UNDEFINED;
    u := ir.UNDEFINED;
    j := 0;
    LOOP
        c := INVCost;
        FOR v:=ir.ZEROVarNum TO SYSTEM.PRED(NNonTempVars) DO
            IF Allocation^[v].Location = ir.UNDEFINED THEN
                w := v;
                FOR i:=0 TO Profits^[v].N-1 DO
                    IF Profits^[v].local[i].cost > c THEN
                        u := v;
                        j := i;
                        c := Profits^[v].local[i].cost;
                    END;
                END;
            END;
        END;
        IF c = INVCost THEN --fixme
(*
  Нельзя поместить в существующую ячейку - надо выделять новую
*)
            s := ir.Vars^[w].Def^.ResSize;
            l := ir.AddLocal (NIL, ir.TmpScope, s);
            ir.Locals^[l].VarSize := VAL (LONGINT, s);
            ir.Locals^[l].VarType := ir.Vars^[w].Def^.ResType;
            DoAllocation (w, l, 0, s);
            RETURN;
       END;              --fixme
(*
  Пытаемся поместить в существующую ячейку; может быть,
  придется посещать сразу несколько переменных
*)
        l := Profits^[u].local[j].local;
        o := Profits^[u].local[j].offset;
        s := Profits^[u].local[j].length;
        Chunk [0] := u;
        NChunk := 1;
        n := 0;
        BitVect.Fill (Vect, FALSE, NNonTempVars);
        BitVect_Incl (Vect, ORD(u));
        LOOP
            IF NOT CanAllocate (n, l, o, s) THEN
                Profits^[u].local[j].cost := MIN (CostType);
                EXIT;
            END;
            INC (n);
            IF n = NChunk THEN
                FOR n:=0 TO NChunk-1 DO
                    DoAllocation (Chunk [n], l, o, s);
                END;
                RETURN;
            END;
        END;
    END;
END AllocateVariable;

PROCEDURE AllocateDebug;
VAR v : VarNum;
BEGIN
    FOR v:=ir.ZEROVarNum TO SYSTEM.PRED(NNonTempVars) DO
        IF ir.Vars [v].LocalNo <> ir.TEMPORARY THEN
            Allocation^[v].Location := ir.Vars [v].LocalNo;
            Allocation^[v].Offset   := 0;
            DEC (NToAllocate);
        END;
    END;
END AllocateDebug;

<* IF TARGET_RISC THEN *>

(*
  Поймать шаблон
  L:    v = fi (..., u)
        ...
        u = v - 1
        if u = 0 then ... else goto L
*)

PROCEDURE IsCounter (v: VarNum): BOOLEAN;
VAR p, q: ir.TriadePtr;
    w:    ir.ParamPtr;

    (*
      Проверить, что w1 - сравнение с 0, w2 - параметр p, p - вход в цикл
    *)

    PROCEDURE CheckUse (w1, w2: ir.ParamPtr): BOOLEAN;
    VAR n:  ir.Node;
        lp: ir.Loop;
    BEGIN
        IF (w2^.triade <> p) OR
           (w1^.triade^.Op <> ir.o_eq) OR
           (w1^.triade^.Params^[1 - w1^.paramnumber].tag <> ir.y_NumConst) OR
           NOT Calc.IsZero (w1^.triade^.Params^[1 - w1^.paramnumber].value,
                            w1^.triade^.ResType, w1^.triade^.ResSize)
        THEN
            RETURN FALSE;
        END;
        n := ir.Nodes^[w1^.triade^.NodeNo].Out^[1];
        WHILE (ir.Nodes^[n].Last^.Op = ir.o_goto) & (n <> p.NodeNo) DO
(* was
        WHILE (ir.Nodes^[n].Last^.Op = ir.o_goto) DO
*)
            n := ir.Nodes^[n].Out^[0];
        END;
        lp := ir.Nodes^[n].LoopNo;
        RETURN (n = p^.NodeNo) & (lp <> ir.UndefLoop) &
               (ir.Nodes^[w1^.triade^.NodeNo].LoopNo = lp) &
               (ir.Nodes^[n].NIn = 2) &
               (ir.Nodes^[n].In^[1-w2^.paramnumber] =
               gr.LoopList^[lp].Preheader);
    END CheckUse;
BEGIN
    p := ir.Vars^[v].Def;
    IF p^.Op <> ir.o_fi THEN
        RETURN FALSE;
    END;
    w := ir.Vars^[v].Use;
    IF (w = NIL) OR (w^.next <> NIL) OR (w^.paramnumber <> 0) THEN
        RETURN FALSE;
    END;
    q := w^.triade;
    IF (q^.Op <> ir.o_sub) OR (q^.ResType = ir.t_float) OR
       (q^.Params^[1].tag <> ir.y_NumConst) OR
       NOT Calc.CompareWithInt (pc.sb_equ, q^.Params^[1].value, 1,
                                q^.ResType, q^.ResSize)
    THEN
        RETURN FALSE;
    END;
    w := ir.Vars^[q^.Name].Use;
    IF (w = NIL) OR (w^.next = NIL) OR (w^.next^.next <> NIL) OR
       NOT (CheckUse (w, w^.next) OR CheckUse (w^.next, w))
    THEN
        RETURN FALSE;
    END;
    RETURN TRUE;
END IsCounter;

PROCEDURE FindForCtrs;
VAR
  v1,v2 : ir.VarNum;
  nest1,
  nest2 : LONGINT;
  count : INTEGER;
BEGIN
  count := 0;
  FOR v1 := ir.ZEROVarNum TO SYSTEM.PRED(NNonTempVars) DO
    IF ((ir.VarOptionsSet{ir.o_LiveAtCall,ir.o_LiveAtCopy} * ir.Vars[v1].Options) = ir.VarOptionsSet{}) &
       IsCounter(v1) &
       ((ir.VarOptionsSet{ir.o_LiveAtCall,ir.o_LiveAtCopy} *
        ir.Vars[ir.Vars[v1].Use.triade.Name].Options) = ir.VarOptionsSet{})
    THEN
      INCL(ir.Vars[v1].Options,ir.o_ForCtr);
      INC(count);
    END;
  END;

  IF count > 1 THEN
    LOOP
      FOR v1 :=ir.ZEROVarNum TO SYSTEM.PRED(NNonTempVars) DO
        IF ir.o_ForCtr IN ir.Vars[v1].Options THEN
          FOR v2 :=ir.ZEROVarNum TO SYSTEM.PRED(NNonTempVars) DO
            IF (v2 # v1) & (ir.o_ForCtr IN ir.Vars[v2].Options) &
               BitVect_In (HookedVars[v1],ORD(v2))
            THEN
              nest1 := ir.Nodes[ir.Vars[v1].Def.NodeNo].Nesting;
              nest2 := ir.Nodes[ir.Vars[v2].Def.NodeNo].Nesting;
              IF nest1 <= nest2 THEN EXCL(ir.Vars[v1].Options,ir.o_ForCtr);
              ELSE                   EXCL(ir.Vars[v2].Options,ir.o_ForCtr);
              END;
              DEC(count);
              IF count = 1 THEN EXIT END;
            END;
          END;
        END;
      END;
      EXIT;
    END;
  END;

  IF count <> 0 THEN
    FOR v1 :=ir.ZEROVarNum TO SYSTEM.PRED(NNonTempVars) DO
      IF (ir.o_ForCtr IN ir.Vars[v1].Options) &
         (ir.Vars^[v1].Def^.Op = ir.o_fi)
      THEN
        INCL(ir.Vars[ir.Vars^[v1].Use^.triade^.Name].Options,ir.o_ForCtr);
      END;
    END;
  END;
END FindForCtrs;

<* END *>

(* -------------------------------------------------------------------------- *)

PROCEDURE ^ SplitVars;

(*
  Собственно главная процедура
*)
VAR WriteTest* : PROCEDURE (id: CHAR; N-: ARRAY OF CHAR);

PROCEDURE ColorVariables*;
VAR i: ir.TSNode;
  <* IF ~ nodebug THEN *>
    profits_proc: xiEnv.String;
  <* END *>
BEGIN
    NLocals   := ir.NLocals;
    NClusters := 0;
    gr.FindLoops;
    gr.TopSort;
    ir.Squeeze;
    SortVars;
    SortUses;
    NEW (Traverse, ir.Nnodes);
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        Traverse^[ir.Order^[i]] := 0;
    END;
    IF ir.NVars = ir.ZEROVarNum THEN
        RETURN;
    END;
    IF WriteTest # NIL THEN
      WriteTest("q","q");
    END;
    CalcHooked;
    IF NNonTempVars = ir.ZEROVarNum THEN
        RETURN;
    END;
    InitProfits;
    Vect := BitVect.New (NNonTempVars, FALSE);
    NToAllocate := NNonTempVars;
<* IF TARGET_RISC THEN *>
    FindForCtrs;
    AllocateForCtrs;
<* END *>
    IF at.nooptimize IN at.COMP_MODE THEN
        AllocateDebug;
    END;
<* IF ~ nodebug THEN *>
    xiEnv.config.Equation( "profits_proc", profits_proc );
    opIO.needed := xiEnv.config.Option("profits") OR
         ( (profits_proc # NIL) & (opAttrs.curr_proc # NIL)
            & (profits_proc^ = opAttrs.curr_proc.name^));
<* END *>
    WHILE NToAllocate <> 0 DO
<* IF ~ nodebug THEN *>
        IF opIO.needed THEN PrintProfits; opIO.printLabel; END;
<* END *>
        AllocateVariable;
    END;
    Profits := NIL;
    BitVect.Free (Vect);
    CalcClusters;
END ColorVariables;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                         Special liveness analysis                          *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

PROCEDURE CanWriteTo (p: TriadePtr; l: Local; o: LONGINT;
                      s: SizeType; v: VarNum): BOOLEAN;
BEGIN
    RETURN (p^.Write <> NIL) & (l < NLocals) & BitVect_In (p^.Write, ORD(l)) &
           Writes (p, l, o, s, v)
        OR
           (p^.Tag = ir.y_Variable) & (p^.Name < NNonTempVars) &
           (Allocation^[p^.Name].Location = l)                 &
           NOT ((Allocation^[p^.Name].Offset + ORD(p^.ResSize) <= o) OR
                (o + ORD(s) <= Allocation^[p^.Name].Offset));
END CanWriteTo;

(* -------------------------------------------------------------------------- *)

(*
  Дано: p - триада, доминирует над q (и над s тоже); nq - узел q
  Начиная с q, идти назад до p, проверяя, пишет ли кто-нибудь в ту
          память, откуда читает s
*)

PROCEDURE CheckLoadStep (p, q, s: TriadePtr; nq: Node): BOOLEAN;
VAR i: INT;
    m: Node;
BEGIN
(*
  Сначала проверить все триады в этом узле
*)
    WHILE q <> NIL DO
        IF q = p THEN
            RETURN TRUE;
        END;
        IF (q^.Write <> NIL) & (s^.Read <> NIL) & Optimize.RWDependence (s, q, s^.Read, q^.Write) THEN
            RETURN FALSE;
        END;
        q := q^.Prev;
    END;
(*
  Дошли до начала узла, не встретив p; надо проверять предшественников
*)
    FOR i := 0 TO ir.Nodes^[nq].NIn-1 DO
        m := ir.Nodes^[nq].In^[i];
        IF Traverse^[m] <> CurrentTraverse THEN
            Traverse^[m] := CurrentTraverse;
            IF NOT CheckLoadStep (p, ir.Nodes^[m].Last, s, m) THEN
                RETURN FALSE;
            END;
        END;
    END;
    RETURN TRUE;
END CheckLoadStep;

(* -------------------------------------------------------------------------- *)

(*
  Пишет ли кто-нибудь в память, откуда читает p, во всех использованиях v?
*)

PROCEDURE NobodyWrites* (p: TriadePtr; v: VarNum): BOOLEAN;
VAR q: ParamPtr;
BEGIN
--    ASSERT (v >= NNonTempVars);
    q := ir.Vars^[v].Use;
    WHILE q <> NIL DO
        INC (CurrentTraverse);
        IF NOT CheckLoadStep (p, q^.triade^.Prev, p, q^.triade^.NodeNo) THEN
            RETURN FALSE;
        END;
        q := q^.next;
    END;
    RETURN TRUE;
END NobodyWrites;

(* -------------------------------------------------------------------------- *)

(*
  Пишет ли кто-нибудь в память, в которой размещена v, на пути из p2 в p1?
*)

PROCEDURE RecalculateStep (p1, p: TriadePtr; n: Node; l: Local;
                           o: LONGINT; s: SizeType; v: VarNum): BOOLEAN;
VAR i: INT;
    m: Node;
BEGIN
    WHILE p <> NIL DO
        IF p = p1 THEN
            RETURN FALSE;
        END;
        IF CanWriteTo (p, l, o, s, v) THEN
            RETURN TRUE;
        END;
        p := p^.Prev;
    END;
    FOR i:=0 TO ir.Nodes^[n].NIn-1 DO
        m := ir.Nodes^[n].In^[i];
        IF Traverse^[m] <> CurrentTraverse THEN
            Traverse^[m] := CurrentTraverse;
            IF RecalculateStep (p1, ir.Nodes^[m].Last, m, l, o, s, v) THEN
                RETURN TRUE;
            END;
        END;
    END;
    RETURN FALSE;
END RecalculateStep;

(* -------------------------------------------------------------------------- *)

(*
  Жива ли переменная v1 во всех использованиях v2?
*)

PROCEDURE LiveAtAllUsages* (v1, v2: VarNum): BOOLEAN;
VAR d,
    p1,
    p2: TriadePtr;
    q:  ParamPtr;
BEGIN
--    ASSERT (v2 >= NNonTempVars);
    IF v1 < NNonTempVars THEN
        p1 := ir.Vars^[v1].Def;
        d  := ir.Vars^[v2].Def;
        q  := ir.Vars^[v2].Use;
        WHILE q <> NIL DO
            p2 := q^.triade;
            INC (CurrentTraverse);
            IF RecalculateStep (d, p2^.Prev, p2^.NodeNo,
                                Allocation^[v1].Location,
                                Allocation^[v1].Offset,
                                p1^.ResSize, v1)
            THEN
                RETURN FALSE;
            END;
            q := q^.next;
        END;
    END;
    RETURN TRUE;
END LiveAtAllUsages;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                      Sort graph after commands selection                   *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

TYPE HasCodeType = PROCEDURE (n: Node): BOOLEAN;

TYPE GenOrderArray = POINTER TO ARRAY OF Node;
VAR GenOrder*      : GenOrderArray;  -- StartGenOrder..EndGenOrder
    StartGenOrder* : GenOrdNode;
    EndGenOrder*   : GenOrdNode;

    CurrentOrder:   GenOrderArray;
    SiberiaOrder:   GenOrderArray;
    HasCode:        HasCodeType;
    Orders:         POINTER TO ARRAY OF
                        RECORD
                            Start:     GenOrdNode;
                            Order:     GenOrderArray;   -- Start..ir.Nnodes-1
                            GoodEntry: BOOLEAN;     -- FALSE, если вход в цикл
                                                    -- "через зад"
                        END;
    Num:            GenOrdNode;
    SiberiaNum:     GenOrdNode;

    TrapGenOrder-:  GenOrdNode;
    RetGenOrder-:   GenOrdNode;


(* -------------------------------------------------------------------------- *)

(*
  А куда на самом деле ведет дуга? (пропускаем пустые узлы)
*)

PROCEDURE To* (a: Arc; HasCode: HasCodeType): Node;
VAR n: Node;
BEGIN
    INC (CurrentTraverse);
    n := gr.Arcs^[a].t;
    LOOP
        IF HasCode (n) OR (Traverse^[n] = CurrentTraverse) THEN
            RETURN n;
        END;
        Traverse^[n] := CurrentTraverse;
        n := ir.Nodes^[n].Out^[0];
    END;
END To;

--------------------------------------------------------------------------------

(*
  Содержится ли линейный участок в цикле?
*)

PROCEDURE NodeInLoop (n: Node; lp: Loop): BOOLEAN;
BEGIN
    RETURN (lp = ir.UndefLoop) OR gr.NodeInLoop (n, lp);
END NodeInLoop;

--------------------------------------------------------------------------------

(*
  Является ли цикл, содержащий линейный участок, "плохим"
  (т.е. входим ли мы в него "через зад")
*)

PROCEDURE BadLoop (n: Node; l: Loop): BOOLEAN;
BEGIN
    RETURN (ir.Nodes^[n].LoopNo <> l) &
           NOT Orders^[ir.Nodes^[n].LoopNo].GoodEntry;
END BadLoop;

--------------------------------------------------------------------------------

PROCEDURE ^ Search (n: Node; l: Loop; toSiberia: BOOLEAN);

--------------------------------------------------------------------------------

(*
  Топологический обход графа со включением выходов из вложенного цикла
*)

PROCEDURE SearchInternal (m: Node; l, nlp: Loop; toSiberia: BOOLEAN);
VAR i: INT;
    p: Node;
BEGIN
    FOR i:=0 TO ir.Nodes^[m].NOut-1 DO
        p := To (ir.Nodes^[m].OutArcs^[i], HasCode);
        IF NOT NodeInLoop (p, nlp) & NodeInLoop (p, l) THEN
            Search (p, l, toSiberia);
        END;
    END;
END SearchInternal;

--------------------------------------------------------------------------------
(*
  Возвращает TRUE, если можем выбрать более приоритетную выходную дугу
*)

PROCEDURE canFindPreferableArc(n: Node): BOOLEAN;
BEGIN
  IF ir.Nodes^[n].NOut=2 THEN
      RETURN gr.Arcs[ir.Nodes^[n].OutArcs[0]].Priority <>
             gr.Arcs[ir.Nodes^[n].OutArcs[1]].Priority;
  ELSE
      RETURN FALSE;
  END;
END canFindPreferableArc;

--------------------------------------------------------------------------------

(*
  Топологический обход графа со включением уже обработанных циклов
  единым куском:
  - если узел прямо в нашем цикле, то включить его потомков, а затем и его
    (при этом сначала включаем "плохие" циклы - на них все равно надо
    делать GOTO, поэтому можно их разместить подальше, затем - все узлы,
    у которых 0 потомков, а затем уж всех остальных)
  - если узел во вложенном цикле, то возобновляем обход со всех выходов из
    этого вложенного цикла (при этом его последний узел обрабатываем в
    последнюю очередь)
*)

PROCEDURE Search (n: Node; l: Loop; toSiberia: BOOLEAN);
VAR m, p: Node;
    node : GenOrdNode;
    i:    INT;
    nlp:  Loop;
    O:    GenOrderArray;
    e:    BitVector;
    arc0, arc1: Arc;
BEGIN
    nlp := ir.Nodes^[n].LoopNo;
    IF nlp = l THEN
        IF NOT ir.Nodes^[n].Processed THEN
          ir.Nodes^[n].Processed := TRUE;
          IF canFindPreferableArc(n) THEN
              arc0 := ir.Nodes^[n].OutArcs[0];
              arc1 := ir.Nodes^[n].OutArcs[1];

              IF gr.Arcs[arc0].Priority < gr.Arcs[arc1].Priority THEN
                 Search(To(arc1, HasCode), l, toSiberia);
                 Search(To(arc0, HasCode), l, TRUE);
              ELSE
                 Search(To(arc0, HasCode), l, toSiberia);
                 Search(To(arc1, HasCode), l, TRUE);
              END;
          ELSE
            FOR i:=0 TO ir.Nodes^[n].NOut-1 DO
                m := To (ir.Nodes^[n].OutArcs^[i], HasCode);
                IF NodeInLoop (m, l) & BadLoop (m, l) THEN
                    Search (m, l, toSiberia);
                END;
            END;
            FOR i:=0 TO ir.Nodes^[n].NOut-1 DO
                m := To (ir.Nodes^[n].OutArcs^[i], HasCode);
                IF NodeInLoop (m, l) & NOT BadLoop (m, l) &
                   (ir.Nodes^[m].NOut = 0) &
                   (ir.Nodes^[m].Last^.Op <> ir.o_ret) &
                   (ir.Nodes^[m].Last^.Op <> ir.o_retfun)
                THEN
                    Search (m, l, toSiberia);
                END;
            END;
            FOR i:=0 TO ir.Nodes^[n].NOut-1 DO
                m := To (ir.Nodes^[n].OutArcs^[i], HasCode);
                IF NodeInLoop (m, l) & NOT BadLoop (m, l) &
                   (ir.Nodes^[m].NOut = 0) &
                   ((ir.Nodes^[m].Last^.Op = ir.o_ret) OR
                    (ir.Nodes^[m].Last^.Op = ir.o_retfun))
                THEN
                    Search (m, l, toSiberia);
                END;
            END;
            FOR i:=0 TO ir.Nodes^[n].NOut-1 DO
                m := To (ir.Nodes^[n].OutArcs^[i], HasCode);
                IF NodeInLoop (m, l) & NOT BadLoop (m, l) &
                   (ir.Nodes^[m].NOut <> 0)
                THEN
                    Search (m, l, toSiberia);
                END;
            END;
          END;
            IF NOT toSiberia THEN
                DEC (Num);
                CurrentOrder^[Num] := n;
            ELSE
                DEC (SiberiaNum);
                SiberiaOrder^[SiberiaNum] := n;
            END;
        END;
    ELSIF gr.LoopInLoop (nlp, l) THEN
        WHILE NOT gr.NodeInLoop (n, nlp) OR (Orders^[nlp].Order = NIL) DO
            INC (nlp);
            IF nlp = gr.NLoops THEN
                RETURN;
            END;
        END;
        O := Orders^[nlp].Order;
        Orders^[nlp].Order := NIL;
        p := O^[VAL(GenOrdNode, SYSTEM.PRED(ir.Nnodes))];
        e := gr.LoopList^[nlp].Exits;
        FOR m:=ir.ZERONode TO SYSTEM.PRED(ir.Nnodes) DO
            IF BitVect_In(e, ORD(m)) & (m <> p) THEN
                SearchInternal (m, l, nlp, TRUE);
            END;
        END;
        SearchInternal (p, l, nlp, toSiberia);
        FOR node:=VAL(GenOrdNode, SYSTEM.PRED(ir.Nnodes)) TO Orders^[nlp].Start BY -1 DO
            IF NOT toSiberia THEN
                DEC (Num);
                CurrentOrder^[Num] := O^[node];
            ELSE
                DEC (SiberiaNum);
                SiberiaOrder^[SiberiaNum] := O^[node];
            END;
        END;
    END;
END Search;

--------------------------------------------------------------------------------

(*
  Упорядочение одного цикла (с возможным формированием
                             входа в него "через зад")
*)

PROCEDURE SortOneLoop (l: Loop);
VAR tn: ir.TSNode;
    i,o,on : GenOrdNode;
    foo: INT;
    m, n: Node;
    e: BitVector;
BEGIN
    Num := VAL(GenOrdNode,ir.Nnodes);
    NEW (CurrentOrder, ir.Nnodes);
    e := gr.LoopList^[l].Exits;
    Search (To (ir.Nodes^[gr.LoopList^[l].Preheader].OutArcs^[0], HasCode), l, FALSE);
    tn := MAX (ir.TSNode);
    on := VAL(GenOrdNode,SYSTEM.PRED(ir.Nnodes));
    FOR o:=Num TO VAL(GenOrdNode,SYSTEM.PRED(ir.Nnodes)) DO
        n := CurrentOrder^[o];
        IF (ir.Nodes^[n].NOut = 2) & BitVect_In (e, ORD(n)) THEN
            foo := 0;
            IF gr.NodeInLoop (ir.Nodes^[n].Out^[0], l) THEN
                foo := 1;
            END;
            m := To (ir.Nodes^[n].OutArcs^[foo], HasCode);
            IF ir.Nodes^[m].TopNumber < tn THEN
                tn := ir.Nodes^[m].TopNumber;
                on := o;
            END;
        END;
    END;
    Orders^[l].Start := Num;
    Orders^[l].Order := CurrentOrder;
    IF on = VAL(GenOrdNode, SYSTEM.PRED(ir.Nnodes)) THEN
        Orders^[l].GoodEntry := TRUE;
    ELSE
        Orders^[l].GoodEntry := FALSE;
        FOR i:=SYSTEM.SUCC(on) TO VAL(GenOrdNode,SYSTEM.PRED(ir.Nnodes)) DO
            GenOrder^[i] := CurrentOrder^[i];
        END;
        FOR i:=on TO Num BY -1 DO
            CurrentOrder^[VAL(GenOrdNode,SYSTEM.PRED(ir.Nnodes))-on+i] := CurrentOrder^[i];
        END;
        FOR i:=SYSTEM.SUCC(on) TO VAL(GenOrdNode,SYSTEM.PRED(ir.Nnodes)) DO
            CurrentOrder^[SYSTEM.PRED(Num)-on+i] := GenOrder^[i];
        END;
    END;
END SortOneLoop;

--------------------------------------------------------------------------------

(*
  Собственно укладка графа
*)

PROCEDURE GenSort* (h: HasCodeType);
VAR i,j: Node;
    t: ir.TSNode;
    l: Loop;
    trapNode: Node;
BEGIN
    -- *shell
    IF at.OptimizeTraps IN at.COMP_MODE THEN
        trapNode := gr.NewNode(); -- node for trap calls
    END;

    gr.FindLoops;
    HasCode := h;
    FOR i:=ir.ZERONode TO SYSTEM.PRED(ir.Nnodes) DO
        ir.Nodes^[i].Processed := FALSE;
    END;
    IF gr.NLoops <> ir.ZEROLoop THEN
        NEW (Orders, gr.NLoops);
    END;
    NEW (GenOrder, ir.Nnodes);
    NEW (SiberiaOrder, ir.Nnodes);
    SiberiaNum := VAL(GenOrdNode,ir.Nnodes);

    -- *shell
    IF at.OptimizeTraps IN at.COMP_MODE THEN
    -- сделать самой последней ноду с трэпами.
        DEC(SiberiaNum);
        SiberiaOrder^[SiberiaNum] := trapNode;
        TrapGenOrder := SiberiaNum;
    END;

    RetGenOrder := VAL(GenOrdNode, SYSTEM.PRED(ir.Nnodes)); -- =EndGenOrder


    IF at.debug IN at.COMP_MODE THEN
        t := LEN (ir.Order^);
        LOOP
            DEC (t);
            IF t < ir.StartOrder THEN
                EXIT;
            ELSIF (ir.Nodes^[ir.Order^[t]].Last <> NIL) AND
                  ((ir.Nodes^[ir.Order^[t]].Last^.Op = ir.o_ret) OR
                  (ir.Nodes^[ir.Order^[t]].Last^.Op = ir.o_retfun))
            THEN
                ir.Nodes^[ir.Order^[t]].Processed := TRUE;
                DEC (SiberiaNum);
                SiberiaOrder^[SiberiaNum] := ir.Order^[t];
                RetGenOrder := Num;
                EXIT;
            END;
        END;
    END;

    FOR l:=ir.ZEROLoop TO SYSTEM.PRED(gr.NLoops) DO
        SortOneLoop (l);
    END;
    Num := VAL(GenOrdNode,ir.Nnodes);
    NEW (CurrentOrder, ir.Nnodes);

    Search (ir.ZERONode, ir.UndefLoop, FALSE);

    FOR i:=SYSTEM.PRED(ir.Nnodes) TO SiberiaNum BY -1 DO
        GenOrder^[i] := SiberiaOrder^[i];
    END;


    j := ir.Nnodes;
    i := SiberiaNum;

    WHILE j>Num DO
        DEC(j);
        DEC(i);
        GenOrder^[i]:=CurrentOrder^[j];
    END;

    StartGenOrder := i;
    EndGenOrder   := VAL(GenOrdNode, SYSTEM.PRED(ir.Nnodes));
    Orders        := NIL;
    CurrentOrder  := NIL;
END GenSort;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                            Generate fi-functions                           *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

(*
  Сейчас сделано очень уж дубово, но, наверное, сойдет; можно сделать:
  последнее присваивание в цепочке не генерировать, а освобождать регистр
  (сейчас - только в простейших случаях); эти присваивания генерировать
  в конце. Кроме того, сейчас циклы генерируются всегда в самом хвосте,
  перед простыми пересылками, а можно в середине.
*)

PROCEDURE GenFies* (n: Node;
                    GenMove:    PROCEDURE (v: VarNum; p: ParamPtr);
                    GenLoop:    PROCEDURE (p: VarNumArray; n: INT);
                    SameMemory: PROCEDURE (v: VarNum; p: ParamPtr): BOOLEAN;
                    IntersectMemory: PROCEDURE (v: VarNum; p: ParamPtr): BOOLEAN);

TYPE StatusType = (
        Waiting,
        Trivial,
        Processed,
        Generated );
VAR
    status: POINTER TO ARRAY OF StatusType;

    needComplexAssign:  BOOLEAN;
    params: ParamArray;
    vars: VarNumArray;
    ds: POINTER TO ARRAY OF INT;
    N:  INT;

(*
  Есть ли переменная в params еще где-нибудь?
*)

    PROCEDURE FindInParams (v: VarNum): INT;
    VAR i: INT;
    BEGIN
        FOR i:=0 TO N DO
            IF (status^[i]<=Processed) & (vars^[i] <> v) & IntersectMemory (v,params^[i]) THEN
                RETURN i;
            END;
        END;
        RETURN -1;
    END FindInParams;

(*
  Процедура генерации пересылок
*)

    PROCEDURE Gen (i: INT);
    VAR j, k, l: INT;
        lp:   VarNumArray;
    BEGIN
        IF status^[i] = Processed THEN          -- Цикл:
            NEW (lp, N + 1);                -- Собрать его
            k := 0;
            j := i;
            REPEAT
                status^[j] := Generated;
                lp^[k] := vars^[j];
                INC (k);
                j := ds^[j];
            UNTIL j = i;
            j := i;                         -- Сгенерировать тех,
            REPEAT                          -- кто от цикла зависит
                LOOP
                    l := FindInParams (vars^[j]);
                    IF l = -1 THEN
                        EXIT;
                    END;
                    Gen (l);
                END;
                j := ds^[j];
            UNTIL j = i;
            GenLoop (lp, k);
        ELSE                                -- Еще не знаем, цикл или нет;
                                            -- Сначала пытаемся сгенерировать
                                            -- другие присваивания, которые
                                            -- надо сделать до этого
            status^[i] := Processed;
            LOOP
                j := FindInParams (vars^[i]);
                IF j = -1 THEN
                    EXIT;
                END;
                ds^[i] := j;
                Gen (j);
                IF status^[i] = Generated THEN  -- Если входила в цикл, то
                                            -- ее уже сгенерировали.
                    RETURN;
                END;
            END;
            GenMove (vars^[i], params^[i]);
            status^[i] := Generated;
        END;
    END Gen;

(*
  Сделать все сложные пересылки
*)

    PROCEDURE GenAllComplex;
    VAR i: INT;
    BEGIN
        FOR i:=0 TO N DO
            IF status^[i] = Waiting THEN
                Gen (i);
            END;
        END;
    END GenAllComplex;

(*
  Сгенерировать один узел
*)

    PROCEDURE GenNode (a: Arc);
    VAR p:    TriadePtr;
        n:    Node;
        i, j: INT;
    BEGIN
        n := gr.Arcs^[a].t;
(*
  Сначала посчитаем количество пересылок
*)
        i := 0;
        p := ir.Nodes^[n].First;
        j := gr.FindInArc (a);
        REPEAT
            INC (i);
            p := p^.Next;
        UNTIL p^.Op <> ir.o_fi;
(*
    Сформировать таблицы
*)
        IF (params = NIL) OR (LEN (params^) < i) THEN
            NEW (params, i);
            NEW (vars, i);
            NEW (status, i);
            NEW (ds, i);
        END;
        i := 0;
        p := ir.Nodes^[n].First;
        REPEAT
            IF NOT SameMemory (p^.Name, p^.Params^[j]) THEN
                vars^[i] := p^.Name;
                params^[i] := p^.Params^[j];
                status^[i] := Waiting;
                INC (i);
            END;
            p := p^.Next;
        UNTIL p^.Op <> ir.o_fi;
        N := i - 1;
(*
    Освободить регистры; выяснить: нужен ли общий алгоритм?
*)
        needComplexAssign := FALSE;
        FOR i:=0 TO N DO
            IF FindInParams (vars^[i]) = -1 THEN
                status^[i] := Trivial;
            ELSE
                needComplexAssign := TRUE;
            END;
        END;
(*
  Сгенерировать сложные пересылки, если надо
*)
        IF needComplexAssign THEN
            GenAllComplex;
        END;
(*
  Теперь - простые пересылки
*)
        FOR i:=0 TO N DO
            IF status^[i] = Trivial THEN
                FOR j := i+1 TO N DO
                  ASSERT(~IntersectMemory (vars^[i], params^[j]));
                END;
                GenMove (vars^[i], params^[i]);
            END;
        END;
    END GenNode;

(*
  Собственно GenFies
*)

BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== Color.GenFies\n");
<* END *>
    IF (ir.Nodes^[n].NOut = 1) &
       (ir.Nodes^[ir.Nodes^[n].Out^[0]].First^.Op = ir.o_fi)
    THEN
        params := NIL;
        GenNode (ir.Nodes^[n].OutArcs^[0]);
    END;
END GenFies;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*            Посчитать выгоды от размещения переменных на регистрах          *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

TYPE RegAllocProfitsType = POINTER TO ARRAY OF INTEGER;
VAR RegAllocProfits: RegAllocProfitsType;
    RegAllocOk*:     BOOLEAN;

CONST
    Undef       = MIN (INTEGER) + 1;
    Nesting     = ARRAY OF INT { 2, 16, 64, 64, 64 };
--    Nesting     = ARRAY OF INT { 0, 4, 16, 32, 64 };
    MaxNest     = 4;
    MaxProf     = 64;
    SameMemory  = <* IF TARGET_VAX OR TARGET_RISC OR TARGET_SPARC THEN *> 2 <* ELSE *> 1 <* END *>;

(* -------------------------------------------------------------------------- *)

(*
  Посчитать выгоду для одной переменной
*)

PROCEDURE CalcOneProfit (v: VarNum; loop: INTEGER): INT;
VAR p: TriadePtr;
    q: ParamPtr;
    c, i, n: INT;
BEGIN
    p := ir.Vars^[v].Def;
    n := ir.Nodes^[p^.NodeNo].Nesting;
    IF n > MaxNest THEN
        INC (n, MaxProf);
    ELSE
        n := Nesting [n];
    END;
    c := -1;
    IF (loop = ir.UndefLoop) OR (gr.NodeInLoop(p.NodeNo,loop)) THEN
        CASE p^.Op OF
        | ir.o_fi:
            c := 0;
            FOR i:=0 TO LEN (p^.Params^)-1 DO
                IF (p^.Params^[i].tag <> ir.y_Variable) OR
                   (Allocation^[p^.Params^[i].name].Cluster <>
                    Allocation^[v].Cluster)
                THEN
                    INC (c);        -- надо бы учитывать вложенность
                END;
            END;
        | ir.o_load:
            IF (Allocation^[v].Location = p^.Params^[0].name) &
               (Allocation^[v].Offset = 0)
            THEN
                c := - n * SameMemory;
            ELSE
                c :=   n;
            END;
        | ir.o_loadr:
            IF (p^.Params^[0].tag = ir.y_AddrConst) &
               (Allocation^[v].Location = p^.Params^[0].name) &
               (Allocation^[v].Offset = p^.Params^[0].offset)
            THEN
                c := - n * SameMemory;
            ELSE
                c :=   n;
            END;
        | ir.o_getpar:
            IF
    <* IF TARGET_RISC OR TARGET_SPARC THEN *>
               (prc.WhereParam(prc.ProcList[at.curr_procno].proto_no,p^.NPar)
                 = prc.STACK) &
    <* END *>
               (Allocation^[v].Location = prc.ParamLoc (p^.NPar)) &
               (Allocation^[v].Offset   = 0)
            THEN
                c := - n * SameMemory;
            ELSE
                c :=   n;
            END;
        | ELSE
            c := n;
        END;
    END;
    q := ir.Vars^[v].Use;
    WHILE q <> NIL DO
        p := q^.triade;
        n := ir.Nodes^[p^.NodeNo].Nesting;
        IF n > MaxNest THEN
            INC (n, MaxProf);
        ELSE
            n := Nesting [n];
        END;
        IF (loop = ir.UndefLoop) OR (gr.NodeInLoop(p.NodeNo,loop)) THEN
            CASE p^.Op OF
            | ir.o_fi:
                IF Allocation^[p^.Name].Cluster <> Allocation^[v].Cluster THEN
                    INC (c);
                END;
            | ir.o_store:
                IF (Allocation^[v].Location = p^.Name) &
                   (Allocation^[v].Offset = 0)
                THEN
                    DEC (c, n * SameMemory);
                ELSE
                    INC (c, n);
                END;
            | ir.o_storer:
                IF (p^.Params^[0].tag = ir.y_AddrConst) &
                   (Allocation^[v].Location = p^.Params^[0].name) &
                   (Allocation^[v].Offset = p^.Params^[0].offset)
                THEN
                    DEC (c, n * SameMemory);
                ELSE
                    INC (c, n);
                END;
            | ELSE
                INC (c, n);
            END;
        END;
        q := q^.next;
    END;
    RETURN c;
END CalcOneProfit;

(* -------------------------------------------------------------------------- *)

(*
  Оценить, в скольки линейных участках жива переменная
*)

PROCEDURE CalcNNodes (v: ir.VarNum; loop:INTEGER): INT;
VAR s: INT;
    i: ir.TSNode;
BEGIN
    s := 0;
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        IF ((loop = ir.UndefLoop) OR (gr.NodeInLoop(ir.Order[i],loop)))
        AND BitVect_In(LiveAtBottom^[ir.Order^[i]], ORD(v)) THEN
            INC (s);
        END;
    END;
    RETURN s;
END CalcNNodes;

(* -------------------------------------------------------------------------- *)

(*
  Посчитать (и запомнить) выгоду от размещения переменной/кластера на регистре
*)

PROCEDURE CalcRegProfit* (v: VarNum): INTEGER;
VAR c, s, n, foo: INT;
    i: VarNum;
    j: INTEGER;
BEGIN
    IF NOT RegAllocOk THEN
        IF (RegAllocProfits = NIL) OR (LEN (RegAllocProfits^) < ir.NVars) THEN
            NEW (RegAllocProfits, ir.NVars);
        END;
        FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
            RegAllocProfits^[i] := Undef;
        END;
        RegAllocOk := TRUE;
    END;
    IF RegAllocProfits^[v] = Undef THEN
        IF v < NNonTempVars THEN
            c := Allocation^[v].Cluster;
            s := 0;
            n := 1;
            FOR foo:=0 TO Clusters^[c].N-1 DO
                INC (n, CalcNNodes (Clusters^[c].v^[foo],ir.UndefLoop));
                INC (s, CalcOneProfit (Clusters^[c].v^[foo],ir.UndefLoop));
            END;
            IF s > 0 THEN
                j := VAL (INTEGER, (s + ir.Nnodes - 1) DIV ((n DIV 5)+1));
            ELSE
                j := VAL (INTEGER, s);
            END;
            FOR foo:=0 TO Clusters^[c].N-1 DO
                RegAllocProfits^[Clusters^[c].v^[foo]] := j;
            END;
        ELSE
            s := CalcOneProfit (v,ir.UndefLoop);
            IF s > 0 THEN
                j := VAL (INTEGER, (s+ir.Nnodes-1) DIV ((CalcNNodes (v,ir.UndefLoop) DIV 5) + 1));
            ELSE
                j := VAL (INTEGER, s);
            END;
            RegAllocProfits^[v] := j;
        END;
    END;
    RETURN RegAllocProfits^[v];
END CalcRegProfit;

(* -------------------------------------------------------------------------- *)

PROCEDURE UnsteadyProfit(v: ir.VarNum; averProf: INTEGER; loop: INTEGER):BOOLEAN;
VAR s: INT;
BEGIN
    s := CalcOneProfit (v,loop);
    IF s > 0 THEN
        s := VAL (INTEGER, (s+ir.Nnodes-1) DIV (CalcNNodes (v,loop) + 1));
        IF s > averProf*4 THEN
<* IF ~NODEBUG THEN *>
          IF opIO.needed THEN
            opIO.print("UnsteadyProfit %d vs average %d for var ",s,averProf);
            ir.PrintVar(v);
            opIO.print(" ir.NVars = %d\n",ir.NVars);
          END;
<* END *>
          RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END UnsteadyProfit;

PROCEDURE SplitVar(v: ir.VarNum; loop: INTEGER);
VAR newv : ir.VarNum;
    tr: TriadePtr;
    param: ir.ParamPtr;
    bv: BitVect.BitVector;
    newprofits: RegAllocProfitsType;
    i: INTEGER;
    done:BOOLEAN;
BEGIN
<* IF ~NODEBUG THEN *>
    IF opIO.needed THEN
      opIO.print("SplitVar ");
      ir.PrintVar(v);
      opIO.print(" in loop %d\n",loop);
    END;
<* END *>
    tr := ir.NewTriadeInit(1, ir.o_assign, ir.Vars[v].Def.ResType,ir.Vars[v].Def.ResSize);
    ir.MakeParVar(tr.Params[0], v);
    ir.GenResVar(tr);
    newv := tr.Name;
    gr.InsertTriade(tr, ir.Nodes[gr.LoopList[loop].Preheader].Last);
    param := ir.Vars[v].Use;
    done := FALSE;
    WHILE param # NIL DO
      IF gr.NodeInLoop(param.triade.NodeNo, loop) THEN
        ir.RemoveUse(param);
        ir.MakeParVar(param, newv);
        done := TRUE;
      END;
      param := param.next;
    END;
    ASSERT(done);
    bv := BitVect.New (ir.NVars, FALSE);
    CalcLiveness (bv);
    NEW (newprofits, LEN (RegAllocProfits^) + 1);
    FOR i:=0 TO ir.NVars-2 DO
        newprofits^[i] := RegAllocProfits^[i];
    END;
    newprofits^[ir.NVars-1] := Undef;
    RegAllocProfits := newprofits;
--    gr.CheckCorrectness();
END SplitVar;

PROCEDURE SplitVars;
VAR v: ir.VarNum;
    l: INTEGER;
    averProf: INTEGER;
    bv: BitVect.BitVector;
BEGIN
<* IF ~NODEBUG THEN *>
    opIO.needed := xiEnv.config.Option("showsplit");
<* END *>
    ir.Squeeze;
    bv := BitVect.New (ir.NVars, FALSE);
    CalcLiveness (bv);
    v := NNonTempVars;
    WHILE v < ir.NVars DO
      averProf := CalcRegProfit(v);
      FOR l := 0 TO gr.NLoops-1 DO
        IF ~gr.NodeInLoop(ir.Vars[v].Def.NodeNo, l) AND UnsteadyProfit(v, averProf, l) THEN
          SplitVar(v,l);
        END;
      END;
      INC(v);
    END;
END SplitVars;

BEGIN
    NNonTempVars    := ir.UNDEFINED;
    RegAllocOk      := FALSE;
    RegAllocProfits := NIL;
END Color.
