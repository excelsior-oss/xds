MODULE Heuristics;

IMPORT ir,
       Color,
       RD := RDefs,
       R := r386,
       StdIO := opIO,
       Emit,
       BitVect;
IMPORT at  := opAttrs;
IMPORT nts := BurgNT;
IMPORT T := BurgTables;
IMPORT reg := reg386;
IMPORT P := p386;
IMPORT D := desc386;
IMPORT env := xiEnv;
IMPORT SYSTEM;
--------------------------------------------------------------------------------


<* IF TARGET_386 AND fast_bitvect_in THEN *>
<* +procinline *>
<* NEW no_set_range_check+ *>
<* PUSH *>
<*-CHECKNIL    *>
<*-CHECKDIV    *>
<*-CHECKPROC   *>
<*-CHECKSET    *>
<*-CHECKINDEX  *>
<*-CHECKDINDEX  *>
<*-CHECKRANGE  *>
<*-CHECKNIL    *>

PROCEDURE BitVect_In (p: BitVect.BitVector; e: LONGINT): BOOLEAN;
<* NEW override_max_set_len+ *>
TYPE
VAR foo:BitVect.LongBitVector;
BEGIN
    foo := SYSTEM.VAL(BitVect.LongBitVector,p);
    RETURN e IN foo^.v;
END BitVect_In;


PROCEDURE BitVect_Incl (p: BitVect.BitVector; e: ir.INT);
BEGIN
    INCL (p^.v [e DIV BitVect.BitsPerSet], e MOD BitVect.BitsPerSet);
END BitVect_Incl;
<* POP *>

<* ELSE *>

PROCEDURE BitVect_In (p: BitVect.BitVector; e: ir.INT): BOOLEAN;
BEGIN
    RETURN BitVect.In(p, e);
END BitVect_In;


PROCEDURE BitVect_Incl (p: BitVect.BitVector; e: ir.INT);
BEGIN
    BitVect.Incl(p, e);
END BitVect_Incl;

<* END *>

PROCEDURE reg386_AreHooked* (v1, v2: ir.VarNum): BOOLEAN;
BEGIN
    RETURN BitVect_In  (RD.HookedVars^[v1], ORD(v2)) OR
           BitVect_In  (RD.HookedVars^[v2], ORD(v1));
END reg386_AreHooked;


(*
  Разобрать дерево - способ адресации; кроме чисто проверки живости компонент
  адресного выражения, задно посчитать, сколько его компонент лежит в памяти
*)


PROCEDURE AddrComponentVars*(p: RD.DAGNODE): INTEGER;
BEGIN
  IF (p.nt = nts.NTmem) & (p.op = ir.o_loadr) THEN
    p := p.l;
  END;
  RETURN ORD(p.a.place1.v#ir.UNDEFINED)+
         ORD(p.a.place2.v#ir.UNDEFINED);
END AddrComponentVars;

PROCEDURE LiveAddrStep (p: RD.DAGNODE; goalnt: nts.NT;
                        v: ir.VarNum; VAR n: ir.INT): BOOLEAN;
VAR eruleno: nts.Rule;
    i:  ir.INT;
    index : T.Index;
    kids:    ARRAY T.MAXNKIDS OF RD.DAGNODE;
BEGIN
    CASE goalnt OF
    | nts.NTbased,
      nts.NTscaled,
      nts.NTaddr:
        eruleno := p.rule[ goalnt ];
        index     := T.NTnts [eruleno];
        T.NTkids (p, eruleno, kids);
        IF T.NTnts_n [index] = nts.NTnowhere THEN
            ASSERT (p^.op = ir.o_par);
            IF p^.par^.tag = ir.y_Variable THEN
                RETURN LiveAddrStep (RD.Trees^[p^.par^.name], goalnt, v, n);
            END;
        ELSE
            i := 0;
            REPEAT
                IF NOT LiveAddrStep (kids [i], T.NTnts_n [SYSTEM.SUCC(index,i)], v, n) THEN
                    RETURN FALSE;
                END;
                INC (i);
            UNTIL T.NTnts_n [SYSTEM.SUCC(index,i)] = nts.NTnowhere;
        END;
        RETURN TRUE;
    | ELSE
        IF p^.op = ir.o_par THEN
            IF p^.par^.tag <> ir.y_Variable THEN
                RETURN TRUE;
            END;
            IF RD.Loc^[p^.par^.name].tag = nts.NTlocal THEN
                INC (n);
            END;
            RETURN Color.LiveAtAllUsages (p^.par^.name, v);
        ELSIF p^.tr^.Name = v THEN
            RETURN FALSE;   -- чтобы избежать цепного правила addr: reg
        ELSE
            RETURN Color.LiveAtAllUsages (p^.tr^.Name, v);
        END;
    END;
END LiveAddrStep;

--------------------------------------------------------------------------------

PROCEDURE LiveAddrMode* (p: RD.DAGNODE;     -- Проверить живость компонент
                         goalnt: nts.NT;       -- адресного выражения по всем
                         v: ir.VarNum)          -- использованиям v
          : BOOLEAN;
VAR n: ir.INT;
BEGIN
    n := 0;
    RETURN ~(at.fastcomp) & LiveAddrStep (p, goalnt, v, n) ;--& (n < 2);
END LiveAddrMode;

--------------------------------------------------------------------------------

(*
   1) u жива всюду, где жива v
   2) выгода u += выгода v
   3) u^.Options |= v^.Options
*)

<* PUSH *>
<*-CHECKINDEX  *>
<*-CHECKRANGE  *>
<*-CHECKNIL    *>
<*-CHECKDIV    *>
<*-CHECKPROC   *>
<*-CHECKSET    *>
PROCEDURE SetHooked (u, v: ir.VarNum);
VAR w,z:    ir.VarNum;
    i, c: ir.INT;
BEGIN
(*<* IF ~NODEBUG THEN*>
    StdIO.print("==== h386.SetHooked\t");
    ir.PrintVar(u);
    StdIO.print(" profit= %d used for ", RD.Loc^[u].profit);
    ir.PrintVar(v);
    StdIO.print(" profit= %d\n", RD.Loc^[v].profit);
<* END *>*)
(*
    IF RD.Loc^[u].profit < RD.Loc^[v].profit DIV 4 THEN
<* IF ~NODEBUG THEN*>
        StdIO.print("NO HOOK\n");
        RETURN;
<* END *>
    END;
*)
    IF RD.Loc[v].hasLongLifeTime THEN
       RD.Loc[u].hasLongLifeTime := TRUE;
       RD.Loc[u].usedFor := v;
    END;
    FOR w:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
<* IF TARGET_386 AND fast_bitvect_in THEN *>
        IF BitVect_In (RD.HookedVars^[w], ORD(v)) THEN
<* ELSE *>
        IF BitVect.In (RD.HookedVars^[w], ORD(v)) THEN
<* END *>
          IF u < Color.NNonTempVars THEN
            c := Color.Allocation^[u].Cluster;
            FOR i:=0 TO Color.Clusters^[c].N-1 DO
              z := Color.Clusters^[c].v^[i];
              BitVect_Incl (RD.HookedVars^[w], ORD(z));
              BitVect_Incl (RD.HookedVars^[z], ORD(w));
            END;
          ELSE
            BitVect_Incl (RD.HookedVars^[w], ORD(u));
            BitVect_Incl (RD.HookedVars^[u], ORD(w));
          END;

(*<* IF ~NODEBUG THEN*>
            StdIO.print("  Hook \t");
            ir.PrintVar(w);
            StdIO.print("\t");
            ir.PrintVar(u);
            StdIO.print("\n");
<* END *>*)
        END;
    END;
    IF u < Color.NNonTempVars THEN
        c := Color.Allocation^[u].Cluster;
        FOR i:=0 TO Color.Clusters^[c].N-1 DO
            w := Color.Clusters^[c].v^[i];
            INC (RD.Loc^[w].profit, RD.Loc^[v].profit);
            ir.Vars^[w].Options := ir.Vars^[w].Options + ir.Vars^[v].Options;
        END;
    ELSE
        INC (RD.Loc^[u].profit, RD.Loc^[v].profit);
        ir.Vars^[u].Options := ir.Vars^[u].Options + ir.Vars^[v].Options;
    END;
<* IF ~NODEBUG THEN*>
    StdIO.print("New profit= %d\n", RD.Loc^[u].profit);
<* END *>
END SetHooked;
<* POP *>
--------------------------------------------------------------------------------

(*
  Пересчитать зацепленность vars с учетом разметки DAG'a и изменения областей
  жизни переменных в связи с посажением других переменных на способ адресации
  - один шаг рекурсии
*)

PROCEDURE RecalcStep (p: RD.DAGNODE; goalnt: nts.NT; v: ir.VarNum);
VAR eruleno: nts.Rule;
    i:  ir.INT;
    index : T.Index;
    kids:    ARRAY T.MAXNKIDS OF RD.DAGNODE;
BEGIN
    CASE goalnt OF
    | nts.NTbased,
      nts.NTscaled,
      nts.NTaddr:
        eruleno := p.rule[ goalnt ];
        index     := T.NTnts [eruleno];
        T.NTkids (p, eruleno, kids);
        IF T.NTnts_n [index] = nts.NTnowhere THEN
            ASSERT (p^.op = ir.o_par);
            IF p^.par^.tag = ir.y_Variable THEN
                RecalcStep (RD.Trees^[p^.par^.name], goalnt, v);
            END;
        ELSE
            i := 0;
            REPEAT
                RecalcStep (kids [i], T.NTnts_n [SYSTEM.SUCC(index,i)], v);
                INC (i);
            UNTIL T.NTnts_n [SYSTEM.SUCC(index,i)] = nts.NTnowhere;
        END;
    | ELSE
        IF p^.op = ir.o_par THEN
            IF p^.par^.tag = ir.y_Variable THEN
                SetHooked (p^.par^.name, v);
            END;
        ELSE
            IF p^.tr^.Name <> v THEN
                SetHooked (p^.tr^.Name, v);
            END;
        END;
    END;
END RecalcStep;

--------------------------------------------------------------------------------

(*
  Зацеплены ли два кластера?
*)

PROCEDURE HookedClusters (c1, c2: ir.INT): BOOLEAN;
VAR i, j: ir.INT;
    u:    ir.VarNum;
BEGIN
    FOR i:=0 TO Color.Clusters^[c1].N-1 DO
        u := Color.Clusters^[c1].v^[i];
        FOR j:=0 TO Color.Clusters^[c2].N-1 DO
            IF reg386_AreHooked (u, Color.Clusters^[c2].v^[j]) THEN
                RETURN TRUE;
            END;
        END;
    END;
    RETURN FALSE;
END HookedClusters;

--------------------------------------------------------------------------------

(*
  Отметить зацепленность двух кластеров
*)

PROCEDURE SetHookedClusters (c1, c2: ir.INT);
VAR i, j: ir.INT;
    u, v: ir.VarNum;
    vec:  ir.VarNumArray;
BEGIN
    FOR i:=0 TO Color.Clusters^[c1].N-1 DO
        u   := Color.Clusters^[c1].v^[i];
        vec := Color.Clusters^[c2].v;
        FOR j:=0 TO Color.Clusters^[c2].N-1 DO
            v := vec^[j];
            BitVect_Incl (RD.HookedVars^[u], ORD(v));
            BitVect_Incl (RD.HookedVars^[v], ORD(u));
        END;
    END;
END SetHookedClusters;

--------------------------------------------------------------------------------

(*
  1) Пересчитать зацепленность vars с учетом разметки DAG'a и
     изменения областей жизни переменных в связи с посажением
     других переменных на способ адресации  - головная проце-
     дура; срабатывает только для корней DAG'а;
  2) Зацепить кластера (т.е. отметить,  что каждая переменная
     первого кластера зацеплена с каждой переменной  второго,
     если зацеплена хоть одна пара).
  3) Пересчитать выгоды от размещения переменных на регистрах
     с учестом способов адресации
--4) Правильно выставить RD.Loc^[*].tag у переменных, из которых
--   получается вершина DAGа, находящихся на способе адресации.
*)

PROCEDURE RecalcHookedAndProfits*;
VAR k,v,v3,v2: ir.VarNum;
--  i: ir.VarNum;
    c,j: Color.ClusterNum;
    p:    RD.DAGNODE;
BEGIN
(*
  Изначально взять зацепленность и выгоды из Color,
*)
    FOR k:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
        BitVect.Move (Color.HookedVars^[k], RD.HookedVars^[k]);
        RD.Loc^[k].profit := Color.CalcRegProfit ( k );
        RD.Loc[k].hasLongLifeTime := ~RD.Loc[k].temp;
    END;
(*
  Теперь продлить зацепленность способов адресации
*)
(* commented by kevin
    FOR i:=ir.StartOrder TO ir.Nnodes-1 DO
        p := RD.Dag^[ir.Order^[i]];
        WHILE p <> NIL DO
            IF p^.p^.Tag = ir.y_Variable THEN
                CASE RD.Loc^[p^.p^.Name].tag OF
                | %based,
                  %scaled,
                  %addr:    RecalcStep (p, RD.Loc^[p^.p^.Name].tag,
                                            p^.p^.Name);
                | %mem:     RecalcStep (p^.l, R.NTaddr, p^.p^.Name);
                | ELSE
                END;
            END;
            p := p^.next;
        END;
    END;
*)
    FOR v:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
        p := RD.Trees^[v];
        IF p#NIL THEN
            CASE RD.Loc^[v].tag OF
            | nts.NTbased,
              nts.NTscaled,
              nts.NTaddr:    RecalcStep (p,    RD.Loc^[v].tag, v);
            | nts.NTmem:
                             IF    p^.op=ir.o_loadr THEN
                               RecalcStep (p^.l, nts.NTaddr, v);
                             ELSIF (p^.op=ir.o_val) OR (p^.op=ir.o_cast) OR (p^.op=ir.o_hiword) THEN
                               RecalcStep (p^.l, nts.NTmem, v);
                             END;
            | ELSE
            END;
        END;
    END;

(*
  А теперь сцепить кластера
*)
    FOR c:=0 TO Color.NClusters-1 DO
        FOR j:=0 TO Color.NClusters-1 DO
            IF (c <> j) & HookedClusters (c, j) THEN
                SetHookedClusters (c, j);
            END;
        END;
    END;
(*-- Symmetric check
    FOR k:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
      FOR k2:=ir.ZEROVarNum TO SYSTEM.PRED(k) DO
        ASSERT( BitVect.In (RD.HookedVars^[k], k2) = BitVect.In (RD.HookedVars^[k2], k));
      END;
    END;
*)
END RecalcHookedAndProfits;

--------------------------------------------------------------------------------

PROCEDURE LiveReg* (p: RD.DAGNODE;          -- Проверить живость регистра
                    v: ir.VarNum)               -- по всем использованиям v
          : BOOLEAN;
BEGIN
    IF p^.op = ir.o_par THEN
        IF p^.par^.tag <> ir.y_Variable THEN
            RETURN TRUE;
        END;
        RETURN Color.LiveAtAllUsages (p^.par^.name, v);
    ELSE
        ASSERT (p^.tr^.Tag = ir.y_Variable);
        RETURN Color.LiveAtAllUsages (p^.tr^.Name, v);
    END;
END LiveReg;

--------------------------------------------------------------------------------

PROCEDURE ShortDifferProfits* (p: RD.DAGNODE;         -- Мало ли различаются профиты
                               v: ir.VarNum)               -- для посажения на адресацию
          : BOOLEAN;
VAR u : ir.VarNum;
BEGIN
    IF p^.op = ir.o_par THEN
        IF p^.par^.tag <> ir.y_Variable THEN
            RETURN TRUE;
        END;
        u := p^.par^.name;
    ELSE
        ASSERT (p^.tr^.Tag = ir.y_Variable);
        u := p^.tr^.Name;
    END;
    RETURN
           (Color.CalcRegProfit(u) > Color.CalcRegProfit(v) DIV 10)
       AND (Color.CalcRegProfit(v) > Color.CalcRegProfit(u) DIV 10);
END ShortDifferProfits;

--------------------------------------------------------------------------------

PROCEDURE NobodyWrites* (p: RD.DAGNODE)     -- Пишет ли кто-нибудь в память
          : BOOLEAN;                            -- откуда читает p, до всех
                                                -- использований p^.p^.Name?
BEGIN
    RETURN Color.NobodyWrites (p^.tr, p^.tr^.Name);
END NobodyWrites;

--------------------------------------------------------------------------------

(*
  Попытаться по использованию вершины, которой еще не приписан
  нетерминал из других соображений,  решить,  какой ей следует
  приписать нетерминал - СПЛОШНАЯ ЭВРИСТИКА !

  Условия должны быть не менее строгими, чем в грамматике, т.к.
  based и addr можно вывести просто из reg, а зазря лучше не выводить
*)

PROCEDURE GoalNonTerm* (p: RD.DAGNODE): nts.NT;
VAR q: ir.TriadePtr;
    v: ir.VarNum;

    PROCEDURE UsedAsAddr (v: ir.VarNum): BOOLEAN;
    VAR p: ir.TriadePtr;
        q: ir.ParamPtr;
    BEGIN
        q := ir.Vars^[v].Use;
        WHILE q <> NIL DO
            p := q^.triade;
            CASE p^.Op OF
            | ir.o_loadr:
            | ir.o_storer:
                IF q^.paramnumber <> 0 THEN
                    RETURN FALSE;
                END;
            | ir.o_add:
                IF (p^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} <> ir.OptionsSet{}) OR
                   (p^.ResType = ir.t_float) OR (p^.ResSize <> 4)      OR
                   NOT P.ParConstNotStackAddr (p^.Params^[1-q^.paramnumber]) OR
                   NOT UsedAsAddr (p^.Name)
                THEN
                    RETURN FALSE;
                END;
            | ir.o_sub:
                IF (p^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} <> ir.OptionsSet{}) OR
                   (p^.ResType = ir.t_float) OR (p^.ResSize <> 4)      OR
                   NOT P.ParConstNotStackAddr (p^.Params^[1])          OR
                   NOT UsedAsAddr (p^.Name)
                THEN
                    RETURN FALSE;
                END;
            | ELSE
                RETURN FALSE;
            END;
            q := q^.next;
        END;
        RETURN TRUE;
    END UsedAsAddr;

    PROCEDURE UsedAsAddrComponent (v: ir.VarNum): BOOLEAN;
    VAR p: ir.TriadePtr;
        q: ir.ParamPtr;
    BEGIN
        q := ir.Vars^[v].Use;
        WHILE q <> NIL DO
            p := q^.triade;
            CASE p^.Op OF
            | ir.o_loadr:
            | ir.o_storer:
                IF q^.paramnumber <> 0 THEN
                    RETURN FALSE;
                END;
            | ir.o_add:
                IF    (p^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} <> ir.OptionsSet{})
                   OR (p^.ResType = ir.t_float) OR (p^.ResSize <> 4)
                THEN
                    RETURN FALSE;
                END;
            | ir.o_sub:
                IF (p^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} <> ir.OptionsSet{}) OR
                   (p^.ResType = ir.t_float) OR (p^.ResSize <> 4)      OR
                   NOT P.ParConstNotStackAddr (p^.Params^[1])
                THEN
                    RETURN FALSE;
                END;
            | ELSE
                RETURN FALSE;
            END;
            q := q^.next;
        END;
        RETURN TRUE;
    END UsedAsAddrComponent;

    PROCEDURE NotMuchUsages (p: ir.TriadePtr): BOOLEAN;
    VAR q: ir.ParamPtr;
        n: ir.INT;
    BEGIN
        q := ir.Vars^[p^.Name].Use;
        n := 0;
        WHILE q <> NIL DO
            IF (q^.triade^.NodeNo <> p^.NodeNo) OR (n > 0) THEN
                RETURN FALSE;
            END;
            INC (n);
            q := q^.next;
        END;
        RETURN TRUE;
    END NotMuchUsages;

BEGIN
    q := p^.tr;
    v := q^.Name;
    IF v >= Color.NNonTempVars THEN
        IF (q^.Op = ir.o_loadr) THEN
            IF ~p.forbidden[nts.NTmem] &
               Color.NobodyWrites (q, v) &
               LiveAddrMode (p^.l, nts.NTaddr, v) &
               NotMuchUsages (q) &
               ~UsedAsAddrComponent (v)
            THEN
                RETURN nts.NTmem;
            END;
        ELSIF q^.ResType <> ir.t_float THEN
            CASE q^.Op OF
            | ir.o_mul:
                IF (q^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{}) &
                   (* (q^.ResType <> ir.t_float) & was already checked *) (q^.ResSize = 4)      &
                   (p^.cost [nts.NTscaled] < MAX (INTEGER)) &
                   UsedAsAddrComponent (v)
                THEN
                    RETURN nts.NTscaled;
                END;
            | ir.o_add,
              ir.o_sub:
                IF (q^.Op = ir.o_sub) &
                   NOT P.ParConstNotStackAddr (q^.Params^[1]) OR
                   (q^.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} <> ir.OptionsSet{}) OR
                   (q^.ResType = ir.t_float) OR (q^.ResSize <> 4)
                THEN
                    RETURN nts.NTreg;
                END;
                IF (p^.cost [nts.NTscaled] < MAX (INTEGER)) &
                   UsedAsAddrComponent (v)
                THEN
                    RETURN nts.NTscaled;
                END;
                IF (p^.cost [nts.NTaddr] < MAX (INTEGER)) &
                   (p^.cost [nts.NTaddr] <
                    p^.cost [nts.NTreg]) &
                   LiveAddrMode (p, nts.NTaddr, v) & UsedAsAddr (v)
                THEN
                    RETURN nts.NTaddr;
                END;
                IF (p^.cost [nts.NTbased] < MAX (INTEGER)) &
                   (P.ParConstNotStackAddr (q^.Params^[0]) OR
                    P.ParConstNotStackAddr (q^.Params^[1])) &
                   LiveAddrMode (p, nts.NTbased, v) &
                   UsedAsAddrComponent (v)
                THEN
                    RETURN nts.NTbased;
                END;
            | ELSE
            END;
        END;
    END;
    IF q^.ResType = ir.t_float THEN
        IF (q^.Op <> ir.o_getpar) & P.PlaceInTOS (v) THEN
            RETURN nts.NTtos;
        ELSE
            RETURN nts.NTlocal;
        END;
    END;
    IF q^.ResType = ir.t_arr THEN
        RETURN nts.NTmem;
    END;
    RETURN nts.NTreg;
END GoalNonTerm;

--------------------------------------------------------------------------------


<* IF TRACE THEN *>

PROCEDURE NTPLUS* (s-: ARRAY OF CHAR);
BEGIN
    StdIO.print (" Принято %s\n", s);
END NTPLUS;

PROCEDURE NTMINUS* (s-: ARRAY OF CHAR);
BEGIN
    StdIO.print (" Отвергнуто %s\n", s);
END NTMINUS;

PROCEDURE NTtrace* (p: RD.DAGNODE; eruleno: INTEGER;
                       c0: LONGINT; bc0: INTEGER);
BEGIN
    StdIO.print ("Узел ");
    IF p^.p <> NIL THEN
        IF p^.p^.Tag = ir.y_Variable THEN
            StdIO.print ("v%d", p^.p^.Name);
        ELSE
            StdIO.print ("op%d", p^.p^.Op);
        END;
    ELSE
        StdIO.print ("par%d", p^.par^.tag);
    END;
    StdIO.print (" оценка правила %d = %d против %d", eruleno, c0, bc0);
END NTtrace;

<* END *>
END Heuristics.
