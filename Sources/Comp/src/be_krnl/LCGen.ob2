(* library for long constants support during code generation

   History:
   put new entries on the top:

   06.09.96             ENAL    68k support added
   V@Ts: 22.05.96 10:34 Shortcut LC created for explicit LCDef usage
-- 17.05.96 19:08       KDV     не обходятся fi-функции при перевычислении
                                точек использования длинных констант
   30.04.96 16:12       KDV     created
*)
<*-PROCINLINE *>
MODULE LCGen;

<* +o2addkwd *>
FROM SYSTEM IMPORT PRED,SUCC;
IMPORT LC := LCDef,
       ir,
       gr := ControlGraph,
       bv := BitVect,
       num := LCNumer,
       Color,
       RD := RDefs,
       RDD := RDefs_D;
IMPORT BurgNT;

--------------------------------------------------------------------------------

PROCEDURE SetInLC* (i: LC.LCNum);
BEGIN
<* IF ~NODEBUG THEN *>
    ASSERT((i>=ir.ZEROLCNum) AND (i<=LC.NLCs));
<* END *>
    RD.LCLoc^[i].tag := BurgNT.NTlconst;
END SetInLC;

--------------------------------------------------------------------------------

TYPE LCGen_IDB     *= POINTER TO LCGen_IDB_Rec;
     LCGen_IDB_Rec *= RECORD END;

(*
  Еще до начала первой итерации разместить часть длинных констант как
  IN_LCONST
*)

PROCEDURE(idb : LCGen_IDB) SetLocations*;
BEGIN
  ASSERT(FALSE);
END SetLocations;

(*
  Создать массив с информацией о длинных константах
*)

PROCEDURE(idb : LCGen_IDB) InitLocations*;
VAR i: LC.LCNum;
BEGIN
    IF LC.NLCs <> ir.ZEROLCNum THEN
        NEW (RD.LCLoc, LC.NLCs);
        FOR i:=ir.ZEROLCNum TO PRED(LC.NLCs) DO
            RD.LCLoc^[i].temp := TRUE;
        END;
        idb.SetLocations;
    ELSE
        RD.LCLoc := NIL;
    END;
END InitLocations;

(*
  Перед началом итерации очистить разметку регистров; кроме того,
  сбросить IN_LCONST выгруженные на предыдущем шаге длинные константы
*)

PROCEDURE(idb : LCGen_IDB) ClearLocations*;
BEGIN
  ASSERT(FALSE);
END ClearLocations;

VAR IDB *: LCGen_IDB;

--------------------------------------------------------------------------------

(* считает шикарным вверх-вниз обходом участков живость
   констант в терминах этих самых участков
   после стабилизации находит все константы, живые в точке вызова
*)

PROCEDURE LCLiveness*();
VAR changed: BOOLEAN;
    j   : ir.INT;
    i   : ir.TSNode;
    new    : bv.BitVector;
    lc     : LC.LCNum;
    n      : ir.Node;

    PROCEDURE FindFirst(lc : LC.LCNum);
    VAR i : ir.TSNode;
    BEGIN
        FOR i:=ir.StartOrder TO PRED(LEN(ir.Order^)) DO
          IF bv.In(LC.ConstsOfNodes^[ir.Order[i]].Live,ORD(lc)) THEN
            LC.LongConsts[lc].firstlive := i;
            RETURN;
          END;
        END;
        LC.LongConsts[lc].firstlive := VAL(ir.TSNode, LEN(ir.Order^));
    END FindFirst;

    PROCEDURE FindLast(lc : LC.LCNum);
    VAR i : ir.TSNode;
    BEGIN
        FOR i:=PRED(LEN(ir.Order^)) TO ir.StartOrder BY -1 DO
          IF bv.In(LC.ConstsOfNodes^[ir.Order[i]].Live,ORD(lc)) THEN
            LC.LongConsts[lc].lastlive := i;
            RETURN;
          END;
        END;
        LC.LongConsts[lc].lastlive := ir.UndefTSNode;
    END FindLast;

    PROCEDURE FindCall(lc : LC.LCNum);
    VAR i : ir.TSNode;
    BEGIN
      FOR i:=LC.LongConsts[lc].firstlive TO LC.LongConsts[lc].lastlive DO
        n := ir.Order^[i];
        IF bv.In(LC.NodesWithCalls,ORD(n)) & bv.In(LC.ConstsOfNodes^[n].Live,ORD(lc))
        THEN
            (* в узле есть вызов,
               lc теперь живa в точке вызова
            *)
            INCL(LC.LongConsts[lc].attrs,LC.LiveAtCall_LCATTR);
            RETURN;
        END;
      END;
    END FindCall;

BEGIN
    IF LC.NLCs=ir.ZEROLCNum THEN RETURN; END;
    LC.InitConstsOfNodes;
    FOR i:=ir.StartOrder TO PRED(LEN(ir.Order^)) DO
        bv.Move (LC.ConstsOfNodes^[ir.Order^[i]].Live,
                 LC.ConstsOfNodes^[ir.Order^[i]].Work);
    END;
    new := bv.New (ORD(LC.NLCs), FALSE);
    REPEAT
        changed := FALSE;
        (* FOR i:=ir.StartOrder+1 TO LEN(ir.Order^)-1 DO *)
        (* ir.StartOrder+1 - это для переменных, поскольку на входе в первый
           участок не может быть живых переменных,
           но жизненность констант считается с точностью до участка,
           поэтому уже на входе в первый участок могут быть живые константы
           поэтому цикл от ir.StartOrder
        *)
        FOR i:=ir.StartOrder TO PRED(LEN(ir.Order^)) DO
            n := ir.Order^[i];
            IF ir.Nodes^[n].NIn = 1 THEN
                changed :=
                    bv.UnionAssign(
                        LC.ConstsOfNodes^[n].Work,
                        LC.ConstsOfNodes^[ir.Nodes^[n].In^[0]].Work)
                    OR changed;
            ELSE
                bv.Move (LC.ConstsOfNodes^[n].Work, new);
                FOR j:=0 TO ir.Nodes^[n].NIn-1 DO
                    bv.Union (LC.ConstsOfNodes^[ir.Nodes^[n].In^[j]].Work,
                              new, new);
                END;
                changed := bv.Assign(new, LC.ConstsOfNodes^[n].Work)
                           OR changed;
            END;
        END;
        FOR i:=PRED(LEN(ir.Order^)) TO ir.StartOrder BY -1 DO
            n := ir.Order^[i];
            IF ir.Nodes^[n].NOut = 1 THEN
                changed := bv.UnionAssign(
                              LC.ConstsOfNodes^[n].Live,
                              LC.ConstsOfNodes^[ir.Nodes^[n].Out^[0]].Live)
                           OR changed;
            ELSIF ir.Nodes^[n].NOut <> 0 THEN
                bv.Move (LC.ConstsOfNodes^[n].Live, new);
                FOR j:=0 TO ir.Nodes^[n].NOut-1 DO
                    bv.Union (LC.ConstsOfNodes^[ir.Nodes^[n].Out^[j]].Live,
                              new,
                              new);
                END;
                changed := bv.Assign (new, LC.ConstsOfNodes^[n].Live)
                           OR changed;
            END;
        END;
    UNTIL NOT changed;
    FOR i:=ir.StartOrder TO PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        bv.Intersect (LC.ConstsOfNodes^[n].Live,
                      LC.ConstsOfNodes^[n].Work,
                      LC.ConstsOfNodes^[n].Live);
    END;
    bv.Free (new);
    FOR n := ir.ZERONode TO PRED(ir.Nnodes) DO
        bv.Free(LC.ConstsOfNodes^[n].Work);
    END;
   (*
    теперь находим все константы, живые в точке хоть какого-нибудь вызова
    *)
    FOR lc := ir.ZEROLCNum TO PRED(LC.NLCs) DO
      FindFirst(lc);
      FindLast(lc);
      FindCall(lc);
    END;
END LCLiveness;

--------------------------------------------------------------------------------

(* определяет, на каких дугах выполнять загрузку длинных констант *)
PROCEDURE LCLoadness*();
VAR a        : ir.Arc;
    live_dif : LC.ConstScale;

    PROCEDURE LoadInArc(arc : ir.Arc);
    VAR lc       : LC.LCNum;
        where    : LC.ConstScale;
        from, to : ir.Node;
    BEGIN
        from := gr.Arcs[arc].f;
        to   := gr.Arcs[arc].t;
        bv.Sub(LC.ConstsOfNodes[to].Live,
               LC.ConstsOfNodes[from].Live,
               live_dif);
        IF ir.Nodes[from].NOut = 1 THEN
            where := LC.ConstsOfNodes[from].LoadAtBottom;
        ELSE
            where := LC.ConstsOfNodes[to].LoadAtTop;
        END;
        FOR lc := ir.ZEROLCNum TO PRED(LC.NLCs) DO
            IF bv.In(live_dif, ORD(lc)) THEN bv.Incl(where,ORD(lc)); END;
        END;
    END LoadInArc;


BEGIN
    IF LC.NLCs = ir.ZEROLCNum THEN RETURN; END;
    live_dif := bv.New (ORD(LC.NLCs), FALSE);
    FOR a := ir.ZEROArc TO PRED(gr.Narcs) DO LoadInArc(a); END;
    bv.Union(LC.ConstsOfNodes[ir.ZERONode].LoadAtTop,
             LC.ConstsOfNodes[ir.ZERONode].Live,
             LC.ConstsOfNodes[ir.ZERONode].LoadAtTop);
    bv.Free(live_dif);
END LCLoadness;

--------------------------------------------------------------------------------

(* считает массивы зацепленности
   RD.HookedLCLC, RD.HookedLCVar
*)

PROCEDURE LCHooking*();
VAR lc1, lc2 : LC.LCNum;
       v     : ir.VarNum;
        i, c : ir.INT;
        in_FS: BOOLEAN;

    PROCEDURE LCLCHooked(lc1, lc2 : LC.LCNum) : BOOLEAN;
    VAR n : ir.Node;
        i, max, min: ir.TSNode;
    BEGIN
        max := LC.LongConsts[lc1].firstlive;
        IF max  < LC.LongConsts[lc2].firstlive
        THEN max := LC.LongConsts[lc2].firstlive
        END;
        min := LC.LongConsts[lc1].lastlive;
        IF min  > LC.LongConsts[lc2].lastlive
        THEN min := LC.LongConsts[lc2].lastlive
        END;
        FOR i := max TO min DO
            n := ir.Order[i];
            IF bv.In(LC.ConstsOfNodes^[n].Live,ORD(lc1)) &
               bv.In(LC.ConstsOfNodes^[n].Live,ORD(lc2)) THEN
               RETURN TRUE;
            END;
        END;
        RETURN FALSE;
    END LCLCHooked;

    PROCEDURE LCVarHooked(lc : LC.LCNum; v : ir.VarNum) : BOOLEAN;
    VAR i : ir.TSNode;
        n : ir.Node;
    BEGIN
        IF bv.In(RD.HookedLCVar[lc],ORD(v)) THEN RETURN TRUE; END;
        n := ir.Vars[v].Def.NodeNo;
        IF bv.In (LC.ConstsOfNodes^[n].Live, ORD(lc)) THEN
            RETURN TRUE;
        ELSE
            FOR i:=LC.LongConsts[lc].firstlive TO LC.LongConsts[lc].lastlive DO
                n := ir.Order^[i];
                IF bv.In (LC.ConstsOfNodes^[n].Live, ORD(lc)) &
                   bv.In (Color.LiveAtTop^[n], ORD(v))
                THEN
                    RETURN TRUE;
                END;
            END;
            RETURN FALSE;
        END;
    END LCVarHooked;

    PROCEDURE LCClusterHooked (lc: LC.LCNum; c: ir.INT): BOOLEAN;
    VAR j: ir.INT;
        v : ir.VarNum;
        i :   ir.TSNode;
        n: ir.Node;
    BEGIN
    (*
        FOR i:=0 TO Color.Clusters^[c].N-1 DO
            IF LCVarHooked (lc, Color.Clusters^[c].v^[i]) THEN
                RETURN TRUE;
            END;
        END;
    *)
        FOR i:=LC.LongConsts[lc].firstlive TO LC.LongConsts[lc].lastlive DO
            n := ir.Order^[i];
            IF bv.In (LC.ConstsOfNodes^[n].Live, ORD(lc)) THEN
                FOR j:=0 TO Color.Clusters^[c].N-1 DO
                    v := Color.Clusters^[c].v^[j];
                    IF (n = ir.Vars[v].Def.NodeNo) OR
                       bv.In (Color.LiveAtTop^[n], ORD(v))
                    THEN
                        RETURN TRUE;
                    END;
                END;
            END;
        END;
        RETURN FALSE;
    END LCClusterHooked;

BEGIN
    FOR lc1 := ir.ZEROLCNum TO PRED(LC.NLCs) DO
        IF (RD.LCLoc^[lc1].tag <> BurgNT.NTlconst)
        THEN
            in_FS := LC.LongConsts[lc1].const.tag IN LC.y_Fs;
            bv.Incl(RD.HookedLCLC[lc1],ORD(lc1));
            FOR lc2 := SUCC(lc1) TO PRED(LC.NLCs) DO
                IF (RD.LCLoc^[lc2].tag <> BurgNT.NTlconst)         &
                   (in_FS = (LC.LongConsts[lc2].const.tag IN LC.y_Fs))
                                                              &
                   LCLCHooked(lc1,lc2)
                THEN
                    bv.Incl(RD.HookedLCLC[lc1],ORD(lc2));
                    bv.Incl(RD.HookedLCLC[lc2],ORD(lc1));
                END;
            END;
            FOR c := 0 TO Color.NClusters-1 DO
                IF (RD.Loc^[Color.Clusters^[c].v^[0]].tag <> BurgNT.NTlocal) &
                   (in_FS = (ir.Vars[Color.Clusters^[c].v^[0]].Def.ResType=ir.t_float))
                                                                        &
                   LCClusterHooked (lc1, c)
                THEN
                    FOR i:=0 TO Color.Clusters^[c].N-1 DO
                        bv.Incl(RD.HookedLCVar[lc1],
                                ORD(Color.Clusters^[c].v^[i]));
                    END;
                END;
            END;
            FOR v := Color.NNonTempVars TO PRED(ir.NVars) DO
                IF (RD.Loc^[v].tag <> BurgNT.NTlocal)          &
                   (in_FS = (ir.Vars[v].Def.ResType=ir.t_float))
                                                          &
                   LCVarHooked(lc1,v)
                THEN
                    bv.Incl(RD.HookedLCVar[lc1],ORD(v));
                    bv.Union(RD.HookedLCVar[lc1],RD.HookedVars[v],RD.HookedLCVar[lc1]);
                END;
            END;
        END;
     END;
END LCHooking;

--------------------------------------------------------------------------------

(*
  В скольки линейных участках жива константа?
*)

PROCEDURE CalcNNodes (lc: LC.LCNum): LONGINT;
VAR s: LONGINT;
    i: ir.TSNode;
BEGIN
    s := 0;
    FOR i:=ir.StartOrder TO PRED(LEN(ir.Order^)) DO
        INC(s,ORD(bv.In(LC.ConstsOfNodes^[ir.Order^[i]].Live,ORD(lc))));
    END;
    RETURN s;
END CalcNNodes;

--------------------------------------------------------------------------------

(*
  Нормализовать выгоды от размещения констант на регистрах так, чтобы
  они выглядели точно так же, как и выгоды для переменных, насчитанные
  в Color. Одновременно спасаем реальную выгоду, чтобы ею можно было
  пользоватся при необходимости.
*)

PROCEDURE NormalizeProfits*;
VAR lc: LC.LCNum;
    s:  LONGINT;
BEGIN
    FOR lc:=ir.ZEROLCNum TO PRED(LC.NLCs) DO
        s := LC.LongConsts^[lc].profit;
        LC.LongConsts^[lc].real_profit := s;
        IF s > 0 THEN
            s := (s + ORD(ir.Nnodes) - 1) DIV (CalcNNodes (lc) + 1);
        END;
        LC.LongConsts^[lc].profit := s;
    END;
END NormalizeProfits;

--------------------------------------------------------------------------------

(*
  Пересчитать использования констант
  (без учета использования их в способах адресации)
*)

PROCEDURE RecalcUsesAndProfits*;
VAR lc : LC.LCNum;
BEGIN
    FOR lc:=ir.ZEROLCNum TO PRED(LC.NLCs) DO
        LC.LongConsts^[lc].profit := LC.IDB.InitProf;
    END;
--  Массивы use не обнуляются, т.к. пока что использования только накапливаются
    num.IDB.ProcessNodes (TRUE);
END RecalcUsesAndProfits;

--------------------------------------------------------------------------------

(*
  Вычисляем изначальную выгоду от размещения констант на регистрах,
  побочный эффект - получаем изначальные области их жизни.
*)

PROCEDURE CalcInitialProfitsAndUses*;
BEGIN
    IF LC.NLCs > ir.ZEROLCNum THEN
        RecalcUsesAndProfits;
        NormalizeProfits;
    END;
END CalcInitialProfitsAndUses;

--------------------------------------------------------------------------------

(*
 Следующая процедура занимается в основном цикле iSelect
 перевычислением дескрипторов длинных констант
 (выгод, точек использований, областей жизни и зацепленности).

 - добавляет в атрибут used_in множества узлов, в которых
 используются переменные, размещенные
 IN_LOCAL, (в таких узлах используются константы - базы и смещения),
 перевычисляя и profits.

 - вызывается после того, как RecalcHookedAndProfits
 найдет константы, используемые в различных способах адресации,
 на которые посажены переменные и продлит области жизни первых на
 области жизни последних.
 Сия процедура вычисляет с помощью вверх-вниз замыканий
 области жизни длинных констант и
 зацепленность их между собой и с переменными.
*)

PROCEDURE(idb : LCGen_IDB) RecalcAll*();
BEGIN
  ASSERT(FALSE);
END RecalcAll;

BEGIN
  IDB := NIL;
END LCGen.
