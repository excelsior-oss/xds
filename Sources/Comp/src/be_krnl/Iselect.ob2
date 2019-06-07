MODULE Iselect;

(*
  Головной модуль собственно генератора кода:
  - обход DAG'а с вызовом разметчика
  - обход DAG'а с вызовом reducer'а
*)

IMPORT EXCEPTIONS;

IMPORT Memory;
IMPORT lh := LocalHeap;
IMPORT BitVect;

IMPORT ir;
IMPORT gr := ControlGraph;
IMPORT Color;
IMPORT Optimize;
IMPORT RD := RDefs;
IMPORT DAG;

<* IF ~ nodebug  THEN *>
IMPORT opIO;
<* END *>
IMPORT SYSTEM;
IMPORT nts := BurgNT;

TYPE
  Iselect_IDB     *= POINTER TO Iselect_IDB_Rec;
  Iselect_IDB_Rec *= RECORD END;
VAR
  IDB *: Iselect_IDB;


(******************************************************************************)
(*                                                                            *)
(*                     Первый этап - разметка DAG'а                           *)
(*                                                                            *)
(******************************************************************************)

--------------- Разметка одного узла при рекурсивной разметке дерева

PROCEDURE(idb : Iselect_IDB) label(p: RD.DAGNODE);
BEGIN
  ASSERT(FALSE);
END label;

--------------- Разметка всего DAG'а процедуры методом Pattern Matching

<* IF ~nodebug THEN *>
PROCEDURE(idb : Iselect_IDB) LabelDAG_Print(p : RD.DAGNODE);
BEGIN
  ASSERT(FALSE);
END LabelDAG_Print;
<* END *>

(*  вычисление целевого нетерминала дла переменных после разметки *)
PROCEDURE(idb : Iselect_IDB) CalcNonTerm(v : ir.VarNum; p : RD.DAGNODE);
BEGIN
  ASSERT(FALSE);
END CalcNonTerm;

PROCEDURE (idb : Iselect_IDB) CalcNonRootVarsTags(p: RD.DAGNODE;
                                                     goalnt: nts.NT;
                                                     nest_level: INTEGER);
BEGIN
  ASSERT(FALSE);
END CalcNonRootVarsTags;

(* разметка всего DAG *)
PROCEDURE(idb : Iselect_IDB) LabelDAG*;
VAR i: ir.TSNode;
    p: RD.DAGNODE;
    q: ir.TriadePtr;
BEGIN
  FOR i:=ir.StartOrder TO VAL(ir.TSNode, SYSTEM.PRED(ir.Nnodes)) DO
    p := RD.Dag^[ir.Order^[i]];
<* IF ~NODEBUG THEN *>
    IF opIO.needed THEN
      opIO.print("--------------------------------Node %d--------------- \n",ir.Order^[i]);
    END;
<* END *>
    WHILE p <> NIL DO
      idb.label (p);
      q := p^.tr;
      IF q^.Tag = ir.y_Variable THEN
        idb.CalcNonTerm(q^.Name,p);
      ELSE
        p.nt := nts.NTstm;
      END;
      idb.CalcNonRootVarsTags(p, p.nt, 0);
<* IF ~NODEBUG THEN *>
      IF opIO.needed THEN
        idb.LabelDAG_Print(p);
      END;
<* END *>
      ASSERT(p.cost[p.nt]#MAX(INTEGER), 55555);
      p := p^.next;
    END;
  END;
END LabelDAG;

(******************************************************************************)
(*                                                                            *)
(*               Второй этап - обработка семантических правил                 *)
(*                                                                            *)
(******************************************************************************)

(* Буферы для передачи в GenFies *)
PROCEDURE SameMemory(v : ir.VarNum; p : ir.ParamPtr) : BOOLEAN;
BEGIN
  RETURN RD.IDB.SameMemory(v,p);
END SameMemory;

PROCEDURE IntersectMemory(v : ir.VarNum; p : ir.ParamPtr) : BOOLEAN;
BEGIN
  RETURN RD.IDB.IntersectMemory(v,p);
END IntersectMemory;

PROCEDURE FiMove(v : ir.VarNum; p : ir.ParamPtr);
BEGIN
  RD.IDB.FiMove(v,p);
END FiMove;

PROCEDURE FiLoop(p : ir.VarNumArray; n : LONGINT);
BEGIN
  RD.IDB.FiLoop(p,n);
END FiLoop;

--------------- Семантическая обработка поддерева ранее размеченного дерева

(* reduce дерева с результатом *)
PROCEDURE(idb : Iselect_IDB) ReduceVar(p: RD.DAGNODE);
BEGIN
  ASSERT(FALSE);
END ReduceVar;

(* reduce дерева без результа *)
PROCEDURE(idb : Iselect_IDB) ReduceStm(p: RD.DAGNODE);
BEGIN
  ASSERT(FALSE);
END ReduceStm;

--------------- Семантическая обработка всего DAG'а

PROCEDURE(idb : Iselect_IDB) ReduceDAG*;
VAR i: ir.TSNode;
    n: ir.Node;
    p: RD.DAGNODE;
BEGIN
    FOR i:=ir.StartOrder TO VAL(ir.TSNode, SYSTEM.PRED(ir.Nnodes)) DO
        n := ir.Order^[i];
        RD.IDB.EnterNode (n, FALSE);
        p := RD.Dag^[n];
        WHILE p <> NIL DO
            IF p^.tr^.Tag = ir.y_Variable THEN idb.ReduceVar(p);
            ELSE                              idb.ReduceStm(p);
            END;
            p := p^.next;
        END;
        Color.GenFies (n, FiMove, FiLoop, SameMemory, IntersectMemory);
        RD.IDB.ExitNode (n);
    END;
<*+ O2ADDKWD *>
EXCEPT
<*- O2ADDKWD *>
    IF EXCEPTIONS.IsCurrentSource (RD.source) THEN RETURN; END;
END ReduceDAG;


(******************************************************************************)
(*                                                                            *)
(*                     Вызов всего этого хозяйства                            *)
(*                                                                            *)
(******************************************************************************)

(*
  Попытаться очистить память ДО ИТЕРАЦИЙ
*)

PROCEDURE FreeUnneededMemory*;
VAR i: ir.Node;
BEGIN
    gr.dfArray := NIL;
    gr.dfLen   := NIL;
    FOR i:=ir.ZERONode TO SYSTEM.PRED(ir.Nnodes) DO
        BitVect.Free (ir.Nodes^[i].Dominators);
        BitVect.Free (ir.Nodes^[i].aDominators);
    END;
    Optimize.Consts     := NIL;
    Optimize.NextTriade := NIL;
END FreeUnneededMemory;

(* основная процедура кодогенерации *)
PROCEDURE(idb : Iselect_IDB)
         Generate* (was_float, frame_ptr, was_alloca: BOOLEAN);
BEGIN
  ASSERT(FALSE);
END Generate;

(* Очистить все, связанное с этой процедурой *)
PROCEDURE(idb : Iselect_IDB) ClearAll*;
VAR
  i: ir.TSNode;
  n: ir.Node;
BEGIN
    ir.Nodes   := NIL;
    gr.Arcs    := NIL;
    ir.Vars    := NIL;
    ir.Locals  := NIL;
    RD.Loc        := NIL;
    RD.Trees      := NIL;
    RD.Dag        := NIL;

    IF Color.LiveAtTop # NIL THEN
      FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        BitVect.Free(Color.LiveAtTop^[n]);
      END;
      Color.LiveAtTop    := NIL;
    END;

    IF Color.LiveAtBottom # NIL THEN
      FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        BitVect.Free(Color.LiveAtBottom^[n]);
      END;
      Color.LiveAtBottom    := NIL;
    END;

    Color.Allocation   := NIL;
    Color.Clusters     := NIL;
    Memory.DEALLOCATE_ALL;
    lh.DEALLOCATE_ALL(BitVect.defaultHeap);
END ClearAll;

(* set emitter *)
PROCEDURE InitOutput*;
BEGIN
  RD.IDB.InitOutput;
END InitOutput;

BEGIN
  NEW(IDB);
END Iselect.
