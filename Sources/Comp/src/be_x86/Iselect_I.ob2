<* IF TARGET_68K OR TARGET_RISC OR TARGET_VAX THEN *>
This module can be used only in TARGET_386
<* END *>
MODULE Iselect_I;

IMPORT
<* IF ~nodebug THEN *>
  opIO,
<* END *>
  EXCEPTIONS,
  pcK,
  ir, DAG, Iselect, Color, BitVect, opProcs, opAttrs,
  B := Burg,
  T := BurgTables,
  nts := BurgNT,
  H := Heuristics,
  L := LinkProc,
  reg := reg386,
  Emit,
--commented by kevin
  EmitRrd, Rrd2Bin,
  xiEnv,
  R := r386, RD := RDefs;
IMPORT SYSTEM;
IMPORT at := opAttrs; -- *shell
<* IF ~NODEBUG THEN *>
IMPORT TestIO;
<* END *>

IMPORT P := p386;
TYPE
  Iselect_386_IDB = POINTER TO RECORD(Iselect.Iselect_IDB_Rec) END;
VAR
  IDB : Iselect_386_IDB;
  kids_reshuffle : BOOLEAN;
  unstrict:BOOLEAN;

<* IF ~nodebug THEN *>

PROCEDURE DebugReduce(p: RD.DAGNODE; goalnt: nts.NT; nest_level: INTEGER);
VAR eruleno: nts.Rule;
    i:  LONGINT;
    index : T.Index;
    kids:    ARRAY T.MAXNKIDS OF RD.DAGNODE;
BEGIN
    eruleno := p.rule[ goalnt ];
    index     := T.NTnts [eruleno];
    T.NTkids (p, eruleno, kids);
(*
    IF (p^.tr <> NIL) & (p^.tr^.Tag = ir.y_Variable)&(RD.Loc[p^.tr^.Name].temp) THEN
      RD.Loc[p^.tr^.Name].tag := goalnt;
    END;
*)
    i := 0;
    IF (~kids_reshuffle) OR (p.op # ir.o_par)&(p.tr.ResType=ir.t_float) THEN
        WHILE T.NTnts_n [SYSTEM.SUCC(index,i)] # nts.NTnowhere DO
            DebugReduce (kids [i], T.NTnts_n [SYSTEM.SUCC(index,i)], nest_level +1);
            INC (i);
        END;
    ELSE
        WHILE T.NTnts_n [SYSTEM.SUCC(index,i)] # nts.NTnowhere DO
            IF kids [i].op # ir.o_par THEN
              DebugReduce (kids [i], T.NTnts_n [SYSTEM.SUCC(index,i)], nest_level +1);
            END;
            INC (i);
        END;
        i := 0;
        WHILE T.NTnts_n [SYSTEM.SUCC(index,i)] # nts.NTnowhere DO
            IF kids [i].op = ir.o_par THEN
              DebugReduce (kids [i], T.NTnts_n [SYSTEM.SUCC(index,i)], nest_level +1);
            END;
            INC (i);
        END;
    END;
    IF opIO.needed THEN
        opIO.print ("LabelRule %3d= %-30s \t", eruleno, T.RuleNames[eruleno]);
        FOR i := 0 TO nest_level DO
            opIO.print("   ");
        END;
        DAG.PrintOp (p);
        IF (p.tr # NIL)&(p.tr^.Tag = ir.y_Variable) THEN
          IF (RD.Loc[p.tr.Name].tag # goalnt)&(kids[0]#p) THEN
            opIO.print("$$$$");
          END;
        END;
        opIO.print("\n");
    END;
END DebugReduce;

PROCEDURE(idb : Iselect_386_IDB) LabelDAG_Print(p : RD.DAGNODE);
VAR
  q  : ir.TriadePtr;
  v  : ir.VarNum;
  tag: nts.NT;
BEGIN
  opIO.print("\n");
  q := p^.tr;
  IF q^.Tag = ir.y_Variable THEN
    v   := q^.Name;
    tag := RD.Loc^[v].tag;
  ELSE
    tag := nts.NTstm;
  END;

  IF p^.cost[tag] = MAX (INTEGER) THEN
          opIO.print ("¤_а_ўR а ┐_вЁвм -_ г¤ <Rбм:\n");
          opIO.print ("RDD.IN_REG = %d\n", p^.cost[nts.NTreg]);
          opIO.print ("RDD.IN_LOCAL=%d\n", p^.cost[nts.NTlocal]);
          opIO.print ("RDD.IN_MEM = %d\n", p^.cost[nts.NTmem]);
          opIO.print ("RDD.IN_BASED=%d\n", p^.cost[nts.NTbased]);
          opIO.print ("RDD.IN_SCALED=%d\n",p^.cost[nts.NTscaled]);
          opIO.print ("RDD.IN_ADDR= %d\n", p^.cost[nts.NTaddr]);
          opIO.print ("RDD.IN_TOS = %d\n", p^.cost[nts.NTtos]);
          DAG.PrintTree (p, 0);
          ASSERT(FALSE);
  END;

  IF q^.Tag = ir.y_Variable THEN
    DebugReduce(p, tag, 0);
    opIO.print ("Var %d: %s\n\n", v, nts.NTName[tag]);
  ELSE
    DebugReduce(p, nts.NTstm, 0);
    opIO.print ("\n");
  END;
END LabelDAG_Print;
<* END *>

--------------- Разметка одного узла при рекурсивной разметке дерева

PROCEDURE(idb : Iselect_386_IDB) label(p: RD.DAGNODE);
VAR q: B.newstate_proc;
    i : nts.NT;
BEGIN
    CASE T.NTarity [p^.op] OF
    | 0:
    | 1:    idb.label (p^.l);
    | 2:    idb.label (p^.l);
            idb.label (p^.r);
    END;
<* IF TRACE THEN *> B.NTnp := p; <* END *>
    q := B.newstates [ORD(p^.op)];
<* IF ~ NODEBUG THEN *>
    IF opIO.needed THEN
      opIO.printLabel;
    END;
<* END *>
    q (p);
    FOR i := SYSTEM.SUCC(MIN(nts.NT)) TO MAX(nts.NT) DO
      IF p.forbidden[i] THEN
        p.cost[i] := MAX(INTEGER);
      END;
    END;
    IF p.op # ir.o_par THEN
--      IF (p.tr.Tag = ir.y_Variable) AND (RD.Loc[p.tr.Name].tag # nts.NTreg) THEN

--      IF (p.tr.Tag = ir.y_Variable) AND (RD.Loc[p.tr.Name].isSpilled) THEN
--        p.cost[nts.NTreg] := MAX(INTEGER);
--      END;
    END;
<* IF ~ NODEBUG THEN *>
    IF opIO.needed THEN
        opIO.print (" Label %10s ", TestIO.TriadeName[p^.op]);
        FOR i := SYSTEM.SUCC(MIN(nts.NT)) TO MAX(nts.NT) DO
            opIO.print ("%7s%3d:%5d, ", nts.NTName[i] ,p.rule[i], p.cost[i]);
        END;
        DAG.PrintOp (p);
        opIO.print ("\n");
    END;
<* END *>
END label;

PROCEDURE (idb : Iselect_386_IDB) CalcNonRootVarsTags(p: RD.DAGNODE;
                                                     goalnt: nts.NT;
                                                     nest_level: INTEGER);
VAR eruleno: nts.Rule;
    i:  LONGINT;
    index : T.Index;
    kids:    ARRAY T.MAXNKIDS OF RD.DAGNODE;
BEGIN
    eruleno := p.rule[ goalnt ];
    ASSERT(eruleno#0, 57777);
    index     := T.NTnts [eruleno];
    T.NTkids (p, eruleno, kids);
    IF (p^.tr <> NIL)
        & (p.tr.Tag = ir.y_Variable)
        AND RD.Loc[p^.tr^.Name].temp
        AND ~RD.Loc[p.tr.Name].isSpilled THEN
--    IF (p^.tr <> NIL) & (p^.tr^.Tag = ir.y_Variable)&(RD.Loc[p^.tr^.Name].temp(*>=Color.NNonTempVars*)) THEN
      RD.Loc[p^.tr^.Name].tag := goalnt;
<* IF ~NODEBUG THEN*>
      IF opIO.needed THEN
        opIO.print("Marked var %d as %s\n", p^.tr^.Name, nts.NTName[goalnt]);
      END;
<* END *>
    END;
    i := 0;
    WHILE T.NTnts_n [SYSTEM.SUCC(index,i)] # nts.NTnowhere DO
        idb.CalcNonRootVarsTags (kids [i], T.NTnts_n [SYSTEM.SUCC(index,i)], nest_level +1);
        INC (i);
    END;
END CalcNonRootVarsTags;

(*  вычисление целевого нетерминала дла переменных после разметки *)
PROCEDURE(idb : Iselect_386_IDB) CalcNonTerm(v : ir.VarNum; p : RD.DAGNODE);
BEGIN
  IF (RD.Loc^[v].tag <> nts.NTlocal) &
     (RD.Loc^[v].tag <> nts.NTtos)
  THEN
      RD.Loc^[v].tag := H.GoalNonTerm (p);
      IF ((RD.Loc^[v].tag = nts.NTlocal) & (p.tr^.ResType = ir.t_float)) --OR P.Whole8(p.tr)
      THEN
          DAG.IDB.SetInMem (v);
      END;
  END;
  p.nt := RD.Loc^[v].tag;
END CalcNonTerm;

--------------- Семантическая обработка поддерева ранее размеченного дерева

PROCEDURE Reduce(p: RD.DAGNODE; goalnt: nts.NT; nest_level: INTEGER);
VAR eruleno: nts.Rule;
    i:  LONGINT;
    index : T.Index;
    kids:    ARRAY T.MAXNKIDS OF RD.DAGNODE;
BEGIN
    eruleno := p.rule[ goalnt ];
    B.NTactions (-eruleno, p);
    index     := T.NTnts [eruleno];
    T.NTkids (p, eruleno, kids);
    i := 0;
    IF ~kids_reshuffle OR (p.op # ir.o_par)&(p.tr.ResType=ir.t_float)THEN
        WHILE T.NTnts_n [SYSTEM.SUCC(index,i)] # nts.NTnowhere DO
            Reduce (kids [i], T.NTnts_n [SYSTEM.SUCC(index,i)], nest_level +1);
            INC (i);
        END;
    ELSE
        WHILE T.NTnts_n [SYSTEM.SUCC(index,i)] # nts.NTnowhere DO
            IF kids [i].op # ir.o_par THEN
              Reduce (kids [i], T.NTnts_n [SYSTEM.SUCC(index,i)], nest_level +1);
            END;
            INC (i);
        END;
        i := 0;
        WHILE T.NTnts_n [SYSTEM.SUCC(index,i)] # nts.NTnowhere DO
            IF kids [i].op = ir.o_par THEN
              Reduce (kids [i], T.NTnts_n [SYSTEM.SUCC(index,i)], nest_level +1);
            END;
            INC (i);
        END;
    END;
<* IF ~ NODEBUG THEN *>
    IF opIO.needed THEN
        opIO.print("\n");
        opIO.print (" Rule %3d= %-30s \tps%d\t", eruleno,
                                                     T.RuleNames[eruleno],
                                                     Emit.PushSize);
        FOR i := 0 TO nest_level DO
            opIO.print("   ");
        END;
        DAG.PrintOp (p);
        opIO.print("\n");
        RD.IDB.PrintRegs;
        reg.PrintFreeRegs;
        reg.PrintRegSet(reg.usedSoFar, "usedSoFar: ");
        reg.PrintRegSet(reg.dirtyRegs, "dirtyRegs: ");
    END;
<* END *>
    IF ~unstrict THEN
      reg.CheckIntegrity(reg.freeRegs);
      reg.CheckIntegrity(reg.usedSoFar);
      reg.CheckRegContents;
    END;
--    IF  (p^.p <> NIL) & ( NOT Stm OR (ir.Nodes^[p^.p.NodeNo].First.Op # ir.o_fi)) & (p^.p^.Op <> ir.o_getpar) THEN
    IF  (p^.tr <> NIL) & (p^.tr.Op # ir.o_goto) & (p^.tr^.Op # ir.o_getpar) THEN
        Emit.TryAddPosition (p^.tr);
    END;
<* IF ~ NODEBUG THEN *>
    IF opIO.needed THEN
      opIO.printLabel;
    END;
<* END *>
    B.NTactions (eruleno, p);
<* IF ~ NODEBUG THEN *>
    opIO.print("\t\t *%s\n",nts.NTName[p.nt]);
<* END *>
END Reduce;

PROCEDURE(idb : Iselect_386_IDB) ReduceVar(p : RD.DAGNODE);
BEGIN
<* IF ~ NODEBUG THEN *>
  IF opIO.needed THEN
      opIO.print ("---------------------\n\t\t\t\t\t\t\t%3d: %s",
             p^.tr^.Name, nts.NTName[RD.Loc^[p^.tr^.Name].tag] );
      DAG.PrintOp (p);
      opIO.print("\n");
--      RD.IDB.PrintRegs;
      opIO.print ("PushSize=%d, LocalsOffset=%d",Emit.PushSize, Emit.LocalsOffset);
  END;
<* END *>
  Reduce (p, RD.Loc^[p^.tr^.Name].tag, 0);
  RD.IDB.SetUsages(p);
END ReduceVar;

PROCEDURE(idb : Iselect_386_IDB) ReduceStm(p : RD.DAGNODE);
BEGIN
<* IF ~ NODEBUG THEN *>
  IF opIO.needed THEN
      opIO.print ("---------------------\n\t\t\t\t\t\t\t     stm" );
      DAG.PrintOp (p);
      opIO.print("\n");
--      RD.IDB.PrintRegs;
      opIO.print ("PushSize=%d, LocalsOffset=%d",Emit.PushSize, Emit.LocalsOffset);
  END;
<* END *>
  Reduce (p, nts.NTstm, 0);
END ReduceStm;

PROCEDURE(idb : Iselect_386_IDB)
         Generate(was_float, frame_ptr, was_alloca: BOOLEAN);
VAR i: ir.VarNum;
    lis_proc :pcK.STRING;
    v1,v2: ir.VarNum;
    savneed: BOOLEAN;
BEGIN
    kids_reshuffle := TRUE;--xiEnv.config.Option("kids_reshuffle");
    unstrict := xiEnv.config.Option("unstrict_regalloc");
    R.NeverUsePushInCall := xiEnv.config.Option("neverusepushincall");
    Iselect.FreeUnneededMemory;
(*
  Подготовка к генерации
*)
    DAG.IDB.InitLocations;
    reg.InitRegs;
    DAG.MakeDAG;
    IF ir.NVars <> ir.ZEROVarNum THEN
        NEW (RD.HookedVars, ir.NVars);
        FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
            RD.HookedVars^[i] := BitVect.New (ORD(ir.NVars), FALSE);
        END;
    ELSE
        ASSERT(RD.HookedVars = NIL);
    END;

    IF at.OptimizeTraps IN at.COMP_MODE THEN
        NEW (RD.NodeInfo, SYSTEM.SUCC(ir.Nnodes));
    ELSE
        NEW (RD.NodeInfo, ir.Nnodes);
    END;

    R.SetNeedFP (frame_ptr OR was_alloca, was_alloca);
    DAG.IDB.SetLocations;
<* IF ~ NODEBUG THEN *>
    xiEnv.config.Equation( "lis_proc", lis_proc );
    opIO.needed := xiEnv.config.Option("lis") OR
         ( (lis_proc # NIL) & (opAttrs.curr_proc # NIL)
          & (lis_proc^ = opAttrs.curr_proc.name^));
    IF opIO.needed THEN
        DAG.PrintDAG;
        opIO.print ("Hooked variables in Color.ob2:\n");
        FOR v1:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
            ir.PrintVar (v1);
            opIO.print ("\t: ");
            FOR v2:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                IF Color.AreHooked (v1, v2) THEN
                    ir.PrintVar (v2);
                    opIO.print (" ");
                END;
            END;
            opIO.print ("\n");
        END;
    END;
<* END *>

<* IF ~ NODEBUG THEN *>
    IF opIO.needed AND (opAttrs.GENASM IN opAttrs.COMP_MODE) THEN
      Emit.work := Emit.trace;
    ELSE
      Emit.work := Emit.empty;
    END;
<* ELSE *>
    Emit.work := Emit.empty;
<* END *>
(*
  Итерации - раскраска регистров
*)
    reg.Generation := FALSE;
    REPEAT
<* IF ~ NODEBUG THEN *>
        IF opIO.needed THEN
          L.InitGenProc;
        END;
<* END *>
        reg.iteration := FALSE;
        DAG.IDB.ClearLocations (FALSE);
        R.TEMP_SIZE    := 0;
        R.TEMP_START   := 0;
<* IF ~ NODEBUG THEN *>
        IF opIO.needed THEN
          RD.IDB.EnterNode (ir.ZERONode, TRUE);
          R.Prologue (opProcs.NestedUseLocals ());
          opIO.print("Locations before iteration:\n");
          DAG.IDB.PrintLocations();
        END;
<* END *>
        RD.HookedsWith8 := BitVect.New( ir.NVars, FALSE );
        idb.LabelDAG;
<* IF ~ NODEBUG THEN *>
        IF opIO.needed THEN DAG.PrintDAG; END;
<* END *>
        L.ClearUltimateRegContents;
        H.RecalcHookedAndProfits;
<* IF ~ NODEBUG THEN *>
        IF opIO.needed THEN
            FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                opIO.print ("Profit for ");
                ir.PrintVar(i);
                opIO.print("\t= %d\n", RD.Loc^[i].profit);
            END;
            opIO.print ("Number of non-temporary variables: %d\n",Color.NNonTempVars);
            opIO.print ("Hooked variables:\n");
            FOR v1:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                ir.PrintVar (v1);
                opIO.print ("\t: ");
                FOR v2:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                    IF reg.AreHooked (v1, v2) THEN
                        ir.PrintVar (v2);
                        opIO.print (" ");
                    END;
                END;
                opIO.print ("\n");
            END;
        END;
<* END *>
        R.UsedByCalled := opProcs.RegMask{};
        reg.usedSoFar    := opProcs.RegMask{};
        idb.ReduceDAG;
<* IF ~ NODEBUG THEN *>
        IF opIO.needed THEN
          opIO.print("Locations after iteration:\n");
          DAG.IDB.PrintLocations();
        END;
<* END *>
        R.CheckFloats;
    UNTIL NOT reg.iteration;
(*
  Собственно генерация кода
*)
--commented by kevin
    IF opAttrs.DOREORDER IN opAttrs.COMP_MODE THEN
        EmitRrd.InitReordering();
    ELSIF Rrd2Bin.BE <> NIL THEN
        Emit.full := Rrd2Bin.BE;
    END;

<* IF ~ NODEBUG THEN *>
    IF xiEnv.config.Option("printdag") THEN
        savneed := opIO.needed;
        opIO.needed := TRUE;
        DAG.PrintDAG;
        opIO.needed := savneed;
    END;
<* END *>
    Emit.work := Emit.full;
<* IF ~ nodebug THEN *>
    opIO.print("\n!!!!!  FULL EMITTER IS SET AS WORK: %s !!!!!\n\n",
                     opAttrs.curr_proc.name^);
<* END *>

    DAG.IDB.ClearLocations (TRUE);
    (* we believe that the following action is not needed *)
    (*
    idb.LabelDAG;
    *)
    reg.Generation := TRUE;
    L.ClearUltimateRegContents;
    reg.everUsed  := reg.usedSoFar;
    reg.usedSoFar := opProcs.RegMask{};

    FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
      BitVect.Free(Color.HookedVars^[i]);
    END;
    Color.HookedVars := NIL;
    Color.Profits    := NIL;
    L.InitGenProc;
    RD.IDB.EnterNode (ir.ZERONode, TRUE);
    R.Prologue (opProcs.NestedUseLocals ());
    idb.ReduceDAG;
    L.EndGenProc;
    IF ~(pcK.ttag_strictcallconv IN opAttrs.curr_proc.type.tags) THEN
        opProcs.SetBackEndInfo (opAttrs.curr_procno,
                                reg.everUsed + reg.usedSoFar + R.UsedByCalled);
    END;
END Generate;

PROCEDURE(idb : Iselect_386_IDB) ClearAll;
VAR
  i: ir.VarNum;
BEGIN
(*
  Очистить все, связанное с этой процедурой
*)
--<* IF BIN THEN *>
    (**)
-- commented by kevin
    IF opAttrs.DOREORDER IN opAttrs.COMP_MODE THEN
        EmitRrd.CompleteReordering();
    END;
    (**)
--<* END *>
    FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
      BitVect.Free(RD.HookedVars^[i]);
    END;
    RD.HookedVars := NIL;
    RD.NodeInfo   := NIL;
    idb.ClearAll^();
END ClearAll;


BEGIN
  NEW(IDB);
  Iselect.IDB := IDB;
END Iselect_I.
