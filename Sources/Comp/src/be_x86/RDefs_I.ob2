MODULE RDefs_I;

IMPORT ir,
       Calc,
       Color,
       at := opAttrs,
       Emit,
       EmitGAS,
       EmitBin,
       CodeDef,
       BitVect,
       RD := RDefs,
       RDD := RDefs_D,
       nts := BurgNT,

<* IF ~ nodebug THEN *>
       opIO,
<* END *>
       SYSTEM,
       opTune,
       xiEnv,
       prc := opProcs;
IMPORT D := desc386;
IMPORT P := p386;
IMPORT reg := reg386;
IMPORT pcK;
IMPORT std := opStd;
IMPORT R := r386;
IMPORT gr := ControlGraph;

<* IF DEFINED(OVERDYE) AND OVERDYE THEN *> -- FIXME
IMPORT od := OverDye;
<* END *>


TYPE
    INT         = ir.INT;
    CARD        = SYSTEM.CARD32;
    VALUE       = ir.VALUE;
    TypeType    = ir.TypeType;
    TriadePtr   = ir.TriadePtr;
    ParamPtr    = ir.ParamPtr;
    Node        = ir.Node;
    Local       = ir.Local;
    VarNum      = ir.VarNum;
--    BitMatrix   = ir.BitMatrix;
    SizeType    = ir.SizeType;
    RegPlacement    = Emit.RegPlacement;
    OffsRecPtr  = Emit.OffsRecPtr;
--    OffsRec     = Emit.OffsRec;
    ProcRecPtr  = Emit.ProcRecPtr;
--    ProcRec     = Emit.ProcRec;
    AddrMode    = Emit.AddrMode;
    Reg         = D.Reg;
    RegSet      = D.RegSet;

-- Этот кусок скопирован из выхода BURG - потом вынести в отдельный модуль -----

    NT = nts.NT;
--CONST stm=      nts.stm;
CONST NTreg=      nts.NTreg;
--CONST rc=       nts.rc;
--CONST mrc=      nts.mrc;
CONST NTmem=      nts.NTmem;
CONST NTbased=    nts.NTbased;
CONST NTscaled=   nts.NTscaled;
CONST NTaddr=     nts.NTaddr;
CONST NTlocal=    nts.NTlocal;
CONST NTtos=      nts.NTtos;
CONST NTconst=    nts.NTconst;
CONST NTimem=      nts.NTimem;
--CONST max_nt=      nts.max_nt;
CONST

    UNDEF_REG    = RDD.UNDEF_REG;
    MINREG       = D.MINREG;
    MAXREG       = D.MAXREG;

TYPE
    DAGNODE = RD.DAGNODE;


--------------------------------------------------------------------------------
TYPE RDefs_386_IDB = POINTER TO RECORD(RD.RDefs_IDB_Rec) END;
VAR IDB : RDefs_386_IDB;

<* IF ~ NODEBUG THEN *>
PROCEDURE(idb : RDefs_386_IDB) PrintRegs;
--VAR i: Reg;
BEGIN
    reg.PrintRegs();
    reg.PrintBusyRegs();
    reg.PrintRegSet(reg.usedSoFar, "Used So Far: ");
(*    FOR i:=MINREG TO MAXREG DO
        IF i IN reg.FreeRegs THEN
            IF reg.RContents [i] <> ir.UNDEFINED THEN
                opIO.print ("  ");
                reg.PrintReg (i);
                opIO.print (": (");
                ir.PrintVar   (reg.RContents [i]);
                opIO.print (")-%d", reg.Counters [i]);
            END;
        ELSIF (i <> D.ESP) & (i <> D.BaseReg) THEN
            opIO.print ("  ");
            reg.PrintReg   (i);
            opIO.print (": ");
            ir.PrintVar   (reg.RContents [i]);
            opIO.print ("-%d", reg.Counters [i]);
        END;
    END;
    opIO.print ("\n");
*)
END PrintRegs;

--------------------------------------------------------------------------------
<* END *>


--------------------------------------------------------------------------------
--
--  Подпрограммы, связанные с пересылками на дугах
--
--------------------------------------------------------------------------------

PROCEDURE(idb : RDefs_386_IDB) FiFreeReg(v: VarNum);
BEGIN
    IF (RD.Loc^[v].tag = NTreg) & (RD.Loc^[v].reg <> UNDEF_REG) THEN
        reg.FreeReg(RD.Loc^[v].reg);
    END;
END FiFreeReg;

--------------------------------------------------------------------------------

PROCEDURE(idb : RDefs_386_IDB) FiMove(v: VarNum; p: ParamPtr);
VAR sz:     SizeType;
     a, ap: AddrMode;
     r:  Reg;
     i:     INT;
     q:     TriadePtr;
     s:     RegSet;
     u:     ParamPtr;
     pos : ir.TPOS;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== RDefs_386_IDB.FiMove ");
    ir.PrintVar (v);
    IF p.tag = ir.y_Variable THEN
        opIO.print(" := ");
        ir.PrintVar (p.name);
    END;
    opIO.print("\n");
    opIO.printLabel;
<* END *>
 <* IF DEFINED(OVERDYE) AND OVERDYE THEN *> -- FIXME
  IF NOT (at.DbgRefine IN at.COMP_MODE) THEN
 <* END *>
    IF Emit.work = Emit.full THEN
        IF NOT p.position.IsNull() THEN
            Emit.work.AddPosition (p.position);
        ELSE
            q := p^.triade;
            LOOP
                pos := gr.FindNextPosition(q);
                IF NOT pos.IsNull() THEN
                  EXIT
                END;
                IF NOT q.Position.IsNull () THEN
                    Emit.work.AddPosition (q.Position);
                    EXIT;
                END;
                IF q^.Params # NIL THEN
                    FOR i:=0 TO LEN(q^.Params^)-2 DO
                       IF NOT q^.Params^[i]^.position.IsNull () THEN
                           Emit.work.AddPosition (q^.Params^[i]^.position);
                           EXIT;
                       END;
                    END;
                END;
                q := q^.Next;
                IF q = NIL THEN
                    EXIT;
                END;
            END;

        END;
    END;
 <* IF DEFINED(OVERDYE) AND OVERDYE THEN *> -- FIXME
  END;
 <* END *>

    IF (p^.tag = ir.y_Variable) AND (ir.popt_LastUse IN p.options) THEN
        reg.FreeReg(RD.Loc[p^.name].reg);
    END;
    sz := ir.Vars^[v].Def^.ResSize;
    CASE RD.Loc^[v].tag OF
    | NTreg:   ASSERT( RD.Loc^[v].reg <> UNDEF_REG );
                r := reg.GR (v, D.allIRegs);
                IF p^.tag = ir.y_Variable THEN
                    R.VarToReg (r, p^.name, sz);
                ELSE
                    R.ParamPtrToReg (r, p, sz);
                END;
                reg.VarInReg (v, r);
    | NTlocal: R.FormMem (a, v);
                IF sz <= 8 THEN
                    CASE p^.tag OF
                    | ir.y_Variable:
                        IF RD.Loc^[p^.name].tag = NTreg THEN
                            Emit.GenMoveM_R (a, RD.Loc^[p^.name].reg);
                            ASSERT( (RD.Loc^[p^.name].reg IN D.SPILLED_REGS)OR
                                    (reg.regContents[RD.Loc^[p^.name].reg]=p^.name));
                            IF ir.popt_LastUse IN p.options THEN
                              reg.FreeReg(RD.Loc[p^.name].reg);
                            END;
                        ELSE
                          IF sz <= 4 THEN
                            r := reg.GTR (D.allowedIRegs [sz]);
                            R.VarToReg (r, p^.name, sz);
                            Emit.GenMoveM_R (a, r);
                            reg.WipeReg(r);
                          ELSE
                            r := reg.GTR (D.allowedIRegs[4]);
                            reg.WipeReg(r);
                            R.FormMem (ap, p^.name);
                            INC (ap.offs, 4);
                            INC (a.offs, 4);
                            Emit.GenMoveR_M (r, ap);
                            Emit.GenMoveM_R (a, r);
                            DEC (ap.offs, 4);
                            DEC (a.offs, 4);
                            Emit.GenMoveR_M (r, ap);
                            Emit.GenMoveM_R (a, r);
                          END;
                        END;
                    | ir.y_RealConst:
--                        IF ir.Vars^[v].Def^.ResType = ir.t_float THEN
                            IF (reg.freeRegs = RegSet{}) OR (at.CPU <= at.i486) THEN
                                r := D.UNDEF_REG;
                            ELSE
                                r := reg.GTR (D.allowedIRegs [4]);
                                reg.WipeReg(r);
                            END;
                            Emit.GenMoveM_FConst (a, p^.value, r, sz);
                    | ir.y_NumConst:
(*
                        ELSIF (* (sz <> 1) & *)
                              NOT (at.SPACE IN at.COMP_MODE) &
                              (at.CPU <> at.i386)
                        THEN
                            r := reg.GTR (D.allowedIRegs [sz], sz);
                            reg.regContents [r] := ir.UNDEFINED;
                            INCL (reg.freeRegs, r);
                            Emit.GenMoveR_INum (r, Emit.GetVal (p, sz), sz);
                            Emit.GenMoveM_R (a, r, sz);
*)
--                        ELSE
                          IF sz <= 4 THEN
                            Emit.work.GenMoveM_INum (a, Emit.GetVal (p, sz), sz);
                          ELSE
                            INC(a.offs,4);
                            Emit.work.GenMoveM_INum (a, p^.value.get_NDWord(1), 4);
                            DEC(a.offs,4);
                            Emit.work.GenMoveM_INum (a, p^.value.get_NDWord(0), 4);
                          END;
  --                      END;
                    | ir.y_AddrConst:
                        IF NOT ir.IsExternal (p^.name) THEN
                            r := reg.GTR (D.allowedIRegs [sz]);
                            R.StackAddrToBased (ap, p);
                            Emit.GenLEA (r, ap);
                            Emit.GenMoveM_R (a, r);
                            reg.WipeReg(r);
                        ELSE
                            R.ParToAddr (ap, p);
                            Emit.work.GenMoveM_Iglobal (a, ap, sz);
                        END;
                    | ir.y_ProcConst:
                        R.ParToAddr (ap, p);
                        Emit.work.GenMoveM_Iglobal (a, ap, sz);
                    | ir.y_Nothing:
                        IF R.MustFillFrame & (sz = 4) THEN
                            Emit.work.GenMoveM_INum (a, 0, 4);
                        END;
                    END;
                ELSE
                    CASE p^.tag OF
                    | ir.y_Variable:
                        ASSERT (RD.Loc^[p^.name].tag = NTlocal);
                        R.FormMem (ap, p^.name);
                        Emit.work.GenMoveTOS_M (ap, 10);
                        R.IncFloats;
                        Emit.work.GenMoveM_TOS (a,  10);
                        DEC (Emit.FloatSize);
                    | ir.y_RealConst:
                        IF (reg.freeRegs = RegSet{}) OR (at.CPU <= at.i486) THEN
                            r := D.UNDEF_REG;
                        ELSE
                            r := reg.GTR (D.allowedIRegs [4]);
                            reg.WipeReg(r);
                        END;
                        Emit.GenMoveM_FConst (a, p^.value, r, sz);
                    | ir.y_Nothing:
                    END;
                END;
    END;
(*
    IF ir.popt_LastUse IN p.Options THEN
      reg.FreeReg();
    END;
*)
END FiMove;

--------------------------------------------------------------------------------

PROCEDURE GenFiXchg (v1, v2: VarNum);
VAR a1, a2: AddrMode;
    s, sz: SizeType;
    r:  Reg;
BEGIN
    ASSERT ((RD.Loc^[v1].tag <> NTreg) OR (RD.Loc^[v1].reg <> UNDEF_REG));
    ASSERT ((RD.Loc^[v2].tag <> NTreg) OR (RD.Loc^[v2].reg <> UNDEF_REG));
    sz := ir.Vars^[v1].Def^.ResSize;
    IF sz < ir.Vars^[v2].Def^.ResSize THEN
        sz := ir.Vars^[v2].Def^.ResSize
    END;
--    ASSERT (sz = ir.Vars^[v2].Def^.ResSize);
    IF RD.Loc^[v1].tag = NTreg THEN
        IF RD.Loc^[v2].tag = NTreg THEN
            Emit.GenXchgR_R (RD.Loc^[v1].reg, RD.Loc^[v2].reg);
            reg.VarInReg (v2, RD.Loc^[v2].reg);
        ELSE
            R.FormMem (a2, v2);
            Emit.GenXchgR_M (RD.Loc^[v1].reg, a2);
        END;
        reg.VarInReg (v1, RD.Loc^[v1].reg);
    ELSIF RD.Loc^[v2].tag = NTreg THEN
        R.FormMem (a1, v1);
        Emit.GenXchgR_M (RD.Loc^[v2].reg, a1);
        reg.VarInReg (v2, RD.Loc^[v2].reg);
    ELSE
        R.FormMem (a1, v1);
        R.FormMem (a2, v2);
        IF sz <= 4 THEN
            r := reg.GTR (D.allowedIRegs [sz]);
            s := sz;
        ELSE
            ASSERT (sz = 8);
            r := reg.GTR (D.allowedIRegs[4]);
            s := 4;
        END;
        Emit.GenMoveR_M (r, a1);
        Emit.GenXchgR_M (r, a2);
        Emit.GenMoveM_R (a1, r);
        IF sz > 4 THEN
            INC (a1.offs, 4);
            INC (a2.offs, 4);
            Emit.GenMoveR_M (r, a1);
            Emit.GenXchgR_M (r, a2);
            Emit.GenMoveM_R (a1, r);
        END;
        reg.WipeReg(r);
    END;
END GenFiXchg;

--------------------------------------------------------------------------------

PROCEDURE(idb : RDefs_386_IDB) FiLoop(p: ir.VarNumArray; n: INT);
VAR i: INT;
BEGIN
    FOR i:=1 TO n-1 DO
        GenFiXchg (p[0], p[i]);
    END;
END FiLoop;

--------------------------------------------------------------------------------

(*
  Корень дерева посчитали; теперь, если он в регистре или в способе адресации,
  увеличить их счетчики на количество использований корня (т.е. переменной) в
  этом участке - 1 (т.к. единица была записана в счетчик изначально).
*)

PROCEDURE(idb : RDefs_386_IDB) SetUsages(p: DAGNODE);
VAR v: VarNum;
BEGIN
    v := p^.tr^.Name;
    CASE p^.nt OF
    | NTreg:
        reg.SetInReg   (v, v, p^.place.r, p^.tr^.NodeNo);
        reg.MarkUseReg (p^.place);
    | NTbased,
      NTscaled,
      NTaddr,
      NTmem:
        IF p^.a.place1.r <> UNDEF_REG THEN
            reg.CheckSpilledReg(p^.a.place1);
            reg.SetInReg   (p^.a.place1.v, v, p^.a.place1.r, p^.tr^.NodeNo);
            reg.MarkUseReg (p^.a.place1);
        END;
        IF p^.a.place2.r <> UNDEF_REG THEN
            reg.CheckSpilledReg(p^.a.place2);
            reg.SetInReg   (p^.a.place2.v, v, p^.a.place2.r, p^.tr^.NodeNo);
            reg.MarkUseReg (p^.a.place2);
        END;
    | ELSE
    END;
END SetUsages;

--------------- Выполнить действия в момент входа в узел

(*
  1. Отметить переменные, которые случайно находятся в
     регистрах во всех предшественников;
  2. Посадить на регистры все живые в момент входа переменные;
     посчитать количество их использования

  3. Отметить позицию первой триады
     AVY: последнее просто закомментарено, так как при оптимизации
     первая триада может вычистится, а для остальных триад смещение
     в сегменте будет не ноль
*)

PROCEDURE(idb : RDefs_386_IDB) EnterNode(n: Node; prologue: BOOLEAN);
VAR i   :INT;
    j: Reg;
    v: VarNum;
    --p: TriadePtr;
--    bv : BitVect.BitVector;
BEGIN
    Emit.PushSize := 0;
    reg.last_find_reg  := D.UNDEF_REG;
    IF (n <> ir.ZERONode) OR prologue THEN
        <* IF ~ NODEBUG THEN *>
        opIO.print ("\n***************** Enter node %d **************************\n", n);
        opIO.printLabel;
        <* END *>
        Emit.work.EnterNode (n, RD.NodeInfo^[n].sg);
       <* IF DEFINED(OVERDYE) AND OVERDYE THEN *> -- FIXME
        IF at.DbgRefine IN at.COMP_MODE THEN
          Emit.AddPosition (ir.Nodes[n].Position);
        END;
       <* END *>
--         Emit.AddPosition (gr.FindNextPosition(ir.Nodes[n].First));
    END;
<* IF ~NODEBUG THEN *>
    IF opIO.needed AND(at.GENASM IN at.COMP_MODE) THEN
      Emit.trace.EnterNode (n, RD.NodeInfo^[n].sg);
    END;
    IF NOT prologue AND(at.GENASM IN at.COMP_MODE)
      & (opIO.needed OR (Emit.work=Emit.full))AND xiEnv.config.Option("asmnode") THEN
      EmitGAS.Text("#Node ");
      EmitGAS.Number(n);
      EmitGAS.Enter;
    END;
<* END *>
    reg.ResetRegisters();

    IF (ir.NVars#0)&(n <> ir.ZERONode) THEN
(*
  Отметить случайно находящиеся на регистрах на входе переменные,
  если в регистр со всех входов приежзает одна и та же переменная
*)
        FOR j:=MINREG TO MAXREG DO
            v := RD.NodeInfo^[ir.Nodes^[n].In^[0]].ultimateRegContents[j];
            IF (v <> ir.UNDEFINED)
              &~(j IN RD.NodeInfo^[ir.Nodes^[n].In^[0]].ultimateDirtyRegs)
            THEN
                i:=1;
                LOOP
                    IF i = ir.Nodes^[n].NIn THEN
                        reg.OccupyReg(j);
                        reg.MarkVarInReg( v, j);
--                        reg.RecalcDirties;
                        reg.FreeReg(j);
                        EXIT;
                    ELSIF RD.NodeInfo^[ir.Nodes^[n].In^[i]].ultimateRegContents[j] <> v THEN
                        EXIT;
                    END;
                    INC (i);
                END;
            END;
        END;
(*
  Отметить живые переменные
*)
--        bv := Color.LiveAtTop^[n];
        IF Color.LiveAtTopArray[n] # NIL THEN
          FOR i:=0 TO LEN(Color.LiveAtTopArray[n]^)-1 DO
              v := Color.LiveAtTopArray[n][i];
              IF (RD.Loc^[v].tag <> NTlocal)
--               & BitVect.In (bv, ORD(v))
              THEN
                  CASE RD.Loc^[v].tag OF
                  | NTreg:
                      reg.SetInReg (v, v, RD.Loc^[v].reg, n);
                  | NTbased:
                      IF RD.Trees^[v]^.a.place1.r <> UNDEF_REG THEN
                          reg.SetInReg (RD.Trees^[v]^.a.place1.v, v, RD.Trees^[v]^.a.place1.r, n);
                      END;
                  | NTscaled:
                      IF RD.Trees^[v]^.a.place2.r <> UNDEF_REG THEN
                          reg.SetInReg (RD.Trees^[v]^.a.place2.v, v, RD.Trees^[v]^.a.place2.r, n);
                      END;
                  | NTaddr,
                    NTmem:
                      IF RD.Trees^[v]^.a.place1.r <> UNDEF_REG THEN
                          reg.SetInReg (RD.Trees^[v]^.a.place1.v, v, RD.Trees^[v]^.a.place1.r, n);
                      END;
                      IF RD.Trees^[v]^.a.place2.r <> UNDEF_REG THEN
                          reg.SetInReg (RD.Trees^[v]^.a.place2.v, v, RD.Trees^[v]^.a.place2.r, n);
                      END;
                  | ELSE
                  END;
              END;
          END;
        END;
    END;
    reg.usagesLeft [D.ESP]     := reg.HUGE;
    reg.usagesLeft [Emit.baseReg] := reg.HUGE;
    reg.usagesLeft [D.UNDEF_REG] := reg.HUGE;
<* IF ~ NODEBUG THEN *>
    IF opIO.needed THEN
        IDB.PrintRegs;
        reg.PrintRegSet (reg.freeRegs, "Free regs: ");
        reg.PrintRegSet (reg.dirtyRegs, "Dirty regs: ");
        reg.PrintRegSet (reg.busyRegs, "Busy regs: ");
        reg.PrintRegSet(reg.usedSoFar, "Used So Far: ");
    END;
<* END *>

(* 3. Отметить позицию первой триады
     AVY: последнее просто закомментарено, так как при оптимизации
     первая триада может вычистится, а для остальных триад смещение
     в сегменте будет не ноль

    IF NOT prologue & (Emit.work = D.Full) THEN
        p := ir.Nodes^[n].First;
        LOOP
            IF Emit.AddTriadePosition (p) THEN
                EXIT;
            END;
            p := p^.Next;
            IF p = NIL THEN
                EXIT;
            END;
        END;
    END;
*)
END EnterNode;

--------------- Выполнить какие-нибудь действия в конце узла

PROCEDURE(idb : RDefs_386_IDB) ExitNode(n: Node);
BEGIN
    IF (ir.Nodes^[n].Last^.Op <> ir.o_error) &
       (ir.Nodes^[n].Last^.Op <> ir.o_stop)
    THEN
        R.AdjustSP;
    END;
    ASSERT (Emit.FloatSize = 0);
    RD.NodeInfo^[n].ultimateRegContents := reg.regContents;
    RD.NodeInfo^[n].ultimateDirtyRegs  := reg.dirtyRegs;
<* IF ~ NODEBUG THEN *>
    IF opIO.needed THEN
        reg.PrintRegSet (reg.dirtyRegs, "Dirty regs: ");
    END;
<* END *>
END ExitNode;

--------------------------------------------------------------------------------

PROCEDURE GetRegForFiResult(v: ir.VarNum);
VAR
     q:     TriadePtr;
     s:     RegSet;
     sz:    SizeType;
     r, fr:     Reg;
     i:     INT;
     u:     ParamPtr;
BEGIN
    sz := ir.Vars^[v].Def^.ResSize;
    q := ir.Vars^[v].Def;
    s := reg.freeRegs * D.allowedIRegs [sz] - reg.HookedRegs (v);
    IF (ir.Vars^[v].Use <> NIL) &
       (ir.Vars^[v].Use^.triade.Op = ir.o_retfun) &
       NOT (ir.o_LiveAtCall IN ir.Vars^[v].Options) &
       (D.RegTable[D.EAXp,sz] IN s)
    THEN
        r := reg.GetPreferredReg (D.RegTable[D.EAXp,sz], v, TRUE, FALSE);
    ELSE
        LOOP
            FOR i:=0 TO LEN(q^.Params^)-1 DO
                u := q^.Params^[i];
                IF (u^.tag = ir.y_Variable) &
                   (RD.Loc^[u^.name].tag = NTreg) &
                   (RD.Loc^[u^.name].reg <> UNDEF_REG) &
                   ~(RD.Loc^[u^.name].reg IN D.SPILLED_REGS) &
                   (RD.Loc^[u^.name].reg IN s)
                THEN
                    r := RD.Loc^[u^.name].reg;
                    reg.SetLoc (v, r);
                    EXIT;
                END;
            END;
            r := reg.GR (v, D.allIRegs, FALSE);
            EXIT;
        END;
    END;
--    reg.FreeReg (r);
END GetRegForFiResult;
--------------------------------------------------------------------------------

(*
  Лежат ли две переменных в одном месте?
  Специально обработать случай, когда они пересекаются по регистрам -
  надо сбросить одну из них в память.
*)
PROCEDURE(idb : RDefs_386_IDB)
         SameMemory(v : ir.VarNum; p : ir.ParamPtr) : BOOLEAN;
BEGIN
    IF (RD.Loc^[v].tag = NTreg)AND(RD.Loc^[v].reg = D.UNDEF_REG) THEN
      GetRegForFiResult(v);
      ASSERT(RD.Loc^[v].reg # D.UNDEF_REG, v);
    END;
    RETURN (p^.tag = ir.y_Variable) &
            ((Color.Allocation^[p^.name].Location =
              Color.Allocation^[v].Location) &
             (Color.Allocation^[p^.name].Offset =
              Color.Allocation^[v].Offset)
             OR
             (RD.Loc^[v].tag = NTreg) & (RD.Loc^[p^.name].tag = NTreg) &
             (RD.Loc^[v].reg = RD.Loc^[p^.name].reg)&~(RD.Loc^[v].reg IN D.SPILLED_REGS))
END SameMemory;

PROCEDURE(idb : RDefs_386_IDB)
         IntersectMemory(v : ir.VarNum; p : ir.ParamPtr) : BOOLEAN;
BEGIN
    RETURN (p^.tag = ir.y_Variable) &
            ((Color.Allocation^[p^.name].Location =
              Color.Allocation^[v].Location) &
             (Color.Allocation^[p^.name].Offset =
              Color.Allocation^[v].Offset)
             OR
             (RD.Loc^[v].tag = NTreg) & (RD.Loc^[p^.name].tag = NTreg) &
             ~(RD.Loc^[v].reg IN D.SPILLED_REGS) &
             (RD.Loc^[v].reg IN D.RegAndIntersectWith[RD.Loc^[p^.name].reg]))
END IntersectMemory;

PROCEDURE(idb : RDefs_386_IDB) InitOutput;
BEGIN
  Emit.work.InitEmitter;
  IF at.GENASM IN at.COMP_MODE THEN
      EmitGAS.InitOutput;
  ELSE
      EmitBin.InitOutput;
  END;

END InitOutput;

BEGIN
    NEW(IDB);
    RD.IDB := IDB;

END RDefs_I.
