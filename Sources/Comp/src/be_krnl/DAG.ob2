<* +o2addkwd *>
(* Процедуры построения DAG (и отладочной печати) *)

MODULE DAG;

IMPORT ir;
IMPORT gr := ControlGraph;
IMPORT Calc;
IMPORT Optimize;
IMPORT Color;
IMPORT RD := RDefs;
IMPORT env := xiEnv;
IMPORT at := opAttrs;

IMPORT BurgNT;
<* IF ~ NODEBUG THEN *>
IMPORT  opIO                    -- Отладка
       ,opProcs                 -- Отладка
       ,atr := opAttrs;          -- Отладка
IMPORT TestIO;
IMPORT pc := pcK;
<* END *>
IMPORT SYSTEM;

TYPE
    INT          = ir.INT;
    Node         = ir.Node;
    VarNum       = ir.VarNum;
    TriadePtr    = ir.TriadePtr;
    ParamPtr     = ir.ParamPtr;
    TypeType     = ir.TypeType;
    SizeType     = ir.SizeType;
    Local        = ir.Local;
    DAGNODE      = RD.DAGNODE;

    DAG_IDB    *= POINTER TO DAG_IDB_Rec;
    DAG_IDB_Rec*= RECORD
                  END;

VAR
    IDB *: DAG_IDB;
--------------------------------------------------------------------------------

PROCEDURE(idb : DAG_IDB) MakeLeaf(t : ParamPtr) : DAGNODE;
VAR w: DAGNODE;
BEGIN
    IF t^.tag = ir.y_AddrConst THEN
        ir.Locals^[t^.name].Addressed := TRUE;
    END;
    NEW (w);
    w^.nt      := BurgNT.NTnowhere;
    w^.op      := ir.o_par;
    w^.l       := NIL;
    w^.r       := NIL;
    w^.next    := NIL;
    w^.prev    := NIL;
    w^.par     := t;
    RETURN w;
END MakeLeaf;

PROCEDURE(idb : DAG_IDB) CalcGenericOp(p : TriadePtr) : ir.Operation;
BEGIN
  CASE p^.Op OF
  | ir.o_shl,
    ir.o_shr,
    ir.o_sar,
    ir.o_rol,
    ir.o_ror: RETURN ir.o_shift;
  | ir.o_and,
    ir.o_or,
    ir.o_xor: RETURN ir.o_logical;
  | ELSE      RETURN p^.Op;
  END;
END CalcGenericOp;

PROCEDURE(idb : DAG_IDB) InitNode(p : TriadePtr) : DAGNODE;
VAR q : DAGNODE;
BEGIN
  NEW (q);
  q^.nt := BurgNT.NTnowhere;
  q^.op := IDB.CalcGenericOp(p);
  q^.tr := p;
  q^.sz := p^.ResSize;
  RETURN q;
END InitNode;

PROCEDURE(idb : DAG_IDB) CalcArity(p : TriadePtr) : ir.INT;
BEGIN
  IF p^.Params = NIL THEN
      RETURN 0;
  ELSE
      CASE p^.Op OF
      | ir.o_case:
          RETURN 1;
      | ir.o_move_eq, ir.o_move_le:
          RETURN 2;
      | ELSE
          RETURN LEN (p^.Params^);
      END;
  END;
END CalcArity;

PROCEDURE CheckCorrectness(n: DAGNODE; tr: TriadePtr; parnum: ir.INT);
BEGIN
    ASSERT ( (n^.par = NIL)
            OR ((n^.par.tag # ir.y_Variable )
            OR ((ir.ParamType (tr, parnum) = ir.Vars[n^.par.name].Def.ResType)
                &(n^.sz = ir.Vars[n^.par.name].Def.ResSize))),
            n.par.name);
END CheckCorrectness;

PROCEDURE(idb : DAG_IDB) NeedCallDagging() : BOOLEAN;
BEGIN
  RETURN FALSE;
END NeedCallDagging;

VAR highForest: BOOLEAN;

PROCEDURE MakeNode (n: Node): DAGNODE;
VAR p:            TriadePtr;
    hd, tl, q, s: DAGNODE;
    i:            INT;

    PROCEDURE MakeParam (r: ParamPtr): DAGNODE;
    VAR w: DAGNODE;
    BEGIN
        IF (r^.tag = ir.y_Variable) &
           (highForest OR(r^.name >= Color.NNonTempVars)) &
           (r^.next = NIL) &
           (ir.Vars^[r^.name].Def^.NodeNo = n) &
           (ir.Vars^[r^.name].Use = r) &
           (ir.Vars^[r^.name].Use.next = NIL) &
           (RD.Trees^[r^.name] <> NIL) & (RD.Trees^[r^.name]^.next = NIL)
           -- AVY: but not temporary variables constrained with real variables
           AND NOT ((ir.Vars^[r^.name].LocalNo # ir.TEMPORARY) AND
                    (at.NoRegVars IN at.COMP_MODE))
        THEN
            RD.Loc^[r^.name].temp := TRUE;
            w  := RD.Trees^[r^.name];
            tl := w^.prev;
            IF tl = NIL THEN
                hd := NIL;
            ELSE
                tl^.next := NIL;
            END;
            RETURN w;
        ELSE
            RETURN IDB.MakeLeaf (r);
        END;
    END MakeParam;

    PROCEDURE CallDagging(call    : TriadePtr;
                          pos     : INT) : DAGNODE;
    VAR s  : DAGNODE;
    BEGIN
        NEW(s);
        IF pos = LEN(call^.Params^) THEN
           s^.nt  := BurgNT.NTnowhere;
           s^.op   := ir.o_empty;
           s^.next := NIL;
           s^.prev := NIL;
           s^.tr    := NIL;
           s^.r    := NIL;
           s^.l    := NIL;
        ELSE
           s^.nt  := BurgNT.NTnowhere;
           s^.op   := ir.o_comma;
           s^.next := NIL;
           s^.prev := NIL;
           s^.tr    := call;
           s^.l    := MakeParam (call^.Params^[pos]);
           s^.l.parent := s;
           s^.l^.sz := ir.ParamSize (call, pos);
           IF at.ir_strict IN at.COMP_MODE THEN
              CheckCorrectness(s^.l, call, pos);
           END;
           s^.r    := CallDagging(call,pos+1);
        END;
        RETURN s;
    END CallDagging;

BEGIN
    hd := NIL;
    tl := NIL;
    p := ir.Nodes^[n].First;
    REPEAT
        IF p^.Op = ir.o_fi THEN
            FOR i:=0 TO LEN (p^.Params^)-1 DO
                IF p^.Params^[i].tag = ir.y_AddrConst THEN
                    ir.Locals^[p^.Params^[i].name].Addressed := TRUE;
                END;
            END;
        ELSE
            q := IDB.InitNode(p);
            IF (q^.op = ir.o_call) & IDB.NeedCallDagging() THEN
                q^.l     := MakeParam (p^.Params^[0]);
                q.l.parent := q;
                q^.l^.sz := ir.ParamSize (p, 0);
                IF at.ir_strict IN at.COMP_MODE THEN
                    CheckCorrectness(q^.l, p, 0);
                END;
                q^.r     := CallDagging(p, 1);
            ELSE
                CASE IDB.CalcArity(p) OF
                | 0:
                    q^.r := NIL;
                    q^.l := NIL;
                | 1:
                    q^.r     := NIL;
                    q^.l     := MakeParam (p^.Params^[0]);
                    q.l.parent := q;
                    q^.l^.sz := ir.ParamSize (p, 0);
                    IF at.ir_strict IN at.COMP_MODE THEN
                        CheckCorrectness(q^.l, p, 0);
                    END;
                | 2:
                    IF ir.isCommu IN ir.OpProperties [p^.Op] THEN
                        IF (p^.Params^[0].tag <> ir.y_Variable) OR
                           ((tl <> NIL) &
                            (tl^.tr^.Name = p^.Params^[0].name) &
                            NOT (p^.Params^[1].tag <> ir.y_Variable))
                        THEN
                            q^.r := MakeParam (p^.Params^[0]);
                            q.r.parent := q;
                            q^.r^.sz := ir.ParamSize (p, 0);
                            IF at.ir_strict IN at.COMP_MODE THEN
                                CheckCorrectness(q^.r, p, 0);
                            END;
                            q^.l := MakeParam (p^.Params^[1]);
                            q.l.parent := q;
                            q^.l^.sz := ir.ParamSize (p, 1);
                            IF at.ir_strict IN at.COMP_MODE THEN
                                CheckCorrectness(q^.l, p, 1);
                            END;
                        ELSE
                            q^.r := MakeParam (p^.Params^[1]);
                            q.r.parent := q;
                            q^.r^.sz := ir.ParamSize (p, 1);
                            IF at.ir_strict IN at.COMP_MODE THEN
                                CheckCorrectness(q^.r, p, 1);
                            END;
                            q^.l := MakeParam (p^.Params^[0]);
                            q.l.parent := q;
                            q^.l^.sz := ir.ParamSize (p, 0);
                            IF at.ir_strict IN at.COMP_MODE THEN
                                CheckCorrectness(q^.l, p, 0);
                            END;
                        END;
                    ELSE
                        q^.r := MakeParam (p^.Params^[1]);
                        q.r.parent := q;
                        q^.r^.sz := ir.ParamSize (p, 1);
                        IF at.ir_strict IN at.COMP_MODE THEN
                            CheckCorrectness(q^.r, p, 1);
                        END;
                        q^.l := MakeParam (p^.Params^[0]);
                        q.l.parent := q;
                        q^.l^.sz := ir.ParamSize (p, 0);
                        IF at.ir_strict IN at.COMP_MODE THEN
                            CheckCorrectness(q^.l, p, 0);
                        END;
                    END;
                | 3:
                    ASSERT (p^.Op IN ir.OpSet{ir.o_copy, ir.o_cmpswap, ir.o_error});
                    q^.r := MakeParam (p^.Params^[2]);
                    q.r.parent := q;
                    q^.r^.sz := ir.ParamSize (p, 2);
                    IF at.ir_strict IN at.COMP_MODE THEN
                        CheckCorrectness(q^.r, p, 2);
                    END;
                    NEW (s);
                    s^.nt  := BurgNT.NTnowhere;
                    s^.op   := ir.o_comma;
                    s^.next := NIL;
                    s^.prev := NIL;
                    s^.tr    := p;
                    s^.r    := MakeParam (p^.Params^[1]);
                    s.r.parent := q;
                    s^.r^.sz := ir.ParamSize (p, 1);
                    IF at.ir_strict IN at.COMP_MODE THEN
                        CheckCorrectness(s^.r, p, 1);
                    END;
                    s^.l    := MakeParam (p^.Params^[0]);
                    s.l.parent := q;
                    s^.l^.sz := ir.ParamSize (p, 0);
                    IF at.ir_strict IN at.COMP_MODE THEN
                        CheckCorrectness(s^.l, p, 0);
                    END;
                    q^.l    := s;
                END;
            END;
            IF tl = NIL THEN
                hd      := q;
                q^.prev := NIL;
            ELSE
                tl^.next := q;
                q^.prev  := tl;
            END;
            q^.next := NIL;
            tl := q;
            IF p^.Tag = ir.y_Variable THEN
                RD.Trees^[p^.Name] := q;
            END;
            IF p^.ResType = ir.t_float THEN
                CASE p^.Op OF
                | ir.o_add,
                  ir.o_sub,
                  ir.o_mul,
                  ir.o_dvd,
                  ir.o_rem: q^.op := ir.o_fbin;
                | ir.o_abs,
                  ir.o_sin,
                  ir.o_cos,
                  ir.o_tan,
                  ir.o_atan,
                  ir.o_exp,
                  ir.o_sqrt,
                  ir.o_ln,
                  ir.o_lg,
                  ir.o_neg: q^.op := ir.o_funary;
                | ir.o_eq:  q^.op := ir.o_feq;
                | ir.o_le:  q^.op := ir.o_fle;
                | ELSE
                END;
            END;
        END;
        p := p^.Next;
    UNTIL p = NIL;
    RETURN hd;
END MakeNode;

--------------------------------------------------------------------------------

PROCEDURE MakeDAG*;
VAR i: VarNum;
    t: ir.TSNode;
    n: Node;
BEGIN
    highForest := FALSE;--~env.config.Option("lowforest");
    IF ir.NVars <> ir.ZEROVarNum THEN
        NEW (RD.Trees,  ir.NVars);
        FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
            RD.Trees^ [i] := NIL;
        END;
    ELSE
        RD.Trees  := NIL;
    END;
    NEW (RD.Dag, ir.Nnodes);
    FOR t:=ir.StartOrder TO ir.Nnodes-1 DO
        n := ir.Order^[t];
        RD.Dag^[n] := MakeNode (n);
    END;
END MakeDAG;

--------------------------------------------------------------------------------

<* IF ~ NODEBUG THEN *>

PROCEDURE PrintPar* (r: ParamPtr);
VAR s:    ARRAY 50 OF CHAR;
    name: ir.NameType;
    i:    ir.VarNum;
BEGIN
    CASE r^.tag OF
    | ir.y_Variable:
        IF ir.Vars^[r^.name].LocalNo = ir.TEMPORARY THEN
            opIO.print ("t");
        ELSE
            i := ir.Vars^[r^.name].LocalNo;
            IF ir.Locals[i].Name <> NIL THEN
                opIO.print (ir.Locals[i].Name^);
            ELSE
                opIO.print ("tmp%d", i);
            END;
            opIO.print ("_");
        END;
        opIO.print ("%d", r^.name);
    | ir.y_NumConst:
        r.value.value_to_str (s, pc.flag_c);
        opIO.print ("%s", s);
    | ir.y_RealConst:
        r.value.value_to_str (s, pc.flag_c);
        opIO.print ("%s", s);
    | ir.y_ComplexConst:
        r.value.value_to_str (s, pc.flag_c);
        opIO.print ("%s", s);
    | ir.y_RealVar:
        i := r^.name;
        IF ir.Locals[i].Name <> NIL THEN
            opIO.print ("%s", ir.Locals[i].Name^);
        ELSE
            opIO.print ("tmp%d", i);
        END;
    | ir.y_ProcConst:
        name := opProcs.ProcName (VAL(opProcs.ProcNum, r^.name));
        opIO.print ("%s", name^);
    | ir.y_AddrConst:
        IF r^.offset <> 0 THEN
            opIO.print ("(");
        END;
        opIO.print ("&");
        IF r^.name = MAX (ir.VarNum) THEN
            opIO.print ("CONST");
        ELSE
            i := r^.name;
            IF ir.Locals[i].Name <> NIL THEN
                opIO.print (ir.Locals[i].Name^);
            ELSE
                opIO.print ("tmp%d", i);
            END;
        END;
        IF r^.offset > 0 THEN
            opIO.print ("+");
        END;
        IF r^.offset <> 0 THEN
            opIO.print ("%d)", r^.offset);
        END;
    | ELSE
        opIO.print ("?%d", r^.tag);
    END;
END PrintPar;

PROCEDURE PrintOp* (p: RD.DAGNODE);
BEGIN
    IF (p^.tr <> NIL) & (p^.tr^.Tag = ir.y_Variable) THEN
      opIO.print ("%3d: ", p^.tr^.Name);
      opIO.print ("%7s ", BurgNT.NTName[RD.Loc[p^.tr^.Name].tag]);
    ELSE
      opIO.print ("             ");
    END;

    IF p^.op = ir.o_par THEN
        opIO.print ("<     %d>  ", p^.sz);
    ELSE
        IF p^.tr = NIL THEN RETURN; END;
        IF p^.tr.ResType = ir.t_void THEN
            opIO.print ("<      >  ");
        ELSE
            opIO.print ("<%s%d>  ", ir.TypeName[p^.tr.ResType], p^.sz);
        END;
    END;
    CASE p^.op OF
    | ir.o_eq:          opIO.print ("o_EQ then %d else %d",
                                        ir.Nodes[p.tr.NodeNo].Out[0],
                                        ir.Nodes[p.tr.NodeNo].Out[1]);
    | ir.o_le:          opIO.print ("o_LE then %d else %d",
                                        ir.Nodes[p.tr.NodeNo].Out[0],
                                        ir.Nodes[p.tr.NodeNo].Out[1]);
    | ir.o_goto:        opIO.print ("o_GOTO %d",ir.Nodes[p.tr.NodeNo].Out[0]);
    | ir.o_par:         PrintPar (p^.par);
    ELSE
        opIO.print ("%s",TestIO.TriadeName[p^.op]);
    END;
END PrintOp;

--------------------------------------------------------------------------------
PROCEDURE PrintFi (t: TriadePtr);
VAR i: INT;
BEGIN
    opIO.print ("%3d: ", t^.Name);
    opIO.print ("%s ", BurgNT.NTName[RD.Loc[t^.Name].tag]);
    opIO.print ("<%s%d>  o_FI ( ", ir.TypeName[t^.ResType], t^.ResSize);

    FOR i:=0 TO LEN(t^.Params^)-1 DO
        PrintPar (t^.Params^[i]);
        opIO.print (" ");
    END;
    opIO.print (")\n");
END PrintFi;

PROCEDURE PrintTree* (p: DAGNODE; nest_level: INT);
VAR i: INT;
BEGIN
    IF p.op = ir.o_empty THEN RETURN;  END;
    IF p^.l <> NIL THEN
        PrintTree (p^.l, nest_level + 1);
        IF p^.r <> NIL THEN
            PrintTree (p^.r, nest_level + 1);
        END;
    END;
    IF (p^.tr <> NIL) & (p^.tr^.Tag = ir.y_Variable) THEN
        opIO.print ("%3d: ", p^.tr^.Name);
        opIO.print ("%s\t", BurgNT.NTName[RD.Loc[p^.tr^.Name].tag]);
    ELSE
        opIO.print ("\t\t");
    END;
    FOR i:=1 TO nest_level DO
        opIO.print ("  ");
    END;
    PrintOp (p);
    opIO.print ("\n");
    IF nest_level = 0 THEN
        opIO.print ("\n");
    END;
    ASSERT((p.op = ir.o_par) OR(p.op = ir.o_comma) OR
             ((p.tr#NIL)AND( ((p.tr.Tag=ir.y_Nothing)AND(nest_level=0))
                            OR(p.tr.Tag=ir.y_Variable))
              )) ;
END PrintTree;

--------------------------------------------------------------------------------

PROCEDURE PrintDAG*;
VAR i: ir.TSNode;
    p: DAGNODE;
    t: TriadePtr;
BEGIN
    opIO.print ("\n********* DAG: ");
    IF atr.curr_proc = NIL THEN opIO.print ("BEGIN");
    ELSE opIO.print (atr.curr_proc.name^);
    END;
    opIO.print ("  *********\n");
    FOR i:=ir.StartOrder TO VAL(ir.TSNode, SYSTEM.PRED(ir.Nnodes)) DO
        opIO.print ("------------------------ Node %d\n", ir.Order^[i]);
        t := ir.Nodes^[ir.Order^[i]].First;
        WHILE t^.Op = ir.o_fi DO
            PrintFi(t);
            t := t^.Next;
        END;
        opIO.print ("\n");
        p := RD.Dag^[ir.Order^[i]];
        WHILE p <> NIL DO
            PrintTree (p, 0);
            p := p^.next;
        END;
    END;
END PrintDAG;

<* END *>

--------------------------------------------------------------------------------

PROCEDURE CanPlaceInLocal*(v: VarNum; l: Local): BOOLEAN;
VAR u: VarNum;
BEGIN
    FOR u:=Color.NNonTempVars TO SYSTEM.PRED(ir.NVars) DO
        IF (RD.Loc^[u].tag = BurgNT.NTlocal) & (RD.Loc^[u].mem = l) &
           Color.AreHooked (u, v)
        THEN
            RETURN FALSE;
        END;
    END;
    RETURN TRUE;
END CanPlaceInLocal;

--------------------------------------------------------------------------------

PROCEDURE(idb : DAG_IDB) SetInMem*(i: VarNum);
VAR sz: SizeType;
    tp: TypeType;
    l:  Local;
    j:  ir.VarNum;
    foo: INT;
BEGIN
    RD.Loc^[i].tag := BurgNT.NTlocal;
    IF i < Color.NNonTempVars THEN

        FOR foo:=0 TO Color.Clusters^[Color.Allocation^[i].Cluster].N-1 DO
          RD.Loc^[Color.Clusters^[Color.Allocation^[i].Cluster].v^[foo]].mem  := Color.Allocation^[i].Location;
          RD.Loc^[Color.Clusters^[Color.Allocation^[i].Cluster].v^[foo]].offs := Color.Allocation^[i].Offset;
          RD.Loc^[Color.Clusters^[Color.Allocation^[i].Cluster].v^[foo]].tag := BurgNT.NTlocal;
        END;
        RETURN;
        --        RD.Loc^[i].mem  := Color.Allocation^[i].Location;
        --        RD.Loc^[i].offs := Color.Allocation^[i].Offset;
        --        RETURN;
    END;
    sz := ir.Vars^[i].Def^.ResSize;
    tp := ir.Vars^[i].Def^.ResType;
    RD.Loc^[i].offs := 0;
    IF tp = ir.t_float THEN
        FOR j:=Color.NNonTempVars TO SYSTEM.PRED(ir.NVars) DO
            IF (i <> j) & (RD.Loc^[j].tag = BurgNT.NTlocal) &
                (ir.Vars^[j].Def^.ResType = ir.t_float) &
                (ir.Vars^[j].Def^.ResSize = sz) &
                NOT Color.AreHooked (i, j) &
                CanPlaceInLocal (i, RD.Loc^[j].mem)
            THEN
                RD.Loc^[i].mem := RD.Loc^[j].mem;
                RETURN;
            END;
        END;
    END;
    l := ir.AddLocal (NIL, ir.TmpScope, sz);
    ir.Locals^[l].VarSize := VAL (LONGINT, sz);
    ir.Locals^[l].VarType := tp;
    RD.Loc^[i].mem  := l;
END SetInMem;

--------------------------------------------------------------------------------

(*
  Создать массив с информацией о переменных
*)

PROCEDURE(idb : DAG_IDB) InitLocations*;
VAR i: VarNum;
BEGIN
    IF ir.NVars <> ir.ZEROVarNum THEN
        NEW (RD.Loc, ir.NVars);
        FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
            RD.Loc^[i].temp := FALSE;
            RD.Loc^[i].wasUnited := FALSE;
            RD.Loc^[i].tag := BurgNT.NTreg;
            RD.Loc^[i].reg := RD.UNDEF_REG;
            RD.Loc^[i].usedFor := ir.UNDEFINED;
            IF i<Color.NNonTempVars THEN
              RD.Loc^[i].mem  := Color.Allocation^[i].Location;
              RD.Loc^[i].offs := Color.Allocation^[i].Offset;
            ELSE
              RD.Loc^[i].mem := ir.UNDEFINED;
            END;
        END;
    ELSE
        RD.Loc := NIL;
    END;
END InitLocations;

--------------------------------------------------------------------------------

(*
  Еще до начала первой итерации разместить часть переменных в памяти
*)
PROCEDURE(idb : DAG_IDB) SetLocations*;
BEGIN
  ASSERT(FALSE);
END SetLocations;

--------------------------------------------------------------------------------

(*
  Перед началом итерации очистить разметку регистров; кроме того,
  сбросить в память выгруженные на предыдущем шаге переменные
  (а для вытесненных байтами - установить, что регистры для них
  надо выбирать, начиная со словных)
*)
PROCEDURE(idb : DAG_IDB) ClearLocations*(last_iteration: BOOLEAN);
BEGIN
  ASSERT(FALSE);
END ClearLocations;

PROCEDURE(idb : DAG_IDB) SplitDAG*(root: RD.DAGNODE; branch: DAGNODE);
VAR foo: RD.DAGNODE;
    param:ir.ParamPtr;
    new: RD.DAGNODE;
    i :INT;
BEGIN
  foo := root;
  WHILE (foo.prev # NIL) AND (gr.DominatesTriade(branch.tr, foo.prev.tr)) DO
    foo := foo.prev;
  END;
  -- теперь вставляем перед foo

  IF foo.prev = NIL THEN
    RD.Dag[foo.tr.NodeNo] := branch;
    branch.prev := NIL;
  ELSE
    branch.prev := foo.prev;
    foo.prev.next := branch;
  END;
  branch.next := foo;
  foo.prev := branch;

  param := NIL;
  FOR i := 0 TO LEN(branch.parent.tr.Params^)-1 DO
    IF (branch.parent.tr.Params[i].tag = ir.y_Variable) AND
       (branch.parent.tr.Params[i].name = branch.tr.Name) THEN
      param := branch.parent.tr.Params[i];
    END;
  END;

  new := IDB.MakeLeaf (param);

  IF branch.parent.l = branch THEN
    branch.parent.l := new;
  ELSE
    ASSERT(branch.parent.r = branch);
    branch.parent.r := new;
  END;
  branch.parent := NIL;
  RD.Loc[branch.tr.Name].temp := FALSE;
END SplitDAG;

<* IF ~ NODEBUG THEN *>
PROCEDURE(idb : DAG_IDB) PrintLocations*();
BEGIN
  ASSERT(FALSE);
END PrintLocations;
<* END *>

--------------------------------------------------------------------------------
BEGIN
  NEW(IDB);
END DAG.
