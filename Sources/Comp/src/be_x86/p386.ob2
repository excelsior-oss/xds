MODULE p386;

IMPORT RD := RDefs;
IMPORT nts := BurgNT;
IMPORT ir;
IMPORT Calc;
IMPORT Emit;
IMPORT Color;
IMPORT at := opAttrs;
IMPORT prc := opProcs;
IMPORT D := desc386;
IMPORT pc := pcK;

TYPE NT = nts.NT;
CONST NTnowhere = nts.NTnowhere;
CONST stm = nts.NTstm;
CONST rc = nts.NTrc;
CONST mrc = nts.NTmrc;
CONST mem = nts.NTmem;
CONST reg = nts.NTreg;
CONST based = nts.NTbased;
CONST scaled = nts.NTscaled;
CONST addr = nts.NTaddr;
CONST local = nts.NTlocal;
CONST tos = nts.NTtos;
CONST const = nts.NTconst;
CONST imem = nts.NTimem;

TYPE
    TriadePtr = ir.TriadePtr;
    ParamPtr = ir.ParamPtr;
    TypeType = ir.TypeType;
    VarNum   = ir.VarNum;
    DAGNODE = RD.DAGNODE;
CONST
    LangResultInTos = pc.LangSet{ pc.flag_syscall, prc.Pascal, pc.flag_javacall,
                                  pc.flag_stdcall, pc.flag_vmcall, pc.flag_lightcall
                                };
-------------------------------------------------------
--
--      Условия для применимости правил - вызываются из грамматики
--
--------------------------------------------------------------------------------

(* to suppress no importability of rdefs from e386_s *)
PROCEDURE RD_LocIN_MEM*(v : ir.VarNum) : BOOLEAN;
BEGIN
  RETURN RD.Loc[v].tag = mem;
END RD_LocIN_MEM;

<* PUSH *>
<* WOFF301+ *>
PROCEDURE IsAggregateByValue* (p: TriadePtr): BOOLEAN;
BEGIN
    RETURN FALSE;
END IsAggregateByValue;
<* POP *>

--------------------------------------------------------------------------------

PROCEDURE ParConstNotStackAddr* (p: ParamPtr): BOOLEAN;
BEGIN
    RETURN (p^.tag = ir.y_NumConst)  OR
           (p^.tag = ir.y_RealConst) OR
           (p^.tag = ir.y_ProcConst) OR
           (p^.tag = ir.y_AddrConst) & ir.IsExternal (p^.name);
END ParConstNotStackAddr;

--------------------------------------------------------------------------------

PROCEDURE ConstNotStackAddr* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) & ParConstNotStackAddr (p^.par);
END ConstNotStackAddr;

--------------------------------------------------------------------------------

PROCEDURE ConstReal* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) & (p^.par^.tag = ir.y_RealConst);
END ConstReal;

--------------------------------------------------------------------------------

PROCEDURE ParConstNotVar* (p: ParamPtr): BOOLEAN;
BEGIN
    RETURN (p^.tag = ir.y_NumConst)  OR
           (p^.tag = ir.y_RealConst) OR
           (p^.tag = ir.y_ProcConst) OR
           (p^.tag = ir.y_AddrConst);
END ParConstNotVar;

--------------------------------------------------------------------------------

PROCEDURE ConstNotVar* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) & ParConstNotVar (p^.par);
END ConstNotVar;

--------------------------------------------------------------------------------

PROCEDURE Whole8* (p: TriadePtr):BOOLEAN;
BEGIN
  RETURN (p.ResType IN ir.WholeTypes) & (p.ResSize = 8);
END Whole8;

PROCEDURE Whole8NotConst* (p: DAGNODE):BOOLEAN;
BEGIN
  IF (p^.op = ir.o_par) & (p^.par^.tag = ir.y_NumConst) THEN
    RETURN FALSE;
  END;
  IF p^.op = ir.o_par THEN
    RETURN (p^.par^.tag = ir.y_Variable)&
      (ir.Vars[p^.par^.name].Def.ResSize = 8)&
      (ir.Vars[p^.par^.name].Def.ResType IN ir.WholeTypes);
  ELSE
    RETURN (p.tr.ResType IN ir.WholeTypes) & (p.tr.ResSize = 8);
  END;
END Whole8NotConst;

PROCEDURE Whole8Const* (p: DAGNODE): BOOLEAN;
BEGIN
  IF (p^.op # ir.o_par) OR (p^.par^.tag # ir.y_NumConst) THEN
    RETURN FALSE;
  END;
  RETURN (p^.sz = 8);
END Whole8Const;

PROCEDURE NotWhole8Node* (p: DAGNODE): BOOLEAN;
BEGIN
  RETURN ~Whole8NotConst(p) & ~Whole8Const(p);
END NotWhole8Node;

PROCEDURE Whole8Node* (p: DAGNODE): BOOLEAN;
BEGIN
  RETURN Whole8NotConst(p) OR Whole8Const(p);
END Whole8Node;


--------------------------------------------------------------------------------

PROCEDURE ParConstNotAddr* (p: ParamPtr): BOOLEAN;
BEGIN
    RETURN (p^.tag = ir.y_NumConst) OR (p^.tag = ir.y_RealConst);
END ParConstNotAddr;

--------------------------------------------------------------------------------

PROCEDURE ConstNotAddr* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) & ParConstNotAddr (p^.par);
END ConstNotAddr;

PROCEDURE ConstAddr* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) & (p^.par.tag = ir.y_AddrConst);
END ConstAddr;

--------------------------------------------------------------------------------

PROCEDURE Const248* (p: DAGNODE): BOOLEAN;
BEGIN
    IF (p^.op <> ir.o_par) OR
       (p^.par^.tag <> ir.y_NumConst) OR
       Calc.IsNegative (p^.par^.value, p^.sz)
    THEN
        RETURN FALSE;
    END;
    CASE Calc.ToCardinal (p^.par^.value, p^.sz) OF
    | 2, 4, 8:  RETURN TRUE;
    | ELSE      RETURN FALSE;
    END;
END Const248;

--------------------------------------------------------------------------------

PROCEDURE Const123* (p: DAGNODE): BOOLEAN;
BEGIN
    IF (p^.op <> ir.o_par) OR
       (p^.par^.tag <> ir.y_NumConst) OR
       Calc.IsNegative (p^.par^.value, p^.sz)
    THEN
        RETURN FALSE;
    END;
    CASE Calc.ToCardinal (p^.par^.value, p^.sz) OF
    | 1, 2, 3:  RETURN TRUE;
    | ELSE      RETURN FALSE;
    END;
END Const123;

--------------------------------------------------------------------------------
PROCEDURE Const4* (p: DAGNODE): BOOLEAN;
BEGIN
    IF (p^.op <> ir.o_par) OR
       (p^.par^.tag <> ir.y_NumConst) OR
       Calc.IsNegative (p^.par^.value, p^.sz)
    THEN
        RETURN FALSE;
    END;
    CASE Calc.ToCardinal (p^.par^.value, p^.sz) OF
    | 4:  RETURN TRUE;
    | ELSE      RETURN FALSE;
    END;
END Const4;

--------------------------------------------------------------------------------

PROCEDURE Const359* (p: DAGNODE): BOOLEAN;
BEGIN
    IF (p^.op <> ir.o_par) OR
       (p^.par^.tag <> ir.y_NumConst) OR
       Calc.IsNegative (p^.par^.value, p^.sz)
    THEN
        RETURN FALSE;
    END;
    CASE Calc.ToCardinal (p^.par^.value, p^.sz) OF
    | 3, 5, 9:  RETURN TRUE;
    | ELSE      RETURN FALSE;
    END;
END Const359;

--------------------------------------------------------------------------------

PROCEDURE GoodFloatConst* (p: DAGNODE): BOOLEAN;
VAR w: ir.FLOAT;
BEGIN
    IF (p^.op <> ir.o_par) OR (p^.par^.tag <> ir.y_RealConst) THEN
        RETURN FALSE;
    END;
    w := Calc.ToReal (p^.par^.value, p^.sz);
    RETURN (w = 0.0) OR (w = 1.0) OR
           (p^.sz = 4) & (w = D.Pi4) OR
           (p^.sz = 8) & (w = D.Pi8);
END GoodFloatConst;

--------------------------------------------------------------------------------

PROCEDURE SmallestSize* (p: DAGNODE): ir.SizeType;
BEGIN
    IF (p^.op <> ir.o_par) OR (p^.par^.tag <> ir.y_RealConst) THEN
        RETURN p^.sz;
    END;
    RETURN Emit.SmallestSize (Calc.ToReal (p^.par^.value, p^.sz), p^.sz);
END SmallestSize;

--------------------------------------------------------------------------------

PROCEDURE IsAllOnes* (p: DAGNODE; tp: TypeType): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) & (p^.par^.tag = ir.y_NumConst) &
           Calc.IsOnes (p^.par^.value, tp, p^.sz);
END IsAllOnes;

--------------------------------------------------------------------------------

PROCEDURE IsOne* (par: DAGNODE): BOOLEAN;
BEGIN
    RETURN (par^.op = ir.o_par) & (par^.par^.tag = ir.y_NumConst) &
           NOT Calc.IsNegative (par^.par^.value, par^.sz) &
           (Calc.ToCardinal (par^.par^.value, par^.sz) = 1);
END IsOne;

--------------------------------------------------------------------------------

PROCEDURE IsZero* (sz: Emit.SizeType; par: DAGNODE): BOOLEAN;
BEGIN
    RETURN (par^.op = ir.o_par) & (par^.par^.tag = ir.y_NumConst) &
           Calc.IsZero (par^.par^.value, ir.t_int, sz);
END IsZero;

--------------------------------------------------------------------------------

PROCEDURE IsRealZero* (sz: Emit.SizeType; par: DAGNODE): BOOLEAN;
BEGIN
    RETURN (par^.op = ir.o_par) & (par^.par^.tag = ir.y_RealConst) &
           Calc.IsZero (par^.par^.value, ir.t_float, sz);
END IsRealZero;

--------------------------------------------------------------------------------

PROCEDURE RegVar* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) &
           (p^.par^.tag = ir.y_Variable) &
           (RD.Loc^[p^.par^.name].tag = reg);
END RegVar;

--------------------------------------------------------------------------------

PROCEDURE MemVar* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) &
           (p^.par^.tag = ir.y_Variable) &
           (RD.Loc^[p^.par^.name].tag = mem);
END MemVar;

--------------------------------------------------------------------------------

PROCEDURE LocalVar* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) &
           (p^.par^.tag = ir.y_Variable) &
           (RD.Loc^[p^.par^.name].tag = local);
END LocalVar;

--------------------------------------------------------------------------------

PROCEDURE BasedVar* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) &
           (p^.par^.tag = ir.y_Variable) &
           (RD.Loc^[p^.par^.name].tag = based);
END BasedVar;

--------------------------------------------------------------------------------

PROCEDURE ScaledVar* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) &
           (p^.par^.tag = ir.y_Variable) &
           (RD.Loc^[p^.par^.name].tag = scaled);
END ScaledVar;

--------------------------------------------------------------------------------

PROCEDURE AddrVar* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) &
           (p^.par^.tag = ir.y_Variable) &
           (RD.Loc^[p^.par^.name].tag = addr);
END AddrVar;

--------------------------------------------------------------------------------

PROCEDURE TosVar* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) &
           (p^.par^.tag = ir.y_Variable) &
           (RD.Loc^[p^.par^.name].tag = tos);
END TosVar;

--------------------------------------------------------------------------------

PROCEDURE RootNode* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op <> ir.o_par) &
           (p^.tr^.Tag = ir.y_Variable) &
           NOT RD.Loc^[p^.tr^.Name].temp;
END RootNode;

--------------------------------------------------------------------------------

PROCEDURE InLocal* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op <> ir.o_par) &
           (p^.tr^.Tag = ir.y_Variable) &
           (RD.Loc^[p^.tr^.Name].tag = local);
END InLocal;

--------------------------------------------------------------------------------

PROCEDURE ResultOfPriorTriade* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) &
           (p^.par^.tag = ir.y_Variable) &
--           NOT RD.Loc^[p^.par^.name].temp &
           (RD.Loc^[p^.par^.name].tag = local) &
           (ir.Vars^[p^.par^.name].Def = p^.par^.triade^.Prev);
END ResultOfPriorTriade;

--------------------------------------------------------------------------------

PROCEDURE ConstStackAddr* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op = ir.o_par) &
           (p^.par^.tag = ir.y_AddrConst) &
           NOT ir.IsExternal (p^.par^.name);
END ConstStackAddr;

--------------------------------------------------------------------------------

PROCEDURE SameAlloc* (res, par: DAGNODE): BOOLEAN;
VAR v: VarNum;
BEGIN
    IF (res^.tr^.Tag <> ir.y_Variable) OR
       (res^.tr^.Name >= Color.NNonTempVars)
    THEN
        RETURN FALSE;
    END;
    IF par^.op = ir.o_par THEN
        IF par^.par^.tag <> ir.y_Variable THEN
            RETURN FALSE;
        END;
        v := par^.par^.name;
    ELSE
        v := par^.tr^.Name;
    END;
    RETURN (v < Color.NNonTempVars) & (Color.Allocation^[res^.tr^.Name].Cluster =
                                       Color.Allocation^[v].Cluster);
END SameAlloc;

--------------------------------------------------------------------------------

PROCEDURE SameAddrs* (p, memory: DAGNODE): BOOLEAN;
VAR w: ParamPtr;
BEGIN
    ASSERT (p^.op = ir.o_storer);
    IF memory^.op = ir.o_par THEN
        IF (memory^.par^.tag = ir.y_Variable) &
           (RD.Loc^[memory^.par^.name].tag = mem) &
           (ir.Vars^[memory^.par^.name].Def^.Op = ir.o_loadr)
        THEN
            w := ir.Vars^[memory^.par^.name].Def^.Params^[0];
        ELSE
            RETURN FALSE;
        END;
    ELSIF memory^.op = ir.o_loadr THEN
        w := memory^.tr^.Params^[0];
    ELSE
        RETURN FALSE;
    END;
    RETURN ir.EqParams (p^.tr^.Params^[0], w, p^.tr^.ResType, p^.tr^.ResSize);
END SameAddrs;

--------------------------------------------------------------------------------

PROCEDURE SameMemLoadr* (p, mem: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op <> ir.o_par) & (p^.tr^.Tag = ir.y_Variable) &
           (p^.tr^.Name < Color.NNonTempVars) &
           (mem^.op = ir.o_par) & --(RD.Loc^[p^.tr^.Name].tag = local) &
           (mem^.par^.tag = ir.y_AddrConst) &
           (Color.Allocation^[p^.tr^.Name].Location = mem^.par^.name) &
           (Color.Allocation^[p^.tr^.Name].Offset   = mem^.par^.offset);
END SameMemLoadr;

--------------------------------------------------------------------------------

PROCEDURE SameMemStorer* (addr, mem: DAGNODE): BOOLEAN;
VAR v: ir.VarNum;
BEGIN
    IF mem^.op = ir.o_par THEN
        IF mem^.par^.tag <> ir.y_Variable THEN
            RETURN FALSE;
        END;
        v := mem^.par^.name;
    ELSE
        v := mem^.tr^.Name;
    END;
    RETURN (v < Color.NNonTempVars) & (RD.Loc^[v].tag = local) &
           (addr^.op = ir.o_par) & (addr^.par^.tag = ir.y_AddrConst) &
           (Color.Allocation^[v].Location = addr^.par^.name) &
           (Color.Allocation^[v].Offset   = addr^.par^.offset);
END SameMemStorer;

--------------------------------------------------------------------------------

(*
  Выдать первую триаду поддерева
*)

PROCEDURE FirstTriade (p: TriadePtr): TriadePtr;
VAR r: DAGNODE;
BEGIN
    IF p^.Tag = ir.y_Variable THEN
        r := RD.Trees^[p^.Name];
    ELSE
        r := RD.Dag^[p^.NodeNo];
        WHILE r^.tr <> p DO
            r := r^.next;
        END;
    END;
    IF (r^.l <> NIL) & (r^.l^.op <> ir.o_par) THEN
        RETURN FirstTriade (r^.l^.tr);
    ELSIF (r^.r <> NIL) & (r^.r^.op <> ir.o_par) THEN
        RETURN FirstTriade (r^.r^.tr);
    ELSE
        RETURN p;
    END;
END FirstTriade;

--------------------------------------------------------------------------------

(*
  Можно ли разместить переменную в TOS?
*)

PROCEDURE PlaceInTOS* (v: VarNum): BOOLEAN;
VAR p, q, s: TriadePtr;
    r:    ParamPtr;
BEGIN
    IF v < Color.NNonTempVars THEN
        RETURN FALSE;
    END;
    p := ir.Vars^[v].Def;
    r := ir.Vars^[v].Use;
    IF (r = NIL) OR (r^.next <> NIL) OR (r^.triade^.NodeNo <> p^.NodeNo) THEN
        RETURN FALSE;
    END;
    q := r^.triade;
    s := p^.Next;
    WHILE s <> q DO
        IF (s^.Op = ir.o_call) OR
           (s^.Op = ir.o_val) & (s^.ResType <> ir.t_float) &
                                (s^.OpType  =  ir.t_float)
        THEN
            RETURN FALSE;
        END;
        s := s^.Next;
    END;
    q := FirstTriade (q);
    LOOP
        q := q^.Prev;
        IF q = p THEN
            RETURN TRUE;
        ELSIF (q^.Op <> ir.o_putpar) &
              ((q^.ResType = ir.t_float) OR (q^.OpType = ir.t_float))
        THEN
            IF RD.Loc^[q^.Name].temp OR PlaceInTOS (q^.Name) THEN
                RETURN FALSE;
            END;
            q := FirstTriade (q);
        END;
    END;
END PlaceInTOS;


PROCEDURE IsFloatNode* (p: DAGNODE): BOOLEAN;
BEGIN
  IF p.op = ir.o_par THEN
    RETURN (p.par.tag = ir.y_Variable) & (ir.Vars[p.par.name].Def.ResType = ir.t_float);
  ELSE
    RETURN p.tr.ResType = ir.t_float;
  END;
END IsFloatNode;

--------------------------------------------------------------------------------
PROCEDURE isNodeNode* (p: DAGNODE): BOOLEAN;
BEGIN
    RETURN (p^.op <> ir.o_par) &
           (p^.tr^.Tag = ir.y_Variable) &
           RD.Loc^[p^.tr^.Name].temp;
END isNodeNode;

--------------------------------------------------------------------------------
PROCEDURE CanBePlacedInReg* (node: DAGNODE): BOOLEAN;
BEGIN
  IF isNodeNode(node) AND (RD.Loc[node.tr.Name].tag = local) THEN
    RETURN FALSE;
  END;
  RETURN TRUE;
END CanBePlacedInReg;

-- doesn't enable long lifed compound addrmodes with spillees
PROCEDURE NotDeprecCompoundVar* (node: DAGNODE): BOOLEAN;
BEGIN
  RETURN ~((node.op # ir.o_par) AND
            (node.tr.Tag = ir.y_Variable) AND
            RD.Loc[node.tr.Name].isSpilled AND
            RD.Loc[node.tr.Name].hasLongLifeTime);
END NotDeprecCompoundVar;

--------------------------------------------------------------------------------

PROCEDURE ProtoResultInTOS (pt: prc.ProtoNum; intern: BOOLEAN): BOOLEAN;
BEGIN
    RETURN (prc.ProtoList[pt].ret_type = ir.t_float) &
           ((prc.ProtoList[pt].ret_size > 8) OR
            (at.CC IN D.CCResultInTos) OR
            (at.MC = at.NATIVE) &
            (prc.LangByProto(pt) IN prc.NativeLangs) OR
            (prc.LangByProto(pt) IN LangResultInTos) OR
            intern);
END ProtoResultInTOS;

PROCEDURE CallResultInTOS* (p: ir.TriadePtr): BOOLEAN;
VAR intern: BOOLEAN;
BEGIN
    ASSERT(p.Op = ir.o_call);
    intern := (p.Params[0].tag = ir.y_ProcConst) &
              prc.IsInternal(VAL(prc.ProcNum, p.Params[0].name));
    RETURN ProtoResultInTOS(p.Prototype, intern);
END CallResultInTOS;

PROCEDURE ProcResultInTOS* (p: prc.ProcNum): BOOLEAN;
BEGIN
    RETURN ProtoResultInTOS( prc.ProcProtoNum(p), prc.IsInternal(p) );
END ProcResultInTOS;

PROCEDURE RetvalRegs* (pt: prc.ProtoNum; intern:=FALSE: BOOLEAN): D.RegSet;
BEGIN
  IF ProtoResultInTOS(pt, intern) THEN
    RETURN D.RegSet{D.ST0};
  ELSIF (prc.ProtoList[pt].ret_size = 0) THEN
    RETURN D.RegSet{};
  ELSIF (prc.ProtoList[pt].ret_size = 8) THEN
    RETURN D.RegSet{D.EAX,D.EDX}
  ELSE
    RETURN D.RegSet{D.EAX}
  END;
END RetvalRegs;

PROCEDURE TrashedRegs* (pt: prc.ProtoNum): D.RegSet;
VAR lang: pc.Lang;
BEGIN
  lang := prc.LangByProto(pt);
  IF lang = pc.flag_vmcall THEN
    RETURN RetvalRegs(pt) - D.RegSet{D.ST0};
  ELSIF lang = pc.flag_lightcall THEN
    RETURN D.RegSet{D.EAX,D.EDX,D.ECX,D.EBX,D.ESI,D.EDI,D.EBP};
  ELSE
    RETURN D.RegSet{D.EAX,D.EDX,D.ECX};
  END;
END TrashedRegs;

--------------------------------------------------------------------------------

END p386.