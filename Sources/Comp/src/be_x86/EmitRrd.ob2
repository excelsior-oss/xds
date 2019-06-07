<*-IOVERFLOW*>
<*-COVERFLOW*>

(* Created by KDV;
   object code representation emitter;
   the emitter substitutes EmitBin

*)

MODULE EmitRrd;

IMPORT ir,
       Color,
       ocir,
       def := OcirDef,
       reorder,
       ScheStr,
       r2b := Rrd2Bin,
       FLoops,
       EmitBin,
       EmitGAS,
--     EmitTxt,
       CodeDef,
       BitVect,
       Emit,
       at := opAttrs,
       pc := pcK,
       prc := opProcs;
IMPORT D := desc386;
IMPORT RD := RDefs;
IMPORT SYSTEM;

TYPE
    INT         = ir.INT;
    SizeType    = Emit.SizeType;
    ScaleType  *= D.ScaleType;
    VALUE       = ir.VALUE;
    TriadePtr   = ir.TriadePtr;
    Node        = ir.Node;
    Local       = ir.Local;
    AddrMode    = Emit.AddrMode;
    BitVector   = BitVect.BitVector;
    PhysReg    = D.PhysReg;
    Reg    = D.Reg;
    RegSet = D.RegSet;
--------------------------------------------------------------------------------

TYPE
    SelfRrdPtr *= POINTER TO SelfRrdRec;
    SelfRrdRec *= RECORD (Emit.SelfRec)
                  END;

    LAST_R8_MOV_TYPE = D.PhysReg[D.ALp..D.BLp];
VAR RE*     : SelfRrdPtr;

    LAST_FLAGS_REG:    D.PhysReg;    -- С каким регистром выполнялась последняя
                                    --   команда, устанавливающая флажки
    LAST_FLAGS_SIZE:   SizeType;    -- размер этого регистра
    LAST_FLAGS_OFFSET: LONGINT;     -- смещение этой команды (в текущем узле)
    LAST_FLAGS_OP    : LONGINT;
    LAST_R8_MOV :  ARRAY LAST_R8_MOV_TYPE OF LONGINT; -- номера последних команд
                                    -- movr8,r/m для каждого младшего r8


--------------------------------------------------------------------------------
--
--      Служебные подпрограммы
--
--------------------------------------------------------------------------------

                (* interface with emit registers *)

(* перевести Emit's регистр в OcirDef's регистр,
*)
PROCEDURE R2R(r: D.PhysReg; sz: SizeType) : def.Register;
BEGIN
    IF (sz =1) & (r IN D.PhysRegSet{D.AHp..D.BHp}) THEN RETURN VAL(def.Register,ORD(r)-4);
    ELSE RETURN VAL(def.Register,r);
    END;
END R2R;

PROCEDURE SetLastFlags (op: LONGINT; r:D.PhysReg; sz: SizeType);
BEGIN
    LAST_FLAGS_OP     := op;
    LAST_FLAGS_REG    := r;
    LAST_FLAGS_SIZE   := sz;
    LAST_FLAGS_OFFSET := ocir.c_seg.code_len;
END SetLastFlags;

PROCEDURE ResetLastFlags;
BEGIN
    LAST_FLAGS_REG    := D.UNDEF_REGp;
    LAST_FLAGS_OFFSET := MAX (LONGINT);
    LAST_FLAGS_SIZE   := 0;
END ResetLastFlags;

PROCEDURE SetLastR8Move(r : PhysReg);
BEGIN
    LAST_R8_MOV[r] := ocir.c_seg.code_len-1;
END SetLastR8Move;

--------------------------------------------------------------------------------

VAR GOAddr, MAddr  : AddrMode;(* used for creation of ocir and store last
                                 offset evaluated by CalcOffset
                              *)

(*
  1) Посчитать displacement
  2) Выдать, полное оно или нет
*)

PROCEDURE CalcOffset (VAR offs: INT; a-: AddrMode);
VAR op: Emit.OffsRecPtr;
BEGIN
    offs := a.offs;
    IF a.local <> ir.UNDEFINED THEN
        IF NOT ir.IsExternal (a.local) THEN
            INC (offs, ir.Locals^[a.local].Offset + Emit.LocalsOffset);
            IF Emit.baseReg = D.ESP THEN
                INC (offs, Emit.PushSize);
            END;
        END;
    END;
    op := a.offslist;
    WHILE op <> NIL DO
        IF NOT ir.IsExternal (op^.name) THEN
            INC (offs, ir.Locals^[op^.name].Offset + Emit.LocalsOffset);
            IF Emit.baseReg = D.ESP THEN
                INC (offs, Emit.PushSize);
            END;
        END;
        op := op^.next;
    END;
END CalcOffset;

--------------------------------------------------------------------------------

PROCEDURE GO (a-: AddrMode);
BEGIN
    GOAddr := a;
    CalcOffset (GOAddr.offs, a);
END GO;

--------------------------------------------------------------------------------

PROCEDURE DD (a-: AddrMode);
BEGIN
    IF (a.place1.r = D.EBP) & Emit.DSneqSS &
       (Emit.baseReg <> D.EBP) &
       ((a.local = ir.UNDEFINED) OR ir.IsExternal (a.local)) &
       ((a.place2.r = D.UNDEF_REG) OR
        (a.place2.r = D.EBP) OR
        (a.scale <> D.x1))
    THEN
        ocir.SetPrefix(def.P_DS);
    END;
END DD;

--------------------------------------------------------------------------------

PROCEDURE P (sz: SHORTINT);
BEGIN
    IF sz = 2 THEN ocir.SetPrefix(def.P_OS); END;
END P;
--------------------------------------------------------------------------------

(*
  Выдать способ адресации, вставив в mod-r/m байт-параметр
*)

PROCEDURE M (a-: AddrMode);
BEGIN
    MAddr := a;
    IF (a.place1.r = D.EBP) & (a.place2.r <> D.UNDEF_REG) & (a.scale = D.x1) THEN
        MAddr.place1.r := MAddr.place2.r;
        MAddr.place2.r := D.EBP;
    ELSIF a.place2.r = D.ESP THEN
        ASSERT ((a.place1.r <> D.ESP) & (a.scale = D.x1));
        MAddr.place2.r := MAddr.place1.r;
        MAddr.place1.r := D.EBP;
    END;
    CalcOffset (MAddr.offs, a);
END M;

--------------------------------------------------------------------------------

PROCEDURE Mov2MovzxFull(r:PhysReg; sz: SizeType);
VAR bin : def.BinRecipe;
    op  : def.Operation;
    new_binR_R : r2b.MovezxR_R;
    new_binR_M : r2b.MovezxR_M;
BEGIN
    IF (r  = D.UNDEF_REGp)            OR
       NOT (r IN D.PhysRegSet{D.ALp..D.BLp})    OR
       (LAST_R8_MOV[r] = def.UNDEF_POS) OR
       (sz <> 4)
    THEN
        RETURN
    END;
    op := ocir.c_seg.code[LAST_R8_MOV[r]];
    op.code := def.MOVZX;
    op.attrs.pair  := def.NP;
    op.attrs.clocks:= 3;
    bin := op.bin;
    WITH
      bin : r2b.MoveR_R DO
        NEW(new_binR_R);
        new_binR_R.Fill(bin.d,bin.s,4,1);
        def.SetOpR_R(op, R2R(bin.d,4), R2R(bin.s,1));
        op.bin := new_binR_R;
    | bin : r2b.MovesxR_R DO
        NEW(new_binR_R);
        new_binR_R.Fill(bin.d,bin.s,4,1);
        def.SetOpR_R(op, R2R(bin.d,4), R2R(bin.s,1));
        op.bin := new_binR_R;
    | bin : r2b.MoveR_M DO
        NEW(new_binR_M);
        new_binR_M.Fill(bin.r,bin.a,4,1);
        M (bin.a);
        def.SetOpR_M(op,R2R(bin.r,4), MAddr);
        op.bin := new_binR_M;
    | bin : r2b.MoveR_Iglobal DO
        NEW(new_binR_M);
        new_binR_M.Fill(bin.r,bin.a,4,1);
        M (bin.a);
        def.SetOpR_M(op,R2R(bin.r,4), MAddr);
        op.bin := new_binR_M;
    | bin : r2b.MovesxR_M DO
        NEW(new_binR_M);
        new_binR_M.Fill(bin.r,bin.a,4,1);
        M (bin.a);
        def.SetOpR_M(op,R2R(bin.r,4), MAddr);
        op.bin := new_binR_M;
    END;
    def.AddAssignOpArgRes(op);
    ocir.c_seg.code[LAST_R8_MOV[r]] := op;
    LAST_R8_MOV[r] := def.UNDEF_POS;
END Mov2MovzxFull;

<* PUSH *>
<* WOFF301+ *>
PROCEDURE Mov2MovzxEmpty(r: PhysReg; sz: SizeType);
END Mov2MovzxEmpty;
<* POP *>

VAR Mov2Movzx : PROCEDURE (r: PhysReg; sz: SizeType);

--------------------------------------------------------------------------------
--
--      Вход в линейный участок, координаты, etc...
--
--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) EnterNode* (n: Node; seg: CodeDef.CODE_SEGM);
VAR i : PhysReg;
BEGIN
    ocir.c_seg        := ocir.GetLastSegment(ocir.ProcSegms[n]);
    ocir.InLoop       := ir.Nodes[n].Nesting > 0;
    Emit.PushSize    := 0;
    Emit.FloatSize   := 0;
    ResetLastFlags;
    FOR i := D.ALp TO D.BLp DO LAST_R8_MOV[i] := def.UNDEF_POS; END;
    IF at.CPU = at.iPentiumPro THEN Mov2Movzx := Mov2MovzxFull;
    ELSE Mov2Movzx := Mov2MovzxEmpty;
    END;
END EnterNode;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) Flush*;
   VAR j      : INT;
       i:       ir.TSNode;
       n         : Node;
       cur_pos   : ir.TPOS;
       can_remove: BOOLEAN;
       tmp       : ocir.Segment;

BEGIN
    IF ocir.WasFloatMemInLoop THEN FLoops.FLOptimizeMain END;
    IF (at.CPU >= at.iPentium) OR (at.CPU = at.iGeneric) THEN
        FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
            FLoops.Insert_FXCH(ir.Order^[i]);
        END;
    END;
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        IF (ocir.ProcSegms[n].code_len <> 0) &
           (ir.Nodes^[n].NOut > 1) THEN
            can_remove := TRUE;
            j := 0;
            WHILE can_remove & (j <= ir.Nodes^[n].NOut-1) DO
                can_remove := ir.Nodes^[ir.Nodes^[n].Out^[j]].NIn = 1;
                INC(j);
            END;
            IF can_remove THEN
                reorder.DoRemove(ocir.ProcSegms[n]);
                IF ScheStr.SegmForRemoved <> NIL THEN
                    FOR j:=0 TO ir.Nodes^[n].NOut-1 DO
                        ocir.AddToStart(ocir.ProcSegms[ir.Nodes^[n].Out^[j]],
                                        ScheStr.SegmForRemoved);
                    END;
                END;
            END;
        END;
    END;
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        IF ocir.ProcSegms[n].code_len <> 0 THEN
            r2b.BE.EnterNode(n,RD.NodeInfo^[n].sg);
            ocir.FindSetUsedFlags(ocir.ProcSegms[n],
                                  def.GetJumpUsedFlags(RD.NodeInfo^[n].j));
            ocir.SplitSegm(ocir.ProcSegms[n]);
            reorder.DoReOrder(ocir.ProcSegms[n]);
            tmp := ocir.ProcSegms[n];
            (* initial unfolding *)
            cur_pos := tmp.code[0].tpos;
            r2b.BE.AddPosition(cur_pos);
            Emit.PushSize := tmp.code[0].PUSH_SIZE;
            tmp.code[0].bin.ToBin();
            FOR j:=1 TO tmp.code_len-1 DO
                Emit.PushSize := tmp.code[j].PUSH_SIZE;
                IF NOT Emit.PosEqu(cur_pos,tmp.code[j].tpos)
                THEN
                    cur_pos := tmp.code[j].tpos;
                    r2b.BE.AddPosition(cur_pos);
                END;
                tmp.code[j].bin.ToBin();
            END;
            tmp := tmp^.next;
            WHILE tmp <> NIL DO
                FOR j:=0 TO tmp.code_len-1 DO
                    Emit.PushSize := tmp.code[j].PUSH_SIZE;
                    IF NOT Emit.PosEqu(cur_pos,tmp.code[j].tpos)
                    THEN
                        cur_pos := tmp.code[j].tpos;
                        r2b.BE.AddPosition(cur_pos);
                    END;
                    tmp.code[j].bin.ToBin();
                END;
                tmp := tmp^.next;
            END;
        END;
    END;
END Flush;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) SetSegment* (n: Node; seg: CodeDef.CODE_SEGM);
BEGIN
    ocir.c_seg := ocir.GetLastSegment(ocir.ProcSegms[n]);
    r2b.BE.SetSegment(n,seg);
END SetSegment;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) EmptySegment* (n: Node;
                                            sg: CodeDef.CODE_SEGM): BOOLEAN;
VAR tmp : ocir.Segment;
BEGIN
    tmp := ocir.GetLastSegment(ocir.ProcSegms[n]);
    RETURN tmp.code_len = 0;
END EmptySegment;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) AddPosition* (pos-: ir.TPOS);
BEGIN
    def.CURRENT_TPOS := pos;
END AddPosition;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) DisableOptimizations*;
BEGIN
    ResetLastFlags;
    def.ENABLE_OPTIMIZATION := FALSE;
END DisableOptimizations;

PROCEDURE (self: SelfRrdPtr) EnableOptimizations*;
BEGIN
    ResetLastFlags;
    def.ENABLE_OPTIMIZATION := TRUE;
END EnableOptimizations;

PROCEDURE (self: SelfRrdPtr) DisableMoveStores*;
BEGIN
     def.ENABLE_MOVE_STORE := FALSE;
END DisableMoveStores;

PROCEDURE (self: SelfRrdPtr) EnableMoveStores*;
BEGIN
    def.ENABLE_MOVE_STORE := TRUE;
END EnableMoveStores;
--------------------------------------------------------------------------------
--
--      Собственно генератор команд
--
--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveR_R* (d, s: D.PhysReg; sz: SizeType);
VAR op : r2b.MoveR_R;
BEGIN
    NEW(op);
    op.Fill(d,s,sz);
    ocir.prepare_nextCP(op, def.MOV, 1, def.UV);
    ocir.SetOpR_R(R2R(d,sz), R2R(s,sz));
    ocir.AddAssignOpArgRes();
    INC(LAST_FLAGS_OFFSET,ORD(d <> LAST_FLAGS_REG));
    Mov2Movzx(s,sz);
    IF sz=1 THEN SetLastR8Move(d); END;
END GenMoveR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveR_M* (r: PhysReg; a-: AddrMode; sz: SizeType);
VAR op : r2b.MoveR_M;
BEGIN
    INC(LAST_FLAGS_OFFSET,
        ORD((r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = ocir.c_seg.code_len)));
    NEW(op);
    op.Fill(r,a,sz);
    ocir.prepare_nextCP(op, def.MOV, 1, def.UV);
    P (sz);
    DD (a);
    IF (r = D.EAXp) & (a.place1.r = D.UNDEF_REG) & (a.place2.r = D.UNDEF_REG)
    THEN
        GO (a);
        ocir.SetOpR_M(R2R(r,sz), GOAddr);
    ELSE
        M (a);
        ocir.SetOpR_M(R2R(r,sz), MAddr);
        GOAddr := MAddr;
    END;
    ocir.AddAssignOpArgRes();
    ocir.CheckMoveLocal();
    Mov2Movzx(VAL(PhysReg,GOAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,GOAddr.place2.r),4);
    IF sz=1 THEN SetLastR8Move(r); END;
END GenMoveR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveR_INum* (r: PhysReg; v: LONGINT; sz: SizeType);
VAR op : r2b.MoveR_INum;
BEGIN
    INC(LAST_FLAGS_OFFSET,
        ORD((r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = ocir.c_seg.code_len)));
    NEW(op);
    op.Fill(r,v,sz);
    ocir.prepare_nextCP(op, def.MOV, 1, def.UV);
    ocir.SetOpR_I(R2R(r,sz));
    ocir.AddAssignOpArgRes();
    P (sz);
    IF sz=1 THEN SetLastR8Move(r); END;
END GenMoveR_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveR_Iglobal* (r: PhysReg; a-: AddrMode; sz: SizeType);
VAR op : r2b.MoveR_Iglobal;
BEGIN
    INC(LAST_FLAGS_OFFSET,
        ORD((r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = ocir.c_seg.code_len)));
    NEW(op);
    op.Fill(r,a,sz);
    GO (a);
    ocir.prepare_nextCP(op, def.MOV, 1, def.UV);
    ocir.SetOpR_M(R2R(r,sz), GOAddr);
    ocir.AddAssignOpArgRes();
    IF sz=1 THEN SetLastR8Move(r); END;
END GenMoveR_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveM_R* (a-: AddrMode; r: PhysReg; sz: SizeType);
VAR op : r2b.MoveM_R;
BEGIN
    INC(LAST_FLAGS_OFFSET,ORD(LAST_FLAGS_OFFSET = ocir.c_seg.code_len));
    NEW(op);
    op.Fill(a,r,sz);
    ocir.prepare_nextCP(op, def.MOV, 1, def.UV);
    P (sz);
    DD (a);
    IF (r = D.EAXp) & (a.place1.r = D.UNDEF_REG) & (a.place2.r = D.UNDEF_REG)
    THEN
        GO (a);
        ocir.SetOpM_R(GOAddr, R2R(r,sz));
    ELSE
        M (a);
        ocir.SetOpM_R(MAddr, R2R(r,sz));
        GOAddr := MAddr;
    END;
    ocir.AddAssignOpArgRes();
    Mov2Movzx(VAL(PhysReg,GOAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,GOAddr.place2.r),4);
    Mov2Movzx(r,sz);
    ocir.CheckMoveLocal();
END GenMoveM_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveM_INum* (a-: AddrMode; v: LONGINT; sz: SizeType);
VAR op : r2b.MoveM_INum;
BEGIN
    INC(LAST_FLAGS_OFFSET,ORD(LAST_FLAGS_OFFSET = ocir.c_seg.code_len));
    NEW(op);
    op.Fill(a,v,sz);
    ocir.prepare_nextCP(op, def.MOV, 1, def.UV);
    P (sz);
    DD (a);
    M (a);
    ocir.SetOpM_I(MAddr);
    ocir.AddAssignOpArgRes();
    ocir.CheckMoveLocal();
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenMoveM_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveM_Iglobal* (a-, ag-: AddrMode; sz: SizeType);
VAR op : r2b.MoveM_Iglobal;
BEGIN
    INC(LAST_FLAGS_OFFSET,ORD(LAST_FLAGS_OFFSET = ocir.c_seg.code_len));
    NEW(op);
    op.Fill(a,ag,sz);
    ocir.prepare_nextCP(op, def.MOV, 1, def.UV);
    DD (a);
    M (a);
    GO (ag);
    ocir.SetOpM_M(MAddr, GOAddr);
    ocir.AddAssignOpArgRes();
    ocir.CheckMoveLocal();
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenMoveM_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenXchgR_R* (d, s: PhysReg; sz: SizeType);
VAR op : r2b.XchgR_R;
BEGIN
    NEW(op);
    op.Fill(d,s,sz);
    ocir.prepare_nextP(op, def.XCHG, def.NP);
    ocir.SetOpR_R(R2R(d,sz), R2R(s,sz));
    ocir.AddXchgOpArgRes();
    IF sz = 1 THEN
        ocir.SetOpClocks(3);
    ELSE
        P (sz);
        ocir.SetOpClocks(2 + VAL(SHORTINT, ORD((d <> D.EAXp) AND (s <> D.EAXp))));
    END;
    Mov2Movzx(d,sz);
    Mov2Movzx(s,sz);
END GenXchgR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenXchgR_M* (r: PhysReg; a-: AddrMode; sz: SizeType);
VAR op : r2b.XchgR_M;
BEGIN
    NEW(op);
    op.Fill(r,a,sz);
    ocir.prepare_nextCP(op, def.XCHG, 3, def.NP);
    P (sz);
    DD (a);
    M (a);
    ocir.SetOpR_M(R2R(r,sz), MAddr);
    ocir.AddXchgOpArgRes();
    ocir.CheckMoveLocal();
    Mov2Movzx(r,sz);
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenXchgR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenPush_R* (r: PhysReg);
VAR op : r2b.Push_R;
BEGIN
    INC (LAST_FLAGS_OFFSET);
    NEW(op);
    op.Fill(r);
    ocir.prepare_nextCP(op, def.PUSH, 1, def.UV);
    ocir.SetDestReg(r);
    ocir.AddDestArg();
    ocir.AddOpArgR(def.Regs{def.ESP});
    ocir.AddOpArgRA(def.Regs{def.ESP});
    ocir.AddOpResR(def.Regs{def.ESP});
    INCL(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.tag_attrs,
         def.IsPush_R_OR_I);
    Mov2Movzx(r,4);
END GenPush_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenPush_M* (a-: AddrMode; sz: SizeType);
VAR op : r2b.Push_M;
BEGIN
    INC(LAST_FLAGS_OFFSET,ORD(LAST_FLAGS_OFFSET = ocir.c_seg.code_len));
    NEW(op);
    op.Fill(a,sz);
    ocir.prepare_nextCP(op, def.PUSH, 2, def.NP);
    DD (a);
    M (a);
    ocir.SetDestMem(MAddr);
    ocir.AddDestArg();
    ocir.AddOpArgR(def.Regs{def.ESP});
    ocir.AddOpArgRA(def.Regs{def.ESP});
    ocir.AddOpResR(def.Regs{def.ESP});
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenPush_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenPush_INum* (v: LONGINT; sz: SizeType);
VAR op : r2b.Push_INum;
BEGIN
    INC(LAST_FLAGS_OFFSET,ORD(LAST_FLAGS_OFFSET = ocir.c_seg.code_len));
    NEW(op);
    op.Fill(v,sz);
    ocir.prepare_nextCP(op, def.PUSH, 1, def.UV);
    ocir.SetDestImm();
    ocir.AddOpArgR(def.Regs{def.ESP});
    ocir.AddOpArgRA(def.Regs{def.ESP});
    ocir.AddOpResR(def.Regs{def.ESP});
    INCL(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.tag_attrs,
         def.IsPush_R_OR_I);
END GenPush_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenPush_Iglobal* (a-: AddrMode);
VAR op : r2b.Push_Iglobal;
BEGIN
    INC(LAST_FLAGS_OFFSET,ORD(LAST_FLAGS_OFFSET = ocir.c_seg.code_len));
    GO (a);
    NEW(op);
    op.Fill(a);
    ocir.prepare_nextCP(op, def.PUSH, 1, def.NP);
    ocir.SetDestMem(GOAddr);
    ocir.AddDestArg();
    ocir.AddOpArgR(def.Regs{def.ESP});
    ocir.AddOpArgRA(def.Regs{def.ESP});
    ocir.AddOpResR(def.Regs{def.ESP});
END GenPush_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenPop_R* (r: PhysReg);
VAR op : r2b.Pop_R;
BEGIN
    INC (LAST_FLAGS_OFFSET);
    NEW(op);
    op.Fill(r);
    ocir.prepare_nextCP(op, def.POP, 1, def.UV);
    ocir.SetDestReg(r);
    ocir.AddDestRes();
    ocir.AddOpArgR(def.Regs{def.ESP});
    ocir.AddOpArgRA(def.Regs{def.ESP});
    ocir.AddOpResR(def.Regs{def.ESP});
    INCL(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.tag_attrs,
         def.IsPop_R);
    Mov2Movzx(r,4);
END GenPop_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenOpINum_R* (ttt: D.BinaryOp; v: VALUE; sz: SizeType;
                                                r: PhysReg );
BEGIN
    ASSERT(FALSE);
END GenOpINum_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenOpR_R* (ttt: D.BinaryOp; d, s: PhysReg;
                                        sz: SizeType);
VAR op : r2b.OpR_R;
BEGIN
    NEW(op);
    op.Fill(ttt,d,s,sz);
    IF ttt = D.TTT_mul THEN
        ocir.prepare_nextCP(op, def.IMUL, 10, def.NP);
        P (sz);
    ELSE
        ocir.prepare_nextCP(op,
                            def.BOp2BOpConsts[ttt],
                            1,
                            def.BOp2PairConsts[ttt]);
        P (sz);
        IF ttt <> D.TTT_cmp THEN SetLastFlags (ORD(ttt), d, sz); END;
    END;
    ocir.AddOpResF(def.ALL_AFLAGS);
    ocir.SetOpR_R(R2R(d,sz), R2R(s,sz));
    ocir.AddSrcArg();
    ocir.AddDestArg();
    IF ttt <> D.TTT_cmp THEN ocir.AddDestRes(); END;
    IF (ttt = D.TTT_adc) OR (ttt = D.TTT_sbb) THEN
        ocir.AddOpArgF(def.Flags{def.CF});
    END;
    Mov2Movzx(d,sz);
    Mov2Movzx(s,sz);
END GenOpR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenOpR_M* (ttt: D.BinaryOp; r: PhysReg;
                                        a-: AddrMode; sz: SizeType);
VAR op : r2b.OpR_M;
BEGIN
    NEW(op);
    op.Fill(ttt,r,a,sz);
    IF ttt = D.TTT_mul THEN
        ocir.prepare_nextCP(op, def.IMUL, 10, def.NP);
        P (sz);
        DD (a);
        M  (a);
        ocir.SetOpR_M(R2R(r,sz), MAddr);
    ELSE
        ocir.prepare_nextCP(op,
                            def.BOp2BOpConsts[ttt],
                            2,
                            def.BOp2PairConsts[ttt]);
        P (sz);
        DD (a);
        M (a);
        IF ttt <> D.TTT_cmp THEN SetLastFlags (ORD(ttt), r, sz); END;
        ocir.SetOpR_M(R2R(r,sz), MAddr);
    END;
    ocir.AddOpResF(def.ALL_AFLAGS);
    ocir.AddSrcArg();
    ocir.AddDestArg();
    IF ttt <> D.TTT_cmp THEN ocir.AddDestRes(); END;
    IF (ttt = D.TTT_adc) OR (ttt = D.TTT_sbb) THEN
        ocir.AddOpArgF(def.Flags{def.CF});
    END;
    ocir.CheckMoveLocal();
    Mov2Movzx(r,sz);
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenOpR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenOpR_INum* (ttt: D.BinaryOp; r: PhysReg;
                                           v: LONGINT; sz: SizeType);
VAR op : r2b.OpR_INum;
BEGIN
    NEW(op);
    op.Fill(ttt,r,v,sz);
    IF ttt = D.TTT_mul THEN
        ocir.prepare_nextCP(op, def.IMUL, 10, def.NP);
        ocir.SetOpR_I(R2R(r,sz));
        ocir.AddDestArg();
        ocir.AddDestRes();
        ocir.AddOpResF(def.ALL_AFLAGS);
        P (sz);
    ELSE
        ocir.prepare_nextCP(op,
                            def.BOp2BOpConsts[ttt],
                            1,
                            def.BOp2PairConsts[ttt]);
        ocir.SetOpR_I(R2R(r,sz));
        ocir.AddBOpArgRes(ttt);
        IF sz <> 1 THEN P (sz); END;
        IF ttt <> D.TTT_cmp THEN SetLastFlags (ORD(ttt), r, sz); END;
    END;
    IF (ttt = D.TTT_adc) OR (ttt = D.TTT_sbb) THEN
        ocir.AddOpArgF(def.Flags{def.CF});
    END;
    Mov2Movzx(r,sz);
END GenOpR_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenOpR_Iglobal* (ttt: D.BinaryOp; r: PhysReg;
                                              a-: AddrMode; sz: SizeType);
VAR op : r2b.OpR_Iglobal;
BEGIN
    NEW(op);
    op.Fill(ttt,r,a,sz);
    IF ttt = D.TTT_mul THEN
        ocir.prepare_nextCP(op, def.IMUL, 10, def.NP);
    ELSE
        ocir.prepare_nextCP(op, def.BOp2BOpConsts[ttt],
                            1+VAL(SHORTINT,ORD(r<>D.EAXp)),
                            def.BOp2PairConsts[ttt]);
    END;
    ocir.AddOpResF(def.ALL_AFLAGS);
    GO (a);
    ocir.SetOpR_M(R2R(r,sz), GOAddr);
    ocir.AddSrcArg();
    ocir.AddDestArg();
    IF ttt <> D.TTT_cmp THEN ocir.AddDestRes(); END;
    IF (ttt = D.TTT_adc) OR (ttt = D.TTT_sbb) THEN
        ocir.AddOpArgF(def.Flags{def.CF});
    END;
    Mov2Movzx(r,sz);
END GenOpR_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenOpM_R* (ttt: D.BinaryOp; a-: AddrMode;
                                        r: PhysReg; sz: SizeType);
VAR op : r2b.OpM_R;
BEGIN
    NEW(op);
    op.Fill(ttt,a,r,sz);
    ocir.prepare_nextCP(op,
                        def.BOp2BOpConsts[ttt],
                        3,
                        def.BOp2PairConsts[ttt]);
    P (sz);
    DD (a);
    M (a);
    ocir.SetOpM_R(MAddr, R2R(r,sz));
    ocir.AddBOpArgRes(ttt);
    IF (ttt = D.TTT_adc) OR (ttt = D.TTT_sbb) THEN
        ocir.AddOpArgF(def.Flags{def.CF});
    END;
    ocir.CheckMoveLocal();
    Mov2Movzx(r,sz);
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenOpM_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenOpM_INum* (ttt: D.BinaryOp; a-: AddrMode;
                                           v: LONGINT; sz: SizeType);
VAR op : r2b.OpM_INum;
BEGIN
    NEW(op);
    op.Fill(ttt,a,v,sz);
    ocir.prepare_nextCP(op,
                        def.BOp2BOpConsts[ttt],
                        3,
                        def.BOp2PairConsts[ttt]);
    P (sz);
    DD (a);
    M (a);
    ocir.SetOpM_I(MAddr);
    ocir.AddBOpArgRes(ttt);
    IF (ttt = D.TTT_adc) OR (ttt = D.TTT_sbb) THEN
        ocir.AddOpArgF(def.Flags{def.CF});
    END;
    ocir.CheckMoveLocal();
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenOpM_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenOpM_Iglobal* (ttt: D.BinaryOp; a-, ag-: AddrMode;
                                              sz: SizeType);
VAR op : r2b.OpM_Iglobal;
BEGIN
    NEW(op);
    op.Fill(ttt,a,ag,sz);
    ocir.prepare_nextCP(op,
                        def.BOp2BOpConsts[ttt],
                        3,
                        def.BOp2PairConsts[ttt]);
    DD (a);
    M (a);
    GO (ag);
    ocir.SetOpM_M(MAddr, GOAddr);
    ocir.AddSrcArg();
    ocir.AddDestArg();
    IF ttt <> D.TTT_cmp THEN ocir.AddDestRes(); END;
    ocir.AddOpResF(def.ALL_AFLAGS);
    IF (ttt = D.TTT_adc) OR (ttt = D.TTT_sbb) THEN
        ocir.AddOpArgF(def.Flags{def.CF});
    END;
    ocir.CheckMoveLocal();
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenOpM_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE DivPrefix(sz: SizeType; signed : BOOLEAN);
BEGIN
   IF signed THEN
       ocir.SetOpCode(def.IDIV);
       ocir.SetDestReg(def.EAX);
       ocir.AddOpResF(def.ALL_AFLAGS);
       CASE sz OF
         1 : ocir.AddOpArgR(def.Regs{def.EAX});
             ocir.AddOpResR(def.Regs{def.EAX});
             ocir.SetOpClocks(22);       |
         2 : ocir.AddOpArgR(def.Regs{def.EAX,def.EDX});
             ocir.AddOpResR(def.Regs{def.EAX,def.EDX});
             ocir.SetOpClocks(30);       |
         4 : ocir.AddOpArgR(def.Regs{def.EAX,def.EDX});
             ocir.AddOpResR(def.Regs{def.EAX,def.EDX});
             ocir.SetOpClocks(46);       |
       END;
   ELSE
       ocir.SetOpCode(def._DIV);
       ocir.SetDestReg(def.EAX);
       ocir.AddOpResF(def.ALL_AFLAGS);
       CASE sz OF
         1 : ocir.AddOpArgR(def.Regs{def.EAX});
             ocir.AddOpResR(def.Regs{def.EAX});
             ocir.SetOpClocks(17);       |
         2 : ocir.AddOpArgR(def.Regs{def.EAX,def.EDX});
             ocir.AddOpResR(def.Regs{def.EAX,def.EDX});
             ocir.SetOpClocks(25);       |
         4 : ocir.AddOpArgR(def.Regs{def.EAX,def.EDX});
             ocir.AddOpResR(def.Regs{def.EAX,def.EDX});
             ocir.SetOpClocks(41);       |
       END;
   END;
END DivPrefix;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenDiv_R* (r: PhysReg; sz: SizeType;
                                        signed: BOOLEAN);
VAR op : r2b.Div_R;
BEGIN
    NEW(op);
    op.Fill(r,sz,signed);
    ocir.prepare_nextP(op, def.UNDEF_OP, def.NP);
    DivPrefix (sz, signed);
    P (sz);
    ocir.SetSrcReg(R2R(r,sz));
    ocir.AddSrcArg();
    Mov2Movzx(r,sz);
END GenDiv_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenDiv_M* (a-: AddrMode; sz: SizeType;
                                        signed: BOOLEAN);
VAR op : r2b.Div_M;
BEGIN
    NEW(op);
    op.Fill(a,sz,signed);
    ocir.prepare_nextP(op, def.UNDEF_OP, def.NP);
    DivPrefix(sz, signed);
    P (sz);
    DD (a);
    M (a);
    ocir.SetSrcMem(MAddr);
    ocir.AddSrcArg();
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenDiv_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenDiv_INum* (v: LONGINT; sz: SizeType;
                                           signed: BOOLEAN);
VAR op : r2b.Div_INum;
BEGIN
    NEW(op);
    op.Fill(v,sz,signed);
    ocir.prepare_nextP(op, def.UNDEF_OP, def.NP);
    DivPrefix (sz, signed);
    P (sz);
    ocir.SetSrcImm();
END GenDiv_INum;

--------------------------------------------------------------------------------

PROCEDURE MulPrefix(sz: SizeType; signed : BOOLEAN);
BEGIN
    IF signed THEN
        ocir.SetOpCode(def.IMUL);
        ocir.AddOpResF(def.ALL_AFLAGS);
        CASE sz OF
          1 : ocir.AddOpArgR(def.Regs{def.EAX});
              ocir.AddOpResR(def.Regs{def.EAX});
              ocir.SetOpClocks(11);       |
          2 : ocir.AddOpArgR(def.Regs{def.EAX});
              ocir.AddOpResR(def.Regs{def.EAX,def.EDX});
              ocir.SetOpClocks(11);       |
          4 : ocir.AddOpArgR(def.Regs{def.EAX});
              ocir.AddOpResR(def.Regs{def.EAX,def.EDX});
              ocir.SetOpClocks(10);       |
        END;
    ELSE
        ocir.SetOpCode(def.MUL);
        ocir.AddOpResF(def.Flags{def._OF,def.CF});
        ocir.SetDestReg(def.EAX);
        CASE sz OF
          1 : ocir.AddOpArgR(def.Regs{def.EAX});
              ocir.AddOpResR(def.Regs{def.EAX});
              ocir.SetOpClocks(11);       |
          2 : ocir.AddOpArgR(def.Regs{def.EAX});
              ocir.AddOpResR(def.Regs{def.EAX,def.EDX});
              ocir.SetOpClocks(11);       |
          4 : ocir.AddOpArgR(def.Regs{def.EAX});
              ocir.AddOpResR(def.Regs{def.EAX,def.EDX});
              ocir.SetOpClocks(10);       |
        END;
    END;
END MulPrefix;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMul_R* (r: PhysReg; sz: SizeType;
                                        signed: BOOLEAN);
VAR op : r2b.Mul_R;
BEGIN
    NEW(op);
    op.Fill(r,sz,signed);
    ocir.prepare_nextP(op, def.UNDEF_OP, def.NP);
    ocir.SetSrcReg(R2R(r,sz));
    ocir.AddSrcArg();
    MulPrefix(sz, signed);
    P (sz);
    Mov2Movzx(r,sz);
END GenMul_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMul_M* (a-: AddrMode; sz: SizeType;
                                        signed: BOOLEAN);
VAR op : r2b.Mul_M;
BEGIN
    NEW(op);
    op.Fill(a,sz,signed);
    ocir.prepare_nextP(op, def.UNDEF_OP, def.NP);
    MulPrefix(sz, signed);
    P (sz);
    DD (a);
    M (a);
    ocir.SetSrcMem(MAddr);
    ocir.AddSrcArg();
    ocir.CheckMoveLocal();
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenMul_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMul_INum* (v: LONGINT; sz: SizeType;
                                           signed: BOOLEAN);
VAR op : r2b.Mul_INum;
BEGIN
    NEW(op);
    op.Fill(v,sz,signed);
    ocir.prepare_nextP(op, def.UNDEF_OP, def.NP);
    ocir.SetSrcImm();
    MulPrefix(sz, signed);
    P (sz);
END GenMul_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenIMulR_RC* (d, s: PhysReg; v: LONGINT;
                                           sz: SizeType);
VAR op : r2b.IMulR_RC;
BEGIN
    NEW(op);
    op.Fill(d,s,v,sz);
    ocir.prepare_nextCP(op, def.IMUL, 10, def.NP);
    ocir.SetOpR_R(R2R(d,sz), R2R(s,sz));
    ocir.AddSrcArg();
    ocir.AddDestRes();
    ocir.AddOpResF(def.ALL_AFLAGS);
    P (sz);
    Mov2Movzx(d,sz);
    Mov2Movzx(s,sz);
END GenIMulR_RC;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenIMulR_MC* (r: PhysReg; a-: AddrMode;
                                           v: LONGINT; sz: SizeType);
VAR op : r2b.IMulR_MC;
BEGIN
    NEW(op);
    op.Fill(r,a,v,sz);
    ocir.prepare_nextCP(op, def.IMUL, 10, def.NP);
    P (sz);
    DD (a);
    M (a);
    ocir.SetOpR_M(R2R(r,sz), MAddr);
    ocir.AddSrcArg();
    ocir.AddDestRes();
    ocir.AddOpResF(def.ALL_AFLAGS);
    ocir.CheckMoveLocal();
    Mov2Movzx(r,sz);
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenIMulR_MC;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenTestR_R* (r1, r2: PhysReg; sz: SizeType);
VAR op : r2b.TestR_R;
BEGIN
    IF (r1 <> r2) OR (r1 <> LAST_FLAGS_REG) OR (sz <> LAST_FLAGS_SIZE) OR
       (ocir.c_seg.code_len <> LAST_FLAGS_OFFSET) OR (VAL(D.BinaryOp, LAST_FLAGS_OP) = D.TTT_add)
    THEN
        NEW(op);
        op.Fill(r1,r2,sz);
        ocir.prepare_nextCP(op, def.TEST, 1, def.UV);
        ocir.SetOpR_R(R2R(r1,sz), R2R(r2,sz));
        ocir.AddSrcArg();
        ocir.AddDestArg();
        ocir.AddOpResF(def.ALL_AFLAGS);
        P (sz);
        IF r1 = r2 THEN SetLastFlags (ORD(D.TTT_and), r1, sz); END;
    END;
    Mov2Movzx(r1,sz);
    Mov2Movzx(r2,sz);
END GenTestR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenTestR_INum* (r: PhysReg; v: LONGINT; sz: SizeType);
VAR op : r2b.TestR_INum;
BEGIN
    NEW(op);
    op.Fill(r,v,sz);
    ocir.prepare_nextCP(op, def.TEST, 1, def.NP);
    ocir.SetOpR_I(R2R(r,sz));
    ocir.AddDestArg();
    ocir.AddOpResF(def.ALL_AFLAGS);
    P (sz);
    Mov2Movzx(r,sz);
END GenTestR_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenTestM_INum* (a-: AddrMode; v: LONGINT; sz: SizeType);
VAR op : r2b.TestM_INum;
BEGIN
    NEW(op);
    op.Fill(a,v,sz);
    ocir.prepare_nextCP(op, def.TEST, 2, def.NP);
    P (sz);
    DD (a);
    M (a);
    ocir.SetOpM_I(MAddr);
    ocir.AddDestArg();
    ocir.AddOpResF(def.ALL_AFLAGS);
    ocir.CheckMoveLocal();
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenTestM_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenLEA* (r: PhysReg; a-: AddrMode);
VAR op : r2b.LEA;
BEGIN
    M (a);
    NEW(op);
    op.Fill(r,a);
    ocir.prepare_nextCP(op, def.LEA, 1, def.UV);
    ocir.SetOpR_M(r, MAddr);
    ocir.AddSrcIndirectArg();
    ocir.AddDestRes();
    ocir.CheckLEA();
    ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.arg.v.l := ir.UNDEFINED;
    BitVect.Free(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.arg.v.vs);
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenLEA;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMovesxR_M* (r: PhysReg; a-: AddrMode; rsz, sz: SizeType);
VAR op : r2b.MovesxR_M;
BEGIN
    INC(LAST_FLAGS_OFFSET,
        ORD((r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = ocir.c_seg.code_len)));
    NEW(op);
    op.Fill(r,a,rsz,sz);
    ocir.prepare_nextCP(op, def.MOV, 3, def.UV);
    P (rsz);
    DD (a);
    M (a);
    ocir.SetOpR_M(R2R(r,rsz), MAddr);
    ocir.AddAssignOpArgRes();
    ocir.CheckMoveLocal();
    IF rsz=1 THEN SetLastR8Move(r); END;
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenMovesxR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMovesxR_R* (d, s: PhysReg; rsz, sz: SizeType);
VAR op : r2b.MovesxR_R;
BEGIN
    INC(LAST_FLAGS_OFFSET,
        ORD((d <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = ocir.c_seg.code_len)));
    NEW(op);
    op.Fill(d,s,rsz,sz);
    IF (rsz = 4) & (sz = 2) & (d = D.EAXp) & (s = D.AXp) THEN
        ocir.prepare_nextCP(op, def.CWDE, 3, def.NP);
        ocir.SetOpR_R(def.EAX, def.EAX);
        ocir.AddAssignOpArgRes();
    ELSIF (rsz = 2) & (sz = 1) & (d = D.AXp) & (s = D.ALp) THEN
        ocir.prepare_nextCP(op, def.CWDE, 3, def.NP);
        ocir.SetOpR_R(def.EAX, def.EAX);
        ocir.SetPrefix(def.P_OS);
        ocir.AddAssignOpArgRes();
        Mov2Movzx(s,sz);
    ELSE
        ocir.prepare_nextCP(op, def.MOV, 3, def.UV);
        ocir.SetOpR_R(R2R(d,rsz), R2R(s,sz));
        ocir.AddAssignOpArgRes();
        P  (rsz);
        Mov2Movzx(s,sz);
        IF rsz=1 THEN SetLastR8Move(d); END;
    END;
END GenMovesxR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMovezxR_M* (r: PhysReg; a-: AddrMode; rsz, sz: SizeType);
VAR op : r2b.MovezxR_M;
BEGIN
    INC(LAST_FLAGS_OFFSET,
        ORD((r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = ocir.c_seg.code_len)));
    NEW(op);
    op.Fill(r,a,rsz,sz);
    ocir.prepare_nextCP(op, def.MOVZX, 3, def.NP);
    DD (a);
    P (rsz);
    M (a);
    ocir.SetOpR_M(R2R(r,rsz), MAddr);
    ocir.AddAssignOpArgRes();
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenMovezxR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMovezxR_R* (d, s: PhysReg; rsz, sz: SizeType);
VAR op : r2b.MovezxR_R;
BEGIN
    INC(LAST_FLAGS_OFFSET,
        ORD((d <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = ocir.c_seg.code_len)));
    NEW(op);
    op.Fill(d,s,rsz,sz);
    ocir.prepare_nextCP(op, def.MOVZX, 3, def.NP);
    ocir.SetOpR_R(R2R(d,rsz), R2R(s,sz));
    ocir.AddAssignOpArgRes();
    P  (rsz);
    Mov2Movzx(s,sz);
END GenMovezxR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenOpR* (ttt: D.UnaryOp; r: PhysReg; sz: SizeType);
VAR op : r2b.OpR;
BEGIN
    NEW(op);
    op.Fill(ttt,r,sz);
    ocir.prepare_nextCP(op, def.UOp2UOpConsts[ttt], 1, def.UOp2PairConsts[ttt]);
    ocir.SetDestReg(R2R(r,sz));
    ocir.AddUOpArgRes(ttt);
    P (sz);
    IF ttt # D.TTT_not THEN
        SetLastFlags (ORD(ttt), r, sz);
    END;
    Mov2Movzx(r,sz);
END GenOpR;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenOpM* (ttt: D.UnaryOp; a-: AddrMode; sz: SizeType);
VAR op : r2b.OpM;
BEGIN
    NEW(op);
    op.Fill(ttt,a,sz);
    ocir.prepare_nextCP(op, def.UOp2UOpConsts[ttt], 3, def.UOp2PairConsts[ttt]);
    P (sz);
    DD (a);
    M (a);
    ocir.SetDestMem(MAddr);
    ocir.AddUOpArgRes(ttt);
    ocir.CheckMoveLocal();
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenOpM;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenSetC_R* (r: PhysReg; c: D.Condition);
VAR op : r2b.SetC_R;
BEGIN
    NEW(op);
    op.Fill(r,c);
    ocir.prepare_nextCP(op, def.Set2SetConsts[c], 1, def.NP);
    ocir.SetDestReg(r);
    ocir.AddDestRes();
    ocir.AddOpArgF(def.Set2FConsts[c]);
END GenSetC_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenSetC_M* (a-: AddrMode; c: D.Condition);
VAR op : r2b.SetC_M;
BEGIN
    M (a);
    NEW(op);
    op.Fill(a,c);
    ocir.prepare_nextCP(op, def.Set2SetConsts[c], 2, def.NP);
    ocir.SetDestMem(MAddr);
    ocir.AddDestIndirectArg();
    ocir.AddDestRes();
    ocir.AddOpArgF(def.Set2FConsts[c]);
    ocir.CheckMoveLocal();
END GenSetC_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenShiftR_R* (ttt: D.ShiftOp; r: PhysReg;
                                           sz: SizeType);
VAR op : r2b.ShiftR_R;
BEGIN
    NEW(op);
    op.Fill(ttt,r,sz);
    ocir.prepare_nextCP(op, def.Shift2ShiftConsts[ttt], 4, def.NP);
    ocir.SetOpR_R(R2R(r,sz), def.ECX);
    ocir.AddSrcDestArg();
    ocir.AddDestRes();
    ocir.AddOpResF(def.Shift2FConsts[ttt]);
    P (sz);
    Mov2Movzx(r,sz);
    Mov2Movzx(D.EAXp,4);
END GenShiftR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenShiftR_INum* (ttt: D.ShiftOp; r: PhysReg;
                                              v: LONGINT; sz: SizeType);
VAR op : r2b.ShiftR_INum;
BEGIN
    NEW(op);
    op.Fill(ttt,r,v,sz);
    ocir.prepare_nextCP(op, def.Shift2ShiftConsts[ttt], 1, def.PU);
    ocir.SetOpR_I(R2R(r,sz));
    ocir.AddDestArg();
    ocir.AddDestRes();
    ocir.AddOpResF(def.Shift2FConsts[ttt]);
    P (sz);
    Mov2Movzx(r,sz);
END GenShiftR_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenShiftM_INum* (ttt: D.ShiftOp; a-: AddrMode;
                                              v: LONGINT; sz: SizeType);
VAR op : r2b.ShiftM_INum;
BEGIN
    NEW(op);
    op.Fill(ttt,a,v,sz);
    ocir.prepare_nextCP(op, def.Shift2ShiftConsts[ttt], 3, def.PU);
    P (sz);
    DD (a);
    M (a);
    ocir.SetOpM_I(MAddr);
    ocir.AddDestArg();
    ocir.AddDestRes();
    ocir.AddOpResF(def.Shift2FConsts[ttt]);
    ocir.CheckMoveLocal();
    Mov2Movzx(VAL(PhysReg,MAddr.place1.r),4);
    Mov2Movzx(VAL(PhysReg,MAddr.place2.r),4);
END GenShiftM_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenCBW*;
VAR op : r2b.CBW;
BEGIN
    NEW(op);
    op.Fill();
    ocir.prepare_nextCP(op, def.CBW, 3, def.NP);
    ocir.AddOpArgR(def.Regs{def.EAX});
    ocir.AddOpResR(def.Regs{def.EAX});
    INC (LAST_FLAGS_OFFSET);
END GenCBW;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenCDQ* (sz: SizeType);
VAR op : r2b.CDQ;
BEGIN
    NEW(op);
    op.Fill(sz);
    ocir.prepare_nextCP(op, def.CDQ, 2, def.NP);
    ocir.AddOpArgR(def.Regs{def.EAX});
    ocir.AddOpResR(def.Regs{def.EDX});
    INC (LAST_FLAGS_OFFSET);
END GenCDQ;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenRepStoSD*;
VAR op : r2b.RepStoSD;
BEGIN
    NEW(op);
    op.Fill();
    ocir.prepare_nextCP(op, def.STOSD, 4, def.NP);
    ocir.SetPrefix(def.P_REP);
    ocir.AddOpArgR(def.Regs{def.ECX,def.EDI,def.EAX});
    ocir.AddOpArgRA(def.Regs{def.EDI});
    ocir.AddOpArgBadMem();
    ocir.AddOpResR(def.Regs{def.EDI});
    ocir.AddOpResBadMem();
END GenRepStoSD;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenRepMovSD*;
VAR op : r2b.RepMovSD;
BEGIN
    NEW(op);
    op.Fill();
    ocir.prepare_nextCP(op, def.MOVSD, 4, def.NP);
    ocir.SetPrefix(def.P_REP);
    ocir.AddOpArgR(def.Regs{def.ECX,def.ESI,def.EDI});
    ocir.AddOpArgRA(def.Regs{def.ESI,def.EDI});
    ocir.AddOpArgBadMem();
    ocir.AddOpResR(def.Regs{def.ESI,def.EDI});
    ocir.AddOpResBadMem();
END GenRepMovSD;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMovSD*;
VAR op : r2b.MovSD;
BEGIN
    NEW(op);
    op.Fill();
    ocir.prepare_nextCP(op, def.MOVSD, 4, def.NP);
    ocir.AddOpArgR(def.Regs{def.ESI,def.EDI});
    ocir.AddOpArgRA(def.Regs{def.ESI,def.EDI});
    ocir.AddOpArgBadMem();
    ocir.AddOpResR(def.Regs{def.ESI,def.EDI});
    ocir.AddOpResBadMem();
END GenMovSD;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMovSW*;
VAR op : r2b.MovSW;
BEGIN
    NEW(op);
    op.Fill();
    ocir.prepare_nextCP(op, def.MOVSW, 4, def.NP);
    ocir.SetPrefix(def.P_OS);
    ocir.AddOpArgR(def.Regs{def.ESI,def.EDI});
    ocir.AddOpArgRA(def.Regs{def.ESI,def.EDI});
    ocir.AddOpArgBadMem();
    ocir.AddOpResR(def.Regs{def.ESI,def.EDI});
    ocir.AddOpResBadMem();
END GenMovSW;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenRepMovSB*;
VAR op : r2b.RepMovSB;
BEGIN
    NEW(op);
    op.Fill();
    ocir.prepare_nextCP(op, def.MOVSB, 4, def.NP);
    ocir.SetPrefix(def.P_REP);
    ocir.AddOpArgR(def.Regs{def.ECX,def.ESI,def.EDI});
    ocir.AddOpArgRA(def.Regs{def.ESI,def.EDI});
    ocir.AddOpArgBadMem();
    ocir.AddOpResR(def.Regs{def.ESI,def.EDI});
    ocir.AddOpResBadMem();
END GenRepMovSB;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMovSB*;
VAR op : r2b.MovSB;
BEGIN
    NEW(op);
    op.Fill();
    ocir.prepare_nextCP(op, def.MOVSB, 4, def.NP);
    ocir.AddOpArgR(def.Regs{def.ESI,def.EDI});
    ocir.AddOpArgRA(def.Regs{def.ESI,def.EDI});
    ocir.AddOpArgBadMem();
    ocir.AddOpResR(def.Regs{def.ESI,def.EDI});
    ocir.AddOpResBadMem();
END GenMovSB;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenSahf*;
VAR op : r2b.Sahf;
BEGIN
    NEW(op);
    op.Fill();
    ocir.prepare_nextCP(op, def.SAHF, 2, def.NP);
    ocir.AddOpArgR(def.Regs{def.EAX});
    ocir.AddOpResF(def.AFLAGS_NO_OF);
END GenSahf;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenJ* (j: D.Condition; o: LONGINT; long: BOOLEAN);
VAR op : r2b.J;
BEGIN
    NEW(op);
    op.Fill(j,o,long);
    IF j = D.UnJ THEN
        ocir.prepare_nextCP(op, def.JMP, 1, def.NP);
    ELSE
        ocir.prepare_nextCP(op, def.J2JConsts[j], 1, def.PV);
        ocir.AddOpArgF(def.J2FConsts[j]);
        INCL(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.tag_attrs,
             def.IsJcc);
    END;
    ocir.SetDestImm();
END GenJ;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenBinJ* (j: D.Condition; o: LONGINT; long: BOOLEAN);
BEGIN
    r2b.BE.GenJ (j, o, long);
END GenBinJ;

PROCEDURE (self: SelfRrdPtr) GenTxtJ* (j: D.Condition; lb: Emit.LABEL);
BEGIN
    r2b.BE.GenTxtJ (j, lb);
END GenTxtJ;

PROCEDURE (self: SelfRrdPtr) NewLabel* (VAR x: Emit.LABEL);
BEGIN
  r2b.BE.NewLabel(x);
END NewLabel;

PROCEDURE (self: SelfRrdPtr) SetLabel* (x: Emit.LABEL);
BEGIN
  r2b.BE.SetLabel(x);
END SetLabel;

PROCEDURE (self: SelfRrdPtr) InsertLabel* (VAR x: Emit.LABEL);
BEGIN
  r2b.BE.InsertLabel(x);
END InsertLabel;

PROCEDURE (self: SelfRrdPtr) DwLabel* (x: Emit.LABEL);
BEGIN
  r2b.BE.DwLabel(x);
END DwLabel;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenCall_M* (a-: AddrMode; u, m: D.PhysRegSet;
                                         r, w: BitVector);
VAR op : r2b.Call_M;
BEGIN
    NEW(op);
    op.Fill(a,u,m,r,w);
    ocir.prepare_nextCP(op, def.CALL, 2, def.NP);
    INCL(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.tag_attrs,
         def.DANG_OP);
    DD (a);
    M (a);
    ocir.SetDestMem(MAddr);
    ocir.AddDestArg();
    ocir.AddOpArgR(def.Regs{def.ESP} + u);
    ocir.AddOpArgRA(def.Regs{def.ESP});
    IF r <> NIL THEN ocir.AddOpArgV(r); END;
    ocir.AddOpResR(def.Regs{def.ESP} + m);
    ocir.AddOpResF(def.ALL_AFLAGS);
    IF w <> NIL THEN ocir.AddOpResV(w); END;
END GenCall_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenCall_R* (r: PhysReg; u, m: D.PhysRegSet;
                                         rd, w: BitVector);
VAR op : r2b.Call_R;
BEGIN
    NEW(op);
    op.Fill(r,u,m,rd,w);
    ocir.prepare_nextCP(op, def.CALL, 2, def.NP);
    INCL(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.tag_attrs,
         def.DANG_OP);
    ocir.SetDestReg(r);
    ocir.AddDestArg();
    ocir.AddOpArgR(def.Regs{def.ESP} + u);
    ocir.AddOpArgRA(def.Regs{def.ESP});
    IF rd <> NIL THEN ocir.AddOpArgV(rd); END;
    ocir.AddOpResR(def.Regs{def.ESP} + m);
    ocir.AddOpResF(def.ALL_AFLAGS);
    IF w <> NIL THEN ocir.AddOpResV(w); END;
END GenCall_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenCall_INum* (v: LONGINT; u, m: D.PhysRegSet;
                                            r, w: BitVector);
BEGIN
    ASSERT (FALSE);
END GenCall_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenCall_Iglobal* (a-: AddrMode; u, m: D.PhysRegSet;
                                               r, w: BitVector);
VAR op : r2b.Call_Iglobal;
BEGIN
    ASSERT(NOT prc.IsCodeProc (a.proc));
    NEW(op);
    op.Fill(a,u,m,r,w);
    ocir.prepare_nextCP(op, def.CALL, 1, def.PV);
    INCL(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.tag_attrs,
         def.DANG_OP);
    ocir.SetDestMem(a);
    ocir.AddOpArgR(def.Regs{def.ESP} + u);
    ocir.AddOpArgRA(def.Regs{def.ESP});
    IF r <> NIL THEN ocir.AddOpArgV(r); END;
    ocir.AddOpResR(def.Regs{def.ESP} + m);
    ocir.AddOpResF(def.ALL_AFLAGS);
    IF w <> NIL THEN ocir.AddOpResV(w); END;
END GenCall_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenRet* (n: INT);
VAR op : r2b.Ret;
BEGIN
    NEW(op);
    op.Fill(n);
    ocir.prepare_nextP(op, def.RET, def.NP);
    ocir.SetDestImm();
    ocir.AddOpArgR(def.Regs{def.ESP});
    ocir.AddOpArgRA(def.Regs{def.ESP});
    ocir.AddOpResR(def.Regs{def.ESP});
    ASSERT (n < 65535);
    ocir.SetOpClocks(2 + VAL(SHORTINT,ORD(n<>0)));
END GenRet;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenCase* (r: PhysReg; v: LONGINT; lb: Emit.LABEL);
VAR op : r2b.Case;
BEGIN
    NEW(op);
    op.Fill(r,v,lb);
    ocir.prepare_nextCP(op, def.JMP, 2, def.PV);
    ocir.SetDestMemComp(D.UNDEF_REGp, r, D.x1);
    ocir.AddDestArg();
    IF Emit.DSneqSS THEN
        ocir.SetPrefix(def.P_CS);
    END;
END GenCase;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveR_Table* (r, i: PhysReg; t: pc.OBJECT;
                                              sz: SizeType);
VAR op : r2b.MoveR_Table;
BEGIN
    NEW(op);
    op.Fill(r,i,t,sz);
    ocir.prepare_nextCP(op, def.MOV, 1, def.UV);
    ocir.SetDestReg(R2R(r,sz));
    ocir.SetSrcMemComp(D.UNDEF_REGp, i, D.x4);
    ocir.AddAssignOpArgRes();
    P (sz);
END GenMoveR_Table;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenOpR_Table* (ttt: D.BinaryOp; r, i: PhysReg;
                                           t: pc.OBJECT; sz: SizeType);
VAR op : r2b.OpR_Table;
BEGIN
    NEW(op);
    op.Fill(ttt,r,i,t,sz);
    ocir.prepare_nextCP(op,
                        def.BOp2BOpConsts[ttt],
                        2,
                        def.BOp2PairConsts[ttt]);
    ocir.SetDestReg(R2R(r,sz));
    ocir.SetSrcMemComp(D.UNDEF_REGp, i, D.x4);
    ocir.AddBOpArgRes(ttt);
    P (sz);
    SetLastFlags (ORD(ttt), r, sz);
END GenOpR_Table;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenTestR_Table* (r, i: PhysReg; t: pc.OBJECT;
                                              sz: SizeType);
VAR op : r2b.TestR_Table;
BEGIN
    NEW(op);
    op.Fill(r,i,t,sz);
    ocir.prepare_nextCP(op, def.TEST, 2, def.UV);
    ocir.SetDestReg(R2R(r,sz));
    ocir.SetSrcMemComp(D.UNDEF_REGp, i, D.x4);
    ocir.AddSrcArg();
    ocir.AddDestArg();
    ocir.AddOpResF(def.ALL_AFLAGS);
    P (sz);
END GenTestR_Table;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenHalt*;
VAR op : r2b.Halt;
BEGIN
    NEW(op);
    op.Fill();
    ocir.prepare_nextCP(op, def.CALL, 1, def.PV);
    INCL(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.tag_attrs,
         def.DANG_OP);
    ocir.SetDestImm();
    ocir.AddOpArgR(def.Regs{def.ESP});
    ocir.AddOpArgRA(def.Regs{def.ESP});
    ocir.AddOpResR(def.Regs{def.ESP});
    ocir.AddOpResF(def.ALL_AFLAGS);
END GenHalt;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenRaise* (is_assert: BOOLEAN);
VAR op : r2b.Raise;
BEGIN
    NEW(op);
    op.Fill(is_assert, FALSE);
    ocir.prepare_nextCP(op, def.CALL, 1, def.PV);
    INCL(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.tag_attrs,
         def.DANG_OP);
    ocir.SetDestImm();
    ocir.AddOpArgR(def.Regs{def.ESP});
    ocir.AddOpArgRA(def.Regs{def.ESP});
    ocir.AddOpResR(def.Regs{def.ESP});
    ocir.AddOpResF(def.ALL_AFLAGS);
END GenRaise;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenSkipTrap* (j:D.Condition; intno: Emit.Trap; pos-: ir.TPOS);
VAR op : r2b.SkipTrap;
BEGIN
    INC(LAST_FLAGS_OFFSET,ORD(LAST_FLAGS_OFFSET = ocir.c_seg.code_len));
    NEW(op);
    op.Fill(j,intno,pos);
    IF intno = Emit.IntOTrap THEN
        ocir.prepare_nextCP(op, def.INTO, 4, def.NP);
        ocir.AddOpArgR(def.Regs{def.ESP});
        ocir.AddOpArgRA(def.Regs{def.ESP});
        ocir.AddOpResR(def.Regs{def.ESP});
        ocir.AddOpArgF(def.Flags{def._OF});
    ELSE
        ocir.prepare_nextCP(op, def.J2JConsts[j], 1, def.PV);
        INCL(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.tag_attrs,
             def.MOV_CONTROL_TRANSFER);
        INCL(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.tag_attrs,
             def.IsJcc);
    END;
    ocir.AddOpArgF(def.GetJumpUsedFlags(j));
    INCL(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.tag_attrs,
         def.DANG_OP);
    ocir.SetDestImm();
END GenSkipTrap;

--------------------------------------------------------------------------------
PROCEDURE (self: SelfRrdPtr) GenCmpXchg* (a-: AddrMode; r: def.Register; sz: ir.SizeType; lock, generic: BOOLEAN);
VAR op : r2b.CmpXchg;
BEGIN
    NEW(op);
    INC(LAST_FLAGS_OFFSET,
        ORD((r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = ocir.c_seg.code_len)));
    op.Fill(a,r,sz,lock,generic);
    GO (a);
    ocir.prepare_nextCP(op, def.CMPXCHG, 1, def.UV);
    ocir.AddOpArgR(def.Regs{def.EAX,def.ESP});
    ocir.AddOpResR(def.Regs{def.EAX});
    ocir.AddOpResF(def.Flags{def.ZF});
    ocir.SetOpR_M(R2R(r,sz), GOAddr);
    ocir.AddAssignOpArgRes();
END GenCmpXchg;

--------------------------------------------------------------------------------
--
--  Вещественная арифметика
--
--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveTOS_M* (a-: AddrMode; sz: SizeType);
VAR op : r2b.MoveTOS_M;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill(a,sz);
    IF sz < 10 THEN ocir.prepare_nextCP(op, def.FLD, 1, def.FP);
    ELSE ocir.prepare_nextCP(op, def.FLD, 3, def.FNP);
    END;
    DD (a);
    M(a);
    ocir.SetDestMem(MAddr);
    ocir.AddDestIndirectArg();
    ocir.AddOpArgR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResR(def.Regs{def.ST0..def.ST7});
    ocir.AddDestRes();
    ocir.AddOpResF(def.Flags{def.C1});
    ocir.CheckMoveLocal();
END GenMoveTOS_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveTOS_STi* (i : LONGINT);
VAR op : r2b.MoveTOS_STi;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill(i);
    ocir.prepare_nextCP(op, def.FLD, 1, def.FP);
    ocir.SetDestReg(VAL(def.Register, VAL(SHORTINT,def.ST0) +VAL(SHORTINT,i)));
    ocir.AddOpArgR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResF(def.Flags{def.C1});
END GenMoveTOS_STi;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveTOS_INum* (w: VALUE; sz: SizeType);
VAR op     : r2b.MoveTOS_INum;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill(w,sz);
    IF sz < 10 THEN
        ocir.prepare_nextCP(op, def.FLD, 1, def.FP);
    ELSE
        ocir.prepare_nextCP(op, def.FLD, 3, def.FNP);
    END;
    ocir.SetDestImm();
    ocir.AddOpArgR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResF(def.Flags{def.C1});
END GenMoveTOS_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFILD* (a-: AddrMode; sz: SizeType);
VAR op : r2b.FILD;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill(a,sz);
    ocir.prepare_nextCP(op, def.FILD, 3, def.FNP);
    DD (a);
    M (a);
    ocir.SetDestMem(MAddr);
    ocir.AddDestArg();
    ocir.AddOpArgR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResF(def.Flags{def.C1});
END GenFILD;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveM_TOS* (a-: AddrMode; sz: SizeType);
VAR op : r2b.MoveM_TOS;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill(a,sz);
    ocir.prepare_nextP(op, def.FSTP, def.FNP);
    INCL(ocir.c_seg.attrs,ocir.WITH_FSTP);
    DD (a);
    M (a);
    ocir.SetOpClocks(2+VAL(SHORTINT,ORD(sz>8)));
    ocir.SetDestMem(MAddr);
    ocir.AddDestIndirectArg();
    ocir.AddOpArgR(def.Regs{def.ST0..def.ST7});
    ocir.AddDestRes();
    ocir.AddOpResR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResF(def.Flags{def.C1});
    ocir.CheckMoveLocal();
END GenMoveM_TOS;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveSTi_TOS* (i: LONGINT);
VAR op : r2b.MoveSTi_TOS;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill(i);
    ocir.prepare_nextCP(op, def.FSTP, 2, def.FNP);
    INCL(ocir.c_seg.attrs,ocir.WITH_FSTP);
    ocir.SetDestReg(VAL(def.Register, VAL(SHORTINT,def.ST0)+VAL(SHORTINT,i)));
    ocir.AddOpArgR(def.Regs{def.ST0});
    ocir.AddOpResR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResF(def.Flags{def.C1});
END GenMoveSTi_TOS;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenMoveSTi_ST0* (i: LONGINT);
VAR op : r2b.MoveSTi_ST0;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill(i);
    ocir.prepare_nextCP(op, def.FST, 2, def.FNP);
    ocir.SetDestReg(VAL(def.Register, VAL(SHORTINT,def.ST0)+VAL(SHORTINT,i)));
    ocir.AddOpArgR(def.Regs{def.ST0});
    ocir.AddOpResR(def.Regs{VAL(def.Register, VAL(SHORTINT,def.ST0)+i)});
    ocir.AddOpResF(def.Flags{def.C1});
END GenMoveSTi_ST0;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFOpSTi_TOS* (op: D.FloatOp; r: SHORTINT);
VAR _op : r2b.FOpSTi_TOS;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(_op);
    _op.Fill(op,r);
    CASE op OF
    | D.FADD,
      D.FMUL,
      D.FSUB,
      D.FSUBR:
        ocir.prepare_nextP(_op, def.GetFOpCode(op,TRUE), def.FP);
        ocir.SetOpClocks(3);
    | D.FDIV,
      D.FDIVR:
        ocir.prepare_nextP(_op, def.GetFOpCode(op,TRUE), def.FNP);
        ocir.SetOpClocks(39);
    END;
    ocir.SetOpR_R(VAL(def.Register, VAL(SHORTINT,def.ST0)+r), def.ST0);
    ocir.AddOpArgR(def.Regs{def.ST0, VAL(def.Register, VAL(SHORTINT,def.ST0)+r)});
    ocir.AddOpResR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResF(def.Flags{def.C1});
END GenFOpSTi_TOS;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFOpSTi_ST0* (op: D.FloatOp; r: SHORTINT);
VAR _op : r2b.FOpSTi_ST0;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(_op);
    _op.Fill(op,r);
    CASE op OF
    | D.FADD,
      D.FMUL,
      D.FSUB,
      D.FSUBR:
        ocir.prepare_nextP(_op, def.GetFOpCode(op,FALSE), def.FP);
        ocir.SetOpClocks(3);
    | D.FDIV,
      D.FDIVR:
        ocir.prepare_nextP(_op, def.GetFOpCode(op,FALSE), def.FNP);
        ocir.SetOpClocks(39);
    END;
    ocir.SetOpR_R(VAL(def.Register, VAL(SHORTINT,def.ST0)+r), def.ST0);
    ocir.AddOpArgR(def.Regs{def.ST0, VAL(def.Register, VAL(SHORTINT,def.ST0)+r)});
    ocir.AddOpResR(def.Regs{VAL(def.Register, VAL(SHORTINT,def.ST0)+r)});
    ocir.AddOpResF(def.Flags{def.C1});
END GenFOpSTi_ST0;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFOpST0_STi* (op: D.FloatOp; r: SHORTINT);
VAR _op : r2b.FOpST0_STi;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(_op);
    _op.Fill(op,r);
    CASE op OF
    | D.FADD,
      D.FMUL,
      D.FSUB,
      D.FSUBR:
        ocir.prepare_nextP(_op, def.GetFOpCode(op,FALSE), def.FP);
        ocir.SetOpClocks(3);
    | D.FDIV,
      D.FDIVR:
        ocir.prepare_nextP(_op, def.GetFOpCode(op,FALSE), def.FNP);
        ocir.SetOpClocks(39);
    END;
    ocir.SetOpR_R(def.ST0, VAL(def.Register, VAL(SHORTINT,def.ST0)+r));
    ocir.AddOpArgR(def.Regs{def.ST0, VAL(def.Register, VAL(SHORTINT,def.ST0)+r)});
    ocir.AddOpResR(def.Regs{def.ST0});
    ocir.AddOpResF(def.Flags{def.C1});
END GenFOpST0_STi;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFOpST0_M* (op: D.FloatOp; a-: AddrMode; sz: SizeType);
VAR _op : r2b.FOpST0_M;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(_op);
    _op.Fill(op,a,sz);
    DD (a);
    M (a);
    CASE op OF
    | D.FADD, D.FMUL,
      D.FSUB, D.FSUBR:
        ocir.prepare_nextP(_op, def.GetFOpCode(op,FALSE), def.FP);
        ocir.SetOpClocks(3);
    | D.FDIV, D.FDIVR:
        ocir.prepare_nextP(_op, def.GetFOpCode(op,FALSE), def.FNP);
        ocir.SetOpClocks(39);
    END;
    ocir.SetDestMem(MAddr);
    ocir.AddDestArg();
    ocir.AddOpArgR(def.Regs{def.ST0});
    ocir.AddOpResR(def.Regs{def.ST0});
    ocir.AddOpResF(def.Flags{def.C1});
    ocir.CheckMoveLocal();
END GenFOpST0_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFOpST0_IM* (op: D.FloatOp; a-: AddrMode; sz: SizeType);
VAR _op : r2b.FOpST0_IM;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(_op);
    _op.Fill(op,a,sz);
    DD (a);
    M (a);
    CASE op OF
    | D.FADD, D.FMUL,
      D.FSUB, D.FSUBR:
        ocir.prepare_nextP(_op, def.GetFOpCode(op,FALSE), def.FP);
        ocir.SetOpClocks(7);
    | D.FDIV, D.FDIVR:
        ocir.prepare_nextP(_op, def.GetFOpCode(op,FALSE), def.FNP);
        ocir.SetOpClocks(42);
    END;
    ocir.SetDestMem(MAddr);
    ocir.AddDestArg();
    ocir.AddOpArgR(def.Regs{def.ST0});
    ocir.AddOpResR(def.Regs{def.ST0});
    ocir.AddOpResF(def.Flags{def.C1});
END GenFOpST0_IM;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFOpST0_INum* (op: D.FloatOp; w: VALUE; sz: SizeType);
VAR _op : r2b.FOpST0_INum;
BEGIN
    NEW(_op);
    _op.Fill(op,w,sz);
    CASE op OF
    | D.FADD, D.FMUL,
      D.FSUB, D.FSUBR:
        ocir.prepare_nextP(_op, def.GetFOpCode(op,FALSE), def.FP);
        ocir.SetOpClocks(3);
    | D.FDIV, D.FDIVR:
        ocir.prepare_nextP(_op, def.GetFOpCode(op,FALSE), def.FNP);
        ocir.SetOpClocks(39);
    END;
    ocir.SetDestImm();
    ocir.AddOpArgR(def.Regs{def.ST0});
    ocir.AddOpResR(def.Regs{def.ST0});
    ocir.AddOpResF(def.Flags{def.C1});
END GenFOpST0_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFOp* (code: D.FloatOp);
VAR op : r2b.FOp;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill(code);
    ocir.GenFOp(op,code);
END GenFOp;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFComTOS_TOS*;
VAR op : r2b.FComTOS_TOS;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill();
    ocir.prepare_nextCP(op, def.FCOMP, 4, def.FP);
    ocir.AddOpArgR(def.Regs{def.ST0, def.ST1});
    ocir.AddOpResR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResF(def.ALL_CFLAGS);
END GenFComTOS_TOS;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFComTOS_INum* (w: VALUE; sz: SizeType);
VAR op : r2b.FComTOS_INum;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill(w,sz);
    ocir.prepare_nextCP(op, def.FCOMP, 4, def.FP);
    ocir.SetDestImm();
    ocir.AddOpArgR(def.Regs{def.ST0});
    ocir.AddDestArg();
    ocir.AddOpResR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResF(def.ALL_CFLAGS);
END GenFComTOS_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFComTOS_M* (a-: AddrMode; sz: SizeType);
VAR op : r2b.FComTOS_M;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill(a,sz);
    ocir.prepare_nextCP(op, def.FCOMP, 4, def.FP);
    DD (a);
    M (a);
    ocir.SetDestMem(MAddr);
    ocir.AddOpArgR(def.Regs{def.ST0});
    ocir.AddOpResR(def.Regs{def.ST0..def.ST7});
    ocir.AddDestArg();
    ocir.AddOpResF(def.ALL_CFLAGS);
END GenFComTOS_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFComTOS_STi* (r: SHORTINT);
VAR op : r2b.FComTOS_STi;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill(r);
    ocir.prepare_nextCP(op, def.FCOMP, 4, def.FP);
    ocir.SetDestReg(VAL(def.Register, VAL(SHORTINT,def.ST0)+r));
    ocir.AddOpArgR(def.Regs{def.ST0});
    ocir.AddOpResR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResF(def.ALL_CFLAGS);
END GenFComTOS_STi;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFComTOS_IM* (a-: AddrMode; sz: SizeType);
VAR op : r2b.FComTOS_M;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill(a,sz);
    ocir.prepare_nextCP(op, def.FICOMP, 8, def.FNP);
    DD (a);
    M (a);
    ocir.SetDestMem(MAddr);
    ocir.AddOpArgR(def.Regs{def.ST0});
    ocir.AddDestArg();
    ocir.AddOpResR(def.Regs{def.ST0..def.ST7});
    ocir.AddOpResF(def.ALL_CFLAGS);
END GenFComTOS_IM;


--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenFstsw*;
VAR op : r2b.Fstsw;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill();
    ocir.prepare_nextCP(op, def.FSTSW, 5, def.FNP);
    ocir.SetDestReg(def.EAX);
    ocir.AddOpArgF(def.ALL_CFLAGS);
    ocir.AddDestRes();
END GenFstsw;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfRrdPtr) GenCallToOrdinal* (card: BOOLEAN; u, m: D.PhysRegSet; f: BOOLEAN; sz: SizeType);
VAR op : r2b.CallToOrdinal;
BEGIN
    at.UseFloatOps := TRUE;
    NEW(op);
    op.Fill(card,u,m,f,sz);
    ocir.prepare_nextCP(op, def.CALL, 1, def.PV);
    INCL(ocir.c_seg.code[ocir.c_seg.code_len-1].attrs.tag_attrs,
         def.DANG_OP);
    ocir.SetDestImm();
    ocir.AddOpArgR(def.Regs{def.ESP} + u);
    ocir.AddOpArgRA(def.Regs{def.ESP});
    ocir.AddOpResR(def.Regs{def.ESP} + m);
    ocir.AddOpResF(def.ALL_AFLAGS);
    IF f THEN
      ocir.AddOpResF(def.Flags{def.C1});
    END;
END GenCallToOrdinal;

--------------------------------------------------------------------------------

PROCEDURE InitReordering*();
BEGIN
    IF RE = NIL THEN
        NEW (RE);
        r2b.BE := Emit.full;
    END;
    Emit.full := RE;
    ocir.InitProcSegms();
    reorder.InitReordering();
END InitReordering;

PROCEDURE CompleteReordering*();
BEGIN
    ocir.FreeProcSegms;
END CompleteReordering;

END EmitRrd.
