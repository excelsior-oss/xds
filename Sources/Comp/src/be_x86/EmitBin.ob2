<*-IOVERFLOW*>
<*-COVERFLOW*>

MODULE EmitBin;

IMPORT ir,
       CodeDef,
       Calc,
       BitVect,
       SYSTEM,
       opStd,
       Emit,
       at := opAttrs,
       pc := pcK,
       prc := opProcs
;
IMPORT D := desc386;
IMPORT RD := RDefs;
<* IF ~nodebug THEN *>
IMPORT opIO;
<* END *>
IMPORT def := opDef;


IMPORT Color;
IMPORT R:=r386;
IMPORT ObjNames;
IMPORT InOut;


TYPE
    INT         = ir.INT;
    ScaleType   = D.ScaleType;
    VALUE       = ir.VALUE;
    TriadePtr   = ir.TriadePtr;
    Node        = ir.Node;
    Local       = ir.Local;
    AddrMode    = Emit.AddrMode;
    BitVector   = BitVect.BitVector;
    PhysReg     = D.PhysReg;
    Reg         = D.Reg;
    RegSet      = D.RegSet;
    SizeType    = Emit.SizeType;
--------------------------------------------------------------------------------

TYPE
    SelfBinPtr *= POINTER TO SelfBinRec;
    SelfBinRec *= RECORD (Emit.SelfRec)
                  END;

--------------------------------------------------------------------------------

VAR sg: CodeDef.CODE_SEGM;          -- Текущий генерируемый сегмент
    cur_node: Node; -- *shell

--------------------------------------------------------------------------------

(*
  Щелевая оптимизация
*)

VAR
    LAST_FLAGS_OP:     LONGINT;
    LAST_FLAGS_REG:    PhysReg;    -- С каким регистром выполнялась последняя
                                    --   команда, устанавливающая флажки
    LAST_FLAGS_SIZE:   SizeType;    -- размер этого регистра
    LAST_FLAGS_OFFSET: LONGINT;     -- смещение этой команды (в текущем узле)

    LAST_FSTP_OFFSET:  LONGINT;     -- Для оптимизации FSTP FLD -> FST
    LAST_FCODE_OFFSET: LONGINT;
    LAST_FSTP_ADDR:    AddrMode;

--------------------------------------------------------------------------------
--
--      Служебные подпрограммы
--
--------------------------------------------------------------------------------

PROCEDURE EqAddrModes (a1-, a2-: AddrMode): BOOLEAN;
BEGIN
    RETURN (a1.place1.r  = a2.place1.r)  &
           (a1.place2.r  = a2.place2.r)  &
           (a1.scale = a2.scale) &
           (a1.offs  = a2.offs)  &
           (a1.local = a2.local) &
           (a1.offslist = a2.offslist)     &
           (a1.proc  = a2.proc)  &
           (a1.proclist= a2.proclist)
END EqAddrModes;

--------------------------------------------------------------------------------

PROCEDURE SetLastFlags (op: LONGINT; r: PhysReg; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
    LAST_FLAGS_OP     := op;
    LAST_FLAGS_REG    := r;
    LAST_FLAGS_SIZE   := sz;
    LAST_FLAGS_OFFSET := sg.code_len;
END SetLastFlags;

--------------------------------------------------------------------------------

PROCEDURE B (b: LONGINT);
BEGIN
    CodeDef.GenByte (SYSTEM.VAL (SYSTEM.BYTE, b));
END B;

--------------------------------------------------------------------------------

PROCEDURE B2 (b1, b2: LONGINT);
BEGIN
    B (b1);
    B (b2);
END B2;

--------------------------------------------------------------------------------

PROCEDURE B3 (b1, b2, b3: LONGINT);
BEGIN
    B (b1);
    B (b2);
    B (b3);
END B3;

--------------------------------------------------------------------------------

PROCEDURE B4 (b1, b2, b3, b4: LONGINT);
BEGIN
    B (b1);
    B (b2);
    B (b3);
    B (b4);
END B4;

--------------------------------------------------------------------------------

PROCEDURE W (w: LONGINT);
BEGIN
    CodeDef.GenWord (SYSTEM.VAL (INTEGER, w));
END W;

--------------------------------------------------------------------------------

CONST L = CodeDef.GenLWord;

--------------------------------------------------------------------------------

PROCEDURE GenLocal (l: ir.VarNum);
VAR codedef_sg: CodeDef.CODE_SEGM;
BEGIN
    CodeDef.get_segm(codedef_sg);
    ASSERT(codedef_sg = sg);
    CodeDef.add_fixup (ir.Locals^[l].Obj, 0, sg.code_len, CodeDef.fx_obj32);
END GenLocal;

--------------------------------------------------------------------------------

PROCEDURE GenProc (p: prc.ProcNum);
VAR codedef_sg: CodeDef.CODE_SEGM;
BEGIN
    CodeDef.get_segm(codedef_sg);
    ASSERT(codedef_sg = sg);
    CodeDef.add_fixup (prc.ProcObj (p), 0, sg.code_len, CodeDef.fx_obj32);
END GenProc;

--------------------------------------------------------------------------------

PROCEDURE IsShort (v: LONGINT; sz: SHORTINT): BOOLEAN;
BEGIN
    ASSERT(sz<=4);
    RETURN (sz <> 1) & (v >= -128) & (v <=  127);
END IsShort;

--------------------------------------------------------------------------------

(*
  1) Посчитать displacement
  2) Выдать, полное оно или нет
*)

PROCEDURE CalcOffset (VAR offs: INT; VAR full: BOOLEAN; a-: AddrMode);
VAR op: Emit.OffsRecPtr;
    o:  INT;
    f:  BOOLEAN;
BEGIN
    o := a.offs;
    f := FALSE;
    IF a.local <> ir.UNDEFINED THEN
        IF ir.IsExternal (a.local) THEN
            f := TRUE;
        ELSE
            INC (o, ir.Locals^[a.local].Offset +
                    Emit.LocalsOffset);
            IF Emit.baseReg = D.ESP THEN
                INC (o, Emit.PushSize);
            END;
        END;
    END;
    op := a.offslist;
    WHILE op <> NIL DO
        IF ir.IsExternal (op^.name) THEN
            f := TRUE;
        ELSE
            INC (o, ir.Locals^[op^.name].Offset +
                    Emit.LocalsOffset);
            IF Emit.baseReg = D.ESP THEN
                INC (o, Emit.PushSize);
            END;
        END;
        op := op^.next;
    END;
    IF a.proc <> ir.ProcUNDEFINED THEN
        f := TRUE;
    END;
    full := f OR (o < -128) OR (o > 127);
    offs := o;
END CalcOffset;

--------------------------------------------------------------------------------

PROCEDURE O (o: INT; a-: AddrMode);
VAR op: Emit.OffsRecPtr;
    pp: Emit.ProcRecPtr;
BEGIN
    IF a.local <> ir.UNDEFINED THEN
        IF ir.IsExternal (a.local) THEN
            GenLocal (a.local);
        END;
        op := a.offslist;
        WHILE op <> NIL DO
            IF ir.IsExternal (op^.name) THEN
                GenLocal (op^.name);
            END;
            op := op^.next;
        END;
    END;
    IF a.proc <> ir.ProcUNDEFINED THEN
        GenProc (a.proc);
        pp := a.proclist;
        WHILE pp <> NIL DO
            GenProc (pp^.name);
            pp := pp^.next;
        END;
    END;
    L (o);
END O;

--------------------------------------------------------------------------------

PROCEDURE GO (a-: AddrMode);
VAR o: INT;
    f: BOOLEAN;
BEGIN
    CalcOffset (o, f, a);
    O (o, a);
END GO;

--------------------------------------------------------------------------------

(*
  Выдать способ адресации, вставив в mod-r/m байт-параметр
*)

PROCEDURE M (a-: AddrMode; modrm: LONGINT);
VAR full:   BOOLEAN;
    o:      INT;
    r1, r2: PhysReg;
BEGIN
    r1 := D.RegInfo[a.place1.r].code;
    r2 := D.RegInfo[a.place2.r].code;
    IF (r1 = D.EBPp) & (r2 <> D.UNDEF_REGp) & (a.scale = D.x1) THEN
        r1 := r2;
        r2 := D.EBPp;
    ELSIF r2 = D.ESPp THEN
        ASSERT ((r1 <> D.ESPp) & (a.scale = D.x1));
        r2 := r1;
        r1 := D.ESPp;
    END;
    CalcOffset (o, full, a);
    IF (r1 = D.UNDEF_REGp) & (r2 = D.UNDEF_REGp) THEN
        B (modrm + 5);
        O (o, a);
        RETURN;
    END;
    ASSERT(a.place1.r IN (D.AllowedIRegsWEBPWUNDEF [4]
                         +D.AllowedIRegsWEBP [2]
                         +D.AllowedIRegsWEBP [1]
                         +RegSet{D.ESP}+RegSet{D.BPlow,D.SIlow,D.DIlow}));
    ASSERT(a.place2.r IN (D.AllowedIRegsWEBPWUNDEF [4]
                         +D.AllowedIRegsWEBP [2]
                         +D.AllowedIRegsWEBP [1]
                         +RegSet{D.ESP}+RegSet{D.BPlow,D.SIlow,D.DIlow}));
    ASSERT((a.place1.r # D.UNDEF_REG)OR (a.place2.r # D.UNDEF_REG));
    IF (r1 <> D.ESPp) & (r2 = D.UNDEF_REGp) THEN
        IF NOT full & (o = 0) & (r1 <> D.EBPp) THEN
            B (modrm + ORD(r1));
        ELSIF NOT full THEN
            B2 (40H + modrm + ORD(r1), o);
        ELSE
            B (80H + modrm + ORD(r1));
            O (o, a);
        END;
    ELSIF (r1 = D.ESPp) & (r2 = D.UNDEF_REGp) THEN
        IF full THEN
            B2 (84H + modrm, ASH (ORD(D.ESPp), 3) + ORD(D.ESPp));
            O (o, a);
        ELSIF o = 0 THEN
            B2 (4 + modrm, ASH (ORD(D.ESPp), 3) + ORD(D.ESPp));
        ELSE
            B3 (44H + modrm, ASH (ORD(D.ESP), 3) + ORD(D.ESP), o);
        END;
    ELSIF r1 = D.UNDEF_REGp THEN
        B2 (4 + modrm, ORD(a.scale) + ASH (ORD(r2), 3) + 5);
        O (o, a);
    ELSIF NOT full & (r1 <> D.EBPp) & (o = 0) THEN
        B2 (4 + modrm, ORD(a.scale) + ASH (ORD(r2), 3) + ORD(r1));
    ELSIF NOT full THEN
        B3 (44H + modrm, ORD(a.scale) + ASH (ORD(r2), 3) + ORD(r1), o);
    ELSE
        B2 (84H + modrm, ORD(a.scale) + ASH (ORD(r2), 3) + ORD(r1));
        O (o, a);
    END;
END M;

--------------------------------------------------------------------------------

PROCEDURE BM (b: LONGINT; a-: AddrMode; modrm: LONGINT);
BEGIN
    B (b);
    M (a, modrm);
END BM;

--------------------------------------------------------------------------------

PROCEDURE V (v: LONGINT; sz: SHORTINT; short: BOOLEAN);
BEGIN
    ASSERT(sz<=4);
    IF short OR (sz = 1) THEN
        B (v);
    ELSIF sz = 2 THEN
        W (v);
    ELSE
        L (v);
    END;
END V;

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
        B (3EH);
    END;
END DD;

--------------------------------------------------------------------------------

PROCEDURE P (sz: SHORTINT);
BEGIN
    ASSERT(sz<=4);
    IF sz = 2 THEN
        B (066H);
    END;
END P;

--------------------------------------------------------------------------------

PROCEDURE BFM (b: LONGINT; x: VALUE; modrm: LONGINT; sz: SizeType);
BEGIN
    IF sz = 12 THEN
        sz := 10;
    END;
    at.UseFloatOps := TRUE;
    B2 (b, modrm + 5);
    CodeDef.gen_fixup (CodeDef.new_float (x, sz), 0, CodeDef.fx_obj32);
END BFM;

--------------------------------------------------------------------------------
--
--      Вход в линейный участок, координаты, etc...
--
--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) EnterNode* (n: Node; seg: CodeDef.CODE_SEGM);
VAR i: Emit.Trap;
BEGIN
    sg := seg;
    cur_node := n; -- *shell

    CodeDef.set_segm (seg);
    Emit.PushSize  := 0;
    Emit.FloatSize := 0;
    LAST_FLAGS_REG    := D.UNDEF_REGp;
    LAST_FLAGS_OFFSET := MAX (LONGINT) DIV 2;
    LAST_FLAGS_SIZE   := 0;
    LAST_FSTP_OFFSET  := MAX (LONGINT) DIV 2;
END EnterNode;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) SetSegment* (n: Node; seg: CodeDef.CODE_SEGM);
BEGIN
    sg := seg;
    CodeDef.set_segm (seg);
END SetSegment;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) EmptySegment* (n: Node;
                                            sg: CodeDef.CODE_SEGM): BOOLEAN;
BEGIN
    RETURN sg.code_len = 0;
END EmptySegment;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) AddPosition* (pos-: ir.TPOS);
<* IF ~nodebug THEN *>
  VAR ln,col: LONGINT;   fname: pc.STRING;
<* END *>
BEGIN
    IF NOT pos.IsNull () THEN
<* IF ~nodebug THEN *>
   IF opIO.needed THEN
       pos.unpack(fname, ln, col);
       opIO.print("# [%d:%d]\n",ln+1,col+1);
   END;
<* END *>
        CodeDef.add_pos (sg.code_len, pos);
    END;
END AddPosition;

--------------------------------------------------------------------------------
--
--      Собственно генератор команд
--
--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveR_R* (d, s: PhysReg; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
    B2 (08AH + ORD (sz > 1), 0C0H + ASH (ORD(d), 3) + ORD(s));
    IF d <> LAST_FLAGS_REG THEN
        INC (LAST_FLAGS_OFFSET, 2);
    END;
END GenMoveR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveR_M* (r: PhysReg; a-: AddrMode; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
    b := (r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
    P (sz);
    DD (a);
    IF (r = D.EAXp) & (a.place1.r = D.UNDEF_REG) &
                        (a.place2.r = D.UNDEF_REG)
    THEN
        B  (0A0H + ORD (sz > 1));
        GO (a);
    ELSE
        BM (8AH + ORD (sz > 1), a, ASH (ORD(r), 3));
    END;
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenMoveR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveR_INum* (r: PhysReg; v: LONGINT; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
    b := (r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
    P (sz);
    B (0B0H + ASH (ORD (sz > 1), 3) + ORD(r));
    V (v, sz, FALSE);
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenMoveR_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveR_Iglobal* (r: PhysReg; a-: AddrMode; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
    ASSERT(sz<=4);
    b := (r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
    B2 (0C7H, 0C0H + ORD(r));
    GO (a);
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenMoveR_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveM_R* (a-: AddrMode; r: PhysReg; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
    b := (LAST_FLAGS_OFFSET = sg.code_len);
    P (sz);
    DD (a);
    IF (r = D.EAXp) & (a.place1.r = D.UNDEF_REG) &
                        (a.place2.r = D.UNDEF_REG)
    THEN
        B  (0A2H + ORD (sz > 1));
        GO (a);
    ELSE
        BM (88H + ORD (sz > 1), a, ASH (ORD(r), 3));
    END;
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenMoveM_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveM_INum* (a-: AddrMode; v: LONGINT; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
    b := (LAST_FLAGS_OFFSET = sg.code_len);
    P (sz);
    DD (a);
    BM (0C6H + ORD (sz > 1), a, 0);
    V (v, sz, FALSE);
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenMoveM_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveM_Iglobal* (a-, ag-: AddrMode; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
    b := (LAST_FLAGS_OFFSET = sg.code_len);
    ASSERT (sz = 4);
    DD (a);
    B (0C7H);
    M (a, 0);
    GO (ag);
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenMoveM_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenXchgR_R* (d, s: PhysReg; sz: SizeType);
BEGIN
    IF sz = 1 THEN
        B2 (086H, 0C0H + ASH (ORD(s), 3) + ORD(d));
    ELSE
        P (sz);
        IF d = D.EAXp THEN
            B (090H + ORD(s));
        ELSIF s = D.EAXp THEN
            B (090H + ORD(d));
        ELSE
            B2 (087H, 0C0H + ASH (ORD(s), 3) + ORD(d));
        END;
    END;
END GenXchgR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenXchgR_M* (r: PhysReg; a-: AddrMode; sz: SizeType);
BEGIN
    P (sz);
    DD (a);
    BM (86H + ORD (sz > 1), a, ASH (ORD(r), 3));
END GenXchgR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenPush_R* (r: PhysReg);
BEGIN
    B (50H + ORD(r));
    INC (LAST_FLAGS_OFFSET);
END GenPush_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenPush_M* (a-: AddrMode; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
    ASSERT((sz=4) OR (sz=2));
    P (sz);
    b := (LAST_FLAGS_OFFSET = sg.code_len);
    DD (a);
    BM (0FFH, a, ASH (6, 3));
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenPush_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenPush_INum* (v: LONGINT; sz: SizeType);
VAR b, s: BOOLEAN;
BEGIN
    ASSERT(sz<=4);
    b := (LAST_FLAGS_OFFSET = sg.code_len);
    s := (sz = 1) OR IsShort (v, 4);
    B (68H + ASH (ORD (s), 1));
    V (v, 4, s);
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenPush_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenPush_Iglobal* (a-: AddrMode);
VAR b: BOOLEAN;
BEGIN
    b := (LAST_FLAGS_OFFSET = sg.code_len);
    B  (68H);
    GO (a);
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenPush_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenPop_R* (r: PhysReg);
BEGIN
    B (58H + ORD(r));
    IF r <> LAST_FLAGS_REG THEN
        INC (LAST_FLAGS_OFFSET);
    END;
END GenPop_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenOpINum_R* (ttt: D.BinaryOp; v: VALUE; sz: SizeType;
                                                r: PhysReg );
VAR o:pc.OBJECT;
    dummy:LONGINT;
    seg,old: CodeDef.CODE_SEGM;
BEGIN
    ASSERT(FALSE);
(*    P (sz);
    CASE ttt OF
    | D.TTT_bt:
        B2( 0FH ,0A3H);
        CodeDef.gen_fixup (o, 0, CodeDef.fx_obj32);
        B ( ASH (ORD(r), 3));
    END;
*)
END GenOpINum_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenOpR_R* (ttt: D.BinaryOp; d, s: PhysReg;
                                        sz: SizeType);
BEGIN
    P (sz);
    CASE ttt OF
    | D.TTT_mul:
       B3 (0FH, 0AFH, 0C0H + ASH (ORD(d), 3) + ORD(s));
    | D.TTT_bt:
       B3 (0FH, 0A3H, 0C0H + ASH (ORD(s), 3) + ORD(d));
    | D.TTT_bts:
       B3 (0FH, 0ABH, 0C0H + ASH (ORD(s), 3) + ORD(d));
    | D.TTT_btr:
       B3 (0FH, 0B3H, 0C0H + ASH (ORD(s), 3) + ORD(d));
    ELSE
        B2 (ASH (ttt, 3) + 2 + ORD (sz > 1), 0C0H + ASH (ORD(d), 3) + ORD(s));
        IF ttt <> D.TTT_cmp THEN
            SetLastFlags (ORD(ttt), d, sz);
        END;
    END;
END GenOpR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenOpR_M* (ttt: D.BinaryOp; r: PhysReg;
                                        a-: AddrMode; sz: SizeType);
BEGIN
    P (sz);
    DD (a);
    IF ttt = D.TTT_mul THEN
        B2 (0FH, 0AFH);
        M  (a, ASH (ORD(r), 3));
    ELSE
        BM (ASH (ttt, 3) + 2 + ORD (sz > 1), a, ASH (ORD(r), 3));
        IF ttt <> D.TTT_cmp THEN
            SetLastFlags (ORD(ttt), r, sz);
        END;
    END;
END GenOpR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenOpR_INum* (ttt: D.BinaryOp; r: PhysReg;
                                           v: LONGINT; sz: SizeType);
VAR s: BOOLEAN;
BEGIN
    ASSERT(sz<=4);
    CASE ttt OF
    | D.TTT_mul:
        P (sz);
        s := IsShort (v, sz);
        B2 (69H + ASH (ORD (s), 1), 0C0H + ASH (ORD(r), 3) + ORD(r));
        V (v, sz, s);
    | D.TTT_bt:
       s := IsShort (v, 1);
       B3 (0FH, 0BAH, 0C0H + ASH (4, 3) + ORD(r));
       V(v, 1, s);
    | D.TTT_bts:
       s := IsShort (v, 1);
       B3 (0FH, 0BAH, 0C0H + ASH (6, 3) + ORD(r));
       V(v, 1, s);
    | D.TTT_btr:
       s := IsShort (v, 1);
       B3 (0FH, 0BAH, 0C0H + ASH (5, 3) + ORD(r));
       V(v, 1, s);
    ELSE
        IF sz = 1 THEN
            IF r = D.ALp THEN
                B2 (ASH (ttt, 3) + 4, v);
            ELSE
                B3 (80H, 0C0H + ASH (ttt, 3) + ORD(r), v);
            END;
        ELSE
            P (sz);
            s := IsShort (v, sz);
            IF (r = D.EAXp) & ((sz = 2) OR NOT s) THEN
                B (ASH (ttt, 3) + 5);
                s := FALSE;
            ELSE
                B2 (81H + ASH (ORD (s), 1), 0C0H + ASH (ttt, 3) + ORD(r));
            END;
            V (v, sz, s);
        END;
        IF ttt <> D.TTT_cmp THEN
            SetLastFlags (ORD(ttt), r, sz);
        END;
    END;
END GenOpR_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenOpR_Iglobal* (ttt: D.BinaryOp; r: PhysReg;
                                              a-: AddrMode; sz: SizeType);
BEGIN
    ASSERT (sz = 4);
    IF ttt = D.TTT_mul THEN
        B2 (0C9H, 0C0H + ASH (ORD(r), 3) + ORD(r));
    ELSIF r = D.EAXp THEN
        B (ASH (ttt, 3) + 5);
    ELSE
        B2 (81H, 0C0H + ASH (ttt, 3) + ORD(r));
    END;
    GO (a);
END GenOpR_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenOpM_R* (ttt: D.BinaryOp; a-: AddrMode;
                                        r: PhysReg; sz: SizeType);
BEGIN
    P (sz);
    DD (a);
    CASE ttt OF
    | D.TTT_bt:
        B( 0FH );
        BM (0A3H, a, ASH (ORD(r), 3));
    | D.TTT_bts:
        B( 0FH );
        BM (0ABH, a, ASH (ORD(r), 3));
    | D.TTT_btr:
        B( 0FH );
        BM (0B3H, a, ASH (ORD(r), 3));
    ELSE
        BM (ASH (ttt, 3) + ORD (sz > 1), a, ASH (ORD(r), 3));
    END;
END GenOpM_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenOpM_INum* (ttt: D.BinaryOp; a-: AddrMode;
                                           v: LONGINT; sz: SizeType);
VAR s: BOOLEAN;
BEGIN
    P (sz);
    DD (a);
    s := IsShort (v, sz);
    BM (80H + ASH (ORD (s), 1) + ORD (sz > 1), a, ASH (ttt, 3));
    V (v, sz, s);
END GenOpM_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenOpM_Iglobal* (ttt: D.BinaryOp; a-, ag-: AddrMode;
                                              sz: SizeType);
BEGIN
    ASSERT (sz = 4);
    DD (a);
    B (81H);
    M (a, ASH (ttt, 3));
    GO (ag);
END GenOpM_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenDiv_R* (r: PhysReg; sz: SizeType;
                                        signed: BOOLEAN);
BEGIN
    P (sz);
    B2 (0F6H + ORD (sz > 1), 0C0H + ASH (6 + ORD (signed), 3) + ORD(r));
END GenDiv_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenDiv_M* (a-: AddrMode; sz: SizeType;
                                        signed: BOOLEAN);
BEGIN
    P (sz);
    DD (a);
    B (0F6H + ORD (sz > 1));
    M (a, ASH (6 + ORD (signed), 3));
END GenDiv_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenDiv_INum* (v: LONGINT; sz: SizeType;
                                           signed: BOOLEAN);
BEGIN
    P (sz);
    B2 (0F6H + ORD (sz > 1), ASH (6 + ORD (signed), 3) + 5);
    CodeDef.gen_fixup (CodeDef.new_integer (Calc.GetInteger (v, sz),
                                            ir.t_int, sz),
                        0, CodeDef.fx_obj32);
END GenDiv_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMul_R* (r: PhysReg; sz: SizeType;
                                        signed: BOOLEAN);
BEGIN
    P (sz);
    B2 (0F6H + ORD (sz > 1), 0C0H + ASH (4 + ORD (signed), 3) + ORD(r));
END GenMul_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMul_M* (a-: AddrMode; sz: SizeType;
                                        signed: BOOLEAN);
BEGIN
    P (sz);
    DD (a);
    B (0F6H + ORD (sz > 1));
    M (a, ASH (4 + ORD (signed), 3));
END GenMul_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMul_INum* (v: LONGINT; sz: SizeType;
                                           signed: BOOLEAN);
BEGIN
    P (sz);
    B2 (0F6H + ORD (sz > 1), ASH (4 + ORD (signed), 3) + 5);
    CodeDef.gen_fixup (CodeDef.new_integer (Calc.GetInteger (v, sz),
                                            ir.t_int, sz),
                        0, CodeDef.fx_obj32);
END GenMul_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenIMulR_RC* (d, s: PhysReg; v: LONGINT;
                                           sz: SizeType);
VAR sh: BOOLEAN;
BEGIN
    P (sz);
    sh := IsShort (v, sz);
    B2 (69H + ASH (ORD (sh), 1), 0C0H + ASH (ORD(d), 3) + ORD(s));
    V (v, sz, sh);
END GenIMulR_RC;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenIMulR_MC* (r: PhysReg; a-: AddrMode;
                                           v: LONGINT; sz: SizeType);
VAR s: BOOLEAN;
BEGIN
    P (sz);
    DD (a);
    s := IsShort (v, sz);
    BM (69H + ASH (ORD (s), 1), a, ASH (ORD(r), 3));
    V (v, sz, s);
END GenIMulR_MC;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenTestR_R* (r1, r2: PhysReg; sz: SizeType);
BEGIN
    IF (r1 <> r2) OR (r1 <> LAST_FLAGS_REG) OR (sz <> LAST_FLAGS_SIZE) OR
       (sg.code_len <> LAST_FLAGS_OFFSET) OR (VAL(D.BinaryOp, LAST_FLAGS_OP) = D.TTT_add)
    THEN
        P (sz);
        B2 (84H + ORD (sz > 1), 0C0H + ASH (ORD(r1), 3) + ORD(r2));
        IF r1 = r2 THEN
            SetLastFlags (ORD(D.TTT_and), r1, sz);
        END;
    END;
END GenTestR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenTestR_INum* (r: PhysReg; v: LONGINT; sz: SizeType);
BEGIN
    P (sz);
    IF r = D.EAXp THEN
        B (0A8H + ORD (sz > 1));
    ELSE
        B2 (0F6H + ORD (sz > 1), 0C0H + ORD(r));
    END;
    V (v, sz, FALSE);
END GenTestR_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenTestM_INum* (a-: AddrMode; v: LONGINT; sz: SizeType);
BEGIN
    P (sz);
    DD (a);
    BM (0F6H + ORD (sz > 1), a, 0);
    V (v, sz, FALSE);
END GenTestM_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenLEA* (r: PhysReg; a-: AddrMode);
BEGIN
    BM (8DH, a, ASH (ORD(r), 3));
END GenLEA;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMovesxR_M* (r: PhysReg; a-: AddrMode; rsz, sz: SizeType);
VAR b: BOOLEAN;
BEGIN
    b := (r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
    P (rsz);
    DD (a);
    B2 (0FH, 0BEH + ORD (sz > 1));
    M (a, ASH (ORD(r), 3));
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenMovesxR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMovesxR_R* (d, s: PhysReg; rsz, sz: SizeType);
VAR b: BOOLEAN;
BEGIN
    b := (d <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
    IF (rsz = 4) & (sz = 2) & (d = D.EAXp) & (s = D.AXp) THEN
        B (98H);
    ELSIF (rsz = 2) & (sz = 1) & (d = D.AXp) & (s = D.ALp) THEN
        B2 (66H, 98H);
    ELSE
        P  (rsz);
        B3 (0FH, 0BEH + ORD (sz > 1), 0C0H + ASH (ORD(d), 3) + ORD(s));
    END;
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenMovesxR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMovezxR_M* (r: PhysReg; a-: AddrMode; rsz, sz: SizeType);
VAR b: BOOLEAN;
BEGIN
    b := (r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
    DD (a);
    P (rsz);
    B2 (0FH, 0B6H + ORD (sz > 1));
    M (a, ASH (ORD(r), 3));
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenMovezxR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMovezxR_R* (d, s: PhysReg; rsz, sz: SizeType);
VAR b: BOOLEAN;
BEGIN
    b := (d <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
    P  (rsz);
    B3 (0FH, 0B6H + ORD (sz > 1), 0C0H + ASH (ORD(d), 3) + ORD(s));
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenMovezxR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenOpR* (ttt: D.UnaryOp; r: PhysReg; sz: SizeType);
BEGIN
    P (sz);
    IF (ttt = D.TTT_inc) OR (ttt = D.TTT_dec) THEN
        IF sz = 1 THEN
            B2 (0FEH, 0C0H + ASH (ttt, 3) + ORD(r));
        ELSE
            B (040H + ASH (ttt, 3) + ORD(r));
        END;
    ELSE
        B2 (0F6H + ORD (sz > 1), 0C0H + ASH (ttt, 3) + ORD(r));
    END;
    IF ttt # D.TTT_not THEN
        SetLastFlags (ORD(ttt), r, sz);
    END;
END GenOpR;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenOpM* (ttt: D.UnaryOp; a-: AddrMode; sz: SizeType);
BEGIN
    P (sz);
    DD (a);
    IF (ttt = D.TTT_inc) OR (ttt = D.TTT_dec) THEN
        BM (0FEH + ORD (sz > 1), a, ASH (ttt, 3));
    ELSE
        BM (0F6H + ORD (sz > 1), a, ASH (ttt, 3));
    END;
END GenOpM;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenSetC_R* (r: PhysReg; c: D.Condition);
BEGIN
    B3 (0FH, 90H + ORD(c), 0C0H + ORD(r));
END GenSetC_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenSetC_M* (a-: AddrMode; c: D.Condition);
BEGIN
    B2 (0FH, 90H + ORD(c));
    M  (a, 0);
END GenSetC_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenShiftR_R* (ttt: D.ShiftOp; r: PhysReg;
                                           sz: SizeType);
BEGIN
    P (sz);
    B2 (0D2H + ORD (sz > 1), 0C0H + ASH (ttt, 3) + ORD(r));
END GenShiftR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenShiftR_INum* (ttt: D.ShiftOp; r: PhysReg;
                                              v: LONGINT; sz: SizeType);
BEGIN
    P (sz);
    B3 (0C0H + ORD (sz > 1), 0C0H + ASH (ttt, 3) + ORD(r), v);
END GenShiftR_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenShiftM_INum* (ttt: D.ShiftOp; a-: AddrMode;
                                              v: LONGINT; sz: SizeType);
BEGIN
    P (sz);
    DD (a);
    BM (0C0H + ORD (sz > 1), a, ASH (ttt, 3));
    B (v);
END GenShiftM_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenCBW*;
VAR b: BOOLEAN;
BEGIN
    b := (LAST_FLAGS_OFFSET = sg.code_len);
    B (66H);
    B (98H);
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenCBW;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenCDQ* (sz: SizeType);
VAR b: BOOLEAN;
BEGIN
    b := (LAST_FLAGS_OFFSET = sg.code_len);
    P (sz);
    B (99H);
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenCDQ;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenRepMovSD*;
BEGIN
    B2 (0F3H, 0A5H);
END GenRepMovSD;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMovSD*;
BEGIN
    B (0A5H);
END GenMovSD;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMovSW*;
BEGIN
    B2 (66H, 0A5H);
END GenMovSW;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenRepMovSB*;
BEGIN
    B2 (0F3H, 0A4H);
END GenRepMovSB;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMovSB*;
BEGIN
    B (0A4H);
END GenMovSB;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenSahf*;
BEGIN
    B (09EH);
END GenSahf;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenJ* (j: D.Condition; o: LONGINT; long: BOOLEAN);
BEGIN
    IF j = D.UnJ THEN
        IF long THEN
            B (0E9H);
            L (o);
        ELSE
            B2 (0EBH, o);
        END;
    ELSIF long THEN
        B2 (0FH, 80H + ORD(j));
        L (o);
    ELSE
        B2 (70H + ORD(j), o);
    END;
END GenJ;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenBinJ* (j: D.Condition; o: LONGINT; long: BOOLEAN);
BEGIN
    self.GenJ (j, o, long)
END GenBinJ;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenCall_M* (a-: AddrMode; u, m: D.PhysRegSet;
                                         r, w: BitVector);
BEGIN
    DD (a);
    BM (0FFH, a, ASH (2, 3));
END GenCall_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenCall_R* (r: PhysReg; u, m: D.PhysRegSet;
                                         rd, w: BitVector);
BEGIN
    B2 (0FFH, 0C0H + ASH (2, 3) + ORD(r));
END GenCall_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenCall_INum* (v: LONGINT; u, m: D.PhysRegSet;
                                            r, w: BitVector);
BEGIN
    ASSERT (FALSE);
END GenCall_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenCall_Iglobal* (a-: AddrMode; u, m: D.PhysRegSet;
                                               r, w: BitVector);
VAR s: CodeDef.CODE_SEGM;
BEGIN
    IF prc.IsCodeProc (a.proc) THEN
        s := prc.GetCodeProc (a.proc, Emit.LocalsOffset);
        CodeDef.AddSegment (s);
    ELSE
        B (0E8H);
        CodeDef.gen_fixup (prc.ProcObj (a.proc), 0, CodeDef.fx_relcall);
    END;
END GenCall_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenRet* (n: INT);
BEGIN
    IF n = 0 THEN
        B (0C3H);
    ELSE
        ASSERT (n < 65535);
        B (0C2H);
        W (n);
    END;
END GenRet;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenCase* (r: PhysReg; v: LONGINT; lb: Emit.LABEL);
BEGIN
    IF Emit.DSneqSS THEN
        B (2EH);
    END;
    B3 (0FFH, 4 + ASH (4, 3), ORD(D.x4) + ASH (ORD(r), 3) + 5);
    L  (ASH (v, 2));
END GenCase;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveR_Table* (r, i: PhysReg; t: pc.OBJECT;
                                             sz: SizeType);
BEGIN
    P (sz);
    B3 (8AH + ORD (sz > 1), 4 + ASH (ORD(r), 3), ORD(D.x4) + ASH (ORD(i), 3) + 5);
    CodeDef.gen_fixup (t, 0, CodeDef.fx_obj32);
END GenMoveR_Table;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenOpR_Table* (ttt: D.BinaryOp; r, i: PhysReg;
                                           t: pc.OBJECT; sz: SizeType);
BEGIN
    P (sz);
    B3 (ASH (ttt, 3) + 2 + ORD (sz > 1), 4 + ASH (ORD(r), 3),
        ORD(D.x4) + ASH (ORD(i), 3) + 5);
    CodeDef.gen_fixup (t, 0, CodeDef.fx_obj32);
    SetLastFlags (ORD(ttt), r, sz);
END GenOpR_Table;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenTestR_Table* (r, i: PhysReg; t: pc.OBJECT;
                                              sz: SizeType);
BEGIN
    P (sz);
    B3 (84H + ORD (sz > 1), 4 + ASH (ORD(r), 3), ORD(D.x4) + ASH (ORD(i), 3) + 5);
    CodeDef.gen_fixup (at.setop_table (ir.o_incl), 0, CodeDef.fx_obj32);
END GenTestR_Table;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenHalt*;
BEGIN
    B (0E8H);
    CodeDef.gen_fixup (opStd.Proc (opStd.X2C_HALT), 0, CodeDef.fx_relcall);
END GenHalt;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenRaise* (is_assert: BOOLEAN);
  VAR p: pc.OBJECT;
BEGIN
    IF is_assert THEN
        p := opStd.Proc (opStd.X2C_ASSERT_F);
    ELSE
        p := opStd.Proc (opStd.X2C_TRAP_F);
    END;
    B (0E8H);
    CodeDef.gen_fixup (p, 0, CodeDef.fx_relcall);
END GenRaise;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenSkipTrap* (j:D.Condition; intno: Emit.Trap; pos-: ir.TPOS);
VAR i: INT;
    b: BOOLEAN;
    a: Emit.AddrMode;
    ln,col: LONGINT;
    fname: pc.STRING;
BEGIN
    b := (LAST_FLAGS_OFFSET = sg.code_len);
    IF intno = Emit.IntOTrap THEN
        B (0CEH);
    ELSE
        i := Emit.Traps [intno];
        IF i < 0 THEN
            IF at.OptimizeTraps IN at.COMP_MODE THEN
                Emit.AddLocalFixupList(intno, cur_node, sg.code_len, Emit.InverseCond(j));
                B2(90H, 90H);
            ELSE
                ASSERT(ir.modname_local # ir.UNDEFINED);
                pos.unpack(fname, ln, col);
                -- gen skip trapcall
                B2 (70H + ORD(j), 12+3*VAL(INTEGER,~IsShort(ln,4)));
                -- push line number
                self.GenPush_INum(ln,4);
                -- push filename
                Emit.InitAddrMode(a);
                a.local := ir.modname_local;
                a.offs := 0;
                self.GenPush_Iglobal(a);
                -- gen trap call
                B(0E8H);
                CodeDef.gen_fixup (opStd.Proc (- i), 0, CodeDef.fx_relcall);
            END;
        ELSE
            B4 (70H + ORD(j), 2, 0CDH, i);
        END;
    END;
    IF b THEN
        LAST_FLAGS_OFFSET := sg.code_len;
    END;
END GenSkipTrap;

--------------------------------------------------------------------------------
--
--  Вещественная арифметика
--
--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveTOS_M* (a-: AddrMode; sz: SizeType);
BEGIN
    at.UseFloatOps := TRUE;
    IF ((sz = 4) OR (sz = 8)) &
        (LAST_FSTP_OFFSET = sg.code_len) &
        EqAddrModes (a, LAST_FSTP_ADDR)
    THEN
        sg.bcode^[LAST_FCODE_OFFSET] :=
            SYSTEM.VAL (SYSTEM.BYTE,
                    SYSTEM.VAL (SYSTEM.CARD8, sg.bcode^[LAST_FCODE_OFFSET]) -
                    VAL (SYSTEM.CARD8, ASH (1, 3)));
        LAST_FSTP_OFFSET  := MAX (LONGINT) DIV 2;
    ELSE
        DD (a);
        CASE sz OF
        | 4:    BM (0D9H, a, ASH (0, 3));
        | 8:    BM (0DDH, a, ASH (0, 3));
        | 10,
          12:   BM (0DBH, a, ASH (5, 3));
        END;
    END;
END GenMoveTOS_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveTOS_STi* (i: LONGINT);
BEGIN
    at.UseFloatOps := TRUE;
    B2 (0D9H, 0C0H + i);
END GenMoveTOS_STi;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveTOS_INum* (w: VALUE; sz: SizeType);
BEGIN
    at.UseFloatOps := TRUE;
    CASE sz OF
    | 4:    BFM (0D9H, w, ASH (0, 3), 4);
    | 8:    BFM (0DDH, w, ASH (0, 3), 8);
    | 10,
      12:   BFM (0DBH, w, ASH (5, 3), 10);
    END;
END GenMoveTOS_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFILD* (a-: AddrMode; sz: SizeType);
BEGIN
    at.UseFloatOps := TRUE;
    DD (a);
    CASE sz OF
    | 2:    BM (0DFH, a, ASH (0, 3));
    | 4:    BM (0DBH, a, ASH (0, 3));
    | 8:    BM (0DFH, a, ASH (5, 3));
    END;
END GenFILD;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveM_TOS* (a-: AddrMode; sz: SizeType);
BEGIN
    at.UseFloatOps := TRUE;
--    IF LAST_FSTP_OFFSET <> sg.code_len THEN   -- Нельзя из-за новых
                                                -- оптимизаций - надо еще
                                                -- учитывать FLOAT_SIZE
        LAST_FSTP_ADDR := a;
--    END;
    DD (a);
    LAST_FCODE_OFFSET := sg.code_len + 1;
    CASE sz OF
    | 4:    BM (0D9H, a, ASH (3, 3));
    | 8:    BM (0DDH, a, ASH (3, 3));
    | 10,
      12:   BM (0DBH, a, ASH (7, 3));
    END;
    LAST_FSTP_OFFSET := sg.code_len;
END GenMoveM_TOS;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveM_ST0* (a-: AddrMode; sz: SizeType);
BEGIN
    at.UseFloatOps := TRUE;
    DD (a);
    CASE sz OF
    | 4:    BM (0D9H, a, ASH (2, 3));
    | 8:    BM (0DDH, a, ASH (2, 3));
    END;
END GenMoveM_ST0;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveSTi_TOS* (i: LONGINT);
BEGIN
    at.UseFloatOps := TRUE;
    B2 (0DDH, 0D8H + i);
END GenMoveSTi_TOS;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenMoveSTi_ST0* (i: LONGINT);
BEGIN
    at.UseFloatOps := TRUE;
    B2 (0DDH, 0D0H + i);
END GenMoveSTi_ST0;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFOpSTi_TOS* (op: D.FloatOp; r: SHORTINT);
BEGIN
    at.UseFloatOps := TRUE;
    CASE op OF
    | D.FADD:    B2 (0DEH, 0C0H + r);
    | D.FMUL:    B2 (0DEH, 0C8H + r);
    | D.FDIV:    B2 (0DEH, 0F8H + r);
    | D.FDIVR:   B2 (0DEH, 0F0H + r);
    | D.FSUB:    B2 (0DEH, 0E8H + r);
    | D.FSUBR:   B2 (0DEH, 0E0H + r);
    END;
END GenFOpSTi_TOS;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFOpSTi_ST0* (op: D.FloatOp; r: SHORTINT);
BEGIN
    at.UseFloatOps := TRUE;
    CASE op OF
    | D.FADD:    B2 (0DCH, 0C0H + r);
    | D.FMUL:    B2 (0DCH, 0C8H + r);
    | D.FDIV:    B2 (0DCH, 0F8H + r);
    | D.FDIVR:   B2 (0DCH, 0F0H + r);
    | D.FSUB:    B2 (0DCH, 0E8H + r);
    | D.FSUBR:   B2 (0DCH, 0E0H + r);
    END;
END GenFOpSTi_ST0;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFOpST0_STi* (op: D.FloatOp; r: SHORTINT);
BEGIN
    at.UseFloatOps := TRUE;
    CASE op OF
    | D.FADD:    B2 (0D8H, 0C0H + r);
    | D.FMUL:    B2 (0D8H, 0C8H + r);
    | D.FDIV:    B2 (0D8H, 0F0H + r);
    | D.FDIVR:   B2 (0D8H, 0F8H + r);
    | D.FSUB:    B2 (0D8H, 0E0H + r);
    | D.FSUBR:   B2 (0D8H, 0E8H + r);
    END;
END GenFOpST0_STi;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFOpST0_M* (op: D.FloatOp; a-: AddrMode; sz: SizeType);
BEGIN
    at.UseFloatOps := TRUE;
    DD (a);
    IF sz = 4 THEN
        BM (0D8H, a, ASH (op, 3));
    ELSE
        BM (0DCH, a, ASH (op, 3));
    END;
END GenFOpST0_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFOpST0_IM* (op: D.FloatOp; a-: AddrMode; sz: SizeType);
BEGIN
    at.UseFloatOps := TRUE;
    DD (a);
    IF sz = 4 THEN
        BM (0DAH, a, ASH (op, 3));
    ELSE
        BM (0DEH, a, ASH (op, 3));
    END;
END GenFOpST0_IM;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFOpST0_INum* (op: D.FloatOp; w: VALUE; sz: SizeType);
BEGIN
    IF sz = 4 THEN
        BFM (0D8H, w, ASH (op, 3), sz);
    ELSE
        BFM (0DCH, w, ASH (op, 3), sz);
    END;
END GenFOpST0_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFOp* (code: D.FloatOp);
BEGIN
    at.UseFloatOps := TRUE;
    B2 (0D9H, ORD(code));
END GenFOp;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFComTOS_TOS*;
BEGIN
    at.UseFloatOps := TRUE;
    B2 (0DEH, 0D9H);
END GenFComTOS_TOS;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFComTOS_INum* (w: VALUE; sz: SizeType);
BEGIN
    at.UseFloatOps := TRUE;
    IF sz = 4 THEN
        BFM (0D8H, w, ASH (3, 3), sz);
    ELSE
        BFM (0DCH, w, ASH (3, 3), sz);
    END;
END GenFComTOS_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFComTOS_M* (a-: AddrMode; sz: SizeType);
BEGIN
    at.UseFloatOps := TRUE;
    DD (a);
    IF sz = 4 THEN
        BM (0D8H, a, ASH (3, 3));
    ELSE
        BM (0DCH, a, ASH (3, 3));
    END;
END GenFComTOS_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFComTOS_STi* (r: SHORTINT);
BEGIN
    at.UseFloatOps := TRUE;
    B2 (0D8H, 0D8H + r);
END GenFComTOS_STi;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFComTOS_IM* (a-: AddrMode; sz: SizeType);
BEGIN
    at.UseFloatOps := TRUE;
    DD (a);
    IF sz = 4 THEN
        BM (0DAH, a, ASH (3, 3));
    ELSE
        BM (0DEH, a, ASH (3, 3));
    END;
END GenFComTOS_IM;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenFstsw*;
BEGIN
    at.UseFloatOps := TRUE;
    B2 (0DFh, 0E0H);
END GenFstsw;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfBinPtr) GenCallToOrdinal* (card: BOOLEAN; u, m: D.PhysRegSet; f: BOOLEAN; sz: SizeType);
  VAR p: pc.OBJECT;
BEGIN
    at.UseFloatOps := TRUE;
    B (0E8H);
    IF sz = 8 THEN
        p := opStd.Proc (opStd.X2J_TRUNCI64);
    ELSIF card THEN
        p := opStd.Proc (opStd.X2C_TRUNCC);
    ELSE
        p := opStd.Proc (opStd.X2C_TRUNCI);
    END;
    CodeDef.gen_fixup (p, 0, CodeDef.fx_relcall);
END GenCallToOrdinal;

--------------------------------------------------------------------------------

VAR e: SelfBinPtr;

PROCEDURE InitOutput*;
BEGIN
    NEW (e);
    Emit.full := e;
END InitOutput;

PROCEDURE (self: SelfBinPtr) EndGenSkipTrap*();
VAR
  i: Emit.Trap;
  n: Node;
  seg: CodeDef.CODE_SEGM;

BEGIN
  n := Color.GenOrder[Color.TrapGenOrder];
  ASSERT(RD.NodeInfo^[n].sg = NIL);
  CodeDef.new_segm(RD.NodeInfo^[n].sg);
  ASSERT(RD.NodeInfo^[n].sg <> NIL);
  RD.NodeInfo^[n].j := D.NoJ;

  FOR i:=MIN(Emit.Trap) TO MAX(Emit.Trap) DO
    IF Emit.TrapUsages[i].Used = TRUE THEN
      seg := RD.NodeInfo^[n].sg;
      CodeDef.set_segm(seg);
      Emit.TrapUsages[i].offset := seg.code_len;
      self.GenPush_INum(0,4);
      self.GenPush_INum(0,4);
      B(0E8H); -- call
      CodeDef.gen_fixup(opStd.Proc (-Emit.Traps[i]), 0, CodeDef.fx_relcall);

    END; -- IF
  END; -- FOR
END EndGenSkipTrap;

PROCEDURE (self: SelfBinPtr) CalcSkipTrapJumps*(i: Color.GenOrdNode): BOOLEAN;
VAR
  tmp, tmp1: POINTER TO Emit.LinkedList;
  j: Color.GenOrdNode;
  t: Emit.Trap;
  n: Node;
  trap_fixup_offset: LONGINT;
  added_code_len: LONGINT;
  seg, old_sg: CodeDef.CODE_SEGM;
  changed, res_changed: BOOLEAN;
BEGIN
  n := Color.GenOrder^[i];
  res_changed := FALSE;

  tmp := Emit.TrapFixupList;
  WHILE tmp<>NIL DO
    IF (tmp^.node = n) & (RD.NodeInfo^[n].sg.code_len <> 0) THEN
      -- проверка длины сегмента кода линейного участка нужна чтобы
      -- определить, не выкосился ли линейный участок процедурой LinkProc.UniteSegments;
      changed := FALSE;
      seg := RD.NodeInfo^[n].sg;

      IF tmp^.l THEN
        trap_fixup_offset := 6;
      ELSE
        trap_fixup_offset := 2;
      END;

      trap_fixup_offset := RD.NodeInfo^[Color.GenOrder^[Color.TrapGenOrder]].o -
        trap_fixup_offset - tmp^.offset - RD.NodeInfo^[n].o + Emit.TrapUsages[tmp^.trap].offset;

      CodeDef.get_segm(old_sg);
      CodeDef.set_segm(seg);

      added_code_len := 0;
      -- если переход нужен длинный, а до этого был короткий
      IF ((trap_fixup_offset>127) OR (-128>trap_fixup_offset)) AND NOT tmp^.l THEN
        changed := TRUE;
        tmp^.l := TRUE;
        CodeDef.MoveCodeFromTo(seg, tmp^.offset+2, tmp^.offset+6);
        INC(added_code_len, 4);

      -- если переход нужен короткий, а до этого был длинный
      ELSIF ((-128<=trap_fixup_offset) AND (trap_fixup_offset<=127)) AND tmp^.l THEN
        changed := TRUE;
        tmp^.l := FALSE;
        CodeDef.MoveCodeFromTo(seg, tmp^.offset+6, tmp^.offset+2);
        DEC(added_code_len, 4);
      END;


      IF changed THEN
        tmp1 := Emit.TrapFixupList;
        WHILE tmp1<>NIL DO
          IF (tmp1^.node = n) AND (tmp1^.offset > tmp^.offset) THEN
            IF tmp^.l THEN
              -- переход удлиннился (был коротким)
              INC(tmp1^.offset, 4);
            ELSE
              -- переход укоротился (был длинным)
              DEC(tmp1^.offset, 4);
            END;
          END;
          tmp1 := tmp1^.next;
        END;

        FOR j:=SYSTEM.SUCC(i) TO Color.EndGenOrder DO
          INC(RD.NodeInfo^[Color.GenOrder^[j]].o, added_code_len);
        END;

      END;

      CodeDef.set_segm(old_sg);
      res_changed := res_changed OR changed;
    END;
    tmp := tmp^.next;
  END;

  RETURN res_changed;

END CalcSkipTrapJumps;







PROCEDURE (self: SelfBinPtr) GenSkipTrapJumps*();
VAR
  tmp: POINTER TO Emit.LinkedList;
  j: Color.GenOrdNode;
  trap_fixup_offset: LONGINT;
  n: Node;
  b0, b1, b2, b3: SYSTEM.BYTE;
  seg: CodeDef.CODE_SEGM;
BEGIN

  FOR j:=Color.StartGenOrder TO Color.EndGenOrder DO

    tmp := Emit.TrapFixupList;
    WHILE tmp<>NIL DO
      n := Color.GenOrder^[j];

      IF (tmp^.node = n) & (RD.NodeInfo^[n].sg.code_len <> 0) THEN
        -- проверка длины сегмента кода линейного участка нужна чтобы
        -- определить, не выкосился ли линейный участок процедурой LinkProc.UniteSegments;
        seg := RD.NodeInfo^[n].sg;

        CodeDef.set_segm(seg);

        IF tmp^.l THEN
          trap_fixup_offset := 6;
        ELSE
          trap_fixup_offset := 2;
        END;

        trap_fixup_offset := RD.NodeInfo^[Color.GenOrder^[Color.TrapGenOrder]].o -
          trap_fixup_offset - tmp^.offset - RD.NodeInfo^[n].o + Emit.TrapUsages[tmp^.trap].offset;


        IF NOT tmp^.l THEN
          ASSERT((-128<=trap_fixup_offset) & (trap_fixup_offset<=127));

          b0 := SYSTEM.VAL(SYSTEM.BYTE, 070H + ORD(tmp^.cond) );
          b1 := SYSTEM.VAL(SYSTEM.BYTE, trap_fixup_offset MOD 256);

          ASSERT( seg.code_len > (tmp^.offset+1) );
          CodeDef.GenByteBToPos(b0, tmp^.offset);
          CodeDef.GenByteBToPos(b1, tmp^.offset+1);

        ELSE
          ASSERT(NOT ((-128<=trap_fixup_offset) & (trap_fixup_offset<=127)));

          b0 := 0FH;
          b1 := SYSTEM.VAL(SYSTEM.BYTE, 080H + ORD(tmp^.cond) );

          ASSERT( seg.code_len > (tmp^.offset+5) );
          CodeDef.GenByteBToPos(b0, tmp^.offset);
          CodeDef.GenByteBToPos(b1, tmp^.offset+1);


          b3 := SYSTEM.VAL(SYSTEM.BYTE, trap_fixup_offset DIV (65536*256) );
          b2 := SYSTEM.VAL(SYSTEM.BYTE, (trap_fixup_offset MOD (65536*256)) DIV 65536);
          b1 := SYSTEM.VAL(SYSTEM.BYTE, (trap_fixup_offset MOD 65536) DIV 256);
          b0 := SYSTEM.VAL(SYSTEM.BYTE, trap_fixup_offset MOD 256);

          CodeDef.GenByteBToPos(b0, tmp^.offset+2);
          CodeDef.GenByteBToPos(b1, tmp^.offset+3);
          CodeDef.GenByteBToPos(b2, tmp^.offset+4);
          CodeDef.GenByteBToPos(b3, tmp^.offset+5);

        END;

      END; -- IF

      tmp := tmp^.next;

    END; -- WHILE

  END; -- FOR

END GenSkipTrapJumps;


END EmitBin.
