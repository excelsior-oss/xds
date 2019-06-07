(*
  Интерфейс с собственно генератором кода. Состоит из:
  1. Описания машинных регистров, способов адресации, ...
  2. ОО интерфейса с нижним уровнем - генератом двоичного представления
     машинных команд; описаны 3 интерфейсных объекта:
        - пустой emitter
        - "рабочий" (т.е. выполняющий собственно генерацию кода) emitter,
          ему кто-нибудь должен присвоить значение
        - текущий emitter - туда надо присваивать или тот, или другой
  3. Процедурного интерфейса с "верхним" уровнем (реально с r386.ob2) -
     выполняет мелкие оптимизации на лету; иногда генерирует несколько
     команд при одном вызове.
*)

<*-IOVERFLOW*>
<*-COVERFLOW*>

MODULE Emit;

IMPORT ir,
       CodeDef,
       Calc,
       BitVect,
       SYSTEM,
       RealMath,
       LongMath,
       opStd,
       xiEnv,
       at := opAttrs,
       pc := pcK,
       tune := opTune
;
IMPORT D := desc386;
--IMPORT opIO;
IMPORT opProcs;
IMPORT Color;-- *shell
--------------------------------------------------------------------------------

TYPE
    INT         = ir.INT;
    ScaleType   = D.ScaleType;
    SizeType    *= ir.SizeType;
    FLOAT       = ir.FLOAT;
    PhysReg      = D.PhysReg;
    Reg          = D.Reg;
    RegSet       = D.RegSet;
    VALUE       = ir.VALUE;
    TriadePtr   = ir.TriadePtr;
    ParamPtr    = ir.ParamPtr;
    Node        = ir.Node;
    Local       = ir.Local;
    VarNum      = ir.VarNum;
    TypeType    = ir.TypeType;
    BitVector   = BitVect.BitVector;

TYPE
    SelfPtr  *= POINTER TO SelfRec;
    SelfRec  *= RECORD
                END;

--------------------------------------------------------------------------------

VAR
    empty-  : SelfPtr;  -- Пустой emitter
    full*   : SelfPtr;  -- "Рабочий" emitter
    work*   : SelfPtr;  -- Текущий используемый emitter - или full, или work
    trace*  : SelfPtr;  -- вместо пустого для трассировки

--------------------------------------------------------------------------------

CONST UNDEF_REG *= D.UNDEF_REG;
CONST SPILLED_REGS = D.SPILLED_REGS;

    TTT_add = D.TTT_add;           -- Бинарные операции
    TTT_or  = D.TTT_or ;
    TTT_adc = D.TTT_adc;
    TTT_sbb = D.TTT_sbb;
    TTT_and = D.TTT_and;
    TTT_sub = D.TTT_sub;
    TTT_xor = D.TTT_xor;
    TTT_cmp = D.TTT_cmp;
    TTT_bt  = D.TTT_bt ;         -- Работа с битами
    TTT_bts = D.TTT_bts;         -- Работа с битами
    TTT_btr = D.TTT_btr;         -- Работа с битами
                     
    TTT_mul = D.TTT_mul;
                     
    TTT_inc = D.TTT_inc;           -- Унарные операции
    TTT_dec = D.TTT_dec;
    TTT_not = D.TTT_not;
    TTT_neg = D.TTT_neg;
                     
    TTT_rol = D.TTT_rol;           -- Сдвиги
    TTT_ror = D.TTT_ror;
    TTT_shl = D.TTT_shl;
    TTT_shr = D.TTT_shr;
    TTT_sar = D.TTT_sar;

TYPE
    Trap *= (
      NilTrap     ,
      RangeTrap   ,
      IndexTrap   ,
      OverflowTrap,
      DivideTrap  ,
      IntOTrap
    );


VAR
    Traps-:         ARRAY Trap OF LONGINT;
    DSneqSS-:       BOOLEAN;
    GuardPage-:     BOOLEAN;    -- Выделять память на стеке
                                -- кусками не более чем по 4K
    LocalsOffset*: INT;        -- смещение переменной
                                -- относительно записанного
                                -- в ir.Locals^[l].Offset
                                -- смещения
    PushSize*:     INT;        -- сколько натолкано в стек
                                -- командами PUSH
    FloatSize*:    INT;        -- сколько чисел в стеке
                                -- сопроцессора
    baseReg*         : Reg;


TYPE -- *shell

    LinkedList   *= RECORD -- *shell
                        trap*: Trap;
                        node*: Node; -- номер узла, в котором эта команда
                        offset*: LONGINT; -- смещение команды в линейном участке
                        l*: BOOLEAN; -- длинный или не более 128
                        cond*: D.Condition; -- условие перехода
                        next*: POINTER TO LinkedList;
                    END;
                                    

    TrapUsage    *= RECORD -- *shell
                        Used*: BOOLEAN;
                        offset*: LONGINT;
                    END;

VAR -- *shell
    TrapUsages*:    ARRAY Trap OF TrapUsage; -- *shell
    TrapFixupList*: POINTER TO LinkedList;




TYPE
    RegPlacement *= RECORD
                        v*: VarNum;     -- если r = UNDEF_REG
                        r*: Reg;   -- UNDEF_REG или регистр
                    END;

    OffsRecPtr   *= POINTER TO OffsRec; -- Список смещений (локалов и глобалов)
    OffsRec      *= RECORD
                        name*: Local;
                        next*: OffsRecPtr;
                    END;

    ProcRecPtr   *= POINTER TO ProcRec; -- Список смещений (процедур)
    ProcRec      *= RECORD
                        name*: opProcs.ProcNum;
                        next*: ProcRecPtr;
                    END;

    AddrMode     *= RECORD
                        place1*, place2*: RegPlacement;
                        scale*:   ScaleType;
                        offs*:    LONGINT;
                        local*:   Local;
                        offslist*:       OffsRecPtr;
                        proc*:    opProcs.ProcNum;
                        proclist*:ProcRecPtr;
                        bv*:      BitVector;    -- may be NIL
                    END;

--------------------------------------------------------------------------------
(*
  Инициализировать AddrMode
*)

PROCEDURE InitAddrMode* (VAR a: AddrMode);
BEGIN
    a.place1.v  := ir.UNDEFINED;  a.place1.r  := UNDEF_REG;
    a.place2.v  := ir.UNDEFINED;  a.place2.r  := UNDEF_REG;
    a.scale := D.x1;
    a.local := ir.UNDEFINED;
    a.proc  := ir.ProcUNDEFINED;
    a.offs  := 0;
    a.offslist     := NIL;
    a.proclist := NIL;
    a.bv    := NIL;
END InitAddrMode;

--------------------------------------------------------------------------------

PROCEDURE UncheckedMove* (VAR d, s: ARRAY OF SYSTEM.BYTE; j, l: INT);
VAR i: INT;
BEGIN
    FOR i:=0 TO l-1 DO
        d [i] := s [j];
        INC (j);
    END;
END UncheckedMove;

--------------------------------------------------------------------------------

PROCEDURE GetIntVal* (value: VALUE; ndword: SHORTINT; sz: SizeType): LONGINT;
BEGIN
  IF ndword > 0 THEN
    RETURN value.get_NDWord(ndword);
  END;
  IF Calc.IsNegative (value, sz) THEN
    RETURN Calc.ToInteger (value, sz);
  ELSE
    RETURN SYSTEM.VAL (LONGINT,
      Calc.ToCardinal (value, sz));
  END;
END GetIntVal;

PROCEDURE GetVal* (p: ParamPtr; sz: SizeType): LONGINT;
VAR l: LONGINT;
    x: REAL;
BEGIN
    CASE p.tag OF
    | ir.y_NumConst:    RETURN GetIntVal(p.value,0,sz);
    | ir.y_RealConst:   x := VAL (REAL, Calc.ToReal (p.value, sz));
                        UncheckedMove (l, x, 0, 4);
                        RETURN l;
    END;
END GetVal;

--------------------------------------------------------------------------------

(*
  Выдать по условию обратное к нему
*)

PROCEDURE InverseCond* (j: D.Condition): D.Condition;
BEGIN
    IF ODD (j) THEN
        RETURN SYSTEM.PRED(j);
    ELSE
        RETURN SYSTEM.SUCC(j);
    END;
END InverseCond;

--------------------------------------------------------------------------------

(*
  Вернуть наименьший размер, в который можно втиснуть вещественную константу.
*)

PROCEDURE SmallestSize* (v: FLOAT; sz: SizeType): SizeType;
VAR x, x0: REAL;
    y, y0: LONGREAL;
BEGIN
    CASE sz OF
    | 4:    RETURN 4;
    | 8:    IF ABS (v) <= VAL(FLOAT, MAX (REAL)) THEN
                y := VAL (LONGREAL, v);
                x := VAL (REAL, y);
                UncheckedMove (x0, x, 0, 4);
                IF VAL (LONGREAL, x0) = y THEN
                    RETURN 4;
                END;
            END;
    | 10,
      12:   IF ABS (v) <= VAL(FLOAT, MAX (REAL)) THEN
                x := VAL (REAL, v);
                UncheckedMove (x0, x, 0, 4);
                IF VAL (FLOAT, x0) = v THEN
                    RETURN 4;
                END;
            END;
            IF ABS (v) <= VAL(FLOAT, MAX (LONGREAL)) THEN
                y := VAL (LONGREAL, v);
                UncheckedMove (y0, y, 0, 8);
                IF VAL (FLOAT, y0) = v THEN
                    RETURN 8;
                END;
            END;
    END;
    RETURN sz;
END SmallestSize;

--------------------------------------------------------------------------------

(*
  Равны ли две позиции в тексте? Мы не можем пользоваться обычной
  процедурой, т.к. не надо учитывать позиции в строках.
*)

PROCEDURE PosEqu* (p1-, p2-: ir.TPOS): BOOLEAN;
VAR fname1, fname2: xiEnv.String;
    line1, line2, col1, col2: LONGINT;
BEGIN
    p1.unpack (fname1, line1, col1);
    p2.unpack (fname2, line2, col2);
    RETURN (fname1 = fname2) & (line1 = line2);
END PosEqu;

--------------------------------------------------------------------------------
--
--      Вход в линейный участок, координаты, etc...
--
--------------------------------------------------------------------------------

<* PUSH *>
<* WOFF301+ *>
PROCEDURE (self: SelfPtr) EnterNode* (n: Node; sg: CodeDef.CODE_SEGM);
BEGIN
    PushSize  := 0;
    FloatSize := 0;
END EnterNode;

(*
PROCEDURE (self: SelfPtr) AllocPlaceForShortJump*;
END AllocPlaceForShortJump;

PROCEDURE (self: SelfPtr) FetchJump*(cond: D.Condition);
END FetchJump;
*)

PROCEDURE (self: SelfPtr) SetSegment* (n: Node; sg: CodeDef.CODE_SEGM);
END SetSegment;

PROCEDURE (self: SelfPtr) EmptySegment* (n: Node;
                                         sg: CodeDef.CODE_SEGM): BOOLEAN;
BEGIN
    RETURN TRUE;
END EmptySegment;

PROCEDURE (self: SelfPtr) Flush*;
END Flush;

PROCEDURE (self: SelfPtr) AddPosition* (pos-: ir.TPOS);
END AddPosition;

PROCEDURE (self: SelfPtr) DisableOptimizations*;
END DisableOptimizations;

PROCEDURE (self: SelfPtr) EnableOptimizations*;
END EnableOptimizations;

PROCEDURE (self: SelfPtr) DisableMoveStores*;
END DisableMoveStores;

PROCEDURE (self: SelfPtr) EnableMoveStores*;
END EnableMoveStores;

--------------------------------------------------------------------------------
--
--      Метки
--
--------------------------------------------------------------------------------

TYPE
    LABEL *= LONGINT;

CONST
    UNDEF_LABEL *= LABEL{-1};
    ZEROLABEL   *= LABEL{ 0};

PROCEDURE (self: SelfPtr) NewLabel* (VAR x: LABEL);
END NewLabel;

PROCEDURE (self: SelfPtr) SetLabel* (x: LABEL);
END SetLabel;

PROCEDURE (self: SelfPtr) InsertLabel* (VAR x: LABEL);
END InsertLabel;

PROCEDURE (self: SelfPtr) DwLabel* (x: LABEL);
END DwLabel;

--------------------------------------------------------------------------------
--
--      Собственно генератор команд
--
--------------------------------------------------------------------------------

PROCEDURE (self: SelfPtr) GenMoveR_R* (d, s: PhysReg; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenMoveR_R;

PROCEDURE (self: SelfPtr) GenMoveR_M* (r: PhysReg; a-: AddrMode; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenMoveR_M;

PROCEDURE (self: SelfPtr) GenMoveR_INum* (r: PhysReg; v: LONGINT; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenMoveR_INum;

PROCEDURE (self: SelfPtr) GenMoveR_Iglobal* (r: PhysReg; a-: AddrMode; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenMoveR_Iglobal;

PROCEDURE (self: SelfPtr) GenMoveM_R* (a-: AddrMode; r: PhysReg; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenMoveM_R;

PROCEDURE (self: SelfPtr) GenMoveM_INum* (a-: AddrMode; v: LONGINT; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenMoveM_INum;

PROCEDURE (self: SelfPtr) GenMoveM_Iglobal* (a-, ag-: AddrMode; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenMoveM_Iglobal;

PROCEDURE (self: SelfPtr) GenXchgR_R* (d, s: PhysReg; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenXchgR_R;

PROCEDURE (self: SelfPtr) GenXchgR_M* (r: PhysReg; a-: AddrMode; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenXchgR_M;

PROCEDURE (self: SelfPtr) GenPush_R* (r: PhysReg);
END GenPush_R;

PROCEDURE (self: SelfPtr) GenPush_M* (a-: AddrMode;sz: SizeType);
END GenPush_M;

PROCEDURE (self: SelfPtr) GenPush_INum* (v: LONGINT; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenPush_INum;

PROCEDURE (self: SelfPtr) GenPush_Iglobal* (a-: AddrMode);
END GenPush_Iglobal;

PROCEDURE (self: SelfPtr) GenPop_R* (r: PhysReg);
END GenPop_R;

PROCEDURE (self: SelfPtr) GenOpR_R* (ttt: D.BinaryOp; d, s: PhysReg;
                                     sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenOpR_R;

PROCEDURE (self: SelfPtr) GenOpR_M* (ttt: D.BinaryOp; r: PhysReg;
                                     a-: AddrMode; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenOpR_M;

PROCEDURE (self: SelfPtr) GenOpR_INum* (ttt: D.BinaryOp; r: PhysReg;
                                        v: LONGINT; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenOpR_INum;

PROCEDURE (self: SelfPtr) GenOpINum_R* (ttt: D.BinaryOp; v: VALUE; sz: SizeType;
                                                r: PhysReg );
BEGIN
    ASSERT(sz<=4);
END GenOpINum_R;

PROCEDURE (self: SelfPtr) GenOpR_Iglobal* (ttt: D.BinaryOp; r: PhysReg;
                                           a-: AddrMode; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenOpR_Iglobal;

PROCEDURE (self: SelfPtr) GenOpM_R* (ttt: D.BinaryOp; a-: AddrMode;
                                     r: PhysReg; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenOpM_R;

PROCEDURE (self: SelfPtr) GenOpM_INum* (ttt: D.BinaryOp; a-: AddrMode;
                                        v: LONGINT; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenOpM_INum;

PROCEDURE (self: SelfPtr) GenOpM_Iglobal* (ttt: D.BinaryOp; a-, ag-: AddrMode;
                                           sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenOpM_Iglobal;

PROCEDURE (self: SelfPtr) GenDiv_R* (r: PhysReg; sz: SizeType;
                                     signed: BOOLEAN);
BEGIN
    ASSERT(sz<=4);
END GenDiv_R;

PROCEDURE (self: SelfPtr) GenDiv_M* (a-: AddrMode; sz: SizeType;
                                     signed: BOOLEAN);
BEGIN
    ASSERT(sz<=4);
END GenDiv_M;

PROCEDURE (self: SelfPtr) GenDiv_INum* (v: LONGINT; sz: SizeType;
                                        signed: BOOLEAN);
BEGIN
    ASSERT(sz<=4);
END GenDiv_INum;

PROCEDURE (self: SelfPtr) GenMul_R* (r: PhysReg; sz: SizeType;
                                     signed: BOOLEAN);
BEGIN
    ASSERT(sz<=4);
END GenMul_R;

PROCEDURE (self: SelfPtr) GenMul_M* (a-: AddrMode; sz: SizeType;
                                     signed: BOOLEAN);
BEGIN
    ASSERT(sz<=4);
END GenMul_M;

PROCEDURE (self: SelfPtr) GenMul_INum* (v: LONGINT; sz: SizeType;
                                        signed: BOOLEAN);
BEGIN
    ASSERT(sz<=4);
END GenMul_INum;

PROCEDURE (self: SelfPtr) GenIMulR_RC* (d, s: PhysReg; v: LONGINT;
                                        sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenIMulR_RC;

PROCEDURE (self: SelfPtr) GenIMulR_MC* (r: PhysReg; a-: AddrMode;
                                        v: LONGINT; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenIMulR_MC;

PROCEDURE (self: SelfPtr) GenTestR_R* (r1, r2: PhysReg; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenTestR_R;

PROCEDURE (self: SelfPtr) GenTestR_INum* (r: PhysReg; v: LONGINT; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenTestR_INum;

PROCEDURE (self: SelfPtr) GenTestM_INum* (a-: AddrMode; v: LONGINT; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenTestM_INum;

PROCEDURE (self: SelfPtr) GenLEA* (r: PhysReg; a-: AddrMode);
END GenLEA;

PROCEDURE (self: SelfPtr) GenMovesxR_M* (r: PhysReg; a-: AddrMode; rsz, sz: SizeType);
BEGIN
    ASSERT(rsz<=4);
  ASSERT(rsz > sz );
END GenMovesxR_M;

PROCEDURE (self: SelfPtr) GenMovesxR_R* (d, s: PhysReg; rsz, sz: SizeType);
BEGIN
    ASSERT(rsz<=4);
  ASSERT(rsz > sz );
END GenMovesxR_R;

PROCEDURE (self: SelfPtr) GenMovezxR_M* (r: PhysReg; a-: AddrMode; rsz, sz: SizeType);
BEGIN
    ASSERT(rsz<=4);
  ASSERT(rsz > sz );
END GenMovezxR_M;

PROCEDURE (self: SelfPtr) GenMovezxR_R* (d, s: PhysReg; rsz, sz: SizeType);
BEGIN
  ASSERT(rsz > sz );
    ASSERT(rsz<=4);
END GenMovezxR_R;

PROCEDURE (self: SelfPtr) GenOpR* (ttt: D.UnaryOp; r: PhysReg; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenOpR;

PROCEDURE (self: SelfPtr) GenOpM* (ttt: D.UnaryOp; a-: AddrMode; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenOpM;

PROCEDURE (self: SelfPtr) GenSetC_R* (r: PhysReg; c: D.Condition);
END GenSetC_R;

PROCEDURE (self: SelfPtr) GenSetC_M* (a-: AddrMode; c: D.Condition);
END GenSetC_M;

PROCEDURE (self: SelfPtr) GenShiftR_R* (ttt: D.ShiftOp; r: PhysReg;
                                        sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenShiftR_R;

PROCEDURE (self: SelfPtr) GenShiftR_INum* (ttt: D.ShiftOp; r: PhysReg;
                                           v: LONGINT; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenShiftR_INum;

PROCEDURE (self: SelfPtr) GenShiftM_INum* (ttt: D.ShiftOp; a-: AddrMode;
                                           v: LONGINT; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenShiftM_INum;

PROCEDURE (self: SelfPtr) GenCBW*;
END GenCBW;

PROCEDURE (self: SelfPtr) GenCDQ* (szfrom: SizeType);
BEGIN
    ASSERT(szfrom<=4);
END GenCDQ;

PROCEDURE (self: SelfPtr) GenRepMovSD*;
END GenRepMovSD;

PROCEDURE (self: SelfPtr) GenMovSD*;
END GenMovSD;

PROCEDURE (self: SelfPtr) GenMovSW*;
END GenMovSW;

PROCEDURE (self: SelfPtr) GenRepMovSB*;
END GenRepMovSB;

PROCEDURE (self: SelfPtr) GenMovSB*;
END GenMovSB;

PROCEDURE (self: SelfPtr) GenSahf*;
END GenSahf;

PROCEDURE (self: SelfPtr) GenJ* (j: D.Condition; o: LONGINT; long: BOOLEAN);
END GenJ;

PROCEDURE (self: SelfPtr) GenBinJ* (j: D.Condition; o: LONGINT; long: BOOLEAN);
    -- direct output of a jump instruction
END GenBinJ;

PROCEDURE (self: SelfPtr) GenTxtJ* (j: D.Condition; lb: LABEL);
    -- direct output of a jump instruction
END GenTxtJ;

PROCEDURE (self: SelfPtr) GenCall_M* (a-: AddrMode;
                                      uses, modifies: D.PhysRegSet;
                                      r, w: BitVector);
END GenCall_M;

PROCEDURE (self: SelfPtr) GenCall_R* (r: PhysReg;
                                      uses, modifies: D.PhysRegSet;
                                      rd, w: BitVector);
END GenCall_R;

PROCEDURE (self: SelfPtr) GenCall_INum* (v: LONGINT;
                                         uses, modifies: D.PhysRegSet;
                                         r, w: BitVector);
END GenCall_INum;

PROCEDURE (self: SelfPtr) GenCall_Iglobal* (a-: AddrMode;
                                            uses, modifies: D.PhysRegSet;
                                            r, w: BitVector);
END GenCall_Iglobal;

PROCEDURE (self: SelfPtr) GenRet* (n: INT);
END GenRet;

PROCEDURE (self: SelfPtr) GenCase* (r: PhysReg; v: LONGINT; lb: LABEL);
END GenCase;

PROCEDURE (self: SelfPtr) GenMoveR_Table* (r, i: PhysReg; t: pc.OBJECT;
                                           sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenMoveR_Table;

PROCEDURE (self: SelfPtr) GenOpR_Table* (ttt: D.BinaryOp; r, i: PhysReg;
                                         t: pc.OBJECT; sz: SizeType);
BEGIN
    ASSERT(sz<=4);
END GenOpR_Table;

PROCEDURE (self: SelfPtr) GenTestR_Table* (r, i: PhysReg; t: pc.OBJECT;
                                           sz: SizeType);
END GenTestR_Table;

PROCEDURE (self: SelfPtr) GenHalt*;
END GenHalt;

PROCEDURE (self: SelfPtr) GenRaise* (is_assert: BOOLEAN);
END GenRaise;

PROCEDURE (self: SelfPtr) GenSkipTrap* (j: D.Condition; intno: Trap; pos-: ir.TPOS);
END GenSkipTrap;


--------------------------------------------------------------------------------
--
--  Skip Trap...
--
--------------------------------------------------------------------------------


PROCEDURE (self: SelfPtr) InitGenSkipTrap*();
VAR
  i: Trap;
BEGIN
  FOR i:=MIN(Trap) TO MAX(Trap) DO
    TrapUsages[i].Used := FALSE;
  END;
  TrapFixupList := NIL;

END InitGenSkipTrap;



PROCEDURE (self: SelfPtr) EndGenSkipTrap*();
END EndGenSkipTrap;


PROCEDURE (self: SelfPtr) CalcSkipTrapJumps*(i: Color.GenOrdNode): BOOLEAN;
BEGIN
  RETURN FALSE;
END CalcSkipTrapJumps;


PROCEDURE (self: SelfPtr) GenSkipTrapJumps*();
END GenSkipTrapJumps;




-- insert first
PROCEDURE AddLocalFixupList*(index: Trap; n: Node; offset: LONGINT; j: D.Condition);
VAR
  tmp: POINTER TO LinkedList;
BEGIN

  TrapUsages[index].Used := TRUE;

  NEW(tmp);

  tmp^.node := n;
  tmp^.offset := offset;
  tmp^.trap := index;
  tmp^.cond := j;
  tmp^.next := TrapFixupList;
  tmp^.l := FALSE;
  TrapFixupList := tmp;

END AddLocalFixupList;





--------------------------------------------------------------------------------
--
--  Вещественная арифметика
--
--------------------------------------------------------------------------------

PROCEDURE (self: SelfPtr) GenMoveTOS_M* (a-: AddrMode; sz: SizeType);
END GenMoveTOS_M;

PROCEDURE (self: SelfPtr) GenMoveTOS_INum* (w: VALUE; sz: SizeType);
END GenMoveTOS_INum;

PROCEDURE (self: SelfPtr) GenFILD* (a-: AddrMode; sz: SizeType);
END GenFILD;

PROCEDURE (self: SelfPtr) GenMoveM_TOS* (a-: AddrMode; sz: SizeType);
END GenMoveM_TOS;

PROCEDURE (self: SelfPtr) GenMoveM_ST0* (a-: AddrMode; sz: SizeType);
END GenMoveM_ST0;

PROCEDURE (self: SelfPtr) GenMoveSTi_TOS* (i: LONGINT);
END GenMoveSTi_TOS;

PROCEDURE (self: SelfPtr) GenMoveTOS_STi* (i: LONGINT);
END GenMoveTOS_STi;

PROCEDURE (self: SelfPtr) GenMoveSTi_ST0* (i: LONGINT);
END GenMoveSTi_ST0;

PROCEDURE (self: SelfPtr) GenFOpSTi_TOS* (op: D.FloatOp; r: SHORTINT);
END GenFOpSTi_TOS;

PROCEDURE (self: SelfPtr) GenFOpSTi_ST0* (op: D.FloatOp; r: SHORTINT);
END GenFOpSTi_ST0;

PROCEDURE (self: SelfPtr) GenFOpST0_STi* (op: D.FloatOp; r: SHORTINT);
END GenFOpST0_STi;

PROCEDURE (self: SelfPtr) GenFOpST0_M* (op: D.FloatOp; a-: AddrMode; sz: SizeType);
END GenFOpST0_M;

PROCEDURE (self: SelfPtr) GenFOpST0_IM* (op: D.FloatOp; a-: AddrMode; sz: SizeType);
END GenFOpST0_IM;

PROCEDURE (self: SelfPtr) GenFOpST0_INum* (op: D.FloatOp; w: VALUE; sz: SizeType);
END GenFOpST0_INum;

PROCEDURE (self: SelfPtr) GenFOp* (code: D.FloatOp);
END GenFOp;

PROCEDURE (self: SelfPtr) GenFComTOS_TOS*;
END GenFComTOS_TOS;

PROCEDURE (self: SelfPtr) GenFComTOS_INum* (w: VALUE; sz: SizeType);
END GenFComTOS_INum;

PROCEDURE (self: SelfPtr) GenFComTOS_M* (a-: AddrMode; sz: SizeType);
END GenFComTOS_M;

PROCEDURE (self: SelfPtr) GenFComTOS_STi* (r: SHORTINT);
END GenFComTOS_STi;

PROCEDURE (self: SelfPtr) GenFComTOS_IM* (a-: AddrMode; sz: SizeType);
END GenFComTOS_IM;

PROCEDURE (self: SelfPtr) GenFstsw*;
END GenFstsw;

PROCEDURE (self: SelfPtr) GenCallToOrdinal* (card: BOOLEAN;
                                             u, m: D.PhysRegSet; f: BOOLEAN; sz: SizeType);
END GenCallToOrdinal;

<* POP *>
--------------------------------------------------------------------------------

PROCEDURE (self: SelfPtr) InitEmitter*;
BEGIN
    CASE at.TARGET OF
    | at.trg_DOS4G:
        DSneqSS := FALSE;
        Traps [IndexTrap]    := - ORD(opStd.X2C_TRAP_INDEX);
        Traps [RangeTrap]    := - ORD(opStd.X2C_TRAP_RANGE);
        Traps [NilTrap]      := - ORD(opStd.X2C_TRAP_NIL);
        GuardPage := FALSE;
    | at.trg_FLASHTEK:
        DSneqSS   := TRUE;                -- Symantec/FlashTek settings
        GuardPage := TRUE;
        Traps [DivideTrap]   := 0;
        Traps [OverflowTrap] := 4;
        Traps [IndexTrap]    := 5;
        Traps [RangeTrap]    := 0FBH;
        Traps [NilTrap]      := 0FCH;
        --Traps [NilTrap]      := MAX (LONGINT);
   ELSE
        GuardPage := TRUE;
        DSneqSS := FALSE;
        Traps [IndexTrap]    := - ORD(opStd.X2C_TRAP_INDEX);
        Traps [NilTrap]      := - ORD(opStd.X2C_TRAP_NIL);
        Traps [RangeTrap]    := - ORD(opStd.X2C_TRAP_RANGE);
        Traps [DivideTrap]   := - ORD(opStd.X2C_TRAP_DIV);
        Traps [OverflowTrap] := - ORD(opStd.X2C_TRAP_OVERFL);
    END;
END InitEmitter;

--------------------------------------------------------------------------------
--
--  Нормальный процедурный интерфейс с верхним уровнем
--
--------------------------------------------------------------------------------

PROCEDURE TTT (op: ir.Operation): D.BinaryOp;
BEGIN
    CASE op OF
    | ir.o_add:     RETURN TTT_add;
    | ir.o_or:      RETURN TTT_or;
    | ir.o_and:     RETURN TTT_and;
    | ir.o_sub:     RETURN TTT_sub;
    | ir.o_xor:     RETURN TTT_xor;
    | ir.o_mul:     RETURN TTT_mul;
    END;
END TTT;

PROCEDURE TTT_hi (op: ir.Operation): D.BinaryOp;
BEGIN
    CASE op OF
    | ir.o_add:     RETURN TTT_adc;
    | ir.o_or:      RETURN TTT_or;
    | ir.o_and:     RETURN TTT_and;
    | ir.o_sub:     RETURN TTT_sbb;
    | ir.o_xor:     RETURN TTT_xor;
    END;
END TTT_hi;

PROCEDURE TTT_shift (op: ir.Operation): D.ShiftOp;
BEGIN
    CASE op OF
    | ir.o_shl: RETURN TTT_shl;
    | ir.o_shr: RETURN TTT_shr;
    | ir.o_sar: RETURN TTT_sar;
    | ir.o_rol: RETURN TTT_rol;
    | ir.o_ror: RETURN TTT_ror;
    END;
END TTT_shift;

PROCEDURE DivAdjust (sz: SizeType);
BEGIN
    CASE sz OF
    | 1:    work.GenShiftR_INum (TTT_sar, D.AHp, 7, 1);
            work.GenOpR_R       (TTT_add, D.ALp, D.AHp, 1);
    | 2:    work.GenShiftR_INum (TTT_sar, D.DXp, 15, 2);
            work.GenOpR_R       (TTT_add, D.EAXp, D.EDXp, 4);
    | 4:    work.GenShiftR_INum (TTT_sar, D.EDXp, 31, 4);
            work.GenOpR_R       (TTT_add, D.EAXp, D.EDXp, 4);
    | 8:    -- !!!!!!!!!!!!!!!!!!!!!1111
    END;
END DivAdjust;

--------------------------------------------------------------------------------

PROCEDURE AddPosition* (pos-: ir.TPOS);
BEGIN
    work.AddPosition (pos);
END AddPosition;

PROCEDURE AddTriadePosition* (p: TriadePtr): BOOLEAN;
VAR i: INT; val: VALUE;
BEGIN
    IF NOT p^.Position.IsNull () THEN
        work.AddPosition (p^.Position);
        RETURN TRUE;
    ELSIF (p^.Op <> ir.o_fi) & (p^.Params <> NIL) THEN
        FOR i:=0 TO LEN (p^.Params^)-1 DO
            IF (p^.Params^[i].tag IN { ir.y_NumConst, ir.y_RealConst }) THEN
                val := p^.Params^[i].value;
                IF NOT val.pos.IsNull () THEN
                    work.AddPosition (val.pos);
                    RETURN TRUE;
                END;
            END;
        END;
    END;
    RETURN FALSE;
END AddTriadePosition;

PROCEDURE TryAddPosition* (p: TriadePtr);
BEGIN
    IF AddTriadePosition (p) THEN END;
END TryAddPosition;

PROCEDURE GenMoveM_INum* (a: AddrMode; v: VALUE; sz: SizeType);
BEGIN
    work.GenMoveM_INum (a, v.get_NDWord(0), D.TruncSz4[sz]);
    IF sz = 8 THEN
      INC(a.offs,4);
      work.GenMoveM_INum (a, v.get_NDWord(1), 4);
      DEC(a.offs,4);
    END;
END GenMoveM_INum;

PROCEDURE GenFMoveM_INum* (a: AddrMode; w: VALUE; sz: SizeType);
VAR v: FLOAT;
    x : REAL;
    y: LONGREAL;
    k: LONGINT;
BEGIN
    v := Calc.ToReal (w, sz);
    CASE sz OF
    | 4:    x := VAL (REAL, v);
            UncheckedMove (k, x, 0, 4);
            work.GenMoveM_INum (a, k, 4);
--            work.GenPush_INum (k, 4);
    | 8:    y := VAL (LONGREAL, v);
            UncheckedMove (k, y, 4, 4);
            INC(a.offs,4);
            work.GenMoveM_INum (a, k, 4);
--            work.GenPush_INum (k, 4);
            UncheckedMove (k, y, 0, 4);
            DEC(a.offs,4);
            work.GenMoveM_INum (a, k, 4);
--            work.GenPush_INum (k, 4);
    END;
(*
    work.GenMoveM_INum (a, GetVal(v,), D.TruncSz4[sz]);
    IF sz = 8 THEN
      INC(a.offs,4);
      work.GenMoveM_INum (a, v.get_NDWord(1), 4);
      DEC(a.offs,4);
    END;
*)
END GenFMoveM_INum;

PROCEDURE GenMoveR_R* (d, s: Reg);
VAR sz : SizeType;
BEGIN
    ASSERT (d <> UNDEF_REG);
    ASSERT (s <> UNDEF_REG);
    ASSERT (D.RegInfo[d].sz = D.RegInfo[s].sz);
    IF d = s THEN RETURN END;
    IF D.RegInfo[d].sz < 8 THEN
      IF RegSet{d,s}*D.AllowedLowRegsWEBP[1] # RegSet{} THEN
        sz := D.Conv12_4[D.RegInfo[d].sz];
      ELSE
        sz := D.Conv2_4[D.RegInfo[d].sz];
      END;
      work.GenMoveR_R (D.RegInfo[d].code, D.RegInfo[s].code, sz(* must be D.Conv2_4[D.RegInfo[d].sz]*));
    ELSE
      IF D.RegInfo[d].codehi # D.RegInfo[s].code THEN
        IF D.RegInfo[d].codehi # D.RegInfo[s].codehi THEN
          work.GenMoveR_R (D.RegInfo[d].codehi, D.RegInfo[s].codehi, 4);
        END;
        IF D.RegInfo[d].code # D.RegInfo[s].code THEN
          work.GenMoveR_R (D.RegInfo[d].code,   D.RegInfo[s].code,   4);
        END;
      ELSE
        IF D.RegInfo[d].code # D.RegInfo[s].code THEN
          work.GenMoveR_R (D.RegInfo[d].code,   D.RegInfo[s].code,   4);
        END;
        IF D.RegInfo[d].codehi # D.RegInfo[s].codehi THEN
          work.GenMoveR_R (D.RegInfo[d].codehi, D.RegInfo[s].codehi, 4);
        END;
      END;
    END;
END GenMoveR_R;

PROCEDURE GenXchgR_R* (d, s: Reg);
BEGIN
    ASSERT (d <> UNDEF_REG);
    ASSERT (s <> UNDEF_REG);
    ASSERT (D.RegInfo[d].sz = D.RegInfo[d].sz);
    IF D.RegInfo[d].sz < 8 THEN
      work.GenXchgR_R (D.RegInfo[d].code, D.RegInfo[s].code, D.RegInfo[d].sz);
    ELSE
      work.GenXchgR_R (D.RegInfo[d].codehi, D.RegInfo[s].codehi, 4);
      work.GenXchgR_R (D.RegInfo[d].code,   D.RegInfo[s].code, 4);
    END;
END GenXchgR_R;

PROCEDURE GenXchgR_M* (r: Reg; a: AddrMode);
BEGIN
    ASSERT (r <> UNDEF_REG);
    IF D.RegInfo[r].sz < 8 THEN
      work.GenXchgR_M (D.RegInfo[r].code, a, D.RegInfo[r].sz);
    ELSE
      INC(a.offs, 4);
      work.GenXchgR_M (D.RegInfo[r].codehi, a, 4);
      DEC(a.offs, 4);
      work.GenXchgR_M (D.RegInfo[r].code,   a, 4);
    END;
END GenXchgR_M;

PROCEDURE GenAndR_INum* (r: Reg; v: VALUE);
BEGIN
    ASSERT (r <> UNDEF_REG);
    ASSERT (D.RegInfo[r].sz < 8);
    work.GenOpR_INum (TTT_and, D.RegInfo[r].code, GetIntVal(v,0,D.RegInfo[r].sz), D.RegInfo[r].sz);
END GenAndR_INum;

PROCEDURE GenXorR_INum* (r: Reg; v: VALUE);
BEGIN
    ASSERT (r <> UNDEF_REG);
    ASSERT (D.RegInfo[r].sz < 8);
    work.GenOpR_INum (TTT_xor, D.RegInfo[r].code, GetIntVal(v,0,D.RegInfo[r].sz), D.RegInfo[r].sz);
END GenXorR_INum;


PROCEDURE GenBTR_INum* (r: Reg; v: VALUE);
BEGIN
    ASSERT (r <> UNDEF_REG);
    ASSERT (D.RegInfo[r].sz < 8);
    work.GenOpR_INum (TTT_bt, D.RegInfo[r].code, GetIntVal(v,0,D.RegInfo[r].sz), D.RegInfo[r].sz);
END GenBTR_INum;

PROCEDURE GenBTINum_R* (v: VALUE; sz: SizeType; r:Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    ASSERT (D.RegInfo[r].sz < 8);
    work.GenOpINum_R (TTT_bt, v, D.RegInfo[r].code, D.RegInfo[r].sz);
END GenBTINum_R;

PROCEDURE GenBTR_R* (r1,r2: Reg);
BEGIN
    ASSERT (r1 <> UNDEF_REG);
    ASSERT (r2 <> UNDEF_REG);
    ASSERT (D.RegInfo[r1].sz < 8);
    ASSERT (D.RegInfo[r2].sz < 8);
    work.GenOpR_R (TTT_bt, D.RegInfo[r1].code, D.RegInfo[r2].code, D.RegInfo[r1].sz);
END GenBTR_R;

PROCEDURE GenBTM_R* (a-: AddrMode; r: Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    ASSERT (D.RegInfo[r].sz < 8);
    work.GenOpM_R (TTT_bt, a, D.RegInfo[r].code, D.RegInfo[r].sz);
END GenBTM_R;

PROCEDURE GenAddR_R* (d, s: Reg);
BEGIN
    ASSERT (d <> UNDEF_REG);
    ASSERT (s <> UNDEF_REG);
    ASSERT (D.RegInfo[d].sz < 8);
    ASSERT (D.RegInfo[s].sz < 8);
    work.GenOpR_R (TTT_add, D.RegInfo[d].code, D.RegInfo[s].code, D.RegInfo[d].sz);
END GenAddR_R;

PROCEDURE GenAddR_INum* (r: Reg; v: VALUE);
BEGIN
    ASSERT (r <> UNDEF_REG);
    ASSERT (D.RegInfo[r].sz < 8);
    work.GenOpR_INum (TTT_add, D.RegInfo[r].code, GetIntVal(v,0,D.RegInfo[r].sz), D.RegInfo[r].sz);
END GenAddR_INum;

PROCEDURE GenSubR_R* (d, s: Reg);
BEGIN
    ASSERT (d <> UNDEF_REG);
    ASSERT (s <> UNDEF_REG);
    ASSERT (D.RegInfo[d].sz < 8);
    ASSERT (D.RegInfo[s].sz < 8);
    work.GenOpR_R (TTT_sub, D.RegInfo[d].code, D.RegInfo[s].code, D.RegInfo[d].sz);
END GenSubR_R;

PROCEDURE GenSubR_INum* (r: Reg; v: VALUE);
BEGIN
    ASSERT (r <> UNDEF_REG);
    ASSERT (D.RegInfo[r].sz < 8);
    work.GenOpR_INum (TTT_sub, D.RegInfo[r].code, GetIntVal(v,0,D.RegInfo[r].sz), D.RegInfo[r].sz);
END GenSubR_INum;

PROCEDURE GenSubR_M* (r: Reg; a-: AddrMode);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenOpR_M (TTT_sub, D.RegInfo[r].code, a, D.RegInfo[r].sz);
END GenSubR_M;

PROCEDURE GenOrR_R* (d, s: Reg);
BEGIN
    ASSERT (d <> UNDEF_REG);
    ASSERT (s <> UNDEF_REG);
    work.GenOpR_R (TTT_or, D.RegInfo[d].code, D.RegInfo[s].code, D.RegInfo[d].sz);
END GenOrR_R;

PROCEDURE GenIncR* (r: Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    IF (D.RegInfo[r].sz = 2) OR (D.RegInfo[r].sz = 1) (*must be (D.RegInfo[r].sz = 2) *)
    THEN
        r := D.ChSize( r, 4 );
    END;
    work.GenOpR (TTT_inc, D.RegInfo[r].code, D.RegInfo[r].sz);
END GenIncR;

PROCEDURE GenDecR* (r: Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    IF (D.RegInfo[r].sz = 2) THEN
        r := D.ChSize( r, 4 );
    END;
    work.GenOpR (TTT_dec, D.RegInfo[r].code, D.RegInfo[r].sz);
END GenDecR;

PROCEDURE GenPop_R* (r: Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenPop_R (D.RegInfo[r].code);
    IF r IN D.REGS8 THEN
      work.GenPop_R (D.RegInfo[r].codehi);
    END;
END GenPop_R;

PROCEDURE ^ GenMoveR_M* (r: Reg; a: AddrMode);

PROCEDURE LoadLongToReg8( r:Reg; a: AddrMode );
BEGIN
  IF (a.place1.r # D.HighPart[r]) AND (a.place2.r # D.HighPart[r]) THEN
    INC(a.offs, 4);
    GenMoveR_M (D.HighPart[r], a);
    DEC(a.offs, 4);
    GenMoveR_M (D.LowPart[r], a);
  ELSE
    IF (a.place1.r # D.LowPart[r]) AND (a.place2.r # D.LowPart[r]) THEN
      GenMoveR_M (D.LowPart[r], a);
      INC(a.offs, 4);
      GenMoveR_M (D.HighPart[r], a);
      DEC(a.offs, 4);
    ELSE
      work.GenPush_M (a,4);
      INC(a.offs, 4);
      GenMoveR_M (D.HighPart[r], a);
      DEC(a.offs, 4);
      GenPop_R(D.LowPart[r]);
    END;
  END;
END LoadLongToReg8;

PROCEDURE GenMoveR_M* (r: Reg; a: AddrMode);
BEGIN
    ASSERT (r <> UNDEF_REG);
    IF D.RegInfo[r].sz = 8 THEN
      LoadLongToReg8(r,a);
      RETURN
    END;
    IF (D.RegInfo[r].sz = 2) -- OR (D.RegInfo[r].sz = 1) (*must be (D.RegInfo[r].sz = 2) *)
       & ((a.place1.r = UNDEF_REG) & (a.place2.r = UNDEF_REG)
       OR (a.place1.r = baseReg) OR (a.place2.r = baseReg))
    THEN
        r := D.ChSize( r, 4 );
    END;
    work.GenMoveR_M (D.RegInfo[r].code, a, D.TruncSz4[D.RegInfo[r].sz]);
(*
      work.GenMoveR_M (D.RegInfo[r].code, a, 4);
      INC(a.offs, 4);
      work.GenMoveR_M (D.RegInfo[r].codehi, a, 4);
      DEC(a.offs, 4);
*)
END GenMoveR_M;

PROCEDURE GenMoveM_R* (a: AddrMode; r: Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenMoveM_R (a, D.RegInfo[r].code, D.TruncSz4[D.RegInfo[r].sz]);
    IF r IN D.REGS8 THEN
      INC(a.offs,4);
      work.GenMoveM_R (a, D.RegInfo[r].codehi, 4);
      DEC(a.offs,4);
    END;
END GenMoveM_R;

PROCEDURE GenMoveR_INum* (r: Reg; v: VALUE);
BEGIN
    IF work = empty THEN RETURN; END;
    ASSERT (r <> UNDEF_REG);
    IF Calc.CompareValues(pc.sb_equ, v, Calc.GetInteger(0,D.RegInfo[r].sz),
                           ir.t_int, D.RegInfo[r].sz, TRUE)
    THEN
        IF D.RegInfo[r].sz = 8 THEN
          work.GenOpR_R (TTT_xor, D.RegInfo[r].codehi, D.RegInfo[r].codehi, 4);
        END;
        work.GenOpR_R (TTT_xor, D.RegInfo[r].code, D.RegInfo[r].code, D.TruncSz4[D.RegInfo[r].sz]);
    ELSE
        IF D.RegInfo[r].sz = 8 THEN
          work.GenMoveR_INum (D.RegInfo[r].codehi, v.get_NDWord(1), 4);
          work.GenMoveR_INum (D.RegInfo[r].code, v.get_NDWord(0), 4);
        ELSE
        work.GenMoveR_INum (D.RegInfo[r].code, GetIntVal(v,0,D.RegInfo[r].sz), D.TruncSz4[D.RegInfo[r].sz]);
        END;
    END;
END GenMoveR_INum;

PROCEDURE GenMoveR_Iglobal* (r: Reg; a-: AddrMode);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenMoveR_Iglobal (D.RegInfo[r].code, a, D.RegInfo[r].sz);
END GenMoveR_Iglobal;

PROCEDURE GenLEA* (r: Reg; a-: AddrMode);
VAR oldr: Reg;
BEGIN
    ASSERT (r <> UNDEF_REG);
    oldr := r;
    IF (D.RegInfo[r].sz = 2 ) THEN
        r := D.ChSize( r, 4 );
    END;
    IF (a.local = ir.UNDEFINED) & (a.proc = ir.ProcUNDEFINED) & (a.scale = D.x1) THEN
        IF (a.offs = 0) THEN
            IF a.place1.r = oldr THEN
                IF a.place2.r <> UNDEF_REG THEN
                    GenAddR_R (r, a.place2.r);
                END;
                RETURN;
            ELSIF a.place2.r = oldr THEN
                IF a.place1.r <> UNDEF_REG THEN
                    GenAddR_R (r, a.place1.r);
                END;
                RETURN;
            ELSIF a.place2.r = UNDEF_REG THEN
                IF a.place1.r = UNDEF_REG THEN
                    GenMoveR_INum (r, Calc.GetInteger(0, D.RegInfo[r].sz));
                ELSE
                    GenMoveR_R (r, D.ChSize(a.place1.r,D.RegInfo[r].sz));
                END;
                RETURN;
            END;
        ELSIF (a.place1.r = oldr) & (a.place2.r = UNDEF_REG) OR
              (a.place2.r = oldr) & (a.place1.r = UNDEF_REG)
        THEN
            IF a.offs = 1 THEN
                GenIncR (D.ChSize(r,4));
            ELSIF a.offs = -1 THEN
                GenDecR (D.ChSize(r,4));
            ELSE
                GenAddR_INum (r, Calc.GetInteger(a.offs, D.RegInfo[r].sz));
            END;
            RETURN;
        END;
    END;
    work.GenLEA (D.RegInfo[r].code, a);
END GenLEA;

PROCEDURE SX8_4 (d: Reg);
BEGIN
    IF d = D.EDX_EAX THEN
      work.GenCDQ(4);
    ELSE
      work.GenMoveR_R (D.RegInfo[d].codehi, D.RegInfo[d].code,
                       4);
      work.GenShiftR_INum (TTT_sar, D.RegInfo[d].codehi, 31, 4);
    END;
END SX8_4;

PROCEDURE GenMovesxR_M* (r: Reg; a: AddrMode; srcsz: SizeType);
BEGIN
    ASSERT (r <> UNDEF_REG);
    IF D.RegInfo[r].sz <= 4 THEN
        IF D.RegInfo[r].sz = srcsz THEN
            work.GenMoveR_M (D.RegInfo[r].code, a, srcsz);
        ELSE
            work.GenMovesxR_M (D.RegInfo[r].code, a, D.RegInfo[r].sz, srcsz);
        END;
    ELSE
        CASE srcsz OF
        |8:
            INC(a.offs,4);
            work.GenMoveR_M (D.RegInfo[r].codehi, a, 4);
            DEC(a.offs,4);
            work.GenMoveR_M (D.RegInfo[r].code, a, 4);
        |4:
            work.GenMoveR_M (D.RegInfo[r].code, a, srcsz);
            SX8_4(r);
        |2,1:
            work.GenMovesxR_M (D.RegInfo[r].code, a, 4, srcsz);
            SX8_4(r);
        END;
    END;
END GenMovesxR_M;

PROCEDURE GenMovesxR_R* (d, s: Reg);
BEGIN
    ASSERT (d <> UNDEF_REG);
    ASSERT (s <> UNDEF_REG);
    IF NOT (at.SPACE IN at.COMP_MODE) & (at.CPU = at.iPentium) &
       (d IN D.OwnersOf[s])&(D.RegInfo[d].sz<=4)
    THEN
        work.GenShiftR_INum (TTT_shl, D.RegInfo[d].code, 32 - D.RegInfo[s].sz * 8, 4);
        work.GenShiftR_INum (TTT_sar, D.RegInfo[d].code, 32 - D.RegInfo[s].sz * 8, 4);
    ELSE
        IF D.RegInfo[d].sz = D.RegInfo[s].sz THEN
            work.GenMoveR_R (D.RegInfo[d].code, D.RegInfo[s].code, D.RegInfo[d].sz);
        ELSIF D.RegInfo[d].sz = 8 THEN
            ASSERT(D.RegInfo[s].sz <8);
            IF ~(d IN D.OwnersOf[s]) THEN
              IF D.RegInfo[s].sz = 4 THEN
                work.GenMoveR_R (D.RegInfo[d].code, D.RegInfo[s].code,
                                 4);
              ELSE
                work.GenMovesxR_R (D.RegInfo[d].code, D.RegInfo[s].code,
                                   4,   D.RegInfo[s].sz);
              END;
            ELSIF D.RegInfo[s].sz < 4 THEN
              ASSERT(D.RegInfo[d].code = D.RegInfo[s].code);
              work.GenMovesxR_R (D.RegInfo[d].code, D.RegInfo[d].code,
                                 4,   D.RegInfo[s].sz);
            END;
            SX8_4(d);
        ELSE
            work.GenMovesxR_R (D.RegInfo[d].code, D.RegInfo[s].code,
                               D.RegInfo[d].sz,   D.RegInfo[s].sz);
        END;
    END;
END GenMovesxR_R;

PROCEDURE GenMovezxR_M* (r: Reg; a-: AddrMode; srcsz: SizeType);
BEGIN
    ASSERT (r <> UNDEF_REG);
    IF (D.RegInfo[r].sz = 2) THEN
        r := D.ChSize( r, 4 );
    END;
    IF NOT (at.SPACE IN at.COMP_MODE) &
           (at.CPU <> at.i386) & (at.CPU <> at.iPentiumPro)
    THEN
        IF (srcsz = 1) & (r < D.EBP) THEN
            IF D.RegInfo[r].sz = 8 THEN
              work.GenOpR_R   (TTT_xor, D.RegInfo[r].codehi, D.RegInfo[r].codehi, 4);
            END;
            IF (a.place1.r <> r) & (a.place2.r <> r) THEN
                work.GenOpR_R   (TTT_xor, D.RegInfo[r].code, D.RegInfo[r].code, 4);
                work.GenMoveR_M (D.RegInfo[r].code, a, 1);
            ELSE
                work.GenMoveR_M  (D.RegInfo[r].code, a, 1);
                work.GenOpR_INum (TTT_and, D.RegInfo[r].code, 255, 4);
            END;
            RETURN;
        ELSIF (a.place1.r = baseReg) OR (a.place2.r = baseReg) OR
              (a.place1.r = UNDEF_REG) & (a.place2.r = UNDEF_REG)
        THEN
            work.GenMoveR_M (D.RegInfo[r].code, a, 4);
            IF D.RegInfo[r].sz = 8 THEN
              work.GenOpR_R   (TTT_xor, D.RegInfo[r].codehi, D.RegInfo[r].codehi, 4);
            ELSE
              CASE srcsz OF
              | 1:    work.GenOpR_INum (TTT_and, D.RegInfo[r].code,   255, 4);
              | 2:    work.GenOpR_INum (TTT_and, D.RegInfo[r].code, 65535, 4);
              END;
            END;
            RETURN;
        END;
    END;
    IF srcsz = D.RegInfo[r].sz THEN
        work.GenMoveR_M (D.RegInfo[r].code, a, 4);
    ELSE
        IF D.RegInfo[r].sz = 8 THEN
          work.GenOpR_R   (TTT_xor, D.RegInfo[r].codehi, D.RegInfo[r].codehi, 4);
          IF srcsz = 4 THEN
            work.GenMoveR_M (D.RegInfo[r].code, a, 4);
          ELSE
            work.GenMovezxR_M (D.RegInfo[r].code, a, 4, srcsz);
          END;
        ELSE
          work.GenMovezxR_M (D.RegInfo[r].code, a, D.RegInfo[r].sz, srcsz);
        END;
    END;
END GenMovezxR_M;

PROCEDURE GenMovezxR_R* (d, s: Reg);
BEGIN
    ASSERT (d <> UNDEF_REG);
    ASSERT (s <> UNDEF_REG);
    ASSERT (D.RegInfo[d].sz > D.RegInfo[s].sz);
    IF (D.RegInfo[d].sz = 2) THEN
        d := D.ChSize( d, 4 );
    END;
    IF (d IN D.OwnersOf[s]) THEN
        CASE D.RegInfo[s].sz OF
        | 1:    work.GenOpR_INum (TTT_and, D.RegInfo[d].code,   255, 4);
        | 2:    work.GenOpR_INum (TTT_and, D.RegInfo[d].code, 65535, 4);
        | 4:    work.GenOpR_R   (TTT_xor, D.RegInfo[d].codehi, D.RegInfo[d].codehi, 4);
        END;
    ELSIF (D.RegInfo[s].sz = 1)&(D.RegInfo[d].code <= D.EBXp)&
          NOT (at.SPACE IN at.COMP_MODE) &
              (at.CPU <> at.i386) & (at.CPU <> at.iPentiumPro)
    THEN
        work.GenOpR_R   (TTT_xor, D.RegInfo[d].code, D.RegInfo[d].code, 4);
        work.GenMoveR_R (D.RegInfo[d].code, D.RegInfo[s].code, 1);
    ELSE
        IF D.RegInfo[d].sz = 8 THEN
          work.GenOpR_R   (TTT_xor, D.RegInfo[d].codehi, D.RegInfo[d].codehi, 4);
          IF D.RegInfo[s].sz = 4 THEN
            work.GenMoveR_R (D.RegInfo[d].code, D.RegInfo[s].code, 4);
          ELSE
            work.GenMovezxR_R (D.RegInfo[d].code, D.RegInfo[s].code,
                               4              ,   D.RegInfo[s].sz);
          END;
        ELSE
          work.GenMovezxR_R (D.RegInfo[d].code, D.RegInfo[s].code,
                             D.RegInfo[d].sz,   D.RegInfo[s].sz);
        END;
    END;
END GenMovezxR_R;

PROCEDURE GenSetC_R* (r: Reg; c: D.Condition);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenSetC_R (D.RegInfo[r].code, c);
END GenSetC_R;

PROCEDURE GenCheckO* (p: TriadePtr);
BEGIN
    IF ir.o_Checked IN p^.Options THEN
        IF p^.ResType = ir.t_int THEN
            work.GenSkipTrap (D.JNO, IntOTrap, p^.Position);
        ELSE
            work.GenSkipTrap (D.JNC, OverflowTrap, p^.Position);
        END;
    END;
END GenCheckO;

PROCEDURE GenInto* (pos-: ir.TPOS);
BEGIN
    work.GenSkipTrap (D.JNO, IntOTrap, pos);
END GenInto;

PROCEDURE GenOpR_INum* (p: TriadePtr; r: Reg; v: VALUE);
VAR sz: SizeType;
BEGIN
    sz := p^.ResSize;
    ASSERT (r <> UNDEF_REG);
    IF (sz = 2) & NOT (ir.o_Checked IN p^.Options) THEN
        sz := 4;
    END;
    IF ~((TTT (p^.Op) = TTT_or)&(v.get_NDWord(0)=0))
       &~((TTT (p^.Op)=TTT_and)&(v.get_NDWord(0)=MAX(SYSTEM.CARD32)))
    THEN
      work.GenOpR_INum (TTT (p^.Op), D.RegInfo[r].code, v.get_NDWord(0), D.TruncSz4[sz]);
    END;
    IF sz = 8 THEN
      IF ~((TTT_hi (p^.Op) = TTT_or)&(v.get_NDWord(1)=0))
         &~((TTT_hi (p^.Op) = TTT_and)&(v.get_NDWord(1)=MAX(SYSTEM.CARD32)))
      THEN
        work.GenOpR_INum (TTT_hi (p^.Op), D.RegInfo[r].codehi, v.get_NDWord(1), 4);
      END;
    END;
    GenCheckO (p);
END GenOpR_INum;

PROCEDURE GenOpR_Iglobal* (p: TriadePtr; r: Reg; a-: AddrMode);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenOpR_Iglobal (TTT (p^.Op), D.RegInfo[r].code, a, p^.ResSize);
    GenCheckO (p);
END GenOpR_Iglobal;

PROCEDURE GenOpR_R* (p: TriadePtr; d, s: Reg);
VAR sz: SizeType;
BEGIN
    sz := p^.ResSize;
    ASSERT (d <> UNDEF_REG);
    ASSERT (s <> UNDEF_REG);
    IF (sz = 2) & NOT (ir.o_Checked IN p^.Options) THEN
        sz := 4;
    END;
    IF sz < 8 THEN
      work.GenOpR_R (TTT (p^.Op), D.RegInfo[d].code, D.RegInfo[s].code, sz);
    ELSE
      work.GenOpR_R (TTT (p^.Op), D.RegInfo[d].code, D.RegInfo[s].code, 4);
      work.GenOpR_R (TTT_hi (p^.Op), D.RegInfo[d].codehi, D.RegInfo[s].codehi, 4);
    END;
    GenCheckO (p);
END GenOpR_R;

PROCEDURE GenOpR_M* (p: TriadePtr; r: Reg; a: AddrMode);
VAR sz: SizeType;
BEGIN
    sz := p^.ResSize;
    ASSERT (r <> UNDEF_REG);
    IF (sz = 2) & NOT (ir.o_Checked IN p^.Options) &
       ((a.place1.r = UNDEF_REG) & (a.place2.r = UNDEF_REG) OR
        (a.place1.r = baseReg) OR (a.place2.r = baseReg))
    THEN
        sz := 4;
    END;
    work.GenOpR_M (TTT (p^.Op), D.RegInfo[r].code, a, D.TruncSz4[sz]);
    IF sz = 8 THEN
      INC(a.offs,4);
      work.GenOpR_M (TTT_hi (p^.Op), D.RegInfo[r].codehi, a, 4);
      DEC(a.offs,4);
    END;
    GenCheckO (p);
END GenOpR_M;

PROCEDURE GenOpM_INum* (p: TriadePtr; a: AddrMode; v: VALUE);
BEGIN
    IF ~((TTT (p^.Op) = TTT_or)&(v.get_NDWord(0)=0))
       &~((TTT (p^.Op) = TTT_and)&(v.get_NDWord(0)=MAX(SYSTEM.CARD32)))
    THEN
      work.GenOpM_INum (TTT (p^.Op), a, v.get_NDWord(0), D.TruncSz4[p^.ResSize]);
    END;
    IF p^.ResSize = 8 THEN
      INC(a.offs,4);
      IF ~((TTT_hi (p^.Op) = TTT_or)&(v.get_NDWord(1)=0))
         &~((TTT_hi (p^.Op) = TTT_and)&(v.get_NDWord(1)=MAX(SYSTEM.CARD32)))
      THEN
        work.GenOpM_INum (TTT_hi (p^.Op), a, v.get_NDWord(1), 4);
      END;
      DEC(a.offs,4);
    END;

    GenCheckO (p);
END GenOpM_INum;

PROCEDURE GenOpM_Iglobal* (p: TriadePtr; a-, ag-: AddrMode);
BEGIN
    work.GenOpM_Iglobal (TTT (p^.Op), a, ag, p^.ResSize);
    GenCheckO (p);
END GenOpM_Iglobal;

PROCEDURE GenOpM_R* (p: TriadePtr; a: AddrMode; r: Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    IF D.RegInfo[r].sz < 8 THEN
      work.GenOpM_R (TTT (p^.Op), a, D.RegInfo[r].code, p^.ResSize);
    ELSE
      work.GenOpM_R (TTT (p^.Op), a, D.RegInfo[r].code, 4);
      INC(a.offs, 4);
      work.GenOpM_R (TTT_hi (p^.Op), a, D.RegInfo[r].codehi, 4);
      DEC(a.offs, 4);
    END;
    GenCheckO (p);
END GenOpM_R;

PROCEDURE GenDiv_R* (t: TriadePtr; r: Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenDiv_R (D.RegInfo[r].code, t^.ResSize, t^.ResType = ir.t_int);
    IF t^.ResType = ir.t_int THEN
        CASE t^.Op OF
        | ir.o_div:
            DivAdjust (t^.ResSize);
        | ir.o_mod:
            CASE t^.ResSize OF
            | 1:    work.GenMoveR_R     (D.ALp, D.AHp, 1);
                    work.GenShiftR_INum (TTT_sar, D.ALp, 7, 1);
                    work.GenOpR_R       (TTT_and, D.ALp, D.RegInfo[r].code, 1);
                    work.GenOpR_R       (TTT_add, D.AHp, D.ALp, 1);
            | 2:    work.GenMoveR_R     (D.EAXp, D.EDXp, 4);
                    work.GenShiftR_INum (TTT_sar, D.AXp, 15, 2);
                    work.GenOpR_R       (TTT_and, D.EAXp, D.RegInfo[r].code, 4);
                    work.GenOpR_R       (TTT_add, D.EDXp, D.EAXp, 4);
            | 4:    work.GenMoveR_R     (D.EAXp, D.EDXp, 4);
                    work.GenShiftR_INum (TTT_sar, D.EAXp, 31, 4);
                    work.GenOpR_R       (TTT_and, D.EAXp, D.RegInfo[r].code, 4);
                    work.GenOpR_R       (TTT_add, D.EDXp, D.EAXp, 4);
            | 8:    --- !!!!!!!!!!!!!!!!!11
            END;
        | ELSE
        END;
    END;
END GenDiv_R;

PROCEDURE GenDiv_M* (t: TriadePtr; a-: AddrMode);
BEGIN
    work.GenDiv_M (a, t^.ResSize, t^.ResType = ir.t_int);
    IF t^.ResType = ir.t_int THEN
        CASE t^.Op OF
        | ir.o_div:
            DivAdjust (t^.ResSize);
        | ir.o_mod:
            CASE t^.ResSize OF
            | 1:    work.GenMoveR_R     (D.ALp, D.AHp, 1);
                    work.GenShiftR_INum (TTT_sar, D.ALp, 7, 1);
                    work.GenOpR_M       (TTT_and, D.ALp, a, 1);
                    work.GenOpR_R       (TTT_add, D.AHp, D.ALp, 1);
            | 2:    work.GenMoveR_R     (D.EAXp, D.EDXp, 4);
                    work.GenShiftR_INum (TTT_sar, D.AXp, 15, 2);
                    work.GenOpR_M       (TTT_and, D.AXp, a, 2);
                    work.GenOpR_R       (TTT_add, D.EDXp, D.EAXp, 4);
            | 4:    work.GenMoveR_R     (D.EAXp, D.EDXp, 4);
                    work.GenShiftR_INum (TTT_sar, D.EAXp, 31, 4);
                    work.GenOpR_M       (TTT_and, D.EAXp, a, 4);
                    work.GenOpR_R       (TTT_add, D.EDXp, D.EAXp, 4);
            END;
        | ELSE
        END;
    END;
END GenDiv_M;

PROCEDURE GenDiv_INum* (t: TriadePtr; v: VALUE);
BEGIN
    work.GenDiv_INum (v.get_NDWord(0), t^.ResSize, t^.ResType = ir.t_int);
    IF t^.ResType = ir.t_int THEN
        CASE t^.Op OF
        | ir.o_div:
            DivAdjust (t^.ResSize);
        | ir.o_mod:
            CASE t^.ResSize OF
            | 1:    work.GenMoveR_R     (D.ALp, D.AHp, 1);
                    work.GenShiftR_INum (TTT_sar, D.ALp, 7, 1);
                    work.GenOpR_INum    (TTT_and, D.ALp, v.get_NDWord(0), 1);
                    work.GenOpR_R       (TTT_add, D.AHp, D.ALp, 1);
            | 2:    work.GenMoveR_R     (D.EAXp, D.EDXp, 4);
                    work.GenShiftR_INum (TTT_sar, D.AXp, 15, 2);
                    work.GenOpR_INum    (TTT_and, D.EAXp, v.get_NDWord(0), 4);
                    work.GenOpR_R       (TTT_add, D.EDXp, D.EAXp, 4);
            | 4:    work.GenMoveR_R     (D.EAXp, D.EDXp, 4);
                    work.GenShiftR_INum (TTT_sar, D.EAXp, 31, 4);
                    work.GenOpR_INum    (TTT_and, D.EAXp, v.get_NDWord(0), 4);
                    work.GenOpR_R       (TTT_add, D.EDXp, D.EAXp, 4);
            END;
        | ELSE
        END;
    END;
END GenDiv_INum;

PROCEDURE GenMul_R* (t: TriadePtr; r: Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenMul_R (D.RegInfo[r].code, t^.ResSize, t^.ResType = ir.t_int);
END GenMul_R;

PROCEDURE GenMul_M* (t: TriadePtr; a-: AddrMode);
BEGIN
    work.GenMul_M (a, t^.ResSize, t^.ResType = ir.t_int);
END GenMul_M;

PROCEDURE GenMul_INum* (t: TriadePtr; v: VALUE);
BEGIN
    work.GenMul_INum (v.get_NDWord(0), t^.ResSize, t^.ResType = ir.t_int);
END GenMul_INum;

PROCEDURE GenPush_M* (a: AddrMode; sz:SizeType);
BEGIN
  IF sz=2 THEN
    work.GenPush_M (a,2);
    INC (PushSize, 4);
  ELSE
    ASSERT(sz MOD 4 = 0);
    INC (a.offs, sz);
    REPEAT
        DEC (a.offs, 4);
        work.GenPush_M (a,4);
        INC (PushSize, 4);
        DEC (sz, 4);
    UNTIL sz = 0;
  END;
END GenPush_M;

PROCEDURE GenPush_INum* (v: VALUE; sz:SizeType);
BEGIN
    IF sz = 8 THEN
        work.GenPush_INum (v.get_NDWord(1), 4);
        INC (PushSize, 4);
        work.GenPush_INum (v.get_NDWord(0), 4);
        INC (PushSize, 4);
    ELSE
        work.GenPush_INum (v.get_NDWord(0), sz);
        INC (PushSize, 4);
    END;
END GenPush_INum;

PROCEDURE GenPush_FNum* (par: ParamPtr; sz:SizeType);
BEGIN
    work.GenPush_INum (GetVal (par, sz), sz);
    INC (PushSize, 4);
END GenPush_FNum;

PROCEDURE GenPush_R* (r: Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    IF D.RegInfo[r].sz = 8 THEN
      work.GenPush_R (D.RegInfo[r].codehi);
    END;
    work.GenPush_R (D.RegInfo[r].code);
END GenPush_R;

PROCEDURE GenIMulR_MC* (p: TriadePtr; r: Reg; a-: AddrMode; v: VALUE);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenIMulR_MC (D.RegInfo[r].code, a, v.get_NDWord(0), p^.ResSize);
    GenCheckO (p);
END GenIMulR_MC;

PROCEDURE GenIMulR_RC* (p: TriadePtr; d, s: Reg; v: LONGINT);
BEGIN
    ASSERT (d <> UNDEF_REG);
    ASSERT (s <> UNDEF_REG);

    work.GenIMulR_RC (D.RegInfo[d].code, D.RegInfo[s].code, v, p^.ResSize);
    GenCheckO (p);
END GenIMulR_RC;

PROCEDURE GenCmpR_INum* (r: Reg; v: VALUE);
BEGIN
    ASSERT (r <> UNDEF_REG);
    ASSERT(D.RegInfo[r].sz <= 4);
(*    IF D.RegInfo[r].sz = 8 THEN
      work.GenOpR_INum (TTT_cmp, D.RegInfo[r].codehi, v.get_NDWord(1), 4);
      work.AllocPlaceForShortJump;
    END;
*)
    work.GenOpR_INum (TTT_cmp, D.RegInfo[r].code, v.get_NDWord(0), D.TruncSz4[D.RegInfo[r].sz]);
(*
      work.DisableOptimizations;
      work.GenOpR_INum (TTT_cmp, D.RegInfo[r].codehi, v.get_NDWord(1), 4);
      work.GenJ (D.JNE, .6, FALSE);
      --GenSubR_R (r, r, 4);
      work.GenOpR_INum (TTT_cmp, D.RegInfo[r].code, v.get_NDWord(0), 4);
      work.EnableOptimizations;
    END;
*)
END GenCmpR_INum;

PROCEDURE GenUnR* (p: TriadePtr; r: Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    CASE p^.Op OF
    | ir.o_add:     ASSERT(D.RegInfo[r].sz#8);
                    work.GenOpR (TTT_inc, D.RegInfo[r].code, 4);
    | ir.o_sub:     ASSERT(D.RegInfo[r].sz#8);
                    work.GenOpR (TTT_dec, D.RegInfo[r].code, 4);
    | ir.o_neg:
                    IF p^.ResSize < 8 THEN
                      work.GenOpR (TTT_neg, D.RegInfo[r].code, p^.ResSize);
                    ELSE
                      work.GenOpR (TTT_neg, D.RegInfo[r].code, 4);
                      work.GenOpR_INum (TTT_adc, D.RegInfo[r].codehi, 0, 4);
                      work.GenOpR (TTT_neg, D.RegInfo[r].codehi, 4);
                    END;
                    GenCheckO (p);
    | ir.o_xor:     --ASSERT(D.RegInfo[r].sz#8);
                    IF p^.ResSize < 8 THEN
                      work.GenOpR (TTT_not, D.RegInfo[r].code, p^.ResSize);
                    ELSE
                      work.GenOpR (TTT_not, D.RegInfo[r].code, 4);
                      work.GenOpR (TTT_not, D.RegInfo[r].codehi, 4);
                    END;
    | ir.o_not:     ASSERT(D.RegInfo[r].sz#8);
                    work.GenOpR_INum (TTT_xor, D.RegInfo[r].code, 1, p^.ResSize);
    | ir.o_sgnext:  ASSERT(D.RegInfo[r].sz#8);
                    work.GenShiftR_INum (TTT_sar, D.RegInfo[r].code, 8 * p^.ResSize - 1,
                                         p^.ResSize);
    | ir.o_cap:     ASSERT(D.RegInfo[r].sz#8);
                    GenSubR_INum (D.ChSize(r,1), Calc.GetInteger(ORD ('a'), 1));
                    GenCmpR_INum (D.ChSize(r,1), Calc.GetInteger(ORD ('z') - ORD ('a'),1));
                    work.DisableOptimizations;
                    work.GenJ (D.JA, 2 + ORD (r <> D.AL), FALSE);
                    GenSubR_INum (D.ChSize(r,1), Calc.GetInteger(20H, 1));
                    work.EnableOptimizations;
                    GenAddR_INum (D.ChSize(r,1), Calc.GetInteger(ORD ('a'), 1));
    END;
END GenUnR;

PROCEDURE GenUnM* (p: TriadePtr; a: AddrMode);
BEGIN
    CASE p^.Op OF
    | ir.o_add:     work.GenOpM (TTT_inc, a, p^.ResSize);
    | ir.o_sub:     work.GenOpM (TTT_dec, a, p^.ResSize);
(*<An5*)
    | ir.o_neg:
                    IF p^.ResSize < 8 THEN
                      work.GenOpM (TTT_neg, a, p^.ResSize);
                    ELSE
                      work.GenOpM (TTT_neg, a, 4);
                      INC(a.offs,4);
                      work.GenOpM_INum (TTT_adc, a, 0, 4);
                      work.GenOpM (TTT_neg, a, 4);
                      DEC(a.offs,4);
                    END;
                    GenCheckO (p);
(*An5>*)
    | ir.o_xor:     work.GenOpM (TTT_not, a, p^.ResSize);
    | ir.o_not:     work.GenOpM_INum (TTT_xor, a, 1, p^.ResSize);
    END;
END GenUnM;

PROCEDURE GenCmpR_Iglobal* (r: Reg; a-: AddrMode);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenOpR_Iglobal (TTT_cmp, D.RegInfo[r].code, a, D.RegInfo[r].sz);
END GenCmpR_Iglobal;

PROCEDURE GenCmpR_R* (d, s: Reg);
BEGIN
    ASSERT (d <> UNDEF_REG);
    ASSERT (s <> UNDEF_REG);
    work.GenOpR_R (TTT_cmp, D.RegInfo[d].code, D.RegInfo[s].code, D.RegInfo[d].sz);
END GenCmpR_R;

PROCEDURE GenCmpR_M* (r: Reg; a-: AddrMode);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenOpR_M (TTT_cmp, D.RegInfo[r].code, a, D.RegInfo[r].sz);
END GenCmpR_M;

PROCEDURE GenCmpM_INum* (a: AddrMode; v: VALUE; sz: SizeType);
BEGIN
    ASSERT( sz <= 4 );
    work.GenOpM_INum (TTT_cmp, a, v.get_NDWord(0), sz);
(*
      work.DisableOptimizations;
      INC(a.offs,4);
      work.GenOpM_INum (TTT_cmp, a, v.get_NDWord(1), 4);
      work.GenJ (D.JNE, .6, FALSE);
      DEC(a.offs,4);
      work.GenOpM_INum (TTT_cmp, a, v.get_NDWord(0), 4);
      work.EnableOptimizations;
    END;
*)
END GenCmpM_INum;

PROCEDURE GenCmpM_Iglobal* (a-, ag-: AddrMode; sz: SizeType);
BEGIN
    work.GenOpM_Iglobal (TTT_cmp, a, ag, sz);
END GenCmpM_Iglobal;

PROCEDURE GenCmpM_R* (a-: AddrMode; r: Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenOpM_R (TTT_cmp, a, D.RegInfo[r].code, D.RegInfo[r].sz);
END GenCmpM_R;

PROCEDURE GenTestR_R* (r1, r2: Reg);
BEGIN
    ASSERT (r1 <> UNDEF_REG);
    ASSERT (r2 <> UNDEF_REG);
    work.GenTestR_R (D.RegInfo[r1].code, D.RegInfo[r2].code, D.RegInfo[r1].sz);
END GenTestR_R;

PROCEDURE GenTestR_INum* (r: Reg; v: VALUE);
BEGIN
    ASSERT (r <> UNDEF_REG);
    ASSERT (D.RegInfo[r].sz < 8 );
    work.GenTestR_INum (D.RegInfo[r].code, v.get_NDWord(0), D.RegInfo[r].sz);
END GenTestR_INum;

PROCEDURE GenShiftR_R* (p: TriadePtr; r: Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenShiftR_R (TTT_shift (p^.Op), D.RegInfo[r].code, D.RegInfo[r].sz);
END GenShiftR_R;

PROCEDURE GenShiftR_INum* (p: TriadePtr; r: Reg; v: VALUE);
BEGIN
    ASSERT (r <> UNDEF_REG);
    ASSERT( D.RegInfo[r].sz < 8 );
    work.GenShiftR_INum (TTT_shift (p^.Op), D.RegInfo[r].code, v.get_NDWord(0), D.RegInfo[r].sz);
END GenShiftR_INum;

PROCEDURE GenShiftM_INum* (p: TriadePtr; a-: AddrMode; v: VALUE; sz: SizeType);
BEGIN
    ASSERT(sz < 8);
    work.GenShiftM_INum (TTT_shift (p^.Op), a, v.get_NDWord(0), sz);
END GenShiftM_INum;

PROCEDURE GenCall_R* (r: Reg;
                      uses, modifies: RegSet;
                      rd, w: BitVector);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenCall_R (D.RegInfo[r].code, D.RegSet2PhysRegSet(uses), D.RegSet2PhysRegSet(modifies), rd, w);
END GenCall_R;

PROCEDURE GenShrR_INum* (r: Reg; v: VALUE);
BEGIN
    ASSERT(D.RegInfo[r].sz < 8 );
    work.GenShiftR_INum (TTT_shr, D.RegInfo[r].code, v.get_NDWord(0), D.RegInfo[r].sz);
END GenShrR_INum;

PROCEDURE GenCase* (r: Reg; v: LONGINT; lb: LABEL);
BEGIN
    ASSERT (r <> UNDEF_REG);
    work.GenCase (D.RegInfo[r].code, v, lb);
END GenCase;

PROCEDURE GenInclExclMem* (p: TriadePtr; a-: AddrMode; r: Reg);
VAR ttt: D.BinaryOp;
    sz:  SizeType;
BEGIN
    sz := p^.ResSize;
    ASSERT (r <> UNDEF_REG);
      IF p^.Op = ir.o_incl THEN
          ttt := TTT_bts;
      ELSE
          ttt := TTT_btr;
      END;
      work.GenOpM_R (ttt, a, D.RegInfo[r].code, D.RegInfo[r].sz);
END GenInclExclMem;

PROCEDURE GenMoveR_Table64 (dest, ind: Reg; obj, hiobj:pc.OBJECT);
BEGIN
  IF (ind IN D.RegAndIntersectWith[D.HighPart[dest]]) THEN
      work.GenMoveR_Table (D.RegInfo[dest].code,   D.RegInfo[ind].code, obj, 4);
      work.GenMoveR_Table (D.RegInfo[dest].codehi, D.RegInfo[ind].code, hiobj, 4);
  ELSE
      work.GenMoveR_Table (D.RegInfo[dest].codehi, D.RegInfo[ind].code, hiobj, 4);
      work.GenMoveR_Table (D.RegInfo[dest].code,   D.RegInfo[ind].code, obj, 4);
  END;
END GenMoveR_Table64;

PROCEDURE GenOpR_Table64 (ttt: D.BinaryOp; dest, ind: Reg; obj, hiobj:pc.OBJECT);
BEGIN
  IF (ind IN D.RegAndIntersectWith[D.HighPart[dest]]) THEN
      work.GenOpR_Table (ttt, D.RegInfo[dest].code, D.RegInfo[ind].code, obj, 4);
      work.GenOpR_Table (ttt, D.RegInfo[dest].codehi, D.RegInfo[ind].code, hiobj, 4);
  ELSE
      work.GenOpR_Table (ttt, D.RegInfo[dest].codehi, D.RegInfo[ind].code, hiobj, 4);
      work.GenOpR_Table (ttt, D.RegInfo[dest].code, D.RegInfo[ind].code, obj, 4);
  END;
END GenOpR_Table64;

PROCEDURE GenInclExcl* (p: TriadePtr; r, r1, r2: Reg);
VAR ttt: D.BinaryOp;
    sz:  SizeType;
    b,bhi:   pc.OBJECT;
BEGIN
    sz := p^.ResSize;
    ASSERT (r <> UNDEF_REG);
    ASSERT (r1 <> UNDEF_REG);

    IF p^.Op = ir.o_and THEN
        b := at.setop_table (ir.o_loset);
        ASSERT(sz#8);
    ELSE
        b := at.setop_table (p^.Op);
        bhi := at.setop_table_hi (p^.Op)
    END;
    IF p^.Op = ir.o_incl THEN
        ttt := TTT_or;
    ELSE
        ttt := TTT_and;
    END;
    IF sz = 2 THEN
        sz := 4;
    END;
    IF r = r2 THEN
--        ASSERT ((r2 <> UNDEF_REG) & ((r2 IN AllowedIRegs[sz]) OR (r2 = SPILLED_REG)));
--        it was been checked
        ASSERT(sz # 8);
        work.GenMoveR_Table (D.RegInfo[r2].code, D.RegInfo[r2].code, b, sz);
        work.GenOpR_R       (ttt, D.RegInfo[r2].code, D.RegInfo[r1].code, sz);
    ELSE
        IF r <> r1 THEN
            work.GenMoveR_R (D.RegInfo[r].code, D.RegInfo[r1].code, 4);
        END;
        -- here we assume that r2 register always be used with size = 4 in GenOrR_Table
        IF sz = 8 THEN
          GenOpR_Table64(ttt,r,r2,b,bhi);
        ELSE
          work.GenOpR_Table (ttt, D.RegInfo[r].code, D.RegInfo[r2].code, b, sz);
        END;
    END;
END GenInclExcl;

PROCEDURE GenMoveInclExcl* (p: TriadePtr; r, r2: Reg);
VAR sz: SizeType;
BEGIN
    sz := p^.ResSize;
    ASSERT (r <> UNDEF_REG);
    ASSERT (r2 <> UNDEF_REG);
    IF sz = 2 THEN
        sz := 4;
    END;
    IF sz = 8 THEN
      GenMoveR_Table64 (r, r2, at.setop_table (p^.Op), at.setop_table_hi (p^.Op));
    ELSE
      work.GenMoveR_Table (D.RegInfo[r].code, D.RegInfo[r2].code, at.setop_table (p^.Op), sz);
    END;
END GenMoveInclExcl;

CONST GenLoHiSet *= GenMoveInclExcl;

PROCEDURE GenInSet* (l, r: Reg);
BEGIN
    ASSERT (r <> UNDEF_REG);
    IF (D.RegInfo[l].sz = 2) THEN
        l := D.ChSize( l, 4 );
    END;
    work.GenTestR_Table (D.RegInfo[r].code, D.RegInfo[l].code, at.setop_table (ir.o_incl), D.Conv2_4[D.RegInfo[r].sz]);
END GenInSet;

--------------------------------------------------------------------------------
--
--  Вещественная арифметика
--
--------------------------------------------------------------------------------

PROCEDURE GenMoveTOS_INum* (w: VALUE; sz: SizeType);
VAR v: FLOAT;
BEGIN
    v := Calc.ToReal (w, sz);
    IF v = 0.0 THEN
        work.GenFOp (D.FLDZ);
    ELSIF v = 1.0 THEN
        work.GenFOp (D.FLD1);
    ELSIF (sz = 4) & (v = D.Pi4) OR (sz = 8) & (v = D.Pi8) THEN
        work.GenFOp (D.FLDPI);
    ELSE
        work.GenMoveTOS_INum (w, SmallestSize (Calc.ToReal (w, sz), sz));
    END;
END GenMoveTOS_INum;

PROCEDURE GenPushReal* (w: VALUE; sz: SizeType);
VAR k: LONGINT;
    v: FLOAT;
    x: REAL;
    y: LONGREAL;
<* IF ~__GEN_C__ THEN *>
    z: LONGLONGREAL;
<* END *>
BEGIN
    v := Calc.ToReal (w, sz);
    CASE sz OF
    | 4:    x := VAL (REAL, v);
            UncheckedMove (k, x, 0, 4);
            work.GenPush_INum (k, 4);
    | 8:    y := VAL (LONGREAL, v);
            UncheckedMove (k, y, 4, 4);
            work.GenPush_INum (k, 4);
            UncheckedMove (k, y, 0, 4);
            work.GenPush_INum (k, 4);
<* IF ~__GEN_C__ THEN *>
    | 10,
      12:   z := VAL (LONGLONGREAL, v);
            k := 0;
            UncheckedMove (k, z, 8, 2);
            work.GenPush_INum (k, 4);
            UncheckedMove (k, z, 4, 4);
            work.GenPush_INum (k, 4);
            UncheckedMove (k, z, 0, 4);
            work.GenPush_INum (k, 4);
<* END *>
    END;
END GenPushReal;

PROCEDURE GenMoveRR_Real* (r1, r2: Reg; w: VALUE; sz: SizeType);
VAR k: LONGINT;
    v: FLOAT;
    x: REAL;
BEGIN
    v := Calc.ToReal (w, sz);
    CASE sz OF
    | 4:    x := VAL (REAL, v);
            UncheckedMove (k, x, 0, 4);
            GenMoveR_INum (r1, Calc.GetInteger(k, 4));
    | 8:    UncheckedMove (k, v, 4, 4);
            GenMoveR_INum (r2, Calc.GetInteger(k, 4));
            UncheckedMove (k, v, 0, 4);
            GenMoveR_INum (r1, Calc.GetInteger(k, 4));
    END;
END GenMoveRR_Real;

PROCEDURE GenMoveM_FConst* (a: AddrMode; w: VALUE; r: Reg; sz: SizeType);
VAR k, l: LONGINT;
    v: FLOAT;
    x: REAL;
    y: LONGREAL;
<* IF ~__GEN_C__ THEN *>
    z: LONGLONGREAL;
<* END *>
BEGIN
    v := Calc.ToReal (w, sz);
    CASE sz OF
    | 4:    x := VAL (REAL, v);
            UncheckedMove (k, x, 0, 4);
            IF (r = UNDEF_REG) OR (a.local = ir.UNDEFINED) & (a.offs = 0) THEN
                work.GenMoveM_INum (a, k, 4);
            ELSE
                GenMoveR_INum   (r, Calc.GetInteger(k, 4));
                work.GenMoveM_R (a, D.RegInfo[r].code, 4);
            END;
    | 8:    y := VAL (LONGREAL, v);
            UncheckedMove (k, y, 0, 4);
            IF r = UNDEF_REG THEN
                work.GenMoveM_INum (a, k, 4);
                INC (a.offs, 4);
                UncheckedMove (k, y, 4, 4);
                work.GenMoveM_INum (a, k, 4);
            ELSE
                GenMoveR_INum   (r, Calc.GetInteger(k, 4));
                work.GenMoveM_R (a, D.RegInfo[r].code, 4);
                INC (a.offs, 4);
                UncheckedMove   (l, y, 4, 4);
                IF k <> l THEN
                    GenMoveR_INum (r, Calc.GetInteger(l, 4));
                END;
                work.GenMoveM_R (a, D.RegInfo[r].code, 4);
            END;
    | 10,
      12:
<* IF ~__GEN_C__ THEN *>
            z := VAL (LONGLONGREAL, v);
            UncheckedMove (k, z, 0, 4);
            IF r = UNDEF_REG THEN
                work.GenMoveM_INum (a, k, 4);
                INC (a.offs, 4);
                UncheckedMove (k, z, 4, 4);
                work.GenMoveM_INum (a, k, 4);
                INC (a.offs, 4);
                k := 0;
                UncheckedMove (k, z, 8, 2);
                work.GenMoveM_INum (a, k, 2);
            ELSE
                GenMoveR_INum   (r, Calc.GetInteger(k, 4));
                work.GenMoveM_R (a, D.RegInfo[r].code, 4);
                INC (a.offs, 4);
                UncheckedMove (l, z, 4, 4);
                IF k <> l THEN
                    GenMoveR_INum (r, Calc.GetInteger(l, 4));
                END;
                work.GenMoveM_R (a, D.RegInfo[r].code, 4);
                INC (a.offs, 4);
                k := 0;
                UncheckedMove (k, z, 8, 2);
                IF k <> l THEN
                    GenMoveR_INum (r, Calc.GetInteger(k, 4));
                END;
                work.GenMoveM_R (a, D.RegInfo[r].code, 2);
            END;
<* ELSE *>
        -- to avoid type insufficiency in msvc
            GenMoveTOS_INum (w, sz);
            work.GenMoveM_TOS (a, sz);
<* END *>
    END;
END GenMoveM_FConst;

PROCEDURE GenFAbs*;
BEGIN
    work.GenFOp (D.FABS);
END GenFAbs;

PROCEDURE GenFNeg*;
BEGIN
    work.GenFOp (D.FCHS);
END GenFNeg;

PROCEDURE GenFLn*;
BEGIN
    work.GenFOp (D.FLDLN2);
    work.GenFOp (D.FXCH);
    work.GenFOp (D.FYL2X);
END GenFLn;

PROCEDURE GenFLg*;
BEGIN
    work.GenFOp (D.FLDLG2);
    work.GenFOp (D.FXCH);
    work.GenFOp (D.FYL2X);
END GenFLg;

PROCEDURE GenFCos*;
BEGIN
    work.GenFOp (D.FCOS);
END GenFCos;

PROCEDURE GenFSin*;
BEGIN
    work.GenFOp (D.FSIN);
END GenFSin;

PROCEDURE GenFSqrt*;
BEGIN
    work.GenFOp (D.FSQRT);
END GenFSqrt;

PROCEDURE GenFTan*;
BEGIN
    work.GenFOp         (D.FPTAN);
    work.GenMoveSTi_TOS (0);
END GenFTan;

PROCEDURE GenFAtan*;
BEGIN
    work.GenFOp (D.FLD1);
    work.GenFOp (D.FPATAN);
END GenFAtan;

PROCEDURE GenFExp*;
BEGIN
    work.GenFOp         (D.FLD1);
    work.GenFOp         (D.FLDL2E);
    work.GenFOpST0_STi  (D.FMUL, 2);
    work.GenMoveSTi_ST0 (2);
    work.GenFOp         (D.FPREM);
    work.GenFOp         (D.F2XM1);
    work.GenFOpSTi_TOS  (D.FADD, 1);
    work.GenFOp         (D.FSCALE);
    work.GenMoveSTi_TOS (1);
END GenFExp;

PROCEDURE GenFOpFReg_TOS* (op: D.FloatOp; r: SHORTINT);
BEGIN
    work.GenFOpSTi_TOS (op, r);
END GenFOpFReg_TOS;

PROCEDURE GenFOpFReg0_Freg* (op: D.FloatOp; r: SHORTINT);
BEGIN
    work.GenFOpST0_STi (op, r);
END GenFOpFReg0_Freg;

PROCEDURE GenFOpST0_INum* (op: D.FloatOp; w: VALUE; sz: SizeType);
BEGIN
    -- sometimes sz is not real size(w) because it is adjusted in
    -- p386.GetSmallestSize()
    -- so the last parameter of CompareWithReal is false.
    IF (op = D.FMUL) & Calc.CompareWithReal (pc.sb_equ, w, 2.0, sz, FALSE) THEN
        work.GenFOpST0_STi (D.FADD, 0);
    ELSE
        work.GenFOpST0_INum (op, w, sz);
    END;
END GenFOpST0_INum;

PROCEDURE GenFstswSahf*;
BEGIN
    work.GenFstsw;
    work.GenSahf;
END GenFstswSahf;

PROCEDURE GenCallToOrdinal* (card: BOOLEAN; sz: SizeType);
BEGIN
    work.GenCallToOrdinal (card, D.PhysRegSet{}, D.RegSet2PhysRegSet(D.allowedIRegs[sz] - D.SavedByProc), FALSE, sz);
END GenCallToOrdinal;

PROCEDURE GenFXCH*;
BEGIN
    work.GenFOp (D.FXCH);
END GenFXCH;

PROCEDURE GenFPREM*;
BEGIN
    work.GenFOp (D.FPREM);
    work.GenMoveSTi_TOS (1);
END GenFPREM;

--------------------------------------------------------------------------------

BEGIN
    NEW (work);
    empty := work;
    full  := work;
--    D.AllIRegs  := RegSet{ D.AX..D.BX, D.BP..D.DI };
    D.Pi4       := RealMath.pi;
    D.Pi8       := LongMath.pi;

(* инициализация машино-зависимых переменных из opTune *)
    tune.PARAM_START := 4;    (* смещение для первого параметра - далее возрастает *)

END Emit.
