MODULE r386;

(*
  Семантическое наполнение селектора команд и распределения регистров
    (а заодно и описание всех типов данных -
     этот модуль импортируют все, кому не лень)

  Недоделки:
  - в выгрузке регистра хорошо бы научиться сбрасывать сам способ
    адресации (сначала в регистр, а потом в память)
  - всюду: прежде чем грузить переменную из памяти, хорошо бы ее поискать
    на регистрах
  - при наследовании регистра: если проходит сквозь вызов процедуры -
    не наследовать
*)

IMPORT ir,
       Calc,
       Color,
       at := opAttrs,
       pc := pcK,
       Emit,
       CodeDef,
       RD := RDefs,
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
IMPORT std := opStd;
IMPORT xProfRTS;
IMPORT bv := BitVect;

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

--------------------------------------------------------------------------------

(*
  Описание структуры DAG'а
*)

CONST
    UNDEF_REG    = RD.UNDEF_REG;
    MINREG       = D.MINREG;
    MAXREG       = D.MAXREG;

TYPE
    DAGNODE = RD.DAGNODE;

--------------------------------------------------------------------------------

VAR
    MustFillFrame*: BOOLEAN;
    UsedByCalled*:RegSet;   -- Какие регистры используются
                            -- вызываемыми процедурами
    NeverUsePushInCall* : BOOLEAN; -- inited in Iselect_I.generate

    SavedRegisters: RegSet; -- Saved at prologue/restored at epilogue registers
    NSavedRegs: INT;        -- Number of such regs

VAR
    TEMP_SIZE*:  INT;       -- размер временной области на стеке
    TEMP_START*: INT;       -- смещение начала этой области относительно ESP
                            --  в момент входа в процедуру
    FRAME_SIZE*: INT;       -- размер захваченной на стеке процедурой области
    WasAlloca*:  BOOLEAN;
    SavedESP*:   INT;       -- сюда спасается ESP если были alloca
    MAX_FLOAT*:  INT;       -- максимальная глубина использования стека
                            -- сопроцессора

    WasUnsignToReal*: BOOLEAN;  -- надо ли под областью TEMP завести еще 4 байта
(*
    MemOps*:          BOOLEAN;  -- Допустимы ли OP MEM, RC
*)

CONST 
    MemOps* = TRUE;             -- Допустимы ли OP MEM, RC

--------------------------------------------------------------------------------


--    NNonTempVars*:  INT;
VAR
    CODE_START*:     LONGINT;
    InitialFrameOffset*: LONGINT;
    ProcHasFrame*: BOOLEAN;

--------------------------------------------------------------------------------

CONST
  NOP = 90X;        -- NOP

--------------------------------------------------------------------------------

PROCEDURE ToInteger (v: VALUE; sz: SizeType): LONGINT;
BEGIN
    IF Calc.IsNegative (v, sz) THEN
        RETURN Calc.ToInteger (v, sz);
    ELSE
        RETURN SYSTEM.VAL (LONGINT, Calc.ToCardinal (v, sz));
    END;
END ToInteger;

--------------------------------------------------------------------------------

PROCEDURE TryAddPosition* (p: DAGNODE);
BEGIN
    IF p^.tr <> NIL THEN
        Emit.TryAddPosition (p^.tr);
    ELSE
        Emit.TryAddPosition (p^.par^.triade);
    END;
END TryAddPosition;

--------------------------------------------------------------------------------

PROCEDURE IncFloats*;
BEGIN
    INC (Emit.FloatSize);
    IF Emit.FloatSize > MAX_FLOAT THEN
        MAX_FLOAT := Emit.FloatSize;
    END;
END IncFloats;

--------------------------------------------------------------------------------
--
--      Пролог / эпилог / распределение памяти на стеке
--
--------------------------------------------------------------------------------
(*
  Сформировать смещение на стеке, если оно известно только
  относительно ESP в момент входа в процедуру
*)

PROCEDURE FormStackOffset (VAR a: AddrMode; offs: INT);
BEGIN
    Emit.InitAddrMode (a);
    IF Emit.baseReg = D.ESP THEN
        INC (offs, Emit.PushSize);
    END;
    a.offs  := offs + Emit.LocalsOffset;
    a.place1.r  := Emit.baseReg;
END FormStackOffset;

--------------------------------------------------------------------------------

(*
  Инициализировать доступные регистры и пр. перед началом работы
*)

PROCEDURE SetNeedFP* (need: BOOLEAN; alloca: BOOLEAN);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.SetNeedFP\n");
<* END *>
    need := need OR (at.use_frame_ptr IN at.COMP_MODE)
            OR (pc.ttag_except IN at.curr_proc.type.tags)
            OR (pc.ttag_throws IN at.curr_proc.type.tags);

    IF need THEN
        Emit.baseReg := D.EBP;
        D.allIRegs := D.AllIRegsWoEBP;
        D.allowedIRegs := D.AllowedIRegsWoEBP;
    ELSE
        Emit.baseReg := D.ESP;
        D.allIRegs := D.AllIRegsWEBP;
        D.allowedIRegs := D.AllowedIRegsWEBP;
    END;
    WasAlloca := alloca;
    MAX_FLOAT := 0;
    Emit.LocalsOffset := 0;
    WasUnsignToReal := FALSE;
    MustFillFrame := prc.MustFillFrame;
    CodeDef.ret_node := ir.UndefNode;
    CodeDef.ret_offs := 0;
(*
    MemOps   := (at.SPACE IN at.COMP_MODE) OR
                NOT (at.DOREORDER IN at.COMP_MODE) OR (at.CPU <= at.i486);
*)
END SetNeedFP;

--------------------------------------------------------------------------------

PROCEDURE CheckFloats*;
BEGIN
    IF MAX_FLOAT > 8 THEN
        xiEnv.errors.Fault (xiEnv.null_pos, 951);
        HALT;
    END;
END CheckFloats;

--------------------------------------------------------------------------------

PROCEDURE AddrModeByProc(VAR a: AddrMode; p: pc.OBJECT);
VAR attr: at.ATTR_EXT;
    inf: at.INFO_EXT;
BEGIN
  attr := at.attr(p.ext, at.a_self);
  inf := attr(at.INFO_EXT);
  ASSERT(inf.e_tag = ir.y_ProcConst);
  Emit.InitAddrMode(a);
  a.proc := VAL(prc.ProcNum, inf.name);
END AddrModeByProc;

PROCEDURE GenProfilerCall( N: ir.INT );
VAR a: AddrMode;
    o: pc.OBJECT;
BEGIN
  IF at.profilingMode = xProfRTS.PROF_MODE_NONE THEN RETURN; END;
  IF at.curr_mod = at.curr_proc THEN RETURN; END;

  Emit.InitAddrMode(a);
  a.local := CodeDef.prof_info_loc;
  a.offs := ORD(at.curr_user_procno) * CodeDef.prof_info_elem_len;
  Emit.work.GenPush_Iglobal(a);

  o := std.Proc(N);
  AddrModeByProc(a, o);
  Emit.work.GenCall_Iglobal (a, D.PhysRegSet{}, D.PhysRegSet{}, NIL, NIL);
END GenProfilerCall;

--------------------------------------------------------------------------------

PROCEDURE SavedReg(r: Reg; check_only_used: BOOLEAN): BOOLEAN;
BEGIN
  ASSERT((r >= D.MIN4REG) & (r <= D.MAX4REG) & (r # D.ESP));

  IF (r IN P.TrashedRegs(prc.ProcProtoNum(at.curr_procno))) THEN
    RETURN FALSE;
  END;

  RETURN ~check_only_used OR (r = Emit.baseReg) OR (r IN reg.everUsed);
END SavedReg;

-- Tries to find reg from 'src' or reg that can be trashed according to
-- procedure's calling convention. For use in prologue/epilogue
PROCEDURE GetRegForTrash(src: RegSet; useRetval: BOOLEAN; VAR r: Reg): BOOLEAN;
VAR pt: prc.ProtoNum;
    rs: RegSet;
    i: Reg;
BEGIN
  pt := prc.ProcProtoNum(at.curr_procno);
  rs := P.TrashedRegs(pt);
  IF ~useRetval THEN
    rs := rs - P.RetvalRegs(pt);
  END;

  FOR i := D.MIN4REG TO D.MAX4REG DO
    IF (i IN src) OR (i IN rs) THEN
      r := i;
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END GetRegForTrash;

--------------------------------------------------------------------------------

PROCEDURE MakeFrame;
VAR i: INT;
    c: LONGINT;
BEGIN
  IF (Emit.work # Emit.full) OR (FRAME_SIZE = 0) THEN RETURN END;

  IF FRAME_SIZE = 4 THEN
      IF MustFillFrame THEN
          Emit.work.GenPush_INum (0, 4);
      ELSE
          Emit.GenPush_R (D.EAX);
      END;
  ELSIF (FRAME_SIZE = 8) & MustFillFrame THEN
      Emit.work.GenPush_INum (0, 4);
      INC (Emit.PushSize, 4);
      Emit.work.GenPush_INum (0, 4);
  ELSE
      IF MustFillFrame THEN
          c := FRAME_SIZE;
          INCL (reg.everUsed, D.EAX);
          Emit.GenSubR_R (D.EAX, D.EAX);
          IF c > 32 THEN
              INCL (reg.everUsed, D.ECX);
              Emit.work.GenMoveR_INum (D.ECXp, c DIV 16, 4);
              FOR i:=1 TO 4 DO
                  Emit.GenPush_R (D.EAX);
              END;
              Emit.GenDecR (D.ECX);
              Emit.work.GenJ (D.JNE, -7, FALSE);
              c := c MOD 16;
              Emit.PushSize := - c;
          END;
          WHILE c <> 0 DO
              Emit.GenPush_R (D.EAX);
              INC (Emit.PushSize, 4);
              DEC (c, 4);
          END;
      ELSIF (FRAME_SIZE >= 4096) & Emit.GuardPage THEN
          c := FRAME_SIZE;
          WHILE c >= 4096 DO
              Emit.GenSubR_INum (D.ESP, Calc.GetInteger(4092,4));
              INC (Emit.PushSize, 4092);
              Emit.GenPush_R (D.EAX);
              INC (Emit.PushSize, 4);
              DEC (c, 4096);
          END;
          IF c <> 0 THEN
              Emit.GenAddR_INum (D.ESP, Calc.GetInteger(-c,4));
          END;
      ELSE
          Emit.GenAddR_INum (D.ESP, Calc.GetInteger(-FRAME_SIZE,4));
      END;
  END;
END MakeFrame;

PROCEDURE DestroyFrame;
VAR r: Reg;
BEGIN
  IF FRAME_SIZE + Emit.PushSize = 0 THEN RETURN END;

  IF (Emit.baseReg = D.EBP) & (SavedRegisters = RegSet{D.EBP})
  THEN -- no need to restore regs except EBP
      Emit.GenMoveR_R (D.ESP, D.EBP);
  ELSIF (FRAME_SIZE + Emit.PushSize = 4) AND
        GetRegForTrash(SavedRegisters, FALSE, r)
  THEN
      Emit.GenPop_R(r);
      reg.MarkUsedSoFar(r);
  ELSE
      Emit.GenSubR_INum (D.ESP, Calc.GetInteger(- (FRAME_SIZE + Emit.PushSize),4));
  END;
  Emit.PushSize := 0;
END DestroyFrame;

--------------------------------------------------------------------------------

(*
  Приписать переменным смещения на стеке; выдать общий их размер
*)

PROCEDURE AllocStackMemory (start: INT): INT;

VAR a: INT;
    vec : bv.BitVector;

    PROCEDURE TryAllocLocal(local: Local);
    BEGIN
        IF NOT ir.IsExternal (local) &
           ((local>=Color.NLocals) OR ~bv.In (vec, ORD(local)))
           (*& (ir.Locals^[local].Offset = ir.UNKNOWN_OFFSET)*) --comm by kevin
        THEN
            DEC (a, (ir.Locals^[local].VarSize + 3) DIV 4 * 4);
            ir.Locals^[local].Offset := a;
            bv.Incl (vec, ORD(local));
<* IF ~nodebug THEN *>
            opIO.print("Local ");
            ir.PrintLocal(local);
            opIO.print(" placed to offset %d\n",a);
<* END *>
        END;
    END TryAllocLocal;

    PROCEDURE Compounds;
    VAR l: Local;
    BEGIN
        FOR l:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NLocals) DO
            IF ir.Locals^[l].Addressed THEN
                TryAllocLocal(l);
            END;
        END;
    END Compounds;

    PROCEDURE AllocVarsInLocal;
    VAR i: VarNum;
        local: Local;
    BEGIN
        FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
            IF RD.Loc^[i].tag = NTlocal THEN
                local := RD.Loc^[i].mem;
                TryAllocLocal(local);
            END;
        END;
    END AllocVarsInLocal;

VAR     l:       Local;
BEGIN
    vec := bv.New (ORD(ir.NLocals)+1, FALSE);
    bv.Move (ir.AllocatedLocals, vec);
    a := start;
<* IF ~nodebug THEN *>
    opIO.print("===r386.AllocStackMemory\n");
    opIO.print("Start at %d\n", start);
<* END *>
    FOR l:=ir.ZEROVarNum TO SYSTEM.PRED(Color.NLocals) DO
        IF bv.In (vec, ORD(l)) &
           NOT ir.IsExternal (l) &
               (ir.Locals^[l].Offset < a) THEN
            a := ir.Locals^[l].Offset;
<* IF ~nodebug THEN *>
            opIO.print("Found local ");
            ir.PrintLocal(l);
            opIO.print(" at offset %d\n",a);
<* END *>
        END;
    END;
    a := - (((-a) + 3) DIV 4) * 4;
    Compounds;
    AllocVarsInLocal;
    DEC(a, TEMP_SIZE);
    IF WasUnsignToReal THEN
        DEC (a, 4);
<* IF ~nodebug THEN *>
        opIO.print("WasUnsignToReal\n");
<* END *>
    END;
    TEMP_START := a;
<* IF ~nodebug THEN *>
    opIO.print("TEMP_SIZE=%d TEMP_START=%d\n",TEMP_SIZE,a);
<* END *>
    IF WasAlloca THEN
        DEC (a, 4);
        SavedESP := a;
<* IF ~nodebug THEN *>
        opIO.print("WasAlloca\n");
<* END *>
    END;
<* IF ~nodebug THEN *>
    opIO.print("Final a=%d\n",a);
    opIO.print("Return %d\n",start - a);
<* END *>
    bv.Free(vec);
    RETURN start - a;
END AllocStackMemory;

--------------------------------------------------------------------------------

PROCEDURE Prologue* (save_all: BOOLEAN);
VAR a: AddrMode;
    p: ir.TPOS;
    savedSize: INT;

    PROCEDURE TrySave(r: Reg);
    BEGIN
        IF SavedReg(r, ~save_all) THEN
            Emit.GenPush_R(r);
            INCL(SavedRegisters, r);
            IF (r # D.EBP) & (Emit.baseReg # D.EBP) THEN
              INCL(reg.everUsed, r);
            END;
        END;
    END TrySave;

BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.Prologue\n");
<* END *>
    at.GetStartTPos (at.curr_proc, p);
    Emit.AddPosition (p);
    Emit.work.DisableMoveStores;
    Emit.work.DisableOptimizations;

    CASE at.profilingMode OF
    | xProfRTS.PROF_MODE_NONE:  -- nothing to do
    | xProfRTS.PROF_MODE_FULL:
      IF at.main & (at.curr_proc = at.curr_mod) THEN
        GenProfilerCall( std.X2C_PROFILE_PROC_START );
      ELSE
        GenProfilerCall( std.X2C_PROFILE_PROC_START_C );
      END;
    ELSE
      GenProfilerCall( std.X2C_PROFILE_PROC_START );
    END;

    IF (pc.ttag_except IN at.curr_proc.type.tags) THEN
      save_all := TRUE;
    END;

<* IF ~ NODEBUG THEN *>
    reg.PrintRegSet(reg.everUsed, "everUsed: ");
<* END *>
    NSavedRegs := ORD(SavedReg(D.EBP, ~save_all)) +
                  ORD(SavedReg(D.EBX, ~save_all)) +
                  ORD(SavedReg(D.ESI, ~save_all)) +
                  ORD(SavedReg(D.EDI, ~save_all)) +
                  ORD(SavedReg(D.EDX, ~save_all)) +
                  ORD(SavedReg(D.EAX, ~save_all)) +
                  ORD(SavedReg(D.ECX, ~save_all));

    savedSize := 4 * NSavedRegs;

    FRAME_SIZE := AllocStackMemory(-savedSize);
    ASSERT (FRAME_SIZE MOD 4 = 0);
    Emit.PushSize := - (savedSize + FRAME_SIZE);
    ProcHasFrame := Emit.baseReg = D.EBP;

    SavedRegisters := RegSet{};
    TrySave(D.EBP);
    IF ProcHasFrame THEN
        InitialFrameOffset := - Emit.PushSize + 4; -- offset of caller's IP (ret_adr)
        Emit.GenMoveR_R (D.EBP, D.ESP);
    ELSE
        InitialFrameOffset := - Emit.PushSize;     -- offset of caller's IP (ret_adr)
    END;
    TrySave(D.EBX);
    TrySave(D.ESI);
    TrySave(D.EDI);
    TrySave(D.EDX);
    TrySave(D.EAX);
    TrySave(D.ECX);

    MakeFrame;

    Emit.work.EnableOptimizations;
    Emit.work.EnableMoveStores;
    Emit.PushSize := 0;
    IF Emit.baseReg = D.EBP THEN
        Emit.LocalsOffset := 4;
    ELSE
        Emit.LocalsOffset := FRAME_SIZE + savedSize;
    END;
    IF WasAlloca THEN
        FormStackOffset (a, SavedESP);
        Emit.GenMoveM_R (a, D.ESP);
    END;
    IF WasUnsignToReal THEN
        FormStackOffset (a, TEMP_START + TEMP_SIZE);
        Emit.work.GenMoveM_INum (a, 0, 4);
    END;
    IF Emit.work = Emit.full THEN
        CODE_START := RD.NodeInfo^[ir.ZERONode].sg.code_len;
    END;
END Prologue;

--------------------------------------------------------------------------------

PROCEDURE ProcRetSize(proto: prc.ProtoNum; lang: pc.Lang; internal: BOOLEAN): LONGINT;

  PROCEDURE C_compatible(lang: pc.Lang): BOOLEAN;
  BEGIN
    RETURN (lang = pc.flag_c) OR
           (at.MC = at.CC) & (lang IN pc.LangSet{ prc.Oberon, prc.Modula })
  END C_compatible;

BEGIN
  IF (internal OR (lang IN pc.LangSet{ prc.Pascal, pc.flag_javacall,
                                       pc.flag_stdcall, pc.flag_vmcall, pc.flag_lightcall
                                     })) &
     NOT prc.SeqProto(proto) OR
     (at.MC = at.NATIVE) & (lang IN pc.LangSet{ prc.Oberon, prc.Modula })
  THEN
    RETURN prc.LenParams(proto);
  ELSIF ~internal & (at.CC = at.GCC) & C_compatible(lang) THEN
    ASSERT(at.TARGET = at.trg_LINUX);
    IF prc.ProtoList[proto].ext_rtn THEN
      ASSERT(prc.ProtoList[proto].npar > 0);
      ASSERT(prc.ProtoList[proto].par[0].mode = prc.pm_return);
      ASSERT(prc.ProtoList[proto].par[0].where = prc.STACK);
      RETURN prc.ProtoList[proto].par[0].size;
    ELSE
      RETURN 0;
    END;
  ELSE
    RETURN 0;
  END;
END ProcRetSize;

PROCEDURE Epilogue* (finalizer: BOOLEAN);
VAR a: AddrMode;
    retsize: LONGINT;
    restRegisters: RegSet;
    retvalRegs: RegSet;
    trash: Reg;

    PROCEDURE TryRest(r: Reg);
    BEGIN
        IF r IN SavedRegisters THEN
            Emit.GenPop_R(r);
            EXCL(restRegisters, r);
        END;
    END TryRest;

BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.Epilogue\n");
<* END *>
    GenProfilerCall( std.X2C_PROFILE_PROC_END );
    Emit.work.DisableOptimizations;

    IF finalizer THEN
      ASSERT(pc.ttag_except IN at.curr_proc.type.tags);
      -- set esp
      Emit.InitAddrMode(a);
      a.place1.r := D.EBP;
      a.offs := -4*(NSavedRegs-1);
      Emit.GenLEA (D.ESP, a);

      IF (at.curr_proc.attr # NIL) THEN
        -- push calleripref
        Emit.GenPush_R(D.EBP); -- push ebp; add dword [esp], 4
        Emit.InitAddrMode(a);
        a.place1.r := D.ESP;
        Emit.work.GenOpM_INum (D.TTT_add, a, 4, 4);
        -- call proc
        AddrModeByProc(a, at.curr_proc.attr(pc.OBJECT));
        Emit.work.GenCall_Iglobal (a, D.PhysRegSet{}, D.PhysRegSet{}, NIL, NIL);
        -- set esp
        Emit.InitAddrMode(a);
        a.place1.r := D.EBP;
        a.offs := -4*(NSavedRegs-1);
        Emit.GenLEA (D.ESP, a);
      ELSE -- generate 'return 0'
        retvalRegs := P.RetvalRegs(prc.ProcProtoNum(at.curr_procno));
        IF D.ST0 IN retvalRegs THEN
          Emit.work.GenFOp (D.FLDZ);
        ELSIF D.EAX IN retvalRegs THEN
          Emit.work.GenOpR_R(D.TTT_xor, D.EAXp, D.EAXp, 4);
          IF D.EDX IN retvalRegs THEN
            Emit.work.GenMoveR_R (D.EDXp, D.EAXp, 4);
          END;
        END;
      END;

    ELSE
      IF WasAlloca THEN
          FormStackOffset (a, SavedESP);
          Emit.GenMoveR_M (D.ESP, a);
          Emit.PushSize := 0;
      END;
      DestroyFrame;
    END;
    restRegisters := SavedRegisters;
    TryRest(D.ECX);
    TryRest(D.EAX);
    TryRest(D.EDX);
    TryRest(D.EDI);
    TryRest(D.ESI);
    TryRest(D.EBX);
    TryRest(D.EBP);
    ASSERT(restRegisters = RegSet{});

    retsize := ProcRetSize( prc.ProcProtoNum(at.curr_procno),
                            prc.ProcCallLang(at.curr_procno),
                            prc.IsInternal(at.curr_procno) );
    Emit.work.GenRet(retsize);
    Emit.work.EnableOptimizations;
END Epilogue;

--------------------------------------------------------------------------------
--
--      Собственно семантические действия
--
--------------------------------------------------------------------------------

(*
  Посчитать, сколько раз регистр фигурирует в узле
*)

PROCEDURE UsedInNode (p: DAGNODE; r: Reg; needIntersectors: BOOLEAN ): CARD;
VAR s: CARD;
BEGIN
    s := 0;
    CASE p^.nt OF
    | NTreg:       reg.CheckSpilledReg (p^.place);
                   IF ~needIntersectors THEN
                     IF p^.place.r = r THEN
                         s := 1;
                     END;
                   ELSE
                     IF p^.place.r IN D.RegAndIntersectWith4Max[r] THEN
                         s := 1;
                     END;
                   END;
    | NTmem,
      NTaddr,
      NTbased,
      NTscaled:    reg.CheckSpilledReg (p^.a.place1);
                   IF ~needIntersectors THEN
                     IF p^.a.place1.r = r THEN
                         s := 1;
                     END;
                   ELSE
                     IF p^.a.place1.r IN D.RegAndIntersectWith4Max[r] THEN
                         s := 1;
                     END;
                   END;
                    reg.CheckSpilledReg (p^.a.place2);
                   IF ~needIntersectors THEN
                     IF p^.a.place2.r = r THEN
                        s := s + 1;
                     END;
                   ELSE
                     IF p^.a.place2.r IN D.RegAndIntersectWith4Max[r] THEN
                        s := s + 1;
                     END;
                   END;
    | ELSE
    END;
    RETURN s;
END UsedInNode;

--------------------------------------------------------------------------------

(*
  По check'у выдать его int
*)

PROCEDURE TrapNo* (p: TriadePtr): Emit.Trap;
BEGIN
    IF ir.o_Division IN p^.Options THEN
        RETURN Emit.DivideTrap;
    ELSIF ir.o_Range IN p^.Options THEN
        RETURN Emit.RangeTrap;
    ELSIF p^.Op = ir.o_checknil THEN
        RETURN Emit.NilTrap;
    ELSE
        RETURN Emit.IndexTrap;
    END;
END TrapNo;

(*
  Выяснить размер операнда
*)

PROCEDURE Size* (p: DAGNODE): SHORTINT;
BEGIN
    IF p^.op = ir.o_par THEN
        RETURN ir.ParamSize (p^.par^.triade, p^.par^.paramnumber);
    ELSE
        RETURN p^.tr^.ResSize;
    END;
END Size;

--------------------------------------------------------------------------------

(*
  Сформировать способ адресации из var'а
*)

PROCEDURE FormMem* (VAR a: AddrMode; v: VarNum);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.FormMem\n");
<* END *>
    Emit.InitAddrMode (a);
    a.offs  := RD.Loc^[v].offs;
    a.local := RD.Loc^[v].mem;
    IF NOT ir.IsExternal (a.local) THEN
        a.place1.r := Emit.baseReg;
    END;
END FormMem;

--------------------------------------------------------------------------------

(*
  Выполнить отложенную загрузку переменной на регистр, если нужно
*)

PROCEDURE ToReg (VAR r: RegPlacement; ps: RegSet; isSizeOf4: BOOLEAN(* must be always FALSE*));
VAR g: Reg;
    oldDirtyRegs: RegSet;
    a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.ToReg\n");
<* END *>
  IF r.r # UNDEF_REG THEN
    ASSERT((r.r IN D.SPILLED_REGS) OR (r.r IN ps));
    RETURN;
  END;
  ASSERT (r.v <> ir.UNDEFINED);
  g := reg.FindInR (r.v, ps);
--  oldDirtyRegs := reg.dirtyRegs;
  IF g = UNDEF_REG THEN
      g := reg.GTR (ps);
  END;
  IF ~(g IN D.SPILLED_REGS) & (*~(g IN oldDirtyRegs) &*) (reg.regContents [g] <> r.v) THEN
      FormMem (a, r.v);
      IF isSizeOf4 THEN
        Emit.GenMoveR_M (D.ChSize(g, 4), a);
      ELSE
        Emit.GenMoveR_M (g, a);
      END;
      reg.MarkVarInReg( r.v, g );
  END;
  reg.OccupyReg(g);
  reg.usagesLeft [g] := reg.usagesLeft [g] + 1;
  r.r := g;
END ToReg;

--------------------------------------------------------------------------------

(*
  Выполнить отложенную загрузку переменных для
  способа адресации на регистр, если нужно
*)

PROCEDURE GetAddrMode (VAR a: AddrMode; sz: SizeType);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.GetAddrMode\n");
<* END *>
    IF a.place1.v <> ir.UNDEFINED THEN
        ToReg (a.place1, D.allowedIRegs[sz] + D.AllowedLowRegsWEBP[sz]
               - D.RegSet{a.place2.r} + D.RegSet{a.place1.r}, TRUE);
    END;
    IF a.place2.v <> ir.UNDEFINED THEN
        ToReg (a.place2, D.allowedIRegs[sz] + D.AllowedLowRegsWEBP[sz]
               - D.RegSet{a.place1.r} + D.RegSet{a.place2.r}, TRUE);
    END;
END GetAddrMode;

--------------------------------------------------------------------------------

(*
  Выполнить отложенную загрузку переменных для способа адресации на регистр,
  если нужно; при этом можно использовать регистр r, если он занят, но еще
  не использован
*)

PROCEDURE GetAddrModeUsingR (r: Reg; VAR a: AddrMode; sz: SizeType);
BEGIN
    IF (reg.usagesLeft [r] = 0) & (~(r IN D.REGS8) OR ((reg.usagesLeft [D.HighPart[r]] = 0)&(reg.usagesLeft [D.LowPart[r]] = 0))) THEN
        reg.FreeReg(r);
    END;
    GetAddrMode (a, sz);
END GetAddrModeUsingR;

--------------------------------------------------------------------------------

(*
  mem -> reg
*)

PROCEDURE MemToReg* (p: DAGNODE; sz: SizeType);
VAR v: VarNum;
    r: Reg;
    q: TriadePtr;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.MemToReg\n");
<* END *>
    IF (p^.op = ir.o_par) &
       (p^.par^.tag = ir.y_Variable) &
       (RD.Loc^[p^.par^.name].tag = NTlocal)
    THEN
        p^.place.r := UNDEF_REG;
        p^.place.v := p^.par^.name;
    ELSE
        GetAddrMode  (p^.a, opTune.addr_sz);
        reg.MarkUseAddrMode (p^.a);
        IF (p^.op = ir.o_par) THEN
            IF p^.par^.tag = ir.y_Variable THEN
               q := p^.par^.triade;
               IF (q^.Tag = ir.y_Variable) &
                  ((RD.Loc^[q^.Name].tag = NTaddr) OR
                   (RD.Loc^[q^.Name].tag = NTbased) OR
                   (RD.Loc^[q^.Name].tag = NTscaled) OR
                   (RD.Loc^[q^.Name].tag = NTmem))
                THEN
                    v := p^.par^.name;
                    r := reg.GR (v, D.allIRegs, TRUE, RD.Loc[v].temp );
                ELSE
                    v := ir.UNDEFINED;
                    r := reg.GTR (D.allowedIRegs [sz]);
                END;
            ELSE
                v := ir.UNDEFINED;
                r := reg.GTR (D.allowedIRegs [sz]);
            END;
        ELSE
            v := p^.tr^.Name;
            r := reg.gr (v, D.allIRegs);
        END;
        Emit.GenMoveR_M (r, p^.a);
        reg.VarInReg (v, r);
        p^.place.v := v;
        p^.place.r := r;
    END;
    p^.nt := NTreg;
END MemToReg;

--------------------------------------------------------------------------------

(*
  reg -> based
*)

PROCEDURE RegToBased* (p: DAGNODE);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.RegToBased\n");
<* END *>
    p^.a.offs  := 0;
    p^.a.local := ir.UNDEFINED;
    p^.a.place1    := p.place;
    IF (p^.a.place1.v <> ir.UNDEFINED) & (RD.Loc^[p^.a.place1.v].tag = NTlocal) THEN
        p^.a.place1.r := UNDEF_REG;
        reg.MarkUseReg (p.place);
    END;
    p^.a.place2.v  := ir.UNDEFINED;
    p^.a.place2.r  := UNDEF_REG;
    p^.a.scale := D.x1;
    p^.a.proc  := ir.ProcUNDEFINED;
    p^.a.offslist     := NIL;
    p^.a.proclist := NIL;
    p^.a.bv    := NIL;
    p^.nt     := NTbased;
END RegToBased;

--------------------------------------------------------------------------------

(*
  par -> based
*)

PROCEDURE StackAddrToBased* (VAR a: AddrMode; p: ParamPtr);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.StackAddrToBased\n");
<* END *>
    Emit.InitAddrMode (a);
    a.offs  := p^.offset;
    a.local := p^.name;
    a.place1.r  := Emit.baseReg;
END StackAddrToBased;

--------------------------------------------------------------------------------

(*
  addr + par -> addr
*)

PROCEDURE AddAddrParToAddr* (p, p1, p2: DAGNODE);
VAR op: OffsRecPtr;
    pp: ProcRecPtr;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.AddAddrParToAddr\n");
<* END *>
    p^.a := p1^.a;
    CASE p2^.par^.tag OF
    | ir.y_NumConst:
        INC (p^.a.offs, ToInteger (p2^.par^.value, p^.tr^.ResSize));
    | ir.y_AddrConst:
        IF p^.a.local = ir.UNDEFINED THEN
            p^.a.local := p2^.par^.name;
        ELSE
            NEW (op);
            op^.name := p2^.par^.name;
            op^.next := p^.a.offslist;
            p^.a.offslist   := op;
        END;
        INC (p^.a.offs, p2^.par^.offset);
    | ir.y_ProcConst:
        IF p^.a.proc = ir.ProcUNDEFINED THEN
            p^.a.proc := VAL(prc.ProcNum, p2^.par^.name);
        ELSE
            NEW (pp);
            pp^.name := VAL(prc.ProcNum, p2^.par^.name);
            pp^.next := p^.a.proclist;
            p^.a.proclist   := pp;
        END;
    | ir.y_RealConst:
        xiEnv.errors.Fault (p^.tr^.Position, 952);
        HALT;
    END;
END AddAddrParToAddr;

--------------------------------------------------------------------------------

(*
  addr - par -> addr
*)

PROCEDURE SubAddrParToAddr* (p, p1, p2: DAGNODE);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.SubAddrParToAddr\n");
<* END *>
    p^.a := p1^.a;
    CASE p2^.par^.tag OF
    | ir.y_NumConst:
        DEC (p^.a.offs, ToInteger (p2^.par^.value, p^.tr^.ResSize));
    | ir.y_RealConst:
        xiEnv.errors.Fault (p^.tr^.Position, 952);
        HALT;
    END;
END SubAddrParToAddr;

--------------------------------------------------------------------------------

(*
  reg * const -> scaled
*)

PROCEDURE MultToScaled* (p, p1, p2: DAGNODE);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.MultToScaled\n");
<* END *>
    p^.a.offs  := 0;
    p^.a.place1.v  := ir.UNDEFINED;
    p^.a.place1.r  := UNDEF_REG;
    p^.a.place2    := p1.place;
    IF (p^.a.place2.v <> ir.UNDEFINED) & (RD.Loc^[p^.a.place2.v].tag = NTlocal) THEN
        p^.a.place2.r := UNDEF_REG;
        reg.MarkUseReg (p1.place);
    END;
    CASE Calc.ToInteger (p2^.par^.value, p2^.sz)  OF
    | 2:    p^.a.scale := D.x2;
    | 4:    p^.a.scale := D.x4;
    | 8:    p^.a.scale := D.x8;
    END;
    p^.a.local := ir.UNDEFINED;
    p^.a.proc  := ir.ProcUNDEFINED;
    p^.a.offslist     := NIL;
    p^.a.proclist     := NIL;
    p^.a.bv    := NIL;
    p^.nt     := NTscaled;
END MultToScaled;

--------------------------------------------------------------------------------

(*
  reg << const -> scaled
*)

PROCEDURE ShiftToScaled* (p, p1, p2: DAGNODE);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.ShiftToScaled\n");
<* END *>
    p^.a.offs  := 0;
    p^.a.place1.v  := ir.UNDEFINED;
    p^.a.place1.r  := UNDEF_REG;
    p^.a.place2    := p1.place;
    IF (p^.a.place2.v <> ir.UNDEFINED) & (RD.Loc^[p^.a.place2.v].tag = NTlocal) THEN
        p^.a.place2.r := UNDEF_REG;
        reg.MarkUseReg (p1.place);
    END;
    CASE Calc.ToInteger (p2^.par^.value, p2^.sz)  OF
    | 1:    p^.a.scale := D.x2;
    | 2:    p^.a.scale := D.x4;
    | 3:    p^.a.scale := D.x8;
    END;
    p^.a.local := ir.UNDEFINED;
    p^.a.proc  := ir.ProcUNDEFINED;
    p^.a.offslist     := NIL;
    p^.a.proclist     := NIL;
    p^.a.bv    := NIL;
    p^.nt     := NTscaled;
END ShiftToScaled;

--------------------------------------------------------------------------------

(*
  reg * const -> addr
*)

PROCEDURE MultToAddr* (p, p1, p2: DAGNODE);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.MultToAddr\n");
<* END *>
    p^.a.offs  := 0;
    p^.a.place1    := p1.place;
    p^.a.place2    := p1.place;
    IF (p^.a.place1.v <> ir.UNDEFINED) & (RD.Loc^[p^.a.place1.v].tag = NTlocal) THEN
        p^.a.place1.r := UNDEF_REG;
        p^.a.place2.r := UNDEF_REG;
        reg.MarkUseReg (p1^.place);
    ELSIF p1^.place.r <> UNDEF_REG THEN
        reg.FillUsagesLeft (p1^.place.r,reg.usagesLeft [p1^.place.r] + 1);
    END;
    CASE Calc.ToInteger (p2^.par^.value, p2^.sz)  OF
    | 3:    p^.a.scale := D.x2;
    | 5:    p^.a.scale := D.x4;
    | 9:    p^.a.scale := D.x8;
    END;
    p^.a.local := ir.UNDEFINED;
    p^.a.proc  := ir.ProcUNDEFINED;
    p^.a.offslist     := NIL;
    p^.a.proclist     := NIL;
    p^.a.bv    := NIL;
    p^.nt     := NTaddr;
END MultToAddr;

--------------------------------------------------------------------------------

(*
  addr + addr -> addr
*)

PROCEDURE AddAddrAddrToAddr* (p, p1, p2: DAGNODE;
                              r1, r2: RegPlacement; s: D.ScaleType);
VAR op: OffsRecPtr;
    pp: ProcRecPtr;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.AddAddrAddrToAddr\n");
<* END *>
    p^.a.offs  := p1^.a.offs + p2^.a.offs;
    p^.a.place1    := r1;
    p^.a.place2    := r2;
    p^.a.scale := s;
    p^.a.proc  := ir.ProcUNDEFINED;
    p^.a.proclist     := NIL;
    IF p1^.a.local = ir.UNDEFINED THEN
        p^.a.local := p2^.a.local;
        p^.a.offslist     := p2^.a.offslist;
    ELSE
        p^.a.local := p1^.a.local;
        p^.a.offslist     := p1^.a.offslist;
        IF p2^.a.offslist <> NIL THEN
            IF p^.a.offslist = NIL THEN
                p^.a.offslist := p2^.a.offslist;
            ELSE
                p^.a.offslist^.next := p2^.a.offslist;
            END;
        END;
        IF p2^.a.local <> ir.UNDEFINED THEN
            NEW (op);
            op^.name := p2^.a.local;
            op^.next := p^.a.offslist;
            p^.a.offslist   := op;
        END;
    END;
    IF p1^.a.proc = ir.ProcUNDEFINED THEN
        p^.a.proc := p2^.a.proc;
        p^.a.proclist     := p2^.a.proclist;
    ELSE
        p^.a.proc := p1^.a.proc;
        p^.a.proclist     := p1^.a.proclist;
        IF p2^.a.proclist <> NIL THEN
            IF p^.a.proclist = NIL THEN
                p^.a.proclist := p2^.a.proclist;
            ELSE
                p^.a.proclist^.next := p2^.a.proclist;
            END;
        END;
        IF p2^.a.proc <> ir.ProcUNDEFINED THEN
            NEW (pp);
            pp^.name := p2^.a.proc;
            pp^.next := p^.a.proclist;
            p^.a.proclist   := pp;
        END;
    END;
    IF p1^.a.bv <> NIL THEN
        p^.a.bv := p1^.a.bv;
    ELSE
        p^.a.bv := p2^.a.bv;
    END;
    p^.nt := NTaddr;
END AddAddrAddrToAddr;

--------------------------------------------------------------------------------

(*
  par -> addr
*)

PROCEDURE ParToAddr* (VAR a: AddrMode; p: ParamPtr);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.ParToAddr\n");
<* END *>
    Emit.InitAddrMode (a);
    CASE p^.tag OF
    | ir.y_NumConst:
        a.offs  := ToInteger (p^.value, 4);
    | ir.y_AddrConst:
        a.local := p^.name;
        a.offs  := p^.offset;
    | ir.y_ProcConst:
        a.proc  := VAL(prc.ProcNum, p^.name);
    | ir.y_RealConst:
        xiEnv.errors.Fault (p^.value^.pos, 952);
        HALT;
    END;
END ParToAddr;

--------------------------------------------------------------------------------

(*
  Сгенерировать пересылку параметра в регистр
*)

PROCEDURE ParamPtrToReg* (r: Reg; p: ParamPtr; sz: SizeType);
VAR a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.ParamPtrToReg\n");
<* END *>
    CASE p^.tag OF
    | ir.y_NumConst,
      ir.y_RealConst:
        Emit.GenMoveR_INum (r, p.value);
    | ir.y_AddrConst:
        IF NOT ir.IsExternal (p^.name) THEN
            StackAddrToBased (a, p);
            Emit.GenLEA (r, a);
        ELSE
            ParToAddr (a, p);
            Emit.GenMoveR_Iglobal (r, a);
        END;
    | ir.y_ProcConst:
        ParToAddr (a, p);
        Emit.GenMoveR_Iglobal (r, a);
    | ir.y_Nothing:
        IF MustFillFrame & (sz = 4) THEN
            Emit.GenMoveR_INum (r, Calc.GetInteger(0,4));
        END;
    END;
END ParamPtrToReg;

--------------------------------------------------------------------------------

(*
  регистр := регистр;
  spill'а быть не может
*)

PROCEDURE MoveRegReg (r: Reg; p: DAGNODE(*; sz: SizeType*));
VAR a: AddrMode;
    found : Reg;
    srcreg:Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.MoveRegReg\n");
<* END *>
    reg.CheckSpilledReg(p^.place);
    srcreg := p^.place.r;
    IF p^.place.v # ir.UNDEFINED THEN
      found := reg.FindInR(p^.place.v, D.allowedIRegs[ir.Vars^[p^.place.v].Def^.ResSize]);
      IF found # UNDEF_REG THEN
        srcreg := found;
      END;
    END;
    IF srcreg = UNDEF_REG THEN
        IF reg.regContents [r] <> p^.place.v THEN
            FormMem (a, p^.place.v);
            Emit.GenMoveR_M (r, a);
            IF D.RegInfo[r].sz = ir.Vars^[p^.place.v].Def^.ResSize THEN
                reg.MarkVarInReg( p^.place.v, r );
            ELSE
                reg.WipeReg(r);
            END;
        END;
    ELSIF srcreg <> r THEN
        Emit.GenMoveR_R (r, srcreg);
        ASSERT( (reg.regContents[srcreg] = p.place.v) OR (srcreg IN D.SPILLED_REGS) );
        reg.MarkVarInReg( p.place.v, r );
    END;
    reg.OccupyReg(r);
END MoveRegReg;

--------------------------------------------------------------------------------

(*
  reg := mrc; Может быть spill
*)

PROCEDURE MRCToReg (r: Reg; p: DAGNODE;(* sz: SizeType; *)free: BOOLEAN);
VAR a: AddrMode;
    sz: SizeType;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.MRCToReg\n");
<* END *>
    sz := D.RegInfo[r].sz;
    CASE p^.nt OF
    | NTconst:
        ParamPtrToReg (r, p^.par, sz);
    | NTreg:
        IF free THEN
            MoveRegReg (r, p);
            reg.MarkUseReg (p^.place);
        ELSE                            -- То же, что и MoveRegReg,
                                        -- но не трогаем reg.regContents и reg.freeRegs
            IF p^.place.r = UNDEF_REG THEN
                IF reg.regContents [r] <> p^.place.v THEN
                    FormMem (a, p^.place.v);
                    Emit.GenMoveR_M (r, a);
                END;
            ELSIF p^.place.r <> r THEN
                Emit.GenMoveR_R (r, p^.place.r);
            END;
        END;
    | NTmem:
        IF free THEN
            GetAddrModeUsingR (r, p^.a, opTune.addr_sz);
            reg.MarkUseAddrMode      (p^.a);
        END;
        Emit.GenMoveR_M (r, p^.a);
    END;
END MRCToReg;

--------------------------------------------------------------------------------

(*
  reg := mrc( trash extension ); Может быть spill
*)

PROCEDURE MRCToRegTX (r: Reg; p: DAGNODE; free: BOOLEAN);
VAR a: AddrMode;
    sz: SizeType;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.MRCToRegTX\n");
<* END *>
    sz := D.RegInfo[r].sz;
    CASE p^.nt OF
    | NTconst:
        ParamPtrToReg (r, p^.par, sz);
    | NTreg:
        IF free THEN
            MoveRegReg (r, p);
            reg.MarkUseReg (p^.place);
        ELSE                            -- То же, что и MoveRegReg,
                                        -- но не трогаем reg.regContents и reg.freeRegs
            IF p^.place.r = UNDEF_REG THEN
                IF reg.regContents [r] <> p^.place.v THEN
                    FormMem (a, p^.place.v);
                    Emit.GenMoveR_M (r, a);
                END;
            ELSIF p^.place.r <> r THEN
                Emit.GenMoveR_R (r, D.ChSize(p^.place.r, sz));
            END;
        END;
    | NTmem:
        IF free THEN
            GetAddrModeUsingR (r, p^.a, opTune.addr_sz);
            reg.MarkUseAddrMode      (p^.a);
        END;
        Emit.GenMoveR_M (r, p^.a);
    END;
END MRCToRegTX;

(*
PROCEDURE MRCToRegEx (r1, r2: SHORTINT; p: DAGNODE; sz: SizeType; free: BOOLEAN);
BEGIN
  ASSERT((p^.nt = mem));
  IF free THEN
    GetAddrModeUsingR (r1, p^.a);
    reg.MarkUseAddrMode      (p^.a);
  END;
  IF sz = 8 THEN
    Emit.GenMoveR_M (r1, p^.a, 4);
  ELSE
    Emit.GenMoveR_M (r1, p^.a, sz);
  END;
  IF sz = 8 THEN
    INC(p^.a.offs, 4);
    IF free THEN
      GetAddrModeUsingR (r2, p^.a);
      reg.MarkUseAddrMode      (p^.a);
    END;
    Emit.GenMoveR_M (r2, p^.a, 4);
  END;
END MRCToRegEx;
*)

--------------------------------------------------------------------------------

(*
  reg := sx (mrc); Может быть spill
*)

PROCEDURE MRCToRegSX (r: Reg; p: DAGNODE; srcsz: SizeType);
VAR a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.MRCToRegSX\n");
<* END *>
    CASE p^.nt OF
    | NTconst:
        ParamPtrToReg (r, p^.par, D.RegInfo[r].sz);
    | NTreg:
        IF p^.place.r = UNDEF_REG THEN
            IF reg.regContents [r] = p^.place.v THEN
                Emit.GenMovesxR_R (r, r);
            ELSE
                FormMem (a, p^.place.v);
                Emit.GenMovesxR_M (r, a, srcsz);
            END;
        ELSE
            Emit.GenMovesxR_R (r, p^.place.r);
            reg.MarkUseReg (p^.place);
        END;
    | NTmem:
        GetAddrModeUsingR (r, p^.a, opTune.addr_sz);
        reg.MarkUseAddrMode      (p^.a);
        Emit.GenMovesxR_M (r, p^.a, srcsz);
    END;
END MRCToRegSX;

--------------------------------------------------------------------------------

(*
  reg := zx (mrc); Может быть spill
*)

PROCEDURE MRCToRegZX (r: Reg; p: DAGNODE; srcsz: SizeType);
VAR a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.MRCToRegZX\n");
<* END *>
    CASE p^.nt OF
    | NTconst:
        ParamPtrToReg (r, p^.par, D.RegInfo[r].sz);
    | NTreg:
        IF p^.place.r = UNDEF_REG THEN
            IF reg.regContents [r] = p^.place.v THEN
                Emit.GenMovezxR_R (r, r);
            ELSE
                FormMem (a, p^.place.v);
                Emit.GenMovezxR_M (r, a, srcsz);
            END;
        ELSE
            Emit.GenMovezxR_R (r, p^.place.r);
            reg.MarkUseReg (p^.place);
        END;
    | NTmem:
        GetAddrModeUsingR (r, p^.a, opTune.addr_sz);
        reg.MarkUseAddrMode      (p^.a);
        Emit.GenMovezxR_M (r, p^.a, srcsz);
    END;
END MRCToRegZX;
--------------------------------------------------------------------------------
PROCEDURE MRCHiToReg (r: Reg; p: DAGNODE);
VAR a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.MRCHiToReg\n");
<* END *>
    ASSERT(D.RegInfo[r].sz = 4);
    CASE p^.nt OF
--    | NTconst:
--        ParamPtrToReg (r, p^.par, D.RegInfo[r].sz); --FIXME
    | NTreg:
        IF p^.place.r = UNDEF_REG THEN
            IF reg.regContents [r] = p^.place.v THEN
--                Emit.GenMovezxR_R (r, r);
            ELSE
                FormMem (a, p^.place.v);
                INC(a.offs,4);
                Emit.GenMoveR_M (r, a);
            END;
        ELSE
            Emit.GenMoveR_R (r, D.HighPart[p^.place.r]);
            reg.MarkUseReg (p^.place);
        END;
    | NTmem:
        GetAddrModeUsingR (r, p^.a, opTune.addr_sz);
        reg.MarkUseAddrMode      (p^.a);
        INC(p.a.offs,4);
        Emit.GenMoveR_M (r, p^.a);
        DEC(p.a.offs,4);
    END;
END MRCHiToReg;
--------------------------------------------------------------------------------
(*
  reg := trunc (mrc); Может быть spill
*)

PROCEDURE MRCToRegTrunc (r: Reg; p: DAGNODE );
VAR a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.MRCToRegTrunc\n");
<* END *>
    CASE p^.nt OF
    | NTconst:
        ParamPtrToReg (r, p^.par, D.RegInfo[r].sz);
    | NTreg:
        IF p^.place.r = UNDEF_REG THEN
            IF reg.regContents [r] # p^.place.v THEN
                FormMem (a, p^.place.v);
                Emit.GenMoveR_M (r, a);
            END;
        ELSE
            Emit.GenMoveR_R (r, D.ChSize(p^.place.r, D.RegInfo[r].sz));
            reg.MarkUseReg (p^.place);
        END;
    | NTmem:
        GetAddrModeUsingR (r, p^.a, opTune.addr_sz);
        reg.MarkUseAddrMode      (p^.a);
        Emit.GenMoveR_M (r, p^.a);
    END;
END MRCToRegTrunc;

--------------------------------------------------------------------------------

(*
  Сгенерировать пересылку переменной в регистр
*)

PROCEDURE VarToReg* (r: Reg; v: VarNum; sz: SizeType);
VAR a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.VarToReg\n");
<* END *>
    IF RD.Loc^[v].tag = NTreg THEN
        IF RD.Loc^[v].reg <> r THEN
            Emit.GenMoveR_R (r, RD.Loc^[v].reg);
        END;
    ELSE
        FormMem (a, v);
        Emit.GenMoveR_M (r, a);
    END;
END VarToReg;

--------------------------------------------------------------------------------

(*
  reg -> local
*)

PROCEDURE RegToLocal* (p: DAGNODE; sz: SizeType);
VAR a: AddrMode;
    r: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.RegToLocal\n");
<* END *>
    ASSERT (p^.place.r <> UNDEF_REG);
    FormMem (a, p^.tr^.Name);
    IF p^.place.r IN D.allowedIRegs [sz] THEN
        Emit.GenMoveM_R (a, p^.place.r);
    ELSE
        r := reg.GTR (D.allowedIRegs [sz]);
        reg.MarkVarInReg( p^.tr^.Name, r );
        Emit.GenMoveR_R (r, p^.place.r);
        Emit.GenMoveM_R (a, r);
    END;
    reg.MarkUseReg (p^.place);
END RegToLocal;

--------------------------------------------------------------------------------

PROCEDURE ReasonableReg (v: VarNum; r: Reg): BOOLEAN;
BEGIN
    RETURN (RD.Loc^[v].tag <> NTreg) OR
           ((r IN D.SavedByProc) OR
            NOT (ir.o_LiveAtCall IN ir.Vars^[v].Options)) &
           (~(r IN RegSet{D.EAX, D.AX, D.AL}) OR
            NOT (ir.o_LiveAtFCom IN ir.Vars^[v].Options)) &
           (~(r IN RegSet{D.EAX, D.AX, D.AL, D.EDX, D.DX, D.DL}) OR
            NOT (ir.o_LiveAtMulDiv IN ir.Vars^[v].Options));
END ReasonableReg;

--------------------------------------------------------------------------------

(*
  LEA reg, mem
*)

PROCEDURE LEA* (p: DAGNODE; sz: SizeType);
VAR r: Reg;
    v: VarNum;
    allowedregset, hookedRegsV : RegSet;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.LEA\n");
<* END *>
    GetAddrMode  (p^.a, sz);
    reg.MarkUseAddrMode (p^.a);
    allowedregset := D.allowedIRegs [sz];
--    IF (sz = 1) & ()

    IF (p^.op = ir.o_par) THEN
        v := ir.UNDEFINED;
        r := reg.GTR (D.allowedIRegs [sz]);
    ELSE
        v := p^.tr^.Name;
        hookedRegsV := reg.HookedRegs (v);
        IF (RD.Loc^[v].tag = NTreg) & (RD.Loc^[v].reg <> UNDEF_REG) THEN
            r := reg.GR (v, D.allIRegs);
        ELSIF (p^.a.place1.r <> UNDEF_REG) &
           ((p^.a.place2.r = UNDEF_REG) OR
            (p^.a.scale = D.x1) & (p^.a.offs = 0)) &
           (reg.usagesLeft [p^.a.place1.r] = 0) &
           ((RD.Loc^[v].tag <> NTreg) OR NOT (p^.a.place1.r IN hookedRegsV)) &
           ReasonableReg (v, p^.a.place1.r) &
           (p^.a.place1.r IN D.allowedIRegs [sz])
        THEN
            r := p^.a.place1.r;
            reg.SetLoc (v, r);
        ELSIF (p^.a.place2.r <> UNDEF_REG) & (p^.a.scale = D.x1) &
              ((p^.a.offs = 0) OR (p^.a.place1.r = UNDEF_REG)) &
              (reg.usagesLeft [p^.a.place2.r] = 0) &
              ((RD.Loc^[v].tag <> NTreg) OR NOT (p^.a.place2.r IN hookedRegsV)) &
              ReasonableReg (v, p^.a.place2.r) &
              (p^.a.place2.r IN D.allowedIRegs [sz])
        THEN
            r := p^.a.place2.r;
            reg.SetLoc (v, r);
        ELSE
            r := reg.gr (v, D.allIRegs);
        END;
    END;
    Emit.GenLEA (r, p^.a);
    reg.VarInReg (v, r);
    p^.place.r := r;
    p^.place.v := v;
    p^.nt := NTreg;
END LEA;

--------------------------------------------------------------------------------

(*
  MOV reg, mem
*)

PROCEDURE LoadrToReg* (p, l: DAGNODE; sz: SizeType);
VAR r: Reg;
    v: VarNum;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.LoadrToReg\n");
<* END *>
    GetAddrMode  (l^.a, opTune.addr_sz);
    reg.MarkUseAddrMode (l^.a);
    v := p^.tr^.Name;
    r := reg.gr (v, D.allIRegs);
    l^.a.bv := p^.tr^.Read;
    Emit.GenMoveR_M (r, l^.a);
    reg.VarInReg (v, r);
    p^.place.r := r;
    p^.place.v := v;
    p^.nt := NTreg;
END LoadrToReg;

--------------------------------------------------------------------------------

(*
  Параметр лежит в "родной" памяти?
*)

PROCEDURE GetParInLocal* (p: TriadePtr): BOOLEAN;
BEGIN
    RETURN (Color.Allocation^[p^.Name].Offset = 0) &
           (Color.Allocation^[p^.Name].Location = prc.ParamLoc (p^.NPar));
END GetParInLocal;

--------------------------------------------------------------------------------

(*
  Параметр в регистр
*)

PROCEDURE GetParToReg* (p: DAGNODE);
VAR r: Reg;
    v: VarNum;
    a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.GetParToReg\n");
<* END *>
    v := p^.tr^.Name;
    r := reg.gr (v, D.allIRegs);
    FormStackOffset (a, prc.ParamOffs (p^.tr^.NPar));
    Emit.GenMoveR_M (r, a);
    reg.VarInReg (v, r);
    p^.place.r := r;
    p^.place.v := v;
    p^.nt := NTreg;
    IF Emit.work = Emit.full THEN
        CODE_START := RD.NodeInfo^[ir.ZERONode].sg.code_len;
    END;
END GetParToReg;

--------------------------------------------------------------------------------

PROCEDURE PushTOS(sz: SizeType);
VAR a:    AddrMode;
BEGIN
    ASSERT ((sz = 4) OR (sz = 8) OR (sz = 12));
    Emit.GenSubR_INum (D.ESP, Calc.GetInteger(sz,4));
    INC (Emit.PushSize, sz);
    FormStackOffset (a, 0);
    a.place1.r := D.ESP;
    a.offs := 0;
    Emit.work.GenMoveM_TOS (a, sz);
    DEC (Emit.FloatSize);
END PushTOS;
--------------------------------------------------------------------------------

PROCEDURE Push (p: DAGNODE; sz: SizeType; free: BOOLEAN);
VAR a:    AddrMode;
    r:    Reg;
--    b:    BOOLEAN;
--    z, u: SHORTINT;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.Push\n");
<* END *>
    IF sz = 10 THEN
        sz := 12;
    END;
    CASE p^.nt OF
    | NTconst:
        CASE p^.par^.tag OF
        | ir.y_RealConst:
                Emit.work.GenPush_INum (Emit.GetVal (p^.par, sz), sz);
                INC (Emit.PushSize, 4);
        | ir.y_NumConst:
                Emit.GenPush_INum(p^.par^.value, sz);
        | ir.y_AddrConst,
          ir.y_ProcConst:
            ParToAddr (a, p^.par);
            Emit.work.GenPush_Iglobal (a);
            INC (Emit.PushSize, 4);
        END;
    | NTreg:
        IF free THEN
            ToReg (p^.place, D.allowedIRegs [sz], FALSE);
            reg.MarkUseReg (p^.place);
        END;
        Emit.GenPush_R (p^.place.r);
        INC (Emit.PushSize, 4);
        IF p^.place.r IN D.REGS8 THEN
          INC (Emit.PushSize, 4);
        END;
    | NTmem:
        IF (sz <= 4) & (p^.op = ir.o_par) &
           (p^.par^.tag = ir.y_Variable) & (RD.Loc^[p^.par^.name].tag = NTlocal)
        THEN
            r := reg.FindInR (p^.par^.name, D.allIRegs);
            IF r <> UNDEF_REG THEN
                Emit.GenPush_R (r);
                INC (Emit.PushSize, 4);
                IF r IN D.REGS8 THEN
                  INC (Emit.PushSize, 4);
                END;
                RETURN;
            END;
        END;
(*        IF NOT (at.SPACE IN at.COMP_MODE) & (at.CPU <> at.i386) THEN
            b := free;               -- Надо ли освобождать способ адресации
            z := (sz + 3) DIV 4 * 4; -- Длина (в полных словах) объекта
            u := sz;                 -- Размер одного читаемого из памяти слова
            IF u <> 1 THEN
                u := 4;
            END;
            ASSERT ((z = 4) OR (z = 8) OR (z = 12));
            IF free THEN
                GetAddrMode (p^.a);
                IF z = 4 THEN
                    b := FALSE;
                    reg.MarkUseAddrMode (p^.a);
                END;
            END;
            INC (p^.a.offs, z);
            r := reg.GTR (D.allowedIRegs [u], u);
            REPEAT
                DEC (p^.a.offs, 4);
                Emit.GenMoveR_M (r, p^.a, u);
                Emit.GenPush_R  (r);
                INC (Emit.PushSize, 4);
                DEC (z, 4);
            UNTIL z = 0;
            IF (sz <= 4) & (p^.op = ir.o_par) &
               (p^.par^.tag = ir.y_Variable) &
               (RD.Loc^[p^.par^.name].tag = NTlocal)
            THEN
                reg.regContents [r] := p^.par^.name;
            ELSE
                reg.regContents [r] := ir.UNDEFINED;
            END;
            INCL (reg.freeRegs, r);
            IF b THEN
                reg.MarkUseAddrMode (p^.a);
            END;
        ELSE
*)
            IF p.a.place1.r = Emit.baseReg THEN
              IF sz < 4 THEN
                sz := 4;
              END;
            END;
            IF (sz = 1)OR(sz = 2) THEN
              r := reg.GTR(D.allowedIRegs [sz]);
              IF free THEN
                  GetAddrMode  (p^.a, opTune.addr_sz);
                  reg.MarkUseAddrMode (p^.a);
              END;
              Emit.GenMoveR_M(r, p^.a);
              Emit.GenPush_R(r);
              INC (Emit.PushSize, 4);
              reg.WipeReg(r);
            ELSE
              IF free THEN
                  GetAddrMode  (p^.a, opTune.addr_sz);
                  reg.MarkUseAddrMode (p^.a);
              END;
              Emit.GenPush_M(p^.a, sz);
            END;
--        END;
    | NTtos:
        PushTOS(sz);
    END;
END Push;

--------------------------------------------------------------------------------

(*
  Передать параметр: t - узел o_putpar, p - t^.l
*)

PROCEDURE PutPar* (t, p: DAGNODE);
BEGIN
    Push (p, t^.tr^.OpSize, TRUE);
END PutPar;

--------------------------------------------------------------------------------

(*
  Передать параметром агрегат в памяти: t - узел o_putpar, p - t^.l
*)

PROCEDURE PutAggregate* (t, p: DAGNODE);
VAR sz: INT;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.PutAggregate\n");
<* END *>
    GetAddrMode  (p^.a, opTune.addr_sz);
    reg.MarkUseAddrMode (p^.a);
    sz := (t^.tr^.OpSize + 3) DIV 4 * 4;
    INC (p^.a.offs, sz);
    REPEAT
        DEC (p^.a.offs, 4);
        Emit.work.GenPush_M (p^.a,4);
        INC (Emit.PushSize, 4);
        DEC (sz, 4);
    UNTIL sz = 0;
END PutAggregate;

--------------------------------------------------------------------------------

(*
  Передать параметр - вещественную константу
*)

PROCEDURE PutParRealConst* (t, p: DAGNODE);
VAR sz: SizeType;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.PutParRealConst\n");
<* END *>
    sz := t^.tr^.OpSize;
    IF sz = 10 THEN
        sz := 12;
    END;
    ASSERT ((sz = 4) OR (sz = 8) OR (sz = 12));
<* IF ~__GEN_C__ THEN *>
    Emit.GenPushReal (p^.par^.value, sz);
<* ELSE *>
    IF sz <= 8 THEN
      Emit.GenPushReal (p^.par^.value, sz);
    ELSE
      Emit.GenMoveTOS_INum (p^.par^.value, sz);
      PushTOS(sz);
    END;
<* END *>
    INC (Emit.PushSize, sz);
END PutParRealConst;

--------------------------------------------------------------------------------

PROCEDURE DoRaise* (t, p, file,line: DAGNODE);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.DoRaise\n");
<* END *>
    Push (line, 4, TRUE);
    Push (file, 4, TRUE);
    Push (p, 4, TRUE);
    Emit.work.GenRaise (ir.o_Assert IN t^.tr^.Options);
END DoRaise;

--------------------------------------------------------------------------------

PROCEDURE DoHalt* (p: DAGNODE);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.DoHalt\n");
<* END *>
    Push (p, 4, TRUE);
    Emit.work.GenHalt;
END DoHalt;

--------------------------------------------------------------------------------

PROCEDURE AdjustSP*;
VAR s: RegSet;
    r: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.AdjustSP\n");
<* END *>
    IF Emit.PushSize <> 0 THEN
        s := P.TrashedRegs(prc.ProcProtoNum(at.curr_procno))
           + reg.usedSoFar * D.allowedIRegs[4];
        IF (Emit.PushSize = 4) & (s * reg.freeRegs <> RegSet{}) THEN
            r := reg.FindFreeReg (s * reg.freeRegs);
            Emit.GenPop_R (r);
            reg.WipeReg(r);
        ELSE
            Emit.GenSubR_INum (D.ESP, Calc.GetInteger(-Emit.PushSize,4));
        END;
        Emit.PushSize := 0;
    END;
END AdjustSP;

--------------------------------------------------------------------------------

(*
  Спасти регистры вокруг вызова; m - какие регистры надо спасать.
  push = TRUE - можно спасать командой PUSH,
                иначе надо спасать в область сохранения.
  Вернуть размер области сохранения.
*)

PROCEDURE SaveCallRegs (push: BOOLEAN; m: RegSet): INT;
VAR r: Reg;
    s: INT;
    a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.SaveCallRegs\n");
<* END *>
    s := 0;
    FOR r:=D.MINREG TO D.MAXREG DO
        IF (D.RegAndIntersectWith[r] * m # RegSet{}) THEN
            IF r IN reg.busyRegs THEN
              IF push THEN
                  Emit.GenPush_R (r);
                  INC (Emit.PushSize, 4);
                  IF r IN D.REGS8 THEN
                    INC (Emit.PushSize, 4);
                  END;
              ELSE
                  FormStackOffset (a, TEMP_START + s);
                  Emit.GenMoveM_R (a, r);
                  INC (s, 4);
                  IF r IN D.REGS8 THEN
                    INC (s, 4);
                  END;
              END;
            END;
        END;
    END;
    RETURN s;
END SaveCallRegs;


--------------------------------------------------------------------------------

(*
  Спасти вещественные регистры вокруг вызова
  Вернуть увеличенный размер области сохранения.
*)

PROCEDURE SaveCallFRegs (s, n: INT): INT;
VAR a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.SaveCallFRegs\n");
    IF n > 0 THEN xiEnv.info.print("\n==== r386.SaveCallFRegs: %d\n", n); END;
<* END *>

    WHILE n > 0 DO
        FormStackOffset (a, TEMP_START + s);
        Emit.work.GenMoveM_TOS (a, 12);
        INC (s, 12);
        DEC (n);
    END;
    IF s > TEMP_SIZE THEN
        TEMP_SIZE := s;
    END;
    RETURN s;
END SaveCallFRegs;

--------------------------------------------------------------------------------

(*
  Восстановить регистры вокруг вызова
*)

PROCEDURE RestCallRegs (pop: BOOLEAN; s: INT; m, trashed: RegSet; res: Reg);
VAR r: Reg;
    a: AddrMode;
    var: ir.VarNum;
    left: SYSTEM.CARD32;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.RestCallRegs\n");
<* END *>
    FOR r:=D.MAXREG TO D.MINREG BY -1 DO
        IF (D.RegAndIntersectWith[r] * m # RegSet{}) THEN
            IF r IN reg.busyRegs THEN
              var := reg.regContents[r];
              left := reg.usagesLeft[r];
              reg.WipeReg(r);
              reg.OccupyReg(r);
              IF var # ir.UNDEFINED THEN
                reg.VarInReg(var,D.ChSize(r,ir.Vars[var].Def.ResSize));
              END;
              reg.FillUsagesLeft(r,left);
              IF pop THEN
                  Emit.GenPop_R (r);
                  DEC (Emit.PushSize, 4);
                  IF r IN D.REGS8 THEN
                    DEC (Emit.PushSize, 4);
                  END;
              ELSE
                  IF r IN D.REGS8 THEN
                    DEC (s, 4);
                  END;
                  DEC (s, 4);
                  FormStackOffset (a, TEMP_START + s);
                  Emit.GenMoveR_M ( r, a);
              END;
            END;
        ELSIF (r IN trashed) & ~(r IN D.RegAndIntersectWith[res]) THEN
            ASSERT (reg.usagesLeft [r] = 0);
            reg.WipeReg(r);
        END;
    END;
END RestCallRegs;

--------------------------------------------------------------------------------

(*
  Восстановить плавающие регистры вокруг вызова
*)

PROCEDURE RestCallFRegs (s, n: INT; b: BOOLEAN): INT;
VAR a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.RestCallFRegs\n");
<* END *>
    WHILE n > 0 DO
        DEC (s, 12);
        FormStackOffset (a, TEMP_START + s);
        Emit.work.GenMoveTOS_M (a, 12);
        IF b THEN
            Emit.GenFXCH ();
        END;
        DEC (n);
    END;
    RETURN s;
END RestCallFRegs;

--------------------------------------------------------------------------------

PROCEDURE GenMoveNWords (p: DAGNODE);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.GenMoveNWords\n");
<* END *>
    Emit.GenMoveR_INum (D.AL, Calc.GetInteger(prc.LenParams (p^.tr^.Prototype) DIV 4,4));
END GenMoveNWords;

--------------------------------------------------------------------------------

(*
  Вызов процедуры/функции t - o_call, p - t^.l
*)

PROCEDURE CallProcFunc* (t, p: DAGNODE; loc: NT);
VAR s:       INT;
    push:    BOOLEAN;
    y, m, u: RegSet;
    r:       Reg;
    v:       VarNum;
    a:       AddrMode;
    lang:    pc.Lang;
    os2:     BOOLEAN;
    intern:  BOOLEAN;
    wasEAXfree: BOOLEAN;
    wasEDXfree: BOOLEAN;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.CallProcFunc\n");
<* END *>
    reg.CheckSpilledNode (p);
    lang := prc.LangByProto (t^.tr^.Prototype);
    intern := (t^.tr^.Params^[0].tag = ir.y_ProcConst) &
              prc.IsInternal (VAL(prc.ProcNum, t^.tr^.Params^[0].name));
    os2 := NOT intern &
           ((lang = pc.flag_syscall) OR
            ((t^.tr^.Params^[0].tag <> ir.y_ProcConst) OR
             NOT prc.IsOurProc (VAL(prc.ProcNum, t^.tr^.Params^[0].name))) &
            (lang IN pc.LangSet{prc.Oberon, prc.Modula, pc.flag_c}) &
            (at.CC = at.OS2SYS_CALL));
    IF os2 THEN
        u := RegSet{ D.EAX };
    ELSE
        u := RegSet{};
    END;
    IF p^.nt = NTmem THEN
        IF os2 & (D.EAX IN reg.freeRegs) THEN
            reg.OccupyReg(D.EAX);
            reg.WipeReg(D.EAX);
            GetAddrMode (p^.a, opTune.addr_sz);
        ELSE
            GetAddrMode  (p^.a, opTune.addr_sz);
        END;
        reg.MarkUseAddrMode (p^.a);
    ELSIF p^.nt = NTreg THEN
        IF p^.place.r = UNDEF_REG THEN
            FormMem (p^.a, p^.place.v);
            p^.nt := NTmem;
        ELSE
            reg.MarkUseReg (p^.place);
        END;
    END;
    push := (NOT NeverUsePushInCall) AND (prc.ProtoList [t^.tr^.Prototype].npar = 0);

    IF ~os2 AND (t.tr.Params[0].tag = ir.y_ProcConst) AND
       prc.IsBackEndInfo (VAL(prc.ProcNum, t.tr.Params[0].name))
    THEN
        prc.GetBackEndInfo (VAL(prc.ProcNum, t.tr.Params[0].name), y);
        y := y * P.TrashedRegs(t.tr.Prototype);
    ELSE
        y := P.TrashedRegs(t.tr.Prototype);
    END;

    UsedByCalled := UsedByCalled + y;
    m := y - reg.freeRegs;
    s := SaveCallRegs  (push, m);
    s := SaveCallFRegs (s, Emit.FloatSize);

    CASE at.profilingMode OF
    | xProfRTS.PROF_MODE_STANDARD, xProfRTS.PROF_MODE_FULL:
        IF (p.nt#NTconst) OR ~(p^.par^.tag IN {ir.y_AddrConst, ir.y_ProcConst}) OR ~prc.IsCodeProc (p^.a.proc) THEN
          GenProfilerCall( std.X2C_PROFILE_NEST_START );
        END;
    ELSE -- nothing to do
    END;

    CASE p^.nt OF
    | NTreg:   IF os2 THEN
                    IF p^.place.r = D.EAX THEN
                        Emit.GenMoveR_R (D.ECX, D.EAX);
                        p^.place.r := D.ECX;
                    END;
                    GenMoveNWords (t);
                END;
                Emit.GenCall_R (p^.place.r, u, y, t^.tr^.Read, t^.tr^.Write);
    | NTmem:   IF os2 THEN
                    IF (p^.a.place1.r = D.EAX) OR (p^.a.place2.r = D.EAX) THEN
                        Emit.GenLEA (D.ECX, p^.a);
                        GenMoveNWords (t);
                        Emit.GenCall_R (D.ECX, RegSet{D.EAX}, y,
                                        t^.tr^.Read, t^.tr^.Write);
                    ELSE
                        GenMoveNWords (t);
                        Emit.work.GenCall_M (p^.a, D.PhysRegSet{D.EAXp}, D.RegSet2PhysRegSet(y),
                                        t^.tr^.Read, t^.tr^.Write);
                    END;
                ELSE
                    Emit.work.GenCall_M (p^.a, D.PhysRegSet{}, D.RegSet2PhysRegSet(y), t^.tr^.Read, t^.tr^.Write);
                END;
    | NTconst: IF os2 THEN
                    GenMoveNWords (t);
                END;
                CASE p^.par^.tag OF
                | ir.y_NumConst,
                  ir.y_RealConst:
                    Emit.work.GenCall_INum (Emit.GetVal (p^.par, 4),
                                        D.RegSet2PhysRegSet(u),
                                        D.RegSet2PhysRegSet(y),
                                       t^.tr^.Read, t^.tr^.Write);
                | ir.y_AddrConst,
                  ir.y_ProcConst:
                    ParToAddr (p^.a, p^.par);
                    Emit.work.GenCall_Iglobal (p^.a,
                                        D.RegSet2PhysRegSet(u),
                                        D.RegSet2PhysRegSet(y),
                                          t^.tr^.Read, t^.tr^.Write);
                END;
    END;

    DEC (Emit.PushSize, ProcRetSize(t.tr.Prototype, lang, intern));

    CASE at.profilingMode OF
    | xProfRTS.PROF_MODE_STANDARD, xProfRTS.PROF_MODE_FULL:
        IF (p.nt#NTconst) OR ~(p^.par^.tag IN {ir.y_AddrConst, ir.y_ProcConst}) OR ~prc.IsCodeProc (p^.a.proc) THEN
          GenProfilerCall( std.X2C_PROFILE_NEST_END );
        END;
    ELSE -- nothing to do
    END;

    r := UNDEF_REG;
    IF loc <> nts.NTstm THEN
        v := t^.tr^.Name;
        CASE loc OF
        | NTlocal:
            FormMem (a, v);
            IF P.CallResultInTOS (t^.tr) THEN
                IF (t^.tr^.Name >= Color.NNonTempVars) &
                   (ir.Vars^[t^.tr^.Name].Use = NIL)
                THEN
                    Emit.work.GenMoveSTi_TOS (0);
                ELSE
                  Emit.work.GenMoveM_TOS (a, t^.tr^.ResSize);
                END;
            ELSIF (t^.tr^.Name < Color.NNonTempVars) OR
                  (ir.Vars^[t^.tr^.Name].Use <> NIL)
            THEN
                IF t^.tr^.ResSize <= 4 THEN
                    Emit.GenMoveM_R (a, D.RegTable[D.EAXp, t^.tr^.ResSize]);
                ELSE
                    Emit.GenMoveM_R (a, D.EAX);
                    INC (a.offs, 4);
                    Emit.GenMoveM_R (a, D.EDX);
                END;
            END;
            s := RestCallFRegs (s, Emit.FloatSize, FALSE);
            t^.nt := NTlocal;
        | NTtos:
            IF P.CallResultInTOS (t^.tr) THEN
                s := RestCallFRegs (s, Emit.FloatSize, TRUE);
            ELSE
                s := RestCallFRegs (s, Emit.FloatSize, FALSE);
                IF s + ORD(t^.tr^.ResSize) > TEMP_SIZE THEN
                    TEMP_SIZE := s + ORD(t^.tr^.ResSize);
                END;
                FormStackOffset (a, TEMP_START + s);
                Emit.GenMoveM_R (a, D.EAX);
                IF t^.tr^.ResSize = 4 THEN
                    Emit.work.GenMoveTOS_M  (a, 4);
                ELSE
                    INC (a.offs, 4);
                    Emit.work.GenMoveM_R (a, D.EDXp, 4);
                    DEC (a.offs, 4);
                    Emit.work.GenMoveTOS_M  (a, 8);
                END;
            END;
            IncFloats;
            t^.nt := NTtos;
        | NTreg:
            SYSTEM.EVAL(reg.GetPreferredReg (D.RegTable[D.EAXp,t^.tr^.ResSize], v, TRUE, TRUE, RD.Loc[t.tr.Name].hasLongLifeTime));
            r := RD.Loc^[v].reg;
            IF P.CallResultInTOS (t^.tr) THEN
                IF TEMP_SIZE < s + 4 THEN
                    TEMP_SIZE := s + 4;
                END;
                FormStackOffset (a, TEMP_START + s);
                Emit.work.GenMoveM_TOS (a, 4);
                Emit.GenMoveR_M (r, a);
            ELSE
                Emit.GenMoveR_R (r, D.CallResReg[t^.tr^.ResSize]);
            END;
            reg.VarInReg (v, r);
            t^.place.r := r;
            t^.place.v := v;
            t^.nt := NTreg;
            s := RestCallFRegs (s, Emit.FloatSize, FALSE);
        END;
    END;
    wasEAXfree := D.EAX IN reg.freeRegs;
    wasEDXfree := D.EDX IN reg.freeRegs;
    RestCallRegs (push, s, m, y, r);
    IF (t^.tr^.ResType <> ir.t_float) & (loc # nts.NTstm) THEN
      IF (t^.tr^.ResSize = 8) THEN
        IF wasEAXfree & wasEDXfree THEN
          reg.OccupyReg(D.EDX_EAX);
          reg.MarkVarInReg(t^.tr^.Name, D.EDX_EAX);
          reg.FreeReg(D.EDX_EAX);
        END;
      ELSE
        IF wasEAXfree THEN
            reg.OccupyReg(D.CallResReg[t^.tr^.ResSize]);
            reg.MarkVarInReg(t^.tr^.Name, D.CallResReg[t^.tr^.ResSize]);
            reg.FreeReg(D.CallResReg[t^.tr^.ResSize]);
        END;
      END;
    END;
END CallProcFunc;

--------------------------------------------------------------------------------

(*
  Возврат из процедуры
*)

PROCEDURE RetProc* (t: TriadePtr);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.RetProc\n");
<* END *>
    CodeDef.ret_node := t.NodeNo;
    IF Emit.work = Emit.full THEN
      CodeDef.ret_offs := RD.NodeInfo[t.NodeNo].sg.code_len;
    END;
    Epilogue(FALSE);
END RetProc;

--------------------------------------------------------------------------------

(*
  Возврат из функции
*)

PROCEDURE RetFunc* (p: DAGNODE; t: TriadePtr);
VAR a:  AddrMode;
    sz: SizeType;
    b:  BOOLEAN;
    r:  Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.RetFunc\n");
<* END *>
    sz := t^.ResSize;
    IF t^.ResType = ir.t_float THEN
        IF P.ProcResultInTOS(at.curr_procno) THEN
            CASE p^.nt OF
            | NTtos:
            | NTconst: Emit.GenMoveTOS_INum (t^.Params^[0].value, sz);
            | NTmem:   GetAddrMode  (p^.a, opTune.addr_sz);
                        reg.MarkUseAddrMode (p^.a);
                        Emit.work.GenMoveTOS_M (p^.a, sz);
            | NTreg:   IF p^.place.r = UNDEF_REG THEN
                            FormMem (a, p^.place.v);
                        ELSE
                            IF TEMP_SIZE < 8 THEN
                                TEMP_SIZE := 8;
                            END;
                            FormStackOffset (a, TEMP_START);
                            Emit.GenMoveM_R (a, p^.place.r);
                        END;
                        Emit.work.GenMoveTOS_M (a, sz);
            END;
            Emit.FloatSize := 0;
        ELSIF p^.nt = NTconst THEN
            reg.MarkUsedSoFar(D.EAX);
            IF sz = 8 THEN
                reg.MarkUsedSoFar(D.EDX);
            END;
            Emit.GenMoveRR_Real (D.EAX, D.EDX, t^.Params^[0].value, sz);
        ELSIF p^.nt = NTreg THEN
            reg.MarkUsedSoFar( D.EAX);
            IF p^.place.r = UNDEF_REG THEN
                FormMem (a, p^.place.v);
                Emit.GenMoveR_M (D.EAX, a);
            ELSIF p^.place.r <> D.EAX THEN
                Emit.GenMoveR_R (D.EAX, p^.place.r);
            END;
        ELSE
            reg.MarkUsedSoFar( D.EAX);
            IF p^.nt = NTtos THEN
                IF TEMP_SIZE < ORD(sz) THEN
                    TEMP_SIZE := sz;
                END;
                FormStackOffset   (a, TEMP_START);
                Emit.work.GenMoveM_TOS (a, sz);
                DEC (Emit.FloatSize);
            ELSE
                IF D.EAX IN reg.freeRegs THEN
                    reg.OccupyReg(D.EAX);
                    reg.MarkVarInReg(ir.UNDEFINED, D.EAX);
                END;
                GetAddrMode  (p^.a, opTune.addr_sz);
                reg.MarkUseAddrMode (p^.a);
                a := p^.a;
            END;
            IF sz = 4 THEN
                Emit.GenMoveR_M (D.EAX, a);
            ELSE
                reg.MarkUsedSoFar( D.EDX);
                b := (a.place1.r = D.EAX) OR (a.place2.r = D.EAX);
                IF b THEN
                    IF (a.place1.r = D.ECX) OR (a.place2.r = D.ECX) THEN
                        r := D.EBX;
                    ELSE
                        r := D.ECX;
                    END;
                    reg.MarkUsedSoFar(r);
                    Emit.GenMoveR_M (r, a);
                ELSE
                    Emit.GenMoveR_M (D.EAX, a);
                END;
                INC (a.offs, 4);
                Emit.GenMoveR_M (D.EDX, a);
                IF b THEN
                    Emit.GenMoveR_R (D.EAX, r);
                END;
            END;
        END;
    ELSIF p^.nt = NTtos THEN
        IF TEMP_SIZE < 4 THEN
            TEMP_SIZE := 4;
        END;
        FormStackOffset (a, TEMP_START);
        Emit.work.GenMoveM_TOS (a, 4);
        DEC (Emit.FloatSize);
        reg.MarkUsedSoFar( D.EAX);
        Emit.GenMoveR_M (D.EAX, a);
    ELSE
        reg.MarkUsedSoFar( D.EAX);
        IF sz = 8 THEN
          MRCToReg(D.EDX_EAX, p, (*sz,*) TRUE);
--          INCL (reg.usedSoFar, D.EDX);
--          MRCToRegEx (D.EAX, D.EDX, p, sz, TRUE);
        ELSE
          MRCToReg(D.RegTable[D.EAXp,sz], p, (*sz,*) TRUE);
--          MRCToReg(D.EAX, p, sz, TRUE);
        END;
    END;
    CodeDef.ret_node := t.NodeNo;
    IF Emit.work = Emit.full THEN
      CodeDef.ret_offs := RD.NodeInfo[t.NodeNo].sg.code_len;
    END;
    Epilogue(FALSE);
END RetFunc;

--------------------------------------------------------------------------------

(*
  MOV reg, imm или LEA reg, addr
*)

PROCEDURE ImmToReg* (p: DAGNODE; sz: SizeType);
VAR
  r, r2: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.ImmToReg\n");
<* END *>
(*
    IF sz = 8 THEN
      r  := reg.GTR (D.allowedIRegs [4]);
      r2 := reg.GTR (D.allowedIRegs [4]);
      reg.VarInReg      (ir.UNDEFINED, r);
      reg.VarInReg      (ir.UNDEFINED, r2);
      p^.place.r := D.Pair[r,r2];
    ELSE
*)
      r := reg.GTR (D.allowedIRegs [sz]);
      reg.VarInReg      (ir.UNDEFINED, r);
      ParamPtrToReg (r, p^.par, sz);
--    END;
    p^.place.r := r;
    p^.place.v := ir.UNDEFINED;
    p^.nt := NTreg;
END ImmToReg;

--------------------------------------------------------------------------------

(*
  MOV mem, imm
*)

PROCEDURE RCToMem* (ap, p: DAGNODE; sz: SizeType);
VAR r: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.RCToMem\n");
<* END *>
    GetAddrMode (ap^.a, opTune.addr_sz);
    CASE p^.nt OF
    | NTreg:
        ToReg (p^.place, D.allowedIRegs [sz], FALSE);
        IF p^.place.r IN D.allowedIRegs [sz] + D.SPILLED_REGS THEN
            Emit.GenMoveM_R (ap^.a, p^.place.r);
        ELSE
            r := reg.GTR (D.allowedIRegs [sz]);
            reg.WipeReg(r);
            Emit.GenMoveR_R (r, p^.place.r);
            Emit.GenMoveM_R (ap^.a, r);
        END;
        reg.MarkUseReg (p^.place);
    | NTconst:
        CASE p^.par^.tag OF
        | ir.y_NumConst:
                Emit.GenMoveM_INum (ap^.a, p^.par.value, sz);
        | ir.y_RealConst:
                Emit.GenFMoveM_INum (ap^.a, p^.par.value, sz);
(*            IF (* (sz <> 1) & *)
               NOT (at.SPACE IN at.COMP_MODE) &
               NOT (at.nooptimize IN at.COMP_MODE) &
               (at.CPU <> at.i386) &
               ((ap^.a.local <> ir.UNDEFINED) OR (ap^.a.offs <> 0))
            THEN
                r := reg.GTR (D.allowedIRegs [sz], sz);
                reg.regContents [r] := ir.UNDEFINED;
                INCL (reg.freeRegs, r);
                Emit.GenMoveR_INum (r, Emit.GetVal (p^.par, sz), sz);
                Emit.GenMoveM_R (ap^.a, r, sz);
            ELSE
*)
--            END;
        | ir.y_AddrConst,
          ir.y_ProcConst:
            ParToAddr (p^.a, p^.par);
            p^.nt := NTaddr;
            Emit.work.GenMoveM_Iglobal (ap^.a, p^.a, sz);
        END;
    END;
    reg.MarkUseAddrMode (ap^.a);
    IF (ap^.op = ir.o_par)  & (ir.o_Parameters IN ap^.par^.triade^.Options) OR
       (ap^.op <> ir.o_par) & (ir.o_Parameters IN ap^.tr^.Options)
    THEN
        IF Emit.work = Emit.full THEN
            CODE_START := RD.NodeInfo^[ir.ZERONode].sg.code_len;
        END;
    END;
END RCToMem;

--------------------------------------------------------------------------------

PROCEDURE BitTest* (where, which: DAGNODE);
--VAR a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.BitTest\n");
<* END *>
--    CASE where^.nt OF
--    | reg:
    Emit.GenBTR_R (where^.place.r, which^.place.r);
    reg.MarkUseReg (where^.place);
    reg.MarkUseReg (which^.place);
--    | mem:
--        GetAddrMode  (where^.a);
--        reg.MarkUseAddrMode (where^.a);
--        r := reg.GTR()
--        Emit.GenBTM_R (where^.a, which^.place.r, where^.tr^.OpSize);
--    END;
END BitTest;

--------------------------------------------------------------------------------
(*
  Сравнить Reg и MRC; eq - это если ResType = unsign,
                      но мы будем делать только jeq / jne
*)

PROCEDURE CompareR_MRC* (p, l, g: DAGNODE; eq: BOOLEAN);
VAR a:  AddrMode;
    sz: SizeType;
    r:  Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.CompareR_MRC\n");
<* END *>
    sz := p^.tr^.OpSize;
    CASE g^.nt OF
    | NTconst:
        ToReg (l^.place, D.allowedIRegs [sz], FALSE);
        CASE g^.par^.tag OF
        | ir.y_NumConst,
          ir.y_RealConst:
            IF ((p^.tr^.OpType = ir.t_int) OR eq) &
               (g^.par^.tag = ir.y_NumConst) &
               Calc.IsZero (g^.par^.value, p^.tr^.OpType, sz)
            THEN
                Emit.GenTestR_R (l^.place.r, l^.place.r);
            ELSE
                Emit.GenCmpR_INum (l^.place.r, g^.par.value);
            END;
        | ir.y_AddrConst,
          ir.y_ProcConst:
            ParToAddr (g^.a, g^.par);
            Emit.GenCmpR_Iglobal (l^.place.r, g^.a);
        END;
    | NTreg:
        IF g^.place.r = UNDEF_REG THEN
            r := reg.FindInR (g^.place.v, D.allowedIRegs [sz]);
        ELSE
            r := UNDEF_REG;
        END;
        IF r = UNDEF_REG THEN
            ToReg (l^.place, D.allowedIRegs [sz], FALSE);
            IF g^.place.r = UNDEF_REG THEN
                FormMem (a, g^.place.v);
                Emit.GenCmpR_M (l^.place.r, a);
            ELSE
                Emit.GenCmpR_R (l^.place.r, g^.place.r);
                reg.MarkUseReg (g^.place);
            END;
        ELSIF l^.place.r = UNDEF_REG THEN
            FormMem (a, l^.place.v);
            Emit.GenCmpM_R (a, r);
        ELSE
            Emit.GenCmpR_R (l^.place.r, r);
        END;
    | NTmem:
        IF (g^.op = ir.o_par) & (g^.par^.tag = ir.y_Variable) &
           (reg.FindInR (g^.par^.name, D.allowedIRegs [sz]) <> UNDEF_REG)
        THEN
            r := reg.FindInR (g^.par^.name, D.allowedIRegs [sz]);
            IF l^.place.r <> UNDEF_REG THEN
                Emit.GenCmpR_R (l^.place.r, r);
            ELSIF reg.FindInR (l^.place.v, D.allowedIRegs [sz]) <> UNDEF_REG THEN
                Emit.GenCmpR_R (reg.FindInR (l^.place.v, D.allowedIRegs [sz]),
                                r);
            ELSE
                FormMem (l^.a, l^.place.v);
                Emit.GenCmpM_R (l^.a, r);
            END;
        ELSE
            GetAddrMode (g^.a, opTune.addr_sz);
            ToReg (l^.place, D.allowedIRegs [sz], FALSE);
            Emit.GenCmpR_M (l^.place.r, g^.a);
            reg.MarkUseAddrMode (g^.a);
        END;
    END;
    reg.MarkUseReg (l^.place);
END CompareR_MRC;

--------------------------------------------------------------------------------

(*
  Сравнить Reg и Mem; eq - это если ResType = unsign,
                      но мы будем делать только jeq / jne
*)

PROCEDURE CompareM_RC* (p, l, g: DAGNODE; eq: BOOLEAN);
VAR sz: SizeType;
    r:  Reg;
    ps: RegSet;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.CompareM_RC\n");
<* END *>
    GetAddrMode (l^.a, opTune.addr_sz);
    sz := p^.tr^.OpSize;
--    ASSERT(sz <8 );
--    IF sz = 8 THEN
--      sz := 4;
--    END;
    ps := D.allowedIRegs [sz];
    IF (l^.op = ir.o_par) & (l^.par^.tag = ir.y_Variable) THEN
        r := reg.FindInR (l^.par^.name, ps);
    ELSE
        r := UNDEF_REG;
    END;
    CASE g^.nt OF
    | NTconst:
        CASE g^.par^.tag OF
        | ir.y_NumConst,
          ir.y_RealConst:
            IF r = UNDEF_REG THEN
--                IF NOT (at.SPACE IN at.COMP_MODE) & (at.CPU <> at.i386) &
--                   ((l^.a.local <> ir.UNDEFINED) OR (l^.a.offs <> 0))
--                THEN
                    IF ((p^.tr^.OpType = ir.t_int) OR eq) &
                        (g^.par^.tag = ir.y_NumConst) &
                        Calc.IsZero (g^.par^.value, p^.tr^.OpType, sz) &
                        (sz <=4)&
                        (reg.freeRegs*D.allowedIRegs [sz]#RegSet{})
                    THEN
                        r := reg.GTR (D.allowedIRegs [sz]);
                        IF (l^.op = ir.o_par) & (l^.par^.tag = ir.y_Variable) THEN
                          reg.MarkVarInReg(l^.par^.name, r);
                          reg.FreeReg(r);
                        ELSE
                          reg.WipeReg(r);
                        END;
                        Emit.GenMoveR_M (r, l^.a);
                        Emit.GenTestR_R (r, r);
                    ELSE
--                    ELSIF eq THEN
--                        Emit.GenMoveR_M (r, l^.a, sz);
--                        IF (l^.op = ir.o_par) & (l^.par^.tag = ir.y_Variable) THEN
--                            reg.regContents[r] := l.par.name;
--                        END;
--                        Emit.GenCmpR_INum (r, Emit.GetVal (g^.par, sz), sz);
--                    ELSE
--                        Emit.GenMoveR_INum (r, Emit.GetVal (g^.par, sz), sz);
--                        Emit.GenCmpM_R (l^.a, r, sz);
--                    END;
--                ELSE
                       Emit.GenCmpM_INum (l^.a, g^.par.value, sz);
                    END;
            ELSIF ((p^.tr^.OpType = ir.t_int) OR eq) &
               (g^.par^.tag = ir.y_NumConst) &
               Calc.IsZero (g^.par^.value, p^.tr^.OpType, sz)
            THEN
                Emit.GenTestR_R (r, r);
            ELSE
                Emit.GenCmpR_INum (r, g^.par.value);
            END;
        | ir.y_AddrConst,
          ir.y_ProcConst:
            ParToAddr (g^.a, g^.par);
            g^.nt := NTaddr;
            IF r = UNDEF_REG THEN
                Emit.GenCmpM_Iglobal (l^.a, g^.a, sz);
            ELSE
                Emit.GenCmpR_Iglobal (r, g^.a);
            END;
        END;
    | NTreg:
        IF (r <> UNDEF_REG) & (g^.place.r = UNDEF_REG) &
           (reg.FindInR (g^.place.v, ps) = UNDEF_REG)
        THEN
            FormMem (g^.a, g^.place.v);
            Emit.GenCmpR_M (r, g^.a);
        ELSE
            ToReg (g^.place, ps, FALSE);
            reg.MarkUseReg (g^.place);
            IF (r = UNDEF_REG) OR (r = g^.place.r) THEN
                Emit.GenCmpM_R (l^.a, g^.place.r);
            ELSE
                Emit.GenCmpR_R (r, g^.place.r);
            END;
        END;
    END;
    reg.MarkUseAddrMode (l^.a);
END CompareM_RC;

(*
  (MRC & par) = 0?
*)

PROCEDURE Test_MR* (p, l, g: DAGNODE);
VAR sz: SizeType;
    c:  CARD;
    r: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.Test_MR\n");
<* END *>
    sz := p^.tr^.OpSize;
    c  := Calc.ToCardinal (g^.par^.value, g^.sz);
    CASE l^.nt OF
    | NTreg:
        ToReg      (l^.place, D.allowedIRegs [sz], FALSE);
        reg.MarkUseReg (l^.place);
        IF (l^.place.r IN D.XREGS) & (c < 256) THEN
            r := D.ChSize( l^.place.r, 1 );
        ELSE
            r := l^.place.r;
        END;
        Emit.GenTestR_INum (r, g^.par.value);
    | NTmem:
        IF (l^.op = ir.o_par) & (l^.par^.tag = ir.y_Variable) THEN
            r := reg.FindInR (l^.par^.name, D.allowedIRegs [sz]);
        ELSE
            r := UNDEF_REG;
        END;
        IF (c < 256) & (r IN D.XREGS+RegSet{D.UNDEF_REG}) THEN
            sz := 1;
            IF r # UNDEF_REG THEN
              r := D.ChSize( r, 1 );
            END;
        END;
        GetAddrMode  (l^.a, opTune.addr_sz);
        reg.MarkUseAddrMode (l^.a);
        IF r = UNDEF_REG THEN
            Emit.work.GenTestM_INum (l^.a, Emit.GetVal (g^.par, sz), sz);
        ELSE
            Emit.GenTestR_INum (r, g^.par.value);
        END;
    END;
END Test_MR;

--------------------------------------------------------------------------------

(*
  Reg = NIL?
*)

PROCEDURE CompareR_NIL* (p, l: DAGNODE);
VAR sz: SizeType;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.CompareR_NIL\n");
<* END *>
    sz := p^.tr^.ResSize;
    ToReg (l^.place, D.allowedIRegs [sz], FALSE);
    reg.MarkUseReg (l^.place);
    IF Calc.IsZero (opTune.nil_val, ir.t_int, sz) THEN
        Emit.GenTestR_R (l^.place.r, l^.place.r);
    ELSE
        Emit.GenCmpR_INum (l^.place.r, opTune.nil_val);
    END;
END CompareR_NIL;

--------------------------------------------------------------------------------

(*
  Собственно выполнить бинарную операцию reg = reg op mrc
  (op = ADD, SUB, IMUL, LOGICAL, IMUL, SHIFT)
  Освободить регистр(ы) второго параметра;
  Может произойти spill!
*)

PROCEDURE OpR_MRC (r: Reg; g: DAGNODE; p: TriadePtr);
VAR a: AddrMode;
    t: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.OpR_MRC\n");
<* END *>
    CASE g^.nt OF
    | NTconst:
        CASE g^.par^.tag OF
        | ir.y_NumConst,
          ir.y_RealConst:
            Emit.GenOpR_INum (p, r, g^.par.value);
        | ir.y_AddrConst,
            ir.y_ProcConst:
            ParToAddr (g^.a, g^.par);
            g^.nt := NTaddr;
            Emit.GenOpR_Iglobal (p, r, g^.a);
        END;
    | NTreg:
        IF g^.place.r = UNDEF_REG THEN
            t := UNDEF_REG;
            IF (g^.op = ir.o_par) & (g^.par^.tag = ir.y_Variable) THEN
                t := reg.FindInR (g^.par^.name, D.allowedIRegs [p^.ResSize]);
                IF t = r THEN
                    t := UNDEF_REG;
                END;
            END;
            IF t = UNDEF_REG THEN
                FormMem (a, g^.place.v);
                Emit.GenOpR_M (p, r, a);
            ELSE
                Emit.GenOpR_R (p, r, t);
            END;
        ELSE
            Emit.GenOpR_R (p, r, g^.place.r);
            reg.MarkUseReg (g^.place);
        END;
    | NTmem:
        t := UNDEF_REG;
        IF (g^.op = ir.o_par) & (g^.par^.tag = ir.y_Variable) THEN
            t := reg.FindInR (g^.par^.name, D.allowedIRegs [p^.ResSize]);
            IF t = r THEN
                t := UNDEF_REG;
            END;
        END;
        IF t = UNDEF_REG THEN
            GetAddrMode  (g^.a, opTune.addr_sz);
            reg.MarkUseAddrMode (g^.a);
            Emit.GenOpR_M (p, r, g^.a);
        ELSE
            Emit.GenOpR_R (p, r, t);
        END;
    END;
END OpR_MRC;

--------------------------------------------------------------------------------

(*
  Выделить регистры для бинарной операции reg = reg op mrc
  (op = ADD, SUB, IMUL, LOGICAL, IMUL)
*)

PROCEDURE BinOp* (p, l, g: DAGNODE);
VAR v:    VarNum;
    t, r : Reg;
    sz: SizeType;
    hookedRegsV: RegSet;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.BinOp\n");
<* END *>
    reg.CheckSpilledReg  (l^.place);
    reg.CheckSpilledNode (g);
    v := p^.tr^.Name;
    sz := p^.tr^.ResSize;
    hookedRegsV := reg.HookedRegs (v);
    IF (RD.Loc^[v].tag = NTreg) & (RD.Loc^[v].reg <> UNDEF_REG) THEN
        r := RD.Loc^[v].reg;
        IF reg.usagesLeft [r] = 0 THEN
            IF (ir.isCommu IN ir.OpProperties [p^.op]) &
               (g^.nt = NTmem)
            THEN
                GetAddrMode  (g^.a, opTune.addr_sz);
                reg.MarkUseAddrMode (g^.a);
                r := RD.Loc^[v].reg;
                Emit.GenMoveR_M (r, g^.a);
                OpR_MRC  (r, l, p^.tr);
            ELSE
                MoveRegReg (r, l);
                reg.MarkUseReg (l^.place);
                OpR_MRC  (r, g, p^.tr);
            END;
        ELSIF r = l^.place.r THEN
            OpR_MRC (r, g, p^.tr);
        ELSIF (ir.isCommu IN ir.OpProperties [p^.op]) &
              (g^.nt = NTreg) & (r = g^.place.r)
        THEN
            OpR_MRC (r, l, p^.tr);
        ELSE                        -- r1 = r2 - r1
            t := UNDEF_REG;
            IF l^.place.v <> ir.UNDEFINED THEN
                t := reg.FindInR (l^.place.v,
                              D.allowedIRegs [sz] * reg.freeRegs);
            END;
            IF t = UNDEF_REG THEN
                t := reg.GTR (D.allowedIRegs [sz]);
                MoveRegReg (t, l);
                reg.MarkUseReg (l^.place);
            END;
            reg.OccupyReg(t);
            reg.MarkVarInReg(v,t);
            OpR_MRC (t, g, p^.tr);
            Emit.GenMoveR_R (r, t);
            reg.FreeReg(t);
        END;
    ELSIF (l^.place.r <> UNDEF_REG) &
          (l^.place.r IN D.allowedIRegs [reg.MinClusterSizeOfVar(v)]) &
          (reg.usagesLeft [l^.place.r] = 1 + UsedInNode (g, l^.place.r, FALSE)) &
          ((RD.Loc^[v].tag <> NTreg) OR NOT (l^.place.r IN hookedRegsV)) &
          ReasonableReg (v, l^.place.r)
    THEN
        r := l^.place.r;
        reg.SetLoc  (v, r, ~RD.Loc[v].hasLongLifeTime);
        OpR_MRC (r, g, p^.tr);
    ELSIF (ir.isCommu IN ir.OpProperties [p^.op]) &
          (g^.nt = NTreg) & (g^.place.r <> UNDEF_REG) &
          (g^.place.r IN D.allowedIRegs [sz]) &
          (reg.usagesLeft [g^.place.r] = 1 + UsedInNode (l, g^.place.r, FALSE)) &
          ((RD.Loc^[v].tag <> NTreg) OR NOT (g^.place.r IN hookedRegsV)) &
          ReasonableReg (v, g^.place.r)
    THEN
        r := g^.place.r;
        reg.SetLoc  (v, r);
        OpR_MRC (r, l, p^.tr);
    ELSIF (ir.isCommu IN ir.OpProperties [p^.op]) &
          (g^.nt = NTmem)
    THEN
        GetAddrMode  (g^.a, opTune.addr_sz);
        reg.MarkUseAddrMode (g^.a);
        r := reg.gr (v, D.allIRegs);
        Emit.GenMoveR_M (r, g^.a);
        OpR_MRC (r, l, p^.tr);
    ELSE
        r := reg.gr    (v, D.allIRegs);
        MoveRegReg (r, l);
        reg.MarkUseReg (l^.place);
        OpR_MRC    (r, g, p^.tr);
    END;
    IF RD.Loc^[v].tag = NTreg THEN
        r := RD.Loc^[v].reg;
    END;
    reg.VarInReg (v, r);
    p^.place.r := r;
    p^.place.v := p^.tr^.Name;
    p^.nt := NTreg;
END BinOp;

--------------------------------------------------------------------------------

(*
  Выделить регистры для операции reg = mr * const
*)

PROCEDURE IMulByConst* (p, l, g: DAGNODE);
VAR v: VarNum;
    r: Reg;
    a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.IMulByConst\n");
<* END *>
    ASSERT(p^.tr^.ResSize<=4);
    reg.CheckSpilledNode (l);
    reg.CheckSpilledNode (g);
    v := p^.tr^.Name;
    IF (RD.Loc^[v].tag = NTreg) & (RD.Loc^[v].reg <> UNDEF_REG) THEN
        r := RD.Loc^[v].reg;
    ELSIF (l^.nt = NTreg) & (l^.place.r <> UNDEF_REG) & (reg.usagesLeft [l^.place.r] = 1) &
          (l^.place.r IN D.allowedIRegs [p^.tr^.ResSize]) &
          ((RD.Loc^[v].tag <> NTreg) OR NOT (l^.place.r IN reg.HookedRegs (v))) &
          ReasonableReg (v, l^.place.r)
    THEN
        r := l^.place.r;
        reg.SetLoc (v, r);
    ELSE
        r := reg.gr (v, D.allIRegs);
    END;
    IF l^.nt = NTmem THEN
        GetAddrModeUsingR (r, l^.a, opTune.addr_sz);
        reg.MarkUseAddrMode (l^.a);
        Emit.GenIMulR_MC  (p^.tr, r, l^.a, g^.par.value);
    ELSIF l^.place.r = UNDEF_REG THEN
        FormMem (a, l^.place.v);
        Emit.GenIMulR_MC (p^.tr, r, a, g^.par.value);
    ELSE
        Emit.GenIMulR_RC (p^.tr, r, l^.place.r, Emit.GetVal (g^.par, g^.sz));
        reg.MarkUseReg (l^.place);
    END;
    IF RD.Loc^[v].tag = NTreg THEN
        r := RD.Loc^[v].reg;
    END;
    reg.VarInReg (v, r);
    p^.place.r := r;
    p^.place.v := v;
    p^.nt := NTreg;
END IMulByConst;

--------------------------------------------------------------------------------

(*
  Выделить регистры для унарной операции reg = op mrc
  (op = INC, DEC, NEG, NOT (XOR 1), SGNEXT)
*)

PROCEDURE UnOp* (p, l: DAGNODE);
VAR
  v,src : VarNum;
  newhigh,r : Reg;
  r_sz, o_sz: SizeType;
  hookedRegsV: RegSet;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.UnOp\n");
<* END *>
    reg.CheckSpilledNode (l);
    v  := p^.tr^.Name;
    r_sz := p^.tr^.ResSize;
    o_sz := p^.tr^.OpSize;
(*
    IF r_sz = 8 THEN
      r_sz := 4;
    END;
    IF o_sz = 8 THEN
      o_sz := 4;
    END;
*)
    newhigh := UNDEF_REG;
    IF l.op = ir.o_par THEN
      src := l.par.name
    ELSE
      src := l.tr.Name;
    END;
--    hookedRegsV := reg.HookedRegsWOVar (v, src);
    hookedRegsV := reg.HookedRegs (v);

    IF (r_sz = 8)&(o_sz <8) AND (l^.place.r IN D.allowedIRegs[o_sz]) THEN
      IF RD.Loc^[v].tag <> NTreg THEN
        IF reg.freeRegs*D.allowedIRegs[4]*D.HighPartCand[D.ChSize(l^.place.r,4)] # RegSet{} THEN
          newhigh:=reg.FindFreeReg(reg.freeRegs*D.allowedIRegs[4]*D.HighPartCand[D.ChSize(l^.place.r,4)]);
          IF (D.ChSize(l^.place.r,4) = D.EAX) & (D.EDX IN reg.freeRegs) THEN
            newhigh := D.EDX;
          END;
        END;
      ELSIF reg.freeRegs*(D.allowedIRegs[4]-hookedRegsV)*D.HighPartCand[D.ChSize(l^.place.r,4)] # RegSet{} THEN
        newhigh:=reg.FindFreeReg(reg.freeRegs*(D.allowedIRegs[4]-hookedRegsV)*D.HighPartCand[D.ChSize(l^.place.r,4)]);
        IF (D.ChSize(l^.place.r,4) = D.EAX) & (D.EDX IN reg.freeRegs-hookedRegsV) THEN
          newhigh := D.EDX;
        END;
      END;
    END;
    IF (RD.Loc^[v].tag = NTreg) & (RD.Loc^[v].reg <> UNDEF_REG) THEN
        r := RD.Loc^[v].reg;
    ELSIF (r_sz <=4)&(l^.nt = NTreg) & (l^.place.r <> UNDEF_REG) & (reg.usagesLeft [l^.place.r] = 1) &
          ((RD.Loc^[v].tag <> NTreg) OR NOT (l^.place.r IN hookedRegsV)) &
          (D.ChSize(l^.place.r, r_sz) IN D.allowedIRegs [r_sz]) &
          ReasonableReg (v, l^.place.r)
    THEN
        r := D.ChSize(l^.place.r, r_sz);
        reg.SetLoc (v, r);
    ELSIF (r_sz = 8)&(o_sz <8)&(l^.nt = NTreg) & (l^.place.r <> UNDEF_REG) & (reg.usagesLeft [l^.place.r] = 1) &
          (newhigh#UNDEF_REG)&
          ((RD.Loc^[v].tag <> NTreg) OR (NOT (l^.place.r IN hookedRegsV))) &
          ReasonableReg (v, D.Pair[newhigh,D.ChSize(l^.place.r,4)])
    THEN
        r := D.Pair[newhigh,D.ChSize(l^.place.r,4)];
        reg.SetLoc (v, r);
    ELSIF (r_sz = 8)&(o_sz =8)&(l^.nt = NTreg) & (l^.place.r <> UNDEF_REG) & (reg.usagesLeft [l^.place.r] = 1) &
          ((RD.Loc^[v].tag <> NTreg) OR (NOT (l^.place.r IN hookedRegsV))) &
          ReasonableReg (v, l^.place.r)
    THEN
        r := l^.place.r;
        reg.SetLoc (v, r);
    ELSE
        r := reg.gr (v, D.allIRegs);
    END;
    IF RD.Loc^[v].tag = NTreg THEN
        r := RD.Loc^[v].reg;
        hookedRegsV := reg.HookedRegsWOVar (v,src);
        IF r IN hookedRegsV THEN
<* IF ~ nodebug THEN *>
          opIO.print("!!! ");
          reg.PrintReg(r);
          reg.PrintRegSet(hookedRegsV," IN hookedregs ");
<* END *>
--          ASSERT(FALSE); ?????
-- todo verbose setloc
--      verbose IN hookedregs - s kem?
        END;
    END;
    IF (p^.tr^.Op = ir.o_val) OR (p^.tr^.Op = ir.o_cast) OR (p^.tr^.Op = ir.o_assign) THEN
        IF r_sz = o_sz THEN
            MRCToReg (r, l, (*r_sz,*) TRUE);
        ELSIF r_sz < o_sz THEN
            MRCToRegTrunc (r, l (*r_sz,*));
        ELSIF (p^.tr^.ResType = ir.t_int) & (p^.tr^.OpType = ir.t_int) THEN
            MRCToRegSX (r, l, (*r_sz*) o_sz);
        ELSE
            MRCToRegZX (r, l, (*r_sz*) o_sz);
        END;
    ELSIF (p^.tr^.Op = ir.o_hiword) THEN
        MRCHiToReg (r, l);
    ELSE
        MRCToReg (r, l, (*r_sz,*) TRUE);
        Emit.GenUnR (p^.tr, r);
    END;
    IF RD.Loc^[v].tag = NTreg THEN
        r := RD.Loc^[v].reg;
    END;
    reg.VarInReg (v, r);
    p^.place.r := r;
    p^.place.v := p^.tr^.Name;
    p^.nt := NTreg;
END UnOp;

--------------------------------------------------------------------------------

PROCEDURE MemCast* (p, l: DAGNODE);
VAR
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.MemCast\n");
<* END *>
    reg.CheckSpilledNode (l);
    p^.a := l^.a;
    IF (p^.tr^.Op = ir.o_hiword) THEN
       ASSERT(p^.tr^.OpSize=8);
       ASSERT(p^.tr^.ResSize=4);
       INC(p.a.offs, 4);
    END;
    p^.nt := NTmem;
END MemCast;
--------------------------------------------------------------------------------
(*
  Сгенерировать a = cond ? b : c, где b и c = 0, 1
*)

PROCEDURE CondSetR* (p: DAGNODE; j: D.Condition);
VAR v: VarNum;
    r: Reg;
    q: TriadePtr;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.CondSetR\n");
<* END *>
    q := p^.tr;
    v := q^.Name;
    IF (RD.Loc^[v].tag = NTreg) & (RD.Loc^[v].reg <> UNDEF_REG) THEN
        r := RD.Loc^[v].reg;
    ELSIF (ir.Vars^[v].Use <> NIL) &
          (ir.Vars^[v].Use^.triade^.Op = ir.o_retfun)
    THEN
        r := reg.GetPreferredReg (D.RegTable[D.EAXp,q^.ResSize], v, FALSE);
    ELSE
        r := reg.gr (v, D.allIRegs);
    END;
    IF Emit.work = Emit.full THEN
        IF Calc.IsZero (q^.Params^[2].value, q^.ResType, q^.ResSize) THEN
            j := Emit.InverseCond (j);
        END;
        IF r IN D.XREGS THEN
            Emit.GenSetC_R (r, j);
            IF q^.ResSize <> 1 THEN
                Emit.GenAndR_INum (D.ChSize(r,4), Calc.GetInteger(1,4));
            END;
        ELSE
            Emit.GenMoveR_INum (r, Calc.GetInteger(1,D.RegInfo[r].sz));
            Emit.work.DisableOptimizations;
            Emit.work.GenJ (j, 2, FALSE);
            Emit.GenSubR_R (D.ChSize(r,4), D.ChSize(r,4));
            Emit.work.EnableOptimizations;
        END;
    END;
    reg.VarInReg (v, r);
    p^.place.r := r;
    p^.place.v := v;
    p^.nt := NTreg;
END CondSetR;

--------------------------------------------------------------------------------

PROCEDURE CondSetM* (p: DAGNODE; VAR a: AddrMode; j: D.Condition);
VAR q: TriadePtr;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.CondSetM\n");
<* END *>
    reg.CheckSpilledReg (a.place1);
    reg.CheckSpilledReg (a.place2);
    GetAddrMode (a, opTune.addr_sz);
    reg.MarkUseAddrMode (a);
    q := p^.tr;
    IF Calc.IsZero (q^.Params^[2].value, q^.ResType, q^.ResSize) THEN
        j := Emit.InverseCond (j);
    END;
    Emit.work.GenSetC_M (a, j);
END CondSetM;

--------------------------------------------------------------------------------

(*
  reg/local = alloca (mrc)
*)

PROCEDURE Alloca* (p, l: DAGNODE);
VAR v:    VarNum;
    r, t: Reg;
    a:    AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.Alloca\n");
<* END *>
    reg.CheckSpilledNode (l);
    v := p^.tr^.Name;
    IF l^.nt = NTreg THEN
        reg.MarkUseReg (l^.place);
    ELSIF l^.nt = NTmem THEN
        GetAddrMode  (l^.a, opTune.addr_sz);
        reg.MarkUseAddrMode (l^.a);
    END;
    r := reg.GetPreferredReg (D.EDI, v, TRUE);
    IF RD.Loc^[v].tag = NTreg THEN
        r := RD.Loc^[v].reg;
    END;
    IF Emit.GuardPage THEN
        Emit.GenPush_R (D.EAX);
        t := reg.GTR (D.allowedIRegs[4]);
        IF Emit.work = Emit.full THEN
            CASE l^.nt OF
            | NTconst:     Emit.GenMoveR_INum (t, l^.par.value);
            | NTreg:       IF l^.place.r <> UNDEF_REG THEN
                                Emit.GenMoveR_R (t, l^.place.r);
                            ELSE
                                FormMem (a, l^.place.v);
                                Emit.GenMoveR_M (t, a);
                            END;
            | NTmem:       Emit.GenMoveR_M (t, l^.a);
            END;
            Emit.work.DisableOptimizations;
            Emit.GenCmpR_INum (t, Calc.GetInteger(4096,4));
            Emit.work.GenJ    (D.JB, 15 - ORD (t = D.EAX), FALSE);
            Emit.GenSubR_INum (D.ESP, Calc.GetInteger(4092,4));
            Emit.GenSubR_INum (t,        Calc.GetInteger(4096,4));
            Emit.GenPush_R    (D.EAX);
            Emit.work.GenJ    (D.UnJ, -23 + 2 * ORD (t = D.EAX), FALSE);
            Emit.work.EnableOptimizations;
            Emit.GenSubR_R    (D.ESP, t);
        END;
        reg.WipeReg(t);
    ELSE
        CASE l^.nt OF
        | NTconst:     Emit.GenSubR_INum (D.ESP, l^.par.value);
        | NTreg:       IF l^.place.r <> UNDEF_REG THEN
                            Emit.GenSubR_R (D.ESP, l^.place.r);
                        ELSE
                            FormMem (a, l^.place.v);
                            Emit.GenSubR_M (D.ESP, a);
                        END;
        | NTmem:       Emit.GenSubR_M (D.ESP, l^.a);
        END;
    END;
    Emit.GenAndR_INum (D.ESP, Calc.GetInteger(-4,4));
    IF RD.Loc^[v].tag = NTlocal THEN
        FormMem (a, v);
        Emit.GenMoveM_R (a, D.ESP);
        p^.nt := NTlocal;
    ELSE
        Emit.GenMoveR_R (r, D.ESP);
        reg.VarInReg (v, r);
        p^.place.r := r;
        p^.place.v := p^.tr^.Name;
        p^.nt := NTreg;
    END;
END Alloca;

--------------------------------------------------------------------------------

(*
  Сгенерировать код для бинарной операции mem = mem op rc
  (op = ADD, SUB, LOGICAL)
*)

PROCEDURE BinOpM* (l, g: DAGNODE; p: TriadePtr);
VAR r: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.BinOpM\n");
<* END *>
    GetAddrMode (l^.a, opTune.addr_sz);
    reg.CheckSpilledNode (l);
    reg.CheckSpilledNode (g);
    CASE g^.nt OF
    | NTconst:
        CASE g^.par^.tag OF
        | ir.y_NumConst,
          ir.y_RealConst:
            Emit.GenOpM_INum (p, l^.a, g^.par.value);
        | ir.y_AddrConst,
          ir.y_ProcConst:
            ParToAddr (g^.a, g^.par);
            Emit.GenOpM_Iglobal (p, l^.a, g^.a);
        END;
    | NTreg:
        ToReg (g^.place, D.allowedIRegs [p^.ResSize], FALSE);
        IF g^.place.r IN D.allowedIRegs [p^.ResSize] THEN
            r := g^.place.r;
        ELSE
            r := reg.GTR (D.allowedIRegs [p^.ResSize]);
            reg.WipeReg(r);
            Emit.GenMoveR_R (r, g^.place.r);
        END;
        Emit.GenOpM_R (p, l^.a, r);
        reg.MarkUseReg (g^.place);
    END;
    reg.MarkUseAddrMode (l^.a);
END BinOpM;

--------------------------------------------------------------------------------

(*
  Сгенерировать код для унарной операции mem = op mem
  (op = INC, DEC, NEG, NOT (XOR 1))
*)

PROCEDURE UnOpM* (a: DAGNODE; p: TriadePtr);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.UnOpM\n");
<* END *>
    reg.CheckSpilledNode (a);
    GetAddrMode  (a^.a, opTune.addr_sz);
    reg.MarkUseAddrMode (a^.a);
    Emit.GenUnM  (p, a^.a);
END UnOpM;

--------------------------------------------------------------------------------

(*
  Сгенерировать сдвиг регистра (не ECX) на неконстанту
  (ECX гарантированно свободен); может произойти spill
*)

PROCEDURE ShiftByNotConst (p: TriadePtr; r: Reg; l, g: DAGNODE;
                           sz: SizeType);
VAR gvar: VarNum;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.ShiftByNotConst\n");
<* END *>
    reg.MarkUsedSoFar( D.ECX);
(*
    IF g^.nt = NTreg THEN
        MoveRegReg (D.RegTable[D.ECXp, ir.Vars[g.place.v].Def.ResSize], g);
        reg.MarkUseReg (g^.place);
    ELSE
        GetAddrMode  (g^.a, opTune.addr_sz);
        reg.MarkUseAddrMode (g^.a);
        Emit.GenMoveR_M (D.CL, g^.a);
    END;
    Emit.GenShiftR_R (p, r);
    reg.WipeReg(D.ECX);
*)
    gvar := reg.regContents [g^.place.r];
    IF g^.nt = NTreg THEN
        Emit.GenPush_R(g^.place.r);  --FIXME
        reg.MarkUseReg (g^.place);
    ELSE
        GetAddrMode  (g^.a, opTune.addr_sz);
        reg.MarkUseAddrMode (g^.a);
        Emit.work.GenPush_M(g^.a,4);
    END;
    INC  (Emit.PushSize, 4);
    MoveRegReg (r, l);
    reg.MarkUseReg (l^.place);

    Emit.GenPop_R(D.ECX);
    DEC  (Emit.PushSize, 4);
-----
    Emit.GenShiftR_R (p, r);
    reg.WipeReg(D.ECX);
END ShiftByNotConst;

--------------------------------------------------------------------------------

(*
  Выделить регистры для операции reg = reg shift mrc
*)

PROCEDURE ShiftOp* (p, l, g: DAGNODE);
VAR t, r: Reg;
    v:    VarNum;
    sz, ECXsz:   SizeType;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.ShiftOp\n");
<* END *>
    reg.CheckSpilledReg  (l^.place);
    reg.CheckSpilledNode (g);
    v  := p^.tr^.Name;
    sz := p^.tr^.ResSize;
(*
    IF sz = 8 THEN
      sz := 4;
    END;
*)
(*
  Получить регистр под результат
*)
    IF (RD.Loc^[v].tag = NTreg) & (RD.Loc^[v].reg <> UNDEF_REG) THEN
        r := RD.Loc^[v].reg;
        IF r IN reg.freeRegs THEN
          reg.VarInReg (v, r);
        END;
    ELSIF (l^.place.r <> UNDEF_REG) &
          ((l^.place.r <> D.ECX) OR (g^.nt = NTconst)) &
          (reg.usagesLeft [l^.place.r] = 1 + UsedInNode (g, l^.place.r, FALSE)) &
          (l^.place.r IN D.allowedIRegs [sz]) &
          ((RD.Loc^[v].tag <> NTreg) OR NOT (l^.place.r IN reg.HookedRegs (v)))
    THEN
        r := l^.place.r;
        reg.SetLoc (v, r);
    ELSE
        r := reg.gr (v, D.allIRegs);
        reg.CheckSpilledReg  (l^.place);
        reg.CheckSpilledNode (g);
    END;
(*
  Получили регистр под результат, начали действовать
*)
    IF (g^.nt = NTconst) THEN
        MoveRegReg (r, l);
        reg.MarkUseReg (l^.place);
        Emit.GenShiftR_INum (p^.tr, r, g^.par.value);
    ELSIF (D.RegInfo[r].code <> D.ECXp) & (UsedInNode (g, r, TRUE) = 0) &
          ((D.ECX IN reg.freeRegs) OR
            (reg.usagesLeft [reg.BusyRegIntersectWith(D.ECX)] =
                     UsedInNode (g, D.ECX, TRUE)
                   + UsedInNode (l, D.ECX, TRUE)))
    THEN
--        MoveRegReg (r, l);
--        reg.MarkUseReg (l^.place);
        ShiftByNotConst (p^.tr, r, l, g, sz);
    ELSE
        reg.OccupyReg(r);
        IF RD.Loc^[v].tag = NTreg THEN
            r := RD.Loc^[v].reg;
        END;
        IF r IN RegSet{D.ECX, D.CX, D.CL} THEN
--            MoveRegReg (t, l);
--            reg.MarkUseReg (l^.place);
            t := reg.GTR (D.allowedIRegs[ir.Vars[v].Def.ResSize]-D.RegAndIntersectWith[D.ECX]);
            ShiftByNotConst (p^.tr, t, l, g, sz);
        ELSE
            IF reg.regContents [D.ECX] # ir.UNDEFINED THEN
              ECXsz := ir.Vars[reg.regContents [D.ECX]].Def.ResSize;
            ELSE
              ECXsz := 4;
            END;
            t := reg.GTR (D.allowedIRegs[ECXsz]-D.RegAndIntersectWith[r]);
            Emit.GenMoveR_R (t, D.RegTable[D.ECXp, ECXsz]);
            IF reg.regContents [D.ECX] # ir.UNDEFINED THEN
              reg.MarkVarInReg(reg.regContents [D.ECX], t);
            END;
--            reg.usagesLeft  [t] := reg.usagesLeft  [D.ECX];
            reg.FillUsagesLeft  (t, reg.usagesLeft  [D.ECX]);
            IF l^.place.r = D.ECX THEN
                l^.place.r := t;
            END;
            IF g^.nt = NTreg THEN
                IF g^.place.r = D.ECX THEN
                    g^.place.r := t;
                END;
            ELSIF g^.nt = NTmem THEN
                IF g^.a.place1.r = D.ECX THEN
                    g^.a.place1.r := t;
                END;
                IF g^.a.place2.r = D.ECX THEN
                    g^.a.place2.r := t;
                END;
            END;
            IF (g^.nt = NTreg) & (r = g^.place.r) THEN
                reg.MarkUsedSoFar( D.ECX);
                MoveRegReg (D.RegTable[D.ECXp,sz], g);
                MoveRegReg (r, l);
                reg.MarkUseReg (l^.place);
                Emit.GenShiftR_R (p^.tr, r);
            ELSE
--                MoveRegReg (r, l);
--                reg.MarkUseReg (l^.place);
                ShiftByNotConst (p^.tr, r, l, g, sz);
            END;
            IF reg.regContents [t]# ir.UNDEFINED THEN
              reg.MarkVarInReg( reg.regContents [t], D.RegTable[D.ECXp, ECXsz] );
              reg.FillUsagesLeft(D.RegTable[D.ECXp, ECXsz], reg.usagesLeft[t]);
              reg.OccupyReg(D.RegTable[D.ECXp, ECXsz]);
            END;
            IF reg.usagesLeft [D.ECX] = 0 THEN
                reg.FreeReg(D.ECX);
            END;
            IF reg.usagesLeft [D.RegTable[D.ECXp, ECXsz]] = 0 THEN
                reg.FreeReg(D.RegTable[D.ECXp, ECXsz]);
            END;
        END;
        Emit.GenMoveR_R (D.RegTable[D.ECXp, D.RegInfo[t].sz], t);
        reg.WipeReg(t);
    END;
    IF RD.Loc^[v].tag = NTreg THEN
        r := RD.Loc^[v].reg;
    END;
    reg.VarInReg (v, r);
    p^.place.r := r;
    p^.place.v := p^.tr^.Name;
    p^.nt := NTreg;
END ShiftOp;

--------------------------------------------------------------------------------

(*
  Выделить регистры для операции mem = mem shift c
*)

PROCEDURE ShiftOpM* (p, l, g: DAGNODE);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.ShiftOpM\n");
<* END *>
    reg.CheckSpilledNode (l);
    GetAddrMode  (l^.a, opTune.addr_sz);
    reg.MarkUseAddrMode (l^.a);
    Emit.GenShiftM_INum (p^.tr, l^.a, g^.par.value,
                         p^.tr^.ResSize);
END ShiftOpM;

--------------------------------------------------------------------------------

(*
  Сгенерировать обход перехода
*)

PROCEDURE SkipTrap* (p: DAGNODE; if_int, if_unsign: D.Condition);
VAR trap: D.Condition;
    q:    TriadePtr;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.SkipTrap\n");
<* END *>
    ASSERT(p.sz <=4);
    q := p^.tr;
    IF (q^.ResType = ir.t_int) THEN
        trap := if_int;
    ELSE
        trap := if_unsign;
    END;
    Emit.work.GenSkipTrap (trap, TrapNo (q), q^.Position);
END SkipTrap;

--------------------------------------------------------------------------------

(*
  Освободить регистр, если надо
*)

PROCEDURE qFree (p: DAGNODE);
BEGIN
    reg.CheckSpilledNode (p);
    IF p^.nt = NTreg THEN
        reg.MarkUseReg (p^.place);
    END;
END qFree;

--------------------------------------------------------------------------------

(*
  Спасти регистр, если надо
*)

PROCEDURE qPush (r: Reg; limit: INT);
VAR u: VarNum;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.qPush\n");
<* END *>
    reg.MarkUsedSoFar(r);
    IF NOT (r IN reg.freeRegs) THEN
        u := reg.regContents [r];
        IF (u <> ir.UNDEFINED) &
           (RD.Loc^[reg.regContents[r]].tag = NTreg) &
           NOT RD.Loc^[reg.regContents[r]].temp &
           (Color.CalcRegProfit (u) <= limit)
        THEN
            reg.SetSpilledVar (u, 4);
        ELSE
            Emit.GenPush_R (r);
            INC (Emit.PushSize, 4);
        END;
    END;
END qPush;

--------------------------------------------------------------------------------

(*
  Восстановить регистр, если надо
*)

PROCEDURE qPop (r: Reg);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.qPop\n");
<* END *>
    IF r IN reg.freeRegs THEN
        reg.WipeReg(r);
    ELSE
        Emit.GenPop_R (r);
        DEC  (Emit.PushSize, 4);
    END;
END qPop;

--------------------------------------------------------------------------------

PROCEDURE qGenMove (r: Reg; p: DAGNODE);
VAR a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.qGenMove\n");
<* END *>
    IF p^.nt = NTconst THEN
        ParamPtrToReg (r, p^.par, 4);
    ELSIF p^.place.r = UNDEF_REG THEN
        FormMem (a, p^.place.v);
        Emit.GenMoveR_M (r, a);
    ELSIF p^.place.r <> r THEN
        Emit.GenMoveR_R (r, p^.place.r);
    END;
END qGenMove;

--------------------------------------------------------------------------------


TYPE
      RegArrayIndex = [0..8];
      RegArray   = ARRAY RegArrayIndex OF Reg;
      NODE_ARRAY = ARRAY RegArrayIndex OF DAGNODE;

CONST Waiting   = 0;
      Trivial   = 1;
      Processed = 2;
      Generated = 3;

(*
  Содрано из Color с заменой vs -> rs
*)

PROCEDURE qMoves (VAR rs: RegArray; ps: NODE_ARRAY; N: INT);
VAR ts: ARRAY RegArrayIndex OF INT;
    ds: ARRAY RegArrayIndex OF INT;
    i:  INT;
    b:  BOOLEAN;

    PROCEDURE GenLoop (VAR p: RegArray; n: INT);
    VAR i: INT;
    BEGIN
        FOR i:=1 TO n-1 DO
            Emit.GenXchgR_R (p[0], p[i]);
        END;
    END GenLoop;

    PROCEDURE ^ Gen (i: INT);

    PROCEDURE FindInPS (r: Reg): INT;
    VAR i: INT;
    BEGIN
        FOR i:=0 TO N DO
            IF (ts [i] <= Processed) & (rs [i] <> r) &
               (ps [i]^.nt = NTreg) & (ps [i]^.place.r = r)
            THEN
                RETURN i;
            END;
        END;
        RETURN -1;
    END FindInPS;

    PROCEDURE GenComplex;
    VAR i: INT;
    BEGIN
        FOR i:=0 TO N DO
            IF ts [i] = Waiting THEN
                Gen (i);
            END;
        END;
    END GenComplex;

    PROCEDURE Gen (i: INT);
    VAR j, k, l: INT;
        lp:      RegArray;
    BEGIN
        IF ts [i] = Processed THEN          -- Цикл:
            k := 0;                         -- Собрать его
            j := i;
            REPEAT
                ts [j] := Generated;
                lp [k] := rs [j];
                INC (k);
                j := ds [j];
            UNTIL j = i;
            j := i;                         -- Сгенерировать тех,
            REPEAT                          -- кто от цикла зависит
                LOOP
                    l := FindInPS (rs [j]);
                    IF l = -1 THEN
                        EXIT;
                    END;
                    Gen (l);
                END;
                j := ds [j];
            UNTIL j = i;
            GenLoop (lp, k);
        ELSE                                -- Еще не знаем, цикл или нет;
                                            -- Сначала пытаемся сгенерировать
                                            -- другие присваивания, которые
                                            -- надо сделать до этого
            ts [i] := Processed;
            LOOP
                j := FindInPS (rs [i]);
                IF j = -1 THEN
                    EXIT;
                END;
                ds [i] := j;
                Gen (j);
                IF ts [i] = Generated THEN  -- Если входила в цикл, то
                                            -- ее уже сгенерировали.
                    RETURN;
                END;
            END;
            qGenMove (rs [i], ps [i]);
            ts [i] := Generated;
        END;
    END Gen;

BEGIN
    DEC (N);
    FOR i:=0 TO N DO
        ts [i] := Waiting;
    END;
    b := FALSE;
    FOR i:=0 TO N DO
        IF FindInPS (rs [i]) = -1 THEN
            ts [i] := Trivial;
        ELSE
            b := TRUE;
        END;
    END;
    IF b THEN
        GenComplex;
    END;
    FOR i:=0 TO N DO
        IF ts [i] = Trivial THEN
            qGenMove (rs [i], ps [i]);
        END;
    END;
END qMoves;

--------------------------------------------------------------------------------

(*
  Сгенерировать массовую пересылку
*)

PROCEDURE GenCopy* (q, f, t, n: DAGNODE);
VAR c: CARD;
    b: BOOLEAN;
    r: RegArray;
    p: NODE_ARRAY;

BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.GenCopy\n");
<* END *>
    qFree (f);
    qFree (t);
    qFree (n);
    IF (n^.nt = NTconst) & (n^.par^.tag = ir.y_NumConst) THEN
        c := Calc.ToCardinal (n^.par^.value, 4);
        IF D.ECX IN reg.freeRegs THEN
            b := (c < 32);
        ELSE
            b := (c < 40);
        END;
    ELSE
        b := FALSE;
    END;
    qPush (D.ESI, ir.Nodes^[q^.tr^.NodeNo].Nesting * 2);
    qPush (D.EDI, ir.Nodes^[q^.tr^.NodeNo].Nesting * 2);
    IF NOT b THEN
        qPush (D.ECX, 0);
    END;
    r [0] := D.ESI;
    r [1] := D.EDI;
    p [0] := f;
    p [1] := t;
    IF (n^.nt <> NTconst) OR (n^.par^.tag <> ir.y_NumConst) THEN
        r [2] := D.ECX;
        p [2] := n;
        qMoves (r, p, 3);
    ELSE
        qMoves (r, p, 2);
    END;
    IF (n^.nt = NTconst) & (n^.par^.tag = ir.y_NumConst) THEN
        IF b THEN
            WHILE c >= 4 DO
                Emit.work.GenMovSD;
                c := c - 4;
            END;
        ELSE
            Emit.GenMoveR_INum (D.ECX, Calc.GetInteger(c DIV 4,4));
            Emit.work.GenRepMovSD;
        END;
        c := c MOD 4;
        WHILE c >= 2 DO
            Emit.work.GenMovSW;
            c := c - 2;
        END;
        WHILE c <> 0 DO
            Emit.work.GenMovSB;
            c := c - 1;
        END;
    ELSE
        IF (n^.nt = NTreg) & (n^.place.r IN RegSet{ D.ECX, D.ESI, D.EDI }) THEN
            Emit.GenPush_R (D.ECX);
            INC  (Emit.PushSize, 4);
        END;
        Emit.GenShrR_INum (D.ECX, Calc.GetInteger(2,4));
        Emit.work.GenRepMovSD;
        IF (n^.nt = NTreg) & (n^.place.r IN RegSet{ D.ECX, D.ESI, D.EDI }) THEN
            Emit.GenPop_R (D.ECX);
            DEC  (Emit.PushSize, 4);
        ELSE
            qGenMove (D.ECX, n);
        END;
        Emit.GenAndR_INum (D.ECX, Calc.GetInteger(3,4));
        Emit.work.GenRepMovSB;
    END;
    IF NOT b THEN
        qPop (D.ECX);
    END;
    qPop (D.EDI);
    qPop (D.ESI);
    IF ir.o_Parameters IN q^.tr^.Options THEN
        IF Emit.work = Emit.full THEN
            CODE_START := RD.NodeInfo^[ir.ZERONode].sg.code_len;
        END;
    END;
END GenCopy;

--------------------------------------------------------------------------------
-- если copy рамера, кратного 4
PROCEDURE GenCopy4* (q, f, t, n: DAGNODE);
VAR c: CARD;
    b: BOOLEAN;
    r: RegArray;
    p: NODE_ARRAY;

BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.GenCopy4\n");
<* END *>
    qFree (f);
    qFree (t);
    qFree (n);
    IF (n^.nt = NTconst) & (n^.par^.tag = ir.y_NumConst) THEN
        c := Calc.ToCardinal (n^.par^.value, 4) * 4;
        IF D.ECX IN reg.freeRegs THEN
            b := (c < 32);
        ELSE
            b := (c < 40);
        END;
    ELSE
        b := FALSE;
    END;
    qPush (D.ESI, ir.Nodes^[q^.tr^.NodeNo].Nesting * 2);
    qPush (D.EDI, ir.Nodes^[q^.tr^.NodeNo].Nesting * 2);
    IF NOT b THEN
        qPush (D.ECX, 0);
    END;
    r [0] := D.ESI;
    r [1] := D.EDI;
    p [0] := f;
    p [1] := t;
    IF (n^.nt <> NTconst) OR (n^.par^.tag <> ir.y_NumConst) THEN
        r [2] := D.ECX;
        p [2] := n;
        qMoves (r, p, 3);
    ELSE
        qMoves (r, p, 2);
    END;
    IF (n^.nt = NTconst) & (n^.par^.tag = ir.y_NumConst) THEN
        IF b THEN
            WHILE c >= 4 DO
                Emit.work.GenMovSD;
                c := c - 4;
            END;
        ELSE
            Emit.GenMoveR_INum (D.ECX, Calc.GetInteger(c DIV 4,4));
            Emit.work.GenRepMovSD;
        END;
        c := c MOD 4;
        WHILE c >= 2 DO
            Emit.work.GenMovSW;
            c := c - 2;
        END;
        WHILE c <> 0 DO
            Emit.work.GenMovSB;
            c := c - 1;
        END;
    ELSE
        Emit.work.GenRepMovSD;
    END;
    IF NOT b THEN
        qPop (D.ECX);
    END;
    qPop (D.EDI);
    qPop (D.ESI);
    IF ir.o_Parameters IN q^.tr^.Options THEN
        IF Emit.work = Emit.full THEN
            CODE_START := RD.NodeInfo^[ir.ZERONode].sg.code_len;
        END;
    END;
END GenCopy4;

--------------------------------------------------------------------------------

(*
  reg/local = rc * mrc
*)

PROCEDURE Mul* (p, p1, p2: DAGNODE; res: Reg);
VAR sz: SizeType;
    r:  Reg;
    v:  VarNum;
    a:  AddrMode;

    PROCEDURE GenMul (p: DAGNODE; t: TriadePtr);
    BEGIN
        CASE p^.nt OF
        | NTreg:   IF p^.place.r = UNDEF_REG THEN
                        FormMem (a, p^.place.v);
                        Emit.GenMul_M (t, a);
                    ELSE
                        Emit.GenMul_R (t, p.place.r);
                    END;
        | NTmem:   Emit.GenMul_M    (t, p^.a);
        | NTconst: Emit.GenMul_INum (t, p^.par.value);
        END;
    END GenMul;

BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.Mul\n");
<* END *>
    reg.CheckSpilledNode (p1);
    reg.CheckSpilledNode (p2);
    sz := p^.tr^.ResSize;
    v  := p^.tr^.Name;
    IF (sz = 1) & (res = D.DL) THEN
        res := D.AH;
    END;
    IF p2^.nt = NTreg THEN
        reg.MarkUseReg (p2^.place);
    ELSIF p2^.nt = NTmem THEN
        GetAddrMode  (p2^.a, opTune.addr_sz);
        reg.MarkUseAddrMode (p2^.a);
    END;
    IF p1^.nt = NTreg THEN
        reg.MarkUseReg (p1^.place);
    END;
    r := reg.GetPreferredReg (res, v, TRUE);
    IF RD.Loc^[v].tag = NTreg THEN
        r := RD.Loc^[v].reg;
    END;
    IF D.RegInfo[r].code <> D.EAXp THEN
        qPush (D.EAX, 0);
    END;
    IF (D.RegInfo[r].code <> D.EDXp) & (sz <> 1) THEN
        qPush (D.EDX, 0);
    END;
    IF (p1^.nt <> NTreg) OR (D.RegInfo[p1^.place.r].code <> D.EAXp) THEN
        MRCToRegTX (D.EAX, p2, (*sz,*) FALSE);
        GenMul (p1, p^.tr);
    ELSE
        GenMul (p2, p^.tr);
    END;
    IF D.EAX IN reg.freeRegs THEN
      reg.WipeReg(D.EAX);
    END;
    IF (sz # 1) & (D.EDX IN reg.freeRegs) THEN
      reg.WipeReg(D.EDX);
    END;
    IF ir.o_Checked IN p^.tr^.Options THEN
        Emit.GenInto (p^.tr^.Position);
    END;
    IF RD.Loc^[v].tag = NTlocal THEN
        FormMem (a, v);
        Emit.GenMoveM_R (a, res);
    ELSIF r <> res THEN
        IF sz = 1 THEN
            Emit.GenMoveR_R (r, res);
        ELSE
            Emit.GenMoveR_R (D.ChSize(r, D.RegInfo[res].sz), res);
        END;
    END;
    IF (D.RegInfo[r].code <> D.EDXp) & (sz <> 1) THEN
        qPop (D.EDX);
    END;
    IF D.RegInfo[r].code <> D.EAXp THEN
        qPop (D.EAX);
    END;
    IF RD.Loc^[v].tag = NTlocal THEN
        p^.nt := NTlocal;
    ELSE
        reg.VarInReg (v, r);
        p^.place.r := r;
        p^.place.v := p^.tr^.Name;
        p^.nt := NTreg;
    END;
END Mul;

--------------------------------------------------------------------------------

(*
  reg/local = rc / mrc
*)

PROCEDURE Div* (p, p1, p2: DAGNODE);
VAR sz:    SizeType;
    r, g:  Reg;
    v:     VarNum;
    a, a1: AddrMode;
    b:     BOOLEAN;

    PROCEDURE GenDiv (p: DAGNODE; t: TriadePtr);
    BEGIN
        CASE p^.nt OF
        | NTreg:   IF p^.place.r = UNDEF_REG THEN
                        FormMem (a, p^.place.v);
                        Emit.GenDiv_M (t, a);
                    ELSE
                        Emit.GenDiv_R (t, p.place.r);
                    END;
        | NTmem:   Emit.GenDiv_M    (t, p^.a);
        | NTconst: Emit.GenDiv_INum (t, p^.par.value);
        END;
    END GenDiv;

    PROCEDURE SexImm (n: LONGINT);
    BEGIN
        IF sz = 1 THEN
            Emit.GenMoveR_INum (D.AH, Calc.GetInteger(n,1));
        ELSIF sz = 2 THEN
            Emit.GenMoveR_INum (D.DX, Calc.GetInteger(n,2));
        ELSE
            Emit.GenMoveR_INum (D.EDX, Calc.GetInteger(n,4));
        END;
    END SexImm;

BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.Div\n");
<* END *>
    reg.CheckSpilledNode (p2);
    sz := p^.tr^.ResSize;
    ASSERT(sz <=4);
    v  := p^.tr^.Name;
    b  := FALSE;
    IF p2^.nt = NTreg THEN
        b := (UsedInNode (p2, D.EAX, TRUE) <> 0) OR
             (sz <> 1) & (UsedInNode (p2, D.EDX, TRUE) <> 0);
        reg.MarkUseReg (p2^.place);
    ELSIF p2^.nt = NTmem THEN
        GetAddrMode  (p2^.a, opTune.addr_sz);
        b := (UsedInNode (p2, D.EAX, TRUE) <> 0) OR
             (sz <> 1) & (UsedInNode (p2, D.EDX, TRUE) <> 0);
        reg.MarkUseAddrMode (p2^.a);
    END;
    IF p1^.nt = NTreg THEN
        reg.MarkUseReg (p1^.place);
    END;
    IF (p^.op = ir.o_div) OR (p^.op = ir.o_dvd) THEN
        g := D.RegTable[D.EAXp, sz];
    ELSIF sz = 1 THEN
        g := D.AH;
    ELSE
        g := D.RegTable[D.EDXp, sz];
    END;
    r := reg.GetPreferredReg (g, v, TRUE);
    IF RD.Loc^[v].tag = NTreg THEN
        r := RD.Loc^[v].reg;
    END;
    IF D.RegInfo[r].code <> D.EAXp THEN
        qPush (D.EAX, 0);
    END;
    IF (D.RegInfo[r].code <> D.EDXp) & (sz <> 1) THEN
        qPush (D.EDX, 0);
    END;
    IF b THEN
        CASE p2^.nt OF
        | NTreg:   Emit.GenPush_R (p2^.place.r);
        | NTmem:   Emit.work.GenPush_M (p2^.a,4);
        END;
        INC (Emit.PushSize, 4);
        p2^.nt := NTmem;
        FormStackOffset (p2^.a, 0);
        p2^.a.place1.r := D.ESP;
        p2^.a.offs := 0;
    END;
    MRCToRegTX (D.EAX, p1, (*4,*) FALSE);
    IF p^.tr^.ResType = ir.t_int THEN
        IF (p1^.nt = NTconst) & (p1^.par.tag = ir.y_NumConst) THEN
            IF Calc.IsNegative (p1^.par^.value, sz) THEN
                SexImm (-1);
            ELSE
                SexImm (0);
            END;
        ELSE
            IF sz = 1 THEN
                Emit.work.GenCBW;
            ELSIF NOT (at.SPACE IN at.COMP_MODE) &
                  (at.CPU <> at.i386) & (at.CPU <> at.iPentiumPro) &
                  (p1^.nt = NTreg)
            THEN
                IF p1^.place.r = UNDEF_REG THEN
                    FormMem (a1, p1^.place.v);
                    Emit.GenMoveR_M (D.EDX, a1);
                ELSIF D.RegInfo[p1^.place.r].code <> D.EDXp THEN
                    Emit.GenMoveR_R (D.EDX, D.ChSize(p1^.place.r, 4));
                END;
                Emit.work.GenShiftR_INum (D.TTT_sar, D.EDXp, sz * 8 - 1, sz);
            ELSE
                Emit.work.GenCDQ (sz);
            END;
        END;
    ELSE
        SexImm (0);
    END;
    GenDiv (p2, p^.tr);
    IF D.EAX IN reg.freeRegs THEN
      reg.WipeReg(D.EAX);
    END;
    IF (sz # 1) & (D.EDX IN reg.freeRegs) THEN
      reg.WipeReg(D.EDX);
    END;
    IF RD.Loc^[v].tag = NTlocal THEN
        FormMem (a, v);
        Emit.GenMoveM_R (a, g);
    ELSIF r <> g THEN
        IF sz = 1 THEN
            Emit.GenMoveR_R (r, g);
        ELSE
            Emit.GenMoveR_R (D.ChSize(r,4), D.ChSize(g,4));
        END;
    END;
    IF b & (Emit.PushSize <> 4) THEN
        Emit.GenAddR_INum (D.ESP, Calc.GetInteger(4,4));
        DEC (Emit.PushSize, 4);
    END;
    IF (D.RegInfo[r].code <> D.EDXp) & (sz <> 1) THEN
        qPop (D.EDX);
    END;
    IF D.RegInfo[r].code <> D.EAXp THEN
        qPop (D.EAX);
    END;
    IF RD.Loc^[v].tag = NTlocal THEN
        p^.nt := NTlocal;
    ELSE
        reg.VarInReg (v, r);
        p^.place.r := r;
        p^.place.v := p^.tr^.Name;
        p^.nt := NTreg;
    END;
END Div;

--------------------------------------------------------------------------------

PROCEDURE DoCase* (p, l: DAGNODE);
VAR sz: SizeType;
    lo: LONGINT;
    ca: Emit.LABEL;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.DoCase\n");
<* END *>
    sz := p^.tr^.OpSize;
    reg.CheckSpilledReg (l^.place);
    ToReg      (l^.place, D.allowedIRegs [sz], FALSE);
    reg.MarkUseReg (l^.place);
    IF p^.tr^.OpType = ir.t_int THEN
        lo := Calc.ToInteger (p^.tr^.Params^[1].value, sz);
    ELSE
        lo := SYSTEM.VAL (LONGINT,
                          Calc.ToCardinal (p^.tr^.Params^[1].value, sz));
    END;
    IF sz <> 4 THEN
        IF (p^.tr^.OpType = ir.t_int) & (lo < 0) THEN
            Emit.GenMovesxR_R (D.ChSize(l^.place.r, 4), l^.place.r);
        ELSE
            Emit.GenMovezxR_R (D.ChSize(l^.place.r, 4), l^.place.r);
        END;
    END;
    IF lo <> MIN (LONGINT) THEN
        lo := -lo;
    END;
    IF at.GENASM IN at.COMP_MODE THEN
        Emit.work.NewLabel (ca);
        RD.NodeInfo^[p^.tr^.NodeNo].ca := ca;
        Emit.GenCase (l^.place.r, lo, ca);
    ELSE
        Emit.GenCase (l^.place.r, lo, Emit.ZEROLABEL);
    END;
END DoCase;

--------------------------------------------------------------------------------

PROCEDURE InclExcl* (p, l, g: DAGNODE);
VAR sz: SizeType;
    v:  VarNum;
    r: Reg;
    q: TriadePtr;
    ttt : D.BinaryOp;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.InclExcl\n");
<* END *>
    reg.CheckSpilledReg (g^.place);
    ToReg (g^.place, D.allowedIRegs[4], FALSE);
    sz := p^.tr^.ResSize;
    IF l^.nt = NTreg THEN
        reg.CheckSpilledReg (l^.place);
        ToReg (l^.place, D.allowedIRegs [sz], FALSE);
        reg.MarkUseReg (l^.place);
        v := p^.tr^.Name;
        r := reg.GetPreferredReg (l^.place.r, v, FALSE);
        IF RD.Loc^[v].tag = NTreg THEN
            r := RD.Loc^[v].reg;
        END;
        IF xiEnv.config.Option("o_incl_bts") THEN
            IF p.tr.Op = ir.o_incl THEN
                ttt := D.TTT_bts;
            ELSE
                ttt := D.TTT_btr;
            END;
            Emit.work.GenOpR_R(ttt, D.RegInfo[l^.place.r].code,
                                    D.RegInfo[g^.place.r].code, 4);
            IF r <> l^.place.r THEN
                Emit.work.GenMoveR_R (D.RegInfo[r].code, D.RegInfo[l^.place.r].code, 4);
            END;
        ELSE
            Emit.GenInclExcl (p^.tr, r, l^.place.r, g^.place.r);
        END;
        reg.MarkUseReg (g^.place);
        reg.VarInReg (v, r);
        p^.place.r := r;
        p^.place.v := v;
        p^.nt := NTreg;
    ELSE
        reg.CheckSpilledNode (l);
        GetAddrMode (l^.a, opTune.addr_sz);
        reg.MarkUseReg (g^.place);
--        r := reg.GTR (D.allowedIRegs [sz]);
        reg.MarkUseAddrMode (l^.a);
        IF p^.op = ir.o_storer THEN
            q := p^.r^.tr;
        ELSE
            p^.nt := NTlocal;
            q := p^.tr;
        END;
        Emit.GenInclExclMem (q, l^.a, g^.place.r);
--        reg.WipeReg(r);
    END;
END InclExcl;

--------------------------------------------------------------------------------

PROCEDURE MoveInclExcl* (p, g: DAGNODE);
VAR sz: SizeType;
    v:  VarNum;
    r: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.MoveInclExcl\n");
<* END *>
    sz := p^.tr^.ResSize;
    v  := p^.tr^.Name;
    ToReg (g^.place, D.allowedIRegs[4], FALSE);
    reg.MarkUseReg (g^.place);
    r := reg.gr (v, D.allIRegs);
    IF RD.Loc^[v].tag = NTreg THEN
        r := RD.Loc^[v].reg;
    END;
    Emit.GenMoveInclExcl (p^.tr, r, g^.place.r);
    reg.VarInReg (v, r);
    p^.place.r := r;
    p^.place.v := v;
    p^.nt := NTreg;
END MoveInclExcl;

--------------------------------------------------------------------------------

PROCEDURE LoHiSet* (p, l: DAGNODE);
VAR sz: SizeType;
    v:  VarNum;
    r: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.LoHiSet\n");
<* END *>
    sz := p^.tr^.ResSize;
    v  := p^.tr^.Name;
    reg.CheckSpilledReg (l^.place);
    ToReg (l^.place, D.allowedIRegs[4], FALSE);
    reg.MarkUseReg (l^.place);
    r := reg.gr (v, D.allIRegs);
    Emit.GenLoHiSet (p^.tr, r, l^.place.r);
    reg.VarInReg (v, r);
    p^.place.r := r;
    p^.place.v := v;
    p^.nt := NTreg;
END LoHiSet;

--------------------------------------------------------------------------------

PROCEDURE InSet* (p, l, g: DAGNODE);
VAR sz: SizeType;
    a:AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.InSet\n");
<* END *>
    sz := p^.tr^.ResSize;
    reg.CheckSpilledReg (l^.place);
    ToReg (l^.place, D.allowedIRegs[4], FALSE);
    IF (sz=8)&(g.nt=NTreg) THEN
        IF g.place.r # UNDEF_REG THEN
          IF TEMP_SIZE < 8 THEN
              TEMP_SIZE := 8;
          END;
          FormStackOffset (a, TEMP_START);
          Emit.GenMoveM_R (a, g.place.r);
        ELSE
          FormMem(a, g.place.v);
        END;
        reg.MarkUseReg (l^.place);
        Emit.GenBTM_R (a, l^.place.r);
        RD.NodeInfo^[p^.tr^.NodeNo].j := D.JC;
    ELSE
      CASE g.nt OF
      | NTreg:
          reg.CheckSpilledReg (g^.place);
          ToReg (g^.place, D.allowedIRegs [sz], FALSE);
          reg.MarkUseReg (l^.place);
          reg.MarkUseReg (g^.place);
          IF xiEnv.config.Option("o_in_bt") THEN
              Emit.GenBTR_R (g^.place.r, l^.place.r);
              RD.NodeInfo^[p^.tr^.NodeNo].j := D.JC;
          ELSE
              Emit.GenInSet (l^.place.r, g^.place.r);
              RD.NodeInfo^[p^.tr^.NodeNo].j := D.JNE;
          END;
      | NTmem,NTaddr:
          GetAddrMode  (g^.a, opTune.addr_sz);
          reg.MarkUseReg (l^.place);
          reg.MarkUseAddrMode (g^.a);
          Emit.GenBTM_R (g^.a, l^.place.r);
          RD.NodeInfo^[p^.tr^.NodeNo].j := D.JC;
      END;
    END;
END InSet;

PROCEDURE InSetConst* (p, l, g: DAGNODE); -- g - const l - reg
VAR sz: SizeType;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.InSetConst\n");
<* END *>
    ASSERT(l.nt=NTreg);
    sz := p^.tr^.ResSize;
    reg.MarkUseReg (l^.place);
    Emit.GenBTINum_R (g.par.value, sz, l^.place.r);
    RD.NodeInfo^[p^.tr^.NodeNo].j := D.JC;
END InSetConst;

--------------------------------------------------------------------------------
--
--  Вещественная арифметика
--
--------------------------------------------------------------------------------

PROCEDURE MemToTOS* (p, l: DAGNODE; sz: SizeType);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.MemToTOS\n");
<* END *>
    reg.CheckSpilledNode (l);
    GetAddrMode  (l^.a, opTune.addr_sz);
    reg.MarkUseAddrMode (l^.a);
    Emit.work.GenMoveTOS_M (l^.a, sz);
    IncFloats;
    p^.nt := NTtos;
END MemToTOS;

--------------------------------------------------------------------------------

PROCEDURE TOSToLocal* (p: DAGNODE; sz: SizeType);
VAR a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.TOSToLocal\n");
<* END *>
    IF (p^.tr^.Name >= Color.NNonTempVars) &
       (ir.Vars^[p^.tr^.Name].Use = NIL)
    THEN
        Emit.work.GenMoveSTi_TOS (0);
    ELSE
        FormMem (a, p^.tr^.Name);
        Emit.work.GenMoveM_TOS (a, sz);
    END;
    DEC (Emit.FloatSize);
    p^.nt := NTlocal;
END TOSToLocal;

--------------------------------------------------------------------------------

PROCEDURE TOSToMem* (addr: DAGNODE; sz: SizeType);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.TOSToMem\n");
<* END *>
    reg.CheckSpilledNode (addr);
    GetAddrMode  (addr^.a, opTune.addr_sz);
    reg.MarkUseAddrMode (addr^.a);
    Emit.work.GenMoveM_TOS (addr^.a, sz);
    DEC (Emit.FloatSize);
END TOSToMem;

--------------------------------------------------------------------------------

PROCEDURE Mem8ToMem* (addr1, addr2: DAGNODE);
VAR r: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.Mem8ToMem\n");
<* END *>
    reg.CheckSpilledNode (addr1);
    reg.CheckSpilledNode (addr2);
    GetAddrMode (addr2^.a, opTune.addr_sz);
    GetAddrMode (addr1^.a, opTune.addr_sz);
    r := reg.GTR (D.allowedIRegs[4]);
    Emit.GenMoveR_M (r, addr2^.a);
    Emit.GenMoveM_R (addr1^.a, r);
    INC (addr1^.a.offs, 4);
    INC (addr2^.a.offs, 4);
    Emit.GenMoveR_M (r, addr2^.a);
    Emit.GenMoveM_R (addr1^.a, r);
    reg.MarkUseAddrMode (addr1^.a);
    reg.MarkUseAddrMode (addr2^.a);
    reg.WipeReg(r);
END Mem8ToMem;

--------------------------------------------------------------------------------

PROCEDURE FConstToMem* (addr, NTconst: DAGNODE; sz: SizeType);
VAR r: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.FConstToMem\n");
<* END *>
    reg.CheckSpilledNode (addr);
    GetAddrMode  (addr^.a, opTune.addr_sz);
    IF (reg.freeRegs = RegSet{}) OR (at.CPU <= at.i486) THEN
        r := D.UNDEF_REG;
    ELSE
        r := reg.GTR (D.allowedIRegs [4]);
        reg.WipeReg(r);
    END;
    reg.MarkUseAddrMode (addr^.a);
    Emit.GenMoveM_FConst (addr^.a, NTconst^.par^.value, r, sz);
END FConstToMem;

--------------------------------------------------------------------------------

PROCEDURE ImmToTOS* (p: DAGNODE; sz: SizeType);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.ImmToTOS\n");
<* END *>
    Emit.GenMoveTOS_INum (p^.par^.value, sz);
    IncFloats;
    p^.nt := NTtos;
END ImmToTOS;

--------------------------------------------------------------------------------

 PROCEDURE GetParToTOS* (p: DAGNODE);
 VAR a: AddrMode;
 BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.GetParToTOS\n");
<* END *>
    FormStackOffset (a, prc.ParamOffs (p^.tr^.NPar));
    Emit.work.GenMoveTOS_M (a, p^.tr^.ResSize);
    IncFloats;
    p^.nt := NTtos;
    IF Emit.work = Emit.full THEN
        CODE_START := RD.NodeInfo^[ir.ZERONode].sg.code_len;
    END;
 END GetParToTOS;

--------------------------------------------------------------------------------

PROCEDURE SYSTEMVAL_ToTos* (p, l: DAGNODE; sz: SizeType);
VAR a: AddrMode;
    r: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.SYSTEMVAL_ToTOS\n");
<* END *>
    IF (l^.nt = NTreg) & (l^.place.r = UNDEF_REG) THEN
        FormMem (l^.a, l^.place.v);
        l^.nt := NTmem;
    ELSIF l^.nt = NTmem THEN
        GetAddrMode  (l^.a, opTune.addr_sz);
        reg.MarkUseAddrMode (l^.a);
    END;
    IF sz < 4 THEN sz := 4 END;
    IF (l^.nt = NTmem) THEN
        Emit.work.GenMoveTOS_M (l^.a, sz);
    ELSE
        ASSERT (l^.nt = NTreg);
        reg.MarkUseReg (l^.place);
        r := l^.place.r;
        IF TEMP_SIZE < 4 THEN
            TEMP_SIZE := 4;
        END;
        FormStackOffset (a, TEMP_START);
        Emit.GenMoveM_R (a, r);
        Emit.work.GenMoveTOS_M (a, sz);
    END;
    IncFloats;
    p^.nt := NTtos;
END SYSTEMVAL_ToTos;

--------------------------------------------------------------------------------

PROCEDURE SYSTEMVALTosToReg* (p: DAGNODE; sz: SizeType);
VAR v: VarNum;
    r: Reg;
    a: AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.SYSTEMVALTosToReg\n");
<* END *>
    IF sz = 10 THEN
        sz := 12;
    END;
    IF p^.op = ir.o_par THEN
        v := ir.UNDEFINED;
        r := reg.GTR (D.allowedIRegs [p.sz]);
    ELSE
        v := p^.tr^.Name;
        r := reg.gr (v, D.allowedIRegs [p.sz]);
    END;
    IF TEMP_SIZE < ORD(sz) THEN
        TEMP_SIZE := sz;
    END;
    FormStackOffset (a, TEMP_START);
    Emit.work.GenMoveM_TOS (a, sz);
    DEC (Emit.FloatSize);
    Emit.GenMoveR_M (r, a);
    reg.VarInReg (v, r);
    p^.place.v := v;
    p^.place.r := r;
    p^.nt := NTreg;
END SYSTEMVALTosToReg;

--------------------------------------------------------------------------------

(*
  TOS = TOS op ...
*)

PROCEDURE TOS_FBinary* (p, l, g: DAGNODE; gloc: NT;
                        rev: BOOLEAN; sz: SizeType);

    PROCEDURE GetFOp (Op: ir.Operation; rev: BOOLEAN): D.FloatOp;
    BEGIN
        CASE Op OF
        | ir.o_add: RETURN D.FADD;
        | ir.o_mul: RETURN D.FMUL;
        | ir.o_sub: IF rev THEN
                        RETURN D.FSUBR;
                    ELSE
                        RETURN D.FSUB;
                    END;
        | ir.o_dvd: IF rev THEN
                        RETURN D.FDIVR;
                    ELSE
                        RETURN D.FDIV;
                    END;
        END;
    END GetFOp;

VAR op: D.FloatOp;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.TOS_FBinary\n");
<* END *>
    ASSERT (l^.nt = NTtos);
    op := GetFOp (p^.tr^.Op, rev);
    CASE gloc OF
    | NTtos:   Emit.GenFOpFReg_TOS (op, 1);
                DEC (Emit.FloatSize);
    | NTmem:   IF (g^.op = ir.o_par) & (l^.op = ir.o_par) &
                   (g^.par^.tag = ir.y_Variable) &
                   (l^.par^.tag = ir.y_Variable) &
                   (g^.par^.name = l^.par^.name)
                THEN
                    Emit.GenFOpFReg0_Freg (op, 0);
                ELSE
                    GetAddrMode  (g^.a, opTune.addr_sz);
                    reg.MarkUseAddrMode (g^.a);
                    Emit.work.GenFOpST0_M (op, g^.a, sz);
                END;
    | NTimem:  GetAddrMode  (g^.a, opTune.addr_sz);
                reg.MarkUseAddrMode (g^.a);
                Emit.work.GenFOpST0_IM (op, g^.a, sz);
    | NTconst: Emit.GenFOpST0_INum (op, g^.par^.value, sz);
    END;
    p^.nt := NTtos;
END TOS_FBinary;

--------------------------------------------------------------------------------

PROCEDURE FUnary* (p: DAGNODE);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.Funary\n");
<* END *>
    CASE p^.tr^.Op OF
    | ir.o_abs:     Emit.GenFAbs;
    | ir.o_neg:     Emit.GenFNeg;
    | ir.o_sin:     Emit.GenFSin;
    | ir.o_cos:     Emit.GenFCos;
    | ir.o_sqrt:    Emit.GenFSqrt;
    | ir.o_tan:     Emit.GenFTan;
                    IF Emit.FloatSize + 1 > MAX_FLOAT THEN
                        MAX_FLOAT := Emit.FloatSize + 1;
                    END;
    | ir.o_atan:    Emit.GenFAtan;
                    IF Emit.FloatSize + 1 > MAX_FLOAT THEN
                        MAX_FLOAT := Emit.FloatSize + 1;
                    END;
    | ir.o_ln:      Emit.GenFLn;
                    IF Emit.FloatSize + 1 > MAX_FLOAT THEN
                        MAX_FLOAT := Emit.FloatSize + 1;
                    END;
    | ir.o_lg:      Emit.GenFLg;
                    IF Emit.FloatSize + 1 > MAX_FLOAT THEN
                        MAX_FLOAT := Emit.FloatSize + 1;
                    END;
    | ir.o_exp:     Emit.GenFExp;
                    IF Emit.FloatSize + 2 > MAX_FLOAT THEN
                        MAX_FLOAT := Emit.FloatSize + 2;
                    END;
    END;
    p^.nt := NTtos;
END FUnary;

--------------------------------------------------------------------------------

PROCEDURE UnAbsNeg* (addr: DAGNODE; q: TriadePtr);
VAR sz: SizeType;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.UnabsNeg\n");
<* END *>
    GetAddrMode  (addr^.a, opTune.addr_sz);
    reg.MarkUseAddrMode (addr^.a);
    IF q^.ResSize = 12 THEN
        sz := 10;
    ELSE
        sz := q^.ResSize;
    END;
    INC (addr^.a.offs, sz - 1);
    IF q^.Op = ir.o_abs THEN
        Emit.work.GenOpM_INum (D.TTT_and, addr^.a, 127, 1);
    ELSE
        Emit.work.GenOpM_INum (D.TTT_xor, addr^.a, 128, 1);
    END;
END UnAbsNeg;

--------------------------------------------------------------------------------

PROCEDURE UseSAHF* (): BOOLEAN;
BEGIN
    RETURN (at.SPACE IN at.COMP_MODE) OR
           (at.CPU = at.i386) OR (at.CPU = at.iPentiumPro);
END UseSAHF;

--------------------------------------------------------------------------------

PROCEDURE SetFlags (v: SHORTINT);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.SetFlags\n");
<* END *>
    AdjustSP;
    IF NOT (D.EAX IN reg.freeRegs) THEN
        Emit.GenPush_R (D.EAX);
        INC (Emit.PushSize, 4);
    END;
    IF UseSAHF () THEN
        Emit.GenFstswSahf;
    ELSE
        Emit.work.GenFstsw;
        Emit.GenAndR_INum (D.AH, Calc.GetInteger(v,1));
(*
        Emit.GenTestR_INum (Emit.AH, v, 1);
*)
    END;
    IF D.EAX IN reg.freeRegs THEN
        reg.WipeReg(D.EAX);
    ELSE
        Emit.GenPop_R (D.EAX);
        DEC (Emit.PushSize, 4);
    END;
END SetFlags;

--------------------------------------------------------------------------------

PROCEDURE FCompare* (g: DAGNODE; gloc: NT; sz: SizeType;
                     v: SHORTINT);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.FCompare\n");
<* END *>
    CASE gloc OF
    | NTtos:   Emit.work.GenFComTOS_TOS;
                DEC (Emit.FloatSize);
    | NTmem:   GetAddrMode  (g^.a, opTune.addr_sz);
                reg.MarkUseAddrMode (g^.a);
                Emit.work.GenFComTOS_M (g^.a, sz);
    | NTimem:  GetAddrMode  (g^.a, opTune.addr_sz);
                reg.MarkUseAddrMode (g^.a);
                Emit.work.GenFComTOS_IM (g^.a, sz);
    | NTconst: Emit.work.GenFComTOS_INum (g^.par^.value, sz);
    END;
    DEC (Emit.FloatSize);
    SetFlags (v);
END FCompare;

--------------------------------------------------------------------------------

PROCEDURE FCompareTOS_NIL*;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.FCompareTOS_NIL\n");
<* END *>
    Emit.work.GenFOp (D.FLDZ);
    Emit.work.GenFComTOS_STi(1);
    DEC(Emit.FloatSize);
    SetFlags (40H);
END FCompareTOS_NIL;

--------------------------------------------------------------------------------

PROCEDURE FEqZero* (l: DAGNODE; sz: SizeType);
VAR r: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.FeqZero\n");
<* END *>
    AdjustSP;
    reg.CheckSpilledNode (l);
    GetAddrMode  (l^.a, opTune.addr_sz);
    IF sz = 4 THEN
        Emit.work.GenTestM_INum (l^.a, 7FFFFFFFH, 4);
    ELSE
        ASSERT (sz = 8);
        r := reg.GTR (D.allowedIRegs[4]);
        reg.WipeReg(r);
        INC (l^.a.offs, 4);
        Emit.work.GenMoveR_M (D.RegInfo[r].code, l^.a, 4);
        Emit.work.GenOpR_INum (D.TTT_and, D.RegInfo[r].code, 7FFFFFFFH, 4);
        DEC (l^.a.offs, 4);
        Emit.work.GenOpR_M (D.TTT_or, D.RegInfo[r].code, l^.a, 4);
    END;
    reg.MarkUseAddrMode (l^.a);
END FEqZero;

--------------------------------------------------------------------------------

PROCEDURE FMemLeZero* (l: DAGNODE; sz: SizeType);
VAR r: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.FmemLeZero\n");
<* END *>
    AdjustSP;
    reg.CheckSpilledNode (l);
    GetAddrMode  (l^.a, opTune.addr_sz);
    r := reg.GTR (D.allowedIRegs[4]);
    reg.WipeReg(r);
    IF sz = 4 THEN
        Emit.work.GenOpM_INum (D.TTT_cmp, l^.a, 1, 4);
    ELSE
        Emit.work.GenMoveR_M (D.RegInfo[r].code, l^.a, 4);
        INC (l^.a.offs, 4);
        Emit.work.GenOpR_M    (D.TTT_or, D.RegInfo[r].code, l^.a, 4);
        Emit.work.GenOpR_INum (D.TTT_cmp, D.RegInfo[r].code, 1, 4);
    END;
    Emit.work.GenOpR_R (D.TTT_sbb, D.RegInfo[r].code, D.RegInfo[r].code, 4);
    Emit.work.GenOpR_M (D.TTT_or, D.RegInfo[r].code, l^.a, 4);
    reg.MarkUseAddrMode (l^.a);
END FMemLeZero;

--------------------------------------------------------------------------------

PROCEDURE ToOrdinal* (t, l: DAGNODE; tp: TypeType; sz: SizeType);
VAR m: RegSet;
    s: INT;
    v: VarNum;
    a: AddrMode;
    r: Reg;
    b: BOOLEAN;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.ToOrdinal\n");
<* END *>
    b := (l^.nt <> NTmem);
    IF b THEN
        m := RegSet{ D.EAX, D.ECX, D.EDX } - reg.freeRegs;
        UsedByCalled := UsedByCalled + RegSet{ D.EAX, D.ECX, D.EDX };
        s := SaveCallRegs (TRUE, m);
    END;
    IF tp = ir.t_unsign THEN
        CASE sz OF
        | 1:    Emit.work.GenPush_INum (255, 4);
        | 2:    Emit.work.GenPush_INum (65535, 4);
        | 4:    Emit.work.GenPush_INum (SYSTEM.VAL (LONGINT, Calc.MaxCard32), 4);
        END;
        INC (Emit.PushSize, 4);
        Emit.work.GenPush_INum (0, 4);
    ELSE
        CASE sz OF
        | 1:    Emit.work.GenPush_INum ( 127,   4);
                INC (Emit.PushSize, 4);
                Emit.work.GenPush_INum (-128,   4);
        | 2:    Emit.work.GenPush_INum ( 32767, 4);
                INC (Emit.PushSize, 4);
                Emit.work.GenPush_INum (-32768, 4);
        | 4:    Emit.work.GenPush_INum (MAX (LONGINT), 4);
                INC (Emit.PushSize, 4);
                Emit.work.GenPush_INum (MIN (LONGINT), 4);
        END;
    END;
    INC (Emit.PushSize, 4);
    Push (l, 8, TRUE);
    IF NOT b THEN
        m := RegSet{ D.EAX, D.ECX, D.EDX } - reg.freeRegs;
        UsedByCalled := UsedByCalled + RegSet{ D.EAX, D.ECX, D.EDX };
        s := SaveCallRegs (FALSE, m);
    END;
    s := SaveCallFRegs (s, Emit.FloatSize);
    Emit.GenCallToOrdinal (tp = ir.t_unsign, 4);
    Emit.GenAddR_INum (D.ESP, Calc.GetInteger(16,4));
    DEC (Emit.PushSize, 16);
    s := RestCallFRegs (s, Emit.FloatSize, FALSE);
    v := t^.tr^.Name;
    r := UNDEF_REG;
    IF RD.Loc^[v].tag = NTlocal THEN
        FormMem (a, v);
        Emit.GenMoveM_R (a, D.RegTable[D.EAXp,sz]);
        t^.nt := NTlocal;
    ELSE
        SYSTEM.EVAL(reg.GetPreferredReg (D.RegTable[D.EAXp,sz], v, TRUE));
        r := RD.Loc^[v].reg;
        IF r <> D.EAX THEN
            Emit.GenMoveR_R (r, D.RegTable[D.EAXp,sz]);
        END;
        reg.VarInReg (v, r);
        t^.place.r := r;
        t^.place.v := v;
        t^.nt := NTreg;
    END;
    RestCallRegs (b, s, m, RegSet{ D.EAX, D.ECX, D.EDX }, r);
END ToOrdinal;

--------------------------------------------------------------------------------

PROCEDURE ToReal* (p, l: DAGNODE; tp: TypeType; sz: SizeType);
VAR r:   Reg;
    a:   AddrMode;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== r386.ToReal\n");
<* END *>
    IF (l^.nt = NTreg) & (l^.place.r = UNDEF_REG) THEN
        FormMem (l^.a, l^.place.v);
        l^.nt := NTmem;
    ELSIF l^.nt = NTmem THEN
        GetAddrMode  (l^.a, opTune.addr_sz);
        reg.MarkUseAddrMode (l^.a);
    END;
    IF (l^.nt = NTmem) & (tp = ir.t_int) & (sz >= 2) THEN
        Emit.work.GenFILD (l^.a, sz);
    ELSIF (sz = 8) AND (tp = ir.t_unsign) THEN
        IF l^.nt = NTreg THEN
            reg.MarkUseReg (l^.place);
            r := l^.place.r;
        ELSE
            r := reg.GTR (D.allowedIRegs[8]);
            Emit.GenMoveR_M (r, l.a);
        END;
        FormStackOffset (a, TEMP_START);
        Emit.GenMoveM_R (a, r);
        Emit.work.GenOpM_INum (D.TTT_and, a, 7FFFFFFFH, 4);
        Emit.work.GenFILD (a, sz);
        Emit.GenMoveM_R (a, D.HighPart[r]);
        Emit.work.GenOpM_INum (D.TTT_and, a, 80000000H, 4);
        INC(a.offs,4);
        Emit.work.GenOpM_INum (D.TTT_and, a, 00000000H, 4);
        DEC(a.offs,4);
        Emit.work.GenFILD (a, sz);
        Emit.work.GenFOp (D.FCHS);
        Emit.GenFOpFReg_TOS (D.FADD, 1);
        IF l^.nt <> NTreg THEN
            reg.WipeReg(r);
        END;
    ELSE
        IF l^.nt = NTreg THEN
            reg.MarkUseReg (l^.place);
            r := l^.place.r;
            IF (sz = 1) OR (sz =2) THEN
                IF tp = ir.t_unsign THEN
                    Emit.GenMovezxR_R (D.ChSize(r, 4), r);
                ELSE
                    Emit.GenMovesxR_R (D.ChSize(r, 4), r);
                END;
            END;
        ELSE
            r := reg.GTR (D.allowedIRegs[4]);
            IF sz = 4 THEN
                Emit.GenMoveR_M (r, l^.a);
            ELSIF tp = ir.t_unsign THEN
                Emit.GenMovezxR_M (r, l^.a, sz);
            ELSIF sz = 1 THEN
                Emit.GenMovesxR_M (r, l^.a, sz);
            END;
        END;
        IF TEMP_SIZE < 4 THEN
            TEMP_SIZE := 4;
        END;
        IF (sz=8)THEN
            ASSERT(tp = ir.t_int);
            FormStackOffset (a, TEMP_START);
            WasUnsignToReal := TRUE;
            Emit.GenMoveM_R (a, r);
        ELSIF (tp = ir.t_unsign) & (sz = 4) THEN
            FormStackOffset (a, TEMP_START + TEMP_SIZE - 4);
            WasUnsignToReal := TRUE;
            sz := 8;
            Emit.GenMoveM_R (a, D.ChSize(r, 4));
            INC(a.offs,4);
            Emit.GenMoveM_INum (a, Calc.GetInteger(0,4), 4);
            DEC(a.offs,4);
        ELSE
            FormStackOffset (a, TEMP_START);
            sz := 4;
            Emit.GenMoveM_R (a, D.ChSize(r, 4));
        END;
        Emit.work.GenFILD    (a, sz);
        IF l^.nt <> NTreg THEN
            reg.WipeReg(r);
        END;
    END;
    IncFloats;
    p^.nt := NTtos;
END ToReal;

BEGIN
    RD.NodeInfo := NIL;
    MustFillFrame := TRUE;
    NeverUsePushInCall := FALSE;
END r386.
