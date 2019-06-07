MODULE reg386;

IMPORT ir,
       Calc,
       Color,
       at := opAttrs,
       Emit,
       CodeDef,
       pc := pcK,
       BitVect,
       Optimize,
       RD := RDefs,
       nts := BurgNT,
       EXCEPTIONS,

<* IF ~ nodebug THEN *>
       opIO,
<* END *>
       SYSTEM,
       opTune,
       env := xiEnv,
       prc := opProcs;

IMPORT D := desc386;

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
    BitMatrix   = ir.BitMatrix;
    SizeType    = ir.SizeType;
    RegPlacement    = Emit.RegPlacement;
    NT          = nts.NT;
    Reg         = D.Reg;
    RegSet      = D.RegSet;

-- Этот кусок скопирован из выхода BURG - потом вынести в отдельный модуль -----

CONST NTstm=      nts.NTstm;
CONST NTreg=      nts.NTreg;
CONST NTrc=       nts.NTrc;
CONST NTmrc=      nts.NTmrc;
CONST NTmem=      nts.NTmem;
CONST NTbased=    nts.NTbased;
CONST NTscaled=   nts.NTscaled;
CONST NTaddr=     nts.NTaddr;
CONST NTlocal=    nts.NTlocal;
CONST NTtos=      nts.NTtos;
CONST NTconst=    nts.NTconst;
CONST NTimem=     nts.NTimem;

--------------------------------------------------------------------------------

CONST
    UNDEF_REG    *= D.UNDEF_REG;
    MINREG       = D.MINREG;
    MAXREG       = D.MAXREG;
    HUGE         *= 1000000; -- Если регистр жив в конце линейного участка,
                            -- то столько мы пишем в Counters [r]

TYPE
    DAGNODE = RD.DAGNODE;
(*
  Текущее состояние регистров
*)

VAR
    iteration*:  BOOLEAN;
    Generation*:  BOOLEAN;

VAR
    everUsed*:    RegSet;                          -- Используется при спасении/
                                                -- восстановлении в прологе/
                                                -- эпилоге
    usedSoFar*:   RegSet;                          -- Состояние каких регистров
                                                -- изменялось при компиляции
                                                -- до сего момента
VAR

    regContents-:   RD.Contents;                     -- Текущее состояние регистров
--    usedForRegContents-:   RD.Contents;
    freeRegs-        : RegSet;                   -- Свободные регистры
    dirtyRegs-       : RegSet;                   -- Загаженные регистры
    busyRegs-        : RegSet;                   -- Занятые регистры

    usagesLeft*      : ARRAY Reg OF SYSTEM.CARD32;   -- Для занятых регистров:
                                                     -- сколько раз к нему будут

--------------------------------------------------------------------------------

<* IF ~ NODEBUG THEN *>
PROCEDURE PrintReg* (r: Reg);
BEGIN
    opIO.print ("  %s: ", D.regName [r]);
    IF (r # UNDEF_REG) AND ~(r IN D.SPILLED_REGS) THEN
      IF r IN dirtyRegs THEN
        opIO.print("#");
      --  ir.PrintVar (regContents [r]);
      --ELSE
      END;
        IF r IN freeRegs THEN
          opIO.print("(");
        END;
        ir.PrintVar (regContents [r]);
        IF r IN freeRegs THEN
          opIO.print(")");
        END;
        opIO.print ("-%d", usagesLeft [r]);
--      END;
    END;
END PrintReg;

PROCEDURE PrintRegIfNeeded* (r: Reg);

BEGIN
    IF (r IN D.RegAndIntersectWith [Emit.baseReg]) THEN RETURN; END;

    IF ~((r IN freeRegs)OR(r IN dirtyRegs)) OR (regContents [r] # ir.UNDEFINED) THEN
        PrintReg(r);
    END;

END PrintRegIfNeeded;

--------------------------------------------------------------------------------

PROCEDURE PrintRegSet* (set: RegSet; c-: ARRAY OF CHAR);
VAR i: Reg;
BEGIN
    opIO.print (c);
    FOR i := D.MINREG TO D.MAXREG DO
        IF i IN set THEN
            opIO.print ("%s ", D.regName [i]);
        END;
    END;
    opIO.print ("\n");
END PrintRegSet;

PROCEDURE PrintFreeRegs*;
VAR r : Reg;
BEGIN
    opIO.print ("Free regs: ");
    FOR r := D.EAX TO D.EBX DO
        IF r IN freeRegs THEN
            opIO.print (" %s  ", D.regName [r]);
        END;
        IF SYSTEM.SUCC(r,20B) IN freeRegs THEN
            opIO.print (" %s  ", D.regName [SYSTEM.SUCC(r,20B)]);
        END;
        IF SYSTEM.SUCC(r,30B) IN freeRegs THEN
            opIO.print (" %s  ", D.regName [SYSTEM.SUCC(r,30B)]);
        END;
    END;
    FOR r := SYSTEM.SUCC(Emit.baseReg) TO D.MAX4REG DO
        IF r IN freeRegs THEN
            opIO.print (" %s  ", D.regName [r]);
        END;
        IF SYSTEM.SUCC(r,20B) IN freeRegs THEN
            opIO.print (" %s  ", D.regName [SYSTEM.SUCC(r,20B)]);
        END;
        IF SYSTEM.SUCC(r,40B) IN freeRegs THEN
            opIO.print (" %s  ", D.regName [SYSTEM.SUCC(r,40B)]);
        END;
    END;
    opIO.print ("\n");
END PrintFreeRegs;

--------------------------------------------------------------------------------

PROCEDURE PrintBusyRegs*;
VAR r : Reg;
    i: SHORTINT;
BEGIN
    opIO.print ("Busy regs: ");
    FOR r := D.EAX TO D.EBX DO
        IF r IN busyRegs THEN
            opIO.print (" %s  ", D.regName [r]);
        ELSE
            IF SYSTEM.SUCC(r,20B) IN busyRegs THEN
                opIO.print ("  %s  ", D.regName [SYSTEM.SUCC(r,20B)]);
            END;
            IF SYSTEM.SUCC(r,30B) IN busyRegs THEN
                opIO.print ("%s ", D.regName [SYSTEM.SUCC(r,30B)]);
            ELSE opIO.print ("   ");
            END;
            IF SYSTEM.SUCC(r,34B) IN busyRegs THEN
                opIO.print ("%s ", D.regName [SYSTEM.SUCC(r,34B)]);
            ELSE opIO.print ("   ");
            END;
        END;
    END;
    FOR r := SYSTEM.SUCC(Emit.baseReg) TO D.MAX4REG DO
        IF r IN busyRegs THEN
            opIO.print (" %s  ", D.regName [r]);
        ELSIF SYSTEM.SUCC(r,20B) IN busyRegs THEN
            opIO.print ("  %s  ", D.regName [SYSTEM.SUCC(r,20B)]);
        ELSE opIO.print ("      ");
        END;
    END;
    FOR r := D.MIN8REG TO D.MAX8REG DO
        IF r IN busyRegs THEN
            opIO.print (" %s  ", D.regName [r]);
        END;
    END;
    FOR i := 0 TO 3 DO
      IF D.SPILLED2Pow[i] IN busyRegs THEN
        opIO.print (" %s  ", D.regName [D.SPILLED2Pow[i]]);
      END;
    END;

    opIO.print ("\n");
END PrintBusyRegs;

--------------------------------------------------------------------------------

PROCEDURE PrintRegs*;
BEGIN
    PrintRegIfNeeded (D.EAX); PrintRegIfNeeded (D.AX); PrintRegIfNeeded (D.AL);  --PrintRegIfNeeded (D.AH);
    PrintRegIfNeeded (D.ECX); PrintRegIfNeeded (D.CX); PrintRegIfNeeded (D.CL);  --PrintRegIfNeeded (D.CH);
    PrintRegIfNeeded (D.EDX); PrintRegIfNeeded (D.DX); PrintRegIfNeeded (D.DL);  --PrintRegIfNeeded (D.DH);
    PrintRegIfNeeded (D.EBX); PrintRegIfNeeded (D.BX); PrintRegIfNeeded (D.BL);  --PrintRegIfNeeded (D.BH);

    PrintRegIfNeeded (D.EBP); PrintRegIfNeeded (D.BP); --PrintRegIfNeeded (D.BPlow);
    PrintRegIfNeeded (D.ESI); PrintRegIfNeeded (D.SI); --PrintRegIfNeeded (D.SIlow);
    PrintRegIfNeeded (D.EDI); PrintRegIfNeeded (D.DI); --PrintRegIfNeeded (D.DIlow);

    PrintRegIfNeeded (D.ECX_EAX);
    PrintRegIfNeeded (D.EDX_EAX);    PrintRegIfNeeded (D.EDX_ECX);
    PrintRegIfNeeded (D.EBX_EAX);    PrintRegIfNeeded (D.EBX_ECX);    PrintRegIfNeeded (D.EBX_EDX);
    PrintRegIfNeeded (D.EBP_EAX);    PrintRegIfNeeded (D.EBP_ECX);    PrintRegIfNeeded (D.EBP_EDX);
    PrintRegIfNeeded (D.ESI_EAX);    PrintRegIfNeeded (D.ESI_ECX);    PrintRegIfNeeded (D.ESI_EDX);
    PrintRegIfNeeded (D.EDI_EAX);    PrintRegIfNeeded (D.EDI_ECX);    PrintRegIfNeeded (D.EDI_EDX);

    PrintRegIfNeeded (D.EBP_EBX);
    PrintRegIfNeeded (D.ESI_EBX);
    PrintRegIfNeeded (D.EDI_EBX);

    PrintRegIfNeeded (D.ESI_EBP);
    PrintRegIfNeeded (D.EDI_EBP);
    PrintRegIfNeeded (D.EDI_ESI);
    opIO.print("\n");

END PrintRegs;

<* END *>

--------------------------------------------------------------------------------
PROCEDURE CheckRegContents*;
VAR r, r8 : Reg;
BEGIN
    ASSERT(~(UNDEF_REG IN freeRegs+dirtyRegs+busyRegs));
    ASSERT(freeRegs*busyRegs=RegSet{});
    FOR r := D.MIN4REG TO D.MAX4REG DO
      IF (r # D.ESP) AND (r#Emit.baseReg) THEN
        ASSERT((r IN dirtyRegs)
               OR(regContents[r] = ir.UNDEFINED)
               OR (ir.Vars[regContents[r]].Def.ResSize=4));
        ASSERT((SYSTEM.SUCC(r,20B) IN dirtyRegs)
               OR(regContents[SYSTEM.SUCC(r,20B)] = ir.UNDEFINED)
               OR (ir.Vars[regContents[SYSTEM.SUCC(r,20B)]].Def.ResSize=2));
        ASSERT( regContents[r] = regContents[SYSTEM.SUCC(r,20B)] , ORD(r));
        ASSERT( usagesLeft[r] = usagesLeft[SYSTEM.SUCC(r,20B)] , ORD(r));
        ASSERT((regContents[r] = ir.UNDEFINED)
               OR (ORD(~(r IN dirtyRegs))+ORD(~(SYSTEM.SUCC(r,20B) IN dirtyRegs))<=1), ORD(r));
        ASSERT(ORD(r IN busyRegs)+ORD(SYSTEM.SUCC(r,20B) IN busyRegs) <= 1,ORD(r));
        IF r IN D.XREGS THEN
          ASSERT(ORD(r IN busyRegs)
               +ORD(SYSTEM.SUCC(r,20B) IN busyRegs)
               +ORD(SYSTEM.SUCC(r,30B) IN busyRegs) <= 1,ORD(r));
          ASSERT((SYSTEM.SUCC(r,30B) IN dirtyRegs)
               OR(regContents[SYSTEM.SUCC(r,30B)] = ir.UNDEFINED)
               OR (ir.Vars[regContents[SYSTEM.SUCC(r,30B)]].Def.ResSize=1));
          ASSERT( regContents[r] = regContents[SYSTEM.SUCC(r,30B)] , ORD(r));
          ASSERT( usagesLeft[r] = usagesLeft[SYSTEM.SUCC(r,30B)] , ORD(r));
          ASSERT((regContents[r] = ir.UNDEFINED)
               OR (ORD(~(r IN dirtyRegs))+ORD(~(SYSTEM.SUCC(r,30B) IN dirtyRegs))<=1), ORD(r));
        END;
      END;
    END;

    FOR r := D.MIN8REG TO D.MAX8REG DO
      IF r IN busyRegs THEN
        ASSERT( usagesLeft[r] = usagesLeft[D.LowPart[r]] , ORD(D.LowPart[r]));
        ASSERT( usagesLeft[r] = usagesLeft[D.HighPart[r]], ORD(D.HighPart[r]));
        ASSERT( ~(D.LowPart[r] IN busyRegs), ORD(D.LowPart[r]));
        ASSERT( ~(D.HighPart[r]IN busyRegs), ORD(D.HighPart[r]));
      END;
      IF r IN freeRegs THEN
        ASSERT( D.LowPart[r]  IN freeRegs, ORD(D.LowPart[r]));
        ASSERT( D.HighPart[r] IN freeRegs, ORD(D.HighPart[r]));
      ELSE
        ASSERT( ~(D.LowPart[r]  IN freeRegs)
              OR ~(D.HighPart[r] IN freeRegs), ORD(r));
      END;
      IF ~(r IN D.RegAndIntersectWith [Emit.baseReg]) THEN
        IF r IN dirtyRegs THEN
          ASSERT( (regContents[D.LowPart[r]] # ir.UNDEFINED) OR
                  (regContents[D.HighPart[r]] # ir.UNDEFINED) OR
                  (usagesLeft[D.LowPart[r]] >0 ) OR
                  (usagesLeft[D.HighPart[r]] >0),  ORD(r));
        ELSE
          ASSERT((regContents[r] = ir.UNDEFINED)
              OR (ir.Vars[regContents[r]].Def.ResSize=8));
          IF r IN busyRegs THEN
            ASSERT( (D.LowPart[r]  IN dirtyRegs), ORD(D.LowPart[r]));
            ASSERT( regContents[r] = regContents[D.LowPart[r]] , ORD(r));
            ASSERT( (D.HighPart[r] IN dirtyRegs), ORD(D.HighPart[r]));
            ASSERT( regContents[r] = regContents[D.HighPart[r]] , ORD(r));
          ELSE
            ASSERT( (D.LowPart[r] = Emit.baseReg)
              OR (D.LowPart[r]  IN freeRegs), ORD(D.LowPart[r]));
            ASSERT( (D.HighPart[r] = Emit.baseReg)
              OR (D.HighPart[r] IN freeRegs), ORD(D.HighPart[r]));
          END;
        END;
      END;
    END;
END CheckRegContents;

--------------------------------------------------------------------------------
PROCEDURE CheckIntegrity*(s: RegSet);
VAR r : Reg;
BEGIN
    FOR r := D.EAX TO D.EBX DO
        IF r IN s THEN
            ASSERT(SYSTEM.SUCC(r,20B) IN s, ORD(r)+20B);
            ASSERT(SYSTEM.SUCC(r,30B) IN s, ORD(r)+30B);
        ELSE
            ASSERT(~(SYSTEM.SUCC(r,20B) IN s), ORD(r)+20B);
            ASSERT(~(SYSTEM.SUCC(r,30B) IN s), ORD(r)+30B);
        END;
    END;
    FOR r := SYSTEM.SUCC(Emit.baseReg) TO D.MAX4REG DO
        IF r IN s THEN
            ASSERT(SYSTEM.SUCC(r,20B) IN s, ORD(r)+20B);
            ASSERT(SYSTEM.SUCC(r,40B) IN s, ORD(r)+40B);
        ELSE
            ASSERT(~(SYSTEM.SUCC(r,20B) IN s), ORD(r)+30B);
            ASSERT(~(SYSTEM.SUCC(r,40B) IN s), ORD(r)+40B);
        END;
    END;
END CheckIntegrity;

--------------------------------------------------------------------------------
--
--      Подпрограммы, связанные с распределением регистров
--
--------------------------------------------------------------------------------

        ----------------
        -- Busy |
        --------------
        --
        --
        --
        --

--------------------------------------------------------------------------------
PROCEDURE ResetRegisters*;
VAR j: Reg;
BEGIN
    freeRegs  := D.allIRegs;
    dirtyRegs := D.RegSet{};
    busyRegs  := D.RegSet{};
    FOR j:=MINREG TO MAXREG DO
        usagesLeft  [j] := 0;
        regContents [j] := ir.UNDEFINED;
    END;
END ResetRegisters;

--------------------------------------------------------------------------------

PROCEDURE MarkUsedSoFar* (r: Reg);
BEGIN
    usedSoFar := usedSoFar + D.RegAndIntersectWith[ r ];
--  dirtyRegs:= dirtyRegs - D.IntersectWith[ r ] ;
END MarkUsedSoFar;

--------------------------------------------------------------------------------
PROCEDURE MarkDirtyRegs(r:Reg);
BEGIN
    dirtyRegs:= dirtyRegs - RegSet{ r } + D.IntersectWith[ r ];
END MarkDirtyRegs;

--------------------------------------------------------------------------------
PROCEDURE RecalcDirties*;
VAR reg:Reg;
BEGIN
(*    FOR reg := D.MIN8REG TO D.MAX8REG DO
      IF reg IN busyRegs THEN
        dirtyRegs := dirtyRegs + D.IntersectWith[reg];
      END;
    END;
*)
    FOR reg := D.MIN8REG TO D.MAX8REG DO
      IF (reg IN busyRegs) THEN
        dirtyRegs := dirtyRegs + D.IntersectWith[reg];
      ELSE
        IF (         D.HighPart[reg] IN busyRegs) OR
           (D.ChSize(D.HighPart[reg],2) IN busyRegs) OR
           (D.ChSize(D.HighPart[reg],1) IN busyRegs) OR
           (         D.LowPart[reg] IN busyRegs) OR
           (D.ChSize(D.LowPart[reg],2) IN busyRegs) OR
           (D.ChSize(D.LowPart[reg],1) IN busyRegs) OR
           (regContents[D.HighPart[reg]]#ir.UNDEFINED) OR
           (regContents[D.LowPart[reg]]#ir.UNDEFINED)
        THEN
          INCL(dirtyRegs,reg);
        END;
      END;
    END;


(*
    FOR r2 := D.MINREG TO SYSTEM.PRED(D.MIN8REG) DO
      FOR reg := SYSTEM.SUCC(r2) TO SYSTEM.PRED(D.MIN8REG) DO
       IF (r2 IN D.XREGS + RegSet{D.EBP..D.EDI,D.BP..D.DI,D.BPlow..D.DIlow})
          AND (reg IN D.XREGS + RegSet{D.EBP..D.EDI,D.BP..D.DI,D.BPlow..D.DIlow})
          AND ~(reg IN D.IntersectWith[r2]) THEN
         IF ~(D.Pair[D.ChSize(r2,4),D.ChSize(reg,4)] IN busyRegs) THEN
           IF (reg IN busyRegs) OR (r2 IN busyRegs) OR
             (regContents[reg]#ir.UNDEFINED) OR (regContents[r2]#ir.UNDEFINED)THEN
             INCL(dirtyRegs,D.Pair[D.ChSize(r2,4),D.ChSize(reg,4)]);
(*<* IF ~NODEBUG THEN *>
           IF opIO.needed THEN
             opIO.print("RecDir ");
             PrintReg(reg);
             PrintReg(r2);
             opIO.print("\n");
             PrintRegSet(dirtyRegs, "DR: ")
           END;
<* END *>
*)
           END;
         END;
       END;
      END;
    END;
*)
END RecalcDirties;
--------------------------------------------------------------------------------
PROCEDURE OccupyReg* (r: Reg);
BEGIN
    ASSERT(r#UNDEF_REG);
    freeRegs := freeRegs  - D.RegAndIntersectWith[ r ];
    INCL( busyRegs, r );
--    dirtyRegs := dirtyRegs - D.REGS8;
    IF r IN D.REGS8 THEN
--      dirtyRegs := dirtyRegs + D.IntersectWith[r];
    END;
--    RecalcDirties;
    MarkDirtyRegs(r);
    MarkUsedSoFar(r);
END OccupyReg;
--------------------------------------------------------------------------------

PROCEDURE FillUsagesLeft*(r: Reg; u: SYSTEM.CARD32);
BEGIN
    usagesLeft [r] := u;
    IF ~(r IN D.SPILLED_REGS) THEN
      IF r IN D.REGS8 THEN
        FillUsagesLeft(D.LowPart [r],u);
        FillUsagesLeft(D.HighPart[r],u);
      ELSE
        r := D.ChSize(r,4);
        usagesLeft [r] := u;
        usagesLeft[SYSTEM.SUCC(r,20B)] := u;
        IF r IN D.XREGS THEN
          usagesLeft[SYSTEM.SUCC(r,30B)] := u;
        END;
      END;
    END;
END FillUsagesLeft;

PROCEDURE FreeReg* (r: Reg);
VAR r2,reg:Reg;
    foo:RegSet;
BEGIN
    freeRegs := freeRegs + D.RegAndIntersectWith4Max[ r ];
    FOR r2 := D.MIN4REG TO D.MAX4REG DO
     FOR reg := D.MIN4REG TO D.MAX4REG DO
      IF (reg # r2) THEN
        foo := RegSet{reg,r2};
        IF (foo*freeRegs=foo) THEN
          INCL(freeRegs,D.Pair[r2,reg]);
          INCL(freeRegs,D.Pair[reg,r2]);
        END;
      END;
     END;
    END;
    FillUsagesLeft(r,0);
    EXCL( busyRegs, r );
--    dirtyRegs:= dirtyRegs- D.RegAndIntersectWith4Max[ r ];--D.SpecialIntersect[ r ];
--    dirtyRegs := dirtyRegs - D.REGS8;
--    RecalcDirtie--s;
END FreeReg;
--------------------------------------------------------------------------------

PROCEDURE WipeReg* (r: Reg);
VAR reg,r2: Reg;

(*PROCEDURE Wipe(wreg:Reg);
VAR r3:Reg;
BEGIN
    FOR r3 := D.MIN8REG TO D.MAX8REG DO
      IF r3 IN D.RegAndIntersectWith[wreg] THEN
        regContents[r3] := ir.UNDEFINED;
      END;
    END;
END Wipe;
*)
BEGIN
    FOR r2 := D.MINREG TO D.MAXREG DO
      IF r2 IN D.SpecialIntersect[ r ] THEN
--        Wipe(r2);
        regContents[r2] := ir.UNDEFINED;
      END;
    END;
    FreeReg(r);
--    freeRegs := freeRegs + D.RegSet{r} + D.PartsOf[ r ];
    dirtyRegs:= dirtyRegs- D.RegAndIntersectWith4Max[ r ];--D.SpecialIntersect[ r ];
    IF r IN D.REGS8 THEN
      dirtyRegs := dirtyRegs - D.REGS8;
      RecalcDirties;
    ELSE
      FOR r2 := D.MIN4REG TO D.MAX4REG DO
        reg := D.Pair[D.ChSize(r,4),r2];
        IF reg # UNDEF_REG THEN
          ASSERT(~(reg IN busyRegs));
          IF (         r2 IN busyRegs) OR
             (D.ChSize(r2,2) IN busyRegs) OR
             (D.ChSize(r2,1) IN busyRegs) OR
             (regContents[r2]#ir.UNDEFINED)
          THEN
            INCL(dirtyRegs,reg);
          ELSE
            EXCL(dirtyRegs,reg);
          END;
        END;
      END;
    END;
(*
    regContents[r] := ir.UNDEFINED;
    FOR reg := MINREG TO MAXREG DO
      IF reg IN D.SpecialIntersect[ r ] THEN
        regContents[reg] := ir.UNDEFINED;
--        usagesLeft[reg] := 0;
      END;
    END;
*)
--    EXCL( busyRegs, r );
    MarkUsedSoFar(r);
END WipeReg;
--------------------------------------------------------------------------------

PROCEDURE MarkVarInReg* (v: VarNum; r: Reg);
VAR reg: Reg;
BEGIN
    FOR reg := MINREG TO MAXREG DO
      IF reg IN D.RegAndIntersectWith[ r ] THEN
        regContents[reg] := v;
      END;
    END;
END MarkVarInReg;
--------------------------------------------------------------------------------

PROCEDURE MarkDirtyVarInReg* (v: VarNum; r: Reg);
BEGIN
    regContents[r] := v;
    INCL(dirtyRegs,r);
END MarkDirtyVarInReg;
--------------------------------------------------------------------------------

PROCEDURE BusyRegIntersectWith*(r:Reg): Reg;
VAR reg: Reg;
BEGIN
   IF (r IN busyRegs) THEN RETURN r; END;
   FOR reg := MINREG TO MAXREG DO
       IF (reg IN busyRegs)&(reg IN D.IntersectWith[r]) THEN
         RETURN reg;
       END;
   END;
   ASSERT(FALSE);
END BusyRegIntersectWith;
--------------------------------------------------------------------------------

(*
  По-прежнему ли в регистре то, что мы думаем, или его успели выгрузить?
*)

PROCEDURE CheckSpilledReg* (VAR r: RegPlacement);
BEGIN
    IF (r.r <> UNDEF_REG) &~(r.r IN D.SPILLED_REGS) &
       (r.v <> ir.UNDEFINED) &
       (regContents [r.r] <> r.v)
    THEN
<* IF ~ nodebug THEN *>
      IF opIO.needed THEN
        IF NOT iteration THEN
            opIO.print ("CheckSpilledReg: var %d reg ", r.v);
            PrintReg (r.r);
            opIO.print ("\n");
            RD.IDB.PrintRegs;
        END;
      END;
<* END *>
        ASSERT (iteration);
        r.r := D.SPILLED[ir.Vars[r.v].Def.ResSize];
    END;
END CheckSpilledReg;

--------------------------------------------------------------------------------

(*
  Освободить регистр, если это возможно
*)

PROCEDURE MarkUseReg* (VAR r: RegPlacement);
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print("==== reg386.MarkUseReg ");
    PrintReg(r.r);
    opIO.print("\n");
<* END *>
    CheckSpilledReg (r);
    IF (r.r <> UNDEF_REG) & ~(r.r IN D.SPILLED_REGS) THEN
        ASSERT(usagesLeft [r.r] > 0);
        ASSERT((r.r IN busyRegs) OR (r.r=Emit.baseReg));
        FillUsagesLeft(r.r, usagesLeft [r.r] - 1);
        IF usagesLeft [r.r] = 0 THEN
            IF (regContents [r.r] = ir.UNDEFINED) OR
               (RD.Loc^[regContents [r.r]].tag <> NTlocal)
            THEN
                WipeReg(r.r);
            ELSE
                FreeReg(r.r);
            END;
        END;
    END;
END MarkUseReg;

--------------------------------------------------------------------------------

(*
  Освободить регистр(ы) способа адресации
*)

PROCEDURE MarkUseAddrMode* (VAR a: Emit.AddrMode);
BEGIN
    MarkUseReg (a.place1);
    MarkUseReg (a.place2);
END MarkUseAddrMode;

--------------------------------------------------------------------------------

(*
  Разместить переменную в выделенном под нее регистре -
  вызывается для корня дерева; заодно посчитает количество использований
*)

PROCEDURE SetInReg* (u, v: VarNum; r: Reg; n: Node); -- u in r,
                                                    -- v = u or
                                                    -- v in addr, u used by v
VAR p: ParamPtr;
    c: CARD;
BEGIN
    OccupyReg(r);
    MarkVarInReg( u, r );
    IF (u # ir.UNDEFINED) & RD.Loc[u].temp THEN
      RD.Loc[u].usedFor := v;
    END;
    IF usagesLeft [r] < HUGE DIV 2 THEN
        IF BitVect.In (Color.LiveAtBottom^[n], ORD(v)) THEN
            c := HUGE;
        ELSE
            c := 0;
            p := ir.Vars^[v].Use;
            WHILE p <> NIL DO
                IF (p^.triade^.NodeNo = n) & (p^.triade^.Op <> ir.o_fi) THEN
                    c := c + 1;
                END;
                p := p^.next;
            END;
        END;
<* IF ~ NODEBUG THEN *>
        IF opIO.needed THEN
            opIO.print ("SetInReg: u=%d v=%d counter=%d ", u, v, c);
            PrintReg(r);
            opIO.print ("\n");
        END;
<* END *>
        FillUsagesLeft(r,usagesLeft [r] + c);
    END;
END SetInReg;

--------------------------------------------------------------------------------

PROCEDURE VarClusterSize (c: INT): SizeType;
VAR i: INT;
   VV: ir.VarNumArray;
   retval : SizeType;
BEGIN
    retval := 0;
    VV := Color.Clusters^[c].v;
    FOR i := 0 TO Color.Clusters^[c].N-1 DO
        IF (ir.Vars^[VV^[i]].Def^.ResSize > retval)
             & (ir.Vars^[VV^[i]].Def^.ResType IN ir.WholeTypes+ir.TypeTypeSet{ir.t_arr,ir.t_ref})
        THEN
            retval := ir.Vars^[VV^[i]].Def^.ResSize;
        END;
    END;
    RETURN retval;
END VarClusterSize;

PROCEDURE MinVarClusterSize (c: INT): SizeType;
VAR i: INT;
   VV: ir.VarNumArray;
   retval : SizeType;
BEGIN
    retval := MAX(SizeType);
    VV := Color.Clusters^[c].v;
    FOR i := 0 TO Color.Clusters^[c].N-1 DO
        IF (ir.Vars^[VV^[i]].Def^.ResSize < retval)
             & (ir.Vars^[VV^[i]].Def^.ResType IN ir.WholeTypes+ir.TypeTypeSet{ir.t_arr,ir.t_ref})
        THEN
            retval := ir.Vars^[VV^[i]].Def^.ResSize;
        END;
    END;
    RETURN retval;
END MinVarClusterSize;

PROCEDURE MinClusterSizeOfVar* (v: ir.VarNum): SizeType;
BEGIN
    IF v >=Color.NNonTempVars THEN
      RETURN ir.Vars[v].Def.ResSize;
    ELSE
      RETURN MinVarClusterSize(Color.Allocation[v].Cluster);
    END;
END MinClusterSizeOfVar;

(*
  Посадить на регистр кластер целиком
*)

PROCEDURE SetClusterReg* (c: INT; r: Reg);
VAR i: INT;
BEGIN
    FOR i:=0 TO Color.Clusters^[c].N-1 DO
        IF ir.Vars[Color.Clusters^[c].v^[i]].Def.ResType # ir.t_float THEN
<* IF ~nodebug THEN *>
          IF opIO.needed THEN
             opIO.print ("==== reg386.SetClusterReg ");
             ir.PrintVar(Color.Clusters^[c].v^[i]);
             opIO.print (" to reg ");
             PrintReg (r);
             opIO.print ("\n");
          END;
<* END *>
          RD.Loc^[Color.Clusters^[c].v^[i]].reg :=
              D.ChSize(r, ir.Vars[Color.Clusters^[c].v^[i]].Def.ResSize);
        END;
    END;
END SetClusterReg;

--------------------------------------------------------------------------------

(*
  Посмотреть: нет ли такой переменной в регистре?
*)

PROCEDURE FindInR* (v: VarNum; ps: RegSet): Reg;
VAR r: Reg;
BEGIN
    FOR r:=MINREG TO MAXREG DO
        IF (regContents [r] = v) & (r IN ps) & ~(r IN dirtyRegs)THEN
            RETURN r;
        END;
    END;
    RETURN UNDEF_REG;
END FindInR;

--------------------------------------------------------------------------------
PROCEDURE FindClusterElementInR* (c: INT; ps: RegSet): Reg;
VAR r: Reg; i:INT;
BEGIN
    FOR r:=MINREG TO MAXREG DO
      FOR i:=0 TO Color.Clusters^[c].N-1 DO
        IF (regContents [r] = Color.Clusters^[c].v^[i]) & (r IN ps) & ~(r IN dirtyRegs)THEN
            RETURN r;
        END;
      END;
    END;
    RETURN UNDEF_REG;
END FindClusterElementInR;

--------------------------------------------------------------------------------

(*
  Выгрузить переменную в память
*)

PROCEDURE SetSpilledVar* (v: VarNum; szSpillingWith: SizeType );
VAR r: Reg;
    u: VarNum;
    c: INT;
    i: INT;
    sz: SizeType;
BEGIN
<* IF ~ nodebug THEN *>
    IF opIO.needed THEN
        IF v < Color.NNonTempVars THEN
            c := Color.Allocation^[v].Cluster;
            opIO.print ("SET SPILLED LOCAL: ");
            ir.PrintLocal(Color.Allocation^[v].Location);
            opIO.print ("\n");
            FOR i:=0 TO Color.Clusters^[c].N-1 DO
                opIO.print ("  SET SPILLED VAR: ");
                ir.PrintVar( Color.Clusters^[c].v^[i] );
                opIO.print (" from ");
                PrintReg (RD.Loc^[v].reg);
                opIO.print (" to ");
                PrintReg (D.SPILLED[ir.Vars[v].Def.ResSize]);
                opIO.print ("\n");
            END;
        ELSE
            opIO.print ("SET SPILLED VAR: ");
            ir.PrintVar( v );
            opIO.print( " from " );
            PrintReg( RD.Loc^[v].reg);
            opIO.print (" to ");
            PrintReg (D.SPILLED[ir.Vars[v].Def.ResSize]);
            opIO.print ("\n");
        END;
    END;
<* END *>
    iteration := TRUE;
    IF RD.Loc[v].temp AND
       RD.Loc[v].hasLongLifeTime AND
       (RD.Loc[v].usedFor # v)
    THEN
      RD.Trees[RD.Loc[v].usedFor].forbidden[RD.Trees[RD.Loc[v].usedFor].nt] := TRUE;
      ASSERT(RD.Loc[RD.Loc[v].usedFor].tag # NTreg);
<* IF ~ nodebug THEN *>
      IF opIO.needed THEN
        opIO.print ("Compound <> ");
        ir.PrintVar( RD.Loc[v].usedFor );
        opIO.print ("\n");
      END;
<* END *>
    ELSE
      ASSERT(~RD.Loc[v].isSpilled,55656);
--      ASSERT ((RD.Loc^[v].tag = NTreg) OR (RD.Loc^[v].tag = NTmem));
      ASSERT(~Generation);
      sz := ir.Vars[v].Def.ResSize;
      IF v < Color.NNonTempVars THEN
          c := Color.Allocation^[v].Cluster;
          r := FindClusterElementInR(c,D.allIRegs);
          SetClusterReg (c, D.SPILLED[VarClusterSize(c)]);
          IF szSpillingWith = 1 THEN
              FOR i:=0 TO Color.Clusters^[c].N-1 DO
                  INCL (ir.Vars^[Color.Clusters^[c].v^[i]].Options,
                        ir.o_SpilledByByte);
              END;
          END;
              FOR i:=0 TO Color.Clusters^[c].N-1 DO
--                  IF RD.Loc[Color.Clusters^[c].v^[i]].hasLongLifeTime THEN
                    ASSERT(~RD.Loc[Color.Clusters^[c].v^[i]].isSpilled, 55656);
                    RD.Loc[Color.Clusters^[c].v^[i]].isSpilled := TRUE;
--                  END;
              END;
      ELSE
          r := FindInR(v,D.allIRegs);
          IF r = UNDEF_REG THEN
            r := RD.Loc^[v].reg;
          END;
          c := -1;
          RD.Loc^[v].reg := D.SPILLED[sz];
          IF szSpillingWith = 1 THEN
              INCL (ir.Vars^[v].Options, ir.o_SpilledByByte);
          END;
--          IF RD.Loc[v].hasLongLifeTime THEN
            ASSERT(~RD.Loc[v].isSpilled,55656);
            RD.Loc[v].isSpilled := TRUE;
--          END;
      END;
      IF (r <> UNDEF_REG) & NOT (r IN freeRegs) THEN
          u := regContents [r];
          IF (u = v) OR
             (u < Color.NNonTempVars) & (Color.Allocation^[u].Cluster = c)
          THEN
  --            INCL (freeRegs, r);
              WipeReg(r);
  --            usagesLeft  [r] := 0;
          END;
      END;
      IF r <> UNDEF_REG THEN
          FOR u:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars)  DO
              CASE RD.Loc^[u].tag OF
              | NTaddr,
                NTbased,
                NTscaled,
                NTmem:
                      IF (RD.Trees^[u]^.a.place1.v <> ir.UNDEFINED) &
                          ((RD.Trees^[u]^.a.place1.v = v) OR
                          (RD.Trees^[u]^.a.place1.v < Color.NNonTempVars) &
                          (Color.Allocation^[RD.Trees^[u]^.a.place1.v].Cluster = c))
                      THEN
                          RD.Trees^[u]^.a.place1.r := D.SPILLED[sz];
                      END;
                      IF (RD.Trees^[u]^.a.place2.v <> ir.UNDEFINED) &
                          ((RD.Trees^[u]^.a.place2.v = v) OR
                          (RD.Trees^[u]^.a.place2.v < Color.NNonTempVars) &
                          (Color.Allocation^[RD.Trees^[u]^.a.place2.v].Cluster = c))
                      THEN
                          RD.Trees^[u]^.a.place2.r := D.SPILLED[sz];
                      END;
              | ELSE
              END;
          END;
      END;
    END;

END SetSpilledVar;

--------------------------------------------------------------------------------

(*
  По-прежнему ли в регистре то, что мы думаем, или его успели выгрузить?
*)

PROCEDURE CheckSpilledNode* (p: DAGNODE);
BEGIN
    CASE p^.nt OF
    | NTreg:       CheckSpilledReg (p^.place);
    | NTmem,
      NTaddr,
      NTbased,
      NTscaled:    CheckSpilledReg (p^.a.place1);
                    CheckSpilledReg (p^.a.place2);
    | ELSE
    END;
END CheckSpilledNode;

--------------------------------------------------------------------------------

(*
  Занести regContents
*)

PROCEDURE VarInReg* (v: VarNum; r: Reg);
BEGIN
    IF ~(r IN D.SPILLED_REGS) THEN
        OccupyReg(r);
        MarkVarInReg( v, r );
        FillUsagesLeft(r,1);
    END;
END VarInReg;

--------------------------------------------------------------------------------

(*
  Занести RD.Loc^[v].reg
*)

PROCEDURE SetLoc* (v: VarNum; r: Reg; temp:= FALSE:BOOLEAN);
BEGIN
    IF temp THEN
      IF (v <> ir.UNDEFINED) THEN
        RD.Loc^[v].reg := D.ChSize(r,ir.Vars[v].Def.ResSize);
      END;
    ELSE
      IF (v <> ir.UNDEFINED) &
         ((RD.Loc^[v].tag = NTreg) OR (RD.Loc^[v].tag = NTmem))
      THEN
          IF (v < Color.NNonTempVars) THEN
              SetClusterReg (Color.Allocation^[v].Cluster, r);
          ELSE
              RD.Loc^[v].reg := D.ChSize(r,ir.Vars[v].Def.ResSize);
          END;
      END;
    END;
END SetLoc;

--------------------------------------------------------------------------------

(*
  Зацеплены ли две переменные?
*)
<* IF TARGET_386 AND fast_bitvect_in THEN *>
<* +procinline *>
<* NEW no_set_range_check+ *>
<* PUSH *>
<*-CHECKNIL    *>
<*-CHECKDIV    *>
<*-CHECKPROC   *>
<*-CHECKSET    *>
<*-CHECKINDEX  *>
<*-CHECKDINDEX  *>
<*-CHECKRANGE  *>
<*-CHECKNIL    *>

PROCEDURE BitVect_In (p: BitVect.BitVector; e: INT): BOOLEAN;
<* NEW override_max_set_len+ *>
<* NEW no_set_range_check+ *>
TYPE
VAR foo:BitVect.LongBitVector;
BEGIN
    foo := SYSTEM.VAL(BitVect.LongBitVector,p);
    RETURN e IN foo^.v;
END BitVect_In;
<* POP *>
<* END *>

PROCEDURE AreHooked* (v1, v2: VarNum): BOOLEAN;
BEGIN
<* IF TARGET_386 AND fast_bitvect_in THEN *>
    RETURN BitVect_In (RD.HookedVars^[v1], ORD(v2));--cos now it's symmetric! OR BitVect.In (RD.HookedVars^[v2], ORD(v1));
<* ELSE *>
    RETURN BitVect.In (RD.HookedVars^[v1], ORD(v2));--cos now it's symmetric! OR BitVect.In (RD.HookedVars^[v2], ORD(v1));
<* END *>
END AreHooked;
<* POP *>
--------------------------------------------------------------------------------

(*
  Выгрузить переменную в память, т.к. не хватило регистра под оперативные нужды
*)

PROCEDURE SpillForTemp (ps: RegSet; szSpillingWith: SizeType): Reg;
VAR g, r: Reg;
    s:    INT;
    u, v: VarNum;
<* IF ~ nodebug THEN *>
    foo : Reg;
<* END *>
BEGIN
<* IF ~ nodebug THEN *>
    IF opIO.needed THEN
    opIO.print ("Spill for temp: ");
    RD.IDB.PrintRegs;
    PrintRegSet (ps, "Allowed regs: ");
    FOR foo := MINREG TO MAXREG DO
      IF ~(foo IN D.SPILLED_REGS) & (regContents[foo] # ir.UNDEFINED) THEN
        PrintReg(foo);
--        ir.PrintVar( regContents[foo] );
        opIO.print( "=%d ", RD.Loc^[regContents[foo]].profit );
      END;
    END;
    END;
<* END *>
    ASSERT(ps#RegSet{});
    g := D.UNDEF_REG;
    s := MAX (INT);
    u := ir.UNDEFINED;
    FOR r:=MINREG TO MAXREG DO
        IF r IN ps THEN
            v := regContents [r];
            IF (v <> ir.UNDEFINED) &
               ((RD.Loc^[v].tag = NTreg) OR (RD.Loc^[v].tag = NTmem)) &
               NOT RD.Loc^[v].temp & (RD.Loc^[v].profit < s)
            THEN
                s := RD.Loc^[v].profit;
                g := r;
                u := v;
            END;
        END;
    END;
    IF s = MAX (INT) THEN
        FOR r:=MINREG TO MAXREG DO
            IF r IN ps THEN
                v := regContents [r];
                IF (v <> ir.UNDEFINED) &
                   (RD.Loc^[v].tag = NTreg) & RD.Loc^[v].temp &
                   (ir.Vars^[v].Use^.triade^.Tag = ir.y_Variable) &
                   ((RD.Loc^[ir.Vars^[v].Use^.triade^.Name].tag = NTmem)    OR
                    (RD.Loc^[ir.Vars^[v].Use^.triade^.Name].tag = NTaddr)   OR
                    (RD.Loc^[ir.Vars^[v].Use^.triade^.Name].tag = NTbased)  OR
                    (RD.Loc^[ir.Vars^[v].Use^.triade^.Name].tag = NTscaled)) &
                   (RD.Loc^[v].profit < s)
                THEN
                    s := RD.Loc^[v].profit;
                    g := r;
                    u := v;
                END;
            END;
        END;
        IF s = MAX (INT) THEN
            env.errors.Fault (env.null_pos, 951);
            HALT;
        END;
        SetSpilledVar (u, szSpillingWith);
        EXCEPTIONS.RAISE (RD.source, 0, "");
    END;
    SetSpilledVar (u, szSpillingWith);
    ASSERT( g # D.UNDEF_REG );
    RETURN g;
END SpillForTemp;

--------------------------------------------------------------------------------

VAR last_find_reg*: Reg;

CONST preferred8regs = ARRAY OF Reg{ D.EDX_EAX, D.EBX_ECX, D.EDI_ESI, D.EBP_EBX};

(*
  Найти регистр, в котором нет ничего осмысленного
  (по возможности с наименьшим номером)
*)

PROCEDURE FindFreeReg* (s: RegSet): Reg;
VAR r, g: Reg;
    t:    RegSet;
    sz : SizeType;
    i: SHORTINT;
BEGIN
    t := s * (D.allIRegs - D.SavedByProc + usedSoFar);
    sz := D.GetRegSetSize(s);
    IF (sz = 8) THEN
      last_find_reg := UNDEF_REG
    END;
    IF t - D.RegAndIntersectWith[ last_find_reg ] <> RegSet{} THEN
        s := t - D.RegAndIntersectWith[ last_find_reg ];
    ELSIF t <> RegSet{} THEN
        s := t;
    END;
    g := D.SPILLED[sz];
    IF sz = 8 THEN
      FOR i:= 0 TO SIZE(preferred8regs)-1 DO
        IF preferred8regs[i] IN s THEN
          last_find_reg := preferred8regs[i];
          RETURN preferred8regs[i];
        END;
      END;
    END;
    FOR r:=MINREG TO MAXREG DO
        IF r IN s THEN
            IF regContents [r] = ir.UNDEFINED THEN
                last_find_reg := r;
                RETURN r;
            ELSIF g IN D.SPILLED_REGS THEN
                g := r;
            END;
        END;
    END;
    IF (usagesLeft [g] # 0) THEN
<* IF ~ nodebug THEN *>
      opIO.print ("usagesLeft # 0 for gotten reg ");
      PrintReg(g);
      opIO.print ("\n");
<* END *>
      ASSERT (FALSE);
    END;
    last_find_reg := g;
    RETURN g;
END FindFreeReg;

--------------------------------------------------------------------------------

PROCEDURE GetPairFrom* (s: RegSet): Reg;
VAR r:Reg;
    i,count:SHORTINT;
    low:Reg;
BEGIN
<* IF ~ nodebug THEN *>
    IF opIO.needed THEN
      opIO.print ("==== reg386.GetPairFrom ");
      PrintRegSet (s, "");
      opIO.print ("\n");
    END;
<* END *>

  s := s * D.allowedIRegs[4];
  count := 0;
  FOR r := D.MIN4REG TO D.MAX4REG DO
    IF r IN s THEN
      INC(count);
    END;
  END;
  IF count < 2 THEN
<* IF ~ nodebug THEN *>
    IF opIO.needed THEN
      opIO.print ("count < 2\n");
    END;
<* END *>
    RETURN UNDEF_REG;
  END;

  FOR i:= 0 TO SIZE(preferred8regs)-1 DO
    IF (D.LowPart[preferred8regs[i]] IN s)&
       (D.HighPart[preferred8regs[i]] IN s)
    THEN
<* IF ~ nodebug THEN *>
    IF opIO.needed THEN
      opIO.print ("got ");
      PrintReg(preferred8regs[i]);
      opIO.print ("\n");
    END;
<* END *>
      RETURN preferred8regs[i];
    END;
  END;
  LOOP
    FOR r := D.MIN4REG TO D.MAX4REG DO
      IF r IN s THEN
        low := r;
        EXIT;
      END;
    END;
    EXIT;
  END;
  FOR r := SYSTEM.SUCC(low) TO D.MAX4REG DO
    IF r IN s THEN
<* IF ~ nodebug THEN *>
    IF opIO.needed THEN
      opIO.print ("got ");
      PrintReg(D.Pair[low,r]);
      opIO.print ("\n");
    END;
<* END *>
      RETURN D.Pair[low,r];
    END;
  END;
  ASSERT(FALSE);
END GetPairFrom;

--------------------------------------------------------------------------------
(*
  Найти регистр, в котором нет ничего осмысленного
  (по возможности с наибольшим номером); Только целый!
*)

PROCEDURE FindFreeRegBack* (s: RegSet): Reg;
VAR r, g: Reg;
BEGIN
    g := D.SPILLED[D.GetRegSetSize(s)];
    FOR r:=MAXREG TO MINREG BY -1 DO
        IF r IN s THEN
            IF regContents [r] = ir.UNDEFINED THEN
                RETURN r;
            ELSIF g IN D.SPILLED_REGS THEN
                g := r;
            END;
        END;
    END;
    ASSERT (usagesLeft [g] = 0);
    RETURN g;
END FindFreeRegBack;

--------------------------------------------------------------------------------

PROCEDURE InitRegs*;
VAR r: Reg;
BEGIN
    freeRegs := D.allIRegs;
    dirtyRegs := RegSet {};
    busyRegs := RegSet {};
    usedSoFar := D.RegSet{};
    FOR r := MINREG TO MAXREG DO
        regContents [r] := ir.UNDEFINED;
        IF r IN D.allIRegs THEN
            usagesLeft  [r] := 0;
        ELSE
            usagesLeft  [r] := HUGE;
        END;
    END;
END InitRegs;
--------------------------------------------------------------------------------

(*
  Выделить совсем временный регистр на одну-две команды
*)

PROCEDURE GTR* (ps: RegSet): Reg;
VAR
  ps1, ps2 : RegSet;
  r, reg, low,high: Reg;
  firstReg : Reg;
BEGIN
<* IF ~ nodebug THEN *>
    firstReg := D.UNDEF_REG;
    FOR reg := D.MINREG TO D.MAXREG DO
      IF reg IN ps THEN
        IF firstReg # D.UNDEF_REG THEN
          IF D.RegInfo[firstReg].sz # D.RegInfo[reg].sz THEN
            opIO.print ("GTR!!!!!1 => unsized - ");
            PrintReg (firstReg);
            opIO.print (" & ");
            PrintReg (reg);
          END;
        ELSE
          firstReg := reg;
        END;
      END;
    END;
<* END *>
    ASSERT(ps#RegSet{});
    IF freeRegs * ps = RegSet{} THEN
        IF D.GetRegSetSize(ps * D.allIRegs) = 8 THEN
          ps1 := D.GetAllLows(ps) * D.allIRegs;
          IF RegSet{} = freeRegs * ps1 THEN
            low := SpillForTemp (ps1, 8);
          ELSE
            low := FindFreeReg (freeRegs * ps1);
          END;
          ASSERT(low#UNDEF_REG);
          ps1 := D.GetAllHighsWithLow(ps * D.allIRegs, low)* D.allIRegs;
          IF RegSet{} = freeRegs *ps1 THEN
            ps2 := ps1;
            ps1 := D.GetAllLowsWithHigh(ps * D.allIRegs, low)* D.allIRegs;
            IF RegSet{} = freeRegs *ps1 THEN
              high := SpillForTemp (ps1+ps2, 8);
            ELSE
              high := FindFreeReg (freeRegs *ps1);
            END;
          ELSE
            high := FindFreeReg (freeRegs *ps1);
          END;
          ASSERT(high#UNDEF_REG);
          ASSERT(high#low);
          r := D.Pair[high,low];
        ELSE
          r := SpillForTemp (ps * D.allIRegs, D.GetRegSetSize(ps * D.allIRegs));
        END;
    ELSE
        r := FindFreeReg (freeRegs * ps);
    END;
    WipeReg(r);
    OccupyReg(r);
<* IF ~ nodebug THEN *>
    IF opIO.needed THEN
        opIO.print ("GTR => ");
        PrintReg (r);
        opIO.print ("\n");
    END;
<* END *>
    RETURN r;
END GTR;

--------------------------------------------------------------------------------

(*
  Посчитать регистры, занятые зацепленными с v не-временными переменными
*)
<* PUSH *>
<*-CHECKINDEX  *>
<*-CHECKRANGE  *>
<*-CHECKNIL    *>
<*-CHECKDIV    *>
<*-CHECKPROC   *>
<*-CHECKSET    *>

PROCEDURE HookedRegs* (v: VarNum): RegSet;
VAR u: VarNum;
    s: RegSet;
    r: Reg;
    RDHVv: BitVect.BitVector;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print( "==== reg386.HookedRegs\n" );
<* END *>
    s := RegSet{};
    RDHVv := RD.HookedVars^[v];
    FOR u:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
        IF (u <> v) & (RD.Loc^[u].tag <> NTlocal) &
             AreHooked (v,u)
(* inlined start AreHooked (u, v) *)
--                BitVect_In (RDHVv,u)
(* inlined end *)
        THEN
          LOOP
            IF (RD.Loc[v].tag = NTlocal) AND (~RD.Loc[u].hasLongLifeTime) THEN
--              EXIT
            END;
            CASE RD.Loc^[u].tag OF
            | NTreg:
<* IF ~ nodebug THEN *>
--                   PrintReg(RD.Loc^[u].reg);
<* END *>
                   r := RD.Loc^[u].reg;
            | NTbased:
<* IF ~ nodebug THEN *>
--                 PrintReg(RD.Trees^[u]^.a.place1.r);
<* END *>
                 r := RD.Trees^[u]^.a.place1.r;
            | NTscaled:
<* IF ~ nodebug THEN *>
--                PrintReg(RD.Trees^[u]^.a.place2.r);
<* END *>
                r := RD.Trees^[u]^.a.place2.r;
            | NTaddr,
              NTmem:
<* IF ~ nodebug THEN *>
--                     PrintReg(RD.Trees^[u]^.a.place1.r);
--                     PrintReg(RD.Trees^[u]^.a.place2.r);
<* END *>
                     s := s + D.RegAndIntersectWith[RD.Trees^[u]^.a.place1.r];
                           r := RD.Trees^[u]^.a.place2.r;
            | ELSE
                EXIT;
            END;
            s := s + D.RegAndIntersectWith[r];
<* IF ~ nodebug THEN *>
--          opIO.print(" ");
--          ir.PrintVar(u);
--          opIO.print("\n");
<* END *>
            EXIT
          END;
        END;
    END;
<* IF ~ nodebug THEN *>
   IF opIO.needed THEN
    opIO.print ("==== reg386.HookedRegs ");
    ir.PrintVar(v);
    PrintRegSet(s, " = ");
   END;
<* END *>
    RETURN s;
END HookedRegs;
<* POP *>

PROCEDURE HookedRegsWOVar* (v, wovar: VarNum): RegSet;
VAR prevWOVarTag: nts.NT;
    retval : RegSet;
BEGIN
    IF wovar # ir.UNDEFINED THEN
      prevWOVarTag := RD.Loc[wovar].tag;
      RD.Loc[wovar].tag := nts.NTnowhere;
      retval := HookedRegs(v);
      RD.Loc[wovar].tag := prevWOVarTag;
    ELSE
      retval := HookedRegs(v);
    END;
    RETURN retval;
END HookedRegsWOVar;
--------------------------------------------------------------------------------

(*
  Решить, какую из сцепленных с v переменных надо разместить в памяти;
  может потребоваться выгрузить сразу много переменных - до тех пор, пока
  не появится регистр, не зацепленный с v или не выгрузим саму v.
*)

PROCEDURE SpillReg* (v: VarNum; ps: RegSet; obligatory:=FALSE: BOOLEAN): Reg;
VAR u, sv: VarNum;
    sc:    INT;
    r,rr,wr:     Reg;
    b:     BOOLEAN;
    hookedregsV: RegSet;
    pullingSize: SizeType;
<* IF ~ nodebug THEN *>
    foo: Reg;
<* END *>

    PROCEDURE TrySpill (v: VarNum);
    VAR r: Reg;
    BEGIN
        r := RD.Loc^[v].reg;
        IF (r = UNDEF_REG) OR (r IN freeRegs) OR (regContents [r] <> ir.UNDEFINED)
        THEN
            IF RD.Loc^[v].profit < sc THEN
                sc := RD.Loc^[v].profit;
                sv := v;
            ELSIF RD.Loc^[v].profit = sc THEN
                IF b & (RD.Loc^[v].reg IN D.SavedByProc) OR
                   NOT b & NOT (RD.Loc^[v].reg IN D.SavedByProc)
                THEN
                    sv := v;
                END;
            END;
        END;
    END TrySpill;

BEGIN
<* IF ~ nodebug THEN *>
    IF opIO.needed THEN
    opIO.print( "==== reg386.SpillReg for " );
    ir.PrintVar (v);
    opIO.print( "=%d", RD.Loc^[v].profit );
    PrintRegSet (ps, "; allowed regs: ");
    PrintRegSet (freeRegs, "Free regs: ");
    FOR foo := MINREG TO MAXREG DO
      IF ~(foo IN D.SPILLED_REGS) & (regContents[foo] # ir.UNDEFINED) THEN
        PrintReg(foo);
--        ir.PrintVar( regContents[foo] );
        opIO.print( "=%d ", RD.Loc^[regContents[foo]].profit );
      END;
    END;
    opIO.print( "\n" );
    END;
<* END *>
    ASSERT (NOT (D.ESP IN ps) & NOT (Emit.baseReg IN ps));
    ASSERT(ps#RegSet{});
    b := ir.VarOptionsSet{ ir.o_LiveAtCall, ir.o_Backwards } * ir.Vars^[v].Options <> ir.VarOptionsSet{};
    REPEAT
        sc := MAX (INT);
        FOR u:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
            IF NOT RD.Loc^[u].temp THEN
                IF ~obligatory AND (u = v) THEN
                    TrySpill (u);
                ELSIF (RD.Loc^[u].tag <> NTlocal) & AreHooked (u, v) THEN
                    CASE RD.Loc^[u].tag OF
                    | NTreg:       IF (RD.Loc^[u].reg <> UNDEF_REG) &
                                       (D.RegAndIntersectWith[RD.Loc^[u].reg] * ps # RegSet {})
                                    THEN
                                        TrySpill (u);
                                    END;
                    | NTaddr,
                      NTbased,
                      NTscaled,
                      NTmem:       IF (RD.Trees^[u]^.a.place1.r <> UNDEF_REG) &
                                       (RD.Trees^[u]^.a.place1.v <> ir.UNDEFINED) &
                                       NOT RD.Loc^[RD.Trees^[u]^.a.place1.v].temp &
                                       (RD.Trees^[u]^.a.place1.r IN ps) &
                                       RD.Loc[RD.Trees^[u]^.a.place1.v].hasLongLifeTime
                                    THEN
                                        TrySpill (RD.Trees^[u]^.a.place1.v);
                                    END;
                                    IF (RD.Trees^[u]^.a.place2.r <> UNDEF_REG) &
                                       (RD.Trees^[u]^.a.place2.v <> ir.UNDEFINED) &
                                       NOT RD.Loc^[RD.Trees^[u]^.a.place2.v].temp &
                                       (RD.Trees^[u]^.a.place2.r IN ps)&
                                       RD.Loc[RD.Trees^[u]^.a.place2.v].hasLongLifeTime
                                    THEN
                                        TrySpill (RD.Trees^[u]^.a.place2.v);
                                    END;
                    | ELSE
                    END;
                END;
            END;
        END;
        LOOP
          IF sc = MAX (INT) THEN
              FOR u:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                  IF ~obligatory AND (u = v) THEN
                      TrySpill (u);
                  ELSIF (RD.Loc^[u].tag <> NTlocal) & AreHooked (u, v) THEN
                      CASE RD.Loc^[u].tag OF
                      | NTaddr,
                        NTbased,
                        NTscaled,
                        NTmem:       IF (RD.Trees^[u]^.a.place1.r <> UNDEF_REG) &
                                          (RD.Trees^[u]^.a.place1.v <> ir.UNDEFINED) &
                                          RD.Loc^[RD.Trees^[u]^.a.place1.v].temp &
                                          (RD.Trees^[u]^.a.place1.r IN ps)&
                                         RD.Loc[RD.Trees^[u]^.a.place1.v].hasLongLifeTime
                                      THEN
                                          TrySpill (RD.Trees^[u]^.a.place1.v);
                                      END;
                                      IF (RD.Trees^[u]^.a.place2.r <> UNDEF_REG) &
                                          (RD.Trees^[u]^.a.place2.v <> ir.UNDEFINED) &
                                          RD.Loc^[RD.Trees^[u]^.a.place2.v].temp &
                                          (RD.Trees^[u]^.a.place2.r IN ps)&
                                         RD.Loc[RD.Trees^[u]^.a.place2.v].hasLongLifeTime
                                      THEN
                                          TrySpill (RD.Trees^[u]^.a.place2.v);
                                      END;
                      | ELSE
                      END;
                  END;
              END;
              IF sc = MAX (INT) THEN
                IF obligatory THEN
                  sv := v;
                  EXIT;  -- loop
                ELSE
                  env.errors.Fault (env.null_pos, 951);
                  HALT;
                END;
              END;
              SetSpilledVar (sv, ir.Vars^[v].Def^.ResSize);
              EXCEPTIONS.RAISE (RD.source, 0, "");
          END;
          EXIT;
        END;
        r := RD.Loc^[sv].reg;
        IF (r # UNDEF_REG)&(regContents[r] # ir.UNDEFINED) THEN
          pullingSize := ir.Vars^[regContents[r]].Def^.ResSize;
        ELSIF (r # UNDEF_REG)&(regContents[r] = ir.UNDEFINED) THEN
          pullingSize := 4;
        ELSE
          pullingSize := -1;  -- hard checking for internal errors
        END;
        SetSpilledVar (sv, ir.Vars^[v].Def^.ResSize);
        IF sv = v THEN
            RETURN D.SPILLED[ir.Vars[sv].Def.ResSize];
        END;
        hookedregsV := HookedRegs (v);
    UNTIL ( (ir.Vars^[v].Def^.ResSize<=4) & NOT (r IN hookedregsV) ) OR
          ( (ir.Vars^[v].Def^.ResSize=8) & (GetPairFrom(freeRegs-hookedregsV)#UNDEF_REG) );
    IF ir.Vars^[v].Def^.ResSize<=4 THEN
--        wr := r;
--        IF regContents[r] # ir.UNDEFINED THEN
        wr := D.ChSize(r, pullingSize);
--        END;
        rr := D.ChSize(r, D.GetRegSetSize(ps));
    ELSE
      rr := GetPairFrom(freeRegs-hookedregsV);
      wr := rr;
    END;
    WipeReg(wr);
    RETURN rr;
END SpillReg;

--------------------------------------------------------------------------------

(*
  Is there a byte variable in the cluster of a given variable
*)

PROCEDURE IsSizeIntVarInCluster (v: VarNum; sz: SizeType): BOOLEAN;
VAR i: INT;
    c: INT;
   VV: ir.VarNumArray;
BEGIN
    c := Color.Allocation^[v].Cluster;
    VV := Color.Clusters^[c].v;
    FOR i := 0 TO Color.Clusters^[c].N-1 DO
        IF (ir.Vars^[VV^[i]].Def^.ResSize = sz)
             & (ir.Vars^[VV^[i]].Def^.ResType IN ir.WholeTypes+ir.TypeTypeSet{ir.t_arr})
        THEN
            RETURN TRUE
        END;
    END;
    RETURN FALSE
END IsSizeIntVarInCluster;

--------------------------------------------------------------------------------

(*
  Выделить постоянный регистр под переменную;
*)

PROCEDURE GR* (v: VarNum; ps: RegSet; doOccupy:=TRUE:BOOLEAN; obligatory:=FALSE:BOOLEAN): Reg;
VAR r: Reg;
    s: RegSet;
    o: ir.VarOptionsSet;
    sz: ir.SizeType;
    error:BOOLEAN;
BEGIN
    sz := ir.Vars[v].Def.ResSize;
<* IF ~ nodebug THEN *>
    opIO.print ("==== reg386.GR sz %d ", sz);
    ir.PrintVar(v);
    PrintRegSet(ps, " in ");
    PrintFreeRegs;
<* END *>
    ASSERT(ps#RegSet{});
    r := RD.Loc^[v].reg;
    IF r # UNDEF_REG THEN
      ASSERT(r IN RegSet{D.SPILLED[ir.Vars[v].Def.ResSize]} + ps * D.allowedIRegs[ir.Vars[v].Def.ResSize]);
    ELSE
--      ASSERT((v >= Color.NNonTempVars) OR (sz = 8) OR ~IsSizeVarInCluster (v,8));
      ASSERT(ps # RegSet{});
      IF (v < Color.NNonTempVars) & IsSizeIntVarInCluster (v,8) THEN
        ps := ps * D.allowedIRegs[8];
      ELSE
        ps := ps * D.allowedIRegs[ir.Vars[v].Def.ResSize];
      END;
      ASSERT(ps # RegSet{});
      IF (v < Color.NNonTempVars) & IsSizeIntVarInCluster (v,1)
           OR (ir.Vars[v].Def.ResSize = 1) THEN
            ps := ps * D.XREGS;
      END;
      s := freeRegs * ps - HookedRegs (v);;

      IF s = RegSet{} THEN
--          IF ir.Vars^[v].Def^.ResType = ir.t_float THEN
--              r := SpillReg (v, ps);
--          ELSE
              r := SpillReg (v, ps * D.allIRegs, obligatory);
--          END;
      ELSE
          o := ir.Vars^[v].Options;
          IF ir.VarOptionsSet{ir.o_LiveAtCall, ir.o_Backwards} * o <> ir.VarOptionsSet{} THEN
              IF (ir.o_LiveAtCopy IN o) & (D.EBP IN s) THEN
                  r := D.EBP;
              ELSIF s * D.SavedByProc <> RegSet{} THEN
                  r := FindFreeRegBack (s * D.SavedByProc);
              ELSE
                  r := FindFreeRegBack (s);
              END;
          ELSIF ir.o_LiveAtMulDiv IN o THEN
              IF (s - D.RegAndIntersectWith4Max[D.EAX]
                    - D.RegAndIntersectWith4Max[D.EDX]) <> RegSet{} THEN
                  r := FindFreeReg (s - D.RegAndIntersectWith4Max[D.EAX]
                                      - D.RegAndIntersectWith4Max[D.EDX]);
              ELSIF D.RegTable[D.EDXp,sz] IN s THEN
                  r := D.RegTable[D.EDXp,sz];
              ELSE
                  ASSERT (D.RegTable[D.EAXp,sz] IN s);
                  r := D.RegTable[D.EAXp,sz]
              END;
          ELSIF (ir.o_LiveAtFCom IN o) & ((s - D.RegAndIntersectWith4Max[D.EAX]) <> RegSet{}) THEN
              r := FindFreeReg (s - D.RegAndIntersectWith4Max[D.EAX]);
          ELSE
              r := FindFreeReg (s);
          END;
      END;
      SetLoc (v, r,  obligatory);
--        IF (v < Color.NNonTempVars) & (sz < 8) & IsSizeVarInCluster (v,8) THEN END;
      r := D.ChSize(r,sz);
    END;
    IF regContents[r] = ir.UNDEFINED THEN
      error := ~(r IN freeRegs+D.SPILLED_REGS);
    ELSE
      error := ~(r IN freeRegs+D.SPILLED_REGS) AND (v # regContents[r]) AND ~RD.Loc[regContents[r]].temp;
    END;
    IF doOccupy & ~(r IN D.SPILLED_REGS) THEN
        OccupyReg(r);
    END;
<* IF ~ nodebug THEN *>
    IF opIO.needed THEN
      opIO.print ("  got ");
      ir.PrintVar(v);
      opIO.print (" in ");
      PrintReg (r);
      opIO.print ("\n");
    END;
<* END *>
   ASSERT(~(Generation AND (r IN D.SPILLED_REGS)));
    IF error THEN
<* IF ~ nodebug THEN *>
      opIO.print ("!!!!! NOT IN FREEREGS\n");
<* END *>
      ASSERT(FALSE);
    END;
    RETURN r;
END GR;

--------------------------------------------------------------------------------

(*
  Выделить регистр под переменную-результат операции
*)

PROCEDURE gr* (v: VarNum; ps: RegSet): Reg;
VAR r: Reg;
BEGIN
    -- finding lifetime
    IF ~RD.Loc[v].hasLongLifeTime THEN
        r :=  GTR (ps*D.allowedIRegs [ir.Vars[v].Def.ResSize]);
        VarInReg(v,r);
        SetLoc (v, r, TRUE);
        RETURN r;
    ELSIF (RD.Loc^[v].tag = NTreg) OR (RD.Loc[v].temp) THEN
        RETURN GR (v, ps, TRUE, RD.Loc[v].temp);--~!!!
    ELSE
        RETURN GTR (ps*D.allowedIRegs [ir.Vars[v].Def.ResSize]);
    END;
END gr;

--------------------------------------------------------------------------------

(*
  Выделить по возможности просимый регистр
*)

PROCEDURE GetPreferredReg* (r: Reg; v: VarNum; notlocal: BOOLEAN; doOccupy:=TRUE:BOOLEAN; longlife:=TRUE: BOOLEAN): Reg;
VAR sz: SHORTINT;
    reg: Reg;
BEGIN
<* IF ~ nodebug THEN *>
    opIO.print ("==== reg386.GetPreferredReg sz %d reg ", ir.Vars[v].Def.ResSize );
    PrintReg(r);
    opIO.print (" ");
    ir.PrintVar(v);
    PrintFreeRegs;
<* END *>
    sz := ir.Vars^[v].Def^.ResSize;
    IF RD.Loc^[v].tag = NTlocal THEN
        ASSERT(doOccupy);
        IF notlocal AND RD.Loc[v].hasLongLifeTime THEN
            RETURN UNDEF_REG;
        ELSIF (r IN freeRegs) & (r IN D.allowedIRegs [sz]) THEN
            SetLoc(v, r, TRUE);
            RETURN r;
        ELSE
            reg := GTR (D.allowedIRegs [sz]);
            SetLoc(v, reg, TRUE);
            RETURN reg;
        END;
    ELSIF (RD.Loc^[v].reg = UNDEF_REG) & (r IN freeRegs) &
          (r IN D.allowedIRegs [sz]) & NOT (r IN HookedRegs (v)) &
          ((r IN D.SavedByProc) OR
           (ir.VarOptionsSet{ ir.o_LiveAtCall, ir.o_Backwards } * ir.Vars^[v].Options = ir.VarOptionsSet{}))
    THEN
        SetLoc (v, r, ~longlife );
    ELSE
        r := GR (v, D.allIRegs, doOccupy);
    END;
<* IF ~ nodebug THEN *>
    IF opIO.needed THEN
      opIO.print ("  got ");
      ir.PrintVar(v);
      opIO.print (" in ");
      PrintReg (r);
      opIO.print ("\n");
    END;
<* END *>
    RETURN r;
END GetPreferredReg;

--------------------------------------------------------------------------------
(*
  Попытаться при проходе сверху вниз по дереву приписать переменной регистр
*)

PROCEDURE TryAssignReg* (p: DAGNODE; r: Reg);
(*
VAR v: VarNum;
BEGIN
    IF (r IN freeRegs) & (p^.op <> ir.o_par) THEN
        v := p^.p^.Name;
        IF (RD.Loc^[v].tag = reg) & (RD.Loc^[v].reg = UNDEF_REG) &
           (r IN D.AllowedIRegs [ir.Vars^[v].Def^.ResSize]) &
           NOT (r IN HookedRegs (v))
        THEN
            SetLoc (v, r);
            EXCL (freeRegs, r);
        END;
    END;
*)
END TryAssignReg;


END reg386.
