(* Created by KDV;
   definition of internal representation of BE_386 object code;
*)

MODULE OcirDef;

IMPORT  ir,
        Color,
        b := BitVect,
        Emit;
IMPORT D := desc386;
IMPORT SYSTEM;
------------------------------------------------------------------------------
                      (* Registers definition *)

CONST
        UNDEF_REG *= D.UNDEF_REG;
        UNDEF_REGp *= D.UNDEF_REGp;

        EAX *= D.EAXp; ECX *= D.ECXp; EDX *= D.EDXp; EBX *= D.EBXp;

        ESP *= D.ESPp; EBP *= D.EBPp; ESI *= D.ESIp;  EDI *= D.EDIp;

        ST0 *= D.ST0p;  ST1 *= D.ST1p; ST2 *= D.ST2p; ST3 *= D.ST3p;
        ST4 *= D.ST4p;  ST5 *= D.ST5p; ST6 *= D.ST6p; ST7 *= D.ST7p;

        MINREG  * = EAX;
        MAXFREG * = ST7;
--        RegsNum *= ST7 + 1;


TYPE    Register* = D.PhysReg;
        Regs*     = D.PhysRegSet;
        SizeType  *= Emit.SizeType;

------------------------------------------------------------------------------
                              (* flags *)

CONST
        CF*  =  0; PF*  =  1; AF*  =  2; ZF*   =  3; SF* =  4;
        _OF* =  5;

        C0* = 6; C1* = 7; C2* = 8; C3* = 9;

        FlagsNum *= C3 + 1;

TYPE    Flags*  = SET;

CONST
        ALL_AFLAGS   *= Flags{CF.._OF};
        AFLAGS_NO_AF *= Flags{CF,PF,ZF.._OF};
        AFLAGS_NO_CF *= Flags{PF.._OF};
        AFLAGS_NO_OF *= Flags{CF..SF};

        ALL_CFLAGS   *= Flags{C0..C1};


------------------------------------------------------------------------------
                 (* definition of operation prefixes *)
CONST
        P_REP*  = 0;    P_REPE* = 1;    P_REPNE*= 2;    P_REPZ* = 3;
        P_REPNZ*= 4;    P_LOCK* = 5;

        P_CS*   = 6;    P_SS*   = 7;    P_DS*   = 8;    P_ES*   = 9;
        P_FS*   = 10;   P_GS*   = 11;

        P_OS*   = 12;

        P_AS*   = 13;

        LAST_PREFIX* = P_AS;


TYPE   OpPrefix*  = SET;

------------------------------------------------------------------------------
                (* definitions of operation mnemonic *)

    (* GeneralDataTransfer *)

TYPE  OpCode*     = ( --INTEGER; (* [UNDEF_OP..WRMSR];*)
    UNDEF_OP,  (* used only for initialization *)
    BSWAP,    CMPXCHG,    CMPXCHG8B,  _IN,
    LEA,      MOV,        MOVSX,      MOVZX,
    OUT,      POP,        POPA,       POPAD,
    PUSH,     PUSHA,      PUSHAD,     XCHG,

    (* SegmentControl *)

    LDS,            LES,            LFS,            LGS,
    LSS,

    (* FlagControl *)

    CLC,            CLD,            CLI,            CLTS,
    CMC,            LAHF,           POPF,           POPFD,
    PUSHF,          PUSHFD,         SAHF,           STC,
    STD,            STI,

    (* Arithmetic *)

    AAA,            AAD,            AAM,            AAS,
    ADC,            ADD,            CBW,            CDQ,
    CMP,            CWD,            CWDE,           DAA,
        DAS,            _DEC,           _DIV,          IDIV,
        IMUL,           _INC,          MUL,           NEG,
    SBB,           SUB,           XADD,

    (* Logic *)

        _AND,          _NOT,          _OR,           RCL,
    RCR,           ROL,           ROR,           SAL,
    SAR,           SHL,           SHLD,          SHR,
    SHRD,          TEST,          XOR,

    (* StringManipulation *)

    CMPS,          CMPSB,         CMPSD,         CMPSW,
    INS,           INSB,          INSD,          INSW,
    LODS,          LODSB,         LODSD,         LODSW,
    MOVS,          MOVSB,         MOVSD,         MOVSW,
    OUTS,          OUTSB,         OUTSD,         OUTSW,
    SCAS,          SCASB,         SCASD,         SCASW,
    STOS,          STOSB,         STOSD,         STOSW,
    XLAT,          XLATB,

    (* BitManipulation *)

    BSF,           BSR,           BT,        BTC,
    BTR,           BTS,

    (* ControlTransfer *)

    CALL,          JMP,           RET,

    (* ConditionalJumps *)

    JA,        JAE,           JB,        JBE,
    JC,        JCXZ,          JE,        JECXZ,
    JG,        JGE,           JL,        JLE,
    JNA,           JNAE,          JNB,           JNBE,
    JNC,           JNE,           JNG,           JNGE,
    JNL,           JNLE,          JNO,           JNP,
    JNS,           JNZ,           JO,        JP,
    JPE,           JPO,           JS,        JZ,
        _LOOP,         LOOPE,         LOOPNE,        LOOPNZ,
    LOOPZ,

    (* ConditionalByteSet *)

    ENTER,         LEAVE,         SETA,          SETAE,
    SETB,          SETBE,         SETC,          SETE,
    SETG,          SETGE,         SETL,          SETLE,
    SETNA,         SETNAE,        SETNB,         SETNBE,
    SETNC,         SETNE,         SETNG,         SETNGE,
    SETNL,         SETNLE,        SETNO,         SETNP,
    SETNS,         SETNZ,         SETO,          SETP,
    SETPE,         SETPO,         SETS,          SETZ,

    (* Interrupt *)

    BOUND,         INT,           INTO,          IRET,
    IRETD,

    (* ProcessorControl *)

    HLT,           NOP,           WAIT,

    (* Float *)

    F2XM1,         FABS,          FADD,          FADDP,
    FBLD,          FBSTP,         FCHS,          FCLEX,
    FCOM,          FCOMP,         FCOMPP,        FCOS,
    FDECSTP,       FDIV,          FDIVP,         FDIVR,
    FDIVRP,        FFREE,         FIADD,         FICOM,
    FICOMP,        FIDIV,         FIDIVR,        FILD,
    FINCSTP,       FINIT,         FIST,          FISTP,
    FISUB,         FISUBR,        FLD,           FLD1,
    FLDCW,         FLDENV,        FLDL2E,        FLDL2T,
    FLDLG2,        FLDLN2,        FLDPI,         FLDZ,
        FMUL,          FMULP,
        FNCLEX,        FNINIT,        FNOP,          FNSAVE,
        FNSTCW,        FNSTENV,       FNSTSW,        FPATAN,
        FPREM,         FPREM1,        FPTAN,         FRNDINT,
        FRSTOR,        FSAVE,         FSCALE,        FSIN,
        FSINCOS,       FSQRT,         FST,           FSTCW,
        FSTENV,        FSTP,          FSTSW,         FSUB,
        FSUBP,         FSUBR,         FSUBRP,        FTST,
        FUCOM,         FUCOMP,        FUCOMPP,       FWAIT,
        FXAM,          FXCH,          FXTRACT,       FYL2X,
        FYL2XP1,

    (* LowProcessorControl *)

        ARPL,          CPUID,         INVD,          INVLPG,
        LAR,           LGDT,          LIDT,          LLDT,
        LMSW,          LSL,           LTR,           RDMSR,
        RSM,           SGDT,          SIDT,          SLDT,
        SMSW,          STR,           VERR,          VERW,
        WBINVD,        WRMSR
      );

      OpGroup*    = (  --      SHORTINT;(* [GDT_OGROUP..LPC_GROUP];*)
        GDT_OGROUP,
        SC_OGROUP,
        FC_OGROUP,
        A_OGROUP,
        L_OGROUP,
        SM_OGROUP,
        BM_OGROUP,
        CT_OGROUP,
        CJ_OGROUP,
        CBS_OGROUP,
        I_OGROUP , 
        PC_OGROUP, 
        F_OGROUP,   
        LPC_OGROUP
      );
      OpGroupSet *= PACKEDSET OF OpGroup;

PROCEDURE GetOpGroup*(op : OpCode) : OpGroup;
BEGIN
  CASE op OF
    BSWAP..XCHG :    RETURN GDT_OGROUP |
    LDS..LSS    :    RETURN SC_OGROUP  |
    CLC..STI    :    RETURN FC_OGROUP  |
    AAA..XADD   :    RETURN A_OGROUP   |
    _AND..XOR   :    RETURN L_OGROUP   |
    CMPS..XLATB :    RETURN SM_OGROUP  |
    BSF..BTS    :    RETURN BM_OGROUP  |
    CALL..RET   :    RETURN CT_OGROUP  |
    JA..LOOPZ   :    RETURN CJ_OGROUP  |
    ENTER..SETZ :    RETURN CBS_OGROUP |
    BOUND..IRETD:    RETURN I_OGROUP   |
    HLT..WAIT   :    RETURN PC_OGROUP  |
    F2XM1..FYL2XP1:  RETURN F_OGROUP   |
    ARPL..WRMSR :    RETURN LPC_OGROUP |
  END
END GetOpGroup;


TYPE
  Conditions = D.Condition[D.JO..D.JG];
  J2J = ARRAY Conditions OF OpCode;
CONST J2JConsts* = J2J{
        JO,JNO,JB,JAE,JE,JNE,JBE,JA,JS,JNS,JPE,JPO,JL,JGE,JLE,JG};

TYPE  J2F = ARRAY Conditions OF Flags;
CONST J2FConsts* = J2F{
          {_OF},{_OF},{CF},{CF},{ZF},{ZF},{ZF,CF},{ZF,CF},{SF},{SF},
          {PF}, {PF}, {SF,_OF}, {SF,_OF}, {ZF,SF,_OF},{ZF,SF,_OF}};

PROCEDURE GetJumpUsedFlags*(j : D.Condition) : Flags;
BEGIN
  IF j<=D.JG THEN RETURN J2FConsts[j];ELSE RETURN Flags{}; END
END GetJumpUsedFlags;

TYPE  Set2Set = ARRAY Conditions OF OpCode;
CONST Set2SetConsts* = Set2Set{
          SETO,SETNO,SETB,SETAE,SETE,SETNE,SETBE,SETA,SETS,SETNS,SETP,SETNP,
          SETL,SETGE,SETLE,SETG};

TYPE  Set2F = ARRAY Conditions OF Flags;
CONST Set2FConsts* = Set2F{
          {_OF},{_OF},{CF},{CF},{ZF},{ZF},{CF,ZF},{CF,ZF},{SF},{SF},
          {PF},{PF},{SF,_OF},{SF,_OF},{ZF,SF,_OF},{ZF,SF,_OF}};

TYPE
  BinaryOps = D.BinaryOp[D.TTT_add..D.TTT_last];
  BOp2BOp = ARRAY BinaryOps OF OpCode;
CONST BOp2BOpConsts* =
      BOp2BOp{ADD,_OR,ADC,SBB,_AND,SUB,XOR,CMP,BT, BTS, BTR};


TYPE
  UnaryOps = D.UnaryOp[D.TTT_inc..D.TTT_neg];
  UOp2UOp = ARRAY UnaryOps OF OpCode;
CONST UOp2UOpConsts* = UOp2UOp{_INC,_DEC,NEG,_NOT};

TYPE  UOp2F = ARRAY UnaryOps OF Flags;
CONST UOp2FConsts* = UOp2F{AFLAGS_NO_CF,AFLAGS_NO_CF,ALL_AFLAGS,ALL_AFLAGS};

TYPE
  ShiftOps = D.ShiftOp[D.TTT_rol..D.TTT_sar];
  Shift2Shift = ARRAY ShiftOps OF OpCode;
CONST Shift2ShiftConsts* = Shift2Shift{
          ROL,ROR,UNDEF_OP,UNDEF_OP,SHL,SHR,UNDEF_OP,SAR};

TYPE
  Shift2F = ARRAY ShiftOps OF Flags;
CONST Shift2FConsts* = Shift2F{
          {_OF,CF},{_OF,CF},{},{},ALL_AFLAGS,ALL_AFLAGS,{},ALL_AFLAGS};

PROCEDURE GetFOpCode*(op : D.FloatOp; P : BOOLEAN): OpCode;
BEGIN
   CASE op OF
   | D.FADD  : IF P THEN RETURN FADDP  ELSE RETURN FADD  END
   | D.FMUL  : IF P THEN RETURN FMULP  ELSE RETURN FMUL  END
   | D.FDIV  : IF P THEN RETURN FDIVP  ELSE RETURN FDIV  END
   | D.FDIVR : IF P THEN RETURN FDIVRP ELSE RETURN FDIVR END
   | D.FSUB  : IF P THEN RETURN FSUBP  ELSE RETURN FSUB  END
   | D.FSUBR : IF P THEN RETURN FSUBRP ELSE RETURN FSUB  END
   | D.FXCH  : RETURN FXCH
   | D.FABS  : RETURN FABS
   | D.FCHS  : RETURN FCHS
   | D.FLD1  : RETURN FLD1
   | D.FLDL2E: RETURN FLDL2E
   | D.FLDPI : RETURN FLDPI
   | D.FLDLG2: RETURN FLDLG2
   | D.FLDLN2: RETURN FLDLN2
   | D.FLDZ  : RETURN FLDZ
   | D.F2XM1 : RETURN F2XM1
   | D.FYL2X : RETURN FYL2X
   | D.FPTAN : RETURN FPTAN
   | D.FPATAN: RETURN FPATAN
   | D.FPREM : RETURN FPREM
   | D.FSQRT : RETURN FSQRT
   | D.FSCALE: RETURN FSCALE
   | D.FSIN  : RETURN FSIN
   | D.FCOS  : RETURN FCOS
   END;
END GetFOpCode;

------------------------------------------------------------------------------

                (* definition of operation operands *)

TYPE  OpOpndKind* = SHORTINT;

CONST (* operation operand kinds *)
      UNDEF_OPND* = OpOpndKind{ 0 }; UNDEF_OPND_CODE* = Register{ -1 };
      REG*        = OpOpndKind{ 1 };
      MEM*        = OpOpndKind{ 2 }; MEM_OPND_CODE* = Register{ -2 };
      IMM*        = OpOpndKind{ 3 }; IMM_OPND_CODE* = Register{ -3 };

PROCEDURE GetOpOpndKind*(r: Register) : OpOpndKind;
BEGIN
    CASE r OF
    | UNDEF_OPND_CODE : RETURN UNDEF_OPND;
    | MEM_OPND_CODE   : RETURN MEM;
    | IMM_OPND_CODE   : RETURN IMM;
    ELSE RETURN REG;
    END;
END GetOpOpndKind;

TYPE
      AddrMode*   = Emit.AddrMode;

      OpOpnd*     = RECORD
                        r*      : Register;
                        a*      : AddrMode; (* in the case r is used as memory size *)
                    END;
CONST EmptyAddrMode* = AddrMode{
          Emit.RegPlacement{ir.UNDEFINED,D.UNDEF_REG},
          Emit.RegPlacement{ir.UNDEFINED,D.UNDEF_REG},
          D.ScaleType{0},
          0,
          ir.UNDEFINED,
          NIL,
          ir.ProcUNDEFINED,
          NIL,
          NIL};

------------------------------------------------------------------------------
               (* definition of operation attributes *)

VAR      ENABLE_OPTIMIZATION* : BOOLEAN;
         ENABLE_MOVE_STORE*   : BOOLEAN;

CONST    UNDEF_POS* = -1;
VAR      CURRENT_TPOS* : ir.TPOS;

CONST    UNDEF_CLOCKS* = 127;
         BAD_MEM*      = VAL(ir.VarNum, ORD(ir.UNDEFINED)*2);

                --------------------------------------
                           (* pairing tags*)

TYPE
    PairTag*   = (
        FP,
        FNP,
        UV,
        PU,
        PV,
        NP,
        UNDEF_PAIR
    );
    PairTags*  = PACKEDSET OF PairTag;
CONST

TYPE  BOp2Pair = ARRAY BinaryOps OF PairTag;
CONST BOp2PairConsts* = BOp2Pair{UV,UV,PU,PU,UV,UV,UV,UV,UV, UV, UV};

TYPE  UOp2Pair = ARRAY UnaryOps OF PairTag;
CONST UOp2PairConsts* = UOp2Pair{UV,UV,NP,NP};

------------------------------------------------------------------------------
                 (* operation arguments and results *)

TYPE
         RegsInAddr* = Regs;

         Vars*       = RECORD
                           l*  : ir.Local;
                           vs* : b.BitVector;
                       END;

         OpArg*      = RECORD
                           r*  : Regs;
                           ra* : RegsInAddr;
                           f*  : Flags;
                           v*  : Vars;
                           a*  : AddrMode;
                       END;

         OpRes*      = RECORD
                           r* : Regs;
                           f* : Flags;
                           v* : Vars;
                           a* : AddrMode;
                       END;

CONST
         HAS_IN*         = 0;  (* operation has in arcs *)
         HAS_OUT*        = 1;  (* operation has out arcs *)
         USED_MEM*       = 2;  (* operation uses memory location *)
         SET_MEM*        = 3;  (* operation sets memory location *)
         DANG_OP*        = 4;  (* dangerous operation ---
                                  cannot be changed with another
                                  dangerous operation
                               *)
         MOV_CONTROL_TRANSFER* = 5;
                               (* operations can be passed thru the
                                  transfer operation
                               *)
         SET_USED_FLAGS* = 6;  (* operation sets used flags *)
         NOT_MOVABLE*    = 7;  (* the operation shuld be in the same place
                                  as in initial fragment *)
         MOV_ESP_LOCAL*  = 8;  (* mov m,_ or mov _,m where
                                  m is a 'good' local likely offset[esp]
                               *)
         IsPush_R_OR_I*  = 9;  (* is the operation push reg/imm *)
         IsPop_R*        = 10; (* is the operation pop reg *)
         IsJcc*          = 11; (* is the operation Jcc *)
         WITH_AGI*       = 12; (* is the operation causes agi
                                  in initial segment
                               *)
         ALREADY_REMOVED*= 13; (* the operation already passed from
                                  previous node - do't pass it further
                               *)
         NOT_TRANSFER_LOCAL_MOVE* = 14;
                               (* move with local memory result
                                  couldn't be passed thru the
                                  operation
                               *)

         LAST_TAG_ATTR*  = NOT_TRANSFER_LOCAL_MOVE;
TYPE
         OpTagAttrs*=  SET;

         OpAttrs*  = RECORD
                        arg*       : OpArg;
                        res*       : OpRes;
                        tag_attrs* : OpTagAttrs;
                        clocks*    : SHORTINT; (* latency *)
                        pair*      : PairTag;  (* pairing tag *)
                        chain*,                (* max length of dep chain *)
                        ready*     : INTEGER;  (* number of operations which should
                                                be emitted before emission of the
                                                operation *)
                        cycle*     : LONGINT;  (* first cycle in which the operation
                                                can be performed without delay *)
                        used_mem*  : SHORTINT;
                     END;

CONST EmptyAttrs* = OpAttrs{
           OpArg{{},{},{},Vars{ir.UNDEFINED,NIL},EmptyAddrMode},
           OpRes{{},{},   Vars{ir.UNDEFINED,NIL},EmptyAddrMode},
           {},
           UNDEF_CLOCKS,
           UNDEF_PAIR,
           -1,
           0,
           0,
           0};


                --------------------------------------
                    (* definition of operations *)

TYPE
  (* все операции *)
  BinRecipe*     = POINTER TO BinRecipeRec;
  BinRecipeRec*  = RECORD
                   END;

PROCEDURE (self : BinRecipe) ToBin*();
BEGIN
   ASSERT(FALSE);
END ToBin;

PROCEDURE (self : BinRecipe) GetVal*() : LONGINT;
BEGIN
   ASSERT(FALSE);
END GetVal;

TYPE
  (* операции, которые читают из памяти *)
  BinRecipeR*    = POINTER TO BinRecipeRRec;
  BinRecipeRRec* = RECORD (BinRecipeRec)
                   END;

PROCEDURE (self : BinRecipeR) ReadMem*(VAR r : AddrMode);
BEGIN
   ASSERT(FALSE);
END ReadMem;

PROCEDURE (self : BinRecipeR) GetReadSz*() : SizeType;
BEGIN
    ASSERT(FALSE);
END GetReadSz;

TYPE
  (* операции, которые читают из вещественной памяти *)
  BinRecipeFloatR*      = POINTER TO BinRecipeFloatRRec;
  BinRecipeFloatRRec*   = RECORD (BinRecipeRRec)
                          END;

TYPE
  (* операции, которые пишут в память *)
  BinRecipeW*    = POINTER TO BinRecipeWRec;
  BinRecipeWRec* = RECORD (BinRecipeRec)
                   END;

PROCEDURE (self : BinRecipeW) WriteMem*(VAR w : AddrMode);
BEGIN
   ASSERT(FALSE);
END WriteMem;

PROCEDURE (self : BinRecipeW) GetWriteSz*() : SizeType;
BEGIN
    ASSERT(FALSE);
END GetWriteSz;

TYPE
  (* операции, которые пишут в вещественную память *)
  BinRecipeFloatW*      = POINTER TO BinRecipeFloatWRec;
  BinRecipeFloatWRec*   = RECORD (BinRecipeWRec)
                          END;

TYPE
  (* операции, которые и пишут и читают *)
  BinRecipeRW*   = POINTER TO BinRecipeRWRec;
  BinRecipeRWRec*= RECORD (BinRecipeRec)
                   END;

PROCEDURE (self : BinRecipeRW) ReadMem*(VAR r : AddrMode);
BEGIN
   ASSERT(FALSE);
END ReadMem;

PROCEDURE (self : BinRecipeRW) WriteMem*(VAR r : AddrMode);
BEGIN
   ASSERT(FALSE);
END WriteMem;

PROCEDURE (self : BinRecipeRW) GetReadSz*() : SizeType;
BEGIN
    ASSERT(FALSE);
END GetReadSz;

PROCEDURE (self : BinRecipeRW) GetWriteSz*() : SizeType;
BEGIN
    ASSERT(FALSE);
END GetWriteSz;


TYPE
  Operation*    = RECORD
                      bin*    : BinRecipe;(* object used to convert
                                             internal representation of
                                             operation into the object code
                                          *)
                      pos*    : INTEGER;  (* initial position in segment
                                             can be changed after reordering
                                          *)
                      tpos*   : ir.TPOS;  (* used for Emit.AddPosition *)
                      prefix* : OpPrefix;
                      code*   : OpCode;
                      dest*,
                      src*    : OpOpnd;
                      attrs*  : OpAttrs;
                      PUSH_SIZE*,
                      NEXT_PUSH_SIZE*,
                      FLOAT_SIZE*
                              : ir.INT;
                  END;

  INT_SCALE*  = POINTER TO ARRAY OF INTEGER;

CONST UNDEF_SIZE* = -1;

PROCEDURE GetReadSz*(op : Operation) : SizeType;
VAR bin : BinRecipe;
BEGIN
    bin := op.bin;
    WITH
      bin : BinRecipeFloatR DO
        RETURN bin.GetReadSz();
    | bin : BinRecipeR DO
        RETURN bin.GetReadSz();
    | bin : BinRecipeRW DO
        RETURN bin.GetReadSz();
    ELSE
        RETURN UNDEF_SIZE;
    END;
END GetReadSz;

PROCEDURE GetWriteSz*(op : Operation) : SizeType;
VAR bin : BinRecipe;
BEGIN
    bin := op.bin;
   WITH
     bin : BinRecipeFloatW DO
       RETURN bin.GetWriteSz();
   | bin : BinRecipeW DO
       RETURN bin.GetWriteSz();
   | bin : BinRecipeRW DO
       RETURN bin.GetWriteSz();
   ELSE
       RETURN UNDEF_SIZE;
   END;
END GetWriteSz;

PROCEDURE CheckOpTagAttr*(self : Operation; tag : SHORTINT) : BOOLEAN;
BEGIN
   RETURN tag IN self.attrs.tag_attrs;
END CheckOpTagAttr;

PROCEDURE InitOperation*(VAR self : Operation;
                             bin  : BinRecipe;
                             code : OpCode);
BEGIN
  self := Operation{
      bin,
      0,
      CURRENT_TPOS,
      {},
      code,
      OpOpnd{SYSTEM.VAL(Register,UNDEF_OPND_CODE),EmptyAddrMode},
      OpOpnd{SYSTEM.VAL(Register,UNDEF_OPND_CODE),EmptyAddrMode},
      EmptyAttrs,
      Emit.PushSize,
      Emit.PushSize,
      Emit.FloatSize};
  IF NOT ENABLE_OPTIMIZATION THEN INCL(self.attrs.tag_attrs,NOT_MOVABLE); END;
  IF NOT ENABLE_MOVE_STORE THEN INCL(self.attrs.tag_attrs, NOT_TRANSFER_LOCAL_MOVE); END;
END InitOperation;

PROCEDURE GetLocal*(a- : Emit.AddrMode) : ir.Local;
VAR k, l : ir.Local;
BEGIN
   l := a.local;
   IF (a.proc <> ir.ProcUNDEFINED) OR
      (a.offslist <> NIL)   OR (a.proclist <> NIL)
   THEN
      RETURN ir.UNDEFINED;
   END;
   IF l = ir.UNDEFINED THEN
      IF (a.bv = NIL) OR b.In (a.bv, ORD(Color.NLocals)) THEN
         RETURN ir.UNDEFINED;
      END;
      FOR k:=ir.ZEROVarNum TO SYSTEM.PRED(Color.NLocals) DO
        IF b.In (a.bv, ORD(k)) THEN
           IF l = ir.UNDEFINED THEN
              l := k;
           ELSE
              RETURN ir.UNDEFINED;
           END;
        END;
      END;
   END;
   RETURN l;
END GetLocal;

PROCEDURE EvalAddrMode(a- : Emit.AddrMode) : ir.Local;
VAR l : ir.Local;
BEGIN
    l := GetLocal(a);
    IF l = ir.UNDEFINED THEN RETURN BAD_MEM; END;
    RETURN l;
(*
   IF ((a.r1.r = Emit.BASE_REG)  AND
       (a.r2.r = D.UNDEF_REG) AND
       (NOT ir.IsExternal(l)))
      OR
      ((a.r1.r = D.UNDEF_REG)  AND
       (a.r2.r = D.UNDEF_REG)  AND
       (ir.IsExternal(l))) THEN
       RETURN l;
   ELSE
       RETURN BAD_MEM;
   END;
*)
END EvalAddrMode;

PROCEDURE AddOpOpndArg*(o-: OpOpnd; VAR a : OpAttrs);
BEGIN
    IF o.a.place1.r <> UNDEF_REG THEN
        INCL(a.arg.r,  D.RegInfo[o.a.place1.r].code);
        INCL(a.arg.ra, D.RegInfo[o.a.place1.r].code);
    END;
    IF o.a.place2.r <> UNDEF_REG THEN
        INCL(a.arg.r,  D.RegInfo[o.a.place2.r].code);
        INCL(a.arg.ra, D.RegInfo[o.a.place2.r].code);
    END;
END AddOpOpndArg;

PROCEDURE AddOpOpndArgAll*(o-: OpOpnd; VAR a : OpAttrs);
BEGIN
   CASE GetOpOpndKind(o.r) OF
     | REG : INCL(a.arg.r, o.r);
     | MEM : IF o.a.place1.r <> UNDEF_REG THEN
                 INCL(a.arg.r,  D.RegInfo[o.a.place1.r].code);
                 INCL(a.arg.ra, D.RegInfo[o.a.place1.r].code);
             END;
             IF o.a.place2.r <> UNDEF_REG THEN
                 INCL(a.arg.r,  D.RegInfo[o.a.place2.r].code);
                 INCL(a.arg.ra, D.RegInfo[o.a.place2.r].code);
             END;
             a.arg.v.l := EvalAddrMode(o.a);
             a.arg.a   := o.a;
   ELSE
   END;
END AddOpOpndArgAll;

PROCEDURE AddDestRes*(VAR self : Operation);
BEGIN
   CASE GetOpOpndKind(self.dest.r) OF
     | REG        : INCL(self.attrs.res.r, self.dest.r);
     | MEM        : self.attrs.res.v.l := EvalAddrMode(self.dest.a);
                    self.attrs.res.a := self.dest.a;
   ELSE
   END;
END AddDestRes;

PROCEDURE AddAssignOpArg*(VAR self : Operation);
BEGIN
   AddOpOpndArg(self.dest, self.attrs);
   AddOpOpndArgAll(self.src, self.attrs);
END AddAssignOpArg;

PROCEDURE AddAssignOpArgRes*(VAR self : Operation);
BEGIN
   AddOpOpndArg(self.dest, self.attrs);
   AddOpOpndArgAll(self.src, self.attrs);
   AddDestRes(self);
END AddAssignOpArgRes;

PROCEDURE AddOpArgV*(VAR self : Operation; vs: b.BitVector);
BEGIN
    IF self.attrs.arg.v.vs = NIL THEN
        self.attrs.arg.v.vs := b.New(ORD(Color.NLocals)+1,FALSE);
    END;
    b.Union(self.attrs.arg.v.vs, vs, self.attrs.arg.v.vs);
END AddOpArgV;

PROCEDURE AddOpResV*(VAR self : Operation; vs : b.BitVector);
BEGIN
    IF self.attrs.res.v.vs = NIL THEN
        self.attrs.res.v.vs := b.New(ORD(Color.NLocals)+1,FALSE);
    END;
    b.Union(self.attrs.res.v.vs, vs, self.attrs.res.v.vs);
END AddOpResV;

PROCEDURE AddXchgOpArgRes*(VAR self : Operation);
VAR o1 : Operation;
BEGIN
   AddOpOpndArg(self.dest, self.attrs);
   AddOpOpndArgAll(self.src, self.attrs);
   AddDestRes(self);
   InitOperation(o1,NIL,UNDEF_OP);
   o1.dest  := self.src;
   o1.src   := self.dest;
   AddOpOpndArg(o1.dest, self.attrs);
   AddOpOpndArgAll(o1.src, o1.attrs);
   AddDestRes(o1);
   self.attrs.arg.r := self.attrs.arg.r + o1.attrs.arg.r;
   self.attrs.arg.ra := self.attrs.arg.ra + o1.attrs. arg.ra;
   self.attrs.arg.f := self.attrs.arg.f + o1.attrs.arg.f;
   IF o1.attrs.arg.v.vs <> NIL THEN
       AddOpArgV(self, o1.attrs.arg.v.vs);
   END;
   self.attrs.res.r := self.attrs.res.r + o1.attrs.res.r;
   self.attrs.res.f := self.attrs.res.f + o1.attrs.res.f;
   IF o1.attrs.res.v.vs <> NIL THEN AddOpResV(self, o1.attrs.res.v.vs); END;
   b.Free(o1.attrs.arg.v.vs);
   b.Free(o1.attrs.res.v.vs);
END AddXchgOpArgRes;

PROCEDURE CheckLEA*(VAR op : Operation);
BEGIN
   IF (op.dest.r <> ESP) & (op.src.a.place1.r = D.ESP) & (op.src.a.place2.r = UNDEF_REG)
   THEN INCL(op.attrs.tag_attrs, MOV_ESP_LOCAL);
   END;
END CheckLEA;

PROCEDURE CheckMoveLocal*(VAR op : Operation);
BEGIN
   IF op.dest.r = MEM_OPND_CODE THEN
       IF (op.src.r <> ESP) &
          (op.dest.a.place1.r = D.ESP) &
          (op.dest.a.place2.r = UNDEF_REG) &
          (op.attrs.res.v.l >= ir.ZEROVarNum) &
          (NOT ir.IsExternal(op.attrs.res.v.l))
       THEN INCL(op.attrs.tag_attrs, MOV_ESP_LOCAL);
       END;
   ELSE
       IF (op.dest.r <> ESP) &
          (op.src.a.place1.r = D.ESP) &
          (op.src.a.place2.r = UNDEF_REG) &
          (op.attrs.arg.v.l >= ir.ZEROVarNum) &
          (NOT ir.IsExternal(op.attrs.arg.v.l))
       THEN INCL(op.attrs.tag_attrs, MOV_ESP_LOCAL);
       END;
   END;
END CheckMoveLocal;

PROCEDURE GetFOpAttrs*(      op : D.FloatOp;
                       VAR code   : OpCode;
                       VAR clocks : SHORTINT;
                       VAR pair   : PairTag;
                       VAR ArgR,
                           ResR   : Regs;
                       VAR ResF   : Flags);
BEGIN
    CASE op OF
    | D.FXCH   : code := FXCH; clocks := 1; pair := FNP;
                    ArgR := Regs{ST0,ST1};
                    ResR := Regs{ST0,ST1};
                    ResF := Flags{C1};
    | D.FABS   : code := FABS; clocks := 1; pair := FP;
                    ArgR := Regs{ST0};
                    ResR := Regs{ST0};
                    ResF := Flags{C1};
    | D.FCHS   : code := FCHS; clocks := 1; pair := FP;
                    ArgR := Regs{ST0};
                    ResR := Regs{ST0};
                    ResF := Flags{C1};
    | D.FLD1   : code := FLD1; clocks := 2; pair := FNP;
                    ArgR := Regs{ST0..ST7};
                    ResR := Regs{ST0..ST7};
                    ResF := Flags{C1};
    | D.FLDL2E : code := FLDL2E; clocks := 5; pair := FNP;
                    ArgR := Regs{ST0..ST7};
                    ResR := Regs{ST0..ST7};
                    ResF := Flags{C1};
    | D.FLDPI  : code := FLDPI; clocks := 5; pair := FNP;
                    ArgR := Regs{ST0..ST7};
                    ResR := Regs{ST0..ST7};
                    ResF := Flags{C1};
    | D.FLDLG2 : code := FLDLG2; clocks := 5; pair := FNP;
                    ArgR := Regs{ST0..ST7};
                    ResR := Regs{ST0..ST7};
                    ResF := Flags{C1};
    | D.FLDLN2 : code := FLDLN2; clocks := 5; pair := FNP;
                    ArgR := Regs{ST0..ST7};
                    ResR := Regs{ST0..ST7};
                    ResF := Flags{C1};
    | D.FLDZ   : code := FLDZ; clocks := 2; pair := FNP;
                    ArgR := Regs{ST0..ST7};
                    ResR := Regs{ST0..ST7};
                    ResF := Flags{C1};
    | D.F2XM1  : code := F2XM1; clocks := 13; pair := FNP;
                    ArgR := Regs{ST0};
                    ResR := Regs{ST0};
                    ResF := Flags{C1};
    | D.FYL2X  : code := FYL2X; clocks := 22; pair := FNP;
                    ArgR := Regs{ST0, ST1};
                    ResR := Regs{ST0..ST7};
                    ResF := Flags{C1};
    | D.FPTAN  : code := FPTAN; clocks := 17; pair := FNP;
                    ArgR := Regs{ST0};
                    ResR := Regs{ST0..ST7};
                    ResF := Flags{C1, C2};
    | D.FPATAN : code := FPATAN; clocks := 17; pair := FNP;
                    ArgR := Regs{ST0, ST1};
                    ResR := Regs{ST0..ST7};
                    ResF := Flags{C1, C2};
    | D.FPREM  : code := FPREM; clocks := 16; pair := FNP;
                    ArgR := Regs{ST0, ST1};
                    ResR := Regs{ST0};
                    ResF := ALL_CFLAGS;
    | D.FSQRT  : code := FSQRT; clocks := 70; pair := FNP;
                    ArgR := Regs{ST0};
                    ResR := Regs{ST0};
                    ResF := Flags{C1};
    | D.FSCALE : code := FSCALE; clocks := 20; pair := FNP;
                    ArgR := Regs{ST0};
                    ResR := Regs{ST0};
                    ResF := Flags{C1};
    | D.FCOS   : code := FCOS; clocks := 18; pair := FNP;
                    ArgR := Regs{ST0};
                    ResR := Regs{ST0};
                    ResF := Flags{C1, C2};
    | D.FSIN   : code := FSIN; clocks := 16; pair := FNP;
                    ArgR := Regs{ST0};
                    ResR := Regs{ST0};
                    ResF := Flags{C1, C2};
    END;
END GetFOpAttrs;

PROCEDURE ClearOpAttrs*(VAR self : Operation);
BEGIN
   b.Free(self.attrs.arg.v.vs);
   b.Free(self.attrs.res.v.vs);
END ClearOpAttrs;

PROCEDURE GetFLOAT_SIZE_delta*(code : OpCode) : SHORTINT;
BEGIN
    CASE code OF
    | FLD,    FILD,
      FLD1,   FLDL2E,
      FLDPI,  FLDLG2,
      FLDLN2, FLDZ,
      FPTAN                 : RETURN 1;
    | FYL2X,  FYL2XP1,
      FPATAN,
      FSTP,   FADDP,
      FMULP,  FSUBP,
      FSUBRP, FDIVP,
      FDIVRP, FISTP,
      FCOMP,  FICOMP        : RETURN -1;
    ELSE                      RETURN 0;
    END;
END GetFLOAT_SIZE_delta;

PROCEDURE IsLongFloat*(code : OpCode) : BOOLEAN;
BEGIN
    CASE code OF
    | FLD, FLD1, FLDZ,
      FST, FSTP        : RETURN FALSE;
    ELSE
      RETURN GetOpGroup(code) = F_OGROUP;
    END;
END IsLongFloat;

PROCEDURE SetOpR_R*(VAR op : Operation; d,s : Register);
BEGIN
  op.dest.r := d; op.src.r := s;
END SetOpR_R;

PROCEDURE SetOpR_M*(VAR op : Operation; r : Register; a- : AddrMode);
BEGIN
  op.dest.r := r; op.src := OpOpnd{SYSTEM.VAL(Register,MEM_OPND_CODE),a};
END SetOpR_M;

BEGIN
    ENABLE_OPTIMIZATION    := TRUE;
    ENABLE_MOVE_STORE      := TRUE;
END OcirDef.

