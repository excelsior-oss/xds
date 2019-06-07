MODULE desc386;

<* +o2addkwd *>
FROM SYSTEM IMPORT PRED,SUCC;
IMPORT at := opAttrs;
IMPORT ir;
IMPORT SYSTEM;

TYPE
   Reg *=(  --can't be changed!
         EAX,     ECX,     EDX,     EBX,     ESP,      EBP,     ESI,     EDI,
         ST0,     ST1,     ST2,     ST3,     ST4,      ST5,     ST6,     ST7,
         AX,      CX,      DX,      BX,      SP,       BP,      SI,      DI,
         AL,      CL,      DL,      BL,      AH,       CH,      DH,      BH,
         SPILLED1,SPILLED2,SPILLED4,SPILLED8,UNDEF_REG,BPlow,   SIlow,   DIlow,

            --can   be changed!                                 \
         ECX_EAX,
         EDX_EAX,
         EBX_EAX,
         EBP_EAX,
         ESI_EAX,
         EDI_EAX,

         EDX_ECX,
         EBX_ECX,
         EBP_ECX,
         ESI_ECX,
         EDI_ECX,

         EBX_EDX,
         EBP_EDX,
         ESI_EDX,
         EDI_EDX,

         EBP_EBX,
         ESI_EBX,
         EDI_EBX,

         ESI_EBP,
         EDI_EBP,

         EDI_ESI
         );

        RegSet *= PACKEDSET OF Reg;

   PhysReg *= SHORTINT;
   PhysRegSet *= PACKEDSET OF PhysReg;
CONST
   EAXp *= PhysReg{ 0 };
   ECXp *= PhysReg{ 1 };
   EDXp *= PhysReg{ 2 };
   EBXp *= PhysReg{ 3 };
   ESPp *= PhysReg{ 4 };
   EBPp *= PhysReg{ 5 };
   ESIp *= PhysReg{ 6 };
   EDIp *= PhysReg{ 7 };


   ST0p *= PhysReg{ 8 };
   ST1p *= PhysReg{ 9 };
   ST2p *= PhysReg{ 10 };
   ST3p *= PhysReg{ 11 };
   ST4p *= PhysReg{ 12 };
   ST5p *= PhysReg{ 13 };
   ST6p *= PhysReg{ 14 };
   ST7p *= PhysReg{ 15 };

   SPILLED1p *= PhysReg{ ORD(SPILLED1) };
   SPILLED2p *= PhysReg{ ORD(SPILLED2) };
   SPILLED4p *= PhysReg{ ORD(SPILLED4) };
   UNDEF_REGp *= PhysReg{ ORD(UNDEF_REG) };
   SPILLED8phi *= PhysReg{ ORD(BPlow) };
   SPILLED8plo *= PhysReg{ ORD(SIlow) };

    ALp  *= EAXp; CLp  *= ECXp; DLp  *= EDXp; BLp  *= EBXp;
    AHp  *= ESPp; CHp  *= EBPp; DHp  *= ESIp; BHp  *= EDIp;
    AXp  *= EAXp; CXp  *= ECXp; DXp  *= EDXp; BXp  *= EBXp;
    SPp  *= ESPp; BPp  *= EBPp; SIp  *= ESIp; DIp  *= EDIp;
TYPE
(*   Reg *=
        (EAX,     ECX,     EDX,     EBX,     ESP,     EBP,     ESI,     EDI,
         ECX_EAX, _a,      _b,      _c,      AL,      CL,      DL,      BL,
         EDX_EAX, EDX_ECX, _d,      _e,      AH,      CH,      DH,      BH,
         EBX_EAX, EBX_ECX, EBX_EDX, _f,      SPILLED1,SPILLED2,SPILLED4,SPILLED8,
         AX,      CX,      DX,      BX,      _k,      BP,      SI,      DI,
         EBP_EAX, EBP_ECX, EBP_EDX, EBP_EBX, _g,      _h,      _i,      _j,
         ESI_EAX, ESI_ECX, ESI_EDX, ESI_EBX, _l,      ESI_EBP, _m,      _n,
         EDI_EAX, EDI_ECX, EDI_EDX, EDI_EBX, _o,      EDI_EBP, EDI_ESI, UNDEF_REG
        );
*)
CONST
    MINREG *= MIN(Reg);
    MAXREG *= MAX(Reg);
    MIN8REG *= ECX_EAX;--EDX_EAX;
    MAX8REG *= EDI_ESI;--EDX_EAX;
    MIN4REG *= EAX;
    MAX4REG *= EDI;
    MIN2REG *= AX;
    MAX2REG *= DI;
    MIN1REG *= AL;
    MAX1REG *= BH;

    XREGS *= RegSet{EAX..EBX,AX..BX,AL..BL,
                    ECX_EAX, EDX_EAX, EBX_EAX, EBP_EAX, ESI_EAX, EDI_EAX,
                             EDX_ECX, EBX_ECX, EBP_ECX, ESI_ECX, EDI_ECX,
                                      EBX_EDX, EBP_EDX, ESI_EDX, EDI_EDX,
                                               EBP_EBX, ESI_EBX, EDI_EBX};
    REGS8 *= RegSet{SPILLED8,MIN8REG..MAX8REG};
TYPE
    Array9OfReg = ARRAY 9 OF Reg;
    Array4OfReg = ARRAY 4 OF Reg;
CONST
    SPILLED *= Array9OfReg{UNDEF_REG, SPILLED1, SPILLED2,UNDEF_REG,SPILLED4,
                           UNDEF_REG,UNDEF_REG,UNDEF_REG,SPILLED8};
    SPILLED2Pow *= Array4OfReg{SPILLED1, SPILLED2,SPILLED4,SPILLED8};
    SPILLED_REGS *= RegSet{SPILLED1,SPILLED2,SPILLED4,SPILLED8};
--    NIRegs *= 8;

VAR
    Pi4*:           ir.FLOAT;      -- для оптимизации использования
    Pi8*:           ir.FLOAT;      -- 3.141592653589793238462643383

TYPE
    ArrayRegOfRegSet = ARRAY Reg OF RegSet;

VAR
    PartsOf-         : ArrayRegOfRegSet;
    OwnersOf-        : ArrayRegOfRegSet;
    IntersectWith-   : ArrayRegOfRegSet;
    RegAndIntersectWith-   : ArrayRegOfRegSet;
    RegAndIntersectWith4Max-   : ArrayRegOfRegSet;
    SpecialIntersect-   : ArrayRegOfRegSet;
    RegInfo-         : ARRAY Reg OF RECORD
                          sz-  : ir.SizeType;
                          code-  : PhysReg;    -- internal number
                          codehi-: PhysReg;   -- int. number of high sign. part
                                              -- ( only for size  = 8 )
                          END;
TYPE
    ArrayRegOfArrayOfChar = ARRAY Reg OF ARRAY 9 OF CHAR;
CONST
    regName* = ArrayRegOfArrayOfChar
    {
         "EAX",     "ECX",     "EDX",     "EBX",     "ESP",     "EBP",     "ESI",     "EDI",
         "ST0",     "ST1",     "ST2",     "ST3",     "ST4",     "ST5",     "ST6",     "ST7",
         "AX",      "CX",      "DX",      "BX",      "SP",      "BP",      "SI",      "DI",
         "AL",      "CL",      "DL",      "BL",      "AH",      "CH",      "DH",      "BH",
         "SPILLED1","SPILLED2","SPILLED4","SPILLED8","???",     "BPlow",   "SIlow",   "DIlow",

         "ECX_EAX",
         "EDX_EAX",
         "EBX_EAX",
         "EBP_EAX",
         "ESI_EAX",
         "EDI_EAX",

         "EDX_ECX",
         "EBX_ECX",
         "EBP_ECX",
         "ESI_ECX",
         "EDI_ECX",

         "EBX_EDX",
         "EBP_EDX",
         "ESI_EDX",
         "EDI_EDX",
                 
         "EBP_EBX",
         "ESI_EBX",
         "EDI_EBX",

         "ESI_EBP",
         "EDI_EBP",

         "EDI_ESI"

    };

TYPE
    Reg4Range *= [EAX..EDI];
    PairType = ARRAY Reg4Range OF ARRAY Reg4Range OF Reg; --[hign,low]
CONST Pair *=  PairType{
        { UNDEF_REG, ECX_EAX,   EDX_EAX,   EBX_EAX,   UNDEF_REG, EBP_EAX,   ESI_EAX,   EDI_EAX},
        { ECX_EAX,   UNDEF_REG, EDX_ECX,   EBX_ECX  , UNDEF_REG, EBP_ECX  , ESI_ECX  , EDI_ECX  },
        { EDX_EAX,   EDX_ECX,   UNDEF_REG, EBX_EDX  , UNDEF_REG, EBP_EDX  , ESI_EDX  , EDI_EDX  },
        { EBX_EAX,   EBX_ECX,   EBX_EDX,   UNDEF_REG, UNDEF_REG, EBP_EBX  , ESI_EBX  , EDI_EBX  },
        { UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        { EBP_EAX,   EBP_ECX,   EBP_EDX,   EBP_EBX,   UNDEF_REG, UNDEF_REG, ESI_EBP  , EDI_EBP  },
        { ESI_EAX,   ESI_ECX,   ESI_EDX,   ESI_EBX,   UNDEF_REG, ESI_EBP,   UNDEF_REG, EDI_ESI  },
        { EDI_EAX,   EDI_ECX,   EDI_EDX,   EDI_EBX,   UNDEF_REG, EDI_EBP,   EDI_ESI,   UNDEF_REG}
    };

TYPE HPC = ARRAY Reg4Range OF RegSet;
CONST HighPartCand *= HPC
        { RegSet{ECX,EDX,EBX,EBP,ESI,EDI},
          RegSet{    EDX,EBX,EBP,ESI,EDI},
          RegSet{        EBX,EBP,ESI,EDI},
          RegSet{            EBP,ESI,EDI},
          RegSet{                       },
          RegSet{                ESI,EDI},
          RegSet{                    EDI},
          RegSet{                       }
        };

TYPE
    Reg1Range = [AL..BH];
--    BrotherType = ARRAY Reg1Range OF Reg;
--CONST
--    Brother *= BrotherType { AH, CH, DH, BH, AL, CL, DL, BL };

TYPE
    Reg8Range *= [SPILLED8..(*ECX_EAX..*)EDI_ESI];
    PartType = ARRAY Reg8Range OF Reg;
CONST
    HighPart *= PartType {
        SPILLED4, UNDEF_REG,UNDEF_REG,UNDEF_REG,UNDEF_REG,
        ECX, EDX, EBX, EBP, ESI, EDI,
             EDX, EBX, EBP, ESI, EDI,
                  EBX, EBP, ESI, EDI,
                       EBP, ESI, EDI,
                            ESI, EDI,
                                 EDI
    };
    LowPart *= PartType {
        SPILLED4, UNDEF_REG,UNDEF_REG,UNDEF_REG,UNDEF_REG,
        EAX, EAX, EAX, EAX, EAX, EAX,
             ECX, ECX, ECX, ECX, ECX,
                  EDX, EDX, EDX, EDX,
                       EBX, EBX, EBX,
                            EBP, EBP,
                                 ESI
    };

    RetFuncRegs * = RegSet{ AL, AX, EAX, EDX_EAX };
TYPE
    PhysPange = PhysReg[EAXp..SPILLED8plo];
    RegTableType = ARRAY PhysPange OF ARRAY 9 OF Reg;
CONST
    RegTable* = RegTableType
    {
        {UNDEF_REG, AL,    AX, UNDEF_REG, EAX, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, CL,    CX, UNDEF_REG, ECX, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, DL,    DX, UNDEF_REG, EDX, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, BL,    BX, UNDEF_REG, EBX, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, AH,    SP, UNDEF_REG, ESP, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, BPlow, BP, UNDEF_REG, EBP, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, SIlow, SI, UNDEF_REG, ESI, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, DIlow, DI, UNDEF_REG, EDI, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, SPILLED1,  SPILLED2,  UNDEF_REG, SPILLED4,  UNDEF_REG, UNDEF_REG, UNDEF_REG, SPILLED8},
        {UNDEF_REG, SPILLED1,  SPILLED2,  UNDEF_REG, SPILLED4,  UNDEF_REG, UNDEF_REG, UNDEF_REG, SPILLED8},
        {UNDEF_REG, SPILLED1,  SPILLED2,  UNDEF_REG, SPILLED4,  UNDEF_REG, UNDEF_REG, UNDEF_REG, SPILLED8},
        {UNDEF_REG, SPILLED1,  SPILLED2,  UNDEF_REG, SPILLED4,  UNDEF_REG, UNDEF_REG, UNDEF_REG, SPILLED8},
        {UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG, UNDEF_REG},
        {UNDEF_REG, SPILLED1,  SPILLED2,  UNDEF_REG, SPILLED4,  UNDEF_REG, UNDEF_REG, UNDEF_REG, SPILLED8},
        {UNDEF_REG, SPILLED1,  SPILLED2,  UNDEF_REG, SPILLED4,  UNDEF_REG, UNDEF_REG, UNDEF_REG, SPILLED8}
         };
TYPE
    CallResRegType = ARRAY 9 OF Reg;
CONST
    CallResReg* = CallResRegType
      {UNDEF_REG, AL, AX, UNDEF_REG, EAX, UNDEF_REG, UNDEF_REG, UNDEF_REG, EDX_EAX};
TYPE
    RegsOfSize = ARRAY 9 OF RegSet;

VAR allowedIRegs *: RegsOfSize;
    allIRegs*:      RegSet;

CONST
    AllowedIRegsWEBP *= RegsOfSize            -- Допустимые регистры
                    {                       -- в зависимости от длины
                        {}                      (* 0 *)
                      , { AL..BL(*, AH..BH, BPlow..DIlow*) }              (* 1 *)
                      , { AX..BX, BP..DI   }              (* 2 *)
                      , {}                      (* 3 *)
                      , { EAX..EBX, EBP..EDI }            (* 4 *)
                      , {}                      (* 5 *)
                      , {}                      (* 6 *)
                      , {}                      (* 7 *)
                      , { MIN8REG..MAX8REG }                 (* 8 *)
                    };
    AllowedIRegsWoEBP *= RegsOfSize
        {RegSet {},
         RegSet {AL..BL(*, AH..BH, BPlow..DIlow*)},
         RegSet {AX..BX, SI..DI},
         RegSet {},
         RegSet {EAX..EBX, ESI..EDI},
         RegSet {},
         RegSet {},
         RegSet {},
         RegSet {ECX_EAX..EBX_EAX,
                 ESI_EAX..EBX_ECX,
                 ESI_ECX..EBX_EDX,
                 ESI_EDX, EDI_EDX,
                 ESI_EBX, EDI_EBX,
                 EDI_ESI                }
        };
    AllowedIRegsWEBPWSpilled *= RegsOfSize            -- Допустимые регистры
                    {                       -- в зависимости от длины
                        {}                      (* 0 *)
                      , AllowedIRegsWEBP[1]+RegSet{SPILLED1}              (* 1 *)
                      , AllowedIRegsWEBP[2]+RegSet{SPILLED2}              (* 2 *)
                      , {}                      (* 3 *)
                      , AllowedIRegsWEBP[4]+RegSet{SPILLED4}            (* 4 *)
                      , {}                      (* 5 *)
                      , {}                      (* 6 *)
                      , {}                      (* 7 *)
                      , AllowedIRegsWEBP[8]+RegSet{SPILLED8}                 (* 8 *)
                    };
    AllowedIRegsWoEBPWUNDEF *= RegsOfSize            -- Допустимые регистры
                    {                       -- в зависимости от длины
                        {}                      (* 0 *)
                      , AllowedIRegsWoEBP[1]+RegSet{UNDEF_REG}              (* 1 *)
                      , AllowedIRegsWoEBP[2]+RegSet{UNDEF_REG}              (* 2 *)
                      , {}                      (* 3 *)
                      , AllowedIRegsWoEBP[4]+RegSet{UNDEF_REG}            (* 4 *)
                      , {}                      (* 5 *)
                      , {}                      (* 6 *)
                      , {}                      (* 7 *)
                      , AllowedIRegsWoEBP[8]+RegSet{UNDEF_REG}                 (* 8 *)
                    };
    AllowedIRegsWEBPWUNDEF *= RegsOfSize            -- Допустимые регистры
                    {                       -- в зависимости от длины
                        {}                      (* 0 *)
                      , AllowedIRegsWEBP[1]+RegSet{UNDEF_REG}              (* 1 *)
                      , AllowedIRegsWEBP[2]+RegSet{UNDEF_REG}              (* 2 *)
                      , {}                      (* 3 *)
                      , AllowedIRegsWEBP[4]+RegSet{UNDEF_REG}            (* 4 *)
                      , {}                      (* 5 *)
                      , {}                      (* 6 *)
                      , {}                      (* 7 *)
                      , AllowedIRegsWEBP[8]+RegSet{UNDEF_REG}                 (* 8 *)
                    };
    AllowedIRegsWoEBPWoxH *= RegsOfSize            -- Допустимые регистры
                    {                       -- в зависимости от длины
                        {}                      (* 0 *)
                      , RegSet{AL..BL}          (* 1 *)
                      , AllowedIRegsWoEBP[2]    (* 2 *)
                      , {}                      (* 3 *)
                      , AllowedIRegsWoEBP[4]    (* 4 *)
                      , {}                      (* 5 *)
                      , {}                      (* 6 *)
                      , {}                      (* 7 *)
                      , AllowedIRegsWoEBP[8]    (* 8 *)
                    };
    AllowedIRegsWEBPWoxH *= RegsOfSize           -- Допустимые регистры
                    {                       -- в зависимости от длины
                        {}                      (* 0 *)
                      , RegSet{AL..BL}          (* 1 *)
                      , AllowedIRegsWEBP[2]     (* 2 *)
                      , {}                      (* 3 *)
                      , AllowedIRegsWEBP[4]            (* 4 *)
                      , {}                      (* 5 *)
                      , {}                      (* 6 *)
                      , {}                      (* 7 *)
                      , AllowedIRegsWEBP[8]     (* 8 *)
                    };

    AllowedLowRegsWEBP *= RegsOfSize            -- Допустимые регистры
                    {                       -- в зависимости от длины
                        {}                      (* 0 *)
                      , { BPlow..DIlow }              (* 1 *)
                      , {}              (* 2 *)
                      , {}                      (* 3 *)
                      , {}             (* 4 *)
                      , {}                      (* 5 *)
                      , {}                      (* 6 *)
                      , {}                      (* 7 *)
                      , {}                  (* 8 *)
                    };
    AllowedLowRegsWoEBP *= RegsOfSize            -- Допустимые регистры
                    {                       -- в зависимости от длины
                        {}                      (* 0 *)
                      , { SIlow..DIlow }              (* 1 *)
                      , {}              (* 2 *)
                      , {}                      (* 3 *)
                      , {}             (* 4 *)
                      , {}                      (* 5 *)
                      , {}                      (* 6 *)
                      , {}                      (* 7 *)
                      , {}                  (* 8 *)
                    };

    AllIRegsWEBP *= AllowedIRegsWEBP [1] + AllowedIRegsWEBP [2]
                  + AllowedIRegsWEBP [4] + AllowedIRegsWEBP [8] + AllowedLowRegsWEBP[1];
    AllIRegsWoEBP  *= AllowedIRegsWoEBP  [1] + AllowedIRegsWoEBP  [2]
                    + AllowedIRegsWoEBP  [4] + AllowedIRegsWoEBP  [8] + AllowedLowRegsWoEBP[1];

PROCEDURE ChSize*( reg: Reg; sz: ir.SizeType ): Reg;
BEGIN
    IF sz = 8 THEN
      IF reg IN REGS8 THEN
        RETURN reg;
      END;
        ASSERT(FALSE);  --fixme
      RETURN reg;
    ELSE
(*
      IF reg IN REGS8 THEN
        ASSERT(FALSE);  --fixme
      ELSE
*)
      RETURN RegTable[ RegInfo[ reg ].code, sz ];
--      END;
    END;
END ChSize;

PROCEDURE GetRegSetSize*( set: RegSet): ir.SizeType;
VAR
    sz : ir.SizeType;
    reg : Reg;
BEGIN
    sz := 0;
    FOR reg := MINREG TO MAXREG DO
       IF (reg IN set) THEN
         IF sz = 0 THEN
           sz := RegInfo[reg].sz;
         ELSE
           ASSERT(sz = RegInfo[reg].sz);
         END;
       END;
    END;
    RETURN sz;
END GetRegSetSize;

PROCEDURE RegSet2PhysRegSet*(s:RegSet):PhysRegSet;
VAR ps:PhysRegSet;
    r: Reg;
BEGIN
    ps := PhysRegSet{};
    FOR r:=MIN4REG TO MAX4REG DO
        IF r IN s THEN
            INCL(ps, VAL(PhysReg,r));
        END;
    END;
    RETURN ps;
END RegSet2PhysRegSet;

CONST  Conv2_4 *= ARRAY OF ir.SizeType{0,1,4,0,4,0,0,0,8};
CONST Conv12_4 *= ARRAY OF ir.SizeType{0,4,4,0,4,0,0,0,8};

CONST TruncSz4*= ARRAY OF ir.SizeType{0,1,2,0,4,0,0,0,4};
(*PROCEDURE TruncSz4*(sz: ir.SizeType):ir.SizeType;
BEGIN
  IF sz <= 4 THEN
    RETURN sz;
  ELSE
    RETURN 4;
  END;
END TruncSz24;
*)
PROCEDURE GetAllLows*(ps:RegSet):RegSet;
VAR r:Reg;
    retval: RegSet;
BEGIN
  retval := RegSet{};
  FOR r := MIN8REG TO MAX8REG DO
    INCL(retval,LowPart[r]);
  END;
  RETURN retval;
END GetAllLows;

PROCEDURE GetAllHighsWithLow*(ps:RegSet; reg:Reg):RegSet;
VAR r:Reg;
    retval: RegSet;
BEGIN
  retval := RegSet{};
  FOR r := MIN8REG TO MAX8REG DO
    IF LowPart[r] = reg THEN
      INCL(retval,HighPart[r]);
    END;
  END;
  RETURN retval;
END GetAllHighsWithLow;

PROCEDURE GetAllLowsWithHigh*(ps:RegSet; reg:Reg):RegSet;
VAR r:Reg;
    retval: RegSet;
BEGIN
  retval := RegSet{};
  FOR r := MIN8REG TO MAX8REG DO
    IF HighPart[r] = reg THEN
      INCL(retval,LowPart[r]);
    END;
  END;
  RETURN retval;
END GetAllLowsWithHigh;

TYPE
    Condition *= SYSTEM.CARD8;
    BinaryOp *= SYSTEM.CARD8;
    UnaryOp *= SYSTEM.CARD8;
    FloatOp *= SYSTEM.CARD8;
    ShiftOp *= SYSTEM.CARD8;

CONST
    JO  *=  Condition{ 0 };   JNO *= Condition{  1 };
    JB  *=  Condition{ 2 };   JAE *= Condition{  3 };   JC *=JB; JNC *= JAE;
    JE  *=  Condition{ 4 };   JNE *= Condition{  5 };
    JBE *=  Condition{ 6 };   JA  *= Condition{  7 };
    JS  *=  Condition{ 8 };   JNS *= Condition{  9 };
    JPE *= Condition{ 10 };   JPO *= Condition{ 11 };
    JL  *= Condition{ 12 };   JGE *= Condition{ 13 };
    JLE *= Condition{ 14 };   JG  *= Condition{ 15 };

    NoJ *= Condition{  127 };             -- Переход в конце луча не нужен
    UnJ *= Condition{  126 };             -- Безусловный переход в конце луча

--    HiFetch *= ARRAY OF Condition {255,255,JB,JA,255,255,JB,JA,
--                                   255,255,255,255,JL,JG,JL,JG };
    SavedByProc     *= RegSet{ EBX, EBP, ESI, EDI }
                     + RegSet{ BL, BH, BX, BPlow, BP, SIlow, SI, DIlow, DI }
                     + RegSet{ EBP_EBX, ESI_EBX, EDI_EBX }
                     + RegSet{ ESI_EBP, EDI_EBP }
                     + RegSet{ EDI_ESI };
    CCResultInTos   *= { at.BORLAND, at.MSVC, at.OS2SYS_CALL, at.DJGPP, at.GCC };


    TTT_add *= BinaryOp{ 0 };           -- Бинарные операции
    TTT_or  *= BinaryOp{ 1 };
    TTT_adc *= BinaryOp{ 2 };
    TTT_sbb *= BinaryOp{ 3 };
    TTT_and *= BinaryOp{ 4 };
    TTT_sub *= BinaryOp{ 5 };
    TTT_xor *= BinaryOp{ 6 };
    TTT_cmp *= BinaryOp{ 7 };
    TTT_bt  *= BinaryOp{ 8 };         -- Работа с битами
    TTT_bts *= BinaryOp{ 9 };         -- Работа с битами
    TTT_btr *= BinaryOp{ 10 };         -- Работа с битами

    TTT_last *= TTT_btr;

    TTT_mul *= BinaryOp{ 255 };

    TTT_inc *= UnaryOp{ 0 };           -- Унарные операции
    TTT_dec *= UnaryOp{ 1 };
    TTT_not *= UnaryOp{ 2 };
    TTT_neg *= UnaryOp{ 3 };

    TTT_rol *= ShiftOp{ 0 };           -- Сдвиги
    TTT_ror *= ShiftOp{ 1 };
    TTT_shl *= ShiftOp{ 4 };
    TTT_shr *= ShiftOp{ 5 };
    TTT_sar *= ShiftOp{ 7 };

    FADD  *= FloatOp{ 0 };             -- Вещественные операции
    FMUL  *= FloatOp{ 1 };
    FSUB  *= FloatOp{ 4 };
    FSUBR *= FloatOp{ 5 };
    FDIV  *= FloatOp{ 6 };
    FDIVR *= FloatOp{ 7 };

    FXCH   *= FloatOp{ 0C9H };
    FABS   *= FloatOp{ 0E1H };
    FCHS   *= FloatOp{ 0E0H };
    FLD1   *= FloatOp{ 0E8H };
    FLDL2E *= FloatOp{ 0EAH };
    FLDPI  *= FloatOp{ 0EBH };
    FLDLG2 *= FloatOp{ 0ECH };
    FLDLN2 *= FloatOp{ 0EDH };
    FLDZ   *= FloatOp{ 0EEH };
    F2XM1  *= FloatOp{ 0F0H };
    FYL2X  *= FloatOp{ 0F1H };
    FPTAN  *= FloatOp{ 0F2H };
    FPATAN *= FloatOp{ 0F3H };
    FPREM  *= FloatOp{ 0F8H };
    FSQRT  *= FloatOp{ 0FAH };
    FSCALE *= FloatOp{ 0FDH };
    FSIN   *= FloatOp{ 0FEH };
    FCOS   *= FloatOp{ 0FFH };

TYPE
    ScaleType  *= INTEGER;
CONST
    x1 *= ScaleType{ ASH (0, 6) };
    x2 *= ScaleType{ ASH (1, 6) };
    x4 *= ScaleType{ ASH (2, 6) };
    x8 *= ScaleType{ ASH (3, 6) };

VAR
    i,j, pair: Reg;
    foo : SHORTINT;
BEGIN
    FOR i := MINREG TO MAXREG DO
        PartsOf  [i] := RegSet {};
        OwnersOf [i] := RegSet {};
        RegInfo  [i].code  := UNDEF_REGp;
        RegInfo  [i].codehi:= UNDEF_REGp;
    END;
    FOR i := AL TO BH DO
        RegInfo [i].sz := 1;
        RegInfo [i].code := VAL(PhysReg, ORD(i) MOD 8);
    END;
    FOR i := BPlow TO DIlow DO
        RegInfo [i].sz := 1;
        RegInfo [i].code := VAL(PhysReg, ORD(i) MOD 8);
    END;

    FOR i := AX TO BX DO
        RegInfo [i].sz := 2;
        RegInfo [i].code := VAL(PhysReg, ORD(i) MOD 8);
        PartsOf [i] := RegSet {SUCC(VAL(Reg, ORD(i) MOD 8),34B),
                               SUCC(VAL(Reg, ORD(i) MOD 8),30B)};
    END;
    FOR i := BP TO DI DO
        RegInfo [i].sz := 2;
        RegInfo [i].code := VAL(PhysReg, ORD(i) MOD 8);
        PartsOf [i] := RegSet {SUCC(VAL(Reg, ORD(i) MOD 8),40B)};
    END;

    FOR i := MIN4REG TO MAX4REG DO
        RegInfo [i].sz := 4;
        RegInfo [i].code := VAL(PhysReg, ORD(i) MOD 8);
      IF i # ESP THEN
        PartsOf [i] := RegSet {SUCC(i, 20B)} + PartsOf [SUCC(i,20B)];
      END;
    END;

    FOR pair := MIN8REG TO MAX8REG DO
         RegInfo [pair].sz := 8;
         RegInfo [pair].code := VAL(PhysReg, LowPart[pair]);
         RegInfo [pair].codehi:= VAL(PhysReg, HighPart[pair]);
         PartsOf [pair] := RegSet {HighPart[pair], LowPart[pair]}
                         + PartsOf[HighPart[pair]]
                         + PartsOf[LowPart[pair]];
    END;
    FOR foo:= 0 TO 2 DO
      RegInfo[SPILLED2Pow[foo]].sz := VAL(ir.SizeType,ASH(1, foo));
      RegInfo[SPILLED2Pow[foo]].code   := VAL(SHORTINT,SPILLED2Pow[foo]);
--      RegInfo[SPILLED2Pow[foo]].codehi := SPILLED4p;
    END;
    RegInfo[SPILLED2Pow[3]].sz := VAL(ir.SizeType,ASH(1, 3));
    RegInfo[SPILLED2Pow[3]].code   := SPILLED8plo;
    RegInfo[SPILLED2Pow[3]].codehi := SPILLED8phi;

    FOR i := MINREG TO MAXREG DO
        FOR j := MINREG TO MAXREG DO
            IF j IN PartsOf [i] THEN
                OwnersOf [j] := OwnersOf [j] + RegSet {i};
            END;
        END;
    END;

    FOR i:= MINREG TO SYSTEM.PRED(MIN8REG) DO
        IntersectWith [i]       := PartsOf [i] + OwnersOf [i];
        RegAndIntersectWith [i] := IntersectWith [i] + RegSet{i};
        SpecialIntersect [i] := RegAndIntersectWith [i];
        RegAndIntersectWith4Max [i] := RegAndIntersectWith [i] * (-RegSet{MIN8REG..MAX8REG});
    END;

    FOR i:= MIN8REG TO MAX8REG DO
        RegAndIntersectWith [i]       := RegAndIntersectWith [LowPart[i]]
                                 + RegAndIntersectWith [HighPart[i]];
        IntersectWith [i] := RegAndIntersectWith [i] - RegSet{i};
        SpecialIntersect [i] := RegAndIntersectWith [i] * (-RegSet{MIN8REG..MAX8REG}) + RegSet{i};
        RegAndIntersectWith4Max [i] := SpecialIntersect [i];
    END;

END desc386.
