MODULE ir;

IMPORT BitVect;
IMPORT ir_def;
IMPORT ir_D;
IMPORT env := xiEnv;
IMPORT pc := pcK;
<* IF ~ NODEBUG THEN *>
IMPORT opIO;
<* END *>
IMPORT Printf;
IMPORT SYSTEM;
IMPORT Calc;
IMPORT tune := opTune;
IMPORT fmt := xcStr;
IMPORT DStrings;

<* IF TARGET_IDB THEN *>
TYPE iv_wrn=PROCEDURE(p:pc.TPOS);
VAR unreachable*:iv_wrn;
<* END *>
(* ---------------------------- Various types ------------------------------- *)

TYPE
        INT            *= LONGINT;
        TPOS           *= pc.TPOS;
        VALUE          *= pc.VALUE;
        FLOAT          *= ir_def.FLOAT;
        ScopeType      *= INTEGER;                      (* ??? *)
        NameType       *= pc.STRING;                    (* ??? *)
        TypeType       *= ir_def.TypeType;
        SizeType       *= ir_def.SizeType;

        Node           *= INT;
        TriadePtr      *= POINTER TO Triade;
        TriadeArray    *= POINTER TO ARRAY OF TriadePtr;
        ParamPtr       *= POINTER TO Param;

        TSNode         *= Node;
        Arc            *= INTEGER;
        Loop           *= Node;

        NodeArray      *= POINTER TO ARRAY OF Node;
        TSNodeArray    *= POINTER TO ARRAY OF Node;  -- INDEXED BY "TSNode"
        NodeNodeArray  *= POINTER TO ARRAY OF Node;  -- INDEXED BY "Node"
        ArcArray       *= POINTER TO ARRAY OF Arc;

        TypeTypeSet    *= ir_def.TypeTypeSet;
        SizeTypeRange  *= ir_def.SizeTypeRange;


CONST   MaxVarSize *= ir_def.MaxVarSize;

CONST   t_invalid  *= ir_def.t_invalid;
        t_void     *= ir_def.t_void;
        t_int      *= ir_def.t_int;
        t_unsign   *= ir_def.t_unsign;
        t_float    *= ir_def.t_float;
        t_complex  *= ir_def.t_complex;
        t_ref      *= ir_def.t_ref;
        t_arr      *= ir_def.t_arr;
        t_rec      *= ir_def.t_rec;
        t_flxarr   *= ir_def.t_flxarr;
        t_ZZ       *= ir_def.t_ZZ;
        t_RR       *= ir_def.t_RR;

CONST   WholeTypes *= TypeTypeSet{t_int, t_unsign};
        BiggestIntSize *= 8;

CONST   TypeName* = ir_def.TypeName;

(* ------------------------------ Operation --------------------------------- *)

TYPE Operation      *=
     (  o_invalid,
        o_fi,                      (* r = fi (x, y, ...)                 *)
        o_assign,                  (* r = x                              *)
        o_copy,                    (* COPY: * x -> * y (z bytes)         *)
        o_val,                     (* r = VAL ((OpType, OpSize) x)       *)
        o_cast,                    (* r = SYSTEM.VAL((OpType,OpSize) x)  *)
        o_cap,                     (* r = CAP (x)                        *)
        o_abs,                     (* r = ABS (x)                        *)
        o_complex,                 (* r = x + i * y                      *)
        o_re,                      (* r = Re (x)                         *)
        o_im,                      (* r = Im (x)                         *)
        o_add,                     (* r = x + y + z + ...                *)
        o_mul,                     (* r = x * y * z * ...                *)
        o_mulh,                    (* r = HIGH (x * y)                   *)
        o_div,                     (* r = x DIV y                        *)
        o_dvd,                     (* r = x / y                          *)
        o_mod,                     (* r = x MOD y                        *)
        o_rem,                     (* r = x REM y                        *)
        o_power,                   (* r = x ** (OpType, OpSize) y        *)
        o_and,                     (* r = x AND y AND z AND ...          *)
        o_or,                      (* r = x OR y OR z OR ...             *)
        o_andnot,                  (* r = x ANDNOT y ANDNOT z ANDNOT ... *)
        o_xor,                     (* r = x XOR y XOR z XOR ...          *)
        o_not,                     (* r = NOT x                          *)
        o_incl,                    (* r = x; INCL (r, y)                 *)
        o_excl,                    (* r = x; EXCL (r, y)                 *)
        o_loset,                   (* r = { 0 .. x }                     *)
        o_hiset,                   (* r = { x .. ResSize * 8 - 1 }       *)
        o_shl,                     (* r = x << (OpType, OpSize) y        *)
        o_shr,                     (* r = x >> (OpType, OpSize) y        *)
        o_sar,                     (* r = x >> (OpType, OpSize) y        *)
        o_rol,                     (* r = x rol (OpType, OpSize) y       *)
        o_ror,                     (* r = x ror (OpType, OpSize) y       *)
        o_sgnext,                  (* r = sign-extend x (0 or -1)        *)
        o_bf_get,                  (* r = bit_field_get (x, first, len)  *)
        o_bf_put,                  (* r = bit_field_put (x, first, len)  *)
        o_call,                    (* [ r = ] CALL x (y, z, ... )        *)
        o_ret,                     (* RETURN [x]                         *)
        o_checknil,                (* CHECK: x <> NIL                    *)
        o_checklo,                 (* CHECK: x >= y                      *)
        o_checkhi,                 (* CHECK: x <  y                      *)
        o_stop,                    (* HALT (x)                           *)
        o_error,                   (* RAISE (x)                          *)
        o_load,                    (* load r = X                         *)
        o_store,                   (* store R = x                        *)
        o_loadr,                   (* r = * x                            *)
        o_storer,                  (* * x = y                            *)

        o_forstart,                (* IF z > 0 AND x <= y OR
                                        z < 0 AND x >= y THEN
                                          r = from
                                          GOTO arc1
                                     [ ELSE
                                          GOTO arc2 ]                   *)
        o_forcont,                 (* IF (NOT last iteration)
                                          INCREMENT r BY z
                                          GOTO arc1
                                     ELSE
                                          GOTO arc2                     *)

        o_eq,                      (* IF x = y  THEN arc1 ELSE arc2      *)
        o_le,                      (* IF x <= y THEN arc1 ELSE arc2      *)
        o_leset,                   (* IF x <= y THEN arc1 ELSE arc2      *)
        o_in,                      (* IF x IN y THEN arc1 ELSE arc2      *)
        o_odd,                     (* IF ODD (x) THEN arc1 ELSE arc2     *)
        o_case,                    (* CASE x OF y, z: arc1
                                               t, w: arc2
                                               ...
                                     ELSE            arcn               *)
        o_goto,                    (* GOTO arc1                          *)

        o_getpar,                  (* r = GETPAR (NPar)                  *)
        o_move_eq,                 (* r = (x == y) ? z : t               *)
        o_move_le,                 (* r = (x <= y) ? z : t               *)
        o_alloca,                  (* r = alloca (x)                     *)

---- НЕ МОГУТ ГЕНЕРИРОВАТЬСЯ frontend-ом и появляться во время оптимизации:

        o_neg,                     (* r = - x                            *)
        o_sub,                     (* r = x - y                          *)
        o_putpar,                  (* PUT (optype, opsize) x             *)
        o_comma,                   (* ═єцэю фы  COPY                     *)
        o_par,                     (* ╦шёЄ т DAG-e                       *)
        o_retfun,                  (* ┬ючтЁрЄ шч ЇєэъЎшш                 *)
        o_shift,                   (* ┬ёх ёфтшуш, ўЄю є эрё с√тр■Є       *)
        o_logical,                 (* AND, OR, XOR                       *)
        o_fbin,                    (* +, -, *, / тх∙хёЄтхээ√х            *)
        o_funary,                  (* -, ABS тх∙хёЄтхээ√х                *)
        o_fle,                     (* float <=                           *)
        o_feq,                     (* float =                            *)

---- Появляются во время оптимизации:

        o_sin,
        o_cos,
        o_tan,
        o_atan,
        o_exp,
        o_sqrt,
        o_ln,
        o_lg,

---- Добавлена KDV для построения в деревянном представлении списков
---- параметров
        o_empty,

---- Добавлена KDV в Emit_PPC
        o_neg_abs,

---- База памяти для SL/1

        o_base,

        o_clear,                  (* memset(x, y, 0) x is addr, y is len*)
        o_hiword,                 (* getting the high half of X      *)
                                  (* this triade is used on x86 only *)
        o_clinit,                 (* if x is not clinited, clinit it *)
        o_checkb,                 (* check: 0 <= x < y *)
        o_cmpswap,                (* if *x != y then *x := z; goto ARC1;
                                                else goto ARC2;
                                                end;
                                  *)
        o_seqpoint,               (* sequence point for synchronized block
                                     bounds: noone triade that can raise an
                                     exception or can alter data accessible from
                                     another thread may be moved over it.
                                  *)
        o_constr,                 (* creates variable of new type from two
                                     ones of lesser size.
                                     now is used for creation of interface(int64)
                                     from one t_ref and one t_int
                                  *)
        o_checkneq                (* if param == 0 then
                                         call x2j_trap_division()
                                  *)

    );

CONST   max_op_code    *= MAX(Operation);

TYPE    OpSet *= PACKEDSET OF Operation;

TYPE    OpPropertiesType *=
        (
         isMulti,       -- Multioperation?
         isMovable,     -- Can move out of loop?
         isRead,        -- Читает ли триада на самом деле из памяти?
                        -- (а то вектор Read может стоять из-за ее опасности)
         isCommu,       -- Коммутативная операция?
         isPrelive,     -- Нельзя ее вычищать
         isLast,        -- Последняя триада в линейном участке
         isNoReturn     -- never lets execution go further
        );

        OpPropertiesSet   *= PACKEDSET OF OpPropertiesType;

        OpPropertiesArray *= ARRAY Operation OF OpPropertiesSet;

CONST   OpProperties *= OpPropertiesArray {
                {},                                             (* o_invalid *)
                {},                                             (* o_fi *)
                {isMovable},                                    (* o_assign *)
                {isRead, isPrelive},                            (* o_copy *)
                {isMovable},                                    (* o_val *)
                {isMovable},                                    (* o_cast *)
                {isMovable},                                    (* o_cap *)
                {isMovable},                                    (* o_abs *)
                {isMovable},                                    (* o_complex *)
                {isMovable},                                    (* o_re *)
                {isMovable},                                    (* o_im *)
                {isMulti, isMovable, isCommu},                  (* o_add *)
                {isMulti, isMovable, isCommu},                  (* o_mul *)
                {isMovable, isCommu},                           (* o_mulh *)
                {isMovable},                                    (* o_div *)
                {isMovable},                                    (* o_dvd *)
                {isMovable},                                    (* o_mod *)
                {isMovable},                                    (* o_rem *)
                {isMovable},                                    (* o_power *)
                {isMulti, isMovable, isCommu},                  (* o_and *)
                {isMulti, isMovable, isCommu},                  (* o_or *)
                {isMovable},                                    (* o_andnot *)
                {isMulti, isMovable, isCommu},                  (* o_xor *)
                {isMovable},                                    (* o_not *)
                {isMovable},                                    (* o_incl *)
                {isMovable},                                    (* o_excl *)
                {isMovable},                                    (* o_loset *)
                {isMovable},                                    (* o_hiset *)
                {isMovable},                                    (* o_shl *)
                {isMovable},                                    (* o_shr *)
                {isMovable},                                    (* o_sar *)
                {isMovable},                                    (* o_rol *)
                {isMovable},                                    (* o_ror *)
                {isMovable},                                    (* o_sgnext *)
                {isMovable},                                    (* o_bf_get *)
                {isMovable},                                    (* o_bf_put *)
                {isRead, isPrelive},                            (* o_call *)
                {isPrelive, isLast, isNoReturn},                (* o_ret *)
                {isMovable, isPrelive},                         (* o_checknil *)
                {isMovable, isPrelive},                         (* o_checklo *)
                {isMovable, isPrelive},                         (* o_checkhi *)
                {isPrelive, isLast, isNoReturn},                (* o_stop *)
                {isPrelive, isLast, isNoReturn},                (* o_error *)
                {isRead},                                       (* o_load *)
                {isPrelive},                                    (* o_store *)
                {isMovable, isRead},                            (* o_loadr *)
                {isPrelive},                                    (* o_storer *)
                {isLast},                                       (* o_forstart *)
                {isLast},                                       (* o_forcont *)
                {isCommu, isLast},                              (* o_eq *)
                {isLast},                                       (* o_le *)
                {isLast},                                       (* o_leset *)
                {isRead,isLast},                                       (* o_in *)
                {isLast},                                       (* o_odd *)
                {isLast},                                       (* o_case *)
                {isLast},                                       (* o_goto *)
                {},                                             (* o_getpar *)
                {isCommu, isMovable},                           (* o_move_eq *)
                {isMovable},                                    (* o_move_le *)
                {},                                             (* o_alloca *)
                {},                                             (* o_neg *)
                {},                                             (* o_sub *)
                {},                                             (* o_putpar *)
                {},                                             (* o_comma *)
                {},                                             (* o_par *)
                {isLast},                                       (* o_retfun *)
                {},                                             (* o_shift *)
                {isCommu},                                      (* o_logical *)
                {},                                             (* o_fbin *)
                {},                                             (* o_funary *)
                {isLast},                                       (* o_fle *)
                {isCommu, isLast},                              (* o_feq *)

                {isMovable},                                    (* o_sin *)
                {isMovable},                                    (* o_cos *)
                {isMovable},                                    (* o_tan *)
                {isMovable},                                    (* o_atan *)
                {isMovable},                                    (* o_exp *)
                {isMovable},                                    (* o_sqrt *)
                {isMovable},                                    (* o_ln *)
                {isMovable},                                    (* o_lg *)
                {},                                             (* o_empty *)
                {},                                             (* o_negabs *)
                {},                                             (* o_base *)
                {isPrelive},                                    (* o_clear *)
                {isMovable},                                    (* o_hiword *)
                {isPrelive, isMovable, isRead},                 (* o_clinit *)
                {isMovable, isPrelive},                         (* o_checkb *)
                {isLast, isRead, isPrelive},                    (* o_cmpswap *)
                {isRead, isPrelive},                            (* o_seqpoint *)
                {},                                             (* o_constr *)
                {isMovable, isPrelive}                          (* o_checkneq *)
        };        


(* ------------------------------ TagType ---------------------------------- *)

TYPE    TagType       *= SHORTINT;
CONST
        y_Nothing       * = TagType{ 1 };
        y_NumConst      * = TagType{ 2 };
        y_AddrConst     * = TagType{ 3 };
        y_RealConst     * = TagType{ 4 };
        y_ComplexConst  * = TagType{ 5 };
        y_ProcConst     * = TagType{ 6 };
        y_RealVar       * = TagType{ 7 };
        y_Variable      * = TagType{ 8 };


TYPE    TagTypeSet *= PACKEDSET OF TagType;

        ConstTagsType = ARRAY TypeType OF TagType;

CONST   NumericTags *= TagTypeSet{ y_RealConst, y_NumConst };

CONST   ConstTags *= ConstTagsType {
                        MAX (TagType)   (* t_invalid *)
                      , MAX (TagType)   (* t_void    *)
                      , y_NumConst      (* t_int     *)
                      , y_NumConst      (* t_unsign  *)
                      , y_RealConst     (* t_float   *)
                      , y_ComplexConst  (* t_complex *)
                      , y_NumConst      (* t_ref     *)
                      , MAX (TagType)   (* t_arr     *)
                      , MAX (TagType)   (* t_rec     *)
                      , MAX (TagType)   (* t_flxarr  *)
                      , y_NumConst      (* t_ZZ      *)
                      , y_RealConst     (* t_RR      *)
                   };

TYPE    IsFloatType = ARRAY TypeType OF BOOLEAN;

CONST   IsFloat *= IsFloatType {
                        FALSE                   (* t_invalid *)
                      , FALSE                   (* t_void    *)
                      , FALSE                   (* t_int     *)
                      , FALSE                   (* t_unsign  *)
                      , TRUE                    (* t_float   *)
                      , TRUE                    (* t_complex *)
                      , FALSE                   (* t_ref     *)
                      , FALSE                   (* t_arr     *)
                      , FALSE                   (* t_rec     *)
                      , FALSE                   (* t_flxarr  *)
                      , TRUE                    (* t_ZZ      *)
                      , TRUE                    (* t_RR      *)
                   };

TYPE    IsOrdinalType = ARRAY TypeType OF BOOLEAN;

CONST   Ordinal* = IsOrdinalType {
                                        FALSE          (* t_invalid *)
                                      , FALSE          (* t_void    *)
                                      , TRUE           (* t_int     *)
                                      , TRUE           (* t_unsign  *)
                                      , FALSE          (* t_float   *)
                                      , FALSE          (* t_complex *)
                                      , TRUE           (* t_ref     *)
                                      , FALSE          (* t_arr     *)
                                      , FALSE          (* t_rec     *)
                                      , FALSE          (* t_flxarr  *)
                                      , FALSE          (* t_ZZ      *)
                                      , FALSE          (* t_RR      *)
                                 };

(* ------------------------------ Options ----------------------------------- *)

TYPE Option *=
     (
        o_Dangerous    ,
        o_Checked      ,
        o_Processed    ,
        o_Live         ,
        o_Constant     ,    -- На чтении из памяти: никто не может в эту
                                -- память записать (при доступе к методу,
                                -- длине гибкого массива,
                                -- ссылке на тело массива)

        o_Index,            -- Только для checklo, checkhi
        o_Range,            -- Только для checklo, checkhi
        o_Division,         -- Только для checklo, checkhi, checkneq

        o_Silent,           -- Не выдавать предупреждений при оптимизации
        o_Assert,           -- Только на o_error: на самом деле ASSERT

        o_Volatile,             -- На o_loadr, o_storer: обращение в volatile
                                -- память; кроме того, этот же флажок может
                                -- стоять и на local, если тот описан как
                                -- volatile.

        o_Removable,            -- Триаду можно вычистить, даже если она опасная
                                -- на o_div, o_mod - если выключен CHECKDIV,
                                -- на o_loadr - если выключен CHECKNIL

        o_Positive,             -- На o_forstart: начальное значение >= 0

        o_Parameters,           -- Ставится на o_copy, связанную с приемом
                                -- параметров и на локалы, которые и есть параметры

        o_NoMemWrite,           -- only for o_call
                                -- means that this procedure doesn't write any memory
                                -- which is analyzed by compiler, so
                                -- excessive Load/Stores will be eliminated.
                                -- for instance, x2j_new may be marked with this flag

        o_SideEffectSafe,       -- o_call: Means that call is absolutely side effects safe
                                -- so we can remove it if it's result is not used
        o_NoReturn,

        o_IncomparableAsTrue,   -- only for o_le & o_eq triades with float
                                -- parameters. Means that the computation of
                                -- this triade must return TRUE if the arguments
                                -- are incomparable (one of them is NAN,
                                -- for instance)

        o_NotInsideTcf,         -- is put only on triades wchich possibly
                                -- will generate excetions.
                                -- means that this triade is not inside
                                -- try-block or catch-block,
                                -- so all local variable should not be stored
                                -- before execution of this triade. Used in SSA
        o_Trap,
        o_Initializer,          -- is put to X2J_CLASS_INIT to show that this
                                -- routine may write to all variables,
                                -- even to constant ones
        o_NoOptimize,
        o_IsLast,
        o_FirstPutPar,          -- first o_putpar for some o_call. Useful for
                                -- making stack frame alignment
        o_Lock,                 -- atomic instruction. 'lock' prefix needed on
                                -- multiprocessor platform

(*
  Эти options ставятся на локалы, в их поле Options
*)

        o_Debug,                -- Используется при выдаче отладочной информации
--      o_Volatile
        o_IsChecknil
       );

TYPE    OptionsSet     *= PACKEDSET OF Option;

TYPE    GenMode *= (* ---- моды генерации ---- *)
        (
          REF,       (* требуется адрес *)
          LVL,       (* требуется l-value (адрес или объект) *)
          NEG,
          CC,        (* генерировать C-шные строковые константы *)

          VOLATILE,  (* возвращаемый в arg признак - вместе c REF *)

          VPTR       (* SL-1 virtual pointer - возвращаемый признак *)
        );

        GenModeSet *= PACKEDSET OF GenMode;

(* ------------------------------ Triade ------------------------------------ *)

TYPE
    ProcNum  *= INTEGER;
    ProtoNum *= INTEGER;
    VarNum   *= LONGINT;
    LCNum    *= ir_def.LCNum;
    Local    *= VarNum;

    ParamOption *=
    (
        popt_LastUse,
        popt_Checking
    );
    ParamOptionsSet     *= PACKEDSET OF ParamOption;

    Param    *= RECORD(ir_D.Param)
                         reverse    -: BOOLEAN;    (* For o_add only        *)

<* IF TARGET_386 THEN *>
                         tag        -: TagType;
                         name       -: VarNum;
<* ELSE *>
                         tag        *: TagType;
                         name       *: VarNum;
<* END *>
                         value      *: VALUE;
                         offset     *: LONGINT;
                         next       *: ParamPtr;
                         prev       *: ParamPtr;
<* IF TARGET_386 THEN *>
                         triade     -: TriadePtr;
<* ELSE *>
                         triade     *: TriadePtr;
<* END *>
                         paramnumber*: INTEGER;
                         position   *: TPOS;        (* For o_fi only *)
                         options    *: ParamOptionsSet;
                     END;

    ParamArray *= POINTER TO ARRAY OF ParamPtr;
(*
        y_NumConst:     value      - значение
        y_RealConst:    value      - значение
        y_ComplexConst: value      - значение
        y_RealVar:      name       - # локала
        y_Variable:     name       - # переменной,
                        next, prev - список использований
        y_AddrConst:    name   - # локала
                        offset - смещение (в байтах)
        y_ProcConst:    name       - # процедуры
*)

    Triade    *= RECORD
                         Next        *: TriadePtr;
                         Prev        *: TriadePtr;
                         Options     *: OptionsSet;
                         NodeNo      *: Node;
                         Tag         *: TagType;
                         Op          *: Operation;
                         OpType      *: TypeType;
                         OpSize      *: SizeType;
                         ResType     *: TypeType;
                         ResSize     *: SizeType;
                         Name        *: VarNum;
                         Prototype   *: ProtoNum;        (* For o_Call only *)
                         Read        *: BitVect.BitVector;
                         Write       *: BitVect.BitVector;
                         Params      *: ParamArray;
                         Position    *: TPOS;
                         NPar        *: SHORTINT;       (* For o_getpar only *)
                 END;                                   (* NPar for o_copy - alignment *)

TYPE
    Loc *= (
        LocUndef       ,
        LocInReg       ,
        LocInFrame
    );

        DebugInfo      *= RECORD
                              Location* : Loc;
                              Value*    : LONGINT;
                          END;

        LocalType      *= RECORD
                              Scope    *:  ScopeType;
                              Name     *:  NameType;
                              Obj      *:  pc.OBJECT;
                              Align    *:  SHORTINT;
                              VarSize  *:  LONGINT;
                              VarType  *:  TypeType;
                              Offset   *:  LONGINT;


--                            Lo       *:  VALUE;
--                            Hi       *:  VALUE;
                              Options  *: OptionsSet;
                              Addressed*: BOOLEAN;
                              Debug    *: DebugInfo;
                          END;

        VarOption *=
        (
          o_LiveAtCall,
          o_LiveAtCopy,
          o_LiveAtFCom,
          o_LiveAtMulDiv,
          o_LiveAtCmpSwap,
          o_LocInFMUL,

          o_ForCtr,

          o_InFixReg,
          o_SpilledByByte,
          o_Backwards
        );
        VarOptionsSet   *= PACKEDSET OF VarOption;

        VarInfoType    *= RECORD
                              Def     *: TriadePtr;
                              Use     *: ParamPtr;
                              LocalNo *: Local;
                              Next    *: VarNum;
                              Options *: VarOptionsSet;
                          END;

        LocalRefs       = POINTER TO ARRAY OF LocalType;   -- INDEXED BY "Local"
        VariableRefs    = POINTER TO ARRAY OF VarInfoType; -- INDEXED BY "VarNum"
        VarNumArray    *= POINTER TO ARRAY OF VarNum;

(* -------------------------------------------------------------------------- *)

TYPE    NodeType *= RECORD
                        First      *: TriadePtr;
                        Last       *: TriadePtr;
                        NIn        *: INT;
                        NOut       *: INT;
                        InArcs     *: ArcArray;
                        OutArcs    *: ArcArray;
                        In         *: NodeArray;
                        Out        *: NodeArray;
                        TopNumber  *: TSNode;
                        Dominators *: BitVect.BitVector;
                        aDominators*: BitVect.BitVector;
                        IDom       *: Node;
                        DomChild   *: Node;
                        DomLink    *: Node;
                        LoopNo     *: Loop;
                        IsPreheaderOf*: Loop;
                        Nesting    *: INT;
                        Alive      *: BOOLEAN;
                        Processed  *: BOOLEAN;
                        IndirectEnter*: BOOLEAN;
                       <* IF DEFINED(OVERDYE) AND OVERDYE THEN *>
                        Position   *: TPOS;
                       <* END *>
                    END;

       BitMatrix *= POINTER TO ARRAY OF BitVect.BitVector;
       BitMatrixNode *= POINTER TO ARRAY OF BitVect.BitVector; -- INDEXED BY "Node"
       BitMatrixVar  *= POINTER TO ARRAY OF BitVect.BitVector; -- INDEXED BY "VarNum"
       BitMatrixLC   *= POINTER TO ARRAY OF BitVect.BitVector; -- INDEXED BY "LCNum"
       Matrix    *= POINTER TO ARRAY OF NodeArray;
       MatrixNode*= POINTER TO ARRAY OF NodeArray; -- INDEXED BY "Node"
       Vector    *= POINTER TO ARRAY OF INT;
       VectorNode*= POINTER TO ARRAY OF INT;       -- INDEXED BY "Node"
       NodeRefs  *= POINTER TO ARRAY OF NodeType;  -- INDEXED BY "Node"

CONST  UndefLoop      *= Loop   { -1};
       UndefNode      *= MAX (Node);
       UndefTSNode    *= TSNode { -1};
       UNDEFINED      *= VarNum { -1};
       LCUNDEFINED    *= LCNum  { -1};
       ProcUNDEFINED  *= ProcNum{ -1};
       TEMPORARY      *= VarNum { -1};
       UNKNOWN_OFFSET *=  0;
       INVProtoNum    *= ProtoNum{ -1 };

CONST  TmpScope       *= ScopeType{-1};
       ZEROScope      *= ScopeType{ 0};
       ZEROArc        *= Arc{ 0 };
       ZERONode       *= Node{ 0 };
       ZEROTSNode     *= TSNode{ 0 };
       ZEROLocal      *= Local{ 0 };
       ZEROVarNum     *= VarNum{ 0 };
       ZEROLCNum      *= LCNum{ 0 };
       ZEROLoop       *= Loop{ 0 };
       ZEROProcNum    *= ProcNum{ 0 };
       ZEROProtoNum   *= ProtoNum{ 0 };

VAR
       NullPos-          : TPOS;
       Locals*           : LocalRefs;
       NLocals*          : Local;

       Vars*             : VariableRefs;
       NVars*            : VarNum;
       VarHead           : VarNum;

       Nnodes*           : Node;     -- all these variables have not been moved
       Nodes*            : NodeRefs; -- to ControlGraph.ob2
                                     -- because there are too many references to
                                     -- them in the other source files.
                                     -- Mne vlomy vse ispravlyat'
       StartOrder*       : TSNode;
       Order*            : TSNodeArray;
       AllocatedLocals-   : BitVect.BitVector;

       modname_local*     :Local;

PROCEDURE (tr: TriadePtr) IsRead*(): BOOLEAN;
BEGIN
  IF tr.Op IN OpSet{o_in, o_incl, o_excl} THEN
    RETURN tr.OpType = t_ref;
  ELSE
    RETURN isRead IN OpProperties[tr.Op];
  END;
END IsRead;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                          Symbol table operations                           *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NewLocals*;
BEGIN
<* IF MODE = "WORK" THEN *>
    NEW (Locals, 200);
    NEW (Vars,   500);
<* ELSE *>
    NEW (Locals, 20);
    NEW (Vars,   20);
<* END *>
    NLocals := 0;
    NVars   := 0;
    VarHead := UNDEFINED;
END NewLocals;

PROCEDURE FindLocal* (name: NameType; scope: ScopeType): Local;
VAR i: Local;
BEGIN
    FOR i:=0 TO NLocals-1 DO
        IF (Locals^[i].Name = name) & (Locals^[i].Scope = scope) THEN
            RETURN i;
        END;
    END;
    RETURN UNDEFINED;
END FindLocal;

PROCEDURE AddLocal* (name: NameType; scope: ScopeType; align: SHORTINT): Local;
VAR l: Local;
    p: LocalRefs;
BEGIN
    IF NLocals = LEN (Locals^) THEN
        NEW (p, LEN (Locals^) * 2);
        FOR l:=0 TO NLocals-1 DO
            p^[l] := Locals^[l];
        END;
        Locals := p;
    END;
    Locals^[NLocals].Name           := name;
    Locals^[NLocals].Obj            := NIL;
    Locals^[NLocals].Scope          := scope;
    Locals^[NLocals].Align          := align;
    Locals^[NLocals].Offset         := UNKNOWN_OFFSET;
    Locals^[NLocals].Addressed      := FALSE;
    Locals^[NLocals].Options        := OptionsSet{};
    Locals^[NLocals].Debug.Location := LocUndef;
    INC (NLocals);
    RETURN SYSTEM.PRED(NLocals);
END AddLocal;

PROCEDURE MarkAllocatedLocals*;
VAR l: Local;
BEGIN
    AllocatedLocals := BitVect.New (NLocals+1, FALSE);
    FOR l := ZEROVarNum TO SYSTEM.PRED(NLocals) DO
      IF Locals[l].Offset # UNKNOWN_OFFSET THEN
        BitVect.Incl(AllocatedLocals, l);
      END;
    END;
END MarkAllocatedLocals;

PROCEDURE ClearAllocatedLocals*;
BEGIN
    BitVect.Free (AllocatedLocals);
END ClearAllocatedLocals;

PROCEDURE CanBePointed* (l: Local): BOOLEAN;
BEGIN
    RETURN Locals^[l].Scope >= ZEROScope;
END CanBePointed;

PROCEDURE IsExternal* (l: Local): BOOLEAN;
BEGIN
    RETURN Locals^[l].Scope >= ZEROScope;
END IsExternal;

PROCEDURE IsParameter*(l: Local): BOOLEAN;
BEGIN
    RETURN o_Parameters IN Locals[l].Options;
END IsParameter;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GenVar* (l: Local; VAR v: VarNum; def: TriadePtr);
VAR i: VarNum;
    p: VariableRefs;
BEGIN
    IF VarHead <> UNDEFINED THEN
       v := VarHead;
       VarHead := Vars^[v].Next;
    ELSE
        IF NVars = LEN (Vars^) THEN
            NEW (p, LEN (Vars^) * 2);
            FOR i:=0 TO NVars-1 DO
                p^[i] := Vars^[i];
            END;
            Vars := p;
        END;
        v := NVars;
        INC (NVars);
    END;
    Vars^[v].LocalNo := l;
    Vars^[v].Use     := NIL;
    Vars^[v].Def     := def;
    Vars^[v].Next    := UNDEFINED;
    Vars^[v].Options := VarOptionsSet{};
END GenVar;

PROCEDURE RemoveVar* (v: VarNum);
BEGIN
    ASSERT (Vars^[v].Next = UNDEFINED);
    Vars^[v].Def  := NIL;
    Vars^[v].Next := VarHead;
    VarHead := v;
END RemoveVar;

PROCEDURE SetDef* (s: VarNum; p: TriadePtr);
BEGIN
    Vars^[s].Def := p;
    IF p <> NIL THEN
        p^.Tag  := y_Variable;
        p^.Name := s;
    END;
END SetDef;

PROCEDURE AddUse* (p: ParamPtr);
BEGIN
    IF p^.tag <> y_Variable THEN
        RETURN;
    END;
    p^.next := Vars^ [p^.name].Use;
    p^.prev := NIL;
    Vars^ [p^.name].Use := p;
    IF p^.next <> NIL THEN
        p^.next^.prev := p;
    END;
END AddUse;

PROCEDURE RemoveUse* (p: ParamPtr);
BEGIN
    ASSERT (p^.tag = y_Variable);
    IF p^.prev = NIL THEN
        Vars^[p^.name].Use := p^.next;
    ELSE
        p^.prev^.next := p^.next;
    END;
    IF p^.next <> NIL THEN
        p^.next^.prev := p^.prev;
    END;
    p^.next := NIL;
    p^.prev := NIL;
END RemoveUse;

PROCEDURE RemoveUse_Ex* (p: ParamPtr): ParamPtr;
VAR q: ParamPtr;
BEGIN
    q := p^.next;
    RemoveUse(p);
    RETURN q;
END RemoveUse_Ex;

PROCEDURE FirstUse* (v: VarNum): ParamPtr;
BEGIN
    RETURN Vars^[v].Use;
END FirstUse;

(*
  Существует ли такая переменная?
*)
PROCEDURE IsActive* (v: VarNum): BOOLEAN;
BEGIN
      RETURN (v < NVars) & (Vars^[v].Def <> NIL);
END IsActive;

(* -------------------------------------------------------------------------- *)

(*
  Уплотнить таблицу переменных
*)
PROCEDURE PartialReplace (p: ParamPtr; v1: VarNum);
BEGIN
    WHILE (p <> NIL) DO
          p^.name := v1;
          p := p^.next;
    END;
END PartialReplace;

PROCEDURE ReplaceBy (v2, v1: VarNum);
BEGIN
      PartialReplace (FirstUse (v2), v1);
      Vars^[v1]           := Vars^[v2];
      Vars^[v1].Def^.Name := v1;
END ReplaceBy;

PROCEDURE Squeeze*;
VAR v1, v2: VarNum;
BEGIN
    IF (NVars = 0) OR (VarHead = UNDEFINED) THEN
        RETURN;
    END;
    v1 := 0;
    v2 := NVars-1;
    LOOP
        WHILE Vars^[v2].Def = NIL DO
            DEC (v2);
            IF v1 > v2 THEN
                EXIT;
            END;
        END;
        WHILE Vars^[v1].Def <> NIL DO
            INC (v1);
            IF v1 > v2 THEN
                EXIT;
            END;
        END;
        ReplaceBy (v2, v1);
        DEC (v2);
        INC (v1);
        IF v1 > v2 THEN
            EXIT;
        END;
    END;
    NVars := SYSTEM.SUCC(v2);
    VarHead := UNDEFINED;
END Squeeze;

(* -------------------------------------------------------------------------- *)

PROCEDURE SwapVars* (v1, v2: VarNum);
VAR p: ParamPtr;
    t: VarInfoType;
BEGIN
    p := Vars^[v2].Use;
    WHILE p <> NIL DO
        p^.name := v1;
        p := p^.next;
    END;
    p := Vars^[v1].Use;
    WHILE p <> NIL DO
        p^.name := v2;
        p := p^.next;
    END;
    t         := Vars^[v1];
    Vars^[v1] := Vars^[v2];
    Vars^[v2] := t;
    Vars^[v1].Def^.Name := v1;
    Vars^[v2].Def^.Name := v2;
END SwapVars;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                          Parameters operations                             *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

PROCEDURE(p : ParamPtr) Init*;
BEGIN
  p.Init^();
  p.reverse    := FALSE;
  p.position   := NullPos;
  p.tag        := y_Nothing;
END Init;

VAR
  CallParamType* : PROCEDURE (p: TriadePtr; i: INT): TypeType;
  CallParamSize* : PROCEDURE(p: TriadePtr; i: INT): SizeType;


<* PUSH *>
<* WOFF301+ *>

PROCEDURE CallParamTypeDummy(p: TriadePtr; i: INT): TypeType;
BEGIN
  ASSERT(FALSE);
END CallParamTypeDummy;

PROCEDURE CallParamSizeDummy(p: TriadePtr; i: INT): SizeType;
BEGIN
  ASSERT(FALSE);
END CallParamSizeDummy;

<* POP *>

PROCEDURE ParamType* (p: TriadePtr; i: INT): TypeType;
BEGIN
    CASE p^.Op OF
    | o_power,
      o_rol,
      o_ror,
      o_sar,
      o_shl,
      o_shr:        IF i = 1 THEN
                        RETURN p^.OpType;
                    END;
    | o_bf_get,
      o_bf_put:     IF i = 0 THEN
                        RETURN p^.OpType;
                    ELSE
                        RETURN tune.index_ty;
                    END;
    | o_im,
      o_re:         RETURN t_complex;
    | o_complex:    RETURN t_float;
    | o_copy:       IF i < 2 THEN
                        RETURN t_ref;
                    ELSE
                        RETURN tune.index_ty;
                    END;
    | o_sgnext:     RETURN p.ResType;
    | o_incl,
      o_excl:       IF i <> 0 THEN
                        RETURN tune.index_ty;
                    END;
    | o_loset,
      o_hiset:      RETURN tune.index_ty;
    | o_loadr,
      o_storer:     IF i = 0 THEN
                        RETURN t_ref;
                    END;
    | o_in:         IF i = 0 THEN
                        RETURN tune.index_ty;
                    END;
    | o_eq,
      o_le,
      o_move_eq,
      o_move_le:    IF i < 2 THEN
                        RETURN p^.OpType;
                    END;
    | o_call:       IF i = 0 THEN
                        RETURN t_ref;
                    ELSE
                        RETURN CallParamType(p, i);
                    END;
    | o_val,
      o_cast,
      o_putpar:     RETURN p^.OpType;

    | o_clear:      IF i = 0 THEN RETURN t_ref; END;
                    ASSERT(i=1);
                    RETURN tune.index_ty;

    | o_alloca:     ASSERT(i=0);
                    RETURN tune.index_ty;

    | o_hiword:
                    ASSERT(i=0);
                    RETURN p.OpType;
    | o_clinit:
                ASSERT(i=0);
                    RETURN t_ref;
    | o_cmpswap:
                    IF i = 0 THEN
                RETURN t_ref;
                    ELSE  
                        RETURN p.OpType;
                    END;
    | ELSE
    END;
    RETURN p^.ResType;
END ParamType;

PROCEDURE ParamSize* (p: TriadePtr; i: INT): SizeType;
BEGIN
    CASE p^.Op OF
    | o_power,
      o_rol,
      o_ror,
      o_sar,
      o_shl,
      o_shr:        IF i = 1 THEN
                        RETURN p^.OpSize;
                    END;
    | o_bf_get,
      o_bf_put:     IF i = 0 THEN
                        RETURN p^.OpSize;
                    ELSE
                        RETURN tune.index_sz;
                    END;
    | o_copy:       IF i < 2 THEN
                        RETURN tune.addr_sz;
                    ELSE
                        RETURN tune.index_sz;
                    END;
    | o_sgnext:     RETURN p.ResSize;
    | o_incl,
      o_excl:       IF i <> 0 THEN
                        RETURN tune.index_sz;
                    END;
    | o_loset,
      o_hiset:      RETURN tune.index_sz;
    | o_loadr,
      o_storer:     IF i = 0 THEN
                        RETURN tune.addr_sz;
                    END;
    | o_in:         IF i = 0 THEN
                        RETURN tune.index_sz;
                    END;
    | o_eq,
      o_le,
      o_move_eq,
      o_move_le:    IF i < 2 THEN
                        RETURN p^.OpSize;
                    END;
    | o_call:       IF i = 0 THEN
                        RETURN tune.addr_sz;
                    ELSE
                        RETURN CallParamSize(p, i);
                    END;
    | o_putpar,
      o_val,
      o_cast:       RETURN p^.OpSize;

    | o_clear:      IF i = 0 THEN RETURN tune.addr_sz; END;
                    ASSERT(i=1);
                    RETURN tune.index_sz;

    | o_alloca:     ASSERT(i=0);
                    RETURN tune.index_sz;
    | o_hiword:
                    ASSERT(i=0);
                    RETURN p.ResSize * 2;
    | o_clinit:
                ASSERT(i=0);
                    RETURN tune.addr_sz;
    | o_cmpswap:
                    IF i = 0 THEN
                RETURN tune.addr_sz;
                    ELSE
                        RETURN p.OpSize;
                    END;
    ELSE
    END;
    RETURN p^.ResSize;
END ParamSize;

(* -------------------------------------------------------------------------- *)

PROCEDURE NewParamPtr* (VAR p : ParamPtr);
BEGIN
   NEW(p);
   p.Init();
END NewParamPtr;

PROCEDURE NewParams* (nparams: INT; q: TriadePtr): ParamArray;
VAR p: ParamArray;
    i: INT;
BEGIN
    IF nparams = 0 THEN
        RETURN NIL;
    END;
    NEW (p, nparams);
    FOR i:=0 TO nparams-1 DO
        NewParamPtr  (p^[i]);
        p^[i].triade      := q;
        p^[i].paramnumber := SHORT (i);
    END;
    RETURN p;
END NewParams;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE MakeParNothing*(par: ParamPtr);
BEGIN
  par.Init();
  par.tag  := y_Nothing;
END MakeParNothing;

PROCEDURE MakeParLocal* (p: ParamPtr; l: Local);
BEGIN
    p.Init();
    p^.tag  := y_RealVar;
    p^.name := l;
END MakeParLocal;

PROCEDURE MakeParVar_Ex* (p: ParamPtr; v: VarNum; addUse: BOOLEAN; reverse: BOOLEAN);
BEGIN
    p.Init();
    p^.tag  := y_Variable;
    p^.name := v;
    p^.reverse := reverse;
    IF addUse THEN
      AddUse (p);
    END;
END MakeParVar_Ex;

PROCEDURE MakeParVar* (p: ParamPtr; v: VarNum);
BEGIN
  MakeParVar_Ex(p, v, TRUE, FALSE);
END MakeParVar;

PROCEDURE MakeParNum* (p: ParamPtr; u: VALUE);
BEGIN
    p.Init();
    p^.tag   := y_NumConst;
    p^.value := u;
    p^.reverse := FALSE;
END MakeParNum;

PROCEDURE MakeParFloat* (p: ParamPtr; u: VALUE);
BEGIN
    p.Init();
    p^.tag   := y_RealConst;
    p^.value := u;
    p^.reverse := FALSE;
END MakeParFloat;

PROCEDURE MakeParComplex* (p: ParamPtr; u: VALUE);
BEGIN
    p.Init();
    p^.tag   := y_ComplexConst;
    p^.value := u;
    p^.reverse := FALSE;
END MakeParComplex;

PROCEDURE MakeParNum_Ex* (p: ParamPtr; tag: TagType; u: VALUE; reverse: BOOLEAN);
BEGIN
    p.Init();
    p^.tag   := tag;
    p^.value := u;
    p^.reverse := reverse;
END MakeParNum_Ex;

PROCEDURE MakeParNil*(p: ParamPtr);
BEGIN
    MakeParNum(p, Calc.GetNilPointer());
END MakeParNil;

PROCEDURE MakeParAddr* (p: ParamPtr; u: Local; offset: LONGINT);
BEGIN
    p.Init();
    p^.tag    := y_AddrConst;
    p^.name   := u;
    p^.offset := offset;
    p^.reverse := FALSE;
END MakeParAddr;

PROCEDURE MakeParProc* (p: ParamPtr; u: INTEGER);
BEGIN
    p.Init();
    p^.tag    := y_ProcConst;
    p^.name   := VAL(VarNum, u);
    p^.offset := 0;
    p^.reverse := FALSE;
END MakeParProc;

PROCEDURE SetParamReverse*(s: ParamPtr; val: BOOLEAN);
BEGIN
    s.reverse := val;
END SetParamReverse;

PROCEDURE SetParamTriade*(s: ParamPtr; tr: TriadePtr);
BEGIN
    s.triade := tr;
END SetParamTriade;

(* -------------------------------------------------------------------------- *)

PROCEDURE CopyParamWithRev* (s, d: ParamPtr);
BEGIN
    d^.reverse := s^.reverse;
    d^.tag     := s^.tag;
    d^.value   := s^.value;
    d^.name    := s^.name;
    d^.offset  := s^.offset;
    IF d^.tag = y_Variable THEN
        AddUse (d);
    END;
END CopyParamWithRev;

PROCEDURE CopyParam* (s, d: ParamPtr);
BEGIN
    d^.tag     := s^.tag;
    d^.value   := s^.value;
    d^.name    := s^.name;
    d^.offset  := s^.offset;
    IF d^.tag = y_Variable THEN
        AddUse (d);
    END;
END CopyParam;

(*
  Скопировать параметр, после чего удалить его на старом месте
*)
PROCEDURE MoveParam* (s, d: ParamPtr);
BEGIN
  CopyParam (s, d);
  IF s^.tag = y_Variable THEN
    RemoveUse (s);
  END;
END MoveParam;

PROCEDURE MoveParamWithRev* (s, d: ParamPtr);
BEGIN
  CopyParamWithRev (s, d);
  IF s^.tag = y_Variable THEN
    RemoveUse (s);
  END;
END MoveParamWithRev;

PROCEDURE SwapParams* (s, d: ParamPtr);
VAR rev: BOOLEAN;
    tag: TagType;
    val: VALUE;
    nam: VarNum;
    ofs: LONGINT;
BEGIN
    IF d^.tag = y_Variable THEN
        RemoveUse(d);
    END;
    IF s^.tag = y_Variable THEN
        RemoveUse(s);
    END;
    rev        := d^.reverse;
    d^.reverse := s^.reverse;
    s^.reverse := rev;
    tag        := d^.tag;
    d^.tag     := s^.tag;
    s^.tag     := tag;
    val        := d^.value;
    d^.value   := s^.value;
    s^.value   := val;
    nam        := d^.name;
    d^.name    := s^.name;
    s^.name    := nam;
    ofs        := d^.offset;
    d^.offset  := s^.offset;
    s^.offset  := ofs;

    IF d^.tag = y_Variable THEN
        AddUse (d);
    END;
    IF s^.tag = y_Variable THEN
        AddUse (s);
    END;
END SwapParams;

(* -------------------------------------------------------------------------- *)


(*
  Текстуально совпадают ли два параметра?
*)
PROCEDURE EqParams* (p, q: ParamPtr; t: TypeType; s: SizeType): BOOLEAN;
BEGIN
    IF (p^.tag <> q^.tag) OR (p^.reverse <> q^.reverse) THEN
        RETURN FALSE;
    END;
    CASE p^.tag OF
    | y_Variable,
      y_RealVar,
      y_ProcConst :
        IF p^.name = q^.name THEN
          RETURN TRUE;
        END;
        RETURN FALSE;

    | y_NumConst,
      y_RealConst,
      y_ComplexConst:
        IF t = t_arr THEN RETURN FALSE END;
        IF Calc.CompareValues (pc.sb_equ, p^.value, q^.value, t, s, FALSE) THEN
          RETURN TRUE;
        END;
        RETURN FALSE;

    | y_AddrConst:
        RETURN (p^.name   <> MAX (VarNum)) &
               (p^.name   =  q^.name)     &
               (p^.offset = q^.offset)
        OR
               (p^.name   = MAX (VarNum))  &
               (q^.name   = MAX (VarNum))  &
               (p^.value  = q^.value)     &
               (p^.offset = q^.offset);
    | y_Nothing:
        RETURN TRUE;
    END;
END EqParams;

-- it just remove the parameter from def-use chain
PROCEDURE RemoveParam*(par: ParamPtr);
BEGIN
  IF par.tag = y_Variable THEN
    RemoveUse(par);
  END;
END RemoveParam;

PROCEDURE KillParams* (p: TriadePtr);
VAR
  i : INT;
BEGIN
    IF p^.Params <> NIL THEN
        FOR i:=0 TO LEN(p^.Params^)-1 DO
            IF p^.Params^[i].tag = y_Variable THEN
                RemoveUse(p^.Params^[i]);
            END;
        END;
    END;
END KillParams;

(* ----------------------- Triades ----------------------------------------- *)

VAR
  insideInline*: BOOLEAN;

VAR Opt: OptionsSet;

PROCEDURE SetSilentMode*(isSilent:=TRUE: BOOLEAN);
BEGIN
    IF isSilent THEN
      Opt := OptionsSet{ o_Silent };
    ELSE
      Opt := OptionsSet{ };
    END;
END SetSilentMode;

PROCEDURE SetNormalMode*;
BEGIN
    SetSilentMode(FALSE);
END SetNormalMode;

PROCEDURE GetSilentMode*():BOOLEAN;
BEGIN
    RETURN o_Silent IN Opt;
END GetSilentMode;

PROCEDURE NewTriade* (nparams: INT): TriadePtr;
VAR q: TriadePtr;
BEGIN
    NEW (q);
    q^.Params    := NewParams (nparams, q);
    q^.Next      := NIL;
    q^.Prev      := NIL;
    q^.Read      := NIL;
    q^.Write     := NIL;
    q^.Options   := Opt;
    q^.Prototype := MAX (ProtoNum);
    q^.NodeNo    := MAX (Node);
    q^.Position  := NullPos;
    RETURN q;
END NewTriade;

PROCEDURE NewTriadeInit* (nparams: INT; Op: Operation;
                          OpType: TypeType; OpSize: SizeType): TriadePtr;
VAR q : TriadePtr;
BEGIN
    q := NewTriade (nparams);
    q^.Op      := Op;
    q^.Tag     := y_Nothing;
    q^.OpType  := OpType;
    q^.OpSize  := OpSize;
    q^.ResType := OpType;
    q^.ResSize := OpSize;
    RETURN q;
END NewTriadeInit;

PROCEDURE NewTriadeTInit* (nparams: INT; Op: Operation; Tag: TagType;
                           OpType: TypeType; OpSize: SizeType): TriadePtr;
VAR q : TriadePtr;
BEGIN
    q := NewTriadeInit (nparams, Op, OpType, OpSize);
    q^.Tag := Tag;
    RETURN q;
END NewTriadeTInit;

PROCEDURE NewTriadeLike* (p: TriadePtr; nparams: INT): TriadePtr;
VAR q: TriadePtr;
BEGIN
    q := NewTriade (nparams);
    q^.Options   := p^.Options;
    q^.Prototype := p^.Prototype;
    q^.Tag       := p^.Tag;
    q^.OpType    := p^.OpType;
    q^.OpSize    := p^.OpSize;
    q^.ResType   := p^.ResType;
    q^.ResSize   := p^.ResSize;
    q^.Op        := p^.Op;
    q^.Position  := p^.Position;
    IF p^.Read <> NIL THEN
        q^.Read := BitVect.New (NLocals+1, FALSE);
        BitVect.Move (p^.Read, q^.Read);
    END;
    IF p^.Write <> NIL THEN
        q^.Write := BitVect.New (NLocals+1, FALSE);
        BitVect.Move (p^.Write, q^.Write);
    END;
    RETURN q;
END NewTriadeLike;

PROCEDURE NewTriadeOp*(nparams: INT; op: Operation): TriadePtr;
  VAR q: TriadePtr;
BEGIN
  q    := NewTriade(nparams);
  q.Op := op;
  RETURN q
END NewTriadeOp;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GenResVar* (q: TriadePtr);
BEGIN
  q.Tag := y_Variable;
  GenVar(TEMPORARY, q.Name, q);
END GenResVar;

PROCEDURE is_volatile_loc * (l: Local) : BOOLEAN;
BEGIN
  RETURN (o_Volatile IN Locals^[l].Options)
END is_volatile_loc;

(* -------------------------------------------------------------------------- *)
(* ----------------------- Arguments ---------------------------------------- *)
(* -------------------------------------------------------------------------- *)

TYPE
  PArg* = POINTER TO Arg;

  Arg* = RECORD (pc.backend_node_info)
           mode       *: GenModeSet;

           tag        *: TagType;
           name       *: VarNum;
           value      *: VALUE;
           offset     *: LONGINT;
         END;

PROCEDURE MakeArgLocal* (VAR arg: Arg; l: Local; offset: LONGINT);
BEGIN
    arg.mode := GenModeSet{};

    arg.tag := y_RealVar;
    arg.name := l;
    arg.value := NIL;
    arg.offset := offset;
--  arg.parpos   := NullPos;
--  arg.operpos  := NullPos;    (* position of previous operation *)
END MakeArgLocal;

PROCEDURE MakeArgVar* (VAR arg: Arg; v: VarNum);   (* no AddUse !! *)
BEGIN
    arg.mode := GenModeSet{};

    arg.tag := y_Variable;
    arg.name := v;
    arg.value := NIL;
    arg.offset := 0;
--  arg.parpos   := NullPos;
--  arg.operpos  := NullPos;    (* position of previous operation *)

END MakeArgVar;

PROCEDURE MakeArgNum* (VAR arg: Arg; u: VALUE);
BEGIN
    arg.mode := GenModeSet{};
    arg.tag := y_NumConst;
    arg.name := UNDEFINED;
    arg.value := u;
    arg.offset := 0;
--  arg.parpos   := NullPos;
--  arg.operpos  := NullPos;    (* position of previous operation *)
END MakeArgNum;

PROCEDURE MakeArgFloat* (VAR arg: Arg; u: VALUE);
BEGIN
    arg.mode := GenModeSet{};

    arg.tag := y_RealConst;
    arg.name := UNDEFINED;
    arg.value := u;
    arg.offset := 0;
--  arg.parpos   := NullPos;
--  arg.operpos  := NullPos;    (* position of previous operation *)
END MakeArgFloat;

-- make arg to be 'address(u)+offs'
PROCEDURE MakeArgAddr* (VAR arg: Arg; u: Local; offset: LONGINT);
BEGIN
    arg.mode := GenModeSet{};
    IF u # MAX(Local) THEN
      IF is_volatile_loc(u) THEN INCL(arg.mode, VOLATILE) END;
    END;
    arg.tag := y_AddrConst;
    arg.name := u;
    arg.value := NIL;
    arg.offset := offset;
--  arg.parpos   := NullPos;
--  arg.operpos  := NullPos;    (* position of previous operation *)
END MakeArgAddr;

PROCEDURE MakeArgNil*(VAR arg: Arg);
BEGIN
    MakeArgNum(arg, Calc.GetNilPointer() );
END MakeArgNil;

PROCEDURE MakeArgNothing*(VAR arg: Arg);
BEGIN
    arg.mode := GenModeSet{};
    arg.tag := y_Nothing;
    arg.name := UNDEFINED;
    arg.value := NIL;
    arg.offset := 0;
END MakeArgNothing;

(*  Сделать аргументом адрес константы *)
PROCEDURE MakeArgAddrConst* (VAR arg: Arg; u: VALUE; offset: LONGINT);
BEGIN
    arg.mode := GenModeSet{};

    arg.tag    := y_AddrConst;
    arg.name   := MAX(Local);
    arg.value  := u;
    arg.offset := offset;
--  arg.parpos   := NullPos;
--  arg.operpos  := NullPos;    (* position of previous operation *)

END MakeArgAddrConst;

PROCEDURE MakeArgProc* (VAR arg: Arg; u: VarNum);
BEGIN
    arg.mode := GenModeSet{};

    arg.tag := y_ProcConst;
    arg.name := u;
    arg.value := NIL;
    arg.offset := 0;
--  arg.parpos   := NullPos;
--  arg.operpos  := NullPos;    (* position of previous operation *)

END MakeArgProc;

(*---------------------------- Parameters ------------------------------------*)
(*----------------------------------------------------------------------------*)

PROCEDURE ParmByArg*(prm: ParamPtr; arg-: Arg);
<* IF ASSERT THEN *>  VAR ok: BOOLEAN; <* END *>
BEGIN
<* IF ASSERT THEN *>
  CASE arg.tag OF
  | y_NumConst
  , y_RealConst:
      ok := ((arg.value # NIL) & (arg.value IS pc.VALUE));
  | y_AddrConst:
      ok := ( (0<=arg.name) & (arg.name < NLocals) OR
        (arg.name=MAX(Local)) & (arg.value # NIL) & (arg.value IS pc.VALUE));
  | y_ProcConst:
      ok := (0<=arg.name) (*& (arg.name <= opProcs.NProc)*);
  | y_RealVar:
      ok := ((0<=arg.name) & (arg.name < NLocals));
  | y_Variable:
      ok := ((0<=arg.name) & (arg.name < NVars));
  ELSE ok := FALSE
  END;
  ASSERT(ok, 100 + VAL(SHORTINT,arg.tag));
<* END *>
  prm.tag      := arg.tag;
  prm.value    := arg.value;
  prm.name     := arg.name;
  prm.offset   := arg.offset;

  IF arg.tag = y_Variable THEN
    AddUse(prm)
  END;
  IF prm.reverse THEN 
    ASSERT(FALSE);
  END;
END ParmByArg;

PROCEDURE ArgByParm*(VAR arg: Arg; prm-: ParamPtr);
BEGIN
  arg.tag      := prm.tag;
  arg.value    := prm.value;
  arg.name     := prm.name;
  arg.offset   := prm.offset;

END ArgByParm;

PROCEDURE GT*(p1, p2: pc.VALUE; ty: TypeType; sz: SizeType; strict: BOOLEAN): BOOLEAN;
BEGIN
  RETURN Calc.CompareValues(pc.sb_gtr, p1, p2, ty, sz, strict);
END GT;

PROCEDURE EQ*(p1, p2: pc.VALUE; ty: TypeType; sz: SizeType; strict: BOOLEAN): BOOLEAN;
BEGIN
  RETURN Calc.CompareValues(pc.sb_equ, p1, p2, ty, sz, strict);
END EQ;

(*----------------------------------------------------------------------------*)
(*----------------------------------------------------------------------------*)

<* IF ~ NODEBUG THEN *>

PROCEDURE PrintLocal* (l: Local);
BEGIN
    IF Locals[l].Name <> NIL THEN
        opIO.print (Locals [l].Name^);
    ELSE
        opIO.print ("tmp%d", l);
    END;
END PrintLocal;

PROCEDURE PrintVar* (r: VarNum);
BEGIN
    IF r = UNDEFINED THEN
        opIO.print ("?");
    ELSE
        IF Vars^[r].LocalNo = TEMPORARY THEN
            opIO.print ("t");
        ELSE
            PrintLocal (Vars^[r].LocalNo);
            opIO.print ("_");
        END;
        opIO.print ("%d", r);
    END;
END PrintVar;

<* END *>

PROCEDURE is_constant (l: Local): BOOLEAN;
  VAR o: pc.OBJECT;
BEGIN
  o := Locals[l].Obj;
  RETURN (o # NIL) AND
         ( (pc.xot_final IN o.xtags) OR
           (o.mode = pc.ob_cons)
         )
END is_constant;

BEGIN
  NullPos := env.null_pos;
  SetNormalMode;
  CallParamSize := CallParamSizeDummy;
  CallParamType := CallParamTypeDummy;
END ir.
