(*
  18.02.97      KDV     Created
                        Interface of r_modules with up levels
*)
MODULE RDefs;

IMPORT ir, D := RDefs_D, BurgNT;
IMPORT EXCEPTIONS;
IMPORT BitVect;

CONST UNDEF_REG   *= D.UNDEF_REG;
      SPILLED_REG *= D.SPILLED_REG;

TYPE
    Location *=
        RECORD
            tag*    : BurgNT.NT;       -- alloc.tag (IN_*)
            reg*    : D.Reg;       -- (tag=IN_REG)   reg   - # of register
            mem*    : ir.Local;       -- (tag=IN_LOCAL) local - # of local
            offs*   : LONGINT;        -- (tag=IN_LOCAL) offset
            temp*   : BOOLEAN;        -- TRUE for intermediate nodes
            wasUnited* : BOOLEAN;
            profit*,
            real_profit* : LONGINT;        -- of being allocated in reg
            hasLongLifeTime*   : BOOLEAN;        -- TRUE for temp DAG variables, being used for composite addresation
            isSpilled*:     BOOLEAN;
            usedFor*:     ir.VarNum;    --for temp nodes
        END;
VAR
    Loc*   : POINTER TO ARRAY OF Location; -- var locations array INDEXED BY ir.VarNum
    LCLoc* : POINTER TO ARRAY OF Location; -- LC  locations array INDEXED BY ir.LCNum

TYPE
    DAGNODE      *= POINTER TO TREE;
    TREE         *= RECORD(D.TREE)
                        op*:            ir.Operation;
                        l*, r*:         DAGNODE;   -- левый, правый сыновья
                        prev*, next*, parent*:   DAGNODE;   -- следующее,
                                                        -- предыдущее дерево
                        par*:           ir.ParamPtr;       -- если op = ir.o_par
                        tr*:            ir.TriadePtr;      -- триада
                        nt*:            BurgNT.NT;       -- IN_...
                        sz*:            ir.SizeType;       -- размер как параметра

                        cost*: BurgNT.CostArray;
      			rule*: BurgNT.RuleArray;
                        forbidden*: 	ARRAY BurgNT.NT OF BOOLEAN;
                    END;

    Contents  *= D.Contents;

VAR NodeInfo*:    POINTER TO ARRAY OF D.NodeInfoType;

VAR
    Dag*:        POINTER TO ARRAY OF DAGNODE;
    Trees*:      POINTER TO ARRAY OF DAGNODE;

VAR
    HookedVars* : ir.BitMatrixVar;
    HookedLCLC*,
    HookedLCVar*   : ir.BitMatrixLC;
    HookedsWith8* : BitVect.BitVector;

--------------------------------------------------------------------------------

VAR source*: EXCEPTIONS.ExceptionSource;

TYPE
    RDefs_IDB *= POINTER TO RDefs_IDB_Rec;
    RDefs_IDB_Rec *= RECORD END;
VAR
    IDB *: RDefs_IDB;

PROCEDURE(idb : RDefs_IDB) EnterNode*(n : ir.Node; prologue : BOOLEAN);
BEGIN
  ASSERT(FALSE);
END EnterNode;

PROCEDURE(idb : RDefs_IDB) ExitNode*(n : ir.Node);
BEGIN
  ASSERT(FALSE);
END ExitNode;

PROCEDURE(idb : RDefs_IDB) FiFreeReg*(v : ir.VarNum);
BEGIN
  ASSERT(FALSE);
END FiFreeReg;

PROCEDURE(idb : RDefs_IDB) FiMove*(v : ir.VarNum; p : ir.ParamPtr);
BEGIN
  ASSERT(FALSE);
END FiMove;

PROCEDURE(idb : RDefs_IDB) FiLoop*(p : ir.VarNumArray; n : LONGINT);
BEGIN
  ASSERT(FALSE);
END FiLoop;

PROCEDURE(idb : RDefs_IDB) SetUsages*(p : DAGNODE);
BEGIN
  ASSERT(FALSE);
END SetUsages;

PROCEDURE(idb : RDefs_IDB)
         SameMemory*(v : ir.VarNum; p : ir.ParamPtr) : BOOLEAN;
BEGIN
  ASSERT(FALSE);
END SameMemory;

PROCEDURE(idb : RDefs_IDB)
         IntersectMemory*(v : ir.VarNum; p : ir.ParamPtr) : BOOLEAN;
BEGIN
<* IF TARGET_386 THEN *>
  ASSERT(FALSE);
<* ELSE *>
  RETURN idb.SameMemory(v,p);
<* END *>
END IntersectMemory;

<* IF ~nodebug THEN *>
PROCEDURE(idb : RDefs_IDB) PrintRegs*;
BEGIN
  ASSERT(FALSE);
END PrintRegs;
<* END *>

(* generic procedure - used by iSelect to setup emitters *)
PROCEDURE(idb : RDefs_IDB) InitOutput*;
BEGIN
END InitOutput;

BEGIN
  Loc := NIL;
  LCLoc := NIL;

  Dag := NIL;
  Trees := NIL;

  HookedVars := NIL;
  HookedLCLC := NIL;
  HookedLCVar := NIL;
  NodeInfo := NIL;

  EXCEPTIONS.AllocateSource(source);
END RDefs.
