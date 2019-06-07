(*
   общие типы данных, структуры, константы и т.д.
   для всех, кто испольует понятие длинной константы при кодогенерации.
   History:
   put new entries on the top

   17.02.97             KDV     LCDesc is subclass of LCDef_D.LCDesc
                                IsLongConst
                                CompareConstsValuesStrict
                                FindReallyLongConsts
                                IsReallyLongConst
                                are now procedure variables which have to be initialized
                                by LCDef_I
   09.09.96             ENAL    AddUse: Nesting считается так же, как в Color
                                CompareConstValues: 68k - считаю, что размер
                                                    целой константы всегда 4
                                константа InitProf
   03.05.96 11:26       KDV     Created
*)
<* IF NOT GENDEBUG THEN *> <* +PROCINLINE *> <* END *>
<*+WOFF301 *>
MODULE LCDef;

<* +o2addkwd *>
FROM SYSTEM IMPORT PRED,SUCC;
IMPORT sys:= SYSTEM,
       pcK,
       Calc,
       ir,
<* IF ~nodebug THEN *>
       out := opIO,
       prc := opProcs,
<* END *>
       D := LCDef_D,
       bv := BitVect;

TYPE
    LCNum*      = D.LCNum;
    BaseNum*    = INTEGER;
    ConstScale* = bv.BitVector;

VAR NLCs*       : LCNum;

(*
     теги для собственного пользования
*)

CONST
      -- Const of ir.TagType
      y_BaseConst*      = SUCC(ir.y_Variable);
      y_ProcTOCConst*   = SUCC(y_BaseConst);
      y_CASETableConst* = SUCC(y_ProcTOCConst);
      y_PseudoRealConst*= SUCC(y_CASETableConst);
      y_PseudoNumConst *= SUCC(y_PseudoRealConst);
      y_LastConst      *= y_PseudoNumConst;

      UNDEFINED        *= D.UNDEFINED;

      ConstTags        *= {ir.y_NumConst,
                           ir.y_AddrConst,
                           ir.y_RealConst,
                           ir.y_ProcConst,
                           y_BaseConst..y_LastConst};
CONST U2F* = VAL(ir.LCNum, 0);
      I2F* = VAL(ir.LCNum, 1);
      F2U* = VAL(ir.LCNum, 2);
      F2I* = VAL(ir.LCNum, 3);

VAR   U2FNum*,
      I2FNum*,
      F2UNum*,
      U2FAuxNum* : LCNum;


--------------------------------------------------------------------------------

(* LCDef interface depending on backends *)
TYPE LCDef_IDB     *= POINTER TO LCDef_IDB_Rec;
     LCDef_IDB_Rec *= RECORD
                        InitProf*   : LONGINT;
                            -- начальная выгода от размещения константы на регистре
                      END;

PROCEDURE(i : LCDef_IDB) IsLongConst*(p: ir.ParamPtr;
                                      collecting: BOOLEAN)
                                     : BOOLEAN;
BEGIN
  ASSERT(FALSE);
END IsLongConst;

PROCEDURE(i : LCDef_IDB) CompareConstsValuesStrict(p1,p2 : ir.ParamPtr;
                                                   t1,t2 : Calc.TypeType;
                                                   s1,s2 : Calc.SizeType)
                                                  : BOOLEAN;
BEGIN
  ASSERT(FALSE);
END CompareConstsValuesStrict;

PROCEDURE(i : LCDef_IDB) FindReallyLongConsts*() : BOOLEAN;
BEGIN
  ASSERT(FALSE);
END FindReallyLongConsts;

PROCEDURE(i : LCDef_IDB) IsReallyLongConst*(lc : LCNum) : BOOLEAN;
BEGIN
  ASSERT(FALSE);
END IsReallyLongConst;

VAR IDB *: LCDef_IDB;

--------------------------------------------------------------------------------

(* массив с номерами баз (для всех глобалов, нумеруемых через
   ir.Local)
   как длинных констант
   если ExternalLCBases[l] = ir.UNDEFINED то у l нет базы
*)
VAR ExternalLCBases* : POINTER TO ARRAY OF LCNum;

PROCEDURE GetLCBaseByLocal*(l : ir.Local) : LCNum;
BEGIN
    RETURN ExternalLCBases[l];
END GetLCBaseByLocal;

--------------------------------------------------------------------------------
(*
 массив с номерами длинных констант =
 смещение локала + смещение от локала для всех переменных
*)
VAR VarOffsetNums* : POINTER TO ARRAY OF LCNum;

--------------------------------------------------------------------------------

(* LongConsts -  массив дескрипторов длинных констант,
   равно из внутреннего представления и порожденных при нумерации,

   содержит сами констаты,
            множества участков, в которых константы используются,
            выгоду от держания константы на регистре
            другие атрибуты -- см. XXX_LCATTR константы
*)

TYPE LCAttrs *= (
       LiveAtCall_LCATTR,  -- *= 0;
       UsedEA_LCATTR,      -- *= 1;

     UsedAsByte_LCATTR,    -- *= 2;
     UsedAsWord_LCATTR,    -- *= 3;
     UsedAsLong_LCATTR,    -- *= 4;
     UsedAsHuge_LCATTR,    -- *= 5;
     UsedInLogical_LCATTR, -- *= 6;
     ReallyLong_LCATTR,    -- *= 7;
     UsedInLoop_LCATTR     -- *= 8;
     );

TYPE LCAttrsSet *= PACKEDSET OF LCAttrs;

TYPE NodeScale* = bv.BitVector;
     LCDesc*    = RECORD(D.LCDesc)
                      const*        : ir.ParamPtr;  --- собственно константа
                      profit*       : ir.INT;       --- выгода от нахождения
                                                    --- на регистре
                      real_profit*  : ir.INT;       --- Unnormalized profit
                      used_in*      : NodeScale;    --- в каких участках
                                                    --- используется
                      attrs*        : LCAttrsSet;   --- служебные аттрибуты
                      firstlive*    : ir.TSNode;      --  первый узел, в котором жива константа
                      lastlive*     : ir.TSNode;      --  посл. узел, в котором жива константа
                  END;

     LCArray*   = POINTER TO ARRAY OF LCDesc;

CONST
    Nesting     = ARRAY OF LONGINT { 1, 2, 4, 8, 16 };
    MaxNest     = 4;
    MaxProf     = 32;

PROCEDURE(VAR d : LCDesc) Init*();
BEGIN
  d.const       := NIL;
  d.profit      := IDB.InitProf;
  d.real_profit := IDB.InitProf;
  d.used_in     := bv.New(ORD(ir.Nnodes),FALSE);
  d.attrs       := LCAttrsSet{};
  d.firstlive   := ir.UndefTSNode;
  d.lastlive    := ir.UndefTSNode;
  d.Init^();
END Init;

VAR  LongConsts* :  LCArray;
     NodesWithCalls* : NodeScale; -- узлы с вызовами, вычисляется в MakeLCInfo

<* IF ~nodebug THEN *>

PROCEDURE ShowLocal (l: ir.Local);
BEGIN
  IF ir.Locals[l].Name <> NIL THEN out.print (ir.Locals[l].Name^);
  ELSE                             out.print ('tmp%d', l);
  END;
END ShowLocal;

PROCEDURE ShowProcName (p: prc.ProcNum);
  VAR name: ir.NameType;
BEGIN
  name := prc.ProcName(p);
  out.print (name^);
END ShowProcName;

PROCEDURE ShowVar (r: ir.VarNum);
BEGIN
  IF ir.Vars^[r].LocalNo = ir.TEMPORARY THEN
    out.print ('t%d', r);
  ELSE
    ShowLocal (ir.Vars^[r].LocalNo);
    out.print ('_%d', r);
  END;
END ShowVar;

PROCEDURE ShowParam* (p: ir.ParamPtr);
VAR s: ARRAY 200 OF CHAR;
BEGIN
  CASE p.tag OF
  | ir.y_Nothing:   out.print('UNDEFINED');
  | ir.y_NumConst:  p.value.value_to_str(s, pcK.flag_c);
                    out.print(s);
  | ir.y_RealConst: p.value.value_to_str(s, pcK.flag_c);
                    out.print(s);
  | ir.y_RealVar:   ShowLocal(p.name);
  | ir.y_Variable:  ShowVar(p.name);
  | ir.y_ProcConst: ShowProcName(VAL(prc.ProcNum, p.name));
                    IF p.offset <> 0 THEN
                      IF p.offset > 0 THEN out.print('+'); END;
                      out.print("%d", p.offset);
                    END;
  | ir.y_AddrConst: IF p.offset <> 0 THEN out.print('('); END;
                    out.print('&');
                    IF p.name = ir.UNDEFINED THEN out.print('CONST');
                    ELSE                           ShowLocal (p.name);
                    END;
                    IF p.offset > 0 THEN out.print ('+'); END;
                    IF p.offset <> 0 THEN out.print ("%d)", p.offset); END;
  | y_BaseConst:    out.print ("base %d offset %d", p.name,p.offset);
  | y_CASETableConst:
                    out.print ("case table with offset %d", p.offset);
  | y_PseudoRealConst:
                    out.print("pseudo real const ");
                    CASE VAL(LCNum, p.name) OF
                    | U2F : out.print("VAC.U2F");
                    | I2F : out.print("VAC.I2F");
                    | F2U : out.print("VAC.F2U");
                    END;
  | y_PseudoNumConst:
                    out.print("pseudo num const ");
                    CASE VAL(LCNum, p.name) OF
                    | U2F : out.print("U2F");
                    | I2F : out.print("I2F");
                    | F2U : out.print("F2U");
                    | F2I : out.print("F2I");
                    END;
  | ELSE            out.print ('?%d', p.tag);
  END;
END ShowParam;

PROCEDURE ShowScale(v : bv.BitVector; len : LONGINT);
VAR i : LONGINT;
BEGIN
  FOR i := 0 TO len-1 DO
    IF bv.In(v,i) THEN out.print("%d ",i); END;
  END;
END ShowScale;

PROCEDURE ShowLCInfo*();
VAR lc : LCNum;
BEGIN
  IF NLCs=ir.ZEROLCNum THEN RETURN END;
  out.print("LCInfo:\n");
  FOR lc := ir.ZEROLCNum TO PRED(NLCs) DO
    out.print("\tlc=%d, ", lc);
    ShowParam (LongConsts[lc].const);
    IF LiveAtCall_LCATTR IN LongConsts[lc].attrs THEN
        out.print(", live at call");
    END;
    IF UsedEA_LCATTR IN LongConsts[lc].attrs THEN
        out.print(", used as EA");
    END;
    out.print(", profit=%d, used_in=(",LongConsts[lc].profit);
    ShowScale(LongConsts[lc].used_in,ORD(ir.Nnodes));
    out.print(")\n");
  END;
END ShowLCInfo;

<* END *>

--------------------------------------------------------------------------------

(* сравнение двух констант по VALUE*)


PROCEDURE CompareConstsValues(p1,p2 : ir.ParamPtr) : BOOLEAN;
   VAR t1, t2 : Calc.TypeType;
       s1, s2 : Calc.SizeType;
BEGIN
    t1 := ir.ParamType(p1^.triade,p1^.paramnumber);
    s1 := ir.ParamSize(p1^.triade,p1^.paramnumber);
    t2 := ir.ParamType(p2^.triade,p2^.paramnumber);
    s2 := ir.ParamSize(p2^.triade,p2^.paramnumber);
    (* затычка из-за того, что в параметрах не хранится тип *)
    IF t1 = Calc.t_void THEN
        IF s1 <= 4 THEN t1 := Calc.t_int;
        ELSE t1 := Calc.t_float;
        END;
    END;
    IF t2 = Calc.t_void THEN
        IF s2 <= 4 THEN t2 := Calc.t_int;
        ELSE t2 := Calc.t_float;
        END;
    END;
    RETURN IDB.CompareConstsValuesStrict(p1,p2,t1,t2,s1,s2);
END CompareConstsValues;

--------------------------------------------------------------------------------

(* сравнение двух констант по полной программе *)
PROCEDURE TheSameConsts*(p1,p2 : ir.ParamPtr) : BOOLEAN;
BEGIN
  IF (p1^.tag <> p2^.tag) THEN RETURN FALSE END;
  CASE p1^.tag OF
  | ir.y_NumConst,
    ir.y_RealConst:   RETURN CompareConstsValues(p1,p2);
  | ir.y_AddrConst:   RETURN (p1^.name = p2^.name) &
                             (p1^.offset = p2^.offset);
  | ir.y_ProcConst,
    y_PseudoRealConst,
    y_PseudoNumConst: RETURN p1^.name = p2^.name;
  | y_BaseConst:      RETURN p1^.offset = p2^.offset;
  | y_CASETableConst :RETURN p1^.triade = p2^.triade;
  END;
END TheSameConsts;

--------------------------------------------------------------------------------

PROCEDURE FindLCNum* (p: ir.ParamPtr): LCNum;
BEGIN
  RETURN p^.lcnum;
END FindLCNum;

--------------------------------------------------------------------------------

(* добавлает участок n к использующим константу с дескриптором lc_desc,
   перевычисляя выгоду
*)
PROCEDURE AddUse*(VAR lc_desc : LCDesc;  n : ir.Node);
BEGIN
  IF ir.Nodes^[n].Nesting > MaxNest THEN
    INC(lc_desc.profit,MaxProf);
  ELSE
    IF ir.Nodes[n].LoopNo <> ir.UndefLoop THEN
      INCL(lc_desc.attrs,UsedInLoop_LCATTR);
    END;
    INC(lc_desc.profit,Nesting[ir.Nodes^[n].Nesting]);
  END;
  bv.Incl(lc_desc.used_in,ORD(n));
END AddUse;

--------------------------------------------------------------------------------

(* добавляет к множеству участков, в которых используется константа lc
   все участки, в которых используется переменная v,
   если with_def, то добавляется и участок, в котором переменная v рождается
   (или рожается :-))
*)

PROCEDURE AddUseList*(lc : LCNum; v : ir.VarNum; with_def : BOOLEAN);
    VAR p : ir.ParamPtr;
BEGIN
  IF with_def THEN AddUse(LongConsts^[lc], ir.Vars^[v].Def^.NodeNo); END;
  p := ir.Vars^[v].Use;
  WHILE p <> NIL DO
    AddUse(LongConsts^[lc],p^.triade^.NodeNo);
    p := p^.next;
  END;
END AddUseList;

--------------  Возня с действительно длинными константами -------------------
--                      и настоящими смещениями                             --
------------------------------------------------------------------------------

PROCEDURE(i : LCDef_IDB) IsShortValue_SExt*(v : LONGINT) : BOOLEAN;
BEGIN -- Is Short Value Sign-Extended 
  ASSERT(FALSE);
END IsShortValue_SExt;

PROCEDURE(i : LCDef_IDB) IsShortValue_ZExt*(v : LONGINT) : BOOLEAN;
BEGIN  -- Is Short Value Zero-Extended 
  ASSERT(FALSE);
END IsShortValue_ZExt;


VAR ReallyLongConsts* : ConstScale;

PROCEDURE EvalRealOffsetByParam*(p : ir.ParamPtr; FRAME_SIZE : LONGINT) : LONGINT;
BEGIN
<* IF ~NODEBUG THEN *>
  ASSERT(p.tag = ir.y_AddrConst);
<* END *>
  IF ir.IsExternal(p.name) THEN
    RETURN ir.Locals[p.name].Offset + p.offset;
  ELSE
    RETURN ir.Locals[p.name].Offset + p.offset + FRAME_SIZE;
  END;
END EvalRealOffsetByParam;

PROCEDURE EvalRealOffset*(lc : LCNum; FRAME_SIZE : LONGINT) : LONGINT;
VAR c : ir.ParamPtr;
BEGIN
<* IF ~NODEBUG THEN *>
  ASSERT(LongConsts[lc].const.tag = ir.y_AddrConst);
<* END *>
  c := LongConsts[lc].const;
  IF ir.IsExternal(c.name) THEN
    RETURN ir.Locals[c.name].Offset + c.offset;
  ELSE
    RETURN ir.Locals[c.name].Offset + c.offset + FRAME_SIZE;
  END;
END EvalRealOffset;

(*
  Для 68k приходится обходить граф и решать, константа длинная или нет -
  может зависить от того, где находится результат триады.
*)


CONST y_Fs* = {ir.y_RealConst,y_PseudoRealConst};
PROCEDURE LCLCCanBeHooked*(lc1,lc2 : LCNum) : BOOLEAN;
BEGIN
  RETURN (LongConsts[lc1].const.tag IN y_Fs) =
         (LongConsts[lc2].const.tag IN y_Fs);
END LCLCCanBeHooked;

PROCEDURE LCVarCanBeHooked*(lc : LCNum; v : ir.VarNum) : BOOLEAN;
BEGIN
  RETURN (LongConsts[lc].const.tag IN y_Fs) =
         (ir.Vars[v].Def.ResType = ir.t_float);
END LCVarCanBeHooked;

PROCEDURE VarVarCanBeHooked*(v1,v2 : ir.VarNum) : BOOLEAN;
BEGIN
  RETURN (ir.Vars[v1].Def.ResType = ir.t_float) =
         (ir.Vars[v2].Def.ResType = ir.t_float);
END VarVarCanBeHooked;
--------------------------------------------------------------------------------


(* массив, содержащий для каждого участка используемые в нем константы *)
TYPE
    ConstsOfNodesDesc* = RECORD
                             Live*,
                             Work*,
                             LoadAtTop*,
                             LoadAtBottom* :  ConstScale;
                         END;
VAR
    ConstsOfNodes* : POINTER TO ARRAY OF ConstsOfNodesDesc;

<* IF ~nodebug THEN *>
PROCEDURE ShowLCLive*();
VAR n  : ir.Node;
BEGIN
  IF NLCs=ir.ZEROLCNum THEN RETURN END;
  out.print("LCLiveness:\n");
  FOR n := ir.ZERONode TO PRED(ir.Nnodes) DO
    out.print("\tnode=%d, Live=(",n);
    ShowScale(ConstsOfNodes[n].Live,ORD(NLCs));
    out.print(")\n");
    out.print("\t        LoadAtTop=(",n);
    ShowScale(ConstsOfNodes[n].LoadAtTop,ORD(NLCs));
    out.print(")\n");
    out.print("\t        LoadAtBottom=(",n);
    ShowScale(ConstsOfNodes[n].LoadAtBottom,ORD(NLCs));
    out.print(")\n");
  END;
END ShowLCLive;
<* END *>

PROCEDURE InitConstsOfNodes*();
VAR n  : ir.Node;
    lc : LCNum;
BEGIN
  IF NLCs = ir.ZEROLCNum THEN RETURN; END;
  FOR n := ir.ZERONode TO PRED(ir.Nnodes) DO
    bv.Fill(ConstsOfNodes^[n].Live,FALSE,ORD(NLCs));
    FOR lc:=ir.ZEROLCNum TO PRED(NLCs) DO
      IF IDB.IsReallyLongConst(lc) & bv.In(LongConsts^[lc].used_in,ORD(n)) THEN
          bv.Incl(ConstsOfNodes^[n].Live,ORD(lc));
      END;
    END;
    ConstsOfNodes^[n].Work := bv.New(ORD(NLCs),FALSE);
    bv.Fill(ConstsOfNodes^[n].LoadAtTop,FALSE,ORD(NLCs));
    bv.Fill(ConstsOfNodes^[n].LoadAtBottom,FALSE,ORD(NLCs));
  END;
END InitConstsOfNodes;


------ Возня с короткими константами для выделения им желаемых регистров -------
--   Накапливание информации о коротких константах, превращеных в длинные     --
--------------------------------------------------------------------------------
TYPE
     SCNum = LONGINT;
     LongSConstDesc  = RECORD
        lc   : ir.ParamPtr;
        val  : LONGINT;
     END;
     LongSConstList = POINTER TO ARRAY OF LongSConstDesc;

CONST
     ZEROSCNum = SCNum{0};
VAR  -- короткие константы ставшие длинными (LCNumer_I.ProcessParamAux)
     LongSConsts  : RECORD
        list      : LongSConstList;
        list_len  : SCNum;
     END;

PROCEDURE FindLongSConstWithValue * (val: LONGINT): ir.LCNum;
VAR i: SCNum;
BEGIN
  FOR i:= ZEROSCNum TO PRED(LongSConsts.list_len) DO
    IF LongSConsts.list[i].val = val THEN
      RETURN LongSConsts.list[i].lc.lcnum;
    END;
  END;
  RETURN ir.LCUNDEFINED;
END FindLongSConstWithValue;

PROCEDURE Add2LongSConsts * (lc: ir.ParamPtr; val: LONGINT): BOOLEAN;
VAR i: SCNum;
    same_lc: ir.LCNum;
    new: LongSConstList;
BEGIN
  same_lc:= FindLongSConstWithValue(val);
  IF ( same_lc # ir.LCUNDEFINED ) THEN
    lc.lcnum:= same_lc;
    RETURN FALSE;
  ELSE
    IF LongSConsts.list = NIL THEN
      NEW(LongSConsts.list, 8);
     ELSIF LongSConsts.list_len = LEN(LongSConsts.list^) THEN
      NEW(new, ORD(LongSConsts.list_len)*2);
      FOR i:= ZEROSCNum TO PRED(LongSConsts.list_len) DO
        new[i]:= LongSConsts.list[i]
      END;
      LongSConsts.list := new;
    END;
    LongSConsts.list[LongSConsts.list_len].lc := lc;
    LongSConsts.list[LongSConsts.list_len].val := val;
    INC(LongSConsts.list_len);
    RETURN TRUE;
  END;
END Add2LongSConsts;


--------------------------------------------------------------------------------
--                            Освобождалки                                    --
--------------------------------------------------------------------------------
PROCEDURE FreeLongSConsts();
BEGIN
  LongSConsts.list:= NIL;
  LongSConsts.list_len:= ZEROSCNum;
END FreeLongSConsts;

PROCEDURE FreeConstsOfNodes();
VAR n : ir.Node;
BEGIN
  IF NLCs <> ir.ZEROLCNum THEN
    FOR n := ir.ZERONode TO PRED(ir.Nnodes)DO
      bv.Free(ConstsOfNodes^[n].Live);
      bv.Free(ConstsOfNodes^[n].LoadAtTop);
      bv.Free(ConstsOfNodes^[n].LoadAtBottom);
    END;
  END;
  ConstsOfNodes := NIL;
END FreeConstsOfNodes;

PROCEDURE FreeLCArrays*();
VAR lc : LCNum;
BEGIN
  FOR lc := ir.ZEROLCNum TO PRED(NLCs) DO bv.Free(LongConsts^[lc].used_in); END;
  ExternalLCBases := NIL;
  VarOffsetNums   := NIL;
  LongConsts      := NIL;
  FreeConstsOfNodes;
  FreeLongSConsts();
  bv.Free(ReallyLongConsts);
  bv.Free(NodesWithCalls);
END FreeLCArrays;

BEGIN
  NEW(IDB);
  IDB.InitProf := -1;
  FreeLongSConsts();
END LCDef.
