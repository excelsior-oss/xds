<* Storage + *>

IMPLEMENTATION MODULE Expr;

IMPORT sys := SYSTEM;
IMPORT exc := EXCEPTIONS;
IMPORT exm := M2EXCEPTION;
IMPORT fmt := FormStr;
IMPORT str := Strings;
IMPORT tm  := TimeConv;
IMPORT clk := SysClock;

IMPORT xs  := xStr;
IMPORT sor := Sort;
IMPORT msg := MsgNo;
IMPORT opt := Options;
IMPORT fil := File;

IMPORT r2s := Real2Str;
IMPORT i2s := Int2Str;

IMPORT brk := Breaks;
IMPORT lst := Lists;

IMPORT eve := Events;
IMPORT exe := ExeMain;
IMPORT mem := Exe_Mem;
IMPORT erc := ExeReact;

IMPORT stk := CallStk;

IMPORT kt  := KrnTypes;
IMPORT kexe:= KrnExec;
IMPORT dsm := Krn_Dasm;

IMPORT tls := DI_Tools;
IMPORT dt  := DI_Types;


FROM SYSTEM IMPORT ADDRESS, ADR, BYTE, WORD;

FROM Printf IMPORT  printf;


<* IF xd_batch_included THEN *>

IMPORT prc := PckReact;

<* END *>


<* IF DEST_K26 THEN *>

IMPORT typ := PckTypes;
IMPORT bas := PckBase;

IMPORT mdl := Model;

IMPORT int  := IntVMain;
IMPORT flt  := IntV_AFl;
IMPORT ityp := IntVType;
IMPORT icnv := IntV_cvf;

VAR
  zero   : INTEGER;

  zero_f : ityp.BoardFloatF;
  zero_d : ityp.BoardFloatD;
  zero_g : ityp.BoardFloatG;

<* ELSIF DEST_XDS THEN *>

IMPORT xi := XDInterface;

IMPORT xmRTS;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

<* END *>



CONST
  EL = 0C;

TYPE
  MAX_LEN = ARRAY [1..4] OF CARDINAL;
  INT_LENS = ARRAY [1..4] OF INTEGER;

CONST
  Max_Card = MAX_LEN { MAX(sys.CARD8), MAX(sys.CARD16), MAX(sys.CARD16), MAX(sys.CARD32) };
  Max_Int  = INT_LENS{ MAX(sys.INT8) , MAX(sys.INT16), MAX(sys.INT16),   MAX(sys.INT32) };
  Min_Int  = INT_LENS{ MIN(sys.INT8) , MIN(sys.INT16), MIN(sys.INT16),   MIN(sys.INT32) };


TYPE
  TOKEN     = ( none,
                plus,     minus,      equ,    neq,      gtr,
                geq,      lss,        leq,    times,    and,
                or,       xor,        slash,  div,      mod,
                rem,      const_val,  func_res,
                period,   lbr,        rbr,    lbrace,   rbrace,
                bar,      lpar,       rpar,   arrow,    ident,
                identrus, take_addr,  ref,    not,      comma,
                address,  indir_addr, sep,    register, loc_qual,
                exe_time, pc_time);

  TOKENLIST = SET OF TOKEN;
  IDENT     = xs.String;

TYPE
  FUNCTIONS = ( dec, hex, bin, sdec
              , oct, adr, size, high
              , low, type, pass, arg );

  STD_FUNC_PROC = PROCEDURE (VAR ExprRes);

  STD_FUNC_DESC = RECORD
                    func: STD_FUNC_PROC;
                    spec: BOOLEAN;
                  END;

  STD_FUNC = ARRAY FUNCTIONS OF STD_FUNC_DESC;

  KIND = (std_token, std_const, std_reg, std_func, std_var);

  PSTD_NAME_DESC = POINTER TO STD_NAME_DESC;

  STD_NAME_DESC = RECORD
                    CASE kind: KIND OF
                    | std_token: t     : TOKEN;
                    | std_const: res   : ExprRes;
                                 intern: BOOLEAN; -- стандартный (системы отладки) идентификатор, требующий @
                    | std_var  : pvar  : PExprRes;
                    | std_reg  : num   : CARDINAL;
                    | std_func : f_ID  : FUNCTIONS;
                    END;
                  END;

  STD_NAME = RECORD
               name: xs.STRING;
               desc: STD_NAME_DESC;
             END;

  STD_ARRAY = POINTER TO ARRAY OF STD_NAME;
  STD_TABLE = RECORD
                free: CARDINAL;
                tbl : STD_ARRAY;
              END;

VAR
  Table: STD_TABLE;
  name : xs.txt_ptr;


PROCEDURE CompTable(i: CARDINAL): INTEGER;
BEGIN
  IF name^ > Table.tbl^[i].name^ THEN
    RETURN -1;
  ELSIF name^ < Table.tbl^[i].name^ THEN
    RETURN 1;
  ELSE
    RETURN 0;
  END;
END CompTable;

PROCEDURE Find(str-: ARRAY OF CHAR; VAR pos: CARDINAL): BOOLEAN;
BEGIN
  WITH Table DO
    IF tbl  = NIL THEN RETURN FALSE END;
    IF free = 0 THEN RETURN FALSE END;
    name := sys.ADR(str);
    RETURN sor.BinaryFind(free, CompTable, pos);
  END;
END Find;

PROCEDURE FindStdName(str-:ARRAY OF CHAR; VAR dsc: STD_NAME_DESC): BOOLEAN;
VAR
  pos: CARDINAL;
BEGIN
  IF Find(str, pos) THEN
    dsc := Table.tbl^[pos].desc;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END FindStdName;


PROCEDURE AddStdName (str-: ARRAY OF CHAR; dsc-: STD_NAME_DESC): BOOLEAN;
VAR
  pos, i: CARDINAL;
  tmp: STD_ARRAY;
BEGIN
  WITH Table DO
    IF tbl = NIL THEN
      NEW (tbl, 32);
      free := 0;
      pos := 0;
    ELSE
      IF Find(str, pos) THEN
        RETURN FALSE;
      END;
      IF str > tbl^[pos].name^ THEN
        INC(pos);
      END;
      IF HIGH(tbl^)+1 = free THEN
        NEW(tmp, 2*(HIGH(tbl^)+1));
        sys.MOVE(sys.ADR(tbl^), sys.ADR(tmp^), SIZE(tbl^));
        DISPOSE (tbl);
        tbl := tmp;
      END;
      IF free # pos THEN
        FOR i := free TO pos+1 BY -1 DO
          tbl^[i] := tbl^[i-1];
        END;
      END;
    END;
    xs.alloc_from (tbl^[pos].name, str);
    tbl^[pos].desc := dsc;
    INC(free);
    RETURN TRUE;
  END;
END AddStdName;



PROCEDURE AddIdentExprConst (ident: ARRAY OF CHAR; expr: ExprRes): BOOLEAN;
BEGIN
  RETURN AddStdName (ident, STD_NAME_DESC{ std_const, expr, TRUE });
END AddIdentExprConst;


PROCEDURE AddIdentExprVar (ident: ARRAY OF CHAR; expr: PExprRes): BOOLEAN;
BEGIN
  RETURN AddStdName (ident, STD_NAME_DESC{ std_var, expr });
END AddIdentExprVar;


(*
PROCEDURE PrintTable;
VAR
  i: CARDINAL;
BEGIN
  WITH Table DO
    FOR i := 0 TO free-1 DO
      printf("%s\n", tbl^[i].name^);
    END;
  END;
END PrintTable;

PROCEDURE MakeNewStdNameDesc (dsc: STD_NAME_DESC): PSTD_NAME_DESC;
VAR
  tmp: PSTD_NAME_DESC;
BEGIN
  NEW (tmp);
  tmp^ := dsc;
  RETURN tmp;
END MakeNewStdNameDesc;
*)

VAR
  ExprStr      : xs.txt_ptr;
  Token        : TOKEN;
  char, next   : CHAR;
  pos          : CARDINAL;
  Ident        : IDENT;
  source       : exc.ExceptionSource;
  ComNum       : dt.ComNo;
  ModNum       : dt.ModNo; (* Текущий номер модуля *)
  IndirectType : dt.PTYPE; (* Тип для приведения   *)
  TmpRes       : ExprRes;
  TmpFunc      : FUNCTIONS;


PROCEDURE GetLastErrorPos (): CARDINAL;
BEGIN
  IF pos < 3 THEN
    RETURN 0;
  ELSE
    RETURN pos-2;
  END;
END GetLastErrorPos;


PROCEDURE Error (no:CARDINAL);
BEGIN
  exc.RAISE(source, no, '');
END Error;


<* IF DEST_XDS THEN *>

PROCEDURE GetName (addr: sys.ADDRESS; VAR name: ARRAY OF CHAR): BOOLEAN;
VAR
  i: CARDINAL;
  a: kt.ADDRESS;
BEGIN
  a := kt.ADDRESS(addr);
  i := 0;
  REPEAT
    IF NOT mem.Get (a+i, sys.ADR(name[i]), 1) THEN
      RETURN FALSE;
    END;
    INC(i);
  UNTIL (i > HIGH(name)) OR (name[i-1] = '');
  RETURN TRUE;
END GetName;



VAR
  TD: POINTER TO xi.X2C_TD_STR;

CONST
  TD_SIZE = sys.FIELDOFS(xi.X2C_TD_STR.procs);


PROCEDURE ROT_xjRTS_Java (addr: kt.ADDRESS; get_td: BOOLEAN; VAR type: dt.PTYPE): BOOLEAN;

  PROCEDURE Replace (VAR s: ARRAY OF CHAR; from, to: CHAR);
  VAR
    i: CARDINAL;
  BEGIN
    i := 0;
    WHILE i < LENGTH(s) DO
      IF s[i] = from THEN
        s[i] := to;
      END;
      INC (i);
    END;
  END Replace;


  PROCEDURE BuildClassTypeName (VAR type_name: ARRAY OF CHAR);
  VAR
    i, j: CARDINAL;
  BEGIN
    i := LENGTH (type_name);
    LOOP
      IF (type_name[i] = '`') OR (i = 0) THEN
        EXIT;
      END;
      DEC (i);
    END;
    IF i > 0 THEN
      j := 0;
      WHILE i < LENGTH(type_name) DO
        INC (i);
        type_name [j] := type_name [i];
        INC (j);
      END;
    END;
    xs.Append ('`', type_name);
  END BuildClassTypeName;

VAR
  com : dt.ComNo;
  mod : dt.ModNo;
  name: xs.String;
--    da  : xjRTS.X2J_DYNARR_STR;

--    t_pointer : dt.TYPE_POINTER;
--    t_array_of: dt.TYPE_ARRAY_OF;

BEGIN
  IF get_td THEN
    IF (addr < SIZE(xi.X2C_LINK_STR)-sys.FIELDOFS(xi.X2C_LINK_STR.td)) OR
       NOT  mem.Get (addr-SIZE(xi.X2C_LINK_STR)+sys.FIELDOFS(xi.X2C_LINK_STR.td), sys.ADR(addr), SIZE(addr))
    THEN
      RETURN FALSE;
    END;
  ELSIF NOT mem.Get (addr, sys.ADR(addr), SIZE(addr)) THEN
    RETURN FALSE;
  END;
  IF NOT mem.Get (addr, sys.ADR(TD^), TD_SIZE) THEN
    RETURN FALSE;
  END;
  IF NOT mem.Get (addr, sys.ADR(TD^), sys.FIELDOFS(TD^.kind)+SIZE(TD^.kind)) THEN
    RETURN FALSE;
  END;
  IF TD^.kind = xi.k_gc_array THEN
(*
    IF mem.Get (arg.location, sys.ADR(da), SIZE(da))
      AND mem.Get (kt.ADDRESS(da.elemtype), sys.ADR(td), SIZE(td))
    THEN
      IF td.kind = xi.k_primitive THEN
        IF NOT GetName (sys.ADDADR(sys.ADDRESS(addr),sys.FIELDOFS(xi.X2C_TD_STR.name))) THEN
          RETURN FALSE;
        END;
        IF NOT tls.FindStdType (str, type) THEN
          RETURN FALSE;
        END;
        t_array_of.TypeData.Tag := dt.Array_of;
        t_array_of.TypeData.Name := dt.Empty_Name;
        t_array_of.Base := type;
        arg.arr_desc := arg.location;
        RETURN tls.FindComponentByAddr (mem.GetIP(), com) AND  --   <--- kt.ADDRESS(da.elemtype) может, так лучше?
               tls.FindTypeLikeThis (com, t_array_of, TRUE, type) AND
               ArrayOf_Desc2Loc (arg);
      ELSIF GetName (md.name) AND
            tls.FindComponentByAddr (kt.ADDRESS(td.module), com) AND
            ((Replace (str, '.', '`') AND tls.FindModInComp (com, str, mod) AND (mod # dt.Invalid_Module)) OR
            (Replace (str, '`', '/') AND tls.FindModInComp (com, str, mod) AND (mod # dt.Invalid_Module))) AND
            GetName (td.name) AND BuildClassTypeName(str) AND tls.FindType (com, mod, str, type)
      THEN
        t_pointer.TypeData.Tag := dt.Pointer;
        t_pointer.TypeData.Name := 0; -- any
        t_pointer.Base := type;
        IF tls.FindTypeLikeThis (com, t_pointer, TRUE, type) THEN
          t_array_of.TypeData.Tag := dt.Array_of;
          t_array_of.TypeData.Name := 0; -- any
          t_array_of.Base := type;
          IF tls.FindTypeLikeThis (com, t_array_of, TRUE, type) THEN
            arg.arr_desc := arg.location; --- !!!
            RETURN ArrayOf_Desc2Loc (arg);
          END;
        END;
      END;
    END;
*)
    RETURN FALSE;
  ELSIF TD^.kind = xi.k_class THEN
    IF NOT mem.Get (addr, sys.ADR(TD^), sys.FIELDOFS(TD^.name)+SIZE(TD^.name)) THEN
      RETURN FALSE;
    END;
    IF NOT GetName (sys.ADDRESS(tls.AddAdr (addr, VAL (CARDINAL, TD^.name))), name) THEN
      RETURN FALSE;
    END;
    IF NOT tls.FindComponentByAddr (addr, com) THEN
      RETURN FALSE;
    END;
    IF NOT tls.FindModInComp (com, name, mod) THEN
      Replace (name, '.', '`');
      IF NOT tls.FindModInComp (com, name, mod) THEN
        Replace (name, '/', '`');
        IF NOT tls.FindModInComp (com, name, mod) THEN
          RETURN FALSE;
        END;
      END;
    END;
    BuildClassTypeName (name);
    RETURN tls.FindType (com, mod, name, type);
  ELSE
    RETURN FALSE;
  END;
END ROT_xjRTS_Java;


(* Проверяет, можно ли для выражения вычислить настоящий *)
(* тип обьекта; делает это, если возможно                *)
PROCEDURE CheckActualType (VAR arg: ExprRes;  get_td: BOOLEAN): BOOLEAN;

  PROCEDURE ROT_xmRTS_M2O2 (VAR type: dt.PTYPE): BOOLEAN;
  VAR
    com : dt.ComNo;
    mod : dt.ModNo;
    addr: kt.ADDRESS;
    td  : xmRTS.X2C_TD_STR;
    md  : xmRTS.X2C_MD_STR;
    str : xs.String;
  BEGIN
    addr := 0;
    IF get_td THEN
      IF (arg.location < SIZE(xmRTS.X2C_LINK_STR)-sys.FIELDOFS(xmRTS.X2C_LINK_STR.td)) OR
         NOT mem.Get (arg.location-SIZE(xmRTS.X2C_LINK_STR)+sys.FIELDOFS(xmRTS.X2C_LINK_STR.td), sys.ADR(addr), SIZE(addr))
      THEN
        RETURN FALSE;
      END;
    ELSE
      IF NOT mem.Get (arg.location, sys.ADR(addr), SIZE(addr)) THEN
        RETURN FALSE;
      END;
    END;
    RETURN tls.FindComponentByAddr (addr, com) AND
           mem.Get (addr, sys.ADR(td), SIZE(xmRTS.X2C_TD_STR)) AND
           mem.Get (kt.ADDRESS(td.module), sys.ADR(md), SIZE(xmRTS.X2C_MD_STR)) AND
           GetName (md.name, str) AND tls.FindModInComp (com, str, mod) AND
           (mod # dt.Invalid_Module) AND GetName (td.name, str) AND
           tls.FindType (com, mod, str, type);
  END ROT_xmRTS_M2O2;


VAR
  tag : dt.TYPE_TAG;
  lang: dt.LANGUAGE;
  res : BOOLEAN;
  type: dt.PTYPE;
  com : dt.ComNo;
  mod : dt.ModNo;

BEGIN
  res := FALSE;
  type := dt.Invalid_Type;
  IF (arg.sort = Variable) AND NOT tls.IsTypePrimitive (arg.var_type) THEN
    ASSERT (tls.TypeTag (arg.var_type, tag));
    IF tag = dt.Class THEN
      com := tls.TypeCom (arg.var_type);
      mod := tls.TypeMod (arg.var_type);
      lang := tls.ModLanguage (com, mod);
      IF (lang = dt.Lng_M2) OR (lang = dt.Lng_O2) THEN
        res := ROT_xmRTS_M2O2 (type);
      ELSIF lang = dt.Lng_Java THEN
        res := ROT_xjRTS_Java (arg.location, get_td, type); -- может сменить arg.arr_desc!!!
      END;
    END;
  END;
  IF res THEN
    arg.var_type := type;
  END;
  RETURN res;
END CheckActualType;


(* Перевычисляет для выражения настоящий тип *)
PROCEDURE GetActualType (VAR arg: ExprRes);
BEGIN
  sys.EVAL(CheckActualType (arg, TRUE));
END GetActualType;

<* END *>



PROCEDURE Var_Value (ConvertVar2Ref: BOOLEAN;   -- Делать из переменной ссылку
                     ConvertAllTag: BOOLEAN;    -- Для всех тегов переменных
                     arg:ExprRes;               -- Аргумент
                     VAR res:ExprRes); FORWARD; -- Результат

PROCEDURE Expr(VAR res: ExprRes); FORWARD;

PROCEDURE GetToken; FORWARD;


PROCEDURE Std_Func_Arg (VAR res: ExprRes);
BEGIN
  Expr(res);
  CASE res.sort OF
  | WHOLEval, CARDval:
  | INTval:
    IF sys.INT32 (res.value) < 0 THEN
      Error (msg.ExpectedIntConstant);
    END;
  ELSE
    Error (msg.ExpectedIntConstant);
  END;
  res.sort := STRINGval;
  IF NOT lst.GetArg (res.value, res.string) THEN
    COPY("", res.string);
  END;
END Std_Func_Arg;


PROCEDURE Std_Func_Pass (VAR res: ExprRes);
VAR
  number  : CARDINAL;
  React   : erc.PREACTION;
  BreakPos: CARDINAL;
  Event   : eve.EVENT_TYPE;
BEGIN
  number := 0;
  CASE Token OF
  | const_val:
    IF TmpRes.sort = CARDval THEN
      number := TmpRes.value;
    ELSE
      Error(msg.IllegalUsageOperation);
    END;
  | ident:
    IF NOT lst.GetBreakNumber (Ident, number) THEN
      Error(msg.Expected_break_ident);
    END;
  ELSE
    Error(msg.IllegalUsageOperation);
  END;
  IF prc.FindBreak (number, Event, React, BreakPos) THEN
    CASE Event OF
    | eve.SingleStep:
      res.value := brk.ConditionBreaks.ConditionBreaks^[BreakPos].Break.Pass;
    | eve.BreakpointHit:
      res.value := brk.Breakpoints.Breakpoints^[BreakPos].Break.Pass;
    | eve.MemoryAccess
   <* IF DEST_K26 THEN *>
    , eve.RegisterAccess, eve.DeviceAccess
   <* END *>
    : res.value := brk.AccessBreaks.AccessBreaks^[BreakPos].Break.Pass;;
    ELSE
      Error(msg.IllegalUsageOperation);
    END;
    res.sort := WHOLEval;
    res.type := dt.st_original;
    GetToken;
    RETURN;
  ELSE
    lst.DelBreak (Ident);
  END;
END Std_Func_Pass;


PROCEDURE Std_Func_Hex (VAR res: ExprRes);
BEGIN
  Expr(res);
  Var_Value(opt.ConvertVar2Ref, TRUE, res, res);
  CASE res.sort OF
  | CARDval, INTval, WHOLEval:
    res.type := dt.st_hex;
  ELSE
    Error(msg.IllegalUsageOperation);
  END;
END Std_Func_Hex;

PROCEDURE Std_Func_Dec (VAR res: ExprRes);
BEGIN
  Expr(res);
  Var_Value(opt.ConvertVar2Ref, TRUE, res, res);
  CASE res.sort OF
  | CARDval, INTval, WHOLEval:
    res.type := dt.st_unsigned;
  ELSE
    Error(msg.IllegalUsageOperation);
  END;
END Std_Func_Dec;

PROCEDURE Std_Func_SDec (VAR res: ExprRes);
BEGIN
  Expr(res);
  Var_Value(opt.ConvertVar2Ref, TRUE, res, res);
  CASE res.sort OF
  | CARDval, INTval, WHOLEval:
    res.type := dt.st_signed;
  ELSE
    Error(msg.IllegalUsageOperation);
  END;
END Std_Func_SDec;

PROCEDURE Std_Func_Bin (VAR res: ExprRes);
BEGIN
  Expr(res);
  Var_Value(opt.ConvertVar2Ref, TRUE, res, res);
  CASE res.sort OF
  | CARDval, INTval, WHOLEval:
    res.type := dt.st_bin;
  ELSE
    Error(msg.IllegalUsageOperation);
  END;
END Std_Func_Bin;

PROCEDURE Std_Func_Oct (VAR res: ExprRes);
BEGIN
  Expr(res);
  Var_Value(opt.ConvertVar2Ref, TRUE, res, res);
  CASE res.sort OF
  | CARDval, INTval, WHOLEval:
    res.type := dt.st_oct;
  ELSE
    Error(msg.IllegalUsageOperation);
  END;
END Std_Func_Oct;

PROCEDURE Std_Func_Adr (VAR res: ExprRes);
VAR
  tmp: ExprRes;
BEGIN
  Expr(tmp);
  IF (tmp.sort = Variable) THEN
    res.sort := Address;
    res.address := tmp.location;
    res.type := dt.st_original;
  ELSE
    Error(msg.IllegalUsageOperation);
  END;
END Std_Func_Adr;

PROCEDURE Std_Func_Size (VAR res: ExprRes);
VAR
  tmp: ExprRes;
BEGIN
  Expr(tmp);
  IF (tmp.sort = Variable) THEN
    res.sort  := CARDval;
    res.type  := dt.st_original;
    res.value := tls.TypeSize (tmp.var_type);
  ELSIF (tmp.sort = Register) AND (tmp.reg_type # dt.Invalid_Type) THEN
    res.sort  := CARDval;
    res.type  := dt.st_original;
    res.value := tls.TypeSize (tmp.reg_type);
  ELSE
    Error(msg.IllegalUsageOperation);
  END;
END Std_Func_Size;


PROCEDURE ArrayOf_Desc2Loc (VAR arg: ExprRes): BOOLEAN;
VAR
  lang: dt.LANGUAGE;
  loc_valid : BOOLEAN;
  desc_valid: BOOLEAN;
  test: sys.CARD8;
  com : dt.ComNo;
  mod : dt.ModNo;
BEGIN
  IF tls.IsTypePrimitive(arg.var_type) THEN
    RETURN FALSE;
  END;
  com := tls.TypeCom(arg.var_type);
  mod := tls.TypeMod(arg.var_type);
  lang := tls.ModLanguage (com , mod);
  CASE lang OF
  | dt.Lng_M2, dt.Lng_O2:
    test := 0;
    loc_valid := mem.Get (arg.arr_desc, ADR(arg.location), 4);
    desc_valid := mem.Get (arg.location, ADR(test), 1);
    RETURN loc_valid AND desc_valid;
 <* IF DEST_XDS THEN *>
  | dt.Lng_Java:
    arg.location := arg.arr_desc + xi.BODY_OFFS;
    RETURN TRUE;
 <* END *>
  ELSE
    RETURN FALSE;
  END;
END ArrayOf_Desc2Loc;


PROCEDURE ArrayOf_Size (array_of: ExprRes; VAR size: CARDINAL): BOOLEAN;
VAR
  tag : dt.TYPE_TAG;
  dim : CARDINAL;
  sub : dt.PTYPE;
BEGIN
  ASSERT(array_of.sort = Variable);
  ASSERT(tls.TypeTag(array_of.var_type, tag));
  ASSERT(tag = dt.Array_of);
  dim := tls.ArrayDim(array_of.var_type);
  IF dim = 1 THEN
    tls.SubType(array_of.var_type, sub);
    size := tls.TypeSize (sub);
  ELSIF NOT mem.Get(array_of.arr_desc + 8*(dim-1), ADR(size), 4) THEN
    RETURN FALSE;
  END;
  RETURN TRUE;
END ArrayOf_Size;


PROCEDURE ArrayOf_Len (array_of: ExprRes; VAR len: CARDINAL): BOOLEAN;
VAR
  tag : dt.TYPE_TAG;
  addr: kt.ADDRESS;
  lang: dt.LANGUAGE;
  com : dt.ComNo;
  mod : dt.ModNo;
BEGIN
  ASSERT(array_of.sort = Variable);
  ASSERT(tls.TypeTag(array_of.var_type, tag));
  ASSERT(tag = dt.Array_of);
  IF  tls.IsTypePrimitive(array_of.var_type)  THEN
    RETURN FALSE;
  END;
  com := tls.TypeCom(array_of.var_type);
  mod := tls.TypeMod(array_of.var_type);
  lang := tls.ModLanguage (com , mod);
  CASE lang OF
  | dt.Lng_M2, dt.Lng_O2:
    addr := array_of.arr_desc + 4 + 8*(tls.ArrayDim(array_of.var_type)-1);
 <* IF DEST_XDS THEN *>
  | dt.Lng_Java:
    addr := array_of.arr_desc + sys.FIELDOFS(xi.X2J_DYNARR_STR.length);
 <* END *>
  ELSE
    RETURN FALSE;
  END;
  RETURN mem.Get (addr, ADR(len), 4) AND ((len # 0) OR (lang = dt.Lng_Java));
END ArrayOf_Len;


PROCEDURE OpenArray_Len (open_array: ExprRes; VAR len: CARDINAL): BOOLEAN;
VAR
  tag    : dt.TYPE_TAG;
  addr   : kt.ADDRESS;
  desc   : dt.TYPE_OPEN_ARRAY;
  reg_no : CARDINAL;
  ref    : BOOLEAN;
  rel_reg: kt.REG_VALUE;
BEGIN
  ASSERT(open_array.sort = Variable);
  ASSERT(tls.TypeTag(open_array.var_type, tag));
  IF tag # dt.OpenArray THEN RETURN FALSE; END;
  ASSERT(tls.OpenArrayTypeDescription (open_array.var_type, desc));
  CASE desc.Length.Tag OF
  | dt.Sy_Relative:
    reg_no := desc.Length.DataRel.RegNo;
    ref := (dt.SA_Param IN desc.Length.DataRel.Attrib) AND (dt.SA_Reference IN desc.Length.DataRel.Attrib);
    ASSERT(mem.GetReg(reg_no, rel_reg));
    IF desc.Length.DataRel.Relative < 0 THEN
      addr := rel_reg - CARDINAL(-desc.Length.DataRel.Relative);
    ELSE
      addr := rel_reg + CARDINAL(desc.Length.DataRel.Relative);
    END;
    IF ref AND NOT mem.Get(addr, sys.ADR(addr), 4) THEN RETURN FALSE; END;
    IF NOT mem.Get(addr, ADR(len), 4) OR (len = 0) THEN RETURN FALSE; END;
  | dt.Sy_Register:
    reg_no := desc.Length.DataReg.RegNo;
    ref := (dt.SA_Param IN desc.Length.DataReg.Attrib) AND (dt.SA_Reference IN desc.Length.DataReg.Attrib);
    ASSERT(mem.GetReg(reg_no, rel_reg));
    addr := kt.ADDRESS(rel_reg);
    IF ref AND NOT mem.Get(addr, sys.ADR(addr), 4) THEN RETURN FALSE; END;
  END;
  RETURN TRUE;
END OpenArray_Len;


PROCEDURE OpenArray_Size (open_array: ExprRes; VAR size: CARDINAL): BOOLEAN;
VAR
  tag: dt.TYPE_TAG;
  dim: CARDINAL;
  len: CARDINAL;
BEGIN
  ASSERT(open_array.sort = Variable);
  ASSERT(tls.TypeTag(open_array.var_type, tag));
  IF tag # dt.OpenArray THEN RETURN FALSE; END;
  dim := tls.ArrayDim(open_array.var_type);
  tls.SubType(open_array.var_type, open_array.var_type);
  IF dim = 1 THEN
    size := tls.TypeSize (open_array.var_type);
  ELSE
    IF NOT OpenArray_Size (open_array, size) THEN RETURN FALSE; END;
    IF NOT OpenArray_Len (open_array, len) THEN RETURN FALSE; END;
    size := size * len;
  END;
  RETURN TRUE;
END OpenArray_Size;



PROCEDURE RangeArray (arg: ExprRes; VAR low_res, high_res: ExprRes);
VAR
  tag: dt.TYPE_TAG;
  index_type, range_type: dt.PTYPE;
  min, max: CARDINAL;
BEGIN
  IF (arg.sort # Variable) THEN Error(msg.ObjectIsNotArray); END;
  ASSERT( tls.TypeTag(arg.var_type, tag) );
  IF (tag # dt.Array) AND (tag # dt.Array_of) AND (tag # dt.OpenArray)THEN Error(msg.ObjectIsNotArray); END;
  tls.ArrayIndexType(arg.var_type, index_type);
  low_res.sort := Variable;
  low_res.type := dt.st_original;
  high_res.sort := Variable;
  high_res.type := dt.st_original;
  tls.SubType(arg.var_type, low_res.var_type);
  tls.SubType(arg.var_type, high_res.var_type);
  CASE tag OF
  | dt.Array:
    ASSERT(tls.TypeTag(index_type, tag));
    IF tag = dt.Range THEN
      tls.SubType(index_type, range_type);
      ASSERT( tls.TypeTag(range_type, tag) );
    END;
    tls.Index(index_type, min, max);

  | dt.Array_of:
    ASSERT(tls.TypeTag(index_type, tag));
    min := 0;
    IF NOT ArrayOf_Len(arg, max) THEN Error(msg.ErrorAccessMemory); END;
    IF max = 0 THEN
      Error(msg.RangeError);
    END;
    DEC(max);

  | dt.OpenArray:
    ASSERT(tls.TypeTag(index_type, tag));
    min := 0;
    IF NOT OpenArray_Len(arg, max) THEN Error(msg.ErrorAccessMemory); END;
    IF max = 0 THEN
      Error(msg.RangeError);
    END;
    DEC(max);
  END;
  CASE tag OF
  | dt.Card:
    low_res.sort   := CARDval;
    low_res.type   := dt.st_original;
    low_res.value  := min;
    high_res.sort  := CARDval;
    high_res.type  := dt.st_original;
    high_res.value := max;

  | dt.Int :
    low_res.sort   := INTval;
    low_res.type   := dt.st_original;
    low_res.value  := min;
    high_res.sort  := INTval;
    high_res.type  := dt.st_original;
    high_res.value := max;

  | dt.Char:
    low_res.sort  := CHARval;
    low_res.ch    := CHR(min);
    high_res.sort := CHARval;
    high_res.ch   := CHR(max);

  | dt.Boolean:
    low_res.sort   := BOOLval;
    low_res.b_val  := VAL(BOOLEAN, min);
    high_res.sort  := BOOLval;
    high_res.b_val := VAL(BOOLEAN, max);

  | dt.Enum:
    low_res.sort   := CARDval;
    low_res.type   := dt.st_original;
    low_res.value  := min;
    high_res.sort  := CARDval;
    high_res.type  := dt.st_original;
    high_res.value := max;
  ELSE
    Error(msg.IllegalUsageOperation);
  END;
END RangeArray;


PROCEDURE Std_Func_High (VAR res: ExprRes);
VAR
  tmp: ExprRes;
BEGIN
  Expr(tmp);
  RangeArray(tmp, tmp, res);
END Std_Func_High;


PROCEDURE Std_Func_Low (VAR res: ExprRes);
VAR
  tmp: ExprRes;
BEGIN
  Expr(tmp);
  RangeArray(tmp, res, tmp);
END Std_Func_Low;


PROCEDURE Std_Func_Type (VAR res: ExprRes);
BEGIN
  Expr(res);
  Error(msg.IllegalUsageOperation);
END Std_Func_Type;


CONST
  Std_Func = STD_FUNC { STD_FUNC_DESC{ Std_Func_Dec   , FALSE }
                      , STD_FUNC_DESC{ Std_Func_Hex   , FALSE }
                      , STD_FUNC_DESC{ Std_Func_Bin   , FALSE }
                      , STD_FUNC_DESC{ Std_Func_SDec  , FALSE }
                      , STD_FUNC_DESC{ Std_Func_Oct   , FALSE }
                      , STD_FUNC_DESC{ Std_Func_Adr   , FALSE }
                      , STD_FUNC_DESC{ Std_Func_Size  , FALSE }
                      , STD_FUNC_DESC{ Std_Func_High  , FALSE }
                      , STD_FUNC_DESC{ Std_Func_Low   , FALSE }
                      , STD_FUNC_DESC{ Std_Func_Type  , FALSE }
                      , STD_FUNC_DESC{ Std_Func_Pass  , TRUE  }
                      , STD_FUNC_DESC{ Std_Func_Arg   , TRUE  }
                      };

PROCEDURE getchar;
BEGIN
   char := next;
   IF next # EL THEN
     next := ExprStr^[pos]; INC(pos);
   END;
END getchar;

PROCEDURE getchar_b;
BEGIN
  REPEAT
    getchar
  UNTIL char # ' ';
END getchar_b;

TYPE ALPHATAB = ARRAY CHAR OF CHAR;

(* Bits : 0 - letter/digit  1 - 'A'..'F'  2 - dec digit  3 - oct digit  *)

CONST ALPHA = ALPHATAB {
(*-----  00  01  02  03  04  05  06  07  08  09  0A  0B  0C  0D  0E  0F *)
(*00h*)  0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C,
(*10h*)  0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C,
(*20h*)  0C, 1C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C,
(*30h*) 15C,15C,15C,15C,15C,15C,15C,15C, 5C, 5C, 0C, 0C, 0C, 0C, 0C, 1C,
(*40h*)  0C, 3C, 3C, 3C, 3C, 3C, 3C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C,
(*50h*)  1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 0C, 0C, 0C, 0C, 1C,
(*60h*)  1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C,
(*70h*)  1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 0C, 0C, 0C, 0C, 0C,
(*80h*)  1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C,
(*90h*)  1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C,
(*A0h*)  1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C,
(*B0h*)  0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C,
(*C0h*)  0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C,
(*D0h*)  0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C,
(*E0h*)  1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C, 1C,
(*F0h*)  0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C, 0C };


<* IF DEST_K26 THEN *>

VAR
  need_quota: BOOLEAN;

PROCEDURE ObtainBase_pre(VAR Base:CARDINAL; char, next: CHAR): BOOLEAN;
BEGIN
  need_quota := FALSE;
  CASE char OF
  | 'B' : IF next = "'" THEN Base :=  2; need_quota := TRUE; RETURN TRUE END;
  | 'O' : IF next = "'" THEN Base :=  8; need_quota := TRUE; RETURN TRUE END;
  | 'X' : IF next = "'" THEN Base := 16; need_quota := TRUE; RETURN TRUE END;
  | '0' : IF CAP(next) = "X" THEN Base := 16; RETURN TRUE END;
  ELSE
  END;
  RETURN FALSE;
END ObtainBase_pre;

PROCEDURE ObtainBase_post(VAR Base: CARDINAL; char: CHAR; VAR wasBase: BOOLEAN): BOOLEAN;
BEGIN
  IF wasBase THEN
    IF need_quota THEN
      IF (char = "'") THEN
        wasBase := TRUE;
        RETURN TRUE;
      ELSE
        wasBase := FALSE;
      END;
    END;
  ELSE
    wasBase := TRUE;
    IF char = 'H' THEN Base := 16; RETURN TRUE; END;
  END;
  RETURN FALSE;
END ObtainBase_post;

<* ELSE *>

PROCEDURE ObtainBase_pre(VAR Base:CARDINAL; char, next: CHAR): BOOLEAN;
BEGIN
  CASE char OF
  | '0' : IF CAP(next) = "X" THEN Base := 16; RETURN TRUE END;
  ELSE
  END;
  RETURN FALSE;
END ObtainBase_pre;

PROCEDURE ObtainBase_post(VAR Base: CARDINAL; char: CHAR; VAR wasBase: BOOLEAN): BOOLEAN;
BEGIN
  IF NOT wasBase THEN
    wasBase := TRUE;
    CASE char OF
    | 'H': Base := 16; RETURN TRUE;
    | 'I': Base := 2;  RETURN TRUE;
    | 'B': Base := 8;  RETURN TRUE;
    | 'C': Base := 0;  RETURN TRUE;
    ELSE
    END;
  END;
  RETURN FALSE;
END ObtainBase_post;

<* END *>



PROCEDURE power10 (s:CARDINAL): LONGLONGREAL;
VAR
  i : CARDINAL;
  r : LONGLONGREAL;
BEGIN
  r := 1.0;
  FOR i := 1 TO s DO
    r := 10.0 * r;
  END;
  RETURN r;
EXCEPT
  IF exm.IsM2Exception() THEN
    ASSERT(exm.M2Exception() = exm.realValueException);
    Error(msg.RealTooBig); -- Слишком большое число
    RETURN r;
  END;
END power10;


PROCEDURE GetNumber;

  PROCEDURE EvalCardNumber( str-: ARRAY OF CHAR; Base: CARDINAL;
                            VAR sum: CARDINAL                   ): BOOLEAN;
  VAR
    c: CHAR;
    i: CARDINAL;
  BEGIN
    sum := 0;
    FOR i := 0 TO LENGTH(str)-1 DO
      c := str[i];
      IF ALPHA[c] >= 4C THEN (* десятичная цифpа *)
        IF (Base = 2) THEN
          IF (c # '0') AND (c # '1') THEN
            RETURN FALSE;
          END;
        ELSIF (Base = 8) THEN
          IF (ALPHA[c] < 10C) THEN
            RETURN FALSE;
          END;
        END;
        sum := sum*Base + (ORD(c) - ORD('0'));
      ELSE
        c := CAP(c);
        IF ALPHA[c] < 2C THEN (* не шестнадцатеpичная цифpа *)
          RETURN FALSE
        END;
        IF Base # 16 THEN Error(msg.IncorrectIntConstant); END;
        sum := sum*16 + (10+(ORD(c) - ORD('A')));
      END;
    END;
    RETURN TRUE;
  END EvalCardNumber;


VAR
  c         : CHAR;            (* Очередной символ                 *)
  result    : CARDINAL;
  sum_r     : LONGLONGREAL;
  d         : LONGLONGREAL;
  Base      : CARDINAL;
  BaseOK    : BOOLEAN;
  HexAllowed: BOOLEAN;
  Number    : xs.String;
  pos       : CARDINAL;
BEGIN
  Base := 10;
  HexAllowed := FALSE;
  BaseOK := ObtainBase_pre(Base, char, next);
  IF BaseOK THEN
    getchar; getchar;
    IF Base = 16 THEN HexAllowed := TRUE; END;
  END;
  pos := 0;
  LOOP
    c := char;
    IF ALPHA[c] >= 4C THEN (* десятичная цифра *)
      IF NOT BaseOK THEN HexAllowed := TRUE; END;
    ELSE
      CASE c OF
      | 'C', 'B':
        IF NOT BaseOK AND ((NOT HexAllowed) OR ((ALPHA[CAP(next)] < 2C) AND (next # 'H'))) THEN
          EXIT
        END;
      ELSE
      END;
      c := CAP(c);
      IF (NOT HexAllowed) OR (ALPHA[c] < 2C) THEN (* не шестнадцатеpичная цифpа *)
        EXIT
      END;
    END;
    Number[pos] := c;
    INC(pos);
    getchar;
  END;
  IF pos = 0 THEN Error(msg.EmptyIntConstant); END;
  Number[pos] := 0C;
  INC(pos);
  IF ObtainBase_post(Base, char, BaseOK) THEN getchar END;
  IF NOT BaseOK THEN Error(msg.IncorrectIntConstant); END;
  IF Base # 0 THEN
    IF NOT EvalCardNumber(Number, Base, result) THEN
      Error(msg.IncorrectIntConstant);
    END;
    IF (Base = 10) AND (char = '.') THEN
      sum_r := VAL(LONGLONGREAL, result);
      d := 10.0;
      getchar;
      LOOP
        IF ALPHA[char] >= 4C THEN
         sum_r := sum_r + VAL(LONGLONGREAL,ORD(char) - ORD('0')) / d;
         d := d * 10;
        ELSE
          EXIT;
        END;
        getchar;
      END;
      IF (char = 'E') OR (char = 'e') THEN
        getchar;
        c := char;
        IF (c = '+') OR (c = '-') THEN
          getchar;
        ELSE
          c := '+';
        END;
        IF ALPHA[char] < 4C THEN
          Error(msg.IncorrectRealConstant) -- Обязательно цифра
        END;
        result := 0;
        LOOP
          IF ALPHA[char] >= 4C THEN
            result := 10*result + (ORD(char) - ORD('0'));
          ELSE
            EXIT
          END;
          getchar;
        END;
        IF c = '+' THEN
          sum_r := sum_r * power10(result);
        ELSE
          sum_r := sum_r / power10(result);
        END;
      END;
      Token        := const_val;
      TmpRes.sort  := REALval;
  <* IF DEST_K26 THEN *>
      TmpRes.r_type      := PC_const;
      TmpRes.r_val_const := sum_r
  <* ELSE *>
      TmpRes.r_val := sum_r
  <* END *>
    ELSE
      Token           := const_val;
      TmpRes.sort     := CARDval;
      TmpRes.type     := dt.st_original;
      TmpRes.value    := result;
    END;
  ELSE
    IF NOT EvalCardNumber(Number, 8, result) OR (result > 255) THEN
      Error(msg.IncorrectIntConstant);
    ELSE
      Token      := const_val;
      TmpRes.sort:= CHARval;
      TmpRes.ch  := VAL(CHAR, result);
    END;
  END;

EXCEPT
  IF exm.IsM2Exception() THEN
    ASSERT(exm.M2Exception() = exm.wholeValueException);
    Error(msg.IntTooBig); -- Слишком длинное число
    RETURN;
  END;
END GetNumber;



PROCEDURE CheckToken(t: TOKEN);
BEGIN
  IF Token # t THEN
    Error(msg.IncorrectToken); -- Ошибка в выpажении !
  ELSE
    GetToken;
  END;
END CheckToken;


PROCEDURE GetToken;
TYPE
  IdentSort = (nodef, lat, rus);

VAR
  p : CARDINAL;
  identsort : IdentSort;
  desc: STD_NAME_DESC;

  PROCEDURE GetIdent;
  BEGIN
    identsort := nodef;
    p := 0;
    REPEAT
      IF char IN LatLetter+CHARSET{'_'} THEN identsort := lat;
      ELSIF char IN RusLetter THEN identsort := rus;
      END;
      Ident[p] := char;
      INC(p);
      getchar;
    UNTIL NOT ( (char = '_') OR (char = '$') OR (char = '`') OR
                (char IN Digits) OR
                ((identsort = lat) AND (char IN LatLetter)) OR
                ((identsort = rus) AND (char IN RusLetter))     );
    Ident[p] := 0C;
  END GetIdent;

BEGIN
  IF char = ' ' THEN getchar_b; END;
  CASE char OF
  | EL : Token := none;
  | '+': Token := plus  ; getchar_b;
  | '*': Token := times ; getchar_b;
  | '/': Token := slash ; getchar_b;
  | '%': Token := mod   ; getchar_b;
  | '[': Token := lbr   ; getchar_b;
  | ']': Token := rbr   ; getchar_b;
  | '{': Token := lbrace; getchar_b;
  | '}': Token := rbrace; getchar_b;
  | '^': Token := bar   ; getchar_b;
  | ',': Token := comma ; getchar_b;
  | '|': Token := or    ; getchar_b;
  | '(': Token := lpar  ; getchar_b;
  | ')': Token := rpar  ; getchar_b;
  | '#': Token := neq   ; getchar_b;
  | '-': Token := minus ; getchar_b;
  | '.': Token := period; getchar_b;
  | '~': Token := not   ; getchar_b;
  | '&': Token := and   ; getchar_b;
  | '=': IF next = '>' THEN
           getchar;
           Token := arrow;
         ELSE
           Token := equ;
         END;
         getchar_b;
  | '>': IF next = '=' THEN
           getchar;
           Token := geq;
         ELSE
           Token := gtr;
         END;
         getchar_b;
  | '<': CASE next OF
         | '=' : getchar; Token := leq;
         | '>' : getchar; Token := neq;
         ELSE
           Token := lss;
         END;
         getchar_b;
  | ':': IF next = ':' THEN
           getchar;
           Token := loc_qual;
         ELSE
           Error(msg.IncorrectToken);
         END;
         getchar_b;
  | '@':
    getchar;
    GetIdent;
    xs.Uppercase(Ident);
    IF FindStdName (Ident, desc) THEN
      CASE desc.kind OF
      | std_const:
        IF desc.intern THEN
          -- стандартный (системы отладки) идентификатор, требующий @
          Token := const_val;
          TmpRes := desc.res;
        ELSE
          Error(msg.UnknownSpecialName);
        END;
      | std_reg:
        Token := register;
        WITH TmpRes DO
          sort   := Register;
          reg_no := desc.num;
          reg_type := dt.Invalid_Type;
        END;
      | std_var:
        Token := const_val; --действительно, далее значение станет константным!
        TmpRes := desc.pvar^;
      | std_func:
        IF Std_Func[desc.f_ID].spec THEN
          Token := func_res;
          TmpFunc := desc.f_ID;
        ELSE
          Error(msg.UnknownSpecialName);
        END;
      ELSE
        Error(msg.UnknownSpecialName);
      END;
    ELSE
      Error(msg.UnknownSpecialName);
    END;

  |"'": getchar;
        IF next # "'" THEN Error(msg.IncorrectToken); END;
        TmpRes.sort := CHARval;
        TmpRes.ch := char;
        Token := const_val;
        getchar;
        getchar;

  |'"': TmpRes.string := "";
        LOOP
          IF next = '"' THEN
            getchar; -- get symbol "
            EXIT;
          ELSIF next = EL THEN
            Error (msg.String_not_terminated);
          END;
          getchar;
          xs.Append (char, TmpRes.string);
        END;
        getchar; -- get next char and prepare next token
        TmpRes.sort := STRINGval;
        Token := const_val;

  |'0'..'9': GetNumber;

  |'a'..'z','A'..'Z','а'..'п','р'..'я','А'..'Я','_','$':
     IF (char = 'A') AND (next = ':') THEN
       Token := address;
       getchar;
       getchar;
    <* IF DEST_K26 THEN *>
     ELSIF ((char = 'B') OR (char = 'O') OR (char = 'X')) AND (next = "'") THEN
       GetNumber;
     ELSIF (char = 'A') AND (next = '{') THEN
       Token := take_addr;
       getchar;
       getchar_b;
    <* END *>
     ELSE
       GetIdent;
       IF (FindStdName(Ident, desc)) AND (desc.kind = std_token) THEN
         Token := desc.t;
       ELSE
         CASE identsort OF
         | nodef, lat : Token := ident;
         | rus        : Token := identrus;
         END;
       END;
     END;
  ELSE
    Error(msg.IncorrectToken); -- Если невеpный токен
  END;
END GetToken;


<* IF DEST_K26 THEN *>

PROCEDURE SetDefaultFormat;
BEGIN
  Fmt_Special    := '';
  Fmt_REALval    := '%g';
  Fmt_CHARval    := "'%c'";
  Fmt_CTRL_CHAR  := "%oC";
  Fmt_STRINGval  := '"%s"';
  Fmt_ADDRval    := "A:%$8X";
  Fmt_Register   := 'Регистр %s';
  Fmt_RegWindow  := "X'%$8X'";
END SetDefaultFormat;

<* ELSIF DEST_XDS THEN *>

PROCEDURE SetDefaultFormat;
BEGIN
  Fmt_Special    := '';
  Fmt_REALval    := '%g';
  Fmt_ADDRval    := "0x%$8X";
  Fmt_Register   := 'Register %s';
  Fmt_RegWindow  := "0x%$8X";
  Fmt_STRINGval  := '"%s"';
  Fmt_CHARval    := "'%c'";
  Fmt_CTRL_CHAR  := "%oC";
END SetDefaultFormat;

<* ELSE *>

Not defined TARGET or unknown target!

<* END *>


PROCEDURE Designator(VAR res: ExprRes); FORWARD;

PROCEDURE Simple (VAR res: ExprRes); FORWARD;


CONST
  ADDITIVES = TOKENLIST { plus, minus, or, xor };
  RELATION  = TOKENLIST { equ, neq, gtr, geq, lss, leq };
  MULTIPLOP = TOKENLIST { times, and, slash, div, mod, rem};


(* Проверяет типы результатов выражений на совместимость           *)
(* Если один из них - константа, то он становится (если возможно)  *)
(* значением с типом второго операнда                              *)
PROCEDURE TypesCompatible (token: TOKEN; VAR res1, res2: ExprRes): BOOLEAN;
VAR
  tmp, res: ExprRes;
  tag: dt.TYPE_TAG;

BEGIN
  CASE res1.sort OF
  | INTval:
    CASE res2.sort OF
    | INTval  :
    | WHOLEval: res2.sort := INTval;
                res2.type := dt.st_original;
    | CARDval : IF LONGINT(res2.value) >= 0 THEN
                  res2.sort := INTval;
                  res2.type := dt.st_original;
                ELSE
                  RETURN FALSE;
                END;
    | Address :
      IF token IN RELATION THEN
        res2.sort := INTval;
        res2.value := CARDINAL(res2.address);
        res2.type := dt.st_original;
      ELSE
        RETURN FALSE;
      END;

    | REALval:
      <* IF DEST_K26 THEN *>
        res1.r_val_const := VAL(LONGLONGREAL, LONGINT(res1.value));
        res1.r_type := PC_const;
      <* ELSE *>
        res1.r_val := VAL(LONGLONGREAL, LONGINT(res1.value));
      <* END *>
      res1.sort := REALval;
    ELSE
      RETURN FALSE;
    END;

  | CARDval:
    CASE res2.sort OF
    | CARDval  :
    | WHOLEval: res2.sort := CARDval;
    | INTval:
      IF (token = slash) OR (token = rem) THEN
        IF LONGINT(res1.value) >= 0 THEN
          res1.sort := INTval;
          res1.type := dt.st_original;
        ELSE
          RETURN FALSE;
        END;
      ELSE
        IF LONGINT(res2.value) >= 0 THEN
          res2.sort := CARDval;
          res2.type := dt.st_original;
        ELSE
          RETURN FALSE;
        END;
      END;
    | Address:
      IF token IN RELATION THEN
        res2.sort := CARDval;
        res2.value := CARDINAL(res2.address);
        res2.type := dt.st_original;
      ELSE
        RETURN FALSE;
      END;
    | REALval:
      <* IF DEST_K26 THEN *>
        res1.r_val_const := VAL(LONGLONGREAL, res1.value);
        res1.r_type      := PC_const;
      <* ELSE *>
        res1.r_val := VAL(LONGLONGREAL, res1.value);
      <* END *>
      res1.sort := REALval;
    ELSE
      RETURN FALSE;
    END;
    RETURN TRUE;
  | WHOLEval:
    CASE res2.sort OF
    | CARDval  : res1.sort := CARDval;
    | INTval   : res1.sort := INTval;
    | WHOLEval :
    | Address:
      IF token IN RELATION THEN
        res2.sort  := WHOLEval;
        res2.value := CARDINAL(res2.address);
        res2.type  := dt.st_original;
      ELSE
        RETURN FALSE;
      END;
    | REALval  :
      <* IF DEST_K26 THEN *>
        res1.r_val_const := VAL(LONGLONGREAL, res1.value);
        res1.r_type      := PC_const;
      <* ELSE *>
        res1.r_val := VAL(LONGLONGREAL, res1.value);
      <* END *>
      res1.sort := REALval;
    ELSE
      RETURN FALSE;
    END;
  | CHARval:
    RETURN (res2.sort = CHARval);
  | BOOLval:
    RETURN (res2.sort = BOOLval);
  | STRINGval:
    RETURN (res2.sort = STRINGval);
  | REALval:
    CASE res2.sort OF
    | CARDval,
      WHOLEval :
      res2.sort := REALval;
      res := res2;

    <* IF DEST_K26 THEN *>

      CASE res1.r_type OF
      | PC_const: res2.r_val_const := VAL(LONGLONGREAL, res.value);
                  res2.r_type := PC_const;
      | f : IF CARDINAL(res.value) > MAX(INTEGER) THEN Error(msg.ErrorRealOperation); END;
            flt.CVTLF(sys.ADR(res.value), res2.r_val_f);
            IF flt.Et THEN Error(msg.ErrorRealOperation); END;
            res2.r_type := f;
      | g : IF CARDINAL(res.value) > MAX(INTEGER) THEN Error(msg.ErrorRealOperation); END;
            flt.CVTLG(sys.ADR(res.value), res2.r_val_g);
            IF flt.Et THEN Error(msg.ErrorRealOperation); END;
            res2.r_type := g;
      | d : IF CARDINAL(res.value) > MAX(INTEGER) THEN Error(msg.ErrorRealOperation); END;
            flt.CVTLD(sys.ADR(res.value), res2.r_val_d);
            IF flt.Et THEN Error(msg.ErrorRealOperation); END;
            res2.r_type := d;
      END;

    <* ELSE *>

       res2.r_val := VAL(LONGLONGREAL, res.value);

    <* END *>

    | INTval   :
      res := res2;
      <* IF DEST_K26 THEN *>
        res2.r_val_const := VAL(LONGLONGREAL, LONGINT(res.value));
        res2.r_type      := PC_const;
      <* ELSE *>
        res2.r_val := VAL(LONGLONGREAL, LONGINT(res.value));
      <* END *>
      res2.sort := REALval;
    | REALval  :
      <* IF DEST_K26 THEN *>
        res := res2;
        CASE res1.r_type OF
        | PC_const: IF res2.r_type # PC_const THEN
                      RETURN TypesCompatible(token, res2, res1);
                    END;
        | f:
            CASE res2.r_type OF
            | PC_const:
              IF NOT icnv.FloatPC_VAX(res.r_val_const, sys.ADR(res2.r_val_f), 'f', error) THEN RETURN FALSE END;
            | f:
            | g: flt.CVTGF(sys.ADR(res.r_val_g), res2.r_val_f); IF flt.Et THEN RETURN FALSE END;
            | d: flt.CVTDF(sys.ADR(res.r_val_d), res2.r_val_f); IF flt.Et THEN RETURN FALSE END;
            END;
            res2.r_type := f;
        | g:
            CASE res2.r_type OF
            | PC_const:
              IF NOT icnv.FloatPC_VAX(res.r_val_const, sys.ADR(res2.r_val_g), 'g', error) THEN RETURN FALSE END;
            | g:
            | f: flt.CVTFG(sys.ADR(res.r_val_f), res2.r_val_g); IF flt.Et THEN RETURN FALSE END;
            --| d: flt.CVTDG(sys.ADR(res.r_val_d), res2.r_val_g); IF flt.Et THEN RETURN FALSE END;
            | d: RETURN FALSE;
            END;
            res2.r_type := g;
        | d:
            CASE res2.r_type OF
            | PC_const:
              IF NOT icnv.FloatPC_VAX(res.r_val_const, sys.ADR(res2.r_val_d), 'd', error) THEN RETURN FALSE END;
            | d:
            --| g: flt.CVTGD(sys.ADR(res.r_val_g), res2.r_val_d); IF flt.Et THEN RETURN FALSE END;
            | g: RETURN FALSE;
            | f: flt.CVTFD(sys.ADR(res.r_val_f), res2.r_val_d); IF flt.Et THEN RETURN FALSE END;
            END;
            res2.r_type := d;
        END;
      <* END *>
    ELSE
      RETURN FALSE;
    END;
  | Address:
    CASE res2.sort OF
    | CARDval, INTval:
    | WHOLEval:
      res2.sort := CARDval;
      res2.type := dt.st_original;

    | Address        :
    | Reference      : res2.sort := Address;
                       res2.address := res2.reference;
    | Variable       : IF (token = equ) OR (token = neq) THEN
                         ASSERT(tls.TypeTag(res2.var_type, tag));
                         IF tag = dt.Pointer THEN
                           res2.sort := Address;
                           res2.type := dt.st_original;
                           IF mem.Get(res2.location, sys.ADR(res2.address), 4) THEN
                             RETURN TRUE;
                           END;
                         END;
                       END;
                       RETURN FALSE;

    ELSE
      RETURN FALSE;
    END;
  | Reference:
    CASE res2.sort OF
    | CARDval, INTval:
    | WHOLEval       : res2.sort := CARDval;
                       res2.type := dt.st_original;
    | Reference      : res2.sort    := Address;
                       res2.address := res2.reference;
                       res1.sort    := Address;
                       res1.address := res1.reference;
    | Address        : res1.sort    := Address;
                       res1.address := res1.reference;
    ELSE
      RETURN FALSE;
    END;
  | Variable       : IF (token = equ) OR (token = neq) THEN
                       ASSERT(tls.TypeTag(res1.var_type, tag));
                       IF tag = dt.Pointer THEN
                         tmp := res1;
                         tmp.sort := Address;
                         tmp.type := dt.st_original;
                         IF mem.Get(tmp.location, sys.ADR(tmp.address), 4) THEN
                           CASE res2.sort OF
                           | Address:
                             res1 := tmp;
                             RETURN TRUE;
                           | Variable:
                             IF TypesCompatible(token, tmp, res2) THEN
                               res1 := tmp;
                               RETURN TRUE;
                             END;
                           ELSE

                           END;
                         END;
                       END;
                       RETURN FALSE;
                     END;
  ELSE
    RETURN FALSE;
  END;
  RETURN TRUE;
END TypesCompatible;


PROCEDURE Exe_Time (VAR res: ExprRes);
BEGIN
 <* IF DEST_K26 THEN *>
  res.sort := WHOLEval;
  res.value := int.Time;
 <* ELSIF DEST_XDS THEN *>
  res.sort := WHOLEval;
  res.value := 0;
 <* END *>
END Exe_Time;


PROCEDURE PC_Time (VAR res: ExprRes);
VAR
  DateTime: clk.DateTime;
BEGIN
  clk.GetClock (DateTime);
  res.sort := WHOLEval;
  tm.pack (DateTime, res.value);
END PC_Time;


PROCEDURE CheckBit(set: ExprRes; N: CARDINAL; VAR bit: BOOLEAN): BOOLEAN;
VAR
  addr: CARDINAL;
  byte: sys.CARD8;
  tag : dt.TYPE_TAG;
BEGIN
  IF set.sort # Variable THEN RETURN FALSE; END;
  IF NOT tls.TypeTag(set.var_type, tag) THEN RETURN FALSE; END;
  IF tag # dt.Set THEN RETURN FALSE END;
  addr := set.location + (N DIV 8);
  IF NOT mem.Get(addr, sys.ADR(byte), 1) THEN RETURN FALSE; END;
<* PUSH *>
<* WOFF304+ *>
  bit := (N MOD 8) IN BITSET(VAL(CARDINAL, byte));
<* POP *>
  RETURN TRUE;
END CheckBit;

PROCEDURE ToggleBit(set: ExprRes; N: CARDINAL): BOOLEAN;
VAR
  addr: CARDINAL;
  byte: sys.CARD8;
  tag : dt.TYPE_TAG;
  bs:   BITSET;
BEGIN
  IF set.sort # Variable THEN RETURN FALSE; END;
  IF NOT tls.TypeTag(set.var_type, tag) THEN RETURN FALSE; END;
  IF tag # dt.Set THEN RETURN FALSE END;
  addr := set.location + (N DIV 8);
  IF NOT mem.Get(addr, sys.ADR(byte), 1) THEN RETURN FALSE; END;
<* PUSH *>
<* WOFF304+ *>
  bs := BITSET(VAL(CARDINAL, byte));
  IF (N MOD 8) IN bs THEN
    EXCL(bs, N MOD 8);
  ELSE
    INCL(bs, N MOD 8);
  END;
  byte := VAL(sys.CARD8, CARDINAL(bs));
<* POP *>
  IF NOT mem.Put(addr, sys.ADR(byte), 1) THEN RETURN FALSE; END;
  RETURN TRUE;
END ToggleBit;



<* IF DEST_K26 THEN *>
PROCEDURE GetTypeRealType (type: dt.PTYPE): CARDINAL;
VAR
  tag : dt.TYPE_TAG;
  name: xs.txt_ptr;
BEGIN
  ASSERT (tls.IsTypeValid (type));
  ASSERT (tls.IsTypePrimitive (type));
  ASSERT (tls.TypeTag(type, tag));
  ASSERT (tag = dt.Real);
  ASSERT (tls.TypeSize (type) = dt.STD_TYPES[dt.STD_TYPE_REAL_D].Length);
  ASSERT (tls.TypeName (type, name));
  IF name^ = dt.STD_TYPES[dt.STD_TYPE_REAL_G].Name THEN
    RETURN dt.STD_TYPE_REAL_G;
  END;
  -- закомментарено, так как все остальные на VAX вормата D
  -- ASSERT (name^ = dt.STD_TYPES[dt.STD_TYPE_REAL_D].Name);
  RETURN dt.STD_TYPE_REAL_D;
END GetTypeRealType;
<* END *>


--------------------------------------------------------------------------------
PROCEDURE Var_Value (ConvertVar2Ref: BOOLEAN;   -- Делать из переменной ссылку
                     ConvertAllTag: BOOLEAN;    -- Для всех тегов переменных
                     arg:ExprRes;               -- Аргумент
                     VAR res:ExprRes);          -- Результат
    -- 1 ---- Var_Value --------------------------------------------------------
    PROCEDURE VarOnRegisters_Value (arg: ExprRes; VAR res: ExprRes): BOOLEAN;
    VAR type_tag   : dt.TYPE_TAG;
        type_size  : CARDINAL;
        reg_val    : kt.REG_VALUE;
        addr_reg   : sys.ADDRESS;
        pcard      : POINTER TO CARDINAL;
       <* IF TARGET_x86 OR TARGET_m68k THEN *>
        zzz        : REAL;
       <* END *>
    BEGIN
      IF (arg.sort # Register) OR (arg.reg_type = dt.Invalid_Type) THEN
        RETURN FALSE;
      END;
      ASSERT(mem.GetReg (arg.reg_no, reg_val));
      type_size := tls.TypeSize(arg.reg_type);
      addr_reg := sys.ADR(reg_val);
      ASSERT(tls.TypeTag(arg.reg_type, type_tag));
      WHILE type_tag = dt.Range DO
        tls.SubType(arg.reg_type, arg.reg_type);
        ASSERT(tls.TypeTag(arg.reg_type, type_tag));
      END;
      CASE type_tag OF
      | dt.Card, dt.Byte:
        res.sort := CARDval;
        res.value := 0;
        sys.MOVE (addr_reg, sys.ADR(res.value), type_size);
        res.type := arg.type;
      | dt.Char:
        IF type_size # 1 THEN RETURN FALSE; END;
        res.sort := CHARval;
        res.type := arg.type;
        sys.MOVE (addr_reg, sys.ADR(res.ch), type_size);
      | dt.Boolean:
        IF type_size # 1 THEN RETURN FALSE; END;
        res.sort := BOOLval;
        res.type := arg.type;
        sys.MOVE (addr_reg, sys.ADR(res.b_val), type_size);
      | dt.Int:
        res.sort  := INTval;
        pcard := addr_reg;
        CASE type_size OF
        | 1 : res.value := CARDINAL(VAL(LONGINT, sys.CAST(sys.INT8,  pcard^)));
        | 2 : res.value := CARDINAL(VAL(LONGINT, sys.CAST(sys.INT16, pcard^)));
        | 4 : res.value := CARDINAL(VAL(LONGINT, sys.CAST(sys.INT32, pcard^)));
        ELSE
          RETURN FALSE;
        END;
        res.type := arg.type;
      | dt.Pointer, dt.Address:
        res.sort := Address;
        res.address := 0;
        res.type := arg.type;
        sys.MOVE (addr_reg, sys.ADR(res.address), type_size);
      | dt.Real:
        IF type_size <= SIZE(kt.REG_VALUE) THEN
          res.sort := REALval;
         <* IF TARGET_VAX THEN *>
          res.r_type := f;
          sys.MOVE (addr_reg, sys.ADR(res.r_val_f), type_size);
         <* ELSIF TARGET_x86 OR TARGET_m68k THEN *>
          zzz := REAL(reg_val);
          res.r_val := VAL(LONGLONGREAL, zzz);
         <* END *>
        ELSE
          res.sort := REALval;
         <* IF TARGET_VAX THEN *>
          res.r_type := d;
          sys.MOVE (addr_reg, sys.ADR(res.r_val_d), SIZE(kt.REG_VALUE));
          ASSERT(mem.GetReg(arg.reg_no+1, reg_val));
          sys.MOVE (addr_reg, sys.ADDADR(sys.ADR(res.r_val_d), SIZE(kt.REG_VALUE)), type_size-SIZE(kt.REG_VALUE));
         <* ELSE *>
          RETURN FALSE;
         <* END *>
        END;
        res.type := arg.type;
      | dt.Procedure: -- leave register variable to Res2Str
      | dt.Enum:      -- leave register variable to Res2Str
      ELSE
        RETURN FALSE;
      END;
      RETURN TRUE;
    END VarOnRegisters_Value;

-- 0 ---- Var_Value ------------------------------------------------------------
VAR
  type: dt.PTYPE;
  len : CARDINAL;
  tag : dt.TYPE_TAG;
  addr: sys.ADDRESS;
  p1  : POINTER TO sys.INT8;
  p2  : POINTER TO sys.INT16;
  p4  : POINTER TO sys.INT32;
  b   : CARDINAL;
 <* IF DEST_XDS THEN *>
  lr  : LONGREAL;
  r   : REAL;
 <* END *>
BEGIN
  res := arg;
  IF arg.sort = Register THEN
    IF NOT VarOnRegisters_Value (arg, res) THEN
      res.sort  := WHOLEval;
      res.type  := dt.st_original;
      IF NOT mem.GetReg(arg.reg_no, res.value) THEN Error(msg.IncorrectExpression); END;
    END;
  END;
  IF arg.sort # Variable THEN RETURN; END;
  IF ConvertVar2Ref THEN
    res.sort := Reference;
    res.ref_type := arg.var_type;
    res.reference := arg.location;
    RETURN;
  END;
  type := arg.var_type;
  ASSERT(tls.TypeTag(type, tag));
  CASE tag OF
  | dt.Byte:
    res.sort  := WHOLEval;
    res.value := 0;
    len := tls.TypeSize (arg.var_type);
    IF NOT mem.Get(arg.location, sys.ADR(res.value), len) THEN
      Error (msg.ErrorAccessMemory);
    END;
  | dt.Int:
    len := tls.TypeSize (arg.var_type);
    IF len <= SIZE(res.value) THEN
      addr := sys.ADR(res.value);
      res.value := 0;
      IF NOT mem.Get(arg.location, addr, len) THEN
        Error(msg.ErrorAccessMemory);
      END;
      CASE len OF
      | 1 : p1 := addr; res.value := LONGCARD(VAL(LONGINT, p1^));
      | 2 : p2 := addr; res.value := LONGCARD(VAL(LONGINT, p2^));
      | 4 : p4 := addr; res.value := LONGCARD(VAL(LONGINT, p4^));
      END;
      res.sort  := INTval;
    END;
  | dt.Card:
    len := tls.TypeSize(arg.var_type);
    IF len <= SIZE(res.value) THEN
      res.value := 0;
      IF NOT mem.Get(arg.location, sys.ADR(res.value), len) THEN
        Error(msg.ErrorAccessMemory);
      END;
      res.sort := CARDval;
    END;
  | dt.Real:
    res.sort  := REALval;
   <* IF DEST_K26 THEN *>
    CASE tls.TypeSize (arg.var_type) OF
    | 4 : res.r_type := f;
          IF NOT mem.Get(arg.location, sys.ADR(res.r_val_f),4) THEN
            Error(msg.ErrorAccessMemory);
          END;
    | 8 : CASE GetTypeRealType (arg.var_type) OF
          | dt.STD_TYPE_REAL_D:
            res.r_type := d;
            IF NOT mem.Get(arg.location, sys.ADR(res.r_val_d), 8) THEN
              Error(msg.ErrorAccessMemory);
            END;
          | dt.STD_TYPE_REAL_G:
            res.r_type := g;
            IF NOT mem.Get(arg.location, sys.ADR(res.r_val_g), 8) THEN
              Error(msg.ErrorAccessMemory);
            END;
          END;
    | 10: res.r_type := PC_const;
          IF NOT mem.Get(arg.location, sys.ADR(res.r_val_const), 10) THEN
            Error(msg.ErrorAccessMemory);
          END;
    END;
   <* ELSE *>
   <* PUSH *>
   <* WOFF304+ *>
    CASE tls.TypeSize (arg.var_type) OF
    | 10: IF NOT mem.Get(arg.location, sys.ADR(res.r_val), 10) THEN
            Error(msg.ErrorAccessMemory);
          END;
    | 8 : IF NOT mem.Get(arg.location, sys.ADR(lr), 8) THEN
            Error(msg.ErrorAccessMemory);
          END;
          res.r_val := lr;
    | 4 : IF NOT mem.Get(arg.location, sys.ADR(r), 4) THEN
            Error(msg.ErrorAccessMemory);
          END;
          res.r_val := r;
    END;
   <* POP *>
   <* END  *>
  | dt.Boolean :
    len := tls.TypeSize(arg.var_type);
    b := 0;
    IF NOT mem.Get(arg.location, sys.ADR(b), len) THEN
      Error(msg.ErrorAccessMemory);
    END;
    res.sort  := BOOLval;
    IF b = 0 THEN
      res.b_val := FALSE;
    ELSE
      res.b_val := TRUE;
    END;
  | dt.Address:
    len := tls.TypeSize(arg.var_type);
    ASSERT(len = SIZE(res.address));
    IF NOT mem.Get(arg.location, sys.ADR(res.address), len) THEN
      Error(msg.ErrorAccessMemory);
    END;
    res.sort := Address;
  | dt.Char:
    len := tls.TypeSize(arg.var_type);
    IF (len=SIZE(res.ch)) THEN
      IF mem.Get(arg.location, sys.ADR(res.ch), SIZE(res.ch)) THEN
        res.sort := CHARval;
      ELSE
        Error(msg.ErrorAccessMemory);
      END;
    END;
  | dt.Range:
    tls.SubType(type, res.var_type);
    Var_Value(ConvertVar2Ref, ConvertAllTag, res, res);
  | dt.Enum:
    IF ConvertAllTag THEN
      len := tls.TypeSize(arg.var_type);
      res.value := 0;
      IF NOT mem.Get(arg.location, sys.ADR(res.value), len) THEN
        Error(msg.ErrorAccessMemory);
      END;
      res.sort := CARDval;
      res.type := dt.st_original;
    END;
  | dt.Pointer:
    IF ConvertAllTag THEN
      len := tls.TypeSize(arg.var_type);
      res.address := 0;
      IF NOT mem.Get(arg.location, sys.ADR(res.address), len) THEN
        Error(msg.ErrorAccessMemory);
      END;
      res.sort := Address;
    END;
  | dt.Procedure:
    IF ConvertAllTag THEN
      res.address := 0;
      IF NOT mem.Get(arg.location, sys.ADR(res.address), 4) THEN
        Error(msg.ErrorAccessMemory);
      END;
      res.sort := Address;
    END;
  ELSE
  END;
END Var_Value;


PROCEDURE Var2Value(arg:ExprRes; VAR res:ExprRes): BOOLEAN;
BEGIN
   Var_Value(FALSE, FALSE, arg, res);
   RETURN TRUE;
EXCEPT
  IF exc.IsCurrentSource(source) THEN
    error := exc.CurrentNumber(source);
    IF error = 0 THEN
      dfn := FALSE;
      error := msg.Undefined_Expression;
    END;
  ELSE
    error := msg.SomeInternalDebuggerError;
  END;
  RETURN FALSE;
END Var2Value;


PROCEDURE IndirectTypeAddr(arg:ExprRes; VAR res: ExprRes);
BEGIN
  IF NOT tls.IsTypeValid(IndirectType) THEN Error(msg.UnknownType); END;
  Var_Value(opt.ConvertVar2Ref, TRUE, arg, arg);
  CASE arg.sort OF
  | WHOLEval, CARDval: res.location := arg.value;
  | INTval    : IF LONGINT(arg.value) >= 0 THEN
                  res.location := LONGINT(arg.value);
                ELSE
                  Error(msg.IllegalUsageOperation);
                END;
  | Reference : res.location := arg.reference;
  | Address   : res.location := arg.address;
  ELSE
    Error(msg.IllegalUsageOperation) -- Хочу, но не могу...
  END;
  res.sort := Variable;
  res.type := dt.st_original;
  res.var_type := IndirectType;
 <* IF DEST_XDS THEN *>
  IF opt.AutoDetectActualType THEN
    GetActualType (res);
  END;
 <* END *>
  IndirectType := dt.Invalid_Type;
END IndirectTypeAddr;


PROCEDURE TypeCast (arg:ExprRes; VAR res: ExprRes);
BEGIN
  IF NOT tls.IsTypeValid(IndirectType) THEN Error(msg.UnknownType); END;
  CASE arg.sort OF
  | Variable:
    res := arg;
    res.var_type := IndirectType;
  ELSE
    Error(msg.IllegalUsageOperation) -- Хочу, но не могу...
  END;
  IndirectType := dt.Invalid_Type;
END TypeCast;


PROCEDURE Unar(t: TOKEN; arg: ExprRes; VAR res: ExprRes);
VAR
  tag   : dt.TYPE_TAG;
  start : kt.ADDRESS;
  size  : CARDINAL;
  access: kt.ATTRIBS;
BEGIN
  IF (t#take_addr) AND (t#bar) THEN
    Var_Value(opt.ConvertVar2Ref, TRUE, arg, arg);
  END;
<* PUSH *>
<* COVERFLOW- *>
  CASE t OF
  | plus :
    CASE arg.sort OF
    | CARDval, INTval:
      res := arg;
      res.type := dt.st_original;
    | REALval:
      res := arg;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;
  | minus :
    CASE arg.sort OF
    | INTval, CARDval, WHOLEval:
      res.sort  := INTval;
      res.type  := dt.st_original;
<* PUSH *>
<* IOVERFLOW- *>
      res.value := LONGCARD(- LONGINT(arg.value));
<* POP *>
    | REALval:
      res.sort := REALval;
      <* IF DEST_K26 THEN *>
        res.r_type := arg.r_type;
        CASE res.r_type OF
        | PC_const: res.r_val_const := -arg.r_val_const;
        | f: flt.SUBF (sys.ADR(arg.r_val_f), sys.ADR(zero_f), res.r_val_f); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
        | g: flt.SUBG (sys.ADR(arg.r_val_g), sys.ADR(zero_f), res.r_val_g); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
        | d: flt.SUBD (sys.ADR(arg.r_val_d), sys.ADR(zero_f), res.r_val_d); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
        END;
      <* ELSE *>
        res.r_val := - arg.r_val;
      <* END *>
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;
  | take_addr :
    IF (arg.sort = Variable) THEN
      res.sort := Address;
      res.type := dt.st_original;
      res.address := arg.location;
    ELSE
      Error(msg.IllegalUsageOperation) -- Хочу, но не могу...
    END;
  | indir_addr :
    CASE arg.sort OF
    | Address :
      res.sort := Variable;
      res.type := dt.st_original;
      res.var_type := dt.STD_TYPE_BYTE8;
      res.location := arg.address;
    | CARDval, WHOLEval:
      res.sort := Variable;
      res.type := dt.st_original;
      res.var_type := dt.STD_TYPE_BYTE8;
      res.location := arg.value;
    | INTval:
      IF sys.INT32 (arg.value) < 0 THEN
        Error (msg.IllegalUsageOperation);
      END;
      res.sort := Variable;
      res.type := dt.st_original;
      res.var_type := dt.STD_TYPE_BYTE8;
      res.location := sys.INT32 (arg.value);
    | Reference :
      res.sort := Variable;
      res.type := dt.st_original;
      res.location := arg.reference;
      res.var_type := arg.ref_type;
     <* IF DEST_XDS THEN *>
      IF opt.AutoDetectActualType THEN
        GetActualType (res);
      END;
     <* END *>
    ELSE
      Error (msg.IllegalUsageOperation) -- Хочу, но не могу...
    END;
  | bar :
    IF arg.sort = Variable THEN
      ASSERT(tls.TypeTag(arg.var_type, tag));
      IF tag = dt.Pointer THEN
        res.sort := Variable;
        res.type := dt.st_original;
        tls.SubType(arg.var_type, res.var_type);
        ASSERT(tls.TypeTag(res.var_type, tag));
        res.location := 0;
        IF NOT mem.Get(arg.location, sys.ADR(res.location), tls.TypeSize(arg.var_type)) THEN
          Error(msg.ErrorAccessMemory);
        END;
        IF NOT (mem.GetSegmentInfo(res.location, start, size, access) AND (kt.read IN access)) THEN
          Error(msg.NilPointerDereference);
        END;
-- Это сильно тормозит под Win'95
-- Теперь region может быть invalid, но это не страшно (dialog.StructList)
--        IF NOT CheckRegionValid(res) THEN Error(msg.NilPointerDereference); END;
        IF tag = dt.Array_of THEN
          res.arr_desc := res.location;
          IF NOT ArrayOf_Desc2Loc (res) THEN Error(msg.ErrorAccessMemory); END;
        END;
       <* IF DEST_XDS THEN *>
        IF (tag = dt.Class) AND opt.AutoDetectActualType THEN
          GetActualType (res);
        END
       <* END *>
      ELSE
        Error(msg.IllegalUsageOperation) -- Хочу, но не могу...
      END;
    ELSIF arg.sort = Register THEN
      IF tls.IsTypeValid(IndirectType) THEN
        IndirectTypeAddr (arg, res);
      ELSE
        Error(msg.IllegalUsageOperation) -- Хочу, но не могу...
      END;
    ELSE
      Error(msg.IllegalUsageOperation) -- Хочу, но не могу...
    END;
  | not :
    CASE arg.sort OF
    | INTval, CARDval, WHOLEval:
      res.sort := arg.sort;
      res.value := LONGCARD( - BITSET(arg.value) );
    | BOOLval:
      res.sort := BOOLval;
      res.b_val := NOT arg.b_val;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;
  ELSE
    Error(msg.IllegalUsageOperation); -- Hе понял. Чего делать-то?!
  END;
<* POP *>
END Unar;


PROCEDURE Binar(t: TOKEN; arg1, arg2: ExprRes; VAR res: ExprRes);
BEGIN
  Var_Value (opt.ConvertVar2Ref, TRUE, arg1, arg1);
  Var_Value (opt.ConvertVar2Ref, TRUE, arg2, arg2);
  IF NOT TypesCompatible(t, arg1, arg2) THEN
    Error(msg.IncompatibleTypes);
  END;
 <* PUSH       *>
 <* COVERFLOW- *>
  CASE t OF
  | plus :
    CASE arg1.sort OF
    | INTval:
      res.sort := INTval;
      res.type  := dt.st_original;
      res.value := CARDINAL(LONGINT(arg1.value) + LONGINT(arg2.value));
    | CARDval, WHOLEval:
      res.sort := arg1.sort;
      res.type  := dt.st_original;
      res.value := arg1.value + arg2.value;
    | REALval:
      res.sort := REALval;
<* IF DEST_K26 THEN *>
      res.r_type := arg1.r_type;
      CASE res.r_type OF
      | PC_const: res.r_val_const := arg1.r_val_const + arg2.r_val_const;
      | f : flt.ADDF(sys.ADR(arg1.r_val_f), sys.ADR(arg2.r_val_f), res.r_val_f); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      | g : flt.ADDG(sys.ADR(arg1.r_val_g), sys.ADR(arg2.r_val_g), res.r_val_g); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      | d : flt.ADDD(sys.ADR(arg1.r_val_d), sys.ADR(arg2.r_val_d), res.r_val_d); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      END;
<* ELSE *>
      res.r_val := arg1.r_val + arg2.r_val;
<* END  *>
    | Address:
      IF (arg2.sort = INTval) THEN
        res.address := kt.ADDRESS(LONGINT(arg1.address) + LONGINT(arg2.value));
      ELSIF (arg2.sort = CARDval) THEN
        res.address := arg1.address + arg2.value;
      ELSE
        Error(msg.IncompatibleTypes) -- Хочу, но не могу...
      END;
    | Reference:
      IF (arg2.sort = INTval) THEN
        res.reference := kt.ADDRESS(LONGINT(arg1.reference) + LONGINT(arg2.value));
      ELSIF (arg2.sort = CARDval) THEN
        res.reference := arg1.reference + arg2.value;
      ELSE
        Error(msg.IncompatibleTypes) -- Хочу, но не могу...
      END;
    | STRINGval:
      res.sort := STRINGval;
      res.type := dt.st_original;
      res.string := arg1.string;
      xs.Append (arg2.string, res.string);
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;
  | minus :
    CASE arg1.sort OF
    | INTval:
      res.sort := INTval;
      res.type := dt.st_original;
      res.value := CARDINAL(arg1.value - arg2.value);
    | CARDval, WHOLEval:
      res.sort := arg1.sort;
      res.type := dt.st_original;
      res.value := CARDINAL(arg1.value - arg2.value);
    | REALval:
      res.sort := REALval;
<* IF DEST_K26 THEN *>
      res.r_type := arg1.r_type;
      CASE res.r_type OF
      | PC_const: res.r_val_const := arg1.r_val_const - arg2.r_val_const;
      | f : flt.SUBF (sys.ADR(arg2.r_val_f), sys.ADR(arg1.r_val_f), res.r_val_f); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      | g : flt.SUBG (sys.ADR(arg2.r_val_g), sys.ADR(arg1.r_val_f), res.r_val_g); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      | d : flt.SUBD (sys.ADR(arg2.r_val_d), sys.ADR(arg1.r_val_f), res.r_val_d); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      END;
<* ELSE *>
      res.r_val := arg1.r_val - arg2.r_val;
<* END  *>
    | Address:
      res.sort := Address;
      IF (arg2.sort = INTval) THEN
        res.address := kt.ADDRESS(LONGINT(arg1.address) - LONGINT(arg2.value));
      ELSIF (arg2.sort = CARDval) THEN
        res.address := kt.ADDRESS(arg1.address - arg2.value);
      ELSIF (arg2.sort = Address) THEN
         IF arg1.address >= arg2.address THEN
           res.sort  := CARDval;
           res.type  := dt.st_original;
           res.value := CARDINAL(arg1.address - arg2.address);
         ELSE
           res.sort  := INTval;
           res.type  := dt.st_original;
           res.value := CARDINAL(LONGINT(arg1.address) - LONGINT(arg2.address));
         END;
      ELSE
        Error(msg.IncompatibleTypes) -- Хочу, но не могу...
      END;
    | Reference:
      res.sort := Address;
      IF (arg2.sort = INTval) THEN
        res.reference := kt.ADDRESS(LONGINT(arg1.reference) - LONGINT(arg2.value));
      ELSIF (arg2.sort = CARDval) THEN
        res.reference := kt.ADDRESS(arg1.reference - arg2.value);
      ELSE
        Error(msg.IncompatibleTypes) -- Хочу, но не могу...
      END;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;
  | times :
    CASE arg1.sort OF
    | INTval:
      res.sort := INTval;
      res.type := dt.st_original;
      res.value := CARDINAL(LONGINT(arg1.value) * LONGINT(arg2.value));
    | CARDval, WHOLEval:
      res.sort := arg1.sort;
      res.type := dt.st_original;
      res.value := arg1.value * arg2.value;
    | REALval:
      res.sort := REALval;
<* IF DEST_K26 THEN *>
      res.r_type := arg1.r_type;
      CASE res.r_type OF
      | PC_const: res.r_val_const := arg1.r_val_const * arg2.r_val_const;
      | f : flt.MULF (sys.ADR(arg2.r_val_f), sys.ADR(arg1.r_val_f), res.r_val_f); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      | g : flt.MULG (sys.ADR(arg2.r_val_g), sys.ADR(arg1.r_val_g), res.r_val_g); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      | d : flt.MULD (sys.ADR(arg2.r_val_d), sys.ADR(arg1.r_val_d), res.r_val_d); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      END;
<* ELSE *>
      res.r_val := arg1.r_val * arg2.r_val;
<* END  *>
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;
  | slash :
    CASE arg1.sort OF
    | INTval:
      res.sort := INTval;
      res.type := dt.st_original;
      res.value := CARDINAL(LONGINT(arg1.value) / LONGINT(arg2.value));
    | CARDval, WHOLEval:
      res.sort := arg1.sort;
      res.type := dt.st_original;
      res.value := arg1.value / arg2.value;
    | REALval:
      res.sort := REALval;
<* IF DEST_K26 THEN *>
      res.r_type := arg1.r_type;
      CASE res.r_type OF
      | PC_const: res.r_val_const := arg1.r_val_const / arg2.r_val_const;
      | f : flt.DIVF (sys.ADR(arg2.r_val_f), sys.ADR(arg1.r_val_f), res.r_val_f); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      | g : flt.DIVG (sys.ADR(arg2.r_val_g), sys.ADR(arg1.r_val_g), res.r_val_g); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      | d : flt.DIVD (sys.ADR(arg2.r_val_d), sys.ADR(arg1.r_val_d), res.r_val_d); IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      END;
<* ELSE *>
      res.r_val := arg1.r_val / arg2.r_val;
<* END  *>
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;
  | div :
    CASE arg1.sort OF
    | INTval:
      res.sort := INTval;
      res.type := dt.st_original;
      res.value := CARDINAL(LONGINT(arg1.value) DIV LONGINT(arg2.value));
    | CARDval, WHOLEval:
      res.sort := arg1.sort;
      res.type := dt.st_original;
      res.value := arg1.value DIV arg2.value;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;
  | mod :
    CASE arg1.sort OF
    | INTval:
      res.sort := CARDval;
      res.type := dt.st_original;
      res.value := CARDINAL(LONGINT(arg1.value) MOD LONGINT(arg2.value));
    | CARDval, WHOLEval:
      res.sort := arg1.sort;
      res.type := dt.st_original;
      res.value := arg1.value MOD arg2.value;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;
  | rem :
    CASE arg1.sort OF
    | INTval:
      res.sort := INTval;
      res.type := dt.st_original;
      res.value := CARDINAL(LONGINT(arg1.value) REM LONGINT(arg2.value));
    | CARDval, WHOLEval:
      res.sort := arg1.sort;
      res.type := dt.st_original;
      res.value := arg1.value REM arg2.value;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;
  | equ :
    CASE arg1.sort OF
    | CARDval, WHOLEval, INTval:
      res.sort := BOOLval;
      res.b_val := arg1.value = arg2.value;
    | REALval:
      res.sort := BOOLval;
     <* IF DEST_K26 THEN *>
      CASE arg1.r_type OF
      | PC_const: res.b_val := arg1.r_val_const = arg2.r_val_const; RETURN
      | f : flt.CMPF (sys.ADR(arg1.r_val_f), sys.ADR(arg2.r_val_f));
      | g : flt.CMPG (sys.ADR(arg1.r_val_g), sys.ADR(arg2.r_val_g));
      | d : flt.CMPD (sys.ADR(arg1.r_val_d), sys.ADR(arg2.r_val_d));
      END;
      IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      res.b_val := flt.Z;
     <* ELSE *>
      res.b_val := arg1.r_val = arg2.r_val;
     <* END  *>
    | Address:
      res.sort := BOOLval;
      IF (arg2.sort = Address) THEN
        res.b_val := arg1.address = arg2.address;
      ELSE
        Error(msg.IncompatibleTypes) -- Хочу, но не могу...
      END;
    | BOOLval:
      res.sort := BOOLval;
      res.b_val := arg1.b_val = arg2.b_val;
    | CHARval:
      res.sort := BOOLval;
      res.b_val := arg1.ch = arg2.ch;
    | STRINGval:
      res.sort := BOOLval;
      res.type := dt.st_original;
      res.b_val := arg1.string = arg2.string;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;

  | neq :
    CASE arg1.sort OF
    | CARDval, WHOLEval, INTval:
      res.sort := BOOLval;
      res.b_val := arg1.value # arg2.value;
    | REALval:
      res.sort := BOOLval;
     <* IF DEST_K26 THEN *>
      CASE arg1.r_type OF
      | PC_const: res.b_val := arg1.r_val_const # arg2.r_val_const; RETURN
      | f : flt.CMPF (sys.ADR(arg1.r_val_f), sys.ADR(arg2.r_val_f));
      | g : flt.CMPG (sys.ADR(arg1.r_val_g), sys.ADR(arg2.r_val_g));
      | d : flt.CMPD (sys.ADR(arg1.r_val_d), sys.ADR(arg2.r_val_d));
      END;
      IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      res.b_val := NOT flt.Z;
     <* ELSE *>
      res.b_val := arg1.r_val # arg2.r_val;
     <* END  *>
    | Address:
      res.sort := BOOLval;
      IF (arg2.sort = Address) THEN
        res.b_val := arg1.address # arg2.address;
      ELSE
        Error(msg.IncompatibleTypes) -- Хочу, но не могу...
      END;
    | BOOLval:
      res.sort := BOOLval;
      res.b_val := arg1.b_val # arg2.b_val;
    | CHARval:
      res.sort := BOOLval;
      res.b_val := arg1.ch # arg2.ch;
    | STRINGval:
      res.sort := BOOLval;
      res.type := dt.st_original;
      res.b_val := arg1.string # arg2.string;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;

  | gtr :
    CASE arg1.sort OF
    | INTval, CARDval, WHOLEval:
      res.sort := BOOLval;
      res.b_val := arg1.value > arg2.value;
    | REALval:
      res.sort := BOOLval;
     <* IF DEST_K26 THEN *>
      CASE arg1.r_type OF
      | PC_const: res.b_val := arg1.r_val_const > arg2.r_val_const; RETURN
      | f : flt.CMPF (sys.ADR(arg1.r_val_f), sys.ADR(arg2.r_val_f));
      | g : flt.CMPG (sys.ADR(arg1.r_val_g), sys.ADR(arg2.r_val_g));
      | d : flt.CMPD (sys.ADR(arg1.r_val_d), sys.ADR(arg2.r_val_d));
      END;
      IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      res.b_val := NOT flt.Z AND NOT flt.N;
     <* ELSE *>
      res.b_val := arg1.r_val > arg2.r_val;
     <* END  *>
    | Address:
      res.sort := BOOLval;
      IF (arg2.sort = Address) THEN
        res.b_val := arg1.address > arg2.address;
      ELSE
        Error(msg.IncompatibleTypes) -- Хочу, но не могу...
      END;
    | CHARval:
      res.sort := BOOLval;
      res.b_val := arg1.ch > arg2.ch;
    | STRINGval:
      res.sort := BOOLval;
      res.type := dt.st_original;
      res.b_val := arg1.string > arg2.string;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;

  | geq :
    CASE arg1.sort OF
    | INTval, CARDval, WHOLEval:
      res.sort := BOOLval;
      res.b_val := arg1.value >= arg2.value;
    | REALval:
      res.sort := BOOLval;
<* IF DEST_K26 THEN *>
      CASE arg1.r_type OF
      | PC_const: res.b_val := arg1.r_val_const >= arg2.r_val_const; RETURN
      | f : flt.CMPF (sys.ADR(arg1.r_val_f), sys.ADR(arg2.r_val_f));
      | g : flt.CMPG (sys.ADR(arg1.r_val_g), sys.ADR(arg2.r_val_g));
      | d : flt.CMPD (sys.ADR(arg1.r_val_d), sys.ADR(arg2.r_val_d));
      END;
      IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      res.b_val := flt.Z OR NOT flt.N;
<* ELSE *>
      res.b_val := arg1.r_val >= arg2.r_val;
<* END  *>
    | Address:
      res.sort := BOOLval;
      IF (arg2.sort = Address) THEN
        res.b_val := arg1.address >= arg2.address;
      ELSE
        Error(msg.IncompatibleTypes) -- Хочу, но не могу...
      END;
    | CHARval:
      res.sort := BOOLval;
      res.b_val := arg1.ch >= arg2.ch;
    | STRINGval:
      res.sort := BOOLval;
      res.type := dt.st_original;
      res.b_val := arg1.string >= arg2.string;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;

  | lss :
    CASE arg1.sort OF
    | INTval, CARDval, WHOLEval:
      res.sort := BOOLval;
      res.b_val := arg1.value < arg2.value;
    | REALval:
      res.sort := BOOLval;
     <* IF DEST_K26 THEN *>
      CASE arg1.r_type OF
      | PC_const: res.b_val := arg1.r_val_const < arg2.r_val_const; RETURN
      | f : flt.CMPF (sys.ADR(arg1.r_val_f), sys.ADR(arg2.r_val_f));
      | g : flt.CMPG (sys.ADR(arg1.r_val_g), sys.ADR(arg2.r_val_g));
      | d : flt.CMPD (sys.ADR(arg1.r_val_d), sys.ADR(arg2.r_val_d));
      END;
      IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      res.b_val := NOT flt.Z AND flt.N;
     <* ELSE *>
      res.b_val := arg1.r_val < arg2.r_val;
     <* END  *>
    | Address:
      res.sort := BOOLval;
      IF (arg2.sort = Address) THEN
        res.b_val := arg1.address < arg2.address;
      ELSE
        Error(msg.IncompatibleTypes) -- Хочу, но не могу...
      END;
    | CHARval:
      res.sort := BOOLval;
      res.b_val := arg1.ch < arg2.ch;
    | STRINGval:
      res.sort := BOOLval;
      res.type := dt.st_original;
      res.b_val := arg1.string < arg2.string;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;

  | leq :
    CASE arg1.sort OF
    | INTval, CARDval, WHOLEval:
      res.sort := BOOLval;
      res.b_val := arg1.value <= arg2.value;
    | REALval:
      res.sort := BOOLval;
     <* IF DEST_K26 THEN *>
      CASE arg1.r_type OF
      | PC_const: res.b_val := arg1.r_val_const <= arg2.r_val_const; RETURN
      | f : flt.CMPF (sys.ADR(arg1.r_val_f), sys.ADR(arg2.r_val_f));
      | g : flt.CMPG (sys.ADR(arg1.r_val_g), sys.ADR(arg2.r_val_g));
      | d : flt.CMPD (sys.ADR(arg1.r_val_d), sys.ADR(arg2.r_val_d));
      END;
      IF flt.Et THEN Error(msg.ErrorRealOperation) END;
      res.b_val := flt.Z OR flt.N;
     <* ELSE *>
      res.b_val := arg1.r_val <= arg2.r_val;
     <* END  *>
    | Address:
      res.sort := BOOLval;
      IF (arg2.sort = Address) THEN
        res.b_val := arg1.address <= arg2.address;
      ELSE
        Error(msg.IncompatibleTypes) -- Хочу, но не могу...
      END;
    | CHARval:
      res.sort := BOOLval;
      res.b_val := arg1.ch <= arg2.ch;
    | STRINGval:
      res.sort := BOOLval;
      res.type := dt.st_original;
      res.b_val := arg1.string <= arg2.string;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;

  | and :
    CASE arg1.sort OF
    | INTval, CARDval, WHOLEval:
      res.sort  := arg1.sort;
      res.value := LONGCARD( BITSET(arg1.value) * BITSET(arg2.value) );
    | BOOLval:
      res.sort := BOOLval;
      res.b_val := arg1.b_val AND arg2.b_val;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;

  | or :
    CASE arg1.sort OF
    | INTval, CARDval, WHOLEval:
      res.sort  := arg1.sort;
      res.value := LONGCARD( BITSET(arg1.value) + BITSET(arg2.value) );
    | BOOLval:
      res.sort := BOOLval;
      res.b_val := arg1.b_val OR arg2.b_val;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;

  | xor :
    CASE arg1.sort OF
    | INTval, CARDval, WHOLEval:
      res.sort  := arg1.sort;
      res.value := LONGCARD( BITSET(arg1.value) / BITSET(arg2.value) );
    | BOOLval:
      res.sort := BOOLval;
      res.b_val := arg1.b_val # arg2.b_val;
    ELSE
      Error(msg.IncompatibleTypes) -- Хочу, но не могу...
    END;
  ELSE
    Error(msg.IncompatibleTypes); -- Hе понял. Чего делать-то?!
  END;
<* POP *>
EXCEPT
  IF exm.IsM2Exception() THEN
    IF (exm.M2Exception() = exm.wholeDivException) OR
       (exm.M2Exception() = exm.realDivException)
    THEN
      Error(msg.DivizionByZero); -- Деление на ноль
    END;
  END;
END Binar;


PROCEDURE Field (rec: ExprRes; VAR res: ExprRes);
VAR
  i    : CARDINAL;
  tag  : dt.TYPE_TAG;
  name : xs.txt_ptr;
  field: dt.TYPE_RECORD_FIELD;
BEGIN
  IF rec.sort = Register THEN
    IF tls.IsTypeValid(IndirectType) THEN
      IndirectTypeAddr (rec, rec);
    ELSE
      Error(msg.IllegalUsageOperation) -- Хочу, но не могу...
    END;
  ELSIF rec.sort # Variable THEN
    Error(msg.ExpectedIdent);
  END;
  ASSERT( tls.TypeTag(rec.var_type, tag) );
  IF tag = dt.Pointer THEN
    tls.SubType(rec.var_type, res.var_type);
    ASSERT(tls.TypeTag(res.var_type, tag));
    IF (tag = dt.Record) OR (tag = dt.Class) THEN
      Unar (bar, rec, rec);
    END;
  END;
  CASE tag OF
  | dt.Record, dt.Class:
    GetToken;
    IF Token # ident THEN Error(msg.ExpectedFieldName); END;
    FOR i := 1 TO tls.TypeLen (rec.var_type) DO
      tls.Field (rec.var_type, i, field);
      name := tls.GetName ( field.FieldName);
      IF Ident = name^ THEN
        WITH res DO
          sort := Variable;
          type := dt.st_original;
          var_type :=  field.FieldType;
          location := rec.location+field.FieldOffs;
        END;
       <* IF DEST_XDS THEN *>
        IF opt.AutoDetectActualType THEN
          GetActualType (res);
        END;
       <* END *>
        GetToken;
        RETURN;
      END;
    END;
  ELSE
    Error(msg.ObjectIsNotRecord);
  END;
  Error(msg.ExpectedFieldName);
END Field;


PROCEDURE Index(arr, index: ExprRes; VAR res: ExprRes);
VAR
  tag: dt.TYPE_TAG;
  index_type, range_type: dt.PTYPE;
  min, max: CARDINAL;
  l : CARDINAL;
  SubSize: CARDINAL;
BEGIN
  IF (arr.sort # Variable) THEN Error(msg.ObjectIsNotArray); END;
  ASSERT( tls.TypeTag(arr.var_type, tag) );
  IF tag = dt.Pointer THEN
    tls.SubType(arr.var_type, index_type);
    ASSERT( tls.TypeTag(index_type, tag) );
    IF (tag = dt.Array) OR (tag = dt.Array_of) OR (tag = dt.OpenArray) THEN
      Unar (bar, arr, arr);
    END;
  END;
  IF (tag # dt.Array) AND (tag # dt.Array_of) AND (tag # dt.OpenArray) THEN Error(msg.ObjectIsNotArray); END;
  Var_Value(FALSE, FALSE, index, index);
  tls.ArrayIndexType(arr.var_type, index_type);
  res.sort := Variable;
  res.type := dt.st_original;
  tls.SubType(arr.var_type, res.var_type);
  CASE tag OF
  | dt.Array:
    ASSERT( tls.TypeTag(index_type, tag) );
    IF tag = dt.Range THEN
      tls.SubType(index_type, range_type);
      ASSERT( tls.TypeTag(range_type, tag) );
    END;
    tls.Index(index_type, min, max);
    SubSize := tls.TypeSize(res.var_type)

  | dt.Array_of:
    ASSERT(tls.TypeTag(index_type, tag));
    min := 0;
    IF NOT ArrayOf_Len(arr, max) THEN Error(msg.ErrorAccessMemory); END;
    DEC(max);
    IF NOT ArrayOf_Size(arr, SubSize) THEN Error(msg.ErrorAccessMemory); END;

  | dt.OpenArray:
    ASSERT( tls.TypeTag(index_type, tag) );
    min := 0;
    IF NOT OpenArray_Len(arr, max) THEN Error(msg.ErrorAccessMemory); END;
    DEC(max);
    IF NOT OpenArray_Size(arr, SubSize) THEN Error(msg.ErrorAccessMemory); END;
  END;
  CASE tag OF
  | dt.Card:
    CASE index.sort OF
    | CARDval , WHOLEval, INTval  :
      IF LONGINT(index.value) < 0 THEN Error(msg.RangeError); END;
      IF (index.value < min ) OR (index.value > max) THEN
        Error(msg.RangeError); -- Range error;
      END;
      res.location := arr.location + (index.value-min) * SubSize;
    ELSE
      Error(msg.IncompatibleTypeIndex);
    END;
  | dt.Char:
    IF index.sort = CHARval THEN
      IF (index.ch < CHAR(min) ) OR (index.ch > CHAR(max)) THEN
        Error(msg.RangeError); -- Range error;
      END;
      res.location := arr.location + (ORD(index.ch)-min) * SubSize;
    ELSE
      Error(msg.IncompatibleTypeIndex); -- Индекс - только целое число
    END;
  | dt.Boolean:
    IF index.sort = BOOLval THEN
      IF (index.b_val < BOOLEAN(min) ) OR (index.b_val > BOOLEAN(max)) THEN
        Error(msg.RangeError); -- Range error;
      END;
      res.location := arr.location + (ORD(index.b_val)-min) * SubSize;
    ELSE
      Error(msg.IncompatibleTypeIndex); -- Индекс - только целое число
    END;
  | dt.Int :
    CASE index.sort OF
    | CARDval, WHOLEval, INTval  :
      IF (index.sort = CARDval) AND (LONGINT(index.value) < 0) THEN Error(msg.RangeError); END;
      IF (LONGINT(index.value) < LONGINT(min)) OR (LONGINT(index.value) > LONGINT(max)) THEN
        Error(msg.RangeError); -- Range error;
      END;
      res.location :=  arr.location + LONGCARD((LONGINT(index.value)-LONGINT(min))) * SubSize;
    ELSE
      Error(msg.IncompatibleTypeIndex);
    END;
  | dt.Enum:
    IF (index.sort = Variable) THEN
      IF NOT tls.TypeTag(index.var_type, tag) THEN Error(msg.RangeError); END;
      IF tag # dt.Enum THEN Error(msg.RangeError) END;
      l := 0;
      IF NOT mem.Get(index.location, sys.ADR(l), tls.TypeSize(index.var_type)) THEN
        Error(msg.ErrorAccessMemory);
      END;
      IF (l < min) OR (l > max) THEN
        Error(msg.IncompatibleTypeIndex); -- Range error;
      END;
      res.location :=  arr.location + (l-min) * SubSize;
      res.type := dt.st_original;
    ELSIF (index.sort = CARDval) OR (index.sort = WHOLEval) OR
          ((index.sort = INTval) AND (LONGINT(index.value) >= 0)) THEN
      IF (index.value < min) OR (index.value > max) THEN
        Error(msg.RangeError); -- Range error;
      END;
      res.location :=  arr.location + (index.value-min) * SubSize;
      res.type := dt.st_original;
    ELSE
      Error(msg.IncompatibleTypeIndex);
    END;
  ELSE
    Error(msg.IncompatibleTypeIndex);
  END;
 <* IF DEST_XDS THEN *>
  IF opt.AutoDetectActualType THEN
    GetActualType (res);
  END;
 <* END *>
END Index;



PROCEDURE CheckRegionValid (arg: ExprRes): BOOLEAN;
VAR
  start, size: CARDINAL;
  access: kt.ATTRIBS;
BEGIN
  ASSERT(arg.sort = Variable);
--  IF arg.location = NIL_ADDR THEN RETURN FALSE; END;
  IF NOT mem.GetSegmentInfo(arg.location, start, size, access) THEN RETURN FALSE END;
  RETURN (kt.read IN access) AND ((arg.location + tls.TypeSize(arg.var_type)) <= (start + size));
EXCEPT
  RETURN FALSE;
END CheckRegionValid;


PROCEDURE QualIdent(VAR res: ExprRes);

<* IF xd_batch_included THEN *>
VAR
  equname : xs.String;

  _ExprStr : xs.txt_ptr;
  _Token   : TOKEN;
  _char    : CHAR;
  _next    : CHAR;
  _pos     : CARDINAL;
  _Ident   : IDENT;

  PROCEDURE Save;
  BEGIN
    _ExprStr := ExprStr;
    _Token   := Token;
    _char    := char;
    _next    := next;
    _pos     := pos;
    _Ident   := Ident;
  END Save;

  PROCEDURE Restore;
  BEGIN
    ExprStr := _ExprStr;
    Token   := _Token;
    char    := _char;
    next    := _next;
    pos     := _pos;
    Ident   := _Ident;
  END Restore;

<* END *>

VAR
  ExecScope: dt.OBJECT;
  comno, c : dt.ComNo;
  modno, m : CARDINAL;
  IP       : kt.ADDRESS;
  type     : dt.PTYPE;
  sym_tag  : dt.SYM_TAG;

  FirstTime : BOOLEAN;
  FirstTime2: BOOLEAN;

 <* IF DEST_K26 THEN *>
  device: CARDINAL;
  reg   : CARDINAL;
 <* END *>


  PROCEDURE GetLocalObject (p: dt.OBJECT): BOOLEAN;
  VAR
    v        : dt.OBJECT;
    level    : CARDINAL;
    ref      : BOOLEAN;
    reg_no   : CARDINAL;
    reg_val  : kt.REG_VALUE;
    type_tag : dt.TYPE_TAG;
    top_call : BOOLEAN;
   <* IF DEST_XDS THEN *>
    tmp      : kt.ADDRESS;
   <* END *>
   <* IF TARGET_x86 THEN *>
    begin    : kt.ADDRESS;
   <* END *>
  BEGIN
    -- поиск локальных обьектов происходит только если:
    --
    -- 1) ExecScope - текущая исполняемая процедура и выполняется
    --    - квалифицированный поиск локальных обьектов
    --    - неквалифицированный поиск локальных обьектов
    --    при этом ExecScope == p
    --
    -- 2) глобальный поиск среди процедур модуля привел к
    --    - квалифицированному поиску локальных обьектов
    --    при этом возможно, что ExecScope ## p
    --
    -- 3) найденный локальный обьект оказался процедурой и выполняется
    --    - квалифицированный поиск локальных обьектов
    --    при этом возможно, что ExecScope ## p

    -- определение текущего контекста
    LOOP
      IF tls.EqualObjects (p, dt.Invalid_Object) THEN
        RETURN FALSE;
      END;
      v := tls.FindLocalVar (p, Ident);
      IF tls.IsObjectValid (v) THEN
        EXIT;
      END;
      p := tls.ObjectParentScope (p);
    END;

    ASSERT (tls.ObjectTag(v, sym_tag));
    ASSERT (tls.ObjectType(v, res.var_type));
    CASE sym_tag OF
    | dt.Sy_Register:
      -- только для текущей *исполняемой* процедуры
      IF NOT tls.EqualObjects (p, ExecScope) THEN
        Error(0);
      END;
      -- теперь ясно, что и ExecScope определен
     <* IF TARGET_x86 THEN *>
      -- адрес должен быть как минимум внутри тела процедуры,
      IF NOT tls.AddrInProcBody (p, mem.GetIP()) THEN
        Error(0);
      END;
     <* END *>
      ASSERT (tls.GetLocalObject_Reg(v, reg_no, ref));
      ASSERT (mem.GetReg(reg_no, reg_val));
      IF ref THEN
        res.sort := Variable;
        res.type := dt.st_original;
        res.location := kt.ADDRESS(reg_val);
       <* IF DEST_XDS THEN *>
        IF opt.AutoDetectActualType THEN
          GetActualType (res);
        END;
       <* END *>
      ELSE
        ASSERT(tls.TypeTag(res.var_type, type_tag));
        IF type_tag = dt.Pointer THEN
          -- возможно, в дальнейшем потребуется разыменование
          -- указателя, расположенного на регистре, см. Unar(bar)
          -- вызов Unar сразу после завершения этого вызова QualIdent
          -- в Designator, приведет к разыменованию регистра
          -- (т.е. косвенно по регистру с типом IndirectType
          tls.SubType (res.var_type, IndirectType);
        ELSE
          IndirectType := dt.Invalid_Type;
        END;
        res.sort := Register;
        res.reg_no := reg_no;
        res.reg_type := res.var_type;
      END;
      GetToken;

    | dt.Sy_Relative:
      top_call := tls.EqualObjects (ExecScope, p);
      IF top_call AND tls.ProcHasFrame (ExecScope) AND
        NOT tls.AddrInProcBody (ExecScope, mem.GetIP())
      THEN
        -- текущий и исполняемый контекст совпали, но
        -- еще не инициализирована база текущего контекста
        Error (0);
      END;
      ASSERT (tls.GetLocalObject_Reg (v, reg_no, ref));
      IF reg_no = kt.FRAME_REG THEN
        -- переменная относительно кадра (FRAME_REG), а не произвольного регистра
       <* IF DEST_XDS THEN *>
        stk.ScanCallStack;
       <* END *>
        -- поискать текущий в стеке вызовов
        IF NOT stk.GetObjectLevelInCallStack (0, p, level) THEN
          RETURN FALSE;
        END;
        -- достать базу текущего контекста из стека
        IF NOT stk.GetFrame (level, reg_val) THEN
          RETURN FALSE;
        END;
      ELSIF top_call THEN
        -- текущий и исполняемый контекст совпали
        -- при этом база обьекта не является регистром кадра
        ASSERT (mem.GetReg (reg_no, reg_val));
      ELSE
        -- текущий и исполняемый контекст не совпали, получить
        -- значение регистра уже нельзя
        Error (0);
      END;
      res.sort := Variable;
      res.type := dt.st_original;
      ASSERT(tls.GetLocalObject_Addr (v, reg_val, res.location));
      IF ref THEN
       <* IF DEST_XDS THEN *>
        IF opt.AutoDetectActualType THEN
          tmp := res.location;
          INC(res.location, 4);
          sys.EVAL(CheckActualType (res, FALSE));
          res.location := tmp;
        END;
       <* END *>
        IF NOT mem.Get (res.location, sys.ADR(res.location), 4) THEN
          Error (0);
        END;
      ELSE
       <* IF DEST_XDS THEN *>
        IF opt.AutoDetectActualType THEN
          GetActualType (res);
        END;
       <* END *>
      END;
      GetToken;

    | dt.Sy_Var:
      res.sort := Variable;
      res.type := dt.st_original;
      ASSERT(tls.ObjectAddr(v, res.location));
      GetToken;
     <* IF DEST_XDS THEN *>
      IF opt.AutoDetectActualType THEN
        GetActualType (res);
      END;
     <* END *>

    | dt.Sy_Proc:
      res.sort := Address;
      ASSERT(tls.ObjectAddr(v, res.address));
      GetToken;
      (* Qualified search local variable *)
      IF Token = loc_qual THEN
        GetToken;
        IF Token <> ident THEN
          Error(msg.ExpectedIdent);
        END;
        IF NOT GetLocalObject(v) THEN
          Error(msg.ObjectNotFound);
        END;
      END;
    END;
    RETURN TRUE;
  END GetLocalObject;


  PROCEDURE FindGlobal (module_number: dt.ModNo): BOOLEAN;
  VAR
    p: dt.OBJECT;
  BEGIN
    p := tls.FindObjectByName (comno, module_number, Ident);
    IF tls.IsObjectValid(p) THEN
      ASSERT(tls.ObjectTag(p, sym_tag));
      CASE sym_tag OF
      | dt.Sy_Proc:
        res.sort := Address;
        ASSERT(tls.ObjectAddr(p, res.address));
        GetToken;
        (* Qualified search local variable *)
        IF Token = loc_qual THEN
          GetToken;
          IF Token <> ident THEN
            Error(msg.ExpectedIdent);
          END;
          IF NOT GetLocalObject(p) THEN
            Error(msg.ObjectNotFound);
          END;
        END;
      | dt.Sy_Var:
        res.sort := Variable;
        res.type := dt.st_original;
        ASSERT(tls.ObjectType(p, res.var_type));
        ASSERT(tls.ObjectAddr(p, res.location));
        GetToken;
       <* IF DEST_XDS THEN *>
        IF opt.AutoDetectActualType THEN
          GetActualType (res);
        END;
       <* END *>
      END;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END FindGlobal;


VAR
  name: xs.String;

BEGIN
 <* IF xd_batch_included THEN *>
  (* Поиск эквивалентного имени *)
  IF lst.IsEquName(Ident) THEN
    IF lst.IsEngaged(Ident) THEN Error(msg.UsageEquNameInCycles); END; -- -ЁЄ<Ёз_бЄR_ ЁбЇR<мRў -Ё_ нЄў. Ё┐_-Ё
    lst.SetEngaged(Ident);
    lst.GetEquName (Ident, equname);
    Save;
    GetLeftValue (ComNum, ModNum, equname, res);
    Restore;
    lst.DelEngaged(Ident);
    IF (error#0) OR NOT dfn THEN Error(error); END;
    GetToken;
    RETURN;
  END;
 <* IF DEST_K26 THEN *>
  IF bas.CheckMode(typ.KodSuccess) AND mdl.FindDeviceByName (Ident, device) THEN
    GetToken;
    IF Token # period THEN Error(msg.ExpectedDot); END;
    GetToken;
    IF (Token # ident) AND (Token # identrus) THEN Error(msg.ExpectedIdent); END;
    IF mdl.FindByNameAddress (device, Ident, reg) THEN
      res.sort := Address;
      mdl.RegisterAddress (device, reg, res.address);
      GetToken;
      RETURN;
    ELSE
      Error(msg.ObjectNotFound);
    END;
  END;
 <* END *>
 <* END *>

  IF kexe.Loaded THEN
    (* Find executed scope *)
    IP := mem.GetIP();
    IF tls.FindModByAddr (IP, c, m) THEN
      ExecScope := tls.FindProcByAddr (c, m, IP);
    ELSE
      ExecScope := dt.Invalid_Object;
    END;
    comno := ComNum;
    modno := ModNum;
    FirstTime := TRUE;
    LOOP
      IF (comno # dt.Invalid_Component) AND (modno # dt.Invalid_Module) THEN
        IF tls.IsObjectValid(ExecScope) THEN
          tls.ObjectName (ExecScope, name);
          IF name = Ident THEN
            -- это исполняемая процедура
            res.sort := Address;
            ASSERT(tls.ObjectAddr(ExecScope, res.address));
            GetToken;
            IF Token = loc_qual THEN
              -- квалифицированный поиск локальных переменных
              GetToken;
              IF Token <> ident THEN
                Error(msg.ExpectedIdent);
              END;
              IF NOT GetLocalObject(ExecScope) THEN
                Error(msg.ObjectNotFound);
              END;
              -- результат поиска - локальныя переменная
            END;
            -- результат поиска - процедура
            RETURN;
          ELSIF FirstTime AND             -- только если сумеем найти среди локалов с первого раза
                GetLocalObject(ExecScope) -- неквалифицированный поиск локальных переменных
          THEN
            -- результат поиска - локальныя переменная
            RETURN;
          END;
        END;
        -- Поиск глобальных обьектов в модуле
        IF FindGlobal (modno) THEN
          RETURN;
        END;
        -- Поиск глобальных обьектов в компоненте
        IF FindGlobal (dt.Fake_Module) THEN
          RETURN;
        END;
        -- Поиск значения перечислимого типа
        IF tls.FindValueByEnumeration (comno, modno, Ident, res.value) THEN
          res.sort := CARDval;
          res.type  := dt.st_original;
          GetToken;
          RETURN;
        END;
        -- Поиск типа
        IF tls.FindType (comno, modno, Ident, IndirectType) THEN
          GetToken;
          CASE Token OF
          | lss:
            type := IndirectType;
            GetToken;
            Simple(res);
            CheckToken(gtr);
            IndirectType := type;
            IndirectTypeAddr(res,res);
            RETURN;
          | lpar:
            type := IndirectType;
            GetToken;
            Simple(res);
            CheckToken(rpar);
            IndirectType := type;
            TypeCast(res, res);
            RETURN;
          ELSE
            Error(msg.IncorrectExpression);
          END;
        END;
      END;

      IF NOT FirstTime THEN EXIT; END;
      FirstTime := FALSE;

      FirstTime2 := TRUE;
      LOOP
        IF comno # dt.Invalid_Component THEN
          IF tls.FindModInComp (comno, Ident, modno) THEN
            GetToken;
            IF Token # period THEN Error(msg.ExpectedDot); END;
            GetToken;
            IF Token # ident THEN Error(msg.ExpectedIdent); END;
            EXIT;
          END;
          IF tls.FindPublicByNameInCom (comno, Ident, res.address) THEN
            res.sort := Address;
            GetToken;
            RETURN;
          END;
        END;

        IF NOT FirstTime2 THEN EXIT END;
        FirstTime2 := FALSE;

        IF tls.FindComponentByName (Ident, comno) THEN
          GetToken;
          IF Token # period THEN Error(msg.ExpectedDot); END;
          GetToken;
          IF (Token # ident) AND (Token # identrus) THEN Error(msg.ExpectedIdent); END;
        ELSE
          comno := 0;
        END;
      END;
    END;
  END;
  (* Поиск стандартного типа *)
  IF tls.FindStdType(Ident, IndirectType) THEN
    GetToken;
    CASE Token OF
    | lss:
      type := IndirectType;
      GetToken;
      Simple(res);
      CheckToken(gtr);
      IndirectType := type;
      IndirectTypeAddr(res,res);
      RETURN;
    | lpar:
      type := IndirectType;
      GetToken;
      Simple(res);
      CheckToken(rpar);
      IndirectType := type;
      TypeCast(res, res);
      RETURN;
    ELSE
      Error(msg.IncorrectToken);
    END;
  END;
  Error(msg.ObjectNotFound);
END QualIdent;



PROCEDURE Designator(VAR res: ExprRes);
VAR
  res1: ExprRes;
BEGIN
  IF (Token#ident) AND (Token#identrus) THEN Error(msg.ExpectedIdent); END;
  QualIdent(res);
  LOOP
    CASE Token OF
    | period:
      Field(res, res);
    | bar:
      Unar(bar, res, res);
      GetToken;
    | lbr:
      GetToken;
      Expr(res1);
      IF Token = comma THEN Token := lbr;
      ELSE                  CheckToken(rbr);
      END;
      Index(res, res1, res);
    ELSE
      EXIT;
    END;
  END;
  IndirectType := dt.Invalid_Type;
END Designator;

PROCEDURE ExactlyAddress(VAR res:ExprRes);
VAR
  c   : CHAR;            (* Очередной символ                 *)
  sum : CARDINAL;
  emp : BOOLEAN;
BEGIN
  (* Явный адрес - "A:"{ШестнадцатиричнаяЦифра} *)
  sum := 0;
  emp := TRUE;
  LOOP
    c := char;
    IF ALPHA[c] >= 4C THEN (* десятичная цифpа *)
      sum := sum*16 + (ORD(c) - ORD('0'));
    ELSE
      c := CAP(c);
      IF ALPHA[c] < 2C THEN (* не шестнадцатеpичная цифpа *)
        EXIT;
      END;
      sum := sum*16 + (10+(ORD(c) - ORD('A')));
    END;
    emp := FALSE;
    getchar;
  END;
  IF NOT emp THEN
    res.sort    := Address;
    res.address := sum;
  ELSE
    Error(msg.ExpectedIntConstant); -- Явный адрес содержит только целое X-значение
  END;
EXCEPT
  IF exm.IsM2Exception() THEN
    ASSERT(exm.M2Exception() = exm.wholeValueException);
    Error(msg.Incorrect_address); -- Слишком длинное число
    RETURN;
  END;
END ExactlyAddress;

PROCEDURE Factor(VAR res: ExprRes);
VAR
  op  : TOKEN;
  desc: STD_NAME_DESC;
BEGIN
  CASE Token OF
  | not:
    op := Token;
    GetToken;
    Expr(res);
    Unar(op, res, res);
  | address:
    ExactlyAddress(res);
    GetToken;
  | lpar:
    GetToken;
    Expr(res);
    CheckToken(rpar)
  | exe_time:
    Exe_Time(res);
    GetToken;
  | pc_time:
    PC_Time(res);
    GetToken;
  | lss:
    GetToken;
    Simple(res);
    Unar(indir_addr,res,res);
    IF Token = geq THEN
      Token := equ;
    ELSE
      CheckToken(gtr);
    END;
  | take_addr:
    GetToken;
    Designator(res);
    CheckToken(rbrace);
    Unar(take_addr, res, res);
  | func_res:
    GetToken;
    CheckToken(lpar);
    Std_Func[TmpFunc].func(res);
    CheckToken(rpar);
  | ref:
    res := TmpRes;
    GetToken;
  | register:
    res := TmpRes;
    GetToken;
  | const_val:
    res := TmpRes;
    GetToken;
  | ident, identrus:
    IF FindStdName (Ident, desc) THEN
      CASE desc.kind OF
      | std_const:
        IF NOT desc.intern THEN
          -- стандартный идентификатор, не требующий @
          res := desc.res;
          GetToken;
          RETURN;
        END;
      | std_func:
        IF NOT Std_Func[desc.f_ID].spec THEN
          GetToken;
          CheckToken(lpar);
          Std_Func[desc.f_ID].func(res);
          CheckToken(rpar);
          RETURN;
        END;
      ELSE
      END;
    END;
    Designator(res);
  ELSE
    Error(msg.IncorrectFactor);
  END
END Factor;

PROCEDURE Term(VAR res: ExprRes);
VAR
  op: TOKEN;
  res1, res2: ExprRes;
BEGIN
  Factor(res1);
  LOOP
    IF Token IN MULTIPLOP THEN
      op := Token;
      GetToken;
      Factor(res2);
      Binar(op, res1, res2, res1)
    ELSE
      EXIT;
    END
  END;
  res := res1;
END Term;

PROCEDURE Simple(VAR res: ExprRes);
VAR
  op: TOKEN;
  res1, res2: ExprRes;
BEGIN
  IF (Token = plus) OR (Token = minus) THEN
    op := Token;
    GetToken;
    Term(res1);
    Unar(op, res1, res1);
  ELSE
    Term(res1)
  END;
  LOOP
    IF Token IN ADDITIVES THEN
      op := Token;
      GetToken;
      Term(res2);
      Binar(op, res1, res2, res1)
    ELSE
      EXIT
    END
  END;
  res := res1;
END Simple;

PROCEDURE Expr(VAR res: ExprRes);
VAR
  op   : TOKEN;
  res1, res2 : ExprRes;
BEGIN
  Simple(res1);
  LOOP
    IF Token IN RELATION THEN
      op := Token;
      GetToken;
      Simple(res2);
      Binar(op, res1, res2, res1)
    ELSE
      EXIT;
    END
  END;
  res := res1;
END Expr;


PROCEDURE GetLeftValue (com: dt.ComNo; mod: dt.ModNo; st-: ARRAY OF CHAR; VAR res: ExprRes);
BEGIN
  ComNum := com;
  ModNum := mod;
  ExprStr := sys.ADR(st);
  char := ExprStr^[0];
  IF char = EL THEN
    Error(msg.ExpressionIsEmpty); -- Выражение пустое
  ELSE
    next := ExprStr^[1];
  END;
  pos := 2;
  dfn := TRUE;
  error := 0;
  GetToken;
  Expr(res);
  IF Token <> none THEN Error(msg.IncorrectExpression); END;
EXCEPT
  IF exc.IsCurrentSource(source) THEN
    error := exc.CurrentNumber(source);
    IF error = 0 THEN
      dfn := FALSE;
      error := msg.Undefined_Expression;
    END;
    RETURN;
  END;
END GetLeftValue;


PROCEDURE CalcExpr(com: dt.ComNo; mod: dt.ModNo; st-: ARRAY OF CHAR; VAR res: ExprRes);
BEGIN
  GetLeftValue (com, mod, st, res);
  IF (error = 0) AND dfn THEN
    Var_Value (opt.ConvertVar2Ref, FALSE, res, res);
  END;
EXCEPT
  IF exc.IsCurrentSource(source) THEN
    error := exc.CurrentNumber(source);
    IF error = 0 THEN
      dfn := FALSE;
      error := msg.Undefined_Expression;
    END;
    RETURN;
  END;
END CalcExpr;

PROCEDURE GetIntValue (com: dt.ComNo; mod: dt.ModNo; st-: ARRAY OF CHAR; VAR value: LONGINT) : BOOLEAN;
VAR
  exprRes : ExprRes;
BEGIN
  CalcExpr (com, mod, st, exprRes);
  IF (error # 0) OR NOT dfn THEN RETURN FALSE; END;
  CASE exprRes.sort OF
  | INTval, WHOLEval :
    value := LONGINT(exprRes.value);
    RETURN TRUE;
  ELSE
    error := msg.CannotConvet2Int;
    RETURN FALSE;
  END;
END GetIntValue;


PROCEDURE ConvertReal2Host (VAR res: ExprRes; VAR value: LONGLONGREAL): BOOLEAN;

<* IF DEST_K26 THEN *>
VAR
  format: CHAR;
<* END *>

BEGIN
  IF res.sort = REALval THEN
   <* IF DEST_K26 THEN *>
    CASE res.r_type OF
    | g        : format := 'g';
    | f        : format := 'f';
    | d        : format := 'd';
    | PC_const : value := res.r_val_const;
                 RETURN TRUE;
    END;
    IF icnv.FloatVAX_PC(sys.ADR(res.r_val_f), format, value) THEN
      RETURN TRUE;
    END;
   <* ELSE *>
    value := res.r_val;
    RETURN TRUE;
   <* END *>
  END;
  RETURN FALSE;
END ConvertReal2Host;


PROCEDURE GetRealValue (com: dt.ComNo; mod: dt.ModNo; st: ARRAY OF CHAR; VAR value:LONGLONGREAL): BOOLEAN;
VAR
  exprRes : ExprRes;
BEGIN
  CalcExpr (com, mod, st, exprRes);
  IF (error # 0) OR NOT dfn THEN RETURN FALSE; END;
  IF exprRes.sort = REALval THEN
   <* IF DEST_K26 THEN *>
    RETURN ConvertReal2Host (exprRes, value);
   <* ELSE *>
    value := exprRes.r_val;
    RETURN TRUE;
   <* END *>
  ELSE
    error := msg.IncompatibleTypes;
    RETURN FALSE;
  END;
END GetRealValue;


PROCEDURE GetCardValue (com: dt.ComNo; mod: dt.ModNo; st-: ARRAY OF CHAR; VAR value: LONGCARD) : BOOLEAN;
VAR
  exprRes : ExprRes;
BEGIN
  CalcExpr (com, mod, st, exprRes);
  IF (error # 0) OR NOT dfn THEN RETURN FALSE; END;
  CASE exprRes.sort OF
  | CARDval, WHOLEval :
    value := exprRes.value;
    RETURN TRUE;
  ELSE
    error := msg.CannotConvet2Int;
    RETURN FALSE;
  END;
END GetCardValue;

PROCEDURE GetRelation (com: dt.ComNo; mod: dt.ModNo; st-: ARRAY OF CHAR; VAR res:BOOLEAN) : BOOLEAN;
VAR
  exprRes : ExprRes;
BEGIN
  CalcExpr (com, mod, st, exprRes);
  IF (error # 0) OR NOT dfn OR (exprRes.sort # BOOLval) THEN RETURN FALSE; END;
  res := exprRes.b_val;
  RETURN TRUE;
END GetRelation;


PROCEDURE GetAddrOrReg (com: dt.ComNo; mod: dt.ModNo; st-: ARRAY OF CHAR;
                        VAR addr :kt.ADDRESS;
                        VAR RegNo:CARDINAL)            : BOOLEAN;
VAR
  exprRes: ExprRes;
  tag    : dt.TYPE_TAG;
BEGIN
  GetLeftValue (com, mod, st, exprRes);
  IF (error # 0) OR NOT dfn THEN RETURN FALSE; END;
  RegNo := MAX(CARDINAL);
  addr  := NIL_ADDR;
  IF exprRes.sort = Register THEN
    RegNo := exprRes.reg_no;
    RETURN TRUE;
  END;

  IF NOT Var2Value (exprRes, exprRes) THEN RETURN FALSE; END; 

  IF (exprRes.sort = CARDval) OR (exprRes.sort = WHOLEval) THEN
    addr := kt.ADDRESS(exprRes.value);
  ELSIF (exprRes.sort = INTval) AND (LONGCARD(exprRes.value) > 0) THEN
    addr := kt.ADDRESS(exprRes.value);
  ELSIF exprRes.sort = Address  THEN
    addr := exprRes.address;
  ELSIF exprRes.sort = Reference THEN
    addr := exprRes.reference;
  ELSIF exprRes.sort = Variable THEN
    exprRes.type := dt.st_original;
    ASSERT( tls.TypeTag(exprRes.var_type, tag) );
    CASE tag OF
    | dt.Procedure:
      addr := exprRes.location;
    | dt.Pointer:
      addr := 0;
      IF NOT mem.Get(exprRes.location, sys.ADR(addr), 4) THEN
        error := msg.CannotConvert2AddrReg;
        RETURN FALSE;
      END;
    ELSE
      error := msg.CannotConvert2AddrReg;
      RETURN FALSE;
    END;
  ELSE
    error := msg.CannotConvert2AddrReg;
    RETURN FALSE;
  END;
  RETURN TRUE;
END GetAddrOrReg;


PROCEDURE GetAddress (com: dt.ComNo; mod: dt.ModNo; st-: ARRAY OF CHAR; VAR addr:kt.ADDRESS) : BOOLEAN;
VAR
  RegNo: CARDINAL;
BEGIN
  IF GetAddrOrReg (com, mod, st, addr, RegNo) THEN
    IF RegNo = MAX(CARDINAL) THEN 
      RETURN TRUE; 
    ELSE
      IF mem.GetReg(RegNo, addr) THEN RETURN TRUE END;
    END; (* Не регистр, а адрес *)
  END;
  error := msg.Expected_address;
  RETURN FALSE;
END GetAddress;


PROCEDURE Assign_Register(reg_no: CARDINAL; right: ExprRes): BOOLEAN;
BEGIN
  Var_Value(opt.ConvertVar2Ref, TRUE, right, right);
  CASE right.sort OF
  | CARDval, INTval, WHOLEval:
    IF mem.SetReg(reg_no, right.value) THEN RETURN TRUE; ELSE error := 999; END;
  | Address:
    IF mem.SetReg(reg_no, right.address) THEN RETURN TRUE; ELSE error := 999; END;
  | Reference:
    IF mem.SetReg(reg_no, right.reference) THEN RETURN TRUE; ELSE error := 999; END;
  ELSE
    error := msg.IncompatibleTypes;
  END;
  RETURN FALSE;
END Assign_Register;


-- left <- right
PROCEDURE Assign (left, right: ExprRes): BOOLEAN;
VAR
  t   : dt.PTYPE;
  l, i: CARDINAL;
  a   : kt.ADDRESS;
  tag : dt.TYPE_TAG;
  p1  : POINTER TO sys.INT8;
  p2  : POINTER TO sys.INT16;
  p4  : POINTER TO sys.INT32;
  buff: LONGCARD;
  addr: sys.ADDRESS;

  len   : CARDINAL;
<* IF DEST_K26 THEN *>
  format: CHAR;
  buffer: ityp.BoardFloatD;
<* ELSE *>
  r: REAL;
  lr: LONGREAL;
<* END *>

BEGIN
  IF left.sort = Register THEN
    RETURN Assign_Register (left.reg_no, right);
  END;
  IF left.sort # Variable THEN
    error := msg.Expected_VarOrReg;
    RETURN FALSE;
  END;
  a := left.location;
  t := left.var_type;
  l := tls.TypeSize(t);
  Var_Value (opt.ConvertVar2Ref, TRUE, right, right);
  ASSERT (tls.TypeTag (t, tag));
  CASE tag OF
  | dt.Byte:
    CASE right.sort OF
    | CARDval, WHOLEval, INTval:
      IF right.value <= Max_Card[l] THEN
        IF mem.Put(a, sys.ADR(right.value), l) THEN
          RETURN TRUE;
        ELSE
          error := msg.ErrorAccessMemory;
        END;
      ELSE
        error := msg.RangeError;
      END;
    ELSE
      error := msg.IncompatibleTypes;
    END;
  | dt.Card:
    IF  (right.sort = CARDval) OR (right.sort = WHOLEval) OR
       ((right.sort = INTval) AND (LONGINT(right.value) >= 0)) THEN
      IF right.value <= Max_Card[l] THEN
        IF mem.Put(a, sys.ADR(right.value), l) THEN
          RETURN TRUE;
        ELSE
          error := msg.ErrorAccessMemory;
        END;
      ELSE
        error := msg.RangeError;
      END;
    ELSE
      error := msg.IncompatibleTypes;
    END;
  | dt.Int:
    IF  (right.sort = INTval) OR (right.sort = WHOLEval) OR
       ((right.sort = CARDval) AND (LONGINT(right.value) >= 0)) THEN
      IF ((LONGINT(right.value) >= 0) AND (LONGINT(right.value) <= Max_Int[l])) OR
         ((LONGINT(right.value) <  0) AND (LONGINT(right.value) >= Min_Int[l])) THEN
        addr := sys.ADR(buff);
        CASE l OF
        | 1 : p1 := addr; p1^ := VAL(sys.INT8,  LONGINT(right.value));
        | 2 : p2 := addr; p2^ := VAL(sys.INT16, LONGINT(right.value));
        | 4 : p4 := addr; p4^ := VAL(sys.INT32, LONGINT(right.value));
        ELSE
          ASSERT(FALSE);
        END;
        IF mem.Put(a, addr, l) THEN
          RETURN TRUE;
        ELSE
          error := msg.ErrorAccessMemory;
        END;
      ELSE
        error := msg.RangeError;
      END;
    ELSE
      error := msg.IncompatibleTypes;
    END;
  | dt.Real:
    IF right.sort = REALval THEN
<* IF DEST_K26 THEN *>
      CASE right.r_type OF
      | PC_const:
        len := tls.TypeSize (left.var_type);
        CASE len OF
        | 4: format := 'f';
        | 8: CASE GetTypeRealType (left.var_type) OF
             | dt.STD_TYPE_REAL_D:
               format := 'd';
             | dt.STD_TYPE_REAL_G:
               format := 'g';
             END;
        |10: IF mem.Put (left.location, sys.ADR(right.r_val_const), len) THEN
               RETURN TRUE;
             ELSE
               error := msg.ErrorAccessMemory;
               RETURN FALSE;
             END;
        END;
        IF icnv.FloatPC_VAX (right.r_val_const, sys.ADR(buffer), format, error) THEN
          IF error # 0 THEN
            error := msg.ErrorRealOperation;
            RETURN FALSE;
          END;
          IF NOT mem.Put (left.location, sys.ADR(buffer), len) THEN
            error :=msg.ErrorAccessMemory;
            RETURN FALSE;
          END;
        ELSE
          error := msg.ErrorRealOperation;
          RETURN FALSE;
        END;
      | f : IF tls.TypeSize (left.var_type) # 4 THEN
              error := msg.Type_inconsistance;
              RETURN FALSE;
            END;
            IF NOT mem.Put (left.location, sys.ADR(right.r_val_f), 4) THEN
              error := msg.ErrorAccessMemory;
              RETURN FALSE;
            END;
      | g : IF tls.TypeSize (left.var_type) # 8 THEN
              error := msg.Type_inconsistance;
              RETURN FALSE;
            END;
            IF NOT mem.Put (left.location, sys.ADR(right.r_val_g), 8) THEN
              error := msg.ErrorAccessMemory;
              RETURN FALSE;
            END;
      | d : IF tls.TypeSize (left.var_type) # 8 THEN
              error := msg.Type_inconsistance;
              RETURN FALSE;
            END;
            IF NOT mem.Put (left.location, sys.ADR(right.r_val_d), 8) THEN
              error := msg.ErrorAccessMemory;
              RETURN FALSE;
            END;
      END;
      RETURN TRUE;
<* ELSE *>
      len := tls.TypeSize (left.var_type);
      CASE len OF
      | 10: IF NOT mem.Put (left.location, sys.ADR(right.r_val), len) THEN
              error := msg.ErrorAccessMemory;
              RETURN FALSE;
            END;
      | 8 : IF ( MAX(LONGREAL) >= right.r_val ) AND ( right.r_val >= MIN(LONGREAL)) THEN
              lr := VAL(LONGREAL, right.r_val);
              IF NOT mem.Put (left.location, sys.ADR(lr), len) THEN
                error := msg.ErrorAccessMemory;
                RETURN FALSE;
              END;
            ELSE
              error := msg.Type_inconsistance;
              RETURN FALSE;
            END;
      | 4 : IF ( MAX(REAL) >= right.r_val ) AND ( right.r_val >= MIN(REAL)) THEN
              r := VAL(REAL, right.r_val);
              IF NOT mem.Put (left.location, sys.ADR(r), len) THEN
                error := msg.ErrorAccessMemory;
                RETURN FALSE;
              END;
            ELSE
              error := msg.Type_inconsistance;
              RETURN FALSE;
            END;
      END;
      RETURN TRUE;
<* END *>
    ELSE
      error := msg.IncompatibleTypes;
    END;
  | dt.Char:
    IF right.sort = CHARval THEN
      IF l > SIZE(right.ch) THEN
        buff := 0;
        FOR i := 0 TO l-1 DO
          IF NOT mem.Put(a+i, sys.ADR(buff), 1) THEN
            error := msg.ErrorAccessMemory;
          END;
        END;
      END;
      IF mem.Put(a, sys.ADR(right.ch), SIZE(right.ch)) THEN
        RETURN TRUE;
      ELSE
        error := msg.ErrorAccessMemory;
      END;
    ELSE
      error := msg.IncompatibleTypes;
    END;
  | dt.Boolean:
    IF  (right.sort = BOOLval) THEN
      ASSERT(l = SIZE(right.b_val));
      IF mem.Put(a, sys.ADR(right.b_val), l) THEN
        RETURN TRUE;
      ELSE
        error := msg.ErrorAccessMemory;
      END;
    ELSE
      error := msg.IncompatibleTypes;
    END;
  | dt.Range:
    tls.SubType(left.var_type, left.var_type);
    RETURN Assign(left, right);
  | dt.Enum:
    IF  (right.sort = CARDval) OR (right.sort = WHOLEval) OR
       ((right.sort = INTval) AND (LONGINT(right.value) >= 0)) THEN
      IF right.value <= Max_Card[l] THEN
        IF mem.Put(a, sys.ADR(right.value), l) THEN
          RETURN TRUE;
        ELSE
          error := msg.ErrorAccessMemory;
        END;
      ELSE
        error := msg.RangeError;
      END;
    ELSIF (right.sort = Variable) THEN
      IF tls.TypesCompatible(t, right.var_type) THEN
        buff := 0;
        IF NOT mem.Get(right.location, sys.ADR(buff), tls.TypeSize(right.var_type)) THEN
          error := msg.ErrorAccessMemory;
        END;
        IF buff <= Max_Card[l] THEN
          IF mem.Put(a, sys.ADR(buff), l) THEN
            RETURN TRUE;
          ELSE
          END;
        ELSE
          error := msg.RangeError;
        END;
      END;
    ELSE
      error := msg.IncompatibleTypes;
    END;
  | dt.Pointer,
    dt.Address,
    dt.Procedure:
    IF  (right.sort = CARDval) OR (right.sort = WHOLEval) OR
       ((right.sort = INTval) AND (LONGINT(right.value) >= 0))
    THEN
      IF right.value <= Max_Card[l] THEN
        IF mem.Put(a, sys.ADR(right.value), l) THEN
          RETURN TRUE;
        ELSE
          error := msg.ErrorAccessMemory;
        END;
      ELSE
        error := msg.RangeError;
      END;
    ELSIF right.sort = Address THEN
      IF right.address <= Max_Card[l] THEN
        IF mem.Put(a, sys.ADR(right.address), l) THEN
          RETURN TRUE;
        ELSE
          error := msg.ErrorAccessMemory;
        END;
      ELSE
        error := msg.RangeError;
      END;
    ELSIF (right.sort = Variable) THEN
      IF tls.TypesCompatible(t, right.var_type) THEN
        buff := 0;
        IF NOT mem.Get(right.location, sys.ADR(buff), tls.TypeSize(right.var_type)) THEN
          error := msg.ErrorAccessMemory;
        END;
        IF buff <= Max_Card[l] THEN
          IF mem.Put(a, sys.ADR(buff), l) THEN
            RETURN TRUE;
          ELSE
            error := msg.ErrorAccessMemory;
          END;
        ELSE
          error := msg.RangeError;
        END;
      END;
    ELSE
      error := msg.IncompatibleTypes;
    END;
  ELSE
    error := msg.IncompatibleTypes;
  END;
  RETURN FALSE;
END Assign;


(* Выдать полное имя объекта *)
PROCEDURE ObjectFullName (object: dt.OBJECT; VAR name: ARRAY OF CHAR);
VAR
  s  : xs.String;
  m  : xs.txt_ptr;
  com: dt.ComNo;
  mod: dt.ModNo;
BEGIN
  com := tls.ObjectCom(object);
  mod := tls.ObjectMod(object);
  COPY('', name);
  LOOP
    tls.ObjectName (object, s);
    xs.Insert (s, 0, name);
    object := tls.ObjectParentScope (object);
    IF tls.EqualObjects (object, dt.Invalid_Object) THEN
      EXIT;
    END;
    xs.Insert ('::', 0, name);
  END;
  IF tls.ModName (com, mod, m) THEN
    xs.Insert ('.', 0, name);
    xs.Insert (m^, 0, name);
  END;
END ObjectFullName;



PROCEDURE res2str (res: ExprRes; VAR str: ARRAY OF CHAR; string_len: CARDINAL);
CONST
  JavaStringClassName1 = "String`";
  JavaStringClassName2 = "java_lang_String_String`";
  JavaStringClassName3 = "xjRTS_X2J_STRING_STR";

VAR
  len    : CARDINAL;
  min,max: CARDINAL;
  l, k, i: CARDINAL;
  a, b   : kt.ADDRESS;
  name   : xs.txt_ptr;
  nm, val: xs.String;
  bit    : BOOLEAN;
  tag    : dt.TYPE_TAG;
  savetag: dt.TYPE_TAG;
  subtype: dt.PTYPE;
  st     : xs.String;
  proc   : dt.OBJECT;
  com    : dt.ComNo;
  mod    : dt.ModNo;
  expr   : ExprRes;
  access : kt.ATTRIBS;
  lang   : dt.LANGUAGE;
  new_com: dt.ComNo;
  reg_val: kt.REG_VALUE;

 <* IF DEST_K26 THEN *>
  value  : LONGLONGREAL;
 <* ELSE*>
  buff   : ARRAY [0..15] OF sys.CARD8;
  preal  : POINTER TO REAL;
  plreal : POINTER TO LONGREAL;
 <* END *>


  PROCEDURE PrintAddress (addr: kt.ADDRESS);
  BEGIN
    IF addr = 0 THEN
      CASE lang OF
      | dt.Lng_Java, dt.Lng_C, dt.Lng_CPP:
        COPY('null', str);
      ELSE
        COPY('NIL', str);
      END;
    ELSE
      fmt.print(str, Fmt_ADDRval, addr);
    END;
  END PrintAddress;

  PROCEDURE PrintProcedure (addr: kt.ADDRESS);
  VAR new_com: dt.ComNo;
      com    : dt.ComNo;
      mod    : dt.ModNo;
      proc   : dt.OBJECT;
  BEGIN
  <* IF TARGET_x86 THEN *>
    IF addr # NIL_ADDR THEN
      b := dsm.IsJmpForDll (addr);
      IF b # 0 THEN
        addr := b;
      END;
    END;
  <* END *>

    IF tls.FindModByAddr (addr, com, mod) THEN
      proc := tls.FindLabelByAddr (com, mod, addr);
      IF tls.IsObjectValid (proc) THEN
        ASSERT (tls.ModName (com, mod, name));
        fmt.print (str, "%s.", name^);
        tls.ObjectName (proc, nm);
        fmt.print (st, "%s", nm);
        xs.Append (st, str);
        new_com := com;
        IF tls.FindComponentByAddr (res.location, com) AND (com # new_com) THEN
          xs.Insert (".", 0, str);
          ASSERT (tls.ComName (new_com, name));
          xs.Insert (name^, 0, str);
        END;
        RETURN;
      END;
    END;
    PrintAddress(addr);
  END PrintProcedure;


  PROCEDURE PrintReal (real: LONGLONGREAL);
  BEGIN
    --r2s.to_any (real, 8, 17, str);
    r2s.to_float(real, 17, 1, 17, 'e', FALSE, FALSE, str);
  END PrintReal;


  PROCEDURE GetMem (source: kt.ADDRESS; dest: sys.ADDRESS; len: CARDINAL): BOOLEAN;
  BEGIN
    IF mem.Get(source, dest, len) THEN
      RETURN TRUE;
    ELSE
      COPY('???', str);
      RETURN FALSE;
    END;
  END GetMem;

VAR
  field: dt.TYPE_RECORD_FIELD;

BEGIN
  IF NOT Var2Value (res, res) THEN
    COPY ('???', str);
    RETURN;
  END;
  str[0] := 0C;
  lang := dt.Lng_Unknown;
  CASE res.sort OF
  | CARDval, INTval, WHOLEval:
    IF Fmt_Special = '' THEN
      IF res.type = dt.st_original THEN
        IF opt.WholeHex THEN
          res.type := dt.st_hex;
        ELSE
          CASE res.sort OF
          | CARDval, WHOLEval:
            res.type := dt.st_unsigned;
          | INTval:
            res.type := dt.st_signed;
          END;
        END;
      END;
      fmt.print(str, dt.Types[res.type].fmt, res.value);
    ELSE
      fmt.print(str, Fmt_Special, res.value);
    END;

  | REALval:
   <* IF DEST_K26 THEN *>
    IF ConvertReal2Host (res, value) THEN
      PrintReal(value);
    ELSE
      COPY("Ошибка при конвертации", str);
    END;
   <* ELSE *>
    PrintReal(res.r_val);
   <* END *>
  | CHARval:
    IF ORD(res.ch) < 32 THEN
      fmt.print(str, Fmt_CTRL_CHAR, ORD(res.ch));
    ELSE
      fmt.print(str, Fmt_CHARval, res.ch);
    END;
  | BOOLval:
    IF res.b_val THEN
      COPY('TRUE', str);
    ELSE
      COPY('FALSE', str);
    END;
  | STRINGval:
    fmt.print(str, Fmt_STRINGval, res.string);
  | Address:
    PrintAddress(res.address);
  | Reference:
    PrintAddress(res.reference);
  | Register:
    IF (res.reg_type # dt.Invalid_Type) THEN
      -- get type_tag
      ASSERT (tls.TypeTag (res.reg_type, tag));
      CASE tag OF
      | dt.Procedure:
        ASSERT (mem.GetReg (res.reg_no, reg_val));
        PrintProcedure (reg_val);
      | dt.Enum:
        ASSERT (mem.GetReg (res.reg_no, reg_val));
        tls.EnumName (res.var_type, reg_val, str);
      ELSE
        COPY('IF THIS MESSAGE DISPLAYED SOME INTERNAL DEBUGGER ERROR WAS DETECTED!', str);
      END;
    ELSE
      ASSERT( exe.GetRegName(res.reg_no, nm) );
      fmt.print(str, Fmt_Register, nm);
    END;
  | Variable:
    IF  tls.IsTypePrimitive(res.var_type)  THEN
      lang := dt.Lng_Unknown;
    ELSE
      lang := tls.ModLanguage (tls.TypeCom(res.var_type), tls.TypeMod(res.var_type));
    END;
    ASSERT(tls.TypeTag(res.var_type, tag));
    CASE tag OF
    | dt.T_Void:
      l := 0;
      IF GetMem (res.location, sys.ADR(l), 1) THEN
        fmt.print (str, "0x%$2X", l);
      END;
    | dt.Char:
      -- unicode charecter
      len := tls.TypeSize (res.var_type);
      -- если длина 1, то такая переменная должна была
      -- быть ранее преобразована Var_Value в выражение
      ASSERT(len=2);
      l := 0;
      IF GetMem (res.location, sys.ADR(l), 2) THEN
        IF (31 < l) AND (l < 128) THEN
          res.sort := CHARval;
          res.ch := CHAR(l);
          Res2Str (res, str);
        ELSIF lang = dt.Lng_Java THEN
          fmt.print (str, "\u%$4X", l);
        ELSE
          fmt.print (str, "%2oC,%2oC", l DIV 100H, l MOD 100H);
        END;
      END;

    | dt.Complex:
      l := tls.TypeSize (res.var_type) DIV 2;
      expr.sort := REALval;
     <* IF DEST_K26 THEN *>
      CASE l OF
      | 4 : expr.r_type  := f;
            ASSERT (l = SIZE(expr.r_val_f));
            IF NOT GetMem (res.location, sys.ADR(expr.r_val_f), l) THEN
              RETURN;
            END;
      | 8 : -- только тип D, поскольку COMPLEX может получиться только из Modula-2
            expr.r_type  := d;
            ASSERT (l = SIZE(expr.r_val_d));
            IF NOT GetMem (res.location, sys.ADR(expr.r_val_d), l) THEN
              RETURN;
            END;
      END;
     <* ELSE *>
      preal  := sys.ADR(buff);
      plreal := sys.ADR(buff);
      IF NOT GetMem (res.location, sys.ADR(buff), l) THEN
        RETURN;
      END;
      CASE l OF
      | 4 : expr.r_val := VAL(LONGLONGREAL, preal^);
      | 8 : expr.r_val := VAL(LONGLONGREAL, plreal^);
      END;
     <* END *>
      Res2Str(expr, st);
      fmt.print(str, "(%s,", st);
     <* PUSH *>
     <* WOFF902+ *>
     <* IF DEST_K26 THEN *>
      CASE l OF
      | 4 : IF NOT GetMem (res.location + l, sys.ADR(expr.r_val_f), l) THEN
              RETURN;
            END;
      | 8 : IF NOT GetMem (res.location + l, sys.ADR(expr.r_val_d), l) THEN
              RETURN;
            END;
      END;
     <* ELSE *>
      IF NOT GetMem (res.location + l, sys.ADR(buff), l) THEN RETURN; END;
      CASE l OF
      | 4 : expr.r_val := VAL(LONGLONGREAL, preal^);
      | 8 : expr.r_val := VAL(LONGLONGREAL, plreal^);
      END;
     <* END *>
     <* POP *>
      Res2Str(expr, st);
      xs.Append(st, str);
      xs.Append(')', str);

    | dt.Array:
      ASSERT(tls.TypeName(res.var_type, name));
      IF name^ = '' THEN
        fmt.print(str, "ARRAY");
      ELSE
        fmt.print(str, "%s", name^);
      END;
      tls.SubType(res.var_type, expr.var_type);
      ASSERT(tls.TypeTag(expr.var_type, tag));
      IF (tag = dt.Char) OR (res.type = dt.st_inside) THEN
        savetag := tag;
        tls.ArrayIndexType (res.var_type, subtype);
        ASSERT(tls.TypeTag(subtype, tag));
        WHILE tag = dt.Range DO
          tls.SubType(subtype, subtype);
          ASSERT(tls.TypeTag(subtype, tag));
        END;
        IF (tag=dt.Int) OR (tag=dt.Card) OR (tag=dt.Byte) OR (tag=dt.Enum) OR (tag=dt.Char) OR (tag=dt.Boolean) THEN
          l := tls.TypeLen (res.var_type);
          len := tls.TypeSize (expr.var_type);
          expr.sort := Variable;
          expr.type := dt.st_original;
          IF (savetag = dt.Char) AND (res.type # dt.st_inside) THEN
            st := "";
            COPY('%c', Fmt_CHARval);
            COPY('%c', Fmt_CTRL_CHAR);
          ELSE
            COPY(str, st);
            xs.Append ('(', st);
          END;
          i := 0;
          LOOP
            IF (i = l) OR (LENGTH(st) >= HIGH(st)) THEN EXIT; END;
            expr.location := res.location + i*len;
            Res2Str (expr, str);
            IF str = "???" THEN
              EXIT;
            END;
            xs.Append (str, st);
            IF (savetag = dt.Char) AND (res.type # dt.st_inside) THEN
              IF str = '' THEN
                EXIT;
              END;
            ELSE
              xs.Append (',', st);
            END;
            INC(i);
          END;
          SetDefaultFormat;
          IF (savetag = dt.Char) AND (res.type # dt.st_inside) THEN
            fmt.print(str, Fmt_STRINGval, st);
          ELSE
            len := LENGTH(st);
            IF st[len-1] = ',' THEN
              st [len-1] := '';
            END;
            xs.Append (')', st);
            COPY(st, str);
          END;
        END;
      END;

    | dt.Array_of:
      ASSERT(tls.TypeName(res.var_type, name));
      IF name^ = '' THEN
        fmt.print(str, "DYNAMIC ARRAY");
      ELSE
        COPY(name^, nm);
        fmt.print(str, "%s", nm);
      END;
      tls.SubType(res.var_type, expr.var_type);
      ASSERT(tls.TypeTag(expr.var_type, tag));
      IF (string_len <> MAX(CARDINAL)) OR ArrayOf_Len (res, string_len) THEN
        l := string_len;
        IF (tag = dt.Char) OR (res.type = dt.st_inside) THEN
          savetag := tag;
          len := tls.TypeSize (expr.var_type);
          IF l > HIGH(str) THEN l := HIGH(str); END;
          expr.sort := Variable;
          expr.type := dt.st_original;
          IF (savetag = dt.Char) AND (res.type # dt.st_inside) THEN
            st := "";
            COPY('%c', Fmt_CHARval);
            COPY('%c', Fmt_CTRL_CHAR);
          ELSE
            COPY(str, st);
            xs.Append ('(', st);
          END;
          i := 0;
          LOOP
            IF (i = l) OR (LENGTH(st) >= HIGH(st)) THEN EXIT; END;
            expr.location := res.location + i*len;
            Res2Str (expr, str);
            IF str = "???" THEN
              EXIT;
            END;
            xs.Append (str, st);
            IF (savetag = dt.Char) AND (res.type # dt.st_inside) THEN
              IF str = '' THEN
                EXIT;
              END;
            ELSE
              xs.Append (',', st);
            END;
            INC(i);
          END;
          SetDefaultFormat;
          IF (savetag = dt.Char) AND (res.type # dt.st_inside) THEN
            fmt.print(str, Fmt_STRINGval, st);
          ELSE
            len := LENGTH(st);
            IF st[len-1] = ',' THEN
              st [len-1] := '';
            END;
            xs.Append (')', st);
            COPY(st, str);
          END;
        END;
      END;

    | dt.OpenArray:
      ASSERT(tls.TypeName(res.var_type, name));
      IF name^ = '' THEN
        fmt.print(str, "OPEN ARRAY");
      ELSE
        COPY(name^, nm);
        fmt.print(str, "%s", nm);
      END;
      tls.SubType(res.var_type, expr.var_type);
      ASSERT(tls.TypeTag(expr.var_type, tag));
      IF OpenArray_Len (res, l) THEN
        IF (tag = dt.Char) OR (res.type = dt.st_inside) THEN
          savetag := tag;
          len := tls.TypeSize (expr.var_type);
          IF l > HIGH(str) THEN l := HIGH(str); END;
          expr.sort := Variable;
          expr.type := dt.st_original;
          IF (savetag = dt.Char) AND (res.type # dt.st_inside) THEN
            st := "";
            COPY('%c', Fmt_CHARval);
            COPY('%c', Fmt_CTRL_CHAR);
          ELSE
            COPY(str, st);
            xs.Append ('(', st);
          END;
          i := 0;
          LOOP
            IF (i = l) OR (LENGTH(st) >= HIGH(st)) THEN EXIT; END;
            expr.location := res.location + i*len;
            Res2Str (expr, str);
            xs.Append (str, st);
            IF (savetag = dt.Char) AND (res.type # dt.st_inside) THEN
              IF str = '' THEN EXIT; END;
            ELSE
              xs.Append (',', st);
            END;
            INC(i);
          END;
          SetDefaultFormat;
          IF (savetag = dt.Char) AND (res.type # dt.st_inside) THEN
            fmt.print(str, Fmt_STRINGval, st);
          ELSE
            len := LENGTH(st);
            IF st[len-1] = ',' THEN
              st [len-1] := '';
            END;
            xs.Append (')', st);
            COPY(st, str);
          END;
        END;
      END;

    | dt.Record:
      ASSERT(tls.TypeName(res.var_type, name));
      IF name^ = '' THEN
        fmt.print (str, "RECORD");
      ELSE
        COPY(name^, nm);
        fmt.print (str, "%s", nm);
        IF (name^ = JavaStringClassName3)
        THEN
          tls.Field (res.var_type, 3, field); -- "count" field
          b := res.location+field.FieldOffs;  -- save the address of "count" field
          tls.Field (res.var_type, 2, field); -- "offset" field
          a := res.location+field.FieldOffs;  -- save the address of "offset" field
          tls.Field (res.var_type, 1, field); -- "value" field
          res.var_type := field.FieldType;
          res.arr_desc := res.location;
          res.location := res.location+field.FieldOffs;
          res.type := dt.st_dereference;
          IF mem.Get (b, sys.ADR(string_len), 4)
            AND mem.Get (a, sys.ADR(len), 4)
            AND Dereference (res, res)
          THEN
            res.location := res.location+2*len;
            res2str (res, st, string_len);
            xs.Append (" ", str);
            xs.Append (st, str);
          END;
        END;
      END;

    | dt.Class:
      ASSERT(tls.TypeName(res.var_type, name));
      IF name^ = '' THEN
        fmt.print (str, "CLASS");
      ELSE
        fmt.print (str, "%s", name^);
        IF (name^ = JavaStringClassName1)
          OR (name^ = JavaStringClassName2)
        THEN
          tls.Field (res.var_type, 3, field); -- "count" field
          b := res.location+field.FieldOffs;  -- save the address of "count" field
          tls.Field (res.var_type, 2, field); -- "offset" field
          a := res.location+field.FieldOffs;  -- save the address of "offset" field
          tls.Field (res.var_type, 1, field); -- "value" field
          res.var_type := field.FieldType;
          res.arr_desc := res.location;
          res.location := res.location+field.FieldOffs;
          res.type := dt.st_dereference;
          IF mem.Get (b, sys.ADR(string_len), 4)
            AND mem.Get (a, sys.ADR(len), 4)
            AND Dereference (res, res)
          THEN
            res.location := res.location+2*len;
            res2str (res, st, string_len);
            xs.Append (" ", str);
            xs.Append (st, str);
          END;
        END;
      END;

    | dt.Pointer:
      a := 0;
      IF NOT GetMem (res.location, sys.ADR(a), 4) THEN RETURN; END;
      tls.SubType(res.var_type, subtype);
      ASSERT(tls.TypeTag(subtype, tag));
      IF tag = dt.Array_of THEN
        CASE lang OF
        | dt.Lng_M2, dt.Lng_O2:
          -- переменная типа POINTER TO ARRAY OF ... ссылается
          -- на дескриптор xmRTS.X2C_ARRSTR, который имеет первое
          -- поле, указывающие на собственно сам массив
          IF mem.GetSegmentInfo (a, b, l, access) AND
             mem.Get(a, sys.ADR(b), 4) AND
             mem.GetSegmentInfo (b, len, l, access)
          THEN
            a := b;
          END;
       <* IF DEST_XDS THEN *>
        | dt.Lng_Java:
          -- здесь тело массива просто смещено относительно
          -- переменной на xi.BODY_OFFS
          IF mem.Get (a, sys.ADR(b), 4) AND (a <= MAX(CARDINAL) - xi.BODY_OFFS) THEN
            a := a + xi.BODY_OFFS;
          ELSE
            PrintAddress (a);
            IF (a # 0) AND (res.type = dt.st_dereference)  THEN
              xs.Append (' -> ???', str);
            END;
            RETURN;
          END;
       <* END *>
        ELSE
        END;
      ELSIF tag = dt.Procedure THEN
        res.var_type := subtype;
        Res2Str(res, str);
        RETURN;
      END;
      PrintAddress(a);
      IF res.type = dt.st_dereference THEN
        IF Dereference (res, res) THEN
          Res2Str(res, st);
        ELSE
          COPY('???', st);
        END;
        xs.Append (' -> ', str);
        xs.Append (st, str);
      ELSE
        ASSERT(res.type = dt.st_original);
      END;

    | dt.Enum:
      l := 0;
      IF NOT GetMem (res.location, sys.ADR(l), tls.TypeSize (res.var_type)) THEN RETURN; END;
      tls.EnumName (res.var_type, l, str);

    | dt.Procedure:
      a := 0;
      IF NOT GetMem (res.location, sys.ADR(a), 4) THEN
        RETURN;
      END;

     <* IF TARGET_x86 THEN *>
      IF a # NIL_ADDR THEN
        b := dsm.IsJmpForDll (a);
        IF b # 0 THEN
          a := b;
        END;
      END;
     <* END *>

      IF tls.FindModByAddr (a, com, mod) THEN
        proc := tls.FindLabelByAddr (com, mod, a);
        IF tls.IsObjectValid (proc) THEN
          ASSERT (tls.ModName (com, mod, name));
          fmt.print (str, "%s.", name^);
          tls.ObjectName (proc, nm);
          fmt.print (st, "%s", nm);
          xs.Append (st, str);
          new_com := com;
          IF tls.FindComponentByAddr (res.location, com) AND (com # new_com) THEN
            xs.Insert (".", 0, str);
            ASSERT (tls.ComName (new_com, name));
            xs.Insert (name^, 0, str);
          END;
          RETURN;
        END;
      END;
      PrintAddress(a);

    | dt.Set:
      ASSERT(tls.TypeName(res.var_type,name));
      tls.SubType(res.var_type, subtype);
      IF name^ = '' THEN
        ASSERT(tls.TypeName(subtype,name));
        IF name^ = '' THEN
          COPY("SET{", str);
        ELSE
          COPY(name^, nm);
          fmt.print(str, "SET OF %s{", nm);
        END;
      ELSE
        COPY(name^, nm);
        fmt.print(str,"%s{", nm);
      END;
      tls.Index(res.var_type, min, max);
      ASSERT(tls.TypeTag(subtype, tag));
      IF tag = dt.Range THEN
        tls.SubType(subtype, expr.var_type);
        ASSERT( tls.TypeTag(expr.var_type, tag) );
      END;
      IF tag = dt.Int THEN
        len := INTEGER(max)-INTEGER(min)+1;
      ELSE
        len := max-min+1;
      END;
      FOR i := 1 TO len DO
        IF CheckBit(res, i-1, bit) AND bit THEN
          i2Str(subtype, i-1, val);
          fmt.append(str, '%s,', val);
        END;
      END;
      len := LENGTH(str);
      IF str[len-1] = ',' THEN
        str [len-1] := '}';
      ELSE
        xs.Append('}', str);
      END;

    | dt.Int, dt.Card:
      len := tls.TypeSize (res.var_type);
      ASSERT(len=8);
      l := 0;
      k := 0;
      IF NOT GetMem (res.location, sys.ADR(l), 4) THEN
        RETURN;
      END;
      IF NOT GetMem (res.location+4, sys.ADR(k), 4) THEN
        RETURN;
      END;
      IF (res.type = dt.st_original) AND opt.WholeHex THEN
        res.type := dt.st_hex;
      END;
      CASE res.type OF
      | dt.st_original:
        i2s.IntToStr (tag = dt.Int, k, l, str);
      | dt.st_unsigned:
        i2s.IntToStr (FALSE, k, l, str);
      | dt.st_signed:
        i2s.IntToStr (TRUE, k, l, str);
      | dt.st_hex:
        IF k = 0 THEN
          fmt.print(str, dt.Types[res.type].fmt, l);
        ELSE
          fmt.print(str, "0%X%$8XH", k, l);
        END;
      | dt.st_bin:
        IF k = 0 THEN
          fmt.print(str, dt.Types[res.type].fmt, l);
        ELSE
          fmt.print(str, "0%b%$32bI", k, l);
        END;
      ELSE
        COPY("???", str);
      END;

    ELSE
      COPY('IF THIS MESSAGE DISPLAYED SOME INTERNAL DEBUGGER ERROR WAS DETECTED!', str);
    END;
  END;
EXCEPT
  COPY ("???", str);
  RETURN;
END res2str;


PROCEDURE Res2Str (res: ExprRes; VAR str: ARRAY OF CHAR);
BEGIN
  res2str (res, str, MAX(CARDINAL)); -- do not use length of "array of char"
END Res2Str;


(* Печать i-го элемента типа, если этот тип может быть перечислен *)
PROCEDURE i2Str (type: dt.PTYPE; i: LONGCARD; VAR str: ARRAY OF CHAR);
VAR
  tag: dt.TYPE_TAG;
  res: ExprRes;
  sub: dt.PTYPE;
  l  ,
  min,
  max: CARDINAL;

BEGIN
  ASSERT( tls.TypeTag(type, tag) );
  CASE tag OF
  | dt.Byte,
    dt.Card:
    tls.Index (type, min, max);
    ASSERT(i<=max);
    WITH res DO
      sort  := CARDval;
      type  := dt.st_original;
      value := i;
    END;

  | dt.Char:
    tls.Index (type, min, max);
    ASSERT(i<=max);
    WITH res DO
      sort := CHARval;
      ch := VAL(CHAR, i);
    END;

  | dt.Boolean:
    tls.Index (type, min, max);
    ASSERT(i<=max);
    WITH res DO
      sort := BOOLval;
      b_val := VAL(BOOLEAN, i);
    END;

  | dt.Int:
    l := tls.TypeSize (type);
    WITH res DO
      sort := INTval;
      type := dt.st_original;
      IF i <= MAX(LONGCARD) DIV 2 THEN
        value := LONGCARD(VAL(LONGINT, Min_Int[l]+VAL(LONGINT,i)));
      ELSE
        value := i - VAL(LONGCARD, MAX(LONGINT));
      END;
    END;

  | dt.Range:
    tls.Index (type, min, max);
    tls.SubType(type, sub);
    ASSERT( tls.TypeTag(sub, tag) );
    WITH res DO
      CASE tag OF
      | dt.Byte:    sort  := WHOLEval;
                    type  := dt.st_original;
                    ASSERT(i<=max);
                    value := min + i;
      | dt.Card:    sort  := CARDval;
                    type := dt.st_original;
                    ASSERT(i<=max);
                    value := min + i;
      | dt.Char:    sort  := CHARval;
                    ASSERT(i<=max);
                    ch    := VAL(CHAR,min + i);
      | dt.Boolean: sort  := BOOLval;
                    ASSERT(i<=max);
                    b_val := VAL(BOOLEAN, i);
      | dt.Int :    sort  := INTval;
                    type := dt.st_original;
                    value := LONGCARD(LONGINT(min) + LONGINT(i));
      | dt.Enum:    ASSERT(i<=max);
                    tls.EnumName (sub, min+i, str);
                    RETURN;
      ELSE
        ASSERT(FALSE);
      END;
    END;

  | dt.Enum:
    tls.EnumName (type, i, str);
    RETURN;

  ELSE
    COPY('???', str);
    RETURN;
  END;
  Res2Str(res,str);
END i2Str;

PROCEDURE CompareRes (res1, res2: ExprRes): BOOLEAN;

 <* IF DEST_K26 THEN *>
  PROCEDURE compare_floats (a1, a2: ARRAY OF SHORTCARD): BOOLEAN;
  VAR
    i: CARDINAL;
  BEGIN
    ASSERT(HIGH(a1)=HIGH(a2));
    FOR i := 0 TO HIGH(a1) DO
      IF a1[i] # a2[i] THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END compare_floats;
 <* END *>

BEGIN
  IF res1.sort # res2.sort THEN RETURN FALSE; END;
  CASE res1.sort OF
  | INTval, CARDval, WHOLEval:
    RETURN res1.value = res2.value;
  | REALval:
   <* IF DEST_K26 THEN *>
    IF res1.r_type = res2.r_type THEN
      CASE res1.r_type OF
      | PC_const:
        RETURN res1.r_val_const = res2.r_val_const;
      | f:
        RETURN compare_floats (res1.r_val_f, res2.r_val_f); --typ.BoardFloatF;
      | g:
        RETURN compare_floats (res1.r_val_g, res2.r_val_g); --typ.BoardFloatG;
      | d:
        RETURN compare_floats (res1.r_val_d, res2.r_val_d); --typ.BoardFloatD;
      END;
    ELSE
      RETURN FALSE;
    END;
   <* ELSE *>
    RETURN res1.r_val = res2.r_val;
   <* END  *>
  | BOOLval:
    RETURN res1.b_val = res2.b_val;
  | CHARval:
    RETURN res1.ch = res2.ch;
  | Address:
    RETURN res1.address = res2.address;
  | Register:
    RETURN res1.reg_no = res2.reg_no;
  | Variable:
    RETURN (res1.location     = res2.location)     AND
           (res1.var_type     = res2.var_type);
  | Reference:
    RETURN (res1.reference    = res2.reference)    AND
           (res1.ref_type     = res2.ref_type);
  END;
EXCEPT
  RETURN FALSE;
END CompareRes;

VAR
  LocalLongConstBuffer: LONGCONST;

PROCEDURE GetSource (com: dt.ComNo; mod: dt.ModNo; st-: ARRAY OF CHAR;
                     VAR LC: LONGCONST;
                     VAR Source: ExprRes) : BOOLEAN;

  PROCEDURE StrToImagePC () : BOOLEAN;
  TYPE
    PBYTE = POINTER TO sys.CARD8;

  VAR
    i : LONGCARD;
    b    , (* База *)
    bits : CARDINAL;  (* Кол-во бит на символ *)
    length : CARDINAL; (* Длина константа в памяти *)
    ln : CARDINAL; (* Кол-во символов на целой число байт *)
    ln2, k, cnt: CARDINAL;
    tmp : xs.String;
    ok : BOOLEAN;
    p : PBYTE;
    flag : BOOLEAN;
    schar : ARRAY [0..0] OF CHAR;
  BEGIN
    ExprStr := sys.ADR(st);
    char := ExprStr^[0];
    IF char = EL THEN RETURN FALSE; END;
    next := ExprStr^[1];
    pos := 2;
    CASE char OF
    | '0'..'9'    : GetToken;
                    IF Token = const_val THEN
                      IF TmpRes.sort IN WHOLES THEN
                        WITH LocalLongConstBuffer DO
                          len := 4;
                          sys.MOVE (sys.ADR(TmpRes.value), sys.ADR(ptr^), len);
                          WHILE (ptr^[len-1] = 0) AND (len > 1) DO
                            DEC (len);
                          END;
                        END;
                      ELSE
                        Error(msg.IncompatibleTypes);
                      END;
                    ELSE
                      Error(msg.IncompatibleTypes);
                    END;
    | 'X','B','O' : (* Вычисляем длину константы - кол-во байт, необходимое для нее *)
                    IF next = "'" THEN
                      CASE char OF
                      | 'X' : ln := 2; b := 16;
                      | 'O' : ln := 8; b := 8;
                      | 'B' : ln := 8; b := 2;
                      END;
                      str.FindNext("'",st,2,ok,pos);
                      IF NOT ok OR (pos = 2) THEN Error(msg.EmptyIntConstant); END;
                      length := 0;
                      cnt := 0;
                      LOOP
                        k := pos-ln*cnt-1;
                        CASE b OF
                        | 2 : IF NOT (st[k] IN CHARSET{'0'..'1'}) THEN EXIT; END;
                        | 8 : IF NOT (st[k] IN CHARSET{'0'..'7'}) THEN EXIT; END;
                        | 16: IF NOT (st[k] IN CHARSET{'0'..'9', 'A'..'F'}) THEN EXIT; END;
                        END;
                        ln2 := 0;
                        tmp := '';

                        CASE b OF
                        | 2 : flag := (st[k] IN CHARSET{'0'..'1'});
                        | 8 : flag := (st[k] IN CHARSET{'0'..'7'});
                        | 16: flag := (st[k] IN CHARSET{'0'..'9', 'A'..'F'});
                        END;
                        WHILE (ln2 < ln) AND flag DO
                          schar[0] := st[k-ln2];
                          xs.Insert(schar,0,tmp);
                          INC(ln2);
                          CASE b OF
                          | 2 : flag := (st[k-ln2] IN CHARSET{'0'..'1'});
                          | 8 : flag := (st[k-ln2] IN CHARSET{'0'..'7'});
                          | 16: flag := (st[k-ln2] IN CHARSET{'0'..'9', 'A'..'F'});
                          END;
                        END;
<* WOFF313+ *>
<* WOFF903+ *>
                        i := xs.StrToCard (tmp, b, ok);
<* WOFF313- *>
<* WOFF903- *>
                        IF NOT ok THEN Error(msg.IncorrectIntConstant); END;
                        IF (b=2) OR (b=16) THEN
                          INC(length);
                        ELSE
                          IF st[k-ln2] = "'" THEN
                            bits := ln2*3;        (* Кол-во бит *)
                            CASE st[k-ln2+1] OF
                            | '0'..'1' : DEC(bits,2);
                            | '2'..'3' : DEC(bits,1);
                            | '4'..'7' :
                            END;
                            INC(length,(bits+7) DIV 8);
                          ELSE
                            INC(length,3);
                          END;
                        END;
                        INC(cnt);
                        IF pos <= ln*cnt+1 THEN EXIT; END;
                      END;
                      IF length = 0 THEN Error(msg.IncorrectIntConstant); END;

                      (* Размещаем константу *)
                      LocalLongConstBuffer.len := length;
                      p := sys.ADR(i);
                      cnt := 0;
                      length := 0;
                      LOOP
                        k := pos-ln*cnt-1;

                        CASE b OF
                        | 2 : IF NOT (st[k] IN CHARSET{'0'..'1'}) THEN EXIT; END;
                        | 8 : IF NOT (st[k] IN CHARSET{'0'..'7'}) THEN EXIT; END;
                        | 16: IF NOT (st[k] IN CHARSET{'0'..'9', 'A'..'F'}) THEN EXIT; END;
                        END;
                        ln2 := 0;
                        tmp := '';

                        CASE b OF
                        | 2 : flag := (st[k] IN CHARSET{'0'..'1'});
                        | 8 : flag := (st[k] IN CHARSET{'0'..'7'});
                        | 16: flag := (st[k] IN CHARSET{'0'..'9', 'A'..'F'});
                        END;
                        WHILE (ln2 < ln) AND flag DO
                          schar[0] := st[k-ln2];
                          xs.Insert(schar,0,tmp);
                          INC(ln2);
                          CASE b OF
                          | 2 : flag := (st[k-ln2] IN CHARSET{'0'..'1'});
                          | 8 : flag := (st[k-ln2] IN CHARSET{'0'..'7'});
                          | 16: flag := (st[k-ln2] IN CHARSET{'0'..'9', 'A'..'F'});
                          END;
                        END;
                        i := xs.StrToCard (tmp, b, ok);
                        IF NOT ok THEN Error(msg.IncorrectIntConstant); END;
                        IF (b=2) OR (b=16) THEN
                          LocalLongConstBuffer.ptr^[length] := p^;
                          INC(length);
                        ELSE
                          sys.MOVE(p,sys.ADR(LocalLongConstBuffer.ptr^[length]),3);
                          IF st[k-ln2] = "'" THEN
                            bits := ln2*3;        (* Кол-во бит *)
                            CASE st[k-ln2+1] OF
                            | '0'..'1' : DEC(bits,2);
                            | '2'..'3' : DEC(bits,1);
                            | '4'..'7' :
                            END;
                            INC(length,(bits+7) DIV 8);
                          ELSE
                            INC(length,3);
                          END;
                        END;
                        INC(cnt);
                        IF pos <= ln*cnt+1 THEN EXIT; END;
                      END;
                    ELSE
                      Error(msg.IncorrectIntConstant);
                    END;
    ELSE
      Error(msg.ExpectedIntConstant);
    END;
    RETURN TRUE;
  EXCEPT
    IF exc.IsCurrentSource(source) THEN RETURN FALSE; END;
  END StrToImagePC;


VAR
  tag: dt.TYPE_TAG;

BEGIN
  WITH LC DO ptr := NIL; len := 0; END;
  IF StrToImagePC() THEN
    LC := LocalLongConstBuffer;
    RETURN TRUE;
  ELSE
    CalcExpr (com, mod, st, Source);
    IF (error = 0) AND dfn THEN
      IF Source.sort = REALval THEN
        WITH LocalLongConstBuffer DO
         <* IF DEST_K26 THEN *>
          CASE Source.r_type OF
          | PC_const:
            len := SIZE(Source.r_val_const);
            sys.MOVE (sys.ADR(Source.r_val_const), sys.ADR(ptr^), len);
          | f:
            len := SIZE(Source.r_val_f);
            sys.MOVE (sys.ADR(Source.r_val_f), sys.ADR(ptr^), len);
          | g:
            len := SIZE(Source.r_val_const);
            sys.MOVE (sys.ADR(Source.r_val_g), sys.ADR(ptr^), len);
          | d:
            len := SIZE(Source.r_val_const);
            sys.MOVE (sys.ADR(Source.r_val_d), sys.ADR(ptr^), len);
          END;
         <* ELSE *>
          len := SIZE(Source.r_val);
          sys.MOVE (sys.ADR(Source.r_val), sys.ADR(ptr^), len);
         <* END *>
        END;
        LC := LocalLongConstBuffer;
      ELSIF Source.sort = Reference THEN
        Source.sort    := Address;
        Source.type := dt.st_original;
        Source.address := Source.reference;
      ELSIF Source.sort = Variable THEN
        ASSERT (tls.TypeTag(Source.var_type, tag));
        IF tag = dt.Procedure THEN
          Source.sort    := Address;
          Source.type := dt.st_original;
          Source.address := Source.reference;
        END;
      END;
      IF Source.sort IN SORTS{REALval, Register, Address} THEN
        RETURN TRUE;
      END;
    END;
  END;
  error := msg.CannotConvert2AddrReg;
  RETURN FALSE;
END GetSource;


PROCEDURE Dereference (arg: ExprRes; VAR res: ExprRes): BOOLEAN;
BEGIN
  Unar(bar,arg,res);
  RETURN TRUE;
EXCEPT
  IF exc.IsCurrentSource(source) THEN
    error := exc.CurrentNumber(source);
    IF error = 0 THEN
      dfn := FALSE;
      error := msg.Undefined_Expression;
    END;
    RETURN FALSE;
  END;
END Dereference;


PROCEDURE CheckName (st-: ARRAY OF CHAR; lat: BOOLEAN) : BOOLEAN;
BEGIN
  ExprStr := sys.ADR(st);
  char := ExprStr^[0];
  IF char = EL THEN
    Error(msg.ExpressionIsEmpty); -- Имя пустое
  ELSE
    next := ExprStr^[1];
  END;
  pos := 2;
  dfn := TRUE;
  error := 0;
  GetToken;
  IF NOT ((lat AND (Token = ident)) OR
         ((NOT lat) AND (Token = identrus))) THEN Error(msg.IncorrectExpression); END;
  GetToken;
  IF Token <> none THEN Error(msg.IncorrectExpression); END;
  RETURN TRUE;
EXCEPT
  IF exc.IsCurrentSource(source) THEN
    error := exc.CurrentNumber(source);
    IF error = 0 THEN
      dfn := FALSE;
      error := msg.Undefined_Expression;
    END;
    RETURN FALSE;
  END;
END CheckName;


(* Проверка Имени файла *)
PROCEDURE CheckFileName (st-:ARRAY OF CHAR; VAR ext:BOOLEAN) : BOOLEAN;
VAR
  p: CARDINAL;
BEGIN
  ext := FALSE;
  p := LENGTH(st);
  IF p > HIGH(st) THEN DEC(p); END;
  LOOP
    ext := st[p] = '.';
    IF ext OR (p = 0) THEN EXIT; END;
    DEC(p);
  END;
  RETURN TRUE;
END CheckFileName;


VAR
  i: CARDINAL;

BEGIN
  exc.AllocateSource(source);

  ASSERT(AddStdName('XOR',   STD_NAME_DESC{ std_token, xor }));
  ASSERT(AddStdName('OR',    STD_NAME_DESC{ std_token, or  }));
  ASSERT(AddStdName('AND',   STD_NAME_DESC{ std_token, and }));
  ASSERT(AddStdName('NOT',   STD_NAME_DESC{ std_token, not }));
  ASSERT(AddStdName('DIV',   STD_NAME_DESC{ std_token, div }));
  ASSERT(AddStdName('MOD',   STD_NAME_DESC{ std_token, mod }));
  ASSERT(AddStdName('REM',   STD_NAME_DESC{ std_token, rem }));

  ASSERT(AddStdName('TRUE',  STD_NAME_DESC{ std_const, ExprRes{ dt.st_original, BOOLval, TRUE }, FALSE }));
  ASSERT(AddStdName('FALSE', STD_NAME_DESC{ std_const, ExprRes{ dt.st_original, BOOLval, FALSE }, FALSE }));
  ASSERT(AddStdName('NIL',   STD_NAME_DESC{ std_const, ExprRes{ dt.st_original, Address, 0 }, FALSE } ));

  ASSERT(AddStdName('DEC'  , STD_NAME_DESC{ std_func, dec  } ));
  ASSERT(AddStdName('HEX'  , STD_NAME_DESC{ std_func, hex  } ));
  ASSERT(AddStdName('BIN'  , STD_NAME_DESC{ std_func, bin  } ));
  ASSERT(AddStdName('SDEC' , STD_NAME_DESC{ std_func, sdec } ));
  ASSERT(AddStdName('OCT'  , STD_NAME_DESC{ std_func, oct  } ));
  ASSERT(AddStdName('ADR'  , STD_NAME_DESC{ std_func, adr  } ));
  ASSERT(AddStdName('SIZE' , STD_NAME_DESC{ std_func, size } ));
  ASSERT(AddStdName('HIGH' , STD_NAME_DESC{ std_func, high } ));
  ASSERT(AddStdName('LOW'  , STD_NAME_DESC{ std_func, low  } ));
  ASSERT(AddStdName('TYPE' , STD_NAME_DESC{ std_func, type } ));
  ASSERT(AddStdName('PASS' , STD_NAME_DESC{ std_func, pass } ));
  ASSERT(AddStdName('ARG'  , STD_NAME_DESC{ std_func, arg  } ));

  FOR i := 0 TO kt.RegsNum - 1 DO
    ASSERT(AddStdName(kt.Registers[i].name, STD_NAME_DESC{ std_reg, kt.Registers[i].reg_no } ));
  END;

  SetDefaultFormat;

 <* IF DEST_K26 THEN *>
  zero := 0;
  flt.CVTLF(sys.ADR(zero), zero_f); ASSERT(NOT flt.Et);
  flt.CVTLD(sys.ADR(zero), zero_d); ASSERT(NOT flt.Et);
  flt.CVTLG(sys.ADR(zero), zero_g); ASSERT(NOT flt.Et);
 <* END *>

  NEW(LocalLongConstBuffer.ptr, 2096);

 <* IF DEST_XDS THEN *>
  ALLOCATE (TD, sys.FIELDOFS(xi.X2C_TD_STR.procs));
 <* END *>
FINALLY
  DISPOSE(LocalLongConstBuffer.ptr);
 <* IF DEST_XDS THEN *>
  DEALLOCATE (TD, sys.FIELDOFS(xi.X2C_TD_STR.procs));
 <* END *>
END Expr.
