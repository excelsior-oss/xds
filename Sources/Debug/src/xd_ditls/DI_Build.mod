<* Storage+ *>

<* NEW stats- *>

IMPLEMENTATION MODULE DI_Build;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;
IMPORT kt  := KrnTypes;

IMPORT xs  := xStr;
IMPORT sor := Sort;

IMPORT Ud:= Unidata;

IMPORT opt := Options;

FROM Printf  IMPORT printf;
FROM Strings IMPORT Equal;


<* IF DEFINED (stats) & stats THEN *>

VAR
  AddNameReNEW           : CARDINAL;
  AddNamesReNEW          : CARDINAL;
  AddTypeReNEWKeys       : CARDINAL;
  AddTypeReNEWNamedTypes : CARDINAL;
  AddObjectReNEW         : CARDINAL;
  AddPublicReNEW         : CARDINAL;
  AddEnumReNEW           : CARDINAL;
  Sy_Proc_No             : CARDINAL;
  Sy_Var_No              : CARDINAL;
  Sy_Reg_No              : CARDINAL;
  Sy_Rel_No              : CARDINAL;
  ProcedureLines         : CARDINAL;
  ReverseOrder           : CARDINAL;
  ProcedureExactlyOrder  : CARDINAL;
  ProcedureTotalLength   : CARDINAL;
<* END *>


CONST
  HN_Public    = 2048;           -- for component
  HN_KTypes    = 10000H DIV 9;   -- for module
  HN_Object    = 256;            -- for module
  HN_Enum      = 256;            -- for module

  HASH_SIZE_FOR_NAMES =  147401;--99733;--67139;
  SLOT_SIZE_FOR_NAMES =  20000H;

  SLOT_SIZE_FOR_TYPES = 80000H;


TYPE
  HASH_TABLE_SIZES = ARRAY dt.TYPE_TAG OF CARDINAL;

CONST
  HashTableSizes = HASH_TABLE_SIZES { 0              -- T_Void
                                    , 0              -- Byte
                                    , 0              -- Boolean
                                    , 0              -- Char
                                    , 0              -- Int
                                    , 0              -- Card
                                    , 0              -- Real
                                    , 0              -- Complex
                                    , 0              -- Address
                                      -- non-primitive types
                                    , 523            -- Range
                                    , 523-- 27127    -- Enum
                                    , 523            -- Pointer
                                    , 523            -- Reference
                                    , 27127-- 147401 -- Class
                                    , 523            -- Set
                                    , 27127          -- Procedure
                                    , 523            -- Array
                                    , 27127          -- Array_of
                                    , 27127          -- Record
                                    , 27127          -- OpenArray
                                    };


VAR
  anykey: dt.KEY;


(* Сортировка ключа, обмен *)
PROCEDURE Shake (i,j: CARDINAL);
VAR
  tmp: dt.KEY_INDEX;
BEGIN
  tmp := anykey^[i];
  anykey^[i] := anykey^[j];
  anykey^[j] := tmp;
END Shake;

 <* PUSH *>
 <* -COVERFLOW *>
  PROCEDURE name_hash(image: Ud.IMAGE): Ud.HASH_RESULT;
  VAR i,ind: CARDINAL;
      n,len: CARDINAL;
      str: xs.txt_ptr;
      res : Ud.HASH_RESULT;
  BEGIN
    ASSERT(image # NIL);
    str:= xs.txt_ptr(image);
    ind:= 0;
    len := LENGTH(str^);
    n:= len DIV 2;
    FOR i:= 0 TO n DO
      ind:= ind + 31*ORD(str^[i]) ;
    END;
   FOR i:= n TO len DO
     IF ind < MAX(CARDINAL)-i -i*ORD(str^[i]) THEN
      ind:= ind + i + i*ORD(str^[i]);
     ELSE
       ind:= ind MOD HASH_SIZE_FOR_NAMES;
     END;
    END;
    res.index  := ind MOD HASH_SIZE_FOR_NAMES;;
    res.number := 0;
    RETURN res;
  END name_hash;
 <* POP *>

PROCEDURE Length(image: Ud.IMAGE): CARDINAL;
VAR Pstr:  xs.txt_ptr;
BEGIN
  ASSERT( image # NIL);
  Pstr:= xs.txt_ptr(image);
  RETURN LENGTH(Pstr^);
END Length;

<*PUSH*>
<*WOFF301+*>
PROCEDURE IsEqual(im1: Ud.IMAGE; im2: Ud.IMAGE; len: CARDINAL): BOOLEAN;
<*POP*>
VAR
  str1,str2:  xs.txt_ptr;
BEGIN
    ASSERT( im1 # NIL);
    ASSERT( im2 # NIL);
    str1:= xs.txt_ptr(im1);
    str2:= xs.txt_ptr(im2);
    RETURN Equal(str1^, str2^);
END IsEqual;


(* Добавить имя в таблицу имен модуля *)
PROCEDURE AddName (name-: ARRAY OF CHAR): CARDINAL;
VAR
  unique: BOOLEAN;
BEGIN
  unique:= FALSE;
  RETURN CARDINAL(Ud.Add(Names, sys.ADR(name),  IsEqual, Length, unique));
END AddName;


(* Добавить новое имя в таблицу имен модуля *)
PROCEDURE AddNewName (name-: ARRAY OF CHAR): CARDINAL;
VAR
  unique: BOOLEAN;
BEGIN
  unique:= TRUE;
  RETURN CARDINAL(Ud.Add(Names, sys.ADR(name),  IsEqual, Length, unique));
END AddNewName;


(* Создание ключей для таблицы соответствия кода и текста *)
VAR
  cltable: dt.PACLTABLE;

<* PUSH *>
<* -CHECKINDEX  *>
<* -CHECKDINDEX *>
<* -CHECKRANGE  *>
<* -CHECKNIL    *>

PROCEDURE CompareTable (i,j: CARDINAL) : BOOLEAN;
BEGIN
  WITH cltable^[i] DO
    IF Line > cltable^[j].Line THEN    RETURN TRUE
    ELSIF Line = cltable^[j].Line THEN RETURN Addr > cltable^[j].Addr;
    ELSE                               RETURN FALSE;
    END;
  END;
END CompareTable;

PROCEDURE ShakeTable (i,j: CARDINAL);
VAR
  tmp: dt.CLTABLE;
BEGIN
  tmp := cltable^[i];
  cltable^[i] := cltable^[j];
  cltable^[j] := tmp;
END ShakeTable;

PROCEDURE CompareCodeTable (i,j: CARDINAL) : BOOLEAN;
VAR
  ik, jk: CARDINAL;
  ia, ja: kt.ADDRESS;
BEGIN
  ik := anykey^[i];
  ia := cltable^[ik].Addr;
  jk := anykey^[j];
  ja := cltable^[jk].Addr;
  RETURN ia > ja;
END CompareCodeTable;

<* POP *>

PROCEDURE CreateKCLTable (com: dt.ComNo; mod: dt.ModNo);
VAR
  N, i: CARDINAL;
BEGIN
  WITH dt.Components.Components^[com].DI.Modules^[mod].CLTable DO
    IF CLTable # NIL THEN
      cltable := CLTable;
      N := HIGH(CLTable^)+1;
      (* Сортировка таблицы: вначале сравнение по строкам, затем по адресам *)
      sor.Shell(N,CompareTable, ShakeTable);
      i := 0;
      LOOP
        IF CLTable^[i].Line = 0 THEN
          CLTable^[i].Line := 1;
        ELSIF CLTable^[i].Line > 0 THEN
          EXIT;
        END;
        INC (i);
        IF i = N THEN
          EXIT;
        END;
      END;
      (* Ключ по коду *)
      NEW(KCLTableAddr, N);
      FOR i := 0 TO N-1 DO
        KCLTableAddr^[i] := i;
      END;
      anykey := KCLTableAddr;
      sor.Shell(N, CompareCodeTable, Shake);
      (* Ключ по строкам *)
      NEW(KCLTableLine, CLTable^[N-1].Line);
      FOR i := 0 TO HIGH(KCLTableLine^) DO
        -- инициализация: любая строка не имеет кода
        KCLTableLine^[i] := dt.Invalid_Line;
      END;
      FOR i := N-1 TO 0 BY -1 DO
        -- для всех строк в таблице указываем ссылку на запись
        KCLTableLine^[CLTable^[i].Line-1] := i;
      END;
    END;
  END;
END CreateKCLTable;



(* Создание ключа для таблицы сегментов *)
VAR
  segments: dt.PASEGMENT;
  CurrCom : dt.ComNo;

PROCEDURE ShakeSegmentsInMod (i,j: CARDINAL);
VAR
  tmp: dt.SEGMENT;
BEGIN
  tmp := segments^[i];
  segments^[i] := segments^[j];
  segments^[j] := tmp;
END ShakeSegmentsInMod;

PROCEDURE CompareSegmentsInMod (i,j: CARDINAL) : BOOLEAN;
BEGIN
  RETURN segments^[i].Begin > segments^[j].Begin;
END CompareSegmentsInMod;

PROCEDURE ShakeSegments (i,j: CARDINAL);
VAR
  tmp: dt.GSEGMENT;
BEGIN
  WITH dt.Components.Components^[CurrCom] DO
    tmp := DI.KGSegments^[i];
    DI.KGSegments^[i] := DI.KGSegments^[j];
    DI.KGSegments^[j] := tmp;
  END;
END ShakeSegments;

PROCEDURE CompareSegments (i,j: CARDINAL) : BOOLEAN;
BEGIN
  WITH dt.Components.Components^[CurrCom] DO
    RETURN DI.KGSegments^[i].Begin > DI.KGSegments^[j].Begin;
  END;
END CompareSegments;


(* Создание ключа для сводной таблицы сегментов программы *)
PROCEDURE CreateKGroupSegments (com: dt.ComNo);
VAR
  N_Segments, mod, seg, N, i, j: CARDINAL;
BEGIN
  WITH dt.Components.Components^[com].DI DO
    N_Segments := 0;
    IF LastModule = 0 THEN
      RETURN;
    END;
    FOR i := 0 TO LastModule-1 DO
      WITH Modules^[i] DO
        IF Segments # NIL THEN
          segments := Segments;
          N := HIGH(Segments^)+1;
          IF N > 1 THEN
            sor.Shell(N ,CompareSegmentsInMod, ShakeSegmentsInMod);
          END;
          INC(N_Segments);
          IF N > 1 THEN
            FOR j := 0 TO N-2 DO
              WITH Segments^[j] DO
                IF (Begin+Sz # Segments^[j+1].Begin) THEN
                  INC(N_Segments);
                END;
              END;
            END;
          END;
        END; 
      END;    
    END;  
    IF N_Segments = 0 THEN
      RETURN;
    END;
    ASSERT(KGSegments = NIL);
    NEW(KGSegments, N_Segments);
    seg := 0;
    FOR mod := 0 TO LastModule-1 DO
      WITH Modules^[mod] DO
        IF Segments # NIL THEN
          WITH KGSegments^[seg] DO
            ModNo := mod;
            Begin := Segments^[0].Begin;
            ASSERT(Segments^[0].Sz # 0);
            End   := Begin + Segments^[0].Sz - 1;
          END;
          N := HIGH(Segments^)+1;
          IF N > 1 THEN
            FOR i := 0 TO N-2 DO
              IF Segments^[i].Begin+Segments^[i].Sz = Segments^[i+1].Begin THEN
                INC(KGSegments^[seg].End, Segments^[i+1].Sz);
              ELSE
                INC(seg);
                WITH KGSegments^[seg] DO
                  ModNo := mod;
                  Begin := Segments^[i+1].Begin;
                  ASSERT(Segments^[i+1].Sz # 0);
                  End   := Begin + Segments^[i+1].Sz - 1;
                END;
              END;
            END;
          END;
          INC(seg);
        END;
      END;
    END;
  END;
  ASSERT(seg=N_Segments);
  CurrCom := com;
  IF N_Segments > 1 THEN
    sor.Shell(N_Segments,CompareSegments, ShakeSegments);
  END;
END CreateKGroupSegments;


VAR
  ComponentIndex: CARDINAL;






PROCEDURE AddComponent (Component: dt.COMPONENT);

CONST
  N = 16;
VAR
  tmp: dt.PACOMPONENT;
BEGIN
  WITH dt.Components DO
    IF Components = NIL THEN
      NEW(Components, N);
      Count := 0;
    ELSIF Count = HIGH(Components^) THEN
      NEW(tmp, HIGH(Components^)+1 + N);
      sys.MOVE(sys.ADR(Components^), sys.ADR(tmp^), SIZE(Components^));
      DISPOSE(Components);
      Components := tmp;
    END;
    Components^[Count] := Component;
    Components^[Count].Index := ComponentIndex;
    INC(Count);
    INC(ComponentIndex);
  END;
END AddComponent;




PROCEDURE TypeImageLength (addr: Ud.IMAGE): CARDINAL;
VAR
  ptag: POINTER TO dt.TYPE_TAG;
  len: CARDINAL;
  PEnumType  : POINTER TO dt.TYPE_ENUM;
  PRecordType: POINTER TO dt.TYPE_RECORD;
  PProcType  : POINTER TO dt.TYPE_PROCEDURE;
  PClassType : POINTER TO dt.TYPE_CLASS;
BEGIN
  ptag := addr;
  CASE ptag^ OF
  | dt.Set       : len := SIZE(dt.TYPE_SET);
  | dt.Range     : len := SIZE(dt.TYPE_RANGE);
  | dt.Pointer   : len := SIZE(dt.TYPE_POINTER);
  | dt.Reference : len := SIZE(dt.TYPE_REFERENCE);
  | dt.Array_of  : len := SIZE(dt.TYPE_ARRAY_OF);
  | dt.Array     : len := SIZE(dt.TYPE_ARRAY);
  | dt.OpenArray : len := SIZE(dt.TYPE_OPEN_ARRAY);
  | dt.Enum      : PEnumType := addr;
                   len := SIZE(dt.TYPE_ENUM)
                          + PEnumType^.Quantity * SIZE(dt.TYPE_ENUM_ITEM);
  | dt.Record    : PRecordType := addr;
                   len := SIZE(dt.TYPE_RECORD)
                          + PRecordType^.Fields * SIZE(dt.TYPE_RECORD_FIELD);
  | dt.Class     : PClassType := addr;
                   len := SIZE(dt.TYPE_CLASS)
                          + PClassType^.MyMembers * SIZE(dt.TYPE_RECORD_FIELD);
  | dt.Procedure : PProcType := addr;
                   len := SIZE(dt.TYPE_PROCEDURE)
                          + PProcType^.ParamCount * SIZE(dt.TYPE_PROCEDURE_PARAM);

  END;
  RETURN len;
END TypeImageLength;


 <* PUSH *>
 <* -COVERFLOW *>
PROCEDURE type_hash(im: Ud.IMAGE) : Ud.HASH_RESULT;
VAR
  res: Ud.HASH_RESULT;
  i , ind  : CARDINAL;
  ad: P1;
  l:CARDINAL;
  ptype: dt.PTYPE_DATA;
BEGIN
  ASSERT(im # NIL);
  ind  := 0;
  ptype := dt.PTYPE_DATA(im);
  l:=  TypeImageLength(im);
  FOR i := SIZE(dt.TYPE_DATA) TO l-1  DO
    ad := sys.ADDADR(im, i);
    ind := (ind*i + ad^ ) MOD HashTableSizes[ptype^.Tag];
  END;
  res.number := ORD(ptype^.Tag);
  res.index := ind;
  RETURN res;
END type_hash;

<* POP *>

PROCEDURE CompareTypes(a1,a2: Ud.IMAGE; len: CARDINAL) : BOOLEAN;
VAR
  i       : CARDINAL;
  ad1, ad2: P1;
BEGIN
  ad1 := a1;
  ad2 := a2;
  IF ad1^ # ad2^ THEN    (*  теги не совпадают *)
    RETURN FALSE;
  END;
  FOR i := SIZE(dt.TYPE_DATA) TO len-1 DO
    ad1 := sys.ADDADR(a1, i);
    ad2 := sys.ADDADR(a2, i);
    IF ad1^ # ad2^ THEN
      RETURN FALSE;
    END;
  END;
  RETURN TRUE;
END CompareTypes;

(* Добавить тип в таблицу типов, вернет номер типа *)
PROCEDURE AddType (type-: ARRAY OF sys.LOC; VAR IsUnique: BOOLEAN): dt.R_TYPE;
BEGIN
  RETURN dt.FIRST_NONPRIMITIVE + Ud.Add(TypesTbl, Ud.IMAGE(sys.ADR(type)), CompareTypes , TypeImageLength, IsUnique);
END AddType;


(* Создание лексикографического ключа для таблицы типов *)
VAR
  CurrMod: CARDINAL;

PROCEDURE CompareTypeName (i, j: CARDINAL): BOOLEAN;
VAR
  in, jn: xs.txt_ptr;
  type  : dt.PTYPE;
BEGIN
  WITH dt.Components.Components^[CurrCom].DI.Modules^[CurrMod-1].ModuleTypes DO
    type := KTName^[i];
    ASSERT(tls.TypeName(type, in));
    type := KTName^[j];
    ASSERT(tls.TypeName(type, jn));
  END;
  RETURN in^ > jn^;
END CompareTypeName;


PROCEDURE SortKTName (com: dt.ComNo; mod: CARDINAL);
VAR
  name1: xs.txt_ptr;
  type1: dt.PTYPE;
  pdata: dt.PTYPE_DATA;

<* IF DEFINED (xd_debug) & xd_debug THEN *>
VAR
  i: CARDINAL;
  name2: xs.txt_ptr;
  type2: dt.PTYPE;
  tmp: xs.String;
<* END *>
BEGIN
  WITH dt.Components.Components^[com].DI.Modules^[mod-1].ModuleTypes DO
    IF Sorted THEN
      RETURN;
    END;
    Sorted := TRUE;
    KTQuantity := 0;
    type1 := tls.FirstType();
    LOOP
      pdata := dt.PTYPE_DATA (tls.TypeImage (type1));
      IF (com = pdata^.Com) AND (mod = pdata^.Mod) THEN
        ASSERT(tls.TypeName (type1, name1));
        IF name1^ # "" THEN
          INC (KTQuantity);
        END;
      END;
      tls.Get_Next_Type (type1);
      IF NOT tls.IsTypeValid (type1) THEN
        EXIT;
      END;
    END;
    IF KTQuantity = 0 THEN
      RETURN;
    END;
    NEW (KTName, KTQuantity);
    KTQuantity := 0;
    type1 := tls.FirstType();
    LOOP
      pdata := dt.PTYPE_DATA (tls.TypeImage (type1));
      IF (com = pdata^.Com) AND (mod = pdata^.Mod) THEN
        ASSERT(tls.TypeName (type1, name1));
        IF name1^ # "" THEN
          KTName^[KTQuantity] := type1;
          INC (KTQuantity);
        END;
      END;
      tls.Get_Next_Type (type1);
      IF NOT tls.IsTypeValid (type1) THEN
        EXIT;
      END;
    END;
    anykey := KTName;
    CurrCom := com;
    CurrMod := mod;
    sor.Shell(KTQuantity, CompareTypeName, Shake);

   <* IF DEFINED (xd_debug) & xd_debug THEN *>
    IF NOT opt.MergeEqualTypes THEN
      FOR i := 1 TO KTQuantity-1 DO
        type1 :=  KTName^[i];
        ASSERT(tls.TypeName (type1, name1));
        type2 :=  KTName^[i-1];
        ASSERT(tls.TypeName (type2, name2));
        IF name1^ = name2^ THEN
          IF opt.Debug(opt.Load) THEN
            printf ("Equal names '%s' found for types [%d] and [%d]\n", name1^, KTName^[i-1], KTName^[i]);
          END;
          IF tls.TypesCompatible (type1, type2) THEN
            IF opt.Debug(opt.Load) THEN
              printf ("but types is equal also\n", name1^, KTName^[i-1], KTName^[i]);
            END;
          ELSE
            fmt.print (tmp, "%s%d", name1^, KTName^[i-1]);
            ASSERT(tls.RenameType (type1, tmp));
            fmt.print (tmp, "%s%d", name2^, KTName^[i]);
            ASSERT(tls.RenameType (type2, tmp));
            IF opt.Debug(opt.Load) THEN
              printf ("but types is not equal (RenameType)\n", name1^, KTName^[i-1], KTName^[i]);
            END;
          END;
        END;
      END;
    END;
   <* END *>
  END;
END SortKTName;



(* Добавить обьект в таблицу обьектов модуля, вернет номер обьекта *)
PROCEDURE AddObject (com: dt.ComNo; mod: dt.ModNo; obj: dt.RAW_OBJECT);
VAR
  PObjects: dt.POBJECTS;
  l: CARDINAL;
  tmp: dt.PARAW_OBJECT;

BEGIN
  PObjects := tls.MakePObjects (com, mod);
  WITH PObjects^ DO
    WITH RawObjects DO
      IF RawObjects = NIL THEN
        (* Создание таблицы *)
        ASSERT(Count = 0);
        NEW(RawObjects, HN_Object);
        ASSERT(RawObjects # NIL);
        Count := 0;
      ELSIF Count > HIGH(RawObjects^) THEN
        (* Места в таблице хватает? *)
        l := HIGH(RawObjects^)+1;
        NEW(tmp, 2*l);
       <* IF DEFINED (stats) & stats THEN *>
        INC (AddObjectReNEW);
       <* END *>
        ASSERT(tmp <> NIL);
        sys.MOVE (sys.ADR(RawObjects^), sys.ADR(tmp^), SIZE(RawObjects^));
        DISPOSE(RawObjects);
        RawObjects := tmp;
      END;
      (* Записать в таблицу *)
      RawObjects^[Count] := obj;
      INC(Count);
    END;
  END;
END AddObject;


(* Создание ключа для таблицы модулей *)
PROCEDURE CompareModuleName (i,j: CARDINAL): BOOLEAN;
VAR
  in,  jn : xs.txt_ptr;
  iup, jup: xs.String;
BEGIN
  WITH dt.Components.Components^[CurrCom] DO
    in := tls.GetName( DI.Modules^[anykey^[i]].ModuleName);
    COPY(in^, iup);
    xs.Uppercase(iup);
    jn := tls.GetName( DI.Modules^[anykey^[j]].ModuleName);
    COPY(jn^, jup);
    xs.Uppercase(jup);
  END;
  RETURN iup > jup;
END CompareModuleName;

(* Создание ключей для таблицы модулей *)
PROCEDURE CreateKeysByModules (com: dt.ComNo);
VAR
  N, i: CARDINAL;
BEGIN
  WITH dt.Components.Components^[com] DO
    N := DI.LastModule;
    IF DI.KModules  # NIL THEN DISPOSE(DI.KModules); END;
    NEW(DI.KModules , N);
    FOR i := 0 TO N-1 DO DI.KModules ^[i] := i; END;
    anykey := DI.KModules ;
    CurrCom := com;
    sor.Shell(N,CompareModuleName, Shake);
  END;
END CreateKeysByModules;


(* Добавить паблик в таблицу *)
PROCEDURE AddPublic (com: dt.ComNo; public: dt.PUBLIC);
VAR
  tmp: dt.PAPUBLIC;
BEGIN
  WITH dt.Components.Components^[com].DI.Publics DO
    IF Publics = NIL THEN
      NEW(Publics, HN_Public);
      ASSERT(Publics # NIL);
      Count := 0;
    ELSIF Count > HIGH(Publics^) THEN
      NEW(tmp, (HIGH(Publics^)+1)*2);
     <* IF DEFINED (stats) & stats THEN *>
      INC (AddPublicReNEW);
     <* END *>
      ASSERT(tmp # NIL);
      sys.MOVE(sys.ADR(Publics^), sys.ADR(tmp^), SIZE(Publics^));
      DISPOSE(Publics);
      Publics := tmp;
    END;
    Publics^[Count] := public;
    INC(Count);
  END;
END AddPublic;



PROCEDURE ComparePublicAddress (i,j: CARDINAL): BOOLEAN;
VAR
  ia, ja: kt.ADDRESS;
BEGIN
  WITH dt.Components.Components^[CurrCom].DI.Publics DO
    ia := Publics^[KPAddr^[i]].addr;
    ja := Publics^[KPAddr^[j]].addr;
  END;
  RETURN ia > ja;
END ComparePublicAddress;


(* Отсортировать паблики по адресам *)
PROCEDURE SortPublicsByAddr (com: dt.ComNo);
VAR
  i, j, k: CARDINAL;
  a1, a2 : kt.ADDRESS;
  last   : BOOLEAN;
BEGIN
  WITH dt.Components.Components^[com].DI.Publics DO
    IF (Count#0) AND (KPAddr=NIL) THEN
      NEW(KPAddr, Count);
      FOR i := 0 TO Count-1 DO KPAddr^[i] := i; END;
      anykey := KPAddr;
      CurrCom := com;
      IF Count > 1 THEN
        sor.Shell(Count,ComparePublicAddress, Shake);
      END;
      i := 0;
      last := FALSE;
      LOOP
        k := i;
        IF i = Count-1 THEN EXIT; END;
        a1 := Publics^[KPAddr^[i]].addr;
        LOOP
          a2 := Publics^[KPAddr^[i+1]].addr;
          IF a1 # a2 THEN EXIT; END;
          INC(i);
          IF i = Count-1 THEN last := TRUE; EXIT; END;
        END;
        IF last THEN EXIT; END;
        FOR j := k TO i DO Publics^[KPAddr^[j]].len := a2 - a1; END;
        INC(i);
      END;
      FOR j := k TO i DO Publics^[KPAddr^[j]].len := 1; END;
    END;
  END;
END SortPublicsByAddr;


PROCEDURE ComparePublicName (i,j: CARDINAL): BOOLEAN;
VAR
  in, jn: xs.txt_ptr;
BEGIN
  WITH dt.Components.Components^[CurrCom].DI.Publics DO
    in := tls.GetName(Publics^[KPName^[i]].name);
    jn := tls.GetName(Publics^[KPName^[j]].name);
  END;
  RETURN in^ > jn^;
END ComparePublicName;


PROCEDURE SortPublicsByName (com: dt.ComNo);
VAR
  i: CARDINAL;
BEGIN
  WITH dt.Components.Components^[com].DI.Publics DO
    IF (Count#0) AND (KPName=NIL) THEN
      NEW(KPName, Count);
      FOR i := 0 TO Count-1 DO KPName^[i] := i; END;
      anykey := KPName;
      CurrCom := com;
      IF Count > 1 THEN
        sor.Shell(Count,ComparePublicName, Shake);
      END;
    END;
  END;
END SortPublicsByName;



PROCEDURE AddEnumeration (com: dt.ComNo; mod: CARDINAL; name, value: CARDINAL);
VAR
  tmp: dt.PARENUM;
BEGIN
  WITH dt.Components.Components^[com].DI.Modules^[mod-1].Enumerations DO
    IF Enums = NIL THEN
      NEW(Enums, HN_Enum);
      ASSERT(Enums # NIL);
      Count := 0;
    ELSIF Count > HIGH(Enums^) THEN
      NEW(tmp, HIGH(Enums^)+1 + HN_Enum);
     <* IF DEFINED (stats) & stats THEN *>
      INC (AddEnumReNEW);
     <* END *>
      ASSERT(tmp # NIL);
      sys.MOVE(sys.ADR(Enums^), sys.ADR(tmp^), SIZE(Enums^));
      DISPOSE(Enums);
      Enums := tmp;
    END;
    WITH Enums^[Count] DO
      Name  := name;
      Value := value;
    END;
    INC(Count);
  END;
END AddEnumeration;



PROCEDURE CompareEnumerationName (i,j: CARDINAL): BOOLEAN;
VAR
  in, jn: xs.txt_ptr;
BEGIN
  WITH dt.Components.Components^[CurrCom].DI.Modules^[CurrMod-1].Enumerations DO
    in := tls.GetName( Enums^[KEName^[i]].Name);
    jn := tls.GetName( Enums^[KEName^[j]].Name);
  END;
  RETURN in^ > jn^;
END CompareEnumerationName;


PROCEDURE SortEnumerationsByName (com: dt.ComNo; mod: CARDINAL);
VAR
  i: CARDINAL;
BEGIN
  WITH dt.Components.Components^[com].DI.Modules^[mod-1].Enumerations DO
    ASSERT( (Count#0) AND (KEName=NIL));
    NEW(KEName, Count);
    FOR i := 0 TO Count-1 DO KEName^[i] := i; END;
    anykey := KEName;
    CurrMod := mod;
    CurrCom := com;
    IF Count > 1 THEN
      sor.Shell(Count,CompareEnumerationName, Shake);
    END;
  END;
END SortEnumerationsByName;






PROCEDURE CreateProcBegin (com: dt.ComNo);
VAR
  P, m, p: CARDINAL;
BEGIN
  WITH dt.Components.Components^[com] DO
    ASSERT(DI.ProcBeginAddr = NIL);
    IF DI.LastModule # 0 THEN
      P := 0;
      FOR m := 0 TO DI.LastModule-1 DO
        INC(P, DI.Modules^[m].ModuleObjects.NestedObjects.ProcQuan);
      END;
      INC(P, DI.GlobalObjects.NestedObjects.ProcQuan);
      IF P # 0 THEN
        NEW(DI.ProcBeginAddr, P);
        P := 0;
        FOR m := 0 TO DI.LastModule-1 DO
          WITH DI.Modules^[m].ModuleObjects DO
            IF NestedObjects.ProcQuan # 0 THEN
              IF KObjects = NIL THEN
                ReadObjects (com, m+1);
                ASSERT(KObjects # NIL);
              END;
              FOR p := 0 TO NestedObjects.ProcQuan-1 DO
                WITH DI.ProcBeginAddr^[P] DO
                  Proc := tls.MakeObject (com, m+1, NestedObjects.ProcPos+p);
                  ASSERT(tls.ObjectAddr(Proc, ProcBegin));
                  Active := FALSE;
                END;
              END;
              INC(P);
            END;
          END;
        END;
        WITH DI.GlobalObjects DO
          IF NestedObjects.ProcQuan # 0 THEN
            IF KObjects = NIL THEN
              ReadObjects (com, 0);
              ASSERT(KObjects # NIL);
            END;
            FOR p := 0 TO NestedObjects.ProcQuan-1 DO
              WITH DI.ProcBeginAddr^[P] DO
                Proc := tls.MakeObject (com, 0, NestedObjects.ProcPos+p);
                ASSERT(tls.ObjectAddr(Proc, ProcBegin));
                Active := FALSE;
              END;
            END;
            INC(P);
          END;
        END;
      END;
      ASSERT(HIGH(DI.ProcBeginAddr^)+1 = P);
    END;
  END;
END CreateProcBegin;



PROCEDURE Read (Dump: dt.DUMP; l: CARDINAL; VAR Pos: CARDINAL): CARDINAL;
VAR
  p  : sys.ADDRESS;
  p1 : P1;
  p2 : P2;
  p4 : P4;
  res: CARDINAL;
BEGIN
  p := sys.ADR(Dump^[Pos]);
  INC(Pos,l);
  CASE l OF
  | 1 : p1 := p; res := VAL(CARDINAL,p1^);
  | 2 : p2 := p; res := VAL(CARDINAL,p2^);
  | 4 : p4 := p; res := VAL(CARDINAL,p4^);
  END;
  RETURN res;
END Read;

PROCEDURE Write (Dump: dt.DUMP; l,v: CARDINAL; VAR Pos: CARDINAL);
VAR
  p  : sys.ADDRESS;
  p1 : P1;
  p2 : P2;
  p4 : P4;
BEGIN
  p := sys.ADR(Dump^[Pos]);
  INC(Pos,l);
  CASE l OF
  | 1 : p1 := p; p1^ := VAL(sys.CARD8, v);
  | 2 : p2 := p; p2^ := VAL(sys.CARD16,v);
  | 4 : p4 := p; p4^ := VAL(sys.CARD32,v);
  END;
END Write;



<* IF DEFINED (xd_debug) & xd_debug THEN *>

PROCEDURE ShowType (type: dt.PTYPE);
VAR
  ptag: POINTER TO dt.TYPE_TAG;
  tag : dt.TYPE_TAG;

  PClassType       : POINTER TO dt.TYPE_CLASS;
  PEnumType        : POINTER TO dt.TYPE_ENUM;
  PEnumItemType    : POINTER TO dt.TYPE_ENUM_ITEM;
  PRangeType       : POINTER TO dt.TYPE_RANGE;

  name: xs.txt_ptr;
  sub : dt.PTYPE;
  i   : CARDINAL;

  field: dt.TYPE_RECORD_FIELD;

BEGIN
    IF tls.IsTypeBasePrimitive (type) THEN
      printf("%s {%d} ", dt.STD_TYPES[type].Name, dt.STD_TYPES[type].Length);
    ELSIF tls.IsTypePointerPrimitive (type) THEN
      printf("POINTER TO %s {%d} ", dt.STD_TYPES[type-dt.PRIMITIVE_TYPES].Name, dt.STD_TYPES[type-dt.PRIMITIVE_TYPES].Length);
    ELSE
      ASSERT(tls.TypeName(type, name));
      printf("%s = ", name^);
        ptag:=  tls.TypeImage(type);
        CASE ptag^ OF
        | dt.Reference:
            printf("REF ");

        | dt.Range  :
            PRangeType := sys.CAST(sys.ADDRESS, ptag);
            WITH PRangeType^ DO
              type := Base;
              ASSERT(tls.TypeName (type, name));
              printf("%s[%d] ", name^, Base);
              ASSERT(tls.TypeTag (type, tag));
              IF tag = dt.Int THEN
                printf("[%i..%i]", LONGINT(Min), LONGINT(Max));
              ELSIF tag = dt.Char THEN
                printf("['%c'..'%c']", VAL(CHAR, CARDINAL(Min)), VAL(CHAR, CARDINAL(Max)));
              ELSIF tag = dt.Boolean THEN
                printf("[FALSE=%u..TRUE=%u]", CARDINAL(Min), CARDINAL(Max));
              ELSE
                printf("[%u..%u]", CARDINAL(Min), CARDINAL(Max));
              END;
            END;

        | dt.Enum:
            PEnumType := sys.CAST(sys.ADDRESS, ptag);
            printf('ENUM[%d] (', PEnumType^.Base);
            WITH PEnumType^ DO
              PEnumItemType := sys.ADDADR(ptag, SIZE(dt.TYPE_ENUM));
              FOR i := 1 TO Quantity DO
                name := tls.GetName (PEnumItemType^.EnumName);
                printf('%s=%u', name^, PEnumItemType^.EnumValue);
                IF i # Quantity THEN
                  printf(', ');
                END;
                PEnumItemType := sys.ADDADR(PEnumItemType, SIZE(dt.TYPE_ENUM_ITEM));
              END;
            END;
            printf(')');


        | dt.Pointer:
            tls.SubType(type, sub);
            ASSERT(tls.TypeName(sub, name));
            IF name^ # "" THEN
              printf("POINTER TO %s", name^);
            ELSE
              printf("POINTER TO");
              IF type = sub THEN
                printf("ITSELF");
              ELSE
                ShowType(sub);
              END;
            END;
        | dt.Set:
            tls.SubType(type, type);
            ASSERT(tls.TypeName(type, name));
            printf("SET OF %s", name^);

        | dt.Procedure :
            tls.SubType(type, type);
            ASSERT(tls.TypeTag(type, tag));
            IF tag = dt.T_Void THEN
              printf("PROCEDURE");
            ELSE
              ASSERT(tls.TypeName(type, name));
              printf("PROCEDURE(): %s", name^);
            END;

        | dt.Array:
            printf('ARRAY ');
            sub := type;
            tls.ArrayIndexType(type, type);
            ShowType(type);
            printf(' OF ');
            tls.SubType(sub, type);
            ShowType (type);

        | dt.Array_of:
            printf('ARRAY OF ');
            tls.SubType(type, sub);
            ShowType (sub);

        | dt.OpenArray:
            printf('OPEN ARRAY OF ');
            tls.SubType(type, sub);
            ShowType (sub);

        | dt.Record:
            printf("RECORD ");
            FOR i := 1 TO tls.TypeLen (type) DO
              tls.Field (type, i, field);
              name := tls.GetName (field.FieldName);
              printf(' %s: ', name^);
              ASSERT(tls.TypeName (field.FieldType, name));
              printf(' %s [%d]; ', name^, field.FieldOffs);
            END;
            printf(' END');

        | dt.Class:
            printf("CLASS ");
            PClassType := sys.CAST (sys.ADDRESS, ptag);
            WITH PClassType^ DO
              IF Base # 0 THEN
                type := Base;
                ASSERT(tls.TypeName(type, name));
                printf(" (%s) [%d/%d]", name^, MyMembers, AllMembers);
              END;
              FOR i := 1 TO tls.TypeLen (type) DO
                tls.Field (type, i, field);
                name := tls.GetName (field.FieldName);
                printf(' %s: ', name^);
                ASSERT(tls.TypeName (field.FieldType, name));
                printf(' %s [%d]; ', name^, field.FieldOffs);
              END;
            END;
            printf(' END');

        END;
    END;
END ShowType;



PROCEDURE ShowSymbol (obj: dt.OBJECT);
VAR
  name    : xs.String;
  tname   : xs.txt_ptr;
  type    : dt.PTYPE;
  tag     : dt.SYM_TAG;
  reg_no  : CARDINAL;
  ref     : BOOLEAN;
  attr    : dt.SYM_ATTRIB;
  offs    : CARDINAL;
  addr    : kt.ADDRESS;
  size    : CARDINAL;
  start   : kt.ADDRESS;
  end     : kt.ADDRESS;
  object  : dt.OBJECT;
  RegName : xs.String;
  scope   : xs.String;
  obj_size: BOOLEAN;

  PROCEDURE GetRegName (RegNo: CARDINAL);
  VAR
    i: CARDINAL;
  BEGIN
    FOR i := 0 TO kt.RegsNum-1 DO
      IF kt.Registers[i].reg_no = RegNo THEN
        COPY(kt.Registers[i].name, RegName);
        RETURN;
      END;
    END;
    fmt.print (RegName, "%u", RegNo);
  END GetRegName;

BEGIN
  printf("[%i] ", obj.rec);
  tls.ObjectName (obj, name);
  ASSERT (tls.ObjectTag(obj, tag));
  obj_size := tls.ObjectSize (obj, size);
  ASSERT (tls.ObjectType(obj, type));
  ASSERT (tls.TypeName(type, tname));
  object := tls.ObjectParentScope (obj);
  IF tls.EqualObjects (object, dt.Invalid_Object) THEN
    fmt.print (scope, "global");
  ELSE
    fmt.print (scope, "local in [%d]", object.rec);
  END;
  CASE tag OF
  | dt.Sy_Register:
   <* IF DEFINED (stats) & stats THEN *>
    INC(Sy_Reg_No);
   <* END *>
    ASSERT (tls.GetLocalObject_Reg (obj, reg_no, ref));
    GetRegName (reg_no);
    printf("VAR %s: %s [%i] (s=%u); %s, base register %s", name, tname^, type, size, scope, RegName);
    ASSERT (tls.ObjectAttr (obj, attr));
    IF dt.SA_Param     IN attr THEN printf(", Param"); END;
    IF dt.SA_ReadOnly  IN attr THEN printf(", ReadOnly"); END;
    IF dt.SA_Reference IN attr THEN printf(", Reference"); END;
    printf("\n");

  | dt.Sy_Relative:
   <* IF DEFINED (stats) & stats THEN *>
    INC(Sy_Rel_No);
   <* END *>
    ASSERT (tls.GetLocalObject_Reg (obj, reg_no, ref));
    ASSERT (tls.GetLocalObject_Addr (obj, 0, offs));
    GetRegName (reg_no);
    printf("VAR %s: %s [%i] (s=%u); %s, offset %i by register %s", name, tname^, type, size, scope, offs, RegName);
    ASSERT (tls.ObjectAttr (obj, attr));
    IF dt.SA_Param     IN attr THEN printf(", Param"); END;
    IF dt.SA_ReadOnly  IN attr THEN printf(", ReadOnly"); END;
    IF dt.SA_Reference IN attr THEN printf(", Reference"); END;
    printf("\n");

  | dt.Sy_Proc:
   <* IF DEFINED (stats) & stats THEN *>
    INC(Sy_Proc_No);
    INC (ProcedureTotalLength, size);
   <* END *>
    ASSERT (obj_size);
    ASSERT (tls.ObjectAddr (obj, addr));
    tls.ProcAttr (obj, start, end);
    printf("PROCEDURE [%i] %s; scope=%s, addr=%$8X, size=%u",
           type, name, scope, addr, size);
    printf(", start=%$8X, end=%$8X, length%$8X, frame=%d, %d\n",
           start, end, addr+size, tls.ProcHasFrame (obj), tls.ProcFrameSize (obj));

  | dt.Sy_Var:
   <* IF DEFINED (stats) & stats THEN *>
    INC(Sy_Var_No);
   <* END *>
    ASSERT (obj_size);
    ASSERT (tls.ObjectAddr (obj, addr));
    IF tname^ # '' THEN
      printf("VAR %s: %s [%i] (s=%u); %s, %$8Xh\n", name, tname^, type, size, scope, addr);
    ELSE
      printf("VAR %s: ", name);
      ShowType(type);
      printf('[%i]; %s, %$8Xh\n', type, scope, addr);
    END;
  END;
END ShowSymbol;

<* END *>


CONST
  -- Максимально допустимое количество вложености процедур
  MAX_PROCEDURE_LEVEL = 255;

TYPE
  -- Для уровня вложенности содержит ссылку на описание
  -- вложенных обьектов модуля или процедуры
  LEVEL_DATA = RECORD
                 PNestedObjects: dt.PNESTED_OBJECTS;
               END;

  NESTED_LEVELS = ARRAY [0..MAX_PROCEDURE_LEVEL] OF LEVEL_DATA;

VAR
  LevelNestedObjects: NESTED_LEVELS;


-- Сортировка ключей для таблицы обьектов

-- Сравнение по адресам обьектов
PROCEDURE CompareObjAddr (i,j: CARDINAL): BOOLEAN;
VAR
  in, jn: kt.ADDRESS;
  var: dt.OBJECT;
BEGIN
  var := tls.MakeObject (CurrCom, CurrMod, anykey^[i]);
  ASSERT(tls.ObjectAddr(var, in));
  var := tls.MakeObject (CurrCom, CurrMod, anykey^[j]);
  ASSERT(tls.ObjectAddr(var, jn));
  RETURN in > jn;
END CompareObjAddr;


VAR
  -- левый индекс в ключе для сортироки
  LeftIndex: sys.CARD32;

-- Сравнение по именам обьектов
PROCEDURE CompareObjName (i,j: CARDINAL): BOOLEAN;
VAR
  in, jn: xs.String;
  var: dt.OBJECT;
BEGIN
  var := tls.MakeObject (CurrCom, CurrMod, anykey^[LeftIndex+i]);
  tls.ObjectName (var, in);
  var := tls.MakeObject (CurrCom, CurrMod, anykey^[LeftIndex+j]);
  tls.ObjectName (var, jn);
  RETURN in > jn;
END CompareObjName;

-- Обмен ключа по имена  обьектов
PROCEDURE ShakeObjName (i,j: CARDINAL);
VAR
  tmp: dt.KEY_INDEX;
BEGIN
  INC(i, LeftIndex);
  INC(j, LeftIndex);
  tmp := anykey^[i];
  anykey^[i] := anykey^[j];
  anykey^[j] := tmp;
END ShakeObjName;



(* Построение таблицы обьектов                                     *)
(* Если номер модуля равен dt.Fake_Module - таблица для компоненты *)
PROCEDURE ReadObjects (com: dt.ComNo; mod: dt.ModNo);
VAR
  PObjects     : dt.POBJECTS;   (* Обьекты                                 *)
  proc_level   : CARDINAL;      (* Уровень вложености процедур             *)
  CountObjects : CARDINAL;      (* Всего обьектов                          *)
  NextFreeIndex: CARDINAL;      (* Свободный индекс для локальных обьектов *)


  PROCEDURE AllocNestedObjects;
  BEGIN
    WITH LevelNestedObjects[proc_level].PNestedObjects^ DO
      VarPos        := NextFreeIndex;      -- индекс переменных
      ParamPos      := VarPos+VarQuan;     -- индекс параметров
      ProcPos       := ParamPos+ParamQuan; -- индекс процедур
      NextFreeIndex := ProcPos+ProcQuan;   -- следующий свободный
      VarQuan       := 0;                  -- количество переменных
      ParamQuan     := 0;                  -- количество параметров
      ProcQuan      := 0;                  -- количество процедур
    END;
  END AllocNestedObjects;


  PROCEDURE ReadOneObject (put: BOOLEAN; i: CARDINAL);
  VAR
    pred_scope: CARDINAL;
    local_index: CARDINAL;
  BEGIN
    WITH PObjects^ DO
      WITH RawObjects DO
        WITH RawObjects^[i] DO
          CASE Tag OF
          | dt.Sy_Proc:
            pred_scope := MAX(CARDINAL);
            local_index := MAX(CARDINAL);
            IF put THEN
              WITH LevelNestedObjects[proc_level].PNestedObjects^ DO
                pred_scope := LocalIndex;
                local_index := ProcPos+ProcQuan;
              END;
            ELSE
              DataProc.NestedObjects := dt.EmptyNestedObjects;
              INC(CountObjects);
            END;
            INC(LevelNestedObjects[proc_level].PNestedObjects^.ProcQuan);
            INC(proc_level);
            LevelNestedObjects[proc_level].PNestedObjects := sys.ADR(DataProc.NestedObjects);
            IF put THEN
              ParentScope := pred_scope;
              WITH LevelNestedObjects[proc_level].PNestedObjects^ DO
                LocalIndex := local_index;
                KObjects^[LocalIndex] := i;
              END;
              AllocNestedObjects;
            END;
          | dt.Sy_End_Block:
            IF put THEN
              WITH LevelNestedObjects[proc_level].PNestedObjects^ DO
                ParentScope := LocalIndex;
                ASSERT(ParamPos = VarPos+VarQuan);
                ASSERT(ProcPos  = ParamPos+ParamQuan);
              END;
            END;
            DEC(proc_level);
          | dt.Sy_Var:
            WITH LevelNestedObjects[proc_level].PNestedObjects^ DO
              IF put THEN
                ParentScope := LocalIndex;
                KObjects^[VarPos+VarQuan] := i;
              ELSE
                INC(CountObjects);
              END;
              INC(VarQuan);
            END;
          | dt.Sy_Register :
            ASSERT(proc_level#0);
            WITH LevelNestedObjects[proc_level].PNestedObjects^ DO
              IF put THEN
                ParentScope := LocalIndex;
                IF dt.SA_Param IN DataReg.Attrib THEN
                  KObjects^[ParamPos+ParamQuan] := i;
                ELSE
                  KObjects^[VarPos+VarQuan] := i;
                END;
              ELSE
                INC(CountObjects);
              END;
              IF dt.SA_Param IN DataReg.Attrib THEN
                INC(ParamQuan);
              ELSE
                INC(VarQuan);
              END;
            END;
          | dt.Sy_Relative :
            ASSERT(proc_level#0);
            WITH LevelNestedObjects[proc_level].PNestedObjects^ DO
              IF put THEN
                ParentScope := LocalIndex;
                IF dt.SA_Param IN DataRel.Attrib THEN
                  KObjects^[ParamPos+ParamQuan] := i;
                ELSE
                  KObjects^[VarPos+VarQuan] := i;
                END;
              ELSE
                INC(CountObjects);
              END;
              IF dt.SA_Param IN DataRel.Attrib THEN
                INC(ParamQuan);
              ELSE
                INC(VarQuan);
              END;
            END;
          END;
        END;
      END;
    END;
  END ReadOneObject;


  PROCEDURE CreateKObjects;

    PROCEDURE SortNestedObjName (VAR NestedObjects: dt.NESTED_OBJECTS);
    BEGIN
      WITH NestedObjects DO
        LeftIndex := VarPos;
        IF  VarQuan > 1 THEN
          sor.Shell(VarQuan,   CompareObjName, ShakeObjName);
        END;
        LeftIndex := ParamPos;
        IF  ParamQuan > 1 THEN
          sor.Shell(ParamQuan, CompareObjName, ShakeObjName);
        END;
        LeftIndex := ProcPos;
        IF ProcQuan > 1 THEN
          sor.Shell(ProcQuan,  CompareObjName, ShakeObjName);
        END;
      END;
    END SortNestedObjName;

  VAR
    i, N: CARDINAL;
    obj : dt.OBJECT;
    addr: kt.ADDRESS;
  BEGIN
    WITH PObjects^ DO
      CurrCom := com;
      CurrMod := mod;
      -- подсчитаем количество обьектов, имеющих адрес
      N := 0;
      FOR i := 0 TO CountObjects-1 DO
        obj := tls.MakeObject (com, mod, i);
        IF tls.ObjectAddr (obj, addr) THEN
          INC(N);
        END;
      END;
      IF N # 0 THEN
        -- выделяем ключи по адресам
        NEW(KObjAddr, N);
        ASSERT(KObjAddr#NIL);
        N := 0;
        FOR i := 0 TO CountObjects-1 DO
          obj := tls.MakeObject (com, mod, i);
          IF tls.ObjectAddr (obj, addr) THEN
            KObjAddr^[N] := i;
            INC(N);
          END;
        END;
        -- сортировка ключа по адресам, для всех обьектов
        anykey := KObjAddr;
        IF N > 1 THEN
          sor.Shell(N, CompareObjAddr, Shake);
        END;
      END;
      -- выделяем ключи по локальным именам
      NEW(KObjName, CountObjects);
      ASSERT(KObjName#NIL);
      FOR i := 0 TO CountObjects-1 DO
        KObjName^[i] := i;
      END;
      -- сортировка ключа по именам
      anykey := KObjName;
      SortNestedObjName (NestedObjects);
      -- для локальных обьектов модуля
      WITH RawObjects DO
        FOR i := 0 TO CountObjects-1 DO
          WITH RawObjects^[KObjects^[i]] DO
            IF Tag = dt.Sy_Proc THEN
              -- для локальных обьектов модуля
              SortNestedObjName (DataProc.NestedObjects);
            END;
          END;
        END;
      END;
    END;
  END CreateKObjects;

VAR
  i: CARDINAL;

BEGIN
  PObjects := tls.MakePObjects (com, mod);
  WITH PObjects^ DO
    IF RawObjects.Count = 0 THEN
      -- нет обьектов в этом модуле
      RETURN;
    END;
    -- глобальные обьекты, для процедур - собственные
    -- вложенные обьекты, содержатся в обьекте Sy_Proc
    NestedObjects := dt.EmptyNestedObjects;
    -- нет предшественника и не имеет индекса в ключе
    NestedObjects.LocalIndex := MAX(sys.CARD16);
    -- подсчитываем число вложенных обьектов
    proc_level := 0;
    LevelNestedObjects[0].PNestedObjects := sys.ADR(NestedObjects);
    -- подсчитываем число всех обьектов
    CountObjects := 0;
    NextFreeIndex := 0;
    FOR i := 0 TO RawObjects.Count-1 DO
      ReadOneObject (FALSE, i);
    END;
    ASSERT(proc_level = 0);
    IF CountObjects = 0 THEN RETURN; END;
    -- выделяем ключ
    NEW(KObjects, CountObjects);
    ASSERT(KObjects#NIL);
    -- глобальные переменные и процедуры в начале ключа
    NextFreeIndex := 0;
    AllocNestedObjects;
    -- распределяем обьекты по ключу
    FOR i := 0 TO RawObjects.Count-1 DO
      ReadOneObject (TRUE, i);
    END;
    -- выделение и сортировка ключей для таблицы обьектов
    CreateKObjects;
  END;
END ReadObjects;




---------- Создание таблицы стандартных типов -----------
PROCEDURE CompareStdTypeName (i,j: CARDINAL): BOOLEAN;
VAR
  in, jn: xs.txt_ptr;
BEGIN
  in := sys.ADR(dt.STD_TYPES[dt.KNamesStdTypes[i]].Name);
  jn := sys.ADR(dt.STD_TYPES[dt.KNamesStdTypes[j]].Name);
  RETURN in^ > jn^;
END CompareStdTypeName;

PROCEDURE ShakeStdTypeName (i,j: CARDINAL);
VAR
  tmp: dt.KEY_INDEX;
BEGIN
  tmp                   := dt.KNamesStdTypes[i];
  dt.KNamesStdTypes[i] := dt.KNamesStdTypes[j];
  dt.KNamesStdTypes[j] := tmp;
END ShakeStdTypeName;

PROCEDURE CreateKStdTypeName;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO HIGH(dt.KNamesStdTypes) DO dt.KNamesStdTypes[i] := i; END;
    sor.Shell(HIGH(dt.KNamesStdTypes)+1, CompareStdTypeName, ShakeStdTypeName);
END CreateKStdTypeName;


PROCEDURE SetComLanguage (com: dt.ComNo);
VAR
  mod : dt.ModNo;
  lang: dt.LANGUAGE;
BEGIN
  ASSERT (tls.IsComValid (com));
  WITH dt.Components.Components^[com].DI DO
    mod := 0;
    Language := dt.Lng_Unknown;
    WHILE mod < LastModule DO
      lang := Modules^[mod].Language;
      IF Language < lang THEN
        Language := lang;
      END;
      INC (mod);
    END;
  END;
END SetComLanguage;



PROCEDURE CorrectModuleName (VAR name: ARRAY OF CHAR);
VAR
  l, i: CARDINAL;
BEGIN
  l := LENGTH(name);
  IF l > 0 THEN
    FOR i := 0 TO l-1 DO
      CASE name[i] OF
      | '\', '/':
        name[i] := '`';
      ELSE
      END;
    END;
  END;
END CorrectModuleName;



<* IF DEFINED (xd_debug) & xd_debug THEN *>

PROCEDURE ShowDebugInfo (com: dt.ComNo);

  PROCEDURE write_procedure (indent: ARRAY OF CHAR; obj: dt.OBJECT);

  VAR
    i, size, rev: CARDINAL;
    line1, line2: CARDINAL;
    lines       : CARDINAL;
    param_count : CARDINAL;
    local_count : CARDINAL;
    proc_count  : CARDINAL;
    loc_obj     : dt.OBJECT;
    tmp         : xs.String;
    addr        : kt.ADDRESS;
  BEGIN
    printf (indent);
    ShowSymbol (obj);
    param_count := tls.ParamVarsNo (obj);
    IF param_count # 0 THEN
      printf ("%s      Parameters [%d]\n", indent, param_count);
      FOR i :=  0 TO param_count-1 DO
        loc_obj := tls.GetParamVar (obj, i);
        printf ("%s        ", indent);
        ShowSymbol (loc_obj);
      END;
    END;
    local_count := tls.LocalVarsNo (obj);
    IF local_count # 0 THEN
      printf ("%s      Variables [%d]\n", indent, local_count);
      FOR i :=  0 TO local_count-1 DO
        loc_obj := tls.GetLocalVar (obj, i);
        printf ("%s        ", indent);
        ShowSymbol (loc_obj);
      END;
    END;
    proc_count := tls.LocalProcsNo (obj);
    IF proc_count # 0 THEN
      printf ("%s      Procedures [%d]\n", indent, proc_count);
      FOR i :=  0 TO proc_count-1 DO
        loc_obj := tls.GetLocalProc (obj, i);
        COPY(indent, tmp);
        xs.Append ('        ', tmp);
        write_procedure (tmp, loc_obj);
      END;
    END;
   <* IF DEFINED (stats) & stats THEN *>
    IF opt.Debug (opt.Another) THEN
      ASSERT(tls.ObjectAddr (obj, addr));
      ASSERT(tls.ObjectSize (obj, size));
      IF tls.SourceByAddrInMod (tls.ObjectCom(obj), tls.ObjectMod(obj), addr, line1) THEN
        lines := 1;
      ELSE
        lines := 0;
        line1 := 0;
      END;
      rev := 0;
      FOR i := 0 TO size-1 DO
        IF tls.SourceByAddrInMod (tls.ObjectCom(obj), tls.ObjectMod(obj), tls.AddAdr(addr, i), line2) THEN
          IF line1 # line2 THEN
            INC(lines);
            INC(rev, ORD(line1 > line2));
            line1 := line2;
          END;
        END;
      END;
      IF lines = 0 THEN
        printf ("%s      No lines respect to code\n", indent);
      ELSE
        printf ("%s      Lines order with respect to code (rev/all): %d/%d, reverse code order: %.1f%\n", indent, rev, lines, 100. * VAL(REAL, rev) / VAL(REAL, lines));
      END;
      INC(ProcedureLines, lines);
      INC(ReverseOrder, rev);
      INC(ProcedureExactlyOrder, ORD(rev = 0));
    END;
   <* END *>
  END write_procedure;


VAR
  type: dt.PTYPE;
  obj : dt.OBJECT;
  m   : CARDINAL;
  i, j: CARDINAL;
  n   : xs.txt_ptr;


  PROCEDURE write_objects (mod: dt.ModNo);
  VAR
    var_count: CARDINAL;
    proc_count: CARDINAL;
    j: CARDINAL;
  BEGIN
    printf('  Objects\n');
     var_count := tls.VarsNo (com, mod);
     IF var_count # 0 THEN
       printf('    Global variables [%d], sorting by name\n', var_count);
       FOR j := 0 TO var_count-1 DO
         obj := tls.GetVar (com, mod, j);
         printf('      ');
         ShowSymbol (obj);
       END;
     END;
     proc_count := tls.LabelsNo (com, mod);
     IF proc_count # 0 THEN
       printf('    Global procedures [%d], sorting by name\n', proc_count);
       FOR j := 0 TO proc_count-1 DO
         obj := tls.GetLabel (com, mod, j);
         write_procedure ('      ', obj);
       END;
     END;
  END write_objects;


VAR
  str: xs.String;
  public: dt.PUBLIC;
  lastline, line: CARDINAL;
  addr: kt.ADDRESS;
  coderec, codei: CARDINAL;

BEGIN
  ASSERT( tls.ComName (com, n) );
  printf('\n--------------------------\n');
  printf('Debug info for component - %s\n', n^);
  WITH dt.Components.Components^[com].DI DO
    printf('\n--------------------------\n');

    printf('Modules - %u\n\n', LastModule);
    IF LastModule = 0 THEN
      printf('  No modules\n');
    ELSE
      printf('\n  --------------------------\n');
      printf('  Fake module - global objects\n\n');
      write_objects (dt.Fake_Module);
      FOR i := 0 TO LastModule-1 DO
        WITH Modules^[i] DO
          printf('\n  --------------------------\n');
          printf('  Module index - [%d]\n', i+1);
          ASSERT (tls.ModName (com, i+1, n));
          printf('  Module name - %s\n', n^);
          IF tls.ModHaveDebugInfo (com, i+1) THEN
            printf('  Language - ');
            CASE tls.ModLanguage (com, i+1) OF
            | dt.Lng_Unknown:
              printf("Unknown\n");
            | dt.Lng_Asm:
              printf("Assembler\n");
            | dt.Lng_M2:
              printf("Modula-2\n");
            | dt.Lng_O2:
              printf("Oberon-2\n");
            | dt.Lng_Java:
              printf("Jaba\n");
            | dt.Lng_C:
              printf("C\n");
            | dt.Lng_CPP:
              printf("C++\n");
            END;
            IF tls.SourceName (com, i+1, str) THEN
              printf('  Source file - %s\n', str);
            END;
            IF tls.ModHaveSource (com, i+1) THEN
              printf('  Source file found\n');
              printf('  Last source line - %d\n', tls.LastSourceLine (com, i+1));
            END;
            lastline := tls.LastLineHasCode (com, i+1);
            IF lastline # 0 THEN
              printf('  Last source line has code - %d\n', lastline);
              printf('  Source/code table\n');
              FOR line := 0 TO lastline DO
                IF tls.AddrBySource (com, i+1, line, addr) THEN
                  printf('    line - %5d -> addr - 0x%$8x\n', line, addr);
                  coderec := tls.CodeNum (com, i+1, line);
                  printf('    code records - %d\n', coderec);
                  FOR codei := 1 TO coderec DO
                    printf('      addr - 0x%$8x\n', tls.GetNCode (com, i+1, line, codei));
                  END;
                END;
              END;
            END;

            IF Segments # NIL THEN
              printf('  Segments of module, total %u\n', HIGH(Segments^)+1);
              printf('             Name                 Number    Begin    Size    End\n');
              FOR j := 0 TO HIGH(Segments^) DO
                WITH Segments^[j] DO
                  n := tls.GetName (Name);
                  printf('    %-30s %3d   0x%$8X %4u 0x%$8X\n', n^, Number, Begin, Sz, Begin+Sz-1);
                END;
              END;
            END;
            write_objects (i+1);

            WITH ModuleTypes DO
              IF KTQuantity # 0 THEN
                printf('  Table of named types of module\n');
                IF NOT Sorted THEN
                  SortKTName (com, i+1);
                END;
                FOR j := 0 TO KTQuantity-1 DO
                  type := KTName^[j];
                  printf('    [%3i] ', type);
                  ShowType(type);
                  printf('\n');
                END;
              END;
            END;

            WITH Enumerations DO
              IF Count # 0 THEN
                printf('  Enumerations\n');
                IF KEName = NIL THEN
                  SortEnumerationsByName (com, i+1);
                  ASSERT(KEName#NIL);
                END;
                FOR j := 0 TO Count-1 DO
                  WITH Enums^[j] DO
                    n := tls.GetName(Name);
                    printf("    %2i <- %-30s", Value, n^);
                  END;
                  WITH Enums^[KEName^[j]] DO
                    n := tls.GetName(Name);
                    printf(" | %2i <- %-30s\n", Value, n^);
                  END;
                END;
              END;
            END;

          ELSE
            printf('  No debug info\n');
          END;
        END;
      END;
    END;
    WITH Publics DO
      IF Count # 0 THEN
        printf('  Publics\n');
        SortPublicsByAddr (com);
        SortPublicsByName (com);
        FOR m := 0 TO Count-1 DO
          public := tls.GetPublic (com, m);
          n := tls.GetName(public.name);
          IF public.code THEN
            printf('    %3u. PROC %$8X %-4u %-30s\n', m+1, public.addr, public.len, n^);
          ELSE
            printf('    %3u. VAR  %$8X %-4u %-30s\n', m+1, public.addr, public.len, n^);
          END;
        END;
      END;
    END;
    IF KGSegments # NIL THEN
      printf("  Module Begin End\n");
      FOR m := 0 TO HIGH(KGSegments^) DO
        WITH KGSegments^[m] DO
          ASSERT (tls.ModName (com, ModNo+1, n));
          printf("\t%3u\t%$8X - %$8X\t%s\n", ModNo+1, Begin, End, n^);
          IF m # HIGH(KGSegments^) THEN
            IF End >= KGSegments^[m+1].Begin THEN
              printf ("overlapping\n");
            END
          END;
        END;  
      END;
    END;
  END;
END ShowDebugInfo;


PROCEDURE ShowDebugStat ();
VAR
  type: dt.PTYPE;
BEGIN
  printf('Types  information: \n' );
  IF NOT Ud.IsEmptyStorage(TypesTbl) THEN
    printf('  Table of types\n');
    type := tls.FirstType();
    LOOP
      printf('    [%$8X] ', type);
      ShowType(type);
      printf('\n');
      tls.Get_Next_Type(type);
      IF NOT tls.IsTypeValid(type) THEN
        EXIT;
      END;
    END;
  END;
  Ud.PrintStorageInfo(TypesTbl);
  Ud.PrintStorageInfo(Names);
  --Ud.PrintHashTbl(TypesTbl);
END ShowDebugStat;


PROCEDURE IncrementalDbgModules ();
VAR
  com: dt.ComNo;
  mod: dt.ModNo;
  AllModules: CARDINAL;
  AllHaveDebugInfo: CARDINAL;
  AllProcessed: CARDINAL;
BEGIN
  AllModules := 0;
  AllHaveDebugInfo := 0;
  AllProcessed := 0;
  WITH dt.Components DO
    IF Count > 0 THEN
      FOR com := 0 TO Count-1 DO
        WITH Components^[com].DI DO
          IF LastModule > 0 THEN
            INC (AllModules, LastModule);
            FOR mod := 0 TO LastModule-1 DO
              WITH Modules^[mod] DO
                IF DebugInfoReference # NIL THEN
                  INC (AllHaveDebugInfo);
                END;
                IF DebugInfoProcessed THEN
                  INC (AllProcessed);
                END;
              END;
            END;
          END;
        END;
      END;
    END;
  END;
  printf ('Incremental dbg info statistic\n');
  printf ('  All modules = %d\n', AllModules);
  IF AllModules = 0 THEN
    printf ('  Modules have incremental debug info = %d\n', AllHaveDebugInfo);
  ELSE
    printf ('  Modules have incremental debug info = %d (%.2f%%)\n', AllHaveDebugInfo, 100.0*VAL(REAL, AllHaveDebugInfo)/VAL(REAL, AllModules));
  END;
  IF AllHaveDebugInfo = 0 THEN
    printf ('  Processed modules = %d\n', AllProcessed);
  ELSE
    printf ('  Processed modules = %d (%.2f%%)\n', AllProcessed, 100.0*VAL(REAL, AllProcessed)/VAL(REAL, AllHaveDebugInfo));
  END;
END IncrementalDbgModules;

<* END *>



PROCEDURE CreateNames();
BEGIN
 Ud.CreateStorage(Names,  SLOT_SIZE_FOR_NAMES, HASH_SIZE_FOR_NAMES, name_hash);
END CreateNames;

PROCEDURE CreateTypes();
BEGIN
  Ud.CreateStorage(TypesTbl,  SLOT_SIZE_FOR_TYPES, HashTableSizes,type_hash);
END CreateTypes;




BEGIN
  ComponentIndex := 0;
  CreateNames();
  CreateTypes();
  CreateKStdTypeName;
<* IF DEFINED (xd_debug) & xd_debug THEN *>
<* IF DEFINED (stats) & stats THEN *>
  AddNameReNEW           := 0;
  AddNamesReNEW          := 0;
  AddTypeReNEWKeys       := 0;
  AddTypeReNEWNamedTypes := 0;
  AddObjectReNEW         := 0;
  AddPublicReNEW         := 0;
  AddEnumReNEW           := 0;
  Sy_Proc_No             := 0;
  Sy_Var_No              := 0;
  Sy_Reg_No              := 0;
  Sy_Rel_No              := 0;
  ProcedureLines         := 0;
  ReverseOrder           := 0;
  ProcedureExactlyOrder  := 0;
  ProcedureTotalLength   := 0;
FINALLY
  IF opt.Debug (opt.Another) THEN
    printf ("\n----- Statistics -------\n");
    printf ("  Procedures     = %i\n", Sy_Proc_No);
    printf ("  Variables      = %i\n", Sy_Var_No);
    printf ("  Reg. variables = %i / %-6i", Sy_Reg_No, Sy_Reg_No+Sy_Rel_No);
    IF Sy_Reg_No+Sy_Rel_No > 0 THEN
      printf (" %6.2f%%", VAL(REAL, Sy_Reg_No) * 100 / VAL(REAL, Sy_Reg_No+Sy_Rel_No));
    END;
    printf ("\n");
    printf ("  Rel. variables = %i / %-6i", Sy_Rel_No, Sy_Reg_No+Sy_Rel_No);
    IF Sy_Reg_No+Sy_Rel_No > 0 THEN
      printf (" %6.2f%%", VAL(REAL, Sy_Rel_No) * 100 / VAL(REAL, Sy_Reg_No+Sy_Rel_No));
    END;
    printf ("\n");
    IF ProcedureLines > 0 THEN
      printf ("  Lines correspondense to code %d\n", ProcedureLines);
      printf ("  Reverse lines order %d %.2f%%\n", ReverseOrder, VAL(REAL, ReverseOrder) * 100 / VAL(REAL, ProcedureLines));
      printf ("  Exactly lines order %d %.2f%%\n", ProcedureExactlyOrder, VAL(REAL, ProcedureExactlyOrder) * 100 / VAL(REAL, Sy_Proc_No));
    END;
    printf ("------------------------\n\n");
    printf ("AddNameReNEW\t\t%d\n", AddNameReNEW);
    printf ("AddNamesReNEW\t\t%d\n", AddNamesReNEW);
    printf ("AddTypeReNEWKeys\t%d\n", AddTypeReNEWKeys);
    printf ("AddTypeReNEWNamedTypes\t%d\n", AddTypeReNEWNamedTypes);
    printf ("AddObjectReNEW\t\t%d\n", AddObjectReNEW);
    printf ("AddPublicReNEW\t\t%d\n", AddPublicReNEW);
    printf ("AddEnumReNEW\t\t%d\n", AddEnumReNEW);
    printf ("------------------------\n");
    printf ("Procedure Average Length\t\t%d\n", ProcedureTotalLength DIV Sy_Proc_No);
  END;
  IncrementalDbgModules ();
<* END *>
<* END *>
END DI_Build.
