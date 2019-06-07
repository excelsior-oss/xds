-- Конвертор для формата NB04 (HLL) во внутреннее представление

<* Storage+ *>
<* ALIGNMENT="1" *>

IMPLEMENTATION MODULE DI_NB04;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT dt  := DI_Types;
IMPORT bld := DI_Build;
IMPORT tls := DI_Tools;

IMPORT xs  := xStr;
IMPORT msg := MsgNo;
IMPORT opt := Options;

IMPORT kt  := KrnTypes;

IMPORT exc := EXCEPTIONS;

FROM Printf IMPORT printf;

FROM KrnTypes IMPORT EXEC_INFO;
FROM DI_Types IMPORT ComNo, COMPONENT, RAW_DEBUG_INFO, PROCESS_DEBUGINFO_PROC;

TYPE
  PROCESS_MODE = (mode_brief, mode_full);

VAR
  CurrComponent: POINTER TO COMPONENT;
  CurrCom: CARDINAL;
  source   : exc.ExceptionSource;


PROCEDURE Error (no: CARDINAL);
BEGIN
  exc.RAISE(source, no, '');
END Error;


(* Размещение новых модулей *)
PROCEDURE AllocateModules (no: CARDINAL);

VAR
  i  : CARDINAL;

BEGIN
  IF CurrComponent^.DI.Modules = NIL THEN
    NEW(CurrComponent^.DI.Modules, no);
    IF CurrComponent^.DI.Modules = NIL THEN
      Error(msg.Not_enough_memory);
    END;
  ELSE
    IF HIGH(CurrComponent^.DI.Modules^)+1 # no THEN
      DISPOSE(CurrComponent^.DI.Modules);
      NEW(CurrComponent^.DI.Modules, no);
      IF CurrComponent^.DI.Modules = NIL THEN
        Error(msg.Not_enough_memory);
      END;
    END;
  END;
  FOR i := 0 TO no-1 DO
    CurrComponent^.DI.Modules^[i] := dt.EmptyModule;
  END;
END AllocateModules;

VAR
  DebugInfo: RAW_DEBUG_INFO;
  Exec_Info: kt.EXEC_INFO;
  rpos: CARDINAL;

<* PUSH *>
<* -CHECKINDEX  *>
<* -CHECKDINDEX *>
<* -CHECKRANGE  *>
<* -CHECKNIL    *>

PROCEDURE get1(): sys.CARD8;
BEGIN
  INC(rpos);
  RETURN DebugInfo^[rpos-1];
END get1;

PROCEDURE get2(): sys.CARD16;
VAR
  zzz: POINTER TO sys.CARD16;
BEGIN
  INC(rpos, 2);
  zzz := sys.ADR(DebugInfo^[rpos-2]);
  RETURN zzz^;
END get2;

PROCEDURE get4(): sys.CARD32;
VAR
  zzz: POINTER TO sys.CARD32;
BEGIN
  INC(rpos, 4);
  zzz := sys.ADR(DebugInfo^[rpos-4]);
  RETURN zzz^;
END get4;

PROCEDURE getN(VAR zzz: ARRAY OF sys.BYTE);
BEGIN
  sys.MOVE(sys.ADR(DebugInfo^[rpos]), sys.ADR(zzz), HIGH(zzz)+1);
  INC(rpos, HIGH(zzz)+1);
END getN;

PROCEDURE getxN(x: CARDINAL; VAR zzz: ARRAY OF sys.BYTE);
BEGIN
  sys.MOVE(sys.ADR(DebugInfo^[rpos]), sys.ADR(zzz), x);
  INC(rpos, x);
END getxN;


TYPE
  NAME = xs.String;

PROCEDURE get_name(VAR name: ARRAY OF CHAR);
VAR
  len: CARDINAL;
BEGIN
  IF ExtendedFormat THEN
    len := get2();
  ELSE
    len := get1();
  END;
  sys.MOVE(sys.ADR(DebugInfo^[rpos]), sys.ADR(name), len);
  name[len] := 0C;
  INC(rpos, len);
END get_name;


PROCEDURE getLen(): CARDINAL;
BEGIN
  IF DebugInfo^[rpos] >= 80H THEN
    INC(rpos,2);
    RETURN (DebugInfo^[rpos-2]-80H) * VAL(CARDINAL, 256) + DebugInfo^[rpos-1];
  ELSE
    INC(rpos);
    RETURN DebugInfo^[rpos-1];
  END;
END getLen;

<* POP *>

TYPE
  DNT_ENTRY = RECORD
                type : sys.CARD16;
                modNo: sys.CARD16;
                off  : CARDINAL;
                size : CARDINAL;
              END;


PROCEDURE Read_sstModules(mod: CARDINAL);
TYPE
  SST_MODULE  = RECORD
                  CS_Base : sys.CARD16;
                  CS_Offs : sys.CARD32;
                  CS_Len  : sys.CARD32;
                  Ovl_No  : sys.CARD16;
                  Lib_Ind : sys.CARD16; (* 0 if non-library module *)
                  Segs_No : sys.CARD8;  (* 0 or 1 - one seg, n > 1 = n-1 additional segs *)
                  Reserved: sys.CARD8;
                  Style   : ARRAY [0..1] OF CHAR; (* 'CV' or 'HL' *)
                  Version : ARRAY [0..1] OF sys.CARD8;
                  Name_Len: sys.CARD8;
                END;
VAR
  module: SST_MODULE;
  name  : NAME;
  N     : CARDINAL;
  i     : CARDINAL;
  nm    : xs.txt_ptr;
  save  : CARDINAL;
  t1    : CARDINAL;
  t2    : CARDINAL;
  t3    : CARDINAL;
  dotwas: BOOLEAN;
BEGIN
  getN(module);
  getxN(module.Name_Len, name);
  name[module.Name_Len] := 0C;
  WITH CurrComponent^.DI.Modules^[mod] DO
    i := LENGTH(name);
    nm := sys.ADR(name);
    dotwas := FALSE;
    LOOP
      IF i = 0 THEN EXIT END;
      DEC(i);
      CASE name[i] OF
      | '.':
        IF NOT dotwas THEN
          name[i] := 0C;
          dotwas := TRUE;
        END;
      | '\', '/':
        IF dotwas THEN
          nm := sys.ADR(name[i+1]);
          EXIT;
        END;
      ELSE
      END;
    END;
    ModuleName := bld.AddName (nm^);

    IF module.CS_Base > 0 THEN
      IF (module.Segs_No = 0) OR (module.Segs_No = 1) THEN
        IF module.CS_Len > 0 THEN
          NEW(Segments, 1);
          WITH Segments^[0] DO
            Name := 0;
            Begin := Exec_Info.Objects^[module.CS_Base-1].Begin + module.CS_Offs;
            Sz    := module.CS_Len;
          END;
        END;
      ELSE
        save := rpos;
        IF module.CS_Len = 0 THEN
          N := 0;
        ELSE
          N := 1;
        END;
        FOR i := 1 TO module.Segs_No-1 DO
          INC (rpos, 2+4);
          IF get4() # 0 THEN
            INC (N);
          END
        END;
        IF N > 0 THEN
          NEW (Segments, N);
          rpos := save;
          IF module.CS_Len = 0 THEN
            N := 0;
          ELSE
            WITH Segments^[0] DO
              Name := 0;
              Begin := Exec_Info.Objects^[module.CS_Base-1].Begin + module.CS_Offs;
              Sz    := module.CS_Len;
            END;
            N := 1;
          END;
          FOR i := 1 TO module.Segs_No-1 DO
            t1 := get2();
            t2 := get4();
            t3 := get4();
            IF t3 # 0 THEN
              WITH Segments^[N] DO
                Name := 0;
                Begin := Exec_Info.Objects^[t1-1].Begin + t2;
                Sz    := t3;
              END;
              INC (N);
            END;
          END;
        END;
      END;
    END;

    Active  := TRUE;
  END;
END Read_sstModules;


--------------------- Чтение типов -------------------------

CONST
  N_IMAGE_ITEM = 64;

TYPE
  EXTERNAL_INDEX = sys.CARD16;

  INTERNAL_INDEX = dt.R_TYPE;

  RACCORDING = RECORD
                 Index: INTERNAL_INDEX;
                 Flag : sys.BOOL8;
               END;
  ACCORDING = POINTER TO ARRAY OF RACCORDING;


CONST
  INITIAL_SIZE = (MAX(EXTERNAL_INDEX)+1) DIV 16 - 512;
  FULL_SIZE    = (MAX(EXTERNAL_INDEX)+1);
  HALF_SIZE    = FULL_SIZE DIV 2;

  EMPTY_RACCORDING = RACCORDING {MAX(INTERNAL_INDEX), FALSE};


VAR
  According   : ACCORDING;
  CurrentIndex: EXTERNAL_INDEX;




<* PUSH *>
<* CHECKINDEX- *>
<* CHECKDINDEX- *>
<* CHECKNIL- *>

PROCEDURE PrepareForType (i: EXTERNAL_INDEX);
VAR
  tmp : ACCORDING;
  inx : CARDINAL;
  high: CARDINAL;
BEGIN
  IF According = NIL THEN
    -- заранее выделяется таблица, в большинстве случаев достаточного размера
    IF i <= INITIAL_SIZE THEN
      NEW (According, INITIAL_SIZE);
    ELSE
      IF i < HALF_SIZE THEN
        NEW (According, i*2);
      ELSE
        NEW (According, FULL_SIZE);
      END;
    END;
    FOR inx := 0 TO HIGH(According^) DO
      According^[inx] := EMPTY_RACCORDING;
    END;
  ELSE
    high := HIGH(According^);
    IF high < i THEN
      IF i < HALF_SIZE THEN
        NEW (tmp, i*2);
      ELSE
        NEW (tmp, FULL_SIZE);
      END;
      sys.MOVE (sys.ADR(According^), sys.ADR(tmp^), SIZE(According^));
      FOR inx := high+1 TO HIGH(tmp^) DO
        tmp^[inx] := EMPTY_RACCORDING;
      END;
      DISPOSE (According);
      According := tmp;
    END;
  END;
END PrepareForType;


PROCEDURE AssignAccording (i: EXTERNAL_INDEX; data: RACCORDING);
BEGIN
  PrepareForType (i);
  According^[i] := data;
END AssignAccording;


<* POP *>




TYPE
  TYPE_IMAGE = POINTER TO ARRAY OF sys.CARD8;

  P_RECORD = POINTER TO dt.TYPE_RECORD;
  P_ENUM   = POINTER TO dt.TYPE_ENUM;
  P_CLASS  = POINTER TO dt.TYPE_CLASS;

  P_RECFIELDS_TYPE    = POINTER TO ARRAY [0..10000H] OF dt.TYPE_RECORD_FIELD;
  P_ENUMITEMS_TYPE    = POINTER TO ARRAY [0..10000H] OF dt.TYPE_ENUM_ITEM;

VAR
  TypeImage         : TYPE_IMAGE;
  RecordTypeImage   : P_RECORD;
  EnumTypeImage     : P_ENUM;
  ClassTypeImage    : P_CLASS;
  RecFieldsImage    : P_RECFIELDS_TYPE;
  EnumItemsImage    : P_ENUMITEMS_TYPE;


PROCEDURE AllocateItems (tag: dt.TYPE_TAG; C: CARDINAL);
VAR
  N, I, S: CARDINAL;
BEGIN
  CASE tag OF
  | dt.Record:
    N := SIZE(dt.TYPE_RECORD);
    I := SIZE(dt.TYPE_RECORD_FIELD);
  | dt.Class:
    N := SIZE(dt.TYPE_CLASS);
    I := SIZE(dt.TYPE_RECORD_FIELD);
  | dt.Enum:
    N := SIZE(dt.TYPE_ENUM);
    I := SIZE(dt.TYPE_ENUM_ITEM);
  END;
  C := ((C DIV N_IMAGE_ITEM)+1)*N_IMAGE_ITEM;
  S := N+I*C;
  IF TypeImage = NIL THEN
    NEW(TypeImage, S);
  ELSIF HIGH(TypeImage^)+1 < S THEN
    DISPOSE(TypeImage);
    NEW(TypeImage, S);
  END;
  CASE tag OF
  | dt.Record:
    RecordTypeImage := sys.ADR(TypeImage^);
    RecFieldsImage  := sys.ADDADR(RecordTypeImage, N);
  | dt.Class:
    ClassTypeImage := sys.ADR(TypeImage^);
    RecFieldsImage := sys.ADDADR(ClassTypeImage, N);
  | dt.Enum:
    EnumTypeImage  := sys.ADR(TypeImage^);
    EnumItemsImage := sys.ADDADR(EnumTypeImage, N);
  END;
END AllocateItems;


PROCEDURE GetType(inx: CARDINAL):  INTERNAL_INDEX;
BEGIN
  IF inx < 100H THEN
    CASE inx OF
    | 80H .. 82H:
      RETURN dt.STD_TYPE_INT8  + inx - 80H;
    | 84H .. 86H:
      RETURN dt.STD_TYPE_CARD8 + inx - 84H;
    | 88H .. 8AH:
      RETURN dt.STD_TYPE_REAL  + inx - 88H;
    | 8CH .. 8DH:
      RETURN dt.STD_TYPE_COMPLEX  + inx - 8CH;
    | 90H .. 92H:
      RETURN dt.STD_TYPE_BOOLEAN8  + inx - 90H;
    | 94H .. 96H:
      RETURN dt.STD_TYPE_CHAR8  + inx - 94H;
    | 97H:
      RETURN dt.STD_TYPE_VOID;
    | 9BH:
      RETURN dt.STD_TYPE_INT64;
    | 9CH:
      RETURN dt.STD_TYPE_CARD64;
    | 0A0H .. 0A2H:
      RETURN dt.STD_POINTERS_BEGIN + dt.STD_TYPE_INT8  + inx - 0A0H;
    | 0A4H .. 0A6H:
      RETURN dt.STD_POINTERS_BEGIN + dt.STD_TYPE_CARD8 + inx - 0A4H;
    | 0A8H .. 0AAH:
      RETURN dt.STD_POINTERS_BEGIN + dt.STD_TYPE_REAL  + inx - 0A8H;
    | 0ACH .. 0ADH:
      RETURN dt.STD_POINTERS_BEGIN + dt.STD_TYPE_COMPLEX  + inx - 0ACH;
    | 0B0H .. 0B2H:
      RETURN dt.STD_POINTERS_BEGIN + dt.STD_TYPE_BOOLEAN8  + inx - 0B0H;
    | 0B4H .. 0B6H:
      RETURN dt.STD_POINTERS_BEGIN + dt.STD_TYPE_CHAR8  + inx - 0B4H;
    | 0B7H:
      RETURN dt.STD_TYPE_ADDRESS + inx - 0B7H;
    | 0BBH:
      RETURN dt.STD_POINTERS_BEGIN + dt.STD_TYPE_INT64;
    | 0BCH:
      RETURN dt.STD_POINTERS_BEGIN + dt.STD_TYPE_CARD64;
    ELSE
      RETURN dt.STD_TYPE_VOID;
    END;
  ELSIF inx >= 512 THEN
    -- здесь не нужно делать PrepareForType (inx-512), так все типы, кроме
    -- указателей ссылаются только назад, на описанные типы, так что они
    -- обязаны быть в такблице. Для указателей делается трюк, который
    -- позволяет это ограничение обойти (см. разбор типа Pointer)
    RETURN According^[inx-512].Index;
  END;
  ASSERT(FALSE);
END GetType;


PROCEDURE GetFID_span (): CARDINAL;
VAR
  FID_span: sys.CARD8;
BEGIN
  FID_span := get1();
  CASE FID_span OF
  | 08BH : RETURN get1();
  | 085H : RETURN get2();
  | 086H : RETURN get4();
  ELSE
    ASSERT(FALSE);
  END;
END GetFID_span;


PROCEDURE Read_sstTypes(mod, size: CARDINAL);

  PROCEDURE RecalcTypeNumbers (internal_index: CARDINAL);
  VAR
    new: INTERNAL_INDEX;
    type: dt.PTYPE;
    PPointerType  : POINTER TO dt.TYPE_POINTER;

  BEGIN
    -- возможно, внешний тип еще не был описан в таблице
    PrepareForType (CurrentIndex-512);
    WITH According^[CurrentIndex-512] DO
      IF Flag THEN
        -- внешний тип еще не описан, но на него есть внутренняя ссылка
        -- из какого-то другого внутреннего типа
        new := Index;
        REPEAT
          -- new содержит индекс внутреннего типа, ссылающигося на
          -- текущий внешний тип, поэтому индекс должен быть правильным
          ASSERT (new # MAX(INTERNAL_INDEX));
          type :=  new;
          PPointerType := tls.TypeImage (type);
          -- очевидно, что такой внутренний тип может быть только указателем
          ASSERT (PPointerType^.TypeData.Tag = dt.Pointer);
          -- возможно, new будет содержать индекс внутреннего типа, так же
          -- указателя, ссылающигося на этот же самый текущий внешний тип,
          -- но это не может быть указатель на внутренний тип
          new := PPointerType^.Base;
          -- теперь установим правильно ссылку внутреннего типа
          -- на внутренний текущий тип
          PPointerType^.Base := internal_index;
          -- попробуем еще раз, так как таких внутренних типов (указателей)
          -- на текущий внешний может быть несколько
        UNTIL new = MAX(INTERNAL_INDEX);
        -- больше не должно оказаться ссылок на этот тип, так как все
        -- остальные ссылки из внешних типов могут быть только для указателей
        Flag := FALSE;
      END;
      Index := internal_index;
    END;
  END RecalcTypeNumbers;


VAR
  len, beg, tag, i, b, n, save: CARDINAL;
  before: BOOLEAN;

  TQual: sys.CARD8;

  RecordType    : dt.TYPE_RECORD;
  EnumType      : dt.TYPE_ENUM;
  ClassType     : dt.TYPE_CLASS;
  RangeType     : dt.TYPE_RANGE;
  ArrayType     : dt.TYPE_ARRAY;
  ArrayOfType   : dt.TYPE_ARRAY_OF;
  OpenArray     : dt.TYPE_OPEN_ARRAY;
  SetType       : dt.TYPE_SET;
  PointerType   : dt.TYPE_POINTER;
  ReferenceType : dt.TYPE_REFERENCE;
  PClassType    : POINTER TO dt.TYPE_CLASS;
  PRecordType   : POINTER TO dt.TYPE_RECORD;
  ProcType      : dt.TYPE_PROCEDURE;

  name: NAME;
  type: dt.PTYPE;
  IsUnique: BOOLEAN;
  field_name: NAME;
  type_tag: dt.TYPE_TAG;

BEGIN
<* PUSH *>
<* CHECKINDEX- *>
<* CHECKDINDEX- *>
<* CHECKNIL- *>
  -- не используем AssignAccording для увеличения скорости
  -- 1) первый раз таблица должны быть пустой, поэтому CurrentIndex равен 0
  -- N) в следующие разы нужно рассписывать только до последнего занятого типа
  FOR i := 1 TO CurrentIndex DO
    According^[i-1] := EMPTY_RACCORDING;
  END;
<* POP *>

  beg := rpos;
  CurrentIndex := 511;             (* Первый непримитивный тип имеет индекс 512 *)
  WHILE size > rpos - beg DO
    INC (CurrentIndex); -- номер текущего внешнего типа
    INC (rpos, 2);
    tag := get1();
    CASE tag OF
    | 06FH : (* Range *)
        WITH RangeType DO
          TypeData.Tag := dt.Range;
          TypeData.Com := CurrCom;
          TypeData.Mod :=  mod + 1;
          ASSERT (get1()=0);                  (* TQual=0                      *)
          Base := GetType(get2());
          CASE get1() OF
          | 085H : Min := VAL(CARDINAL, get2());
          | 086H : Min := VAL(CARDINAL, get4());
          | 088H : Min := VAL(INTEGER,  sys.INT8(get1()));
          | 089H : Min := VAL(INTEGER,  sys.INT16(get2()));
          | 08AH : Min := sys.INT32(get4());
          | 08BH : Min := VAL(CARDINAL, get1());
          END;
          CASE get1() OF
          | 085H : Max := VAL(CARDINAL, get2());
          | 086H : Max := VAL(CARDINAL, get4());
          | 088H : Max := VAL(INTEGER,  sys.INT8(get1()));
          | 089H : Max := VAL(INTEGER,  sys.INT16(get2()));
          | 08AH : Max := sys.INT32(get4());
          | 08BH : Max := VAL(CARDINAL, get1());
          END;
          IF CARDINAL(Max) = MAX(CARDINAL) THEN
            Max := CARDINAL(Max) - 1;
          END;
          get_name(name);
          TypeData.Name := 0;
        END;
        IsUnique := NOT opt.MergeEqualTypes;
        type := bld.AddType ( RangeType, IsUnique);
        RecalcTypeNumbers (type);
        IF IsUnique THEN
          ASSERT(tls.RenameType (type, name));
        END;

    | 078H : (* Array *)
        WITH ArrayType DO
          TQual := get1();
          TypeData.Com := CurrCom;
          TypeData.Mod :=  mod + 1;
          CASE TQual OF
          | 0:                                (* ARRAY [..]        *)
            ASSERT( get4()#0 );               (* Size in bytes     *)
            TypeData.Tag := dt.Array;
          | 08H:                              (* OPEN ARRAY OF ... *)
            ASSERT( get4()=0 );               (* Size in bytes     *)
            TypeData.Tag := dt.OpenArray;
          | 0CH:                              (* ARRAY OF ...      *)
            ASSERT( get4()=0 );               (* Size in bytes     *)
            TypeData.Tag := dt.Array_of;
          END;
          ASSERT( get1() = 083H );
          Index := GetType(get2());
          ASSERT( get1() = 083H );
          Base := GetType(get2());
          ASSERT( get1() = 082H );
          get_name(name);
          TypeData.Name := 0;
        END;
        CASE TQual OF
        | 0:
          IsUnique := NOT opt.MergeEqualTypes;
          type := bld.AddType ( ArrayType, IsUnique);
          RecalcTypeNumbers (type);
          IF IsUnique THEN
            ASSERT(tls.RenameType (type, name));
          END;
        | 08H:
          ASSERT(ArrayType.Index = dt.STD_TYPE_VOID);
          WITH OpenArray DO
            TypeData := ArrayType.TypeData;
            Base := ArrayType.Base;
            WITH Length DO
              Tag   := dt.Sy_Relative;
              Name  := 0;
              Type  := dt.STD_TYPE_CARD32;
              ST_ID := dt.st_original;
              WITH DataRel DO
                RegNo    := kt.FRAME_REG;
                Relative := 0;
                Attrib   := dt.SYM_ATTRIB{dt.SA_Param};
              END;
            END;
          END;
          IsUnique := NOT opt.MergeEqualTypes;
          type := bld.AddType ( OpenArray, IsUnique);
          RecalcTypeNumbers (type);
          IF IsUnique THEN
            ASSERT(tls.RenameType (type, name));
          END;
        | 0CH:
          ASSERT(ArrayType.Index = dt.STD_TYPE_VOID);
          WITH ArrayOfType DO
            TypeData := ArrayType.TypeData;
            Base     := ArrayType.Base;
          END;
          IsUnique := NOT opt.MergeEqualTypes;
          type := bld.AddType ( ArrayOfType, IsUnique);
          RecalcTypeNumbers (type);
          IF IsUnique THEN
            ASSERT(tls.RenameType (type, name));
          END;
        END;

    | 079H : (* Record *)
      WITH RecordType DO
        TypeData.Tag := dt.Record;
        TypeData.Com := CurrCom;
        TypeData.Mod :=  mod + 1;
        ASSERT( get1()=0 );                  (* TQual=0                      *)
        Length := get4();                    (* Size in bytes                *)
        Fields := get2();
        AllocateItems (dt.Record, Fields);   (* Allocate for fields *)
        ASSERT (get1() = 083H);
        ASSERT (CurrentIndex+1 = get2());   (* Индекс типов                 *)
        ASSERT (get1() = 083H);
        ASSERT (CurrentIndex+2 = get2());   (* Индекс имен                  *)
        ASSERT (get1() = 082H);
        get_name(name);
        TypeData.Name := 0;
        INC(rpos, 2);
        ASSERT( get1() = 07FH );
        ASSERT( get1()=1 );                  (* TQual=1                      *)
        FOR i := 1 TO Fields DO
          ASSERT( get1() = 083H );
          RecFieldsImage^[i-1].FieldType := GetType(get2());
        END;
        INC(rpos, 2);
        ASSERT( get1() = 07FH );
        ASSERT( get1()=2 );                  (* TQual=2                      *)
        FOR i := 1 TO Fields DO
          ASSERT( get1() = 082H );
          get_name(field_name);
          WITH RecFieldsImage^[i-1] DO
            FieldName := bld.AddName (field_name);
            FieldOffs := GetFID_span();
            FieldSTID := dt.st_original;
          END;
        END;
      END;
      RecordTypeImage^ := RecordType;
      IsUnique := NOT opt.MergeEqualTypes;
      type := bld.AddType ( TypeImage^, IsUnique);
      RecalcTypeNumbers (type);
      IF IsUnique THEN
        ASSERT(tls.RenameType (type, name));
      END;
      INC (CurrentIndex, 2);

    | 040H : (* Class *)
      WITH ClassType DO
        TypeData.Tag := dt.Class;
        TypeData.Com := CurrCom;
        TypeData.Mod :=  mod + 1;
        Base := 0;
        ASSERT( get1() = 1 );                (* TQual = 1 - is_struct        *)
        Length := get4();                    (* Size in bytes                *)
        MyMembers := get2();
        AllMembers := MyMembers;
        AllocateItems (dt.Class, MyMembers);
        ASSERT (CurrentIndex+1 = get2());   (* Индекс элементов класса      *)
        get_name(name);
        TypeData.Name := 0;
        INC(rpos, 2);
        ASSERT( get1() = 07FH );
        ASSERT( get1()=1 );                  (* TQual=1                      *)
        FOR i := 1 TO MyMembers DO
          ASSERT (get1() = 083H);
          ASSERT (get2() = CurrentIndex+1 + i);
        END;
        IF MyMembers > 0 THEN
          save := rpos;
          INC(rpos, 2);
          IF get1() = 041H THEN
            DEC(MyMembers);
            DEC(AllMembers);
            ASSERT (get1() = 0);  (* TQual = 0           *)
            ASSERT (get1() = 2);  (* Protection = Public *)
            Base := GetType (get2 ());
            type := Base;
            ASSERT (tls.TypeTag (type, type_tag));
            IF type_tag = dt.Class THEN
              PClassType := tls.TypeImage (type);
              AllMembers := MyMembers + PClassType^.AllMembers;
            ELSE
              -- в Обероне разрешено расширять классы записями
              ASSERT (type_tag = dt.Record);
              PRecordType := tls.TypeImage (type);
              AllMembers := MyMembers + PRecordType^.Fields;
            END;
            ASSERT( GetFID_span() = 0 );
          ELSE
            rpos := save;
          END;
          FOR i := 1 TO MyMembers DO
            INC(rpos, 2);
            WITH RecFieldsImage^[i-1] DO
              ASSERT( get1() = 046H );
              ASSERT( get1() = 0 );  (* TQual = 0           *)
              ASSERT( get1() = 2 );  (* Protection = Public *)
              FieldType := GetType( get2() );
              FieldOffs := GetFID_span();
              FieldSTID := dt.st_original;
              get_name(field_name);
              ASSERT(field_name = '' );
              get_name(field_name);
              FieldName := bld.AddName (field_name);
            END;
          END;
        END;
      END;
      ClassTypeImage^ := ClassType;
      IsUnique := NOT opt.MergeEqualTypes;
      type := bld.AddType ( TypeImage^, IsUnique);
      RecalcTypeNumbers (type);
      IF IsUnique THEN
        ASSERT(tls.RenameType (type, name));
      END;
      INC (CurrentIndex, 1+ClassType.MyMembers);
      IF ClassType.Base # 0 THEN
        INC (CurrentIndex);
      END;

    | 052H :  (* Set *)
        WITH SetType DO
          TypeData.Tag := dt.Set;
          TypeData.Com := CurrCom;
          TypeData.Mod :=  mod + 1;
          ASSERT( get1() = 0 );
          Base := GetType( get2() );
          get_name(name);
          TypeData.Name := 0;
        END;
        IsUnique := NOT opt.MergeEqualTypes;
        type := bld.AddType ( SetType, IsUnique);
        RecalcTypeNumbers (type);
        IF IsUnique THEN
          ASSERT(tls.RenameType (type, name));
        END;

    | 07BH: (* Enumeration *)
      WITH EnumType DO
        TypeData.Tag := dt.Enum;
        TypeData.Com := CurrCom;
        TypeData.Mod :=  mod + 1;
        ASSERT (get1() = 0);
        ASSERT (get1() = 083H);
        Base := GetType(get2());
        ASSERT (get1() = 083H);
        ASSERT (CurrentIndex+1 = get2());
        ASSERT (GetFID_span() = 0);
        Quantity := GetFID_span()+1;
        AllocateItems (dt.Enum, Quantity);
        ASSERT( get1() = 082H );
        get_name(name);
        TypeData.Name := 0;
        INC(rpos,2);
        ASSERT( get1() = 07FH );
        ASSERT( get1()=3 );                  (* TQual=1                      *)
        FOR i := 1 TO Quantity DO
          ASSERT( get1() = 082H );
          get_name(field_name);
          WITH EnumItemsImage^[i-1] DO
            EnumName  := bld.AddName (field_name);
            EnumValue := GetFID_span();
            bld.AddEnumeration (CurrCom, mod+1, EnumName, EnumValue);
          END;
        END;
      END;
      EnumTypeImage^ := EnumType;
      IsUnique := NOT opt.MergeEqualTypes;
      type := bld.AddType ( TypeImage^, IsUnique);
      RecalcTypeNumbers (type);
      IF IsUnique THEN
        ASSERT(tls.RenameType (type, name));
      END;
      INC (CurrentIndex);

    | 048H : (* Reference - means VAR parameter *)
        WITH ReferenceType DO
          TypeData.Tag := dt.Reference;
          TypeData.Name := 0;
          TypeData.Com := CurrCom;
          TypeData.Mod :=  mod + 1;
          ASSERT (get1() = 0);
          Base := get2();
          ASSERT (Base < CurrentIndex);
          Base := GetType(Base);
        END;
        IsUnique := NOT opt.MergeEqualTypes;
        type := bld.AddType (ReferenceType, IsUnique);
        RecalcTypeNumbers (type);

    | 07AH : (* Pointer *)
        WITH PointerType DO
          TypeData.Tag := dt.Pointer;
          TypeData.Com := CurrCom;
          TypeData.Mod :=  mod + 1;
          ASSERT( get1()=1 );      (* TQual = 1 - 32bit near *)
          ASSERT( get1()=083H );
          Length := 4;
          Base := get2();
          IF Base < CurrentIndex THEN
            -- базовый тип описан, ссылка назад
            before := TRUE;
            -- не нужно устанавливать соотвествие
            b := 0;
            -- так как тип просто можно получить
            Base := GetType(Base);
          ELSE
            -- базовый тип еще не описан, ссылка вперед,
            -- но указетель может так же ссылаться сам на себя
            before := NOT (Base = CurrentIndex);
            -- запомним внешний базовый тип (без примитивных)
            b := Base-512;
            -- возможно, этого типа еще нет, так что подготовим таблицу
            PrepareForType (b);
            -- теперь мы либо получим в базе индекс другого указателя,
            -- который тоже ссылается на этот же, либо сошлемся на базовый
            -- тип первый раз, что и проверим ниже
            Base := According^[b].Index;
            IF NOT According^[b].Flag THEN
              -- нет ссылок, не должно быть и внутреннего индекса
              ASSERT (According^[b].Index = MAX(INTERNAL_INDEX));
            END;
          END;
          ASSERT (get1()=082H);
          get_name(name);
          TypeData.Name := bld.AddName (name);
        END;
        IsUnique := TRUE;
        n := bld.AddType ( PointerType, IsUnique);
        IF before THEN
          -- базовый описан, поэтому уже можно пересчитать ссылки для текущего
          RecalcTypeNumbers(n);
        END;
        IF b # 0 THEN
          -- базовый тип еще был не описан, ссылка вперед
          -- нужно установть проверку соответствия на внешний базовый тип
          AssignAccording (b, RACCORDING {n, TRUE});
        END;
        IF NOT before THEN
          -- так как теперь установлена проверка соответствия на внешний
          -- базовый тип, то можно пересчитывать ссылки и для текущего
          RecalcTypeNumbers(n);
        END;

    | 075H : (* Procedure *)
        WITH ProcType DO
          TypeData.Tag := dt.Procedure;
          TypeData.Com := CurrCom;
          TypeData.Mod :=  mod + 1;
          fmt.print (name, "Procedure_0x%x", CurrentIndex);
          TypeData.Name := 0;
          CallingConv := get1();
          ParamCount  := get2();
          ASSERT( get2()=ParamCount );
          ASSERT( get1()=083H );
          ResultType := GetType(get2());
          ASSERT (get1()=083H);
          ASSERT (CurrentIndex+1 = get2());
          len := get2();
          ASSERT( len=2 );
          ASSERT( get1() = 07FH );
          ASSERT( get1()=4 );                  (* TQual=4 Args list *)
        END;
        IsUnique := NOT opt.MergeEqualTypes;
        type := bld.AddType ( ProcType, IsUnique);
        RecalcTypeNumbers (type);
        IF IsUnique THEN
          ASSERT(tls.RenameType (type, name));
        END;
        INC (CurrentIndex, 1);

    ELSE
<* IF DEFINED (xd_debug) & xd_debug THEN *>
      printf("Unknown tag 0x%x", tag);
<* END *>
      ASSERT(FALSE);
    END;
  END;
  DEC (CurrentIndex, 511);
END Read_sstTypes;

PROCEDURE Read_sstPublic(size: CARDINAL);
VAR
  beg : CARDINAL;
  name: ARRAY [0..255] OF CHAR;
  pbl:  RECORD
          offs: CARDINAL;
          seg, type: sys.CARD16;
        END;
  public: dt.PUBLIC;
BEGIN
  beg := rpos;
  WHILE size > rpos - beg DO
    getN(pbl);
    get_name(name);
    IF (0 < pbl.seg) AND (pbl.seg <= Exec_Info.N_Objects) THEN
      public.code := pbl.seg = Exec_Info.Code_Object+1;
      public.name := bld.AddName (name);
      public.len := 1;
      public.addr := pbl.offs + Exec_Info.Objects^[pbl.seg-1].Begin;
      bld.AddPublic (CurrCom, public);
    END;
  END;
END Read_sstPublic;



PROCEDURE Read_sstSymbols(mod, size: CARDINAL; mode: PROCESS_MODE);

  PROCEDURE SpecifyOpenArrayType (relative: LONGCARD; type: dt.PTYPE);
  VAR
    POpenArray: POINTER TO dt.TYPE_OPEN_ARRAY;
    i, dim    : CARDINAL;
    tag       : dt.TYPE_TAG;
  BEGIN
    dim := tls.ArrayDim (type);
    FOR i := 1 TO dim DO
      ASSERT(tls.TypeTag(type, tag) & (tag = dt.OpenArray));
      POpenArray := tls.TypeImage (type);
      WITH POpenArray^ DO
        IF Length.Tag = dt.Sy_Relative THEN
          Length.DataRel.Relative := relative+i*4;
        END;
      END;
      tls.SubType (type, type);
    END;
  END SpecifyOpenArrayType;

CONST
  Proc        = 1;
  End         = 2;
  StaticVar   = 5;

  LocalVarAuto             = 04H; -- var location
  LocalVarInRegister       = 0DH; -- var location
  LocalVarRelativeRegister = 20H; -- var location


VAR
  len : CARDINAL;
  type: CARDINAL;
  name: NAME;
  beg: CARDINAL;
  ptype: dt.PTYPE; tag: dt.TYPE_TAG;
  Object: dt.RAW_OBJECT;
  proc_level: CARDINAL;
  seg_no: CARDINAL;
  Skip: BOOLEAN;
  StoredLevel: CARDINAL;
  save : CARDINAL;
BEGIN
  IF (mode = mode_brief) AND
     (CurrComponent^.DI.Modules^[mod].ModuleObjects.RawObjects.Count # 0)
  THEN
    RETURN
  END;
  beg := rpos;
  Skip := FALSE;
  StoredLevel := MAX(CARDINAL);
  proc_level := 0;
  WHILE size > rpos - beg DO
    len  := getLen();
    save := rpos;
    type := get1();
    ASSERT((type # 0) AND (len # 0));
    CASE type OF
    | Proc:
      INC(proc_level);
      IF NOT Skip THEN
        WITH Object DO
          Tag := dt.Sy_Proc;
          ST_ID := dt.st_original;
          DataProc.Address := get4();
          seg_no := get2();
          Skip := (seg_no = 0);
          IF Skip THEN
            StoredLevel := proc_level-1;
          ELSE
            INC(DataProc.Address, Exec_Info.Objects^[seg_no-1].Begin);
            Type := dt.STD_TYPE_VOID;
            DataProc.Length    := get4();
            DataProc.Begin     := get2();
            DataProc.End       := get4();
            ASSERT(get2() = 0);  -- Class Type
            ASSERT(get1() = 8);  -- 32 bit near;
            get_name(name);
            ASSERT(name#'');
            --Write(name);
            Name := bld.AddName (name);
            IF get1 () = 0EAH THEN  -- EA_PROC  Procedure extended attributes
              ASSERT (get1 () = 8); -- length of extended attributes
              DataProc.HasFrame := get4 () = 1;
              DataProc.FrameSize := get4 ();
            ELSE
              DataProc.HasFrame := FALSE;
              DataProc.FrameSize := 0;
            END;
           <* IF DEFINED (xd_debug) & xd_debug THEN *>
            -- printf ("%-20s hasFrame=%d frameSize=%d\n", name, DataProc.HasFrame, DataProc.FrameSize);
           <* END *>
            bld.AddObject (CurrCom, mod+1, Object);
          END;
        END;
      END;

    | End:
      ASSERT( proc_level#0 );
      DEC(proc_level);
      IF NOT Skip THEN
        Object.Tag := dt.Sy_End_Block;
        Object.Name := 0;
        bld.AddObject (CurrCom, mod+1, Object);
      ELSIF (proc_level = StoredLevel) THEN
        Skip := FALSE;
      END;
      -- INC (rpos, len-1);
    ELSE
      IF mode = mode_full THEN
        CASE type OF
        | StaticVar:
          ASSERT( proc_level=0 );
          WITH Object DO
            ST_ID := dt.st_original;
            Tag := dt.Sy_Var;
            DataVar.Address := get4();
            seg_no      := get2();
            IF seg_no # 0 THEN
              INC(DataVar.Address, Exec_Info.Objects^[seg_no-1].Begin);
            END;
            type := get2();
            Type := GetType(type);
            get_name(name);
            --Write(name);
            --Write(' type = %x',type);
            IF NOT Skip AND (seg_no # 0) AND (name # '') THEN
              Name := bld.AddName (name);
              bld.AddObject (CurrCom, mod+1, Object);
            END;
          END;

        | LocalVarAuto:
          ASSERT( proc_level#0 );
          WITH Object DO
            ST_ID := dt.st_original;
            Tag := dt.Sy_Relative;
            DataRel.RegNo := kt.FRAME_REG;
            DataRel.Relative := LONGINT(get4());
            type := get2();
            get_name(name);
            IF NOT Skip AND (name # '') THEN
              Type  := GetType(type);
              ptype := Object.Type;
              IF tls.TypeTag(ptype, tag) & (tag = dt.Reference) THEN
                DataRel.Attrib := dt.SYM_ATTRIB{dt.SA_Param, dt.SA_Reference};
                tls.SubType(ptype, ptype);
                Type := ptype;
                IF tls.TypeTag(ptype, tag) & (tag = dt.OpenArray) THEN
                  SpecifyOpenArrayType (DataRel.Relative, ptype);
                END;
              ELSE
                DataRel.Attrib := dt.SYM_ATTRIB{};
              END;
              Name := bld.AddName (name);
              bld.AddObject (CurrCom, mod+1, Object);
            END;
          END;

        | LocalVarRelativeRegister:
          ASSERT( proc_level#0 );
          WITH Object DO
            ST_ID := dt.st_original;
            Tag := dt.Sy_Relative;
            DataRel.RegNo := get2();
            IF DataRel.RegNo = 0FFFFH THEN -- dynamic frame relative
              DataRel.RegNo := kt.FRAME_REG;
            END;
            DataRel.Relative := LONGINT(get4());
            type := get2();
            get_name(name);
            IF NOT Skip AND (name # '') THEN
              Type  := GetType(type);
              ptype := Object.Type;
              IF tls.TypeTag(ptype, tag) & (tag = dt.Reference) THEN
                DataRel.Attrib := dt.SYM_ATTRIB{dt.SA_Param, dt.SA_Reference};
                tls.SubType(ptype, ptype);
                Type := ptype;
                IF tls.TypeTag(ptype, tag) & (tag = dt.OpenArray) THEN
                  SpecifyOpenArrayType (DataRel.Relative, ptype);
                END;
              ELSE
                DataRel.Attrib := dt.SYM_ATTRIB{};
              END;
              Name := bld.AddName (name);
              bld.AddObject (CurrCom, mod+1, Object);
            END;
          END;

        | LocalVarInRegister :
          ASSERT( proc_level#0 );
          WITH Object DO
            ST_ID := dt.st_original;
            Tag := dt.Sy_Register;
            type := get2();
            Type := GetType(type);
            DataReg.RegNo := get1(); (* NB внутренняя нумерация регистров *)
                                     (* совпадает с нумерацией формата    *)
            get_name(name);
            DataReg.Attrib := dt.SYM_ATTRIB{};
            IF NOT Skip AND (name # '') THEN
              Name := bld.AddName (name);
              bld.AddObject (CurrCom, mod+1, Object);
            END;
          END;
        ELSE
          printf('type = %u', type);
          printf('len  = %u',len);
        END;
      END;
    END;
    rpos := save + len;
  END;
  ASSERT (proc_level=0);
  CurrComponent^.DI.Modules^[mod].HasInfo := TRUE;
END Read_sstSymbols;


PROCEDURE Read_sstHLLSrc(mod, size: CARDINAL);
VAR
  type   : CARDINAL;
  N      : CARDINAL;
  i      : CARDINAL;
  beg    : CARDINAL;
  name   : ARRAY [0..255] OF CHAR;
  added  : BOOLEAN;
  N_pair : CARDINAL;
  SegAddr: CARDINAL;
  tmp    : CARDINAL;

BEGIN
  IF CurrComponent^.DI.Modules^[mod].CLTable.CLTable # NIL THEN RETURN END;
  added := FALSE;
  beg := rpos;
  WITH CurrComponent^.DI.Modules^[mod] DO
    N_pair := 0;
    WHILE size > rpos -beg DO
      ASSERT(get2()=0);
      type := get1(); INC(rpos);
      N := get2();    INC(rpos,2);

      CASE type OF
      | 0 :
        INC(N_pair, N);
        ASSERT( N # 0);
        INC(rpos, N*8+4);
      | 3 :
        ASSERT(N = 0);
        INC(rpos, get4())
      END;
    END;

    rpos := beg;
    NEW(CLTable.CLTable, N_pair);

    N_pair := 0;
    WHILE size > rpos -beg DO
      ASSERT(get2()=0);
      type := get1();
      INC(rpos);
      N := get2();

      CASE type OF
      | 0 :
        tmp := get2();
        IF tmp # 0 THEN
          ASSERT(tmp = Exec_Info.Code_Object + 1, tmp);
        END;
        SegAddr := 0;
        WITH Exec_Info.Objects^[Exec_Info.Code_Object]  DO
          tmp := get4();
          IF tmp # 0 THEN
            IF tmp > RelocationBase THEN
              SegAddr := (tmp - RelocationBase) + Begin;
            ELSE
              SegAddr := (tmp + Begin) - RelocationBase;
            END;
          ELSIF Segments # NIL THEN
            SegAddr := Segments^[0].Begin;
            tmp := MAX(CARDINAL);
          END;
        END;
        FOR i := 0 TO N-1 DO
          WITH CLTable.CLTable^[N_pair+i] DO
            Line := get2();
            ASSERT(get2() = 1);
            Addr := get4() + SegAddr;
            IF tmp = 0 THEN
              Addr := 0;
            END;
          END;
        END;

        INC(N_pair, N);
      | 3 :
        INC(rpos, 2 + 4+4+4);
        ASSERT(N = 0);
        ASSERT(get4() = 1);
        get_name(name);
        IF NOT added THEN
          SourceName := bld.AddName (name);
          Language := tls.TryDetectModLang (name);
          added := TRUE;
        END;
      END;
    END;
    HasInfo := TRUE;
  END;
END Read_sstHLLSrc;


PROCEDURE PrepareToReadDirectory(com: ComNo; VAR Component: COMPONENT): CARDINAL;
BEGIN
  WITH Component DO
    DebugInfo := raw;
    Exec_Info := EI;
  END;

  CurrCom       := com;
  CurrComponent := sys.ADR(Component);

  rpos := 4;
  rpos := get4();

  IF (get2() # 8) THEN RETURN 0; END;
  IF get2() # SIZE(DNT_ENTRY) THEN RETURN 0; END;
  RETURN get4();
END PrepareToReadDirectory;


PROCEDURE CorrectSourceName (mod: dt.ModNo);
VAR
  name: xs.txt_ptr;
  i   : CARDINAL;
  del : BOOLEAN;
(*
  dot : BOOLEAN;
  cut : BOOLEAN;
*)
BEGIN
  WITH CurrComponent^.DI.Modules^[mod-1] DO
    name := tls.GetName (SourceName);
    del := name^ = "";
    name := tls.GetName (ModuleName);
    IF del THEN
      SourceName := bld.AddNewName (name^);
    END;
    FOR i := 0 TO LENGTH(name^) DO
      IF name^[i] IN xs.CHARSET { '\', '/' } THEN
        name^[i] := '`';
      END;
    END;

(*
    IF del THEN
      SourceName := bld.AddName (name^);
      name := tls.GetName (CurrCom, SourceName);
    END;
    dot := FALSE;
    cut := FALSE;
    i := LENGTH(name^);
    LOOP
      IF i = 0 THEN EXIT; END;
      DEC(i);
      CASE name^[i] OF
      | '.' :
        IF NOT cut THEN
          name^[i] := 0C;
          dot := TRUE;
        END;
      | '\', '/':
        IF del OR dot THEN
          INC(i);
          EXIT;
        END;
        name^[i] := '`';
        cut := TRUE;
      ELSE
      END;
    END;
    INC(ModuleName, i);
*)
  END;
END CorrectSourceName;


PROCEDURE CorrectSourceNames;
VAR
  mod: CARDINAL;
BEGIN
  WITH CurrComponent^.DI DO
    FOR mod := 1 TO LastModule DO
      CorrectSourceName (mod);
    END;
  END;
END CorrectSourceNames;


PROCEDURE internal_ProcessDebugInfo(com: ComNo; VAR Component: COMPONENT;
                                    mode: PROCESS_MODE): CARDINAL;
VAR
  i    : CARDINAL;
  entry: DNT_ENTRY;
  save : CARDINAL;
  N    : CARDINAL;
  N_mod: CARDINAL;
  ModNo: CARDINAL;

BEGIN
  CurrCom := com;
  Component.DI := dt.EmptyDebugInfo;
  N := PrepareToReadDirectory(com, Component);
  IF N = 0 THEN RETURN 999 END;

  save := rpos;

  (* Строим структуру с информацией о модулях *)

  N_mod := 0;
  (* первый проход: подсчет числа модулей. *)
  FOR i := 0 TO N-1 DO
    getN(entry);
    IF entry.type = 101H THEN INC(N_mod); END;
  END;
  rpos := save;

  AllocateModules(N_mod);

  (* второй проход. заполнение структуры информацией о модулях *)
  IF mode = mode_full THEN
    IF According = NIL THEN
      PrepareForType (INITIAL_SIZE);
      CurrentIndex := 0;
    END;
  ELSE
    CurrentIndex := MAX(EXTERNAL_INDEX);
    According := NIL;
  END;

  CurrComponent^.DI.LastModule := N_mod;

  ModNo := 0;
  N_mod := 0;
  FOR i := 0 TO N-1 DO
    getN(entry);
    save := rpos;
    rpos := entry.off;

    CASE entry.type OF
    | 101H: ModNo := entry.modNo; Read_sstModules(N_mod); INC(N_mod);
    ELSE
      IF mode = mode_full THEN
        CASE entry.type OF
        | 102H: ASSERT(ModNo = entry.modNo); Read_sstPublic(entry.size);
        | 103H: ASSERT(ModNo = entry.modNo); Read_sstTypes(N_mod-1, entry.size);
        | 104H: ASSERT(ModNo = entry.modNo); Read_sstSymbols(N_mod-1, entry.size, mode_full);
        | 10BH: ASSERT(ModNo = entry.modNo); Read_sstHLLSrc(N_mod-1, entry.size);
        ELSE
        END;
      END;
    END;
    rpos := save;
  END;

  CorrectSourceNames;

  (* Создание ключей по модулям *)
  bld.CreateKeysByModules (CurrCom);

  (* Создание ключа для таблицы сегментов *)
  bld.CreateKGroupSegments (CurrCom);

  (* Установка языка компоненты по модулям *)
  bld.SetComLanguage (CurrCom);

  IF mode = mode_full THEN
    DebugInfo := NIL;
  END;

 <* IF DEFINED (xd_debug) & xd_debug THEN *>
  IF opt.Debug(opt.Load) THEN
    bld.ShowDebugInfo (CurrCom);
  END;
 <* END *>

  CurrCom := dt.Invalid_Component;
  CurrComponent := NIL;

  RETURN 0;
EXCEPT
  tls.ClearDebugInfo (CurrCom);
  CurrCom := dt.Invalid_Component;
  CurrComponent := NIL;

  IF exc.IsCurrentSource(source) THEN
    RETURN exc.CurrentNumber(source);
  ELSE
  <* IF DEFINED (xd_debug) & xd_debug THEN *>
    IF NOT opt.Debug(opt.Load) THEN
      RETURN msg.WrongDebugInfo;
    END;
  <* ELSE *>
   RETURN msg.WrongDebugInfo;
  <* END *>
  END;
END internal_ProcessDebugInfo;


PROCEDURE ProcessDebugInfo(com: ComNo; VAR Component: COMPONENT): CARDINAL;
BEGIN
  RETURN internal_ProcessDebugInfo(com, Component, mode_full);
END ProcessDebugInfo;

PROCEDURE ReadModules(com: ComNo; VAR Component: COMPONENT): CARDINAL;
BEGIN
  RETURN internal_ProcessDebugInfo(com, Component, mode_brief);
END ReadModules;

PROCEDURE BuildForMod(modno: CARDINAL; com: ComNo; VAR Component: COMPONENT);
VAR
  N_mod: CARDINAL;
  N    : CARDINAL;
  i    : CARDINAL;
  sect : CARDINAL;
  entry: DNT_ENTRY;
  Ok   : BOOLEAN;
BEGIN
  N := PrepareToReadDirectory(com, Component);
  IF CurrComponent^.DI.Modules^[modno].HasInfo THEN RETURN; END;
  ASSERT(N # 0);
  CurrCom := com;

  Ok := FALSE;
  N_mod := 0;
  FOR i := 0 TO N-1 DO
    getN(entry);
    sect := rpos;
    rpos := entry.off;
    CASE entry.type OF
    | 101H: INC(N_mod);
    | 104H:
      IF N_mod-1 = modno THEN
        Read_sstSymbols(modno, entry.size, mode_brief);
        IF Ok THEN
          CurrComponent^.DI.Modules^[modno].HasInfo := TRUE;
          RETURN;
        ELSE
          Ok := TRUE;
        END;
      END;
    | 10BH:
      IF N_mod-1 = modno THEN
        Read_sstHLLSrc(modno, entry.size);
        IF Ok THEN
          CurrComponent^.DI.Modules^[modno].HasInfo := TRUE;
          RETURN;
        ELSE
          Ok := TRUE;
        END;
      END;
    ELSE
    END;
    rpos := sect;
  END;
EXCEPT
  tls.ClearDebugInfo (CurrCom);
  CurrCom := dt.Invalid_Component;
  CurrComponent := NIL;
  RETURN;
END BuildForMod;


BEGIN
  ExtendedFormat := FALSE;
  TypeImage := NIL;
  According := NIL;
  CurrComponent := NIL;
  CurrCom := MAX(CARDINAL);
  exc.AllocateSource(source);
FINALLY
  IF TypeImage # NIL THEN
    DISPOSE (TypeImage);
  END;
  IF According # NIL THEN
    DISPOSE (According);
  END;
END DI_NB04.
