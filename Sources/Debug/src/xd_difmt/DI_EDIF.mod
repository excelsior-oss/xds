-- Конвертор для формата EDIF во внутреннее представление

<* Storage+ *>
<* ALIGNMENT="1" *>

<* IF TARGET_OS = "WINNT" THEN *>
  <* NEW USEFILEMAPPING+ *>
<* ELSE *>
  <* NEW USEFILEMAPPING- *>
<* END *>

<* NEW TESTEDIF- *>

IMPLEMENTATION MODULE DI_EDIF;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;
IMPORT str := Strings;

IMPORT dt  := DI_Types;
IMPORT bld := DI_Build;
IMPORT tls := DI_Tools;

IMPORT xs  := xStr;
IMPORT msg := MsgNo;
IMPORT opt := Options;
IMPORT red := RedFile;
IMPORT fil := File;
IMPORT ods := OutDebug;

IMPORT kt  := KrnTypes;

IMPORT exc := EXCEPTIONS;

FROM Printf IMPORT printf;

FROM KrnTypes IMPORT EXEC_INFO;

FROM DI_Types IMPORT ComNo, ModNo, COMPONENT, RAW_DEBUG_INFO, PROCESS_DEBUGINFO_PROC;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

IMPORT EDIF;



<* IF USEFILEMAPPING THEN *>
IMPORT win := Windows;
<* ELSE *>
IMPORT rf  := RndFile;
IMPORT xfp := xFilePos;
IMPORT io  := IOChan;
<* END *>



TYPE
  PROCESS_MODE = (mode_brief, mode_full, mode_headers_only);
  PSEGMENT_ENTRY = POINTER TO ARRAY OF EDIF.SEGMENT_ENTRY;

  String = ARRAY [0..1023] OF CHAR;


VAR
  source   : exc.ExceptionSource;
  CurrComponent: POINTER TO COMPONENT;
  CurrCom: CARDINAL;
  SegmentsKey: dt.KEY;
  Exec_Info: kt.EXEC_INFO;
  RawDebugInfo: RAW_DEBUG_INFO;
  DebugSize: CARDINAL;
  rpos: CARDINAL;
  ConsistentKey: xs.String;




PROCEDURE Error (no: CARDINAL);
BEGIN
  exc.RAISE(source, no, '');
END Error;

PROCEDURE ErrorIf (condition: BOOLEAN; no: CARDINAL);
BEGIN
  IF condition THEN
    exc.RAISE(source, no, '');
  END
END ErrorIf;


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


PROCEDURE CreateSegmentsKey (mod: ModNo);
VAR
  s, max: CARDINAL;
BEGIN
  WITH CurrComponent^.DI.Modules^[mod] DO
    max := 0;
    FOR s := 0 TO HIGH(Segments^) DO
      IF max < Segments^[s].Number THEN
        max := Segments^[s].Number;
      END;
    END;
   <* IF TESTEDIF THEN *>
    printf("  max = %d\n", max);
   <* END *>
    IF SegmentsKey # NIL THEN
      DISPOSE (SegmentsKey);
    END;
    NEW (SegmentsKey, max+1);
    ErrorIf (SegmentsKey = NIL, msg.Not_enough_memory);
    FOR s := 0 TO max DO
      SegmentsKey^[s] := MAX(CARDINAL); -- invalid segment number
    END;
    FOR s := 0 TO HIGH(Segments^) DO
      SegmentsKey^[Segments^[s].Number] := s;
    END;
  END;
END CreateSegmentsKey;


PROCEDURE SegmNum2Inx (segm_num: CARDINAL): CARDINAL;
BEGIN
  IF (SegmentsKey = NIL) OR (segm_num > HIGH(SegmentsKey^)) THEN
    RETURN MAX(CARDINAL);
  END;
  RETURN SegmentsKey^[segm_num];
END SegmNum2Inx;




<* PUSH *>
<* -CHECKINDEX  *>
<* -CHECKDINDEX *>
<* -CHECKRANGE  *>
<* -CHECKNIL    *>

PROCEDURE get1(): sys.CARD8;
BEGIN
  INC(rpos);
  RETURN RawDebugInfo^[rpos-1];
END get1;

PROCEDURE get2(): sys.CARD16;
VAR
  zzz: POINTER TO sys.CARD16;
BEGIN
  INC(rpos, 2);
  zzz := sys.ADR(RawDebugInfo^[rpos-2]);
  RETURN zzz^;
END get2;

PROCEDURE get4(): sys.CARD32;
VAR
  zzz: POINTER TO sys.CARD32;
BEGIN
  INC(rpos, 4);
  zzz := sys.ADR(RawDebugInfo^[rpos-4]);
  RETURN zzz^;
END get4;

PROCEDURE getN(VAR zzz: ARRAY OF sys.BYTE);
BEGIN
  sys.MOVE(sys.ADR(RawDebugInfo^[rpos]), sys.ADR(zzz), HIGH(zzz)+1);
  INC(rpos, HIGH(zzz)+1);
END getN;

PROCEDURE getxN(x: CARDINAL; VAR zzz: ARRAY OF sys.BYTE);
BEGIN
  sys.MOVE(sys.ADR(RawDebugInfo^[rpos]), sys.ADR(zzz), x);
  INC(rpos, x);
END getxN;

PROCEDURE getstr0 (VAR str: ARRAY OF CHAR);
VAR
  ch: CHAR;
  i: CARDINAL;
BEGIN
  i := 0;
  REPEAT
    ch := CHAR(get1 ());
    str[i] := ch;
    INC (i);
  UNTIL (ch = 0C) OR (i > HIGH(str));
END getstr0;

PROCEDURE align (n: CARDINAL);
BEGIN
  WHILE rpos MOD n # 0 DO
    INC (rpos);
  END;
END align;

PROCEDURE getstr0align (VAR str: ARRAY OF CHAR; n: CARDINAL);
BEGIN
  getstr0 (str);
  align (n);
END getstr0align;




PROCEDURE skip4 ();
BEGIN
  INC(rpos, 4);
END skip4;


PROCEDURE get_name(VAR name: ARRAY OF CHAR);
VAR
  len: CARDINAL;
BEGIN
  len := get2();
  IF len > HIGH(name) THEN
    len := HIGH(name);
   <* IF TESTEDIF THEN *>
    printf(">> get_name(): strip name length %d to high %d\n", len, HIGH(name));
   <* END *>
  END;
  sys.MOVE(sys.ADR(RawDebugInfo^[rpos]), sys.ADR(name), len);
  name[len] := 0C;
  INC(rpos, len);
END get_name;


PROCEDURE getLen(): CARDINAL;
BEGIN
  IF RawDebugInfo^[rpos] >= 80H THEN
    INC(rpos,2);
    RETURN (RawDebugInfo^[rpos-2]-80H) * VAL(CARDINAL, 256) + RawDebugInfo^[rpos-1];
  ELSE
    INC(rpos);
    RETURN RawDebugInfo^[rpos-1];
  END;
END getLen;



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
    CurrentIndex := 0;
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

<* POP *>


--------------------- Чтение типов -------------------------


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






PROCEDURE TypesSection (mod: dt.ModNo);

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
        -- внешний тип еще не описан, но него есть внутренняя ссылка
        -- из какого-то другого внутреннего типа
        new := Index;
        REPEAT
          -- new содержит индекс внутреннего типа, ссылающигося на
          -- текущий внешний тип, поэтому индекс должен быть правильным
          ASSERT(new # MAX(INTERNAL_INDEX));
          --WITH CurrComponent^.DI.Types DO
            type:=  new;
            PPointerType:= tls.TypeImage(type);
            --PPointerType := sys.ADR(ptr^[key^[new - dt.FIRST_NONPRIMITIVE]]);
          --END;
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
  size, len, beg, tag, i, b, n, save: CARDINAL;
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

  name: String;
  type: dt.PTYPE;
  IsUnique: BOOLEAN;
  field_name: String;
  type_tag: dt.TYPE_TAG;

BEGIN
  size := get4 (); -- get types section size
  IF size = 0 THEN
    RETURN;
  END;

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
            ASSERT( get1() = 0 );  (* TQual = 0           *)
            ASSERT( get1() = 2 );  (* Protection = Public *)
            Base := GetType( get2() );
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
     <* IF TESTEDIF THEN *>
      printf ("Unknown tag 0x%x", tag
     <* END *>
      ASSERT(FALSE);
    END;
  END;
  DEC (CurrentIndex, 511);
END TypesSection;



PROCEDURE SymbolsSection (mod: dt.ModNo; mode: PROCESS_MODE);

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
  size: CARDINAL;
  len : CARDINAL;
  type: CARDINAL;
  name: String;
  beg: CARDINAL;
  ptype: dt.PTYPE; tag: dt.TYPE_TAG;
  Object: dt.RAW_OBJECT;
  proc_level: CARDINAL;
  Skip: BOOLEAN;
  StoredLevel: CARDINAL;
  save : CARDINAL;
  inx: CARDINAL;
  segm: CARDINAL;
  segments: dt.PASEGMENT;
BEGIN
 <* IF TESTEDIF THEN *>
  printf (">> Read_sstSymbols\n");
 <* END *>
  size := get4 (); -- get symbols section size
  IF size = 0 THEN
    RETURN;
  END;
  segments := CurrComponent^.DI.Modules^[mod].Segments;
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
          DataProc.Address := get4();
          segm := get2();
          inx := SegmNum2Inx (segm);
         <* IF TESTEDIF THEN *>
          printf ("   segm = %d, inx = %d\n", segm, inx);
         <* END *>
          Skip := inx = MAX(CARDINAL);
          IF Skip THEN
            StoredLevel := proc_level-1; -- skip all objects under this level
          ELSE
            INC (DataProc.Address, segments^[inx].Begin);
            Tag := dt.Sy_Proc;
            ST_ID := dt.st_original;
            Type := 0; -- void type
            DataProc.Length  := get4();
            DataProc.Begin   := get2();
            DataProc.End     := get4();
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
      ASSERT (proc_level # 0);
      DEC (proc_level);
      IF NOT Skip THEN
        Object.Tag := dt.Sy_End_Block;
        Object.Name := 0;
        bld.AddObject (CurrCom, mod+1, Object);
      ELSIF proc_level = StoredLevel THEN
        Skip := FALSE;
      END;
    ELSE
      IF mode = mode_full THEN
        CASE type OF
        | StaticVar:
          IF NOT Skip THEN
            WITH Object DO
              DataVar.Address := get4();
              segm := get2();
              inx := SegmNum2Inx (segm);
             <* IF TESTEDIF THEN *>
              printf ("   segm = %d, inx = %d\n", segm, inx);
             <* END *>
              IF inx # MAX(CARDINAL) THEN
                type := get2();
                Type := GetType(type);
                get_name(name);
                --Write(name);
                --Write(' type = %x',type);
                IF name # '' THEN
                  ST_ID := dt.st_original;
                  Tag := dt.Sy_Var;
                  INC (DataVar.Address, segments^[inx].Begin);
                  Name := bld.AddName (name);
                  bld.AddObject (CurrCom, mod+1, Object);
                END;
              END;
            END;
          END;

        | LocalVarAuto :
          IF NOT Skip THEN
            ASSERT (proc_level # 0);
            WITH Object DO
              DataRel.Relative := LONGINT(get4());
              type := get2();
              get_name(name);
              IF name # '' THEN
                ST_ID := dt.st_original;
                Tag := dt.Sy_Relative;
                DataRel.RegNo := kt.FRAME_REG;
                Type  := GetType(type);
                ptype := Object.Type;
                IF tls.TypeTag(ptype, tag) & (tag = dt.Reference) THEN
                  DataRel.Attrib := dt.SYM_ATTRIB{dt.SA_Param, dt.SA_Reference};
                  tls.SubType (ptype, ptype);
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
          END;

        | LocalVarRelativeRegister :
          IF NOT Skip THEN
            ASSERT (proc_level # 0);
            WITH Object DO
              DataRel.RegNo := get2();
              IF DataRel.RegNo = 0FFFFH THEN -- dynamic frame relative
                DataRel.RegNo := kt.FRAME_REG;
              END;
              DataRel.Relative := LONGINT(get4());
              type := get2();
              get_name(name);
              IF name # '' THEN
                ST_ID := dt.st_original;
                Tag := dt.Sy_Relative;
                Type  := GetType(type);
                ptype := Object.Type;
                IF tls.TypeTag(ptype, tag) & (tag = dt.Reference) THEN
                  DataRel.Attrib := dt.SYM_ATTRIB{dt.SA_Param, dt.SA_Reference};
                  tls.SubType (ptype, ptype);
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
          END;

       | LocalVarInRegister :
          IF NOT Skip THEN
            ASSERT (proc_level # 0);
            WITH Object DO
              Type := GetType (get2());
              DataReg.RegNo := get1(); (* NB внутренняя нумерация регистров *)
              get_name(name);          (* совпадает с нумерацией формата    *)
              IF name # '' THEN
                ST_ID := dt.st_original;
                Tag := dt.Sy_Register;
                DataReg.Attrib := dt.SYM_ATTRIB{};
                Name := bld.AddName (name);
                bld.AddObject (CurrCom, mod+1, Object);
              END;
            END;
          END;

        ELSE
         <* IF TESTEDIF THEN *>
          printf('unknown type = %u, len = %u\n', type, len);
         <* END *>
        END;
      END;
    END;
    rpos := save + len;
  END;
  ASSERT (proc_level = 0);
END SymbolsSection;




------------------------------------------------------------------------------

PROCEDURE CheckVersionNumber (): BOOLEAN;
VAR
  version: EDIF.DIRECTORY_VERSION;
BEGIN
  skip4 (); -- directory size
  getN (version);
 <* IF TESTEDIF THEN *>
  printf("  VersionMajorNumber = %d\n", version.VersionMajorNumber);
  printf("  VersionMinorNumber = %d\n", version.VersionMinorNumber);
 <* END *>
  RETURN (version.VersionMajorNumber = EDIF.VersionMajorNumber) AND
         (version.VersionMinorNumber = EDIF.VersionMinorNumber);
END CheckVersionNumber;


PROCEDURE CheckReferenceFileName (): BOOLEAN;
VAR
  ReferenceFileName: xs.String;
BEGIN
  getxN (get4(), ReferenceFileName);
 <* IF TESTEDIF THEN *>
  printf("  ReferenceFileName = %s\n", ReferenceFileName);
 <* END *>
  RETURN TRUE;
END CheckReferenceFileName;


PROCEDURE CheckConsistentKey (): BOOLEAN;
VAR
  DbgFileConsistentKey: xs.String;
BEGIN
  getxN (get4(), DbgFileConsistentKey);
 <* IF TESTEDIF THEN *>
  printf("  DbgFileConsistentKey = %s\n", DbgFileConsistentKey);
 <* END *>
  RETURN DbgFileConsistentKey = ConsistentKey;
END CheckConsistentKey;


PROCEDURE SourceFileName (mod: dt.ModNo);
VAR
  name: xs.String;
BEGIN
  getxN (get4(), name);
 <* IF TESTEDIF THEN *>
  printf("  SourceFileName = %s\n", name);
 <* END *>
  WITH CurrComponent^.DI.Modules^[mod] DO
    SourceName := bld.AddName (name);
    Language := tls.TryDetectModLang (name);
  END;
END SourceFileName;


PROCEDURE ModuleName (mod: dt.ModNo);
VAR
  name: xs.String;
BEGIN
  getxN (get4(), name);
 <* IF TESTEDIF THEN *>
  printf("  ModuleName = %s\n", name);
 <* END *>
  CurrComponent^.DI.Modules^[mod].ModuleName := bld.AddName (name);
END ModuleName;



PROCEDURE LinumsSection (mod: dt.ModNo);
VAR
  size, Ninitial, Nfinal, i, inx: CARDINAL;
  linum: EDIF.LINUM_ENTRY;
  savepos: CARDINAL;
BEGIN
 <* IF TESTEDIF THEN *>
  printf(">> LinumsSection\n");
 <* END *>
  WITH CurrComponent^.DI.Modules^[mod] DO
    size := get4();
   <* IF TESTEDIF THEN *>
    printf("  section size = %d\n", size);
   <* END *>
    IF size > 0 THEN
      ErrorIf (size MOD SIZE(linum) # 0, msg.WrongDebugInfo);
      Ninitial := size DIV SIZE(linum);
     <* IF TESTEDIF THEN *>
      printf("  initial N = %d\n", Ninitial);
     <* END *>
      savepos := rpos;
      Nfinal := 0;
      FOR i := 0 TO Ninitial-1 DO
        getN (linum);
       <* IF TESTEDIF THEN *>
        printf("  %5d: %2d, %5d, 0x%$4X\n", i, linum.SegmentNumber, linum.LineNumber, linum.CodeOffset);
       <* END *>
        inx := SegmNum2Inx (linum.SegmentNumber);
        IF inx # MAX(CARDINAL) THEN
          INC (Nfinal);
       <* IF TESTEDIF THEN *>
        ELSE
          printf("  linum is invalid\n");
       <* END *>
        END;
      END;

     <* IF TESTEDIF THEN *>
      printf("  final N = %d\n", Nfinal);
     <* END *>

      IF Nfinal = 0 THEN
        RETURN;
      END;

      NEW (CLTable.CLTable, Nfinal);
      rpos := savepos;
      Nfinal := 0;
      FOR i := 0 TO Ninitial-1 DO
        getN (linum);
        inx := SegmNum2Inx (linum.SegmentNumber);
        IF inx # MAX(CARDINAL) THEN
          CLTable.CLTable^[Nfinal].Addr := Segments^[inx].Begin + linum.CodeOffset;
          CLTable.CLTable^[Nfinal].Line := linum.LineNumber;
          INC (Nfinal);
        END;
      END;

      ASSERT (HIGH(CLTable.CLTable^)+1 = Nfinal);
    END;
  END;
END LinumsSection;



------------------------------------------------------------------------------


PROCEDURE ReadDirectories (dbgfname-, location-: ARRAY OF CHAR; mod: dt.ModNo; mode: PROCESS_MODE): BOOLEAN;
VAR
  Header: EDIF.EDIF_HEADER;
  Directory: EDIF.DIRECTORY;
  dir, saverpos: sys.CARD32;
BEGIN
 <* IF TESTEDIF THEN *>
  printf(">> ReadDirectories\n");
 <* END *>
  ErrorIf (SIZE (Header) > DebugSize, msg.WrongDebugInfo);
  getN (Header);
  ErrorIf (Header.Prefix # EDIF.Prefix, msg.WrongDebugInfo);
 <* IF TESTEDIF THEN *>
  printf("  Directory Table = %d\n", Header.DirectoryTable);
 <* END *>
  FOR dir := 1 TO Header.DirectoryTable DO
    getN (Directory);
   <* IF TESTEDIF THEN *>
    printf("  Directory Type  = %d\n", Directory.Type);
    printf("  Directory Entry = %d\n", Directory.Entry);
    printf("  Directory Size  = %d\n", Directory.Size);
   <* END *>
    saverpos := rpos;
    rpos := Directory.Entry;
    ErrorIf (get4 () # Directory.Code, msg.WrongDebugInfo);
    ErrorIf (get4 () # Directory.Entry, msg.WrongDebugInfo);
    CASE Directory.Type OF
    | EDIF.dtVersionNumber:
      IF NOT CheckVersionNumber () THEN
        ods.AddOutputDebugString ("Invalid version number for module %s (location %s)", dbgfname, location);
        RETURN FALSE;
      END;
    | EDIF.dtConsistentKey:
      IF NOT CheckConsistentKey () THEN
        ods.AddOutputDebugString ("Inconsistent key for module %s (location %s)", dbgfname, location);
        RETURN FALSE;
      END;
    | EDIF.dtSourceFileName:
      SourceFileName (mod);
    | EDIF.dtModuleName:
      ModuleName (mod);
    ELSE
      IF mode = mode_full THEN
        CASE Directory.Type OF
        | EDIF.dtReferenceFileName:
          IF NOT CheckReferenceFileName () THEN
            RETURN FALSE;
          END;
        | EDIF.dtTypesSection:
          TypesSection (mod);
        | EDIF.dtSymbolsSection:
          SymbolsSection (mod, mode);
        | EDIF.dtLinumsSection:
          LinumsSection (mod);
        END;
      END;
    END;
    rpos := saverpos;
  END;
  RETURN TRUE;
END ReadDirectories;



-- Information format:
-- base path; file name; consistent key; module name

TYPE
  InformationField = ( if_basepath
                     , if_filename
                     , if_consistentkey
                     , if_modulename
                     );

PROCEDURE ParseInformation (information-: ARRAY OF CHAR; field: InformationField; VAR value: xs.String);
VAR
  patternFound: BOOLEAN;
  posOfPattern: CARDINAL;
  i, posOld: CARDINAL;
BEGIN
  posOld := 0;
  posOfPattern := 0;
  FOR i := 0 TO ORD(field) DO
    posOld := posOfPattern;
    str.FindNext (";", information, posOld, patternFound, posOfPattern);
    ErrorIf (NOT patternFound, msg.WrongDebugInfo);
    INC (posOfPattern);
  END;
  xs.Extract (information, posOld, posOfPattern-posOld-1, value);
END ParseInformation;






PROCEDURE ReadModuleInfo (mod: dt.ModNo; information-: ARRAY OF CHAR; Segments: PSEGMENT_ENTRY);
VAR
  s, N_seg, N_new: CARDINAL;
  modulename: xs.String;
BEGIN
 <* IF TESTEDIF THEN *>
  printf(">> ReadDbgFileHeader: information = %s\n", information);
 <* END *>

  WITH CurrComponent^.DI DO
    N_seg := HIGH(Segments^)+1;
    N_new := 0;
    FOR s := 0 TO N_seg-1 DO
      IF Segments^[s].SegmentSize # 0 THEN
        INC (N_new);
      END;
    END;
    IF N_new # 0 THEN
      NEW (Modules^[mod].Segments, N_new);
      N_new := 0;
      FOR s := 0 TO N_seg-1 DO
        IF Segments^[s].SegmentSize # 0 THEN
          Modules^[mod].Segments^[N_new].Name    := 0;
          Modules^[mod].Segments^[N_new].Number  := Segments^[s].SegmentNumber;
          Modules^[mod].Segments^[N_new].Begin   := Segments^[s].SegmentOffset;
          Modules^[mod].Segments^[N_new].Sz      := Segments^[s].SegmentSize;
          Modules^[mod].Segments^[N_new].Attribs := Exec_Info.Objects^[Segments^[s].SectionNumber-1].Attributes;
          INC (N_new);
        END;
      END;
      ASSERT (HIGH(Modules^[mod].Segments^)+1 = N_new);
    END;
    WITH Modules^[mod] DO
      ParseInformation (information, if_modulename, modulename);
      ModuleName := bld.AddName (modulename);
      HasInfo := TRUE;
      xs.alloc_from (DebugInfoReference, information);
    END;
  END;
END ReadModuleInfo;






PROCEDURE ReadDbgFile (mod: dt.ModNo; mode: PROCESS_MODE);

VAR
  basepath: xs.String;
  dbgfname: xs.String;
  dbgfullfname: xs.String;
  tmpDebugInfo: RAW_DEBUG_INFO;
  tmpDebugSize: CARDINAL;
  tmprpos: CARDINAL;

 <* IF USEFILEMAPPING THEN *>
  hMapFile   : win.HANDLE;
  lpFile     : win.PVOID;
  hfile      : win.HFILE;
  ReOpenBuff : win.OFSTRUCT;
 <* ELSE *>
  file       : rf.ChanId;
  res        : rf.OpenResults;
 <* END *>


  PROCEDURE Save;
  BEGIN
    tmpDebugInfo := RawDebugInfo;
    tmpDebugSize := DebugSize;
    tmprpos := rpos;
  END Save;


  PROCEDURE Restore;
  BEGIN
    RawDebugInfo := tmpDebugInfo;
    DebugSize := tmpDebugSize;
    rpos := tmprpos;
  END Restore;


  PROCEDURE Init;
  BEGIN
   <* IF USEFILEMAPPING THEN *>
    hfile := 0;
    lpFile := NIL;
    hMapFile := NIL;
   <* ELSE *>
    res := rf.otherProblem;
   <* END *>
    RawDebugInfo := NIL;
    DebugSize := 0;
    rpos := 0;
  END Init;


  PROCEDURE Close;
  BEGIN
   <* IF USEFILEMAPPING THEN *>
    IF hfile # 0 THEN
      win._lclose (hfile);
      hfile := 0;
    END;
    IF lpFile # NIL THEN
      win.UnmapViewOfFile (lpFile);
      lpFile := NIL;
    END;
    IF hMapFile # NIL THEN
      win.CloseHandle (hMapFile);
      hMapFile := NIL;
    END;
   <* ELSE *>
    IF res = rf.opened THEN
      rf.Close (file);
      res := rf.otherProblem;
      IF RawDebugInfo # NIL THEN
        DEALLOCATE (RawDebugInfo, DebugSize);
      END;
    END;
   <* END *>
  END Close;


  MODULE Exit;

  IMPORT Init, Save, Restore, Close;

  BEGIN
    Save ();
    Init ();
  FINALLY
    Close ();
    Restore ();
  END Exit;

BEGIN
 <* IF TESTEDIF THEN *>
   printf(">> ReadDbgFile\n");
 <* END *>
  WITH CurrComponent^.DI.Modules^[mod] DO
    HasInfo := FALSE; -- reset to protect from exceptions

    IF DebugInfoReference = NIL THEN
     <* IF TESTEDIF THEN *>
      printf("  no debug info\n");
     <* END *>
      RETURN;
    END;
   <* IF TESTEDIF THEN *>
    printf("  debug info: %s\n", DebugInfoReference^);
   <* END *>

    ParseInformation (DebugInfoReference^, if_basepath, basepath);
    ParseInformation (DebugInfoReference^, if_filename, dbgfname);
    ParseInformation (DebugInfoReference^, if_consistentkey, ConsistentKey); -- save to the global

   <* IF TESTEDIF THEN *>
    printf("  basepath = %s\n", basepath);
    printf("  dbgfname = %s\n", dbgfname);
    printf("  ConsistentKey = %s\n", ConsistentKey);
   <* END *>

   IF red.ReadEx (basepath, dbgfname, dbgfullfname) # red.Ok THEN
     ods.AddOutputDebugString ("Dbg file %s not found ", dbgfname);
     RETURN;
   END;

   <* IF USEFILEMAPPING THEN *>
    hfile := win.OpenFile (dbgfullfname, ReOpenBuff, win.OF_READ+win.OF_SHARE_DENY_WRITE);
    IF ReOpenBuff.nErrCode # 0 THEN
      ods.AddOutputDebugString ("Cannot open dbg file %s (location %s)", dbgfname, dbgfullfname);
      RETURN;
    END;
    hMapFile := win.CreateFileMapping (win.HANDLE(hfile), NIL, win.PAGE_READONLY, 0, 0, NIL);
    IF hMapFile = NIL THEN
      ods.AddOutputDebugString ("Cannot create mapping dbg file %s (location %s)", dbgfname, dbgfullfname);
      RETURN;
    END;
    lpFile := win.MapViewOfFile (hMapFile, win.FILE_MAP_READ, 0, 0, 0);
    IF lpFile = NIL THEN
      ods.AddOutputDebugString ("Cannot map dbg file %s (location %s)", dbgfname, dbgfullfname);
      RETURN;
    END;
    RawDebugInfo := sys.ADDRESS(lpFile);
    DebugSize := win.GetFileSize  (win.HANDLE(hfile), NIL);
    IF DebugSize = 0FFFFFFFFH THEN
      ods.AddOutputDebugString ("Invalid size of dbg file %s (location %s)", dbgfname, dbgfullfname);
      RETURN;
    END;
   <* ELSE *>
    rf.OpenOld (file, dbgfullfname, rf.raw, res);
    IF res # rf.opened THEN
      ods.AddOutputDebugString ("Cannot open dbg file %s (location %s)", dbgfname, dbgfullfname);
      RETURN;
    END;
    IF NOT xfp.PosToCard (DebugSize, rf.EndPos (file)) OR (DebugSize = 0) THEN
      ods.AddOutputDebugString ("Invalid size of dbg file %s (location %s)", dbgfname, dbgfullfname);
      RETURN;
    END;
    ALLOCATE (RawDebugInfo, DebugSize);
    io.RawRead (file, sys.ADR (RawDebugInfo^), DebugSize, read);
    IF DebugSize # read THEN
      ods.AddOutputDebugString ("Cannot read dbg file %s (location %s)", dbgfname, dbgfullfname);
      RETURN;
    END;
   <* END *>

    CreateSegmentsKey (mod);
    HasInfo := ReadDirectories (dbgfname, dbgfullfname, mod, mode);
    Active := HasInfo;
  END;
EXCEPT
  Close ();
  Restore ();
  RETURN;
END ReadDbgFile;






PROCEDURE SetGlobalVars (com: ComNo; VAR Component: COMPONENT; clear: BOOLEAN);
BEGIN
  IF clear THEN
    -- clear debug info for this component
    Component.DI := dt.EmptyDebugInfo;
  END;
  CurrCom := com;
  RawDebugInfo := Component.raw;
  Exec_Info := Component.EI;
  DebugSize := Exec_Info.DebugInfoSize;
  CurrComponent := sys.ADR(Component);
  rpos := 0;
  PrepareForType (INITIAL_SIZE);
END SetGlobalVars;







PROCEDURE ReadModules (com: ComNo; VAR Component: COMPONENT): CARDINAL;

VAR
  Segments: PSEGMENT_ENTRY;

  (* Размещение сегментов  *)
  PROCEDURE AllocateSegments (VAR Segments: PSEGMENT_ENTRY; no: CARDINAL);
  BEGIN
   <* IF TESTEDIF THEN *>
    printf(">> AllocateSegments, no = %d\n", no);
   <* END *>
    IF no = 0 THEN
      IF Segments # NIL THEN
        DISPOSE (Segments);
      END;
      IF SegmentsKey # NIL THEN
        DISPOSE (SegmentsKey);
      END;
      RETURN;
    END;
    IF Segments = NIL THEN
      NEW (Segments, no);
    ELSE
      IF HIGH(Segments^)+1 # no THEN
        DISPOSE (Segments);
      END;
      NEW (Segments, no);
    END;
    IF Segments = NIL THEN
      Error (msg.Not_enough_memory);
    END;
  END AllocateSegments;


  (* Размещение сегментов  *)
  PROCEDURE AdjustSegments (Segments: PSEGMENT_ENTRY);
  VAR
    s: CARDINAL;
  BEGIN
    FOR s := 0 TO HIGH(Segments^) DO
      WITH Segments^[s] DO
        INC (SegmentOffset, Exec_Info.Objects^[SectionNumber-1].Begin);
      END;
    END;
  END AdjustSegments;



VAR
 <* IF USEFILEMAPPING THEN *>
  hMapFile   : win.HANDLE;
  lpFile     : win.PVOID;
  hfile      : win.HFILE;
  ReOpenBuff : win.OFSTRUCT;
 <* ELSE *>
  file       : rf.ChanId;
  res        : rf.OpenResults;
 <* END *>


  PROCEDURE Init;
  BEGIN
   <* IF USEFILEMAPPING THEN *>
    hfile := 0;
    lpFile := NIL;
    hMapFile := NIL;
   <* ELSE *>
    res := rf.otherProblem;
   <* END *>
  END Init;


  PROCEDURE Close;
  BEGIN
   <* IF USEFILEMAPPING THEN *>
    IF hfile # 0 THEN
      win._lclose (hfile);
      hfile := 0;
    END;
    IF lpFile # NIL THEN
      win.UnmapViewOfFile (lpFile);
      lpFile := NIL;
    END;
    IF hMapFile # NIL THEN
      win.CloseHandle (hMapFile);
      hMapFile := NIL;
    END;
   <* ELSE *>
    IF res = rf.opened THEN
      rf.Close (file);
      res := rf.otherProblem;
      IF RawDebugInfo # NIL THEN
        DEALLOCATE (RawDebugInfo, DebugSize);
      END;
    END;
   <* END *>
  END Close;


  MODULE Exit;

  IMPORT Init, Close;

  BEGIN
    Init ();
  FINALLY
    Close ();
  END Exit;


VAR
  N_mod, m: CARDINAL;
  N_seg, s: CARDINAL;
  tag: EDIF.PREFIX;
  information: String;
  public: dt.PUBLIC;
  name: String;
  P_num, p, section: CARDINAL;
  ConsistentExeKey: CARDINAL;
  ConsistentDbgKey: CARDINAL;
  dbgfname: String;
  dbgfullfname: xs.String;
  section_end: CARDINAL;

BEGIN
 <* IF TESTEDIF THEN *>
  printf(">> ReadModules: com = %d\n", com);
 <* END *>

  SetGlobalVars (com, Component, TRUE);

  -- read EDIF section for the execatable file
  ErrorIf (Exec_Info.DebugInfoTag # EDIF.Prefix, msg.WrongDebugInfo);
  getN (tag);
  ErrorIf (tag # EDIF.Prefix, msg.WrongDebugInfo);
  -- get the consistent key for the executable file
  ConsistentExeKey := get4 ();
  -- get the full path to the debugging information
  getstr0align (dbgfname, 4);
  getN (tag);
  ErrorIf (tag # EDIF.Prefix, msg.WrongDebugInfo);

  IF red.Read (dbgfname, dbgfullfname) # red.Ok THEN
    COPY (Exec_Info.full_name, dbgfname);
    fil.ChangeExtension (dbgfname, "EDI");
    IF (red.Read (dbgfname, dbgfullfname) # red.Ok) AND
       (red.ReadEx ("", dbgfname, dbgfullfname) # red.Ok)
    THEN
      ods.AddOutputDebugString ("EDIF: Debug info not found for %s", Exec_Info.full_name);
      RETURN 0;
    END;
  END;

 <* IF TESTEDIF THEN *>
  printf("  dbgfullfname = %s\n", dbgfullfname);
 <* END *>

  RawDebugInfo := NIL;
  DebugSize := 0;
  rpos := 0;
 <* IF USEFILEMAPPING THEN *>
  hMapFile := NIL;
  lpFile := NIL;
  hfile := win.OpenFile (dbgfullfname, ReOpenBuff, win.OF_READ+win.OF_SHARE_DENY_WRITE);
  IF ReOpenBuff.nErrCode # 0 THEN
    ods.AddOutputDebugString ("Cannot open file %s", dbgfullfname);
    RETURN 0;
  END;
  hMapFile := win.CreateFileMapping (win.HANDLE(hfile), NIL, win.PAGE_READONLY, 0, 0, NIL);
  IF hMapFile = NIL THEN
    ods.AddOutputDebugString ("Cannot create mapping file %s", dbgfullfname);
    RETURN 0;
  END;
  lpFile := win.MapViewOfFile (hMapFile, win.FILE_MAP_READ, 0, 0, 0);
  IF lpFile = NIL THEN
    ods.AddOutputDebugString ("Cannot map view file %s", dbgfullfname);
    RETURN 0;
  END;
  RawDebugInfo := sys.ADDRESS(lpFile);
  DebugSize := win.GetFileSize  (win.HANDLE(hfile), NIL);
  IF DebugSize = 0FFFFFFFFH THEN
    ods.AddOutputDebugString ("Invalid size of file %s", dbgfullfname);
    RETURN 0;
  END;
 <* ELSE *>
  rf.OpenOld (file, dbgfullfname, rf.raw, res);
  IF res # rf.opened THEN
    ods.AddOutputDebugString ("Cannot open file %s", dbgfullfname);
    RETURN;
  END;
  IF NOT xfp.PosToCard (DebugSize, rf.EndPos (file)) OR (DebugSize = 0) THEN
    ods.AddOutputDebugString ("Invalid size of file %s", dbgfullfname);
    RETURN;
  END;
  ALLOCATE (RawDebugInfo, DebugSize);
  io.RawRead (file, sys.ADR (RawDebugInfo^), DebugSize, read);
  IF DebugSize # read THEN
    ods.AddOutputDebugString ("Cannot read file %s", dbgfullfname);
    RETURN;
  END;
 <* END *>

 <* IF TESTEDIF THEN *>
  printf("  Read dbg file...\n");
 <* END *>

  rpos := 0;

  getN (tag);
  ErrorIf (tag # EDIF.Prefix, msg.WrongDebugInfo);

  ConsistentDbgKey := get4 ();

  IF ConsistentExeKey # ConsistentDbgKey THEN
   <* IF TESTEDIF THEN *>
    printf("  Inconsistent keys 0x%$8X and 0x%$8X\n", ConsistentExeKey, ConsistentDbgKey);
   <* END *>
    ods.AddOutputDebugString ("Executable file (%s) and debug information (%s) inconsistent",
                               Exec_Info.full_name, dbgfullfname);
    RETURN msg.ExecDebugInconsistency;
  END;

  LOOP
    getN (tag); -- section name
   <* IF TESTEDIF THEN *>
    printf ("  Record type = %s\n", tag);
   <* END *>
    IF tag = EDIF.ModuleSection THEN
      section_end := rpos + get4();
      N_mod := get4 ();
     <* IF TESTEDIF THEN *>
      printf ("  N mod = %d\n", N_mod);
     <* END *>
      IF N_mod > 0 THEN
        AllocateModules (N_mod);
        CurrComponent^.DI.LastModule := N_mod;
        Segments := NIL;
        (* заполнение структуры информацией о модулях *)
        FOR m := 0 TO N_mod-1 DO
          getstr0align (information, 4);
         <* IF TESTEDIF THEN *>
          printf ("\n  Module number = %d\n", m);
          printf ("  Information = %s\n", information);
         <* END *>
          N_seg := get4 ();
         <* IF TESTEDIF THEN *>
          printf ("  N seg = %d\n", N_seg);
         <* END *>
          IF N_seg > 0 THEN
            AllocateSegments (Segments, N_seg);
            FOR s := 0 TO N_seg-1 DO
              getN (Segments^[s]);
            END;
            AdjustSegments (Segments);
           <* IF TESTEDIF THEN *>
            FOR s := 0 TO N_seg-1 DO
              WITH Segments^[s] DO
                printf ("   %d. seg = %d, sect = %d, offs = 0x%$8x, size = %d\n", s, SegmentNumber, SectionNumber, SegmentOffset, SegmentSize);
              END;
            END;
           <* END *>
            ReadModuleInfo (m, information, Segments);
          END;
        END;
        IF Segments # NIL THEN
          DISPOSE (Segments);
        END;
      END;
      ASSERT (section_end = rpos);
    ELSIF tag = EDIF.PublicSection THEN
      -- public section presents
      section_end := rpos + get4();
      P_num := get4();
      FOR p := 1 TO P_num DO
        getstr0align (name, 4);
        public.name := bld.AddName (name);
        section := get4();
        public.addr := get4();
        IF (0 < section) AND (section <= Exec_Info.N_Objects) THEN
          public.addr := public.addr + Exec_Info.Objects^[section-1].Begin;
          public.code := TRUE;
          public.len := 1;
          bld.AddPublic (CurrCom, public);
        END;
      END;
      ASSERT (section_end = rpos);
    ELSIF tag = EDIF.Terminator THEN
      ASSERT (DebugSize = get4 ());
      EXIT;
    ELSE
     <* IF TESTEDIF THEN *>
      printf ("  Unknown record type\n");
     <* END *>
    END;
    IF rpos = DebugSize THEN
      EXIT;
    END;
  END;

  Close ();

  (* Создание ключей по модулям *)
  bld.CreateKeysByModules (CurrCom);

  (* Создание ключа для таблицы сегментов *)
  bld.CreateKGroupSegments (CurrCom);

  (* Установка языка компоненты по модулям *)
  bld.SetComLanguage (CurrCom);

 <* IF DEFINED (xd_debug) & xd_debug THEN *>
  IF opt.Debug(opt.Load) THEN
    bld.ShowDebugInfo (CurrCom);
  END;
 <* END *>

  RETURN 0;
EXCEPT
  Close ();
  tls.ClearDebugInfo (CurrCom);
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
END ReadModules;



PROCEDURE BuildForMod (modno: ModNo; com: ComNo; VAR Component: COMPONENT);
BEGIN
 <* IF TESTEDIF THEN *>
  printf(">> BuildForMod: com = %d, mod = %d\n", com, modno);
 <* END *>
  WITH Component.DI.Modules^[modno] DO
    IF NOT DebugInfoProcessed THEN
      SetGlobalVars (com, Component, FALSE);
      ReadDbgFile (modno, mode_full);
      IF HasInfo THEN
        -- rebuild the modules name key
        bld.CreateKeysByModules (CurrCom);
        -- re-set the component language
        bld.SetComLanguage (CurrCom);
      END;
      DebugInfoProcessed := TRUE;
    END;
  END;
EXCEPT
  IF exc.IsCurrentSource(source) THEN
    RETURN;
  ELSE
  <* IF DEFINED (xd_debug) & xd_debug THEN *>
    IF NOT opt.Debug (opt.Load) THEN
      RETURN;
    END;
  <* ELSE *>
   RETURN;
  <* END *>
  END;
END BuildForMod;



PROCEDURE ProcessDebugInfo(com: ComNo; VAR Component: COMPONENT): CARDINAL;
BEGIN
  RETURN ReadModules (com, Component);
END ProcessDebugInfo;



BEGIN
  TypeImage := NIL;
  According := NIL;
  CurrComponent := NIL;
  CurrCom := MAX(CARDINAL);
  SegmentsKey := NIL;
  exc.AllocateSource(source);
FINALLY
  IF TypeImage # NIL THEN
    DISPOSE (TypeImage);
  END;
  IF According # NIL THEN
    DISPOSE (According);
  END;
  IF SegmentsKey # NIL THEN
    DISPOSE (SegmentsKey);
  END;
END DI_EDIF.
