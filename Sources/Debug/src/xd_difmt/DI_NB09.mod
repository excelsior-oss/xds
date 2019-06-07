-- Конвертор для формата NB09 (CodeView) во внутреннее представление

IMPLEMENTATION MODULE DI_NB09;

<* Storage+ *>
<* ALIGNMENT="1" *>

IMPORT sys := SYSTEM;
IMPORT exc := EXCEPTIONS;
IMPORT fmt := FormStr;

IMPORT dt  := DI_Types;
IMPORT bld := DI_Build;
IMPORT tls := DI_Tools;

IMPORT xs  := xStr;
IMPORT msg := MsgNo;
IMPORT opt := Options;

IMPORT kt  := KrnTypes;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM KrnTypes IMPORT EXEC_INFO;
FROM DI_Types IMPORT ComNo, COMPONENT, RAW_DEBUG_INFO, PROCESS_DEBUGINFO_PROC, EmptyModule;


<* IF DEFINED (xd_debug) & xd_debug THEN *>

FROM Printf IMPORT printf;

<* END *>


TYPE
  PROCESS_MODE = (mode_brief, mode_full);

VAR
  CurrComponent: POINTER TO COMPONENT;
  CurrCom  : CARDINAL;
  Exec_info: kt.EXEC_INFO;
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
    IF CurrComponent^.DI.Modules = NIL THEN Error(msg.Not_enough_memory); END;
  ELSE
    IF HIGH(CurrComponent^.DI.Modules^)+1 # no THEN
      DISPOSE(CurrComponent^.DI.Modules);
      NEW(CurrComponent^.DI.Modules, no);
      IF CurrComponent^.DI.Modules = NIL THEN Error(msg.Not_enough_memory); END;
    END;
  END;
  FOR i := 0 TO no-1 DO
    CurrComponent^.DI.Modules^[i] := EmptyModule;
  END;
END AllocateModules;



---------------------------------------------------------------------------

VAR
  db_raw: RAW_DEBUG_INFO;
  rpos: CARDINAL;


<* PUSH *>
<* -CHECKINDEX  *>
<* -CHECKDINDEX *>
<* -CHECKRANGE  *>
<* -CHECKNIL    *>
<* -COVERFLOW   *>

PROCEDURE get1(): sys.CARD8;
BEGIN
  INC(rpos);
  RETURN db_raw^[rpos-1];
END get1;

PROCEDURE get2(): sys.CARD16;
VAR
  zzz: POINTER TO sys.CARD16;
BEGIN
  zzz := sys.ADR(db_raw^[rpos]);
  INC(rpos, 2);
  RETURN zzz^;
END get2;

PROCEDURE get4(): sys.CARD32;
VAR
  zzz: POINTER TO sys.CARD32;
BEGIN
  zzz := sys.ADR(db_raw^[rpos]);
  INC(rpos, 4);
  RETURN zzz^;
END get4;

PROCEDURE getN(VAR zzz: ARRAY OF sys.BYTE);
BEGIN
  sys.MOVE(sys.ADR(db_raw^[rpos]), sys.ADR(zzz), HIGH(zzz)+1);
  INC(rpos, HIGH(zzz)+1);
END getN;

(*
PROCEDURE getxN(x: CARDINAL; VAR zzz: ARRAY OF sys.BYTE);
BEGIN
  sys.MOVE(sys.ADR(db_raw^[rpos]), sys.ADR(zzz), x);
  INC(rpos, x);
END getxN;
*)


PROCEDURE skip1 ();
BEGIN
  INC (rpos, 1);
END skip1;

PROCEDURE skip2 ();
BEGIN
  INC (rpos, 2);
END skip2;

PROCEDURE skip4 ();
BEGIN
  INC (rpos, 4);
END skip4;

PROCEDURE skipN (howmany: CARDINAL);
BEGIN
  INC (rpos, howmany);
END skipN;



TYPE
  NAME = ARRAY [0..255] OF CHAR;

PROCEDURE get_name(VAR name: ARRAY OF CHAR);
VAR
  len: sys.CARD8;
BEGIN
  len := db_raw^[rpos];
  sys.MOVE(sys.ADR(db_raw^[rpos+1]), sys.ADR(name), len);
  name[len] := 0C;
  INC(rpos, len+1);
END get_name;

<* POP *>

CONST
  SubSecMod      = 0120H;
  sstAlignSym    = 0125H;
  SubSecSrc      = 0127H;
  sstLibraries   = 0128H;
  sstGlobalSym   = 0129H;
  sstGlobalPub   = 012aH;
  sstGlobalTypes = 012bH;
  sstSegMap      = 012dH;
  sstFileIndex   = 0133H;
  sstStaticSym   = 0134H;
  
VAR
  N_types: CARDINAL;

  -- Сохраняет абсолютное смещение записи типа
  TypesOffsets: POINTER TO ARRAY OF dt.R_TYPE;

  -- Сохраняет соответствия внешних и внутренних типов
  According: POINTER TO ARRAY OF dt.R_TYPE;



PROCEDURE Read_sstGlobalTypes;
VAR
  i: CARDINAL;
  p_types: POINTER TO ARRAY [0..0FFFFFEH] OF CARDINAL;
BEGIN
  ASSERT(get4() = 1);
  N_types := get4();
  IF N_types > 0 THEN
    IF TypesOffsets # NIL THEN
      DISPOSE(TypesOffsets);
    END;
    NEW(TypesOffsets, N_types);
    p_types := sys.ADR(db_raw^[rpos]);
    FOR i := 0 TO N_types-1 DO
      TypesOffsets^[i] := p_types^[i] + N_types*4+rpos;
    END;
    NEW(According, N_types);
    ASSERT(According#NIL);
  END;
END Read_sstGlobalTypes;
  
CONST
  LF_NUMERIC   = 8000H;
  LF_CHAR      = LF_NUMERIC;
  LF_SHORT     = LF_NUMERIC + 01H;
  LF_USHORT    = LF_NUMERIC + 02H;
  LF_LONG      = LF_NUMERIC + 03H;
  LF_ULONG     = LF_NUMERIC + 04H;

PROCEDURE Read_len(): CARDINAL;
VAR
  val: CARDINAL;
BEGIN
  val := get2();
  IF (val < 8000H) THEN RETURN val;
  ELSE
    CASE val OF
    | LF_CHAR:   RETURN get1();
    | LF_SHORT:  RETURN get2();
    | LF_USHORT: RETURN get2();
    | LF_LONG:   RETURN get4();
    | LF_ULONG:  RETURN get4();
    END;
  END;
END Read_len;



TYPE
  -- Member attribute field
  MAF_Access = ( Access_No_access_protected
               , Access_Private
               , Access_Protected
               , Access_Public
               );

  MAF_MProp = ( MProp_Vanilla
              , MProp_Virtual
              , MProp_Static
              , MProp_Friend
              , MProp_Introducing_Virtual
              , MProp_Pure_Virtual
              , MProp_Pure_Introducing_Virtual
              );

  MAF_MProps = SET OF MAF_MProp;

CONST
   MAF_MProp_Virtuals = MAF_MProps { MProp_Introducing_Virtual
                                   , MProp_Pure_Introducing_Virtual
                                   };


PROCEDURE Get_MAF_MProp (maf: sys.CARD16): MAF_MProp;
BEGIN
  RETURN MAF_MProp (sys.SHIFT (sys.SET16(maf), -2) * sys.SET16(00000007H));
END Get_MAF_MProp;



CONST
  MAX_ITEM = 0FFFFH;
  EMBEGGING_LEVEL = 32;
  INIT_ITEMS = 128;


TYPE
  RECORD_IMAGE_FIELD = ARRAY [0..MAX_ITEM-1] OF dt.TYPE_RECORD_FIELD;
  ENUM_IMAGE_ITEM    = ARRAY [0..MAX_ITEM-1] OF dt.TYPE_ENUM_ITEM;

  -- НИКОГДА не переопределяйте тип! Он должен соответствовать порядку
  -- записей в определениях указанных типов в DI_Types.def
 <* PUSH *>
 <* ALIGNMENT = "1" *>

  HEAD_TYPE  = RECORD
                  CASE :CARDINAL OF
                 | 0 : RecordType: dt.TYPE_RECORD;
                 | 1 : EnumType  : dt.TYPE_ENUM;
                 | 2 : ClassType : dt.TYPE_CLASS;
                  END;
               END;

  ITEM_TYPE = RECORD
                 CASE :CARDINAL OF
                 | 0 : RecField : dt.TYPE_RECORD_FIELD;
                 | 1 : Item     : dt.TYPE_ENUM_ITEM;
                 | 2 : Member   : dt.TYPE_RECORD_FIELD;
                 END;
               END;

  IMAGE_TYPE = RECORD
                 CASE :CARDINAL OF
                 | 0 : Header    : HEAD_TYPE;
                 | 1 : RecordType: dt.TYPE_RECORD;
                       RecFields : RECORD_IMAGE_FIELD;
                 | 2 : EnumType  : dt.TYPE_ENUM;
                       Items     : ENUM_IMAGE_ITEM;
                 | 3 : ClassType : dt.TYPE_CLASS;
                       Members   : RECORD_IMAGE_FIELD;
                 END;
               END;

  PBYTE = POINTER TO ARRAY [0..SIZE(HEAD_TYPE)+MAX_ITEM*SIZE(ITEM_TYPE)-1] OF sys.CARD8;


 <* POP *>

  PIMAGE = POINTER TO IMAGE_TYPE;

  IMAGE_ENTRY = RECORD
                  Pimage: PIMAGE;
                  N     : CARDINAL;
                END;

  PIMAGETBL = POINTER TO ARRAY OF IMAGE_ENTRY;

  INTERNAL_INDEX = dt.R_TYPE;


VAR
  Images:  PIMAGETBL;
  Index :  CARDINAL;


PROCEDURE GetType (mod: CARDINAL; ext_type: CARDINAL): INTERNAL_INDEX;

 PROCEDURE GetImageIndex(): CARDINAL;
 VAR
   Pbyte: PBYTE;
   tmp  : PIMAGETBL;
   size : CARDINAL;
   i    : CARDINAL;
 BEGIN
   IF Images = NIL THEN
     -- создать таблицу
     ASSERT (Index = 0);
     NEW (Images, EMBEGGING_LEVEL);
     ASSERT (Images # NIL);
     FOR i := 0 TO HIGH(Images^) DO
       Images^[i] := IMAGE_ENTRY {NIL, 0};
     END;
     Index := 0;
   ELSIF Index > HIGH(Images^) THEN
     -- увеличиваем таблицу в два раза
     size := HIGH(Images^)+1;
     NEW (tmp, 2*size);
     ASSERT (tmp # NIL);
     sys.MOVE (sys.ADR(Images^), sys.ADR(tmp^), SIZE(Images^));
     DISPOSE (Images);
     Images := tmp;
     FOR i := size TO HIGH(Images^) DO
       Images^[i] := IMAGE_ENTRY {NIL, 0};
     END;
   END;
   IF Images^[Index].Pimage = NIL THEN
     ALLOCATE (Pbyte, SIZE(HEAD_TYPE)+INIT_ITEMS*SIZE(ITEM_TYPE));
     ASSERT (Pbyte # NIL);
     Images^[Index] := IMAGE_ENTRY {sys.ADDRESS(Pbyte), INIT_ITEMS};
   END;
   INC (Index);
   RETURN Index-1;
 END GetImageIndex;


 PROCEDURE FreeImageIndex ();
 BEGIN
   DEC (Index);
 END FreeImageIndex;


 PROCEDURE CheckSpace (CurImagePos: CARDINAL; MemNum: CARDINAL);
 VAR
   tmp : PBYTE;
   Head: HEAD_TYPE;
 BEGIN
   IF MemNum > Images^[CurImagePos].N THEN
     Head := Images^[CurImagePos].Pimage^.Header;
     tmp := sys.ADDRESS (Images^[CurImagePos].Pimage);
     DEALLOCATE (tmp, SIZE(HEAD_TYPE)+Images^[CurImagePos].N*SIZE(ITEM_TYPE));
     ALLOCATE (tmp, SIZE(HEAD_TYPE)+MemNum*SIZE(ITEM_TYPE));
     ASSERT (tmp # NIL);
     Images^[CurImagePos] := IMAGE_ENTRY {sys.ADDRESS(tmp), MemNum};
     Images^[CurImagePos].Pimage^.Header := Head;
   END;
 END CheckSpace;


  MODULE Save_rpos;
  IMPORT rpos;
  VAR
    save: CARDINAL;
  BEGIN
    save := rpos;
  FINALLY
    rpos := save;
  END Save_rpos;


VAR
  RangeType    : dt.TYPE_RANGE;
  ProcType     : dt.TYPE_PROCEDURE;
  Pointer      : dt.TYPE_POINTER;
  Reference    : dt.TYPE_REFERENCE;
  p            : POINTER TO dt.TYPE_POINTER;
  ref          : POINTER TO dt.TYPE_REFERENCE;
  ArrayType    : dt.TYPE_ARRAY;
  ArrayOfType  : dt.TYPE_ARRAY_OF;
  A_Base       : CARDINAL;
  A_Index      : CARDINAL;
  A_Max        : CARDINAL;
  name         : ARRAY [0..255] OF CHAR;
  field_name   : ARRAY [0..255] OF CHAR;
  len          : CARDINAL;
  tag          : CARDINAL;
  save_rpos    : CARDINAL;
  i, j         : CARDINAL;
  type, ind    : sys.CARD32;
  attribs      : sys.CARD8;
  PClassType   : POINTER TO dt.TYPE_CLASS;
  PRecordType   : POINTER TO dt.TYPE_RECORD;
  CurImagePos  : CARDINAL;
  IsUnique     : BOOLEAN;
  MembersCount : CARDINAL;
  type_tag     : dt.TYPE_TAG;
BEGIN
  IF ext_type DIV 100H = 4 THEN
    RETURN GetType (mod, ext_type MOD 100H) + dt.STD_POINTERS_BEGIN;
  END;
  CASE ext_type OF
  | 003H : RETURN 0;
  | 070H : RETURN dt.STD_TYPE_CHAR8;
  | 071H : RETURN dt.STD_TYPE_CHAR16;
  | 010H, 11H, 12H, 13H : RETURN dt.STD_TYPE_INT8    + ext_type - 010H;
  | 020H, 21H, 22H, 23H : RETURN dt.STD_TYPE_CARD8   + ext_type - 020H;
  | 040H, 41H, 42H : RETURN dt.STD_TYPE_REAL    + ext_type - 040H;
  | 050H, 51H, 52H : RETURN dt.STD_TYPE_COMPLEX + ext_type - 050H;
  | 074H: RETURN dt.STD_TYPE_INT32;
  | 075H: RETURN dt.STD_TYPE_CARD32;
  | 076H: RETURN dt.STD_TYPE_INT64;
  | 077H: RETURN dt.STD_TYPE_CARD64;
  | 030H .. 032H: RETURN dt.STD_TYPE_BOOLEAN8 + (ext_type-030H);
  ELSE
    IF ext_type >= 1000H THEN
      DEC(ext_type, 1000H);
      ASSERT(ext_type <= HIGH(According^), ext_type);
      IF According^[ext_type] # MAX(CARDINAL) THEN
        RETURN According^[ext_type];
      END;
      rpos := TypesOffsets^[ext_type];
        INC(rpos, 2);
        tag := get2();
        CASE tag OF
        | 001H: -- Type Modifier
          INC(rpos, 2);                 -- attribute
          RETURN GetType (mod, get2()); -- next @index

        | 002H:
          attribs := get1();
          skip1 (); -- Only first byte of attribs
          ASSERT((attribs MOD 32) = 10);
          CASE (attribs DIV 32) OF
          | 0: -- Usual pointer
            Pointer.TypeData.Tag := dt.Pointer;
            Pointer.TypeData.Com := CurrCom;
            IF mod = dt.Fake_Module THEN
              Pointer.TypeData.Mod := dt.Fake_Module;
            ELSE
              Pointer.TypeData.Mod := mod + 1;
            END;
            Pointer.TypeData.Name := 0;
            Pointer.Base := 0;
            Pointer.Length := 4;
            IsUnique := TRUE;
            ind := bld.AddType (Pointer, IsUnique);
            According^[ext_type] := ind;
            type := GetType (mod, get2());
            p:= tls.TypeImage(ind);
            p^.Base := type;

            RETURN ind;
          | 1: -- reference (means VAR parameter);
--            printf('%u - Ref', ext_type+1000H);
            WITH Reference DO
              WITH TypeData DO
                Tag := dt.Reference;
                Name := 0;
                Com := CurrCom;
                IF mod = dt.Fake_Module THEN
                  Mod := dt.Fake_Module;
                ELSE
                  Mod := mod + 1;
                END;
              END;
              Base := 0;
            END;
            IsUnique := NOT opt.MergeEqualTypes;
            According^[ext_type] := bld.AddType (Reference, IsUnique);
            ref := tls.TypeImage (According^[ext_type]);
            ref^.Base := GetType (mod, get2());
            RETURN According^[ext_type];
          END;

        | 003H:
          -- Array
          --
          -- 1) High index not equal zero means ordinary array
          --    ARRAY [...] with lower and upper boundary.
          --
          -- 2) If high index equal zero and exists type POINTER
          --    which alludes to _that_ (under consideration)
          --    means ARRAY OF as 'dynamical array'.
          --
          -- 3) If high index equal zero and exists type REFERENCE
          --    which alludes to _that_ (under consideration)
          --    means ARRAY OF as paramater 'open array'.
          --    We use such method because every open array type
          --    is referenced only once.

          -- General information about array
          A_Base := GetType (mod, get2());
          A_Index := GetType (mod, get2());
          A_Max := Read_len();

          get_name(name);
          IF A_Max = 0 THEN
            WITH ArrayOfType DO
              WITH TypeData DO
                Tag := dt.Array_of;;
                Com := CurrCom;
                IF mod = dt.Fake_Module THEN
                  Mod := dt.Fake_Module;
                ELSE
                  Mod := mod + 1;
                END;
                Name := 0;
              END;
              Base := A_Base;
            END;
            IsUnique := NOT opt.MergeEqualTypes;
            According^[ext_type] := bld.AddType (ArrayOfType, IsUnique);
            IF IsUnique THEN
              ASSERT(tls.RenameType (According^[ext_type], name));
            END;
            RETURN According^[ext_type];
          ELSE
            WITH ArrayType DO
              WITH TypeData DO
                Tag := dt.Array;
                Com := CurrCom;
                IF mod = dt.Fake_Module THEN
                  Mod := dt.Fake_Module;
                ELSE
                  Mod := mod + 1;
                END;
                Name := 0;
              END;
              Base := A_Base;
              WITH RangeType DO
                WITH TypeData DO
                  Tag := dt.Range;
                  Com := CurrCom;
                  IF mod = dt.Fake_Module THEN
                    Mod := dt.Fake_Module;
                  ELSE
                    Mod := mod + 1;
                  END;
                  Name := 0;
                END;
                Base := A_Index;
                Min := CARDINAL(0);
                Max := A_Max / tls.TypeSize(A_Base) - 1 ;
              END;
              IsUnique := NOT opt.MergeEqualTypes;
              Index := bld.AddType (RangeType, IsUnique);
            END;
            IsUnique := NOT opt.MergeEqualTypes;
            According^[ext_type] := bld.AddType (ArrayType, IsUnique);
            IF IsUnique THEN
              ASSERT(tls.RenameType (According^[ext_type], name));
            END;
            RETURN According^[ext_type];
          END;

        | 004H: -- Class
            CurImagePos := GetImageIndex();
            WITH Images^[CurImagePos].Pimage^ DO
              WITH ClassType DO
                TypeData.Tag := dt.Class;
                TypeData.Com := CurrCom;
                IF mod = dt.Fake_Module THEN
                  TypeData.Mod := dt.Fake_Module;
                ELSE
                  TypeData.Mod := mod + 1;
                END;
                MembersCount := get2(); -- count
                MyMembers := 0;
                AllMembers := MyMembers;
                Base := 0;
                CheckSpace (CurImagePos, MembersCount);
              END;
            END;
            WITH Images^[CurImagePos].Pimage^ DO
              WITH ClassType DO
                len := get2(); -- @fields, type index of the field list
                IF MembersCount # 0 THEN
                  save_rpos := rpos;
                  rpos := TypesOffsets^[len-01000H] + 2;
                  len := get2();
                  ASSERT (len = 0204H, len); -- must be @field list
                  -- read the field list of the class
                  i := 1;
                  LOOP
                    IF i > MembersCount THEN
                      EXIT;
                    END;
                    len := get1();
                    WHILE len > 0F0H DO
                      INC(rpos, len-0F0H-1);
                      len := get1();
                    END;
                    INC (len, get1() * VAL(CARDINAL, 256));
                    CASE len OF
                    | 002: -- LF_POINTER
                      ------------------------ SKIP IT ------------------------
                      attribs := get1(); -- attribs, 1 byte
                      skip1();           -- attribs, 2 byte
                      skip2(); -- @type
                      -- variant
                      CASE attribs MOD 32 OF
                      | 3 : -- Based on segment
                        skip2(); -- segment value
                      | 6 : -- Based on symbol
                        skipN (get2());
                      | 8 : -- Based on type
                        skip2(); --@type
                      ELSE
                      END;
                      CASE attribs DIV 32 OF
                      | 2   -- Pointer to data member
                      , 3 : -- Pointer to method
                        skip2(); -- @class
                        CASE get2() OF -- format
                        | 0
                        , 1
                        , 3
                        , 5 : skip2(); -- mdisp
                        | 2
                        , 9 : skip2(); -- mdisp
                              skip2(); -- pdisp
                              skip2(); -- vdisp
                        | 4 : skip4(); -- mdisp
                              skip4(); -- pdisp
                              skip4(); -- vdisp
                        | 6
                        , 8 : skip2(); -- mdisp
                              skip2(); -- pdisp
                        | 7 : skip2(); -- mdisp
                              skip2(); -- pdisp
                              skip2(); -- vdisp
                              skip2();
                        | 10: skip2(); -- mdisp
                              skip2(); -- pdisp
                              skip2(); -- vdisp
                              skip2();
                        | 11: skip4(); -- mdisp
                        | 12: skip4(); -- mdisp
                              skip4(); -- vdisp
                        | 13: skip4(); -- mdisp
                              skip4(); -- pdisp
                              skip4(); -- vdisp
                              skip4();
                        END;
                      ELSE
                      END;

                    | 00AH: -- Virtual Function Table Shape
                      ------------------------ SKIP IT ------------------------
                      len := get2 (); -- Count of bases classes
                      FOR j := 1 TO (len+1) DIV 2 DO
                        skip1 ();
                      END;

                    | 012H: -- LF_VFTPATH
                      len := get2 (); -- Count of bases classes
                      FOR j := 1 TO len DO
                        skip2 (); -- sys.EVAL (GetType (mod, get2())) Type index of the base class in the path
                      END;

                    | 400H: -- Real Base Class
                      IF Base = 0 THEN
                        Base := GetType (mod, get2()); -- type
                        ASSERT (tls.TypeTag (Base, type_tag));
                        IF type_tag = dt.Class THEN
                          PClassType := tls.TypeImage (Base);
                          AllMembers := MyMembers + PClassType^.AllMembers;
                        ELSE
                          -- в Обероне разрешено расширять классы записями
                          ASSERT (type_tag = dt.Record);
                          PRecordType := tls.TypeImage (Base);
                          AllMembers := MyMembers + PRecordType^.Fields;
                        END;
                      ELSE
                        skip2(); -- type, mutliinheritens
                      END;
                      skip2(); -- Member attribute bit field
                      skip2(); -- offset

                    | 406H: -- LF_MEMBER
                      WITH Members[MyMembers] DO
                        FieldType := GetType (mod, get2());
                        skip2 (); -- Member attribute bit field
                        FieldOffs := Read_len();
                        FieldSTID := dt.st_original;
                        get_name(field_name);
                        FieldName := bld.AddName ( field_name);
                      END;
                      INC (MyMembers);
                      INC (AllMembers);


                    | 407H: -- LF_STMEMBER
                      skip2 (); -- sys.EVAL (GetType (mod, get2())); -- Index to type record for field
                      skip2 (); -- Member attribute bit field
                      get_name(name);
--                      printf ("LF_STMEMBER name=%s\n", name);

                    | 408H: -- LF_METHOD
                      skip2 (); -- Number (level) of overloaded method
                      skip2 (); -- sys.EVAL (GetType (mod, get2())); -- Type index of method
                      get_name(name);
--                      printf ("LF_METHOD name=%s\n", name);

                    | 409H: -- LF_NESTEDTYPE
                      skip2 (); -- sys.EVAL (GetType (mod, get2())); -- Type index of nested type
                      get_name(name);  -- Name of nested type
--                      printf ("LF_NESTEDTYPE name = %s\n", name);

                    | 40AH: -- LF_VFUNCTAB
                      skip2 (); -- sys.EVAL (GetType (mod, get2())); -- Index to the pointer record descibing the pointer.

                    | 40CH: -- LF_ONEMETHOD
                      len := get2 (); -- Method attribute
                      skip2 (); -- sys.EVAL (GetType (mod, get2())); -- Type index of method
                      IF Get_MAF_MProp (len) IN MAF_MProp_Virtuals THEN
                        skip4 (); -- Offset in VTable if virtual method
                      END;
                      get_name(name);
--                      printf ("LF_ONEMETHOD name=%s\n", name);
                    ELSE
--                      printf ("CLASS leaf=0x%X\n", len);
                      EXIT;
                    END;
                    INC (i);
                  END; -- end of LOOP
                  rpos := save_rpos;
                END;

                -- Property bit field
                --    packed:1    Structure is packed
                --    ...         ...
                --    ...         ...
                --    ...         ...
                --    ...         ...
                --    ...         ...
                --    ...         ...
                --    ...         ...
                --    scoped:1    This is a scoped definition
                --    reserved:8  Reserved
                skip2 (); -- property
                skip2 (); -- dlist
                skip2 (); -- vshape
                Length := Read_len();
                get_name (name);
                TypeData.Name := 0;
              END;
            END;
            IsUnique := NOT opt.MergeEqualTypes;
            According^[ext_type] := bld.AddType (Images^[CurImagePos].Pimage^, IsUnique);
            IF IsUnique THEN
              ASSERT(tls.RenameType (According^[ext_type], name));
            END;
            FreeImageIndex();
            RETURN According^[ext_type];

        | 005H  -- Record
        , 006H: -- Union
            CurImagePos := GetImageIndex();
            WITH Images^[CurImagePos].Pimage^  DO
              WITH RecordType DO
                TypeData.Tag := dt.Record;
                TypeData.Com := CurrCom;
                IF mod = dt.Fake_Module THEN
                  TypeData.Mod := dt.Fake_Module;
                ELSE
                  TypeData.Mod := mod + 1;
                END;
                Fields := get2();
                CheckSpace (CurImagePos, Fields);
              END;
            END;
            WITH Images^[CurImagePos].Pimage^  DO
              WITH RecordType DO
                IF Fields # 0 THEN                     (* Reading fields *)
                  save_rpos := rpos+2;
                  rpos := TypesOffsets^[get2()-01000H] + 2;   (* Fields type    *)
                  len := get2();
                  ASSERT( len = 0204H, len );
                  FOR i := 0 TO Fields-1 DO
                    len := get1();
                    WHILE len > 0F0H DO
                      INC(rpos, len-0F0H-1);
                      len := get1();
                    END;
                    INC(len, get1() * VAL(CARDINAL, 256));
                    CASE len OF
                    | 400H: -- Real Base Class
                      skip2(); -- type
                      skip2(); -- attributes
                      skip2(); -- offset

                    | 406H:
                      WITH RecFields[i] DO
                        FieldType := GetType (mod, get2());
                        INC(rpos, 2); -- field attributes
                        FieldOffs := Read_len();
                        FieldSTID := dt.st_original;
                        get_name(field_name);
                        FieldName := bld.AddName ( field_name);
                      END;
                    | 408H: -- LF_METHOD
                      skip2 (); -- Number (level) of overloaded method
                      sys.EVAL (GetType (mod, get2())); -- Type index of method
                      get_name(name);
--                      printf ("LF_METHOD name=%s\n", name);
                    | 409H: -- LF_NESTEDTYPE
                      sys.EVAL (GetType (mod, get2())); -- Type index of nested type
                      get_name(name);  -- Name of nested type
--                      printf ("LF_NESTEDTYPE name = %s\n", name);
                    | 40AH: -- LF_VFUNCTAB
                      skip2 (); -- sys.EVAL (GetType (mod, get2())); -- Index to the pointer record descibing the pointer.
                    | 40CH: -- LF_ONEMETHOD
                      len := get2 (); -- Method attribute
                      skip2 (); -- sys.EVAL (GetType (mod, get2())); -- Type index of method
                      IF Get_MAF_MProp (len) IN MAF_MProp_Virtuals THEN
                        skip4 (); -- Offset in VTable if virtual method
                      END;
                      get_name(name);
--                      printf ("LF_ONEMETHOD name=%s\n", name);
                    | 012H: -- LF_VFTPATH
                      len := get2 (); -- Count of bases classes
                      FOR j := 1 TO len DO
                        skip2 (); -- sys.EVAL (GetType (mod, get2())) Type index of the base class in the path
                      END;
                    | 00AH: -- Virtual Function Table Shape
                      ------------------------ SKIP IT ------------------------
                      len := get2 (); -- Count of bases classes
                      FOR j := 1 TO (len+1) DIV 2 DO
                        skip1 ();
                      END;
                    ELSE
--                      printf ("RECORD/UNION leaf=0x%X\n", len);
                      ASSERT (FALSE);
                    END;
                  END;
                  rpos := save_rpos;
                ELSE
                  skip2 ();
                END;
                skip2 (); -- property
                IF tag = 005H THEN
                  skip2 (); -- dlist
                  skip2 (); -- vshape
                END;
                Length := Read_len();
                get_name(name);
                TypeData.Name := 0;
              END;
            END;
            IsUnique := NOT opt.MergeEqualTypes;
            According^[ext_type] := bld.AddType (Images^[CurImagePos].Pimage^, IsUnique);
            IF IsUnique THEN
              ASSERT(tls.RenameType (According^[ext_type], name));
            END;
            FreeImageIndex();
            RETURN According^[ext_type];

        | 007H: -- Enum
            CurImagePos := GetImageIndex();
            WITH Images^[CurImagePos].Pimage^ DO
              WITH EnumType DO
                TypeData.Tag := dt.Enum;
                TypeData.Com := CurrCom;
                IF mod = dt.Fake_Module THEN
                  TypeData.Mod := dt.Fake_Module;
                ELSE
                  TypeData.Mod := mod + 1;
                END;
                Quantity := get2();
                CheckSpace (CurImagePos, Quantity);
              END;
            END;
            WITH Images^[CurImagePos].Pimage^ DO
              WITH EnumType DO
                Base := GetType (mod, get2());
                IF Quantity # 0 THEN                     (* Reading fields *)
                  save_rpos := rpos+2;
                  len := get2()-01000H;
                  rpos := TypesOffsets^[len] + 2;   (* Fields type    *)
                  len := get2();
                  ASSERT (len = 0204H, len);
                  FOR i := 0 TO Quantity-1 DO
                    len := get1();
                    WHILE len > 0F0H DO
                      INC(rpos, len-0F0H-1);
                      len := get1();
                    END;
                    INC(len, get1() * VAL(CARDINAL, 256));
                    ASSERT( len=0403H, len );
                    WITH Items[i] DO
                      skip2(); -- Member attribute bit field
                      EnumValue := Read_len();
                      get_name(field_name);
                      EnumName := bld.AddName ( field_name);
                      bld.AddEnumeration (CurrCom, mod+1, EnumName, EnumValue);
                    END;
                  END;
                  rpos := save_rpos;
                ELSE
                  INC(rpos, 2);
                END;
                skip2(); -- Property
                get_name (name);
                TypeData.Name := 0;
              END;
            END;
            IsUnique := NOT opt.MergeEqualTypes;
            According^[ext_type] := bld.AddType (Images^[CurImagePos].Pimage^, IsUnique);
            IF IsUnique THEN
              ASSERT(tls.RenameType (According^[ext_type], name));
            END;
            FreeImageIndex();
            RETURN According^[ext_type];

        | 008H: -- Procedure
            WITH ProcType DO
              TypeData.Tag := dt.Procedure;
              TypeData.Com := CurrCom;
              IF mod = dt.Fake_Module THEN
                TypeData.Mod := dt.Fake_Module;
              ELSE
                TypeData.Mod := mod + 1;
              END;
              TypeData.Name := 0;                -- None
              ResultType := GetType (mod, get2());
              CallingConv := get1();
              INC(rpos);
              ParamCount := get2();
(*
              IF ParamCount # 0 THEN                     (* Reading fields *)
                save_rpos := rpos;
                WITH ProcParamType DO
                  Descrition: sys.CARD8;
                  Type      : R_TYPE;
                END;
                rpos := save;
              END;
*)
              INC(rpos, 2);
            END;
            IsUnique := NOT opt.MergeEqualTypes;
            According^[ext_type] := bld.AddType (ProcType, IsUnique);
            RETURN According^[ext_type];

        | 009H: -- Member Function
            WITH ProcType DO
              TypeData.Tag := dt.Procedure;
              TypeData.Com := CurrCom;
              IF mod = dt.Fake_Module THEN
                TypeData.Mod := dt.Fake_Module;
              ELSE
                TypeData.Mod := mod + 1;
              END;
              TypeData.Name := 0; -- None
              ResultType := GetType (mod, get2());
              skip2(); -- @class
              skip2(); -- @this
              CallingConv := get1();
              skip1(); -- res
              ParamCount := get2();
              skip2(); -- @arglist
              skip4(); -- thisadjust
            END;
            IsUnique := NOT opt.MergeEqualTypes;
            According^[ext_type] := bld.AddType (ProcType, IsUnique);
            RETURN According^[ext_type];

        | 206H: -- Bit field
            skip1(); -- length
            skip1(); -- position
            According^[ext_type] := GetType (mod, get2()); -- @type
            RETURN According^[ext_type];

        ELSE
         <* IF DEFINED (xd_debug) & xd_debug THEN *>
          printf(' Type No = %x\n', ext_type);
          printf(' Type tag = %x\n', tag);
         <* END *>
          RETURN 0;
        END;
    ELSE
     <* IF DEFINED (xd_debug) & xd_debug THEN *>
      printf(' Unknown Std Type = %x\n', ext_type);
     <* END *>
      RETURN 0;
    END;
  END;
END GetType;
  
  
PROCEDURE Convert(regno: sys.CARD16): CARDINAL;
BEGIN
  CASE regno OF
  | 17..24: RETURN 10H + (regno-17);
  | 09..16: RETURN 08H + (regno-09);
  | 25..30: RETURN 18H + (regno-25);
  | 01..08: RETURN regno - 1;
  | 33:     RETURN 22H;
  | 34:     RETURN 24H;
  ELSE
    ASSERT(FALSE);
  END;
END Convert;
  

PROCEDURE Read_sstAlignSym (mod, size: CARDINAL; mode: PROCESS_MODE);

  PROCEDURE DublicateRefOpenArray (mod: dt.ModNo; old: dt.PTYPE; VAR new: dt.PTYPE);
  VAR
    OpenArray: dt.TYPE_OPEN_ARRAY;
    Reference: dt.TYPE_REFERENCE;
    tag: dt.TYPE_TAG;
    IsUnique : BOOLEAN;
  BEGIN
    new := old;
    ASSERT(tls.TypeTag(old, tag));
    IF (tag = dt.Reference) OR (tag = dt.Array_of) THEN
      tls.SubType (old, old);
      DublicateRefOpenArray (mod, old, old);
      CASE tag OF
      | dt.Reference:
        WITH Reference DO
          WITH TypeData DO
            Tag  := dt.Reference;
            Name := 0;
          END;
          Base := old;
        END;
        IsUnique := NOT opt.MergeEqualTypes;
        new := bld.AddType (Reference, IsUnique);
      | dt.Array_of:
        WITH OpenArray DO
          WITH TypeData DO
            Tag  := dt.OpenArray;
            Name := 0;
          END;
          Base := old;
        END;
        IsUnique := NOT opt.MergeEqualTypes;
        new := bld.AddType(OpenArray, IsUnique);
      END;
    END;
  END DublicateRefOpenArray;


  -- Specify OPEN ARRAY type by relative
  PROCEDURE SpecifyOpenArrayType (relative: LONGCARD; type: dt.PTYPE);
  VAR
    POpenArray: POINTER TO dt.TYPE_OPEN_ARRAY;
    i, dim    : CARDINAL;
    tag       : dt.TYPE_TAG;
  BEGIN
    dim := tls.ArrayDim (type);
    FOR i := 1 TO dim DO
      -- Not for a while yet type tag must be ARRAY OF
      ASSERT(tls.TypeTag(type, tag) AND (tag = dt.OpenArray));
      POpenArray := tls.TypeImage (type);
      WITH POpenArray^ DO
        WITH Length DO
          Tag   := dt.Sy_Relative;
          Name  := 0;
          Type  := dt.STD_TYPE_CARD32;
          ST_ID := dt.st_original;
          WITH DataRel DO
            RegNo    := kt.FRAME_REG;
            Relative := relative+i*4;
            Attrib   := dt.SYM_ATTRIB{dt.SA_Param};
          END;
        END;
      END;
      tls.SubType (type, type);
    END;
  END SpecifyOpenArrayType;


VAR
  beg  : CARDINAL;
  len  : CARDINAL;
  segm : CARDINAL;
  offs : CARDINAL;
  index: CARDINAL;
  save : CARDINAL;
  name : xs.String;
  addr : xs.String;
  type : CARDINAL;
  stype: dt.PTYPE;
  tag  : dt.TYPE_TAG;
  flags: sys.SET8;

  ProcCount : CARDINAL;
  BlockCount: CARDINAL;
  BlockNo   : CARDINAL;
  AlignSym  : dt.RAW_OBJECT;

BEGIN
  beg := rpos;
  ProcCount := 0;
  BlockCount := 0;
  BlockNo := 0;
  ASSERT(get4() = 1);
  IF (mode = mode_full) AND (According # NIL) THEN
    sys.FILL(sys.ADR(According^[0]), 0FFH, SIZE(According^));
  END;
  WHILE (rpos-beg) < size DO
    len   := get2();
    save := rpos;
    index := get2();
    CASE index OF
    | 205H, 204H: -- Local, Global procedure start
      INC(ProcCount);
      INC(rpos,4+4+4); -- pParent, pEnd, pNext
      WITH AlignSym DO
        Tag := dt.Sy_Proc;
        ST_ID := dt.st_original;
        DataProc.Length := get4();
        DataProc.Begin := get4();
        DataProc.End := get4();
        offs  := get4();
        segm  := get2();
        type  := get2();
        flags := sys.SET8 (get1 ());
        get_name(name);
        IF (0 < segm) AND (segm <= Exec_info.N_Objects) THEN
          DataProc.Address := offs + Exec_info.Objects^[segm-1].Begin;
          IF (DataProc.Length = 0) AND (DataProc.Address <= Exec_info.Objects^[segm-1].End) THEN
            DataProc.Length := 1;
            DataProc.End := 1;
          END;
          IF DataProc.Length # 0 THEN
            IF mode = mode_full THEN
              Type := GetType (mod, type);
            ELSE
              Type := 0;
            END;
            ASSERT(name # '');
            Name := bld.AddName (name);
            DataProc.HasFrame  := NOT (7 IN flags);
            IF get1 () = 0EAH THEN  -- EA_PROC  Procedure extended attributes
              DataProc.FrameSize := get4 ();
            ELSE
              DataProc.FrameSize := 0;
            END;
           <* IF DEFINED (xd_debug) & xd_debug THEN *>
            -- printf ("%-20s hasFrame=%d frameSize=%d\n", name, DataProc.HasFrame, DataProc.FrameSize);
           <* END *>
            bld.AddObject (CurrCom, mod+1, AlignSym);
          END;
        END;
      END;

    | 206H: -- Thunk
      INC(BlockCount);
      INC(BlockNo);

    | 207H: -- Block start
      INC(BlockCount);
      INC(BlockNo);
      INC(rpos, 4);   -- pParent
      INC(rpos, 4);   -- pEnd
      INC(rpos, 4);   -- length
      INC(rpos, 4);   -- offset
      INC(rpos, 2);   -- segment
      get_name(name); -- name

    | 006H:
      IF BlockCount > 0 THEN
        DEC(BlockCount);
      ELSE
        DEC(ProcCount);
        AlignSym.Tag := dt.Sy_End_Block;
        AlignSym.Name := 0;
        bld.AddObject (CurrCom, mod+1, AlignSym);
      END;

    | 209H: -- Code label

    ELSE
      IF mode = mode_full THEN
        CASE index OF
        | 402H: -- Alignment
        | 001H: -- Compiler flags
        | 005H:
  --        printf('Start Search:   offs=%u segment = %u', get4(), get2());
        | 009H:
  --        printf('Object File Name:   signature=%x', get4());
  --        get_name(name);
        | 201H, 202H: -- Static variable
          WITH AlignSym DO
            offs := get4();
            segm := get2();
            IF (0 < segm) AND (segm <= Exec_info.N_Objects) THEN
              DataVar.Address := offs + Exec_info.Objects^[segm-1].Begin;
              Type := GetType(mod, get2());
              get_name(name);
              IF (name # '') AND (tls.TypeSize (Type) # 0)THEN
                IF BlockCount > 0 THEN
                  fmt.print (addr, "_%d", BlockNo);
                  xs.Append (addr, name);
                END;
                Name := bld.AddName (name);
                ST_ID := dt.st_original;
                Tag := dt.Sy_Var;
                bld.AddObject (CurrCom, mod+1, AlignSym);
              END;
            END;
          END;
        | 200H: -- BP Relative local variable
          ASSERT(ProcCount#0);
          WITH AlignSym DO
            ST_ID := dt.st_original;
            Tag := dt.Sy_Relative;
            DataRel.Relative := LONGINT(get4());
            DataRel.RegNo := kt.FRAME_REG;
            type := get2();
            get_name(name);
            ASSERT(name # '');
            IF BlockCount > 0 THEN
              fmt.print (addr, "_%d", BlockNo);
              xs.Append (addr, name);
            END;
            Type := GetType(mod, type);
            IF tls.TypeTag(Type, tag) & (tag = dt.Reference) THEN
              DataRel.Attrib := dt.SYM_ATTRIB{dt.SA_Param, dt.SA_Reference};
              tls.SubType (Type, stype);
              ASSERT(tls.TypeTag (stype, tag));
              IF tag = dt.Array_of THEN
                DublicateRefOpenArray (mod, Type, Type);
                tls.SubType(Type, stype);
                SpecifyOpenArrayType (DataRel.Relative, stype);
              END;
              Type := stype;
            ELSE
              DataRel.Attrib := dt.SYM_ATTRIB{};
            END;
            Name := bld.AddName ( name);
            bld.AddObject (CurrCom, mod+1, AlignSym);
          END;

        | 20CH: -- Relative local variable
          ASSERT(ProcCount#0);
          WITH AlignSym DO
            ST_ID := dt.st_original;
            Tag := dt.Sy_Relative;
            DataRel.Relative := LONGINT(get4());
            DataRel.RegNo := get2 ();
            IF DataRel.RegNo = 0FFFFH THEN
              DataRel.RegNo := kt.FRAME_REG;
            END;
            type := get2();
            get_name(name);
            ASSERT(name # '');
            IF BlockCount > 0 THEN
              fmt.print (addr, "_%d", BlockNo);
              xs.Append (addr, name);
            END;
            Type := GetType(mod, type);
            IF tls.TypeTag(Type, tag) & (tag = dt.Reference) THEN
              DataRel.Attrib := dt.SYM_ATTRIB{dt.SA_Param, dt.SA_Reference};
              tls.SubType (Type, stype);
              ASSERT(tls.TypeTag (stype, tag));
              IF tag = dt.Array_of THEN
                DublicateRefOpenArray (mod, Type, Type);
                tls.SubType(Type, stype);
                SpecifyOpenArrayType (DataRel.Relative, stype);
              END;
              Type := stype;
            ELSE
              DataRel.Attrib := dt.SYM_ATTRIB{};
            END;
            Name := bld.AddName ( name);
            bld.AddObject (CurrCom, mod+1, AlignSym);
          END;

        | 002h : -- Register based local variable
          ASSERT (ProcCount#0);
          WITH AlignSym DO
            ST_ID := dt.st_original;
            Tag := dt.Sy_Register;
            type := get2();
            Type := GetType(mod, type);
            DataReg.RegNo := Convert(get2());
            DataReg.Attrib := dt.SYM_ATTRIB{};
            get_name(name);
            ASSERT(name # '');
            IF BlockCount > 0 THEN
              fmt.print (addr, "_%d", BlockNo);
              xs.Append (addr, name);
            END;
            Name := bld.AddName ( name);
            bld.AddObject (CurrCom, mod+1, AlignSym);
          END;

        | 20Dh : -- Dynamic frame based local variable
          -- see S_REGREL32 description in compiler's module dbgcv.ob
          ASSERT (ProcCount#0);
         <* IF DEFINED (xd_debug) & xd_debug THEN *>
          printf('Unprocessed symbol S_FRAMEREL32\n');
         <* END *>

        ELSE
         <* IF DEFINED (xd_debug) & xd_debug THEN *>
          printf('Unknown symbol %x\n', index);
         <* END *>
        END;
      END;
    END;
    rpos := save + len;
  END;
  CurrComponent^.DI.Modules^[mod].HasInfo := TRUE;
END Read_sstAlignSym;


TYPE
  SST_MODULE = RECORD
    ovlNo: sys.CARD16;
    iLib : sys.CARD16;
    segCC: sys.CARD16;
    style: ARRAY [0..1] OF CHAR;
  END;

  SEG_INFO = RECORD
               segNo  : sys.CARD16;
               pad    : sys.CARD16;
               codeofs: LONGCARD;
               codelen: LONGCARD;
             END;

PROCEDURE Read_sstModules(mod: CARDINAL);
VAR
  module: SST_MODULE;
  name  : ARRAY [0..255] OF CHAR;
  i, N  : CARDINAL;
  Segm  : SEG_INFO;
  save  : CARDINAL;

  PROCEDURE is_segment_valid (segm-: SEG_INFO): BOOLEAN;
  BEGIN
    RETURN (0 < segm.segNo) AND (segm.segNo <= Exec_info.N_Objects) AND (segm.codelen # 0);
  END is_segment_valid;

BEGIN
  getN(module);
  WITH CurrComponent^.DI.Modules^[mod] DO
    IF module.segCC > 0 THEN
      N := 0;
      save := rpos;
      FOR i := 0 TO module.segCC-1 DO
        getN(Segm);
        IF is_segment_valid (Segm) THEN
          INC (N);
        END;
      END;
      IF N > 0 THEN
        rpos := save;
        NEW(Segments, N);
        N := 0;
        FOR i := 0 TO module.segCC-1 DO
          getN(Segm);
          IF is_segment_valid (Segm) THEN
            WITH Segments^[N] DO
              Name  := 0;
              Begin := Exec_info.Objects^[Segm.segNo-1].Begin + Segm.codeofs;
              Sz    := Segm.codelen;
            END;
            INC (N);
          END;
        END;
      END;
    END;
    get_name(name);
    ModuleName := bld.AddName ( name);
    Active  := TRUE;
  END;
END Read_sstModules;


PROCEDURE Read_SubSecSrc (mod: CARDINAL; read_all_data: BOOLEAN);
VAR
  pos: LONGCARD;
  i, beg, cSeg: CARDINAL;
--  Seg: CARDINAL;
  pairCC: sys.CARD16;
  name: ARRAY[0..1024] OF CHAR;
  tmp: xs.txt_ptr;
BEGIN
  beg := rpos;

  skip2 (); -- Number of source files
  skip2 (); -- Number of code segments receiving from this module
  rpos := beg+get4(); -- go to file table - here for first file only!
   -- reading file table
  cSeg := get2();
  skip2 ();
  pos := get4(); -- position of line number to address mapping information
  INC(rpos, 4*(cSeg-1)+8*cSeg); -- skip rest of baseSrcLn array and start/end array
  get_name(name);
  rpos := beg+pos; -- go to line number to address mapping information

--  Seg := get2(); -- segment index
--  ASSERT(Seg = 1);
  skip2();
  pairCC := get2(); -- pair number
  ASSERT (pairCC > 0);

  WITH CurrComponent^.DI.Modules^[mod] DO
    tmp := tls.GetName (SourceName);
    IF tmp^ = "" THEN
      SourceName := bld.AddName (name);
    END;
    IF NOT read_all_data THEN
      RETURN;
    END;
    Language := tls.TryDetectModLang (name);
    ASSERT(Active);
    WITH CLTable DO
      NEW(CLTable, pairCC);
      FOR i := 0 TO pairCC-1 DO
        WITH CLTable^[i] DO
          Addr := get4() + Exec_info.Objects^[Exec_info.Code_Object].Begin;
        END;
      END;
      FOR i := 0 TO pairCC-1 DO
        WITH CLTable^[i] DO
          Line := get2();
        END;
      END;
    END;
    HasInfo := TRUE;
  END;
END Read_SubSecSrc;

TYPE
  SEG_DESC = RECORD
               flags: sys.CARD16;
               ovl  : sys.CARD16;
               group: sys.CARD16;
               frame: sys.CARD16;
               iSegName, iClassName: sys.CARD16;
               offset, size: CARDINAL;
             END;

PROCEDURE Read_sstSegMap;
<* IF DEFINED (xd_debug) & xd_debug THEN *>
VAR
  cSeg, cSegLog: sys.CARD16;
  seg_desc: SEG_DESC;
  i       : CARDINAL;
BEGIN
  IF NOT opt.Debug(opt.Load) THEN RETURN END;
  cSeg := get2();
  cSegLog := get2();
  printf('', cSegLog);
  FOR i := 0 TO cSeg-1 DO
    printf('------------------------\n');  
    getN(seg_desc);
    WITH seg_desc DO
      printf('  flags = %$4X', flags);
      printf('  ovl   = %$4X', ovl);
      printf('  group = %$4X', group);
      printf('  frame = %$4X', frame);
      printf('  iSegName   = %$4X', iSegName);
      printf('  iClassName = %$4X', iClassName);
      printf('  offset = %$8X', offset);
      printf('  size   = %$8X', size);
    END;
  END;
<* END *>
END Read_sstSegMap;


TYPE
  GP_HEADER = RECORD
                symhash   : sys.CARD16;
                addrhash  : sys.CARD16;
                cbSymbol  : sys.CARD32;
                cbSymHash : sys.CARD32;
                cbAddrHash: sys.CARD32;
              END;

TYPE
  STR= POINTER TO ARRAY [0..0FFFEH] OF CHAR;
  

(*

PROCEDURE NameHashFunc(str: STR; cb: CARDINAL): CARDINAL;
VAR
  ulEnd, ulSum: BITSET;
  cul  : CARDINAL;
  lpulName: POINTER TO ARRAY [0.10000] OF BITSET;
  i : CARDINAL;
BEGIN
  ulEnd := {};
  WHILE cb MOD 4 # 0 DO
    DEC (cb);
    ulEnd := sys.SHIFT (ulEnd + (BITSET(0DFH) * BITSET(ORD(str^[cb]))), 8);
  END;
  cul   := cb DIV 4;
  ulSum := {};
  lpulName := sys.ADDRESS(str);
  IF cul > 0 THEN
    FOR i := 0 TO cul-1 DO
      ulSum := sys.ROTATE (ulSum / (lpulName^[i] * BITSET(0DFDFDFDFH)), 4);
    END;
  END;
  RETURN CARDINAL (ulSum / ulEnd);
END NameHashFunc;
*)


PROCEDURE Read_sstGlobalSym;
VAR
  header : GP_HEADER;
  symbols: CARDINAL;

  len : sys.CARD16;
  name: xs.String;
  save: CARDINAL;
  segm: CARDINAL;
  offs: CARDINAL;

  StaticVar: dt.RAW_OBJECT;

BEGIN
  getN(header);

  symbols := rpos;

  WHILE rpos < symbols + header.cbSymbol DO
    len := get2();
    save := rpos;
    CASE get2() OF
    | 202H:
      WITH StaticVar DO
        offs := get4();
        segm := get2();
        IF (0 < segm) AND (segm <= Exec_info.N_Objects) THEN
          DataVar.Address := offs + Exec_info.Objects^[segm-1].Begin;
          Type := GetType (dt.Fake_Module, get2());
          get_name(name);
          IF (name # '') AND (tls.TypeSize (Type) # 0)THEN
            Name := bld.AddName (name);
            ST_ID := dt.st_original;
            Tag := dt.Sy_Var;
            bld.AddObject (CurrCom, dt.Fake_Module, StaticVar);
          END;
        END;
      END;
    | 03H: -- Constant
    | 400H, 401H, 402H, 04H:
    ELSE
      ASSERT(FALSE, rpos);
    END;
    rpos := save + len;
  END;
END Read_sstGlobalSym;



PROCEDURE Read_sstGlobalPub;
  
VAR
  header : GP_HEADER;
  symbols: CARDINAL;
  offs   : CARDINAL;
  segm   : CARDINAL;
  len    : sys.CARD16;
  name   : xs.String;
  save   : CARDINAL;
  public : dt.PUBLIC;
BEGIN
  getN(header);

  symbols := rpos;

  WHILE rpos < symbols + header.cbSymbol DO
    len := get2();
    save := rpos;
    CASE get2() OF 
    | 203H: -- Public 32:16
      offs := get4();
      segm := get2();
      IF (0 < segm) AND (segm <= Exec_info.N_Objects) THEN
        public.code := segm = Exec_info.Code_Object+1;
        INC(rpos, 2);
        get_name(name);
        public.name := bld.AddName (name);
        public.len := 1;
        public.addr := offs + Exec_info.Objects^[segm-1].Begin;
        bld.AddPublic (CurrCom, public);
      END;  
    | 103H: -- Public 16:16
    | 402H:
    ELSE
      ASSERT(FALSE, rpos);
    END;
    rpos := save + len;
  END;
END Read_sstGlobalPub;

TYPE
  DIR_HEADER = RECORD
    hdrLen: sys.CARD16;
    entryLen: sys.CARD16;
    eCC: CARDINAL;
    nextofs: CARDINAL;
    flags: BITSET;
  END;

  DIR_ENTRY = RECORD
    type   : sys.CARD16;
    modNo   : sys.CARD16;
    offs   : CARDINAL;
    length : CARDINAL;
  END;


PROCEDURE PrepareToReadDirectory(com: ComNo; VAR Component: COMPONENT): CARDINAL;
VAR
  dh   : DIR_HEADER;
BEGIN
  WITH Component DO
    db_raw := raw;
    Exec_info := EI;
  END;

  CurrCom       := com;
  CurrComponent := sys.ADR(Component);

  rpos := 4;
  rpos := get4();

  getN(dh);

  IF dh.nextofs # 0 THEN RETURN 0; END;
  IF dh.entryLen # SIZE(DIR_ENTRY) THEN RETURN 0; END;
  IF dh.flags # BITSET{} THEN RETURN 0 END;

  RETURN dh.eCC;
END PrepareToReadDirectory;



PROCEDURE CorrectSourceName (mod: dt.ModNo);
VAR
  name: xs.txt_ptr;
  i   : CARDINAL;
  del : BOOLEAN;
  dot : BOOLEAN;
  cut : BOOLEAN;
BEGIN
  WITH CurrComponent^.DI.Modules^[mod-1] DO
    name := tls.GetName ( SourceName);
    del := name^ = "";
    name := tls.GetName (ModuleName);
    IF del THEN
      SourceName := bld.AddNewName (name^);
      name := tls.GetName (SourceName);
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




(* Чтение отладочной информации для программы *)
(* Имя программы нужно указать полное         *)
PROCEDURE internal_ProcessDebugInfo(com: ComNo; VAR Component: COMPONENT;
                                    mode: PROCESS_MODE): CARDINAL;
VAR
  entry: DIR_ENTRY;
  
  i, N, N_mod: CARDINAL;
  save, save2 : CARDINAL;

BEGIN
  CurrCom := com;
  Component.DI := dt.EmptyDebugInfo;
  N := PrepareToReadDirectory(com, Component);

  (* Строим структуру с информацией о модулях *)

  (* первый проход: подсчет числа модулей. Предполагается, что информация           *)
  (* о модулях идет единым куском в начале каждой структуры с отладочной информацей *)

  save := rpos;

  N_mod := 0;
  FOR i := 0 TO N-1 DO
    getN(entry);
    save2 := rpos;
    rpos := entry.offs;
    CASE entry.type OF
    | SubSecMod: INC(N_mod);
    | sstGlobalTypes: IF mode = mode_full THEN Read_sstGlobalTypes; END;
    ELSE
    END;
    rpos := save2;
  END;

  rpos := save;
  
  AllocateModules(N_mod);

  CurrComponent^.DI.LastModule := N_mod;

  FOR i := 0 TO N-1 DO
    getN(entry);
    save := rpos;
    rpos := entry.offs;
    IF mode = mode_full THEN
      CASE entry.type OF
      | SubSecMod      : Read_sstModules (entry.modNo-1);
      | SubSecSrc      : Read_SubSecSrc (entry.modNo-1, TRUE (* читать все данные *));
      | sstSegMap      : Read_sstSegMap;
      | sstGlobalPub   : Read_sstGlobalPub;
      | sstAlignSym    : Read_sstAlignSym (entry.modNo-1, entry.length, mode_full);
      | sstGlobalSym   : Read_sstGlobalSym;
      | sstGlobalTypes : -- has already read above
      | sstLibraries   : -- not supported
      | sstFileIndex   : -- not supported
      | sstStaticSym   : -- not supported
      ELSE
       <* IF DEFINED (xd_debug) & xd_debug THEN *>
        printf('Unprocessed %x\n', entry.type);
       <* END *>
      END;
    ELSE -- mode = mode_brief
      CASE entry.type OF
      | SubSecMod      : Read_sstModules (entry.modNo-1);
      | SubSecSrc      : Read_SubSecSrc (entry.modNo-1, FALSE (* читать только имя исходного файла *));
      | sstSegMap      : -- only at full mode
      | sstGlobalPub   : -- only at full mode
      | sstAlignSym    : -- only at full mode
      | sstGlobalSym   : -- only at full mode
      | sstGlobalTypes : -- has already read above
      | sstLibraries   : -- not supported
      | sstFileIndex   : -- not supported
      | sstStaticSym   : -- not supported
      ELSE
       <* IF DEFINED (xd_debug) & xd_debug THEN *>
        printf('Unprocessed %x\n', entry.type);
       <* END *>
      END;
    END;
    rpos := save;
  END;

  (* Изменения имен модулей для отображения и вычислений выражений *)
  CorrectSourceNames;
  (* Создание ключей по модулям *)
  bld.CreateKeysByModules (CurrCom);
  (* Создание ключа для таблицы сегментов *)
  bld.CreateKGroupSegments (CurrCom);
  (* Установка языка компоненты по модулям *)
  bld.SetComLanguage (CurrCom);

  IF mode = mode_full THEN
   -- DISPOSE(Component.raw);
    IF According # NIL THEN
      DISPOSE(According);
    END;
    db_raw := NIL;
  END;

<* IF DEFINED (xd_debug) & xd_debug THEN *>
  IF opt.Debug(opt.Load) THEN bld.ShowDebugInfo (CurrCom); END;
<* END *>  

  CurrCom := dt.Invalid_Component;

  RETURN 0;
EXCEPT
  IF According # NIL THEN
    DISPOSE(According);
  END;
  tls.ClearDebugInfo (CurrCom);
  CurrCom := dt.Invalid_Component;

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


PROCEDURE BuildForMod(modno: CARDINAL; com: dt.ComNo; VAR Component: COMPONENT);
VAR
  entry: DIR_ENTRY;
  i, N : CARDINAL;
  save : CARDINAL;
  Ok   : BOOLEAN;
BEGIN
  N := PrepareToReadDirectory(com, Component);
  ASSERT(N # 0);
  CurrCom := com;

  IF CurrComponent^.DI.Modules^[modno].HasInfo THEN RETURN; END;

  Ok := FALSE;
  FOR i := 0 TO N-1 DO
    getN(entry);
    save := rpos;
    rpos := entry.offs;
    CASE entry.type OF
    | SubSecSrc  :
      IF modno = entry.modNo-1 THEN
        Read_SubSecSrc (entry.modNo-1, TRUE (* читать все данные *));
        IF Ok THEN
          CurrComponent^.DI.Modules^[modno].HasInfo := TRUE;
          CorrectSourceName (modno+1);
          RETURN;
        ELSE
          Ok := TRUE;
        END;
      END;
    | sstAlignSym:
      IF modno = entry.modNo-1 THEN
        Read_sstAlignSym(entry.modNo-1, entry.length, mode_brief);
        IF Ok THEN
          CurrComponent^.DI.Modules^[modno].HasInfo := TRUE;
          CorrectSourceName (modno+1);
          RETURN;
        ELSE
          Ok := TRUE;
        END;
      END;
    ELSE
    END;
    rpos := save;
  END;
  CurrComponent^.DI.Modules^[modno].HasInfo := TRUE;
  CorrectSourceName (modno+1);
EXCEPT
  tls.ClearDebugInfo (CurrCom);
  CurrCom := dt.Invalid_Component;
  CurrComponent := NIL;
  RETURN;
END BuildForMod;

BEGIN
  Images:= NIL;
  Index:= 0;
  TypesOffsets := NIL;
  According := NIL;
  CurrCom := dt.Invalid_Component;
  exc.AllocateSource(source);
END DI_NB09.