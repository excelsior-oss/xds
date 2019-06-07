(* ??????
  - распределение DW_OP_reg* - пока что там прописаны первые попавшиеся константы
  - верно ли употребление DW_AT_variable_parameter?
*)
MODULE dbgDWARF;

IMPORT SYSTEM;

IMPORT cmd := CodeDef;
IMPORT pc  := pcK;
IMPORT at  := opAttrs;
IMPORT def := opDef;
IMPORT nms := ObjNames;
IMPORT ir;
IMPORT pr  := opProcs;
IMPORT tun := opTune;
IMPORT str := Strings;
IMPORT dbg := DbgFace;
IMPORT opt := Options;
IMPORT reg := Registry;

<* IF TARGET_386 THEN *>
IMPORT D := desc386;
<* END *>


-- IMPLEMENTATION --

TYPE
  INT8   = SYSTEM.INT8;
  INT16  = SYSTEM.INT16;
  INT32  = SYSTEM.INT32;
  CARD32 = SYSTEM.CARD32;


CONST
  first_nonprimitive = 512;


------------- S y m b o l s --------------------------------------------

CONST

  -- DWARF constants:

  DW_TAG_array_type             = 01H;
  DW_TAG_class_type             = 02H;
  DW_TAG_entry_point            = 03H;
  DW_TAG_enumeration_type       = 04H;
  DW_TAG_formal_parameter       = 05H;
  DW_TAG_imported_declaration   = 08H;
  DW_TAG_label                  = 0aH;
  DW_TAG_lexical_block          = 0bH;
  DW_TAG_member                 = 0dH;
  DW_TAG_pointer_type           = 0fH;
  DW_TAG_reference_type         = 10H;
  DW_TAG_compile_unit           = 11H;
  DW_TAG_string_type            = 12H;
  DW_TAG_structure_type         = 13H;
  DW_TAG_subroutine_type        = 15H;
  DW_TAG_typedef                = 16H;
  DW_TAG_union_type             = 17H;
  DW_TAG_unspecified_parameters = 18H;
  DW_TAG_variant                = 19H;
  DW_TAG_common_block           = 1aH;
  DW_TAG_common_inclusion       = 1bH;
  DW_TAG_inheritance            = 1cH;
  DW_TAG_inlined_subroutine     = 1dH;
  DW_TAG_module                 = 1eH;
  DW_TAG_ptr_to_member_type     = 1fH;
  DW_TAG_set_type               = 20H;
  DW_TAG_subrange_type          = 21H;
  DW_TAG_with_stmt              = 22H;
  DW_TAG_access_declaration     = 23H;
  DW_TAG_base_type              = 24H;
  DW_TAG_catch_block            = 25H;
  DW_TAG_const_type             = 26H;
  DW_TAG_constant               = 27H;
  DW_TAG_enumerator             = 28H;
  DW_TAG_file_type              = 29H;
  DW_TAG_friend                 = 2aH;
  DW_TAG_namelist               = 2bH;
  DW_TAG_namelist_item          = 2cH;
  DW_TAG_packed_type            = 2dH;
  DW_TAG_subprogram             = 2eH;
  DW_TAG_template_type_param    = 2fH;
  DW_TAG_template_value_param   = 30H;
  DW_TAG_thrown_type            = 31H;
  DW_TAG_try_block              = 32H;
  DW_TAG_variant_part           = 33H;
  DW_TAG_variable               = 34H;
  DW_TAG_volatile_type          = 35H;
  DW_TAG_lo_user                = 04080H;
  DW_TAG_hi_user                = 0ffffH;

  DW_AT_sibling                 = 01H;
  DW_AT_location                = 02H;
  DW_AT_name                    = 03H;
  DW_AT_ordering                = 09H;
  DW_AT_byte_size               = 0bH;
  DW_AT_bit_offset              = 0cH;
  DW_AT_bit_size                = 0dH;
  DW_AT_stmt_list               = 10H;
  DW_AT_low_pc                  = 11H;
  DW_AT_high_pc                 = 12H;
  DW_AT_language                = 13H;
  DW_AT_discr                   = 15H;
  DW_AT_discr_value             = 16H;
  DW_AT_visibility              = 17H;
  DW_AT_import                  = 18H;
  DW_AT_string_length           = 19H;
  DW_AT_common_reference        = 1aH;
  DW_AT_comp_dir                = 1bH;
  DW_AT_const_value             = 1cH;
  DW_AT_containing_type         = 1dH;
  DW_AT_default_value           = 1eH;
  DW_AT_inline                  = 20H;
  DW_AT_is_optional             = 21H;
  DW_AT_lower_bound             = 22H;
  DW_AT_producer                = 25H;
  DW_AT_prototyped              = 27H;
  DW_AT_return_addr             = 2aH;
  DW_AT_start_scope             = 2cH;
  DW_AT_stride_size             = 2eH;
  DW_AT_upper_bound             = 2fH;
  DW_AT_abstract_origin         = 31H;
  DW_AT_accessibility           = 32H;
  DW_AT_address_class           = 33H;
  DW_AT_artifical               = 34H;
  DW_AT_base_types              = 35H;
  DW_AT_calling_convention      = 36H;
  DW_AT_count                   = 37H;
  DW_AT_data_member_location    = 38H;
  DW_AT_decl_column             = 39H;
  DW_AT_decl_file               = 3aH;
  DW_AT_decl_line               = 3bH;
  DW_AT_declaration             = 3cH;
  DW_AT_discr_list              = 3dH;
  DW_AT_encoding                = 3eH;
  DW_AT_external                = 3fH;
  DW_AT_frame_base              = 40H;
  DW_AT_friend                  = 41H;
  DW_AT_identifier_case         = 42H;
  DW_AT_macro_info              = 43H;
  DW_AT_namelist_item           = 44H;
  DW_AT_priority                = 45H;
  DW_AT_segment                 = 46H;
  DW_AT_specification           = 47H;
  DW_AT_static_link             = 48H;
  DW_AT_type                    = 49H;
  DW_AT_use_location            = 4aH;
  DW_AT_variable_parameter      = 4bH;
  DW_AT_virtuality              = 4cH;
  DW_AT_vtable_elem_location    = 4dH;
  DW_AT_lo_user                 = 02000H;
  DW_AT_hi_user                 = 03fffH;


  DW_CHILDREN_no                = 0;
  DW_CHILDREN_yes               = 1;

  DW_FORM_addr                  = 01H;
  DW_FORM_block2                = 03H;
  DW_FORM_block4                = 04H;
  DW_FORM_data2                 = 05H;
  DW_FORM_data4                 = 06H;
  DW_FORM_data8                 = 07H;
  DW_FORM_string                = 08H;
  DW_FORM_block                 = 09H;
  DW_FORM_block1                = 0aH;
  DW_FORM_data1                 = 0bH;
  DW_FORM_flag                  = 0cH;
  DW_FORM_sdata                 = 0dH;
  DW_FORM_strp                  = 0eH;
  DW_FORM_udata                 = 0fH;
  DW_FORM_ref_addr              = 10H;
  DW_FORM_ref1                  = 11H;
  DW_FORM_ref2                  = 12H;
  DW_FORM_ref4                  = 13H;
  DW_FORM_ref8                  = 14H;
  DW_FORM_ref_udata             = 15H;
  DW_FORM_indirect              = 16H;

  DW_ATE_address                = 01H;
  DW_ATE_boolean                = 02H;
  DW_ATE_complex_float          = 03H;
  DW_ATE_float                  = 04H;
  DW_ATE_signed                 = 05H;
  DW_ATE_signed_char            = 06H;
  DW_ATE_unsigned               = 07H;
  DW_ATE_unsigned_char          = 08H;
  DW_ATE_lo_user                = 80H;
  DW_ATE_hi_user                = 0FFH;

  DW_OP_addr                    = 03H;
  DW_OP_regx                    = 90H;
  DW_OP_fbreg                   = 91H;

  -- output segnents:
  SEG_DEBUG_INFO*               = 0;
  SEG_DEBUG_ABBREV*             = 1;
  SEG_DEBUG_ARANGES*            = 2;
  SEG_DEBUG_LOC*                = 3;
  SEG_DEBUG_LINE*               = 4;
  SEG_DEBUG_PUBNAMES*           = 5;
  SEG_DEBUG_STR*                = 6;
  SEG_DEBUG_MACINFO*            = 7;
  SEG_MAX                       = 7;

  -- Эти ty_* определены в дополнение к описанным в pcK.OB2
  -- PrimitiveTypeNo возвращает одно из pc.ty_* или ty_* значений [ + near32ptr]
  ty_boolean_1                  = pc.ty_boolean;
  ty_boolean_2                  = pc.ty_aux1;
  ty_boolean_4                  = pc.ty_aux2;
  ty_boolean_8                  = pc.ty_aux3;
  ty_char_1                     = pc.ty_char;
  ty_char_2                     = pc.ty_aux4;
  ty_char_4                     = pc.ty_aux5;
  ty_char_8                     = pc.ty_aux6;
  ty_pvoid_                     = pc.ty_aux7;
  ty_max_                       = ty_pvoid_;

  -- PrimitiveTypeNo прибавит near32ptr к типу, если это - указатель на него.
  near32ptr = 080H;

TYPE PARRINT32 = POINTER TO ARRAY OF INT32;

VAR
  aSegments*:    ARRAY SEG_MAX+1 OF cmd.CODE_SEGM;
  abbrCodeCount: LONGINT;

(*======================================================================================================*)
(* Debugger offsets table support *)
VAR
  dbgOffsTable     : PARRINT32;  -- Для всех индексов мы будем запоминать оффсеты
  dbgOffsTableSize : INT32;      -- Выделено элементов массива

PROCEDURE add_dbg_offset(idx : dbg.TYPE_INDEX);
-- Берет из типа его индекс и запоминает под этим индексом текущий дебаггерный оффсет
VAR
  i,l    : INT32;
  newTbl : PARRINT32;
BEGIN
  ASSERT(idx >= 0);
  WHILE dbgOffsTableSize < idx+1 DO
    IF dbgOffsTableSize=0 THEN l := 512;
    ELSE                       l := dbgOffsTableSize*2; END;
    NEW(newTbl, l);
    FOR i:=0 TO dbgOffsTableSize-1 DO newTbl[i] := dbgOffsTable[i]; END;
    FOR i:=dbgOffsTableSize TO l-1 DO newTbl[i] := -1; END;
    dbgOffsTable := newTbl;
    dbgOffsTableSize := l;
  END;
  dbgOffsTable[idx] := aSegments[SEG_DEBUG_INFO].code_len;
END add_dbg_offset;

PROCEDURE query_dbg_offset(idx : dbg.TYPE_INDEX) : INT32;
-- По индексу типа вернет ранее сопоставленный ему оффсет
-- (если это - базовый тип, то вернется указатель на определение DW_TAG_base_type
BEGIN
  ASSERT((idx>=0) AND (idx < dbgOffsTableSize) AND (dbgOffsTable[idx] # -1));
  RETURN dbgOffsTable[idx];
END query_dbg_offset;

(* При описании рекорда, дебаггерный оффсет содержащегося в нем поля типа рекорд, не описанного
   отдельным типом, неизвестен, есть только его индекс. Тут накапливаются записи с указанием
   оффсетов в .debug_info сегменте, куда следует прописать значения оффсетов соответствующих
   индексам. По мере генерации типов этих записей, они будут прописываться и вычеркиваться из списка. *)
TYPE
  FORWARDTYPESLIST = RECORD
                       idx     : dbg.TYPE_INDEX;
                       offsPtr : INT32;
                       next    : POINTER TO FORWARDTYPESLIST;
                     END;
  PFORWARDTYPESLIST = POINTER TO FORWARDTYPESLIST;

VAR pForwardList : PFORWARDTYPESLIST;

-- Запомнить текущее место для записи оффсета типа #idx
PROCEDURE addForwardPtr(idx: dbg.TYPE_INDEX);
VAR pNew : PFORWARDTYPESLIST;
BEGIN
  NEW (pNew);
  pNew^.idx     := idx;
  pNew^.offsPtr := aSegments[SEG_DEBUG_INFO].code_len;
  pNew^.next    := pForwardList;
  pForwardList  := pNew;
END addForwardPtr;

-- Прописать (если надо) оффсет dbgoffset типа #idx
PROCEDURE writeForwardPtr(idx: dbg.TYPE_INDEX);
VAR
  p         : PFORWARDTYPESLIST;
  dbgoffset : INT32;
BEGIN
  -- Приходится извращаться, так как нет поинтера на поинтер... :(
  dbgoffset := aSegments[SEG_DEBUG_INFO].code_len;
  WHILE (pForwardList # NIL) AND (pForwardList.idx = idx) DO
    cmd.move4b (SYSTEM.ADR (dbgoffset),
                SYSTEM.ADR (aSegments[SEG_DEBUG_INFO].bcode[pForwardList.offsPtr]),
                cmd.inverse_byte_order);
    pForwardList := pForwardList.next;
  END;
  p := pForwardList;
  WHILE (p # NIL) AND (p.next # NIL) DO
    IF (p.next.idx = idx) THEN
      cmd.move4b (SYSTEM.ADR (dbgoffset),
                  SYSTEM.ADR (aSegments[SEG_DEBUG_INFO].bcode[p.next.offsPtr]),
                  cmd.inverse_byte_order);
      p.next := p.next.next;
    ELSE
      p := p.next;
    END;
  END;
END writeForwardPtr;

(*======================================================================================================*)


PROCEDURE outLEB128(seg, val: CARD32);
VAR
  b   : SYSTEM.CARD8;
  old : cmd.CODE_SEGM;
BEGIN
  cmd.get_segm(old);
  cmd.set_segm(aSegments[seg]);
  REPEAT
    b   := VAL(SYSTEM.CARD8, val MOD 80H);
    val := val DIV 80H;
    IF val # 0 THEN b := b + VAL(SYSTEM.CARD8,80H); END;
    cmd.GenByte(b);
  UNTIL val=0;
  cmd.set_segm(old);
END outLEB128;

PROCEDURE outData1(seg : CARD32; val: SYSTEM.BYTE);
VAR
  old : cmd.CODE_SEGM;
BEGIN
  cmd.get_segm(old);
  cmd.set_segm(aSegments[seg]);
  cmd.GenByte(val);
  cmd.set_segm(old);
END outData1;

PROCEDURE outData2(seg : CARD32; val: INTEGER);
VAR
  old : cmd.CODE_SEGM;
BEGIN
  cmd.get_segm(old);
  cmd.set_segm(aSegments[seg]);
  cmd.GenWord(val);
  cmd.set_segm(old);
END outData2;

PROCEDURE outData4(seg : CARD32; val: LONGINT);
VAR
  old : cmd.CODE_SEGM;
BEGIN
  cmd.get_segm(old);
  cmd.set_segm(aSegments[seg]);
  cmd.GenLWord(val);
  cmd.set_segm(old);
END outData4;

PROCEDURE outString(seg: CARD32; str-: ARRAY OF CHAR);
VAR
  i   : INT32;
  old : cmd.CODE_SEGM;
BEGIN
  cmd.get_segm(old);
  cmd.set_segm(aSegments[seg]);
  FOR i := 0 TO LENGTH(str)-1 DO cmd.GenByte(str[i]) END;
  cmd.GenByte(0);
  cmd.set_segm(old);
END outString;


PROCEDURE out_AT_00();
  -- attribute series terminator
BEGIN
  outLEB128(SEG_DEBUG_ABBREV, 0);
  outLEB128(SEG_DEBUG_ABBREV, 0);
END out_AT_00;

PROCEDURE out_AT_4 (attr, val : INT32);
BEGIN
  outLEB128(SEG_DEBUG_ABBREV, attr);
  outLEB128(SEG_DEBUG_ABBREV, DW_FORM_data4);
  outData4 (SEG_DEBUG_INFO,   val);
END out_AT_4;

PROCEDURE out_AT_name (t : pc.STRUCT);
VAR name : ARRAY 256 OF CHAR;
BEGIN
  dbg.write_type_name(t, name);
  IF LENGTH(name)>0 THEN
    outLEB128(SEG_DEBUG_ABBREV, DW_AT_name);
    outLEB128(SEG_DEBUG_ABBREV, DW_FORM_string);
    outString(SEG_DEBUG_INFO, name);
  END;
END out_AT_name;

PROCEDURE out_AT_name_str (name : ARRAY OF CHAR);
BEGIN
  IF LENGTH(name)>0 THEN
    outLEB128(SEG_DEBUG_ABBREV, DW_AT_name);
    outLEB128(SEG_DEBUG_ABBREV, DW_FORM_string);
    outString(SEG_DEBUG_INFO, name);
  END;
END out_AT_name_str;

PROCEDURE out_AT_type (type : pc.STRUCT);
VAR offs : INT32;
BEGIN
  offs := query_dbg_offset(dbg.get_index(type));
  IF (offs # -1) THEN
    outLEB128(SEG_DEBUG_ABBREV, DW_AT_type);
    outLEB128(SEG_DEBUG_ABBREV, DW_FORM_ref4);
    outData4 (SEG_DEBUG_INFO,   offs);
  END;
END out_AT_type;


PROCEDURE out_AT_byte_size (len : INT32);
BEGIN
  outLEB128(SEG_DEBUG_ABBREV, DW_AT_byte_size);
  IF (len<100H) THEN
    outLEB128(SEG_DEBUG_ABBREV, DW_FORM_data1);
    outData1 (SEG_DEBUG_INFO,   SYSTEM.VAL(SYSTEM.BYTE,len));
  ELSE
    outLEB128(SEG_DEBUG_ABBREV, DW_FORM_data4);
    outData4 (SEG_DEBUG_INFO,   len);
  END;
END out_AT_byte_size;

PROCEDURE end_type;
BEGIN
  INC(dbg.write_type_cnt);
END end_type;


PROCEDURE out_o_AT_name (o : pc.OBJECT);
VAR name : ARRAY 256 OF CHAR;
BEGIN
  dbg.write_obj_name(o, name);
  IF LENGTH(name)>0 THEN
    outLEB128(SEG_DEBUG_ABBREV, DW_AT_name);
    outLEB128(SEG_DEBUG_ABBREV, DW_FORM_string);
    outString(SEG_DEBUG_INFO, name);
  END;
END out_o_AT_name;

PROCEDURE wr_far_adr(o: pc.OBJECT; offs : INT32);
VAR
  old : cmd.CODE_SEGM;
BEGIN
  cmd.get_segm(old);
  cmd.set_segm(aSegments[SEG_DEBUG_INFO]);
  cmd.gen_fixup(o, offs, cmd.fx_obj32(*far*));  -- 32-bit offset
  cmd.set_segm(old);
  outData4(SEG_DEBUG_INFO, 0);
END wr_far_adr;


CONST
  noreg = 0;
<* IF TARGET_386 THEN *>
CONST Reg2VCNum = dbg.Reg2NumTable{
        17,18,19,20,21,22,23,24,
        128,129,130,131,132,133,134,135,
        9,10,11,12,13,14,15,16,
        1,2,3,4,5,6,7,8,
        noreg,noreg,noreg,noreg,noreg,14,15,16,
        1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,
        1,1,1,1,1
};
(*<* ELSE *>
(* CV register enumerations -- see TIS, part 6 *)

(* 8-bit Registers *)  (* 16-bit Registers *)  (* 32-bit Registers *)
  AL = 1;                 AX = 9;                 EAX = 17;
  CL = 2;                 CX = 10;                ECX = 18;
  DL = 3;                 DX = 11;                EDX = 19;
  BL = 4;                 BX = 12;                EBX = 20;
  AH = 5;                 SP = 13;                ESP = 21;
  CH = 6;                 BP = 14;                EBP = 22;
  DH = 7;                 SI = 15;                ESI = 23;
  BH = 8;                 DI = 16;                EDI = 24;

(* Float Point Registers *)
  ST0 = 128;
  ST1 = 129;
  ST2 = 130;
  ST3 = 131;
  ST4 = 132;
  ST5 = 133;
  ST6 = 134;
  ST7 = 135;
*)
<* END *>

PROCEDURE register_num(r: pr.Reg): INT16;
BEGIN
<* IF ~TARGET_386 THEN *>
  RETURN noreg;
<* ELSE *>
  RETURN Reg2VCNum[r];

(* OBSOLETE
  CASE r OF
  | D.EAX: IF    sz = 1 THEN res := AL;
              ELSIF sz = 2 THEN res := AX;
              ELSE              res := EAX;
              END;
  | D.ECX: IF    sz = 1 THEN res := CL;
              ELSIF sz = 2 THEN res := CX;
              ELSE              res := ECX;
              END;
  | D.EDX: IF    sz = 1 THEN res := DL;
              ELSIF sz = 2 THEN res := DX;
              ELSE              res := EDX;
              END;
  | D.EBX: IF    sz = 1 THEN res := BL;
              ELSIF sz = 2 THEN res := BX;
              ELSE              res := EBX;
              END;
  | D.ESP: IF    sz = 1 THEN res := AH;
              ELSIF sz = 2 THEN res := SP;
              ELSE              res := ESP;
              END;
  | D.EBP: IF    sz = 1 THEN res := CH;
              ELSIF sz = 2 THEN res := BP;
              ELSE              res := EBP;
              END;
  | D.ESI: IF    sz = 1 THEN res := DH;
              ELSIF sz = 2 THEN res := SI;
              ELSE              res := ESI;
              END;
  | D.EDI: IF    sz = 1 THEN res := BH;
              ELSIF sz = 2 THEN res := DI;
              ELSE              res := EDI;
              END;
  | D.ST0..D.ST7:  res := ST0 + VAL(INTEGER, r) - VAL(INTEGER,D.ST0);
  ELSE res := noreg
  END;
  RETURN res;
*)
<* END *>
END register_num;



-- initSegments : создает сегменты.
PROCEDURE initSegments();
VAR i: INT32;
BEGIN
  FOR i:=0 TO SEG_MAX DO
    cmd.new_segm(aSegments[i]);
  END;
END initSegments;



PROCEDURE outSegInitialInfo();
CONST
  PrimTotal = 20;
TYPE
  PrimType  = RECORD
                idx      : dbg.TYPE_INDEX;
                szName   : ARRAY 20 OF CHAR;
                encoding : SYSTEM.CARD8;
                sizeof   : SYSTEM.CARD8;
              END;
  PrimArray = ARRAY PrimTotal OF PrimType;

CONST
  PrimArrayConst = PrimArray
  {
     PrimType { ORD(pc.ty_shortcard),    'SHORTCARD',    DW_ATE_unsigned,        1 },
     PrimType { ORD(pc.ty_cardinal),     'CARDINAL',     DW_ATE_unsigned,        2 },
     PrimType { ORD(pc.ty_longcard),     'LONGCARD',     DW_ATE_unsigned,        4 },
     PrimType { ORD(pc.ty_shortint),     'SHORTINT',     DW_ATE_signed,          1 },
     PrimType { ORD(pc.ty_integer),      'INTEGER',      DW_ATE_signed,          2 },
     PrimType { ORD(pc.ty_longint),      'LONGINT',      DW_ATE_signed,          4 },
     PrimType { ORD(pc.ty_real),         'REAL',         DW_ATE_float,           4 },
     PrimType { ORD(pc.ty_longreal),     'LONGREAL',     DW_ATE_float,           8 },
     PrimType { ORD(pc.ty_ld_real),      'LONGLONGREAL', DW_ATE_float,          10 },
     PrimType { ORD(pc.ty_complex),      'COMPLEX',      DW_ATE_complex_float,   4 },
     PrimType { ORD(pc.ty_lcomplex),     'LONGCOMPLEX',  DW_ATE_complex_float,   8 },
     PrimType { ORD(ty_boolean_1),       'BOOL8',        DW_ATE_boolean,         1 },
     PrimType { ORD(ty_boolean_2),       'BOOL16',       DW_ATE_boolean,         2 },
     PrimType { ORD(ty_boolean_4),       'BOOL32',       DW_ATE_boolean,         4 },
     PrimType { ORD(ty_boolean_8),       'BOOL64',       DW_ATE_boolean,         8 },
     PrimType { ORD(ty_char_1),          'CHAR',         DW_ATE_unsigned_char,   1 },
     PrimType { ORD(ty_char_2),          'CHAR16',       DW_ATE_unsigned_char,   2 },
     PrimType { ORD(ty_char_4),          'CHAR32',       DW_ATE_unsigned_char,   4 },
     PrimType { ORD(ty_char_8),          'CHAR64',       DW_ATE_unsigned_char,   8 },
     PrimType { ORD(ty_pvoid_),          'POINTER',      DW_ATE_address,         4 }
  };

VAR
  primarray : PrimArray;
  i         : INT32;
BEGIN
  primarray := PrimArrayConst;
  -- .debug_info header:
  outData4(SEG_DEBUG_INFO,     7); -- Segment length
  outData2(SEG_DEBUG_INFO,     2); -- DWARF Version 2
  outData4(SEG_DEBUG_INFO,     0); -- Offset in .debug_abbrev (always 0)
  outData1(SEG_DEBUG_INFO,     4); --?? Target archeticture address size
  -- .debug_aranges header:
  outData4(SEG_DEBUG_ARANGES,  7); -- Segment length
  outData2(SEG_DEBUG_ARANGES,  2); -- DWARF Version 2
  outData4(SEG_DEBUG_ARANGES,  0); -- Offset in .debug_info
  outData1(SEG_DEBUG_ARANGES,  4); --?? Target archeticture segment descriptor size
  -- .debug_pubnames header:
  outData4(SEG_DEBUG_PUBNAMES, 10);-- Segment length
  outData2(SEG_DEBUG_PUBNAMES, 2); -- DWARF Version 2
  outData4(SEG_DEBUG_PUBNAMES, 0); -- Offset in .debug_info
  outData4(SEG_DEBUG_PUBNAMES, 0); -- .debug_info size

  outData1(SEG_DEBUG_STR,      0); -- Nothing
  outData1(SEG_DEBUG_MACINFO,  0); --     more

  abbrCodeCount := 1;

  ------ Write primitive types --------
  outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
  outLEB128(SEG_DEBUG_ABBREV, DW_TAG_base_type);
  outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
  outLEB128(SEG_DEBUG_ABBREV, DW_AT_name);
  outLEB128(SEG_DEBUG_ABBREV, DW_FORM_string);
  outLEB128(SEG_DEBUG_ABBREV, DW_AT_encoding);
  outLEB128(SEG_DEBUG_ABBREV, DW_FORM_data1);
  outLEB128(SEG_DEBUG_ABBREV, DW_AT_byte_size);
  outLEB128(SEG_DEBUG_ABBREV, DW_FORM_data1);
  out_AT_00();
  -- pointer to primitive type
  outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount+1);
  outLEB128(SEG_DEBUG_ABBREV, DW_TAG_pointer_type);
  outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
  outLEB128(SEG_DEBUG_ABBREV, DW_AT_name);
  outLEB128(SEG_DEBUG_ABBREV, DW_FORM_string);
  outLEB128(SEG_DEBUG_ABBREV, DW_AT_type);
  outLEB128(SEG_DEBUG_ABBREV, DW_FORM_ref4);
  out_AT_00();
  FOR i := 0 TO PrimTotal-1 DO
    -- prim. type
    add_dbg_offset (primarray[i].idx);
    outLEB128      (SEG_DEBUG_INFO, abbrCodeCount);         -- abbrev. value
    outString      (SEG_DEBUG_INFO, primarray[i].szName);   -- type name
    outData1       (SEG_DEBUG_INFO, primarray[i].encoding); -- encoding
    outData1       (SEG_DEBUG_INFO, primarray[i].sizeof);   -- sizeof

    -- pointer to it
    add_dbg_offset (primarray[i].idx + near32ptr);
    outLEB128      (SEG_DEBUG_INFO, abbrCodeCount+1);       -- abbrev. value
    outString      (SEG_DEBUG_INFO, 'POINTER TO ');
    DEC(aSegments[SEG_DEBUG_INFO].code_len);                -- concatination :)
    outString      (SEG_DEBUG_INFO, primarray[i].szName);   -- 'POINTER TO <type name>'
    outData4       (SEG_DEBUG_INFO, query_dbg_offset(primarray[i].idx)); -- AT_TYPE
  END;
  INC(abbrCodeCount,2);

END outSegInitialInfo;

PROCEDURE outSegFinishInfo();
VAR l : LONGINT;
BEGIN
  outLEB128(SEG_DEBUG_ABBREV, 0); --> sibling chain terminator
  -- Write .debug_info size
  l := aSegments[SEG_DEBUG_INFO].code_len - 4;
  cmd.move4b (SYSTEM.ADR (l),
              SYSTEM.ADR (aSegments[SEG_DEBUG_INFO].bcode[0]),
              cmd.inverse_byte_order);
  -- Write .debug_aranges size
  l := aSegments[SEG_DEBUG_ARANGES].code_len - 4;
  cmd.move4b (SYSTEM.ADR (l),
              SYSTEM.ADR (aSegments[SEG_DEBUG_ARANGES].bcode[0]),
              cmd.inverse_byte_order);
  -- Write .debug_pubnames size
  l := aSegments[SEG_DEBUG_PUBNAMES].code_len - 4;
  cmd.move4b (SYSTEM.ADR (l),
              SYSTEM.ADR (aSegments[SEG_DEBUG_PUBNAMES].bcode[0]),
              cmd.inverse_byte_order);
END outSegFinishInfo;


PROCEDURE write_procedure(t: pc.STRUCT; idx : dbg.TYPE_INDEX);
BEGIN
  IF NOT ((t.obj = NIL) OR (t.obj.mode = pc.ob_type)) THEN
  ASSERT(dbg.get_index(t)=dbg.write_type_cnt);
  END;
  add_dbg_offset(idx);
  writeForwardPtr(idx); -- может потребоваться?
  outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
  outLEB128(SEG_DEBUG_INFO,   abbrCodeCount);
  INC(abbrCodeCount);
  outLEB128(SEG_DEBUG_ABBREV, DW_TAG_subroutine_type);   --> DW_TAG_subroutine_type
  outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
  out_AT_name (t);                                       --> DW_AT_name
  IF (t.base # NIL) THEN
    out_AT_type (t.base);                                --> DW_AT_type
  END;
  out_AT_00();
  end_type();
END write_procedure;


PROCEDURE field_filter (f: pc.OBJECT): BOOLEAN;
BEGIN
  CASE f.mode OF
  | pc.ob_field:
    RETURN TRUE;
  | pc.ob_field_bts:
    RETURN FALSE;
  END;
END field_filter;


PROCEDURE set_field_type (f: pc.OBJECT);
BEGIN
  dbg.emit_type(dbg.act_set, f.type)
END set_field_type;

PROCEDURE write_field (f: pc.OBJECT);
VAR
  byte_offs : LONGINT;
  dbg_offs  : LONGINT;
  name      : ARRAY 256 OF CHAR;
  idx       : dbg.TYPE_INDEX;
BEGIN
  CASE f.mode OF
  | pc.ob_field:     byte_offs := def.obj_offset(f);
  | pc.ob_field_bts: IF NOT dbg.whole_word_field(f, byte_offs) THEN RETURN; END;
  ELSE               ASSERT(FALSE);
  END;
  dbg.write_obj_name(f, name);
  outLEB128(SEG_DEBUG_ABBREV, DW_TAG_member);       --> DW_TAG_member
  outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
  IF LENGTH(name)>0 THEN
    outLEB128(SEG_DEBUG_ABBREV, DW_AT_name);        --> DW_AT_name
    outLEB128(SEG_DEBUG_ABBREV, DW_FORM_string);
    outString(SEG_DEBUG_INFO, name);
  END;
  idx      := dbg.get_index(f.type);
  dbg_offs := query_dbg_offset(idx);
  outLEB128(SEG_DEBUG_ABBREV, DW_AT_type);          --> DW_AT_type
  outLEB128(SEG_DEBUG_ABBREV, DW_FORM_ref4);
  IF (dbg_offs # -1) THEN
    outData4 (SEG_DEBUG_INFO,   dbg_offs);
  ELSE
    addForwardPtr(idx);
    outData4 (SEG_DEBUG_INFO,   0);                 (* пропишем потом, когда будем формировать этот тип (запись) *)
  END;
  out_AT_4 (DW_AT_data_member_location, byte_offs); --> DW_AT_data_member_location
  out_AT_00       ();
END write_field;

PROCEDURE write_field_type (f: pc.OBJECT);
BEGIN
  dbg.emit_type(dbg.act_write, f.type)
END write_field_type;


--------------------------------------------------------------------------

TYPE
  emit_rec    = RECORD (dbg.emit_rec)
                END;

  EMIT_DWARF  = POINTER TO emit_rec;


PROCEDURE (emit: EMIT_DWARF) TypeEmitter (ttag: dbg.TYPE_TAG; act: dbg.ACTION; type: pc.STRUCT);
VAR
  int_val_min: INT32;
  int_val_max: INT32;

  cns  : pc.OBJECT;
  val  : INT32;
  len  : INT32;
  temp : INT32;
  base : pc.STRUCT;
  inx  : dbg.TYPE_INDEX;
  name : ARRAY 256 OF CHAR;

BEGIN
  CASE ttag OF
  | dbg.ty_start:
    ASSERT((act = dbg.act_write) AND (type = NIL));
    dbg.type_cnt       := first_nonprimitive;
    dbg.write_type_cnt := first_nonprimitive;
    -- Начнем здесь Compile unit, закончится он в exi
    outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
    outLEB128(SEG_DEBUG_INFO,   abbrCodeCount);
    INC(abbrCodeCount);
    outLEB128(SEG_DEBUG_ABBREV, DW_TAG_compile_unit); --> DW_TAG_compile_unit
    outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_yes);
    outLEB128(SEG_DEBUG_ABBREV, DW_AT_name);          -->DW_AT_name
    outLEB128(SEG_DEBUG_ABBREV, DW_FORM_string);
    outString(SEG_DEBUG_INFO,   dbg.objname);
    -- outLEB128(SEG_DEBUG_ABBREV, DW_AT_language);   -->DW_AT_language --??
    out_AT_00();

  | dbg.ty_end:
    ASSERT((act = dbg.act_write) AND (type = NIL));
    dbg.type_cnt       := MAX(dbg.TYPE_INDEX);
    dbg.write_type_cnt := MAX(dbg.TYPE_INDEX);
  | dbg.ty_range:
    CASE act OF
    | dbg.act_set:
      dbg.emit_type(act, type.base);
      dbg.put_index(type);
    | dbg.act_write:
      dbg.emit_type(act, type.base);
      add_dbg_offset(dbg.get_index(type));
      writeForwardPtr(dbg.get_index(type));
      len  := pc.code.get_size(pc.su_bytes, type);
      IF type.base.mode IN pc.TY_SET{pc.ty_shortint, pc.ty_integer, pc.ty_longint} THEN
        int_val_min := type.min.get_integer();
        int_val_max := type.max.get_integer();
      ELSE
        int_val_min := SYSTEM.VAL(INT32,type.min.get_cardinal());
        int_val_max := SYSTEM.VAL(INT32,type.max.get_cardinal());
      END;
      outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
      outLEB128(SEG_DEBUG_INFO,   abbrCodeCount);
      INC(abbrCodeCount);
      outLEB128(SEG_DEBUG_ABBREV, DW_TAG_subrange_type); --> DW_TAG_subrange_type
      outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
      out_AT_name     (type);                            --> DW_AT_name
      out_AT_type     (type.base);                       --> DW_AT_type
      out_AT_byte_size(len);                             --> DW_AT_byte_size
      out_AT_4        (DW_AT_lower_bound, int_val_min);  --> DW_AT_lower_bound
      out_AT_4        (DW_AT_upper_bound, int_val_max);  --> DW_AT_upper_bound
      out_AT_00       ();
      end_type();
    END;
  | dbg.ty_enum:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      dbg.emit_type(act, type.base);
      dbg.put_index(type);
    | dbg.act_write:
      dbg.emit_type(act, type.base);
      add_dbg_offset(dbg.get_index(type));
      writeForwardPtr(dbg.get_index(type));
      len := pc.code.get_size(pc.su_bytes, type);
      outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
      outLEB128(SEG_DEBUG_INFO,   abbrCodeCount);
      INC(abbrCodeCount);
      outLEB128(SEG_DEBUG_ABBREV, DW_TAG_enumeration_type); --> DW_TAG_enumeration_type
      outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_yes);
      out_AT_name     (type);                               --> DW_AT_name
      out_AT_byte_size(len);                                --> DW_AT_byte_size
      out_AT_00       ();
      cns := type.prof;
      REPEAT
        ASSERT((cns.mode = pc.ob_cons) & (cns.val.mode = pc.nd_value));
        val := cns.val.val.get_integer();
        dbg.write_obj_name(cns, name);
        outLEB128(SEG_DEBUG_ABBREV, DW_TAG_enumerator);     --> DW_TAG_enumerator
        outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
        out_AT_name_str(name);                              --> DW_AT_NAME
        out_AT_4       (DW_AT_const_value, val);            --> DW_AT_const_value
        out_AT_00       ();
        cns := cns.next;
      UNTIL cns = NIL;
      outLEB128(SEG_DEBUG_ABBREV, 0);                       --> sibling chain terminator
      end_type();
    END;
  | dbg.ty_pointer,
    dbg.ty_reference:
    CASE act OF
    | dbg.act_set:
      ASSERT((ttag#dbg.ty_reference) AND (type#NIL));
      dbg.put_index(type);
      dbg.emit_type(act, type.base);
    | dbg.act_write:
      IF type = NIL THEN
        add_dbg_offset(dbg.write_type_cnt);
      ELSE
        add_dbg_offset(dbg.get_index(type));
      END;
      outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
      outLEB128(SEG_DEBUG_INFO,   abbrCodeCount);
      INC(abbrCodeCount);
      IF (ttag = dbg.ty_pointer) THEN
        outLEB128(SEG_DEBUG_ABBREV, DW_TAG_pointer_type);   --> DW_TAG_pointer_type
      ELSE
        outLEB128(SEG_DEBUG_ABBREV, DW_TAG_reference_type); --> DW_TAG_reference_type
      END;
      outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
      IF type = NIL THEN
        -- сылка на тип, индекс которого лежит сейчас в tindex_transfer
        outLEB128(SEG_DEBUG_ABBREV, DW_AT_type);
        outLEB128(SEG_DEBUG_ABBREV, DW_FORM_ref4);
        outData4 (SEG_DEBUG_INFO,   query_dbg_offset(dbg.tindex_transfer)); --> AT_TYPE
        end_type();
      ELSE
        writeForwardPtr(dbg.get_index(type));
        base := type.base;
        ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
        out_AT_name (type);                                 --> DW_AT_name
        outLEB128   (SEG_DEBUG_ABBREV, DW_AT_type);         --> DW_AT_type
        outLEB128   (SEG_DEBUG_ABBREV, DW_FORM_ref4);
        temp := aSegments[SEG_DEBUG_INFO].code_len;         --> AT_type value offset
        outData4 (SEG_DEBUG_INFO,   0);                     --> Place to write AT_type value
        end_type();
        dbg.emit_type(act, base);
        val := query_dbg_offset(dbg.get_index(base));
        cmd.move4b (SYSTEM.ADR (val),                       --> Write AT_type value
                    SYSTEM.ADR (aSegments[SEG_DEBUG_INFO].bcode[temp]),
                    cmd.inverse_byte_order);
      END;
      out_AT_00();
    END;
  | dbg.ty_opaque:
    CASE act OF
    | dbg.act_set:
      dbg.emit_type(act, type.base);
      IF dbg.get_index (type) <= 0 THEN
        dbg.put_index_val(type, dbg.get_index(type.base));
      END;
    | dbg.act_write:
      IF dbg.get_index (type.base) >= dbg.write_type_cnt THEN
        emit.TypeEmitter (dbg.ty_pointer, act, type.base);
      END;
    END;
  | dbg.ty_set:
    CASE act OF
    | dbg.act_set:
      dbg.emit_type(act, type.base);
      dbg.put_index(type);
    | dbg.act_write:
      dbg.emit_type(act, type.base);
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      add_dbg_offset(dbg.get_index(type));
      writeForwardPtr(dbg.get_index(type));
      len := pc.code.get_size(pc.su_bytes, type);
      outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
      outLEB128(SEG_DEBUG_INFO,   abbrCodeCount);
      INC(abbrCodeCount);
      outLEB128(SEG_DEBUG_ABBREV, DW_TAG_set_type); --> DW_TAG_set_type
      outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
      out_AT_name     (type);                       --> DW_AT_name
      out_AT_type     (type.base);                  --> DW_AT_type
      out_AT_byte_size(len);                        --> DW_AT_byte_size
      out_AT_00       ();
      end_type();
    END;
  | dbg.ty_proctype:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      IF (type.obj = NIL) OR (type.obj.mode = pc.ob_type) THEN
        dbg.put_index(type); -- idx == pointer, idx+1 == proc
      ELSE
        dbg.put_index_val(type, dbg.type_cnt);
      END;
      INC(dbg.type_cnt);       -- LF_PROCEDURE
    | dbg.act_write:
      IF (type.obj = NIL) OR (type.obj.mode = pc.ob_type) THEN
      -- дополнительно пишем ук-ль на процедуру, idx(proc) будет на 1 больше
        ASSERT( dbg.get_index(type)=dbg.write_type_cnt);
        add_dbg_offset(dbg.get_index(type));
        outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
        outLEB128(SEG_DEBUG_INFO,   abbrCodeCount);
        INC(abbrCodeCount);
        outLEB128(SEG_DEBUG_ABBREV, DW_TAG_pointer_type); --> DW_TAG_pointer_type
        outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
        out_AT_name (type);                                 --> DW_AT_name
        outLEB128   (SEG_DEBUG_ABBREV, DW_AT_type);         --> DW_AT_type
        outLEB128   (SEG_DEBUG_ABBREV, DW_FORM_ref4);
        temp := aSegments[SEG_DEBUG_INFO].code_len;         --> AT_type value offset
        outData4 (SEG_DEBUG_INFO,   0);                     --> Place to write AT_type value
        out_AT_00       ();
        end_type();
        -- И саму процедуру с индексом на 1 бОльшим
        write_procedure(type,dbg.get_index(type)+1);
        -- Пропишем оффсет
        val := query_dbg_offset(dbg.get_index(type));
        cmd.move4b (SYSTEM.ADR (val),                       --> Write AT_type value
                    SYSTEM.ADR (aSegments[SEG_DEBUG_INFO].bcode[temp]),
                    cmd.inverse_byte_order);
      ELSE
        write_procedure(type,dbg.get_index(type));
      END;
    END;
  | dbg.ty_array,      -- обычный типизированный статический массив
    dbg.ty_array_of,   -- POINTER TO ARRAY OF <type>
    dbg.ty_open_array: -- параметр типа ARRAY OF <type>
    dbg.emit_type(act, type.base);
    IF ttag = dbg.ty_array THEN
      dbg.emit_type(act, type.inx.base); -- сам тип .inx расписывается тут же
    END;
    CASE act OF
    | dbg.act_set:
      IF NOT (at.tmark_db_index IN type.marks) THEN
        dbg.put_index(type);          -- LF_ARRAY
      END;
    | dbg.act_write:
      inx := dbg.get_index(type);
      IF inx = dbg.write_type_cnt THEN
        add_dbg_offset(inx);
        writeForwardPtr(inx);
        outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
        outLEB128(SEG_DEBUG_INFO,   abbrCodeCount);
        INC(abbrCodeCount);
        outLEB128   (SEG_DEBUG_ABBREV, DW_TAG_array_type);  --> DW_TAG_array_type
        outData1    (SEG_DEBUG_ABBREV, DW_CHILDREN_yes);
        out_AT_name (type);                                 --> DW_AT_name
        out_AT_type (type.base);                            --> DW_AT_type
        out_AT_00   ();
        (*>
          > Пишем дочерний тэг (DW_TAG_enumeration_type или DW_TAG_subrange_type), описывающий тип индекса.
          >    dbg.ty_array       - нормальный индекс
          >    dbg.ty_array_of    - поставим тип индекса LONGINT, range [1..0]
          >    dbg.ty_open_array: - поставим тип индекса LONGINT, range [2..0]
          >*)
        IF (ttag = dbg.ty_array) AND (type.inx.mode = pc.ty_enum) THEN
          -- write enumeration type
          len := pc.code.get_size(pc.su_bytes, type.inx);
          outLEB128(SEG_DEBUG_ABBREV, DW_TAG_enumeration_type); --> DW_TAG_enumeration_type
          outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_yes);
          out_AT_name     (type.inx);                           --> DW_AT_name
          out_AT_byte_size(len);                                --> DW_AT_byte_size
          out_AT_00       ();
          cns := type.inx.prof;
          REPEAT
            ASSERT((cns.mode = pc.ob_cons) & (cns.val.mode = pc.nd_value));
            val := cns.val.val.get_integer();
            dbg.write_obj_name(cns, name);
            outLEB128(SEG_DEBUG_ABBREV, DW_TAG_enumerator);     --> DW_TAG_enumerator
            outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
            out_AT_name_str(name);                              --> DW_AT_NAME
            out_AT_4       (DW_AT_const_value, val);            --> DW_AT_const_value
            out_AT_00       ();
            cns := cns.next;
          UNTIL cns = NIL;
          outLEB128(SEG_DEBUG_ABBREV, 0);                       --> sibling chain terminator
          -- enumeration: done
        ELSE
          outLEB128(SEG_DEBUG_ABBREV, DW_TAG_subrange_type);    --> DW_TAG_subrange_type
          outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
          len := 4;
          IF (ttag = dbg.ty_array) THEN
            ASSERT (type.inx.mode = pc.ty_range);
            len  := pc.code.get_size(pc.su_bytes, type.inx);
            IF type.inx.base.mode IN pc.TY_SET{pc.ty_shortint, pc.ty_integer, pc.ty_longint} THEN
              int_val_min := type.inx.min.get_integer();
              int_val_max := type.inx.max.get_integer();
            ELSE
              int_val_min := SYSTEM.VAL(INT32,type.inx.min.get_cardinal());
              int_val_max := SYSTEM.VAL(INT32,type.inx.max.get_cardinal());
            END;
            out_AT_name     (type.inx);                        --> DW_AT_name
            out_AT_type     (type.inx.base);                   --> DW_AT_type
          ELSE
            out_AT_4 (DW_AT_type, query_dbg_offset(ORD(pc.ty_longint))); --> DW_AT_type
            int_val_min := 1;
            int_val_max := 0;
            IF (ttag = dbg.ty_open_array) THEN int_val_min := 2; END;
          END;
          out_AT_byte_size(len);                               --> DW_AT_byte_size
          out_AT_4        (DW_AT_lower_bound, int_val_min);    --> DW_AT_lower_bound
          out_AT_4        (DW_AT_upper_bound, int_val_max);    --> DW_AT_upper_bound
          out_AT_00       ();
        END;
        outLEB128(SEG_DEBUG_ABBREV, 0);                       --> sibling chain terminator
        end_type();
      ELSE
        ASSERT(inx < dbg.write_type_cnt);
      END;
    END;
  | dbg.ty_SS: -- ARAY OF CHAR известной длины
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);  -- LF_ARRAY OF CHAR;
    | dbg.act_write:
      add_dbg_offset(dbg.get_index(type));
      writeForwardPtr(dbg.get_index(type));
      outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
      outLEB128(SEG_DEBUG_INFO,   abbrCodeCount);
      INC(abbrCodeCount);
      outLEB128(SEG_DEBUG_ABBREV, DW_TAG_array_type);       --> DW_TAG_array_type
      outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_yes);
      out_AT_4 (DW_AT_type, query_dbg_offset(ORD(ty_char_1)));   --> DW_AT_type
      out_AT_00   ();
      -- Write index type
      int_val_max := pc.code.get_size(pc.su_bytes, type)-1;
      outLEB128(SEG_DEBUG_ABBREV, DW_TAG_subrange_type); --> DW_TAG_subrange_type
      outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
      out_AT_4 (DW_AT_type, query_dbg_offset(ORD(pc.ty_longcard)));  --> DW_AT_type
      out_AT_4        (DW_AT_lower_bound, 0);            --> DW_AT_lower_bound
      out_AT_4        (DW_AT_upper_bound, int_val_max);  --> DW_AT_upper_bound
      out_AT_00       ();
      outLEB128(SEG_DEBUG_ABBREV, 0);                    --> sibling chain terminator
      end_type();
    END;
  | dbg.ty_record:
    IF type.base # NIL THEN
      dbg.emit_type(act, type.base);
    END;
    CASE act OF
    | dbg.act_set:
      IF NOT (at.tmark_db_index IN type.marks) THEN
        dbg.put_index(type);
        dbg.field_list (type.prof, field_filter, set_field_type);
      END;
    | dbg.act_write:
      inx := dbg.get_index(type);
      IF inx = dbg.write_type_cnt THEN
        add_dbg_offset(inx);
        writeForwardPtr(inx);
        outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
        outLEB128(SEG_DEBUG_INFO,   abbrCodeCount);
        INC(abbrCodeCount);
        outLEB128   (SEG_DEBUG_ABBREV, DW_TAG_structure_type); --> DW_TAG_structure_type
        outData1    (SEG_DEBUG_ABBREV, DW_CHILDREN_yes);
        out_AT_name (type);                                    --> DW_AT_name

        IF (type.base # NIL) THEN
          outLEB128   (SEG_DEBUG_ABBREV, DW_TAG_inheritance);    --> DW_TAG_inheritance
          outData1    (SEG_DEBUG_ABBREV, DW_CHILDREN_no);        -->
          out_AT_type (type.base);                               --> DW_AT_type (parent calss)
          out_AT_4    (DW_AT_data_member_location, 0);           --> DW_AT_data_member_location
          out_AT_00   ();
        END;

        len  := pc.code.get_size(pc.su_bytes, type);
        IF (len # -1) THEN
          out_AT_byte_size(len);                               --> DW_AT_byte_size
        END;
        out_AT_00();
        dbg.field_list (type.prof, field_filter, write_field);
        outLEB128(SEG_DEBUG_ABBREV, 0);                        --> sibling chain terminator
        end_type ();
        dbg.field_list (type.prof, field_filter, write_field_type);
      ELSE
        ASSERT(inx < dbg.write_type_cnt);
      END;
    END;
  | dbg.ty_module:
    CASE act OF
    | dbg.act_set:
      emit.TypeEmitter (dbg.ty_proctype, act, type);
    | dbg.act_write:
      dbg.emit_type(act, type.base);
      write_procedure(type,dbg.get_index(type));
    END;
  END;
END TypeEmitter;


PROCEDURE (emit: EMIT_DWARF) SymbEmitter (stag: dbg.SYMB_TAG; o: pc.OBJECT; tind: dbg.TYPE_INDEX);
VAR
  type: pc.STRUCT;
  a   : at.ATTR_EXT;
  db  : at.DBG_EXT;
  i,l : INT32;
BEGIN
  CASE stag OF
  | dbg.sy_start:
    ASSERT((o=NIL) AND (tind=-1));

  | dbg.sy_end:
    ASSERT((o=NIL) AND (tind=-1));

  | dbg.sy_var: -- Global variable
    outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
    outLEB128(SEG_DEBUG_INFO,   abbrCodeCount);
    INC(abbrCodeCount);
    outLEB128(SEG_DEBUG_ABBREV, DW_TAG_variable);   --> DW_TAG_variable
    outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
    out_o_AT_name  (o);                             --> DW_AT_name
    outLEB128(SEG_DEBUG_ABBREV, DW_AT_type);        --> DW_AT_type
    outLEB128(SEG_DEBUG_ABBREV, DW_FORM_ref4);
    outData4 (SEG_DEBUG_INFO,   query_dbg_offset(tind));
    IF o.is_public() THEN
      outLEB128(SEG_DEBUG_ABBREV, DW_AT_external);  --> DW_AT_external
      outLEB128(SEG_DEBUG_ABBREV, DW_FORM_flag);
      outData1 (SEG_DEBUG_INFO,   1);
    END;
    outLEB128 (SEG_DEBUG_ABBREV, DW_AT_location);   --> DW_AT_location ...
    outLEB128 (SEG_DEBUG_ABBREV, DW_FORM_indirect);
    outData1  (SEG_DEBUG_INFO,   DW_FORM_block2);       -- Block:
    outData2  (SEG_DEBUG_INFO,   5);                    --   5 bytes:
    outData1  (SEG_DEBUG_INFO,   DW_OP_addr);           --     DW_OP_addr
    wr_far_adr(o, 0);                                   --     00 00 00 00 & fixup it
    out_AT_00 ();

  | dbg.sy_proc:
    outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
    outLEB128(SEG_DEBUG_INFO,   abbrCodeCount);
    INC(abbrCodeCount);
    IF (o.mode = pc.ob_module) THEN
      outLEB128(SEG_DEBUG_ABBREV, DW_TAG_module);     --> DW_TAG_module
      outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_yes);
    ELSE
      outLEB128(SEG_DEBUG_ABBREV, DW_TAG_subprogram); --> DW_TAG_subprogram
      outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_yes);
      IF o.is_public() THEN
        outLEB128(SEG_DEBUG_ABBREV, DW_AT_external);  --> DW_AT_external
        outLEB128(SEG_DEBUG_ABBREV, DW_FORM_flag);
        outData1 (SEG_DEBUG_INFO,   1);
      END;
      type := o.type;
      IF (type # NIL) AND (type.base # NIL) THEN
        out_AT_type (type.base);                      --> DW_AT_type
      END;
      --?? DW_AT_return_addr   - оно
      --?? DW_AT_static_link   -    нам
      --?? DW_AT_frame_base    -       надо?
    END;
    out_o_AT_name  (o);                             --> DW_AT_name
    outLEB128(SEG_DEBUG_ABBREV, DW_AT_low_pc);      --> DW_AT_low_pc
    outLEB128(SEG_DEBUG_ABBREV, DW_FORM_addr);
    wr_far_adr(o, 0);                                   -- offset = 0                     & fixup it
    outLEB128(SEG_DEBUG_ABBREV, DW_AT_high_pc);     --> DW_AT_high_pc
    outLEB128(SEG_DEBUG_ABBREV, DW_FORM_addr);
    wr_far_adr(o, dbg.end_proc_debug(o));               -- offset = dbg.end_proc_debug(o) & fixup it
    out_AT_00       ();

  | dbg.sy_scope_open:
    ASSERT(tind = -1);

  | dbg.sy_scope_close:
    ASSERT(tind = -1);
    outLEB128(SEG_DEBUG_ABBREV, 0);                     --> sibling chain terminator

  | dbg.sy_local_var,
    dbg.sy_param:
    a := at.attr(o.ext, at.a_dbg);
    ASSERT(a # NIL);
    db := a(at.DBG_EXT);
    type := o.type;
    outLEB128(SEG_DEBUG_ABBREV, abbrCodeCount);
    outLEB128(SEG_DEBUG_INFO,   abbrCodeCount);
    INC(abbrCodeCount);
    IF (stag = dbg.sy_param) THEN
      outLEB128(SEG_DEBUG_ABBREV, DW_TAG_formal_parameter);   --> DW_TAG_formal_parameter
    ELSE
      outLEB128(SEG_DEBUG_ABBREV, DW_TAG_variable);           --> DW_TAG_variable
    END;
    outData1 (SEG_DEBUG_ABBREV, DW_CHILDREN_no);
    IF (stag = dbg.sy_param) AND (o.mode = pc.ob_varpar) THEN
      outLEB128(SEG_DEBUG_ABBREV, DW_AT_variable_parameter);  --> DW_AT_variable_parameter
      outLEB128(SEG_DEBUG_ABBREV, DW_FORM_flag);
      outData1 (SEG_DEBUG_INFO,   1);
    END;
    out_o_AT_name  (o);                                       --> DW_AT_name
    outLEB128(SEG_DEBUG_ABBREV, DW_AT_type);                  --> DW_AT_type
    outLEB128(SEG_DEBUG_ABBREV, DW_FORM_ref4);
    outData4 (SEG_DEBUG_INFO,   query_dbg_offset(tind));
    IF o.is_public() THEN
      outLEB128(SEG_DEBUG_ABBREV, DW_AT_external);            --> DW_AT_external
      outLEB128(SEG_DEBUG_ABBREV, DW_FORM_flag);
      outData1 (SEG_DEBUG_INFO,   1);
    END;
    outLEB128(SEG_DEBUG_ABBREV, DW_AT_location);              --> DW_AT_location ...
    outLEB128(SEG_DEBUG_ABBREV, DW_FORM_indirect);
    outData1 (SEG_DEBUG_INFO,   DW_FORM_block4);                -- Block4:
    i := aSegments[SEG_DEBUG_INFO].code_len;                    -- write block size here
      outData4(SEG_DEBUG_INFO, 0);                              -- place to write block size
    IF db.list[0].info.Location = ir.LocInReg THEN
      outData1 (SEG_DEBUG_INFO, DW_OP_regx);                                                               -- DW_OP_regx
      outLEB128(SEG_DEBUG_INFO, register_num(VAL(pr.Reg,db.list[0].info.Value)(*, pc.code.get_size(pc.su_bytes, type)*))); -- register num.
    ELSE
      outData1 (SEG_DEBUG_INFO, DW_OP_fbreg);
      outLEB128(SEG_DEBUG_INFO, db.list[0].info.Value);         -- DW_OP_fbreg, offset
    END;
    l := aSegments[SEG_DEBUG_INFO].code_len - i - 4;
    cmd.move4b (SYSTEM.ADR (l),
                SYSTEM.ADR (aSegments[SEG_DEBUG_INFO].bcode[i]),
                cmd.inverse_byte_order);                        -- write down block size
    out_AT_00();
  ELSE
  END;
END SymbEmitter;


PROCEDURE (emit: EMIT_DWARF) PrimitiveTypeNo (type: pc.STRUCT): dbg.TYPE_INDEX;
VAR
  ptr: BOOLEAN;
  inx: dbg.TYPE_INDEX;
  sz:  INT32;
BEGIN
  IF (type.mode = pc.ty_opaque) AND (type.base # NIL) THEN
    type := type.base;
  END;
  ptr := (type.mode = pc.ty_pointer);
  IF ptr THEN type := type.base; END;
  CASE type.mode OF
  | pc.ty_shortcard,
    pc.ty_cardinal,
    pc.ty_longcard,
    pc.ty_shortint,
    pc.ty_integer,
    pc.ty_longint,
    pc.ty_real,
    pc.ty_longreal,
    pc.ty_ld_real,
    pc.ty_complex,
    pc.ty_lcomplex  : inx := ORD(type.mode);

  | pc.ty_boolean   : sz := def.type_size(type.base);
                      IF    sz = 1 THEN    inx := ORD(ty_boolean_1);
                      ELSIF sz = 2 THEN    inx := ORD(ty_boolean_2);
                      ELSIF sz = 4 THEN    inx := ORD(ty_boolean_4);
                      ELSE ASSERT(sz = 8); inx := ORD(ty_boolean_8);
                      END;
  | pc.ty_char      : sz := def.type_size(type.base);
                      IF    sz = 1 THEN    inx := ORD(ty_char_1);
                      ELSIF sz = 2 THEN    inx := ORD(ty_char_2);
                      ELSE ASSERT(sz = 4); inx := ORD(ty_char_4);
                      END;
  | pc.ty_AA        : IF ptr THEN inx := -1;
                      ELSE ptr := TRUE; inx := ORD(ty_pvoid_); (* void *)
                      END;
  | pc.ty_opaque    : IF ptr THEN inx := -1
                      ELSE ptr := TRUE; inx := ORD(ty_pvoid_); (* void *)
                      END;
  | pc.ty_pointer   : inx := -1;
  | pc.ty_loc       : IF ptr AND (type.mno < pc.ZEROMno) THEN
                        inx := ORD(ty_pvoid_);
                      ELSE
                        inx := ORD(pc.ty_shortcard);
                      END;
  | pc.ty_protection: inx := ORD(pc.ty_cardinal);
  | pc.ty_void      : inx := ORD(ty_pvoid_); (* void *)
  ELSE
    inx := -1;
  END;
  IF (inx >= 0) & ptr THEN INC(inx, near32ptr); END;
  RETURN inx
END PrimitiveTypeNo;

PROCEDURE (emit: EMIT_DWARF) generate (): BOOLEAN;
BEGIN
  initSegments();
  outSegInitialInfo();
  emit.generate^;
  outLEB128(SEG_DEBUG_ABBREV, 0); --> sibling chain terminator (DW_TAG_compile_unit)
  outSegFinishInfo();
  RETURN TRUE;
END generate;

---+--+--+--+--+--+--+--+---

VAR
  emitDWARF : EMIT_DWARF;
  i         : LONGINT;

BEGIN
  dbgOffsTable     := NIL;
  dbgOffsTableSize := 0;
  pForwardList     := NIL;
  FOR i:=0 TO SEG_MAX DO
    aSegments[i] := NIL;
  END;
  NEW(emitDWARF);
  reg.Register(opt.dbgFormat, opt.dbg_DWARF, emitDWARF);
END dbgDWARF.



