MODULE dbgHLL;

IMPORT sys := SYSTEM;

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
IMPORT SYSTEM;

<* IF TARGET_386 THEN *>
IMPORT D := desc386;
TYPE Reg = D.Reg;
<* ELSE *>
TYPE Reg = SHORTINT;
<* END *>


-- IMPLEMENTATION --

TYPE
  INT8  = sys.INT8;
  INT16 = sys.INT16;
  INT32 = sys.INT32;


CONST
  first_nonprimitive = 512;


------------- S y m b o l s --------------------------------------------

CONST
  S_COMPILE = 0001H;     (* Compile Flag *)
  S_OBJECT  = 0009H;     (* Object File Name *)
  S_LDATA32 = 0201H;     (* Local Data *)
  S_LPROC32 = 0204H;     (* Local Procedure Start *)

  S_PROC32  = 01H;       (* Procedure Start           *)
  S_END     = 02H;       (* End of block or Procedure *)
  S_GDATA32 = 05;        (* Static variable           *)

  EA_PROC   = 0EAH;      (* Procedure extended attributes *)


(* ----- <type> type constanst ----- *)

  void_type        = 97H;
  signed_type      = 80H;
  long_signed_type = 9BH;
  long_unsigned_type = 9CH;
  unsign_type      = 84H;
  real_type        = 88H;
  complex_type     = 8CH;
  boolean_type     = 90H;
  character_type  = 94H;

(* ----- <size> type constants ----- *)

  size_1_byte  = 0H;        (* --- sign/unsign, boolean --- *)
  size_2_bytes = 1H;
  size_4_bytes = 2H;

  size_real32  = 0H;        (* --- real, complex --- *)
  size_real64  = 1H;
  size_real80  = 2H;

  near32ptr = 020H;
  near32bit = 8;

  Auto = 04H; -- var location
  Register  = 0DH; -- var location
  RegRelative= 20H; -- var location

  FID_index    = 083H;
  FID_string   = 082H;

  LF_ARRAY     = 078H;
  LF_RANGE     = 06FH;
  LF_RECORD    = 079H;
  LF_CLASS     = 040H;
  LF_BASECLASS = 041H;
  LF_MEMBER    = 046H;
  LF_LIST      = 07FH;
  LF_SET       = 052H;
  LF_ENUM      = 07BH;
  LF_POINTER   = 07AH;
  LF_REFERENCE = 048H;
  LF_PROCEDURE = 075H;


CONST
  noreg = 0;
<* IF TARGET_386 THEN *>
CONST Reg2HLLNum = dbg.Reg2NumTable{
        16,17,18,19,20,21,22,23,
        128,129,130,131,132,133,134,135,
        8,9,10,11,12,13,14,15,
        0,1,2,3,4,5,6,7,
        noreg,noreg,noreg,noreg,noreg,14,15,16,
        1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,
        1,1,1,1,1
};
<* ELSE *>
-- 8-bit Registers      -- 16-bit Registers     -- 32-bit Registers
  AL = 0;                 AX = 8;                 EAX = 16;
  CL = 1;                 CX = 9;                 ECX = 17;
  DL = 2;                 DX = 10;                EDX = 18;
  BL = 3;                 BX = 11;                EBX = 19;
  AH = 4;                 SP = 12;                ESP = 20;
  CH = 5;                 BP = 13;                EBP = 21;
  DH = 6;                 SI = 14;                ESI = 22;
  BH = 7;                 DI = 15;                EDI = 23;

(* Float Point Registers *)
  ST0 = 80H;
  ST1 = 81H;
  ST2 = 82H;
  ST3 = 83H;
  ST4 = 84H;
  ST5 = 85H;
  ST6 = 86H;
  ST7 = 87H;
<* END *>

PROCEDURE register_num (r: Reg; sz: LONGINT): INTEGER;
VAR
  res: INT16;
BEGIN
<* IF TARGET_386 THEN *>
   RETURN Reg2HLLNum[r];
<* ELSE *>
  RETURN noreg;
<* END *>
END register_num;


VAR
  symb_start: INT32;

PROCEDURE begin_symb(tag: sys.BYTE);
BEGIN
  symb_start := dbg.symb_info.code_len;
  cmd.GenWord(0);  -- to reserve place for record length
  cmd.GenByte(tag);
END begin_symb;


PROCEDURE end_symb;
VAR
 len: INT32;
BEGIN
  len := dbg.symb_info.code_len - (symb_start+2);
  ASSERT(len > 0);
  INC(len, 8000H);
  dbg.symb_info.bcode[symb_start]   := sys.VAL(sys.BYTE, len DIV 256);
  dbg.symb_info.bcode[symb_start+1] := sys.VAL(sys.BYTE, len MOD 256);
  symb_start := -1;
END end_symb;


VAR
  length_pos: INT32;

PROCEDURE begin_type(tag: SHORTINT);
BEGIN
  length_pos := dbg.type_info.code_len;
  cmd.GenWord(0);
  cmd.GenByte(tag);
END begin_type;


PROCEDURE end_type;
VAR
  len: INT16;
BEGIN
  len := VAL (INT16, dbg.type_info.code_len - (length_pos+2));
  ASSERT(len > 0);
  cmd.move2b (sys.ADR(len), sys.ADR(dbg.type_info.bcode[length_pos]), cmd.inverse_byte_order);
  INC(dbg.write_type_cnt);
END end_type;


PROCEDURE gen_Card_FID_span(len: LONGINT);
BEGIN
  IF len < 256 THEN
    cmd.GenByte(08BH); cmd.GenByte(SYSTEM.VAL(SHORTINT,len));
  ELSIF len < 256*256 THEN
    cmd.GenByte(085H); cmd.GenWord(SYSTEM.VAL(INTEGER,len));
  ELSE
    cmd.GenByte(086H); cmd.GenLWord(len);
  END;
END gen_Card_FID_span;


PROCEDURE wr_far_adr(o: pc.OBJECT);
BEGIN
  cmd.gen_fixup(o, 0, cmd.fx_obj32far);  -- 32-bit offset
  cmd.GenWord(0);                        -- 16-bit segment
END wr_far_adr;


VAR
  IsXHLL: BOOLEAN;

PROCEDURE write_name(nm-: ARRAY OF CHAR);
VAR
  ln, i: INT32;
BEGIN
  ln := LENGTH(nm);
  IF IsXHLL THEN
    cmd.GenWord(VAL(INT16, ln));
  ELSE
    IF ln > 255 THEN
      ln := 255;
    END;
    cmd.GenByte(VAL(INT8, ln));
  END;
  FOR i := 0 TO ln-1 DO
    cmd.GenByte(nm[i]);
  END;
END write_name;


PROCEDURE write_obj_name (o: pc.OBJECT);
VAR
  name: ARRAY 1024 OF CHAR;
BEGIN
  dbg.write_obj_name(o, name);
  write_name(name);
END write_obj_name;


PROCEDURE write_type_name (t: pc.STRUCT);
VAR
  name: ARRAY 1024 OF CHAR;
BEGIN
  dbg.write_type_name(t, name);
  write_name(name);
END write_type_name;


PROCEDURE write_procedure(t: pc.STRUCT);
BEGIN
  IF NOT ((t.obj = NIL) OR (t.obj.mode = pc.ob_type)) THEN
  ASSERT(dbg.get_index(t)=dbg.write_type_cnt);
  END;
  begin_type(LF_PROCEDURE); cmd.GenByte(0);
  cmd.GenWord(0); (*count_params(t)*);
  cmd.GenWord(0);  -- max params
  cmd.GenByte(FID_index);
  cmd.GenWord(VAL(INT16, dbg.get_index(t.base)));
  cmd.GenByte(FID_index);
  cmd.GenWord(VAL(INT16, dbg.write_type_cnt+1)); ----- args_list has the next index ??!!
  end_type;
  begin_type(LF_LIST); -- arglist;
  cmd.GenByte(4);
  end_type;
END write_procedure;

PROCEDURE field_filter (f: pc.OBJECT): BOOLEAN;
VAR
  offset: INT32;
BEGIN
  CASE f.mode OF
  | pc.ob_field:
    RETURN TRUE;
  | pc.ob_field_bts:
    RETURN dbg.whole_word_field (f, offset);
  END;
END field_filter;


PROCEDURE set_field_type (f: pc.OBJECT);
BEGIN
  dbg.emit_type(dbg.act_set, f.type)
END set_field_type;

PROCEDURE write_field_type (f: pc.OBJECT);
BEGIN
  dbg.emit_type(dbg.act_write, f.type)
END write_field_type;

PROCEDURE write_field (f: pc.OBJECT);
BEGIN
  cmd.GenByte(FID_index);
  cmd.GenWord(VAL(INT16, dbg.get_index(f.type)));
END write_field;


PROCEDURE write_field_name (f: pc.OBJECT);
BEGIN
  cmd.GenByte(FID_string);
  write_obj_name(f);
  gen_Card_FID_span(def.obj_offset(f));
END write_field_name;


PROCEDURE write_o2_class_field(f: pc.OBJECT);
BEGIN
  ASSERT(f#NIL);
  cmd.GenByte(FID_index);
  cmd.GenWord(VAL(INT16, dbg.tindex_transfer));
  INC(dbg.tindex_transfer);
END write_o2_class_field;


PROCEDURE write_o2_class_member(f: pc.OBJECT);
BEGIN
  begin_type(LF_MEMBER);
  cmd.GenByte(0);
  cmd.GenByte(2); -- Public
  cmd.GenWord(VAL(INT16, dbg.get_index(  f.type)));
  gen_Card_FID_span(def.obj_offset(f));
  write_name("");
  write_obj_name(f);
  end_type;
END write_o2_class_member;


--------------------------------------------------------------------------

TYPE
  emit_rec = RECORD (dbg.emit_rec)
             END;

  EMIT_HLL  = POINTER TO emit_rec;


PROCEDURE (emit: EMIT_HLL) TypeEmitter (ttag: dbg.TYPE_TAG; act: dbg.ACTION; type: pc.STRUCT);
VAR
  int_val_min: INT32;
  int_val_max: INT32;
  card_val   : sys.CARD32;

  cns : pc.OBJECT;
  val : INT32;
  len : INT32;
  base: pc.STRUCT;
  l_st: INT16;
  inx : dbg.TYPE_INDEX;

BEGIN
  CASE ttag OF
  | dbg.ty_start:
    ASSERT((act = dbg.act_write) AND (type = NIL));
    dbg.type_cnt := first_nonprimitive;
    dbg.write_type_cnt := first_nonprimitive;
    IsXHLL := emit.name = opt.dbg_XHLL;
  | dbg.ty_end:
    ASSERT((act = dbg.act_write) AND (type = NIL));
    dbg.type_cnt := MAX(dbg.TYPE_INDEX);
    dbg.write_type_cnt := MAX(dbg.TYPE_INDEX);
  | dbg.ty_range:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);          -- LF_RANGE
    | dbg.act_write:
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      begin_type(LF_RANGE);
      cmd.GenByte(0);
      cmd.GenWord(VAL(INT16, dbg.get_index(  type.base)));
      len := pc.code.get_size(pc.su_size, type);
      IF type.base.mode IN pc.TY_SET{pc.ty_shortint, pc.ty_integer, pc.ty_longint} THEN
        int_val_min := type.min.get_integer();
        int_val_max := type.max.get_integer();
        CASE len OF
        | 1:
          cmd.GenByte(088H); cmd.GenByte(VAL(INT8, int_val_min));
          cmd.GenByte(088H); cmd.GenByte(VAL(INT8, int_val_max));
        | 2:
          cmd.GenByte(089H); cmd.GenWord(VAL(INT16, int_val_min));
          cmd.GenByte(089H); cmd.GenWord(VAL(INT16, int_val_max));
        | 4:
          cmd.GenByte(08AH); cmd.GenLWord(int_val_min);
          cmd.GenByte(08AH); cmd.GenLWord(int_val_max);
        END;
      ELSE
        gen_Card_FID_span(SYSTEM.VAL(LONGINT,type.min.get_cardinal()));
        gen_Card_FID_span(SYSTEM.VAL(LONGINT,type.max.get_cardinal()));
      END;
      write_type_name(type);
      end_type;
    END;
  | dbg.ty_enum:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);     -- LF_ENUM
      INC(dbg.type_cnt);       -- LF_FIELDLIST
    | dbg.act_write:
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      begin_type(LF_ENUM);
      cmd.GenByte(0);
      cmd.GenByte(FID_index);
      cmd.GenWord(VAL(INT16, dbg.get_index (type.base)));
      cmd.GenByte(FID_index);
      cmd.GenWord(VAL(INT16, dbg.write_type_cnt+1));         -- @fList
      gen_Card_FID_span(0);                      -- min
      gen_Card_FID_span(type.len-1);             -- max
      cmd.GenByte(FID_string);
      write_type_name(type);
      end_type;
      begin_type(LF_LIST);
      cmd.GenByte(3); -- Flist for enum
      cns := type.prof;
      REPEAT
        ASSERT((cns.mode = pc.ob_cons) & (cns.val.mode = pc.nd_value));
        val := cns.val.val.get_integer();
        cmd.GenByte(FID_string);
        write_obj_name(cns);
        gen_Card_FID_span(val);
        cns := cns.next;
      UNTIL cns = NIL;
      end_type;
    END;
  | dbg.ty_pointer:
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);           -- LF_POINTER
      dbg.emit_type(act, type.base); -- [base]
    | dbg.act_write:
      IF type = NIL THEN
        begin_type(LF_POINTER);
        cmd.GenByte(1+0);                  (* Near 32-bit Pointer *)
        cmd.GenByte(FID_index);
        cmd.GenWord(VAL(INT16, dbg.tindex_transfer));
        cmd.GenByte(FID_string);
        write_name("");
        end_type;
      ELSE
        base := type.base;
        ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
        begin_type(LF_POINTER);
        cmd.GenByte(1+0);                  (* Near 32-bit Pointer *)
        cmd.GenByte(FID_index);
        cmd.GenWord(VAL(INT16, dbg.get_index(  base)));
        cmd.GenByte(FID_string);
        write_type_name(type);
        end_type;
        dbg.emit_type(act, base);
      END;
    END;
  | dbg.ty_reference:
    CASE act OF
    | dbg.act_set:
      ASSERT(FALSE);
    | dbg.act_write:
      IF type = NIL THEN
        begin_type(LF_REFERENCE);
        cmd.GenByte(0);
        cmd.GenWord(VAL(INT16, dbg.tindex_transfer));
        end_type;
      ELSE
        begin_type(LF_REFERENCE);
        cmd.GenByte(0);
        cmd.GenWord(VAL(INT16, dbg.get_index(  type)));
        end_type;
      END;
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
      begin_type(LF_SET);
      cmd.GenByte(0);
      cmd.GenWord(VAL(INT16, dbg.get_index(  type.base)));
      write_type_name(type);
      end_type;
    END;
  | dbg.ty_proctype:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      IF (type.obj = NIL) OR (type.obj.mode = pc.ob_type) THEN
        dbg.put_index(type);
      ELSE
        dbg.put_index_val(type, dbg.type_cnt);
      END;
      INC(dbg.type_cnt);         -- LF_PROCEDURE
      INC(dbg.type_cnt);         -- LF_ARGLIST

    | dbg.act_write:
      IF (type.obj = NIL) OR (type.obj.mode = pc.ob_type) THEN
        ASSERT( dbg.get_index(type)=dbg.write_type_cnt);
        begin_type(LF_POINTER);
        cmd.GenByte(1+0);                  (* Near 32-bit Pointer *)
        cmd.GenByte(FID_index);
        cmd.GenWord(VAL(INT16, dbg.write_type_cnt+1));
        cmd.GenByte(FID_string);
        write_type_name(type);
        end_type;
      END;
      write_procedure(type);
    END;
  | dbg.ty_array:
    dbg.emit_type(act, type.inx);
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      IF NOT (at.tmark_db_index IN type.marks) THEN
        dbg.put_index(type);          -- LF_ARRAY
      END;
    | dbg.act_write:
      IF dbg.get_index(  type)=dbg.write_type_cnt THEN
        begin_type(LF_ARRAY);
        cmd.GenByte(0H + 00H + 0H + 0H); -- row major + unpacked + no descriptor + fixed len
        cmd.GenLWord(pc.code.get_size(pc.su_bytes, type));
        cmd.GenByte(083H);                              -- FID_index
        cmd.GenWord(VAL(INT16, dbg.get_index(  type.inx)));
        cmd.GenByte(083H);                              -- FID_index
        cmd.GenWord(VAL(INT16, dbg.get_index(  type.base)));
        cmd.GenByte(082H);                              -- FID_string
        write_type_name(type);
        end_type;
      END;
    END;
  | dbg.ty_array_of:
    dbg.emit_type (act, type.base);
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);          -- LF_ARRAY
    | dbg.act_write:
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      begin_type(LF_ARRAY);
      cmd.GenByte(0H + 00H + 4H + 8H); -- row major + unpacked + descriptor + variable len
      cmd.GenLWord(0);
      cmd.GenByte(083H);                              -- FID_index
      cmd.GenWord(void_type);
      cmd.GenByte(083H);                              -- FID_index
      cmd.GenWord(VAL(INT16, dbg.get_index(  type.base)));
      cmd.GenByte(082H);                              -- FID_string
      write_type_name(type);
      end_type;
    END;
  | dbg.ty_open_array:
    CASE act OF
    | dbg.act_set:
      -- We have not "set_open_array" because every open array type
      -- is referenced only once, and a following procedure
      -- "write_open_array" combines both 'set' and 'write' functions
      ASSERT(FALSE);
    | dbg.act_write:
      begin_type(LF_ARRAY);
      cmd.GenByte(0H + 0H + 8H); -- row major + unpacked + no descriptor + variable len
      cmd.GenLWord(0);
      cmd.GenByte(083H);                              -- FID_index
      cmd.GenWord(void_type);
      cmd.GenByte(083H);                              -- FID_index
      cmd.GenWord(VAL(INT16, dbg.tindex_transfer));
      cmd.GenByte(082H);                              -- FID_string
      write_name("");
      end_type;
    END;
  | dbg.ty_SS:
    CASE act OF
    | dbg.act_set:
      INC(dbg.type_cnt);        -- Range;
      dbg.put_index(type);  -- LF_ARRAY OF CHAR;
    | dbg.act_write:
      ASSERT(type.mode=pc.ty_SS);
      begin_type(LF_RANGE);
      cmd.GenByte(0);
      card_val := pc.code.get_size(pc.su_bytes, type)-1;
      l_st := VAL(INTEGER, (card_val+1) DIV 256);
      cmd.GenWord(unsign_type+l_st);
      gen_Card_FID_span(0);
      gen_Card_FID_span(card_val);
      cmd.GenByte(0); -- No name
      end_type;
      ASSERT(dbg.get_index(  type)=dbg.write_type_cnt);
      begin_type(LF_ARRAY);
      cmd.GenByte(0H + 00H + 0H + 0H); -- row major + unpacked + no descriptor + fixed len
      cmd.GenLWord(pc.code.get_size(pc.su_bytes, type));
      cmd.GenByte(083H);                              -- FID_index
      cmd.GenWord(VAL(INT16, dbg.get_index(  type.inx)));
      cmd.GenByte(083H);                              -- FID_index
      cmd.GenWord(character_type (* + 0 *) );
      cmd.GenByte(082H);                              -- FID_string
      cmd.GenByte(0); -- No name;
      end_type;
    END;
  | dbg.ty_record:
    IF type.base # NIL THEN
      dbg.emit_type(act, type.base);
    END;
    CASE act OF
    | dbg.act_set:
      dbg.field_list (type.prof, field_filter, set_field_type);
      IF NOT (at.tmark_db_index IN type.marks) THEN
        dbg.put_index(type);
        IF type.flag IN pc.OOP_langs THEN
          INC(dbg.type_cnt);   -- Items Lists
          INC(dbg.type_cnt, dbg.count_fields (type, field_filter));
        ELSE
          dbg.field_list(type.prof, field_filter, set_field_type);
          INC(dbg.type_cnt, 2); ---  Types & Names Lists
        END;
      END;
    | dbg.act_write:
      dbg.field_list (type.prof, field_filter, write_field_type);
      inx := VAL(INT16, dbg.get_index(  type));
      IF inx = dbg.write_type_cnt THEN
        IF type.flag IN pc.OOP_langs THEN
          begin_type(LF_CLASS);
          cmd.GenByte(1); -- is_struct + not a DTS class
          cmd.GenLWord(pc.code.get_size(pc.su_bytes,type));
          cmd.GenWord(dbg.count_fields (type, field_filter));
          cmd.GenWord(VAL(INT16, dbg.write_type_cnt+1));
          write_type_name(type);
          end_type;
          begin_type(LF_LIST);
          cmd.GenByte(1); -- types list
          dbg.tindex_transfer := dbg.write_type_cnt+1;
          IF type.base # NIL THEN
            cmd.GenByte(FID_index);
            cmd.GenWord(VAL(INT16, dbg.tindex_transfer));
            INC(dbg.tindex_transfer);
          END;
          dbg.field_list (type.prof, field_filter, write_o2_class_field);
          end_type; -- LF_LIST
          IF type.base # NIL THEN
            begin_type(LF_BASECLASS);
            cmd.GenByte(0);
            cmd.GenByte(2); -- public
            cmd.GenWord(VAL(INT16, dbg.get_index(  type.base)));
            gen_Card_FID_span(0);
            end_type;
          END;
          dbg.field_list (type.prof, field_filter, write_o2_class_member);
        ELSE
          begin_type(LF_RECORD);
          cmd.GenByte(0);
          cmd.GenLWord(pc.code.get_size(pc.su_bytes,type));
          cmd.GenWord(dbg.count_fields (type, field_filter));
          cmd.GenByte(FID_index);
          cmd.GenWord(VAL(INT16, dbg.write_type_cnt+1)); -- types list and names list
          cmd.GenByte(FID_index);
          cmd.GenWord(VAL(INT16, dbg.write_type_cnt+2)); -- always must be exact after record
          cmd.GenByte(FID_string);
          write_type_name(type);
          end_type;
          begin_type(LF_LIST);
          cmd.GenByte(1);      -- types list
          dbg.field_list (type.prof, field_filter, write_field);
          end_type;
          begin_type(LF_LIST);
          cmd.GenByte(2);      -- names and offsets list
          dbg.field_list (type.prof, field_filter, write_field_name);
          end_type;
        END;
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
      write_procedure(type);
    END;
  END;
END TypeEmitter;


PROCEDURE (emit: EMIT_HLL) SymbEmitter (stag: dbg.SYMB_TAG; o: pc.OBJECT; tind: dbg.TYPE_INDEX);
VAR
  a   : at.ATTR_EXT;
  db  : at.DBG_EXT;
BEGIN
  CASE stag OF
  | dbg.sy_start:
    ASSERT((o=NIL) AND (tind=-1));
  | dbg.sy_end:
    ASSERT((o=NIL) AND (tind=-1));
  | dbg.sy_var:
    begin_symb(S_GDATA32);
    wr_far_adr(o);
    cmd.GenWord(VAL(INT16, tind));
    write_obj_name(o);
    end_symb;
  | dbg.sy_proc:
    begin_symb(S_PROC32);
    wr_far_adr(o);
    cmd.GenLWord(dbg.proc_len(o));
    cmd.GenWord(VAL(INT16, dbg.start_proc_debug(o)));
    cmd.GenLWord(dbg.end_proc_debug(o));
    cmd.GenWord(0);                   -- ClassType = 0 Because of where is no classes
    cmd.GenByte(near32bit);
    write_obj_name(o);
    -- XD debugger extensions
    cmd.GenByte(EA_PROC);
    cmd.GenByte(8); -- length of extended attributes
    IF dbg.has_frame(o) THEN
      cmd.GenLWord(1);
    ELSE
      cmd.GenLWord(0);
    END;
    cmd.GenLWord(dbg.proc_frame_size(o));
    end_symb;
  | dbg.sy_scope_open:
    ASSERT(tind = -1);
  | dbg.sy_scope_close:
    ASSERT(tind = -1);
    begin_symb(S_END);
    end_symb;
  | dbg.sy_local_var,
    dbg.sy_param:
    a := at.attr(o.ext, at.a_dbg);
    ASSERT(a # NIL);
    db := a(at.DBG_EXT);
    IF db.list[0].info.Location = ir.LocInReg THEN
      begin_symb(Register);
      cmd.GenWord(VAL(INT16, tind));
      cmd.GenByte(VAL(SHORTINT, register_num(VAL(Reg,db.list[0].info.Value), pc.code.get_size(pc.su_size, o.type))));
      write_obj_name(o);
      end_symb;
    ELSE
------------- change this to commented to obtain frames in XD
      begin_symb(Auto);
---------
--      begin_symb(RegRelative);
--      cmd.GenWord(-1);
------------
      cmd.GenLWord(db.list[0].info.Value);
      cmd.GenWord(VAL(INT16, tind));
      write_obj_name(o);
      end_symb;
    END;
  END;
END SymbEmitter;


PROCEDURE (emit: EMIT_HLL) PrimitiveTypeNo (type: pc.STRUCT): dbg.TYPE_INDEX;
VAR
  ptr: BOOLEAN;
  inx: dbg.TYPE_INDEX;
BEGIN
  IF (type.mode = pc.ty_opaque) AND (type.base # NIL) THEN
    type := type.base;
  END;
  ptr := (type.mode = pc.ty_pointer);
  IF ptr THEN type := type.base; END;
  CASE type.mode OF
  | pc.ty_shortcard : inx := unsign_type + size_1_byte;
  | pc.ty_cardinal  : inx := unsign_type + size_2_bytes;
  | pc.ty_longcard  : inx := unsign_type + size_4_bytes;
  | pc.ty_shortint  : inx := signed_type + size_1_byte;
  | pc.ty_integer   : inx := signed_type + size_2_bytes;
  | pc.ty_longint   : inx := signed_type + size_4_bytes;
  | pc.ty_longlongint: inx := long_signed_type;
  | pc.ty_longlongcard: inx := long_unsigned_type;
  | pc.ty_real      : inx := real_type + size_real32;
  | pc.ty_longreal  : inx := real_type + size_real64;
  | pc.ty_ld_real   : inx := real_type + size_real80;
  | pc.ty_complex   : inx := complex_type + size_real32;
  | pc.ty_lcomplex  : inx := complex_type + size_real64;
  | pc.ty_boolean   : inx := emit.PrimitiveTypeNo(type.base);
                      inx := boolean_type   + (inx-80H) MOD 4;
  | pc.ty_char      : inx := emit.PrimitiveTypeNo(type.base);
                      inx := character_type + (inx-80H) MOD 4;
  | pc.ty_uchar     : inx := emit.PrimitiveTypeNo(type.base);
                      inx := character_type + (inx-80H) MOD 4;
  | pc.ty_AA        : IF ptr THEN inx := -1;
                      ELSE ptr := TRUE; inx := void_type; (* void *)
                      END;
  | pc.ty_opaque    : IF ptr THEN inx := -1
                      ELSE ptr := TRUE; inx := void_type; (* void *)
                      END;
  | pc.ty_pointer   : inx := -1;
  | pc.ty_loc       : IF ptr AND (type.mno < pc.ZEROMno) THEN
                        inx := void_type;
                      ELSE
                        inx := unsign_type + size_1_byte;
                      END;
  | pc.ty_protection: inx := unsign_type + size_2_bytes;
  | pc.ty_void      : inx := void_type; (* void *)
  ELSE
    inx := -1;
  END;
  IF (inx >= 0) & ptr THEN INC(inx, near32ptr); END;
  RETURN inx
END PrimitiveTypeNo;

VAR
  emitHLL: EMIT_HLL;

BEGIN
  NEW(emitHLL);
  reg.Register(opt.dbgFormat, opt.dbg_HLL, emitHLL);
  NEW(emitHLL);
  reg.Register(opt.dbgFormat, opt.dbg_XHLL, emitHLL);
END dbgHLL.
