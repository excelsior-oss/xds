MODULE dbgCV;

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
IMPORT env := xiEnv;

--IMPORT Printf;

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
  OMF_Version = 1; -- Debug info type


----------------------- Types Information ----------------------------

CONST
  first_nonprimitive = 1000H;


------------- S y m b o l s ------------------------------------

CONST
  S_COMPILE  = 0001H;       (* Compile Flag *)
  S_REGISTER = 0002H;       (* Register *)
  S_END      = 0006H;       (* End of block *)
  S_OBJECT   = 0009H;       (* Object File Name *)
  S_BPREL32  = 0200H;       (* BP Relative *)
  S_LDATA32  = 0201H;       (* Local Data *)
  S_GDATA32  = 0202H;       (* Global Data *)
  S_LPROC32  = 0204H;       (* Local Procedure Start *)
  S_GPROC32  = 0205H;       (* Global Procedure Start *)

  EA_PROC    = 0EAH;        (* Procedure extended attributes *)


PROCEDURE write_name (nm-: ARRAY OF CHAR);
VAR
 ln, i: INT32;
BEGIN
  ln := LENGTH(nm);
  IF ln > 255 THEN ln := 255; END;
  cmd.GenByte(VAL(INT8,ln));
  FOR i := 0 TO ln-1 DO cmd.GenByte(nm[i]); END;
END write_name;


PROCEDURE write_obj_name (o: pc.OBJECT);
VAR
  name: ARRAY 1024 OF CHAR;
BEGIN
  dbg.write_obj_name(o, name);
--  Printf.printf("dbg.write_obj_name %s",name);
  write_name(name);
END write_obj_name;


PROCEDURE write_type_name (t: pc.STRUCT);
VAR
  name: ARRAY 1024 OF CHAR;
BEGIN
  dbg.write_type_name(t, name);
  write_name(name);
END write_type_name;

CONST
  LF_NUMERIC   = -8000H;      (* = 8000H for 2-byte numbers!! *)
  LF_CHAR      = LF_NUMERIC;
  LF_SHORT     = LF_NUMERIC + 01H;
  LF_USHORT    = LF_NUMERIC + 02H;
  LF_LONG      = LF_NUMERIC + 03H;
  LF_ULONG     = LF_NUMERIC + 04H;
  LF_REAL32    = LF_NUMERIC + 05H;
  LF_REAL64    = LF_NUMERIC + 06H;
  LF_REAL80    = LF_NUMERIC + 07H;
  LF_COMPLEX32 = LF_NUMERIC + 0CH;
  LF_COMPLEX64 = LF_NUMERIC + 0DH;
  LF_COMPLEX80 = LF_NUMERIC + 0EH;
  LF_VARSTRING = LF_NUMERIC + 10H;

  LF_MODIFIER  = 0001H;
  LF_POINTER   = 0002H;
  LF_ARRAY     = 0003H;
  LF_CLASS     = 0004H;
  LF_STRUCTURE = 0005H;
  LF_ENUM      = 0007H;
  LF_PROCEDURE = 0008H;

  LF_ARGLIST   = 0201H;
  LF_FIELDLIST = 0204H;
  LF_DIMVARU   = 020AH;
  LF_REFSYM    = 020CH;

  LF_BASECLASS = 0400H;
  LF_ENUMERATE = 0403H;
  LF_INDEX     = 0405H;
  LF_MEMBER    = 0406H;


PROCEDURE write_int(val: INT32; t: pc.STRUCT);
BEGIN
  IF (0<= val) & (val < 8000H) THEN cmd.GenWord(VAL(INT16, val));
  ELSE
    CASE t.mode OF
    | pc.ty_shortint : cmd.GenWord(LF_CHAR);   cmd.GenByte(VAL(INT8, val));
    | pc.ty_integer  : cmd.GenWord(LF_SHORT);  cmd.GenWord(VAL(INT16, val));
    | pc.ty_cardinal : cmd.GenWord(LF_USHORT); cmd.GenWord(sys.VAL(INT16, val));
    | pc.ty_longint  : cmd.GenWord(LF_LONG);   cmd.GenLWord(val);
    | pc.ty_longcard : cmd.GenWord(LF_ULONG);  cmd.GenLWord(val);
    ELSE
      cmd.GenWord(LF_ULONG); cmd.GenLWord(val);     (* ?? *)
    END;
  END;
END write_int;


PROCEDURE write_type_length(t: pc.STRUCT);
BEGIN
  write_int(pc.code.get_size(pc.su_bytes, t), def.SIZE_T);
END write_type_length;


VAR
  symb_start: INT32;

PROCEDURE begin_symb(tag: INTEGER);
BEGIN
  symb_start := dbg.symb_info.code_len;
  cmd.GenWord(tag);  -- to reserve place for record length
  cmd.GenWord(tag);
END begin_symb;

PROCEDURE end_symb;
VAR
  len: INT16;
BEGIN
  len := VAL(INT16, dbg.symb_info.code_len - symb_start);
  ASSERT(len > 2);
  WHILE (len MOD 4) # 0 DO cmd.GenByte(0X); INC(len); END;
  DEC(len, 2);
  cmd.move2b(sys.ADR(len), sys.ADR(dbg.symb_info.bcode[symb_start]), cmd.inverse_byte_order);
  symb_start := MAX(INT32);
END end_symb;



CONST
  PAD0 = 0F0H;

PROCEDURE make_pad;
VAR
  r: INT32;
BEGIN
  r := 4 - dbg.type_info.code_len MOD 4;
  IF r <> 4 THEN
    REPEAT
      cmd.GenByte(CHR(PAD0 + r));
      DEC(r);
    UNTIL r = 0;
  END;
END make_pad;


VAR
  length_pos: INT32;

PROCEDURE begin_type(tag: INT16);
BEGIN
  length_pos := dbg.type_info.code_len;
  cmd.GenWord(0);
  cmd.GenWord(tag);
END begin_type;


PROCEDURE end_type;
VAR
  len: INT16;
BEGIN
  make_pad;
  len := VAL(INT16, dbg.type_info.code_len - (length_pos+2));
  ASSERT(len > 0);
  cmd.move2b (sys.ADR(len), sys.ADR(dbg.type_info.bcode[length_pos]), cmd.inverse_byte_order);
  INC(dbg.write_type_cnt);
END end_type;


PROCEDURE wr_far_adr(o: pc.OBJECT);
BEGIN
  cmd.gen_fixup(o, 0, cmd.fx_obj32far);  -- 32-bit offset
  cmd.GenWord(0);                        -- 16-bit segment
END wr_far_adr;


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
  dbg.emit_type(dbg.act_set, f.type);
END set_field_type;


PROCEDURE write_field_type (f: pc.OBJECT);
BEGIN
  dbg.emit_type(dbg.act_write, f.type);
END write_field_type;


PROCEDURE write_field (f: pc.OBJECT);
VAR
  offs: LONGINT;
BEGIN
  CASE f.mode OF
  | pc.ob_field:
    offs := def.obj_offset(f);
  | pc.ob_field_bts:
    ASSERT(dbg.whole_word_field(f, offs));
  END;
  cmd.GenWord(LF_MEMBER);
  cmd.GenWord(VAL(INT16, dbg.get_index(f.type)));
  cmd.GenWord(0);                      -- attributes
  write_int(offs, def.INDEX_T);
  write_obj_name(f);
  make_pad;
END write_field;


PROCEDURE write_procedure(t: pc.STRUCT);
VAR
  call: INT8;
BEGIN
  begin_type(LF_PROCEDURE);
  cmd.GenWord(VAL(INT16, dbg.get_index(t.base)));
  CASE t.flag OF
  | pc.flag_o2       : call := 0; (* near C ?? *)
  | pc.flag_m2       : call := 0; (* near C ?? *)
  | pc.flag_c        : call := 0; (* near C *)
  | pc.flag_sl1      : call := 0; (* near C *)
  | pc.flag_p        : call := 2; (* near Pascal *)
  | pc.flag_bnrp     : call := 2; (* near Pascal *)
  | pc.flag_stdcall  : call := 7; (* near stdcall *)
  | pc.flag_vmcall   : call := 7; (* near stdcall *)
  | pc.flag_lightcall: call := 7; (* near stdcall *)
  | pc.flag_javacall : call := 2; (* near Pascal *)
  | pc.flag_syscall  : call := 8; (* near syscall *)
  ELSE                 call := 0; (* near C *)
  END;
  cmd.GenByte(call);
  cmd.GenByte(0);
  cmd.GenWord(0); --< count_params(t)
  cmd.GenWord(VAL(INT16, dbg.write_type_cnt+1)); -- args_list has the next index ??!!
  end_type;
  begin_type(LF_ARGLIST);
  cmd.GenWord(0); -- ????
  end_type;
END write_procedure;


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
 (* CV register enumerations -- see TIS, part 6 *)
<* END *>

PROCEDURE register_num(r: Reg; sz: INT32): INT16;
VAR
  res: INT16;
BEGIN
<* IF ~TARGET_386 THEN *>
  RETURN noreg;
<* ELSE *>
  RETURN Reg2VCNum[r];
<* END *>
END register_num;


CONST
  T_VOID  = 0003H;
  T_ULONG = 0022H;
  T_RCHAR = 0070H;
  T_INT4  = 0074H;


---------------------------------------------------------------------------

TYPE
  emit_rec = RECORD (dbg.emit_rec)
             END;

  EMIT_CV  = POINTER TO emit_rec;

PROCEDURE (emit: EMIT_CV) TypeEmitter (ttag: dbg.TYPE_TAG; act: dbg.ACTION; type: pc.STRUCT);
VAR
  cns : pc.OBJECT;
  val : INT32;
  base: pc.STRUCT;
  inx : dbg.TYPE_INDEX;
BEGIN
  CASE ttag OF
  | dbg.ty_start:
    ASSERT((act = dbg.act_write) AND (type = NIL));
    cmd.GenLWord(OMF_Version);
    dbg.type_cnt := first_nonprimitive;
    dbg.write_type_cnt := first_nonprimitive;
  | dbg.ty_end:
    ASSERT((act = dbg.act_write) AND (type = NIL));
    dbg.type_cnt := MAX(dbg.TYPE_INDEX);
    dbg.write_type_cnt := MAX(dbg.TYPE_INDEX);
  | dbg.ty_range:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      dbg.put_index_val(type, dbg.get_index(type.base));
    | dbg.act_write:
    END;
  | dbg.ty_enum:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type); -- LF_ENUM
      INC(dbg.type_cnt);   -- LF_FIELDLIST
    | dbg.act_write:
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      begin_type(LF_ENUM);
      cmd.GenWord(VAL(INT16, type.len));                 -- count
      cmd.GenWord(VAL(INT16, dbg.get_index(type.base))); -- @type
      cmd.GenWord(VAL(INT16, dbg.write_type_cnt+1));     -- fList
      cmd.GenWord(0);                                    -- property
      write_type_name(type);
      end_type;
      begin_type(LF_FIELDLIST);
      cns := type.prof;
      REPEAT
        ASSERT((cns.mode = pc.ob_cons) & (cns.val.mode = pc.nd_value));
        val := cns.val.val.get_integer();
        cmd.GenWord(LF_ENUMERATE);
        cmd.GenWord(0);                    -- attribute
        write_int(val, type.base);
        write_obj_name(cns);
        cns := cns.next;
      UNTIL cns = NIL;
      end_type;
    END;
  | dbg.ty_pointer,
    dbg.ty_reference:
    CASE act OF
    | dbg.act_set:
      ASSERT((ttag#dbg.ty_reference) AND (type#NIL));
      dbg.put_index(type);                          -- LF_MODIFIER
      dbg.emit_type(act, type.base);                -- [base]
      dbg.put_second_index_val(type, dbg.type_cnt); -- LF_POINTER
      INC(dbg.type_cnt);
    | dbg.act_write:
      IF type = NIL THEN
        begin_type(LF_POINTER);
        IF (ttag = dbg.ty_pointer) AND NOT env.config.Option("DBGVCCOMPATIBLE") THEN
          cmd.GenWord(10+0);    (* Near 32-bit Pointer             *)
        ELSE
          cmd.GenWord(10+32);   (* Near 32-bit Pointer + Reference *)
        END;
        cmd.GenWord(VAL(INT16, dbg.tindex_transfer));
        end_type;
      ELSE
        base := type.base;
        ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
        begin_type(LF_MODIFIER);
        cmd.GenWord(0);                     (* No attributes *)
        cmd.GenWord(VAL(INT16, dbg.get_second_index(type)));
        end_type;
        dbg.emit_type(act, base);
        begin_type(LF_POINTER);
        IF (ttag = dbg.ty_pointer) AND NOT env.config.Option("DBGVCCOMPATIBLE") THEN
          cmd.GenWord(10+0);    (* Near 32-bit Pointer             *)
        ELSE
          cmd.GenWord(10+32);   (* Near 32-bit Pointer + Reference *)
        END;
        cmd.GenWord(VAL(INT16, dbg.get_index(base)));
        end_type;
      END;
    END;
  | dbg.ty_opaque:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      -- Чтобы дважды не вызывать put_index_val
      IF dbg.get_index(type) > 0 THEN RETURN; END;
      dbg.put_index_val(type, dbg.get_index(type.base));
    | dbg.act_write:
    END;
  | dbg.ty_set:
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);
    | dbg.act_write:
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      begin_type(LF_ARRAY);
      cmd.GenWord(T_ULONG);
      cmd.GenWord(T_INT4);
      write_type_length(type);
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
      INC(dbg.type_cnt); -- LF_PROCEDURE
      INC(dbg.type_cnt); -- LF_ARGLIST
    | dbg.act_write:
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      IF (type.obj = NIL) OR (type.obj.mode = pc.ob_type) THEN
        dbg.tindex_transfer := dbg.write_type_cnt+1;
        emit.TypeEmitter (dbg.ty_pointer, act, NIL);
      END;
      write_procedure(type);
    END;
  | dbg.ty_array,
    dbg.ty_array_of:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      IF NOT (at.tmark_db_index IN type.marks) THEN
        dbg.put_index(type);          -- LF_ARRAY
      END;
    | dbg.act_write:
      inx := dbg.get_index(type);
      IF inx = dbg.write_type_cnt THEN
        begin_type(LF_ARRAY);
        cmd.GenWord(VAL(INT16, dbg.get_index(type.base)));
        cmd.GenWord(T_INT4);
        IF type.mode = pc.ty_array_of THEN
          write_int(0, def.SIZE_T);
        ELSE
          write_type_length(type);
        END;
        write_type_name(type);
        end_type;
      ELSE
        ASSERT(inx < dbg.write_type_cnt);
      END;
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
      cmd.GenWord(VAL(INT16, dbg.tindex_transfer));
      cmd.GenWord(T_INT4);
      write_int(0, def.SIZE_T);
      write_name("");
      end_type;
    END;
  | dbg.ty_SS:
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);           -- LF_ARRAY
    | dbg.act_write:
      ASSERT(dbg.get_index(type) = dbg.write_type_cnt);
      begin_type(LF_ARRAY);
      cmd.GenWord(T_RCHAR);
      cmd.GenWord(T_INT4);
      write_type_length(type);
      write_type_name(type);
      end_type;
    END;
  | dbg.ty_record:
    IF type.base # NIL THEN
      dbg.emit_type(act, type.base);
    END;
    CASE act OF
    | dbg.act_set:
      IF NOT (at.tmark_db_index IN type.marks) THEN
        dbg.put_index(type);               -- LF_STRUCTURE or LF_CLASS
        INC(dbg.type_cnt);                 -- LF_FIELDLIST
        dbg.field_list (type.prof, field_filter, set_field_type);
      END;
    | dbg.act_write:
      inx := dbg.get_index(type);
      IF inx = dbg.write_type_cnt THEN
        IF type.flag IN pc.OOP_langs THEN
          begin_type(LF_CLASS);
        ELSE
          begin_type(LF_STRUCTURE);
        END;
        cmd.GenWord(dbg.count_fields (type, field_filter));
        cmd.GenWord(VAL(INT16, inx+1));
        cmd.GenWord(0);       -- property
        cmd.GenWord(0);       -- dList
        cmd.GenWord(0);       -- vshape
        write_type_length(type);
        write_type_name(type);
        end_type;
        begin_type(LF_FIELDLIST);
        IF type.base # NIL THEN
          cmd.GenWord(LF_BASECLASS);
          cmd.GenWord(VAL(INT16, dbg.get_index(type.base)));
          cmd.GenWord(0);  -- attributes
          write_int(0, def.INDEX_T);
        END;
        dbg.field_list (type.prof, field_filter, write_field);
        end_type;
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
      write_procedure(type);
    END;
  END;
END TypeEmitter;


PROCEDURE (emit: EMIT_CV) SymbEmitter (stag: dbg.SYMB_TAG; o: pc.OBJECT; tind: dbg.TYPE_INDEX);
CONST
  intel_386     = 03X;
  intel_486     = 04X;
  intel_Pentium = 05X;

VAR
  type: pc.STRUCT;
  a   : at.ATTR_EXT;
  db  : at.DBG_EXT;
  size: LONGINT;

BEGIN
  CASE stag OF
  | dbg.sy_start:
    cmd.GenLWord(OMF_Version);
    begin_symb(S_OBJECT);
    cmd.GenLWord(0);         -- signature
    write_name(dbg.objname);
    end_symb;
  | dbg.sy_end:
    begin_symb(S_COMPILE);
    cmd.GenByte(intel_386);
    cmd.GenByte(0C);         -- Language: C  ??
    cmd.GenByte(0C);
    cmd.GenByte(8X);
    write_name(pc.code.vers);
    end_symb;
  | dbg.sy_var:
    IF o.is_public() THEN
      begin_symb(S_GDATA32);
    ELSE
      begin_symb(S_LDATA32);
    END;
    wr_far_adr(o);
    cmd.GenWord(VAL(INT16, tind));
    write_obj_name(o);
    end_symb;
  | dbg.sy_proc:
    begin_symb(S_GPROC32);   (* WD does not work in LPROC's propertly!! *)
    cmd.GenLWord(0);              -- pParent, will be filled by CVPack
    cmd.GenLWord(0);              -- pEnd,    will be filled by CVPack
    cmd.GenLWord(0);              -- pNext,   will be filled by CVPack
    cmd.GenLWord(dbg.proc_len(o));
    cmd.GenLWord(dbg.start_proc_debug(o));
    cmd.GenLWord(dbg.end_proc_debug(o));
    wr_far_adr(o);
    cmd.GenWord(VAL(INT16, tind));
    -- flags
    -- 0..3 bits are reserved, so use 0 bit that means frame size is written
    -- 7 bit means function has frame pointer
    IF dbg.has_frame(o) THEN
      cmd.GenByte(00H+1);
    ELSE
      cmd.GenByte(80H+1);
    END;
    write_obj_name(o);
    -- XD debugger extensions
    cmd.GenByte (EA_PROC);
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
    type := o.type;
    IF type.mode = pc.ty_array_of THEN
      size := 4;
    ELSE
      size := pc.code.get_size(pc.su_size, type);
    END;
    IF (o.mode = pc.ob_varpar)
      OR ((pc.otag_RO IN o.tags) & ~def.is_scalar(type))
    THEN
      IF type.mode # pc.ty_array_of THEN
        -- we have to make a reference to that type
        size := 4;
      END;
    END;
    IF db.list[0].info.Location = ir.LocInReg THEN
      begin_symb(S_REGISTER);
      cmd.GenWord(VAL(INT16, tind));
      cmd.GenWord(register_num(VAL(Reg,db.list[0].info.Value),size));
      write_name(o^.name^);
      end_symb;
    ELSE
      begin_symb(S_BPREL32);
      cmd.GenLWord(db.list[0].info.Value);
      cmd.GenWord(VAL(INT16, tind));
      write_name(o^.name^);
      end_symb;
    END;
  END;
END SymbEmitter;


-- bits in a primitive type description:
--  11 - reserved; 10-8 - mode; 7-4 - type; 3 - reserved; 2-0 - size

----- <type> type constanst -----

CONST
  void_type     = 003H;

  special_type  = 00H * 10H;
  signed_type   = 01H * 10H;
  unsign_type   = 02H * 10H;
  boolean_type  = 03H * 10H;
  real_type     = 04H * 10H;
  complex_type  = 05H * 10H;
  special2_type = 06H * 10H;
  real_int_type = 07H * 10H;

  real_char_type = real_int_type;

  ----- <size> type constants -----

  size_1_byte  = 0H;        (* --- sign/unsign, boolean --- *)
  size_2_bytes = 1H;
  size_4_bytes = 2H;
  size_8_bytes = 3H;

  size_real32  = 0H;        (* --- real, complex --- *)
  size_real64  = 1H;
  size_real80  = 2H;
  size_real128 = 3H;
  size_real48  = 4H;

  size_char      = 0H;      (* --- Real int --- *)
  size_wide_char = 1H;
  size_2_sign    = 2H;
  size_2_unsign  = 3H;
  size_4_sign    = 4H;
  size_4_unsign  = 5H;
  size_8_sign    = 6H;
  size_8_unsign  = 7H;

  ----- <mode> type constants -----

  near32ptr = 04H * 100H;


PROCEDURE (emit: EMIT_CV) PrimitiveTypeNo (type: pc.STRUCT): dbg.TYPE_INDEX;
VAR
  ptr: BOOLEAN;
  inx: dbg.TYPE_INDEX;
BEGIN
  IF (type.mode = pc.ty_opaque) AND (type.base # NIL) THEN
    type := type.base;
  END;
  IF type.mode = pc.ty_pointer THEN
    type := type.base;
    ptr := TRUE;
  ELSE
    ptr := FALSE;
  END;
  CASE type.mode OF
  | pc.ty_shortcard : inx := unsign_type + size_1_byte;
  | pc.ty_cardinal  : inx := unsign_type + size_2_bytes;    (* inx := real_int_type + size_2_unsign *)
  | pc.ty_longcard  : inx := real_int_type + size_4_unsign; (* inx := unsign_type + size_4_bytes *)
  | pc.ty_longlongcard:inx:= real_int_type + size_8_unsign; (* inx := unsign_type + size_4_bytes *)
  | pc.ty_shortint  : inx := signed_type + size_1_byte;
  | pc.ty_integer   : inx := signed_type + size_2_bytes;    (* inx := real_int_type + size_2_sign *)
  | pc.ty_longint   : inx := real_int_type + size_4_sign;   (* inx := signed_type + size_4_bytes *)
  | pc.ty_longlongint: inx := real_int_type + size_8_sign;
  | pc.ty_real      : inx := real_type + size_real32;
  | pc.ty_longreal  : inx := real_type + size_real64;
  | pc.ty_ld_real   : inx := real_type + size_real80;
  | pc.ty_complex   : inx := complex_type + size_real32;
  | pc.ty_lcomplex  : inx := complex_type + size_real64;
  | pc.ty_boolean   : IF env.config.Option("DBGVCCOMPATIBLE") THEN
                        CASE def.type_size(type.base) OF
                        | 1:  inx := unsign_type + size_1_byte;
                        | 2:  inx := unsign_type + size_2_bytes;
                        | 4:  inx := unsign_type + size_4_bytes;
                        | 8:  inx := unsign_type + size_8_bytes;
                        END;
                      ELSE
                        CASE def.type_size(type.base) OF
                        | 1:  inx := boolean_type + size_1_byte;
                        | 2:  inx := boolean_type + size_2_bytes;
                        | 4:  inx := boolean_type + size_4_bytes;
                        | 8:  inx := boolean_type + size_8_bytes;
                        END;
                      END;
  | pc.ty_char,
    pc.ty_uchar     : CASE def.type_size(type.base) OF
                      | 1:  inx := real_char_type + size_1_byte;
                      | 2:  inx := real_char_type + size_2_bytes;
                      | 4:  inx := real_char_type + size_4_bytes;
                      END;
  | pc.ty_AA        : IF ptr THEN inx := -1;
                      ELSE ptr := TRUE; inx := void_type; (* void *)
                      END;
  | pc.ty_range     : inx := emit.PrimitiveTypeNo(type.base);
  | pc.ty_opaque    : IF ptr THEN inx := -1
                      ELSE ptr := TRUE; inx := void_type; (* void *)
                      END;
  | pc.ty_pointer   : inx := -1;
  | pc.ty_set       : IF type.inx # NIL THEN inx := emit.PrimitiveTypeNo(type.inx);
                      ELSIF type.len > tun.BITSET_LEN THEN inx := -1;
                      ELSIF type.len <= 8 THEN inx := unsign_type + size_1_byte;
                      ELSIF type.len <=16 THEN inx := unsign_type + size_2_bytes;
                      ELSE                     inx := unsign_type + size_4_bytes;
                      END;
  | pc.ty_loc       : inx := unsign_type + size_1_byte;
  | pc.ty_protection: inx := unsign_type + size_2_bytes;
  | pc.ty_void      : inx := void_type;
  ELSE
    inx := -1;
  END;
  IF (inx > 0) & ptr THEN INC(inx, near32ptr); END;
  RETURN inx;
END PrimitiveTypeNo;


VAR
  emitCV: EMIT_CV;

BEGIN
  NEW(emitCV);
  reg.Register(opt.dbgFormat, opt.dbg_CV, emitCV);
END dbgCV.

