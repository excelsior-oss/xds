MODULE dbgTEXT;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;
IMPORT fn  := FileName;

IMPORT ir;
IMPORT pc  := pcK;
IMPORT at  := opAttrs;
IMPORT def := opDef;
IMPORT nms := ObjNames;
IMPORT pr  := opProcs;
IMPORT tun := opTune;
IMPORT str := Strings;
IMPORT xfs := xiFiles;
IMPORT env := xiEnv;
IMPORT dbg := DbgFace;
IMPORT opt := Options;
IMPORT reg := Registry;

<* IF ~TARGET_RISC AND ~TARGET_SPARC THEN *>
IMPORT Emit;
<* END *>


-- IMPLEMENTATION --

TYPE
  INT8  = sys.INT8;
  INT16 = sys.INT16;
  INT32 = sys.INT32;


CONST
  first_nonprimitive = 512;

(* ----- <type> type constanst ----- *)

  void_type      = 97H;
  signed_type    = 80H;
  unsign_type    = 84H;
  real_type      = 88H;
  complex_type   = 8CH;
  boolean_type   = 90H;
  character_type = 94H;

(* ----- <size> type constants ----- *)

  size_1_byte  = 0H;        (* --- sign/unsign, boolean --- *)
  size_2_bytes = 1H;
  size_4_bytes = 2H;

  size_real32  = 0H;        (* --- real, complex --- *)
  size_real64  = 1H;
  size_real80  = 2H;

  near32ptr = 020H;

  noreg = 0;

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

PROCEDURE register_num(r,sz: LONGINT): INTEGER;
<* IF ~TARGET_RISC AND ~TARGET_SPARC THEN *>
  VAR res: INTEGER;
BEGIN
  CASE r OF
  | Emit.EAX: IF    sz = 1 THEN res := AL;
              ELSIF sz = 2 THEN res := AX;
              ELSE              res := EAX;
              END;
  | Emit.ECX: IF    sz = 1 THEN res := CL;
              ELSIF sz = 2 THEN res := CX;
              ELSE              res := ECX;
              END;
  | Emit.EDX: IF    sz = 1 THEN res := DL;
              ELSIF sz = 2 THEN res := DX;
              ELSE              res := EDX;
              END;
  | Emit.EBX: IF    sz = 1 THEN res := BL;
              ELSIF sz = 2 THEN res := BX;
              ELSE              res := EBX;
              END;
  | Emit.ESP: IF    sz = 1 THEN res := AH;
              ELSIF sz = 2 THEN res := SP;
              ELSE              res := ESP;
              END;
  | Emit.EBP: IF    sz = 1 THEN res := CH;
              ELSIF sz = 2 THEN res := BP;
              ELSE              res := EBP;
              END;
  | Emit.ESI: IF    sz = 1 THEN res := DH;
              ELSIF sz = 2 THEN res := SI;
              ELSE              res := ESI;
              END;
  | Emit.EDI: IF    sz = 1 THEN res := BH;
              ELSIF sz = 2 THEN res := DI;
              ELSE              res := EDI;
              END;
  | Emit.FR1..Emit.FR8:  res := ST0 + VAL(INTEGER, r-Emit.FR1);
  ELSE res := noreg
  END;
  RETURN res
<* ELSE *>
BEGIN
  RETURN noreg;
<* END *>
END register_num;


CONST
  DBG_EXT = "DBG";

VAR
  dbg_file: xfs.TextFile;

PROCEDURE create_dbg_file (name-: ARRAY OF CHAR);
VAR
  fname: pc.STRING;
  s    : pc.STRING;
  ext  : ARRAY 16 OF CHAR;
BEGIN
  env.config.Equation("DBGEXT", s);
  IF s = NIL THEN
    COPY(DBG_EXT, ext);
  ELSE
    IF s[0] = '.' THEN
      ext[0] := 0X;
    ELSE
      ext[0] := '.';
      ext[1] := 0X;
    END;
    str.Append(s^, ext);
  END;
  xfs.sys.Create('', name, ext, fname);
  xfs.sys.UseFirst(fname^, fname);
  xfs.text.Open(fname^, TRUE);
  IF xfs.text.file = NIL THEN
    env.errors.Fault(env.null_pos, 424, xfs.text.msg^);
  END;
  dbg_file := xfs.text.file(xfs.TextFile);
END create_dbg_file;


PROCEDURE close_dbg_file;
VAR
  err: pc.STRING;
BEGIN
  dbg_file.CloseNew (TRUE, FALSE, FALSE, err);
  IF err # NIL THEN
    env.errors.Fault(env.null_pos, 432, err^);
  END;
  dbg_file := NIL;
END close_dbg_file;


PROCEDURE out (s-: ARRAY OF CHAR; SEQ arg: sys.BYTE);
BEGIN
  dbg_file.print (s, arg);
END out;


PROCEDURE GenByte (b: sys.BYTE);
BEGIN
  out ("%$2X", b);
END GenByte;

PROCEDURE GenWord (b: INT16);
BEGIN
  out ("%$4X", b);
END GenWord;

PROCEDURE GenLWord (b: INT32);
BEGIN
  out ("%$8X", b);
END GenLWord;


PROCEDURE begin_symb (s-: ARRAY OF CHAR);
BEGIN
  out ("<< S='%s' ", s);
END begin_symb;

PROCEDURE end_symb;
BEGIN
  out ("S >>\n");
END end_symb;


PROCEDURE begin_type(s-: ARRAY OF CHAR);
BEGIN
  out ("<< T[%$3X]='%s' ", dbg.write_type_cnt, s);
END begin_type;


PROCEDURE end_type;
BEGIN
  out ("T >>\n");
  INC(dbg.write_type_cnt);
END end_type;


PROCEDURE gen_Card_FID_span(len: LONGINT);
BEGIN
  IF len < 256 THEN
    GenByte(VAL(SHORTINT,len));
  ELSIF len < 256*256 THEN
    GenWord(VAL(INTEGER,len));
  ELSE
    GenLWord(len);
  END;
END gen_Card_FID_span;


PROCEDURE write_name(nm-: ARRAY OF CHAR);
BEGIN
  out ('"%s" ', nm);
END write_name;


PROCEDURE write_obj_name (o: pc.OBJECT);
VAR
  name: ARRAY 256 OF CHAR;
BEGIN
  dbg.write_obj_name(o, name);
  write_name(name);
END write_obj_name;


PROCEDURE write_type_name (t: pc.STRUCT);
VAR
  name: ARRAY 256 OF CHAR;
BEGIN
  dbg.write_type_name(t, name);
  write_name(name);
END write_type_name;


PROCEDURE write_procedure(t: pc.STRUCT);
BEGIN
  IF NOT ((t.obj = NIL) OR (t.obj.mode = pc.ob_type)) THEN
    ASSERT(dbg.get_index(t)=dbg.write_type_cnt);
  END;
  begin_type("PROCEDURE");
  out("(");
  GenWord(VAL(INT16, dbg.write_type_cnt+1)); ----- args_list has the next index ??!!
  out("): ");
  GenWord(VAL(INT16, dbg.get_index(t.base)));
  end_type;
  begin_type("PROC ARG LIST"); -- arglist;
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
  GenWord(VAL(INT16, dbg.get_index(f.type)));
  out(", ");
END write_field;


PROCEDURE write_field_name (f: pc.OBJECT);
BEGIN
  write_obj_name(f);
  out("[");
  gen_Card_FID_span(def.obj_offset(f));
  out("], ");
END write_field_name;


PROCEDURE write_o2_class_field(f: pc.OBJECT);
BEGIN
  ASSERT(f#NIL);
  GenWord(VAL(INT16, dbg.tindex_transfer));
  out(", ");
  INC(dbg.tindex_transfer);
END write_o2_class_field;


PROCEDURE write_o2_class_member(f: pc.OBJECT);
BEGIN
  begin_type("CLASS MEMBER");
  out("(");
  GenWord(VAL(INT16, dbg.get_index(f.type)));
  out(") [");
  gen_Card_FID_span(def.obj_offset(f));
  out("] ");
  write_obj_name(f);
  end_type;
END write_o2_class_member;


--------------------------------------------------------------------------

TYPE
  emit_rec = RECORD (dbg.emit_rec)
             END;
  EMIT_TEXT= POINTER TO emit_rec;



PROCEDURE (emit: EMIT_TEXT) TypeEmitter (ttag: dbg.TYPE_TAG; act: dbg.ACTION; type: pc.STRUCT);
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
  | dbg.ty_end:
    ASSERT((act = dbg.act_write) AND (type = NIL));
    dbg.type_cnt := MAX(dbg.TYPE_INDEX);
    dbg.write_type_cnt := MAX(dbg.TYPE_INDEX);
  | dbg.ty_range:
    CASE act OF
    | dbg.act_set:
      dbg.emit_type(act, type.base);
      dbg.put_index(type);          -- LF_RANGE
    | dbg.act_write:
      dbg.emit_type(act, type.base);
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      begin_type("RANGE");
      out("(");
      GenWord(VAL(INT16, dbg.get_index(type.base)));
      out(") [");
      len := pc.code.get_size(pc.su_size, type);
      IF type.base.mode IN {pc.ty_shortint, pc.ty_integer, pc.ty_longint} THEN
        int_val_min := type.min.get_integer();
        CASE len OF
        | 1: GenByte(VAL(INT8, int_val_min));
        | 2: GenWord(VAL(INT16, int_val_min));
        | 4: GenLWord(int_val_min);
        END;
        out("..");
        int_val_max := type.max.get_integer();
        <* PUSH *>
        <* WOFF902+ *>
        CASE len OF
        | 1: GenByte(VAL(INT8, int_val_max));
        | 2: GenWord(VAL(INT16, int_val_max));
        | 4: GenLWord(int_val_max);
        END;
        <* POP *>
      ELSE
        gen_Card_FID_span(type.min.get_cardinal());
        out("..");
        gen_Card_FID_span(type.max.get_cardinal());
      END;
      out("], ");
      write_type_name(type);
      end_type;
    END;
  | dbg.ty_enum:
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);     -- LF_ENUM
      INC(dbg.type_cnt);       -- LF_FIELDLIST
    | dbg.act_write:
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      begin_type("ENUM");
      out("[");
      GenWord(VAL(INT16, dbg.get_index(type.base)));
      out("] C=");
      gen_Card_FID_span(type.len-1);         -- max
      out(", EL=");
      GenWord(VAL(INT16, dbg.write_type_cnt+1));         -- @fList
      out(", ");
      write_type_name(type);
      end_type;
      begin_type("ENUM LIST");
      cns := type.prof;
      len := 1;
      REPEAT
        IF len MOD 3 = 0 THEN out("\n                      "); END;
        ASSERT((cns.mode = pc.ob_cons) & (cns.val.mode = pc.nd_value));
        write_obj_name(cns);
        val := cns.val.val.get_integer();
        out("[");
        gen_Card_FID_span(val);
        out("], ");
        cns := cns.next;
        INC(len);
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
        begin_type("POINTER TO");
        GenWord(VAL(INT16, dbg.tindex_transfer));
        end_type;
      ELSE
        base := type.base;
        ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
        begin_type("POINTER TO");
        GenWord(VAL(INT16, dbg.get_index(base)));
        out(", ");
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
        begin_type("REFERENCE");
        GenByte(0);
        GenWord(VAL(INT16, dbg.tindex_transfer));
        end_type;
      ELSE
        begin_type("REFERENCE");
        GenByte(0);
        GenWord(VAL(INT16, dbg.get_index(type)));
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
      begin_type("SET OF");
      GenWord(VAL(INT16, dbg.get_index(type.base)));
      out(", ");
      write_type_name(type);
      end_type;
    END;
  | dbg.ty_proctype:
    dbg.emit_type(dbg.act_write, type.base);
    CASE act OF
    | dbg.act_set:
      IF (type.obj = NIL) OR (type.obj.mode = pc.ob_type) THEN
        dbg.put_index(type);
      ELSE
        dbg.put_index_val(type, dbg.type_cnt);
      END;
      INC(dbg.type_cnt);         -- LF_PROCEDURE
      INC(dbg.type_cnt);         -- LF_ARGLIST
      dbg.emit_type(act, type.base);

    | dbg.act_write:
      IF (type.obj = NIL) OR (type.obj.mode = pc.ob_type) THEN
        ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
        begin_type("POINTER TO");
        GenWord(VAL(INT16, dbg.write_type_cnt+1));
        out(", ");
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
      IF dbg.get_index(type)=dbg.write_type_cnt THEN
        begin_type("ARRAY");
        out("[");
        GenWord(VAL(INT16, dbg.get_index(type.inx)));
        out("] OF ");
        GenWord(VAL(INT16, dbg.get_index(type.base)));
        out(", S=");
        GenLWord(pc.code.get_size(pc.su_bytes, type));
        out(", ");
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
      begin_type("ARRAY OF");
      GenWord(VAL(INT16, dbg.get_index(type.base)));
      out(", ");
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
      begin_type("OPEN ARRAY OF");
      GenWord(VAL(INT16, dbg.tindex_transfer));
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
      begin_type("RANGE");
      GenByte(0);
      card_val := pc.code.get_size(pc.su_bytes, type)-1;
      l_st := VAL(INTEGER, (card_val+1) DIV 256);
      GenWord(unsign_type+l_st);
      gen_Card_FID_span(0);
      gen_Card_FID_span(card_val);
      GenByte(0); -- No name
      end_type;
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      begin_type("RANGE ATTR");
      GenByte(0H + 00H + 0H + 0H); -- row major + unpacked + no descriptor + fixed len
      GenLWord(pc.code.get_size(pc.su_bytes, type));
      GenWord(VAL(INT16, dbg.get_index(type.inx)));
      GenWord(character_type (* + 0 *) );
      GenByte(0); -- No name;
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
        CASE type.flag OF
        | pc.flag_o2:
          INC(dbg.type_cnt);   -- Items Lists
          INC(dbg.type_cnt, dbg.count_fields (type, field_filter));
        ELSE
          dbg.field_list (type.prof, field_filter, set_field_type);
          INC(dbg.type_cnt, 2); ---  Types & Names Lists
        END;
      END;
    | dbg.act_write:
      dbg.field_list (type.prof, field_filter, write_field_type);
      inx := dbg.get_index(type);
      IF inx = dbg.write_type_cnt THEN
        CASE type.flag OF
        | pc.flag_o2:
          begin_type("CLASS");
          out("S=");
          GenLWord(pc.code.get_size(pc.su_bytes,type));
          out(" C=");
          GenWord(dbg.count_fields (type, field_filter));
          out(" (");
          GenWord(VAL(INT16, dbg.write_type_cnt+1));
          out("), ");
          write_type_name(type);
          end_type;
          begin_type("CLASS LIST");
          dbg.tindex_transfer := dbg.write_type_cnt+1;
          IF type.base # NIL THEN
            out("B=");
            GenWord(VAL(INT16, dbg.tindex_transfer));
            INC(dbg.tindex_transfer);
            out(", ");
          END;
          dbg.field_list (type.prof, field_filter, write_o2_class_field);
          end_type; -- LF_LIST
          IF type.base # NIL THEN
            begin_type("BASE CLASS");
            out("(");
            GenWord(VAL(INT16, dbg.get_index(type.base)));
            out(") ");
            end_type;
          END;
          dbg.field_list (type.prof, field_filter, write_o2_class_member);
        ELSE
          begin_type("RECORD");
          out("S=");
          GenLWord(pc.code.get_size(pc.su_bytes,type));
          out(" C=");
          GenWord(dbg.count_fields (type, field_filter));
          out(" T=");
          GenWord(VAL(INT16, dbg.write_type_cnt+1)); -- types list and names list
          out(" N=");
          GenWord(VAL(INT16, dbg.write_type_cnt+2)); -- always must be exact after record
          out(", ");
          write_type_name(type);
          end_type;
          begin_type("RECORD TYPES LIST");
          dbg.field_list (type.prof, field_filter, write_field);
          end_type;
          begin_type("RECORD NAMES LIST");
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
      dbg.emit_type(dbg.act_write, type.base);
      write_procedure(type);
    END;
  END;
END TypeEmitter;


PROCEDURE (emit: EMIT_TEXT) SymbEmitter (stag: dbg.SYMB_TAG; o: pc.OBJECT; tind: dbg.TYPE_INDEX);
VAR
  type: pc.STRUCT;
  a   : at.ATTR_EXT;
  db  : at.DBG_EXT;
BEGIN
  CASE stag OF
  | dbg.sy_start:
    ASSERT((o=NIL) AND (tind=-1));
    out ("<< DEBUG INFO\n");
  | dbg.sy_end:
    ASSERT((o=NIL) AND (tind=-1));
    out (">> DEBUG INFO\n");
  | dbg.sy_var:
    begin_symb("VAR");
    out ("[%$3X] ", tind);
    write_obj_name(o);
    end_symb;
  | dbg.sy_proc:
    begin_symb("PROC");
    out("L=");
    GenWord(VAL(INT16, dbg.proc_len(o)));
    out(" S=");
    GenWord(VAL(INT16, dbg.start_proc_debug(o)));
    out(" E=");
    GenWord(VAL(INT16, dbg.end_proc_debug(o)));
    out(", ");
    write_obj_name(o);
    end_symb;
  | dbg.sy_scope_open:
    ASSERT(tind = -1);
    out ("<< SCOPE\n");
  | dbg.sy_scope_close:
    ASSERT(tind = -1);
    out (">> SCOPE\n");
  | dbg.sy_local_var,
    dbg.sy_param:
    a := at.attr(o.ext, at.a_dbg);
    ASSERT(a # NIL);
    db := a(at.DBG_EXT);
    type := o.type;
    IF db.list[0].info.Location = ir.LocInReg THEN
      begin_symb("VAR [regi]");
      out("R=");
      GenByte(VAL(SHORTINT, register_num(db.list[0].info.Value, pc.code.get_size(pc.su_size, type))));
      out(" T=");
      GenWord(VAL(INT16, tind));
      out(", ");
      write_obj_name(o);
      end_symb;
    ELSE
      begin_symb("VAR [auto]");
      out("A=");
      GenLWord(db.list[0].info.Value);
      out(" T=");
      GenWord(VAL(INT16, tind));
      out(", ");
      write_obj_name(o);
      end_symb;
    END;
  END;
END SymbEmitter;


PROCEDURE (emit: EMIT_TEXT) PrimitiveTypeNo (type: pc.STRUCT): dbg.TYPE_INDEX;
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
  | pc.ty_real      : inx := real_type + size_real32;
  | pc.ty_longreal  : inx := real_type + size_real64;
  | pc.ty_ld_real   : inx := real_type + size_real80;
  | pc.ty_complex   : inx := complex_type + size_real32;
  | pc.ty_lcomplex  : inx := complex_type + size_real64;
  | pc.ty_boolean   : inx := emit.PrimitiveTypeNo(type.base);
                      inx := boolean_type   + (inx-80H) MOD 4;
  | pc.ty_char      : inx := emit.PrimitiveTypeNo(type.base);
                      inx := character_type + (inx-80H) MOD 4;
  | pc.ty_AA        : IF ptr THEN inx := -1;
                      ELSE ptr := TRUE; inx := void_type; (* void *)
                      END;
  | pc.ty_opaque    : IF ptr THEN inx := -1
                      ELSE ptr := TRUE; inx := void_type; (* void *)
                      END;
  | pc.ty_pointer   : inx := -1;
  | pc.ty_loc       : IF ptr AND (type.mno < 0) THEN
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

PROCEDURE (emit: EMIT_TEXT) generate (): BOOLEAN;
BEGIN
  create_dbg_file(at.curr_mod.name^);
  IF emit.generate^ () THEN
  END;
  close_dbg_file;
  RETURN TRUE;
END generate;

VAR
  emitTEXT: EMIT_TEXT;

BEGIN
  NEW(emitTEXT);
  reg.Register(opt.dbgFormat, opt.dbg_TEXT, emitTEXT);
END dbgTEXT.
 