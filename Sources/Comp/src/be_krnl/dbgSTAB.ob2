<* IF NOT DEFINED(DEBUG_STB) THEN *>
<* NEW DEBUG_STB- *>
<* END *>

MODULE dbgSTAB;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;
IMPORT fn  := FileName;
IMPORT scl := SysClock;
IMPORT tim := TimeConv;

IMPORT ir;
IMPORT pc  := pcK;
IMPORT at  := opAttrs;
IMPORT def := opDef;
IMPORT nms := ObjNames;
IMPORT xfs := xiFiles;
IMPORT env := xiEnv;
IMPORT dbg := DbgFace;
IMPORT opt := Options;
IMPORT str := Strings;
IMPORT reg := Registry;

--<* IF TARGET_386 THEN *>
--IMPORT Emit;
--<* END *>

<* IF DEFINED(DEBUG_STB) AND DEBUG_STB THEN *>
IMPORT Printf;
<* END *>



TYPE
  EMIT = RECORD (dbg.emit_rec)
           file: xfs.TextFile;       -- Output file
           text0*: ARRAY 32 OF CHAR; -- Depends from assembler
         END;

  EMIT_STAB * = POINTER TO EMIT;

PROCEDURE (emit: EMIT_STAB) Connect * (f: xfs.TextFile);
BEGIN
  emit.file := f;
END Connect;

PROCEDURE (emit: EMIT_STAB) Disconnect;
BEGIN
  emit.file := NIL;
END Disconnect;

PROCEDURE (emit: EMIT_STAB) print (f: ARRAY OF CHAR; SEQ arg: sys.BYTE);
BEGIN
  emit.file.print (f, arg);
END print;

VAR
  emitSTAB *: EMIT_STAB;


CONST
  N_UNDF  = 0;    -- N_* are STAB types
  N_GSYM  = 020H; -- Global symbols
  N_FNAME = 022H; -- procedure name (f77 kludge): name,,0
  N_FUN   = 024H; -- Function name or text segment variable
  N_STSYM = 026H; -- Data segment file-scope variable
  N_LCSYM = 028H; -- BSS segment file-scope variable
  N_MAIN  = 02AH; -- main routine name
  N_ROSYM = 02CH; -- Variable in ".rodata" section
  N_PC    = 030H; -- Global symbol (for pascal?)
  N_RSYM  = 040H; -- Register variable
  N_M2C   = 042H; -- Modula-2 compilation unit
  N_SLINE = 044H; -- Line number in text segment
  N_DLINE = 046H; -- Line number in data
  N_BLINE = 048H; -- Line number in bss
  N_DEFD  = 04AH; -- GNU MOdula-2 module dependency info
  N_FLINE = 04CH; -- function start/body/end line numbers (Solaris2)
  N_MOD2  = 050H; -- Modula-2 info for imc?????
  N_SSYM  = 060H; -- Structure or union element
  N_ENDM  = 062H; -- Last stab for module
  N_SO    = 064H; -- Source
  N_LSYM  = 080H; -- Stack variable or type
  N_BINCL = 082H; -- Header file: name,,0,0,0
  N_SOL   = 084H; -- Name of include file
  N_PSYM  = 0A0H; -- Parameter variable
  N_EINCL = 0A2H; -- End of include file
  N_ENTRY = 0A4H; -- Alternative entry point
  N_LBRAC = 0C0H; -- Beginning of a lexical block
  N_EXCL  = 0C2H; -- excluded include file
  N_SCOPE = 0C4H; -- M2 scope information
  N_RBRAC = 0E0H; -- End of lexical block
  N_BCOMM = 0E2H; -- begin common: name,,
  N_ECOMM = 0E4H; -- end common: name,,
  N_ECOML = 0E8H; -- end common (local name): ,,address
  N_LENG  = 0FEH; -- second stab entry with length information

  N_LPOOLSYM  = 032H; -- Local pool symbol type
  N_AUDITINFO = 0eaH; -- Audit information symbol type

  STABS = ".stabs\t";
  STABN = ".stabn\t";
  STABD = ".stabd\t";

  LINUM    = ".LN"; -- Prefix for line-number label
  LEXBLOCK = "LB";  -- Prefix for procedure lexical block


TYPE
  STAB = RECORD
           N_str   *: sys.CARD32; -- string index
           N_type  *: sys.CARD8;  -- type
           N_other *: sys.INT8;   -- attibute
           N_desc  *: sys.INT16;  -- description
           N_value *: sys.CARD32; -- string index
         END;

  STABX = RECORD (STAB)
            obj *: pc.OBJECT; -- object reference to generate fixup
                              -- and this is to tell whether we
                              -- should make a fixup for the stub
          END;


  PSTABS = POINTER TO ARRAY OF STABX;

  STRINGS = POINTER TO ARRAY OF CHAR;


VAR
  Stabs      : PSTABS;    -- stabs array
  StabsPos   : sys.INT32; -- stabs current pos
  Strings    : STRINGS;   -- strings of stabs
  StringsPos : sys.INT32; -- strings current pos

  Binary: BOOLEAN; -- binary segments

  linum_count: sys.INT32; -- counter line-code label


PROCEDURE put_str (str-: ARRAY OF CHAR): sys.CARD32;
VAR
  N, i, l: sys.INT32;
  tmp: STRINGS;
BEGIN
  l := LENGTH(str);
  IF l = 0 THEN RETURN 0; END;
  IF Strings = NIL THEN
    NEW(Strings, 1);
    ASSERT(Strings#NIL);
    Strings^[0] := 0C; -- в начале всегда записана пустая строка
    StringsPos := 1;
  END;
  LOOP
    N := LEN(Strings^);
    IF StringsPos+l < N THEN EXIT; END;
    NEW(tmp, 2*N);
    ASSERT(tmp#NIL);
    FOR i := 0 TO N-1 DO tmp^[i] := Strings^[i]; END;
    Strings := tmp;
  END;
  N := StringsPos;
  i := 0;
  REPEAT
    Strings^[StringsPos] := str[i];
    INC(StringsPos);
    INC(i);
  UNTIL i > l;
  RETURN N;
END put_str;


PROCEDURE put_s (strx- : ARRAY OF CHAR;
                 type  : sys.CARD8;
                 other : sys.INT8;
                 desc  : sys.INT16;
                 value-: ARRAY OF CHAR;
                 obj   : pc.OBJECT);
VAR
  N, i: sys.INT32;
  tmp : PSTABS;
BEGIN
  IF Binary THEN
    IF Stabs = NIL THEN
      NEW(Stabs, 1);
      ASSERT(Stabs#NIL);
      StabsPos := 0;
    ELSIF StabsPos > LEN(Stabs^)-1 THEN
      N := LEN(Stabs^);
      NEW(tmp, 2*N);
      ASSERT(tmp#NIL);
      FOR i := 0 TO N-1 DO tmp^[i] := Stabs^[i]; END;
      Stabs := tmp;
    END;
    Stabs^[StabsPos].N_str   := put_str (strx);
    Stabs^[StabsPos].N_type  := type;
    Stabs^[StabsPos].N_other := other;
    Stabs^[StabsPos].N_desc  := desc;
    Stabs^[StabsPos].N_value := put_str (value);
    Stabs^[StabsPos].obj     := obj;
    INC(StabsPos);
  ELSE
    emitSTAB.file.print (STABS + '"%s",%d,%d,%i,%s\n', strx, type, other, desc, value);
  END;
END put_s;


PROCEDURE put_sf (strx-  : ARRAY OF CHAR;
                  type   : sys.CARD8;
                  other  : sys.INT8;
                  desc   : sys.INT16;
                  value- : ARRAY OF CHAR;
                  obj    : pc.OBJECT;
                  SEQ arg: sys.BYTE);
VAR
  s: ARRAY 1024 OF CHAR;
BEGIN
  fmt.print (s, strx, arg);
  put_s (s, type, other, desc, value, obj);
END put_sf;


PROCEDURE put_type (fmt-: ARRAY OF CHAR; SEQ arg: sys.BYTE);
BEGIN
  put_sf (fmt, N_LSYM, 0, 0, "0", NIL, arg);
END put_type;


CONST
  MSL1_68K     = 0;
  MSL1_X86     = 1;
  GCC_X86      = 2;
  DefaultStabs = MSL1_68K;

VAR
  stabs_style: SHORTINT;


CONST
  first_nonprimitive        = 100;

  pt_int                    = 1;
  pt_char                   = 2;
  pt_long_int               = 3;
  pt_unsigned_int           = 4;
  pt_long_unsigned_int      = 5;
  pt_short_int              = 6;
  pt_long_long_int          = 7;
  pt_short_unsigned_int     = 8;
  pt_long_long_unsigned_int = 9;
  pt_signed_char            = 10;
  pt_unsigned_char          = 11;
  pt_float                  = 12;
  pt_double                 = 13;
  pt_long_double            = 14;
  pt_void                   = 15;

PROCEDURE write_std_types;
BEGIN
  put_type ("int:t1=r1;%d;%d;", MIN(LONGINT), MAX(LONGINT));
  put_type ("char:t2=r2;0;127;");
  put_type ("long int:t3=r1;%d;%d;", MIN(LONGINT), MAX(LONGINT));
  put_type ("unsigned int:t4=r1;0;-1;");
  put_type ("long unsigned int:t5=r1;0;-1;");
  put_type ("short int:t6=r1;-32768;32767;");
  put_type ("long long int:t7=r1;0;-1;");
  put_type ("short unsigned int:t8=r1;0;65535;");
  put_type ("long long unsigned int:t9=r1;0;-1;");
  put_type ("signed char:t10=r1;-128;127;");
  put_type ("unsigned char:t11=r1;0;255;");
  put_type ("float:t12=r1;4;0;");
  put_type ("double:t13=r1;8;0;");
  put_type ("long double:t14=r1;8;0;");
  put_type ("void:t15=15", N_LSYM, 0, 0, 0);
END write_std_types;


VAR
  current_write_name: ARRAY 256 OF CHAR;

PROCEDURE write_name (nm-: ARRAY OF CHAR);
BEGIN
  COPY(nm, current_write_name);
END write_name;

PROCEDURE write_obj_name (o: pc.OBJECT);
VAR
  name: ARRAY 256 OF CHAR;
BEGIN
  dbg.write_obj_name(o, name);
  write_name (name);
END write_obj_name;

PROCEDURE write_asm_obj_name (o: pc.OBJECT);
VAR
  name: ARRAY 256 OF CHAR;
BEGIN
  nms.makename (o, name);
  write_name (name);
END write_asm_obj_name;

PROCEDURE write_type_name (t: pc.STRUCT);
VAR
  name: ARRAY 256 OF CHAR;
BEGIN
  dbg.write_type_name(t, name);
  write_name (name);
END write_type_name;


PROCEDURE get_type_length(t: pc.STRUCT): sys.INT32;
BEGIN
  RETURN pc.code.get_size(pc.su_bytes, t);
END get_type_length;


PROCEDURE set_field_type (f: pc.OBJECT);
BEGIN
  dbg.emit_type (dbg.act_set, f.type);
END set_field_type;


PROCEDURE write_field_type (f: pc.OBJECT);
BEGIN
  dbg.emit_type (dbg.act_write, f.type);
END write_field_type;


PROCEDURE field_filter (f: pc.OBJECT): BOOLEAN;
BEGIN
  ASSERT(f # NIL);
  RETURN TRUE;
END field_filter;


VAR
  RecordFields: ARRAY 80 OF CHAR;

PROCEDURE make_field (f: pc.OBJECT);
VAR
  bit_offset, bit_width: LONGINT;
  s, t: ARRAY 256 OF CHAR;
  offs, len: sys.INT8;
  type_no: sys.INT32;
BEGIN
  CASE f.mode OF
  | pc.ob_field:
    type_no    := dbg.get_index(f.type);
    bit_offset := 8 * def.obj_offset (f);
    bit_width  := 8 * get_type_length (f.type);
  | pc.ob_field_bts:
    type_no := pt_unsigned_int;
    at.bit_field_info (f, bit_offset, offs, len);
    bit_offset := 8 * bit_offset + offs;
    bit_width := len;
  END;
  write_obj_name(f);
  fmt.print (s, "%s:%d,%d,%d;", current_write_name, type_no, bit_offset, bit_width);
  IF LENGTH(RecordFields)+LENGTH(s) > LEN(RecordFields)-16 THEN
    -- выдадим stab с переводом на новую строку
    put_type ("%s\\\", RecordFields);
    RecordFields := "";
  END;
  fmt.print (t, "%s%s", RecordFields, s);
  COPY(t, RecordFields);
END make_field;


PROCEDURE write_src_name;
VAR
  fname: xfs.String;
  fullname, dir, name: ARRAY 1024 OF CHAR;
  ln, i, j, k: sys.INT32;
BEGIN
  xfs.sys.GetFullPathName (env.info.file^, fullname);
  xfs.sys.ConvertFromHost (fullname, fname);
  xfs.sys.ConvertToTarget (fname^, fname);
  COPY(fname^, fullname);
  ln := LENGTH(fullname);
  i := ln;
  REPEAT
    DEC(i);
  UNTIL (i < 0) OR (fullname[i] = '\') OR (fullname[i] = '/') OR (fullname[i] = ':');
  j := 0;
  FOR k := i+1 TO ln-1 DO
    name[j] := fullname[k];
    INC (j)
  END;
  name[j] := 0C;
  j := 0;
  FOR k := 0 TO i DO
    dir[j] := fullname[k];
    INC (j)
  END;
  dir[j] := 0C;
  IF name # "" THEN
    IF dir # "" THEN
      put_sf('%s', N_SO, 0, 0, emitSTAB.text0, NIL, dir);
    END;
    put_sf('%s', N_SO, 0, 0, emitSTAB.text0, NIL, name);
  END;
END write_src_name;


PROCEDURE write_main_program;
BEGIN
  IF at.main THEN
    IF at.GENDLL IN at.COMP_MODE THEN
      write_name(nms.DLL_ENTR);
    ELSE
      write_name(nms.MAIN_PROG_ENTR);
    END;
    put_sf ("%s", N_MAIN, 0, 0, "0", at.curr_mod, current_write_name);
  END;
END write_main_program;


CONST
  noreg = 0;

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


<* PUSH *>
<* WOFF+ *>
PROCEDURE register_num (r, sz: sys.INT32): sys.INT32;
<* POP *>
BEGIN
<* IF TARGET_RISC OR TARGET_SPARC  THEN *>
  RETURN noreg;
<* ELSE *>
  RETURN r;
<* END *>
END register_num;



PROCEDURE (emit: EMIT_STAB) TypeEmitter * (ttag: dbg.TYPE_TAG; act: dbg.ACTION; type: pc.STRUCT);
VAR
  cns  : pc.OBJECT;
  val  : sys.INT32;
  base : pc.STRUCT;
  inx  : dbg.TYPE_INDEX;
  imin : sys.INT32;
  imax : sys.INT32;
  cmin : sys.CARD32;
  cmax : sys.CARD32;
  s, f : ARRAY 80 OF CHAR;
  count: sys.INT32;
BEGIN
  CASE ttag OF
  | dbg.ty_start:
    ASSERT((act = dbg.act_write) AND (type = NIL));
    dbg.type_cnt := first_nonprimitive;
    dbg.write_type_cnt := first_nonprimitive;
    write_std_types;
  | dbg.ty_end:
    ASSERT((act = dbg.act_write) AND (type = NIL));
    dbg.type_cnt := MAX(sys.INT16);
    dbg.write_type_cnt := MAX(sys.INT16);
  | dbg.ty_range:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);
    | dbg.act_write:
      write_type_name (type);
      inx := dbg.get_index(type.base);
      IF type.base.mode IN pc.TY_SET{pc.ty_shortint, pc.ty_integer, pc.ty_longint} THEN
        imin := type.min.get_integer();
        imax := type.max.get_integer();
        put_type ("%s:t%u=r%u;%i;%i;", current_write_name, dbg.write_type_cnt, inx, imin, imax);
      ELSE
        cmin := type.min.get_cardinal();
        cmax := type.max.get_cardinal();
        put_type ("%s:t%u=r%u;%u;%u;", current_write_name, dbg.write_type_cnt, inx, cmin, cmax);
      END;
      INC(dbg.write_type_cnt);
    END;
  | dbg.ty_enum:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);
    | dbg.act_write:
      inx := dbg.get_index(type);
      ASSERT(inx=dbg.write_type_cnt);
      write_type_name (type);
      fmt.print (s, "%s:T%d=e", current_write_name, inx);
      count := type.len;     -- count
      cns := type.prof;
      REPEAT
        ASSERT((cns.mode = pc.ob_cons) & (cns.val.mode = pc.nd_value));
        write_obj_name(cns);
        IF LENGTH(s)+LENGTH(current_write_name) > LEN(s)-16 THEN
          -- выдадим stab с переводом на новую строку
          put_type ("%s\\\", s);
          s := "";
        END;
        val := cns.val.val.get_integer();
        IF type.base.mode IN pc.TY_SET{pc.ty_shortint, pc.ty_integer, pc.ty_longint} THEN
          fmt.print (f, "%s%s:%i,", s, current_write_name, val);
        ELSE
          fmt.print (f, "%s%s:%i,", s, current_write_name, val);
        END;
        COPY(f, s);
        DEC(count);
        cns := cns.next;
      UNTIL cns = NIL;
      ASSERT(count=0);
      put_type ("%s;", s);
      INC(dbg.write_type_cnt);
    END;
  | dbg.ty_pointer, dbg.ty_reference:
    CASE act OF
    | dbg.act_set:
      ASSERT((ttag#dbg.ty_reference) AND (type#NIL));
      dbg.put_index(type);
      dbg.emit_type(act, type.base);   -- [base]
    | dbg.act_write:
      IF type = NIL THEN
        put_type ("%s:t%d=*%d", dbg.tname_transfer, dbg.write_type_cnt, dbg.tindex_transfer);
        INC(dbg.write_type_cnt);
      ELSE
        ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
        base := type.base;
        write_type_name (type);
        put_type ("%s:t%d=*%d", current_write_name, dbg.write_type_cnt, dbg.get_index(base));
        INC(dbg.write_type_cnt);
        dbg.emit_type(act, base);
      END;
    END;
  | dbg.ty_opaque:
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);
      base := type.base;
      IF base # NIL THEN
        dbg.emit_type(act, base);
      END;
    | dbg.act_write:
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      write_type_name (type);
      COPY(current_write_name, dbg.tname_transfer);
      base := type.base;
      IF (base = NIL) OR (base.base = NIL) THEN
        dbg.tindex_transfer := pt_void;
      ELSE
         dbg.tindex_transfer := dbg.get_index(base.base);
      END;
      emit.TypeEmitter (dbg.ty_pointer, act, NIL);
      IF base # NIL THEN
        dbg.emit_type(act, base);
      END;
    END;
  | dbg.ty_set:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);
    | dbg.act_write:
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      write_type_name(type);
      put_type ("%s:t%d=ar%d;0;%d;%d", current_write_name, dbg.write_type_cnt, pt_int,
                                       get_type_length(type)-1, pt_unsigned_char);
      INC(dbg.write_type_cnt);
    END;
  | dbg.ty_array:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      IF NOT (at.tmark_db_index IN type.marks) THEN
        dbg.put_index(type);
      END;
    | dbg.act_write:
      inx := dbg.get_index(type);
      IF inx = dbg.write_type_cnt THEN
        write_type_name(type);
        IF type.inx.mode IN pc.TY_SET{pc.ty_shortint, pc.ty_integer, pc.ty_longint} THEN
          imin := type.min.get_integer();
          imax := type.max.get_integer();
          ASSERT(imin<=imax);
          IF imin < 0 THEN
            imax := imax-imin;
            imin := 0;
          END;
          cmin := imin;
          cmax := imax;
        ELSE
          cmin := type.min.get_cardinal();
          cmax := type.max.get_cardinal();
        END;
        put_type ("%s:t%d=ar%d;%d;%d;%d", current_write_name, dbg.write_type_cnt, pt_int,
                                         cmin, cmax, dbg.get_index(type.base));
        INC(dbg.write_type_cnt);
      ELSE
        ASSERT(inx < dbg.write_type_cnt);
      END;
    END;
  | dbg.ty_array_of:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      IF NOT (at.tmark_db_index IN type.marks) THEN
        dbg.put_index(type);
      END;
    | dbg.act_write:
      inx := dbg.get_index(type);
      IF inx = dbg.write_type_cnt THEN
        write_type_name(type);
        put_type ("%s:t%d=A%d", current_write_name, dbg.write_type_cnt, dbg.get_index(type.base));
        INC(dbg.write_type_cnt);
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
      (* ??? по типу узнать где лежит длина *)
      put_type (":t%d=A%d", dbg.tname_transfer, dbg.write_type_cnt, dbg.tindex_transfer);
      INC(dbg.write_type_cnt);
    END;
  | dbg.ty_SS:
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);
    | dbg.act_write:
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      write_type_name(type);
      put_type ("%s:t%d=ar%d;%d;%d;%d", current_write_name, dbg.write_type_cnt, pt_int, 0,
                        get_type_length(type)-1, dbg.get_index(type.base));
      INC(dbg.write_type_cnt);
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
        inx := dbg.write_type_cnt;
        INC(dbg.write_type_cnt);
        dbg.field_list(type.prof, field_filter, write_field_type);
        write_type_name(type);
--        put_type ("%s:T%d=s%d", current_write_name, inx, get_type_length(type));
--        RecordFields := "";
        fmt.print (RecordFields, "%s:T%d=s%d", current_write_name, inx, get_type_length(type));
        dbg.field_list(type.prof, field_filter, make_field);
        put_type ("%s;", RecordFields);
      ELSE
        ASSERT(inx < dbg.write_type_cnt);
      END;
    END;
  | dbg.ty_proctype:
    base := type.base;
    dbg.emit_type(act, base);
    CASE act OF
    | dbg.act_set:
      IF (type.obj = NIL) OR (type.obj.mode = pc.ob_type) THEN
        dbg.put_index(type);
      ELSE
        dbg.put_index_val(type, dbg.get_index(base));
      END;
    | dbg.act_write:
      ASSERT(dbg.get_index(type)=dbg.write_type_cnt);
      IF (type.obj = NIL) OR (type.obj.mode = pc.ob_type) THEN
        dbg.tindex_transfer := dbg.write_type_cnt+1;
        dbg.tname_transfer := "";
        emit.TypeEmitter (dbg.ty_pointer, act, NIL);
        write_type_name(type);
        put_type ("%s:t%d=f%d", current_write_name, dbg.write_type_cnt, dbg.get_index(type.base));
      END;
    END;
  | dbg.ty_module:
    base := type.base;
    dbg.emit_type(act, base);
    CASE act OF
    | dbg.act_set:
      IF NOT (at.tmark_db_index IN type.marks) THEN
        dbg.put_index_val(type, dbg.get_index(base));
      END;
    | dbg.act_write:
    END;
  ELSE
  END;
END TypeEmitter;


PROCEDURE (emit: EMIT_STAB) LexBlockName * (stag: dbg.SYMB_TAG; o: pc.OBJECT; VAR name: ARRAY OF CHAR);
BEGIN
  ASSERT((o.mode IN pc.PROCs) OR (o.mode = pc.ob_module));
  write_asm_obj_name (o);
  CASE stag OF
  | dbg.sy_scope_open:
    fmt.print (name, ".%sB%s", LEXBLOCK, current_write_name);
  | dbg.sy_scope_close:
    fmt.print (name, ".%sE%s", LEXBLOCK, current_write_name);
  END;
END LexBlockName;


PROCEDURE (emit: EMIT_STAB) SymbEmitter * (stag: dbg.SYMB_TAG; o: pc.OBJECT; tind: dbg.TYPE_INDEX);

  PROCEDURE rtnsize (t: pc.STRUCT): sys.INT32;
  BEGIN
    ASSERT(t # NIL);
    RETURN 0;
  END rtnsize;

  PROCEDURE write_nested_scopes (o: pc.OBJECT; VAR name: ARRAY OF CHAR);
  BEGIN
    ASSERT((o.mode IN pc.PROCs) OR (o.mode = pc.ob_module));
    COPY('', name);
    IF (at.DbgNestedProc IN at.COMP_MODE) AND (o.name # NIL) AND (o.lev > 0) THEN
      str.Append(',', name);
      write_obj_name(o);
      str.Append (current_write_name, name);
      str.Append(',', name);
      write_obj_name(o.host.obj);
      str.Append (current_write_name, name);
    END;
  END write_nested_scopes;

VAR
  type  : pc.STRUCT;
  a     : at.ATTR_EXT;
  db    : at.DBG_EXT;
  size  : LONGINT;
  ftype : CHAR;
  reg   : sys.INT32;
  s     : ARRAY 16 OF CHAR;
  scopes: ARRAY 1024 OF CHAR;
  name  : ARRAY 1024 OF CHAR;

BEGIN
  CASE stag OF
  | dbg.sy_start:
    write_main_program;
  | dbg.sy_end:
  | dbg.sy_var:
    write_obj_name(o);
    put_sf("%s:G%d", N_GSYM, 0, 0, "0", o, current_write_name, tind)
  | dbg.sy_proc:
    IF o.is_public() THEN
      ftype := 'F';
    ELSE
      ftype := 'f';
    END;
    write_nested_scopes(o, scopes);
    write_obj_name(o);
    COPY(current_write_name, name);
    write_asm_obj_name(o);
    IF stabs_style = GCC_X86 THEN
      emitSTAB.file.print(STABS+'"%s:%c(0,%d)%s",%d,0,%d,%s\n', name, ftype, tind, scopes, N_FUN, rtnsize(o.type.base), current_write_name);
    ELSE
      emitSTAB.file.print(STABS+'"%s:%c%d%s",%d,0,%d,%s\n', name, ftype, tind, scopes, N_FUN, rtnsize(o.type.base), current_write_name);
    END;
  | dbg.sy_scope_open:
    ASSERT(tind = -1);
    -->>> SHEV
    --emit.LexBlockName (dbg.sy_scope_open, o, s);
    --put_s ("", N_LBRAC, 0, 0, s, o);
  | dbg.sy_scope_close:
    ASSERT(tind = -1);
    -->>> SHEV
    --emit.LexBlockName (dbg.sy_scope_close, o, s);
    --put_s ("", N_RBRAC, 0, 0, s, o);
  | dbg.sy_local_var,
    dbg.sy_param:
    a := at.attr(o.ext, at.a_dbg);
    IF a # NIL THEN
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
      write_obj_name(o);
      IF db.list[0].info.Location = ir.LocInReg THEN
        reg := register_num(db.list[0].info.Value, size);
        fmt.print (s, "%d", reg);
        IF stag = dbg.sy_local_var THEN
          put_sf ("%s:%d", N_LSYM, 0, 0, s, o, current_write_name, tind);
        ELSE
          put_sf ("%s:p%d", N_PSYM, 0, 0, s, o, current_write_name, tind);
          put_sf ("%s:r%d", N_RSYM, 0, 0, s, o, current_write_name, tind);
        END;
      ELSE
        fmt.print (s, "%d", db.list[0].info.Value);
        IF stag = dbg.sy_local_var THEN
          put_sf ("%s:%d", N_LSYM, 0, 0, s, o, current_write_name, tind);
        ELSE
          IF (o.mode = pc.ob_varpar)
            OR ((pc.otag_RO IN o.tags) & ~def.is_scalar(type))
          THEN
            put_sf ("%s:p%d", N_PSYM, 0, 0, s, o, current_write_name, tind);
          ELSE
            put_sf ("%s:v%d", N_PSYM, 0, 0, s, o, current_write_name, tind);
          END;
        END;
      END;
    END;
  ELSE
  END;
END SymbEmitter;


PROCEDURE (emit: EMIT_STAB) LineNoEmitter * (p: pc.OBJECT; col, line: sys.INT32);
VAR
  proc_name: ARRAY 256 OF CHAR;
BEGIN
  nms.makename(p, proc_name);
 <* IF TARGET_68K THEN *>
  emitSTAB.file.print (STABD + "0x%x,%d,%d\n", N_SLINE, col, line);
 <* ELSE *>
  emitSTAB.file.print (STABN + "%d,0,%d,%s%d-%s\n%s%d:\n", N_SLINE, line, LINUM, linum_count, proc_name, LINUM, linum_count);
 <* END *>
  INC(linum_count);
END LineNoEmitter;


PROCEDURE (emit: EMIT_STAB) PrimitiveTypeNo (type: pc.STRUCT): dbg.TYPE_INDEX;
VAR
  inx: sys.INT16;
  sz : sys.INT32;
BEGIN
  CASE type.mode OF
  | pc.ty_shortcard : inx := pt_unsigned_char;
  | pc.ty_cardinal  : inx := pt_unsigned_int;
  | pc.ty_longcard  : inx := pt_long_unsigned_int;
  | pc.ty_shortint  : inx := pt_signed_char;
  | pc.ty_integer   : inx := pt_short_int;
  | pc.ty_longint   : inx := pt_long_int;
  | pc.ty_real      : inx := pt_float;
  | pc.ty_longreal  : inx := pt_double;
  | pc.ty_ld_real   : inx := pt_long_double;
  | pc.ty_boolean   : sz := def.type_size(type.base);
                      IF    sz = 1 THEN    inx := pt_unsigned_char;
                      ELSIF sz = 2 THEN    inx := pt_short_unsigned_int;
                      ELSIF sz = 4 THEN    inx := pt_long_unsigned_int;
                      ELSE ASSERT(sz = 8); inx := pt_long_long_unsigned_int;
                      END;
  | pc.ty_char      : sz := def.type_size(type.base);
                      IF    sz = 1 THEN    inx := pt_unsigned_char;
                      ELSIF sz = 2 THEN    inx := pt_short_unsigned_int;
                      ELSE ASSERT(sz = 4); inx := pt_long_unsigned_int;
                      END;
  | pc.ty_loc       : inx := pt_unsigned_char;
  | pc.ty_protection: inx := pt_short_unsigned_int;
  | pc.ty_void      : inx := pt_void;
(*
  | pc.ty_AA        : inx := pt_void;
  | pc.ty_complex   : inx := complex_type + size_real32;
  | pc.ty_lcomplex  : inx := complex_type + size_real64;
*)
  ELSE
    inx := 0;
  END;
  RETURN inx;
END PrimitiveTypeNo;


<* IF DEFINED(DEBUG_STB) AND DEBUG_STB THEN *>

PROCEDURE PrintStabType (type: sys.CARD16);
BEGIN
  CASE type OF
  | N_UNDF      : Printf.printf("N_UNDF");
  | N_GSYM      : Printf.printf("N_GSYM");
  | N_FUN       : Printf.printf("N_FUN");
  | N_STSYM     : Printf.printf("N_STSYM");
  | N_LCSYM     : Printf.printf("N_LCSYM");
  | N_MAIN      : Printf.printf("N_MAIN");
  | N_ROSYM     : Printf.printf("N_ROSYM");
  | N_PC        : Printf.printf("N_PC");
  | N_RSYM      : Printf.printf("N_RSYM");
  | N_M2C       : Printf.printf("N_M2C");
  | N_SLINE     : Printf.printf("N_SLINE");
  | N_DLINE     : Printf.printf("N_DLINE");
  | N_BLINE     : Printf.printf("N_BLINE");
  | N_DEFD      : Printf.printf("N_DEFD");
  | N_FLINE     : Printf.printf("N_FLINE");
  | N_MOD2      : Printf.printf("N_MOD2");
  | N_SSYM      : Printf.printf("N_SSYM");
  | N_ENDM      : Printf.printf("N_ENDM");
  | N_SO        : Printf.printf("N_SO");
  | N_LSYM      : Printf.printf("N_LSYM");
  | N_SOL       : Printf.printf("N_SOL");
  | N_PSYM      : Printf.printf("N_PSYM");
  | N_EINCL     : Printf.printf("N_EINCL");
  | N_ENTRY     : Printf.printf("N_ENTRY");
  | N_LBRAC     : Printf.printf("N_LBRAC");
  | N_RBRAC     : Printf.printf("N_RBRAC");
  | N_FNAME     : Printf.printf("N_FNAME");
  | N_BINCL     : Printf.printf("N_BINCL");
  | N_EXCL      : Printf.printf("N_EXCL");
  | N_SCOPE     : Printf.printf("N_SCOPE");
  | N_BCOMM     : Printf.printf("N_BCOMM");
  | N_ECOMM     : Printf.printf("N_ECOMM");
  | N_ECOML     : Printf.printf("N_BCOML");
  | N_LENG      : Printf.printf("N_LENG");
  | N_LPOOLSYM  : Printf.printf("N_LPOOLSYM");
  | N_AUDITINFO : Printf.printf("N_AUDITINFO");
  END;
END PrintStabType;


PROCEDURE PrintStabs;
VAR
  i, j: sys.INT32;
BEGIN
  i := 0;
  WHILE i < StabsPos DO
    Printf.printf('.stabs "');
    j := Stabs^[i].N_str;
    WHILE Strings^[j] # 0C DO
      Printf.printf('%c', Strings^[j]);
      INC(j);
    END;
    PrintStabType(Stabs^[i].N_type);
    Printf.printf('",');
    Printf.printf('%i,%i,%i\n', Stabs^[i].N_other,
                  Stabs^[i].N_desc, Stabs^[i].N_value);
    INC(i);
  END;
END PrintStabs;
<* END *>


PROCEDURE (emit: EMIT_STAB) ini*;
BEGIN
-- Если режим вывода текстовый, необходимо инициализировать
-- файл для вывода процедурой
  linum_count := 0;
  Stabs := NIL;
  Strings := NIL;
  Binary := emit.Binary;
 <* IF TARGET_68K THEN *>
  stabs_style := MSL1_68K;
 <* ELSIF TARGET_386 THEN *>
    stabs_style := GCC_X86;
 <* ELSE *>
  stabs_style := DefaultStabs;
 <* END *>
  write_src_name;
END ini;

PROCEDURE (emit: EMIT_STAB) exi*;
BEGIN
 <* IF DEFINED(DEBUG_STB) AND DEBUG_STB THEN *>
  PrintStabs;
 <* END *>
  Stabs := NIL;
  Strings := NIL;
  emit.Disconnect;
END exi;

PROCEDURE (emit: EMIT_STAB) generate (): BOOLEAN;
BEGIN
  IF Binary THEN -- if output to assembler then do nothing
    emit.ini;
    IF emit.generate^ () THEN
    END;
    emit.exi;
  END;
  RETURN TRUE;
END generate;

BEGIN
  NEW(emitSTAB);
  reg.Register(opt.dbgFormat, opt.dbg_STAB, emitSTAB);
END dbgSTAB.

