MODULE dbgGO32;

IMPORT SYSTEM;
IMPORT cmd := CodeDef;
IMPORT fc  := CodeFace;
IMPORT pc  := pcK;
IMPORT at  := opAttrs;
IMPORT def := opDef;
IMPORT nms := ObjNames;
IMPORT ir;
IMPORT pr  := opProcs;
IMPORT tun := opTune;
IMPORT str := Strings;
IMPORT dbg := DbgFace;
<* IF TARGET_386 THEN *>
IMPORT D := desc386;
<* END *>
IMPORT cfd := coffDef,
       cfr := coffRoutine;

IMPORT opt := Options;
IMPORT reg := Registry;

TYPE
  INT8  = SYSTEM.INT8;
  INT16 = SYSTEM.INT16;
  INT32 = SYSTEM.INT32;
  CARD16 = SYSTEM.CARD16;
  CARD32 = SYSTEM.CARD32;

  TYPE_INDEX = dbg.TYPE_INDEX;
  OBJ_NAME = ARRAY 256 OF CHAR;

CONST
    NullType = cfd.T_ULONG + 1;
    LongDoubleType = NullType;
    last_primitive = NullType;
    first_nonprimitive = last_primitive + 1;

PROCEDURE register_num (r,size: INT32): INT16;
BEGIN
<* IF TARGET_386 THEN *>
    IF (VAL(D.Reg,r)>=D.EAX) & (VAL(D.Reg,r)<=D.EDI) THEN
    	RETURN SYSTEM.VAL(INT16,r);
    ELSIF (VAL(D.Reg,r) >= D.ST0) & (VAL(D.Reg,r)<= D.ST7) THEN
        RETURN 0CH + SYSTEM.VAL(INT16, r-ORD(D.ST0));
    ELSE
        RETURN -1;
    END;
<* ELSE *>
   RETURN -1;
<* END *>
END register_num;

PROCEDURE generate_fake_name( VAR a : ARRAY OF CHAR );
BEGIN
	str.Assign( "_noname", a ); -- FIXME
END generate_fake_name;

PROCEDURE add_underscope( VAR name : ARRAY OF CHAR );
VAR tmp: OBJ_NAME;
BEGIN
  IF name = '' THEN RETURN END;
  COPY(name, tmp);
  COPY('_', name);
  str.Append(tmp, name);
END add_underscope;

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

TYPE IndexEntry = RECORD
                    btype, navorot: CARD16;
                    baseIdx: INT32;
                    naux: INT8;
                    ndim: INT8;
                    size: CARD16;
                    dimen: ARRAY cfd.E_DIMNUM OF CARD16;
                  END;
CONST IndexEntrySize = 1;

TYPE IndexTableRef = POINTER TO ARRAY OF IndexEntry;

VAR IndexTable *: IndexTableRef;

VAR IndexTableSize  *: INT32;

CONST IndexTable_InitSize = 15342;

------------------------------------------------------------------------------
------------------------------procedures--------------------------------------
------------------------------------------------------------------------------
PROCEDURE InitIndexEntry(VAR ent: IndexEntry; btype: CARD16);
VAR i: INTEGER;
BEGIN
    ent.btype := btype;
    ent.navorot := 0;
    ent.naux := 0;
    ent.baseIdx := 0;
    ent.ndim := 0;
    ent.size := 0;
    FOR i:=0 TO cfd.E_DIMNUM-1 DO
      ent.dimen[i] := 0;
    END;
END InitIndexEntry;

PROCEDURE EnlargeIndexTable;
VAR newsize : INT32;
    newtable : IndexTableRef;
BEGIN
  ASSERT( IndexTable # NIL );
  IF LEN( IndexTable^ ) <= dbg.write_type_cnt THEN
    newsize := LEN( IndexTable^ ) * 2;
    IF newsize <= dbg.type_cnt THEN
      newsize  := dbg.type_cnt + 1;
    END;
    NEW( newtable, newsize );
    SYSTEM.MOVE( SYSTEM.ADR( IndexTable[0] ),
                 SYSTEM.ADR( newtable[0] ),
                 dbg.write_type_cnt * IndexEntrySize );
    IndexTable := newtable;
    newtable := NIL;
  END;
END EnlargeIndexTable;


PROCEDURE AddIndexEntry(e: IndexEntry);
BEGIN
  EnlargeIndexTable();
  IndexTable[dbg.write_type_cnt] := e;
  INC( dbg.write_type_cnt );
END AddIndexEntry;


------------------------------------------------------------------------------

PROCEDURE ApplyNavorot( base_ent: IndexEntry;
                        ttag: dbg.TYPE_TAG;
                        type: pc.STRUCT ): IndexEntry;
VAR modifier : CARD16;
    ent  : IndexEntry;
BEGIN
  ent := base_ent;

  CASE ttag OF
    dbg.ty_pointer  : modifier := cfd.DT_PTR;
  | dbg.ty_array    : modifier := cfd.DT_ARY;
  | dbg.ty_proctype : modifier := cfd.DT_FCN;
  ELSE
    ASSERT(FALSE);
  END;

  IF ent.navorot >= 04000H THEN
    ent.btype   := cfd.T_VOID;
    ent.navorot := ent.navorot MOD 4000H;
  END;
  ent.navorot := ent.navorot * 4 + modifier;

  CASE ttag OF
    dbg.ty_pointer :
      IF ent.ndim # 0 THEN
        IF type # NIL THEN
          ent.size := SYSTEM.VAL( CARD16, pc.code.get_size(pc.su_bytes, type) );
        ELSE
          ent.size := 4;
        END;
      END;
  | dbg.ty_array   :
      ASSERT(type # NIL);
      ent.naux := 1;
      IF ent.ndim < 4 THEN
        ent.dimen[ent.ndim] := SYSTEM.VAL( CARD16, type.len );
        INC(ent.ndim);
      END;
      ent.size := SYSTEM.VAL( CARD16, pc.code.get_size(pc.su_bytes, type) );
  | dbg.ty_proctype:
      IF (type # NIL) & ((type.obj = NIL) OR (type.obj.mode = pc.ob_type)) THEN
        RETURN ApplyNavorot(ent, dbg.ty_pointer, type);
      END;
  END;

  RETURN ent;
END ApplyNavorot;

PROCEDURE WriteAux( ent: IndexEntry );
VAR aux: cfd.AUXENT;
      i: INTEGER;
BEGIN
  IF ent.naux = 0 THEN RETURN END;

  cfr.ClearAUXENT( aux );
  aux.x_sym.x_tagndx := ent.baseIdx;
  aux.x_sym.x_misc.x_lnsz.x_size := ent.size;
  FOR i:=0 TO ent.ndim-1 DO
    aux.x_sym.x_fcnary.x_ary.x_dimen[ent.ndim-1 - i] := ent.dimen[i];
  END;
  cfr.AddAUXENT_pure( aux );
END WriteAux;
------------------------------------------------------------------------------
------------------------------------------------------------------------------


TYPE
  emit_rec = RECORD (dbg.emit_rec)
             END;

  EMIT_GO32  = POINTER TO emit_rec;


------------------------------ Typedefs --------------------------------------

PROCEDURE WriteTypedef(type: pc.STRUCT; ent: IndexEntry);
VAR type_name : OBJ_NAME;
BEGIN
  IF (type # NIL) & (type.obj # NIL) & (type.obj.mode = pc.ob_type) THEN
    dbg.write_type_name( type, type_name );
    add_underscope(type_name);
    cfr.AddSYMENT( type_name, 0, cfr.SEC_DBG_NDX, ent.btype + ent.navorot,
                   cfd.C_TPDEF, ent.naux );
    WriteAux( ent );
  END;

  AddIndexEntry(ent);
END WriteTypedef;

-------------------------------- Record ------------------------------------

PROCEDURE field_filter (f: pc.OBJECT): BOOLEAN;
VAR
  offset: INT32;
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
  dbg.emit_type(dbg.act_set, f.type);
END set_field_type;

PROCEDURE write_field_type (f: pc.OBJECT);
BEGIN
  dbg.emit_type(dbg.act_write, f.type);
END write_field_type;

PROCEDURE write_record_field (f: pc.OBJECT);
VAR field_name: OBJ_NAME;
    tind: dbg.TYPE_INDEX;
BEGIN
  CASE f.mode OF
  | pc.ob_field:
      IF f.name # NIL THEN
        str.Assign( f.name^, field_name );
        add_underscope(field_name);
      ELSE
        generate_fake_name( field_name );
      END;

      tind := dbg.get_index(f.type);
      ASSERT(tind > 0);

      cfr.AddSYMENT ( field_name,
                      def.obj_offset(f),
                      cfd.N_ABS,
                      IndexTable[tind].btype + IndexTable[tind].navorot,
                      cfd.C_MOS,
                      IndexTable[tind].naux
                    );

      WriteAux( IndexTable[tind] );

  | pc.ob_field_bts:
      ASSERT(FALSE);
  END;
END write_record_field;




PROCEDURE WriteRecordType(type: pc.STRUCT);
VAR aux       : cfd.AUXENT;
    type_name : OBJ_NAME;
    ent       : IndexEntry;
BEGIN
  dbg.field_list (type.prof, field_filter, write_field_type);

  InitIndexEntry( ent, cfd.T_STRUCT );
  ent.naux := 1;
  ent.baseIdx := cfr.FileHeader.f_nsyms;
  ent.size := SYSTEM.VAL( CARD16, pc.code.get_size( pc.su_bytes, type ) );

  dbg.write_type_name( type, type_name );
  add_underscope(type_name);
  cfr.AddSYMENT( type_name, 0, cfr.SEC_DBG_NDX, cfd.T_STRUCT, cfd.C_STRTAG, 1 );
  cfr.ClearAUXENT( aux );
  aux.x_sym.x_misc.x_lnsz.x_size := ent.size;
--  aux.x_sym.x_fcnary.x_fcn.x_endndx := NextSymbolAfterRecord( type );
-- compute it after writing ".eos"
  cfr.AddAUXENT_pure( aux );

  dbg.field_list (type.prof, field_filter, write_record_field);

  cfr.AddSYMENT( ".eos", ent.size, cfd.N_ABS, cfd.T_NULL, cfd.C_EOS, 1 );
  aux.x_sym.x_tagndx := ent.baseIdx;
  cfr.AddAUXENT_pure( aux ); -- we use the same aux as in T_STRUCT

-- write NextSymbolAfterRecord( type ) to record's aux entry
  cfr.SymbolTable[ent.baseIdx + 1].aux.x_sym.x_fcnary.x_fcn.x_endndx :=
  		cfr.FileHeader.f_nsyms;

  AddIndexEntry(ent);

  cfr.AddSYMENT( type_name, 0, cfr.SEC_DBG_NDX, cfd.T_STRUCT, cfd.C_TPDEF, 1 );
  cfr.AddAUXENT_pure( aux ); -- we use the same aux as in ".eos"
END WriteRecordType;

-------------------------------- Enum ----------------------------------------

PROCEDURE WriteEnumType(type: pc.STRUCT);
VAR aux       : cfd.AUXENT;
    type_name : OBJ_NAME;
    ent       : IndexEntry;
    const     : pc.OBJECT;
    const_name: OBJ_NAME;
    const_val : LONGINT;
BEGIN
  InitIndexEntry( ent, cfd.T_ENUM );
  ent.naux := 1;
  ent.baseIdx := cfr.FileHeader.f_nsyms;
  ent.size := SYSTEM.VAL( CARD16, pc.code.get_size( pc.su_bytes, type ) );

  dbg.write_type_name( type, type_name );
  add_underscope(type_name);
  cfr.AddSYMENT( type_name, 0, cfr.SEC_DBG_NDX, cfd.T_ENUM, cfd.C_ENTAG, 1 );
  cfr.ClearAUXENT( aux );
  aux.x_sym.x_misc.x_lnsz.x_size := ent.size;
--  aux.x_sym.x_fcnary.x_fcn.x_endndx := NextSymbolAfterEnum( type );
-- compute it after writing ".eos"
  cfr.AddAUXENT_pure( aux );

  const := type.prof;
  REPEAT
    ASSERT((const.mode = pc.ob_cons) & (const.val.mode = pc.nd_value));
    const_val := const.val.val.get_integer();
    dbg.write_obj_name(const, const_name);
    add_underscope(const_name);
    cfr.AddSYMENT( const_name, const_val, cfd.N_ABS, cfd.T_MOE, cfd.C_MOE, 0 );
    const := const.next;
  UNTIL const = NIL;

  cfr.AddSYMENT( ".eos", ent.size, cfd.N_ABS, cfd.T_NULL, cfd.C_EOS, 1 );
  aux.x_sym.x_tagndx := ent.baseIdx;
  cfr.AddAUXENT_pure( aux ); -- we use the same aux as in T_STRUCT

-- write NextSymbolAfterEnum( type ) to record's aux entry
  cfr.SymbolTable[ent.baseIdx + 1].aux.x_sym.x_fcnary.x_fcn.x_endndx :=
  		cfr.FileHeader.f_nsyms;

  AddIndexEntry(ent);

  cfr.AddSYMENT( type_name, 0, cfr.SEC_DBG_NDX, cfd.T_STRUCT, cfd.C_TPDEF, 1 );
  cfr.AddAUXENT_pure( aux ); -- we use the same aux as in ".eos"
END WriteEnumType;

------------------------------------------------------------------------------

PROCEDURE (emit: EMIT_GO32) TypeEmitter (ttag: dbg.TYPE_TAG; act: dbg.ACTION; type: pc.STRUCT);
VAR inx: dbg.TYPE_INDEX;
BEGIN
  CASE ttag OF
  | dbg.ty_start:
    ASSERT((act = dbg.act_write) AND (type = NIL));
    dbg.type_cnt       := first_nonprimitive;
    dbg.write_type_cnt := first_nonprimitive;

  | dbg.ty_end:
    ASSERT((act = dbg.act_write) AND (type = NIL));
    dbg.type_cnt       := MAX(dbg.TYPE_INDEX);
    dbg.write_type_cnt := MAX(dbg.TYPE_INDEX);

  | dbg.ty_module:
      emit.TypeEmitter(dbg.ty_proctype, act, type);
  | dbg.ty_set:
    CASE act OF
    | dbg.act_set:
      dbg.put_index_val( type, NullType );
    | dbg.act_write:
    END;

  | dbg.ty_array_of:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      IF NOT (at.tmark_db_index IN type.marks) THEN
        IF type.base.mode # pc.ty_array_of THEN
          dbg.put_index( type );
        ELSE
          dbg.put_index_val( type, dbg.get_index(type.base) );
        END;
      END;
    | dbg.act_write:
      IF type.base.mode # pc.ty_array_of THEN
        inx := dbg.get_index(type);
        IF inx = dbg.write_type_cnt THEN
          WriteTypedef( type, ApplyNavorot( IndexTable[dbg.get_index(type.base)],
                                            dbg.ty_pointer, type ) );
        ELSE
          ASSERT(inx < dbg.write_type_cnt);
        END;
      END;
    END;

  | dbg.ty_reference:
    ASSERT((act = dbg.act_write) AND (type = NIL));
    AddIndexEntry( ApplyNavorot( IndexTable[dbg.tindex_transfer],
                                 dbg.ty_pointer, NIL ) );

  | dbg.ty_open_array:
    ASSERT(act = dbg.act_write);
    AddIndexEntry( IndexTable[dbg.tindex_transfer] );

  ------------------------- typedefs -------------------------------------
  | dbg.ty_opaque,
    dbg.ty_range:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      dbg.put_index_val(type, dbg.get_index(type.base));
    | dbg.act_write:
    END;

  | dbg.ty_pointer, dbg.ty_proctype:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);
    | dbg.act_write:
      WriteTypedef( type, ApplyNavorot( IndexTable[dbg.get_index(type.base)],
                    ttag, type ) );
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
        WriteTypedef( type, ApplyNavorot( IndexTable[dbg.get_index(type.base)],
                                          dbg.ty_array, type ) );
      ELSE
        ASSERT(inx < dbg.write_type_cnt);
      END;
    END;

  ------------------------- complex types ------------------------------------
  | dbg.ty_enum:
    dbg.emit_type(act, type.base);
    CASE act OF
    | dbg.act_set:
      dbg.put_index(type);

    | dbg.act_write:
      WriteEnumType(type);
    END;

  | dbg.ty_record:
    IF type.base # NIL THEN
      dbg.emit_type(act, type.base);
    END;
    CASE act OF
    | dbg.act_set:
      IF NOT (at.tmark_db_index IN type.marks) THEN
       	dbg.field_list (type.prof, field_filter, set_field_type);
        dbg.put_index(type);        
      END;
    | dbg.act_write:
      inx := dbg.get_index(type);
      IF inx = dbg.write_type_cnt THEN
        WriteRecordType(type);
      ELSE
        ASSERT(inx < dbg.write_type_cnt);
      END;
    END;

  END;
END TypeEmitter;

-----------------------------------------------------------------------------
---------------------------- Symbols ----------------------------------------
-----------------------------------------------------------------------------

--------------------------- Procedure ---------------------------------------

PROCEDURE WriteProcedure(o: pc.OBJECT; tind: TYPE_INDEX);
VAR proc_name: OBJ_NAME;
BEGIN
  fc.ObjectName(o, proc_name); -- why fc. ?
  cfr.Add_procedure_entry( o, proc_name,
                           IndexTable[tind].btype + IndexTable[tind].navorot,
                           IndexTable[tind].baseIdx );
END WriteProcedure;

------------------------------ Variables -----------------------------------

PROCEDURE WriteVariable( o: pc.OBJECT; tind: TYPE_INDEX );
VAR var_name: OBJ_NAME;
BEGIN
  fc.ObjectName(o, var_name);
  cfr.Add_object_entry( o, var_name,
                        IndexTable[tind].btype + IndexTable[tind].navorot,
                        IndexTable[tind].naux );
  WriteAux( IndexTable[tind] );
END WriteVariable;

----------------------------- Locals ---------------------------------------

PROCEDURE WriteLocalOrParam( o: pc.OBJECT; tind: TYPE_INDEX; is_par: BOOLEAN );
VAR
  a   : at.ATTR_EXT;
  size: LONGINT;

  is_reg : BOOLEAN;
  sclass : INT8;

  value : INT32;
  obj_name: OBJ_NAME;
BEGIN
  a := at.attr(o.ext, at.a_dbg);
  ASSERT(a # NIL);
  WITH a: at.DBG_EXT DO
    is_reg := a.list[0].info.Location = ir.LocInReg;
    value  := a.list[0].info.Value;
  ELSE
    ASSERT(FALSE);
  END;

  IF is_par THEN	-- function parameter
    sclass := cfd.C_ARG;
  ELSE			-- local variable
    IF is_reg THEN
      sclass := cfd.C_REG;
    ELSE
      sclass := cfd.C_AUTO;
    END;
  END;

  IF is_reg THEN
    -- calculate size
    IF (o.mode = pc.ob_varpar)
        OR ((pc.otag_RO IN o.tags) & ~def.is_scalar(o.type))
        OR (o.type.mode = pc.ty_array_of)
    THEN
      -- we have to make a reference to that type
      size := 4;
    ELSE
      size := pc.code.get_size(pc.su_size, o.type);
    END;
    value := register_num(value, size);
  END;

  dbg.write_obj_name(o, obj_name);
  cfr.AddSYMENT( obj_name,
                 value,
                 cfd.N_ABS,
                 IndexTable[tind].btype + IndexTable[tind].navorot,
                 sclass,
                 IndexTable[tind].naux );

  WriteAux( IndexTable[tind] );
END WriteLocalOrParam;

------------------------------------------------------------------------------

PROCEDURE (emit: EMIT_GO32) SymbEmitter (stag: dbg.SYMB_TAG; o: pc.OBJECT; tind: dbg.TYPE_INDEX);
BEGIN
  CASE stag OF
    dbg.sy_start,
    dbg.sy_end:

  | dbg.sy_var:         WriteVariable(o, tind);

  | dbg.sy_local_var,
    dbg.sy_param:       WriteLocalOrParam(o, tind, stag = dbg.sy_param);

  | dbg.sy_proc:        WriteProcedure(o, tind);

  | dbg.sy_scope_open,
    dbg.sy_scope_close: ASSERT(tind = -1);
                        cfr.Add_scope_entry( o, stag = dbg.sy_scope_open );
  END;
END SymbEmitter;



PROCEDURE (emit: EMIT_GO32) PrimitiveTypeNo (type: pc.STRUCT): dbg.TYPE_INDEX;
VAR sz : INT32;
BEGIN
  CASE type.mode OF
  | pc.ty_shortcard : RETURN cfd.T_UCHAR;
  | pc.ty_cardinal  : RETURN cfd.T_USHORT;
  | pc.ty_longcard  : RETURN cfd.T_ULONG;
  | pc.ty_shortint  : RETURN cfd.T_CHAR;
  | pc.ty_integer   : RETURN cfd.T_SHORT;
  | pc.ty_longint   : RETURN cfd.T_LONG;
  | pc.ty_real      : RETURN cfd.T_FLOAT;
  | pc.ty_longreal  : RETURN cfd.T_DOUBLE;
  | pc.ty_ld_real   : RETURN LongDoubleType;
  | pc.ty_boolean   : sz := def.type_size(type.base);
                      IF    sz = 1 THEN    RETURN cfd.T_UCHAR;
                      ELSIF sz = 2 THEN    RETURN cfd.T_USHORT;
                      ELSIF sz = 4 THEN    RETURN cfd.T_ULONG;
                      ELSE ASSERT(sz = 8); RETURN NullType; -- FIXME
                      END;

  | pc.ty_char      : sz := def.type_size(type.base);
                      IF    sz = 1 THEN    RETURN cfd.T_UCHAR;
		      ELSIF sz = 2 THEN    RETURN cfd.T_USHORT;
                      ELSE ASSERT(sz = 4); RETURN cfd.T_ULONG;
                      END;

  | pc.ty_loc       : RETURN cfd.T_UCHAR;

  | pc.ty_protection: RETURN cfd.T_USHORT;

  | pc.ty_void      : RETURN cfd.T_VOID;
  ELSE
	RETURN -1;
  END;
END PrimitiveTypeNo;

PROCEDURE (emit: EMIT_GO32) generate (): BOOLEAN;
VAR i: INTEGER;
BEGIN
  NEW( IndexTable, IndexTable_InitSize );
  FOR i:= 0 TO last_primitive-1 DO
    InitIndexEntry( IndexTable[i], i );
  END;
  InitIndexEntry( IndexTable[last_primitive], cfd.T_NULL );
  RETURN emit.generate^ ();
END generate;
------------------------------------------------------------------------------


VAR
  emitGO32: EMIT_GO32;

BEGIN
  NEW(emitGO32);
  reg.Register(opt.dbgFormat, opt.dbg_GO32, emitGO32);
END dbgGO32.
