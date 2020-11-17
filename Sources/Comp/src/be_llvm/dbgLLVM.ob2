--------------------------------------------------------------------------------
--                          Excelsior LLVM Compiler
--                         (c) 2020, Excelsior, LLC.
--
-- Module:   dbgLLVM
-- Mission:  LLVM debug information for LLVM assembler 
-- Authors:  Lvov Konstantin
-- Created:  07-Oct-2020
--------------------------------------------------------------------------------
MODULE dbgLLVM;

IMPORT sys := SYSTEM;

IMPORT DStrings;
IMPORT FileSys;

IMPORT  pc := pcK;
IMPORT xfs := xiFiles;
IMPORT env := xiEnv;
IMPORT fmt := FormStr;
IMPORT pcO;

IMPORT reg := Registry;

IMPORT ir;

IMPORT   fc := CodeFace;
IMPORT   at := opAttrs;
IMPORT  cmd := CodeDef;
IMPORT  dbg := DbgFace;
IMPORT  opt := Options;
IMPORT  def := opDef;
IMPORT  prc := opProcs;

IMPORT nms := llvmNames;

--------------------------------------------------------------------------------
TYPE
  MetaDataIndex *= LONGINT;  -- Index of MetaData Node

--------------------------------------------------------------------------------
CONST
  -- Fixed Indexes of MetaData Nodes
  MDI_Undefined        *= MetaDataIndex {-1};
  MDI_DICompileUnit    *= MetaDataIndex {0};
  MDI_DIFile           *= MetaDataIndex {1};
  MDI_enums             = MetaDataIndex {2};
  MDI_globals           = MetaDataIndex {3};
  MDI_ident            *= MetaDataIndex {4};
  MDI_DwarfVersion     *= MetaDataIndex {5};
  MDI_DebugInfoVersion *= MetaDataIndex {6};
  MDI_EmptyNode        *= MetaDataIndex {7};

  LAST_FIXED_MD_INDEX = MDI_EmptyNode; 

--------------------------------------------------------------------------------
CONST
  -- DWARF language encodings.
  DW_LANG_Modula2 = "DW_LANG_Modula2";  -- 0x000a: ISO Modula-2:1996 
  
  -- DWARF base type encodings.
  DW_ATE_address       = "DW_ATE_address";        -- 01H
  DW_ATE_boolean       = "DW_ATE_boolean";        -- 02H
  DW_ATE_complex_float = "DW_ATE_complex_float";  -- 03H
  DW_ATE_float         = "DW_ATE_float";          -- 04H
  DW_ATE_signed        = "DW_ATE_signed";         -- 05H
  DW_ATE_signed_char   = "DW_ATE_signed_char";    -- 06H
  DW_ATE_unsigned      = "DW_ATE_unsigned";       -- 07H
  DW_ATE_unsigned_char = "DW_ATE_unsigned_char";  -- 08H
  
  -- DWARF tags
  DW_TAG_typedef  = "DW_TAG_typedef";  -- 16H;
  
  -- LLVM Debug Emission Kind
  EMKIND_NoDebug        = "NoDebug";
  EMKIND_FullDebug      = "FullDebug";
  EMKIND_LineTablesOnly = "LineTablesOnly"; 

  FIRST_PTR_PRIMITIVE_TYPE_INDEX = dbg.TYPE_INDEX{ (ORD(MAX(pc.TY_MODE))+1) };
  FIRST_NON_PRIMITIVE_TYPE_INDEX = FIRST_PTR_PRIMITIVE_TYPE_INDEX * 2;

  TYPE_INDEX_Invalid = dbg.TYPE_INDEX{ -1 };

--------------------------------------------------------------------------------
TYPE
  String *= DStrings.String;

  MetadataTypeIndexTableT = POINTER TO ARRAY OF MetaDataIndex;
  
--------------------------------------------------------------------------------
VAR
  Metadata: cmd.CODE_SEGM;
  CurrModule: pc.OBJECT;
  EnumsCount: LONGINT;
  GlobalsCount: LONGINT;
  
--------------------------------------------------------------------------------
VAR
  MetadataTypeIndexTable: MetadataTypeIndexTableT; 
  
--------------------------------------------------------------------------------
PROCEDURE GetMetadata *(): cmd.CODE_SEGM; 
BEGIN 
  RETURN Metadata; 
END GetMetadata; 

--------------------------------------------------------------------------------
PROCEDURE GetNextMetadataIndex (): MetaDataIndex; 
BEGIN 
  RETURN Metadata.code_len; 
END GetNextMetadataIndex; 


--------------------------------------------------------------------------------
PROCEDURE AllocateMetadataTypeIndexTable ();
CONST INIT_TABLE_SIZE = FIRST_NON_PRIMITIVE_TYPE_INDEX * 2;
VAR i: LONGINT; 
BEGIN
  NEW(MetadataTypeIndexTable, INIT_TABLE_SIZE);
  FOR i := 0 TO INIT_TABLE_SIZE - 1 DO  
    MetadataTypeIndexTable[i] := MDI_Undefined;  
  END;
END AllocateMetadataTypeIndexTable;

--------------------------------------------------------------------------------
PROCEDURE SetMetadataTypeIndex (inx: dbg.TYPE_INDEX; md_inx: MetaDataIndex; override:=FALSE: BOOLEAN); 
VAR i: LONGINT;
    size, new_size: LONGINT;
    new_table: MetadataTypeIndexTableT;
BEGIN 
  ASSERT( inx >= 0);
  IF MetadataTypeIndexTable = NIL THEN
    size := 0;  
    new_size := FIRST_NON_PRIMITIVE_TYPE_INDEX * 2;
  ELSE  
    size := LEN(MetadataTypeIndexTable^);  
    new_size := size * 2;
  END;
  IF size <= inx THEN
    ASSERT( inx < new_size );
    NEW(new_table, new_size);
    FOR i := 0 TO size-1 DO  new_table[i] := MetadataTypeIndexTable[i];  END;
    FOR i := size TO new_size-1 DO  new_table[i] := MDI_Undefined;  END;
    MetadataTypeIndexTable := new_table;
  END;
  ASSERT( override OR (MetadataTypeIndexTable[inx] = MDI_Undefined) );
  MetadataTypeIndexTable[inx] := md_inx;
END SetMetadataTypeIndex; 

--------------------------------------------------------------------------------
PROCEDURE SetMetadatPrimitiveTypeIndex (type: pc.TY_MODE; md_inx: MetaDataIndex; ptr: BOOLEAN);
VAR inx: dbg.TYPE_INDEX;
BEGIN
  inx := ORD(type);
  IF ptr THEN
    INC(inx, FIRST_PTR_PRIMITIVE_TYPE_INDEX);
  END;
  SetMetadataTypeIndex(inx, md_inx);
END SetMetadatPrimitiveTypeIndex;  


--------------------------------------------------------------------------------
PROCEDURE GetLine (pos-: env.TPOS): LONGINT;
VAR fname: String;
    line, col: LONGINT;
BEGIN
  IF pos.IsNull() THEN
    line := -1; -- invalid line
  ELSE
    pos.unpack(fname, line, col);
    INC(line)
  END;
  RETURN line;
END GetLine;

--------------------------------------------------------------------------------
PROCEDURE GetFileName (): String;
VAR fname, fext: String;
BEGIN
  xfs.sys.GetName(env.info.file^, fname);
  xfs.sys.GetExt(env.info.file^, fext);
  DStrings.Append(".", fname);
  DStrings.Append(fext^, fname);
  RETURN fname;
END GetFileName;

--------------------------------------------------------------------------------
PROCEDURE GetCompDir (): String;
VAR dname: String;
    full_name: ARRAY 1024 OF CHAR;
BEGIN
  xfs.sys.GetDir(env.info.file^, dname);
  IF dname^ = "" THEN
    DStrings.Append(".", dname);
  ELSE
    xfs.sys.ConvertToHost (dname^, dname);
  END;
  IF env.config.Option ("DBGFULLPATHS") THEN 
    -- секретная опция для выдачи в отладчике полных путей в 'DW_AT_comp_dir'
    FileSys.FullName(full_name, dname^);
    DStrings.Assign(full_name, dname);
  END;
  RETURN dname;
END GetCompDir;

--------------------------------------------------------------------------------
PROCEDURE GetProducer (): String; 
VAR str: ARRAY 128 OF CHAR;
    producer: String;
BEGIN 
  fmt.print(str, "XDS %s [%s]", pc.code.vers, pc.pars.vers);
  DStrings.Assign(str, producer);
  RETURN producer;
END GetProducer; 


--------------------------------------------------------------------------------
PROCEDURE GetObjName (obj: pc.OBJECT; VAR name: ARRAY OF CHAR);
BEGIN
  dbg.write_obj_name(obj, name);
END GetObjName;

--------------------------------------------------------------------------------
PROCEDURE GetTypeName (type: pc.STRUCT; VAR name: ARRAY OF CHAR);
BEGIN
  dbg.write_type_name(type, name);
END GetTypeName;

--------------------------------------------------------------------------------
PROCEDURE GetObjCodeOffs (obj: pc.OBJECT): LONGINT; 
VAR attr: at.ATTR_EXT;
    offs: MetaDataIndex;
BEGIN
  attr := at.attr(obj.ext, at.a_codeOFFS);
  IF attr = NIL THEN  offs := -1;
  ELSE                offs := attr(at.OFFS_EXT).offset;
  END;
  RETURN offs;
END GetObjCodeOffs; 

--------------------------------------------------------------------------------
PROCEDURE GetParamNumber (obj: pc.OBJECT): LONGINT; 
VAR attr: at.ATTR_EXT;
    param_no: LONGINT;
BEGIN 
  attr := at.attr(obj.ext, at.a_param_no);
  IF attr = NIL THEN  param_no := -1;
  ELSE                param_no := attr(at.SIZE_EXT).size + 1;
  END;
  RETURN param_no;
END GetParamNumber; 

--------------------------------------------------------------------------------
PROCEDURE AppendInstr (inx: MetaDataIndex; format-: ARRAY OF CHAR; SEQ args: sys.BYTE);
VAR str: ARRAY 1024 OF CHAR;
BEGIN
  ASSERT( (inx # MDI_Undefined) & (inx < Metadata.code_len) );
  fmt.print(str, format, args);
  DStrings.Append(str, Metadata.acode[inx]);
END AppendInstr;

--------------------------------------------------------------------------------
PROCEDURE PutInstr (inx: MetaDataIndex; format-: ARRAY OF CHAR; SEQ args: sys.BYTE);
VAR str: ARRAY 1024 OF CHAR;
BEGIN
  ASSERT( (inx # MDI_Undefined) & (inx < Metadata.code_len) );
  fmt.print(str, format, args);
  DStrings.Assign(str, Metadata.acode[inx]);
END PutInstr;

--------------------------------------------------------------------------------
PROCEDURE GenInstr *(format-: ARRAY OF CHAR; SEQ args: sys.BYTE);
VAR str: ARRAY 1024 OF CHAR;
BEGIN
  fmt.print(str, format, args);
  cmd.GenInstr(str, TRUE);   
END GenInstr;

--------------------------------------------------------------------------------
PROCEDURE GenComment (format-: ARRAY OF CHAR; SEQ args: sys.BYTE); 
VAR inx: MetaDataIndex;
BEGIN
  -- 'Metadata.code_len' is used as current index in metadata.
  -- So, the comment is appended to the previous item with new line prefix.
  inx := GetNextMetadataIndex();
  ASSERT( inx > 0 );
  DEC(inx);
  AppendInstr(inx, "\n;");
  AppendInstr(inx, format, args);
END GenComment;  

--------------------------------------------------------------------------------
CONST
  -- The first two parameters are replaced outside of 'dbgLLVM' in LLVM instruction emitter, 
  -- the third parameter is replaced in 'UpdateDbgCallInstr'  
  FORMAT_LocalVarAddr *= 'call void @llvm.dbg.addr(metadata %s %%%s, metadata !%%d, metadata !DIExpression())';
  
PROCEDURE UpdateDbgCallInstr (obj: pc.OBJECT; obj_inx: MetaDataIndex); 
VAR offs: LONGINT;
    sg: cmd.CODE_SEGM; 
    str: ARRAY 1024 OF CHAR;
BEGIN 
  ASSERT( obj.mode IN pc.VARs );
  offs := GetObjCodeOffs(obj); 
  sg := cmd.get_ready(obj.host.obj);
  fmt.print(str, sg.acode[offs]^, obj_inx);
  DStrings.Assign(str, sg.acode[offs]);
END UpdateDbgCallInstr;  


--------------------------------------------------------------------------------
PROCEDURE GetObjIndex *(obj: pc.OBJECT): MetaDataIndex;
VAR attr: at.ATTR_EXT;
    index: MetaDataIndex;
BEGIN
  attr := at.attr(obj.ext, at.a_index);
  IF attr = NIL THEN  index := MDI_Undefined;
  ELSE                index := attr(at.SIZE_EXT).size;
  END;
  RETURN index;
END GetObjIndex;

--------------------------------------------------------------------------------
PROCEDURE SetObjIndex (obj: pc.OBJECT; index: MetaDataIndex);
VAR attr: at.SIZE_EXT;
BEGIN
  ASSERT( GetObjIndex(obj) = MDI_Undefined );
  NEW(attr);
  attr.size := index;
  at.app_obj_attr(obj, attr, at.a_index);
END SetObjIndex;


--------------------------------------------------------------------------------
PROCEDURE SetTypeIndex (type: pc.STRUCT; inx: dbg.TYPE_INDEX); 
BEGIN 
  dbg.put_index_val(type, inx); 
END SetTypeIndex; 

--------------------------------------------------------------------------------
PROCEDURE GetTypeIndex (type: pc.STRUCT): dbg.TYPE_INDEX; 
BEGIN 
  RETURN dbg.get_index(type); 
END GetTypeIndex; 

--------------------------------------------------------------------------------
PROCEDURE GetTypeMetadataIndex (type: pc.STRUCT): MetaDataIndex; 
VAR inx: dbg.TYPE_INDEX;
    md_inx: MetaDataIndex;
BEGIN 
  inx := GetTypeIndex(type);
  IF inx = TYPE_INDEX_Invalid THEN  md_inx := MDI_Undefined;
  ELSE                              md_inx := MetadataTypeIndexTable[inx];
  END;
  RETURN md_inx; 
END GetTypeMetadataIndex; 

--------------------------------------------------------------------------------
PROCEDURE LockNextTypeIndex (str-: ARRAY OF CHAR); 
VAR inx: MetaDataIndex;
BEGIN
  inx := GetNextMetadataIndex(); 
  ASSERT( inx = dbg.type_cnt );
  INC(dbg.type_cnt);
  GenInstr('!%d type %s', inx, str);
END LockNextTypeIndex;
 
--------------------------------------------------------------------------------
PROCEDURE PutIndex (type: pc.STRUCT);
VAR inx: dbg.TYPE_INDEX;
    md_inx: MetaDataIndex;
    name: ARRAY 128 OF CHAR;
BEGIN
  dbg.put_index(type);
  inx := GetTypeIndex(type);
  md_inx := GetNextMetadataIndex();
  GetTypeName(type, name);
  GenInstr('!%d type %s', inx, name);
  SetMetadataTypeIndex(inx, md_inx);
END PutIndex;

--------------------------------------------------------------------------------
PROCEDURE EndType ();
BEGIN
  INC(dbg.write_type_cnt);
END EndType;


--------------------------------------------------------------------------------
PROCEDURE EmitDICompileUnit (); 
VAR emmission_kind: ARRAY 32 OF CHAR;
    producer: String;
    inx: MetaDataIndex;
BEGIN 
  inx := GetNextMetadataIndex();
  ASSERT( inx = MDI_DICompileUnit );
  producer := GetProducer();
  IF fc.GenDebug()     THEN  emmission_kind := EMKIND_FullDebug;
  ELSIF fc.GenLineno() THEN  emmission_kind := EMKIND_LineTablesOnly;
  ELSE                       emmission_kind := EMKIND_NoDebug;
  END;      
  GenInstr( '!%d = distinct !DICompileUnit(language: %s' +
            ', file: !%d, producer: "%s"' + 
            ', isOptimized: false, runtimeVersion: 0' +
            ', emissionKind: %s' +
            ', enums: !%d, globals: !%d, nameTableKind: None)'
          , inx, DW_LANG_Modula2
          , MDI_DIFile, producer^
          , emmission_kind
          , MDI_enums, MDI_globals );
END EmitDICompileUnit; 

--------------------------------------------------------------------------------
PROCEDURE EmitDIFile (); 
VAR fname, curpath: String;
    inx: MetaDataIndex;
BEGIN 
  inx := GetNextMetadataIndex();
  ASSERT( inx = MDI_DIFile );
  fname   := GetFileName();
  curpath := GetCompDir();  
  GenInstr( '!%d = !DIFile(filename: "%s", directory: "%s")'
          , inx, fname^, curpath^);
END EmitDIFile; 

--------------------------------------------------------------------------------
PROCEDURE EmitEnumsPrefix (); 
VAR inx: MetaDataIndex;
BEGIN 
  inx := GetNextMetadataIndex();  
  ASSERT( inx = MDI_enums );
  GenInstr("!%d = !{", inx);
END EmitEnumsPrefix; 

--------------------------------------------------------------------------------
PROCEDURE EmitGlobalsPrefix (); 
VAR inx: MetaDataIndex;
BEGIN 
  inx := GetNextMetadataIndex();
  ASSERT( inx = MDI_globals );
  GenInstr("!%d = !{", inx);
END EmitGlobalsPrefix; 

--------------------------------------------------------------------------------
PROCEDURE EmitIdent (); 
VAR producer: String;
    inx: MetaDataIndex;
BEGIN 
  inx := GetNextMetadataIndex();
  ASSERT( inx = MDI_ident );
  producer := GetProducer();
  GenInstr('!%d = !{!"%s"}', inx, producer^);
END EmitIdent; 

--------------------------------------------------------------------------------
PROCEDURE EmitDwarfVersion (); 
VAR inx: MetaDataIndex;
BEGIN 
  inx := GetNextMetadataIndex();
  ASSERT( inx = MDI_DwarfVersion );
  GenInstr('!%d = !{i32 2, !"Dwarf Version", i32 2}', inx);
END EmitDwarfVersion; 

--------------------------------------------------------------------------------
PROCEDURE EmitDebugInfoVersion (); 
VAR inx: MetaDataIndex;
BEGIN 
  inx := GetNextMetadataIndex();
  ASSERT( inx = MDI_DebugInfoVersion );
  GenInstr('!%d = !{i32 2, !"Debug Info Version", i32 3}', inx);
END EmitDebugInfoVersion; 

--------------------------------------------------------------------------------
PROCEDURE EmitEmptyNode (); 
VAR inx: MetaDataIndex;
BEGIN 
  inx := GetNextMetadataIndex();
  ASSERT( inx = MDI_EmptyNode );
  GenInstr('!%d = !{}', inx);
END EmitEmptyNode; 

--------------------------------------------------------------------------------
PROCEDURE EmitPredefinedMetadataNodes (); 
BEGIN
  ASSERT( Metadata # NIL );
  EmitDICompileUnit();     -- !0 
  EmitDIFile();            -- !1
  EmitEnumsPrefix();       -- !2
  EmitGlobalsPrefix();     -- !3
  EmitIdent();             -- !4
  EmitDwarfVersion();      -- !5
  EmitDebugInfoVersion();  -- !6
  EmitEmptyNode();         -- !7
END EmitPredefinedMetadataNodes; 

--------------------------------------------------------------------------------
PROCEDURE ClosePredefinedMetadataNodes (); 
BEGIN 
  AppendInstr(MDI_enums, "}  ; enums");
  AppendInstr(MDI_globals, "}  ; globals");
END ClosePredefinedMetadataNodes; 


--------------------------------------------------------------------------------
PROCEDURE GetAddrSize (): LONGINT; 
BEGIN 
  RETURN pc.code.get_size(pc.su_bits, pcO.addr);
END GetAddrSize; 

--------------------------------------------------------------------------------
PROCEDURE EmitTypedef (type: pc.STRUCT; type_inx, base_inx: MetaDataIndex): MetaDataIndex;
CONST FORMAT_DIDerivedType = '!%d = !DIDerivedType(tag: DW_TAG_typedef' 
                           + ', name: "%s", file: !%d, line: %d, baseType: !%d)'; 
VAR name: ARRAY 128 OF CHAR;
BEGIN
  GetTypeName(type, name);
  IF type_inx # MDI_Undefined THEN
    PutInstr( type_inx, FORMAT_DIDerivedType
            , type_inx, name, MDI_DIFile, GetLine(type.pos), base_inx);
  ELSE  
    type_inx := GetNextMetadataIndex();
    GenInstr( FORMAT_DIDerivedType
            , type_inx, name, MDI_DIFile, GetLine(type.pos), base_inx);
  END;  
  RETURN type_inx;
END EmitTypedef;  

--------------------------------------------------------------------------------
PROCEDURE EmitEnumerationType (type: pc.STRUCT): MetaDataIndex; 
VAR inx, base_inx, elements_inx: MetaDataIndex;
    item: pc.OBJECT;
    name: ARRAY 128 OF CHAR;
    elements: String;
    val, size: LONGINT;
BEGIN
  -- Enumeration elements
  elements := NIL; 
  item := type.prof;
  REPEAT
    ASSERT((item.mode = pc.ob_cons) & (item.val.mode = pc.nd_value));
    GetObjName(item, name);
    inx := GetNextMetadataIndex();
    val := item.val.val.get_integer();
    GenInstr( '!%d = !DIEnumerator(name: "%s", value: %d, isUnsigned: true)'
            , inx, name, val );
    fmt.print(name, '!%d', inx);
    IF elements = NIL THEN
      DStrings.Assign(name, elements);        
    ELSE
      DStrings.Append(", ", elements);        
      DStrings.Append(name, elements);        
    END;            
    item := item.next;
  UNTIL item = NIL;

  elements_inx := GetNextMetadataIndex();
  GenInstr('!%d = !{%s}', elements_inx, elements^);
  
  -- DW_TAG_enumeration_type
  inx := GetTypeMetadataIndex(type);
  base_inx := GetTypeMetadataIndex(type.base);
  size := pc.code.get_size(pc.su_bits, type);
  IF (type.obj # NIL) THEN
    GetTypeName(type, name);
    PutInstr( inx, '!%d = !DICompositeType(tag: DW_TAG_enumeration_type, name: "%s"'
                   + ', file: !%d, line: %d, baseType: !%d, size: %d, elements: !%d)'
            , inx, name, MDI_DIFile, GetLine(type.pos), base_inx, size, elements_inx );
  ELSE 
    PutInstr( inx, '!%d = !DICompositeType(tag: DW_TAG_enumeration_type'
            + ', file: !%d, line: %d, baseType: !%d, size: %d, elements: !%d)'
            , inx, MDI_DIFile, GetLine(type.pos), base_inx, size, elements_inx );
  END;
  RETURN inx; 
END EmitEnumerationType; 

--------------------------------------------------------------------------------
-- Reference to the type whose index is currently in 'DbgFace.tindex_transfer' 
PROCEDURE EmitReferenceType (); 
VAR inx, base_inx: MetaDataIndex;
    size: LONGINT;
BEGIN
  inx := GetNextMetadataIndex();
  base_inx := MetadataTypeIndexTable[dbg.tindex_transfer];
  size := GetAddrSize();
  GenInstr( '!%d = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !%d, size: %d)'
          , inx, base_inx, size );
  SetMetadataTypeIndex(dbg.write_type_cnt, inx);
END EmitReferenceType; 

--------------------------------------------------------------------------------
PROCEDURE EmitPointerType (type: pc.STRUCT); 
VAR inx, base_inx: MetaDataIndex;
    size: LONGINT;
    name: ARRAY 128 OF CHAR;
BEGIN
  size := pc.code.get_size(pc.su_bits, type);
  base_inx := GetTypeMetadataIndex(type.base);
  IF (type.obj = NIL) THEN
    inx := GetTypeMetadataIndex(type);
    PutInstr( inx, '!%d = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !%d, size: %d)'
            , inx, base_inx, size );
  ELSE 
    inx := GetNextMetadataIndex();
    GenInstr( '!%d = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !%d, size: %d)'
            , inx, base_inx, size );
    base_inx := inx;    
    inx := GetTypeMetadataIndex(type);
    GetTypeName(type, name);
    sys.EVAL( EmitTypedef(type, inx, base_inx) );
  END;
END EmitPointerType; 

--------------------------------------------------------------------------------
PROCEDURE EmitArrayType (type: pc.STRUCT); 
VAR inx, base_inx, elements_inx: MetaDataIndex;
    name: ARRAY 128 OF CHAR;
    elements: String;
    size: LONGINT;
BEGIN
  -- Array size
  inx := GetNextMetadataIndex();
  GenInstr( '!%d = !DISubrange(count: %d, lowerBound: %d)'
          , inx, type.len, type.min.get_integer() );
  fmt.print(name, '!%d', inx);
  DStrings.Assign(name, elements);        

  elements_inx := GetNextMetadataIndex();
  GenInstr('!%d = !{%s}', elements_inx, elements^);
  
  inx := GetTypeMetadataIndex(type);
  base_inx := GetTypeMetadataIndex(type.base);  
  size := pc.code.get_size(pc.su_bits, type);
  IF (type.obj # NIL) THEN
    GetTypeName(type, name);
    PutInstr( inx, '!%d = !DICompositeType(tag: DW_TAG_array_type, name: "%s"'
                 + ' , baseType: !%d, size: %d, elements: !%d)'
            , inx, name, base_inx, size, elements_inx );
  ELSE 
    PutInstr( inx, '!%d = !DICompositeType(tag: DW_TAG_array_type'
                 + ' , baseType: !%d, size: %d, elements: !%d)'
            , inx, base_inx, size, elements_inx );
  END;  
END EmitArrayType;

--------------------------------------------------------------------------------
PROCEDURE EmitProcedureType (type: pc.STRUCT): MetaDataIndex;
VAR inx: MetaDataIndex;
(*  
    proto: prc.Proto;
    proto_str: ARRAY 1024 OF CHAR;
    i: LONGINT;
    ptype: pc.STRUCT;
    ptype_inx: MetaDataIndex;
*)    
BEGIN
  IF NOT fc.GenDebug() THEN
    inx := GetNextMetadataIndex();
    GenInstr('!%d = !DISubroutineType(types: !%d)', inx, MDI_EmptyNode);
  ELSE  
    inx := GetTypeMetadataIndex(type);
    ASSERT( inx # MDI_Undefined );
    PutInstr(inx, '!%d = !DISubroutineType(types: !%d)', inx, MDI_EmptyNode);
  END;
  RETURN inx;
(* -- required supporting all types in 'TypeEmitter'
  IF NOT fc.GenDebug() THEN
    inx := GetNextMetadataIndex();
    GenInstr('!%d = !DISubroutineType(types: !%d)', inx, MDI_EmptyNode);
    RETURN inx;
  END;

  proto := nms.GetProcPrototype(type);
  
  -- Return type
  IF proto.ret_type = ir.t_void THEN
    fmt.print(proto_str, '!{null');
  ELSE
    ptype_inx := GetTypeMetadataIndex(type.base);  
    ASSERT( ptype_inx # MDI_Undefined );  
    fmt.print(proto_str, '!{!%d', ptype_inx);
  END;
  -- Arguments type
  FOR i := 0 TO proto.npar - 1 DO
    IF proto.par[i].obj # NIL THEN
      ptype := proto.par[i].obj.type;
    ELSE  
      ptype := nms.TagSizeToType(proto.par[i].type, proto.par[i].size);
    END;
    ptype_inx := GetTypeMetadataIndex(ptype);
    ASSERT( ptype_inx # MDI_Undefined );  
    fmt.append(proto_str, ', !%d', ptype_inx); 
  END;
  fmt.append(proto_str, '}');
  
  inx := GetTypeMetadataIndex(type);
  ASSERT( inx # MDI_Undefined );
  PutInstr(inx, '!%d = !DISubroutineType(types: %s)', inx, proto_str);
  RETURN inx; 
*)      
END EmitProcedureType;  


--------------------------------------------------------------------------------
PROCEDURE EmitProcedure *(obj: pc.OBJECT): MetaDataIndex;
VAR scope_inx: MetaDataIndex; 
    di_subprogram_inx: MetaDataIndex;
    di_subroutine_type_inx: MetaDataIndex;
    segm_old: cmd.CODE_SEGM;
BEGIN
  ASSERT( (pc.mods[obj.mno] = CurrModule) & (Metadata # NIL) );
  
  di_subprogram_inx := GetObjIndex(obj);
  IF di_subprogram_inx # MDI_Undefined THEN
    RETURN di_subprogram_inx;
  ELSIF obj.is_global() THEN
    scope_inx := MDI_DIFile; 
  ELSE  
    scope_inx := EmitProcedure(obj.host.obj);
  END;
  
  cmd.get_segm(segm_old);
  cmd.set_segm(Metadata);

  GenComment(" ------- PROCEDURE %s", obj.name^);
  di_subroutine_type_inx := GetTypeMetadataIndex(obj.type);
  IF di_subroutine_type_inx = MDI_Undefined THEN
    ASSERT( NOT fc.GenDebug() );
    di_subroutine_type_inx := EmitProcedureType(obj.type);
  END;

  -- !DISubprogram
  di_subprogram_inx := GetNextMetadataIndex();
  GenInstr( '!%d = distinct !DISubprogram(name: "%s"' + 
            ', scope: !%d, file: !%d, line: %d' + 
            ', type: !%d, scopeLine: %d' + 
            ', flags: DIFlagPrototyped, spFlags: DISPFlagDefinition' + 
            ', unit: !%d, retainedNodes: !%d)'
          , di_subprogram_inx, obj.name^
          , scope_inx, MDI_DIFile, GetLine(obj.pos)
          , di_subroutine_type_inx, GetLine(obj.val.pos)
          , MDI_DICompileUnit, MDI_EmptyNode );
  SetObjIndex(obj, di_subprogram_inx);
  

  cmd.set_segm(segm_old);
  RETURN di_subprogram_inx;
END EmitProcedure;

--------------------------------------------------------------------------------
PROCEDURE EmitLocation *(obj: pc.OBJECT; line: LONGINT): MetaDataIndex; 
VAR scope_inx: MetaDataIndex; 
    di_location_inx: MetaDataIndex;
    segm_old: cmd.CODE_SEGM;
BEGIN 
  ASSERT( (pc.mods[obj.mno] = CurrModule) & (Metadata # NIL) );
  scope_inx := GetObjIndex(obj);
  IF scope_inx = MDI_Undefined THEN
    RETURN MDI_Undefined;
  END;

  cmd.get_segm(segm_old);
  cmd.set_segm(Metadata);
  
  di_location_inx := GetNextMetadataIndex();
  GenInstr('!%d = !DILocation(line: %d, scope: !%d)', di_location_inx, line, scope_inx);

  cmd.set_segm(segm_old);
  RETURN di_location_inx; 
END EmitLocation; 

--------------------------------------------------------------------------------
PROCEDURE EmitComment *(format-: ARRAY OF CHAR; SEQ args: sys.BYTE); 
VAR segm_old: cmd.CODE_SEGM;
BEGIN
  cmd.get_segm(segm_old);
  cmd.set_segm(Metadata);
  GenComment(format, args);
  cmd.set_segm(segm_old);  
END EmitComment;  


--------------------------------------------------------------------------------
CONST
  -- Эти "ty_*" определены в дополнение к описанным в pcK.ob2
  -- PrimitiveTypeNo возвращает одно из "pc.ty_*" или "ty_*" значений [ + MAX(pc.TY_MODE)]
  ty_boolean_1  = pc.ty_boolean;
  ty_boolean_2  = pc.ty_aux1;
  ty_boolean_4  = pc.ty_aux2;
  ty_boolean_8  = pc.ty_aux3;
  ty_char_1     = pc.ty_char;
  ty_char_2     = pc.ty_aux4;
  ty_char_4     = pc.ty_aux5;
  ty_char_8     = pc.ty_aux6;
  ty_void       = pc.ty_void;
  ty_address    = pc.ty_aux7;

--------------------------------------------------------------------------------
PROCEDURE EmitPrimitiveType (type: pc.TY_MODE; ptr: BOOLEAN): MetaDataIndex;
VAR inx, base_inx: MetaDataIndex; 
    name: ARRAY 32 OF CHAR;
    size: LONGINT;
    encoding: ARRAY 32 OF CHAR;
BEGIN
  ASSERT( Metadata # NIL );
  IF ptr THEN
    base_inx := EmitPrimitiveType(type, FALSE);
    inx := GetNextMetadataIndex();   
    size := GetAddrSize();
    SetMetadatPrimitiveTypeIndex(type, inx, TRUE);
    GenInstr( '!%d = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !%d, size: %d)'
            , inx, base_inx, size );
  ELSE  
    CASE type OF
    | pc.ty_shortcard:    name := 'SHORTCARD';    encoding := DW_ATE_unsigned;  size := 1; 
    | pc.ty_cardinal:     name := 'CARDINAL';     encoding := DW_ATE_unsigned;  size := 2; 
    | pc.ty_longcard:     name := 'LONGCARD';     encoding := DW_ATE_unsigned;  size := 4; 
    | pc.ty_longlongcard: name := 'LONGLONGCARD'; encoding := DW_ATE_unsigned;  size := 8; 

    | pc.ty_shortint:     name := 'SHORTINT';     encoding := DW_ATE_signed;    size := 1; 
    | pc.ty_integer:      name := 'INTEGER';      encoding := DW_ATE_signed;    size := 2; 
    | pc.ty_longint:      name := 'LONGINT';      encoding := DW_ATE_signed;    size := 4; 
    | pc.ty_longlongint:  name := 'LONGLONGINT';  encoding := DW_ATE_signed;    size := 8; 
    
    | ty_void:            name := 'VOID';         encoding := DW_ATE_unsigned;  size := 0; 
    | ty_address:         name := 'ADDRESS';      encoding := DW_ATE_address;   size := GetAddrSize(); 

    | ty_boolean_1:       name := 'BOOL8';        encoding := DW_ATE_boolean;   size := 1; 
    | ty_boolean_2:       name := 'BOOL16';       encoding := DW_ATE_boolean;   size := 2; 
    | ty_boolean_4:       name := 'BOOL32';       encoding := DW_ATE_boolean;   size := 4; 
    | ty_boolean_8:       name := 'BOOL64';       encoding := DW_ATE_boolean;   size := 8; 
    
    | ty_char_1:          name := 'CHAR';         encoding := DW_ATE_unsigned_char;  size := 1; 
    | ty_char_2:          name := 'CHAR16';       encoding := DW_ATE_unsigned_char;  size := 2; 
    | ty_char_4:          name := 'CHAR32';       encoding := DW_ATE_unsigned_char;  size := 4; 
    | ty_char_8:          name := 'CHAR64';       encoding := DW_ATE_unsigned_char;  size := 8; 

    | pc.ty_real:         name := 'REAL';         encoding := DW_ATE_float;    size := 4; 
    | pc.ty_longreal:     name := 'LONGREAL';     encoding := DW_ATE_float;    size := 8; 
    | pc.ty_ld_real:      name := 'LONGLONGREAL'; encoding := DW_ATE_float;    size := 10;
     
    | pc.ty_complex:      name := 'COMPLEX';      encoding := DW_ATE_complex_float;  size := 4; 
    | pc.ty_lcomplex:     name := 'LONGCOMPLEX';  encoding := DW_ATE_complex_float;  size := 8; 
    
    ELSE
        RETURN MDI_Undefined;
    END;
    inx := GetNextMetadataIndex();   
    SetMetadatPrimitiveTypeIndex(type, inx, FALSE);
    GenInstr( '!%d = !DIBasicType(name: "%s", size: %d, encoding: %s)'
            , inx, name, size * 8, encoding);
  END;
  RETURN inx;
END EmitPrimitiveType;  


--------------------------------------------------------------------------------
PROCEDURE IsTypeSupported *(type: pc.STRUCT): BOOLEAN;
VAR ptr: BOOLEAN; 
BEGIN 
  IF (type = NIL) OR (type = pcO.addr) THEN
    RETURN TRUE;
  END;
    
  ptr := (type.mode = pc.ty_pointer);
  CASE type.mode OF
  | pc.ty_array:    RETURN IsTypeSupported(type.base) & IsTypeSupported(type.inx);
  | pc.ty_pointer:  RETURN IsTypeSupported(type.base);
  ELSE 
  END;  

--  IF (type = pcO.card8) OR (type = pcO.int8) THEN
--    type := type.base;
--  END;

  IF type = pcO.word THEN
    RETURN TRUE;
  END;
     
  CASE type.mode OF
  | pc.ty_shortcard
  , pc.ty_cardinal
  , pc.ty_longcard
  , pc.ty_longlongcard
  , pc.ty_shortint
  , pc.ty_integer
  , pc.ty_longint
  , pc.ty_longlongint
  , pc.ty_real
  , pc.ty_longreal
  , pc.ty_ld_real
  , pc.ty_complex
  , pc.ty_lcomplex

  , pc.ty_boolean
  , pc.ty_char

  , pc.ty_loc

  , pc.ty_protection

  , pc.ty_void      : RETURN TRUE;

  | pc.ty_AA
  , pc.ty_opaque        
                    : IF ptr THEN RETURN FALSE;
                      ELSE        RETURN TRUE;
                      END;

  | pc.ty_range
  , pc.ty_enum      : RETURN TRUE;
  
  ELSE                RETURN FALSE;
  END;
END IsTypeSupported;  


--------------------------------------------------------------------------------
TYPE
  EMIT_LLVM    *= POINTER TO emitLLVM_rec;
  emitLLVM_rec *= RECORD (dbg.emit_rec)
  END;
    
--------------------------------------------------------------------------------
PROCEDURE (emit: EMIT_LLVM) PrimitiveTypeNo *(type: pc.STRUCT): dbg.TYPE_INDEX;
VAR ptr: BOOLEAN;
    type_mode: pc.TY_MODE; 
    inx: LONGINT;
    size:  LONGINT;
BEGIN
  ptr := FALSE;
  IF type = pcO.addr THEN
    type_mode := ty_address;
  ELSE
    IF (type.mode = pc.ty_opaque) AND (type.base # NIL) THEN
      type := type.base;
    END;

    ptr := (type.mode = pc.ty_pointer);
    IF ptr THEN type := type.base; END;

--    IF (type = pcO.card8) OR (type = pcO.int8) THEN
--      type := type.base;
--    END;

    IF type = pcO.word THEN
      CASE type.len OF
      | 2: type := pcO.cardinal;
      | 4: type := pcO.longcard;
      END;
    END;
       
    CASE type.mode OF
    | pc.ty_shortcard
    , pc.ty_cardinal
    , pc.ty_longcard
    , pc.ty_longlongcard
    , pc.ty_shortint
    , pc.ty_integer
    , pc.ty_longint
    , pc.ty_longlongint
    , pc.ty_real
    , pc.ty_longreal
    , pc.ty_ld_real
    , pc.ty_complex
    , pc.ty_lcomplex  : type_mode := type.mode;

    | pc.ty_boolean   : size := def.type_size(type.base);
                        IF    size = 1 THEN    type_mode := ty_boolean_1;
                        ELSIF size = 2 THEN    type_mode := ty_boolean_2;
                        ELSIF size = 4 THEN    type_mode := ty_boolean_4;
                        ELSE ASSERT(size = 8); type_mode := ty_boolean_8;
                        END;
                        
    | pc.ty_char      : size := def.type_size(type.base);
                        IF    size = 1 THEN    type_mode := ty_char_1;
                        ELSIF size = 2 THEN    type_mode := ty_char_2;
                        ELSE ASSERT(size = 4); type_mode := ty_char_4;
                        END;

    | pc.ty_AA        : IF ptr THEN RETURN TYPE_INDEX_Invalid;
                        ELSE        ptr := TRUE;  type_mode := ty_address;
                        END;

    | pc.ty_opaque    : IF ptr THEN RETURN TYPE_INDEX_Invalid;
                        ELSE        ptr := TRUE;  type_mode := ty_address;
                        END;

    | pc.ty_pointer   : RETURN TYPE_INDEX_Invalid;

    | pc.ty_loc       : IF ptr AND (type.mno < pc.ZEROMno) THEN
                          type_mode := ty_address;
                        ELSE
                          type_mode := pc.ty_shortcard;
                        END;

    | pc.ty_protection: type_mode := pc.ty_cardinal;

    | pc.ty_void      : type_mode := ty_void;  (* void *)

    ELSE                RETURN TYPE_INDEX_Invalid;
    END;
  END;
  inx := ORD(type_mode);
  IF ptr THEN  
    INC(inx, FIRST_PTR_PRIMITIVE_TYPE_INDEX);  
  END;
  IF MetadataTypeIndexTable[inx] = MDI_Undefined THEN
    sys.EVAL( EmitPrimitiveType(type_mode, ptr) );
  END;
  RETURN inx;
END PrimitiveTypeNo;

--------------------------------------------------------------------------------
PROCEDURE (emit: EMIT_LLVM) TypeEmitter *(ttag: dbg.TYPE_TAG; act: dbg.ACTION; type: pc.STRUCT);
VAR inx, base_inx: MetaDataIndex;
    name: ARRAY 128 OF CHAR;
--    size: LONGINT;
BEGIN
  IF NOT fc.GenDebug() THEN  RETURN END;
--  size := pc.code.get_size(pc.su_bits, type);
  CASE ttag OF
  | dbg.ty_start:
      ASSERT( (act = dbg.act_write) AND (type = NIL) );
      dbg.type_cnt       := FIRST_NON_PRIMITIVE_TYPE_INDEX;
      dbg.write_type_cnt := FIRST_NON_PRIMITIVE_TYPE_INDEX;

  | dbg.ty_end:
      ASSERT( (act = dbg.act_write) AND (type = NIL) );
      dbg.type_cnt       := MAX(dbg.TYPE_INDEX);
      dbg.write_type_cnt := MAX(dbg.TYPE_INDEX);

  | dbg.ty_range:
      dbg.emit_type(act, type.base);
      GetTypeName(type, name);
      IF name = "" THEN
        -- make the anonymous type equal to its base type 
        CASE act OF
        | dbg.act_set:
            SetTypeIndex(type, GetTypeIndex(type.base));
        | dbg.act_write:
        END;
      ELSE  
        -- generate range type as a typedef to its base type (it the best that can be done now) 
        CASE act OF
        | dbg.act_set:
            PutIndex(type);
        | dbg.act_write:
            inx := GetTypeMetadataIndex(type);
            base_inx := GetTypeMetadataIndex(type.base);
            sys.EVAL( EmitTypedef(type, inx, base_inx) );
            EndType();
        END;
      END;
      
  | dbg.ty_enum:    
      dbg.emit_type(act, type.base);
      CASE act OF
      | dbg.act_set:
          PutIndex(type);
      | dbg.act_write:
          inx := EmitEnumerationType(type);
          IF EnumsCount = 0 THEN  AppendInstr(MDI_enums, "!%d", inx);
          ELSE                    AppendInstr(MDI_enums, ", !%d", inx);  
          END;
          INC(EnumsCount);
          EndType();
      END;

  | dbg.ty_reference:
      CASE act OF
      | dbg.act_set:
          ASSERT( FALSE );
      | dbg.act_write:
          ASSERT( type = NIL );
          EmitReferenceType();
          EndType();
      END;      

  | dbg.ty_pointer:
      IF NOT IsTypeSupported(type.base) THEN
        RETURN;
      END;
      CASE act OF
      | dbg.act_set:
          PutIndex(type);
          dbg.emit_type(act, type.base);
      | dbg.act_write:
          EmitPointerType(type);
          EndType();
      END;      
      
  | dbg.ty_array:      -- обычный типизированный статический массив
      IF NOT IsTypeSupported(type.base) THEN
        RETURN;
      END;
      CASE act OF
      | dbg.act_set:
          IF NOT (at.tmark_db_passed IN type.marks) THEN
            INCL(type.marks,at.tmark_db_passed);
            dbg.emit_type(act, type.base);
--            dbg.emit_type(act, type.inx);
            PutIndex(type);          -- LF_ARRAY
          END;
      | dbg.act_write:
          IF (at.tmark_db_passed IN type.marks) THEN
            EXCL(type.marks,at.tmark_db_passed);
            dbg.emit_type(act, type.base);
--            dbg.emit_type(act, type.inx);
            EmitArrayType(type);
            EndType();
          END;
      END;
      
  | dbg.ty_proctype:
      dbg.emit_type(act, type.base);
      CASE act OF
      | dbg.act_set:
          PutIndex(type);
      | dbg.act_write:
          base_inx := EmitProcedureType(type);  
          IF (type.obj = NIL) OR (type.obj.mode = pc.ob_type) THEN
            -- replace procedure type with a pointer to procedure type
            -- idx == proc, idx+1 == pointer to proc
            inx := GetNextMetadataIndex();   
            GenInstr( '!%d = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !%d, size: %d)'
                    , inx, base_inx, GetAddrSize());            
            GetTypeName(type, name);
            IF name # "" THEN
              inx := EmitTypedef(type, MDI_Undefined, inx);
            END;
            SetMetadataTypeIndex(GetTypeIndex(type), inx, TRUE); 
          END;
          EndType();
      END;

  | dbg.ty_module:
      CASE act OF
      | dbg.act_set:
          emit.TypeEmitter(dbg.ty_proctype, act, type);
      | dbg.act_write:
          dbg.emit_type(act, type.base);
          sys.EVAL( EmitProcedureType(type) );  
          EndType();
      END;

  ELSE
  END;  
END TypeEmitter;  

--------------------------------------------------------------------------------
PROCEDURE (emit: EMIT_LLVM) SymbEmitter *(stag: dbg.SYMB_TAG; o: pc.OBJECT; tind: dbg.TYPE_INDEX);
VAR name: ARRAY 256 OF CHAR;
    sym_inx, type_inx: MetaDataIndex; 
    var_expr_inx, scope_inx: MetaDataIndex;
    param_no: LONGINT;
BEGIN
  IF NOT fc.GenDebug() & NOT ((stag = dbg.sy_proc) & fc.GenLineno()) THEN  
    RETURN 
  END;
  CASE stag OF
  | dbg.sy_start:
      ASSERT( (o = NIL) & (tind = TYPE_INDEX_Invalid) );

  | dbg.sy_end:
      ASSERT( (o = NIL) & (tind = TYPE_INDEX_Invalid) );

  | dbg.sy_var: -- Global variable
      sym_inx := GetNextMetadataIndex();
      type_inx := MetadataTypeIndexTable[tind];
      GetObjName(o, name);
      GenInstr( '!%d = distinct !DIGlobalVariable(name: "%s"' + 
                ', scope: !%d, file: !%d, line: %d, type: !%d' + 
                ', isLocal: false, isDefinition: true)'
              , sym_inx, name
              , MDI_DICompileUnit, MDI_DIFile, GetLine(o.pos), type_inx );
      
      var_expr_inx := GetNextMetadataIndex();
      GenInstr( '!%d = !DIGlobalVariableExpression(var: !%d, expr: !DIExpression())'
              , var_expr_inx, sym_inx);

      SetObjIndex(o, var_expr_inx);
      IF GlobalsCount = 0 THEN  AppendInstr(MDI_globals, "!%d", var_expr_inx);
      ELSE                      AppendInstr(MDI_globals, ", !%d", var_expr_inx);  
      END;
      INC(GlobalsCount);

  | dbg.sy_proc:
      sys.EVAL( EmitProcedure(o) );
      
  | dbg.sy_scope_open:
      ASSERT( tind = TYPE_INDEX_Invalid );
  
  | dbg.sy_scope_close:
      ASSERT( tind = TYPE_INDEX_Invalid );

  | dbg.sy_local_var:
      scope_inx := GetObjIndex(o.host.obj); 
      sym_inx := GetNextMetadataIndex();
      type_inx := MetadataTypeIndexTable[tind];
      GetObjName(o, name);
      GenInstr( '!%d = !DILocalVariable(name: "%s", scope: !%d, file: !%d, line: %d, type: !%d)'
              , sym_inx, name, scope_inx, MDI_DIFile, GetLine(o.pos), type_inx );
      SetObjIndex(o, sym_inx);
      UpdateDbgCallInstr(o, sym_inx);

  | dbg.sy_param:
      scope_inx := GetObjIndex(o.host.obj); 
      sym_inx := GetNextMetadataIndex();
      type_inx := MetadataTypeIndexTable[tind];
      GetObjName(o, name);
      param_no := GetParamNumber(o);
      GenInstr( '!%d = !DILocalVariable(name: "%s", arg: %d, scope: !%d, file: !%d, line: %d, type: !%d)'
              , sym_inx, name, param_no, scope_inx, MDI_DIFile, GetLine(o.pos), type_inx );
      SetObjIndex(o, sym_inx);
      UpdateDbgCallInstr(o, sym_inx);

  ELSE            
  END;
END SymbEmitter;  

--------------------------------------------------------------------------------
PROCEDURE (emit: EMIT_LLVM) generate *(): BOOLEAN;
VAR res: BOOLEAN;
    segm_old: cmd.CODE_SEGM;
BEGIN
  CurrModule := at.curr_mod;
  EnumsCount := 0;
  GlobalsCount := 0;
  cmd.new_segm(Metadata);
  cmd.get_segm(segm_old);
  cmd.set_segm(Metadata);
  
  EmitPredefinedMetadataNodes();
  AllocateMetadataTypeIndexTable();
  IF fc.GenDebug() OR fc.GenLineno() THEN 
    res := emit.generate^();
  ELSE
    res := TRUE;  
  END;
  ClosePredefinedMetadataNodes();
  
  cmd.set_segm(segm_old);
  RETURN res;
END generate;

--------------------------------------------------------------------------------
PROCEDURE Clear *();
BEGIN
  Metadata := NIL;
  CurrModule := NIL;
  MetadataTypeIndexTable := NIL;
END Clear;

--------------------------------------------------------------------------------
VAR
  emitLLVM: EMIT_LLVM;

BEGIN
  Metadata := NIL;
  NEW(emitLLVM);
  reg.Register(opt.dbgFormat, opt.dbg_LLVM, emitLLVM);
END dbgLLVM.