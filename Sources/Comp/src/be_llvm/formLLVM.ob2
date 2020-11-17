--------------------------------------------------------------------------------
--                          Excelsior LLVM Compiler
--                         (c) 2020, Excelsior, LLC.
--
-- Module:   formLLVM
-- Mission:  Output LLVM Assembler Code 
-- Authors:  Lvov Konstantin
-- Created:  18-Feb-2020
--------------------------------------------------------------------------------
MODULE formLLVM;

IMPORT sys := SYSTEM;
IMPORT Strings;
IMPORT DStrings;
IMPORT fmt := FormStr;

IMPORT  pc := pcK;
IMPORT env := xiEnv;
IMPORT xfs := xiFiles;
IMPORT reg := Registry;
IMPORT plt := xmPlatform;
IMPORT xcStr;

IMPORT ir;

IMPORT  at := opAttrs;
IMPORT  fc := CodeFace;
IMPORT  cd := CodeDef;
IMPORT prc := opProcs;
IMPORT opt := Options;
IMPORT dbg := DbgFace;

IMPORT nms := llvmNames;
IMPORT def := llvmDefs;

IMPORT dbgLLVM;

--------------------------------------------------------------------------------
CONST 
  ASM_EXT = ".ll";

--------------------------------------------------------------------------------
TYPE 
  CARD32  = sys.CARD32;
  String  = ARRAY 1024 OF CHAR;
  DString = DStrings.String;


--------------------------------------------------------------------------------
VAR 
  fd: xfs.TextFile;  -- descriptor of output asm-file
  LINENOSRC: BOOLEAN;
  GENDEBUG : BOOLEAN;
  GENLINENO: BOOLEAN;

--------------------------------------------------------------------------------
--                      Useful Predicates and Stuff
--------------------------------------------------------------------------------

PROCEDURE isMainModuleEntry (obj: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (obj.mode = pc.ob_module) & (obj.mno = at.curr_mno) & at.main;
END isMainModuleEntry;

--------------------------------------------------------------------------------
CONST
  CODE_INDENT *= "  ";

--------------------------------------------------------------------------------
PROCEDURE WriteIndent (); 
BEGIN 
  fd.print(CODE_INDENT); 
END WriteIndent; 

--------------------------------------------------------------------------------
PROCEDURE WriteComment (format-: ARRAY OF CHAR; SEQ args: sys.BYTE); 
BEGIN 
  fd.print("%c", cd.LINE_COMMENT_CHAR); 
  fd.print(format, args); 
END WriteComment; 

--------------------------------------------------------------------------------
PROCEDURE WriteModuleTitle();
VAR str: env.String;
BEGIN
  env.info.Version(str);
  WriteComment(" %s\n", env.info.title^);
  WriteComment(" %s\n", str^);
END WriteModuleTitle;

--------------------------------------------------------------------------------
PROCEDURE WriteTargeDatatLayout();
VAR layout: DString;
BEGIN
  IF plt.targetBigendian THEN DStrings.Assign("E", layout); 
  ELSE                        DStrings.Assign("e", layout);
  END;
  DStrings.Append("-S64", layout);      -- the natural alignment of the stack in bits
  DStrings.Append("-p:32:32", layout);  -- the size of a pointer and its alignment
  DStrings.Append("-n32", layout);      -- a set of native integer widths in bits
  DStrings.Append("-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64", layout); -- the alignment for an integer type in bits
  DStrings.Append("-f32:32:32-f64:64:64", layout); -- the alignment for a floating-point type in bits
  fd.print('target datalayout = "%s"\n', layout^);
END WriteTargeDatatLayout;  

--------------------------------------------------------------------------------
PROCEDURE WriteTargetTriple();
VAR triple: pc.STRING;
BEGIN
  env.config.Equation("LLVM_TARGET_TRIPLE", triple);
  IF triple # NIL THEN
    fd.print('target triple = "%s"\n', triple^);
  ELSE
    env.errors.Warning(env.null_pos, 1025, "LLVM_TARGET_TRIPLE");
  END;
END WriteTargetTriple;

--------------------------------------------------------------------------------
PROCEDURE WriteModuleHeader();
VAR val: pc.STRING;
    ext: xfs.FNAME;
BEGIN
  WriteModuleTitle();
  env.config.Equation("FILE", val);
  IF (val # NIL) & xfs.sys.sGetExt(val^, ext) THEN
    WriteComment(" ModuleID = '%s.%s'\n", at.curr_mod.name^, ext);
    fd.print('source_filename = "%s.%s"\n', at.curr_mod.name^, ext);
  END;
  WriteTargeDatatLayout();
  WriteTargetTriple();
END WriteModuleHeader;


--------------------------------------------------------------------------------
--                            File Operations                                 --
--------------------------------------------------------------------------------
PROCEDURE Open *();
VAR fn: pc.STRING;
BEGIN
  xfs.sys.Create('', at.curr_mod.name^, ASM_EXT, fn);
  xfs.sys.UseFirst(fn^, fn);
  xfs.text.Open(fn^, TRUE);
  IF xfs.text.file = NIL THEN
    env.errors.Fault(env.null_pos, 424, xfs.text.msg^);
  END;
  fd := xfs.text.file(xfs.TextFile);
END Open;

--------------------------------------------------------------------------------
PROCEDURE Close *();
VAR err: pc.STRING;
BEGIN
  fd.CloseNew(TRUE, FALSE, FALSE, err);
  IF err # NIL THEN 
    env.errors.Fault(env.null_pos, 432, err^); 
  END;
  fd := NIL;
END Close;


--------------------------------------------------------------------------------
--                           Generate Labels                                  --
--------------------------------------------------------------------------------
PROCEDURE MakeName (obj: pc.OBJECT; VAR buf: ARRAY OF CHAR);
BEGIN
  nms.GetObjName(buf, obj);
END MakeName;


--------------------------------------------------------------------------------
--                           Generate Types                                   --
--------------------------------------------------------------------------------

PROCEDURE WriteIRTypeName (type: ir.TypeType; size: ir.SizeType);
VAR tname: nms.String; 
BEGIN
  nms.GetTagTypeName(tname, type, size);
  fd.print(tname);
END WriteIRTypeName;

--------------------------------------------------------------------------------
PROCEDURE WriteTypeName (type: pc.STRUCT);
VAR tname: nms.String; 
BEGIN
  nms.GetTypeName(tname, type);
  fd.print(tname);
END WriteTypeName;

--------------------------------------------------------------------------------
PROCEDURE WriteProcReturnType (obj: pc.OBJECT);
VAR tname: nms.String; 
BEGIN
  nms.GetProcReturnTypeName(tname, obj);
  fd.print(tname);
END WriteProcReturnType;  


--------------------------------------------------------------------------------
--                         Source Line Access                                 --
--------------------------------------------------------------------------------
TYPE
  SegmentSourceLines = RECORD
    fname:      pc.STRING;
    start_line: LONGINT;
    line_buf:   POINTER TO ARRAY OF pc.STRING;
  END;

--------------------------------------------------------------------------------
PROCEDURE (VAR ssl: SegmentSourceLines) init (sg: cd.CODE_SEGM);

    -- 1 -- init ---------------------------------------------------------------
    PROCEDURE readSourceLines (toLine: LONGINT);
    CONST
      MaxSrcLineLen = 1024;
    VAR
      fsFileName: xfs.String;
      file      : xfs.TextFile;
      str, buf  : ARRAY MaxSrcLineLen OF CHAR;
      i         : LONGINT;
    BEGIN
      xfs.sys.Lookup (ssl.fname^, (* VAR *) fsFileName);
      IF fsFileName = NIL THEN
        RETURN;
      END;
      xfs.text.Open (fsFileName^, FALSE);
      IF xfs.text.file = NIL THEN
        RETURN;
      END;

      file := xfs.text.file (xfs.TextFile);
      xfs.text.file := NIL;

      FOR i := 1 TO toLine DO
        COPY ("", str);
        LOOP
          file.ReadString (buf);
          ASSERT (file.readRes # xfs.bufOverflow);
          Strings.Append (buf, str);
          IF (file.readRes = xfs.endOfLine) OR (file.readRes = xfs.endOfInput) THEN
            EXIT;
          END;
        END;
        IF i >= ssl.start_line THEN
          DStrings.Assign (str, ssl.line_buf[i-ssl.start_line]);
        END;
      END;

      file.Close();
    END readSourceLines;

-- 0 -- init -------------------------------------------------------------------
VAR line, end_line, col, i: LONGINT;
    fname: pc.STRING;
BEGIN
  IF sg.xref_len = 0 THEN
    ssl.line_buf := NIL;
    ssl.start_line := 0;
  ELSE
    ssl.start_line := MAX(LONGINT);
    fname := NIL;
    end_line := 0;
    FOR i := 0 TO sg.xref_len-1 DO
      sg.xref[i].txtpos.unpack(fname, line, col);
      IF line < ssl.start_line THEN
        ssl.start_line := line;
      END;
      IF line > end_line THEN
        end_line := line;
      END;
    END;
    ssl.fname := fname;
    NEW(ssl.line_buf, end_line - ssl.start_line + 2);
    FOR i:= 0 TO LEN(ssl.line_buf^)-1 DO
      ssl.line_buf[i] := NIL;
    END;
    readSourceLines(end_line+1);
  END;
END init;

--------------------------------------------------------------------------------
PROCEDURE (VAR ssl: SegmentSourceLines) finalize ();
BEGIN
  ssl.fname      := NIL;
  ssl.start_line := 0;
  ssl.line_buf   := NIL;
END finalize;

--------------------------------------------------------------------------------
PROCEDURE (VAR ssl: SegmentSourceLines) getSrcLine ( fileName: pc.STRING
                                                   ; line: LONGINT
                                                   ): pc.STRING;
BEGIN
  ASSERT( fileName^ = ssl.fname^ );
  RETURN ssl.line_buf[line - ssl.start_line];
END getSrcLine;


--------------------------------------------------------------------------------
--                            Generate Segments                               --
--------------------------------------------------------------------------------
TYPE
  LabelMask = POINTER TO ARRAY OF def.LabelHolder;
  
--------------------------------------------------------------------------------
PROCEDURE BuildLabelMask (sg: cd.CODE_SEGM): LabelMask;
VAR i: LONGINT;
    lh: def.LabelHolder; 
    lmask: LabelMask;
BEGIN
  NEW(lmask, sg.code_len);
  FOR i := 0 TO sg.code_len-1 DO lmask[i] := NIL; END;
  FOR i := 0 TO sg.labels_len-1 DO
    lh := sg.labels[i];
    IF lh.offs < sg.code_len THEN
      ASSERT( lmask[lh.offs] = NIL ); 
      lmask[lh.offs] := lh; 
    END;
  END;
  RETURN lmask;  
END BuildLabelMask; 

--------------------------------------------------------------------------------
PROCEDURE WriteSegment (obj: pc.OBJECT);
VAR sg: cd.CODE_SEGM;
    lmask: LabelMask;
      
   -- 1 -- WriteSegment -----------------------------------------------------------
   -- output code from offset "i" to "offs"
   PROCEDURE WriteCode (VAR i: LONGINT; offs: LONGINT; line: LONGINT);
   VAR str: String;
       pos_inx: dbgLLVM.MetaDataIndex;
   BEGIN
     pos_inx := dbgLLVM.MDI_Undefined;
     IF GENLINENO THEN
       IF (line > 0) THEN 
         pos_inx := dbgLLVM.EmitLocation(obj, line);
       END;
     END;
     WHILE (i < offs) DO
       IF lmask[i] # NIL THEN
         IF i # 0 THEN
           fd.print("\n");
         END;
         IF lmask[i].node_no = ir.UndefNode THEN
           fd.print("%s:\n", lmask[i].label);
         ELSE  
           fmt.print(str, "%s:", lmask[i].label);
           xcStr.AlignStr(str, 50);
           fd.print("%s", str);
           WriteComment(" ==== Node: %d\n", lmask[i].node_no);
         END;  
       END;

       WriteIndent(); 
       fd.print(sg.acode[i]^);
       IF GENLINENO THEN
         IF pos_inx # dbgLLVM.MDI_Undefined THEN
           fd.print(", !dbg !%d", pos_inx);
         END;
       END;
       fd.print("\n");

       INC(i);
     END;     
   END WriteCode;

-- 0 -- WriteSegment -----------------------------------------------------------
VAR i, x, offs: LONGINT;
    ssl: SegmentSourceLines;
    line, oline, col: LONGINT;
    fname, srcLine: pc.STRING;
BEGIN
  sg := cd.get_ready(obj);
  lmask := BuildLabelMask(sg);
  i := 0;
  oline := -2;
  IF GENLINENO OR LINENOSRC THEN
    IF GENLINENO & (sg.xref_len > 0) THEN
      dbgLLVM.EmitComment(" ------- PROCEDURE %s BEGIN", obj.name^);
    END;
    ssl.init(sg);
    x := 0;  
    WHILE x < sg.xref_len DO
      offs := sg.xref[x].offs;
      REPEAT -- join xref with the same offs
        INC(x);
      UNTIL (x >= sg.xref_len) OR (offs # sg.xref[x].offs);
      DEC(x);

      sg.xref[x].txtpos.unpack(fname, line, col);
      IF (line # oline) THEN       -- changed line, but not column
        WriteCode(i, offs, oline+1);
        IF LINENOSRC THEN
          srcLine := ssl.getSrcLine(fname, line+1);
          IF srcLine # NIL THEN
            fd.print("\n");
            WriteComment(" %d: %s\n", line+1, srcLine^);
          END;
        END;
      END;

      oline := line;
      INC(x);
    END; -- WHILE x < s.xref_len
    ssl.finalize();
  END;
     
  -- output residual code
  offs := sg.code_len;
  WriteCode(i, offs, oline+1);
END WriteSegment;


--------------------------------------------------------------------------------
--                             Write Type                                     --
--------------------------------------------------------------------------------
PROCEDURE ^ CleanTypeMarks (type: pc.STRUCT);
PROCEDURE ^ WriteType (type: pc.STRUCT);

--------------------------------------------------------------------------------
PROCEDURE IsPrimitiveType (type: pc.STRUCT): BOOLEAN; 
CONST NONPRIMITIVEs = pc.TY_SET{pc.ty_record};
BEGIN
  CASE type.mode OF
  | pc.ty_record
  , pc.ty_array
  , pc.ty_proctype:  
      RETURN FALSE; 
  ELSE             
      RETURN TRUE; 
  END; 
END IsPrimitiveType; 

--------------------------------------------------------------------------------
PROCEDURE CleanFieldListMarks (field: pc.OBJECT);
BEGIN
  WHILE field # NIL DO
    CASE field.mode OF
    | pc.ob_field:
        CleanTypeMarks(field.type);
    | pc.ob_header:
        ASSERT(FALSE);
    END;
    field := field.next;
  END;  
END CleanFieldListMarks;   

--------------------------------------------------------------------------------
PROCEDURE CleanTypeMarks (type: pc.STRUCT);
BEGIN
  IF NOT (at.tmark_llvm_defined IN type.marks) OR IsPrimitiveType(type) THEN  
    RETURN;
  END;
  EXCL(type.marks, at.tmark_llvm_defined);
  CASE type.mode OF
  | pc.ty_record:
      CleanFieldListMarks(type.prof);
  | pc.ty_array:  
      CleanTypeMarks(type.base);  
  ELSE
  END;  
END CleanTypeMarks;  

--------------------------------------------------------------------------------
PROCEDURE CleanObjTypeMarks (obj: pc.OBJECT);
BEGIN
  CleanTypeMarks(obj.type);
END CleanObjTypeMarks;  

--------------------------------------------------------------------------------
PROCEDURE CleanExternTypeMarks (use: pc.USAGE);
BEGIN
  CleanObjTypeMarks(use.obj);
END CleanExternTypeMarks;  


--------------------------------------------------------------------------------
PROCEDURE MakeType(VAR buf: DString; type: pc.STRUCT);
VAR tname: nms.String;
BEGIN
  WriteType(type);
  nms.GetTypeName(tname, type);
  DStrings.Append(tname, buf);
END MakeType;   

--------------------------------------------------------------------------------
PROCEDURE MakeFieldType(VAR buf: DString; field: pc.OBJECT);
VAR tname: nms.String;
BEGIN
  WriteType(field.type);
  nms.GetTypeName(tname, field.type);
  DStrings.Append(tname, buf);
END MakeFieldType;   

--------------------------------------------------------------------------------
PROCEDURE MakeFieldList(VAR buf: DString; field: pc.OBJECT);
VAR idx: LONGINT;
    comment: String;
BEGIN
  idx := 0;
  WHILE field # NIL DO
    DStrings.Append("    ", buf);
    CASE field.mode OF
    | pc.ob_field:
        MakeFieldType(buf, field);
        IF field.next # NIL THEN
          DStrings.Append(", ", buf);
        ELSE  
          DStrings.Append("  ", buf);
        END;
        fmt.print(comment, "  %c %2d: %s", cd.LINE_COMMENT_CHAR, idx, field.name^);
        DStrings.Append(comment, buf);

    | pc.ob_header:
        ASSERT(FALSE);
    END;
    DStrings.Append("\n", buf);
    INC(idx);
    field := field.next;
  END;  
END MakeFieldList;   

--------------------------------------------------------------------------------
PROCEDURE MakeProcType (VAR buf: DString; type: pc.STRUCT);
VAR i: LONGINT;
    proto: prc.Proto;
    tname: nms.String;
BEGIN
  proto := nms.GetProcPrototype(type);
  ASSERT( proto # NIL );

  IF proto.ret_type = ir.t_void THEN
    DStrings.Append("void", buf);
  ELSE
    MakeType(buf, type.base);  
  END;
  DStrings.Append(" (", buf);

  FOR i := 0 TO proto.npar - 1 DO
    IF i > 0 THEN
      DStrings.Append(", ", buf);
    END;
    IF proto.par[i].obj # NIL THEN
      MakeType(buf, proto.par[i].obj.type);
    ELSE 
      nms.GetTagTypeName(tname, proto.par[i].type, proto.par[i].size); 
      DStrings.Append(tname, buf);
    END;
  END;

  DStrings.Append(")*", buf);
END MakeProcType;

--------------------------------------------------------------------------------
PROCEDURE MakeTypeDef (VAR buf: DString; type: pc.STRUCT);
VAR tname, str: nms.String;
BEGIN
  CASE type.mode OF
  | pc.ty_record:
      IF type.align = 1 THEN 
        DStrings.Append("<", buf);
      END;
      DStrings.Append("{\n", buf);
      MakeFieldList(buf, type.prof);
      DStrings.Append("}", buf);
      IF type.align = 1 THEN 
        DStrings.Append(">", buf);
      END;
      
  | pc.ty_array:
      IF (type.base.mode = pc.ty_loc) & (type.len = 2) THEN 
        nms.GetTypeName(tname, pc.cardinal_type);
        DStrings.Append(tname, buf);
      ELSIF (type.base.mode = pc.ty_loc) & (type.len = 4) THEN 
        nms.GetTypeName(tname, pc.longcard_type);
        DStrings.Append(tname, buf);
      ELSE
        fmt.print(str, "[%d x ", type.len);
        DStrings.Append(str, buf);
        MakeType(buf, type.base);
        DStrings.Append("]", buf);
      END;
      
  | pc.ty_proctype:
      MakeProcType(buf, type);      
      
  ELSE
      nms.GetTypeName(tname, type);
      DStrings.Append(tname, buf);
  END;
END MakeTypeDef;  

--------------------------------------------------------------------------------
PROCEDURE WriteType (type: pc.STRUCT);
VAR name: nms.String;
    type_def: DString;
BEGIN
  IF (at.tmark_llvm_defined IN type.marks) 
  OR (type.obj = NIL) OR IsPrimitiveType(type) 
  THEN
    RETURN;
  END;
  INCL(type.marks, at.tmark_llvm_defined);
  MakeName(type.obj, name);
  DStrings.Append("", type_def);
  MakeTypeDef(type_def, type);
  fd.print("\n");
  fd.print("%s = type ", name);
  fd.print(type_def^);
END WriteType;  

--------------------------------------------------------------------------------
PROCEDURE WriteObjType (obj: pc.OBJECT);
BEGIN
  WriteType(obj.type);
END WriteObjType;  

--------------------------------------------------------------------------------
PROCEDURE WriteExternType (use: pc.USAGE);
BEGIN
  WriteObjType(use.obj);
END WriteExternType;  


--------------------------------------------------------------------------------
--                           Write Variable                                   --
--------------------------------------------------------------------------------

PROCEDURE WriteExternVariable (use: pc.USAGE);
VAR name: nms.String;
BEGIN
  MakeName(use.obj, name);
  fd.print("%s = external dso_local global ", name);
  WriteTypeName(use.obj.type);
  fd.print("\n");
END WriteExternVariable;

--------------------------------------------------------------------------------
PROCEDURE WriteVariable (obj: pc.OBJECT);
VAR name: nms.String;
    md_var_inx: dbgLLVM.MetaDataIndex;
BEGIN
  MakeName(obj, name);
  fd.print("%s = ", name);
  IF obj.is_public() THEN  fd.print("common dso_local ");  
  ELSE                     fd.print("internal ");
  END;  
  fd.print("global ");
  WriteTypeName(obj.type);
  IF obj.is_initvar() THEN
    ASSERT( FALSE );
  ELSIF (obj.type.mode IN pc.PADRs) THEN
    fd.print(" null");
  ELSE
    fd.print(" zeroinitializer");
  END;
  IF GENDEBUG THEN
    md_var_inx := dbgLLVM.GetObjIndex(obj);
    IF md_var_inx # dbgLLVM.MDI_Undefined THEN
      fd.print(", !dbg !%d", md_var_inx);
    END;
  END;
  fd.print("\n");
END WriteVariable;

--------------------------------------------------------------------------------
--                           Write Constant                                   --
--------------------------------------------------------------------------------

PROCEDURE WriteExternConstant (use: pc.USAGE);
VAR name: nms.String;
BEGIN
  MakeName(use.obj, name);
  fd.print("%s = external dso_local constant ", name);
  WriteTypeName(use.obj.type);
  fd.print("\n");
END WriteExternConstant;

--------------------------------------------------------------------------------
PROCEDURE WriteConstant (obj: pc.OBJECT);
VAR name: nms.String;
BEGIN
  MakeName(obj, name);
  fd.print("%s = ", name);
  IF obj.is_public() THEN  fd.print("dso_local ");  
  ELSE                     fd.print("internal ");
  END;  
  fd.print("constant ");
  WriteSegment(obj);
  fd.print("\n");
END WriteConstant;

--------------------------------------------------------------------------------
--                           Write Procedures                                 --
--------------------------------------------------------------------------------

PROCEDURE WriteParamListByProto (proto: prc.Proto; proc_def: BOOLEAN);
VAR obj: pc.OBJECT;
    i: ir.ProtoParNumber;
BEGIN
  FOR i := 0 TO proto.npar-1 DO
    IF i # 0 THEN fd.print(", ");  END;
    obj := proto.par[i].obj;
    IF obj # NIL THEN
      WriteTypeName(obj.type);
      IF nms.isProcParamReference(proto, i) & (obj.type.mode # pc.ty_array_of) THEN
        fd.print("*");
      END;
    ELSE  
      WriteIRTypeName(proto.par[i].type, proto.par[i].size);
      IF nms.isProcParamReference(proto, i) THEN
        fd.print("*");
      END;
    END;
      
    IF proc_def THEN
      fd.print(" %%%d", i);
    END;
  END;
END WriteParamListByProto;

--------------------------------------------------------------------------------
PROCEDURE WriteModuleEntryHeader (obj: pc.OBJECT; VAR name: ARRAY OF CHAR);
BEGIN
  MakeName(obj, name);
  IF (at.omark_not_used IN obj.marks) THEN
    RETURN;
  END;

  fd.print("define dso_local void %s ", name);
  IF isMainModuleEntry(obj) THEN
    IF at.GENDLL IN at.COMP_MODE THEN
      fd.print("() ");
    ELSE  
      fd.print("(i32 %argc, i8** %argv) ");
    END;
  ELSE 
    fd.print("() ");
  END;
END WriteModuleEntryHeader;  

--------------------------------------------------------------------------------
PROCEDURE WriteProcHeader (obj: pc.OBJECT; VAR name: ARRAY OF CHAR; proc_def: BOOLEAN);
VAR proto: prc.Proto;
    md_proc_inx: dbgLLVM.MetaDataIndex;
BEGIN
  MakeName(obj, name);

  IF fc.ObjectStatus(obj) = fc.status_Extern THEN  fd.print("declare ");
  ELSE                                             fd.print("define ");  
  END;
  IF obj.is_public() THEN  fd.print("dso_local ");
  ELSE                     fd.print("internal "); 
  END;
  
  WriteProcReturnType(obj);
  fd.print(" %s ", name);
  fd.print("(");
  proto := prc.ProcProtoByObj(obj);
  WriteParamListByProto(proto, proc_def);
  fd.print(") ");

  fd.print("local_unnamed_addr ");
  IF proc_def THEN
    IF (pc.ttag_neverinline IN obj.type.tags) THEN
      fd.print("noinline ");    
    END;
    fd.print("#0 ");
    IF GENDEBUG OR GENLINENO THEN
      md_proc_inx := dbgLLVM.EmitProcedure(obj);
      ASSERT( md_proc_inx # dbgLLVM.MDI_Undefined );
      fd.print("!dbg !%d ", md_proc_inx);
    END;      
  END;
END WriteProcHeader;

--------------------------------------------------------------------------------
PROCEDURE WriteProcDefinition (obj: pc.OBJECT);
VAR name: nms.String;
BEGIN
  ASSERT( nms.IsProc(obj) );
  IF (at.omark_gen_ready IN obj.marks) THEN
    fd.print("\n\n");
    WriteComment("--------------------------------------------------------------------------------\n");
--    IF isModuleEntry(obj) THEN
--      WriteModuleEntryHeader(obj, name);
--    ELSE
--      WriteProcHeader(obj, name);
--    END;     
    WriteProcHeader(obj, name, TRUE);
--    fd.print("noinline ");
    fd.print("{\n");
    WriteSegment(obj);
    fd.print("} ");  
    WriteComment(" %s\n", name);
  END;
END WriteProcDefinition;  


--------------------------------------------------------------------------------
PROCEDURE FilterLocals (obj: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN fc.ObjectStatus(obj) = fc.status_Local;
END FilterLocals;

PROCEDURE FilterPublics (obj: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN fc.ObjectStatus(obj) = fc.status_Global;
END FilterPublics;

PROCEDURE FilterExterns (obj: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN fc.ObjectStatus(obj) = fc.status_Extern;
END FilterExterns;

PROCEDURE FilterInternalGlobals (obj: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN NOT FilterExterns(obj) & obj.is_global();
END FilterInternalGlobals;

PROCEDURE FilterCode (obj: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (fc.ObjectClass(obj) = fc.class_Text);
END FilterCode;

PROCEDURE FilterVar (obj: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (obj.mode = pc.ob_var);
END FilterVar;

PROCEDURE FilterConst (obj: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (obj.mode = pc.ob_cons);
END FilterConst;

PROCEDURE FilterType (obj: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (obj.mode = pc.ob_type);
END FilterType;

PROCEDURE FilterVarWithAnonymousType (obj: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (obj.mode = pc.ob_var) & (pc.ttag_anonim IN obj.type.tags);
END FilterVarWithAnonymousType;

--------------------------------------------------------------------------------
TYPE 
  IterateUsageProcType = PROCEDURE (use: pc.USAGE);

--------------------------------------------------------------------------------
PROCEDURE IterateUsedImport (proc: IterateUsageProcType; filter: fc.filter_proc_type);
VAR use: pc.USAGE;
BEGIN
  use := at.curr_mod.type.use_import;
  WHILE use # NIL DO
    IF (filter = NIL) OR filter(use.obj) THEN
      proc(use);
    END;  
    use := use.next;
  END;
END IterateUsedImport; 

--------------------------------------------------------------------------------
PROCEDURE WriteTypes ();
BEGIN
  fd.print('\n');
  IterateUsedImport(WriteExternType, FilterType);
  IterateUsedImport(WriteExternType, FilterVarWithAnonymousType);
  fd.print('\n');
  fc.IterateContext(WriteObjType, FilterType, NIL);
  fc.IterateContext(WriteObjType, FilterVarWithAnonymousType, NIL);
  
  IterateUsedImport(CleanExternTypeMarks, FilterType);
  IterateUsedImport(CleanExternTypeMarks, FilterVarWithAnonymousType);
  fc.IterateContext(CleanObjTypeMarks, FilterType, NIL);
  fc.IterateContext(CleanObjTypeMarks, FilterVarWithAnonymousType, NIL);  
END WriteTypes;

--------------------------------------------------------------------------------
PROCEDURE WriteVariables ();
BEGIN
  fd.print('\n');
  IterateUsedImport(WriteExternVariable, FilterVar);
  fd.print('\n');
  fc.IterateContext(WriteVariable, FilterInternalGlobals, FilterVar);
END WriteVariables; 

--------------------------------------------------------------------------------
PROCEDURE WriteConstants ();
BEGIN
  fd.print('\n');
  IterateUsedImport(WriteExternConstant, FilterConst);
  fd.print('\n');
  fc.IterateContext(WriteConstant, FilterInternalGlobals, FilterConst);
END WriteConstants;

--------------------------------------------------------------------------------
PROCEDURE WriteProcs();
BEGIN
  fc.IterateContext(WriteProcDefinition, FilterLocals,  FilterCode);
  fc.IterateContext(WriteProcDefinition, FilterPublics, FilterCode);
END WriteProcs;


--------------------------------------------------------------------------------
PROCEDURE WriteExternalProcsDecl ();
VAR i: ir.ProcNum;
    name: ARRAY 256 OF CHAR;
BEGIN
  fd.print('\n');
  FOR i := ir.ZEROProcNum TO sys.PRED(prc.NProc) DO
    IF prc.IsExternal(i) THEN
      WriteProcHeader(prc.ProcObj(i), name, FALSE);
      fd.print('\n');
    END;    
  END;
  IF GENDEBUG THEN
    fd.print('\n');
    fd.print('declare void @llvm.dbg.addr(metadata, metadata, metadata) nounwind readnone speculatable willreturn');
    fd.print('\n');    
  END;
END WriteExternalProcsDecl;  


--------------------------------------------------------------------------------
PROCEDURE WriteAttributes();
BEGIN
  fd.print('\n\n');
  fd.print('attributes #0 = { "no-builtins" }');
END WriteAttributes;


--------------------------------------------------------------------------------
PROCEDURE WriteDebugInfo();
VAR i: LONGINT; 
    metadata: cd.CODE_SEGM;
BEGIN
  metadata := dbgLLVM.GetMetadata();
  FOR i := 0 TO metadata.code_len-1 DO
    fd.print("%s\n", metadata.acode[i]^ );
  END;
END WriteDebugInfo;

--------------------------------------------------------------------------------
PROCEDURE WriteMetadata();
VAR producer: DString;
BEGIN
  fd.print('\n\n');
  IF GENDEBUG OR GENLINENO THEN
    fd.print('!llvm.dbg.cu = !{!%d}\n', dbgLLVM.MDI_DICompileUnit);
    fd.print('!llvm.module.flags = !{!%d, !%d}\n', dbgLLVM.MDI_DwarfVersion, dbgLLVM.MDI_DebugInfoVersion);
    fd.print('!llvm.ident = !{!%d}\n', dbgLLVM.MDI_ident);
    fd.print('\n');
    WriteDebugInfo();
  ELSE    
    fd.print('!llvm.ident = !{!0}\n');
    fd.print('\n');
    env.info.Version(producer);
    fd.print('!0 = !{!"%s"}\n', producer^);
  END;   
END WriteMetadata;

--------------------------------------------------------------------------------
--              Main Entry to Output LLVM Assembler Code                      --
--------------------------------------------------------------------------------
TYPE
  FORM_LLVM    *= POINTER TO formLLVM_rec;
  formLLVM_rec *= RECORD (fc.formobj_rec)
  END;

--------------------------------------------------------------------------------
PROCEDURE (this: FORM_LLVM) generate *();
BEGIN
  LINENOSRC := env.config.Option("LINENOSRC");
  GENDEBUG  := this.SetDebugFormat() AND fc.GenDebug();
  GENLINENO := GENDEBUG OR fc.GenLineno();
  IF GENDEBUG OR GENLINENO THEN 
     IF NOT dbg.generate("", "", FALSE, at.DbgNestedProc IN at.COMP_MODE) THEN
       GENDEBUG  := FALSE; 
       GENLINENO := FALSE;
     END;
  END;           
  Open();
  WriteModuleHeader();
  WriteTypes();
  WriteVariables();
  WriteConstants();
  WriteExternalProcsDecl();
  WriteProcs();
  WriteAttributes();
  WriteMetadata();
  Close();
  dbgLLVM.Clear();
END generate;

--------------------------------------------------------------------------------
--                          Initialization Routines
--------------------------------------------------------------------------------
PROCEDURE (this: FORM_LLVM) activate *();
BEGIN
END activate;

--------------------------------------------------------------------------------
VAR
  formLLVM: FORM_LLVM;
  dbgFmt:   reg.ITEM;
  
BEGIN
  LINENOSRC := FALSE;
  NEW(formLLVM);
  formLLVM.defaultFormat := NIL;

  NEW (dbgFmt);
  formLLVM.AddItem (opt.dbg_LLVM, dbgFmt);
  formLLVM.defaultFormat := dbgFmt;

  reg.Register(opt.objFormat, opt.objLLVM, formLLVM);
END formLLVM.
