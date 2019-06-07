(* Generator of Definition files. Establish by Lion  Tue  10-24-95  *)
<* NEW h2ddebug- *>

MODULE H2DGen;

(*------------------------------------------------------------------------*)
IMPORT io:= Printf, adt, WholeStr, Strings,
       conv:= WholeConv,
       clock:= SysClock,
       sys:= SYSTEM,
       obj:= H2DObjs,
       lstr:= LongStrs,
       plat:= platform,
       cfg:= H2DCfg,
       msg:= H2DMsg,
       fio:= H2DFile;

(*  Synonyms  *)
TYPE
  INT = sys.INT32;
  CARD = sys.CARD32;

(*------------------------------------------------------------------------
	Nonexported types, constants and global variables.
------------------------------------------------------------------------*)
CONST
  comment_leadtab	   = 3;
  min_comment_length	   = 20;
  default_comment_position = 0;
  new_line                 = -2;
  ins_empty_line_before    = -3;
  new_line_and_position    = -4;
  size_output_str          = 1000; (*start size for each output line*)

  long_name_sep  = "_";
  universal_char = "_";

  definition_in_foregin_module = "It is defined in header ";

  (* Modula Types |   Default C types	*)
  int8		  (* signed  char	*) = 'SYSTEM.INT8';
  int		  (* signed int 	*) = 'SYSTEM.int';
  int16 	  (* signed short int	*) = 'SYSTEM.INT16';
  int32 	  (* signed long int	*) = 'SYSTEM.INT32';
  card8 	  (* unsigned char	*) = 'SYSTEM.CARD8';
  unsigned	  (* unsigned int	*) = 'SYSTEM.unsigned';
  card16	  (* unsigned short int *) = 'SYSTEM.CARD16';
  card32	  (* unsigned long int	*) = 'SYSTEM.CARD32';
  real		  (* float		*) = 'REAL';
  longreal	  (* double		*) = 'LONGREAL';

  (* Unreal Modula Types *)
  long_float	  (* long float   *) = 'LONGFLOAT';
  long_double	  (* long double  *) = 'LONGDOUDLE';
  void		  (* void	  *)  = 'SYSTEM.void';

  (* Modula & XDS base types *)
  m2_INTEGER   = "INTEGER";
  m2_SHORTINT  = "SHORTINT";
  m2_LONGINT   = "LONGINT";
  m2_CARDINAL  = "CARDINAL";
  m2_SHORTCARD = "SHORTCARD";
  m2_LONGCARD  = "LONGCARD";
  m2_CHAR      = "CHAR";
  m2_BITSET    = "BITSET";
  m2_BOOLEAN   = "BOOLEAN";
  m2_REAL      = "REAL";
  m2_LONGREAL  = "LONGREAL";
  m2_BYTE      = "SYSTEM.BYTE";
  m2_BOOL8     = "SYSTEM.BOOL8";
  m2_BOOL16    = "SYSTEM.BOOL16";
  m2_BOOL32    = "SYSTEM.BOOL32";
  m2_INT8      = "SYSTEM.INT8";
  m2_INT16     = "SYSTEM.INT16";
  m2_INT32     = "SYSTEM.INT32";
  m2_CARD8     = "SYSTEM.CARD8";
  m2_CARD16    = "SYSTEM.CARD16";
  m2_CARD32    = "SYSTEM.CARD32";
  m2_SET8      = "SYSTEM.SET8";
  m2_SET16     = "SYSTEM.SET16";
  m2_SET32     = "SYSTEM.SET32";
  m2_int       =  "SYSTEM.int";
  m2_unsigned  =  "SYSTEM.unsigned";

  (* XDS key Statemets *)
  new_gentypedef_p = '<* IF NOT DEFINED(GENTYPEDEF) THEN *> <* NEW GENTYPEDEF+ *> <* END *>';
  new_gentypedef_m = '<* IF NOT DEFINED(GENTYPEDEF) THEN *> <* NEW GENTYPEDEF- *> <* END *>';
  gentypedef_p	 = '<*+ GENTYPEDEF *>';
  gentypedef_m	 = '<*- GENTYPEDEF *>';
  noheader	 = '<*+ NOHEADER *> ';
  cstdlib	 = '<*+ CSTDLIB *>';
  m2extensions	 = '<*+ M2EXTENSIONS *>';
  m2addtypes	 = '<*+ M2ADDTYPES *>';

  (*  Modula key words and statements  *)
  kw_array_of_byte = 'ARRAY OF SYSTEM.BYTE ';
  cdecl       = '["C"]';
  pascalcall  = '["StdCall"]';
  pascal      = '["Pascal"]';
  syscall     = '["SysCall"]';

  (*  Generator's commentary  *)
  comment_for_macro = ' H2D: this procedure corresponds to a macro. ';
  import_comment    = ' Required IMPORT clause: ';
  h2d_comment	    = ' H2D: ';


  (* Type of  generate objects *)
  Generate_Objects_base = 100 + obj.last_number;
  obj_procedure = 1 + Generate_Objects_base;
  obj_const	= 2 + Generate_Objects_base;
  obj_type	= 3 + Generate_Objects_base;
  obj_var	= 4 + Generate_Objects_base;
  obj_macro	= 5 + Generate_Objects_base;

  (* results of check Modula name *)
  Check_Modula_Name_base = 200 + obj.last_number;
  all_right	  = 1 + Check_Modula_Name_base;
  bad_identifier  = 2 + Check_Modula_Name_base;
  consist_path	  = 3 + Check_Modula_Name_base;
  not_present	  = 4 + Check_Modula_Name_base;

  (* Other option *)
  Other_Option_base = 300 + obj.last_number;
  none	  = 1 + Other_Option_base;

  (* UniqueTagName return value *)
  Unique_Tag_Name_base = 400 + obj.last_number;
  no		  = 1 + Unique_Tag_Name_base;
  yes		  = 2 + Unique_Tag_Name_base;
  exist_synonym   = 3 + Unique_Tag_Name_base;

  (* Types of modifier *)
  Modifier_Type_base = 500 + obj.last_number;
  language_mod	   = 1 + Modifier_Type_base; (* cdecl, fortran, pascal*)
  memory_model_mod = 2 + Modifier_Type_base; (* far, near, huge *)
  assign_mod	   = 3 + Modifier_Type_base; (* const, volatil *)
  all_mod	   = 4 + Modifier_Type_base;
  func_mod	   = 5 + Modifier_Type_base; (* interrupt, syscall *)

  (* Types of output modules *)
  Type_Module_base = 600 + obj.last_number;
  std_def_module   = 1 + Type_Module_base; (* basic def module       *)
  macro_def_module = 2 + Type_Module_base; (* additional def module for
					      macros relised in native code *)
  macro_mod_module = 3 + Type_Module_base; (* implementation module for
					      macro_def_mod	     *)

  (* Other *)
  Other_base = 700 + obj.last_number;
  struct_already_denerated = 0 + Other_base;


TYPE
  FILE = POINTER TO FILEDesc;
  FILEDesc = RECORD
    type	 : INT; 	(* see Type_Module *)
    hand	 : fio.FILE;
    path	 : lstr.String;
    name, modname: lstr.String;
    line, pos	 : INT;
    header	 : obj.Header;
    creatednames : adt.Tree;    (* names are created during generation   *)
    created	 : BOOLEAN;	(* need for delete def file if was error *)
    used	 : BOOLEAN;	(* need for delete def & mod files if
				   wasn't macros                         *)
    obj_line	 : INT; 	(* position of first letter of name	 *)
    tab 	 : INT; 	(* tab of last output line		 *)
    emptyLine	  : BOOLEAN;	(* Just wrote empty line	       *)
  END;


  HFILE  = POINTER TO HFILEDesc;
  HFILEDesc  = RECORD
    hand     : fio.FILE;
    name     : lstr.String;
    created  : BOOLEAN;
    define   : lstr.String;
    line     : INT;
    emptyLine: BOOLEAN;
  END;


  OutString = POINTER TO OutStringDesc;
  OutStringDesc = RECORD (adt.ElementDesc)
    str 	 : lstr.String;
    comment	 : adt.List;
    leadtab, tab : INT;
    line	 : INT;
    backend	 : INT;
  END;

  GeneratedType = POINTER TO GeneratedTypeDesc;
  GeneratedTypeDesc = RECORD (adt.NamedElementDesc)
    type: obj.Type;
  END;

  PrintString = POINTER TO PrintStringDesc;
  PrintStringDesc = RECORD (adt.ElementDesc)
    str          : lstr.String;
    leadtab, tab : INT;
    ln           : BOOLEAN;
  END;

VAR
  ModuleSystem: obj.Header;
  OutFile      : FILE;	 (* current output Modula-2 file *)
  HFile        : HFILE;
  StdDefFile   : FILE;   (* basic DEF file      *)
  MacroDefFile : FILE;	 (* DEF file for macros *)
  MacroModFile : FILE;	 (* MOD file for macro	*)
  divideCharsForProcedure: lstr.String;
  nothingDivideChars	 : lstr.String;
  stdDivideChars	 : lstr.String;
  emptyString		 : lstr.String;
  GenTypeDef    : BOOLEAN;     (* On or Off XDS option                *)
  BackEndSection: INT;         (* native, c_code, common              *)
  --  What object section generated
  OSectionNative : INT;        -- in native backend
  OSectionCommon : INT;        -- in common backend
  OSectionC_Code : INT;        -- in c_cobe backend
  ObjectSection  : INT;        -- independet from backend
  GeneratedTypes: adt.Tree;    (* Consist of types which have name
                                  in header file  //obj.type          *)

  comment_was_just_generated		 : BOOLEAN;
  comment_was_just_generated_in_same_line: BOOLEAN;
  InsertEmptyLine   : BOOLEAN;
  comment_position  : INT;
  last_string_length: INT;
  NameOfGeneratedType: adt.Stack; (* need for comment in enumerated type *)
  ValueHandlerOn    : BOOLEAN;  (* for generate const during compute
                                   expression*)
  declaration_without_definition: obj.Comment;
  simple_comment    : obj.Comment; (* need for tag defined in foreign heade *)

  Modula2KeyWords: adt.Tree;
  Import   : adt.List;  -- список импорта
  AdtImport: adt.List;  -- имена модулей для типов встречающихся в
                        -- директиве #variant
  TagDefinedInOtherHeader: adt.List; (* list of tag defined in other header
                                        and declared here *)

  ListForExtractNamedTypes: adt.List;

  -- Output
  WriteBuffer: adt.List; -- буфер для native section в случае common backend'a
  C_CodeSectionOpen: BOOLEAN; -- выводилась ли в файл последней C_CodeSection
  WriteStrLn: PROCEDURE( str-: ARRAY OF CHAR; VAR pstr: lstr.String;
                         leadtab, tab: INT );
  WriteStr  : PROCEDURE( str-: ARRAY OF CHAR; VAR pstr: lstr.String;
                         leadtab, tab: INT );
  null: lstr.String;


(*------------------------------------------------------------------------*)
(*                    Temporary variables                                 *)
(*------------------------------------------------------------------------*)
VAR BlankString: lstr.String;


(*------------------------------------------------------------------------*)
(*			FORWARD  DECLARATION				  *)
(*------------------------------------------------------------------------*)


PROCEDURE ^ GetType ( type: obj.Type ): obj.Type;

PROCEDURE ^ MakeObjectType( tobj: obj.TypedObject; VAR outStr: lstr.String );

PROCEDURE ^ CheckName(ne: adt.NamedElement);

PROCEDURE ^ NewOutString ( VAR outStr: OutString );

PROCEDURE ^ HGenType ( type: obj.Type; buffer: adt.List; VAR tail: lstr.String );

PROCEDURE ^ GenConstant( tobj: obj.TypedObject );

PROCEDURE ^ RemoveOutStringsBuffer(VAR b: adt.List);

PROCEDURE ^ RemoveNamedElementsList(VAR l: adt.List);

PROCEDURE ^ RemoveGeneratedTypeTree( VAR tree: adt.Tree );

PROCEDURE ^ RemoveFILE(VAR f: FILE);

(*------------------------------------------------------------------------*)
(*		       COMMON USED RPOCEDURES				  *)
(*------------------------------------------------------------------------*)
(*
VAR crash_ptr: lstr.String;
PROCEDURE crash;
BEGIN
  lstr.Assign(crash_ptr^, crash_ptr);
END crash;
*)

(*------------------------------------------------------------------------*)
VAR GeneratedTypeStorage: adt.List;
    OutStringStorage    : adt.List;
    PrintStringStorage  : adt.List;

PROCEDURE Deallocate(e: adt.Element);
VAR e1: adt.Element;
BEGIN
  IF e = NIL THEN RETURN END;
  WITH
     e: GeneratedType DO
       GeneratedTypeStorage.Insert(e);
    |e: OutString DO
       e.comment.FindFirst(e1);
       WHILE e1 # NIL DO
         WITH
            e1: obj.Comment DO
           |e1: adt.NamedElement DO
             adt.Deallocate(e1);
         END;
         e.comment.FindNext(e1);
       END;
       e.comment.Clean();
       OutStringStorage.Insert(e);
    |e: PrintString DO
       lstr.Deallocate(e.str);
       PrintStringStorage.Insert(e);
  ELSE
  END;
END Deallocate;

(*------------------------------------------------------------------------*)
PROCEDURE Assign(src-: ARRAY OF CHAR; VAR dest: lstr.String);
BEGIN
  lstr.Allocate( dest, size_output_str );
  lstr.Assign( src, dest );
END Assign;

PROCEDURE BlendLists( from: adt.List; VAR to: adt.List );
(* If element from 'from' can be compare with other elements and
   it present in 'to' then one doesn't placed in 'to'
*)
VAR elm, found: adt.Element;
    id: INT;
BEGIN
  IF to = NIL THEN adt.NewList( to ) END;
  IF from = NIL THEN RETURN END;
  from.FindFirst( elm );
  WHILE elm # NIL DO
    from.Backup( id );
    to.Find( elm, found );
    IF found = NIL THEN  to.Insert( elm ) END;
    from.Restore( id );
    from.FindNext( elm );
  END;
END BlendLists;

(*--------------------------------------------------------------------*)
PROCEDURE ListToTree ( from: adt.List; to: adt.Tree );
VAR elm: adt.Element;
BEGIN
  IF to = NIL THEN adt.NewTree( to )  END;
  IF from = NIL THEN RETURN END;
  from.FindFirst( elm );
  WHILE elm # NIL DO
    IF ( elm IS adt.NamedElement ) &
       ( lstr.Length1(elm(adt.NamedElement).name) > 0 ) THEN
      to.Insert( elm );
    END;
    from.FindNext( elm );
  END;
END ListToTree;

(*--------------------------------------------------------------------*)
PROCEDURE SpaceStr ( VAR str: lstr.String; number: INT );
(* Create string consist space sumbols *)
BEGIN
  Assign('', str);
  WHILE number > 0 DO
    lstr.AppendChar(' ', str);
    DEC(number);
  END;
END SpaceStr;

(*--------------------------------------------------------------------*)
PROCEDURE IntToStr ( int: INT; VAR str: lstr.String );
VAR intStr: lstr.String;
BEGIN
  lstr.Allocate(intStr, (int DIV 10)+2);
  WholeStr.IntToStr(int, intStr^);
  lstr.Append(intStr^, str);
  lstr.Deallocate(intStr);
END IntToStr;

(*------------------------------------------------------------------------*)
PROCEDURE NewPrintString( VAR prStr: PrintString; leadtab, tab: INT;
                          ln: BOOLEAN );
VAR e: adt.Element;
BEGIN
  IF PrintStringStorage.IsEmpty() THEN  NEW(prStr);
  ELSE
    PrintStringStorage.FindFirst(e);
    PrintStringStorage.DeleteCurrent();
    prStr:= e(PrintString);
  END;
  prStr.leadtab:= leadtab;
  prStr.tab    := tab;
  prStr.ln     := ln;
END NewPrintString;

(*------------------------------------------------------------------------*)
PROCEDURE ExtractModName( src-: ARRAY OF CHAR; VAR m_name: lstr.String );
VAR pos: INT;
BEGIN
  pos:= lstr.FindPos(src, ".");
  IF pos > 0 THEN
    lstr.Extract(src, 0, pos, m_name);
  END;
END ExtractModName;

(*------------------------------------------------------------------------*)
PROCEDURE GetTime ( VAR outStr: lstr.String );
VAR data: clock.DateTime;

  (*----------------------*)
  PROCEDURE MakeMonth ( month: INT );
  BEGIN
    CASE month OF
       1 : lstr.Append( 'Jan', outStr );
      |2 : lstr.Append( 'Feb', outStr );
      |3 : lstr.Append( 'Mar', outStr );
      |4 : lstr.Append( 'Apr', outStr );
      |5 : lstr.Append( 'May', outStr );
      |6 : lstr.Append( 'Jun', outStr );
      |7 : lstr.Append( 'Jul', outStr );
      |8 : lstr.Append( 'Aug', outStr );
      |9 : lstr.Append( 'Sep', outStr );
      |10: lstr.Append( 'Oct', outStr );
      |11: lstr.Append( 'Nov', outStr );
      |12: lstr.Append( 'Dec', outStr );
    END;
  END MakeMonth;
(*----------------------*)
BEGIN
  IF clock.CanGetClock() THEN
    clock.GetClock( data );
    Assign( '  ', outStr );

    MakeMonth( data.month );
    lstr.Append( ' ', outStr );
    IntToStr( data.day, outStr );
    lstr.Append( '  ', outStr );

    IntToStr( data.hour, outStr );
    lstr.AppendChar( ':', outStr );
    IntToStr( data.minute, outStr );
    lstr.AppendChar( ':', outStr );
    IntToStr( data.second, outStr );
    lstr.Append( '  ', outStr );

    IntToStr( data.year, outStr );
  END;
END GetTime;



(*------------------------------------------------------------------------*)
(*			 ERRORS AND WARNINGS				  *)
(*------------------------------------------------------------------------*)
(*
PROCEDURE Warning( number: INT; SEQ contens: sys.BYTE );
BEGIN
  IF OutFile.header = NIL THEN
    msg.File:= OutFile.name;
    msg.Line:= OutFile.line;
    msg.Pos := OutFile.pos;
    msg.Warning( number, contens );
  ELSE
    OutFile.header.stat.Warning( number, OutFile.name^, OutFile.line,
                                 OutFile.pos, contens );
  END;
END Warning;
*)

(*------------------------------------------------------------------------*)
PROCEDURE Error (number: INT; SEQ contens: sys.BYTE );
BEGIN
  IF OutFile.header = NIL THEN
    msg.File:= OutFile.name;
    msg.Line:= OutFile.line;
    msg.Pos := OutFile.pos;
    msg.Error( number, contens );
  ELSE
    OutFile.header.stat.Error( number, OutFile.name^, OutFile.line,
                               OutFile.pos, contens );
  END;
END Error;


(*------------------------------------------------------------------------*)
PROCEDURE HError(number: INT; object: obj.Object; SEQ contens: sys.BYTE );
BEGIN
  IF OutFile.header = NIL THEN
    msg.File:= object.file;
    msg.Line:= object.line;
    msg.Pos := 0;
    msg.Error( number, contens );
  ELSE
    OutFile.header.stat.Error( number, object.file^,
                               object.line, 0, contens );
  END;
END HError;

(*------------------------------------------------------------------------*)
(*		     GENERATOR OF HEADER FILES				  *)
(*------------------------------------------------------------------------*)

PROCEDURE HWriteStr ( str-: ARRAY OF CHAR; leadtab, tab: INT );
BEGIN
  HFile.line:= fio.FWriteStr( HFile.hand, str, leadtab, tab ) + HFile.line;
  HFile.emptyLine:= FALSE;
END HWriteStr;

(*------------------------------------------------------------------------*)
PROCEDURE HWriteStrLn ( str-: ARRAY OF CHAR; leadtab, tab: INT );
BEGIN
  HFile.line:= fio.FWriteStrLn( HFile.hand, str, leadtab, tab ) + HFile.line;
  HFile.emptyLine:= FALSE;
END HWriteStrLn;

(*------------------------------------------------------------------------*)
PROCEDURE HWriteEmptyLine ();
BEGIN
  IF HFile.emptyLine THEN  RETURN  END;
  HWriteStrLn( '', 0, 0 );
  HFile.emptyLine:= TRUE;
END HWriteEmptyLine;

(*------------------------------------------------------------------------*)
PROCEDURE GetRealHFileName ( VAR outStr: lstr.String );
VAR str: lstr.String;
BEGIN
  Assign( OutFile.header.name^, str );
  IF OutFile.header.cstdlib THEN
    lstr.Insert( '<', 0, str );
    lstr.Append( '>', str );
  ELSE
    lstr.Insert( '"', 0, str );
    lstr.Append( '"', str );
  END;
  lstr.Append( str^, outStr ); lstr.Deallocate(str);
END GetRealHFileName;

(*------------------------------------------------------------------------*)
PROCEDURE GenHFileTitle ();
VAR str: lstr.String;
BEGIN
  str:= NIL; GetTime( str );
  lstr.Insert( HFile.name^, 0, str );
  lstr.Insert( '/* ', 0, str );
  lstr.Append( ' */', str );
  HWriteStr( str^, 0, 0 );

  Assign( '/* Generated by H2D', str );
  lstr.Append( msg.version, str );
  lstr.Append( '*/', str );
  HWriteStrLn( str^, 0, 0 );
  Assign( '/* from module ', str );
  lstr.Append( OutFile.modname^, str );
  lstr.Append( ' and header ', str );
  lstr.Append( OutFile.header.name^, str );
  lstr.Append( '.', str );
  lstr.Append( ' */', str );
  HWriteStrLn( str^, 0, 0 );
  HWriteStrLn( '/* Types which were created during translation */', 0, 0 );
  HWriteStrLn( '', 0, 0 );

  Assign('#include ', str );
  GetRealHFileName( str );
  IF ~OutFile.header.preheader THEN
    HWriteStrLn( str^, 0, 0 );
  END;
  HWriteStrLn( '', 0, 0 );

  Assign( '#ifndef ', str );
  lstr.Append( HFile.define^, str );
  HWriteStrLn( str^, 0, 0 );
  Assign( '#define ', str );
  lstr.Append( HFile.define^, str );
  HWriteStrLn( str^, 0, 0 ); lstr.Deallocate(str);
  HWriteStrLn( '', 0, 0 );
END GenHFileTitle;

(*-------------------------------------------------------------------*)
PROCEDURE GenIncludeList();
VAR str: lstr.String;
    elm: adt.Element;
BEGIN
  AdtImport.FindFirst(elm);
  WHILE elm # NIL DO
    lstr.Assign('#include "', str);
    lstr.Append(elm(adt.NamedElement).name^, str);
    lstr.Append('.h"', str);
    HWriteStrLn( str^, 0, 0 );
    AdtImport.FindNext(elm);
  END;
  HWriteStrLn( '', 0, 0 );
  lstr.Deallocate(str);
END GenIncludeList;

(*-------------------------------------------------------------------*)
PROCEDURE NewHFILE ();
VAR  ext, path: lstr.String;
BEGIN
  IF msg.WasError THEN RETURN END;
  NEW( HFile );
  HFile.line:= 0;
  HFile.emptyLine:= FALSE;
  fio.SplitName( OutFile.header.name^, path, HFile.name, ext );
  IF fio.IsAbsolutePath( path^ ) THEN
    fio.CreateName( '', OutFile.modname^, cfg.HeaderExtension^, HFile.name );
  ELSE
    fio.CreateName( path^, OutFile.modname^, cfg.HeaderExtension^, HFile.name );
  END;

  lstr.Assign( OutFile.modname^, HFile.define );
  lstr.Append( '_H_', HFile.define );

  IF ( fio.Open(HFile.name^, fio.crmode, HFile.hand) ) THEN
    HFile.created:= TRUE;
  ELSE
    Error(6, HFile.name^);
    RETURN;
  END;
  GenHFileTitle(); lstr.Deallocate(ext); lstr.Deallocate(path);
  GenIncludeList();
END NewHFILE;


(*-------------------------------------------------------------------*)
PROCEDURE CloseHFile ();
VAR done: BOOLEAN;
    str: lstr.String;
BEGIN
  IF HFile = NIL THEN RETURN END;
  IF HFile.created THEN
    Assign( '#endif ', str );
    lstr.Append( ' /* ', str );
    lstr.Append( HFile.define^, str );
    lstr.Append( ' */', str );
    HWriteStrLn( '', 0, 0 );
    HWriteStrLn( str^, 0, 0 ); lstr.Deallocate(str);
    fio.Close( HFile.hand );
    IF msg.WasError  THEN
      fio.Delete( HFile.name^ , done );
    END;
  END;
  HFile:= NIL;
END CloseHFile;


(*-------------------------------------------------------------------*)
PROCEDURE HGenDefine( what-, how-: ARRAY OF CHAR );
BEGIN
  IF (HFile = NIL) THEN RETURN END;
  fio.UnlimitedSizeOutputLine:= TRUE;
  HWriteStrLn( "#define ", 0, 0 );
  HWriteStr( what, 0, 0 );
  HWriteStr( how, 2, 0 );
  fio.UnlimitedSizeOutputLine:= FALSE;
END HGenDefine;

(*-------------------------------------------------------------------*)
PROCEDURE HMakeQualident( type: obj.Type; VAR str: lstr.String );
BEGIN
  lstr.Append( type.name^, str );
END HMakeQualident;

(*-------------------------------------------------------------------*)
PROCEDURE HMakeModifier ( modifier: adt.List; VAR str: lstr.String;
			  mode: INT );
VAR elm: adt.Element;
    id: INT;
BEGIN
  modifier.FindFirst ( elm );
  WHILE elm # NIL DO
    modifier.Backup( id );
    IF (mode = language_mod) OR (mode = all_mod) THEN
      CASE elm(obj.Modifier).modifier OF
	obj.pascal  : lstr.Append( ' pascal' , str );
       |obj.cdecl   : lstr.Append( ' cdecl'  , str );
       |obj.fortran : lstr.Append( ' fortran', str );
      ELSE
      END;
    END;
    IF (mode = memory_model_mod) OR (mode = all_mod) THEN
      CASE elm(obj.Modifier).modifier OF
	obj.far  : lstr.Append( ' far' , str );
       |obj.near : lstr.Append( ' near', str );
       |obj.huge : lstr.Append( ' huge', str );
      ELSE
      END;
    END;
    IF (mode = assign_mod) OR (mode = all_mod) THEN
      CASE elm(obj.Modifier).modifier OF
	obj.const    : lstr.Append( ' const'   , str );
       |obj.volatile : lstr.Append( ' volatile', str );
      ELSE
      END;
    END;
    IF (mode = func_mod) OR (mode = all_mod) THEN
      CASE elm(obj.Modifier).modifier OF
	obj.interrupt : lstr.Append( ' interrupt'   , str );
       |obj.syscall   : lstr.Append( ' syscall', str );
      ELSE
      END;
    END;

    modifier.Restore( id );
    modifier.FindNext( elm );
  END;
END HMakeModifier;

(*-------------------------------------------------------------------*)
PROCEDURE HMakeBaseType ( type: obj.Type; buffer: adt.List; VAR tail: lstr.String);
VAR elm: adt.Element;
    outStr: PrintString;
BEGIN
  buffer.FindLast( elm );
  outStr:= elm(PrintString);
  HMakeModifier( type.modifier, outStr.str, assign_mod );
  CASE type.type OF
     obj.bt_s_char   :	lstr.Append( ' signed char'       , outStr.str );
    |obj.bt_s_int    :	lstr.Append( ' signed int'        , outStr.str );
    |obj.bt_s_sh_int :	lstr.Append( ' signed short int'  , outStr.str );
    |obj.bt_s_l_int  :	lstr.Append( ' signed long int'   , outStr.str );
    |obj.bt_u_char   :	lstr.Append( ' unsigned char'     , outStr.str );
    |obj.bt_u_int    :	lstr.Append( ' unsigned int'      , outStr.str );
    |obj.bt_u_sh_int :	lstr.Append( ' unsigned short int', outStr.str );
    |obj.bt_u_l_int  :	lstr.Append( ' unsigned long int' , outStr.str );
    |obj.bt_float    :	lstr.Append( ' float'             , outStr.str );
    |obj.bt_double   :	lstr.Append( ' double'            , outStr.str );
    |obj.bt_l_float  :	lstr.Append( ' long float'        , outStr.str );
    |obj.bt_l_double :	lstr.Append( ' long double'       , outStr.str );
    |obj.bt_void     :	lstr.Append( ' void'              , outStr.str );
    --|obj.bt_bitset   :  lstr.Append( ' unsigned long int' , outStr.str );
  END;
  HMakeModifier( type.modifier, outStr.str, language_mod );
  Assign('', tail);
END HMakeBaseType;

(*-------------------------------------------------------------------*)
PROCEDURE HMakeSynonym ( type: obj.Type; buffer: adt.List; VAR tail: lstr.String);
VAR elm: adt.Element;
    outStr: PrintString;
BEGIN
  buffer.FindLast( elm );
  outStr:= elm(PrintString);
  Assign('', tail);
  HMakeModifier( type.modifier, outStr.str, assign_mod );
  IF lstr.Length1( type.name ) > 0 THEN
    lstr.Append( ' ', outStr.str);
    HMakeModifier( type.modifier, outStr.str, language_mod );
    HMakeQualident( type, outStr.str );
  ELSE
    Error(28, 'HMakeSynonym');
  END;
END HMakeSynonym;

(*-------------------------------------------------------------------*)
PROCEDURE HMakeArrayType( type: obj.Type; buffer: adt.List; VAR tail: lstr.String);
VAR elm: adt.Element;
    outStr: PrintString;


  (*-------------------------------*)
  PROCEDURE NamberArrayElement();
  VAR result: obj.ConstantValue;
      s: lstr.String;
  BEGIN
    IF type.expr = NIL THEN RETURN END;
    type.expr.ComputeExpression( result, FALSE );
    IF result.type = obj.int_const THEN
      s := result.GetText(FALSE);
      lstr.Insert( s^, 0, tail );
    ELSE
      lstr.Append( ' illegal size of array', outStr.str );
      Error(28, 'H2DGen.HMakeArrayType.NumberArrayElement');
    END;
    obj.Deallocate(result);
  END NamberArrayElement;

(*-------------------------------*)
BEGIN
  HGenType( type.base, buffer, tail );
  IF msg.WasError THEN RETURN END;
  buffer.FindLast( elm );
  outStr:= elm(PrintString);
  IF ( lstr.Length1( tail ) > 0 ) & ( type.base.type # obj.t_array ) THEN
    lstr.Append( '(', outStr.str );
    lstr.Insert( ')', 0, tail );
  END;
  HMakeModifier( type.modifier, outStr.str, all_mod );
  lstr.Insert( ']', 0, tail );
  NamberArrayElement();
  IF msg.WasError THEN RETURN END;
  lstr.Insert( '[', 0, tail );

END HMakeArrayType;


(*-------------------------------------------------------------------*)
PROCEDURE HMakeFuncType( type: obj.Type; buffer: adt.List; VAR tail: lstr.String);
VAR elm: adt.Element;
    outStr: PrintString;
    id: INT;


  (*----------------------------------*)
  PROCEDURE MakeOneArguments( arg: obj.TypedObject );
  VAR localBuffer: adt.List;
      localTail: lstr.String;
      loutStr: PrintString;
      lelm: adt.Element;
      id: INT;
  BEGIN
    NewPrintString( loutStr, 0, 0, TRUE );
    adt.NewList( localBuffer );
    localBuffer.Insert( loutStr  );
    Assign( '', loutStr.str );

    IF arg.obj = obj.arguments THEN
      Assign( ' ...', loutStr.str );
    ELSE
      HGenType( arg.type, localBuffer, localTail );
    END;
    IF msg.WasError THEN RETURN  END;
    localBuffer.FindLast( lelm );
    localBuffer.Backup( id );
    loutStr:= lelm(PrintString);
    IF lstr.Length1( arg.name ) > 0 THEN
      lstr.Insert( arg.name^, 0, localTail );
      lstr.Insert( ' ', 0, localTail );
    END;
    lstr.Insert( loutStr.str^, 0, localTail );
    localBuffer.Restore( id );
    localBuffer.FindPrev( lelm );
    WHILE lelm # NIL DO
      localBuffer.Backup( id );
      lstr.Insert( lelm(PrintString).str^, 0, localTail );
      localBuffer.Restore( id );
      Deallocate( lelm(PrintString) );
      localBuffer.FindPrev( lelm );
    END; localBuffer.Clean(); adt.Deallocate(localBuffer);
    lstr.Insert( localTail^, 0, tail ); lstr.Deallocate(localTail);
  END MakeOneArguments;

(*----------------------------------*)
BEGIN
  buffer.FindLast( elm );
  outStr:= elm(PrintString);
  HGenType( type.base, buffer, tail );
  IF msg.WasError THEN RETURN END;

  IF lstr.Length1(tail) > 0 THEN
    lstr.Append( '(', outStr.str );
    lstr.Insert( ')', 0, tail );
  END;

  HMakeModifier( type.modifier, outStr.str, all_mod );

  lstr.Insert( ' )', 0, tail );
  type.vars.FindLast( elm );
  WHILE (elm # NIL) & ~(elm IS obj.TypedObject) DO
    type.vars.FindPrev( elm );
  END;
  IF elm # NIL THEN
    type.vars.Backup( id );
    MakeOneArguments( elm(obj.TypedObject) );
    IF msg.WasError THEN RETURN END;
    type.vars.Restore( id );
    type.vars.FindPrev( elm );
    WHILE elm # NIL DO
      type.vars.Backup( id );
      WITH elm: obj.TypedObject DO
        lstr.Insert( ',', 0, tail );
	MakeOneArguments( elm );
        IF msg.WasError THEN RETURN END;
      ELSE
      END;
      type.vars.Restore( id );
      type.vars.FindPrev( elm );
    END;
  END;
  lstr.Insert( '(', 0, tail );
END HMakeFuncType;

(*-------------------------------------------------------------------*)
PROCEDURE HMakePointerType( type: obj.Type; buffer: adt.List; VAR tail: lstr.String );
VAR elm: adt.Element;
    outStr: PrintString;
    modifier: adt.List;

BEGIN
  buffer.FindLast( elm );
  outStr:= elm(PrintString);
  HMakeModifier( type.modifier, outStr.str, assign_mod );
  HGenType( type.base, buffer, tail );
  IF msg.WasError THEN RETURN END;

  buffer.FindLast( elm );
  outStr:= elm(PrintString);
  HMakeModifier( type.modifier, outStr.str, func_mod );
  IF lstr.Length1( tail ) > 0 THEN
    lstr.Append( '(', outStr.str );
    lstr.Insert( ')', 0, tail );
  END;
  HMakeModifier( type.modifier, outStr.str, memory_model_mod );
  IF type.ptr_modifier.modifier # obj.nonmodified THEN
    adt.NewList( modifier );
    modifier.Insert( type.ptr_modifier );
    HMakeModifier( modifier, outStr.str, all_mod );
  END;
  lstr.Append( ' *', outStr.str );
  HMakeModifier( type.modifier, outStr.str, language_mod );
END HMakePointerType;


(*-------------------------------------------------------------------*)
PROCEDURE HMakeStructUnionType( type: obj.Type; buffer: adt.List;
                                VAR tail: lstr.String);
VAR elm: adt.Element;
    outStr: PrintString;
    id, leadtab, tab: INT;

  (*------------------------------------*)
  PROCEDURE MakeField ( field: obj.TypedObject );
  VAR tail, s: lstr.String;
      loutStr: PrintString;
      elm: adt.Element;
      result: obj.ConstantValue;
  BEGIN
    CASE field.obj OF
      obj.variable :
	buffer.Insert( outStr );
        HGenType( field.type, buffer, tail );
	buffer.FindLast( elm );
        loutStr:= elm(PrintString);
	lstr.Append( ' ', loutStr.str );
	lstr.Append( field.name^, loutStr.str );
	lstr.Append( tail^, loutStr.str );
     |obj.bit_field:
	buffer.Insert( outStr );
        HGenType( field.type, buffer, tail );
	buffer.FindLast( elm );
        loutStr:= elm(PrintString);
	lstr.Append( ' ', loutStr.str );
	IF lstr.Length1( field.name ) > 0 THEN
	  lstr.Append( field.name^, loutStr.str );
	  lstr.Append( ' ', loutStr.str );
	END;
	lstr.Append( ': ', loutStr.str );
	field.expr.ComputeExpression( result, FALSE );
	IF result.type = obj.int_const THEN
          s := result.GetText(FALSE);
	  lstr.Append( s^, loutStr.str );
          obj.Deallocate(result);
	ELSE
          obj.Deallocate(result);
          lstr.Append( ' illegal size of array', loutStr.str );
	  Error(28, 'H2DGen.HMakeStructType.MakeField');
          RETURN;
	END;
	lstr.Append( tail^, loutStr.str );
    ELSE
      Error(28, 'H2DGen.HMakeStructType');
      RETURN;
    END;
    lstr.Append( ';', loutStr.str );
  END MakeField;

(*------------------------------------*)
BEGIN
  buffer.FindLast( elm );
  outStr:= elm(PrintString);
  leadtab:= outStr.leadtab - 7;
  tab:= outStr.tab;
  Assign('', tail);
  IF leadtab < 0 THEN  leadtab:= 0  END;
  HMakeModifier( type.modifier, outStr.str, assign_mod );
  IF type.type = obj.t_struct THEN
    lstr.Append( ' struct', outStr.str );
  ELSE
    lstr.Append( ' union', outStr.str );
  END;
  IF lstr.Length1(type.name) > 0 THEN
    lstr.Append( ' ', outStr.str );
    HMakeQualident( type, outStr.str );
    IF ~OutFile.header.preheader OR type.h_generated THEN
      RETURN;
    END;
    type.h_generated:= TRUE;
  END;
  lstr.Append( ' ', outStr.str );
  lstr.Append( '{', outStr.str );

  type.vars.FindFirst( elm );
  WHILE elm # NIL DO
    type.vars.Backup( id );
    NewPrintString( outStr, 0, 0, TRUE );
    outStr.leadtab:= leadtab+9;
    WITH elm: obj.TypedObject DO
      MakeField( elm );
      IF msg.WasError THEN RETURN END;
    ELSE
    END;
    type.vars.Restore( id );
    type.vars.FindNext( elm );
  END;

  NewPrintString( outStr, 0, 0, TRUE );
  outStr.leadtab:= leadtab+8;
  outStr.tab:= tab+4;
  Assign( '}', outStr.str );
  buffer.Insert( outStr );
END HMakeStructUnionType;


(*-------------------------------------------------------------------*)
PROCEDURE HMakeEnumType ( type: obj.Type; buffer: adt.List; VAR tail: lstr.String);
VAR elm: adt.Element;
    outStr: PrintString;
    id, prevValue: INT;

  (*-------------------------------*)
  PROCEDURE MakeEnumElm ( enum: obj.TypedObject );
  VAR result: obj.ConstantValue;
      value: lstr.String;
  BEGIN
    lstr.Append( ' ', outStr.str );
    lstr.Append( enum.name^, outStr.str );
    enum.expr.ComputeExpression( result, FALSE );
    IF result.type = obj.int_const THEN
      IF result.int # prevValue + 1 THEN
        value:= result.GetText(FALSE);
	lstr.Append( '=', outStr.str );
	lstr.Append( value^ , outStr.str );
	prevValue:= result.int;
      ELSE
	INC(prevValue);
      END;
    ELSE
      Error(28, 'H2DGen.MakeEnumConst');
    END;
    obj.Deallocate(result);
  END MakeEnumElm;

(*-------------------------------*)
BEGIN
  prevValue:= -1;
  buffer.FindLast( elm );
  outStr:= elm(PrintString);
  Assign('', tail);
  HMakeModifier( type.modifier, outStr.str, assign_mod );
  lstr.Append( ' enum', outStr.str );
  IF lstr.Length1(type.name) > 0 THEN
    lstr.Append( ' ', outStr.str );
    HMakeQualident( type, outStr.str );
    IF ~OutFile.header.preheader OR  type.h_generated THEN
      RETURN;
    END;
    type.h_generated:= TRUE;
  END;
  lstr.Append( ' ', outStr.str );
  lstr.Append( '{', outStr.str );

  type.vars.FindFirst( elm );
  type.vars.Backup( id );
  IF elm IS  obj.TypedObject THEN
    MakeEnumElm( elm(obj.TypedObject) );
  END;
  type.vars.Restore( id );
  type.vars.FindNext( elm );
  WHILE elm # NIL DO
    type.vars.Backup( id );
    lstr.Append( ',', outStr.str );
    IF elm IS  obj.TypedObject THEN
      MakeEnumElm( elm(obj.TypedObject) );
    END;
    type.vars.Restore( id );
    type.vars.FindNext( elm );
  END;
  lstr.Append( '}', outStr.str );
END HMakeEnumType;

(*-------------------------------------------------------------------*)
PROCEDURE HGenType ( type: obj.Type; buffer: adt.List; VAR tail: lstr.String );
BEGIN
  Assign( '', tail );
  CASE type.type OF
    --obj.bt_s_char..obj.bt_bitset: HMakeBaseType( type, buffer, tail );
    obj.bt_s_char..obj.bt_void: HMakeBaseType( type, buffer, tail );
   |obj.t_ptr     : HMakePointerType( type, buffer, tail );
   |obj.t_struct  : HMakeStructUnionType( type, buffer, tail );
   |obj.t_array   : HMakeArrayType( type, buffer, tail );
   |obj.t_union   : HMakeStructUnionType( type, buffer, tail );
   |obj.t_enum    : HMakeEnumType( type, buffer, tail );
   |obj.t_func    : HMakeFuncType( type, buffer, tail );
   |obj.t_synonym : HMakeSynonym( type, buffer, tail );
  ELSE
    Error(28, 'HGenType');

  END;
END HGenType;


(*-------------------------------------------------------------------*)
PROCEDURE HGenSynonym ( type: obj.Type );
(* must receive not only synonym *)
VAR outStr: PrintString;
    buffer: adt.List;
    elm: adt.Element;
    tail, ident: lstr.String;

  (*---------------------------*)
  PROCEDURE Ifndef ();
  BEGIN
    Assign( "_H2D_TYPE__", ident );
    lstr.Append( type.name^, ident );

    HWriteStrLn( "", 0, 0 );
    HWriteStrLn( "#ifndef ", 0, 0 );
    HWriteStr( ident^, 0, 0 );
    HWriteStrLn( "#define ", 0, 0 );
    HWriteStr( ident^, 0, 0 );

  END Ifndef;

  (*---------------------------*)
  PROCEDURE Endif ();
  BEGIN
    HWriteStrLn( "#endif  /* ", 0, 0 );
    HWriteStr( ident^, 0, 0 );
    HWriteStr( " */", 0, 0 );
    lstr.Deallocate( ident );
  END Endif;

  (*---------------------------*)
  PROCEDURE FlushBuffer ();
  VAR id: INT;
      elm: adt.Element;
  BEGIN
    Ifndef();
    HWriteEmptyLine();
    buffer.FindFirst( elm );
    WHILE elm # NIL DO
      buffer.Backup( id );
      WITH elm: PrintString DO
	 HWriteStrLn( elm.str^, elm.leadtab, elm.tab );
         Deallocate(elm);
      END;
      buffer.Restore( id );
      buffer.FindNext( elm );
    END;
    Endif();
    buffer.Clean(); adt.Deallocate(buffer);
  END FlushBuffer;

(*---------------------------*)
BEGIN
  IF type.h_generated OR (HFile = NIL) THEN RETURN END;
  adt.NewList( buffer );
  NewPrintString( outStr, 0, 0, TRUE );
  outStr.tab:= 8;
  buffer.Insert( outStr  );


  IF type.type = obj.t_synonym THEN
    Assign( 'typedef', outStr.str );
    type.h_generated:= TRUE;
    HGenType( type.base, buffer, tail );
  ELSE
    IF ~OutFile.header.preheader THEN
      Error(28, 'H2DGen.HMakeStructType.MakeField');
      RETURN;
    END;
    HGenType( type, buffer, tail );
    outStr.leadtab:= 0;
  END;
  IF msg.WasError THEN RETURN END;

  buffer.FindLast( elm );
  WITH elm: PrintString DO
    IF type.type = obj.t_synonym THEN
      lstr.Append( ' ', elm.str );
      lstr.Append( type.name^, elm.str );
    END;
    lstr.Append( tail^, elm.str );
    lstr.Append( ';', elm.str );
  END;
  FlushBuffer(); lstr.Deallocate(tail);
END HGenSynonym;



(*------------------------------------------------------------------------*)
(*	 PROCEDURES WHICH CHECK STRUCTURING COMPATIBLE			  *)
(*------------------------------------------------------------------------*)
PROCEDURE NewGeneratedType ( VAR node: GeneratedType; type: obj.Type );
VAR e: adt.Element;
    tmp: BOOLEAN;
BEGIN
  IF GeneratedTypeStorage.IsEmpty() THEN
    NEW( node );
  ELSE
    GeneratedTypeStorage.FindFirst(e);
    GeneratedTypeStorage.DeleteCurrent();
    node:= e(GeneratedType);
    lstr.Assign('', node.name);
  END;
  node.type:= type;
  tmp:= ValueHandlerOn;
  ValueHandlerOn:= FALSE;
  obj.TranslateTypeToString( type, node.name );
  ValueHandlerOn:= tmp;
END NewGeneratedType;

(*------------------------------------------------------------------------*)
PROCEDURE IsPointerToBaseType( type: obj.Type ): BOOLEAN;
BEGIN
  IF type = NIL THEN RETURN FALSE END;
  WHILE (type.type = obj.t_synonym) OR (type.type = obj.t_ptr) DO
    type:= type.base;
  END;
  IF (type.type IN obj.base_types) THEN RETURN TRUE;
  ELSE RETURN FALSE;
  END;
END IsPointerToBaseType;

(*------------------------------------------------------------------------*)
PROCEDURE CompareType ( type1, type2: obj.Type ): BOOLEAN;
VAR node1, node2: GeneratedType;
BEGIN
  NewGeneratedType( node1, type1 );
  NewGeneratedType( node2, type2 );
  RETURN node1.Compare( node2 ) = adt.equal;
END CompareType;

(*------------------------------------------------------------------------*)
PROCEDURE FindCompatibleType(pattern: obj.Type): obj.Type;
VAR elm: adt.Element;
    node: GeneratedType;
    type: obj.Type;
BEGIN
  IF pattern = NIL THEN
    Error(28, 'H2DGen.FindCompatibleType');  RETURN NIL;
  ELSIF pattern.translation_variant # NIL THEN RETURN pattern;
    (* если тип модифицирован пользователем, то совместимый с ним не ищем, д
       абы не править много других мест *)
  END;

  type:= NIL;
  NewGeneratedType( node, pattern );
  GeneratedTypes.Find( node, elm );
  IF elm # NIL THEN
    type:= elm(GeneratedType).type;
  END;
  Deallocate(node);
  RETURN type;
END FindCompatibleType;

(*------------------------------------------------------------------------*)
PROCEDURE ReplaceCompatibleType( pattern: obj.Type );
(* Never replaced types: base type (except void), vopi * and any
   pointer to base type.
   Pattern must be already generated, non created_by_back_end.
*)
VAR elm: adt.Element;
    node, findNode: GeneratedType;
BEGIN
  IF (pattern=NIL) OR ~((pattern.header # OutFile.header) OR pattern.generated )
     OR pattern.created_by_back_end
  THEN
    RETURN;
  END;
  NewGeneratedType( node, pattern );
  GeneratedTypes.Find( node, elm );
  IF elm # NIL THEN
    findNode:= elm(GeneratedType);
    IF (findNode.type.type IN obj.base_types) &
       (findNode.type.type # obj.bt_void)     &
       ~(IsPointerToBaseType(pattern)) &
       ((findNode.type.type # obj.t_ptr) OR (findNode.type.base.type # obj.bt_void))
    THEN
      findNode.type:= pattern;
    END;
  END;
  Deallocate(node);
END ReplaceCompatibleType;


(*------------------------------------------------------------------------*)
(*		     SET ENVIRONMENT OF GENERATOR			  *)
(*------------------------------------------------------------------------*)
PROCEDURE SetModula2KeyWords();
VAR ne: adt.NamedElement;

  PROCEDURE ins(w-: ARRAY OF CHAR);
  BEGIN
    NEW(ne); ne.SetName(w);
    Modula2KeyWords.Insert(ne);
  END ins;

BEGIN
  adt.NewTree(Modula2KeyWords);
  ins('AND');                 ins('MODULE');
  ins('ARRAY');               ins('NOT');
  ins('BEGIN');               ins('OF');
  ins('BY');                  ins('OR');
  ins('CASE');                ins('PACKEDSET');
  ins('CONST');               ins('PROCEDURE');
  ins('DEFINITION');          ins('QUALIFIED');
  ins('DIV');                 ins('RECORD');
  ins('EXIT');                ins('REM');
  ins('EXCEPT');              ins('REPEAT');
  ins('EXPORT');              ins('RETRY');
  ins('FINALLY');             ins('RETURN');
  ins('FOR');                 ins('SET');
  ins('FORWARD');             ins('THEN');
  ins('FROM');                ins('TO');
  ins('IF');                  ins('TYPE');
  ins('IMPLEMENTATION');      ins('UNTIL');
  ins('IMPORT');              ins('VAR');
  ins('IN');                  ins('WHILE');
  ins('LOOP');                ins('WITH');
  ins('MOD');                 ins('IS');
  ins('DO');

  (* predeclared identifiers Modula-2 and Oberon-2 *)
  ins('ABS');                 ins('ASH');
  ins('BITSET');              ins('BOOLEAN');
  ins('CARDINAL');            ins('CAP');
  ins('CHR');                 ins('CHAR');
  ins('COMPLEX');             ins('CMPLX');
  ins('COPY');                ins('DEC');
  ins('DISPOSE');             ins('ENTIER');
  ins('ENTER');               ins('EXKL');
  ins('FALSE');               ins('FLOAT');
  ins('HALT');                ins('HIGH');
  ins('IM');                  ins('INC');
  ins('INCL');                ins('INT');
  ins('INTERRUPTIBLE');       ins('INTEGER');
  ins('LEAV');                ins('LEN');
  ins('LENGTH');              ins('LFLOAT');
  ins('LONG');                ins('LONGCOMPLEX');
  ins('LONGINT');             ins('LONGREAL');
  ins('MAX');                 ins('MIN');
  ins('NEW');                 ins('NIL');
  ins('ODD');                 ins('ORD');
  ins('PROC');                ins('PROTECTION');
  ins('RE');                  ins('REAL');
  ins('SIZE');                ins('SHORT');
  ins('SHORTINT');            ins('TRUE');
  ins('TRUNC');               ins('UNINTERRUPTIBLE');
  ins('VAL');
END SetModula2KeyWords;

(*------------------------------------------------------------------------*)
PROCEDURE SetNothingdDivideChars ();
BEGIN
  lstr.Assign( '', nothingDivideChars );
END SetNothingdDivideChars;

(*------------------------------------------------------------------------*)
PROCEDURE SetStdDivideChars ();
BEGIN
  lstr.Assign(' ,;', stdDivideChars);
(*
  NEW( stdDivideChars, 4 );
  stdDivideChars[0]:= ' ';
  stdDivideChars[1]:= ',';
  stdDivideChars[2]:= ';';
*)
END SetStdDivideChars;

(*------------------------------------------------------------------------*)
PROCEDURE SetDivideCharsForProcedure ();
BEGIN
  lstr.Assign(',;', divideCharsForProcedure);
(*
  NEW( divideCharsForProcedure, 3 );
  divideCharsForProcedure[0]:= ',';
  divideCharsForProcedure[1]:= ';';
*)
END SetDivideCharsForProcedure;

(*------------------------------------------------------------------------*)
VAR BaseTypesStorage: adt.List;
PROCEDURE InitBaseType * (cType: INT; defaultModulaType-: ARRAY OF CHAR; default: BOOLEAN);
VAR type: obj.Type;
    node: GeneratedType;
BEGIN
  obj.NewType( type );
  type.type:= cType;
  IF ~default THEN
    cfg.GetBaseType( cType, type.name );
  END;
  IF type.name = NIL THEN
    lstr.Assign( defaultModulaType, type.name );
  END;
  type.header:= NIL;
  type.generated:= TRUE;
  NewGeneratedType( node, type );
  GeneratedTypes.Insert( node );
  BaseTypesStorage.Insert(type);
END InitBaseType;

PROCEDURE InitPreDefinedTypes ();
VAR ptrVoid, baseType: obj.Type;
    node: GeneratedType;
BEGIN
  obj.NewType( ptrVoid );
  ptrVoid.SetName( 'SYSTEM.ADDRESS' );
  ptrVoid.type:= obj.t_ptr;
  ptrVoid.header:= NIL;
  ptrVoid.generated:= TRUE;

  obj.NewType( baseType );
  baseType.type:= obj.bt_void;
  baseType.header:= NIL;
  baseType.generated:= FALSE;

  ptrVoid.base:= GetType(baseType);
  NewGeneratedType( node, ptrVoid );
  GeneratedTypes.Insert( node );
  BaseTypesStorage.Insert(ptrVoid);
  BaseTypesStorage.Insert(baseType);
END InitPreDefinedTypes;

PROCEDURE RemoveBaseTypesStorage();
VAR e: adt.Element;
BEGIN
  BaseTypesStorage.FindFirst(e);
  WHILE e # NIL DO
    obj.Deallocate(e(obj.Type));
    BaseTypesStorage.FindNext(e);
  END;
  BaseTypesStorage.Clean();
END RemoveBaseTypesStorage;

(*------------------------------------------------------------------------*)
PROCEDURE SetBaseTypes ();
BEGIN
  InitBaseType( obj.bt_s_char  , int8, FALSE );
  InitBaseType( obj.bt_s_int   , int, FALSE );
  InitBaseType( obj.bt_s_sh_int, int16, FALSE );
  InitBaseType( obj.bt_s_l_int , int32, FALSE );
  InitBaseType( obj.bt_u_char  , card8, FALSE );
  InitBaseType( obj.bt_u_int   , unsigned, FALSE );
  InitBaseType( obj.bt_u_sh_int, card16, FALSE );
--  IF bitset_flag THEN
--    InitBaseType( obj.bt_u_l_int , bitset, TRUE );
--  ELSE
    InitBaseType( obj.bt_u_l_int , card32, FALSE );
--  END;
  InitBaseType( obj.bt_float   , real, FALSE );
  InitBaseType( obj.bt_double  , longreal, FALSE );
  InitBaseType( obj.bt_l_float , long_float, FALSE );
  InitBaseType( obj.bt_l_double, long_double, FALSE );
  InitBaseType( obj.bt_void    , void, FALSE );
--  InitBaseType( obj.bt_bitset  , bitset, TRUE );
  InitPreDefinedTypes();
END SetBaseTypes;


(*------------------------------------------------------------------------*)
(*			   WORK WITH FILE				  *)
(*------------------------------------------------------------------------*)
PROCEDURE IsIdentifier ( name-: ARRAY OF CHAR; type: INT ): BOOLEAN;
(* it is worked correct only for module name *)
VAR i, size: INT;
    res: BOOLEAN;

  PROCEDURE IsGoodChar( c: CHAR ): BOOLEAN;
  BEGIN
    RETURN  (c = '_') OR
	    ((c >= 'A') & (c <= 'Z')) OR
	    ((c >= 'a') & (c <= 'z')) OR
	    ((c >= '0') & (c <= '9'));
  END IsGoodChar;

BEGIN
  size:= lstr.Length(name);
  IF (size < 1) OR (~IsGoodChar(name[0])) OR
     ((name[0] <='9') & (name[0] >= '0') &
      ~( ((type=std_def_module) & (lstr.Length1(cfg.DefinitionFilePrefix)>0)) OR
         ((type#std_def_module) & (lstr.Length1(cfg.MacrosFilePrefix)>0))
        )
     ) THEN RETURN FALSE END;
  res:= TRUE;
  FOR i:= 1 TO size-1 DO
    res:= res & IsGoodChar( name[i] );
  END;
  RETURN res;
END IsIdentifier;

(*------------------------------------------------------------------------*)
PROCEDURE CheckModuleName ( name-: ARRAY OF CHAR; VAR res: INT; type: INT );
VAR path, ext, fileName: lstr.String;
BEGIN
  res:= all_right;
  fio.SplitName( name, path, fileName, ext );
  IF lstr.Length( path^ ) > 0 THEN
    res:= consist_path;
  END;
  IF lstr.Length( fileName^ )<= 0 THEN
    res:= not_present;
  ELSIF ~IsIdentifier( fileName^, type ) THEN
    res:= bad_identifier;
  END; lstr.Deallocate(path); lstr.Deallocate(ext); lstr.Deallocate(fileName);
END CheckModuleName;

(*------------------------------------------------------------------------*)
PROCEDURE GetModuleName ( fileName-: ARRAY OF CHAR; VAR modName: lstr.String;
			  type: INT );

   (*-------------------------------------*)
   PROCEDURE Translate2LongName( VAR str: lstr.String );
   VAR found: BOOLEAN;
       pos: CARD;
       pathSep: ARRAY 2 OF CHAR;
   BEGIN
     pathSep[0]:= plat.pathSep;
     pathSep[1]:= 0X;
     Strings.FindNext( pathSep, str^, 0, found, pos);
     WHILE found DO
       str^[pos]:= long_name_sep;
       Strings.FindNext( pathSep, str^, pos, found, pos);
     END;
     lstr.Append( long_name_sep, str );
   END Translate2LongName;

   (*-------------------------------------*)
VAR path, ext: lstr.String;
    res: INT;
    nelm: adt.NamedElement;
    elm: adt.Element;
BEGIN
  adt.NewNamedElement(nelm, fileName);
  obj.ModuleNames.Find( nelm, elm ); adt.Deallocate(nelm);
  IF elm # NIL THEN
    lstr.Assign(elm(obj.NameSynonym).newname^, modName);
  ELSE
    CheckModuleName( fileName, res, type );
    CASE res OF
      all_right, consist_path:
        fio.SplitName( fileName, path, modName, ext );
        IF ( cfg.LongFileName ) & ( lstr.Length1(path) > 0 ) &
           (~fio.IsAbsolutePath(path^) )
        THEN
          Translate2LongName( path );
	  lstr.Append( modName^, path );
          lstr.Assign(path^, modName);
	END;
        lstr.Deallocate(ext);
        lstr.Deallocate(path);
     |bad_identifier:
	Error(48, fileName);
	RETURN;
     |not_present:
        Error(28, 'H2DGen.GetModuleName');
	RETURN;
    END;
  END;
  IF type = std_def_module THEN
    lstr.Insert( cfg.DefinitionFilePrefix^, 0, modName );
  ELSE
    lstr.Insert( cfg.MacrosFilePrefix^, 0, modName );
  END;
END GetModuleName;


(*------------------------------------------------------------------------*)
PROCEDURE OpenFile ();
BEGIN
  IF ( fio.Open(OutFile.name^, fio.crmode, OutFile.hand) ) THEN
    OutFile.created:= TRUE;
    lstr.Assign( OutFile.name^, msg.File );
    OutFile.line:= 0;
    OutFile.pos:= 0;
  ELSE
    Error(6, OutFile.name^);
    RETURN;
  END;
END OpenFile;

(*------------------------------------------------------------------------*)
PROCEDURE WriteStr2File ( str-: ARRAY OF CHAR; VAR pstr: lstr.String;
                          leadtab, tab: INT );
VAR numberLine: INT;
BEGIN
  numberLine:= fio.FWriteStr( OutFile.hand, str, leadtab, tab );
  IF numberLine = 1 THEN OutFile.pos:= lstr.Length( str ) + leadtab;
  ELSE                   OutFile.pos:= 0;
  END;
  OutFile.line:= OutFile.line + numberLine;
  msg.Progress( OutFile.name^, OutFile.line );
  comment_was_just_generated:= FALSE;
  comment_was_just_generated_in_same_line:= FALSE;
  last_string_length:= lstr.Length(str) + leadtab;
  lstr.Deallocate(pstr);
END WriteStr2File;

(*--------------------------------------------------------------------*)
PROCEDURE WriteStrLn2File ( str-: ARRAY OF CHAR; VAR pstr: lstr.String;
                            leadtab, tab: INT );
VAR numberLine: INT;
BEGIN
  IF (lstr.Length(str) = 0)  THEN
    IF OutFile.emptyLine OR ~InsertEmptyLine THEN
      lstr.Deallocate(pstr);  RETURN;
    ELSE OutFile.emptyLine:= TRUE;
    END;
  ELSE
    OutFile.emptyLine:= FALSE;
  END;
  numberLine:= fio.FWriteStrLn( OutFile.hand, str, leadtab, tab );
  OutFile.line:= OutFile.line + numberLine;
  OutFile.pos:= 0;
  msg.Progress( OutFile.name^, OutFile.line );
  comment_was_just_generated:= FALSE;
  comment_was_just_generated_in_same_line:= FALSE;
  last_string_length:= lstr.Length(str) + leadtab;
  lstr.Deallocate(pstr);
END WriteStrLn2File;

(*------------------------------------------------------------------------*)
PROCEDURE WriteStr2Null ( str-: ARRAY OF CHAR; VAR pstr: lstr.String;
                          leadtab, tab: INT );
BEGIN
  RETURN;
END WriteStr2Null;

(*------------------------------------------------------------------------*)
PROCEDURE WriteStrLn2Buffer ( str-: ARRAY OF CHAR; VAR pstr: lstr.String;
                              leadtab, tab: INT );
VAR prStr: PrintString;
BEGIN
  IF (pstr = NIL) THEN  lstr.Assign( str, pstr) END;
  IF (lstr.Length(str) = 0)  THEN
    IF OutFile.emptyLine OR ~InsertEmptyLine THEN
      lstr.Deallocate(pstr);  RETURN;
    ELSE OutFile.emptyLine:= TRUE;
    END;
  ELSE
    OutFile.emptyLine:= FALSE;
  END;
  NewPrintString( prStr, leadtab, tab, TRUE);
  prStr.str:= pstr;
  WriteBuffer.Insert(prStr);
  last_string_length:= lstr.Length(str) + leadtab;
  pstr:= NIL;
END WriteStrLn2Buffer;

(*------------------------------------------------------------------------*)
PROCEDURE WriteStr2Buffer ( str-: ARRAY OF CHAR; VAR pstr: lstr.String;
                            leadtab, tab: INT );
VAR prStr: PrintString;
BEGIN
  IF (pstr = NIL) THEN  lstr.Assign( str, pstr) END;
  NewPrintString( prStr, leadtab, tab, FALSE);
  prStr.str:= pstr;
  WriteBuffer.Insert(prStr);
  last_string_length:= lstr.Length(str) + leadtab;
  pstr:= NIL;
END WriteStr2Buffer;

(*------------------------------------------------------------------------*)
PROCEDURE WriteEmptyLine ();
BEGIN
  WriteStrLn( '', null, 0, 0 );
END WriteEmptyLine;

(*------------------------------------------------------------------------*)
PROCEDURE CloseFile ( );
BEGIN
  IF OutFile = NIL THEN RETURN END;
  IF OutFile.created THEN
    fio.Close( OutFile.hand );
    OutFile.created:= FALSE;
  END;
END CloseFile;


(*------------------------------------------------------------------------*)
(*		       PROCEDURES BIND WITH OBJECTS			  *)
(*------------------------------------------------------------------------*)

PROCEDURE FlushWriteBuffer();
VAR e: adt.Element;
BEGIN
  WriteBuffer.FindFirst(e);
  WHILE (e # NIL) DO
    WITH e: PrintString DO
      IF e.ln THEN
        WriteStrLn(e.str^, e.str, e.leadtab, e.tab);
      ELSE
        WriteStr(e.str^, e.str, e.leadtab, e.tab);
      END;
      Deallocate(e);
    END;
    WriteBuffer.FindNext(e);
  END;
  WriteBuffer.Clean();
END FlushWriteBuffer;

(*------------------------------------------------------------------------*)
PROCEDURE EmitSection ( backEnd: INT);
-- если соответствующий backend не включен, то соответствующая
-- секция не попадает во WriteBuffer
BEGIN
  IF (BackEndSection = backEnd) THEN  RETURN END;
  IF    (cfg.BackEnd = obj.common) & (backEnd = obj.common) THEN
    WriteStrLn:= WriteStrLn2File;
    WriteStr  := WriteStr2File;
    IF ( (BackEndSection = obj.c_code) & (~WriteBuffer.IsEmpty()) ) OR
       ( (BackEndSection = obj.native) & (~WriteBuffer.IsEmpty()) &
          C_CodeSectionOpen
       )
    THEN
      WriteEmptyLine();
      WriteStrLn( "<* ELSE *>", null, 0, 0 );
      WriteEmptyLine();
      FlushWriteBuffer();
      WriteEmptyLine();
      OSectionNative:= none; OSectionC_Code:= none; OSectionCommon:= none;
    ELSIF (BackEndSection = obj.native) & (~WriteBuffer.IsEmpty()) THEN
      WriteEmptyLine();
      WriteStrLn( "<* IF NOT __GEN_C__ THEN *>", null, 0, 0 );
      WriteEmptyLine();
      FlushWriteBuffer();
      WriteEmptyLine();
      OSectionNative:= none; OSectionC_Code:= none; OSectionCommon:= none;
    END;
    WriteStrLn2File( "<* END *>", null, 0, 0 );
    C_CodeSectionOpen:= FALSE;
    WriteEmptyLine();
  ELSIF (cfg.BackEnd = obj.common) & (backEnd = obj.c_code) THEN
    WriteStrLn:= WriteStrLn2File;
    WriteStr  := WriteStr2File;
    IF ( BackEndSection = obj.common  ) OR
       ( (BackEndSection = obj.native) & (~C_CodeSectionOpen)
       )
    THEN
      WriteEmptyLine();
      WriteStrLn( "<* IF  __GEN_C__ THEN *>", null, 0, 0 );
      WriteEmptyLine();
    END;
    C_CodeSectionOpen:= TRUE;
  ELSIF (cfg.BackEnd = obj.common) & (backEnd = obj.native) THEN
    WriteStrLn:= WriteStrLn2Buffer;
    WriteStr  := WriteStr2Buffer;
  ELSIF (cfg.BackEnd = obj.c_code) & (backEnd = obj.native) THEN
    WriteStrLn:= WriteStr2Null;
    WriteStr  := WriteStr2Null;
  ELSIF (cfg.BackEnd = obj.native) & (backEnd = obj.c_code) THEN
    WriteStrLn:= WriteStr2Null;
    WriteStr  := WriteStr2Null;
  ELSE
    WriteStrLn:= WriteStrLn2File;
    WriteStr  := WriteStr2File;
  END;
  BackEndSection:= backEnd;
END EmitSection;

(*------------------------------------------------------------------------*)
PROCEDURE GenHeadingObject ( osection, genTypeDef, backEnd: INT );
-- перед началом генерации каждого объекта необходимо вызвать GenHeadingObject
-- при генерации одного объекта для разных BackEndSection, перед каждой
--   из них надо вызвать GenHeadingObject
BEGIN
  IF backEnd = obj.no_change THEN
    backEnd:= BackEndSection;
  END;
  EmitSection(backEnd);
  CASE backEnd OF
    obj.native: IF (osection = OSectionNative) THEN  RETURN;
                ELSE OSectionNative:= osection;
                END;
   |obj.c_code: IF (osection = OSectionC_Code) THEN  RETURN;
                ELSE OSectionC_Code:= osection;
                END;
   |obj.common: IF (osection = OSectionCommon) THEN RETURN
                ELSE OSectionCommon:= osection;
                END;
  END;
  WriteEmptyLine();
  CASE osection OF
    obj_const : WriteStrLn( 'CONST ', null, 0, 0 ); OutFile.emptyLine:= TRUE;
   |obj_type  : WriteStrLn( 'TYPE ', null, 0, 0 );  OutFile.emptyLine:= TRUE;
   |obj_var   : WriteStrLn( 'VAR ', null, 0, 0 );   OutFile.emptyLine:= TRUE;
  ELSE
  END;
  ObjectSection:= osection;
END GenHeadingObject;

(*------------------------------------------------------------------------*)
PROCEDURE MakeQualident ( type: obj.Object; VAR outStr: lstr.String );
VAR qualident: lstr.String;
    res: INT;
BEGIN
  IF ( type = NIL ) OR ( lstr.Length1(type.name) <= 0 ) THEN
    -- модифицированны могут быть только объекты с именем
    Error(28, 'H2DGen.Make.Qualident');
    RETURN;
  END;
  IF (type IS obj.Type) & (type(obj.Type).translation_variant # NIL) THEN
    type(obj.Type).translation_variant.generated:= TRUE;
    lstr.Append(type(obj.Type).translation_variant.name^, outStr);
  ELSIF (type IS obj.TypedObject) & (type(obj.TypedObject).type_name # NIL) THEN
    type(obj.TypedObject).type_name.generated:= TRUE;
    lstr.Append(type(obj.TypedObject).type_name.name^, outStr);
  ELSE
    IF ( type.header # NIL ) & ( type.header # OutFile.header ) &
       ( ~(type IS obj.Type) OR
          (type(obj.Type).tag_description_header # OutFile.header) ) & (* in this case type type defined in this module *)
       ( type.header.name # NIL ) &
       ( lstr.FindPos(type.name^, '.') = lstr.none )
    THEN
      CheckModuleName( type.header.name^, res, OutFile.type );
      CASE res OF
        all_right, consist_path:
          GetModuleName( type.header.name^, qualident, std_def_module  );
          IF msg.WasError THEN RETURN END;
          lstr.Append( qualident^, outStr ); lstr.Deallocate(qualident);
          lstr.AppendChar( '.', outStr );
      ELSE
        Error(48, type.header.name^);
        RETURN;
      END;
    END;
    lstr.Append( type.name^, outStr );
  END;
END MakeQualident;

(*------------------------------------------------------------------------*)
PROCEDURE MakeOnlyLanguageModifier ( modifier: adt.List; mode: INT;
				     VAR outStr: lstr.String );
(* mode - Type of  generate objects; see corresponding const *)
VAR elm: adt.Element;
    id: INT;
BEGIN
  modifier.FindFirst ( elm );
  WHILE elm # NIL DO
    modifier.Backup( id );
    CASE elm(obj.Modifier).modifier OF
      obj.pascal:
	IF mode = obj_procedure THEN
	  lstr.Append( ' ', outStr );
	  lstr.Append( pascalcall, outStr );
	ELSIF mode = obj_var THEN
	  lstr.Append( ' [] ', outStr );
	  lstr.Append( pascal, outStr );
	ELSE
	  lstr.Append( ' ', outStr );
	  lstr.Append( pascal, outStr );
	END;
     |obj.syscall:
	lstr.Append( ' ', outStr );
	lstr.Append( syscall, outStr );
     |obj.cdecl, obj.fortran:
    ELSE
    END;
    modifier.Restore( id );
    modifier.FindNext( elm );
  END;
END MakeOnlyLanguageModifier;

(*------------------------------------------------------------------------*)
PROCEDURE MakeOtherModifier ( modifier: adt.List; VAR outStr: lstr.String );
(* mode - Type of  generate objects; see corresponding const *)
VAR elm: adt.Element;
    id: INT;
BEGIN
  modifier.FindFirst( elm );
  WHILE elm # NIL DO
    modifier.Backup( id );
    CASE elm(obj.Modifier).modifier OF
      obj.const:
	lstr.Append( ' -', outStr );
    ELSE
    END;
    modifier.Restore( id );
    modifier.FindNext( elm );
  END;
END MakeOtherModifier;

(*------------------------------------------------------------------------*)
PROCEDURE MakeModifier ( modifier: adt.List; mode: INT;
			 VAR outStr: lstr.String );
(* mode - Type of  generate objects; see corresponding const *)
BEGIN
  MakeOnlyLanguageModifier( modifier, mode, outStr );
  MakeOtherModifier( modifier, outStr );
END MakeModifier;

(*----------------------------------------------------------------------*)
PROCEDURE NumberArrayElement( expr: obj.Expression; VAR outStr: lstr.String );
VAR result: obj.ConstantValue;
    s: lstr.String;
    dimension: obj.BinaryOperation;
    value: obj.Value;
BEGIN
  IF expr = NIL THEN RETURN END;
  obj.NewValue(value);
  value.type:= obj.int_const;
  value.hex:= FALSE;
  value.pos:= expr.pos;
  value.line:= expr.line;
  Assign('1', value.value);
  obj.NewBinaryOperation(dimension);
  dimension.operation:= obj.minus;
  dimension.operation:= obj.minus;
  dimension.pos:= expr.pos;
  dimension.line:= expr.line;
  dimension.first:= expr;
  dimension.second:= value;
  dimension.ComputeExpression( result, FALSE );
  IF result.type = obj.int_const THEN
    result.int:= result.int;
    s := result.GetText(TRUE);
    lstr.Append( s^, outStr );
  ELSE
    lstr.Append( ' illegal size of array', outStr );
    Error(28, 'H2DGen.NumberArrayElement');
  END;
  obj.Deallocate(result);
  obj.Deallocate(dimension);
  obj.Deallocate(value);
END NumberArrayElement;

(*------------------------------------------------------------------------*)
PROCEDURE IsTypeInCBackEnd ( type: obj.Type): INT;
VAR tmpType: obj.Type;
BEGIN
  IF type = NIL THEN  RETURN obj.no_change  END;
  tmpType:= type;
  WHILE tmpType # NIL DO
    IF tmpType.back_end = obj.c_code THEN
      RETURN obj.c_code;
    END;
    tmpType:= tmpType.base;
  END;
  RETURN obj.common;
END IsTypeInCBackEnd;

(*------------------------------------------------------------------------*)
PROCEDURE IsAnyTypeInCBackEnd ( list: adt.List ): INT;
(* If list doesn't contain obj.type or obj.TypedObject then
   will be always return obj.off
*)
VAR elm: adt.Element;
    id: INT;
BEGIN
  IF list = NIL THEN   RETURN obj.no_change END;
  list.FindFirst( elm );
  WHILE elm # NIL DO
    list.Backup( id );
    WITH
      elm: obj.TypedObject DO
	IF IsTypeInCBackEnd( elm.type ) = obj.c_code THEN
	  list.Restore( id );
	  RETURN obj.c_code;
	END;
     |elm: obj.Type DO
	IF IsTypeInCBackEnd( elm ) = obj.c_code THEN
	  list.Restore( id );
	  RETURN obj.c_code;
	END;
    ELSE
    END;
    list.Restore( id );
    list.FindNext( elm );
  END;
  RETURN obj.common;
END IsAnyTypeInCBackEnd;

(*------------------------------------------------------------------------*)
PROCEDURE IsFunction ( VAR type: obj.Type; VAR modifier: adt.List ): BOOLEAN;
VAR func: obj.Type;
    tmpModifier: adt.List;
BEGIN
  IF type = NIL THEN RETURN FALSE END;
  func:= type;
  adt.NewList( tmpModifier );
  WHILE func.type = obj.t_synonym DO
    BlendLists( func.modifier, tmpModifier );
    func:= func.base;
  END;
  IF func.type = obj.t_func THEN
    BlendLists( tmpModifier, modifier );
    type:= func;
  END;
  adt.Deallocate(tmpModifier);
  RETURN type.type = obj.t_func;
END IsFunction;

(*------------------------------------------------------------------------*)
PROCEDURE ExtractSynonymModifier ( tobj: obj.TypedObject );
VAR type: obj.Type;

  (*---------------------------------*)
  PROCEDURE AddSynonymsModifier ( modifier: adt.List );
  VAR elm: adt.Element;
      buffer: adt.List;
      id: INT;
  BEGIN
    adt.NewList( buffer );
    modifier.FindFirst( elm );
    WHILE elm # NIL DO
      modifier.Backup( id );
      CASE elm(obj.Modifier).modifier OF
	obj.volatile, obj.const, obj.pascal, obj.fortran, obj.cdecl:
	  buffer.Insert( elm );
      ELSE
      END;
      modifier.Restore( id );
      modifier.FindNext( elm );
    END;
    BlendLists( buffer, tobj.modifier );
  END AddSynonymsModifier;

(*---------------------------------*)
BEGIN
  type:= tobj.type;
  WHILE type.type = obj.t_synonym DO
    AddSynonymsModifier( type.modifier );
    type:= type.base;
  END;
END ExtractSynonymModifier;


(*------------------------------------------------------------------------*)
(*			  NAMES OF TYPE 				  *)
(*------------------------------------------------------------------------*)
PROCEDURE RepairName ( pattern: adt.NamedElement ): BOOLEAN;
CONST  yes      = 0;
       no       = 1;
       continue = 2;
VAR found: adt.Element;
    modName: lstr.String;
    counter, existSuchName: INT;
    ne: adt.NamedElement;

    (*-------------------*)
    PROCEDURE ExistSuchName ();
    BEGIN
      existSuchName:= yes;
--      OutFile.header.Tags.Find( ne, found );
      obj.FindInTags( OutFile.header, ne, found );
      IF found = NIL THEN
--        OutFile.header.Names.Find( ne, found );
        obj.FindInNames( OutFile.header, ne, found );
        IF found = NIL THEN
          OutFile.header.Macros.Find( ne, found );
          IF found = NIL THEN
            OutFile.creatednames.Find( ne, found );
            IF (found = NIL) & (ne.name^ # modName^) THEN
              existSuchName:= no;
            ELSE
              existSuchName:= continue;
            END;
          ELSIF (found(obj.Object).header = OutFile.header) THEN
            existSuchName:= continue;
          END;
        ELSIF (found(obj.Object).header = OutFile.header) THEN
          existSuchName:= continue;
        END;
      ELSIF (found(obj.Object).header = OutFile.header) THEN
        existSuchName:= continue;
      END;
    END ExistSuchName;

(*-------------------*)
BEGIN
  IF ( pattern = NIL ) OR ( pattern.name = NIL ) THEN
    Error(28, 'H2DGen.RepairName'); RETURN FALSE;
  END;
  counter:= 0;
  adt.NewNamedElement(ne, pattern.name^);
  GetModuleName( ModuleSystem.name^, modName, std_def_module );
  ExistSuchName();
  WHILE  (existSuchName = continue) OR (existSuchName = yes)  DO
    ne.SetName( pattern.name^ );
    IntToStr( counter, ne.name );
    INC( counter );
    ExistSuchName();
  END;
  pattern.SetName(ne.name^);
  adt.Deallocate(ne);
  lstr.Deallocate(modName);
  RETURN existSuchName = no;
END RepairName;


(*------------------------------------------------------------------------*)
PROCEDURE CreateSmartTypeName ( type: obj.Type; VAR name: lstr.String );
VAR str: lstr.String;
    result: obj.ConstantValue;

  (*------------------------------*)
  PROCEDURE GetName ( from-: ARRAY OF CHAR; VAR to: lstr.String );
  VAR tmpStr: lstr.String;
      point_pos: INT;
  BEGIN
    point_pos:= lstr.BackFindPos(from, '.');
    IF (point_pos # lstr.none) THEN
      lstr.Extract(from, point_pos+1, lstr.Length(from)-point_pos-1, to);
    ELSE
      lstr.Append( from, to );
    END;
    lstr.AppendChar( to[0], tmpStr );
    lstr.Capitalize( tmpStr^ );
    to[0]:= tmpStr[0];
    lstr.Deallocate(tmpStr);
  END GetName;

(* на модифицированное имя типа (поле type.translation_variant) пробиваем *)
BEGIN
  IF ( lstr.Length1(type.name) > 0 ) & ~(type.type IN obj.base_types) THEN
    GetName( type.name^, name );
    RETURN;
  END;
  CASE type.type OF
    obj.bt_s_char  : lstr.Append( 'SChar'  , name );
   |obj.bt_s_int   : lstr.Append( 'SInt'   , name );
   |obj.bt_s_sh_int: lstr.Append( 'SShInt' , name );
   |obj.bt_s_l_int : lstr.Append( 'SLInt'  , name );
   |obj.bt_u_char  : lstr.Append( 'UChar'  , name );
   |obj.bt_u_int   : lstr.Append( 'UInt'   , name );
   |obj.bt_u_sh_int: lstr.Append( 'UShInt' , name );
   |obj.bt_u_l_int : lstr.Append( 'ULInt'  , name );
   |obj.bt_float   : lstr.Append( 'Float'  , name );
   |obj.bt_double  : lstr.Append( 'Double' , name );
   |obj.bt_l_float : lstr.Append( 'LFloat' , name );
   |obj.bt_l_double: lstr.Append( 'LDouble', name );
   |obj.bt_void    : lstr.Append( 'Void'   , name );

   |obj.t_ptr	: lstr.Append( 'Ptr', name );
      CreateSmartTypeName( type.base, name );
   |obj.t_struct:
      IF lstr.Length1(type.name) # 0 THEN
	lstr.Append(type.name^, name);
      ELSE
	lstr.Append( 'Struct', name );
      END;
   |obj.t_array :
      IF type.expr = NIL THEN
	lstr.Append( 'OpenArray', name );
      ELSE
        type.expr.ComputeExpression(result,FALSE);
        str:= result.GetText(FALSE);
	lstr.Append( 'Array', name );
        lstr.Append( str^, name );
        lstr.Deallocate(str);
        obj.Deallocate(result);
      END;
      CreateSmartTypeName( type.base, name );
   |obj.t_union : lstr.Append( 'Union', name );
   |obj.t_enum	: lstr.Append( 'Enum', name );
   |obj.t_func	: lstr.Append( 'Proc', name );
   |obj.t_synonym: lstr.Append(type.name^, name);
  ELSE
    Error(28, 'H2DGen.CreateSmartTypeName');
  END;
END CreateSmartTypeName;

(*------------------------------------------------------------------------*)
PROCEDURE CreateTypeName ( type: obj.Type; modifier: adt.List ): obj.Type;
VAR nelm: adt.NamedElement;
    name: lstr.String;
    synonym: obj.Type;
BEGIN
  lstr.Assign( 'H2D_', name );

  CreateSmartTypeName( type, name );
  obj.NewType( synonym );
  synonym.header:= OutFile.header;
  synonym.type:= obj.t_synonym;
  synonym.gen_type_def:= obj.on;
  synonym.base:= type;
  BlendLists( modifier, synonym.modifier);
  synonym.SetName( name^ );
  lstr.Deallocate(name);
  IF ~RepairName(synonym) THEN
    Error(48, synonym.name^ );
    RETURN synonym;
  END;
  adt.NewNamedElement(nelm, synonym.name^);
  OutFile.creatednames.Insert( nelm );
  IF ( (cfg.BackEnd = obj.c_code) OR (cfg.BackEnd = obj.common) ) &
     (  ~OutFile.header.preheader )  THEN
    synonym.created_by_back_end:= TRUE;
  END;
  RETURN synonym;
END CreateTypeName;


(*------------------------------------------------------------------------*)
PROCEDURE ResolveCollisionOfName( type: obj.Type );
VAR name: lstr.String;
    nelm: adt.NamedElement;
BEGIN
  IF ( lstr.Length1(type.header.name) > 0 ) &
     ( ~Strings.Equal(type.header.name^, OutFile.header.name^) ) THEN
    RETURN;
  END;
  lstr.Assign(type.name^, name);
  CASE type.type OF
    |obj.t_struct: lstr.Append( '_struct', name );
    |obj.t_union : lstr.Append( '_union', name );
    |obj.t_enum  : lstr.Append( '_enum', name );
  ELSE
    Error(28, 'H2DGen.ResolveCollisionOfName'); RETURN;
  END;
  adt.NewNamedElement(nelm, name^);
  IF ~RepairName(nelm) THEN
    Error(48, type.name^);
    RETURN;
  ELSE
    HGenDefine( name^, type.name^);
  END;
  OutFile.creatednames.Insert( nelm );
  type.SetName(name^);
  lstr.Deallocate(name);
END ResolveCollisionOfName;


(*------------------------------------------------------------------------*)
PROCEDURE UniqueTagName ( pattern: obj.Type ): INT;
VAR found: adt.Element;
BEGIN
  OutFile.header.Macros.Find( pattern, found );
  IF found # NIL  THEN	RETURN no;
  ELSE
--    OutFile.header.Names.Find( pattern, found );
    obj.FindInNames( OutFile.header, pattern, found );
    IF found # NIL THEN
      WITH found: obj.Type DO
	IF  ( found.type = obj.t_synonym ) &
	  ( CompareType(pattern, found) ) THEN
	  RETURN exist_synonym;
	END;
      ELSE
      END;
      RETURN no;
    ELSE
      RETURN yes;
    END;
  END;
END UniqueTagName;


(*------------------------------------------------------------------------*)
(*		       COMMON USED RPOCEDURES				  *)
(*------------------------------------------------------------------------*)

PROCEDURE SetCommentTab ( str: lstr.String; leadtab, tab: INT );
VAR size: INT;
BEGIN
  size:= lstr.Length1( str );
  IF size < fio.SizeOutputLine - (comment_leadtab + leadtab) THEN
    OutFile.tab:= size + comment_leadtab;
  ELSE
    OutFile.tab:= tab + comment_leadtab;
  END;
END SetCommentTab;

(*------------------------------------------------------------------------*)
PROCEDURE AddCommentMark ( VAR str: lstr.String );
VAR size: INT;
    found: BOOLEAN;
    pos: CARD;
BEGIN
  size:= lstr.Length1( str );
  IF size > 0 THEN
    Strings.FindNext( '*', str^, 0, found, pos );
    WHILE found DO
      IF ( pos > 0 ) & ( str[pos-1] = '(' ) THEN
	lstr.Insert( ' ', pos, str );
	pos:= pos + 1;
	INC( size );
      END;
      IF ( pos < sys.VAL(CARD,size-1) ) & ( str[pos+1] = ')' ) THEN
	lstr.Insert( ' ', pos+1, str );
      END;
      Strings.FindNext( '*', str^, pos+1, found, pos );
    END;
  END;
  lstr.Insert( '(* ', 0, str );
  lstr.Append( ' *)', str );
END AddCommentMark;

(*------------------------------------------------------------------------*)
PROCEDURE WriteComment ( content-: ARRAY OF CHAR; leadtab, tab: INT );
VAR str: lstr.String;
BEGIN
  Assign( content, str );
  AddCommentMark( str );
  WriteStrLn( str^, str, leadtab, tab );
END WriteComment;

(*------------------------------------------------------------------------*)
PROCEDURE NewOutString ( VAR outStr: OutString );
VAR e: adt.Element;
BEGIN
  IF OutStringStorage.IsEmpty() THEN
    NEW( outStr );
    adt.NewList( outStr.comment );
  ELSE
    OutStringStorage.FindFirst(e);
    OutStringStorage.DeleteCurrent();
    outStr:= e(OutString);
  END;
  Assign('', outStr.str);
  outStr.leadtab:= 0;
  outStr.tab:= 0;
  outStr.line:= 0;
  outStr.backend:= obj.common;
END NewOutString;

(*------------------------------------------------------------------------*)
PROCEDURE  ( outStr: OutString ) CommentInFile ( startPos, size: INT );
VAR elm: adt.Element;
    tab, tmp: INT;
    str: lstr.String;
BEGIN
  tmp:= fio.SizeOutputLine;
  fio.SizeOutputLine:= MAX(INT);
  tab:= startPos - size + comment_leadtab + 3;
  outStr.comment.FindFirst( elm );
  IF elm # NIL THEN
    Assign( elm(adt.NamedElement).name^, str );
    AddCommentMark( str );
    IF 0 <= startPos THEN
      WriteStr( str^, str, tab, tab );
    ELSE
      WriteStr( str^, str, comment_leadtab, comment_leadtab );
    END;
    outStr.comment.FindNext( elm );
    WHILE elm # NIL DO
      WITH elm: adt.NamedElement DO
         Assign( elm(adt.NamedElement).name^, str );
         AddCommentMark( str );
         WriteStrLn( str^, str, tab + size, tab + size );
      END;
      outStr.comment.FindNext( elm );
    END;
  END;
  fio.SizeOutputLine:= tmp; lstr.Deallocate(str);
END CommentInFile;

(*------------------------------------------------------------------------*)
PROCEDURE GetOutputFileName * ( header-: ARRAY OF CHAR; VAR def: lstr.String);
(* used only outside *)
VAR name, path, ext: lstr.String;
BEGIN
  NEW(OutFile);
  NEW(OutFile.header); OutFile.header.SetName('');
  msg.NewStatistics(OutFile.header.stat, OutFile.header.name^);
  OutFile.line:= 0;
  OutFile.pos:= 0;
  lstr.Assign( header, OutFile.name );
  fio.SplitName( header, path, name, ext );
  GetModuleName( header, name, std_def_module );
  IF msg.WasError THEN RETURN END;
  fio.CreateName( path^, name^, cfg.OutputExtension^, def ); lstr.Deallocate(name); lstr.Deallocate(path); lstr.Deallocate(ext);
  OutFile:= NIL;
END GetOutputFileName;

(*------------------------------------------------------------------------*)
PROCEDURE NewFILE ( VAR file: FILE; header: obj.Header; type: INT );
VAR elm: adt.Element;
    fileName, ext: lstr.String;
    nelm: adt.NamedElement;
    id: INT;
BEGIN
  file:= NIL;
  NEW( file );
  file.type:= type;
  file.header:= header;
  file.obj_line:= new_line - 1;
  file.tab:= 0;
  file.used:= FALSE;
  file.emptyLine:= FALSE;
  file.created:= FALSE;

  lstr.Assign( header.name^, file.name );
  fio.SplitName( header.name^, file.path, fileName, ext );
  GetModuleName( header.name^, file.modname, type );
  IF msg.WasError THEN RETURN END;
  IF type = macro_mod_module THEN
    IF fio.IsAbsolutePath( file.path^ ) THEN
      fio.CreateName( '', file.modname^, cfg.ModExtension^, file.name );
    ELSE
      fio.CreateName( file.path^, file.modname^, cfg.ModExtension^, file.name );
    END;
  ELSE
    IF fio.IsAbsolutePath( file.path^ ) THEN
      fio.CreateName( '', file.modname^, cfg.OutputExtension^,
                      file.name );
    ELSE
      fio.CreateName( file.path^, file.modname^, cfg.OutputExtension^,
                      file.name );
    END;
  END;
  IF type = std_def_module THEN
    adt.NewNamedElement(nelm, file.modname^);
    adt.NewTree( file.creatednames );
    file.creatednames.Insert( nelm );

    header.includes.FindFirst( elm );
    WHILE elm # NIL DO
      header.includes.Backup( id );
      adt.NewNamedElement(nelm, '');
      GetModuleName( elm(adt.NamedElement).name^, nelm.name, std_def_module );
      IF msg.WasError THEN RETURN END;
      file.creatednames.Insert( nelm );
      header.includes.Restore( id );
      header.includes.FindNext( elm );
    END;
    IF ( (cfg.BackEnd = obj.c_code) OR (cfg.BackEnd = obj.common) ) &
       ( lstr.Length1(cfg.DefinitionFilePrefix) > 0 )
    THEN
      NewHFILE();
    END;
  END;
END NewFILE;


(*------------------------------------------------------------------------*)
(*		    GENERATE HEADING MODULE				  *)
(*------------------------------------------------------------------------*)

PROCEDURE GenCopyright ();
VAR str: lstr.String;
BEGIN
  Assign( 'Generated by H2D', str );
  lstr.Append( msg.version, str );
  lstr.Append( 'from ', str );
  lstr.Append( OutFile.header.name^, str );
  lstr.Append( '.', str );
  WriteComment( str^, 0, 0); lstr.Deallocate(str);
END GenCopyright;


(*------------------------------------------------------------------------*)
PROCEDURE BlendedHeadersList();
VAR elm: adt.Element;
    str: lstr.String;
BEGIN
  IF OutFile.header.mergedheaders.IsEmpty() THEN  RETURN  END;
  Assign( 'Merged headers: ', str );
  OutFile.header.mergedheaders.FindFirst( elm );
  lstr.Append( elm(adt.NamedElement).name^, str );
  OutFile.header.mergedheaders.FindNext( elm );
  WHILE elm # NIL DO
    lstr.Append( ', ', str );
    lstr.Append( elm(adt.NamedElement).name^, str );
    OutFile.header.mergedheaders.FindNext( elm );
  END;
  AddCommentMark( str );
  WriteStrLn( str^, str, 0, 0 );
END BlendedHeadersList;

(*------------------------------------------------------------------------*)
PROCEDURE GenHeading ( wrCstdlib: BOOLEAN );
(* First string must be writed by FWriteStr *)
VAR str: lstr.String;
BEGIN
  str:= NIL; GetTime( str );
  lstr.Insert( OutFile.name^, 0, str );
  AddCommentMark( str );
  WriteStr( str^, str, 0, 0 );
  GenCopyright();
  WriteStrLn( m2addtypes, null, 0, 0 );
  WriteStrLn( m2extensions, null, 0, 0 );
  IF OutFile.type = std_def_module THEN
    IF ( (cfg.BackEnd = obj.common) OR (cfg.BackEnd = obj.c_code) )
     (*
       &
       ( (lstr.Length1(cfg.PreHeader) > 0) &
	 ~Strings.Equal(OutFile.header.name^, cfg.PreHeader^)
       )
     *)
       THEN
      WriteStrLn( noheader, null, 0, 0 );
    END;
    IF GenTypeDef THEN
      WriteStrLn( new_gentypedef_p, null, 0, 0 );
    ELSE
      WriteStrLn( new_gentypedef_m, null, 0, 0 );
    END;
    IF wrCstdlib THEN
      WriteStrLn( cstdlib, null, 0, 0 );
    END;
    BlendedHeadersList();
  ELSE
    WriteStrLn( '(* Prototypes for macro implementation *)', null, 0, 0 );
  END;
END GenHeading;

(*------------------------------------------------------------------------*)
PROCEDURE GenModuleTitle ( );
VAR outStr: lstr.String;

  PROCEDURE MakeDirectLanguageSpecification();
  BEGIN
    IF (lstr.Length1(cfg.DLSString) > 0) THEN
      lstr.Append( '["', outStr );
      lstr.Append(cfg.DLSString^, outStr);
      lstr.Append( '"]', outStr );
    END;
  END MakeDirectLanguageSpecification;

BEGIN
  GenHeading( OutFile.header.cstdlib );
  WriteEmptyLine();
  IF OutFile.type = std_def_module THEN
    Assign( 'DEFINITION MODULE ', outStr );
    MakeDirectLanguageSpecification();
    lstr.Append( ' ', outStr );
  ELSIF OutFile.type = macro_def_module  THEN
    Assign( 'DEFINITION MODULE ', outStr );
  ELSE
    Assign( 'IMPLEMENTATION MODULE ', outStr );
  END;
  lstr.Append( OutFile.modname^, outStr );
  lstr.Append( ';', outStr );
  WriteStrLn( outStr^, outStr, 0, 0 );
END GenModuleTitle;

(*------------------------------------------------------------------------*)
PROCEDURE GenImportComment ();
VAR outStr: lstr.String;

  (*-------------------------*)
  PROCEDURE MakeImportList ();
  VAR elm: adt.Element;
      modName: lstr.String;
  BEGIN
    IF Import.IsEmpty() THEN RETURN END;
    Import.FindFirst( elm );
    WHILE elm # NIL DO
      WITH elm: adt.NamedElement DO
        IF (elm.name^ # ModuleSystem.name^)  THEN
          lstr.Append( ', ', outStr );
          lstr.Append( elm.name^, outStr );
        END;
      ELSE
	Error(28, 'H2DGen.MakeImportList');
	RETURN;
      END;
      Import.FindNext( elm );
    END; lstr.Deallocate(modName);
  END MakeImportList;

(*-------------------------*)
BEGIN
  Assign( '(*', outStr );
  lstr.Append( h2d_comment, outStr );
  lstr.Append( import_comment, outStr );
  WriteStrLn( outStr^, outStr, 0, 3 );
  Assign( 'IMPORT ', outStr );
  lstr.Append( OutFile.modname^, outStr );
  MakeImportList();
  lstr.Append( ';', outStr );
  WriteStrLn( outStr^, outStr, 0, 2 );
  WriteStrLn( '*)', null, 0, 0 );
  WriteEmptyLine();
END GenImportComment;

(*------------------------------------------------------------------------*)
PROCEDURE GenImport ( includes: adt.List);
VAR elm: adt.Element;
    str: lstr.String;
BEGIN
  WriteEmptyLine();
  Assign( 'IMPORT ', str );
  includes.FindFirst( elm );
  IF (elm # NIL)  THEN
    lstr.Append(elm(adt.NamedElement).name^, str);
    includes.FindNext(elm);
  END;
  WHILE elm # NIL DO
    lstr.Append( ', ', str );
    lstr.Append( elm(adt.NamedElement).name^, str );
    includes.FindNext( elm );
  END;
  lstr.Append( ';', str );
  WriteStrLn( str^, str, 0, 0 );
  GenImportComment();
END GenImport;


(*------------------------------------------------------------------------*)
(*				GENERATE COMMENTS			  *)
(*------------------------------------------------------------------------*)

PROCEDURE GenComment ( comment: obj.Comment );
VAR tmp, store_last_string_length: INT;
    str: lstr.String;
BEGIN
  tmp:= fio.SizeOutputLine;
  store_last_string_length:= last_string_length;
  fio.SizeOutputLine:= MAX(INT);
  Assign( comment.name^, str );
  AddCommentMark( str );
  IF comment.line = new_line THEN
    WriteStrLn( str^, str, 0, 0 );
  ELSIF comment.line = new_line_and_position  THEN
    WriteStrLn( str^, str, comment_position, 0 );
  ELSIF comment.line = ins_empty_line_before  THEN
    WriteStrLn( "", null, 0, 0 );
    WriteStrLn( str^, str, 0, 0 );
  ELSIF ( comment.line = OutFile.obj_line ) &
     ( ObjectSection # obj_procedure ) & ( ObjectSection # obj_macro )
  THEN
    IF comment_was_just_generated_in_same_line THEN
      WriteStrLn( str^, str, last_string_length + 3, OutFile.tab );
    ELSIF (last_string_length > (comment_position - 1))  THEN
      WriteStr( str^, str, 3, OutFile.tab );
    ELSE
      WriteStr( str^, str, comment_position - last_string_length - 1,
		OutFile.tab );
    END;
    comment_was_just_generated_in_same_line:= TRUE;
  ELSIF ~comment_was_just_generated THEN
    EmitSection( obj.common );
    WriteEmptyLine();
    WriteStrLn( str^, str, comment_position, 0 );
    comment_was_just_generated:= TRUE;
  ELSE
    EmitSection( obj.common );
    WriteStrLn( str^, str, 0, 0 );
    comment_was_just_generated:= TRUE;
  END;
  last_string_length:= store_last_string_length;
  fio.SizeOutputLine:= tmp; lstr.Deallocate(str);
END GenComment;


(*------------------------------------------------------------------------*)
(*			  GENERATE TYPES				  *)
(*------------------------------------------------------------------------*)
PROCEDURE MaxLengthName ( list: adt.List; VAR deep: INT ): INT;
VAR elm: adt.Element;
    length, maxLength: INT;
    id: INT;
BEGIN
  maxLength:= 0;
  deep:= 0;
  list.FindFirst( elm );
  WHILE elm # NIL DO
    list.Backup( id );
    IF ~(elm IS obj.Comment) THEN
      WITH elm: adt.NamedElement DO
	IF elm.name # NIL THEN
	  length:= lstr.Length( elm.name^ );
	  IF length > maxLength THEN
	    maxLength:= length;
	  END;
	END;
      END;
    END;
    INC( deep );
    list.Restore( id );
    list.FindNext( elm );
  END;
  RETURN maxLength;
END MaxLengthName;

(*------------------------------------------------------------------------*)
PROCEDURE MakeTypeComment ( comment: obj.Comment; buffer: adt.List;
                            additional: OutString );
VAR elm: adt.Element;
BEGIN
  buffer.FindLast( elm );
  IF (elm IS OutString) & (elm(OutString).line = comment.line) THEN
    elm(OutString).comment.Insert( comment );
    IF additional # NIL THEN
      additional.comment.Insert( comment );
    END;
  ELSE
    buffer.Insert( comment );
  END;
END MakeTypeComment;

(*------------------------------------------------------------------------*)
PROCEDURE MakeBaseType ( type: obj.Type; buffer: adt.List );
VAR outStr: OutString;
    baseType: obj.Type;
    elm: adt.Element;
BEGIN
  buffer.FindLast( elm );
  outStr:= elm(OutString);
  baseType:= FindCompatibleType( type );
  IF baseType = NIL THEN
    Error(28, 'H2DGen.MakeBaseType');
    RETURN;
  END;
  MakeQualident( baseType, outStr.str );
  lstr.Append( ';', outStr.str );
END MakeBaseType;


(*------------------------------------------------------------------------*)
PROCEDURE MakeEnumType( type: obj.Type; buffer: adt.List );
VAR elm, elm1, typeName: adt.Element;
    outStr: OutString;
    tab, deep, id: INT;
    intType: obj.Type;
    comment: obj.Comment;

  (*---------------------------*)
  PROCEDURE ChangedOrderEnumerating (): BOOLEAN;
  VAR prevValue: INT;
      elm: adt.Element;
      result: obj.ConstantValue;
  BEGIN
    prevValue:= -1;
    type.vars.FindFirst( elm );
    WHILE elm # NIL DO
      type.vars.Backup( id );
      WITH elm: obj.TypedObject DO
	elm.expr.ComputeExpression( result, FALSE );
	IF result.type = obj.int_const THEN
	  IF result.int # prevValue + 1 THEN
            obj.Deallocate(result);
	    RETURN TRUE;
	  ELSE
            obj.Deallocate(result);
            INC(prevValue);
	  END;
	ELSE
          Error(28, 'H2DGen.ChangedOrderEnumerating');
          obj.Deallocate(result);
          RETURN TRUE;
	END;
      ELSE
      END;
      type.vars.Restore( id );
      type.vars.FindNext( elm );
    END;
    RETURN FALSE;
  END ChangedOrderEnumerating;

  (*---------------------------*)
  PROCEDURE MakeEnumAsSetEnumeratedType ();
  VAR  prevValue: INT;
       prev_was_comment: BOOLEAN;
       elm: adt.Element;

       (*---------------------------*)
       PROCEDURE there_are_more_fields(e: adt.Element): BOOLEAN;
       VAR
	 id: INT;
       BEGIN
	 type.vars.Backup(id);
	 WHILE (e # NIL) & (e IS obj.Comment) DO
	   type.vars.FindNext(e);
	 END;
	 type.vars.Restore(id);
	 RETURN e # NIL;
       END there_are_more_fields;

       (*---------------------------*)
       PROCEDURE MakeEnumConst ( obj_cm: obj.TypedObject );
       VAR result: obj.ConstantValue;
	   nelm: adt.NamedElement;
	   value: lstr.String;
       BEGIN
	 obj_cm.expr.ComputeExpression( result, FALSE );
	 IF result.type = obj.int_const THEN
	   IF result.int # prevValue + 1 THEN
             adt.NewNamedElement(nelm, 'H2D: original integer value was ');
             value:= result.GetText(TRUE);
	     lstr.Append(value^ , nelm.name);
	     outStr.comment.Insert( nelm );
	     prevValue:= result.int;
	   ELSE
	     INC(prevValue);
	   END;
	 ELSE
	   Error(28, 'H2DGen.MakeEnumConst');
	 END;
         obj.Deallocate(result);
       END MakeEnumConst;

  (*---------------------------*)
  BEGIN   (* MakeEnumAsSetEnumeratedType *)
    prevValue:= -1;
    type.vars.FindFirst( elm );
    WHILE elm # NIL DO
      type.vars.Backup( id );
      WITH
	 elm: obj.Comment DO
           MakeTypeComment( elm, buffer, NIL );
	   prev_was_comment:= TRUE;
	|elm: obj.TypedObject DO
	   prev_was_comment:= FALSE;
	   NewOutString( outStr );
	   outStr.line:= elm.line;
	   outStr.leadtab:= 4;
	   IF lstr.Length1( elm.name ) # 0 THEN
             Assign( elm.name^, outStr.str );
             SpaceStr( BlankString, tab-lstr.Length(elm.name^) );
             lstr.Append( BlankString^, outStr.str );
	     outStr.tab:= tab+outStr.leadtab;
	     MakeEnumConst(elm);
	     buffer.Insert( outStr );
	   ELSE
	     Error(28, 'H2DGen.MakeEnumType');
	     RETURN;
	   END;
      ELSE
	Error(28, 'H2DGen.MakeEnumType');
	RETURN;
      END;
      type.vars.Restore( id );
      type.vars.FindNext( elm );
      IF (elm # NIL) & ~prev_was_comment & there_are_more_fields(elm) THEN
	lstr.Append( ',', outStr.str );
      END;
    END;
  END MakeEnumAsSetEnumeratedType;

  (*---------------------------*)
  PROCEDURE MakeEnumLikeConst ();
  VAR elm: adt.Element;
  BEGIN
    type.vars.FindFirst( elm );
    WHILE elm # NIL DO
      type.vars.Backup( id );
      WITH
	 elm: obj.Comment DO
	   GenComment( elm );
	|elm: obj.TypedObject DO
           GenConstant( elm );
       ELSE
	 Error(28, 'H2DGen.MakeEnumLikeConst');   RETURN;
      END;
      type.vars.Restore( id );
      type.vars.FindNext( elm );
    END;
  END MakeEnumLikeConst;

(*---------------------------*)
BEGIN
  tab:= MaxLengthName( type.vars, deep );
  buffer.FindLast( elm );
  outStr:= elm(OutString);
  NameOfGeneratedType.Pop( typeName );

  IF ( cfg.EnumeratedType = cfg.Const ) OR
     ( (cfg.EnumeratedType = cfg.Mixed) & (ChangedOrderEnumerating()) ) OR
     ( typeName(adt.NamedElement).name^ = obj.arbitrary_name )
  THEN
    obj.NewComment( comment, new_line, 'H2D: Enumeration: ' );
    lstr.Append( typeName(adt.NamedElement).name^, comment.name );
    GenComment( comment );
    MakeEnumLikeConst();

    comment.SetName( 'H2D: End of enumeration: ');
    lstr.Append( typeName(adt.NamedElement).name^, comment.name );

    IF (typeName(adt.NamedElement).name^ = obj.arbitrary_name) THEN
      buffer.Find( elm, elm1 );
      buffer.DeleteCurrent();
    ELSE
      obj.NewType( intType );
      intType.type:= obj.bt_s_int;
      MakeBaseType( intType, buffer );
    END;

    buffer.Insert( comment );
  ELSE
    lstr.Append( '(', outStr.str );
    MakeEnumAsSetEnumeratedType();
    NewOutString( outStr );
    Assign( ');', outStr.str );
    outStr.leadtab:= 2;
    buffer.Insert( outStr );
  END;
  NameOfGeneratedType.Push( typeName );
END MakeEnumType;

(*------------------------------------------------------------------------*)
PROCEDURE MakeArrayType ( type: obj.Type; buffer: adt.List );
VAR outStr: OutString;
    genType: obj.Type;
    elm: adt.Element;
BEGIN
  buffer.FindLast( elm );
  outStr:= elm(OutString);
  IF type.expr = NIL THEN
    obj.NewType( genType );
    genType.header:= OutFile.header;
    genType.type:= obj.t_ptr;
    genType.base:= type.base;
  ELSE
    lstr.Append( 'ARRAY [0..', outStr.str );
    NumberArrayElement( type.expr, outStr.str );
    IF msg.WasError THEN RETURN END;
    lstr.Append( ']', outStr.str );
    genType:= type.base;
    WHILE  genType.type = obj.t_array  DO
      lstr.Append( ', [0..', outStr.str );
      NumberArrayElement( genType.expr, outStr.str );
      IF msg.WasError THEN RETURN END;
      lstr.Append( ']', outStr.str );
      genType:= genType.base;
    END;
    lstr.Append( ' OF ', outStr.str );
  END;
  genType:= GetType( genType );  IF msg.WasError THEN RETURN END;
  MakeQualident( genType, outStr.str );
  lstr.Append( ';', outStr.str );
  IF type.back_end # obj.c_code THEN
    type.back_end:= IsTypeInCBackEnd( genType );
  END;
END MakeArrayType;


(*------------------------------------------------------------------------*)
PROCEDURE CreateFieldName ( names: adt.Tree; VAR str: lstr.String ): INT;
VAR number: INT;
    nelm: adt.NamedElement;
    find: adt.Element;
BEGIN
  number:= 0;
  adt.NewNamedElement(nelm, 'un');
  names.Find( nelm, find );
  WHILE find # NIL DO
    nelm.SetName( 'un' );
    INC( number );
    IntToStr( number, nelm.name );
    names.Find( nelm, find );
  END;
  lstr.Append( nelm.name^, str );
  names.Insert( nelm );
  RETURN lstr.Length( nelm.name^ );
END CreateFieldName;

(*------------------------------------------------------------------------*)
PROCEDURE MakeCBitField ( field: obj.TypedObject; outStr: OutString );
VAR nelm: adt.NamedElement;
    result: obj.ConstantValue;
BEGIN
  outStr.backend:= obj.c_code;
  MakeObjectType( field, outStr.str );
  lstr.Append( ';', outStr.str );
  adt.NewNamedElement(nelm, 'H2D: bit field. ');
  field.expr.ComputeExpression( result, FALSE );
  IF result.type = obj.int_const THEN
    lstr.Append( field.name^, nelm.name );
    lstr.Append( ':', nelm.name );
    IntToStr( result.int, nelm.name );
    outStr.comment.Insert( nelm );
  ELSE
    Error(28, 'H2DGen.MakeCBitField');
  END;
  obj.Deallocate(result);
END MakeCBitField;


(*------------------------------------------------------------------------*)
PROCEDURE MakeStructType ( type: obj.Type; buffer: adt.List );
VAR elm: adt.Element;
    outStr: OutString;
    nBitFOutStr: OutString; (* duplicate information about bit field for
                               native code part of definition *)
    tab, deep, id, lengthName, numbeOfBit: INT;
    nelm, nBitFComment: adt.NamedElement;
    names: adt.Tree;

  (*----------------------------------*)
  PROCEDURE MakeBitField ( field: obj.TypedObject; outStr: OutString );
  VAR result: obj.ConstantValue;
      writeComma: BOOLEAN;
  BEGIN
    writeComma:= TRUE;
    IF numbeOfBit = 0 THEN
      NewOutString( nBitFOutStr );
      nBitFOutStr.leadtab:= 4;
      nBitFOutStr.line:= outStr.line;
      nBitFOutStr.tab:= outStr.tab;
      Assign( outStr.str^, nBitFOutStr.str );
      nBitFOutStr.backend:= obj.native;
      adt.NewNamedElement(nBitFComment, 'H2D: bit field. ');
      nBitFOutStr.comment.Insert( nBitFComment );
      writeComma:= FALSE;
    END;
    MakeCBitField( field, outStr );

    field.expr.ComputeExpression( result, FALSE );
    IF result.type = obj.int_const THEN
      numbeOfBit:= numbeOfBit + result.int;
      IF writeComma THEN
        lstr.Append( ', ', nBitFComment.name );
      END;
      lstr.Append( field.name^, nBitFComment.name );
      lstr.Append( ':', nBitFComment.name );
      IntToStr( result.int, nBitFComment.name );
    ELSE
      Error(28, 'H2DGen.MakeStructType_MakeBitField');
    END;
    obj.Deallocate(result);
  END MakeBitField;

  (*----------------------------------*)
  PROCEDURE InsertNativeBitField ();
  BEGIN
    IF numbeOfBit <= 0 THEN RETURN END;
    lstr.Append( 'PACKEDSET OF [0..', nBitFOutStr.str );
    IntToStr( numbeOfBit - 1, nBitFOutStr.str );
    lstr.Append( '];', nBitFOutStr.str );
    buffer.Insert( nBitFOutStr );
    nBitFOutStr:= NIL;
    numbeOfBit:= 0;
    lstr.Append( '.', nBitFComment.name );
  END InsertNativeBitField;

(*----------------------------------*)
BEGIN
  adt.NewTree( names );
  ListToTree( type.vars, names );
  tab:= MaxLengthName( type.vars, deep );
  nBitFOutStr:= NIL;
  nBitFComment:= NIL;
  numbeOfBit:= 0;
  buffer.FindLast( elm );
  outStr:= elm(OutString);
  outStr.line:= type.line;
  lstr.Append( 'RECORD', outStr.str );
  IF type.tag_description_header = NIL THEN
    MakeTypeComment( declaration_without_definition, buffer, nBitFOutStr );
  ELSIF type.tag_description_header # OutFile.header  THEN
    simple_comment.SetName( definition_in_foregin_module );
    lstr.Append( type.tag_description_header.name^, simple_comment.name );
    MakeTypeComment( simple_comment, buffer, nBitFOutStr );
    TagDefinedInOtherHeader.Insert(type);
  ELSE
    type.vars.FindFirst( elm );
    WHILE elm # NIL DO
      type.vars.Backup( id );
      WITH
         elm: obj.Comment DO
           MakeTypeComment( elm, buffer, nBitFOutStr );
        |elm: obj.TypedObject DO
           NewOutString( outStr );
           outStr.line:= elm.line;
           outStr.leadtab:= 4;
           lengthName:= lstr.Length1( elm.name );
           IF lengthName = 0 THEN
             IF elm.obj = obj.bit_field THEN
               lengthName:= CreateFieldName( names, outStr.str );
               adt.NewNamedElement(nelm, 'H2D: unnamed field');
               outStr.comment.Insert( nelm );
             ELSE
               Error(28, 'H2DGen.MakeStructType');
               RETURN;
             END;
           ELSE
             Assign( elm.name^, outStr.str );
           END;
           SpaceStr( BlankString, tab-lengthName );
           lstr.Append( BlankString^, outStr.str );
           lstr.Append( ': ', outStr.str );
           outStr.tab:= tab+2+outStr.leadtab;
           CASE elm.obj OF
             obj.variable :
               InsertNativeBitField();
               BlendLists(elm.modifier, elm.type.tobj_modifier);
               MakeObjectType( elm, outStr.str );
               lstr.Append( ';', outStr.str );
               buffer.Insert( outStr );
            |obj.bit_field:
               MakeBitField( elm, outStr );
               buffer.Insert( outStr );
           ELSE
             Error(28, 'H2DGen.MakeStructType');
             RETURN;
           END;
      ELSE
        Error(28, 'H2DGen.MakeStructType');
        RETURN;
      END;
      IF msg.WasError THEN RETURN END;
      type.vars.Restore( id );
      type.vars.FindNext( elm );
    END;
    InsertNativeBitField();
  END;
  NewOutString( outStr );
  Assign( 'END;', outStr.str );
  outStr.leadtab:= 2;
  buffer.Insert( outStr );
  IF type.back_end # obj.c_code THEN
    type.back_end:= IsAnyTypeInCBackEnd( type.vars );
  END;
  adt.Deallocate(names);
END MakeStructType;

(*------------------------------------------------------------------------*)
PROCEDURE MakeUnionType ( type: obj.Type; buffer: adt.List );
VAR elm: adt.Element;
    outStr, nBitFOutStr: OutString;
    tab, deep, number, id: INT;
    names: adt.Tree;

  (*-------------------------------*)
  PROCEDURE MakeField ( obj_cmp: adt.Element );
  VAR nelm: adt.NamedElement;
      lengthName: INT;

    (*------------------------------------*)
    PROCEDURE MakeBitField ( field: obj.TypedObject; outStr: OutString );
    VAR result: obj.ConstantValue;
    BEGIN
      NewOutString( nBitFOutStr );
      nBitFOutStr.leadtab:= 6;
      nBitFOutStr.line:= outStr.line;
      nBitFOutStr.tab:= outStr.tab;
      Assign( outStr.str^, nBitFOutStr.str );
      nBitFOutStr.backend:= obj.native;

      MakeCBitField( field, outStr );

      BlendLists( outStr.comment, nBitFOutStr.comment );
      lstr.Append( 'PACKEDSET OF [0..', nBitFOutStr.str );
      field.expr.ComputeExpression( result, FALSE );
      IF result.type = obj.int_const THEN
        IntToStr( result.int- 1, nBitFOutStr.str );
        lstr.Append( '];', nBitFOutStr.str );
      ELSE
        Error(28, 'H2DGen.MakeUnionType_MakeBitField');
      END;
      obj.Deallocate(result);
    END MakeBitField;

  (*------------------------------------*)
  BEGIN
    WITH
       obj_cmp: obj.Comment DO
         MakeTypeComment( obj_cmp, buffer, nBitFOutStr );
      |obj_cmp: obj.TypedObject DO
	 outStr.leadtab:= 6;
	 outStr.line:= obj_cmp.line;
	 IntToStr( number, outStr.str );
         SpaceStr( BlankString, deep-sys.VAL(INT,conv.LengthInt(number)) );
         lstr.Append( BlankString^, outStr.str );
	 lstr.Append( ': ', outStr.str );
	 lengthName:= lstr.Length1( obj_cmp.name );
	 IF lengthName = 0 THEN
	   IF obj_cmp.obj = obj.bit_field THEN
             lengthName:= CreateFieldName( names, outStr.str );
	     NEW( nelm );
             nelm.SetName( 'H2D: unnamed field' );
	     outStr.comment.Insert( nelm );
	   ELSE
             Error(28, 'H2DGen.MakeField');  RETURN;
	   END;
	 ELSE
	   lstr.Append( obj_cmp.name^, outStr.str );
	 END;
         SpaceStr( BlankString, tab-lengthName );
         lstr.Append( BlankString^, outStr.str );
	 lstr.Append( ': ', outStr.str );
	 outStr.tab:= tab+8+deep+outStr.leadtab;
	 CASE obj_cmp.obj OF
	   obj.variable :
             BlendLists( obj_cmp.modifier, obj_cmp.type.tobj_modifier);
             MakeObjectType( obj_cmp, outStr.str );
	     lstr.Append( ';', outStr.str );
	     outStr.tab:= outStr.tab+1;
	     buffer.Insert( outStr );
	  |obj.bit_field:
             outStr.tab:= outStr.tab+1;
             MakeBitField( obj_cmp, outStr );
             buffer.Insert( outStr );
             buffer.Insert( nBitFOutStr );
             nBitFOutStr:= outStr;
	 ELSE
	   Error(28, 'H2DGen.MakeField');
	   RETURN;
	 END;
	 INC( number );
    END;
  END MakeField;

(*-------------------------------*)
BEGIN
  adt.NewTree( names );
  ListToTree( type.vars, names );
  number:= 0;
  nBitFOutStr:= NIL;
  tab:= MaxLengthName( type.vars, deep );
  deep:= sys.VAL(INT,conv.LengthInt( deep) );
  buffer.FindLast( elm );
  outStr:= elm(OutString);
  lstr.Append( 'RECORD', outStr.str );
  IF type.tag_description_header = NIL THEN
    MakeTypeComment( declaration_without_definition, buffer, nBitFOutStr );
  ELSIF type.tag_description_header # OutFile.header  THEN
    simple_comment.SetName( definition_in_foregin_module );
    lstr.Append( type.tag_description_header.name^, simple_comment.name );
    MakeTypeComment( simple_comment, buffer, nBitFOutStr );
    TagDefinedInOtherHeader.Insert(type);
  ELSE
    NewOutString( outStr );
    Assign( 'CASE : INTEGER OF', outStr.str );
    outStr.leadtab:= 4;
    buffer.Insert( outStr );
    type.vars.FindFirst( elm );
    IF elm # NIL THEN
      NewOutString( outStr );
      Assign( ' ', outStr.str );
      MakeField( elm );
      type.vars.FindNext( elm );
      WHILE elm # NIL DO
        type.vars.Backup( id );
        NewOutString( outStr );
        Assign( '|', outStr.str );
        MakeField( elm );
        type.vars.Restore( id );
        type.vars.FindNext( elm );
      END;
    END;
  END;
  NewOutString( outStr );
  Assign( 'END;', outStr.str );
  outStr.leadtab:= 4;
  buffer.Insert( outStr );

  NewOutString( outStr );
  Assign( 'END;', outStr.str );
  outStr.leadtab:= 2;
  buffer.Insert( outStr );
  IF type.back_end # obj.c_code THEN
    type.back_end:= IsAnyTypeInCBackEnd( type.vars );
  END; adt.Deallocate(names);
END MakeUnionType;

(*------------------------------------------------------------------------*)
PROCEDURE MakeProcTypeArguments ( vars: adt.List; VAR outStr: lstr.String ): INT;
VAR elm: adt.Element;
    id: INT;
    comma: BOOLEAN;

  (*-----------------------*)
  PROCEDURE MakeOneArgument ( tobj: obj.TypedObject );
  VAR genType: obj.Type;
      str: lstr.String;
  BEGIN
    IF tobj.obj = obj.arguments THEN
      lstr.Append( 'SEQ ', outStr );
      lstr.Append( 'SYSTEM.BYTE', outStr );
      RETURN;
    ELSIF tobj.type.type = obj.bt_void THEN
      RETURN;
    ELSIF tobj.type.type = obj.t_array THEN
	tobj.type.expr:= NIL;
    END;
    BlendLists( tobj.modifier, tobj.type.tobj_modifier );
    genType:= GetType( tobj.type );
    IF msg.WasError THEN RETURN END;
    IF lstr.Length1(tobj.name) > 0 THEN
      Assign( tobj.name^, str );
      AddCommentMark( str );
      lstr.Append( str^, outStr );
      lstr.Append( ' ', outStr );
    END;
    MakeQualident( genType, outStr );
    IF tobj.type.back_end # obj.c_code THEN
      tobj.type.back_end:= IsTypeInCBackEnd( genType );
    END;
  END MakeOneArgument;

(*-----------------------*)
BEGIN
  vars.FindFirst( elm );
  vars.Backup( id );
  IF elm = NIL THEN vars.Restore( id );
    RETURN obj.no_change;
  END;
  IF elm IS obj.TypedObject THEN
    MakeOneArgument( elm(obj.TypedObject) );
    IF msg.WasError THEN RETURN obj.no_change END;
    comma:= TRUE;
  ELSIF elm IS obj.Comment THEN
    AddCommentMark(elm(adt.NamedElement).name);
    lstr.Append(elm(adt.NamedElement).name^, outStr);
    lstr.Append(' ', outStr);
    comma:= FALSE;
  ELSE
    Error(28, 'H2DGen.MakeFuncArguments');
    RETURN obj.no_change;
  END;
  vars.Restore( id );
  vars.FindNext( elm );
  WHILE elm # NIL DO
    vars.Backup( id );
    IF comma THEN
      lstr.Append( ', ', outStr );
    END;
    IF elm IS obj.TypedObject THEN
      MakeOneArgument( elm(obj.TypedObject) );
      comma:= TRUE;
    ELSIF elm IS obj.Comment THEN
      AddCommentMark(elm(adt.NamedElement).name);
      lstr.Append(elm(adt.NamedElement).name^, outStr);
      lstr.Append(' ', outStr);
      comma:= FALSE;
    END;
    IF msg.WasError THEN RETURN obj.no_change END;
    vars.Restore( id );
    vars.FindNext( elm );
  END;
  RETURN IsAnyTypeInCBackEnd( vars );
END MakeProcTypeArguments;

(*------------------------------------------------------------------------*)
PROCEDURE MakeProcTypeReturnValue ( type: obj.Type; VAR outStr: lstr.String );
VAR genType: obj.Type;
BEGIN
  IF ( type = NIL ) OR ( type.type = obj.bt_void ) THEN RETURN END;
  genType:= GetType( type );
  IF msg.WasError THEN RETURN END;
  lstr.Append( ': ', outStr );
  MakeQualident( genType, outStr );
  IF type.back_end # obj.c_code THEN
    type.back_end:= IsTypeInCBackEnd( genType );
  END;
END MakeProcTypeReturnValue;

(*------------------------------------------------------------------------*)
PROCEDURE MakeProcedureType ( tobj: obj.TypedObject; buffer: adt.List );
VAR outStr: OutString;
    cBackEnd: INT;
    elm: adt.Element;
BEGIN
  buffer.FindLast( elm );
  outStr:= elm(OutString);
  lstr.Append( 'PROCEDURE', outStr.str );
  MakeModifier( tobj.modifier, obj_procedure, outStr.str );
  lstr.Append( ' ( ', outStr.str );
  outStr.tab:= lstr.Length( outStr.str^ );
  cBackEnd:= MakeProcTypeArguments( tobj.type.vars, outStr.str );
  IF msg.WasError THEN RETURN END;
  lstr.Append( ' )', outStr.str );
  BlendLists( tobj.type.modifier, tobj.type.base.tobj_modifier );
  MakeProcTypeReturnValue( tobj.type.base, outStr.str );
  IF msg.WasError THEN RETURN END;
  lstr.Append( ';', outStr.str );
  IF tobj.type.back_end # obj.c_code THEN
    tobj.type.back_end:= cBackEnd;
  END;
  IF tobj.type.back_end # obj.c_code THEN
    tobj.type.back_end:= IsTypeInCBackEnd( tobj.type );
  END;
END MakeProcedureType;

(*------------------------------------------------------------------------*)
PROCEDURE MakePointerType( type: obj.Type; buffer: adt.List );
VAR outStr: OutString;
    baseType: obj.Type;
    func: obj.TypedObject;
    elm: adt.Element;
    modifier: adt.List;
BEGIN
  buffer.FindLast( elm );
  outStr:= elm(OutString);
  IF IsFunction( type.base, type.modifier ) THEN
    obj.NewTypedObject( func, obj.variable );
    BlendLists( type.modifier, func.modifier );
    func.type:= type.base;
    MakeProcedureType( func, buffer );
    baseType:= func.type; obj.Deallocate(func);
  ELSE
    lstr.Append( 'POINTER', outStr.str );
    adt.NewList( modifier );
    modifier.Insert( type.ptr_modifier );
    MakeOnlyLanguageModifier( modifier, obj_type, outStr.str ); adt.Deallocate(modifier);
    lstr.Append( ' TO ', outStr.str );
    IF lstr.Length1(type.base.name) = 0 THEN
      baseType:= GetType( type.base );
    ELSE
      baseType:= type.base;
    END;
    IF msg.WasError THEN RETURN END;
    MakeQualident( baseType, outStr.str );
    lstr.Append( ';', outStr.str );
  END;	   IF msg.WasError THEN RETURN END;
  IF type.back_end # obj.c_code THEN
    type.back_end:= IsTypeInCBackEnd( baseType );
  END;
END MakePointerType;

(*------------------------------------------------------------------------*)
PROCEDURE MakeSynonym ( type: obj.Type; VAR buffer: adt.List );
VAR outStr: OutString;
    genType: obj.Type;
    elm: adt.Element;
BEGIN
  buffer.FindLast( elm );
  outStr:= elm(OutString);
  IF (  ( type.generated )  ) OR (  ( type.header # OutFile.header ) &
        ( type.tag_description_header # OutFile.header )  )
  THEN
    genType:= type;
  ELSIF ( lstr.Length1(type.name) > 0  ) &
        ( type.tag_description_header # OutFile.header )
  THEN
    genType:= GetType( type );     IF msg.WasError THEN RETURN END;
  ELSE
    CASE type.type OF
--      obj.bt_s_char..obj.bt_bitset:  MakeBaseType( type, buffer ); RETURN;
      obj.bt_s_char..obj.bt_void:  MakeBaseType( type, buffer ); RETURN;
     |obj.t_ptr     : MakePointerType( type, buffer ); RETURN;
     |obj.t_struct  : MakeStructType( type, buffer );  RETURN;
     |obj.t_array   : MakeArrayType( type, buffer );   RETURN;
     |obj.t_union   : MakeUnionType( type, buffer );   RETURN;
     |obj.t_enum    : MakeEnumType( type, buffer );    RETURN;
     |obj.t_synonym : genType:= type;
    ELSE
      Error(28, 'H2DGen.MakeSynonym');
      RETURN;
    END;
  END;
  MakeQualident( genType, outStr.str );
  lstr.Append( ';', outStr.str );
  IF type.back_end # obj.c_code THEN
    type.back_end:= IsTypeInCBackEnd( genType );
  END;
END MakeSynonym;

(*------------------------------------------------------------------------*)
PROCEDURE MakeTypeIdent ( VAR type: obj.Type; VAR outStr: OutString );
VAR foundSynonym: adt.Element;
BEGIN
  NewOutString( outStr );
  outStr.leadtab:= 2;
  IF type.type # obj.t_synonym THEN
--    OutFile.header.Names.Find( type, foundSynonym );
    obj.FindInNames( OutFile.header, type, foundSynonym );
    IF foundSynonym # NIL  THEN
     IF ( foundSynonym IS obj.Type ) &
        ( CompareType(type, foundSynonym(obj.Type)) )
     THEN
       type.gen_type_def:= obj.on;
     ELSE
       type.gen_type_def:= obj.off;
     END;
    END;
  END;
  Assign( type.name^, outStr.str );
  lstr.Append( ' = ', outStr.str );
  outStr.tab:= lstr.Length( outStr.str^ );
END MakeTypeIdent;

(*------------------------------------------------------------------------*)
PROCEDURE GenType ( type: obj.Type );
VAR buffer: adt.List;
    outStr: OutString;
    tmp: INT;
    elm: adt.Element;
    modifier: adt.List;

  (*-------------------------*)
  PROCEDURE FlushBuffer ();
  VAR elm: adt.Element;
      max, id, size: INT;
      notOnlyComment: BOOLEAN;

    (*-------------------------*)
    PROCEDURE MaxStrSize ();
    VAR elm: adt.Element;
	size: INT;
    BEGIN
      max:= 0;
      buffer.FindFirst( elm );
      WHILE elm # NIL DO
	WITH
	   elm: obj.Comment DO;
          |elm: OutString DO
	     size:= lstr.Length1( elm.str );
             notOnlyComment:= TRUE;
	     IF (size < fio.SizeOutputLine-elm.leadtab-min_comment_length) &
		( size > max ) THEN
	       max:= size;
	     END;
	END;
	buffer.FindNext( elm );
      END;
    END MaxStrSize;

  (*-------------------------*)
  BEGIN
    notOnlyComment:= FALSE;
    MaxStrSize();
    buffer.FindFirst( elm );
    buffer.Backup( id );
    IF (elm # NIL) & notOnlyComment THEN
      GenHeadingObject( obj_type, type.gen_type_def, type.back_end );
    END;
    WriteEmptyLine();
    InsertEmptyLine:= FALSE;
    buffer.Restore( id );
    WHILE elm # NIL DO
      buffer.Backup( id );
      WITH
	 elm: obj.Comment DO
           tmp:=comment_position;
           comment_position:= 4;
           GenComment( elm );
           IF elm.line # new_line_and_position THEN
             OutFile.obj_line:= elm.line;
           END;
           comment_position:= tmp;
        |elm: OutString  DO
           EmitSection( elm.backend );
           size:= lstr.Length1(elm.str);
	   IF elm.str # NIL THEN
             WriteStrLn( elm.str^, elm.str, elm.leadtab, elm.tab );
	   END;
	   OutFile.obj_line:= type.line;
           SetCommentTab( elm.str, elm.leadtab, elm.tab );
           elm.CommentInFile( max, size + 4 );
      END;
      fio.DivideChars:= stdDivideChars;
      buffer.Restore( id );
      buffer.FindNext( elm );
    END;
    InsertEmptyLine:= TRUE;
    RemoveOutStringsBuffer(buffer);
  END FlushBuffer;

  (*-------------------------*)
  PROCEDURE IsSynonymOnTag ( type: obj.Type ): BOOLEAN;
  BEGIN
    IF ( type.name = NIL ) OR ( lstr.Length(type.name^) = 0  ) THEN
      Error(28, 'H2DGen.IsSynonyOnTag');
      RETURN TRUE;
    ELSIF (type.type = obj.t_synonym) & (lstr.Length1(type.base.name) > 0) THEN
      (* in case  typedef struct tag_name{int i;} synonym_name;
       tag always full generate; synonym always generate lake refrens to
       tag if it exist;  *)
      GenType(type.base);
      IF Strings.Equal(type.name^, type.base.name^) THEN
        (* if tag_name = synonym_name then synonym musn't generate *)
        RETURN TRUE;
      END;
    ELSIF  UniqueTagName( type ) = no  THEN (*type.type # obj.t_synonym*)
      ResolveCollisionOfName( type );
      RETURN FALSE;
    END;
    RETURN FALSE;
  END IsSynonymOnTag;

  (*-------------------------*)
  PROCEDURE ExistCompatibleType ( type: obj.Type ): BOOLEAN;
  VAR compatibleType: obj.Type;
  BEGIN
    IF type.translation_variant # NIL THEN
      (* приоритет у того что указывает пользователь всегда выше *)
      MakeQualident( type, outStr.str );
      lstr.Append( ';', outStr.str );
      RETURN TRUE;
    END;
    compatibleType:= FindCompatibleType( type );
    IF (compatibleType # NIL) & (type # compatibleType) THEN
      IF (type.type # obj.t_synonym) & (compatibleType.type = obj.t_synonym) &
         (compatibleType.base = type)
      THEN
        RETURN FALSE;
      ELSIF ( compatibleType.generated OR
            (OutFile.header # compatibleType.header) )
      THEN
        MakeQualident( compatibleType, outStr.str );
        lstr.Append( ';', outStr.str );
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END ExistCompatibleType;


(*-------------------------*)
BEGIN
  IF (type.generated) OR (type.header # OutFile.header) THEN  RETURN  END;
  type.generated:= TRUE;
  IF IsSynonymOnTag( type ) THEN RETURN END;
  MakeTypeIdent( type, outStr );   IF msg.WasError THEN RETURN END;
  adt.NewList( buffer );
  buffer.Insert( outStr ); adt.NewList(modifier);
  ValueHandlerOn:= TRUE;
  IF IsFunction(type, modifier) THEN
    RemoveOutStringsBuffer(buffer);
    adt.NewList(buffer);
  ELSIF ~ExistCompatibleType( type ) THEN
    NameOfGeneratedType.Push( type );
    CASE type.type OF
--      obj.bt_s_char..obj.bt_bitset: MakeBaseType( type, buffer );
      obj.bt_s_char..obj.bt_void: MakeBaseType( type, buffer );
     |obj.t_ptr     : MakePointerType( type, buffer );
     |obj.t_struct  : MakeStructType( type, buffer );
     |obj.t_array   : MakeArrayType( type, buffer );
     |obj.t_union   : MakeUnionType( type, buffer );
     |obj.t_enum    : MakeEnumType( type, buffer );
     |obj.t_synonym : MakeSynonym( type.base, buffer );
	IF type.back_end # obj.c_code THEN
	  type.back_end:= IsTypeInCBackEnd( type.base );
	END;
    ELSE
      Error(28, 'GenType');
      RETURN;
    END;
  END;	IF msg.WasError THEN RETURN END;
  FlushBuffer();
  adt.Deallocate(modifier);
  OutFile.obj_line:= type.line;
  NameOfGeneratedType.Pop( elm );

  ReplaceCompatibleType( type );
  IF ( HFile # NIL ) & ( type.created_by_back_end ) THEN
    HGenSynonym( type );
  END;
  ValueHandlerOn:= FALSE;
END GenType;

(*------------------------------------------------------------------------*)
PROCEDURE GetType ( type: obj.Type ): obj.Type;
VAR genType: obj.Type;
    node: GeneratedType;
    e: adt.Element;
    modifier: adt.List;
BEGIN
  IF ( type.generated ) OR ( type.header # OutFile.header ) THEN
    type.tobj_modifier.Clean();
    RETURN type;
  ELSIF lstr.Length1(type.name) > 0 THEN
    GenType( type ); IF msg.WasError THEN RETURN NIL END;
    IF ~IsPointerToBaseType(type) THEN
      NewGeneratedType( node, type );
      GeneratedTypes.Insert( node );
    END;
    type.tobj_modifier.Clean();
    RETURN type;
  ELSE
    modifier:= type.tobj_modifier;
    genType:= FindCompatibleType( type );
    IF genType = NIL THEN
      genType:= CreateTypeName( type, modifier );
      GenType( genType ); IF msg.WasError THEN RETURN NIL END;
      NewGeneratedType( node, genType );
      GeneratedTypes.Find(node, e);
      IF e = NIL THEN
        GeneratedTypes.Insert( node );
      ELSE
        Deallocate(node);
      END;
    ELSIF ~genType.generated & ( (genType.header = OutFile.header) OR
          (genType.tag_description_header = OutFile.header) )
    THEN (* other header may be don't generate yet *)
      GenType( genType );
    END;
    type.tobj_modifier.Clean();
    RETURN genType;
  END;
END GetType;


(*------------------------------------------------------------------------*)
(*			GENERATE VARIABLES				  *)
(*------------------------------------------------------------------------*)

PROCEDURE MakeObjectType( tobj: obj.TypedObject; VAR outStr: lstr.String );
VAR genType, type: obj.Type;
BEGIN
  IF tobj.type_name # NIL THEN  MakeQualident( tobj, outStr );
  ELSE
    type:= tobj.type;
    genType:= FindCompatibleType( type );
    IF genType = NIL THEN
      IF ( type.type = obj.t_array ) & ( type.expr # NIL ) THEN
        lstr.Append( 'ARRAY [0..', outStr );
        NumberArrayElement( type.expr, outStr );
        IF msg.WasError THEN RETURN END;
        lstr.Append( ']', outStr );
        genType:= type.base;
        WHILE  genType.type = obj.t_array  DO
          lstr.Append( ', [0..', outStr );
          NumberArrayElement( genType.expr, outStr );
          IF msg.WasError THEN RETURN END;
          lstr.Append( ']', outStr );
          genType:= genType.base;
        END;
        lstr.Append( ' OF ', outStr );
      ELSE
        genType:= type;
      END;
    ELSE
      genType:= type;
    END;
    genType:= GetType( genType );
    IF msg.WasError THEN RETURN END;
    MakeQualident( genType, outStr );
    IF type.back_end # obj.c_code THEN
      type.back_end:= IsTypeInCBackEnd( genType );
    END;
  END;
END MakeObjectType;

(*------------------------------------------------------------------------*)
PROCEDURE GenVariable ( tobj: obj.TypedObject );
VAR outStr: lstr.String;
    tab: INT;
BEGIN
  BlendLists( tobj.modifier, tobj.type.tobj_modifier );
  Assign( tobj.name^, outStr );
  ExtractSynonymModifier( tobj );
  MakeModifier( tobj.modifier, obj_var, outStr );
  lstr.Append( ': ', outStr );
  tab:= lstr.Length( outStr^ );
  BlendLists( tobj.modifier, tobj.type.tobj_modifier );
  MakeObjectType( tobj, outStr );
  IF msg.WasError THEN RETURN END;
  lstr.Append( ';', outStr );
  IF tobj.mem_class = obj.static THEN
    GenHeadingObject( obj_var, obj.no_change, obj.c_code );
  ELSE
    GenHeadingObject( obj_var, obj.no_change, tobj.type.back_end );
  END;
  IF msg.WasError THEN RETURN END;
  WriteStrLn( outStr^, outStr, 2, 2 + tab );
  OutFile.obj_line:= tobj.line;
  SetCommentTab( outStr, 2, 2 + tab ); lstr.Deallocate(outStr);
END GenVariable;


(*------------------------------------------------------------------------*)
(*			 GENERATE FUNCTION				  *)
(*------------------------------------------------------------------------*)

PROCEDURE SetNameArguments ( vars: adt.List );
VAR elm: adt.Element;
    numberArgument, id: INT;
    nelm: adt.NamedElement;

  PROCEDURE CreateNewArgName ( tobj: obj.TypedObject );

     (*------------------------------------*)
     PROCEDURE NoExistSuchNameArg(): BOOLEAN;
     VAR foundArg: adt.Element;
	 id: INT;
     BEGIN
       vars.Backup( id );
       vars.Find( nelm, foundArg );
       vars.Restore( id );
       RETURN ( foundArg   = NIL )
     END NoExistSuchNameArg;

  (*--------------------------*)
  BEGIN
    REPEAT
      lstr.Assign( 'arg', nelm.name );
      IntToStr( numberArgument, nelm.name );
      INC( numberArgument );
    UNTIL NoExistSuchNameArg();
    tobj.SetName( nelm.name^ );
  END CreateNewArgName;

(*-----------------------*)
BEGIN
  numberArgument:= 0;
  adt.NewNamedElement(nelm, '');
  vars.FindFirst( elm );
  WHILE elm # NIL DO
    vars.Backup( id );
    WITH
       elm: obj.TypedObject DO
	 IF ( elm.name = NIL ) OR ( lstr.Length(elm.name^) = 0 ) THEN
	   CreateNewArgName( elm );
	 END;
      |elm: obj.Comment DO
    ELSE
      Error(28, 'H2DGen.SetNameArguments');
      RETURN;
    END;
    vars.Restore( id );
    vars.FindNext( elm );
  END; adt.Deallocate(nelm);
END SetNameArguments;

(*------------------------------------------------------------------------*)
PROCEDURE MakeFuncArguments ( vars: adt.List; VAR outStr: lstr.String ): INT;
VAR elm: adt.Element;
    id: INT;
    point_comma: BOOLEAN;

  (*-----------------------*)
  PROCEDURE MakeOneArgument ( tobj: obj.TypedObject );
  VAR genType: obj.Type;
  BEGIN
    IF tobj.obj = obj.arguments THEN
      lstr.Append( 'SEQ ', outStr );
      lstr.Append( tobj.name^, outStr );
      lstr.Append( ': SYSTEM.BYTE', outStr );
      RETURN;
    ELSIF tobj.type.type = obj.bt_void THEN
      RETURN;
    END;
    IF tobj.var THEN
      lstr.Append( 'VAR ', outStr );
    END;
    lstr.Append( tobj.name^, outStr );
    lstr.Append( ': ', outStr );
    IF (tobj.type.type = obj.t_array) & (tobj.type.expr = NIL) THEN
      lstr.Append( 'ARRAY OF ', outStr );
      genType:= tobj.type.base;
      BlendLists( tobj.type.modifier, genType.tobj_modifier );
    ELSE
      genType:= tobj.type;
      BlendLists( tobj.modifier, genType.tobj_modifier );
    END;
    IF tobj.type_name # NIL THEN (* обработка модифицированного аргумента *)
      MakeQualident( tobj, outStr );
      IF msg.WasError THEN RETURN END;
    ELSE
      genType:= GetType( genType );
      IF msg.WasError THEN RETURN END;
      MakeQualident( genType, outStr );
    END;
    IF tobj.type.back_end # obj.c_code THEN
      tobj.type.back_end:= IsTypeInCBackEnd( genType );
    END;
  END MakeOneArgument;

(*-----------------------*)
BEGIN
  SetNameArguments( vars );
  IF msg.WasError THEN RETURN obj.no_change END;
  vars.FindFirst( elm );
  vars.Backup( id );
  IF elm = NIL THEN  vars.Restore( id );
    RETURN obj.no_change;
  END;
  IF elm IS obj.TypedObject THEN
    MakeOneArgument( elm(obj.TypedObject) );
    IF msg.WasError THEN RETURN obj.no_change END;
    point_comma:= TRUE;
  ELSIF elm IS obj.Comment THEN
    AddCommentMark(elm(adt.NamedElement).name);
    lstr.Append(elm(adt.NamedElement).name^, outStr);
    lstr.Append(' ', outStr);
    point_comma:= FALSE;
  ELSE
    Error(28, 'H2DGen.MakeFuncArguments');
    RETURN obj.no_change;
  END;
  vars.Restore( id );
  vars.FindNext( elm );
  WHILE elm # NIL DO
    vars.Backup( id );
    IF point_comma THEN
      lstr.Append( '; ', outStr );
    END;
    IF elm IS obj.TypedObject THEN
      MakeOneArgument( elm(obj.TypedObject) );
      point_comma:= TRUE;
    ELSIF elm IS obj.Comment THEN
      AddCommentMark(elm(adt.NamedElement).name);
      lstr.Append(elm(adt.NamedElement).name^, outStr);
      lstr.Append(' ', outStr);
      point_comma:= FALSE;
    END;
    IF msg.WasError THEN RETURN obj.no_change END;
    vars.Restore( id );
    vars.FindNext( elm );
  END;
  RETURN IsAnyTypeInCBackEnd( vars );
END MakeFuncArguments;

(*------------------------------------------------------------------------*)
PROCEDURE MakeFuncReturnValue ( tobj: obj.TypedObject; VAR outStr: lstr.String );
VAR genType, type: obj.Type;
BEGIN
  IF tobj.type_name # NIL THEN
    lstr.Append( ': ', outStr );
    MakeQualident( tobj, outStr );
  ELSE
    type:= tobj.type.base;
    IF ( type = NIL ) OR ( type.type = obj.bt_void ) THEN RETURN END;
    genType:= GetType( type );
    IF msg.WasError THEN RETURN END;
    lstr.Append( ': ', outStr );
    MakeQualident( genType, outStr );
    IF type.back_end # obj.c_code THEN
      type.back_end:= IsTypeInCBackEnd( genType );
    END;
  END;
END MakeFuncReturnValue;

(*------------------------------------------------------------------------*)
PROCEDURE GenFunction ( tobj: obj.TypedObject );
VAR outStr: lstr.String;
    tab, cBackEnd: INT;
BEGIN
  WriteEmptyLine();
  Assign( 'PROCEDURE', outStr );
  ExtractSynonymModifier( tobj );
  MakeModifier( tobj.modifier, obj_procedure, outStr );
  lstr.Append( ' ', outStr );
  lstr.Append( tobj.name^, outStr );
  lstr.Append( ' ( ', outStr );
  tab:= lstr.Length( outStr^ ) - 1;
  cBackEnd:= MakeFuncArguments( tobj.type.vars, outStr );
  IF msg.WasError THEN RETURN END;
  lstr.Append( ' )', outStr );
  BlendLists( tobj.type.modifier, tobj.type.base.tobj_modifier );
  MakeFuncReturnValue( tobj, outStr );
  IF msg.WasError THEN RETURN END;
  lstr.Append( ';', outStr );
  IF cBackEnd # obj.c_code THEN
    cBackEnd:= IsTypeInCBackEnd( tobj.type );
  END;
  GenHeadingObject( obj_procedure, obj.no_change, cBackEnd );
  fio.DivideChars:= divideCharsForProcedure;
  WriteStrLn( outStr^, outStr, 0, tab );
  fio.DivideChars:= stdDivideChars;
  OutFile.obj_line:= tobj.line;
  OutFile.tab:= 0;
END GenFunction;



(*------------------------------------------------------------------------*)
(*			    GENERATE CONSTANTS				  *)
(*------------------------------------------------------------------------*)
PROCEDURE QuotationStr( ch:CHAR; str-: ARRAY OF CHAR; VAR outStr: lstr.String);
BEGIN
  lstr.AppendChar( ch, outStr );
  lstr.Append( str, outStr );
  lstr.AppendChar( ch, outStr );
END QuotationStr;

(*------------------------------------------------------------------------*)
PROCEDURE MakeStringConst ( result: obj.ConstantValue; VAR outStr: lstr.String);
VAR size, i: INT;

  (*------------------------------*)
  PROCEDURE MakeRepresentChars( str-: ARRAY OF CHAR );
  VAR buffer: lstr.String;
      beginPos: INT;
      quote: CHAR;
      codeOfQuote: ARRAY 5 OF CHAR;

     (*------------------------------*)
     PROCEDURE SetQuote();
     BEGIN
       quote:= "'";
       codeOfQuote:= '42C';
       IF ( lstr.FindPos( str, "'" ) # lstr.none ) &
	  ( lstr.FindPos( str, '"' ) = lstr.none )   THEN
	 quote:= '"';
	 codeOfQuote:= '47C';
       END;
     END SetQuote;
     (*------------------------------*)

  BEGIN
    beginPos:= i;
    SetQuote();
    WHILE ( i < size ) & ( cfg.IsRepresentativeChar(ORD(str[i])) )  DO
      IF str[i] = quote THEN
	IF i # beginPos THEN
	  lstr.Extract( str, beginPos, i-beginPos, buffer );
	  lstr.Append( ' + ', outStr );
	  QuotationStr( quote, buffer^, outStr );
	END;
	IF i # 0 THEN
	  lstr.Append( ' + ', outStr );
	END;
	lstr.Append( codeOfQuote, outStr );
	buffer:= NIL;
	beginPos:= i+1;
      END;
      INC( i );
    END;
    IF i # beginPos THEN
      IF beginPos = 0 THEN
	lstr.Extract( str, beginPos, i-beginPos, buffer );
	QuotationStr( quote, buffer^, outStr );
      ELSE
	lstr.Extract( str, beginPos, i-beginPos, buffer );
	lstr.Append( ' + ', outStr );
	QuotationStr( quote, buffer^, outStr );
      END;
    END;
  END MakeRepresentChars;

  (*------------------------------*)
  PROCEDURE OctalCode ( ch: CHAR );
  VAR decCode, modulus, pos: INT;
      string1: ARRAY 2 OF CHAR;
  BEGIN
    decCode:= ORD( ch );
    string1[1]:= fio.EOS;
    lstr.Append( 'C', outStr );
    pos:= lstr.Length( outStr^ ) - 1;
    WHILE decCode # 0 DO
      modulus:= decCode MOD 8;
      string1[0]:= CHR( modulus + ORD('0') );
      lstr.Insert( string1, pos, outStr );
      decCode:= decCode DIV 8;
    END;
  END OctalCode;

  (*------------------------------*)
  PROCEDURE MakeNoRepresentChars( str-: ARRAY OF CHAR );
  BEGIN
    WHILE ( i < size ) & ( ~cfg.IsRepresentativeChar(ORD(str[i])) ) DO
      IF i # 0 THEN
	lstr.Append( ' + ', outStr );
      END;
      OctalCode( str[i] );
      INC( i );
    END;
  END MakeNoRepresentChars;
  (*------------------------------*)

BEGIN
  size:= lstr.Length( result.str^ );
  i:= 0;
  IF size = 0 THEN
    lstr.Append( "''", outStr );
  ELSE
    WHILE i < size DO
      MakeRepresentChars( result.str^ );
      MakeNoRepresentChars( result.str^ );
    END;
  END;
END MakeStringConst;

(*------------------------------------------------------------------------*)
PROCEDURE MakeBitSet(uint: CARD; VAR outStr: lstr.String);
VAR
  bit: INT;
  was_comma: BOOLEAN;
  bitstr: ARRAY 5 OF CHAR;
BEGIN
  bit:= 0;
  lstr.Append('{', outStr);
  was_comma:= TRUE;
  WHILE uint > 0 DO
    IF (uint MOD 2) = 1 THEN
      IF ~was_comma THEN
	lstr.Append(', ', outStr);
      END;
      WholeStr.IntToStr(bit, bitstr);
      lstr.Append(bitstr, outStr);
      was_comma:= FALSE;
    END;
    uint:= uint DIV 2;
    INC(bit);
  END;
  lstr.Append('}', outStr);
END MakeBitSet;


(*------------------------------------------------------------------------*)
PROCEDURE GenUnknownConst( tobj: obj.TypedObject; result: obj.ConstantValue;
                           VAR outStr: lstr.String );
VAR s: lstr.String;
BEGIN
  CASE result.type OF
    obj.real_const:
      s := result.GetText(TRUE);
      lstr.Append( s^, outStr );
   |obj.int_const:
      s := result.GetText(TRUE);
      lstr.Append( s^, outStr );
   |obj.uint_const:
      s := result.GetText(TRUE);
      lstr.Append( s^, outStr );
   |obj.str_const:
     MakeStringConst( result, outStr );
  END;
END GenUnknownConst;

-----------------------------------------------------------
PROCEDURE GenBoolConst( tobj: obj.TypedObject; result: obj.ConstantValue;
                        VAR outStr: lstr.String );
BEGIN
  IF (tobj.type_name.type # obj.bt_BOOLEAN)  THEN
    lstr.Append(tobj.type_name.name^ , outStr); lstr.Append("(", outStr);
  END;
  CASE result.type OF
   |obj.int_const:
     IF (result.int = 0) THEN lstr.Append("FALSE", outStr);
     ELSE lstr.Append("TRUE", outStr);
     END;
   |obj.uint_const:
     IF (result.uint = 0) THEN lstr.Append("FALSE", outStr);
     ELSE lstr.Append("TRUE", outStr);
     END;
  ELSE GenUnknownConst(tobj, result, outStr);
  END;
  IF (tobj.type_name.type # obj.bt_BOOLEAN)  THEN
    lstr.Append(")", outStr);
  END;
END GenBoolConst;

-----------------------------------------------------------
PROCEDURE GenSetConst( tobj: obj.TypedObject; result: obj.ConstantValue;
                       VAR outStr: lstr.String );
BEGIN
  IF (tobj.type_name.type # obj.bt_BITSET)  THEN
    lstr.Append(tobj.type_name.name^ , outStr);
  END;
  CASE result.type OF
   |obj.int_const:
     MakeBitSet(sys.VAL(CARD,result.int), outStr);
   |obj.uint_const:
     MakeBitSet(result.uint, outStr);
  ELSE GenUnknownConst(tobj, result, outStr);
  END;
END GenSetConst;

-----------------------------------------------------------
PROCEDURE GenIntConst( tobj: obj.TypedObject; result: obj.ConstantValue;
                       VAR outStr: lstr.String );
BEGIN
  lstr.Append(tobj.type_name.name^ , outStr);
  lstr.Append("(", outStr);
  GenUnknownConst(tobj, result, outStr);
  lstr.Append(")", outStr);
END GenIntConst;

-----------------------------------------------------------
PROCEDURE GenCardConst( tobj: obj.TypedObject; result: obj.ConstantValue;
                        VAR outStr: lstr.String );
BEGIN
  lstr.Append(tobj.type_name.name^ , outStr);
  lstr.Append("(", outStr);
  GenUnknownConst(tobj, result, outStr);
  lstr.Append(")", outStr);
END GenCardConst;

-----------------------------------------------------------
PROCEDURE GenRealConst( tobj: obj.TypedObject; result: obj.ConstantValue;
                        VAR outStr: lstr.String );
BEGIN
  lstr.Append("VAL(", outStr);
  lstr.Append(tobj.type_name.name^ , outStr);
  lstr.Append(", ", outStr);
  GenUnknownConst(tobj, result, outStr);
  lstr.Append(")", outStr);
END GenRealConst;

-----------------------------------------------------------
PROCEDURE GenCharConst( tobj: obj.TypedObject; result: obj.ConstantValue;
                        VAR outStr: lstr.String );
BEGIN
  CASE result.type OF
   |obj.int_const, obj.uint_const:
     lstr.Append("CHR(", outStr);
     GenUnknownConst(tobj, result, outStr);
     lstr.Append(")", outStr);
   |obj.str_const:
     GenUnknownConst(tobj, result, outStr);
  ELSE
     lstr.Append(tobj.type_name.name^ , outStr);
     lstr.Append("(", outStr);
     GenUnknownConst(tobj, result, outStr);
     lstr.Append(")", outStr);
  END;
END GenCharConst;

CONST
  buf_size = 256; -- размер буфера для представления чисел по битам

(*------------------------------------------------------------------------*)
PROCEDURE CheckBoolConst( tobj: obj.TypedObject; result: obj.ConstantValue );
BEGIN
  IF (result.type # obj.int_const) & (result.type # obj.uint_const) THEN
    HError( 68, tobj, tobj.type_name.name^, tobj.name^);
  END;
END CheckBoolConst;

-----------------------------------------------------------
PROCEDURE CheckSetConst( tobj: obj.TypedObject; result: obj.ConstantValue );
VAR buf: ARRAY buf_size OF CHAR;
BEGIN
  CASE result.type OF
   |obj.int_const:
     io.sprintf(buf, "%b", result.int);
   |obj.uint_const:
     io.sprintf(buf, "%b", result.uint);
  ELSE
    HError( 68, tobj, tobj.type_name.name^, tobj.name^); RETURN;
  END;
  IF (lstr.Length(buf) > (tobj.type_name.type_size * 8)) THEN
    HError( 69, tobj, tobj.name^, tobj.type_name.name^ );
  END;
END CheckSetConst;

-----------------------------------------------------------
PROCEDURE CheckIntConst( tobj: obj.TypedObject; result: obj.ConstantValue );
VAR buf: ARRAY buf_size OF CHAR;
BEGIN
  CASE result.type OF
   |obj.int_const:
     IF (result.int < 0) THEN
       io.sprintf(buf, "%b", -result.int);
     ELSE
       io.sprintf(buf, "%b", result.int);
     END;
   |obj.uint_const:
     io.sprintf(buf, "%b", result.uint);
  ELSE
    HError( 68, tobj, tobj.type_name.name^, tobj.name^); RETURN;
  END;
  IF (lstr.Length(buf) > ((tobj.type_name.type_size * 8) - 1) ) THEN
    HError( 69, tobj, tobj.name^, tobj.type_name.name^ );
  END;
END CheckIntConst;

-----------------------------------------------------------
PROCEDURE CheckCardConst( tobj: obj.TypedObject; result: obj.ConstantValue );
VAR buf: ARRAY buf_size OF CHAR;
BEGIN
  CASE result.type OF
   |obj.int_const:
     IF (result.int < 0) THEN
       HError( 68, tobj, tobj.type_name.name^, tobj.name^);
     ELSE
       io.sprintf(buf, "%b", result.int);
     END;
   |obj.uint_const:
     io.sprintf(buf, "%b", result.uint);
  ELSE
    HError( 68, tobj, tobj.type_name.name^, tobj.name^); RETURN;
  END;
  IF (lstr.Length(buf) > (tobj.type_name.type_size * 8)) THEN
    HError( 69, tobj, tobj.name^, tobj.type_name.name^ );
  END;
END CheckCardConst;

-----------------------------------------------------------
PROCEDURE CheckRealConst( tobj: obj.TypedObject; result: obj.ConstantValue );
BEGIN
  CASE result.type OF
   |obj.int_const, obj.uint_const:
   |obj.real_const:
     IF (tobj.type_name.type_size <= 4) THEN
       IF (result.real > MAX(REAL)) OR (result.real < MIN(REAL)) THEN
         HError( 69, tobj, tobj.name^, tobj.type_name.name^ ); RETURN;
       END;
     ELSIF (tobj.type_name.type_size <= 8) THEN
       IF (result.real > MAX(LONGREAL)) OR (result.real < MIN(LONGREAL)) THEN
         HError( 69, tobj, tobj.name^, tobj.type_name.name^ ); RETURN;
       END;
     END;
  ELSE
    HError( 68, tobj, tobj.type_name.name^, tobj.name^);
  END;
END CheckRealConst;

-----------------------------------------------------------
PROCEDURE CheckCharConst( tobj: obj.TypedObject; result: obj.ConstantValue );
VAR buf: ARRAY buf_size OF CHAR;
BEGIN
  CASE result.type OF
   |obj.int_const:
      IF (result.int < 0) THEN
        HError( 69, tobj, tobj.name^, tobj.type_name.name^); RETURN;
      END;
      io.sprintf(buf, "%b", result.int);
   |obj.uint_const:
      io.sprintf(buf, "%b", result.uint);
   |obj.str_const:
      IF (lstr.Length1(result.str) # 1) THEN
        HError( 68, tobj, tobj.type_name.name^, tobj.name^); RETURN;
      END;
      RETURN;
  ELSE
    HError( 68, tobj, tobj.type_name.name^, tobj.name^); RETURN;
  END;
  IF (lstr.Length(buf) > (tobj.type_name.type_size * 8)) THEN
    HError( 69, tobj, tobj.name^, tobj.type_name.name^ );
  END;
END CheckCharConst;

(*------------------------------------------------------------------------*)
PROCEDURE GenConstant( tobj: obj.TypedObject );
VAR outStr, s: lstr.String;
    result: obj.ConstantValue;
    tab: INT;

  (*---------------------------------------*)
  PROCEDURE ThinkAboutCharConst ( VAR result: obj.ConstantValue );
  BEGIN
    IF ( tobj.expr IS obj.Value ) &
       ( tobj.expr(obj.Value).type = obj.char_const ) THEN
      obj.Deallocate(result); obj.NewConstantValue( result );
      result.type:= obj.str_const;
      Assign( tobj.expr(obj.Value).value^, result.str );
    END;
  END ThinkAboutCharConst;


  PROCEDURE genConstant(backEnd: INT);
     PROCEDURE genModifiedConst();
     BEGIN
       IF    (tobj.type_name.type_kind = obj.tk_bool) THEN
         CheckBoolConst(tobj, result); IF msg.WasError THEN RETURN END;
         GenBoolConst(tobj, result, outStr);
       ELSIF (tobj.type_name.type_kind = obj.tk_set) THEN
         CheckSetConst(tobj, result);  IF msg.WasError THEN RETURN END;
         GenSetConst(tobj, result, outStr);
       ELSIF (tobj.type_name.type_kind = obj.tk_unsigned) THEN
         CheckCardConst(tobj, result); IF msg.WasError THEN RETURN END;
         GenCardConst(tobj, result, outStr);
       ELSIF (tobj.type_name.type_kind = obj.tk_signed) THEN
         CheckIntConst(tobj, result);  IF msg.WasError THEN RETURN END;
         GenIntConst(tobj, result, outStr);
       ELSIF (tobj.type_name.type_kind = obj.tk_real) THEN
         CheckRealConst(tobj, result); IF msg.WasError THEN RETURN END;
         GenRealConst(tobj, result, outStr);
       ELSIF (tobj.type_name.type_kind = obj.tk_char) THEN
         CheckCharConst(tobj, result); IF msg.WasError THEN RETURN END;
         GenCharConst(tobj, result, outStr);
       ELSE  GenUnknownConst(tobj, result, outStr);
       END;
     END genModifiedConst;
  BEGIN
    ThinkAboutCharConst( result );
    GenHeadingObject( obj_const, obj.no_change, backEnd);
    Assign( tobj.name^, outStr );
    lstr.Append( ' = ' , outStr );
    tab:= lstr.Length( outStr^ );
    IF (tobj.type_name # NIL) THEN genModifiedConst()
    ELSE  GenUnknownConst(tobj, result, outStr);
    END;
    lstr.Append( ';', outStr );
    WriteStrLn( outStr^, outStr, 2, tab + 2 );
  END genConstant;

  --------------------------------------
  PROCEDURE genReadOnlyVariable();
     PROCEDURE genModifiedConst();
     BEGIN
       IF    (tobj.type_name.type_kind = obj.tk_bool) THEN
         CheckBoolConst(tobj, result); IF msg.WasError THEN RETURN END;
       ELSIF (tobj.type_name.type_kind = obj.tk_set) THEN
         CheckSetConst(tobj, result);  IF msg.WasError THEN RETURN END;
       ELSIF (tobj.type_name.type_kind = obj.tk_signed) THEN
         CheckIntConst(tobj, result);  IF msg.WasError THEN RETURN END;
       ELSIF (tobj.type_name.type_kind = obj.tk_unsigned) THEN
         CheckCardConst(tobj, result); IF msg.WasError THEN RETURN END;
       ELSIF (tobj.type_name.type_kind = obj.tk_real) THEN
         CheckRealConst(tobj, result); IF msg.WasError THEN RETURN END;
       ELSIF (tobj.type_name.type_kind = obj.tk_char) THEN
         CheckCharConst(tobj, result); IF msg.WasError THEN RETURN END;
       END;
       MakeQualident( tobj, outStr );
     END genModifiedConst;
  --------------------------------------
  BEGIN
    GenHeadingObject( obj_var, obj.no_change, obj.c_code );
    Assign( tobj.name^, outStr );
    lstr.Append( ' - : ' , outStr );
    tab:= lstr.Length( outStr^ );
    IF (tobj.type_name # NIL) THEN  genModifiedConst();
    ELSE
      CASE result.type OF
        obj.int_const:
          IF (tobj.expr IS obj.Value) &
             (tobj.expr(obj.Value).type = obj.char_const)
          THEN
            lstr.Append( m2_CHAR, outStr );
          ELSE
            lstr.Append( m2_INTEGER, outStr );
          END;
        |obj.real_const:
          lstr.Append( m2_REAL, outStr );
        |obj.uint_const:
          IF (tobj.expr IS obj.Value) &
             (tobj.expr(obj.Value).type = obj.char_const)
          THEN
            lstr.Append( m2_CHAR, outStr );
          ELSE
            lstr.Append( m2_CARDINAL, outStr );
          END;
        |obj.str_const:
          lstr.Append( "ARRAY [0..", outStr );
          IntToStr(lstr.Length(result.str^), outStr);
          lstr.Append( "] OF CHAR", outStr );
      END;
    END;
    lstr.Append( ';', outStr );
    WriteStrLn( outStr^, outStr, 2, tab + 2 );
  END genReadOnlyVariable;

(*---------------------------------------*)
BEGIN
  IF (tobj.generated) OR (tobj.header # OutFile.header) THEN RETURN
  ELSE tobj.generated:= TRUE;
  END;
  tobj.expr.ComputeExpression( result, FALSE );
  IF msg.WasError THEN RETURN END;
  IF cfg.ConstantsToVariables THEN
    genReadOnlyVariable();
    IF ~msg.WasError THEN
      genConstant(obj.native);
    END;
  ELSE
    genConstant(obj.common);
  END;
  obj.Deallocate(result);
  IF msg.WasError THEN RETURN END;
  OutFile.obj_line:= tobj.line;
  SetCommentTab( outStr, 2, tab + 2 ); lstr.Deallocate(outStr); lstr.Deallocate(s);
END GenConstant;

(*------------------------------------------------------------------------*)
PROCEDURE GenConstantSynonym( tobj: obj.TypedObject );
VAR outStr: lstr.String;
    tab: INT;
BEGIN
  GenHeadingObject( obj_const, obj.no_change, tobj.back_end );
  Assign( tobj.name^, outStr );
  lstr.Append( ' = ' , outStr );
  tab:= lstr.Length( outStr^ );
  lstr.Append( tobj.type.name^, outStr );
  lstr.Append( ';', outStr );
  WriteStrLn( outStr^, outStr, 2, tab + 2 );
  OutFile.obj_line:= tobj.line;
  SetCommentTab( outStr, 2, tab + 2 );
END GenConstantSynonym;


(*------------------------------------------------------------------------*)
(*			     GENERATE MACRO				  *)
(*------------------------------------------------------------------------*)

PROCEDURE MakeMacro ( tobj: obj.Macro; VAR outStr: lstr.String;
		      mode: INT): INT;
VAR tab: INT;

  (*-------------------------*)
  PROCEDURE MakeParametrs ( params: adt.List );
  VAR elm: adt.Element;
  BEGIN
    params.FindFirst( elm );
    IF elm # NIL THEN
      lstr.Append( elm(adt.NamedElement).name^, outStr );
      params.FindNext( elm );
      WHILE elm # NIL DO
	lstr.Append( ', ', outStr );
	lstr.Append( elm(adt.NamedElement).name^, outStr );
	params.FindNext( elm );
      END;
      lstr.Append( ': ', outStr );
      lstr.Append( kw_array_of_byte, outStr );
    END;
  END MakeParametrs;

  (*-------------------------*)
  PROCEDURE MakeReturnedValue ( );
  BEGIN
  END MakeReturnedValue;

(*-------------------------*)
BEGIN

  Assign( 'PROCEDURE ', outStr );
  IF mode = obj.native THEN
    lstr.Append( ' / ', outStr );
  ELSIF mode = obj.c_code THEN
    lstr.Append( cdecl, outStr );
    lstr.Append( ' ', outStr );
  END;
  lstr.Append( tobj.name^, outStr );
  lstr.Append( ' ', outStr );
  lstr.Append( '( ', outStr );
  tab:= lstr.Length( outStr^ );
  MakeParametrs( tobj.params );
  lstr.Append(')', outStr);
  MakeReturnedValue( );
  lstr.Append( ';', outStr );
  RETURN tab;
END MakeMacro;

(*------------------------------------------------------------------------*)
PROCEDURE GenMacro ( tobj: obj.Macro );
VAR outStr, comment, comment_c: lstr.String;
    foundMacro: adt.Element;
    tab: INT;
BEGIN
  GenHeadingObject( obj_macro, obj.no_change, obj.c_code );
  WriteEmptyLine();
--  OutFile.header.Names.Find( tobj, foundMacro );
  obj.FindInNames( OutFile.header, tobj, foundMacro );
  Assign( '(*', comment );
  lstr.Append( comment_for_macro, comment );
  IF foundMacro = NIL THEN
    lstr.Append( '*)', comment );
    lstr.Assign( comment^, comment_c );
    WriteStrLn( comment^, comment, 0, 0 );
  ELSE
    lstr.Assign( comment^, comment_c );
    WriteStrLn( comment^, comment, 0, 0 );
    WriteStrLn( '   H2D: name already exists.', null, 0, 0 );
  END;
  tab:= MakeMacro( tobj, outStr, obj.common );
  fio.DivideChars:= divideCharsForProcedure;
  WriteStrLn( outStr^, outStr, 0, tab );
  fio.DivideChars:= stdDivideChars;
  IF foundMacro # NIL THEN
    WriteStrLn( '*)', null, 0, 0 );
  END;

  IF cfg.GenerateMacrosFile  THEN
    GenHeadingObject( obj_macro, obj.no_change, obj.native );
    WriteEmptyLine();
    IF foundMacro = NIL THEN
      WriteStrLn( comment_c^, comment_c, 0, 0 );
    ELSE
      WriteStrLn( comment_c^, comment_c, 0, 0 );
      WriteStrLn( '   H2D: name already exists.', null, 0, 0 );
    END;
    tab:= MakeMacro( tobj, outStr, obj.native );
    fio.DivideChars:= divideCharsForProcedure;
    WriteStrLn( outStr^, outStr, 0, tab );
    fio.DivideChars:= stdDivideChars;
    IF foundMacro # NIL THEN
      WriteStrLn( '*)', null, 0, 0 );
    END;
  ELSE
    lstr.Deallocate(comment_c);
  END;
  OutFile.obj_line:= tobj.line;
  OutFile.tab:= 0; lstr.Deallocate(outStr);
END GenMacro;



(*------------------------------------------------------------------------*)
(*			       GENERATE OBJECTS 			  *)
(*------------------------------------------------------------------------*)

PROCEDURE GenTypedObject ( tobj: obj.TypedObject );
VAR comment: obj.Comment;
BEGIN
  CASE tobj.obj OF
    obj.variable:
      IF IsFunction( tobj.type, tobj.modifier ) THEN
        GenFunction( tobj );
      ELSE
	GenVariable( tobj );
      END;
--   |obj.constant, obj.bitset_const:
   |obj.constant:
      IF (tobj.generated) & (tobj.header = OutFile.header) THEN
        obj.NewComment(comment, ins_empty_line_before, "Constant '");
        lstr.Append( tobj.name^, comment.name );
        lstr.Append( "' was declared here in the source file", comment.name );
        GenComment( comment );
        lstr.Deallocate( comment.name );
     ELSE
       GenConstant( tobj );
     END;
   |obj.synonym:
     GenConstantSynonym( tobj );
  ELSE
    Error(28, 'H2DGen.GenTypedObject');
  END;
END GenTypedObject;

(*------------------------------------------------------------------------*)
PROCEDURE CheckName(ne: adt.NamedElement);
VAR e: adt.Element;
    tmp_ne: adt.NamedElement;
    object: obj.Object;
BEGIN
  Modula2KeyWords.Find(ne, e);
  IF e # NIL THEN
    (* error *)
    adt.NewNamedElement( tmp_ne, '');
    WITH
      ne: obj.TypedObject DO
        IF ne.obj = obj.constant THEN
          lstr.Assign( "H2D_Const_", tmp_ne.name );
          lstr.Append( ne.name^, tmp_ne.name );
        ELSE
          HError(49, ne, ne.name^ );
        END;
     |ne: obj.Type DO
        lstr.Assign( "H2D_Type_", tmp_ne.name );
        lstr.Append( ne.name^, tmp_ne.name );
     |ne: obj.Macro DO
        lstr.Assign( "H2D_Macro_", tmp_ne.name );
        lstr.Append( ne.name^, tmp_ne.name );
    ELSE
      NEW(object);
      object.header:= OutFile.header;
      object.line:= 0;
      HError(49, object, ne.name^ );
    END;
    IF lstr.Length1(tmp_ne.name) > 0  THEN
      IF RepairName(tmp_ne) THEN
        HGenDefine( tmp_ne.name^, ne.name^);
        lstr.Assign( tmp_ne.name^, ne.name);
      ELSE
        HError(49, ne(obj.Object), ne.name^ );
      END;
      adt.Deallocate(tmp_ne);
    END;
  END;
END CheckName;

(*------------------------------------------------------------------------*)
PROCEDURE GenObjects ( objects: adt.List );
VAR elm: adt.Element;
    comment: obj.Comment;
    id: INT;
BEGIN
  objects.FindFirst( elm );
  WHILE elm # NIL DO
    objects.Backup( id );
    WITH
      elm: obj.TypedObject DO
        CheckName(elm); IF msg.WasError THEN RETURN END;
        GenTypedObject( elm )
     |elm: obj.Type	   DO
        CheckName(elm); IF msg.WasError THEN RETURN END;
        IF elm.generated THEN
          obj.NewComment(comment, ins_empty_line_before, "Type '");
          lstr.Append( elm.name^, comment.name );
          lstr.Append( "' was declared here in the source file", comment.name );
          GenComment( comment );
          lstr.Deallocate( comment.name );
        ELSE
          GenType( elm );
        END;
     |elm: obj.Macro	   DO
        CheckName(elm); IF msg.WasError THEN RETURN END;
        GenMacro( elm );
     |elm: obj.Comment	   DO	 GenComment( elm );
    ELSE
      Error(28, 'H2DGen.GenObjects');
      RETURN;
    END;
    IF msg.WasError THEN RETURN END;
    objects.Restore( id );
    objects.FindNext( elm );
  END;
END GenObjects;

(*------------------------------------------------------------------------*)
PROCEDURE GenModuleEnd ( );
VAR str: lstr.String;
BEGIN
  EmitSection( obj.common );
  WriteEmptyLine();
  Assign( 'END ', str );
  lstr.Append( OutFile.modname^, str );
  lstr.AppendChar( '.', str );
  WriteStrLn( str^, str, 0, 0 );
  WriteEmptyLine();
  lstr.Deallocate(str);
END GenModuleEnd;

(*------------------------------------------------------------------------*)
PROCEDURE CopyTypeNameFromOtherFile( elm: obj.Type; VAR to: adt.Tree );
VAR e: adt.Element;
    node: GeneratedType;
BEGIN
  (* if tag is defined in other header then no need to insert it
     im tree for compatible types searche *)
  IF (  elm.type = obj.t_synonym  ) OR
     ( elm.tag_description_header = NIL ) OR
     ( elm.tag_description_header = elm.header)
  THEN
    IF ( elm.type # obj.t_synonym ) & ( UniqueTagName(elm) = no ) THEN
      ResolveCollisionOfName( elm );
    END;
    NewGeneratedType( node, elm );
    to.Find(node, e);
    IF e = NIL THEN
      IF ~IsPointerToBaseType(node.type) THEN to.Insert( node ); END;
    ELSE
      Deallocate(node);
    END;
  END;
END CopyTypeNameFromOtherFile;

(*------------------------------------------------------------------------*)
PROCEDURE CopyTypeNameFromCurrentFile ( elm: obj.Type; VAR to: adt.Tree );
VAR e: adt.Element;
    node: GeneratedType;
    synonym: obj.Type;
BEGIN
  IF ( elm.type # obj.t_synonym ) & ( UniqueTagName(elm) = no ) THEN
    ResolveCollisionOfName( elm );
  END;
  NewGeneratedType( node, elm );
  to.Find(node, e);
  IF e = NIL THEN
    IF ~IsPointerToBaseType(node.type) THEN to.Insert( node ); END;
  ELSE
    WITH e: GeneratedType DO
      IF (  e.type.header # NIL) &              -- найденный тип
         ( (e.type.header # OutFile.header) OR  -- из другого файла или
           (e.type.line > elm.line)             -- оисан после типа elm
         )
      THEN
        IF elm.type # obj.t_synonym THEN
          obj.NewType( synonym );
          synonym.SetName( elm.name^ );
          synonym.type:= obj.t_synonym;
          synonym.base:= elm;
          synonym.line:= elm.line;
        END;
        e.type:= elm;
      END;
    END;
    Deallocate(node);
  END;
END CopyTypeNameFromCurrentFile;

(*------------------------------------------------------------------------*)
PROCEDURE CopyTypeNameFromTree( from: adt.Tree; VAR to: adt.Tree);
VAR elm: adt.Element;
BEGIN
  IF from = NIL THEN RETURN END;
  IF to = NIL THEN  adt.NewTree( to ) END;
  from.FindFirst(elm);
  WHILE elm # NIL DO
    WITH elm: obj.Type DO
      IF ( elm.header = OutFile.header ) OR
         ( ( elm.tag_description_header # NIL ) &
           ( elm.tag_description_header = elm.header)
         )
      THEN
        CopyTypeNameFromCurrentFile(elm, to);
      ELSE
        CopyTypeNameFromOtherFile(elm, to);
      END;
    ELSE
    END;
    from.FindNext(elm);
  END;
END CopyTypeNameFromTree;

(*------------------------------------------------------------------------*)
PROCEDURE ExtractNamedTypes ( header: obj.Header );
(* OutFile must be initialised *)
VAR e, h: adt.Element;
BEGIN
  IF (header = NIL) THEN RETURN END;
  CopyTypeNameFromTree(header.Names, GeneratedTypes);
  CopyTypeNameFromTree(header.Tags, GeneratedTypes);
  ListForExtractNamedTypes.Insert(header);
  header.includes.FindFirst(e);
  WHILE e # NIL DO
    ListForExtractNamedTypes.Find(e, h);
    IF h = NIL THEN
      ExtractNamedTypes(e(obj.Header));
    END;
    header.includes.FindNext(e);
  END;
  ListForExtractNamedTypes.FindFirst(h);
  IF h = header THEN ListForExtractNamedTypes.Clean() END;

<* IF h2ddebug THEN *>
  io.printf("  GeneratedTypes\n");
  GeneratedTypes.FindFirst(e);
  WHILE e # NIL DO
    io.printf("    type_name = %s; code = %s\n", e(GeneratedType).type.name^, e(GeneratedType).name^);
    GeneratedTypes.FindNext(e);
  END;
<* END *>

END ExtractNamedTypes;

(*------------------------------------------------------------------------*)
PROCEDURE GenMacroFiles ( header: obj.Header );
VAR elm: adt.Element;
    id, tab: INT;

  (*---------------------------------------*)
  PROCEDURE GenNativeMacro ( macro: obj.Macro );
  VAR outStr, outStr_c, str: lstr.String;
      pos: CARD;
      finded: BOOLEAN;
  BEGIN
    OutFile:= MacroDefFile;
    tab:= MakeMacro( macro, outStr, obj.c_code );
    lstr.Assign( outStr^, outStr_c);
    WriteEmptyLine();
    WriteStrLn( outStr^, outStr, 0, tab);

    OutFile:= MacroModFile;
    WriteEmptyLine();
    WriteStrLn( outStr_c^, outStr_c, 0, tab );
    WriteStrLn( '(* ', null, 0, 0 );
    Assign( macro.text^, outStr );
    fio.DivideChars:= nothingDivideChars;
    Strings.FindNext( '\', outStr^, 0, finded, pos );
    WHILE finded DO
      lstr.Extract( outStr^, 0, pos+1, str );
      lstr.Delete( outStr^, 0, pos+1 );
      WriteStrLn( str^, str, 0, 0 );
      Strings.FindNext( '\', outStr^, 0, finded, pos );
    END;
    WriteStrLn( outStr^, outStr, 0, 0 );
    WriteStrLn( '*)', null, 0, 0 );
    fio.DivideChars:= stdDivideChars;
    WriteStrLn( 'BEGIN', null, 0, 0 );
    Assign( 'END ', outStr );
    lstr.Append( macro.name^, outStr );
    lstr.Append( ';', outStr );
    WriteStrLn( outStr^, outStr, 0, 0 );
  END GenNativeMacro;

(*---------------------------------------*)
BEGIN
    NewFILE( MacroDefFile, header, macro_def_module );
    OutFile:= MacroDefFile;
    IF msg.WasError THEN RETURN END;
    OpenFile();
    IF msg.WasError THEN RETURN END;
    GenModuleTitle();
    IF msg.WasError THEN RETURN END;
    WriteEmptyLine();
    WriteStrLn( 'IMPORT SYSTEM;', null, 0, 0 );

    NewFILE( MacroModFile, header, macro_mod_module );
    OutFile:= MacroModFile;
    IF msg.WasError THEN RETURN END;
    OpenFile();
    IF msg.WasError THEN RETURN END;
    GenModuleTitle();
    IF msg.WasError THEN RETURN END;
    WriteEmptyLine();
    WriteStrLn( 'IMPORT SYSTEM;', null, 0, 0 );

    header.Macros.FindFirst( elm );
    WHILE elm # NIL DO
      header.Macros.Backup( id );
      GenNativeMacro( elm(obj.Macro) );
      header.Macros.Restore( id );
      header.Macros.FindNext( elm );
    END;

    OutFile:= MacroDefFile;
    GenModuleEnd();
    IF msg.WasError THEN RETURN END;
    CloseFile();

    OutFile:= MacroModFile;
    GenModuleEnd();
    IF msg.WasError THEN RETURN END;
    CloseFile();
    OutFile:= StdDefFile;
END GenMacroFiles;

(*------------------------------------------------------------------------*)
PROCEDURE InitImport(header: obj.Header);
VAR elm, found: adt.Element;
    imp: adt.NamedElement;
    modname: lstr.String;
BEGIN
  adt.NewList(Import);
  GetModuleName(header.name^, modname, std_def_module);
  header.includes.FindFirst( elm );
  WHILE elm # NIL DO
    NEW(imp);
    GetModuleName( elm(adt.NamedElement).name^, imp.name, std_def_module );
    IF msg.WasError THEN RETURN END;
    Import.Find( imp, found );
    IF ( imp.name^ = modname^ ) OR ( found # NIL ) THEN
      Error(66, imp.name^); RETURN;
    END;
    CheckName(imp); IF msg.WasError THEN RETURN END;
    Import.Insert( imp );
    header.includes.FindNext( elm );
  END;
  adt.NewNamedElement(imp, ModuleSystem.name^);
  Import.Find( imp, found );
  IF found = NIL THEN  Import.Insert(imp); END;

  adt.NewList(AdtImport);
  obj.M2Types.FindFirst(elm);
  WHILE elm # NIL DO
    NEW(imp);
    ExtractModName(elm(obj.M2Type).name^, imp.name);
    IF imp.name # NIL THEN
      Import.Find(imp, found);
      IF found = NIL THEN
        Import.Insert(imp);
        AdtImport.Insert(imp);
      END;
    END;
    obj.M2Types.FindNext(elm);
  END;
  lstr.Deallocate(modname);
END InitImport;

(*------------------------------------------------------------------------*)
PROCEDURE InitWriteBuffer();
BEGIN
  adt.NewList(WriteBuffer);
  WriteStrLn:= WriteStrLn2File;
  WriteStr  := WriteStr2File;
END InitWriteBuffer;

(*------------------------------------------------------------------------*)
PROCEDURE InitGenerater ( header: obj.Header );
BEGIN
  InitWriteBuffer();
  adt.NewStack( NameOfGeneratedType );
  adt.NewList(TagDefinedInOtherHeader);
  HFile:= NIL;
  InsertEmptyLine:= TRUE;
  InitImport(header);
  NewFILE( OutFile, header, std_def_module );
  IF msg.WasError THEN RETURN END;
  StdDefFile:= OutFile;
  OpenFile();
  IF msg.WasError THEN RETURN END;
  adt.NewTree( GeneratedTypes );
  GenTypeDef:= TRUE;
  BackEndSection:= obj.common;
  OSectionNative:= none;
  OSectionCommon:= none;
  OSectionC_Code:= none;
  ObjectSection := none;
  comment_was_just_generated:= FALSE;
  comment_was_just_generated_in_same_line:= FALSE;
  last_string_length:= 0;
  fio.DivideChars:= stdDivideChars;
  SetBaseTypes();
  ExtractNamedTypes( header );
END InitGenerater;

(*------------------------------------------------------------------------*)
PROCEDURE EraseTrace();
(* clean field generated for tag defined in other header *)
VAR elm: adt.Element;
BEGIN
  TagDefinedInOtherHeader.FindFirst(elm);
  WHILE elm # NIL DO
    elm(obj.Type).generated:= FALSE;
    TagDefinedInOtherHeader.FindNext(elm);
  END;
  adt.Deallocate(TagDefinedInOtherHeader);
END EraseTrace;

(*------------------------------------------------------------------------*)
PROCEDURE StartGenerate ( header: obj.Header );
BEGIN
  InitGenerater( header );
  IF msg.WasError THEN RETURN END;
  GenModuleTitle();
  IF msg.WasError THEN RETURN END;
  GenImport( Import );
  IF msg.WasError THEN RETURN END;
  GenObjects(header.objects );
  IF msg.WasError THEN RETURN END;
  GenModuleEnd();
  IF msg.WasError THEN RETURN END;
  header.generated:= TRUE;
  IF ~msg.WasError & cfg.GenerateMacrosFile & ~header.Macros.IsEmpty() THEN
    GenMacroFiles( header );
  END;
  EraseTrace();
END StartGenerate;

(*------------------------------------------------------------------------*)
PROCEDURE Generate * ( header: obj.Header );
VAR done: BOOLEAN;
BEGIN
  StartGenerate( header );
  IF  msg.WasError  THEN
    IF (StdDefFile # NIL) & StdDefFile.created  THEN
      OutFile:= StdDefFile;
      CloseFile();
      fio.Delete( StdDefFile.name^ , done );
    END;
    IF cfg.GenerateMacrosFile THEN
      IF (MacroDefFile # NIL) & MacroDefFile.created  THEN
	OutFile:= MacroDefFile;
        CloseFile();
	fio.Delete( MacroDefFile.name^ , done );
      END;
      IF (MacroModFile # NIL) & MacroModFile.created  THEN
	OutFile:= MacroModFile;
        CloseFile();
	fio.Delete( MacroModFile.name^ , done );
      END;
    END;
  END;
  CloseFile();
  CloseHFile();
  RemoveGeneratedTypeTree(GeneratedTypes);
  RemoveBaseTypesStorage();
  RemoveNamedElementsList(AdtImport);
  RemoveNamedElementsList(Import);
  RemoveFILE(OutFile);
  StdDefFile:= NIL;
  MacroDefFile:= NIL;
  MacroModFile:= NIL;
  adt.Deallocate(WriteBuffer);
END Generate;


(*------------------------------------------------------------------------*)
(*                      EXPORTED PROCEDURE                                *)
(*------------------------------------------------------------------------*)
PROCEDURE ValueHandler ( tobj: obj.TypedObject; VAR str: lstr.String );
BEGIN
  IF ( OutFile # NIL ) & ( ValueHandlerOn ) &
     ( tobj.obj = obj.constant )
--     ( (tobj.obj = obj.constant) OR (tobj.obj = obj.bitset_const) )
  THEN
    GenConstant( tobj );
  END;
  IF OutFile = NIL THEN
    lstr.Assign( tobj.name^, str);
  ELSE
    lstr.Assign('', str);  --MakeQualident делает все через Append
    MakeQualident(tobj, str);
  END;
END ValueHandler;


(*------------------------------------------------------------------------*)
(*                        MEMORY MENEGMENT                                *)
(*------------------------------------------------------------------------*)
PROCEDURE RemoveGeneratedTypeTree( VAR tree: adt.Tree );
VAR e: adt.Element;
BEGIN
  IF  tree # NIL THEN
    tree.FindFirst(e);
    WHILE e # NIL DO
      Deallocate(e);
      tree.FindNext(e);
    END;
    adt.Deallocate(tree); tree:= NIL;
  END;
END RemoveGeneratedTypeTree;

(*------------------------------------------------------------------------*)
PROCEDURE RemoveNamedElementsList(VAR l: adt.List);
VAR e: adt.Element;
BEGIN
  IF l # NIL THEN
    l.FindFirst(e);
    WHILE e # NIL DO
      lstr.Deallocate(e(adt.NamedElement).name);
      l.FindNext(e);
    END;
    adt.Deallocate(l); l:= NIL;
  END;
END RemoveNamedElementsList;


(*------------------------------------------------------------------------*)
PROCEDURE RemoveFILE(VAR f: FILE);
VAR e: adt.Element;
BEGIN
  IF f # NIL THEN
    lstr.Deallocate(f.path);
    lstr.Deallocate(f.name);
    lstr.Deallocate(f.modname);
    IF f.creatednames # NIL THEN
      f.creatednames.FindFirst(e);
      WHILE e # NIL DO
        WITH
           e: obj.Type DO
             obj.Deallocate(e);
          |e: adt.NamedElement DO
             adt.Deallocate(e);
        END;
        f.creatednames.FindNext(e);
      END;
      adt.Deallocate(f.creatednames);
    END;
    f:= NIL;
  END;
END RemoveFILE;

(*------------------------------------------------------------------------*)
PROCEDURE RemoveOutStringsBuffer(VAR b: adt.List);
VAR e: adt.Element;
BEGIN
  b.FindFirst(e);
  WHILE e # NIL DO
    WITH
      e: OutString DO
        Deallocate(e);
      |e: obj.Comment DO
    END;
    b.FindNext(e);
  END;
  adt.Deallocate(b); b:= NIL;
END RemoveOutStringsBuffer;



BEGIN
  null:= NIL;
  WriteStrLn:= WriteStrLn2File;
  WriteStr  := WriteStr2File;
  adt.NewList(ListForExtractNamedTypes);
  obj.ValueHandler:= ValueHandler;
  adt.NewList(BaseTypesStorage);
  adt.NewList(GeneratedTypeStorage);
  adt.NewList(OutStringStorage);
  adt.NewList(PrintStringStorage);
  BlankString:= NIL;
  lstr.Assign( '', emptyString );
  HFile:= NIL;
  OutFile:= NIL;
  ValueHandlerOn:= FALSE;
  IF cfg.MaxStrLen >= 0 THEN
    fio.SizeOutputLine:= cfg.MaxStrLen;
  END;
  IF cfg.CommentPosition < 0 THEN
    comment_position:= default_comment_position;
  ELSE
    comment_position:= cfg.CommentPosition;
  END;
  obj.NewHeader( 'SYSTEM', ModuleSystem );
  SetStdDivideChars();
  SetNothingdDivideChars();
  SetDivideCharsForProcedure();
  SetModula2KeyWords();
  obj.NewComment( declaration_without_definition, new_line_and_position,
                   'Declaration without definition' );
  obj.NewComment( simple_comment, new_line_and_position, '' );

END H2DGen.
