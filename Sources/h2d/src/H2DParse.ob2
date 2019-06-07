MODULE H2DParse;

IMPORT io:= Printf,
       sys:= SYSTEM,
       WholeStr,
       ConvTypes,
       RegComp,
       msg := H2DMsg,
       cfg := H2DCfg,
       file:= H2DFile,
       objs:= H2DObjs,
       scan:= H2DScan,
       lstr:= LongStrs,
       Strings,
       adt;

CONST
  min_binary_operation_priority = 9;

TYPE
  INT = sys.INT32;

TYPE
  HeaderStackElement = POINTER TO HeaderStackElementDesc;
  HeaderStackElementDesc = RECORD (adt.ElementDesc)
    header	   : objs.Header;
    token_list	   : adt.List;
    current_token  : scan.Token;
    scan_token	   : INT;
    scan_text	   : lstr.String;
    tag_was_parsed : BOOLEAN;
    if_balance	   : INT;
    if_stack	   : adt.Stack;
    end_balance    : INT;
    limited_constant_expression_on: BOOLEAN;
    include_balance: INT;
    transparent_numeration: BOOLEAN;
    C_extensions   : BOOLEAN;
  END;


  RegExpr = POINTER TO RegExprDesc;
  RegExprDesc = RECORD (adt.ElementDesc)
    expr      : RegComp.Expr;
  END;
  PreText = POINTER TO PreTextDesc;
  PreTextDesc = RECORD (adt.ElementDesc)
    patterns, exceptions: adt.List;
    pretokens : adt.List;
    posttokens: adt.List;
  END;

  Designator = POINTER TO DesignatorDesc;
  DesignatorDesc = RECORD (adt.NamedElementDesc)
    designator: INT;
    list: adt.List;
    num: INT;
    var, array: BOOLEAN;
    type_name: lstr.String;
    line, pos: INT;
    file_name: lstr.String;
  END;

VAR
  is_it_preheader: BOOLEAN;
  prepretext_list: adt.List;
  headers_stack  : adt.Stack;
  token_list	 : adt.List;
  empty_token_list: adt.List;
  current_token  : scan.Token;
  scan_token	 : INT;
  scan_text	 : lstr.String;
  if_balance	 : INT;
  if_stack	 : adt.Stack;
  end_balance: INT;
  declaration_without_definition: objs.Comment;
  tag_was_parsed : BOOLEAN;
  limited_constant_expression_on: BOOLEAN;

  binary_operations_priority: ARRAY min_binary_operation_priority + 1 OF
			      ARRAY 4 OF INT;
  was_sizeof	 : BOOLEAN;
  GlobalNamesTree, GlobalTagsTree, GlobalDefinedTree: adt.Tree;
  header_balance: INT;
  include_balance: INT;
  line, pos: INT;
  file_name: lstr.String;
  redefined_macro: BOOLEAN;
  current_list_for_comments: adt.List;
  merge_headers_tree: adt.Tree; (* Tree for header names defined by #merge in header file *)
  project_merge_headers_tree: adt.Tree; (* Tree for header names defined by #merge in project file *)
  pretext_list: adt.List; (* The list for header names defined by #module *)
  comments_lists_stack: adt.Stack;
  merged_headers_stack: adt.Stack;
  C_extensions: BOOLEAN;
  config_on: BOOLEAN;
  project_on: BOOLEAN;
  config_project: objs.Header;
  emptyString: lstr.String;
  emptyToken: scan.Token;

  FileNameTree: adt.Tree;

  HeadHeader: objs.Header; (* This is head header for the current parsed header
			      (defines external context) *)



(*------------------------------------------------------*)
PROCEDURE ^ Parse * (name-: ARRAY OF CHAR; cstdlib: BOOLEAN);
(*------------------------------------------------------*)
PROCEDURE ^ Descriptor( VAR tobj: objs.TypedObject; VAR type: objs.Type);
(*------------------------------------------------------*)
PROCEDURE ^ Define();
(*------------------------------------------------------*)
PROCEDURE ^ Undef();
(*------------------------------------------------------*)
PROCEDURE ^ Declarations(struct_or_union: BOOLEAN; to: adt.List);
(*------------------------------------------------------*)
PROCEDURE ^ ConstExpr(VAR expr: objs.Expression);
(*------------------------------------------------------*)
PROCEDURE ^ RemoveExpression(VAR expr: objs.Expression);
(*------------------------------------------------------*)
PROCEDURE ^ GetToken( parsed: BOOLEAN );
(*------------------------------------------------------*)


(*------------------------------------------------------*)
PROCEDURE SetFileName( VAR str: lstr.String; name: lstr.String );
VAR e: adt.Element;
    ne: adt.NamedElement;
BEGIN
  IF name = NIL THEN
    lstr.Assign("JOPPA", name);
  END;
  adt.NewNamedElement(ne, name^);
  FileNameTree.Find(ne, e);
  IF e = NIL THEN
    FileNameTree.Insert(ne);
  ELSE
    adt.Deallocate(ne);
    ne:= e(adt.NamedElement);
  END;
  str:= ne.name;
  IF name^ = "JOPPA" THEN lstr.Deallocate(name) END;
END SetFileName;

(*------------------------------------------------------*)
PROCEDURE Error(number: INT; SEQ x: sys.BYTE);
BEGIN
  objs.CurrentHeader.stat.Error(number, file_name^, line, pos+1, x);
  scan.Close();
END Error;
(*
PROCEDURE Warning(number: INT; SEQ x: sys.BYTE);
BEGIN
  objs.CurrentHeader.stat.Warning(number, file_name^, line, pos+1, x);
END Warning;
*)

PROCEDURE Open(name-: ARRAY OF CHAR; gen_error: BOOLEAN): BOOLEAN;
BEGIN
  IF scan.Open(name) THEN
    RETURN TRUE;
  ELSIF gen_error THEN
    (* error *)
    Error(6, name);
  END;
  RETURN FALSE;
END Open;

(*------------------------------------------------------*)
PROCEDURE get_token(VAR t: scan.Token);
VAR e: adt.Element;
BEGIN
  IF t = NIL THEN
    empty_token_list.FindFirst(e);
    IF e # NIL THEN
      t:= e(scan.Token);
      empty_token_list.DeleteCurrent();
    END;
  END;
  scan.NewToken(t);
END get_token;


(*------------------------------------------------------*)
PROCEDURE DesignatorPresentError(d: Designator);
BEGIN
  line:= d.line;
  pos:= d.pos;
  lstr.Assign(d.file_name^, file_name);
  Error(56, d.name^);
END DesignatorPresentError;

(*------------------------------------------------------*)
PROCEDURE DesignatorError(d: Designator; errcode:=52: INT);
BEGIN
  line:= d.line;
  pos:= d.pos;
  lstr.Assign(d.file_name^, file_name);
  Error(errcode, '');
END DesignatorError;

(*-----------------------------------------*)
PROCEDURE SkipSynonym(VAR type: objs.Type);
BEGIN
  WHILE (type.base # NIL) & (type.base.type = objs.t_synonym) DO
    type:= type.base;
  END;
END SkipSynonym;

(*------------------------------------------------------*)
PROCEDURE GetObject(d: Designator; VAR type: objs.Type; VAR tobj: objs.TypedObject; err_flag:=TRUE: BOOLEAN);
VAR
  e1, e2: adt.Element;
  i: INT;
BEGIN
  objs.CurrentHeader.objects.Find(d, e1);
  tobj:= NIL; type:= NIL;
  IF e1 = NIL THEN
    IF err_flag THEN
      (* error *)
      DesignatorPresentError( d );
    END;
    RETURN;
  ELSIF e1 IS objs.TypedObject THEN
    tobj:= e1(objs.TypedObject);
    IF (tobj.type # NIL) THEN
      type:= tobj.type; SkipSynonym(type);
      IF (tobj.type.type = objs.t_func) THEN
	type:= tobj.type;
      ELSE
	type:= NIL;
      END;
    END;
  ELSIF e1 IS objs.Type THEN
    type:= e1(objs.Type);
  END;
  IF (type # NIL) OR (tobj # NIL) THEN
    d.list.FindFirst(e1);
    IF (tobj # NIL) & (tobj.type = NIL) THEN
      IF (tobj.obj # objs.constant) OR (e1 # NIL) THEN
	(* error *)
	DesignatorError(e1(Designator));
      END;
      RETURN;
    END;
    WHILE e1 # NIL DO
      CASE e1(Designator).designator OF
	 objs.reference:
	   IF (tobj # NIL) & (tobj.type.type = objs.t_ptr) THEN
	     type:= tobj.type;
	     tobj:= NIL;
	   ELSIF (type # NIL) THEN
	     SkipSynonym(type);
	     IF (type.base # NIL) & (type.base.type = objs.t_ptr) THEN
	       type:= type.base;
	     ELSE
	       (* error *)
	       DesignatorError(e1(Designator));
	       RETURN;
	     END;
	   ELSE
	     (* error *)
	     DesignatorError(e1(Designator));
	     RETURN;
	   END;
	   IF (type.base # NIL) & (type.base.type = objs.t_func) THEN
	     type:= type.base;
	   END;
	|objs.point:
	   IF (tobj # NIL) & (tobj.type.type = objs.t_struct)
			   &
	      (lstr.Length1(tobj.type.name) = 0)
	   THEN
	     type:= tobj.type;
	   ELSIF (type # NIL) THEN
	     SkipSynonym(type);
	     IF (type.base # NIL) & (type.base.type = objs.t_struct) THEN
	       type:= type.base;
	     ELSIF (type.type # objs.t_struct) OR  (lstr.Length1(type.name) <= 0) THEN
	       (* error *)
	       DesignatorError(e1(Designator));
	       RETURN;
	     END;
	   ELSE
	     (* error *)
	     DesignatorError(e1(Designator));
	     RETURN;
	   END;
	   d.list.FindNext(e1);
	   type.vars.Find(e1, e2);
	   WHILE (e2 # NIL) & ~(e2 IS objs.TypedObject) DO
	     type.vars.FindAgain(e2);
	   END;
	   IF e2 # NIL THEN
	     tobj:= e2(objs.TypedObject);
	     type:= NIL;
	   ELSE
	     (* error *)
	     DesignatorError(e1(Designator));
	     RETURN;
	   END;
	|objs.open_round_brack:
	   IF (type # NIL) & (type.type = objs.t_func) THEN
	     type.vars.FindFirst(e2);
	     i:= 0;
	     LOOP
	       IF e2 = NIL THEN
		 (* error *)
		 DesignatorError(e1(Designator));
		 RETURN;
	       ELSIF e2 IS objs.TypedObject THEN
		 IF i = e1(Designator).num THEN
		   EXIT;
		 ELSE
		   INC(i);
		 END;
	       END;
	       type.vars.FindNext(e2);
	     END;
	     tobj:= e2(objs.TypedObject);
	     type:= NIL;
	   ELSE
	     (* error *)
	     DesignatorError(e1(Designator));
	     RETURN;
	   END;
	|objs.open_square_brack:
	   IF (tobj # NIL) & (tobj.type.type = objs.t_array) THEN
	     type:= tobj.type;
	     tobj:= NIL;
	   ELSIF (type # NIL) THEN
	     SkipSynonym(type);
	     IF type.base.type = objs.t_array THEN
	       type:= type.base;
	     ELSE
	       (* error *)
	       DesignatorError(e1(Designator));
	       RETURN;
	     END;
	   ELSE
	     (* error *)
	     DesignatorError(e1(Designator));
	     RETURN;
	   END;
      END;
      d.list.FindNext(e1);
    END;
  ELSE
    (* error *)
    DesignatorError(d);
    RETURN;
  END;
END GetObject;

(*------------------------------------------------------*)
PROCEDURE CheckParameters();
VAR
  tobj: objs.TypedObject;
  type: objs.Type;
  e: adt.Element;
BEGIN
  objs.CurrentHeader.Parameters.FindFirst(e);
  WHILE e # NIL DO
    GetObject(e(Designator), type, tobj); IF msg.WasError THEN RETURN END;
    IF tobj.type.type = objs.t_ptr THEN
      tobj.var:= e(Designator).var;
      IF e(Designator).var & ~e(Designator).array THEN
	tobj.type:= tobj.type.base;
      END;
      IF e(Designator).array THEN
	tobj.type.type:= objs.t_array;
      END;
    ELSE
      DesignatorError(e(Designator));
      RETURN;
    END;
    objs.CurrentHeader.Parameters.FindNext(e);
  END;
END CheckParameters;

PROCEDURE IsBaseType(type: objs.Type; VAR type_number: INT): BOOLEAN;
BEGIN
  WHILE type.type = objs.t_synonym DO type:= type.base END;
  type_number:= type.type - objs.BaseTypes_base;
  RETURN type.type IN objs.base_types;
END IsBaseType;

PROCEDURE CheckModifiedObjects();
VAR
  tobj	 : objs.TypedObject;
  type	 : objs.Type;
  res	 : objs.ConstantValue;
  e	 : adt.Element;
  ne	 : adt.NamedElement;
  d	 : Designator;
  i	 : INT;
  m2Type : objs.M2Type;

  (*----------------------------------------------------*)
  PROCEDURE CheckModifiedType();
  VAR i: INT;
      e: adt.Element;
  BEGIN
    IF d.name^ = "ctype_signed_char"           THEN
      i:= 0;
    ELSIF d.name^ = "ctype_signed_int"         THEN
      i:= 1;
    ELSIF d.name^ = "ctype_signed_short_int"   THEN
      i:= 2;
    ELSIF d.name^ = "ctype_signed_long_int"    THEN
      i:= 3;
    ELSIF d.name^ = "ctype_unsigned_char"      THEN
      i:= 4;
    ELSIF d.name^ = "ctype_unsigned_int"       THEN
      i:= 5;
    ELSIF d.name^ = "ctype_unsigned_short_int" THEN
      i:= 6;
    ELSIF d.name^ = "ctype_unsigned_long_int"  THEN
      i:= 7;
    ELSIF d.name^ = "ctype_float"              THEN
      i:= 8;
    ELSIF d.name^ = "ctype_double"             THEN
      i:= 9;
    ELSIF d.name^ = "ctype_long_float"         THEN
      i:= 10;
    ELSIF d.name^ = "ctype_long_double"        THEN
      i:= 11;
    ELSE
      adt.NewNamedElement(ne, d.name^);
      objs.CurrentHeader.Names.Find(ne, e);
      IF (e # NIL) & (e IS objs.Type) &
	IsBaseType(e(objs.Type), i) &
	cfg.IsValidVariant(i, m2Type.type_size)
      THEN
	e(objs.Type).translation_variant:= m2Type;
      ELSE
	(* error *)
	DesignatorError(d);
      END;
      RETURN;
    END;
    cfg.SetBaseType(i + objs.BaseTypes_base, d.type_name^);
  END CheckModifiedType;

  (*----------------------------------------------------*)
BEGIN
  objs.CurrentHeader.ModifiedObjects.FindFirst(e);
  WHILE e # NIL DO
    d:= e(Designator);
    adt.NewNamedElement(ne, d.type_name^);
    objs.M2Types.Find(ne, e);
    IF e # NIL THEN
      m2Type:= e(objs.M2Type);
    ELSE
      (* error *)
      DesignatorError(d);
      RETURN;
    END;
    GetObject(d, type, tobj, FALSE); IF msg.WasError THEN RETURN END;
    IF tobj # NIL THEN
      IF tobj.obj = objs.variable THEN
	type:= tobj.type; SkipSynonym(type);
	IF type.type = objs.t_func THEN
	  type:= tobj.type.base;
	ELSE
	  type:= tobj.type;
	END;
	IF ~(IsBaseType(type, i) & cfg.IsValidVariant(i, m2Type.type_size)) THEN
	  DesignatorError(d);
	  RETURN;
	END;
      END;
      tobj.type_name:= m2Type;
    ELSIF d.list.IsEmpty() THEN
      CheckModifiedType();
    ELSE
      (* error *)
      DesignatorError(d);
      RETURN;
    END;
    objs.CurrentHeader.ModifiedObjects.FindNext(e);
  END;
END CheckModifiedObjects;

(*------------------------------------------------------*)
PROCEDURE NewDesignator(VAR designator: Designator; name-: ARRAY OF CHAR; des: INT);
BEGIN
  NEW(designator); designator.SetName(name);
  adt.NewList(designator.list);
  designator.designator:= des;
  designator.line:= line;
  designator.pos := pos;
  designator.var:= FALSE;
  designator.array:= FALSE;
  lstr.Assign(file_name^, designator.file_name);
END NewDesignator;

(*------------------------------------------------------*)
PROCEDURE IsEmptyHeader(h: objs.Header): BOOLEAN;
VAR
  e: adt.Element;
BEGIN
  h.objects.FindFirst(e);
  WHILE (e # NIL) & (e IS objs.Comment) DO
    h.objects.FindNext(e);
  END;
  RETURN e = NIL;
END IsEmptyHeader;

(*------------------------------------------------------*)
PROCEDURE PushCommentsList(l: adt.List);
BEGIN
  comments_lists_stack.Push(current_list_for_comments);
  current_list_for_comments:= l;
END PushCommentsList;

(*------------------------------------------------------*)
PROCEDURE PopCommentsList();
VAR
  e: adt.Element;
BEGIN
  comments_lists_stack.Pop(e);
  IF e # NIL THEN
    current_list_for_comments:= e(adt.List);
  ELSE
    current_list_for_comments:= NIL;
  END;
END PopCommentsList;

(*------------------------------------------------------
PROCEDURE CheckTagsDefinition();
VAR
 e, finded: adt.Element;
BEGIN
  objs.CurrentHeader.Tags.FindFirst(e);
  WHILE e # NIL DO
    WITH e: objs.Type DO
      IF e.not_described THEN
	(* special  comment *)
	e.vars.FindLast( finded );
	IF finded = NIL THEN
	  e.vars.Insert( declaration_without_definition );
	END;
      END;
    END;
    objs.CurrentHeader.Tags.FindNext(e);
  END;
END CheckTagsDefinition;
*)

(*------------------------------------------------------*)
PROCEDURE EquReplacements(r1, r2: adt.Element): BOOLEAN;
VAR
  e1, e2: adt.Element;
  PROCEDURE check_params(r1, r2: adt.Element): BOOLEAN;
  VAR
    p1, p2: adt.Element;
  BEGIN
    WITH r1: objs.Replacement DO
      WITH r2: objs.Replacement DO
	r1.params.FindFirst(p1);
	r2.params.FindFirst(p2);
	WHILE (p1 # NIL) & (p2 # NIL) DO
	  IF (p1(adt.NamedElement).name^ = e1(scan.Token).text^)
					 &
	     (p2(adt.NamedElement).name^ = e2(scan.Token).text^)
	  THEN
	    RETURN TRUE;
	  ELSE
	    r1.params.FindNext(p1);
	    r2.params.FindNext(p2);
	  END;
	END;
	RETURN FALSE;
      END;
    END;
  END check_params;
BEGIN
  WITH r1: objs.Replacement DO
    WITH r2: objs.Replacement DO
      IF r1.params.IsEmpty() & r2.params.IsEmpty() THEN
	r1.tokens.FindFirst(e1);
	r2.tokens.FindFirst(e2);
	WHILE (e1 # NIL) & (e2 # NIL) DO
	  IF (e1(scan.Token).token = e2(scan.Token).token)
				   &
	     (e1(scan.Token).text^ = e2(scan.Token).text^)
	  THEN
	    r1.tokens.FindNext(e1);
	    r2.tokens.FindNext(e2);
	  ELSE
	    RETURN FALSE;
	  END;
	END;
	RETURN e1 = e2;
      ELSIF ~r1.params.IsEmpty() & ~r2.params.IsEmpty() THEN
	r1.pattern.FindFirst(e1);
	r2.pattern.FindFirst(e2);
	WHILE (e1 # NIL) & (e2 # NIL) DO
	  IF ((e1(scan.Token).token = e2(scan.Token).token)
				    &
	      (e1(scan.Token).text^ = e2(scan.Token).text^))
				    OR
	      (e1(scan.Token).token = objs.ident)
				    &
	      (e2(scan.Token).token = objs.ident)
				    &
			   check_params(r1, r2)
	  THEN
	    r1.tokens.FindNext(e1);
	    r2.tokens.FindNext(e2);
	  ELSE
	    RETURN FALSE;
	  END;
	END;
	RETURN e1 = e2;
      ELSE
	RETURN FALSE;
      END;
    END;
  END;
END EquReplacements;

(*------------------------------------------------------*)
PROCEDURE EquTypedObjects(tobj1, tobj2: adt.Element): BOOLEAN;
VAR
  res1, res2: objs.ConstantValue;
  equ: BOOLEAN;
BEGIN
  equ:= FALSE;
  WITH tobj1: objs.TypedObject DO
    WITH tobj2: objs.TypedObject DO
      tobj1.expr.ComputeExpression(res1, FALSE);
      tobj2.expr.ComputeExpression(res2, FALSE);
      IF res1.type = res2.type THEN
	CASE res1.type OF
	   objs.int_const:
	     equ:= res1.int = res2.int;
	   |objs.uint_const:
	     equ:= res1.uint = res2.uint;
	  |objs.real_const:
	     equ:= res1.real = res2.real;
	  |objs.str_const:
	     equ:= res1.str^ = res2.str^;
	END;
      END;
      objs.Deallocate(res1); objs.Deallocate(res2);
    END;
  END;
  RETURN equ;
END EquTypedObjects;

(*------------------------------------------------------*)
PROCEDURE InsertToNames(what: adt.NamedElement; check_extern_objects_identity:= FALSE: BOOLEAN);
VAR
  e: adt.Element;
  str1, str2: lstr.String;
BEGIN
  IF redefined_macro THEN
    objs.CurrentHeader.Names.Insert(what);
  ELSE
--    GlobalNamesTree.Find(what, e);
    objs.CurrentHeader.Names.Find(what, e);
    IF e = NIL THEN objs.FindInNames(HeadHeader, what, e) END;

    IF (e = NIL) THEN
      objs.CurrentHeader.Names.Insert(what);
--	GlobalNamesTree.Insert(what);
    ELSIF check_extern_objects_identity & (what IS objs.TypedObject) & (e IS objs.TypedObject) THEN
      objs.TranslateTypeToString(what(objs.TypedObject).type, str1);
      objs.TranslateTypeToString(e(objs.TypedObject).type, str2);
      IF (str1^ = str2^) &
	 ( what(objs.TypedObject).mem_class = objs.extern )
			 &
	 ( e(objs.TypedObject).mem_class = objs.extern )
      THEN
	objs.CurrentHeader.Names.Insert(what);
      ELSE
	(* error *)
	Error(19, what.name^, e(objs.Object).file^, e(objs.Object).line);
	RETURN;
      END;
    ELSE
      (* error *)
      Error(19, what.name^, e(objs.Object).file^, e(objs.Object).line);
      RETURN;
    END;
  END;
END InsertToNames;

(*------------------------------------------------------*)
PROCEDURE InsertToTags(what: adt.NamedElement);
VAR
  e: adt.Element;
BEGIN
  objs.CurrentHeader.Tags.Find(what, e);
  IF e = NIL THEN objs.FindInTags(HeadHeader, what, e) END;
--  GlobalTagsTree.Find(what, e);
  IF (e = NIL) THEN
    objs.CurrentHeader.Tags.Insert(what);
--    GlobalTagsTree.Insert(what);
  ELSE
    (* error *)
    Error(19, what.name^, e(objs.Object).file^, e(objs.Object).line);
    RETURN;
  END;
END InsertToTags;

(*------------------------------------------------------*)
PROCEDURE InsertToDefined(what: adt.NamedElement);
VAR
  e: adt.Element;
BEGIN
  objs.CurrentHeader.defined.Find(what, e);
  IF e = NIL THEN objs.FindInDefined(HeadHeader, what, e) END;
--  GlobalDefinedTree.Find(what, e);
  objs.CurrentHeader.defined.Insert(what);
  IF e = NIL THEN
--    GlobalDefinedTree.Insert(what);
    redefined_macro:= FALSE;
    RETURN;
  ELSIF ~(((what IS objs.Replacement) & (e IS objs.Replacement) & EquReplacements(e, what))
						   OR
	  ((what IS objs.TypedObject) & (e IS objs.TypedObject) & EquTypedObjects(e, what)))
  THEN
    (* error *)
    Error(19, what.name^, e(objs.Object).file^, e(objs.Object).line);
  END;
  redefined_macro:= TRUE;
END InsertToDefined;

(*------------------------------------------------------*)
PROCEDURE InsertToList(to: adt.List; what: adt.NamedElement);
VAR
  e: adt.Element;
BEGIN
  to.Find(what, e);
  IF (e = NIL) OR (lstr.Length1(what.name) = 0) THEN
    to.Insert(what);
  ELSE
    (* error *)
    Error(19, what.name^, e(objs.Object).file^, e(objs.Object).line);
    RETURN;
  END;
END InsertToList;

(*------------------------------------------------------*)
PROCEDURE IsType(type: objs.Type; mode: INT): BOOLEAN;
BEGIN
  WHILE type # NIL DO
    IF type.type = mode THEN
      RETURN TRUE;
    ELSIF type.type = objs.t_synonym THEN
      type:= type.base;
    ELSE
      type:= NIL;
    END;
  END;
  RETURN FALSE;
END IsType;

(*------------------------------------------------------*)
PROCEDURE InsertSynonyms();
VAR
  e, found: adt.Element;
  line: INT;
  t_syn: objs.Type;

  (*-------------------------*)
  PROCEDURE ins_comments();
  VAR
    e1, e2: adt.Element;
  BEGIN
    e(objs.TypedObject).modifier.FindFirst(e1);
    WHILE e1 # NIL DO
      objs.CurrentHeader.objects.FindNext(e2);
      objs.CurrentHeader.objects.InsertAfterCurrent(e1);
      e(objs.TypedObject).modifier.FindNext(e1);
    END;
  END ins_comments;

  (*-------------------------*)
  PROCEDURE skip_comment_in_same_line();
  VAR
    e1: adt.Element;
  BEGIN
    REPEAT
      objs.CurrentHeader.objects.FindNext(e1);
    UNTIL (e1 = NIL) OR ~(e1 IS objs.Comment) OR (e1(objs.Object).line # line);
    IF e1 = NIL THEN
      objs.CurrentHeader.objects.FindLast(e1);
    ELSE
      objs.CurrentHeader.objects.FindPrev(e1);
    END;
  END skip_comment_in_same_line;

  (*-------------------------*)
BEGIN
  objs.CurrentHeader.Synonyms.FindFirst(e);
  WHILE e # NIL DO
    objs.CurrentHeader.objects.Find(e(objs.TypedObject).type, found);
    WHILE (found # NIL) & (found IS objs.Comment) DO
      objs.CurrentHeader.objects.FindAgain(found);
    END;
    e(objs.TypedObject).back_end:= objs.no_change;
    IF found # NIL THEN
      line:= found(objs.Object).line;
      WITH
	 found: objs.TypedObject DO
	   IF (found.obj = objs.constant)
			 OR
	      IsType(found.type, objs.t_func)
	   THEN
	     skip_comment_in_same_line();
	     objs.CurrentHeader.objects.InsertAfterCurrent(e);
	     ins_comments();
	   END;
	|found: objs.Type DO
	   IF (found.type # objs.t_synonym) OR ~IsType(found.base, objs.t_func) THEN
	     objs.NewType(t_syn);
	     t_syn.type:= objs.t_synonym;
	     t_syn.SetName(e(adt.NamedElement).name^);
	     t_syn.line:= e(objs.Object).line; SetFileName( t_syn.file, e(objs.Object).file );
	     t_syn.base:= found;
	     skip_comment_in_same_line();
	     InsertToNames(t_syn);
	     IF msg.WasError THEN RETURN END;
	     objs.CurrentHeader.objects.InsertAfterCurrent(t_syn);
	     ins_comments();
	   END;
	|found: objs.Macro DO
	   e(objs.TypedObject).back_end:= objs.c_code;
	   objs.CurrentHeader.objects.InsertAfterCurrent(e);
	   ins_comments();
      ELSE
      END;
    ELSE
--	objs.CurrentHeader.Names.Find(e(objs.TypedObject).type, found);
      objs.FindInNames(objs.CurrentHeader, e(objs.TypedObject).type, found);
      IF (found # NIL)
		&
	 (found IS objs.TypedObject)
		&
	 (found(objs.TypedObject).obj = objs.enum_const)
      THEN
	line:= found(objs.Object).line;
	IF lstr.Length1(found(objs.TypedObject).type.name) > 0 THEN
	  objs.CurrentHeader.objects.Find(found(objs.TypedObject).type, found);
	  WHILE (found # NIL) & (found IS objs.Comment) DO
	    objs.CurrentHeader.objects.FindAgain(found);
	  END;
	ELSE
	  objs.CurrentHeader.objects.FindLast(found);
	END;
	skip_comment_in_same_line();
	objs.CurrentHeader.objects.InsertAfterCurrent(e);
	ins_comments();
      END;
    END;
    objs.CurrentHeader.Synonyms.FindNext(e);
  END;

END InsertSynonyms;

(*------------------------------------------------------*)
PROCEDURE merge_trees( to, from: adt.Tree );
VAR
  e: adt.Element;
BEGIN
  from.FindFirst(e);
  WHILE e # NIL DO
    to.Insert(e);
    from.FindNext(e);
  END;
END merge_trees;

(*------------------------------------------------------*)
PROCEDURE merge_lists( to, from: adt.List; unique: BOOLEAN );
VAR
  e, e1: adt.Element;
BEGIN
  from.FindFirst(e);
  WHILE e # NIL DO
    e1:= NIL;
    IF unique THEN to.Find(e, e1) END;
    IF e1 = NIL THEN
      to.Insert(e);
    END;
    from.FindNext(e);
  END;
END merge_lists;

(*------------------------------------------------------*)
PROCEDURE merge_include_lists(to, from: adt.List);
VAR
  e, e1: adt.Element;
BEGIN
  from.FindFirst(e);
  WHILE e # NIL DO
    to.Find(e, e1);
    IF e1 = NIL THEN
      to.Insert(e);
    END;
    from.FindNext(e);
  END;
END merge_include_lists;

(*------------------------------------------------------*)
PROCEDURE merge_headers(to, from: objs.Header; insert_to_includes, merge_trees_flag: BOOLEAN);
VAR e: adt.Element;
BEGIN
  IF merge_trees_flag THEN
    merge_trees(to.Replacements, from.Replacements);
    merge_trees(to.Tags,	 from.Tags	  );
    merge_trees(to.Names,	 from.Names	  );
    merge_trees(to.defined,	 from.defined	  );
  END;
  to.includes.Find(from, e);
  IF insert_to_includes & (e = NIL) THEN
    to.includes.Insert(from);
  END;
  merge_include_lists(to.includes, from.includes);
END merge_headers;

(*------------------------------------------------------*)
(* Obsolete procedure
PROCEDURE ComputeEndBalance();
BEGIN
  IF (scan_token = objs.pd_header)
		 OR
     (scan_token = objs.pd_bitset)
		 OR
     (scan_token = objs.pd_parameters)
  THEN
    INC(end_balance);
  ELSIF scan_token = objs.pd_end THEN
    IF end_balance > 0 THEN
      DEC(end_balance);
    ELSE
      (* error *)
      Error(2, '#end');
    END;
  END;
END ComputeEndBalance;
*)

(*------------------------------------------------------*)
PROCEDURE CopyToken(VAR tok: scan.Token; pattr: scan.Token);
BEGIN
  tok:= NIL;
  get_token(tok);
  tok.token:= pattr.token;
  lstr.Assign(pattr.text^, tok.text);
  lstr.Assign(pattr.original_text^, tok.original_text);
  tok.parsed:= FALSE;
  tok.boundary:= FALSE;
  tok.line:= pattr.line;
  tok.pos:= pattr.pos;
  lstr.Assign(pattr.file^, tok.file);
  tok.pretoken:= FALSE;
END CopyToken;

(*------------------------------------------------------*)
PROCEDURE PutPreText(name-: ARRAY OF CHAR);
VAR
  e, e1: adt.Element;
  pretext: PreText;
  t: scan.Token;
BEGIN
  adt.NewList(token_list); t:= NIL;
  pretext_list.FindFirst(e);
  WHILE (e # NIL) DO
    pretext:= e(PreText);
    pretext.patterns.FindFirst(e1);
    WHILE e1 # NIL DO
      IF RegComp.Match(e1(RegExpr).expr, name, 0) THEN
	pretext.exceptions.FindFirst(e1);
	WHILE e1 # NIL DO
	  IF RegComp.Match(e1(RegExpr).expr, name, 0) THEN
	    pretext:= NIL;
	    e1:= NIL;
	  ELSE
	    pretext.exceptions.FindNext(e1);
	  END;
	END;
	IF pretext # NIL THEN
	  pretext.pretokens.FindFirst(e);
	  WHILE e # NIL DO
	    WITH
	      e: scan.Token DO
		CopyToken(t, e);
	     |e: objs.Comment DO
		NEW(t);
		lstr.Assign(e.header.name^, t.file);
		lstr.Assign(e.name^, t.text);
		t.line:= e.line;
		t.token:= objs.comment;
		t.parsed:= FALSE;
	    END;
	    t.pretoken:= TRUE;
	    token_list.Insert(t);
	    pretext.pretokens.FindNext(e);
	  END;
	END;
	e1:= NIL;
      ELSE
	pretext.patterns.FindNext(e1);
      END;
    END;
    pretext_list.FindNext(e);
  END;
  token_list.Reset();
END PutPreText;

(*------------------------------------------------------*)
PROCEDURE PutPostText(name-: ARRAY OF CHAR): adt.Element;
VAR
  e, e1: adt.Element;
  ft: adt.Element;
  pretext: PreText;
  t: scan.Token;
  first_token: BOOLEAN;
BEGIN
  first_token:= TRUE; ft:= NIL; t:= NIL;
  pretext_list.FindFirst(e);
  WHILE (e # NIL) DO
    pretext:= e(PreText);
    pretext.patterns.FindFirst(e1);
    WHILE e1 # NIL DO
      IF RegComp.Match(e1(RegExpr).expr, name, 0) THEN
	pretext.exceptions.FindFirst(e1);
	WHILE e1 # NIL DO
	  IF RegComp.Match(e1(RegExpr).expr, name, 0) THEN
	    pretext:= NIL;
	    e1:= NIL;
	  ELSE
	    pretext.exceptions.FindNext(e1);
	  END;
	END;
	IF pretext # NIL THEN
	  pretext.posttokens.FindFirst(e);
	  WHILE e # NIL DO
	    WITH
	      e: scan.Token DO
		CopyToken(t, e);
	     |e: objs.Comment DO
		NEW(t);
		lstr.Assign(e.header.name^, t.file);
		lstr.Assign(e.name^, t.text);
		t.line:= e.line;
		t.token:= objs.comment;
		t.parsed:= FALSE;
	    END;
	    t.pretoken:= TRUE;
	    token_list.Insert(t);
	    IF first_token THEN
	      first_token:= FALSE;
	      token_list.FindLast(ft);
	    END;
	    pretext.posttokens.FindNext(e);
	  END;
	END;
	e1:= NIL;
      ELSE
	pretext.patterns.FindNext(e1);
      END;
    END;
    pretext_list.FindNext(e);
  END;
  RETURN ft;
END PutPostText;

(*------------------------------------------------------*)
PROCEDURE next_token();
VAR
  e: adt.Element;
  t: scan.Token;
  comment: objs.Comment;
BEGIN
  LOOP
    token_list.FindNext(e);
    IF (e = NIL) OR ~e(scan.Token).parsed & (e(scan.Token).token # objs.comment) THEN
      EXIT;
    ELSIF ~e(scan.Token).parsed THEN
      objs.NewComment(comment, e(scan.Token).line, e(scan.Token).text^);
      current_list_for_comments.Insert(comment);
      e(scan.Token).parsed:= TRUE;
    END;
  END;
  IF e = NIL THEN
    (* parsed tokens deleting *)
    token_list.FindFirst(e);
    WHILE (e # NIL) & e(scan.Token).parsed & ~e(scan.Token).boundary DO
      empty_token_list.Insert(e);
      token_list.DeleteCurrent();
      token_list.FindFirst(e);
    END;
    (* get new token *);
    scan.GetToken(); IF msg.WasError THEN RETURN END;
    get_token(t);
    WHILE scan.token = objs.comment DO
      objs.NewComment(comment, t.line, t.text^);
      current_list_for_comments.Insert(comment);
      scan.GetToken(); IF msg.WasError THEN RETURN END;
      get_token(t);
    END;
    e:= NIL;
    IF (t.token = objs.eof) & ~project_on THEN
      e:= PutPostText(objs.CurrentHeader.name^);
      LOOP
	IF (e = NIL) OR ~e(scan.Token).parsed & (e(scan.Token).token # objs.comment) THEN
	  EXIT;
	ELSIF ~e(scan.Token).parsed THEN
	  objs.NewComment(comment, e(scan.Token).line, e(scan.Token).text^);
	  current_list_for_comments.Insert(comment);
	  e(scan.Token).parsed:= TRUE;
	END;
	token_list.FindNext(e);
      END;
    END;
    token_list.Insert(t);
    IF e = NIL THEN
      token_list.FindLast(e);
    END;
  END;
  current_token:= e(scan.Token);
  scan_token:= current_token.token;
  lstr.Assign(current_token.text^, scan_text);
  line:= current_token.line;
  pos:= current_token.pos;
  objs.line:= current_token.line;
  objs.pos:= current_token.pos;
  lstr.Assign(current_token.file^, file_name);
  lstr.Assign(current_token.file^, objs.file_name);
END next_token;

(*------------------------------------------------------*)
PROCEDURE prev_token();
VAR
  e: adt.Element;
BEGIN
  REPEAT
    token_list.FindPrev(e);
  UNTIL (e = NIL) OR e(scan.Token).boundary OR ~e(scan.Token).parsed;
  IF (e = NIL) OR e(scan.Token).boundary THEN
    current_token:= emptyToken;
  ELSE
    current_token:= e(scan.Token);
    line:= current_token.line;
    pos:= current_token.pos;
    objs.line:= current_token.line;
    objs.pos:= current_token.pos;
  END;
  scan_token:= current_token.token;
  lstr.Assign(current_token.text^, scan_text);
END prev_token;

(*------------------------------------------------------*)
PROCEDURE Header();
VAR
  was_footer: BOOLEAN;
  t: scan.Token;
  pretext: PreText;
  expr: RegExpr;

  PROCEDURE get_headers();
  VAR
    reg_comp_res: LONGINT;
  BEGIN
    NEW(pretext);
    adt.NewList(pretext.patterns);
    adt.NewList(pretext.exceptions);
    adt.NewList(pretext.pretokens);
    adt.NewList(pretext.posttokens);
    IF (scan_token = objs.nonstd_header_name)
		   OR
       (scan_token = objs.std_header_name)
    (* There were many names in 'header' directive.
       Now there is only one.
    WHILE ((scan_token = objs.nonstd_header_name)
			   OR
	   (scan_token = objs.std_header_name))
			   &
		   (scan_text^ # "!")
    DO
    *)
    THEN
      NEW(expr);
      RegComp.Compile(scan_text^, expr.expr, reg_comp_res);
      IF reg_comp_res <= 0 THEN
	(* error*)
	Error(60, '');
	RETURN;
      END;
      pretext.patterns.Insert(expr);
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    END;
    (* There was exception part in 'header' directive.
       Now it is history.
    IF (scan_token = objs.nonstd_header_name)
			   OR
	   (scan_token = objs.std_header_name)
    THEN
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      WHILE (scan_token = objs.nonstd_header_name)
			     OR
	     (scan_token = objs.std_header_name)
      DO
	NEW(expr);
	RegComp.Compile(scan_text^, expr.expr, reg_comp_res);
	IF reg_comp_res <= 0 THEN
	  (* error*)
	  Error(60, '');
	  RETURN;
	END;
	pretext.exceptions.Insert(expr);
	GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      END;
    END;
    *)
  END get_headers;

BEGIN
  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  get_headers(); IF msg.WasError THEN RETURN END;
  PushCommentsList(pretext.pretokens);
  IF scan_token = objs.eol THEN
    current_token.parsed:= TRUE;
    next_token(); IF msg.WasError THEN RETURN END;
  ELSE
    (* error *)
    Error(20, '');
    RETURN;
  END;
  WHILE       (scan_token # objs.eof)
			  &
	    (scan_token # objs.pd_end)
			  &
	    (scan_token # objs.pd_footer)
  DO
    CopyToken(t, current_token);
    pretext.pretokens.Insert(t);
    current_token.parsed:= TRUE;
    next_token(); IF msg.WasError THEN RETURN END;
  END;
  IF scan_token = objs.eof THEN
    (* error *)
    Error(5, '#end');
    RETURN;
  END;
  PopCommentsList();
  was_footer:= scan_token = objs.pd_footer;
  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  IF scan_token # objs.eol THEN
    (* error *)
    Error(20, '');
    RETURN;
  ELSIF was_footer THEN
    PushCommentsList(pretext.posttokens);
    current_token.parsed:= TRUE;
    next_token(); IF msg.WasError THEN RETURN END;
    WHILE	(scan_token # objs.eof)
			    &
		(scan_token # objs.pd_end)
    DO
      CopyToken(t, current_token);
      pretext.posttokens.Insert(t);
      current_token.parsed:= TRUE;
      next_token(); IF msg.WasError THEN RETURN END;
    END;
    IF scan_token = objs.eof THEN
      (* error *)
      Error(5, '#end');
      RETURN;
    END;
    PopCommentsList();
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    IF scan_token # objs.eol THEN
      (* error *)
      Error(20, '');
      RETURN;
    END;
  END;
  pretext_list.Insert(pretext);
END Header;
(*------------------------------------------------------*)
PROCEDURE ParseDesignator(VAR designator: Designator);
VAR
  des: Designator;
  result: ConvTypes.ConvResults;
BEGIN
  IF scan_token = objs.ident THEN
    NewDesignator(designator, scan_text^, objs.ident);
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    LOOP
      CASE scan_token OF
	 objs.reference:
	   NewDesignator(des, '', objs.reference);
	   designator.list.Insert(des);
	|objs.open_round_brack:
	   NewDesignator(des, '', objs.open_round_brack);
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   IF scan_token = objs.int_const THEN
	     WholeStr.StrToInt(scan_text^, des.num, result);
	     GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	     IF scan_token # objs.close_round_brack THEN
	       (* error *)
	       Error(5, "')'");
	       RETURN;
	     END;
	   ELSE
	     (* error *)
	     Error(51, '');
	   END;
	   designator.list.Insert(des);
	|objs.open_square_brack:
	   NewDesignator(des, '', objs.open_square_brack);
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   IF scan_token # objs.close_square_brack THEN
	     (* error *)
	     Error(5, "']'");
	     RETURN;
	   END;
	   designator.list.Insert(des);
	|objs.point:
	   NewDesignator(des, '', objs.point);
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   IF scan_token = objs.ident THEN
	     designator.list.Insert(des);
	     NewDesignator(des, scan_text^, objs.ident);
	   ELSE
	     (* error *)
	     Error(27, '');
	     RETURN;
	   END;
	   designator.list.Insert(des);
      ELSE
	EXIT;
      END;
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    END;
  ELSE
    (* error *)
    Error(27, '');
  END;
END ParseDesignator;

(*------------------------------------------------------*)
(* Obsolete procedure
PROCEDURE Parameters();
VAR
  designator: Designator;
  comments: BOOLEAN;
  e: adt.Element;
BEGIN
  C_extensions:= FALSE;
  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  IF scan_token = objs.eol THEN
    comments:= scan.comments;
    scan.comments:= FALSE;
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    WHILE (scan_token # objs.pd_end) DO
      IF scan_token = objs.eof THEN
	(* error *)
	Error(24, '');
	RETURN;
      ELSE
	ParseDesignator(designator); IF msg.WasError THEN RETURN END;
	designator.list.FindLast(e);
	IF e(Designator).designator # objs.open_round_brack THEN
	  DesignatorError(e(Designator));
	  RETURN;
	ELSIF scan_token = objs.two_points THEN
	  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	  IF (scan_token = objs.ident) & (scan_text^ = 'VAR') THEN
	    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	    designator.var:= TRUE;
	  END;
	  IF (scan_token = objs.ident) & (scan_text^ = 'ARRAY') THEN
	    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	    designator.array:= TRUE;
	  END;
	  IF designator.var OR designator.array THEN
	    objs.CurrentHeader.Parameters.Insert(designator);
	  ELSE
	    (* error *)
	    Error(53, '');
	    RETURN;
	  END;
	END;
	IF scan_token = objs.point_comma THEN
	  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	ELSE
	  (* error *)
	  Error(5, "';'");
	  RETURN;
	END;
      END;
    END;
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    IF scan_token # objs.eol THEN
      (* error *)
      Error(20, '');
      RETURN;
    END;
  ELSE
    (* error *)
    Error(20, '');
    RETURN;
  END;
  scan.comments:= comments;
  C_extensions:= TRUE;
END Parameters;
*)

(*------------------------------------------------------*)
(*  Obsolete procedure
PROCEDURE BitSet();
VAR
  designator: Designator;
  comments: BOOLEAN;

  (*------------------------------------*)
  PROCEDURE ParseExtendedDesignator();
  BEGIN
    IF scan_token = objs.unsigned THEN
      NewDesignator(designator, '$', objs.unsigned);
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    ELSE
      ParseDesignator(designator); IF msg.WasError THEN RETURN END;
    END;
  END ParseExtendedDesignator;

  (*------------------------------------*)
BEGIN
  C_extensions:= FALSE;
  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  IF scan_token = objs.eol THEN
    comments:= scan.comments;
    scan.comments:= FALSE;
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    WHILE (scan_token # objs.pd_end) DO
      IF scan_token = objs.eof THEN
	(* error *)
	Error(24, '');
	RETURN;
      ELSE
	ParseExtendedDesignator(); IF msg.WasError THEN RETURN END;
	objs.CurrentHeader.BitSets.Insert(designator);
	IF scan_token = objs.point_comma THEN
	  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	ELSE
	  (* error *)
	  Error(5, "';'");
	  RETURN;
	END;
      END;
    END;
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    IF scan_token # objs.eol THEN
      (* error *)
      Error(20, '');
      RETURN;
    END;
  ELSE
    (* error *)
    Error(20, '');
    RETURN;
  END;
  scan.comments:= comments;
  C_extensions:= TRUE;
END BitSet;
*)

(*------------------------------------------------------*)
PROCEDURE Variant();
VAR
  designator: Designator;
  type: BOOLEAN;
  comments: BOOLEAN;
  e: adt.Element;
  str: lstr.String;

BEGIN
  C_extensions:= FALSE;
  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  comments:= scan.comments;
  scan.comments:= FALSE;
  type:= FALSE;
  ParseDesignator(designator); IF msg.WasError THEN RETURN END;
  IF scan_token = objs.two_points THEN
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    IF (scan_token = objs.ident) & (scan_text^ = 'VAR') THEN
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      designator.var:= TRUE;
    END;
    IF (scan_token = objs.ident) & (scan_text^ = 'ARRAY') THEN
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      designator.array:= TRUE;
    END;
    IF scan_token = objs.ident THEN
      type:= TRUE;
      lstr.Assign(scan_text^, designator.type_name);
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      IF scan_token = objs.point THEN
	lstr.Append('.', designator.type_name);
	GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	IF (scan_token # objs.ident) &
	   ((scan_token < objs.CKeyWords_base) OR (scan_token > objs.CKeyWords_max))
	THEN
	  (* error *)
	  Error(27, '');
	  RETURN;
	END;
	lstr.Append(scan_text^, designator.type_name);
	GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      END;
    END;
    IF (designator.var OR designator.array) & ~type THEN
      designator.list.FindLast(e);
      IF e(Designator).designator # objs.open_round_brack THEN
	DesignatorError(e(Designator));
	RETURN;
      ELSE
	objs.CurrentHeader.Parameters.Insert(designator);
      END;
    ELSIF type & ~(designator.var OR designator.array) THEN
      objs.CurrentHeader.ModifiedObjects.Insert(designator);
    ELSIF ~designator.var & ~designator.array & ~type THEN
      (* error *)
      Error(53, '');
      RETURN;
    ELSE
      (* error *)
      Error(59, '');
      RETURN;
    END;
  ELSE
    (* error *)
    Error(5, "':'");
    RETURN;
  END;
  IF scan_token # objs.eol THEN
    (* error *)
    Error(20, '');
    RETURN;
  END;
  scan.GetPrevLogicalString(str);
  msg.WriteDirFile(str^);
  scan.comments:= comments;
  C_extensions:= TRUE;
END Variant;

(*------------------------------------------------------*)
PROCEDURE MakeMergedHeaderComment(name-: ARRAY OF CHAR);
VAR
  comment: objs.Comment;
  str1, str2, str3: lstr.String;
  i: INT;

BEGIN
  IF cfg.HeadInserting THEN
    str1:= NIL; str2:= NIL; str3:= NIL;
    FOR i:= 0 TO lstr.Length(name) + 39 DO
      lstr.Append('-', str1);
    END;

    FOR i:= 0 TO lstr.Length(name) + 39 DO
      lstr.Append(' ', str2);
    END;

    FOR i:= 0 TO 19 DO
      lstr.Append(' ', str3);
    END;
    lstr.Append(name, str3);
    FOR i:= 0 TO 19 DO
      lstr.Append(' ', str3);
    END;

    objs.NewComment(comment, -10, str1^);
    objs.CurrentHeader.objects.Insert(comment);
(*
    objs.NewComment(comment, -10, str2^);
    objs.CurrentHeader.objects.Insert(comment);
*)
    objs.NewComment(comment, -10, str3^);
    objs.CurrentHeader.objects.Insert(comment);
(*
    objs.NewComment(comment, -10, str2^);
    objs.CurrentHeader.objects.Insert(comment);
*)
    objs.NewComment(comment, -10, str1^);
    objs.CurrentHeader.objects.Insert(comment);
  END;
END MakeMergedHeaderComment;

(*------------------------------------------------------*)
PROCEDURE NewMergedHeader(name-: ARRAY OF CHAR);
VAR
  ne: adt.NamedElement;
BEGIN
  adt.NewNamedElement(ne, name);
  objs.CurrentHeader.mergedheaders.Insert(ne);
  MakeMergedHeaderComment(name);
  merged_headers_stack.Push(ne);
END NewMergedHeader;

(*------------------------------------------------------*)
PROCEDURE GetToken( parsed: BOOLEAN );
VAR
  hse: HeaderStackElement;
  expr: objs.Expression;
  result: objs.ConstantValue;
  local_if_balance: INT;
  e: adt.Element;
  ne: adt.NamedElement;
  ifdef_ifndef: BOOLEAN;
  namesyn: objs.NameSynonym;
  t: scan.Token;
  str: lstr.String;
  str1: ARRAY 20 OF CHAR;
BEGIN
  LOOP(*LOOP0*)
    IF ( (current_token.token = objs.pd_define	   ) OR
	 (current_token.token = objs.pd_if	   ) OR
	 (current_token.token = objs.pd_endif	   ) OR
	 (current_token.token = objs.pd_elif	   ) OR
	 (current_token.token = objs.pd_else	   ) OR
	 (current_token.token = objs.pd_ifdef	   ) OR
	 (current_token.token = objs.pd_ifndef	   ) OR
	 (current_token.token = objs.pd_undef	   ) OR
	 (current_token.token = objs.pd_line	   ) OR
	 (current_token.token = objs.pd_error	   ) OR
	 (current_token.token = objs.pd_include    ) OR
	 (current_token.token = objs.pd_pragma	   ) OR
	 (current_token.token = objs.pd_merge	   ) OR
	 (*
	 (current_token.token = objs.pd_bitset	   ) OR
	 *)
	 (current_token.token = objs.pd_header	   ) OR
	 (current_token.token = objs.pd_name	   ) OR
	 (current_token.token = objs.pd_variant    ) OR
	 (*
	 (current_token.token = objs.pd_parameters ) OR
	 *)
	 (current_token.token = objs.eol	   ) )
    THEN
      current_token.parsed:= TRUE;
    ELSE
      current_token.parsed:= current_token.parsed OR parsed;
    END;
    next_token(); IF msg.WasError THEN RETURN END;
    CASE scan_token OF
      |objs.eof:
	 IF ~config_on & ~project_on & cfg.IncludingTree THEN
	   msg.WriteLogFile('', FALSE);
	 END;
	 IF include_balance > 0 THEN
	   DEC(include_balance); headers_stack.Pop(e);
	   token_list:= e(adt.List);
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   IF scan_token = objs.eol THEN
	     merged_headers_stack.Pop(e);
	     merged_headers_stack.Pop(e);
	     IF e # NIL THEN
	       MakeMergedHeaderComment(e(adt.NamedElement).name^);
	       merged_headers_stack.Push(e);
	     END;
	   ELSE
	     (* error *)
	     Error(20, '');
	     RETURN;
	   END;
	 ELSIF if_balance > 0 THEN
	   (* error *)
	   if_stack.Pop(e); t:= e(scan.Token);
	   str:= NIL; lstr.Assign('#', str);
	   lstr.Append(t.text^, str); lstr.Append(" [", str);
	   lstr.Append(t.file^, str); lstr.Append(" ", str);
	   WholeStr.IntToStr(t.line, str1); lstr.Append(str1, str);
	   lstr.Append(":", str);
	   WholeStr.IntToStr(t.pos+1, str1); lstr.Append(str1, str);
	   lstr.Append("]", str);
	   Error(65, str^); lstr.Deallocate(str);
	   RETURN;
	 ELSE
	   EXIT(*LOOP0*);
	 END;
      |objs.pd_if:
	 INC(if_balance); CopyToken(t, current_token);
	 if_stack.Push(t);
	 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	 limited_constant_expression_on:= TRUE;
	 LOOP(*LOOP1*)
	   ConstExpr(expr); IF msg.WasError THEN RETURN END;
	   IF scan_token = objs.eol THEN
	     IF expr # NIL THEN
	       expr.ComputeExpression(result, limited_constant_expression_on);
	       RemoveExpression(expr);
	     ELSE
	       (* error *)
	       Error(21, '');
	       RETURN;
	     END;
	     IF msg.WasError THEN RETURN END;
	     IF result.int = 0 THEN
	       objs.Deallocate(result);
	       scan.skipping_mode:= TRUE;
	       local_if_balance:= 0;
	       current_token.parsed:= TRUE;
	       next_token(); IF msg.WasError THEN RETURN END;
	       LOOP(*LOOP2*)
		 IF ((scan_token = objs.pd_elif)
				 OR
		     (scan_token = objs.pd_else)
				 OR
		     (scan_token = objs.pd_endif))
				 &
			 (local_if_balance = 0)
		 THEN
		   EXIT(*LOOP2*);
		 ELSE
		   IF (scan_token = objs.pd_if)
				 OR
		      (scan_token = objs.pd_ifdef)
				 OR
		      (scan_token = objs.pd_ifndef)
		   THEN
		     INC(local_if_balance);
		   ELSIF scan_token = objs.pd_endif THEN
		     DEC(local_if_balance);
		   END;
		   current_token.parsed:= TRUE;
		   next_token(); IF msg.WasError THEN RETURN END;
		 END;
	       END(*LOOP2*);
	       scan.skipping_mode:= FALSE;
	       IF scan_token = objs.pd_else THEN
		 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
		 IF scan_token = objs.eol THEN
		   EXIT(*LOOP1*);
		 ELSE
		   (* error *)
		   Error(20, '');
		   RETURN;
		 END;
	       ELSIF scan_token = objs.pd_endif THEN
		 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
		 IF scan_token = objs.eol THEN
		   DEC(if_balance); if_stack.Pop(e); empty_token_list.Insert(e);

		   EXIT(*LOOP1*);
		 ELSE
		   (* error *)
		   Error(20, '');
		   RETURN;
		 END;
	       ELSE
		 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	       END;
	     ELSE
	       objs.Deallocate(result);
	       EXIT(*LOOP1*);
	     END;
	   ELSE
	     (* error *)
	     Error(20, '');
	     RETURN;
	   END;
	 END(*LOOP1*);
	 limited_constant_expression_on:= FALSE;
      |objs.pd_endif:
	 IF if_balance <= 0 THEN
	   (* error *)
	   Error(2, '#endif');
	   RETURN;
	 ELSE
	   DEC(if_balance); if_stack.Pop(e); empty_token_list.Insert(e);
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   IF scan_token # objs.eol THEN
	     (* error *)
	     Error(20, '');
	     RETURN;
	   END;
	 END;
      |objs.pd_elif, objs.pd_else:
	 local_if_balance:= 0;
	 scan.skipping_mode:= TRUE;
	 LOOP(*LOOP3*)
	   IF (scan_token = objs.pd_endif)
			  &
		 (local_if_balance = 0)
	   THEN
	     EXIT(*LOOP3*);
	   ELSE
	     IF (scan_token = objs.pd_if)
			   OR
		(scan_token = objs.pd_ifdef)
			   OR
		(scan_token = objs.pd_ifndef)
	     THEN
	       INC(local_if_balance);
	     ELSIF scan_token = objs.pd_endif THEN
	       DEC(local_if_balance);
	     END;
	     current_token.parsed:= TRUE;
	     next_token(); IF msg.WasError THEN RETURN END;
	   END;
	 END(*LOOP3*);
	 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	 IF scan_token = objs.eol THEN
	   scan.skipping_mode:= FALSE;
	   DEC(if_balance); if_stack.Pop(e); empty_token_list.Insert(e);
	 ELSE
	   (* error *)
	   Error(20, '');
	   RETURN;
	 END;
      |objs.pd_ifdef, objs.pd_ifndef:
	 INC(if_balance); CopyToken(t, current_token);
	 if_stack.Push(t);
	 ifdef_ifndef:= scan_token = objs.pd_ifdef;
	 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	 IF scan_token = objs.ident THEN
	   adt.NewNamedElement(ne, scan_text^);
--	     objs.CurrentHeader.defined.Find(ne, e);
	   objs.FindInDefined(objs.CurrentHeader, ne, e);
	   adt.Deallocate(ne);
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   IF (e = NIL) & ~ifdef_ifndef
		       OR
	      (e # NIL) & ifdef_ifndef
	   THEN
	     IF scan_token # objs.eol THEN
	       (* error *)
	       Error(20, '');
	       RETURN;
	     END;
	   ELSE
	     limited_constant_expression_on:= TRUE;
	     LOOP(*LOOP4*)
	       IF scan_token = objs.eol THEN
		 current_token.parsed:= TRUE;
		 scan.skipping_mode:= TRUE;
		 next_token(); IF msg.WasError THEN RETURN END;
		 local_if_balance:= 0;
		 LOOP(*LOOP5*)
		   IF ((scan_token = objs.pd_elif)
				     OR
			 (scan_token = objs.pd_else)
				     OR
			 (scan_token = objs.pd_endif))
				     &
			    (local_if_balance = 0)
		   THEN
		     EXIT(*LOOP5*);
		   ELSE
		     IF (scan_token = objs.pd_if)
				   OR
			(scan_token = objs.pd_ifdef)
				   OR
			(scan_token = objs.pd_ifndef)
		     THEN
		       INC(local_if_balance);
		     ELSIF scan_token = objs.pd_endif THEN
		       DEC(local_if_balance);
		     END;
		     current_token.parsed:= TRUE;
		     next_token(); IF msg.WasError THEN RETURN END;
		   END;
		 END(*LOOP5*);
		 scan.skipping_mode:= FALSE;
		 IF scan_token = objs.pd_else THEN
		   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
		   IF scan_token = objs.eol THEN
		     EXIT(*LOOP4*);
		   ELSE
		     (* error *)
		     Error(20, '');
		     RETURN;
		   END;
		 ELSIF scan_token = objs.pd_endif THEN
		   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
		   IF scan_token = objs.eol THEN
		     DEC(if_balance); if_stack.Pop(e); empty_token_list.Insert(e);
		     EXIT(*LOOP4*);
		   ELSE
		     (* error *)
		     Error(20, '');
		     RETURN;
		   END;
		 ELSE
		   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
		 END;

		 ConstExpr(expr); IF msg.WasError THEN RETURN END;
		 expr.ComputeExpression(result, limited_constant_expression_on);
		 IF msg.WasError THEN RETURN END;
		 IF result.int # 0 THEN
		   objs.Deallocate(result);
		   IF scan_token = objs.eol THEN
		     EXIT(*LOOP4*);
		   ELSE
		     (* error *)
		     Error(20, '');
		     RETURN;
		   END;
		 END;
		 objs.Deallocate(result);
	       ELSE
		 (* error *)
		 Error(20, '');
		 RETURN;
	       END;
	     END(*LOOP4*);
	     limited_constant_expression_on:= FALSE;
	   END;
	 ELSE
	   (* error *)
	   Error(26, '');
	   RETURN;
	 END;
      |objs.pd_include: (*+*)
	 IF ~config_on THEN
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   (*
	   CheckHeader(scan_text^);
	   *)
	   adt.NewNamedElement(ne, scan_text^);
	   merge_headers_tree.Find(ne, e);
	   IF ~project_on & cfg.IncludingTree THEN
	     msg.WriteLogFile(scan_text^, cfg.HeadersMerging OR (e # NIL));
	   END;
	   IF (cfg.HeadersMerging) OR (e # NIL) OR project_on THEN
	     objs.CurrentHeader.mergedheaders.Find(ne, e); adt.Deallocate(ne);
	     IF e = NIL THEN
	       INC(include_balance);
	       scan.transparent_numeration:= TRUE;
	       IF Open(scan_text^, TRUE) THEN
		 NewMergedHeader(scan_text^);
		 headers_stack.Push(token_list);
		 IF ~project_on THEN
		   PutPreText(scan_text^);
		 END;
		 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
		 EXIT(*LOOP0*);
	       ELSE
		 RETURN;
	       END;
	     ELSE
	       GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	       IF scan_token # objs.eol THEN
		 (* error *)
		 Error(20, '');
		 RETURN;
	       END;
	       IF cfg.IncludingTree THEN msg.WriteLogFile('', FALSE) END;
	     END;
	   ELSE
	     objs.CurrentHeader.includes.Find(ne, e); adt.Deallocate(ne);
	     IF e = NIL THEN
	       NEW(hse);
	       hse.header		   := objs.CurrentHeader      ;
	       hse.token_list		   := token_list	      ;
	       hse.current_token	   := current_token	      ;
	       hse.scan_token		   := scan_token	      ;
	       lstr.Assign(scan_text^, hse.scan_text)		      ;
	       hse.tag_was_parsed	   := tag_was_parsed	      ;
	       hse.if_balance		   := if_balance	      ;
	       hse.if_stack		   := if_stack		      ;
	       hse.end_balance		   := end_balance	      ;
	       hse.include_balance	   := include_balance	      ;
	       hse.transparent_numeration  := scan.transparent_numeration;
	       hse.C_extensions 	   := C_extensions;
	       hse.limited_constant_expression_on := limited_constant_expression_on;

	       headers_stack.Push(hse);

	       scan.transparent_numeration:= FALSE;
	       Parse(scan_text^, scan_token = objs.std_header_name);
	       IF msg.WasError THEN RETURN END;

	       headers_stack.Pop(e); hse:= e(HeaderStackElement);
	       merge_headers(hse.header, objs.CurrentHeader, TRUE(*~IsEmptyHeader(objs.CurrentHeader)*), FALSE);
	       objs.CurrentHeader      := hse.header		      ;
	       token_list	       := hse.token_list	      ;
	       current_token	       := hse.current_token	      ;
	       scan_token	       := hse.scan_token	      ;
	       lstr.Assign(hse.scan_text^, scan_text)		      ;
	       tag_was_parsed	       := hse.tag_was_parsed	      ;
	       if_balance	       := hse.if_balance	      ;
	       if_stack 	       := hse.if_stack		      ;
	       end_balance	       := hse.end_balance	      ;
	       include_balance	       := hse.include_balance	      ;
	       scan.transparent_numeration  := hse.transparent_numeration;
	       C_extensions	       := hse.C_extensions;
	       limited_constant_expression_on := hse.limited_constant_expression_on;
	       GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	       IF scan_token # objs.eol THEN
		 (* error *)
		 Error(20, '');
		 RETURN;
	       END;
	     ELSE
	       GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	       IF scan_token # objs.eol THEN
		 (* error *)
		 Error(20, '');
		 RETURN;
	       END;
	       IF cfg.IncludingTree THEN msg.WriteLogFile('', FALSE) END;
	     END;
	   END;
	 END;
      |objs.pd_merge:
         IF C_extensions & ~config_on & ~project_on THEN
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   adt.NewNamedElement(ne, scan_text^);
	   merge_headers_tree.Insert(ne);
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   IF scan_token # objs.eol THEN
	     (* error *)
	     Error(20, '');
	     RETURN;
	   END;
           scan.GetPrevLogicalString(str);
           msg.WriteDirFile(str^);
	 ELSE
	   EXIT(*LOOP0*);
	 END;
      |objs.pd_header:
	 IF project_on THEN
	   Header(); IF msg.WasError THEN RETURN END;
	 ELSE
	   EXIT(*LOOP0*);
	 END;
      (*
      |objs.pd_bitset:
	 IF C_extensions & ~config_on & ~project_on THEN
	   BitSet(); IF msg.WasError THEN RETURN END;
	 ELSE
	   EXIT(*LOOP0*);
	 END;
      *)
      (*
      |objs.pd_parameters:
	 IF C_extensions & ~config_on & ~project_on THEN
	   Parameters(); IF msg.WasError THEN RETURN END;
	 ELSE
	   EXIT(*LOOP0*);
	 END;
      *)
      |objs.pd_variant:
	 IF C_extensions & ~config_on & ~project_on THEN
	   Variant(); IF msg.WasError THEN RETURN END;
	 ELSE
	   EXIT(*LOOP0*);
	 END;
      |objs.pd_name:
	 IF project_on THEN
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   NEW(namesyn); namesyn.SetName(scan_text^);
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   IF scan_token = objs.ident THEN
	     namesyn.newname:= NIL;
	     lstr.Assign(scan_text^, namesyn.newname);
	     objs.ModuleNames.Insert(namesyn);
	   ELSE
	     (* error *)
	     Error(27, '');
	     RETURN;
	   END;
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   IF scan_token # objs.eol THEN
	     (* error *)
	     Error(20, '');
	     RETURN;
	   END;
	 ELSE
	   EXIT(*LOOP0*);
	 END;
      |objs.pd_pragma,objs.pd_line,objs.pd_error:
	 REPEAT
	   current_token.parsed:= TRUE;
	   next_token(); IF msg.WasError THEN RETURN END;
	 UNTIL scan_token = objs.eol;
      |objs.pd_define:
	 Define();
      |objs.pd_undef:
	 Undef();
    ELSE
      EXIT(*LOOP0*);
    END;
  END(*LOOP0*);
END GetToken;


(*------------------------------------------------------*)
PROCEDURE open_boundary();
VAR e: adt.Element;
BEGIN
  REPEAT
    token_list.FindPrev(e);
  UNTIL (e = NIL) OR e(scan.Token).boundary;
  IF e # NIL THEN
    e(scan.Token).boundary:= FALSE;
    current_token:= e(scan.Token);
  ELSE
    (* error *)
    Error(28, 'H2DParse.open_boundary');
    RETURN;
  END;
END open_boundary;

(*------------------------------------------------------*)
(*------------------------------------------------------*)
(*------------------------------------------------------*)
(*------------------------------------------------------*)
(*------------------------------------------------------*)
PROCEDURE CheckFullDefinition(tobj: objs.TypedObject);
BEGIN
  IF ((tobj.type.type = objs.t_struct)
		     OR
     (tobj.type.type = objs.t_union))
		     &
	   tobj.type.not_described
		     &
      (tobj.mem_class # objs.extern)
  THEN
    (* error *)
    Error(29, tobj.name^);
    RETURN;
  END;
END CheckFullDefinition;

(*------------------------------------------------------*)
PROCEDURE CheckIntExpr(expr: objs.Expression; boundary: INT);
(* If boundary =  1 then expr must be more than 0
   If boundary =  0 then expr must be more or equal 0
   If boundary = -1 then expr may  be any value
*)
VAR
 res: objs.ConstantValue;
BEGIN
  expr.ComputeExpression(res, FALSE); IF msg.WasError THEN RETURN END;
  IF (res.type # objs.int_const) & (res.type # objs.uint_const) THEN
    (* error *)
    line:= expr.line; pos:= expr.pos;
    Error(30, '');
    RETURN;
  END;
  IF (boundary = 1) THEN
    IF (res.type # objs.uint_const) & (res.int <= 0) THEN
      (* error *)
      line:= expr.line; pos:= expr.pos;
      Error(31, '');
    END;
  ELSIF (boundary = 0) THEN
    IF (res.type # objs.uint_const) & (res.int < 0) THEN
      (* error *)
      line:= expr.line; pos:= expr.pos;
      Error(32, '');
    END;
  END;
  objs.Deallocate(res);
END CheckIntExpr;

(*------------------------------------------------------*)
PROCEDURE Type(VAR type: objs.Type; VAR modifier: INT; VAR type_is_present: BOOLEAN; VAR tag_was_parsed: BOOLEAN);
VAR
  unsigned, signed, long, short: BOOLEAN;
  e:  adt.Element;
  ne: adt.NamedElement;
  tobj: objs.TypedObject;
  last_enum_value: INT;
  val: objs.Value;
  str: ARRAY 20 OF CHAR;
  res: objs.ConstantValue;
  local_type: INT;
  second_definition: BOOLEAN;
  inserting: BOOLEAN;
BEGIN

(*    Parse possible modifier	 *)
  tag_was_parsed:= FALSE;
  IF scan_token = objs.const THEN
    modifier:= objs.const;
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  ELSIF scan_token = objs.volatile THEN
    modifier:= objs.volatile;
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  ELSE
    modifier:= objs.nonmodified;
  END;

(*    Parse type specification	  *)

  signed:= FALSE;
  unsigned:= FALSE;
  long:= FALSE;
  short:= FALSE;

  objs.NewType(type);
  type_is_present:= TRUE;

  IF scan_token = objs.signed THEN
    signed:= TRUE;
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  ELSIF scan_token = objs.unsigned THEN
    unsigned:= TRUE;
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  END;

  IF scan_token = objs.long THEN
    long:= TRUE;
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  ELSIF scan_token = objs.short THEN
    short:= TRUE;
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  END;

  CASE scan_token OF
     objs.int	 :
       IF unsigned THEN
	 IF    long  THEN  type.type:= objs.bt_u_l_int;
	 ELSIF short THEN  type.type:= objs.bt_u_sh_int;
	 ELSE		   type.type:= objs.bt_u_int;
	 END;
       ELSE
	 IF    long  THEN  type.type:= objs.bt_s_l_int;
	 ELSIF short THEN  type.type:= objs.bt_s_sh_int;
	 ELSE		   type.type:= objs.bt_s_int;
	 END;
       END;
    |objs.char	 :
       IF unsigned THEN
	 type.type:= objs.bt_u_char;
       ELSE
	 type.type:= objs.bt_s_char;
       END;
    |objs.float  :
       IF signed OR unsigned OR short THEN
	 (* error *)
	 Error(33, '');
	 RETURN;
       ELSIF long THEN
	 type.type:= objs.bt_l_float;
       ELSE
	 type.type:= objs.bt_float;
       END;
    |objs.double :
       IF signed OR unsigned OR short THEN
	 (* error *)
	 Error(33, '');
	 RETURN;
       ELSIF long THEN
	 type.type:= objs.bt_l_double;
       ELSE
	 type.type:= objs.bt_double;
       END;
    |objs.void:
       IF signed OR unsigned OR short OR long THEN
	 (* error *)
	 Error(33, '');
	 RETURN;
       ELSE
	 type.type:= objs.bt_void;
       END;
    |objs.struct, objs.union:
       inserting:= FALSE;
       IF scan_token = objs.struct THEN
	 type.type:= objs.t_struct;
       ELSE
	 type.type:= objs.t_union;
       END;
       local_type:= type.type;
       type.not_described:= TRUE; second_definition:= FALSE;
       GetToken( TRUE ); IF msg.WasError THEN RETURN END;
       tag_was_parsed:= scan_token = objs.ident;
       IF tag_was_parsed THEN
	 adt.NewNamedElement(ne, scan_text^); type.line:= line; SetFileName(type.file, file_name);
	 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
--	   objs.CurrentHeader.Tags.Find(ne, e);
	 objs.FindInTags(objs.CurrentHeader, ne, e);
	 IF e # NIL THEN
	   adt.Deallocate(ne);
	   second_definition:= TRUE;
	   objs.Deallocate(type);
	   type:= e(objs.Type);
	   IF (type.type = local_type) THEN
	     IF ~type.not_described THEN
	       tag_was_parsed:= FALSE;
	       IF scan_token = objs.open_figure_brack THEN
		 (* error *)
		 Error(19, type.name^, type.file^, type.line);
	       END;
	       RETURN;
	     END;
	   ELSE
	     (* error *)
	     Error(19, type.name^, type.file^, type.line);
	     RETURN;
	   END;
	 ELSE
	   type.SetName(ne.name^); adt.Deallocate(ne);
	   InsertToTags(type);
	   IF msg.WasError THEN RETURN END;
	   inserting:= TRUE;
	 END;
       END;
       IF scan_token = objs.open_figure_brack THEN
	 type.not_described:= FALSE;
	 type.tag_description_header:= objs.CurrentHeader;
	 inserting:= inserting OR (type.tag_description_header # type.header);
	 PushCommentsList(type.vars);
	 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	 Declarations(TRUE, type.vars);
	 PopCommentsList();
	 IF msg.WasError THEN RETURN END;
	 IF scan_token = objs.close_figure_brack THEN
	   IF inserting THEN
	     inserting:= FALSE;
	     objs.CurrentHeader.objects.Insert(type);
	   END;
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	 ELSE
	   (* error *)
	   Error(5, "'}'");
	 END;
       ELSIF ~tag_was_parsed THEN
	 (* error *)
	 Error(54, '');
       ELSIF second_definition THEN
	 tag_was_parsed:= FALSE;
       END;
       IF inserting THEN
	 objs.CurrentHeader.objects.Insert(type);
       END;
       RETURN;
    |objs.enum:
       inserting:= FALSE;
       last_enum_value:= -1;
       type.type:= objs.t_enum;
       type.not_described:= TRUE; second_definition:= FALSE;
       GetToken( TRUE ); IF msg.WasError THEN RETURN END;
       tag_was_parsed:= scan_token = objs.ident;
       IF tag_was_parsed THEN
	 adt.NewNamedElement(ne, scan_text^); type.line:= line; SetFileName(type.file, file_name);
--	   objs.CurrentHeader.Tags.Find(ne, e);
	 objs.FindInTags(objs.CurrentHeader, ne, e);
	 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	 IF e # NIL THEN
	   second_definition:= TRUE;
	   objs.Deallocate(type);
	   type:= e(objs.Type);
	   IF type.type = objs.t_enum THEN
	     IF ~type.not_described THEN
	       tag_was_parsed:= FALSE;
	       RETURN;
	     END;
	   ELSE
	     (* error *)
	     Error(19, scan_text^, type.file^, type.line);
	     RETURN;
	   END;
	 ELSE
	   type.SetName(ne.name^);
	   InsertToTags(type);
	   IF msg.WasError THEN RETURN END;
	   inserting:= TRUE;
	 END;
       END;
       IF scan_token = objs.open_figure_brack THEN
	 type.not_described:= FALSE;
	 type.tag_description_header:= objs.CurrentHeader;
	 PushCommentsList(type.vars);
	 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	 LOOP
	   IF scan_token = objs.ident THEN
	     objs.NewTypedObject(tobj, objs.enum_const);
	     tobj.SetName(scan_text^); tobj.line:= line; SetFileName(tobj.file, file_name);
	     tobj.type:= type;
	     InsertToList(type.vars,tobj);
	     IF msg.WasError THEN RETURN END;
	     InsertToNames(tobj);
	     IF msg.WasError THEN RETURN END;
	     GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	     IF scan_token = objs.assign THEN
	       GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	       ConstExpr(tobj.expr); IF msg.WasError THEN RETURN END;
	       IF tobj.expr = NIL THEN
		 (* error *)
		 Error(21, '');
		 RETURN;
	       ELSE
		 CheckIntExpr(tobj.expr, -1); IF msg.WasError THEN RETURN END;
		 tobj.expr.ComputeExpression(res, FALSE);
		 last_enum_value:= res.int;
		 objs.Deallocate(res);
	       END;
	     ELSE
	       INC(last_enum_value);
	       objs.NewValue(val); val.hex:= FALSE; val.value:= NIL;
	       val.type:= objs.int_const;
	       WholeStr.IntToStr(last_enum_value, str);
	       lstr.Assign(str, val.value);
	       tobj.expr:= val;
	     END;
	     IF scan_token = objs.comma THEN
	       GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	     ELSE
	       EXIT;
	     END;
	   ELSE
	     (* error *)
	     Error(27, '');
	     RETURN;
	   END;
	 END;
	 PopCommentsList();
	 IF scan_token = objs.close_figure_brack THEN
	   IF inserting THEN
	     inserting:= FALSE;
	     objs.CurrentHeader.objects.Insert(type);
	   END;
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	 ELSE
	   (* error *)
	   Error(5, "'}'");
	   RETURN;
	 END;
       ELSIF ~tag_was_parsed THEN
	 (* error *)
	 Error(54, '');
       ELSIF second_definition THEN
	 tag_was_parsed:= FALSE;
       END;
       IF inserting THEN
	 objs.CurrentHeader.objects.Insert(type);
       END;
       RETURN;
  ELSE
    IF scan_token = objs.ident THEN
      adt.NewNamedElement(ne, scan_text^);
--	objs.CurrentHeader.Names.Find(ne, e); adt.Deallocate(ne);
      objs.FindInNames(objs.CurrentHeader, ne, e);
      IF (e # NIL) & (e IS objs.Type) & (e(objs.Type).type = objs.t_synonym) THEN
	objs.Deallocate(type);
	type:= e(objs.Type);
	GetToken( TRUE ); RETURN;
      END;
    END;
    IF	 unsigned THEN
      IF    long  THEN type.type:= objs.bt_u_l_int;
      ELSIF short THEN type.type:= objs.bt_u_sh_int;
      ELSE	       type.type:= objs.bt_u_int;
      END;
    ELSIF signed  THEN
      IF    long  THEN type.type:= objs.bt_s_l_int;
      ELSIF short THEN type.type:= objs.bt_s_sh_int;
      ELSE	       type.type:= objs.bt_s_int;
      END;
    ELSIF long	  THEN type.type:= objs.bt_s_l_int;
    ELSIF short   THEN type.type:= objs.bt_s_sh_int;
    ELSE	       type.type:= objs.bt_s_int;
		       type_is_present:= FALSE;
    END;
    RETURN;
  END;
  GetToken( TRUE );
END Type;


(*------------------------------------------------------*)
(*+*)PROCEDURE Memory_Class( VAR mem_class: INT );
BEGIN
  IF (scan_token = objs.extern)   OR
     (scan_token = objs.auto)	  OR
     (scan_token = objs.register) OR
     (scan_token = objs.static)
  THEN
    mem_class:= scan_token;
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  ELSE
    mem_class:= objs.undefined;
  END;
END Memory_Class;



PROCEDURE Define();
VAR
  tobj: objs.TypedObject;
  token: scan.Token;
  r: objs.Replacement;
  mac: objs.Macro;
  ne: adt.NamedElement;
  e: adt.Element;
  was_dies: BOOLEAN;
  was_double_dies: BOOLEAN;
  first: BOOLEAN;
  _line: INT;
  comments: adt.List;
  comment : objs.Comment;
  comment_text: lstr.String;
  tokens_num: INT;

BEGIN
  adt.NewList(comments);
  tokens_num:= 0;
  PushCommentsList(comments);
  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  IF ((scan_token = objs.macro_name) OR (scan_token = objs.pmacro_name))
				     &
			 (scan_text^ = 'defined')
  THEN
    Error(34, '');
    RETURN;
  END;
  IF scan_token = objs.macro_name THEN
    current_token.boundary:= TRUE;
    objs.NewReplacement(r, scan_text^); _line:= line;
    next_token(); IF msg.WasError THEN RETURN END;
    WHILE scan_token # objs.eol DO
      INC(tokens_num);
      CopyToken(token, current_token);
      r.tokens.Insert(token);
      next_token(); IF msg.WasError THEN RETURN END;
    END;

    PopCommentsList();
    open_boundary();
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    IF (scan_token = objs.ident) & (tokens_num = 1) THEN
      (* It is synonym *)
      objs.NewTypedObject(tobj, objs.synonym);
      tobj.SetName(r.name^); tobj.line:= _line; SetFileName(tobj.file, file_name);
      objs.NewType(tobj.type); tobj.type.SetName(scan_text^);
      merge_lists(tobj.modifier, comments, FALSE);
      InsertToDefined(r);
      IF msg.WasError THEN RETURN END;
      objs.CurrentHeader.Synonyms.Insert(tobj);
      objs.CurrentHeader.Replacements.Insert(r);
      redefined_macro:= FALSE;
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    ELSE
      objs.NewTypedObject(tobj, objs.constant);
      tobj.SetName(r.name^); tobj.line:= _line; SetFileName(tobj.file, file_name);
      objs.pd_define_is_parsed:= TRUE;
      limited_constant_expression_on:= FALSE;
      ConstExpr(tobj.expr); IF msg.WasError THEN RETURN END;
      objs.pd_define_is_parsed:= FALSE;
      IF (tobj.expr = NIL) OR (scan_token # objs.eol) THEN
	(* It is not expression *)
	objs.Deallocate(tobj);
	InsertToDefined(r);
	IF msg.WasError THEN RETURN END;
	redefined_macro:= FALSE;
	objs.CurrentHeader.Replacements.Insert(r);
	scan.GetPrevLogicalString(comment_text);
	objs.NewComment(comment, _line, comment_text^);
	lstr.Deallocate(comment_text);
	objs.CurrentHeader.objects.Insert(comment);
	WHILE scan_token # objs.eol DO
	  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	END;
      ELSE
	(* It is expression *)
	merge_lists(empty_token_list, r.tokens, FALSE);
	objs.Deallocate(r);
--	  objs.CurrentHeader.defined.Find(tobj, e);
	objs.FindInDefined(objs.CurrentHeader, tobj, e);
	IF (e = NIL) OR (e(objs.Object).header # objs.CurrentHeader) THEN
	  objs.CurrentHeader.objects.Insert(tobj);
	  merge_lists(current_list_for_comments, comments, FALSE);
	END;
	InsertToDefined(tobj);
	IF msg.WasError THEN RETURN END;
	InsertToNames(tobj);
	IF msg.WasError THEN RETURN END;
	redefined_macro:= FALSE;
      END;
    END;
  ELSIF scan_token = objs.pmacro_name THEN
    objs.NewMacro(scan_text^, mac);
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    WHILE scan_token # objs.close_round_brack DO
      adt.NewNamedElement(ne, scan_text^);
      IF scan_token = objs.ident THEN
	InsertToList(mac.params,ne);
	IF msg.WasError THEN RETURN END;
      ELSE
	(* error *)
	Error(35, '');
	RETURN;
      END;
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      IF scan_token = objs.comma THEN
	GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      END;
    END;
    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
    objs.NewReplacement(r, mac.name^);
    merge_lists(r.params, mac.params, TRUE);
    was_dies:= FALSE;
    was_double_dies:= FALSE;
    first:= TRUE; adt.NewNamedElement(ne, '');
    WHILE scan_token # objs.eol DO
      IF scan_token = objs.ident THEN
	ne.SetName(scan_text^);
	r.params.Find(ne, e);
	IF (e = NIL) & was_dies THEN
	  (* error *)
	  Error(36, '');
	  RETURN;
	END;
      ELSIF (scan_token = objs.double_dies) & (first OR was_double_dies) THEN
	(* error *)
	Error(2, '##');
	RETURN;
      END;
      was_dies:= scan_token = objs.dies;
      was_double_dies:= scan_token = objs.double_dies;
      CopyToken(token, current_token);
      r.pattern.Insert(token);
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      first:= FALSE;
    END;
    adt.Deallocate(ne);
    IF was_double_dies THEN
      (* error *)
      Error(23, '');
      RETURN;
    END;
    scan.GetPrevLogicalString(mac.text);
--    objs.CurrentHeader.defined.Find(mac, e);
    objs.FindInDefined(objs.CurrentHeader, mac, e);
    IF (e = NIL) OR (e IS objs.Object) & (e(objs.Object).header # objs.CurrentHeader) THEN
      objs.CurrentHeader.objects.Insert(mac);
    END;
    InsertToDefined(r);
    IF msg.WasError THEN RETURN END;
    redefined_macro:= FALSE;
    objs.CurrentHeader.Macros.Insert(mac);
    objs.CurrentHeader.Replacements.Insert(r);
    PopCommentsList();
  ELSE
    (* Error *)
    Error(35, '');
    RETURN;
  END;
  adt.Deallocate(comments);
END Define;

(*------------------------------------------------------*)
PROCEDURE Undef();
VAR
  ne: adt.NamedElement;
  e: adt.Element;
BEGIN
  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
  CASE scan_token OF
    objs.ident, objs.int, objs.double ,objs.case ,objs.auto ,objs.asm
    ,objs.break ,objs.default ,objs.char ,objs.continue ,objs.const ,objs.do
    ,objs.float ,objs.enum ,objs.else ,objs.extern ,objs.goto ,objs.for
    ,objs.if ,objs.struct ,objs.short ,objs.register ,objs.long ,objs.return
    ,objs.static ,objs.sizeof ,objs.union ,objs.switch ,objs.typedef
    ,objs.unsigned ,objs.void ,objs.while ,objs.signed ,objs.volatile ,objs.interrupt
    ,objs.pascal ,objs.fortran ,objs.cdecl ,objs.syscall ,objs.far ,objs.near ,objs.huge:
      adt.NewNamedElement(ne, scan_text^);
      objs.CurrentHeader.Macros.Delete(ne);
      objs.CurrentHeader.defined.Delete(ne);
      objs.CurrentHeader.Replacements.Delete(ne);
--	GlobalDefinedTree.Delete(ne);
      objs.CurrentHeader.Names.Delete(ne);
--	GlobalNamesTree.Delete(ne);
      objs.CurrentHeader.objects.Find(ne, e);
      LOOP
	IF e = NIL THEN
	  EXIT;
	ELSIF (e IS objs.TypedObject)
		     &
	   (e(objs.TypedObject).obj = objs.constant)
		     OR
	      (e IS objs.Macro)
	THEN
	  objs.CurrentHeader.objects.DeleteCurrent();
	  e(adt.NamedElement).SetName('');
	  EXIT;
	END;
	objs.CurrentHeader.objects.FindAgain(e);
      END;
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      IF scan_token # objs.eol THEN
	(* error *)
	Error(20, '');
	RETURN;
      END;
  ELSE
    (* error *)
    Error(27, '');
    RETURN;
  END;
END Undef;

(*------------------------------------------------------*)
PROCEDURE SetModifier(l: adt.List; mod: INT);
VAR
  m: objs.Modifier;
BEGIN
  IF mod # objs.nonmodified THEN
    NEW(m);
    m.modifier:= mod;
    m.line:= line;
    m.pos:= pos;
    lstr.Assign(file_name^, m.file_name);
    l.Insert(m);
  END;
END SetModifier;

(*------------------------------------------------------*)
PROCEDURE RemoveExpression(VAR expr: objs.Expression);
VAR e: objs.Expression;
BEGIN
  IF expr = NIL THEN RETURN END;
  e:= expr;
  WITH
     e: objs.UnaryOperation DO
       RemoveExpression(e.first);
    |e: objs.BinaryOperation DO
       RemoveExpression(e.first);
       RemoveExpression(e.second);
    |e: objs.TernaryOperation DO
       RemoveExpression(e.first);
       RemoveExpression(e.second);
       RemoveExpression(e.third);
    |e: objs.Value DO
       lstr.Deallocate(e.value);
  END;
  objs.Deallocate(expr); expr:= NIL;
END RemoveExpression;

(*------------------------------------------------------*)
PROCEDURE IsUnaryOperation(op: INT): BOOLEAN;
BEGIN
  RETURN (op = objs.minus) OR
	 (op = objs.tilda) OR
	 (op = objs.plus)  OR
(*
	 (op = objs.ampersand) OR
*)
	 (op = objs.not);
END IsUnaryOperation;

(*------------------------------------------------------*)
PROCEDURE BinaryOperationPriority(op: INT): INT;
VAR
  i, j: INT;
BEGIN
  FOR i:= 0 TO min_binary_operation_priority DO
    FOR j:= 0 TO 3 DO
      IF binary_operations_priority[i,j] = op THEN
	RETURN i;
      END;
    END;
  END;
  RETURN -1;
END BinaryOperationPriority;

(*------------------------------------------------------*)
PROCEDURE InitBinaryOperationPriority();
VAR
  i, j: INT;
BEGIN
  FOR i:= 0 TO min_binary_operation_priority DO
    FOR j:= 0 TO 3 DO
      binary_operations_priority[i,j]:= MAX(INT);
    END;
  END;
  binary_operations_priority[0,0]:= objs.star;
  binary_operations_priority[0,1]:= objs.div;
  binary_operations_priority[0,2]:= objs.mod;

  binary_operations_priority[1,0]:= objs.plus;
  binary_operations_priority[1,1]:= objs.minus;

  binary_operations_priority[2,0]:= objs.b_lshift;
  binary_operations_priority[2,1]:= objs.b_rshift;

  binary_operations_priority[3,0]:= objs.more;
  binary_operations_priority[3,1]:= objs.less;
  binary_operations_priority[3,2]:= objs.more_equ;
  binary_operations_priority[3,3]:= objs.less_equ;

  binary_operations_priority[4,0]:= objs.equ;
  binary_operations_priority[4,1]:= objs.not_equ;

  binary_operations_priority[5,0]:= objs.ampersand;

  binary_operations_priority[6,0]:= objs.b_xor;

  binary_operations_priority[7,0]:= objs.b_or;

  binary_operations_priority[8,0]:= objs.l_and;

  binary_operations_priority[9,0]:= objs.l_or;
END InitBinaryOperationPriority;

(*------------------------------------------------------*)
PROCEDURE BinaryOperation(VAR expr: objs.Expression; priority: INT);
VAR
  bin_op: objs.BinaryOperation;
  un_op: objs.UnaryOperation;
  mod: INT;
  type_is_present: BOOLEAN;
  val: objs.Value;
  e: adt.Element;
  ne: adt.NamedElement;
  tmp: BOOLEAN;
(*
  str: lstr.String;
*)
BEGIN
  IF priority = -1 THEN
    CASE scan_token OF
       objs.char_const, objs.str_const, objs.int_const, objs.uint_const,
       objs.uhex_const, objs.hex_const, objs.real_const:
	 objs.NewValue(val); val.hex:= FALSE; val.value:= NIL;
	 val.type:= scan_token;
	 IF val.type = objs.hex_const THEN
	   val.type:= objs.int_const;
	   val.hex:= TRUE;
	 ELSIF val.type = objs.uhex_const THEN
	   val.type:= objs.uint_const;
	   val.hex:= TRUE;
	 END;
	 lstr.Assign(scan_text^, val.value);
	 expr:= val;
	 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      |objs.ident:
	 IF limited_constant_expression_on
			   &
		 (scan_text^ = 'defined')
	 THEN
	   objs.NewUnaryOperation(un_op); expr:= un_op;
	   un_op.operation:= objs.defined_operation;
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   tmp:= scan_token = objs.open_round_brack;
	   IF tmp THEN
	     GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   END;
	   IF scan_token = objs.ident THEN
	     objs.NewValue(val); val.hex:= FALSE; val.value:= NIL;
	     val.type:= objs.str_const;
	     lstr.Assign(scan_text^, val.value);
	     un_op.first:= val;
	     GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	     IF tmp THEN
	       IF scan_token = objs.close_round_brack THEN
		 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	       ELSE
		 IF ~objs.pd_define_is_parsed THEN
		   (* error *)
		   Error(5, "')'");
		 END;
		 RemoveExpression(expr);
		 RETURN;
	       END;
	     END;
	   ELSE
	     IF ~objs.pd_define_is_parsed THEN
	       (* error *)
	       Error(27, '');
	     END;
	     RemoveExpression(expr);
	     RETURN;
	   END;
	 ELSE
	   objs.NewValue(val); val.hex:= FALSE; val.value:= NIL; expr:= val;
	   adt.NewNamedElement(ne, scan_text^);
--	     objs.CurrentHeader.Names.Find(ne, e);
	   objs.FindInNames(objs.CurrentHeader, ne, e);
	   adt.Deallocate(ne);
	   IF e = NIL THEN
	     IF ~objs.pd_define_is_parsed THEN
	       val.type:= objs.int_const;
	       lstr.Assign('0', val.value);
	     ELSE
	       RemoveExpression(expr);
	       RETURN;
	     END;
	   ELSE
	     IF (e IS objs.TypedObject)
			   &
			   (
		(e(objs.TypedObject).obj = objs.constant)
			   OR
			   (
		~limited_constant_expression_on
			   &
		(e(objs.TypedObject).obj = objs.enum_const)
			   )
			   )
	     THEN
	       val.type:= scan_token;
	       val.tobj:= e(objs.TypedObject);
	       val.value:= NIL;
	     ELSE
	       IF ~objs.pd_define_is_parsed THEN
		 (* error *)
		 Error(37, '');
	       END;
	       RemoveExpression(expr);
	       RETURN;
	     END;
	   END;
	   GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	 END;
      |objs.open_round_brack:
	 GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	 objs.NewUnaryOperation(un_op); expr:= un_op;
	 un_op.operation:= objs.type_conv_operation;
	 Type(un_op.type, mod, type_is_present, tmp);
	 IF mod # objs.nonmodified THEN
	   IF ~objs.pd_define_is_parsed THEN
	     (* error *)
	     Error(38, '');
	   END;
	   IF (un_op.type.type # objs.t_synonym) &
	      (un_op.type.type # objs.t_struct)  &
	      (un_op.type.type # objs.t_union)	 &
	      (un_op.type.type # objs.t_enum)
	   THEN
	     objs.Deallocate(un_op.type);
	   END;
	   RemoveExpression(expr);
	   RETURN;
	 ELSIF type_is_present THEN
	   IF scan_token = objs.close_round_brack THEN
	     GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	     ConstExpr(un_op.first); IF msg.WasError THEN RETURN END;
	     IF ~was_sizeof & (un_op.first = NIL) THEN
	       IF ~objs.pd_define_is_parsed THEN
		 (* error *)
		 Error(21, '');
	       END;
	       IF (un_op.type.type # objs.t_synonym) &
		  (un_op.type.type # objs.t_struct)  &
		  (un_op.type.type # objs.t_union)   &
		  (un_op.type.type # objs.t_enum)
	       THEN
		 objs.Deallocate(un_op.type);
	       END;
	       RemoveExpression(expr);
	       RETURN;
	     END;
	   ELSE
	     IF ~objs.pd_define_is_parsed THEN
	       (* error *)
	       Error(5, "')'");
	     END;
	     IF (un_op.type.type # objs.t_synonym) &
		(un_op.type.type # objs.t_struct)  &
		(un_op.type.type # objs.t_union)   &
		(un_op.type.type # objs.t_enum)
	     THEN
	       objs.Deallocate(un_op.type);
	     END;
	     RemoveExpression(expr);
	     RETURN;
	   END;
	 ELSE
	   IF (un_op.type.type # objs.t_synonym) &
	      (un_op.type.type # objs.t_struct)  &
	      (un_op.type.type # objs.t_union)	 &
	      (un_op.type.type # objs.t_enum)
	   THEN
	     objs.Deallocate(un_op.type);
	   END;
	   RemoveExpression(expr);
	   ConstExpr(expr); IF msg.WasError THEN RETURN END;
	   IF scan_token = objs.close_round_brack THEN
	     GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	   ELSE
	     IF ~objs.pd_define_is_parsed THEN
	       (* error *)
	       Error(5, "')'");
	     END;
	     RemoveExpression(expr);
	     RETURN;
	   END;
	 END;
    ELSE
      IF IsUnaryOperation(scan_token) THEN
	objs.NewUnaryOperation(un_op); expr:= un_op;
	un_op.operation:= scan_token;
	GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	BinaryOperation(un_op.first, -1); IF msg.WasError THEN RETURN END;
	IF un_op.first = NIL THEN
	  IF ~objs.pd_define_is_parsed THEN
	    (* error *)
	    Error(21, '');
	  END;
	  RemoveExpression(expr);
	  RETURN;
	END;
      ELSIF ~limited_constant_expression_on
			  &
	      ( scan_token = objs.sizeof)
      THEN
	was_sizeof:= TRUE;
	GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	objs.NewUnaryOperation(un_op); expr:= un_op;
	un_op.operation:= objs.sizeof_operation;
	ConstExpr(un_op.first); IF msg.WasError THEN RETURN END;
	IF un_op.first = NIL THEN
	  IF ~objs.pd_define_is_parsed THEN
	    (* error *)
	    Error(21, '');
	  END;
	  RemoveExpression(expr);
	  RETURN;
	END;
	IF (un_op.first(objs.Operation).operation = objs.type_conv_operation)
			&
	   (un_op.first(objs.UnaryOperation).first = NIL )
	THEN
	  un_op.type:= un_op.first(objs.UnaryOperation).type;
	  RemoveExpression(un_op.first);
	ELSE
	  un_op.type:= NIL;
	END;
	was_sizeof:= FALSE;
      ELSE
	RemoveExpression(expr);
      END;
    END;
  ELSE
    objs.NewBinaryOperation(bin_op); expr:= bin_op;
    BinaryOperation(bin_op.first, priority-1); IF msg.WasError THEN RETURN END;
    IF bin_op.first # NIL THEN
      IF scan_token = objs.str_const THEN
	IF (bin_op.first IS objs.Value)
			 &
	   (bin_op.first(objs.Value).type = objs.str_const)
			 OR
	   (bin_op.first IS objs.BinaryOperation)
			 &
	   (bin_op.first(objs.BinaryOperation).operation = objs.strconcat_operation)
	THEN
	  bin_op.operation:= objs.strconcat_operation;
	  objs.NewValue(val); val.hex:= FALSE; val.value:= NIL;
	  val.type:= objs.str_const;
	  lstr.Assign(scan_text^, val.value);
	  bin_op.second:= val;
	  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	ELSE
	  IF ~objs.pd_define_is_parsed THEN
	    (* error *)
	    Error(39, '');
	  END;
	  RemoveExpression(expr);
	END;
      ELSIF BinaryOperationPriority(scan_token) = priority THEN
	bin_op.operation:= scan_token;
	GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	BinaryOperation(bin_op.second, priority-1); IF msg.WasError THEN RETURN END;
	IF bin_op.second = NIL THEN
	  IF ~objs.pd_define_is_parsed THEN
	    (* error *)
	    Error(21, '');
	  END;
	  RemoveExpression(expr);
	  RETURN;
	END;
	WHILE BinaryOperationPriority(scan_token) = priority DO
	  objs.NewBinaryOperation(bin_op);
	  bin_op.operation:= scan_token;
	  bin_op.first:= expr; expr:= bin_op;
	  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	  BinaryOperation(bin_op.second, priority-1); IF msg.WasError THEN RETURN END;
	  IF bin_op.second = NIL THEN
	    IF ~objs.pd_define_is_parsed THEN
	      (* error *)
	      Error(21, '');
	    END;
	    RemoveExpression(expr);
	    RETURN;
	  END;
	END;
      ELSE
	expr:= bin_op.first;
	objs.Deallocate(bin_op);
      END;
    ELSE
      IF ~objs.pd_define_is_parsed THEN
	(* error *)
	Error(21, '');
      END;
      RemoveExpression(expr);
      RETURN;
    END;
  END;
END BinaryOperation;

(*------------------------------------------------------*)
PROCEDURE ConstExpr(VAR expr: objs.Expression);
VAR
  ter_op: objs.TernaryOperation;
  res: objs.ConstantValue;
  line, pos: INT;
BEGIN
  line:= current_token.line; pos:= current_token.pos;
  objs.line:= current_token.line; objs.pos:= current_token.pos;
  was_sizeof:= FALSE;
  objs.NewTernaryOperation(ter_op); expr:= ter_op;
  BinaryOperation(ter_op.first, min_binary_operation_priority);
  IF msg.WasError THEN RETURN END;
  IF ter_op.first # NIL THEN
    IF scan_token = objs.question THEN
      ter_op.operation:= scan_token;
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      BinaryOperation(ter_op.second, min_binary_operation_priority); IF msg.WasError THEN RETURN END;
      IF ter_op.second = NIL THEN
	IF ~objs.pd_define_is_parsed THEN
	  (* error *)
	  Error(21, '');
	  RemoveExpression(expr);
	END;
	RETURN;
      END;
      IF scan_token = objs.two_points THEN
	GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	BinaryOperation(ter_op.third, min_binary_operation_priority); IF msg.WasError THEN RETURN END;
	IF ter_op.third = NIL THEN
	  IF ~objs.pd_define_is_parsed THEN
	    (* error *)
	    Error(21, '');
	  END;
	  RemoveExpression(expr);
	  RETURN;
	END;
      ELSE
	IF ~objs.pd_define_is_parsed THEN
	  (* error *)
	  Error(5, "':'");
	END;
	RemoveExpression(expr);
	RETURN;
      END;
    ELSE
      expr:= ter_op.first;
      objs.Deallocate(ter_op);
    END;
  ELSE
    IF ~objs.pd_define_is_parsed THEN
      (* error *)
      Error(21, '');
    END;
    RemoveExpression(expr);
    RETURN;
  END;
  IF expr # NIL THEN
    expr.ComputeExpression(res, limited_constant_expression_on);
    expr.line:= line; expr.pos:= pos;
    IF objs.pd_define_is_parsed & (res = NIL) THEN
      RemoveExpression(expr);
    END;
    objs.Deallocate(res);
  END;
END ConstExpr;

(*------------------------------------------------------*)
PROCEDURE get_modifiers(VAR dst_modifier: adt.List; VAR dst_type: objs.Type;
			    src_modifier: adt.List;	src_type: objs.Type);
VAR e: adt.Element;
BEGIN
  adt.NewList(dst_modifier);
  dst_type:= src_type;
  src_modifier.FindFirst(e);
  WHILE e # NIL DO
    dst_modifier.Insert(e);
    src_modifier.FindNext(e);
  END;
  WHILE (dst_type # NIL) & (dst_type.type = objs.t_synonym) DO
    dst_type.modifier.FindFirst(e);
    WHILE e # NIL DO
      dst_modifier.Insert(e);
      dst_type.modifier.FindNext(e);
    END;
    dst_type:= dst_type.base;
  END;
  (*
  IF (src_type # NIL) & (src_type.type = objs.t_synonym) THEN
    adt.NewList(dst_modifier);
    src_type.modifier.FindFirst(e);
    WHILE e # NIL DO
      dst_modifier.Insert(e);
      src_type.modifier.FindNext(e);
    END;
    src_modifier.FindFirst(e);
    WHILE e # NIL DO
      dst_modifier.Insert(e);
      src_modifier.FindNext(e);
    END;
    dst_type:= src_type.base;
    get_modifiers(modifier, type, dst_modifier, dst_type);
    dst_type:= type;
    dst_modifier:= modifier;
  ELSE
    dst_modifier:= src_modifier;
    dst_type:= src_type;
  END;
  *)
END get_modifiers;
(*------------------------------------------------------*)
PROCEDURE CheckDuplicateModifiers(mod_list: adt.List);
VAR
  new_list: adt.List;
  e, e1: adt.Element;
BEGIN
  adt.NewList(new_list);
  mod_list.FindFirst(e);
  WHILE e # NIL DO
    new_list.Find(e, e1);
    IF e1 = NIL THEN
      new_list.Insert(e);
    ELSE
      (* error *)
      line:= e(objs.Modifier).line;
      pos:= e(objs.Modifier).pos;
      Error(40, '');
      RETURN;
    END;
    mod_list.FindNext(e);
  END;
  adt.Deallocate(new_list);
END CheckDuplicateModifiers;

(*------------------------------------------------------*)
PROCEDURE CheckTypeModifiers(host_type: objs.Type);
VAR
  e: adt.Element;
  was_language_modifier, was_location_modifier: BOOLEAN;
  was_status_modifier, was_interrupt_modifier:	BOOLEAN;
  was_syscall_modifier: BOOLEAN;
  modifier: adt.List;
  type: objs.Type;
BEGIN
  IF host_type = NIL THEN RETURN END;
  was_language_modifier:= FALSE;
  was_location_modifier:= FALSE;
  was_status_modifier:= FALSE;
  was_interrupt_modifier:= FALSE;
  was_syscall_modifier:= FALSE;
  get_modifiers(modifier, type, host_type.modifier, host_type.base);
  IF ~modifier.IsEmpty() & (type = NIL) THEN
    modifier.FindFirst(e);
    (* error *)
    line:= e(objs.Modifier).line;
    pos:= e(objs.Modifier).pos;
    Error(38, '');
    RETURN;
  ELSIF type = NIL THEN
    adt.Deallocate(modifier);
    RETURN;
  END;
  CheckDuplicateModifiers(modifier);
  IF msg.WasError THEN RETURN END;
  modifier.FindFirst(e);
  WHILE e # NIL DO
    WITH e: objs.Modifier DO
      CASE e.modifier OF
	objs.pascal, objs.fortran, objs.cdecl:
	   IF ( was_syscall_modifier ) OR ( was_language_modifier ) THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   ELSE
	     was_language_modifier:= TRUE;
	   END;
	|objs.interrupt:
	   IF was_interrupt_modifier THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   ELSE
	     was_interrupt_modifier:= TRUE;
	   END;
	   IF type.type # objs.t_func THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   END;
	|objs.syscall:
	   IF ( was_syscall_modifier ) OR ( was_language_modifier ) THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   ELSE
	     was_syscall_modifier:= TRUE;
	   END;
	   IF type.type # objs.t_func THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   END;
	|objs.const, objs.volatile:
	   IF was_status_modifier THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   ELSE
	     was_status_modifier:= TRUE;
	   END;
	   IF (type.type = objs.t_func)
			 OR
	      (host_type.type = objs.t_func)
	   THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   END;
	|objs.far, objs.near, objs.huge:
	   IF was_location_modifier THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   ELSE
	     was_location_modifier:= TRUE;
	   END;
	   IF (type.type # objs.t_func)
			 &
	      (type.type # objs.t_ptr)
			 &
	      (type.type # objs.t_array)
	   THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   END;
      END;
    END;
    modifier.FindNext(e);
  END;
  adt.Deallocate(modifier);
  CheckTypeModifiers(type.base);
END CheckTypeModifiers;

(*------------------------------------------------------*)
PROCEDURE CheckModifiers(tobj: objs.TypedObject; struct_or_union: BOOLEAN);
VAR
  e: adt.Element;
  was_language_modifier, was_location_modifier: BOOLEAN;
  was_status_modifier, was_interrupt_modifier:	BOOLEAN;
  was_syscall_modifier: BOOLEAN;
  modifier: adt.List;
  type: objs.Type;
BEGIN
  IF tobj = NIL THEN
    RETURN;
  END;
  was_language_modifier:= FALSE;
  was_location_modifier:= FALSE;
  was_status_modifier:= FALSE;
  was_interrupt_modifier:= FALSE;
  was_syscall_modifier:= FALSE;
  get_modifiers(modifier, type, tobj.modifier, tobj.type);
  CheckDuplicateModifiers(modifier);
  IF msg.WasError THEN RETURN END;
  modifier.FindFirst(e);
  WHILE e # NIL DO
    WITH e: objs.Modifier DO
      CASE e.modifier OF
	|objs.pascal, objs.fortran, objs.cdecl:
	   IF struct_or_union THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   END;
	   IF ( was_syscall_modifier ) OR ( was_language_modifier ) THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   ELSE
	     was_language_modifier:= TRUE;
	   END;
	|objs.interrupt:
	   IF was_interrupt_modifier THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   ELSE
	     was_interrupt_modifier:= TRUE;
	   END;
	   IF type.type # objs.t_func THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   END;
	|objs.syscall:
	   IF ( was_syscall_modifier ) OR ( was_language_modifier ) THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   ELSE
	     was_syscall_modifier:= TRUE;
	   END;
	   IF type.type # objs.t_func THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   END;
	|objs.const, objs.volatile:
	(*
	   IF struct_or_union THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   END;
	*)
	   IF was_status_modifier THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   ELSE
	     was_status_modifier:= TRUE;
	   END;
	   IF type.type = objs.t_func THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   END;
	|objs.far, objs.near, objs.huge:
	   IF was_location_modifier THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   ELSE
	     was_location_modifier:= TRUE;
	   END;
	   IF (type.type # objs.t_func)
			 &
	      (type.type # objs.t_ptr)
			 &
	      (type.type # objs.t_array)
	   THEN
	     (* error *)
	     line:= e.line;
	     pos:= e.pos;
	     Error(38, '');
	     RETURN;
	   END;
      END;
    END;
    modifier.FindNext(e);
  END;
  adt.Deallocate(modifier);
  CheckTypeModifiers(type);
END CheckModifiers;

(*------------------------------------------------------*)
PROCEDURE Params(VAR to: adt.List);
VAR
  type, last_type: objs.Type;
  modifier: INT;
  mem_class: INT;
  tobj: objs.TypedObject;
  type_is_present: BOOLEAN;
  token: scan.Token;
  tmp: BOOLEAN;
BEGIN
  IF scan_token = objs.open_round_brack THEN
    current_token.boundary:= TRUE;
    PushCommentsList(to);
    REPEAT
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      IF (scan_token = objs.three_points) THEN
	objs.NewTypedObject(tobj, objs.arguments);
	tobj.SetName(''); tobj.line:= line; SetFileName(tobj.file, file_name);
	InsertToList(to,tobj);
	GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	IF scan_token # objs.close_round_brack THEN
	  (* error *)
	  Error(5, "')'");
	  RETURN;
	END;
      ELSIF (scan_token = objs.close_round_brack) THEN
	objs.NewTypedObject(tobj, objs.arguments);
	tobj.SetName(''); tobj.line:= line; SetFileName(tobj.file, file_name);
	InsertToList(to,tobj);
      ELSE
	Memory_Class(mem_class); IF msg.WasError THEN RETURN END;
	IF (mem_class # objs.register) & (mem_class # objs.undefined) THEN
	  (* error *)
	  Error(41, '');
	  RETURN;
	END;
	Type(type, modifier, type_is_present, tmp);

	WHILE (scan_token = objs.open_round_brack)
			  OR
	      (scan_token = objs.star)
			  OR
	      (scan_token = objs.volatile ) OR
	      (scan_token = objs.const	  ) OR
	      (scan_token = objs.pascal   ) OR
	      (scan_token = objs.fortran  ) OR
	      (scan_token = objs.interrupt) OR
	      (scan_token = objs.cdecl	  ) OR
	      (scan_token = objs.syscall  ) OR
	      (scan_token = objs.far	  ) OR
	      (scan_token = objs.near	  ) OR
	      (scan_token = objs.huge	  )
	DO
	  GetToken( FALSE ); IF msg.WasError THEN RETURN END;
	END;
	IF scan_token # objs.ident THEN
	  CopyToken(token, current_token);
	  lstr.Assign('', token.text);
	  token.token:= objs.ident;
	  token_list.InsertBeforeCurrent(token);
	END;

	WHILE scan_token # objs.empty_token DO
	  prev_token();
	END;
	next_token(); IF msg.WasError THEN RETURN END;

	Descriptor(tobj, last_type); IF msg.WasError THEN RETURN END;

	IF last_type # NIL THEN
	  IF IsType(type, objs.bt_void) & (last_type.type # objs.t_func) &
	     (last_type.type # objs.t_ptr)
	  THEN
	    (* error *)
	    Error(42, '');
	    RETURN;
	  END;
	  last_type.base:= type;
	  SetModifier(last_type.modifier, modifier);
	ELSE
	  IF IsType(type, objs.bt_void) THEN
	    IF (tobj.name^ = '') & (mem_class = objs.undefined) THEN
	       IF scan_token # objs.close_round_brack THEN
		 (* error *)
		 Error(5, "')'");
		 RETURN;
	       ELSE
		 objs.Deallocate(type); objs.Deallocate(tobj);
		 type:= NIL; tobj:= NIL;
	       END;
	    ELSE
	      (* error *)
	      Error(42, '');
	      RETURN;
	    END;
	  END;
	  IF tobj # NIL THEN
	    tobj.type:= type;
	    SetModifier(tobj.modifier, modifier);
	  END;
	END;

	IF tobj # NIL THEN
	  IF IsType(tobj.type, objs.t_func) THEN
	    objs.NewType(type);
	    type.type:= objs.t_ptr;
	    type.base:= tobj.type;
	    tobj.type:= type;
	  END;
	  CheckModifiers(tobj, FALSE); IF msg.WasError THEN RETURN END;
	  tobj.mem_class:= mem_class;
	  InsertToList(to,tobj);
	  IF msg.WasError THEN RETURN END;
	END;
      END;
    UNTIL scan_token # objs.comma;
    PopCommentsList();
    IF scan_token = objs.close_round_brack THEN
      GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      open_boundary();
    ELSE
      (* error *)
      Error(5, "')'");
      RETURN;
    END;
  ELSE
    (* error *)
    Error(5, "'('");
    RETURN;
  END;
END Params;

(*------------------------------------------------------*)
PROCEDURE CheckIntType(tobj: objs.TypedObject);
VAR type: objs.Type;
BEGIN
  type:= tobj.type;
  WHILE type.type = objs.t_synonym DO
    type:= type.base;
  END;
  IF (type.type # objs.bt_s_char  ) &
     (type.type # objs.bt_s_int   ) &
     (type.type # objs.bt_s_sh_int) &
     (type.type # objs.bt_s_l_int ) &
     (type.type # objs.bt_u_char  ) &
     (type.type # objs.bt_u_int   ) &
     (type.type # objs.bt_u_sh_int) &
     (type.type # objs.bt_u_l_int )
   THEN
     (* error *)
     Error(44, '');
     RETURN;
   END;
END CheckIntType;

(*------------------------------------------------------*)
PROCEDURE Descriptor( VAR tobj: objs.TypedObject; VAR type: objs.Type);
VAR
  tmp_type: objs.Type;
  first_dim: BOOLEAN;
  modifier: adt.List;
(******      Parse descriptor	   ******)

BEGIN
  type:= NIL;
  objs.NewTypedObject(tobj, objs.variable);
  modifier:= tobj.modifier;

(******     Find identifier and previous modifiers in declaration     ******)

  WHILE (scan_token = objs.open_round_brack)
		    OR
	(scan_token = objs.star)
		    OR
	(scan_token = objs.volatile ) OR
	(scan_token = objs.const    ) OR
	(scan_token = objs.pascal   ) OR
	(scan_token = objs.fortran  ) OR
	(scan_token = objs.interrupt) OR
	(scan_token = objs.cdecl    ) OR
	(scan_token = objs.syscall  ) OR
	(scan_token = objs.far	    ) OR
	(scan_token = objs.near     ) OR
	(scan_token = objs.huge     )
  DO
    GetToken( FALSE ); IF msg.WasError THEN RETURN END;
  END;



  IF scan_token # objs.ident THEN
    (* error *)
    Error(55, '');
    RETURN;
  ELSE
    tobj.SetName(scan_text^); tobj.line:= line; SetFileName(tobj.file, file_name);
    prev_token();
    LOOP
      CASE scan_token OF
	 objs.volatile, objs.const:
	|objs.pascal, objs.fortran, objs.interrupt, objs.syscall,
	 objs.cdecl, objs.far, objs.near, objs.huge:
	   current_token.parsed:= TRUE;
	   SetModifier(modifier, scan_token);
      ELSE
	EXIT;
      END;
      prev_token();
    END;
    WHILE scan_token # objs.ident DO
      next_token(); IF msg.WasError THEN RETURN END;
    END;
    current_token.parsed:= TRUE;
  END;

  LOOP(*LOOP 0*)
    GetToken(FALSE); IF msg.WasError THEN RETURN END;



(******     Parse right part of subdescriptor	  ******)

  (******     Is it function ?	  ******)

      IF scan_token = objs.open_round_brack THEN
	objs.NewType(tmp_type);
	tmp_type.type:= objs.t_func;
	Params(tmp_type.vars);
	IF msg.WasError THEN RETURN END;
	IF tobj.type = NIL THEN
	  tobj.type:= tmp_type;
	ELSE
	  type.base:= tmp_type;
	  modifier:= type.modifier;
	END;
	type:= tmp_type;

  (******     Is it array ?    ******)

      ELSIF scan_token = objs.open_square_brack THEN
	first_dim:= TRUE;
	REPEAT
	  objs.NewType(tmp_type);
	  tmp_type.type:= objs.t_array;
	  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	  IF ~first_dim OR (scan_token # objs.close_square_brack) THEN
	    ConstExpr(tmp_type.expr); IF msg.WasError THEN RETURN END;
	  END;
	  IF tmp_type.expr = NIL THEN
	    IF ~first_dim THEN
	      (* error *)
	      Error(21, '');
	      RETURN;
	    END;
	  ELSE
	    CheckIntExpr(tmp_type.expr, 1); IF msg.WasError THEN RETURN END;
	  END;
	  IF scan_token = objs.close_square_brack THEN
	    first_dim:= FALSE;
	    IF tobj.type = NIL THEN
	      tobj.type:= tmp_type;
	    ELSE
	      type.base:= tmp_type;
	      modifier:= type.modifier;
	    END;
	    type:= tmp_type;
	    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	  ELSE
	    (* error *)
	    Error(5, "']'");
	    RETURN;
	  END;
	UNTIL scan_token # objs.open_square_brack;
      END;

(******     Parse left part of subdescriptor	 ******)

    LOOP(*LOOP 1*)
      prev_token();

   (*******	 Is anybody home ?     ******)

      IF scan_token = objs.empty_token THEN
	next_token(); RETURN;
      END;

   (******	Is it modifier ?     ******)

      CASE(*CASE 1*) scan_token OF
	 objs.volatile,objs.const:
	   current_token.parsed:= TRUE;
	   IF type = NIL THEN
	     SetModifier(tobj.modifier, scan_token);
	   ELSE
	     SetModifier(type.modifier, scan_token);
	   END;
	|objs.pascal,objs.fortran,objs.cdecl:
	   IF type # NIL THEN
	     current_token.parsed:= TRUE;
	     IF type.type = objs.t_func THEN
	       SetModifier(modifier, scan_token);
	     ELSIF type.type = objs.t_ptr THEN
	       IF type.ptr_modifier.modifier = objs.nonmodified THEN
		 type.ptr_modifier.modifier:= scan_token;
	       ELSE
		 (* Error *)
		 Error(38, '');
		 RETURN;
	       END;
	     ELSE
	       SetModifier(type.modifier, scan_token);
	     END;
	   ELSE
	     (* Error *)
	     Error(38, '');
	     RETURN;
	   END;
	|objs.interrupt, objs.syscall:
	   IF type # NIL THEN
	     current_token.parsed:= TRUE;
	     IF type.type = objs.t_func THEN
	       SetModifier(modifier, scan_token);
	     ELSE
	       SetModifier(type.modifier, scan_token);
	     END;
	   ELSE
	     (* Error *)
	     Error(38, '');
	     RETURN;
	   END;
	|objs.far,objs.near,objs.huge:
	   IF type # NIL THEN
	     current_token.parsed:= TRUE;
	     SetModifier(modifier, scan_token);
	   ELSE
	     (* Error *)
	     Error(38, '');
	     RETURN;
	   END;

    (******	Is it pointer ?     ******)

	|objs.star:
	   objs.NewType(tmp_type);
	   tmp_type.type:= objs.t_ptr;
	   IF tobj.type = NIL THEN
	     tobj.type:= tmp_type;
	   ELSE
	     type.base:= tmp_type;
	     modifier:= type.modifier;
	   END;
	   type:= tmp_type;
	   current_token.parsed:= TRUE;

    (******	May be, it is '('     *******)

	|objs.open_round_brack:
	   current_token.parsed:= TRUE;
	   next_token(); IF msg.WasError THEN RETURN END;
	   IF scan_token = objs.close_round_brack THEN
	     current_token.parsed:= TRUE;
	   ELSE
	     (* error *)
	     Error(5, "')'");
	     RETURN;
	   END;
	   EXIT(*LOOP 1*);

    (******	Else ERROR     ******)

      ELSE(*CASE 1*)
	(* error *)
	Error(5, "'*', '('");
	RETURN;
      END(*CASE 1*);
    END(*LOOP 1*);
  END(*LOOP 0*);
END Descriptor;


(*------------------------------------------------------*)
(*+*)PROCEDURE Declarations(struct_or_union: BOOLEAN; to: adt.List);
VAR
  syn, type, last_type: objs.Type;
  tobj: objs.TypedObject;
  modifier: INT;
  mem_class: INT;
  typedef: BOOLEAN;
  typedef_from_pretext: BOOLEAN;
  expr: objs.Expression;
  brackets_balance: INT;
  tmp: BOOLEAN;
  tag_was_parsed: BOOLEAN;
  e: adt.Element;
BEGIN
  tobj:= NIL; last_type:= NIL;
  syn:= NIL; type:= NIL;
  mem_class:= objs.undefined;
  LOOP(*LOOP0*)
    typedef:= FALSE;
    typedef_from_pretext:= FALSE;
    IF(*IF0*) (scan_token = objs.eof)
			 OR
		  struct_or_union
			  &
	 (scan_token = objs.close_figure_brack)
    THEN(*IF0*)
      RETURN;
    ELSE(*IF0*)

    (******    Parse declarations   *****)

      IF ~struct_or_union THEN
	IF(scan_token = objs.typedef) THEN

	  (******    If 'typedef' then type synonym declaration     ******)

	  typedef:= TRUE;
	  typedef_from_pretext:= current_token.pretoken;
	  GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	ELSE

	  (******     else variable or function declaration,	      ******]
	  [******     that's why parse memory class specification     ******)

	  Memory_Class(mem_class); IF msg.WasError THEN RETURN END;
	  IF (mem_class = objs.auto) OR (mem_class = objs.register) THEN
	    (* error *)
	    Error(41, '');
	    RETURN;
	  END;
	END;
      END;

      (******	   Parse type specification	 ******)

      Type(type, modifier, tmp, tag_was_parsed); IF msg.WasError THEN RETURN END;

      IF(*IF3*) tag_was_parsed & (scan_token = objs.point_comma) THEN
	GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      ELSIF (*IF3*) ~tag_was_parsed & (scan_token = objs.point_comma) &
	(type.type = objs.t_enum)
      THEN
	type.SetName(objs.arbitrary_name);
	objs.CurrentHeader.objects.Insert(type);
	GetToken( TRUE ); IF msg.WasError THEN RETURN END;
      ELSE(*IF3*)
	(******      Parse declaration	    ******)

	LOOP(*LOOP1*)

	  IF scan_token = objs.two_points THEN

	    (**************   Parse unnamed bit field	************)

	    objs.NewTypedObject(tobj, objs.bit_field);
	    tobj.SetName(''); tobj.line:= line; SetFileName(tobj.file, file_name);
	    last_type:= NIL;
	  ELSE

	    (******	 Parse descriptor      ******)

	    Descriptor(tobj, last_type); IF msg.WasError THEN RETURN END;
	  END;


	  (******      Checkings and insertings      ******)

	  IF last_type # NIL THEN
	    IF IsType(type, objs.bt_void) & (last_type.type # objs.t_func) &
	       (last_type.type # objs.t_ptr)
	    THEN
	      (* error *)
	      Error(42, '');
	      RETURN;
	    END;
	    last_type.base:= type;
	    SetModifier(last_type.modifier, modifier);
	  ELSE
	    IF IsType(type, objs.bt_void) & ~typedef THEN
	      (* error *)
	      Error(42, '');
	      RETURN;
	    END;
	    tobj.type:= type;
	    SetModifier(tobj.modifier, modifier);
	  END;


	  CheckModifiers(tobj, struct_or_union); IF msg.WasError THEN RETURN END;


	  IF struct_or_union & (scan_token = objs.two_points) THEN
	    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	    ConstExpr(expr); IF msg.WasError THEN RETURN END;
	    IF expr = NIL THEN
	      (* error *)
	      Error(21, '');
	      RETURN;
	    ELSE
	      CheckIntExpr(expr, 0); IF msg.WasError THEN RETURN END;
	    END;
	    CheckIntType(tobj); IF msg.WasError THEN RETURN END;
	    tobj.expr:= expr;
	    tobj.obj:= objs.bit_field;
	  END;


	  IF typedef THEN
	    objs.NewType(syn);
	    syn.type:= objs.t_synonym;
	    syn.SetName(tobj.name^); syn.line:= tobj.line; SetFileName(syn.file, tobj.file);
	    syn.base:= tobj.type;
	    merge_lists(syn.modifier, tobj.modifier, FALSE);
	    syn.created_by_back_end:= typedef_from_pretext;
	    objs.CurrentHeader.objects.Insert(syn);
	    InsertToNames(syn);
	    objs.Deallocate(tobj); tobj:= NIL;
	    IF msg.WasError THEN RETURN END;
	  ELSE
	    tobj.mem_class:= mem_class;
	    CheckFullDefinition(tobj); IF msg.WasError THEN RETURN END;
	    IF struct_or_union & (tobj.type.type = objs.t_func) THEN
	      (* error *)
	      Error(45, '');
	      RETURN;
	    END;

	    IF struct_or_union THEN
	      InsertToList(to,tobj);
	      IF msg.WasError THEN RETURN END;
	    ELSE
--		objs.CurrentHeader.Names.Find(tobj, e);
	      objs.FindInNames(objs.CurrentHeader, tobj, e);
	      IF (e = NIL) OR (tobj.header # e(objs.Object).header) THEN
		objs.CurrentHeader.objects.Insert(tobj);
	      END;
	      InsertToNames(tobj, TRUE);
	      IF msg.WasError THEN RETURN END;
	    END;
	  END;


	  (**** Skip possible initializer ****)

	  IF (scan_token = objs.assign)
			 &
		  ~struct_or_union
			 &
		     ~typedef
			 &
	      (mem_class # objs.extern)
	  THEN
	    tobj.mem_class:= objs.static;

	    GetToken( TRUE ); IF msg.WasError THEN RETURN END;


	    IF scan_token = objs.open_figure_brack THEN

	      (* Skip possible complex initalizer *)

	      brackets_balance:= 0;
	      LOOP
		IF scan_token = objs.open_figure_brack THEN
		  INC(brackets_balance);
		ELSIF (scan_token = objs.close_figure_brack)
				  &
		      (brackets_balance > 0)
		THEN
		  DEC(brackets_balance);
		ELSIF brackets_balance = 0 THEN
		  EXIT;
		ELSIF (scan_token = objs.point_comma)
				  OR
		      (scan_token = objs.eof)
		THEN
		  (* error *)
		  Error(5, "'}'");
		  RETURN;
		END;
		GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	      END;
	    ELSE

	      (* Skip all tokens till ';' or ',' *)

	      WHILE (scan_token # objs.point_comma)
				&
		    (scan_token # objs.comma)
	      DO
		GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	      END;
	    END;

	  END;

	  (**** Parse next descriptor or declaration ****)

	  IF scan_token = objs.point_comma THEN
	    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	    EXIT(*LOOP1*);
	  ELSIF scan_token = objs.comma THEN
	    GetToken( TRUE ); IF msg.WasError THEN RETURN END;
	  ELSE
	    (* error *)
	    Error(5, "',' ';'");
	    RETURN;
	  END;
	END(*LOOP1*);
      END(*IF3*);
    END(*IF0*);
  END(*LOOP0*);
END Declarations;

(*------------------------------------------------------*)
PROCEDURE Init();
BEGIN
  redefined_macro:= FALSE;
  objs.pd_define_is_parsed:= FALSE;
  current_list_for_comments:= NIL;
  adt.Deallocate(comments_lists_stack);
  adt.NewStack(comments_lists_stack);
--  adt.Deallocate(GlobalNamesTree);
--  adt.NewTree(GlobalNamesTree);
--  adt.Deallocate(GlobalTagsTree);
--  adt.NewTree(GlobalTagsTree);
--  adt.Deallocate(GlobalDefinedTree);
--  adt.NewTree(GlobalDefinedTree);
  adt.Deallocate(pretext_list);
  adt.NewList(pretext_list);
  merge_lists(pretext_list, prepretext_list, FALSE);
  adt.Deallocate(headers_stack);
  adt.NewStack(headers_stack);
  adt.Deallocate(merge_headers_tree);
  adt.NewTree(merge_headers_tree);
  merge_trees(merge_headers_tree, project_merge_headers_tree);
  scan.Init();
END Init;

(*------------------------------------------------------*)
PROCEDURE Parse * (name-: ARRAY OF CHAR; cstdlib: BOOLEAN);
VAR
  e: adt.Element;
  ne: adt.NamedElement;
  was_push: BOOLEAN;
BEGIN
  (*
  CheckHeader(name);
  *)
  NEW(current_token);
  adt.NewList(token_list);
  include_balance:= 0;
  if_balance:= 0;
  adt.NewStack(if_stack);
  end_balance:= 0;
  C_extensions:= TRUE;
  config_on:= FALSE;
  project_on:= FALSE;
  scan.prj_cfg_on:= config_on OR project_on;

  IF header_balance = 0 THEN
    lstr.Assign(name, file_name);
    line:= 1;
    pos:= 1;
    Init();
  END;

  INC(header_balance);
  adt.NewNamedElement(ne, name);
  objs.Headers.Find(ne, e); adt.Deallocate(ne);
  IF e = NIL THEN
    IF Open(name, TRUE) THEN
      objs.NewHeader(name, objs.CurrentHeader);
      IF header_balance = 1 THEN
	HeadHeader:= objs.CurrentHeader;
        IF cfg.IncludingTree THEN msg.OpenLogFile(name, cfg.TreeExtension^, HeadHeader.stat) END;
        IF cfg.StrippingDirs THEN msg.OpenDirFile(name, cfg.DirExtension^) END;
      END;
      objs.Headers.Insert(objs.CurrentHeader);
      IF cfg.HeadersMerging THEN
	adt.NewStack(merged_headers_stack);
	NewMergedHeader(name);
      ELSE
	adt.NewNamedElement(ne, name);
	merged_headers_stack.Push(ne);
      END;
      was_push:= ~cfg.HeadersMerging;
      PutPreText(name);
      IF config_project # NIL THEN
	merge_headers(objs.CurrentHeader, config_project, FALSE, TRUE);
      END;
      PushCommentsList(objs.CurrentHeader.objects);
      GetToken( TRUE ); IF msg.WasError THEN DEC(header_balance); RETURN END;
      Declarations(FALSE, objs.CurrentHeader.objects); IF msg.WasError THEN DEC(header_balance); RETURN END;
      PopCommentsList();
      IF was_push THEN merged_headers_stack.Pop(e) END;
      (*
      CheckTagsDefinition();
      *)
      InsertSynonyms();
      CheckModifiedObjects();
      IF ~msg.WasError THEN
	CheckParameters();
      END;
      objs.CurrentHeader.preheader:= is_it_preheader;
      objs.CurrentHeader.cstdlib:= cstdlib;
      objs.CurrentHeader.parsed:= ~msg.WasError;
      objs.CurrentHeader.stat.lines:= scan.last_header_total_lines;
    END;
  ELSE
    objs.CurrentHeader:= e(objs.Header);
    IF cfg.IncludingTree THEN msg.WriteLogFile('', FALSE) END;
  END;
  DEC(header_balance);
  IF (header_balance = 0) & cfg.IncludingTree THEN msg.CloseLogFile() END;
  IF (header_balance = 0) & cfg.StrippingDirs THEN msg.CloseDirFile() END;
END Parse;

(*------------------------------------------------------*)
PROCEDURE IsIdent(token: INT): BOOLEAN;
BEGIN
  RETURN (token = objs.ident) OR
         (token >= objs.CKeyWords_base) &
         (token <= objs.CKeyWords_max);
END IsIdent;

(*------------------------------------------------------*)
PROCEDURE CfgVar();
VAR
  left, right: lstr.String;
  err: INT;

BEGIN
  left:= NIL; right:= NIL;
  IF scan_token = objs.minus THEN
    GetToken(TRUE); IF msg.WasError THEN RETURN END;
    IF IsIdent(scan_token) THEN
      Strings.Capitalize(scan_text^);
      IF scan_text^ = "CTYPE" THEN
	GetToken(TRUE); IF msg.WasError THEN RETURN END;
	IF scan_token = objs.assign THEN
	  GetToken(TRUE); IF msg.WasError THEN RETURN END;
          lstr.Assign('$ctype', left);
	  WHILE (scan_token # objs.assign) & (scan_token # objs.eof) DO
	    lstr.Append('_', left);
	    lstr.Append(scan_text^, left);
	    GetToken(TRUE); IF msg.WasError THEN RETURN END;
	  END;
	  IF scan_token = objs.assign THEN
	    GetToken(TRUE); IF msg.WasError THEN RETURN END;
	  ELSE
	    (* error *)
	    Error(24, '');
	    RETURN;
	  END;
	  IF scan_token = objs.int_const THEN
	    lstr.Assign(scan_text^, right);
	    err:= cfg.GetInfo(left^, right^);
	    IF err >= 0 THEN
	      (* error *)
              Error(err, left^);
	      RETURN;
	    END;
	    GetToken(TRUE); IF msg.WasError THEN RETURN END;
	  ELSE
	    (* error *)
	    Error(51, '');
	    RETURN;
	  END;
	  IF err = -2 THEN (* if was definition C base type *)
	    IF scan_token = objs.comma THEN (* -ctype = id = SIZE, QALIDENT *)
	      GetToken(TRUE); IF msg.WasError THEN RETURN END;
              IF IsIdent(scan_token) THEN
		lstr.Assign(scan_text^, right);
		GetToken(TRUE); IF msg.WasError THEN RETURN END;
		IF scan_token = objs.point THEN
		  GetToken(TRUE); IF msg.WasError THEN RETURN END;
                  IF IsIdent(scan_token) THEN
		    lstr.Append('.', right);
		    lstr.Append(scan_text^, right);
		    GetToken(TRUE); IF msg.WasError THEN RETURN END;
		  ELSE
		    (* error *)
		    Error(27, '');
		    RETURN;
		  END;
		END;
		err:= cfg.GetInfo(left^, right^);
		IF err >= 0 THEN
		  (* error *)
                  Error(err, left^);
		  RETURN;
		END;
	      ELSE
		(* error *)
		Error(27, '');
		RETURN;
	      END;
	    ELSE
	      (* error *)
	      Error(5, "','");
	      RETURN;
	    END;
	  ELSE
	    (* error *)
	    Error(33, '');
	    RETURN;
	  END;
	ELSE
	  (* error *)
	  Error(5, "'='");
	  RETURN;
	END;
      ELSIF scan_text^ = "M2TYPE" THEN
	GetToken(TRUE); IF msg.WasError THEN RETURN END;
	lstr.Assign("*", left);
        IF scan_token = objs.assign THEN
          GetToken(TRUE); IF msg.WasError THEN RETURN END;
        ELSE
          (* error *)
	  Error(5, "'='");
	  RETURN;
        END;
        IF IsIdent(scan_token) THEN
	  lstr.Append(scan_text^, left);
	  GetToken(TRUE); IF msg.WasError THEN RETURN END;
	  IF scan_token = objs.point THEN
            lstr.Append('.', left);
	    GetToken(TRUE); IF msg.WasError THEN RETURN END;
            IF IsIdent(scan_token) THEN
	      lstr.Append(scan_text^, left);
	      GetToken(TRUE); IF msg.WasError THEN RETURN END;
	    ELSE
	      (* error *)
	      Error(27, '');
	      RETURN;
	    END;
	  END;
	ELSE
	  (* error *)
	  Error(27, '');
	  RETURN;
	END;
	IF scan_token = objs.assign THEN
	  GetToken(TRUE); IF msg.WasError THEN RETURN END;
	  IF scan_token = objs.int_const THEN
	    lstr.Assign(scan_text^, right);
            err:= cfg.GetInfo(left^, right^, FALSE);
	    IF err >= 0 THEN
	      (* error *)
              Error(err, left);
	      RETURN;
	    END;
	    GetToken(TRUE); IF msg.WasError THEN RETURN END;
	    IF scan_token = objs.comma THEN
	      GetToken(TRUE); IF msg.WasError THEN RETURN END;
              Strings.Capitalize(scan_text^);
	      IF (scan_token = objs.ident) &
		 ((scan_text^ = 'BOOL') OR
		  (scan_text^ = 'CHAR') OR
		  (scan_text^ = 'REAL') OR
		  (scan_text^ = 'SET')  OR
                  (scan_text^ = 'SIGNED')  OR
                  (scan_text^ = 'UNSIGNED'))
	      THEN
		left[0]:= '+';
		lstr.Assign(scan_text^, right);
                err:= cfg.GetInfo(left^, right^, FALSE);
		IF err >= 0 THEN
		  (* error *)
                  Error(err, left^);
		  RETURN;
		END;
		GetToken(TRUE); IF msg.WasError THEN RETURN END;
	      ELSE
		(* error *)
                Error(70, '');
		RETURN;
	      END;
	    ELSE
	      (* error *)
	      Error(5, "','");
	      RETURN
	    END;
	  ELSE
	    (* error *)
	    Error(51, '');
	    RETURN;
	  END;
	ELSE
	  (* error *)
	  Error(5, "'='");
	  RETURN;
	END;
      ELSE
	lstr.Assign(scan_text^, left);
	GetToken(TRUE); IF msg.WasError THEN RETURN END;
	IF scan_token = objs.assign THEN
	  GetToken(TRUE); IF msg.WasError THEN RETURN END;
          IF IsIdent(scan_token)           OR
             (scan_token = objs.str_const) OR
             (scan_token = objs.int_const)
          THEN
	    lstr.Assign(scan_text^, right);
            err:= cfg.GetInfo(left^, right^);
	    IF err >= 0 THEN
	      (* error *)
              Error(err, left^);
	      RETURN;
	    END;
	    GetToken(TRUE); IF msg.WasError THEN RETURN END;
	  ELSE
	    (* error *)
	    Error(46, '');
	    RETURN;
	  END;
	ELSIF scan_token = objs.minus THEN
	  GetToken(TRUE); IF msg.WasError THEN RETURN END;
	  lstr.Assign("Off", right);
	  err:= cfg.GetInfo(left^, right^);
	  IF err >= 0 THEN
	    (* error *)
            Error(err, left^);
	    RETURN;
	  END;
	ELSIF scan_token = objs.plus THEN
	  GetToken(TRUE); IF msg.WasError THEN RETURN END;
	  lstr.Assign("On", right);
	  err:= cfg.GetInfo(left^, right^);
	  IF err >= 0 THEN
	    (* error *)
            Error(err, left^);
	    RETURN;
	  END;
	ELSE
	  (* error *)
	  Error(5, "'=', '-', '+'");
	  RETURN;
	END;
      END;
    ELSE
      (* error *)
      Error(27, '');
      RETURN;
    END;
    lstr.Deallocate(left);
    lstr.Deallocate(right);
  ELSE
    (* error *)
    Error(5, "'-'");
    RETURN;
  END;
END CfgVar;

(*------------------------------------------------------*)
PROCEDURE ErrorInParseProjectOrParseConfig();
BEGIN
  objs.CurrentHeader.stat.ShowHeader();
  objs.CurrentHeader.stat.ShowFooter();
  HALT;
END ErrorInParseProjectOrParseConfig;

(*------------------------------------------------------*)
PROCEDURE ParseProject * (name-: ARRAY OF CHAR): adt.List;
VAR
  wholename, fname, ext, path: lstr.String;
  project_list: adt.List;
  module: objs.Module;
BEGIN
  file.SplitName(name, path, fname, ext);
  IF ext^ = '' THEN
    file.CreateName(path^, fname^, cfg.PrjExtension^, wholename);
  ELSE
    lstr.Assign(name, wholename);
  END;

  adt.NewList(project_list);
  adt.NewStack(if_stack);
  NEW(current_token);
  adt.NewList(token_list);
  if_balance:= 0;
  end_balance:= 0;
  C_extensions:= TRUE;
  config_on:= FALSE;
  project_on:= TRUE;
  scan.prj_cfg_on:= config_on OR project_on;
  lstr.Assign(name, file_name);
  line:= 1;
  pos:= 1;
  Init();

  objs.NewHeader(name,objs.CurrentHeader);
  HeadHeader:= objs.CurrentHeader;
  IF Open(wholename^, TRUE) THEN
    PushCommentsList(objs.CurrentHeader.objects);
    GetToken( TRUE ); IF msg.WasError THEN ErrorInParseProjectOrParseConfig() END;
    WHILE scan_token # objs.eof DO
      IF scan_token = objs.pd_module THEN
	GetToken( TRUE ); IF msg.WasError THEN ErrorInParseProjectOrParseConfig() END;
	NEW(module);
	module.cstdlib:= scan_token = objs.std_header_name;
	module.SetName(scan_text^);
	project_list.Insert(module);
	GetToken( TRUE ); IF msg.WasError THEN ErrorInParseProjectOrParseConfig() END;
	IF scan_token = objs.eol THEN
	  GetToken( TRUE ); IF msg.WasError THEN ErrorInParseProjectOrParseConfig() END;
	ELSE
	  (* error *)
	  Error(20, '');
	  scan_token:= objs.eof;
	END;
      ELSE
	CfgVar();
	IF msg.WasError THEN
	  io.printf('');
	  ErrorInParseProjectOrParseConfig();
	END;
      END;
    END;
    PopCommentsList();
    IF project_list.IsEmpty() THEN
      (* error *)
      Error(64, '');
      ErrorInParseProjectOrParseConfig();
    END;
    merge_headers(config_project, objs.CurrentHeader, FALSE, TRUE);
    merge_lists(prepretext_list, pretext_list, FALSE);
    merge_trees(project_merge_headers_tree, merge_headers_tree);
    RETURN project_list;
  ELSE
    ErrorInParseProjectOrParseConfig();
  END;
END ParseProject;


(*------------------------------------------------------*)
PROCEDURE ParseConfig * (name-: ARRAY OF CHAR);
BEGIN
  NEW(current_token);
  adt.NewList(token_list);
  adt.NewStack(if_stack);
  if_balance:= 0;
  end_balance:= 0;
  C_extensions:= TRUE;
  config_on:= TRUE;
  project_on:= FALSE;
  scan.prj_cfg_on:= config_on OR project_on;
  lstr.Assign(name, file_name);
  line:= 1;
  pos:= 1;
  adt.NewList(prepretext_list);
  objs.NewHeader('config_project', config_project);
  Init();

  objs.NewHeader(name,objs.CurrentHeader);
  IF Open(name, FALSE) THEN
    PushCommentsList(objs.CurrentHeader.objects);
    GetToken( TRUE ); IF msg.WasError THEN ErrorInParseProjectOrParseConfig() END;
    WHILE scan_token # objs.eof DO
      CfgVar(); IF msg.WasError THEN ErrorInParseProjectOrParseConfig() END;
    END;
    PopCommentsList();
    merge_headers(config_project, objs.CurrentHeader, FALSE, TRUE);
    merge_lists(prepretext_list, pretext_list, FALSE);
  END;
END ParseConfig;


(*------------------------------------------------------*)
BEGIN
  adt.NewList(empty_token_list);
  header_balance:= 0;
  include_balance:= 0;
  objs.NewComment( declaration_without_definition, -1,
		  'Declaration without definition' );
  adt.NewStack(merged_headers_stack);
  adt.NewTree(merge_headers_tree);
  adt.NewTree(project_merge_headers_tree);
  adt.NewTree(FileNameTree);
  InitBinaryOperationPriority();

  lstr.Assign('', emptyString);
  NEW(emptyToken);
  emptyToken.token:= objs.empty_token;
  emptyToken.text:= emptyString;

END H2DParse.

(*
io.printf('7, token = %d, %s\n',scan_token,scan_text^);
*)
