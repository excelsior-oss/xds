(* H2Def Configuration module *** Al 25-Oct-95 *)

MODULE H2DCfg;

(*  ----------------------------------------------------------------------  *)

IMPORT
  io:= Printf,
  SYSTEM,
  adt,
  str := Strings,
  lstr:= LongStrs,
  file:= H2DFile,
  objs:= H2DObjs,
  msg := H2DMsg,
  ConvTypes,
  WholeStr;

(*  ----------------------------------------------------------------------  *)

TYPE
  INT = SYSTEM.INT32;

(*  ----------------------------------------------------------------------  *)

CONST
  start_const_index = 0;
  Const * = 0 + start_const_index;
  Enum	* = 1 + start_const_index;
  Mixed * = 2 + start_const_index;
  last_const_index * = 3 + start_const_index;

  default_macro_prefix	    = '_';
  default_output_extensions = 'def';

  ON  = 'ON';
  OFF = 'OFF';

(*  ----------------------------------------------------------------------  *)
VAR

(* ---	GLOBALS  --- *)

  OutputExtension * : lstr.String;
  ModExtension	  * : lstr.String;
  PrjExtension	  * : lstr.String;
  DirExtension	  * : lstr.String;
  DLSString       * : lstr.String;
  HeaderExtension * : lstr.String;
  TreeExtension   * : lstr.String;
  Retranslate	  * : BOOLEAN;
  Progress	  * : BOOLEAN;
  CStdLib	  * : BOOLEAN;
  CPPComment	  * : BOOLEAN;
  CommentPosition * : INT;
  HeadersMerging  * : BOOLEAN;
  HeadInserting   * : BOOLEAN;
  MaxStrLen	  * : INT;
  EnumeratedType  * : INT;
  LongFileName	  * : BOOLEAN;
  DefinitionFilePrefix * : lstr.String;
  MacrosFilePrefix     * : lstr.String;
  BackEnd	       * : INT;
  GenerateMacrosFile   * : BOOLEAN;
  IncludingTree        * : BOOLEAN;
  StrippingDirs        * : BOOLEAN;
  ConstantsToVariables * : BOOLEAN;


(* ---	LOCALS	--- *)

TYPE
  CBaseType	= POINTER TO CBaseTypeDesc;
  CBaseTypeDesc = RECORD (adt.NamedElementDesc)
    size: INT;
  END;

VAR
  BTStr: ARRAY 16 OF CBaseType;
  i: INTEGER;

(* ----------------------  LOCAL PROCEDURES  ------------------------------ *)

PROCEDURE GetInfo * (le-, ri-: ARRAY OF CHAR; cap:= TRUE: BOOLEAN): INT;
(* Returns error value. If < 0 then all right. *)
VAR
  r: ConvTypes.ConvResults;
  err: INT;
  left, right: ARRAY 100 OF CHAR;
  left1: lstr.String;
  m2Type: objs.M2Type;
  e: adt.Element;
  ne: adt.NamedElement;

  (* -------------------------------------------- *)
  PROCEDURE SetBTStr( str-: ARRAY OF CHAR; index: INT );
  VAR cBaseType: CBaseType;
  BEGIN
    IF BTStr[index] = NIL THEN
      NEW(cBaseType);
      BTStr[index]:= cBaseType;
    ELSE
      cBaseType:= BTStr[index];
    END;
    IF (str[0] >= '0') & (str[0] <= '9') THEN
      WholeStr.StrToInt(right, cBaseType.size, r);
      IF r # ConvTypes.strAllRight THEN err:= 62; RETURN END;
    ELSE
      cBaseType.SetName(str);
    END;
    err:= -2;
  END SetBTStr;

  (* -------------------------------------------- *)
  PROCEDURE OnOff(s: ARRAY OF CHAR): BOOLEAN;
  BEGIN
    str.Capitalize(s);
    IF str.Equal(s, ON) THEN
      RETURN TRUE;
    ELSIF ~str.Equal(s, OFF) THEN
      (* error *)
      err:= 62;
    END;
    RETURN FALSE;
  END OnOff;

  (* -------------------------------------------- *)


BEGIN
  str.Assign(le, left); str.Assign(ri, right);
  IF cap THEN str.Capitalize(left) END;
  err:= -1;
  IF left[0] = '*' THEN
    lstr.Assign(left, left1);
    lstr.Delete(left1^, 0, 1);
    adt.NewNamedElement(ne, left1^);
    objs.M2Types.Find(ne, e);
    IF e = NIL THEN
      objs.NewM2Type(m2Type, left1^);
      objs.M2Types.Insert(m2Type);
    ELSE
      m2Type:= e(objs.M2Type);
    END;

    WholeStr.StrToInt(right, m2Type.type_size, r);
    IF r # ConvTypes.strAllRight THEN err:= 62 END;
  ELSIF left[0] = '+' THEN
    lstr.Assign(left, left1);
    lstr.Delete(left1^, 0, 1);
    adt.NewNamedElement(ne, left1^);
    objs.M2Types.Find(ne, e);
    IF e = NIL THEN
      objs.NewM2Type(m2Type, left1^);
      objs.M2Types.Insert(m2Type);
    ELSE
      m2Type:= e(objs.M2Type);
    END;
    IF	  right = 'BOOL'     THEN m2Type.type_kind:= objs.tk_bool;
    ELSIF right = 'CHAR'     THEN m2Type.type_kind:= objs.tk_char;
    ELSIF right = 'REAL'     THEN m2Type.type_kind:= objs.tk_real;
    ELSIF right = 'SET'      THEN m2Type.type_kind:= objs.tk_set;
    ELSIF right = 'SIGNED'   THEN m2Type.type_kind:= objs.tk_signed;
    ELSIF right = 'UNSIGNED' THEN m2Type.type_kind:= objs.tk_unsigned;
    END;
  ELSIF str.Equal(left, "DEFEXT")         THEN lstr.Assign(right, OutputExtension);
  ELSIF str.Equal(left, "HEADEXT")        THEN lstr.Assign(right, HeaderExtension);
  ELSIF str.Equal(left, "MODEXT")         THEN lstr.Assign(right, ModExtension);
  ELSIF str.Equal(left, "PRJEXT")         THEN lstr.Assign(right, PrjExtension);
  ELSIF str.Equal(left, "DIREXT")         THEN lstr.Assign(right, DirExtension);
  ELSIF str.Equal(left, "TREEEXT")        THEN lstr.Assign(right, TreeExtension);
  ELSIF str.Equal(left, "DEFPFX")         THEN lstr.Assign(right, DefinitionFilePrefix);
  ELSIF str.Equal(left, "MACPFX")         THEN lstr.Assign(right, MacrosFilePrefix);
  ELSIF str.Equal(left, "DLSSTRING")      THEN lstr.Assign(right, DLSString);

  ELSIF str.Equal(left, "GENMACRO")       THEN GenerateMacrosFile  := OnOff(right);
  ELSIF str.Equal(left, "CHANGEDEF")      THEN Retranslate         := OnOff(right);
  ELSIF str.Equal(left, "PROGRESS")       THEN Progress            := OnOff(right); msg.progress:= Progress;
  ELSIF str.Equal(left, "CSTDLIB")        THEN CStdLib             := OnOff(right);
  ELSIF str.Equal(left, "CPPCOMMENTS")    THEN CPPComment          := OnOff(right);
  ELSIF str.Equal(left, "MERGEALL")       THEN HeadersMerging      := OnOff(right);
  ELSIF str.Equal(left, "GENSEP")         THEN HeadInserting       := OnOff(right);
  ELSIF str.Equal(left, "GENLONGNAMES")   THEN LongFileName        := OnOff(right);
  ELSIF str.Equal(left, "GENTREE")        THEN IncludingTree       := OnOff(right);
  ELSIF str.Equal(left, "GENROVARS")      THEN ConstantsToVariables:= OnOff(right);
  ELSIF str.Equal(left, "GENDIRS")        THEN StrippingDirs       := OnOff(right);

  ELSIF str.Equal(left, "$CTYPE_SIGNED_CHAR")        THEN SetBTStr(right, 0 );
  ELSIF str.Equal(left, "$CTYPE_SIGNED_INT")         THEN SetBTStr(right, 1 );
  ELSIF str.Equal(left, "$CTYPE_SIGNED_SHORT_INT")   THEN SetBTStr(right, 2 );
  ELSIF str.Equal(left, "$CTYPE_SIGNED_LONG_INT")    THEN SetBTStr(right, 3 );
  ELSIF str.Equal(left, "$CTYPE_UNSIGNED_CHAR")      THEN SetBTStr(right, 4 );
  ELSIF str.Equal(left, "$CTYPE_UNSIGNED_INT")       THEN SetBTStr(right, 5 );
  ELSIF str.Equal(left, "$CTYPE_UNSIGNED_SHORT_INT") THEN SetBTStr(right, 6 );
  ELSIF str.Equal(left, "$CTYPE_UNSIGNED_LONG_INT")  THEN SetBTStr(right, 7 );
  ELSIF str.Equal(left, "$CTYPE_FLOAT")              THEN SetBTStr(right, 8 );
  ELSIF str.Equal(left, "$CTYPE_DOUBLE")             THEN SetBTStr(right, 9 );
  ELSIF str.Equal(left, "$CTYPE_LONG_FLOAT")         THEN SetBTStr(right, 10);
  ELSIF str.Equal(left, "$CTYPE_LONG_DOUBLE")        THEN SetBTStr(right, 11);

  ELSIF str.Equal(left, "GENENUM")   THEN
    str.Capitalize(right);
    IF	  str.Equal(right, "ENUM")           THEN EnumeratedType:= Enum;
    ELSIF str.Equal(right, "AUTO")           THEN EnumeratedType:= Mixed;
    ELSIF str.Equal(right, "CONST")          THEN EnumeratedType:= Const;
    ELSE					err:= 62;
    END;

  ELSIF str.Equal(left, "BACKEND")          THEN
    str.Capitalize(right);
    IF	   str.Equal(right, "C")              THEN BackEnd:= objs.c_code;
    ELSIF  str.Equal(right, "NATIVE")         THEN BackEnd:= objs.native;
    ELSIF  str.Equal(right, "COMMON")         THEN BackEnd:= objs.common;
    ELSE				      err:= 62;
    END;

  ELSIF str.Equal(left, "GENWIDTH")   THEN
    WholeStr.StrToInt(right, MaxStrLen, r);
    IF r # ConvTypes.strAllRight THEN
      MaxStrLen:= -1;
      err:= 62;
    END;

  ELSIF str.Equal(left, "COMMENTPOS") THEN
    WholeStr.StrToInt(right, CommentPosition, r);
    IF r # ConvTypes.strAllRight THEN
      CommentPosition:= -1;
      err:= 62;
    END;
(* it isn't needed science add SYSTEM.ADDRESS
  ELSIF str.Equal(left, "void")               THEN lstr.Assign(right, BTStr[12])
*)
  ELSE
    err:= 47;
  END;
  RETURN err;
END GetInfo;

(*  ----------------------------------------------------------------------  *)

PROCEDURE Status*;
END Status;

(* ------------------------  GLOBAL PROCEDURES	--------------------------- *)

PROCEDURE GetBaseType* (CType: INT; VAR MType: lstr.String);
(* Returns Modular type corresponding with C base type,
   otherwise returns NIL.*)
VAR i: INT;
BEGIN
  i := CType - objs.BaseTypes_base;
  IF BTStr[i] = NIL THEN
    MType := NIL;
  ELSE
    lstr.Assign(BTStr[i].name^, MType);
  END;
END GetBaseType;

PROCEDURE GetBaseTypeSize* (CType: INT): INT;
(* Returns size of C base type,
   otherwise returns -1.*)
VAR i: INT;
BEGIN
  i := CType - objs.BaseTypes_base;
  IF BTStr[i] = NIL THEN
    RETURN -1;
  ELSE
    RETURN BTStr[i].size;
  END;
END GetBaseTypeSize;


(*  ----------------------------------------------------------------------  *)

PROCEDURE SetBaseType* (CType: INT; MType-: ARRAY OF CHAR; size:=-1: INT);
(* Sets Modular type corresponding with C base type *)
VAR cBaseType: CBaseType;
BEGIN
  IF BTStr[CType - objs.BaseTypes_base] = NIL THEN
    NEW(cBaseType);
    BTStr[CType - objs.BaseTypes_base]:= cBaseType;
  ELSE
    cBaseType:= BTStr[CType - objs.BaseTypes_base];
  END;
  cBaseType.SetName(MType);
  cBaseType.size:= size;
END SetBaseType;

(*  ----------------------------------------------------------------------  *)
PROCEDURE IsValidVariant * (type: INT; variant_size: INT): BOOLEAN;
VAR i: INT;
BEGIN
  i:= type - objs.BaseTypes_base;
  RETURN (BTStr[i] # NIL) & (BTStr[i].size = variant_size);
END IsValidVariant;

(*  ----------------------------------------------------------------------  *)

PROCEDURE IsRepresentativeChar * (asciiCode: INT): BOOLEAN;
(* Return TRUE if symbol corresponding 'asciiCode' can be shown
   on display. The sequence of this represent char defined defoult
   or by user *)
BEGIN
  IF (asciiCode >= 32) & (asciiCode <= 128) THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END IsRepresentativeChar;

(*  ----------------------------------------------------------------------  *)
PROCEDURE PrintOptions * ();
VAR e: adt.Element;
  PROCEDURE print(name: ARRAY OF CHAR; opt: BOOLEAN);
  BEGIN
    IF opt THEN
      io.printf("%s +\n", name);
    ELSE
      io.printf("%s -\n", name);
    END;
  END print;
BEGIN
  io.printf("\n");
  print("GENMACRO    ", GenerateMacrosFile  );
  print("CHANGEDEF   ", Retranslate         );
  print("PROGRESS    ", Progress            );
  print("CSTDLIB     ", CStdLib             );
  print("CPPCOMMENTS ", CPPComment          );
  print("MERGEALL    ", HeadersMerging      );
  print("GENSEP      ", HeadInserting       );
  print("GENLONGNAMES", LongFileName        );
  print("GENTREE     ", IncludingTree       );
  print("GENROVARS   ", ConstantsToVariables);
  print("GENDIRS     ", StrippingDirs       );

  io.printf("\n");

  io.printf("DEFEXT     = %s\n", OutputExtension^);
  io.printf("HEADEXT    = %s\n", HeaderExtension^);
  io.printf("MODEXT     = %s\n", ModExtension^);
  io.printf("PRJEXT     = %s\n", PrjExtension^);
  io.printf("DIREXT     = %s\n", DirExtension^);
  io.printf("TREEEXT    = %s\n", TreeExtension^);
  io.printf("DEFPFX     = %s\n", DefinitionFilePrefix^);
  io.printf("MACPFX     = %s\n", MacrosFilePrefix^);

  CASE EnumeratedType OF
     Enum : io.printf("GENENUM    = ENUM\n");
    |Mixed: io.printf("GENENUM    = AUTO\n");
    |Const: io.printf("GENENUM    = CONST\n");
  END;

  CASE BackEnd OF
     objs.c_code: io.printf("BACKEND    = C\n");
    |objs.native: io.printf("BACKEND    = NATIVE\n");
    |objs.common: io.printf("BACKEND    = COMMON\n");
  END;

  io.printf("GENWIDTH    = %d\n", MaxStrLen);
  io.printf("COMMENTPOS  = %d\n", CommentPosition);
  io.printf("DLSString   = %d\n", DLSString);

  io.printf("\n");


  IF BTStr[0]  # NIL THEN io.printf("signed char        = %2d, %s\n", BTStr[0].size,  BTStr[0].name^ ) END;
  IF BTStr[1]  # NIL THEN io.printf("signed int         = %2d, %s\n", BTStr[1].size,  BTStr[1].name^ )  END;
  IF BTStr[2]  # NIL THEN io.printf("signed short int   = %2d, %s\n", BTStr[2].size,  BTStr[2].name^ )  END;
  IF BTStr[3]  # NIL THEN io.printf("signed long int    = %2d, %s\n", BTStr[3].size,  BTStr[3].name^ )  END;
  IF BTStr[4]  # NIL THEN io.printf("unsigned char      = %2d, %s\n", BTStr[4].size,  BTStr[4].name^ )  END;
  IF BTStr[5]  # NIL THEN io.printf("unsigned int       = %2d, %s\n", BTStr[5].size,  BTStr[5].name^ )  END;
  IF BTStr[6]  # NIL THEN io.printf("unsigned short int = %2d, %s\n", BTStr[6].size,  BTStr[6].name^ )  END;
  IF BTStr[7]  # NIL THEN io.printf("unsigned long int  = %2d, %s\n", BTStr[7].size,  BTStr[7].name^ )  END;
  IF BTStr[8]  # NIL THEN io.printf("float              = %2d, %s\n", BTStr[8].size,  BTStr[8].name^ )  END;
  IF BTStr[9]  # NIL THEN io.printf("double             = %2d, %s\n", BTStr[9].size,  BTStr[9].name^ )  END;
  IF BTStr[10] # NIL THEN io.printf("long float         = %2d, %s\n", BTStr[10].size, BTStr[10].name^ ) END;
  IF BTStr[11] # NIL THEN io.printf("long double        = %2d, %s\n", BTStr[11].size, BTStr[11].name^ ) END;

  io.printf("\n");

  objs.M2Types.FindFirst(e);
  WHILE e # NIL DO
    io.printf("%-18s = %2d, ", e(objs.M2Type).name^, e(objs.M2Type).type_size);
    CASE e(objs.M2Type).type_kind OF
       objs.tk_bool    : io.printf("BOOL\n");
      |objs.tk_char    : io.printf("CHAR\n");
      |objs.tk_real    : io.printf("REAL\n");
      |objs.tk_set     : io.printf("SET\n");
      |objs.tk_signed  : io.printf("SIGNED\n");
      |objs.tk_unsigned: io.printf("UNSIGNED\n");
    END;
    objs.M2Types.FindNext(e);
  END;

END PrintOptions;

(*  ----------------------------------------------------------------------  *)

BEGIN
  MaxStrLen:= -1;
  CommentPosition:= -1;
  OutputExtension:= NIL; lstr.Assign(default_output_extensions, OutputExtension);
  HeaderExtension:= NIL; lstr.Assign('h', HeaderExtension);
  PrjExtension := NIL; lstr.Assign('prj', PrjExtension);
  ModExtension := NIL; lstr.Assign('mod', ModExtension);
  DirExtension := NIL; lstr.Assign('dir', DirExtension);
  TreeExtension:= NIL; lstr.Assign('dir', TreeExtension);
  Retranslate	 := FALSE;
  Progress	 := FALSE;
  CStdLib	 := FALSE;
  CPPComment	 := TRUE;
  HeadersMerging := FALSE;
  HeadInserting  := FALSE;
  LongFileName	 := FALSE;
  EnumeratedType := Const;
  DefinitionFilePrefix:= NIL;  lstr.Assign('', DefinitionFilePrefix);
  MacrosFilePrefix    := NIL;  lstr.Assign('', MacrosFilePrefix);
  DLSString           := NIL;  lstr.Assign('C', DLSString);
  BackEnd := objs.common;
  GenerateMacrosFile  := FALSE;
  ConstantsToVariables:= FALSE;
  IncludingTree       := FALSE;
  StrippingDirs       := FALSE;

  FOR i := 0 TO LEN(BTStr) - 1 DO BTStr[i] := NIL END;
END H2DCfg.

