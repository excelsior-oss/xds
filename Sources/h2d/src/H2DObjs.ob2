MODULE H2DObjs;
(*----------------------------------------------------*)
IMPORT adt,
       lstr:= LongStrs,
       sys:= SYSTEM,
       WholeStr,
       ConvTypes,
       LongStr,
       io:= Printf,
       msg := H2DMsg,
       file:= H2DFile;


TYPE
  INT = sys.INT32;
  CARD = sys.CARD32;

(*----------------------------------------------------*)
CONST
  arbitrary_name * = 'Unnamed!!!';

  last_number * = 7200;  (* last number of int constant *)

  (* C base types   *)
  BaseTypes_base * = 0;
  bt_s_char   * = 0  + BaseTypes_base;
  bt_s_int    * = 1  + BaseTypes_base;
  bt_s_sh_int * = 2  + BaseTypes_base;
  bt_s_l_int  * = 3  + BaseTypes_base;
  bt_u_char   * = 4  + BaseTypes_base;
  bt_u_int    * = 5  + BaseTypes_base;
  bt_u_sh_int * = 6  + BaseTypes_base;
  bt_u_l_int  * = 7  + BaseTypes_base;
  bt_float    * = 8  + BaseTypes_base;
  bt_double   * = 9  + BaseTypes_base;
  bt_l_float  * = 10 + BaseTypes_base;
  bt_l_double * = 11 + BaseTypes_base;
  bt_void     * = 12 + BaseTypes_base;

  base_types * = {bt_s_char, bt_s_int,	bt_s_sh_int, bt_s_l_int,
		  bt_u_char, bt_u_int,	bt_u_sh_int, bt_u_l_int,
		  bt_float,  bt_double, bt_l_float,  bt_l_double,
                  bt_void};

  (*  Other C types  *)
  OtherTypes_base * = 1000;
  t_ptr      * = 0 + OtherTypes_base;
  t_struct   * = 1 + OtherTypes_base;
  t_array    * = 2 + OtherTypes_base;
  t_synonym  * = 3 + OtherTypes_base;
  t_func     * = 4 + OtherTypes_base;
  t_union    * = 5 + OtherTypes_base;
  t_enum     * = 6 + OtherTypes_base;

  (* Modula 2 types *)
  bt_unknown       * =  0;
  bt_INTEGER       * =  1;
  bt_SHORTINT      * =  2;
  bt_LONGINT       * =  3;
  bt_CARDINAL      * =  4;
  bt_SHORTCARD     * =  5;
  bt_LONGCARD      * =  6;
  bt_CHAR          * =  7;
  bt_BITSET        * =  8;
  bt_BOOLEAN       * =  9;
  bt_REAL          * = 10;
  bt_LONGREAL      * = 11;
  bt_BOOL8         * = 12;
  bt_BOOL16        * = 13;
  bt_BOOL32        * = 14;
  bt_INT8          * = 15;
  bt_INT16         * = 16;
  bt_INT32         * = 17;
  bt_CARD8         * = 18;
  bt_CARD16        * = 19;
  bt_CARD32        * = 20;
  bt_SET16         * = 21;
  bt_SET32         * = 22;
  bt_xds_int       * = 23;
  bt_xds_unsigned  * = 24;
  bt_BYTE          * = 25;

  (* groups of Modula2 types *)
  base_bool * = {bt_BOOLEAN, bt_BOOL8, bt_BOOL16, bt_BOOL32};
  base_set  * = {bt_BITSET, bt_SET16, bt_SET32};
  base_int  * = {bt_INTEGER, bt_SHORTINT, bt_LONGINT, bt_xds_int,
                 bt_INT8, bt_INT16, bt_INT32};
  base_card * = {bt_CARDINAL, bt_SHORTCARD, bt_LONGCARD, bt_xds_unsigned,
                 bt_CARD8, bt_CARD16, bt_CARD32};
  base_real * = {bt_REAL, bt_LONGREAL};
  base_m2   * = {bt_INTEGER, bt_SHORTINT, bt_LONGINT, bt_CARDINAL,
                 bt_SHORTCARD, bt_LONGCARD, bt_CHAR, bt_BITSET, bt_BOOLEAN,
                 bt_REAL, bt_LONGREAL, bt_BYTE, bt_BOOL8, bt_BOOL16, bt_BOOL32,
                 bt_INT8, bt_INT16, bt_INT32, bt_CARD8, bt_CARD16, bt_CARD32,
                 bt_SET16, bt_SET32, bt_xds_int, bt_xds_unsigned};

  (*  Typed objects  *)
  TypedObjects_base * = 2000;
  variable	      * = 0 + TypedObjects_base;
  constant	      * = 1 + TypedObjects_base;  (* number *)
  bit_field	      * = 2 + TypedObjects_base;
  arguments	      * = 3 + TypedObjects_base;  (* unlimited number of
						     arguments *)
  enum_const	      * = 4 + TypedObjects_base;
  synonym	      * = 5 + TypedObjects_base;


  (*  Modifiers  *)
  Modifiers_base * = 3000;

(* since Thu  12-14-95
  m_volatile	* = 0 + Modifiers_base;
  m_const	* = 1 + Modifiers_base;
  m_pascal	* = 2 + Modifiers_base;
  m_fortran	* = 3 + Modifiers_base;
  m_interrupt	* = 4 + Modifiers_base;
  m_cdecl	* = 5 + Modifiers_base;
  m_far 	* = 6 + Modifiers_base;
  m_near	* = 7 + Modifiers_base;
  m_huge	* = 8 + Modifiers_base;
  m_syscall	* = 9 + Modifiers_base;
*)
  nonmodified	* = 9 + Modifiers_base;

  (*   Other constant	*)
  OtherConstant_base = 4000;
  undefined	       * =  0 + OtherConstant_base;

  (* How change XDS option *)
  XDS_option_base * = 7000;
  on	     * =  0 + XDS_option_base;
  off	     * =  1 + XDS_option_base;
  no_change  * =  2 + XDS_option_base;

  (* Type of backend *)
  Type_backend_base * = 7100;
  common * = 0 + Type_backend_base;
  native * = 1 + Type_backend_base;
  c_code * = 2 + Type_backend_base;

  (* Type kinds *)
  Type_kinds_base * = 7200;
  tk_bool     * = 0 + Type_kinds_base;
  tk_char     * = 1 + Type_kinds_base;
  tk_real     * = 2 + Type_kinds_base;
  tk_set      * = 3 + Type_kinds_base;
  tk_signed   * = 4 + Type_kinds_base;
  tk_unsigned * = 5 + Type_kinds_base;

  (* operation *)
  Operation_base  * = 4500;
  sizeof_operation     * =  1 + Operation_base;
  type_conv_operation  * =  2 + Operation_base;
  defined_operation    * =  3 + Operation_base;
  strconcat_operation  * =  4 + Operation_base;

  Max_name_len * = 40;
  Max_file_name_len * = 128;
  max_num_modifier  * = 4;


  (*  C key words  *)
  CKeyWords_base * = 5000;
  int	     * = 0  + CKeyWords_base;
  double     * = 1  + CKeyWords_base;
  case	     * = 2  + CKeyWords_base;
  auto	     * = 3  + CKeyWords_base;
  asm	     * = 4  + CKeyWords_base;
  break      * = 5  + CKeyWords_base;
  default    * = 6  + CKeyWords_base;
  char	     * = 7  + CKeyWords_base;
  continue   * = 8  + CKeyWords_base;
  const      * = 9  + CKeyWords_base;
  do	     * = 10 + CKeyWords_base;
  float      * = 11 + CKeyWords_base;
  enum	     * = 12 + CKeyWords_base;
  else	     * = 13 + CKeyWords_base;
  extern     * = 14 + CKeyWords_base;
  goto	     * = 15 + CKeyWords_base;
  for	     * = 16 + CKeyWords_base;
  if	     * = 17 + CKeyWords_base;
  struct     * = 18 + CKeyWords_base;
  short      * = 19 + CKeyWords_base;
  register   * = 20 + CKeyWords_base;
  long	     * = 21 + CKeyWords_base;
  return     * = 22 + CKeyWords_base;
  static     * = 23 + CKeyWords_base;
  sizeof     * = 24 + CKeyWords_base;
  union      * = 25 + CKeyWords_base;
  switch     * = 26 + CKeyWords_base;
  typedef    * = 27 + CKeyWords_base;
  unsigned   * = 28 + CKeyWords_base;
  void	     * = 29 + CKeyWords_base;
  while      * = 30 + CKeyWords_base;
  signed     * = 31 + CKeyWords_base;
  volatile   * = 32 + CKeyWords_base;
  interrupt  * = 33 + CKeyWords_base;
  pascal     * = 34 + CKeyWords_base;
  fortran    * = 35 + CKeyWords_base;
  cdecl      * = 36 + CKeyWords_base;
  syscall    * = 37 + CKeyWords_base;
  far	     * = 38 + CKeyWords_base;
  near	     * = 39 + CKeyWords_base;
  huge       * = 40 + CKeyWords_base;
  CKeyWords_max * = huge;

  (*  Other tokens  *)
  OtherTokens_base * = 6000;
  two_points	      (*  :		 *)   * = 0  + OtherTokens_base;
  ident 	      (*		 *)   * = 1  + OtherTokens_base;
  plus		      (*  +		 *)   * = 2  + OtherTokens_base;
  minus 	      (*  -		 *)   * = 3  + OtherTokens_base;
  three_points	      (*  ...		 *)   * = 4  + OtherTokens_base;
  comma 	      (*  ,		 *)   * = 5  + OtherTokens_base;
  open_round_brack    (*  (		 *)   * = 6  + OtherTokens_base;
  close_round_brack   (*  )		 *)   * = 7  + OtherTokens_base;
  open_figure_brack   (*  {		 *)   * = 8  + OtherTokens_base;
  close_figure_brack  (*  }		 *)   * = 9  + OtherTokens_base;
  point_comma	      (*  ;		 *)   * = 10 + OtherTokens_base;
  question	      (*  ?		 *)   * = 11 + OtherTokens_base;
  not		      (*  !		 *)   * = 12 + OtherTokens_base;
  assign	      (*  =		 *)   * = 13 + OtherTokens_base;
  more		      (*  >		 *)   * = 14 + OtherTokens_base;
  less		      (*  <		 *)   * = 15 + OtherTokens_base;
  more_equ	      (*  >=		 *)   * = 16 + OtherTokens_base;
  less_equ	      (*  <=		 *)   * = 17 + OtherTokens_base;
  not_equ	      (*  !=		 *)   * = 18 + OtherTokens_base;
  star		      (*  *		 *)   * = 19 + OtherTokens_base;
  div		      (*  /		 *)   * = 20 + OtherTokens_base;
  mod		      (*  %		 *)   * = 21 + OtherTokens_base;
  ampersand	      (*  &		 *)   * = 22 + OtherTokens_base;
  b_or		      (*  |		 *)   * = 23 + OtherTokens_base;
  b_xor 	      (*  ^		 *)   * = 24 + OtherTokens_base;
  b_lshift	      (*  <<		 *)   * = 25 + OtherTokens_base;
  b_rshift	      (*  >>		 *)   * = 26 + OtherTokens_base;
  tilda 	      (*  ~		 *)   * = 27 + OtherTokens_base;
  l_or		      (*  ||		 *)   * = 28 + OtherTokens_base;
  l_and 	      (*  &&		 *)   * = 29 + OtherTokens_base;
  char_const	      (*		 *)   * = 30 + OtherTokens_base;
  str_const	      (*		 *)   * = 31 + OtherTokens_base;
  inc		      (*  ++		 *)   * = 32 + OtherTokens_base;
  dec		      (*  --		 *)   * = 33 + OtherTokens_base;
  plus_assign	      (*  +=		 *)   * = 34 + OtherTokens_base;
  minus_assign	      (*  -=		 *)   * = 35 + OtherTokens_base;
  mul_assign	      (*  *=		 *)   * = 36 + OtherTokens_base;
  div_assign	      (*  /=		 *)   * = 37 + OtherTokens_base;
  mod_assign	      (*  %=		 *)   * = 38 + OtherTokens_base;
  b_or_assign	      (*  |=		 *)   * = 39 + OtherTokens_base;
  b_xor_assign	      (*  ^=		 *)   * = 40 + OtherTokens_base;
  b_lshift_assign     (*  <<=		 *)   * = 41 + OtherTokens_base;
  b_rshift_assign     (*  >>=		 *)   * = 42 + OtherTokens_base;
  b_and_assign	      (*  &=		 *)   * = 43 + OtherTokens_base;
  point 	      (*  .		 *)   * = 44 + OtherTokens_base;
  open_square_brack   (*  [		 *)   * = 45 + OtherTokens_base;
  close_square_brack  (*  ]		 *)   * = 46 + OtherTokens_base;
  back_slash	      (*  \		 *)   * = 47 + OtherTokens_base;
  dies		      (*  #		 *)   * = 48 + OtherTokens_base;
  double_dies	      (*  ##		 *)   * = 49 + OtherTokens_base;
  pd_define	      (*  #define	 *)   * = 50 + OtherTokens_base;
  pd_if 	      (*  #if		 *)   * = 51 + OtherTokens_base;
  pd_endif	      (*  #endif	 *)   * = 52 + OtherTokens_base;
  pd_elif	      (*  #elif 	 *)   * = 53 + OtherTokens_base;
  pd_else	      (*  #else 	 *)   * = 54 + OtherTokens_base;
  pd_ifdef	      (*  #ifdef	 *)   * = 55 + OtherTokens_base;
  pd_ifndef	      (*  #ifndef	 *)   * = 56 + OtherTokens_base;
  pd_undef	      (*  #undef	 *)   * = 57 + OtherTokens_base;
  pd_line	      (*  #line 	 *)   * = 58 + OtherTokens_base;
  pd_error	      (*  #error	 *)   * = 59 + OtherTokens_base;
  pd_include	      (*  #include	 *)   * = 60 + OtherTokens_base;
  pd_pragma	      (*  #pragma	 *)   * = 61 + OtherTokens_base;
  pd_unknown	      (*		 *)   * = 62 + OtherTokens_base;
  eol		      (*  end of line	 *)   * = 63 + OtherTokens_base;
  eof		      (*  end of file	 *)   * = 64 + OtherTokens_base;
  comment	      (*  comment	 *)   * = 65 + OtherTokens_base;
  pointer	      (*  ->		 *)   * = 66 + OtherTokens_base;
  blank 	      (*		 *)   * = 67 + OtherTokens_base;
  std_header_name     (*  <header_name>  *)   * = 68 + OtherTokens_base;
  nonstd_header_name  (*  "header_name"  *)   * = 69 + OtherTokens_base;
  macro_name	      (*  #define macro  *)   * = 70 + OtherTokens_base;
  pmacro_name	      (* #define macro() *)   * = 71 + OtherTokens_base;

  int_const	      (* integer  const  *)   * = 72 + OtherTokens_base;
  uint_const	      (* unsigned const  *)   * = 73 + OtherTokens_base;
  hex_const	      (* hex int  const  *)   * = 74 + OtherTokens_base;
  uhex_const	      (* uhex int const  *)   * = 75 + OtherTokens_base;
  real_const	      (* real	  const  *)   * = 76 + OtherTokens_base;

  scan_error	      (*		 *)   * = 77 + OtherTokens_base;

  equ		      (* ==		 *)   * = 78 + OtherTokens_base;

  empty_token	      (*		 *)   * = 79 + OtherTokens_base;

  pd_merge	      (* #merge 	 *)   * = 80 + OtherTokens_base;
  pd_module	      (* #module	 *)   * = 81 + OtherTokens_base;
  pd_end	      (* #end		 *)   * = 82 + OtherTokens_base;
  pd_bitset	      (* #bitset	 *)   * = 83 + OtherTokens_base;
  pd_parameters       (* #parameters     *)   * = 84 + OtherTokens_base;
  pd_footer	      (* #footer	 *)   * = 85 + OtherTokens_base;
  pd_name	      (* #name		 *)   * = 86 + OtherTokens_base;
  pd_variant	      (* #variant	 *)   * = 87 + OtherTokens_base;
  pd_header	      (* #header	 *)   * = 88 + OtherTokens_base;

  reference	      (* ^		 *)   * = b_xor;

  unary_minus	      (* needed only for     *)
		      (* CheckOperationOrder *) = 89 + OtherTokens_base;

(*----------------------------------------------------*)
TYPE
  Header * = POINTER TO HeaderDesc;
  HeaderDesc * = RECORD ( adt.NamedElementDesc )
    preheader	      * : BOOLEAN;
    Replacements      * : adt.Tree;   (* Replacement *)
    Tags	      * : adt.Tree;   (* Type *)
    Names	      * : adt.Tree;   (* Type, TypedObject *)
    Macros	      * : adt.Tree;   (* Macro *)
    Synonyms	      * : adt.Tree;   (* TypedObject *)
    Parameters        * : adt.List;   (* H2DParse.Designator *)
    ModifiedObjects   * : adt.List;   (* H2DParse.Designator *)
    defined	      * : adt.Tree;
    objects	      * : adt.List;   (* TypedObject, Type, Macro, Comment *)
    mergedheaders     * : adt.List;   (* merged headers  *)
    parsed	      * : BOOLEAN;
    generated	      * : BOOLEAN;
    includes	      * : adt.List;   (* Header *)
    cstdlib	      * : BOOLEAN;
    stat	      * : msg.Statistics;
  END;

  M2Type     * = POINTER TO M2TypeDesc;
  M2TypeDesc * = RECORD (adt.NamedElementDesc)
    generated * : BOOLEAN;
    type      * : sys.INT8; -- для базовых типов Модулы-2
    type_kind * : INT;
    type_size * : INT;
    (* anything what you want *)
  END;


  Module * = POINTER TO ModuleDesc;
  ModuleDesc * = RECORD (adt.NamedElementDesc)
    cstdlib * : BOOLEAN;
  END;

  Modifier * = POINTER TO ModifierDesc;
  ModifierDesc * = RECORD (adt.ElementDesc)
    modifier * : INT;
    line * : INT;
    pos * : INT;
    file_name * : lstr.String;
  END;

  Expression * = POINTER TO ExpressionDesc;
  ExpressionDesc * = RECORD (adt.ElementDesc)
    line * , pos * : INT;
  END;

  Object * = POINTER TO ObjectDesc;
  ObjectDesc * = RECORD (adt.NamedElementDesc)
    header * : Header;
    line   * : INT;  (* for comment it is line of '/*'
			for other it is first letter of name *)
    file   * : lstr.String;
  END;


  Comment * = POINTER TO CommentDesc;
  CommentDesc * = RECORD (ObjectDesc)
  END;

  Type * = POINTER TO TypeDesc;
  TypeDesc * = RECORD (ObjectDesc)
    type	  * : INT;
    base	  * : Type;	  (* for pointers, arrays & functions *)
    expr	  * : Expression; (* dimension for array *)
    modifier	  * : adt.List;   (* list of Modifier *)
    ptr_modifier  * : Modifier;   (* only for pointers *)
    vars          * : adt.List;   (* function parameters or structure fields *)
    generated	  * : BOOLEAN;
    h_generated   * : BOOLEAN;
    gen_type_def  * : INT;
    back_end	  * : INT;	  (* type is used only in c back end or no *)
    ttts_number   * : INT;	  (* for fight with recursive data structures*)
    tobj_modifier * : adt.List;   (* modifier TypedObject *)
    tag_description_header * : Header;
    not_described * : BOOLEAN;	  (* false if structure is described *)
    created_by_back_end * : BOOLEAN;
    translation_variant * : M2Type;
  END;



  TypedObject * = POINTER TO TypedObjectDesc;
  TypedObjectDesc * = RECORD (ObjectDesc)
    obj 	 * : INT;	   (* See Typed objects *)
    type	 * : Type;	   (* constant & bitset_const have no it *)
    modifier	 * : adt.List;	   (* List of Modifier ( only for variables ) *)
    mem_class	 * : INT;	   (* only for variables *)
    expr	 * : Expression;   (* for arithmetic, string, char constants,
				      enum constants and bit fields *)
    back_end	 * : INT;	   (* constant synonym is used only in
				      C back end or no *)
    var          * : BOOLEAN;      (* only for function parameters *)
    generated	 * : BOOLEAN;	   (* need for const in expression *)
    type_name    * : M2Type;
  END;


  Macro * = POINTER TO MacroDesc;
  MacroDesc * = RECORD (ObjectDesc)
    params * : adt.List;  (* adt.NamedElement *)
    text   * : lstr.String;
  END;

  Replacement * = POINTER TO ReplacementDesc;
  ReplacementDesc * = RECORD ( adt.NamedElementDesc )
    params    * : adt.List;  (* adt.NamedElement *)
    pattern   * : adt.List;  (* scan.Token	 *)
    tokens    * : adt.List;  (* scan.Token	 *)
    used      * : BOOLEAN;
  END;

  Operation * = POINTER TO OperationDesc;
  OperationDesc * = RECORD (ExpressionDesc)
    operation * : INT;
  END;

  UnaryOperation * = POINTER TO UnaryOperationDesc;
  UnaryOperationDesc * = RECORD (OperationDesc)
    first * : Expression;
    type  * : Type
  END;

  BinaryOperation * = POINTER TO BinaryOperationDesc;
  BinaryOperationDesc * = RECORD (OperationDesc)
    first *, second * : Expression;
  END;

  TernaryOperation * = POINTER TO TernaryOperationDesc;
  TernaryOperationDesc * = RECORD (OperationDesc)
    first *, second *, third * : Expression;
  END;

  Value * = POINTER TO ValueDesc;
  ValueDesc * = RECORD (ExpressionDesc)
    type  * : INT;  (* uint_const, int_const, real_const, ident, str_const, char_const *)
    value * : lstr.String; (* for all except ident *)
    tobj  * : TypedObject;  (* only for ident *)
    hex   * : BOOLEAN; (* only for int_const & uint_const *)
  END;

  ConstantValue * = POINTER TO ConstantValueDesc;
  ConstantValueDesc * = RECORD (adt.ElementDesc)
    type * : INT;   (* uint_const, int_const, real_const, str_const *)
    int  * : INT;
    uint * : CARD;
    real * : LONGREAL;
    str  * : lstr.String;
    hex  * : BOOLEAN; (* only for int_const & uint_const *)
  END;


  NameSynonym  * = POINTER TO NameSynonymDesc;
  NameSynonymDesc * = RECORD (adt.NamedElementDesc)
    newname * : lstr.String;
  END;

  TypedObjectHandler = PROCEDURE(tobj: TypedObject; VAR str: lstr.String);

VAR
  Headers * : adt.List;
  CurrentHeader * : Header;
  SuperHeader	* : Header;
  ModuleNames	* : adt.Tree;	(* Module names defined by user (NameSynonym) *)
  M2Types       * : adt.Tree;
  pd_define_is_parsed * : BOOLEAN;
  ValueHandler	* : TypedObjectHandler; (* const which used in expression must
					   be generated before using this expression *)

  file_name * : lstr.String;
  line *, pos * : INT;


(*----------------------------------------------------*)
VAR TypeStorage: adt.List;
    TypedObjectStorage: adt.List;
    ReplacementStorage: adt.List;
    UnaryOperationStorage: adt.List;
    BinaryOperationStorage: adt.List;
    TernaryOperationStorage: adt.List;
    ValueStorage: adt.List;
    ConstantValueStorage: adt.List;
    ModifierStorage: adt.List;
    M2TypeStorage: adt.List;
    ListForFindInSomewhere: adt.List;
(*
VAR crash_ptr: lstr.String;
PROCEDURE crash;
BEGIN
  lstr.Assign(crash_ptr^, crash_ptr);
END crash;
*)

(*----------------------------------------------------*)
PROCEDURE Deallocate * (e: adt.Element);
BEGIN
  IF e = NIL THEN RETURN END;
  WITH
     e: Type DO
       e.vars.Clean();
       e.modifier.Clean();
       e.tobj_modifier.Clean();
       Deallocate(e.ptr_modifier);
       Deallocate(e.translation_variant );
       lstr.Deallocate(e.name);
       TypeStorage.Insert(e);
    |e: TypedObject DO
       e.modifier.Clean();
       e.type:= NIL;
       lstr.Deallocate(e.name);
       Deallocate(e.type_name);
       TypedObjectStorage.Insert(e);
    |e: Replacement DO
       e.tokens.Clean();
       e.params.Clean();
       e.pattern.Clean();
       ReplacementStorage.Insert(e);
    |e: UnaryOperation DO
       e.first:= NIL;
       e.type:= NIL;
       UnaryOperationStorage.Insert(e);
    |e: BinaryOperation DO
       e.first:= NIL;
       e.second:= NIL;
       BinaryOperationStorage.Insert(e);
    |e: TernaryOperation DO
       e.first:= NIL;
       e.second:= NIL;
       e.third:= NIL;
       TernaryOperationStorage.Insert(e);
    |e: Value DO
       e.tobj:= NIL;
       lstr.Deallocate(e.value);
       ValueStorage.Insert(e);
    |e: Modifier DO
       lstr.Deallocate(e.file_name);
       ModifierStorage.Insert(e);
    |e: ConstantValue DO
       lstr.Deallocate(e.str);
       ConstantValueStorage.Insert(e);
    |e: M2Type DO
       lstr.Deallocate(e.name);
       M2TypeStorage.Insert(e);
  ELSE
  END
END Deallocate;
(*------------------------------------------------------------------------*)
PROCEDURE Error(number: INT; SEQ x: sys.BYTE);
BEGIN
  CurrentHeader.stat.Error(number, file_name^, line, pos+1, x);
END Error;

PROCEDURE Warning(number: INT; SEQ x: sys.BYTE);
BEGIN
  CurrentHeader.stat.Warning(number, file_name^, line, pos+1, x);
END Warning;

(*------------------------------------------------------------------------*)
PROCEDURE int_to_hex(int: INT; VAR str: lstr.String);
VAR
  digit: ARRAY 2 OF CHAR;
  n: INT;
  card: CARD;
BEGIN
  card:= sys.VAL(CARD, int);
  digit[1]:= file.EOS;
  REPEAT
    n:= card MOD 16;
    IF n < 10 THEN
      digit[0]:= CHR(n + ORD('0'));
    ELSE
      digit[0]:= CHR(n - 10 + ORD('A'));
    END;
    lstr.Insert(digit, 0, str);
    card:= card DIV 16;
  UNTIL card = 0;
  IF (str[0] >= 'A') & (str[0] <= 'F' ) THEN
    digit[0]:= '0';
    lstr.Insert(digit, 0, str);
  END;
  lstr.AppendChar('H', str);
END int_to_hex;

(*----------------------------------------------------*)
PROCEDURE NewM2Type * (VAR m2type: M2Type; name-: ARRAY OF CHAR );
VAR e: adt.Element;
BEGIN
  IF M2TypeStorage.IsEmpty() THEN
    NEW(m2type);
  ELSE
    M2TypeStorage.FindFirst(e);
    M2TypeStorage.DeleteCurrent();
    m2type:= e(M2Type);
  END;
  m2type.SetName(name);
  IF    (name = "INTEGER"   ) THEN m2type.type:= bt_INTEGER;
  ELSIF (name = "SHORTINT"  ) THEN m2type.type:= bt_SHORTINT;
  ELSIF (name = "LONGINT"   ) THEN m2type.type:= bt_LONGINT;
  ELSIF (name = "CARDINAL"  ) THEN m2type.type:= bt_CARDINAL;
  ELSIF (name = "SHORTCARD" ) THEN m2type.type:= bt_SHORTCARD;
  ELSIF (name = "LONGCARD"  ) THEN m2type.type:= bt_LONGCARD;
  ELSIF (name = "CHAR"      ) THEN m2type.type:= bt_CHAR;
  ELSIF (name = "BITSET"    ) THEN m2type.type:= bt_BITSET;
  ELSIF (name = "BOOLEAN"   ) THEN m2type.type:= bt_BOOLEAN;
  ELSIF (name = "REAL"      ) THEN m2type.type:= bt_REAL;
  ELSIF (name = "LONGREAL"  ) THEN m2type.type:= bt_LONGREAL;
  ELSIF (name = "SYSTEM.BYTE"      ) OR
        (name = "BYTE"      ) THEN m2type.type:= bt_BYTE;
  ELSIF (name = "SYSTEM.BOOL8"     ) OR
        (name = "SYSTEM.BOOL8"     ) THEN m2type.type:= bt_BOOL8;
  ELSIF (name = "SYSTEM.BOOL16"    ) OR
        (name = "SYSTEM.BOOL16"    ) THEN m2type.type:= bt_BOOL16;
  ELSIF (name = "SYSTEM.BOOL32"    ) OR
        (name = "SYSTEM.BOOL32"    ) THEN m2type.type:= bt_BOOL32;
  ELSIF (name = "SYSTEM.INT8"      ) OR
        (name = "SYSTEM.INT8"      ) THEN m2type.type:= bt_INT8;
  ELSIF (name = "SYSTEM.INT16"     ) OR
        (name = "SYSTEM.INT16"     ) THEN m2type.type:= bt_INT16;
  ELSIF (name = "SYSTEM.INT32"     ) OR
        (name = "SYSTEM.INT32"     ) THEN m2type.type:= bt_INT32;
  ELSIF (name = "SYSTEM.CARD8"     ) OR
        (name = "SYSTEM.CARD8"     ) THEN m2type.type:= bt_CARD8;
  ELSIF (name = "SYSTEM.CARD16"    ) OR
        (name = "SYSTEM.CARD16"    ) THEN m2type.type:= bt_CARD16;
  ELSIF (name = "SYSTEM.CARD32"    ) OR
        (name = "SYSTEM.CARD32"    ) THEN m2type.type:= bt_CARD32;
  ELSIF (name = "SYSTEM.SET16"     ) OR
        (name = "SYSTEM.SET16"     ) THEN m2type.type:= bt_SET16;
  ELSIF (name = "SYSTEM.SET32"     ) OR
        (name = "SYSTEM.SET32"     ) THEN m2type.type:= bt_SET32;
  ELSIF (name = "SYSTEM.int"       ) OR
        (name = "SYSTEM.int"       ) THEN m2type.type:= bt_xds_int;
  ELSIF (name = "SYSTEM.unsigned"  ) OR
        (name = "SYSTEM.unsigned"  ) THEN m2type.type:= bt_xds_unsigned;
  ELSE                                    m2type.type:= bt_unknown;
  END;
END NewM2Type;

(*----------------------------------------------------*)
PROCEDURE NewConstantValue * (VAR v: ConstantValue);
VAR e: adt.Element;
BEGIN
  IF ConstantValueStorage.IsEmpty() THEN
    NEW(v);
  ELSE
    ConstantValueStorage.FindFirst(e);
    ConstantValueStorage.DeleteCurrent();
    v:= e(ConstantValue);
  END;
END NewConstantValue;

(*----------------------------------------------------*)
PROCEDURE FindInReplacements * (h: Header; ne: adt.NamedElement; VAR e: adt.Element);
VAR h1, elm: adt.Element;
BEGIN
  h.Replacements.Find(ne, e);
  IF e = NIL THEN
    ListForFindInSomewhere.Insert(h);
    h.includes.FindFirst(h1);
    WHILE h1 # NIL DO
      ListForFindInSomewhere.Find(h1, elm);
      IF elm = NIL THEN
	FindInReplacements(h1(Header), ne, e);
	IF e # NIL THEN
	  ListForFindInSomewhere.Clean();
	  RETURN;
	END;
      END;
      h.includes.FindNext(h1);
    END;
  END;
  ListForFindInSomewhere.FindFirst(h1);
  IF (e # NIL) OR (h = h1) THEN ListForFindInSomewhere.Clean() END;
END FindInReplacements;

(*------------------------------------------------------*)
PROCEDURE FindInNames * (h: Header; ne: adt.NamedElement; VAR e: adt.Element);
VAR h1, elm: adt.Element;
BEGIN
  h.Names.Find(ne, e);
  IF e = NIL THEN
    ListForFindInSomewhere.Insert(h);
    h.includes.FindFirst(h1);
    WHILE h1 # NIL DO
      ListForFindInSomewhere.Find(h1, elm);
      IF elm = NIL THEN
	FindInNames(h1(Header), ne, e);
	IF e # NIL THEN RETURN END;
      END;
      h.includes.FindNext(h1);
    END;
  END;
  ListForFindInSomewhere.FindFirst(h1);
  IF (e # NIL) OR (h = h1) THEN ListForFindInSomewhere.Clean() END;
END FindInNames;

PROCEDURE FindInTags * (h: Header; ne: adt.NamedElement; VAR e: adt.Element);
VAR h1, elm: adt.Element;
BEGIN
  h.Tags.Find(ne, e);
  IF e = NIL THEN
    ListForFindInSomewhere.Insert(h);
    h.includes.FindFirst(h1);
    WHILE h1 # NIL DO
      ListForFindInSomewhere.Find(h1, elm);
      IF elm = NIL THEN
	FindInTags(h1(Header), ne, e);
	IF e # NIL THEN
	  ListForFindInSomewhere.Clean();
	  RETURN;
	END;
      END;
      h.includes.FindNext(h1);
    END;
  END;
  ListForFindInSomewhere.FindFirst(h1);
  IF (e # NIL) OR (h = h1) THEN ListForFindInSomewhere.Clean() END;
END FindInTags;

PROCEDURE FindInDefined * (h: Header; ne: adt.NamedElement; VAR e: adt.Element);
VAR h1, elm: adt.Element;
BEGIN
  h.defined.Find(ne, e);
  IF e = NIL THEN
    ListForFindInSomewhere.Insert(h);
    h.includes.FindFirst(h1);
    WHILE h1 # NIL DO
      ListForFindInSomewhere.Find(h1, elm);
      IF elm = NIL THEN
	FindInDefined(h1(Header), ne, e);
	IF e # NIL THEN
	  ListForFindInSomewhere.Clean();
	  RETURN;
	END;
      END;
      h.includes.FindNext(h1);
    END;
  END;
  ListForFindInSomewhere.FindFirst(h1);
  IF (e # NIL) OR (h = h1) THEN ListForFindInSomewhere.Clean() END;
END FindInDefined;


(*------------------------------------------------------------------------*)
PROCEDURE ( val: ConstantValue ) GetText* (string: BOOLEAN): lstr.String;
(* Sometimes need only number without strings representation
   argumen 'string' switch it *)
VAR
  s: lstr.String;
  a: ARRAY 32 OF CHAR;
BEGIN
  s:= NIL;
  IF ( lstr.Length1(val.str) > 0 ) & string THEN
    lstr.Assign( val.str^, s );
  ELSE
    CASE val.type OF
      |int_const:
	IF val.hex THEN
	  int_to_hex(val.int, s);
	ELSE
	  WholeStr.IntToStr( val.int, a ); lstr.Assign( a, s );
	END;
      |uint_const:
	IF val.hex THEN
	  int_to_hex(sys.VAL(INT, val.uint), s);
	ELSE
	  WholeStr.IntToStr( sys.VAL(INT, val.uint), a ); lstr.Assign( a, s );
	END;
      |real_const: lstr.RealToStr( val.real, s ); 
      |str_const, char_const: lstr.Assign( val.str^, s );
    END;
  END;
  RETURN s
END GetText;

PROCEDURE Stop( number: INT; VAR res: ConstantValue );
BEGIN
  IF pd_define_is_parsed THEN res := NIL
  ELSE Error(number, '') END
END Stop;


PROCEDURE ( expr: Expression )
  ComputeExpression* ( VAR res: ConstantValue; limex: BOOLEAN );
END ComputeExpression;

PROCEDURE ( expr: Expression )
  CheckOperationOrder ( operation: INT; res: ConstantValue );
BEGIN
END CheckOperationOrder;


PROCEDURE ( expr: Value )
  ComputeExpression* ( VAR res: ConstantValue; limex: BOOLEAN );
VAR r: ConvTypes.ConvResults;
BEGIN
(*
  io.printf("Value: type = %d", expr.type );
  IF expr.value # NIL THEN io.printf( "value = %s\n", expr.value^ ) END;
*)
  NewConstantValue( res ); res.hex:= expr.hex;
  res.type := expr.type;
  CASE expr.type OF
    |int_const: WholeStr.StrToInt( expr.value^, res.int, r );
      res.str:= res.GetText(TRUE);
    |uint_const: WholeStr.StrToCard( expr.value^, res.uint, r );
      res.str:= res.GetText(TRUE);
    |real_const:
      IF limex THEN
	Error(16, '');
	RETURN
      END;
      LongStr.StrToReal( expr.value^, res.real, r );
      res.str:= res.GetText(TRUE);
    |char_const:
      res.type := int_const;
      res.int := ORD( expr.value[0] );
    |str_const:
      IF limex THEN
	Error(16, '');
	RETURN
      END;
      lstr.Assign(expr.value^, res.str);
    |ident: Deallocate(res); expr.tobj.expr.ComputeExpression( res, limex );
      IF lstr.Length1(expr.tobj.name) > 0 THEN
	ValueHandler(expr.tobj, res.str);
	--lstr.Assign( expr.tobj.name^, res.str); делается теперь в ValueHandler
      END;
  END
END ComputeExpression;


PROCEDURE ( expr: UnaryOperation )
  CheckOperationOrder ( operation: INT; res: ConstantValue );
BEGIN
END CheckOperationOrder;

PROCEDURE ( expr: UnaryOperation )
  ComputeExpression* ( VAR res: ConstantValue; limex: BOOLEAN );
VAR
  e1: ConstantValue;
  i, ui, r: BOOLEAN;
  ne: adt.NamedElement;
  e: adt.Element;

(* fix for string representation  minus for r, i
*)

  PROCEDURE TypeConv( nt: Type );
  BEGIN
    IF msg.WasError THEN RETURN END;
    CASE nt.type OF
      |bt_s_char, bt_s_int, bt_s_sh_int, bt_s_l_int:
	 res.type := int_const;
	 IF i THEN
	   res.int:= e1.int;
	 ELSIF ui THEN
	   res.int:= sys.VAL(INT, e1.uint);
	 ELSE
	   res.int:= ENTIER(e1.real);
	 END;
      |bt_u_int, bt_u_l_int, bt_u_char, bt_u_sh_int:
	 res.type := uint_const;
	 IF i THEN
	   res.uint:= sys.VAL(CARD, e1.int);
	 ELSIF ui THEN
	   res.uint:= e1.uint;
	 ELSE
	   res.uint:= sys.VAL(CARD, ENTIER(e1.real));
	 END;
      |bt_float, bt_double, bt_l_float, bt_l_double:
	 res.type := real_const;
	 IF i THEN
	   res.real:= e1.int;
	 ELSIF ui THEN
	   res.real:= e1.uint;
	 ELSE
	   res.real:= e1.real;
	 END;
      |t_synonym: TypeConv( nt.base );
      ELSE
	Stop(11, res)
    END
  END TypeConv;

BEGIN
  IF expr.first = NIL THEN
    io.printf('\nline = %d, pos = %d\n',msg.Line, msg.Pos);
  END;

  NewConstantValue( res ); res.hex:= FALSE;
  IF expr.operation = defined_operation THEN
    expr.first.ComputeExpression( e1, FALSE ); IF msg.WasError THEN RETURN END;
    IF ~limex THEN
      Stop(12, res);
      RETURN
    END;
    IF e1.type # str_const THEN
      Stop(13, res);
      RETURN
    END;
    res.type := int_const;
    adt.NewNamedElement(ne, e1.str^); Deallocate(e1);
--    CurrentHeader.defined.Find( ne, e );
    FindInDefined(CurrentHeader, ne, e);
    adt.Deallocate(ne);
    IF e # NIL THEN res.int := 1 ELSE res.int := 0 END;
    RETURN
  END;
  expr.first.ComputeExpression( e1, limex ); IF msg.WasError THEN RETURN END;
  i := e1.type = int_const;
  ui:= e1.type = uint_const;
  r := e1.type = real_const;
  IF ~i & ~r & ~ui THEN Deallocate(e1); Stop(13, res); RETURN END;
  IF i OR ui THEN res.type := int_const ELSE res.type := real_const END;
  CASE expr.operation OF
    |not:
      res.type := int_const;
      IF r THEN IF e1.real = 0 THEN e1.int := 0 ELSE e1.int := 1 END END;
      IF e1.int = 0 THEN res.int := 1 ELSE res.int := 0 END;
      Deallocate(e1);
      RETURN
    |type_conv_operation:
      IF limex THEN
	Error(17, '');
	Deallocate(e1);
	RETURN
      END;
      TypeConv( expr.type );
      Deallocate(e1);
      RETURN
    |sizeof_operation:
      IF limex THEN
	Error(17, '');
	Deallocate(e1);
	RETURN
      END;
      Warning(90, '');
      Deallocate(e1);
      RETURN
    ELSE
  END;

  IF r THEN
    CASE expr.operation OF
      |minus: res.real := -e1.real;
	 expr.first.CheckOperationOrder(unary_minus, e1);
	 res.str:= e1.GetText(TRUE);
	 lstr.Insert('-', 0, res.str);
      |plus: res.real := e1.real;
	 res.str:= e1.GetText(TRUE);
      |sizeof_operation:
    ELSE Stop(13, res) END;
    Deallocate(e1);
    RETURN
  END;

  IF i THEN
    CASE expr.operation OF
      |sizeof_operation:	    (* I think it must be defined in CFG *)
      |minus: res.int := -e1.int;
	 expr.first.CheckOperationOrder(unary_minus, e1);
	 res.str:= e1.GetText(TRUE);
	 lstr.Insert('-', 0, res.str);
      |plus: res.int := e1.int;
	 res.str:= e1.GetText(TRUE);
      |tilda: res.int := sys.VAL( INT, - sys.VAL( SET, e1.int ) );
    END
  END;

  IF ui THEN
    CASE expr.operation OF
      |sizeof_operation:	    (* I think it must be defined in CFG *)
      |minus:
	res.type:= int_const;
	res.int := -sys.VAL(INT, e1.uint);
	 expr.first.CheckOperationOrder(unary_minus, e1);
	 res.str:= e1.GetText(TRUE);
	lstr.Insert('-', 0, res.str);
      |plus:
	res.type:= int_const;
	res.int := sys.VAL(INT, e1.uint);
	res.str:= e1.GetText(TRUE);
      |tilda: res.uint := sys.VAL( CARD, - sys.VAL( SET, e1.uint ) );
    END
  END;
  Deallocate(e1);
END ComputeExpression;


PROCEDURE ( expr: BinaryOperation )
  CheckOperationOrder ( operation: INT; res: ConstantValue );
BEGIN
  IF lstr.Length1(res.str) > 0	THEN
    lstr.Insert('(', 0, res.str);
    lstr.Append(')', res.str);
  END;
END CheckOperationOrder;

PROCEDURE ( expr: BinaryOperation )
  ComputeExpression* ( VAR res: ConstantValue; limex: BOOLEAN );
VAR
  e1, e2: ConstantValue;
  i, i1, i2, ui, ui1, ui2, r, r1, r2, b, cmp: BOOLEAN;
  n: INTEGER;
  s: SET;

(*fix for string representation  star, div, mod, plus, minus
*)

BEGIN
  NewConstantValue( res ); res.hex:= FALSE; e1:= NIL; e2:= NIL;
  expr.first.ComputeExpression( e1, limex ); IF msg.WasError THEN RETURN END;
  IF expr.operation = l_and THEN
    b := ~(( e1.type = int_const  ) & ( e1.int	= 0 )) &
	 ~(( e1.type = uint_const ) & ( e1.uint = 0 )) &
	 ~(( e1.type = real_const ) & ( e1.real = 0 ));
    IF b THEN
      expr.second.ComputeExpression( e2, limex ); IF msg.WasError THEN RETURN END;
      b := ~(( e2.type = int_const  ) & ( e2.int  = 0 )) &
	   ~(( e2.type = uint_const ) & ( e2.uint = 0 )) &
	   ~(( e2.type = real_const ) & ( e2.real = 0 ));
    END;
    res.type := int_const;
    IF b THEN res.int := 1 ELSE res.int := 0 END;
    Deallocate(e1); Deallocate(e2);
    RETURN
  END;
  IF expr.operation = l_or THEN
    b := ~(( e1.type = int_const  ) & ( e1.int	= 0 )) &
	 ~(( e1.type = uint_const ) & ( e1.uint = 0 )) &
	 ~(( e1.type = real_const ) & ( e1.real = 0 ));
    IF ~b THEN
      expr.second.ComputeExpression( e2, limex ); IF msg.WasError THEN RETURN END;
      b := ~(( e2.type = int_const  ) & ( e2.int = 0 )) &
	   ~(( e2.type = uint_const ) & ( e2.uint = 0 )) &
	   ~(( e2.type = real_const ) & ( e2.real = 0 ));
    END;
    res.type := int_const;
    IF b THEN res.int := 1 ELSE res.int := 0 END;
    Deallocate(e1); Deallocate(e2);
    RETURN
  END;
  expr.second.ComputeExpression( e2, limex ); IF msg.WasError THEN RETURN END;
  IF expr.operation = strconcat_operation THEN
    res.type := str_const;
    lstr.Concat( e1.str^, e2.str^, res.str );
    Deallocate(e1); Deallocate(e2);
    RETURN
  END;
  i1 := e1.type = int_const;
  i2 := e2.type = int_const;
  ui1:= e1.type = uint_const;
  ui2:= e2.type = uint_const;
  r1 := e1.type = real_const;
  r2 := e2.type = real_const;
  IF ( ~i1 & ~ui1 & ~r1 ) OR ( ~i2 & ~ui2 & ~r2 ) THEN
    Stop(13, res);
    Deallocate(e1); Deallocate(e2);
    RETURN
  END;
  r := r1 OR r2;
  ui := (ui1 OR ui2) & ~r1 & ~r2;
  i := i1 & i2;
  IF i THEN
    res.type := int_const;
  ELSIF r THEN
    res.type := real_const;
  ELSE
    res.type := uint_const;
  END;
  IF r & i1  THEN e1.real := e1.int END;
  IF r & ui1 THEN e1.real := e1.uint END;
  IF r & i2  THEN e2.real := e2.int END;
  IF r & ui2 THEN e2.real := e2.uint END;
  IF ui & i1 THEN e1.uint  := sys.VAL( CARD,	e1.int	) END;
  IF ui & i2 THEN e2.uint  := sys.VAL( CARD,	e2.int	) END;
  cmp := TRUE;
  b := TRUE;		    (* for remove warning *)
  CASE expr.operation OF
    |more:
      IF i THEN
	b := e1.int > e2.int;
      ELSIF ui THEN
	b := e1.uint > e2.uint;
      ELSE
	b := e1.real > e2.real;
      END
    |less:
      IF i THEN
	b := e1.int < e2.int;
      ELSIF ui THEN
	b := e1.uint < e2.uint;
      ELSE
	b := e1.real < e2.real;
      END
    |more_equ:
      IF i THEN
	b := e1.int >= e2.int;
      ELSIF ui THEN
	b := e1.uint >= e2.uint;
      ELSE
	b := e1.real >= e2.real;
      END
    |less_equ:
      IF i THEN
	b := e1.int <= e2.int;
      ELSIF ui THEN
	b := e1.uint <= e2.uint;
      ELSE
	b := e1.real <= e2.real;
      END
    |equ:
      IF i THEN
	b := e1.int = e2.int;
      ELSIF ui THEN
	b := e1.uint = e2.uint;
      ELSE
	b := e1.real = e2.real;
      END
    |not_equ:
      IF i THEN
	b := e1.int # e2.int;
      ELSIF ui THEN
	b := e1.uint # e2.uint;
      ELSE
	b := e1.real # e2.real;
      END
    ELSE cmp := FALSE
  END;
  IF cmp THEN
    res.type := int_const;
    IF b THEN res.int := 1 ELSE res.int := 0 END;
    Deallocate(e1); Deallocate(e2);
    RETURN
  END;

  IF r THEN
    CASE expr.operation OF
      |star: res.real := e1.real*e2.real;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(star, e1);
	  expr.second.CheckOperationOrder(star, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( '*', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      |div:
	IF e2.real = 0 THEN Stop(14, res); RETURN END;
	res.real := e1.real/e2.real;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(div, e1);
	  expr.second.CheckOperationOrder(div, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( '/', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      |plus: res.real := e1.real+e2.real;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(plus, e1);
	  expr.second.CheckOperationOrder(plus, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( '+', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      |minus: res.real := e1.real-e2.real;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(minus, e1);
	  expr.second.CheckOperationOrder(minus, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( '-', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      ELSE Stop(13, res)
    END;
    Deallocate(e1); Deallocate(e2);
    RETURN
  END;

  IF i THEN
    CASE expr.operation OF
      |star: res.int := e1.int*e2.int;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(star, e1);
	  expr.second.CheckOperationOrder(star, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( '*', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      |div:
	IF e2.int = 0 THEN Stop(14, res); RETURN END;
	res.int := e1.int DIV e2.int;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(div, e1);
	  expr.second.CheckOperationOrder(div, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( ' DIV ', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      |mod:
	IF e2.int <= 0 THEN Stop(15, res); RETURN END;
	res.int := e1.int MOD e2.int;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(mod, e1);
	  expr.second.CheckOperationOrder(mod, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( ' MOD ', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      |plus: res.int := e1.int+e2.int;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(plus, e1);
	  expr.second.CheckOperationOrder(plus, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( '+', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      |minus: res.int := e1.int-e2.int;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(minus, e1);
	  expr.second.CheckOperationOrder(minus, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( '-', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      |b_lshift: res.int := sys.LSH( e1.int, e2.int );
      |b_rshift:
	e2.int := ABS( e2.int ) MOD MAX( SET );
	s := {};
	FOR n := 0 TO MAX( SET ) DO
	  IF ( e1.int < 0 ) & ( n > MAX( SET ) - e2.int ) THEN INCL( s, n )
	  ELSIF ( n + e2.int ) IN sys.VAL( SET, e1.int ) THEN INCL( s, n ) END
	END;
	res.int := sys.VAL( INT, s );
      |ampersand:
	res.int := sys.VAL( INT, sys.VAL( SET, e1.int ) * sys.VAL( SET, e2.int ));
      |b_xor:
	res.int := sys.VAL( INT, sys.VAL( SET, e1.int ) / sys.VAL( SET, e2.int ));
      |b_or:
	res.int := sys.VAL( INT, sys.VAL( SET, e1.int ) + sys.VAL( SET, e2.int ));
    END
  END;

  IF ui THEN
    CASE expr.operation OF
      |star: res.uint := e1.uint*e2.uint;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(star, e1);
	  expr.second.CheckOperationOrder(star, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( '*', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      |div:
	IF e2.uint = 0 THEN Stop(14, res); RETURN END;
	res.uint := e1.uint DIV e2.uint;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(div, e1);
	  expr.second.CheckOperationOrder(div, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( ' DIV ', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      |mod:
	IF e2.uint <= 0 THEN Stop(15, res); RETURN END;
	res.uint := e1.uint MOD e2.uint;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(mod, e1);
	  expr.second.CheckOperationOrder(mod, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( ' MOD ', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      |plus: res.uint := e1.uint+e2.uint;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(plus, e1);
	  expr.second.CheckOperationOrder(plus, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( '+', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      |minus: res.uint := e1.uint-e2.uint;
	IF ( lstr.Length1(e1.str) > 0 ) & ( lstr.Length1(e2.str) >0 ) THEN
	  expr.first.CheckOperationOrder(minus, e1);
	  expr.second.CheckOperationOrder(minus, e2);
	  lstr.Append( e1.str^, res.str);
	  lstr.Append( '-', res.str);
	  lstr.Append( e2.str^, res.str);
	END;
      |b_lshift: res.uint := sys.LSH( sys.VAL(INT, e1.uint), sys.VAL(INT, e2.uint) );
      |b_rshift:
	e2.uint := e2.uint MOD MAX( SET );
	s := {};
	FOR n := 0 TO MAX( SET ) DO
	  IF ( sys.VAL(CARD, n) + e2.uint ) IN sys.VAL( SET, e1.uint ) THEN INCL( s, n ) END
	END;
	res.uint := sys.VAL( INT, s );
      |ampersand:
	res.uint := sys.VAL( INT, sys.VAL( SET, e1.uint ) * sys.VAL( SET, e2.uint ));
      |b_xor:
	res.uint := sys.VAL( INT, sys.VAL( SET, e1.uint ) / sys.VAL( SET, e2.uint ));
      |b_or:
	res.uint := sys.VAL( INT, sys.VAL( SET, e1.uint ) + sys.VAL( SET, e2.uint ));
    END
  END;
  Deallocate(e1); Deallocate(e2);
END ComputeExpression;


PROCEDURE ( expr: TernaryOperation )
  ComputeExpression* ( VAR res: ConstantValue; limex: BOOLEAN );
VAR
  e1, e2, e3: ConstantValue;
  i1, i2, i3, r1, r2, r3: BOOLEAN;
  ui1, ui2, ui3: BOOLEAN;
BEGIN
  expr.first.ComputeExpression( e1, limex ); IF msg.WasError THEN RETURN END;
  expr.second.ComputeExpression( e2, limex ); IF msg.WasError THEN RETURN END;
  expr.third.ComputeExpression( e3, limex ); IF msg.WasError THEN RETURN END;
  i1 := e1.type  = int_const;
  i2 := e2.type  = int_const;
  i3 := e3.type  = int_const;
  ui1:= e1.type = uint_const;
  ui2:= e2.type = uint_const;
  ui3:= e3.type = uint_const;
  r1 := e1.type  = real_const;
  r2 := e2.type  = real_const;
  r3 := e3.type  = real_const;
  IF ( ~i1 & ~ui1 & ~r1 ) OR ( ~i2 & ~ui2 & ~r2 ) OR ( ~i3 & ~ui3 & ~r3 ) THEN
    Stop(13, res);
    RETURN
  END;
  IF ( i1 & ( e1.int = 0 )) OR ( r1 & ( e1.real = 0 ))
			    OR
		 ( ui1 & ( e1.uint = 0 ))
  THEN res := e3
  ELSE res := e2 END
END ComputeExpression;


(*----------------------------------------------------*)
PROCEDURE ( m: Modifier ) Compare * ( e: adt.Element ): INT;
BEGIN
  IF e IS Modifier THEN
    IF m.modifier = e(Modifier).modifier THEN
      RETURN adt.equal;
    ELSIF m.modifier > e(Modifier).modifier THEN
      RETURN adt.more;
    ELSE
      RETURN adt.less;
    END;
  ELSE
    RETURN adt.noncompared;
  END;
END Compare;

(*----------------------------------------------------*)
PROCEDURE NewHeader * ( name-: ARRAY OF CHAR; VAR h: Header );
BEGIN
  NEW(h);
  h.SetName(name);
  h.preheader:= FALSE;
  adt.NewTree(h.Replacements);
  adt.NewTree(h.Tags);
  adt.NewTree(h.Names);
  adt.NewTree(h.Macros);
  adt.NewTree(h.Synonyms);
  adt.NewList(h.Parameters);
  adt.NewList(h.ModifiedObjects);
  adt.NewTree(h.defined);
  adt.NewList(h.objects);
  adt.NewList(h.includes);
  adt.NewList(h.mergedheaders);
  h.parsed:= FALSE;
  h.generated:= FALSE;
  h.cstdlib:= FALSE;
  msg.NewStatistics(h.stat, name);
END NewHeader;

(*----------------------------------------------------*)
PROCEDURE NewModifier * (mod: INT; VAR m: Modifier);
VAR e: adt.Element;
BEGIN
  IF ModifierStorage.IsEmpty() THEN
    NEW(m);
  ELSE
    ModifierStorage.FindFirst(e);
    ModifierStorage.DeleteCurrent();
    m:= e(Modifier);
  END;
  m.modifier:= mod;
END NewModifier;

(*----------------------------------------------------*)
PROCEDURE NewTypedObject * ( VAR tobj: TypedObject; obj: INT );
VAR e: adt.Element;
BEGIN
  IF TypedObjectStorage.IsEmpty() THEN
    NEW(tobj);
    adt.NewList(tobj.modifier);
  ELSE
    TypedObjectStorage.FindFirst(e);
    TypedObjectStorage.DeleteCurrent();
    tobj:= e(TypedObject);
  END;
  tobj.header:= CurrentHeader;
  tobj.obj:= obj;
  tobj.type:= NIL;
  tobj.mem_class:= undefined;
  tobj.expr:= NIL;
  tobj.type_name:= NIL;
  tobj.var:= FALSE;
  tobj.back_end:= common;
  tobj.generated:= FALSE;
END NewTypedObject;

(*----------------------------------------------------*)
PROCEDURE NewType * ( VAR type: Type );
VAR e: adt.Element;
BEGIN
  IF TypeStorage.IsEmpty() THEN
    NEW(type);
    adt.NewList(type.modifier);
    adt.NewList(type.vars);
    adt.NewList(type.tobj_modifier);
  ELSE
    TypeStorage.FindFirst(e);
    TypeStorage.DeleteCurrent();
    type:= e(Type);
  END;
  type.header:= CurrentHeader;
  type.type:= undefined;
  type.base:= NIL;
  type.expr:= NIL;
  type.tag_description_header:= NIL;
  type.translation_variant:= NIL;
  type.generated:= FALSE;
  type.h_generated:= FALSE;
  NewModifier(nonmodified, type.ptr_modifier);
  type.gen_type_def:= on;
  type.back_end:= common;
  type.ttts_number:= -1;
  type.not_described:= FALSE;
  type.created_by_back_end:= FALSE;
  type.translation_variant:= NIL;
END NewType;

(*----------------------------------------------------*)
PROCEDURE NewReplacement * (VAR r: Replacement; name-: ARRAY OF CHAR);
VAR e: adt.Element;
BEGIN
  IF ReplacementStorage.IsEmpty() THEN
    NEW(r);
    adt.NewList(r.tokens);
    adt.NewList(r.pattern);
    adt.NewList(r.params);
  ELSE
    ReplacementStorage.FindFirst(e);
    ReplacementStorage.DeleteCurrent();
    r:= e(Replacement);
  END;
  r.SetName(name);
  r.used:= FALSE;
END NewReplacement;

(*----------------------------------------------------*)
PROCEDURE NewMacro * (name-: ARRAY OF CHAR; VAR m: Macro);
BEGIN
  NEW(m);
  m.SetName(name);
  m.header:= CurrentHeader;
  m.text:= NIL;
  adt.NewList(m.params);
END NewMacro;

(*----------------------------------------------------*)
PROCEDURE NewComment * ( VAR cm: Comment; line: INT; content-: ARRAY OF CHAR );
BEGIN
  NEW( cm );
  cm.header:= CurrentHeader;
  lstr.Assign( content, cm.name );
  cm.line:= line;
END NewComment;

(*----------------------------------------------------*)
PROCEDURE NewUnaryOperation * (VAR op: UnaryOperation);
VAR e: adt.Element;
BEGIN
  IF UnaryOperationStorage.IsEmpty() THEN
    NEW(op);
  ELSE
    UnaryOperationStorage.FindFirst(e);
    UnaryOperationStorage.DeleteCurrent();
    op:= e(UnaryOperation);
  END;
END NewUnaryOperation;

(*----------------------------------------------------*)
PROCEDURE NewBinaryOperation * (VAR op: BinaryOperation);
VAR e: adt.Element;
BEGIN
  IF BinaryOperationStorage.IsEmpty() THEN
    NEW(op);
  ELSE
    BinaryOperationStorage.FindFirst(e);
    BinaryOperationStorage.DeleteCurrent();
    op:= e(BinaryOperation);
  END;
END NewBinaryOperation;

(*----------------------------------------------------*)
PROCEDURE NewTernaryOperation * (VAR op: TernaryOperation);
VAR e: adt.Element;
BEGIN
  IF TernaryOperationStorage.IsEmpty() THEN
    NEW(op);
  ELSE
    TernaryOperationStorage.FindFirst(e);
    TernaryOperationStorage.DeleteCurrent();
    op:= e(TernaryOperation);
  END;
END NewTernaryOperation;

(*----------------------------------------------------*)
PROCEDURE NewValue * (VAR v: Value);
VAR e: adt.Element;
BEGIN
  IF ValueStorage.IsEmpty() THEN
    NEW(v);
  ELSE
    ValueStorage.FindFirst(e);
    ValueStorage.DeleteCurrent();
    v:= e(Value);
  END;
END NewValue;


(*----------------------------------------------------*)
PROCEDURE TranslateTypeToString * ( type : Type; VAR str: lstr.String );
VAR number: INT;
    modifier: adt.List;
    e: adt.Element;

  (*---------------------------*)
  PROCEDURE append_int ( prefix-: ARRAY OF CHAR; int: INT;
			 VAR str: lstr.String );
  VAR
    tmpstr: ARRAY 10 OF CHAR;
  BEGIN
    WholeStr.IntToStr( int, tmpstr );
    lstr.Append( prefix, str );
    lstr.Append( tmpstr, str );
  END append_int;

  (*---------------------------*)
  PROCEDURE _TranslateTypeToString ( type : Type; type_modifier: adt.List; VAR str: lstr.String;
				    VAR number: INT);
  VAR res: ConstantValue;
      e: adt.Element;
      tmp_tree: adt.Tree;
      resstr: lstr.String;

  BEGIN
    IF type # NIL THEN
      IF (type.ttts_number >= 0) & (type.type # t_ptr) THEN
	append_int( '->', type.ttts_number, str );
      ELSE
	adt.NewTree(tmp_tree);
	IF (type_modifier # NIL) & ~type_modifier.IsEmpty() THEN
	  type_modifier.FindFirst(e);
	  WHILE e # NIL DO
	    CASE e(Modifier).modifier OF
	      const, volatile:
	    ELSE
	      tmp_tree.Insert(e);
	    END;
	    type_modifier.FindNext(e);
	  END;
	END;

	WHILE type.type = t_synonym DO
	  type.ttts_number:= number;
	  type.modifier.FindFirst(e);
	  WHILE e # NIL DO
	    CASE e(Modifier).modifier OF
	      const, volatile:
	    ELSE
	      tmp_tree.Insert(e);
	    END;
	    type.modifier.FindNext(e);
	  END;
	  type:= type.base;
	END;

	tmp_tree.FindFirst(e);
	IF e # NIL THEN
	  append_int( 'tm{', e(Modifier).modifier, str );
	  tmp_tree.FindNext( e );
	  WHILE e # NIL DO
	    lstr.Append( ',', str );
	    append_int( '', e(Modifier).modifier, str );
	    tmp_tree.FindNext( e );
	  END;
	  lstr.Append( '};', str );
	END;
	adt.Deallocate(tmp_tree);

	(* type field *)
	type.ttts_number:= number; INC( number );
	append_int( 't', type.type, str );
	lstr.Append( ';', str );

	(* expr field *)
	IF type.expr # NIL THEN
	  type.expr.ComputeExpression( res, FALSE );
	  append_int( 'e', res.int, str ); Deallocate(res);
	ELSE
	  lstr.Append( 'e', str );
	END;
	lstr.Append( ';', str );

	(* ptr_modifier field *)
	IF type.ptr_modifier.modifier = nonmodified THEN
	  lstr.Append( 'pm', str );
	ELSE
	  append_int( 'pm', type.ptr_modifier.modifier, str );
	END;
	lstr.Append( ';', str );

	(* vars field *)
	IF type.vars.IsEmpty() THEN
	  lstr.Append( 'v', str );
	ELSE
	  lstr.Append( 'v{', str );
	  type.vars.FindFirst( e );
	  WITH e: TypedObject DO
	    append_int( 'o', e.obj, str );
	    IF				   (
	       ((type.type = t_struct) OR (type.type = t_union))
					   &
			       (e.obj = variable)
					   OR
				(type.type = t_enum)
					   &
			       (e.obj = enum_const)
					   )
	    THEN
	      lstr.Append( '[', str );
	      lstr.Append( e.name^, str );
	      lstr.Append( ']', str );
	    END;
	    lstr.Append( ':(', str );
	    _TranslateTypeToString( e.type, e.modifier, str, number );
	    lstr.Append( ')', str );
	    IF (e.obj = enum_const) OR (e.obj = bit_field) THEN
	      e.expr.ComputeExpression(res, FALSE);
	      resstr:= res.GetText(FALSE); Deallocate(res);
	      lstr.Append( '=', str );
	      lstr.Append( resstr^, str );
	    END;
	  ELSE
	  END;
	  type.vars.FindNext( e );
	  WHILE e # NIL DO
	    lstr.Append( ',', str );
	    WITH e: TypedObject DO
	      append_int( 'o', e.obj, str );
	      lstr.Append( ':(', str );
	      _TranslateTypeToString( e.type, e.modifier, str, number );
	      lstr.Append( ')', str );
	      IF (e.obj = enum_const) OR (e.obj = bit_field) THEN
		e.expr.ComputeExpression(res, FALSE);
		resstr:= res.GetText(FALSE); Deallocate(res);
		lstr.Append( '=', str );
		lstr.Append( resstr^, str );
	      END;
	    ELSE
	    END;
	    type.vars.FindNext( e );
	  END;
	  lstr.Append( '}', str );
	END;
	lstr.Append( ';', str );

	(* base field *)
	IF type.type = t_func THEN
	  lstr.Append( ':(', str );
	ELSE
	  lstr.Append( 'b(', str );
	END;
	_TranslateTypeToString( type.base, type.modifier, str, number );
	lstr.Append( ');', str );
      END;
    END;
  END _TranslateTypeToString;

  (*------------------------*)
  PROCEDURE Clean_ttts_number ( type: Type );
  VAR elm: adt.Element;
  BEGIN
    IF (type = NIL) OR (type.ttts_number = -1) THEN
      RETURN;
    ELSE
      type.ttts_number:= -1;
      type.vars.FindFirst( elm );
      WHILE elm # NIL DO
	IF elm IS TypedObject THEN
	  Clean_ttts_number( elm(TypedObject).type );
	END;
	type.vars.FindNext( elm );
      END;
      Clean_ttts_number( type.base );
    END;
  END Clean_ttts_number;

(*-----------------------*)
BEGIN
  number:= 0;
  adt.NewList(modifier);
  IF type.tobj_modifier # NIL THEN
    type.tobj_modifier.FindFirst(e);
    WHILE e # NIL DO
      CASE e(Modifier).modifier OF
	far, near, huge:
	  modifier.Insert(e);
      ELSE
      END;
      type.tobj_modifier.FindNext(e);
    END;
  END;
  _TranslateTypeToString( type, modifier, str, number );
  Clean_ttts_number( type );
  adt.Deallocate(modifier);
END TranslateTypeToString;




(*----------------------------------------------------*)
(*----------------------------------------------------*)


BEGIN
  adt.NewList(TypeStorage);
  adt.NewList(TypedObjectStorage);
  adt.NewList(ReplacementStorage);
  adt.NewList(ReplacementStorage);
  adt.NewList(UnaryOperationStorage);
  adt.NewList(BinaryOperationStorage);
  adt.NewList(TernaryOperationStorage);
  adt.NewList(ValueStorage);
  adt.NewList(ConstantValueStorage);
  adt.NewList(ModifierStorage);
  adt.NewList(M2TypeStorage);
  adt.NewList(ListForFindInSomewhere);

  adt.NewList(Headers);
  adt.NewTree(ModuleNames);
  adt.NewTree(M2Types);
  NewHeader('', SuperHeader);
  CurrentHeader:= NIL;
  file_name:= NIL;
  lstr.Assign('', file_name);

END H2DObjs.
(*
io.printf('7, token = %d, %s\n',scan_token, scan_text^);
*)
