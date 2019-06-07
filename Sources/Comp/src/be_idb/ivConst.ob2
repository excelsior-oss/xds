MODULE ivConst;
IMPORT DBAPI;
CONST
 (*Entity type constants*)
  et_module*            = 0;
  et_vardecl*           = 1;
  et_procdecl*          = 2;
  et_varusage*          = 3;
  et_proccall*          = 4;
  et_for*               = 5;
  et_while*             = 6;
  et_repeat*            = 7;
  et_assign*            = 8;
  et_bool_expr*         = 9;
  et_then_branch*       = 10;
  et_else_branch*       = 11;
  et_if*                = 12;
  et_typedecl*          = 13;--each STRUCT coresponds to some ENTITY.
  et_methodcall*        = 14;
  et_fielddecl*         = 15;
  et_constdecl*         = 16;
  et_fieldusage*        = 17;
  et_constusage*        = 18;
  et_return*            = 19;
  et_loop*              = 20;
  et_exit*              = 21;
  et_with*              = 22;
  et_expr*              = 23;
  et_elsif*             = 24;
  et_case*              = 25;
  et_case_branch*       = 26;
  et_actual_parameter*  = 27;
  et_paramdecl*         = 28; (*formal parametr*)
  et_block*             = 29; (* bodies of modules and procedures *)
  et_imported_object*   = 30; (*Designator of imported object *)
  et_import_section*    = 31;
  et_sproc*             = 32;
  et_proc_header*       = 33;
  et_deref*             = 34;
  et_with_option*       = 35;
  et_index*             = 36;
  et_varsec*            = 37;
  et_count* = 38;




et_DECLS* = {et_vardecl, et_procdecl, et_typedecl,et_fielddecl, et_constdecl, et_paramdecl};

(* entities that have objects which can be imported*)
et_IMPORTABLES* = {et_varusage, et_proccall, et_methodcall,
                  et_constusage};

et_USAGES* = et_IMPORTABLES + {et_fieldusage};

(*Entities that correspond to triads to some extent*)
et_WHOLE_OP*   = {et_assign, et_for, et_bool_expr, et_expr,
                  et_proccall, et_methodcall, et_exit, et_return};


(*Entities that aggregate entities from et_WHOLE_OP.
  Some entities from et_COMPLEX_OP belong to et_WHOLE_OP*)
et_COMPLEX_OP* = {et_while, et_repeat, et_loop, et_elsif, et_if,
                  et_then_branch, et_then_branch, et_for, et_proccall,
                  et_methodcall,et_bool_expr};

et_OP* =  et_WHOLE_OP + et_COMPLEX_OP;
 (*Relation type constants*)
  rt_DUR*         =0;  (* between vardecl(fielddecl) and varusage(fieldusage)*)
                       (* !!!InterModule relationship*)

  rt_DCR*         =1;  (* between procdecl and proccall/methodcall*)
                       (* !!!InterModule relationship*)

  rt_IMPORT*      =2;  (* one module imports another one*)
                       (* !!!InterModule relationship*)

  rt_TYPE_OF*     =3;  (*TYPE RETURNED BY PROCEDURE, TYPE of Variable, field, param*)
                       (* !!!InterModule relationship*)

  rt_BASE*        =4;  (*Base type for records, arrays and pointers*)
                       (* !!!InterModule relationship*)

  rt_METHOD*      =5;  (*Method of a record*)
                       (*Relationship within one module (one source file)*)

  rt_LOCALPROC*   =6;  (*Relationship within one module (one source file)*)

  rt_host*        =7;  (*between field declaration and record declaration*)
                       (*Relationship within one module (one source file)*)

  rt_exit*        =8;  (* exit from LOOP*)
                       (*Relationship within one module (one source file)*)

  rt_return*      =9;  (*  PROCDECL and RETURN statement*)
                       (*Relationship within one module (one source file)*)

  rt_hostvar*     =10; (* between variable declaration of record type and fieldusage*)
                       (* !!!InterModule relationship*)	
  rt_deref*       =11;
  rt_index*       =12;
  rt_usage*       =13; (* usage of imported object*)

  rt_impdef*      =14; (* declaration of imported object*)
                       (* !!!InterModule relationship*)
  rt_header*      =15;  (* between procdecl and procheader*)
                        (* !!!InterModule relationship*)
  rt_override*    =16; (* between declaration of methods (et_procdecl)*)
                       (* !!!InterModule relationship*)

  rt_count*= 17;

 (*Attribute type constants*)
  at_public*         = 0; (*an object is public*)
  at_compwarn*       = 1; (*compiler printed a warning at this entity*)
  at_self_recursive* = 2; (*a procedure is self recursive*)
  at_nest_used*      = 3; (*local is used in nested procedure*)
  at_name*           = 4; (*name of object local*)
  at_never_used*     = 5; (*imported object is never used*)
  at_modified*       = 6; (*varusage or fieldusage where object is modified*)
  at_readonly*       = 7; (* declared with "-"*)
  at_struct_param*   = 8;
  at_modified_vpar*  = 9; (* val parameter is modified*)
  at_count*= 10;

(* structure constants*)
no_parent*=DBAPI.no_parent;
BEGIN


END ivConst.