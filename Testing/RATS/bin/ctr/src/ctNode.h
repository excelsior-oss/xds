/***	Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Syntax tree structure for CTROUT(CityRout) - Compiler Testing Routine
														  Den: 10-Aug-96
*/

#ifndef __ctNode_h
#define __ctNode_h

#include "ctLimits.h"
#include "lists.h"
#include "ctScan.h"

#include <stdio.h>


typedef struct node_module_t node_module_t;
typedef struct node_proc_t   node_proc_t;
typedef struct node_param_t  node_param_t;
typedef struct node_stmt_t	 node_stmt_t;
typedef struct node_expr_t   node_expr_t;
typedef struct node_var_t    node_var_t;
typedef struct node_val_t	 node_val_t;



/* ---------------------- Values ------------------------ */

typedef enum
{
  val_undef,
  val_str,
  val_int,
  val_bool
} val_tag_t;

struct node_val_t
{
  val_tag_t vtag;
  union
  {
	char * string;
	long   integer;
    long   boolean;
  } _;
};

/* -------------------- Variables ----------------------- */

struct node_var_t
{
  long name;
  node_val_t * val;
  long protection;
};

/* -------------------- Expresions ---------------------- */

typedef enum
{
  expr_if,
  expr_lt,
  expr_le,
  expr_gt,
  expr_ge,
  expr_eq,
  expr_ne,
  expr_plus,
  expr_uplus,
  expr_minus,
  expr_uminus,
  expr_or,
  expr_div,
  expr_mul,
  expr_and,
  expr_val,
  expr_var,
  expr_funccall
} expr_tag_t;

typedef enum
{
  exprslice_none,
  exprslice_simple,
  exprslice_to,
  exprslice_for
} exprslice_tag_t;

typedef enum
{
  funccall_first,
  funccall_feature,
  funccall_nofeature,
  funccall_integer,
  funccall_string,
  funccall_size
} funccall_tag_t;

struct node_un_expr_t
{
  node_expr_t *x1;
};

struct node_bin_expr_t
{
  node_expr_t *x1, *x2;
};

struct node_tern_expr_t
{
  node_expr_t *x1, *x2, *x3;
};

struct node_funccall_expr_t
{
  funccall_tag_t ftag;
  node_expr_t  * expr;
};

struct node_expr_t
{
  expr_tag_t etag;
  union
  {
    struct node_un_expr_t         unary;
    struct node_bin_expr_t        binary;
    struct node_tern_expr_t       ternary;
    node_val_t                  * val;
    node_var_t                  * var;
    struct node_funccall_expr_t   funccall;
  } _;

  exprslice_tag_t stag;
  node_expr_t *s1, *s2;
};



/* -------------------- Statements ---------------------- */

typedef enum
{
  stmt_Assign,
  stmt_Comment,
  stmt_Compile,
  stmt_Exit,
  stmt_For,
  stmt_Foreach,
  stmt_Generate,
  stmt_If,
  stmt_Loop,
  stmt_Proccall,
  stmt_Return,
  stmt_Run,
  stmt_write
} node_tag_t;

typedef enum
{
  gen_bypass,
  gen_runok,
  gen_runerr,
  gen_compileok,
  gen_compileerr
} generate_tag_t;


struct node_assign_stmt_t
{
  node_var_t  * lvalue;
  node_expr_t * expr;
};

struct node_comment_stmt_t
{
  char * string;
};

struct node_compile_stmt_t
{
  node_expr_t * expr;
};

struct node_exit_stms_t
{ long dummy; };

struct node_for_stmt_t
{
  node_var_t  * lval;
  node_expr_t * from;
  node_expr_t * to;
  node_val_t  * to_val;
  List_t	  * body;
  long          init;
};

struct node_foreach_stmt_t
{
  List_t * lvals;
  List_t * exprs;
  List_t * body;
  long     init;
};

struct node_generate_stmt_t
{
  generate_tag_t gtag;
};

struct node_if_stmt_t
{
  node_expr_t  * cond;
  List_t	   * iftrue;
  List_t	   * iffalse;
};

struct node_loop_stmt_t
{
  List_t * body;
};

struct node_proccall_stmt_t
{
  node_proc_t * proc;
  List_t      * exprs;
};

struct node_return_stmt_t
{ long dummy; };

struct node_run_stmt_t
{
  node_expr_t * expr;
};

struct node_write_stmt_t
{
  char	   ln;
  List_t * exprs;
};

struct node_stmt_t
{
  node_tag_t stag;
  union
  {
	 struct node_assign_stmt_t	 assign;
	 struct node_comment_stmt_t  comment;
	 struct node_compile_stmt_t  compile;
	 struct node_exit_stms_t	 exit;
	 struct node_for_stmt_t 	 fors;
	 struct node_foreach_stmt_t  foreach;
         struct node_generate_stmt_t generate;
	 struct node_if_stmt_t		 ifs;
	 struct node_loop_stmt_t	 loop;
	 struct node_proccall_stmt_t proccall;
         struct node_return_stmt_t   returns;
	 struct node_run_stmt_t 	 run;
	 struct node_write_stmt_t	 write;
  } _;
};


/* -------------------- Modules ---------------------- */

typedef enum template_tag_t{
  tmpl_template,
  tmpl_cfgtemplate,
  tmpl_deftemplate,
  tmpl_stdin_template,
  tmpl_stdout_template,
  tmpl_outputs_template,
  tmpl_text_template

} template_tag_t;

typedef struct node_template_t{
  template_tag_t ttag;
  char         * suffix;
  char         * extension;
  char         * pattern;
} node_template_t;

typedef struct struct_out{
 char * outfile;
 char * txtfile;
 char * patterntxt;
} out ;

struct node_module_t{
  Scan_Scanner_t * scan;

  FILE       * report;
  int	       logFD, logFD1;
  char       * templatename;
  char	       firstName[32], lastName[32];

  unsigned int needstdin;
  unsigned int needstdout;
  char         stdinfile[150];
  char         stdoutfile[150];
  char         stdtxtfile[150];
  out        * outputfiles;
  char       * textfiles[50];
  int          outputcnt;
  int          textcnt;
  char       * execfile;

  char	     * testprefix;
  List_t     * templates;
  char       * compile_options;
  char	     * comment;
  List_t     * procedures;
  List_t     * globals;
  List_t     * locals;
  List_t     * body;

  char         gen_run, gen_compile;
};

/* -------------------- Procedures ---------------------- */

typedef enum{
  param_value,
  param_reference
} param_tag_t;

struct node_proc_t{
  long      name;
  List_t  * params;
  List_t  * locals;
  List_t  * body;
};

struct node_param_t{
  param_tag_t  ptag;
  node_var_t * var;
};



/* -------------------- Functions protoypes ---------------------- */

extern void                ctNode_Init          ( void );


extern List_t           *  SL_Create    ( void );
extern void                SL_Destroy   ( List_t * l );
extern void                SL_Insert    ( List_t * l, node_stmt_t * stmt );
extern node_stmt_t      *  SL_GetFirst  ( List_t * l );
extern node_stmt_t      *  SL_GetNext   ( List_t * l );
extern long                SL_SaveState ( List_t * l );
extern void                SL_RestoreState ( List_t * l, long state );

extern List_t           *  VL_Create    ( void );
extern void                VL_Destroy   ( List_t * l );
extern void                VL_Insert    ( List_t * l, node_var_t * var );
extern node_var_t       *  VL_GetFirst  ( List_t * l );
extern node_var_t       *  VL_GetNext   ( List_t * l );
extern node_var_t       *  VL_Get       ( List_t * l, long name );

extern List_t           *  EL_Create    ( void );
extern void                EL_Destroy   ( List_t * l );
extern void                EL_Insert    ( List_t * l, node_expr_t * expr );
extern node_expr_t      *  EL_GetFirst  ( List_t * l );
extern node_expr_t      *  EL_GetNext   ( List_t * l );

extern List_t           *  PL_Create    ( void );
extern void                PL_Destroy   ( List_t * l );
extern void                PL_Insert    ( List_t * l, node_proc_t * proc );
extern node_proc_t      *  PL_GetFirst  ( List_t * l );
extern node_proc_t      *  PL_GetNext   ( List_t * l );
extern node_proc_t      *  PL_Get       ( List_t * l, long name );

extern List_t           *  PML_Create   ( void );
extern void                PML_Destroy  ( List_t * l );
extern void                PML_Insert   ( List_t * l, node_param_t * param );
extern node_param_t     *  PML_GetFirst ( List_t * l );
extern node_param_t     *  PML_GetNext  ( List_t * l );

extern List_t           *  TL_Create    ( void );
extern void                TL_Destroy   ( List_t * l );
extern void                TL_Insert    ( List_t * l, node_template_t * template );
extern node_template_t  *  TL_GetFirst  ( List_t * l );
extern node_template_t  *  TL_GetNext   ( List_t * l );

#endif /* __ctNode_h */
