/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Syntax analyzer for CTRout(CityRout) - Compiler Testing Routine
												   Alexs: 4-Nov-96
*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <limits.h>
#include <string.h>

#include "ctParse.h"
#include "ctScan.h"
#include "ctStrs.h"
#include "ctLimits.h"
#include "ctMagic.h"
#include "ctAssert.h"
#include "ctErrors.h"
#include "ctMemory.h"


DeclareMagic(CTPARSE_MAGIC);


#define MAX_OutFile               10
#define MAGIC_ASSERT(x)           ASSERT ( ( (x)->magic == CTPARSE_MAGIC ) );
#define ERROR(no, addinfo)        Errors_Error((no), Scan_Line(p->scan), addinfo, p->report)

#define Symbol			  (Scan_Symbol(p->scan))
#define NextSymbol		  (Scan_Skip(p->scan),((Symbol == sym_ERROR)?(ERROR(Scan_ErrorCode(p->scan),0),0):Symbol))
#define SkipExpected(what)        ((Symbol == what)?(NextSymbol):(ERROR(prerr_Expected, Scan_SymName(what)),0))
#define SkipOptional(what)	  ((Symbol == what)?(NextSymbol,1):(0))
#define Expected(what)		  ((Symbol == what)?(Symbol):(ERROR(prerr_Expected, Scan_SymName(what)),0))
#define String(str) 		  (ctStrs_COPY((str), Scan_StringValue(p->scan)))
#define Integer                   (Scan_IntegerValue(p->scan))
#define IdentName                 (Scan_IdentName(p->scan))


struct Parse_Parser_t{
  long			   magic;
  Scan_Scanner_t * scan;

  FILE           * report;
  char           * templatename;

  char			 * testprefix;
  node_module_t  * module;
  List_t         * vars;

  char             gen_run, gen_compile;
};

/*--------------------------------------------------*/
void Parse_Statements( Parse_Parser_t * p, List_t * stmts);
node_expr_t * Parse_Expression( Parse_Parser_t * p, char optional);


/*--------------------------------------------------*/
node_var_t * DeclareVariable(long name, List_t * vars){
  node_var_t * var;

  var = VL_Get(vars, name);
  if (!var){
    var = allocate(node_var_t);
    var->val = allocate(node_val_t);
    var->name = name;
    var->val->vtag = val_undef;
    VL_Insert(vars, var);
  };
  return var;
};


/*--------------------------------------------------*/
unsigned char SkipOptionalRealtion( Parse_Parser_t * p, int * operation ){
  unsigned char ret_code = 1;
  switch (Symbol){
    case sym_Eq: *operation = expr_eq; /* "="  */ break;
    case sym_Ne: *operation = expr_ne; /* "#"  */ break;
    case sym_Gt: *operation = expr_gt; /* ">"  */ break;
    case sym_Lt: *operation = expr_lt; /* "<"  */ break;
    case sym_Ge: *operation = expr_ge; /* ">=" */ break;
    case sym_Le: *operation = expr_le; /* "<=" */ break;
    default    : ret_code   = 0;
  };
  if (ret_code) NextSymbol;
  return ret_code;
};

/*--------------------------------------------------*/
unsigned char SkipOptionalAddOperator( Parse_Parser_t * p, int * operation ){
  unsigned char ret_code = 1;
  switch (Symbol){
    case sym_Plus : *operation = expr_plus;   break;
    case sym_Minus: *operation = expr_minus;  break;
    case sym_Or   : *operation = expr_or;     break;
    default       : ret_code   = 0;
  };
  if (ret_code) NextSymbol;
  return ret_code;
};

/*--------------------------------------------------*/
unsigned char SkipOptionalMulOperator( Parse_Parser_t * p, int * operation ){
  unsigned char ret_code = 1;
  switch (Symbol){
    case sym_Mul: *operation = expr_mul;   break;
    case sym_Div: *operation = expr_div;  break;
    case sym_And: *operation = expr_and;     break;
    default       : ret_code   = 0;
  };
  if (ret_code) NextSymbol;
  return ret_code;
};

/*--------------------------------------------------*/
void Parse_FuncCall( Parse_Parser_t * p, node_expr_t * expr ){

  switch (Symbol){
    case sym_First    : expr->_.funccall.ftag = funccall_first;     break;
    case sym_Feature  : expr->_.funccall.ftag = funccall_feature;   break;
    case sym_Integer  : expr->_.funccall.ftag = funccall_integer;   break;
    case sym_Nofeature: expr->_.funccall.ftag = funccall_nofeature; break;
    case sym_Size     : expr->_.funccall.ftag = funccall_size;      break;
    case sym_String   : expr->_.funccall.ftag = funccall_string;    break;
  };
  NextSymbol;
  SkipExpected(sym_Lparen);
  expr->_.funccall.expr = Parse_Expression(p,0);
  SkipExpected(sym_Rparen);
};

/*--------------------------------------------------*/
node_expr_t * Parse_Factor( Parse_Parser_t * p ){
  node_expr_t * expr;

  expr = allocate(node_expr_t);
  switch (Symbol){
    case sym_Ok:
    case sym_Error:
    case sym_True:
    case sym_False:       expr->etag = expr_val;
                          expr->_.val = allocate(node_val_t);
                          expr->_.val->vtag = val_bool;
                          expr->_.val->_.boolean = Symbol == sym_True || Symbol == sym_Ok;
                          NextSymbol;
                          break;
    case sym_Stringval  : expr->etag = expr_val;
                          expr->_.val = allocate(node_val_t);
                          expr->_.val->vtag = val_str;
                          String(expr->_.val->_.string);
                          NextSymbol;
                          break;
    case sym_Integerval : expr->etag = expr_val;
                          expr->_.val = allocate(node_val_t);
                          expr->_.val->vtag = val_int;
                          expr->_.val->_.integer = Integer;
                          NextSymbol;
                          break;
    case sym_Ident      : expr->etag = expr_var;
                          if (!(expr->_.var = VL_Get(p->vars, IdentName))) ERROR(prerr_UndeclaredIdent,0);
                          NextSymbol;
                          break;
    case sym_First      :
    case sym_Feature    :
    case sym_Integer    :
    case sym_Nofeature  :
    case sym_Size       :
    case sym_String     : expr->etag = expr_funccall;
                          Parse_FuncCall(p, expr);
                          break;
    case sym_Lparen     : NextSymbol;
                          expr = Parse_Expression(p,0);
                          SkipExpected(sym_Rparen);
                          break;
    default             : expr = NULL;
  };

  if (expr) {
    if ( SkipOptional(sym_Lbrack) ){
      expr->s1 = Parse_Expression(p, 0);
      if ( SkipOptional(sym_For) ){
        expr->stag = exprslice_for;
        expr->s2   = Parse_Expression(p, 0);
      }else if ( SkipOptional(sym_To) ){
        expr->stag = exprslice_to;
        expr->s2   = Parse_Expression(p, 1);
      }else{
        expr->stag = exprslice_simple;
      };
      SkipExpected(sym_Rbrack);
    }else{
      expr->stag = exprslice_none;
    };
  };
  return expr;
};

/*--------------------------------------------------*/
node_expr_t * Parse_Term( Parse_Parser_t * p ){
  node_expr_t * expr = NULL;
  node_expr_t * bin  = NULL;
  int           operation;

  expr = Parse_Factor(p);
  if (expr){
    while(SkipOptionalMulOperator(p,&operation)){
      bin = allocate(node_expr_t);
      bin->etag      = operation;
      bin->_.binary.x1 = expr;
      bin->_.binary.x2 = Parse_Factor(p);
      expr = bin;
    };
  };

  return expr;
};

/*--------------------------------------------------*/
node_expr_t * Parse_VerySimpleExpression( Parse_Parser_t * p ){
  node_expr_t * expr = NULL;
  node_expr_t * bin  = NULL;
  node_expr_t * un   = NULL;
  int           operation = -1;

  switch (Symbol){
    case sym_Plus : operation = expr_uplus;  break;
    case sym_Minus: operation = expr_uminus; break;
  };
  if (operation != -1){
    un = allocate(node_expr_t);
    un->etag = operation;
    NextSymbol;
  };

  expr = Parse_Term(p);

  if (un){
    if (expr){
      un->_.unary.x1 = expr;
      expr = un;
    }else{
      ERROR(prerr_Expected, "expression");
    };
  };

  if (expr){
    while(SkipOptionalAddOperator(p,&operation)){
      bin = allocate(node_expr_t);
      bin->etag      = operation;
      bin->_.binary.x1 = expr;
      bin->_.binary.x2 = Parse_Term(p);
      expr = bin;
    };
  };

  return expr;
};

/*--------------------------------------------------*/
node_expr_t * Parse_SimpleExpression( Parse_Parser_t * p ){
  node_expr_t * expr = NULL;
  node_expr_t * bin  = NULL;
  int           operation;

  expr = Parse_VerySimpleExpression(p);
  if (expr && SkipOptionalRealtion(p,&operation)){
    bin = allocate(node_expr_t);
    bin->etag      = operation;
    bin->_.binary.x1 = expr;
    bin->_.binary.x2 = Parse_SimpleExpression(p);
    expr = bin;
  };
  return expr;
};

/*--------------------------------------------------*/
node_expr_t * Parse_Expression( Parse_Parser_t * p, char optional){
  node_expr_t * expr = NULL;
  node_expr_t * tern = NULL;

  expr = Parse_SimpleExpression(p);
  if (expr && SkipOptional(sym_Question)){
    tern = allocate(node_expr_t);
    tern->etag       = expr_if;
    tern->_.ternary.x1 = expr;
    tern->_.ternary.x2 = Parse_Expression(p,0);
    SkipExpected(sym_Colon);
    tern->_.ternary.x3 = Parse_Expression(p,0);
    expr = tern;
  };
  if ( (expr == NULL) && (!optional) ) ERROR(prerr_Expected, "expression");
  return expr;
};

/*--------------------------------------------------*/
void Parse_Comment( Parse_Parser_t * p, node_stmt_t * node){
  NextSymbol;
  node->stag = stmt_Comment;
  Expected(sym_Stringval);
  String(node->_.comment.string);
//  String (  p->module->comment );  // adb 
  NextSymbol;
};

/*--------------------------------------------------*/
void Parse_Compile( Parse_Parser_t * p, node_stmt_t * node){
  NextSymbol;
  node->stag = stmt_Compile;
  SkipExpected(sym_Lparen);
  node->_.compile.expr = Parse_Expression(p,0);
  SkipExpected(sym_Rparen);
};

/*--------------------------------------------------*/
void Parse_Exit( Parse_Parser_t * p, node_stmt_t * node){
  NextSymbol;
  node->stag = stmt_Exit;
};

/*--------------------------------------------------*/
void Parse_For( Parse_Parser_t * p, node_stmt_t * node){

  NextSymbol;
  node->stag = stmt_For;
  Expected(sym_Ident);
  node->_.fors.lval = DeclareVariable(IdentName, p->vars);
  NextSymbol;
  SkipExpected(sym_Becomes);
  node->_.fors.from = Parse_Expression(p,0);
  SkipExpected(sym_To);
  node->_.fors.to   = Parse_Expression(p,0);
  node->_.fors.to_val = allocate( node_val_t);
  SkipExpected(sym_Do);
  node->_.fors.body = SL_Create();
  Parse_Statements(p, node->_.fors.body);
  node->_.fors.init = 1;
  SkipExpected(sym_End);
};

/*--------------------------------------------------*/
void Parse_Foreach( Parse_Parser_t * p, node_stmt_t * node){

  NextSymbol;
  node->stag = stmt_Foreach;
  node->_.foreach.lvals = VL_Create();
  node->_.foreach.exprs = EL_Create();
  node->_.foreach.body  = SL_Create();
  node->_.foreach.init  = 1;

  Expected(sym_Ident);
  VL_Insert(node->_.foreach.lvals, DeclareVariable(IdentName, p->vars));
  while (NextSymbol == sym_Comma){
    NextSymbol;
    Expected(sym_Ident);
    VL_Insert(node->_.foreach.lvals, DeclareVariable(IdentName, p->vars));
  };
  SkipExpected(sym_In);

  EL_Insert(node->_.foreach.exprs, Parse_Expression(p,0));
  while( SkipOptional(sym_Comma) ){
    EL_Insert(node->_.foreach.exprs, Parse_Expression(p,0));
  };

  SkipExpected(sym_Do);
  Parse_Statements(p, node->_.foreach.body);
  SkipExpected(sym_End);
};

/*--------------------------------------------------*/
void Parse_Generate( Parse_Parser_t * p, node_stmt_t * node){

   NextSymbol;
   node->stag = stmt_Generate;
   switch (Symbol){
     case sym_Bypass      : node->_.generate.gtag = gen_bypass;
                            break;
     case sym_Compileok   : node->_.generate.gtag = gen_compileok;
                            p->gen_compile = 1;
                            break;
     case sym_Compileerror: node->_.generate.gtag = gen_compileerr;
                            p->gen_compile = 1;
                            break;
     case sym_Runerror    : node->_.generate.gtag = gen_runerr;
                            p->gen_run = 1;
                            break;
     case sym_Runok       : node->_.generate.gtag = gen_runok;
                            p->gen_run = 1;
                            break;
     default              : ERROR(prerr_Expected, "generate result");
   };
   NextSymbol;
};

/*--------------------------------------------------*/
void Parse_If( Parse_Parser_t * p, node_stmt_t * node){
  node_stmt_t * stmt;

  NextSymbol;
  node->stag        = stmt_If;
  node->_.ifs.cond    = Parse_Expression(p,0);
  node->_.ifs.iftrue  = SL_Create();
  node->_.ifs.iffalse = SL_Create();

  SkipExpected(sym_Then); Parse_Statements(p, node->_.ifs.iftrue);

  while( SkipOptional(sym_Elsif) ){
    stmt = allocate( node_stmt_t );
    stmt->stag        = stmt_If;
    stmt->_.ifs.cond    = Parse_Expression(p,0);
    stmt->_.ifs.iftrue  = SL_Create();
    stmt->_.ifs.iffalse = SL_Create();
    SL_Insert( node->_.ifs.iffalse, stmt );
    node = stmt;
    SkipExpected(sym_Then); Parse_Statements(p, node->_.ifs.iftrue);
  };

  if ( SkipOptional(sym_Else) ) Parse_Statements(p, node->_.ifs.iffalse);

  SkipExpected(sym_End);
};

/*--------------------------------------------------*/
void Parse_Loop( Parse_Parser_t * p, node_stmt_t * node){
  NextSymbol;
  node->stag = stmt_Loop;
  node->_.loop.body = SL_Create();
  Parse_Statements(p, node->_.loop.body);
  SkipExpected(sym_End);
};

/*--------------------------------------------------*/
void Parse_Return( Parse_Parser_t * p, node_stmt_t * node){
  NextSymbol;
  node->stag = stmt_Return;
};

/*--------------------------------------------------*/
void Parse_Run( Parse_Parser_t * p, node_stmt_t * node){
  NextSymbol;
  node->stag = stmt_Run;
  SkipExpected(sym_Lparen);
  node->_.run.expr = Parse_Expression(p,0);
  SkipExpected(sym_Rparen);
};

/*--------------------------------------------------*/
void Parse_Write( Parse_Parser_t * p, node_stmt_t * node, char ln){
  node_expr_t * expr = NULL;

  NextSymbol;
  node->stag = stmt_write;
  node->_.write.ln = ln;
  node->_.write.exprs = EL_Create();
  SkipExpected(sym_Lparen);
  expr = Parse_Expression(p,1);
  while (expr){
    EL_Insert(node->_.write.exprs, expr);
	expr = SkipOptional(sym_Comma) ? Parse_Expression(p,0) : NULL;
  };
  SkipExpected(sym_Rparen);
};

/*--------------------------------------------------*/
void Parse_ProcCall( Parse_Parser_t * p, node_stmt_t * node, long name){
  node_expr_t  * expr  = NULL;
  node_proc_t  * proc  = NULL;
  node_param_t * param = NULL;

  if ( NULL == (proc = PL_Get(p->module->procedures, name)) ) ERROR(prerr_UndeclaredIdent,0);
  node->stag = stmt_Proccall;
  node->_.proccall.proc  = proc;
  node->_.proccall.exprs = EL_Create();
  SkipExpected(sym_Lparen);
  param = PML_GetFirst( proc->params );
  if (param){
    expr = Parse_Expression(p,0);
    EL_Insert(node->_.proccall.exprs, expr);
    if ( param->ptag == param_reference && expr->etag != expr_var ) ERROR(prerr_ActualParamMustBeLValue,0);
    param = PML_GetNext( proc->params );
    while (param){
      SkipExpected(sym_Comma);
      expr = Parse_Expression(p,0);
      EL_Insert(node->_.proccall.exprs, expr);
      param = PML_GetNext( proc->params );
    };
  };
  SkipExpected(sym_Rparen);
};

/*--------------------------------------------------*/
void Parse_ProcDecls( Parse_Parser_t * p, List_t * procs){
  node_proc_t  * proc;
  node_param_t * param;
  node_stmt_t  * ret;

  while ( Symbol == sym_Procedure ){
    NextSymbol;
    Expected(sym_Ident);
    if ( NULL != PL_Get(p->module->procedures, IdentName) ) ERROR(prerr_DuplicateIdent,0);
    proc = allocate(node_proc_t);
    proc->name = IdentName;
    proc->params = PML_Create();
    proc->body   = SL_Create();
    PL_Insert(p->module->procedures, proc);
    NextSymbol;
    p->vars = VL_Create();
    if ( SkipOptional(sym_Lparen) ){
      while (1){
        param = allocate(node_param_t);
        param->ptag = SkipOptional(sym_Var) ? param_reference : param_value;
        Expected(sym_Ident);
        if ( NULL != VL_Get(p->vars, IdentName) ) ERROR(prerr_DuplicateIdent,0);
        param->var  = DeclareVariable(IdentName, p->vars);
        PML_Insert(proc->params, param);
        NextSymbol;
        if (!SkipOptional(sym_Comma) && (Symbol == sym_Rparen)) break;
      };
      NextSymbol;
    };
    SkipExpected(sym_Semicolon);
    SkipExpected(sym_Begin);
    Parse_Statements(p, proc->body);
    ret = allocate(node_stmt_t);
    ret->stag = stmt_Return;
    SL_Insert(proc->body, ret);
    SkipExpected(sym_End);
    proc->locals = p->vars; p->vars = NULL;
  };
};

/*--------------------------------------------------*/
void Parse_Statements( Parse_Parser_t * p, List_t * stmts){
  char go_on = 1;
  node_stmt_t * n = NULL;
  long name;

  while (go_on){
	n = allocate(node_stmt_t);
    switch (Symbol){
      case sym_Comment : Parse_Comment(p, n);       break;
      case sym_Compile : Parse_Compile(p, n);       break;
      case sym_Exit    : Parse_Exit(p, n);          break;
      case sym_For     : Parse_For(p, n);           break;
      case sym_Foreach : Parse_Foreach(p, n);       break;
      case sym_Generate: Parse_Generate(p, n);      break;
      case sym_Ident   : name = IdentName;
                         NextSymbol;
                         if ( SkipOptional(sym_Becomes) ){
                           n->stag = stmt_Assign;
                           n->_.assign.lvalue = DeclareVariable(name, p->vars);
                           n->_.assign.expr   = Parse_Expression(p,0);
                         }else{
                           Parse_ProcCall(p, n, name);
                         };
                                                    break;
      case sym_If      : Parse_If(p, n);            break;
      case sym_Loop    : Parse_Loop(p, n);          break;
      case sym_Return  : Parse_Return(p, n);        break;
      case sym_Run     : Parse_Run(p, n);           break;
      case sym_Write   : Parse_Write(p, n, 0);      break;
      case sym_Writeln : Parse_Write(p, n,1);       break;
	  default		   : go_on = 0; n = NULL;
	};
    if (n != NULL) {
      SL_Insert(stmts, n);
      SkipExpected(sym_Semicolon);
    };
  };
};

/*--------------------------------------------------*/
node_module_t * Parse_Module ( Parse_Parser_t * p ){
  node_module_t   * module   = NULL;
  node_template_t * template = NULL;

  MAGIC_ASSERT( p );

  if (Symbol == sym_Eof) return module;

  p->gen_run = p->gen_compile = 0;

  Scan_ResetIdents( p->scan );


  if ((p->testprefix == NULL) && (SkipExpected(sym_Testprefix),1) ||
	  SkipOptional(sym_Testprefix))
  {
    Expected(sym_Stringval); String(p->testprefix); NextSymbol;
  };
  module = allocate( node_module_t );
  module->compile_options = NULL;
  ctStrs_COPY( module->testprefix, p->testprefix );
  module->templates   = TL_Create();
  module->needstdin   = 0;
  module->needstdout  = 0;
  module->outputcnt   = 0; 
  module->textcnt     = 0; 
  module->outputfiles = _allocate(out*, sizeof(out *) * MAX_OutFile );

  while( (Symbol == sym_Cfgtemplate) || (Symbol == sym_Deftemplate) ||
 (Symbol == sym_Template) || (Symbol == sym_Stdin) || 
 (Symbol == sym_Stdout) || (Symbol == sym_Outputs) || (Symbol == sym_Text)){

    template = allocate(node_template_t);
    if ( SkipOptional(sym_Text) ){
      template->ttag = tmpl_text_template;
      module->textcnt++;
    }else if ( SkipOptional(sym_Cfgtemplate) ){
      template->ttag = tmpl_cfgtemplate;
    }else if ( SkipOptional(sym_Deftemplate) ){
      template->ttag = tmpl_deftemplate;
    }else if ( SkipOptional(sym_Outputs) ){
      template->ttag = tmpl_outputs_template;
      Expected(sym_Stringval);
      String(module->outputfiles[module->outputcnt].patterntxt); NextSymbol;
      module->outputcnt++;
    }else if ( SkipOptional(sym_Stdin) ){
      template->ttag = tmpl_stdin_template;
      if (module->needstdin){ ERROR(scerr_Duplicate_Stdin,""); };
      module->needstdin=1;
    }else if ( SkipOptional(sym_Stdout) ){
      template->ttag = tmpl_stdout_template;
      if (module->needstdout){ ERROR(scerr_Duplicate_Stdout,""); };
      module->needstdout=1;
    }else{
      SkipExpected(sym_Template);
      template->ttag = tmpl_template;
    };
    if ( SkipOptional(sym_Suffix) ){
      Expected(sym_Stringval); String(template->suffix); NextSymbol;
     };
    if ( SkipOptional(sym_Extension) ){
      Expected(sym_Stringval); String(template->extension); NextSymbol;
    }else if (template->ttag == tmpl_text_template){
      ctStrs_COPY(template->extension, "dat");
    }else if (template->ttag == tmpl_deftemplate){
      ctStrs_COPY(template->extension, "def");
    }else if (template->ttag == tmpl_stdin_template){
      ctStrs_COPY(template->extension, "in");
    }else if (template->ttag == tmpl_stdout_template){
      ctStrs_COPY(template->extension, "out");
    }else if (template->ttag == tmpl_outputs_template){
      ctStrs_COPY(template->extension, "ots");
	};

    Expected(sym_Stringval); String(template->pattern); NextSymbol;
    TL_Insert(module->templates, template);
	if (module->outputcnt > MAX_OutFile ) ERROR(scerr_TooManyOutFiles,"");
  };
 if ( SkipOptional(sym_Comment) ){
    Expected(sym_Stringval); String(module->comment); NextSymbol;
  };

  module->outputcnt = 0;
  p->module = module;
  module->report = p->report;
  module->logFD = module->logFD1 = -1;
  module->firstName[0] = module->lastName[0] = 0;
  ctStrs_COPY( module->templatename, p->templatename );
  module->scan = p->scan;
  module->procedures = PL_Create();
  module->body       = SL_Create();
  Parse_ProcDecls(p, module->procedures);
  SkipExpected(sym_Begin);
  p->vars = VL_Create();
  DeclareVariable( Scan_SetIdName( p->scan, "name"), p->vars );
  DeclareVariable( Scan_SetIdName( p->scan, "header"), p->vars );
  DeclareVariable( Scan_SetIdName( p->scan, "suffix"), p->vars );
  Parse_Statements(p, module->body);
  module->globals = p->vars; p->vars = NULL;
  SkipExpected(sym_End);
  p->module = NULL;

  module->gen_compile = p->gen_compile;
  module->gen_run     = p->gen_run;

  return module;
};

/*--------------------------------------------------*/
Parse_Parser_t * Parse_Init ( char * name, char * reportname ){
  Parse_Parser_t * p;
  Scan_Scanner_t * s;
  FILE           * report;

  ctNode_Init();

  if ( NULL == ( s = Scan_Open( name ) ) ) return NULL;
  if ( NULL == ( report = fopen( reportname, "w" ) ) ) { printf("Can't open report file %s",reportname); return NULL; }
  p = allocate( Parse_Parser_t );
  p->scan   = s;
  p->report = report;
  ctStrs_COPY( p->templatename, name );
  p->magic = SetMagic(CTPARSE_MAGIC);

  return p;
};

/*--------------------------------------------------*/
void Parse_Final ( Parse_Parser_t * p ){
  MAGIC_ASSERT( p );
  Scan_Close( p->scan );
  fclose( p->report );
};

/*--------------------------------------------------*/
