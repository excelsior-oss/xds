/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Interpreter for CTROUT(CityRout) - Compiler Testing Routine
                                                                                          Alexs: 11-Nov-96
*/

#include "ctAssert.h"
#include "ctErrors.h"
#include "ctErrCodes.h"
#include "ctMemory.h"
#include "ctMagic.h"
#include "ctStrs.h"
#include "ctCfg.h"
#include "ctExec.h"
#include "ctInterpret.h"
#include "stacks.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


DeclareMagic(CTINTERPRET_MAGIC);
DeclareMagic(StackMagic);


#define MAGIC_ASSERT(x)           ASSERT ( ( (x)->magic == CTINTERPRET_MAGIC ) );
#define ERROR(no, addinfo)        Errors_Error((no), 153, addinfo, i->module->report)






struct Interpret_Interpereter_t{
  long magic;

  Cfg_Configurator_t * cfg;
  Exec_Executor_t    * exec;
  Stack_t            * stack;
  node_module_t      * module;

};

typedef enum{
  state_basic,
  state_return,
  state_exit
} state_tag_t;

typedef enum{
  stmtstate_inprogress,
  stmtstate_done
} stmt_state_t;

typedef struct state_node_t{
  state_tag_t   stag;
  stmt_state_t  sstate;
  node_stmt_t * statement;
  List_t          * statements;
  long          stmt_list_state;
  List_t      * vars;
  List_t      * locals;
} state_node_t;

/*----------------------------------------------------*/
Stack_t * CreateStack(void){
  return Stack_Create( StackMagic );
};

/*----------------------------------------------------*/
void DestroyStack( Stack_t * s ){
  Stack_Destroy( s, StackMagic );
};

/*----------------------------------------------------*/
void Push( Stack_t * s, state_tag_t stag, List_t * statements, List_t * locals ){
  node_stmt_t  * statement   = NULL;
  state_node_t * state       = NULL;
  List_t       * prev_locals = NULL ;

  if ( state = (state_node_t*)Stack_Top( s, StackMagic ) ){
    state->stmt_list_state = SL_SaveState( state->statements );
    prev_locals = state->locals;
  };

  state = allocate(state_node_t);
  state->stag           = stag;
  statement = SL_GetFirst( statements );
  if ( !statement ){
    statement = allocate( node_stmt_t );
    state->sstate = stmtstate_done;
  }else{
    state->sstate = stmtstate_inprogress;
  };
  state->locals = locals ? locals : prev_locals;
  state->statement  = statement;
  state->statements = statements;
  state->stmt_list_state = SL_SaveState( statements );
  Stack_Push( s, StackMagic, state );
};

/*----------------------------------------------------*/
state_node_t * Pop( Stack_t * s ){
  return (state_node_t *) Stack_Pop( s, StackMagic );
};

/*----------------------------------------------------*/
state_node_t * Top( Stack_t * s ){
  return (state_node_t *) Stack_Top( s, StackMagic );
};


/*----------------------------------------------------*/
int  numbers[100];
int  nnumbers;
long getAction(Interpret_Interpereter_t * i, long run_action){

  int j, r;

  if (nnumbers){
    for(r=j=0; j < nnumbers; j++) r = r || i->exec->number == numbers[j];
    if (!r) return Exec_DoNone;
  };

  switch(Cfg_GetTest( i->cfg )){
    case ctCfg_TestComment: return Exec_DoNone;
    case ctCfg_TestCompile: return Exec_DoCompile;
    case ctCfg_TestCompare: return Exec_DoCompare;
    case ctCfg_TestAll:     return run_action ? Exec_DoRun : Exec_DoCompile;
    case ctCfg_TestRun:     return Exec_DoRun;
  };
  return -1;
};

/*----------------------------------------------------*/
void Interpret_CalculateExpression( Interpret_Interpereter_t * i, node_expr_t * expr, node_val_t * val );


/*----------------------------------------------------*/
void Interpret_CopyValue( node_val_t * dest, node_val_t * src ){

  if ( dest == src ) return;
  if ( dest->vtag == val_str ){
    dest->_.string = NULL;
  };
  dest->vtag = src->vtag;
  if ( src->vtag == val_int ){
    dest->_.integer = src->_.integer;
  }else if ( src->vtag == val_bool ){
    dest->_.boolean = src->_.boolean;
  }else if ( src->vtag == val_str ){
    ctStrs_COPY( dest->_.string, src->_.string );
  };
};

/*----------------------------------------------------*/
void Interpret_CalculateFuncCall( Interpret_Interpereter_t * i, node_expr_t * expr, node_val_t * val ){
  node_val_t val1 = {val_undef, 0};
  long l = 0, j = 0, sign = 0;
  char str[100] = "";

  Interpret_CalculateExpression( i, expr->_.funccall.expr, &val1 );
  switch(expr->_.funccall.ftag){
    case funccall_first     :
            val->vtag = val_str;
            if ( val1.vtag == val_str ){
              ctStrs_COPY( val->_.string, val1._.string );
              while( val->_.string[l] == ' ' ) l++;
              while( val->_.string[l] && val->_.string[l] != ' ' ) {
                val->_.string[j++] =  val->_.string[l++];
              };
              val->_.string[j] = 0;
            }else{
              ERROR(inerr_TypeMismatch,0);
            };
            break;
    case funccall_feature   :
            if ( val1.vtag == val_str ) {
              val->vtag = val_bool;
              val->_.boolean = Cfg_Feature( i->cfg, val1._.string );
            }else{
              ERROR(inerr_TypeMismatch,0);
            };
            break;
    case funccall_nofeature :
            if ( val1.vtag == val_str ) {
              val->vtag = val_bool;
              val->_.boolean = Cfg_Nofeature( i->cfg, val1._.string );
            }else{
              ERROR(inerr_TypeMismatch,0);
            };
            break;
    case funccall_integer   :
            val->vtag = val_int;
            if ( val1.vtag == val_str ){
              val->_.integer = strtol(val1._.string, NULL, 10);
/*              if (errno) ERROR(inerr_BadConstant,0); */
            }else{
              ERROR(inerr_TypeMismatch,0);
            };
            break;
    case funccall_string    :
            val->vtag = val_str;
            if ( val1.vtag == val_int ){
              l = 99;
              str[l--] = 0;
              sign = val1._.integer < 0;
              if (sign) val1._.integer = -val1._.integer;
              while( val1._.integer ){
                str[l--] = (val1._.integer % 10) + '0';
                val1._.integer = val1._.integer / 10;
              };
              if (sign) str[l] = '-';
              else      l++;
              j = 0;
              while ( l < 99 ) str[j++] = str[l++];
              str[j] = 0;
              ctStrs_COPY( val->_.string, str );
            }else{
              ERROR(inerr_TypeMismatch,0);
            };
            break;
    case funccall_size      :
            if ( val1.vtag == val_str ) {
              val->vtag = val_int;
              val->_.integer = Cfg_Size( i->cfg, val1._.string );
            }else{
              ERROR(inerr_TypeMismatch,0);
            };
            break;
  };
};

/*----------------------------------------------------*/
void Interpret_CalculateExpression( Interpret_Interpereter_t * i, node_expr_t * expr, node_val_t * result ){
  node_val_t val  = {val_undef, 0};
  node_val_t val1 = {val_undef, 0};
  node_val_t val2 = {val_undef, 0};
  node_val_t val3 = {val_undef, 0};
  long l;

  switch (expr->etag){
    case expr_if:
            Interpret_CalculateExpression(i, expr->_.ternary.x1, &val1);
            if ( val1.vtag == val_bool ) {
              Interpret_CalculateExpression( i, val1._.boolean ? expr->_.ternary.x2 : expr->_.ternary.x3, &val );
            }else{
              ERROR(inerr_TypeMismatch,0);
            };
            break;
    case expr_lt: case expr_le: case expr_gt: case expr_ge: case expr_eq: case expr_ne:
    case expr_plus: case expr_minus: case expr_or:
    case expr_div: case expr_mul: case expr_and:
            Interpret_CalculateExpression( i, expr->_.binary.x1, &val1);
            Interpret_CalculateExpression( i, expr->_.binary.x2, &val2);
            if (val1.vtag == val2.vtag) {
              if (val1.vtag == val_int ){
                val.vtag = val_bool;
                switch(expr->etag){
                  case expr_lt:
                    val._.boolean = val1._.integer < val2._.integer;
                    break;
                  case expr_le:
                    val._.boolean = val1._.integer <= val2._.integer;
                    break;
                  case expr_gt:
                    val._.boolean = val1._.integer > val2._.integer;
                    break;
                  case expr_ge:
                    val._.boolean = val1._.integer >= val2._.integer;
                    break;
                  case expr_eq:
                    val._.boolean = val1._.integer == val2._.integer;
                    break;
                  case expr_ne:
                    val._.boolean = val1._.integer != val2._.integer;
                    break;
                  case expr_plus:
                    val.vtag = val_int;
                    val._.integer = val1._.integer + val2._.integer;
                    break;
                  case expr_minus:
                    val.vtag = val_int;
                    val._.integer = val1._.integer - val2._.integer;
                    break;
                  case expr_div:
                    val.vtag = val_int;
                    val._.integer = val1._.integer / val2._.integer;
                    break;
                  case expr_mul:
                    val.vtag = val_int;
                    val._.integer = val1._.integer * val2._.integer;
                    break;
                  default:
                    ERROR(inerr_TypeMismatch,0);
                };
              }else if (val1.vtag == val_str) {
                val.vtag = val_bool;
                switch(expr->etag){
                  case expr_lt:
                    val._.boolean = strcmp( val1._.string, val2._.string ) < 0;
                    break;
                  case expr_le:
                    val._.boolean = strcmp( val1._.string, val2._.string ) <= 0;
                    break;
                  case expr_gt:
                    val._.boolean = strcmp( val1._.string, val2._.string ) > 0;
                    break;
                  case expr_ge:
                    val._.boolean = strcmp( val1._.string, val2._.string ) >= 0;
                    break;
                  case expr_eq:
                    val._.boolean = strcmp( val1._.string, val2._.string ) == 0;
                    break;
                  case expr_ne:
                    val._.boolean = strcmp( val1._.string, val2._.string ) != 0;
                    break;
                  case expr_plus:
                    val.vtag = val_str;
                    val._.string = _allocate(char, strlen(val1._.string) + strlen(val2._.string) + 1 );
                    sprintf(val._.string, "%s%s", val1._.string, val2._.string);
                    break;
                  case expr_minus:
                  default:
                    ERROR(inerr_TypeMismatch,0);
                    break;
                };
              }else if (val1.vtag == val_bool){
                val.vtag = val_bool;
                switch(expr->etag){
                  case expr_or:
                    val._.boolean = val1._.boolean || val2._.boolean;
                    break;
                  case expr_and:
                    val._.boolean = val1._.boolean && val2._.boolean;
                    break;
                  case expr_eq:
                    val._.boolean = val1._.boolean == val2._.boolean;
                    break;
                  case expr_ne:
                    val._.boolean = val1._.boolean != val2._.boolean;
                    break;
                  default:
                    ERROR(inerr_TypeMismatch,0);
                };
              }else{
                ERROR(inerr_TypeMismatch,0);
              };
            }else{
              ERROR(inerr_TypeMismatch,0);
            };
            break;
    case expr_uplus:
            Interpret_CalculateExpression( i, expr->_.unary.x1, &val);
            if ( val.vtag != val_int ){
              ERROR(inerr_TypeMismatch,0);
            };
            break;
    case expr_uminus:
            Interpret_CalculateExpression( i, expr->_.unary.x1, &val);
            if ( val.vtag != val_int ){
              ERROR(inerr_TypeMismatch,0);
            };
            val._.integer = -val._.integer;
            break;
    case expr_val:
            val.vtag = expr->_.val->vtag;
            if (val.vtag == val_int) {
              val._.integer = expr->_.val->_.integer;
            }else if (val.vtag == val_str) {
              ctStrs_COPY(val._.string, expr->_.val->_.string);
            }else if (val.vtag == val_bool) {
              val._.boolean = expr->_.val->_.boolean;
            }else{
              ERROR(inerr_TypeMismatch,0);
            };
            break;
    case expr_var:
            val.vtag = expr->_.var->val->vtag;
            if (val.vtag == val_int) {
              val._.integer = expr->_.var->val->_.integer;
            }else if (val.vtag == val_str) {
              ctStrs_COPY(val._.string, expr->_.var->val->_.string);
            }else if (val.vtag == val_bool) {
              val._.boolean = expr->_.var->val->_.boolean;
            }else{
              ERROR(inerr_TypeMismatch,0);
            };
            break;
    case expr_funccall:
            Interpret_CalculateFuncCall( i, expr, &val );
            break;
    default: ASSERT(0);
  };

  if ( expr->stag != exprslice_none ){
    if ( val.vtag == val_str ) {
      Interpret_CalculateExpression( i, expr->s1, &val1);
      if (val1.vtag != val_int) ERROR(inerr_TypeMismatch,0);
      if ( expr->s2 ){
        Interpret_CalculateExpression( i, expr->s2, &val2);
        if (val2.vtag != val_int) ERROR(inerr_TypeMismatch,0);
      };
      if ( expr->stag == exprslice_simple ){
        val._.string[0] = (strlen(val._.string) >= val1._.integer) ? val._.string[val1._.integer-1] : 0;
        val._.string[1] = 0;
      }else if ( expr->stag == exprslice_to ){
        if ( strlen(val._.string) < val1._.integer || val1._.integer <= 0 ||
             expr->s2 && (val1._.integer > val2._.integer))
        {
          val._.string[0] = 0;
        }else{
          l = 0;
          val2._.integer = (expr->s2) ? val2._.integer - val1._.integer + 1 : strlen(val._.string) - val1._.integer + 1;
          while( val2._.integer && val._.string[val1._.integer-1]){
            val._.string[l] = val._.string[val1._.integer-1];
            val1._.integer++; val2._.integer--; l++;
          };
          val._.string[l] = 0;
        };
      }else if ( expr->stag == exprslice_for ){
        if ( strlen(val._.string) < val1._.integer ){
          val._.string[0] = 0;
        }else{
          l = 0;
          while( val2._.integer-- && val._.string[val1._.integer-1]){
            val._.string[l++] = val._.string[(val1._.integer++)-1];
          };
          val._.string[l] = 0;
        };
      };
    }else{
    };
  };
  Interpret_CopyValue( result, &val );
};

/*----------------------------------------------------*/
void Interpret_ExecuteAssign( Interpret_Interpereter_t * i, node_stmt_t * stmt ){
  node_val_t val = {val_undef, 0};

  if ( stmt->_.assign.lvalue->protection ) ERROR(inerr_IllegalAssignment,0);
  Interpret_CalculateExpression( i,  stmt->_.assign.expr, &val);
  Interpret_CopyValue( stmt->_.assign.lvalue->val, &val );
};

/*----------------------------------------------------*/
void Interpret_ExecuteComment( Interpret_Interpereter_t * i, node_stmt_t * stmt ){

};

/*----------------------------------------------------*/
void Interpret_ExecuteCompile( Interpret_Interpereter_t * i, node_stmt_t * stmt ){
  node_val_t val = {val_undef, 0};

  if(Cfg_GetTest( i->cfg ) == ctCfg_TestCompile ||
     Cfg_GetTest( i->cfg ) == ctCfg_TestAll ||
     Cfg_GetTest( i->cfg ) == ctCfg_TestCompare ||
     Cfg_GetTest( i->cfg ) == ctCfg_TestComment)
  {
    Interpret_CalculateExpression( i,  stmt->_.compile.expr, &val );
    if ( val.vtag == val_int ){
      Exec_CompileInt( i->exec, i->module, val._.integer, getAction(i, 0));
    }else if ( val.vtag == val_bool ){
      Exec_CompileBool( i->exec, i->module, val._.boolean, getAction(i, 0));
    }else{
      ERROR(inerr_TypeMismatch,0);
    };
  };
};

/*----------------------------------------------------*/
void Interpret_ExecuteExit( Interpret_Interpereter_t * i, node_stmt_t * stmt ){
  state_node_t * state = NULL;

  state = Top( i->stack );
  while( state && state->stag == state_basic ) {
    Pop( i->stack );
    state = Top( i->stack );
  };
  if ( state && state->stag == state_exit ){
    Pop( i->stack );
    state = Top( i->stack );
    state->sstate = stmtstate_done;
  }else{
    ERROR(inerr_ExitWithoutLoop,0);
  };
};

/*----------------------------------------------------*/
void Interpret_ExecuteFor( Interpret_Interpereter_t * i, node_stmt_t * stmt ){

  if (stmt->_.fors.init) {
    stmt->_.fors.init = 0;
    stmt->_.fors.lval->protection = 1;
    Interpret_CalculateExpression( i,  stmt->_.fors.from, stmt->_.fors.lval->val );
    Interpret_CalculateExpression( i,  stmt->_.fors.to, stmt->_.fors.to_val );
    if ( stmt->_.fors.lval->val->vtag != val_int ){
      ERROR(inerr_TypeMismatch,0);
    };
    if ( stmt->_.fors.to_val->vtag != val_int ){
      ERROR(inerr_TypeMismatch,0);
    };
    Top( i->stack )->sstate = stmtstate_inprogress;
    Push( i->stack, state_basic, stmt->_.fors.body, NULL );
  }else{
    if ( ++stmt->_.fors.lval->val->_.integer > stmt->_.fors.to_val->_.integer ){
      stmt->_.fors.lval->protection = 0;
      stmt->_.fors.init = 1;
      stmt->_.fors.lval->val->_.integer--;
    }else{
      Top( i->stack )->sstate = stmtstate_inprogress;
      Push( i->stack, state_basic, stmt->_.fors.body, NULL );
    };
  };
};

/*----------------------------------------------------*/
void Interpret_ExecuteForeach( Interpret_Interpereter_t * i, node_stmt_t * stmt ){
  node_var_t  * var  = NULL;
  node_expr_t * expr = NULL;


  if (stmt->_.foreach.init) {
    stmt->_.foreach.init = 0;
    var  = VL_GetFirst( stmt->_.foreach.lvals );
    expr = EL_GetFirst( stmt->_.foreach.exprs );
    while ( var ) {
      if (!expr) ERROR(inerr_TooFewValues,0);
      var->protection = 1;
      Interpret_CalculateExpression( i,  expr, var->val );
      var  = VL_GetNext( stmt->_.foreach.lvals );
      if (var) expr = EL_GetNext( stmt->_.foreach.exprs );
    };
    Top( i->stack )->sstate = stmtstate_inprogress;
    Push( i->stack, state_basic, stmt->_.foreach.body, NULL );
  }else{
    var  = VL_GetFirst( stmt->_.foreach.lvals );
    expr = EL_GetNext ( stmt->_.foreach.exprs );
    if (expr) {
      while ( var ) {
        if (!expr) ERROR(inerr_TooFewValues,0);
        Interpret_CalculateExpression( i,  expr, var->val );
        var  = VL_GetNext( stmt->_.foreach.lvals );
        if (var) expr = EL_GetNext( stmt->_.foreach.exprs );
      };
      Top( i->stack )->sstate = stmtstate_inprogress;
      Push( i->stack, state_basic, stmt->_.foreach.body, NULL );
    }else{
      stmt->_.foreach.init = 1;
      while ( var ) {
        var->protection = 0;
        var = VL_GetNext( stmt->_.foreach.lvals );
      };
    };
  };
};

/*----------------------------------------------------*/
void Interpret_ExecuteGenerate( Interpret_Interpereter_t * i, node_stmt_t * stmt ){

  switch ( stmt->_.generate.gtag ){
    case gen_bypass     : break;
    case gen_runok      : if ( Cfg_GetTest( i->cfg ) == ctCfg_TestRun ||
                               Cfg_GetTest( i->cfg ) == ctCfg_TestAll ||
                               Cfg_GetTest( i->cfg ) == ctCfg_TestCompare ||
                               Cfg_GetTest( i->cfg ) == ctCfg_TestComment)
                          {
                            Exec_RunBool( i->exec, i->module, 1, getAction(i, 1));
                          
                          } else if (Cfg_GetTest( i->cfg ) == ctCfg_TestCompile) {
                            Exec_CompileBool( i->exec, i->module, 1, getAction(i, 0));

                          };
                          break;
    case gen_runerr     : if ( Cfg_GetTest( i->cfg ) == ctCfg_TestRun ||
                               Cfg_GetTest( i->cfg ) == ctCfg_TestAll ||
                               Cfg_GetTest( i->cfg ) == ctCfg_TestCompare ||
                               Cfg_GetTest( i->cfg ) == ctCfg_TestComment)
                          {
                            Exec_RunBool( i->exec, i->module, 0, getAction(i, 1));

                          } else if (Cfg_GetTest( i->cfg ) == ctCfg_TestCompile) {
                            Exec_CompileBool( i->exec, i->module, 1, getAction(i, 0));

                          };
                          break;
    case gen_compileok  : if ( Cfg_GetTest( i->cfg ) == ctCfg_TestCompile ||
                               Cfg_GetTest( i->cfg ) == ctCfg_TestAll ||
                               Cfg_GetTest( i->cfg ) == ctCfg_TestCompare ||
                               Cfg_GetTest( i->cfg ) == ctCfg_TestComment)
                          {
                            Exec_CompileBool( i->exec, i->module, 1, getAction(i, 0));

                          };
                          break;
    case gen_compileerr : if ( Cfg_GetTest( i->cfg ) == ctCfg_TestCompile ||
                               Cfg_GetTest( i->cfg ) == ctCfg_TestAll ||
                               Cfg_GetTest( i->cfg ) == ctCfg_TestCompare ||
                               Cfg_GetTest( i->cfg ) == ctCfg_TestComment)
                          {
                            Exec_CompileBool( i->exec, i->module, 0, getAction(i, 0));

                          };
                          break;
  };
};

/*----------------------------------------------------*/
void Interpret_ExecuteIf( Interpret_Interpereter_t * i, node_stmt_t * stmt ){
  node_val_t val;

  Interpret_CalculateExpression( i,  stmt->_.ifs.cond, &val );
  if ( (val.vtag != val_bool) && (val.vtag != val_int) ) ERROR(inerr_TypeMismatch,0);
  if ( (val.vtag == val_bool) && val._.boolean || (val.vtag == val_int) && val._.integer ){
    Push( i->stack, state_basic, stmt->_.ifs.iftrue, NULL );
  }else{
    Push( i->stack, state_basic, stmt->_.ifs.iffalse, NULL );
  };
};

/*----------------------------------------------------*/
void Interpret_ExecuteLoop( Interpret_Interpereter_t * i, node_stmt_t * stmt ){

  Top( i->stack )->sstate = stmtstate_inprogress;
  Push( i->stack, state_exit, stmt->_.loop.body, NULL );
};

/*----------------------------------------------------*/
void Interpret_ExecuteProcCall( Interpret_Interpereter_t * i, node_stmt_t * stmt ){
  node_proc_t  * proc  = NULL;
  node_param_t * param = NULL;
  node_expr_t  * expr  = NULL;
  node_var_t   * var   = NULL;
  node_var_t   * local = NULL;
  List_t       * vars  = NULL;
  state_node_t * state = NULL;


  proc  = stmt->_.proccall.proc;

  vars  = VL_Create();
  local = VL_GetFirst( proc->locals );
  while ( local ){
    var = allocate( node_var_t );
    var->val = allocate( node_val_t );
    Interpret_CopyValue( var->val, local->val );
    var->name = local->name;
    VL_Insert( vars, var );
    local = VL_GetNext( proc->locals );
  };

  param = PML_GetFirst( proc->params );
  expr  = EL_GetFirst ( stmt->_.proccall.exprs );
  while ( expr ){
    Interpret_CalculateExpression( i,  expr, param->var->val );
    param = PML_GetNext( proc->params );
    expr  = EL_GetNext ( stmt->_.proccall.exprs );
  };

  state = Top ( i->stack );
  state->vars = vars;

  Push( i->stack, state_return, proc->body, proc->locals );
};

/*----------------------------------------------------*/
void Interpret_ExecuteReturn( Interpret_Interpereter_t * i, node_stmt_t * stmt ){
  state_node_t * state = NULL;
  node_expr_t  * expr  = NULL;
  node_param_t * param = NULL;
  node_proc_t  * proc  = NULL;
  node_var_t   * var   = NULL;
  node_var_t   * local = NULL;

  state = Top( i->stack );
  while( state && state->stag == state_basic ) {
    Pop( i->stack );
    state = Top( i->stack );
  };
  if ( state && state->stag == state_return ){
    Pop( i->stack );
    state = Top( i->stack );
    proc  = state->statement->_.proccall.proc;

    param = PML_GetFirst( proc->params );
    while ( param ){
      if ( param->ptag == param_reference ){
        var = VL_Get( state->vars, param->var->name );
        Interpret_CopyValue( var->val, param->var->val );
      };
      param = PML_GetNext( proc->params );
    };

    var   = VL_GetFirst( state->vars );
    local = VL_GetFirst( proc->locals );
    while ( local ){
      Interpret_CopyValue( local->val, var->val );
      var   = VL_GetNext( state->vars );
      local = VL_GetNext( proc->locals );
    };
    VL_Destroy( state->vars );

    param = PML_GetFirst( proc->params );
    expr  = EL_GetFirst ( state->statement->_.proccall.exprs );
    while ( param ){
      if ( param->ptag == param_reference ){
        Interpret_CopyValue( expr->_.var->val, param->var->val );
      };
      param = PML_GetNext( proc->params );
      expr  = EL_GetNext ( state->statement->_.proccall.exprs );
    };
  }else{
    ERROR(inerr_ReturnWithoutCall,0);
  };
};

/*----------------------------------------------------*/
void Interpret_ExecuteRun( Interpret_Interpereter_t * i, node_stmt_t * stmt ){
  node_val_t val = {val_undef, 0};

  if(Cfg_GetTest( i->cfg ) == ctCfg_TestRun ||
     Cfg_GetTest( i->cfg ) == ctCfg_TestAll ||
     Cfg_GetTest( i->cfg ) == ctCfg_TestCompare ||
     Cfg_GetTest( i->cfg ) == ctCfg_TestComment)
  {

    Interpret_CalculateExpression( i,  stmt->_.run.expr, &val );
    if ( val.vtag == val_int ){
      Exec_RunInt( i->exec, i->module, val._.integer, getAction(i, 1));

    }else if ( val.vtag == val_bool ){
      Exec_RunBool( i->exec, i->module, val._.boolean, getAction(i, 1));
    }else{
      ERROR(inerr_TypeMismatch,0);
    };
  };
};

/*----------------------------------------------------*/
void Interpret_ExecuteWrite( Interpret_Interpereter_t * i, node_stmt_t * stmt ){
  node_expr_t * expr = NULL;
  node_val_t    val  = {val_undef, 0};

  expr = EL_GetFirst( stmt->_.write.exprs );
  while(expr){
    Interpret_CalculateExpression( i,  expr, &val );
    if ( val.vtag == val_int ){
      printf( "%d", val._.integer );
    }else if ( val.vtag == val_str ){
      printf( val._.string );
    }else if ( val.vtag == val_bool ){
      printf( "%s", val._.boolean ? "<TRUE>" : "<FALSE>" );
    };
    expr = EL_GetNext( stmt->_.write.exprs );
  };

  if ( stmt->_.write.ln ) printf("\n");
};

/*----------------------------------------------------*/
void Interpret_ExecuteStatement( Interpret_Interpereter_t * i,
                                 node_stmt_t * stmt )
{
   Top( i->stack )->sstate = stmtstate_done;
   switch( stmt->stag ){
     case stmt_Assign   : Interpret_ExecuteAssign   ( i, stmt );
                          break;
     case stmt_Comment  : Interpret_ExecuteComment  ( i, stmt );
                          break;
     case stmt_Compile  : Interpret_ExecuteCompile  ( i, stmt );
                          break;
     case stmt_Exit     : Interpret_ExecuteExit     ( i, stmt );
                          break;
     case stmt_For      : Interpret_ExecuteFor      ( i, stmt );
                          break;
     case stmt_Foreach  : Interpret_ExecuteForeach  ( i, stmt );
                          break;
     case stmt_Generate : Interpret_ExecuteGenerate ( i, stmt );
                          break;
     case stmt_If       : Interpret_ExecuteIf       ( i, stmt );
                          break;
     case stmt_Loop     : Interpret_ExecuteLoop     ( i, stmt );
                          break;
     case stmt_Proccall : Interpret_ExecuteProcCall ( i, stmt );
                          break;
     case stmt_Return   : Interpret_ExecuteReturn   ( i, stmt );
                          break;
     case stmt_Run      : Interpret_ExecuteRun      ( i, stmt );
                          break;
     case stmt_write    : Interpret_ExecuteWrite    ( i, stmt );
                          break;
     default            : ASSERT(0);
   };
};

/*----------------------------------------------------*/
void Interpret_ExecuteModule( Interpret_Interpereter_t * i ){
  state_node_t * state;

  while(1){
    state = Top( i->stack );
    ASSERT( state && state->statement && state->statements );
    if ( state->sstate == stmtstate_inprogress ){
      i->module->locals = state->locals;
      state->stmt_list_state = SL_SaveState( state->statements );
      Interpret_ExecuteStatement(i, state->statement);
        }else{
      SL_RestoreState( state->statements, state->stmt_list_state );
          state->statement = SL_GetNext( state->statements );
          if ( state->statement ){
        state->sstate = stmtstate_inprogress;
          }else{
        Pop( i->stack );
        if ( !Top( i->stack ) ) return;
          };
        };
  };
};

/*----------------------------------------------------*/
long Interpret_GetTestsCount( Interpret_Interpereter_t * i ){

  return Exec_GetTestsCount( i->exec );

};

/*----------------------------------------------------*/
Interpret_Interpereter_t * Interpret_Init( void ){
  Interpret_Interpereter_t * i;

  i = allocate( Interpret_Interpereter_t );
  i->magic = SetMagic( CTINTERPRET_MAGIC );
  i->stack = CreateStack();
  i->cfg   = Cfg_Init( "ctr.cfg" );
  if ( !i->cfg ) ERROR(exerr_CantOpenFile,"ctr.cfg");
  i->exec  = Exec_Init( i->cfg );

  return i;
};

/*----------------------------------------------------*/
void Interpret_Final( Interpret_Interpereter_t * i ){

  MAGIC_ASSERT( i );
  DestroyStack( i->stack );
  Cfg_Final( i->cfg );
  Exec_Final( i->exec );

};

/*----------------------------------------------------*/
void Interpret_Module ( Interpret_Interpereter_t * i, node_module_t * m ){

  MAGIC_ASSERT( i );

  if ( ( Cfg_GetTest( i->cfg ) == ctCfg_TestComment) ||
       ( Cfg_GetTest( i->cfg ) == ctCfg_TestCompare) ||
       ( Cfg_GetTest( i->cfg ) == ctCfg_TestAll && (m->gen_compile || m->gen_run) ) ||
       ( Cfg_GetTest( i->cfg ) == ctCfg_TestCompile) ||
       ( Cfg_GetTest( i->cfg ) == ctCfg_TestRun     && m->gen_run     ) )
  {
    i->module = m;

    Push( i->stack, state_basic, m->body, NULL );

    Interpret_ExecuteModule( i );
  };
};


