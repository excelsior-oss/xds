/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Syntax tree structure for CTROUT(CityRout) - Compiler Testing Routine
                                                        Alexs: 11-Nov-96
*/


#include "ctNode.h"
#include "ctMagic.h"
#include "ctMemory.h"
#include "ctAssert.h"



DeclareMagic(StatementListMagic);
DeclareMagic(VariableListMagic);
DeclareMagic(ExpressionListMagic);
DeclareMagic(ProcedureListMagic);
DeclareMagic(ParamListMagic);
DeclareMagic(TemplateListMagic);
DeclareMagic(StoreListMagic);


/*--------------------------------------------------*/
List_t * SL_Create(void){
  return List_Create(StatementListMagic);
};

/*--------------------------------------------------*/
void SL_Destroy(List_t * l){
  List_Destroy(l, StatementListMagic);
};

/*--------------------------------------------------*/
void SL_Insert(List_t * l, node_stmt_t * stmt){
  List_Insert(l, StatementListMagic, stmt);
};

/*--------------------------------------------------*/
node_stmt_t * SL_GetFirst(List_t * l){
  return (node_stmt_t*)List_GetFirst(l, StatementListMagic);
};

/*--------------------------------------------------*/
node_stmt_t * SL_GetNext(List_t * l){
  return (node_stmt_t*)List_GetNext(l, StatementListMagic);
};

/*--------------------------------------------------*/
long SL_SaveState ( List_t * l ){
  return List_SaveState( l, StatementListMagic);
};

/*--------------------------------------------------*/
void SL_RestoreState ( List_t * l, long state ){
  List_RestoreState( l, StatementListMagic, state );
};

/*--------------------------------------------------*/
List_t * VL_Create(void){
  return List_Create(VariableListMagic);
};

/*--------------------------------------------------*/
void VL_Destroy(List_t * l){
  List_Destroy(l, VariableListMagic);
};

/*--------------------------------------------------*/
void VL_Insert(List_t * l, node_var_t * var){
  List_Insert(l, VariableListMagic, var);
};

/*--------------------------------------------------*/
node_var_t * VL_GetFirst(List_t * l){
  return (node_var_t*)List_GetFirst(l, VariableListMagic);
};

/*--------------------------------------------------*/
node_var_t * VL_GetNext(List_t * l){
  return (node_var_t*)List_GetNext(l, VariableListMagic);
};

/*--------------------------------------------------*/
node_var_t * VL_Get(List_t * l, long name){
  node_var_t * var;

  var = VL_GetFirst(l);
  while(var){
    if (var->name == name) return var;
    var = VL_GetNext(l);
  };
  return var;
};

/*--------------------------------------------------*/
List_t * EL_Create(void){
  return List_Create(ExpressionListMagic);
};

/*--------------------------------------------------*/
void EL_Destroy(List_t * l){
  List_Destroy(l, ExpressionListMagic);
};

/*--------------------------------------------------*/
void EL_Insert(List_t * l, node_expr_t * expr){
  List_Insert(l, ExpressionListMagic, expr);
};

/*--------------------------------------------------*/
node_expr_t * EL_GetFirst(List_t * l){
  return (node_expr_t*)List_GetFirst(l, ExpressionListMagic);
};

/*--------------------------------------------------*/
node_expr_t * EL_GetNext(List_t * l){
  return (node_expr_t*)List_GetNext(l, ExpressionListMagic);
};

/*--------------------------------------------------*/
List_t * PL_Create(void){
  return List_Create(ProcedureListMagic);
};

/*--------------------------------------------------*/
void PL_Destroy(List_t * l){
  List_Destroy(l, ProcedureListMagic);
};

/*--------------------------------------------------*/
void PL_Insert(List_t * l, node_proc_t * proc){
  List_Insert(l, ProcedureListMagic, proc);
};

/*--------------------------------------------------*/
node_proc_t * PL_GetFirst(List_t * l){
  return (node_proc_t*)List_GetFirst(l, ProcedureListMagic);
};

/*--------------------------------------------------*/
node_proc_t * PL_GetNext(List_t * l){
  return (node_proc_t*)List_GetNext(l, ProcedureListMagic);
};

/*--------------------------------------------------*/
node_proc_t * PL_Get(List_t * l, long name){
  node_proc_t * proc;

  proc = PL_GetFirst(l);
  while(proc){
    if (proc->name == name) return proc;
    proc = PL_GetNext(l);
  };
  return proc;
};

/*--------------------------------------------------*/
List_t * PML_Create(void){
  return List_Create(ParamListMagic);
};

/*--------------------------------------------------*/
void PML_Destroy(List_t * l){
  List_Destroy(l, ParamListMagic);
};

/*--------------------------------------------------*/
void PML_Insert(List_t * l, node_param_t * param){
  List_Insert(l, ParamListMagic, param);
};

/*--------------------------------------------------*/
node_param_t * PML_GetFirst(List_t * l){
  return (node_param_t*)List_GetFirst(l, ParamListMagic);
};

/*--------------------------------------------------*/
node_param_t * PML_GetNext(List_t * l){
  return (node_param_t*)List_GetNext(l, ParamListMagic);
};

/*--------------------------------------------------*/
List_t * TL_Create(void){
  return List_Create(TemplateListMagic);
};

/*--------------------------------------------------*/
void TL_Destroy(List_t * l){
  List_Destroy(l, TemplateListMagic);
};

/*--------------------------------------------------*/
void TL_Insert(List_t * l, node_template_t * template){
  List_Insert(l, TemplateListMagic, template);
};

/*--------------------------------------------------*/
node_template_t * TL_GetFirst(List_t * l){
  return (node_template_t*)List_GetFirst(l, TemplateListMagic);
};

/*--------------------------------------------------*/
node_template_t * TL_GetNext(List_t * l){
  return (node_template_t*)List_GetNext(l, TemplateListMagic);
};

/*--------------------------------------------------*/
void ctNode_Init(void){
  static init = 0;

  if (init){
    return;
  }else{
    init = 1;
  };

  SetMagic(StatementListMagic);
  SetMagic(VariableListMagic);
  SetMagic(ExpressionListMagic);
  SetMagic(ProcedureListMagic);
  SetMagic(ParamListMagic);
  SetMagic(TemplateListMagic);
  SetMagic(StoreListMagic);
};

