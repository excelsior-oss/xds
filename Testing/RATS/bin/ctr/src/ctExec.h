/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Executor for CTROUT(CityRout) - Compiler Testing Routine
                                           Alexs: 18-Nov-96
*/

#ifndef __ctExec_h
#define __ctExec_h

#include "ctCfg.h"
#include "ctNode.h"

  typedef struct Exec_Executor_t Exec_Executor_t;  /* hidden type */

struct Exec_Executor_t{
  long magic;

  Cfg_Configurator_t * cfg;
  long		       number;
                       
  char		     * curr_template_name;
  char		     * buff;

  char		    ** namelist;
  char              ** filenamelist;

  long		       error;

  FILE               * report;
};



  #define Exec_DoCompare 0
  #define Exec_DoCompile 1
  #define Exec_DoRun     2
  #define Exec_DoNone    3


  extern Exec_Executor_t * Exec_Init        ( Cfg_Configurator_t * c );
  extern void              Exec_Final       ( Exec_Executor_t * e );

  extern void              Exec_RunBool     ( Exec_Executor_t * e , node_module_t * m, long result, long action);
  extern void              Exec_RunInt      ( Exec_Executor_t * e , node_module_t * m, long result, long action);
  extern void              Exec_CompileBool ( Exec_Executor_t * e , node_module_t * m, long result, long action);
  extern void              Exec_CompileInt  ( Exec_Executor_t * e , node_module_t * m, long result, long action);
  extern long              Exec_GetTestsCount( Exec_Executor_t * e);


#endif /* __ctExec_h */


