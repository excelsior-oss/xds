/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Syntax analyzer for CTROUT(CityRout) - Compiler Testing Routine
                                                       Alexs: 4-Nov-96
*/
#ifndef __ctParse_h
#define __ctParse_h

#include "ctNode.h"


  typedef struct Parse_Parser_t Parse_Parser_t;  /* hidden type */


  extern Parse_Parser_t *  Parse_Init   ( char * name, char * reportname );
  extern void              Parse_Final  ( Parse_Parser_t * p );

  extern node_module_t  *  Parse_Module ( Parse_Parser_t * p );

#endif /* __ctParse_h */
