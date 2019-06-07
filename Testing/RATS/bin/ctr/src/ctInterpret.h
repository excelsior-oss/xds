/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Interpreter for CTROUT(CityRout) - Compiler Testing Routine
                                              Alexs: 11-Nov-96
*/

#ifndef __ctInterpret_h
#define __ctInterpret_h

#include "ctNode.h"


  extern int  numbers[];
  extern int  nnumbers;


  typedef struct Interpret_Interpereter_t Interpret_Interpereter_t;  /* hidden type */


  extern Interpret_Interpereter_t * Interpret_Init   ( void );
  extern void                       Interpret_Final  ( Interpret_Interpereter_t * i );

  extern void                       Interpret_Module ( Interpret_Interpereter_t * i,
                                                       node_module_t * m );

  extern long                       Interpret_GetTestsCount( Interpret_Interpereter_t * i );

#endif /* __ctInterpret_h */
