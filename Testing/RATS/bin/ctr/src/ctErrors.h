/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Error manager for CTRout(CityRout) - Compiler Testing Routine
                                                 Alexs: 4-Nov-96
*/
#ifndef __ctErrorManager
#define __ctErrorManager

#include "ctErrCodes.h"
#include <stdio.h>

  extern void Errors_Error  (ErrCodes_ErrorCode_t error, long lineno, char * additional_info, FILE * report);
  extern void Errors_Warning(ErrCodes_WarningCode_t warning, long lineno, char * additional_info, FILE * report);

#endif /* __ctErrorManager */
