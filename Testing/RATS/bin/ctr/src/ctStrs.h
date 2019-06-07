/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Strings manager for CTRout(CityRout) - Compiler Testing Routine
                                                   Alexs: 4-Nov-96
*/
#ifndef __ctStrs_h
#define __ctStrs_h

#include "ctMemory.h"
#include <string.h>
#include <stdlib.h>

#define ctStrs_COPY(to, from)  (((to)==(from))?(to):((((to)=strcpy(_allocate(char,strlen(from)+1),from)))))

#endif /* __ctStrs_h */
