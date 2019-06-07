/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   ASSERTIONs  Den: 10-Aug-96
*/

#ifndef __ctAssert_h
#define __ctAssert_h


#ifdef CT_ASSERTIONS_OFF

#define ASSERT(x) ((void)0)
#define ASSERTM(x,message) ((void)0)

#else

#include <stdio.h>
#include <stdlib.h>
#define ASSERT(x) ((x)?(0):(fprintf(stderr,"\nInternal error: ASSERTion failed, file %s(compiled %s at %s) line # %d\n", __FILE__, __DATE__, __TIME__, __LINE__),exit(1)))
#define ASSERTM(x,message) ((x)?(0):(fprintf(stderr,"\nInternal error: ASSERTion failed(%s), file %s(compiled %s at %s) line # %d\n", message, __FILE__, __DATE__, __TIME__, __LINE__),exit(1)))

#endif



#endif __ctAssert_h
