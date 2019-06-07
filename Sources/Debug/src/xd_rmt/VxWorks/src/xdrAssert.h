/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrAssert.h                                                *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains interface to assertion           *|
|*                 facilities.                                                *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrAssert_h
#define _xdrAssert_h



#ifdef XDR_ASSERTIONS_OFF

#define ASSERT(x) ((void)x)
#define ASSERTM(x,message) ((void)x)

#else

#include <stdlib.h>

#define ASSERT(x) ((x)?(0): xdrAssert_Failure(NULL, __FILE__, __DATE__, __TIME__, __LINE__))
#define ASSERTM(x, msg) ((x)?(0): xdrAssert_Failure((msg), __FILE__, __DATE__, __TIME__, __LINE__))

extern int xdrAssert_Failure(
  char * msg, 
  char * file, 
  char * date, 
  char * time, 
  int    line
);


#endif


#endif
