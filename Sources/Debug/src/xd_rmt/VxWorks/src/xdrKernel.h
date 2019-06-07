/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrKernel.h                                                *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains interface to kernel.             *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrKernel_h
#define _xdrKernel_h



#include <semLib.h>

#include "xdrMessages.h"


 /* Incoming messages queue */
extern xdrMessages_MessageQueue * xdrKernel_InQueue;

/* Kernel initialization procedure.
   Returns True if initialization was succesfully completed,
   and False otherwise.
*/
extern int xdrKernel_Init(void);

#endif