/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrTAHooks.h                                               *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains interface to target agent hooks. *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrTAHooks_h
#define _xdrTAHooks_h


#include "xdrMessages.h"

extern xdrMessages_MessageQueue * xdrTAHooks_OutQueue;
                                  /* Outcoming messages queue */

/* Target agent hooks initialization procedure.
   Returns True if initialization was succesfully completed,
   and False otherwise.
*/
extern int xdrTAHooks_Init(void);


#endif