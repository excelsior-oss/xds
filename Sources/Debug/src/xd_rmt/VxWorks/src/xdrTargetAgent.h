/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrTargetAgent.h                                           *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains interface to target agent        *|
|*                 (queues of incoming and outcoming messages,                *|
|*                 initialization function).                                  *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrTargetAgent_h
#define _xdrTargetAgent_h



#include "xdrMessages.h"



extern xdrMessages_MessageQueue * xdrTargetAgent_InQueue;
                             /* Incoming messages queue */

extern xdrMessages_MessageQueue * xdrTargetAgent_OutQueue;
                             /* Outcoming messages queue */

/* Target agent initialization procedure.
   Returns True if initialization was succesfully completed,
   and False otherwise.
*/
extern int xdrTargetAgent_Init(void);


#endif