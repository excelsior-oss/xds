/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrTAHooks.c                                               *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This file contains implementation of target agent hooks.   *|
|*                                                                            *|
\******************************************************************************/

#include <taskLib.h>


#include "xdrIncl.h"
#include "xdrTrace.h"
#include "xdrTypes.h"
#include "xdrTAHooks.h"
#include "xdrMessages.h"


/*----------------------------------------------------------------------------*/
xdrMessages_MessageQueue * xdrTAHooks_OutQueue; /* Outcoming messages queue */




/*----------------------------------------------------------------------------*/
#define InterruptNumber_SingleStep    0x24
#define InterruptNumber_BreakpointHit 0x88

void xdrTAHooks_IntrStub(int intr){
  xdrMessages_Message msg;

  msg.Header.senderDesc = NULL;

  if(taskSuspend((int)taskIdCurrent) == ERROR) return;

  switch(intr){
    case InterruptNumber_SingleStep:
           msg.Header.mode                        = msgMode_EventSingleStep;
           msg.Body.EventSingleStep.currentTaskID = (int)taskIdCurrent;
           break;
    case InterruptNumber_BreakpointHit:
           msg.Header.mode                           = msgMode_EventBreakpointHit;
           msg.Body.EventBreakpointHit.currentTaskID = (int)taskIdCurrent;
           break;
    default:
           msg.Header.mode                     = msgMode_EventUnknown;
           msg.Body.EventUnknown.currentTaskID = (int)taskIdCurrent;
           break;
  };
  xdrMessages_PostMsg(xdrTAHooks_OutQueue, &msg);
};


/*----------------------------------------------------------------------------*/
void xdrTAHooks_TaskDeleteHook(WIND_TCB * pTcb){
  xdrMessages_Message msg;

  msg.Header.senderDesc = NULL;

  msg.Header.mode                         = msgMode_EventDeleteTask;
  msg.Body.EventDeleteTask.deletingTaskID = (int)taskIdCurrent;
  msg.Body.EventDeleteTask.deletedTaskID  = (int)pTcb;
  xdrMessages_PostMsg(xdrTAHooks_OutQueue, &msg);
};



/*----------------------------------------------------------------------------*/
/* Target agent hooks initialization procedure.
   Returns True if initialization was succesfully completed,
   and False otherwise.
*/
int xdrTAHooks_Init(void){

  /* Create queue */
  if((xdrTAHooks_OutQueue = xdrMessages_CreateMessageQueue(NO_WAIT)) == NULL) return False;

  /* Set hooks */
  if(intConnect((VOIDFUNCPTR*)InterruptNumber_SingleStep,    (VOIDFUNCPTR)xdrTAHooks_IntrStub, InterruptNumber_SingleStep)    == ERROR) return False;
  if(intConnect((VOIDFUNCPTR*)InterruptNumber_BreakpointHit, (VOIDFUNCPTR)xdrTAHooks_IntrStub, InterruptNumber_BreakpointHit) == ERROR) return False;
  if(taskDeleteHookAdd((FUNCPTR)xdrTAHooks_TaskDeleteHook) == ERROR) return False;

  return True;
};


