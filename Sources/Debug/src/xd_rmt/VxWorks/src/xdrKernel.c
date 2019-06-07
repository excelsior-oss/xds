/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrKernel.h                                                *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This file contains implementation of kernel.               *|
|*                                                                            *|
\******************************************************************************/

#include "xdrIncl.h"
#include "xdrTrace.h"
#include "xdrInit.h"
#include "xdrAssert.h"
#include "xdrTypes.h"
#include "xdrKernel.h"
#include "xdrTransports.h"
#include "xdrClient.h"
#include "xdrTargetAgent.h"



/*----------------------------------------------------------------------------*\
|*                                                                            *|
|*                                 Access rights manager                      *|
|*                                                                            *|
\*----------------------------------------------------------------------------*/
typedef struct xdrKernel_tagModuleDescriptor{
  int moduleID;
  xdrClient_ClientDescriptor * client;
} xdrKernel_ModuleDescriptor;

typedef struct xdrKernel_tagTaskDescriptor{
  int taskID;
  xdrClient_ClientDescriptor * client;
} xdrKernel_TaskDescriptor;


xdrKernel_ModuleDescriptor xdrKernel_Modules[256];
int                        xdrKernel_ModulesNumber = 256;

xdrKernel_TaskDescriptor   xdrKernel_Tasks[256];
int                        xdrKernel_TasksNumber   = 256;

xdrClient_ClientDescriptor xdrKernel_Clients[256];
int                        xdrKernel_ClientsNumber = 256;



xdrClient_ClientDescriptor * xdrKernel_WhoseTask(int taskID){
  int i;

  for(i = 0; i < xdrKernel_TasksNumber; i++){
    if(xdrKernel_Tasks[i].taskID == taskID) return xdrKernel_Tasks[i].client;
  };
  return NULL;
};



/*----------------------------------------------------------------------------*\
|*                                                                            *|
|*                         Target Agent Output Queue Handler                  *|
|*                                                                            *|
\*----------------------------------------------------------------------------*/

int xdrKernel_TAOutQueueHandlerTaskID = 0;

#define TAOutQueueHandlerTaskName "xdrKernel_TAOutQueueHandlerTask"


/*-------------------------------------------------------*/
void xdrKernel_TAOutQueueHandler(void){
  xdrMessages_Message msg;
  xdrClient_ClientDescriptor * client;

  for(;;){
    /* Get message */
    xdrMessages_GetMessage(xdrTargetAgent_OutQueue, &msg);

    if(msg.Header.senderDesc != NULL){
      if(msg.Header.sync != NULL){
        /* synchronous message */
        ASSERT(semGive(msg.Header.sync) == OK);
      }else{
        /* asynchronous message */
        ASSERT(xdrMessages_IsDuplexCommunicator(msg.Header.senderDesc));
        xdrMessages_PutMessage(xdrMessages_DuplexCommunicatorInQueue(msg.Header.senderDesc), &msg);
      };
    }else{
      /* Process message */
      switch(msg.Header.mode){
        case msgMode_EventSingleStep:
             client = xdrKernel_WhoseTask(msg.Body.EventSingleStep.currentTaskID);
             if(client != NULL){
               ASSERT(xdrMessages_IsDuplexCommunicator(client));
               xdrMessages_PutMessage(xdrMessages_DuplexCommunicatorInQueue(client), &msg);
             };
             break;
        case msgMode_EventBreakpointHit:
             client = xdrKernel_WhoseTask(msg.Body.EventSingleStep.currentTaskID);
             if(client != NULL){
               ASSERT(xdrMessages_IsDuplexCommunicator(client));
               xdrMessages_PutMessage(xdrMessages_DuplexCommunicatorInQueue(client), &msg);
             };
             break;
        case msgMode_EventDeleteTask:
             client = xdrKernel_WhoseTask(msg.Body.EventDeleteTask.deletedTaskID);
             if(client != NULL){
               ASSERT(xdrMessages_IsDuplexCommunicator(client));
               xdrMessages_PutMessage(xdrMessages_DuplexCommunicatorInQueue(client), &msg);
             };
             break;
/*
        case :
             break;
*/
        default:
             printf("Kernel (xdrTargetAgent_OutQueue): unknown message mode %d\n", msg.Header.mode);
             break;
      };
    };
  };
};


/*-------------------------------------------------------*/
void xdrKernel_TAOutQueueHandlerTask(void){

  /* Wait for start semaphore */
  if(semTake(xdrInit_StartSemaphore, WAIT_FOREVER) != OK){
    perror("semTake");
    for(;;);
  };

  xdrKernel_TAOutQueueHandler();
};




/*----------------------------------------------------------------------------*\
|*                                                                            *|
|*                         Input Queue and its Handler                        *|
|*                                                                            *|
\*----------------------------------------------------------------------------*/

xdrMessages_MessageQueue * xdrKernel_InQueue;

int xdrKernel_InQueueHandlerTaskID = 0;

#define InQueueHandlerTaskName "xdrKernel_InQueueHandlerTask"

/*-------------------------------------------------------*/
/*-------------------------------------------------------*/
/*-------------------------------------------------------*/
void xdrKernel_ConnectionRequest(xdrMessages_Message * msg){
  int result, i;

  perror("Connection request");

  result = xdrIO_ServerConnect(msg->Body.ConnectionRequest.pipeDesc, DummyChannel);
  if(result){
    printf("Right identification\n");
  }else{
    printf("Wrong identification\n");
  };

  for(i = 0; i < xdrKernel_ClientsNumber && xdrKernel_Clients[i].magic == xdrMagic_ClientDescriptor; i++);

  if(i == xdrKernel_ClientsNumber){
    printf("No more clients\n");
    return;
  }

  if(!xdrClient_Create(&xdrKernel_Clients[i], msg->Body.ConnectionRequest.pipeDesc)){
    printf("Can't create client\n");
    return;
  };
};


/*-------------------------------------------------------*/
void xdrKernel_InQueueHandler(void){
  xdrMessages_Message msg;

  for(;;){
    /* Get message */
    xdrMessages_GetMessage(xdrKernel_InQueue, &msg);

    /* Process message */
    switch(msg.Header.mode){
      case msgMode_ConnectionRequest:
           xdrKernel_ConnectionRequest(&msg);
           break;
      case msgMode_LoadModule:
      case msgMode_GetModuleInfo:
      case msgMode_FindSymbol:
      case msgMode_GetLoadedModulesList:
      case msgMode_CreateTask:
      case msgMode_GetRegisters:
      case msgMode_PutRegisters:
      case msgMode_GetModuleID:
      case msgMode_GetModuleSymbols:
      case msgMode_GetMemory:
      case msgMode_PutMemory:
      case msgMode_GetSegmentInfo:
      case msgMode_SetTraceMode:
      case msgMode_ResetTraceMode:
      case msgMode_ResumeTask:
           xdrMessages_PutMessage(xdrTargetAgent_InQueue, &msg);
           break;
      default:
           printf("Kernel (xdrKernel_InQueue): unknown message mode %d", msg.Header.mode);
           break;
    };
  };
};


/*-------------------------------------------------------*/
void xdrKernel_InQueueHandlerTask(void){

  /* Wait for start semaphore */
  if(semTake(xdrInit_StartSemaphore, WAIT_FOREVER) != OK){
    /* Fatal */
    printf("xdrKernel: semTake [1]\n");
    for(;;);
  };

  xdrKernel_InQueueHandler();
};



/*----------------------------------------------------------------------------*/
int xdrKernel_Init(void){

  memset(xdrKernel_Clients, 0, xdrKernel_ClientsNumber*sizeof(xdrClient_ClientDescriptor));
  memset(xdrKernel_Modules, 0, xdrKernel_ModulesNumber*sizeof(xdrKernel_ModuleDescriptor));
  memset(xdrKernel_Tasks,   0, xdrKernel_TasksNumber*sizeof(xdrKernel_TaskDescriptor));

  /* Create input queue */
  if((xdrKernel_InQueue = xdrMessages_CreateMessageQueue(WAIT_FOREVER)) == NULL) return False;

  /* Spawn input queue handler task */
  if((xdrKernel_InQueueHandlerTaskID = taskSpawn(InQueueHandlerTaskName, 
                 xdrTypes_TaskWorkPriority, 0, xdrTypes_TaskStackSize,
                 (FUNCPTR)xdrKernel_InQueueHandlerTask, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == ERROR){
    perror("taskSpawn");
    return False;
  };
  xdrInit_StartedTasksNumber++;

  /* Spawn Target Agent output queue handler task */
  if((xdrKernel_TAOutQueueHandlerTaskID = taskSpawn(TAOutQueueHandlerTaskName, 
                 xdrTypes_TaskWorkPriority, 0, xdrTypes_TaskStackSize,
                 (FUNCPTR)xdrKernel_TAOutQueueHandlerTask, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == ERROR){
    perror("taskSpawn");
    return False;
  };
  xdrInit_StartedTasksNumber++;

  return True;
};


