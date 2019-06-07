/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrClient.h                                                *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains interface to client              *|
|*                 (client descriptor definition and managment functions).    *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrClient_h
#define _xdrClient_h

#include <taskLib.h>
#include <msgQLib.h>

#include "xdrMessages.h"
#include "xdrTransports.h"

typedef struct xdrClient_tagClientDescriptor xdrClient_ClientDescriptor;


/* Client descriptor definition */
struct xdrClient_tagClientDescriptor{
  int                            magic;    /* Magic number                        */
  xdrMessages_MessageQueue     * inQueue;  /* Incoming messages queue             */
  xdrTransports_PipeDescriptor * pipeDesc; /* Pipe to interact with remote client */


  int number;         
  int inQueueTaskID;
  int commandsTaskID;
  MSG_Q_ID eventsQueue;
  SEM_ID   eventSem, eventSem1;




  /* Default parameters */
  int moduleID;       
  int debuggedTaskID;

/*
  WIND_TCB   debuggedTcb;
  void     * debuggedTaskStack;
*/


  /*  Locals  */
  DWORD                       arg_size;
  BYTE                        arg_buf[1024];
  BYTE                        programStarted;
  xdrKernelTypes_ModuleInfo   moduleInfo;
};


/* Procedure to create client. 
   Receives two paremeters: 'pipeDesc' that is pipe to interact with remote 
   client and pointer to client descriptor that will be filled.
   Returns True if a client is successfully created and False if some errors 
   have happend.
*/
extern int xdrClient_Create(
  xdrClient_ClientDescriptor   * clientDesc,
  xdrTransports_PipeDescriptor * pipeDesc
);

/* Procedure determining client tasks */
extern int xdrClient_IsClientTask(
  xdrClient_ClientDescriptor   * clientDesc,
  int				 taskID
);

#endif