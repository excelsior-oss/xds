#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vxWorks.h>
#include <taskLib.h>
#include <taskHookLib.h>
#include <intLib.h>

#include "xdTypes.h"
#include "xdAssert.h"
#include "xdServer.h"
#include "xdTransport.h"
#include "xdTransportTCP.h"


#define SERVER_WORK_PRIORITY   100
#define SERVER_STACK_SIZE      10000

#define taskNamePttr "taskXDServer_%d"


typedef struct {
  char               name[16];
  xdTransport_Init   transInit;
  xdTransport_Accept transAccept;
  xdTransport_Final  transFinal;
} Transport;

static int taskNumber = 0;

static Transport xdDemon_transports[] = {
  {"TCP", xdTransportTCP_Init, xdTransportTCP_Accept, xdTransportTCP_Final},
  {""}
};


STATUS xdDemon_SetHooks(void){
  if(intConnect((VOIDFUNCPTR*)0x24, (VOIDFUNCPTR)xdServer_IntrStub, 0x24) == ERROR) return;
  if(intConnect((VOIDFUNCPTR*)0x88, (VOIDFUNCPTR)xdServer_IntrStub, 0x88) == ERROR) return;
  if(taskDeleteHookAdd((FUNCPTR)xdServer_TaskDeleteHook) == ERROR) return ERROR;
  return OK;
};

int xdDemon_FindTransport(char name[]){
  int num;

  for(num = 0; xdDemon_transports[num].name[0] && strcmp(name, xdDemon_transports[num].name) != 0; num++);
  if(xdDemon_transports[num].name[0]){
    return num;
  }else{
    printf("Unknown transport \"%s\"\n", name);
    return ERROR;
  };
};

int xdDemon_Demon(char * transport){
  int  taskNumber;
  char taskName[100];
  int  pipeID;
  int  transID;
  int  i;

  xdTransportDesc * transDesc;

  if(xdDemon_SetHooks() == ERROR) return;

  if((transID = xdDemon_FindTransport(transport)) == ERROR) return;
  transDesc = xdDemon_transports[transID].transInit();

  for(i = 0; i < MAX_SERVERS; i++) xdServer_Servers[i].free = TRUE;

  for(;;){
    if((pipeID = xdDemon_transports[transID].transAccept(&transDesc)) == ERROR) return;
    sprintf(taskName, taskNamePttr, ++taskNumber);
    if(taskSpawn(taskName, SERVER_WORK_PRIORITY, 0, SERVER_STACK_SIZE,
                   (FUNCPTR)xdServer_Server, pipeID, (int)transport, taskNumber, 0, 0, 0, 0, 0, 0, 0) == ERROR){
      perror("taskSpawn");
      return ERROR;
    };
  };
};
