/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrInit.c                                                  *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This file contains main routine implementation.            *|
|*                                                                            *|
\******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <taskLib.h>

#include "xdrIncl.h"
#include "xdrTrace.h"
#include "xdrInit.h"
#include "xdrKernel.h"
#include "xdrTransports.h"
#include "xdrCfg.h"


int    xdrInit_StartedTasksNumber = 0;
SEM_ID xdrInit_StartSemaphore;

static InitComponent(int res, char name[]){
  printf(name);
  if(res){
    printf(" - ok\n");
  }else{
    printf(" - faulure\n");
    exit(1);
  };
};


int xdrInit_Init(void){
  return (xdrInit_StartSemaphore = semBCreate(SEM_Q_FIFO, SEM_EMPTY)) != NULL;
};

/*extern int xdrTransportTCP_AcceptTask(int);*/

/* Main procedure */
void xdrInit(char * args){
  int tmp[256];

  /* Initialize components */
  InitComponent(xdrInit_Init()       , "Initialization module");
  InitComponent(xdrTargetAgent_Init(), "Target agent   module");
  InitComponent(xdrKernel_Init()     , "Kernel         module");
  InitComponent(xdrCfg_Init(args)    , "Configuration  module");  
  InitComponent(xdrTransports_Init() , "Transports     module"); 


  /* Start */
  do{
    taskDelay(100);
  }while(semInfo(xdrInit_StartSemaphore, tmp, 256) != xdrInit_StartedTasksNumber);
  InitComponent(semFlush(xdrInit_StartSemaphore) == OK,
                                       "Start                ");
};
