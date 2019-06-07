/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrTransportTCP.c                                          *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This file contains implementation of TCP/IP transport      *|
|*                 module.                                                    *|
|*                                                                            *|
\******************************************************************************/

#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <taskLib.h>



#include "xdrIncl.h"
#include "xdrTrace.h"
#include "xdrTypes.h"
#include "xdrInit.h"
#include "xdrTransportTCP.h"



/*----------------------------------------------------------------------------*/
#define PORT_NUMBER            0x1055
#define MAX_QUEUED_CONNECTIONS 4

#define TCPTaskName "xdrTransport_TCP"




/*-------------------------------------------------------*/
static xdrAccept(int socketID){
  struct sockaddr_in client_address;
  int pipeID, client_address_len;
  return accept(socketID, &client_address, &client_address_len);
};

/*-------------------------------------------------------*/
static void xdrTransportTCP_Accept(int socketID, xdrTransports_TransportDescriptor * desc){
  int pipeID;

  /* Accept connection */
  for(;;){
    printf("Waiting for connection...\n");
    if((pipeID = xdrAccept(socketID)) == ERROR){
      /* Notify the kernel about failure */
      xdrTransports_NotifyKernel(xdrTransports_MsgTransportFailure, desc, 0);
      perror("accept");
      for(;;);
    };

    printf("Connection accepted!\n");

    /* Notify the kernel about accepted connection */
    xdrTransports_NotifyKernel(xdrTransports_MsgConnectionRequest, desc, pipeID);
  };
};


/*-------------------------------------------------------*/
int xdrTransportTCP_AcceptTask(int socketID, xdrTransports_TransportDescriptor * desc){

  /* Wait for start semaphore */
  if(semTake(xdrInit_StartSemaphore, WAIT_FOREVER) != OK){
    perror("semTake");
    for(;;);
  };

  xdrTransportTCP_Accept(socketID, desc);
};




/*----------------------------------------------------------------------------*/
int xdrTransportTCP_Init(xdrTransports_TransportDescriptor * desc){
  struct sockaddr_in server_address;
  int                socketID, taskID;


  /* Get socket */
  if((socketID = socket(PF_INET, SOCK_STREAM, 0)) == ERROR){
    perror("socket");
    return False;
  };

  /* Bind socket */ 
  server_address.sin_family      = PF_INET;
  server_address.sin_port        = PORT_NUMBER;
  server_address.sin_addr.s_addr = htonl(INADDR_ANY);

  if((bind(socketID, &server_address, sizeof(server_address))) == ERROR){
    perror("bind");
    return False;
  };

  /* Listen socket */
  if(listen(socketID, MAX_QUEUED_CONNECTIONS) == ERROR){
    close(socketID);
    perror("listen");
    return False;
  };

  /* Spawn accepting task */
  if((taskID = taskSpawn(TCPTaskName, xdrTypes_TaskWorkPriority, 0, xdrTypes_TaskStackSize,
                 (FUNCPTR)xdrTransportTCP_AcceptTask, socketID, (int)desc, 0, 0, 0, 0, 0, 0, 0, 0)) == ERROR){
    close(socketID);
    perror("taskSpawn");
    return False;
  };
  xdrInit_StartedTasksNumber++;

  /* Fill descriptor */
  desc->Body.TCP.socketID = socketID;
  desc->Body.TCP.taskID   = taskID;

  return True;
};



/*----------------------------------------------------------------------------*/
int xdrTransportTCP_Final(xdrTransports_TransportDescriptor * desc){
  return close(desc->Body.TCP.socketID)    == OK && 
         taskDelete(desc->Body.TCP.taskID) == OK;
};