#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>

#include <symLib.h>
#include <sysSymTbl.h>
#include <usrLib.h>
#include <unldLib.h>


#include "xdAssert.h"
#include "xdTransportTCP.h"


#define MAGIC 0xDEADFACE

#define PORT_NUMBER            0x1055
#define MAX_QUEUED_CONNECTIONS 4

typedef struct{
  long magic;
  int  socketID;
} xdTransportTCPDesc;

int xdTransportTCP_Accept(xdTransportDesc ** d){
  xdTransportTCPDesc ** desc;
  struct sockaddr_in client_address;
  int pipeID, client_address_len;

  desc = (xdTransportTCPDesc**)d;
  ASSERT((*desc)->magic==MAGIC);

  if((pipeID = accept((*desc)->socketID, &client_address, &client_address_len)) == ERROR){
    xdTransportTCP_Final(d);
    perror("accept");
    return ERROR;
  };

  printf("Connection accepted!\n");
  return pipeID;
};

xdTransportDesc * xdTransportTCP_Init(void){
  struct sockaddr_in server_address, client_address;
  int socketID, pipeID, client_address_len;

  xdTransportTCPDesc * desc;



  if((socketID = socket(AF_INET, SOCK_STREAM, 0)) == ERROR){
    perror("socket");
    return NULL;
  };

  server_address.sin_family = AF_INET;
  server_address.sin_port   = PORT_NUMBER;
  server_address.sin_addr.s_addr = htonl(INADDR_ANY);

  if((bind(socketID, &server_address, sizeof(server_address))) == ERROR){
    perror("bind");
    return NULL;
  };

  if(listen(socketID, MAX_QUEUED_CONNECTIONS) == ERROR){
    close(socketID);
    perror("listen");
    return NULL;
  };


  desc = (xdTransportTCPDesc*) malloc(sizeof(xdTransportTCPDesc));
  desc->magic    = MAGIC;
  desc->socketID = socketID;

  return (xdTransportDesc*)desc;
};


void xdTransportTCP_Final(xdTransportDesc ** d){
  xdTransportTCPDesc ** desc;

  desc = (xdTransportTCPDesc**)d;
  ASSERT((*desc)->magic==MAGIC);
  close((*desc)->socketID);
  free(*desc);
  desc = NULL;
};