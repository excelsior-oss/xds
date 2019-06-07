/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrTransports.c                                            *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This file contains implementation of common transport      *|
|*                 module.                                                    *|
|*                                                                            *|
\******************************************************************************/

#include <vxWorks.h>
#include <stdio.h>


#include "xdrIncl.h"
#include "xdrTrace.h"
#include "xdrAssert.h"
#include "xdrTypes.h"
#include "xdrMagic.h"
#include "xdrMemoryManager.h"
#include "xdrTransports.h"
#include "xdrKernel.h"

#ifdef xdrIncl_TransportTCP
#include "xdrTransportTCP.h"
#endif


/*----------------------------------------------------------------------------*/
xdrTransports_TransportDescriptor xdrTransports_Transports[] = {
#ifdef xdrIncl_TransportTCP
  { /* magic */               xdrMagic_TransportDescriptor,
    /* Header */ {
      /* transport_type */    xdrTransports_TCP,
      /* initialized    */    False,
      /* init           */    xdrTransportTCP_Init,
      /* final          */    xdrTransportTCP_Final
    }
    /* Body */ 
  },
#endif
  { /* magic */               xdrMagic_TransportDescriptor,
    /* Header */ {
      /* transport_type */    xdrTransports_None,
      /* initialized    */    False,
      /* init           */    NULL,
      /* final          */    NULL
    }
    /* Body */ 
  }
};


/*-------------------------------------------------------*/
static int TransportRequired(int number){
  return True;
};




/*-------------------------------------------------------*/
int xdrTransports_Init(void){
  int i, init_trans_num = 0;
  for(i = 0; xdrTransports_Transports[i].Header.transport_type != xdrTransports_None; i++){
    if(TransportRequired(i)){
      init_trans_num += ((xdrTransports_Transports[i].Header.initialized = 
        xdrTransports_Transports[i].Header.init(&xdrTransports_Transports[i])) != False);
    };
  };
  return init_trans_num > 0;
};



/*-------------------------------------------------------*/
int xdrTransports_Final(void){
  int i, res = 1;
  for(i = 0; xdrTransports_Transports[i].Header.transport_type != xdrTransports_None; i++){
    if(xdrTransports_Transports[i].Header.initialized){
      res = res && xdrTransports_Transports[i].Header.final(&xdrTransports_Transports[i]);
    };
  };
  return res;
};



/*----------------------------------------------------------------------------*/
xdrTransports_PipeDescriptor * xdrTransports_CreatePipeDescriptor(int pipeID){
  xdrTransports_PipeDescriptor * pipeDesc;

  pipeDesc = xdrMM_AllocRec(xdrTransports_PipeDescriptor);
  pipeDesc->pipeID = pipeID;
  return pipeDesc;
};



/*-------------------------------------------------------*/
void xdrTransports_DestroyPipeDescriptor(xdrTransports_PipeDescriptor * pipeDesc){
  close(pipeDesc->pipeID);
  xdrMM_Free(pipeDesc);
};




/*-------------------------------------------------------*/
void xdrTransports_NotifyKernel(
  int                                 msgID, 
  xdrTransports_TransportDescriptor * senderDesc,
  int                                 arg0
){
  xdrMessages_Message msg;

  msg.Header.senderDesc = senderDesc;

  switch(msgID){
    case xdrTransports_MsgConnectionRequest:
         msg.Header.mode = msgMode_ConnectionRequest;
         msg.Body.ConnectionRequest.pipeDesc = xdrTransports_CreatePipeDescriptor(arg0);
         xdrMessages_PutAsyncMessage(xdrKernel_InQueue, &msg);
         break;
    default:
         perror("Unknown message identifier");
    case xdrTransports_MsgTransportFailure:
         break;
  };

};






/*-------------------------------------------------------*/
/* Read and write functions (like standard ANSI C functions) */
int xdrTransports_Write(
  xdrTransports_PipeDescriptor * pipeDesc,
  int                            channelNo,
  char                         * buffer,
  int                            nbytes
){
  return write(pipeDesc->pipeID, buffer, nbytes);
};





/*-------------------------------------------------------*/
int xdrTransports_Read(
  xdrTransports_PipeDescriptor * pipeDesc,
  int                            channelNo,
  char                         * buffer,
  int                            maxbytes
){
  return read(pipeDesc->pipeID, buffer, maxbytes);
};

