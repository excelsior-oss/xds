/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrMessages.c                                              *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This file contains implementation of message queues.       *|
|*                                                                            *|
\******************************************************************************/


#include <msgQLib.h>



#include "xdrIncl.h"
#include "xdrTrace.h"
#include "xdrTypes.h"
#include "xdrAssert.h"
#include "xdrMemoryManager.h"
#include "xdrMessages.h"


struct xdrMessages_tagMessageQueue{
  MSG_Q_ID queue;
  int      timeout;
};



/* Create message queue. */
xdrMessages_MessageQueue * xdrMessages_CreateMessageQueue(int timeout){
  MSG_Q_ID queueID;
  xdrMessages_MessageQueue * queue = NULL;


  if((queueID = msgQCreate(xdrMessages_MaxMessages, xdrMessages_MessageSize, MSG_Q_FIFO)) == NULL) return NULL;

  queue = xdrMM_AllocRec(xdrMessages_MessageQueue);
  queue->queue   = queueID;
  queue->timeout = timeout;
  return queue;
};




/* Puts message to queue. Does not change the synchronization field
   of message */
void xdrMessages_PutMessage(
  xdrMessages_MessageQueue * queue, 
  xdrMessages_Message      * msg
){
  ASSERT(msgQSend(queue->queue, (char*)msg, xdrMessages_MessageSize, queue->timeout, MSG_PRI_NORMAL) == OK);
};






/* Puts message to queue. Cleans the synchronization field of message */
void xdrMessages_PutAsyncMessage(
  xdrMessages_MessageQueue * queue, 
  xdrMessages_Message      * msg
){
  msg->Header.sync = NULL;
  xdrMessages_PutMessage(queue, msg);
};






/* Puts message to queue. Allocates semaphore, assigns it to the synchronization 
   field of message and returns it as a result, sets address of returned result
*/
SEM_ID xdrMessages_PutSyncMessage(
  xdrMessages_MessageQueue * queue, 
  xdrMessages_Message      * msg,
  int                      * presult
){
  ASSERT((msg->Header.sync = semBCreate(SEM_Q_FIFO, SEM_EMPTY)) != NULL);
  msg->Header.result.p = presult;
  xdrMessages_PutMessage(queue, msg);
  return msg->Header.sync;
};






/* Calls xdrMessages_PutSyncMessage and waits for returned semaphore.
   Return result.
*/
int xdrMessages_SendMsg(
  xdrMessages_MessageQueue * queue, 
  xdrMessages_Message      * msg
){
  int result;
  SEM_ID sem;

  sem = xdrMessages_PutSyncMessage(queue, msg, &result);
  ASSERT(semTake(sem, WAIT_FOREVER) == OK);
  ASSERT(semDelete(sem) == OK);
  msg->Header.sync = NULL;
  return result;
};






/* Gets message from queue. */
void xdrMessages_GetMessage(
  xdrMessages_MessageQueue * queue, 
  xdrMessages_Message      * msg
){
  ASSERT(msgQReceive(queue->queue, (char*)msg, xdrMessages_MessageSize, WAIT_FOREVER) == xdrMessages_MessageSize);
};





/* Set result accordingly to message type */
void xdrMessages_SetResult(xdrMessages_Message * msg, int result){
  if(msg->Header.sync == NULL){
    msg->Header.result.v = result;
  }else{
    *(msg->Header.result.p) = result;
  };
};
