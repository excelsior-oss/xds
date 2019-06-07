/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrMessages.h                                              *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains interface to message queues      *|
|*                 (definitions of message type and modes, message queues and *|
|*                 management functions).                                     *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrMessages_h
#define _xdrMessages_h



#include <semLib.h>
#include <regs.h>


#include "xdrTypes.h"
#include "xdrKernelTypes.h"
#include "xdrMagic.h"
#include "xdrTransports.h"


/*********************         Message definitions         ********************/

/* Message queue parameters */
#define xdrMessages_MaxMessages  (256)
#define xdrMessages_MessageSize  (sizeof(xdrMessages_Message))



/* Message modes */
#define msgMode_ConnectionRequest    0
#define msgMode_LoadModule           1
#define msgMode_GetModuleInfo        2
#define msgMode_FindSymbol           3
#define msgMode_GetLoadedModulesList 4
#define msgMode_CreateTask           5
#define msgMode_GetRegisters         6
#define msgMode_PutRegisters         7
#define msgMode_GetModuleID          8
#define msgMode_GetModuleSymbols     9
#define msgMode_GetMemory            10
#define msgMode_PutMemory            11
#define msgMode_GetSegmentInfo       12
#define msgMode_SetTraceMode         13
#define msgMode_ResetTraceMode       14
#define msgMode_ResumeTask           15
                       
#define msgMode_EventUnknown         100
#define msgMode_EventDeleteTask      101
#define msgMode_EventSingleStep      102
#define msgMode_EventBreakpointHit   103


/* Message type */
typedef struct xdrMessages_tagMessage {

  /* Header of message (message mode independent data) */
  struct{
    int       mode;       /* Message mode */
    void   *  senderDesc; /* Sender identifier */
    SEM_ID    sync;       /* Semaphore for synchronous message,
                             NULL for asynchronous message */
    union{
      int * p;            /* Result for synchronous message  */
      int   v;            /* Result for asynchronous message */
    } result;
  } Header;   

  /* Body of message (message mode dependent data) */
  union{

    /* Connection request: is sent by transport module to kernel. */
    struct{
      xdrTransports_PipeDescriptor * pipeDesc;
    } ConnectionRequest; 

    /* Following messages are sent by client to kernel */
    struct{
      xdrKernelTypes_ProgramName   moduleName;
      int                  * pmoduleID;
    } LoadModule; 

    struct{
      int                         moduleID;
      int                         force; /* If there is no module info try 
                                            to examine module symbols */
      xdrKernelTypes_ModuleInfo * pmoduleInfo;
    } GetModuleInfo; 

    struct{
      xdrKernelTypes_SymbolName   symbolName;
      DWORD               * paddr;
    } FindSymbol; 

    struct{
      DWORD *  pnum;
      int   ** pmoduleIDs;
    } GetLoadedModulesList; 

    struct{
      xdrKernelTypes_ProgramName   taskName;
      void                       * entryPoint;
      int                        * ptaskID;
    } CreateTask; 

    struct{
      int	taskID;
      REG_SET * pregSet;
    } GetRegisters; 

    struct{
      int	taskID;
      REG_SET * pregSet;
    } PutRegisters; 

    struct{
      xdrKernelTypes_ProgramName   moduleName;
      int                        * pmoduleID;
    } GetModuleID; 

    struct{
      int                        moduleID;
      xdrKernelTypes_Exported ** pExport;
      DWORD                   *  pexpLen;
    } GetModuleSymbols; 

    struct{
      DWORD   addr;
      DWORD   len;
      void ** pbuff;
    } GetMemory; 

    struct{
      DWORD  addr;
      DWORD  len;
      void * buff;
    } PutMemory; 

    struct{
      DWORD                    addr;
      DWORD                  * pbegin;
      DWORD                  * plen;
      xdrKernelTypes_Attribs * pattr;
    } GetSegmentInfo; 

    struct{
      int taskID;
    } ResetTraceMode; 

    struct{
      int taskID;
    } SetTraceMode; 

    struct{
      int taskID;
    } ResumeTask; 


    /* Following messages are sent by TA hooks module to Target Agent */
    struct{
      int currentTaskID;
    } EventUnknown;

    struct{
      int deletedTaskID;
      int deletingTaskID;
    } EventDeleteTask;

    struct{
      int currentTaskID;
    } EventSingleStep;

    struct{
      int currentTaskID;
    } EventBreakpointHit;


  } Body;

} xdrMessages_Message;




/*********************     Message  queue  definitions     ********************/

typedef struct xdrMessages_tagMessageQueue xdrMessages_MessageQueue;


/* Create message queue. 'timeout' is put message timeout.
   Returns NULL if error.
*/
extern xdrMessages_MessageQueue * xdrMessages_CreateMessageQueue(int timeout);

/* Puts message to queue. Does not change the synchronization field
   of message */
extern void xdrMessages_PutMessage(
  xdrMessages_MessageQueue * queue, 
  xdrMessages_Message      * msg
);

/* Puts message to queue. Cleans the synchronization field of message */
extern void xdrMessages_PutAsyncMessage(
  xdrMessages_MessageQueue * queue, 
  xdrMessages_Message      * msg
);

/* Alias for xdrMessages_PutAsyncMessage */
#define xdrMessages_PostMsg xdrMessages_PutAsyncMessage

/* Puts message to queue. Allocates semaphore, assigns it to the synchronization 
   field of message and returns it as a result, sets address of returned result
*/
extern SEM_ID xdrMessages_PutSyncMessage(
  xdrMessages_MessageQueue * queue, 
  xdrMessages_Message      * msg,
  int                      * presult
);

/* Calls xdrMessages_PutSyncMessage and waits for returned semaphore.
   Return result.
*/
extern int xdrMessages_SendMsg(
  xdrMessages_MessageQueue * queue, 
  xdrMessages_Message      * msg
);


/* Gets message from queue. */
extern void xdrMessages_GetMessage(
  xdrMessages_MessageQueue * queue, 
  xdrMessages_Message      * msg
);


/* Set result accordingly to message type */
extern void xdrMessages_SetResult(xdrMessages_Message * msg, int result);


/*********************    Duplex communicator attributes   ********************/
/* Duplex communicator descriptor */
typedef struct xdrMessages_tagDuplexCommunicatorDescriptor{
  int                        magic;   /* Magic number                        */
  xdrMessages_MessageQueue * inQueue; /* Incoming messages queue             */
} xdrMessages_DuplexCommunicatorDescriptor;

#define xdrMessages_IsDuplexCommunicator(desc) \
          (((xdrMessages_DuplexCommunicatorDescriptor*)(desc))->magic == xdrMagic_ClientDescriptor)
#define xdrMessages_DuplexCommunicatorInQueue(desc) \
          (xdrMessages_IsDuplexCommunicator(desc) ? ((xdrMessages_DuplexCommunicatorDescriptor*)(desc))->inQueue : NULL)



#endif