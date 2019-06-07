/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrClient.c                                                *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This file contains implementation of client.               *|
|*                                                                            *|
\******************************************************************************/

#include "xdrIncl.h"
#include "xdrTrace.h"
#include "xdrTypes.h"
#include "xdrKernelTypes.h"
#include "xdrAssert.h"
#include "xdrMagic.h"
#include "xdrClient.h"
#include "xdrCfg.h"
#include "xdrMessages.h"
#include "xdrMemoryManager.h"
#include "xdrTransports.h"
#include "xdrKernel.h"
#include "xdrIO.h"

/*----------------------------------------------------------------------------*/
#define InQueueHandlerTaskNamePttr  "xdrClient_InQueueHandlerTask%d"
#define CommandsHandlerTaskNamePttr "xdrClient_CommandsHandlerTask%d"

int     clientNumber = 0;




/*----------------------------------------------------------------------------*/
void xdrClient_InQueueHandler(xdrClient_ClientDescriptor * clientDesc){
  xdrMessages_Message  inMsg, outMsg;
  xdrKernelTypes_Event event;

  REG_SET regSet;
  int result, stoppingEvent;

  outMsg.Header.senderDesc = (void*)clientDesc;

  for(;;){
    /* Get message */
    xdrMessages_GetMessage(clientDesc->inQueue, &inMsg);

    stoppingEvent = False; 
    /* Process message */
    switch(inMsg.Header.mode){
      case msgMode_EventDeleteTask:
           event.Header.eventType = xdrKernelTypes_EventType_Exception;
           event.Header.pc        = 0;
           event.Body.Exception.exceptionID = xdrKernelTypes_ExceptionID_ProgramException;
           event.Body.Exception.XCPT_INFO_1 = 0;
           event.Body.Exception.XCPT_INFO_3 = 0;
           stoppingEvent = True; 
           break;
      case msgMode_EventSingleStep:
           /* Get registers */
           outMsg.Header.mode               = msgMode_GetRegisters;
           outMsg.Body.GetRegisters.taskID  = inMsg.Body.EventSingleStep.currentTaskID;
           outMsg.Body.GetRegisters.pregSet = &regSet;
           result = xdrMessages_SendMsg(xdrKernel_InQueue, &outMsg);
           if(result != xdrTypes_errOK){
             printf("xdrClient_InQueueHandler: 1\n");
             continue;
           };

           event.Header.eventType = xdrKernelTypes_EventType_SingleStep;
           event.Header.pc        = (DWORD)regSet.pc;
           stoppingEvent = True;
           break;
      case msgMode_EventBreakpointHit:
           /* Get registers */
           outMsg.Header.mode               = msgMode_GetRegisters;
           outMsg.Body.GetRegisters.taskID  = inMsg.Body.EventBreakpointHit.currentTaskID;
           outMsg.Body.GetRegisters.pregSet = &regSet;
           result = xdrMessages_SendMsg(xdrKernel_InQueue, &outMsg);
           if(result != xdrTypes_errOK){
             printf("xdrClient_InQueueHandler: 1\n");
             continue;
           };

           event.Header.eventType = xdrKernelTypes_EventType_BreakpointHit;
           event.Header.pc        = (DWORD)regSet.pc - 2;
           event.Body.BreakpointHit.BreakpointInd = 0;
           stoppingEvent = True;
           break;
/*
      case :
           break;
*/
      default:
           printf("xdrClient (InQueue): unknown message mode %d\n", inMsg.Header.mode);
           continue;
    };


    if(msgQSend(clientDesc->eventsQueue, (char*)&event, sizeof(xdrKernelTypes_Event), NO_WAIT, MSG_PRI_NORMAL) == ERROR){
      /* Fatal */
      printf("xdrClient_InQueueHandler: 2\n");
      continue;
    }else if(stoppingEvent){
      semGive(clientDesc->eventSem);
      if(semTake(clientDesc->eventSem1, WAIT_FOREVER) == ERROR){
        /* Fatal */
        printf("xdrClient_InQueueHandler: 3\n");
      };
    };
  };
};


/*-------------------------------------------------------*/
void xdrClient_InQueueHandlerTask(xdrClient_ClientDescriptor * clientDesc){
  xdrClient_InQueueHandler(clientDesc);
};



/*----------------------------------------------------------------------------*/
void xdrClient_StartProgram_Result(xdrClient_ClientDescriptor * clientDesc, int result){
  xdrTransports_PipeDescriptor * pipeDesc;

  pipeDesc = clientDesc->pipeDesc;
  if(SendEvents(DummyChannel, result, clientDesc->eventsQueue) == ERROR) printf("Send result error\n");
};

void xdrClient_StartProgram(xdrClient_ClientDescriptor * clientDesc){
  xdrKernelTypes_ProgramName progName;
  xdrKernelTypes_ProgramArgs progArgs;
  xdrMessages_Message        msg;
  xdrKernelTypes_Event       event;

  int     i;
  DWORD   num = 0;
  int   * moduleIDs = NULL;
  int     result;

  xdrKernelTypes_ModuleInfo minfo;


  msg.Header.senderDesc = (void*)clientDesc;

  if(!clientDesc->programStarted){

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: start program\n", clientDesc->number);
#endif

    strcpy(progName, clientDesc->arg_buf);
    strcpy(progArgs, clientDesc->arg_buf + strlen(clientDesc->arg_buf));
    printf("name: %s\n", progName);
    printf("args: %s\n", progArgs);


    /* Load module */
    msg.Header.mode               = msgMode_LoadModule;
    msg.Body.LoadModule.pmoduleID = &(clientDesc->moduleID);
    strcpy(msg.Body.LoadModule.moduleName, progName);
    result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
    if(result != xdrTypes_errOK){
      xdrClient_StartProgram_Result(clientDesc, result);
      return;
    };

    /* Get module info */
    msg.Header.mode                    = msgMode_GetModuleInfo;
    msg.Body.GetModuleInfo.moduleID    = clientDesc->moduleID;
    msg.Body.GetModuleInfo.force       = False;
    msg.Body.GetModuleInfo.pmoduleInfo = &(clientDesc->moduleInfo);
    result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
    if(result != xdrTypes_errOK){
      xdrClient_StartProgram_Result(clientDesc, result);
      return;
    };

   
    /* Find and set entry point */
    msg.Header.mode           = msgMode_FindSymbol;
    msg.Body.FindSymbol.paddr = &(clientDesc->moduleInfo.MainEntry);
    strcpy(msg.Body.FindSymbol.symbolName, "_Test_Func");
    result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
    if(result != xdrTypes_errOK){
      xdrClient_StartProgram_Result(clientDesc, result);
      return;
    };



    /* Get loaded modules info */
    msg.Header.mode                          = msgMode_GetLoadedModulesList;
    msg.Body.GetLoadedModulesList.pnum       = &num;
    msg.Body.GetLoadedModulesList.pmoduleIDs = &moduleIDs;
    result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
    if(result != xdrTypes_errOK){
      xdrClient_StartProgram_Result(clientDesc, result);
      return;
    };
    for(i = 0; i < num; i++){
      if(moduleIDs[i] == clientDesc->moduleID) continue;

      /* Get module info */
      msg.Header.mode                    = msgMode_GetModuleInfo;
      msg.Body.GetModuleInfo.moduleID    = moduleIDs[i];
      msg.Body.GetModuleInfo.force       = True;
      msg.Body.GetModuleInfo.pmoduleInfo = &(event.Body.ComponentCreated.Component);
      result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
      if(result != xdrTypes_errOK){
        printf("xdrClient: error during getting loaded modules info [1].\n");
        continue;
      };

      event.Header.eventType = xdrKernelTypes_EventType_ComponentCreated;
      event.Body.ComponentCreated.Stopable = TRUE;
      if(msgQSend(clientDesc->eventsQueue, (char*)&event, sizeof(xdrKernelTypes_Event), NO_WAIT, MSG_PRI_NORMAL) == ERROR){
        printf("xdrClient: error during getting loaded modules info [2].\n");
        continue;
      };
      printf("xdrClient: component created: %s\n", event.Body.ComponentCreated.Component.full_name);
    };
    if(moduleIDs != NULL) xdrMM_Free(moduleIDs);
  
    /* Create task */
    msg.Header.mode                = msgMode_CreateTask;
    msg.Body.CreateTask.entryPoint = (void*)(clientDesc->moduleInfo.MainEntry);
    msg.Body.CreateTask.ptaskID    = &(clientDesc->debuggedTaskID);
    strcpy(msg.Body.CreateTask.taskName, "tTest");
    result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
    if(result != xdrTypes_errOK){
      xdrClient_StartProgram_Result(clientDesc, result);
      return;
    };

    xdrClient_StartProgram_Result(clientDesc, xdrTypes_errOK);
  }else{

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: error: second \"Start program\" command.\n", clientDesc->number);
#endif

    xdrClient_StartProgram_Result(clientDesc, xdrTypes_errInvalidCommand);
  };
};

/*-------------------------------------------------------*/
void xdrClient_GetExecInfo(xdrClient_ClientDescriptor * clientDesc){
  xdrTransports_PipeDescriptor * pipeDesc;
  int i;

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: get exec information\n", clientDesc->number);
#endif

  pipeDesc = clientDesc->pipeDesc;

  if(SendResult(DummyChannel, xdrTypes_errOK, xdrKernelTypes_ModuleInfoSize + clientDesc->moduleInfo.N_Objects*xdrKernelTypes_ObjectSize) == ERROR) printf("Send result error\n");
  if(SendModuleInfo(DummyChannel, clientDesc->moduleInfo) == ERROR) printf("Send result error\n");
  for(i = 0; i < clientDesc->moduleInfo.N_Objects; i++){
    if(SendObjectInfo(DummyChannel, clientDesc->moduleInfo.Objects[i]) == ERROR) printf("Send result error\n");
  };
};

/*-------------------------------------------------------*/
void xdrClient_GetDebugInfo(xdrClient_ClientDescriptor * clientDesc){

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: get debug information\n", clientDesc->number);
#endif

};

/*-------------------------------------------------------*/
void xdrClient_QuitDebug(xdrClient_ClientDescriptor * clientDesc){
  xdrTransports_PipeDescriptor * pipeDesc;

  pipeDesc = clientDesc->pipeDesc;

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: quit debug\n", clientDesc->number);
#endif

  if(SendResult(DummyChannel, xdrTypes_errOK, 0) == ERROR) printf("Send result error\n");
};

/*-------------------------------------------------------*/
void xdrClient_GetRegisterCache(xdrClient_ClientDescriptor * clientDesc){
  xdrMessages_Message            msg;
  xdrTransports_PipeDescriptor * pipeDesc;
  REG_SET regSet;
  int result;

  msg.Header.senderDesc = (void*)clientDesc;
  pipeDesc = clientDesc->pipeDesc;

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: get register cashe\n", clientDesc->number);
#endif

  if(!clientDesc->programStarted){

    /* Get registers */
    msg.Header.mode               = msgMode_GetRegisters;
    msg.Body.GetRegisters.taskID  = clientDesc->debuggedTaskID;
    msg.Body.GetRegisters.pregSet = &regSet;
    result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
    if(result != xdrTypes_errOK){
      if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
      return;
    };

    if(SendResult(DummyChannel, xdrTypes_errOK, 18*4) == ERROR) printf("Send result error\n");

    if(SendDW(DummyChannel, regSet.dataReg[0]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.dataReg[1]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.dataReg[2]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.dataReg[3]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.dataReg[4]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.dataReg[5]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.dataReg[6]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.dataReg[7]) == ERROR) printf("Send result error\n");

    if(SendDW(DummyChannel, regSet.addrReg[0]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.addrReg[1]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.addrReg[2]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.addrReg[3]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.addrReg[4]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.addrReg[5]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.addrReg[6]) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, regSet.addrReg[7]) == ERROR) printf("Send result error\n");

    if(SendDW(DummyChannel, *(DWORD*)(((BYTE*)&regSet) + SR_OFFSET)) == ERROR) printf("Send result error\n");

    if(SendDW(DummyChannel, (DWORD)regSet.pc) == ERROR) printf("Send result error\n");
  }else{
    if(SendResult(DummyChannel, xdrTypes_errInvalidCommand, 0) == ERROR) printf("Send result error\n");
  };
};

/*-------------------------------------------------------*/
void xdrClient_PutRegisterCache(xdrClient_ClientDescriptor * clientDesc){
  xdrMessages_Message            msg;
  xdrTransports_PipeDescriptor * pipeDesc;
  REG_SET regSet;
  int result;

  msg.Header.senderDesc = (void*)clientDesc;
  pipeDesc = clientDesc->pipeDesc;

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: put register cashe\n", clientDesc->number);
#endif

  if(!clientDesc->programStarted){

    regSet.dataReg[0] = DWChEnd(*(DWORD*)(clientDesc->arg_buf));
    regSet.dataReg[1] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+4));
    regSet.dataReg[2] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+8));
    regSet.dataReg[3] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+12));
    regSet.dataReg[4] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+16));
    regSet.dataReg[5] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+20));
    regSet.dataReg[6] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+24));
    regSet.dataReg[7] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+28));

    regSet.addrReg[0] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+32));
    regSet.addrReg[1] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+36));
    regSet.addrReg[2] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+40));
    regSet.addrReg[3] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+44));
    regSet.addrReg[4] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+48));
    regSet.addrReg[5] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+52));
    regSet.addrReg[6] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+56));
    regSet.addrReg[7] = DWChEnd(*(DWORD*)(clientDesc->arg_buf+60));

    *(DWORD*)(((BYTE*)&regSet)+SR_OFFSET) = DWChEnd(*(DWORD*)(clientDesc->arg_buf+64));
    regSet.pc         = (INSTR*)DWChEnd(*(DWORD*)(clientDesc->arg_buf+68));

    /* Put registers */
    msg.Header.mode               = msgMode_PutRegisters;
    msg.Body.GetRegisters.taskID  = clientDesc->debuggedTaskID;
    msg.Body.GetRegisters.pregSet = &regSet;
    result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
    if(result != xdrTypes_errOK){
      if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
    }else{
      if(SendResult(DummyChannel, xdrTypes_errOK, 0) == ERROR) printf("Send result error\n");
    };
  }else{
    if(SendResult(DummyChannel, xdrTypes_errInvalidCommand, 0) == ERROR) printf("Send result error\n");
  };
};

/*-------------------------------------------------------*/
void xdrClient_GetMem(xdrClient_ClientDescriptor * clientDesc){
  xdrMessages_Message            msg;
  xdrTransports_PipeDescriptor * pipeDesc;
  int result;
  DWORD len;
  void * buff;

  msg.Header.senderDesc = (void*)clientDesc;
  pipeDesc = clientDesc->pipeDesc;

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: get memory\n", clientDesc->number);
#endif

  /* Get memory */
  msg.Header.mode          = msgMode_GetMemory;
  msg.Body.GetMemory.addr  = DWChEnd(*(DWORD*)(clientDesc->arg_buf));
  msg.Body.GetMemory.len   = len = DWChEnd(*(DWORD*)(clientDesc->arg_buf+4));
  msg.Body.GetMemory.pbuff = &buff;
  result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
  if(result != xdrTypes_errOK){
    if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
    return;
  };

  if(SendResult(DummyChannel, xdrTypes_errOK, len) == ERROR) printf("Send result error\n");
  if(Send(DummyChannel, buff, len) == ERROR) printf("Send result error\n");
};

/*-------------------------------------------------------*/
void xdrClient_PutMem(xdrClient_ClientDescriptor * clientDesc){
  xdrMessages_Message            msg;
  xdrTransports_PipeDescriptor * pipeDesc;
  int result;
  DWORD len;
  void * buff;

  msg.Header.senderDesc = (void*)clientDesc;
  pipeDesc = clientDesc->pipeDesc;

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: put memory\n", clientDesc->number);
#endif

  /* Put memory */
  msg.Header.mode         = msgMode_PutMemory;
  msg.Body.PutMemory.addr = DWChEnd(*(DWORD*)(clientDesc->arg_buf));
  msg.Body.PutMemory.len  = len = clientDesc->arg_size - 4;
  msg.Body.PutMemory.buff = (void*)(clientDesc->arg_buf + 4);
  result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
  if(result != xdrTypes_errOK){
    if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
    return;
  };

  if(SendResult(DummyChannel, xdrTypes_errOK, 0) == ERROR) printf("Send result error\n");
};

/*-------------------------------------------------------*/
int xdrClient_GetGoMode(
  xdrClient_ClientDescriptor * clientDesc,
  xdrKernelTypes_GoMode      * goMode
){
  BYTE * buff;
  DWORD  size;

  size = clientDesc->arg_size;
  buff = clientDesc->arg_buf;

  if(size <= 0) return xdrTypes_errBadArguments;

  goMode->Header.mode = *buff;
  size--; buff++;
  if(size < 0) return xdrTypes_errBadArguments;

  switch(goMode->Header.mode){
    case xdrKernelTypes_GoMode_SingleStep:
      goMode->Body.SingleStep.add_step = *buff;
      size--; buff++;
      if(size <= 0) return xdrTypes_errBadArguments;
      break;
    case xdrKernelTypes_GoMode_RangeStep:
      goMode->Body.RangeStep.Begin = DWChEnd(*(DWORD*)buff);
      size -= 4; buff +=4;
      if(size <= 0) return xdrTypes_errBadArguments;
      goMode->Body.RangeStep.End = DWChEnd(*(DWORD*)buff);
      size -= 4; buff +=4;
      if(size <= 0) return xdrTypes_errBadArguments;
      break;
    case xdrKernelTypes_GoMode_Go:
      break;
    default:
      return xdrTypes_errBadArguments;
  };
  return xdrTypes_errOK;
};




void xdrClient_Execute(xdrClient_ClientDescriptor * clientDesc){
  xdrMessages_Message            msg;
  xdrTransports_PipeDescriptor * pipeDesc;
  xdrKernelTypes_GoMode          goMode;
  int result;

  msg.Header.senderDesc = (void*)clientDesc;
  pipeDesc = clientDesc->pipeDesc;

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: execute program\n", clientDesc->number);
#endif

  result = xdrClient_GetGoMode(clientDesc, &goMode);
  if(result != xdrTypes_errOK){
    if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
    return;
  };

  switch(goMode.Header.mode){
    case xdrKernelTypes_GoMode_SingleStep:
      /* Set trace mode */
      msg.Header.mode              = msgMode_SetTraceMode;
      msg.Body.SetTraceMode.taskID = clientDesc->debuggedTaskID;
      result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
      if(result != xdrTypes_errOK){
        if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
        return;
      };

      /* Resume task */
      msg.Header.mode            = msgMode_ResumeTask;
      msg.Body.ResumeTask.taskID = clientDesc->debuggedTaskID;
      result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
      if(result != xdrTypes_errOK){
        if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
        return;
      };

      break;
    case xdrKernelTypes_GoMode_RangeStep:
      result = xdrTypes_errUnknown;
      break;
    case xdrKernelTypes_GoMode_Go:
      /* Reset trace mode */
      msg.Header.mode                = msgMode_ResetTraceMode;
      msg.Body.ResetTraceMode.taskID = clientDesc->debuggedTaskID;
      result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
      if(result != xdrTypes_errOK){
        if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
        return;
      };

      /* Resume task */
      msg.Header.mode            = msgMode_ResumeTask;
      msg.Body.ResumeTask.taskID = clientDesc->debuggedTaskID;
      result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
      if(result != xdrTypes_errOK){
        if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
        return;
      };

      break;
  };

  if(result != xdrTypes_errOK){
    if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
    return;
  };

  if(semTake(clientDesc->eventSem, WAIT_FOREVER) == ERROR){
    /* Fatal */
    if(SendResult(DummyChannel, xdrTypes_errUnknown, 0) == ERROR) printf("Send result error\n");
    return;
  };

  if(SendEvents(DummyChannel, result, clientDesc->eventsQueue) == ERROR) printf("Send result error\n");
  semGive(clientDesc->eventSem1);
};

/*-------------------------------------------------------*/
void xdrClient_GetSegmentInfo(xdrClient_ClientDescriptor * clientDesc){
  xdrMessages_Message            msg;
  xdrTransports_PipeDescriptor * pipeDesc;
  int result;
  DWORD begin, len;
  xdrKernelTypes_Attribs attr;

  msg.Header.senderDesc = (void*)clientDesc;
  pipeDesc = clientDesc->pipeDesc;
 
#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: get segment info\n", clientDesc->number);
#endif

  if(clientDesc->arg_size == 4){
    /* Get segment info */
    msg.Header.mode                = msgMode_GetSegmentInfo;
    msg.Body.GetSegmentInfo.addr   = DWChEnd(*(DWORD*)(clientDesc->arg_buf));
    msg.Body.GetSegmentInfo.pbegin = &begin;
    msg.Body.GetSegmentInfo.plen   = &len;
    msg.Body.GetSegmentInfo.pattr  = &attr;
    result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
    if(result != xdrTypes_errOK){
      if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
      return;
    };

    if(SendResult(DummyChannel, xdrTypes_errOK, 9) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, begin) == ERROR) printf("Send result error\n");
    if(SendDW(DummyChannel, len)   == ERROR) printf("Send result error\n");
    if(SendB(DummyChannel,  attr)  == ERROR) printf("Send result error\n");
  }else{
    if(SendResult(DummyChannel, xdrTypes_errBadArguments, 0) == ERROR) printf("Send result error\n");
    return;
  };
};

/*-------------------------------------------------------*/
void xdrClient_IsExecutableSeg(xdrClient_ClientDescriptor * clientDesc){
  xdrTransports_PipeDescriptor * pipeDesc;

  pipeDesc = clientDesc->pipeDesc;

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: is executable segment\n", clientDesc->number);
#endif

  if(SendResult(DummyChannel, xdrTypes_errOK, 1) == ERROR) printf("Send result error\n");
  if(SendB(DummyChannel, True) == ERROR) printf("Send result error\n");
};

/*-------------------------------------------------------*/
void xdrClient_ReadExport(xdrClient_ClientDescriptor * clientDesc){
  xdrTransports_PipeDescriptor * pipeDesc;
  xdrMessages_Message            msg;
  xdrKernelTypes_Exported      * export;
  xdrKernelTypes_ModuleInfo      moduleInfo;
  int                            moduleID;
  DWORD                          expLen;
  int                            result;
  int                            i;

  msg.Header.senderDesc = (void*)clientDesc;
  pipeDesc = clientDesc->pipeDesc;

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: read export\n", clientDesc->number);
#endif

  /* Get module ID */
  msg.Header.mode                = msgMode_GetModuleID;
  msg.Body.GetModuleID.pmoduleID = &(moduleID);
  strcpy(msg.Body.GetModuleID.moduleName, clientDesc->arg_buf);
  result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
  if(result != xdrTypes_errOK){
    if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
    return;
  };


  /* Get module info */
  msg.Header.mode                    = msgMode_GetModuleInfo;
  msg.Body.GetModuleInfo.moduleID    = moduleID;
  msg.Body.GetModuleInfo.force       = True;
  msg.Body.GetModuleInfo.pmoduleInfo = &(moduleInfo);
  result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
  if(result != xdrTypes_errOK){
    if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
    return;
  };

  /* Get module symbols */
  msg.Header.mode                    = msgMode_GetModuleSymbols;
  msg.Body.GetModuleSymbols.pExport  = &(export);
  msg.Body.GetModuleSymbols.pexpLen  = &(expLen);
  msg.Body.GetModuleSymbols.moduleID = moduleID;
  result = xdrMessages_SendMsg(xdrKernel_InQueue, &msg);
  if(result != xdrTypes_errOK){
    if(SendResult(DummyChannel, result, 0) == ERROR) printf("Send result error\n");
    return;
  };


  /* Set object numbers and offsets */
  for(i = 0; i < expLen; i++){
    if(export[i].obj == 5 &&
      export[i].offset >= moduleInfo.Objects[0].Begin &&
      export[i].offset < moduleInfo.Objects[0].End)
    {
      export[i].obj = 1;
      export[i].offset -= moduleInfo.Objects[0].Begin;
    }else{
      export[i].obj = 0;
    };
  };

  printf("%s: symbol number = %d\n", clientDesc->arg_buf, expLen);
  if(SendExport(DummyChannel, export, expLen) == ERROR) printf("Send result error\n");

  xdrMM_Free(export);
};

/*-------------------------------------------------------*/
void xdrClient_GetThreadDescription(xdrClient_ClientDescriptor * clientDesc){

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: get thread description\n", clientDesc->number);
#endif

};

/*-------------------------------------------------------*/
void xdrClient_SwitchToThread(xdrClient_ClientDescriptor * clientDesc){

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: switch to thread\n", clientDesc->number);
#endif

};

/*-------------------------------------------------------*/
void xdrClient_SuspendThread(xdrClient_ClientDescriptor * clientDesc){

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: suspend thread\n", clientDesc->number);
#endif

};

/*-------------------------------------------------------*/
void xdrClient_ResumeThread(xdrClient_ClientDescriptor * clientDesc){

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: resume thread\n", clientDesc->number);
#endif

};

/*-------------------------------------------------------*/
void xdrClient_UnknownCommand(xdrClient_ClientDescriptor * clientDesc){
  xdrTransports_PipeDescriptor * pipeDesc;

  pipeDesc = clientDesc->pipeDesc;

#if xdrTrace_Mode & xdrTrace_modeCommand
  xdrTrace_Trace("Client %d: unknown command\n", clientDesc->number);
#endif

  SendResult(DummyChannel, xdrTypes_errUnknownCommand, 0);
};

/*-------------------------------------------------------*/
void xdrClient_CommandsHandler(xdrClient_ClientDescriptor * clientDesc){
  xdrTransports_PipeDescriptor * pipeDesc;
  xdrIO_Command command;


  pipeDesc = clientDesc->pipeDesc;
  for(;;){

#if xdrTrace_Mode & xdrTrace_modeCommand
    xdrTrace_Trace("Client %d: waiting for command...\n", clientDesc->number);
#endif

    command = ReceiveCommand(DummyChannel, &(clientDesc->arg_size));

#if xdrTrace_Mode & xdrTrace_modeCommand
    xdrTrace_Trace("Client %d: arg size = %d\n", clientDesc->number, clientDesc->arg_size);
#endif

    if(clientDesc->arg_size != 0 && Receive(DummyChannel, clientDesc->arg_buf, clientDesc->arg_size) == ERROR){

#if xdrTrace_Mode & xdrTrace_modeCommand
      xdrTrace_Trace("Client %d: command arguments were not received.\n", clientDesc->number);
#endif

      continue;
    };

#if xdrTrace_Mode & xdrTrace_modeCommand
    xdrTrace_Trace("Client %d: command %d\n", clientDesc->number, command);
#endif

    switch(command){
      case cmdNone:
        SendResult(DummyChannel, xdrTypes_errOK, 0);
        break;
      case cmdStartProgram:
        xdrClient_StartProgram(clientDesc);
        break;
      case cmdGetExecInfo:
        xdrClient_GetExecInfo(clientDesc);
        break;
      case cmdGetDebugInfo:
        xdrClient_GetDebugInfo(clientDesc);
        break;
      case cmdQuitDebug:
        xdrClient_QuitDebug(clientDesc);
        break;
      case cmdGetRegisterCache:
        xdrClient_GetRegisterCache(clientDesc);
        break;
      case cmdPutRegisterCache:
        xdrClient_PutRegisterCache(clientDesc);
        break;
      case cmdGetMem:
        xdrClient_GetMem(clientDesc);
        break;
      case cmdPutMem:
        xdrClient_PutMem(clientDesc);
        break;
      case cmdExecute:
        xdrClient_Execute(clientDesc);
        break;
      case cmdGetSegmentInfo:
        xdrClient_GetSegmentInfo(clientDesc);
        break;
      case cmdIsExecutableSeg:
        xdrClient_IsExecutableSeg(clientDesc);
        break;
      case cmdReadExport:
        xdrClient_ReadExport(clientDesc);
        break;
      case cmdGetThreadDescription:
        xdrClient_GetThreadDescription(clientDesc);
        break;
      case cmdSwitchToThread:
        xdrClient_SwitchToThread(clientDesc);
        break;
      case cmdSuspendThread:
        xdrClient_SuspendThread(clientDesc);
        break;
      case cmdResumeThread:
        xdrClient_ResumeThread(clientDesc);
        break;
      default:
        xdrClient_UnknownCommand(clientDesc);
        break;
    };

#if xdrTrace_Mode & xdrTrace_modeCommand
    xdrTrace_Trace("\n");
#endif

  };
};


/*-------------------------------------------------------*/
void xdrClient_CommandsHandlerTask(xdrClient_ClientDescriptor * clientDesc){
  xdrClient_CommandsHandler(clientDesc);
};



/*----------------------------------------------------------------------------*/
/* Procedure determining client tasks */
int xdrClient_IsClientTask(
  xdrClient_ClientDescriptor   * clientDesc,
  int				 taskID
){
  return (clientDesc->inQueueTaskID == taskID) ||
         (clientDesc->commandsTaskID == taskID);
};



/*----------------------------------------------------------------------------*/
/* Procedure to create client. 
   Receives two paremeters: 'pipeDesc' that is pipe to interact with remote 
   client and pointer to client descriptor that will be filled.
   Returns True if a client is successfully created and False if some errors 
   have happend.
*/
int xdrClient_Create(
  xdrClient_ClientDescriptor   * clientDesc,
  xdrTransports_PipeDescriptor * pipeDesc
){
  char name[64];

  clientDesc->magic    = xdrMagic_ClientDescriptor;
  clientDesc->number   = clientNumber++;
  clientDesc->pipeDesc = pipeDesc;

  /* Create input queue */
  if((clientDesc->inQueue = xdrMessages_CreateMessageQueue(WAIT_FOREVER)) == NULL) return False;

  /* Create events queue */
  if((clientDesc->eventsQueue = msgQCreate(MAX_EVENTS, sizeof(xdrKernelTypes_Event), MSG_Q_FIFO)) == NULL) return False;

  /* Create event semaphore */
  if((clientDesc->eventSem = semBCreate(SEM_Q_FIFO, SEM_EMPTY)) == NULL){
    printf("Can't create event semaphore.\n");
    return False;
  };
  if((clientDesc->eventSem1 = semBCreate(SEM_Q_FIFO, SEM_EMPTY)) == NULL){
    printf("Can't create event semaphore.\n");
    return False;
  };

  /* Spawn input queue handler task */
  sprintf(name, InQueueHandlerTaskNamePttr, clientDesc->number);
  if((clientDesc->inQueueTaskID = taskSpawn(name, 
                 xdrTypes_TaskWorkPriority, 0, xdrTypes_TaskStackSize,
                 (FUNCPTR)xdrClient_InQueueHandlerTask, (int)clientDesc, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == ERROR){
    printf("taskSpawn: xdrClient InQueueTask\n");
    return False;
  };

  /* Spawn commands handler task */
  sprintf(name, CommandsHandlerTaskNamePttr, clientDesc->number);
  if((clientDesc->commandsTaskID = taskSpawn(name, 
                 xdrTypes_TaskWorkPriority, 0, xdrTypes_TaskStackSize,
                 (FUNCPTR)xdrClient_CommandsHandlerTask, (int)clientDesc, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == ERROR){
    printf("taskSpawn: xdrClient CommandsTask\n");
    return False;
  };

  return True;
};


