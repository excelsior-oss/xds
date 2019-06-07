/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrTargetAgent.c                                           *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This file contains implementation of target agent.         *|
|*                                                                            *|
\******************************************************************************/

#include <vxWorks.h>
#include <stdio.h>
#include <string.h>
#include <usrLib.h>
#include <unldLib.h>
#include <moduleLib.h>
#include <taskLib.h>
#include <sysLib.h>
#include <symLib.h>
#include <sysSymTbl.h>
#include <regs.h>


#include "xdrIncl.h"
#include "xdrTrace.h"
#include "xdrTypes.h"
#include "xdrKernelTypes.h"
#include "xdrAssert.h"
#include "xdrInit.h"
#include "xdrTAHooks.h"
#include "xdrTargetAgent.h"
#include "xdrMemoryManager.h"


/*----------------------------------------------------------------------------*\
|*                                                                            *|
|*                 Input and Output Queues and their Handlers                 *|
|*                                                                            *|
\*----------------------------------------------------------------------------*/

xdrMessages_MessageQueue * xdrTargetAgent_InQueue; /* Incoming messages queue */
                                               
xdrMessages_MessageQueue * xdrTargetAgent_OutQueue;/* Outcoming messages queue*/

/*-------------------------------------------------------*/
int xdrTargetAgent_InQueueHandlerTaskID    = 0;
int xdrTargetAgent_HooksQueueHandlerTaskID = 0;

#define InQueueHandlerTaskName    "xdrTargetAgent_InQueueHandlerTask"
#define HooksQueueHandlerTaskName "xdrTargetAgent_HooksQueueHandlerTask"




/*----------------------------------------------------------------------------*/
#define xdrTargetAgent_AnyTrace		0x8000
#define xdrTargetAgent_FlowTrace	0x4000
#define xdrTargetAgent_TraceMask	0xc000





/*----------------------------------------------------------------------------*/
#define xdrTargetAgent_ExamineModule_Action_Calc        0
/* Calculate number of loaded modules:
     initial value of number is zero
*/

#define xdrTargetAgent_ExamineModule_Action_BuildList   1
/* Build list of loaded modules:
     initial value of number is zero
*/

typedef struct {
  int          action;       /* Action to perform                      */
  STATUS       status;       /* Returned status                        */
  int          number;       /* Number of modules                      */
  int          mod_num;      /* Number of elements of 'mod_ids' array  */
  MODULE_ID  * mod_ids;      /* Descriptors of modules loaded modules  */
} xdrTargetAgent_ExamineModule_Args;

BOOL xdrTargetAgent_ExamineModule(MODULE_ID mod_id, xdrTargetAgent_ExamineModule_Args * args){
  int i;

  switch(args->action){
    case xdrTargetAgent_ExamineModule_Action_Calc:
      args->number++;
      break;
    case xdrTargetAgent_ExamineModule_Action_BuildList:
      if(args->number < args->mod_num) args->mod_ids[args->number++] = mod_id;
      break;
    default:
      args->status = ERROR;
      return FALSE;
  };
  args->status = OK;
  return TRUE;
};


/*----------------------------------------------------------------------------*/
#define xdrTargetAgent_ExamineSymbol_Action_Calc                         0
/* Calculate number of symbols of module specified by 'group' 
and put it to 'number' (initial value of 'number' is zero)*/

#define xdrTargetAgent_ExamineSymbol_Action_PutData                      1
/* Put data about symbols of module specified by 'group' 
to 'exp' array (initial value of 'number' is number of 
'exp' elements, initial value of 'i' is zero):
  exp[?].obj   - undefined;
  exp[?].offet - symbol address;
  exp[?].name  - symbol name;
*/

#define xdrTargetAgent_ExamineSymbol_Action_FindOutMinMax                2
/* Find out maximal and minimal address for eacj=h section of 
module specified by 'group' and put it to corresponding
'*_min' and '*_max' arguments (initial value of '*_min' is 0,  
'*_max' is memory top). '*_present' arguments must be set. */


typedef struct {
  int                       action; /* Action to perform        */
  STATUS                    status; /* Returned status          */
  int                       number; /* Number of symbols        */
  int                       i;      /* General purpose variable */
  xdrKernelTypes_Exported * exp;    /* Symbol data array        */
  UINT16                    group;  /* Module group             */
  BOOL                      text_present;
  BOOL                      data_present;
  BOOL                      bss_present;
  DWORD                     text_min;
  DWORD                     text_max;
  DWORD                     data_min;
  DWORD                     data_max;
  DWORD                     bss_min;
  DWORD                     bss_max;
} xdrTargetAgent_ExamineSymbol_Args;

BOOL xdrTargetAgent_ExamineSymbol(char * name, int val, SYM_TYPE type,
                            xdrTargetAgent_ExamineSymbol_Args * args, UINT16 group)
{
  switch(args->action){
    case xdrTargetAgent_ExamineSymbol_Action_Calc:
      if(args->group == group) args->number++;
      break;
    case xdrTargetAgent_ExamineSymbol_Action_PutData:
      if(args->group == group){
        if(args->i >= args->number){
          args->status = ERROR;
          return FALSE;
        };
        args->exp[args->i].obj    = (DWORD)type;
        args->exp[args->i].offset = (DWORD)val;
        strncpy(args->exp[args->i].name, name, sizeof(xdrKernelTypes_EntryName)-1);
        args->exp[args->i].name[sizeof(xdrKernelTypes_EntryName)-1] = 0;
        args->i++;
      };
      break;
    case xdrTargetAgent_ExamineSymbol_Action_FindOutMinMax:
      if(args->group == group){
        switch(type){
          case 5: /* Text symbol */
            args->text_present = TRUE;
            if(args->text_min > (DWORD)val) args->text_min = (DWORD)val;
            if(args->text_max < (DWORD)val) args->text_max = (DWORD)val;
            break;
          case 7: /* Data symbol */
            args->data_present = TRUE;
            if(args->data_min > (DWORD)val) args->data_min = (DWORD)val;
            if(args->data_max < (DWORD)val) args->data_max = (DWORD)val;
            break;
          case 9: /* Bss symbol */
            args->bss_present = TRUE;
            if(args->bss_min > (DWORD)val) args->bss_min = (DWORD)val;
            if(args->bss_max < (DWORD)val) args->bss_max = (DWORD)val;
            break;
          default:
            break;
        };
      };
      break;
    default:
      args->status = ERROR;
      return FALSE;
  };
  args->status = OK;
  return TRUE;
};






/*----------------------------------------------------------------------------*/
void xdrTargetAgent_LoadModule(xdrMessages_Message * msg){
  MODULE_ID mod_id;
  int       result;

  mod_id = ld(1, TRUE, msg->Body.LoadModule.moduleName);
  if(mod_id != NULL){
    *(MODULE_ID*)(msg->Body.LoadModule.pmoduleID) = mod_id;
    result = xdrTypes_errOK;
  }else{
    result = xdrTypes_errUnknown;
  };
  xdrMessages_SetResult(msg, result);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};

/*-------------------------------------------------------*/
void xdrTargetAgent_GetModuleInfo(xdrMessages_Message * msg){
  MODULE_ID   mod_id;
  MODULE_INFO mod_info;
  DWORD       obj = 0;
  xdrTargetAgent_ExamineSymbol_Args ExamineSymbol_Args;


  mod_id = (MODULE_ID)msg->Body.GetModuleInfo.moduleID;

  strcpy(msg->Body.GetModuleInfo.pmoduleInfo->short_name, mod_id->name);
  strcpy(msg->Body.GetModuleInfo.pmoduleInfo->full_name, mod_id->name);
  msg->Body.GetModuleInfo.pmoduleInfo->app_type = xdrKernelTypes_apptypeNone;
  msg->Body.GetModuleInfo.pmoduleInfo->Handle = 0;
  msg->Body.GetModuleInfo.pmoduleInfo->MainEntry = 0;
  memset(&(msg->Body.GetModuleInfo.pmoduleInfo->DebugInfoTag), 0, sizeof(xdrKernelTypes_typeDebugInfoTag));
  msg->Body.GetModuleInfo.pmoduleInfo->DebugInfoStart = 0;
  msg->Body.GetModuleInfo.pmoduleInfo->DebugInfoSize  = 0;

  if(moduleInfoGet(mod_id, &mod_info) == ERROR){
    xdrMessages_SetResult(msg, xdrTypes_errUnknown);
    xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
    return;
  };

  msg->Body.GetModuleInfo.pmoduleInfo->N_Objects = 0;
  if(mod_info.segInfo.textSize > 0){
    msg->Body.GetModuleInfo.pmoduleInfo->N_Objects++;
    msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Begin = (DWORD)mod_info.segInfo.textAddr;
    msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].End   = msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Begin + mod_info.segInfo.textSize;
    msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Attributes = xdrKernelTypes_attrRead | xdrKernelTypes_attrWrite | xdrKernelTypes_attrExecute;
    msg->Body.GetModuleInfo.pmoduleInfo->CodeObject = obj;
    obj++;
  };
  if(mod_info.segInfo.dataSize > 0){
    msg->Body.GetModuleInfo.pmoduleInfo->N_Objects++;
    msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Begin = (DWORD)mod_info.segInfo.dataAddr;
    msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].End   = msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Begin + mod_info.segInfo.dataSize;
    msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Attributes = xdrKernelTypes_attrRead | xdrKernelTypes_attrWrite;
    obj++;
  };
  if(mod_info.segInfo.bssSize > 0){
    msg->Body.GetModuleInfo.pmoduleInfo->N_Objects++;
    msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Begin = (DWORD)mod_info.segInfo.bssAddr;
    msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].End   = msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Begin + mod_info.segInfo.bssSize;
    msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Attributes = xdrKernelTypes_attrRead | xdrKernelTypes_attrWrite;
    obj++;
  };

  if(msg->Body.GetModuleInfo.pmoduleInfo->N_Objects == 0 && msg->Body.GetModuleInfo.force){
    ExamineSymbol_Args.action = xdrTargetAgent_ExamineSymbol_Action_FindOutMinMax;
    ExamineSymbol_Args.group  = mod_id->group;
    ExamineSymbol_Args.text_min = 
    ExamineSymbol_Args.data_min = 
    ExamineSymbol_Args.bss_min  = (DWORD)sysMemTop();
    ExamineSymbol_Args.text_max = 
    ExamineSymbol_Args.data_max = 
    ExamineSymbol_Args.bss_max  = 0;
    if(symEach(sysSymTbl, (FUNCPTR)xdrTargetAgent_ExamineSymbol, (int)&ExamineSymbol_Args) != NULL){
      xdrMessages_SetResult(msg, xdrTypes_errUnknown);
      xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
      return;
    };
    if(ExamineSymbol_Args.text_present){
      msg->Body.GetModuleInfo.pmoduleInfo->N_Objects++;
      msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Begin = ExamineSymbol_Args.text_min;
      msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].End   = ExamineSymbol_Args.text_max;
      msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Attributes = xdrKernelTypes_attrRead | xdrKernelTypes_attrWrite | xdrKernelTypes_attrExecute;
      msg->Body.GetModuleInfo.pmoduleInfo->CodeObject = obj;
      obj++;
    };
    if(ExamineSymbol_Args.data_present){
      msg->Body.GetModuleInfo.pmoduleInfo->N_Objects++;
      msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Begin = ExamineSymbol_Args.data_min;
      msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].End   = ExamineSymbol_Args.data_max;
      msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Attributes = xdrKernelTypes_attrRead | xdrKernelTypes_attrWrite;
      obj++;
    };
    if(ExamineSymbol_Args.bss_present){
      msg->Body.GetModuleInfo.pmoduleInfo->N_Objects++;
      msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Begin = ExamineSymbol_Args.bss_min;
      msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].End   = ExamineSymbol_Args.bss_max;
      msg->Body.GetModuleInfo.pmoduleInfo->Objects[obj].Attributes = xdrKernelTypes_attrRead | xdrKernelTypes_attrWrite;
      obj++;
    };
  };

  xdrMessages_SetResult(msg, xdrTypes_errOK);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};

/*-------------------------------------------------------*/
void xdrTargetAgent_FindSymbol(xdrMessages_Message * msg){
  SYM_TYPE sym_type;
  int result;

  result = (symFindByName(sysSymTbl, msg->Body.FindSymbol.symbolName, (char**)msg->Body.FindSymbol.paddr, &sym_type) == ERROR) ? xdrTypes_errUnknown : xdrTypes_errOK;
  xdrMessages_SetResult(msg, result);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};

/*-------------------------------------------------------*/
void xdrTargetAgent_GetLoadedModulesList(xdrMessages_Message * msg){
  xdrTargetAgent_ExamineModule_Args ExamineModule_Args;

  xdrMessages_SetResult(msg, xdrTypes_errOK);

  ExamineModule_Args.number = 0;
  ExamineModule_Args.action = xdrTargetAgent_ExamineModule_Action_Calc;
  if(moduleEach((FUNCPTR)xdrTargetAgent_ExamineModule, (int)&ExamineModule_Args) != NULL){
    xdrMessages_SetResult(msg, xdrTypes_errUnknown);
    xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
    return;
  };

  if(ExamineModule_Args.number > 0){
    *(msg->Body.GetLoadedModulesList.pnum) = ExamineModule_Args.mod_num = ExamineModule_Args.number;
    *(msg->Body.GetLoadedModulesList.pmoduleIDs) = (int*)(ExamineModule_Args.mod_ids = (MODULE_ID*)malloc(sizeof(MODULE_ID) * ExamineModule_Args.mod_num));
    ExamineModule_Args.number = 0;
    ExamineModule_Args.action = xdrTargetAgent_ExamineModule_Action_BuildList;
    if(moduleEach((FUNCPTR)xdrTargetAgent_ExamineModule, (int)&ExamineModule_Args) != NULL){
      xdrMessages_SetResult(msg, xdrTypes_errUnknown);
    };
  };
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};

/*-------------------------------------------------------*/
void xdrTargetAgent_CreateTask(xdrMessages_Message * msg){
  char     * taskStack;
  WIND_TCB * pTcb;
  int        result;

  taskStack = (char*) xdrMM_Alloc(xdrTypes_TaskStackSize);
  pTcb      = xdrMM_AllocRec(WIND_TCB);

  if(taskInit(pTcb, msg->Body.CreateTask.taskName, xdrTypes_TaskWorkPriority, 0, 
              taskStack+xdrTypes_TaskStackSize, xdrTypes_TaskStackSize, (FUNCPTR)msg->Body.CreateTask.entryPoint, 
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0) == ERROR){
    xdrMM_Free(taskStack);
    xdrMM_Free(pTcb);
    result = xdrTypes_errCreateTask;
  }else{
    *(msg->Body.CreateTask.ptaskID) = (int)pTcb;
    result = xdrTypes_errOK;
  };
  xdrMessages_SetResult(msg, result);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};

/*-------------------------------------------------------*/
void xdrTargetAgent_GetRegisters(xdrMessages_Message * msg){
  int result;

  result = (taskRegsGet(msg->Body.GetRegisters.taskID, msg->Body.GetRegisters.pregSet) == ERROR) ? xdrTypes_errGetRegisters : xdrTypes_errOK;
  xdrMessages_SetResult(msg, result);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};

/*-------------------------------------------------------*/
void xdrTargetAgent_PutRegisters(xdrMessages_Message * msg){
  int result;

  result = (taskRegsSet(msg->Body.PutRegisters.taskID, msg->Body.GetRegisters.pregSet) == ERROR) ? xdrTypes_errSetRegisters : xdrTypes_errOK;
  xdrMessages_SetResult(msg, result);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};

/*-------------------------------------------------------*/
void xdrTargetAgent_GetModuleID(xdrMessages_Message * msg){
  MODULE_ID mod_id;
  int result;

  mod_id = moduleFindByName(msg->Body.GetModuleID.moduleName);
  *(msg->Body.GetModuleID.pmoduleID) = (int)mod_id;
  result = (mod_id == NULL) ? xdrTypes_errUnknown : xdrTypes_errOK;
  xdrMessages_SetResult(msg, result);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};

/*-------------------------------------------------------*/
void xdrTargetAgent_GetModuleSymbols(xdrMessages_Message * msg){
  xdrTargetAgent_ExamineSymbol_Args ExamineSymbol_Args;
  int result;

  result = xdrTypes_errOK;

  ExamineSymbol_Args.action = xdrTargetAgent_ExamineSymbol_Action_Calc;
  ExamineSymbol_Args.group  = ((MODULE_ID)msg->Body.GetModuleSymbols.moduleID)->group;
  ExamineSymbol_Args.number = 0;
  if(symEach(sysSymTbl, (FUNCPTR)xdrTargetAgent_ExamineSymbol, (int)&ExamineSymbol_Args) != NULL){
    result = xdrTypes_errUnknown;
  }else{
    *(msg->Body.GetModuleSymbols.pexpLen) = ExamineSymbol_Args.number;
    ExamineSymbol_Args.action = xdrTargetAgent_ExamineSymbol_Action_PutData;
    ExamineSymbol_Args.i = 0;
    *(msg->Body.GetModuleSymbols.pExport) = ExamineSymbol_Args.exp = (xdrKernelTypes_Exported*) xdrMM_Alloc(ExamineSymbol_Args.number * sizeof(xdrKernelTypes_Exported));
    if(symEach(sysSymTbl, (FUNCPTR)xdrTargetAgent_ExamineSymbol, (int)&ExamineSymbol_Args) != NULL){
      result = xdrTypes_errUnknown;
    };
  };
  xdrMessages_SetResult(msg, result);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};

/*-------------------------------------------------------*/
void xdrTargetAgent_GetMemory(xdrMessages_Message * msg){
  int result;
  void * buff;

  if(msg->Body.GetMemory.len > 0 && msg->Body.GetMemory.addr >= 0 && msg->Body.GetMemory.addr < (DWORD)sysMemTop() && (msg->Body.GetMemory.addr+msg->Body.GetMemory.len) <= (DWORD)sysMemTop()){
    *(msg->Body.GetMemory.pbuff) = buff = xdrMM_Alloc(msg->Body.GetMemory.len);
    memcpy(buff, (void*)msg->Body.GetMemory.addr, msg->Body.GetMemory.len);
    result = xdrTypes_errOK;
  }else{
    result = xdrTypes_errUnknown;
  };

  xdrMessages_SetResult(msg, result);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};

/*-------------------------------------------------------*/
void xdrTargetAgent_PutMemory(xdrMessages_Message * msg){
  int result;

  if(msg->Body.PutMemory.len > 0 && msg->Body.PutMemory.addr >= 0 && msg->Body.PutMemory.addr < (DWORD)sysMemTop() && (msg->Body.PutMemory.addr+msg->Body.PutMemory.len) <= (DWORD)sysMemTop()){
    memcpy((void*)msg->Body.PutMemory.addr, msg->Body.PutMemory.buff, msg->Body.PutMemory.len);
    result = xdrTypes_errOK;
  }else{
    result = xdrTypes_errUnknown;
  };

  xdrMessages_SetResult(msg, result);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};

/*-------------------------------------------------------*/
void xdrTargetAgent_GetSegmentInfo(xdrMessages_Message * msg){
  int result;



  if(msg->Body.GetSegmentInfo.addr >= 0 && msg->Body.GetSegmentInfo.addr < (DWORD)sysMemTop()){
   *(msg->Body.GetSegmentInfo.pbegin) = 0;
   *(msg->Body.GetSegmentInfo.plen)   = (DWORD)sysMemTop();
   *(msg->Body.GetSegmentInfo.pattr)  = xdrKernelTypes_attrRead  | 
                                        xdrKernelTypes_attrWrite |
                                        xdrKernelTypes_attrExecute;
   result = xdrTypes_errOK;
  }else{
   result = xdrTypes_errUnknown;
  };

  xdrMessages_SetResult(msg, xdrTypes_errOK);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};


/*-------------------------------------------------------*/
void xdrTargetAgent_SetTraceMode(xdrMessages_Message * msg){
  REG_SET regSet;
  int result;

  result = xdrTypes_errUnknown;

  if(taskRegsGet(msg->Body.SetTraceMode.taskID, &regSet) != ERROR){
    *(DWORD*)(((BYTE*)&regSet)+SR_OFFSET) &= ~xdrTargetAgent_TraceMask;
    *(DWORD*)(((BYTE*)&regSet)+SR_OFFSET) |= xdrTargetAgent_AnyTrace;
    if(taskRegsSet(msg->Body.SetTraceMode.taskID, &regSet) != ERROR) result = xdrTypes_errOK;
  };

  xdrMessages_SetResult(msg, result);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};


/*-------------------------------------------------------*/
void xdrTargetAgent_ResetTraceMode(xdrMessages_Message * msg){
  REG_SET regSet;
  int result;

  result = xdrTypes_errUnknown;

  if(taskRegsGet(msg->Body.ResetTraceMode.taskID, &regSet) != ERROR){
    *(DWORD*)(((BYTE*)&regSet)+SR_OFFSET) &= ~xdrTargetAgent_TraceMask;
    if(taskRegsSet(msg->Body.ResetTraceMode.taskID, &regSet) != ERROR) result = xdrTypes_errOK;
  };

  xdrMessages_SetResult(msg, result);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};

/*-------------------------------------------------------*/
void xdrTargetAgent_ResumeTask(xdrMessages_Message * msg){
  int result;

  if(taskResume(msg->Body.ResumeTask.taskID) == ERROR){
    result = xdrTypes_errUnknown;
  }else{
    result = xdrTypes_errOK;
  };

  xdrMessages_SetResult(msg, result);
  xdrMessages_PutMessage(xdrTargetAgent_OutQueue, msg);
};


/*-------------------------------------------------------*/
/*-------------------------------------------------------*/
/*-------------------------------------------------------*/
/*-------------------------------------------------------*/
void xdrTargetAgent_InQueueHandler(void){
  xdrMessages_Message msg;

  for(;;){
    /* Get message */
    xdrMessages_GetMessage(xdrTargetAgent_InQueue, &msg);

    /* Process message */
    switch(msg.Header.mode){
      case msgMode_LoadModule:
           xdrTargetAgent_LoadModule(&msg);
           break;
      case msgMode_GetModuleInfo:
           xdrTargetAgent_GetModuleInfo(&msg);
           break;
      case msgMode_FindSymbol:
           xdrTargetAgent_FindSymbol(&msg);
           break;
      case msgMode_GetLoadedModulesList:
           xdrTargetAgent_GetLoadedModulesList(&msg);
           break;
      case msgMode_CreateTask:
           xdrTargetAgent_CreateTask(&msg);
           break;
      case msgMode_GetRegisters:
           xdrTargetAgent_GetRegisters(&msg);
           break;
      case msgMode_PutRegisters:
           xdrTargetAgent_PutRegisters(&msg);
           break;
      case msgMode_GetModuleID:
           xdrTargetAgent_GetModuleID(&msg);
           break;
      case msgMode_GetModuleSymbols:
           xdrTargetAgent_GetModuleSymbols(&msg);
           break;
      case msgMode_GetMemory:
           xdrTargetAgent_GetMemory(&msg);
           break;
      case msgMode_PutMemory:
           xdrTargetAgent_PutMemory(&msg);
           break;
      case msgMode_GetSegmentInfo:
           xdrTargetAgent_GetSegmentInfo(&msg);
           break;
      case msgMode_SetTraceMode:
           xdrTargetAgent_SetTraceMode(&msg);
           break;
      case msgMode_ResetTraceMode:
           xdrTargetAgent_ResetTraceMode(&msg);
           break;
      case msgMode_ResumeTask:
           xdrTargetAgent_ResumeTask(&msg);
           break;

/*
      case :
           break;
*/
      default:
           printf("TA: Unknown message mode %d\n", msg.Header.mode);
           break;
    };
  };
};


/*-------------------------------------------------------*/
void xdrTargetAgent_InQueueHandlerTask(void){

  /* Wait for start semaphore */
  if(semTake(xdrInit_StartSemaphore, WAIT_FOREVER) != OK){
    perror("semTake");
    for(;;);
  };

  xdrTargetAgent_InQueueHandler();
};



/*----------------------------------------------------------------------------*/
void xdrTargetAgent_HooksQueueHandler(void){
  xdrMessages_Message msg;

  for(;;){
    /* Get message */
    xdrMessages_GetMessage(xdrTAHooks_OutQueue, &msg);

    /* Process message */
    switch(msg.Header.mode){
      case msgMode_EventSingleStep:
           break;
      case msgMode_EventBreakpointHit:
           break;
      case msgMode_EventDeleteTask:
           break;
      default:
           printf("Target Agent (xdrTargetAgent_HooksQueue): unknown message mode %d\n", msg.Header.mode);
           continue;
    };
    xdrMessages_PutMessage(xdrTargetAgent_OutQueue, &msg);
  };
};


/*-------------------------------------------------------*/
void xdrTargetAgent_HooksQueueHandlerTask(void){

  /* Wait for start semaphore */
  if(semTake(xdrInit_StartSemaphore, WAIT_FOREVER) != OK){
    perror("semTake");
    for(;;);
  };

  xdrTargetAgent_HooksQueueHandler();
};



/*----------------------------------------------------------------------------*/
/* Target agent initialization procedure.
   Returns True if initialization was succesfully completed,
   and False otherwise.
*/
int xdrTargetAgent_Init(void){

  /* Create queues */
  if((xdrTargetAgent_InQueue  = xdrMessages_CreateMessageQueue(WAIT_FOREVER)) == NULL) return False;
  if((xdrTargetAgent_OutQueue = xdrMessages_CreateMessageQueue(WAIT_FOREVER)) == NULL) return False;

  /* Spawn input queue handler task */
  if((xdrTargetAgent_InQueueHandlerTaskID = taskSpawn(InQueueHandlerTaskName, 
                 xdrTypes_TaskWorkPriority, 0, xdrTypes_TaskStackSize,
                 (FUNCPTR)xdrTargetAgent_InQueueHandlerTask, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == ERROR){
    perror("taskSpawn");
    return False;
  };
  xdrInit_StartedTasksNumber++;

  /* Set hooks */
  if(!xdrTAHooks_Init()) return False;

  /* Spawn hooks output queue handler task */
  if((xdrTargetAgent_HooksQueueHandlerTaskID = taskSpawn(HooksQueueHandlerTaskName, 
                 xdrTypes_TaskWorkPriority, 0, xdrTypes_TaskStackSize,
                 (FUNCPTR)xdrTargetAgent_HooksQueueHandlerTask, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == ERROR){
    perror("taskSpawn");
    return False;
  };
  xdrInit_StartedTasksNumber++;

  return True;
};
