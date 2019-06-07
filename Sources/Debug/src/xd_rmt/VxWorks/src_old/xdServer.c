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
#include <msgQLib.h>

#include "xdTypes.h"
#include "xdAssert.h"
#include "xdTransact.h"
#include "xdServer.h"



#define TASK_WORK_PRIORITY   100
#define TASK_STACK_SIZE      0x4000

#define ANY_TRACE   0x8000
#define FLOW_TRACE  0x4000
#define TRACE_MASK  0xc000

#define DebuggedTaskNamePttr "tDebuggedTask_%d"

/* -----------------------------------------------------*/
xdServer_ServerDesc xdServer_Servers[MAX_SERVERS];


/* -----------------------------------------------------*/

#define xdServer_ExamineSymbol_Action_Calc                         0
/* Calculate number of symbols of module specified by 'group' 
and put it to 'number' (initial value of 'number' is zero)*/

#define xdServer_ExamineSymbol_Action_PutData                      1
/* Put data about symbols of module specified by 'group' 
to 'exp' array (initial value of 'number' is number of 
'exp' elements, initial value of 'i' is zero):
  exp[?].obj   - undefined;
  exp[?].offet - symbol address;
  exp[?].name  - symbol name;
*/

#define xdServer_ExamineSymbol_Action_FindOutMinMax                2
/* Find out maximal and minimal address for eacj=h section of 
module specified by 'group' and put it to corresponding
'*_min' and '*_max' arguments (initial value of '*_min' is 0,  
'*_max' is memory top). '*_present' arguments must be set. */


typedef struct {
  int        action; /* Action to perform        */
  STATUS     status; /* Returned status          */
  int        number; /* Number of symbols        */
  int        i;      /* General purpose variable */
  EXPORTED * exp;    /* Symbol data array        */
  UINT16     group;  /* Module group             */
  BOOL       text_present;
  BOOL       data_present;
  BOOL       bss_present;
  DWORD      text_min;
  DWORD      text_max;
  DWORD      data_min;
  DWORD      data_max;
  DWORD      bss_min;
  DWORD      bss_max;
} xdServer_ExamineSymbol_Args;

BOOL xdServer_ExamineSymbol(char * name, int val, SYM_TYPE type, 
                            xdServer_ExamineSymbol_Args * args, UINT16 group)
{
  switch(args->action){
    case xdServer_ExamineSymbol_Action_Calc:
      if(args->group == group) args->number++;
      break;
    case xdServer_ExamineSymbol_Action_PutData:
      if(args->group == group){
        if(args->i >= args->number){
          args->status = ERROR;
          return FALSE;
        };
        args->exp[args->i].obj    = (DWORD)type;
        args->exp[args->i].offset = (DWORD)val;
        strncpy(args->exp[args->i].name, name, sizeof(ENTRY_NAME)-1);
        args->exp[args->i].name[sizeof(ENTRY_NAME)-1] = 0;
        args->i++;
      };
      break;
    case xdServer_ExamineSymbol_Action_FindOutMinMax:
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

/* -----------------------------------------------------*/
STATUS xdServer_GetExecInfo(MODULE_ID mod_id, EXEC_INFO * exec_info, BOOL force){
  MODULE_INFO mod_info;
/*  SEGMENT_ID seg_id; */
  DWORD obj = 0;
  xdServer_ExamineSymbol_Args ExamineSymbol_Args;

  strcpy(exec_info->short_name, mod_id->name);
  strcpy(exec_info->full_name, mod_id->name);
  exec_info->app_type = apptypeNone;
  exec_info->Handle = 0;
  exec_info->MainEntry = 0;
  memset(&(exec_info->DebugInfoTag), 0, sizeof(typeDebugInfoTag));
  exec_info->DebugInfoStart = 0;
  exec_info->DebugInfoSize  = 0;

  if(moduleInfoGet(mod_id, &mod_info) == ERROR) return ERROR;

/*
  seg_id = moduleSegFirst(mod_id);
  while(seg_id){
    printf("Addr %x\n", seg_id->address);
    printf("Size %d\n", seg_id->size);
    printf("Type %d\n", seg_id->type);
    seg_id = moduleSegNext(seg_id);
  };
*/

  exec_info->N_Objects = 0;
  if(mod_info.segInfo.textSize > 0){
    exec_info->N_Objects++;
    exec_info->Objects[obj].Begin = (DWORD)mod_info.segInfo.textAddr;
    exec_info->Objects[obj].End   = exec_info->Objects[obj].Begin + mod_info.segInfo.textSize;
    exec_info->Objects[obj].Attributes = attrRead | attrWrite | attrExecute;
    exec_info->CodeObject = obj;
    obj++;
  };
  if(mod_info.segInfo.dataSize > 0){
    exec_info->N_Objects++;
    exec_info->Objects[obj].Begin = (DWORD)mod_info.segInfo.dataAddr;
    exec_info->Objects[obj].End   = exec_info->Objects[obj].Begin + mod_info.segInfo.dataSize;
    exec_info->Objects[obj].Attributes = attrRead | attrWrite;
    obj++;
  };
  if(mod_info.segInfo.bssSize > 0){
    exec_info->N_Objects++;
    exec_info->Objects[obj].Begin = (DWORD)mod_info.segInfo.bssAddr;
    exec_info->Objects[obj].End   = exec_info->Objects[obj].Begin + mod_info.segInfo.bssSize;
    exec_info->Objects[obj].Attributes = attrRead | attrWrite;
    obj++;
  };

  if(exec_info->N_Objects == 0 && force){
    ExamineSymbol_Args.action = xdServer_ExamineSymbol_Action_FindOutMinMax;
    ExamineSymbol_Args.group  = mod_id->group;
    ExamineSymbol_Args.text_min = 
    ExamineSymbol_Args.data_min = 
    ExamineSymbol_Args.bss_min  = (DWORD)sysMemTop();
    ExamineSymbol_Args.text_max = 
    ExamineSymbol_Args.data_max = 
    ExamineSymbol_Args.bss_max  = 0;
    if(symEach(sysSymTbl, (FUNCPTR)xdServer_ExamineSymbol, (int)&ExamineSymbol_Args) != NULL) return ERROR;
    if(ExamineSymbol_Args.text_present){
      exec_info->N_Objects++;
      exec_info->Objects[obj].Begin = ExamineSymbol_Args.text_min;
      exec_info->Objects[obj].End   = ExamineSymbol_Args.text_max;
      exec_info->Objects[obj].Attributes = attrRead | attrWrite | attrExecute;
      exec_info->CodeObject = obj;
      obj++;
    };
    if(ExamineSymbol_Args.data_present){
      exec_info->N_Objects++;
      exec_info->Objects[obj].Begin = ExamineSymbol_Args.data_min;
      exec_info->Objects[obj].End   = ExamineSymbol_Args.data_max;
      exec_info->Objects[obj].Attributes = attrRead | attrWrite;
      obj++;
    };
    if(ExamineSymbol_Args.bss_present){
      exec_info->N_Objects++;
      exec_info->Objects[obj].Begin = ExamineSymbol_Args.bss_min;
      exec_info->Objects[obj].End   = ExamineSymbol_Args.bss_max;
      exec_info->Objects[obj].Attributes = attrRead | attrWrite;
      obj++;
    };
  };

  return OK;
};


/* -----------------------------------------------------*/
#define xdServer_ExamineModule_Action_CreateComp   0
/* Put to 'events_queue' 'CompCreated' event for 
component specified by 'mod_id' function argument
*/

typedef struct {
  int          action;       /* Action to perform                           */
  STATUS       status;       /* Returned status                             */
  MSG_Q_ID     events_queue; /* Events queue                                */
  int          mod_num;      /* Number of elements of 'mod_ids' array       */
  MODULE_ID  * mod_ids;      /* Descriptors of modules that must be skipped */
} xdServer_ExamineModule_Args;

BOOL xdServer_ExamineModule(MODULE_ID mod_id, xdServer_ExamineModule_Args * args){
  EVENT event;
  int i;

  switch(args->action){
    case xdServer_ExamineModule_Action_CreateComp:
      for(i = 0; i < args->mod_num; i++){
        if(mod_id == args->mod_ids[i]) break;
      };
      if(i < args->mod_num) break;
      if(xdServer_GetExecInfo(mod_id, &event.CompCreated.Component, TRUE) == ERROR){
        args->status = ERROR;
        return FALSE;
      };
      event.Common.Event = eventType_CompCreated;
      event.CompCreated.Stopable = 1;
      if(msgQSend(args->events_queue, (char*)&event, sizeof(EVENT), NO_WAIT, MSG_PRI_NORMAL) == ERROR){
        args->status = ERROR;
        return FALSE;
      };
      printf("CompCreated: name = %s\n", event.CompCreated.Component.short_name);
      break;
    default:
      args->status = ERROR;
      return FALSE;
  };
  args->status = OK;
  return TRUE;
};

/* -----------------------------------------------------*/
void xdServer_CleanEventsQueue(MSG_Q_ID q){
  EVENT e;
  while(msgQReceive(q, (char*)&e, sizeof(EVENT), NO_WAIT) != ERROR);
};

/* -----------------------------------------------------*/
STATUS xdServer_Register(int debuggedTaskId, int debuggerTaskId){
  int i;
  for(i = 0; i < MAX_SERVERS; i++){
    if(xdServer_Servers[i].free){
      xdServer_Servers[i].free           = FALSE;
      xdServer_Servers[i].debuggedTaskId = debuggedTaskId;
      xdServer_Servers[i].debuggerTaskId = debuggerTaskId;
      xdServer_Servers[i].semaphore      = NULL;
      xdServer_Servers[i].event          = xdServer_Event_None;
      return i;
    };
  };
  return ERROR;
};

/* -----------------------------------------------------*/
STATUS xdServer_SetTraceMode(int taskId){
  REG_SET reg_set;

  if(taskRegsGet((int)taskId, &reg_set) != ERROR){
    *(DWORD*)(((BYTE*)&reg_set)+SR_OFFSET) &= ~TRACE_MASK;
    *(DWORD*)(((BYTE*)&reg_set)+SR_OFFSET) |= ANY_TRACE;
    if(taskRegsSet((int)taskId, &reg_set) != ERROR) return OK;
  };
  return ERROR;
};

/* -----------------------------------------------------*/
STATUS xdServer_ResetTraceMode(int taskId){
  REG_SET reg_set;

  if(taskRegsGet((int)taskId, &reg_set) != ERROR){
    *(DWORD*)(((BYTE*)&reg_set)+SR_OFFSET) &= ~TRACE_MASK;
    if(taskRegsSet((int)taskId, &reg_set) != ERROR) return OK;
  };
  return ERROR;
};

/* -----------------------------------------------------*/
void xdServer_TaskDeleteHook(WIND_TCB * pTcb){
  int i;

  for(i = 0; i < MAX_SERVERS; i++){
    if(!xdServer_Servers[i].free && 
        xdServer_Servers[i].debuggedTaskId == (int)pTcb)
    {
      break;
    };
  };

  if(i == MAX_SERVERS) return;
  xdServer_Servers[i].event = xdServer_Event_TaskDelete;
  semGive(xdServer_Servers[i].semaphore);
  xdServer_Servers[i].semaphore = NULL;
};

/* -----------------------------------------------------*/
void xdServer_IntrStub(int intr){
  int i;

  if(taskSuspend((int)taskIdCurrent) == ERROR) return;

  for(i = 0; i < MAX_SERVERS; i++){
    if(!xdServer_Servers[i].free && 
        xdServer_Servers[i].debuggedTaskId == (int)taskIdCurrent)
    {
      break;
    };
  };

  if(i == MAX_SERVERS) return;
  switch(intr){
    case 0x24: xdServer_Servers[i].event = xdServer_Event_SingleStep;    break;
    case 0x88: xdServer_Servers[i].event = xdServer_Event_BreakpointHit; break;
  };
  semGive(xdServer_Servers[i].semaphore);
  xdServer_Servers[i].semaphore = NULL;
};

/* -----------------------------------------------------*/
/* -----------------------------------------------------*/
STATUS xdServer_GetGoMode(GO_MODE * go_mode, char * buff, DWORD size){

  if(size <= 0) return ERROR;

  go_mode->mode = *(BYTE*)(buff);
  size--; buff++;
  if(size < 0) return ERROR;

  switch(go_mode->mode){
    case modeSingleStep:
      go_mode->SingleStep.add_step = *(BYTE*)buff;
      size--; buff++;
      if(size <= 0) return ERROR;
      break;
    case modeRangeStep:
      go_mode->RangeStep.Begin = DWChEnd(*(DWORD*)buff);
      size -= 4; buff +=4;
      if(size <= 0) return ERROR;
      go_mode->RangeStep.End = DWChEnd(*(DWORD*)buff);
      size -= 4; buff +=4;
      if(size <= 0) return ERROR;
      break;
    case modeGo:
      break;
    default:
      return ERROR;
  };
  return OK;
};

/* -----------------------------------------------------*/
DWORD xdServer_DoSingleStep(int taskId, BOOL * prog_activated){

  if(xdServer_SetTraceMode(taskId) == ERROR) return 1;
  if(*prog_activated){
    if(taskResume(taskId) == ERROR) return 1;
  }else{
    if(taskActivate(taskId) == ERROR) return 1;
    *prog_activated = TRUE;
  };
  return 0;
};

/* -----------------------------------------------------*/
DWORD xdServer_DoGo(int taskId, BOOL * prog_activated){

  if(*prog_activated){
    if(taskResume(taskId) == ERROR) return 1;
  }else{
    if(taskActivate(taskId) == ERROR) return 1;
    *prog_activated = TRUE;
  };
  return 0;
};

/* -----------------------------------------------------*/
DWORD xdServer_StartProgram(MSG_Q_ID events_queue, char * name, char * args, 
                            MODULE_ID * mod_id, EXEC_INFO * exec_info,
                            WIND_TCB * pTcb, char * taskName, 
                            char * taskStackTop)
{
  MODULE_INFO mod_info;
/*  SEGMENT_ID seg_id;*/
  DWORD obj = 0;
  SYM_TYPE sym_type;
  FUNCPTR EntryPoint;
  xdServer_ExamineModule_Args ExamineModule_Args;


  /* Load module */
  *mod_id = ld(1, TRUE, name);
  if(*mod_id == NULL) return 1;

  /* Get module info */
  if(xdServer_GetExecInfo(*mod_id, exec_info, FALSE) == ERROR){
    unldByModuleId(*mod_id, UNLD_KEEP_BREAKPOINTS);
    *mod_id = NULL;
    return 1;
  };

  /* Find and set entry point */
  if(symFindByName(sysSymTbl, "_Test_Func", (char**)&EntryPoint, &sym_type) == ERROR){
    unldByModuleId(*mod_id, UNLD_KEEP_BREAKPOINTS);
    *mod_id = NULL;
    return 1;
  };

  exec_info->MainEntry = (DWORD)EntryPoint;


  /* Get loaded modules info */
  ExamineModule_Args.mod_num = 1;
  ExamineModule_Args.mod_ids = (MODULE_ID*)malloc(sizeof(MODULE_ID) * ExamineModule_Args.mod_num);
  ExamineModule_Args.mod_ids[0] = *mod_id;

  ExamineModule_Args.action = xdServer_ExamineModule_Action_CreateComp;
  ExamineModule_Args.events_queue = events_queue;
  if(moduleEach((FUNCPTR)xdServer_ExamineModule, (int)&ExamineModule_Args) != NULL){
    unldByModuleId(*mod_id, UNLD_KEEP_BREAKPOINTS);
    *mod_id = NULL;
    return 1;
  };


  /* Create task */
  if(taskInit(pTcb, taskName, TASK_WORK_PRIORITY, 0, 
              taskStackTop+TASK_STACK_SIZE, TASK_STACK_SIZE, EntryPoint, 
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0) == ERROR)
  {
    unldByModuleId(*mod_id, UNLD_KEEP_BREAKPOINTS);
    *mod_id = NULL;
    return 1;
  };


  return 0;
};

/* -----------------------------------------------------*/
void xdServer_Server(int pipeID, char * transport, DWORD instance){
  COMMAND       command;
  DWORD         arg_size, len, addr;
  BYTE          arg_buf[1024];
  PROGRAM_NAME  prog_name;
  char          prog_args[512];
  EXEC_INFO     exec_info, tmp_exec_info;
  GO_MODE       go_mode;
  EVENT         event;
  DWORD         result;
  BOOL          prog_started = FALSE, prog_activated = FALSE, continue_flag = TRUE;
  BOOL          add_event, stopping_event;
  MODULE_ID     mod_id = NULL, tmp_mod_id;
  WIND_TCB      debuggedTcb;
  char          taskName[32];
  char        * taskStackTop = NULL;
  REG_SET       RegSet;
  int           serverNum;
  SEM_ID        semaphore = NULL;
  MSG_Q_ID      events_queue = NULL;
  xdServer_ExamineSymbol_Args ExamineSymbol_Args;
  int           i;

  if((serverNum = xdServer_Register((int)&debuggedTcb, taskIdSelf())) == ERROR){
    printf("Can't register server.\n");
    return;
  };

  if((semaphore = semBCreate(SEM_Q_FIFO, SEM_EMPTY)) == NULL){
    printf("Can't create semaphore.\n");
    return;
  };

  if((events_queue = msgQCreate(MAX_EVENTS, sizeof(EVENT), MSG_Q_FIFO)) == NULL){
    printf("Can't create events queue.\n");
    return;
  };

  if(ServerConnect(transport) == ERROR) return;

  while(continue_flag){
    printf("\nWait command...");
    command = ReceiveCommand(&arg_size);
    if(arg_size != 0 && Receive(arg_buf, arg_size) == ERROR){
      printf("Command argument wasn't received.\n");
      return;
    };
    printf("\n");
    switch (command) {
      case cmdStartProgram:
        if(!prog_started){
          printf("Start program\n");
          strcpy(prog_name, arg_buf);
          strcpy(prog_args, arg_buf + strlen(arg_buf));
          printf("name: %s\n", prog_name);
          printf("args: %s\n", prog_args);
          sprintf(taskName, DebuggedTaskNamePttr, instance);
          taskStackTop = (char*) malloc(TASK_STACK_SIZE);
          xdServer_CleanEventsQueue(events_queue);
          result = xdServer_StartProgram(
                       events_queue,
                       prog_name, prog_args, 
                       &mod_id, &exec_info, &debuggedTcb,
                       taskName, taskStackTop
                   );
          prog_started = result == 0;
        }else{
          printf("Error: second \"Start program\" command.\n");
          result = 1;
        };
        if(SendEvents(result, events_queue) == ERROR) printf("Send result error\n");
        break;
      case cmdGetExecInfo:
        printf("Get exec information\n");
        if(SendResult(0, EXEC_INFO_SIZE + exec_info.N_Objects*OBJECT_SIZE) == ERROR) printf("Send result error\n");
        if(SendEXEC_INFORec(exec_info) == ERROR) printf("Send result error\n");
        for(len = 0; len < exec_info.N_Objects; len++){
          if(SendOBJECTRec(exec_info.Objects[len]) == ERROR) printf("Send result error\n");
        };
        break;
      case cmdGetDebugInfo:
        printf("Get debug information\n");
        if(SendResult(0, 0) == ERROR) printf("Send result error\n");
        break;
      case cmdQuitDebug:
        printf("Quit debug\n\n");
        if(SendResult(0, 0) == ERROR) printf("Send result error\n");
        continue_flag = FALSE;
        break;
      case cmdGetRegisterCache:
        printf("Get register cache\n");
        if(!prog_started || taskRegsGet((int)&debuggedTcb, &RegSet) != ERROR){
          if(SendResult(0, 18*4) == ERROR) printf("Send result error\n");

          if(SendDW(RegSet.dataReg[0]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.dataReg[1]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.dataReg[2]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.dataReg[3]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.dataReg[4]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.dataReg[5]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.dataReg[6]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.dataReg[7]) == ERROR) printf("Send result error\n");

          if(SendDW(RegSet.addrReg[0]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.addrReg[1]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.addrReg[2]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.addrReg[3]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.addrReg[4]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.addrReg[5]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.addrReg[6]) == ERROR) printf("Send result error\n");
          if(SendDW(RegSet.addrReg[7]) == ERROR) printf("Send result error\n");

          if(SendDW(*(DWORD*)(((BYTE*)&RegSet) + SR_OFFSET)) == ERROR) printf("Send result error\n");

          if(SendDW((DWORD)RegSet.pc) == ERROR) printf("Send result error\n");
        }else{
          if(SendResult(1, 0) == ERROR) printf("Send result error\n");
        };
        break;
      case  cmdPutRegisterCache:
        printf("Put register cache\n");

        RegSet.dataReg[0] = DWChEnd(*(DWORD*)(arg_buf));
        RegSet.dataReg[1] = DWChEnd(*(DWORD*)(arg_buf+4));
        RegSet.dataReg[2] = DWChEnd(*(DWORD*)(arg_buf+8));
        RegSet.dataReg[3] = DWChEnd(*(DWORD*)(arg_buf+12));
        RegSet.dataReg[4] = DWChEnd(*(DWORD*)(arg_buf+16));
        RegSet.dataReg[5] = DWChEnd(*(DWORD*)(arg_buf+20));
        RegSet.dataReg[6] = DWChEnd(*(DWORD*)(arg_buf+24));
        RegSet.dataReg[7] = DWChEnd(*(DWORD*)(arg_buf+28));

        RegSet.addrReg[0] = DWChEnd(*(DWORD*)(arg_buf+32));
        RegSet.addrReg[1] = DWChEnd(*(DWORD*)(arg_buf+36));
        RegSet.addrReg[2] = DWChEnd(*(DWORD*)(arg_buf+40));
        RegSet.addrReg[3] = DWChEnd(*(DWORD*)(arg_buf+44));
        RegSet.addrReg[4] = DWChEnd(*(DWORD*)(arg_buf+48));
        RegSet.addrReg[5] = DWChEnd(*(DWORD*)(arg_buf+52));
        RegSet.addrReg[6] = DWChEnd(*(DWORD*)(arg_buf+56));
        RegSet.addrReg[7] = DWChEnd(*(DWORD*)(arg_buf+60));

        *(DWORD*)(((BYTE*)&RegSet)+SR_OFFSET) = DWChEnd(*(DWORD*)(arg_buf+64));
        RegSet.pc         = (INSTR*)DWChEnd(*(DWORD*)(arg_buf+68));

        if(prog_started && taskRegsSet((int)&debuggedTcb, &RegSet) == ERROR){
          if(SendResult(1, 0) == ERROR) printf("Send result error\n");
        }else{
          if(SendResult(0, 0) == ERROR) printf("Send result error\n");
        };
        break;
      case cmdGetMem:
        printf("Get memory\n");
        addr = DWChEnd(*(DWORD*)(arg_buf));
        len  = DWChEnd(*(DWORD*)(arg_buf+4));
        if(len > 0 && addr >= 0 && addr < (DWORD)sysMemTop() && (addr+len) <= (DWORD)sysMemTop()){
          if(SendResult(0, len) == ERROR) printf("Send result error\n");
          if(Send(addr, len) == ERROR) printf("Send result error\n");
        }else{
          if(SendResult(1, len) == ERROR) printf("Send result error\n");
        };
        break;
      case cmdPutMem:
        printf("Put memory\n");
        addr = DWChEnd(*(DWORD*)(arg_buf));
        len = arg_size - 4;
        if(len > 0 && addr >= 0 && addr < (DWORD)sysMemTop() && (addr+len) <= (DWORD)sysMemTop()){
          memcpy((void*)addr, (void*)(arg_buf+4), len);
          if(SendResult(0, 0) == ERROR) printf("Send result error\n");
        };
        break;
      case cmdExecute:
        printf("Execute program\n");
        if(xdServer_GetGoMode(&go_mode, arg_buf, arg_size) != ERROR){
          stopping_event = FALSE;
          while(!stopping_event){
            xdServer_Servers[serverNum].semaphore = semaphore;
            xdServer_Servers[serverNum].event     = xdServer_Event_None;
            result = 1;
            if(xdServer_ResetTraceMode((int)&debuggedTcb) != ERROR){
              switch(go_mode.mode){
                case modeSingleStep:
                  result = xdServer_DoSingleStep((int)&debuggedTcb, &prog_activated);
                  break;
                case modeRangeStep:
                  break;
                case modeGo:
                  result = xdServer_DoGo((int)&debuggedTcb, &prog_activated);
                  break;
              };
            };
            if(result != 0 || semTake(semaphore, WAIT_FOREVER) == ERROR){
              result = 1;
              xdServer_CleanEventsQueue(events_queue);
              break;
            };
            add_event = TRUE;
            switch(xdServer_Servers[serverNum].event){
              case xdServer_Event_SingleStep:
                taskRegsGet((int)&debuggedTcb, &RegSet);
                event.Common.Event = eventType_SingleStep;
                add_event = go_mode.SingleStep.add_step;
                stopping_event = TRUE; 
                break;
              case xdServer_Event_TaskDelete:
                RegSet.pc = NULL;
                event.Common.Event = eventType_Exception;
                event.Exception.Exception_ID = excID_ProgramException;
                event.Exception.XCPT_INFO_1  = 0;
                event.Exception.XCPT_INFO_3  = 0;
                prog_started = FALSE;
                stopping_event = TRUE; 
                break;
              case xdServer_Event_BreakpointHit:
                taskRegsGet((int)&debuggedTcb, &RegSet);
                (DWORD)RegSet.pc -= 2;
                event.Common.Event = eventType_BreakpointHit;
                event.BreakpointHit.BreakpointInd = 0;
                stopping_event = TRUE; 
                break;
            };
            event.Common.pc = (DWORD)RegSet.pc;
            if(add_event){
              if(msgQSend(events_queue, (char*)&event, sizeof(EVENT), NO_WAIT, MSG_PRI_NORMAL) == ERROR){
                result = 1;
                xdServer_CleanEventsQueue(events_queue);
                break;
              };
            };
            if(stopping_event) break;
          };
        };
        if(SendEvents(result, events_queue) == ERROR) printf("Send result error\n");
        break;
      case cmdGetSegmentInfo:
        printf("Get segment info\n");
        addr = DWChEnd(*(DWORD*)(arg_buf));
        if(arg_size == 4 && addr >= 0 && addr < (DWORD)sysMemTop()){
          if(SendResult(0, 9) == ERROR) printf("Send result error\n");
          if(SendDW(0) == ERROR) printf("Send result error\n");
          if(SendDW((DWORD)sysMemTop()) == ERROR) printf("Send result error\n");
          if(SendB(attrRead | attrWrite | attrExecute) == ERROR) printf("Send result error\n");
        }else{
          if(SendResult(1, 0) == ERROR) printf("Send result error\n");
        };
        break;
      case cmdIsExecutableSeg:
        printf("Is executable segment\n");
        if(SendResult(0, 1) == ERROR) printf("Send result error\n");
        if(SendB(1) == ERROR) printf("Send result error\n");
        break;
      case cmdReadExport:
        printf("Read export\n");
        ExamineSymbol_Args.action = xdServer_ExamineSymbol_Action_Calc;
        tmp_mod_id = moduleFindByName(arg_buf);
        if(tmp_mod_id == NULL){
          if(SendResult(1, 0) == ERROR) printf("Send result error\n");
        }else{
          printf("%s: there is mod id!\n", arg_buf);
          ExamineSymbol_Args.group  = tmp_mod_id->group;
          ExamineSymbol_Args.number = 0;
          if(symEach(sysSymTbl, (FUNCPTR)xdServer_ExamineSymbol, (int)&ExamineSymbol_Args) != NULL){
            if(SendResult(1, 0) == ERROR) printf("Send result error\n");
          }else{
            printf("%s: symbol number = %d\n", arg_buf, ExamineSymbol_Args.number);
            ExamineSymbol_Args.action = xdServer_ExamineSymbol_Action_PutData;
            ExamineSymbol_Args.i = 0;
            ExamineSymbol_Args.exp = (EXPORTED*) malloc(ExamineSymbol_Args.number * sizeof(EXPORTED));
            if(symEach(sysSymTbl, (FUNCPTR)xdServer_ExamineSymbol, (int)&ExamineSymbol_Args) != NULL ||
               xdServer_GetExecInfo(tmp_mod_id, &tmp_exec_info, TRUE) == ERROR)
            {
              if(SendResult(1, 0) == ERROR) printf("Send result error\n");
            }else{

              for(len = i = 0; i < ExamineSymbol_Args.number; i++){
                if(ExamineSymbol_Args.exp[i].obj == 5 &&
                   ExamineSymbol_Args.exp[i].offset >= tmp_exec_info.Objects[0].Begin &&
                   ExamineSymbol_Args.exp[i].offset < tmp_exec_info.Objects[0].End)
                {
                  ExamineSymbol_Args.exp[i].obj = 1;
                  ExamineSymbol_Args.exp[i].offset -= tmp_exec_info.Objects[0].Begin;
                  len++;
                }else{
                  ExamineSymbol_Args.exp[i].obj = 0;
                };
              };
              printf("%s: symbol number = %d\n", arg_buf, ExamineSymbol_Args.number);
              SendExport(ExamineSymbol_Args.exp, ExamineSymbol_Args.number);
            };
            free(ExamineSymbol_Args.exp);
          };
        };
        break;
      default:
        printf("Something error, command code %u\n", command);
        continue_flag = FALSE;
        break;
    };
  };

  /* Finilize */
  close(pipeID);
  if(mod_id) unldByModuleId(mod_id, UNLD_KEEP_BREAKPOINTS);
  if(prog_started) taskDelete((int)&debuggedTcb);
  if(semaphore) semDelete(semaphore);
  if(events_queue) msgQDelete(events_queue);
  xdServer_Servers[serverNum].free = TRUE;
};