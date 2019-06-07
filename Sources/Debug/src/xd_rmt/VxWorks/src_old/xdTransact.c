#include <vxWorks.h>
#include <stdio.h>

#include "xdAssert.h"
#include "xdTransact.h"

/* -----------------------------------------------------*/
int xdTransact_Send(int pipeID, char * buff, int len){
  return write(pipeID, buff, len);
};

int xdTransact_Receive(int pipeID, char * buff, int len){
  return read(pipeID, buff, len);
};

/* -----------------------------------------------------*/
int xdTransact_SendB(int pipeID, BYTE b){
  return Send(&b, 1);
};

int xdTransact_SendW(int pipeID, WORD w){
  w = WChEnd(w);
  return Send(&w, 2);
};

int xdTransact_SendDW(int pipeID, DWORD dw){
  dw = DWChEnd(dw);
  return Send(&dw, 4);
};

int xdTransact_ReceiveW(int pipeID, WORD * w){
  if(Receive(w, 2) == ERROR) return ERROR;
  *w = WChEnd(*w);
  return 2;
};

int xdTransact_ReceiveDW(int pipeID, DWORD * dw){
  if(Receive(dw, 4) == ERROR) return ERROR;
  *dw = DWChEnd(*dw);
  return 4;
};

/* -----------------------------------------------------*/
int xdTransact_SendDesc(int pipeID, DATA_DESC * desc){
  if(SendB(desc->sort) == ERROR) return ERROR;
  printf("Desc.len = %d\n", desc->len);
  return SendDW(desc->len);
};

/* -----------------------------------------------------*/
int xdTransact_ReceiveDesc(int pipeID, DATA_DESC * desc){
  if(ReceiveB(desc->sort) == ERROR) return ERROR;
  return ReceiveDW(desc->len);
};

/* -----------------------------------------------------*/
int xdTransact_SendReady(int pipeID){
  DATA_DESC desc;

  desc.sort = dtReady;
  desc.len  = 0;
  return SendDesc(desc);
};

int xdTransact_SendResult(int pipeID, DWORD resCode, DWORD size){
  DATA_DESC desc;

  desc.sort = dtResult;
  desc.len  = size + 4;
  if(SendDesc(desc) == ERROR) return ERROR;
  SendDW(resCode);
};

int xdTransact_SendStr(int pipeID, char * str){
  DATA_DESC desc;
  unsigned char * p;

  desc.sort = dtString;
  desc.len  = strlen(str) + 1;

  if(SendDesc(desc) == ERROR) return ERROR;
  return Send(str, strlen(str) + 1);
};

int xdTransact_SendRawData(int pipeID, BYTE * buff, int len){
  DATA_DESC desc;

  desc.sort = dtData;
  desc.len  = 1;
  if(SendDesc(desc) == ERROR) return ERROR;
  return Send(buff, len);
};

int xdTransact_SendEXEC_INFORec (int pipeID, EXEC_INFO * exec_info){

  Send   (exec_info->short_name, sizeof(PROGRAM_NAME));
  Send   (exec_info->full_name , sizeof(PROGRAM_NAME));
  SendB  (exec_info->app_type);
  SendDW (exec_info->Handle);
  SendDW (exec_info->MainEntry);
  Send   (exec_info->DebugInfoTag, sizeof(typeDebugInfoTag));
  SendDW (exec_info->DebugInfoStart);
  SendDW (exec_info->DebugInfoSize);
  SendDW (exec_info->N_Objects);
  SendDW ((DWORD)exec_info->Objects);
  SendDW (exec_info->CodeObject);
};

int xdTransact_SendOBJECTRec    (int pipeID, OBJECT * object){

  SendB(object->Attributes);
  SendDW(object->Begin);
  SendDW(object->End);
};

int xdTransact_SendEvents       (int pipeID, DWORD result, MSG_Q_ID events_queue){
  BYTE * buff, * p, * begin;
  EVENT event;
  int   i;

  if(result == 0){

    if((p = buff = (BYTE*) malloc(MAX_EVENTS*552)) == NULL) return ERROR;

    while(msgQReceive(events_queue, (char*)&event, sizeof(EVENT), NO_WAIT) != ERROR){
      begin = p;
      *(DWORD*)p = DWChEnd(event.Common.pc); p += 4;
      *p = event.Common.Event; p++;
      switch(event.Common.Event){
        case eventType_SingleStep:
          printf("Send event: Single Step\n");
          if((DWORD)(p - begin) % 552 > 0) p += 552 - (DWORD)(p - begin) % 552;
          break;
        case eventType_Exception:
          printf("Send event: Exception\n");
          *p = event.Exception.Exception_ID; p++;
          *(DWORD*)p = DWChEnd(event.Exception.XCPT_INFO_1); p += 4;
          *(DWORD*)p = DWChEnd(event.Exception.XCPT_INFO_2); p += 4;
          *(DWORD*)p = DWChEnd(event.Exception.XCPT_INFO_3); p += 4;
          *(DWORD*)p = DWChEnd(event.Exception.XCPT_INFO_4); p += 4;
          if((DWORD)(p - begin) % 552 > 0) p += 552 - (DWORD)(p - begin) % 552;
          break;
        case eventType_BreakpointHit:
          printf("Send event: Breakpoint Hit\n");
          *(DWORD*)p = DWChEnd(event.BreakpointHit.BreakpointInd); p += 4;
          if((DWORD)(p - begin) % 552 > 0) p += 552 - (DWORD)(p - begin) % 552;
          break;
        case eventType_CompCreated:
          printf("Send event: Component Created\n");
          memcpy(p, event.CompCreated.Component.short_name, sizeof(PROGRAM_NAME)); p += sizeof(PROGRAM_NAME);
          memcpy(p, event.CompCreated.Component.full_name , sizeof(PROGRAM_NAME)); p += sizeof(PROGRAM_NAME);
          *p = event.CompCreated.Component.app_type; p++;
          *(DWORD*)p = DWChEnd(event.CompCreated.Component.Handle); p += 4;
          *(DWORD*)p = DWChEnd(event.CompCreated.Component.MainEntry); p += 4;
 
          memcpy(p, event.CompCreated.Component.DebugInfoTag, sizeof(typeDebugInfoTag)); p += sizeof(typeDebugInfoTag);

          *(DWORD*)p = DWChEnd(event.CompCreated.Component.DebugInfoStart); p += 4;
          *(DWORD*)p = DWChEnd(event.CompCreated.Component.DebugInfoSize); p += 4;
          *(DWORD*)p = DWChEnd(event.CompCreated.Component.N_Objects); p += 4;
          *(DWORD*)p = DWChEnd((DWORD)event.CompCreated.Component.Objects); p += 4;
          *(DWORD*)p = DWChEnd(event.CompCreated.Component.CodeObject); p += 4;
  
          *p = event.CompCreated.Stopable; p++;

          if((DWORD)(p - begin) % 552 > 0) p += 552 - (DWORD)(p - begin) % 552;
    
          for(i = 0; i < event.CompCreated.Component.N_Objects; i++){
            *p = event.CompCreated.Component.Objects[i].Attributes; p++;
            *(DWORD*)p = DWChEnd(event.CompCreated.Component.Objects[i].Begin); p += 4;
            *(DWORD*)p = DWChEnd(event.CompCreated.Component.Objects[i].End); p += 4;
          };

          break;
        default:
          return ERROR;
      };
    };

    if(SendResult(result, (DWORD)(p - buff)) == ERROR) return ERROR;
    if(Send(buff, (DWORD)(p - buff)) == ERROR) return ERROR;

    free(buff);
  }else{
    if(SendResult(result, 0) == ERROR) return ERROR;
  };
  return OK;
};

int xdTransact_SendExport(int pipeID, EXPORTED * export, int exp_len){
  int i, len, num;

  printf("exp_len = %d\n", exp_len);

  for(len = num = i = 0; i < exp_len; i++){
    if(export[i].obj > 0){
      num++;
      len += 12 + strlen(export[i].name) + 1;
    };
  };

  if(SendResult(0, len + 4) == ERROR) return ERROR;
  if(SendDW((DWORD)num) == ERROR) return ERROR;
  if(len > 4){
    for(i = 0; i < exp_len; i++){
      if(export[i].obj > 0){
        if(SendDW(export[i].obj)                   == ERROR) return ERROR;
        if(SendDW(export[i].offset)                == ERROR) return ERROR;
        if(SendDW(len = strlen(export[i].name)+1) == ERROR) return ERROR;
        if(Send(export[i].name, len)              == ERROR) return ERROR;
      };
    };
  };
};

/* -----------------------------------------------------*/
int xdTransact_ReceiveStr(int pipeID, char * str, int len){
  DATA_DESC desc;
  int l;

  if(ReceiveDesc(desc) == ERROR || desc.sort != dtString || desc.len < 1) return ERROR;
  if((l = Receive(str, len)) == ERROR || l != desc.len) return ERROR;
  return l;
};

WORD xdTransact_ReceiveCommand(int pipeID, DWORD * arg_size){
  DATA_DESC desc;
  COMMAND c;

  c = cmdNone;
  *arg_size = 0;
  if(ReceiveDesc(desc) != ERROR && (desc.sort == dtCommand) && (desc.len >= 2)){
    if(ReceiveW(c) && (c <= MaxCmd)){
      *arg_size = desc.len - 2;
    };
  };
  return c;
};

int xdTransact_ReceiveRawData(int pipeID, BYTE * buff, int len){
  DATA_DESC desc;
  DWORD l;


  if(ReceiveDesc(desc) == ERROR || desc.sort != dtData || desc.len != len) return ERROR;
  if((l = Receive(buff, desc.len)) == ERROR) return ERROR;
  return l == desc.len ? desc.len : ERROR;
};

/* -----------------------------------------------------*/
#define ServerKeyPttr  "XDS x86 %s-server side"
#define ClientKeyPttr  "XDS x86 %s-client side"
#define VersionKey     230


/* -----------------------------------------------------*/
int xdTransact_ServerConnect(int pipeID, char * transport){
  char ServerKey[100], ClientKey[100];
  char client_key[100];
  BYTE ver_key = VersionKey;


  sprintf(ServerKey, ServerKeyPttr, transport);
  sprintf(ClientKey, ClientKeyPttr, transport);

  if(SendStr(ServerKey) != ERROR && SendRawData(&ver_key, 1) != ERROR){
    if(ReceiveStr(client_key) != ERROR && strcmp(client_key, ClientKey) == 0){
      if(ReceiveRawData(&ver_key, 1) != ERROR && ver_key == VersionKey){
        if(SendReady() != ERROR){
          return 0;
        };
      };
    };
  };

  perror("Wrong identification\n");
  return ERROR;
};



