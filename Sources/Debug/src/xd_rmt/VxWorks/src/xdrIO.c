/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrIO.c                                                    *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This file contains implementation of input/output module   *|
|*                                                                            *|
\******************************************************************************/


#include <vxWorks.h>
#include <stdio.h>
#include <msgQLib.h>

#include "xdrIncl.h"
#include "xdrTrace.h"
#include "xdrAssert.h"
#include "xdrIO.h"


/*----------------------------------------------------------------------------*/
#define ServerKey  "XDS x86 TCP-server side"
#define ClientKey  "XDS x86 TCP-client side"
#define VersionKey 230


/*----------------------------------------------------------------------------*/
/* Message types */

#define dtReady   0
#define dtData    1
#define dtString  2
#define dtCommand 3
#define dtResult  4



struct xdrIO_tagDataDescriptor{
  BYTE  sort;
  DWORD len;
};


/*----------------------------------------------------------------------------*/
DWORD DWChEnd(DWORD dw){
  BYTE * p, b;

  p = (BYTE*)&dw; b = *p;
  *p = *(p+3);
  *(p+3) = b;
  b = *(p+1);
  *(p+1) = *(p+2);
  *(p+2) = b;
  return dw;
};

WORD WChEnd(WORD w){
  BYTE * p, b;

  p = (BYTE*)&w; b = *p;
  *p = *(p+1);
  *(p+1) = b;
  return w;
};


/*----------------------------------------------------------------------------*/
int xdrIO_Send(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, char * buff, int len){
  return xdrTransports_Write(pipeDesc, channelNo, buff, len);
};

int xdrIO_Receive(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, char * buff, int len){
  return xdrTransports_Read(pipeDesc, channelNo, buff, len);
};

/*----------------------------------------------------------------------------*/
int xdrIO_SendB(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, BYTE b){
  return Send(channelNo, &b, 1);
};

/* -----------------------------------------------------*/
int xdrIO_SendW(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, WORD w){
  w = WChEnd(w);
  return Send(channelNo, &w, 2);
};

/* -----------------------------------------------------*/
int xdrIO_SendDW(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, DWORD dw){
  dw = DWChEnd(dw);
  return Send(channelNo, &dw, 4);
};

/* -----------------------------------------------------*/
int xdrIO_ReceiveW(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, WORD * w){
  if(Receive(channelNo, w, 2) == ERROR) return ERROR;
  *w = WChEnd(*w);
  return 2;
};

/* -----------------------------------------------------*/
int xdrIO_ReceiveDW(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, DWORD * dw){
  if(Receive(channelNo, dw, 4) == ERROR) return ERROR;
  *dw = DWChEnd(*dw);
  return 4;
};

/*----------------------------------------------------------------------------*/
int xdrIO_SendDesc(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, xdrIO_DataDescriptor * desc){
  int res;

  res = SendB(channelNo, desc->sort);
  if(res != ERROR) res = SendDW(channelNo, desc->len);

#if xdrTrace_Mode & xdrTrace_modeIO
  if(res != ERROR){
    xdrTrace_Trace("IO: data descriptor was sent, length %d\n", desc->len);
  }else{
    xdrTrace_Trace("IO: data descriptor WAS NOT sent, length %d\n", desc->len);
  };
#endif

  return res;
};

/* -----------------------------------------------------*/
int xdrIO_ReceiveDesc(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, xdrIO_DataDescriptor * desc){
  int res;

  res = ReceiveB(channelNo, desc->sort);
  if(res != ERROR) res = ReceiveDW(channelNo, desc->len);

#if xdrTrace_Mode & xdrTrace_modeIO
  if(res != ERROR){
    xdrTrace_Trace("IO: data descriptor was received, length %d\n", desc->len);
  }else{
    xdrTrace_Trace("IO: data descriptor WAS NOT received\n");
  };
#endif

  return res;
};

/*----------------------------------------------------------------------------*/
int xdrIO_SendReady(xdrTransports_PipeDescriptor * pipeDesc, int channelNo){
  xdrIO_DataDescriptor desc;

  desc.sort = dtReady;
  desc.len  = 0;
  return SendDesc(channelNo, desc);
};

/* -----------------------------------------------------*/
int xdrIO_SendResult(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, DWORD resCode, DWORD size){
  xdrIO_DataDescriptor desc;

  desc.sort = dtResult;
  desc.len  = size + 4;
  if(SendDesc(channelNo, desc) == ERROR) return ERROR;
  SendDW(channelNo, resCode);
};

/* -----------------------------------------------------*/
int xdrIO_SendStr(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, char * str){
  xdrIO_DataDescriptor desc;
  unsigned char * p;

  desc.sort = dtString;
  desc.len  = strlen(str) + 1;

  if(SendDesc(channelNo, desc) == ERROR) return ERROR;
  return Send(channelNo, str, strlen(str) + 1);
};

/* -----------------------------------------------------*/
int xdrIO_SendRawData(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, BYTE * buff, int len){
  xdrIO_DataDescriptor desc;

  desc.sort = dtData;
  desc.len  = 1;
  if(SendDesc(channelNo, desc) == ERROR) return ERROR;
  return Send(channelNo, buff, len);
};

/* -----------------------------------------------------*/
int xdrIO_SendModuleInfo(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, xdrKernelTypes_ModuleInfo * moduleInfo){

  Send  (channelNo, moduleInfo->short_name, sizeof(xdrKernelTypes_ProgramName));
  Send  (channelNo, moduleInfo->full_name , sizeof(xdrKernelTypes_ProgramName));
  SendB (channelNo, moduleInfo->app_type);
  SendDW(channelNo, moduleInfo->Handle);
  SendDW(channelNo, moduleInfo->MainEntry);
  Send  (channelNo, moduleInfo->DebugInfoTag, sizeof(xdrKernelTypes_typeDebugInfoTag));
  SendDW(channelNo, moduleInfo->DebugInfoStart);
  SendDW(channelNo, moduleInfo->DebugInfoSize);
  SendDW(channelNo, moduleInfo->N_Objects);
  SendDW(channelNo, (DWORD)moduleInfo->Objects);
  SendDW(channelNo, moduleInfo->CodeObject);
};

int xdrIO_SendObjectInfo(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, xdrKernelTypes_Object * object){

  SendB(channelNo,  object->Attributes);
  SendDW(channelNo, object->Begin);
  SendDW(channelNo, object->End);
};

int xdrIO_SendEvents(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, DWORD result, MSG_Q_ID eventsQueue){
  BYTE * buff, * p, * begin;
  xdrKernelTypes_Event event;
  int   i;

  if(result == xdrTypes_errOK){
    
    if((p = buff = (BYTE*) malloc(MAX_EVENTS*552)) == NULL) return ERROR;

    while(msgQReceive(eventsQueue, (char*)&event, sizeof(xdrKernelTypes_Event), NO_WAIT) != ERROR){
      begin = p;
      *(DWORD*)p = DWChEnd(event.Header.pc); p += 4;
      *p = event.Header.eventType; p++;
      switch(event.Header.eventType){
        case xdrKernelTypes_EventType_SingleStep:

#if xdrTrace_Mode & xdrTrace_modeIO
          xdrTrace_Trace("Send event: Single Step\n");
#endif

          if((DWORD)(p - begin) % 552 > 0) p += 552 - (DWORD)(p - begin) % 552;
          break;
        case xdrKernelTypes_EventType_Exception:

#if xdrTrace_Mode & xdrTrace_modeIO
          xdrTrace_Trace("Send event: Exception\n");
#endif

          *p = event.Body.Exception.exceptionID; p++;
          *(DWORD*)p = DWChEnd(event.Body.Exception.XCPT_INFO_1); p += 4;
          *(DWORD*)p = DWChEnd(event.Body.Exception.XCPT_INFO_2); p += 4;
          *(DWORD*)p = DWChEnd(event.Body.Exception.XCPT_INFO_3); p += 4;
          *(DWORD*)p = DWChEnd(event.Body.Exception.XCPT_INFO_4); p += 4;
          if((DWORD)(p - begin) % 552 > 0) p += 552 - (DWORD)(p - begin) % 552;
          break;
        case xdrKernelTypes_EventType_BreakpointHit:

#if xdrTrace_Mode & xdrTrace_modeIO
          xdrTrace_Trace("Send event: Breakpoint Hit\n");
#endif

          *(DWORD*)p = DWChEnd(event.Body.BreakpointHit.BreakpointInd); p += 4;
          if((DWORD)(p - begin) % 552 > 0) p += 552 - (DWORD)(p - begin) % 552;
          break;
        case xdrKernelTypes_EventType_ComponentCreated:

#if xdrTrace_Mode & xdrTrace_modeIO
          xdrTrace_Trace("Send event: Component Created\n");
#endif

          memcpy(p, event.Body.ComponentCreated.Component.short_name, sizeof(xdrKernelTypes_ProgramName)); p += sizeof(xdrKernelTypes_ProgramName);
          memcpy(p, event.Body.ComponentCreated.Component.full_name , sizeof(xdrKernelTypes_ProgramName)); p += sizeof(xdrKernelTypes_ProgramName);
          *p = event.Body.ComponentCreated.Component.app_type; p++;
          *(DWORD*)p = DWChEnd(event.Body.ComponentCreated.Component.Handle); p += 4;
          *(DWORD*)p = DWChEnd(event.Body.ComponentCreated.Component.MainEntry); p += 4;

          memcpy(p, event.Body.ComponentCreated.Component.DebugInfoTag, sizeof(xdrKernelTypes_typeDebugInfoTag)); p += sizeof(xdrKernelTypes_typeDebugInfoTag);

          *(DWORD*)p = DWChEnd(event.Body.ComponentCreated.Component.DebugInfoStart); p += 4;
          *(DWORD*)p = DWChEnd(event.Body.ComponentCreated.Component.DebugInfoSize); p += 4;
          *(DWORD*)p = DWChEnd(event.Body.ComponentCreated.Component.N_Objects); p += 4;
          *(DWORD*)p = DWChEnd((DWORD)event.Body.ComponentCreated.Component.Objects); p += 4;
          *(DWORD*)p = DWChEnd(event.Body.ComponentCreated.Component.CodeObject); p += 4;

          *p = event.Body.ComponentCreated.Stopable; p++;

          if((DWORD)(p - begin) % 552 > 0) p += 552 - (DWORD)(p - begin) % 552;

          for(i = 0; i < event.Body.ComponentCreated.Component.N_Objects; i++){
            *p = event.Body.ComponentCreated.Component.Objects[i].Attributes; p++;
            *(DWORD*)p = DWChEnd(event.Body.ComponentCreated.Component.Objects[i].Begin); p += 4;
            *(DWORD*)p = DWChEnd(event.Body.ComponentCreated.Component.Objects[i].End); p += 4;
          };

          break;
        default:
          return ERROR;
      };
    };

#if xdrTrace_Mode & xdrTrace_modeIO
    xdrTrace_Trace("Send events: full length %d\n", (DWORD)(p - buff));
#endif

    if(SendResult(channelNo, result, (DWORD)(p - buff)) == ERROR) return ERROR;
    if(Send(channelNo, buff, (DWORD)(p - buff)) == ERROR) return ERROR;

    free(buff);
  }else{
    if(SendResult(channelNo, result, 0) == ERROR) return ERROR;
  };
  return OK;
};

int xdrIO_SendExport(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, xdrKernelTypes_Exported * export, int exp_len){
  int i, len, num;

  for(len = num = i = 0; i < exp_len; i++){
    if(export[i].obj > 0){
      num++;
      len += 12 + strlen(export[i].name) + 1;
    };
  };

  if(SendResult(channelNo, xdrTypes_errOK, len + 4) == ERROR) return ERROR;
  if(SendDW(channelNo, (DWORD)num) == ERROR) return ERROR;
  if(len > 4){
    for(i = 0; i < exp_len; i++){
      if(export[i].obj > 0){
        if(SendDW(channelNo, export[i].obj)                   == ERROR) return ERROR;
        if(SendDW(channelNo, export[i].offset)                == ERROR) return ERROR;
        if(SendDW(channelNo, len = strlen(export[i].name)+1)  == ERROR) return ERROR;
        if(Send(channelNo, export[i].name, len)               == ERROR) return ERROR;
      };
    };
  };
  return OK;
};

/* -----------------------------------------------------*/
int xdrIO_ReceiveStr(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, char * str, int len){
  xdrIO_DataDescriptor desc;
  int l;

  if(ReceiveDesc(channelNo, desc) == ERROR || desc.sort != dtString || desc.len < 1) return ERROR;
  if((l = Receive(channelNo, str, len)) == ERROR || l != desc.len) return ERROR;
  return l;
};

/* -----------------------------------------------------*/
WORD xdrIO_ReceiveCommand(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, DWORD * arg_size){
  xdrIO_DataDescriptor desc;
  xdrIO_Command c;

  c = cmdNone;
  *arg_size = 0;
  if(ReceiveDesc(channelNo, desc) != ERROR && (desc.sort == dtCommand) && (desc.len >= 2)){
    if(ReceiveW(channelNo, c) && (c <= MaxCmd)){
      *arg_size = desc.len - 2;
    };
  };                       
  return c;
};

/* -----------------------------------------------------*/
int xdrIO_ReceiveRawData(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, BYTE * buff, int len){
  xdrIO_DataDescriptor desc;
  DWORD l;


  if(ReceiveDesc(channelNo, desc) == ERROR || desc.sort != dtData || desc.len != len) return ERROR;
  if((l = Receive(channelNo, buff, desc.len)) == ERROR) return ERROR;
  return l == desc.len ? desc.len : ERROR;
};

/* -----------------------------------------------------*/
int xdrIO_ServerConnect(xdrTransports_PipeDescriptor * pipeDesc, int channelNo){
  char client_key[100];
  BYTE ver_key = VersionKey;



  if(SendStr(channelNo, ServerKey) != ERROR && SendRawData(channelNo, &ver_key, 1) != ERROR){
    if(ReceiveStr(channelNo, client_key) != ERROR && strcmp(client_key, ClientKey) == 0){
      if(ReceiveRawData(channelNo, &ver_key, 1) != ERROR && ver_key == VersionKey){
        if(SendReady(channelNo) != ERROR){
          return True;
        };
      };
    };
  };

  return False;
};



