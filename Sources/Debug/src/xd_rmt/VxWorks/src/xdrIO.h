/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrIO.h                                                    *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains interface to input/output module *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrIO_h
#define _xdrIO_h

#include <msgQLib.h>

#include "xdrTypes.h"
#include "xdrKernelTypes.h"
#include "xdrTransports.h"

/* -----------------------------------------------------*/

typedef struct xdrIO_tagDataDescriptor xdrIO_DataDescriptor;
typedef WORD                           xdrIO_Command;

/* -----------------------------------------------------*/
#define cmdNone                  0
#define cmdStartProgram          1
#define cmdGetExecInfo           2
#define cmdGetDebugInfo          3
#define cmdQuitDebug             4
#define cmdGetRegisterCache      5
#define cmdPutRegisterCache      6
#define cmdGetMem                7
#define cmdPutMem                8
#define cmdExecute               9
#define cmdGetSegmentInfo        10
#define cmdIsExecutableSeg       11
#define cmdReadExport            12
#define cmdGetThreadDescription  13
#define cmdSwitchToThread        14
#define cmdSuspendThread         15
#define cmdResumeThread          16



#define MinCmd           cmdNone
#define MaxCmd           cmdResumeThread


/* -----------------------------------------------------*/
#define SendB(channleNo, b)                          xdrIO_SendB(pipeDesc, (channleNo), (b))
#define SendW(channleNo, w)                          xdrIO_SendW(pipeDesc, (channleNo), (w))
#define SendDW(channleNo, dw)                        xdrIO_SendDW(pipeDesc, (channleNo), (dw))
#define Send(channleNo, buff, len)                   xdrIO_Send(pipeDesc, (channleNo), (char*)(buff), (len))

#define SendDesc(channleNo, desc)                    xdrIO_SendDesc(pipeDesc, (channleNo), &(desc))
#define SendRawData(channleNo, buff, len)            xdrIO_SendRawData(pipeDesc, (channleNo), (buff), (len))
#define SendReady(channleNo)                         xdrIO_SendReady(pipeDesc, (channleNo))
#define SendResult(channleNo, resCode, size)         xdrIO_SendResult(pipeDesc, (channleNo), (resCode), (size))
#define SendStr(channleNo, str)                      xdrIO_SendStr(pipeDesc, (channleNo), str)

#define SendModuleInfo(channleNo, exec_info)         xdrIO_SendModuleInfo(pipeDesc, (channleNo), &(exec_info))
#define SendObjectInfo(channleNo, exec_info)         xdrIO_SendObjectInfo(pipeDesc, (channleNo), &(exec_info))
#define SendEvents(channleNo, result, events_queue)  xdrIO_SendEvents(pipeDesc, (channleNo), (result), (events_queue))
#define SendExport(channleNo, export, exp_len)       xdrIO_SendExport(pipeDesc, (channleNo), (export), (exp_len))


#define ReceiveB(channleNo, b)                       xdrIO_Receive(pipeDesc, (channleNo), (char*)&(b),  1)
#define ReceiveW(channleNo, w)                       xdrIO_ReceiveW(pipeDesc, (channleNo), &(w))
#define ReceiveDW(channleNo, dw)                     xdrIO_ReceiveDW(pipeDesc, (channleNo), &(dw))
#define Receive(channleNo, buf, size)                xdrIO_Receive(pipeDesc, (channleNo), (char*)(buf), (size))

#define ReceiveDesc(channleNo, desc)                 xdrIO_ReceiveDesc(pipeDesc, (channleNo), &(desc))
#define ReceiveRawData(channleNo, buff, len)         xdrIO_ReceiveRawData(pipeDesc, (channleNo), (buff), (len))
#define ReceiveStr(channleNo, str)                   xdrIO_ReceiveStr(pipeDesc, (channleNo), (str), sizeof(str))
#define ReceiveCommand(channleNo, arg_size)          xdrIO_ReceiveCommand(pipeDesc, (channleNo), (arg_size))

#define ServerConnect(channelNo)                     xdrIO_ServerConnect(pipeDesc, (channelNo))


/* -----------------------------------------------------*/
extern int xdrIO_Send(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, char * buff, int len);
extern int xdrIO_Receive(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, char * buff, int len);


/* -----------------------------------------------------*/
extern int xdrIO_SendB    (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, BYTE    b );
extern int xdrIO_SendW    (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, WORD    w );
extern int xdrIO_SendDW   (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, DWORD   dw);

extern int xdrIO_ReceiveW (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, WORD  * w );
extern int xdrIO_ReceiveDW(xdrTransports_PipeDescriptor * pipeDesc, int channelNo, DWORD * dw);

/* -----------------------------------------------------*/
extern int  xdrIO_SendDesc         (xdrTransports_PipeDescriptor * pipeDesc, int channleNo, xdrIO_DataDescriptor * desc);
extern int  xdrIO_SendReady        (xdrTransports_PipeDescriptor * pipeDesc, int channelNo);
extern int  xdrIO_SendResult       (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, DWORD resCode, DWORD size);
extern int  xdrIO_SendStr          (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, char * str);
extern int  xdrIO_SendRawData      (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, BYTE * buff, int len);

extern int  xdrIO_SendModuleInfo   (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, xdrKernelTypes_ModuleInfo * moduleInfo);
extern int  xdrIO_SendObjectInfo   (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, xdrKernelTypes_Object * object);
extern int  xdrIO_SendEvents       (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, DWORD result, MSG_Q_ID eventsQueue);
extern int  xdrIO_SendExport       (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, xdrKernelTypes_Exported * export, int exp_len);
extern int  xdrIO_ReceiveDesc      (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, xdrIO_DataDescriptor * desc);
extern int  xdrIO_ReceiveStr       (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, char * str, int len);
extern xdrIO_Command xdrIO_ReceiveCommand
                                   (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, DWORD * arg_size);
extern int  xdrIO_ReceiveRawData   (xdrTransports_PipeDescriptor * pipeDesc, int channelNo, BYTE * buff, int len);

extern int  xdrIO_ServerConnect    (xdrTransports_PipeDescriptor * pipeDesc, int channelNo);


#endif
