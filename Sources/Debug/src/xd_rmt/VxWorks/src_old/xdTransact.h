#ifndef _xdTransact_H
#define _xdTransact_H

#include <msgQLib.h>
#include "xdTypes.h"

/* -----------------------------------------------------*/
/* Message types */

#define dtReady   0
#define dtData    1
#define dtString  2
#define dtCommand 3
#define dtResult  4

/* -----------------------------------------------------*/

typedef struct{
  BYTE  sort;
  DWORD len;
} DATA_DESC;

typedef WORD COMMAND;

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
#define SendB(b)                    xdTransact_SendB(pipeID, (b))
#define SendW(w)                    xdTransact_SendW(pipeID, (w))
#define SendDW(dw)                  xdTransact_SendDW(pipeID, (dw))
#define Send(buff, len)             xdTransact_Send(pipeID, (char*)(buff), (len))

#define SendDesc(desc)              xdTransact_SendDesc(pipeID, &(desc))
#define SendRawData(buff, len)      xdTransact_SendRawData(pipeID, (buff), (len))
#define SendReady()                 xdTransact_SendReady(pipeID)
#define SendResult(resCode, size)   xdTransact_SendResult(pipeID, (resCode), (size))
#define SendStr(str)                xdTransact_SendStr(pipeID, str)

#define SendEXEC_INFORec(exec_info)       xdTransact_SendEXEC_INFORec(pipeID, &(exec_info))
#define SendOBJECTRec(exec_info)          xdTransact_SendOBJECTRec(pipeID, &(exec_info))
#define SendEvents(result, events_queue)  xdTransact_SendEvents(pipeID, (result), (events_queue))
#define SendExport(export, exp_len)       xdTransact_SendExport(pipeID, (export), (exp_len))


#define ReceiveB(b)                 xdTransact_Receive(pipeID, (char*)&(b),  1)
#define ReceiveW(w)                 xdTransact_ReceiveW(pipeID, &(w))
#define ReceiveDW(dw)               xdTransact_ReceiveDW(pipeID, &(dw))
#define Receive(buf,size)           xdTransact_Receive(pipeID, (char*)(buf), (size))

#define ReceiveDesc(desc)           xdTransact_ReceiveDesc(pipeID, &(desc))
#define ReceiveRawData(buff, len)   xdTransact_ReceiveRawData(pipeID, (buff), (len))
#define ReceiveStr(str)             xdTransact_ReceiveStr(pipeID, (str), sizeof(str))
#define ReceiveCommand(arg_size)    xdTransact_ReceiveCommand(pipeID, (arg_size))

#define ServerConnect(transport)    xdTransact_ServerConnect(pipeID, (transport))

/* -----------------------------------------------------*/

/* -----------------------------------------------------*/
extern int xdTransact_SendB    (int pipeID, BYTE    b );
extern int xdTransact_SendW    (int pipeID, WORD    w );
extern int xdTransact_SendDW   (int pipeID, DWORD   dw);

extern int xdTransact_ReceiveW (int pipeID, WORD  * w );
extern int xdTransact_ReceiveDW(int pipeID, DWORD * dw);

/* -----------------------------------------------------*/
extern int  xdTransact_SendDesc         (int pipeID, DATA_DESC * desc);
extern int  xdTransact_SendReady        (int pipeID);
extern int  xdTransact_SendResult       (int pipeID, DWORD resCode, DWORD size);
extern int  xdTransact_SendStr          (int pipeID, char * str);
extern int  xdTransact_SendRawData      (int pipeID, BYTE * buff, int len);

extern int  xdTransact_SendEXEC_INFORec (int pipeID, EXEC_INFO * exec_info);
extern int  xdTransact_SendOBJECTRec    (int pipeID, OBJECT * object);
extern int  xdTransact_SendEvents       (int pipeID, DWORD result, MSG_Q_ID events_queue);
extern int  xdTransact_SendExport       (int pipeID, EXPORTED * export, int exp_len);

extern int  xdTransact_ReceiveDesc      (int pipeID, DATA_DESC * desc);
extern int  xdTransact_ReceiveStr       (int pipeID, char * str, int len);
extern WORD xdTransact_ReceiveCommand   (int pipeID, DWORD * arg_size);
extern int  xdTransact_ReceiveRawData   (int pipeID, BYTE * buff, int len);

extern int  xdTransact_ServerConnect    (int pipeID, char * transport);

#endif
