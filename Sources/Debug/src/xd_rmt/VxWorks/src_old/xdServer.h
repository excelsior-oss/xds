#ifndef _xdServer_H
#define _xdServer_H

#include <semLib.h>

#define MAX_SERVERS 10

#define xdServer_Event_None               0
#define xdServer_Event_SingleStep         0x100
#define xdServer_Event_BreakpointHit      0x101
#define xdServer_Event_TaskDelete         0x102

#define xdServer_DebuggedTaskTag_Protected 1

typedef struct {
  int   tid;
  DWORD tags;
} xdServer_DebuggedTaskDesc;


typedef struct {
  BOOL                          free;
  int                           debuggedTasksNum;
  xdServer_DebuggedTaskDesc  *  debuggedTasks;
  int                           debuggerTaskId;
  SEM_ID                        semaphore;
  int                           event;
} xdServer_ServerDesc;

extern xdServer_ServerDesc xdServer_Servers[MAX_SERVERS];

extern void xdServer_Server(int pipeID, char * transport, DWORD instance);

/* Hooks */
extern void xdServer_IntrStub(int intr);
extern void xdServer_TaskDeleteHook(WIND_TCB * pTcb);

#endif