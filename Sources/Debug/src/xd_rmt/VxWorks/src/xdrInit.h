/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrInit.h                                                  *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains intarface to main routine.       *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrInit_h
#define _xdrInit_h

#include <semLib.h>

extern int    xdrInit_StartedTasksNumber;  /* Number of tasks waiting start 
                                              semaphore. */
extern SEM_ID xdrInit_StartSemaphore;      /* Semaphore showing all 
                                              to run.                   */

#endif