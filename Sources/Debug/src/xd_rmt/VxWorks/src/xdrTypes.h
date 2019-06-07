/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrTypes.h                                                 *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains common date types and constants  *|
|*                 definitions.                                               *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrTypes_h
#define _xdrTypes_h



typedef unsigned char  BYTE;
typedef unsigned short WORD;
typedef unsigned long  DWORD;



#define False 0
#define True  1




/* Task parameters */
#define xdrTypes_TaskWorkPriority   100
#define xdrTypes_TaskStackSize      0x3000



/*----------------------------------------------------------------------------*\
|*                                                                            *|
|*                                Error codes                                 *|
|*                                                                            *|
\*----------------------------------------------------------------------------*/

#define xdrTypes_errOK			0
#define xdrTypes_errInvalidCommand	1
#define xdrTypes_errAccessViolation	2
#define xdrTypes_errBadArguments	3
#define xdrTypes_errUnknownCommand	4
#define xdrTypes_errCreateTask  	5
#define xdrTypes_errGetRegisters  	6
#define xdrTypes_errSetRegisters  	7

#define xdrTypes_errUnknown		1000




#endif
