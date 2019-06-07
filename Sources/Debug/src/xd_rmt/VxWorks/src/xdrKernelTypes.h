/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrKernelTypes.h                                           *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains special data types and constants *|
|*                 definitions.                                               *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrKernelTypes_h
#define _xdrKernelTypes_h


#include "xdrTypes.h"


typedef char xdrKernelTypes_ProgramName[256];
typedef char xdrKernelTypes_ProgramArgs[512];
typedef char xdrKernelTypes_SymbolName [256];


typedef BYTE xdrKernelTypes_AppType;
#define xdrKernelTypes_apptypeNone    0
#define xdrKernelTypes_apptypeConsole 1


typedef BYTE xdrKernelTypes_Attribs;
#define xdrKernelTypes_attrExecute 1
#define xdrKernelTypes_attrRead    2
#define xdrKernelTypes_attrWrite   4



typedef struct xdrKernelTypes_tagObject{
  xdrKernelTypes_Attribs Attributes;
  DWORD                  Begin;
  DWORD                  End;
} xdrKernelTypes_Object;

typedef char xdrKernelTypes_typeDebugInfoTag[5];

typedef struct xdrKernelTypes_tagModuleInfo{
  xdrKernelTypes_ProgramName       short_name;
  xdrKernelTypes_ProgramName       full_name;
  xdrKernelTypes_AppType           app_type;
  int                              Handle;
  DWORD                            MainEntry;
  xdrKernelTypes_typeDebugInfoTag  DebugInfoTag;
  DWORD                            DebugInfoStart;
  DWORD                            DebugInfoSize;
  DWORD                            N_Objects;
  xdrKernelTypes_Object            Objects[3];
  DWORD                            CodeObject;
} xdrKernelTypes_ModuleInfo;        


#define xdrKernelTypes_ModuleInfoSize \
                      (sizeof(xdrKernelTypes_ProgramName) * 2 + \
                       sizeof(xdrKernelTypes_AppType) + \
                       sizeof(int) + \
                       sizeof(DWORD) * 5 + \
                       sizeof(xdrKernelTypes_typeDebugInfoTag) + \
                       sizeof(xdrKernelTypes_Object*))

#define xdrKernelTypes_ObjectSize (sizeof(xdrKernelTypes_Attribs) + sizeof(DWORD) * 2)



typedef char xdrKernelTypes_EntryName[256];

typedef struct{
  DWORD                    obj;
  DWORD                    offset;
  xdrKernelTypes_EntryName name;
} xdrKernelTypes_Exported;



/*----------------------------------------------------------------------------*/
#define xdrKernelTypes_GoMode_None	  0  /* Режимы исполнения программы             */
#define xdrKernelTypes_GoMode_SingleStep  1  /* Исполнить одну команду                  */
#define xdrKernelTypes_GoMode_RangeStep   2  /* Исполнять в указанном диапазоне адресов */
#define xdrKernelTypes_GoMode_Go          3  /* Исполнять до возникновения события      */
  

typedef struct xdrKernelTypes_tagGoMode{

  struct{
    BYTE mode;
  } Header;

  union{
    struct{
      DWORD Begin, End; /* Диапазон адресов                        */
    } RangeStep;        /* Исполнять в указанном диапазоне адресов */

    struct{
      BYTE add_step;    /* Добавлять событие SingleStep? */
    } SingleStep;       /* Исполнить одну команду        */
  } Body;

} xdrKernelTypes_GoMode;


/*----------------------------------------------------------------------------*/
typedef BYTE xdrKernelTypes_ExceptionID;
#define xdrKernelTypes_ExceptionID_OutOfMemory       0   /* Доступ по адресу вне диапазона адресов */
#define xdrKernelTypes_ExceptionID_WriteProtected    1   /* Запись в защищенную область памяти     */
#define xdrKernelTypes_ExceptionID_ProgramException  2   /* Программное прерывание                 */
#define xdrKernelTypes_ExceptionID_UserException     3   /* Исполнение прервано пользователем      */


typedef BYTE xdrKernelTypes_EventType;
#define xdrKernelTypes_EventType_InternalError      0   /* Исключительная ситуация в отладчике */
#define xdrKernelTypes_EventType_Exception          1   /* Исключительная ситуация в программе */
#define xdrKernelTypes_EventType_BreakpointHit      2   /* Точка останова                      */
#define xdrKernelTypes_EventType_SingleStep         3   /* Исполнена одна команда              */
#define xdrKernelTypes_EventType_Call               4   /* Выполнена инструкция CALL           */
#define xdrKernelTypes_EventType_Return             5   /* Выполнена инструкция RET            */
#define xdrKernelTypes_EventType_MemoryAccess       6   /* Достпуп к памяти                    */
#define xdrKernelTypes_EventType_ComponentCreated   7   /* Создана новая компонента программы  */
#define xdrKernelTypes_EventType_ComponentDestroyed 8   /* Создана новая компонента программы  */
#define xdrKernelTypes_EventType_ThreadCreated      9   /* Создан thread                       */
#define xdrKernelTypes_EventType_ThreadDestroyed    10  /* Удален thread                       */


typedef BYTE xdrKernelTypes_AccessType;
#define xdrKernelTypes_AccessType_Nothing    0
#define xdrKernelTypes_AccessType_Read       1
#define xdrKernelTypes_AccessType_Write      2
#define xdrKernelTypes_AccessType_ReadWrite  3

/* Информация о последнем произошедшем событии */
typedef struct xdrKernelTypes_tagEvent{

  struct{
    DWORD                    pc;        /* Текуший адрес */
    xdrKernelTypes_EventType eventType; /* Тип события   */
  } Header;

  union{

    struct{
    } SingleStep;     /* Исполнена одна команда  */

    struct{
      DWORD      ErrorNo;      /* Номер ошибки                        */
      DWORD      ErrorContext; /* Дополнительные атрибуты             */
    } InternalError;           /* Исключительная ситуация в отладчике */

    struct{
      xdrKernelTypes_ExceptionID exceptionID;
      DWORD                      XCPT_INFO_1;
      DWORD                      XCPT_INFO_2;    
      DWORD                      XCPT_INFO_3;    
      DWORD                      XCPT_INFO_4;    
    } Exception;      /* Исключительная ситуация в программе */

    struct{
      DWORD CallAddr;          /* Адрес вызываемой команды   */
    } Call;           /* Выполнена инструкция CALL  */

    struct{
      DWORD ReturnAddr;        /* Адрес возврата             */
    } Return;         /* Выполнена инструкция RET   */

    struct{
      DWORD BreakpointInd;
    } BreakpointHit;  /* Точка останова             */

    struct{
      xdrKernelTypes_ModuleInfo Component;
      BYTE                      Stopable;
    } ComponentCreated;

    struct{
      DWORD        Handle;
    } ComponentDestroyed;

    struct{
      DWORD tid;                 /* Task ID                    */
    } ThreadCreated;

    struct{
      DWORD tid;                 /* Task ID                    */
    } ThreadDestroyed;

  } Body;

} xdrKernelTypes_Event;

#define MAX_EVENTS 10



#endif
