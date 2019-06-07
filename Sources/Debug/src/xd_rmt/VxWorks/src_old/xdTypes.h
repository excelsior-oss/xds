#ifndef _xdTypes_H
#define _xdTypes_H

typedef unsigned char  BYTE;
typedef unsigned short WORD;
typedef unsigned long  DWORD;

extern DWORD DWChEnd(DWORD dw);
extern WORD  WChEnd (WORD w);

/* -----------------------------------------------------*/
typedef char PROGRAM_NAME[256];

typedef BYTE APP_TYPE;
#define apptypeNone    0
#define apptypeConsole 1


typedef BYTE ATTRIBS;
#define attrExecute 1
#define attrRead    2
#define attrWrite   4



typedef struct{
  ATTRIBS Attributes;
  DWORD   Begin;
  DWORD   End;
} OBJECT;

typedef char typeDebugInfoTag[5];

typedef struct{
  PROGRAM_NAME      short_name;
  PROGRAM_NAME      full_name;
  APP_TYPE          app_type;
  int               Handle;
  DWORD             MainEntry;
  typeDebugInfoTag  DebugInfoTag;
  DWORD             DebugInfoStart;
  DWORD             DebugInfoSize;
  DWORD             N_Objects;
  OBJECT            Objects[3];
  DWORD             CodeObject;
} EXEC_INFO;        


#define EXEC_INFO_SIZE (sizeof(PROGRAM_NAME) * 2 + \
                       sizeof(APP_TYPE) + \
                       sizeof(int) + \
                       sizeof(DWORD) * 5 + \
                       sizeof(typeDebugInfoTag) + \
                       sizeof(OBJECT*))

#define OBJECT_SIZE (sizeof(ATTRIBS) + sizeof(DWORD) * 2)

/* -----------------------------------------------------*/
typedef BYTE EXCEPTION_ID;
#define excID_OutOfMemory       0   /* Доступ по адресу вне диапазона адресов */
#define excID_WriteProtected    1   /* Запись в защищенную область памяти     */
#define excID_ProgramException  2   /* Программное прерывание                 */
#define excID_UserException     3   /* Исполнение прервано пользователем      */


typedef BYTE EVENT_TYPE;
#define eventType_InternalError    0   /* Исключительная ситуация в отладчике */
#define eventType_Exception        1   /* Исключительная ситуация в программе */
#define eventType_BreakpointHit    2   /* Точка останова                      */
#define eventType_SingleStep       3   /* Исполнена одна команда              */
#define eventType_Call             4   /* Выполнена инструкция CALL           */
#define eventType_Return           5   /* Выполнена инструкция RET            */
#define eventType_MemoryAccess     6   /* Достпуп к памяти                    */
#define eventType_CompCreated      7   /* Создана новая компонента программы  */
#define eventType_CompDestroyed    8   /* Создана новая компонента программы  */
#define eventType_ThreadCreated    9   /* Создан thread                       */
#define eventType_ThreadDestroyed  10  /* Удален thread                       */


typedef BYTE ACCESS_TYPE;    
#define accesType_Nothing    0
#define accesType_Read       1
#define accesType_Write      2
#define accesType_ReadWrite  3

/* Информация о последнем произошедшем событии */
typedef union {

  struct{
    DWORD      pc;        /* Текуший адрес */
    EVENT_TYPE Event;     /* Событие       */
  } Common;

  struct{
    DWORD      pc;        /* Текуший адрес           */
    EVENT_TYPE Event;     /* Событие                 */
  } SingleStep;           /* Исполнена одна команда  */

  struct{
    DWORD      pc;           /* Текуший адрес                       */
    EVENT_TYPE Event;        /* Событие                             */
    DWORD      ErrorNo;      /* Номер ошибки                        */
    DWORD      ErrorContext; /* Дополнительные атрибуты             */
  } InternalError;           /* Исключительная ситуация в отладчике */

  struct{
    DWORD        pc;           /* Текуший адрес                       */
    EVENT_TYPE   Event;        /* Событие                             */
    EXCEPTION_ID Exception_ID;
    DWORD        XCPT_INFO_1;
    DWORD        XCPT_INFO_2;    
    DWORD        XCPT_INFO_3;    
    DWORD        XCPT_INFO_4;    
  } Exception;                 /* Исключительная ситуация в программе */

  struct{
    DWORD        pc;           /* Текуший адрес              */
    EVENT_TYPE   Event;        /* Событие                    */
    DWORD        CallAddr;     /* Адрес вызываемой команды   */
  } Call;                      /* Выполнена инструкция CALL  */

  struct{
    DWORD        pc;           /* Текуший адрес              */
    EVENT_TYPE   Event;        /* Событие                    */
    DWORD        ReturnAddr;   /* Адрес возврата             */
  } Return;                    /* Выполнена инструкция RET   */

  struct{
    DWORD        pc;           /* Текуший адрес              */
    EVENT_TYPE   Event;        /* Событие                    */
    DWORD        BreakpointInd;
  } BreakpointHit;             /* Точка останова             */

  struct{
    DWORD        pc;           /* Текуший адрес              */
    EVENT_TYPE   Event;        /* Событие                    */
    EXEC_INFO    Component;
    BYTE         Stopable;
  } CompCreated;

  struct{
    DWORD        pc;           /* Текуший адрес              */
    EVENT_TYPE   Event;        /* Событие                    */
    DWORD        Handle;
  } CompDestroyed;

  struct{
    DWORD        pc;           /* Текуший адрес              */
    EVENT_TYPE   Event;        /* Событие                    */
    DWORD tid;                 /* Task ID                    */
  } ThreadCreated;

  struct{
    DWORD        pc;           /* Текуший адрес              */
    EVENT_TYPE   Event;        /* Событие                    */
    DWORD tid;                 /* Task ID                    */
  } ThreadDestroyed;


} EVENT;

#define MAX_EVENTS 10


typedef BYTE MODE;
#define modeNone        0    /* Режимы исполнения программы             */
#define modeSingleStep  1    /* Исполнить одну команду                  */
#define modeRangeStep   2    /* Исполнять в указанном диапазоне адресов */
#define modeGo          3    /* Исполнять до возникновения события      */
  

typedef union{

  MODE mode;

  struct{
    MODE  mode;
    DWORD Begin, End; /* Диапазон адресов                        */
  } RangeStep;        /* Исполнять в указанном диапазоне адресов */

  struct{
    MODE mode;
    BYTE add_step;    /* Добавлять событие SingleStep? */
  } SingleStep;       /* Исполнить одну команду        */

} GO_MODE;

typedef char ENTRY_NAME[256];


typedef struct{
  DWORD      obj;
  DWORD      offset;
  ENTRY_NAME name;
} EXPORTED;


#endif