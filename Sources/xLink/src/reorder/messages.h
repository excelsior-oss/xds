
#include <stdarg.h>

#define xMESSAGE 0
#define xFATAL   1
#define xERROR   2
#define xWARNING 3

#define  msgUNABLE_TO_OPEN_FILE         0
#define  msgUNABLE_TO_WRITE_FILE        1
#define  msgUNABLE_TO_READ_FILE         2
#define  msgINSUFFICIENT_MEMORY         3
#define  msgEMPTY_FILE                  4
#define  msgFILE_TOO_LONG               5
#define  msgSTR_NOT_CLOSED              6
#define  msgFILE__EXPECTED              7
#define  msgCOMPONENT_RELOCATION        8
#define  msgCOMPONENT_NOT_FOUND         9
#define  msgUNKNOWN_DIRECTIVE           10
#define  msgNUMBER_OF_PAGEFAULTS        11

#define  MESSAGE_TYPES                  12

extern int       TotalErrors;
extern int       TotalWarnings;

extern void Message(int code, int number,...);
