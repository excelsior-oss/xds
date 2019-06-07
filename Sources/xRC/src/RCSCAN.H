#include <windows.h>
#include <setjmp.h>

#define S_P_QUOTES      1
#define S_P_ESCAPES     2
#define S_P_DELIMITERS  4 
#define S_P_EOF         8
#define S_P_EOL         16
#define S_P_DONTEXPAND  32
#define S_P_QCHAR       64
#define S_P_LQUOTES     128

void ScanInit(void);
void GetToken(int flags);
int TestDefined(void);
void DefSym(const char *name,int perm);

typedef enum {
               S_T_EOF,
               S_T_EOL,
               S_T_RAW,
               S_T_NUMBER,
               S_T_STRING,
               S_T_KEYWORD,
               S_T_DELIM,
               S_T_ID,
               S_T_QCHAR
             } S_TOKEN;
   
#define KEY_TYPE_RESOURCE     1
#define KEY_TYPE_CONTROL      2
#define KEY_TYPE_STATEMENT    4
#define KEY_TYPE_ATTRIBUTE    8

#define D_TYPE_DLM            1
#define D_TYPE_OP             2

#define OP_U                  1
#define OP_B                  2
#define OP_UB                 3

#define K_INC
enum
{
   #include "keys.h"
};

enum
{
   #include "dlms.h"
};
#undef K_INC

typedef struct
   {
      const char *str;
      int type;
      int val;
      DWORD style;
      int sclass;
   } keyType;

typedef struct
   {
      const char *str;
      int type;
      int val;
      int pri;
      int arn;
   } dlmType;
      
extern jmp_buf eof_jmp;
extern S_TOKEN CurToken;
extern long CurValue;
extern int CurStrLen;
extern const char *CurStrValue;
extern int CurStrLen16;
extern const wchar_t *CurStrValue16;
extern const keyType *CurKeyRec;
extern int CurKeyType;
extern int CurKeyVal;
extern const dlmType *CurDlmRec;
extern int CurDlmVal;
extern const char *TokenB;
extern const char *TokenE;
extern int LFlag;
