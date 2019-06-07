#include "rcexpr.h"
#include "rcscan.h"
#include "rcerr.h"

#define STSIZE 1000

static unsigned long vstack[STSIZE];
static int vstackpos;
static const dlmType* ostack[STSIZE];
static int uostack[STSIZE];
static int ostackpos;

static unsigned long V1,V2;
static unsigned long IVal;
static int OP;
static int IsB;
static int IsNot;

#define PUSHV(v)  if (vstackpos>=STSIZE)\
                  {\
                     error(MSG_COMPLEX_EXPR);\
                     return 0;\
                  } else\
                     vstack[vstackpos++]=v;

#define PUSHO(a)  if (ostackpos>=STSIZE)\
                  {\
                     error(MSG_COMPLEX_EXPR);\
                     return 0;\
                  } else\
                  {\
                     ostack[ostackpos]=CurDlmRec;\
                     uostack[ostackpos++]=a;\
                  }

#define POPO(o)   o=ostack[--ostackpos]->val
#define POPV(v)   v=vstack[--vstackpos]

static unsigned long DoOp(void)
{
unsigned long VT;
   if (IsB)
      switch (OP)
      {
         case D_PLUS: return V2+V1;
         case D_MINUS: return V2-V1;
         case D_MULT: return V2*V1;
         case D_DIV: return V2/V1;
         case D_REM: return V2%V1;
         case D_LSHIFT: return V2<<V1;
         case D_RSHIFT: return V2>>V1;
         case D_LT: return V2<V1;
         case D_LE: return V2<=V1;
         case D_GT: return V2>V1;
         case D_GE: return V2>=V1;
         case D_EQ: return V2==V1;
         case D_NE: return V2!=V1;
         case D_BAND: return V2&V1;
         case D_BXOR: return V2^V1;
         case D_BOR: return V2|V1;
         case D_AND: return V2&&V1;
         case D_OR: return V2||V1;
         default: error(MSG_OP);
                  return 0;
      }
   else
   {
      if (IsNot)
      {
         VT=IVal&~V1;
         IVal=0;
         return VT;
      }
      switch (OP)
      {
         case D_PLUS: return V1;
         case D_MINUS: return (unsigned long)-(long)V1;
         case D_NOT: return !V1;
         case D_BNOT: return ~V1;
         default: error(MSG_OP);
                  return 0;
      }
   }
}

static void Calc(int flag)
{
   while (ostackpos)
   {
      if (!uostack[ostackpos-1])
      {
         if (flag)
            error(MSG_EXPECTED,")");
         if (CurDlmVal==D_RPAR)
            POPO(OP);
         return;
      }
      if (flag||
          CurDlmVal==D_RPAR||
          uostack[ostackpos-1]==OP_U||
          uostack[ostackpos-1]==-1||
          uostack[ostackpos-1]==OP_B&&
            ostack[ostackpos-1]->pri>=CurDlmRec->pri
         )
      {
         if (!vstackpos)
            error(MSG_VSTACK);
         POPV(V1);
         IsB=uostack[ostackpos-1]==OP_B;
         IsNot=uostack[ostackpos-1]==-1;
         if (IsB)
         {
            if (!vstackpos)
               error(MSG_VSTACK);
            POPV(V2);
         }
         POPO(OP);
         vstack[vstackpos++]=DoOp();
      } else
         return;
   }
   if (!flag&&CurDlmVal==D_RPAR)
      error(MSG_RPAR);
}
      
unsigned long CalcExprVal(int dontget,int pmode,unsigned long ival)
{
int tofin;
int first;
unsigned long r;
   IVal=ival;
   vstackpos=0;
   ostackpos=0;
   tofin=0;
   first=1;
   for (;;)
   {
      for (;;)
      {
         if (!first||!dontget)
            GetToken(S_P_DELIMITERS|S_P_QCHAR|(pmode?S_P_EOL:0));
         first=0;
         if (CurToken==S_T_NUMBER)
         {
            r=CurValue;
            break;
         }
         if (pmode&&CurToken==S_T_KEYWORD&&CurKeyVal==K_defined)
         {
            GetToken(S_P_DELIMITERS|S_P_EOL);
            if (!(CurToken==S_T_DELIM&&CurDlmVal==D_LPAR))
            {
               error(MSG_EXPECTED,"(");
               return 0;
            }
            r=TestDefined();
            GetToken(S_P_DELIMITERS|S_P_EOL);
            if (!(CurToken==S_T_DELIM&&CurDlmVal==D_RPAR))
            {
               error(MSG_EXPECTED,")");
               return 0;
            }
            break;
         }
         if (CurToken==S_T_KEYWORD&&CurKeyVal==K_NOT)
         {
            PUSHO(-1);
            continue;
         }
         if (CurToken==S_T_DELIM)
         {
            if (CurDlmVal==D_LPAR)
            {
               PUSHO(0);
               continue;
            }
            if (CurDlmRec->type==D_TYPE_OP&&(CurDlmRec->arn&OP_U))
            {
               PUSHO(OP_U);
               continue;
            }
         } else if (pmode)
         {
            r=0;
            break;
         }
         error(MSG_NUMBER_EXPECTED);
         return 0;
      }
      PUSHV(r);
      for (;;)
      {
         GetToken(S_P_DELIMITERS|S_P_QCHAR|S_P_EOF|(pmode?S_P_EOL:0));
         if (CurToken==S_T_DELIM&&CurDlmVal==D_RPAR)
         {
            Calc(0);
            continue;
         }
         if (CurToken==S_T_DELIM&&CurDlmRec->type==D_TYPE_OP&&(CurDlmRec->arn&OP_B))
            Calc(0);
         else
            tofin=1;
         break;
      }
      if (tofin)
         break;
      PUSHO(OP_B);
   }
   Calc(1);
   return vstack[0]|IVal;
}





