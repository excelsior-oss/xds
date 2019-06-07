#include <windows.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "rcerr.h"
#include "rcio.h"
#include "rcsup.h"
#include "rcstb.h"
#include "rcexpr.h"
#include "rcscan.h"
#include "rcparse.h"

#define IsSp(c) isspace(c)

jmp_buf eof_jmp;

S_TOKEN CurToken;
long CurValue;
int CurStrLen;
int CurStrLen16;
const char *CurStrValue;
const wchar_t *CurStrValue16;
const char *TokenB;
const char *TokenE;
const keyType* CurKeyRec;
int CurKeyType;
int CurKeyVal;
const dlmType *CurDlmRec;
int CurDlmVal;
int LFlag;

#define EXPSTSIZE 1000
#define IFSTSIZE 1000
#define IBUFSIZE 32768

static char iline[IBUFSIZE];
static char *ibuf=iline;
static int ipos=0;
static int ilen=0;

static char    kbuf[IBUFSIZE];
static wchar_t kbuf16[IBUFSIZE];

static keyType Keys[]=
   {
#include "keys.h"
   };

static dlmType Dlms[]=
   {
#include "dlms.h"
   };

static int ifpos;
static int if_true,if_else,if_wastrue;
static int iftstack[IFSTSIZE];
static int ifestack[IFSTSIZE];
static int ifwstack[IFSTSIZE];

static int exppos;
static char *expbstack[EXPSTSIZE];
static int exppstack[EXPSTSIZE];
static int wpar=0;

void DefSym(const char *name,int perm)
{
stbVal *stb;
stb=StbLookup(name,STB_FL_INSERT|(perm?STB_FL_PERMANENT:0));
   stb->type=STB_RAW;
   stb->val.text=Malloc(2);
   *(char *)stb->val.text='1';
   *((char *)stb->val.text+1)=0;
}

void ScanInit(void)
{
int i;
stbVal *stb;
   for (i=0;i<sizeof(Keys)/sizeof(keyType);++i)
   {
      stb=StbLookup(Keys[i].str,STB_FL_INSERT|STB_FL_PERMANENT);
      stb->type=STB_KEY;
      stb->val.num=i;
   }
   DefSym("RC_INVOKED",1);
   DefSym("XRC_INVOKED",1);
   DefSym("_WIN32",1);
   ifpos=0;
   if_true=1;
   if_else=0;
   exppos=0;
}

static int CheckDlm(const char *p)
{
int i;
const char *q;
const char *r;
   for (i=0;i<sizeof(Dlms)/sizeof(dlmType);++i)
   {
      q=p;
      r=Dlms[i].str;
      for (;*r;++r,++q)
         if (*r!=*q)
            break;
      if (!*r)
         return i;
   }
   return -1;
}

static void DoUndef(void)
{
int rfl;
stbVal *stb;
   GetToken(S_P_DELIMITERS|S_P_EOL|S_P_DONTEXPAND);
   if (CurToken!=S_T_ID)
   {
      error(MSG_ID);
      return;
   }
   stb=StbLookupEx(kbuf,0,&rfl);
   if (!stb)
   {
//      warning(MSG_UNDEF);
      return;
   }
   if (rfl&STB_FL_PERMANENT)
   {  
      warning(MSG_UNDEF_PERM);
      return;
   }
   if (stb->type==STB_RAW||stb->type==STB_STR)
   {
      Free((void *)stb->val.text);
      stb->val.text=0;
   }
   StbDelete(kbuf);
}

static void DoInc(void)
{
int sys;
char *p;
   while (IsSp(ibuf[ipos]))
      ++ipos;
   sys=ibuf[ipos]=='<';
   if (!sys&&ibuf[ipos]!='"')
   {
      error(MSG_INCLUDE_DELIM);
      return;
   }
   ++ipos;
   p=ibuf+ipos;
   while (*p&&*p!='"'&&*p!='>')
      ++p;
   if (*p!=(sys?'>':'"'))
   {
      error(MSG_INCLUDE_DELIM);
      return;
   }
   memcpy(kbuf,ibuf+ipos,p-(ibuf+ipos));
   kbuf[p-(ibuf+ipos)]=0;
   InputInclude(kbuf,sys);
}


static void DoDefine(void)
{
int rfl;
stbVal *stb;
char *p,*st;
int len;
int stsize;
int wasbs;

   GetToken(S_P_DELIMITERS|S_P_EOL|S_P_DONTEXPAND);
   if (CurToken!=S_T_ID)
   {
      error(MSG_ID);
      return;
   }
   if (*TokenE=='(')
   {
      if (!wpar)
         warning(MSG_MACRO_PAR);
      wpar=1;
      return;
   }
   if (*TokenE&&!IsSp(*TokenE))
   {
      error(MSG_ID);
      return;
   }
   while (IsSp(ibuf[ipos]))
      ++ipos;
   st=Malloc(1);
   *st=0;
   stsize=1;
   for (;;)
   {
      p=ibuf+strlen(ibuf)-1;
      while (IsSp(*p))
         --p;
      wasbs=*p=='\\';
      if (!wasbs)
         ++p;
      len=p-(ibuf+ipos);
      if (len>00)
      {
         st=Realloc(st,stsize+len);
         memcpy(st+stsize-1,ibuf+ipos,len);
         stsize+=len;
         st[stsize-1]=0;
      }
      if (!wasbs)
         break;
      ipos=0;
      ilen=InputRead(iline,IBUFSIZE);
      if (ilen<0)
         longjmp(eof_jmp,1);
   }
   ipos=ilen;
   stb=StbLookupEx(kbuf,STB_FL_INSERT,&rfl);
   if (stb->type!=STB_NONE)
   {
      if (stb->type==STB_RAW&&
          !strcpy((char *)stb->val.text,st))
      {
         Free(st);
         return;
      }
      if (rfl&STB_FL_PERMANENT)
      {
         warning(MSG_REDEF_PERM);
         Free(st);
         return;
      }
      warning(MSG_REDEF);
      if (stb->type==STB_RAW||stb->type==STB_STR)
         Free((void *)stb->val.text);
   }
   stb->type=STB_RAW;
   stb->val.text=st;
}

int TestDefined(void)
{
stbVal *stb;
   GetToken(S_P_DELIMITERS|S_P_EOL|S_P_DONTEXPAND);
   if (CurToken!=S_T_ID)
   {
      error(MSG_ID);
      return 0;
   }
   stb=StbLookup(kbuf,0);
   return stb&&stb->type!=STB_KEY;
}

static void DoPre(void)
{
char *pb;
stbVal *stb;
   ++ipos;
   while (IsSp(ibuf[ipos]))
      ++ipos;
   if (!ibuf[ipos])
      return;
   pb=ibuf+ipos;
   while (isalnum(ibuf[ipos])||ibuf[ipos]=='_')
      ++ipos;
   kbuf[0]='#';
   memcpy(kbuf+1,pb,ibuf+ipos-pb);
   kbuf[ibuf+ipos-pb+1]=0;
   stb=StbLookup(kbuf,0);
   if (!stb)
   {
      if (if_true)
         warning(MSG_UNKNOWN_PRE);
      return;
   }
   switch (Keys[stb->val.num].val)
   {
      case P_ifdef:  if (ifpos>=IFSTSIZE)
                     {
                        error(MSG_IF);
                        break;
                     }
                     iftstack[ifpos]=if_true;
                     ifwstack[ifpos]=if_wastrue;
                     ifestack[ifpos++]=if_else;
                     if_else=0;
                     if (if_true)
                        if_true=TestDefined();
                     if_wastrue=if_true;
                     break;
      case P_ifndef: if (ifpos>=IFSTSIZE)
                     {
                        error(MSG_IF);
                        break;
                     }
                     iftstack[ifpos]=if_true;
                     ifwstack[ifpos]=if_wastrue;
                     ifestack[ifpos++]=if_else;
                     if_else=0;
                     if (if_true)
                        if_true=!TestDefined();
                     if_wastrue=if_true;
                     break;
      case P_if:  if (ifpos>=IFSTSIZE)
                  {
                     error(MSG_IF);
                     break;
                  }
                  iftstack[ifpos]=if_true;
                  ifwstack[ifpos]=if_wastrue;
                  ifestack[ifpos++]=if_else;
                  if_else=0;
                  if (if_true)
                     if_true=CalcExprEx(0,1)!=0;
                  if_wastrue=if_true;
                  break;
      case P_elif:if (!ifpos||if_else)
                  {
                     error(MSG_ELIF);
                     break;
                  }
                  if (if_true)
                     if_true=0;
                  else
                  {
                     if_true=1;
                     if_true=iftstack[ifpos-1]&&CalcExprEx(0,1);
                  }
                  if_wastrue=if_wastrue||if_true;
                  break;
      case P_else:if (!ifpos||if_else)
                  {
                     error(MSG_ELSE);
                     break;
                  }
                  if_else=1;
                  if (if_wastrue)
                     if_true=0;
                  else
                     if_true=iftstack[ifpos-1];
                  break;
      case P_endif:  if (!ifpos)
                     {
                        error(MSG_ENDIF);
                        break;
                     }
                     if_true=iftstack[ifpos-1];
                     if_wastrue=ifwstack[ifpos-1];
                     if_else=ifestack[--ifpos];
                     break;
      case P_define: if (if_true) 
                        DoDefine();
                     break;
      case P_undef:  if (if_true)
                        DoUndef();
                     break;
      case P_include:if (if_true)
                        DoInc();
                     break;
      default: error(MSG_ILL_PRE);
               break;
   }
   return;
}


static wchar_t try_hex4(char * pch) 
{
int i;
char c;
wchar_t res = 0;
    for (i=0; i<4; ++i) {
        c = pch[i];
        res <<= 4;
        if      (c>='0' && c<='9') res += (c-'0');
        else if (c>='a' && c<='f') res += (c-'a'+10);
        else return 0;
    }
    return res;
}

void GetToken(int flags)
{
char c;
stbVal *stb;
const char *p;
char *ep;
int dnum;
int ctlch;
static int bline=0;
char *pk;
wchar_t *pk16;
int spos;
wchar_t wch;
char ch_from_w[10];
int len;
int f;

   for (;;)
   {
      for (;;)
      {
         if (ipos>=ilen)
         {
            if (exppos)
            {
               ibuf=expbstack[exppos-1];
               ipos=exppstack[--exppos];
               ilen=strlen(ibuf);
               continue;
            }
            if (flags&S_P_EOL)
            {
               CurToken=S_T_EOL;
               return;
            }
            ipos=0;
            ilen=InputRead(iline,IBUFSIZE);
            bline=1;
            if (ilen<0)
            {
               if (ifpos)
                  error(MSG_OPEN_IF);
               if (flags&S_P_EOF)
               {
                  CurToken=S_T_EOF;
                  return;
               } else
                  longjmp(eof_jmp,1);
            }
         }
         do
            c=ibuf[ipos++];
         while (IsSp(c));
         if (c)
            --ipos;
         else
            continue;
         if (ibuf[ipos]==';'||ibuf[ipos]=='/'&&ibuf[ipos+1]=='/')
         {
            ipos=ilen;
            continue;
         }
         if (ibuf[ipos]=='/'&&ibuf[ipos+1]=='*')
         {
            ipos+=2;
            for (;;)
            {
               p=strstr(ibuf+ipos,"*/");
               if (p)
                  ipos=p-ibuf+2;
               else
               {
                  if (exppos)
                  {
                     ibuf=expbstack[exppos-1];
                     ipos=exppstack[--exppos];
                     ilen=strlen(ibuf);
                     continue;
                  }
                  ipos=0;
                  ilen=InputRead(iline,IBUFSIZE);
                  bline=1;
                  if (ilen<0)
                  {
                     error(MSG_OPEN_COMMENT);
                     error(MSG_OPEN_IF);
                     if (flags&S_P_EOF)
                     {
                        CurToken=S_T_EOF;
                        return;
                     } else
                        longjmp(eof_jmp,1);
                  }
                  continue;
               }
               break;
            }
            continue;
         }
         if (bline&&ibuf[ipos]=='#')
         {
            DoPre();
            ipos=ilen;
            continue;
         }
         if (if_true&&((flags&S_P_EOL)||!InputIsH()))
            break;
         else
            ipos=ilen;
      }
      bline=0;
      CurToken=S_T_RAW;
      TokenE=TokenB=ibuf+ipos;
      if ((flags&(S_P_LQUOTES|S_P_QUOTES|S_P_QCHAR))&&c=='"'||
          (flags&S_P_LQUOTES)&&c=='L'&&ibuf[ipos+1]=='"')
      {
         spos=ipos;
         LFlag=c=='L';
         if (LFlag)
            ++ipos;
         ++ipos;
         if (flags&S_P_QCHAR)
         {
            ctlch=ibuf[ipos]=='^';
            if (ctlch)
               ++ipos;
            if (ibuf[ipos]=='"'||ibuf[ipos+1]!='"')
            {
               ipos=spos;
               GetToken(flags&~S_P_QCHAR);
               return;
            }
            CurValue=(unsigned char)ibuf[ipos];
            ipos+=2;
            if (ctlch)
               CurValue&=0x1f;
            CurToken=S_T_QCHAR;
         } else
         {
            pk=kbuf;
            pk16=kbuf16;
            for (;;)
            {
               f = 1;
               c = ibuf[ipos++];
               if (!c||c=='"')
                  break;
               if ((flags&S_P_ESCAPES)&&c=='\\')
               {
                  c=ibuf[ipos++];
                  switch (c)
                  {
                     case 'a' : 
                     case 'b' : c=0x8;  break;
                     case 'f' : c=0xc;  break;
                     case 'n' : c=0xa;  break;
                     case 'r' : c=0xd;  break;
                     case 't' : c=0x9;  break;
                     case 'v' : c=0xb;  break;
                     case '"' : c='"';  break; //We don't understand '""' but we understand '\"'
                     case '\\': c='\\'; break;
                     case '0' : 
                         c=(char)strtoul(ibuf+ipos-1,&ep,0);
                               ipos=ep-ibuf;
                         break;
                     case 'x': 
                         wch = try_hex4(ibuf+ipos);
                         if (wch) {
                            ipos += 4;
                            len = WideCharToMultiByte(ResCP, 0, &wch, 1, 
                                     ch_from_w, sizeof(ch_from_w), NULL, NULL);
                            strncpy(pk, ch_from_w, len);
                            pk += len;
                            *pk16++ = wch;
                            f = 0;
                         } else {
                            *pk++='\\';
                            *pk16++ = L'\\';
                            c = 'x';
                         }
                         break;
                     default: 
                         ipos--; 
                         c = '\\'; // note: can't concatinate strings at '\'+EOL
                  }
                  }
               if (f) {
                   *pk++   = c;
                   *pk16++ = (wchar_t)c; // note: cat't understand multibyte with modifyers
               }
            }
            CurToken=S_T_STRING;
            CurStrLen=pk-kbuf;
            *pk++=0;
            CurStrValue=kbuf;
            CurStrLen16=pk16-kbuf16;
            *pk16++=0;
            CurStrValue16=kbuf16;

            if (!c)
               error(MSG_STRING);
         }
         TokenE=ibuf+ipos;
         return;
      } else
      {
         if ((flags&S_P_DELIMITERS)&&(dnum=CheckDlm(TokenB))>=0)
         {
            CurToken=S_T_DELIM;
            CurDlmRec=Dlms+dnum;
            CurDlmVal=Dlms[dnum].val;
            ipos+=strlen(Dlms[dnum].str);
            TokenE=ibuf+ipos;
            return;
         }
         do
            c=ibuf[ipos++];
         while (c&&!IsSp(c)&&(!((flags&S_P_DELIMITERS)&&(dnum=CheckDlm(ibuf+ipos-1))>=0)));
         --ipos;
         TokenE=ibuf+ipos;
         c=*TokenB;
         if (isdigit(c))
         {
            CurValue=(long)strtoul(TokenB,&ep,0);
            if (ep==TokenE||ep==TokenE-1&&*ep=='L')
            {
               CurToken=S_T_NUMBER;
               LFlag=ep!=TokenE;
               return;
            }
         } else if (isalpha(c)||c=='_')
         {
            for (p=TokenB+1;p<TokenE;++p)
               if (!(isalnum(*p)||*p=='_'))
                  break;
            if (p==TokenE)
            {
               memcpy(kbuf,TokenB,TokenE-TokenB);
               kbuf[TokenE-TokenB]=0;
               if (flags&S_P_DONTEXPAND)
               {
                  CurToken=S_T_ID;
                  return;
               }
               stb=StbLookup(kbuf,0);
               if (stb)
                  switch (stb->type)
                  {
                     case STB_RAW: if (exppos>=EXPSTSIZE)
                                   {
                                      error(MSG_NESTED_EXP);
                                      break;
                                   }
                                   expbstack[exppos]=ibuf;
                                   exppstack[exppos++]=ipos;
                                   ibuf=(char *)stb->val.text;
                                   ipos=0;
                                   ilen=strlen(ibuf);
                                   continue;
                     case STB_NUM: CurValue=stb->val.num;
                                   CurToken=S_T_NUMBER;
                                   break;
                     case STB_KEY: CurKeyRec=Keys+stb->val.num;
                                   CurToken=S_T_KEYWORD;
                                   break;
                  }
               else
               {
                  _strupr(kbuf);
                  stb=StbLookup(kbuf,0);
                  if (stb&&stb->type==STB_KEY)
                  {
                     CurKeyRec=Keys+stb->val.num;
                     CurToken=S_T_KEYWORD;
                  }
               }
               if (CurToken==S_T_KEYWORD)
               {
                  CurKeyType=CurKeyRec->type;
                  CurKeyVal=CurKeyRec->val;
               }
            }
         }
      }
      return;
   }
}

