#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "rcsup.h"
#include "rcerr.h"
#include "rcstb.h"

#define HEADSIZE 65536UL

typedef struct _TableEntry
   {
      struct _TableEntry *next;
      struct _TableEntry *prev;
      const char *str;
      short flags;
      stbVal *val;
   } TableEntry;
      
static TableEntry * Table[HEADSIZE]={0};

#ifdef _DEBUG

void StbDump(const char *name)
{
int i;
FILE *f;
TableEntry *p;
   f=fopen(name,"w");
   if (!f)
      return;
   printf("Dumping symbols");
   for (i=0;i<HEADSIZE;++i)
   {
      for (p=Table[i];p;p=p->next)
         if (!(p->flags&STB_FL_PERMANENT)&&
             p->val->type==STB_RAW)
            fprintf(f,"%s=%s\n",p->str,p->val->val.text);
   }
   fclose(f);
}

#endif

void StbRead(const char *name)
{
FILE *f;
static char str[10000];
stbVal *stb;
char *p;
   f=fopen(name,"r");
   if (!f)
      fatal_error(MSG_OPEN,name);
   while (fgets(str,10000,f))
   {
      str[strlen(str)-1]=0;
      p=strchr(str,'=');
      if (!p)
         continue;
      *p++=0;
      stb=StbLookup(str,STB_FL_INSERT);
      stb->type=STB_RAW;
      stb->val.text=Malloc(strlen(p)+1);
      strcpy((char *)stb->val.text,p);
   }
   if (ferror(f))
      fatal_error(MSG_READ,name);
   fclose(f);
}


static int CalcInd(const char *str)
{
unsigned short hsum;
unsigned short tmp;
const char *p;
   hsum=0;
   for (p=str;*p;++p)
   {
      tmp=(hsum>>13)&7;
      hsum=(hsum<<3)+tmp+(unsigned char)*p;
   }
   return hsum%HEADSIZE;
}

stbVal *StbLookupEx(const char *str,int flags,int *rflags)
{
int index;
TableEntry *p;
   index=CalcInd(str);
   for (p=Table[index];p;p=p->next)
      if (!strcmp(p->str,str))
         break;
   if (p)
   {
      if (rflags)
         *rflags=p->flags;
      return p->val;
   }
   if (!(flags&STB_FL_INSERT))
      return 0;
   p=Malloc(sizeof(TableEntry));
   p->str=Malloc(strlen(str)+1);
   strcpy((char *)p->str,str);
   p->flags=(short)(flags&0x0ffff);
   p->val=Malloc(sizeof(stbVal));
   p->val->type=STB_NONE;
   p->next=Table[index];
   p->prev=(TableEntry *)(Table+index);
   Table[index]=p;
   if (p->next)
      p->next->prev=p;
   if (rflags)
      *rflags=p->flags;
   return p->val;
}

int StbDelete(const char *str)
{
int index;
TableEntry *p;
   index=CalcInd(str);
   for (p=Table[index];p;p=p->next)
      if (!strcmp(p->str,str))
         break;
   if (p)
   {
      if (p->flags&STB_FL_PERMANENT)
         return -1;
      Free(p->val);
      Free((void *)p->str);
      p->prev->next=p->next;
      if (p->next)
         p->next->prev=p->prev;
      Free(p);
      return 0;
   }
   return -1;
}


