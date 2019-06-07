#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "winmake.h"

#include "rcio.h"
#include "rcerr.h"
#include "rcsup.h"

#define FILELEN 1024
#define INCSTSIZE 100
#define INCPSIZE 100

static FILE *fi;
static FILE *fo;
static FILE *fa;
static char goname[FILELEN];
static char giname[FILELEN];
static char ganame[FILELEN];

static int nstr;

static int incpos;
static FILE *incfstack[INCSTSIZE];
static char incnstack[INCSTSIZE][FILELEN];
static int inclstack[INCSTSIZE];

static int incptr=1;
static char *incpath[INCPSIZE]={""};
static int IsShellf;


int IsShell()
{
   return IsShellf;
}

int InputIsH(void)
{
int len;
   len=strlen(giname);
   return len>=2&&giname[len-2]=='.'&&giname[len-1]=='h';
}

void ParsePath(const char *paths)
{
const char *p,*q;
char *r;
int len,nsl;
   p=paths;
   do
   {
      q=strchr(p,';');
      len=q?q-p:strlen(p);
      nsl=p[len-1]!='\\';
      r=Malloc(len+1+nsl);
      memcpy(r,p,len);
      if (nsl)
         r[len]='\\';
      r[len+nsl]=0;
      incpath[incptr++]=r;
      if (q)
         p=q+1;
      else
         p=0;
   } while (p);
}

char *InputGetName(void)
{
   return giname;
}

int InputGetLineNumber(void)
{
   return nstr;
}


static void FullName(const char *pname,char *fname,const char *ext)
{
char *p;
char c;
   strcpy(fname,pname);
   p=fname+strlen(fname);
   while (p!=fname)
   {
      c=*--p;
      if (c=='.')
         return;
      if (c=='\\'||c==':')
         break;
   }
   strcat(fname,".");
   strcat(fname,ext);
}

void FileIni(const char *iname,const char *oname,int progress)
{
static char bf[1000];
   IsShellf=progress&&(ConnectShell()==1);
   incpos=0;
   nstr=0;
   FullName(iname,giname,"rc");
   fi=fopen(giname,"r");
   if (!fi)
      fatal_error(MSG_OPEN,giname);
   FullName(oname,goname,"res");
   fo=fopen(goname,"wb");
   if (!fo)
      fatal_error(MSG_OPEN,goname);
   strcpy(bf,"Compiling resources: ");
   strcat(bf,giname);
   if (IsShellf)
      SendComment(bf);
}

void OutputWrite(const void *buf,int len)
{
   if (nErr)
      return;
   if (fwrite(buf,len,1,fo)!=1)
      fatal_error(MSG_WRITE,goname);
}

int InputRead(char *buf,int len)
{
char *p;
int l;
   buf[len-1] = 0;
   for (;;)
   {
      p=fgets(buf,len-1,fi);
      ++nstr;
      if (!p)
      {
         if (feof(fi))
         {
            if (!incpos)
               return -1;
            fclose(fi);
            strcpy(giname,incnstack[incpos-1]);
            nstr=inclstack[incpos-1];
            fi=incfstack[--incpos];
            continue;
         }
         fatal_error(MSG_READ,giname);
      }
      break;
   }
   if ((l=strlen(buf))==len-1)
      warning(MSG_LINE_TOO_LONG);
   return l;
}

static int ExpandIncludeName(const char *src,char *dst,int sys)
{
int i;
struct _stat stbuf;
char *p;
   if (src[0]=='\\'||src[1]==':')
   {
      strcpy(dst,src);
      for (p=dst;*p;++p)
         if (*p=='/')
            *p='\\';
      if (_stat(dst,&stbuf)<0)
         return -1;
      return 0;
   }
   for (i=sys!=0;i<incptr;++i)
   {
      strcpy(dst,incpath[i]);
      strcat(dst,src);
      for (p=dst;*p;++p)
         if (*p=='/')
            *p='\\';
      if (_stat(dst,&stbuf)>=0)
         return 0;
   }
   return -1;
}

void InputInclude(const char *name,int sys)
{
char liname[FILELEN];
FILE *lf;
   if (incpos>=INCSTSIZE)
   {
      error(MSG_INCDEPTH);
      return;
   }
   if (ExpandIncludeName(name,liname,sys)<0)
   {
      error(MSG_FIND,name);
      return;
   }
   lf=fopen(liname,"r");
   if (!lf)
   {
      error(MSG_OPEN,liname);
      return;
   }
   inclstack[incpos]=nstr;
   incfstack[incpos]=fi;
   strcpy(incnstack[incpos],giname);
   ++incpos;
   nstr=0;
   fi=lf;
   strcpy(giname,liname);
   return;
}

void OutputClose(void)
{
   fclose(fo);
}

void OutputDelete(void)
{
   fclose(fo);
   remove(goname);
}

long OutputGetPos(void)
{
long pos;
   pos=ftell(fo);
   if (pos<0)
      fatal_error(MSG_FTELL,goname);
   return pos;
}

void OutputSetPos(long pos)
{
   if (fseek(fo,pos,SEEK_SET))
      fatal_error(MSG_FSEEK,goname);
}

void AuxOpen(const char *name)
{
   strcpy(ganame,name);
   fa=fopen(ganame,"rb");
   if (!fa)
      fatal_error(MSG_OPEN,ganame);
}

void AuxSetPos(long pos)
{
   if (fseek(fa,pos,SEEK_SET))
      fatal_error(MSG_FSEEK,goname);
}

int AuxRead(char *buf,int len)
{
int r;
   r=fread(buf,1,len,fa);
   if (r<len&&ferror(fa))
      fatal_error(MSG_READ,ganame);
   return r;
}

void AuxClose(void)
{
   fclose(fa);
}