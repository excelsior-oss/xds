#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "winmake.h"

#include "rcio.h"
#include "rcerr.h"

int nErr=0;

static char buf[1000];

void outerr(char head,const char *text,int code)
{
   if (IsShell())
      SendError(head,code,-1,InputGetLineNumber(),InputGetName(),(char *)text);
   else
      fprintf(stderr,"%s(%d) : %s -- %s\n",InputGetName(),InputGetLineNumber(),head==MSG_SEVERE?"Fatal error":(head==MSG_ERROR?"Error":"Warning"),buf);
}

void fatal_error(int code,...)
{
va_list v;
int err;
   ++nErr;
   err=errno;
   va_start(v,code);
   FormatMessage(FORMAT_MESSAGE_FROM_HMODULE,
                 GetModuleHandle(0),code,
                 0,buf,1000,&v);
   va_end(v);
   buf[strlen(buf)-2]=0;
   if (err>0&&err<_sys_nerr)
   {
      strcat(buf," (");
      strcat(buf,_sys_errlist[err]);
      strcat(buf,")");
   }
   outerr(MSG_SEVERE,buf,code);
   exit(2);
}

void syserror(int code,...)
{
va_list v;
int err;
static char lbuf[1000];
   ++nErr;
   err=GetLastError();;
   va_start(v,code);
   FormatMessage(FORMAT_MESSAGE_FROM_HMODULE,
                 GetModuleHandle(0),code,
                 0,buf,1000,&v);
   va_end(v);
   buf[strlen(buf)-2]=0;
   FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,
                 0,err,0,lbuf,1000,0);
   strcat(buf," (");
   strcat(buf,lbuf);
   buf[strlen(buf)-3]=0;
   strcat(buf,")");
   outerr(MSG_ERROR,buf,code);
}

void warning(int code,...)
{
va_list v;
   va_start(v,code);
   FormatMessage(FORMAT_MESSAGE_FROM_HMODULE,
                 GetModuleHandle(0),code,
                 0,buf,1000,&v);
   va_end(v);
   buf[strlen(buf)-2]=0;
   outerr(MSG_WARNING,buf,code);
}

void error(int code,...)
{
va_list v;
   ++nErr;
   va_start(v,code);
   FormatMessage(FORMAT_MESSAGE_FROM_HMODULE,
                 GetModuleHandle(0),code,
                 0,buf,1000,&v);
   va_end(v);
   buf[strlen(buf)-2]=0;
   outerr(MSG_ERROR,buf,code);
}

void fatal(int code,...)
{
va_list v;
   ++nErr;
   va_start(v,code);
   FormatMessage(FORMAT_MESSAGE_FROM_HMODULE,
                 GetModuleHandle(0),code,
                 0,buf,1000,&v);
   va_end(v);
   buf[strlen(buf)-2]=0;
   outerr(MSG_SEVERE,buf,code);
   exit(2);
}
