#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "rcerr.h"
#include "rcio.h"
#include "rcsup.h"
#include "rcerr.h"
#include "rcstb.h"
#include "rcscan.h"
#include "rcparse.h"

static char oname[1000];

#define ARG(p,n)  if (argv[i][n])\
                     p=argv[i]+n;\
                  else\
                  {\
                     ++i;\
                     if (i>=argc)\
                        fatal(MSG_CMDLINE);\
                     p=argv[i];\
                  }


static void Usage(void)
{
DWORD size,dummy;
char *vbuf;
int vfl;
VS_FIXEDFILEINFO *vf;
   GetModuleFileName(0,oname,1000);
   size=GetFileVersionInfoSize(oname,&dummy);
   vfl=0;
   if (size)
   {
      vbuf=Malloc(size);
      if (GetFileVersionInfo(oname,dummy,size,vbuf))
         if (VerQueryValue(vbuf,"\\",&vf,&dummy))
            vfl=1;
   }
   printf("Resource compiler ");
   if (vfl)
   {
      printf("v%d.%d%c ",HIWORD(vf->dwFileVersionMS),LOWORD(vf->dwFileVersionMS),
         HIWORD(vf->dwFileVersionLS)?HIWORD(vf->dwFileVersionLS)-1+'a':' ');
      if (vf->dwFileFlags&VS_FF_PRERELEASE)
         printf("(prelease) ");
   }
   printf("Excelsior LLC, 1996,2006\n");
   printf(

      "Usage:  xrc [options] <.RC input file>\n"
      "Switches:\n"
      "   /d    Define a symbol\n"
      "   /fo   Rename .RES file\n"
      "   /l    Default language ID in hex\n"
      "   /i    Add a path for INCLUDE searches\n"
      "   /x    Ignore INCLUDE environment variable\n"
      "   /c    Define a code page used by NLS conversion\n"
      "   /stb  Predefine all Windows symbols\n"
      );
   if (size)
      Free(vbuf);
}

int main(int argc,char **argv)
{
char *p;
char *onamep;
int x_flag=0,c_flag=0,l_flag=0;
int li,cp;
int i;
static char nbuf[1000];
int IsSh;
#ifdef _DEBUG
char *dsname=0;
#endif

   onamep=0;
   IsSh=0;
   for (i=1;i<argc;++i)
   {
      if (argv[i][0]!='/'&&argv[i][0]!='-')
         break;
      if (!strcmp(argv[i]+1,"__XDS_SHELL__"))
      {
         IsSh=1;
         continue;
      }
      if (!strncmp(argv[i]+1,"d",1))
      {
         ARG(p,2);
         DefSym(p,0);
         continue;
      }
      if (!strncmp(argv[i]+1,"fo",2))
      {
         ARG(p,3);
         onamep=p;
         continue;
      }
      if (!strncmp(argv[i]+1,"l",1))
      {
         ARG(p,2);
         l_flag=1;
         li=strtoul(p,NULL,16);
         continue;
      }
      if (!strncmp(argv[i]+1,"i",1))
      {
         ARG(p,2);
         ParsePath(p);
         continue;
      }
      if (!strcmp(argv[i]+1,"x"))
      {
         x_flag=1;
         continue;
      }
      if (!strncmp(argv[i]+1,"c",1))
      {
         ARG(p,2);
         c_flag=1;
         cp=atoi(p);
         continue;
      }
#ifdef _DEBUG
      if (!strncmp(argv[i]+1,"wstb",4))
      {
         ARG(p,5);
         dsname=p;
         continue;
      }
#endif          
      if (!strncmp(argv[i]+1,"stb",4))
      {
         GetModuleFileName(0,nbuf,1000);
         p=nbuf+strlen(nbuf)-1;
         while (p>=nbuf&&*p!=':'&&*p!='\\')
            --p;
         ++p;
         strcpy(p,"windows.stb");
         StbRead(nbuf);
         continue;
      }
      if (!strcmp(argv[i]+1,"?")||
          !strcmp(argv[i]+1,"?"))
      {
         Usage();
         exit(0);
      }
      fatal(MSG_CMDLINE);
   }

   if (i!=argc-1)
   {
      Usage();
      exit(0);
   }
   
   if (!x_flag)
   {
      p=getenv("INCLUDE");
      if (p)
         ParsePath(p);
   }
   if (l_flag)
      SetLI(li);
   else
      SetLI(MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL));
   if (c_flag)
   {
      if (!IsValidCodePage(cp))
         fatal(MSG_CPAGE);
      SetCP(cp);
   }
   if (!onamep)
   {
      strcpy(oname,argv[i]);
      p=oname+strlen(oname)-1;
      while (p>oname&&*p!='\\'&&*p!='.')
         --p;
      if (*p=='.')
         *p=0;
      onamep=oname;
   }
   FileIni(argv[i],onamep,IsSh);
   DoRC();
   if (nErr==0)
   {
      OutputClose();
#ifdef _DEBUG
      if (dsname)
         StbDump(dsname);
#endif
   }else
      OutputDelete();
   return nErr!=0;
}
