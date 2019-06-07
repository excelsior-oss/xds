// system.cpp - system-dependent routines

#if defined(_WIN32) || defined (__NT__)
// MSVC or Watcom on NT?
#include <windows.h>
#define CCHMAXPATH 260
#elif defined __linux__
#include <errno.h>
#include <unistd.h>
#else
// no, assume IBM C Set on OS/2
#define INCL_DOSPROCESS
#define INCL_DOSMODULEMGR
#include <os2.h>
#endif

unsigned int LocateSelf(char buf[], int len) {
#if defined(_WIN32) || defined (__NT__)
    if (GetModuleFileName(GetModuleHandle(0),buf,len) == 0)
		return GetLastError();
	else 
		return 0;
#elif defined __linux__
    if(readlink("/proc/self/exe", buf, len) <= 0)
        return errno;
    else
        return 0;
#else
    PTIB ptib = NULL;
    PPIB ppib = NULL;
    DosGetInfoBlocks(&ptib,&ppib);
    return DosQueryModuleName(ppib->pib_hmte,len,buf);
#endif
}

