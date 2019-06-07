#include <windows.h>
#include <stdio.h>
#include <string.h>

#ifndef JETVERSION
#error  JETVERSION not defined!
#endif

#define MAX_CMDLINE 16384

int extractCmdLine (char *cmdLine) {
    char *winCmdLine = GetCommandLine();
    char  myNameBuf [MAX_PATH];
    char  cmdBuf [MAX_CMDLINE];
    char *p;
    if (*winCmdLine == '\"') {
        strcpy (cmdBuf, winCmdLine + 1);
        p = strchr (cmdBuf, '\"');
        if (p != NULL) {
            p++;
            strcpy (cmdLine, p);
            return 0;
        } else
            return 1;
    } else {
        strcpy (cmdBuf, winCmdLine);
        p = cmdBuf;
    }
    strcpy (cmdLine, "");
    while (1) {
        HANDLE myHandle = NULL;
        while ((*p != '\0') && (*p != ' '))
            p ++;
        if (*p == '\0')
            break;
        *p = '\0';
        strcpy (myNameBuf, cmdBuf);
        myHandle = GetModuleHandle (myNameBuf);
        if (myHandle == NULL) {
            strcat (myNameBuf, ".exe");
            myHandle = GetModuleHandle (myNameBuf);
        }
        if (myHandle != NULL) {
            if (myHandle != GetModuleHandle (NULL)) {
                return 1;
            }
            p++;
            strcpy (cmdLine, p);
            return 0;
        }
        *p = ' ';
        p++;
    }
    return 0;
}

/////////////////////////////////////////////////////////////////////////////

int (* InvokeXLink) (char *cmdLine, int isSilent);

int main (void) {
  char XLinkDLLName [MAX_PATH];
  HANDLE XLinkLibrary;
  char cmdLineBuf [16384];

  sprintf (XLinkDLLName, "xlink%d.dll", JETVERSION);

  XLinkLibrary = LoadLibrary (XLinkDLLName);

  if (XLinkLibrary == INVALID_HANDLE_VALUE) {
     printf ("Can't load %s (code %d)\n", XLinkDLLName, GetLastError());
     return 1;
  }

  InvokeXLink = (int (*)(char *, int))GetProcAddress(XLinkLibrary, "InvokeXLink");
  if (!InvokeXLink) {
     printf ("Invalid %s (code %d)\n", XLinkDLLName, GetLastError());
     return 1;
  }

  if (extractCmdLine (cmdLineBuf)) {
     printf ("Internal error occured while parsing command line\n");
     return 1;
  }

  InvokeXLink (cmdLineBuf, /* not silent */ 0);

  FreeLibrary (XLinkLibrary);

  return 0;
}
