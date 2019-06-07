#include "xdefs.h"

open_namespace

#if defined(xos_WINNT)
  #include "WinMake.cpp"
#else
  #include "xshell.h"

  int ConnectShell (void) {
      return 0;
  }

  void DisconnectShell (void) {
  }

  void SendError (ERRCLASS /* err_class */,
                  long     /* err_no */,
                  long     /* x */,
                  long     /* y */,
                  char *   /* filename */,
                  char *   /* body*/)
  {
  }
#endif

close_namespace

