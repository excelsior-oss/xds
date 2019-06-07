#if defined(xos_WINNT)
  #include "WinMake.h"
#else
  #define MSG_SEVERE      'S'
  typedef char ERRCLASS;

  extern int  ConnectShell (void);
  extern void DisconnectShell (void);
  extern void SendError (ERRCLASS err_class, long err_no, long x, long y,
                         char * filename, char * body);
#endif
