/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Message stuff.  */

#ifndef _MESSAGES_H_
#define _MESSAGES_H_

#include "defs.h"


extern void info (const char *msg, ...);

extern void warning (const char *msg, ...);

extern void error (const char *msg, ...);

extern void assert (const char *file, int line);


#endif // _MESSAGES_H_
