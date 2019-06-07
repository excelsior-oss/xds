/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Message stuff.  */

#include "messages.h"
#include <stdio.h>
#include <stdarg.h>


#define MAX_MESSAGE_LENGTH 1024


static void
print_message (const char *header, const char *msg, va_list vl)
{
    char buffer [MAX_MESSAGE_LENGTH];

    vsprintf (buffer, msg, vl);

    printf (header, buffer);
}

extern int X2C_ASSERT_FC (int no, const char *file, int line);
extern int X2C_ASSERT_F  (int no);

void
assert (const char *file, int line)
{
    printf ("ASSERT: file: %s, line: %d\n", file, line);

    // signal to xds' runtime
    X2C_ASSERT_F (197);
}


void
info (const char *msg, ...)
{
    va_list vl;
    va_start (vl, msg);

    print_message ("INFO: %s\n", msg, vl);
    
    va_end (vl);
}

void
warning (const char *msg, ...)
{
    va_list vl;
    va_start (vl, msg);

    print_message ("WARNING: %s\n", msg, vl);
    
    va_end (vl);
}

void
error (const char *msg, ...)
{
    va_list vl;
    va_start (vl, msg);

    print_message ("ERROR: %s\n", msg, vl);
    
    va_end (vl);
}

/*
void
error (const char *msg, ...)
{
    va_list vl;
    char buffer [MAX_MESSAGE_LENGTH];

    va_start (vl, msg);
    vsprintf (buffer, msg, vl);
    va_end (vl);

    printf ("ERROR: %s\n", buffer);
}
*/


