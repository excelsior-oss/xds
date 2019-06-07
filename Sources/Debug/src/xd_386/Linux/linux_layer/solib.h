/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Solib stuff.  */

#ifndef _SOLIB_H_
#define _SOLIB_H_

#include "defs.h"
#include "list.h"
#include "thread.h"

#include <bfd.h>


struct solib
{
    /* The name of solib. */
    char * name;

    /* The base address of loaded solib. */
    CORE_ADDR base;

    /* The proper bfd object. */
    bfd * abfd;

    /* Used by update_solib_list to mark already checked solibs. */
    unsigned int checked :1;

    /* The flag is set for main debugee's executable solib. */
    unsigned int main_solib :1;

    /* The flag is set for XKRN solib. */
    unsigned int runtime_solib :1;

    /* Double-linked list pointers. */
    list_head_t list;
};

typedef struct solib solib_t;


/* Creates solib event handler. Return false on errors. */
extern int enable_solib_event ();

/* Removes solib event handler. Return false on errors. */
//extern int disable_solib_event ();

/* Returns true if given process is stopped by the solib event handler. */
extern int check_solib_event (thread_info_t thread_info);
extern int check_solib_event2 (CORE_ADDR address);
                                                                             
extern solib_t * find_solib_if (xbool (*comp)(solib_t*, void*), void * data);

/* Finds proper solib_t object that correspond to given values among registered. */
extern solib_t *find_solib_entry (const char *solib_name, CORE_ADDR solib_base);

/* Creates proper solib_t object that correspond to given values and registers it. */
extern solib_t *create_solib_entry (const char *solib_name, CORE_ADDR solib_base);

/* Updates proc_info's list of handled solibs.
   As a result the [out]solib_t would contain either 0 or proper solib entry.
   Returns 0 if the list consists with the process' dynamic linker state.
   Returns 1 if the solib entry was added to the list (solib loaded).
   Returns 2 if the solib entry was removed from the list (solib unloaded). */
extern int update_solib_list (solib_t ** solib, thread_info_t thread_info);

extern void print_solib_list ();

#endif // _SOLIB_H_
