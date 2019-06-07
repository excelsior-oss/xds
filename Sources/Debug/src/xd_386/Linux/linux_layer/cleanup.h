/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Cleanup stuff.  */

#ifndef _CLEANUP_H_
#define _CLEANUP_H_

#include "list.h"


typedef (*cleanup_func_cb) (void*);

struct cleanup
{
    /* Double-linked list pointers. */
    struct list_head list;

    /* Function to be called to perform cleanup. */
    cleanup_func_cb cleanup_func;

    /* Data to be passed to cleanup function. */
    void *cleanup_data;
};

typedef struct cleanup cleanup_t;



/* Cleanup manipulating functions. 
   Return false of errors. */

extern int register_cleanup (list_head_t *cleanup_list, 
                             cleanup_func_cb cleanup_func,
                             void *cleanup_data);

extern int perform_cleanup (list_head_t *cleanup_list);


#endif // _CLEANUP_H_
