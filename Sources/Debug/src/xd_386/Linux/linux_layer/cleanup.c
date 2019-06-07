/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Cleanup stuff.  */

#include "cleanup.h"


/* Cleanup manipulating functions. 
   Return false of errors. */

int 
register_cleanup (list_head_t *cleanup_list, 
                  cleanup_func_cb cleanup_func,
                  void *cleanup_data)
{
    cleanup_t * cleanup = (cleanup_t *)malloc (sizeof (cleanup_t));

    cleanup->cleanup_func = cleanup_func;
    cleanup->cleanup_data = cleanup_data;

    // add it to the cleanup_list
    list_add_tail (&cleanup->list, cleanup_list);
    
    return 1;
}

int 
perform_cleanup (list_head_t *cleanup_list)
{
    list_head_t *pos, *tmp;
    cleanup_t * cleanup;

    list_for_each_safe (pos, tmp, cleanup_list) {
        list_del (pos);

        cleanup = list_entry (pos, cleanup_t, list);
        cleanup->cleanup_func (cleanup->cleanup_data);
        free (cleanup);
    }
    return 1;
}

