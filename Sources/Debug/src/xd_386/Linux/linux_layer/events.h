/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* This module provides low level events. */

#ifndef _EVENTS_H_
#define _EVENTS_H_


#include "thread.h"
#include "defs.h"


struct event {
  struct event * next;
  thread_info_t thread_info;
};
 

extern struct event * create_event(thread_info_t info);
extern void           delete_event(struct event * event);
extern void           add_event(struct event ** queue, struct event * event);
extern struct event * remove_event(struct event ** queue);
extern int            pid_in_queue(struct event * queue, pid_t pid);
extern int            events_number(struct event * queue);

extern struct event * wait_for_events_internal();



#endif // _EVENTS_H_
