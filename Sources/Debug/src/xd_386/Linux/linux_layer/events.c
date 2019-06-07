/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

#include "thread.h"
#include "linux-low.h"
#include "events.h"
#include "malloc.h"

#include <sys/wait.h>
#include <errno.h>


struct event * create_event(thread_info_t info) {
    struct event * event = (struct event*) malloc(sizeof(struct event));
    ASSERT(info);
    event->next = NULL;
    event->thread_info = info;
    return event;
}


void delete_event(struct event * event) {
    free(event);
}


/* adds the given event to end of events queue */
void add_event(struct event ** queue, struct event * event) {
    ASSERT(queue);

    if(!event)
        return;

    if(!*queue) {
        *queue = event;
    }
    else {
        struct event * q = *queue;
        while(q->next)
            q = q->next;
        q->next = event;
    }
}


/* removes first event from queue */
struct event * remove_event(struct event ** queue) {
    ASSERT(queue);

    if(*queue) {
        struct event * q = *queue;
        *queue = (*queue)->next;
        return q;
    }
    else
        return NULL;
}


int pid_in_queue(struct event * queue, pid_t pid) {
    while(queue) {
        if (get_pid(queue->thread_info) == pid)
            return 1;

        queue = queue->next;
    }

    return 0;
}


int events_number(struct event * queue) {
    int n=0;
    
    while(queue) {
        ++n;
        queue = queue->next;
    }

    return n;
}



/************************************************************************/


struct event * wait_for_events_internal() {
    struct event * events, * event = NULL;
    pid_t pid;
    int status;
    
    info ("***** waiting for events");

    for(;;) {

        pid = target_waitpid (-1, &status, WNOHANG | __WALL);

        if(pid == 0) { // no children available

            if(events) // there are events already received
                break;

            // wait for an event
            if((pid = target_waitpid (-1, &status, __WALL)) < 0)
                error ("%s: target_waitpid failed: %s", strerror(errno));
            else {
                info ("***** OK, event detected [pid=%d]", pid);
                events = create_event(find_thread_info_entry (pid));
                events->thread_info->last_status = status;
            }

            break;
        }

        if(pid < 0) {
            error ("%s: target_waitpid failed: %s", strerror(errno));
            break;
        }

        info ("***** OK, event detected [pid=%d]", pid);

        event = create_event(find_thread_info_entry (pid));
        event->thread_info->last_status = status;

        add_event(&events, event);
    }

    ASSERT(events);

    info ("%s: Total events received: %d", __FUNCTION__, events_number(events));

    proc_info->last_signaled_thread = events->thread_info;

    return events;
}



