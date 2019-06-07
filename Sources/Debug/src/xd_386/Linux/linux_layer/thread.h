/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Thread debugging stuff.  */

#ifndef _THREAD_H_
#define _THREAD_H_

#include "defs.h"
#include "signals.h"

#include <thread_db.h>


#define STATE_RUNNING          'R'
#define STATE_SLEEPING         'S'
#define STATE_UNINTERRUPTIBLE  'D'
#define STATE_TRACED           'T'
#define STATE_ZOMBIE           'Z'


/* Either pthread-like thread or child process. */
struct thread_info
{
    /* Thread ptid. */
    ptid_t ptid;

    /* Proper pthread_db's thread handle. The fact that the value is not NULL
       means that this thread_info correspond to pthread's thread now. */
    const td_thrhandle_t * td_thrhandle;

    /* Set if the corresponding thread already died. */
    uint_t expired :1;
    
    /* Indicates if the thread is stopped already. */
    uint_t is_stopped :1;
    
    /* Indicates if the thread corresponds to the pthread's thread or
       just to child process. */
    uint_t is_phtread_thread :1;

    /* The the last status value for the thread. */
    int last_status;

    /* The last signal. */
    enum target_signal last_signal;

    
    /* The linked list field. */
    list_head_t list;

    /* Indicates whether the thread is in system call. */
    uint_t is_syscall :1;
    
    /* Indicates if the current syscall is critical. */
    uint_t is_syscall_critical :1;
};

typedef struct thread_info * thread_info_t;

struct event;


#define IS_THREAD(thread_info)  (thread_info->td_thrhandle != 0)



extern char * thread_db_err_str (td_err_e err);

extern char * thread_db_state_str (td_thr_state_e state);


/* Tries to connect to pthread library. */
extern void try_to_connect_to_pthread_lib ();

/* Creates thread event handler. Return false on errors. */
extern int enable_thread_event_reporting ();

/* Removes thread event handler. Return false on errors. */
extern int disable_thread_event_reporting ();

/* Returns true if given process is stopped by the thread event handler. */
extern int check_thread_event (thread_info_t thread_info);

/* Finds proper thread_info_t object that correspond to given ptid. */
extern thread_info_t find_thread_info_entry (pid_t pid);

/* Finds proper thread_info_t object that correspond to given ptid and
   satisfying conditiion COND with DATA. */
extern thread_info_t find_thread_info_entry_cond (pid_t pid, xbool (*cond)(thread_info_t, void*), void *data);

extern thread_info_t find_thread_info_if (xbool (*cond)(thread_info_t, void*), void *data);

/* Creates proper thread_info_t object that correspond to given ptid. */
extern thread_info_t create_thread_info_entry (pid_t pid);


/* Updates proc_info's list of handled threads.
   As a result the [out]thread_info would contain either 0 or proper thread_info entry.
   Returns 0 if the list is consistent with the process' set of threads.
   Returns 1 if the thread_info entry was added to the list (thread created).
   Returns 2 if the thread_info entry was removed from the list (thread is dead). */
extern int update_thread_list (thread_info_t *thread_info);

extern thread_info_t search_for_unregistered_threads ();



extern int attach_thread (thread_info_t thread_info);

extern int detach_thread (thread_info_t thread_info);

/* Returns one of RSDZTW chars. And E for error. */
extern char get_proc_state (pid_t pid);


/* Iterates over all known threads and ensures they are in T (traced) state. */
extern void stop_all_threads (struct event ** events);

/* Iterates over all known threads and calls target_cont for each of them. */
extern struct event * resume_all_threads ();

/* Terminates all threads. For calling from terminate_process. */
extern int terminate_all_threads ();

extern void remove_thread_info_entry (thread_info_t thread_info);

extern void print_threads_states();

extern pid_t get_pid(thread_info_t thread_info);


#endif // _THREAD_H_
