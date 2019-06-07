/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* This module provides low level interface to debugee process. */

#ifndef _INFERIOR_H_
#define _INFERIOR_H_

#include <stdio.h>
#include <sys/types.h>
#include <thread_db.h>

#include "defs.h"
#include "solib.h"
#include "breakpoint.h"
#include "cleanup.h"
#include "signals.h"
#include "thread.h"
#include "syscall.h"



/* The -1 ptid, often used to indicate either an error condition
   or a "don't care" condition, i.e, "run all threads."  */
extern ptid_t minus_one_ptid;

/* The null or zero ptid, often used to indicate no process. */
extern ptid_t null_ptid;


/* Attempt to find and return an existing ptid with the given PID, LWP,
   and TID components.  If none exists, create a new one and return
   that.  */
ptid_t ptid_build (int pid, long lwp, long tid);

/* Find/Create a ptid from just a pid. */
ptid_t pid_to_ptid (int pid);

/* Fetch the pid (process id) component from a ptid. */
int ptid_get_pid (ptid_t ptid);

/* Fetch the lwp (lightweight process) component from a ptid. */
long ptid_get_lwp (ptid_t ptid);

/* Fetch the tid (thread id) component from a ptid. */
long ptid_get_tid (ptid_t ptid);

/* Compare two ptids to see if they are equal */
extern int ptid_equal (ptid_t p1, ptid_t p2);



/* The debugee process info. */

struct proc_info
{
    /* Process id. */
    pid_t pid;

    /* The name of debugee program. */
    char * debugee_name;

    /* The handle of the /proc/%d/maps file. */
    FILE * fmaps;

    /* The bfd object that corresponds to the main debugee program. */
    bfd * debugee_bfd;

    /* The startup address for the debugee. */
    CORE_ADDR startup_address;

    /* The entry point of the debugee program. */
    breakpoint_t entry_point;

    /* event for processing calls of debuggee to X2C_Ptrace and X2C_Waitpid. */
    breakpoint_t ptrace_event;
    breakpoint_t waitpid_event;

    /* Thread agent for the debugee. */
    td_thragent_t * thread_agent;

    /* pthread_db events for handling thread creation and death. */
    breakpoint_t thread_create_event;
    breakpoint_t thread_death_event;

    /* The the last status value. */
//    int last_status;

    /* The last signal. */
//    int last_signal;
//    enum target_signal last_signal;
    
    /* The flag indicates that t. */
    uint_t main_thread_registered :1;

    
    /* The flag indicates that more than one thread was created. */
    uint_t multithreaded :1;

    /* Indicates that the debugee was executed in singlestep mode. */
    uint_t singlestep :1;

    /* The flag is set if the debugee has finished. */
    uint_t is_finished :1;

    /* This flag makes wait_for_debug_event to search for unregistered threads. */
    uint_t do_search_for_threads :1;

    /**/
//    unsigned int is_ld_solib_signaled :1;
    
    /* Forces wait_for_debug_event function to check for events again
       before calling to wait. */
    uint_t check_events_again :1;

    /* The flag is set if the debugee is re-execing a process. */
    uint_t do_reexec :1;
    struct syscall_status reexec_status; 

    /* ptid where the last event happened. */
    ptid_t last_ptid;

    thread_info_t last_signaled_thread;

    thread_info_t main_thread;



    /* The list of thread_info objects. */
    list_head_t threads;

    /* The solib event breakpoint. */
    breakpoint_t solib_event;    
    
    /* The list of solib_t objects. */
    list_head_t loaded_solib;
  
    /* The address of rdebug symbol of debugee program. Having this address 
       it is possible to receive events from the dynamic loader. */
    CORE_ADDR rdebug;



    /* The list of cleanup_t objects. */
    list_head_t cleanup;

    xbool current_thread_is_dead;

    /* this flags means that routines working with the maps file must re-read it */       
    xbool maps_file_consistent;
};

typedef struct proc_info * proc_info_t;


/* Structure that identifies the target process.  */
struct ps_prochandle
{
    /* We need process info here.  */
    proc_info_t proc_info;
};


extern proc_info_t proc_info;


#define PCLEANUP (&proc_info->cleanup)



/* The debug_event definition. */
typedef enum {VM_EXEC=1, VM_READ=2, VM_WRITE=4} access_flags;

typedef enum {
    DE_INITIALIZED = 1,
    DE_ASK_AGAIN,
    DE_EXITPROCESS,
    DE_EXCEPTION,
    DE_LOAD_SOLIB,
    DE_UNLOAD_SOLIB,
    DE_THREAD_CREATED,
    DE_THREAD_EXITED
} debug_event_type;


typedef struct {
    int exit_code;
    int status;
    int pc;
    int is_terminated;
} exit_process_debug_info;


typedef enum {
    EE_EXCEPTION = 1,
    EE_BREAKPOINTHIT,
    EE_SINGLESTEP,
    EE_HIDDEN_EXCEPTION
} exception_event_type;



typedef struct {
    char* name;
    FILE* hfile;
    int base;
    int is_loaded; // true if solib is loaded
    int is_program;
} solib_debug_info;


typedef struct {
    exception_event_type type;
    int pc;
    int signal;
} exception_debug_info;


typedef struct {
    thread_info_t new_thread_info;
} thread_debug_info;


typedef struct {
    debug_event_type type;
    thread_info_t thread_info;
    union {
        exception_debug_info exception;
        exit_process_debug_info exit_process;
        solib_debug_info solib;
        thread_debug_info thread;
    };

} debug_event;





/* Initializes whole linux layer. */
extern int init_linux_layer ();


/* Duplicates given string and registers its cleanup in proc_info. */
extern char * xcstrdup (const char * str);


/* The high level process IO functions. */

/* Returns true if the debugee is multithreaded. */
extern int is_multithreaded ();

/* Creates new process, allocates and initializes proc_info structure.
   Returns proper ptid_t object.
 */
extern thread_info_t create_process (const char *program, const char *args[]);

/* Terminates current process and deallocates current proc_info structure. */
extern int terminate_process ();

/* Checks debugee status and fills the debug_event structure. */
extern int wait_for_debug_event (debug_event* di);

/* Continues debugee execution. */
extern int continue_debug_event (thread_info_t thread_info, int do_singlestep, int cont_flag);


/* Process' mapping accessing procedures. */
extern int get_segment_info (CORE_ADDR addr, int *begin, int *len, int *access_flags);







/* The debugee process accessing functions.
   They return true on success and false on errors. */

extern long proc_get_pc (thread_info_t thread_info);

extern int proc_set_pc (thread_info_t thread_info, long value);

extern long proc_get_reg (thread_info_t thread_info, int regno);

extern int proc_set_reg (thread_info_t thread_info, int regno, long value);

extern int proc_get_regs (thread_info_t thread_info, long *buffer);

extern int proc_set_regs (thread_info_t thread_info, long *buffer);

extern int proc_get_fregs (thread_info_t thread_info, long *buffer);

extern int proc_set_fregs (thread_info_t thread_info, long *buffer);

extern int proc_get_dbgregs (thread_info_t thread_info, long *buffer);

extern int proc_set_dbgregs (thread_info_t thread_info, long *buffer);

extern int proc_get_memory (thread_info_t thread_info, CORE_ADDR source, int size, void *dest);

extern int proc_set_memory (thread_info_t thread_info, const void *source, int size, CORE_ADDR dest);




#endif // _INFERIOR_H_
