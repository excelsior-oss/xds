/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* This module provides low level interface to debugee process.  */

#include <sys/wait.h>

//#include <sys/user.h>

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <memory.h>
#include <malloc.h>
#include <signal.h>
#include <sys/ptrace.h>
#include <asm/ptrace.h>

#include "inferior.h"
#include "thread.h"
#include "signals.h"
#include "events.h"
#include "syscall.h"

//#define SYSCALL_PROCESSING_ENABLED

static char* 
regname[] =
{
    "ebx",
    "ecx",
    "edx",
    "esi",
    "edi",
    "ebp",
    "eax",
    "xds",
    "xes",
    "xfs",
    "xgs",
    "OAX",
    "eip",
    "xcs",
    "EFL",
    "esp",
    "xss",
    0
};


/* Oft used ptids */
ptid_t null_ptid = {0, 0, 0};
ptid_t minus_one_ptid = {-1, 0, 0};



/* Create a ptid given the necessary PID, LWP, and TID components.  */
   
ptid_t
ptid_build (int pid, long lwp, long tid)
{
  ptid_t ptid;

  ptid.pid = pid;
  ptid.lwp = lwp;
  ptid.tid = tid;
  return ptid;
}

/* Create a ptid from just a pid.  */

ptid_t
pid_to_ptid (int pid)
{
  return ptid_build (pid, 0, 0);
}

/* Fetch the pid (process id) component from a ptid.  */

int
ptid_get_pid (ptid_t ptid)
{
  return ptid.pid;
}

/* Fetch the lwp (lightweight process) component from a ptid.  */

long
ptid_get_lwp (ptid_t ptid)
{
  return ptid.lwp;
}

/* Fetch the tid (thread id) component from a ptid.  */

long
ptid_get_tid (ptid_t ptid)
{
  return ptid.tid;
}

/* ptid_equal() is used to test equality of two ptids.  */

int
ptid_equal (ptid_t ptid1, ptid_t ptid2)
{
  return (ptid1.pid == ptid2.pid && ptid1.lwp == ptid2.lwp
          && ptid1.tid == ptid2.tid);
}




/* Duplicates given string and registers its cleanup in proc_info. */
char * 
xcstrdup (const char * str)
{
    char * str2 = strdup (str);
    register_cleanup (PCLEANUP, (cleanup_func_cb)&free, str2);
    return str2;
}



/* Initializes whole linux layer. */
int
init_linux_layer ()
{
    static int initialized = 0;
    td_err_e err;

    if (initialized) {
        return;
    }

    bfd_init ();

    err = td_init ();
    if (err != TD_OK) {
        error ("Cannot initialize libthread_db: %d", err);
    }
    initialized = 1;
}




/* The info about the process being debugged. */
proc_info_t proc_info = 0;



static xbool 
is_signal_reporting_enabled (int signal);



/* The debugee process accessing functions.
   They return true on success and false on errors. */

long 
proc_get_pc (thread_info_t thread_info)
{
    int res = proc_get_reg (thread_info, EIP);
    if (res == 0) {
        warning ("the pc is 0");
    } 
    return res;
}


int 
proc_set_pc (thread_info_t thread_info, long value)
{
//    return proc_set_reg (thread_info, EIP, value);
    int res = proc_set_reg (thread_info, EIP, value);
    if (res == 0) {
        warning ("setting pc to 0");
    } 
    return res;
}


long 
proc_get_reg (thread_info_t thread_info, int regno)
{
    pid_t pid = get_pid(thread_info);
    return target_get_reg (pid, regno);
}

int 
proc_set_reg (thread_info_t thread_info, int regno, long value)
{
    pid_t pid = get_pid(thread_info);
    return target_set_reg (pid, regno, value);
}

int 
proc_get_regs (thread_info_t thread_info, long *buffer)
{
    pid_t pid;
    int res, i;

    if (proc_info->is_finished) {
        return 1;
    }

    pid = get_pid(thread_info);
    res = target_get_regs (pid, buffer);

    return res;
}

int 
proc_set_regs (thread_info_t thread_info, long *buffer)
{
    pid_t pid = get_pid(thread_info);
    return target_set_regs (pid, buffer);
}

int 
proc_get_fregs (thread_info_t thread_info, long *buffer)
{
    return 1;
/*
    register long pid = GET_TGTPID (ptid);
    //ensure_stopped (pid);
    return target_get_fregs (pid, buffer);
*/
}

int 
proc_set_fregs (thread_info_t thread_info, long *buffer)
{
    return 1;
/*
    register long pid = GET_TGTPID (ptid);
    //ensure_stopped (pid);
    return target_set_fregs (pid, buffer);
*/
}


int 
proc_get_dbgregs (thread_info_t thread_info, long *buffer)
{
    return 1;
/*
    register long pid = GET_TGTPID (ptid);
    //ensure_stopped (pid);
    return target_get_dbgregs (pid, buffer);
*/
}

int 
proc_set_dbgregs (thread_info_t thread_info, long *buffer)
{
    return 1;
/*
    register long pid = GET_TGTPID (ptid);
    //ensure_stopped (pid);
    return target_set_dbgregs (pid, buffer);
*/
}

int 
proc_get_memory (thread_info_t thread_info, CORE_ADDR source, int size, void *dest)
{
    pid_t pid;

    if (proc_info->is_finished)
        return 1;

    pid = get_pid(thread_info);
    return target_get_memory (pid, source, size, dest);
}

int 
proc_set_memory (thread_info_t thread_info, const void *source, int size, CORE_ADDR dest)
{
    pid_t pid;

    if (proc_info->is_finished)
        return 1;

    pid = get_pid(thread_info);
    return target_set_memory (pid, source, size, dest);
}



/* Returns true if the debugee is multithreaded. */
int 
is_multithreaded ()
{
    ASSERT (proc_info != 0);
    return proc_info->multithreaded;
}


/* The high level process IO functions. */

/* Creates new process, allocates and initializes proc_info structure.
   Returns proper ptid_t object.
 */
thread_info_t
create_process (const char *program, const char *args[])
{
    int status;
    char buffer[256];
    bfd *debugee_bfd;

    pid_t pid;
    ptid_t ptid;
    thread_info_t thread_info;

    struct syscall_status reexec;
    xbool do_reexec = xfalse;

    info ("%s called proc_info = %#X", __FUNCTION__, proc_info);

    _initialize_signals();

    if (proc_info != 0) {
        do_reexec = proc_info->do_reexec;
        reexec = proc_info->reexec_status;

        info ("forcing termination");
        terminate_process ();
    }

    info ("%s: reexec = %s", __FUNCTION__, do_reexec? "YES" : "NO");

    pid = fork ();
    if (pid < 0) {
        error ("failed to create process");
        return 0;//null_ptid;
    }

    if (pid == 0) {
        target_traceme ();

        if(do_reexec) {
            execve (reexec.args.execve_args.filename,
                    reexec.args.execve_args.argv,
                    reexec.args.execve_args.envp);

            error ("execve failed, program=%s, errno=%d: %s\n", program, errno, strerror(errno));
        }
        else {
            execv (program, args);
            error ("execv failed, program=%s, errno=%d: %s\n", program, errno, strerror(errno));
        }
        _exit (153); // failed to execute process
    }

    target_waitpid (pid, &status, 0);

    if (WIFEXITED (status) || !WIFSTOPPED (status)) {
        error ("failed to create process");
        return 0;//null_ptid;
    }

    if(do_reexec) {
        (*reexec.cleanup)(&reexec);
    }

    proc_info = (proc_info_t)malloc (sizeof (struct proc_info));
    memset (proc_info, 0, sizeof (struct proc_info));

    INIT_LIST_HEAD (&proc_info->cleanup);
    INIT_LIST_HEAD (&proc_info->loaded_solib);
    INIT_LIST_HEAD (&proc_info->threads);

    proc_info->entry_point = create_breakpoint (BPT_TYPE_BPT_RET);
    proc_info->solib_event = create_breakpoint (BPT_TYPE_BPT_RET);

    proc_info->ptrace_event  = create_breakpoint (BPT_TYPE_BPT_RETXX);
    proc_info->waitpid_event = create_breakpoint (BPT_TYPE_BPT_RETXX);

    proc_info->thread_create_event    = create_breakpoint (BPT_TYPE_BPT_RET);
    proc_info->thread_death_event     = create_breakpoint (BPT_TYPE_BPT_RET);

    proc_info->pid = pid;
    ptid = pid_to_ptid (proc_info->pid);

    proc_info->maps_file_consistent = xfalse;

    sprintf (buffer, "/proc/%d/maps", pid);
    proc_info->fmaps = fopen (buffer, "r");
    if (proc_info->fmaps == 0) {
        error ("failed to open inferior mappings file");
        free (proc_info);
        proc_info = 0;
        return 0;//null_ptid;
    }

    thread_info = create_thread_info_entry (pid);

    proc_info->main_thread = thread_info;
    proc_info->last_signaled_thread = thread_info;

    proc_info->debugee_bfd = debugee_bfd = bfd_openr (program, 0);

    if (bfd_check_format (debugee_bfd, bfd_object)) {
        if (debugee_bfd == 0 || enable_solib_event () == 0 ) {
            error ("bfd_openr error, program=%s\n", program);
            bfd_close (debugee_bfd);
            fclose (proc_info->fmaps);
            free (proc_info);
            proc_info = 0;
            return 0;//null_ptid;
        }
    } else {
        // ignore error and continue
        bfd_close (debugee_bfd);
        proc_info->debugee_bfd = 0;
        warning ("bad program bfd");
    }

    proc_info->debugee_name = xcstrdup (program);
    proc_info->startup_address = proc_get_pc (thread_info);

    proc_info->current_thread_is_dead = xfalse;

    info ("process created: pid=%d (debugger pid is %d)\n", proc_info->pid, getpid());

    return thread_info;
}


/* Terminates current process and deallocates current proc_info structure. */
int 
terminate_process ()
{
    td_err_e err;
    struct my_link_map *lm;
    struct string_list *sl;
    struct list_head *pos, *tmp;

    if (proc_info == 0) {
        return 1;
    }

    info ("process termination started");

    if (proc_info->thread_agent) {
        err = td_ta_delete (proc_info->thread_agent);
        if (err != TD_OK) {
            warning ("Cannot deinitialize thread debugging library: %s\n", strerror(err));
        }
    }
    terminate_all_threads ();

    // destroy proc object
    if (proc_info->fmaps) {
        fclose (proc_info->fmaps);
    }

    if (proc_info->debugee_bfd) {
        bfd_close (proc_info->debugee_bfd);
    }

    perform_cleanup (PCLEANUP);

    free (proc_info);
    proc_info = 0;

    info ("process terminated\n\n");
    return 1;
}



static int do_syscall(pid_t pid, long int syscall)
{
    long int result;

    if(!execute_syscall(pid, syscall, &result)) {
        error("[pid=%d]: Cannot execute system call %d (%s)",
               pid, syscall, syscall_name(syscall));
        return 0;
    }

    info ("%s: syscall %d (%s) executed OK, result: %d", __FUNCTION__, syscall, syscall_name(syscall), result);

    if(!store_syscall_result(pid, result)) {
        error("[pid=%d]: Cannot store system call %d (%s) result (%d)",
               pid, syscall, syscall_name(syscall), result);
        return 0;
    }
    
    return 1;
}


static int process_syscall(thread_info_t thread_info, debug_event* di, exception_debug_info *ex_di) {
    pid_t pid = get_pid(thread_info);
    struct syscall_status sc_status = get_syscall_status(pid);
    xbool do_cleanup = xtrue;

    if (thread_info->is_syscall && sc_status.syscall == -1) { 
        // already entered
        return 1;
    }

    if(!syscall_is_valid(sc_status.syscall)) {
        warning("%s: [pid=%d] bad syscall number received: %d", __FUNCTION__, pid, sc_status.syscall);
        return 1;
    }

    /* Because we use PTRACE_SYSCALL, a process (thread) will
       be stopped twice: at the next entry to or exit from a
       system call. If the given syscall is critical, we should
       execute it by hand. */
    
    if(!thread_info->is_syscall) { // entering?
        if(sc_status.state != -ENOSYS)  {
            //info ("[pid=%d]: our heuristics is bad - looks like we're exiting system call", pid, syscall, syscall_name(syscall));
            thread_info->is_syscall = 1;
        }
    } else { // exiting?
        if(sc_status.state == -ENOSYS)  {
            //info ("[pid=%d]: our heuristics is bad - looks like we're entering system call", pid, syscall, syscall_name(syscall));
            thread_info->is_syscall = 0;
        }
    }

    if(!thread_info->is_syscall) { // entering
        info ("[pid=%d]: SYSCALL %d (%s) ENTERING", pid, sc_status.syscall, syscall_name(sc_status.syscall));

        /* do not execute critial syscalls */
        if(thread_info->is_syscall_critical = syscall_is_critical(pid, sc_status.syscall)) {
            info ("[pid=%d]: SYSCALL %d (%s) is critical!!!", pid, sc_status.syscall, syscall_name(sc_status.syscall));

            if(syscall_is_nonreturnee(sc_status.syscall)) {
                if(!do_syscall(pid, sc_status.syscall))
                    error("%s (line %d): do_syscall failed!", __FUNCTION__, __LINE__);
            }
            else if(!skip_syscall(pid))
                error ("%s (line %d): skip_syscall() failed", __FUNCTION__, __LINE__);
        }

        thread_info->is_syscall = 1;

        /* now let's examine special cases */
        if(sc_status.syscall == SYSCALL_EXECVE) {
            proc_info->do_reexec = 1;
            proc_info->reexec_status = sc_status;
            do_cleanup = xfalse; // do not kill execve args!
        }                
    }
    else { // exit
        info ("[pid=%d]: SYSCALL %d (%s) EXITING", pid, sc_status.syscall, syscall_name(sc_status.syscall));

        if(thread_info->is_syscall_critical) {
            info ("[pid=%d]: SYSCALL %d (%s) is critical!!!", pid, sc_status.syscall, syscall_name(sc_status.syscall));

            if(!do_syscall(pid, sc_status.syscall))
                error("%s (line %d): do_syscall failed!", __FUNCTION__, __LINE__);
        }

        thread_info->is_syscall = 0;
    }

    // do cleanup stuff
    if(do_cleanup) {
        if(sc_status.cleanup)
            (*sc_status.cleanup)(&sc_status);
    }

    return 1;
}


static xbool threads_not_equal(thread_info_t t1, thread_info_t t2)
{
    return t1 != t2;
}


static xbool solib_not_main(solib_t *so, void *unused)
{
    return !so->main_solib;
}


int proceed_event(thread_info_t thread_info, debug_event* di) {
    enum target_signal signal;
    struct list_head *pos;
    exception_debug_info *ex_di;
    solib_t * solib;
    td_err_e err;
    pid_t pid = get_pid(thread_info);
    int status = thread_info->last_status;
    int pc;

    info ("%s: thread_info == %#X [pid=%d]", __FUNCTION__, thread_info, pid);

    if(WIFEXITED (status))
        info ("%s: WIFEXITED. Status: %d", __FUNCTION__, WEXITSTATUS (status));
    else if(WIFSIGNALED (status))
        info ("%s: WIFSIGNALED. Signal: %d (%s)", __FUNCTION__, WTERMSIG (status), target_signal_to_name (WTERMSIG (status)));
    else if(WIFSTOPPED (status))
        info ("%s: WIFSTOPPED. Signal: %d (%s)", __FUNCTION__, WSTOPSIG (status), target_signal_to_name (WSTOPSIG (status)));
    else 
        warning ("%s: Unknown wait status!!!", __FUNCTION__);


    if (proc_info->do_reexec) {
       thread_info_t thread_info;
       solib_t *solib;

//       proc_info->check_events_again = 1;
       
/*       thread_info = find_thread_info_if((xbool (*)(thread_info_t, void*))threads_not_equal,
                                         proc_info->main_thread);
       
       if (thread_info) {
           di->thread.new_thread_info = thread_info;
           di->type = DE_THREAD_EXITED;
           detach_thread (thread_info);
           return 1;
       }

       solib = find_solib_if(solib_not_main, NULL);

       if(solib) {
           di->solib.name = solib->name;
           di->solib.base = solib->base;
           di->type = DE_UNLOAD_SOLIB;
           return 1;
       }

       proc_info->do_reexec = 0;

       terminate_all_threads ();
*/            
       di->type = DE_EXITPROCESS;
       di->exit_process.pc = proc_info->startup_address;

       //proc_info->do_reexec = 0;

       return 1;
    }

    if (WIFEXITED (status) || WIFSIGNALED (status) ||
        (WIFSTOPPED (status) && WSTOPSIG(status) == SIGCHLD && pid == proc_info->pid))
    {
        if (pid == proc_info->pid) {
            proc_info->is_finished = 1;

//            remove_thread_info_entry (thread_info);

            terminate_all_threads ();
            
            di->type = DE_EXITPROCESS;
            di->exit_process.pc = proc_info->startup_address;

            if (WIFEXITED (status)) {

                di->exit_process.status = WEXITSTATUS (status);
                info ("Process [pid=%d] finished with status %d", pid, WEXITSTATUS (status));

            } else if (WIFSTOPPED (status)) {

                di->exit_process.status = WSTOPSIG (status);
                info ("Process [pid=%d] stopped via signal %d", pid, WSTOPSIG (status));

            } else {
            
                di->exit_process.status = WTERMSIG (status);
                info ("Process [pid=%d] terminated as signaled (signal %d)", pid, WTERMSIG (status));
            
            }

        } else {
            if (WIFEXITED (status)) {
                info ("Thread [pid=%d] exited silently", pid);
            } else if (WIFSIGNALED (status)) { // signaled
                warning ("Thread [pid=%d] terminated via signal %d", pid, WTERMSIG(status));
            }

            di->thread.new_thread_info = thread_info;
            di->type = DE_THREAD_EXITED;

            remove_thread_info_entry (thread_info);

            if (thread_info == proc_info->last_signaled_thread) {
                proc_info->last_signaled_thread = NULL;
                proc_info->current_thread_is_dead = xtrue;
            }
        }

        return 1;
    }

    if (thread_info->expired) {
        proc_info->check_events_again = xfalse;
        di->type = DE_ASK_AGAIN;
        return 1;
    }

    di->type = DE_EXCEPTION;
    ex_di = &di->exception;

    ex_di->pc = pc = proc_get_pc (thread_info);
    ex_di->signal = signal = WSTOPSIG (status);

    if (signal == SIGTRAP) {

        if (get_proc_state (pid) == STATE_UNINTERRUPTIBLE) {
            di->thread.new_thread_info = thread_info;
            di->type = DE_THREAD_EXITED;

            remove_thread_info_entry (thread_info);
    
            if (thread_info == proc_info->last_signaled_thread) {
                proc_info->last_signaled_thread = NULL;
                proc_info->current_thread_is_dead = xtrue;
            }

            return 1;
        }

        if (proc_info->singlestep) {
            ex_di->type = EE_SINGLESTEP;
            // the instruction to be executed addr

        } else {

            if (check_solib_event (thread_info)) {
                int update_res = update_solib_list (&solib, thread_info);

                switch (update_res) {
                    case 0: // consistent
                      proc_info->check_events_again = xfalse;
                      di->type = DE_ASK_AGAIN;
//                      print_solib_list ();
                      break;

                    case 1: // solib loaded
                    case 2: // solib unloaded
                      proc_info->check_events_again = xtrue;

                      di->solib.name = solib->name;
                      di->solib.base = solib->base;
                      di->solib.is_program = solib->main_solib;

                      if (update_res == 1) {
                          // solib loaded
                          di->solib.hfile = fopen (solib->name, "r");
                          di->type = DE_LOAD_SOLIB;

                          if(!di->solib.hfile)
                              error ("failed to open solib '%s': %s", solib->name, strerror(errno));

                          try_to_connect_to_pthread_lib ();
                          try_to_connect_to_JET_runtime_lib ();

                      } else {
                          // solib unloaded
                          di->type = DE_UNLOAD_SOLIB;
                      }
                      break;

                    default:
                      error ("unexpected update_solib_list result");
                      return 0;
                }

            } else if (pc == proc_info->entry_point->address + BREAKPOINT_INSTR_LEN) {
                di->type = DE_INITIALIZED;
                remove_breakpoint (proc_info->entry_point);
                proc_set_pc (thread_info, proc_get_pc (thread_info) - BREAKPOINT_INSTR_LEN);

            } else if (check_thread_event (thread_info)) {
                int update_res = update_thread_list (&thread_info);

                info ("thread event detected (%d)", update_res);

                di->thread.new_thread_info = thread_info;
                switch (update_res) {
                    case 0: // consistent 
                      proc_info->check_events_again = xfalse;
                      di->type = DE_ASK_AGAIN;
                      break;

                    case 1: // new thread created
                      di->type = DE_THREAD_CREATED;

                      proc_info->check_events_again = xtrue;

                      attach_thread (thread_info);
                      break;

                    case 2: // thread is dead
                      di->type = DE_THREAD_EXITED;

                      if (thread_info == proc_info->last_signaled_thread) {
                          proc_info->check_events_again = xfalse;
                          proc_info->last_signaled_thread = NULL;
                          proc_info->current_thread_is_dead = xtrue;
                      } else {
                          proc_info->check_events_again = xtrue;
                      }

                      detach_thread (thread_info);
                      break;

                    default:
                      error ("unexpected update_thread_list result");
                      return 0;
                  }
            } else if (check_ptrace_event (thread_info)) {

                info ("ptrace event detected");
                info ("xd_ptrace2: returned = %d", xd_ptrace2 (thread_info));

                di->type = DE_ASK_AGAIN;
                proc_info->check_events_again = xfalse;

            } else if (check_waitpid_event (thread_info)) {

                info ("waitpid event detected");
                info ("xd_waitpid2: returned = %d", xd_waitpid2 (thread_info));

                di->type = DE_ASK_AGAIN;
                proc_info->check_events_again = xfalse;

            } else {
                char buffer[BREAKPOINT_INSTR_LEN];

                // check if the current instruction is a breakpoint
                if(!target_get_memory (pid,
                                       pc - BREAKPOINT_INSTR_LEN, 
                                       BREAKPOINT_INSTR_LEN,
                                       buffer))
                {
                    error ("%s (line %d): target_get_memory() failed", __FUNCTION__, __LINE__);
                }

                if(is_breakpoint(buffer)) {
                    ex_di->type = EE_BREAKPOINTHIT;
                    // the "int 3" instruction addr
                    ex_di->pc = pc - BREAKPOINT_INSTR_LEN; 
                    info ("EE_BREAKPOINTHIT: at %#x", ex_di->pc);
                } else {
#ifdef SYSCALL_PROCESSING_ENABLED
                    char buffer0[SYSCALL_INSTR_LEN];

                    if(!target_get_memory (pid,
                                           pc - SYSCALL_INSTR_LEN, 
                                           SYSCALL_INSTR_LEN,
                                           buffer0))
                    {
                        error ("%s (line %d): target_get_memory() failed", __FUNCTION__, __LINE__);
                    }
                                        
                    if(is_syscall(buffer0)) {
                        if(!process_syscall(thread_info, di, ex_di))
                            error ("%s: process_syscall failed", __FUNCTION__);
                    }
                    else {
                        //warning ("%s: unknown state of [pid=%d]", __FUNCTION__, pid);
                    }
#endif
/*
                    if(proc_info->do_reexec) {
                        info ("[pid=%d]: process is going to reexec...", pid);
                    }
*/

                    di->type = DE_ASK_AGAIN;
                    proc_info->check_events_again = xfalse;
                    thread_info->last_signal = TARGET_SIGNAL_0;

                    return 1;
                }
            }
        }
    } else {

        thread_info->last_signal = signal;

        info ("debugee received: pid=%d, %s=%d: %s", GET_LWP (thread_info->ptid), 
              target_signal_to_name (signal), signal, target_signal_to_string (signal));

        if (is_signal_reporting_enabled (signal)) {
            ex_di->type = EE_EXCEPTION;

        } else {
            info ("ignoring signal: %d", signal);
            di->type = DE_ASK_AGAIN;
            proc_info->check_events_again = xfalse;
        }
    }

    return 1;
}



/* events queue waiting to proceed */
static struct event * events = NULL;


/* Checks debugee status and fills the debug_event structure. */
int 
wait_for_debug_event (debug_event* di)
{
    int status; //, signal;

    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);

    info ("wait_for_debug_event");

    if (proc_info->check_events_again) {
        thread_info_t thread_info;

        proc_info->check_events_again = xfalse;

        if (proc_info->do_search_for_threads) {
            proc_info->do_search_for_threads = xfalse;
            proc_info->multithreaded = xtrue;

            thread_info = search_for_unregistered_threads ();
            
            if (thread_info != 0) {
                info ("found unregistered thread");
                proc_info->check_events_again = xtrue;

                di->type = DE_THREAD_CREATED;
                di->thread_info = proc_info->main_thread;

                attach_thread (thread_info);
                return 1;
            }
        }
        thread_info = proc_info->last_signaled_thread;
        di->thread_info = thread_info;

        return proceed_event(thread_info, di);
    } else {
        struct event * event = events;
        int ret;

        if (!events) {
            
            /* get a events queue */
            events = wait_for_events_internal();
            stop_all_threads(&events);

            info ("Total events received: %d", events_number(events));

            ASSERT(events);
        }

        event = remove_event(&events);
        di->thread_info = event->thread_info;
        ret = proceed_event(event->thread_info, di);
        delete_event(event);
        return ret;
    }
}


/* Continues debugee execution. */
int 
continue_debug_event (thread_info_t thread_info, int do_singlestep, int cont_flag)
{
    td_err_e err;
    pid_t pid;
    int res = 1;

    info ("continue_debug_event: step=%d", do_singlestep);

    proc_info->singlestep = do_singlestep;

    if(!proc_info->current_thread_is_dead)
        proc_info->last_signaled_thread = thread_info;
    else 
        proc_info->current_thread_is_dead = xfalse;

    if (events || proc_info->check_events_again) {
        info ("continue_debug_event: there are events, so just return...");
        return 1;
    }

    if ((events = resume_all_threads()) != NULL) {
        info ("continue_debug_event: there are events, so just return...");
        return 1;
    }

    info ("continue_debug_event: step=%d, cont=%d, {pid=%d, lwp=%d, tid=%d}",
          do_singlestep, cont_flag, thread_info->ptid);

    return res;
}




struct Segment {
    unsigned long start, end;
    int flags;
    struct Segment *next;
};


void delete_segments_cache(struct Segment *segments_cache) {
    struct Segment *temp;

    while(segments_cache) {
        temp = segments_cache->next;
        free(segments_cache);
        segments_cache = temp;
    }
}


/* Process' mapping accessing procedures. */
int 
get_segment_info (CORE_ADDR addr, int *begin, int *len, int *access_flags)
{
    static struct Segment *segments_cache = NULL;

    struct Segment *last_segment;

    FILE * fmaps = proc_info->fmaps;
    char r,w,x,p;
    unsigned long start, end, offs, dev0, dev1, inode;
    char buffer [1024];
    int res;

    if(!proc_info->maps_file_consistent) {
        struct Segment *seg;

        delete_segments_cache(segments_cache);

        segments_cache = NULL;
        last_segment = NULL;

        fseek (fmaps, 0, SEEK_SET);

        while ( (res = fscanf (fmaps, "%x-%x %c%c%c%c %x %x:%x %d", &start, &end, &r, &w, &x, &p,
            &offs, &dev0, &dev1, &inode)) && res!=EOF)
        {
            seg = (struct Segment *)malloc(sizeof(struct Segment));
            ASSERT(seg != NULL);

            seg->start = start;
            seg->end = end;
            seg->flags = 0;

            if (x=='x')
                seg->flags |= VM_EXEC;

            if (r=='r')
                seg->flags |= VM_READ;

            if (w=='w')
                seg->flags |= VM_WRITE;

            seg->next = NULL;

            if(segments_cache == NULL) {
                segments_cache = last_segment = seg;
            } else {
                ASSERT(last_segment != NULL);

                /* tie to end of the list */ 
                last_segment->next = seg;
                last_segment = seg;
            }
                        
            fgets(buffer, sizeof(buffer), fmaps);
        }

        proc_info->maps_file_consistent = xtrue;
    }
    
    last_segment = segments_cache;

    if (!segments_cache) { // no segments?
        warning("%s(%X): no segments in the cache", __FUNCTION__, addr);
        return 0;
    }

    while(last_segment) {
        if (last_segment->start <= addr && addr < last_segment->end) {
            // match!
            *begin = last_segment->start;
            *len = last_segment->end - last_segment->start;
            *access_flags = last_segment->flags;
            return 1;
        }

        last_segment = last_segment->next;
    }

    // not found
    return 0;
}


static xbool 
is_signal_reporting_enabled (int signal)
{
    if (signal >= __SIGRTMIN || ignored_signals[signal]) {
        info ("signal reporting disabled for: %s", target_signal_to_name (signal));
        return xfalse;
    }

    return xtrue;
}



