/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Thread debugging stuff.  */

#include "thread.h"
#include "events.h"
#include <thread_db.h>



char *
thread_db_err_str (td_err_e err)
{
  static char buf[64];

  switch (err)
    {
    case TD_OK:
      return "generic 'call succeeded'";
    case TD_ERR:
      return "generic error";
    case TD_NOTHR:
      return "no thread to satisfy query";
    case TD_NOSV:
      return "no sync handle to satisfy query";
    case TD_NOLWP:
      return "no LWP to satisfy query";
    case TD_BADPH:
      return "invalid process handle";
    case TD_BADTH:
      return "invalid thread handle";
    case TD_BADSH:
      return "invalid synchronization handle";
    case TD_BADTA:
      return "invalid thread agent";
    case TD_BADKEY:
      return "invalid key";
    case TD_NOMSG:
      return "no event message for getmsg";
    case TD_NOFPREGS:
      return "FPU register set not available";
    case TD_NOLIBTHREAD:
      return "application not linked with libthread";
    case TD_NOEVENT:
      return "requested event is not supported";
    case TD_NOCAPAB:
      return "capability not available";
    case TD_DBERR:
      return "debugger service failed";
    case TD_NOAPLIC:
      return "operation not applicable to";
    case TD_NOTSD:
      return "no thread-specific data for this thread";
    case TD_MALLOC:
      return "malloc failed";
    case TD_PARTIALREG:
      return "only part of register set was written/read";
    case TD_NOXREGS:
      return "X register set not available for this thread";
    default:
      snprintf (buf, sizeof (buf), "unknown thread_db error '%d'", err);
      return buf;
    }
}

char *
thread_db_state_str (td_thr_state_e state)
{
  static char buf[64];

  switch (state)
    {
    case TD_THR_STOPPED:
      return "stopped by debugger";
    case TD_THR_RUN:
      return "runnable";
    case TD_THR_ACTIVE:
      return "active";
    case TD_THR_ZOMBIE:
      return "zombie";
    case TD_THR_SLEEP:
      return "sleeping";
    case TD_THR_STOPPED_ASLEEP:
      return "stopped by debugger AND blocked";
    default:
      snprintf (buf, sizeof (buf), "unknown thread_db state %d", state);
      return buf;
    }
}


/* Convert between user-level thread ids and LWP ids.  */
static ptid_t
thread_from_lwp (ptid_t ptid)
{
    td_thrinfo_t ti;
    td_thrhandle_t th;
    td_err_e err;

    if (GET_LWP (ptid) == 0) {
        ptid = BUILD_LWP (GET_PID (ptid), GET_PID (ptid));
    }

    ASSERT (is_lwp (ptid));

    err = td_ta_map_lwp2thr (proc_info->thread_agent, GET_LWP (ptid), &th);
    if (err != TD_OK) {
        error ("Cannot find user-level thread for LWP %d: %s",
            GET_LWP (ptid), thread_db_err_str (err));
    }

    err = td_thr_get_info (&th, &ti);
    if (err != TD_OK) {
        error ("Cannot get thread info: %s", thread_db_err_str (err));
    }

    return BUILD_THREAD (ti.ti_tid, GET_PID (ptid));
}

static void
thrhandle_from_tid (const pthread_t tid, td_thrhandle_t *th)
{
    td_err_e err;

    err = td_ta_map_id2thr (proc_info->thread_agent, tid, th);
    if (err != TD_OK) {
        error ("Cannot find thread %ld: %s", tid, thread_db_err_str (err));
    }
}

/*
static ptid_t
lwp_from_thread (ptid_t ptid)
{
    td_thrinfo_t ti;
    td_thrhandle_t th;
    td_err_e err;

    if (!is_thread (ptid)) {
        return ptid;
    }

    err = td_ta_map_id2thr (proc_info->thread_agent, GET_THREAD (ptid), &th);
    if (err != TD_OK) {
        error ("Cannot find thread %ld: %s",
            (long) GET_THREAD (ptid), thread_db_err_str (err));
    }

    err = td_thr_get_info (&th, &ti);
    if (err != TD_OK) {
        error ("Cannot get thread info: %s", thread_db_err_str (err));
    }

    return BUILD_LWP (ti.ti_lid, GET_PID (ptid));
}
*/

static ptid_t
lwp_from_thrhandle (const td_thrhandle_t *th)
{
    td_thrinfo_t ti;
    td_err_e err;

    err = td_thr_get_info (th, &ti);
    if (err != TD_OK) {
        error ("Cannot get thread info: %s", thread_db_err_str (err));
    }
    return ptid_build (proc_info->pid, ti.ti_lid, ti.ti_tid);
}


pid_t
get_pid(thread_info_t thread_info)
{
    return IS_THREAD (thread_info)? GET_LWP (thread_info->ptid)
                                  : GET_PID (thread_info->ptid);
}


/* Tries to connect to pthread library. */
void 
try_to_connect_to_pthread_lib ()
{
    td_err_e err;
    thread_debug_info thr_info;
    struct ps_prochandle *ph;
    
    ph = (struct ps_prochandle *)malloc (sizeof (struct ps_prochandle));
    ph->proc_info = proc_info;

    register_cleanup (PCLEANUP, (cleanup_func_cb)&free, ph);

    // Now attempt to open a connection to the thread library.
    err = td_ta_new (ph, &proc_info->thread_agent);
    
    switch (err) {
      case TD_NOLIBTHREAD:
        // No thread library was detected.
        break;

      case TD_OK:
        // The thread library was detected.

        enable_thread_event_reporting ();
        proc_info->do_search_for_threads = xtrue;
        proc_info->check_events_again = xtrue;

        info ("Thread library detected");
        break;

      default:
        error ("Cannot initialize thread debugging library: %s\n", 
               thread_db_err_str (err));
        break;
    }
}



/* Creates thread event handler. Return false on errors. */
int
enable_thread_event_reporting ()
{
    // almost the whole method was got from gdb's sources,
    // see enable_thread_event_reporting ()

    td_thr_events_t events;
    td_notify_t notify;
    td_err_e err;

    // Set the process wide mask saying which events we're interested in.
    td_event_emptyset (&events);
    td_event_addset (&events, TD_CREATE);
    td_event_addset (&events, TD_DEATH);

    err = td_ta_set_event (proc_info->thread_agent, &events);
    if (err != TD_OK) {
        error ("Unable to set global thread event mask: %d", err);
        return 0;
    }

    // Delete previous thread event breakpoints, if any.
    remove_breakpoint (proc_info->thread_create_event);
    remove_breakpoint (proc_info->thread_death_event);

    // Get address for thread creation breakpoint.
    err = td_ta_event_addr (proc_info->thread_agent, TD_CREATE, &notify);
    if (err != TD_OK) {
        error ("Unable to get location for thread creation breakpoint: %d", err);
        return 0;
    }

    // Set up the breakpoint.
    set_breakpoint (proc_info->thread_create_event, (CORE_ADDR)notify.u.bptaddr);

    // Get address for thread death breakpoint.
    err = td_ta_event_addr (proc_info->thread_agent, TD_DEATH, &notify);
    if (err != TD_OK) {
        error ("Unable to get location for thread death breakpoint: %d", err);
        return 0;
    }

    // Set up the breakpoint.
    return set_breakpoint (proc_info->thread_death_event, (CORE_ADDR)notify.u.bptaddr);
}

/* Removes thread event handler. Return false on errors. */
int
disable_thread_event_reporting ()
{
    td_thr_events_t events;

    // Set the process wide mask saying we aren't interested in any
    // events anymore.
    td_event_emptyset (&events);
    td_ta_set_event (proc_info->thread_agent, &events);

    // Delete thread event breakpoints, if any.
    remove_breakpoint (proc_info->thread_create_event);
    remove_breakpoint (proc_info->thread_death_event);

    return 1;
}


/* Returns true if given process is stopped by the thread event handler. */
int 
check_thread_event (thread_info_t thread_info)
{
    CORE_ADDR address = (CORE_ADDR)proc_get_pc (thread_info) - BREAKPOINT_INSTR_LEN;

    return proc_info->thread_create_event->address == address ||
           proc_info->thread_death_event->address == address;
}


/* Finds proper thread_info_t object that correspond to given ptid. */
thread_info_t
find_thread_info_entry (pid_t pid)
{
    list_head_t * pos;
    thread_info_t thread_info;

    list_for_each (pos, &proc_info->threads) {
        thread_info = list_entry (pos, struct thread_info, list);

        if (GET_LWP (thread_info->ptid) == pid)
            return thread_info;
    }

    list_for_each (pos, &proc_info->threads) {
        thread_info = list_entry (pos, struct thread_info, list);

        if (GET_LWP (thread_info->ptid) == 0 &&
            GET_PID (thread_info->ptid) == pid) 
        {
            return thread_info;
        }
    }
    
    return 0;
}


/* Finds proper thread_info_t object that correspond to given ptid and
   satisfying conditiion COND. */
thread_info_t 
find_thread_info_entry_cond (pid_t pid, xbool (*cond)(thread_info_t, void*), void *data)
{
    list_head_t * pos;
    thread_info_t thread_info;

    if(!cond)
        return find_thread_info_entry(pid);

    list_for_each (pos, &proc_info->threads) {
        thread_info = list_entry (pos, struct thread_info, list);

        if (GET_LWP (thread_info->ptid) == pid && cond(thread_info, data))
            return thread_info;
    }

    list_for_each (pos, &proc_info->threads) {
        thread_info = list_entry (pos, struct thread_info, list);

        if (GET_LWP (thread_info->ptid) == 0 &&
            GET_PID (thread_info->ptid) == pid &&
            cond(thread_info, data)) 
        {
            return thread_info;
        }
    }
    
    return 0;
}


thread_info_t find_thread_info_if (xbool (*cond)(thread_info_t, void*), void *data)
{
    list_head_t * pos;
    thread_info_t thread_info;

    ASSERT(cond);

    list_for_each (pos, &proc_info->threads) {
        thread_info = list_entry (pos, struct thread_info, list);

        if (cond(thread_info, data))
            return thread_info;
    }

    return 0;
}


/* Destroys given thread_info_t object. */
static void 
free_thread_info_entry (thread_info_t * thread_info)
{
    if (thread_info == 0) {
        return;
    }

    free (thread_info);
}


/* Creates proper thread_info_t object that correspond to given ptid. */
thread_info_t 
create_thread_info_entry (pid_t pid)
{
    thread_info_t thread_info = (thread_info_t)malloc (sizeof (struct thread_info));
    memset (thread_info, 0, sizeof (struct thread_info));

    thread_info->ptid = BUILD_LWP (pid, proc_info->pid);

    list_add_tail (&thread_info->list, &proc_info->threads);
    
    register_cleanup (PCLEANUP, (cleanup_func_cb)free_thread_info_entry, thread_info);

    return thread_info;
}


xbool
update_td_thrhandle (thread_info_t thread_info, const td_thrhandle_t *td_thrhandle)
{
    td_thrhandle_t *new_td_thrhandle;
    td_err_e err;

    ASSERT (thread_info);

    if (thread_info->td_thrhandle != 0) {
        return;
    }

    new_td_thrhandle = (td_thrhandle_t*)malloc (sizeof (td_thrhandle_t));
    register_cleanup (PCLEANUP, (cleanup_func_cb)&free, new_td_thrhandle);

    *new_td_thrhandle = *td_thrhandle;

    thread_info->ptid         = lwp_from_thrhandle (new_td_thrhandle);
    thread_info->td_thrhandle = new_td_thrhandle;

    // Enable thread event reporting for this thread.
    err = td_thr_event_enable (new_td_thrhandle, 1);
    if (err != TD_OK) {
        error ("Cannot enable thread event reporting for lwp %d: %s", 
               GET_LWP (thread_info->ptid), thread_db_err_str (err));
        return xfalse;
    }
    return xtrue;
}




/* Unregisters given thread_info_t object. */
void
remove_thread_info_entry (thread_info_t thread_info)
{
    list_del (&thread_info->list);
}




static int 
update_thread_list_f (const td_thrhandle_t * td_thrhandle, void * data);



/* Updates proc_info's list of handled threads.
   As a result the [out]thread_info would contain either 0 or proper thread_info entry.
   Returns 0 if the list is consistent with the process' set of threads.
   Returns 1 if the thread_info entry was added to the list (thread created).
   Returns 2 if the thread_info entry was removed from the list (thread is dead). */
int update_thread_list (thread_info_t * result_thread_info)
{
    td_event_msg_t msg;
    td_thrinfo_t ti;
    td_err_e err;
    thread_info_t thread_info;
    td_thrhandle_t td_thrhandle;
    ptid_t ptid;

    *result_thread_info = 0;

    err = td_ta_event_getmsg (proc_info->thread_agent, &msg);

    if (err != TD_OK) {
        if (err == TD_NOMSG) {
            info ("update_thread_list: td_ta_event_getmsg: %s", 
                  thread_db_err_str (err));
            return 0;
        }
        error ("Cannot get thread event message: %s", thread_db_err_str (err));
        return 0;
    }
    info ("update_thread_list: td_ta_event_getmsg: inspecting thread message");

    err = td_thr_get_info (msg.th_p, &ti);
    if (err != TD_OK) {
        error ("Cannot get thread info: %s", thread_db_err_str (err));
        return 0;
    }

    switch (msg.event) {
      case TD_CREATE:
        if (err != TD_OK) {
            error ("TD_CREATE");
            break;
        }
        thread_info = find_thread_info_entry (ti.ti_lid);
        ASSERT (thread_info == 0);

        thread_info = create_thread_info_entry (ti.ti_lid);

        thrhandle_from_tid (ti.ti_tid, &td_thrhandle);
        if (!update_td_thrhandle (thread_info, &td_thrhandle)) {
            return 0;
        }

        ASSERT (thread_info != 0);
        info ("TD_CREATE, {pid=%d, lwp=%d, tid=%d}", GET_PID (thread_info->ptid), 
              GET_LWP (thread_info->ptid), ti.ti_tid);

//        printf ("TD_CREATE, {pid=%d, lwp=%d, tid=%d} ti_lid=%d\n", GET_PID (thread_info->ptid), 
//              GET_LWP (thread_info->ptid), ti.ti_tid, ti.ti_lid);

//        printf ("TD_CREATE, {pid=%d, lwp=%d, tid=%d}\n", GET_PID (thread_info->ptid), 
//              GET_LWP (thread_info->ptid), ti.ti_tid);

        *result_thread_info = thread_info;
        return 1;

      case TD_DEATH:
        if (err != TD_OK) {
            error ("TD_DEATH\n");
            break;
        }
        info("TD_DEATH: ti.ti_tid=%d ti.ti_lid=%d", ti.ti_tid, ti.ti_lid);

//        ptid = lwp_from_thread (BUILD_THREAD (ti.ti_tid, proc_info->pid));

//        if ( !(thread_info = find_thread_info_entry (GET_LWP (ptid)/*msg.th_p*/)) ) {
        if ( !(thread_info = find_thread_info_entry (ti.ti_lid)) ) {
            error ("Unknown thread info, {%d, %d, %d}", ptid);
            return 0;
        }
        info ("TD_DEATH, {pid=%d, lwp=%d, tid=%d}", GET_PID (thread_info->ptid), 
              GET_LWP (thread_info->ptid), ti.ti_tid);

//        printf ("TD_DEATH, {pid=%d, lwp=%d, tid=%d} ti_lid=%d\n", GET_PID (thread_info->ptid), 
//              GET_LWP (thread_info->ptid), ti.ti_tid, ti.ti_lid);

//        printf ("TD_DEATH, {pid=%d, lwp=%d, tid=%d}\n", GET_PID (thread_info->ptid), 
//              GET_LWP (thread_info->ptid), ti.ti_tid);

        // remove thread_info from the list
        remove_thread_info_entry (thread_info);
        thread_info->expired = xtrue;
        *result_thread_info = thread_info;
        return 2;

      default:
        error ("Spurious thread event.");
    }
    return 0;
}


struct search_for_unregistered_threads_data {

    /* Indicates if the search finished successfully. */
    uint_t unregistered_thread_found :1;

    /* The result of search. */
    thread_info_t thread_info;
};



/* Callback for iteration over threads.  */
static int
search_for_unregistered_threads_f (const td_thrhandle_t *th,
                                   struct search_for_unregistered_threads_data *data)
{
    thread_info_t thread_info;
    ptid_t ptid;

    if (data->unregistered_thread_found) {
        return 0;
    }
    ptid = lwp_from_thrhandle (th);

    thread_info = find_thread_info_entry (GET_LWP (ptid));

    if (thread_info == 0) {
        thread_info = create_thread_info_entry (GET_LWP (ptid));

        data->thread_info = thread_info;
        data->unregistered_thread_found = xtrue;
    }

    if (!update_td_thrhandle (thread_info, th)) {
        return 1;
    }
    return 0;
}

thread_info_t
search_for_unregistered_threads ()
{
    td_err_e err;
    struct search_for_unregistered_threads_data data;

    data.unregistered_thread_found = xfalse;

    err = td_ta_thr_iter (proc_info->thread_agent, 
                          (td_thr_iter_f*)search_for_unregistered_threads_f,
                          &data, TD_THR_ANY_STATE, 0, 0, 0);

    if (err != TD_OK) {
        error ("Iteration over threads failed: %s", thread_db_err_str (err));
        return 0;
    }

    if (data.unregistered_thread_found) {
        return data.thread_info;
    }
    return 0;
}



int 
attach_thread (thread_info_t thread_info)
{
    int status;
    long lwp = GET_TGTPID (thread_info->ptid);
    int pid;

    info ("%s is called for lwp=%d", __FUNCTION__, lwp);

    if (lwp == proc_info->pid) {
        thread_info->is_stopped = xtrue;
        // already attached through target_traceme ()
        return 1;
    }

    if (!target_attach (lwp)) {
        error ("cannot attach to lwp %d", lwp);
        return 0;
    }

    pid = target_waitpid (lwp, &status, 0);
    if (pid == -1 && errno == ECHILD) {
        // Try again with __WCLONE to check cloned processes.
        pid = target_waitpid (lwp, &status, __WCLONE);
//        lp->cloned = 1;
    }

    ASSERT (pid == GET_LWP (thread_info->ptid) && 
            WIFSTOPPED (status) && WSTOPSIG (status));

    thread_info->is_stopped = xtrue;

    return 1;
}

int 
detach_thread (thread_info_t thread_info)
{
    int status;
    long lwp = GET_TGTPID (thread_info->ptid);

    info ("%s is called", __FUNCTION__);

    if (!target_detach (lwp)) {
        error ("cannot detach to lwp %d", lwp);
        return 0;
    }
    return 1;
}


/* Returns one of RSDZTW chars. And E for error. */
char
get_proc_state (pid_t pid)
{
    FILE *fstat;
    char name[1024];

    pid_t fstat_pid = 0;
    char state = 'E';

    sprintf (name, "/proc/%d/stat", pid);
    fstat = fopen (name, "r");
    if (fstat == 0) {
        return 'E';
    }
    fscanf (fstat, "%d %s %c", &fstat_pid, name, &state);
    if (fstat_pid != pid) {
        return 'E';
    }
    fclose (fstat);
    return state;
}


static void
ensure_stopped (pid_t pid, struct event ** queue)
{
    const int stopsig = SIGTRAP;

    struct event * event = NULL;

    pid_t pid2;
    int status;
    char state;
    int wres, s;
    xbool first_shot;

    state = get_proc_state (pid);
    
    info ("ensure_stopped: state '%c' pid=%d", state, pid);

    switch (state) {

        case STATE_TRACED: {

          pid_t signaled_pid = get_pid(proc_info->last_signaled_thread);

          if(!proc_info->singlestep && pid != signaled_pid) {
              wres = target_waitpid (pid, &status, WNOHANG | __WALL);
              
              if (wres == 0) {
                  // info ("%s: No child [pid=%d] available", __FUNCTION__, pid);
              }
              else if (wres < 0) {
                  error ("waitpid [pid=%d] error: %d", pid, errno);
              }
              else {
                  if (WIFEXITED(status)) {
                      // info ("Child [pid=%d] has exited. Exit status: %d", pid, WEXITSTATUS(status));
                  } else if (WIFSTOPPED(status)) {
                      info ("%s: [pid=%d] new event detected: stopped signal %d", __FUNCTION__, pid, WSTOPSIG(status));

                      event = create_event(find_thread_info_entry (pid));
                      event->thread_info->last_status = status;

                      if (WSTOPSIG(status)==SIGTRAP)
                         info ("Thread [pid=%d] has been stopped by SIGTRAP signal", pid);
                  } else if (WIFSIGNALED(status)) {
                      //  info ("Child [pid=%d] has signaled. Terminate signal: %d", pid, WTERMSIG(status));
                  }

                  add_event(queue, event);
              }
          }

          break;
      }
    
      case STATE_RUNNING:
      case STATE_SLEEPING:
        if (kill (pid, stopsig) < 0) {
            error ("ensure_stopped: kill failed: %s", strerror(errno));
        }

        for(first_shot = xfalse;;) {
            if(!first_shot) {
                wres = target_waitpid (pid, &status, __WALL);

                if(wres < 0)
                    error ("waitpid [pid=%d] error: %d", pid, errno);

                first_shot = xtrue;
            } else {
                wres = target_waitpid (pid, &status, __WALL | WNOHANG);

                if(wres <= 0) {
                    if(wres < 0) {
                        error ("waitpid [pid=%d] error: %d", pid, errno);
                    }
                    break;
                }
            }

            if (!WIFSTOPPED (status)) {                                            
                // it didn't stop, strange...
                error ("ensure_stopped check failed: couldn't stop process (thread)");
            }

            if ((s = WSTOPSIG(status)) != stopsig) {
                // it seems that a signal has arrived during time window
                // between 'get_proc_state' and 'kill' calls
                info ("ensure_stopped: stopsignal (%d) is not the sent one (%d)",
                        s, stopsig);

                // so we must catch and process it later
                event = create_event(find_thread_info_entry (pid));
                event->thread_info->last_status = status;
                add_event(queue, event);
            } else {
                break;
            }
        }

        if ((state = get_proc_state (pid)) != 'T') {
            error ("ensure_stopped check failed: unexpected state %c\n", state);
        }

        break;

      case STATE_ZOMBIE: {
/*
          info ("ensure_stopped: pid=%d: ZOMBIE", pid);

          if (queue && !pid_in_queue(*queue, pid)) {
               info ("ensure_stopped: pid=%d: adding to queue", pid);
               add_event(queue, create_event(find_thread_info_entry (pid)));

          }
*/
          break;
      }
    }
}



/* Iterates over all known threads and ensures they are in T (traced) state.
   If there are undetected events, it stores them into queue. 
 */
void
stop_all_threads (struct event ** events)
{
    list_head_t * pos;
    thread_info_t thread_info;
    pid_t pid;

    list_for_each (pos, &proc_info->threads) {
        thread_info = list_entry (pos, struct thread_info, list);
        ensure_stopped (get_pid(thread_info), events);
    }
}

        
/* Iterates over all known threads and calls target_cont for each of them. */
struct event * 
resume_all_threads ()
{
    list_head_t * pos;
    thread_info_t thread_info;
    pid_t pid;
    char state;
    int singlestep = proc_info->singlestep;
    thread_info_t last_signaled_thread = proc_info->last_signaled_thread;
    struct event * events = NULL;
    int waitres, status;

    info ("%s: singlestep=%d", __FUNCTION__, singlestep);

    proc_info->maps_file_consistent = xfalse;

    if (!singlestep) {
        list_for_each (pos, &proc_info->threads) {
            thread_info = list_entry (pos, struct thread_info, list);

            pid = get_pid(thread_info);
            state = get_proc_state (pid);

            info ("%s: pid=%d state=%c last signal=%d", __FUNCTION__, pid, state, thread_info->last_signal);

            if (thread_info == last_signaled_thread)  {
                info ("%s: thread_info == last_signaled_thread, skipping", __FUNCTION__);
                continue;
            }

            if (state == 'T') {
                thread_info->last_status = -1;
                
                if ( !target_cont (pid, singlestep, thread_info->last_signal) ) {
                    error ("%s: failed to resume lwp=%d", __FUNCTION__, pid);
                }
        
                thread_info->last_signal = TARGET_SIGNAL_0;

            } else if (state != 'D') {
                // ignore state 'D' for a while...
                warning ("unexpected state for pid=%d, '%c'", pid, state);
            }
        }
    }

    /* and last, resume the signaled thread */
    if(last_signaled_thread != NULL) {
        thread_info = last_signaled_thread;
        pid = get_pid(thread_info);
        state = get_proc_state (pid);

        info ("%s [signaled thread]: pid=%d state=%c last signal=%d", __FUNCTION__, pid, state, thread_info->last_signal);
        if (state == 'T') {
            if ( !target_cont (pid, singlestep, thread_info->last_signal) ) {
                error ("%s: failed to resume lwp=%d", __FUNCTION__, pid);
            }

            thread_info->last_signal = TARGET_SIGNAL_0;
        } else {
            warning ("unexpected state for pid=%d, '%c'", pid, state);
        }
    } else {
        info ("%s: last_signaled_thread==NULL", __FUNCTION__);
    }

    return events;
}


int
terminate_all_threads ()
{
    list_head_t * pos;
    thread_info_t thread_info;
    pid_t pid;

    list_for_each (pos, &proc_info->threads) {
        thread_info = list_entry (pos, struct thread_info, list);
        pid = get_pid(thread_info);
        if (thread_info != proc_info->main_thread) {
            char state = get_proc_state (pid);
            if (state == 'Z')
                target_waitpid (pid, NULL, __WALL);
            else if (state != 'E') {
                target_detach (pid);
                kill (pid, SIGKILL);
            }
        } else {
            target_kill (pid);
        }
    }
    wait (0);
    return 0;
}


void print_threads_states()
{
    list_head_t * pos;
    thread_info_t thread_info;
    pid_t pid;

    list_for_each (pos, &proc_info->threads) {
        thread_info = list_entry (pos, struct thread_info, list);
        pid = get_pid(thread_info);
        info("%s: Checking thread [pid=%d] state: %c", __FUNCTION__, pid, get_proc_state (pid));
    }
}


