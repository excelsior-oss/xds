/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Signals and their string representations. Taken from gdb's signals.c with 
   minor changes.  */

#include "signals.h"
#include <signal.h>

/* This table must match in order and size the signals in enum target_signal
   in target.h.  */
/* *INDENT-OFF* */
static struct {
  char *name;
  char *string;
  } signals [] =
{
  {"0", "Signal 0"},
  {"SIGHUP", "Hangup"},
  {"SIGINT", "Interrupt"},
  {"SIGQUIT", "Quit"},
  {"SIGILL", "Illegal instruction"},
  {"SIGTRAP", "Trace/breakpoint trap"},
  {"SIGABRT", "Aborted"},
  {"SIGBUS", "Bus error"},
  {"SIGFPE", "Arithmetic exception"},
  {"SIGKILL", "Killed"},
  {"SIGUSR1", "User defined signal 1"},
  {"SIGSEGV", "Segmentation fault"},
  {"SIGUSR2", "User defined signal 2"},
  {"SIGPIPE", "Broken pipe"},
  {"SIGALRM", "Alarm clock"},
  {"SIGTERM", "Terminated"},
  {"SIGSTKFLT", "Stack overflow??"},
  {"SIGCHLD", "Child status changed"},
  {"SIGCONT", "Continued"},
  {"SIGSTOP", "Stopped (signal)"},
  {"SIGTSTP", "Stopped (user)"},
  {"SIGTTIN", "Stopped (tty input)"},
  {"SIGTTOU", "Stopped (tty output)"},
  {"SIGURG", "Urgent I/O condition"},
  {"SIGXCPU", "CPU time limit exceeded"},
  {"SIGXFSZ", "File size limit exceeded"},
  {"SIGVTALRM", "Virtual timer expired"},
  {"SIGPROF", "Profiling timer expired"},
  {"SIGWINCH", "Window size changed"},
  {"SIGIO", "I/O possible"},
  {"SIGPWR", "Power fail/restart"},
  {"SIGSYS", "Bad system call"},

  {"SIG32", "Real-time event 32"},
  {"SIG33", "Real-time event 33"},
  {"SIG34", "Real-time event 34"},
  {"SIG35", "Real-time event 35"},
  {"SIG36", "Real-time event 36"},
  {"SIG37", "Real-time event 37"},
  {"SIG38", "Real-time event 38"},
  {"SIG39", "Real-time event 39"},
  {"SIG40", "Real-time event 40"},
  {"SIG41", "Real-time event 41"},
  {"SIG42", "Real-time event 42"},
  {"SIG43", "Real-time event 43"},
  {"SIG44", "Real-time event 44"},
  {"SIG45", "Real-time event 45"},
  {"SIG46", "Real-time event 46"},
  {"SIG47", "Real-time event 47"},
  {"SIG48", "Real-time event 48"},
  {"SIG49", "Real-time event 49"},
  {"SIG50", "Real-time event 50"},
  {"SIG51", "Real-time event 51"},
  {"SIG52", "Real-time event 52"},
  {"SIG53", "Real-time event 53"},
  {"SIG54", "Real-time event 54"},
  {"SIG55", "Real-time event 55"},
  {"SIG56", "Real-time event 56"},
  {"SIG57", "Real-time event 57"},
  {"SIG58", "Real-time event 58"},
  {"SIG59", "Real-time event 59"},
  {"SIG60", "Real-time event 60"},
  {"SIG61", "Real-time event 61"},
  {"SIG62", "Real-time event 62"},
  {"SIG63", "Real-time event 63"},

  {NULL, "Unknown signal"},

  /* Last entry, used to check whether the table is the right size.  */
  {NULL, "TARGET_SIGNAL_MAGIC"}
};
/* *INDENT-ON* */


static sigset_t normal_mask, blocked_mask;


int ignored_signals[__SIGRTMIN];


/* Return the string for a signal.  */
char *
target_signal_to_string (enum target_signal sig)
{
  if ((sig >= TARGET_SIGNAL_FIRST) && (sig <= TARGET_SIGNAL_LAST))
    return signals[sig].string;
  else
    return signals[TARGET_SIGNAL_UNKNOWN].string;
}


/* Return the name for a signal.  */
char *
target_signal_to_name (enum target_signal sig)
{
  if ((sig >= TARGET_SIGNAL_FIRST) && (sig <= TARGET_SIGNAL_LAST)
      && signals[sig].name != NULL)
    return signals[sig].name;
  else
    // I think the code which prints this will always print it along
    // with the string, so no need to be verbose (very old comment).
    return "?";
}

/* Given a name, return its signal.  */
enum target_signal
target_signal_from_name (char *name)
{
  enum target_signal sig;

  // It's possible we also should allow "SIGCLD" as well as "SIGCHLD"
  // for TARGET_SIGNAL_SIGCHLD.  SIGIOT, on the other hand, is more
  // questionable; seems like by now people should call it SIGABRT
  // instead.

  // This ugly cast brought to you by the native VAX compiler.
  for (sig = TARGET_SIGNAL_HUP;
       signals[sig].name != NULL;
       sig = (enum target_signal) ((int) sig + 1))
    if (strcmp (name, signals[sig].name) == 0)
      return sig;
  return TARGET_SIGNAL_UNKNOWN;
}


void
_initialize_signals (void)
{
    sigset_t mask;

    memset(ignored_signals, sizeof(ignored_signals), 0);

    /* block pesky SIGCHLD */
    sigemptyset(&mask);
    sigaddset(&mask, SIGCHLD);
    sigprocmask(SIG_BLOCK, &mask, NULL);
}

