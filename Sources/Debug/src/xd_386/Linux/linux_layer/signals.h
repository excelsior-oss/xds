/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Signals and their string representations. Taken from gdb's signals.c with 
   minor changes.  */

#ifndef _SIGNALS_H_
#define _SIGNALS_H_

#include "defs.h"


/* The numbering of these signals is chosen to match traditional unix
   signals (insofar as various unices use the same numbers, anyway).
   It is also the numbering of the GDB remote protocol.  Other remote
   protocols, if they use a different numbering, should make sure to
   translate appropriately.

   Since these numbers have actually made it out into other software
   (stubs, etc.), you mustn't disturb the assigned numbering.  If you
   need to add new signals here, add them to the end of the explicitly
   numbered signals.

   This is based strongly on Unix/POSIX signals for several reasons:
   (1) This set of signals represents a widely-accepted attempt to
   represent events of this sort in a portable fashion, (2) we want a
   signal to make it from wait to child_wait to the user intact, (3) many
   remote protocols use a similar encoding.  However, it is
   recognized that this set of signals has limitations (such as not
   distinguishing between various kinds of SIGSEGV, or not
   distinguishing hitting a breakpoint from finishing a single step).
   So in the future we may get around this either by adding additional
   signals for breakpoint, single-step, etc., or by adding signal
   codes; the latter seems more in the spirit of what BSD, System V,
   etc. are doing to address these issues.  */

/* For an explanation of what each signal means, see
   target_signal_to_string.  */

enum target_signal {
    /* Used some places (e.g. stop_signal) to record the concept that
       there is no signal.  */
    TARGET_SIGNAL_0 = 0,
    TARGET_SIGNAL_FIRST = 0,
    TARGET_SIGNAL_HUP = 1,
    TARGET_SIGNAL_INT = 2,
    TARGET_SIGNAL_QUIT = 3,
    TARGET_SIGNAL_ILL = 4,
    TARGET_SIGNAL_TRAP = 5,
    TARGET_SIGNAL_ABRT = 6,
    TARGET_SIGNAL_BUS = 7,
    TARGET_SIGNAL_FPE = 8,
    TARGET_SIGNAL_KILL = 9,
    TARGET_SIGNAL_USR1 = 10,
    TARGET_SIGNAL_SEGV = 11,
    TARGET_SIGNAL_USR2 = 12,
    TARGET_SIGNAL_PIPE = 13,
    TARGET_SIGNAL_ALRM = 14,
    TARGET_SIGNAL_TERM = 15,
    TARGET_SIGNAL_STKFLT = 16,
    TARGET_SIGNAL_CHLD = 17,
    TARGET_SIGNAL_CONT = 18,
    TARGET_SIGNAL_STOP = 19,
    TARGET_SIGNAL_TSTP = 20,
    TARGET_SIGNAL_TTIN = 21,
    TARGET_SIGNAL_TTOU = 22,
    TARGET_SIGNAL_URG = 23,
    TARGET_SIGNAL_XCPU = 24,
    TARGET_SIGNAL_XFSZ = 25,
    TARGET_SIGNAL_VTALRM = 26,
    TARGET_SIGNAL_PROF = 27,
    TARGET_SIGNAL_WINCH = 28,
    TARGET_SIGNAL_IO = 29,
    TARGET_SIGNAL_PWR = 30,
    TARGET_SIGNAL_SYS = 31,

    TARGET_SIGNAL_REALTIME_32 = 32,
    TARGET_SIGNAL_REALTIME_33 = 33,
    TARGET_SIGNAL_REALTIME_34 = 34,
    TARGET_SIGNAL_REALTIME_35 = 35,
    TARGET_SIGNAL_REALTIME_36 = 36,
    TARGET_SIGNAL_REALTIME_37 = 37,
    TARGET_SIGNAL_REALTIME_38 = 38,
    TARGET_SIGNAL_REALTIME_39 = 39,
    TARGET_SIGNAL_REALTIME_40 = 40,
    TARGET_SIGNAL_REALTIME_41 = 41,
    TARGET_SIGNAL_REALTIME_42 = 42,
    TARGET_SIGNAL_REALTIME_43 = 43,
    TARGET_SIGNAL_REALTIME_44 = 44,
    TARGET_SIGNAL_REALTIME_45 = 45,
    TARGET_SIGNAL_REALTIME_46 = 46,
    TARGET_SIGNAL_REALTIME_47 = 47,
    TARGET_SIGNAL_REALTIME_48 = 48,
    TARGET_SIGNAL_REALTIME_49 = 49,
    TARGET_SIGNAL_REALTIME_50 = 50,
    TARGET_SIGNAL_REALTIME_51 = 51,
    TARGET_SIGNAL_REALTIME_52 = 52,
    TARGET_SIGNAL_REALTIME_53 = 53,
    TARGET_SIGNAL_REALTIME_54 = 54,
    TARGET_SIGNAL_REALTIME_55 = 55,
    TARGET_SIGNAL_REALTIME_56 = 56,
    TARGET_SIGNAL_REALTIME_57 = 57,
    TARGET_SIGNAL_REALTIME_58 = 58,
    TARGET_SIGNAL_REALTIME_59 = 59,
    TARGET_SIGNAL_REALTIME_60 = 60,
    TARGET_SIGNAL_REALTIME_61 = 61,
    TARGET_SIGNAL_REALTIME_62 = 62,
    TARGET_SIGNAL_REALTIME_63 = 63,
    
    /* Some signal we don't know about.  */
    TARGET_SIGNAL_UNKNOWN = 64,

    /* Last and unused enum value, for sizing arrays, etc.  */
    TARGET_SIGNAL_LAST
};


extern int ignored_signals[];


/* Return the string for a signal.  */
extern char * target_signal_to_string (enum target_signal sig);

/* Return the name for a signal.  */
extern char * target_signal_to_name (enum target_signal sig);

/* Given a name, return its signal.  */
extern enum target_signal target_signal_from_name (char *name);


extern void _initialize_signals ();



#endif // _SIGNALS_H_

