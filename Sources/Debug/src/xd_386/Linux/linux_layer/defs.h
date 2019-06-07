/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Frequently used stuff.  */

#ifndef _DEFS_H_
#define _DEFS_H_

#include <sys/procfs.h>
#include <sys/types.h>
#include "messages.h"


//typedef long lwpid_t;

/* The ptid struct is a collection of the various "ids" necessary
   for identifying the inferior.  This consists of the process id
   (pid), thread id (tid), and other fields necessary for uniquely
   identifying the inferior process/thread being debugged.  When
   manipulating ptids, the constructors, accessors, and predicate
   declared in inferior.h should be used.  These are as follows:

      ptid_build        - Make a new ptid from a pid, lwp, and tid.
      pid_to_ptid       - Make a new ptid from just a pid.
      ptid_get_pid      - Fetch the pid component of a ptid.
      ptid_get_lwp      - Fetch the lwp component of a ptid.
      ptid_get_tid      - Fetch the tid component of a ptid.
      ptid_equal        - Test to see if two ptids are equal.

   Please do NOT access the struct ptid members directly (except, of
   course, in the implementation of the above ptid manipulation
   functions).  */

struct ptid
{
    /* Process id */
    pid_t pid;

    /* Lightweight process id */
    lwpid_t lwp;

    /* Thread id */
    int tid;
};

typedef struct ptid ptid_t;


/* Provide default definitions of PIDGET, TIDGET, and MERGEPID.
   The name ``TIDGET'' is a historical accident.  Many uses of TIDGET
   in the code actually refer to a lightweight process id, i.e,
   something that can be considered a process id in its own right for
   certain purposes.  */
/*
#ifndef PIDGET
#define PIDGET(PTID) (ptid_get_pid (PTID))
#define TIDGET(PTID) (ptid_get_lwp (PTID))
#define MERGEPID(PID, TID) ptid_build (PID, TID, 0)
#define TGIDGET(PTID) (TIDGET(PTID) ?: PIDGET(PTID))
#endif
*/
/* Building process ids.  */

#define GET_PID(ptid)           ptid_get_pid (ptid)
#define GET_LWP(ptid)           ptid_get_lwp (ptid)
#define GET_THREAD(ptid)        ptid_get_tid (ptid)

#define GET_TGTPID(PTID)        (GET_LWP (PTID) ?: GET_PID (PTID))

#define is_lwp(ptid)            (GET_LWP (ptid) != 0)
#define is_thread(ptid)         (GET_THREAD (ptid) != 0)

#define BUILD_LWP(lwp, pid)     ptid_build (pid, lwp, 0)
#define BUILD_THREAD(tid, pid)  ptid_build (pid, 0, tid)



/* The address in debugee address space */
typedef unsigned long CORE_ADDR;


typedef char xbool;
#define xtrue 1
#define xfalse 0

typedef unsigned int uint_t;

typedef elf_greg_t prgreg_t;


/* The following for the benefit of computing offsets of structure members */
#ifndef offsetof
#  define offsetof(T, member)  ((char *)&(((T *)0)->member) - (char *)0)
#endif






#define ASSERT(c) if (!(c)) { assert (__FILE__, __LINE__); }
#define ASSERT_FALSE() assert (__FILE__, __LINE__);


#endif // _DEFS_H_
