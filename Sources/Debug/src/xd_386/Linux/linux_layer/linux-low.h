/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* This module provides low level interface to debugee process. */

#ifndef _LINUX_LOW_H_
#define _LINUX_LOW_H_

#include "defs.h"
#include "inferior.h"


/* The low level process IO functions.
   They return true on success and false on errors. */

extern int target_attach (pid_t pid);

extern int target_detach (pid_t pid);

extern int target_traceme ();

extern int target_kill (pid_t pid);

extern pid_t target_waitpid (pid_t pid, int *status, int options);

extern int target_cont (pid_t pid, int singlestep, int signal);

extern long target_get_reg (pid_t pid, int regno);

extern int target_set_reg (pid_t pid, int regno, long value);

extern int target_get_regs (pid_t pid, long *buffer);

extern int target_set_regs (pid_t pid, long *buffer);

extern int target_get_fregs (pid_t pid, long *buffer);

extern int target_set_fregs (pid_t pid, long *buffer);

extern int target_get_dbgregs (pid_t pid, long *buffer);

extern int target_set_dbgregs (pid_t pid, long *buffer);

extern int target_get_memory (pid_t pid, CORE_ADDR source, int size, void *dest);

extern int target_set_memory (pid_t pid, const void *source, int size, CORE_ADDR dest);

extern void dump_mem(pid_t pid, void * adr, size_t nbytes);


#endif // _LINUX_LOW_H_
