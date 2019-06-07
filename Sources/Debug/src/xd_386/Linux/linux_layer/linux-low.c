/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* This module provides low level interface to debugee process.  */

#include <sys/wait.h>

//#include <sys/user.h>

#include <sys/ptrace.h>

#include <stdio.h>
#include <errno.h>

#include "defs.h"
#include "linux-low.h"


/* The low level process IO functions.
   They return true on success and false on errors. */

int 
target_attach (pid_t pid)
{
    errno = 0;
    if (ptrace (PTRACE_ATTACH, pid, 0, 0)) {
        error ("PTRACE_ATTACH failed, pid=%d, %s", pid, strerror (errno));
        return 0;
    }
    return 1;
}

int 
target_detach (pid_t pid)
{
    errno = 0;
    if (ptrace (PTRACE_DETACH, pid, 0, 0)) {
        error ("PTRACE_DETACH failed, pid=%d, %s", pid, strerror (errno));
        return 0;
    }
    return 1;
}

int 
target_traceme ()
{
    if (ptrace (PTRACE_TRACEME, 0, 0, 0)) {
        error ("PTRACE_TRACEME failed, %s", strerror (errno));
        return 0;
    }
    return 1;
}


int 
target_kill (pid_t pid)
{
    if (ptrace (PTRACE_KILL, pid, 0, 0)) {
        error ("PTRACE_KILL failed, %s", strerror (errno));
        return 0;
    }
    return 1;
}


pid_t 
target_waitpid (pid_t pid, int *status, int options)
{
    return waitpid (pid, status, options);
}

int 
target_cont (pid_t pid, int singlestep, int signal)
{
    int status;

#if 0
    int cmd_cont = PTRACE_SYSCALL;
    char cmd_cont_s[] = "PTRACE_SYSCALL";
#else
    int cmd_cont = PTRACE_CONT;
    char cmd_cont_s[] = "PTRACE_CONT";
#endif

    info ("%s: pid=%d, step=%d, %s=%d: %s",
      __FUNCTION__,
      pid,
      singlestep,
      target_signal_to_name (signal), 
      signal, 
      target_signal_to_string (signal));

    if(ptrace (singlestep ? PTRACE_SINGLESTEP : cmd_cont, pid, 0, signal) < 0) {
        error ("%s failed, %s", 
               singlestep ? "PTRACE_SINGLESTEP" : cmd_cont_s, strerror (errno));
        return 0;
    }

    return 1;
}



long 
target_get_reg (pid_t pid, int regno)
{
    long val = ptrace(PTRACE_PEEKUSER, pid, regno * 4, 0);
    
    if (val == -1 && errno)
        error ("%s: ptrace(PTRACE_PEEKUSER) failed: %s", __FUNCTION__, strerror(errno));

    return val;
}



int 
target_set_reg (pid_t pid, int regno, long value)
{
    if (ptrace(PTRACE_POKEUSER, pid, regno * sizeof (long), value) == -1) {
        error ("PTRACE_POKEUSER failed, %s", strerror (errno));
        return 0;
    }
    return 1;
}

int 
target_get_regs (pid_t pid, long *buffer)
{
    if (ptrace(PTRACE_GETREGS, pid, 0, buffer)) {
        error ("target_get_regs: PTRACE_GETREGS failed for pid=%d, %s", pid, strerror (errno));
        return 0;
    }
    return 1;
}

int 
target_set_regs (pid_t pid, long *buffer)
{
    if (ptrace(PTRACE_SETREGS, pid, 0, buffer)) {
        error ("PTRACE_SETREGS failed for pid=%d, %s", pid, strerror (errno));
        return 0;
    }
    return 1;
}

int 
target_get_fregs (pid_t pid, long *buffer)
{
    if (ptrace(PTRACE_GETFPREGS, pid, 0, buffer)) {
        error ("PTRACE_GETFPREGS failed for pid=%d, %s", pid, strerror (errno));
        return 0;
    }
    return 1;
}

int 
target_set_fregs (pid_t pid, long *buffer)
{
    if (ptrace(PTRACE_SETFPREGS, pid, 0, buffer)) {
        error ("PTRACE_SETFPREGS failed for pid=%d, %s", pid, strerror (errno));
        return 0;
    }
    return 1;
}


int 
target_get_dbgregs (pid_t pid, long *buffer)
{
    const int offs = offsetof (struct user, u_debugreg);

    register int i;

    errno = 0;

    // Intel has 8 debug registers
    for (i = 0; i < 8; i++) {
        buffer[i] = ptrace(PTRACE_PEEKUSER, pid, offs + i*4, 0);
        if (buffer[i] == -1 && errno) {
            error ("PTRACE_PEEKUSER failed (dbg) for pid=%d, %s", pid, strerror (errno));
            return 0;
        }
    }
    return 1;
}

int 
target_set_dbgregs (pid_t pid, long *buffer)
{
    const int offs = offsetof (struct user, u_debugreg);
    register int i;

    // Intel has 8 debug registers
    for (i = 0; i < 8; i++) {
        if (ptrace(PTRACE_POKEUSER, pid, offs + i*4, &buffer[i]) == -1) {
            error ("PTRACE_POKEUSER failed (dbg) for pid=%d, %s", pid, strerror (errno));
            // return 0;
        }
    }
    return 1;
}



#define PTRACE_XFER_TYPE long
#define PTRACE_ARG3_TYPE long

int 
target_get_memory (pid_t pid, CORE_ADDR source, int size, void *dest)
{
    register int i;

    // Round starting address down to longword boundary.
    register CORE_ADDR addr = source & -(CORE_ADDR) sizeof (PTRACE_XFER_TYPE);

    // Round ending address up; get number of longwords that makes.
    register int count
      = (((source + size) - addr) + sizeof (PTRACE_XFER_TYPE) - 1)
       / sizeof (PTRACE_XFER_TYPE);

    // Allocate buffer of that many longwords.
    register PTRACE_XFER_TYPE *buffer
      = (PTRACE_XFER_TYPE *) alloca (count * sizeof (PTRACE_XFER_TYPE));

    // Read all the longwords
    errno = 0;
    for (i = 0; i < count; i++, addr += sizeof (PTRACE_XFER_TYPE)) {
        buffer[i] = ptrace (PTRACE_PEEKTEXT, pid, addr, 0);
    }

    // Copy appropriate bytes out of the buffer.
    memcpy (dest, (void *) buffer + (source & (sizeof (PTRACE_XFER_TYPE) - 1)), size);
    return 1;
}

int 
target_set_memory (pid_t pid, const void *source, int size, CORE_ADDR dest)
{
    register int i;

    // Round starting address down to longword boundary.
    register CORE_ADDR addr = dest & -(CORE_ADDR) sizeof (PTRACE_XFER_TYPE);

    // Round ending address up; get number of longwords that makes.
    register int count
        = (((dest + size) - addr) + sizeof (PTRACE_XFER_TYPE) - 1) / sizeof (PTRACE_XFER_TYPE);

    // Allocate buffer of that many longwords.
    register PTRACE_XFER_TYPE *buffer = (PTRACE_XFER_TYPE *) alloca (count * sizeof (PTRACE_XFER_TYPE));

    // Fill start and end extra bytes of buffer with existing memory data.
    buffer[0] = ptrace (PTRACE_PEEKTEXT, pid,
                        (PTRACE_ARG3_TYPE) addr, 0);

    if (count > 1) {
        buffer[count - 1] = ptrace (PTRACE_PEEKTEXT, pid,
                                    (PTRACE_ARG3_TYPE) (addr + (count - 1)
                                                        * sizeof (PTRACE_XFER_TYPE)),
                                    0);
    }

    // Copy data to be written over corresponding part of buffer
    memcpy ((char *) buffer + (dest & (sizeof (PTRACE_XFER_TYPE) - 1)), source, size);

    // Write the entire buffer.
    errno = 0;
    for (i = 0; i < count; i++, addr += sizeof (PTRACE_XFER_TYPE)) {
        if(ptrace (PTRACE_POKETEXT, pid, (PTRACE_ARG3_TYPE) addr, buffer[i]) == -1) {
            error ("PTRACE_POKETEXT failed for pid=%d, %s", pid, strerror (errno));
            error ("PTRACE_POKETEXT pid=%d addr=%#X (dest=%#X) data=%#X", pid, addr, dest, buffer[i]);
            error ("PTRACE_POKETEXT state='%c'", get_proc_state (pid));
            return 0;
        }
    }
    return 1;
}


void dump_mem(pid_t pid, void * adr, size_t nbytes)
{
    int i;
    unsigned char * buf = malloc(nbytes),
                  * str = malloc((nbytes+1)*3),
                  * s = str;

    target_get_memory (pid, (CORE_ADDR) adr, nbytes, buf);

    for(i = 0; i < nbytes; ++i, s+=3)
        sprintf(s, "%2X ", (unsigned int)buf[i]);

    *(--s) = '\0';

    info("Memory dump at 0x%X of process [pid=%d]: [%s]", adr, pid, str);

    free(buf);
    free(str);    
}

