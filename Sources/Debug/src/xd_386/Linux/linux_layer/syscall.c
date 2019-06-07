/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* Syscall magic */

#include <sys/ptrace.h>
#include <asm/ptrace.h>
#include <sys/user.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <limits.h>
#include <unistd.h>

#include "syscall.h"
#include "linux-low.h"


#define TRACE_FILE      0x01   /* Trace file-related syscalls. */
#define TRACE_IPC       0x02   /* Trace IPC-related syscalls. */
#define TRACE_NETWORK   0x04   /* Trace network-related syscalls. */
#define TRACE_PROCESS   0x08   /* Trace process-related syscalls. */
#define TRACE_SIGNAL    0x10   /* Trace signal-related syscalls. */

#define TRACE_NONRETURN 0x20
#define TRACE_CRITICAL  0x40

#define TF TRACE_FILE
#define TI TRACE_IPC
#define TN TRACE_NETWORK
#define TP TRACE_PROCESS
#define TS TRACE_SIGNAL

#define TNR TRACE_NONRETURN
#define TCR TRACE_CRITICAL


struct sysent {
    int  nargs;
    int  flags;
    char *name;
    void (*filler)(pid_t, struct syscall_status*);
    long int (*func)(pid_t);
};


//-----------------------------------------------------------------------------------


static long int xd_ptrace  (pid_t pid);
static long int xd_waitpid (pid_t pid);
static long int xd_wait4   (pid_t pid);

static inline long int arg1(pid_t pid) { return target_get_reg(pid, EBX); }
static inline long int arg2(pid_t pid) { return target_get_reg(pid, ECX); }
static inline long int arg3(pid_t pid) { return target_get_reg(pid, EDX); }
static inline long int arg4(pid_t pid) { return target_get_reg(pid, ESI); }
static inline long int arg5(pid_t pid) { return target_get_reg(pid, EDI); }

static void chdir_filler(pid_t pid, struct syscall_status * status);
static void chdir_cleanup(struct syscall_status * status);
static void execve_filler(pid_t pid, struct syscall_status * status);
static void execve_cleanup(struct syscall_status * status);

static long int get_syscall_number(pid_t pid);
static long int get_syscall_state(pid_t pid);


//-----------------------------------------------------------------------------------

    
static struct sysent syscalls[] = {
  /* 0 */   { 0,    0,         "setup",                  NULL, NULL },
  /* 1 */   { 1,    TP,        "exit",                   NULL, NULL },
  /* 2 */   { 0,    TP,        "fork",                   NULL, NULL },
  /* 3 */   { 3,    0,         "read",                   NULL, NULL },
  /* 4 */   { 3,    0,         "write",                  NULL, NULL },
  /* 5 */   { 3,    TF,        "open",                   NULL, NULL },
  /* 6 */   { 1,    0,         "close",                  NULL, NULL },
  /* 7 */   { 3,    TP|TCR,    "waitpid",                NULL, xd_waitpid  },
  /* 8 */   { 2,    TF,        "creat",                  NULL, NULL },
  /* 9 */   { 2,    TF,        "link",                   NULL, NULL },
  /* 10 */  { 1,    TF,        "unlink",                 NULL, NULL },
  /* 11 */  { 3,    TF|TP|TNR, "execve",                 execve_filler, NULL },
  /* 12 */  { 1,    TF,        "chdir",                  NULL, NULL },
  /* 13 */  { 1,    0,         "time",                   NULL, NULL },
  /* 14 */  { 3,    TF,        "mknod",                  NULL, NULL },
  /* 15 */  { 2,    TF,        "chmod",                  NULL, NULL },
  /* 16 */  { 3,    TF,        "lchown",                 NULL, NULL },
  /* 17 */  { 0,    0,         "break",                  NULL, NULL },
  /* 18 */  { 2,    TF,        "oldstat",                NULL, NULL },
  /* 19 */  { 3,    0,         "lseek",                  NULL, NULL },
  /* 20 */  { 0,    0,         "getpid",                 NULL, NULL },
  /* 21 */  { 5,    TF,        "mount",                  NULL, NULL },
  /* 22 */  { 1,    TF,        "umount",                 NULL, NULL },
  /* 23 */  { 1,    0,         "setuid",                 NULL, NULL },
  /* 24 */  { 0,    0,         "getuid",                 NULL, NULL },
  /* 25 */  { 1,    0,         "stime",                  NULL, NULL },
  /* 26 */  { 4,    TCR,       "ptrace",                 NULL, xd_ptrace  },
  /* 27 */  { 1,    0,         "alarm",                  NULL, NULL },
  /* 28 */  { 2,    0,         "oldfstat",               NULL, NULL },
  /* 29 */  { 0,    TS,        "pause",                  NULL, NULL },
  /* 30 */  { 2,    TF,        "utime",                  NULL, NULL },
  /* 31 */  { 2,    0,         "stty",                   NULL, NULL },
  /* 32 */  { 2,    0,         "gtty",                   NULL, NULL },
  /* 33 */  { 2,    TF,        "access",                 NULL, NULL },
  /* 34 */  { 1,    0,         "nice",                   NULL, NULL },
  /* 35 */  { 0,    0,         "ftime",                  NULL, NULL },
  /* 36 */  { 0,    0,         "sync",                   NULL, NULL },
  /* 37 */  { 2,    TS,        "kill",                   NULL, NULL },
  /* 38 */  { 2,    TF,        "rename",                 NULL, NULL },
  /* 39 */  { 2,    TF,        "mkdir",                  NULL, NULL },
  /* 40 */  { 1,    TF,        "rmdir",                  NULL, NULL },
  /* 41 */  { 1,    0,         "dup",                    NULL, NULL },
  /* 42 */  { 1,    0,         "pipe",                   NULL, NULL },
  /* 43 */  { 1,    0,         "times",                  NULL, NULL },
  /* 44 */  { 0,    0,         "prof",                   NULL, NULL },
  /* 45 */  { 1,    0,         "brk",                    NULL, NULL },
  /* 46 */  { 1,    0,         "setgid",                 NULL, NULL },
  /* 47 */  { 0,    0,         "getgid",                 NULL, NULL },
  /* 48 */  { 3,    TS,        "signal",                 NULL, NULL },
  /* 49 */  { 0,    0,         "geteuid",                NULL, NULL },
  /* 50 */  { 0,    0,         "getegid",                NULL, NULL },
  /* 51 */  { 1,    TF,        "acct",                   NULL, NULL },
  /* 52 */  { 2,    TF,        "umount2",                NULL, NULL },
  /* 53 */  { 0,    0,         "lock",                   NULL, NULL },
  /* 54 */  { 3,    0,         "ioctl",                  NULL, NULL },
  /* 55 */  { 3,    0,         "fcntl",                  NULL, NULL },
  /* 56 */  { 0,    0,         "mpx",                    NULL, NULL },
  /* 57 */  { 2,    0,         "setpgid",                NULL, NULL },
  /* 58 */  { 2,    0,         "ulimit",                 NULL, NULL },
  /* 59 */  { 1,    0,         "oldolduname",            NULL, NULL },
  /* 60 */  { 1,    0,         "umask",                  NULL, NULL },
  /* 61 */  { 1,    TF,        "chroot",                 NULL, NULL },
  /* 62 */  { 2,    0,         "ustat",                  NULL, NULL },
  /* 63 */  { 2,    0,         "dup2",                   NULL, NULL },
  /* 64 */  { 0,    0,         "getppid",                NULL, NULL },
  /* 65 */  { 0,    0,         "getpgrp",                NULL, NULL },
  /* 66 */  { 0,    0,         "setsid",                 NULL, NULL },
  /* 67 */  { 3,    TS,        "sigaction",              NULL, NULL },
  /* 68 */  { 0,    TS,        "sgetmask",               NULL, NULL },
  /* 69 */  { 1,    TS,        "ssetmask",               NULL, NULL },
  /* 70 */  { 2,    0,         "setreuid",               NULL, NULL },
  /* 71 */  { 2,    0,         "setregid",               NULL, NULL },
  /* 72 */  { 3,    TS|TNR,    "sigsuspend",             NULL, NULL },
  /* 73 */  { 1,    TS,        "sigpending",             NULL, NULL },
  /* 74 */  { 2,    0,         "sethostname",            NULL, NULL },
  /* 75 */  { 2,    0,         "setrlimit",              NULL, NULL },
  /* 76 */  { 2,    0,         "getrlimit",              NULL, NULL },
  /* 77 */  { 2,    0,         "getrusage",              NULL, NULL },
  /* 78 */  { 2,    0,         "gettimeofday",           NULL, NULL },
  /* 79 */  { 2,    0,         "settimeofday",           NULL, NULL },
  /* 80 */  { 2,    0,         "getgroups",              NULL, NULL },
  /* 81 */  { 2,    0,         "setgroups",              NULL, NULL },
  /* 82 */  { 1,    0,         "select",                 NULL, NULL },
  /* 83 */  { 2,    TF,        "symlink",                NULL, NULL },
  /* 84 */  { 2,    TF,        "oldlstat",               NULL, NULL },
  /* 85 */  { 3,    TF,        "readlink",               NULL, NULL },
  /* 86 */  { 1,    TF,        "uselib",                 NULL, NULL },
  /* 87 */  { 1,    TF,        "swapon",                 NULL, NULL },
  /* 88 */  { 3,    0,         "reboot",                 NULL, NULL },
  /* 89 */  { 3,    0,         "readdir",                NULL, NULL },
  /* 90 */  { 6,    0,         "mmap",                   NULL, NULL },
  /* 91 */  { 2,    0,         "munmap",                 NULL, NULL },
  /* 92 */  { 2,    TF,        "truncate",               NULL, NULL },
  /* 93 */  { 2,    0,         "ftruncate",              NULL, NULL },
  /* 94 */  { 2,    0,         "fchmod",                 NULL, NULL },
  /* 95 */  { 3,    0,         "fchown",                 NULL, NULL },
  /* 96 */  { 2,    0,         "getpriority",            NULL, NULL },
  /* 97 */  { 3,    0,         "setpriority",            NULL, NULL },
  /* 98 */  { 4,    0,         "profil",                 NULL, NULL },
  /* 99 */  { 2,    TF,        "statfs",                 NULL, NULL },
  /* 100 */ { 2,    0,         "fstatfs",                NULL, NULL },
  /* 101 */ { 3,    0,         "ioperm",                 NULL, NULL },
  /* 102 */ { 2,    0,         "socketcall",             NULL, NULL },
  /* 103 */ { 3,    0,         "syslog",                 NULL, NULL },
  /* 104 */ { 3,    0,         "setitimer",              NULL, NULL },
  /* 105 */ { 2,    0,         "getitimer",              NULL, NULL },
  /* 106 */ { 2,    TF,        "stat",                   NULL, NULL },
  /* 107 */ { 2,    TF,        "lstat",                  NULL, NULL },
  /* 108 */ { 2,    0,         "fstat",                  NULL, NULL },
  /* 109 */ { 1,    0,         "olduname",               NULL, NULL },
  /* 110 */ { 1,    0,         "iopl",                   NULL, NULL },
  /* 111 */ { 0,    0,         "vhangup",                NULL, NULL },
  /* 112 */ { 0,    0,         "idle",                   NULL, NULL },
  /* 113 */ { 1,    0,         "vm86old",                NULL, NULL },
  /* 114 */ { 4,    TP|TCR,    "wait4",                  NULL, xd_wait4 },
  /* 115 */ { 1,    0,         "swapoff",                NULL, NULL },
  /* 116 */ { 1,    0,         "sysinfo",                NULL, NULL },
  /* 117 */ { 6,    0,         "ipc",                    NULL, NULL },
  /* 118 */ { 1,    0,         "fsync",                  NULL, NULL },
  /* 119 */ { 1,    TS|TNR,    "sigreturn",              NULL, NULL },
  /* 120 */ { 5,    TP,        "clone",                  NULL, NULL },
  /* 121 */ { 2,    0,         "setdomainname",          NULL, NULL },
  /* 122 */ { 1,    0,         "uname",                  NULL, NULL },
  /* 123 */ { 3,    0,         "modify_ldt",             NULL, NULL },
  /* 124 */ { 1,    0,         "adjtimex",               NULL, NULL },
  /* 125 */ { 3,    0,         "mprotect",               NULL, NULL },
  /* 126 */ { 3,    TS,        "sigprocmask",            NULL, NULL },
  /* 127 */ { 2,    0,         "create_module",          NULL, NULL },
  /* 128 */ { 2,    0,         "init_module",            NULL, NULL },
  /* 129 */ { 1,    0,         "delete_module",          NULL, NULL },
  /* 130 */ { 1,    0,         "get_kernel_syms",        NULL, NULL },
  /* 131 */ { 4,    0,         "quotactl",               NULL, NULL },
  /* 132 */ { 1,    0,         "getpgid",                NULL, NULL },
  /* 133 */ { 1,    0,         "fchdir",                 NULL, NULL },
  /* 134 */ { 0,    0,         "bdflush",                NULL, NULL },
  /* 135 */ { 3,    0,         "sysfs",                  NULL, NULL },
  /* 136 */ { 1,    0,         "personality",            NULL, NULL },
  /* 137 */ { 5,    0,         "afs_syscall",            NULL, NULL },
  /* 138 */ { 1,    0,         "setfsuid",               NULL, NULL },
  /* 139 */ { 1,    0,         "setfsgid",               NULL, NULL },
  /* 140 */ { 5,    0,         "_llseek",                NULL, NULL },
  /* 141 */ { 3,    0,         "getdents",               NULL, NULL },
  /* 142 */ { 5,    0,         "_newselect",             NULL, NULL },
  /* 143 */ { 2,    0,         "flock",                  NULL, NULL },
  /* 144 */ { 3,    0,         "msync",                  NULL, NULL },
  /* 145 */ { 3,    0,         "readv",                  NULL, NULL },
  /* 146 */ { 3,    0,         "writev",                 NULL, NULL },
  /* 147 */ { 1,    0,         "getsid",                 NULL, NULL },
  /* 148 */ { 1,    0,         "fdatasync",              NULL, NULL },
  /* 149 */ { 1,    0,         "_sysctl",                NULL, NULL },
  /* 150 */ { 1,    0,         "mlock",                  NULL, NULL },
  /* 151 */ { 2,    0,         "munlock",                NULL, NULL },
  /* 152 */ { 2,    0,         "mlockall",               NULL, NULL },
  /* 153 */ { 0,    0,         "munlockall",             NULL, NULL },
  /* 154 */ { 0,    0,         "sched_setparam",         NULL, NULL },
  /* 155 */ { 2,    0,         "sched_getparam",         NULL, NULL },
  /* 156 */ { 3,    0,         "sched_setscheduler",     NULL, NULL },
  /* 157 */ { 1,    0,         "sched_getscheduler",     NULL, NULL },
  /* 158 */ { 0,    0,         "sched_yield",            NULL, NULL },
  /* 159 */ { 1,    0,         "sched_get_priority_max", NULL, NULL },
  /* 160 */ { 1,    0,         "sched_get_priority_min", NULL, NULL },
  /* 161 */ { 2,    0,         "sched_rr_get_interval",  NULL, NULL },
  /* 162 */ { 2,    0,         "nanosleep",              NULL, NULL },
  /* 163 */ { 4,    0,         "mremap",                 NULL, NULL },
  /* 164 */ { 3,    0,         "setresuid",              NULL, NULL },
  /* 165 */ { 3,    0,         "getresuid",              NULL, NULL },
  /* 166 */ { 5,    0,         "vm86",                   NULL, NULL },
  /* 167 */ { 5,    0,         "query_module",           NULL, NULL },
  /* 168 */ { 3,    0,         "poll",                   NULL, NULL },
  /* 169 */ { 3,    0,         "nfsservctl",             NULL, NULL },
  /* 170 */ { 3,    0,         "setresgid",              NULL, NULL },
  /* 171 */ { 3,    0,         "getresgid",              NULL, NULL },
  /* 172 */ { 5,    0,         "prctl",                  NULL, NULL },
  /* 173 */ { 1,    TS|TNR,    "rt_sigreturn",           NULL, NULL },
  /* 174 */ { 4,    TS,        "rt_sigaction",           NULL, NULL },
  /* 175 */ { 4,    TS,        "rt_sigprocmask",         NULL, NULL },
  /* 176 */ { 2,    TS,        "rt_sigpending",          NULL, NULL },
  /* 177 */ { 4,    TS,        "rt_sigtimedwait",        NULL, NULL },
  /* 178 */ { 3,    TS,        "rt_sigqueueinfo",        NULL, NULL },
  /* 179 */ { 2,    TS|TNR,    "rt_sigsuspend",          NULL, NULL },
  /* 180 */ { 5,    TF,        "pread",                  NULL, NULL },
  /* 181 */ { 5,    TF,        "pwrite",                 NULL, NULL },
  /* 182 */ { 3,    TF,        "chown",                  NULL, NULL },
  /* 183 */ { 2,    TF,        "getcwd",                 NULL, NULL },
  /* 184 */ { 2,    0,         "capget",                 NULL, NULL },
  /* 185 */ { 2,    0,         "capset",                 NULL, NULL },
  /* 186 */ { 2,    TS,        "sigaltstack",            NULL, NULL },
  /* 187 */ { 4,    TF,        "sendfile",               NULL, NULL },
  /* 188 */ { 5,    0,         "getpmsg",                NULL, NULL },
  /* 189 */ { 5,    0,         "putpmsg",                NULL, NULL },
  /* 190 */ { 0,    TP,        "vfork",                  NULL, NULL },
  /* 191 */ { 2,    0,         "ugetrlimit",             NULL, NULL },
  /* 192 */ { 6,    0,         "mmap2",                  NULL, NULL },
  /* 193 */ { 2,    TF,        "truncate64",             NULL, NULL },
  /* 194 */ { 2,    TF,        "ftruncate64",            NULL, NULL },
  /* 195 */ { 2,    TF,        "stat64",                 NULL, NULL },
  /* 196 */ { 2,    TF,        "lstat64",                NULL, NULL },
  /* 197 */ { 2,    TF,        "fstat64",                NULL, NULL },
  /* 198 */ { 3,    TF,        "lchown32",               NULL, NULL },
  /* 199 */ { 0,    0,         "getuid32",               NULL, NULL },
  /* 200 */ { 0,    0,         "getgid32",               NULL, NULL },
  /* 201 */ { 0,    0,         "geteuid32",              NULL, NULL },
  /* 202 */ { 0,    0,         "getegid32",              NULL, NULL },
  /* 203 */ { 2,    0,         "setreuid32",             NULL, NULL },
  /* 204 */ { 2,    0,         "setregid32",             NULL, NULL },
  /* 205 */ { 2,    0,         "getgroups32",            NULL, NULL },
  /* 206 */ { 2,    0,         "setgroups32",            NULL, NULL },
  /* 207 */ { 3,    0,         "fchown32",               NULL, NULL },
  /* 208 */ { 3,    0,         "setresuid32",            NULL, NULL },
  /* 209 */ { 3,    0,         "getresuid32",            NULL, NULL },
  /* 210 */ { 3,    0,         "setresgid32",            NULL, NULL },
  /* 211 */ { 3,    0,         "getresgid32",            NULL, NULL },
  /* 212 */ { 3,    TF,        "chown32",                NULL, NULL },
  /* 213 */ { 1,    0,         "setuid32",               NULL, NULL },
  /* 214 */ { 1,    0,         "setgid32",               NULL, NULL },
  /* 215 */ { 1,    0,         "setfsuid32",             NULL, NULL },
  /* 216 */ { 1,    0,         "setfsgid32",             NULL, NULL },
  /* 217 */ { 2,    TF,        "pivot_root",             NULL, NULL },
  /* 218 */ { 3,    0,         "mincore",                NULL, NULL },
  /* 219 */ { 3,    0,         "madvise",                NULL, NULL },
  /* 220 */ { 4,    0,         "madvise1",               NULL, NULL },
  /* 221 */ { 3,    0,         "getdents64",             NULL, NULL },
  /* 222 */ { 4,    0,         "fcntl64",                NULL, NULL },
  /* 223 */ { 5,    0,         "security",               NULL, NULL },
  /* 224 */ { 0,    0,         "gettid",                 NULL, NULL },
  /* 225 */ { 4,    0,         "readahead",              NULL, NULL },
  /* 226 */ { 5,    TF,        "setxattr",               NULL, NULL },
  /* 227 */ { 5,    TF,        "lsetxattr",              NULL, NULL },
  /* 228 */ { 5,    0,         "fsetxattr",              NULL, NULL },
  /* 229 */ { 4,    TF,        "getxattr",               NULL, NULL },
  /* 230 */ { 4,    TF,        "lgetxattr",              NULL, NULL },
  /* 231 */ { 4,    0,         "fgetxattr",              NULL, NULL },
  /* 232 */ { 3,    TF,        "listxattr",              NULL, NULL },
  /* 233 */ { 3,    TF,        "llistxattr",             NULL, NULL },
  /* 234 */ { 3,    0,         "flistxattr",             NULL, NULL },
  /* 235 */ { 2,    TF,        "removexattr",            NULL, NULL },
  /* 236 */ { 2,    TF,        "lremovexattr",           NULL, NULL },
  /* 237 */ { 2,    0,         "fremovexattr",           NULL, NULL },
  /* 238 */ { 2,    TS,        "tkill",                  NULL, NULL },
  /* 239 */ { 4,    TF,        "sendfile64",             NULL, NULL },
  /* 240 */ { 4,    0,         "futex",                  NULL, NULL },
  /* 241 */ { 3,    0,         "sched_setaffinity",      NULL, NULL },
  /* 242 */ { 3,    0,         "sched_getaffinity",      NULL, NULL },
  /* 243 */ { 1,    0,         "set_thread_area",        NULL, NULL },
  /* 244 */ { 1,    0,         "get_thread_area",        NULL, NULL },
            { 0,    0,         NULL,                     NULL, NULL }
};


/////////////////////////////////////////////////////////////////////////////////

                                                   
struct syscall_status get_syscall_status(pid_t pid) {
    struct syscall_status status = { -1, 0, 0 };
    long int number = get_syscall_number(pid);

    if(syscall_is_valid(number)) {
        status.syscall = number;
        status.state = get_syscall_state(pid);

        if(syscalls[number].filler)
            (*syscalls[number].filler)(pid, &status);
    }

    return status;
}


const char * syscall_name(long int syscall) {                
    if(!syscall_is_valid(syscall))
         return NULL;

    return syscalls[syscall].name;
}                                                  
                                                   
                                                   
static long int get_syscall_number(pid_t pid)
{                                                  
    return target_get_reg(pid, ORIG_EAX);
}                                                  


xbool syscall_is_valid(long int syscall)
{
    return (syscall < sizeof(syscalls)/sizeof(syscalls[0])) && (syscall >= 0);
}


xbool syscall_is_critical(pid_t pid, long int syscall) {
    if(!syscall_is_valid(syscall))
         return 0;

    return find_thread_info_entry(pid) &&
           (syscalls[syscall].flags & TCR);
}


xbool syscall_is_nonreturnee(long int syscall) {
    if(!syscall_is_valid(syscall))
         return 0;

    return (syscalls[syscall].flags & TNR);
}


int execute_syscall(pid_t pid, long int syscall, long int * result) {
    if(!syscall_is_valid(syscall))
         return 0;

    ASSERT(result);

    if (syscalls[syscall].func) {
        *result = (*syscalls[syscall].func)(pid);
        return 1;
    }

    return 0;
}


int store_syscall_result(pid_t pid, long int value) {
    return target_set_reg (pid, EAX, value);
}


static long int get_syscall_state(pid_t pid) {
    return target_get_reg(pid, EAX);
}


int skip_syscall(pid_t pid) {
    return target_set_reg (pid, EAX, 0);
}


xbool syscall_is_reexec(pid_t pid, long int syscall) {
    return syscall == 11;
}


/////////////////////////////////////////////////////////////////////////////////


static long int xd_ptrace(pid_t pid) {
    long int request = arg1(pid);
    long int req_pid = arg2(pid);
    long int addr    = arg3(pid);
    long int data    = arg4(pid);
    int ret;

    /* Make sure that the process PID is already traced by us. */
    ASSERT(find_thread_info_entry(req_pid) != 0);

    info("%s: request: %d req_pid: %d addr: %#X data: %#X", __FUNCTION__, request, req_pid, addr, data);


    switch (request) {
        /* Ignore the following requests, because they will cause a failure. */
        case PTRACE_ATTACH:
        case PTRACE_DETACH:
        case PTRACE_TRACEME:
            return 0;

        case PTRACE_PEEKTEXT:
        case PTRACE_PEEKDATA:
        case PTRACE_PEEKUSER:
        case PTRACE_POKETEXT:
        case PTRACE_POKEDATA:
        case PTRACE_POKEUSER:
        case PTRACE_SYSCALL:
        case PTRACE_CONT:
        case PTRACE_KILL:
        case PTRACE_SINGLESTEP:
            ret = ptrace((int)request, (pid_t)req_pid, (void*)addr, (void*)data);
            break;

        /* For the following requests we need to create buffers
           for data/addr. Next, we'll copy contents of these buffers
           between PID's and REQ_PID's virtual memory spaces.
         */

        case PTRACE_GETREGS: {
            struct user_regs_struct regs;

            ret = ptrace(PTRACE_GETREGS, (pid_t)req_pid, NULL, &regs);

            if(ret != 0)
                break;

            if(target_set_memory(pid, &regs, sizeof(regs), (CORE_ADDR)data) == -1)
                error("%s (line %d): target_set_memory failed: %s", __FUNCTION__, __LINE__, strerror(errno));

            break;
        };

        case PTRACE_SETREGS: {
            struct user_regs_struct regs;

            if(target_get_memory(pid, (CORE_ADDR)data, sizeof(regs), &regs) == -1)
                error("%s (line %d): target_get_memory failed: %s", __FUNCTION__, __LINE__, strerror(errno));

            ret = ptrace(PTRACE_SETREGS, (pid_t)req_pid, NULL, &regs);
            break;
        };

        case PTRACE_GETFPREGS: {
            struct user_fpregs_struct regs;

            ret = ptrace(PTRACE_GETFPREGS, (pid_t)req_pid, NULL, &regs);

            if(ret != 0)
                break;

            if(target_set_memory(pid, &regs, sizeof(regs), (CORE_ADDR)data) == -1)
                error("%s (line %d): target_set_memory failed: %s", __FUNCTION__, __LINE__, strerror(errno));

            break;
        };

        case PTRACE_SETFPREGS: {
            struct user_fpregs_struct regs;

            if(target_get_memory(pid, (CORE_ADDR)data, sizeof(regs), &regs) == -1)
                error("%s (line %d): target_get_memory failed: %s", __FUNCTION__, __LINE__, strerror(errno));

            ret = ptrace(PTRACE_SETFPREGS, (pid_t)req_pid, NULL, &regs);
            break;
        };

        default:
            /* do nothing, the following ptrace call will do all black work */
            ret = ptrace(request, (pid_t)req_pid, (void*)addr, (void*)data);
    }

    return ret;
}


static long int xd_waitpid(pid_t pid) {
    long int req_pid = arg1(pid);
    long int statusp = arg2(pid);
    long int options = arg3(pid);
    
    info ("%s: req_pid: %d statusp: %#X options: %#X", 
          __FUNCTION__, req_pid, statusp, options);

    ASSERT(req_pid <= 0 || find_thread_info_entry(req_pid) != 0);

    return waitpid((pid_t)req_pid, (int*)statusp, (int)options);
}


static xbool not_pid_status_valid(thread_info_t thread_info, void *pid) {
    return (get_pid(thread_info) != (pid_t)pid) &&
           (thread_info->last_status != -1);
}


static long int xd_wait4(pid_t pid) {
    long int req_pid = arg1(pid);
    long int statusp = arg2(pid);
    long int options = arg3(pid);
    long int rusage  = arg4(pid);
    
    info ("%s: req_pid: %#X statusp: %#X options: %#X rusage: %#  X",
          __FUNCTION__, req_pid, statusp, options, rusage);

    if(req_pid < -1) {
        /* not implemented */
    }
    else if(req_pid == -1) {
        find_thread_info_entry_cond(req_pid, not_pid_status_valid, (void*)req_pid);
    }
    else if(req_pid == 0) {
        /* not implemented */
    }
    else { // >0
/*
        thread_info_t thread_info = find_thread_info_entry(req_pid);

        info("%s: found pid=%d last status: %#X", __FUNCTION__, get_pid(thread_info), thread_info->last_status);

        if(thread_info != 0)
            return thread_info->last_status;
*/
        return req_pid;
    }

    return wait4((pid_t)req_pid, (int*)statusp, (int)options, (struct rusage *)rusage);
}



//-----------------------------------------------------------------------------------
static void chdir_filler(pid_t pid, struct syscall_status * status)
{
    long int path_l = arg1(pid);
    char path[PATH_MAX];

    ASSERT(status->syscall == SYSCALL_CHDIR);
    ASSERT(status);

    info("%s called", __FUNCTION__);
    
    if(!target_get_memory (pid, (CORE_ADDR)path_l, PATH_MAX, path))
        error("%s (line %d): target_get_memory failed", __FUNCTION__, __LINE__);

    if(!(status->args.chdir_path = malloc(strlen(path)+1)))
        error("%s (line %d): memory out!!!", __FUNCTION__, __LINE__);

    strcpy(status->args.chdir_path, path);
    status->cleanup = chdir_cleanup;

    info("%s: path: '%s'", __FUNCTION__, status->args.chdir_path);
}


static void chdir_cleanup(struct syscall_status * status) {
    ASSERT(status->syscall == SYSCALL_CHDIR);
    ASSERT(status->args.chdir_path);

    info("%s called", __FUNCTION__);
    free(status->args.chdir_path);
    status->args.chdir_path = NULL;
}


//-----------------------------------------------------------------------------------
static void execve_filler(pid_t pid, struct syscall_status * status)
{
    long int file_l = arg1(pid),
             argv_l = arg2(pid),
             envp_l = arg3(pid),
             p;

    char filename[PATH_MAX],
         argument[_POSIX_ARG_MAX];

    char * ptr;

    char ** argv = NULL,
         ** envp = NULL;

    int argno, envno, i;

    ASSERT(status->syscall == SYSCALL_EXECVE);
    ASSERT(status);
    ASSERT(file_l && argv_l);
                     
    /* retrieve all information passed to execve
     */

    /* get filename */
    if(!target_get_memory (pid, file_l, PATH_MAX, filename))
        error("%s: target_get_memory failed", __FUNCTION__);

    info("%s: filename: '%s'", __FUNCTION__, filename);
                                                
    /* get argno & argv array */
    argno = 0;

    if(!(argv = (char**)malloc(sizeof(char*))))
        error("%s (line %d): memory out!!!", __FUNCTION__, __LINE__);

    for(p = argv_l;; p += sizeof(char*)) {
        if(!target_get_memory (pid, (CORE_ADDR)p, sizeof(char*), &ptr))
            error("%s (line %d): target_get_memory failed", __FUNCTION__, __LINE__);

        if(ptr != NULL) {
            if(!target_get_memory (pid, (CORE_ADDR)ptr, _POSIX_ARG_MAX, argument))
                error("%s (line %d): target_get_memory failed", __FUNCTION__, __LINE__);

            if(!(argv[argno] = malloc(strlen(argument)+1)))
                error("%s (line %d): memory out!!!", __FUNCTION__, __LINE__);

            strcpy(argv[argno], argument);
        }
        else {
            argv[argno] = NULL;
            break;
        }

        argv = realloc(argv, (++argno+1)*sizeof(char*));
    }

    ASSERT(argno); // must be at least one parameter (program name)

    /* get envno & envp array */
    envno = 0;

    if(envp_l) {
        if(!(envp = (char**)malloc(sizeof(char*))))
            error("%s (line %d): memory out!!!", __FUNCTION__, __LINE__);

        for(p = envp_l;; p += sizeof(char*)) {
            if(!target_get_memory (pid, (CORE_ADDR)p, sizeof(char*), &ptr))
                error("%s (line %d): target_get_memory failed", __FUNCTION__, __LINE__);

            if(ptr != NULL) {
                if(!target_get_memory (pid, (CORE_ADDR)ptr, _POSIX_ARG_MAX, argument))
                    error("%s (line %d): target_get_memory failed", __FUNCTION__, __LINE__);

                if(!(envp[envno] = malloc(strlen(argument)+1)))
                    error("%s (line %d): memory out!!!", __FUNCTION__, __LINE__);

                strcpy(envp[envno], argument);
            }
            else {
                envp[envno] = NULL;
                break;
            }

            envp = realloc(envp, (++envno+1)*sizeof(char*));
        }
    }

    if(!(status->args.execve_args.filename = malloc(strlen(filename)+1)))
        error("%s (line %d): memory out!!!", __FUNCTION__, __LINE__);

    strcpy(status->args.execve_args.filename, filename);

    status->args.execve_args.argv = argv;
    status->args.execve_args.envp = envp;
    status->cleanup = execve_cleanup;

    //for(i = 0; i < argno; ++i)
    //    info("%s: argv[%d]: '%s'", __FUNCTION__, i, status->args.execve_args.argv[i]);

    //for(i = 0; i < envno; ++i)
    //   info("%s: envp[%d]: '%s'", __FUNCTION__, i, status->args.execve_args.envp[i]);
}


static void execve_cleanup(struct syscall_status * status) {
    char ** ptr;

    ASSERT(status->syscall == SYSCALL_EXECVE);
    ASSERT(status->args.execve_args.argv);
    ASSERT(status->args.execve_args.filename);

    info("%s called", __FUNCTION__);

    free(status->args.execve_args.filename);

    for(ptr = status->args.execve_args.argv; *ptr; ++ptr) {
        ASSERT(*ptr);
        info("%s: freeing argv: '%s'", __FUNCTION__, *ptr);
        free(*ptr);
    }

    free(status->args.execve_args.argv);
    status->args.execve_args.argv = NULL;

    if(status->args.execve_args.envp) {
        for(ptr = status->args.execve_args.envp; *ptr; ++ptr) {
            ASSERT(*ptr);
            info("%s: freeing envp: '%s'", __FUNCTION__, *ptr);
            free(*ptr);
        }

        free(status->args.execve_args.envp);
        status->args.execve_args.envp = NULL;
    }
}

//-----------------------------------------------------------------------------------


/* Returns true if given process is stopped by the ptrace or waitpid event handler. */
int 
check_ptrace_event (thread_info_t thread_info)
{
    CORE_ADDR address = (CORE_ADDR)proc_get_pc (thread_info) - BREAKPOINT_INSTR_LEN;

    return proc_info->ptrace_event->address == address;
}

int 
check_waitpid_event (thread_info_t thread_info)
{
    CORE_ADDR address = (CORE_ADDR)proc_get_pc (thread_info) - BREAKPOINT_INSTR_LEN;

    return proc_info->waitpid_event->address == address;
}


/* Tries to connect to JET runtime library in order to catch calls to ptrace. */
int
try_to_connect_to_JET_runtime_lib ()
{
    list_head_t * head = &proc_info->loaded_solib;
    solib_t * last_loaded_solib;
    CORE_ADDR symaddr;
    bfd * abfd = 0;

//    info ("%s (%#x, %s, %s) called", __FUNCTION__, ph, obj, name);

    // check last loaded solib symbols
    if (head->prev == head) {
        error ("solib list is empty");
        return 0;
    }
    last_loaded_solib = list_entry (head->prev, solib_t, list);

    abfd = last_loaded_solib->abfd;

    if (abfd == 0) {
        error ("abfd was not initialized for (%s)", last_loaded_solib->name);
        return 0;
    }

    // try to lookup symbol
    symaddr = bfd_lookup_symbol (last_loaded_solib->abfd, "X2C_Ptrace");

    if (symaddr == 0) {
        return 0;
    }
    symaddr += last_loaded_solib->base;

printf ("!!!: X2C_Ptrace was found in '%s' at addr %#x\n", last_loaded_solib->name, symaddr);
    set_breakpointXX (proc_info->ptrace_event, symaddr, 16);


    // try to lookup symbol
    symaddr = bfd_lookup_symbol (last_loaded_solib->abfd, "X2C_Waitpid");

    if (symaddr == 0) {
        return 0;
    }
    symaddr += last_loaded_solib->base;

printf ("!!!: X2C_Waitpid was found in '%s' at addr %#x\n", last_loaded_solib->name, symaddr);
    set_breakpointXX (proc_info->waitpid_event, symaddr, 12);


    last_loaded_solib->runtime_solib = xtrue;

    return 1;
}


/* Emulate the result of call to X2C_Ptrace. */
xbool 
xd_ptrace2 (thread_info_t thread_info)
{
    pid_t pid = get_pid (thread_info);

    long int esp     = 0;
    long int request = 0;
    long int req_pid = 0;
    long int addr    = 0;
    long int data    = 0;
    int res = xtrue;
    int ret;

    esp = target_get_reg (pid, UESP);
/*
    ASSERT (target_get_memory (pid, esp+4,  4, &request) &&
            target_get_memory (pid, esp+8,  4, &req_pid) &&
            target_get_memory (pid, esp+12, 4, &addr) &&
            target_get_memory (pid, esp+16, 4, &data));
*/
    res &= target_get_memory (pid, esp+4,  4, &request);
    res &= target_get_memory (pid, esp+8,  4, &req_pid);
    res &= target_get_memory (pid, esp+12, 4, &addr);
    res &= target_get_memory (pid, esp+16, 4, &data);

    ASSERT (res);

    info("%s: esp=%#x, request: %d req_pid: %d addr: %#x data: %#x", __FUNCTION__, esp, request, req_pid, addr, data);

    switch (request) {
        /* Ignore the following requests, because they will cause a failure. */
        case PTRACE_ATTACH:
        case PTRACE_DETACH:
        case PTRACE_TRACEME:
            return 0;

        case PTRACE_PEEKTEXT:
        case PTRACE_PEEKDATA:
        case PTRACE_PEEKUSER:
        case PTRACE_POKETEXT:
        case PTRACE_POKEDATA:
        case PTRACE_POKEUSER:
        case PTRACE_SYSCALL:
        case PTRACE_CONT:
        case PTRACE_KILL:
        case PTRACE_SINGLESTEP:
            ret = ptrace((int)request, (pid_t)req_pid, (void*)addr, (void*)data);
            break;

        /* For the following requests we need to create buffers
           for data/addr. Next, we'll copy contents of these buffers
           between PID's and REQ_PID's virtual memory spaces.
         */

        case PTRACE_GETREGS: {
            struct user_regs_struct regs;

            ret = ptrace(PTRACE_GETREGS, (pid_t)req_pid, NULL, &regs);

            if(ret != 0)
                break;

            if(target_set_memory(pid, &regs, sizeof(regs), (CORE_ADDR)data) == -1)
                error("%s (line %d): target_set_memory failed: %s", __FUNCTION__, __LINE__, strerror(errno));

            break;
        };

        case PTRACE_SETREGS: {
            struct user_regs_struct regs;

            if(target_get_memory(pid, (CORE_ADDR)data, sizeof(regs), &regs) == -1)
                error("%s (line %d): target_get_memory failed: %s", __FUNCTION__, __LINE__, strerror(errno));

            ret = ptrace(PTRACE_SETREGS, (pid_t)req_pid, NULL, &regs);
            break;
        };

        case PTRACE_GETFPREGS: {
            struct user_fpregs_struct regs;

            ret = ptrace(PTRACE_GETFPREGS, (pid_t)req_pid, NULL, &regs);

            if(ret != 0)
                break;

            if(target_set_memory(pid, &regs, sizeof(regs), (CORE_ADDR)data) == -1)
                error("%s (line %d): target_set_memory failed: %s", __FUNCTION__, __LINE__, strerror(errno));

            break;
        };

        case PTRACE_SETFPREGS: {
            struct user_fpregs_struct regs;

            if(target_get_memory(pid, (CORE_ADDR)data, sizeof(regs), &regs) == -1)
                error("%s (line %d): target_get_memory failed: %s", __FUNCTION__, __LINE__, strerror(errno));

            ret = ptrace(PTRACE_SETFPREGS, (pid_t)req_pid, NULL, &regs);
            break;
        };

        default:
            /* do nothing, the following ptrace call will do all black work */
            ret = ptrace(request, (pid_t)req_pid, (void*)addr, (void*)data);
    }

    // store the result to the eax
    return target_set_reg (pid, EAX, ret);
}

/* Emulate the result of call to X2C_Waitpid. */
xbool 
xd_waitpid2 (thread_info_t thread_info)
{
    pid_t pid = get_pid (thread_info);

    // just return 0
    return target_set_reg (pid, EAX, 0);
}

