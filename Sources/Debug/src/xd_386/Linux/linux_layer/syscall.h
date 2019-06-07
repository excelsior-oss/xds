/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* This module provides low level interface to Linux system calls. */

#ifndef _SYSCALL_H_
#define _SYSCALL_H_


#include "defs.h"


#define SYSCALL_INSTR_LEN    2


enum {
    SYSCALL_SETUP = 0,
    SYSCALL_EXIT,
    SYSCALL_FORK,
    SYSCALL_READ,
    SYSCALL_WRITE,
    SYSCALL_OPEN,
    SYSCALL_CLOSE,
    SYSCALL_WAITPID,
    SYSCALL_CREAT,
    SYSCALL_LINK,
    SYSCALL_UNLINK,
    SYSCALL_EXECVE,
    SYSCALL_CHDIR,
    SYSCALL_TIME,
    SYSCALL_MKNOD,
    SYSCALL_CHMOD,
    SYSCALL_LCHOWN,
    SYSCALL_BREAK,
    SYSCALL_OLDSTAT,
    SYSCALL_LSEEK,
    SYSCALL_GETPID,
    SYSCALL_MOUNT,
    SYSCALL_UMOUNT,
    SYSCALL_SETUID,
    SYSCALL_GETUID,
    SYSCALL_STIME,
    SYSCALL_PTRACE,
    SYSCALL_ALARM,
    SYSCALL_OLDFSTAT,
    SYSCALL_PAUSE,
    SYSCALL_UTIME,
    SYSCALL_STTY,
    SYSCALL_GTTY,
    SYSCALL_ACCESS,
    SYSCALL_NICE,
    SYSCALL_FTIME,
    SYSCALL_SYNC,
    SYSCALL_KILL,
    SYSCALL_RENAME,
    SYSCALL_MKDIR,
    SYSCALL_RMDIR,
    SYSCALL_DUP,
    SYSCALL_PIPE,
    SYSCALL_TIMES,
    SYSCALL_PROF,
    SYSCALL_BRK,
    SYSCALL_SETGID,
    SYSCALL_GETGID,
    SYSCALL_SIGNAL,
    SYSCALL_GETEUID,
    SYSCALL_GETEGID,
    SYSCALL_ACCT,
    SYSCALL_UMOUNT2,
    SYSCALL_LOCK,
    SYSCALL_IOCTL,
    SYSCALL_FCNTL,
    SYSCALL_MPX,
    SYSCALL_SETPGID,
    SYSCALL_ULIMIT,
    SYSCALL_OLDOLDUNAME,
    SYSCALL_UMASK,
    SYSCALL_CHROOT,
    SYSCALL_USTAT,
    SYSCALL_DUP2,
    SYSCALL_GETPPID,
    SYSCALL_GETPGRP,
    SYSCALL_SETSID,
    SYSCALL_SIGACTION,
    SYSCALL_SGETMASK,
    SYSCALL_SSETMASK,
    SYSCALL_SETREUID,
    SYSCALL_SETREGID,
    SYSCALL_SIGSUSPEND,
    SYSCALL_SIGPENDING,
    SYSCALL_SETHOSTNAME,
    SYSCALL_SETRLIMIT,
    SYSCALL_GETRLIMIT,
    SYSCALL_GETRUSAGE,
    SYSCALL_GETTIMEOFDAY,
    SYSCALL_SETTIMEOFDAY,
    SYSCALL_GETGROUPS,
    SYSCALL_SETGROUPS,
    SYSCALL_SELECT,
    SYSCALL_SYMLINK,
    SYSCALL_OLDLSTAT,
    SYSCALL_READLINK,
    SYSCALL_USELIB,
    SYSCALL_SWAPON,
    SYSCALL_REBOOT,
    SYSCALL_READDIR,
    SYSCALL_MMAP,
    SYSCALL_MUNMAP,
    SYSCALL_TRUNCATE,
    SYSCALL_FTRUNCATE,
    SYSCALL_FCHMOD,
    SYSCALL_FCHOWN,
    SYSCALL_GETPRIORITY,
    SYSCALL_SETPRIORITY,
    SYSCALL_PROFIL,
    SYSCALL_STATFS,
    SYSCALL_FSTATFS,
    SYSCALL_IOPERM,
    SYSCALL_SOCKETCALL,
    SYSCALL_SYSLOG,
    SYSCALL_SETITIMER,
    SYSCALL_GETITIMER,
    SYSCALL_STAT,
    SYSCALL_LSTAT,
    SYSCALL_FSTAT,
    SYSCALL_OLDUNAME,
    SYSCALL_IOPL,
    SYSCALL_VHANGUP,
    SYSCALL_IDLE,
    SYSCALL_VM86OLD,
    SYSCALL_WAIT4,
    SYSCALL_SWAPOFF,
    SYSCALL_SYSINFO,
    SYSCALL_IPC,
    SYSCALL_FSYNC,
    SYSCALL_SIGRETURN,
    SYSCALL_CLONE,
    SYSCALL_SETDOMAINNAME,
    SYSCALL_UNAME,
    SYSCALL_MODIFY_LDT,
    SYSCALL_ADJTIMEX,
    SYSCALL_MPROTECT,
    SYSCALL_SIGPROCMASK,
    SYSCALL_CREATE_MODULE,
    SYSCALL_INIT_MODULE,
    SYSCALL_DELETE_MODULE,
    SYSCALL_GET_KERNEL_SYMS,
    SYSCALL_QUOTACTL,
    SYSCALL_GETPGID,
    SYSCALL_FCHDIR,
    SYSCALL_BDFLUSH,
    SYSCALL_SYSFS,
    SYSCALL_PERSONALITY,
    SYSCALL_AFS_SYSCALL,
    SYSCALL_SETFSUID,
    SYSCALL_SETFSGID,
    SYSCALL__LLSEEK,
    SYSCALL_GETDENTS,
    SYSCALL__NEWSELECT,
    SYSCALL_FLOCK,
    SYSCALL_MSYNC,
    SYSCALL_READV,
    SYSCALL_WRITEV,
    SYSCALL_GETSID,
    SYSCALL_FDATASYNC,
    SYSCALL__SYSCTL,
    SYSCALL_MLOCK,
    SYSCALL_MUNLOCK,
    SYSCALL_MLOCKALL,
    SYSCALL_MUNLOCKALL,
    SYSCALL_SCHED_SETPARAM,
    SYSCALL_SCHED_GETPARAM,
    SYSCALL_SCHED_SETSCHEDULER,
    SYSCALL_SCHED_GETSCHEDULER,
    SYSCALL_SCHED_YIELD,
    SYSCALL_SCHED_GET_PRIORITY_MAX,
    SYSCALL_SCHED_GET_PRIORITY_MIN,
    SYSCALL_SCHED_RR_GET_INTERVAL,
    SYSCALL_NANOSLEEP,
    SYSCALL_MREMAP,
    SYSCALL_SETRESUID,
    SYSCALL_GETRESUID,
    SYSCALL_VM86,
    SYSCALL_QUERY_MODULE,
    SYSCALL_POLL,
    SYSCALL_NFSSERVCTL,
    SYSCALL_SETRESGID,
    SYSCALL_GETRESGID,
    SYSCALL_PRCTL,
    SYSCALL_RT_SIGRETURN,
    SYSCALL_RT_SIGACTION,
    SYSCALL_RT_SIGPROCMASK,
    SYSCALL_RT_SIGPENDING,
    SYSCALL_RT_SIGTIMEDWAIT,
    SYSCALL_RT_SIGQUEUEINFO,
    SYSCALL_RT_SIGSUSPEND,
    SYSCALL_PREAD,
    SYSCALL_PWRITE,
    SYSCALL_CHOWN,
    SYSCALL_GETCWD,
    SYSCALL_CAPGET,
    SYSCALL_CAPSET,
    SYSCALL_SIGALTSTACK,
    SYSCALL_SENDFILE,
    SYSCALL_GETPMSG,
    SYSCALL_PUTPMSG,
    SYSCALL_VFORK,
    SYSCALL_UGETRLIMIT,
    SYSCALL_MMAP2,
    SYSCALL_TRUNCATE64,
    SYSCALL_FTRUNCATE64,
    SYSCALL_STAT64,
    SYSCALL_LSTAT64,
    SYSCALL_FSTAT64,
    SYSCALL_LCHOWN32,
    SYSCALL_GETUID32,
    SYSCALL_GETGID32,
    SYSCALL_GETEUID32,
    SYSCALL_GETEGID32,
    SYSCALL_SETREUID32,
    SYSCALL_SETREGID32,
    SYSCALL_GETGROUPS32,
    SYSCALL_SETGROUPS32,
    SYSCALL_FCHOWN32,
    SYSCALL_SETRESUID32,
    SYSCALL_GETRESUID32,
    SYSCALL_SETRESGID32,
    SYSCALL_GETRESGID32,
    SYSCALL_CHOWN32,
    SYSCALL_SETUID32,
    SYSCALL_SETGID32,
    SYSCALL_SETFSUID32,
    SYSCALL_SETFSGID32,
    SYSCALL_PIVOT_ROOT,
    SYSCALL_MINCORE,
    SYSCALL_MADVISE,
    SYSCALL_MADVISE1,
    SYSCALL_GETDENTS64,
    SYSCALL_FCNTL64,
    SYSCALL_SECURITY,
    SYSCALL_GETTID,
    SYSCALL_READAHEAD,
    SYSCALL_SETXATTR,
    SYSCALL_LSETXATTR,
    SYSCALL_FSETXATTR,
    SYSCALL_GETXATTR,
    SYSCALL_LGETXATTR,
    SYSCALL_FGETXATTR,
    SYSCALL_LISTXATTR,
    SYSCALL_LLISTXATTR,
    SYSCALL_FLISTXATTR,
    SYSCALL_REMOVEXATTR,
    SYSCALL_LREMOVEXATTR,
    SYSCALL_FREMOVEXATTR,
    SYSCALL_TKILL,
    SYSCALL_SENDFILE64,
    SYSCALL_FUTEX,
    SYSCALL_SCHED_SETAFFINITY,
    SYSCALL_SCHED_GETAFFINITY,
    SYSCALL_SET_THREAD_AREA,
    SYSCALL_GET_THREAD_AREA
};


/* aruments passed to execve */
struct execve_args {
    char * filename;
    char ** argv;
    char ** envp;
};


struct syscall_status {
    long int syscall;
    long int state; // EAX
    void (*cleanup)(struct syscall_status *);

    union {
        char * chdir_path;

        /* struct execve_args defined in file defs.h */
        struct execve_args execve_args; 
    } args;
};


/* Checks whether at the address ADDR there is 'int 0x80'
   instruction code.
   
   This is a system hard-depended function! 
 */
inline xbool is_syscall(void *addr) {
    return (*((unsigned char*)addr) == (unsigned char)0xCD) &&
         (*((unsigned char*)addr+1) == (unsigned char)0x80);
}


/* Gets status of the current syscall of the current process.
   Assume that that process is going to execute the syscall.
 */
extern struct syscall_status get_syscall_status(pid_t pid);


/* A number of a syscall of the current process.
   Assume that that process is going to execute the syscall.
 */
//extern long int get_syscall_number(pid_t pid);


/* Returns name of system call. For example, name of syscall 26
   is "ptrace".
 */
extern const char * syscall_name(long int syscall);


/* Tells whether the SYSCALL number is valid.
 */ 
extern xbool syscall_is_valid(long int syscall);


/* Tells whether the SYSCALL is critical fot proocess PID, i.e. that
   we should execute that call instead of executing process or not.
 */
extern xbool syscall_is_critical(pid_t pid, long int syscall);


/* Tells whether the SYSCALL never returns, i.e. that we should
   not set thread_info->sys_syscall bit.
 */
extern xbool syscall_is_nonreturnee(long int syscall);


/* For the give procedd id executes a syscall which number
   is SYSCALL and stores its result into RESULT.
 */
extern int execute_syscall(pid_t pid, long int syscall, long int * result);


/* Saves RESULT int process' EAX register.
 */
extern int store_syscall_result(pid_t pid, long int value);


/* Gets a state of PID's current system call */
//extern long int syscall_state(pid_t pid);


/* Disables execution of current syscall if necessary.
   The process PID should be in "entering to syscalll" state.
 */
extern int skip_syscall(pid_t pid);


/* Tries to connect to JET runtime library in order to catch calls to ptrace. */
extern int try_to_connect_to_JET_runtime_lib ();


/* Returns true if given process is stopped by the ptrace or waitpid event handler. */
extern int check_ptrace_event  (thread_info_t thread_info);
extern int check_waitpid_event (thread_info_t thread_info);

/* Emulate the result of call to X2C_Ptrace. */
extern xbool xd_ptrace2 (thread_info_t thread_info);

/* Emulate the result of call to X2C_Waitpid. */
extern xbool xd_waitpid2 (thread_info_t thread_info);


#endif // _SYSCALL_H_


