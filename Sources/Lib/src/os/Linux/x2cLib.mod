(* Copyright (c) 1991,98 XDS Ltd, Russia. All Rights Reserved. *)
(* Copyright (c) 2002 Excelsior LLC. All Rights Reserved. *)


<*+ M2ADDTYPES *>
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE x2cLib; (* Jek 28-Nov-2002. *)


IMPORT SYSTEM, X2C, xrtsOS, xosDLL;

IMPORT ctype,dirent,errno,fcntl,float,glob,grp,limits,locale,math,pthread;
IMPORT pwd,select,semaphore,setjmp,signal,spawn,stat,stddef,stdio,stdlib;
IMPORT string,termios,time,times,types,unistd,utime,utsname,wait,uio;

FROM stdio IMPORT FILE, fileno, fdopen;
FROM string IMPORT memset;
FROM unistd IMPORT ftruncate, dup, dup2, environ;
FROM stat IMPORT stat_t, __fxstat, __xstat, S_IFMT, S_IFREG, S_IFCHR, S_IFIFO, _STAT_VER;
FROM time IMPORT time_t, tm_ptr, localtime, clock;
FROM select IMPORT fd_set, __NFDBITS;
FROM xlibOS IMPORT X2C_TimeStruct;


(* Returns time in seconds *)
PROCEDURE X2C_Time(): LONGCARD;
VAR
  t : time.time_t;
BEGIN
  time.time(t);
  RETURN t;
END X2C_Time;


(* Returns time in 1/100 fractions of second *)
PROCEDURE X2C_Clock(): LONGCARD;
BEGIN
  RETURN time.clock();
END X2C_Clock;


(* setup system dependend output file mode for:
        'R' - raw
        'T' - text
*)
PROCEDURE X2C_SetFileModes(name: ARRAY OF CHAR; type: CHAR);
BEGIN
END X2C_SetFileModes;


PROCEDURE X2C_FDup(VAR f: FILE; type: ARRAY OF CHAR): SYSTEM.ADDRESS;
BEGIN
  RETURN fdopen(dup(fileno(f)),type);
END X2C_FDup;


PROCEDURE X2C_FDup2(VAR f1: FILE; VAR f2: SYSTEM.ADDRESS): SYSTEM.int;
TYPE
  PFILE = POINTER TO FILE;

BEGIN
  RETURN dup2(fileno(f1), fileno(SYSTEM.CAST(PFILE, f2)^));
END X2C_FDup2;


PROCEDURE X2C_ChSize(VAR f: FILE; sz: SYSTEM.INT32): SYSTEM.int;
BEGIN
  RETURN ftruncate(fileno(f), sz);
END X2C_ChSize;


-- This done because the fstat and stat functions are
-- inline in glibc (at least in glibc 2.2.93).
PROCEDURE fstat (filedes: INTEGER; VAR buf: stat_t): INTEGER;
BEGIN
  RETURN __fxstat(_STAT_VER, filedes, buf);
END fstat; 


PROCEDURE X2C_stat (path: ARRAY OF CHAR; VAR buf: stat_t): INTEGER;
BEGIN
  RETURN __xstat(_STAT_VER, path, buf);
END X2C_stat;


<* IF (env_target="x86linux") OR (env_target = "linux") THEN *>

PROCEDURE FD_ZERO (VAR set: fd_set);
BEGIN
  memset(SYSTEM.ADR(set.fds_bits), 0, SIZE(set.fds_bits));
END FD_ZERO;


PROCEDURE FD_SET (fd : INTEGER; VAR set: fd_set);
BEGIN
  set.fds_bits[fd DIV __NFDBITS] :=
    set.fds_bits[fd DIV __NFDBITS] OR SYSTEM.CAST(LONGINT, SYSTEM.SHIFT(SYSTEM.CAST(BITSET, 1), fd MOD __NFDBITS));
END FD_SET;


PROCEDURE FD_CLR (fd : INTEGER; VAR set: fd_set);
BEGIN
  set.fds_bits[fd DIV __NFDBITS] :=
    set.fds_bits[fd DIV __NFDBITS] AND (NOT SYSTEM.CAST(LONGINT, SYSTEM.SHIFT(SYSTEM.CAST(BITSET, 1), fd MOD __NFDBITS)));
END FD_CLR;


PROCEDURE FD_ISSET (fd : INTEGER; VAR set: fd_set): LONGINT;
BEGIN
  RETURN set.fds_bits[fd DIV __NFDBITS] AND SYSTEM.CAST(LONGINT, SYSTEM.SHIFT(SYSTEM.CAST(BITSET, 1), fd MOD __NFDBITS));
END FD_ISSET;

<* ELSE *>
-- not implemented, because those macros are system-depended.
<* END *>

PROCEDURE X2C_GetFileType(VAR f: FILE): SYSTEM.int;
VAR
  buf : stat_t;
BEGIN
  IF fstat(fileno(f), buf) # 0 THEN
    RETURN 3  (* No diagnostics here yet *)
  END;

  IF    ((buf.st_mode & S_IFMT) = S_IFREG)  THEN RETURN 0
  ELSIF ((buf.st_mode & S_IFMT) = S_IFCHR)  THEN RETURN 1
  ELSIF ((buf.st_mode & S_IFMT) = S_IFIFO)  THEN RETURN 2
  ELSE                                           RETURN 3
  END;
END X2C_GetFileType;


PROCEDURE ["C"] X2C_GetModuleName ( hmod      :SYSTEM.ADDRESS;
                                    VAR mname :ARRAY OF CHAR;
                                    nmlen     :SYSTEM.CARD32 );
BEGIN
(* intentionally empty *)
END X2C_GetModuleName;  


<* IF (env_target="x86linux") THEN *>
TYPE
  pINTEGER = POINTER TO INTEGER;

PROCEDURE ["C"] / __errno_location (): pINTEGER;
<* END *>


PROCEDURE get_errno() :INTEGER;
BEGIN
<* IF (env_target="x86linux") THEN *>
  RETURN __errno_location()^;
<* ELSE *>
  RETURN errno.errno;
<* END *>
END get_errno;


END x2cLib.

