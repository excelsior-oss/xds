(* Copyright (c) 1997 xTech Ltd, Russia. All Rights Reserved. *)
(* Copyright (c) Excelsior LLC, 2002.  All Rights Reserved *)
<* +m2extensions *>

IMPLEMENTATION MODULE xosDirs; (* Jek. 29 Nov 2002 *)

IMPORT SYSTEM, xlibOS, limits;

FROM SYSTEM IMPORT ADDRESS, int;
FROM xmRTS IMPORT X2C_pCHAR;
FROM xrtsOS IMPORT X2C_malloc, X2C_free;
FROM errno IMPORT ERANGE;
FROM types IMPORT size_t;
FROM stat IMPORT stat_t, mkdir, S_IFMT, S_IFDIR, S_IRWXU, S_IRWXG, S_IRWXO;
FROM x2cLib IMPORT X2C_stat, get_errno;
FROM dirent IMPORT DIR, dirent, opendir, readdir, closedir;
FROM unistd IMPORT chdir, rmdir, getcwd;
FROM string IMPORT strcpy, strncpy, strlen, strrchr;
FROM unpkTime IMPORT X2C_UnpackTime;

(*
FROM stdio IMPORT printf, perror;
FROM xosFmtNL IMPORT X2C_StdOutN;
*)

TYPE
  pDIR = POINTER TO DIR;
  
  DirIterator = RECORD
    dp : pDIR;
    full_relative_name : X2C_pCHAR;    (* full directory name relative to the curent one *)
    full_relative_name_len : CARDINAL;
    magic : CARDINAL;
    name : ARRAY[0..0] OF CHAR;
  END;

  pDirIterator = POINTER TO DirIterator;

  pARRAY_OF_CHAR = POINTER TO ARRAY OF CHAR;
  

CONST
  DI_MAGIC = 0CAFECAFEH;
  DI_NAME_MAX = xlibOS.X2C_DirSysAreaSize-SIZE(DirIterator);


(* wrapper functions for libc std calls *)
PROCEDURE ["C"] c_strrchr(s: X2C_pCHAR; c: INTEGER): ADDRESS;
BEGIN
  RETURN strrchr(SYSTEM.CAST(pARRAY_OF_CHAR, s)^, c);
END c_strrchr;

PROCEDURE ["C"] c_strlen(s: X2C_pCHAR): size_t;
BEGIN
  RETURN strlen(SYSTEM.CAST(pARRAY_OF_CHAR, s)^);
END c_strlen;

PROCEDURE ["C"] c_strcpy(s1, s2: X2C_pCHAR): ADDRESS;
BEGIN
  RETURN strcpy(SYSTEM.CAST(pARRAY_OF_CHAR, s1)^, SYSTEM.CAST(pARRAY_OF_CHAR, s2)^);
END c_strcpy;



PROCEDURE ["C"] X2C_DirOpen(VAR dir: xlibOS.X2C_Dir; name: X2C_pCHAR);
VAR
  dp : pDIR;
  iter : pDirIterator;
  buf : stat_t;
  endslash : X2C_pCHAR;
  
BEGIN
  dp := opendir(name);

  IF dp = NIL THEN
    dir.done := FALSE;
    RETURN;
  END;

  iter := SYSTEM.ADR(dir.sys);
  iter^.dp := dp;
  iter^.full_relative_name := X2C_malloc(limits.PATH_MAX+1);
  iter^.magic := DI_MAGIC;

  (* store the full directory name *)
  c_strcpy(iter^.full_relative_name, name);
  iter^.full_relative_name_len := c_strlen(name);

  (* check if the give name is relative *)
  IF name^ # '/' THEN
    (* check if file is a directory *)
    X2C_stat(iter^.full_relative_name, buf);

    IF ((buf.st_mode & S_IFMT) # S_IFDIR) THEN (* the file is not a directory *)
      (* remove file name *)
      endslash := c_strrchr(iter^.full_relative_name, SYSTEM.CAST(SYSTEM.INT32, '/'));
      endslash := SYSTEM.ADDADR(endslash, 1);
      (* recalculate length *)
      iter^.full_relative_name_len := iter^.full_relative_name_len - c_strlen(endslash);
      endslash^ := 0C;
    END;
  END;

  (* add slash to back if it is not present *)
  IF SYSTEM.CAST(X2C_pCHAR, SYSTEM.ADDADR(iter^.full_relative_name, iter^.full_relative_name_len-1))^ # '/' THEN
    endslash := SYSTEM.ADDADR(iter^.full_relative_name, iter^.full_relative_name_len); (* that is, 0C *)
    endslash^ := '/';
    endslash := SYSTEM.ADDADR(endslash, 1);
    endslash^ := 0C;
    INC(iter^.full_relative_name_len);
  END;

(*
  X2C_StdOutN;
  printf("X2C_DirOpen: name: %s", name);
  X2C_StdOutN;
  printf("X2C_DirOpen: resulting name: %s", iter^.full_relative_name);
  X2C_StdOutN;
*)

  X2C_DirNext(dir);
END X2C_DirOpen;


PROCEDURE ["C"] X2C_DirNext(VAR dir: xlibOS.X2C_Dir);
VAR
  dep : POINTER TO dirent;
  it : pDirIterator;
  endslash : X2C_pCHAR;
  buf : stat_t;
  
BEGIN
  dir.done := FALSE;
  it := SYSTEM.ADR(dir.sys);
  
  IF it^.magic # DI_MAGIC THEN
    RETURN
  END;

  dep := readdir(it^.dp^);
        
  IF dep = NIL THEN RETURN END;

  endslash := SYSTEM.ADDADR(it^.full_relative_name, it^.full_relative_name_len);
  c_strcpy(endslash, dep^.d_name);

(*
  printf("X2C_DirNext: name: %s", dep^.d_name);
  X2C_StdOutN;
  printf("X2C_DirNext: resulting name: %s", it^.full_relative_name);
  X2C_StdOutN;
*)

  IF X2C_stat(it^.full_relative_name, buf) # 0 THEN
--    perror("X2C_DirNext: X2C_stat: ");
  END;

  (* restore original name *)
  endslash^ := 0C;

  strncpy(it^.name, dep^.d_name, DI_NAME_MAX);
  dir.namelen := strlen(dep^.d_name);
  dir.size := buf.st_size;
  X2C_UnpackTime(buf.st_mtime, dir.cretime);
  X2C_UnpackTime(buf.st_mtime, dir.mdftime);  
  dir.is_dir := ((buf.st_mode & S_IFMT) = S_IFDIR);
  dir.done := TRUE;
END X2C_DirNext;


PROCEDURE ["C"] X2C_DirClose(VAR dir: xlibOS.X2C_Dir);
VAR
  it : pDirIterator;
  
BEGIN
  dir.done := FALSE;
  it := SYSTEM.ADR(dir.sys);
  
  IF it^.magic # DI_MAGIC THEN
     RETURN
  END;

  dir.done := (closedir(it^.dp^) = 0);

  IF dir.done THEN
     X2C_free(it^.full_relative_name, limits.PATH_MAX+1);
     it^.full_relative_name := NIL;
     it^.magic := 0;
  END;
END X2C_DirClose;


PROCEDURE ["C"] X2C_DirGetName(VAR dir: xlibOS.X2C_Dir; name: X2C_pCHAR; nmlen: SYSTEM.CARD32);
VAR
  it : pDirIterator;
  
BEGIN
  it := SYSTEM.ADR(dir.sys);
  dir.done := (it^.magic = DI_MAGIC);
  IF dir.done THEN
    strncpy(SYSTEM.CAST(pARRAY_OF_CHAR, name)^, it^.name, nmlen);
  END;   
END X2C_DirGetName;


VAR
  _buffer : X2C_pCHAR;
  _size : int;
  
PROCEDURE ["C"] X2C_GetCDNameLength(): SYSTEM.CARD32;
VAR
  ps : pARRAY_OF_CHAR;
BEGIN
  IF _buffer = NIL THEN
     _buffer := X2C_malloc(limits.PATH_MAX);
     _size := limits.PATH_MAX;
  END;
  
  LOOP
    ps := SYSTEM.CAST(pARRAY_OF_CHAR, _buffer);

    IF getcwd(ps^, _size) # NIL THEN
      RETURN strlen(_buffer)
    END;
    
    IF get_errno() # ERANGE THEN
      RETURN 0
    END;
    
    (* Buffer too small *)
    X2C_free(_buffer, _size);
    _size := _size * 2;
    _buffer := X2C_malloc(_size);
  END;
END X2C_GetCDNameLength;


PROCEDURE ["C"] X2C_GetCDName(s: X2C_pCHAR; slen: SYSTEM.CARD32);
VAR
  ps : pARRAY_OF_CHAR;
BEGIN
  ps := SYSTEM.CAST(pARRAY_OF_CHAR, s);
  IF getcwd(ps^, slen) = NIL THEN
    s^ := 0C;
  END;
END X2C_GetCDName;


PROCEDURE ["C"] X2C_SetCD(s: X2C_pCHAR): BOOLEAN;
BEGIN
  RETURN (chdir(s) = 0);
END X2C_SetCD;


PROCEDURE ["C"] X2C_CreateDirectory(name: X2C_pCHAR): BOOLEAN;
BEGIN
  RETURN (mkdir(name, S_IRWXU OR S_IRWXG OR S_IRWXO) = 0);
END X2C_CreateDirectory;


PROCEDURE ["C"] X2C_RemoveDirectory(name: X2C_pCHAR): BOOLEAN;
BEGIN
  RETURN (rmdir(name) = 0);
END X2C_RemoveDirectory;


END xosDirs.
