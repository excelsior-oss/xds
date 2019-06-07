(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
(* Copyright (c) Excelsior LLC, 2002.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosFS;

IMPORT SYSTEM, xmRTS, xrtsOS;

FROM SYSTEM IMPORT ADDADR, SUBADR, DIFADR;
FROM stat IMPORT stat_t;
FROM x2cLib IMPORT X2C_stat, get_errno;
FROM string IMPORT strchr;
FROM stdio IMPORT remove, rename, printf;
FROM errno IMPORT ENOSYS;


  
PROCEDURE ["C"] X2C_Exists(fname: X2C_pCHAR): BOOLEAN;
VAR
  buf : stat_t;
BEGIN
  RETURN X2C_stat(fname, buf) = 0;
END X2C_Exists;


PROCEDURE ["C"] X2C_ModifyTime(fname: X2C_pCHAR;
                            VAR time: CARD32;
                           VAR exist: BOOLEAN);
VAR
  buf : stat_t;
BEGIN
  exist := FALSE;

  IF X2C_stat(fname, buf) = 0 THEN
    exist := TRUE;
    time := buf.st_mtime;
  END;
END X2C_ModifyTime;


PROCEDURE ["C"] X2C_SetFileTime(fname: X2C_pCHAR; time: CARD32);
BEGIN
END X2C_SetFileTime;


PROCEDURE ["C"] X2C_Remove(fname: xmRTS.X2C_pCHAR): int;
BEGIN
  IF remove(fname) # 0 THEN
    RETURN get_errno()
  END;

  RETURN 0;
END X2C_Remove;


PROCEDURE ["C"] X2C_Rename(oldname, newname: xmRTS.X2C_pCHAR): int;
BEGIN
  IF rename(oldname, newname) # 0 THEN
    RETURN get_errno()
  END;

  RETURN 0;
END X2C_Rename;


PROCEDURE ["C"] / X2C_GetCDName(s: xmRTS.X2C_pCHAR; slen: CARD32);


PROCEDURE ["C"] X2C_FullName(full: X2C_pCHAR;
                             len: CARD32;
                             name: X2C_pCHAR);
VAR
  s, d : X2C_pCHAR;

BEGIN
  IF name^ = '/' THEN
    d := full
  ELSE
    X2C_GetCDName(full, len);
    d := strchr(full, 0);
    d^ := '/';
    d := ADDADR(d, 1);
  END;
  
  s := name;

(* JET-1697 fix: striping "./" from name *)
  IF s^ = '.' THEN
    s := ADDADR(s, 1);
    IF s^ = '/' THEN
      s := ADDADR(s, 1);
    ELSE
      s := name;
    END;
  END;

  WHILE s^ # 0C DO
    IF SYSTEM.CAST(CARD32, DIFADR(d, full)) >= len THEN
      full^ := 0C;
      RETURN
    END;

    d^ := s^;
    d := ADDADR(d, 1);
    s := ADDADR(s, 1);
  END;
  
  IF SYSTEM.CAST(CARD32, DIFADR(d, full)) < len THEN
     d^ := 0C;
  END;
END X2C_FullName;


PROCEDURE ["C"] X2C_GetDrive(VAR drive: CHAR): SYSTEM.INT32;
BEGIN
  RETURN ENOSYS;
END X2C_GetDrive;


PROCEDURE ["C"] X2C_SetDrive(drive: CHAR): SYSTEM.INT32;
BEGIN
  RETURN ENOSYS;
END X2C_SetDrive;


PROCEDURE getDriveCD(drive: CHAR; buf: xmRTS.X2C_pCHAR; VAR len: CARD32): SYSTEM.INT32;
BEGIN
  RETURN ENOSYS;
END getDriveCD;


PROCEDURE ["C"] X2C_GetDriveCDNameLength(drive: CHAR; VAR len: CARD32): SYSTEM.INT32;
BEGIN
  RETURN ENOSYS;
END X2C_GetDriveCDNameLength;


PROCEDURE ["C"] X2C_GetDriveCDName(drive: CHAR; dir: xmRTS.X2C_pCHAR; len: CARD32): SYSTEM.INT32;
BEGIN
  RETURN ENOSYS;
END X2C_GetDriveCDName;


PROCEDURE ["C"] X2C_GetLabel(drive: CHAR; label: xmRTS.X2C_pCHAR; len: CARD32): SYSTEM.INT32;
BEGIN
  RETURN ENOSYS;
END X2C_GetLabel;


END xosFS.
