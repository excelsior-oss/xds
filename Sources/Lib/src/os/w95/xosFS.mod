(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosFS; (* Hady. 31.05.96 17:18 *)

IMPORT  SYSTEM, xmRTS, xWin32, xlibOS;

PROCEDURE ["C"] X2C_Exists(fname: X2C_pCHAR): BOOLEAN;
BEGIN
  RETURN xWin32.GetFileAttributesA(fname) <> 0FFFFFFFFH;
END X2C_Exists;

CONST FirstUnixDate = xlibOS.X2C_TimeStruct{1970,1,1,0,0,0,0,0,FALSE};

PROCEDURE ["C"] X2C_ModifyTime(  fname: X2C_pCHAR;
                              VAR time: CARD32;
                             VAR exist: BOOLEAN);

  VAR e: xWin32.FindData;
      h: xWin32.HANDLE;
      d: xWin32.TimeDate;
     fd: xWin32.FileTime;
     DT: xlibOS.X2C_TimeStruct;
BEGIN
  h:=xWin32.FindFirstFileA(fname,e);
  exist:=(h # xWin32.INVALID_HANDLE_VALUE);
  IF NOT exist THEN RETURN END;
  IF NOT xWin32.FileTimeToLocalFileTime(e.write,fd) THEN
    IF xWin32.FindClose(h) THEN END;
    time:=0; RETURN
  END;
  IF xWin32.FindClose(h) THEN END;
  IF NOT xWin32.FileTimeToSystemTime(fd,d) THEN
    time:=0; RETURN
  END;
  DT.sec  :=d.sec;
  DT.min  :=d.min;
  DT.hour :=d.hour;
  DT.day  :=d.day;
  DT.month:=d.month;
  DT.year :=d.year;
  time := xlibOS.X2C_TimeSecInt(DT,FirstUnixDate);
END X2C_ModifyTime;

PROCEDURE ["C"] X2C_SetFileTime(fname: X2C_pCHAR;
                                time: CARD32);
  VAR h: xWin32.HANDLE;
      d: xWin32.TimeDate;
     fd: xWin32.FileTime;
    lfd: xWin32.FileTime;
     DT: xlibOS.X2C_TimeStruct;
BEGIN
  h := xWin32.CreateFileA(fname,
                          xWin32.GENERIC_READ+xWin32.GENERIC_WRITE,
                          xWin32.FILE_SHARE_READ+xWin32.FILE_SHARE_WRITE,
                          NIL,
                          xWin32.OPEN_EXISTING,
                          xWin32.FILE_ATTRIBUTE_NORMAL,
                          0);
  IF h <> xWin32.INVALID_HANDLE_VALUE THEN
    xlibOS.X2C_TimeSecAdd(FirstUnixDate, time, DT);
    d.frac  := 0;
    d.sec   := DT.sec;
    d.min   := DT.min;
    d.hour  := DT.hour;
    d.day   := DT.day;
    d.month := DT.month;
    d.year  := DT.year;
    IF xWin32.SystemTimeToFileTime(d, fd) AND 
       xWin32.LocalFileTimeToFileTime(fd, lfd) THEN
      xWin32.SetFileTime(h, NIL, NIL, lfd);
    END;
    xWin32.CloseHandle(h);
  END;
END X2C_SetFileTime;

PROCEDURE ["C"] X2C_Remove(fname: xmRTS.X2C_pCHAR): SYSTEM.int;
BEGIN
  IF xWin32.DeleteFileA(fname) THEN RETURN 0 END;
  RETURN xWin32.GetLastError();
END X2C_Remove;

PROCEDURE ["C"] X2C_Rename(oldname, newname: xmRTS.X2C_pCHAR): SYSTEM.int;
BEGIN
  IF xWin32.MoveFileA(oldname,newname) THEN RETURN 0 END;
  RETURN xWin32.GetLastError();
END X2C_Rename;

PROCEDURE ["C"] X2C_FullName(full: X2C_pCHAR;
                             len: SYSTEM.CARD32;
                             name: X2C_pCHAR);
VAR
  filePart: SYSTEM.ADDRESS;
BEGIN
  xWin32.GetFullPathName (name, len, full, filePart);
END X2C_FullName;

PROCEDURE makeDriveStr(drive: CHAR; VAR drvStr: ARRAY OF CHAR );
BEGIN
  drvStr[0] := drive;
  drvStr[1] := ':';
  drvStr[2] := 0C;
END makeDriveStr;

PROCEDURE makeRootStr(drive: CHAR; VAR drvStr :ARRAY OF CHAR );
BEGIN
  drvStr[0] := drive;
  drvStr[1] := ':';
  drvStr[2] := '\';
  drvStr[3] := 0C;
END makeRootStr;

PROCEDURE ["C"] X2C_GetDrive(VAR drive: CHAR): SYSTEM.INT32;
VAR
  buf : ARRAY [0..511] OF CHAR;
BEGIN
  IF xWin32.GetCurrentDirectory(HIGH(buf)+1, buf) = 0 THEN
    RETURN xWin32.GetLastError();
  ELSE
    drive := buf[0];
    RETURN 0;
  END;
END X2C_GetDrive;

PROCEDURE ["C"] X2C_SetDrive(drive: CHAR): SYSTEM.INT32;
VAR
  drvStr :ARRAY [0..2] OF CHAR;
BEGIN
  makeDriveStr ( drive, drvStr );
  IF NOT xWin32.SetCurrentDirectory (drvStr) THEN
    RETURN xWin32.GetLastError()
  ELSE
    RETURN 0;
  END;
END X2C_SetDrive;

PROCEDURE getDriveCD(drive: CHAR; buf: xmRTS.X2C_pCHAR; VAR len: SYSTEM.CARD32): SYSTEM.INT32;
VAR
  drvStr : ARRAY [0..2] OF CHAR;
  dummy  : SYSTEM.ADDRESS;
  ret    : xWin32.DWORD;
BEGIN
  IF drive <> '' THEN
    makeDriveStr(drive,drvStr);
    len := xWin32.GetFullPathName(drvStr,len,buf,dummy)
  ELSE
    len := xWin32.GetCurrentDirectory(len,buf)
  END;
  IF len = 0 THEN
    RETURN xWin32.GetLastError()
  ELSE
    RETURN 0
  END;
END getDriveCD;

PROCEDURE ["C"] X2C_GetDriveCDNameLength(drive: CHAR; VAR len: SYSTEM.CARD32): SYSTEM.INT32;
VAR 
  buf : ARRAY [0..63] OF CHAR;
  rc  : SYSTEM.INT32;
BEGIN
  len := HIGH(buf)+1;
  rc := getDriveCD(drive, SYSTEM.ADR(buf), len);
  RETURN rc;
END X2C_GetDriveCDNameLength;

PROCEDURE ["C"] X2C_GetDriveCDName(drive: CHAR; dir: xmRTS.X2C_pCHAR; len: SYSTEM.CARD32): SYSTEM.INT32;
VAR 
  l  : SYSTEM.CARD32;
  rc : SYSTEM.INT32;
BEGIN
  l := len;
  rc := getDriveCD(drive, dir, l);
  IF (rc = 0) AND (l > len) THEN
    RETURN xWin32.ERROR_INSUFFICIENT_BUFFER
  END;
  RETURN rc
END X2C_GetDriveCDName;

PROCEDURE ["C"] X2C_GetLabel(drive: CHAR; label: xmRTS.X2C_pCHAR; len: SYSTEM.CARD32): SYSTEM.INT32;
VAR
  drvStr: ARRAY [0..3] OF CHAR;
  zzz   : CARDINAL;
  fsN   : ARRAY [0..10] OF CHAR;
  FSF   : xWin32.DWORD;
BEGIN
  makeRootStr ( drive, drvStr );
  IF NOT xWin32.GetVolumeInformationA(drvStr, label, len, NIL,
                                      SYSTEM.ADR(zzz), SYSTEM.ADR(FSF), fsN, 7 ) THEN
    RETURN xWin32.GetLastError();
  ELSE
    RETURN 0;
  END;
END X2C_GetLabel;

END xosFS.
