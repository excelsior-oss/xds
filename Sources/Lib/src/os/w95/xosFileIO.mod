(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
(*
   implementation of RTS file I/O functions for Windows95/NT
*)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosFileIO; (* Hady 29.05.96 18:47 *)

FROM SYSTEM  IMPORT  int;
FROM xrtsOS  IMPORT  X2C_OSFILE;
IMPORT X2C, xWin32, SYSTEM;

PROCEDURE ["C"] X2C_FileOpenRead(VAR f: X2C_OSFILE; name-: ARRAY OF CHAR): int;
  VAR df: xWin32.HANDLE;
BEGIN
  df:=xWin32.CreateFileA(
    SYSTEM.ADR(name),
    xWin32.GENERIC_READ,
    xWin32.FILE_SHARE_READ+xWin32.FILE_SHARE_WRITE,
    NIL,
    xWin32.OPEN_EXISTING,
    {},
    0);
  IF df#xWin32.INVALID_HANDLE_VALUE THEN f:=SYSTEM.CAST(X2C_OSFILE,df); RETURN 0 END;
  RETURN 1;
END X2C_FileOpenRead;

PROCEDURE ["C"] X2C_FileOpenWrite(VAR f: X2C_OSFILE; name-: ARRAY OF CHAR): int;
  VAR df: xWin32.HANDLE;
BEGIN
  df:=xWin32.CreateFileA(
    SYSTEM.ADR(name),
    xWin32.GENERIC_WRITE,
    xWin32.FILE_SHARE_READ,
    NIL,
    xWin32.CREATE_ALWAYS,
    {},
    0);
  IF df#xWin32.INVALID_HANDLE_VALUE THEN f:=SYSTEM.CAST(X2C_OSFILE,df); RETURN 0 END;
  RETURN 1;
END X2C_FileOpenWrite;

PROCEDURE ["C"] X2C_FileOpenRW(VAR f: X2C_OSFILE; name-: ARRAY OF CHAR): int;
  VAR df: xWin32.HANDLE;
BEGIN
  df:=xWin32.CreateFileA(
    SYSTEM.ADR(name),
    xWin32.GENERIC_READ + xWin32.GENERIC_WRITE,
    xWin32.FILE_SHARE_READ,
    NIL,
    xWin32.OPEN_EXISTING,
    {},
    0);
  IF df#xWin32.INVALID_HANDLE_VALUE THEN f:=SYSTEM.CAST(X2C_OSFILE,df); RETURN 0 END;
  RETURN 1;
END X2C_FileOpenRW;

PROCEDURE ["C"] X2C_FileClose(f: X2C_OSFILE): int;
BEGIN
  RETURN INT(NOT xWin32.CloseHandle(SYSTEM.CAST(xWin32.HANDLE,f)));
END X2C_FileClose;

PROCEDURE ["C"] X2C_FileSeek(f: X2C_OSFILE; VAR ofs: SYSTEM.WORD; org: int): int;
  VAR res: SYSTEM.CARD32;
BEGIN
  res:=xWin32.SetFilePointer(SYSTEM.CAST(xWin32.HANDLE,f),ofs,NIL,org);
  IF (res#0FFFFFFFFH) OR (xWin32.GetLastError()=0) THEN
    ofs:=res;
    RETURN 0
  END;
  RETURN 1;
END X2C_FileSeek;

PROCEDURE ["C"] X2C_FileRead(f: X2C_OSFILE; buf: SYSTEM.ADDRESS; VAR len: SYSTEM.CARD32): int;
BEGIN
  IF xWin32.ReadFile(SYSTEM.CAST(xWin32.HANDLE,f),buf,len,len,NIL) THEN RETURN 0 END;
  RETURN 1;
END X2C_FileRead;

PROCEDURE ["C"] X2C_FileWrite(f: X2C_OSFILE; buf: SYSTEM.ADDRESS; VAR len: SYSTEM.CARD32): int;
BEGIN
  IF xWin32.WriteFile(SYSTEM.CAST(xWin32.HANDLE,f),buf,len,len,NIL) THEN RETURN 0 END;
  RETURN 1;
END X2C_FileWrite;

CONST
  MSGBUFLEN = 1024;
  CAPTION   = "Application Error";

VAR 
  stderr: xWin32.HANDLE;
  msgbuf: ARRAY [0..MSGBUFLEN-1] OF CHAR;
  msglen: CARDINAL;

PROCEDURE ["C"] X2C_StdOut(s-: ARRAY OF CHAR; len: CARDINAL);
VAR 
  i: CARDINAL;
BEGIN
  IF len=0 THEN RETURN END;
  IF (stderr <> xWin32.INVALID_HANDLE_VALUE) THEN
    xWin32.WriteFile(stderr,SYSTEM.ADR(s),len,len,NIL);
  ELSE
    i := 0;
    WHILE (i < len) AND (msglen < MSGBUFLEN-1) DO
      msgbuf[msglen] := s[i];
      INC(msglen); INC(i);
    END;
  END;
END X2C_StdOut;

PROCEDURE ["C"] X2C_StdOutFlush;
VAR
  caption: POINTER TO CHAR;
BEGIN
  IF (stderr <> xWin32.INVALID_HANDLE_VALUE) THEN RETURN END;
  IF msglen > 0 THEN
    IF (X2C.X2C_argc = 0) THEN
      caption := SYSTEM.ADR(CAPTION);
    ELSE
      caption := X2C.X2C_argv^;
    END; 
    msgbuf[msglen] := 0C;
    xWin32.MessageBoxA(0,
                       SYSTEM.ADR(msgbuf),
                       caption,
                       xWin32.MB_OK+
                       xWin32.MB_ICONERROR+
                       xWin32.MB_TASKMODAL+
                       xWin32.MB_SETFOREGROUND);
    msglen := 0;
  END;
END X2C_StdOutFlush;

PROCEDURE ["C"] X2C_StdOutInit*(h: xWin32.HANDLE); 
(* called from xrnStart.X2C_xStart *)
BEGIN
  msglen := 0;
  stderr := h;
END X2C_StdOutInit;

END xosFileIO.
