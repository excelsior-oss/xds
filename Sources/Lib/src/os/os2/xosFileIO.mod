(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
(*
   implementation of RTS file I/O functions for OS/2
*)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xosFileIO; (* Hady 29.05.96 18:47 *)

FROM SYSTEM  IMPORT  int;
FROM xrtsOS  IMPORT  X2C_OSFILE;
IMPORT  xOS2, SYSTEM;

TYPE File = xOS2.HFILE;

PROCEDURE ["C"] X2C_FileOpenRead(VAR f: X2C_OSFILE; name-: ARRAY OF CHAR): int;
  VAR action: CARDINAL;
      fh: File;
      res: xOS2.APIRET;
BEGIN
  res:=xOS2.DosOpen(SYSTEM.ADR(name), fh, action, 0,
               xOS2.FILE_NORMAL,
               xOS2.FAIL_IF_NEW + xOS2.OPEN_IF_EXISTS,
               xOS2.OPEN_ACCESS_READONLY + xOS2.OPEN_SHARE_DENYNONE,
               NIL);
  IF res=0 THEN
    f:=SYSTEM.CAST(X2C_OSFILE,fh);
    RETURN 0
  END;
  RETURN 1;
END X2C_FileOpenRead;

PROCEDURE ["C"] X2C_FileOpenWrite(VAR f: X2C_OSFILE; name-: ARRAY OF CHAR): int;
  VAR action: CARDINAL;
      fh: File;
      res: xOS2.APIRET;
BEGIN
  res:=xOS2.DosOpen(SYSTEM.ADR(name), fh, action, 0,
               xOS2.FILE_NORMAL,
               xOS2.CREATE_IF_NEW + xOS2.REPLACE_IF_EXISTS,
               xOS2.OPEN_ACCESS_WRITEONLY + xOS2.OPEN_SHARE_DENYWRITE,
               NIL);
  IF res=0 THEN
    f:=SYSTEM.CAST(X2C_OSFILE,fh);
    RETURN 0
  END;
  RETURN 1;
END X2C_FileOpenWrite;

PROCEDURE ["C"] X2C_FileOpenRW(VAR f: X2C_OSFILE; name-: ARRAY OF CHAR): int;
  VAR action: CARDINAL;
      fh: File;
      res: xOS2.APIRET;
BEGIN
  res:=xOS2.DosOpen(SYSTEM.ADR(name), fh, action, 0,
               xOS2.FILE_NORMAL,
               xOS2.FAIL_IF_NEW + xOS2.OPEN_IF_EXISTS,
               xOS2.OPEN_ACCESS_READWRITE + xOS2.OPEN_SHARE_DENYWRITE,
               NIL);
  IF res=0 THEN
    f:=SYSTEM.CAST(X2C_OSFILE,fh);
    RETURN 0
  END;
  RETURN 1;
END X2C_FileOpenRW;

PROCEDURE ["C"] X2C_FileClose(f: X2C_OSFILE): int;
BEGIN
  RETURN xOS2.DosClose(SYSTEM.CAST(File,f))
END X2C_FileClose;

PROCEDURE ["C"] X2C_FileSeek(f: X2C_OSFILE; VAR ofs: SYSTEM.WORD; org: int): int;
  VAR pos: SYSTEM.CARD32; rc: xOS2.APIRET;
BEGIN
  rc := xOS2.DosSetFilePtr(SYSTEM.CAST(File,f), SYSTEM.CAST(INTEGER,ofs), org, pos);
  ofs := pos;
  RETURN rc
END X2C_FileSeek;

PROCEDURE ["C"] X2C_FileRead(f: X2C_OSFILE; buf: SYSTEM.ADDRESS; VAR len: CARDINAL): int;
BEGIN
  RETURN xOS2.DosRead (SYSTEM.CAST(File,f), buf, len, len);
END X2C_FileRead;

PROCEDURE ["C"] X2C_FileWrite(f: X2C_OSFILE; buf: SYSTEM.ADDRESS; VAR len: CARDINAL): int;
BEGIN
  IF len=0 THEN
    RETURN 0
  ELSE
    RETURN xOS2.DosWrite (SYSTEM.CAST(File,f), buf, len, len)
  END;
END X2C_FileWrite;

PROCEDURE ["C"] X2C_StdOut(s-: ARRAY OF CHAR; len: CARDINAL);
BEGIN
  IF X2C_FileWrite(SYSTEM.CAST(X2C_OSFILE,xOS2.STDOUT), SYSTEM.ADR(s), len) # 0 THEN END;
END X2C_StdOut;

PROCEDURE ["C"] X2C_StdOutFlush;
BEGIN
END X2C_StdOutFlush;

END xosFileIO.
