(* Copyright (c) 1992,96,2000 Excelsior, Russia. All Rights Reserved. *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosFiles; (* Hady. 19.06.96 14:33 *)

IMPORT  SYSTEM, xlibOS, xWin32, ChanConsts, xmRTS;

TYPE File = xWin32.HANDLE;
     Res = xWin32.DWORD;
     int = SYSTEM.int;

TYPE
  X2C_OSFHANDLE = xlibOS.X2C_OSFHANDLE;
  X2C_FPOS = xlibOS.X2C_FPOS;
  ADDRESS = SYSTEM.ADDRESS;
  CARD32 = SYSTEM.CARD32;
  OpenResults = ChanConsts.OpenResults;

PROCEDURE convErr(res: Res): int;
BEGIN
  RETURN SYSTEM.CAST(int,res);
END convErr;

TYPE
  xAttrs = RECORD
    share: SYSTEM.SET32;
    sec  : SYSTEM.ADDRESS;
    attrs: SYSTEM.SET32;
  END;

VAR cAttrs: xAttrs;

PROCEDURE initAttrs;
BEGIN
  WITH cAttrs DO
    share:=xWin32.FILE_SHARE_READ;
    sec  :=NIL;
    attrs:=xWin32.FILE_ATTRIBUTE_NORMAL;
  END;
  xmRTS.X2C_fs_init:=TRUE;
END initAttrs;

PROCEDURE getAttrs(VAR a: xAttrs);
BEGIN
  IF NOT xmRTS.X2C_fs_init THEN initAttrs; END;
  a:=cAttrs;
END getAttrs;

PROCEDURE ["C"] X2C_fGetXAttrs(VAR data: xlibOS.X2C_FXATTRS);
BEGIN
  IF NOT xmRTS.X2C_fs_init THEN initAttrs END;
  data[0]:=SYSTEM.CAST(SYSTEM.CARD32,cAttrs.share);
  data[1]:=SYSTEM.CAST(SYSTEM.CARD32,cAttrs.sec);
  data[2]:=SYSTEM.CAST(SYSTEM.CARD32,cAttrs.attrs*{24..31});
  data[3]:=SYSTEM.CAST(SYSTEM.CARD32,cAttrs.attrs*{ 0..23});
END X2C_fGetXAttrs;

PROCEDURE ["C"] X2C_fSetXAttrs(VAR data: xlibOS.X2C_FXATTRS);
BEGIN
  cAttrs.share:=SYSTEM.CAST(SYSTEM.SET32,data[0]);
  cAttrs.sec  :=SYSTEM.CAST(SYSTEM.ADDRESS,data[1]);
  cAttrs.attrs:=SYSTEM.CAST(SYSTEM.SET32,data[2])*{24..31};
  cAttrs.attrs:=cAttrs.attrs+SYSTEM.CAST(SYSTEM.SET32,data[3])*{0..23};
  xmRTS.X2C_fs_init:=TRUE;
END X2C_fSetXAttrs;

PROCEDURE ["C"] X2C_IsMixAllowed(): BOOLEAN;
BEGIN
  RETURN TRUE;
END X2C_IsMixAllowed;

PROCEDURE ["C"] X2C_fOpen(VAR f: X2C_OSFHANDLE; name: ARRAY OF CHAR; tags: BITSET): OpenResults;
  VAR h: File;
   access: BITSET;
   a: xAttrs;
   method: xWin32.CreateMethod;
   err: Res;
BEGIN
  getAttrs(a);
  IF tags*X2C_fAccessRead#{} THEN access:=xWin32.GENERIC_READ
  ELSE access:={};
  END;
  IF tags*X2C_fAccessWrite#{} THEN
    access:=access+xWin32.GENERIC_WRITE;
  END;

  IF X2C_fModeNew*tags#{} THEN  method:=xWin32.CREATE_ALWAYS;
  ELSE method:=xWin32.OPEN_EXISTING;
  END;
  h:=xWin32.CreateFileA(SYSTEM.ADR(name),access,a.share,a.sec,method,a.attrs,0);
  IF h=xWin32.INVALID_HANDLE_VALUE THEN
    err:=xWin32.GetLastError();
    IF (err = xWin32.ERROR_FILE_NOT_FOUND) OR
       (err = xWin32.ERROR_PATH_NOT_FOUND) THEN
      RETURN ChanConsts.noSuchFile
    ELSIF err = xWin32.ERROR_TOO_MANY_OPEN_FILES  THEN RETURN ChanConsts.tooManyOpen
    ELSIF (err = xWin32.ERROR_ACCESS_DENIED) OR
          (err=xWin32.ERROR_SHARING_VIOLATION) OR
          (err=xWin32.ERROR_LOCK_VIOLATION) THEN
      RETURN ChanConsts.wrongPermissions
    ELSIF err = xWin32.ERROR_FILE_EXISTS   THEN RETURN ChanConsts.fileExists
    END;
    RETURN ChanConsts.otherProblem
  END;
  f:=SYSTEM.CAST(X2C_OSFHANDLE,h);
  RETURN ChanConsts.opened;
END X2C_fOpen;

PROCEDURE ["C"] X2C_fClose(VAR f: X2C_OSFHANDLE): int;
BEGIN
  IF xWin32.CloseHandle(SYSTEM.CAST(File,f)) THEN RETURN 0 END;
  RETURN convErr(xWin32.GetLastError());
END X2C_fClose;

PROCEDURE ["C"] X2C_fRead(f: X2C_OSFHANDLE; buf: ADDRESS; size: CARD32; VAR rd: CARD32): int;
BEGIN
  IF xWin32.ReadFile(SYSTEM.CAST(File,f),buf,size,rd,NIL) THEN RETURN 0 END;
  RETURN convErr(xWin32.GetLastError());
END X2C_fRead;

PROCEDURE ["C"] X2C_fWrite(f: X2C_OSFHANDLE; buf: ADDRESS; size: CARD32; VAR wr: CARD32): int;
BEGIN
  IF xWin32.WriteFile(SYSTEM.CAST(File,f),buf,size,wr,NIL) THEN RETURN 0 END;
  RETURN convErr(xWin32.GetLastError());
END X2C_fWrite;

PROCEDURE ["C"] X2C_fSeek(f: X2C_OSFHANDLE; VAR ofs: X2C_FPOS; how: int): int;
  VAR low,high,dir: xWin32.DWORD; res: Res;
BEGIN
  low:=ofs.low; high:=ofs.high;
  CASE how OF
    |X2C_fSeekSet: dir:=xWin32.FILE_BEGIN;
    |X2C_fSeekCur: dir:=xWin32.FILE_CURRENT;
  ELSE
    dir:=xWin32.FILE_END;
  END;
  low:=xWin32.SetFilePointer(SYSTEM.CAST(File,f),low,SYSTEM.ADR(high),dir);
  IF low=0FFFFFFFFH THEN
    res:=xWin32.GetLastError();
    IF res#0 THEN RETURN convErr(res) END;
  END;
  ofs.low:=low; ofs.high:=high;
  RETURN 0;
END X2C_fSeek;

PROCEDURE ["C"] X2C_fTell(f: X2C_OSFHANDLE; VAR pos: X2C_FPOS): int;
  VAR low,high: xWin32.DWORD; res: Res;
BEGIN
  high:=0; low:=0;
  low:=xWin32.SetFilePointer(SYSTEM.CAST(File,f),low,SYSTEM.ADR(high),xWin32.FILE_CURRENT);
  IF low=0FFFFFFFFH THEN
    res:=xWin32.GetLastError();
    IF res#0 THEN RETURN convErr(res) END;
  END;
  pos.low:=low; pos.high:=high;
  RETURN 0;
END X2C_fTell;

PROCEDURE ["C"] X2C_fSize(f: X2C_OSFHANDLE; VAR size: X2C_FPOS): int;
  VAR low,high: xWin32.DWORD; res: Res;
BEGIN
  low:=xWin32.GetFileSize(SYSTEM.CAST(File,f),SYSTEM.ADR(high));
  IF low=0FFFFFFFFH THEN
    res:=xWin32.GetLastError();
    IF res#0 THEN RETURN convErr(res) END;
  END;
  size.low:=low; size.high:=high;
  RETURN 0;
END X2C_fSize;

PROCEDURE ["C"] X2C_fFlush(f: X2C_OSFHANDLE): int;
  VAR res: xWin32.DWORD;
BEGIN
  IF xWin32.FlushFileBuffers(SYSTEM.CAST(File,f)) THEN RETURN 0 END;
  res:=xWin32.GetLastError();
  IF res=xWin32.ERROR_INVALID_HANDLE THEN RETURN 0 END;
  RETURN convErr(res);
END X2C_fFlush;

PROCEDURE ["C"] X2C_fChSize(f: X2C_OSFHANDLE): int;
BEGIN
  IF xWin32.FlushFileBuffers(SYSTEM.CAST(File,f)) &
     xWin32.SetEndOfFile(SYSTEM.CAST(File,f)) THEN
    RETURN 0
  END;
  RETURN xWin32.GetLastError();
END X2C_fChSize;

PROCEDURE ["C"] X2C_fGetStd(VAR F: X2C_OSFHANDLE; what: int): int;
  VAR f: File; dev: xWin32.DWORD;
BEGIN
  CASE what OF
    |X2C_fStdIn : dev:=xWin32.STD_INPUT_HANDLE;
    |X2C_fStdOut: dev:=xWin32.STD_OUTPUT_HANDLE;
  ELSE
    dev:=xWin32.STD_ERROR_HANDLE;
  END;
  f:=xWin32.GetStdHandle(dev);
  IF f=xWin32.INVALID_HANDLE_VALUE THEN
    RETURN convErr(xWin32.GetLastError());
  END;
  F:=SYSTEM.CAST(X2C_OSFHANDLE,f);
  RETURN 0;
END X2C_fGetStd;

PROCEDURE ["C"] X2C_fSetStd(new: X2C_OSFHANDLE; which: int): int;
  VAR dev: xWin32.DWORD;
BEGIN
  CASE which OF
    |X2C_fStdIn : dev:=xWin32.STD_INPUT_HANDLE;
    |X2C_fStdOut: dev:=xWin32.STD_OUTPUT_HANDLE;
  ELSE
    dev:=xWin32.STD_ERROR_HANDLE;
  END;
  IF xWin32.SetStdHandle(dev,SYSTEM.CAST(File,new)) THEN RETURN 0 END;
  RETURN convErr(xWin32.GetLastError());
END X2C_fSetStd;

(*
PROCEDURE ["C"] X2C_fNoBuffer(f: xlibOS.X2C_OSFHANDLE): BOOLEAN;
BEGIN
  RETURN xWin32.GetFileType(SYSTEM.CAST(File,f)) = xWin32.FILE_TYPE_CHAR;
END X2C_fNoBuffer;
*)

PROCEDURE ["C"] X2C_fGetFileType(f: xlibOS.X2C_OSFHANDLE): SYSTEM.int;
  VAR s: xWin32.DWORD; t: SYSTEM.int;
BEGIN
  s:=xWin32.GetFileType(SYSTEM.CAST(File,f));
  s:=s MOD xWin32.FILE_TYPE_REMOTE;
  CASE s OF
    |xWin32.FILE_TYPE_DISK: t:=X2C_ftDisk;
    |xWin32.FILE_TYPE_CHAR: t:=X2C_ftChar;
    |xWin32.FILE_TYPE_PIPE: t:=X2C_ftPipe;
  ELSE
    t:=X2C_ftUnk;
  END;
  RETURN t;
END X2C_fGetFileType;


END xosFiles.
