(* Copyright (c) 1992,96,2000 Excelsior, Russia. All Rights Reserved. *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosFiles; (* Hady. 19.06.96 14:33 *)

IMPORT  SYSTEM, xlibOS, xOS2, xrInt64, ChanConsts, xmRTS;

TYPE File = xOS2.HFILE;
     Res = xOS2.APIRET;
     int = SYSTEM.int;

TYPE
  X2C_OSFHANDLE = xlibOS.X2C_OSFHANDLE;
  X2C_FPOS = xlibOS.X2C_FPOS;
  ADDRESS = SYSTEM.ADDRESS;
  CARD32 = SYSTEM.CARD32;
  OpenResults = ChanConsts.OpenResults;

TYPE
  xAttrs = RECORD
    share: SYSTEM.SET32;
    attrs: SYSTEM.SET32;
  END;

VAR cAttrs: xAttrs;

PROCEDURE initAttrs;
BEGIN
  WITH cAttrs DO
    share := xOS2.OPEN_SHARE_DENYNONE;
    attrs := BITSET(xOS2.FILE_NORMAL);
  END;
  xmRTS.X2C_fs_init:=TRUE;
END initAttrs;

PROCEDURE getAttrs(VAR a: xAttrs);
BEGIN
  IF NOT xmRTS.X2C_fs_init THEN initAttrs; END;
  a := cAttrs;
END getAttrs;

PROCEDURE ["C"] X2C_fGetXAttrs ( VAR data :xlibOS.X2C_FXATTRS);
BEGIN
  IF NOT xmRTS.X2C_fs_init THEN initAttrs END;
  data[0] := SYSTEM.CARD32(cAttrs.share);
  data[1] := SYSTEM.CARD32(cAttrs.attrs*{0..5});
END X2C_fGetXAttrs;

PROCEDURE ["C"] X2C_fSetXAttrs ( VAR data :xlibOS.X2C_FXATTRS);
BEGIN
  cAttrs.share := SYSTEM.SET32(data[0]);
  cAttrs.attrs := SYSTEM.SET32(data[1])*{0..5};
  xmRTS.X2C_fs_init:=TRUE;
END X2C_fSetXAttrs;


PROCEDURE ["C"] X2C_IsMixAllowed(): BOOLEAN;
BEGIN
  RETURN TRUE;
END X2C_IsMixAllowed;


PROCEDURE ["C"] X2C_fOpen(VAR f: X2C_OSFHANDLE; name: ARRAY OF CHAR; tags: BITSET): OpenResults;
CONST
  rw = X2C_fAccessRead+X2C_fAccessWrite;
VAR
  h: File;
  a: xAttrs;
  access: BITSET;
  method: BITSET;
  action: SYSTEM.CARD32;
  err: Res;
BEGIN
  getAttrs(a);
  IF tags*rw=rw THEN
    access:=xOS2.OPEN_ACCESS_READWRITE+a.share;
  ELSIF tags*X2C_fAccessWrite#{}  THEN
    access:=xOS2.OPEN_ACCESS_WRITEONLY+a.share;
  ELSE
    access:=xOS2.OPEN_ACCESS_READONLY+a.share;
  END;
  IF tags*X2C_fModeNew#{} THEN
    method:=xOS2.CREATE_IF_NEW+xOS2.REPLACE_IF_EXISTS;
  ELSE method:=xOS2.OPEN_IF_EXISTS;
  END;

  err:=xOS2.DosOpen(SYSTEM.ADR(name),h,action,0,SYSTEM.CARD32(a.attrs),method,access,NIL);
  IF err#0 THEN
    IF (err = xOS2.not_found) OR (err = xOS2.no_path) OR (err = xOS2.open_faild) THEN
      RETURN ChanConsts.noSuchFile
    ELSIF err = xOS2.too_many_files THEN RETURN ChanConsts.tooManyOpen
    ELSIF err = xOS2.denied_access  THEN RETURN ChanConsts.wrongPermissions
    END;
    RETURN ChanConsts.otherProblem
  END;
  f:=SYSTEM.CAST(X2C_OSFHANDLE,h);
  RETURN ChanConsts.opened;
END X2C_fOpen;

PROCEDURE ["C"] X2C_fClose(VAR f: X2C_OSFHANDLE): int;
BEGIN
  RETURN xOS2.DosClose(SYSTEM.CAST(File,f));
END X2C_fClose;

PROCEDURE ["C"] X2C_fRead(f: X2C_OSFHANDLE;
                        buf: ADDRESS;
                       size: CARD32;
                     VAR rd: CARD32): int;
BEGIN
  RETURN xOS2.DosRead(SYSTEM.CAST(File,f),buf,size,rd);
END X2C_fRead;

PROCEDURE ["C"] X2C_fWrite(f: X2C_OSFHANDLE;
                         buf: ADDRESS;
                        size: CARD32;
                      VAR wr: CARD32): int;
BEGIN
  RETURN xOS2.DosWrite(SYSTEM.CAST(File,f),buf,size,wr);
END X2C_fWrite;

PROCEDURE ["C"] X2C_fSeek(f: X2C_OSFHANDLE;
                    VAR ofs: X2C_FPOS;
                        how: int): int;
  VAR dir: CARDINAL;
      distance: SYSTEM.INT32;
      pos: CARDINAL;
      res: Res;
BEGIN
  IF xrInt64.X2C_64TOINT(distance,ofs) THEN RETURN xOS2.denied_access END;
  CASE how OF
    |X2C_fSeekSet: dir:=xOS2.FILE_BEGIN;
    |X2C_fSeekCur: dir:=xOS2.FILE_CURRENT;
  ELSE
    dir:=xOS2.FILE_END;
  END;
  res:=xOS2.DosSetFilePtr(SYSTEM.CAST(File,f),distance,dir,pos);
  IF res#xOS2.ok THEN RETURN res END;
  xrInt64.X2C_CARDTO64(ofs,pos);
  RETURN 0;
END X2C_fSeek;

PROCEDURE ["C"] X2C_fTell(f: X2C_OSFHANDLE; VAR pos: X2C_FPOS): int;
  VAR res: Res; ofs: SYSTEM.CARD32;
BEGIN
  res:=xOS2.DosSetFilePtr(SYSTEM.CAST(File,f),0,xOS2.FILE_CURRENT,ofs);
  IF res=0 THEN xrInt64.X2C_CARDTO64(pos,ofs) END;
  RETURN res;
END X2C_fTell;

PROCEDURE ["C"] X2C_fSize(f: X2C_OSFHANDLE; VAR size: X2C_FPOS): int;
  VAR res: Res; pos,eof: SYSTEM.CARD32;
BEGIN
  res:=xOS2.DosSetFilePtr(SYSTEM.CAST(File,f),0,xOS2.FILE_CURRENT,pos);
  IF res#xOS2.ok THEN RETURN res END;
  res:=xOS2.DosSetFilePtr(SYSTEM.CAST(File,f),0,xOS2.FILE_END    ,eof);
  IF res#xOS2.ok THEN RETURN res END;
  res:=xOS2.DosSetFilePtr(SYSTEM.CAST(File,f),pos,xOS2.FILE_BEGIN,pos);
  IF res=xOS2.ok THEN xrInt64.X2C_CARDTO64(size,eof) END;
  RETURN res;
END X2C_fSize;

PROCEDURE ["C"] X2C_fFlush(f: X2C_OSFHANDLE): int;
BEGIN
  RETURN xOS2.DosResetBuffer(SYSTEM.CAST(File,f));
END X2C_fFlush;

PROCEDURE ["C"] X2C_fChSize(f: X2C_OSFHANDLE): int;
  VAR rc: Res; pos: CARD32;
BEGIN
  rc := xOS2.DosResetBuffer(SYSTEM.CAST(File,f));
  IF rc = xOS2.ok THEN
    rc := xOS2.DosSetFilePtr(SYSTEM.CAST(File,f), 0, xOS2.FILE_CURRENT, pos);
  END;
  IF rc = xOS2.ok THEN
    rc := xOS2.DosSetFileSize(SYSTEM.CAST(File,f), pos);
  END;
  RETURN rc;
END X2C_fChSize;

PROCEDURE ["C"] X2C_fGetStd(VAR F: X2C_OSFHANDLE; what: int): int;
  VAR dev: xOS2.HFILE;
BEGIN
  CASE what OF
    |X2C_fStdIn : dev:=xOS2.STDIN;
    |X2C_fStdOut: dev:=xOS2.STDOUT;
  ELSE
    dev:=xOS2.STDERR;
  END;
  F:=SYSTEM.CAST(X2C_OSFHANDLE,dev);
  RETURN 0;
END X2C_fGetStd;

PROCEDURE ["C"] X2C_fSetStd(new: X2C_OSFHANDLE; which: int): int;
BEGIN
  RETURN 0;
END X2C_fSetStd;

(*
PROCEDURE ["C"] X2C_fNoBuffer(f: xlibOS.X2C_OSFHANDLE): BOOLEAN;
  VAR rc : Res; type : BITSET; attr : CARD32;
BEGIN
  rc := xOS2.DosQueryHType(SYSTEM.CAST(xOS2.HFILE,f),type,attr);
  IF rc = xOS2.ok THEN
    RETURN (type * {0..7} <> xOS2.FHT_DISKFILE) OR
           (type * {8..15} = xOS2.FHB_DSKREMOTE);
  END;
  RETURN TRUE;
END X2C_fNoBuffer;
*)

PROCEDURE ["C"] X2C_fGetFileType(f: xlibOS.X2C_OSFHANDLE): SYSTEM.int;
  VAR rc : Res; type : BITSET; attr : CARD32;
BEGIN
  rc := xOS2.DosQueryHType(SYSTEM.CAST(xOS2.HFILE,f),type,attr);
  IF rc = xOS2.ok THEN
    type:=type*{0..7};
    IF type * xOS2.FHT_PIPE # {} THEN RETURN X2C_ftPipe
    ELSIF type * xOS2.FHT_CHRDEV # {} THEN RETURN X2C_ftChar
    ELSIF type = xOS2.FHT_DISKFILE THEN RETURN X2C_ftDisk
    END;
  END;
  RETURN X2C_ftUnk;
END X2C_fGetFileType;

END xosFiles.
