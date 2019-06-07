(* Copyright (c) 1997 xTech Ltd, Russia. All Rights Reserved. *)
<* +m2extensions *>
IMPLEMENTATION MODULE xosDirs; (* Hady, 4 Sep 1997. *)

IMPORT SYSTEM, xlibOS, xWin32, xmRTS;

TYPE
  Entry = POINTER TO EntryRec;
  EntryRec = RECORD
    int: xWin32.FindData;
    hnd: xWin32.HANDLE;
    mgk: SYSTEM.CARD32;
  END;

CONST MAGIC = 0FADEFADEH;

PROCEDURE unpackEntry(VAR ent: xWin32.FindData; VAR dir: xlibOS.X2C_Dir);
  VAR zone: xWin32.TimeZone;

  PROCEDURE unpack(t: xWin32.FileTime; VAR tm: xlibOS.X2C_TimeStruct);
    VAR
      ft : xWin32.FileTime;
      fts: xWin32.TimeDate;
  BEGIN
    xWin32.FileTimeToLocalFileTime(t,ft);
    xWin32.FileTimeToSystemTime(ft,fts);
    tm.year:=fts.year;
    tm.month:=fts.month;
    tm.day:=fts.day;
    tm.hour:=fts.hour;
    tm.min:=fts.min;
    tm.sec:=fts.sec;
    tm.fracs:=fts.frac;
    tm.zone:=zone.bias;
    tm.stf:=FALSE;
  END unpack;

BEGIN
  dir.namelen:=LENGTH(ent.name);
  dir.size:=ent.szlow;
  IF ent.szhigh#0 THEN dir.size:=MAX(SYSTEM.CARD32) END;
  dir.is_dir:=(xWin32.FILE_ATTRIBUTE_DIRECTORY*ent.attrs#{});
  xWin32.GetTimeZoneInformation(zone);
  unpack(ent.write,dir.mdftime);
  IF (ent.creation.low#0) OR (ent.creation.high#0) THEN
    unpack(ent.creation,dir.cretime);
  ELSE
    dir.cretime:=dir.mdftime;
  END;
END unpackEntry;

PROCEDURE ["C"] X2C_DirOpen(VAR dir: xlibOS.X2C_Dir; name: xmRTS.X2C_pCHAR);
  VAR ent: Entry;
      buf: ARRAY [0..xWin32.MAX_PATH-1] OF CHAR;
      len: CARDINAL;
      i: CARDINAL;
BEGIN
  ASSERT(xlibOS.X2C_DirSysAreaSize>SIZE(EntryRec));
  IF name^ = 0C THEN (* set pattern to ".\*.*" *)
    buf := ".\*.*"
  ELSE;
    len := xmRTS.X2C_LENGTH(name,HIGH(buf)+2); (* len > 0 because name^ <> 0C *)
    IF len >= HIGH(buf) THEN dir.done := FALSE; RETURN END;
    xmRTS.X2C_COPY(name,len,SYSTEM.ADR(buf),HIGH(buf)+1);
    i := len;
    LOOP
      DEC(i);
      IF (buf[i] = '*') OR (buf[i] = "?") THEN EXIT END;
      IF (buf[i] = '\') OR (buf[i] = ":") OR (i = 0) THEN 
        IF len >= xWin32.MAX_PATH-4 THEN dir.done:=FALSE; RETURN END;
        IF NOT (((len = 2) & (buf[1]=":")) OR
               ((len > 0) & (buf[len-1]="\"))) THEN
          buf[len] := "\"; INC(len);
        END;
        buf[len]:="*"; INC(len);
        buf[len]:="."; INC(len);
        buf[len]:="*"; INC(len);
        buf[len]:=0C;
        EXIT;
      END;
    END;
  END;
  ent:=SYSTEM.CAST(Entry,SYSTEM.ADR(dir));
  ent^.hnd:=xWin32.FindFirstFile(buf,ent^.int);
  dir.done:=(ent^.hnd#xWin32.INVALID_HANDLE_VALUE);
  IF dir.done THEN
    ent^.mgk:=MAGIC;
    unpackEntry(ent^.int,dir)
  END;
END X2C_DirOpen;

PROCEDURE ["C"] X2C_DirNext(VAR dir: xlibOS.X2C_Dir);
  VAR ent: Entry;
BEGIN
  ent:=SYSTEM.CAST(Entry,SYSTEM.ADR(dir));
  dir.done:=ent^.mgk=MAGIC;
  IF dir.done THEN
    dir.done:=xWin32.FindNextFile(ent^.hnd,ent^.int);
    IF dir.done THEN unpackEntry(ent^.int,dir) END;
  END;
END X2C_DirNext;

PROCEDURE ["C"] X2C_DirClose(VAR dir: xlibOS.X2C_Dir);
  VAR ent: Entry;
BEGIN
  ent:=SYSTEM.CAST(Entry,SYSTEM.ADR(dir));
  dir.done:=ent^.mgk=MAGIC;
  IF dir.done THEN
    dir.done:=xWin32.FindClose(ent^.hnd);
    IF dir.done THEN ent^.mgk:=0 END;
  END;
END X2C_DirClose;

PROCEDURE ["C"] X2C_DirGetName(VAR dir: xlibOS.X2C_Dir; name: xmRTS.X2C_pCHAR; nmlen: SYSTEM.CARD32);
  VAR ent: Entry; i: CARDINAL;
BEGIN
  ent:=SYSTEM.CAST(Entry,SYSTEM.ADR(dir));
  dir.done:=ent^.mgk=MAGIC;
  IF dir.done THEN
    i:=0;
    WHILE (i<nmlen) & (i<xWin32.MAX_PATH) & (ent^.int.name[i]#0C) DO
      name^:=ent^.int.name[i]; INC(i);
      name:=SYSTEM.ADDADR(name,SIZE(CHAR));
    END;
    IF i<nmlen THEN name^:=0C END;
  END;
END X2C_DirGetName;

PROCEDURE ["C"] X2C_GetCDNameLength(): SYSTEM.CARD32;
BEGIN
  RETURN xWin32.GetCurrentDirectory(0,NIL);
END X2C_GetCDNameLength;

PROCEDURE ["C"] X2C_GetCDName(s: xmRTS.X2C_pCHAR; slen: SYSTEM.CARD32);
BEGIN
  xWin32.GetCurrentDirectory(slen,s);
END X2C_GetCDName;

PROCEDURE ["C"] X2C_SetCD(s: xmRTS.X2C_pCHAR): BOOLEAN;
BEGIN
  RETURN xWin32.SetCurrentDirectory(s);
END X2C_SetCD;

PROCEDURE ["C"] X2C_CreateDirectory(name: xmRTS.X2C_pCHAR): BOOLEAN;
BEGIN
  RETURN xWin32.CreateDirectory(name,NIL);
END X2C_CreateDirectory;

PROCEDURE ["C"] X2C_RemoveDirectory(name: xmRTS.X2C_pCHAR): BOOLEAN;
BEGIN
  RETURN xWin32.RemoveDirectory(name);
END X2C_RemoveDirectory;

END xosDirs.