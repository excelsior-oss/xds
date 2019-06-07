(* Copyright (c) 1997 xTech Ltd, Russia. All Rights Reserved. *)
<* +m2extensions *>
IMPLEMENTATION MODULE xosDirs; (* VitVit, 18 Sep 1997. *)

IMPORT SYSTEM, xlibOS, xmRTS, OS2;

FROM SYSTEM IMPORT ADR, CAST;

CONST
  findBufLen = SIZE(OS2.FILEFINDBUF3);
  MAGIC      = 0FADEFADEH;

TYPE
  Entry    = POINTER TO EntryRec;
  EntryRec = RECORD
               findBuffer :OS2.FILEFINDBUF3;
               dirHandle  :SYSTEM.CARD32;
               mgk        :SYSTEM.CARD32;
             END;

PROCEDURE TransfRes (VAR fb :OS2.FILEFINDBUF3; VAR dir: xlibOS.X2C_Dir);

  PROCEDURE unpack( date :OS2.FDATE; time :OS2.FTIME; VAR tm: xlibOS.X2C_TimeStruct);
  BEGIN
    tm.day   := date MOD 32;
    date     := date DIV 32;
    tm.month := date MOD 16;
    tm.year  := (date DIV 16)+80;

    tm.sec   := time MOD 32;
    time     := time DIV 32;
    tm.min   := time MOD 64;
    tm.hour  := time DIV 64;

    tm.fracs := 0;
    tm.zone  := 0;
    tm.stf:=FALSE;
  END unpack;

BEGIN
  WITH fb DO
    dir.namelen := LENGTH(achName);
    dir.size    := cbFile;
    dir.is_dir  := (BITSET(OS2.FILE_DIRECTORY)*BITSET(attrFile)#{});

    unpack(fdateCreation,  ftimeCreation,  dir.mdftime);
    unpack(fdateLastWrite, ftimeLastWrite, dir.cretime);
  END;
END TransfRes;


VAR
  findCount :SYSTEM.CARD32;

PROCEDURE ["C"] X2C_DirOpen ( VAR dir :xlibOS.X2C_Dir; name :xmRTS.X2C_pCHAR );
CONST
  mayHaveEverything = OS2.FILE_ARCHIVED+OS2.FILE_DIRECTORY+OS2.FILE_SYSTEM+
                      OS2.FILE_HIDDEN+OS2.FILE_READONLY;
VAR
  ent     :Entry;
  buf     :ARRAY [0..OS2.CCHMAXPATH-1] OF CHAR;
  i       :CARDINAL;
  res     :OS2.APIRET;
BEGIN
  ASSERT(xlibOS.X2C_DirSysAreaSize>SIZE(EntryRec));
  IF name^=0C THEN (* set pattern to ".\*.*" *)
    COPY(".\*.*",buf);
  ELSE
    i:=0;
    WHILE (i<OS2.CCHMAXPATH-1) & (name^ # 0C) DO
      buf[i]:=name^;
      INC(i); name:=SYSTEM.ADDADR(name,SIZE(CHAR));
    END;
    buf[i]:=0C;
    IF ((i=2) & (buf[1]=":")) OR ((i>0) & (buf[i-1]="\")) THEN (* skip appending backslash *)
    ELSE
      buf[i]:="\"; INC(i);
    END;
    buf[i]:="*"; INC(i);
    buf[i]:=0C;
  END;

  ent := CAST(Entry, ADR(dir));
  WITH ent^ DO
    dirHandle := OS2.HDIR_CREATE;
    findCount := 1;    -- Look for 1 file at a time
    res := OS2.DosFindFirst( buf,               -- File pattern (wildcard)
                             dirHandle,         -- Directory search handle
                             mayHaveEverything, -- Search attribute
                             ADR(findBuffer),   -- Result buffer
                             findBufLen,        -- Result buffer length
                             findCount,         -- Number of entries to find
                             OS2.FIL_STANDARD); -- Return level 1 file info
    dir.done := (res = OS2.NO_ERROR);
    IF (dir.done) THEN
      mgk := MAGIC;
      TransfRes(findBuffer, dir);
    END;
  END;
END X2C_DirOpen;


PROCEDURE ["C"] X2C_DirNext(VAR dir: xlibOS.X2C_Dir);
VAR
  ent :Entry;
  res :OS2.APIRET;
BEGIN
  ent := CAST(Entry, ADR(dir));
  WITH ent^ DO
    dir.done := (mgk=MAGIC);
    IF (dir.done) THEN
      findCount := 1;
      res := OS2.DosFindNext( dirHandle,       -- Directory handle
                              ADR(findBuffer), -- Result buffer
                              findBufLen,      -- Result buffer length
                              findCount);      -- Number of entries to find
      dir.done := (res = OS2.NO_ERROR);
      IF (dir.done) THEN TransfRes(findBuffer, dir) END;
    END;
  END;
END X2C_DirNext;

PROCEDURE ["C"] X2C_DirClose(VAR dir: xlibOS.X2C_Dir);
VAR
  ent :Entry;
  res :OS2.APIRET;
BEGIN
  ent := CAST(Entry, ADR(dir));
  WITH ent^ DO
    dir.done := (mgk=MAGIC);
    IF (dir.done) THEN
      res := OS2.DosFindClose(dirHandle);
      dir.done := (res = OS2.NO_ERROR);
      IF (dir.done) THEN mgk := 0 END;
    END;
  END;
END X2C_DirClose;


PROCEDURE ["C"] X2C_DirGetName(VAR  dir: xlibOS.X2C_Dir;
                                   name: xmRTS.X2C_pCHAR;
                                  nmlen: SYSTEM.CARD32 );
VAR
  ent :Entry;
  i: CARDINAL;
BEGIN
  ent := CAST(Entry, ADR(dir));
  WITH ent^ DO
    dir.done:= (mgk=MAGIC);
    IF dir.done THEN
      i:=0;
      WHILE (i<nmlen) & (i<OS2.CCHMAXPATH) & (findBuffer.achName[i]#0C) DO
        name^ := findBuffer.achName[i]; INC(i);
        name  := SYSTEM.ADDADR(name,SIZE(CHAR));
      END;
      IF (i<nmlen) THEN name^:=0C END;
    END;
  END;
END X2C_DirGetName;

PROCEDURE ["C"] X2C_GetCDNameLength(): SYSTEM.CARD32;
VAR
  slen :SYSTEM.CARD32;
  zz   :ARRAY [0..0] OF CHAR;
BEGIN
  slen := 0;
  OS2.DosQueryCurrentDir(0, zz, slen );
  RETURN slen+3;                        -- C:\
END X2C_GetCDNameLength;

PROCEDURE ["C"] X2C_GetCDName(s: xmRTS.X2C_pCHAR; slen: SYSTEM.CARD32);
TYPE
  p2C = POINTER TO ARRAY[0..OS2.CCHMAXPATH-1] OF CHAR;
VAR
  ss                 :p2C;
  i                  :CARDINAL;
  driveNum, driveMap :SYSTEM.CARD32;
  dirName            :ARRAY [0..OS2.CCHMAXPATH-1] OF CHAR;
BEGIN
  OS2.DosQueryCurrentDisk(driveNum, driveMap);
  OS2.DosQueryCurrentDir(0, dirName, slen );
  ss := p2C(s);

  i := 0;
  IF (slen >2) THEN
    ss^[0] := CHR( ORD('A') + driveNum - 1 );
    ss^[1] := ':';
    ss^[2] := '\';

    i := 3;
    WHILE (i<slen) & (i<OS2.CCHMAXPATH) & (dirName[i-3] # 0C) DO
      ss^[i] := dirName[i-3];
      INC(i);
    END;
  END;
  IF (i<slen) THEN
    ss^[i]:=0C
  ELSE
    ss^[slen-1] := 0C;
  END;
END X2C_GetCDName;

-----

VAR
  dirName :ARRAY [0..OS2.CCHMAXPATH-1] OF CHAR;

PROCEDURE strCopy ( s: xmRTS.X2C_pCHAR );
VAR
  i    :CARDINAL;
BEGIN
  i := 0;
  LOOP
    dirName[i] := s^;
    IF (s^=0C) OR (i=OS2.CCHMAXPATH-1) THEN EXIT END;
    INC(i);
    s := SYSTEM.ADDADR(s,SIZE(CHAR));
  END;
END strCopy;

-----

PROCEDURE ["C"] X2C_SetCD(name :xmRTS.X2C_pCHAR): BOOLEAN;
VAR
  res :OS2.APIRET;
BEGIN
  strCopy(name);
  res := OS2.DosSetCurrentDir(dirName);
  RETURN (res = OS2.NO_ERROR);
END X2C_SetCD;


PROCEDURE ["C"] X2C_CreateDirectory(name :xmRTS.X2C_pCHAR): BOOLEAN;
VAR
  res :OS2.APIRET;
BEGIN
  strCopy(name);
  res := OS2.DosCreateDir(dirName , NIL);
  RETURN (res = OS2.NO_ERROR);
END X2C_CreateDirectory;

PROCEDURE ["C"] X2C_RemoveDirectory(name :xmRTS.X2C_pCHAR): BOOLEAN;
VAR
  res :OS2.APIRET;
BEGIN
  strCopy(name);
  res := OS2.DosDeleteDir(dirName);
  RETURN (res = OS2.NO_ERROR);
END X2C_RemoveDirectory;

END xosDirs.
