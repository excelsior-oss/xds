(* Copyright (c) 1994,95 xTech Ltd, Russia. All Rights Reserved. *)
<*+ M2ADDTYPES *>
<*+ M2EXTENSIONS *>
<*+ STORAGE *>
IMPLEMENTATION MODULE FileSys; (* Ned 17-Feb-94. *)

IMPORT  xlibOS, SYSTEM, xmRTS, SysClock;
IMPORT  TimeConv;  -- to guarantee TimeConv initiation for X2C_ModifyTime

TYPE NMBUF = POINTER TO ARRAY OF CHAR;

TYPE
  Directory = POINTER TO xlibOS.X2C_Dir;

PROCEDURE ztname(VAR nmbuf: NMBUF; nm-: ARRAY OF CHAR): SYSTEM.ADDRESS;
  VAR i: CARDINAL;
BEGIN
  nmbuf:=NIL;
  i:=0;
  WHILE (i<=HIGH(nm)) & (nm[i]#0C) DO INC(i) END;
  IF i<=HIGH(nm) THEN RETURN SYSTEM.ADR(nm) END;
  IF (nmbuf#NIL) THEN
    IF i>HIGH(nmbuf^) THEN
      DISPOSE(nmbuf); NEW(nmbuf,i+1);
    END;
  ELSE
    NEW(nmbuf,i+1);
  END;
  COPY(nm,nmbuf^);
  RETURN SYSTEM.ADR(nmbuf^);
END ztname;

VAR buf0,buf1: NMBUF;

PROCEDURE Exists(fname-: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN xlibOS.X2C_Exists(SYSTEM.CAST(xmRTS.X2C_pCHAR,ztname(buf0,fname)));
END Exists;

PROCEDURE ModifyTime(fname-: ARRAY OF CHAR;
                  VAR time: LONGCARD;
                 VAR exist: BOOLEAN);
BEGIN
  xlibOS.X2C_ModifyTime(SYSTEM.CAST(xmRTS.X2C_pCHAR,ztname(buf0,fname)),time,exist);
END ModifyTime;

PROCEDURE SetFileTime(fname-: ARRAY OF CHAR;
                      time: LONGCARD);
BEGIN
  xlibOS.X2C_SetFileTime(SYSTEM.CAST(xmRTS.X2C_pCHAR,ztname(buf0,fname)),time);
END SetFileTime;

PROCEDURE EqualIgnoreCase(fname,newname: ARRAY OF CHAR):BOOLEAN;
VAR i:INTEGER;
BEGIN
  FOR i := 0 TO LEN(fname)-1 DO
    fname[i]:=CAP(fname[i]);
  END;
  FOR i := 0 TO LEN(newname)-1 DO
    newname[i]:=CAP(newname[i]);
  END;
  RETURN xmRTS.X2C_STRCMP_PROC (SYSTEM.ADR(fname), SIZE (fname), SYSTEM.ADR(newname), SIZE (newname)) = 0;
END EqualIgnoreCase;

PROCEDURE Rename(fname-,newname-: ARRAY OF CHAR; VAR done: BOOLEAN);
  VAR newnm: xmRTS.X2C_pCHAR;
BEGIN
  newnm:=SYSTEM.CAST(xmRTS.X2C_pCHAR,ztname(buf0,newname));
  IF EqualIgnoreCase(fname, newname) THEN
    done := Exists(fname);
    RETURN;
  END;
  done := (xlibOS.X2C_Remove(newnm) = 0);
  done := (xlibOS.X2C_Rename(SYSTEM.CAST(xmRTS.X2C_pCHAR,ztname(buf1,fname)),newnm) = 0);
END Rename;

PROCEDURE Remove(fname-: ARRAY OF CHAR; VAR done: BOOLEAN);
BEGIN
  done := (xlibOS.X2C_Remove(SYSTEM.CAST(xmRTS.X2C_pCHAR,ztname(buf0,fname))) = 0);
END Remove;

PROCEDURE FullName(VAR full: ARRAY OF CHAR; name: ARRAY OF CHAR);
BEGIN
  xlibOS.X2C_FullName(SYSTEM.CAST(xmRTS.X2C_pCHAR,SYSTEM.ADR(full)),
                      SIZE(full),
                      SYSTEM.CAST(xmRTS.X2C_pCHAR,ztname(buf0,name)));
END FullName;

PROCEDURE GetCDNameLength(): CARDINAL;
BEGIN
  RETURN xlibOS.X2C_GetCDNameLength();
END GetCDNameLength;

PROCEDURE GetCDName(VAR s: ARRAY OF CHAR);
BEGIN
  xlibOS.X2C_GetCDName(SYSTEM.CAST(xmRTS.X2C_pCHAR,SYSTEM.ADR(s)),SIZE(s));
END GetCDName;

PROCEDURE SetCD(name: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN xlibOS.X2C_SetCD(ztname(buf0,name));
END SetCD;

PROCEDURE copyEntry(VAR e: Entry; VAR d: xlibOS.X2C_Dir);

  PROCEDURE copy(VAR t: SysClock.DateTime; f: xlibOS.X2C_TimeStruct);
  BEGIN
    IF f.month<1  THEN f.month:=1  END;
    IF f.month>12 THEN f.month:=12 END;
    IF f.day<1    THEN f.day:=1    END;
    IF f.day>31   THEN f.day:=31   END;
    IF f.hour>23  THEN f.hour:=23  END;
    IF f.min>59   THEN f.min:=59   END;
    IF f.sec>59   THEN f.sec:=59   END;
    IF f.fracs>SysClock.maxSecondParts THEN f.fracs:=SysClock.maxSecondParts END;
    IF f.zone<-780 THEN f.zone:=-780 END;
    IF f.zone>720 THEN f.zone:=720 END;
    t.year:=f.year;
    t.month:=f.month;
    t.day:=f.day;
    t.hour:=f.hour;
    t.minute:=f.min;
    t.second:=f.sec;
    t.fractions:=f.fracs;
    t.zone:=f.zone;
    t.SummerTimeFlag:=f.stf;
  END copy;

BEGIN
  e.fileSize:=d.size;
  e.nameSize:=d.namelen;
  e.isDir:=d.is_dir;
  copy(e.creaTime,d.cretime);
  copy(e.modfTime,d.mdftime);
END copyEntry;

PROCEDURE OpenDir(VAR dir: Directory; name: ARRAY OF CHAR; VAR entry: Entry);
BEGIN
  entry.done:=FALSE;
  NEW(dir);
  IF dir=NIL THEN RETURN END;
  xlibOS.X2C_DirOpen(dir^,ztname(buf0,name));
  entry.done:=dir^.done;
  IF entry.done THEN copyEntry(entry,dir^) END;
END OpenDir;

PROCEDURE NextDirEntry(dir: Directory; VAR entry: Entry);
BEGIN
  IF dir=NIL THEN entry.done:=FALSE; RETURN END;
  xlibOS.X2C_DirNext(dir^);
  entry.done:=dir^.done;
  IF entry.done THEN copyEntry(entry,dir^) END;
END NextDirEntry;

PROCEDURE CloseDir(VAR dir: Directory);
BEGIN
  IF dir=NIL THEN RETURN END;
  xlibOS.X2C_DirClose(dir^);
  IF dir^.done THEN DISPOSE(dir); dir:=NIL END;
END CloseDir;

PROCEDURE GetName(dir: Directory; VAR name: ARRAY OF CHAR);
BEGIN
  xlibOS.X2C_DirGetName(dir^,SYSTEM.CAST(xmRTS.X2C_pCHAR,SYSTEM.ADR(name)),SIZE(name));
END GetName;

PROCEDURE CreateDirectory(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN xlibOS.X2C_CreateDirectory(ztname(buf0,name));
END CreateDirectory;

PROCEDURE RemoveDirectory(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN xlibOS.X2C_RemoveDirectory(ztname(buf0,name));
END RemoveDirectory;

PROCEDURE GetDrive(VAR drive: CHAR): BOOLEAN;
BEGIN
  RETURN xlibOS.X2C_GetDrive(drive) = 0;
END GetDrive;

PROCEDURE SetDrive(drive: CHAR): BOOLEAN;
BEGIN
  RETURN xlibOS.X2C_SetDrive(drive) = 0;
END SetDrive;

PROCEDURE GetDriveCDNameLength(drive: CHAR; VAR len: CARDINAL): BOOLEAN;
BEGIN
  RETURN xlibOS.X2C_GetDriveCDNameLength(drive,len) = 0;
END GetDriveCDNameLength;

PROCEDURE GetDriveCDName(drive: CHAR; VAR dir: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN xlibOS.X2C_GetDriveCDName(drive,SYSTEM.ADR(dir),HIGH(dir)+1) = 0;
END GetDriveCDName;

PROCEDURE GetLabel(drive: CHAR; VAR label: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN xlibOS.X2C_GetLabel(drive,SYSTEM.ADR(label),HIGH(label)+1) = 0;
END GetLabel;

BEGIN
  buf0:=NIL; buf1:=NIL;
END FileSys.
