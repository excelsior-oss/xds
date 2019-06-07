(* Copyright (C) 1999 XDS Ltd. *)
<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>
IMPLEMENTATION MODULE FIOR;

IMPORT FileName;
IMPORT FileSys;
IMPORT FIO;
IMPORT IOChan;
IMPORT platform;
IMPORT Str;

PROCEDURE HasExt(s-: ARRAY OF CHAR; VAR pos: CARDINAL): BOOLEAN;
VAR i: CARDINAL;
BEGIN
  i := LENGTH(s);
  LOOP
    IF i = 0 THEN RETURN FALSE END;
    DEC(i);
    IF    s[i] = platform.extSep  THEN pos := i; RETURN TRUE
    ELSIF (s[i] = platform.pathSep) OR
          (s[i] = platform.drvSep) THEN RETURN FALSE
    END;
  END;
END HasExt;

PROCEDURE AddExtension    ( VAR s : ARRAY OF CHAR; ext-: ARRAY OF CHAR ) ;
VAR i,j: CARDINAL;
BEGIN
  IF HasExt(s,i) THEN RETURN END;
  i := LENGTH(s);
  j := 0;
  IF i <= HIGH(s) THEN s[i] := '.'; INC(i); END;
  WHILE (i <= HIGH(s)) AND (j <= HIGH(ext)) AND (ext[j] <> '') DO
    s[i] := ext[j]; INC(i); INC(j);
  END;
  IF i <= HIGH(s) THEN s[i] := ''; END;
END AddExtension;

PROCEDURE ChangeExtension ( VAR s : ARRAY OF CHAR; ext-: ARRAY OF CHAR ) ;
BEGIN
  RemoveExtension(s);
  AddExtension(s,ext);
END ChangeExtension;

PROCEDURE RemoveExtension ( VAR s : ARRAY OF CHAR ) ;
VAR i: CARDINAL;
BEGIN
  IF HasExt(s,i) THEN s[i] := '' END;
END RemoveExtension;

PROCEDURE IsExtension     (     s- : ARRAY OF CHAR; ext-: ARRAY OF CHAR ) : BOOLEAN;
VAR i,j: CARDINAL;
BEGIN
  IF NOT HasExt(s,i) THEN RETURN (ext[0] = '') END;
  INC(i); j := 0;
  LOOP
    IF i > HIGH(s)   THEN RETURN (j > HIGH(ext)) OR (ext[j] = '') END;
    IF j > HIGH(ext) THEN RETURN s[i] = '' END;
    IF platform.pl_unix THEN
      IF s[i] <> ext[j] THEN RETURN FALSE END;
    ELSIF platform.pl_msdos THEN
      IF CAP(s[i]) <> CAP(ext[j]) THEN RETURN FALSE END;
    END;
    INC(i); INC(j);
  END;
END IsExtension;

PROCEDURE ExpandPath( path-        : ARRAY OF CHAR; 
                      VAR fullpath : ARRAY OF CHAR);
VAR
  temp  : ARRAY [0..1023] OF CHAR;
  drive : SHORTCARD;
  i,j   : CARDINAL;
BEGIN
  IF platform.pl_unix THEN
    i := 0;
  ELSIF platform.pl_msdos THEN
    IF path[1] <> platform.drvSep THEN
      drive := FIO.GetDrive(); i := 0;
    ELSE
      drive := ORD(CAP(path[0]))-ORD("A")+1; i := 2
    END;
  END;
  IF path[i] = platform.pathSep THEN
    COPY(path,temp);
  ELSE
    IF platform.pl_unix THEN
      FileSys.GetCDName(temp);
    ELSIF platform.pl_msdos THEN
      FIO.GetDir(drive,temp);
    END;
  END;  
  IF platform.pl_msdos AND (i = 0) THEN
    Str.Insert(temp,"  ",0);
    temp[0] := CHR(ORD("A")+drive-1);
    temp[1] := platform.drvSep;
  END;
  i := LENGTH(temp); j := 0;
  IF (path[0] <> platform.pathSep) AND (i <= HIGH(temp)) THEN 
    temp[i] := platform.pathSep; INC(i);
  END;
  WHILE (i <= HIGH(temp)) AND (j <= HIGH(path)) AND (path[j] <> '') DO
    temp[i] := path[j]; INC(i); INC(j);
  END;
  IF i <= HIGH(temp) THEN temp[i] := '' END;
  COPY(temp,fullpath);
END ExpandPath;

PROCEDURE MakePath  ( VAR path        : ARRAY OF CHAR ;
                          head-,tail- : ARRAY OF CHAR ) ;
VAR
  i,j  : CARDINAL;
BEGIN
  ExpandPath(head,path);
  i := LENGTH(path); j := 0;
  IF (i > 0) AND (path[i-1] <> platform.pathSep) THEN
    IF (tail[0] <> platform.pathSep) AND (i <= HIGH(path)) THEN
      path[i] := platform.pathSep; INC(i)
    END;
  END;
  WHILE (i <= HIGH(path)) AND (j <= HIGH(tail)) AND (tail[j] <> '') DO
    path[i] := tail[j]; INC(i); INC(j);
  END;
  IF i <= HIGH(path) THEN path[i] := '' END;
END MakePath;

PROCEDURE SplitPath ( 	  path-     : ARRAY OF CHAR ;
                      VAR head,tail : ARRAY OF CHAR ) ;
VAR
  f: FileName.Format;
  i: CARDINAL;
BEGIN
  FileName.GetFormat(path,f);
  i := 0;
  WHILE (i < f.dirPos+f.dirLen-1) AND (i < HIGH(head)) DO
    head[i] := path[f.dirPos+i];
    INC(i)
  END;
  head[i] := '';
  i := 0;
  WHILE (i < f.extPos+f.extLen-1) AND (i < HIGH(tail)) DO
    tail[i] := path[f.namePos+i];
    INC(i)
  END;
  tail[i] := '';
END SplitPath;

BEGIN
  ASSERT(platform.pl_unix OR platform.pl_msdos);
END FIOR.