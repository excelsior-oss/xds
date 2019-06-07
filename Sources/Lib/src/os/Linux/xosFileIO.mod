(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosFileIO; (* Leg 28.06.96 17:01 *)

IMPORT  xrtsOS, SYSTEM, stdio;

FROM SYSTEM  IMPORT  int, CARD32;
FROM xrtsOS  IMPORT  X2C_OSFILE;
FROM stdio IMPORT ftell, fseek, fwrite, fread, fopen, fclose, feof, fflush;
FROM unistd IMPORT write;

TYPE File = POINTER TO stdio.FILE;
     PFile = POINTER TO File;

PROCEDURE ["C"] X2C_FileOpenRead(VAR f: X2C_OSFILE; name: ARRAY OF CHAR): int;
  VAR t: File;
BEGIN
  t:=fopen(name,"r");
  IF t#NIL THEN f:=SYSTEM.CAST(X2C_OSFILE,t); RETURN 1
  ELSE RETURN 0
  END;
END X2C_FileOpenRead;

PROCEDURE ["C"] X2C_FileClose(f: X2C_OSFILE): int;
  VAR cf: File;
BEGIN
  cf:=SYSTEM.CAST(File,f);
  IF fclose(cf^)=0 THEN RETURN 1
  ELSE RETURN 0
  END
END X2C_FileClose;

PROCEDURE ["C"] X2C_FileSeek(f: X2C_OSFILE; VAR ofs: SYSTEM.WORD; org: int): int;
  VAR cf: File; 
BEGIN
  cf:=SYSTEM.CAST(File,f);
  IF fseek(cf^,SYSTEM.CAST(SYSTEM.INT32,ofs),org)=0 THEN RETURN 1
  ELSE RETURN 0
  END;
END X2C_FileSeek;

PROCEDURE ["C"] X2C_FileOpenWrite(VAR f: X2C_OSFILE; name: ARRAY OF CHAR): int;
  VAR t: File;
BEGIN
  t:=fopen(name,"rw");
  IF t#NIL THEN f:=SYSTEM.CAST(X2C_OSFILE,t); RETURN 1
  ELSE RETURN 0
  END;
END X2C_FileOpenWrite;

PROCEDURE ["C"] X2C_FileOpenRW(VAR f: X2C_OSFILE; name: ARRAY OF CHAR): int;
  VAR t: File;
BEGIN
  t:=fopen(name,"r+");
  IF t#NIL THEN f:=SYSTEM.CAST(X2C_OSFILE,t); RETURN 1
  ELSE RETURN 0
  END;
END X2C_FileOpenRW;

PROCEDURE ["C"] X2C_FileRead(f: X2C_OSFILE; buf: SYSTEM.ADDRESS; VAR len: CARD32): int;
  VAR cf: File;
BEGIN
  cf:=SYSTEM.CAST(File,f);
  IF fread(buf, len, 1, cf^)=len THEN RETURN 1
  ELSE RETURN 0
  END;
END X2C_FileRead;

PROCEDURE ["C"] X2C_FileWrite(f: X2C_OSFILE; buf: SYSTEM.ADDRESS; VAR len: CARD32): int;
  VAR cf: File;
BEGIN
  cf:=SYSTEM.CAST(File,f);
  IF fwrite(buf, len, 1, cf^)=len THEN RETURN 1
  ELSE RETURN 0
  END;
END X2C_FileWrite;

PROCEDURE ["C"] X2C_StdOut(s: ARRAY OF CHAR; len: CARDINAL);
BEGIN
  IF write(1,SYSTEM.ADR(s), len)=0 THEN END;
END X2C_StdOut;

PROCEDURE ["C"] X2C_StdOutFlush;
BEGIN
END X2C_StdOutFlush;

END xosFileIO.
