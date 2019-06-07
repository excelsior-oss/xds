(* Copyright (c) 1994,2000 Excelsior, Russia. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE FileName; (* Ned 15-Feb-94. *) (* paul 27-Jan-00 *)

IMPORT  env:=platform, xrFName;
IMPORT  Strings;

(*----------------------------------------------------------------*)

PROCEDURE GetFormat(s-: ARRAY OF CHAR; VAR f: Format);
VAR f1: xrFName.Format;
BEGIN
  xrFName.X2C_ParseFileName(s, f1);
  f.ok := f1.ok;
  IF f.ok THEN
    f.dirPos  := f1.dirPos;
    f.dirLen  := f1.dirLen;
    f.namePos := f1.namePos;
    f.nameLen := f1.nameLen;
    f.extPos  := f1.extPos;
    f.extLen  := f1.extLen;
  END;
END GetFormat;

PROCEDURE Get(fname: ARRAY OF CHAR; VAR dir,name,ext: ARRAY OF CHAR);
BEGIN
  xrFName.X2C_SplitFileName(fname, dir, name, ext);
END Get;

PROCEDURE GetDir(fname: ARRAY OF CHAR; VAR dir: ARRAY OF CHAR);
BEGIN
  xrFName.X2C_ExtractPath(fname, dir);
END GetDir;

PROCEDURE GetName(fname: ARRAY OF CHAR; VAR name: ARRAY OF CHAR);
BEGIN
  xrFName.X2C_ExtractBaseName(fname, name);
END GetName;

PROCEDURE GetExt(fname: ARRAY OF CHAR; VAR ext: ARRAY OF CHAR);
BEGIN
  xrFName.X2C_ExtractFileExt(fname, ext);
END GetExt;

(*----------------------------------------------------------------*)

PROCEDURE Length(dir,name,ext: CARDINAL): CARDINAL;
   VAR len: CARDINAL;
BEGIN
  len:=dir;
  IF dir>0 THEN
    IF env.pl_vms THEN INC(len,2) ELSE INC(len) END;
  END;
  IF env.pl_fatfs & (name > 8) THEN INC(len,8) ELSE INC(len,name) END;
  IF env.pl_fatfs & (ext > 3)  THEN INC(len,3) ELSE INC(len,ext) END;
  IF ext > 0 THEN INC(len) END;
  RETURN len
END Length;

PROCEDURE append(VAR d: ARRAY OF CHAR;
                 VAR p: CARDINAL;
                    s-: ARRAY OF CHAR;
                 i,len: CARDINAL);
BEGIN
  WHILE (p<=HIGH(d)) & (len>0) DO
    d[p]:=s[i]; INC(p); INC(i); DEC(len)
  END;
END append;

PROCEDURE char(VAR d: ARRAY OF CHAR; VAR p: CARDINAL; c: CHAR);
BEGIN
  IF p<=HIGH(d) THEN d[p]:=c; INC(p) END;
END char;

PROCEDURE Convert(str: ARRAY OF CHAR; VAR fname: ARRAY OF CHAR);
  VAR f: Format; p: CARDINAL; last: CHAR;
BEGIN
  GetFormat(str,f);
  IF f.ok THEN
    p:=0;
    IF (f.dirLen>0) & (~ env.pl_amiga OR (f.dirLen#1) OR (str[f.dirPos]#'.')) THEN
      append(fname,p,str,f.dirPos,f.dirLen);
      last:=str[f.dirPos+f.dirLen-1];
      IF env.pl_msdos OR env.pl_amiga THEN
	IF (last#env.pathEnd) & (last#env.drvSep) THEN char(fname,p,env.pathEnd) END;
      ELSIF NOT env.pl_vms & (last#env.pathEnd) THEN char(fname,p,env.pathEnd)
      END
    END;
    IF env.pl_fatfs & (f.nameLen > 8) THEN f.nameLen:=8 END;
    append(fname,p,str,f.namePos,f.nameLen);
    IF f.extLen#0 THEN
      char(fname,p,env.extSep);
      IF env.pl_fatfs & (f.extLen > 3) THEN f.extLen:=3 END;
      append(fname,p,str,f.extPos,f.extLen);
    END;
    IF p<=HIGH(fname) THEN fname[p]:=0C END;
    IF env.pl_msdos THEN Strings.Capitalize(fname) END;
  ELSE
    fname[0]:=0C;
  END;
END Convert;

PROCEDURE ConvertExt(VAR ext: ARRAY OF CHAR);
BEGIN
  IF ext[0]='.' THEN Strings.Delete(ext,0,1) END;
  IF env.pl_fatfs THEN
    IF LENGTH(ext) > 3 THEN ext[3]:=0X END;
    Strings.Capitalize(ext);
  END;
END ConvertExt;

PROCEDURE Create(dir,name,ext: ARRAY OF CHAR; VAR fname: ARRAY OF CHAR);
  VAR p,len: CARDINAL; last: CHAR;
BEGIN
  p:=0;
  len:=LENGTH(dir);
  IF (len>0) & (~ env.pl_amiga OR (dir#'.')) THEN
    append(fname,p,dir,0,len);
    last:=dir[len-1];
    IF env.pl_msdos OR env.pl_amiga THEN
      IF (last#env.pathEnd) & (last#env.drvSep) THEN char(fname,p,env.pathEnd) END;
    ELSIF NOT env.pl_vms & (last#env.pathEnd) THEN char(fname,p,env.pathEnd)
    END
  END;
  len:=LENGTH(name);
  IF env.pl_fatfs & (len > 8) THEN len:=8 END;
  append(fname,p,name,0,len);
  IF ext[0]=env.extSep THEN Strings.Delete(ext,0,1) END;
  len:=LENGTH(ext);
  IF len#0 THEN
    char(fname,p,env.extSep);
    IF env.pl_fatfs & (len > 3) THEN len:=3 END;
    append(fname,p,ext,0,len);
  END;
  IF p<=HIGH(fname) THEN fname[p]:=0C END;
  IF env.pl_msdos THEN Strings.Capitalize(fname) END;
END Create;

END FileName.
