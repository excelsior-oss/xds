(* Copyright (c) 2000 Excelsior, Russia. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xrFName; (* paul 27-Jan-00 *)

IMPORT  env:=platform;


(* similar to M2 ISO Strings.Extract *)
PROCEDURE Extract(s: ARRAY OF CHAR; p,len: CARDINAL; VAR d: ARRAY OF CHAR);
  VAR i: CARDINAL;
BEGIN
  i:=0;
  WHILE (len>0) & (i<=HIGH(d)) & (p<=HIGH(s)) & (s[p]#0C) DO
    d[i]:=s[p]; DEC(len); INC(i); INC(p)
  END;
  IF i<=HIGH(d) THEN d[i]:=0C END;
END Extract;

(*----------------------------------------------------------------*)

PROCEDURE X2C_ParseFileName(s-: ARRAY OF CHAR; VAR f: Format);
  VAR len,i: CARDINAL;
      checkDrvSep: BOOLEAN;
BEGIN
  f.ok:=FALSE;
  f.dirPos:=0;  f.dirLen:=0;
  f.namePos:=0; f.nameLen:=0;
  f.extPos:=0;  f.extLen:=0;
  len:=LENGTH(s);
  IF len = 0 THEN RETURN END;
  i:=len;
  checkDrvSep := env.pl_msdos OR env.pl_vms OR env.pl_amiga;

  REPEAT DEC(i)
  UNTIL (i=0) OR (s[i]=env.extSep) OR (s[i]=env.pathEnd)
              OR checkDrvSep & (s[i]=env.drvSep);

  IF s[i]=env.extSep THEN
    f.extPos:=i+1;
    f.extLen:=len-i-1;
    len:=i;
  END;

  WHILE (i>0) & (s[i]#env.pathEnd) & NOT( checkDrvSep & (s[i]=env.drvSep) )
  DO DEC(i) END;

  IF s[i]=env.pathEnd THEN
    f.namePos:=i+1;
    f.nameLen:=len-i-1;
    f.dirLen:=i;
    IF i=0 THEN
      f.dirLen:=1;
    ELSIF env.pl_vms OR env.pl_msdos & (i=2) & (s[1]=env.drvSep) THEN
      INC(f.dirLen)
    END;
  ELSIF checkDrvSep & (s[i]=env.drvSep) THEN
    IF env.pl_msdos & (i#1) THEN RETURN END;
    f.namePos:=i+1;
    f.nameLen:=len-i-1;
    f.dirLen:=i+1;
  ELSE
    f.nameLen:=len;
  END;

  f.ok:=(f.nameLen + f.extLen > 0);
END X2C_ParseFileName;

PROCEDURE X2C_SplitFileName (fname: ARRAY OF CHAR;
                 VAR path,name,ext: ARRAY OF CHAR);
  VAR f: Format;
BEGIN
  X2C_ParseFileName(fname, f);
  IF f.ok THEN
    Extract(fname, f.dirPos, f.dirLen, path);
    Extract(fname, f.namePos, f.nameLen, name);
    Extract(fname, f.extPos, f.extLen, ext);
  ELSE
    path[0]:=0C; name[0]:=0C; ext[0]:=0C;
  END;
END X2C_SplitFileName;

PROCEDURE X2C_ExtractPath(fname: ARRAY OF CHAR; VAR path: ARRAY OF CHAR);
  VAR f: Format;
BEGIN
  X2C_ParseFileName(fname, f);
  IF f.ok THEN
    Extract(fname, f.dirPos, f.dirLen, path);
  ELSE
    path[0]:=0C;
  END;
END X2C_ExtractPath;

PROCEDURE X2C_ExtractBaseName(fname: ARRAY OF CHAR; VAR n: ARRAY OF CHAR);
  VAR f: Format;
BEGIN
  X2C_ParseFileName(fname, f);
  IF f.ok THEN
    Extract(fname, f.namePos, f.nameLen, n);
  ELSE
    n[0]:=0C;
  END;
END X2C_ExtractBaseName;

PROCEDURE X2C_ExtractFileExt(fname: ARRAY OF CHAR; VAR ext: ARRAY OF CHAR);
  VAR f: Format;
BEGIN
  X2C_ParseFileName(fname, f);
  IF f.ok THEN
    Extract(fname, f.extPos, f.extLen, ext);
  ELSE
    ext[0]:=0C;
  END;
END X2C_ExtractFileExt;

END xrFName.
