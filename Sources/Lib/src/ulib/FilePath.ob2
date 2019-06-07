(** Copyright (c) 1994 xTech Ltd, Russia. All Rights Reserved. *)
(** Utility library *)
<*+ O2EXTENSIONS *>
MODULE FilePath; (* Ned 17-Feb-94. *)

IMPORT
   env:=platform
  ,DStrings
  ,FileName
  ,FileSys
  ,Strings
  ;

VAR null: DStrings.String;

PROCEDURE IsSimpleName*(name-: ARRAY OF CHAR): BOOLEAN;
(** Returns TRUE, if the filename does not contain directories. *)
  VAR f: FileName.Format;
BEGIN
  FileName.GetFormat(name,f);
  RETURN f.ok & (f.dirLen=0)
END IsSimpleName;

PROCEDURE Parse(VAR name: ARRAY OF CHAR; VAR dir,ext: DStrings.String);
  VAR f: FileName.Format;
BEGIN
  FileName.GetFormat(name,f);
  IF f.dirLen=0 THEN dir:=null ELSE NEW(dir,f.dirLen+1) END;
  IF f.extLen=0 THEN ext:=null ELSE NEW(ext,f.extLen+1) END;
  FileName.Get(name,dir^,name,ext^);
END Parse;

PROCEDURE Create(dir,ext: DStrings.String;
                   name-: ARRAY OF CHAR;
               VAR fname: DStrings.String);
  VAR len: LONGINT;
BEGIN
  IF dir^="." THEN dir[0]:=0X END;
  len:=FileName.Length(SHORT(LENGTH(dir^)),
  		SHORT(LENGTH(name)),SHORT(LENGTH(ext^)));
  IF len >= LEN(fname^) THEN NEW(fname,len+1) END;
  FileName.Create(dir^,name,ext^,fname^);
END Create;

PROCEDURE Lookup*(path-,name: ARRAY OF CHAR;
                  VAR fname: DStrings.String;
                  VAR n    : INTEGER);
(** Builds a filename using search path.
  Returns:
    n = -1  -- name is not simple (fname := name)
    n =  0  -- file not found (first directory is used)
    n >  0  -- file is found in the n-th directory
*)
  VAR
    dir,ext: DStrings.String;
    p,i,fpos,flen: INTEGER; len: LONGINT;
    dir_no: INTEGER;
BEGIN
  fname:=null;
  Parse(name,dir,ext);
  IF dir=null THEN
    n:=0; dir_no:=1;
    i:=0; len:=LENGTH(path); flen:=0; fpos:=MAX(INTEGER);
    LOOP
      WHILE (i<len) & env.IsPathDelim(path[i]) DO INC(i) END;
      IF i=len THEN EXIT END;
      p:=i;
      IF fpos>i THEN fpos:=i END;
      WHILE (i<len) & ~ env.IsPathDelim(path[i]) DO INC(i) END;
      IF flen=0 THEN flen:=i-fpos END;
      IF i-p >= LEN(dir^) THEN NEW(dir,i-p+1) END;
      Strings.Extract((path),p,i-p,dir^);
      Create(dir,ext,name,fname);
      IF FileSys.Exists(fname^) THEN n:=dir_no; RETURN END;
      INC(dir_no);
    END;
    IF flen>0 THEN
      ASSERT(flen < LEN(dir^));
      Strings.Extract((path),fpos,flen,dir^);
    ELSE dir[0]:=0X;
    END;
  ELSE n:=-1
  END;
  Create(dir,ext,name,fname);
END Lookup;

PROCEDURE UseFirst*(path-,name: ARRAY OF CHAR;
                    VAR fname: DStrings.String);
(** Builds a filename using the first directory from
  the search path.
*)
  VAR
    dir,ext: DStrings.String;
    p,i: INTEGER; len: LONGINT;
BEGIN
  fname:=null;
  Parse(name,dir,ext);
  IF dir=null THEN
    i:=0; len:=LENGTH(path);
    WHILE (i < len) & env.IsPathDelim(path[i]) DO INC(i) END;
    IF i < len THEN
      p:=i;
      WHILE (i<len) & ~ env.IsPathDelim(path[i]) DO INC(i) END;
      IF i-p >= LEN(dir^) THEN NEW(dir,i-p+1) END;
      Strings.Extract((path),p,i-p,dir^);
    END;
  END;
  Create(dir,ext,name,fname);
END UseFirst;

BEGIN
  NEW(null,1); null[0]:=0X;
END FilePath.
