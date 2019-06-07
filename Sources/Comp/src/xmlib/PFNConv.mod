(* Copyright (c) 1994,97 XDS Ltd, Russia. All Rights Reserved. *)
<* +M2EXTENSIONS *> (* turned ON for RO  parameters *)
IMPLEMENTATION MODULE PFNConv;

IMPORT CharClass, Strings;

VAR fatfs: BOOLEAN;

PROCEDURE convert_from_host(host-,fname-: ARRAY OF CHAR; VAR str: ARRAY OF CHAR; VAR len: CARDINAL);
  VAR i: CARDINAL;
     ch: CHAR;

  PROCEDURE get;
  BEGIN
    IF i<=HIGH(fname) THEN ch:=fname[i]; INC(i); ELSE ch:=0C END;
  END get;

  PROCEDURE put(ch: CHAR);
  BEGIN
    IF ch = '"' THEN RETURN END;
    IF len<=HIGH(str) THEN str[len]:=ch; END;
    INC(len);
  END put;

BEGIN
  len:=0; i:=0;
  IF host=VMS THEN
    get;
    IF ch#"[" THEN
      LOOP;
        WHILE (ch#0C) & (ch#":") DO put(ch); get; END;
        IF ch=":" THEN
          put(":"); get;
          IF ch="[" THEN EXIT END;
        ELSE EXIT
        END;
      END;
    END;
    IF ch="[" THEN
      get;
      IF ch="." THEN
        put("."); get;
        IF ch#"]" THEN put("/") END;
      ELSIF ch="]" THEN put(".");
      ELSIF (ch#"-") THEN  (* absolute pathname *)
        put("/");
      END;
      WHILE (ch#"]") & (ch#0C) DO
        IF ch="." THEN ch:="/"
        ELSIF (ch="-") THEN put("."); ch:=".";
        END;
        put(ch); get;
      END;
      IF ch="]" THEN put("/"); get; END;
    END;
    WHILE (ch#0C) DO put(ch); get; END;
  ELSIF host=UNIX THEN
    WHILE (i<=HIGH(fname)) & (fname[i]#0C) DO put(fname[i]); INC(i); END;
  ELSE
    WHILE (i<=HIGH(fname)) & (fname[i]#0C) DO
      ch:=fname[i]; INC(i);
      IF ch="\" THEN ch:="/" END;
      put(ch);
    END;
  END;
  put(0C);
END convert_from_host;

PROCEDURE ConvertFromHostLength(host-,str-: ARRAY OF CHAR): CARDINAL;
  VAR buf: ARRAY [0..0] OF CHAR; len: CARDINAL;
BEGIN
  convert_from_host(host,str,buf,len);
  RETURN len;
END ConvertFromHostLength;

PROCEDURE ConvertFromLength(str-: ARRAY OF CHAR): CARDINAL;
  VAR buf: ARRAY [0..0] OF CHAR; len: CARDINAL;
BEGIN
  convert_from_host(PLATFORM,str,buf,len);
  RETURN len;
END ConvertFromLength;

PROCEDURE ConvertFromHost(host-, fname: ARRAY OF CHAR; VAR str: ARRAY OF CHAR);
  VAR ignore: CARDINAL;
BEGIN
  convert_from_host(host,fname,str,ignore);
END ConvertFromHost;

PROCEDURE ConvertFrom(fname: ARRAY OF CHAR; VAR str: ARRAY OF CHAR);
  VAR ignore: CARDINAL;
BEGIN
  convert_from_host(PLATFORM,fname,str,ignore);
END ConvertFrom;

PROCEDURE convert_to_target(trg-,fname-: ARRAY OF CHAR; VAR str: ARRAY OF CHAR; VAR len: CARDINAL);

  VAR i,n: CARDINAL; n8: BOOLEAN; ch: CHAR;

  PROCEDURE put(ch: CHAR);
  CONST dt=ORD('a')-ORD("A");
  BEGIN
    IF len<=HIGH(str) THEN
(*
      IF (trg#UNIX) AND (ch>='A') AND (ch<='Z') THEN
        ch:= CHR(ORD(ch) + dt);
      END;
*)
      str[len]:=ch;
    END;
    INC(len);
  END put;

  PROCEDURE get;
  BEGIN
    IF i<=HIGH(fname) THEN ch:=fname[i]; INC(i); ELSE ch:=0C END;
  END get;

BEGIN
  len:=0; i:=0;
  IF trg=VMS THEN
    n:=0;
    LOOP (* Search last ":" in disk prefix *)
      WHILE (i<=HIGH(fname)) & (fname[i]#0C) & (fname[i]#":") & (fname[i]#"/") DO
        INC(i);
      END;
      IF fname[i]=":" THEN n:=i; INC(i)
      ELSE EXIT
      END;
    END;
    i:=0;
    IF (n>0) OR (fname[0]=":") THEN
      WHILE (i<=n) DO put(fname[i]); INC(i); END;
    END;
    n:=LENGTH(fname);
    WHILE (n>i) & (fname[n]#"/") DO DEC(n) END;
    IF (n>i) THEN (* path with subdirectories *)
      put("[");
      IF (fname[i]="/") THEN INC(i); (* nothing to put *)
      ELSIF (fname[i]=".") THEN
        IF (i<HIGH(fname)) & (fname[i+1]="/") THEN INC(i) END;
      ELSE put(".");
      END;
      WHILE (i<n) DO
        IF (fname[i]=".") THEN
          IF (i<HIGH(fname)) & (fname[i+1]=".") THEN INC(i); put("-");
          ELSE put(".");
          END;
        ELSIF (fname[i]="/") THEN put(".");
        ELSE put(fname[i]);
        END;
        INC(i);
      END;
      INC(i);
      put("]");
    ELSIF (i=n) & (fname[n]="/") THEN
      (* root dir; no subdirs. As I've found on v850 roots
         of all volumes are called "[000000]". *)
      put("[");
      FOR i:=1 TO 6 DO put("0"); END;
      put("]");
      i:=n+1;
    END;
    WHILE (i<=HIGH(fname)) & (fname[i]#0C) DO put(fname[i]); INC(i) END;
  ELSIF trg=UNIX THEN
    IF fatfs THEN
      (* Новый способ преобразования имен введен для поддержания способа 
         именования принятого в DJGPP - 8.3 и прямые слэши
      *)
      get;
      LOOP
        n:=0;
        WHILE (ch#'/') & (ch#'.') & (ch#0C) DO
          IF n<8 THEN put(ch) END;
          INC(n); get;
        END;
        IF ch='.' THEN
          put('.'); n:=0; get;
          WHILE (ch#'/') & (ch#0C) DO
            IF n<3 THEN put(ch) END;
            INC(n); get;
          END;
        END;
        IF ch=0C THEN EXIT END;
        ASSERT(ch='/');
        put('/'); get;
      END;
    ELSE
      (* Старый способ - простое копирование *)
      WHILE (i<=HIGH(fname)) & (fname[i]#0C) DO put(fname[i]); INC(i); END;
    END;
  ELSE
    n8:=(trg=FATFS) OR fatfs;
    IF CharClass.IsLetter(fname[0]) & (HIGH(fname)>0) & (fname[1]=":") THEN
      put(fname[0]); put(fname[1]); i:=2;
    END;
    get;
    IF (ch='/') THEN put('\'); get; END;
    LOOP
      n:=0;
      WHILE (ch#'/') & (ch#'.') & (ch#0C) DO
        IF ~n8 OR (n<8) THEN put(ch) END;
        INC(n); get;
      END;
      IF ch='.' THEN
        put('.'); n:=0; get;
        WHILE (ch#'/') & (ch#0C) DO
          IF ~n8 OR (n<3) THEN put(ch) END;
          INC(n); get;
        END;
      END;
      IF ch=0C THEN EXIT END;
      ASSERT(ch='/');
      put('\'); get;
    END;
  END;
  put(0C);
END convert_to_target;

PROCEDURE ConvertToTargetLength(target-,fname-: ARRAY OF CHAR): CARDINAL;
  VAR buf: ARRAY [0..0] OF CHAR; len: CARDINAL;
BEGIN
  convert_to_target(target,fname,buf,len); RETURN len;
END ConvertToTargetLength;

PROCEDURE ConvertToLength(fname-: ARRAY OF CHAR): CARDINAL;
  VAR buf: ARRAY [0..0] OF CHAR; len: CARDINAL;
BEGIN
  convert_to_target(PLATFORM,fname,buf,len); RETURN len;
END ConvertToLength;

PROCEDURE ConvertToTarget(target-,fname: ARRAY OF CHAR; VAR str: ARRAY OF CHAR);
  VAR ignore: CARDINAL;
BEGIN
  convert_to_target(target,fname,str,ignore);
END ConvertToTarget;

PROCEDURE ConvertTo(fname: ARRAY OF CHAR; VAR str: ARRAY OF CHAR);
  VAR ignore: CARDINAL;
BEGIN
  convert_to_target(PLATFORM,fname,str,ignore);
END ConvertTo;

PROCEDURE ConvertCaseToTarget(target-,fname-: ARRAY OF CHAR; VAR str: ARRAY OF CHAR);
BEGIN
  COPY(fname, str);
  IF target # UNIX THEN Strings.Capitalize(str) END;
END ConvertCaseToTarget;

PROCEDURE GetFormat(s-: ARRAY OF CHAR; VAR f: Format);
  VAR len,i: CARDINAL;
BEGIN
  f.ok:=FALSE;
  f.dirPos:=0;  f.dirLen:=0;
  f.namePos:=0; f.nameLen:=0;
  f.extPos:=0;  f.extLen:=0;
  len:=LENGTH(s);
  IF len = 0 THEN RETURN END;
  i:=len;
  REPEAT DEC(i) UNTIL (i=0) OR (s[i]='.') OR (s[i]='/') OR (s[i]=':') & (i=1);
  IF s[i]='.' THEN
    f.extPos:=i+1;
    f.extLen:=len-i-1;
    len:=i;
  END;
  WHILE (i>0) & (s[i]#'/') & ((s[i]#':') OR (i#1)) DO DEC(i) END;
  IF (s[i]='/') OR (s[i]=':') & (i=1) THEN
    f.namePos:=i+1;
    f.nameLen:=len-i-1;
    IF (s[i]='/') & (i#0) & ((i#2) OR (s[i-1]#':')) THEN
      f.dirLen:=i;
    ELSE
      f.dirLen:=i+1;
    END;
  ELSE
    f.nameLen:=len;
  END;
  f.ok:=(f.nameLen + f.extLen > 0);
END GetFormat;

PROCEDURE Get(fname-: ARRAY OF CHAR; VAR dir,name,ext: ARRAY OF CHAR);
  VAR f: Format;
BEGIN
  GetFormat(fname,f);
  IF f.ok THEN
    Strings.Extract((fname),f.dirPos,f.dirLen,dir);
    Strings.Extract((fname),f.namePos,f.nameLen,name);
    Strings.Extract((fname),f.extPos,f.extLen,ext);
  ELSE
    dir[0]:=0C; name[0]:=0C; ext[0]:=0C;
  END;
END Get;

PROCEDURE GetDir(fname-: ARRAY OF CHAR; VAR dir: ARRAY OF CHAR);
  VAR f: Format;
BEGIN
  GetFormat(fname,f);
  IF f.ok THEN
    Strings.Extract((fname),f.dirPos,f.dirLen,dir);
  ELSE
    dir[0]:=0C;
  END;
END GetDir;

PROCEDURE GetName(fname-: ARRAY OF CHAR; VAR name: ARRAY OF CHAR);
  VAR f: Format;
BEGIN
  GetFormat(fname,f);
  IF f.ok THEN
    Strings.Extract((fname),f.namePos,f.nameLen,name);
  ELSE
    name[0]:=0C;
  END;
END GetName;

PROCEDURE GetExt(fname-: ARRAY OF CHAR; VAR ext: ARRAY OF CHAR);
  VAR f: Format;
BEGIN
  GetFormat(fname,f);
  IF f.ok THEN
    Strings.Extract((fname),f.extPos,f.extLen,ext);
  ELSE
    ext[0]:=0C;
  END;
END GetExt;

(*----------------------------------------------------------------*)

PROCEDURE Length(dir,name,ext: CARDINAL): CARDINAL;
BEGIN
  RETURN dir+name+ext+2;
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

PROCEDURE Create(dir-,name-,ext-: ARRAY OF CHAR; VAR fname: ARRAY OF CHAR);
  VAR p,d,i,j,k,len: CARDINAL; bk: BOOLEAN;
BEGIN
  ASSERT(name#'');
  p:=0; d:=0; k:=0;
  IF ((dir[0]#0C) & (dir[1]=':')) OR
     ((dir[0]="/") & (dir[1]="/")) THEN
    fname[0]:=dir[0];
    fname[1]:=dir[1];
    p:=2; d:=2; k:=2;
  END;
  IF dir[d]='/' THEN
    fname[p]:='/'; INC(d); INC(p); INC(k);
  END;
  WHILE dir[d]#0C DO
    i:=d;
    WHILE (dir[d]#'/') & (dir[d]#0C) DO INC(d) END;
    IF (i+1=d) & (dir[i]='.') THEN i:=d END;
    IF i<d THEN
      j:=0;
      bk:=(i+2=d) & (dir[i]='.') & (dir[i+1]='.') & (p>k);
      IF bk THEN
        j:=p-1;
        ASSERT(fname[j]='/');
        WHILE (j>k) & (fname[j-1]#'/') DO DEC(j) END;
        IF (j=p-3) & (fname[j]='.') & (fname[j+1]='.') THEN
          bk:=FALSE;
        END;
      END;
      IF bk THEN
        p:=j;
      ELSE
        WHILE i<d DO fname[p]:=dir[i]; INC(p); INC(i) END;
        fname[p]:='/'; INC(p);
      END;
    END;
    IF dir[d]='/' THEN INC(d) END;
  END;
  append(fname,p,name,0,LENGTH(name));
  len:=LENGTH(ext); d:=0;
  IF ext[0]='.' THEN DEC(len); d:=1 END;
  IF len#0 THEN
    char(fname,p,'.');
    append(fname,p,ext,d,len);
  END;
  fname[p]:=0C;
END Create;

PROCEDURE IsPathSep(e-: ARRAY OF CHAR; ch: CHAR): BOOLEAN;
(* Is "ch" environment variable PATH names separator? *)
BEGIN
  CASE ch OF
    |'(',')': RETURN (e=UNIX);
    |' '    : RETURN (e#UNC);
    |';'    : RETURN TRUE; (* is used in xm configuration files *)
    |':'    : RETURN (e=UNIX);
  ELSE
    RETURN FALSE;
  END;
END IsPathSep;

PROCEDURE SetFATFS(on: BOOLEAN);
BEGIN
  fatfs:=on;
END SetFATFS;

BEGIN
  <* IF TARGET_FS="UNIX" THEN *>
    COPY(UNIX,PLATFORM);
  <* ELSIF TARGET_FS="VMS" THEN *>
    COPY(VMS,PLATFORM);
  <* ELSIF TARGET_FS="OS2" THEN *>
    COPY(OS2,PLATFORM);
  <* ELSIF TARGET_FS="FATFS" THEN *>
    COPY(FATFS,PLATFORM);
  <* ELSIF TARGET_FS="UNC" THEN *>
    COPY(UNC,PLATFORM);
  <* ELSE *>
    COPY(UNC,PLATFORM);
  <* END *>
  fatfs:=FALSE;
END PFNConv.
