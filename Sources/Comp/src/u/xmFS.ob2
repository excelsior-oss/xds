(** Copyright (c) 1994,97 XDS Ltd, Russia. All Rights Reserved. *)
(** o2/m2 development system v2.0 *)
(** Utility libraries based implementation of *)
(** FileSys managers *)

<*+ O2EXTENSIONS *>

MODULE xmFS; (* Ned 24-Feb-94. *)

(** Modifications:
  28-Mar-96 Ned     SearchPath - ProgEnv is used instead of stdlib.getenv
                    <*IF USE_CLIBS*> is deleted.
  30-Jun-97 Snowman Init - TimeConv.time is always used instead of
                    x2clib.X2C_Time
                    <*IF USE_CLIBS*> is deleted.
  10-Mar-99 Snowman SearchPath uses xmPlatform.hostExeExt instead of EXEEXT
*)

IMPORT
  xfs:=xiFiles,
  env:=xiEnv,
  xmPlatform,
  FileName:=PFNConv,
  sys:=FileSys,
  DStrings,
  RegComp,
  CharClass,
  ProgEnv,
  TimeConv,
  SYSTEM;

TYPE
  Dir = POINTER TO DirDesc;
  DirDesc = RECORD
    path: xfs.String;
    next: Dir;
  END;

  Node = POINTER TO NodeDesc;
  NodeDesc = RECORD
    expr : RegComp.Expr;
    dirs : Dir;
    level: INTEGER;
    next : Node;
  END;

  FileSys = POINTER TO FileSysDesc;
  FileSysDesc = RECORD (xfs.FileSysDesc)
    nodes: Node;
    level: INTEGER;
    vers : xfs.Time;
    path : Dir;      (* environment PATH *)
  END;

VAR
  null: xfs.String;
  is_path_sep: ARRAY 256 OF BOOLEAN;
  
PROCEDURE Match(re: RegComp.Expr; s-: ARRAY OF CHAR; pos: LONGINT; ext- : ARRAY OF CHAR): BOOLEAN;
VAR
  ss : xfs.String;
BEGIN
  IF (ext = "") THEN
    -- Apply "-lookup=*.=smth" to names w/o extensions (for linux executables)
    DStrings.Assign(s, ss);
    DStrings.Append(".", ss);
    RETURN RegComp.Match(re, ss^, pos);
  ELSE
    RETURN RegComp.Match(re, s, pos);
  END;
END Match;

(*--------------------  FileSys  ----------------------*)

PROCEDURE Exists(fnm-: ARRAY OF CHAR): BOOLEAN;
  VAR nm: xfs.FNAME;
BEGIN
  FileName.ConvertTo(fnm,nm);
  RETURN sys.Exists(nm);
END Exists;

PROCEDURE ModifyTime(fnm-: ARRAY OF CHAR;
                     VAR  time: xfs.Time;
                     VAR exist: BOOLEAN);
  VAR nm: xfs.FNAME;
BEGIN
  FileName.ConvertTo(fnm,nm);
  sys.ModifyTime(nm,time,exist);
END ModifyTime;

PROCEDURE Extract(s-: ARRAY OF CHAR; p,len: INTEGER; VAR d: ARRAY OF CHAR);
(* Do not use Extract - it tries to copy first argument. (Sem) *)
  VAR i: INTEGER;
BEGIN
  i:=0;
  IF len>=LEN(d) THEN len:=SHORT(LEN(d))-1 END;
  WHILE (i<len) & (s[p]#0X) DO
    d[i]:=s[p]; INC(i); INC(p);
  END;
  d[i]:=0X;
END Extract;

PROCEDURE LookupFile(dir: Dir;
              name-,ext-: ARRAY OF CHAR;
               VAR fname: xfs.FNAME;
               VAR n    : INTEGER);
(** Builds a filename using search path.
  Returns:
    n =  0  -- file not found (fname="")
    n >  0  -- file is found in the n-th directory
*)
  VAR dir_no: INTEGER;
BEGIN
  n:=0;
  dir_no:=1;
  WHILE dir # NIL DO
    FileName.Create(dir.path^,name,ext,fname);
    IF Exists(fname) THEN
      n:=dir_no;
      RETURN;
    END;
    dir:=dir.next;
    INC(dir_no);
  END;
  fname:="";
END LookupFile;

PROCEDURE Look(fs: FileSys;
               nm-: ARRAY OF CHAR;
               VAR n: INTEGER;
               VAR fname: xfs.FNAME): BOOLEAN;
  VAR l: Node; dir,name,ext: xfs.FNAME; sNm: xfs.String;
BEGIN
  FileName.Get(nm,dir,name,ext);
  IF dir#"" THEN RETURN FALSE END;
  l:=fs.nodes;
  fs.ConvertCaseToHost(nm, sNm);
  WHILE l#NIL DO
    IF Match(l.expr,sNm^,0,ext) THEN
      LookupFile(l.dirs,name,ext,fname,n);
      IF n>0 THEN RETURN TRUE END;
    END;
    l:=l.next;
  END;
  RETURN FALSE
END Look;

PROCEDURE (fs: FileSys) CreateDir(name-: ARRAY OF CHAR): BOOLEAN;
  VAR enm: env.String;
BEGIN
  fs.ConvertToHost(name,enm);
  RETURN sys.CreateDirectory(enm^);
END CreateDir;

PROCEDURE (fs: FileSys) RemoveDir(name-: ARRAY OF CHAR): BOOLEAN;
  VAR enm: env.String;
BEGIN
  fs.ConvertToHost(name,enm);
  RETURN sys.RemoveDirectory(enm^);
END RemoveDir;

PROCEDURE (fs: FileSys) IterateDir(name-: ARRAY OF CHAR; VAR i: xfs.DirIterator): BOOLEAN;
  VAR dnm,enm: env.String;
      dir: sys.Directory;
      ent: sys.Entry;
BEGIN
  fs.ConvertToHost(name,dnm);
  sys.OpenDir(dir,dnm^,ent);
  IF NOT ent.done THEN RETURN FALSE END;
  NEW(enm,64);
  LOOP
    IF LEN(enm^)-1<SYSTEM.VAL(LONGINT,ent.nameSize) THEN NEW(enm,ent.nameSize+1) END;
    sys.GetName(dir,enm^);
    IF i.Entry(enm^,ent.isDir) THEN EXIT END;
    sys.NextDirEntry(dir,ent);
    IF NOT ent.done THEN EXIT END;
  END;
  sys.CloseDir(dir);
  RETURN TRUE;
END IterateDir;

PROCEDURE (fs: FileSys) sLookup(name: xfs.FNAME; VAR fname: xfs.FNAME);
  VAR n: INTEGER;
BEGIN
  IF Look(fs,name,n,fname) THEN RETURN END;
  fname:=name;
END sLookup;

PROCEDURE (fs: FileSys) SysLookup(ext-: ARRAY OF CHAR;
                          VAR fname: xfs.String);
  VAR u,dir,name,x: xfs.String; n: INTEGER; fnm: xfs.FNAME;
BEGIN
  env.args.ProgramName(u);
  fs.Get(u^,dir,name,x);
  fs.Create('',name^,ext,x);
  IF Look(fs,x^,n,fnm) THEN
    DStrings.Assign(fnm,fname); RETURN;
  END;
  IF Exists(x^) THEN fname:=x; RETURN END;
  IF dir^#"" THEN
    fs.Create(dir^,name^,ext,fname);
    IF Exists(fname^) THEN RETURN END;
  END;
  fs.Create('',"xm",ext,x);
  IF Look(fs,x^,n,fnm) THEN
    DStrings.Assign(fnm,fname); RETURN;
  END;
  IF Exists(x^) THEN fname:=x; RETURN END;
  IF dir^#"" THEN
    fs.Create(dir^,"xm",ext,fname);
    IF Exists(fname^) THEN RETURN END;
  END;
  fs.Create('',name^,ext,fname);
END SysLookup;

PROCEDURE ParsePath(VAR dirs: Dir;
                       path-: ARRAY OF CHAR;
                           i: INTEGER;  (* start position in path *)
                     VAR err: LONGINT);
(* rewritten in asumption that path_sep may not include space *)
  VAR d,last: Dir; b,e: INTEGER; q: CHAR;
BEGIN
  last:=NIL;
  LOOP
    WHILE is_path_sep[ORD(path[i])] OR (path[i]=' ') DO INC(i) END;
                                    ----------------
    b:=i; q:=path[i];
    IF (q = '"') OR (q = "'") THEN
      INC(b); INC(i);
      WHILE (path[i]#0X) & (path[i] # q) DO INC(i) END;
      IF path[i] = 0X THEN err:=i; EXIT END;
      e:=i;
      INC(i);
    ELSE
      WHILE (path[i]#0X) & ~ is_path_sep[ORD(path[i])] DO INC(i) END;
      e:=i;
      IF ~ is_path_sep[ORD(' ')] THEN
        DEC(e);
        WHILE (e>=b) & (path[e] = ' ') DO DEC(e) END;
        INC(e);
      END;
    END;
    IF b=e THEN EXIT END;
    NEW(d);
    NEW(d.path,e-b+1);
    Extract((path),b,e-b,d.path^);
    FileName.ConvertFrom(d.path^,d.path^);
    IF last = NIL THEN dirs:=d ELSE last.next:=d END;
    last:=d;
  END;
END ParsePath;

PROCEDURE (fs: FileSys) SearchPath(fr-: ARRAY OF CHAR;
                                   VAR fnm: xfs.String);
  VAR
    dir,name,ext,str,bin: xfs.FNAME;
    len,err: LONGINT;
    p      : xfs.String;
    i      : INTEGER;
    d      : Dir;
BEGIN
  FileName.ConvertFrom(fr,str);
  FileName.Get(str,dir,name,ext);
  IF (ext="") THEN COPY(xmPlatform.hostExeExt,ext) END;
  IF dir='' THEN
    IF fs.path = NIL THEN
      len:=ProgEnv.StringLength("PATH");
      IF len > 0 THEN
        NEW(p,len+1);
        ProgEnv.String("PATH",p^);
        ParsePath(fs.path,p^,0,err);
      END;
      env.args.ProgramName(p);
      IF xfs.sys.sGetDir(p^,bin) & (bin[0]#0X) THEN
        NEW(d); DStrings.Assign(bin,d.path);
        d.next:=fs.path;
        fs.path:=d;
      END;
    END;
    IF fs.path # NIL THEN
      LookupFile(fs.path,name,ext,str,i);
      IF i>0 THEN DStrings.Assign(str,fnm); RETURN END;
    END;
  END;
  fs.Create(dir,name,ext,fnm);
END SearchPath;

PROCEDURE (fs: FileSys) sUseFirst(nm-: xfs.FNAME; VAR fname: xfs.FNAME);
  VAR l: Node; n: INTEGER; dir,name,ext: xfs.FNAME; ovr: BOOLEAN; sNm: xfs.String;
BEGIN
  ovr:=env.config.Option("OVERWRITE");
  FileName.Get(nm,dir,name,ext);
  IF dir="" THEN
    l:=fs.nodes;
    fs.ConvertCaseToHost(nm, sNm);
    WHILE l#NIL DO
      IF Match(l.expr,sNm^,0,ext) THEN
        IF ovr THEN
          LookupFile(l.dirs,name,ext,fname,n);
          IF n>0 THEN RETURN END;
        ELSE
          IF l.dirs = NIL THEN
            FileName.Create("",name,ext,fname);
          ELSE
            FileName.Create(l.dirs.path^,name,ext,fname);
          END;
          RETURN;
        END;
      END;
      l:=l.next;
    END;
  END;
  FileName.Create(dir,name,ext,fname);
END sUseFirst;

PROCEDURE (fs: FileSys) sUseFirstDir(nm-: xfs.FNAME; VAR fname: xfs.FNAME);
  VAR l: Node; i, n: INTEGER; dir,name,ext, name2: xfs.FNAME;
      ovr: BOOLEAN; sNm: xfs.String; --fd: xfs.FileDescriptor;
      hostdir: xfs.String; d: Dir;
BEGIN
  ovr:=env.config.Option("OVERWRITE");
  FileName.Get(nm,dir,name,ext);
  IF dir="" THEN
    l:=fs.nodes;
    fs.ConvertCaseToHost(nm, sNm);
    WHILE l#NIL DO
      IF Match(l.expr,sNm^,0,ext) THEN
        IF ovr THEN
          LookupFile(l.dirs,name,ext,fname,n);
          IF n>0 THEN
            d:=l.dirs;
            FOR i:=0 TO n-1 DO d:=d.next END;
            xfs.sys.ConvertToHost(d.path^,hostdir);
            COPY(hostdir^,fname);
            RETURN
          END;
        ELSE
          IF l.dirs = NIL THEN
            COPY(".",fname);
          ELSE
            xfs.sys.ConvertToHost(l.dirs.path^,hostdir);
            COPY(hostdir^,fname);
          END;
          RETURN;
        END;
      END;
      l:=l.next;
    END;
  END;
  COPY(".",fname);
END sUseFirstDir;

PROCEDURE Set(fs: FileSys;
        pattern-: ARRAY OF CHAR;
           path-: ARRAY OF CHAR;
               i: INTEGER;      (* start pos in path *)
         VAR err: LONGINT);
  VAR e: RegComp.Expr; res: LONGINT; l,p,n: Node; sPatt: xfs.String;
BEGIN
  fs.ConvertCaseToHost(pattern, sPatt);
  RegComp.Compile(sPatt^,e,res);
  IF res <= 0 THEN (* error *) err:=ABS(res)
  ELSE
    NEW(n);
    n.expr:=e; n.level:=fs.level;
    ParsePath(n.dirs,path,i,err);
    IF err < 0 THEN
      l:=fs.nodes; p:=NIL;
      WHILE (l#NIL) & (l.level=fs.level) DO p:=l; l:=l.next END;
      IF p = NIL THEN fs.nodes:=n ELSE p.next:=n END;
      n.next:=l;
    END;
  END;
END Set;

PROCEDURE (fs: FileSys) ParseRed(s-: ARRAY OF CHAR; VAR err: LONGINT);
  VAR
    i,p    : INTEGER;
    len    : LONGINT;
    pattern: xfs.FNAME;
BEGIN
  err:=-1;
  i:=0; len:=LENGTH(s);
  WHILE (i<len) & CharClass.IsWhiteSpace(s[i]) DO INC(i) END;
  p:=i;
  WHILE (i<len) & (s[i]#'=') & ~ CharClass.IsWhiteSpace(s[i]) DO INC(i) END;
  Extract(s,p,i-p,pattern);
  WHILE (i<len) & (s[i]#'=') DO INC(i) END;
  IF i = len THEN err:=i
  ELSE
    INC(i);
    WHILE (i<len) & CharClass.IsWhiteSpace(s[i]) DO INC(i) END;
    Set(fs,pattern,s,i,err);
  END;
END ParseRed;

(*
PROCEDURE show(fs: FileSys);
  VAR l: Node;
BEGIN
  tty.print('%.10c %d %.10c\n',"-",fs.level,"-");
  l:=fs.nodes;
  WHILE l#NIL DO
    tty.print('%2d %-16s %s\n',l.level,l.patt^,l.path^);
    l:=l.next;
  END;
  tty.print('%.25c\n',"-");
END show;
*)

PROCEDURE (fs: FileSys) SaveRed;
BEGIN
  INC(fs.level);
END SaveRed;

PROCEDURE (fs: FileSys) RestoreRed;
  VAR l: Node;
BEGIN
  l:=fs.nodes;
  WHILE (l#NIL) & (l.level=fs.level) DO l:=l.next END;
  fs.nodes:=l;
  DEC(fs.level);
END RestoreRed;

PROCEDURE (fs: FileSys) ModifyTime(name-: ARRAY OF CHAR;
                              VAR  time: xfs.Time;
                              VAR exist: BOOLEAN);
BEGIN
  ModifyTime(name,time,exist);
  IF NOT exist THEN time:=MIN(xfs.Time) END;
END ModifyTime;

PROCEDURE (fs: FileSys) GetFullPathName(fr-: ARRAY OF CHAR; 
                                      VAR to: ARRAY OF CHAR);
BEGIN
  sys.FullName(to,fr);
END GetFullPathName;

(*----------------------------------------------------------------*)

PROCEDURE (fs: FileSys) sGet(s-: ARRAY OF CHAR; VAR dir,name,ext: xfs.FNAME): BOOLEAN;
  VAR f: FileName.Format;
BEGIN
  FileName.GetFormat(s,f);
  IF ~f.ok THEN RETURN FALSE END;
  Extract(s,VAL(INTEGER,f.dirPos),VAL(INTEGER,f.dirLen),dir);
  Extract(s,VAL(INTEGER,f.namePos),VAL(INTEGER,f.nameLen),name);
  Extract(s,VAL(INTEGER,f.extPos),VAL(INTEGER,f.extLen),ext);
  RETURN TRUE;
END sGet;

PROCEDURE (fs: FileSys) sGetName(s-: ARRAY OF CHAR; VAR name: xfs.FNAME): BOOLEAN;
  VAR f: FileName.Format;
BEGIN
  FileName.GetFormat(s,f);
  IF ~f.ok THEN RETURN FALSE END;
  Extract(s,VAL(INTEGER,f.namePos),VAL(INTEGER,f.nameLen),name);
  RETURN TRUE;
END sGetName;

PROCEDURE (fs: FileSys) sGetDir(s-: ARRAY OF CHAR; VAR dir: xfs.FNAME): BOOLEAN;
  VAR f: FileName.Format;
BEGIN
  FileName.GetFormat(s,f);
  IF ~f.ok THEN RETURN FALSE END;
  Extract(s,VAL(INTEGER,f.dirPos),VAL(INTEGER,f.dirLen),dir);
  RETURN TRUE;
END sGetDir;

PROCEDURE (fs: FileSys) sGetExt(s-: ARRAY OF CHAR; VAR ext: xfs.FNAME): BOOLEAN;
  VAR f: FileName.Format;
BEGIN
  FileName.GetFormat(s,f);
  IF ~f.ok THEN RETURN FALSE END;
  Extract(s,VAL(INTEGER,f.extPos),VAL(INTEGER,f.extLen),ext);
  RETURN TRUE;
END sGetExt;

PROCEDURE (fs: FileSys) sCreate(dir-,name-,ext-: ARRAY OF CHAR; VAR fname: xfs.FNAME): BOOLEAN;
BEGIN
  FileName.Create(dir,name,ext,fname);
  RETURN TRUE;
END sCreate;

PROCEDURE (fs: FileSys) Exists(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN Exists(name);
END Exists;

PROCEDURE (fs: FileSys) VersionTag(): LONGINT;
BEGIN
  fs.vers:=fs.vers+1;
  RETURN SYSTEM.VAL(LONGINT,fs.vers);
END VersionTag;

PROCEDURE (fs: FileSys) ConvertToHost(fr-: ARRAY OF CHAR; VAR to: env.String);
BEGIN
  NEW(to,FileName.ConvertToLength(fr));
  FileName.ConvertTo(fr,to^);
END ConvertToHost;

PROCEDURE (fs: FileSys) ConvertToTarget(fr-: ARRAY OF CHAR; VAR to: env.String);
  VAR e: env.String;
BEGIN
  env.config.Equation("TARGET_FS",e);
  IF e=NIL THEN DStrings.Assign(FileName.PLATFORM,e) END;
  NEW(to,FileName.ConvertToTargetLength(e^,fr));
  FileName.ConvertToTarget(e^,fr,to^);
END ConvertToTarget;

PROCEDURE (fs: FileSys) ConvertFromHost(fr-: ARRAY OF CHAR; VAR to: xfs.String);
BEGIN
  NEW(to,FileName.ConvertFromLength(fr));
  FileName.ConvertFrom(fr,to^);
END ConvertFromHost;

PROCEDURE (fs: FileSys) ConvertCaseToHost(fr-: ARRAY OF CHAR; VAR to: xfs.String);
BEGIN
  NEW(to,LENGTH(fr)+1);
  FileName.ConvertCaseToTarget(FileName.PLATFORM,fr,to^);
END ConvertCaseToHost;

PROCEDURE (fs: FileSys) ConvertCaseToTarget(fr-: ARRAY OF CHAR; VAR to: xfs.String);
VAR e: env.String;
BEGIN
  env.config.Equation("TARGET_FS",e);
  IF e=NIL THEN DStrings.Assign(FileName.PLATFORM,e) END;
  NEW(to,LENGTH(fr)+1);
  FileName.ConvertCaseToTarget(e^,fr,to^);
END ConvertCaseToTarget;

(*----------------------------------------------------------------*)

PROCEDURE Init(fs: FileSys);
BEGIN
  fs.nodes:=NIL;
  fs.level:=0;
  fs.path:=NIL;
  fs.vers:=TimeConv.time();
END Init;

PROCEDURE SetManagers*;
  VAR sys: FileSys; i: INTEGER;
BEGIN
  NEW(sys); Init(sys); xfs.sys:=sys;
  FOR i:=0 TO 255 DO
    is_path_sep[i]:=FileName.IsPathSep(FileName.PLATFORM,CHR(i));
  END;
  ASSERT(~is_path_sep[0]);
END SetManagers;

BEGIN
  NEW(null,1);
  null[0]:=0X;
END xmFS.
