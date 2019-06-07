(** Copyright (c) 1994,97 XDS Ltd, Russia. All Rights Reserved. *)
(** o2/m2 development system v2.0: abstract files *)
<*+ O2EXTENSIONS *>
MODULE xiFiles; (* Ned 23-Feb-94. *)

(*
  13-Apr-94: RawFile:  SetPos, GetPos are appended
             SymFile - implemented on the base of RawFile
  07-Mar-94: RawFile.Length
  18-Oct-95: TextFile.Length (Ned)
*)

IMPORT SYSTEM, DStrings, env:=xiEnv;

CONST
  (** Read results *)
  allRight*   = 0;
  endOfInput* = 1;
  endOfLine*  = 2; (** for TextFile only *)
  bufOverflow*= 3;

TYPE
  FNAME*  = ARRAY 256 OF CHAR;
  String* = DStrings.String;
  Time*   = SYSTEM.CARD32;

  File* = POINTER TO FileDesc;
  FileDesc* = RECORD
    new*    : BOOLEAN;
    name*   : String;
    readRes*: INTEGER; (** result of the last Read operation *)
    readLen*: LONGINT; (** lenght of the last Read operation *)
  END;

  TextFile* = POINTER TO TextFileDesc;
  TextFileDesc* = RECORD (FileDesc)
  END;

  RawFile* = POINTER TO RawFileDesc;
  RawFileDesc* = RECORD (FileDesc)
  END;

  SymFile* = POINTER TO SymFileDesc;
  SymFileDesc* = RECORD (FileDesc)
    tag-: LONGINT;      (* file version (timestamp used)                   *)
    raw : RawFile;
    inp : SymFile;      (* previous version of file to compare with if any *)
    equ : BOOLEAN;      (* if contents is equal to 'inp' file *)
    errs*: BOOLEAN;      (* if TRUE, report read error *)
    bpos: LONGINT;      (* file position of the block *)
    blen: LONGINT;      (* block length *)
    pos : INTEGER;      (* position in buf *)
    len : INTEGER;      (* buf length (used for read only) *)
    buf : ARRAY 1024 OF SYSTEM.BYTE;
    fnames: ARRAY 256 OF String; (* File name of last text position *)
    fnm_no: INTEGER;
    fnm_ps: INTEGER;
  END;

  Manager* = POINTER TO ManagerDesc;
  ManagerDesc* = RECORD
    file*: File;
    msg *: String;
  END;

  SymManager* = POINTER TO SymManagerDesc;
  SymManagerDesc* = RECORD
    file*: SymFile;
    msg *: String;
  END;

  FileSys* = POINTER TO FileSysDesc;
  FileSysDesc* = RECORD
  END;

  DirIterator *= RECORD
  END;

CONST EOS = 0AX; (* SymFile: end of string *)

VAR
  sys* : FileSys;
  text*: Manager;
  raw* : Manager;
  sym- : SymManager;
  null-: String;

(*------------------- DirIterator ---------------------*)

PROCEDURE (VAR i: DirIterator) Entry*(name-: ARRAY OF CHAR; dir: BOOLEAN): BOOLEAN;
  (** ABSTRACT. Result TRUE means stop iteration now. *)
BEGIN
  RETURN TRUE;
END Entry;

(*--------------------  FileSys  ----------------------*)

PROCEDURE (fs: FileSys) CreateDir*(name-: ARRAY OF CHAR): BOOLEAN;
  (** ABSTRACT. TRUE means success *)
BEGIN
  RETURN FALSE;
END CreateDir;

PROCEDURE (fs: FileSys) RemoveDir*(name-: ARRAY OF CHAR): BOOLEAN;
  (** ABSTRACT. TRUE means success *)
BEGIN
  RETURN FALSE;
END RemoveDir;

PROCEDURE (fs: FileSys) IterateDir*(name-: ARRAY OF CHAR; VAR i: DirIterator): BOOLEAN;
  (** ABSTRACT. TRUE means success *)
BEGIN
  RETURN FALSE;
END IterateDir;

PROCEDURE (fs: FileSys) sLookup*(name: FNAME; VAR fname: FNAME);
  (** ABSTRACT *)
END sLookup;

PROCEDURE (fs: FileSys) Lookup*(name-: ARRAY OF CHAR; VAR fname: String);
  VAR f: FNAME;
BEGIN
  COPY(name,f);
  fs.sLookup(f,f);
  DStrings.Assign(f,fname);
END Lookup;

PROCEDURE (fs: FileSys) Exists*(name-: ARRAY OF CHAR): BOOLEAN;
  (** ABSTRACT *)
BEGIN
  RETURN FALSE
END Exists;

PROCEDURE (fs: FileSys) SysLookup*(ext-: ARRAY OF CHAR; VAR fname: String);
  (** ABSTRACT *)
END SysLookup;

PROCEDURE (fs: FileSys) SearchPath*(name-: ARRAY OF CHAR; VAR fname: String);
  (** ABSTRACT *)
END SearchPath;

PROCEDURE (fs: FileSys) sUseFirst*(name: FNAME; VAR fname: FNAME);
  (** ABSTRACT *)
END sUseFirst;

PROCEDURE (fs: FileSys) UseFirst*(name-: ARRAY OF CHAR; VAR fname: String);
  VAR f: FNAME;
BEGIN
  COPY(name,f);
  fs.sUseFirst(f,f);
  DStrings.Assign(f,fname);
END UseFirst;

PROCEDURE (fs: FileSys) sUseFirstDir*(name: FNAME; VAR dirname: FNAME);
  (** ABSTRACT *)
END sUseFirstDir;

PROCEDURE (fs: FileSys) UseFirstDir*(name-: ARRAY OF CHAR; VAR dirname: String);
  VAR f: FNAME;
BEGIN
  COPY(name,f);
  fs.sUseFirstDir(f,f);
  DStrings.Assign(f,dirname);
END UseFirstDir;

PROCEDURE (fs: FileSys) ParseRed*(line-: ARRAY OF CHAR; VAR pos: LONGINT);
  (** ABSTRACT; pos < 0 -- ok, else position of error in line *)
END ParseRed;

PROCEDURE (fs: FileSys) SaveRed*;
  (** ABSTRACT *)
END SaveRed;

PROCEDURE (fs: FileSys) RestoreRed*;
  (** ABSTRACT *)
END RestoreRed;

PROCEDURE (fs: FileSys) ModifyTime*(name-: ARRAY OF CHAR;
                               VAR  time: Time;
                               VAR exist: BOOLEAN);
  (** ABSTRACT *)
END ModifyTime;

PROCEDURE (fs: FileSys) ConvertToHost*(fr-: ARRAY OF CHAR; VAR to: String);
  (** ABSTRACT *)
END ConvertToHost;

PROCEDURE (fs: FileSys) ConvertToTarget*(fr-: ARRAY OF CHAR; VAR to: String);
  (** ABSTRACT *)
END ConvertToTarget;

PROCEDURE (fs: FileSys) ConvertFromHost*(fr-: ARRAY OF CHAR; VAR to: String);
  (** ABSTRACT *)
END ConvertFromHost;

PROCEDURE (fs: FileSys) ConvertCaseToHost*(fr-: ARRAY OF CHAR; VAR to: String);
  (** ABSTRACT *)
END ConvertCaseToHost;

PROCEDURE (fs: FileSys) ConvertCaseToTarget*(fr-: ARRAY OF CHAR; VAR to: String);
  (** ABSTARCT *)
END ConvertCaseToTarget;

PROCEDURE (fs: FileSys) GetFullPathName*(fr-: ARRAY OF CHAR; 
                                      VAR to: ARRAY OF CHAR);
BEGIN
  (** ABSTARCT *)
  COPY(fr, to)
END GetFullPathName;

(*----------------------------------------------------------------*)

PROCEDURE (fs: FileSys) sGet*(s-: ARRAY OF CHAR; VAR dir,name,ext: FNAME): BOOLEAN;
  (** ABSTRACT. Returns FALSE, if bad format *)
BEGIN RETURN FALSE;
END sGet;

PROCEDURE (fs: FileSys) sGetName*(s-: ARRAY OF CHAR; VAR name: FNAME): BOOLEAN;
  (** ABSTRACT. Returns FALSE, if bad format *)
BEGIN RETURN FALSE;
END sGetName;

PROCEDURE (fs: FileSys) sGetDir*(s-: ARRAY OF CHAR; VAR dir: FNAME): BOOLEAN;
  (** ABSTRACT. Returns FALSE, if bad format *)
BEGIN RETURN FALSE;
END sGetDir;

PROCEDURE (fs: FileSys) sGetExt*(s-: ARRAY OF CHAR; VAR ext: FNAME): BOOLEAN;
  (** ABSTRACT. Returns FALSE, if bad format *)
BEGIN RETURN FALSE;
END sGetExt;

PROCEDURE (fs: FileSys) sCreate*(dir-,name-,ext-: ARRAY OF CHAR; VAR fnm: FNAME): BOOLEAN;
  (** ABSTRACT. Returns FALSE, if bad format *)
BEGIN RETURN FALSE;
END sCreate;

PROCEDURE (fs: FileSys) Get*(s-: ARRAY OF CHAR; VAR dir,name,ext: String);
  (** Returns null, if bad format *)
  VAR d,n,e: FNAME;
BEGIN
  IF fs.sGet(s,d,n,e) THEN
    DStrings.Assign(d,dir);
    DStrings.Assign(n,name);
    DStrings.Assign(e,ext);
  ELSE
    dir:=null;
    name:=null;
    ext:=null;
  END;
END Get;

PROCEDURE (fs: FileSys) GetName*(s-: ARRAY OF CHAR; VAR name: String);
  (** Returns null, if bad format *)
  VAR n: FNAME;
BEGIN
  IF fs.sGetName(s,n) THEN
    DStrings.Assign(n,name);
  ELSE
    name:=null;
  END;
END GetName;

PROCEDURE (fs: FileSys) GetDir*(s-: ARRAY OF CHAR; VAR dir: String);
  (** Returns null, if bad format *)
  VAR n: FNAME;
BEGIN
  IF fs.sGetDir(s,n) THEN
    DStrings.Assign(n,dir);
  ELSE
    dir:=null;
  END;
END GetDir;

PROCEDURE (fs: FileSys) GetExt*(s-: ARRAY OF CHAR; VAR ext: String);
  (** Returns null, if bad format *)
  VAR n: FNAME;
BEGIN
  IF fs.sGetExt(s,n) THEN
    DStrings.Assign(n,ext);
  ELSE
    ext:=null;
  END;
END GetExt;

PROCEDURE (fs: FileSys) Create*(dir-,name-,ext-: ARRAY OF CHAR;
                                     VAR fname: String);
  (** Returns null, if bad format *)
  VAR n: FNAME;
BEGIN
  IF fs.sCreate(dir,name,ext,n) THEN
    DStrings.Assign(n,fname);
  ELSE
    fname:=null;
  END;
END Create;

PROCEDURE (fs: FileSys) VersionTag*(): LONGINT;
  (** ABSTRACT *)
BEGIN
  RETURN -1
END VersionTag;

PROCEDURE (fs: FileSys) CompareExtTarget * (ext1-, ext2-: ARRAY OF CHAR): BOOLEAN;
VAR e1, e2: String;
BEGIN
  fs.ConvertCaseToTarget(ext1, e1);
  fs.ConvertCaseToTarget(ext2, e2);
  RETURN e1^ = e2^;
END CompareExtTarget;


PROCEDURE (fs: FileSys) CompareExtHost * (ext1-, ext2-: ARRAY OF CHAR): BOOLEAN;
VAR e1, e2: String;
BEGIN
  fs.ConvertCaseToHost(ext1, e1);
  fs.ConvertCaseToHost(ext2, e2);
  RETURN e1^ = e2^;
END CompareExtHost;


(*----------------------  Manager  ------------------------*)

PROCEDURE (m: Manager) Open*(name-: ARRAY OF CHAR; new: BOOLEAN);
  (** ABSTRACT. Sets "name" and "new" fields *)
BEGIN
  ASSERT(m.file#NIL);
  NEW(m.file.name,LENGTH(name)+1);
  COPY(name,m.file.name^);
  m.file.new:=new;
END Open;

PROCEDURE (m: Manager) OpenRW*(name-: ARRAY OF CHAR);
  (** ABSTRACT. Sets "name" and "new" fields *)
BEGIN
  ASSERT(m.file#NIL);
  NEW(m.file.name,LENGTH(name)+1);
  COPY(name,m.file.name^);
  m.file.new:=FALSE;
END OpenRW;

(*----------------------  File  ------------------------*)

PROCEDURE (f: File) Close*;
  (** ABSTRACT. Closes old file. *)
BEGIN ASSERT(FALSE)
END Close;

(*----------------------  TextFile  ------------------------*)

PROCEDURE (f: TextFile) print*(format-: ARRAY OF CHAR;
                             SEQ args: SYSTEM.BYTE);
  (** ABSTRACT *)
END print;

PROCEDURE (f: TextFile) ReadString*(VAR s: ARRAY OF CHAR);
  (** ABSTRACT. Always appends 0X. *)
END ReadString;

PROCEDURE (f: TextFile) WriteString*(s-: ARRAY OF CHAR; len: LONGINT);
  (** CONCRETE. May not stop writing on 0X!, len is required! *)
BEGIN
  f.print("%.*s",len,s);
END WriteString;

PROCEDURE (f: TextFile) Length*(): LONGINT;
  (** ABSTRACT *)
BEGIN
  RETURN 0
END Length;

PROCEDURE (f: TextFile) Flush*();
  (** ABSTRACT *)
BEGIN
END Flush;

PROCEDURE (f: TextFile) CloseNew*(register,compare,search: BOOLEAN; VAR err: String);
  (** ABSTRACT. Closes created (temporary) file.
    If NOT register, removes the newly created file,
    otherwise compares it with old one (if compare).
    err=NIL, if success
    otherwise it contains error message
    If file is sucessfully compared with the old one,
    f.new should be set to FALSE.
  *)
BEGIN ASSERT(FALSE)
END CloseNew;

(*----------------------  RawFile  -------------------------*)

PROCEDURE (f: RawFile) ReadBlock*(VAR x: ARRAY OF SYSTEM.BYTE;
                                pos,len: LONGINT);
  (** ABSTRACT *)
END ReadBlock;

PROCEDURE (f: RawFile) WriteBlock*(VAR x: ARRAY OF SYSTEM.BYTE;
                                 pos,len: LONGINT);
  (** ABSTRACT *)
END WriteBlock;

PROCEDURE (f: RawFile) Length*(): LONGINT;
  (** ABSTRACT *)
BEGIN
  RETURN 0
END Length;

PROCEDURE (f: RawFile) GetPos*(): LONGINT;
  (** ABSTRACT *)
BEGIN
  RETURN 0
END GetPos;

PROCEDURE (f: RawFile) SetPos*(pos: LONGINT);
  (** ABSTRACT *)
END SetPos;

PROCEDURE (f: RawFile) CloseNew*(register,compare: BOOLEAN; VAR err: String);
  (** ABSTRACT. See comment to TextFile.CloseNew *)
BEGIN ASSERT(FALSE)
END CloseNew;

PROCEDURE (f: RawFile) GetDate(): Time;
BEGIN
    ASSERT(FALSE);
END GetDate;

PROCEDURE (f: RawFile) SetDate(new_date: Time);
BEGIN
    ASSERT(FALSE);
END SetDate;

(*----------------------  SymFile  -------------------------*)

PROCEDURE (m: SymManager) Open*(name-: ARRAY OF CHAR; new,compare: BOOLEAN);

  PROCEDURE SetCompareMode(f: SymFile);
  BEGIN
    f.equ:=TRUE;
(*
    VAR dir,name,ext,fn: String;
    sys.Get(f.name^,dir,name,ext);
    sys.Create('',name^,ext^,fn);
    sys.Lookup(fn^,fn);
    sym.Open(fn^,FALSE,FALSE);
*)
    sym.Open(f.name^,FALSE,FALSE);
    IF sym.file=NIL THEN
      f.inp:=NIL;
      f.equ:=FALSE;
    ELSE
      f.inp:=sym.file;
      f.equ:=TRUE;
      f.inp.errs:=FALSE;
    END;
  END SetCompareMode;

  VAR f: SymFile;
BEGIN
  raw.Open(name,new);
  IF raw.file#NIL THEN
    NEW(f);
    f.raw:=raw.file(RawFile);
    f.name:=raw.file.name;
    f.new:=new;
    f.errs:=TRUE;
    f.bpos:=-1; f.blen:=0;
    f.pos:=0; f.len:=0;
    f.fnm_no:=0;
    f.fnm_ps:=-1;
    IF compare THEN ASSERT(new); SetCompareMode(f) ELSE f.equ:=FALSE END;
    m.file:=f;
  ELSE
    m.file:=NIL;
    m.msg:=raw.msg
  END;
END Open;

PROCEDURE (m: SymManager) OpenRW*(name-: ARRAY OF CHAR);

  VAR f: SymFile;
BEGIN
  raw.OpenRW(name);
  ASSERT(raw.file#NIL);
  NEW(f);
  f.raw:=raw.file(RawFile);
  f.name:=raw.file.name;
  f.new:=FALSE;
  f.errs:=TRUE;
  f.bpos:=-1; f.blen:=0;
  f.pos:=0; f.len:=0;
  f.fnm_no:=0;
  f.fnm_ps:=-1;
  f.equ:=FALSE;
  m.file:=f;
END OpenRW;

(*----------------------------------------------------------------*)

PROCEDURE ReadError(f: SymFile);
VAR fname: String;
BEGIN
  sys.ConvertToHost(f.name^, fname);
  env.errors.Fault(env.null_pos,445,fname^);
END ReadError;

PROCEDURE (f: SymFile) Read*(VAR x: SYSTEM.BYTE);
BEGIN
  IF f.pos>=f.len THEN
    f.raw.ReadBlock(f.buf,0,LEN(f.buf));
    IF f.raw.readRes=allRight THEN
      f.pos:=0; f.len:=SHORT(f.raw.readLen);
    ELSE
      IF f.errs THEN ReadError(f) END;
      f.readRes:=endOfInput; f.readLen:=0;
      RETURN
    END;
  END;
  x:=f.buf[f.pos]; INC(f.pos); DEC(f.blen);
  f.readLen:=1; f.readRes:=allRight;
END Read;

PROCEDURE (f: SymFile) ReadInt*(VAR i: LONGINT);
  (** Reads packed integer *)
  VAR n: LONGINT; shift: SHORTINT; x: CHAR;
BEGIN
  IF f.pos<f.len THEN
    x:=SYSTEM.VAL(CHAR,f.buf[f.pos]); INC(f.pos); DEC(f.blen);
  ELSE
    f.Read(x);
  END;
  IF x>=80X THEN
    i:=ORD(x)-192;
    RETURN;
  END;
  shift:=7; n:=ORD(x); f.Read(x);
  WHILE x < 80X DO
    INC(n,ASH(ORD(x),shift));
    INC(shift,7);
    f.Read(x);
  END;
  i:=n+ASH(ORD(x)-192,shift);
END ReadInt;

PROCEDURE read(f: SymFile; VAR x: ARRAY OF SYSTEM.BYTE);
  VAR i: LONGINT; y: SYSTEM.BYTE;
BEGIN
  FOR i:=0 TO LEN(x)-1 DO f.Read(y); x[i]:=y END;
END read;

PROCEDURE (f: SymFile) ReadReal*(VAR x: LONGREAL);
BEGIN
  read(f,x);
END ReadReal;

PROCEDURE (f: SymFile) ReadSet*(VAR x: SET);
  (** Reads packed set. *)
  VAR i: LONGINT;
BEGIN
  f.ReadInt(i);
  x:=SYSTEM.VAL(SET,i);
END ReadSet;

PROCEDURE (f: SymFile) ReadString*(VAR s: ARRAY OF CHAR);
  (** Reads string: {character} LF *)
  VAR i: LONGINT; c: CHAR;
BEGIN
  i:=0;
  f.Read(c);
  IF f.readRes=allRight THEN
    LOOP
      IF (i>=LEN(s)) OR (c=EOS) THEN EXIT END;
      s[i]:=c; INC(i);
      IF f.pos>=f.len THEN
        f.Read(c);
        IF f.readRes#allRight THEN EXIT END;
	INC(f.blen);
      ELSE
        c:=SYSTEM.VAL(CHAR,f.buf[f.pos]); INC(f.pos);
      END;
    END;
  END;
  IF i<LEN(s) THEN s[i]:=0X ELSE f.readRes:=bufOverflow END;
  f.readLen:=i;
  DEC(f.blen,i);
END ReadString;

PROCEDURE (f: SymFile) ReadTPOS*(VAR ps: env.TPOS);
  VAR i,l,c: LONGINT; buf: FNAME;
BEGIN
  f.ReadInt(i);
  IF i<0 THEN
    f.ReadString(buf);
    f.ReadInt(i);
  ELSE
    buf[0]:=0X;
  END;
  f.ReadInt(l);
  f.ReadInt(c);
  IF buf#"" THEN ps.pack(buf,l,c); ps.unpack(f.fnames[i],l,c);
  ELSE ps.pack(f.fnames[i]^,l,c);
  END;
END ReadTPOS;

(*----------------------------------------------------------------*)

PROCEDURE Update(f: SymFile);
BEGIN
  f.raw.WriteBlock(f.buf,0,f.pos);
  f.pos:=0;
END Update;

PROCEDURE (f: SymFile) Update*;
BEGIN
  Update(f);
END Update;
PROCEDURE (f: SymFile) Write*(x: SYSTEM.BYTE);
  VAR y: SYSTEM.BYTE;
BEGIN
  IF f.pos>=LEN(f.buf) THEN Update(f) END;
  f.buf[f.pos]:=x; INC(f.pos); INC(f.blen);
  IF f.equ THEN
    f.inp.Read(y);
    f.equ:=(f.inp.readLen = 1) & (x = y);
  END;
END Write;


PROCEDURE (f: SymFile) WriteInt*(x: LONGINT);
  (** Writes packed integer *)
BEGIN
  WHILE (x<-64) OR (x>63) DO f.Write(CHR(x MOD 128)); x:=x DIV 128 END;
  f.Write(CHR(x+192));
END WriteInt;

PROCEDURE (f: SymFile) WriteTag*(x: LONGINT);
BEGIN
  IF f.equ THEN
    f.equ:=FALSE;
    f.WriteInt(x);
    f.equ:=TRUE;
    f.inp.ReadInt(f.tag);
  ELSE
    f.WriteInt(x);
  END;
END WriteTag;

PROCEDURE write(f: SymFile; VAR x: ARRAY OF SYSTEM.BYTE);
  VAR i: LONGINT;
BEGIN
  FOR i:=0 TO LEN(x)-1 DO f.Write(x[i]) END;
END write;

PROCEDURE (f: SymFile) WriteReal*(x: LONGREAL);
BEGIN
  write(f,x);
END WriteReal;

PROCEDURE (f: SymFile) ReadDWord*(VAR x: SYSTEM.CARD32);
BEGIN
  read(f,x);
END ReadDWord;

PROCEDURE (f: SymFile) WriteDWord*(VAR x: SYSTEM.CARD32);
BEGIN
  write(f,x);
END WriteDWord;

PROCEDURE (f: SymFile) WriteDWordTag*(x: SYSTEM.CARD32);
VAR dummy: SYSTEM.CARD32;
BEGIN
  IF f.equ THEN
    f.equ:=FALSE;
    f.WriteDWord(x);
    f.equ:=TRUE;
    f.inp.ReadDWord(dummy);
  ELSE
    f.WriteDWord(x);
  END;
END WriteDWordTag;

PROCEDURE (f: SymFile) WriteSet*(x: SET);
BEGIN
  f.WriteInt(SYSTEM.VAL(LONGINT,x))
END WriteSet;

PROCEDURE (f: SymFile) WriteString*(s-: ARRAY OF CHAR);
  (** Writes string: {character} LF *)
  VAR i: LONGINT;
BEGIN
  i:=0;
  WHILE (i<LEN(s)) & (s[i]#0X) DO f.Write(s[i]); INC(i) END;
  f.Write(EOS);
END WriteString;

PROCEDURE (f: SymFile) WriteTPOS*(ps-: env.TPOS);
  VAR nm: String; i: INTEGER; l,c: LONGINT; buf: FNAME; e: BOOLEAN;
BEGIN
  e:=f.equ; f.equ:=FALSE;
  ps.unpack(nm,l,c);
  IF (f.fnm_no=0) OR (nm^#f.fnames[f.fnm_ps]^) THEN
    i:=f.fnm_ps;
    LOOP
      INC(i);
      IF i>=f.fnm_no THEN i:=0 END;
      IF (f.fnm_no=0) OR (i=f.fnm_ps) THEN
        f.fnames[f.fnm_no]:=nm;
	INC(f.fnm_no);
	f.WriteInt(-1);
	f.WriteString(nm^);
	f.fnm_ps:=f.fnm_no-1;
	EXIT;
      END;
      IF nm^=f.fnames[i]^ THEN f.fnm_ps:=i; EXIT END;
    END;
  END;
  f.WriteInt(f.fnm_ps);
  f.WriteInt(l);
  f.WriteInt(c);
  IF e THEN
    f.inp.ReadInt(l);
    IF l<0 THEN
      f.inp.ReadString(buf);
      f.inp.ReadInt(l);
    END;
    f.inp.ReadInt(l);
    f.inp.ReadInt(l);
    f.equ:=TRUE;
  END;
END WriteTPOS;

(*----------------------------------------------------------------*)

PROCEDURE (f: SymFile) OpenBlock*;
  VAR x,y: CHAR;
BEGIN
  ASSERT(f.bpos < 0);
  IF f.new THEN
    f.bpos:=f.raw.GetPos() + f.pos;
    IF f.equ THEN
      f.inp.Read(x); f.inp.Read(y);
      f.inp.blen:=ORD(x)+ORD(y)*100H;
      f.equ:=FALSE;
	f.Write(0); f.Write(0);
      f.equ:=TRUE;
    ELSE
      f.Write(0); f.Write(0);
    END;
    f.blen:=0;
  ELSE
    f.bpos:=0;
    f.Read(x); f.Read(y);
    f.blen:=ORD(x)+ORD(y)*100H;
  END;
END OpenBlock;

PROCEDURE (f: SymFile) CloseBlock*;
  VAR pos: LONGINT; x: SYSTEM.BYTE;
BEGIN
  ASSERT(f.bpos >= 0);
  IF f.new THEN
    IF f.blen>0 THEN
      pos:=f.raw.GetPos();
      IF f.bpos>=pos THEN (* block length is in the buffer *)
	pos:=f.bpos-pos;
	f.buf[pos  ]:=VAL(CHAR,f.blen MOD 256);
	f.buf[pos+1]:=VAL(CHAR,f.blen DIV 256);
      ELSE
	f.raw.SetPos(f.bpos);
	x:=VAL(CHAR,f.blen MOD 256); f.raw.WriteBlock(x,0,1);
	x:=VAL(CHAR,f.blen DIV 256); f.raw.WriteBlock(x,0,1);
	f.raw.SetPos(pos);
      END;
    END;
    f.equ:=f.equ & (f.inp.blen = 0); (* end of input block *)
  ELSE
    IF f.blen#0 THEN ReadError(f) END;
  END;
  f.bpos:=-1;
END CloseBlock;

PROCEDURE (f: SymFile) SkipBlock*;
  VAR len: LONGINT; x: CHAR;
BEGIN
  ASSERT(~ f.new);
  f.Read(x); len:=ORD(x);
  f.Read(x); len:=len+ORD(x)*100H;
  WHILE len>0 DO f.Read(x); DEC(len) END;
END SkipBlock;

(*----------------------------------------------------------------*)

PROCEDURE (f: SymFile) Close*;
  (** Closes old file. *)
BEGIN
  ASSERT(~ f.new);
  ASSERT(f.inp=NIL);
  f.raw.Close;
END Close;

PROCEDURE (f: SymFile) CloseNew*(overwrite: BOOLEAN; VAR err: String): BOOLEAN;

(** overwrite - permission to overwrite existing sym file    *)
(** OUT err   - contains message string if any               *)
(** returns if symfile was updated or not                    *)

(** Assumptions:                                             *)
(**  - f.new should be TRUE, that means that file was        *)
(**    created by .Open for output .sym file information     *)
(**  - f.equ here means if files are equal in terms          *)
(**    of structure enities already writed to 'f'            *)
(**  - f.inp could be NIL not only when there is no previous *)
(**    version of file but when we don't want to bother      *)
(**    ourself with .sym file comparison (like M2CMPSYM-)    *)

(** SideEffects:                                             *)
(**  - f.equ could stand FALSE if there was no               *)
(**    previous version of file or that version was longer   *)
(**  - f.new could stand FALSE if files are equal            *)

VAR
  tmp    : SYSTEM.BYTE;
  updated: BOOLEAN;
BEGIN
  ASSERT(f.new);
  IF f.pos>0 THEN Update(f) END; (* last block wasn't flushed yet - flush it now *)
  IF f.inp=NIL THEN
    overwrite := TRUE; (* if there was no old file to compare with given one we don't need a permission to overwrite it *)
    f.equ     := FALSE
  ELSIF f.equ THEN
    f.inp.Read(tmp);
    f.equ := (f.inp.readLen = 0);
    (* This check ensures that there are no extra content in old file *)
  END;
  f.new := NOT f.equ;
  updated := overwrite AND f.new;
  IF f.inp # NIL THEN f.inp.Close; f.inp:=NIL END;

  (* we don't compare binary files here because we already have done it *)
  (* and f.equ contains result of comparison                            *)
  f.raw.CloseNew(updated ,FALSE, err);
  RETURN updated;
END CloseNew;

BEGIN
  NEW(sym);
  DStrings.Assign("",null);
END xiFiles.
