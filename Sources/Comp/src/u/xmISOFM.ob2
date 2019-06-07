(** Copyright (c) 1994,97 XDS Ltd, Russia. All Rights Reserved. *)
(** o2/m2 development system v2.0 *)
(** ISO based implementation of File Managers *)
(** Implements: Raw, Sym and Text File Manager *)

<*+ O2EXTENSIONS *>

MODULE xmISOFM; (* Ned 23-Feb-94. *)

IMPORT
   SYSTEM
  ,xfs:=xiFiles
  ,IOChan
  ,RndFile
  ,cc:=ChanConsts
  ,IOConsts
  ,QFile        (* pseudo ISO *)
  ,FileSys
  ,FormOut
  ,FormStr
  ,DStrings
  ,env:=xiEnv
  ,xcStr
  ,xFilePos
  ,Dirs
  ;

TYPE
  CARDINAL = SYSTEM.CARD32;

  ChanId = IOChan.ChanId;
  String = xfs.String;

  TextFile = POINTER TO TextFileDesc;
  TextFileDesc = RECORD (xfs.TextFileDesc)
    cid  : ChanId;
    temp : String;
    buf  : String;
    pos  : LONGINT;
    col  : INTEGER;
    tab  : INTEGER;
    tabs : INTEGER;
    lines: LONGINT;    (* used in demo version         *)
  END;

  RawFile = POINTER TO RawFileDesc;
  RawFileDesc = RECORD (xfs.RawFileDesc)
    cid : ChanId;
    temp: String;
  END;

  Raw = POINTER TO RawDesc;
  RawDesc = RECORD (xfs.ManagerDesc)
  END;

  Text = POINTER TO TextDesc;
  TextDesc = RECORD (xfs.ManagerDesc)
  END;

VAR tempNo: INTEGER;

(*----------------------  Managers  ------------------------*)

PROCEDURE SetMessage(VAR x: String; name: ARRAY OF CHAR; res: cc.OpenResults);
  VAR str: ARRAY 32 OF CHAR;
      msg: ARRAY 128 OF CHAR;
BEGIN
  CASE res OF
   |cc.opened:            ASSERT(FALSE);
   |cc.wrongNameFormat:   COPY("wrong name format",str)
   |cc.wrongFlags:        COPY("wrong flags",str)
   |cc.tooManyOpen:       COPY("too many open",str)
   |cc.outOfChans:        COPY("out of channels",str)
   |cc.wrongPermissions:  COPY("wrong permissions",str)
   |cc.noRoomOnDevice:    COPY("no room on device",str)
   |cc.noSuchFile:        COPY("no such file",str)
   |cc.fileExists:        COPY("file exists",str)
   |cc.wrongFileType:     COPY("wrong file type",str)
   |cc.noTextOperations:  COPY("no text operations",str)
   |cc.noRawOperations:   COPY("no raw operations",str)
   |cc.noMixedOperations: COPY("no mixed operations",str)
   |cc.alreadyOpen:       COPY("already opened",str)
   |cc.otherProblem:      COPY("other problem (no memory)",str)
  ELSE                    COPY("unknown result",str)
  END;
  FormStr.print(msg,'"%s" %s',name,str);
  NEW(x,LENGTH(msg)+1);
  COPY(msg,x^);
END SetMessage;

PROCEDURE MakeTempName(fname: ARRAY OF CHAR; VAR temp: String);
  VAR dir,name,ext,a: String; s,e: ARRAY 32 OF CHAR;
BEGIN
  xfs.sys.Get(fname,dir,name,ext);
  FormStr.print(s,"xtmp_%s",ext^);
  FormStr.print(e,"%d",tempNo);
  IF env.config.Option("MAKEDIRS") THEN 
    xfs.sys.ConvertToHost(dir^,a);
    IF Dirs.mkdirs(a) THEN END;
  END;
  xfs.sys.Create(dir^,s,e,temp);
--tty.print('temp: %s\n',temp^);
  tempNo:=(tempNo+1) MOD 1000;
END MakeTempName;

PROCEDURE MoveFile(fname-,tname-: ARRAY OF CHAR; VAR done: BOOLEAN);
  VAR in,ou: ChanId; buf: String; res: cc.OpenResults; locs: CARDINAL;
    a: xfs.String;
BEGIN
  xfs.sys.ConvertToHost(fname,a);
  RndFile.OpenOld(in,a^,cc.read+cc.raw,res);
  IF res = cc.opened THEN
    xfs.sys.ConvertToHost(tname,a);
    RndFile.OpenClean(ou,a^,cc.write+cc.raw+cc.old,res);
    IF res = cc.opened THEN
      NEW(buf,8*1024);
      LOOP
        IOChan.RawRead(in,SYSTEM.ADR(buf^),LEN(buf^),locs);
        IF locs = 0 THEN EXIT END;
        IOChan.RawWrite(ou,SYSTEM.ADR(buf^),locs);
      END;
      RndFile.Close(in);
      RndFile.Close(ou);
      xfs.sys.ConvertToHost(fname,a);
      FileSys.Remove(a^,done);
    ELSE
      RndFile.Close(in);
    END;
  END;
END MoveFile;

PROCEDURE OldName(fname-: ARRAY OF CHAR; VAR fn: xfs.String);
  VAR dir,name,ext: String;
BEGIN
  xfs.sys.Get(fname,dir,name,ext);
  xfs.sys.Create('',name^,ext^,fn);
  xfs.sys.Lookup(fn^,fn);
END OldName;

PROCEDURE Compare(temp-,old-: ARRAY OF CHAR; search: BOOLEAN; VAR equal: BOOLEAN);
  VAR fn: String; c1,c2: ChanId; i: LONGINT;
    a,b: ARRAY 512 OF CHAR; locs1,locs2: CARDINAL;
    res: cc.OpenResults;
BEGIN
  equal:=FALSE;
  IF search THEN
    OldName(old,fn);
    xfs.sys.ConvertToHost(fn^,fn);
    QFile.OpenRead(c1,fn^,cc.read+cc.raw,res);
  ELSE
    xfs.sys.ConvertToHost(old,fn);
    QFile.OpenRead(c1,fn^,cc.read+cc.raw,res);
  END;
  IF res = cc.opened THEN
    xfs.sys.ConvertToHost(temp,fn);
    QFile.OpenRead(c2,fn^,cc.read+cc.raw,res);
    IF res = cc.opened THEN
      IF QFile.Length(c1)=QFile.Length(c2) THEN
        LOOP
          IOChan.RawRead(c1,SYSTEM.ADR(a),LEN(a),locs1);
          IOChan.RawRead(c2,SYSTEM.ADR(b),LEN(b),locs2);
          ASSERT(locs1=locs2);
          IF locs1 = 0 THEN equal:=TRUE; EXIT END;
          FOR i:=0 TO VAL(LONGINT,locs1)-1 DO
            IF a[i]#b[i] THEN EXIT END;
          END;
        END;
      END;
      QFile.Close(c1);
      QFile.Close(c2);
    ELSE QFile.Close(c1);
    END;
  END;
--tty.print('compare(%s,%s): equal=%d\n',temp,old,equal);
END Compare;

PROCEDURE CloseNew(f: xfs.File; temp: String; reg,comp,search: BOOLEAN;
             VAR err: String);
  VAR done: BOOLEAN; a,b: String;
BEGIN
  IF reg THEN
    IF comp THEN Compare(temp^,f.name^,search,comp) END;
    IF comp THEN
      f.new:=FALSE;
      xfs.sys.ConvertToHost(temp^,a);
      FileSys.Remove(a^,done);
    ELSE
      xfs.sys.ConvertToHost(temp^,a);
      xfs.sys.ConvertToHost(f.name^,b);
      FileSys.Rename(a^,b^,done);
      IF ~ done THEN MoveFile(temp^,f.name^,done) END;
    END;
  ELSE
    xfs.sys.ConvertToHost(temp^,a);
    FileSys.Remove(a^,done);
  END;
  IF done THEN err:=NIL ELSE err:=f.name END;
END CloseNew;

(*----------------------------------------------------------------*)

PROCEDURE (m: Raw) Open(name: ARRAY OF CHAR; new: BOOLEAN);
  CONST
    write = cc.raw + cc.write + cc.old;
    read  = cc.raw + cc.read;
  VAR f: RawFile; cid: IOChan.ChanId; res: cc.OpenResults;
    temp: String;
    a: String;
BEGIN
  m.file:=NIL;
  IF new THEN
    MakeTempName(name,temp);
    xfs.sys.ConvertToHost(temp^,a);
    RndFile.OpenClean(cid,a^,write,res);
  ELSE
    temp:=NIL;
    xfs.sys.ConvertToHost(name,a);
    RndFile.OpenOld(cid,a^,read,res);
  END;
  IF res = cc.opened THEN
    NEW(f); f.cid:=cid; f.temp:=temp;
    m.file:=f;
    m.Open^(name,new);
  ELSE
    SetMessage(m.msg,name,res);
  END;
END Open;

PROCEDURE (m: Raw) OpenRW(name: ARRAY OF CHAR);
  CONST
    write = cc.raw + cc.write + cc.old;
    read  = cc.raw + cc.read;
  VAR f: RawFile; cid: IOChan.ChanId; res: cc.OpenResults;
    a: String;
BEGIN
  m.file:=NIL;
  xfs.sys.ConvertToHost(name,a);
  RndFile.OpenOld(cid,a^,read+write,res);
  IF res = cc.opened THEN
    NEW(f); f.cid:=cid; f.temp:=NIL;
    m.file:=f;
    m.OpenRW^(name);
  ELSE
    SetMessage(m.msg,name,res);
  END;
END OpenRW;

PROCEDURE PosToInt(p: RndFile.FilePos): LONGINT;
  VAR q: LONGINT; b :  BOOLEAN;
BEGIN
  b := xFilePos.PosToInt(q,p);
  ASSERT(b,100H);
  RETURN q;
END PosToInt;

PROCEDURE ReadText(f: TextFile);
  VAR len: LONGINT; locs: CARDINAL;
BEGIN
  len:=PosToInt(RndFile.EndPos(f.cid));
  NEW(f.buf,len);
  IOChan.RawRead(f.cid,SYSTEM.ADR(f.buf^),len,locs);
  ASSERT(VAL(CARDINAL,len)=locs);
  RndFile.Close(f.cid);
  f.pos:=0;
END ReadText;

PROCEDURE (m: Text) Open(name: ARRAY OF CHAR; new: BOOLEAN);
  CONST
    write = cc.text + cc.write + cc.old;
    read  = cc.raw + cc.read;
  VAR f: TextFile; cid: IOChan.ChanId; res: cc.OpenResults;
    temp,tabs: String; a: String; ltabs: LONGINT;
BEGIN
  m.file:=NIL;
  IF new THEN
    MakeTempName(name,temp);
    xfs.sys.ConvertToHost(temp^,a);
    RndFile.OpenClean(cid,a^,write,res);
  ELSE
    temp:=NIL;
    xfs.sys.ConvertToHost(name,a);
    RndFile.OpenOld(cid,a^,read,res);
  END;
  IF res = cc.opened THEN
    env.config.Equation("TABSTOP",tabs);
    IF (tabs=NIL) OR ~ xcStr.StrToInt(tabs^,ltabs) THEN ltabs:=8 END;
    IF (ltabs>MAX(INTEGER)) OR (ltabs<0) THEN ltabs:=8 END;
<* IF env_target="x86nt" THEN *>
    IF env.shell.Active() THEN ltabs:=1 END;
<* END *>
    NEW(f); f.cid:=cid; f.temp:=temp;
    m.file:=f;
    m.Open^(name, new);
    IF ~ new THEN ReadText(f) END;
    f.col:=0;
    f.tab:=0;
    f.tabs:=SHORT(ltabs);
    f.lines:=0;
  ELSE
    SetMessage(m.msg,a^,res);
  END;
END Open;

(*----------------------  TextFile  ------------------------*)

PROCEDURE (f: TextFile) ReadString(VAR s: ARRAY OF CHAR);
  VAR i,res: INTEGER; ch: CHAR;
BEGIN
  res:=xfs.allRight;
  i:=0;
  LOOP
    IF f.tab>0 THEN
      ch:=' '; DEC(f.tab);
    ELSE
      IF f.pos>=LEN(f.buf^) THEN res:=xfs.endOfInput; EXIT END;
      ch:=f.buf[f.pos];
      IF ch<' ' THEN
        IF (ch=1EX) OR (ch=0AX) OR (ch=0X) OR (ch=0DX) THEN
          res:=xfs.endOfLine; f.col:=0; EXIT;
        ELSIF ch=09X THEN
          f.tab:=(f.tabs-1) - f.col MOD f.tabs; ch:=" ";
        END;
      END;
      INC(f.pos);
    END;
    s[i]:=ch; INC(f.col); INC(i);
    IF i=LEN(s)-1 THEN EXIT END;
  END;
  s[i]:=0X;
  f.readLen:=i;
  f.readRes:=res;
  IF res = xfs.endOfLine THEN
    IF i > 0 THEN
      f.readRes:=xfs.allRight
    ELSE
      IF (f.buf[f.pos]=0DX) & (f.pos+1<LEN(f.buf^)) & (f.buf[f.pos+1]=0AX) THEN INC(f.pos,2);
      ELSE INC(f.pos);
      END;
    END;
  ELSIF res = xfs.endOfInput THEN
    IF i > 0 THEN f.readRes:=xfs.allRight END;
  END;
END ReadString;

PROCEDURE [1] TextWrite(w: SYSTEM.ADDRESS; s-: ARRAY OF CHAR; len: LONGINT);
  VAR f: TextFile;
BEGIN
  f:=SYSTEM.VAL(TextFile,w);
  IOChan.TextWrite(f.cid,SYSTEM.ADR(s),len);
END TextWrite;

PROCEDURE (f: TextFile) print(format: ARRAY OF CHAR;
			    SEQ args: SYSTEM.BYTE);
BEGIN
  FormOut.format(f,TextWrite,format,FormOut.default,SYSTEM.ADR(args),SIZE(args));
END print;

PROCEDURE (f: TextFile) Length(): LONGINT;
BEGIN
  IF ~ f.new THEN
    IF f.buf#NIL THEN RETURN LEN(f.buf^) END;
    RETURN 0;
  END;
  RETURN PosToInt(RndFile.EndPos(f.cid));
END Length;

PROCEDURE (f: TextFile) Close;
BEGIN
  ASSERT(~ f.new);
END Close;

PROCEDURE (f: TextFile) Flush;
BEGIN
  IF f.new THEN IOChan.Flush(f.cid) END;
END Flush;

PROCEDURE (f: TextFile) CloseNew(register,compare,search: BOOLEAN; VAR err: String);
BEGIN
  ASSERT(f.new);
  RndFile.Close(f.cid);
  CloseNew(f,f.temp,register,compare,search,err);
END CloseNew;

(*----------------------  RawFile  -------------------------*)

PROCEDURE (f: RawFile) ReadBlock(VAR x: ARRAY OF SYSTEM.BYTE;
                               pos,len: LONGINT);
  VAR locs: CARDINAL;
BEGIN
  ASSERT(len <= LEN(x));
  IOChan.RawRead(f.cid
                ,SYSTEM.ADDADR(SYSTEM.M2ADR(x),VAL(CARDINAL,pos))
                ,len
                ,locs);
  f.readLen:=locs;
  IF (locs = 0) & (len > 0) THEN f.readRes:=xfs.endOfInput
  ELSE f.readRes:=xfs.allRight
  END;
END ReadBlock;

PROCEDURE (f: RawFile) WriteBlock(VAR x: ARRAY OF SYSTEM.BYTE;
                                pos,len: LONGINT);
BEGIN
  ASSERT(len <= LEN(x));
  IOChan.RawWrite(f.cid
                 ,SYSTEM.ADDADR(SYSTEM.M2ADR(x),VAL(CARDINAL,pos))
                 ,len);
END WriteBlock;

PROCEDURE (f: RawFile) Length(): LONGINT;
BEGIN
  RETURN PosToInt(RndFile.EndPos(f.cid))
END Length;

PROCEDURE (f: RawFile) GetPos(): LONGINT;
BEGIN
  RETURN PosToInt(RndFile.CurrentPos(f.cid))
END GetPos;

PROCEDURE (f: RawFile) SetPos(pos: LONGINT);
  VAR npos: RndFile.FilePos;
BEGIN
  npos:=RndFile.NewPos(f.cid,pos,1,RndFile.StartPos(f.cid));
  RndFile.SetPos(f.cid,npos)
END SetPos;

PROCEDURE (f: RawFile) Close;
BEGIN
  ASSERT(~ f.new);
  RndFile.Close(f.cid);
END Close;

PROCEDURE (f: RawFile) GetDate(): xfs.Time;
BEGIN
  RETURN 0; (* !!! *)
END GetDate;

PROCEDURE (f: RawFile) SetDate(new_date: xfs.Time);
BEGIN       (* !!! *)
END SetDate;

PROCEDURE (f: RawFile) CloseNew(register,compare: BOOLEAN; VAR err: String);
BEGIN
  ASSERT(f.new);
  RndFile.Close(f.cid);
  CloseNew(f,f.temp,register,compare,TRUE,err);
END CloseNew;

(*----------------------------------------------------------*)

PROCEDURE SetManagers*;
  VAR raw: Raw; text: Text;
BEGIN
  NEW(raw);   xfs.raw:=raw;
  NEW(text);  xfs.text:=text;
END SetManagers;

BEGIN
  tempNo:=153;
END xmISOFM.
