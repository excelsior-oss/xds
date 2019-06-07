(** Copyright (c) 1994,97 XDS Ltd, Russia. All Rights Reserved. *)
(** o2/m2 development system v2.0 *)
(** C library based implementation of File Managers *)
(** Implements: Raw and Text File Manager *)

<*+ O2EXTENSIONS *>

MODULE xmCFM; (* Ned 23-Feb-94. *)

IMPORT
   SYSTEM
  ,xfs:=xiFiles
  ,env:=xiEnv
  ,stdio
  ,io
  ,fcntl
  ,DStrings
  ,FormOut
  ,FormStr
  ,xcStr
  ,x2cLib
  ,Dirs
  ;

CONST
  TBUF_LEN = 2000H;

TYPE
  String = xfs.String;
  TextFile = POINTER TO TextFileDesc;
  TextFileDesc = RECORD (xfs.TextFileDesc)
    cf    : SYSTEM.int;
    temp  : String;
    eol   : BOOLEAN;
    fst   : BOOLEAN;
    eol_ch: CHAR;
    buf   : ARRAY TBUF_LEN OF CHAR;
    bpos  : INTEGER; 	(* next char position in buf 	*)
    blen  : INTEGER;	(* chars avalible in buf 	*)
    lines : LONGINT;	(* used in demo version		*)
    tcnt  : INTEGER;	(* used for TAB processing	*)
    cpos  : INTEGER;	(* line position of next char 	*)
    tabs  : INTEGER;	(* tabstop value		*)
  END;

  RawFile = POINTER TO RawFileDesc;
  RawFileDesc = RECORD (xfs.RawFileDesc)
    cf  : SYSTEM.int;
    temp: String;
  END;

  Raw = POINTER TO RawDesc;
  RawDesc = RECORD (xfs.ManagerDesc)
  END;

  Text = POINTER TO TextDesc;
  TextDesc = RECORD (xfs.ManagerDesc)
  END;

  Mode = ARRAY 4 OF CHAR;

VAR
  RT,WT : Mode;
  tempNo: INTEGER;

(*----------------------  Managers  ------------------------*)

PROCEDURE MakeTempName(fname-: ARRAY OF CHAR; VAR temp: String);
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

PROCEDURE CloseError(name-: ARRAY OF CHAR);
BEGIN
  env.errors.Fault(env.null_pos,432,name);
END CloseError;

PROCEDURE IOerror(nm-,s-: ARRAY OF CHAR);
BEGIN
  env.errors.PrintMsg(env.null_pos,'f',"%s: %s error",nm,s);
END IOerror;

PROCEDURE FileError(f: xfs.File; s-: ARRAY OF CHAR);
BEGIN
  env.errors.PrintMsg(env.null_pos,'f',"%s: %s error",f.name^,s);
END FileError;

PROCEDURE fopen(nm-: ARRAY OF CHAR; wr: BOOLEAN): SYSTEM.int;
  VAR fnm: String; ac,md: SYSTEM.int;
BEGIN
  IF wr THEN
    ac:=fcntl.O_RDWR+fcntl.O_BINARY+fcntl.O_CREAT;
    md:=fcntl.S_IREAD+fcntl.S_IWRITE;
    <* IF __GEN_C__ THEN *>
      SYSTEM.CODE("#ifdef S_IRGRP");
      md:=md+fcntl.S_IRGRP;
      SYSTEM.CODE("#endif");
      SYSTEM.CODE("#ifdef S_IWGRP");
      md:=md+fcntl.S_IWGRP;
      SYSTEM.CODE("#endif");
      SYSTEM.CODE("#ifdef S_IROTH");
      md:=md+fcntl.S_IROTH;
      SYSTEM.CODE("#endif");
      SYSTEM.CODE("#ifdef S_IWOTH");
      md:=md+fcntl.S_IWOTH;
      SYSTEM.CODE("#endif");
    <* END *>
     (*+fcntl.S_IRGRP+fcntl.S_IWGRP*);
  ELSE
    ac:=fcntl.O_RDONLY+fcntl.O_BINARY;
    md:=0;
  END;
  xfs.sys.ConvertToHost(nm,fnm);
  RETURN io.open(fnm,ac,md);
END fopen;

PROCEDURE MoveFile(fname-,tname-: ARRAY OF CHAR; VAR done: BOOLEAN);
  VAR in,ou: SYSTEM.int;

  PROCEDURE close(VAR f: SYSTEM.int): BOOLEAN;
    VAR r: BOOLEAN;
  BEGIN
    r:=io.close(f)=0; f:=-1;
    RETURN r;
  END close;

  PROCEDURE cleanup;
    VAR b: BOOLEAN; nm: xfs.String;
  BEGIN
    IF in>=0 THEN b:=close(in) END;
    IF ou>=0 THEN b:=close(ou) END;
    xfs.sys.ConvertToHost(fname,nm);
    stdio.remove(nm^);
  END cleanup;

  VAR buf: String; len,len1: LONGINT; nm: xfs.String;
BEGIN
  done:=FALSE; ou:=-1;
  in:=fopen(fname,FALSE);
  IF in>=0 THEN
    ou:=fopen(tname,TRUE);
    IF ou>=0 THEN
      NEW(buf,8*1024);
      LOOP
	len:=io.read(in,SYSTEM.ADR(buf^),VAL(SYSTEM.size_t,LEN(buf^)));
	IF len = 0 THEN EXIT END;
	IF len < 0 THEN cleanup; IOerror(fname,"read") END;
	len1:=io.write(ou,SYSTEM.ADR(buf^),VAL(SYSTEM.size_t,len));
	IF len # len1 THEN cleanup; IOerror(tname,"write") END;
      END;
      IF ~ close(in) THEN cleanup; CloseError(fname) END;
      IF ~ close(ou) THEN cleanup; CloseError(tname) END;
      xfs.sys.ConvertToHost(fname,nm);
      done := (stdio.remove(nm^) = 0);
    ELSE
      IF ~ close(in) THEN cleanup; CloseError(fname) END;
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

PROCEDURE Compare(temp-,name-: ARRAY OF CHAR; search: BOOLEAN; VAR equal: BOOLEAN);
  VAR
    fn: String;
    eof,len,no,i: LONGINT;
    old,new: SYSTEM.int;
    bf1,bf2: ARRAY 4096 OF CHAR;
BEGIN
  equal:=FALSE;
  IF search THEN
    OldName(name,fn);
    old:=fopen(fn^,FALSE);
  ELSE
    old:=fopen(name,FALSE);
  END;
  new:=fopen(temp,FALSE);
  IF (old>=0) & (new>=0) THEN
    eof:=io.lseek(new,0,io.SEEK_END);
    len:=io.lseek(old,0,io.SEEK_END);
    IF eof = len THEN
      io.lseek(new,0,io.SEEK_SET);
      io.lseek(old,0,io.SEEK_SET);
      LOOP
        IF eof=0 THEN EXIT END;
        IF eof>LEN(bf1) THEN len:=LEN(bf1) ELSE len:=eof END;
	no:=io.read(new,SYSTEM.ADR(bf1),VAL(SYSTEM.size_t,len));
	IF no#len THEN EXIT END;
	no:=io.read(old,SYSTEM.ADR(bf2),VAL(SYSTEM.size_t,len));
	IF no#len THEN EXIT END;
        FOR i:=0 TO len-1 DO
          IF bf1[i]#bf2[i] THEN EXIT END;
        END;
	DEC(eof,len);
      END;
      equal:=(eof = 0);
    END;
  END;
  IF old>=0 THEN io.close(old) END;
  IF new>=0 THEN io.close(new) END;
END Compare;

PROCEDURE CloseNew(f: xfs.File;
		  cf: SYSTEM.int;
		  temp: String;
		  reg,comp,search: BOOLEAN;
		  VAR err: String);
  VAR done: BOOLEAN; md: CHAR; nm,nn: xfs.String;
BEGIN
  IF io.close(cf)#0 THEN CloseError(temp^) END;
  IF reg THEN
    IF comp THEN Compare(temp^,f.name^,search,comp) END;
    IF comp THEN
      f.new:=FALSE;
      xfs.sys.ConvertToHost(temp^,nm);
      done:=(stdio.remove(nm^) = 0);
    ELSE
      xfs.sys.ConvertToHost(f.name^,nm);
      xfs.sys.ConvertToHost(temp^,nn);
      stdio.remove(nm^);
      done:=(stdio.rename(nn^,nm^) = 0);
      IF ~ done THEN MoveFile(temp^,f.name^,done) END;
      IF done THEN
	IF f IS TextFile THEN md:='T' ELSE md:='R' END;
	x2cLib.X2C_SetFileModes(f.name^,md);
      END;
    END;
  ELSE
    xfs.sys.ConvertToHost(temp^,nm);
    done:=(stdio.remove(nm^) = 0);
  END;
  IF done THEN err:=NIL ELSE err:=f.name END;
END CloseNew;

(*----------------------------------------------------------------*)

PROCEDURE (m: Raw) Open(name-: ARRAY OF CHAR; new: BOOLEAN);
  VAR f: RawFile; cf: SYSTEM.int; temp: String;
BEGIN
  m.file:=NIL;
  IF new THEN
    MakeTempName(name,temp);
    cf:=fopen(temp^,TRUE);
  ELSE
    temp:=NIL;
    cf:=fopen(name,FALSE);
  END;
  IF cf>=0 THEN
    NEW(f); f.cf:=cf; f.temp:=temp;
    m.file:=f;
    m.Open^(name,new);
  ELSE
    DStrings.Assign(name,m.msg);
  END;
END Open;

PROCEDURE (m: Text) Open(name-: ARRAY OF CHAR; new: BOOLEAN);
  VAR f: TextFile; cf: SYSTEM.int; temp,tabs: String; ltabs: LONGINT;
BEGIN
  m.file:=NIL;
  IF new THEN
    MakeTempName(name,temp);
    cf:=fopen(temp^,TRUE);
  ELSE
    temp:=NIL;
    cf:=fopen(name,FALSE);
  END;
  IF cf>=0 THEN
    NEW(f);
    env.config.Equation("TABSTOP",tabs);
    IF (tabs=NIL) OR ~ xcStr.StrToInt(tabs^,ltabs) THEN ltabs:=8 END;
<* IF env_target="x86nt" THEN *>
    IF env.shell.Active() THEN ltabs:=1 END;
<* END *>
    f.bpos:=0; f.blen:=0;
    f.cpos:=0; f.tcnt:=0;
    f.eol_ch:=0X; f.fst:=TRUE;
    f.cf:=cf; f.temp:=temp;
    f.eol:=FALSE; f.lines:=0;
    f.tabs:=SHORT(ltabs);
    m.file:=f; m.Open^(name,new);
  ELSE
    DStrings.Assign(name,m.msg);
  END;
END Open;

(*----------------------  TextFile  ------------------------*)

PROCEDURE (f: TextFile) ReadString(VAR s: ARRAY OF CHAR);
  VAR i,e,b: INTEGER; no: LONGINT; ch: CHAR;
BEGIN
  ASSERT(NOT f.new);
  IF f.eol THEN
    s[0]:=0X; f.readLen:=0; f.readRes:=xfs.endOfLine; f.eol:=FALSE;
  ELSE
    i:=0;
    LOOP
      e:=i+f.blen-f.bpos;
      IF e>=LEN(s) THEN e:=SHORT(LEN(s)-1) END;
      IF f.tcnt>0 THEN e:=i END;
      b:=i;
      LOOP
        IF i=e THEN EXIT END;
        ch:=f.buf[f.bpos];
        IF ch<' ' THEN EXIT END;
        s[i]:=ch; INC(f.bpos); INC(i);
      END;
      IF b#i THEN
        INC(f.cpos,i-b);
      ELSE
        IF i=LEN(s)-1 THEN
          s[i]:=0X; f.readRes:=xfs.allRight; f.readLen:=i; EXIT;
        END;
        IF f.tcnt>0 THEN
          s[i]:=' '; INC(i); INC(f.cpos); DEC(f.tcnt);
        ELSE
          IF f.bpos>=f.blen THEN
            IF f.fst THEN
              no:=io.read(f.cf,SYSTEM.ADR(f.buf),TBUF_LEN DIV 8);
              f.fst:=FALSE;
            ELSE
              no:=io.read(f.cf,SYSTEM.ADR(f.buf),TBUF_LEN);
            END;
            IF no<0 THEN FileError(f,"read") END;
            f.blen:=SHORT(no); f.bpos:=0;
            IF no=0 THEN
              s[i]:=0X; f.readLen:=i;
              IF i>0 THEN f.readRes:=xfs.allRight;
              ELSE f.readRes:=xfs.endOfInput;
	      END;
	      EXIT;
	    END;
	  END;
	  ch:=f.buf[f.bpos]; INC(f.bpos);
          IF ch<' ' THEN
            IF ch=09X THEN
	      f.tcnt:=(f.tabs-1) - f.cpos MOD f.tabs;
	      s[i]:=' '; INC(i); INC(f.cpos);
            ELSIF (ch=0AX) & (f.eol_ch#0DX) OR (ch=0DX) OR (ch=1EX) THEN
	      s[i]:=0X; f.readLen:=i; f.cpos:=0; f.eol_ch:=ch;
	      IF i>0 THEN f.readRes:=xfs.allRight; f.eol:=TRUE;
	      ELSE f.readRes:=xfs.endOfLine;
	      END;
	      EXIT;
            END;
          ELSE
	    s[i]:=ch; INC(i); INC(f.cpos);
          END;
        END;
        f.eol_ch:=0X;
      END;
    END;
  END;
END ReadString;

PROCEDURE [1] TextWrite(w: SYSTEM.ADDRESS; s-: ARRAY OF CHAR; len: LONGINT);
  VAR f: TextFile; locs,ps: LONGINT;
BEGIN
  f:=SYSTEM.VAL(TextFile,w);
  ps:=0;
  WHILE len>0 DO
    IF f.blen>=TBUF_LEN THEN
      locs:=io.write(f.cf,SYSTEM.ADR(f.buf),TBUF_LEN);
      IF locs#TBUF_LEN THEN FileError(f,"write") END;
      f.blen:=0;
    END;
    locs:=len;
    IF locs>TBUF_LEN-f.blen THEN locs:=TBUF_LEN-f.blen END;
    SYSTEM.MOVE(SYSTEM.ADR(s[ps]),SYSTEM.ADR(f.buf[f.blen]),locs);
    INC(ps,locs); DEC(len,locs); INC(f.blen,SHORT(locs));
  END;
END TextWrite;

PROCEDURE (f: TextFile) print(format-: ARRAY OF CHAR; SEQ args: SYSTEM.BYTE);
BEGIN
  ASSERT(f.new);
  FormOut.format(f,TextWrite,format,FormOut.default,SYSTEM.ADR(args),SIZE(args));
END print;

PROCEDURE (f: TextFile) WriteString(s-: ARRAY OF CHAR; len: LONGINT);
BEGIN
  TextWrite(f,s,len);
END WriteString;

PROCEDURE (f: TextFile) Length(): LONGINT;
  VAR pos,len: LONGINT;
BEGIN
  pos:=io.lseek(f.cf,0,io.SEEK_CUR);
  len:=io.lseek(f.cf,0,io.SEEK_END);
  IF pos#len THEN io.lseek(f.cf,pos,io.SEEK_SET) END;
  RETURN len
END Length;

PROCEDURE (f: TextFile) Close;
BEGIN
  ASSERT(~ f.new);
  IF io.close(f.cf)#0 THEN CloseError(f.name^) END;
END Close;

PROCEDURE (f: TextFile) CloseNew(register,compare,search: BOOLEAN; VAR err: String);
  VAR locs: LONGINT;
BEGIN
  ASSERT(f.new);
  IF f.blen>0 THEN
    locs:=io.write(f.cf,SYSTEM.ADR(f.buf),VAL(SYSTEM.size_t,f.blen));
    IF locs#LONG(f.blen) THEN FileError(f,"write") END;
    f.blen:=0;
  END;
  CloseNew(f,f.cf,f.temp,register,compare,search,err);
END CloseNew;

PROCEDURE (f: TextFile) Flush();
  VAR locs: LONGINT;
BEGIN
  IF f.new & (f.blen>0) THEN
    locs:=io.write(f.cf,SYSTEM.ADR(f.buf),VAL(SYSTEM.size_t,f.blen));
    IF locs#LONG(f.blen) THEN FileError(f,"write") END;
    f.blen:=0;
  END;
END Flush;

(*----------------------  RawFile  -------------------------*)

PROCEDURE (f: RawFile) ReadBlock(VAR x: ARRAY OF SYSTEM.BYTE;
			       pos,len: LONGINT);
  VAR locs: LONGINT;
BEGIN
  locs:=io.read(f.cf,SYSTEM.ADR(x[pos]),VAL(SYSTEM.size_t,len));
  f.readLen:=locs;
  IF (locs = 0) & (len > 0) THEN f.readRes:=xfs.endOfInput
  ELSIF locs<0 THEN FileError(f,"read")
  ELSE f.readRes:=xfs.allRight
  END;
END ReadBlock;

PROCEDURE (f: RawFile) WriteBlock(VAR x: ARRAY OF SYSTEM.BYTE;
				pos,len: LONGINT);
  VAR locs: LONGINT;
BEGIN
  locs:=io.write(f.cf,SYSTEM.ADR(x[pos]),VAL(SYSTEM.size_t,len));
  IF locs#len THEN FileError(f,"write") END;
END WriteBlock;

PROCEDURE (f: RawFile) Length(): LONGINT;
  VAR pos,len: LONGINT;
BEGIN
  pos:=io.lseek(f.cf,0,io.SEEK_CUR);
  len:=io.lseek(f.cf,0,io.SEEK_END);
  IF pos#len THEN io.lseek(f.cf,pos,io.SEEK_SET) END;
  RETURN len
END Length;

PROCEDURE (f: RawFile) GetPos(): LONGINT;
BEGIN
  RETURN io.lseek(f.cf,0,io.SEEK_CUR);
END GetPos;

PROCEDURE (f: RawFile) SetPos(pos: LONGINT);
BEGIN
  io.lseek(f.cf,pos,io.SEEK_SET);
END SetPos;

PROCEDURE (f: RawFile) Close;
BEGIN
  ASSERT(~ f.new);
  IF io.close(f.cf)#0 THEN CloseError(f.name^) END;
END Close;

PROCEDURE (f: RawFile) CloseNew(register,compare: BOOLEAN; VAR err: String);
BEGIN
  ASSERT(f.new);
  CloseNew(f,f.cf,f.temp,register,compare,TRUE,err);
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
  RT:="rb";
  WT:="w+b"
END xmCFM.
