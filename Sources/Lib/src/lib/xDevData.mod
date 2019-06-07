<* +M2EXTENSIONS *>
<* +STORAGE *>
IMPLEMENTATION MODULE xDevData; (* Hady. 13.06.96 13:46 *)

IMPORT  SYSTEM, xlibOS, xrInt64, platform,
        IOLink, cc:=ChanConsts,
        xmRTS, M2EXCEPTION,
        ioc:=IOConsts, IOChan
        ;

(*
есть несколько способов буфферизовать файлы:
    1) для нормальных файлов (с позиционированием) -
  один буффер для ввода и вывода, позиция в файле считается сразу
  за концом буфера
    2) для файлов без позиционирования, открытых только для ввода
  или только для вывода - все ясно
    3) для файлов типа терминала - возможна раздельная буферизация
  ввода и вывода (дуплекс)
*)

CONST (* End-Of-Line Characters *)
  LF = CHAR(12C);
  SEP0 = platform.SEP0;
  SEP1 = platform.SEP1;

CONST
  tag_modf     = 0;   (* buffer was modified *)
  tag_rnd      = 1;   (* handle allows seek operation *)
  tag_lbuf     = 2;   (* line buffering mode *)

TYPE
  ADDRESS = SYSTEM.ADDRESS;
  File = DevData;

TYPE Str10 = ARRAY [0..9] OF CHAR;

PROCEDURE word2str(VAR s: Str10; v: SYSTEM.WORD);
  VAR x,p,ch: CARDINAL;
BEGIN
  x:=SYSTEM.CAST(CARDINAL,v);
  s[9]:=0C; s[8]:="H";
  FOR p:=7 TO 0 BY -1 DO
    ch:=x MOD 16; x:=x DIV 16;
    IF ch>9 THEN s[p]:=CHR(ch-10+ORD("A"))
    ELSE s[p]:=CHR(ch+ORD("0"));
    END;
  END;
END word2str;

PROCEDURE HardError(x: Object; res: SYSTEM.WORD);
  VAR msg: Str10;
BEGIN
  x^.errNum:=SYSTEM.CAST(INTEGER,res);
  word2str(msg,res);
  IOLink.RAISEdevException(x^.cid,x^.did,IOChan.hardDeviceError,msg);
END HardError;

PROCEDURE SoftError(x: Object; res: SYSTEM.WORD);
  VAR msg: Str10;
BEGIN
  x^.errNum:=SYSTEM.CAST(INTEGER,res);
  word2str(msg,res);
  IOLink.RAISEdevException(x^.cid,x^.did,IOChan.softDeviceError,msg);
END SoftError;

PROCEDURE MakeName(VAR fn: FileName; name-: ARRAY OF CHAR; VAR res: OpenResults);
BEGIN
  NEW(fn,LENGTH(name)+1);
  IF fn=NIL THEN res:=cc.outOfChans;
  ELSE COPY(name,fn^); res:=cc.opened;
  END;
END MakeName;

PROCEDURE UnMakeName(VAR fn: FileName);
BEGIN
  IF fn#NIL THEN DISPOSE(fn); fn:=NIL END;
END UnMakeName;

(* =============== single file locks ============= *)

PROCEDURE lock(f: File);
BEGIN
  <* IF multithread THEN *> xmRTS.X2C_LockMutex(f^.lock); <* END*>
END lock;

PROCEDURE unlock(f: File);
BEGIN
  <* IF multithread THEN *> xmRTS.X2C_FreeMutex(f^.lock); <* END*>
END unlock;

(* =============== buffer operations ============= *)

(*
   Two sets of procedures are provided: 
   d*: Direct I/O, used if underlying API or C library is responsible for buffering 
   b*: Buffered I/O, implements buffering when the underlying I/O API is direct
*)

PROCEDURE dWrite(x: File; a: ADDRESS; size: CARDINAL; VAR wr: CARDINAL): INTEGER;
BEGIN
  RETURN xlibOS.X2C_fWrite(x^.cf,a,size,wr);
END dWrite;

PROCEDURE dRead(x: File; a: ADDRESS; size: CARDINAL; VAR rd: CARDINAL): INTEGER;
BEGIN
  RETURN xlibOS.X2C_fRead(x^.cf,a,size,rd);
END dRead;

PROCEDURE dFlush(x: File): INTEGER;
BEGIN
  RETURN 0;
END dFlush;

(* it is assumed that system file position is set to
  the end of the buffer *)

PROCEDURE bWrite(f: File; a: ADDRESS; size: CARDINAL; VAR wr: CARDINAL): INTEGER;
  VAR ch: CHAR; res: INTEGER; l: SYSTEM.CARD32;
BEGIN
  wr:=size;
  IF size=0 THEN RETURN 0 END;
  IF f^.bpos<BUFSIZE THEN
    REPEAT
      SYSTEM.GET(a,ch);
      f^.buf^[f^.bpos]:=ch;
      INC(f^.bpos); DEC(size);
      a:=SYSTEM.ADDADR(a,1);
    UNTIL (size=0) OR (f^.bpos>=BUFSIZE);
    INCL(f^.tags,tag_modf);
    IF f^.blen<f^.bpos THEN f^.blen:=f^.bpos END;
  END;
  IF size=0 THEN RETURN 0 END;
  res:=f^.flush(f);
  IF res#0 THEN wr:=0; RETURN res END;
  WHILE (size>BUFSIZE) DO
    res:=xlibOS.X2C_fWrite(f^.cf,a,BUFSIZE,l);
    a:=SYSTEM.ADDADR(a,l);
    size:=size-l;
    IF res#0 THEN wr:=wr-size; RETURN res END;
  END;
  ASSERT(size<=BUFSIZE);
  IF tag_rnd IN f^.tags THEN
    res:=xlibOS.X2C_fTell(f^.cf,f^.spos);
    IF res#0 THEN RETURN res END;
  END;
  l:=0;
  REPEAT
    SYSTEM.GET(a,ch);
    f^.buf^[l]:=ch;
    INC(l); DEC(size);
    a:=SYSTEM.ADDADR(a,1);
  UNTIL (size=0);
  INCL(f^.tags,tag_modf);
  f^.bpos:=l; f^.blen:=l;
  RETURN 0;
END bWrite;

PROCEDURE bRead(f: File; a: ADDRESS; size: CARDINAL; VAR rd: CARDINAL): INTEGER;
  VAR rr,sz: CARDINAL; res: INTEGER;
BEGIN
  rd:=size;
  IF size=0 THEN RETURN 0 END;
  WHILE (size>0) & (f^.bpos<f^.blen) DO
    SYSTEM.PUT(a,f^.buf^[f^.bpos]);
    a:=SYSTEM.ADDADR(a,SIZE(CHAR));
    INC(f^.bpos); DEC(size);
  END;
  IF size=0 THEN RETURN 0 END;
  res:=f^.flush(f);
  IF res#0 THEN RETURN res END;
  IF size>BUFSIZE THEN
    sz:=size - (size MOD BUFSIZE);
    res:=xlibOS.X2C_fRead(f^.cf,a,sz,rr);
    a:=SYSTEM.ADDADR(a,rr);
    size:=size-rr;
    IF (res#0) OR (rr#sz) THEN rd:=rd-size; RETURN res END;
  END;
  IF tag_rnd IN f^.tags THEN
    res:=xlibOS.X2C_fTell(f^.cf,f^.spos);
    IF res#0 THEN rd:=rd-size; RETURN res END;
  END;
  res:=xlibOS.X2C_fRead(f^.cf,f^.buf,BUFSIZE,f^.blen);
  IF res=0 THEN
    f^.bpos:=0;
    WHILE (size>0) & (f^.bpos<f^.blen) DO
      SYSTEM.PUT(a,f^.buf^[f^.bpos]);
      a:=SYSTEM.ADDADR(a,SIZE(CHAR));
      DEC(size); INC(f^.bpos);
    END;
  END;
  rd:=rd-size;
  RETURN res;
END bRead;

PROCEDURE bFlush(x: File): INTEGER;
  VAR res: INTEGER; wr: CARDINAL;
BEGIN
  IF tag_modf IN x^.tags THEN
    IF tag_rnd IN x^.tags THEN
      res:=xlibOS.X2C_fSeek(x^.cf,x^.spos,xlibOS.X2C_fSeekSet);
      IF res#0 THEN RETURN res END;
    END;
    res:=xlibOS.X2C_fWrite(x^.cf,x^.buf,x^.blen,wr);
    IF res#0 THEN RETURN res END;
    IF tag_rnd IN x^.tags THEN
      res:=xlibOS.X2C_fTell(x^.cf,x^.spos);
      IF res#0 THEN RETURN res END;
    END;
    EXCL(x^.tags,tag_modf);
  END;
  x^.blen:=0;
  x^.bpos:=0;
  RETURN 0;
END bFlush;

(* =============== RAW read/write ============= *)

PROCEDURE rawread(f: File; a: ADDRESS; size: CARDINAL; VAR rd: CARDINAL): INTEGER;
  VAR res: INTEGER;
BEGIN
  res:=0;
  IF tag_lbuf IN f^.tags THEN
    res:=f^.flush(f); IF res#0 THEN RETURN res END;
  END;
  rd:=size;
  IF size>0 THEN
    IF f^.ucnt>0 THEN
      REPEAT
        DEC(f^.ucnt);
        SYSTEM.PUT(a,f^.ubuf[f^.ucnt]);
        DEC(size);
        a:=SYSTEM.ADDADR(a,SIZE(CHAR));
      UNTIL (size=0) OR (f^.ucnt=0);
    END;
    IF size>0 THEN
      res:=f^.bread(f,a,size,rd);
    END;
  END;
  RETURN res;
END rawread;

PROCEDURE rawwrite(f: File; buf: ADDRESS; size: CARDINAL; VAR wr: CARDINAL): INTEGER;
BEGIN
  f^.ucnt:=0;
  RETURN f^.bwrite(f,buf,size,wr);
END rawwrite;

PROCEDURE rawungetc(f: File; ch: CHAR);
BEGIN
  f^.ubuf[f^.ucnt]:=ch;
  INC(f^.ucnt);
END rawungetc;

(* =============== TEXT read/write ============= *)

PROCEDURE convSEPS(VAR buf: ADDRESS; VAR size: CARDINAL; VAR last: CHAR);
  VAR g: ADDRESS; pch,ch: CHAR; cc: CARDINAL;
BEGIN
  (* Following line is to check for linesep pair. The conversion
     algorhythm is implicitly assumes that SEP0 # LF, otherwice it can
     produce invalid results *)
  IF SEP0=LF THEN xmRTS.X2C_ASSERT_F(1) END;
  IF size=0 THEN last:=0C; RETURN END;
  g:=buf; cc:=size;
  SYSTEM.GET(g,pch);
  DEC(cc); g:=SYSTEM.ADDADR(g,SIZE(CHAR));
  WHILE cc>0 DO
    SYSTEM.GET(g,ch);
    DEC(cc); g:=SYSTEM.ADDADR(g,SIZE(CHAR));
    IF (ch=SEP1) & (pch=SEP0) THEN
      pch:=LF;
    ELSE
      SYSTEM.PUT(buf,pch);
      DEC(size); buf:=SYSTEM.ADDADR(buf,SIZE(CHAR));
      pch:=ch;
    END;
  END;
  last:=pch;
  SYSTEM.PUT(buf,pch);
  buf:=SYSTEM.ADDADR(buf,SIZE(CHAR)); DEC(size);
END convSEPS;

PROCEDURE textread(f: File; a: ADDRESS; size: CARDINAL; VAR rd: CARDINAL): INTEGER;
  VAR
    res: INTEGER;
      p: ADDRESS;
    c,i: CARDINAL;
     ch: CHAR;
   flag: BOOLEAN; (* TRUE == read request satisfied totally *)
BEGIN
  IF (tag_lbuf IN f^.tags) THEN
    res:=f^.flush(f);
    IF res#0 THEN RETURN res END;
  END;
  IF size=0 THEN RETURN 0 END;
  res:=0; flag:=TRUE;
  p:=a; c:=0;
  IF f^.ucnt>0 THEN
    REPEAT
      DEC(f^.ucnt); SYSTEM.PUT(p,f^.ubuf[f^.ucnt]);
      INC(c); p:=SYSTEM.ADDADR(p,SIZE(CHAR));
    UNTIL (c>=size) OR (f^.ucnt=0);
  END;
  IF c<size THEN
    res:=f^.bread(f,p,size-c,i);
    IF res#0 THEN RETURN res END;
    flag:=(i=size-c);
    c:=c+i;
  END;
  IF SEP1=0C THEN   (* one-char separator   *)
    IF SEP0#LF THEN (* convert SEP0s to LFs *)
      i:=0;
      REPEAT
        SYSTEM.GET(a,ch);
        IF ch=SEP0 THEN SYSTEM.PUT(a,LF) END;
        a:=SYSTEM.ADDADR(a,SIZE(CHAR)); INC(i);
      UNTIL i=c;
    END;
    (* here we have to sinish, because in case of one-char separator
    the length is not changed *)
    rd:=c; RETURN 0;
  END;

  (* If we are here - separator is 2-chars and conversion
     is somewhat more complicated *)
  i:=c; convSEPS(a,i,ch);

  (* cases to be resolved here:
     1. ch=SEP0 - the next char could be SEP1 (in case NOT flag)
     2. i>0 - this means not all request fulfilled
   *)
  rd:=c-i;
  IF i=0 THEN
    IF (ch=SEP0) & flag THEN
      IF f^.ucnt>0 THEN
        DEC(f^.ucnt); ch:=f^.ubuf[f^.ucnt]; res:=0; i:=1;
      ELSE
        res:=f^.bread(f,SYSTEM.ADR(ch),1,i);
      END;
      IF res#0 THEN RETURN res END;
      IF i>0 THEN
        IF ch=SEP1 THEN
          SYSTEM.PUT(SYSTEM.SUBADR(a,SIZE(CHAR)),LF);
        ELSE
          rawungetc(f,ch);
        END;
      END;
    END;
    RETURN 0;
  END;
  IF NOT flag THEN RETURN 0 END;
  REPEAT
    c:=i; res:=f^.bread(f,a,c,i);
    IF res#0 THEN RETURN res END;
    flag:=(c=i);
    IF ch=SEP0 THEN
      a:=SYSTEM.SUBADR(a,SIZE(CHAR)); INC(i);
    END;
    c:=i; convSEPS(a,i,ch);
    rd:=rd+c-i;
  UNTIL (i=0) OR NOT flag;
  IF flag & (ch=SEP0) THEN
    res:=f^.bread(f,SYSTEM.ADR(ch),1,i);
    IF res#0 THEN RETURN res END;
    IF ch=SEP1 THEN
      SYSTEM.PUT(SYSTEM.SUBADR(a,SIZE(CHAR)),LF);
    ELSE
      rawungetc(f,ch);
    END;
  END;
  RETURN res;
END textread;

PROCEDURE textwrite(f: File; a: ADDRESS; size: CARDINAL; VAR wr: CARDINAL): INTEGER;
  VAR res: INTEGER;
        s: ARRAY [0..BUFSIZE-1] OF CHAR;
        i: CARDINAL;
       ch: CHAR;
     flag: BOOLEAN;
BEGIN
  res:=0;
  f^.ucnt:=0; i:=0; wr:=0; flag:=FALSE;
  WHILE (size>0) DO
    SYSTEM.GET(a,ch); a:=SYSTEM.ADDADR(a,SIZE(CHAR));
    INC(wr); DEC(size);
    flag:=flag OR (ch=LF);
    IF (SEP0#LF) & (ch=LF) THEN
      IF SEP1=0C THEN ch:=SEP0
      ELSE
        IF i>=BUFSIZE THEN
          res:=f^.bwrite(f,SYSTEM.ADR(s),i,i);
          IF res#0 THEN RETURN res END;
          i:=0;
        END;
        s[i]:=SEP0; INC(i);
        ch:=SEP1;
      END;
    END;
    IF i>=BUFSIZE THEN
      res:=f^.bwrite(f,SYSTEM.ADR(s),i,i);
      IF res#0 THEN RETURN res END;
      i:=0;
    END;
    s[i]:=ch; INC(i);
  END;
  IF i>0 THEN
    res:=f^.bwrite(f,SYSTEM.ADR(s),i,i);
  END;
  IF (tag_lbuf IN f^.tags) & flag & (res=0) THEN res:=f^.flush(f) END;
  RETURN res;
END textwrite;

PROCEDURE textungetc(f: File; ch: CHAR);
BEGIN
  IF (SEP0#LF) & (ch=LF) THEN
    IF SEP1=0C THEN ch:=SEP0
    ELSE
     f^.ubuf[f^.ucnt]:=SEP1; INC(f^.ucnt);
     ch:=SEP0;
    END;
  END;
  f^.ubuf[f^.ucnt]:=ch;
  INC(f^.ucnt);
END textungetc;

(* =============== opened files ring ============= *)

VAR
  ring: DevData;
  <* IF multithread THEN *>
    rlock: xmRTS.X2C_MUTEX;
  <* END *>

PROCEDURE TieFile(f: File);
BEGIN
  <* IF multithread THEN *> xmRTS.X2C_LockMutex(rlock); <* END *>
    IF ring=NIL THEN
      f^.fwd:=f;
      f^.bck:=f;
      ring:=f;
    ELSE
      f^.fwd:=ring;
      f^.bck:=ring^.bck;
      f^.bck^.fwd:=f;
      f^.fwd^.bck:=f;
    END;
  <* IF multithread THEN *> xmRTS.X2C_FreeMutex(rlock); <* END *>
END TieFile;

PROCEDURE UntieFile(f: File);
BEGIN
  <* IF multithread THEN *> xmRTS.X2C_LockMutex(rlock); <* END *>
    IF ring=f THEN ring:=ring^.fwd END;
    IF ring=f THEN ring:=NIL; RETURN END;
    f^.fwd^.bck:=f^.bck;
    f^.bck^.fwd:=f^.fwd;
  <* IF multithread THEN *> xmRTS.X2C_FreeMutex(rlock); <* END *>
END UntieFile;

(* ============ Methods implementation =========== *)

PROCEDURE GetName(x: Object; VAR s: ARRAY OF CHAR);
  VAR f: File;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  IF f^.name=NIL THEN s[0]:=0C;
  ELSE COPY(f^.name^,s);
  END;
END GetName;

PROCEDURE RawRead(x: Object; a: ADDRESS; n: CARDINAL; VAR locs: CARDINAL);
  VAR f: File; res: INTEGER;
BEGIN
  IF n=0 THEN locs:=0; RETURN END;
  f:=SYSTEM.CAST(File,x^.cd);
  lock(f);
    res:=f^.read(f,a,n,locs);
  unlock(f);
  IF res#0 THEN HardError(x,res) END;
  IF (locs=0) THEN x^.result:=ioc.endOfInput
  ELSE             x^.result:=ioc.allRight;
  END;
END RawRead;

PROCEDURE Flush(x: Object);
  VAR f: File; res: INTEGER;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  lock(f);
    res:=f^.flush(f);
    f^.ucnt:=0;
    IF (res=0) & (cc.writeFlag IN f^.flags) THEN
      res:=xlibOS.X2C_fFlush(f^.cf);
    END;
  unlock(f);
  IF res#0 THEN HardError(x,res) END;
END Flush;

PROCEDURE TextWrite(x: Object; a: SYSTEM.ADDRESS; n: CARDINAL);
  VAR num: CARDINAL; res: INTEGER; f: File;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  lock(f);
    res:=f^.write(f,a,n,num);
  unlock(f);
  IF (res#0) OR (num#n) THEN HardError(x,res) END;
END TextWrite;

PROCEDURE WriteLn(x: Object);
  VAR n: CARDINAL; res: INTEGER; f: File; ch: CHAR;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd); ch:=LF;
  lock(f);
    res:=f^.write(f,SYSTEM.ADR(ch),1,n);
  unlock(f);
  IF res#0 THEN HardError(x,res) END;
END WriteLn;

PROCEDURE Look(x: Object; VAR ch: CHAR; VAR res: ioc.ReadResults);
  VAR f: File; l: CARDINAL; r: INTEGER;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  lock(f);
    r:=f^.read(f,SYSTEM.ADR(ch),1,l);
    IF r=0 THEN
      IF l=0 THEN res:=ioc.endOfInput
      ELSE
        IF ch = LF THEN res:=ioc.endOfLine;
        ELSE            res:=ioc.allRight;
        END;
        f^.ungetc(f,ch);
      END;
    ELSE
      res:=ioc.endOfInput;
    END;
  unlock(f);
  x^.result:=res;
END Look;

PROCEDURE Skip(x: Object);
  VAR f: File; ch: CHAR; l: CARDINAL; res: INTEGER;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  lock(f);
    res:=f^.read(f,SYSTEM.ADR(ch),1,l);
  unlock(f);
  IF res#0 THEN
    HardError(x,res);
  ELSIF l=0 THEN
    IOLink.RAISEdevException(x^.cid,x^.did,IOChan.skipAtEnd,"");
  END;
  x^.result:=ioc.allRight;
END Skip;

PROCEDURE SkipLook(x: Object; VAR ch: CHAR; VAR res: ioc.ReadResults);
BEGIN
  Skip(x); Look(x,ch,res);
END SkipLook;

PROCEDURE TextRead(x: Object; a: SYSTEM.ADDRESS; n: CARDINAL; VAR locs: CARDINAL);
  VAR f: File;
     ch: CHAR;
      l: CARDINAL;
    res: INTEGER;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  lock(f);
    locs:=0;
    WHILE locs<n DO
      res:=f^.read(f,SYSTEM.ADR(ch),1,l);
      IF res#0 THEN
        unlock(f);
        HardError(x,res);
      ELSIF l=0 THEN
        IF locs>0 THEN x^.result:=ioc.allRight;
        ELSE x^.result:=ioc.endOfInput;
        END;
        unlock(f);
        RETURN
      END;
      IF ch=LF THEN
        f^.ungetc(f,ch);
        IF locs>0 THEN x^.result:=ioc.allRight
        ELSE x^.result:=ioc.endOfLine
        END;
        unlock(f);
        RETURN
      END;
      SYSTEM.PUT(a,ch);
      a:=SYSTEM.ADDADR(a,SIZE(CHAR));
      INC(locs);
    END;
  unlock(f);
  x^.result:=ioc.allRight;
END TextRead;

PROCEDURE IniRead(x: Object);
BEGIN
  IF cc.rawFlag IN x^.flags THEN
    x^.doRawRead:=RawRead;
  END;
  IF cc.textFlag IN x^.flags THEN
    x^.doLook:=Look;
    x^.doSkip:=Skip;
    x^.doSkipLook:=SkipLook;
    x^.doTextRead:=TextRead
  END;
END IniRead;

PROCEDURE IniWrite(x: Object);
BEGIN
  IF cc.rawFlag IN x^.flags THEN
    x^.doRawWrite:=TextWrite;
  END;
  IF cc.textFlag IN x^.flags THEN
    x^.doTextWrite:=TextWrite;
    x^.doLnWrite:=WriteLn;
  END;
END IniWrite;

PROCEDURE Open(x: Object;
            file: xlibOS.X2C_OSFHANDLE;
            name: FileName;
           flags: FlagSet;
           bmode: BufMode;
         VAR res: OpenResults);

  CONST mix=cc.raw+cc.text;

  VAR f: File; r: INTEGER; type: SYSTEM.int;
BEGIN
  IF (mix*flags=mix)& NOT xlibOS.X2C_IsMixAllowed() THEN
    res:=cc.noMixedOperations; RETURN
  END;
  IF name=NIL THEN res:=cc.outOfChans; RETURN END;
  NEW(f);
  IF f=NIL THEN res:=cc.outOfChans; RETURN END;
  f^.name:=name;
  f^.tags:={};
  f^.buf:=NIL;
  type:=xlibOS.X2C_fGetFileType(file);
  CASE bmode OF
    |bmFull,
     bmLine :
      IF type#xlibOS.X2C_ftDisk THEN bmode:=bmNone END;
    |bmDefault:
      IF type=xlibOS.X2C_ftDisk THEN bmode:=bmFull
      ELSE bmode:=bmNone
      END;
  ELSE (* bmNone *)
  END;

<* IF ~ __GEN_C__ THEN *>
  IF (bmode#bmNone) THEN NEW(f^.buf) END;
<* END *>
  IF f^.buf#NIL THEN
    IF bmode=bmLine THEN
      f^.bread:=dRead;
    ELSE
      f^.bread:=bRead;
    END;
    f^.bwrite:=bWrite;
    f^.flush:=bFlush;
  ELSE
    f^.bread:=dRead;
    f^.bwrite:=dWrite;
    f^.flush:=dFlush;
  END;
  IF (flags * cc.text) # FlagSet{} THEN
    f^.read:=textread;
    f^.write:=textwrite;
    f^.ungetc:=textungetc;
  ELSE
    f^.read:=rawread;
    f^.write:=rawwrite;
    f^.ungetc:=rawungetc;
  END;
  f^.cf:=file;
  f^.flags:=flags;
  f^.bpos:=0;
  f^.blen:=0;
<* IF __GEN_C__ THEN *>
  INCL(f^.tags,tag_rnd);
<* ELSE *>
  IF type=xlibOS.X2C_ftDisk THEN
    INCL(f^.tags,tag_rnd);
    IF f^.buf#NIL THEN
      r:=xlibOS.X2C_fTell(file,f^.spos);
      IF r#0 THEN SoftError(x,r) END;
    END;
  END;
<* END *>
  IF bmode=bmLine THEN
    INCL(f^.tags,tag_lbuf);
  END;

  <* IF multithread THEN *>
  xmRTS.X2C_NewMutex(f^.lock);
  <* END *>
  f^.ucnt:=0;
  x^.flags:=flags;
  x^.cd:=SYSTEM.CAST(SYSTEM.ADDRESS,f);
  IF cc.writeFlag IN flags THEN IniWrite(x) END;
  IF cc.readFlag  IN flags THEN IniRead(x) END;
  x^.doGetName:=GetName;
  x^.doFlush:=Flush;
  TieFile(f);
  res:=cc.opened;
END Open;

VAR did: IOLink.DeviceId;

PROCEDURE GetDID(VAR d: IOLink.DeviceId);
BEGIN
  d:=did;
END GetDID;

PROCEDURE GetDevData(x: Object): DevData;
BEGIN
  RETURN SYSTEM.CAST(File,x^.cd);
END GetDevData;

<*$< WOFF301+ *>
PROCEDURE dlook(x: Object; VAR c: CHAR; VAR r: ioc.ReadResults);
BEGIN
  IOLink.RAISEdevException(x^.cid,x^.did,IOChan.notAvailable,"");
END dlook;

PROCEDURE dskip(x: Object);
BEGIN
  IOLink.RAISEdevException(x^.cid,x^.did,IOChan.notAvailable,"");
END dskip;

PROCEDURE dread(x: Object; a: SYSTEM.ADDRESS; max: CARDINAL; VAR n: CARDINAL);
BEGIN
  IOLink.RAISEdevException(x^.cid,x^.did,IOChan.notAvailable,"");
END dread;

PROCEDURE dwrite(x: Object; a: SYSTEM.ADDRESS; max: CARDINAL);
BEGIN
  IOLink.RAISEdevException(x^.cid,x^.did,IOChan.notAvailable,"");
END dwrite;
<*$>*>

PROCEDURE ForbidAll(x: Object);
BEGIN
  x^.doLook:=dlook;
  x^.doSkip:=dskip;
  x^.doSkipLook:=dlook;
  x^.doTextRead:=dread;
  x^.doRawRead:=dread;
  x^.doLnWrite:=dskip;
  x^.doTextWrite:=dwrite;
  x^.doRawWrite:=dwrite;
END ForbidAll;

PROCEDURE SetMode(x: Object; input: BOOLEAN);
  VAR f: File;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  lock(f);
    ForbidAll(x);
    x^.flags:=x^.flags-cc.read-cc.write;
    IF input THEN
      IF cc.readFlag IN f^.flags THEN
        IniRead(x); x^.flags:=x^.flags+cc.read;
      END;
    ELSE
      IF cc.writeFlag IN f^.flags THEN
        IniWrite(x); x^.flags:=x^.flags+cc.write;
      END;
    END;
  unlock(f);
END SetMode;

PROCEDURE Close(x: Object);
  VAR f: File; res: INTEGER;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  lock(f);
    res:=f^.flush(f);
    IF res#0 THEN
      unlock(f);
      HardError(x,res)
    END;
    IF f^.buf#NIL THEN DISPOSE(f^.buf) END;
    DISPOSE(f^.name);
  unlock(f);
  <* IF multithread THEN *>
  xmRTS.X2C_DelMutex(f^.lock);
  <* END *>
  UntieFile(f);
  DISPOSE(f);
END Close;

PROCEDURE CurrentPos(x: Object; VAR pos: xlibOS.X2C_FPOS);
  VAR f: File;
    res: INTEGER;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  ASSERT(tag_rnd IN f^.tags,100H);
  lock(f);
    IF f^.buf#NIL THEN
      xrInt64.X2C_CARDTO64(pos,f^.bpos);
      IF xrInt64.X2C_ADD64(pos,f^.spos,pos) THEN
        unlock(f);
        xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.wholeValueException));
      END;
    ELSE
      res:=xlibOS.X2C_fTell(f^.cf,pos);
      IF res#0 THEN
        unlock(f);
        SoftError(x,res)
      END;
    END;
  unlock(f);
END CurrentPos;

PROCEDURE Length(x: Object; VAR pos: xlibOS.X2C_FPOS);
  VAR f: File;
    res: INTEGER;
      l: xlibOS.X2C_FPOS;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  ASSERT(tag_rnd IN f^.tags,100H);
  lock(f);
    IF f^.buf=NIL THEN
      res:=xlibOS.X2C_fSize(f^.cf,pos);
      IF res#0 THEN
        unlock(f);
        SoftError(x,res)
      END;
    ELSE
      res:=xlibOS.X2C_fSize(f^.cf,l);
      IF res#0 THEN
        unlock(f);
        SoftError(x,res)
      END;
      xrInt64.X2C_CARDTO64(pos,f^.blen);
      IF xrInt64.X2C_ADD64(pos,f^.spos,pos) THEN
        unlock(f);
        xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.wholeValueException));
      END;
      IF xrInt64.X2C_CMP64(l,pos)>0 THEN pos:=l END;
    END;
  unlock(f);
END Length;

PROCEDURE SetPos(x: Object; pos: xlibOS.X2C_FPOS);
  VAR f: File;
    res: INTEGER;
      l: xlibOS.X2C_FPOS;
      q: CARDINAL;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  ASSERT(tag_rnd IN f^.tags,100H);
  lock(f);
    IF f^.buf=NIL THEN
      res:=xlibOS.X2C_fSeek(f^.cf,pos,xlibOS.X2C_fSeekSet);
      IF res#0 THEN
        unlock(f);
        SoftError(x,res)
      END;
    ELSE
      xrInt64.X2C_CARDTO64(l,f^.blen);
      IF xrInt64.X2C_ADD64(l,f^.spos,l) THEN
        unlock(f);
        xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.wholeValueException));
      END;
      IF (xrInt64.X2C_CMP64(pos,f^.spos)>=0) &
         (xrInt64.X2C_CMP64(pos,l)<0) THEN
        IF xrInt64.X2C_UnMinus64(l,f^.spos) OR
           xrInt64.X2C_ADD64(l,pos,l)       OR
           xrInt64.X2C_64TOCARD(q,l) THEN
           unlock(f);
           ASSERT(FALSE,102H);
        END;
        IF q#f^.bpos THEN f^.ucnt:=0 END;
        f^.bpos:=q;
      ELSE
        f^.ucnt:=0;
        res:=f^.flush(f);
        IF res#0 THEN
          unlock(f);
          HardError(x,res)
        END;
        res:=xlibOS.X2C_fSeek(f^.cf,pos,xlibOS.X2C_fSeekSet);
        IF res#0 THEN
          unlock(f);
          SoftError(x,res)
        END;
        f^.spos:=pos;
      END;
    END;
  unlock(f);
END SetPos;

PROCEDURE FlushAll;
  VAR f: File; res: INTEGER;
BEGIN
  <* IF multithread THEN *> xmRTS.X2C_LockMutex(rlock); <* END *>
  IF ring=NIL THEN RETURN END;
  f:=ring;
  REPEAT
    IF f^.buf # NIL THEN res:=f^.flush(f); f^.ucnt:=0; END;
    f:=f^.fwd;
  UNTIL f=ring;
END FlushAll;

(* Sets file position according to the real position of the file.
   Returns true on success or false on errors.
   This usage scenario is the following:
   1) one calls Flush on the stdout/stderr objects
   2) executes several processes that may write something to stdout/stderr streams
   3) update current position of the stdout/stderr objects
   Note: this function does nothing if the buffer is not empty.
*)
PROCEDURE SynchronizePos(x: Object): BOOLEAN;
VAR 
  n: CARDINAL; 
  res: INTEGER; 
  f: File; 
  ch: CHAR;
BEGIN
  f:=SYSTEM.CAST (File,x^.cd); 
  IF f^.bpos # 0 THEN RETURN FALSE; END;

  res:=xlibOS.X2C_fTell(f^.cf, f^.spos);
  IF res#0 THEN RETURN FALSE END;

  RETURN TRUE;
END SynchronizePos;




BEGIN
  IOLink.AllocateDeviceId(did);
  ring:=NIL;
  <* IF multithread THEN *> xmRTS.X2C_NewMutex(rlock); <* END *>
FINALLY
  FlushAll;
END xDevData.
