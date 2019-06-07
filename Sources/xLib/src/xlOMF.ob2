(** Copyright (c) 1995 xTech Ltd, Russia. All Rights Reserved. *)
(** XDS librarian. OMF format objects. *)
MODULE xlOMF; (* Hady. Oct 25, 1995 *)

IMPORT
  xlK
  ,xlStrings
  ,xlFiles
  ,omf:=xldOMF
  ,xlMsg
  ,xlNames
  ,RndFile
  ,IOResult
  ,RawIO
  ,IOChan
  ,SYSTEM
  ,dbg:=xlMsg
  ,Printf
  ;

CONST IOBUFLEN = 256;

TYPE
  String = xlStrings.String;

  ImportModule = POINTER TO IMDesc;
  IMDesc = RECORD (xlK.ModuleDesc)
    dll    :String;
    int    :String;
    ext    :String;
    ord    :LONGINT;
  END;

  FileModule = POINTER TO FMDesc;
  FMDesc = RECORD (xlK.ModuleDesc)
    file: xlFiles.File;
    start: xlFiles.FilePos;
  END;

  DirBlock = RECORD
    data :POINTER TO ARRAY omf.DirPageSize OF CHAR;
    busy :BOOLEAN;
    bucs :ARRAY omf.DirPageBuckets OF INTEGER;
    ofs  :INTEGER;  (* assume to be always even *)
  END;

  LibDirectory = POINTER TO ARRAY OF DirBlock;

  Library = POINTER TO LibraryDesc;
  LibraryDesc = RECORD (xlK.LibraryDesc)
    names  : xlNames.Names;
    dir    : LibDirectory;
    ofs    : LONGINT;
    pad    : LONGINT;
    page   : LONGINT;
    blocks : LONGINT;
  END;

  Collector = RECORD (xlK.Iterator)
    err  :BOOLEAN;
    body :xlK.Module;
    cont :xlNames.Names;
    max  :LONGINT;
  END;

  Iterator = RECORD (xlNames.Iterator)
    err :BOOLEAN;
    l   :Library;
  END;

  Directory *= POINTER TO DirectoryDesc;
  DirectoryDesc *= RECORD (xlK.DirectoryDesc)
  END;

(** Import module methods *)

PROCEDURE (m :ImportModule) calcSize (name- :ARRAY OF CHAR) :LONGINT;
  VAR
    sz,size :LONGINT;
BEGIN
  IF ~omf.IsXOMF THEN
     size:=4+(1+LENGTH(name)); (* THEADR record  *)
     size:=size+5;             (* MODEND record  *)
     sz:=8+(1+LENGTH(m.dll^));
     size:=size+sz+(1+LENGTH(m.int^));
     IF m.ext=NIL THEN
       size:=size+2;
     ELSE
       size:=size+(1+LENGTH(m.ext^));
     END;
  ELSE
     size:=LENGTH(name);
     IF size < 255 THEN
        size:=4+(1+size); (* XOMF_THEADR record  *)
     ELSE
        size:=4+(1+255); (* XOMF_THEADR record  *)
     END;
     size:=size+5;             (* MODEND record  *)
     sz:=8+(1+LENGTH(m.dll^));
     size:=size+sz+(2+LENGTH(m.int^));
     IF m.ext=NIL THEN
       size:=size+2;
     ELSE
       size:=size+(2+LENGTH(m.ext^));
     END;
  END;
  RETURN size;
END calcSize;

PROCEDURE (m: ImportModule) IteratePublics(VAR iter: xlK.Iterator);
BEGIN
  IF m.ext#NIL THEN
    xlStrings.Print(iter.info,"%s.%s",m.dll^,m.ext^);
  ELSE
    xlStrings.Print(iter.info,"%s.[ %d ]",m.dll^,m.ord);
  END;
  iter.Take(m.int^);
END IteratePublics;

PROCEDURE (m: ImportModule) Write(to: xlK.ChanId);
BEGIN
  omf.WriteTHEADR(to,m.name^);
  IF (m.ext # NIL) OR (m.ord = -1)THEN
    omf.WriteImportByName(to,m.int^,m.dll^,m.ext^ );
  ELSE
    omf.WriteImportByOrdinal(to,m.int^,m.dll^,m.ord );
  END;
  omf.WriteMODEND(to);
END Write;

(** File Module methods *)

PROCEDURE (m: FileModule) IteratePublics(VAR iter: xlK.Iterator);
  VAR rec  :omf.Record;
      b    :omf.BYTE;
      grp,
      seg  :LONGINT;
      name :omf.NameType;
      res  :RndFile.OpenResults;

  PROCEDURE readImport ( VAR rec: omf.Record );
    VAR ord     :omf.BYTE;
        module,
        entry   :omf.NameType;
        onum    :omf.WORD;
        t       :omf.BYTE;
  BEGIN
    iter.info:=NIL;
    omf.ReadByte(rec,ord);
    IF rec.err THEN RETURN END;
    omf.ReadName(rec,name);
    omf.ReadName(rec,module);
    IF rec.err THEN RETURN END;
    IF (ord#0) THEN                (* import by ordinal *)
      omf.ReadByte(rec,t);
      onum:=VAL(omf.WORD,t);
      omf.ReadByte(rec,t);
      onum:=onum+256*VAL(omf.WORD,t);
      IF ~rec.err THEN
        xlStrings.Print(iter.info,"%s.[%d]",module,onum);
      END;
    ELSE
      omf.ReadName(rec,entry);
      IF entry[0]=0X THEN COPY(name,entry) END;
      IF ~rec.err THEN
        xlStrings.Print(iter.info,'%s."%s"',module,entry);
      END;
    END;
  END readImport;

BEGIN
  m.file.Resume(res);
  IF (res # RndFile.opened) THEN RETURN END;
  RndFile.SetPos(m.file.cid,m.start);
  LOOP
    omf.OpenRecord(rec,m.file.cid);
    IF rec.err THEN m.file.Suspend; RETURN END;
    CASE rec.hdr OF
      |omf.MODEND,omf.MODEND32: EXIT
      |omf.PUBDEF16,omf.PUBDEF32:
         omf.ReadIndex(rec,grp);
         omf.ReadIndex(rec,seg);
         IF rec.err THEN m.file.Suspend; RETURN END;
         IF seg=0 THEN omf.SkipBytes(rec,2); END;
         WHILE ~rec.err & (rec.len>0) DO
           omf.ReadName(rec,name);
           IF rec.err THEN m.file.Suspend; RETURN END;
           IF rec.hdr=omf.PUBDEF16 THEN
             omf.SkipBytes(rec,2);
           ELSE
             omf.SkipBytes(rec,4);
           END;
           omf.ReadIndex(rec,grp);
           IF ~rec.err THEN
             iter.info:=NIL;
             iter.Take(name);
           END;
         END;
         IF rec.len#0 THEN m.file.Suspend; RETURN END;
         omf.SkipRecord(rec);
      |omf.COMDEF:
         omf.ReadName(rec,name);
         IF rec.err THEN m.file.Suspend; RETURN END;
         omf.SkipRecord(rec);
         IF name#"" THEN
           iter.info:=NIL; iter.Take(name)
         END;
      |omf.COMENT:
         omf.ReadByte(rec,b);
         IF rec.err THEN m.file.Suspend; RETURN END;
         IF (b=0) THEN (* comment type *)
           omf.ReadByte(rec,b);
           IF rec.err THEN m.file.Suspend; RETURN END;
           IF (b=0A0H) THEN (* comment class *)
             omf.ReadByte(rec,b);
             IF rec.err THEN m.file.Suspend; RETURN END;
             IF (b=1) THEN (* IMPDEF *)
               readImport(rec);
               IF rec.err THEN m.file.Suspend; RETURN END;
               iter.Take(name);
             END;
           END;
         END;
         omf.SkipRecord(rec);
    ELSE
      omf.SkipRecord(rec);
    END;
  END;
  m.file.Suspend;
END IteratePublics;

PROCEDURE (m: FileModule) Write(to: xlK.ChanId);
  VAR buf: ARRAY IOBUFLEN OF CHAR;
     size: LONGINT;
      len: SYSTEM.CARD32;
      res: RndFile.OpenResults;
BEGIN
  m.file.Resume(res);
  RndFile.SetPos(m.file.cid,m.start);
  size:=m.size;
  WHILE (size>IOBUFLEN) DO
    RawIO.Read(m.file.cid,buf);
    RawIO.Write(to,buf);
    size:=size-IOBUFLEN;
  END;
  IF size>0 THEN
    IOChan.RawRead(m.file.cid,SYSTEM.ADR(buf),size,len);
    IOChan.RawWrite(to,SYSTEM.ADR(buf),len);
  END;
  m.file.Suspend;
END Write;

(* ------------ OMF library implementation ------------ *)

(* Block operations *)

PROCEDURE (VAR b: DirBlock) init;
  VAR i: LONGINT;
BEGIN
  b.busy:=FALSE;
  b.ofs:=omf.DirPageHeaderSize;
  FOR i:=0 TO omf.DirPageBuckets-1 DO b.bucs[i]:=0 END;
END init;

PROCEDURE (VAR b: DirBlock) pack;
  VAR i: LONGINT;
BEGIN
  FOR i:=0 TO omf.DirPageBuckets-1 DO b.data[i]:=CHR(b.bucs[i] DIV 2) END;
  IF b.busy THEN b.data[omf.DirPageBuckets]:=CHR(255)
  ELSE           b.data[omf.DirPageBuckets]:=CHR(b.ofs DIV 2)
  END;
END pack;

PROCEDURE (VAR b: DirBlock) placeName(name-: ARRAY OF CHAR; buck,nlen,mofs: LONGINT);
  VAR i: LONGINT;
BEGIN
  ASSERT(omf.DirPageSize-b.ofs>=nlen+3);
  ASSERT((mofs>=0) & (mofs<=256*256));
  b.bucs[buck]:=b.ofs;
  IF b.data=NIL THEN
    b.ofs:=b.ofs+VAL(INTEGER,nlen)+3;
    IF ODD(b.ofs) THEN INC(b.ofs) END;
  ELSE
    i:=0;
    b.data[b.ofs]:=CHR(nlen); INC(b.ofs);
    WHILE i<nlen DO
      b.data[b.ofs]:=name[i]; INC(i); INC(b.ofs);
    END;
    b.data[b.ofs]:=CHR(mofs MOD 256); INC(b.ofs);
    b.data[b.ofs]:=CHR(mofs DIV 256); INC(b.ofs);
    IF ODD(b.ofs) THEN
      b.data[b.ofs]:=0X; INC(b.ofs);
    END;
  END;
  ASSERT( b.ofs <= omf.DirPageSize);
  IF (b.ofs=omf.DirPageSize) THEN
    b.busy:=TRUE;
    --Printf.printf("busy.\n");
  END;
END placeName;

(* Collector methods *)

PROCEDURE (VAR c: Collector) Take(name-: ARRAY OF CHAR);
  VAR l: LONGINT;
BEGIN
  l:=LENGTH(name);
  IF ~omf.IsXOMF THEN
    IF l>255 THEN
      c.err:=TRUE;
      xlMsg.error('too long name.\nsymbol "%s"\nin module "%s"',name,c.body.name^);
    END;
    IF c.err THEN RETURN END;
  END;
  IF l>c.max THEN c.max:=l END;
  c.cont.Insert(xlStrings.Make(name),c.body);
END Take;

(** Library methods *)


PROCEDURE (l: Library) calcLibPage(VAR err: BOOLEAN);
VAR
  t   :xlK.Module;
  ord :LONGINT;
BEGIN
  l.page := omf.LibPageSize;
  l.ofs := 1;
  t     := l.list;
  ord   := 0;
  WHILE (t # NIL) DO
    t.fofs := l.ofs;
    l.ofs  := l.ofs+((t.size + l.page-1) DIV l.page);
    t.mord := ord; INC(ord);
    t      := t.next;
  END;
  --UNTIL (l.ofs<=MAX(omf.LengthType)) OR (l.page = omf.DirPageSize);
  --IF l.ofs>MAX(omf.LengthType) THEN err:=TRUE; RETURN END;
  l.ofs:=l.ofs*l.page;
  l.pad:=omf.DirPageSize-(l.ofs MOD omf.DirPageSize);
  l.ofs:=l.ofs+l.pad;
  IF l.pad<3 THEN
    l.ofs:=l.ofs+omf.DirPageSize;
    l.pad:=l.pad+omf.DirPageSize;
  END;
END calcLibPage;

PROCEDURE (l: Library) calcDirSize(max: LONGINT);
CONST
  primesN = 54;
TYPE
 primet = ARRAY primesN OF INTEGER;
CONST
 primes = primet{
 2, 3, 5, 7,11,13,17,19,23,29,
 31,37,41,43,47,53,59,61,67,71,
 73,79,83,89,97,101,103,107,109,113,
 127,131,137,139,149,151,157,163,167,173,
 179,181,191,193,197,199,211,223,227,229,
 233,239,241,251};
VAR
  size,len,i :LONGINT;
  t          :xlNames.Name;
  iter       : Iterator;
BEGIN
  l.blocks := 0;
  t := l.names.list;
  max  :=(max DIV 2+2)*2+omf.DirPageHeaderSize;
  size :=max;
  WHILE (t # NIL) DO
    len:=LENGTH(t.name^);
    len:=((len DIV 2) + 2)*2;
    IF size+len>omf.DirPageSize THEN
      INC(l.blocks);
      size:=max+len;
    ELSE
      size:=size+len;
    END;
    t:=t.next;
  END;
  IF size>max THEN INC(l.blocks) END;
  (* number of blocks MUST BE prime; otherwise a linker may work incorrectly *)
  i:=0;
  LOOP
    IF (l.blocks<=primes[i]) THEN
      l.blocks:=primes[i];
      EXIT;
    END;
    IF (i=primesN) THEN EXIT END;
    INC (i);
  END;
  DEC(l.blocks);

  --l.names.ReorderByLength;
  iter.l:=l;
  iter.err:=TRUE;
  WHILE iter.err & (l.blocks<=VAL(LONGINT,MAX(omf.LengthType))) DO
    INC(l.blocks);
    NEW(l.dir,l.blocks);
    FOR i:=0 TO l.blocks-1 DO l.dir[i].init END;
    iter.err:=FALSE;
    l.names.Order(iter);
  END;
END calcDirSize;

PROCEDURE (l: Library) warnDuplicates;
  VAR n: xlNames.Name; d,m: xlNames.Module;
BEGIN
  n:=l.names.list;
  WHILE n#NIL DO
    d:=n.defined;
    m:=d.next;
    WHILE m#NIL DO
      xlMsg.warning('symbol "%s" (defined in "%s") is redefined in "%s"',n.name^,d.body.name^,m.body.name^);
      m:=m.next;
    END;
    n:=n.next;
  END;
END warnDuplicates;

PROCEDURE (l: Library) Prepare(VAR err: BOOLEAN);
  VAR
    c :Collector;
    m :xlK.Module;
BEGIN
  NEW(l.names); l.names.Init;
  err:=FALSE;
  l.calcLibPage(err);
  IF err THEN
    xlMsg.error("library is too large");
    l.names:=NIL; RETURN
  END;
  m:=l.list;
  c.err:=FALSE; c.cont:=l.names; c.max:=0;
  WHILE ~err & (m # NIL) DO
    c.body:=m; c.err:=FALSE;
    m.IteratePublics(c);
    err:=err OR c.err;
    m:=m.next;
  END;
  IF err THEN l.names:=NIL; RETURN END;
  (* !!!!!!!!!!!!!!!!!!!!!!!!
  l.calcDirSize(c.max);
  IF l.blocks>MAX(omf.LengthType) THEN
    err:=TRUE;
    xlMsg.error("library names space is too large");
  ELSE
    l.warnDuplicates;
  END;
  *)
END Prepare;

PROCEDURE (VAR i: Iterator) Take(n: xlNames.Name);
  VAR
    px,pd,po,od :LONGINT;
    bx,ex       :LONGINT;
    dir         :LibDirectory;
    nlen        :LONGINT;
BEGIN
  IF i.err THEN RETURN END;
  dir  := i.l.dir;
  omf.LibHash ( n.name^, LEN(dir^), omf.DirPageBuckets, px, pd, po, od );
  --Printf.printf("Name in use: %s, inten pos: <%x,%x>\n",n.name^,px,po);
  nlen := LENGTH(n.name^);
  bx   := px;
  REPEAT
    ex := po;
    LOOP
      IF dir[bx].bucs[ex]=0 THEN (* free bucket *)
        IF omf.DirPageSize-dir[bx].ofs>=nlen+3 THEN
          dir[bx].placeName(n.name^,ex,nlen,n.defined.body.fofs);
          RETURN
        ELSE
          --Printf.printf("busy (remains %d)\n",omf.DirPageSize-dir[bx].ofs);
          dir[bx].busy:=TRUE; EXIT
        END;
      END;
      --Printf.printf("iter bucket: %s <%x,%x>\n",n.name^,po,od);
      ex:=(ex+od) MOD omf.DirPageBuckets;
      IF ex=po THEN EXIT END;
    END;
    --Printf.printf("iter block: %s <%x,%x>\n",n.name^,px,pd);
    bx:=(bx+pd) MOD LEN(dir^);
  UNTIL (bx=px);
  i.err := TRUE;
END Take;

PROCEDURE (l: Library) makeDirectory(blocks: LONGINT);
  VAR
   i    :LONGINT;
   iter :Iterator;
BEGIN
  FOR i:=0 TO blocks-1 DO l.dir[i].init; NEW(l.dir[i].data) END;
  iter.l:=l; iter.err:=FALSE;
  l.names.Order(iter);
  ASSERT(~iter.err);
END makeDirectory;

PROCEDURE (l: Library) Write (to :xlK.ChanId);
VAR
  m      :xlK.Module;
  i, pad :LONGINT;
BEGIN
  ASSERT(l.names # NIL);

  (*
  IF (l.blocks>0) THEN
    l.makeDirectory(l.blocks);
  ELSE
    l.ofs:=0;
  END;    !!!!!!!!!!!!!!
  *)
  omf.WriteLibHeader (to, l.page, l.ofs, 0(*l.blocks !!!!!!!*), 01X);
  m := l.list;
  WHILE (m # NIL) DO
    m.Write(to);
    pad := m.size MOD l.page;
    IF (pad # 0) THEN
      pad := l.page-pad;
      omf.WritePadding(to, pad);
    END;
    m:=m.next;
  END;
  omf.WriteLibEnd(to,l.pad);

(* !!!!!!!!!!!!!!!!
  FOR i:=0 TO l.blocks-1 DO
    l.dir[i].pack;
    RawIO.Write(to,l.dir[i].data^);
  END;
*)
END Write;

PROCEDURE searchModName(s-: ARRAY OF CHAR; VAR f,t: LONGINT);
BEGIN
  t:=LENGTH(s);
  f:=t;
  IF f>0 THEN
    REPEAT DEC(f)
    UNTIL (f=0) OR (s[f]=".") OR (s[f]="\") OR (s[f]="/");
  END;
  IF s[f]="." THEN
    t:=f;
    REPEAT DEC(f)
    UNTIL (f<=0) OR (s[f]="\") OR (s[f]="/");
  END;
  IF f<0 THEN f:=0 END;
  IF (s[f]="\") OR (s[f]="/") THEN INC(f) END;
END searchModName;

PROCEDURE compareSubStrings(s0-,s1-: ARRAY OF CHAR; f0,t0,f1,t1: LONGINT): BOOLEAN;
BEGIN
  IF (t1-f1=t0-f0) & (t1-f1>0) THEN
    WHILE t1#f1 DO
      IF CAP(s0[f0])#CAP(s1[f1]) THEN RETURN FALSE END;
      INC(f1); INC(f0);
    END;
    RETURN TRUE
  END;
  RETURN FALSE;
END compareSubStrings;

PROCEDURE (l: Library) Search(name-: ARRAY OF CHAR): xlK.Module;
  VAR mod: xlK.Module; VAR f0,f1,t0,t1: LONGINT;
BEGIN
  searchModName(name,f0,t0);
  mod:=l.list;
  WHILE (mod#NIL) DO
    searchModName(mod.name^,f1,t1);
(*dbg.print('comparing "%s"(%d,%d) and "%s"(%d,%d)\n',name,f0,t0,mod.name^,f1,t1);*)
    IF compareSubStrings(name,mod.name^,f0,t0,f1,t1) THEN RETURN mod END;
    mod:=mod.next;
  END;
  RETURN NIL
END Search;

(* ------------- Directory implementation --------------- *)

PROCEDURE (d: Directory) NewLibrary*(time: LONGINT): xlK.Library;
  VAR l: Library;
BEGIN
  NEW(l); l.Init(time); RETURN l;
END NewLibrary;

PROCEDURE (d: Directory) MakeImportLibrary*(VAR l: xlK.Library; dll: xlK.Dll);
  VAR t   :xlK.Entry;
      im  :ImportModule;
      lib :Library;
BEGIN
  NEW(lib); lib.Init(0);
  WHILE dll#NIL DO
    t:=dll.list;
    WHILE t#NIL DO
      NEW(im);
      im.dll:=dll.name;
      im.int:=t.int;
      im.ord:=t.ord;
      IF t.ord>=0 THEN
        im.ext:=NIL;
      ELSE
        im.ext:=t.ext;
      END;
      im.Init(im.int^,im.calcSize(im.int^),0);
      lib.Add(im);
      t:=t.next;
    END;
    dll:=dll.next;
  END;
  l:=lib;
END MakeImportLibrary;

PROCEDURE (d: Directory) IsModule*(file: xlK.ChanId): BOOLEAN;
BEGIN
  RETURN FALSE;
END IsModule;

PROCEDURE (d: Directory) IsLibrary*(file: xlK.ChanId): BOOLEAN;
  VAR rec: omf.Record;
BEGIN
  omf.OpenRecord(rec,file);
  RETURN ~rec.err &
         ((rec.hdr=omf.THEADR) OR (rec.hdr=omf.LHEADR) OR (rec.hdr=omf.LibHdr));
END IsLibrary;

PROCEDURE (d: Directory) OpenLibrary*(VAR lib: xlK.Library; file: xlFiles.File);
  VAR new: Library;
      mod: FileModule;
      rec: omf.Record;
    align: LONGINT;
   size,i: LONGINT;
      pos: xlFiles.FilePos;
     name: omf.NameType;
BEGIN
  lib:=NIL;
  align:=1;
  NEW(new); new.Init(0);
  pos:=RndFile.StartPos(file.cid);
  RndFile.SetPos(file.cid,pos);
  omf.OpenRecord(rec,file.cid);
  IF rec.err THEN RETURN END;
  IF rec.hdr=omf.LibHdr THEN
    align:=VAL(LONGINT,rec.len)+4;
    omf.SkipRecord(rec);
    pos:=RndFile.CurrentPos(file.cid);
    omf.OpenRecord(rec,file.cid);
    IF rec.err THEN RETURN END;
  END;
  WHILE (rec.hdr=omf.THEADR) OR (rec.hdr=omf.LHEADR) DO
    size:=VAL(LONGINT,rec.len)+4;
    omf.ReadName(rec,name);
    IF rec.err THEN RETURN END;
    omf.SkipRecord(rec);
    REPEAT
      omf.OpenRecord(rec,file.cid);
      IF rec.err THEN RETURN END;
      size:=size+VAL(LONGINT,rec.len)+4;
      omf.SkipRecord(rec);
    UNTIL (rec.hdr=omf.MODEND) OR (rec.hdr=omf.MODEND32);
    i:=(((size+align-1) DIV align) * align) - size;
    IF (i > 0) THEN omf.Skip(file.cid, i) END;
    NEW(mod);
    mod.file:=file;
    mod.start:=pos;
    mod.Init(name,size,0);
    new.Add(mod);
    pos:=RndFile.CurrentPos(file.cid);
    omf.OpenRecord(rec,file.cid);
    IF rec.err THEN
      IF IOResult.ReadResult(file.cid)=IOResult.endOfInput THEN lib:=new END;
      RETURN
    END;
  END;
  IF rec.hdr=omf.LibEnd THEN lib:=new END;
END OpenLibrary;

END xlOMF.
