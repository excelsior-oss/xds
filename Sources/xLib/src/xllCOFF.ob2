(** Copyright (c) 1995 xTech Ltd, Russia. All Rights Reserved. *)
(** XDS librarian. Implementation of Library for Microsoft COFF format. *)
MODULE xllCOFF; (* Hady. Nov 1, 1995 *)

IMPORT
  desc:=xlDesc
  ,defs:=xldPE
  ,RawIO
  ,TimeConv
  ;

TYPE
  String  = POINTER TO ARRAY OF CHAR;
  ChanId  = desc.ChanId;

TYPE
  Member = desc.Member;
  MemberDesc = desc.MemberDesc;
  ArchiveHeader = defs.ArchiveHeader;

  NameList = POINTER TO NameDesc;
  NameDesc = RECORD
    name: String;
    next: NameList;
  END;

  CoffMember* = POINTER TO CoffMemberDesc;
  CoffMemberDesc* = RECORD (MemberDesc)
    hdr   : ArchiveHeader;
    offset: LONGINT;
    names : NameList; (* list of external names *)
    ncount: LONGINT;  (* count of above *)
    next  : Member;
  END;

  LNames = LNDesc;
  LNDesc = RECORD (MemberDesc)
    list: NameList;
    last: NameList;
    ofs : LONGINT;
  END;

  Dir0 = POINTER TO Dir0Desc;
  Dir0Desc = RECORD (CoffMemberDesc)
    offsets: POINTER TO ARRAY OF LONGINT;
    names  : POINTER TO ARRAY OF String;
  END;

  Dir1 = POINTER TO Dir1Desc;
  Dir1Desc = RECORD (CoffMemberDesc)
    offsets: POINTER TO ARRAY OF LONGINT;
    nmtab  : POINTER TO ARRAY OF SYSTEM.CARD16;
    names  : POINTER TO ARRAY OF String;
  END;

  Library* = POINTER TO LibraryDesc;
  LibraryDesc = RECORD (desc.LibraryDesc)
    time : LONGINT;
    list : Member;
    last : Member;
    names: LNames;
    dir0 : Dir0;
    dir1 : Dir1;
    base : LONGINT;
    mcount: LONGINT; (* members count *)
    ncount: LONGINT; (* names count *)
  END;

  NameIterator* = RECORD (desc.NameIterator)
    count: LONGINT;
    list: NameList;
    last: NameList;
  END;

(* Archive Header operations *)

PROCEDURE initHeader(VAR h: ArchiveHeader);
BEGIN
  h.name   [0]:=0X;
  h.date   [0]:=0X;
  h.iserId [0]:=0X;
  h.groupId[0]:=0X;
  h.mode   [0]:=0X;
  h.size   [0]:=0X;
  h.tail      :=defs.ArchiveEnd;
END initHeader;

PROCEDURE fillSpaces(VAR s: ARRAY OF CHAR);
  VAR i: LONGINT;
BEGIN
  i:=LENGTH(s);
  WHILE i<LEN(s) DO s[i]:=" "; INC(i) END;
END fillSpaces;

PROCEDURE prepareHeader(VAR h: ArchiveHeader);
BEGIN
  fillSpaces(h.name   );
  fillSpaces(h.date   );
  fillSpaces(h.iserId );
  fillSpaces(h.groupId);
  fillSpaces(h.mode   );
  fillSpaces(h.size   );
END prepareHeader;

(* Name Iterator methods *)

PROCEDURE (VAR i: NameIterator) Put*(name-: ARRAY OF CHAR);
  VAR n: NameList;
BEGIN
  ASSERT(name[0]#0X);
  NEW(n);
  NEW(n.name,LENGTH(name)+1);
  COPY(name,n.name^);
  IF i.last=NIL THEN
    i.list:=n;
  ELSE
    i.last.next:=n;
  END;
  n.next:=NIL;
  i.last:=n;
  INC(i.count);
END Put;

(* CoffMember methods *)

PROCEDURE (m: CoffMember) Init*(name-: ARRAY OF CHAR; size,time: LONGINT);
(** CONCRETE *)
BEGIN
  m.Init^(name,size,time);
  ofset:=0;
  next:=NIL;
END Init;

(* LNames methods *)

PROCEDURE (m: LNames) Add(name-: ARRAY OF CHAR; VAR ofs: LONGINT);
  VAR l: LONGINT; VAR t: NameList;
BEGIN
  ofs:=-1;
  l:=LENGTH(name);
  IF l<=MaxShortName THEN RETURN END;
  NEW(t);
  NEW(t.name,l+1);
  COPY(name,t.name^);
  ofs:=m.ofs;
  m.ofs:=m.ofs+l+1;
  IF m.last=NIL THEN
    m.list:=t;
  ELSE
    m.last.next:=t;
  END;
  t.next:=NIL; m.last:=t;
END Add;

PROCEDURE (m: LNames) Write(to: ChanId);
  VAR t: NameList;
BEGIN
  ASSERT(m.size>0);
  t:=m.list;
  REPEAT
    RawIO.Write(to,t.name^);
    t:=t.next;
  UNTIL t=NIL;
END Write;

PROCEDURE (m: LNames) Prepare(l: Library);
BEGIN
  m.Init("//",m.ofs,l.time);
END Prepare;

(* Directory methods *)

PROCEDURE Reverse(VAR x: LONGINT);
  VAR s,d: SET; i: INTEGER;
BEGIN
  s:=SYSTEM.VAL(SET,x);
  d:={};
  i:=0;
  REPEAT
    d:=SYSTEM.ROT(d,+8);
    d:=d+s*{0..7};
    s:=SYSTEM.ROT(s,-8);
    INC(i);
  UNTIL (i=4);
  x:=SYSTEM.VAL(LONGINT,d);
END Reverse;

PROCEDURE (m: Dir0) Write(to: ChanId);
  VAR i: LONGINT;
BEGIN
  i:=0;
  WHILE i<LEN(m.offsets^) DO
    Reverse(m.offsets[i]); INC(i);
  END;
  RawIO.RawWrite(to,m.offsets^);
  i:=0;
  WHILE i<LEN(m.names^) DO
    RawIO.RawWrite(to,m.names[i]^);
    INC(i);
  END;
END Write;

PROCEDURE (m: Dir1) Write(to: ChanId);
  VAR i: LONGINT;
BEGIN
  RawIO.RawWrite(to,m.offsets^);
  RawIO.RawWrite(to,m.nmtab^);
  i:=0;
  WHILE i<LEN(m.names^) DO
    RawIO.RawWrite(to,m.names[i]^);
    INC(i);
  END;
END Write;

PROCEDURE calcNamesSize(m: Member; VAR size: LONGINT);
  VAR m: Member; n: NameList;
BEGIN
  WHILE m#NIL DO
    n:=m.names;
    WHILE n#NIL DO
      size:=size+LEN(n.name^);
      n:=n.next;
    END;
    m:=m.next;
  END;
END calcNamesSize;

PROCEDURE (d: Dir0) Prepare(l: Library);
  VAR size: LONGINT;
BEGIN
  NEW(d.offsets,l.ncount+1);
  NEW(d.names,l.ncount+1);
  size:=SIZE(d.offsets^);
  calcNamesSize(l.list,size);
  d.offsets[0]:=l.ncount;
  d.Init("/",size,l.time);
END Prepare;

PROCEDURE (d: Dir1) Prepare(l: Library);
  VAR m: Member; n: NameList; size: LONGINT;
BEGIN
  NEW(d.offsets,l.mcount+2);
  NEW(d.nmtab,l.ncount);
  NEW(d.names,l.ncount);
  size:=SIZE(d.offsets^)+SIZE(d.nmtab^);
  calcNamesSize(l.list,size);
  d.offsets[0]:=l.mcount;
  d.offsets[l.ncount+1]:=l.ncount;
  d.Init("/",size,l.time);
END Prepare;

PROCEDURE (d: Dir0) Calculate(l: Library);
  VAR m: Member; n: NameList; i: LONGINT;
BEGIN
  m:=l.list; i:=1;
  WHILE m#NIL DO
    n:=m.names;
    WHILE n#NIL DO
      d.offsets[i]:=m.offset;
      d.names[i]:=n.name;
      INC(i);
      n:=n.next;
    END;
    m:=m.next;
  END;
END Calculate;

PROCEDURE strGreater(s0-,s1-: ARRAY OF CHAR): BOOLEAN;
(* compare strings, considering the "_" char the greatest one *)
  VAR i: LONGINT; c0,c1: INTEGER;
BEGIN
  LOOP
    IF i>=LEN(s0) THEN c0:=0 ELSE c0:=INT(s0[i]) END;
    IF i>=LEN(s1) THEN c1:=0 ELSE c1:=INT(s1[i]) END;
    IF c0#c1 THEN
      IF c0="_" THEN c0:=INT(MAX(CHAR))+1 END;
      IF c1="_" THEN c1:=INT(MAX(CHAR))+1 END;
      RETURN c0>c1;
    END;
    IF c0=0 THEN EXIT END;
    INC(i);
  END;
  RETURN FALSE;
END strGreater;

PROCEDURE (d: Dir1) Calculate(l: Library);
  VAR m: Member; n: NameList; i,j: LONGINT;

  PROCEDURE sort(d: Dir1; from,to: LONGINT);
    VAR l,r: LONGINT; s,ts: String; tq: SYSTEM.CARD16;
  BEGIN
    l:=from; r:=to;
    s:=d.names[(from+to) DIV 2];
    REPEAT
      WHILE strGreater(s^,d.names[l]^) DO INC(l) END;
      WHILE strGreater(d.names[r]^,s^) DO DEC(r) END;
      IF l<=r THEN
        ts:=d.names[l]; d.names[l]:=d.names[r]; d.names[r]:=ts;
        tq:=d.nmtab[l]; d.nmtab[l]:=d.nmtab[r]; d.nmtab[r]:=tq;
        INC(l); DEC(r);
      END;
    UNTIL l>r;
    IF from<r THEN sort(d,from,r) END;
    IF l<to   THEN sort(d,l,to)   END;
  END sort;

BEGIN
  m:=l.list; i:=0; j:=1;
  WHILE m#NIL DO
    d.offsets[j]:=m.offset;
    n:=m.names;
    WHILE n#NIL DO
      d.nmtab[i]:=VAL(SYSTEM.CARD16,j);
      d.names[i]:=n.name;
      INC(i);
      n:=n.next;
    END;
    INC(j);
    m:=m.next;
  END;
  sort(d,0,LEN(d.names^)-1);
END Calculate;

(* Library methods *)

PROCEDURE (l: Library) Init*;
BEGIN
  NEW(l.names); l.names.ofs:=0;
  NEW(l.dir0);
  NEW(l.dir1);
  l.mcount:=0;
  l.ncount:=0;
  l.time:=TimeConv.time();
END Init;

PROCEDURE ScanNames(n: LNames; m: Member);
  VAR ofs: LONGINT; buf: ARRAY 15 OF CHAR;
BEGIN
  WHILE (m#NIL) DO
    n.Add(m.name,ofs);
    IF ofs=-1 THEN
      WholeStr.CardToStr(ofs,buf);
      Strings.Assign("/",m.hdr.name);
      Strings.Append(buf,m.hdr.name);
    ELSE
      COPY(m.name^,m.hdr.name);
      Strings.Append("/",m.hdr.name);
    END;
    m:=m.next;
  END;
END ScanNames;

PROCEDURE WriteMember(to: ChanId; m: Member);
BEGIN
  ASSERT(FALSE);
  prepareHeader(m.hdr);
  RawIO.Write(m.hdr);
  m.Write(to);
END WriteMember;

PROCEDURE (l: Library) Write*(to: ChanId);
  VAR m: Member;
    ofs: LONGINT;
BEGIN
  ScanNames(l.names,l.list);
  l.dir0.Prepare(l);
  l.dir1.Prepare(l);
  l.base:=8+SIZE(ArchiveHeader)*2+l.dir0.size+l.dir1.size;
  IF l.names.size>0 THEN
    l.base:=l.base+SIZE(ArchiveHeader)+l.names.size;
  END;
  ofs:=l.base; m:=m.list;
  WHILE m#NIL DO
    m.offset:=ofs;
    ofs:=ofs+SIZE(ArchiveHeader)+m.size;
    m:=m.next;
  END;
  l.dir0.Calculate(l);
  l.dir1.Calculate(l);
  WriteMember(to,l.dir0);
  WriteMember(to,l.dir1);
  IF l.names.size>0 THEN
    WriteMember(to,l.names);
  END;
  m:=m.list;
  WHILE m#NIL DO
    WriteMember(to,m);
    m:=m.next;
  END;
END Write;

PROCEDURE (l: Library) Add*(m: Member);
(** Member must be initialized *)
  VAR iter: NameIterator;
BEGIN
  ASSERT(m.name#NIL);
  ASSERT(m.size#0);
  INC(l.mcount);
  iter.count:=0; iter.last:=NIL;
  m.IterExported(iter);
  m.names:=iter.list;
  m.ncount:=iter.count;
  INC(l.ncount,iter.ncount);
  IF l.last=NIL THEN
    l.list:=m;
  ELSE
    l.last.next:=m;
  END;
  m.next:=NIL; l.last:=m;
END Add;

END xllCOFF.
