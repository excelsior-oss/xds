(** Copyright (c) 1995 xTech Ltd, Russia. All Rights Reserved. *)
(** XDS librarian. Description of OMF structures. *)
<*+ M2EXTENSIONS *>
<* ALIGNMENT="1" *>
IMPLEMENTATION MODULE xldOMF; (* Hady. Nov 3 1995 *)

IMPORT
  SYSTEM
  ,IOChan
  ,RawIO
  ,RndFile
  ,IOResult
  ,dbg:=xlMsg
  ,Printf
  ;

FROM SYSTEM IMPORT ROT;

PROCEDURE hash (s- :ARRAY OF CHAR; VAR px,pd,po,od :INTEGER );
  TYPE
    SET16 = SYSTEM.SET16;
    CARD16 = SYSTEM.CARD16;
  CONST
    BL = SET16{5};
  VAR
    block_x,block_d,
    bucket_x,bucket_d,
    ch                :SET16;
    i, j, l           :CARDINAL;
BEGIN
  l := LENGTH(s);  ASSERT( (l>0) & (l<256) );
  block_x  := SET16(l)+BL;
  bucket_d := block_x;
  block_d  := SET16{};
  bucket_x := SET16{};

  i := 0;
  j := l;
  LOOP
    DEC(j);
    ch       := SET16(ORD(s[j]))*SET16{0..7} + BL;
    bucket_x := ROT(bucket_x,-2) / ch;
    block_d  := ROT(block_d,+2) / ch;

    DEC(l); IF (l=0) THEN EXIT END;

    ch       := SET16(ORD(s[i]))*SET16{0..7} + BL;
    INC(i);
    block_x  := ROT(block_x,+2) / ch;
    bucket_d := ROT(bucket_d,-2) / ch;
  END;

  px := VAL(INTEGER,CARD16(block_x));
  pd := VAL(INTEGER,CARD16(block_d));
  po := VAL(INTEGER,CARD16(bucket_x));
  od := VAL(INTEGER,CARD16(bucket_d));
END hash;

(*
PROCEDURE hash(s-: ARRAY OF CHAR; VAR px,pd,po,od: INTEGER);
  TYPE
    SET16 = SYSTEM.SET16;
  VAR page_index,page_index_delta,page_ofset,page_ofset_delta,ch: SET16;
      i,j,l: CARDINAL;
BEGIN
  page_index:=SET16{}; page_index_delta:=SET16{};
  page_ofset:=SET16{}; page_ofset_delta:=SET16{};
  l:=LENGTH(s);
  IF (l>0) THEN
    ASSERT(l<=255);
    i:=0; j:=l;
    ch:=SYSTEM.CAST(SET16,l)*SET16{0..7}+SET16{5};
    REPEAT
      page_index:=SYSTEM.ROT(page_index,+2)/ch;
      page_ofset_delta:=SYSTEM.ROT(page_ofset_delta,-2)/ch;

      j:=j-1;
      ch:=SYSTEM.CAST(SET16,ORD(s[j]))*SET16{0..7}+SET16{5};
      page_ofset:=SYSTEM.ROT(page_ofset,-2)/ch;
      page_index_delta:=SYSTEM.ROT(page_index_delta,+2)/ch;

      ch:=SYSTEM.CAST(SET16,ORD(s[i]))*SET16{0..7}+SET16{5};
      i:=i+1;
    UNTIL (i>=l);
  END;

  px:=SYSTEM.CAST(INTEGER,page_index);
  po:=SYSTEM.CAST(INTEGER,page_ofset);
  pd:=SYSTEM.CAST(INTEGER,page_index_delta);
  od:=SYSTEM.CAST(INTEGER,page_ofset_delta);
END hash;
*)


PROCEDURE LibHash (s- :ARRAY OF CHAR; blocks,buckets :INTEGER; VAR bl_x, bl_d, b_x, b_d :INTEGER);
BEGIN
  hash(s, bl_x, bl_d, b_x, b_d );

  bl_x := bl_x MOD blocks;
  bl_d := bl_d MOD blocks; IF (bl_d=0) THEN bl_d := 1 END;

  b_x := b_x MOD buckets;
  b_d := b_d MOD buckets; IF (b_d=0) THEN b_d := 1 END;
END LibHash;

VAR padding: ARRAY [0..LibPageSize-1] OF CHAR;

PROCEDURE WritePadding(to: ChanId; len: CARDINAL);
BEGIN
  WHILE len>LibPageSize DO
    IOChan.RawWrite(to,SYSTEM.ADR(padding),LibPageSize);
    len:=len-LibPageSize;
  END;
  IF len>0 THEN
    IOChan.RawWrite(to,SYSTEM.ADR(padding),len);
  END;
END WritePadding;

PROCEDURE WriteLibHeader(to: ChanId; page,dofs,dsize: INTEGER; flg: CHAR);
VAR
  type :HeaderType;
  len  :LengthType;
BEGIN
  IF ~IsXOMF THEN
     type := LibHdr;
  ELSE
     type := XOMF_LibHdr;
  END;
  len  := VAL(LengthType,page-3);
  IOChan.RawWrite(to,SYSTEM.ADR(type),1);
  IOChan.RawWrite(to,SYSTEM.ADR(len),2);
  IOChan.RawWrite(to,SYSTEM.ADR(dofs),4);
  IOChan.RawWrite(to,SYSTEM.ADR(dsize),2);
  IOChan.RawWrite(to,SYSTEM.ADR(flg),1);
  WritePadding(to,LibPageSize-10);
END WriteLibHeader;

PROCEDURE WriteLibEnd(to: ChanId; padd: INTEGER);
  VAR type: HeaderType; len: LengthType;
BEGIN
  ASSERT(padd>=3);
  type:=LibEnd;
  len:=VAL(LengthType,padd-3);
  IOChan.RawWrite(to,SYSTEM.ADR(type),1);
  IOChan.RawWrite(to,SYSTEM.ADR(len),2);
  WritePadding(to,padd-3);
END WriteLibEnd;

PROCEDURE WriteTHEADR(cid: ChanId; name-: ARRAY OF CHAR);
  VAR hdr: ARRAY [0..3] OF BYTE; l: CARDINAL;
BEGIN
  l:=LENGTH(name);
  IF ~IsXOMF THEN
     ASSERT(l<=255);
     hdr[0]:=THEADR;
  ELSE
     IF l > 255 THEN l := 255; END;
     hdr[0]:=XOMF_THEADR;
  END;
  hdr[1]:=VAL(BYTE,(l+2) MOD 256);
  hdr[2]:=VAL(BYTE,(l+2) DIV 256);
  hdr[3]:=VAL(BYTE,l);
  IOChan.RawWrite(cid,SYSTEM.ADR(hdr),4);
  IOChan.RawWrite(cid,SYSTEM.ADR(name),l);
  hdr[0]:=0;
  IOChan.RawWrite(cid,SYSTEM.ADR(hdr),1);
END WriteTHEADR;


PROCEDURE WriteImportByName( cid     :ChanId;
                             iname-  :ARRAY OF CHAR;
                             mname-  :ARRAY OF CHAR;
                             ename-  :ARRAY OF CHAR
                           );
  VAR
    il,el,ml,l :CARDINAL;
    hdr        :ARRAY [0..8] OF BYTE;

BEGIN
  IF ~IsXOMF THEN
     il:=LENGTH(iname); ASSERT(il<=255);
     el:=LENGTH(ename); ASSERT(el<=255);
     ml:=LENGTH(mname); ASSERT(ml<=255);
     l:=il+ml+el+8;
     hdr[0]:=COMENT;
     hdr[1]:=VAL(BYTE,l MOD 256);
     hdr[2]:=VAL(BYTE,l DIV 256);
     hdr[3]:=0;    (* Comment type *)
     hdr[4]:=0A0H; (* Comment class *)
     hdr[5]:=1;    (* Subtype *)
     hdr[6]:=0;    (* ordinal flag *)
     hdr[7]:=VAL(BYTE,il);
     IOChan.RawWrite(cid,SYSTEM.ADR(hdr),8);
     IOChan.RawWrite(cid,SYSTEM.ADR(iname),il);
     hdr[0]:=VAL(BYTE,ml);
     IOChan.RawWrite(cid,SYSTEM.ADR(hdr),1);
     IOChan.RawWrite(cid,SYSTEM.ADR(mname),ml);
     hdr[0]:=VAL(BYTE,el);
     IOChan.RawWrite(cid,SYSTEM.ADR(hdr),1);
     IOChan.RawWrite(cid,SYSTEM.ADR(ename),el);
  ELSE
     il:=LENGTH(iname);
     el:=LENGTH(ename);
     ml:=LENGTH(mname); ASSERT(ml<=255);
     l:=il+ml+el+10;
     hdr[0]:=COMENT;
     hdr[1]:=VAL(BYTE,l MOD 256);
     hdr[2]:=VAL(BYTE,l DIV 256);
     hdr[3]:=0;    (* Comment type *)
     hdr[4]:=0A0H; (* Comment class *)
     hdr[5]:=1;    (* Subtype *)
     hdr[6]:=0;    (* ordinal flag *)
     hdr[7]:=VAL(BYTE,il MOD 256);
     hdr[8]:=VAL(BYTE,il DIV 256);
     IOChan.RawWrite(cid,SYSTEM.ADR(hdr),9);
     IOChan.RawWrite(cid,SYSTEM.ADR(iname),il);
     hdr[0]:=VAL(BYTE,ml);
     IOChan.RawWrite(cid,SYSTEM.ADR(hdr),1);
     IOChan.RawWrite(cid,SYSTEM.ADR(mname),ml);
     hdr[0]:=VAL(BYTE,el MOD 256);
     hdr[1]:=VAL(BYTE,el DIV 256);
     IOChan.RawWrite(cid,SYSTEM.ADR(hdr),2);
     IOChan.RawWrite(cid,SYSTEM.ADR(ename),el);
  END;
  hdr[0]:=0;                               -- check sum  
  IOChan.RawWrite(cid,SYSTEM.ADR(hdr),1);
END WriteImportByName;

PROCEDURE WriteImportByOrdinal( cid    :ChanId;
                                iname- :ARRAY OF CHAR;
                                mname- :ARRAY OF CHAR;
                                ord    :CARDINAL
                              );
  VAR il,ml,l :CARDINAL;
      hdr     :ARRAY [0..8] OF BYTE;
BEGIN
  IF ~IsXOMF THEN
      il:=LENGTH(iname); ASSERT(il<=255);
      ml:=LENGTH(mname); ASSERT(ml<=255);
      l:=il+ml+2+7;
      hdr[0]:=COMENT;
      hdr[1]:=VAL(BYTE,l MOD 256);
      hdr[2]:=VAL(BYTE,l DIV 256);
      hdr[3]:=0;    (* Comment type *)
      hdr[4]:=0A0H; (* Comment class *)
      hdr[5]:=1;    (* Subtype *)
      hdr[6]:=1;    (* ordinal flag *)
      hdr[7]:=VAL(BYTE,il);
      IOChan.RawWrite(cid,SYSTEM.ADR(hdr),8);
      IOChan.RawWrite(cid,SYSTEM.ADR(iname),il);
      hdr[0]:=VAL(BYTE,ml);
      IOChan.RawWrite(cid,SYSTEM.ADR(hdr),1);
      IOChan.RawWrite(cid,SYSTEM.ADR(mname),ml);
   ELSE
      il:=LENGTH(iname);
      ml:=LENGTH(mname); ASSERT(ml<=255);
      l:=il+ml+3+7;
      hdr[0]:=COMENT;
      hdr[1]:=VAL(BYTE,l MOD 256);
      hdr[2]:=VAL(BYTE,l DIV 256);
      hdr[3]:=0;    (* Comment type *)
      hdr[4]:=0A0H; (* Comment class *)
      hdr[5]:=1;    (* Subtype *)
      hdr[6]:=1;    (* ordinal flag *)
      hdr[7]:=VAL(BYTE,il MOD 256);
      hdr[8]:=VAL(BYTE,il DIV 256);
      IOChan.RawWrite(cid,SYSTEM.ADR(hdr),9);
      IOChan.RawWrite(cid,SYSTEM.ADR(iname),il);
      hdr[0]:=VAL(BYTE,ml);
      IOChan.RawWrite(cid,SYSTEM.ADR(hdr),1);
      IOChan.RawWrite(cid,SYSTEM.ADR(mname),ml);
  END;
  hdr[0]:=VAL(BYTE,ord MOD 256);
  hdr[1]:=VAL(BYTE,ord DIV 256);
  IOChan.RawWrite(cid,SYSTEM.ADR(hdr),2);
  hdr[0]:=0;
  IOChan.RawWrite(cid,SYSTEM.ADR(hdr),1);
END WriteImportByOrdinal;


PROCEDURE WriteMODEND(cid: ChanId);
  VAR hdr: ARRAY [0..4] OF BYTE;
BEGIN
  hdr[0]:=MODEND;
  hdr[1]:=2;
  hdr[2]:=0;
  hdr[3]:=0;
  hdr[4]:=0;
  IOChan.RawWrite(cid,SYSTEM.ADR(hdr),5);
END WriteMODEND;

(* -------------------- INPUT ---------------------- *)

PROCEDURE readErr(VAR r: Record): BOOLEAN;
BEGIN
  IF r.err THEN RETURN TRUE END;
  r.err:=IOResult.ReadResult(r.file)#IOResult.allRight;
  RETURN r.err;
END readErr;


PROCEDURE ReadByte(VAR r: Record; VAR b: BYTE);
BEGIN
  IF r.len=0 THEN r.err:=TRUE END;
  IF ~r.err THEN
    RawIO.Read(r.file,b);
    IF ~readErr(r) THEN DEC(r.len) END;
  END;
END ReadByte;


(*
PROCEDURE ReadDWord ( VAR r :Record; VAR dw :CARDINAL );
BEGIN
  IF (r.len<4) THEN
    r.err:=TRUE;
    RETURN;
  END;
  RawIO.Read(r.file, dw);
  IF ~readErr(r) THEN DEC(r.len,4) END;
END ReadDWord;
*)

PROCEDURE OpenRecord(VAR r: Record; file: ChanId);
BEGIN
  r.err:=FALSE;
  r.file:=file;
  RawIO.Read(file,r.hdr);
  IF readErr(r) THEN RETURN END;
  RawIO.Read(file,r.len);
  IF readErr(r) THEN RETURN END;
  DEC(r.len);
END OpenRecord;

PROCEDURE Skip(file: ChanId; len: INTEGER);
BEGIN
  RndFile.SetPos(file,RndFile.NewPos(file, len, 1, RndFile.CurrentPos(file)));
END Skip;

PROCEDURE SkipRecord(VAR r: Record);
BEGIN
  IF r.err THEN RETURN END;
  RndFile.SetPos(r.file,RndFile.NewPos(r.file,VAL(INTEGER,r.len)+1,1,RndFile.CurrentPos(r.file)));
END SkipRecord;

PROCEDURE SkipBytes(VAR r: Record; bytes: INTEGER);
BEGIN
  IF bytes>r.len THEN r.err:=TRUE END;
  IF r.err THEN RETURN END;
  RndFile.SetPos(r.file,RndFile.NewPos(r.file, bytes, 1, RndFile.CurrentPos(r.file)));
  r.len:=r.len-bytes;
END SkipBytes;

PROCEDURE ReadName(VAR r: Record; VAR name: NameType);
  VAR len: BYTE; rdlen: CARDINAL;
BEGIN
  IF r.err THEN RETURN END;
  ReadByte(r,len);
  IF r.err THEN RETURN END;
  IF len=0 THEN
    name[0]:=0C;
  ELSE
    IF len>r.len THEN r.err:=TRUE; RETURN END;
    IOChan.RawRead(r.file,SYSTEM.ADR(name),VAL(CARDINAL,len),rdlen);
    IF readErr(r) THEN RETURN END;
    IF rdlen#VAL(CARDINAL,len) THEN
      r.err:=TRUE; RETURN
    END;
    name[rdlen]:=0C;
    r.len:=r.len-VAL(LengthType,len);
  END;
END ReadName;

PROCEDURE ReadIndex(VAR r: Record; VAR c: INTEGER);
  VAR b: BYTE;
BEGIN
  IF r.err THEN RETURN END;
  ReadByte(r,b);
  IF r.err THEN RETURN END;
  c:=VAL(INTEGER,b);
  IF c>127 THEN
    c:=c MOD 128;
    ReadByte(r,b);
    IF r.err THEN RETURN END;
    c:=c*256+VAL(INTEGER,b);
  END;
END ReadIndex;


(* ------------ INIT --------------- *)

PROCEDURE initPad;
  VAR i: CARDINAL;
BEGIN
  FOR i:=0 TO LibPageSize-1 DO padding[i]:=0C END;
END initPad;

BEGIN
  initPad;
END xldOMF.
