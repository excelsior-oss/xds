MODULE ReadAOUT;

(* --------------------------------------------------------------------*)
IMPORT
  SYSTEM,
  adt,
  Strings,
  lstr:= LongStrs,
  file:= H2DFile,
  io:= Printf,
  objs:= Objects,
  RTS:= oberonRTS;


(* --------------------------------------------------------------------*)
CONST
  GetData  = objs.GetData;
  SkipData = objs.SkipData;


(* --------------------------------------------------------------------*)
TYPE
  INT    = SYSTEM.INT32;
  CARD8  = SYSTEM.CARD8; 
  CARD16 = SYSTEM.CARD16;
  CARD   = SYSTEM.CARD32;

(* --------------------------------------------------------------------*)
CONST

  THEADR  = 080H;  (* Translator module header record *)
  LHEADR  = 082H;  (* Library module header record *)
  COMENT  = 088H;  (* Comment record *)
  MODEND  = 08aH;  (* Module end record *)
  EXTDEF  = 08cH;  (* External names definition record *)
  TYPDEF  = 08eH;  (* Type definition record *)
  PUBDEF  = 090H;  (* Public names definition record *)
  LINNUM  = 094H;  (* Line numbers record *)
  LNAMES  = 096H;  (* List of names record *)
  SEGDEF  = 098H;  (* Segment definition record *)
  GRPDEF  = 09aH;  (* Group definition record *)
  FIXUPP  = 09cH;  (* Fixup record *)
  LEDATA  = 0a0H;  (* Logical enumerated data record *)
  LIDATA  = 0a2H;  (* Logical iterated data record *)
  COMDEF  = 0b0H;  (* Communal names definition record *)
  BAKPAT  = 0b2H;  (* Bacpatch record *)
  LEXTDEF = 0b4H;  (* Local external names definition record *)
  LPUBDEF = 0b6H;  (* Local public names definition record *)
  LCOMDEF = 0b8H;  (* Local communal names definition record *)
  CEXTDEF = 0bcH;  (* COMDAT external names definition record *)
  COMDAT  = 0c2H;  (* Common block *)
  LINSYM  = 0c4H;  (* Symbol line numbers record *)
  ALIAS   = 0c6H;  (* Alias definition record *)
  NBKPAT  = 0c8H;  (* Named backpatch record *)
  LLNAMES = 0caH;  (* Local logical names definition record *)
  VERNUM  = 0ccH;  (* OMF version number record *)
  VENDEXT = 0ceH;  (* Vendor-specific OMF extension record *)
  LIBHDR  = 0f0H;  (* Library header *)
  LIBEND  = 0f1H;  (* Library end *)

  OMFEXT  = 0a0H;  (* OMF extension comment record *)
  IMPDEF  = 001H;  (* OMF extension: import definition record *)
  EXPDEF  = 002H;  (* OMF extension: export definition record *)

  NOMFEX  = 0a1H;  (* New omf extension comment record *)

(* Add this constant (using the | operator) to get the 32-bit variant
   of a record type.  Some fields will contain 32-bit values instead
   of 16-bit values. *)

  REC32   = 001H;

(*---------------------------------------------------------------------*)

(* Используется при замыкании внутримодульных ссылок *)

CONST (* tags in tables *)
  K_LOCAL = {7};

VAR WasLocals, WasCOMDEFs, WasLCOMDEFs: BOOLEAN;

TYPE
  Local = POINTER TO LocalDesc;
  LocalDesc = RECORD (adt.ElementDesc)
    seg   : objs.Segment;
    group : objs.Group;
    length: CARD;     (* For (L)COMDEF only *)
    offset: CARD;
    kind  : SET;
  END;

(*---------------------------------------------------------------------*)
(* Таблица lnames и llnames *)
TYPE
  LName = POINTER TO LNameDesc;
  LNameDesc = RECORD (adt.ElementDesc);
    name: objs.HashCode;
    kind: SET;
  END;

(*---------------------------------------------------------------------*)
TYPE
  ExpDef = POINTER TO ExpDefDesc;
  ExpDefDesc = RECORD (adt.ElementDesc)
    int_name, ext_name: objs.HashCode;
    ordinal: CARD16;
    int_name_present: BOOLEAN;
  END;

(*---------------------------------------------------------------------*)
TYPE
  ExtName = POINTER TO ExtNameDesc;
  ExtNameDesc = RECORD (adt.ElementDesc)
    name: objs.HashCode;
    type: CARD16;
    kind: SET;
    length: CARD;
  END;

(* --------------------------------------------------------------------*)
TYPE
  (* Таблица pubdef и lpubdef *)
  PubName = POINTER TO PubNameDesc;
  PubNameDesc = RECORD (adt.ElementDesc)
    name  : objs.HashCode;
    group : objs.Group;
    seg   : objs.Segment;
    offset: CARD;
    type  : CARD16;
    kind  : SET;
  END;


(* --------------------------------------------------------------------*)
CONST
  LINNUM_CV   = 0;
  LINNUM_HLL4 = 1;

(* --------------------------------------------------------------------*)
VAR
(*  record_type: CARD8;*)
  record_len : CARD16;
  record_pos : CARD;
  AoutMidMag : CARD;
  AoutBssSize : CARD; 
  AoutTextSegSize : CARD;
  AoutDataSegSize : CARD; 
  AoutTextRelSize : CARD;
  AoutDataRelSize : CARD;
  AoutSymbTabSize : CARD;  
(*  AoutStrnTabSize : CARD;  *)
  AoutEntryOffset : CARD; 
  OffStr : CARD;
  FPC    : CARD;

  LINNUM_FORMAT: INT;
  Cntr : INT;
  (*libhdr_rec: objs.OMFRecord;*)
  file_name : lstr.String;

  (*Locals,*) 
  Segs, PubNames, Groups, LNames, ExpDefs, ExtNames: adt.List;

  LedataSeg: objs.Segment;
  LastOffset: CARD;


  (* Frames *)
(*  frame_kinds: ARRAY 4 OF CARD8;
  seg_frames : ARRAY 4 OF objs.Segment;
  grp_frames : ARRAY 4 OF objs.Group;
  name_frames: ARRAY 4 OF objs.HashCode; *)

  (* Targets *)
(*  target_kinds: ARRAY 4 OF CARD8;
  seg_targets : ARRAY 4 OF objs.Segment;
  grp_targets : ARRAY 4 OF objs.Group;
  name_targets: ARRAY 4 OF objs.HashCode; *)

  card8 : CARD8;
  card16: CARD16;
  card32: CARD;

(*  CODE *, DATA *, BSS * : objs.HashCode;  *)


(* --------------------------------------------------------------------*)
PROCEDURE SetReverse(CheckVal : CARD);
VAR 
    c1,c2,c3,c4:CARD;   
BEGIN
 c1 := (CheckVal MOD 100H);
 c2 := ((CheckVal DIV 100H) MOD 100H);
 c3 := ((CheckVal DIV 10000H) MOD 100H);
 c4 := ((CheckVal DIV 1000000H) MOD 100H);
 IF (c4 # 0H) THEN objs.ReverseIt := TRUE
 ELSE objs.ReverseIt := FALSE; 
 END;
END SetReverse;
(* --------------------------------------------------------------------*)
PROCEDURE ReverseCard32(VAR d : CARD); 
VAR 
    c1,c2,c3,c4,cr:CARD;   
BEGIN

 IF objs.ReverseIt THEN
   c1 := (d MOD 100H);
   c2 := ((d DIV 100H) MOD 100H);
   c3 := ((d DIV 10000H) MOD 100H);
   c4 := ((d DIV 1000000H) MOD 100H);

   cr := c4 + 100H*c3 + 10000H*c1+1000000H*c2; 
   d := cr;
 END;

END ReverseCard32;
(* --------------------------------------------------------------------*)
(*PROCEDURE ReverseCard16(VAR d : CARD16); 
VAR 
    c1,c2,cr:CARD;   
BEGIN

 IF objs.ReverseIt THEN
   c1 := (d MOD 100H);
   c2 := ((d DIV 100H) MOD 100H);

   cr := c2 + 100H*c1;
 END;

END ReverseCard16;*)

(* --------------------------------------------------------------------*)
(*PROCEDURE Error (fmt-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN
  io.printf(fmt, x);
  HALT;
END Error;

PROCEDURE Warning (fmt-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN
  io.printf(fmt, x);
END Warning; *)

(* --------------------------------------------------------------------*)
(*PROCEDURE getLName(idx: INT; VAR name: objs.HashCode): BOOLEAN;
VAR e: adt.Element;
BEGIN
  LNames.FindFirst(e);
  WHILE idx > 0 DO
    LNames.FindNext(e);
    DEC(idx);
  END;
  name:= e(LName).name;
  RETURN (e(LName).kind * K_LOCAL) = K_LOCAL;
END getLName;*)


(* --------------------------------------------------------------------*)
PROCEDURE getExtName(idx: INT): objs.HashCode;
VAR e: adt.Element;
BEGIN
  ExtNames.FindFirst(e);
  WHILE idx > 0 DO
    ExtNames.FindNext(e);
    DEC(idx);
  END;
  RETURN e(ExtName).name;
END getExtName;

(* --------------------------------------------------------------------*)
(*PROCEDURE getGroup(idx: INT): objs.Group;
VAR e: adt.Element;
BEGIN
  Groups.FindFirst(e);
  WHILE idx > 0 DO
    Groups.FindNext(e);
    DEC(idx);
  END;
  RETURN e(objs.Group);
END getGroup;*)

(* --------------------------------------------------------------------*)
(*PROCEDURE getSeg(idx: INT): objs.Segment;
VAR e: adt.Element;
BEGIN
  Segs.FindFirst(e);
  WHILE idx > 0 DO
    Segs.FindNext(e);
    DEC(idx);
  END;
  RETURN e(objs.Segment);
END getSeg;*)

(* --------------------------------------------------------------------*)
(*PROCEDURE RemainedDataLen(): CARD;
BEGIN
  RETURN record_pos + record_len - 1 - objs.file_ptr;
END RemainedDataLen;*)

(* --------------------------------------------------------------------*)
(* PROCEDURE CanRead(): BOOLEAN;
BEGIN
  RETURN (record_pos + record_len - 1) > objs.file_ptr;
END CanRead;
*)
(* --------------------------------------------------------------------*)
PROCEDURE GetName(VAR name: ARRAY OF CHAR);
VAR iC : CARD16;
BEGIN
  card8 := 10H;
  iC := 0;
  WHILE card8 # 0H DO
    GetData(card8);
    iC := iC + 1;
  END;
  objs.file_ptr := objs.file_ptr - iC;
  GetData(name, iC);
  name[iC] := 0X;
END GetName;
(*
PROCEDURE GetIdent(): objs.HashCode;
VAR name: ARRAY 256 OF CHAR;
BEGIN
  GetName(name);
  RETURN objs.StrToIndex(name);
END GetIdent;
*)
(* --------------------------------------------------------------------*)
PROCEDURE GetIndex(): CARD16;
BEGIN
  GetData(card8);
  IF (card8 DIV 80H) # 0 THEN
    card16:= VAL(CARD16, card8 MOD 80H) * 100H;
    GetData(card8);
    card16:= card16 + card8;
  ELSE
    card16:= card8;
  END;
  RETURN card16;
END GetIndex;

(* --------------------------------------------------------------------*)

PROCEDURE GetAoutHeader();
BEGIN

 GetData(AoutMidMag);
 GetData(AoutTextSegSize);
 GetData(AoutDataSegSize);
 GetData(AoutBssSize);
 GetData(AoutSymbTabSize);
 GetData(AoutEntryOffset);
 GetData(AoutTextRelSize);
 GetData(AoutDataRelSize);
 SetReverse(AoutMidMag);
 ReverseCard32(AoutMidMag);
 ReverseCard32(AoutTextSegSize);
 ReverseCard32(AoutDataSegSize);
 ReverseCard32(AoutBssSize);
 ReverseCard32(AoutSymbTabSize);
 ReverseCard32(AoutEntryOffset);
 ReverseCard32(AoutTextRelSize);
 ReverseCard32(AoutDataRelSize);

 (*io.printf("\n%d", AoutSymbTabSize);
 io.printf("\n%d", AoutTextSegSize);
 io.printf("\n%d", AoutDataSegSize);*)
(* io.printf("%$8x", AoutSymbTabSize); *)

 record_pos:= objs.file_ptr;
END GetAoutHeader;

(* --------------------------------------------------------------------*)
PROCEDURE Init();
BEGIN
  WasLocals:= FALSE;
  WasCOMDEFs:= FALSE;
  WasLCOMDEFs:= FALSE;
  LedataSeg:= NIL;
  LastOffset:= 0;

(*  CODE := objs.StrToIndex("CODE");
  DATA := objs.StrToIndex("DATA");
  BSS  := objs.StrToIndex("BSS");  *)

  adt.NewList(ExpDefs);
  adt.NewList(ExtNames);
  adt.NewList(Segs);
  adt.NewList(PubNames);
  adt.NewList(Groups);
  adt.NewList(LNames);
  adt.NewList(ExpDefs);

  RTS.Collect();
END Init;


PROCEDURE GetTextSeg();
VAR 
    length: CARD;
    class : objs.HashCode;
    s     : objs.Segment;
    p     : objs.RawData;

BEGIN

  length:= AoutTextSegSize ;

(*  card16:= GetIndex(); *)
  (*name:= 1;*)
  class := objs.CODE;
  (*overlay:= 0;*)
  objs.NewSegment(s, 1, class, 0, length);
  Segs.Insert(s); 
  objs.CurrentModule.segs.Insert(s);
  LedataSeg:= s;
  NEW(p, length);
  GetData(p^);
  SYSTEM.MOVE(SYSTEM.ADR(p^), SYSTEM.ADR(s.text[0]), length);
  s.attributes := 3;
  s.alignment := 16;
END GetTextSeg;


PROCEDURE GetDataSeg();
VAR 
    length: CARD;
    class : objs.HashCode;
    s     : objs.Segment;
    p     : objs.RawData;

BEGIN

  length:= AoutTextSegSize ;
  card16:= GetIndex();
 (* name:= 2; *)
  class := objs.DATA;
 (* overlay:= 0;*)
  objs.NewSegment(s, 2, class, 0, length);
  Segs.Insert(s); objs.CurrentModule.segs.Insert(s);
  LedataSeg:= s;
  NEW(p, length);
  GetData(p^);
  SYSTEM.MOVE(SYSTEM.ADR(p^), SYSTEM.ADR(s.text[0]), length);
  s.attributes := 3;
  s.alignment := 16;

END GetDataSeg;


PROCEDURE GetBssSeg();
VAR 
    length: CARD;
    class : objs.HashCode;
    s     : objs.Segment;

BEGIN

  length:= AoutTextSegSize ;
  card16:= GetIndex();
  (*name:= 0;*)
(*  class:= StrToIndex("BSS"); *)
  class := objs.BSS;
  (*overlay:= 0;*)
  objs.NewSegment(s, 0, class, 0, length);
  Segs.Insert(s); objs.CurrentModule.segs.Insert(s);

END GetBssSeg;

PROCEDURE GetExtNameIdent(n : CARD): objs.HashCode;
VAR  name: ARRAY 256 OF CHAR;
(*    name : lstr.String; *)
    
BEGIN
 objs.file_ptr := 20H + AoutDataSegSize + AoutTextSegSize + AoutDataRelSize + AoutTextRelSize + AoutBssSize + AoutSymbTabSize + OffStr;
 GetName(name);
 (*io.printf("\n%s %d", name, Cntr);*)

 objs.file_ptr := 20H + AoutDataSegSize + AoutTextSegSize + AoutDataRelSize + AoutTextRelSize + AoutBssSize + (n*12);                      
 (*io.printf("\n%d", 20H + AoutDataSegSize + AoutTextSegSize + AoutDataRelSize + AoutTextRelSize + AoutBssSize + (n*12) );*)

 RETURN objs.StrToIndex(name);
END GetExtNameIdent;

PROCEDURE AddExtName(n : CARD);
VAR en : ExtName;
(*  pn   : PubName;
  p    : objs.Public; *)

BEGIN
(* group := NIL;
 seg := 1; *)
 FPC := objs.file_ptr;
 GetData(OffStr);
(* IF objs.ReverseIt THEN
   GetData(OffStr);
   GetData(OffStr);
 END;  *)
 ReverseCard32(OffStr);
 FPC := objs.file_ptr;
 (*io.printf("\n%d", OffStr);*)
 IF OffStr > 0 THEN
 NEW(en);
 en.name:= GetExtNameIdent(n);
 en.kind:= objs.K_EXTDEF;
 en.length:= 0;
 ExtNames.Insert(en);
 ELSE 
  objs.file_ptr := 20H + AoutDataSegSize + AoutTextSegSize + AoutDataRelSize + AoutTextRelSize;
  objs.file_ptr := objs.file_ptr + AoutBssSize + (n*12);                      
 END;
END AddExtName;

PROCEDURE BuildExtNames ();
VAR n, e : INT;

BEGIN
 e := AoutSymbTabSize;
 objs.file_ptr := 20H + AoutDataSegSize + AoutTextSegSize +  AoutBssSize + AoutDataRelSize + AoutTextRelSize;
 FPC := objs.file_ptr;
 FOR n := 1 TO (e DIV 12) DO 
   Cntr := 0;
   AddExtName(n);
 END; 

END BuildExtNames;


PROCEDURE AddTextSegFixup();

 VAR
    seg: objs.Segment; 
    fixup: objs.Fixup;
    offset: CARD;

BEGIN

 seg := LedataSeg;
 objs.NewFixup(fixup);
 seg.fixups.Insert(fixup);
 fixup.kind:= objs.FX_OFFSET32;
 GetData(card32);
 ReverseCard32(card32);
 offset:= card32;
 fixup.offset:= offset;
 fixup.k_target:=objs.TK_ID;
 GetData(card32);
 ReverseCard32(card32);
 fixup.name_target:= getExtName((card32 MOD 1000000H));
 (* fixup.name_target:= getExtName((card32 DIV 100H));     *)
 (*io.printf("\n%d", card32 MOD 1000000H);*)
 IF ((card32 DIV 02H) MOD 2) = 1 THEN
   fixup.kind:= fixup.kind + objs.FX_SELFRELATIVE;
 END;
 
END AddTextSegFixup;

PROCEDURE BuildTextRels ();

VAR n, e : INT;
BEGIN
 
 e := AoutTextRelSize;

 objs.file_ptr := 20H + AoutDataSegSize + AoutTextSegSize + AoutBssSize;
                       
 FOR n := 1 TO (e DIV 8) DO 
(*  t := (n-1)*8;
  objs.file_ptr := 20H + AoutDataSegSize + AoutTextSegSize + AoutBssSize + t;*)
  AddTextSegFixup();
 END;

END BuildTextRels; 


PROCEDURE AddDataSegFixup();

 VAR
    seg: objs.Segment; 
    fixup: objs.Fixup;
    offset: CARD;

BEGIN

 seg := LedataSeg;
 objs.NewFixup(fixup);
 seg.fixups.Insert(fixup);
 fixup.kind:= objs.FX_OFFSET32;
 GetData(card32);
 ReverseCard32(card32);
 offset:= card32;
 offset:= offset + AoutTextSegSize;
 fixup.offset:= offset;
 fixup.k_target:=objs.TK_ID;
 GetData(card32);
 ReverseCard32(card32);
 fixup.name_target:= getExtName((card32 MOD 1000000H));
 IF ((card32 DIV 02H) MOD 2) = 1 THEN
   fixup.kind:= fixup.kind + objs.FX_SELFRELATIVE;
 END;
 
END AddDataSegFixup;

PROCEDURE BuildDataRels();
VAR n, e : INT;

BEGIN

 e := AoutDataRelSize;

 objs.file_ptr := 20H + AoutDataSegSize + AoutTextSegSize + AoutBssSize + AoutTextRelSize;
                       
 FOR n := 1 TO (e DIV 8) DO 
   AddDataSegFixup();
 END;

END BuildDataRels;

PROCEDURE ReadObj * (name-: ARRAY OF CHAR; dump:= FALSE: BOOLEAN);
BEGIN
  objs.ReadFile(name);
  lstr.Assign(name, file_name);
  record_len:= MAX(CARD16);
  LINNUM_FORMAT:= LINNUM_CV;
  objs.NewModule("qwerty", objs.OMFFormat);
  GetAoutHeader();
  (*io.printf("Header\n");*)
  OffStr := 0;
  IF(AoutTextSegSize > 0) THEN GetTextSeg();    END;  
  IF(AoutDataSegSize > 0) THEN GetDataSeg();    END;
  IF(AoutBssSize > 0) THEN     GetBssSeg();     END;
  IF(AoutSymbTabSize > 0) THEN BuildExtNames(); END;
  IF(AoutTextRelSize > 0) THEN BuildTextRels(); END;
  IF(AoutDataRelSize > 0) THEN BuildDataRels(); END;
END ReadObj;

(* --------------------------------------------------------------------*)
BEGIN
  Init();
END ReadAOUT.