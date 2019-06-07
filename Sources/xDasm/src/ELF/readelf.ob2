MODULE ReadELF;

(* --------------------------------------------------------------------*)
IMPORT
  SYSTEM,
  adt,
  Strings,
  lstr:= LongStrs,
  file:= H2DFile,
  io:= Printf,
  objs:= Objects,
  elf := ElfDec,
  RTS:= oberonRTS;


(* --------------------------------------------------------------------*)
CONST
  GetData  = objs.GetData;
  SkipData = objs.SkipData;


(* --------------------------------------------------------------------*)
TYPE
  INT16  = SYSTEM.INT16;
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
TYPE ELF_HEADER = RECORD
           e_ident     : ARRAY elf.EI_NIDENT OF CHAR;
           e_type      : CARD16;
           e_machine   : CARD16;
           e_version   : CARD;
           e_entry     : CARD;
           e_phoff     : CARD;
           e_shoff     : CARD;
           e_flags     : CARD;
           e_ehsize    : CARD16;
           e_phentsize : CARD16;
           e_phnum     : CARD16;
           e_shentsize : CARD16;
           e_shnum     : CARD16;
           e_shstrndx  : CARD16;
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
    idx: CARD16;
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
(*  AoutMidMag : CARD;
  AoutBssSize : CARD; 
  AoutTextSegSize : CARD;
  AoutDataSegSize : CARD; 
  AoutTextRelSize : CARD;
  AoutDataRelSize : CARD;
  AoutSymbTabSize : CARD;  *)

  Header  : ELF_HEADER;
  SecName : ARRAY 256 OF CHAR;

(*  AoutStrnTabSize : CARD;  *)
(*  AoutEntryOffset : CARD;  *)
  OffStr : CARD;
  FPC    : CARD;
  STCntr : CARD;

  LINNUM_FORMAT: INT;
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
(*  card32: CARD; *)

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
PROCEDURE Error (fmt-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN
  io.printf(fmt, x);
  HALT;
END Error;
PROCEDURE Warning (fmt-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN
  io.printf(fmt, x);
END Warning; 

(* --------------------------------------------------------------------*)
PROCEDURE getSeg(SecOff : CARD; idx: INT): objs.Segment;
VAR e: adt.Element;
    t: CARD;
BEGIN
  Segs.FindFirst(e);
  t := e(objs.Segment).overlay;
  WHILE t # SecOff DO
    t := e(objs.Segment).overlay;
    Segs.FindNext(e);
  END;
  RETURN e(objs.Segment);

END getSeg;
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
PROCEDURE FindExtName(SecOff : CARD; idx: INT): objs.HashCode;
VAR e: adt.Element;
BEGIN
  ExtNames.FindFirst(e);
  WHILE (e # NIL) AND (e(ExtName).length # SecOff) OR 
        (e(ExtName).idx # idx) DO
    ExtNames.FindNext(e);
  END;
  IF (e # NIL) THEN RETURN e(ExtName).name;
  ELSE RETURN FindExtName(0, 0);
  END;
END FindExtName;

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
PROCEDURE CheckElfMag();
BEGIN
 IF (* (Header.e_ident[0] # 07FH) OR *)    
    (Header.e_ident[1] # 'E') OR
    (Header.e_ident[2] # 'L') OR
    (Header.e_ident[3] # 'F') THEN Error("It is not an ELF object file");
 END;
END CheckElfMag; 
(* --------------------------------------------------------------------*)
PROCEDURE GetElfHeader();
BEGIN

(* GetData(Header.e_ident[0]);        
 GetData(Header.e_ident[1]);        
 GetData(Header.e_ident[2]);        
 GetData(Header.e_ident[3]);        
 CheckElfMag;
 GetData(Header.e_ident[elf.EI_CLASS]);  
 GetData(Header.e_ident[elf.EI_DATA]);   
 GetData(Header.e_ident[elf.EI_VERSION]); 
 GetData(Header.e_type);             
 GetData(Header.e_machine);          
 GetData(Header.e_version);          
 GetData(Header.e_entry);            
 GetData(Header.e_phoff);            
 GetData(Header.e_shoff);   
 GetData(Header.e_flags);            
 GetData(Header.e_ehsize);          
 GetData(Header.e_phentsize);       
 GetData(Header.e_phnum);           
 GetData(Header.e_shentsize);       
 GetData(Header.e_shnum);           
 GetData(Header.e_shstrndx);   *)
 GetData(Header);
 CheckElfMag;   


 record_pos:= objs.file_ptr;
END GetElfHeader;

(* --------------------------------------------------------------------*)
PROCEDURE Init();
BEGIN
  WasLocals:= FALSE;
  WasCOMDEFs:= FALSE;
  WasLCOMDEFs:= FALSE;
  LedataSeg:= NIL;
  LastOffset:= 0;

  adt.NewList(ExpDefs);
  adt.NewList(ExtNames);
  adt.NewList(Segs);
  adt.NewList(PubNames);
  adt.NewList(Groups);
  adt.NewList(LNames);
  adt.NewList(ExpDefs);

  RTS.Collect();
END Init;



(* --------------------------------------------------------------------*)

PROCEDURE IsSectionFixup(Offset : CARD; idx : CARD): BOOLEAN;
VAR e: adt.Element;
BEGIN
  ExtNames.FindFirst(e);
  WHILE  (e # NIL) AND (e(ExtName).length # Offset) OR 
         (e(ExtName).idx # idx)  DO
    ExtNames.FindNext(e);
  END;
 IF (e = NIL) OR (idx = 0) THEN RETURN FALSE; 
 END;
 RETURN (e(ExtName).type = elf.STT_FUNC) ;
END IsSectionFixup;

(* --------------------------------------------------------------------*)

PROCEDURE GetSectionHeader(SHIdx : CARD; VAR SH : elf.SECTION_HEADER);
VAR PosInFile : CARD;
    Cntr : CARD;
BEGIN
 PosInFile := objs.file_ptr;
 objs.file_ptr := Header.e_shoff;
 FPC := objs.file_ptr;
 Cntr := 0;
 WHILE (Cntr <= SHIdx) DO
   GetData(SH);
   Cntr := Cntr + 1;
 END;
 objs.file_ptr := PosInFile;
 FPC := objs.file_ptr;
END GetSectionHeader;
(* --------------------------------------------------------------------*)
PROCEDURE GetSecName(SH : elf.SECTION_HEADER);
VAR StrTabH : elf.SECTION_HEADER;
    PosInFile : CARD;
BEGIN
 PosInFile := objs.file_ptr;
 FPC := objs.file_ptr;
 GetSectionHeader(Header.e_shstrndx, StrTabH);
 objs.file_ptr := StrTabH.sh_offset + SH.sh_name;
 GetName(SecName);
 objs.file_ptr := PosInFile;
 FPC := objs.file_ptr;
END GetSecName;
(* --------------------------------------------------------------------*)
PROCEDURE AddDataSeg(SH : elf.SECTION_HEADER; SHIdx : CARD);

VAR 
    length: CARD;
    class : objs.HashCode;
    s     : objs.Segment;
    p     : objs.RawData;
    PosInFile : CARD;

BEGIN
  PosInFile := objs.file_ptr;
  length:= SH.sh_size; 
(*  card16:= GetIndex(); *)
  class := objs.DATA;
  objs.NewSegment(s, SHIdx, class, SH.sh_offset, length);
  Segs.Insert(s); objs.CurrentModule.segs.Insert(s);
  LedataSeg:= s;
  objs.file_ptr := SH.sh_offset;
  IF (length > 0) THEN
   NEW(p, length);
   GetData(p^);
   SYSTEM.MOVE(SYSTEM.ADR(p^), SYSTEM.ADR(s.text[0]), length);
  END;
  s.attributes := 3;
  s.alignment := 16;
  objs.file_ptr := PosInFile;

END AddDataSeg;
(* --------------------------------------------------------------------*)
PROCEDURE AddTextSeg(SH : elf.SECTION_HEADER; SHIdx : CARD);

VAR 
    length: CARD;
    class : objs.HashCode;
    s     : objs.Segment;
    p     : objs.RawData;
    PosInFile : CARD;

BEGIN
  PosInFile := objs.file_ptr;
  length:= SH.sh_size; 
  (*card16:= GetIndex();*)
  class := objs.CODE;
  objs.NewSegment(s, SHIdx, class, SH.sh_offset, length);
  Segs.Insert(s); objs.CurrentModule.segs.Insert(s);
  LedataSeg:= s;
  objs.file_ptr := SH.sh_offset;
  IF (length > 0) THEN
   NEW(p, length);
   GetData(p^);
   SYSTEM.MOVE(SYSTEM.ADR(p^), SYSTEM.ADR(s.text[0]), length);
  END;
  s.attributes := 3;
  s.alignment := 16;
  objs.file_ptr := PosInFile;

END AddTextSeg;

(* --------------------------------------------------------------------*)

PROCEDURE AddBssSeg(SH : elf.SECTION_HEADER; SHIdx : CARD);

VAR 
    length: CARD;
    class : objs.HashCode;
    s     : objs.Segment;
    PosInFile : CARD;

BEGIN
  PosInFile := objs.file_ptr;
  length:= SH.sh_size; 
  (*card16:= GetIndex();*)
  class := objs.BSS;
  objs.NewSegment(s, SHIdx, class, SH.sh_offset, length);
  s.attributes := 3;
  s.alignment := 16;
  
  Segs.Insert(s); objs.CurrentModule.segs.Insert(s);
  
  objs.file_ptr := PosInFile;

END AddBssSeg;


(* --------------------------------------------------------------------*)
(*PROCEDURE CompStr (S1, S2 : ARRAY 256 OF CHAR);
VAR Cntr : INT;
BEGIN
 WHILE ((S1[Cntr] = S2[Cntr]) AND (S1[Cntr] # ' ')) DO
   Cntr := Cntr + 1;
 END;
 IF (S1[Cntr] = ' ') RETURN 1;
 ELSE RETURN 0;
 END;
END;*)

(* --------------------------------------------------------------------*)
PROCEDURE DetectRelocatableSections();

VAR CurrSHIdx : CARD;


 PROCEDURE CheckSecHdr();
 VAR ShdrData : elf.SECTION_HEADER;
(*     SecName  : lstr.String; *)
 BEGIN
  GetData(ShdrData);
  CASE ShdrData.sh_type OF
  | elf.SHT_PROGBITS :

    GetSecName(ShdrData);
     IF (SecName[1] = 'd') OR (SecName[1] = 'r') OR (SecName[1] = 's') THEN AddDataSeg(ShdrData, CurrSHIdx);
     ELSE 
      IF (SecName[1] = 't') THEN AddTextSeg(ShdrData, CurrSHIdx);
(*      ELSE 
       IF (SecName[1] = 's') THEN AddData(ShdrData,CurrSHIdx); *)
      END
     END;
  | elf.SHT_NOBITS :
    GetSecName(ShdrData);
     IF (SecName[1] = 'b') THEN AddBssSeg(ShdrData, CurrSHIdx);
     END;
  ELSE
  END;

 END CheckSecHdr;

BEGIN

 CurrSHIdx := 0;
 objs.file_ptr := Header.e_shoff;
 FPC := objs.file_ptr;
 WHILE (objs.file_ptr + 1) < objs.Length(objs.file_data^) DO
    CheckSecHdr();
    CurrSHIdx := CurrSHIdx + 1;
 END;

END DetectRelocatableSections;
(* --------------------------------------------------------------------*)

PROCEDURE FindSection(SHIdx : INT) : objs.Segment;
 VAR e: adt.Element;
     s: objs.Segment;
 BEGIN
  Segs.FindFirst(e);
  s := e(objs.Segment);
  WHILE (SHIdx # s.name) AND (e # NIL) DO 
    Segs.FindNext(e);
    IF (e # NIL) THEN
    s := e(objs.Segment); 
    END;
  END;
  IF (e # NIL) THEN RETURN e(objs.Segment); 
  ELSE RETURN NIL; END;
END FindSection;
(* --------------------------------------------------------------------*)
PROCEDURE GetSection(Offset : CARD) : objs.Segment;
 VAR e: adt.Element;
     s: objs.Segment;
     t: CARD;
 BEGIN
  Segs.FindFirst(e);
  s := e(objs.Segment);
  t := s.overlay;
  WHILE (Offset # t) AND (e # NIL) DO 
    Segs.FindNext(e);
    IF (e # NIL) THEN
    s := e(objs.Segment); 
    t := s.overlay;
    END;
  END;
  IF (e # NIL) THEN RETURN e(objs.Segment); 
  ELSE RETURN NIL; END;
END GetSection;


(* --------------------------------------------------------------------*)
PROCEDURE GetSectionOffset(SecHdrIdx : CARD) : CARD;
VAR PosInFile : CARD;
    SH        : elf.SECTION_HEADER;  
    CurrSHIdx : CARD;
BEGIN

 PosInFile := objs.file_ptr;
 objs.file_ptr := Header.e_shoff;
 CurrSHIdx := 0;
 WHILE CurrSHIdx < SecHdrIdx + 1 DO
    GetData(SH);
    CurrSHIdx := CurrSHIdx + 1;
 END;
 objs.file_ptr := PosInFile;
 RETURN SH.sh_offset;
END GetSectionOffset;

(* --------------------------------------------------------------------*)
PROCEDURE FindTargetSecIdx(Idx : CARD) : CARD;
VAR e: adt.Element;
BEGIN

  ExtNames.FindFirst(e);
  WHILE (e(ExtName).idx # Idx)  DO
    ExtNames.FindNext(e);
  END;
 RETURN e(ExtName).length;

END FindTargetSecIdx;

(* --------------------------------------------------------------------*)
PROCEDURE ScanRelTable(SH : elf.SECTION_HEADER);

VAR PosInFile : CARD;
    RTEntr    : elf.REL;
    Size      : CARD;
    SecApl    : CARD;
    SymApl    : CARD;
    Cntr      : CARD;


 PROCEDURE DetectSymbol();

 VAR s : objs.Segment;
 
 BEGIN
  SymApl := SH.sh_link;
  SecApl := SH.sh_info;
  LedataSeg := FindSection(SecApl);

 END DetectSymbol;


 PROCEDURE ProcessRel();

 VAR STIdx : CARD;
     seg: objs.Segment;
     str: lstr.String;
     first: CARD8;
     fixup: objs.Fixup;
     locat: CARD8;
     fix_data: CARD8;
     offset: CARD;
     n: INT;
     method: CARD8;

 BEGIN
   GetData(RTEntr);
   STIdx := RTEntr.r_info DIV 100H;
   DetectSymbol();
   seg := LedataSeg;
   IF seg # NIL THEN
    objs.NewFixup(fixup);
    seg.fixups.Insert(fixup);
    fixup.kind:= objs.FX_OFFSET32; (* + objs.FX_SELFRELATIVE;*)

    IF (seg # NIL) & (seg.class = objs.BSS) THEN
     objs.IndexToStr(seg.name, str);
     Error("File %s: fixup in BSS segment %s\n", objs.CurrentModule.file_name^, str^);
    END;

    IF IsSectionFixup(SymApl, STIdx) THEN
     fixup.offset := RTEntr.r_offset;
     fixup.k_target:=objs.TK_SEG;
     fixup.seg_target:= seg;(*FindSection(FindTargetSecIdx(STIdx));*)


    ELSE

     fixup.offset := RTEntr.r_offset;
     fixup.k_target:=objs.TK_ID;
     fixup.name_target:= FindExtName(SymApl, STIdx);

    END;
   END;                                                   

 END ProcessRel;
 
BEGIN
 PosInFile := objs.file_ptr;
 objs.file_ptr := SH.sh_offset;
 Size := SH.sh_size;
 Size := Size + SH.sh_offset;
 WHILE (objs.file_ptr + 1) < (Size) DO
    ProcessRel();
 END;
 objs.file_ptr := PosInFile;
END ScanRelTable;

(* --------------------------------------------------------------------*)

PROCEDURE ScanRelaTable(SH : elf.SECTION_HEADER);

VAR PosInFile : CARD;
    RTEntr    : elf.RELA;
    SecApl    : CARD;
    SymApl    : CARD;
    Cntr      : CARD;
    Size      : CARD;

 PROCEDURE DetectSymbol();

 VAR s : objs.Segment;
 
 BEGIN
  SymApl := SH.sh_link;
  SecApl := SH.sh_info;
  LedataSeg := FindSection(SecApl);

 END DetectSymbol;


 PROCEDURE ProcessRel();

 VAR STIdx : CARD;
     seg: objs.Segment;
     str: lstr.String;
     first: CARD8;
     fixup: objs.Fixup;
     locat: CARD8;
     fix_data: CARD8;
     offset: CARD;
     n: INT;
     method: CARD8;


 BEGIN

   GetData(RTEntr);
   STIdx := RTEntr.r_info DIV 100H;
   DetectSymbol();
   seg := LedataSeg;
   IF seg # NIL THEN
    objs.NewFixup(fixup);
    seg.fixups.Insert(fixup);
    fixup.kind:= objs.FX_OFFSET32;

    IF (seg # NIL) & (seg.class = objs.BSS) THEN
     objs.IndexToStr(seg.name, str);
     Error("File %s: fixup in BSS segment %s\n", objs.CurrentModule.file_name^, str^);
    END;

    IF IsSectionFixup(SymApl, STIdx) THEN
     fixup.offset := RTEntr.r_offset;
     fixup.k_target:=objs.TK_SEG;
     fixup.seg_target:= GetSection(FindExtName(SymApl, STIdx));

    ELSE

     fixup.offset := RTEntr.r_offset;
     fixup.k_target:=objs.TK_ID;
     fixup.name_target:= FindExtName(SymApl, STIdx);

    END;
   END;                                                   
 END ProcessRel;
 
BEGIN
 PosInFile := objs.file_ptr;
 objs.file_ptr := SH.sh_offset;
 Size := SH.sh_size;
 Size := Size + SH.sh_offset;
 WHILE (objs.file_ptr + 1) < (Size) DO
    ProcessRel();
 END;
 objs.file_ptr := PosInFile;
END ScanRelaTable;

(* --------------------------------------------------------------------*)

PROCEDURE ArrangeRelocations();

 PROCEDURE CheckSecHdr();
 VAR ShdrData : elf.SECTION_HEADER;
     SecName  : lstr.String;
 BEGIN
  GetData(ShdrData);
  CASE ShdrData.sh_type OF
  | elf.SHT_REL :
    ScanRelTable(ShdrData);
  | elf.SHT_RELA :
    ScanRelaTable(ShdrData);
  ELSE
  END;
 END CheckSecHdr;

BEGIN
 objs.file_ptr := Header.e_shoff;
 WHILE (objs.file_ptr + 1) < objs.Length(objs.file_data^) DO
    CheckSecHdr();
 END;
END ArrangeRelocations;

(* --------------------------------------------------------------------*)
PROCEDURE BuildExtNames();
VAR SCntr : CARD16;
    Size      : CARD;

 PROCEDURE GetExtNameIdent(SH : elf.SECTION_HEADER; StrTIdx : CARD; S : elf.SYMBOL; VAR t : CARD16): objs.HashCode;
 VAR PosInFile : CARD;
     SecHdr    : elf.SECTION_HEADER;
     Cntr      : INT;
     name      : ARRAY 256 OF CHAR;

 BEGIN
  PosInFile := objs.file_ptr;
  objs.file_ptr := Header.e_shoff;
  Cntr := 0;
  GetData(SecHdr);
  GetSecName(SecHdr);
  WHILE (SecName[1] # 's') OR (SecName[2] # 't') DO
   GetData(SecHdr);
   GetSecName(SecHdr);
  END;
  
  IF (StrTIdx = 0) AND (t # 1) THEN StrTIdx := 1; END;
  objs.file_ptr := SecHdr.sh_offset + StrTIdx; 
  GetName(name);
  objs.file_ptr := PosInFile;
(*  IF (name = '') AND (SCntr > 0) THEN 
    t := elf.STT_SECTION;
    RETURN S.st_value;
  ELSE 
  *)
(* IF SCntr > 0 THEN *)
  RETURN objs.StrToIndex(name);
(* END;
 ELSE RETURN *)
 END GetExtNameIdent;


 PROCEDURE AddExtName(SH : elf.SECTION_HEADER);
 VAR en : ExtName;
     STEntr : elf.SYMBOL;
 BEGIN
  GetData(STEntr);
  NEW(en);
  en.type := STEntr.st_info MOD 10H;
  en.name:= GetExtNameIdent(SH, STEntr.st_name, STEntr, en.type);
  en.kind:= objs.K_EXTDEF;
  en.length := STCntr; (*STEntr.st_shndx;*)
  en.idx := SCntr;
  ExtNames.Insert(en);

  SCntr := SCntr + 1;
 END AddExtName;


 PROCEDURE ProcessSymTable(SH : elf.SECTION_HEADER);
 VAR STEntr : elf.SYMBOL;
     PosInFile : CARD;
 BEGIN
  PosInFile := objs.file_ptr;
  objs.file_ptr := SH.sh_offset;
  SCntr := 0; 
  Size := SH.sh_size; 
  Size := Size + SH.sh_offset;
  WHILE (objs.file_ptr + 1) < Size DO
    AddExtName(SH);
  END;
  objs.file_ptr := PosInFile;
 END ProcessSymTable;


 PROCEDURE CheckForSymTableSecHdr();

 VAR ShdrData : elf.SECTION_HEADER;
     SecName  : lstr.String;
 BEGIN
  GetData(ShdrData);
  IF (ShdrData.sh_type = elf.SHT_SYMTAB) THEN
    ProcessSymTable(ShdrData);  
  END;
 END CheckForSymTableSecHdr;

BEGIN
 STCntr := 0;
 objs.file_ptr := Header.e_shoff;
 WHILE (objs.file_ptr + 1) < objs.Length(objs.file_data^) DO
    CheckForSymTableSecHdr();
    STCntr := STCntr + 1;
 END;

END BuildExtNames;
(* --------------------------------------------------------------------*)
PROCEDURE ReadObj * (name-: ARRAY OF CHAR; dump:= FALSE: BOOLEAN);
BEGIN

  objs.ReadFile(name);
  lstr.Assign(name, file_name);
  record_len:= MAX(CARD16);
  LINNUM_FORMAT:= LINNUM_CV;
  objs.NewModule("qwerty", objs.OMFFormat);
  GetElfHeader();
  OffStr := 0;
(*  ScanSecHeaderTable(); *)
  DetectRelocatableSections();
  BuildExtNames();
  ArrangeRelocations();

END ReadObj;

(* --------------------------------------------------------------------*)
BEGIN
  Init();
END ReadELF.