MODULE ReadXOMF;

(* --------------------------------------------------------------------*)
IMPORT
  SYSTEM,
  adt,
  Strings,
  Printf,
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
  THEADR  = 0DDH;  (* Translator module header record *)
  LHEADR  = 082H;  (* Library module header record *)
  COMENT  = 088H;  (* Comment record *)
  EXTDEF  = 08cH;  (* External names definition record *)
  TYPDEF  = 08eH;  (* Type definition record *)
  PUBDEF  = 090H;  (* Public names definition record *)
  LINNUM  = 094H;  (* Line numbers record *)
  LNAMES  = 096H;  (* List of names record *)
  LEDATA  = 0a0H;  (* Logical enumerated data record *)
  LIDATA  = 0a2H;  (* Logical iterated data record *)
  BAKPAT  = 0b2H;  (* Bacpatch record *)
  LINSYM  = 0c4H;  (* Symbol line numbers record *)
  ALIAS   = 0c6H;  (* Alias definition record *)
  NBKPAT  = 0c8H;  (* Named backpatch record *)
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

  XOMF_HEADER      = 0D0H; (* XOMF Header              *)
  XOMF_SEGDEF      = 0D1H; (* XOMF Segment definition  *)
  XOMF_OBJEND      = 0D2H; (* XOMF End of object file  *)
  XOMF_JSTRINGDESC = 0D7H; (* XOMF J String descriptor *)
  XOMF_EXPDEF      = 0D9H; (* XOMF J Export            *)
  XOMF_IMPDEF      = 0DBH; (* XOMF J Import            *)
  XOMF_FIXUP       = 0DFH; (* XOMF Fixup record        *)
  XOMF_RAWDATA     = 0DEH; (* XOMF Raw Data record     *)

  (* XOMF Fixup Types *)

  XOMF_FX_ADDR32    = 1;
  XOMF_FX_OFFS32    = 2;
  XOMF_FX_FAR48     = 3;
  XOMF_FX_TDINDEX16 = 4;
  XOMF_FX_JSTR32    = 5;
  XOMF_FX_BYTESTR32 = 6;
  XOMF_FX_CONSTADDR32 = 7;
  XOMF_FX_CONSTADDR32_EXTRA_THUNK = 8;
  XOMF_FX_TDINDEX32 = 9;

  (* XOMF Fixup Target Kinds *)

  XOMF_TK_SEG     = 1;
  XOMF_TK_ID      = 2;
  XOMF_TK_RAWDATA = 3;

  (* XOMF Segment Class Codes *)

  SEGCLASS_CODE         =  1;
  SEGCLASS_DATA         =  2;
  SEGCLASS_BSS          =  3;
  SEGCLASS_RODATA       =  4;
  SEGCLASS_DEBUG        =  5;
  SEGCLASS_NULLCHECKS   =  6;
  SEGCLASS_STACKTRACE   =  7;

  (* XOMF Segment Alignment *)

  SEGALIGN_1             = 0;
  SEGALIGN_2             = 1;
  SEGALIGN_4             = 2;
  SEGALIGN_8             = 3;
  SEGALIGN_16            = 4;
  SEGALIGN_4096          = 12;

  (* XOMF Raw Data Kinds *)

  DK_BYTESTR             = 1;
  DK_UNICODESTR          = 2;

  (* Public Symbol Types (PUBDEF) *)

  XOMF_TYPE_DATA         = 1;
  XOMF_TYPE_CODE         = 2;
  XOMF_TYPE_TD           = 5;

  XOMF_SIGNATURE      = 0464D4F58H;  (* "XOMF" *)
  XOMF_FORMAT_VERSION = 13;

(*---------------------------------------------------------------------*)

(* Используется при замыкании внутримодульных ссылок *)

TYPE
  Local = POINTER TO LocalDesc;
  LocalDesc = RECORD (adt.ElementDesc)
    seg   : objs.Segment;
    group : objs.Group;
    length: CARD;         (* For (L)COMDEF only *)
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
    kind  : SET;
  END;


(* --------------------------------------------------------------------*)

TYPE
  RawData = POINTER TO RawDataDesc;
  RawDataDesc = RECORD (adt.ElementDesc)
    data :objs.RawData;
    id   :objs.HashCode;
  END;

(* --------------------------------------------------------------------*)
CONST
  LINNUM_CV   = 0;
  LINNUM_HLL4 = 1;

(* --------------------------------------------------------------------*)
VAR
  record_type: CARD8;
  record_len : CARD;
  record_pos : CARD;

  LINNUM_FORMAT: INT;

  libhdr_rec: objs.OMFRecord;
  file_name : lstr.String;

  Segs, PubNames, Groups, LNames, ExpDefs, ExtNames, RawDatas: adt.List;

  LedataSeg: objs.Segment;
  LastOffset: CARD;


  card8 : CARD8;
  card16: CARD16;
  card32: CARD;

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
PROCEDURE getLName(idx: INT; VAR name: objs.HashCode);
VAR e: adt.Element;
BEGIN
  LNames.FindFirst(e);
  WHILE idx > 0 DO
    LNames.FindNext(e);
    DEC(idx);
  END;
  name:= e(LName).name;
END getLName;


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
PROCEDURE getSeg(idx: INT): objs.Segment;
VAR e: adt.Element;
BEGIN
  Segs.FindFirst(e);
  WHILE idx > 0 DO
    Segs.FindNext(e);
    DEC(idx);
  END;
  RETURN e(objs.Segment);
END getSeg;


PROCEDURE getByteStringRawData (idx: INT): objs.HashCode;
VAR
  e: adt.Element;
  r: RawData;
  s: POINTER TO ARRAY OF CHAR;
BEGIN
  RawDatas.FindFirst(e);
  WHILE idx > 0 DO
    RawDatas.FindNext(e);
    DEC(idx);
  END;
  r := e(RawData);

  ASSERT (r.data # NIL);
  IF (r.id = -1) THEN
    NEW (s, LEN(r.data^) + 16);
    Printf.sprintf (s^, '"%s"', r.data^);
    r.id := objs.StrToIndex (s^);
  END;
  RETURN r.id;
END getByteStringRawData;

(* --------------------------------------------------------------------*)
PROCEDURE RemainedDataLen(): CARD;
BEGIN
  RETURN record_pos + record_len - 1 - objs.file_ptr;
END RemainedDataLen;

(* --------------------------------------------------------------------*)
PROCEDURE CanRead(): BOOLEAN;
BEGIN
  RETURN (record_pos + record_len - 1) > objs.file_ptr;
END CanRead;

(* --------------------------------------------------------------------*)
PROCEDURE GetName(VAR name: ARRAY OF CHAR);
BEGIN
  GetData(card8);
  IF card8 # 0 THEN GetData(name, card8) END;
  name[card8]:= 0X;
END GetName;

PROCEDURE GetZName(VAR name: ARRAY OF CHAR);
VAR i :CARD;
BEGIN           
  i := 0;
  LOOP
    GetData(card8);
    name[i] := CHR (card8);
    IF card8 = 0 THEN EXIT; END;
    i := i + 1;
  END;
END GetZName;

PROCEDURE GetIdent(): objs.HashCode;
VAR name: ARRAY 256 OF CHAR;
BEGIN
  GetName(name);
  RETURN objs.StrToIndex(name);
END GetIdent;


PROCEDURE GetZStringIdent(): objs.HashCode;
VAR name: ARRAY 1024 OF CHAR;
BEGIN
  GetZName(name);
  RETURN objs.StrToIndex(name);
END GetZStringIdent;


PROCEDURE GetLongName(VAR name: ARRAY OF CHAR);
BEGIN
  GetData(card16);
  IF card16 # 0 THEN GetData(name, card16) END;
  name[card16]:= 0X;
END GetLongName;

PROCEDURE GetLongIdent(): objs.HashCode;
VAR name: ARRAY 1024 OF CHAR;
BEGIN
  GetLongName(name);
  RETURN objs.StrToIndex(name);
END GetLongIdent;

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
PROCEDURE GetWordOrDword(): CARD;
BEGIN
  IF (record_type MOD 2) = 0 THEN
    GetData(card16);
    card32:= card16;
  ELSE
    GetData(card32);
  END;
  RETURN card32;
END GetWordOrDword;

(* --------------------------------------------------------------------*)
PROCEDURE CheckExt(VAR name: ARRAY OF CHAR; ext: ARRAY OF CHAR);
VAR i: INT;
BEGIN
  i:= 0;
  WHILE (name[i] # 0X) & (name[i] # '.') DO INC(i) END;
  IF name[i] = 0X THEN Strings.Append(ext, name) END;
END CheckExt;

(* --------------------------------------------------------------------*)
PROCEDURE coment();
VAR ord_flag: CARD8;
    ext_name, int_name, module: objs.HashCode;
    name: ARRAY 1024 OF CHAR;
    ordinal: CARD16;
    kind: SET;
    n: objs.Name;
    in: objs.ImpName;
    s: lstr.String;
    ed: ExpDef;
BEGIN
  GetData(card8); (* Comment type  *)
  GetData(card8); (* Comment class *)
  CASE card8 OF
    |OMFEXT:
       GetData(card8); (* Subtype *)
       CASE card8 OF
         |IMPDEF:
            GetData(ord_flag);

            int_name:= GetLongIdent();

            GetName(name); CheckExt(name, '.dll');
            Strings.Capitalize(name);
            module:= objs.StrToIndex(name);

            ext_name:= 0; ordinal:= 0;
            IF ord_flag = 0 THEN
              (* Import by name *)
              GetLongName(name);
              IF name[0] = 0X THEN
                ext_name:= int_name;
              ELSE
                ext_name:= objs.StrToIndex(name);
              END;
              kind:= objs.K_IMPORT;
            ELSE
              (* Import by ordinal *)
              GetData(ordinal);
              kind:= objs.K_IMPORT + objs.K_BY_ORDINAL;
            END;

            n:= objs.FindName(int_name);
            IF n # NIL THEN
              IF ~((n IS objs.ImpName)              &
                   (n(objs.ImpName).kind        = kind)     &
                   (n(objs.ImpName).idx = int_name) &
                   (n(objs.ImpName).extname = ext_name) &
                   (n(objs.ImpName).ordinal     = ordinal)
                  )
              THEN
                objs.IndexToStr(int_name, s);
                IF objs.CurrentModule.lib_name[0] = 0X THEN
                  Error("Name %s is declared at least twice\n", s^);
                ELSE
                  Warning("Name %s is declared at least twice\n", s^);
                END;
              END;
            ELSE
              NEW(in);
              in.idx:= int_name;
              in.extname:= ext_name;
              in.ordinal:= ordinal;
              in.kind:= kind;
              objs.Names.Insert(in);
            END;
         |EXPDEF:
            NEW(ed);
            GetData(card8); ord_flag:= card8 DIV 80H;
            ed.ext_name:= GetLongIdent();
            GetLongName(name);
            ed.int_name_present:= name[0] # 0X;
            IF ed.int_name_present THEN
              ed.int_name:= objs.StrToIndex(name);
            ELSE
              ed.int_name:= ed.ext_name;
            END;
            IF ord_flag # 0 THEN GetData(ed.ordinal) END;
            ExpDefs.Insert(ed);
       ELSE
         Error("Unknown comment subtype\n");
       END;
    |NOMFEX:
       GetData(card8); (* Subtype *)
       IF card8 = 4 THEN LINNUM_FORMAT:= LINNUM_HLL4 END;
  ELSE
  END;
END coment;

(* --------------------------------------------------------------------*)
PROCEDURE extdef(kind: SET);
VAR en: ExtName;
BEGIN
  WHILE CanRead() DO
    NEW(en);
    en.name:= GetLongIdent();
    en.type:= GetIndex();
    en.kind:= kind;
    en.length:= 0;
    ExtNames.Insert(en);
  END;
END extdef;

(* --------------------------------------------------------------------*)
PROCEDURE lnames(kind: SET);
VAR ln: LName;
BEGIN
  WHILE CanRead() DO
    NEW(ln);
    ln.name:= GetIdent();
    ln.kind:= kind;
    LNames.Insert(ln);
  END;
END lnames;

PROCEDURE pubdef(kind: SET);
VAR
  seg  : objs.Segment;
  pn   : PubName;
  p    : objs.Public;
  type : CARD8;
BEGIN
  card16:= GetIndex();
  ASSERT (card16 # 0);
  seg:= getSeg(card16 - 1);

  WHILE CanRead() DO
    NEW(pn);
    pn.name  := GetLongIdent();
    GetData (card32);
    pn.offset:= card32;
    GetData (type);
    IF (type = XOMF_TYPE_TD) THEN
      GetData (card32);  -- skip "symbol name hash", defined for type descriptors
    END;
    pn.seg   := seg;
    pn.group := NIL;
    pn.kind  := kind;
    PubNames.Insert(pn);
    objs.NewPublic (p, pn.name, pn.seg, pn.offset);
  END;
END pubdef;

(* --------------------------------------------------------------------*)
PROCEDURE xomf_segdef();
VAR
  segname    :objs.HashCode;
  grpname    :objs.HashCode;
  class      :objs.HashCode;
  class_code :CARD8;
  length     :CARD;
  align_code :CARD8;
  s          :objs.Segment;
  snm, gnm, cnm :lstr.String;
BEGIN
  segname := GetZStringIdent ();

  card16:= GetIndex();  -- segment group name
  ASSERT (card16 # 0);
  getLName (card16 - 1, grpname);

  GetData (class_code);
  GetData (length);
  GetData (align_code);

  CASE class_code OF
    | SEGCLASS_CODE:       class := objs.CODE;
    | SEGCLASS_DATA:       class := objs.DATA;
    | SEGCLASS_BSS:        class := objs.BSS;
    | SEGCLASS_RODATA:     class := objs.Const;
    | SEGCLASS_DEBUG:      class := objs.StrToIndex ("DEBUG");
    | SEGCLASS_NULLCHECKS: class := objs.StrToIndex ("NULLCHECKS");
    | SEGCLASS_STACKTRACE: class := objs.StrToIndex ("STACKTRACE");
  ELSE
    ASSERT (FALSE, class_code);
  END;
  objs.IndexToStr(segname, snm);
  objs.IndexToStr(grpname, gnm);
  objs.IndexToStr(class,   cnm);

  objs.NewSegment(s, grpname, class, 0, length);
  Segs.Insert(s); objs.CurrentModule.segs.Insert(s);

  CASE align_code OF
    | SEGALIGN_1:    s.alignment:= 1;
    | SEGALIGN_2:    s.alignment:= 2;
    | SEGALIGN_4:    s.alignment:= 4;
    | SEGALIGN_8:    s.alignment:= 8;
    | SEGALIGN_16:   s.alignment:= 16;
    | SEGALIGN_4096: s.alignment:= 4096;
  ELSE
    ASSERT (FALSE, align_code);
  END;
  s.attributes := 1;
END xomf_segdef;

(* --------------------------------------------------------------------*)

PROCEDURE xomf_rawdata ();
VAR
  dataKind     :CARD8;
  elementCount :CARD;
  p            :objs.RawData;
  r            :RawData;
BEGIN
  GetData (dataKind);
  GetData (elementCount);

  NEW (r);
  r.data := NIL;
  r.id   := -1;

  CASE dataKind OF
    |DK_BYTESTR:
       NEW(p, elementCount);
       GetData(p^);
       r.data := p;

    |DK_UNICODESTR:
  ELSE
    ASSERT (FALSE, dataKind);
  END;

  RawDatas.Insert(r);
END xomf_rawdata;

(* --------------------------------------------------------------------*)
PROCEDURE ledata();
VAR s: objs.Segment;
    offset: CARD;
    len: CARD;
    str: lstr.String;
    p: objs.RawData;

BEGIN
  s:= getSeg(GetIndex()-1); LedataSeg:= s;
  offset:= GetWordOrDword(); LastOffset:= offset;
  len:= RemainedDataLen();
  IF (offset + len) > s.length THEN
    objs.IndexToStr(s.name, str);
    Error("File %s: too much data for segment %s\n", objs.CurrentModule.file_name^, str^);
  END;

  IF s.class = objs.BSS THEN
    WHILE len # 0 DO
      GetData(card8);
      IF card8 # 0 THEN
        objs.IndexToStr(s.name, str);
        Error("File %s: cannot inititialize BSS segment %s\n", objs.CurrentModule.file_name^, str^);
      END;
      len:= len - 1;
    END;
  ELSE
    NEW(p, len);
    GetData(p^);
    SYSTEM.MOVE(SYSTEM.ADR(p^), SYSTEM.ADR(s.text[offset]), len);
  END;
END ledata;

(* --------------------------------------------------------------------*)
PROCEDURE lidata();
VAR s: objs.Segment;
    offset: CARD;
    str: lstr.String;
    p: objs.RawData;

  PROCEDURE block(VAR p: objs.RawData);
  TYPE data = POINTER TO dataDesc;
       dataDesc = RECORD (adt.ElementDesc)
         p: objs.RawData;
       END;
  VAR rep_count: CARD;
      block_count: CARD16;
      d: data;
      l: adt.List;
      e: adt.Element;
      len: CARD;
      offset: CARD;

  BEGIN
    rep_count:= GetWordOrDword();
    GetData(block_count);
    adt.NewList(l); len:= 0;
    IF block_count = 0 THEN
      GetData(len);
      NEW(d);
      NEW(d.p, len);
      GetData(d.p^);
      l.Insert(d);
    ELSE
      WHILE block_count > 0 DO
        NEW(d); l.Insert(d);
        block(d.p);
        len:= len + objs.Length(p^);
        block_count:= block_count - 1;
      END;
    END;

    NEW(p, len * rep_count); offset:= 0;
    WHILE rep_count > 0 DO
      l.FindFirst(e);
      WHILE e # NIL DO
        SYSTEM.MOVE(e(data).p, SYSTEM.ADR(p[offset]), objs.Length(e(data).p^));
        offset:= offset + objs.Length(e(data).p^);
        l.FindNext(e);
      END;
      rep_count:= rep_count - 1;
    END;
  END block;

BEGIN
  s:= getSeg(GetIndex() - 1);
  IF s.class = objs.BSS THEN
    objs.IndexToStr(s.name, str);
    Error("File %s: cannot inititialize BSS segment %s\n", objs.CurrentModule.file_name^, str^);
  END;
  LedataSeg:= NIL;

  offset:= GetWordOrDword();

  WHILE CanRead() DO
    block(p);
    IF (offset + objs.Length(p^)) > s.length THEN
      objs.IndexToStr(s.name, str);
      Error("File %s: too much data for segmwnt %s\n", objs.CurrentModule.file_name^, str^);
    END;
    SYSTEM.MOVE(p, SYSTEM.ADR(s.text[offset]), objs.Length(p^));
    offset:= offset + objs.Length(p^);
  END;

END lidata;

(* --------------------------------------------------------------------*)

PROCEDURE xomf_fixup();
VAR seg :objs.Segment;
    str :lstr.String;

    fixupType    :CARD8;
    offset       :CARD;
    targetKind   :CARD8;
    targetObject :CARD;

    fixup :objs.Fixup;
BEGIN
  seg:= LedataSeg;
  IF seg = NIL THEN
    Error("File %s: fixup without ledata\n", objs.CurrentModule.file_name^);
  END;
  IF (seg.class = objs.BSS) THEN
    objs.IndexToStr(seg.name, str);
    Error("File %s: fixup in BSS segment %s\n", objs.CurrentModule.file_name^, str^);
  END;
  WHILE CanRead() DO
    objs.NewFixup(fixup);
    seg.fixups.Insert(fixup);

    GetData (fixupType);
    GetData (offset);
    GetData (targetKind);
    GetData (targetObject);

    CASE fixupType OF
      |XOMF_FX_ADDR32:    fixup.kind := objs.FX_OFFSET32;
      |XOMF_FX_OFFS32:    fixup.kind := objs.FX_OFFSET32 + objs.FX_SELFRELATIVE;
      |XOMF_FX_FAR48:     fixup.kind := objs.FX_FAR16_32;
      |XOMF_FX_TDINDEX16: fixup.kind := objs.FX_TDINDEX16;
      |XOMF_FX_TDINDEX32: fixup.kind := objs.FX_TDINDEX32;
      |XOMF_FX_JSTR32:    fixup.kind := objs.FX_JAVASTRING;
      |XOMF_FX_BYTESTR32: fixup.kind := objs.FX_BYTESTRING;
      |XOMF_FX_CONSTADDR32:fixup.kind := objs.FX_OFFSET32;
      |XOMF_FX_CONSTADDR32_EXTRA_THUNK:fixup.kind := objs.FX_OFFSET32;
    ELSE
      Error("File %s: invalid fixup type %d\n", objs.CurrentModule.file_name^, fixupType);
    END;
    fixup.offset  := offset;
    fixup.k_frame := objs.FK_TARGET;
    CASE targetKind OF
      |XOMF_TK_SEG:     fixup.k_target   := objs.TK_SEG;
                        fixup.seg_target := getSeg (targetObject - 1);
      |XOMF_TK_ID:      fixup.k_target   := objs.TK_ID;
                        fixup.name_target:= getExtName (targetObject - 1);
      |XOMF_TK_RAWDATA:
         IF (fixupType = XOMF_FX_BYTESTR32) THEN
           fixup.k_target    := objs.TK_ID;
           fixup.name_target := getByteStringRawData (targetObject-1);
         ELSE
           fixup.k_target := objs.TK_UNKNOWN;
         END;
    ELSE
      Error("File %s: invalid fixup target kind %d\n", objs.CurrentModule.file_name^, targetKind);
    END;
    fixup.fx_offset:= 0;
  END;
END xomf_fixup;

(* --------------------------------------------------------------------*)
PROCEDURE objend();
BEGIN
END objend;

(* --------------------------------------------------------------------*)
PROCEDURE GetHeader();
BEGIN
  IF record_len # MAX(CARD) THEN
    SkipData(record_pos + record_len - objs.file_ptr);
  END;

  GetData(record_type);
  GetData(record_len);
  record_pos:= objs.file_ptr;
END GetHeader;

(* --------------------------------------------------------------------*)

VAR
  moduleInitialized :BOOLEAN;


PROCEDURE Init();
BEGIN
  LedataSeg:= NIL;
  LastOffset:= 0;

  adt.NewList(ExpDefs);
  adt.NewList(ExtNames);
  adt.NewList(Segs);
  adt.NewList(PubNames);
  adt.NewList(Groups);
  adt.NewList(LNames);
  adt.NewList(ExpDefs);
  adt.NewList(RawDatas);

  RTS.Collect();
END Init;

(* --------------------------------------------------------------------*)
PROCEDURE ReadRecord(dump: BOOLEAN);
VAR buf: ARRAY 2048 OF CHAR;
    rec: objs.OMFRecord;

BEGIN
  GetHeader();

  NEW(rec); rec.type:= record_type; rec.len:= record_len + 1 + 4 + 1;
  NEW(rec.data, record_len);
  rec.file_pos:= objs.file_ptr;
  GetData(rec.data^, record_len, FALSE);
  IF objs.CurrentModule # NIL THEN
    objs.CurrentModule.recs.Insert(rec);
  ELSE
    IF ((record_type # THEADR) & (record_type # LHEADR) & (record_type # LIBHDR) & (record_type # XOMF_HEADER)) THEN
      Error("Incorrect object file format\n");
    END;
  END;

  CASE record_type OF
    |XOMF_HEADER:
       GetData(card8);  -- Common Flag
       GetData(card8);  -- Module Kind
       GetZName(buf);
       IF NOT moduleInitialized THEN
         objs.NewModule(buf, objs.OMFFormat);
         moduleInitialized := TRUE;
       END;
       objs.CurrentModule.recs.Insert(rec);
       GetZName(buf);   -- Module Name
       GetZName(buf);   -- UID Name
       GetData(card32); -- Version Stamp
       GetData(card8);  -- Debug Info Format

    |THEADR, LHEADR:
       GetName(buf);
       objs.NewModule(buf, objs.OMFFormat);
       objs.CurrentModule.recs.Insert(rec)

    |XOMF_OBJEND:
       SkipData (record_pos + record_len - objs.file_ptr);
       record_len := MAX(CARD);
       Init();

    |LIBHDR:
       libhdr_rec:= rec;
       lstr.Assign(file_name^, objs.lib_name);
    |LIBEND:
       libhdr_rec:= NIL;
       SkipData(objs.Length(objs.file_data^) - objs.file_ptr);
  ELSE
    IF dump THEN
      SkipData(record_len);
    ELSE
      CASE record_type OF
        |COMENT:
           coment();
        |EXTDEF:
           extdef(objs.K_EXTDEF);
        |TYPDEF:

        |PUBDEF, PUBDEF + REC32:
           pubdef(objs.K_PUBDEF);
        |LINNUM, LINNUM + REC32:
           (* They are not read yet *)
           SkipData(record_len);
        |LNAMES:
           lnames({});
        |XOMF_SEGDEF:
           xomf_segdef();
        |LEDATA, LEDATA + REC32:
           ledata();
        |LIDATA, LIDATA + REC32:
           lidata();
        |XOMF_RAWDATA:
           xomf_rawdata ();
        |XOMF_JSTRINGDESC, XOMF_EXPDEF, XOMF_IMPDEF:
           SkipData(record_len);
        |XOMF_FIXUP:
           xomf_fixup();
      ELSE
        Error("Unknown record type %x\n", record_type);
      END;
    END;
  END;
END ReadRecord;

PROCEDURE ReadObj * (name-: ARRAY OF CHAR; dump:= FALSE: BOOLEAN);
VAR signature :CARD;
    version   :CARD;
BEGIN
  objs.ReadFile(name);
  lstr.Assign(name, file_name);
  record_len:= MAX(CARD);
  LINNUM_FORMAT:= LINNUM_CV;

  LOOP
    GetData(signature);
    IF signature # XOMF_SIGNATURE THEN
      Error("XOMF object file format expected\n");
    END;

    GetData(version);
    IF version # XOMF_FORMAT_VERSION THEN
      Error("XOMF format version %d expected (version %d found)\n", XOMF_FORMAT_VERSION, version);
    END;

    REPEAT
      ReadRecord(dump);
    UNTIL (record_type = XOMF_OBJEND);

    IF (objs.file_ptr >= objs.Length(objs.file_data^)) THEN
      EXIT;
    END;
  END;
END ReadObj;

(* --------------------------------------------------------------------*)
BEGIN
  Init();
END ReadXOMF.
