MODULE ReadOMF;

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
    type  : CARD16;
    kind  : SET;
  END;


(* --------------------------------------------------------------------*)
CONST
  LINNUM_CV   = 0;
  LINNUM_HLL4 = 1;

(* --------------------------------------------------------------------*)
VAR
  record_type: CARD8;
  record_len : CARD16;
  record_pos : CARD;

  LINNUM_FORMAT: INT;

  libhdr_rec: objs.OMFRecord;
  file_name : lstr.String;

  Locals, Segs, PubNames, Groups, LNames, ExpDefs, ExtNames: adt.List;

  LedataSeg: objs.Segment;
  LastOffset: CARD;


  (* Frames *)
  frame_kinds: ARRAY 4 OF CARD8;
  seg_frames : ARRAY 4 OF objs.Segment;
  grp_frames : ARRAY 4 OF objs.Group;
  name_frames: ARRAY 4 OF objs.HashCode;

  (* Targets *)
  target_kinds: ARRAY 4 OF CARD8;
  seg_targets : ARRAY 4 OF objs.Segment;
  grp_targets : ARRAY 4 OF objs.Group;
  name_targets: ARRAY 4 OF objs.HashCode;



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
PROCEDURE getLName(idx: INT; VAR name: objs.HashCode): BOOLEAN;
(* Returns TRUE if name is local *)
VAR e: adt.Element;
BEGIN
  LNames.FindFirst(e);
  WHILE idx > 0 DO
    LNames.FindNext(e);
    DEC(idx);
  END;
  name:= e(LName).name;
  RETURN (e(LName).kind * K_LOCAL) = K_LOCAL;
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
PROCEDURE getGroup(idx: INT): objs.Group;
VAR e: adt.Element;
BEGIN
  Groups.FindFirst(e);
  WHILE idx > 0 DO
    Groups.FindNext(e);
    DEC(idx);
  END;
  RETURN e(objs.Group);
END getGroup;

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

PROCEDURE GetIdent(): objs.HashCode;
VAR name: ARRAY 256 OF CHAR;
BEGIN
  GetName(name);
  RETURN objs.StrToIndex(name);
END GetIdent;

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
PROCEDURE GetSegName(): objs.HashCode;
VAR s: objs.Segment;
BEGIN
  card16:= GetIndex();
  s:= getSeg(card16 - 1);
  RETURN s.name;
END GetSegName;

(* --------------------------------------------------------------------*)
PROCEDURE GetCommLen(): CARD;
BEGIN
  GetData(card8); card32:= card8;
  IF card32 > 080H THEN
    CASE card32 OF
      |081H: GetData (card16);
             card32:= card16;
      |084H: GetData(card16); GetData(card8);
             card32:= card8 * 010000H + card16;
      |088H: GetData(card32);
    ELSE
      Error("File %s: unknown comdat %x\n", objs.CurrentModule.file_name^, card32);
    END;
  END;
  RETURN card32;
END GetCommLen;

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
    name: ARRAY 256 OF CHAR;
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

            int_name:= GetIdent();

            GetName(name); CheckExt(name, '.dll');
            Strings.Capitalize(name);
            module:= objs.StrToIndex(name);

            ext_name:= 0; ordinal:= 0;
            IF ord_flag = 0 THEN
              (* Import by name *)
              GetName(name);
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
            ed.ext_name:= GetIdent();
            GetName(name);
            ed.int_name_present:= name[0] # 0X;
            IF ed.int_name_present THEN
              ed.int_name:= objs.StrToIndex(name);
            ELSE
              ed.int_name:= ed.ext_name;
            END;
            IF ord_flag # 0 THEN GetData(ed.ordinal) END;
            ExpDefs.Insert(ed);
       ELSE
         Error("Unknown commnet subtype\n");
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
    en.name:= GetIdent();
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
  group: objs.Group;
  seg  : objs.Segment;
  pn   : PubName;
  p    : objs.Public;
BEGIN
  card16:= GetIndex();
  IF card16 = 0 THEN
    group:= NIL;
  ELSE
    group:= getGroup(card16 - 1);
  END;
  card16:= GetIndex();
  IF card16 = 0 THEN
    seg:= NIL;
    GetData(card16); (* skip frame *)
  ELSE
    seg:= getSeg(card16 - 1);
  END;

  WHILE CanRead() DO
    NEW(pn);
    pn.name  := GetIdent();
    pn.offset:= GetWordOrDword();
    pn.type  := GetIndex();
    pn.seg   := seg;
    pn.group := group;
    pn.kind  := kind;
    PubNames.Insert(pn);
    objs.NewPublic(p, pn.name, pn.seg, pn.offset);
  END;
END pubdef;

(* --------------------------------------------------------------------*)
PROCEDURE grpdef();
VAR name: objs.HashCode;
    s: lstr.String;
    g: objs.Group;
    e: adt.Element;
    nn: objs.NameNode;
BEGIN
  card16:= GetIndex();

  IF getLName(card16 - 1, name) THEN
    objs.IndexToStr(name, s);
    Error("File %s: local group %s is not supported\n", objs.CurrentModule.file_name^, s^);
  END;


  objs.NewGroup(name, g);
  Groups.Insert(g);

  WHILE CanRead() DO
    GetData(card8);
    IF card8 # 0FFH THEN
      Error("File %s: group type %x is not supported\n", objs.CurrentModule.file_name^, card8);
    END;
    NEW(nn); nn.idx:= GetSegName();
    g.segs.Find(nn, e);
    IF e = NIL THEN g.segs.Insert(nn) END;
  END;
END grpdef;

(* --------------------------------------------------------------------*)
PROCEDURE segdef();
VAR attribs, alignment, combination: CARD8;
    frame : CARD16;
    offset: CARD8;
    length: CARD;
    name, class, overlay: objs.HashCode;
    tmp   : BOOLEAN;
    s     : objs.Segment;

BEGIN
  GetData(attribs);
  alignment:= attribs DIV 32;
  combination:= (attribs MOD 32) DIV 4;
  IF alignment = 0 THEN
    GetData(frame);
    GetData(offset);
  END;

  length:= GetWordOrDword();
  card16:= GetIndex();
  IF card16 # 0 THEN tmp:= getLName(card16 - 1, name) ELSE name:= 0 END;
  card16:= GetIndex();
  IF card16 # 0 THEN tmp:= getLName(card16 - 1, class) ELSE class:= 0 END;
  card16:= GetIndex();
  IF card16 # 0 THEN tmp:= getLName(card16 - 1, overlay) ELSE overlay:= 0 END;
  IF (attribs DIV 2 ) = 2 THEN length:= 10000H END;

  -- support for nasm-generated object files
  IF (class = objs.StrToIndex ("")) THEN
    IF (name = objs.StrToIndex ("text")) THEN
      class := objs.CODE;
    ELSIF (name = objs.StrToIndex ("data")) THEN
      class := objs.DATA;
    ELSIF (name = objs.StrToIndex ("bss")) THEN
      class := objs.BSS;
    END;
  END;

  objs.NewSegment(s, name, class, overlay, length);
  Segs.Insert(s); objs.CurrentModule.segs.Insert(s);
  CASE alignment OF
    |0:   s.address  := frame;
    |1:   s.alignment:= 1;
    |2:   s.alignment:= 2;
    |3:   s.alignment:= 16;
    |4:   s.alignment:= 4096;
    |5:   s.alignment:= 4;
    |6,7: s.alignment:= 0ffffffffH;
  END;
  s.combination:= combination;
  s.attributes := attribs;
END segdef;

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
PROCEDURE comdef(kind: SET);
VAR en: ExtName;
    comm_count, comm_len: CARD;
    data_type: CARD8;
BEGIN
  WHILE CanRead() DO
    NEW(en); ExtNames.Insert(en);
    en.name:= GetIdent();
    en.type:= GetIndex();
    en.kind:= kind;
    GetData(data_type);
    CASE data_type OF
      |061H: comm_count:= GetCommLen();
             comm_len:= GetCommLen();
             en.length:= comm_count * comm_len;
      |062H: comm_len:= GetCommLen();
             en.length:= comm_len;
    ELSE
      Error("File %s: unknown common type %\n", objs.CurrentModule.file_name^, data_type);
    END;
  END;
END comdef;

(* --------------------------------------------------------------------*)
PROCEDURE comdat();
BEGIN
  Error("File %s: unsupported OMF record type %x\n", objs.CurrentModule.file_name^, record_type);
END comdat;

(* --------------------------------------------------------------------*)
PROCEDURE setSegFrame(n: INT; s: objs.Segment);
BEGIN
  seg_frames[n]:= s;
  grp_frames[n]:= NIL;
  name_frames[n]:= 0;
END setSegFrame;

PROCEDURE setGrpFrame(n: INT; g: objs.Group);
BEGIN
  seg_frames[n]:= NIL;
  grp_frames[n]:= g;
  name_frames[n]:= 0;
END setGrpFrame;

PROCEDURE setNameFrame(n: INT; name: objs.HashCode);
BEGIN
  seg_frames[n]:= NIL;
  grp_frames[n]:= NIL;
  name_frames[n]:= name;
END setNameFrame;

PROCEDURE getFrame(f: objs.Fixup; n: INT);
BEGIN
  IF seg_frames[n] # NIL THEN
    f.seg_frame:= seg_frames[n];
  ELSIF grp_frames[n] # NIL THEN
    f.grp_frame:= grp_frames[n];
  ELSIF name_frames[n] # 0 THEN
    f.name_frame:= name_frames[n];
  ELSE
    ASSERT(FALSE);
  END;
END getFrame;


PROCEDURE setSegTarget(n: INT; s: objs.Segment);
BEGIN
  seg_targets[n]:= s;
  grp_targets[n]:= NIL;
  name_targets[n]:= 0;
END setSegTarget;

PROCEDURE setGrpTarget(n: INT; g: objs.Group);
BEGIN
  seg_targets[n]:= NIL;
  grp_targets[n]:= g;
  name_targets[n]:= 0;
END setGrpTarget;

PROCEDURE setNameTarget(n: INT; name: objs.HashCode);
BEGIN
  seg_targets[n]:= NIL;
  grp_targets[n]:= NIL;
  name_targets[n]:= name;
END setNameTarget;

PROCEDURE getTarget(f: objs.Fixup; n: INT);
BEGIN
  IF seg_targets[n] # NIL THEN
    f.seg_target:= seg_targets[n];
  ELSIF grp_targets[n] # NIL THEN
    f.grp_target:= grp_targets[n];
  ELSIF name_targets[n] # 0 THEN
    f.name_target:= name_targets[n];
  ELSE
    ASSERT(FALSE);
  END;
END getTarget;

PROCEDURE fixupp();
VAR seg: objs.Segment;
    str: lstr.String;
    first: CARD8;
    fixup: objs.Fixup;
    locat: CARD8;
    fix_data: CARD8;
    offset: CARD;
    n: INT;
    method: CARD8;



BEGIN
  seg:= LedataSeg;
  IF (seg # NIL) & (seg.class = objs.BSS) THEN
    objs.IndexToStr(seg.name, str);
    Error("File %s: fixup in BSS segment %s\n", objs.CurrentModule.file_name^, str^);
  END;
  WHILE CanRead() DO
    GetData(first);
    IF first >= 080H THEN
      IF seg = NIL THEN
        Error("File %s: fixup without ledata\n", objs.CurrentModule.file_name^);
      END;
      objs.NewFixup(fixup);
      seg.fixups.Insert(fixup);
      locat:= (first DIV 4) MOD 010H;
      fixup.kind:= locat;
      IF ((first DIV 040H) MOD 2) = 0 THEN
        fixup.kind:= fixup.kind + objs.FX_SELFRELATIVE;
      END;
      GetData(card8); offset:= card8;
      offset:= offset + (first MOD 4) * 0100H + LastOffset;
      fixup.offset:= offset;
      GetData(fix_data);
      IF fix_data >= 080H THEN
        n:= (fix_data DIV 16) MOD 4;
        fixup.k_frame:= frame_kinds[n];
        getFrame(fixup, n);
        IF fixup.k_frame = 4 THEN
          fixup.k_frame:= objs.FK_SEG;
          fixup.seg_frame:= seg;
        END;
      ELSE
        method:= (fix_data DIV 16) MOD 8;
        fixup.k_frame:= method;
        CASE method OF
          |0: card16:= GetIndex();
              fixup.seg_frame:= getSeg(card16 - 1);
          |1: card16:= GetIndex();
              fixup.grp_frame:= getGroup(card16 - 1);
          |2: card16:= GetIndex();
              fixup.name_frame:= getExtName(card16 - 1);
          |4: fixup.k_frame:=objs.FK_SEG;
              fixup.seg_frame:= seg;
          |5:
        END;
      END;
      IF ((fix_data MOD 16) DIV 8) = 1 THEN
        n:= fix_data MOD 4;
        fixup.k_target:= target_kinds[n];
        getTarget(fixup, n);
      ELSE
        card16:= GetIndex();
        method:= fix_data MOD 4;
        fixup.k_target:= method;
        CASE method OF
          |0: fixup.seg_target:= getSeg(card16 - 1);
          |1: fixup.grp_target:= getGroup(card16 - 1);
          |2: fixup.name_target:= getExtName(card16 - 1);
        END;
      END;
      IF ((fix_data MOD 8) DIV 4) = 1 THEN
        fixup.fx_offset:= 0;
      ELSE
        fixup.fx_offset:= GetWordOrDword();
      END;
    ELSE
      n:= first MOD 4;
      method:= (first DIV 4) MOD 8;
      IF ((first MOD 080H) DIV 040H) = 1 THEN
        frame_kinds[n]:= method;
        CASE method OF
          |0: card16:= GetIndex();
              setSegFrame(n, getSeg(card16 - 1));
          |1: card16:= GetIndex();
              setGrpFrame(n, getGroup(card16 - 1));
          |2: card16:= GetIndex();
              setNameFrame(n, getExtName(card16 - 1));
        END;
      ELSE
        card16:= GetIndex();
        target_kinds[n]:= method MOD 4;
        CASE target_kinds[n] OF
          |0: setSegTarget(n, getSeg(card16 - 1));
          |1: setGrpTarget(n, getGroup(card16 - 1));
          |2: setNameTarget(n, getExtName(card16 - 1));
        END;
      END;
    END;
  END;
END fixupp;

(* --------------------------------------------------------------------*)
PROCEDURE modend();
VAR entry: objs.Fixup;
    fix_data: CARD8;
    method: CARD8;
    n: INT;
BEGIN
  GetData(card8);
  IF ((card8 MOD 080H) DIV 040H) = 1 THEN
    objs.NewFixup(entry);
    entry.kind:= objs.FX_FAR16_32;
    entry.offset:= 0;
    GetData(fix_data);
    IF fix_data >= 080H THEN
      n:= (fix_data DIV 16) MOD 4;
      entry.k_frame:= frame_kinds [n];
      getFrame(entry, n);
      IF entry.k_frame = 4 THEN
        IF LedataSeg = NIL THEN
          Error("File %s: invalid entry point\n", objs.CurrentModule.file_name^);
        END;
        entry.k_frame:= objs.FK_SEG;
        entry.seg_frame:= LedataSeg;
      END;
    ELSE
      method:= (fix_data DIV 16) MOD 8;
      entry.k_frame:= method;
      CASE method OF
         |0: card16:= GetIndex();
             entry.seg_frame:= getSeg(card16 - 1);
         |1: card16:= GetIndex();
             entry.grp_frame:= getGroup(card16 - 1);
         |2: card16:= GetIndex();
             entry.name_frame:= getExtName(card16 - 1);
         |4: IF LedataSeg = NIL THEN
               Error("File %s: invalid entry point\n", objs.CurrentModule.file_name^);
             END;
             entry.k_frame:= objs.FK_SEG;
             entry.seg_frame:= LedataSeg;
         |5:
      END;
    END;
    IF ((fix_data MOD 16) DIV 8) = 1 THEN
      n:= fix_data MOD 4;
      entry.k_target:= target_kinds [n];
      getTarget(entry, n);
    ELSE
      card16:= GetIndex();
      method:= fix_data MOD 4;
      entry.k_target:= method;
      CASE method OF
        |0: entry.seg_target:= getSeg(card16 - 1);
        |1: entry.grp_target:= getGroup(card16 - 1);
        |2: entry.name_target:= getExtName(card16 - 1);
      END;
    END;
    entry.fx_offset:= GetWordOrDword();
    objs.CurrentModule.entry:= entry;
  END;
END modend;

(* --------------------------------------------------------------------*)
PROCEDURE GetHeader();
BEGIN
  IF record_len # MAX(CARD16) THEN
    SkipData(record_pos + record_len - objs.file_ptr);
  END;
  GetData(record_type);
  GetData(record_len);
  record_pos:= objs.file_ptr;
END GetHeader;

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
PROCEDURE ReadRecord(dump: BOOLEAN);
VAR buf: ARRAY 2048 OF CHAR;
    rec: objs.OMFRecord;

BEGIN
  GetHeader();

  NEW(rec); rec.type:= record_type; rec.len:= record_len + 3;
  NEW(rec.data, record_len);
  rec.file_pos:= objs.file_ptr;
  GetData(rec.data^, record_len, FALSE);
  IF objs.CurrentModule # NIL THEN
    objs.CurrentModule.recs.Insert(rec);
  ELSE
    IF ((record_type # THEADR) & (record_type # LHEADR) & (record_type # LIBHDR)) THEN
      Error("Incorrect object file format (OMF expected)\n");
    END;
  END;

  CASE record_type OF
    |THEADR, LHEADR:
       GetName(buf);
       objs.NewModule(buf, objs.OMFFormat);
       objs.CurrentModule.recs.Insert(rec)
    |MODEND, MODEND + REC32:
       modend();
       (*
       IF WasLocals THEN
         ResolveLocals();
         AttachLocals ();
       END;
       IF ~ExpDefs.IsEmpty() THEN CollectExpdefs() END;
       CollectNames();
       *)
       IF libhdr_rec # NIL THEN
         SkipData(libhdr_rec.len - objs.file_ptr MOD libhdr_rec.len);
         record_len:= MAX(CARD16);
       END;
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
        |SEGDEF, SEGDEF + REC32:
           segdef();
        |GRPDEF:
           grpdef();
        |FIXUPP, FIXUPP + REC32:
           fixupp();
        |LEDATA, LEDATA + REC32:
           ledata();
        |LIDATA, LIDATA + REC32:
           lidata();
        |COMDEF:
           WasCOMDEFs:= TRUE;
           comdef(objs.K_COMDEF);
        |LEXTDEF:
           WasLocals:= TRUE;
           extdef(objs.K_EXTDEF + K_LOCAL);
        |LPUBDEF, LPUBDEF + REC32:
           WasLocals:= TRUE;
           pubdef(objs.K_PUBDEF + K_LOCAL);
        |LCOMDEF:
           WasCOMDEFs:= TRUE;
           WasLocals:= TRUE;
           comdef(objs.K_COMDEF + K_LOCAL);
        |COMDAT, COMDAT + REC32:
           comdat();
        |LLNAMES:
           WasLocals:= TRUE;
           lnames(K_LOCAL);
      ELSE
        Error("Unknown record type %x\n", record_type);
      END;
    END;
  END;
END ReadRecord;

PROCEDURE ReadObj * (name-: ARRAY OF CHAR; dump:= FALSE: BOOLEAN);
BEGIN
  objs.ReadFile(name);
  lstr.Assign(name, file_name);
  record_len:= MAX(CARD16);
  LINNUM_FORMAT:= LINNUM_CV;
  WHILE (objs.file_ptr + 1) < objs.Length(objs.file_data^) DO
    ReadRecord(dump);
  END;
END ReadObj;

(* --------------------------------------------------------------------*)
BEGIN
  Init();
END ReadOMF.
