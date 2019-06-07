MODULE Objects;
IMPORT
  SYSTEM,
  adt,
  lstr:= LongStrs,
  io:= Printf,
  file:= H2DFile,
  fmt:= FormStr,
  RTS:= oberonRTS;


CONST
  (* Module formats *)
  OMFFormat * = 0;

  (* Tags in tables *)

  K_PUBDEF     * = {0};
  K_EXTDEF     * = {1};
  K_COMDEF     * = {0,1};
  K_IMPORT     * = {2};
  K_WEAKDEF    * = {0,2};

  K_MASK       * = {0,1,2};  (* (kind * K_MASK) = K_*DEF *)

  K_USED       * = {7};
  K_PROCESSED  * = {6};
  K_BY_ORDINAL * = {5};
  K_IMPFUNC    * = {4};     (* 0 - variable, 1 - function *)


  (* Fixup kinds *)
  FX_LOW8        * =  0;
  FX_OFFSET16    * =  1;
  FX_SEL16       * =  2;
  FX_FAR16_16    * =  3;
  FX_HIGH8       * =  4;
  FX_OFFSET16LR  * =  5;
  FX_OFFSET32    * =  9;
  FX_FAR16_32    * = 11;
  FX_OFFSET32_LR * = 13;
  FX_TDINDEX16   * = 14;
  FX_JAVASTRING  * = 15;
  FX_BYTESTRING  * = 16;
  FX_SECREL32    * = 17;                             (* Appears only in COFF obj files *)
  FX_OFFSET32NB  * = 18;                             (* Appears only in COFF obj files *)
  FX_TDINDEX32   * = 19;

  FX_SELFRELATIVE  * = 128;


  (* Frame kinds *)
  FK_SEG    * = 0;
  FK_GROUP  * = 1;
  FK_ID     * = 2;
  FK_TARGET * = 5;

  (* Target kinds *)
  TK_SEG     * = 0;
  TK_GROUP   * = 1;
  TK_ID      * = 2;
  TK_FWD_SEG * = 3;
  TK_UNKNOWN * = 77;

TYPE
  INT    = SYSTEM.INT32;
  CARD8  = SYSTEM.CARD8;
  CARD16 = SYSTEM.CARD16;
  CARD   = SYSTEM.CARD32;

  HashCode * = INT;

  RawData * = POINTER TO ARRAY OF SYSTEM.BYTE;

  Record * = POINTER TO RecordDesc;
  RecordDesc * = RECORD (adt.ElementDesc)
    file_pos * : CARD;
    data     * : RawData;
    len      * : CARD;
  END;

  OMFRecord * = POINTER TO OMFRecordDesc;
  OMFRecordDesc * = RECORD (RecordDesc)
    type * : SYSTEM.CARD8;
  END;


  Module  * = POINTER TO ModuleDesc;
  Segment * = POINTER TO SegmentDesc;
  Fixup   * = POINTER TO FixupDesc;
  Text    * = POINTER TO TextDesc;
  Instr   * = POINTER TO InstrDesc;
  Label   * = POINTER TO LabelDesc;


  ModuleDesc * = RECORD (adt.NamedElementDesc)
    format   * : INT;
    src_name * : lstr.String;
    file_name* : lstr.String;
    lib_name * : lstr.String;
    recs     * : adt.List;
    segs     * : adt.List;
    publics  * : adt.List;
    groups   * : adt.List;
    entry    * : Fixup;
    label_num* : CARD;
  END;

  SegmentDesc * = RECORD (adt.ElementDesc)
    mod       * : Module;
    fixups    * : adt.List;
    name      * : HashCode;
    class     * : HashCode;
    overlay   * : HashCode;
    length    * : CARD;
    address   * : CARD;
    alignment * : CARD;
    combination * : CARD8;
    attributes  * : CARD8;
    text        * : RawData;
    labels      * : adt.Tree;
    strings     * : adt.List;
    errors      * : CARD;
  END;

  TextDesc * = RECORD (adt.ElementDesc)
    offset * : CARD;
    text   * : lstr.String;
  END;

  InstrDesc * = RECORD (TextDesc)
    label1 * : Label;
    label2 * : Label;
    label3 * : Label;
  END;

  LabelDesc * = RECORD (TextDesc)
    fixup   * : Fixup;
    fmt     * : lstr.String;
    printed * : BOOLEAN;
  END;

  StrNode = POINTER TO StrNodeDesc;
  StrNodeDesc = RECORD (adt.NamedElementDesc)
    idx: HashCode;
  END;

  NameNode * = POINTER TO NameNodeDesc;
  NameNodeDesc * = RECORD (adt.ElementDesc)
    idx  * : HashCode;
  END;

  IndexNode * = POINTER TO IndexNodeDesc;
  IndexNodeDesc * = RECORD (NameNodeDesc);
    name * : lstr.String;
  END;

  Name * = POINTER TO NameDesc;
  NameDesc * = RECORD (NameNodeDesc)
    kind * : SET;
  END;

  ImpName * = POINTER TO ImpNameDesc;
  ImpNameDesc * = RECORD (NameDesc)
    extname * : HashCode;
    ordinal * : CARD16;
    module  * : HashCode;
  END;

  Group * = POINTER TO GroupDesc;
  GroupDesc * = RECORD (NameNodeDesc)
    segs * : adt.List;
  END;

  Public * = POINTER TO PublicDesc;
  PublicDesc * = RECORD (adt.ElementDesc)
    name       * : HashCode;
    seg        * : Segment;
    offset     * : CARD;
  END;

  FixupDesc * = RECORD (adt.ElementDesc)
    kind    * : CARD8;
    offset  * : CARD;
    k_frame * : CARD8;
    seg_frame  * : Segment;
    grp_frame  * : Group;
    name_frame * : HashCode;
    k_target   * : CARD8;
    seg_target * : Segment;
    grp_target * : Group;
    name_target* : HashCode;
    fx_offset  * : CARD;
  END;



VAR
  CODE * , Const * , DATA * , BSS * ,
  SYMBOLS_OMF * , SYMBOLS_COFF * ,
  TYPES_OMF * , TYPES_COFF * : HashCode;



  file_name * , lib_name * : lstr.String;
  file_data * : RawData;
  file_ptr *  : CARD;

  Modules * : adt.List;
  CurrentModule * : Module;

  Groups * : adt.List;

  Names * : adt.Tree;

  label1, label2, label3: Label;
  empty_label   : Label;


  IndexTree: adt.Tree;
  StrTree  : adt.Tree;
  MaxIdx   : HashCode;

(* --------------------------------------------------------------------- *)
PROCEDURE IsSuchName * (name-: ARRAY OF CHAR): BOOLEAN;
VAR ne: adt.NamedElement;
    e : adt.Element;
BEGIN
  adt.NewNamedElement(ne, name);
  StrTree.Find(ne, e);
  adt.Deallocate(ne);
  RETURN e # NIL;
END IsSuchName;


(* --------------------------------------------------------------------- *)
PROCEDURE NewGroup * (name: HashCode; VAR g: Group);
VAR e : adt.Element;
    nn: NameNode;
BEGIN
  NEW(nn); nn.idx:= name;
  Groups.Find(nn, e);
  IF e = NIL THEN
    NEW(g); g.idx:= name;
    adt.NewList(g.segs);
    CurrentModule.groups.Insert(g);
    Groups.Insert(g);
  ELSE
    g:= e(Group);
  END;
END NewGroup;

(* --------------------------------------------------------------------- *)
PROCEDURE NewSegment * (VAR s: Segment; name, class, overlay: HashCode; length: CARD);
BEGIN
  NEW(s);
  s.mod:= CurrentModule;
  adt.NewList(s.fixups);
  s.name     := name;
  s.class    := class;
  s.overlay  := overlay;
  s.length   := length;
  s.address  := 0ffffffffH;
  s.alignment:= 0;
  IF (class # BSS) & (length > 0) THEN
    NEW(s.text, length);
    SYSTEM.FILL(SYSTEM.ADR(s.text^), 0, length);
  END;
  adt.NewTree(s.labels);
  adt.NewList(s.strings);
END NewSegment;

(* --------------------------------------------------------------------- *)
PROCEDURE (s: Segment) out * (offset:= MAX(CARD): CARD; format-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
VAR instr: Instr;
    text: ARRAY 1024 OF CHAR;
BEGIN
  NEW(instr);
  instr.offset:= offset;
  fmt.print(text, format, x);
  lstr.Assign(text, instr.text);
  s.strings.Insert(instr);
END out;

(* --------------------------------------------------------------------- *)
PROCEDURE (s: Segment) out_instr * (offset: CARD; text-: ARRAY OF CHAR);
VAR instr: Instr;
BEGIN
  NEW(instr);
  instr.offset:= offset;
  lstr.Assign(text, instr.text);
  instr.label1:= label1;
  instr.label2:= label2;
  instr.label3:= label3;
  label1:= empty_label;
  label2:= empty_label;
  label3:= empty_label;
  s.strings.Insert(instr);
END out_instr;

(* --------------------------------------------------------------------- *)
PROCEDURE NewPublic * (VAR p: Public;
                           n: HashCode;
                           s: Segment;
                           o: CARD);
BEGIN
  NEW(p);
  p.name:= n;
  p.seg:= s;
  p.offset:= o;
  CurrentModule.publics.Insert(p);
END NewPublic;

(* --------------------------------------------------------------------- *)
PROCEDURE NewFixup * (VAR f: Fixup);
BEGIN
  NEW(f);
END NewFixup;

(* --------------------------------------------------------------------- *)
PROCEDURE NewLabel * (s: Segment; offset: CARD; VAR name: ARRAY OF CHAR; format: ARRAY OF CHAR);
VAR l: Label;
    e: adt.Element;
BEGIN
  NEW(l);
  l.offset:= offset;
  s.labels.Find(l, e);
  IF e = NIL THEN
    IF name[0] = 0X THEN
      COPY('%s', name);
      IF label1 = empty_label THEN
        label1:= l;
      ELSIF label2 = empty_label THEN
        label2:= l;
      ELSIF label3 = empty_label THEN
        label3:= l;
      ELSE
        ASSERT(FALSE);
      END;
    END;
    lstr.Assign(name, l.text);
    lstr.Assign(format, l.fmt);
    s.labels.Insert(l);
  ELSIF e(Label).text^ = '%s' THEN
    l:= e(Label);
    COPY('%s', name);
    IF label1 = empty_label THEN
      label1:= l;
    ELSIF label2 = empty_label THEN
      label2:= l;
    ELSIF label3 = empty_label THEN
      label3:= l;
    ELSE
      ASSERT(FALSE);
    END;
  ELSE
    COPY(e(Label).text^, name);
  END;
END NewLabel;

(* --------------------------------------------------------------------- *)
PROCEDURE InitLabels * ();
BEGIN
  label1:= empty_label;
  label2:= empty_label;
  label3:= empty_label;
END InitLabels;

(* --------------------------------------------------------------------- *)
PROCEDURE NewModule * (name-: ARRAY OF CHAR; format: INT);
VAR _path, _name, _ext: lstr.String;
BEGIN
  file.SplitName(name, _path, _name, _ext);
  NEW(CurrentModule); CurrentModule.SetName(_name^);
  CurrentModule.format:= format;
  lstr.Assign(name, CurrentModule.src_name);
  lstr.Assign(file_name^, CurrentModule.file_name);
  lstr.Assign(lib_name^, CurrentModule.lib_name);
  adt.NewList(CurrentModule.segs);
  adt.NewList(CurrentModule.recs);
  adt.NewList(CurrentModule.publics);
  adt.NewList(CurrentModule.groups);
  CurrentModule.entry:= NIL;
  Modules.Insert(CurrentModule);
  CurrentModule.label_num:= 0;
END NewModule;

(* --------------------------------------------------------------------- *)
PROCEDURE (p: IndexNode) Compare * ( e: adt.Element ): INT;
BEGIN
  IF e IS IndexNode THEN
    IF p.idx = e(IndexNode).idx THEN
      RETURN adt.equal;
    ELSIF p.idx > e(IndexNode).idx THEN
      RETURN adt.more;
    ELSE
      RETURN adt.less;
    END;
  ELSE
    RETURN adt.noncompared;
  END;
END Compare;

(* --------------------------------------------------------------------- *)
PROCEDURE (p: Text) Compare * ( e: adt.Element ): INT;
BEGIN
  IF e IS Text THEN
    IF p.offset = e(Text).offset THEN
      RETURN adt.equal;
    ELSIF p.offset > e(Text).offset THEN
      RETURN adt.more;
    ELSE
      RETURN adt.less;
    END;
  ELSE
    RETURN adt.noncompared;
  END;
END Compare;

(* --------------------------------------------------------------------- *)
PROCEDURE StrToIndex * (name-: ARRAY OF CHAR): HashCode;
VAR sn: StrNode;
    in: IndexNode;
    e : adt.Element;
BEGIN
  NEW(sn); sn.SetName(name);
  StrTree.Find(sn, e);
  IF e = NIL THEN
    MaxIdx:= MaxIdx + 1;
    sn.idx:= MaxIdx;
    NEW(in); lstr.Assign(name, in.name);
    in.idx:= MaxIdx;
    StrTree.Insert(sn);
    IndexTree.Insert(in);
    RETURN MaxIdx;
  ELSE
    RETURN e(StrNode).idx;
  END;
END StrToIndex;

PROCEDURE IndexToStr * (idx: HashCode; VAR name: lstr.String);
VAR in: IndexNode;
    e : adt.Element;
BEGIN
  NEW(in); in.idx:= idx;
  IndexTree.Find(in, e);
  ASSERT(e # NIL);
  lstr.Assign(e(IndexNode).name^, name);
END IndexToStr;

(* --------------------------------------------------------------------- *)
PROCEDURE FindName * (idx: HashCode): Name;
VAR e: adt.Element;
    n: Name;
BEGIN
  NEW(n); n.idx:= idx;
  Names.Find(n, e);
  IF e # NIL THEN
    RETURN e(Name);
  ELSE
    RETURN NIL;
  END;
END FindName;

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
PROCEDURE Error (fmt-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN
  io.printf(fmt, x);
  HALT;
END Error;

(* --------------------------------------------------------------------- *)
PROCEDURE ReadFile * (name-: ARRAY OF CHAR);

CONST box_size = 1024;

TYPE Box = POINTER TO BoxDesc;
     BoxDesc = RECORD (adt.ElementDesc)
       data: ARRAY box_size OF SYSTEM.BYTE;
     END;

VAR fd   : file.FILE;
    boxes: adt.List;
    box  : Box;
    i, data_ptr: INT;
    byte : SYSTEM.BYTE;
    e    : adt.Element;
    size : INT;

BEGIN
  IF ~file.Open(name, file.rdmode, fd, file.raw_file) THEN Error("Can't open file %s\n", name) END;
  data_ptr:= box_size - 1;
  adt.NewList(boxes); box:= NIL; size:= 0;
  WHILE file.RdBin(fd, byte) DO
    INC(data_ptr);
    IF data_ptr = box_size THEN
      data_ptr:= 0;
      boxes.Insert(box);
      NEW(box);
    END;
    box.data[data_ptr]:= byte;
    INC(size);
  END;
  boxes.Insert(box);
  file.Close(fd);

  IF size = 0 THEN Error("File %s is empty\n", name) END;

  NEW(file_data, size); file_ptr:= 0;
  boxes.FindFirst(e); i:= 0;
  WHILE e # NIL DO
    data_ptr:= 0;
    WHILE (i < size) & (data_ptr < box_size) DO
      file_data[i]:= e(Box).data[data_ptr];
      INC(i); INC(data_ptr);
    END;
    boxes.FindNext(e);
  END;
  file_ptr:= 0;
  lstr.Assign(name, file_name); lstr.Assign('', lib_name);
END ReadFile;

PROCEDURE Length * (VAR a: ARRAY OF SYSTEM.BYTE): CARD;
BEGIN
  RETURN VAL(CARD, LEN(a));
END Length;

(* --------------------------------------------------------------------*)
PROCEDURE GetData * (VAR d: ARRAY OF SYSTEM.BYTE; len:= 0: CARD; shift_ptr:= TRUE: BOOLEAN);
BEGIN
  IF len = 0 THEN len:= LEN(d) END;
  ASSERT(len <= Length(d));
  IF (file_ptr + len) > Length(file_data^) THEN
    Error("Record is too long\n");
  END;
  SYSTEM.MOVE(SYSTEM.ADR(file_data[file_ptr]), SYSTEM.ADR(d), len);
  IF shift_ptr THEN file_ptr:= file_ptr + len END;
END GetData;

(* --------------------------------------------------------------------*)
PROCEDURE SkipData * (len: CARD);
BEGIN
  file_ptr:= file_ptr + len;
END SkipData;

(* --------------------------------------------------------------------- *)
PROCEDURE Init * ();
BEGIN
  NEW(empty_label); lstr.Assign('', empty_label.text);
  label1:= empty_label;
  label2:= empty_label;
  label3:= empty_label;

  CurrentModule:= NIL;
  adt.NewList(Modules);
  adt.NewTree(IndexTree);
  adt.NewTree(StrTree);
  adt.NewTree(Names);
  adt.NewList(Groups);
  CODE := StrToIndex("CODE");
  Const:= StrToIndex("CONST");
  DATA := StrToIndex("DATA");
  BSS  := StrToIndex("BSS");
  SYMBOLS_OMF := StrToIndex("$$SYMBOLS");
  SYMBOLS_COFF:= StrToIndex(".debug$S");
  TYPES_OMF   := StrToIndex("$$TYPES");
  TYPES_COFF  := StrToIndex(".debug$T");
  RTS.Collect;
END Init;

(* --------------------------------------------------------------------- *)
END Objects.
