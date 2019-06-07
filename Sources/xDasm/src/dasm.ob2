MODULE dasm;
IMPORT
  SYSTEM,
  adt,
  io:= Printf,
  objs:= Objects,
  kt:= KrnTypes,
  kd:= Krn_Dasm,
  mem:= Exe_Mem,
  fmt:= FormStr,
  lstr:= LongStrs,
  WholeStr,
  opts:= Options,
  cmdline,
  file:= H2DFile;

CONST
  default_address_format = "%$8x";

TYPE
  INT   = SYSTEM.INT32;
  CARD  = SYSTEM.CARD32;
  CARD8 = SYSTEM.CARD8;

VAR
  segment: objs.Segment;

  op_out_to_file: BOOLEAN;
  out_file      : file.FILE;


  (* formats *)
  offset_format: ARRAY 7 OF CHAR;
  label_format : ARRAY 100 OF CHAR;

(* --------------------------------------------------------------------- *)
PROCEDURE getData(from: objs.RawData; index: CARD; VAR to: ARRAY OF SYSTEM.BYTE);
BEGIN
  SYSTEM.MOVE(SYSTEM.ADR(from[index]), SYSTEM.ADR(to), LEN(to));
END getData;

(* --------------------------------------------------------------------- *)
PROCEDURE Error (fmt-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN
  io.printf(fmt, x);
  HALT;
END Error;

(* --------------------------------------------------------------------- *)
PROCEDURE ProcessPublics(m: objs.Module);
VAR e: adt.Element;
    name: lstr.String;
BEGIN
  m.publics.FindFirst(e);
  WHILE e # NIL DO
    WITH e: objs.Public DO
      objs.IndexToStr(e.name, name);
      objs.NewLabel(e.seg, e.offset, name^, label_format);
    END;
    m.publics.FindNext(e);
  END;
END ProcessPublics;

(* --------------------------------------------------------------------- *)
PROCEDURE ProcessFixup(s: objs.Segment; f: objs.Fixup; VAR label_name: ARRAY OF CHAR): CARD;
VAR tmp: lstr.String;
    offset: CARD;
    size: CARD;
BEGIN
  label_name[0]:= 0X;
  offset:= 0;
  CASE f.kind OF
    |objs.FX_SELFRELATIVE + objs.FX_OFFSET32,
     objs.FX_OFFSET32, objs.FX_OFFSET32NB:
       getData(s.text, f.offset, offset);
       size:= 4;
    |objs.FX_FAR16_32:
       getData(s.text, f.offset, offset);
       size:= 6;
       (*
                                if(FAR16_xxFixups){
                                  for(far_fixup = FAR16_xxFixups; far_fixup -> next; far_fixup = far_fixup ->next);
                                  far_fixup -> next = xalloc(sizeof(struct fixupFAR16_xx));
                                  far_fixup = far_fixup -> next;
                                }else{
                                  far_fixup = FAR16_xxFixups = xalloc(sizeof(struct fixupFAR16_xx));
                                };
                                far_fixup -> next   = NULL;
                                far_fixup -> source = addr + f -> offset;
                                far_fixup -> target = target + *fixup;
                                far_fixup -> kind   = f -> kind;

                                * fixup += target;
                                * (unsigned short * )((byte * )(fixup) + 4) = 0;
                                break;
       *)
    |objs.FX_FAR16_16:
       size:= 4;
       ASSERT(FALSE);
       (*
                                if(import_fixup) break;
                                ASSERT(target < 0x10000);
                                if(FAR16_xxFixups){
                                  for(far_fixup = FAR16_xxFixups; far_fixup -> next; far_fixup = far_fixup ->next);
                                  far_fixup -> next = xalloc(sizeof(struct fixupFAR16_xx));
                                  far_fixup = far_fixup -> next;
                                }else{
                                  far_fixup = FAR16_xxFixups = xalloc(sizeof(struct fixupFAR16_xx));
                                };
                                far_fixup -> next   = NULL;
                                far_fixup -> source = addr + f -> offset;
                                far_fixup -> target = ((struct seg * )(f -> target)) -> address;
                                far_fixup -> kind   = f -> kind;

                                *(unsigned short* )fixup += target;
                                * (unsigned short * )((byte * )(fixup) + 2) = 0;
                                break;
       *)
    |objs.FX_TDINDEX16:
       getData(s.text, f.offset, offset);
       offset := (offset AND 0FFFFH);
       size:= 2;
    |objs.FX_JAVASTRING, objs.FX_BYTESTRING, objs.FX_TDINDEX32:
       getData(s.text, f.offset, offset);
       size:= 4;
  ELSE
    Error("invalid fixup for flat memory model\n");
  END;

  CASE f.k_target OF
    |objs.TK_FWD_SEG:
       (*
                 target = (int) (f -> target);
                 for (t = c, w = s -> link;;){
                   if (w == NULL) {
                     t = t -> next;
                     if (t == NULL){
                       MessageStr2(xFATAL, msgUNRESOLVED_SEG,
                                   s -> file -> file, Index2Str (target));
                       return;
                     };
                     w = t -> segs;
                   }else if (w -> name == target && !strcmp(w->file->file, s->file->file)){
                     break;
                   }else{
                     w = w -> link;
                   };
                 };
                 target = w -> address;
                 break;
         *)
      |objs.TK_SEG:
         objs.NewLabel(f.seg_target, f.fx_offset + offset, label_name, label_format);
      |objs.TK_ID:
         objs.IndexToStr(f.name_target, tmp);
         IF (offset # 0) THEN
           io.sprintf (label_name, "%s+0%XH", tmp^, offset);
         ELSE
           COPY(tmp^, label_name);
         END;
      |objs.TK_UNKNOWN:
         COPY("???", label_name);
  ELSE
    Error("File %s: invalid fixup target\n", s.mod.file_name^);
  END;
  RETURN size;
  (*
                trg = target;
                target += f -> fx_offset;
                offset  = f -> offset;
                fixup = (unsigned * ) (q + offset);
                switch (f -> kind) {
                }
                f ++;
        } while (-- n);
  *)
END ProcessFixup;



(*----------------------------------------------------------------------*)

PROCEDURE ResolveAddressImpl(loc: kt.ADDRESS; addr: kt.ADDRESS; VAR result: ARRAY OF CHAR; ea :BOOLEAN): BOOLEAN;
VAR e: adt.Element;
    name: ARRAY 1025 OF CHAR;
    tmp: CARD;
BEGIN
  segment.fixups.FindFirst(e);
  WHILE e # NIL DO
    IF e(objs.Fixup).offset = loc THEN
      tmp:= ProcessFixup(segment, e(objs.Fixup), name);
      fmt.append(result, name);
      RETURN TRUE;
    END;
    segment.fixups.FindNext(e);
  END;
  IF addr # MAX(kt.ADDRESS) THEN
    <* IF DEST_XDASM THEN *>
    IF ea THEN
      io.sprintf (name, "0%XH", addr);
      fmt.append (result, name);
      RETURN TRUE;
    END;
    <* END *>
    name[0]:= 0X;
    objs.NewLabel(segment, addr, name, label_format);
    fmt.append(result, name);
    RETURN TRUE;
  END;
  RETURN FALSE;
END ResolveAddressImpl;


PROCEDURE ResolveAddress(loc: kt.ADDRESS; addr: kt.ADDRESS; VAR result: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN ResolveAddressImpl (loc, addr, result, FALSE);
END ResolveAddress;

(*----------------------------------------------------------------------*)

PROCEDURE ResolveEA(loc: kt.ADDRESS; addr: kt.ADDRESS; len: SYSTEM.CARD32; VAR str: ARRAY OF CHAR);
BEGIN
  IF ResolveAddressImpl(loc, addr, str, TRUE) THEN END;
END ResolveEA;

(*----------------------------------------------------------------------*)
(*----------------------------------------------------------------------*)
(*----------------------------------------------------------------------*)
(*----------------------------------------------------------------------*)
PROCEDURE genSegHeader(s: objs.Segment);
VAR name: lstr.String;
    align, USExx: ARRAY 20 OF CHAR;
BEGIN
  objs.IndexToStr(s.name,  name);
  CASE s.alignment OF
    |1:    align:= "BYTE";
    |2:    align:= "WORD";
    |4:    align:= "DWORD";
    |16:   align:= "PARA";
    |4096: align:= "4096"
  ELSE
    align:= "????";
  END;
  IF (s.attributes MOD 2) = 1 THEN
    USExx:= "USE32";
  ELSE
    USExx:= "USE16";
  END;
  s.out(,"\n--------------------------------------------------------------------------------\n");
  IF s.length < 10000H THEN
    offset_format:= " %$4x";
    s.out(,"Segment:  %s  %s  %s  %$4x bytes\n\n", name^, align, USExx, s.length);
  ELSE
    offset_format:= " %$8x";
    s.out(,"Segment:  %s  %s  %s  %$8x bytes\n\n", name^, align, USExx, s.length);
  END;
END genSegHeader;

(*----------------------------------------------------------------------*)
PROCEDURE genOffset(s: objs.Segment; offset: CARD);
BEGIN
  s.out(offset, offset_format, offset);
END genOffset;

(*----------------------------------------------------------------------*)
PROCEDURE genHexDump(s: objs.Segment; offset: CARD; len: CARD; empty:= FALSE: BOOLEAN);
VAR i: CARD;
    dump: ARRAY 40 OF CHAR;
    length: CARD;
BEGIN
  IF len = 0 THEN RETURN END;
  i:= 1;
  IF empty THEN
    fmt.print(dump, "%$2x", 0);
  ELSE
    length:= VAL(CARD, LEN(s.text^));
    IF (offset + len) > length THEN
      len:= length - offset;
    END;
    IF offset < length THEN
      fmt.print(dump, "%$2x", s.text[offset]);
    END;
  END;
  WHILE i < len DO
    IF empty THEN
      fmt.append(dump, " %$2x", 0);
    ELSE
      fmt.append(dump, " %$2x", s.text[offset+i]);
    END;
    i:= i + 1;
  END;
  s.out(offset, "  %-34s", dump);
END genHexDump;

(*----------------------------------------------------------------------*)
PROCEDURE genASCIIDump(s: objs.Segment; offset: CARD; len: CARD; empty:= FALSE: BOOLEAN);
VAR i: CARD;
    dump: ARRAY 40 OF CHAR;
BEGIN
  IF len = 0 THEN RETURN END;
  i:= 1;
  IF empty OR (VAL(CARD8, s.text[offset]) < 20H) THEN
    fmt.print(dump, ".");
  ELSE
    fmt.print(dump, "%c", s.text[offset]);
  END;
  WHILE i < len DO
    IF empty OR (VAL(CARD8, s.text[offset+i]) < 20H) THEN
      fmt.append(dump, ".");
    ELSE
      fmt.append(dump, "%c", s.text[offset+i]);
    END;
    i:= i + 1;
  END;
  s.out(offset, "%s", dump);
END genASCIIDump;

(*----------------------------------------------------------------------*)
PROCEDURE genFooter(s: objs.Segment);
BEGIN
  IF s.errors = 0 THEN
    s.out(,"\n No disassembly errors\n");
  ELSIF s.errors = 1 THEN
    s.out(,"\n 1 disassembly error\n");
  ELSE
    s.out(,"\n %d disassembly errors\n", s.errors);
  END;
END genFooter;

(*----------------------------------------------------------------------*)
PROCEDURE disasm_code_seg(s: objs.Segment);
VAR offset: CARD;
    len: CARD;
    instr: ARRAY 100 OF CHAR;
    ignore: ARRAY 1 OF CHAR;
    l: adt.Element;
    min_offset: CARD;

BEGIN
  IF s.text = NIL THEN RETURN END;
  offset:= 0;
  segment:= s;
  mem.data:= s.text;
  genSegHeader(s);
  WHILE offset < s.length DO
    genOffset(s, offset);

    IF NOT kd.Disasm(offset, FALSE, instr, ignore, len) THEN
      s.errors:= s.errors + 1;
      instr:= "; ??? cannot disassemble command";
      len := 1;
    END;

    -- find label within range (offset, offset+len)
    -- if some label is found, roll back to the previous command
    -- and start again from this label
    min_offset := MAX(CARD);
    s.labels.FindFirst (l);
    WHILE l # NIL DO
      WITH l: objs.Label DO
        IF (offset < l.offset) AND (l.offset < offset+len) THEN
          IF min_offset > l.offset THEN
            min_offset := l.offset;
          END;
        END;
      END;
      s.labels.FindNext (l);
    END;

    -- check if found
    IF min_offset # MAX(CARD) THEN
      IF kd.Disasm (offset, FALSE, instr, ignore, len, TRUE) THEN
        IF len <= min_offset - offset THEN
          -- some prefixes of command before the label, it's Ok
          fmt.append(instr, "; prefix of next command");
          min_offset := MAX(CARD);
        END;
      END;
    END;

    -- check if found and not prefix
    IF min_offset # MAX(CARD) THEN
      s.errors:= s.errors + 1;
      len := min_offset - offset;
      genHexDump(s, offset, len);
      instr:= "; ??? incomplete command due to re-synchronization";
      s.out_instr(offset, instr);
      offset := min_offset;
    ELSE
      genHexDump(s, offset, len);
      s.out_instr(offset, instr);
      offset:= offset + len;
    END;
  END;
  genFooter(s);
END disasm_code_seg;

(*----------------------------------------------------------------------*)
PROCEDURE disasm_data_seg(s: objs.Segment);
CONST max_bytes = 8;
VAR offset: CARD;
    name: ARRAY 100 OF CHAR;
    e: adt.Element;
    addrs: adt.Tree;
    l: objs.Label;
    label_off, addr_off: CARD;
    next_off: CARD;
    size: CARD;


  PROCEDURE next(t: adt.Tree; VAR offset: CARD);
  VAR e: adt.Element;
  BEGIN
    t.FindNext(e);
    IF e # NIL THEN
      offset:= e(objs.Text).offset;
    ELSE
      offset:= MAX(CARD);
    END;
  END next;

  PROCEDURE getAddrName(VAR size: CARD): BOOLEAN;
  VAR id: INT;
      l: objs.Label;
      e: adt.Element;
  BEGIN
    addrs.Backup(id);
    NEW(l); l.offset:= offset;
    addrs.Find(l, e);
    addrs.Restore(id);
    IF e # NIL THEN
      size:= ProcessFixup(s, e(objs.Label).fixup, name);
      lstr.Assign(name, e(objs.Label).text);
      RETURN TRUE;
    ELSE
      size:= 0;
      RETURN FALSE;
    END;
  END getAddrName;

BEGIN
  IF s.text = NIL THEN RETURN END;
  offset:= 0;
  segment:= s;

  (* Process fixups *)
  adt.NewTree(addrs);
  s.fixups.FindFirst(e);
  WHILE e # NIL DO
    NEW(l); l.offset:= e(objs.Fixup).offset;
    l.fixup:= e(objs.Fixup);
    addrs.Insert(l);
    s.fixups.FindNext(e);
  END;
  (* end *)

  s.labels.SetMin();
  next(s.labels, label_off);
  addrs.SetMin();
  next(addrs, addr_off);

  genSegHeader(s);
  WHILE offset < s.length DO

    (* Find out next offset *)
    IF getAddrName(size) THEN
      next_off:= offset + size;
    ELSE
      IF (offset + max_bytes) > s.length THEN
        next_off:= s.length;
      ELSE
        next_off:= offset + max_bytes;
      END;
      IF addr_off < label_off THEN
        IF addr_off <= next_off THEN
          next_off:= addr_off;
          next(addrs, addr_off);
        END;
      ELSIF label_off < addr_off THEN
        IF label_off <= next_off THEN
          next_off:= label_off;
          next(s.labels, label_off);
        END;
      ELSE
        IF label_off <= next_off THEN
          next_off:= label_off;
          next(s.labels, label_off);
          next(addrs, addr_off);
        END;
      END;
    END;

    (* Generate text *)
    IF next_off > offset THEN
      genOffset(s, offset);
      genHexDump(s, offset, next_off - offset);
      IF size > 0 THEN
        CASE size OF
          |2: s.out(offset, "DW         ");
          |4: s.out(offset, "DD         ");
          |6: s.out(offset, "DP         ");
        ELSE
          s.out(offset, "?? ");
        END;
        s.out_instr(offset, name);
      ELSE
        genASCIIDump(s, offset, next_off - offset);
        s.out(offset, "\n");
      END;
      offset:= next_off;
    END;
  END;
  genFooter(s);
END disasm_data_seg;

(*----------------------------------------------------------------------*)
PROCEDURE disasm_bss_seg(s: objs.Segment);
CONST max_bytes = 8;
VAR offset: CARD;
    label_off: CARD;
    next_off: CARD;


  PROCEDURE next(t: adt.Tree; VAR offset: CARD);
  VAR e: adt.Element;
  BEGIN
    t.FindNext(e);
    IF e # NIL THEN
      offset:= e(objs.Label).offset;
    ELSE
      offset:= MAX(CARD);
    END;
  END next;

BEGIN
  offset:= 0;
  segment:= s;

  s.labels.SetMin();
  next(s.labels, label_off);

  genSegHeader(s);
  WHILE offset < s.length DO

    (* Find out next offset *)
    IF (offset + max_bytes) > s.length THEN
      next_off:= s.length;
    ELSE
      next_off:= offset + max_bytes;
    END;
    IF label_off <= next_off THEN
      next_off:= label_off;
      next(s.labels, label_off);
    END;

    (* Generate text *)
    IF next_off > offset THEN
      genOffset(s, offset);
      genHexDump(s, offset, next_off - offset, TRUE);
      genASCIIDump(s, offset, next_off - offset, TRUE);
      s.out(offset, "\n");
      offset:= next_off;
    END;
  END;
  genFooter(s);
END disasm_bss_seg;

(*----------------------------------------------------------------------*)
PROCEDURE out(format-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
VAR s: ARRAY 1024 OF CHAR;
BEGIN
  IF op_out_to_file THEN
    fmt.print(s, format, x);
    file.WrStr(out_file, s);
  ELSE
    io.printf(format, x);
  END;
END out;

(*----------------------------------------------------------------------*)
PROCEDURE print_seg(s: objs.Segment);
VAR e, l: adt.Element;
BEGIN
  s.strings.FindFirst(e);
  WHILE e # NIL DO
    s.labels.Find(e, l);
    IF l # NIL THEN
      WITH l: objs.Label DO
        IF ~l.printed THEN
          out(l.fmt^, l.text^);
          l.printed:= TRUE;
        END;
      END;
    END;
    IF e(objs.Instr).label1 = NIL THEN
      out("%s", e(objs.Text).text^);
    ELSE
      out(e(objs.Instr).text^, e(objs.Instr).label1.text^,
                               e(objs.Instr).label2.text^,
                               e(objs.Instr).label3.text^);
      out("\n");
    END;
    s.strings.FindNext(e);
  END;
END print_seg;

(*----------------------------------------------------------------------*)
PROCEDURE CreateOutput(nm-: ARRAY OF CHAR);
VAR v: lstr.String;
    path, name, ext: lstr.String;
BEGIN
  cmdline.StrEquation(opts.list, v);
  op_out_to_file:= v^ # '';
  IF op_out_to_file THEN
    IF v^ = opts.magic THEN
      file.SplitName(nm, path, name, ext);
      file.CreateName('', name^, 'lst', v);
    END;
    IF ~file.Open(v^, file.crmode, out_file) THEN
      Error("Can't open file %s\n", v^);
    END;
  END;
END CreateOutput;


(*----------------------------------------------------------------------*)
PROCEDURE GenModuleHeader(m: objs.Module);
VAR e: adt.Element;

  PROCEDURE genGroup(g: objs.Group);
  VAR e: adt.Element;
      s: lstr.String;
  BEGIN
    objs.IndexToStr(g.idx, s);
    out("Group: '%s' ", s^);
    g.segs.FindFirst(e);
    IF e # NIL THEN
      objs.IndexToStr(e(objs.NameNode).idx, s);
      out("%s", s^);
      g.segs.FindNext(e);
    END;
    WHILE e # NIL DO
      objs.IndexToStr(e(objs.NameNode).idx, s);
      out(",%s", s^);
      g.segs.FindNext(e);
    END;
    out("\n");
  END genGroup;

BEGIN
  (* Generate module name *)
  out("\nModule %s\n", m.src_name^);

  (* Generate module groups *)
  m.groups.FindFirst(e);
  WHILE e # NIL DO
    genGroup(e(objs.Group));
    m.groups.FindNext(e);
  END;
END GenModuleHeader;


(*----------------------------------------------------------------------*)
PROCEDURE DisAssembly * (m: objs.Module; name-: ARRAY OF CHAR);
VAR e, e1: adt.Element;
    s: objs.Segment;
    nm: ARRAY 20 OF CHAR;
    ignore1: CARD;
    ignore2: ARRAY 100 OF CHAR;
BEGIN
  label_format:= "                               %s:\n";

  CreateOutput(name);

  GenModuleHeader(m);

  ProcessPublics(m);


  (* Disassembly code segments *)
  m.segs.FindFirst(e);
  WHILE e # NIL DO
    s:= e(objs.Segment);
    IF s.class = objs.CODE THEN
      disasm_code_seg(s);
    END;
    m.segs.FindNext(e);
  END;


  (* Process fixups for initialized data segments *)
  m.segs.FindFirst(e);
  WHILE e # NIL DO
    s:= e(objs.Segment);
    IF (s.class # objs.CODE) & (s.class # objs.BSS) THEN
      e(objs.Segment).fixups.FindFirst(e1);
      WHILE e1 # NIL DO
        ignore1:= ProcessFixup(e(objs.Segment), e1(objs.Fixup), ignore2);
        objs.InitLabels();
        e(objs.Segment).fixups.FindNext(e1);
      END;
    END;
    m.segs.FindNext(e);
  END;

  (* Disassembly initialized data segments *)
  m.segs.FindFirst(e);
  WHILE e # NIL DO
    s:= e(objs.Segment);
    IF (s.class # objs.CODE) & (s.class # objs.BSS) THEN
      disasm_data_seg(s);
    END;
    m.segs.FindNext(e);
  END;


  (* Disassembly uninitialized data segments *)
  m.segs.FindFirst(e);
  WHILE e # NIL DO
    s:= e(objs.Segment);
    IF s.class = objs.BSS THEN
      disasm_bss_seg(s);
    END;
    m.segs.FindNext(e);
  END;

  (* Assign label names *)
  m.segs.FindFirst(e);
  WHILE e # NIL DO
    s:= e(objs.Segment);
    s.labels.FindFirst(e1);
    WHILE e1 # NIL DO
      IF e1(objs.Label).text^ = '%s' THEN
        REPEAT
          fmt.print(nm, "L%d", s.mod.label_num);
          s.mod.label_num:= s.mod.label_num + 1;
        UNTIL ~objs.IsSuchName(nm);
        lstr.Assign(nm, e1(objs.Label).text);
      END;
      s.labels.FindNext(e1);
    END;
    m.segs.FindNext(e);
  END;


  (* Put text representation out *)
  m.segs.FindFirst(e);
  WHILE e # NIL DO
    s:= e(objs.Segment);
    print_seg(s);
    m.segs.FindNext(e);
  END;

  IF op_out_to_file THEN file.Close(out_file) END;
END DisAssembly;

BEGIN
  kd.ResolveAddr:= ResolveAddress;
  kd.ResolveEA  := ResolveEA;
END dasm.
