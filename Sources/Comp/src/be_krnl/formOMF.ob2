<*+ O2EXTENSIONS *>

<* NEW ALL_PUBLIC- *>  -- write LPUBDEF records for better disasm listing

MODULE formOMF;


<* IF TESTOBJ THEN *>  IMPORT io := opIO; <* END *>
IMPORT SYSTEM;
IMPORT pc  := pcK;
IMPORT cmd := CodeDef;
IMPORT fc  := CodeFace;
IMPORT env := xiEnv;
IMPORT xfs := xiFiles;
IMPORT fmt := FormStr;
IMPORT str := Strings;
IMPORT dstr:= DStrings;
IMPORT at  := opAttrs;
IMPORT nms := ObjNames;
IMPORT dbg := DbgFace;
IMPORT opt := Options;
IMPORT reg := Registry;
IMPORT xProfRTS;

<* IF DBG_EDIF THEN *>
IMPORT tc  := TimeConv;
<* END *>


VAR new_segment : BOOLEAN;   (* каждой процедуре - по отдельному сегменту *)
  cset : BOOLEAN;            (* OBJ-file - like IBM's CSET compiler *)

  gendebug         : BOOLEAN; -- generate debug info
  genlineno        : BOOLEAN; -- generate line numbers
  isHLL            : BOOLEAN; -- HLL debug info format
 <* IF DBG_EDIF THEN *>
  isEDIF           : BOOLEAN; -- EDIF debug info format
 <* END *>
  WasFileNameTable : BOOLEAN; -- write FileNameTable for HLL debug format


TYPE INT = LONGINT;


(* -------------------- OMF Record tags ---------------------- *)

CONST  THEADR  = 080X;   (* Translator Header Record *)
       MODEND  = 08BX;   (* Module End Record *)
       EXTDEF  = 08CX;   (* External Names Definition Record *)
       PUBDEF  = 091X;   (* Public Names Definition Record *)
       LINNUM  = 095X;   (* Line Numbers Record *)
       LNAMES  = 096X;   (* List of Name Records *)
       SEGDEF  = 099X;   (* Segment Definition Record *)
       GRPDEF  = 09AX;   (* Group Definition Record *)
       FIXUPP  = 09DX;   (* Fixup Record *)
       LEDATA  = 0A1X;   (* Logical Enumerated Data Record *)
       LPUBDEF = 0B7X;   (* Local Public Names Definition Record *)
--     COMDAT  = 0C2X;   (* Initialized Communal Data Records *)
--     LLNAMES = 0CAX;   (* Local Logical Names Definition Records *)
       MODNAME = 0C8H;   (* Comment class for sstModule & LI, XDS linker *)
       SRCNAME = 0C9H;   (* Comment class for sstModule, XDS linker *)
      <* IF DBG_EDIF THEN *>
       CONSKEY = 0EDH;   (* Comment class for consistent key, XDS linker *)
      <* END *>


<* IF DBG_EDIF THEN *>
VAR
  ConsistentKey*: ARRAY 64 OF CHAR;
  ReferenceFileName*: ARRAY 512 OF CHAR;
  DbgInfo*: ARRAY 512 OF CHAR;
<* END *>


PROCEDURE max(a,b: LONGINT): LONGINT;
BEGIN
  IF a>b THEN RETURN a ELSE RETURN b END;
END max;

CONST l_dword = 4;

PROCEDURE pre_mkal(VAR s: INT; al: INT);
  VAR r: INT;
BEGIN
  ASSERT((s>=0)&(al>0));
  r := s MOD al;
  IF r # 0 THEN
    INC(s, al-r);
  END;
END pre_mkal;

PROCEDURE pre_mktal(VAR s: INT; t: pc.STRUCT);
BEGIN
  pre_mkal(s, pc.code.get_align(t));
END pre_mktal;

(* ---------------------- Configuration ---------------------- *)
                       (* ------------- *)

VAR
  WRITE_TRG_OFFS: BOOLEAN;

<* NEW C_COMP_MOD *>

(*-------------------- Object addresses --------------------------*)
                    (* ---------------- *)
CONST a_addr = cmd.a_ready_code + 1;

TYPE ADDR_EXT = POINTER TO addr_ext_rec;

  addr_ext_rec = RECORD(at.attr_ext_rec)
                   seg:  INT;
                   offs: INT;
                 END;

PROCEDURE set_adr(o: pc.OBJECT; seg: INT; offs: INT);
  VAR adr_ext: ADDR_EXT;
BEGIN
  NEW(adr_ext);
  adr_ext.seg  := seg;
  adr_ext.offs := offs;
  at.app_obj_attr(o, adr_ext, a_addr);
  INCL(o^.marks, at.omark_gen_marked);
END set_adr;

PROCEDURE get_adr  <* IF DBG_EDIF THEN *> * <* END *>
                  (o: pc.OBJECT; VAR seg: INT; VAR offs: INT);
  VAR a: at.ATTR_EXT;
BEGIN
<* IF TESTOBJ THEN *>
  io.print("get_adr('%s')\n", o.name^);
<* END *>
  ASSERT(at.omark_gen_marked IN o.marks, 9999);
  a := at.attr(o.ext, a_addr);
  WITH a: ADDR_EXT DO
    seg := a.seg;
    offs := a.offs;
  END;
END get_adr;

(*----------------------  Output file  ---------------------------*)
                       (* ----------- *)
VAR obj_file: xfs.RawFile;

PROCEDURE create_obj_file(name-, ext-: ARRAY OF CHAR);
  VAR fn: pc.STRING;
BEGIN
  xfs.sys.Create('', name, ext, fn);
--  io.print("Create('%s', '%s', '%s')\n", name, ext, fn^);
  xfs.sys.UseFirst(fn^, fn);
--  io.print("UseFirst('%s')\n", fn^);
  xfs.raw.Open(fn^, TRUE);
  IF xfs.raw.file = NIL THEN
    env.errors.Fault(env.null_pos, 424, xfs.raw.msg^);
  END;
  obj_file := xfs.raw.file(xfs.RawFile);
END create_obj_file;

PROCEDURE close_obj_file;
  VAR err: pc.STRING;
BEGIN
  obj_file.CloseNew( TRUE(*env.errors.err_cnt=0*), FALSE, err);
  IF err # NIL THEN
    env.errors.Fault(env.null_pos, 432, err^);
  END;
  obj_file := NIL;
END close_obj_file;

(* -------------------- Low-Level Output -------------------- *)
                     (* ---------------- *)
VAR outbuf: ARRAY 4096 OF SYSTEM.BYTE;
    outcnt: LONGINT;

PROCEDURE flush;
BEGIN
  IF outcnt>0 THEN obj_file.WriteBlock(outbuf, 0, outcnt) END;
  outcnt := 0;
END flush;

PROCEDURE out(VAR data: ARRAY OF SYSTEM.BYTE; size: LONGINT);
  VAR req, i: LONGINT;
BEGIN
  ASSERT((0<=size) & (size<=LEN(data)));
  IF size = 0 THEN RETURN END;
  IF size > LEN(outbuf) THEN
    flush; obj_file.WriteBlock(data, 0, size); RETURN
  END;
  req := LEN(outbuf)-outcnt; IF size<req THEN req:=size END;
  IF req>0 THEN
(*     ADR(локала) > MAX(LONGINT) - переполнение при включенном контроле !!!
    SYSTEM.MOVE(SYSTEM.ADR(data[0]), SYSTEM.ADR(outbuf[outcnt]), SHORT(req));
    INC(outcnt,req);
*)
    FOR i := 0 TO req-1 DO
      outbuf[outcnt] := data[i]; INC(outcnt)
    END;
  END;
  DEC(size,req);
  IF size>0 THEN
    flush;
(*     ADR(локала) > MAX(LONGINT) - переполнение при включенном контроле !!!
    SYSTEM.MOVE(SYSTEM.ADR(data[req]), SYSTEM.ADR(outbuf[0]), SHORT(size));
    INC(outcnt,size);
*)
    FOR i := 0 TO size-1 DO
      outbuf[outcnt] := data[req+i]; INC(outcnt)
    END;
  END;
END out;

PROCEDURE BeginOutput(name-, ext-: ARRAY OF CHAR);
BEGIN
  create_obj_file(name, ext);
  outcnt:=0;
END BeginOutput;

PROCEDURE EndOutput;
BEGIN
  flush;
  close_obj_file;
END EndOutput;

CONST REC_LIMIT = 1024;
      REC_MAX = 1016;

VAR rec: ARRAY REC_LIMIT+16 OF SYSTEM.BYTE;
    reccnt: INT;

PROCEDURE rec_empty(): BOOLEAN;
BEGIN
  RETURN reccnt<=3
END rec_empty;

PROCEDURE rec_rem(): INT;
BEGIN
(*  RETURN REC_MAX-(reccnt-3)   *)
    RETURN REC_MAX-reccnt+3
END rec_rem;

PROCEDURE outb(b: LONGINT);
BEGIN
  rec[reccnt]:=SYSTEM.VAL(SYSTEM.BYTE,b);
  INC(reccnt);
END outb;

PROCEDURE outint(w: SYSTEM.INT16);
BEGIN
  cmd.move2b (SYSTEM.ADR (w), SYSTEM.ADR (rec[reccnt]), cmd.inverse_byte_order);
  INC(reccnt, 2);
END outint;

PROCEDURE outdw(dw: SYSTEM.INT32);
BEGIN
  cmd.move4b (SYSTEM.ADR (dw), SYSTEM.ADR (rec[reccnt]), cmd.inverse_byte_order);
  INC(reccnt, 4);
END outdw;

PROCEDURE out_index(inx: INT);
BEGIN
  ASSERT((0<=inx) & (inx<=07FFFH));
  IF inx<=07FH THEN
       outb(inx);
  ELSE outb(inx DIV 100H + 80H); outb(inx MOD 100H);
  END;
END out_index;

PROCEDURE outbin(src: SYSTEM.ADDRESS; len: INT);
BEGIN
  IF len = 0 THEN RETURN END;
  SYSTEM.MOVE(src, SYSTEM.ADR(rec[reccnt]), len);
  INC(reccnt, len);
END outbin;

PROCEDURE out_str(s-: ARRAY OF CHAR);
BEGIN
  outbin(SYSTEM.ADR(s[0]), LENGTH(s));
END out_str;

PROCEDURE out_name(name-: ARRAY OF CHAR);
  VAR len: INT;
BEGIN
  len := LENGTH(name); ASSERT(len<=255);
  outb(len);
  outbin(SYSTEM.ADR(name[0]), len);
END out_name;

PROCEDURE out_longname(name-: ARRAY OF CHAR);
  VAR len: SYSTEM.INT16;
BEGIN
  len := SHORT(LENGTH(name));
  outint(len);
  outbin(SYSTEM.ADR(name[0]), len);
END out_longname;

PROCEDURE begin(tag: SYSTEM.BYTE);
BEGIN
<* IF TESTOBJ THEN *> io.print("\nbegin:%2$X\n", tag); <* END *>
  rec[0] := tag; rec[1] := 0; rec[2] := 0;
  reccnt:=3;
END begin;

PROCEDURE begin_comment(class: INT; purge: BOOLEAN; list: BOOLEAN);
  VAR type: INT;
BEGIN
  IF purge THEN
    type := 0;
  ELSE
    type := 128;
  END;
  IF NOT list THEN
    INC(type, 64);
  END;
  begin (088X);
  outb (type);
  outb (class);
END begin_comment;

PROCEDURE end;
  VAR i: INT; crc: INT; 
    len: SYSTEM.INT16; (*!!*)
BEGIN
  len := VAL (SYSTEM.INT16, reccnt-2);
<* IF TESTOBJ THEN *>
  io.print("end: %2$X (len = %d)\n", rec[0], len);
<* END *>
  IF rec_empty() THEN RETURN END;
  cmd.move2b (SYSTEM.ADR (len), SYSTEM.ADR (rec[1]), cmd.inverse_byte_order);
  crc := 0;
  FOR i := 0 TO reccnt-1 DO
    INC(crc, ORD(SYSTEM.VAL(CHAR, rec[i])));
  END;
  outb(100H - crc MOD 100H);
  out(rec,reccnt);
END end;

PROCEDURE get_fullname(VAR fullname: ARRAY OF CHAR);
VAR
  fname: xfs.String;
BEGIN
  xfs.sys.ConvertToHost(env.info.file^, fname);
  xfs.sys.GetFullPathName(fname^, fullname);
END get_fullname;


PROCEDURE get_fullmodname <* IF DBG_EDIF THEN *> * <* END *>
                          (mod: pc.OBJECT; VAR fullname: ARRAY OF CHAR);
VAR
  fname: xfs.String;
  line, col: LONGINT;
BEGIN
  mod.end.unpack(fname, line, col);
  xfs.sys.ConvertToHost(fname^, fname);
  xfs.sys.GetFullPathName(fname^, fullname);
END get_fullmodname;


<* IF DBG_EDIF THEN *>

PROCEDURE MakeConsistentKey ();
BEGIN
  fmt.print (ConsistentKey, "%$8X", env.config.GetTimeStamp());
END MakeConsistentKey;


PROCEDURE MakeReferenceFileName * (name-, ext-: ARRAY OF CHAR);
VAR
  fname, dname: xfs.String;
  path: ARRAY 512 OF CHAR;
BEGIN
  xfs.sys.Create ('', name, ext, fname);
  xfs.sys.ConvertToHost (fname^, fname);
  xfs.sys.UseFirstDir (fname^, dname);
  dstr.Append ("\", dname);
  xfs.sys.ConvertToHost (dname^, dname);
  xfs.sys.GetFullPathName (dname^, path);
  fmt.print (ReferenceFileName, "%s;%s", path, fname^);
END MakeReferenceFileName;

<* END *>



-------------  H e a d e r  -------------------------------------
             ---------------

PROCEDURE WriteHeader(mod: pc.OBJECT);
  VAR vers: ARRAY 80 OF CHAR;
      fullname: ARRAY 512 OF CHAR;
      emit: reg.ITEM;
BEGIN
  emit := reg.GetActive (opt.dbgFormat);

  begin(THEADR);
    get_fullname(fullname);
    out_name(fullname);
  end;

  begin_comment(0,TRUE,TRUE);
    fmt.print(vers, " XDS %s [%s]", pc.pars.vers, pc.code.vers);
    out_name(vers); outb(0);
  end;

  IF gendebug OR genlineno THEN
    IF isHLL THEN  (* --- write 4HL comment *)
      IF emit.name = opt.dbg_HLL THEN
        begin_comment(0A1H, TRUE, TRUE);
          out_str(4C+"HL");
        end;
      END;
      IF emit.name = opt.dbg_XHLL THEN
        begin_comment(0A1H, TRUE, TRUE);
          out_str(105C+"HL");
        end;
      END;
      WasFileNameTable := FALSE;
    END;
  END;

  -- write Comment for module name, XDS linker supported
  begin_comment (MODNAME, TRUE, TRUE);
  out_str(mod.name^);
  outb(0);
  end;

  IF gendebug OR genlineno THEN
    begin_comment (SRCNAME, TRUE, TRUE);
    get_fullmodname(mod, fullname);
    out_str(fullname);
    outb(0);
    end;
  END;
END WriteHeader;

--------------  N a m e  L i s t  -------------------------------
              --------------------

CONST inx_empty     = 1;
      inx_DGROUP    = 2;
      inx__TEXT     = 3;
      inx_CODE      = 4;
      inx__DATA     = 5;
      inx_DATA      = 6;
      inx_CONST     = 7;
      inx__BSS      = 8;
      inx_BSS       = 9;

      inx_FLAT      = 10;
      inx_CODE32    = inx__TEXT;
      inx_DATA32    = inx__DATA;
      inx_CONST32   = 11;
      inx_STACK     = 12;

      inx_ssTYPES   = 13;
      inx_DEBTYP    = 14;
      inx_ssSYMBOLS = 15;
      inx_DEBSYM    = 16;

PROCEDURE WriteNames;
  TYPE SEG_NAME = ARRAY 64 OF CHAR;
  VAR s: pc.STRING;
    code_name: SEG_NAME;      (* Code segment name *)
    data_name: SEG_NAME;      (* Data segment name *)
BEGIN
  env.config.Equation("CODENAME", s);
  IF (s # NIL) & (s[0]#0X) THEN
    COPY(s^, code_name);
    str.Capitalize(code_name);
  ELSE
    COPY("_TEXT", code_name);
  END;

  env.config.Equation("DATANAME", s);
  IF (s # NIL) & (s[0]#0X) THEN
    COPY(s^, data_name);
    str.Capitalize(data_name);
  ELSE
    COPY("_DATA", data_name);
  END;

  begin(LNAMES);

  out_name(""+0C);                  (* inx_empty  = 1 *)
  out_name("DGROUP");               (* inx_DGROUP = 2 *)
  out_name(code_name);              (* inx__TEXT  = 3 *)
  out_name("CODE");                 (* inx_CODE   = 4 *)
  out_name(data_name);              (* inx__DATA  = 5 *)
  out_name("DATA");                 (* inx_DATA   = 6 *)
  out_name("CONST");                (* inx_CONST  = 7 *)
  out_name("_BSS");                 (* inx__BSS   = 8 *)
  out_name("BSS");                  (* inx_BSS    = 9 *)

  out_name("FLAT");                 (* inx_FLAT   = 10*)
  out_name("CONST32");              (* inx_CONST32= 11*)
  out_name("STACK");                (* inx_STACK  = 12*)


  IF gendebug THEN
   <* IF DBG_EDIF THEN *>
    IF NOT isEDIF THEN
   <* END *>
      out_name("$$TYPES");            (* inx_ssTYPES   = 13 *)
      out_name("DEBTYP");             (* inx_DEBTYP    = 14 *)
      out_name("$$SYMBOLS");          (* inx_ssSYMBOLS = 15 *)
      out_name("DEBSYM");             (* inx_DEBSYM    = 16 *)
   <* IF DBG_EDIF THEN *>
    END;
   <* END *>
  END;

  end;
END WriteNames;

<* PUSH *>
<* WOFF301+ *>
PROCEDURE type_index(o: pc.OBJECT) : INTEGER;
BEGIN
  RETURN 0
  (* так какой же на самом деле номер первого непримитивного типа:      *)
  (*  или   200H - смотри описание OMF, Appendix 1                      *)
  (*  или  1000H - смотри описание Symbol and Type Info, п. 1.4 и 5.1   *)
END type_index;
<* POP *>

--------------  S e g m e n t s  ---------------------------------
              -------------------
CONST
  SegExtern = 00;
  SegCode   = 01;
  SegUdat   = 02;
  SegIdat   = 03;
  SegConst  = 04;
  LastSeg = SegConst;
(*
  SegSystem = 05;
  SegStack  = 06;
  LastSeg = SegStack;
*)

VAR
  FirstNStdSeg : INTEGER;
  SegCnt       : INTEGER;
  SegSizes: ARRAY LastSeg+1 OF LONGINT;

PROCEDURE out_segment(SegInx: INTEGER; ClassInx: INTEGER; Size: LONGINT;
                      Align: INT; Combine: INT);
  VAR attr: INT;
BEGIN
  attr:=(Align MOD 8)*32 + (Combine MOD 8)*4 + 1 (* P bit is set *);
  (* B bit is never set *)
  begin(SEGDEF);
  outb(attr); outdw(Size);
  out_index(SegInx); out_index(ClassInx); out_index(inx_empty);
  end;
END out_segment;

CONST
  BYTE_ALIGN = 1;
  PARA_ALIGN = 3;
  DW_ALIGN   = 5;

  PRIV_COMB  = 0;
  PUBL_COMB  = 2;
  STACK_COMB = 5;

  PARA_LEN = 16; (* Paragraph length in bytes *)

PROCEDURE proc_segment(o: pc.OBJECT);
  VAR seg: INT; offs: INT;
    rsegm: cmd.CODE_SEGM;
    seg_size : LONGINT;
    seg_align: INT;
BEGIN
  IF (o.mno = at.curr_mno) & (at.omark_gen_marked IN o.marks) THEN
    get_adr(o, seg, offs);
    IF seg >= FirstNStdSeg THEN
      rsegm := cmd.get_ready(o);
      seg_size := rsegm.code_len;
      IF NOT (at.SPACE IN at.COMP_MODE) THEN pre_mkal(seg_size, l_dword) END;
      IF at.SPACE IN at.COMP_MODE THEN seg_align := BYTE_ALIGN;
      ELSE                             seg_align := PARA_ALIGN;
      END;
      out_segment(inx_CODE32, inx_CODE, seg_size, seg_align, PUBL_COMB);
    END;
  END;
END proc_segment;

PROCEDURE WriteSegments;
  VAR seg_align: INT;
BEGIN
  IF (at.TARGET = at.trg_OS2) & cset THEN
    out_segment(inx_CODE32, inx_CODE, SegSizes[SegCode],
                               PARA_ALIGN, PUBL_COMB);                  (*1*)
    pre_mkal(SegSizes[SegUdat],PARA_LEN);
    out_segment(inx_DATA32, inx_DATA, SegSizes[SegUdat],
                               PARA_ALIGN,PUBL_COMB);                   (*2*)
    pre_mkal(SegSizes[SegIdat],PARA_LEN);
    out_segment(inx_CONST32,inx_CONST,SegSizes[SegIdat],
                               PARA_ALIGN,PUBL_COMB);                   (*3*)

    (* DGroup definition *)
    begin(GRPDEF);
    out_index(inx_DGROUP);
    outb(0FFH); out_index(2);
    outb(0FFH); out_index(3);
    end;
    begin(GRPDEF); out_index(inx_FLAT); end;
  ELSE
    IF at.SPACE IN at.COMP_MODE THEN seg_align := BYTE_ALIGN;
    ELSE                             seg_align := PARA_ALIGN;
    END;
    out_segment(inx__TEXT, inx_CODE, SegSizes[SegCode], seg_align, PUBL_COMB); (*1*)
    out_segment(inx__BSS , inx_BSS  ,SegSizes[SegUdat], seg_align, PUBL_COMB); (*2*)
    out_segment(inx__DATA, inx_DATA, SegSizes[SegIdat], seg_align, PUBL_COMB); (*3*)
    out_segment(inx_CONST, inx_CONST,SegSizes[SegConst],seg_align, PUBL_COMB); (*4*)
    IF at.main & (at.TARGET = at.trg_OS2) THEN
      out_segment(inx_STACK, inx_STACK, at.stk_lim, DW_ALIGN, STACK_COMB); (*5*)
    END;

    (* DGroup definition *)
    begin(GRPDEF);
    out_index(inx_DGROUP);
    outb(0FFH); out_index(4);
    outb(0FFH); out_index(3);
    outb(0FFH); out_index(2);
    end;
    IF at.TARGET = at.trg_OS2 THEN
      begin(GRPDEF); out_index(inx_FLAT); end;
    END;
  END;

  IF new_segment THEN cmd.iter_context(at.curr_mod, proc_segment) END;

END WriteSegments;

---------------------- Objects Allocation ----------------------------
                       ------------------
VAR start_adr : LONGINT;

PROCEDURE allocate(o: pc.OBJECT);

  PROCEDURE alloc(seg: INTEGER);
    VAR rsegm: cmd.CODE_SEGM;
  BEGIN
    IF at.omark_gen_marked IN o.marks THEN RETURN END;
    IF at.omark_gen_ready IN o.marks THEN
<* IF TESTOBJ THEN *>  io.note("alloc",o);  <* END *>
      IF (seg = SegCode) & new_segment THEN
        set_adr(o, SegCnt, 0);
        INC(SegCnt);
      ELSE
        IF NOT (at.SPACE IN at.COMP_MODE) THEN
          pre_mkal(SegSizes[seg], l_dword);   -- выравнивание
        END;
        set_adr(o, seg, SegSizes[seg]);
        rsegm := cmd.get_ready(o);
        INC(SegSizes[seg], rsegm.code_len);
      END;
    END;
  END alloc;

VAR seg : INT;

BEGIN
<* IF TESTOBJ THEN *>  io.note("allocate",o);  <* END *>
  CASE o^.mode OF
  | pc.ob_var:
      IF (o.lev # 0) OR (at.omark_gen_marked IN o.marks) THEN RETURN END;
      IF at.omark_gen_ready IN o.marks THEN
        alloc(SegIdat);
      ELSE
        pre_mktal(SegSizes[SegUdat], o.type);
        set_adr(o, SegUdat, SegSizes[SegUdat]);
        INC(SegSizes[SegUdat], pc.code.get_size(pc.su_bytes, o.type));
      END;

  | pc.ob_varpar:

  | pc.ob_proc, pc.ob_xproc, pc.ob_lproc:
      alloc(SegCode);

  | pc.ob_eproc, pc.ob_cproc: (* ничего не делать *)

  | pc.ob_module :
      IF (o.mno = at.curr_mno) & ((o.flag IN opt.LangsWithModuleConstructors)OR(at.profilingMode # xProfRTS.PROF_MODE_NONE)) THEN
        alloc(SegCode);
        get_adr(o, seg, start_adr);
      END;

  | pc.ob_cons :
      IF o.mno = at.curr_mno THEN alloc(SegIdat) END;

  | pc.ob_type:
      IF o.mno = at.curr_mno THEN alloc(SegIdat) END;

  | pc.ob_label: (* ничего не делать *)

  ELSE ASSERT(FALSE,100h+ORD(o^.mode));
  END;
END allocate;


------------------- Ready Segment Generation -------------------------
                    ------------------------

PROCEDURE out_ledata(SegInx: INT; SegOffs: LONGINT;
                     Data-: ARRAY OF SYSTEM.BYTE;
                     DataOffs: LONGINT; DataLen: LONGINT);
BEGIN
  begin(LEDATA);
  out_index(SegInx); outdw(SegOffs);
  outbin(SYSTEM.ADR(Data[DataOffs]), DataLen);
  end;
END out_ledata;

PROCEDURE out_xrefs(seg: INT; offs: LONGINT; Seg: cmd.CODE_SEGM);
VAR
  i,n: INT;
  fname: pc.STRING;
  predln, ln, pos: LONGINT;
  predof, of: LONGINT;
  xrefs: cmd.XREFs;
  fullname: ARRAY 512 OF CHAR;
  num: INTEGER;
  xref_len: INTEGER;
  emit: reg.ITEM;

BEGIN
<* IF TESTOBJ THEN *>
  io.print("**** out_xref **** (seg = %d, offs = %d)\n", seg, offs);
<* END *>

  IF isHLL THEN
    xref_len := 8;
  ELSE
    xref_len := 6;
  END;

  n := Seg.xref_len;  ASSERT(n # 0);
  xrefs := Seg.xref;
  predln := -1;  predof := -1;


  IF isHLL AND NOT WasFileNameTable THEN  (* --- write FileNameTable *)
    WasFileNameTable := TRUE;
    get_fullname(fullname);

    begin(LINNUM);
     outb(0); out_index(0);
     outint(0); outb(3); outb(0);
     outint(0); outint(0);
     emit := reg.GetActive (opt.dbgFormat);
     IF emit.name = opt.dbg_XHLL THEN
       outdw(12+2+LENGTH(fullname));
     ELSE
       outdw(12+1+LENGTH(fullname));
     END;
     outdw(0); outdw(0); outdw(1);
     IF emit.name = opt.dbg_XHLL THEN
       out_longname(fullname);
     ELSE
       out_name(fullname);
     END;
    end;
  END;

  begin(LINNUM);  outb(0);  out_index(seg);

  IF isHLL THEN

    num := 0;
    FOR i := 0 TO n-1 DO
      xrefs[i].txtpos.unpack(fname, ln, pos);
      IF (ln # predln) & (xrefs[i].offs # predof) THEN
        INC(num);
        predln := ln;
        predof := xrefs[i].offs;
      END;
    END;
    predln := -1;  predof := -1;

    outint(0); outb(0); outb(0);

    outint(num); outint(0);
    outdw(0);
  END;

  FOR i := 0 TO n-1 DO
    xrefs[i].txtpos.unpack(fname, ln, pos);
    of := xrefs[i].offs + offs;

<* IF TESTOBJ THEN *>
    io.print("  [line = %d, offs = %x)\n", ln, of);
<* END *>

    INC(ln);                   (*  -- т.к. front-end считает номера строк с 0 *)
    IF (ln # predln) & (of # predof) THEN
      IF rec_rem() < xref_len THEN  (* -- начать новую запись -- *)
        end;
        begin(LINNUM); out_index(0); out_index(seg);
      END;
      outint(SHORT(ln));
      IF isHLL THEN
        outint(1);
      END;

      outdw(of);
      predln := ln;
      predof := of;
    END;
  END;

  end;
END out_xrefs;

CONST FIXUP_MAX = 11;

TYPE fixup = RECORD
                offs : INT;        (* in LEDATA record *)
                lkind: SHORTINT;   (* Location kind *)
                self : BOOLEAN;
                fmeth: SHORTINT;
                tmeth: SHORTINT;
                finx : INTEGER;
                tinx : INTEGER;
                toffs: LONGINT;
             END;

PROCEDURE clear_fixup(VAR fx: fixup);
BEGIN
  fx.offs :=0;
  fx.lkind:=0;
  fx.self :=FALSE;
  fx.fmeth:=0; fx.tmeth:=0;
  fx.finx :=0; fx.tinx :=0;
  fx.toffs:=0;
END clear_fixup;

PROCEDURE out_fixup(fx-: fixup; PartOffs: INT);
  VAR B,O,TM: INT;
BEGIN
  IF rec_rem() < FIXUP_MAX THEN end; begin(FIXUPP) END;
  O:=(fx.offs-PartOffs); ASSERT(O<1024);
  B:=(O DIV 256) + (fx.lkind MOD 16)*4 + 128;
  IF NOT fx.self THEN B:=B+64 END; (* Mode bit is set *)
  outb(B); outb(O MOD 256);
  TM:=fx.tmeth;
  IF NOT WRITE_TRG_OFFS OR (fx.toffs=0) THEN TM:=TM+4 END;
  outb((TM MOD 8) + (fx.fmeth MOD 8)*16);
  IF fx.finx>0 THEN out_index(fx.finx) END;
  IF fx.tinx>0 THEN out_index(fx.tinx) END;
  IF WRITE_TRG_OFFS & (fx.toffs#0) THEN outdw(fx.toffs) END;
END out_fixup;

PROCEDURE fxup_length(fx-: cmd.fixup_desc): INTEGER;
BEGIN
  IF fx.kind = cmd.fx_obj32far THEN RETURN 6
  ELSE                              RETURN 4
  END;
END fxup_length;

CONST  (* --- g r o u p   i n d e x e s  --- *)
  DGROUP_INX  = 1;
  FLATGRP_INX = 2;

CONST  (* --- f r a m e   m e t h o d s  --- *)
  FM_SEGM   = 0;
  FM_GROUP  = 1;
  FM_TARGET = 5;

CONST  (* --- t a r g e t   m e t h o d s  --- *)
  TM_SEGM   = 0;
  TM_EXTERN = 2;

PROCEDURE GenReadySegm(SegInx: INT; Adr: INT; Seg: cmd.CODE_SEGM);

  CONST PART_LIMIT = 1000;

  TYPE FX_PAIR = RECORD (fixup)
                   src_no: INT
                 END;

  VAR FxTab: POINTER TO ARRAY OF FX_PAIR;
      fst_fx, lst_fx: INT;

  VAR SegRem,PartOffs,PartLen,LastFxNo: LONGINT;

  PROCEDURE mk_fxtab(list: cmd.FIXUPs; list_len: LONGINT);

    VAR RSAddr: LONGINT;

    PROCEDURE do_qsort(l,r: LONGINT);
      VAR i,j,ix: LONGINT; tmp: INT;
    BEGIN
      i:=l; j:=r;
      ix:=(l+r) DIV 2;
      REPEAT
        WHILE list[FxTab[i].src_no].offs < list[FxTab[ix].src_no].offs DO INC(i) END;
        WHILE list[FxTab[ix].src_no].offs < list[FxTab[j].src_no].offs DO DEC(j) END;
        IF i<=j THEN
          IF i#j THEN
            IF ix=i THEN ix:=j ELSIF ix=j THEN ix:=i END;
            tmp := FxTab[i].src_no;
            FxTab[i].src_no := FxTab[j].src_no;
            FxTab[j].src_no := tmp;
          END;
          INC(i); DEC(j);
        END;
      UNTIL i>j;
      IF l<j THEN do_qsort(l,j) END;
      IF i<r THEN do_qsort(i,r) END;
    END do_qsort;

    PROCEDURE apply_fixup(fx_adr: LONGINT; (* адрес для правки в строке кода *)
                          inf:    LONGINT  (* сама поправка *)
                         );
      VAR zz: SYSTEM.INT32;
    BEGIN
      zz := 0;
      cmd.move4b (fx_adr, SYSTEM.ADR (zz), cmd.inverse_byte_order);
      zz := zz + inf;                      (* add the offset,          *)
                                           (* and place it back...     *)
      cmd.move4b (SYSTEM.ADR (zz), fx_adr,  cmd.inverse_byte_order);
    END apply_fixup;

    PROCEDURE translate(VAR pair: FX_PAIR);

      PROCEDURE IsData(o: pc.OBJECT): BOOLEAN;
      BEGIN
        RETURN
          NOT (o^.mode IN pc.PROCs + pc.OB_SET{ pc.ob_label, pc.ob_sproc, pc.ob_module} )
      END IsData;

      VAR N: INT; seg: INT; addr: LONGINT; dgrp: BOOLEAN;
         o: pc.OBJECT;
    BEGIN (* --- t r a n s l a t e --- *)
      N := pair.src_no; clear_fixup(pair);
      o := list[N].obj;
      get_adr(o, seg, addr);
      dgrp := IsData(o);
      pair.offs := list[N].offs;
      pair.self := (list[N].kind=cmd.fx_relcall);
      IF (list[N].kind = cmd.fx_obj32far) THEN pair.lkind := 11;
      ELSE                                     pair.lkind := 9;
      END;
      IF at.TARGET = at.trg_OS2 THEN
           pair.fmeth := FM_GROUP; pair.finx := FLATGRP_INX;
           IF seg = SegExtern THEN
--           IF dgrp THEN pair.fmeth := FM_GROUP; pair.finx := FLATGRP_INX;
--           ELSE         pair.fmeth := FM_TARGET;
--           END;
             pair.tmeth := TM_EXTERN; pair.tinx := SHORT(addr);
             pair.toffs := list[N].fx_offs;
           ELSE
--           pair.fmeth := FM_GROUP; pair.finx := FLATGRP_INX;
             pair.tmeth := TM_SEGM; pair.tinx := SHORT(seg);
             pair.toffs := addr + list[N].fx_offs;
           END;
      ELSE IF seg = SegExtern THEN
             IF dgrp THEN pair.fmeth := FM_GROUP; pair.finx := DGROUP_INX;
             ELSE         pair.fmeth := FM_TARGET;
             END;
             pair.tmeth := TM_EXTERN; pair.tinx := SHORT(addr);
             pair.toffs := list[N].fx_offs;
           ELSE
             IF dgrp THEN pair.fmeth := FM_GROUP; pair.finx := DGROUP_INX;
             ELSE         pair.fmeth := FM_SEGM;  pair.finx := SHORT(seg);
             END;
             pair.tmeth := TM_SEGM; pair.tinx := SHORT(seg);
             pair.toffs := addr + list[N].fx_offs;
           END;
      END;
      IF ~WRITE_TRG_OFFS THEN
        apply_fixup (RSAddr + SYSTEM.VAL(LONGINT,list[N].offs), pair.toffs);
      END;
    END translate;

    VAR n: INT; cnt,toffs: LONGINT; tseg: INT;
      fx_adr: LONGINT; (* адрес для правки в строке кода *)

  BEGIN (* --- m k _ f x t a b --- *)
    LastFxNo:= -1;
    IF list = NIL THEN ASSERT(list_len = 0); RETURN END;
    RSAddr:=SYSTEM.VAL(LONGINT,SYSTEM.ADR(Seg^.bcode[0]));
    cnt := list_len;
    NEW(FxTab, cnt);
    FOR n := 0 TO cnt-1 DO
      IF list[n].kind = cmd.fx_relcall THEN
        get_adr(list[n].obj, tseg, toffs);
        IF tseg = SegInx THEN (* target object is from the same segment *)
          toffs := toffs + list[n].fx_offs - Adr - list[n].offs - l_dword;
          fx_adr := RSAddr + SYSTEM.VAL(LONGINT,list[n].offs);
          apply_fixup (fx_adr, toffs);
        ELSE INC(LastFxNo); FxTab[LastFxNo].src_no := n;
        END;
      ELSE INC(LastFxNo); FxTab[LastFxNo].src_no := n;
      END;
    END;
    IF LastFxNo>0 THEN do_qsort(0,LastFxNo) END;
    FOR cnt:=0 TO LastFxNo DO translate(FxTab[cnt]) END;
  END mk_fxtab;

  PROCEDURE calc_part(list: cmd.FIXUPs);
    (* Out => PartLen, fst_fx, lst_fx *)
    VAR Size,fxno,offs: LONGINT; N: INT;
  BEGIN
    fst_fx := lst_fx + 1;
    IF SegRem <= PART_LIMIT THEN
      PartLen := SegRem; lst_fx := LastFxNo; RETURN
    END;
    Size := 0; fxno := fst_fx;
    LOOP
      IF fxno>LastFxNo THEN
        PartLen := max(Size,PART_LIMIT); lst_fx := fxno - 1; RETURN
      END;
      N := FxTab[fxno].src_no;
      offs := list[N].offs - PartOffs;
      IF offs > PART_LIMIT THEN
        PartLen:=max(Size,PART_LIMIT); lst_fx := fxno-1; RETURN
      ELSE Size := offs + fxup_length(list[N]); INC(fxno);
      END;
    END;
  END calc_part;

  VAR i: INT;
BEGIN
  IF Seg.code_len = 0 THEN RETURN END;
  mk_fxtab(Seg.fxup, Seg.fxup_len);
  PartOffs := 0; SegRem := Seg.code_len; lst_fx := -1;
  WHILE SegRem > 0 DO
     calc_part(Seg.fxup);
     out_ledata(SegInx, Adr+PartOffs, Seg.bcode^, PartOffs, PartLen);
     begin(FIXUPP);
     IF fst_fx >= 0 THEN
       FOR i:=fst_fx TO lst_fx DO out_fixup(FxTab^[i], PartOffs) END;
     END;
     end;
     INC(PartOffs,PartLen); DEC(SegRem,PartLen);
  END;
(* -- Now it's done by another iteration of program objects
  IF (Seg.xref # NIL) & ({at.lineno,at.history}*at.COMP_MODE # {}) THEN
    out_xrefs(SegInx, Adr, Seg)
  END;
*)
END GenReadySegm;

PROCEDURE place(o: pc.OBJECT);
  VAR seg: INT; offs: INT;
      rsegm: cmd.CODE_SEGM;
BEGIN
<* IF TESTOBJ THEN *>  io.note("place",o);  <* END *>
  IF (o.mno = at.curr_mno) & (at.omark_gen_ready IN o.marks)
     & (o.mode # pc.ob_cproc)
  THEN
    get_adr(o, seg, offs);
    rsegm := cmd.get_ready(o);
    GenReadySegm(seg, offs, rsegm);
  END;
END place;

PROCEDURE GenLinNum(o: pc.OBJECT);
VAR
  seg: INT; offs: INT;
  rsegm: cmd.CODE_SEGM;
BEGIN
  IF (o.mno = at.curr_mno) & (at.omark_gen_ready IN o.marks) & (o.mode # pc.ob_cproc) THEN
    get_adr(o, seg, offs);
    rsegm := cmd.get_ready(o);
    IF (rsegm.code_len # 0) & (rsegm.xref # NIL) THEN
      out_xrefs(seg, offs, rsegm)
    END;
  END;
END GenLinNum;


(* ---------------------- Tables Generation ---------------------- *)
                       (* ----------------- *)

VAR
  ExtCnt     :INTEGER;
  makeNewRec :BOOLEAN;

PROCEDURE out_extern (o :pc.OBJECT);
  VAR
    Cname, name :ARRAY 255 OF CHAR;
    dllName     :pc.STRING;
    len         :INT;

  PROCEDURE makeIMP;
  BEGIN
    begin(088X); out_index(0); outb(0A0h); out_index(01h); -- IMPDEF comment class
      outb(0);                                             -- imported by name
      out_name(name);
      out_name( dllName^ );
      <* IF C_COMP_MOD THEN *>
        out_name(Cname);
      <* ELSE *>
        outb(0);          (* internal name is the same as imported *)
      <* END *>
  END makeIMP;

BEGIN
  <* IF TESTOBJ THEN *> io.note("out_extern",o); <* END *>
  nms.makename(o, name);
  nms.makeCname(o,Cname);
  len := LENGTH(name);

  IF makeNewRec OR (rec_rem() < (len+3)) THEN
    makeNewRec := FALSE;
    end;
    begin(EXTDEF)
  END;
  out_name(name); out_index(type_index(o));
  INC(ExtCnt);
  set_adr(o, SegExtern, ExtCnt);

  IF ~env.config.Option("IMPLIB") & at.get_dllexported(o) THEN
    dllName := at.get_name( o, pc.dllname_id );
    IF (dllName # NIL) THEN
      IF (at.GENDLL IN at.COMP_MODE) & at.isSameDLL(o) THEN
        RETURN
        (* it's the same DLL - don't generate IMPDEF *)
      END;
      end; (* EXTDEF *)

      makeIMP();
     <* IF env_target = "x86nt" THEN *>
      IF at.get_dllexported(o) & ( ( o^.mode=pc.ob_var )  OR
                                   ( o^.mode=pc.ob_cons ) OR
                                   ( o^.mode=pc.ob_type ) )  THEN
        end; (* IMPDEF *)
       (* XDS feature  that allows to export variables from DLL under Win32 *)
        str.Append ("@var153", name);
        str.Append ("@var153", Cname);
        makeIMP();
      END;
      <* END *>
      makeNewRec := TRUE; (* close this IMPDEF and establish a new EXTDEF, if any *)
    END;
  END;
END out_extern;

PROCEDURE collect_externals (o :pc.OBJECT);
  VAR rsegm :cmd.CODE_SEGM;
      fx    :cmd.FIXUPs;
      trg   :pc.OBJECT;
      i     :INT;
BEGIN
<* IF TESTOBJ THEN *>  io.note("collect_externals",o);  <* END *>
  IF (o^.mno = at.curr_mno) & (at.omark_gen_ready IN o^.marks) THEN
    rsegm := cmd.get_ready(o);
    fx := rsegm^.fxup;
    IF fx # NIL THEN
      FOR i := 0 TO rsegm.fxup_len-1 DO
        trg := fx[i].obj;   -- io.note("  trg=", trg);
        IF ((trg^.mno # at.curr_mno) OR (trg.mode = pc.ob_eproc))
          & NOT (at.omark_gen_marked IN trg^.marks)
        THEN
          out_extern(trg);
        END;
      END;
    END;
  END;
END collect_externals;

PROCEDURE WriteExternals;
BEGIN
  ExtCnt     := 0;
  makeNewRec := FALSE;
  begin(EXTDEF);
    cmd.iter_context(at.curr_mod, collect_externals);
  end;
END WriteExternals;

(* ------------------------ Export ------------------------ *)

PROCEDURE exported(o: pc.OBJECT): BOOLEAN;
BEGIN
  CASE o^.mode OF
--| pc.ob_cproc:   RETURN FALSE
  | pc.ob_module:  RETURN TRUE
  ELSE             RETURN o.is_public()
  END;
END exported;

CONST IGNOREs = pc.OB_SET{pc.ob_cproc, pc.ob_eproc};

CONST IGNORED_CONSTs = - (pc.COMPOUNDs + pc.REALs + pc.CPLXs);
                               -- Types of ignored constants
                               -- (all except structures + floatings)

PROCEDURE ignored(o: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (o^.mode IN IGNOREs) OR
   ((o.mode = pc.ob_module) & (o # at.curr_mod)) OR
   NOT (at.omark_gen_ready IN o^.marks) &
   ((o^.mno = at.curr_mno) & (o^.mode#pc.ob_var) OR
    (o^.mode=pc.ob_cons) & (o^.type#NIL) & (o^.type^.mode IN IGNORED_CONSTs))
END ignored;

PROCEDURE write_export(o: pc.OBJECT);
  VAR seg   :INT;
      addr  :LONGINT;
      name  :ARRAY 256 OF CHAR;
      Cname :ARRAY 256 OF CHAR;
      tag   :CHAR;

  PROCEDURE makePUB;
  BEGIN
    begin(tag);
    out_index(0);
    out_index(seg); out_name(name); outdw(addr);
    out_index(type_index(o));
    end;
  END makePUB;

  PROCEDURE makeEXP;
  BEGIN
    IF  NOT ((o^.mode = pc.ob_module) &  at.main) & at.get_dllexported(o) THEN
        (* don't export _dllmain from DLL *)
      IF (tag=PUBDEF) THEN
        begin(088X); out_index(0); outb(0A0h); out_index(02h); -- EXPDEF comment class
        outb(0);                                               -- exported by name
        <* IF C_COMP_MOD THEN *>
          out_name(Cname); out_name(name);
        <* ELSE *>
          out_name(name);
          outb(0);        -- internal name is the same as exported
        <* END *>
        end;
      END;
    END;
  END makeEXP;

BEGIN

 <* IF TESTOBJ THEN *>  io.note("write_export", o);  <* END *>

  IF (o.mno = at.curr_mno) & NOT ignored(o) THEN
    IF exported(o) THEN
      tag := PUBDEF;
 <* IF ALL_PUBLIC THEN *>
    ELSIF ((o.mode IN pc.PROCs) OR (o.lev = 0) & (o.mode # pc.ob_cons))
       & nms.valid_name(o.name)
    THEN
      tag := LPUBDEF;
 <* END *>
    ELSE RETURN
    END;

    get_adr(o, seg, addr); ASSERT(seg#0);
    nms.makename(o, name);
    nms.makeCname(o,Cname);

    makePUB();
    IF ( (at.TARGET = at.trg_NT) OR (at.TARGET = at.trg_LINUX) OR (at.TARGET = at.trg_OS2) ) 
          & (at.GENDLL IN at.COMP_MODE) 
    THEN
      makeEXP();
      <* IF env_target = "x86nt" THEN *>
      IF at.get_dllexported(o) & ( ( o^.mode=pc.ob_var )  OR
                                   ( o^.mode=pc.ob_cons ) OR
                                   ( o^.mode=pc.ob_type ) )  THEN
        (* to make import library using DLL export table *)
        str.Append ("@var153", name);
        str.Append ("@var153", Cname);
        makePUB();
        makeEXP();
      END;
      <* END *>
    END;
  END;
END write_export;

PROCEDURE WriteExport;
BEGIN
  cmd.iter_context(at.curr_mod, write_export);
END WriteExport;

PROCEDURE default_library(nm: ARRAY OF CHAR);
BEGIN
  IF at.DEF_LIBs IN at.COMP_MODE THEN
    begin_comment(9FH, FALSE, TRUE);
    out_str(nm);
    end;
  END;
END default_library;

-- VAR start_inx : INT;

PROCEDURE WriteDefaults;
BEGIN
  IF NOT (at.DEF_LIBs IN at.COMP_MODE) THEN RETURN END;
  IF at.MC = at.NATIVE THEN
    IF at.GENDLL IN at.COMP_MODE THEN
      begin(EXTDEF);
      INC(ExtCnt); out_name("xDLLinit"); out_index(0);
      end;
    ELSIF at.main THEN
      begin(EXTDEF);
      INC(ExtCnt); out_name("xStart"); out_index(0); -- start_inx := ExtCnt;
      end;
    END;
  ELSIF at.TARGET = at.trg_OS2 THEN
    IF at.MC = at.WATCOM THEN
      IF at.main THEN
        begin(EXTDEF);
        INC(ExtCnt); out_name("_cstart_"); out_index(0);
        INC(ExtCnt); out_name("_argc"); out_index(0);
        end;
        default_library("clib3s");
      END;
    ELSE
      (* Nothing for now *)
    END;
  ELSIF (at.CC = at.SYMANTEC) OR (at.CC = at.NATIVE) THEN
    default_library("SDX");
    IF at.UseFloatOps THEN
      begin(EXTDEF);
      INC(ExtCnt); out_name("__fltused"); out_index(0);
      end;
    END;
  ELSIF at.CC = at.WATCOM THEN
    IF at.main THEN
      begin(EXTDEF);
      INC(ExtCnt); out_name("_cstart_"); out_index(0);
      INC(ExtCnt); out_name("_argc"); out_index(0);
      end;
      default_library("clib3s");
    END;
    IF at.EMU_FPU IN at.COMP_MODE THEN
      begin(EXTDEF);
      INC(ExtCnt); out_name("__init_387_emulator"); out_index(0);
      end;
    END;
    IF at.UseFloatOps THEN
      default_library("math387s");
      IF at.EMU_FPU IN at.COMP_MODE THEN
        default_library("emu387");
      ELSE
        default_library("noemu387");
      END;
      IF at.DEF_LIBs IN at.COMP_MODE THEN
        begin(EXTDEF);
        INC(ExtCnt); out_name("__8087"); out_index(0);
        end;
      END;
    END;
  ELSIF at.CC = at.BORLAND THEN
    (* пока нет никаких библиотек и переменных *)
  ELSIF at.CC = at.OS2SYS_CALL THEN
    (* пока нет никаких библиотек и переменных *)
  ELSIF at.CC = at.MSVC THEN
    (* пока нет никаких библиотек и переменных *)
  END;
END WriteDefaults;

PROCEDURE WriteEnd;
  CONST MAIN_BIT  = 80H;
        START_BIT = 40H;
        X_BIT     = 1;
BEGIN
  begin(MODEND);
(*
  IF at.main & (at.TARGET = at.trg_OS2) & (at.MC = at.NATIVE) THEN
    outb(MAIN_BIT+START_BIT+X_BIT);       -- mod_type
    outb(FM_GROUP*16+TM_EXTERN);          -- end_data
    out_index(FLATGRP_INX);               -- frame_date
    out_index(start_inx);                 -- target_date
  ELSE
*)

    outb(1);

(*
  END;
*)

  end
END WriteEnd;

PROCEDURE WriteDebug (name-: ARRAY OF CHAR;
                      ext- : ARRAY OF CHAR);
VAR
  SegType: SYSTEM.INT16;
  SegSymb: SYSTEM.INT16;
BEGIN
 <* IF DBG_EDIF THEN *>
  IF isEDIF THEN
    -- prepare Consistent Key
    MakeReferenceFileName (name, ext);
    MakeConsistentKey ();
    DbgInfo := "";
  END;
 <* END *>
  IF dbg.generate (name, ext, TRUE, at.DbgNestedProc IN at.COMP_MODE) THEN
   <* IF DBG_EDIF THEN *>
    IF isEDIF THEN
      -- write comment for consistent key, XDS linker supported
      begin_comment (0A1H, TRUE, TRUE);
        outb (CONSKEY);
        out_str (DbgInfo);
        outb (0);
      end;
    ELSE
   <* END *>
      SegType := SegCnt;
      INC(SegCnt);
      out_segment(inx_ssTYPES, inx_DEBTYP,
                  dbg.type_info.code_len, BYTE_ALIGN, PRIV_COMB);
      SegSymb := SegCnt;
      INC(SegCnt);
      out_segment(inx_ssSYMBOLS, inx_DEBSYM,
                  dbg.symb_info.code_len, BYTE_ALIGN, PRIV_COMB);
      GenReadySegm(SegType, 0, dbg.type_info);
      GenReadySegm(SegSymb, 0, dbg.symb_info);
      dbg.symb_info := NIL;
      dbg.type_info := NIL;
   <* IF DBG_EDIF THEN *>
    END;
   <* END *>
  END;
END WriteDebug;

PROCEDURE clean(o: pc.OBJECT);
BEGIN
  EXCL(o^.marks, at.omark_gen_marked);
END clean;

PROCEDURE Init;
  VAR i: INT;
BEGIN
  FOR i:= SegExtern TO LastSeg DO SegSizes[i] := 0 END;
  IF (at.TARGET = at.trg_OS2) THEN
    IF cset THEN
      FirstNStdSeg := 4;  (* see WriteSegments *)
    ELSE
      IF at.main THEN  FirstNStdSeg := 6;  (* see WriteSegments *)
      ELSE             FirstNStdSeg := 5;  (* see WriteSegments *)
      END;
    END;
  ELSE
    FirstNStdSeg := 5;
  END;
  SegCnt := FirstNStdSeg;
END Init;

PROCEDURE Exit;
BEGIN
END Exit;

PROCEDURE mkoutname(VAR nm, ext: ARRAY OF CHAR);
  CONST OBJ_EXT = ".obj";
  VAR s: pc.STRING;
BEGIN
  COPY(at.curr_mod.name^,nm);
(*
<* IF env_target # "x86nt" THEN *>
  IF LENGTH(nm)>8 THEN nm[8] := 0X END;
<* END *>
*)
  env.config.Equation("OBJEXT", s);
  IF s = NIL THEN COPY(OBJ_EXT, ext);
  ELSE
    IF s[0] = '.' THEN ext[0] := 0X;
    ELSE ext[0] := '.'; ext[1] := 0X;
    END;
    str.Append(s^, ext);
--    IF LENGTH(ext)>4 THEN ext[4]:=0C END;
  END;
<* IF TESTOBJ THEN *>
  io.print("mkoutname('%s', '%s')\n", nm, ext);
<* END *>
END mkoutname;

TYPE
  FORM_OMF *= POINTER TO formOMF_rec;
  formOMF_rec *= RECORD (fc.formobj_rec)
                  END;

PROCEDURE (this: FORM_OMF) SetDebugFormat*() : BOOLEAN;
BEGIN
  IF at.TARGET=at.trg_OS2 THEN
    this.defaultFormat := this.FindItem(opt.dbg_HLL);
  ELSE
    this.defaultFormat := this.FindItem(opt.dbg_CV);
  END;
  RETURN this.SetDebugFormat^();
END SetDebugFormat;

PROCEDURE (this: FORM_OMF) generate*;
  VAR name: ARRAY 64 OF CHAR;
      ext: ARRAY 16 OF CHAR;
      i: INT;
      emit: reg.ITEM;
      dbg_is_set: BOOLEAN;
BEGIN
 <* IF TESTOBJ THEN *>
  io.print("\n ********** FORM_OBJ ***************\n");
  io.print("COMP_MODE = { ");
  FOR i := 0 TO MAX(SET) DO
    IF i IN at.COMP_MODE THEN io.print("%d ", i) END;
  END;
  io.print("}\n");
 <* END *>

  IF env.errors.err_cnt # 0 THEN RETURN; END;

  dbg_is_set := this.SetDebugFormat();
  gendebug := fc.GenDebug() & dbg_is_set;
  genlineno := fc.GenLineno();

  emit := reg.GetActive (opt.dbgFormat);
  isHLL := (emit # NIL) AND ((emit.name = opt.dbg_HLL) OR (emit.name = opt.dbg_XHLL));
 <* IF DBG_EDIF THEN *>
  isEDIF := (emit # NIL) AND (emit.name = opt.dbg_EDIF);
 <* END *>

  cset := FALSE;

  mkoutname (name, ext);

  Init;

  (* Закоментарено Shev'ом по поводу написания xlink для OS/2 где есть smart *)
  (*
  IF at.TARGET = at.trg_OS2 THEN new_segment := FALSE;
  ELSE new_segment := at.new_segment IN at.COMP_MODE;
  END;
  *)
  new_segment := at.new_segment IN at.COMP_MODE;

  WRITE_TRG_OFFS := FALSE;

  BeginOutput (name, ext);

  cmd.iter_context (at.curr_mod, clean);  (* ?? *)

  WriteHeader(at.curr_mod);
  WriteNames;
  WriteExternals;

  cmd.iter_context(at.curr_mod, allocate);
  FOR i := SegExtern TO LastSeg DO pre_mkal(SegSizes[i], l_dword) END;
  INC(SegSizes[SegUdat], 3); -- to to allow dword access to word sized variables at the very end of BSS
  WriteSegments;

  IF gendebug THEN
    WriteDebug (name, ext);
  END;

  cmd.iter_context (at.curr_mod, place);

  IF genlineno THEN
   <* IF DBG_EDIF THEN *>
    IF NOT isEDIF THEN
   <* END *>
      cmd.iter_context (at.curr_mod, GenLinNum);
   <* IF DBG_EDIF THEN *>
    END;
   <* END *>
  END;

  WriteExport;

--IF at.TARGET <> at.trg_OS2 THEN
    WriteDefaults;
--END;
  WriteEnd;
  EndOutput;
  Exit;
END generate;

VAR
  formOMF: FORM_OMF;
  item: reg.ITEM;

BEGIN
  NEW (formOMF);
  NEW (item);
  formOMF.AddItem (opt.dbg_CV, item);
  formOMF.defaultFormat := item;
  NEW (item);
  formOMF.AddItem (opt.dbg_HLL, item);
  NEW (item);
  formOMF.AddItem (opt.dbg_XHLL, item);
  NEW (item);
  formOMF.AddItem (opt.dbg_EDIF, item);
  reg.Register (opt.objFormat, opt.objOMF, formOMF);
END formOMF.

(* структура порождаемого OMF-файла :
  Header  - имя программы, версия, признак "нового" формата
  Names   - имена сегментов
  Externals - имена внешних объектов (ставится тип = 0)
  Segments  - определено 4 сегмента (CODE, DATA, CONST, BSS),
              последние 3 собраны в группу DGROUPE
  LEDATA, FIXUPs - собственно код и данные программы
  Export    - список имен экспортируемых объектов
  Defaults  - список некоторых необходимых стандартных объектов и библиотек
  End       - задается тип модуля и точка входа, если он главный
*)

