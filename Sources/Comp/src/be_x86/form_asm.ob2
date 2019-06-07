MODULE form_asm;

IMPORT sys := SYSTEM;

IMPORT SysClock;
IMPORT TimeConv;
IMPORT Strings;
IMPORT DStrings;

IMPORT ir;
IMPORT at  := opAttrs;
IMPORT pc  := pcK;
IMPORT cmd := CodeDef;
IMPORT nms := ObjNames;
IMPORT env := xiEnv;
IMPORT xfs := xiFiles;
IMPORT opt := Options;
IMPORT reg := Registry;
IMPORT fc  := CodeFace;

IMPORT dbg := DbgFace;

<* IF dbg_stab THEN *>
IMPORT stb := dbgSTAB;
<* END *>


(* to do:
   1. Local variables/procedures/other objects' naming
   2. Global objects' naming
   3. Local variable/parameter debug information (offset/reg if available)
   4. Declarations of local variables/data
*)


CONST
 <* IF TARGET_68K THEN *>
  text0 = "LL0";            -- first label in text segment
 <* ELSE *>
  text0 = "Ltext0";         -- first label in text segment
 <* END *>

<* IF TARGET_68K THEN *>
  COMMENT   = "|";          -- Start of comment in assembler line
<* ELSE *>
  COMMENT   = "#";          -- Start of comment in assembler line
<* END *>

  XDS_COMPILED = "xds_compiled:\n\n";


TYPE
  int     = LONGINT;
  object  = pc.OBJECT;
  struct  = pc.STRUCT;

VAR
  fileExt*   : pc.STRING;    -- used SL1 front-end

  text       : xfs.TextFile; -- asm file
  lfile      : xfs.TextFile; -- last source file, only for LINESRC
  lfname     : pc.STRING;    -- name of last source file

 <* IF dbg_stab THEN *>
  lline      : int;          -- last required line
 <* END *>

  genasm     : BOOLEAN;
  gencompool : BOOLEAN;
  genlineno  : BOOLEAN;
  genlinesrc : BOOLEAN;
  gendebug   : BOOLEAN;


<* IF dbg_stab THEN *>

---------- Output debug info for procedure  --------------------------

PROCEDURE db_write_proc (o: pc.OBJECT);
VAR
  tind: dbg.TYPE_INDEX;
BEGIN
  -- o.type - type of procedure
  -- o.type.base - type of result of procedure
  tind := dbg.type_index (o.type.base);
  stb.emitSTAB.SymbEmitter(dbg.sy_proc, o, tind);
END db_write_proc;


---------- Output procedure local variables --------------------------

PROCEDURE db_write_var (o: pc.OBJECT);
VAR
  tind: dbg.TYPE_INDEX;
BEGIN
  tind := dbg.type_index (o.type);
  stb.emitSTAB.SymbEmitter (dbg.sy_local_var, o, tind);
END db_write_var;

PROCEDURE db_write_locals (o: object);
VAR
  p: object;
BEGIN
  p := o.type.mem;
  WHILE p # NIL DO
    IF p.mode IN pc.VARs THEN
      db_write_var (p)
    END;
    p := p.next;
  END;
END db_write_locals;


---------- Output procedure lexical blocks --------------------------

PROCEDURE db_write_lex_blocks (o: object);
BEGIN
  stb.emitSTAB.SymbEmitter (dbg.sy_scope_open, o, -1);
  stb.emitSTAB.SymbEmitter (dbg.sy_scope_close, o, -1);
END db_write_lex_blocks;


---------- Output module global --------------------------------------

PROCEDURE db_write_glovar (o: pc.OBJECT);
VAR
  tind: dbg.TYPE_INDEX;
BEGIN
  tind := dbg.type_index (o.type);
  stb.emitSTAB.SymbEmitter (dbg.sy_var, o, tind);
END db_write_glovar;


---------- Output procedure parameters  ------------------------------

PROCEDURE db_write_param (o: pc.OBJECT);
VAR
  tind: dbg.TYPE_INDEX;
BEGIN
  tind := dbg.type_index (o.type);
  stb.emitSTAB.SymbEmitter (dbg.sy_param, o, tind);
END db_write_param;

PROCEDURE db_write_params (o: object);
VAR
  p: object;
BEGIN
  p := o.type.prof;
  WHILE p # NIL DO
    db_write_param (p);
    p := p.next;
  END;
END db_write_params;


---------- Get procedure attributes: prolog & epilogue ---------------

PROCEDURE db_get_proc_body (p: object; VAR start, end: int);
BEGIN
  start := VAL(int, dbg.start_proc_debug (p));
  end   := VAL(int, dbg.end_proc_debug (p));
END db_get_proc_body;

<* END *>


PROCEDURE write_file_name;
VAR
  src_dir, src_nm, src_ext: xfs.String;
BEGIN
  xfs.sys.Get (env.info.file^, src_dir, src_nm, src_ext);
  IF src_nm # xfs.null THEN
    IF src_ext = xfs.null THEN
      text.print ('\t.file "%s"\n', src_nm^);
    ELSE
      text.print ('\t.file "%s.%s"\n', src_nm^, src_ext^);
    END;
  END;
END write_file_name;


---------- Open output file ------------------------------------------

PROCEDURE open (nm-: ARRAY OF CHAR);
VAR
  ext: ARRAY 16 OF CHAR;
  s, fn: pc.STRING;
BEGIN
  lfile := NIL;
  lfname := NIL;

  -- init global options
  gencompool := env.config.Option ("COMPOOL");
  genasm     := at.GENASM IN at.COMP_MODE;
  gendebug   := at.debug IN at.COMP_MODE;
  genlineno  := gendebug OR (at.lineno IN at.COMP_MODE);
  genlinesrc := genlineno AND env.config.Option ("LINESRC");

  env.config.Equation("OUT",fn);
  IF (fn=NIL) OR (fn[0]=0X) THEN
    env.config.Equation ("ASMEXT", s);
    IF s = NIL THEN
      COPY (".s", ext);
    ELSE
      IF s[0] = '.' THEN ext[0] := 0X;
      ELSE ext[0] := '.'; ext[1] := 0X;
      END;
      Strings.Append (s^, ext);
    END;
    DStrings.Assign(ext, fileExt);
    xfs.sys.Create ('', nm, ext, fn);
    xfs.sys.UseFirst (fn^, fn);
  ELSE
    xfs.sys.ConvertFromHost(fn^,fn);
  END;
  xfs.text.Open (fn^, TRUE);
  IF xfs.text.file = NIL THEN
    env.errors.Fault (env.null_pos, 424, xfs.text.msg^);
  END;
  text := xfs.text.file (xfs.TextFile);

  write_file_name;
  text.print (XDS_COMPILED);
END open;

------------------- >>> LAZ
PROCEDURE pre_mkal(align: int);
BEGIN
  CASE align OF
  | 1:
  | 2:   <* IF TARGET_68K THEN *>   text.print ("\t.even\n");
         <* ELSE *>                 text.print ("\t.align\t2\n");
         <* END *>
  | 4:                              text.print ("\t.align\t4\n");
  | 8:                              text.print ("\t.align\t8\n");
  | 16:                             text.print ("\t.align\t16\n");
  END;
END pre_mkal;

PROCEDURE pre_mktal(t: pc.STRUCT);
  VAR al: int;
BEGIN
  WHILE t.mode = pc.ty_array DO t := t.base END;
  IF t.align = 0 THEN al := at.default_alignment;
  ELSE                al := t.align;
  END;
  pre_mkal(al);
END pre_mktal;

(* ------------------------ Export ------------------------ *)

PROCEDURE exported(o: pc.OBJECT): BOOLEAN;
BEGIN
  CASE o^.mode OF
--| pc.ob_cproc:   RETURN FALSE
  | pc.ob_module:  RETURN TRUE
  ELSE             RETURN o.is_public()
  END;
END exported;

CONST IGNOREs = pc.OB_SET{ pc.ob_cproc, pc.ob_eproc};

CONST IGNORED_CONSTs = - (pc.COMPOUNDs + pc.REALs + pc.CPLXs);
                               -- Types of ignored constants
                               -- (all except structures + floatings)

PROCEDURE ignored(o: pc.OBJECT): BOOLEAN;
BEGIN
  RETURN (o^.mode IN IGNOREs) OR
   ((o.mode = pc.ob_module) & (o # at.curr_mod)) OR
  <* IF dbg_stab THEN *>
   (stb.emitSTAB.CheckMarks AND NOT (at.omark_gen_ready IN o^.marks)) &
  <* ELSE *>
   NOT (at.omark_gen_ready IN o^.marks) &
  <* END *>
   ((o^.mno = at.curr_mno) & (o^.mode#pc.ob_var) OR
    (o^.mode=pc.ob_cons) & (o^.type#NIL) & (o^.type^.mode IN IGNORED_CONSTs))
END ignored;

PROCEDURE write_label (o: pc.OBJECT);
VAR
  buf: ARRAY 1024 OF CHAR;
BEGIN
  nms.makename(o, buf);
  IF (o.mno = at.curr_mno) & NOT ignored(o) & exported(o) THEN
   <* IF TARGET_68K THEN *>
    -- to work around as68k feature - global label is not allowed as local one
    text.print (".global %s\n%s:\n", buf, buf);
    text.print ("%s:\n", buf);
   <* ELSE *>
    text.print (".global %s\n%s:\n", buf, buf);
   <* END *>
  ELSE
    text.print ("%s:\n", buf);
  END;
END write_label;


PROCEDURE write_segment(o: pc.OBJECT);
  VAR i: int;
    attr: at.ATTR_EXT;
    s: cmd.CODE_SEGM;
BEGIN
  attr := at.attr (o.ext, cmd.a_ready_code);
  s := attr(cmd.CODE_SEGM);
  FOR i := 0 TO s.code_len - 1 DO
    text.print ("%s\n", s.acode[i]^);
  END;
END write_segment;


PROCEDURE write_one_glovar(o: pc.OBJECT);
  VAR buf: ARRAY 1024 OF CHAR;
BEGIN
  IF (o^.mode = pc.ob_var) & (o.lev = 0) THEN
    -- global var
    IF at.omark_gen_ready IN o.marks THEN
      -- must be written as constant by write_one_const
    ELSE
      IF o.mno = at.curr_mno THEN
        -- from current module
        nms.makename(o, buf);
        text.print (".comm %s, %d\n", buf, pc.code.get_size(pc.su_bytes, o.type));
       <* IF dbg_stab THEN *>
        IF gendebug THEN
          db_write_glovar (o);
        END;
       <* END *>
      END;
    END;
  END;
END write_one_glovar;

PROCEDURE write_glovars;
BEGIN
  text.print ("\n" + COMMENT + " glovars\n");
  cmd.iter_context(at.curr_mod, write_one_glovar);
  text.print (COMMENT + " -- glovars\n\n");
END write_glovars;


PROCEDURE write_one_const(o: pc.OBJECT);
BEGIN
  CASE o^.mode OF
  | pc.ob_proc, pc.ob_xproc, pc.ob_lproc
  , pc.ob_eproc, pc.ob_cproc
  , pc.ob_module :  (* ничего не делать *)

  | pc.ob_var:
    IF (o.lev = 0) AND (o.mno = at.curr_mno) AND (at.omark_gen_ready IN o.marks) THEN
      -- global var with data segment
      pre_mktal (o.type);
      write_label (o);
      write_segment (o);
    END;

  | pc.ob_cons:
      IF (o.mno = at.curr_mno) & (at.omark_gen_ready IN o.marks) THEN
        IF o.type = NIL THEN (* some compiler constructed constants have no type *)
          pre_mkal(at.default_alignment);
        ELSE
          pre_mktal(o.type);
        END;
        write_label(o);
        write_segment(o);
      END;
  | pc.ob_type:
      IF (o.mno = at.curr_mno) & (at.omark_gen_ready IN o.marks) THEN
        pre_mkal(at.default_alignment);
        write_label(o);
        write_segment(o);
      END;

  ELSE ASSERT(FALSE,100h+ORD(o^.mode));
  END;
END write_one_const;

PROCEDURE write_consts;
BEGIN
  text.print ("\n" + COMMENT + " consts\n\t.data\n");
  cmd.iter_context(at.curr_mod, write_one_const);
  text.print (COMMENT + " -- consts\n\n");
END write_consts;

------------------- <<< LAZ

PROCEDURE close;
  VAR err: pc.STRING;
    save: BOOLEAN;
BEGIN
  IF env.errors.err_cnt # 0 THEN
    save := FALSE;
  ELSE
    save := TRUE;
  END;

  text.CloseNew (save, FALSE, FALSE, err);
  IF err # NIL THEN
    env.errors.Fault (env.null_pos, 432, err^);
  END;
  text := NIL;
  lfname := NIL;
  IF lfile # NIL THEN
    lfile.Close;
    lfile := NIL;
  END;
END close;


CONST
  UNDEF_OFFS = MAX(int);

PROCEDURE proc (o: pc.OBJECT);
VAR
  a, offs: int;
  attr: at.ATTR_EXT;
  s: cmd.CODE_SEGM;
  proc_name: ARRAY 256 OF CHAR;

 <* IF dbg_stab THEN *>
  x, start, end, line, col, oline: int;
  fname: pc.STRING;
  -->> SHEV: VxWorks loader swear vxworks.big if lex. block is used
  lex_name: ARRAY 256 OF CHAR;
  buf: ARRAY 256 OF CHAR;
  src: pc.STRING; -- source line
 <* END *>

  -- выдает участок кода от текущего положения до
  -- заданного смещения в коде offs
  -- при этом выдает начало и конец блока (пролог и эпилог)
  PROCEDURE write_code;
  BEGIN
    WHILE a < offs DO
     <* IF dbg_stab THEN *>
      IF (start # UNDEF_OFFS) AND (start <= a) THEN
        -->>> SHEV
        IF env.config.Option (opt.DBG_LEX_BLOCKS) THEN
          stb.emitSTAB.LexBlockName (dbg.sy_scope_open, o, lex_name);
          text.print ("%s:\n", lex_name);
        END;
        start := UNDEF_OFFS;
      END;
      IF (end # UNDEF_OFFS) AND (end <= a) THEN
        -->>> SHEV
        IF env.config.Option (opt.DBG_LEX_BLOCKS) THEN
          stb.emitSTAB.LexBlockName (dbg.sy_scope_close, o, lex_name);
          text.print ("%s:\n", lex_name);
        END;
        end := UNDEF_OFFS;
      END;
     <* END *>
      text.print ("%s\n", s.acode[a]^);
      INC (a);
    END;
  END write_code;

BEGIN
  IF env.errors.err_cnt # 0 THEN RETURN; END;
  attr := at.attr (o.ext, cmd.a_ready_code);
  s := attr(cmd.CODE_SEGM);
 <* IF TARGET_68K THEN *>
  text.print ("\n\n\t.align\t4, 0x4afc\n");
 <* ELSE *>
  text.print ("\n\n\t.align\t4\n");
 <* END *>
  nms.makename(o, proc_name);
  write_label(o);
  a := 0;
 <* IF dbg_stab THEN *>
  IF gendebug THEN
    db_get_proc_body (o, start, end);
  ELSE
    start := UNDEF_OFFS;
    end := UNDEF_OFFS;
  END;
  IF genlineno THEN
    x := 0;
    oline := -2;
    WHILE x < s.xref_len DO
      offs := s.xref[x].offs;
      REPEAT
        INC (x);
      UNTIL (x >= s.xref_len) OR (offs # s.xref[x].offs);
      DEC (x);
      s.xref[x].txtpos.unpack (fname, line, col);
      IF (line # oline) OR (fname # lfname) THEN
        -- changed line or source file, but not column
        write_code;
        stb.emitSTAB.LineNoEmitter (o, col+1, line+1);
        IF genlinesrc THEN
          IF (line < lline) OR (fname # lfname) THEN
            -- line great than last readed line
            IF lfile # NIL THEN
              -- close previous source file
              lfile.Close;
              lfile := NIL;
            END;
            xfs.text.Open (fname^, FALSE);
            IF xfs.text.file # NIL THEN
              -- file is succesfully opened
              lfile := xfs.text.file(xfs.TextFile);
            END;
            -- first line equal 0
            lline := 0;
          END;
          IF lfile # NIL THEN
            LOOP
              IF lline = line THEN
                -- read needed line
                src := NIL;
                LOOP
                  lfile.ReadString (buf);
                  IF lfile.readRes = xfs.endOfLine THEN
                    INC(lline);
                    EXIT;
                  ELSIF lfile.readRes # xfs.allRight THEN
                    EXIT;
                  END;
                  DStrings.Append (buf, src);
                END;
                IF src # NIL THEN
                  text.print (COMMENT + "%5d %s\n", line+1, src^);
                END;
                EXIT;
              END;
              -- skip all lines before required
              lfile.ReadString (buf);
              IF lfile.readRes = xfs.endOfLine THEN
                -- go to next line
                INC(lline);
              ELSIF lfile.readRes = xfs.endOfInput THEN
                EXIT;
              END;
            END; -- OF LOOP reading source file
          END; -- OF IF source file is opened
        END; -- OF IF genlinesrc is on
      END;
      lfname := fname;
      oline := line;
      INC (x);
    END;
  END;
 <* END *>
  -- выдать оставшийся участок кода
  offs := s.code_len;
  write_code;
 <* IF dbg_stab THEN *>
  IF gendebug THEN
    db_write_proc (o);
    db_write_params (o);
    db_write_locals (o);
    db_write_lex_blocks (o);
    text.print ("\n");
  END;
 <* END *>

  text.print (COMMENT + " -- %s\n", proc_name);

  -- AK: release memory
  at.del_attr (o.ext, cmd.a_ready_code);
  s.xref := NIL;
  s.xref_len := 0;
END proc;


PROCEDURE write_one_proc (o: pc.OBJECT);
BEGIN
  IF ( fc.ObjectStatus(o) IN { fc.status_Local, fc.status_Global } ) &
     ( fc.ObjectClass (o) = fc.class_Text ) THEN
        proc (o);
  END;
END write_one_proc;


TYPE
  FORM_ASM *= POINTER TO formASM_rec;
  formASM_rec *= RECORD (fc.formobj_rec)
                 END;

PROCEDURE (this: FORM_ASM) generate*;
BEGIN
  open(at.curr_mod.name^);

  IF NOT this.SetDebugFormat() THEN
    gendebug := FALSE;
  END;

 <* IF dbg_stab THEN *>
  IF gendebug OR genlineno THEN
    IF dbg.generate (at.curr_mod.name^, fileExt^, FALSE, TRUE) THEN
      stb.emitSTAB.Connect (text);
      stb.emitSTAB.CheckMarks := TRUE;
      stb.emitSTAB.text0 := text0;
      stb.emitSTAB.ini;
    ELSE
      gendebug := FALSE;
      genlineno := FALSE;
      genlinesrc := FALSE;
    END;
  END;
 <* END *>
  text.print(".text\n");
  text.print("%s:\n", text0);
 <* IF dbg_stab THEN *>
  IF gendebug OR genlineno THEN
    -- генерировать таблицу стандартных типов при lineno+
    stb.emitSTAB.TypeEmitter (dbg.ty_start, dbg.act_write, NIL);
  END;
  IF gendebug THEN
    stb.emitSTAB.SymbEmitter (dbg.sy_start, NIL, -1);
  END;
 <* END *>

  cmd.iter_context( at.curr_mod, write_one_proc );

  -- close file

  IF env.errors.err_cnt = 0 THEN
    write_consts;
    write_glovars;
   <* IF dbg_stab THEN *>
    IF gendebug THEN
      stb.emitSTAB.TypeEmitter (dbg.ty_end, dbg.act_write, NIL);
      stb.emitSTAB.SymbEmitter (dbg.sy_end, NIL, -1);
      stb.emitSTAB.exi;
    END;
   <* END *>
  END;

  close;

END generate;


VAR
  formASM: FORM_ASM;
  item: reg.ITEM;

BEGIN
  NEW(formASM);
  NEW(item);
  formASM.AddItem(opt.dbg_STAB, item);
  formASM.defaultFormat := item;
  reg.Register(opt.objFormat, opt.objASM, formASM);
END form_asm.
