MODULE CodeDef;
IMPORT pc   := pcK,
       ir,
       at   := opAttrs,
       tune := opTune,
       env  := xiEnv,
       plt  := xmPlatform,
       Calc,

<* IF TARGET_RISC THEN *>
       TOCData,
       def := InstrDef,
<* ELSIF TARGET_SPARC THEN *>
       def := SPARCDefs,
<* END *>
<* IF TARGET_VAX THEN *>
       VAX_F,
<* END *>
<* IF ~ nodebug THEN *>
       opIO,
<* END *>
       DStrings,
       FormStr,
       SYSTEM;

<* IF TARGET_386 THEN *> IMPORT xProfRTS; <* END *>

TYPE 
  INT = LONGINT;
  INT16  = SYSTEM.INT16;
  INT32  = SYSTEM.INT32;
  CARD8  = SYSTEM.CARD8;
  CARD32 = SYSTEM.CARD32;
  SET16  = SYSTEM.SET16;
  SET32  = SYSTEM.SET32;

<* IF TARGET_RISC THEN *>
CONST
  GREEN_HILLS = "GHS";  -- options: generate green hills assembler
<* END *>


VAR
  ret_node *: ir.Node; -- node number with RET instruction
  ret_offs *: LONGINT; -- offset RET instruction

VAR
  makename *: PROCEDURE (o: pc.OBJECT; VAR name: ARRAY OF CHAR);
  -- avoiding cycle import. really this is objNames.makename

--------------- B Y T E   O R D E R ----------------------------
                -------------------
VAR inverse_byte_order* : BOOLEAN;

PROCEDURE invert2b* (VAR w: INT16);
  VAR s: SET16;
BEGIN
  s := SYSTEM.VAL (SET16, w);
  s := SYSTEM.SHIFT (s * SET16 {0..7}, 8) 
     + SYSTEM.SHIFT (s * SET16 {8..15}, -8);
  w := SYSTEM.VAL (INT16, s);
END invert2b;

PROCEDURE invert4b* (VAR w: INT32);
  VAR s: SET32;
BEGIN
  s := SYSTEM.VAL (SET32, w);
  s := SYSTEM.SHIFT (s * SET32 {0..7}, 24) 
     + SYSTEM.SHIFT (s * SET32 {8..15}, 8)
     + SYSTEM.SHIFT (s * SET32 {16..23}, -8) 
     + SYSTEM.SHIFT (s * SET32 {24..31}, -24);
  w := SYSTEM.VAL (INT32, s);
END invert4b;

PROCEDURE move2b* (src, dst: SYSTEM.ADDRESS; inverse: BOOLEAN);
  TYPE 
    pTWO_BYTES = POINTER [1] TO ARRAY 2 OF SYSTEM.BYTE;
  VAR 
    s,d: pTWO_BYTES;
BEGIN 
  ASSERT (NOT (at.GENASM IN at.COMP_MODE));
  s := SYSTEM.VAL (pTWO_BYTES, src);
  d := SYSTEM.VAL (pTWO_BYTES, dst);
  IF inverse THEN
    d [1] := s [0];
    d [0] := s [1];
  ELSE
    d [0] := s [0];
    d [1] := s [1];
  END;
END move2b;

PROCEDURE move4b* (src, dst: SYSTEM.ADDRESS; inverse: BOOLEAN);
  TYPE 
    pFOUR_BYTES = POINTER [1] TO ARRAY 4 OF SYSTEM.BYTE;
  VAR 
    s,d: pFOUR_BYTES;
BEGIN 
  ASSERT (NOT (at.GENASM IN at.COMP_MODE));
  s := SYSTEM.VAL (pFOUR_BYTES, src);
  d := SYSTEM.VAL (pFOUR_BYTES, dst);
  IF inverse THEN
    d [3] := s [0];
    d [2] := s [1];
    d [1] := s [2];
    d [0] := s [3];
  ELSE
    d [0] := s [0];
    d [1] := s [1];
    d [2] := s [2];
    d [3] := s [3];
  END;
END move4b;

---------------  C O D E  --------------------------------------
                 -------
CONST
  a_vars_info   = at.a_gen_code + 0;
  a_ready_code* = at.a_gen_code + 1;

CONST -- fixup kinds
      fx_obj32*    = 0;     -- 32-bit object address
      fx_relcall*  = 1;     -- relative procedure call
      fx_obj32far* = 2;     -- 16:32 object address
<* IF TARGET_RISC THEN *>
      fx_rel24*    = 3;     -- 24-bit relative
      fx_toc16*    = 4;     -- 16-bit offset from .toc start
      fx_ifglue*   = 5;     -- if glue code
      fx_hi16*     = 6;     -- 16-bit high half of object address
      fx_lo16*     = 7;     -- 16-bit low half of object address
<* END *>
<* IF TARGET_SPARC THEN *>
      fx_rel30*    = 3;     -- 30-bit relative
      fx_hi16*     = 4;     -- 16-bit high half of object address
      fx_lo16*     = 5;     -- 16-bit low half of object address
<* END *>
      fx_tdrel32*  = 8;     -- 32-bit TD relative offset
      fx_tdrel16*  = 9;     -- 16-bit TD relative offset
      fx_to_td16*  = 10;    -- fixup to TD, specially handled by linker
      fx_cseg32*   = 11;    -- 32-bit address of class' code segment
      fx_csegsz32* = 12;    -- 32-bit size of class' code segment

TYPE
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  CT_desc* = def.CT_desc;
  CTs*     = def.CTs;
  PC_desc* = def.PH_desc;
  PHs*     = def.PHs;
<* END *>
  CODE_SEGM* = POINTER TO segm_desc;


<* IF TARGET_386 THEN *>
VAR
(* excepttable is array of
     RECORD fxup2proc, procsz, fxup2finalizer: CARD32; END;
  terminated by 4-byte zero value
*)
  excepttable *: pc.OBJECT;
  excepttable_segm *: CODE_SEGM;

  prof_info *: pc.OBJECT;
  prof_info_loc *: ir.Local;
  prof_info_segm *: CODE_SEGM;
  prof_xref *: pc.OBJECT;
  prof_xref_segm *: CODE_SEGM;

CONST
  prof_info_elem_len *= SIZE(xProfRTS.X2C_Profile_STR);
<* END *>

TYPE
  xref_desc   = RECORD
                  offs*  : LONGINT;
                  txtpos*: pc.TPOS;
                END;

  fixup_desc* = RECORD
                  obj*    : pc.OBJECT;
                  fx_offs*: LONGINT;
                  offs*   : LONGINT;
                  kind*   : SHORTINT;
                END;

  one_var_desc* = RECORD
                    obj*      : pc.OBJECT;
                    location* : SHORTINT;
                    value*    : LONGINT;    -- LocInReg   - register #
                                            -- LocInFrame - stack offset
                END;

  Instruction* = DStrings.String;
  CODE_STRING_A = POINTER TO ARRAY OF Instruction;

  CODE_STRING_B = POINTER TO ARRAY OF SYSTEM.BYTE;

  XREFs*      = POINTER TO ARRAY OF xref_desc;
  FIXUPs*     = POINTER TO ARRAY OF fixup_desc;

  segm_desc = RECORD (at.attr_ext_rec)
                acode*: CODE_STRING_A;
                bcode*: CODE_STRING_B;
                fxup* : FIXUPs;
                xref* : XREFs;
                code_len* : LONGINT;
                fxup_len* : LONGINT;
                xref_len* : LONGINT;
                start*, fin*: LONGINT; -- Начало/конец собственно тела процедуры
                frame_size* : LONGINT; -- Размер начального кадра процедуры
                has_frame*  : BOOLEAN;
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
                placeholder*    : def.PHs;
                placeholder_len*: LONGINT;
                casetable*      : def.CTs;
                casetable_len*  : LONGINT;
<* END *>
              END;

PROCEDURE fxup_length*(fx-: fixup_desc): INTEGER;
BEGIN
  CASE fx.kind OF
  | fx_obj32,
    fx_cseg32,
    fx_relcall:  RETURN 4;
  | fx_obj32far: RETURN 6;
  | fx_to_td16 : RETURN 2;
--  ELSE ASSERT(FALSE);
  END;
END fxup_length;

---------------  C O D E  W R I T I N G -----------------------------------
                 ----------------------

VAR c_seg: CODE_SEGM;

PROCEDURE new_segm*(VAR sg: CODE_SEGM);
BEGIN
  NEW(sg);
  IF at.GENASM IN at.COMP_MODE THEN NEW(sg.acode, 32); ELSE NEW(sg.bcode, 32); END;
(*   sg.fxup := NIL;   --  NEW(sg.fxup, 5);  *)
(*   sg.xref := NIL;   --  NEW(sg.xref, 5);  *)
  sg.code_len := 0;
  sg.fxup_len := 0;
  sg.xref_len := 0;
END new_segm;

PROCEDURE get_segm*(VAR sg: CODE_SEGM);
BEGIN
  sg := c_seg;
END get_segm;

PROCEDURE set_segm*(sg: CODE_SEGM);
BEGIN
  c_seg := sg;
END set_segm;

PROCEDURE get_ready*(o: pc.OBJECT): CODE_SEGM;
BEGIN
  RETURN SYSTEM.VAL(CODE_SEGM, at.attr(o.ext, a_ready_code));
END get_ready;

PROCEDURE set_ready*(o: pc.OBJECT; VAR sg: CODE_SEGM);
BEGIN
  ASSERT((sg.code_len > 0) OR (o.mode=pc.ob_cproc));
  at.app_obj_attr(o, sg, a_ready_code);
  INCL(o.marks, at.omark_gen_ready);
  sg := NIL;
END set_ready;

PROCEDURE Realloc;
  VAR newa: CODE_STRING_A; newb: CODE_STRING_B; i: INT;
BEGIN
  IF at.GENASM IN at.COMP_MODE THEN
      NEW(newa, c_seg.code_len*2);
      FOR i := 0 TO c_seg.code_len-1 DO newa[i] := c_seg.acode[i]; END;
      c_seg.acode := newa;
  ELSE
      NEW(newb, c_seg.code_len*2);
      SYSTEM.MOVE (SYSTEM.ADR(c_seg.bcode[0]), 
                   SYSTEM.ADR(newb[0]), 
                   c_seg.code_len);
      c_seg.bcode := newb;
  END;
END Realloc;

PROCEDURE GenInstr * (s- : ARRAY OF CHAR);
BEGIN
  IF c_seg.code_len = LEN(c_seg.acode^) THEN Realloc; END;
  DStrings.Assign (s, c_seg.acode[c_seg.code_len]);
  INC(c_seg.code_len);
END GenInstr;

PROCEDURE GenByteB(b : SYSTEM.BYTE);
BEGIN
<* IF ~ nodebug THEN *>
  IF opIO.needed THEN
    opIO.print("\tbyte       %02x\n", SYSTEM.VAL(CARD8,b));
  END;
<* END *>
  IF c_seg.code_len = LEN(c_seg.bcode^) THEN Realloc; END;
  c_seg.bcode[c_seg.code_len] := b;
  INC(c_seg.code_len);
END GenByteB;

PROCEDURE GenByteBToPos*(b : SYSTEM.BYTE; pos: INT);
BEGIN
<* IF ~ nodebug THEN *>
  IF opIO.needed THEN
    opIO.print("\tbytetopos  %02x pos %d\n", SYSTEM.VAL(CARD8,b), pos);
  END;
<* END *>
--  IF c_seg.code_len = LEN(c_seg.bcode^) THEN Realloc; END;
  c_seg.bcode[pos] := b;
--  INC(c_seg.code_len);
END GenByteBToPos;

PROCEDURE GenByteA(b : SYSTEM.BYTE);
VAR s: ARRAY 32 OF CHAR;
BEGIN
    FormStr.print (s, "        .byte   %d", SYSTEM.VAL(CARD8,b));
    GenInstr (s);
END GenByteA;

PROCEDURE GenByte*(b : SYSTEM.BYTE);
BEGIN
  IF at.GENASM IN at.COMP_MODE THEN 
    GenByteA(b); 
  ELSE 
    GenByteB(b); 
  END;
END GenByte;

PROCEDURE GenWordB(w : INTEGER);
BEGIN
<* IF ~ nodebug THEN *>
  opIO.print("\tword     %04x\n", w);
<* END *>
  IF c_seg.code_len + 2 > LEN(c_seg.bcode^) THEN Realloc; END;
  move2b (SYSTEM.ADR (w),
          SYSTEM.ADR (c_seg.bcode [c_seg.code_len]),
          inverse_byte_order);
  INC(c_seg.code_len, 2);
END GenWordB;

PROCEDURE GenWordA(w : INTEGER);
VAR s: ARRAY 32 OF CHAR;
BEGIN
    FormStr.print (s, "        .short  %d", w);
    GenInstr (s);
END GenWordA;

PROCEDURE GenWord*(w : INTEGER);
BEGIN
  IF at.GENASM IN at.COMP_MODE THEN GenWordA(w); ELSE GenWordB(w); END;
END GenWord;

PROCEDURE GenLWordB(w : LONGINT);
BEGIN
<* IF ~ nodebug THEN *>
  opIO.print("\tlong %08x\n", w);
<* END *>
  IF c_seg.code_len + 4 > LEN(c_seg.bcode^) THEN Realloc; END;
  move4b (SYSTEM.ADR (w), 
          SYSTEM.ADR (c_seg.bcode [c_seg.code_len]), 
          inverse_byte_order);
  INC(c_seg.code_len, 4);
END GenLWordB;

PROCEDURE GenLWordA(w : LONGINT);
VAR s: ARRAY 32 OF CHAR;
BEGIN
    FormStr.print (s, "        .long   %d", w);
    GenInstr (s);
END GenLWordA;

PROCEDURE GenLWord*(w : LONGINT);
BEGIN
  IF at.GENASM IN at.COMP_MODE THEN GenLWordA(w); ELSE GenLWordB(w); END;
END GenLWord;

----------- String generation -----------------
<* IF TARGET_RISC THEN *>
VAR 
  str_begin : ARRAY 32 OF CHAR;
<* ELSE *>
CONST
  str_begin = '        .ascii  "';
<* END *>

VAR
  ss_cnt : INT;
  ss : ARRAY 256 OF CHAR;
<* IF TARGET_RISC THEN *>
  Quota: BOOLEAN;
  WasStart : BOOLEAN;
<* END *>

<* IF TARGET_RISC THEN *>

PROCEDURE GenStartStringA0();
  CONST str_begin = '        .byte   ';  -- to override value above
BEGIN
  ASSERT(ss_cnt = 0);
  COPY(str_begin, ss);
  ss_cnt := LENGTH(str_begin);
  Quota:= FALSE;
  WasStart := TRUE;
END GenStartStringA0;

PROCEDURE GenEndStringA0();
BEGIN
  IF Quota THEN
    ss[ss_cnt] := '"';
    INC(ss_cnt);
    Quota := FALSE;
  END;
  ss[ss_cnt] := 0X;
  GenInstr (ss);
  ss_cnt := 0;
  WasStart := FALSE;
END GenEndStringA0;

<* END *>

PROCEDURE GenStartStringA1();
BEGIN
  ASSERT(ss_cnt = 0);
  COPY(str_begin, ss);
  ss_cnt := LENGTH(str_begin);
END GenStartStringA1;

PROCEDURE GenEndStringA1();
BEGIN
  ss[ss_cnt] := '"';
  ss[ss_cnt+1] := 0X;
  GenInstr (ss);
  ss_cnt := 0;
END GenEndStringA1;

PROCEDURE GenStartString*();
BEGIN
  IF at.GENASM IN at.COMP_MODE THEN
   <* IF TARGET_RISC THEN *>
    IF at.ABI = at.PowerOpen THEN GenStartStringA0; RETURN END;
  <* END *>
    GenStartStringA1;
  END;
END GenStartString;

PROCEDURE GenEndString*();
BEGIN
  IF at.GENASM IN at.COMP_MODE THEN
   <* IF TARGET_RISC THEN *>
    IF at.ABI = at.PowerOpen THEN GenEndStringA0; RETURN END;
  <* END *>
    GenEndStringA1;
  END;
END GenEndString;

PROCEDURE octal(n: INTEGER);
(* This should produce number wich consists of 3 digits exactly, to avoid
   problems with following digit characters, i.e. "\t0" is "\0110" but not
   "\110" *)
BEGIN
  ASSERT(n<=255);
  ss[ss_cnt] := CHR(ORD("0")+ n DIV 64 );
  INC(ss_cnt);
  ss[ss_cnt] := CHR(ORD("0")+ (n DIV 8) MOD 8 );
  INC(ss_cnt);
  ss[ss_cnt] := CHR(ORD("0")+ n MOD 8 );
  INC(ss_cnt);
END octal;

<* IF TARGET_RISC THEN *>
PROCEDURE GenStringCharA(ch: CHAR);
BEGIN
  IF at.ABI = at.PowerOpen THEN
    IF NOT WasStart THEN GenStartString; END;
    IF ss_cnt >= SIZE(ss)-5 THEN
      GenEndString;
      GenStartString;
    END;
    IF (ch < ' ') OR (ch >'~') OR (ch='"')  THEN
      IF Quota THEN GenEndString; GenStartString END;
      octal(ORD(ch));
      GenEndString;
      RETURN
    END;
    IF NOT Quota THEN
      ss[ss_cnt] := '"';
      INC(ss_cnt);
      Quota := TRUE;
    END;
    ss[ss_cnt] := ch; INC(ss_cnt);
  ELSE
    IF ch = CHR(0) THEN RETURN END;
    IF ss_cnt >= SIZE(ss)-5 THEN
      GenEndString;
      GenStartString;
    END;
    IF (ch < ' ') OR (ch >'~') OR (ch='"') THEN
      ss[ss_cnt] := "\";
      INC(ss_cnt);
      octal(ORD(ch));
    ELSE
      ss[ss_cnt] := ch; INC(ss_cnt);
    END;
  END;
END GenStringCharA;

<* ELSE *>
PROCEDURE GenStringCharA(ch: CHAR);
BEGIN
  IF ss_cnt >= SIZE(ss)-5 THEN
    GenEndString;
    GenStartString;
  END;
  IF (ch < ' ') OR (ch >'~') OR (ch='"')  THEN
    ss[ss_cnt] := "\";
    INC(ss_cnt);
    octal(ORD(ch));
    RETURN
  END;
  IF ch ='\' THEN
    ss[ss_cnt] := "\"; INC(ss_cnt);
  END;
  ss[ss_cnt] := ch; INC(ss_cnt);
END GenStringCharA;
<* END *>

PROCEDURE GenStringChar*(ch : CHAR);
BEGIN
  IF at.GENASM IN at.COMP_MODE THEN
    GenStringCharA(ch);
  ELSE
    GenByte(ch);
  END;
END GenStringChar;

PROCEDURE GenBuf* (VAR (*IN*) b: ARRAY OF SYSTEM.BYTE);
  VAR newb: CODE_STRING_B; i, ln, inc: INT;
BEGIN
  ln := SIZE(b);
  IF c_seg.code_len + ln > LEN(c_seg.bcode^) THEN
    IF ln > c_seg.code_len THEN inc := ln ELSE inc := c_seg.code_len END;
    NEW(newb, c_seg.code_len+inc);
    SYSTEM.MOVE (SYSTEM.ADR(c_seg.bcode[0]), 
                 SYSTEM.ADR(newb[0]), 
                 c_seg.code_len);
    c_seg.bcode := newb
  END;
  FOR i := 0 TO ln-1 DO
    c_seg.bcode[c_seg.code_len] := b[i];
    INC(c_seg.code_len);
  END;
END GenBuf;

PROCEDURE add_fixup*(obj: pc.OBJECT;
                fx_offs: LONGINT;
                 offs  : LONGINT;
                 kind  : SHORTINT);
  VAR i: LONGINT; new: FIXUPs;
BEGIN
  IF c_seg.fxup = NIL THEN ASSERT(c_seg.fxup_len = 0);
    NEW(new, 5);
    c_seg.fxup := new;
  ELSIF c_seg.fxup_len = LEN(c_seg.fxup^) THEN
    NEW(new, c_seg.fxup_len*2);
    FOR i := 0 TO c_seg.fxup_len-1 DO new[i] := c_seg.fxup[i] END;
    c_seg.fxup := new;
  END;
  ASSERT(offs <= LEN(c_seg.bcode^) );
  c_seg.fxup[c_seg.fxup_len].obj     := obj;
  c_seg.fxup[c_seg.fxup_len].fx_offs := fx_offs;
  c_seg.fxup[c_seg.fxup_len].offs    := offs;
  c_seg.fxup[c_seg.fxup_len].kind    := kind;
  INC(c_seg.fxup_len);
END add_fixup;

PROCEDURE InsInstr * (s- : ARRAY OF CHAR; pos:INT);
  VAR i: INT;
BEGIN
  IF c_seg.code_len = LEN(c_seg.acode^) THEN Realloc; END;
  FOR i:=c_seg.code_len-1 TO pos BY -1 DO
    c_seg.acode[i+1] := c_seg.acode[i];
  END;
  FOR i:=pos TO c_seg.xref_len-1 DO
    IF c_seg.xref[i].offs <> 0 THEN
      INC (c_seg.xref[i].offs);
    END;
  END;
  DStrings.Assign (s, c_seg.acode[pos]);
  INC(c_seg.code_len);
END InsInstr;

PROCEDURE GenAlign* (align: SHORTINT);
BEGIN
(*
    CASE align OF
    | 1:
    | 2:    GenInstr ("        .even");
    | 4:    GenInstr ("        .align  2");
    | 8:    GenInstr ("        .align  3");
    | 16:   GenInstr ("        .align  4");
    END;
*)
    CASE align OF
    | 1:
    | 2:    GenInstr ("        .even");
    | 4:    GenInstr ("        .align  4");
    | 8:    GenInstr ("        .align  8");
    | 16:   GenInstr ("        .align  16");
    END;
END GenAlign;

-- Количество инструкций на выравнивание
-- Должна согласовываться с GenAlign
PROCEDURE GetAlign* (align: SHORTINT): SHORTINT;
BEGIN
  IF align = 1 THEN
    RETURN 0;
  ELSE
    RETURN 1;
  END;
END GetAlign;


<* IF TARGET_RISC THEN *>    -- added by KDV

PROCEDURE base_for (obj: pc.OBJECT): pc.OBJECT;
BEGIN
  RETURN TOCData.ModuleStartObjects[obj.mno]
END base_for;

PROCEDURE based_var *(obj : pc.OBJECT) : BOOLEAN;
BEGIN
  RETURN (obj.mode = pc.ob_var)            &
         (at.omark_allocated IN obj.marks) &
         (TOCData.ModuleStartObjects <> NIL) &
         (base_for(obj) <> obj);
END based_var;

<* ELSE *>

<* PUSH *>
<* WOFF301+ *>
PROCEDURE based_var *(obj : pc.OBJECT) : BOOLEAN;
BEGIN
  RETURN FALSE
END based_var;
<* POP *>

<* END *>

PROCEDURE gen_fixupB(obj: pc.OBJECT;
                 fx_offs: LONGINT;
                  kind  : SHORTINT);
BEGIN
<* IF TARGET_RISC THEN *>
  IF based_var(obj) THEN
    fx_offs := fx_offs + at.get_global_offset(obj);
    obj := base_for (obj);
  END;
  add_fixup(obj, fx_offs, c_seg.code_len, kind);
  IF (kind = fx_obj32) OR (kind = fx_obj32far) THEN GenLWordB(0) END;
<* ELSE *>
<* IF ~ nodebug THEN *>
  opIO.print("\tfixup");
<* END *>
  add_fixup(obj, fx_offs, c_seg.code_len, kind);
  IF (kind = fx_to_td16) OR (kind = fx_tdrel16) THEN
    GenWordB(0);
  ELSE
    GenLWordB(0);
  END;
<* END *>
END gen_fixupB;

PROCEDURE gen_fixupA(obj: pc.OBJECT;
                 fx_offs: LONGINT;
                  kind  : SHORTINT);
  VAR str, name: ARRAY 512 OF CHAR;
BEGIN
  IF obj # NIL THEN makename(obj, name); END;

  IF kind = fx_cseg32 THEN
    FormStr.print (str, "        .long   CODESEG_ADR");
  ELSIF kind = fx_csegsz32 THEN
    FormStr.print (str, "        .long   CODESEG_SIZE");
  ELSIF kind = fx_to_td16 THEN
    FormStr.print (str, "        .short  TO_TD_FIXUP(%s)", name);
  ELSIF kind = fx_tdrel16 THEN
    FormStr.print (str, "        .short  %s", name);
  ELSE
    FormStr.print (str, "        .long   %s", name);
  END; 

  IF fx_offs # 0 THEN
    FormStr.append (str, "%+d", fx_offs);
  END;
  IF kind = fx_relcall THEN
    FormStr.append (str, "-$");
  ELSIF (kind = fx_tdrel32) OR (kind = fx_tdrel16) THEN
    FormStr.append (str, "-type_desc");
  ELSIF kind = fx_obj32 THEN
<* IF TARGET_RISC THEN *>
    IF obj = at.GlobTOC THEN
      FormStr.print (str, "        .long TOC[tc0]");
    END;
<* END *>
  END;
  GenInstr (str);
END gen_fixupA;

PROCEDURE gen_fixup*(obj: pc.OBJECT;
                 fx_offs: LONGINT;
                  kind  : SHORTINT);
BEGIN
  IF (kind = fx_cseg32) OR (kind = fx_csegsz32) THEN
    ASSERT(obj = NIL);
  ELSE
    ASSERT(obj # NIL);
  END;
  IF at.GENASM IN at.COMP_MODE THEN
    gen_fixupA(obj,fx_offs,kind);
  ELSE
    gen_fixupB(obj,fx_offs,kind)
  END;
END gen_fixup;


PROCEDURE add_pos*(off: LONGINT; pos-: pc.TPOS);
VAR
  new: XREFs;
  i  : LONGINT;
BEGIN
 <* IF DEFINED(OVERDYE) AND OVERDYE THEN *> -- FIXME
  IF at.DbgRefine IN at.COMP_MODE THEN
    IF c_seg.xref_len > 0 THEN
      i := c_seg.xref_len-1;
      ASSERT(c_seg.xref[i].offs <= off);
      IF c_seg.xref[i].txtpos.cmp (pos) = 1 THEN
        RETURN;
      END;
    END;
  END;
 <* END *>
  IF c_seg.xref = NIL THEN ASSERT(c_seg.xref_len = 0);
    NEW(new, 5);
    c_seg.xref := new;
  ELSIF c_seg.xref_len = LEN(c_seg.xref^) THEN
    NEW(new, c_seg.xref_len*2);
    FOR i := 0 TO c_seg.xref_len-1 DO new[i] := c_seg.xref[i] END;
    c_seg.xref := new;
  END;
  c_seg.xref[c_seg.xref_len].offs   := off;
  c_seg.xref[c_seg.xref_len].txtpos := pos;
  INC(c_seg.xref_len);
END add_pos;

PROCEDURE mark_pos*(pos-: pc.TPOS);
BEGIN
  add_pos(c_seg.code_len, pos);
END mark_pos;

PROCEDURE put_ordinal*(v: pc.VALUE; ty: ir.TypeType; sz: ir.SizeType);
VAR
  x: LONGINT;
  x1, x2: SYSTEM.CARD32;
BEGIN
  IF ty = ir.t_int THEN
    IF sz = 8 THEN
      ASSERT(v.get_minBitSize(TRUE) <= 64);
      x1 := v.get_NDWord(0);
      x2 := v.get_NDWord(1);
      GenLWord(x1); GenLWord(x2);
      RETURN;
    ELSE
      x := v.get_integer();
    END;
  END;
  CASE sz OF
  | 1:
       x := SYSTEM.VAL(LONGINT, Calc.ToCardinal(v, sz));
       GenByte(SYSTEM.VAL(SYSTEM.BYTE, x));
  | 2:
       x := SYSTEM.VAL(LONGINT, Calc.ToCardinal(v, sz));
       GenWord(VAL(INT16, x));
  | 4:
       x := SYSTEM.VAL(LONGINT, Calc.ToCardinal(v, sz));
       GenLWord(x);
  | 8:
       GenLWord(v.get_NDWord(0));
       GenLWord(v.get_NDWord(1));
  ELSE
    env.errors.Fault(v.pos, 960); --- "INTERNAL ERROR: invalid constant value"
  END;
END put_ordinal;

PROCEDURE put_nil_value*;
BEGIN
  put_ordinal(Calc.GetNilPointer(), tune.addr_ty, tune.addr_sz);
END put_nil_value;

--------------------------------------------------------------------------------
PROCEDURE MoveCodeFromTo*(sg: CODE_SEGM; from: LONGINT; to: LONGINT);
VAR
  differ, i, j, old_code_len: LONGINT;

BEGIN
  differ := from - to;

  FOR i:=0 TO sg.fxup_len-1 DO
    IF sg.fxup^[i].offs>from THEN
      sg.fxup^[i].offs := sg.fxup^[i].offs - differ;
    END;
  END;

  FOR i:=0 TO sg.xref_len-1 DO
    IF sg.xref^[i].offs>from THEN
      sg.xref^[i].offs := sg.xref^[i].offs - differ;
    END;
  END;

  old_code_len := sg.code_len;
  IF differ>0 THEN
    FOR i:=from TO old_code_len-1 DO
      sg.bcode^[i-differ] := sg.bcode^[i];
    END;
    sg.code_len := old_code_len - differ;
  ELSE
    FOR i:=old_code_len+differ TO old_code_len-1 DO
      GenByte(sg.bcode^[i]);
    END;

    FOR i:=old_code_len-1 TO to BY -1 DO
      sg.bcode^[i] := sg.bcode^[i+differ];
    END;

    FOR i:=from TO to-1 DO
      sg.bcode^[i] := 090H;
    END;
  END;

END MoveCodeFromTo;

--------------------------------------------------------------------------------

(*
  Добавить сегмент 's' в хвост текущего генерируемого
*)
PROCEDURE AddSegmentB(s: CODE_SEGM);
VAR l, j:   INT;
    fxc: fixup_desc;
BEGIN
    l := c_seg.code_len;
    FOR j:=0 TO s.code_len-1 DO
        GenByte (s.bcode^[j]);
    END;
    FOR j:=0 TO s.fxup_len-1 DO
        fxc := s.fxup^[j];
        add_fixup (fxc.obj, fxc.fx_offs, fxc.offs + l, fxc.kind);
    END;
    FOR j:=0 TO s.xref_len-1 DO
        add_pos (s.xref^[j].offs + l, s.xref^[j].txtpos);
    END;
END AddSegmentB;

PROCEDURE AddSegmentA(s: CODE_SEGM);
VAR l, j: INT;
BEGIN
    l := c_seg.code_len;
    FOR j:=0 TO s.code_len-1 DO
        GenInstr (s.acode^[j]^);
    END;
    FOR j:=0 TO s.xref_len-1 DO
        add_pos (s.xref^[j].offs + l, s.xref^[j].txtpos);
    END;
END AddSegmentA;

PROCEDURE AddSegment*(s : CODE_SEGM);
BEGIN
  IF at.GENASM IN at.COMP_MODE THEN AddSegmentA(s) ELSE AddSegmentB(s); END;
END AddSegment;

<* IF TARGET_RISC THEN *>

VAR
  longreal_type : pc.STRUCT;
  F2IVal_AuxConst_ : pc.OBJECT;
  F2UVal_AuxConst_ : pc.OBJECT;
  U2FVal_AuxConst_ : pc.OBJECT;
  I2FVal_AuxConst_ : pc.OBJECT;

PROCEDURE SetVal_AuxConst*(VAR cnst : pc.OBJECT;
                           name-,str1-,str2- : ARRAY OF CHAR);
  VAR nm       : pc.STRING;
      seg, old : CODE_SEGM;
BEGIN
  nm := at.make_name("VAC.%s", name);
  cnst := at.new_work_object(nm, longreal_type,
                                 at.curr_mod.type, pc.ob_cons, FALSE);
  at.alloc_work_object(cnst);
  get_segm(old);
  new_segm(seg); set_segm(seg);
  IF at.GENASM IN at.COMP_MODE THEN
    GenInstr (str1);
    GenInstr (str2);
  ELSE
    ASSERT(FALSE);
  END;
  set_ready(cnst, seg);
  set_segm(old);
END SetVal_AuxConst;

PROCEDURE I2FVal_AuxConst*() : pc.OBJECT;
BEGIN
    IF I2FVal_AuxConst_ = NIL THEN
        SetVal_AuxConst(I2FVal_AuxConst_,
                       "I2F",".long 1127219200",".long -2147483648");
    END;
    RETURN I2FVal_AuxConst_;
END I2FVal_AuxConst;

PROCEDURE U2FVal_AuxConst*() : pc.OBJECT;
BEGIN
    IF U2FVal_AuxConst_ = NIL THEN
        SetVal_AuxConst(U2FVal_AuxConst_,
                        "U2F",".long 1127219200",".long 0");
    END;
    RETURN U2FVal_AuxConst_;
END U2FVal_AuxConst;

PROCEDURE F2UVal_AuxConst*() : pc.OBJECT;
BEGIN
    IF F2UVal_AuxConst_ = NIL THEN
        SetVal_AuxConst(F2UVal_AuxConst_,
                        "F2U",".long 1105199104",".long 0");
    END;
    RETURN F2UVal_AuxConst_;
END F2UVal_AuxConst;

PROCEDURE F2IVal_AuxConst*() : pc.OBJECT;
BEGIN
    RETURN F2IVal_AuxConst_;
END F2IVal_AuxConst;

PROCEDURE InitVal_AuxConst;
BEGIN
    F2IVal_AuxConst_ := NIL;
    F2UVal_AuxConst_ := NIL;
    U2FVal_AuxConst_ := NIL;
    I2FVal_AuxConst_ := NIL;
END InitVal_AuxConst;

<* END *>

<* IF TARGET_RISC THEN *>
PROCEDURE put_floatB(v: pc.VALUE; sz: ir.SizeType);
  VAR r: REAL; lr: LONGREAL; ldr: LONGLONGREAL;
BEGIN
  ldr := v.get_real();
  IF ldr = 0.0 THEN
    ldr := 0.0;
  END;
  IF sz = 4 THEN
    r := VAL (REAL, ldr);
    GenBuf(r);
  ELSIF sz = 8 THEN
    lr := VAL (LONGREAL, ldr);
    GenBuf(lr);
  ELSE
    GenBuf(ldr);
  END;
END put_floatB;

PROCEDURE put_floatA(v: pc.VALUE; sz: ir.SizeType);
VAR str: ARRAY 128 OF CHAR;
    lr:  LONGREAL;
    r :  REAL;

PROCEDURE p(a : ARRAY OF SYSTEM.BYTE);
VAR i : INT32;
BEGIN
  FormStr.print(str,"        .byte ");
  IF inverse_byte_order THEN
    FOR i := LEN(a)-1 TO  0    BY -1 DO
      FormStr.append(str,"%d",SYSTEM.VAL(CARD8,a[i]));
      IF i > 0 THEN FormStr.append(str,", "); END;
    END;
  ELSE
    FOR i := 0        TO  LEN(a) - 1 DO
      IF i > 0 THEN FormStr.append(str,", "); END;
      FormStr.append(str,"%d",SYSTEM.VAL(CARD8,a[i]));
    END;
  END;
END p;

BEGIN
  lr := v.get_real();
  IF lr = 0.0 THEN lr := 0.0; END;
  CASE sz OF
  | 4: ASSERT((lr>=MIN(REAL)) & (lr<=MAX(REAL)));
       r := VAL(REAL,lr);
       p(r);
  | 8: p(lr);
  END;
  GenInstr (str);
END put_floatA;

<* ELSIF TARGET_VAX THEN *>

PROCEDURE put_floatB(v: pc.VALUE; sz: ir.SizeType);
VAR ldr:  LONGLONGREAL;
    tmp4: ARRAY 4 OF SYSTEM.BYTE;
    tmp8: ARRAY 8 OF SYSTEM.BYTE;
BEGIN
  ldr := v.get_real();
  IF ldr = 0.0 THEN
    ldr := 0.0;
  END;
  CASE sz OF
  | 4:  VAX_F.ToVaxReal (ldr, tmp4, 4);
        GenBuf (tmp4);
  | 8:  VAX_F.ToVaxReal (ldr, tmp8, 8);
        GenBuf (tmp8);
  END;
END put_floatB;

PROCEDURE put_floatA(v: pc.VALUE; sz: ir.SizeType);
BEGIN
  ASSERT(FALSE);
END put_floatA;

<* ELSE *>

PROCEDURE put_floatB(v: pc.VALUE; sz: ir.SizeType);
  VAR r: REAL; lr: LONGREAL;ldr: LONGREAL;
BEGIN
  lr := v.get_real();
  IF lr = 0.0 THEN
    lr := 0.0;
  END;
  IF sz = 4 THEN
    r := VAL (REAL, lr);
    GenBuf(r);
  ELSIF sz = 8 THEN
    GenBuf(lr);
  ELSE
--    ASSERT(FALSE)
    ldr := lr;
    GenBuf(ldr);
  END;
END put_floatB;

PROCEDURE put_floatA(v: pc.VALUE; sz: ir.SizeType);
VAR str: ARRAY 64 OF CHAR;
  lr:  LONGREAL;
BEGIN
  lr := v.get_real();
  IF lr = 0.0 THEN
    lr := 0.0;
  END;
  CASE sz OF                 -- byte-wise hex output would be better
  | 4: FormStr.print (str, "        .float  %g", lr);
  | 8: FormStr.print (str, "        .double %g", lr);
  END;
  GenInstr (str);
END put_floatA;

<* END *>

PROCEDURE put_float*(v: pc.VALUE; sz: ir.SizeType);
BEGIN
  IF at.GENASM IN at.COMP_MODE THEN put_floatA(v,sz); ELSE put_floatB(v,sz); END;
END put_float;

PROCEDURE new_integer*(v: pc.VALUE;
                      ty: ir.TypeType; sz: ir.SizeType
                      ): pc.OBJECT;
  VAR o: pc.OBJECT;
    old, seg: CODE_SEGM;
BEGIN
  o := at.new_const(v, ty, sz);
  IF NOT (at.omark_gen_ready IN o.marks) THEN
    get_segm(old);
    new_segm(seg); set_segm(seg);
    put_ordinal(v, ty, sz);
    set_ready(o, seg);
    set_segm(old);
  END;
  RETURN o
END new_integer;

PROCEDURE new_float*(v: pc.VALUE; sz: ir.SizeType): pc.OBJECT;
  VAR o: pc.OBJECT;
    old, seg: CODE_SEGM;
BEGIN
  o := at.new_const(v, ir.t_float, sz);
  IF NOT (at.omark_gen_ready IN o.marks) THEN
    get_segm(old);
    new_segm(seg); set_segm(seg);
    put_float(v, sz);
<* IF NOT TARGET_68k THEN *>
    WHILE (sz MOD 4 ) # 0 DO GenByte(0X); INC(sz) END;   -- alignment ??
<* END *>
    set_ready(o, seg);
    set_segm(old);
  END;
  RETURN o
END new_float;

PROCEDURE EqualSegments * (s1, s2: CODE_SEGM): BOOLEAN;
  VAR ln, i : INT;
BEGIN
  IF s1=s2 THEN RETURN TRUE END;
  IF s1.fxup = NIL THEN
    IF s2.fxup # NIL THEN RETURN FALSE END;
  ELSE
    IF s2.fxup = NIL THEN RETURN FALSE END;
    ln := s1.fxup_len;
    IF ln # s2.fxup_len THEN RETURN FALSE END;
    FOR i := 0 TO ln -1 DO
      IF (s1.fxup[i].obj     # s2.fxup[i].obj) OR
         (s1.fxup[i].fx_offs # s2.fxup[i].fx_offs) OR
         (s1.fxup[i].offs    # s2.fxup[i].offs) OR
         (s1.fxup[i].kind    # s2.fxup[i].kind)
      THEN
        RETURN FALSE
      END;
    END;
  END;
  ln := s1.code_len;
  IF ln # s2.code_len THEN RETURN FALSE END;
  IF at.GENASM IN at.COMP_MODE THEN
    FOR i := 0 TO ln -1 DO
     IF s1.acode[i]^ # s2.acode[i]^ THEN RETURN FALSE END;
    END;
  ELSE
    FOR i := 0 TO ln -1 DO
       IF s1.bcode[i] # s2.bcode[i] THEN RETURN FALSE END;
    END;
  END;
  RETURN TRUE
END EqualSegments;

PROCEDURE IsPrefixOf*(s1, s2: CODE_SEGM): BOOLEAN;
  VAR ln, i : INT;
BEGIN
  IF s1=s2 THEN RETURN TRUE END;
  IF s1.fxup # NIL THEN
    IF s2.fxup = NIL THEN RETURN FALSE END;
    ln := s1.fxup_len;
    IF ln > s2.fxup_len THEN RETURN FALSE END;
    FOR i := 0 TO ln-1 DO
      IF (s1.fxup[i].obj     # s2.fxup[i].obj)     OR
         (s1.fxup[i].fx_offs # s2.fxup[i].fx_offs) OR
         (s1.fxup[i].offs    # s2.fxup[i].offs)    OR
         (s1.fxup[i].kind    # s2.fxup[i].kind)
      THEN
        RETURN FALSE
      END;
    END;
  END;
  ln := s1.code_len;
  IF ln > s2.code_len THEN RETURN FALSE END;
  IF at.GENASM IN at.COMP_MODE THEN
    FOR i := 0 TO ln -1 DO
      IF s1.acode[i]^ # s2.acode[i]^ THEN RETURN FALSE END;
    END
  ELSE
    FOR i := 0 TO ln -1 DO
      IF s1.bcode[i] # s2.bcode[i] THEN RETURN FALSE END;
    END;
  END;
  RETURN TRUE
END IsPrefixOf;

PROCEDURE IsGoodConst*(loc: ir.Local) : BOOLEAN;
  VAR o: pc.OBJECT; sg: CODE_SEGM;
BEGIN
  IF at.GENASM IN at.COMP_MODE THEN
    RETURN FALSE
  ELSE
    o := ir.Locals[loc].Obj;
    IF (o = NIL)
       OR (o.mode # pc.ob_cons)
       OR NOT(at.omark_gen_ready IN o.marks)
    THEN
      RETURN FALSE
    ELSE
      sg := get_ready(o);
      RETURN (sg.fxup = NIL);
    END;
  END;
END IsGoodConst;

PROCEDURE GetConstByte*(loc: ir.Local; inx: LONGINT) : SYSTEM.BYTE;
  VAR sg: CODE_SEGM;
BEGIN
  IF at.GENASM IN at.COMP_MODE THEN
    ASSERT(FALSE);
  ELSE
    sg := get_ready(ir.Locals[loc].Obj);
    IF inx < sg.code_len THEN
      RETURN sg.bcode[inx]
    ELSE
      RETURN 0X
    END;
  END;
END GetConstByte;

--------------  Context iteration  ---------------------------
                -----------------

TYPE iterated_proc* = PROCEDURE(o: pc.OBJECT);

PROCEDURE iter_context*(md: pc.OBJECT; P: iterated_proc);

  PROCEDURE obj_list(o: pc.OBJECT);
  BEGIN
    WHILE o # NIL DO
      IF (o.mode IN pc.PROCs) OR
       (o.mode = pc.ob_type) & (o.type.mode=pc.ty_record) & (o.type.flag IN pc.OOP_langs)
      THEN 
        obj_list(o.type.mem);
      END;
      P(o);
      o := o.next;
    END;
  END obj_list;

  VAR o: pc.OBJECT;
BEGIN                                  -- это надо заменить итератором из pcK
  obj_list(md.type.prof);
  obj_list(md.type.mem);
  o := at.work_objects;
  WHILE o # NIL DO P(o); o := o.next END;
  P(md);
END iter_context;

PROCEDURE get_size * (obj : pc.OBJECT) : LONGINT;
<* IF TARGET_RISC THEN *>
VAR
    o : pc.OBJECT;
    offs: LONGINT;
    align: SHORTINT;
    const: BOOLEAN;
    al0  : SHORTINT;
    GlobalTotalSize : LONGINT;
BEGIN
  ASSERT(~based_var(obj));
  IF TOCData.ModuleStartObjects = NIL THEN
    RETURN pc.code.get_size(pc.su_bytes, obj.type);
  ELSIF TOCData.ModuleStartObjects[obj.mno] = obj THEN
    (* it's a base - calculate total size *)
    (*
       find obj thru getFirstData{GetNextData} calls
       to obtain obj place in Data chain
    *)
    TOCData.GetFirstData(o,offs,align,const);
<* IF ~NODEBUG THEN *>
    ASSERT(o <> NIL);
<* END *>
    WHILE o <> obj DO
      TOCData.GetNextData(o,offs,align,const);
<* IF ~NODEBUG THEN *>
      ASSERT(o <> NIL);
<* END *>
    END;
    GlobalTotalSize := pc.code.get_size(pc.su_bytes, o.type);
    al0 := 16;
    (*
       the following calls of GetNextData will return in 'o' based objects,
       while at.omark_allocated in o.marks
    *)
    LOOP
      TOCData.GetNextData(o,offs,align,const);
      IF (o = NIL) OR ~(at.omark_allocated IN o.marks) THEN
        (* based objects are exhausted *)
        EXIT
      END;
      IF al0 < align THEN
        (* add align bytes to GlobalTotalSize *)
        GlobalTotalSize :=
          (GlobalTotalSize + align - 1) DIV align * align;
      END;
      al0 := align;
      INC(GlobalTotalSize, pc.code.get_size(pc.su_bytes, o.type));
    END;
    RETURN GlobalTotalSize;
  ELSE
    (* self-based object - usual sizing *)
    RETURN pc.code.get_size(pc.su_bytes, obj.type);
  END;
<* ELSE *>
BEGIN
  RETURN pc.code.get_size(pc.su_bytes, obj.type);
<* END *>
END get_size;

PROCEDURE get_align*(obj : pc.OBJECT) : SHORTINT;
BEGIN
  IF obj.type = NIL THEN  RETURN 4  END;
<* IF TARGET_RISC THEN *>
--  ASSERT(~based_var(obj));
  IF (TOCData.ModuleStartObjects # NIL) &
     (TOCData.ModuleStartObjects[obj.mno] = obj)
  THEN
    (* it's a base - always align by MAXIMUM*)
    RETURN 16;
  ELSE
    (* self-based object - usual alignment*)
    RETURN pc.code.get_align(obj.type);
  END;
<* ELSE *>
  RETURN pc.code.get_align(obj.type);
<* END *>
END get_align;
----------------------------- MISC -----------------------------
                             ------
PROCEDURE Init  *;
BEGIN
  c_seg := NIL;
  ss_cnt := 0;
  inverse_byte_order := env.config.Option(plt.TARGET_BIGENDIAN)
                               <> env.config.Option(plt.HOST_BIGENDIAN);
<* IF TARGET_RISC THEN *>
  longreal_type := pc.new_type(pc.ty_longreal);
  longreal_type.align := 8;
  longreal_type.pos := ir.NullPos;
  longreal_type.end := ir.NullPos;
  InitVal_AuxConst;
<* END *>
END Init;

PROCEDURE Exit*;
BEGIN
  c_seg := NIL;
<* IF TARGET_RISC THEN *>
  longreal_type := NIL;
  InitVal_AuxConst;
  IF env.config.Option (GREEN_HILLS) THEN  
    COPY('        .ascii  "', str_begin);  
  ELSE
    COPY('        .string "', str_begin);  
  END;
<* END *>
END Exit;

END CodeDef.
