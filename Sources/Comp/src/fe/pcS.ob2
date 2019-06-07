(** Copyright (c) 1991,97 XDS Ltd, Russia. All Rights Reserved. *)
(** o2/m2 front-end: Scanner *)
MODULE pcS; (** Ned 14-Apr-89. *)
     (** Ned 06-Jan-90. *)
     (** Sem 20-Sep-93. *)

IMPORT pcK, xfs:=xiFiles, env:=xiEnv, str:=DStrings, Strings,SYSTEM;

TYPE
  Symbol * = CHAR;

CONST
  CPPCOMMENTS = "CPPCOMMENTS";

CONST
  none          * = 00X;
  ident         * = 01X;
  val_integer   * = 02X;
  val_real      * = 50X;
  val_long_real * = 51X;
  val_cmplx     * = 55X;
  val_long_cmplx* = 56X;
  val_char      * = 52X;
  val_string    * = 53X;
  exponent      * = 54X;
  shift_l       * = 55X;
  shift_r       * = 56X;
  equ           * = 03X;
  neq           * = 04X;
  lss           * = 05X;
  gtr           * = 06X;
  leq           * = 07X;
  geq           * = 08X;
  times         * = 09X;  (* * *)
  slash         * = 0AX;
  minus         * = 0BX;
  plus          * = 0CX;
  sep           * = 0DX;  (* | *)
  semic         * = 0EX;
  bar           * = 0FX;
  period        * = 10X;
  colon         * = 11X;
  lbr           * = 12X;
  rbr           * = 13X;
  lpar          * = 14X;
  rpar          * = 15X;
  lbrace        * = 16X;
  rbrace        * = 17X;
  comma         * = 18X;  (* , *)
  becomes       * = 19X;
  range         * = 1AX;
                     
  (* key words *)    
  and           * = 1BX;
  asm           * = 1CX;
  array         * = 1DX;
  begin         * = 1EX;
  by            * = 1FX;
  case          * = 20X;
  const         * = 21X;
  definition    * = 22X;
  div           * = 23X;
  do            * = 25X;
  else          * = 26X;
  elsif         * = 27X;
  end           * = 28X;
  except        * = 29X;
  exit          * = 2AX;
  export        * = 2BX;
  finally       * = 2CX;
  for           * = 2DX;
  forward       * = 2EX;
  from          * = 2FX;
  if            * = 30X;
  implementation* = 31X;
  import        * = 32X;
  in            * = 33X;
  is            * = 34X;
  loop          * = 35X;
  mod           * = 36X;
  module        * = 38X;
  not           * = 39X;
  of            * = 3AX;
  or            * = 3BX;
  packedset     * = 3CX;
  pointer       * = 3DX;
  procedure     * = 3EX;
  qualified     * = 3FX;
  record        * = 40X;
  rem           * = 41X;
  repeat        * = 42X;
  retry         * = 43X;
  return        * = 44X;
  seq           * = 45X;
  set           * = 46X;
  then          * = 47X;
  to            * = 48X;
  type          * = 49X;
  until         * = 4AX;
  var           * = 4BX;
  while         * = 4CX;
  with          * = 4DX;
  label         * = 4EX;
  goto          * = 4FX;
  alias         * = 50X; (* TopSpeed's '::=' token *)
<* IF MCS THEN *>
  (* MCS key words *)
  inline        * = 51X;
  setreg        * = 52X;

  first_keyword * = and;
  last_keyword  * = setreg;
<* ELSE *>

  first_keyword * = and;
  last_keyword  * = alias;
<* END *>


--  pragma_end = 70X;

VAR
  name    * : pcK.NAME;         (** identifier value          *)
  val     * : pcK.VALUE;        (** literal value             *)
  val_hex * : BOOLEAN;          (** value is hexadecimal      *)
  txtpos  - : env.TPOS;         (** position in source text   *)
  prevpos - : pcK.TPOS;
  scan      : BOOLEAN;

 (** compilation modes *)
  oberon *  : BOOLEAN;          (** Oberon-2 / NOT Modula-2   *)
  add_goto  : BOOLEAN;
  enh_deref-: BOOLEAN;

(**----------------------------------------------------------------*)

PROCEDURE ^ get*(VAR sy: Symbol);

(**----------------------------------------------------------------*)

PROCEDURE ^ error*(pos: env.TPOS; type: CHAR;
     no: INTEGER; SEQ x: pcK.BYTE);
(** type: e - error, w - warning, f - fatal error *)

PROCEDURE err*(no: INTEGER; SEQ x: pcK.BYTE);
BEGIN
  error(txtpos,'e',no,x);
END err;

PROCEDURE fault*(no: INTEGER; SEQ x: pcK.BYTE);
BEGIN
  error(txtpos,'f',no,x);
  ASSERT(FALSE);
END fault;

PROCEDURE warn*(no: INTEGER; SEQ x: pcK.BYTE);
BEGIN
  env.errors.Warning(txtpos,no,x);
END warn;

PROCEDURE ^ expc* (s: Symbol);

CONST
  err_end_of_file         = 5;
  err_file_io             = 10;
  err_pragma_not_closed   = 173;
  err_comment_not_closed  = 2;
  err_end_of_file_skiping = 174;
  err_ident_expected      = 7;
  err_invalid_cc_option   = 171;
  err_invalid_cc_prefix   = 172;
  err_illegal_char        = 1;
  err_invalid_number      = 3;
  err_too_long_string     = 4;
  err_string_expected     = 11;
  err_too_long_ident      = 6;
  err_extension_not_allowed = 102;
  err_invalid_pragma_syntax = 175;
  err_too_big_char        = 12;

  wrn_unknown_option   = 320;
  wrn_defined_option   = 321;
  wrn_unknown_equation = 322;
  wrn_defined_equation = 323;
  wrn_obsolete_pragma  = 390;

(*---------------------- Options ----------------------------------*)

PROCEDURE option(s-: ARRAY OF CHAR): BOOLEAN;
  VAR val: BOOLEAN;
BEGIN
  IF NOT scan THEN RETURN FALSE END;
  val:=env.config.Option(s);
  IF env.config.res#env.ok THEN warn(wrn_unknown_option,s) END;
  RETURN val
END option;

PROCEDURE set_option(s-: ARRAY OF CHAR; val: BOOLEAN);
BEGIN
  IF NOT scan THEN RETURN END;
  env.config.SetOptionAt(txtpos,s,val);
  IF env.config.res#env.ok THEN warn(wrn_unknown_option,s) END;
END set_option;

PROCEDURE dcl_option(s-: ARRAY OF CHAR; val: BOOLEAN);
BEGIN
  IF NOT scan THEN RETURN END;
  env.config.NewOption(s,val,SYSTEM.VAL(env.CompilerOption,-1));
  IF env.config.res#env.ok THEN warn(wrn_defined_option,s) END;
END dcl_option;

PROCEDURE set_equation(s-,val-: ARRAY OF CHAR);
BEGIN
  IF NOT scan THEN RETURN END;
  env.config.SetEquation(s,val);
  IF env.config.res#env.ok THEN warn(wrn_unknown_equation,s) END;
END set_equation;

PROCEDURE dcl_equation(s-: ARRAY OF CHAR);
BEGIN
  IF NOT scan THEN RETURN END;
  env.config.NewEquation(s);
  IF env.config.res#env.ok THEN warn(wrn_defined_equation,s) END;
END dcl_equation;

PROCEDURE ext*(): BOOLEAN;
BEGIN
  IF oberon THEN RETURN env.o2_ext IN env.config.tags END;
  RETURN env.m2_ext IN env.config.tags;
END ext;

PROCEDURE TS_ext*(): BOOLEAN;
BEGIN
  IF oberon THEN RETURN FALSE END;
  RETURN env.ts_ext IN env.config.tags;
END TS_ext;

PROCEDURE M2_ext*(): BOOLEAN;
BEGIN
  IF oberon THEN RETURN FALSE END;
  RETURN env.m2_ext IN env.config.tags;
END M2_ext;

(*---------------------- Source text ------------------------------*)

CONST
  EOL  = pcK.EOL;
  EOF  = CHR(ORD(EOL)+1);

VAR
  digit_or_letter: ARRAY ORD(MAX(CHAR))+1 OF BOOLEAN;

  char     : CHAR;    (* current source text char *)
  src_line : LONGINT; (* source char line number *)
  src_col  : LONGINT; (* source char column      *)
  src_pos  : LONGINT; (* position in the source text - for progress indicator *)
  src_file : xfs.TextFile;
  src_buf  : ARRAY 512 OF CHAR;
  src_bpos : INTEGER;

PROCEDURE getchar;
BEGIN
  INC(src_col);
  IF char=EOL THEN
    INC(src_line); src_col:=0;
  END;
  char:=src_buf[src_bpos];
  WHILE char=0X DO
    src_bpos:=0;
    src_file.ReadString(src_buf);
    IF src_file.readRes=xfs.allRight THEN
      INC(src_pos,src_file.readLen);
    ELSIF src_file.readRes=xfs.endOfLine THEN
      (* end of line seen before expected data *)
      char:=EOL; RETURN;
    ELSIF src_file.readRes=xfs.endOfInput THEN
      (* end of input seen before expected data *)
      char:=EOF; RETURN;
    ELSE
      fault(err_file_io);
    END;
    char:=src_buf[0];
  END;
  INC(src_bpos);
END getchar;

PROCEDURE read_all*;
BEGIN
  WHILE char#EOF DO getchar END;
END read_all;

(*----------------------------------------------------------------*)

CONST
  _range = 177C;

VAR
  comment    : LONGINT; (* first line of comment *)
  state_level: INTEGER;
  len        : INTEGER;
  string     : ARRAY 256 OF CHAR;

  (* options *)
  iso_pragma : BOOLEAN;
  xcomments  : BOOLEAN; (* exported comments *)
  comments   : BOOLEAN; (* all comments *)
  o2_num_ext*: BOOLEAN;

(*---------------------------------------------------------------*)

TYPE
  Level = POINTER TO LevelDesc;
  LevelDesc = RECORD
    scan  : BOOLEAN;
    active: BOOLEAN;
    entry : Symbol;
    next  : Level;
  END;

VAR
  level   : Level;
  skip_ln : LONGINT;    (* start line of skiping   *)
  skip_ps : LONGINT;    (* start column of skiping *)
  skip_fnm: env.String;
  WasControl: BOOLEAN;

PROCEDURE Defined(): BOOLEAN;
  VAR e: env.String;
BEGIN
  IF env.config.Option(name) OR (env.config.res=env.ok) THEN
    RETURN TRUE;
  END;
  env.config.Equation(name,e);
  RETURN (env.config.res=env.ok) & (e#NIL);
END Defined;

PROCEDURE If(VAR sy: Symbol; entry: BOOLEAN);

  PROCEDURE Expr(): BOOLEAN;

    TYPE EXPR = ARRAY 1024 OF CHAR;

    PROCEDURE Boolean(s-: EXPR; resolve: BOOLEAN): BOOLEAN;
    BEGIN
      IF s="FALSE" THEN RETURN FALSE END;
      IF s="TRUE" THEN RETURN TRUE END;
      IF scan & resolve THEN error(txtpos,'e',30); END;
      RETURN FALSE;
    END Boolean;

    PROCEDURE String(VAR s: EXPR; b: BOOLEAN);
    BEGIN
      IF b THEN COPY("TRUE",s) ELSE COPY("FALSE",s) END;
    END String;

    PROCEDURE Ident(VAR x: EXPR; resolve: BOOLEAN);
      VAR s: env.String;
    BEGIN
      IF sy=ident THEN
        String(x,env.config.Option(name));
        IF env.config.res#env.ok THEN
          env.config.Equation(name,s);
          IF (env.config.res=env.ok) THEN 
            IF s=NIL THEN COPY('', x);
            ELSE          COPY(s^, x);
            END;
          ELSIF scan & resolve THEN
            warn(wrn_unknown_option,name)
          END;
        END;
        get(sy);
      ELSIF sy=val_string THEN
        COPY(string,x); get(sy);
      ELSE
        expc(ident); COPY('', x); get(sy);
      END;
    END Ident;

    PROCEDURE ^ Relation(VAR z: EXPR; resolve: BOOLEAN);

    PROCEDURE Factor(VAR z: EXPR; resolve: BOOLEAN);
    BEGIN
      IF sy=lpar THEN
        get(sy); Relation(z, resolve);
        IF sy#rpar THEN expc(rpar) END;
        get(sy);
      ELSIF sy=not THEN
        get(sy);
        Factor(z, resolve);
        String(z,~Boolean(z, TRUE));
      ELSIF sy=ident THEN
        IF name="DEFINED" THEN
          get(sy);
          IF sy#lpar  THEN expc(lpar)  ELSE get(sy) END;
          IF sy#ident THEN expc(ident) ELSE String(z,Defined()); get(sy) END;
          IF sy#rpar  THEN expc(rpar)  ELSE get(sy) END;
        ELSE
          Ident(z, resolve);
        END;
      ELSE
        Ident(z, resolve);
      END;
    END Factor;

    PROCEDURE AndExpr(VAR z: EXPR; resolve: BOOLEAN);
    VAR
      x  : EXPR;
      res: BOOLEAN;
    BEGIN
      Factor(z, resolve);
      WHILE sy=and DO
        get(sy);
        res := Boolean(z, resolve);
        Factor(x, res);
        String(z, res & Boolean(x, resolve));
      END;
    END AndExpr;

    PROCEDURE OrExpr(VAR z: EXPR; resolve: BOOLEAN);
    VAR
      x: EXPR;
      res: BOOLEAN;
    BEGIN
      AndExpr(z, resolve);
      WHILE sy=or DO
        get(sy);
        res := Boolean(z, resolve);
        AndExpr(x, NOT res);
        String(z, res OR Boolean(x, resolve));
      END;
    END OrExpr;

    PROCEDURE Relation(VAR z: EXPR; resolve: BOOLEAN);
      VAR x: EXPR; not: BOOLEAN;
    BEGIN
      OrExpr(z, resolve);
      IF (sy=equ) OR (sy=neq) THEN
        not:=sy=neq; get(sy);
        OrExpr(x, resolve);
    Strings.Capitalize(z);
    Strings.Capitalize(x);
        String(z,not # (z = x));
      END;
    END Relation;

    VAR x: EXPR;

  BEGIN
    Relation(x, TRUE);
    RETURN Boolean(x, TRUE);
  END Expr;

  VAR val: BOOLEAN; c: Level;
BEGIN
  IF entry THEN
    NEW(c);
    c.active:=scan;
    c.scan:=scan;
    c.entry:=if;
    c.next:=level; level:=c;
  ELSE
    IF (level=NIL) OR (level.entry#if) THEN err(err_invalid_cc_option)
    ELSIF scan & level.active THEN level.active:=FALSE
    END;
  END;
  scan:=level.scan;
  get(sy);
  val:=Expr();
  IF sy=then THEN get(sy) ELSE expc(then) END;
  scan:=val & (level#NIL) & level.active;
END If;

PROCEDURE Else;
BEGIN
  IF (level=NIL) OR (level.entry#if) THEN
    err(err_invalid_cc_option)
  ELSE
    scan:=~ scan & level.active;
  END;
END Else;

PROCEDURE End;
  VAR c: Level;
BEGIN
  IF level=NIL THEN
    err(err_invalid_cc_option)
  ELSE
    c:=level;
    scan:=c.scan;
    level:=c.next;
  END;
END End;

PROCEDURE Prags(VAR sy: Symbol);
  VAR nm: pcK.NAME;
BEGIN
  ASSERT(char='$');
  getchar;
  LOOP
    IF    char = ' ' THEN
      getchar;
    ELSIF char = '<' THEN
      env.config.Save;
      getchar;
    ELSIF char = '>'THEN
      env.config.RestoreAt(txtpos);
      IF env.config.Level() = state_level THEN env.config.Save END;
      getchar;
    ELSIF char = '|' THEN
      WHILE env.config.Level() >= state_level DO
        env.config.RestoreAt(txtpos);
      END;
      env.config.Save;
      getchar;
    ELSE
      get(sy);
      IF sy = ident THEN
        nm:=name;
        get(sy);
        IF (sy = plus) OR (sy = minus) THEN
          set_option(nm,(sy=plus));
        ELSE
          err(err_invalid_pragma_syntax); EXIT;
        END;
      ELSE
        EXIT
      END;
    END;
  END;
END Prags;

PROCEDURE Control;

  PROCEDURE CondStatement(VAR sy: Symbol);
    VAR dcl: BOOLEAN; nm: pcK.NAME;
  BEGIN
    IF    sy=if    THEN If(sy,TRUE)
    ELSIF sy=elsif THEN If(sy,FALSE)
    ELSIF sy=else  THEN Else; get(sy)
    ELSIF sy=end   THEN End;  get(sy)
    ELSIF sy=ident THEN (* [NEW] ident [+|-|="string"] *)
      IF name = "POP" THEN
        env.config.RestoreAt(txtpos);
        IF env.config.Level() = state_level THEN env.config.Save END;
        get(sy);
      ELSIF name = "PUSH" THEN
        env.config.Save;
        get(sy);
      ELSE
        dcl:=(name="NEW");
        IF dcl THEN get(sy) END;
        IF sy#ident THEN expc(ident)
        ELSE
          nm:=name; get(sy);
          IF sy = equ THEN
            get(sy);
            IF sy = val_string THEN
              IF dcl THEN dcl_equation(nm) END;
              set_equation(nm,string);
              get(sy);
            ELSE err(418)
            END;
          ELSIF (sy=plus) OR (sy=minus) THEN
            IF dcl THEN dcl_option(nm,(sy=plus))
            ELSE set_option(nm,(sy=plus));
            END;
            get(sy);
          ELSIF dcl THEN dcl_option(nm,FALSE)
          ELSE err(err_invalid_pragma_syntax)
          END;
        END;
      END;
    ELSE err(err_invalid_cc_prefix)
    END;
  END CondStatement;

  VAR c: CHAR; sy: Symbol; line,col: LONGINT; fnm: env.String;
BEGIN
  IF WasControl THEN
    err(err_invalid_cc_prefix);
  ELSE
    WasControl := TRUE;
  END;
  txtpos.unpack(fnm,line,col);
  getchar;
  c:=char;
  IF c='$' THEN
    Prags(sy);
  ELSIF c='#' THEN
    getchar; get(sy);
    warn(wrn_obsolete_pragma);
    IF    sy=if    THEN If(sy,TRUE)
    ELSIF sy=elsif THEN If(sy,FALSE)
    ELSIF sy=else  THEN Else; get(sy)
    ELSIF sy=end   THEN End;  get(sy)
    ELSE err(err_invalid_cc_prefix)
    END;
  ELSE
    WHILE c=' ' DO getchar; c:=char END;
    IF (c='+') OR (c='-') THEN
      getchar; get(sy);
      IF sy#ident THEN expc(ident)
      ELSE set_option(name,(c='+') OR (c=':')); get(sy)
      END;
    ELSE
      get(sy); CondStatement(sy)
    END;
  END;
  IF (sy=times) & (char='>') THEN
    getchar;
<* IF MCS THEN *>
  ELSIF (sy=times) & (char=')') THEN
    getchar;
<* END *>
  ELSE
    err(err_invalid_pragma_syntax);
    LOOP
      IF char='*' THEN getchar;
        IF char='>' THEN getchar; RETURN 
      <* IF MCS THEN *>
        ELSIF char=')' THEN getchar; RETURN 
      <* END *>
        END;
      ELSIF char=EOF THEN fault(err_pragma_not_closed,line);
      ELSE getchar;
      END
    END;
  END;
  WasControl := FALSE;
END Control;

PROCEDURE TS_Control;
VAR
  c     : CHAR;
  sy    : Symbol;
  line,
  col   : LONGINT;
  fnm   : env.String;
  val   : BOOLEAN;
  l     : Level;
BEGIN
  txtpos.unpack(fnm,line,col);
  getchar;
  c:=char;
  CASE c OF
  | 'T', 'F':
    getchar; get(sy);
    IF sy#ident THEN expc(ident)
    ELSE
      NEW(l);
      l.active:=scan;
      l.scan:=scan;
      l.entry:=if;
      l.next:=level; level:=l;
      val := env.config.Option(name);
      IF c = 'F' THEN
        val := NOT val;
      END;
      scan:=val & level.active;
      get(sy);
    END;
  | 'E':
    getchar; End; get(sy);
  ELSE
    sy := none;
    err(err_invalid_cc_prefix)
  END;

  IF (sy=times) & (char=')') THEN
    getchar;
  ELSE
    err(err_invalid_pragma_syntax);
    LOOP
      IF char='*' THEN getchar;
        IF char=')' THEN getchar; RETURN END;
      ELSIF char=EOF THEN fault(err_pragma_not_closed,line);
      ELSE getchar;
      END
    END;
  END;
END TS_Control;

PROCEDURE Comment(save,line: BOOLEAN; comment_char_must_end:=')':CHAR);

  PROCEDURE append;
    VAR c: pcK.Comment;
  BEGIN
    NEW(c);
    c.tags:=pcK.CTAG_SET{};
    c.pos:=env.null_pos;
    NEW(c.str,len+1); string[len]:=0C; COPY(string,c.str^);
    IF pcK.comments=NIL THEN
      pcK.comments:=c; c.next:=c;
    ELSE
      c.next:=pcK.comments.next;
      pcK.comments.next:=c;
      pcK.comments:=c;
    END;
  END append;

  CONST lim = LEN(string)-9;

  PROCEDURE getc;
  BEGIN
    IF save THEN
      IF len>lim THEN append; len:=0 END;
      string[len]:=char; INC(len);
    END;
    getchar;
  END getc;

  PROCEDURE Comment0(save0: BOOLEAN; comment_char_must_end:=')':CHAR);
    VAR c0: LONGINT;
  BEGIN
    c0:=comment;
    comment:=src_line;
    LOOP
      WHILE char>='@' DO getc END; (* to speed up skiping comment *)
      IF NOT line & (char='*') THEN
        getc;
        IF(*~env.config.Option(CPPCOMMENTS)&*)(char=')')&(char=comment_char_must_end) THEN
          comment:=c0; RETURN
        END;
        IF(* env.config.Option(CPPCOMMENTS)&*)(char='/')&(char=comment_char_must_end) THEN
          comment:=c0; RETURN
        END;
      ELSIF NOT line & (char='(')&~env.config.Option(CPPCOMMENTS) THEN
        getc;
        IF char='*' THEN getc; Comment0(save0); getc END;
      ELSIF NOT line & (char='/')&env.config.Option(CPPCOMMENTS) THEN
        getc;
        IF char='*' THEN getc; Comment0(save0,'/'); getc END;
      ELSIF line & (char=EOL) THEN
        comment:=c0; RETURN;
      ELSIF (char=EOF) OR (src_line>comment+10000) THEN
        error(txtpos,'s',err_comment_not_closed,comment+1);
      ELSE
        getc;
      END
    END;
  END Comment0;

  VAR prev: pcK.Comment; sy: Symbol; end: pcK.TPOS;
BEGIN
  IF (char='$') & scan & NOT line THEN
    warn(wrn_obsolete_pragma); Prags(sy);
    IF (sy=times) & (char=')') THEN getchar; RETURN END;
  END;
  IF save THEN
    prev:=pcK.comments; len:=2;
    IF line THEN string:='--' ELSE string:='(*' END;
    Comment0(TRUE,comment_char_must_end);
    end.pack(src_file.name^,src_line,src_col);
    getc;
    IF len#0 THEN append END;
    IF prev=NIL THEN prev:=pcK.comments END;
    ASSERT(prev#NIL);
    prev.next.pos:=txtpos;
    prev.next.end:=end;
  ELSE
    Comment0(FALSE,comment_char_must_end);
    getc;
  END;
END Comment;

PROCEDURE Skip;
BEGIN
  getchar;
  LOOP
    IF char='<' THEN getchar;
      IF char='*' THEN
        Control;
        IF scan THEN RETURN END;
      END;
    ELSIF char='(' THEN
      getchar;
      IF char='*' THEN
        getchar;
        IF TS_ext() & (char='%') THEN
          TS_Control;
          IF scan THEN RETURN END;
      <* IF MCS THEN *>
        ELSIF (char='$') THEN
          Control;
          IF scan THEN RETURN END;
      <* END *>
        ELSE
          Comment(FALSE,FALSE)
        END
      END;
    ELSIF char='-' THEN getchar;
      IF char='-' THEN getchar; Comment(FALSE,TRUE) END
    ELSIF char=EOF THEN
      fault(err_end_of_file_skiping,skip_ln);
    ELSE getchar()
    END
  END;
END Skip;

(*---------------------------------------------------------------*)

VAR
  val_ai  : pcK.VALUE;
  zz_tmp  : pcK.VALUE;

  type_string : pcK.STRUCT;


PROCEDURE char2SS*(v:pcK.VALUE;pos:pcK.TPOS):pcK.VALUE;
VAR l:LONGINT;
    val:pcK.VALUE;
BEGIN
   l:=v.get_integer();
   IF l=0 THEN
      string[0]:=0X;
      type_string.len:=1;
   ELSE
      string[0]:=CHR(l);
      string[1]:=0X;
      type_string.len:=2;
   END;
   val:=pcK.value.new(pos,type_string);
   val.set_string(string);
   RETURN val;
END char2SS;

PROCEDURE Number(): CHAR;
  VAR
    bf   : ARRAY 256 OF CHAR;
    i    : INTEGER;

  PROCEDURE Float(): Symbol;
    VAR long,cmplx: BOOLEAN;
  BEGIN
    WHILE (char>='0') & (char<='9') DO
      bf[i]:=char; INC(i); getchar;
    END;
    long:=FALSE;
    IF (char='E') OR (oberon & (char='D')) THEN
      IF char='D' THEN long:=TRUE END;
      bf[i]:=char; INC(i); getchar;
      IF (char='+') OR (char='-') THEN
        bf[i]:=char; INC(i); getchar;
      ELSE
        bf[i]:='+';
      END;
      IF (char<'0') OR (char>'9') THEN
        err(err_invalid_number);
      ELSE
        REPEAT
          bf[i]:=char; INC(i); getchar;
        UNTIL (char<'0') OR (char>'9');
      END;
    END;
    cmplx:=FALSE;
    IF oberon & (char='i') THEN getchar; cmplx:=TRUE END;
    IF digit_or_letter[ORD(char)] THEN err(err_invalid_number) END;
    bf[i]:=0X;
    IF cmplx THEN val:=pcK.value.new(txtpos,pcK.CC_type);
    ELSE val:=pcK.value.new(txtpos,pcK.RR_type);
    END;
    val.str_to_value(bf,pcK.flag_o2);
    IF cmplx THEN
      IF long THEN RETURN val_long_cmplx END;
      RETURN val_cmplx;
    ELSE
      IF long THEN RETURN val_long_real END;
      RETURN val_real;
    END;
  END Float;

  VAR
    dig_a: INTEGER; (* 'A'..'F' *)
    dig_8: INTEGER; (* '8'..'F' *)
    ch   : CHAR;
    last : CHAR;
    l    : LONGINT;

BEGIN
  val_hex:=FALSE;
  dig_a:=0;
  dig_8:=0;
  last:=0X;
  i:=0;
  LOOP
    IF ext() THEN ch:=CAP(char) ELSE ch:=char END;
    IF (char>='0') & (char<='9') THEN
      IF char>='8' THEN INC(dig_8) END;
    ELSIF (ch >= 'A') & (ch <= 'F') THEN
      INC(dig_a); INC(dig_8);
    ELSE EXIT
    END;
    bf[i]:=ch; INC(i);
    last:=char; getchar;
  END;
  ASSERT(last#0X);
  IF ch='.' THEN
    getchar;
    IF char='.' THEN
      char := _range;
    ELSE
      IF dig_a>0 THEN err(err_invalid_number) END;
      bf[i]:='.'; INC(i);
      RETURN Float();
    END;
  END;

  IF ch='H' THEN
    bf[i]:=ch; INC(i); bf[i]:=0X;
    val:=pcK.value.new(txtpos, pcK.ZZ_type);
    val.set_radix(16);
    val.str_to_value(bf,pcK.flag_o2);
    getchar;
    val_hex:=TRUE;
    RETURN val_integer;
  END;
  IF ch='X' THEN
    bf[i]:=ch; INC(i); bf[i]:=0X;
    val:=pcK.value.new(txtpos,pcK.ZZ_type);
    IF ~ oberon & ~ ext() THEN err(err_invalid_number);
    ELSE
      val.str_to_value(bf,pcK.flag_o2);
      zz_tmp.binary(pcK.sb_leq,val,pcK.char_type.max);
      IF zz_tmp.is_zero() THEN
        err(err_too_big_char);
        val.set_integer(0);
      END;
    END;
    getchar;
    val_hex:=TRUE;
    RETURN val_char;
  END;
  IF last='B' THEN
    bf[i]:=0X;
    val:=pcK.value.new(txtpos,pcK.ZZ_type);
    val.set_radix(8);
    IF (oberon & ~ ext()) OR (dig_8>1) THEN err(err_invalid_number)
    ELSE val.str_to_value(bf,pcK.flag_m2);
    END;
    RETURN val_integer;
  END;
  IF last='C' THEN
    bf[i]:=0X;
    IF (oberon & ~ ext()) OR (dig_8>1) THEN
      err(err_invalid_number); l:=0;
    ELSE
      val_ai.str_to_value(bf,pcK.flag_m2);
      zz_tmp.binary(pcK.sb_leq,val_ai,pcK.char_type.max);
      IF zz_tmp.is_zero() THEN
        err(err_too_big_char); l:=0;
      ELSE
        l:=val_ai.get_integer();
      END;
    END;
    IF l=0 THEN
      string[0]:=0X;
      type_string.len:=1;
    ELSE
      string[0]:=CHR(l);
      string[1]:=0X;
      type_string.len:=2;
    END;
    val:=pcK.value.new(txtpos,type_string);
    val.set_string(string);
    RETURN val_string;
  END;
  bf[i]:=0X;
  val:=pcK.value.new(txtpos,pcK.ZZ_type);
  IF digit_or_letter[ORD(char)] OR (dig_a>0) THEN err(err_invalid_number);
  ELSE val.str_to_value(bf,pcK.flag_o2);
  END;
  RETURN val_integer;
END Number;

PROCEDURE QStr(q: CHAR): CHAR;
  VAR i: INTEGER;
BEGIN
  i:=0;
  getchar;
  WHILE (char#EOL) & (char#q) & (i<LEN(string)-1) DO
    string[i]:=char;
    getchar;
    INC(i);
  END;
  string[i]:=0C;
  IF char#q THEN
    err(err_too_long_string);
    WHILE (char#EOL) & (char#q) DO getchar() END;
  END;
  getchar;
  type_string.len:=i+1;
  val:=pcK.value.new(txtpos,type_string);
  val.set_string(string);
  val_hex:=FALSE;
  RETURN val_string;
END QStr;

(*----------------------------------------------------------------*)

PROCEDURE identifier(VAR sy: Symbol);
  VAR i: INTEGER;
BEGIN
  i:=0;
  REPEAT
    name[i]:=char;
    getchar;
    INC(i);
  UNTIL (i>=pcK.max_name) OR ~ digit_or_letter[ORD(char)];
  name[i]:=0C; len:=i;
  IF i=pcK.max_name THEN
    err(err_too_long_ident);
    REPEAT getchar() UNTIL ~ digit_or_letter[ORD(char)];
  END;
  sy:=ident;
END identifier;

PROCEDURE get(VAR sy: Symbol);
BEGIN
  prevpos := txtpos;
  LOOP
    WHILE char<=' ' DO
      IF char=EOF THEN fault(err_end_of_file) END;
      getchar;
    END;
    (*
       здесь нет лишних вызовов pack поскольку почти все альтернативы
       оператора CASE закачиваются возвратом из процедуры
    *)
    txtpos.pack(src_file.name^,src_line,src_col);
    CASE char OF
      |'0'..'9': sy:=Number();     RETURN
      |'"',"'" : sy:=QStr(char);   RETURN
      |'+': getchar; sy:=plus;   RETURN
      |'*': getchar;
            IF char='*' THEN
              getchar;
              IF ~ oberon & ~ ext() OR
                 oberon & ~ o2_num_ext
              THEN err(err_extension_not_allowed,"(exponentiation operator)");
              END;
              sy:=exponent;
            ELSE
              sy:=times;
            END;
            RETURN;
      |'[': getchar; sy:=lbr;    RETURN
      |']': getchar; sy:=rbr;    RETURN
      |'{': getchar; sy:=lbrace; RETURN
      |'}': getchar; sy:=rbrace; RETURN
      |'/': getchar;
            IF char='/' THEN
              getchar;
              IF ~ env.config.Option(CPPCOMMENTS) THEN err(err_extension_not_allowed,", use CPPCOMMENTS option ") END;
              Comment(comments,TRUE);
            ELSIF char='*' THEN
              getchar;
              IF ~ env.config.Option(CPPCOMMENTS) THEN err(err_extension_not_allowed,", use CPPCOMMENTS option ") END;
              Comment(xcomments & (char='*') OR comments,FALSE,'/');
            ELSE
              sy:=slash;  RETURN
            END;
      |'^','@': getchar; sy:=bar;RETURN
      |'&': getchar; sy:=and;    RETURN
      |'|': getchar; sy:=sep;    RETURN
      |';': getchar; sy:=semic;  RETURN
      |',': getchar; sy:=comma;  RETURN
      |'=': getchar; sy:=equ;    RETURN
      |'#': getchar; sy:=neq;    RETURN
      |')': getchar; sy:=rpar;   RETURN
      |'~': getchar; sy:=not;    RETURN
      |'-': getchar;
            IF char='-' THEN
              getchar;
              IF env.config.Option(CPPCOMMENTS) THEN
                err(err_extension_not_allowed,", use // instead of --");
              END;
              IF ~ ext() THEN err(err_extension_not_allowed,"  ") END;
              Comment(comments,TRUE);
            ELSE sy:=minus; RETURN
            END
      |':': getchar;
            sy :=colon;
            CASE char OF
            | '=':
              getchar; sy:=becomes
            | ')':
              getchar; sy:=rbrace
            | ':':
              IF TS_ext() THEN
                getchar;
                IF char = '=' THEN
                  getchar; sy := alias;
                ELSE
                  sy := none;
                END;
              END;
            ELSE
            END;
            RETURN
      |'.': getchar;
            IF char='.' THEN getchar; sy:=range ELSE sy:=period END;
            RETURN
      |'!': getchar;
            IF char=')' THEN getchar; sy:=rbr ELSE sy:=sep END;
            RETURN
      |_range: getchar; sy:=range; RETURN
      |'>': getchar;
            sy:=gtr;
            CASE char OF
            | '=':
              getchar; sy:=geq
            | '>':
              IF ext() OR TS_ext() THEN
                getchar; sy := shift_r;
              END;
            ELSE
            END;
            RETURN
      |'<': getchar;
            sy:=lss;
            CASE  char OF
            | '=':
              getchar; sy:=leq; RETURN
            | '>':
              getchar; sy:=neq; RETURN
            | '<':
              IF ext() OR TS_ext() THEN
                getchar; sy := shift_l; RETURN;
              END;
            | '*':
              IF ~ iso_pragma THEN
                err(err_extension_not_allowed,"(ISO pragma syntax)")
              END;
              Control;
              IF ~scan THEN
                txtpos.unpack(skip_fnm,skip_ln,skip_ps);
                Skip;
              END;
            ELSE
              RETURN
            END;
      |'(': getchar;
            IF    char='*' THEN
              getchar;
              IF env.config.Option(CPPCOMMENTS) THEN
                err(err_extension_not_allowed,", use /* instead of *)");
              END;
              IF TS_ext() & (char = '%') THEN
                TS_Control;
                IF ~scan THEN
                  txtpos.unpack(skip_fnm,skip_ln,skip_ps);
                  Skip;
                END;
            <* IF MCS THEN *>
              ELSIF (char = '$') THEN
                Control;
                IF ~scan THEN
                  txtpos.unpack(skip_fnm,skip_ln,skip_ps);
                  Skip;
                END;
            <* END *>
              ELSE
                Comment(xcomments & (char='*') OR comments,FALSE);
              END;
            ELSIF char='!' THEN getchar; sy:=lbr; RETURN
            ELSIF char=':' THEN getchar; sy:=lbrace; RETURN
            ELSE sy:=lpar; RETURN
            END;
      |'_','a'..'z': identifier(sy); RETURN
      |'A': identifier(sy);
            IF (name="AND") THEN
              IF ~ oberon OR ext() THEN sy:=and ELSE sy:=ident END;
            ELSIF (name="ASM") & ext() THEN sy:=asm
            ELSIF name="ARRAY" THEN sy:=array
            END; RETURN
      |'B': identifier(sy);
            IF    name="BEGIN" THEN sy:=begin
            ELSIF name="BY" THEN sy:=by
            END; RETURN
      |'C': identifier(sy);
            IF    name="CONST" THEN sy:=const
            ELSIF name="CASE" THEN sy:=case
            END; RETURN
      |'D': identifier(sy);
            IF    name="DO" THEN sy:=do
            ELSIF name="DIV" THEN sy:=div
            ELSIF name="DEFINITION" THEN
              IF ~ oberon THEN sy:=definition END;
            END; RETURN
      |'E': identifier(sy);
            IF    name="END" THEN sy:=end
            ELSIF name="ELSE" THEN sy:=else
            ELSIF name="ELSIF" THEN sy:=elsif
            ELSIF name="EXIT" THEN sy:=exit
            ELSIF (~oberon OR (env.o2_kwd IN env.config.tags)) & (name="EXCEPT") THEN
              sy:=except
            ELSIF ~ oberon & (name="EXPORT") THEN sy:=export
            END; RETURN
      |'F': identifier(sy);
            IF    name="FOR" THEN sy:=for
            ELSIF (~oberon OR (env.o2_kwd IN env.config.tags)) & (name="FINALLY") THEN
              sy:=finally
            ELSIF ~oberon & (name="FORWARD") THEN
              sy:=forward
            ELSIF (~oberon OR (env.o2_kwd IN env.config.tags)) & (name="FROM") THEN
              sy:=from
            END; RETURN
      |'G': identifier(sy);
            IF  ~oberon & add_goto & (name="GOTO") THEN sy:=goto END; RETURN;
      |'H': identifier(sy); RETURN
      |'I': identifier(sy);
            IF    name="IF" THEN sy:=if
            ELSIF name="IN" THEN sy:=in
            ELSIF oberon & (name="IS") THEN sy:=is
            ELSIF name="IMPORT" THEN sy:=import
            ELSIF ~ oberon & (name="IMPLEMENTATION") THEN
              sy:=implementation;
          <* IF MCS THEN *>
            ELSIF name="INLINE" THEN sy:= inline
          <* END *>
            END; RETURN
      |'J'..'K': identifier(sy); RETURN
      |'L': identifier(sy);
            IF    name = "LOOP"  THEN sy := loop;
            ELSIF ~oberon & add_goto & (name = "LABEL") THEN sy := label;
            END; RETURN
      |'M': identifier(sy);
            IF    name="MOD" THEN sy:=mod
            ELSIF name="MODULE" THEN sy:=module;
            END; RETURN
      |'N': identifier(sy);
            IF name="NOT" THEN
              IF ~ oberon OR ext() THEN sy:=not ELSE sy:=ident END;
            END; RETURN
      |'O': identifier(sy);
            IF    name="OR" THEN sy:=or
            ELSIF name="OF" THEN sy:=of
            END; RETURN
      |'P': identifier(sy);
            IF    name="PROCEDURE" THEN sy:=procedure
            ELSIF name="POINTER" THEN sy:=pointer
            ELSIF (~ oberon OR ext()) & (name="PACKEDSET") THEN sy:=packedset
            END; RETURN
      |'Q': identifier(sy);
            IF ~ oberon & (name="QUALIFIED") THEN sy:=qualified END;
            RETURN
      |'R': identifier(sy);
            IF    name="RETURN" THEN sy:=return
            ELSIF name="REPEAT" THEN sy:=repeat
            ELSIF name="RECORD" THEN sy:=record
            ELSIF ~ oberon & (name="REM") THEN sy:=rem
            ELSIF (~oberon OR (env.o2_kwd IN env.config.tags)) & (name="RETRY") THEN
              sy:=retry
            END; RETURN
      |'S': identifier(sy);
            IF    name="SET" THEN
              IF ~ oberon THEN sy:=set END;
            ELSIF ext() & (name="SEQ") THEN sy:=seq
          <* IF MCS THEN *>
            ELSIF (name="SETREG") THEN sy:= setreg;
          <* END *>
            END; RETURN
      |'T': identifier(sy);
            IF    name="THEN" THEN sy:=then
            ELSIF name="TO" THEN sy:=to
            ELSIF name="TYPE" THEN sy:=type
            END; RETURN
      |'U': identifier(sy);
            IF    name="UNTIL" THEN sy:=until END; RETURN
      |'V': identifier(sy);
            IF    name="VAR" THEN sy:=var END; RETURN
      |'W': identifier(sy);
            IF    name="WHILE" THEN sy:=while
            ELSIF name="WITH" THEN sy:=with
            END; RETURN
      |'X'..'Z': identifier(sy); RETURN
    ELSE
      err(err_illegal_char);
      getchar;
    END;
  END;
END get;

(*----------------------------------------------------------------*)

(*
PROCEDURE app_str(VAR d: ARRAY OF CHAR; s-: ARRAY OF CHAR);
  VAR i,j: INTEGER;
BEGIN
  i:=0;
  WHILE (i<LEN(d)-1) & (d[i]#0X) DO INC(i) END;
  j:=0;
  WHILE (i<LEN(d)-1) & (j<LEN(s)) & (s[j]#0X) DO
    d[i]:=s[j]; INC(i); INC(j);
  END;
  d[j]:=0X;
END app_str;
*)

PROCEDURE error*(pos: pcK.TPOS; type: CHAR; no: INTEGER; SEQ x: pcK.BYTE);
  VAR s: env.MESSAGE;
BEGIN
  ASSERT(env.errors#NIL);
  env.errors.GetMsg(no,s);
  env.errors.PrintMsg(pos,type,s,x);
END error;

PROCEDURE expc(s: Symbol);
  VAR v: ARRAY 16 OF CHAR;
BEGIN
  CASE s OF
    |sep    : v:=' "|"'
    |semic  : v:=' ";"'
    |colon  : v:=' ":"'
    |period : v:=' "."'
    |lbr    : v:=' "["'
    |rbr    : v:=' "]"'
    |lpar   : v:=' "("'
    |rpar   : v:=' ")"'
    |lbrace : v:=' "{"'
    |rbrace : v:=' "}"'
    |comma  : v:=' ","'
    |equ    : v:=' "="'
    |becomes: v:=' ":="'
    |range  : v:=' ".."'
    |do     : v:=' "DO"'
    |end    : v:=' "END"'
    |of     : v:=' "OF"'
    |then   : v:=' "THEN"'
    |to     : v:=' "TO"'
    |until  : v:=' "UNTIL"'
    |import : v:=' "IMPORT"'
    |module : v:=' "MODULE"'
    |ident  : err(err_ident_expected); RETURN;
  ELSE        v:=' "****"'
  END;
  error(txtpos,'e',8,v);
END expc;

PROCEDURE check_cc_level*;
BEGIN
  IF level#NIL THEN err(err_invalid_cc_option) END;
  level:=NIL;
  env.info.lines:=src_line+1;
END check_cc_level;

(*----------------------------------------------------------------*)

PROCEDURE ini*(source: xfs.TextFile;
               en_oberon: BOOLEAN; VAR sy: Symbol);

  VAR i: INTEGER; c: CHAR;
BEGIN

  src_file:=source;
  src_bpos:=0;
  src_buf[0]:=0X;
  src_line:=0;
  src_col:=-1;
  src_pos:=0;
  char:=0X;
  IF src_file#NIL THEN getchar END;

  oberon:=en_oberon;
  (* эти опции нужно опрашивать дважды,
     они используются и изменяются при вызове get(sy)
  *)
  scan:=TRUE; (* is used in option()! *)
  iso_pragma:=option("O2ISOPRAGMA") OR ~ oberon;
  xcomments :=option("XCOMMENTS");
  comments :=option("COMMENT");
  o2_num_ext:=FALSE;

  val:=NIL;
  comment:=-1;
  pcK.comments:=NIL;
  txtpos  := env.null_pos;
  prevpos := env.null_pos;
  IF src_file#NIL THEN txtpos.pack(src_file.name^,0,0) END;
  skip_ln:=-1; skip_ps:=-1;
  level:=NIL;
  env.config.Save;
  state_level:=env.config.Level();

  val_ai:=pcK.value.new(txtpos,pcK.ZZ_type);
  zz_tmp:=pcK.value.new(txtpos,pcK.ZZ_type);

  FOR i:=0 TO LEN(digit_or_letter)-1 DO
    c:=CHR(i);
    digit_or_letter[i]:=(c>='0') & (c<='9') OR
                        (c>='A') & (c<='Z') OR
                        (c>='a') & (c<='z') OR ((c='$')AND env.config.Option("buckident")) OR
                        (c='_');
  END;
  IF src_file#NIL THEN get(sy) END;
  iso_pragma := option("O2ISOPRAGMA") OR ~ oberon;
  xcomments  := option("XCOMMENTS");
  comments   := option("COMMENT");
  o2_num_ext := option("O2NUMEXT");

  add_goto   := TS_ext() OR env.config.Option("M2GOTO");
  enh_deref  := TS_ext() OR M2_ext();
END ini;

PROCEDURE exi*;
BEGIN
  WHILE env.config.Level() >= state_level DO env.config.Restore END;
  level:=NIL;
  val:=NIL;
  pcK.comments:=NIL;
  env.info.lines:=src_line+1;
  skip_fnm:=NIL;
END exi;

(*----------------------------------------------------------------*)

PROCEDURE DeclareOptions*;

  PROCEDURE Set(val: BOOLEAN; name-: ARRAY OF CHAR; bit: env.CompilerOption);
  BEGIN
    env.config.NewOption(name,val,bit);
--    ASSERT(env.config.res=env.ok,env.config.res);
  END Set;

BEGIN
  env.config.NewOption(CPPCOMMENTS, FALSE, SYSTEM.VAL(env.CompilerOption,-1));

  Set(TRUE,"ASSERT",env.assert_check);      (* enable ASSERT checks         *)
  Set(TRUE,"CHECKDINDEX",env.dynamic_check);(* enable dynarr index checks   *)
  Set(TRUE,"CHECKDIV",env.quo_check);       (* DIV,MOD - positive divisor checks *)
  Set(TRUE,"CHECKINDEX",env.index_check);   (* enable array index checks    *)
  Set(TRUE,"CHECKNIL",env.nil_check);       (* enable NIL pointer checks    *)
  Set(TRUE,"CHECKPROC",env.proc_check);     (* enable NIL procedure checks  *)
  Set(TRUE,"CHECKRANGE",env.range_check);   (* enable range checks          *)
  Set(TRUE,"CHECKSET",env.set_check);       (* enable set renge checks      *)
  Set(TRUE,"CHECKTYPE",env.guard_check);    (* enable dynamic type checks   *)
  Set(TRUE,"COVERFLOW",env.card_ovfl);      (* enable cardinal overflow checks *)
  Set(TRUE,"IOVERFLOW",env.int_ovfl);       (* enable integer overflow checks *)
  Set(TRUE,"FOVERFLOW",env.float_ovfl);     (* enable float overflow checks *)

  Set(FALSE,"STORAGE",SYSTEM.VAL(env.CompilerOption,-1));              (* enable default ALLOCATE & DEAL. *)
  Set(FALSE,"M2ADDTYPES",SYSTEM.VAL(env.CompilerOption,-1));           (* add SHORT and LONG types     *)
  Set(FALSE,"M2BASE16",SYSTEM.VAL(env.CompilerOption,-1));             (* use 16-bits INTEGER,CARDINAL,BITSET *)
  Set(FALSE,"M2CMPSYM",SYSTEM.VAL(env.CompilerOption,-1));             (* compare symbol files in Modula-2 *)
  Set(FALSE,"M2EXTENSIONS",env.m2_ext);     (* enable Modula-2 extensions   *)
  Set(FALSE,"O2EXTENSIONS",env.o2_ext);     (* enable Oberon-2 extensions   *)
  Set(FALSE,"O2ISOPRAGMA",SYSTEM.VAL(env.CompilerOption,-1));          (* enable ISO Modula-2 pragmas in Oberon *)
  Set(FALSE,"O2NUMEXT",SYSTEM.VAL(env.CompilerOption,-1));             (* enable Oberon-2 scientific extensions *)
  Set(FALSE,"O2ADDKWD",env.o2_kwd);         (* enable EXEPT, FINALLY, RETRY *)

<* IF DEFINED(TS_COMPATIBLE) AND TS_COMPATIBLE THEN *>
  Set(FALSE,"TOPSPEED",env.ts_ext);     (* enable TopSpeed extensions   *)
<* END *>


  Set(FALSE,"MAKEDEF",SYSTEM.VAL(env.CompilerOption,-1));              (* enable browser               *)
  Set(FALSE,"BSCLOSURE",SYSTEM.VAL(env.CompilerOption,-1));            (* browse: include all visible methods *)
  Set(FALSE,"BSREDEFINE",SYSTEM.VAL(env.CompilerOption,-1));           (* browse: include all redefined methods *)
  Set(FALSE,"BSALPHA",SYSTEM.VAL(env.CompilerOption,-1));              (* browse: sort by name         *)

  Set(FALSE,"CHANGESYM",SYSTEM.VAL(env.CompilerOption,-1));            (* permission to change symbol file *)
  Set(FALSE,"MAIN",SYSTEM.VAL(env.CompilerOption,-1));                 (* Oberon-2 main module         *)
  Set(FALSE,"XCOMMENTS",SYSTEM.VAL(env.CompilerOption,-1));            (* preserve exported comments   *)
  Set(FALSE,"TESTCOVERAGE",SYSTEM.VAL(env.CompilerOption,-1));         (* instrument code to collect test coverage information  *)
END DeclareOptions;

BEGIN
  WasControl := FALSE;
  txtpos  := env.null_pos;
  prevpos := env.null_pos;
  ASSERT(EOL#0C);
  NEW(type_string); type_string.mode:=pcK.ty_SS;
  NEW(type_string.base); type_string.base.mode:=pcK.ty_char;
  type_string.len:=0;
END pcS.

