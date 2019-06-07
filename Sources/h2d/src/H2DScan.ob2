MODULE H2DScan;
(*------------------------------------------------------------------------*)
IMPORT sys:= SYSTEM,
       objs:= H2DObjs,
       adt,
       file:= H2DFile,
       msg:= H2DMsg,
       cfg:= H2DCfg,
       lstr:= LongStrs,
       WholeStr,
       LongConv,
       io:= Printf;

(*  Synonyms  *)
TYPE
  INT = sys.INT32;
  CARD = sys.CARD32;


CONST
  MAX_INT  = MAX(INT);
  MAX_CARD = MAX(CARD);

  line_len = 200;
  name_len =  200;


(*------------------------------------------------------------------------
       Exported types.
------------------------------------------------------------------------*)
TYPE
  Token * = POINTER TO TokenDesc;
  TokenDesc * = RECORD (adt.ElementDesc)
    token * : INT;
    text * : lstr.String;
    original_text * : lstr.String;
    parsed * : BOOLEAN;
    boundary * : BOOLEAN;
    line * , pos * : INT;
    file * : lstr.String;
    _line , _pos: INT;
    pretoken * : BOOLEAN;
  END;



(*------------------------------------------------------------------------
        Nonexported types, constants and global variables.
------------------------------------------------------------------------*)
CONST
  (*  Blanks  *)
  SPACE = 20X;
  HTAB  = 0BX;
  GTAB  = 09X;
  FF    = 0CX;

  EOF	= file.EOF;
  EOL	= file.EOS;


TYPE
  File = POINTER TO FileDesc;
  FileDesc = RECORD (adt.NamedElementDesc)
    was_blank: BOOLEAN;
    replacements:adt.Stack;
    replacement: objs.Replacement;
    pd_parsed  : BOOLEAN;
    pd_define_parsed: BOOLEAN;
    pd_include_parsed: BOOLEAN;
    pd_header_parsed: BOOLEAN;
    transparent_numeration: BOOLEAN;
    handler: file.FILE;
    scanstr: lstr.String;
    line: INT;
    pos: INT;
    total_lines: INT;
    comments: BOOLEAN;
    line_of_last_token_begin: INT;
    pos_of_last_token_begin: INT;
  END;

  KeyWord = POINTER TO KeyWordDesc;
  KeyWordDesc = RECORD (adt.NamedElementDesc)
    code: INT;
    ext : BOOLEAN;
  END;


VAR
  read_file_exists: BOOLEAN;	    (*	Does exist file for reading ?	    *)
  keywords: adt.Tree;		    (*	Set of key words		    *)
  preprocessor_directives: adt.Tree;(*	Set of preprocessor directives	    *)
  handler: file.FILE;               (*  Handler of current scanning file    *)
  file_name: lstr.String;           (*  Name of current scanning file       *)
  files_stack: adt.Stack;	    (*	Stack of scanning files 	    *)
  replacements:adt.Stack;	    (*	Stack of replaced identifiers	    *)
  replacement: objs.Replacement;    (*	Current list of replacements	    *)
  replace_on:  BOOLEAN;             (*  Repalcing ON/OFF                    *)
  scanstr: lstr.String; 	    (*	Scanned string			    *)
  pd_parsed  : BOOLEAN; 	    (*	preprocessor directive is parsed    *)
  pd_define_parsed: BOOLEAN;	    (*	directive 'define' is parsed        *)
  pd_include_parsed: BOOLEAN;       (*  directive 'include' is parsed       *)
  pd_header_parsed: BOOLEAN;        (*  directive 'header' is parsed       *)

  non_presentable_char_array: ARRAY 256 OF CHAR;

  was_blank: BOOLEAN;
  total_lines: INT;
  line: INT;
  pos: INT;
  line_of_last_token_begin: INT;
  pos_of_last_token_begin: INT;

  tokens: adt.List;
  skipp_eol: BOOLEAN;
  was_eof: BOOLEAN;
  bad_const: BOOLEAN;

  logicalStr:     lstr.String;   (* part of current logical string
                                    that has already been scanned *)
  prevLogicalStr: lstr.String;   (* previous logical string (whole) *)

(*------------------------------------------------------------------------
       Exported  global variables and procedures.
------------------------------------------------------------------------*)
VAR
  token       - : INT;
  text	      - : lstr.String;
  last_header_total_lines - : INT;
  original_text - : lstr.String;
  skipping_mode * : BOOLEAN;
  extensions	* : BOOLEAN;
  transparent_numeration * : BOOLEAN;
  comments	* : BOOLEAN;  (*  commemts are parsed (yes/no)	      *)
  prj_cfg_on    * : BOOLEAN;

PROCEDURE ^ Open * (name-: ARRAY OF CHAR): BOOLEAN;
PROCEDURE ^ Close * ();
PROCEDURE ^ GetToken * ();
PROCEDURE ^ NewToken * (VAR tok: Token);

(*------------------------------------------------------------------------
     Nonexported  procedures
------------------------------------------------------------------------*)

PROCEDURE ^ inc();


(*------------------------------------------------------------------------*)
PROCEDURE Error(number: INT; SEQ x: sys.BYTE);
BEGIN
  objs.CurrentHeader.stat.Error(number, file_name^, line, pos+1, x);
  Close();
END Error;

(*------------------------------------------------------------------------*)
PROCEDURE Warning(number: INT; SEQ x: sys.BYTE);
BEGIN
  objs.CurrentHeader.stat.Warning(number, file_name^, line, pos+1, x);
END Warning;

(*------------------------------------------------------------------------*)
PROCEDURE RdPartOfScanStr();
BEGIN
  LOOP
    file.RdStr(handler, scanstr);
    IF scanstr = NIL THEN
      lstr.Assign('', scanstr);
      lstr.AppendChar(EOF, scanstr);
      EXIT;
    ELSIF ~((scanstr[0] = '\') & (scanstr[1] = EOL)) THEN
      INC(total_lines);
      INC(line);
      EXIT;
    END;
  END;
  pos:= 0;
END RdPartOfScanStr;

(*------------------------------------------------------------------------*)
PROCEDURE int_to_str(int: INT; VAR str: lstr.String);
VAR
  digit: ARRAY 2 OF CHAR;
  _digit: INT;
  min, neg: BOOLEAN;
BEGIN
  neg:= int < 0;
  min:= int = MIN(INT);
  IF min THEN int:= int + 1 END;
  IF neg THEN int:= -int END;
  digit[1]:= EOL;
  REPEAT
    _digit:= int MOD 10;
    IF min THEN INC(_digit); min:= FALSE END;
    int:= int DIV 10 + _digit DIV 10;
    digit[0]:= CHR(_digit MOD 10 + ORD('0'));
    lstr.Insert(digit, 0, str);
  UNTIL int = 0;
  IF neg THEN
    lstr.Insert('-', 0, str);
  END;
END int_to_str;

PROCEDURE uint_to_str(uint: CARD; VAR str: lstr.String);
VAR
  digit: ARRAY 2 OF CHAR;
BEGIN
  digit[1]:= EOL;
  REPEAT
    digit[0]:= CHR(uint MOD 10 + ORD('0'));
    lstr.Insert(digit, 0, str);
    uint:= uint DIV 10;
  UNTIL uint = 0;
END uint_to_str;

PROCEDURE check_number(VAR str: lstr.String);
  VAR
    i, degree, result: INT;
    unsigned_degree, unsigned_result: CARD;
    unsigned: BOOLEAN;
    old_unsigned: BOOLEAN;
    was_point, was_e, was_x: BOOLEAN;
    conv_result: LongConv.ConvResults;
    type: INT;

  PROCEDURE OutOfRange();
  BEGIN
    bad_const:= TRUE;
    token:= type;
    IF type = objs.real_const THEN
      lstr.Assign('153.0', str);
    ELSE
      lstr.Assign('153', str);
    END;
    IF ~skipping_mode THEN
      Warning(0, '');
    END;
  END OutOfRange;

  PROCEDURE WrongFormat();
  BEGIN
    bad_const:= TRUE;
    token:= type;
    IF type = objs.real_const THEN
      lstr.Assign('153.0', str);
    ELSE
      lstr.Assign('153', str);
    END;
    IF ~skipping_mode THEN
      Warning(1, '');
    END;
  END WrongFormat;



  PROCEDURE signed_compute(digit, base: INT): BOOLEAN;
  BEGIN
    IF degree = 0 THEN
      degree:= 1;
    ELSIF (MAX_INT DIV base) < degree THEN
      OutOfRange();
      RETURN TRUE;
    ELSE
      degree:= degree*base;
    END;

    IF digit = 0 THEN RETURN FALSE END;
    IF ((MAX_INT DIV digit) < degree) OR
       ((MAX_INT - degree*digit) < result)
    THEN
      OutOfRange();
      RETURN TRUE;
    ELSE
      result:= result + degree*digit;
      RETURN FALSE;
    END;
  END signed_compute;

  PROCEDURE unsigned_compute(digit, base: CARD): BOOLEAN;
  BEGIN
    IF unsigned_degree = 0 THEN
      unsigned_degree:= 1;
    ELSIF (MAX_CARD DIV base) < unsigned_degree THEN
      OutOfRange();
      RETURN TRUE;
    ELSE
      unsigned_degree:= unsigned_degree*base;
    END;

    IF digit = 0 THEN RETURN FALSE END;
    IF ((MAX_CARD DIV digit) < unsigned_degree) OR
       ((MAX_CARD -  unsigned_degree*digit) <  unsigned_result)
    THEN
      OutOfRange();
      RETURN TRUE;
    ELSE
      unsigned_result:= unsigned_result + unsigned_degree*digit;
      RETURN FALSE;
    END;
  END unsigned_compute;

  PROCEDURE compute(digit, base: INT): BOOLEAN;
  BEGIN
    IF unsigned THEN
      RETURN unsigned_compute(sys.VAL(CARD, digit), sys.VAL(CARD, base));
    ELSE
      RETURN signed_compute(digit, base);
    END;
  END compute;

BEGIN
  i:= 0;
  was_point:= FALSE;
  was_e:= FALSE;
  was_x:= FALSE;
  token:= objs.undefined;
  bad_const:= FALSE;

  lstr.Capitalize(str^);

  WHILE str[i] # EOL DO
    IF (str[i] = '.') & (str[i+1] >= '0') & (str[i+1] <= '9') THEN
      was_point:= TRUE;
    END;
    was_e:= was_e OR (str[i] = 'E');
    was_x:= was_x OR (str[i] = 'X');
    INC(i);
  END;

  IF was_point OR (~was_x & was_e) THEN
    IF str[0] = '.' THEN
      lstr.Insert('0', 0, str);
    END;

    i:= lstr.Length(str^)-1;
    IF str[i] = 'F' THEN
      str[i]:= EOL;
    END;

    conv_result:= LongConv.FormatReal(str^);
    type:= objs.real_const;
    token:= objs.real_const;
    IF conv_result = LongConv.strAllRight THEN
      RETURN;
    ELSIF conv_result = LongConv.strOutOfRange THEN
      OutOfRange();
      RETURN;
    ELSIF conv_result = LongConv.strWrongFormat THEN
      WrongFormat();
      RETURN;
    END;

  ELSE
    result:= 0;
    degree:= 0;
    unsigned_result:= 0;
    unsigned_degree:= 0;
    i:= lstr.Length(str^)-1;

    IF str[i] = 'L' THEN
      str[i]:= EOL;
      DEC(i);
    END;

    unsigned:= str[i] = 'U';
    IF unsigned THEN
      type:= objs.uint_const;
      str[i]:= EOL;
      DEC(i);
    ELSE
      type:= objs.int_const;
    END;

    IF str[0] = '0' THEN
      IF str[1] = 'X' THEN
	IF i <= 1 THEN
	  (* error *)
          WrongFormat();
	  RETURN;
	END;
	(* hex constant *)
	IF unsigned THEN
	  type:= objs.uhex_const;
	ELSE
	  type:= objs.hex_const;
	END;
	old_unsigned:= unsigned;
	unsigned:= TRUE;
	WHILE i > 1 DO
	  IF (str[i] >= '0') & (str[i] <= '9') THEN
	    IF compute(ORD(str[i]) - ORD('0'), 16) THEN
	      RETURN;
	    END;
	  ELSIF (str[i] >= 'A') & (str[i] <= 'F') THEN
	    IF compute(ORD(str[i]) - ORD('A') + 0AH, 16) THEN
	      RETURN;
	    END;
	  ELSE;
	    (* error *)
            WrongFormat();
	    RETURN;
	  END;
	  DEC(i);
	END;
	unsigned:= old_unsigned;
	IF ~unsigned THEN
	  result:= sys.VAL(INT, unsigned_result);
	END;
      ELSE
	(* oct constant *)
	WHILE i > 0 DO
	  IF (str[i] >= '0') & (str[i] <= '7') THEN
	    IF compute(ORD(str[i]) - ORD('0'), 8) THEN
	      RETURN;
	    END;
	  ELSE;
	    (* error *)
            WrongFormat();
	    RETURN;
	  END;
	  DEC(i);
	END;
      END;
    ELSE
      (* dec constant *)
      WHILE i > -1 DO
	IF (str[i] >= '0') & (str[i] <= '9') THEN
	  IF compute(ORD(str[i]) - ORD('0'), 10) THEN
	    RETURN;
	  END;
	ELSE;
	  (* error *)
          WrongFormat();
	  RETURN;
	END;
	DEC(i);
      END;
    END;
    lstr.Assign('',str);
    IF unsigned THEN
      uint_to_str(unsigned_result, str);
    ELSE
      int_to_str(result, str);
    END;
    token:= type;
  END;
END check_number;


(*------------------------------------------------------------------------*)
PROCEDURE non_presentable_char(VAR char: CHAR);
VAR
  digit_num: INT;
  number: lstr.String;
  res: WholeStr.ConvResults;
  code: INT;

  PROCEDURE ishexdigit(c: CHAR): BOOLEAN;
  BEGIN
    RETURN (c >= '0') & (c <= '9')
		     OR
	   (c >= 'A') & (c <= 'F')
		     OR
	   (c >= 'a') & (c <= 'f');
  END ishexdigit;

  PROCEDURE isoctdigit(c: CHAR): BOOLEAN;
  BEGIN
    RETURN (c >= '0') & (c <= '7');
  END isoctdigit;

BEGIN
  inc();
  digit_num:= 0;
  IF scanstr[pos] = 'x' THEN
    inc();
    lstr.Assign('0x', number);
    WHILE (digit_num < 2) & ishexdigit(scanstr[pos]) DO
      lstr.AppendChar(scanstr[pos], number);
      inc(); INC(digit_num);
    END;
    check_number(number);
    WholeStr.StrToInt(number^, code, res);
    char:= CHR(code);
  ELSIF isoctdigit(scanstr[pos]) THEN
    lstr.Assign('0', number);
    REPEAT
      lstr.AppendChar(scanstr[pos], number);
      inc(); INC(digit_num);
    UNTIL (digit_num >= 3) OR ~isoctdigit(scanstr[pos]);
    check_number(number);
    WholeStr.StrToInt(number^, code, res);
    IF code > 255 THEN
      (* warning *)
      Warning(0, '');
      code:= 153;
    END;
    char:= CHR(code);
  ELSE
    char:= non_presentable_char_array[ORD(scanstr[pos])];
    inc();
  END;
END non_presentable_char;

(*------------------------------------------------------------------------*)
(*+*)PROCEDURE splitter(c: CHAR): BOOLEAN;
BEGIN
  RETURN  (c # '_') &
	  ((c < 'A') OR (c > 'Z')) &
	  ((c < 'a') OR (c > 'z')) &
	  ((c < '0') OR (c > '9'));
END splitter;


(*------------------------------------------------------------------------*)
(*+*)PROCEDURE nextis(c: CHAR): BOOLEAN;
BEGIN
  RETURN scanstr[pos+1] = c;
END nextis;

(*------------------------------------------------------------------------*)
(*+*)PROCEDURE is(p: INT; c: CHAR): BOOLEAN;
BEGIN
  RETURN scanstr[p] = c;
END is;

(*------------------------------------------------------------------------*)
PROCEDURE inc();
BEGIN
  INC(pos);
  IF skipp_eol & is(pos, '\') & nextis(EOL) THEN
    RdPartOfScanStr();
    lstr.Append(scanstr^, logicalStr);
  END;
END inc;

(*------------------------------------------------------------------------*)
(*+*)PROCEDURE skip_blanks();
BEGIN
  was_blank:= FALSE;
  LOOP
    CASE scanstr[pos] OF SPACE, HTAB, GTAB, FF:
      was_blank:= TRUE;
      inc();
    ELSE
      EXIT;
    END;
  END;
END skip_blanks;

(*------------------------------------------------------------------------*)
PROCEDURE skip_pd();
BEGIN
  IF (token = objs.pd_line) OR (token = objs.pd_error)
			    OR
		  (token = objs.pd_pragma)
  THEN
    WHILE ~is(pos, EOL) DO
      inc();
    END;
    lstr.Assign(logicalStr^, prevLogicalStr);
    RdPartOfScanStr();
    lstr.Assign(scanstr^, logicalStr);
    pd_parsed:= FALSE;
    GetToken(); IF msg.WasError THEN RETURN END;
  END;
END skip_pd;

(*------------------------------------------------------------------------*)
(*+*)PROCEDURE copy_before_splitter();
BEGIN
  REPEAT
    lstr.AppendChar(scanstr[pos], text);
    inc();
  UNTIL  splitter(scanstr[pos]);
END copy_before_splitter;



(*------------------------------------------------------------------------*)
VAR tmp_pattr_for_finding_keywords: KeyWord;
PROCEDURE find(t: adt.Tree): BOOLEAN;
VAR
  elem: adt.Element;
BEGIN
  tmp_pattr_for_finding_keywords.SetName(text^);
  t.Find(tmp_pattr_for_finding_keywords, elem);
  IF elem = NIL THEN
    RETURN FALSE;
  ELSE
    token:= elem(KeyWord).code;
    RETURN ~elem(KeyWord).ext OR extensions;
  END;
END find;

(*------------------------------------------------------------------------*)
VAR empty_token_list: adt.List;
PROCEDURE getToken(VAR t: Token): BOOLEAN;
VAR e: adt.Element;
BEGIN
  empty_token_list.FindFirst(e);
  IF e = NIL THEN
    t:= NIL;
  ELSE
    t:= e(Token);
    empty_token_list.DeleteCurrent();
  END;
  RETURN t # NIL;
END getToken;

(*------------------------------------------------------------------------*)
PROCEDURE putToken(VAR t: Token);
BEGIN
  empty_token_list.Insert(t);
  t:= NIL;
END putToken;

(*------------------------------------------------------------------------*)
PROCEDURE check_concat_tokens(replacement: objs.Replacement);
VAR
  tokens: adt.List;
  e, e1: adt.Element;
  last_token, new_token: Token;
BEGIN
  adt.NewList(tokens);
  replacement.tokens.FindFirst(e);
  last_token:= sys.VAL(Token, e);
  replacement.tokens.FindNext(e);
  WHILE e # NIL DO
    IF e(Token).token = objs.double_dies THEN
      replacement.tokens.FindNext(e);
      WITH e: Token DO
        CASE last_token.token OF
	   objs.int..objs.huge, objs.ident:
	     CASE e.token OF
               objs.int..objs.huge, objs.ident, objs.real_const, objs.int_const:
                 new_token:= NIL;
                 NewToken(new_token);
		 new_token.token:= objs.ident;
		 lstr.Assign(last_token.original_text^, new_token.text);
		 lstr.Append(e.original_text^, new_token.text);
		 lstr.Assign(new_token.text^, new_token.original_text);
		 last_token:= new_token;
	     ELSE
	       tokens.Insert(last_token);
	       last_token:= e;
	     END;
	   |objs.real_const, objs.int_const:
	     CASE e.token OF
	       objs.int..objs.huge, objs.ident, objs.real_const, objs.int_const:
		 lstr.Assign(last_token.original_text^, text);
		 lstr.Append(e.original_text^, text);
                 check_number(text);
                 last_token:= NIL;
		 NewToken(last_token);
	     ELSE
	       tokens.Insert(last_token);
	       last_token:= e;
	     END;
	  |objs.not:
	     IF e.token = objs.assign THEN
               new_token:= NIL;
	       NewToken(new_token);
	       new_token.token:= objs.not_equ;
	       last_token:= new_token;
	     ELSE
	       tokens.Insert(last_token);
	       last_token:= e;
	     END;
	  |objs.assign:
	     IF e.token = objs.assign THEN
               new_token:= NIL;
               NewToken(new_token);
	       new_token.token:= objs.equ;
	       last_token:= new_token;
	     ELSE
	       tokens.Insert(last_token);
	       last_token:= e;
	     END;
	  |objs.more:
	     IF e.token = objs.assign THEN
               new_token:= NIL;
               NewToken(new_token);
	       new_token.token:= objs.more_equ;
	       last_token:= new_token;
	     ELSIF e.token = objs.more THEN
               new_token:= NIL;
               NewToken(new_token);
	       new_token.token:= objs.b_rshift;
	       last_token:= new_token;
	     ELSE
	       tokens.Insert(last_token);
	       last_token:= e;
	     END;
	  |objs.less:
	     IF e.token = objs.assign THEN
               new_token:= NIL;
               NewToken(new_token);
	       new_token.token:= objs.less_equ;
	       last_token:= new_token;
	     ELSIF e.token = objs.less THEN
               new_token:= NIL;
               NewToken(new_token);
	       new_token.token:= objs.b_lshift;
	       last_token:= new_token;
	     ELSE
	       tokens.Insert(last_token);
	       last_token:= e;
	     END;
	  |objs.ampersand:
	     IF e.token = objs.ampersand THEN
               new_token:= NIL;
               NewToken(new_token);
	       new_token.token:= objs.l_and;
	       last_token:= new_token;
	     ELSE
	       tokens.Insert(last_token);
	       last_token:= e;
	     END;
	  |objs.b_or:
	     IF e.token = objs.b_or THEN
               new_token:= NIL;
               NewToken(new_token);
	       new_token.token:= objs.l_or;
	       last_token:= new_token;
	     ELSE
	       tokens.Insert(last_token);
	       last_token:= e;
	     END;
	ELSE
	  tokens.Insert(last_token);
	  replacement.tokens.FindNext(e1);
	  last_token:= sys.VAL(Token, e1);
	END;
      END;
    ELSIF e(Token).token = objs.dies THEN
      (* error *)
      Error(2, '#');
      RETURN;
    ELSE
      tokens.Insert(last_token);
      last_token:= sys.VAL(Token, e);
    END;
    replacement.tokens.FindNext(e);
  END;
  tokens.Insert(last_token); adt.Deallocate(replacement.tokens);
  replacement.tokens:= tokens;
END check_concat_tokens;

(*------------------------------------------------------------------------*)
PROCEDURE build_replacement_with_params(replacement: objs.Replacement): BOOLEAN;
TYPE
  T = POINTER TO TDesc;
  TDesc = RECORD (adt.NamedElementDesc)
    replacement: adt.List;
  END;
VAR
  e, e1, e2: adt.Element;
  ne: adt.NamedElement;
  params: adt.List;
  t, next_token: Token;
  p: T;
  bracket_balance: INT;
BEGIN
  pd_parsed:= TRUE;
  replace_on:= FALSE;
  t:= NIL;
  NewToken(t);
  GetToken(); IF msg.WasError THEN RETURN FALSE END;
  next_token:= NIL;
  NewToken(next_token);
  IF token = objs.open_round_brack THEN
    GetToken(); IF msg.WasError THEN RETURN FALSE END;
    putToken(next_token); putToken(t);
  ELSE
    tokens.Insert(next_token);
    token:= t.token;
    lstr.Assign(t.text^, text);
    lstr.Assign(t.original_text^, original_text);
    line:= t._line;
    pos:= t._pos;
    line_of_last_token_begin:= t.line;
    pos_of_last_token_begin:= t.pos;
    replace_on:= TRUE; pd_parsed:= FALSE;
    putToken(t);
    RETURN FALSE;
  END;
  adt.NewList(params);
  replacement.params.FindFirst(e);
  WHILE e # NIL DO
    NEW(p); p.SetName(e(adt.NamedElement).name^);
    adt.NewList(p.replacement);
    params.Insert(p);
    bracket_balance:= 0;
<*+ WOFF902 *>
    WHILE (bracket_balance > 0) OR ((token # objs.comma) & (token # objs.close_round_brack)) DO
<*- WOFF902 *>
      IF token = objs.eof THEN
	(* error *)
	Error(24, '');
	RETURN FALSE;
      ELSIF token = objs.open_round_brack THEN
        INC(bracket_balance);
      ELSIF token = objs.close_round_brack THEN
        DEC(bracket_balance);
      END;
      IF token # objs.eol THEN
        t:= NIL;
        NewToken(t);
        p.replacement.Insert(t);
      END;
      GetToken(); IF msg.WasError THEN RETURN FALSE END;
    END;
    IF p.replacement.IsEmpty() THEN
      (* error *)
      Error(3, '');
      RETURN FALSE;
    END;
    replacement.params.FindNext(e);
    IF token = objs.comma THEN
      IF e = NIL THEN
	(* error *)
	Error(4, '');
	RETURN FALSE;
      ELSE
	GetToken(); IF msg.WasError THEN RETURN FALSE END;
      END;
    END;
  END;
  adt.NewList(replacement.tokens);
  replacement.pattern.FindFirst(e);
  WHILE e # NIL DO
    WITH e: Token DO
      IF e.token = objs.ident THEN
        adt.NewNamedElement(ne, e.text^);
        params.Find(ne, e1); adt.Deallocate(ne);
	IF e1 = NIL THEN
	  replacement.tokens.Insert(e);
	ELSE
	  e1(T).replacement.FindFirst(e2);
	  WHILE e2 # NIL DO
	    replacement.tokens.Insert(e2);
	    e1(T).replacement.FindNext(e2);
	  END;
	END;
      ELSE
	replacement.tokens.Insert(e);
      END;
    END;
    replacement.pattern.FindNext(e);
  END;
  check_concat_tokens(replacement);
  replace_on:= TRUE;
  pd_parsed:= FALSE;
  params.FindFirst(e);
  WHILE e # NIL DO
    lstr.Deallocate(e(T).name); adt.Deallocate(e(T).replacement);
    params.FindNext(e);
  END; adt.Deallocate(params);
  RETURN TRUE;
END build_replacement_with_params;

(*------------------------------------------------------------------------*)
VAR tmp_pattr_for_finding_replacements: objs.Replacement;
(*+*)PROCEDURE find_replacement(): BOOLEAN;
VAR
  elem: adt.Element;
  r: objs.Replacement;
BEGIN
  tmp_pattr_for_finding_replacements.SetName(text^);
  elem:= NIL;
  IF objs.CurrentHeader # NIL THEN
--    objs.CurrentHeader.Replacements.Find(tmp_pattr_for_finding_replacements, elem);
    objs.FindInReplacements(objs.CurrentHeader, tmp_pattr_for_finding_replacements, elem);
  END;
  IF (elem # NIL) & ~elem(objs.Replacement).used THEN
    r:= elem(objs.Replacement);
    IF ~r.params.IsEmpty()
		 &
       ~build_replacement_with_params(r)
    THEN
      RETURN FALSE;
    END;
    IF msg.WasError THEN RETURN FALSE END;
    replacements.Push(replacement);
    replacement:= r;
    replacement.used:= TRUE;
    replacement.tokens.Reset();
    RETURN TRUE;
  END;
  RETURN FALSE;
END find_replacement;

(*------------------------------------------------------------------------*)
(*+*)PROCEDURE push_file();
VAR
  n: File;
BEGIN
  NEW(n); n.SetName(file_name^);
  n.was_blank:= was_blank;
  n.pd_parsed:= pd_parsed;
  n.pd_define_parsed:= pd_define_parsed;
  n.pd_include_parsed:= pd_include_parsed;
  n.pd_header_parsed:= pd_header_parsed;
  n.transparent_numeration:= transparent_numeration;
  n.replacement:= replacement;
  n.replacements:= replacements;
  n.handler:= handler;
  n.line:= line;
  n.pos:= pos;
  n.scanstr:= NIL;
  n.total_lines:= total_lines;
  n.comments:= comments;
  n.line_of_last_token_begin:= line_of_last_token_begin;
  n.pos_of_last_token_begin:= pos_of_last_token_begin;
  lstr.Assign(scanstr^, n.scanstr);
  files_stack.Push(n);
END push_file;


(*------------------------------------------------------------------------*)
(*+*)PROCEDURE pop_file();
VAR
  elem: adt.Element;
BEGIN
  files_stack.Pop(elem);
  IF elem = NIL THEN
    read_file_exists:= FALSE;
  ELSE
    WITH elem: File DO
      was_blank:= elem.was_blank;
      pd_parsed:= elem.pd_parsed;
      pd_define_parsed:= elem.pd_define_parsed;
      pd_include_parsed:= elem.pd_include_parsed;
      pd_header_parsed:= elem.pd_header_parsed;
      replacement:= elem.replacement;
      replacements:= elem.replacements;
      transparent_numeration:= elem.transparent_numeration;
      IF ~transparent_numeration THEN
	total_lines:= elem.total_lines;
      END;
      comments:= elem.comments;
      line_of_last_token_begin:= elem.line_of_last_token_begin;
      pos_of_last_token_begin:= elem.pos_of_last_token_begin;
      lstr.Assign(elem.name^, file_name);
      handler:= elem.handler;
      line:= elem.line;
      pos:= elem.pos;
      lstr.Assign(elem.scanstr^, scanstr);
    END;
    read_file_exists:= TRUE;
  END;
END pop_file;


(*------------------------------------------------------------------------*)
(*+*)PROCEDURE get_str_const(border: CHAR);
VAR
  str: lstr.String;
  char: CHAR;
  was_eol: BOOLEAN;
BEGIN
  was_eol:= FALSE;
  inc();
  WHILE ~is(pos,border) & ~is(pos,EOL) DO
    IF is(pos,'\') THEN
      non_presentable_char(char); IF msg.WasError THEN RETURN END;
      was_eol:= char = EOL;
    ELSE
      char:= scanstr[pos];
      inc();
    END;
    IF ~was_eol THEN
      lstr.AppendChar(char, text);
    END;
  END;
  IF is(pos,border) THEN
    inc();
  ELSE
    (* error *)
    str:= NIL;
    lstr.Assign('\, ', str);
    lstr.AppendChar(border, str);
    Error(5, str^);
  END;
END get_str_const;

(*------------------------------------------------------------------------*)
(*+*)PROCEDURE get_next_token_from_current_replacement_list(): BOOLEAN;
VAR
  elem: adt.Element;
BEGIN
  WHILE replacement # NIL DO
    replacement.tokens.FindNext(elem);
    IF elem = NIL THEN
      replacement.used:= FALSE;
      IF ~replacement.params.IsEmpty() THEN
        adt.Deallocate(replacement.tokens);
        replacement.tokens:= NIL;
      END;
      replacements.Pop(elem);
      replacement:= sys.VAL(objs.Replacement, elem);
    ELSE
      token:= elem(Token).token;
      lstr.Assign(elem(Token).text^, text);
      IF ~replace_on OR (token # objs.ident) OR ~find_replacement() THEN
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END get_next_token_from_current_replacement_list;

(*------------------------------------------------------------------------*)
PROCEDURE get_include_name ();
VAR
  border: ARRAY 2 OF CHAR;
BEGIN
  border[1]:= EOL;
  IF was_blank THEN
    IF is(pos,'<') THEN
      border[0]:= '>';
      token:= objs.std_header_name;
    ELSIF is(pos,'"') THEN
      border[0]:= '"';
      token:= objs.nonstd_header_name;
    ELSE
      (* error *)
      Error(5, '< , "');
      RETURN;
    END;
    inc();
    WHILE ~is(pos,border[0]) & ~is(pos,EOL) DO
      lstr.AppendChar(scanstr[pos], text);
      inc();
    END;
    IF is(pos,EOL) THEN
      (* error *)
      Error(5, border);
      RETURN;
    END;
  ELSE
    (* error *)
    Error(25, '');
    RETURN;
  END;
  IF (text = NIL) OR (text[0] = EOL) THEN
    (* error *)
    Error(26, '');
    RETURN;
  END;
  inc();
END get_include_name;


(*------------------------------------------------------------------------*)
VAR tmp_str_for_formatting_comments: lstr.String;
PROCEDURE format_comments(comments: adt.List; pos: INT);
VAR
  max_size: INT;
  e: adt.Element;
  tmp_str: lstr.String;
  min_lead_spaces_num: INT;
  tmp: INT;

  (*----------------------------------------------------*)
  PROCEDURE append_spaces(VAR str: lstr.String; len: INT);
  VAR
    i: INT;
  BEGIN
    i:= lstr.Length1(str);
    lstr.Assign('', tmp_str_for_formatting_comments);
    WHILE i < len DO
      lstr.Append(' ', tmp_str_for_formatting_comments);
      INC(i);
    END;
    lstr.Append(tmp_str_for_formatting_comments^, str);
  END append_spaces;

  (*----------------------------------------------------*)
  PROCEDURE lead_spaces_num(str-: ARRAY OF CHAR): INT;
  VAR
    i: INT;
  BEGIN
    i:= 0;
    WHILE str[i] = SPACE DO
      INC(i);
    END;
    RETURN i;
  END lead_spaces_num;

  (*----------------------------------------------------*)
  (*----------------------------------------------------*)


BEGIN
  max_size:= 0;
  tmp_str:= NIL;
  comments.FindFirst(e);
  append_spaces(tmp_str, pos+2);
  lstr.Append(e(Token).text^, tmp_str);
  e(Token).text:= tmp_str;
  comments.FindLast(e);
  append_spaces(e(Token).text, 2);
  comments.FindFirst(e);
  WHILE e # NIL DO
    IF max_size < lstr.Length1(e(Token).text) THEN
      max_size:= lstr.Length1(e(Token).text);
    END;
    comments.FindNext(e);
  END;
  INC(max_size);


  comments.FindFirst(e);
  WHILE e # NIL DO
    append_spaces(e(Token).text, max_size);
    comments.FindNext(e);
  END;

  min_lead_spaces_num:= MAX(INT);
  comments.FindFirst(e);
  WHILE e # NIL DO
    tmp:= lead_spaces_num(e(Token).text^);
    IF tmp < min_lead_spaces_num THEN
      min_lead_spaces_num:= tmp;
    END;
    comments.FindNext(e);
  END;

  DEC(min_lead_spaces_num);
  IF min_lead_spaces_num > 0 THEN
    comments.FindFirst(e);
    WHILE e # NIL DO
      lstr.Delete(e(Token).text^, 0, min_lead_spaces_num);
      comments.FindNext(e);
    END;
  END;
END format_comments;

(*------------------------------------------------------------------------*)
PROCEDURE NewToken * (VAR tok: Token);
BEGIN
  IF (tok = NIL) & ~getToken(tok) THEN
    NEW(tok);
    tok.text:= NIL;
    tok.original_text:= NIL;
    tok.file:= NIL;
  END;
  tok.token:= token;
  lstr.Assign(text^, tok.text);
  lstr.Assign(original_text^, tok.original_text);
  tok.parsed:= FALSE;
  tok.boundary:= FALSE;
  tok.line:= line_of_last_token_begin;
  tok.pos:= pos_of_last_token_begin;
  lstr.Assign(file_name^, tok.file);
  tok._line:= line;
  tok._pos:= pos;
  tok.pretoken:= FALSE;
END NewToken;


(*------------------------------------------------------------------------*)
PROCEDURE Init * ();
BEGIN
  handler:= NIL; replacement:= NIL;
  lstr.Assign('', scanstr);
  lstr.Assign('', original_text); lstr.Assign('', text);
  pd_parsed:= FALSE; pd_define_parsed:= FALSE;
  pd_include_parsed:= FALSE; was_blank:= FALSE;
  pd_header_parsed:= FALSE;
  read_file_exists:= FALSE; replace_on:= TRUE;
  comments:= TRUE; skipping_mode:= FALSE; skipp_eol:= TRUE;
  transparent_numeration:= FALSE;
  total_lines:= 0; line:= 0; pos:= 0;
  line_of_last_token_begin:= 0; pos_of_last_token_begin:= 0;
  token:= objs.scan_error;
  adt.Deallocate(files_stack);
  adt.NewStack(files_stack);
  adt.Deallocate(replacements);
  adt.NewStack(replacements);
  adt.Deallocate(tokens);
  adt.NewList(tokens);
END Init;

(*------------------------------------------------------------------------*)
PROCEDURE Open * (name-: ARRAY OF CHAR): BOOLEAN;
VAR
BEGIN
  IF read_file_exists THEN  push_file()  END;
  file_name:= NIL;
  IF file.Open(name, file.rdmode, handler) THEN
    pd_parsed:= FALSE;
    pd_define_parsed:= FALSE;
    pd_include_parsed:= FALSE;
    pd_header_parsed:= FALSE;
    comments:= TRUE;
    replacement:= NIL;
    adt.NewStack(replacements);
    lstr.Assign(name, file_name);
    read_file_exists:= TRUE;
    scanstr:= NIL;
    line:= 0;
    pos:= 0;
    IF ~transparent_numeration OR files_stack.IsEmpty() THEN
      total_lines:= 0;
    END;
    RdPartOfScanStr();
    lstr.Assign(scanstr^, logicalStr);
    lstr.Assign("", prevLogicalStr);
    was_eof:= FALSE;
    RETURN TRUE;
  ELSE
    read_file_exists:= FALSE;
    (*
    (*	error  *)
    msg.Line:= line;
    msg.Pos:= pos+1;
    msg.Error(6, name);
    *)
    RETURN FALSE;
  END;
END Open;

(*------------------------------------------------------------------------*)
PROCEDURE Close ();
VAR
  elem: adt.Element;
BEGIN
  IF read_file_exists THEN
    file.Close( handler );
    read_file_exists:= FALSE;
  END;
  files_stack.Pop(elem);
  WHILE elem # NIL DO
    file.Close( elem(File).handler );
    files_stack.Pop(elem);
  END;
END Close;

(*------------------------------------------------------------------------*)
PROCEDURE GetPrevLogicalString * (VAR str: lstr.String);
(* must be called only after read token EOL *)
BEGIN
  lstr.Assign( prevLogicalStr^, str );
END GetPrevLogicalString;

(*------------------------------------------------------------------------*)
PROCEDURE GetToken * ();
VAR
  str: lstr.String;
  old_line, old_pos: INT;
  tmp: BOOLEAN;
  begin_of_line: BOOLEAN;
  e: adt.Element;
  next_token: Token;
  comments_list: adt.List;
BEGIN
  IF was_eof THEN
    pop_file();
    was_eof:= FALSE;
  END;
  tokens.FindFirst(e);
  IF e # NIL THEN
    WITH e: Token DO
      token:= e.token;
      lstr.Assign(e.text^, text);
      lstr.Assign(e.original_text^, original_text);
      line:= e._line;
      pos:= e._pos;
      line_of_last_token_begin:= e.line;
      pos_of_last_token_begin:= e.pos;
      tokens.DeleteCurrent();
    END;
    putToken(e(Token));
    RETURN;
  END;
  lstr.Assign('', text);
  lstr.Assign('', original_text);
  IF get_next_token_from_current_replacement_list() THEN RETURN END;
  IF read_file_exists THEN
    begin_of_line:= pos = 0;
    skip_blanks();
    line_of_last_token_begin:= line;
    pos_of_last_token_begin:= pos;
    IF (pd_header_parsed) & (scanstr[pos] # EOL) THEN
      get_include_name(); IF msg.WasError THEN RETURN END;
      RETURN;
    ELSIF pd_include_parsed THEN
      get_include_name(); IF msg.WasError THEN RETURN END;
      pd_include_parsed:= FALSE;
      RETURN;
    END;
    lstr.Assign('', text);
    lstr.Assign('', original_text);
    token:= objs.scan_error;
    IF prj_cfg_on THEN
      IF begin_of_line & (scanstr[pos] = '!') THEN
        scanstr[pos]:= '#';
      END;
      IF scanstr[pos] = '%' THEN
        scanstr[pos]:= '/';
        IF scanstr[pos] = EOL THEN
          lstr.AppendChar('/', scanstr);
        ELSE
          scanstr[pos+1]:= '/';
        END;
      END;
    END;
    CASE scanstr[pos] OF
      EOL: lstr.Assign(logicalStr^, prevLogicalStr);
           RdPartOfScanStr();
           lstr.Assign(scanstr^, logicalStr);
           IF pd_parsed THEN
	     pd_parsed:= FALSE;
	     pd_define_parsed:= FALSE;
             pd_include_parsed:= FALSE;
             pd_header_parsed:= FALSE;
	     comments:= TRUE;
	     token:= objs.eol;
	   ELSE
	     GetToken(); IF msg.WasError THEN RETURN END;
	   END;
     |EOF: token:= objs.eof;
           was_eof:= TRUE;
           last_header_total_lines:= total_lines;
           file.Close( handler );
           read_file_exists:= FALSE;
     |'!': IF nextis('=') THEN
	     token:= objs.not_equ;
	     inc();
	   ELSE
	     token:= objs.not;
	   END;
           inc();
     |'"': get_str_const('"'); IF msg.WasError THEN RETURN END;
	   token:= objs.str_const;
     |'#': IF pd_parsed THEN
	     IF nextis('#') THEN
	       inc();
	       token:= objs.double_dies;
	     ELSE
	       token:= objs.dies;
	     END;
	     inc();
	   ELSIF begin_of_line THEN
	     inc(); skip_blanks();
	     IF scanstr[pos] = EOL THEN
	       GetToken(); IF msg.WasError THEN RETURN END;
	     ELSE
	       copy_before_splitter();
	       IF find(preprocessor_directives) THEN
		 pd_parsed:= TRUE;
		 pd_define_parsed:= token = objs.pd_define;
                 pd_include_parsed:= (token = objs.pd_include)
					    OR
				     (token = objs.pd_merge)
					    OR
                                     (token = objs.pd_module)
                                            OR
                                     (token = objs.pd_name);
                 pd_header_parsed:= (token = objs.pd_header);
		 comments:= pd_define_parsed;
		 skip_pd();
	       ELSE
		 (* error *)
                 line:= line_of_last_token_begin;
                 pos:= pos_of_last_token_begin;
                 lstr.Assign('#', str);
                 lstr.Append(text^, str);
                 Warning(7, str^);
                 WHILE scanstr[pos] # EOL DO inc() END;
                 lstr.Assign(logicalStr^, prevLogicalStr);
                 RdPartOfScanStr();
                 lstr.Assign(scanstr^, logicalStr);
                 GetToken(); IF msg.WasError THEN RETURN END;
	       END;
	     END;
	   ELSE
	     (* error *)
             Error(8, '#');
	     RETURN;
	   END;
     |'%': token:= objs.mod;
	   IF nextis('=') THEN
	     inc();
	     token:= objs.mod_assign;
	   ELSE
	     token:= objs.mod;
	   END;
	   inc();
     |'&': IF nextis('&') THEN
	     inc();
	     token:= objs.l_and;
	   ELSIF nextis('=') THEN
	     inc();
	     token:= objs.b_and_assign;
	   ELSE
	     token:= objs.ampersand;
	   END;
	   inc();
     |"'": get_str_const("'"); IF msg.WasError THEN RETURN END;
	   IF lstr.Length1(text) > 1 THEN
	     (* error *)
	     Error(9, '');
	     RETURN;
	   END;
	   token:= objs.char_const;
     |'(': token:= objs.open_round_brack;
	   inc();
     |')': token:= objs.close_round_brack;
	   inc();
     |'*': IF nextis('=') THEN
	     inc();
	     token:= objs.mul_assign;
	   ELSE
	     token:= objs.star;
	   END;
	   inc();
     |'+': IF nextis('=') THEN
	     inc();
	     token:= objs.plus_assign;
	   ELSE
	     token:= objs.plus;
	   END;
	   inc();
     |',': token:= objs.comma;
	   inc();
     |'-': IF nextis('=') THEN
	     inc();
	     token:= objs.minus_assign;
	   ELSIF nextis('>') THEN
	     inc();
	     token:= objs.pointer;
	   ELSE
	     token:= objs.minus;
	   END;
	   inc();
     |'.': IF nextis('.') & is(pos+2,'.') THEN
	     token:= objs.three_points; inc(); inc();
	   ELSIF (scanstr[pos+1] >= '0') & (scanstr[pos+1] <= '9') THEN
	     copy_before_splitter();
	     token:= objs.real_const;
             check_number(text);
	   ELSE
	     token:= objs.point;
	   END;
	   inc();
     |'/': IF nextis('*') THEN
             skipp_eol:= FALSE;
	     old_line:= line; old_pos:= pos;
	     inc(); inc();
	     token:= objs.comment;
	     was_blank:= TRUE;
             adt.NewList(comments_list);
	     LOOP
	       WHILE ~is(pos,EOL) & (~is(pos,'*') OR ~nextis('/')) DO
		 IF is(pos,EOF) THEN
		   (* error *)
		   line:= old_line;
		   pos:= old_pos;
		   Error(10, '');
		   RETURN;
		 END;
                 lstr.AppendChar(scanstr[pos], text);
		 inc();
	       END;
	       IF is(pos,EOL) THEN
                 next_token:= NIL;
		 NewToken(next_token);
                 lstr.Append(scanstr^, logicalStr);
                 RdPartOfScanStr();
                 lstr.Assign('', text);
                 comments_list.Insert(next_token);
	       ELSE
                 skipp_eol:= TRUE;
                 inc(); inc();
                 next_token:= NIL;
                 NewToken(next_token);
                 IF ~comments_list.IsEmpty() THEN
                   comments_list.Insert(next_token);
                 ELSE
                   putToken(next_token);
		 END;
		 EXIT;
	       END;
	     END;
	     IF ~comments OR skipping_mode THEN
	       GetToken(); IF msg.WasError THEN RETURN END;
               adt.Deallocate(comments_list);
             ELSIF ~comments_list.IsEmpty() THEN
               format_comments(comments_list, old_pos);
               adt.Deallocate(tokens);
               tokens:= comments_list;
	       GetToken(); IF msg.WasError THEN RETURN END;
             ELSE
               adt.Deallocate(comments_list);
	     END;
           ELSIF nextis('/') & (cfg.CPPComment OR prj_cfg_on) THEN
	     (*
             skipp_eol:= FALSE;
	     *)
	     inc(); inc();
	     token:= objs.comment;
	     was_blank:= TRUE;
	     WHILE ~is(pos,EOL) DO
               lstr.AppendChar(scanstr[pos], text);
	       inc();
	     END;
	     (*
             skipp_eol:= TRUE;
	     *)
	     IF ~comments OR skipping_mode THEN
	       GetToken(); IF msg.WasError THEN RETURN END;
	     END;
	   ELSIF nextis('=') THEN
	     inc(); inc();
	     token:= objs.div_assign;
	   ELSE
	     token:= objs.div;
	     inc();
	   END;
     |'0'..'9':
	   copy_before_splitter();
	   IF is(pos,'.') THEN
	     copy_before_splitter();
	   END;
           lstr.Assign(text^, original_text);
	   tmp:= skipping_mode;
	   skipping_mode:= TRUE;
           check_number(text);
           skipping_mode:= tmp;
           IF bad_const THEN
             lstr.Assign(original_text^, text);
	     IF (is(pos-1, 'E') OR is(pos-1, 'e'))
				&
		  (is(pos, '+') OR is(pos, '-'))
	     THEN
	       copy_before_splitter();
	     END;
             lstr.Assign(text^, original_text);
             check_number(text);
           END;
     |';': token:= objs.point_comma;
	   inc();
     |':': token:= objs.two_points;
	   inc();
     |'<': IF nextis('=') THEN
	     token:= objs.less_equ; inc();
	   ELSIF nextis('<') THEN
	     inc();
	     IF nextis('=') THEN
	       inc();
	       token:= objs.b_lshift_assign;
	     ELSE
	       token:= objs.b_lshift;
	     END;
	   ELSE
	     token:= objs.less;
	   END;
	   inc();
     |'=': IF nextis('=') THEN
	     token:= objs.equ; inc();
	   ELSE
	     token:= objs.assign;
	   END;
	   inc();
     |'>': IF nextis('=') THEN
	     token:= objs.more_equ; inc();
	   ELSIF nextis('>') THEN
	     inc();
	     IF nextis('=') THEN
	       inc();
	       token:= objs.b_rshift_assign;
	     ELSE
	       token:= objs.b_rshift;
	     END;
	   ELSE
	     token:= objs.more;
	   END;
	   inc();
     |'?': token:= objs.question;
	   inc();
     |'[': token:= objs.open_square_brack;
	   inc();
     |']': token:= objs.close_square_brack;
	   inc();
     |'^': IF nextis('=') THEN
	     inc();
	     token:= objs.b_xor_assign;
	   ELSE
	     token:= objs.b_xor;
	   END;
	   inc();
     |'A'..'Z', '_', 'a'..'z':
	   copy_before_splitter();
           lstr.Assign(text^, original_text);
           IF (~pd_parsed) & find_replacement() THEN
	     IF msg.WasError THEN RETURN END;
	     GetToken(); IF msg.WasError THEN RETURN END;
	   ELSIF pd_define_parsed THEN
	     pd_define_parsed:= FALSE;
	     IF is(pos,'(') THEN
	       token:= objs.pmacro_name;
	     ELSE
	       token:= objs.macro_name;
	     END;
	   ELSIF ~find(keywords) THEN
	     token:= objs.ident;
	   END;
     |'{': token:= objs.open_figure_brack;
	   inc();
     |'|': IF nextis('|') THEN
	     inc();
	     token:= objs.l_or;
	   ELSIF nextis('=') THEN
	     inc();
	     token:= objs.b_or_assign;
	   ELSE
	     token:= objs.b_or;
	   END;
	   inc();
     |'}': token:= objs.close_figure_brack;
	   inc();
     |'~': token:= objs.tilda;
	   inc();
    ELSE
      (*  error  *)
      str:= NIL;
      lstr.AppendChar(scanstr[pos], str);
      Error(8, str^);
      RETURN;
    END;
  ELSE
    token:= objs.eof;
  END;
  msg.Progress(objs.CurrentHeader.name^, total_lines);
END GetToken;


(*------------------------------------------------------------------------*)
PROCEDURE InitKeyWordTree();
VAR kw: KeyWord;
  PROCEDURE ins(w-: ARRAY OF CHAR; c: INT; ext: BOOLEAN);
  BEGIN
    NEW(kw); kw.SetName(w); kw.code:= c; kw.ext:= ext; keywords.Insert(kw);
  END ins;
BEGIN
  adt.NewTree(keywords);
  ins("int", objs.int, FALSE);                 ins("double", objs.double, FALSE);
  ins("asm", objs.asm, FALSE);                 ins("break", objs.break, FALSE);
  ins("continue", objs.continue, FALSE);       ins("const", objs.const, FALSE);
  ins("enum", objs.enum, FALSE);               ins("else", objs.else, FALSE);
  ins("for", objs.for, FALSE);                 ins("if", objs.if, FALSE);
  ins("register", objs.register, FALSE);       ins("long", objs.long, FALSE);
  ins("sizeof", objs.sizeof, FALSE);           ins("union", objs.union, FALSE);
  ins("unsigned", objs.unsigned, FALSE);       ins("void", objs.void, FALSE);
  ins("case", objs.case, FALSE);               ins("auto", objs.auto, FALSE);
  ins("default", objs.default, FALSE);         ins("char", objs.char, FALSE);
  ins("do", objs.do, FALSE);                   ins("float", objs.float, FALSE);
  ins("extern", objs.extern, FALSE);           ins("goto", objs.goto, FALSE);
  ins("struct", objs.struct, FALSE);           ins("short", objs.short, FALSE);
  ins("return", objs.return, FALSE);           ins("static", objs.static, FALSE);
  ins("switch", objs.switch, FALSE);           ins("typedef", objs.typedef, FALSE);
  ins("while", objs.while, FALSE);             ins("signed", objs.signed, FALSE);
  ins("volatile", objs.volatile, FALSE);       ins("interrupt", objs.interrupt, FALSE);
  ins("pascal", objs.pascal, FALSE);           ins("fortran", objs.fortran, FALSE);
  ins("cdecl", objs.cdecl, FALSE);             ins("far", objs.far, FALSE);
  ins("near", objs.near, FALSE);               ins("huge", objs.huge, FALSE);
  ins("syscall", objs.syscall, FALSE);
END InitKeyWordTree;

(*------------------------------------------------------------------------*)
PROCEDURE InitPreProcessorDirectivesTree();
VAR n: KeyWord;
  PROCEDURE ins(w-: ARRAY OF CHAR; c: INT; ext: BOOLEAN);
  BEGIN
    NEW(n); n.SetName(w);
    n.code:= c;
    n.ext:= ext;
    preprocessor_directives.Insert(n);
  END ins;
BEGIN
  ins("define" , objs.pd_define , FALSE);
  ins("if"     , objs.pd_if     , FALSE);
  ins("endif"  , objs.pd_endif  , FALSE);
  ins("elif"   , objs.pd_elif   , FALSE);
  ins("else"   , objs.pd_else   , FALSE);
  ins("ifdef"  , objs.pd_ifdef  , FALSE);
  ins("ifndef" , objs.pd_ifndef , FALSE);
  ins("undef"  , objs.pd_undef  , FALSE);
  ins("line"   , objs.pd_line   , FALSE);
  ins("error"  , objs.pd_error  , FALSE);
  ins("include", objs.pd_include, FALSE);
  ins("pragma" , objs.pd_pragma , FALSE);
  ins("merge"  , objs.pd_merge  , TRUE );
  ins("module" , objs.pd_module , TRUE );
  ins("end"    , objs.pd_end    , TRUE );
(*
  ins("bitset" , objs.pd_bitset , TRUE );
  ins("parameters" , objs.pd_parameters , TRUE );
*)
  ins("footer" , objs.pd_footer , TRUE );
  ins("name"   , objs.pd_name   , TRUE );
  ins("variant", objs.pd_variant, TRUE );
  ins("header" ,  objs.pd_header, TRUE );
END InitPreProcessorDirectivesTree;
(*------------------------------------------------------------------------*)
PROCEDURE InitNonPresentableCharArray();
VAR
  i: INT;
BEGIN
  FOR i:= 0 TO 255 DO
    non_presentable_char_array[i]:= CHR(i);
  END;
  non_presentable_char_array[ORD('n')]:= 0AX;
  non_presentable_char_array[ORD('t')]:= 09X;
  non_presentable_char_array[ORD('v')]:= 0BX;
  non_presentable_char_array[ORD('b')]:= 08X;
  non_presentable_char_array[ORD('r')]:= 0DX;
  non_presentable_char_array[ORD('f')]:= 0CX;
  non_presentable_char_array[ORD('a')]:= 07X;
  non_presentable_char_array[ORD("'")]:= 2CX;
  non_presentable_char_array[ORD('"')]:= 22X;
  non_presentable_char_array[ORD('\')]:= 5CX;
END InitNonPresentableCharArray;

PROCEDURE InitTmpVars();
BEGIN
  lstr.Assign('                                                                               ',
                tmp_str_for_formatting_comments);
  NEW(tmp_pattr_for_finding_keywords);
  NEW(tmp_pattr_for_finding_replacements);
  adt.NewList(empty_token_list);
END InitTmpVars;


(*------------------------------------------------------------------------*)


BEGIN
  Init();
  file_name:= NIL;
  extensions:= TRUE;
  adt.NewTree(keywords);
  adt.NewTree(preprocessor_directives);
  InitKeyWordTree();
  InitPreProcessorDirectivesTree();
  InitNonPresentableCharArray();
  InitTmpVars();
END H2DScan.
(*
io.printf('100, token = %d, %s\n',token, text^);
*)
