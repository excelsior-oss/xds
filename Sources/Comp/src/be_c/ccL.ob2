(** Copyright (c) 1991,97 XDS Ltd, Russia. All Rights Reserved. *)
MODULE ccL; (** Sem 22-Sep-93. *)

IMPORT
  SYSTEM,
  xcStr,
  FormOut,
  pcK,
  cc:=ccK,
  xfs:=xiFiles,
  env:=xiEnv;

CONST
  STK_SIZE = 20;
  STR_SIZE = 4096;

TYPE
  STR*=POINTER TO STR_REC;
  STR_REC=RECORD
    str: pcK.STRING;
    fwd: STR;
    bck: STR;
    col: INTEGER;
  END;
  STK=POINTER TO STK_REC;
  STK_REC=RECORD
    str   : pcK.STRING;
    str_ps: INTEGER;
    col_ps: INTEGER;
    wp_max: INTEGER;
    wp_pos: INTEGER;
    wp_en : BOOLEAN; (* enable automatic LF *)
    seq   : STR;
  END;
  POS*=RECORD
    str_ps: INTEGER;
    col_ps: INTEGER;
    wp_max: INTEGER;
    wp_pos: INTEGER;
    seq   : STR;
    bck   : STR;
  END;

VAR
  nl     : ARRAY 8 OF CHAR;
  wp     : ARRAY 8 OF CHAR;
  lch    : CHAR; (* last char in line, usally 0AX *)

  code_width : LONGINT;
  indent_step: LONGINT;

  str    : pcK.STRING;
  str_ps : INTEGER;
  col_ps-: INTEGER;
  wp_max : INTEGER;
  wp_pos : INTEGER;
  wp_en  : BOOLEAN;
  seq    : STR;

  stack  : ARRAY STK_SIZE OF STK;
  sptr     : INTEGER;

  wp_lev : ARRAY 256 OF SHORTINT;

PROCEDURE tie(VAR root: STR; s: STR);
  VAR re,se: STR;
BEGIN
  IF root=NIL THEN
    root:=s;
  ELSIF s.fwd=s THEN
    s.fwd:=root; s.bck:=root.bck;
    s.fwd.bck:=s; s.bck.fwd:=s;
  ELSE
    se:=s.bck; re:=root.bck;
    s.bck:=re; se.fwd:=root;
    re.fwd:=s; root.bck:=se;
  END;
END tie;

PROCEDURE flush_str;
  VAR i: LONGINT; p: pcK.STRING; s: STR;
BEGIN
  IF str_ps=0 THEN RETURN END;
  NEW(p,str_ps);
  FOR i:=0 TO str_ps-1 DO p[i]:=str[i] END;
  str_ps:=0;
  wp_max:=-1;
  NEW(s);
  s.str:=p;
  s.col:=col_ps;
  s.fwd:=s;
  s.bck:=s;
  tie(seq,s);
END flush_str;

PROCEDURE wstr*(s: STR);
BEGIN
  IF s=NIL THEN RETURN END;
  flush_str;
  tie(seq,s);
  col_ps:=s.bck.col;
  wp_en:=TRUE;
END wstr;

PROCEDURE gstr*(VAR s: STR);
BEGIN
  flush_str;
  s:=seq;
  seq:=NIL;
  col_ps:=0;
  wp_en:=TRUE;
END gstr;

PROCEDURE push*;
  VAR s: STK; t: pcK.STRING;
BEGIN
  IF stack[sptr]=NIL THEN NEW(stack[sptr]) END;
  s:=stack[sptr]; INC(sptr);
  IF s.str#NIL THEN t:=s.str ELSE t:=NIL END;
  s.str:=str;
  s.str_ps:=str_ps;
  s.col_ps:=col_ps;
  s.wp_max:=wp_max;
  s.wp_pos:=wp_pos;
  s.wp_en:=wp_en;
  s.seq:=seq;
  str:=t;
  str_ps:=0;
  col_ps:=0;
  wp_max:=-1;
  wp_pos:=0;
  wp_en:=TRUE;
  seq:=NIL;
  IF str=NIL THEN NEW(str,STR_SIZE) END;
END push;

PROCEDURE pop*;
  VAR s: STK; t: pcK.STRING;
BEGIN
  ASSERT(seq=NIL);
  ASSERT(str_ps=0);
  ASSERT(str#NIL);
  DEC(sptr); s:=stack[sptr];
  t:=str;
  str:=s.str;
  str_ps:=s.str_ps;
  col_ps:=s.col_ps;
  wp_max:=s.wp_max;
  wp_pos:=s.wp_pos;
  wp_en:=s.wp_en;
  seq:=s.seq;
  s.str:=t;
  ASSERT(str#NIL);
END pop;

PROCEDURE append*(VAR txt: STR);
(*
    Добавляет текущий текст к тексту txt
*)
  VAR app: STR;
BEGIN
  gstr(app);
  IF app=NIL THEN RETURN END;
  tie(txt,app);
END append;

PROCEDURE wr0(ch: CHAR);
  (* put char without checking of current position *)
BEGIN
  IF str_ps>=STR_SIZE THEN flush_str END;
  str[str_ps]:=ch; INC(str_ps);
  IF ch=lch THEN
    col_ps:=0;
    wp_max:=-1;
    IF str_ps>=STR_SIZE*3 DIV 4 THEN flush_str END;
  ELSE
    INC(col_ps);
    IF wp_en & (wp_lev[ORD(ch)]>=wp_max) THEN
      wp_max:=wp_lev[ORD(ch)];
      wp_pos:=str_ps-1;
    END;
  END;
END wr0;

PROCEDURE ws0(s-: ARRAY OF CHAR);
  (* put string without checking of current position *)
  VAR ch: CHAR; i: INTEGER;
BEGIN
  i:=0;
  LOOP
    ch:=s[i]; INC(i);
    IF ch=0X THEN EXIT END;
    wr0(ch);
  END;
END ws0;

PROCEDURE wrap;
(* put automatic LF *)
  VAR i,j,l: INTEGER; bf: ARRAY 256 OF CHAR;
BEGIN
  IF ~wp_en THEN
    ws0(wp); RETURN;
  ELSE
    IF (wp_max<0) OR (col_ps-str_ps+wp_pos<32) THEN RETURN END;
    i:=wp_pos+1;
  END;
  (* i - позиция в str, начиная с которой нужно перенести на новую строку *)
  j:=0; l:=str_ps; str_ps:=i;
  WHILE (i<l) & (str[i]=' ') DO INC(i) END;
  IF l-i>=LEN(bf) THEN str_ps:=l; RETURN END;
  WHILE (i<l) DO bf[j]:=str[i]; INC(i); INC(j) END;
  bf[j]:=0X;
  ws0(nl); ws0("                "); ws0(bf);
END wrap;

PROCEDURE wr*(ch: CHAR);
BEGIN
  IF col_ps>=code_width THEN wrap END;
  wr0(ch);
END wr;

PROCEDURE disable_lf*(VAR prev: BOOLEAN);
(** Disables automatic LF *)
BEGIN
  prev:=wp_en;
  wp_en:=FALSE;
END disable_lf;

PROCEDURE restore_lf*(prev: BOOLEAN);
(** Restores previous state *)
BEGIN
  wp_en:=prev;
END restore_lf;

PROCEDURE wrc*(ch: CHAR);
(* запись символа символьного литерала,
   переносы запрещены.
*)
BEGIN
  (* wr0 вызывать нельзя! *)
  IF str_ps>=STR_SIZE THEN flush_str END;
  str[str_ps]:=ch; INC(str_ps); INC(col_ps);
  ASSERT(ch#lch);
END wrc;

PROCEDURE ws*(s-: ARRAY OF CHAR);
  VAR i: LONGINT; ch: CHAR;
BEGIN
  i:=0;
  LOOP
    ch:=s[i]; INC(i);
    IF ch=0X THEN EXIT END;
    IF col_ps>=code_width THEN wrap END;
    wr0(ch);
  END;
END ws;

PROCEDURE wl*;
BEGIN
  ws0(nl);
END wl;

PROCEDURE wint*(n: LONGINT);
BEGIN
  IF n<0 THEN wr('-'); n:=ABS(n) END;
  IF n>=10 THEN wint(n DIV 10) END;
  wr(CHR(ORD('0')+n MOD 10));
END wint;

PROCEDURE sp*(n: INTEGER);
(* put indentation spaces *)
BEGIN
  ASSERT(col_ps=0);
  n:=n*SHORT(indent_step);
  IF n>36 THEN n:=36 END;
  IF str_ps+n>=STR_SIZE THEN flush_str END;
  INC(col_ps,n);
  INC(n,str_ps);
  WHILE str_ps<n DO
    str[str_ps]:=' ';
    INC(str_ps);
  END;
END sp;

PROCEDURE enter_statement*(VAR ps: POS);
BEGIN
  wp_max:=-1; (*!*)
  wp_pos:=-1;
  ps.str_ps:=str_ps;
  ps.col_ps:=col_ps;
  ps.wp_max:=wp_max;
  ps.wp_pos:=wp_pos;
  ps.seq:=seq;
  IF seq#NIL THEN ps.bck:=seq.bck END;
END enter_statement;

PROCEDURE pack_statement*(ps: POS);

  PROCEDURE letter_or_digit(ch: CHAR): BOOLEAN;
  BEGIN
    RETURN (ch>='A') & (ch<='Z') OR
       (ch>='a') & (ch<='z') OR
       (ch>='0') & (ch<='9') OR
       (ch='_');
  END letter_or_digit;

  VAR i,j,k,n: INTEGER; bf: ARRAY 128 OF CHAR; ch,sy: CHAR;
BEGIN
  ASSERT(col_ps=0);
  IF (ps.seq#seq) OR (seq#NIL) & (seq.bck#ps.bck) THEN RETURN END;
  IF ps.col_ps+(str_ps-ps.str_ps)>LEN(bf) THEN RETURN END;
  ASSERT(str[ps.str_ps]='{');
  i:=ps.str_ps+1;
  LOOP
    IF i>=str_ps THEN RETURN END;
    IF str[i]>' ' THEN EXIT END;
    INC(i);
  END;
  j:=str_ps;
  LOOP
    IF j=i THEN RETURN END;
    DEC(j);
    IF str[j]=';' THEN EXIT END;
  END;
  IF ps.col_ps+(j+1-i)>code_width THEN RETURN END;
  IF (j-i>2) & (str[i]='i') & (str[i+1]='f') &
     NOT letter_or_digit(str[i+2])
  THEN
    RETURN
  END;
  n:=0; ch:=0X;
  FOR k:=i TO j-1 DO
    sy:=str[k];
    IF sy='{' THEN RETURN END;
    IF sy=';' THEN RETURN END;
    IF sy='#' THEN RETURN END;
    IF (sy='*') & (ch='/') THEN RETURN END;
    ch:=sy; bf[n]:=sy; INC(n);
  END;
  bf[n]:=';'; INC(n); bf[n]:=0X;
  str_ps:=ps.str_ps;
  col_ps:=ps.col_ps;
  wp_max:=ps.wp_max;
  wp_pos:=ps.wp_pos;
  ws0(bf); wl;
END pack_statement;

<*+ WOFF301 *>
PROCEDURE [1] wf_proc(x: SYSTEM.ADDRESS; s-: ARRAY OF CHAR; l: LONGINT);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO VAL(INTEGER,l-1) DO
    IF col_ps>=code_width THEN wrap END;
    wr0(s[i]);
  END;
END wf_proc;
<*- WOFF301 *>

PROCEDURE wf*(f-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN
  FormOut.format(NIL,wf_proc,f,FormOut.default,SYSTEM.ADR(x),SIZE(x));
END wf;

PROCEDURE ff*(VAR bf: ARRAY OF CHAR; f-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN
  xcStr.prn_bin(bf,f,x);
END ff;

PROCEDURE save*(name: ARRAY OF CHAR; obj: BOOLEAN;
        src_time: xfs.Time; VAR fn: xfs.String);
  VAR
    file: xfs.TextFile;
    err : xfs.String;
    ext : xfs.String;
    tm  : xfs.Time;
    was : BOOLEAN;
    p   : STR;
BEGIN
  ASSERT(sptr=0);
  IF obj THEN ext:=pcK.code.code_ext ELSE ext:=pcK.code.head_ext END;
  xfs.sys.Create('',name,ext^,fn);
  xfs.sys.UseFirst(fn^,fn);
  IF obj THEN was:=FALSE; tm:=MIN(xfs.Time);
  ELSE xfs.sys.ModifyTime(fn^,tm,was);
  END;
  xfs.text.Open(fn^,TRUE);
  IF xfs.text.file=NIL THEN
    env.errors.Fault(env.null_pos,424,xfs.text.msg^);
  ELSE
    file:=xfs.text.file(xfs.TextFile);
    flush_str;
    IF seq#NIL THEN
      p:=seq;
      REPEAT
    file.WriteString(p.str^,LEN(p.str^));
    p:=p.fwd;
      UNTIL p=seq;
    END;
    file.CloseNew(env.errors.err_cnt=0,NOT obj & was & (tm>src_time),TRUE,err);
    IF err#NIL THEN
      env.errors.Fault(env.null_pos,448,err^); -- file write error
    END;
    IF obj & (env.errors.err_cnt=0) THEN env.info.code_file:=fn END;
  END;
END save;

PROCEDURE ini*;
  VAR tmp: env.String;
BEGIN
  str_ps:=0;
  col_ps:=0;
  wp_max:=-1;
  wp_pos:=-1;
  wp_en:=TRUE;
  seq:=NIL;
  sptr:=0;
  xcStr.prn_bin(nl,"\n");
  xcStr.prn_bin(wp,"\\\n");
  lch:=nl[LENGTH(nl)-1];
  NEW(str,STR_SIZE);
  env.config.Equation("GENINDENT",tmp);
  IF (tmp=NIL) OR NOT xcStr.StrToInt(tmp^,indent_step) THEN indent_step:=3;
  ELSIF indent_step<0  THEN indent_step:=0;
  ELSIF indent_step>32 THEN indent_step:=32;
  END;
  IF cc.op_lineno THEN
    code_width:=256;
  ELSE
    env.config.Equation("GENWIDTH",tmp);
    IF (tmp=NIL) OR NOT xcStr.StrToInt(tmp^,code_width) THEN code_width:=77;
    ELSIF code_width<30 THEN code_width:=30;
    END;
  END;
END ini;

PROCEDURE exi*;
  VAR i: INTEGER;
BEGIN
  seq:=NIL;
  str:=NIL;
  str_ps:=0; (*!*)
  FOR i:=0 TO LEN(stack)-1 DO stack[i]:=NIL END;
END exi;

VAR i: INTEGER;

BEGIN
  str_ps:=0;
  col_ps:=0;
  seq:=NIL;
  FOR i:=0 TO 255 DO wp_lev[i]:=-2 END;
  wp_lev[ORD('}')]:=6;
  wp_lev[ORD(';')]:=5;
  wp_lev[ORD(',')]:=4;
  wp_lev[ORD(')')]:=3;
  wp_lev[ORD(']')]:=2;
  wp_lev[ORD('"')]:=1;
  wp_lev[ORD("'")]:=1;
END ccL.
