(** Copyright (c) 1991,97 XDS Ltd, Russia. All Rights Reserved. *)
MODULE ccComments; (** Sem 22-Sep-93. *)

(* Modifications:
   17/Mar/96 Ned  2.12  Almost all functions are got from BE_C for BNRP.
                        Comments copying is improved.
*)

IMPORT
  pc :=pcK,
  cc :=ccK,
  out:=ccL,
  seg:=ccSeg,
  env:=xiEnv;

CONST
  ctag_ok = VAL(pc.CTAG, 20);
  TLEN = 2048;

TYPE
  CPOS = POINTER TO CPOS_REC;
  CPOS_REC = RECORD
    pos: pc.TPOS;
    nxt: CPOS;
  END;
  CLST = POINTER TO CLST_REC;
  CLST_REC = RECORD
    cmt: pc.Comment;
    l,r: CLST;
    bal: INTEGER;
    nxt: CLST;
  END;

VAR
  cur_ps: CPOS;
  cmts  : CLST;    (* comment records tree *)
  list  : CLST;    (* comment records sorted list *)

PROCEDURE comment(c: pc.Comment; tail: BOOLEAN; lev: INTEGER);

  VAR
    prev  : CHAR;    (* previous character *)
    last  : BOOLEAN; (* is the last part of the comment? *)
    skip  : LONGINT; (* number of spaces to skip for indentation *)
    indent: LONGINT;

  PROCEDURE ws(s-: ARRAY OF CHAR; i: LONGINT);
    VAR ch: CHAR; len: LONGINT; sp: BOOLEAN;
  BEGIN
    len:=LENGTH(s); sp:=FALSE;
    IF last THEN
      IF (len>=2) & (s[len-2]='*') & (s[len-1]=')') THEN
        DEC(len,2);
      ELSIF (len>=1) & (s[len-1]=pc.EOL) THEN
        DEC(len); sp:=TRUE;
      END;
    END;
    WHILE i < len DO
      ch:=s[i];
      IF (skip > 0) & (ch = ' ') THEN DEC(skip)
      ELSE
        IF skip > 0 THEN skip:=0 END;
        (* '/*' in comment produce warning, '*/' - error *)
        IF (prev='/') & (ch='*') THEN ch:=' ' END;
        IF (prev='*') & (ch='/') THEN ch:=' ' END;
        IF ch=pc.EOL THEN out.wl; out.sp(lev); skip:=indent;
        ELSE out.wr(ch)
        END;
      END;
      prev:=ch;
      INC(i);
    END;
    IF sp THEN out.wr(' ') END;
  END ws;

  VAR
    fnm : env.String;
    ln,i: LONGINT;
    lc  : BOOLEAN; (* single line comment *)
BEGIN
  INCL(c.tags,ctag_ok);
  prev:=0X;
  c.pos.unpack(fnm,ln,indent);
  IF tail THEN out.wr(' ') ELSE out.sp(lev) END;
  lc:=cc.op_cpp & (LENGTH(c.str^)>=2) & (c.str[0]='-') & (c.str[1]='-');
  IF lc THEN out.ws("//") ELSE out.ws("/*") END;
  i:=2; skip:=0;
  LOOP
    last:=~ c.next.pos.IsNull();
    ws(c.str^,i);
    IF last THEN EXIT END;
    i:=0; c:=c.next;
  END;
  IF ~ lc THEN out.ws("*/") END;
  IF ~ tail THEN out.wl END;
END comment;

PROCEDURE range(fr-,to-: pc.TPOS; tail: BOOLEAN; lev: INTEGER);
  VAR
    l : CLST;
    c : pc.Comment;
    i : LONGINT;
BEGIN
  l:=cmts;
  IF l=NIL THEN RETURN END;
  LOOP
    ASSERT(NOT l.cmt.end.IsNull());
    i:=fr.cmp(l.cmt.pos);
    IF i<0 THEN
      IF l.l=NIL THEN EXIT END;
      l:=l.l;
    ELSIF i=0 THEN
      EXIT
    ELSE
      IF l.r=NIL THEN EXIT END;
      l:=l.r;
    END;
  END;
  WHILE (l#NIL) & fr.gtr(l.cmt.pos) DO l:=l.nxt END;
  LOOP
    IF l=NIL THEN RETURN END;
    c:=l.cmt; l:=l.nxt;
    ASSERT(~ c.pos.IsNull());
    IF c.pos.gtr(to) THEN RETURN END;
    IF ~ (ctag_ok IN c.tags) THEN comment(c,tail,lev) END;
  END;
END range;

PROCEDURE first_comment*(ps-: pc.TPOS; df,lev: INTEGER);
  VAR fnm: env.String; l,c: LONGINT; fr: pc.TPOS; cp: CPOS;
BEGIN
  IF ~ ps.IsNull() THEN
    ps.unpack(fnm,l,c);
    IF l>df THEN l:=l-df ELSE l:=0 END;
    fr.pack(fnm^,l,0);
    range(fr,ps,FALSE,lev);
  END;
  NEW(cp);
  cp.nxt:=cur_ps;
  cur_ps:=cp;
  cp.pos:=ps;
END first_comment;

PROCEDURE next_comment*(ps: pc.TPOS; lev: INTEGER);
BEGIN
  IF ps.IsNull() OR (cur_ps=NIL) OR cur_ps.pos.IsNull() THEN RETURN END;
  IF ps.gtr(cur_ps.pos) THEN
    range(cur_ps.pos,ps,FALSE,lev);
    cur_ps.pos:=ps;
  END;
END next_comment;

PROCEDURE tail_comment*(ps-: pc.TPOS);
  VAR fnm: env.String; l,c: LONGINT; to: pc.TPOS;
BEGIN
  IF ~ ps.IsNull() THEN
    ps.unpack(fnm,l,c);
    to.pack(fnm^,l,MAX(INTEGER));
    range(ps,to,TRUE,0)
  END;
END tail_comment;

PROCEDURE last_comment*(ps: pc.TPOS; lev: INTEGER);
  VAR fnm: env.String; l,c: LONGINT; to: pc.TPOS;
BEGIN
  IF cur_ps=NIL THEN RETURN END;
  IF ~ ps.IsNull() & ~ cur_ps.pos.IsNull() THEN
    ps.unpack(fnm,l,c);
    to.pack(fnm^,l+1,8); (*!!! was 80 *)
    IF to.gtr(cur_ps.pos) THEN range(cur_ps.pos,to,FALSE,lev) END;
  END;
  cur_ps:=cur_ps.nxt;
END last_comment;

PROCEDURE out_all_comments*(md: pc.OBJECT);
  VAR f0,f1: env.String; l,c: LONGINT; s: seg.SEGMENT; p: CLST;
BEGIN
  IF ~md.pos.IsNull() THEN
    md.pos.unpack(f0,l,c);
    p:=list;
    WHILE p#NIL DO
      IF ~(ctag_ok IN p.cmt.tags) THEN
        p.cmt.pos.unpack(f1,l,c);
        IF f1^=f0^ THEN
          seg.new_seg(s,p.cmt.pos,0);
          seg.enter(s);
          comment(p.cmt,FALSE,0);
          seg.exit;
        END;
      END;
      p:=p.nxt;
    END;
  END;
END out_all_comments;

PROCEDURE app_bal(x: CLST; VAR p: CLST; VAR h: BOOLEAN): CLST;
(* h = TRUE -> tree becomes higher
   p.bal = high(p.r) - high(p.l)
*)
  VAR p1,p2,y: CLST; r: INTEGER;
BEGIN
  IF p=NIL THEN
    p:=x; x.l:=NIL; x.r:=NIL; h:=TRUE; x.bal:=0; RETURN NIL;
  END;
  r:=p.cmt.pos.cmp(x.cmt.pos);
  IF r>0 THEN
    y:=app_bal(x,p.l,h);
    IF h THEN
      CASE p.bal OF
	|+1: p.bal:=0; h:=FALSE;
	| 0: p.bal:=-1;
	|-1:
	  p1:=p.l;
	  IF p1.bal=-1 THEN
	    p.l:=p1.r; p1.r:=p; p.bal:=0; p:=p1;
	  ELSE
	    p2:=p1.r; p1.r:=p2.l; p2.l:=p1; p.l:=p2.r; p2.r:=p;
	    IF p2.bal=-1 THEN p.bal:=+1 ELSE p.bal:=0 END;
	    IF p2.bal=+1 THEN p1.bal:=-1 ELSE p1.bal:=0 END;
	    p:=p2;
	  END;
	  p.bal:=0; h:=FALSE;
      END;
    END;
    RETURN y;
  ELSIF r<0 THEN
    y:=app_bal(x,p.r,h);
    IF h THEN
      CASE p.bal OF
	|-1: p.bal:=0; h:=FALSE;
	| 0: p.bal:=+1;
	|+1:
	  p1:=p.r;
	  IF p1.bal=+1 THEN
	    p.r:=p1.l; p1.l:=p; p.bal:=0; p:=p1;
	  ELSE
	    p2:=p1.l; p1.l:=p2.r; p2.r:=p1; p.r:=p2.l; p2.l:=p;
	    IF p2.bal=+1 THEN p.bal:=-1 ELSE p.bal:=0 END;
	    IF p2.bal=-1 THEN p1.bal:=+1 ELSE p1.bal:=0 END;
	    p:=p2;
	  END;
	  p.bal:=0; h:=FALSE;
      END;
    END;
    RETURN y;
  ELSE
    h:=FALSE; RETURN p;
  END;
END app_bal;

PROCEDURE append(c: pc.Comment);
  VAR h: BOOLEAN; l: CLST;
BEGIN
  ASSERT(~c.pos.IsNull());
  NEW(l);
  l.cmt:=c;
  l:=app_bal(l,cmts,h);
  ASSERT(l=NIL);
END append;

PROCEDURE sort(x: CLST);
BEGIN
  IF x=NIL THEN RETURN END;
  sort(x.r);
  x.nxt:=list;
  list:=x;
  sort(x.l);
END sort;

PROCEDURE ini*;
  VAR c,e: pc.Comment;
BEGIN
  IF pc.comments#NIL THEN
    cmts:=NIL;
    list:=NIL;
    e:=pc.comments;
    WHILE e.pos.IsNull() DO e:=e.next END;
    c:=e;
    REPEAT
      IF ~(ctag_ok IN c.tags) THEN append(c) END;
      c:=c.next;
      WHILE c.pos.IsNull() DO c:=c.next END;
    UNTIL c=e;
    sort(cmts);
  END;
  cur_ps:=NIL;
END ini;

PROCEDURE exi*;
BEGIN
  cmts:=NIL;
  list:=NIL;
  cur_ps:=NIL;
END exi;

END ccComments.
