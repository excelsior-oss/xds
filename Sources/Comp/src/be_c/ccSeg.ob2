(** Copyright (c) 1995,97 XDS Ltd, Russia. All Rights Reserved. *)
MODULE ccSeg;
(** Sem 02-Sep-95. *)

(* Modifications:
   19-Jun-96 Ned  "exit" is optimized.
*)


IMPORT
  out:=ccL,
  xfs:=xiFiles,
  env:=xiEnv;

CONST
  tag_writed  = 0;
  tag_writing = 1;
  tag_entered = 2;

TYPE
  SEGMENT*= POINTER TO SEGMENT_REC;
  SEGLIST = POINTER TO SEGLIST_REC;
  SEGMENT_REC*= RECORD
    pos-  : env.TPOS;
    txt   : out.STR;
    next  : SEGMENT;
    import: SEGLIST;
    tags  : SET;
    pr    : INTEGER;
  END;
  SEGLIST_REC = RECORD
    seg   : SEGMENT;
    next  : SEGLIST;
  END;

VAR
  segment-: SEGMENT;
  segments: SEGMENT;
  last_seg: SEGMENT;

PROCEDURE new_seg*(VAR s: SEGMENT; ps: env.TPOS; pr: INTEGER);
BEGIN
  NEW(s);
  s.pos:=ps;
  s.next:=NIL;
  s.import:=NIL;
  s.tags:={};
  s.pr:=pr; (* priority (for segments with same pos) *)
END new_seg;

PROCEDURE enter*(s: SEGMENT);
BEGIN
  ASSERT(~ (tag_entered IN s.tags));
  INCL(s.tags,tag_entered);
  out.push;
  s.next:=segment;
  segment:=s;
END enter;

PROCEDURE exit*;

  PROCEDURE lss(x,y: SEGMENT): BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    IF y = NIL THEN RETURN TRUE END;
    i:=x.pos.cmp(y.pos);
    RETURN
      (i < 0) OR
      (i = 0) & (x.pr<=y.pr);
  END lss;

  VAR s,l: SEGMENT;
BEGIN
  s:=segment;
  segment:=s.next;
  out.gstr(s.txt);
  out.pop;
  IF lss(s,segments) THEN
    s.next:=segments; segments:=s; RETURN;
  END;
  IF (last_seg = NIL) OR lss(s,last_seg) THEN
    l:=segments;
  ELSE
    l:=last_seg;
  END;
  last_seg:=s;
  LOOP
    IF lss(s,l.next) THEN
      s.next:=l.next; l.next:=s; RETURN;
    END;
    l:=l.next;
  END;
END exit;

PROCEDURE import*(s: SEGMENT);
  VAR l: SEGLIST;
BEGIN
  IF s=NIL THEN RETURN END;
  IF s=segment THEN RETURN END;
  l:=segment.import;
  WHILE l#NIL DO
    IF l.seg=s THEN RETURN END;
    l:=l.next;
  END;
  NEW(l);
  l.seg:=s;
  l.next:=segment.import;
  segment.import:=l;
END import;

PROCEDURE out_file_nm(pos: env.TPOS; VAR file: xfs.String);
  VAR
    f: env.String; i,j: LONGINT;
    dir,fnm,ext: xfs.String;
BEGIN
  (* SC 6.0 fails if this line is before includes! *)
  IF pos.IsNull() THEN RETURN END;
  pos.unpack(f,i,j);
  IF file#f THEN
    file:=f;
    IF ~ env.config.Option("GENFULLFNAME") THEN
      xfs.sys.Get(f^,dir,fnm,ext);
      xfs.sys.Create("",fnm^,ext^,f);
    END;
    out.wf('#line %d "%s"\n',i+1,f^);
  ELSE
    out.wf("#line %d\n",i+1);
  END;
END out_file_nm;

PROCEDURE write*(lineno: BOOLEAN);
  VAR fnm: env.String;

  PROCEDURE write_seg(s: SEGMENT);
    VAR l: SEGLIST;
  BEGIN
    IF tag_writed IN s.tags THEN RETURN END;
    IF tag_writing IN s.tags THEN
      env.errors.Warning(s.pos,353);
      RETURN;
    END;
    INCL(s.tags,tag_writing);
    l:=s.import;
    WHILE l#NIL DO
      write_seg(l.seg);
      l:=l.next;
    END;
    IF lineno THEN out_file_nm(s.pos,fnm) END;
    out.wstr(s.txt);
    EXCL(s.tags,tag_writing);
    INCL(s.tags,tag_writed);
  END write_seg;

BEGIN
  fnm:=NIL;
  WHILE segments#NIL DO
    write_seg(segments);
    segments:=segments.next;
  END;
END write;

PROCEDURE exi*;
BEGIN
  segment:=NIL;
  segments:=NIL;
  last_seg:=NIL;
END exi;

PROCEDURE ini*;
BEGIN
  exi;
END ini;

END ccSeg.
