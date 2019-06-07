(** Copyright (c) 1995,97 XDS Ltd, Russia. All Rights Reserved. *)
MODULE ccRec;

(* Modifications:
   03/Apr/96 Ned  ParseType - append "dummy" field into empty record.
*)

IMPORT
  nms:=ccN,
  pc :=pcK,
  cc :=ccK,
  env:=xiEnv,
  xfs:=xiFiles;

CONST
  m_struct* = 0;
  m_union*  = 1;
  m_field*  = 2;
  m_sl1*    = 3;
  m_word*   = 4;
  m_filler* = 5;
  m_dummy*  = 6;

TYPE
  Node* = POINTER TO NodeRec;
  NodeRec* = RECORD (pc.bext_rec)
    mode- : INTEGER;
    next- : Node;
    down- : Node;
    last  : Node;
    obj-  : pc.OBJECT;
    size- : LONGINT;  (* in words *)
    offs- : LONGINT;  (* in words *)
    bits- : LONGINT;  (* valid only inside word *)
  END;
  Word = ARRAY 32 OF Node;

VAR
  WORD_BITS     *: INTEGER; (* SL1 or BNRP word size *)

(*
PROCEDURE p_list(n: Node);
BEGIN
  env.info.print("-------\n");
  WHILE n#NIL DO
    IF n.obj#NIL THEN
      env.info.print("%-12s %2d %2d\n",n.obj.name^,n.offs,n.size);
    ELSE
      env.info.print("%-12s %2d %2d\n","***",n.offs,n.size);
    END;
    n:=n.next;
  END;
END p_list;
*)

PROCEDURE New(m: INTEGER): Node;
  VAR n: Node;
BEGIN
  NEW(n);
  n.mode:=m;
  n.next:=NIL;
  n.down:=NIL;
  n.last:=NIL;
  n.obj :=NIL;
  n.size:=-1;
  n.offs:=-1;
  n.bits:=-1;
  RETURN n;
END New;

PROCEDURE (n: Node) tie(x: Node);
BEGIN
  IF n.last=NIL THEN n.down:=x ELSE n.last.next:=x END;
  n.last:=x;
  x.next:=NIL;
END tie;

PROCEDURE ParseField(n,b: Node; f: pc.OBJECT);
BEGIN
  IF f.attr#NIL THEN
(* env.info.print("%-12s %d %d %d\n",f.name^,f.attr.val.get_integer(),
                                          f.attr.l.val.get_integer(),
                                          f.attr.l.l.val.get_integer());
*)
    ASSERT(f.flag IN pc.LangSet{pc.flag_sl1,pc.flag_bnrp});
    b.tie(New(m_sl1));
    b.last.obj:=f;
  ELSE
(* env.info.print("%-12s\n",f.name^);
*)
    n.tie(New(m_field));
    n.last.obj:=f;
  END;
END ParseField;

PROCEDURE ParseFlist(n,b: Node; f: pc.OBJECT);
  VAR l: pc.NODE;
BEGIN
  WHILE f#NIL DO
    IF f.mode IN pc.FIELDs THEN
      ParseField(n,b,f);
    ELSIF f.mode=pc.ob_header THEN
      IF f.val.obj#NIL THEN ParseField(n,b,f.val.obj) END;
      l:=f.val.l;
      n.tie(New(m_union));
      WHILE l#NIL DO
        ASSERT(l.mode=pc.nd_node);
        n.last.tie(New(m_struct));
        ParseFlist(n.last.last,b,l.obj);
	l:=l.next;
      END;
    ELSE
      ASSERT(FALSE);
    END;
    f:=f.next;
  END;
END ParseFlist;

PROCEDURE ParseType(n,b: Node; t: pc.STRUCT);
BEGIN
  IF t.base#NIL THEN ParseType(n,b,t.base) END;
  IF (t.base = NIL) & (t.prof = NIL) THEN (* empty record *)
    n.tie(New(m_dummy));
  ELSE
    ParseFlist(n,b,t.prof);
  END;
END ParseType;

PROCEDURE Size(f: pc.OBJECT): LONGINT;
  VAR w,o: LONGINT;
BEGIN
  IF f.flag=pc.flag_bnrp THEN WORD_BITS:=16 ELSE WORD_BITS:=32 END;
  w:=f.attr(pc.NODE).val.get_integer();
  IF w<WORD_BITS THEN
    o:=f.attr(pc.NODE).l.l.val.get_integer();
    ASSERT(o<16);
    IF f.type.mode=pc.ty_array THEN
      RETURN (o+f.type.len*w+15) DIV 16;
    ELSE
      ASSERT(o+w<=16);
      RETURN 1;
    END;
  ELSE
    ASSERT(w MOD WORD_BITS = 0);
    IF f.type.mode=pc.ty_array THEN
      RETURN f.type.len*w DIV WORD_BITS;
    ELSE
      RETURN w DIV WORD_BITS;
    END;
  END;
END Size;

PROCEDURE Bytes*(f: pc.OBJECT): LONGINT;
  VAR s: LONGINT;
BEGIN
  s:=Size(f)+f.attr(pc.NODE).l.val.get_integer();
  RETURN s*(WORD_BITS DIV 8);
END Bytes;

PROCEDURE TieSort(VAR l: Node; n: Node);
  VAR q: Node;
BEGIN
  IF l=NIL THEN l:=n; n.next:=NIL; RETURN END;
  IF l.offs>n.offs THEN n.next:=l; l:=n; RETURN END;
  q:=l;
  LOOP
    IF (q.next=NIL) OR (q.next.offs>n.offs) THEN
      n.next:=q.next;
      q.next:=n;
      RETURN;
    END;
    q:=q.next;
  END;
END TieSort;

PROCEDURE SortBitFields(b: Node): Node;
(* Creates sorted list of sl1 fields, moving them from b.down *)
  VAR n,m,l,q: Node;
BEGIN
  n:=b.down; l:=NIL; q:=NIL;
  WHILE n#NIL DO
    IF n.mode=m_sl1 THEN
      IF n=b.last THEN b.last:=l END;
      IF l=NIL THEN
        b.down:=n.next;
      ELSE
        l.next:=n.next;
      END;
      m:=n; n:=n.next;
      m.size:=Size(m.obj);
      m.offs:=m.obj.attr(pc.NODE).l.val.get_integer();
      TieSort(q,m);
    ELSE
      l:=n; n:=n.next;
    END;
  END;
  RETURN q;
END SortBitFields;

PROCEDURE Claster(VAR q: Node; VAR size: LONGINT): Node;
  VAR l,t: Node; offs: LONGINT;
BEGIN
  size:=0;
  IF q=NIL THEN RETURN NIL END;
  l:=q; t:=l; q:=l.next; l.next:=NIL;
  offs:=l.offs;
  size:=l.size;
  LOOP
    IF (q=NIL) OR (q.offs>=offs+size) THEN RETURN l END;
    IF offs+size<q.offs+q.size THEN
      size:=q.offs+q.size-offs;
    END;
    t.next:=q;
    q:=q.next;
    t:=t.next;
    t.next:=NIL;
  END;
END Claster;

PROCEDURE Packable(n,s: Node): BOOLEAN;
BEGIN
  IF n.mode#m_sl1 THEN RETURN FALSE END;
(*  IF n.offs=0 THEN n.mode:=m_field; RETURN FALSE END; *)
  WHILE s#NIL DO
    IF s.offs>=n.offs+n.size THEN RETURN TRUE END;
    IF n.offs<s.offs+s.size  THEN RETURN FALSE END;
    s:=s.next;
  END;
  RETURN TRUE;
END Packable;

PROCEDURE PackBitFields(VAR b: Node; offs: LONGINT): Node;
  VAR n,m,l,s,q: Node;
BEGIN
  n:=b; l:=NIL; q:=NIL;
  WHILE n#NIL DO
    IF Packable(n,q) THEN
      IF l=NIL THEN b:=n.next ELSE l.next:=n.next END;
      m:=n; n:=n.next;
      m.mode:=m_field;
      TieSort(q,m);
    ELSE
      l:=n; n:=n.next;
    END;
  END;
  IF q=NIL THEN RETURN NIL END;
  s:=New(m_struct);
  s.offs:=offs;
  WHILE q#NIL DO
    n:=q; q:=q.next;
    ASSERT(n.offs>=0);
    ASSERT(n.size>=0);
    IF n.offs>offs THEN
      s.tie(New(m_filler));
      s.last.size:=n.offs-offs;
      s.last.offs:=offs;
      offs:=n.offs;
    END;
    ASSERT(n.offs=offs);
    s.tie(n);
    offs:=offs+n.size;
  END;
  s.size:=offs-s.offs;
  RETURN s;
END PackBitFields;

PROCEDURE Parse(t: pc.STRUCT): Node;
  VAR n,m,b,s,i,j,k: Node; offs,size: LONGINT;
BEGIN
  ASSERT(t.mode=pc.ty_record);
  n:=New(m_struct); (* normal fields *)
  n.offs:=0;
  b:=New(m_union);  (* SL1 fields    *)
  b.offs:=0;
  ParseType(n,b,t);
  m:=SortBitFields(b);
  b.tie(n);
  IF m=NIL THEN RETURN b END;
  i:=New(m_struct); (* all clasters  *)
  i.offs:=0;
  b.tie(i);
  offs:=0;
  LOOP
    j:=Claster(m,size);
    IF j=NIL THEN EXIT END;
    k:=New(m_union); (* claster *)
    k.offs:=j.offs;
    ASSERT(k.offs>=0);
    k.size:=size;
    IF k.offs>offs THEN
      i.tie(New(m_filler));
      i.last.size:=k.offs-offs;
      i.last.offs:=offs;
    END;
    LOOP
      s:=PackBitFields(j,k.offs);
      IF s=NIL THEN EXIT END;
      ASSERT(s.offs=k.offs);
      k.tie(s);
    END;
    i.tie(k);
    offs:=k.offs+k.size;
  END;
  RETURN b;
END Parse;

PROCEDURE Optimize(n: Node);
  VAR l,h: Node;
BEGIN
  l:=n.down; h:=NIL;
  WHILE l#NIL DO
    Optimize(l);
    IF (l.mode IN {m_struct,m_union}) & (l.down=NIL) THEN
      (* remove empty struct or union *)
      IF l=n.last THEN n.last:=h END;
      IF h=NIL THEN n.down:=l.next ELSE h.next:=l.next END;
      l:=l.next;
    ELSIF (l.mode IN {m_struct,m_union}) & (l.mode=n.mode) OR
          (n.mode=m_union) & (l.mode=m_struct) & (l.down=l.last) &
          ((l.down=NIL) OR (l.down.mode#m_word))
    THEN
      (* open struct or union *)
      l.last.next:=l.next;
      IF n.last=l THEN n.last:=l.last END;
      IF h=NIL THEN n.down:=l.down ELSE h.next:=l.down END;
      l:=l.down;
    ELSE
      h:=l;
      l:=l.next;
    END;
  END;
  WHILE (n.mode IN {m_struct,m_union}) &
        (n.down#NIL) &
        (n.down.mode IN {m_struct,m_union}) &
        (n.down.next=NIL)
  DO
    l:=n.down;
    n.mode:=l.mode;
    n.down:=l.down;
    n.last:=l.last;
  END;
  IF    (n.mode=m_union) &
        (n.down#NIL) &
        (n.down.next=NIL)
  THEN
    n.mode:=m_struct;
  END;
END Optimize;

PROCEDURE CreWord(u: Node; w: Word);
  VAR v,l: Node; i,o: LONGINT;
BEGIN
  ASSERT(u.offs>=0);
  ASSERT(u.mode=m_union);
  v:=New(m_word);
  u.tie(New(m_struct));
  u.last.offs:=u.offs;
  u.last.size:=1;
  u.last.tie(v);
  v.size:=1;
  v.offs:=u.offs;
  i:=WORD_BITS-1;
  LOOP
    o:=i;
    WHILE (i>=0) & (w[i]=NIL) DO DEC(i) END;
    IF o#i THEN
      l:=New(m_filler);
      l.size:=1;
      l.bits:=o-i;
      l.offs:=u.offs;
      v.tie(l);
      IF i<0 THEN EXIT END;
    END;
    o:=i; l:=w[i];
    WHILE (i>=0) & (w[i]=l) DO DEC(i) END;
    v.tie(l);
    l.bits:=o-i;
    ASSERT(l.offs=v.offs);
    IF i<0 THEN EXIT END;
  END;
END CreWord;

PROCEDURE GetWord(u: Node; VAR w: Word): BOOLEAN;
  VAR p,l: Node; i,o,s: LONGINT; ok: BOOLEAN; busy: SET;
BEGIN
  busy:={};
  FOR i:=0 TO 31 DO w[i]:=NIL END;
  IF u.mode#m_union THEN RETURN FALSE END;
  p:=NIL; l:=u.down; ok:=FALSE;
  WHILE l#NIL DO
    IF (l.mode=m_field) & (l.size=1) &
       (l.obj.attr#NIL) & l.obj.type.is_ordinal()
    THEN
      o:=l.obj.attr(pc.NODE).l.l.val.get_integer();
      s:=l.obj.attr(pc.NODE).val.get_integer();
      IF (s<WORD_BITS) & (busy*{o..o+s-1}={}) THEN
        busy:=busy+{o..o+s-1};
        FOR i:=o TO o+s-1 DO w[i]:=l END;
        ok:=TRUE;
        IF p=NIL THEN u.down:=l.next;
        ELSE p.next:=l.next;
        END;
        IF l=u.last THEN u.last:=p END;
      ELSE
        p:=l;
      END;
    ELSE
      p:=l;
    END;
    l:=l.next;
  END;
  RETURN ok;
END GetWord;

PROCEDURE MakeWords(n: Node);
  VAR w: Word; o,s: LONGINT; l: Node;
BEGIN
  l:=n.down;
  WHILE l#NIL DO
    IF (l.mode=m_field) & (l.size=1) &
       (l.obj.attr#NIL) & (n.mode=m_struct) &
       l.obj.type.is_ordinal()
    THEN
      o:=l.obj.attr(pc.NODE).l.l.val.get_integer();
      s:=l.obj.attr(pc.NODE).val.get_integer();
      IF s<WORD_BITS THEN
        IF o+s<WORD_BITS THEN
          l.tie(New(m_filler));
          l.last.size:=1;
          l.last.offs:=l.offs;
          l.last.bits:=WORD_BITS-o-s;
        END;
        l.mode:=m_word;
        l.tie(New(m_field));
        l.last.obj:=l.obj; l.obj:=NIL;
        l.last.size:=l.size;
        l.last.offs:=l.offs;
        l.last.bits:=s;
        IF o>0 THEN
          l.tie(New(m_filler));
          l.last.size:=1;
          l.last.offs:=l.offs;
          l.last.bits:=o;
        END;
      END;
    ELSIF l.mode IN {m_struct,m_union} THEN
      MakeWords(l);
    END;
    l:=l.next;
  END;
  IF n.mode=m_union THEN
    WHILE GetWord(n,w) DO CreWord(n,w) END;
  END;
END MakeWords;

PROCEDURE Record*(t: pc.STRUCT);
  VAR n: Node; h: nms.TREE;
BEGIN
  IF t.ext=NIL THEN
    nms.new_tree(h); t.ext:=h;
  ELSE
    h:=t.ext(nms.TREE);
  END;
  IF h.node#NIL THEN RETURN END;
  n:=Parse(t);
  Optimize(n);
  IF pc.mods[t.mno].type.flag IN pc.LangSet{pc.flag_bnrp,pc.flag_sl1} THEN
    MakeWords(n);
    Optimize(n);
  END;
  IF n.mode=m_union THEN INCL(t.tags,cc.ttag_union) END;
  h.node:=n;
END Record;

BEGIN
  WORD_BITS:=0;
END ccRec.

