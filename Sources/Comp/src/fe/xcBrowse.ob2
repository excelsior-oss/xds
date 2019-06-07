(* Copyright (c) 1994,97 XDS Ltd, Russia. All Rights Reserved. *)
MODULE xcBrowse; (* Sem Apr 1994 *)

(* Modifications:
   16-May-96 Ned   PRO0124: ttag_ & otag_ -> tmark & omark  
*)


IMPORT pc:=pcK, env:=xiEnv, xfs:=xiFiles, fmt:=xcStr, SYSTEM;

CONST
  (* Style *)
  st_body = 0;
  st_doc  = 1;
  st_def  = 2;

  equ = " = ";
  range = "..";

TYPE
  Buffer = ARRAY 256 OF CHAR;

VAR
  style   : INTEGER;
  closure : BOOLEAN; (* show inherited methods in xref  *)
  redefine: BOOLEAN; (* show redefined methods in xref  *)
  alpha   : BOOLEAN; (* show all objects in alpha order *)

  out     : xfs.TextFile;
  dst_pos : LONGINT;

CONST
  omark_vis         = pc.omark_aux28; (* already inserted into object list *)
  line_diff         = 1000H;

TYPE
  LIST     = POINTER TO LIST_REC;
  LIST_REC = RECORD
    obj : pc.OBJECT;
    next: LIST;
  END;
  INSERT   = PROCEDURE (VAR l: LIST; o: pc.OBJECT);

TYPE
  OrderType = pc.OB_MODE;
VAR
  order: ARRAY OrderType OF INTEGER;

  bump   : Buffer;
  bpos   : INTEGER;
  cur_mod: pc.OBJECT;

(*----------------------  Output file  ---------------------------*)

PROCEDURE create_out(name-: ARRAY OF CHAR);
  VAR fn,ext: pc.STRING;
BEGIN
  env.config.Equation("BSDEF",ext);
  xfs.sys.Create('',name,ext^,fn);
  xfs.sys.UseFirst(fn^,fn);
  xfs.text.Open(fn^,TRUE);
  IF xfs.text.file=NIL THEN
    env.errors.Fault(env.null_pos,424,xfs.text.msg^);
  END;
  out:=xfs.text.file(xfs.TextFile);
  dst_pos:=0;
  bpos:=0;
END create_out;

PROCEDURE close_out;
  VAR err: pc.STRING;
BEGIN
  out.CloseNew(env.errors.err_cnt=0,FALSE,FALSE,err);
  IF err#NIL THEN
    env.errors.Fault(env.null_pos,432,err^);
  ELSE
    env.info.newDF:=TRUE;
  END;
  out:=NIL;
END close_out;

PROCEDURE wl;
BEGIN
  bump[bpos]:=0X;
  out.print("%s\n",bump);
  bpos:=0; dst_pos:=0;
END wl;

PROCEDURE wc(c: CHAR);
BEGIN
  IF bpos=LEN(bump) THEN
    out.print("%s",bump);
    bpos:=0;
  END;
  bump[bpos]:=c; INC(bpos); INC(dst_pos);
END wc;

PROCEDURE skip(n: LONGINT);
BEGIN
  WHILE n>0 DO wc(' '); DEC(n) END;
END skip;

PROCEDURE ws(s-: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (i<LEN(s)) & (s[i]#0X) DO wc(s[i]); INC(i) END;
END ws;

PROCEDURE wi(f-: ARRAY OF CHAR; x: LONGINT);
  VAR s: Buffer;
BEGIN
  fmt.prn_bin(s,f,x); ws(s);
END wi;

PROCEDURE wr(f-: ARRAY OF CHAR; x: LONGREAL);
  VAR s: Buffer;
BEGIN
  fmt.prn_bin(s,f,x); ws(s);
END wr;

(*----------------------  Objects gathering  ---------------------*)

PROCEDURE get_ps(ps: pc.TPOS): LONGINT;
  VAR l,c: LONGINT; fnm: env.String;
BEGIN
  ps.unpack(fnm,l,c);
  RETURN l*line_diff+c;
END get_ps;

PROCEDURE valid_name(nm: pc.STRING): BOOLEAN;
BEGIN
  RETURN (nm#NIL) & (nm[0]>='A');
END valid_name;

PROCEDURE alpha_insert(VAR list: LIST; o: pc.OBJECT);
  VAR l,p,i: LIST; ord: INTEGER;
BEGIN
  ord:=order[o.mode];
  l:=list; p:=NIL;
  WHILE (l#NIL) & (order[l.obj.mode] < ord) DO p:=l; l:=l.next END;
  WHILE (l#NIL) & (order[l.obj.mode] = ord) & (l.obj.name^ < o.name^) DO
    p:=l; l:=l.next
  END;
  NEW(i);
  i.obj:=o;
  IF p=NIL THEN i.next:=list; list:=i
  ELSE i.next:=l; p.next:=i;
  END;
END alpha_insert;

PROCEDURE pos_insert(VAR list: LIST; o: pc.OBJECT);
  VAR l,p,i: LIST;
BEGIN
  l:=list; p:=NIL;
  IF o.mode#pc.ob_module THEN
    WHILE (l#NIL) & ((l.obj.mode=pc.ob_module) OR (l.obj.pos.cmp(o.pos)<0)) DO
      p:=l; l:=l.next
    END;
  END;
  NEW(i);
  i.obj:=o;
  IF p=NIL THEN i.next:=list; list:=i
  ELSE i.next:=l; p.next:=i;
  END;
END pos_insert;

PROCEDURE mark_objects(mod: pc.OBJECT;
                    insert: INSERT;
                  VAR list: LIST);

  CONST
    tmark_def = pc.tmark_aux31;

  TYPE
    TLIST     = POINTER TO TLIST_REC;
    TLIST_REC = RECORD
      type: pc.STRUCT;
      next: TLIST;
    END;

  VAR
    tlist: TLIST;

  PROCEDURE ^ mark_type_name(t: pc.STRUCT);

  PROCEDURE mark_type_def(t: pc.STRUCT);
    VAR o: pc.OBJECT; tl: TLIST;
  BEGIN
    ASSERT(t#NIL);
    IF tmark_def IN t.marks THEN RETURN END;
    INCL(t.marks,tmark_def);
    NEW(tl); tl.type:=t; tl.next:=tlist; tlist:=tl;
    IF t.base#NIL THEN mark_type_name(t.base) END;
    IF t.inx #NIL THEN mark_type_name(t.inx) END;
    IF t.mode=pc.ty_record THEN
      (* заносим в список методы *)
      o:=t.mem;
      WHILE o#NIL DO
        ASSERT(o.mode=pc.ob_xproc);
        IF pc.otag_public_f IN o.tags THEN
          mark_type_def(o.type);
          IF NOT (omark_vis IN o.marks) THEN
            insert(list,o); INCL(o.marks,omark_vis);
          END;
        END;
        o:=o.next;
      END;
      (* помечаем поля *)
      o:=t.prof;
      WHILE o#NIL DO
        IF (style=st_body) OR (pc.otag_public_f IN o.tags) THEN
          INCL(o.marks,omark_vis); (* mark visible fields *)
          mark_type_name(o.type);
        END;
        o:=o.next;
      END;
    ELSIF t.mode=pc.ty_enum THEN
      (* nothing *)
    ELSE
      o:=t.prof;
      WHILE o#NIL DO mark_type_name(o.type); o:=o.next END;
    END;
  END mark_type_def;

  PROCEDURE mark_type_name(t: pc.STRUCT);
  BEGIN
    IF t=NIL THEN RETURN END;
    IF (t.obj#NIL) & valid_name(t.obj.name) THEN
      (* named type *)
      IF t.obj.mno=cur_mod.mno THEN
        IF NOT (omark_vis IN t.obj.marks) THEN
          insert(list,t.obj);
          INCL(t.obj.marks,omark_vis);
        END;
      ELSIF t.obj.mno>=pc.ZEROMno THEN
        IF NOT (omark_vis IN pc.mods[t.obj.mno].marks) THEN
          insert(list,pc.mods[t.obj.mno]);
          INCL(pc.mods[t.obj.mno].marks,omark_vis);
        END;
      END;
    ELSE
      (* anonymous type *)
      mark_type_def(t);
    END;
  END mark_type_name;

  PROCEDURE mark(o: pc.OBJECT; obj: BOOLEAN);
  BEGIN
    IF obj THEN
      IF NOT (omark_vis IN o.marks) & valid_name(o.name) THEN
        insert(list,o); INCL(o.marks,omark_vis);
      END;
    ELSIF o.mode IN pc.PROCs THEN
      mark_type_def(o.type)
    ELSIF (o.mode=pc.ob_type) & (o.type.obj=o) THEN
      mark_type_def(o.type)
    ELSE
      mark_type_name(o.type)
    END;
  END mark;

  PROCEDURE mark_list(o: pc.OBJECT; obj: BOOLEAN);
  BEGIN
    WHILE o#NIL DO mark(o,obj); o:=o.next END;
  END mark_list;

BEGIN
  list:=NIL; tlist:=NIL;
  mark_list(mod.type.prof,FALSE);
  mark_list(mod.type.prof,TRUE);
  WHILE tlist#NIL DO EXCL(tlist.type.marks,tmark_def); tlist:=tlist.next END;
END mark_objects;

(*-------------------------  Text output  ------------------------*)

CONST
  (* profile_mode *)
  profile_type   = 0;
  profile_proc   = 1;
  profile_method = 2;

VAR
  comment_fst: pc.Comment;

PROCEDURE get_ln(ps: LONGINT): LONGINT;
(* get begining of line *)
BEGIN
  RETURN ps - ps MOD line_diff;
END get_ln;

PROCEDURE get_nl(ps: LONGINT): LONGINT;
(* get begining of next line *)
BEGIN
  RETURN ps - ps MOD line_diff + line_diff;
END get_nl;

PROCEDURE get_le(ps: LONGINT): LONGINT;
(* get end of line *)
BEGIN
  RETURN ps - ps MOD line_diff + (line_diff - 1);
END get_le;

PROCEDURE get_ne(ps: LONGINT): LONGINT;
(* get end of next line *)
BEGIN
  RETURN ps - ps MOD line_diff + (line_diff*2 - 1);
END get_ne;

PROCEDURE next_comment(p: pc.Comment): pc.Comment;
BEGIN
  IF p=NIL THEN RETURN NIL END;
  p:=p.next;
  WHILE p.pos.IsNull() DO p:=p.next END;
  IF p=comment_fst THEN RETURN NIL END;
  RETURN p;
END next_comment;

PROCEDURE bind_down(f: pc.Comment; fr,to: LONGINT): BOOLEAN;
  VAR p: pc.Comment;
BEGIN
  IF f=NIL THEN RETURN FALSE END;
  IF get_ln(get_ps(f.pos))<=fr THEN RETURN FALSE END;
  IF to<=get_ne(get_ps(f.end)) THEN RETURN TRUE END;
  p:=next_comment(f);
  IF p=NIL THEN RETURN FALSE END;
  IF get_ps(p.pos)>to THEN RETURN FALSE END;
  IF get_ps(p.pos)<=get_ne(get_ps(f.end)) THEN RETURN bind_down(p,fr,to) END;
  RETURN FALSE;
END bind_down;

PROCEDURE out_comment(VAR fr: LONGINT; to: LONGINT;
                      dso: INTEGER; nl: BOOLEAN; sep: ARRAY OF CHAR);
        (* dso - destination/source offset *)
  VAR
    ptr: pc.Comment;
    ofs: INTEGER;
    mag: INTEGER;

  PROCEDURE get_comm_char(): CHAR;
  BEGIN
    IF ptr=NIL THEN RETURN 0X END;
    WHILE ptr.str^[ofs]=0X DO
      ptr:=ptr.next;
      IF NOT ptr.pos.IsNull() THEN
        IF ptr=comment_fst THEN ptr:=NIL END;
        RETURN 0X;
      END;
      ofs:=0;
    END;
    IF mag>0 THEN
      DEC(mag);
      IF (mag=0) & (ptr.str^[ofs]#'*') THEN RETURN '*' END;
    END;
    INC(ofs);
    RETURN ptr.str^[ofs-1];
  END get_comm_char;

  PROCEDURE out_com();
    VAR ch: CHAR; str: Buffer; i,j,l: LONGINT; fnm: env.String;
  BEGIN
    IF get_ln(get_ps(ptr.pos))<=fr THEN
      IF dst_pos#0 THEN wc(" ") END;
    ELSE
      IF dst_pos#0 THEN wl END;
      IF get_ps(ptr.pos)>get_ne(fr) THEN wl END;
    END;
    ptr.pos.unpack(fnm,i,j);
    l:=j+dso-dst_pos;
    IF l<0 THEN l:=0 END;
    skip(l);
    fr:=get_ps(ptr.end)+1;
    ofs:=0;
    mag:=3;
    i:=0;
    LOOP
      ch:=get_comm_char();
      IF ch=0X THEN EXIT END;
      IF ch=pc.EOL THEN
        str[i]:=0X;
        ws(str); wl;
        skip(dso);
        i:=0;
      ELSIF ch>=' ' THEN
        str[i]:=ch; INC(i);
        IF i=LEN(str)-1 THEN str[i]:=0X; ws(str); i:=0 END;
      END;
    END;
    str[i]:=0X;
    ws(str);
  END out_com;

BEGIN
  ptr:=comment_fst;
  WHILE (ptr#NIL) & (get_ps(ptr.pos)<fr) DO ptr:=next_comment(ptr) END;
  WHILE (ptr#NIL) & (get_ps(ptr.pos)<to) & NOT bind_down(ptr,fr,to) DO out_com() END;
  IF dst_pos#0 THEN wl END;
  IF nl THEN wl END;
  IF sep[0]#0X THEN ws(sep) END;
  WHILE (ptr#NIL) & (get_ps(ptr.pos)<to) DO out_com() END;
  IF dst_pos#0 THEN wl END;
  IF fr<to THEN fr:=to END;
END out_comment;

PROCEDURE get_bind_down_comment(ps: pc.TPOS): LONGINT;
  VAR p: pc.Comment;
BEGIN
  p:=comment_fst;
  LOOP
    IF p=NIL THEN RETURN get_ps(ps) END;
    IF p.pos.gtr(ps) THEN RETURN get_ps(ps) END;
    IF bind_down(p,0,get_ps(ps)) THEN RETURN get_ps(p.pos) END;
    p:=next_comment(p);
  END;
END get_bind_down_comment;

PROCEDURE get_bind_up_comment(ps: LONGINT): LONGINT;
  VAR p: pc.Comment;
BEGIN
  p:=comment_fst;
  WHILE (p#NIL) & (get_ps(p.pos)<ps) DO p:=next_comment(p) END;
  LOOP
    IF p=NIL THEN RETURN ps END;
    IF get_ps(p.pos)>get_ne(ps) THEN RETURN ps END;
    ps:=get_ps(p.end);
    p:=next_comment(p);
  END;
END get_bind_up_comment;

(*--------------------------  make def  --------------------------*)

PROCEDURE ^ out_profile(VAR ps: LONGINT;
                        t: pc.STRUCT; pm: INTEGER;
                        coms: BOOLEAN; lev,dso: INTEGER);

PROCEDURE ^ out_type_ref(VAR ps: LONGINT; t: pc.STRUCT;
                        lev,dso: INTEGER);

PROCEDURE ^ out_type_name(t: pc.STRUCT);

PROCEDURE ^ out_method(VAR ps: LONGINT; o: pc.OBJECT; redef: BOOLEAN;
                        coms: BOOLEAN; lev,dso: INTEGER);

PROCEDURE ^ out_value(v: pc.VALUE; t: pc.STRUCT);
PROCEDURE ^ out_value_ordinal(v: pc.VALUE; t: pc.STRUCT);

(*----------------------------------------------------------------*)

PROCEDURE out_sysflag(t: pc.STRUCT);
CONST
  s1 = pc.TY_SET{pc.ty_pointer,pc.ty_opaque,pc.ty_record, pc.ty_array, pc.ty_array_of};
  s2 = pc.TY_SET{pc.ty_pointer,pc.ty_opaque,pc.ty_record, pc.ty_array, pc.ty_array_of, pc.ty_proctype};
BEGIN
  CASE t.flag OF
    |pc.flag_o2:
    |pc.flag_m2:
      IF (t.mode IN s1) OR (t.mode = pc.ty_module) THEN
        ws("[Modula] ");
      END;
    |pc.flag_c :
      IF (t.mode IN s2) OR (t.mode = pc.ty_module) THEN
        ws("[C] ");
      END;
    |pc.flag_p :
      IF (t.mode IN s2) OR (t.mode = pc.ty_module) THEN
        ws("[Pascal] ");
      END;
    |pc.flag_stdcall :
      IF (t.mode IN s2) OR (t.mode = pc.ty_module) THEN
        ws("[StdCall] ");
      END;
    |pc.flag_vmcall :
      IF (t.mode IN s2) OR (t.mode = pc.ty_module) THEN
        ws("[VMCall] ");
      END;
    |pc.flag_lightcall :
      IF (t.mode IN s2) OR (t.mode = pc.ty_module) THEN
        ws("[LightCall] ");
      END;
    |pc.flag_javacall :
      IF (t.mode IN s2) OR (t.mode = pc.ty_module) THEN
        ws("[JavaCall] ");
      END;
    |pc.flag_syscall :
      IF (t.mode IN s2) OR (t.mode = pc.ty_module) THEN
        ws("[SysCall] ");
      END;
  ELSE
    wi("[%d] ",ORD(t.flag));
  END;
END out_sysflag;

PROCEDURE strcat(VAR d: ARRAY OF CHAR; s-: ARRAY OF CHAR);
  VAR i,j: INTEGER;
BEGIN
  i:=SHORT(LENGTH(d)); j:=0;
  WHILE (i<LEN(d)-1) & (s[j]#0X) DO d[i]:=s[j]; INC(i); INC(j) END;
  d[i]:=0X;
END strcat;

PROCEDURE str_object_name(VAR s: ARRAY OF CHAR; o: pc.OBJECT; marks: BOOLEAN);
BEGIN
  IF o=NIL THEN COPY("?OBJECT=NIL",s); RETURN END;
  IF (o.host#NIL) & (o.host.mode=pc.ty_module) & (o.mno#cur_mod.mno) THEN
    IF o.mno>=pc.ZEROMno THEN
      COPY(pc.mods[o.mno].name^,s); strcat(s,".");
    ELSIF valid_name(pc.sys_mods[SYSTEM.PRED(-o.mno)].name) THEN
      COPY(pc.sys_mods[SYSTEM.PRED(-o.mno)].name^,s); strcat(s,".");
    ELSE s[0]:=0X;
    END;
  ELSE s[0]:=0X;
  END;
  strcat(s,o.name^);
  IF marks THEN
    ASSERT((o.mno=cur_mod.mno) OR (o.host.mode=pc.ty_record));
    IF pc.otag_RO_public IN o.tags THEN strcat(s,"-")
    ELSIF pc.otag_public IN o.tags THEN strcat(s,"*")
    ELSIF pc.otag_public_f IN o.tags THEN strcat(s,"*");
    END;
  END;
END str_object_name;

PROCEDURE out_object_name(o: pc.OBJECT; marks: BOOLEAN);
  VAR nm: Buffer;
BEGIN
  str_object_name(nm,o,marks); ws(nm);
END out_object_name;

PROCEDURE out_object_name_sz(o: pc.OBJECT; marks: BOOLEAN; sz: INTEGER);
  VAR nm: Buffer; len: LONGINT;
BEGIN
  str_object_name(nm,o,marks);
  ws(nm);
  len:=LENGTH(nm);
  skip(sz-len);
END out_object_name_sz;

PROCEDURE out_prm_name(o: pc.OBJECT);
BEGIN
  ws(o.name^);
  IF (style=st_body) & (pc.otag_RO IN o.tags) THEN wc("-") END;
END out_prm_name;

PROCEDURE out_type_name(t: pc.STRUCT);
(* выводит имя типа, если имени нет то какую-нибудь зюку *)
BEGIN
  ASSERT(t#NIL);
  CASE t.mode OF
    |pc.ty_shortint: ws("SHORTINT");
    |pc.ty_integer : ws("INTEGER");
    |pc.ty_longint : ws("LONGINT");
    |pc.ty_boolean : ws("BOOLEAN");
    |pc.ty_real    : ws("REAL");
    |pc.ty_longreal: ws("LONGREAL");
    |pc.ty_complex : ws("COMPLEX");
    |pc.ty_lcomplex: ws("LONGCOMPLEX");
    |pc.ty_char    : ws("CHAR");
  ELSE
    IF (t.mode=pc.ty_set) & (t.mno<pc.ZEROMno) & (t.len=32) THEN
      ws("SET");
    ELSIF (t.obj#NIL) & valid_name(t.obj.name) THEN
      out_object_name(t.obj,FALSE);
    ELSE
      ws("?TYPE?");
    END;
  END;
END out_type_name;

PROCEDURE out_methods_xref(rec: pc.STRUCT; coms: BOOLEAN; lev,dso: INTEGER);

  PROCEDURE is_vis(m: pc.OBJECT): BOOLEAN;
  (* m - is visible method *)
  BEGIN
    IF NOT (pc.otag_public_f IN m.tags) THEN RETURN FALSE END;
    IF closure THEN
      RETURN redefine OR (m.type.inx=NIL);
    ELSE
      RETURN m.host=rec;
    END;
  END is_vis;

  VAR m: pc.OBJECT; r: pc.STRUCT; list: LIST; ps: LONGINT; insert: INSERT;
BEGIN
  ASSERT(rec.mode=pc.ty_record);
  IF alpha THEN insert:=alpha_insert ELSE insert:=pos_insert END;
  m:=rec.mem; list:=NIL;
  WHILE m#NIL DO
    IF is_vis(m) THEN insert(list,m) END;
    m:=m.next;
  END;
  IF closure THEN
    r:=rec.base;
    WHILE r#NIL DO
      m:=r.mem; r:=r.base;
      WHILE m#NIL DO
        IF is_vis(m) THEN insert(list,m) END;
        m:=m.next;
      END;
    END;
  END;
  WHILE list#NIL DO
    m:=list.obj;
    IF m.mno#cur_mod.mno THEN
      (* external object - no comments *)
      ps:=0;
      out_method(ps,m,m.type.inx#NIL,FALSE,lev,dso);
      wl;
    ELSE
      ps:=get_bind_down_comment(m.pos);
      out_method(ps,m,m.type.inx#NIL,coms,lev,dso);
      IF coms THEN out_comment(ps,get_bind_up_comment(ps),dso,FALSE,"")
      ELSE wl
      END;
    END;
    list:=list.next;
  END;
END out_methods_xref;

PROCEDURE out_type_record(VAR ps: LONGINT; t: pc.STRUCT; lev,dso: INTEGER);

  PROCEDURE with_next(l: pc.OBJECT): BOOLEAN;
  BEGIN
    RETURN (l.next#NIL)
         & (omark_vis IN l.next.marks)
         & (l.next.type=l.type)
         & (get_ps(l.pos)#get_ps(l.next.pos))  (* pos is defined *)
         & (get_ln(get_ps(l.next.pos))=get_ln(get_ps(l.pos)));
          (* the same line *)
  END with_next;

  VAR
    l   : pc.OBJECT;
    m,sz: INTEGER;
    nm  : Buffer;
BEGIN
  ws("RECORD "); out_sysflag(t);
  IF t.base#NIL THEN
    ASSERT(t.base.obj#NIL);
    wc("("); out_type_ref(ps,t.base,lev,dso); wc(")");
  END;

  (* вычислим максимальную длину имени поля *)
  l:=t.prof; m:=0;
  WHILE l#NIL DO
    IF omark_vis IN l.marks THEN
      str_object_name(nm,l,TRUE);
      sz:=SHORT(LENGTH(nm));
      WHILE with_next(l) DO
        l:=l.next;
        str_object_name(nm,l,TRUE);
        sz:=sz+SHORT(LENGTH(nm))+1;
      END;
      IF m<sz THEN m:=sz END;
    END;
    l:=l.next;
  END;

  (* field list *)
  l:=t.prof;
  WHILE l#NIL DO
    IF omark_vis IN l.marks THEN
      out_comment(ps,get_ps(l.pos),dso,FALSE,'');
      skip(lev+2);
      str_object_name(nm,l,TRUE);
      ws(nm);
      sz:=SHORT(LENGTH(nm));
      WHILE with_next(l) DO
        EXCL(l.marks,omark_vis);
        l:=l.next;
        ASSERT(omark_vis IN l.marks);
        str_object_name(nm,l,TRUE);
        wc(','); ws(nm);
        sz:=sz+SHORT(LENGTH(nm))+1;
      END;
      skip(m-sz); wc(':'); wc(' ');
      out_type_ref(ps,l.type,lev+4,dso);
      wc(";");
      EXCL(l.marks,omark_vis);
    END;
    l:=l.next;
  END;
  out_comment(ps,get_ps(t.end),dso,FALSE,'');

  (* выведем список методов *)
  IF style#st_body THEN
    out_methods_xref(t,style=st_def,lev+2,lev+2+dso);
  END;
  skip(lev);
  ws("END");
END out_type_record;

PROCEDURE out_type_enum(t: pc.STRUCT; lev,dso: INTEGER);

  PROCEDURE insert_enumval(o: pc.OBJECT);
    VAR l,p: pc.OBJECT; x: LONGINT;
  BEGIN
    l:=o.type.prof; p:=NIL; x:=o.val.val.get_integer();
    WHILE (l#NIL) & (l.val.val.get_integer() < x) DO p:=l; l:=l.next END;
    o.next:=l;
    IF p=NIL THEN o.type.prof:=o ELSE p.next:=o END;
  END insert_enumval;

  VAR o,n: pc.OBJECT;
BEGIN
  o:=t.prof; t.prof:=NIL;
  WHILE o#NIL DO n:=o.next; insert_enumval(o); o:=n END;
  o:=t.prof;
  wc("(");
  LOOP
    ws(o.name^);
    o:=o.next;
    IF o=NIL THEN EXIT END;
    wc(',');
    IF dst_pos+LENGTH(o.name^)>71 THEN wl; skip(lev+dso+2);
    ELSE wc(" ");
    END;
  END;
  wc(")");
END out_type_enum;

PROCEDURE out_type_def(VAR ps: LONGINT; t: pc.STRUCT; lev,dso: INTEGER);
(* type definition *)
BEGIN
  CASE t.mode OF
    |pc.ty_range    : IF NOT (t.base.mode IN pc.WHOLEs) THEN
                        out_type_name(t.base);
                      END;
                      wc("[");   out_value_ordinal(t.min,t.base);
                      ws(range); out_value_ordinal(t.max,t.base);
                      wc("]");
    |pc.ty_enum     : out_type_enum(t,lev,dso);
    |pc.ty_pointer  : ws("POINTER "); out_sysflag(t); ws("TO ");
                      out_type_ref(ps,t.base,lev,dso);
    |pc.ty_set      : ws("SET OF "); out_type_ref(ps,t.base,lev,dso);
    |pc.ty_proctype : ws("PROCEDURE "); out_sysflag(t);
                      out_profile(ps,t,profile_type,TRUE,lev,dso);
    |pc.ty_array    : ws("ARRAY "); out_type_ref(ps,t.inx,lev,dso);
                      ws(" OF "); out_type_ref(ps,t.base,lev,dso);
    |pc.ty_array_of : ws("ARRAY OF "); out_type_ref(ps,t.base,lev,dso);
    |pc.ty_record   : out_type_record(ps,t,lev,dso);
  ELSE
    ws("???"); wi("%d",ORD(t.mode));
  END;
END out_type_def;

PROCEDURE out_type_ref(VAR ps: LONGINT; t: pc.STRUCT; lev,dso: INTEGER);
(* выводит ссылку на тип, т.е. его имя (если оно есть) или его описание *)
BEGIN
  IF (t.obj#NIL) & valid_name(t.obj.name) THEN
    out_type_name(t);
  ELSE
    IF get_ps(t.pos)>=get_nl(ps) THEN
      INC(lev,2);
      out_comment(ps,get_ps(t.pos),dso,FALSE,"");
      skip(lev);
    END;
    out_type_def(ps,t,lev,dso);
  END;
END out_type_ref;

PROCEDURE out_profile(VAR tpos: LONGINT;
        t: pc.STRUCT; pm: INTEGER;
        coms: BOOLEAN; lev,dso: INTEGER);
(* tpos - current source text position       *)
(* dso  - position difference between destination and source text *)
(* lev  - syntax hilighting level                                 *)
  VAR o: pc.OBJECT; ot: pc.STRUCT; ps: LONGINT; fnm: env.String; l,c: LONGINT;
BEGIN
  o:=t.prof; (* o - first argument *)
  IF pm=profile_method THEN o:=o.next END;
  IF (o=NIL) & (t.base.mode=pc.ty_void) THEN RETURN END;
  ps:=get_ln(tpos);
  IF NOT t.pos.IsNull() THEN ps:=get_ln(get_ps(t.pos)) END;
  wc("(");
  WHILE o#NIL DO
    IF ps#get_ln(get_ps(o.pos)) THEN
      IF coms THEN out_comment(tpos,get_ps(o.pos),dso,FALSE,'');
      ELSE wl; tpos:=get_ps(o.pos);
      END;
      o.pos.unpack(fnm,l,c);
      ps:=c + dso;
      IF (o.mode=pc.ob_varpar) OR (o.mode=pc.ob_seq) THEN DEC(ps,4) END;
      skip(ps);
      ps:=get_ln(get_ps(o.pos));
    END;
    ot:=o.type;
    IF o.mode=pc.ob_varpar THEN ws("VAR ");
    ELSIF o.mode=pc.ob_seq THEN ws("SEQ "); ot:=ot.base;
    END;
    IF pm#profile_type THEN
      LOOP
        out_prm_name(o);
        IF (o.next=NIL) OR
           (o.next.type#o.type) OR
           (o.next.mode#o.mode) OR
           (ps#get_ln(get_ps(o.next.pos))) THEN EXIT END;
        wc(",");
        o:=o.next;
      END;
      wc(':'); wc(' ');
    END;
    out_type_ref(tpos,ot,lev,dso);
    o:=o.next;
    IF o#NIL THEN
      IF pm#profile_type THEN wc(";") ELSE wc(",") END;
      wc(" ");
    END;
  END;

  IF get_ln(get_ps(t.end))>tpos THEN
    IF coms THEN out_comment(tpos,get_ps(t.end),dso,FALSE,"")
    ELSE wl; tpos:=get_ps(t.end);
    END;
    t.end.unpack(fnm,l,c);
    skip(c + dso);
  END;
  wc(")");
  IF t.base.mode#pc.ty_void THEN
    wc(':'); wc(' ');
    out_type_ref(tpos,t.base,lev,dso);
  END;
END out_profile;

PROCEDURE find_enum_const(t: pc.STRUCT; n: LONGINT): pc.OBJECT;
  VAR l: pc.OBJECT;
BEGIN
  IF t=NIL THEN RETURN NIL END;
  l:=t.prof;
  LOOP
    IF l=NIL THEN EXIT END;
    IF (l.val#NIL) & (l.val.mode=pc.nd_value) THEN
      IF l.val.val.get_integer()=n THEN EXIT END;
    END;
    l:=l.next
  END;
  RETURN l;
END find_enum_const;

PROCEDURE out_value_set(v: pc.VALUE; t: pc.STRUCT);
  VAR
    zz_inx: pc.VALUE;
    base: pc.STRUCT;
    fst: BOOLEAN;
    i,j: LONGINT;
BEGIN
  zz_inx:=pc.value.new(env.null_pos,pc.ZZ_type);
  out_type_name(t);
  base:=t.base;
  wc("{");
  fst:=TRUE; i:=0;
  WHILE i<t.len DO
    zz_inx.index_get(i,v);
    IF NOT zz_inx.is_zero() THEN
      IF fst THEN fst:=FALSE ELSE wc(",") END;
      zz_inx.set_integer(i);
      zz_inx.binary(pc.sb_plus,zz_inx,t.min);
      out_value_ordinal(zz_inx,base);
      j:=i;
      LOOP
        IF j+1>=t.len THEN EXIT END;
        zz_inx.index_get(j+1,v);
        IF zz_inx.is_zero() THEN EXIT END;
        INC(j);
      END;
      IF j>i THEN
        zz_inx.set_integer(j);
        zz_inx.binary(pc.sb_plus,zz_inx,t.min);
        ws(range);
        out_value_ordinal(zz_inx,base);
        i:=j;
      END;
      INC(i);
    END;
    INC(i);
  END;
  wc("}");
END out_value_set;

PROCEDURE out_value_vector(v: pc.VALUE; t: pc.STRUCT);
  CONST
    max_size = 64;
  VAR
    b  : pc.STRUCT;
    qwt: BOOLEAN;
    apf: BOOLEAN;
    fst: BOOLEAN;
    i,j: INTEGER;
    l  : INTEGER;
    ch : CHAR;
    bf : ARRAY max_size+1 OF CHAR;
    zz_tmp: pc.VALUE;
BEGIN
  zz_tmp:=pc.value.new(env.null_pos,t.base);
  b:=t.base;
  IF b.mode=pc.ty_range THEN b:=b.base END;
  IF (b.mode=pc.ty_char) & t.min.is_zero() THEN
    fst:=TRUE;
    i:=0;
    l:=0;
    LOOP
      IF i>=t.len THEN EXIT END;
      zz_tmp.index_get(i,v);
      IF zz_tmp.is_zero() THEN EXIT END;
      IF fst THEN fst:=FALSE ELSE ws(" + "); INC(l,3) END;
      IF l>=max_size THEN wl; skip(4); l:=0 END;
      j:=0;
      qwt:=FALSE;
      apf:=FALSE;
      LOOP
        IF l>=max_size THEN EXIT END;
        zz_tmp.index_get(i,v);
        ch:=CHR(zz_tmp.get_integer());
        IF ch=0X THEN EXIT END;
        IF (ch>=' ') & (ch<=176C) THEN
        IF ch="'" THEN
          IF qwt THEN EXIT END;
            apf:=TRUE;
          ELSIF ch='"' THEN
            IF apf THEN EXIT END;
            qwt:=TRUE;
          END;
          bf[j]:=ch; INC(j);
        ELSE
          IF j=0 THEN bf[j]:=ch; INC(j); INC(i); INC(l) END;
          EXIT;
        END;
        INC(i); INC(l);
      END;
      bf[j]:=0X;
      ASSERT(j>0);
      INC(l,2);
      IF j=1 THEN
        ch:=bf[0];
        IF (ch<' ') OR (ch>176C) THEN wi("%.3XX",ORD(ch)); INC(l);
        ELSIF apf THEN wc('"'); wc(ch); wc('"');
        ELSE wc("'"); wc(ch); wc("'");
        END;
      ELSIF qwt THEN
        wc("'"); ws(bf); wc("'");
      ELSE
        wc('"'); ws(bf); wc('"');
      END;
    END;
  ELSE
    ws("???vector");
  END;
END out_value_vector;

PROCEDURE out_value_ordinal(v: pc.VALUE; t: pc.STRUCT);
  VAR o: pc.OBJECT; bf: ARRAY 64 OF CHAR; n: LONGINT;
BEGIN
  CASE t.mode OF
    |pc.ty_shortcard,pc.ty_cardinal,pc.ty_longcard,
     pc.ty_shortint,pc.ty_integer,pc.ty_longint,pc.ty_ZZ:
      v.value_to_str(bf,pc.flag_o2); ws(bf);
    |pc.ty_loc      : wi("LOC(%d)",v.get_integer());
    |pc.ty_range    : out_value_ordinal(v,t.base);
    |pc.ty_enum     : o:=find_enum_const(t,v.get_integer());
      IF o=NIL THEN wi("***%d***",v.get_integer());
      ELSE out_object_name(o,FALSE);
      END;
    |pc.ty_boolean  :
      IF v.is_zero() THEN ws("FALSE") ELSE ws("TRUE") END;
    |pc.ty_char     :
      n:=v.get_integer();
      IF (n>=32) & (n<=126) THEN wi('"%c"',n);
      ELSE wi("%.3XX",n);
      END;
  END;
END out_value_ordinal;

PROCEDURE out_value(v: pc.VALUE; t: pc.STRUCT);
  VAR r: pc.VALUE;
BEGIN
  ASSERT(t#NIL);
  IF t.is_ordinal() THEN out_value_ordinal(v,t); RETURN END;
  CASE t.mode OF
    |pc.ty_real     : wr("%.8LE",v.get_real());
    |pc.ty_longreal : wr("%.16LE",v.get_real());
    |pc.ty_RR       : wr("%.16LE",v.get_real());
    |pc.ty_complex  :
      r:=pc.value.new(v.pos,pc.RR_type);
      r.unary(pc.su_re,v); wr("%.8LE+",r.get_real());
      r.unary(pc.su_im,v); wr("%.8LEi",r.get_real());
    |pc.ty_lcomplex,pc.ty_CC:
      r:=pc.value.new(v.pos,pc.RR_type);
      r.unary(pc.su_re,v); wr("%.16LE+",r.get_real());
      r.unary(pc.su_im,v); wr("%.16LEi",r.get_real());
    |pc.ty_AA       : ws("NIL");
    |pc.ty_set      : out_value_set(v,t);
    |pc.ty_opaque   : ws("???opaque");
    |pc.ty_pointer  : ws("???pointer");
    |pc.ty_proctype : ws("???proctype");
    |pc.ty_array    : ws("???array");
    |pc.ty_array_of : ws("???array_of");
    |pc.ty_record   : ws("???record");
    |pc.ty_SS       : out_value_vector(v,t);
  ELSE ws("???"); wi("%d",ORD(t.mode));
  END;
END out_value;

PROCEDURE out_method(VAR ps: LONGINT; o: pc.OBJECT;
       redef,coms: BOOLEAN; lev,dso: INTEGER);
(* ps - current source text position *)
  VAR p: pc.OBJECT;
BEGIN
  IF coms THEN out_comment(ps,get_ps(o.pos),dso,FALSE,"");
  ELSE ps:=get_ps(o.pos);
  END;
  skip(lev); ws("PROCEDURE ");
  out_sysflag(o.type);
  IF redef THEN ws("/ ") END;
  p:=o.type.prof; (* first parameter - receiver *)
  ASSERT(p#NIL);
  wc("(");
  IF p.mode=pc.ob_varpar THEN ws("VAR ") END;
  out_prm_name(p);
  ws(": ");
  out_type_name(p.type);
  ws(") ");
  out_object_name(o,TRUE);
  out_profile(ps,o.type,profile_method,coms,lev,dso);
  wc(';');
END out_method;

PROCEDURE out_objects(VAR ps: LONGINT; list: LIST);
  VAR
    o    : pc.OBJECT;
    md   : pc.OB_MODE;
    sz   : INTEGER;
    l    : LIST;
    nm   : Buffer;
    ps_fr: LONGINT;
    ps_to: LONGINT;
    fst  : BOOLEAN;
BEGIN
  md:=pc.ob_inv; sz:=-1;
  WHILE list#NIL DO
    o:=list.obj;
    IF o.mode=pc.ob_module THEN
      out_comment(ps,get_ps(o.pos),0,TRUE,"");
      ws("IMPORT "); fst:=TRUE;
      REPEAT
        ASSERT(omark_vis IN o.marks);
        IF fst THEN fst:=FALSE ELSE wc(','); wc(' ') END;
        ws(o.name^);
        EXCL(o.marks,omark_vis);
        list:=list.next;
        IF list=NIL THEN o:=NIL ELSE o:=list.obj END;
      UNTIL (o=NIL) OR (o.mode#pc.ob_module);
      wc(";");
      out_comment(ps,get_le(ps),0,FALSE,"");
      md:=pc.ob_inv;
    ELSE
      ASSERT(omark_vis IN o.marks);
      IF o.mode#md THEN
        sz:=0; l:=list;
        REPEAT
          str_object_name(nm,l.obj,TRUE);
          IF sz<LENGTH(nm) THEN sz:=SHORT(LENGTH(nm)) END;
          l:=l.next;
        UNTIL (l=NIL) OR (l.obj.mode#o.mode);
      END;
      ASSERT(sz>0);
      CASE o.mode OF
      |pc.ob_type:
        IF md#pc.ob_type THEN out_comment(ps,get_ps(o.pos),0,TRUE,"TYPE");
        ELSE out_comment(ps,get_ps(o.pos),0,FALSE,"");
        END;
        skip(2);
        out_object_name_sz(o,TRUE,sz);
        IF o.type.obj#o THEN
          ws(equ); out_type_ref(ps,o.type,2,0);
        ELSIF o.type.mode#pc.ty_opaque THEN
          ws(equ); out_type_def(ps,o.type,2,0);
        END;
        wc(";");
      |pc.ob_var :
        IF md#pc.ob_var THEN out_comment(ps,get_ps(o.pos),0,TRUE,"VAR");
        ELSE out_comment(ps,get_ps(o.pos),0,FALSE,"");
        END;
        skip(2);
        out_object_name_sz(o,TRUE,sz);
        ws(": ");
        out_type_ref(ps,o.type,2,0);
        wc(";");
      |pc.ob_cons:
        IF md#pc.ob_cons THEN out_comment(ps,get_ps(o.pos),0,TRUE,"CONST");
        ELSE out_comment(ps,get_ps(o.pos),0,FALSE,"");
        END;
        skip(2);
        out_object_name_sz(o,TRUE,sz);
        ws(equ);
        CASE o.val.mode OF
        | pc.nd_var:
          out_object_name(o.val.obj,FALSE);
        | pc.nd_proc:
          out_object_name(o.val.obj,FALSE);
        | pc.nd_unary:
          CASE o.val.sub OF
          | pc.su_min: ws('MIN(');
          | pc.su_max: ws('MIN(');
          ELSE
            ASSERT(FALSE, ORD(o.val.sub));
          END;
          ASSERT(o.val.l.mode = pc.nd_type);
          out_type_name(o.val.l.type);
          wc(')');
        ELSE
          ASSERT(o.val.mode=pc.nd_value);
          out_value(o.val.val,o.type);
        END;
        wc(";");
      |pc.ob_proc,pc.ob_xproc,pc.ob_eproc,pc.ob_cproc,pc.ob_lproc:
        IF o.host.mode=pc.ty_record THEN (* method *)
          ps_fr:=get_bind_down_comment(o.pos);
          ps_to:=get_bind_up_comment(get_ps(o.type.end));
          IF (style=st_body) OR (style=st_doc)
(* !!!!!!
                & ( NOT (tags_redefined IN o.tags) OR
                (ps_fr#get_ps(o.pos)) OR (ps_to#get_ps(o.type.end)) )
*)
          THEN
            out_comment(ps,get_ps(o.pos),0,TRUE,"");
            out_method(ps,o,FALSE,TRUE,0,0);
          ELSE
            out_comment(ps,ps_fr,0,FALSE,"");
            ps:=ps_to;
          END;
        ELSE
          out_comment(ps,get_ps(o.pos),0,TRUE,"");
          ws("PROCEDURE ");
          IF o.mode=pc.ob_cproc THEN wc('-'); wc(' ') END;
          out_sysflag(o.type);
          out_object_name(o,TRUE);
          out_profile(ps,o.type,profile_proc,TRUE,0,0);
          wc(";");
        END;
        IF (style=st_body) & (o.mode IN pc.OB_SET{pc.ob_proc,pc.ob_xproc}) THEN
          out_comment(ps,get_bind_up_comment(ps),0,FALSE,"");
          ws("END ");
          out_object_name(o,FALSE);
          wc(";");
        END;
      ELSE
        wi("  ???object.mode = %d",ORD(o.mode));
      END;
      out_comment(ps,get_le(ps),0,FALSE,"");
      md:=o.mode;
      EXCL(o.marks,omark_vis);
      list:=list.next;
    END;
  END;
END out_objects;

PROCEDURE module(o: pc.OBJECT; list: LIST);
  VAR ps: LONGINT;
BEGIN
  ps:=0;
  out_comment(ps,get_ps(o.pos),0,FALSE,"");
  IF style=st_body THEN ws("MODULE ") ELSE ws("DEFINITION ") END;
  out_sysflag(o.type);
  ws(o.name^); wc(';');
  out_comment(ps,get_le(ps),0,FALSE,"");
  out_objects(ps,list);
  out_comment(ps,MAX(LONGINT),0,FALSE,"");
  wl; ws("END "); ws(o.name^); wc('.'); wl;
END module;

PROCEDURE set_style;
  VAR s: pc.STRING;
BEGIN
  style:=st_def;
  env.config.Equation("BSTYLE",s);
  IF s#NIL THEN
    IF    s^="MOD"  THEN style:=st_body
    ELSIF s^="DOC"  THEN style:=st_doc
    ELSIF s^="DEF"  THEN style:=st_def
    ELSE
      env.errors.Warning(env.null_pos,434,"BSTYLE","DEF");
    END;
  END;
END set_style;

PROCEDURE set_options;
BEGIN
  closure :=(style=st_def)  & env.config.Option("BSCLOSURE");
  redefine:=(style=st_def)  & env.config.Option("BSREDEFINE");
  alpha   :=(style#st_body) & env.config.Option("BSALPHA");
END set_options;

PROCEDURE browse_cu*(mod: pc.OBJECT);
  VAR list: LIST;
BEGIN
  cur_mod:=mod;
  set_style;
  set_options;
  ASSERT(mod.mode=pc.ob_module);
  create_out(mod.name^);
  IF alpha THEN
    comment_fst:=NIL;
    mark_objects(mod,alpha_insert,list);
  ELSE
    comment_fst:=pc.comments;
    IF comment_fst#NIL THEN comment_fst:=comment_fst.next END;
    mark_objects(mod,pos_insert,list);
  END;
  module(mod,list);
  close_out;
  cur_mod:=NIL;
END browse_cu;

(*----------------------------------------------------------------*)

BEGIN
  order[pc.ob_inv      ]:=-1;
  order[pc.ob_var      ]:= 3;
  order[pc.ob_varpar   ]:=-1;
  order[pc.ob_seq      ]:=-1;
  order[pc.ob_label    ]:=-1;
  order[pc.ob_proc     ]:= 4;
  order[pc.ob_xproc    ]:= 4;
  order[pc.ob_eproc    ]:= 4;
  order[pc.ob_cproc    ]:= 4;
  order[pc.ob_lproc    ]:= 4;
  order[pc.ob_cons     ]:= 1;
  order[pc.ob_type     ]:= 2;
  order[pc.ob_sproc    ]:=-1;
  order[pc.ob_field    ]:=-1;
  order[pc.ob_field_bts]:=-1;
  order[pc.ob_module   ]:= 0;
END xcBrowse.
