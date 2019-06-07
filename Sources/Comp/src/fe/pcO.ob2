(** Copyright (c) 1991,97 XDS Ltd, Russia. All Rights Reserved. *)
(** M2/O2 front-end: Objects (visibility and symbol files *)
MODULE pcO; (** Ned 28-Mar-91. *)
            (** Sem 23-Sep-93. *)

(* Modifications:
   14-Mar-96 Ned    BUG006: otag_marked is appended.
   19-Mar-96 Ned    fn_ref & SYSTEM.REF are added.
   08-Apr-96 Ned    out_list: put "sym_case sym_end" for empty variant in a
                    record type.
   15-Apr-96 Ned    constants of proctype with value NIL are invented.
                    Fixes in rd_obj, wr_obj and gather_value.
   07-May-96 Enal   rewrite sort_list to use QuickSort.
   22-May-96 Ned    BYTE now have the same STRUCT as LOC
   23-May-96 Ned    symfile version VERS = 23
   24-May-96 Ned    BUG0128: inp_sym_file: browse parameter is added and used.
   27-Feb-97 VitVit read from/write to SYM-file DLL's name
*)

<* +o2addkwd *>
FROM   SYSTEM IMPORT PRED,SUCC, EVAL;
IMPORT SYSTEM;

IMPORT pc  := pcK;
IMPORT xfs := xiFiles;
IMPORT env := xiEnv;
IMPORT xcStr;
IMPORT DStrings;
IMPORT FormStr;
IMPORT pcS;

CONST
  COMPILER* = "COMPILER";  (** name of additional "system" module *)

  IDENT_GAP_DEMO =  -300;  (* to make (sym_ident+gap) < 80H in demo-version *)
  IDENT_GAP_TRIAL= 4000H;  (* to make (sym_ident+gap) > 4000H in trial version *)

CONST
  otag_forward*    = pc.otag_aux13;
  otag_exported*   = pc.otag_aux14;  (* object is exported from (local) module *)
  otag_method*     = pc.otag_aux17;
  otag_varpar_nil* = pc.otag_aux20;  (* object is special VAR parameter, may be NIL *)

  omark_used_in_for*  = pc.omark_aux0;  (* object was used in FOR *)
  omark_threat_proc*  = pc.omark_aux1;  (* procedure is processed for threatening *)
  omark_threatened*   = pc.omark_aux2;  (* object was used from another scope     *)
  (* Отличается от pc.otag_threatened тем, что указывает на использование
     в исходном тексте, а не в оптимизированной программе *)
  omark_dis_array_of* = pc.omark_aux3;  (* forward type must not be array of *)

  omark_tried *= pc.omark_aux19;  -- object was tried to serach

  ttag_incomplete  = pc.ttag_aux20; (* full sym file was not read *)
  ttag_public*     = pc.ttag_aux21; (* used in Oberon *)
  ttag_num_valid*  = pc.ttag_aux22; (* used for method number calculation *)

  ntag_dynarr*     = pc.ntag_aux20; (* used for dynarrs, not flexible arrays! *)

  ob_alias       = pc.ob_aux1;
  ob_usage       = pc.ob_aux2;
  ob_enum        = pc.ob_aux3; (* enum const alias *)

  ty_invtype*    = pc.ty_free;
  ty_undef*      = SYSTEM.SUCC(pc.ty_free);

CONST mno_pervasive = pc.mno_pervasive;
      mno_system    = PRED(mno_pervasive);
      mno_compiler  = PRED(mno_system);
      mno_last      = mno_compiler;

TYPE
  CUR_POS_PROC = PROCEDURE (VAR pos: pc.TPOS);

  EOBJECT*=POINTER TO eobject_rec;
  eobject_rec=RECORD (pc.object_rec)
    l,r : EOBJECT;
    scp : pc.STRUCT;
    bal : SHORTINT;
  END;

  ESTRUCT=POINTER TO estruct_rec;
  estruct_rec=RECORD (pc.struct_rec)
    tree: EOBJECT;
    up  : pc.STRUCT;
    time: INTEGER;   (* for graph coloring when cleaning the BE extensiosn *)
  END;

  TYPES_BUF  = POINTER TO ARRAY OF pc.STRUCT;

(*----------------------------------------------------------------*)

VAR
  -- this var is used to check whether we should intialize
  -- module-dependend part of STRUCT or OBJECT (scope, module, etc) or not.
  initialized : BOOLEAN;

  ident_gap : LONGINT;
  global    : BOOLEAN;
  need_syms : BOOLEAN;

(* active opions & equations *)

  align*    : SHORTINT;      (** type alignment *)
  volatile* : BOOLEAN;       (** volatile variable or type *)

(*------------------------*)

  inv_obj  -: pc.OBJECT;     (** illegal object *)
  false    -: pc.OBJECT;
  true     -: pc.OBJECT;
  nil      -: pc.OBJECT;
  nullproc -: pc.OBJECT;

  index    -: pc.STRUCT;
  lens_type-: pc.STRUCT; (* size_t[1..MAX(size_t)] *)
  difadr   -: pc.STRUCT;
  difadrc   -: pc.STRUCT;
  bitset   -: pc.STRUCT;
--  bitset64   -: pc.STRUCT;
  boolean  -: pc.STRUCT;
  unsigned -: pc.STRUCT;
  signed   -: pc.STRUCT;
  size_t   -: pc.STRUCT;
  m2_int   -: pc.STRUCT;
  m2_card  -: pc.STRUCT;

  level    -: SHORTINT; (* ! модуль не увеличивает уровень вложенности *)
  def      *: BOOLEAN;  (* definition module     *)
  imp      *: BOOLEAN;  (* implementation module *)

  system_imported-: BOOLEAN; (* there was IMPORT SYSTEM in current module *)

  zero      : pc.VALUE;
  one       : pc.VALUE;

  cur_scope-: pc.STRUCT; (** current block *)
  cur_tail  : pc.OBJECT; (* current block mem allocation list tail *)

  anonim_cnt: INTEGER;

  oberon    : BOOLEAN; (* current language is Oberon-2 *)
  cur_pos   : CUR_POS_PROC;


  types     : TYPES_BUF;
  file      : xfs.SymFile; (* sym-file *)


(*--------------------- Standard types --------------------------*)

VAR
  (** standard types *)
  (** стандартные типы не зависят от текущего значения опций! *)
  invtype   -: pc.STRUCT;
  undef     -: pc.STRUCT;
  void      -: pc.STRUCT;
  shortcard -: pc.STRUCT;
  cardinal  -: pc.STRUCT;
  longcard  -: pc.STRUCT;
  shortint  -: pc.STRUCT;
  integer   -: pc.STRUCT;
  longint   -: pc.STRUCT;
  real      -: pc.STRUCT;
  longreal  -: pc.STRUCT;
  ld_real   -: pc.STRUCT;
  complex   -: pc.STRUCT;
  lcomplex  -: pc.STRUCT;
  boolean8   : pc.STRUCT;
  boolean16  : pc.STRUCT;
  boolean32  : pc.STRUCT;
  char      -: pc.STRUCT;
  set8       : pc.STRUCT;
  set16      : pc.STRUCT;
  set32      : pc.STRUCT;
  set64      : pc.STRUCT;
  loc       -: pc.STRUCT;
  word      -: pc.STRUCT;
  word2     -: pc.STRUCT;
  word4     -: pc.STRUCT;
  addr      -: pc.STRUCT;
  proctype0 -: pc.STRUCT;
  protection-: pc.STRUCT;
  AA_type   -: pc.STRUCT;
  CC_type   -: pc.STRUCT;
  ZZ_type   -: pc.STRUCT;
  RR_type   -: pc.STRUCT;
  signed16   : pc.STRUCT;
  unsigned16 : pc.STRUCT;
  size_t16   : pc.STRUCT;
  signed32   : pc.STRUCT;
  unsigned32 : pc.STRUCT;
  size_t32   : pc.STRUCT;
  process   -: pc.STRUCT;
  lens_t16   : pc.STRUCT;
  lens_t32   : pc.STRUCT;

  longlongint-: pc.STRUCT;
  longlongcard-: pc.STRUCT;

  std_types- : ARRAY 64 OF pc.STRUCT;

(*----------------------------------------------------------------*)

PROCEDURE ( o: EOBJECT ) GetReadableName* (need_class_name: BOOLEAN ): pc.STRING;
VAR
  s  : ARRAY 1024 OF CHAR;
  ss : pc.STRING;

  PROCEDURE app_procname(o : pc.OBJECT);
  BEGIN
    IF (o.host#NIL)&(o.host.mode = pc.ty_record) THEN
      FormStr.append (s, "%s::", o.host.obj.name^);
    ELSIF (o.host#NIL)&(o.host.mode = pc.ty_proctype) THEN
      app_procname(o.host.obj);
      FormStr.append (s, "$");
    END;
    FormStr.append (s, "%s", o.name^);
  END app_procname;

BEGIN
  FormStr.print(s, "");
  app_procname(o);
  NEW(ss, LENGTH(s)+1);
  FormStr.print(ss^,"%s",s);
  RETURN ss;
END GetReadableName;

(*----------------------------------------------------------------*)

VAR
  module_super : pc.OBJECT;
  module_system: pc.OBJECT;
  module_xds  *: pc.OBJECT;
  def_import   : ESTRUCT;

PROCEDURE enter_scope*(b: pc.STRUCT);
(* бывает scope у которого obj=NIL ! *)
BEGIN
  IF b.obj#NIL THEN b:=b.obj.type END;
  ASSERT((b.mode=pc.ty_proctype) OR (b.mode=pc.ty_module));
  IF b.mode#pc.ty_module THEN INC(level) END;
  b(ESTRUCT).up:=cur_scope;
  cur_scope:=b;
  cur_tail:=cur_scope.mem;
  IF cur_tail#NIL THEN
    WHILE cur_tail.next#NIL DO cur_tail:=cur_tail.next END;
  END;
END enter_scope;

PROCEDURE correct_cur_tail*;
BEGIN
  cur_tail:=cur_scope.mem;
  IF cur_tail#NIL THEN
    WHILE cur_tail.next#NIL DO cur_tail:=cur_tail.next END;
  END;
END correct_cur_tail;

PROCEDURE exit_scope*;
BEGIN
  IF cur_scope.mode#pc.ty_module THEN DEC(level) END;
  cur_scope:=cur_scope(ESTRUCT).up;
  IF cur_scope=NIL THEN
    cur_tail:=NIL;
  ELSE
    cur_tail:=cur_scope.mem;
    IF cur_tail#NIL THEN
      WHILE cur_tail.next#NIL DO cur_tail:=cur_tail.next END;
    END;
  END;
END exit_scope;

PROCEDURE alloc*(o: pc.OBJECT);
BEGIN
  ASSERT(o.next=NIL);
  ASSERT(cur_scope#NIL);
  o.lev:=level;
  o.host:=cur_scope;
  IF cur_tail=NIL THEN cur_scope.mem:=o;
  ELSE cur_tail.next:=o;
  END;
  cur_tail:=o;
END alloc;

PROCEDURE alloc_enum*(o: pc.OBJECT; enum: pc.STRUCT; VAR h,t: pc.OBJECT);
BEGIN
  ASSERT(enum.mode=pc.ty_enum);
  o.lev:=level;
  o.type:=enum;
  o.host:=enum;
  IF h=NIL THEN h:=o ELSE t.next:=o END;
  t:=o;
END alloc_enum;

PROCEDURE alloc_field*(o: pc.OBJECT; rec: pc.STRUCT; VAR h,t: pc.OBJECT);
BEGIN
  ASSERT(rec.mode=pc.ty_record);
  o.lev:=0;
  o.host:=rec;
  IF h=NIL THEN h:=o ELSE t.next:=o END;
  t:=o;
END alloc_field;

PROCEDURE alloc_param*(o: pc.OBJECT; proc: pc.STRUCT; VAR t: pc.OBJECT);
BEGIN
  ASSERT(proc.mode=pc.ty_proctype);
  ASSERT((proc.obj=NIL) OR (level=proc.obj.lev+1));
  o.lev:=level;
  o.host:=proc;
  IF proc.prof=NIL THEN proc.prof:=o ELSE t.next:=o END;
  t:=o;
END alloc_param;

PROCEDURE alloc_method*(o: pc.OBJECT; rec: pc.STRUCT);
BEGIN
  ASSERT(rec.mode=pc.ty_record);
  ASSERT(o.next=NIL);
  o.lev:=level;
  o.host:=rec;
  o.next:=rec.mem;
  rec.mem:=o;
END alloc_method;

PROCEDURE object_scope*(o: pc.OBJECT): pc.STRUCT;
BEGIN
  RETURN o(EOBJECT).scp;
END object_scope;


(*------------Constructors for OBJECT, STRUCT, NODE---------------*)


PROCEDURE ini_type*( t: pc.STRUCT; mode: pc.TY_MODE );
VAR e: ESTRUCT;
BEGIN
  pc.ini_type(t, mode);
  e := t(ESTRUCT);

  IF initialized THEN
    IF volatile THEN INCL(e.tags,pc.ttag_volatile) END;
    e.align:=align;
    cur_pos(e.pos);
    CASE mode OF
      |   ty_invtype  : e.min:=zero; e.max:=zero;
      |pc.ty_enum     :
      |pc.ty_array_of : e.inx:=index;
      |pc.ty_SS       : e.inx:=index; e.base:=char;
      |pc.ty_pointer,pc.ty_record,pc.ty_opaque:
        IF oberon THEN e.flag:=pc.flag_o2 END;
      |pc.ty_module   :
        IF oberon THEN e.flag:=pc.flag_o2 END;
    ELSE
    END;
  ELSE
    e.align := 0;
  END;
END ini_type;

--------------------------------------------------------------------------------
PROCEDURE new_type*( mode: pc.TY_MODE ): pc.STRUCT;
VAR e: ESTRUCT;
BEGIN
  NEW(e);
  ini_type(e, mode);
  RETURN e;
END new_type;

--------------------------------------------------------------------------------
PROCEDURE ini_obj*( o: pc.OBJECT; mode: pc.OB_MODE );
VAR e: EOBJECT;
BEGIN
  e := o(EOBJECT);
  ASSERT(initialized); -- it's assumed that noone needs to create objects
                       -- before pcO is initialized.
  pc.ini_obj(e, mode);
  cur_pos(e.pos);
  e.end := e.pos;
  e.lev := level;
END ini_obj;

PROCEDURE new_obj*( mode: pc.OB_MODE ): pc.OBJECT;
VAR e: EOBJECT;
BEGIN
  NEW(e);
  ini_obj(e, mode);
  RETURN e;
END new_obj;

PROCEDURE set_name*(o: pc.OBJECT; s-: ARRAY OF CHAR);
  VAR x: pc.STRING;
BEGIN
  NEW(x,LENGTH(s)+1);
  COPY(s,x^);
  o.name:=x;
END set_name;

--------------------------------------------------------------------------------
PROCEDURE set_anonim_name *(o: pc.OBJECT; attr-: ARRAY OF CHAR);
VAR nm: pc.NAME;
BEGIN
  -- can not use jast counter for names, because of .DEF
  IF def THEN xcStr.prn_txt(nm, "$D%s%d", attr, anonim_cnt);
  ELSE        xcStr.prn_txt(nm, "$%s%d",  attr, anonim_cnt);
  END;
  INC(anonim_cnt);
  set_name(o,nm);
END set_anonim_name;


PROCEDURE ^ dcl*(scope: pc.STRUCT; o: pc.OBJECT);

PROCEDURE set_type_base*(t,b: pc.STRUCT);
  VAR ps: pc.TPOS;
BEGIN
  IF t.mode IN pc.TY_SET{pc.ty_array,pc.ty_array_of} THEN
    IF b.flag=pc.flag_o2 THEN
      IF NOT (t.flag IN pc.LangSet{pc.flag_m2, pc.flag_o2}) THEN
        cur_pos(ps);
        env.errors.Error(ps,143);
      END;
      t.flag:=pc.flag_o2
    END;
  END;
  t.base:=b;
END set_type_base;

--------------------------------------------------------------------------------
PROCEDURE new_anonim_obj*(VAR o: pc.OBJECT; m: pc.OB_MODE);
BEGIN
  o := new_obj(m);
  CASE m OF
  | pc.ob_type:  set_anonim_name(o, "Type");
  ELSE           set_anonim_name(o, "");
  END;
END new_anonim_obj;

--------------------------------------------------------------------------------
PROCEDURE new*(VAR x: pc.NODE; mode: pc.ND_MODE);
BEGIN
  NEW(x);
  x.mode:=mode;
  x.sub:=pc.su_none;
  x.tags:=pc.NTAG_SET{};
  cur_pos(x.pos);
END new;

PROCEDURE tie*(VAR head,tail: pc.NODE; mode: pc.ND_MODE);
BEGIN
  IF head=NIL THEN new(head,mode); tail:=head;
  ELSE new(tail.next,mode); tail:=tail.next;
  END;
END tie;

PROCEDURE app*(VAR head,tail: pc.NODE; n: pc.NODE);
BEGIN
  IF n=NIL THEN RETURN END;
  IF head=NIL THEN head:=n ELSE tail.next:=n END;
  WHILE n.next#NIL DO n:=n.next END;
  tail:=n;
END app;


<* IF __GEN_C__ THEN *>

<*+ NOEXTERN *>
PROCEDURE [2] / strcmp(s1,s2: ARRAY OF CHAR): SYSTEM.int;

<* ELSE *>

PROCEDURE strcmp(s1-,s2-: ARRAY OF CHAR): INTEGER;
  VAR c1: CHAR; i: INTEGER;
BEGIN
  i:=0;
  LOOP
    c1:=s1[i];
    IF c1#s2[i] THEN RETURN ORD(c1)-ORD(s2[i]) END;
    IF c1=0C THEN RETURN 0 END;
    INC(i);
  END;
END strcmp;

<* END *>

PROCEDURE app_bal1(x: EOBJECT; VAR p: EOBJECT; VAR h: BOOLEAN): pc.OBJECT;
(* add object to scope and if there is a "ob_usage" object with the same name,
   then drop "ob_usage" *)
  VAR p1,p2: EOBJECT; y: pc.OBJECT; r: LONGINT;
BEGIN
  IF p=NIL THEN
    p:=x; x.l:=NIL; x.r:=NIL; h:=TRUE; x.bal:=0; RETURN NIL;
  END;
  r:=strcmp(p.name^,x.name^);
  IF r>0 THEN
    y:=app_bal1(x,p.l,h);
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
    y:=app_bal1(x,p.r,h);
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
    IF p.mode=ob_usage THEN
       x.l:=p.l; x.r:=p.r; h:=FALSE; x.bal:=p.bal; p:=x;
       RETURN NIL;
    ELSE
       x.scp:=NIL; h:=FALSE; RETURN p;
    END;
  END;
END app_bal1;

PROCEDURE app_bal(x: EOBJECT; VAR p: EOBJECT; VAR h: BOOLEAN): pc.OBJECT;
(* h = TRUE -> ўлбRв  ¤_а_ў  ўRаRб< 
   p.bal = ўлбRв (p.r) - ўлбRв (p.l)
*)
  VAR p1,p2: EOBJECT; y: pc.OBJECT; r: LONGINT;
BEGIN
  IF p=NIL THEN
    p:=x; x.l:=NIL; x.r:=NIL; h:=TRUE; x.bal:=0; RETURN NIL;
  END;
  r:=strcmp(p.name^,x.name^);
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
    x.scp:=NIL; h:=FALSE; RETURN p;
  END;
END app_bal;

PROCEDURE (root: ESTRUCT) append(o: pc.OBJECT): pc.OBJECT;
  VAR h: BOOLEAN;
BEGIN
  o(EOBJECT).scp:=root;
  RETURN app_bal(o(EOBJECT),root.tree,h);
END append;

PROCEDURE (root: ESTRUCT) append1(o: pc.OBJECT): pc.OBJECT;
  VAR h: BOOLEAN;
BEGIN
  o(EOBJECT).scp:=root;
  RETURN app_bal1(o(EOBJECT),root.tree,h);
END append1;

PROCEDURE (root: ESTRUCT) find(name-: ARRAY OF CHAR): pc.OBJECT;
  VAR x: EOBJECT; r: LONGINT;
BEGIN
  x:=root.tree;
  LOOP
    IF x=NIL THEN RETURN NIL END;
    r:=strcmp(x.name^,name);
    IF r=0 THEN RETURN x END;
    IF r>0 THEN x:=x.l ELSE x:=x.r END;
  END;
END find;


--------------------------------------------------------------------------------
PROCEDURE GetNameAlias * (scope: pc.STRUCT; name-: ARRAY OF CHAR): pc.OBJECT;
VAR found: pc.OBJECT;

   -- 1 ---- GetNameAlias ------------------------------------------------------
   PROCEDURE find_alias (node: EOBJECT): BOOLEAN;
   BEGIN
     IF (node = NIL) THEN 
       RETURN FALSE 
     END;
     IF (node.actual # NIL) AND 
        (node.mode = ob_alias) AND
        (node.actual.name^ = name)
     THEN
       found := node;
       RETURN TRUE;
     ELSIF find_alias (node.l) OR find_alias (node.r) THEN
       RETURN TRUE;
     ELSE 
       RETURN FALSE;
     END;
   END find_alias;

-- 0 ---- GetNameAlias --------------------------------------------------------
BEGIN
  ASSERT( (scope.mode = pc.ty_proctype) OR (scope.mode = pc.ty_module) );
  IF find_alias (scope(ESTRUCT).tree) THEN
    RETURN found;
  ELSE
    RETURN NIL;
  END;
END GetNameAlias;


(*----------------------------------------------------------------*)

PROCEDURE invis(name-: ARRAY OF CHAR; VAR o: pc.OBJECT);
  VAR ps: pc.TPOS;
BEGIN
  o := new_obj(pc.ob_inv);
  o.type:=inv_obj.type;
  set_name(o,name);
  cur_pos(ps);
  env.errors.Error(ps,20,name);
END invis;

(*<An1*)
PROCEDURE mark_tried(o:pc.OBJECT);
BEGIN
  INCL(o.marks, omark_tried);
  IF (o.host#NIL)&(o.host.mode=pc.ty_enum) THEN
    INCL(o.host.obj.marks, omark_tried);
  END;
END mark_tried;
(*<An1*)

PROCEDURE try_scp*(scope: pc.STRUCT; name-: ARRAY OF CHAR;
                VAR o: pc.OBJECT): BOOLEAN;
(** ищет среди всех объектов (в том числе неэкспортированных)
    указанной области видимости
*)
  VAR s: ESTRUCT;
BEGIN
  IF scope.obj#NIL THEN scope:=scope.obj.type END;
  s:=scope(ESTRUCT);
  o:=s.find(name);
  IF (o=NIL) & (def_import#NIL) & (s=def_import.up) THEN
    o:=def_import.find(name);
  END;
  IF o=NIL THEN RETURN FALSE END;
  IF o.mode=ob_usage THEN
    o:=NIL; RETURN FALSE
  ELSIF o.mode=ob_alias THEN
    o:=o.actual;
  ELSIF o.mode=ob_enum THEN
    o:=o.actual;
    IF s.mno=pc.cur_mod THEN o:=NIL; RETURN FALSE END;
  END;
(*<An1*)
  mark_tried(o);
(*An1>*)
  RETURN TRUE
END try_scp;

PROCEDURE try_qua*(scope: pc.STRUCT; name-: ARRAY OF CHAR;
                      VAR o: pc.OBJECT): BOOLEAN;
(** ищет среди экспортированных объектов
    указанной области видимости (модуль или запись)
*)
  VAR s: ESTRUCT;
BEGIN
  IF scope.obj#NIL THEN scope:=scope.obj.type END;
  s:=scope(ESTRUCT);
  IF s.mode=pc.ty_module THEN
    o:=s.find(name);
    IF (o=NIL) & (def_import#NIL) & (s=def_import.up) THEN
      o:=def_import.find(name);
    END;
    IF o=NIL THEN RETURN FALSE END;
    IF o.mode=ob_usage THEN o:=NIL; RETURN FALSE END;
    IF NOT (otag_exported IN o.tags) THEN o:=NIL; RETURN FALSE END;
    IF o.mode=ob_alias THEN
      o:=o.actual;
    ELSIF o.mode=ob_enum THEN
      o:=o.actual;
      IF s.mno=pc.cur_mod THEN o:=NIL; RETURN FALSE END;
    END;
(*<An1*)
    mark_tried(o);
(*An1>*)
    RETURN TRUE;
  END;
  LOOP
    o:=s.find(name);
    IF o=NIL THEN
      IF s.base=NIL THEN RETURN FALSE END;
      IF s.mode=ty_invtype THEN RETURN FALSE END;
      s:=s.base(ESTRUCT);
    ELSE
      ASSERT(o.mode IN pc.OB_Common);
(*<An1*)
      mark_tried(o);
(*An1>*)
      RETURN TRUE;
    END;
  END;
END try_qua;

PROCEDURE try_vis*(scope: pc.STRUCT; name-: ARRAY OF CHAR; VAR o: pc.OBJECT): BOOLEAN;
(** ищет в текущей и объемлющих оьластях видимости *)
  VAR s,c: ESTRUCT; r: pc.OBJECT; i: BOOLEAN;
BEGIN
  IF scope.obj#NIL THEN scope:=scope.obj.type END;
  c:=scope(ESTRUCT);
  s:=c;
  LOOP
    IF s=NIL THEN RETURN FALSE END;
    IF s.mode=ty_invtype THEN RETURN FALSE END;
    o:=s.find(name);
    IF (o=NIL) & (def_import#NIL) & (s=def_import.up) THEN
      o:=def_import.find(name);
    END;
    IF o#NIL THEN
      i:=s#c;
      IF o.mode=ob_usage THEN o:=o.next END;
      WHILE s#c DO
        r := new_obj(ob_usage);
        r.name:=o.name;
        r.next:=o;
        dcl(c,r);
        ASSERT(c.mode=pc.ty_proctype);
        c:=c.up(ESTRUCT);
      END;
      (* Имя алиаса может не совпадать с именем объекта! *)
      IF o.mode=ob_alias THEN
        o:=o.actual
      ELSIF o.mode=ob_enum THEN
        o:=o.actual;
        IF s.mno=pc.cur_mod THEN o:=NIL; RETURN FALSE END;
      END;
      IF i THEN INCL(o.marks,omark_threatened) END;
(*<An1*)
      mark_tried(o);
(*An1>*)
      RETURN TRUE;
    END;
    CASE s.mode OF
      |pc.ty_module  : RETURN try_scp(module_super.type,name,o);
      |pc.ty_proctype: s:=s.up(ESTRUCT);
    END;
  END;
END try_vis;

PROCEDURE dcl*(scope: pc.STRUCT; o: pc.OBJECT);
  VAR f: pc.OBJECT; fnm: env.String; l,c: LONGINT; t: pc.STRUCT;
BEGIN
  IF scope=NIL THEN scope:=cur_scope END;
  IF scope.obj#NIL THEN scope:=scope.obj.type END;
  IF (scope.mode=pc.ty_record) & (o.mode=pc.ob_field) &
     (scope.base#NIL) & try_qua(scope.base,o.name^,f)
  THEN
    IF o.pos.IsNull() THEN
      env.errors.Error(o.pos,28,o.name^);
    ELSE
      o.pos.unpack(fnm,l,c);
      env.errors.Error(o.pos,22,o.name^,fnm^,l+1,c+1);
    END;
  ELSE
    IF def & (scope.mode=pc.ty_module) & (scope.mno=pc.cur_mod) THEN
      IF o.mode IN pc.OB_SET{ob_alias,ob_usage} THEN
--        ASSERT((o.next.mno#pc.cur_mod) OR (o.next.mode=pc.ob_inv));
        IF def_import=NIL THEN
          t := new_type(pc.ty_module);
          def_import:=t(ESTRUCT);
          def_import.up:=scope(ESTRUCT);
        END;
        f:=def_import.append(o);
      ELSE
        f:=scope(ESTRUCT).append(o);
        INCL(o.tags,otag_exported);
      END;
    ELSIF scope.mode=pc.ty_proctype THEN
      f:=scope(ESTRUCT).append1(o);
    ELSE
      f:=scope(ESTRUCT).append(o);
    END;
    IF (f#NIL) THEN
      IF f.mode=ob_usage THEN
        o.pos.unpack(fnm,l,c);
        env.errors.Error(f.pos,26,f.name^,fnm^,l+1,c+1);
      ELSIF (f.mode#pc.ob_inv) & (o.mode#pc.ob_inv) THEN
        IF f.pos.IsNull() THEN
          env.errors.Error(o.pos,28,o.name^);
        ELSE
          f.pos.unpack(fnm,l,c);
          env.errors.Error(o.pos,22,o.name^,fnm^,l+1,c+1);
        END;
      END;
    END;
  END;
END dcl;

PROCEDURE fnd_scp*(scope: pc.STRUCT; name-: ARRAY OF CHAR; VAR o: pc.OBJECT);
BEGIN
  IF NOT try_scp(scope,name,o) THEN
    invis(name,o);
    IF scope.mno=pc.cur_mod THEN dcl(scope,o) END;
  END;
END fnd_scp;

PROCEDURE fnd_qua*(scope: pc.STRUCT; name-: ARRAY OF CHAR; VAR o: pc.OBJECT);
BEGIN
  IF NOT try_qua(scope,name,o) THEN
    invis(name,o);
    IF scope.mno=pc.cur_mod THEN dcl(scope,o) END;
  END;
END fnd_qua;

PROCEDURE fnd_vis*(scope: pc.STRUCT; name-: ARRAY OF CHAR; VAR o: pc.OBJECT);
BEGIN
  IF NOT try_vis(scope,name,o) THEN
    invis(name,o);
    IF scope.mno=pc.cur_mod THEN dcl(scope,o) END;
  END;
END fnd_vis;

PROCEDURE dcl_enum_consts(scope: pc.STRUCT; o: pc.OBJECT);
(** для перечислимого типа заносит ссылки на константы этого типа *)
  VAR r,m: pc.OBJECT;
BEGIN
  ASSERT(o.mode=pc.ob_type);
  ASSERT(o.type.mode=pc.ty_enum);
  IF NOT (otag_exported IN o.tags) THEN RETURN END;
  r:=o.type.prof;
  WHILE r#NIL DO
    IF NOT try_scp(scope,r.name^,m) THEN
      INCL(r.marks,omark_threatened);
      m :=new_obj(ob_alias);
      m.name:=r.name; m.actual:=r;
      INCL(m.tags,otag_exported);
      dcl(scope,m);
    END;
    r:=r.next;
  END;
END dcl_enum_consts;

PROCEDURE dcl_ref*(scope: pc.STRUCT; o: pc.OBJECT);
(** занесение ссылки на объект в область видимости,
    для перечислимого типа также заносит ссылки на константы этого типа
*)
  VAR r: pc.OBJECT;
BEGIN
  IF (o.mode=pc.ob_type) & (o.type.mode=pc.ty_enum) THEN
    dcl_enum_consts(scope,o);
  END;
  IF global OR (pcS.TS_ext() & (o.mode=pc.ob_module) & (scope.mode=pc.ty_module))  THEN
    IF try_scp(scope, o.name^, r) & (r = o) THEN RETURN END;
  END;
  INCL(o.marks,omark_threatened);
  ASSERT(o.mode IN pc.OB_Common);
  r := new_obj(ob_alias);
  r.name:=o.name;
  r.actual:=o;
  dcl(scope,r);
END dcl_ref;


PROCEDURE dcl_rename*(scope: pc.STRUCT; o: pc.OBJECT; nm-: ARRAY OF CHAR);
(** занесение ссылки на объект в область видимости с переименованием *)
  VAR r: pc.OBJECT;
BEGIN
  ASSERT(o.mode IN pc.OB_Common);
  r := new_obj(ob_alias);
  set_name(r,nm);
  r.actual:=o;
  dcl(scope,r);
END dcl_rename;

PROCEDURE dcl_exp*(scope: pc.STRUCT; name-: ARRAY OF CHAR; VAR o: pc.OBJECT);
(** экспорт объекта из текущей области видимости в другую *)
  VAR s: ESTRUCT; ps: pc.TPOS;
BEGIN
  ASSERT(cur_scope.mode=pc.ty_module);
  s:=cur_scope(ESTRUCT);
  o:=s.find(name);
  ASSERT(NOT def);
  ASSERT(def_import=NIL);
  IF o=NIL THEN
    cur_pos(ps);
    env.errors.Error(ps,25,name);
    RETURN;
  END;
  INCL(o.tags,otag_exported);
  IF o.mode=ob_alias THEN o:=o.actual END;
  IF omark_used_in_for IN o.marks THEN
    cur_pos(ps);
    env.errors.Error(o.pos,146)
  END;
  IF scope#NIL THEN dcl_ref(scope,o) END;
END dcl_exp;

PROCEDURE create_magic_visibility(scope: pc.STRUCT);
(** для описаных в scope перечислимых типов делает видимыми в scope
    константы этих типов
*)
  PROCEDURE list(o: pc.OBJECT);
  BEGIN
    WHILE o#NIL DO
      IF (o.mode=pc.ob_type) & (o.type.mode=pc.ty_enum) THEN
        dcl_enum_consts(scope,o);
      END;
      o:=o.next;
    END;
  END list;
BEGIN
  list(scope.prof);
  list(scope.mem);
END create_magic_visibility;

PROCEDURE ini_std_types;

  VAR ref: INTEGER;
    old_align : SHORTINT;

  PROCEDURE std(VAR t: pc.STRUCT; m: pc.TY_MODE; rf: INTEGER);
  BEGIN
    ASSERT(rf = ref); (* temporary to check that numbering is preserved *)
    t := new_type(m);
    t.mno:=module_super.mno;
    std_types[ref]:=t;
    t.lref:=ref;
    INC(ref);
  END std;

  PROCEDURE std_cp(VAR t: pc.STRUCT; pct: pc.STRUCT; rf: INTEGER);
  BEGIN
    ASSERT(rf = ref); (* temporary to check that numbering is preserved *)
    t := pct;
    t.mno:=module_super.mno;
    std_types[ref]:=t;
    t.lref:=ref;
    INC(ref);
  END std_cp;

  PROCEDURE std_set(VAR set: pc.STRUCT; len: LONGINT; inx_type: pc.STRUCT);
    VAR base: pc.STRUCT;
  BEGIN
    set.min := zero;
    set.max := pc.value.new (env.null_pos, cardinal);
    set.max.set_integer (len-1);
    set.len := len;
    set.inx := inx_type;
    base := new_type (pc.ty_range);
    base.pos := env.null_pos;
    base.base := shortcard; (* ISO requires unsigned type here! *)
    base.min  := set.min;
    base.max  := set.max;
    set.base  := base;
  END std_set;

BEGIN
  ref:=1;
  old_align := align;
  align := 0;
  IF std_types[1]=NIL THEN
    std(undef     ,ty_undef        ,1);
    std(invtype   ,ty_invtype      ,2);
    std_cp(void      , pc.void_type,      3);
    std_cp(shortcard , pc.shortcard_type, 4);
    std_cp(cardinal  , pc.cardinal_type,  5);
    std_cp(longcard  , pc.longcard_type,  6);
    std_cp(shortint  , pc.shortint_type,  7);
    std_cp(integer   , pc.integer_type,   8);
    std_cp(longint   , pc.longint_type,   9);
    std_cp(real      , pc.real_type,      10);
    std_cp(longreal  , pc.longreal_type,  11);
    std_cp(ld_real   , pc.ld_real_type,   12);
    std_cp(complex   , pc.complex_type,   13);
    std_cp(lcomplex  , pc.lcomplex_type,  14);
    std(boolean8  ,pc.ty_boolean   ,15);
    std(boolean32 ,pc.ty_boolean   ,16);
    std_cp(char      , pc.char_type      ,17);
    std(set16     ,pc.ty_set       ,18);
    std(set32     ,pc.ty_set       ,19);
    std(loc       ,pc.ty_loc       ,20);
    std(word      ,pc.ty_array     ,21);
    std(addr      ,pc.ty_pointer   ,22);
    std(proctype0 ,pc.ty_proctype  ,23);
    std(protection,pc.ty_protection,24);
    std_cp(AA_type   , pc.AA_type,  25);
    std_cp(CC_type   , pc.CC_type,  26);
    std_cp(ZZ_type   , pc.ZZ_type,  27);
    std_cp(RR_type   , pc.RR_type,  28);
    std(process   ,pc.ty_process   ,29);
    std(signed16  ,pc.ty_integer   ,30);
    std(unsigned16,pc.ty_cardinal  ,31);
    std(size_t16  ,pc.ty_cardinal  ,32);
    std(signed32  ,pc.ty_longint   ,33);
    std(unsigned32,pc.ty_longcard  ,34);
    std(size_t32  ,pc.ty_longcard  ,35);
    std(lens_t16  ,pc.ty_range     ,36);
    std(lens_t32  ,pc.ty_range     ,37);
    std(boolean16 ,pc.ty_boolean   ,38);
    std(set8      ,pc.ty_set       ,39);

    std_cp(longlongint , pc.longlongint_type, 40);
    std(word2     ,pc.ty_array     ,41);
    std(word4     ,pc.ty_array     ,42);
    std_cp(longlongcard , pc.longlongcard_type, 43);
    std(set64     ,pc.ty_set       ,44);
  END;

  IF pc.code.int16 THEN
    signed:=signed16; unsigned:=unsigned16;
  ELSE
    signed:=signed32; unsigned:=unsigned32;
  END;
  IF pc.code.index16   THEN
    index:=cardinal; size_t:=size_t16; lens_type:=lens_t16;
  ELSE
    index:=longcard; size_t:=size_t32; lens_type:=lens_t32;
  END;
  IF pc.code.address16 THEN difadr:=integer ELSE difadr:=longint END;
  bitset:=set32;
--  bitset64:=set64;
  IF env.config.Option("M2BASE16") THEN
    m2_int:=integer; m2_card:=cardinal;
    IF NOT oberon THEN bitset:=set16 END;
  ELSE
    m2_int:=longint; m2_card:=longcard;
  END;

  INCL(signed16.tags,pc.ttag_c_type);
  INCL(unsigned16.tags,pc.ttag_c_type);
  INCL(size_t16.tags,pc.ttag_c_type);
  INCL(signed32.tags,pc.ttag_c_type);
  INCL(unsigned32.tags,pc.ttag_c_type);
  INCL(size_t32.tags,pc.ttag_c_type);
  invtype.base:=invtype;
  invtype.inx:=invtype;
  invtype.flag:=pc.flag_m2;
  inv_obj := new_obj(pc.ob_inv);
  inv_obj.type:=invtype;
  invtype.obj:=inv_obj;
  addr.base:=loc;
  addr.flag:=pc.flag_m2;

  set_type_base(word,loc);
  word.inx := new_type(pc.ty_range);
  word.inx.base:=index;
  word.inx.min:=cardinal.min;
  word.inx.max:=pc.value.new(env.null_pos,cardinal);
  word.inx.max.set_integer(pc.code.locs_per_word-1);
  word.min:=word.inx.min;
  word.max:=word.inx.max;
  word.len:=pc.code.locs_per_word;

  set_type_base(word2,loc);
  word2.inx := new_type(pc.ty_range);
  word2.inx.base:=index;
  word2.inx.min:=cardinal.min;
  word2.inx.max:=pc.value.new(env.null_pos,cardinal);
  word2.inx.max.set_integer(1);
  word2.min:=word2.inx.min;
  word2.max:=word2.inx.max;
  word2.len:=2;

  set_type_base(word4,loc);
  word4.inx := new_type(pc.ty_range);
  word4.inx.base:=index;
  word4.inx.min:=cardinal.min;
  word4.inx.max:=pc.value.new(env.null_pos,cardinal);
  word4.inx.max.set_integer(3);
  word4.min:=word4.inx.min;
  word4.max:=word4.inx.max;
  word4.len:=4;


  (* set 8, 16, 32 *)
  std_set (set8,   8, shortcard);
  std_set (set16, 16, cardinal);
  std_set (set32, 32, longcard);
  std_set (set64, 64, longlongcard);

  proctype0.base:=void;
  proctype0.flag:=pc.flag_m2;

  boolean8.base:=shortcard;
  boolean16.base:=cardinal;
  boolean32.base:=longcard;
  boolean:=boolean8;

  char.base:=shortcard;
  lens_t16.base:=size_t16;
  lens_t16.min:=one;
  lens_t16.max:=size_t16.max;
  lens_t32.base:=size_t32;
  lens_t32.min:=one;
  lens_t32.max:=size_t32.max;
  protection.base:=integer;

  align := old_align;
END ini_std_types;

(*------------------------ Sym. files ----------------------------*)

(*
        В сим-файл пишется все, что доступно из CU прослеживанием всех
        ссылок, кроме .mem
        Некоторые объекты при этом становятся 'public', а именно:
        'exported' AND CU +
        ( 'ob_type' : доступны из 'exported' AND CU )

        Сначала пишутся "крючки", затем объекты и типы (в том числе модули).
*)


CONST
  VERS = 24; (* sym file version *)

VAR
  (** symfile read/write additional information *)
  m2cmp     : BOOLEAN;    (** Should Modula .sym files be compared or always rewritten *)
  changesym : BOOLEAN;    (** Is it allowed to overwrite existing .sym files           *)
  SYM_ext   : env.String; (** SYM files extension                                      *)

CONST
  ref_first* =LEN(std_types);

CONST
  sym_type*     = 1;
  sym_hook*     = 2;
  sym_object*   = 3;
  sym_min*      = 4;
  sym_max*      = 5;
  sym_val*      = 6;
  sym_ref*      = 7;
  sym_case*     = 8;
  sym_end*      = 9;
  sym_info*     = 10;
  sym_tag*      = 11;
  sym_prof*     = 12;
  sym_mem*      = 14;
  sym_expr*     = 16;
  sym_module*   = 17;
  sym_incomp*   = 18;
  sym_align*    = 19;
  sym_attr      = 20;
  sym_attr_expr = 21;

  sym_magic     = 4F4D53H;

PROCEDURE resize_types_buf(n: LONGINT);
  VAR t: TYPES_BUF; ln,i: LONGINT;
BEGIN
  ln := LEN(types^);
  IF n<ln THEN RETURN END;
  REPEAT ln := ln*2 UNTIL ln > n ;
  NEW(t,ln);
  FOR i:=0 TO LEN(types^)-1 DO t[i]:=types[i] END;
  types:=t;
END resize_types_buf;

--------------------------------------------------------------------------------
PROCEDURE resize_mods *();
  VAR
    j: pc.Mno;
    m: pc.MnoOBJECTS;
BEGIN
   NEW(m,ORD(pc.mod_cnt)*2);
  FOR j:=pc.ZEROMno TO PRED(pc.mod_cnt) DO
    m[j]:=pc.mods[j]
  END;
  pc.mods:=m;
END resize_mods;

PROCEDURE QuickSort (VAR a: ARRAY OF pc.OBJECT; l, r: LONGINT);
  VAR i, j: LONGINT;
      x, w: pc.OBJECT;
BEGIN
  i := l;
  j := r;
  x := a [(l + r) DIV 2];
  REPEAT
    WHILE a[i].name^ < x.name^ DO
      INC (i);
    END;
    WHILE x.name^ < a[j].name^ DO
      DEC (j);
    END;
    IF i <= j THEN
      w     := a [i];
      a [i] := a [j];
      a [j] := w;
      INC (i);
      DEC (j);
    END;
  UNTIL i > j;
  IF l < j THEN
    QuickSort (a, l, j);
  END;
  IF i < r THEN
    QuickSort (a, i, r);
  END;
END QuickSort;

PROCEDURE sort_list(VAR o: pc.OBJECT);
  VAR i, n: LONGINT;
      l:    pc.OBJECT;
      p:    POINTER TO ARRAY OF pc.OBJECT;
BEGIN
  l := o;
  n := 0;
  WHILE l#NIL DO
    INC (n);
    l := l.next;
  END;
  IF n > 0 THEN
    NEW (p, n);
    DEC (n);
    l := o;
    FOR i:=0 TO n DO
      p^[i] := l;
      l := l.next;
    END;
    QuickSort (p^, 0, n);
    FOR i:=0 TO n-1 DO
      p^[i].next := p^[i+1];
    END;
    p^[n].next := NIL;
    o := p^[0];
  END;
END sort_list;


PROCEDURE wr_obj(o :pc.IROBJECT);
BEGIN
  IF o = NIL THEN
    file.WriteInt(-1);
  ELSE
    WITH o :pc.OBJECT DO
      ASSERT(o.type.obj=o);
      file.WriteInt(o.type.lref);
      ASSERT(o.type.lref>0);
    END;
  END;
END wr_obj;


PROCEDURE rd_obj() :pc.IROBJECT;
  VAR i: LONGINT; o: pc.OBJECT;
BEGIN
  file.ReadInt(i);
  IF i = -1 THEN
    RETURN NIL
  ELSE
    DEC(i,LEN(std_types));
    ASSERT(types[i]#NIL);
    o:=types[i].obj;
    ASSERT(o.type.obj=o);
    RETURN o;
  END;
END rd_obj;


PROCEDURE out_sym_file*;

  VAR
    types_no : INTEGER;
    hook_nm  : ARRAY 4 OF pc.OBJECT;

  PROCEDURE out_ref(s: pc.STRUCT);
  BEGIN
    IF s#NIL THEN file.WriteInt(s.lref); ASSERT(s.lref>0);
    ELSE file.WriteInt(0);
    END;
  END out_ref;

  PROCEDURE out_hook(o: pc.OBJECT);
    VAR nm: ARRAY 4 OF pc.OBJECT; no,hn: INTEGER;
  BEGIN
    file.Write(sym_hook);
    no:=0;
    LOOP
      nm[no]:=o; INC(no);
      ASSERT(o.name^#"");
      IF o.host=NIL THEN EXIT END;
      ASSERT(o(EOBJECT).scp=o.host);
      o:=o.host.obj;
    END;
    file.WriteInt(no); hn:=0;
    REPEAT
      DEC(no);
      IF hook_nm[hn]=nm[no] THEN file.WriteString("");
      ELSE hook_nm[hn]:=nm[no]; file.WriteString(nm[no].name^);
      END;
      INC(hn);
    UNTIL no=0;
    out_ref(nm[0].type);
  END out_hook;

  PROCEDURE out_val(v: pc.VALUE; tag: SHORTINT);
  BEGIN
    IF v#NIL THEN
      IF tag>=0 THEN file.Write(tag) END;
      v.out(file,wr_obj);
    END;
  END out_val;

  PROCEDURE make_public(o: pc.OBJECT);
    (* VAR p: pc.OBJECT; *)
  BEGIN
    IF pc.otag_public IN o.tags THEN RETURN END;
    IF (o.host=NIL) OR (o.host.mode#pc.ty_module) THEN RETURN END;
    env.errors.Error(o.pos,157)
    (* can not make object public:
       back-end method allocate was already called!
    ASSERT(o.lev=0);
    ASSERT(o.mno=pc.cur_mod);
    IF o.host.mem=o THEN
      o.host.mem:=o.next;
    ELSE
      p:=o.host.mem;
      WHILE p.next#o DO p:=p.next END;
      p.next:=o.next;
    END;
    o.next:=o.host.prof;
    o.host.prof:=o;
    INCL(o.tags,pc.otag_public);
    *)
  END make_public;

  PROCEDURE gather_type(tt: pc.STRUCT);

    PROCEDURE gather_value(v: pc.VALUE; t: pc.STRUCT);
      VAR w: pc.VALUE; o: pc.OBJECT; i: LONGINT;
    BEGIN
      ASSERT(v # NIL);
      CASE t.mode OF
        |pc.ty_proctype:
          o:=v.get_object();
          IF o # NIL THEN
            ASSERT(o.type.obj=o);
            gather_type(o.type);
          END;
        |pc.ty_array   :
          IF t.base.mode IN pc.TY_SET{pc.ty_proctype,pc.ty_array,pc.ty_record} THEN
            w:=pc.value.new(v.pos,t.base);
            FOR i:=0 TO t.len-1 DO
              w.index_get(i,v);
              gather_value(w,t.base);
            END;
          END;
        |pc.ty_record  :
          (* !!!!!!! *)
      ELSE
      END;
    END gather_value;

    PROCEDURE gather_node(n: pc.NODE);
    BEGIN
      IF n=NIL THEN RETURN END;
      gather_type(n.type);
      gather_node(n.next);
      gather_node(n.l);
      gather_node(n.r);
      IF n.mode = pc.nd_value THEN gather_value(n.val,n.type) END;
    END gather_node;

    PROCEDURE gather_object(o: pc.OBJECT);
      VAR f: pc.OBJECT; n: pc.NODE;
    BEGIN
      make_public(o);
      gather_type(o.type);
      gather_type(o.host);
      IF o IS EOBJECT THEN gather_type(o(EOBJECT).scp) END;
      IF o.mode=pc.ob_header THEN
        n:=o.val.l;
        WHILE n#NIL DO
          gather_type(n.type);
          f:=n.obj;
          WHILE f#NIL DO gather_object(f); f:=f.next END;
          n:=n.next;
        END;
      ELSIF (o.val#NIL) & NOT (o.mode IN pc.PROCs+pc.OB_SET{pc.ob_module}) THEN
        gather_node(o.val);
      END;
    END gather_object;

    VAR o: pc.OBJECT; t: ESTRUCT;
  BEGIN
    IF tt=NIL THEN RETURN END;
    IF tt.lref>0 THEN RETURN END;
    t:=tt(ESTRUCT);
    IF (t.obj#NIL) & (t.obj.type#t) THEN
      (* is copy of another type *)
      IF t.obj.type.lref>0 THEN
        t.lref:=t.obj.type.lref;
        RETURN;
      END;
      t.lref:=types_no+ref_first;
      t:=t.obj.type(ESTRUCT);
    END;
    IF types_no>=LEN(types^) THEN resize_types_buf(types_no) END;
    types[types_no]:=t; t.lref:=types_no+ref_first; INC(types_no);
    IF t.obj#NIL THEN
      IF t.obj.type=t THEN out_hook(t.obj) END;
      gather_object(t.obj);
    END;
    gather_type(pc.mods[t.mno].type);
    gather_type(t.base);
    gather_type(t.inx);
    IF (t.mode#pc.ty_module) OR (t.mno=pc.cur_mod) THEN
      o:=t.prof;
      WHILE o#NIL DO gather_object(o); o:=o.next END;
      IF t.mode=pc.ty_record THEN
        o:=t.mem;
        WHILE o#NIL DO gather_object(o); o:=o.next END;
      END;
    END;
  END gather_type;

  PROCEDURE ^out_object(o: pc.OBJECT);
  PROCEDURE out_expr(n: pc.NODE);
  BEGIN
    IF n=NIL THEN file.Write(pc.nd_inv); RETURN END;
    file.Write(n.mode);
    file.Write(n.sub);
    file.WriteSet(SYSTEM.VAL(SET,n.tags));
    out_expr(n.next);
    out_ref(n.type);
    file.Write(n.obj#NIL);
    IF (n.obj#NIL) &
       ( (n.obj.type#n.type) OR
         (n.type.obj=NIL) OR
         (n.type.obj.type#n.type)
       )
    THEN
      env.errors.Error(n.pos,87)
    END;
    out_expr(n.l);
    out_expr(n.r);
    IF n.val#NIL THEN out_val(n.val,1) ELSE file.Write(0) END;
  END out_expr;

  PROCEDURE out_object(o: pc.OBJECT);
  BEGIN
    file.Write(sym_object);
    ASSERT(o.mode#pc.ob_inv);
    file.WriteInt(ORD(o.mode));
    file.WriteInt(ORD(o.flag));
    file.WriteTPOS(o.pos);
    ASSERT(o.type#NIL);
    out_ref(o.type);
    ASSERT((o.host#NIL) OR (o.mode=pc.ob_module));
    file.WriteString(o.name^);
    file.WriteSet(SYSTEM.VAL(SET, o.tags));
    IF o IS EOBJECT THEN
      out_ref(o(EOBJECT).scp);
      ASSERT((o(EOBJECT).scp#def_import) OR (def_import=NIL));
    ELSE
      out_ref(pc.mods[o.mno].type);
    END;
    -- variable part
    IF o.type.obj=o THEN file.Write(sym_ref) END; -- must be first
    IF (o.val#NIL) & NOT (o.mode IN pc.PROCs+pc.OB_SET{pc.ob_module}) THEN
      IF o.val.mode=pc.nd_value THEN out_val(o.val.val,sym_val);
      ELSE file.Write(sym_expr); out_expr(o.val);
      END;
    END;
    IF (o.attr#NIL) & NOT (o.mode IN pc.PROCs+pc.OB_SET{pc.ob_module}) THEN
      IF o.attr(pc.NODE).mode=pc.nd_value THEN out_val(o.attr(pc.NODE).val,sym_attr);
      ELSE file.Write(sym_attr_expr); out_expr(o.attr(pc.NODE));
      END;
    END;
    -- we always write backend information.
    -- this is not very good decision, but the matter is that
    -- pc.code.out_object has side-effects and thus should be always
    -- called
    file.Write(sym_info);
    pc.code.out_object(file,o);
    file.Write(sym_end);
  END out_object;

  PROCEDURE out_list(o: pc.OBJECT;
                     tag: SHORTINT;
                     host: pc.STRUCT;
                     all: BOOLEAN);
    VAR n,m: pc.NODE;
  BEGIN
    IF o=NIL THEN RETURN END;
    file.Write(tag);
    WHILE o#NIL DO
      IF o.mode=pc.ob_header THEN
        file.Write(sym_tag);
        out_ref(o.type);
        ASSERT(o.val.mode=pc.nd_case);
        IF o.val.obj#NIL THEN
          ASSERT((o.val.obj.host=host) & (host#NIL));
          out_object(o.val.obj);
        END;
        n:=o.val.l;
        WHILE n#NIL DO
          ASSERT(n.mode=pc.nd_node);
          out_list(n.obj,sym_case,host,TRUE);
          IF n.obj = NIL THEN                          (*!! Ned empty variant *)
            file.Write(sym_case); file.Write(sym_end);
          END;
          m:=n.l;
          WHILE m#NIL DO
            IF m.l#NIL THEN
              out_val(m.l.val,sym_max);
              out_val(m.val,-1);
            ELSE
              out_val(m.val,sym_min);
            END;
            m:=m.next;
          END;
          n:=n.next;
        END;
        file.Write(sym_end);
      ELSIF all OR
            (o.type.obj=o) &
            (o.type.lref>0)
      THEN
        ASSERT((o.host=host) & (host#NIL));
        out_object(o);
      END;
      o:=o.next;
    END;
    file.Write(sym_end);
  END out_list;

  PROCEDURE out_struct(type: pc.STRUCT);
  BEGIN
    file.Write(sym_type);
    ASSERT(type.mode#ty_undef);
    file.WriteInt(ORD(type.mode));
    file.WriteInt(ORD(type.flag));
    out_ref(pc.mods[type.mno].type);
    out_ref(type.base);
    out_ref(type.inx);
    IF type=pc.mods[pc.cur_mod].type THEN
      (* version for module being compiled should not be checked because if   *)
      (* this is only difference then .sym file are equal to its old version  *)
      file.WriteTag(type.len);
    ELSE
      file.WriteInt(type.len);
    END;
    file.WriteInt(type.num);
    file.WriteSet(SYSTEM.VAL(SET, type.tags));
    -- variable part
    IF type.obj#NIL THEN file.Write(sym_ref); out_ref(type.obj.type) END;
    out_val(type.min,sym_min);
    out_val(type.max,sym_max);
    IF (type.mode#pc.ty_module) OR (type.mno=pc.cur_mod) THEN
      out_list(type.prof,sym_prof,type,TRUE);
      IF type.mode=pc.ty_record THEN out_list(type.mem,sym_mem,type,TRUE) END;
    ELSE
      file.Write(sym_incomp);
      out_list(type.prof,sym_prof,type,FALSE);
    END;
    IF type.align # 0 THEN file.Write(sym_align); file.Write(type.align) END;
    IF type.ext#NIL THEN file.Write(sym_info); type.ext.out(file) END;
    file.Write(sym_end);
  END out_struct;

  PROCEDURE write_import;
  VAR use : pc.USAGE;
      count: SHORTINT;
  BEGIN
   (* IF ~pcS.TS_ext() THEN RETURN; END; *)
    use := pc.mods[pc.cur_mod].type.use;
    count := 0;
    WHILE use # NIL DO
      IF use.obj.mode=pc.ob_module THEN
         INC(count);
      END;
      use := use.next;
    END;
    file.Write(count);

    use := pc.mods[pc.cur_mod].type.use;
    WHILE use # NIL DO
     IF use.obj.mode=pc.ob_module THEN
       file.WriteString(use.obj.name^);
     END;
     use := use.next;
    END;
  END write_import;

VAR
  i : INTEGER;
  t : pc.STRUCT;
  fnm, err_str: pc.STRING;
  need_compare_symfiles, do_compare_symfiles: BOOLEAN;
  can_overwrite, updated: BOOLEAN;

BEGIN
  IF global & NOT need_syms THEN RETURN END;
  xfs.sys.Create("",pc.mods[pc.cur_mod].name^, SYM_ext^,fnm);
  xfs.sys.UseFirst(fnm^,fnm);

  need_compare_symfiles := oberon OR m2cmp;
  do_compare_symfiles := need_compare_symfiles & xfs.sys.Exists(fnm^);

  xfs.sym.Open(fnm^, TRUE, do_compare_symfiles);
  IF xfs.sym.file=NIL THEN
    env.errors.Fault(env.null_pos, 424, xfs.sym.msg^);
  END;
  file:=xfs.sym.file(xfs.SymFile);

  NEW(types,200);
  FOR i:=0 TO LEN(hook_nm)-1 DO hook_nm[i]:=NIL END;
  IF need_compare_symfiles THEN sort_list(pc.mods[pc.cur_mod].type.prof) END;
  file.WriteInt(sym_magic);
  file.WriteInt(VERS);
  file.WriteInt(pc.code.sym_ident + ident_gap);
  types_no:=0;
  gather_type(pc.mods[pc.cur_mod].type);
  FOR i:=0 TO types_no-1 DO
    t:=types[i];
    out_struct(t);
    IF t.mode=pc.ty_module THEN
      file.Write(sym_module);
      out_ref(t);
      out_object(t.obj);
    END;
  END;
  file.Write(sym_end);
  write_import;
  FOR i:=0 TO types_no-1 DO types[i].lref:=0 END;
  can_overwrite := changesym OR NOT (need_compare_symfiles);
  can_overwrite := can_overwrite & (env.errors.err_cnt=0);
  updated := file.CloseNew(can_overwrite, err_str);

  IF err_str = NIL THEN (* means no error was occured *)
    env.info.newSF := updated;
    IF NOT file.new THEN
      pc.mods[pc.cur_mod].type.len:=file.tag; (* update file version in tree representation *)
    ELSIF env.errors.err_cnt#0 THEN        (* nothing *)
    ELSIF NOT updated THEN
      (* Only way to get updated FALSE at this point is        *)
      (* to have updated file contents and can_overwrite FALSE *)
      env.errors.Fault(env.null_pos,193)
    END;
  ELSE
    env.errors.Fault(env.null_pos, 448, err_str^); -- file write error
  END;
END out_sym_file;

PROCEDURE inp_sym_file*(imp_name-: ARRAY OF CHAR;
                        my_sym: BOOLEAN;
                        browse: BOOLEAN;
                        VAR head: pc.OBJECT);
(** my_sym - implementation module
    browse - objects will be used for browsing only. In this mode the
             procedure does not checks that imp_name == module name.
*)

  VAR
    ref_no : INTEGER;
    hook_nm: ARRAY 4 OF pc.NAME;
    hook_md: pc.OBJECT;

    dummy_type  : pc.STRUCT;
    dummy_type0 : pc.STRUCT;
    dummy_object: EOBJECT;
    dummy_node  : pc.NODE;
    pos         : pc.TPOS;
    sym_ident   : LONGINT;

  PROCEDURE Assert(i: INTEGER);
  BEGIN
    ASSERT(file#NIL);
    env.errors.Fault(pos,197,i,file.name^);
  END Assert;

  PROCEDURE inp_ref(VAR s: pc.STRUCT);
    VAR i: LONGINT;
  BEGIN
    file.ReadInt(i);
    IF i<0 THEN Assert(1) END;
    IF i<LEN(std_types) THEN
      IF (i#0) & (std_types[i]=NIL) THEN Assert(2) END;
      s:=std_types[i]; RETURN;
    END;
    DEC(i,LEN(std_types));
    IF i>=LEN(types^) THEN resize_types_buf(i) END;
    IF types[i]=NIL THEN types[i] := new_type(ty_undef) END;
    s:=types[i];
  END inp_ref;

  PROCEDURE inp_name(VAR n: pc.STRING);
    VAR s: pc.NAME;
  BEGIN
    file.ReadString(s);
    NEW(n,LENGTH(s)+1);
    COPY(s,n^);
  END inp_name;

  PROCEDURE inp_value(VAR v: pc.VALUE);
  BEGIN
    v:=pc.value.inp(file,rd_obj);
  END inp_value;

  PROCEDURE ^inp_object(VAR o: pc.OBJECT;
                       skip: BOOLEAN;
                       lev: SHORTINT;
                       host: pc.STRUCT;
                       VAR no: BOOLEAN);

  PROCEDURE ^inp_hook;

  PROCEDURE inp_expr(VAR n: pc.NODE; skip: BOOLEAN);
  VAR
    s: pc.ND_MODE;
    no:BOOLEAN;
  BEGIN
    file.Read(s);
    IF s=pc.nd_inv THEN n:=NIL; RETURN END;
    IF skip THEN n:=dummy_node ELSE NEW(n) END;
    n.pos:=env.null_pos;
    n.mode:=s;
    file.Read(n.sub);
    file.ReadSet(SYSTEM.VAL(SET, n.tags));
    inp_expr(n.next,skip);
    inp_ref(n.type);
    file.Read(s);
    IF s # pc.nd_inv THEN n.obj:=n.type.obj END;
    inp_expr(n.l,skip);
    inp_expr(n.r,skip);
    file.Read(s);
    IF s # pc.nd_inv THEN inp_value(n.val) END;
  END inp_expr;

  PROCEDURE inp_object(VAR o: pc.OBJECT;
                       skip: BOOLEAN;
                       lev: SHORTINT;
                       host: pc.STRUCT;
                       VAR no: BOOLEAN);
    VAR
      scp       : pc.STRUCT;
      i         : LONGINT;
      tag       : SHORTINT;
      nm        : pc.NAME;
      v         : pc.VALUE;
      a         : pc.OBJECT;
      skip_i    : BOOLEAN;
      n         : pc.NODE;
      dum_tpos  : pc.TPOS;
  BEGIN
    no:=TRUE;
    file.ReadInt(i);
(*    IF (i<0) OR (i>pc.ob_free) THEN Assert(3) END; !!!!! *)
    IF NOT skip THEN
      o := new_obj(VAL(pc.OB_MODE,i)); skip_i:=FALSE;
    ELSE
      o:=dummy_object; o.mode:=VAL(pc.OB_MODE,i); skip_i:=TRUE;
    END;
    o.lev:=lev;
    o.mno:=host.mno;
    o.host:=host;
    file.ReadInt(i);
    o.flag:=VAL(pc.Lang,i);
    IF o.mode # pc.ob_module THEN
      file.ReadTPOS(o.pos);
    ELSE
      file.ReadTPOS(dum_tpos);
    END;
    inp_ref(o.type);
    IF skip THEN file.ReadString(nm) ELSE inp_name(o.name) END;
    file.ReadSet(SYSTEM.VAL(SET, o.tags));
    inp_ref(scp);
    IF o.type=NIL THEN Assert(4) END;
    LOOP
      file.Read(tag);
      CASE tag OF
        |sym_val      : inp_value(v);
                        IF NOT skip_i THEN
                          new(o.val,pc.nd_value);
                          o.val.type:=o.type;
                          o.val.val:=v;
                        END;
        |sym_attr    : inp_value(v);
                        IF NOT skip_i THEN
                          new(o.attr(pc.NODE),pc.nd_value);
                          o.attr(pc.NODE).type:=o.type;
                          o.attr(pc.NODE).val:=v;
                        END;
        |sym_expr     : inp_expr(n,skip_i);
                        IF NOT skip_i THEN o.val:=n END;
        |sym_attr_expr: inp_expr(n,skip_i);
                        IF NOT skip_i THEN o.attr:=n END;
        |sym_info     : IF skip_i THEN pc.code.skip_object(file,sym_ident);
                        ELSE pc.code.inp_object(file,o,sym_ident);
                        END;
        |sym_object   : IF NOT skip_i THEN
                          new(o.val,pc.nd_proc);
                          o.val.type:=o.type;
                          o.val.obj:=o.type.obj;
                        END;
        |sym_ref      : IF NOT skip THEN
                          IF o.type.obj=NIL THEN Assert(5) END;
                          IF o.type.obj.mode#pc.ob_inv THEN
                            skip_i:=TRUE; no:=FALSE;
                          ELSE
                            IF o.next#NIL THEN Assert(6) END;
                            IF o.type.obj.next#NIL  THEN Assert(7) END;
                            IF o=o.type.obj THEN Assert(8) END;
                            SYSTEM.MOVE(o,o.type.obj,SIZE(o^));
                          END;
                          o:=o.type.obj; scp:=NIL;
                        END;
        |sym_end      : EXIT;
      ELSE Assert(9);
      END;
    END;
    IF skip THEN o:=NIL; RETURN END;
    IF scp#NIL THEN
      a:=scp(ESTRUCT).append(o);
      IF a#NIL  THEN Assert(10) END;
    END;
  END inp_object;

  PROCEDURE inp_list(VAR list: pc.OBJECT;
                     host: pc.STRUCT;
                     skip: BOOLEAN;
                     lev: SHORTINT);
    VAR
      n,m,ntail,vtail: pc.NODE;
      tail,o,p  : pc.OBJECT;
      tag       : SHORTINT;
      v,v1      : pc.VALUE;
      t         : pc.STRUCT;
      no        : BOOLEAN;
  BEGIN
    IF list#NIL THEN
      tail:=list;
      WHILE tail.next#NIL DO tail:=tail.next END;
    ELSE
      tail:=NIL;
    END;
    LOOP
      file.Read(tag);
      CASE tag OF
        |sym_end   : EXIT;
        |sym_object:
          inp_object(o,skip,lev,host,no);
          IF NOT skip & no THEN
            IF o.next#NIL THEN Assert(11) END;
            IF tail=NIL THEN list:=o ELSE tail.next:=o END;
            tail:=o;
          END;
        |sym_tag  :
          inp_ref(t);
          IF NOT skip THEN
            o := new_obj(pc.ob_header);
            o.host:=host;
            o.mno:=host.mno;
            IF tail=NIL THEN list:=o ELSE tail.next:=o END;
            tail:=o; o.type:=t;
            new(o.val,pc.nd_case);
          ELSE
            o:=NIL;
          END;
          n:=NIL;
          LOOP
            file.Read(tag);
            CASE tag OF
              |sym_object:
                inp_object(p,skip,lev,host,no);
                IF ~(no OR skip) THEN Assert(12) END;
                IF NOT skip THEN o.val.obj:=p END;
              |sym_min   :
                inp_value(v);
                IF NOT skip THEN
                  new(m,pc.nd_value);
                  m.type:=o.type; m.val:=v;
                  IF n=NIL THEN Assert(13) END;
                  app(n.l,vtail,m);
                END;
              |sym_max   :
                inp_value(v); inp_value(v1);
                IF NOT skip THEN
                  new(m,pc.nd_pair); m.type:=o.type;
                  new(m.l,pc.nd_value); m.l.type:=o.type;
                  m.l.val:=v; m.val:=v1;
                  IF n=NIL THEN Assert(14) END;
                  app(n.l,vtail,m);
                END;
              |sym_case  :
                p:=NIL;
                inp_list(p,host,skip,lev);
                IF NOT skip THEN
                  new(n,pc.nd_node); n.type:=o.type;
                  app(o.val.l,ntail,n); n.obj:=p;
                END;
              |sym_end   : EXIT;
            ELSE Assert(15);
            END;
          END;
      ELSE Assert(16);
      END;
    END;
  END inp_list;

  PROCEDURE inp_struct;
    VAR
      i         : LONGINT;
      old,type  : pc.STRUCT;
      t         : pc.STRUCT;
      skip      : BOOLEAN;
      lev,tag   : SHORTINT;
  BEGIN
    IF ref_no>=LEN(types^) THEN resize_types_buf(ref_no) END;
    IF types[ref_no]=NIL   THEN types[ref_no] := new_type(ty_undef) END;
    type:=types[ref_no]; INC(ref_no);
    skip:=FALSE; old:=type;
    IF type.mode#ty_undef THEN
      SYSTEM.MOVE(dummy_type0,dummy_type,SIZE(dummy_type^));
      type:=dummy_type;
      skip:=TRUE;
    END;
    type.pos:=env.null_pos;
    type.end:=env.null_pos;
    file.ReadInt(i);
    type.mode:=VAL(pc.TY_MODE,i);
    file.ReadInt(i);
    type.flag:=VAL(pc.Lang,i);
    inp_ref(t);
    type.mno:=t.mno;
    inp_ref(type.base);
    inp_ref(type.inx);
    file.ReadInt(type.len);
    file.ReadInt(type.num);
    file.ReadSet(SYSTEM.VAL(SET, type.tags));
    IF type.mode=pc.ty_proctype THEN lev:=1 ELSE lev:=0 END;
    LOOP
      file.Read(tag);
      CASE tag OF
        |sym_min   : inp_value(type.min);
        |sym_max   : inp_value(type.max);
        |sym_prof  : IF skip &
                        (old.mode=pc.ty_module) &
                        (ttag_incomplete IN old.tags)
                     THEN
                       inp_list(old.prof,old,FALSE,lev);
                     ELSE
                       inp_list(type.prof,type,skip,lev);
                     END;
        |sym_mem   : inp_list(type.mem,type,skip,lev);
        |sym_incomp: INCL(type.tags,ttag_incomplete);
                     IF type.mode#pc.ty_module THEN Assert(17) END;
        |sym_info  : IF skip THEN pc.code.skip_struct(file,sym_ident);
                     ELSE pc.code.inp_struct(file,type,sym_ident);
                     END;
        |sym_ref   : inp_ref(t);
                     IF t.obj=NIL THEN Assert(18) END;
                     type.obj:=t.obj;
        |sym_align : file.Read(type.align);
        |sym_end   : EXIT;
      ELSE Assert(19);
      END;
    END;
    IF skip & (old.mode=pc.ty_module) THEN
      IF old.len#type.len THEN
        env.errors.Fault(pos,192,old.obj.name^);
      END;
      IF NOT (ttag_incomplete IN type.tags) THEN
        EXCL(old.tags,ttag_incomplete);
      END;
    END;
    IF ~ (skip OR (type.mode#pc.ty_enum) OR
         (type.obj#NIL) & (type.obj.type.obj=type.obj))
    THEN Assert(20)
    END;
  END inp_struct;

  PROCEDURE inp_hook;

    PROCEDURE cre(VAR o: pc.OBJECT; nm-: ARRAY OF CHAR);
    BEGIN
      o := new_obj(pc.ob_inv);
      o.type := new_type(ty_undef);
      o.type.obj:=o;
      set_name(o,nm);
    END cre;

    VAR
      nm,mod_nm: pc.NAME;
      no,i,ref: LONGINT;
      o,p: pc.OBJECT;
      m: pc.Mno;
  BEGIN
    file.ReadInt(no);
    file.ReadString(mod_nm);
    FOR i:=0 TO no-2 DO
      file.ReadString(nm);
      IF nm#"" THEN hook_nm[i]:=nm END;
    END;
    file.ReadInt(ref); DEC(ref,LEN(std_types));
    IF ref>=LEN(types^) THEN resize_types_buf(ref) END;
    IF mod_nm#"" THEN
      m:=pc.ZEROMno;
      WHILE (m<pc.mod_cnt) &
            ((pc.mods[m]=NIL) OR (pc.mods[m].name^#mod_nm))
      DO INC(m);
      END;
      IF m>=pc.mod_cnt THEN
        IF pc.mod_cnt>=LEN(pc.mods^) THEN resize_mods END;
        cre(hook_md,mod_nm);
        pc.mods[pc.mod_cnt]:=hook_md;
        hook_md.type.mno:=pc.mod_cnt;
        INC(pc.mod_cnt);
      ELSE
        hook_md:=pc.mods[m];
      END;
    END;
    o:=hook_md;
    FOR i:=0 TO no-2 DO
      p:=o.type(ESTRUCT).find(hook_nm[i]);
      IF p=NIL THEN
        cre(p,hook_nm[i]);
        EVAL(o.type(ESTRUCT).append(p));
      END;
      o:=p;
    END;
    types[ref]:=o.type;
  END inp_hook;

  PROCEDURE inp_all;
    VAR
      o  : pc.OBJECT;
      tag: SHORTINT;
      mod: pc.STRUCT;
      no : BOOLEAN;
      skp: BOOLEAN;
  BEGIN
    hook_md:=NIL;
    dummy_type := new_type(ty_undef);
    dummy_type0 := new_type(ty_undef);
    NEW(dummy_object);
    NEW(dummy_node);
    NEW(types,200);
    ref_no:=0; mod:=NIL;
    LOOP
      file.Read(tag);
      CASE tag OF
        |sym_hook  : inp_hook;
        |sym_type  : inp_struct;
        |sym_module: inp_ref(mod);
        |sym_object: IF mod=NIL THEN Assert(21) END;
                     skp:=mod.obj.mode#pc.ob_inv;
                     inp_object(o,skp,0,mod,no);
                     IF NOT skp THEN o.host:=NIL END;
                     mod:=NIL;
        |sym_end   : EXIT;
      ELSE Assert(22);
      END;
    END;
  END inp_all;

  PROCEDURE inp_import(mod:pc.OBJECT);
  VAR i: LONGINT;
      count: SHORTINT;
      nm: pc.NAME;
      savfile: xfs.SymFile;
      use : pc.USAGE;
      obj : pc.OBJECT;
  BEGIN
    (*IF ~pcS.TS_ext() THEN RETURN; END; *)
    file.errs := FALSE;
    file.Read(count);
    FOR i := 0 TO count-1 DO
      file.ReadString(nm);
      savfile := file;
      inp_sym_file(nm, FALSE, FALSE, obj);
      file := savfile;
      use := mod.type.use;
      WHILE (use # NIL) AND (use.obj.name^ # nm) DO
       use := use.next;
      END;
      IF use = NIL THEN -- not found in current import
        NEW(use);
        use.obj:=obj;
        use.tags:=pc.UTAG_SET{};
        use.next:=mod.type.use;
        mod.type.use:=use;
        IF pcS.TS_ext() THEN dcl_ref(mod.type,obj); END;
      END;
    END;
    file.errs := TRUE;
  END inp_import;

  PROCEDURE chk_types(): BOOLEAN;
    VAR i: LONGINT; t: pc.STRUCT;
  BEGIN
    FOR i:=0 TO LEN(types^)-1 DO
      t:=types[i];
      ASSERT((t=NIL) OR (t.mode#ty_undef));
      ASSERT((t=NIL) OR (t.obj=NIL) OR (t.obj.mode#pc.ob_inv));
    END;
    RETURN TRUE;
  END chk_types;

  PROCEDURE read_file(ext-,name: ARRAY OF CHAR);
    VAR i: LONGINT; fnm: xfs.FNAME; ok: BOOLEAN;
        m:pc.Mno;
  BEGIN
    ok:=xfs.sys.sCreate("",name,ext,fnm); ASSERT(ok);
    xfs.sys.sLookup(fnm,fnm);
    xfs.sym.Open(fnm,FALSE,FALSE);
    IF xfs.sym.file=NIL THEN env.errors.Fault(pos,425,xfs.sym.msg^) END;
    file:=xfs.sym.file(xfs.SymFile);
    file.ReadInt(i);
    IF i#sym_magic THEN env.errors.Fault(pos,190,fnm) END;
    file.ReadInt(i);
    IF i#VERS THEN env.errors.Fault(pos,191,fnm,i,VERS) END;
    file.ReadInt(sym_ident);
    sym_ident := sym_ident - ident_gap;
    IF ~ ((sym_ident - pc.sym_base) IN pc.code.valid_idents) THEN
      IF sym_ident = pc.sym_native THEN
        env.errors.Fault(pos,195,fnm,"Native XDS")
      ELSIF sym_ident = pc.sym_C   THEN
        env.errors.Fault(pos,195,fnm,"XDS-C")
      ELSE
        env.errors.Fault(pos,196,sym_ident,fnm)
      END;
    END;
    inp_all;
    m:=pc.ZEROMno;
    LOOP
      IF m>=pc.mod_cnt THEN EXIT; END;
      IF (pc.mods[m]#NIL) & (name=pc.mods[m].name^) THEN
         inp_import(pc.mods[m]);
         EXIT;
      END;
      INC(m);
    END;
    file.Close;
    file:=NIL;
    ASSERT(chk_types());
  END read_file;

  VAR ext: pc.STRING; mod: pc.OBJECT; m: pc.Mno;
BEGIN
  IF imp_name="SYSTEM" THEN
    system_imported:=TRUE; head:=module_system; RETURN;
  ELSIF imp_name=COMPILER THEN
    head:=module_xds; RETURN
  END;
  cur_pos(pos);
  env.config.Equation("SYM",ext);
  IF browse THEN ASSERT(pc.mod_cnt = pc.ZEROMno) END;
  m:=pc.ZEROMno;
  LOOP
    IF m=pc.mod_cnt THEN
      read_file(ext^,imp_name);
      EXIT;
    ELSIF (pc.mods[m]#NIL) & (imp_name=pc.mods[m].name^) THEN
      head:=pc.mods[m];
      IF ttag_incomplete IN head.type.tags THEN
        head.type.mode:=ty_undef;
        read_file(ext^,head.name^);
      END;
      EXIT;
    END;
    INC(m);
  END;

 IF browse THEN
    m:=pc.ZEROMno; mod:=NIL;
    WHILE m < pc.mod_cnt DO
      IF pc.mods[m] # NIL THEN (*Ned: strange check, see Jack's text below *)
        IF ~ (ttag_incomplete IN pc.mods[m].type.tags) THEN
          ASSERT(mod = NIL); (* only one complete module *)
          mod:=pc.mods[m];
          ASSERT(~ my_sym);
          create_magic_visibility(mod.type);
        END;
      END;
      INC(m);
    END;
    ASSERT(mod # NIL);
    head:=mod;
    RETURN
  END;
  m:=pc.ZEROMno;
  WHILE m<pc.mod_cnt DO
    IF (pc.mods[m]#NIL) & (imp_name=pc.mods[m].name^) THEN
      head:=pc.mods[m];
      IF (head.mode#pc.ob_module) OR
         (ttag_incomplete IN head.type.tags) OR
         (head.type.mode#pc.ty_module) OR
         (head.type.obj#head)
      THEN
        env.errors.Fault(pos,194,imp_name);
      END;
      IF (*<An6*)pcS.TS_ext() OR (*An6>*)NOT my_sym THEN
        create_magic_visibility(head.type)
      END;
      RETURN;
    END;
    INC(m);
  END;
  env.errors.Fault(pos,194,imp_name);
END inp_sym_file;


(* ---------- Cleaning STRUCTs and OBJECTs from BE extensions ----------------*)

VAR
 iterated_mno: pc.Mno;

PROCEDURE clean_object(o: pc.OBJECT);
BEGIN
  o.ext := NIL;
  o.tags:=o.tags*pc.OTAG_Common;
  o.marks:=o.marks*pc.OMARK_SET{pc.omark_aux5,pc.omark_aux6,pc.omark_aux7};
END clean_object;

PROCEDURE clean_type(t: pc.STRUCT);
BEGIN
  t.marks := pc.TMARK_SET{};
  t.ext   := NIL;
  t.tags:=t.tags*pc.TTAG_SET{pc.ttag_usage_ok..pc.ttag_c_type,pc.ttag_intrinsic,pc.ttag_aux21,pc.ttag_aux22,pc.ttag_volatile};
END clean_type;


PROCEDURE IterTypes0(t: pc.STRUCT; time: INTEGER);
VAR
  tt: ESTRUCT;
  o: pc.OBJECT;
BEGIN
  ASSERT(t#NIL);
  IF t.obj # NIL THEN clean_object(t.obj); END;
  IF (t.mno # iterated_mno) & NOT( t IS ESTRUCT ) THEN
    (* Here is the patch because of debug information generetor *)
    (* He is putting attributes over standard types SETxx & their ranges *)
    IF (t.mno < 0) AND (t.mode IN pc.TY_SET{pc.ty_range, pc.ty_set}) THEN
    ELSE
      RETURN;
    END;
  ELSE
    tt := t(ESTRUCT);
    IF tt.time=time THEN RETURN END;
    tt.time := time;
  END;

  clean_type(t);
  IF t.base # NIL THEN IterTypes0(t.base, time); END;
  IF t.inx  # NIL THEN IterTypes0(t.inx,  time); END;
  IF (t.mode = pc.ty_proctype) AND (t.prof # NIL) AND (t.prof.sno < 0) THEN
    t.prof := t.prof.next;
    (* There could be BE introduced fake parameter in order to return structured value *)
  END;
  o:=t.prof;
  WHILE o#NIL DO
    clean_object(o);
    IterTypes0(o.type, time);
    o:=o.next;
  END;

  o:=t.mem;
  WHILE o#NIL DO
    clean_object(o);
    IterTypes0(o.type, time);
    o:=o.next;
  END;
END IterTypes0;

PROCEDURE IterTypes(mno: pc.Mno; t: pc.STRUCT; time: INTEGER);
BEGIN
  iterated_mno := mno;
  IterTypes0(t, time);
END IterTypes;

VAR
  clean_time: INTEGER;

PROCEDURE Clean*;
VAR
  time: INTEGER;
  i   : pc.Mno;
BEGIN
  IF NOT global AND (pc.mod_cnt=pc.ZEROMno) THEN RETURN END;
  INC(clean_time);
  time:=clean_time;
  FOR i:=pc.ZEROMno TO PRED(pc.mod_cnt) DO
    IterTypes(i, pc.mods[i].type, time);
  END;

END Clean;

(*----------------------------------------------------------------*)

CONST
  fn_new*     = 1;
  fn_dispose* = 2;
  fn_resize*  = 3;
  fn_inc*     = 4;
  fn_dec*     = 5;
  fn_incl*    = 6;
  fn_excl*    = 7;
  fn_halt*    = 8;
  fn_assert*  = 9;
  fn_copy*    = 10;
  fn_abs*     = 11;
  fn_cap*     = 12;
  fn_chr*     = 13;
  fn_float*   = 14;
  fn_high*    = 15;
  fn_int*     = 16;
  fn_lfloat*  = 17;
  fn_length*  = 18;
  fn_max*     = 19;
  fn_min*     = 20;
  fn_odd*     = 21;
  fn_ord*     = 22;
  fn_size*    = 23;
  fn_trunc*   = 24;
  fn_val*     = 25;
  fn_ash*     = 26;
  fn_len*     = 27;
  fn_entier*  = 28;
  fn_long*    = 29;
  fn_short*   = 30;
  fn_cc*      = 31;
  fn_adr*     = 32;
  fn_adr_o2*  = 33;
  fn_lsh*     = 34;
  fn_rot*     = 35;
  fn_bit*     = 36;
  fn_cast*    = 37;
  fn_addadr*  = 38;
  fn_subadr*  = 39;
  fn_difadr*  = 40;
  fn_makeadr* = 41;
  fn_get*     = 42;
  fn_getreg*  = 43;
  fn_put*     = 44;
  fn_putreg*  = 45;
  fn_move*    = 46;
  fn_code*    = 47;
  fn_bytes*   = 48;
  fn_bits*    = 49;
  fn_sysnew*  = 50;
  fn_sysdispose*= 51;
  fn_cmplx*   = 60;
  fn_im*      = 62;
  fn_re*      = 63;
  fn_prot*    = 67;
  fn_tsize*   = 69;
  fn_target*  = 70;
  fn_ref*     = 71;
  fn_valid*   = 72;
  fn_fill*    = 73;     (** SYSTEM.FILL *)
  fn_option*  = 74;     (** SYSTEM.OPTION *)
  fn_equation*= 75;     (** SYSTEM.EQUATION *)
  fn_timestamp*= 76;    (** COMPILER.TIMESTAMP *)
  fn_eval*     = 77;
  fn_fldoffs*  = 78;
--  fn_key*      = 79;
  fn_pred*     = 80;
  fn_succ*     = 81;
<* IF MCS THEN *>
  fn_inline*   = 82;
  fn_setreg*   = 83;
  fn_register*   = 84;
<* END *>
  fn_difadrc*  = 85;
  fn_callerIPref*=86;
  fn_excepttable*=87;

PROCEDURE sproc_no*(o: pc.OBJECT): LONGINT;
BEGIN
  ASSERT(o.mode=pc.ob_sproc);
  RETURN o.lref;
END sproc_no;

PROCEDURE ini*( lang: pc.Lang;
                cur_pos_proc: CUR_POS_PROC;
                o2_num_ext: BOOLEAN;
                lang_ext: BOOLEAN);

  PROCEDURE standard(name-: ARRAY OF CHAR; proc_no: SHORTINT);
  (* declare standart function in current scope *)
    VAR o: pc.OBJECT;
  BEGIN
    o := new_obj(pc.ob_sproc);
    set_name(o,name);
    o.lref:=proc_no;
    o.type:=invtype;
    o.mno:=cur_scope.mno;
    INCL(o.tags,otag_exported);
    INCL(o.tags,pc.otag_public);
    dcl(cur_scope,o);
    alloc(o);
  END standard;

  PROCEDURE type(t: pc.STRUCT; name-: ARRAY OF CHAR);
    VAR o: pc.OBJECT;
  BEGIN
    ASSERT(t#NIL);
    o := new_obj(pc.ob_type);
    set_name(o,name);
    o.type:=t;
    o.mno:=cur_scope.mno;
    INCL(o.tags,otag_exported);
    INCL(o.tags,pc.otag_public);
    IF t.obj=NIL THEN t.obj:=o; t.mno:=o.mno END;
    dcl(cur_scope,o);
    alloc(o);
  END type;

  PROCEDURE const(VAR o: pc.OBJECT; name-: ARRAY OF CHAR;
                  t: pc.STRUCT; val: LONGINT);
  BEGIN
    o := new_obj(pc.ob_cons);
    set_name(o,name);
    o.type:=t;
    o.mno:=cur_scope.mno;
    new(o.val,pc.nd_value);
    o.val.type:=t;
    o.val.val:=pc.value.new(env.null_pos,t);
    o.val.val.set_integer(val);
    INCL(o.tags,otag_exported);
    INCL(o.tags,pc.otag_public);
    dcl(cur_scope,o);
    alloc(o);
  END const;

  PROCEDURE std_module(mno: pc.Mno; nm-: ARRAY OF CHAR): pc.OBJECT;
  VAR
    o : pc.OBJECT;
  BEGIN
    -- attention! standard modules have negative 'mno' values!
    IF pc.sys_mods = NIL THEN
      NEW(pc.sys_mods, ORD(-mno_last)+1 );
    END;
    o := new_obj(pc.ob_module);
    o.type := new_type(pc.ty_module);
    o.type.obj:=o;
    -- attention! standard modules have negative 'mno' values!
    pc.sys_mods[PRED(-mno)]:=o;
    o.mno := mno;
    o.type.mno := mno;
    ASSERT(o.lev=0);
    set_name(o,nm);
    RETURN o;
  END std_module;

  PROCEDURE cre_module_system;
    VAR o: pc.OBJECT;
  BEGIN
    module_system := std_module(mno_system, "SYSTEM");
    enter_scope(module_system.type);
    const(o,"BITSPERLOC",ZZ_type,pc.code.bits_per_loc);
    const(o,"LOCSPERWORD",ZZ_type,pc.code.locs_per_word);
    type(loc,  "LOC");
    type(addr,"ADDRESS");
    IF pcS.TS_ext() THEN
      type(word2, "WORD");
      type(word4, "LONGWORD");
    ELSE
      type(word,"WORD");
    END;
    type(index,"INDEX");
    type(difadr,"DIFADR_TYPE");
    IF pc.code.bits_per_loc=8 THEN
      const(o,"LOCSPERBYTE",ZZ_type,1);
      type(loc,"BYTE");
    END;
    standard("ADDADR",fn_addadr);
    standard("SUBADR",fn_subadr);
    standard("DIFADR",fn_difadr);
    standard("DIFADRC",fn_difadrc);
    standard("MAKEADR",fn_makeadr);
    IF oberon THEN standard("ADR",fn_adr_o2);
    ELSE standard("ADR",fn_adr);
    END;
    standard("M2ADR",fn_adr);
    standard("SHIFT",fn_lsh);
    standard("ROTATE",fn_rot);
    IF NOT oberon THEN standard("CAST",fn_cast) END;
    standard("TSIZE",fn_tsize);
    standard("ROT",fn_rot);
    standard("LSH",fn_lsh);
    standard("NEW",fn_sysnew);
    standard("DISPOSE",fn_sysdispose);
    IF oberon THEN type(addr,"PTR") END;
    type(shortint,"INT8");
    type(shortcard,"CARD8");
    type(integer,"INT16");
    type(cardinal,"CARD16");
<* IF MCS THEN *>
    type(cardinal,"UNSIGNED_16");
    type(longcard,"UNSIGNED_32");
<* END *>
    type(longint,"INT32");
    type(longcard,"CARD32");
    type(set8,"SET8");
    type(set16,"SET16");
    type(set32,"SET32");
    type(set64,"SET64");
    type(boolean8, "BOOL8");
    type(boolean16,"BOOL16");
    type(boolean32,"BOOL32");

    type(signed,"int");
    type(unsigned,"unsigned");
    type(size_t,"size_t");
    type(void,"void");

    type(m2_int,"INT");
    type(m2_card,"CARD");

    type(longlongint, "INT64");
    type(longlongcard, "CARD64");

    standard("CC",fn_cc);
    standard("BIT",fn_bit);
    IF oberon THEN standard("VAL",fn_cast) END;
    standard("GET",fn_get);
    standard("GETREG",fn_getreg);
    standard("PUT",fn_put);
    standard("PUTREG",fn_putreg);
    standard("MOVE",fn_move);
    standard("CODE",fn_code);
    standard("BYTES",fn_bytes);
    standard("BITS",fn_bits);
--    standard("TARGET",fn_target);
    standard("REF",fn_ref);
    standard("VALID",fn_valid);
    standard("FILL",fn_fill);
    standard("EVAL",fn_eval);

    IF pcS.TS_ext() THEN
      standard("Eval",fn_eval);
    END;

    standard("FIELDOFS",fn_fldoffs);
    standard("PRED",fn_pred);
    standard("SUCC",fn_succ);

  <* IF MCS THEN *>
    standard("INLINE",fn_inline);
    standard("SETREG",fn_setreg);
    standard("REGISTER",fn_register);
  <* END *>
    standard("CALLERIPREF",fn_callerIPref);
    standard("EXCEPTTABLE",fn_excepttable);

    exit_scope;
    module_system.type.prof:=module_system.type.mem;
    module_system.type.mem:=NIL;
  END cre_module_system;

  PROCEDURE cre_module_xds;
  BEGIN
    module_xds := std_module(mno_compiler, COMPILER);
    enter_scope(module_xds.type);
     standard("TARGET",fn_target);
     standard("OPTION",fn_option);
     standard("EQUATION",fn_equation);
     standard("TIMESTAMP",fn_timestamp);
    exit_scope;
    module_xds.type.prof:=module_xds.type.mem;
    module_xds.type.mem:=NIL;
  END cre_module_xds;

  VAR base: pc.STRUCT; o: pc.OBJECT;
BEGIN
  pc.cur_mod:=MAX(pc.Mno);
  oberon   := lang=pc.flag_o2;
  cur_pos:=cur_pos_proc;
  anonim_cnt:=0;
  index:=NIL;

  IF env.config.Option("GENDEMOSYM") THEN
    ident_gap:=IDENT_GAP_DEMO;
  ELSIF env.config.Option("GENTRIALSYM") THEN
    ident_gap:=IDENT_GAP_TRIAL;
  ELSE
    ident_gap:=0;
  END;

  system_imported:=FALSE;
  (* must be done before first call of "new_type" *)
  NEW(base);
  base.mode:=pc.ty_integer;
  base.tags:=pc.TTAG_SET{}; base.marks:=pc.TMARK_SET{};
  zero:=pc.value.new(env.null_pos,base);
  one :=pc.value.new(env.null_pos,base);
  zero.set_integer(0);
  one .set_integer(1);

  def:=FALSE;
  imp:=FALSE;
  cur_scope:=NIL;
  cur_tail:=NIL;
  level:=0;
  def_import:=NIL;

  global    := env.config.Option("GLOBAL");
  need_syms := env.config.Option("NEEDSYM");
  m2cmp     := env.config.Option("M2CMPSYM");
  changesym := env.config.Option("CHANGESYM");
  env.config.Equation("SYM", SYM_ext);

  module_super:=NIL;
  module_system:=NIL;
  module_xds:=NIL;

  initialized := TRUE;
  (* must be done before first call of "new_obj" *)
  module_super := std_module(mno_pervasive,"");

  ini_std_types;
  enter_scope(module_super.type);

  IF oberon THEN
    type(shortint,"SHORTINT");
    type(integer,"INTEGER");
    type(longint,"LONGINT");
    type(longlongint,"LONGLONGINT");
    type(longlongcard,"LONGLONGCARD");
  ELSE
    type(m2_int,"INTEGER");
    type(m2_card,"CARDINAL");
    IF env.config.Option("M2ADDTYPES") THEN
      type(shortint,"SHORTINT");  type(longint,"LONGINT"); type(longlongint,"LONGLONGINT"); type(longlongcard,"LONGLONGCARD");
      type(shortcard,"SHORTCARD");  type(longcard,"LONGCARD");
    END;
  END;


(*----------------------------------------------------------------*)

  type(boolean  ,"BOOLEAN");
  type(char     ,"CHAR");
  type(real     ,"REAL");
  type(longreal ,"LONGREAL");
  type(ld_real  ,"LONGLONGREAL");
  IF NOT oberon THEN
    type(proctype0 ,"PROC");
    type(bitset    ,"BITSET");
--    type(bitset64    ,"BITSET64");
    type(protection,"PROTECTION");
  ELSE
    type(bitset,"SET");
  END;
  IF NOT oberon OR o2_num_ext THEN
    type(complex   ,"COMPLEX");
    type(lcomplex  ,"LONGCOMPLEX");
  END;

  const(false,"FALSE",boolean,0);
  const(true, "TRUE" ,boolean,1);
  const(nil,  "NIL"  ,AA_type,0);
  IF pcS.TS_ext() THEN
    const(nullproc,  "NULLPROC"  ,AA_type,0);
  END;

  const(o,"INTERRUPTIBLE",protection,0);
  const(o,"UNINTERRUPTIBLE",protection,1);

  standard("ABS",fn_abs);
  standard("DEC",fn_dec);
  standard("DISPOSE",fn_dispose);
  standard("INC",fn_inc);
  standard("INCL",fn_incl);
  standard("EXCL",fn_excl);
  standard("NEW",fn_new);
  standard("HALT",fn_halt);

  IF oberon OR lang_ext THEN
    standard("ASSERT",fn_assert);
    standard("COPY",fn_copy);
  END;

  IF lang_ext THEN
    standard("RESIZE",fn_resize)
  END;

  standard("CAP",fn_cap);
  standard("CHR",fn_chr);
  IF NOT oberon THEN
    standard("FLOAT",fn_float);
    standard("HIGH",fn_high);
    standard("PROT",fn_prot);
    standard("INT",fn_int);
    standard("LFLOAT",fn_lfloat);
  END;
  IF NOT oberon OR o2_num_ext THEN
    standard("CMPLX",fn_cmplx);
    standard("IM",fn_im);
    standard("RE",fn_re);
  END;
  standard("MAX",fn_max);
  standard("MIN",fn_min);
  standard("ODD",fn_odd);
  standard("ORD",fn_ord);
  standard("SIZE",fn_size);
  IF NOT oberon THEN
    standard("TRUNC",fn_trunc)
  END;
  IF NOT oberon OR lang_ext THEN
    standard("LENGTH",fn_length);
    standard("VAL",fn_val)
  END;
  IF oberon OR lang_ext THEN
    standard("ASH",fn_ash);
    standard("LEN",fn_len);
    standard("ENTIER",fn_entier);
  END;
  IF oberon THEN
    standard("LONG",fn_long);
    standard("SHORT",fn_short);
  END;
  IF pcS.TS_ext() THEN
    standard("FieldOfs",fn_fldoffs);
  END;

  cre_module_system;
  cre_module_xds;

  module_super.type.prof:=module_super.type.mem;
  module_super.type.mem:=NIL;
END ini;

PROCEDURE exi*;
BEGIN
  initialized := FALSE;
  inv_obj:=NIL;
  false:=NIL;
  true:=NIL;
  nil:=NIL;
  index:=NIL;
  difadr:=NIL;
  difadrc:=NIL;
  bitset:=NIL;
  unsigned:=NIL;
  signed:=NIL;
  size_t:=NIL;
  m2_int:=NIL;
  m2_card:=NIL;
  zero:=NIL;
  one:=NIL;
  cur_scope:=NIL;
  cur_tail:=NIL;
  module_super:=NIL;
  module_system:=NIL;
  module_xds:=NIL;
  def_import:=NIL;
  types:=NIL;
  file:=NIL;
END exi;

VAR i: LONGINT;
BEGIN
  clean_time := 0;

  FOR i:=0 TO LEN(std_types)-1 DO std_types[i]:=NIL END;
  module_system:=NIL;
  module_xds:=NIL;
  module_super:=NIL;
  align:=0;
  volatile:=FALSE;
  initialized := FALSE;
END pcO.
