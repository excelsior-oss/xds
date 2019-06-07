(** Copyright (c) 1995 xTech Ltd, Russia. All Rights Reserved. *)
(** XDS librarian. Library public names container. *)
MODULE xlNames; (* Hady. Nov 3 1995 *)

IMPORT
  xlK
  ,str:=xlStrings
  ;

TYPE
  String = str.String;
  Module* = POINTER TO ModuleDesc;
  ModuleDesc = RECORD
    body-: xlK.Module;
    next-: Module;
  END;

  Name* = POINTER TO Node;
  Node = RECORD
    name-: String;
    next-: Name;
    defined-: Module;
    (* AVL tree :-) *)
    bal: SHORTINT;
    left,right: Name;
  END;

  Names* = POINTER TO NamesDesc;
  NamesDesc* = RECORD
    tree  : Name;
    list -: Name;
    last  : Name;
    count-: LONGINT;
    size -: LONGINT;
  END;

  Iterator* = RECORD
  END;

VAR found: Name;

(** Iterator's methods *)

PROCEDURE (VAR i: Iterator) Take(n: Name);
(** Abstract. Must be redefined *)
BEGIN
  ASSERT(FALSE);
END Take;

PROCEDURE greater(s0-,s1-: ARRAY OF CHAR): BOOLEAN;
(* compare strings, considering the "_" char the greatest one *)
  VAR i: LONGINT; c0,c1: INTEGER;
BEGIN
  i:=0;
  LOOP
    IF i>=LEN(s0) THEN c0:=0 ELSE c0:=ORD(s0[i]) END;
    IF i>=LEN(s1) THEN c1:=0 ELSE c1:=ORD(s1[i]) END;
    IF c0#c1 THEN
      IF c0=ORD("_") THEN c0:=ORD(MAX(CHAR))+1 END;
      IF c1=ORD("_") THEN c1:=ORD(MAX(CHAR))+1 END;
      RETURN c0>c1;
    END;
    IF c0=0 THEN EXIT END;
    INC(i);
  END;
  RETURN FALSE;
END greater;

(* AVL tree :-) *)

PROCEDURE insert(name: String; m: xlK.Module; VAR p: Name; VAR h: BOOLEAN);
  VAR p1,p2: Name; (* ~h *)

  PROCEDURE add(p: Name; m: xlK.Module);
    VAR n,t: Module;
  BEGIN
    NEW(n);
    n.body:=m;
    n.next:=NIL;
    IF p.defined=NIL THEN
      p.defined:=n;
    ELSE
      t:=p.defined;
      WHILE t.next#NIL DO t:=t.next END;
      t.next:=n;
    END;
  END add;

BEGIN
  IF p=NIL THEN
    NEW(p);
    h:=TRUE;
    p.name:=name;
    p.left:=NIL; p.right:=NIL; p.bal:=0;
    p.defined:=NIL; add(p,m);
    found:=p;
  ELSIF greater(p.name^,name^) THEN
    insert(name,m,p.left,h);
    IF h THEN         (* left branch has grown *)
      CASE p^.bal OF
        | 1: p.bal:=0; h:=FALSE;
        | 0: p.bal:=-1;
        |-1: (* balansing *)
          p1:=p.left;
          IF p1.bal=-1 THEN (* single LL-turn *)
            p.left:=p1.right; p1.right:=p;
            p.bal:=0; p:=p1;
          ELSE               (* double LL-turn *)
            p2:=p1.right;
            p1.right:=p2.left; p2.left:=p1;
            p.left:=p2.right; p2.right:=p;
            IF p2.bal=-1 THEN p.bal:=1 ELSE p.bal:=0 END;
            IF p2.bal=+1 THEN p1.bal:=-1 ELSE p1.bal:=0 END;
            p:=p2;
          END;
          p.bal:=0; h:=FALSE;
      END;
    END;
  ELSIF greater(name^,p.name^) THEN
    insert(name,m,p.right,h);
    IF h THEN (* right branch has grown *)
      CASE p.bal OF
        |-1: p.bal:=0; h:=FALSE;
        | 0: p.bal:=1;
        | 1: (* balansing *)
          p1:=p.right;
          IF p1.bal=1 THEN (* single RR-turn *)
            p.right:=p1.left; p1.left:=p;
            p.bal:=0; p:=p1;
          ELSE              (* double RR-turn *)
            p2:=p1.left;
            p1.left:=p2.right;
            p2.right:=p1;
            p.right:=p2.left;
            p2.left:=p;
            IF p2.bal=+1 THEN
              p.bal:=-1
            ELSE
              p.bal:=0
            END;
            IF p2.bal=-1 THEN
              p1.bal:=1
            ELSE
              p1.bal:=0
            END;
            p:=p2;
          END;
          p.bal:=0; h:=FALSE;
      END;
    END;
  ELSE
    add(p,m);
    found:=p;
  END;
END insert;

PROCEDURE Linsert(n: Name; VAR p: Name; VAR h: BOOLEAN);
  VAR p1,p2: Name; (* ~h *)
BEGIN
  IF p=NIL THEN
    p:=n;
    n.left:=NIL; n.right:=NIL; n.bal:=0;
    h:=TRUE;
  ELSIF LENGTH(p.name^)>LENGTH(n.name^) THEN
    Linsert(n,p.left,h);
    IF h THEN         (* left branch has grown *)
      CASE p^.bal OF
        | 1: p.bal:=0; h:=FALSE;
        | 0: p.bal:=-1;
        |-1: (* balansing *)
          p1:=p.left;
          IF p1.bal=-1 THEN (* single LL-turn *)
            p.left:=p1.right; p1.right:=p;
            p.bal:=0; p:=p1;
          ELSE               (* double LL-turn *)
            p2:=p1.right;
            p1.right:=p2.left; p2.left:=p1;
            p.left:=p2.right; p2.right:=p;
            IF p2.bal=-1 THEN p.bal:=1 ELSE p.bal:=0 END;
            IF p2.bal=+1 THEN p1.bal:=-1 ELSE p1.bal:=0 END;
            p:=p2;
          END;
          p.bal:=0; h:=FALSE;
      END;
    END;
  ELSE
    Linsert(n,p.right,h);
    IF h THEN (* right branch has grown *)
      CASE p.bal OF
        |-1: p.bal:=0; h:=FALSE;
        | 0: p.bal:=1;
        | 1: (* balansing *)
          p1:=p.right;
          IF p1.bal=1 THEN (* single RR-turn *)
            p.right:=p1.left; p1.left:=p;
            p.bal:=0; p:=p1;
          ELSE              (* double RR-turn *)
            p2:=p1.left;
            p1.left:=p2.right; p2.right:=p1;
            p.right:=p2.left; p2.left:=p;
            IF p2.bal=+1 THEN p.bal:=-1 ELSE p.bal:=0 END;
            IF p2.bal=-1 THEN p1.bal:=1 ELSE p1.bal:=0 END;
            p:=p2;
          END;
          p.bal:=0; h:=FALSE;
      END;
    END;
  END;
END Linsert;


(************************************************************
   Next procedures are saved here for the case I will face with
   need to remove nodes from the tree. They left Wirth's unchanged.
 ************************************************************
PROCEDURE balanceL(VAR p: Ptr; VAR h: BOOLEAN);
  VAR p1,p2: Ptr; b1,b2: Balance;
BEGIN (* h; left branch became shorter *)
  CASE p^.bal OF
    |-1: p^.bal:=0;
    | 0: p^.bal:=1; h:=FALSE;
    | 1: (* balancing... *)
      p1:=p^.right; b1:=p1^.bal;
      IF b1>=0 THEN (* single RR-turn *)
        p^.right:=p1^.left; p1^.left:=p;
        IF b1=0 THEN p^.bal:=1; p1^.bal:=-1; h:=FALSE;
        ELSE p^.bal:=0; p1^.bal:=0;
        END;
        p:=p1;
      ELSE          (* double RL-turn *)
        p2:=p1^.left; b2:=p2^.bal;
        p1^.left:=p2^.right; p2^.right:=p1;
        p^.right:=p2^.left; p2^.left:=p;
        IF b2=+1 THEN p^.bal:=-1 ELSE p^.bal:=0 END;
        IF b2=-1 THEN p1^.bal:=1 ELSE p1^.bal:=0 END;
        p:=p2; p2^.bal:=0;
      END;
  END;
END balanceL;

PROCEDURE balanceR(VAR p: Ptr; VAR h: BOOLEAN);
  VAR p1,p2: Ptr; b1,b2: Balance;
BEGIN (* h; right branch became shorter *)
  CASE p^.bal OF
    | 1: p^.bal:=0;
    | 0: p^.bal:=-1; h:=FALSE;
    |-1: (* balancing... *)
      p1:=p^.left; b1:=p1^.bal;
      IF b1<=0 THEN (* single LL-turn *)
        p^.left:=p1^.right; p1^.right:=p;
        IF b1=0 THEN p^.bal:=-1; p1^.bal:=1; h:=FALSE;
        ELSE p^.bal:=0; p1^.bal:=0;
        END;
        p:=p1;
      ELSE          (* double LR-turn *)
        p2:=p1^.right; b2:=p2^.bal;
        p1^.right:=p2^.left; p2^.left:=p1;
        p^.left:=p2^.right; p2^.right:=p;
        IF b2=-1 THEN p^.bal:=1 ELSE p^.bal:=0 END;
        IF b2=+1 THEN p1^.bal:=-1 ELSE p1^.bal:=0 END;
        p:=p2; p2^.bal:=0;
      END;
  END;
END balanceR;

PROCEDURE delete(x: INTEGER; VAR p: Ptr; VAR h: BOOLEAN);
  VAR q: Ptr;

  PROCEDURE del(VAR r: Ptr; VAR h: BOOLEAN);
  BEGIN (* ~h *)
    IF r^.right#NIL THEN
      del(r^.right,h);
      IF h THEN balanceR(r,h) END;
    ELSE
      q^.key:=r^.key; q^.count:=r^.count;
      q:=r; r:=r^.left; h:=TRUE;
    END;
  END del;

BEGIN (* ~h *)
  IF p=NIL THEN (* no such key in the tree... *)
  ELSIF p^.key>x THEN
    delete(x,p^.left,h);
    IF h THEN balanceL(p,h) END;
  ELSIF p^.key<x THEN
    delete(x,p^.right,h);
    IF h THEN balanceR(p,h) END;
  ELSE (* remove p *)
    q:=p;
    IF q^.right=NIL THEN p:=q^.left; h:=TRUE
    ELSIF q^.left=NIL THEN p:=q^.right; h:=TRUE;
    ELSE
      del(q^.left,h);
      IF h THEN balanceL(p,h) END;
    END;
    DISPOSE(q);
  END;
END delete;
*******************************************************)

(** Names methods *)

PROCEDURE (n: Names) Init*;
BEGIN
  n.tree:=NIL;
  n.list:=NIL;
  n.last:=NIL;
  n.count:=0;
  n.size:=0;
END Init;

PROCEDURE (n: Names) Insert*(name: str.String; m: xlK.Module);
  VAR ignore: BOOLEAN;
BEGIN
  found:=NIL; 
  ignore:=FALSE; insert(name,m,n.tree,ignore);
  ASSERT(found#NIL);
  IF found.defined.next=NIL THEN
    IF n.last=NIL THEN
      n.list:=found;
    ELSE
      n.last.next:=found;
    END;
    found.next:=NIL;
    n.last:=found;
    INC(n.count);
    n.size:=n.size+LENGTH(name^);
  END;
END Insert;

PROCEDURE iterate(n: Name; VAR iter: Iterator);
BEGIN
  IF n#NIL THEN
    iterate(n.left,iter);
    iter.Take(n);
    iterate(n.right,iter);
  END;
END iterate;

PROCEDURE (n: Names) ReorderByLength*;
  VAR t: Name; ignore: BOOLEAN;
BEGIN
  n.tree:=NIL;
  t:=n.list;
  WHILE t#NIL DO
    ignore:=FALSE; Linsert(t,n.tree,ignore);
    t:=t.next;
  END;
END ReorderByLength;

PROCEDURE (n: Names) Order*(VAR iter: Iterator);
(**
  iterates all registered names in alphabethical or name length odrer
*)
BEGIN
  iterate(n.tree,iter);
END Order;

END xlNames.
