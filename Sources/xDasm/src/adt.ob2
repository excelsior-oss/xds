<* NEW ListBackupRestoreDebug- *>
MODULE adt;

IMPORT sys:= SYSTEM,
       lstr:= LongStrs,
       io:= Printf;

TYPE
  String = lstr.String;
  INT  = sys.INT32;


CONST
  equal       * = 0;
  more        * = 1;
  less        * = 2;
  noncompared * = 3;


VAR
  CurrList * , MaxList * , CurrTree * , MaxTree * : INT;


(*------------------------------------------------------

  Base type for any elements included 
  in recursive structures( Stack, List, Tree and other).

--------------------------------------------------------*) 
TYPE
  Element * = POINTER TO ElementDesc;
  ElementDesc * = RECORD END;

  NamedElement * = POINTER TO NamedElementDesc;
  NamedElementDesc * = RECORD ( ElementDesc )
    name * : String;
  END;

(*------------------------------------------------------

  Recursive structures

--------------------------------------------------------*)
  (* State *)
  State * = POINTER TO StateDesc;
  StateDesc * = RECORD (ElementDesc)
    state: Element;
    id: INT;
  END;

  (*  Stack  *)  
  Stack * = POINTER TO StackDesc;
  StackDesc = RECORD ( ElementDesc )
    down: Stack;
    e: Element;
  END;

  (*  Tree  *)
  Tree * = POINTER TO TreeDesc;
  TreeDesc = RECORD ( ElementDesc )
    e: Element;
    left, right, up: Tree;
    state: Stack; (* Stack of State *)
    id: INT;
    bal: INT; (* balance *)
  END;

  (*  List  *)

  List * = POINTER TO ListDesc;
  ListDesc = RECORD ( ElementDesc )
    e: Element;
    prev, next, curr: List;
<* IF ListBackupRestoreDebug THEN *>
    state: Stack; (* Stack of State *)
    id: INT;
<* END *>
  END;


(*------------------------------------------------------

                      Storages

--------------------------------------------------------*)

VAR ListStorage : List;
    StackStorage: Stack;
    TreeStorage : Tree;
    NamedElementStorage: List;

PROCEDURE ^ Deallocate * (e: Element);

(*------------------------------------------------------*)
(*
PROCEDURE check_ListStorage;
VAR c: INT;
    l: List;
BEGIN
  c:= 0;
  l:= ListStorage;
  WHILE l # NIL DO INC(c); l:= l.next END;
  io.printf("ListStorage: %d\r", c);
END check_ListStorage;
*)

PROCEDURE get_list(VAR l: List);
BEGIN
  (*
  NEW(l); RETURN;
  *)
  IF ListStorage = NIL THEN
    NEW(l);
  ELSE
    l:= ListStorage;
    ListStorage:= ListStorage.next;
  END;
  (*
  check_ListStorage;
  *)
  INC(CurrList, SIZE(ListDesc));
  IF MaxList < CurrList THEN MaxList:= CurrList END;
END get_list;

PROCEDURE put_list(VAR l: List);
BEGIN
  l.prev:= NIL;
  l.curr:= NIL;
  l.e:= NIL;
<* IF ListBackupRestoreDebug THEN *>
  l.state:= NIL
<* END *>
  l.next:= ListStorage;
  ListStorage:= l;
  l:= NIL;
  DEC(CurrList, SIZE(ListDesc));
END put_list;



(*------------------------------------------------------*)
PROCEDURE get_stack(VAR s: Stack);
BEGIN
  IF StackStorage = NIL THEN
    NEW(s);
  ELSE
    s:= StackStorage;
    StackStorage:= StackStorage.down;
  END;
END get_stack;

PROCEDURE put_stack(VAR s: Stack);
BEGIN
  s.e:= NIL;
  s.down:= StackStorage;
  StackStorage:= s;
  s:= NIL;
END put_stack;

(*------------------------------------------------------*)
PROCEDURE get_tree(VAR t: Tree);
BEGIN
  IF TreeStorage = NIL THEN
    NEW(t);
  ELSE
    t:= TreeStorage;
    TreeStorage:= TreeStorage.up;
  END;
  INC(CurrTree, SIZE(TreeDesc));
  IF MaxTree < CurrTree THEN MaxTree:= CurrTree END;
END get_tree;

PROCEDURE put_tree(VAR t: Tree);
BEGIN
  t.e:= NIL;
  t.left:= NIL;
  t.right:= NIL;
  Deallocate(t.state);
  t.up:= TreeStorage;
  TreeStorage:= t;
  t:= NIL;
  DEC(CurrTree, SIZE(TreeDesc));
END put_tree;

(*-------------------------------------------------------*)
PROCEDURE ( p: Element ) Compare * ( e: Element ): INT;
(* Procedure for comparing two elements.
   It returns
      equal if elements are equal,
      more  if p is more than e,
      less  if p is less than e and
      noncompared if elements are noncompared.
      ( equal, more, less, and noncompared are 
        constants defined in this module )
*)
BEGIN
  RETURN noncompared;
END Compare;




(*-------------------------------------------------------
   
     Procedures for Stack

---------------------------------------------------------*)

PROCEDURE NewStack * ( VAR s: Stack );
BEGIN
  get_stack(s);
  s.e:= NIL;
  s.down:= NIL;
END NewStack;


PROCEDURE ( s: Stack ) Push * ( e: Element );
VAR n: Stack;
BEGIN
  IF e # NIL THEN
    get_stack(n);
    n.e:= e;
    n.down:= s.down;
    s.down:= n;
  END;
END Push;

PROCEDURE ( s: Stack ) Pop * ( VAR e: Element );
VAR ds: Stack;
BEGIN
  IF s.down # NIL THEN
    e:= s.down.e; ds:= s.down;
    s.down:= s.down.down;
    put_stack(ds);
  ELSE
    e:= NIL;
  END;
END Pop;

PROCEDURE ( s: Stack ) IsEmpty * (): BOOLEAN;
BEGIN
  RETURN s.down = NIL;
END IsEmpty;



(*-------------------------------------------------------
   
     Procedures for Tree

---------------------------------------------------------*)
(* Now AVL trees realize with iterating
  Insert and Delete procedures.
*)


(*-------------------------------------------------------------------
PROCEDURE ShowNamedTree * ( tree: Tree );
VAR nodes, oldnodes: POINTER TO ARRAY OF Tree;

  (*------------------------------*)
  PROCEDURE SpaceStr ( VAR str: String; number: INT );
  (* Create string consist space sumbols *)
  BEGIN
    str:= NIL;
    lstr.AppendChar( 0X, str );
    WHILE number > 0 DO
      lstr.AppendChar( ' ', str );
      DEC(number);
    END;
  END SpaceStr;

  (*------------------------------*)
  PROCEDURE ShowNodes();
  VAR i, numb, size: INT;
      empty: BOOLEAN;
      str: String;
  BEGIN
    empty:= TRUE;
    size:= LEN( nodes^ );
    FOR i:=0 TO size - 1  DO
      IF nodes[i] # NIL THEN
         empty:= FALSE;
         IF ( nodes[i].e = NIL ) OR ~( nodes[i].e IS NamedElement ) THEN
           RETURN;
         END;
      END;
    END;
    IF empty THEN RETURN END;
    numb:= 100 DIV size;
    FOR i:=0 TO size - 1  DO
      IF i = 0 THEN  SpaceStr( str, numb DIV 2 );
      ELSE SpaceStr( str, numb - 2 );
      END;
      IF nodes[i] # NIL THEN
        lstr.Append( nodes[i].e(NamedElement).name^, str );
        io.printf(' %s,%d ', str^, nodes[i].bal );
      ELSE
        io.printf(' %s ', str^);
      END;
    END;
    io.printf('\n');
    oldnodes:= nodes;
    NEW( nodes, 2 * size );
    FOR i:=0 TO size - 1 DO
      IF oldnodes[i] # NIL THEN
        nodes[i*2]:=   oldnodes[i].left;
        nodes[i*2+1]:= oldnodes[i].right;
      ELSE
        nodes[i*2]:=   NIL;
        nodes[i*2+1]:= NIL;
      END;
    END;
    ShowNodes();
  END ShowNodes;

(*------------------------------*)
BEGIN
  IF tree = NIL THEN RETURN END;
  io.printf('\n');
  NEW( nodes, 1 );
  nodes[0]:= tree.up;
  ShowNodes();
END ShowNamedTree;
-------------------------------------------------------------------*)



PROCEDURE NewTree * ( VAR t: Tree );
(* There is one node in tree always.
   This node never has meaning field '.e', '.left', '.right'.
   Its field '.up' points to real tree.
*)
BEGIN
  get_tree(t);
  t.e:= NIL;
  t.right:= NIL;
  t.left:= NIL;
  t.up:= NIL;
  t.state:= NIL;
  t.id:= 0;
  t.bal:= 0;
END NewTree;



PROCEDURE ( t: Tree ) Find * ( pattr: Element; VAR found: Element );
VAR p: Tree;
BEGIN
  found:= NIL;
  IF (pattr # NIL) & (t.up # NIL) THEN
    p:= t.up;
    WHILE p # NIL DO
      CASE pattr.Compare(p.e) OF
         equal: found:= p.e; RETURN;
        |more : p:= p.right;
        |less : p:= p.left;
        |noncompared: RETURN;
      END;
    END;
  END;
END Find;



(*---------------------------------------------------------------------*)
PROCEDURE SimpleInsert ( t: Tree; e: Element): Tree;
VAR p, h: Tree;
    direction: BOOLEAN;
BEGIN
  IF t.up # NIL THEN
    h:= t.up;
    REPEAT
      p:= h;
      CASE e.Compare(h.e) OF
         equal: RETURN NIL;
        |more : h:= h.right; direction:= TRUE;
        |less : h:= h.left;  direction:= FALSE;
        |noncompared: RETURN NIL;
      END;
    UNTIL h = NIL;
    get_tree(h);
    h.left:= NIL; h.right:= NIL; h.up:= p;
    h.e:= e;      h.bal:= 0;
    IF direction THEN
      p.right:= h;
    ELSE
      p.left:= h;
    END;
    RETURN h;
  ELSE
    get_tree( t.up );
    t.up.e:= NIL;
    t.up.right:= NIL;
    t.up.left:= NIL;
    t.up.up:= NIL;
    t.up.id:= 0;
    t.up.bal:= 0;
    t.up.e:= e;
    RETURN t.up;
  END;
END SimpleInsert;

(*---------------------------------------------------------------------*)
PROCEDURE ( t: Tree ) Insert * ( e: Element );
VAR cur, p, p2: Tree;
BEGIN
  IF (t.state = NIL) OR (t.state.IsEmpty()) THEN
    IF e = NIL THEN RETURN END;
    cur:= SimpleInsert( t, e );
    WHILE ( cur # NIL ) & ( cur # t.up )  DO
      p:= cur.up;
      IF p.left = cur THEN (* left branch grows up   *)
        CASE p.bal OF
           1 : p.bal:= 0;  RETURN;
          |0 : p.bal:= -1;
          |-1: (* balance *)
            IF cur.bal = -1 THEN (* single LL turn *)
              p.left:= cur.right;
                         IF cur.right # NIL THEN  cur.right.up:= p  END;
              cur.right:= p;        cur.up:= p.up;       p.up:= cur;
              p.bal:= 0;
              IF p = t.up THEN t.up:= cur;
              ELSIF cur.up.left = p THEN cur.up.left:= cur;
              ELSE cur.up.right:= cur;
              END;
              p:= cur;
            ELSE (* double LR turn *)
              p2:= cur.right;
              cur.right:= p2.left;
                        IF p2.left # NIL THEN  p2.left.up:= cur  END;
              p2.left:= cur;        cur.up:= p2;
              p.left:= p2.right;
                        IF p2.right # NIL THEN p2.right.up:= p   END;
              p2.right:= p;         p2.up:= p.up;        p.up:= p2;
              IF p2.bal = -1 THEN  p.bal:= 1     ELSE  p.bal:= 0    END;
              IF p2.bal =  1 THEN  cur.bal:= -1  ELSE  cur.bal:= 0  END;
              IF p = t.up THEN t.up:= p2
              ELSIF p2.up.left = p THEN p2.up.left:= p2;
              ELSE p2.up.right:= p2;
              END;
              p:= p2;
            END;
            p.bal:= 0;
            RETURN;
        END;
      ELSE  (* right branch grows up   *)
        CASE p.bal OF
           -1: p.bal:= 0;  RETURN;
          |0 : p.bal:= 1;
          |1 : (* balance *)
            IF cur.bal = 1 THEN  (* single RR turn *)
              p.right:= cur.left;
                         IF cur.left # NIL THEN cur.left.up:= p END;
              cur.left:= p;         cur.up:= p.up;       p.up:= cur;
              p.bal:= 0;
              IF p = t.up THEN t.up:= cur
              ELSIF cur.up.left = p THEN cur.up.left:= cur;
              ELSE  cur.up.right:= cur;
              END;
              p:= cur;
            ELSE  (* double RL turn *)
              p2:= cur.left;
              cur.left:= p2.right;
                        IF p2.right # NIL THEN  p2.right.up:= cur END;
              p2.right:= cur;        cur.up:= p2;
              p.right:= p2.left;
                        IF p2.left # NIL THEN  p2.left.up:= p    END;
              p2.left:= p;           p2.up:= p.up;         p.up:= p2;
              IF p2.bal =  1 THEN  p.bal:= -1   ELSE  p.bal:= 0    END;
              IF p2.bal = -1 THEN  cur.bal:= 1  ELSE  cur.bal:= 0  END;
              IF p = t.up THEN t.up:= p2
              ELSIF p2.up.left = p THEN p2.up.left:= p2;
              ELSE p2.up.right:= p2;
              END;
              p:= p2;
            END;
            p.bal:= 0;
            RETURN;
        END;
      END;
      cur:= cur.up;
    END;
  ELSE
    io.printf('Error has happend during Tree.Insert action');
  END;
END Insert;


PROCEDURE ( t: Tree ) SetMin * ();
BEGIN
  IF t.up = NIL THEN
    RETURN;
  ELSE
    t.up.up:= t.up;
    WHILE t.up.up.left # NIL DO t.up.up:= t.up.up.left END;
  END;
END SetMin;


PROCEDURE ( t: Tree ) SetMax * ();
BEGIN
  IF t.up = NIL THEN
    RETURN;
  ELSE
    t.up.up:= t.up;
    WHILE t.up.up.right # NIL DO t.up.up:= t.up.up.right END;
  END;
END SetMax;


PROCEDURE ( t: Tree ) FindNext * ( VAR found: Element );
(* If one wants to get nodes of tree
   one by one since minimal, then one have to
   call procedure SetMin before first
   calling of FindNext.
*)
VAR
  p: Tree;
BEGIN
  found:= NIL;
  IF ( t.up # NIL ) & ( t.up.up # NIL ) THEN
    found:= t.up.up.e;
    IF t.up.up.right # NIL THEN
      t.up.up:= t.up.up.right;
      WHILE t.up.up.left # NIL DO t.up.up:= t.up.up.left END;
    ELSE
      p:= t.up.up; t.up.up:= NIL;
      WHILE (p.up # NIL) & (p.up.right = p) DO p:= p.up END;
      IF p # t THEN
        t.up.up:= p.up;
      ELSE
        t.up.up:= NIL (*p1*) ;
      END;
    END;
  END;
END FindNext;


PROCEDURE ( t: Tree ) FindPrev * ( VAR found: Element );
(* If one wants to get nodes of tree
   one by one since maximal, then one have to
   call procedures SetMax or FindLast before first
   calling of FindPrev.
*)
VAR
  p: Tree;
BEGIN
  found:= NIL;
  IF ( t.up # NIL ) & ( t.up.up # NIL ) THEN
    found:= t.up.up.e;
    IF t.up.up.left # NIL THEN
      t.up.up:= t.up.up.left;
      WHILE t.up.up.right # NIL DO t.up.up:= t.up.up.right END;
    ELSE
      p:= t.up.up; t.up.up:= NIL;
      WHILE (p.up # NIL) & (p.up.left = p) DO p:= p.up END;
      IF p # t.up THEN
        t.up.up:= p.up;
      ELSE
        t.up.up:= NIL (*p1*);
      END;
    END;
  END;
END FindPrev;


PROCEDURE ( t: Tree ) FindFirst * ( VAR found: Element );
BEGIN
  t.SetMin();
  t.FindNext(found);
END FindFirst;

PROCEDURE ( t: Tree ) FindLast * ( VAR found: Element );
BEGIN
  t.SetMax();
  t.FindPrev(found);
END FindLast;


(*---------------------------------------------------------------------*)
PROCEDURE BalanceL ( VAR p: Tree; t: Tree; VAR h: BOOLEAN );
(* left branch grows down *)
VAR p1, p2: Tree;
    b1, b2: INT;
BEGIN
  CASE p.bal OF
     -1: p.bal:= 0;
    |0 : p.bal:= 1;  h:= FALSE;
    |1 : (* balance *)
      p1:= p.right;    b1:= p1.bal;
      IF b1 >= 0 THEN  (* single RR turn *)
        p.right:= p1.left;
                   IF p1.left # NIL THEN p1.left.up:= p END;
        p1.left:= p;         p1.up:= p.up;       p.up:= p1;
        IF b1 = 0 THEN  p.bal:= 1;  p1.bal:= -1;  h:= FALSE;
         ELSE p.bal:= 0;  p1.bal:= 0;
        END;
        IF p = t.up THEN t.up:= p1;
         ELSIF p1.up.left = p THEN p1.up.left:= p1;
        ELSE  p1.up.right:= p1;
        END;
        p:= p1;
      ELSE  (* double RL turn *)
        p2:= p1.left;  b2:= p2.bal;
        p1.left:= p2.right;
                  IF p2.right # NIL THEN  p2.right.up:= p1 END;
        p2.right:= p1;        p1.up:= p2;
        p.right:= p2.left;
                  IF p2.left # NIL THEN  p2.left.up:= p    END;
        p2.left:= p;           p2.up:= p.up;         p.up:= p2;
        IF b2 =  1 THEN  p.bal:= -1   ELSE  p.bal:= 0    END;
        IF b2 = -1 THEN  p1.bal:= 1   ELSE  p1.bal:= 0   END;
        IF p = t.up THEN t.up:= p2
         ELSIF p2.up.left = p THEN p2.up.left:= p2;
        ELSE p2.up.right:= p2;
        END;
        p:= p2;
        p2.bal:= 0;
      END;
  END;
END BalanceL;


PROCEDURE BalanceR ( VAR p: Tree; t: Tree; VAR h: BOOLEAN );
(* right branch grows down *)
VAR p1, p2: Tree;
    b1, b2: INT;
BEGIN
  CASE p.bal OF
     1 : p.bal:= 0;
    |0 : p.bal:= -1;  h:= FALSE;
    |-1: (* balance *)
      p1:= p.left;  b1:= p1.bal;
      IF b1 <= 0 THEN (* single LL turn *)
        p.left:= p1.right;
                   IF p1.right # NIL THEN  p1.right.up:= p  END;
        p1.right:= p;        p1.up:= p.up;       p.up:= p1;
        IF b1 = 0 THEN  p.bal:= -1;  p1.bal:= 1;  h:= FALSE;
         ELSE  p.bal:= 0;  p1.bal:= 0;
        END;
        IF p = t.up THEN t.up:= p1;
         ELSIF p1.up.left = p THEN p1.up.left:= p1;
        ELSE p1.up.right:= p1;
        END;
        p:= p1;
      ELSE (* double LR turn *)
        p2:= p1.right;   b2:= p2.bal;
        p1.right:= p2.left;
                  IF p2.left # NIL THEN  p2.left.up:= p1  END;
        p2.left:= p1;        p1.up:= p2;
        p.left:= p2.right;
                  IF p2.right # NIL THEN p2.right.up:= p   END;
        p2.right:= p;         p2.up:= p.up;        p.up:= p2;
        IF b2 = -1 THEN  p.bal:= 1     ELSE  p.bal:= 0    END;
        IF b2 =  1 THEN  p1.bal:= -1   ELSE  p1.bal:= 0   END;
        IF p = t.up THEN t.up:= p2
         ELSIF p2.up.left = p THEN p2.up.left:= p2;
        ELSE p2.up.right:= p2;
        END;
        p:= p2;
        p2.bal:= 0;
      END;
  END;
END BalanceR;

(*----------------------------------------------------------------------*)
PROCEDURE ( t: Tree ) Delete * ( e: Element );
(* If deleting element is current for FindNext or FindPrev
   procedures then it will be NIL.
*)
CONST root  = 0;
      right = 1;
      left  = 2;
VAR p: Tree;
    h: BOOLEAN;
    direction: INT;

  (*------------------------------------*)
  PROCEDURE Del ( r: Tree; VAR h: BOOLEAN );
  BEGIN
    WHILE r.right # NIL DO
      r:= r.right;
    END;
    p.e:= r.e;
    IF r.up = p THEN p.left:= r.left;
     ELSE r.up.right:= r.left;
    END;
    IF r.left # NIL  THEN  r.left.up:= r.up  END;
    h:= TRUE;

    (* balance *)
     r:= r.up;
    WHILE ( h ) & ( r # p ) DO
      BalanceR( r, t, h );
      r:= r.up;
    END;
  END Del;

(*------------------------------------*)
BEGIN
  IF (t.state = NIL) OR t.state.IsEmpty() THEN
    IF ( e = NIL ) OR ( t.up = NIL ) THEN RETURN END;
    p:= t.up;
    direction:= root;
    LOOP
      IF p = NIL THEN RETURN END;
      CASE e.Compare( p.e ) OF
         equal: EXIT;
        |more : p:= p.right;    direction:= right;
        |less : p:= p.left;     direction:= left;
        |noncompared: RETURN;
      END;
    END;
    IF t.up.up = p THEN t.up.up:= NIL END;
    IF p.right = NIL THEN
      h:= TRUE;
      IF p.left # NIL THEN  p.left.up:= p.up  END;
      CASE direction OF
         root : t.up:= p.left;
                RETURN;
        |right: p.up.right:= p.left;
                p:= p.up;
                BalanceR( p, t, h );
        |left : p.up.left:= p.left;
                p:= p.up;
                BalanceL( p, t, h );
      END;
    ELSIF p.left = NIL THEN
      h:= TRUE;
      p.right.up:= p.up;
      CASE direction OF
         root : t.up:= p.right;
                RETURN;
        |right: p.up.right:= p.right;
                p:= p.up;
                BalanceR( p, t, h );
        |left : p.up.left:= p.right;
                p:= p.up;
                BalanceL( p, t, h );
      END;
    ELSE
      Del( p.left, h );
      IF h THEN  BalanceL( p, t, h )  END;
    END;

    (* balance *)
    WHILE ( h ) & ( p # t.up ) DO
      IF p.up.left = p THEN
        p:= p.up;
        BalanceL( p, t, h );
      ELSE
        p:= p.up;
        BalanceR( p, t, h );
      END;
    END;
  ELSE
    io.printf('Error has happend during Tree.Delete action');
  END;
END Delete;


PROCEDURE ( t: Tree ) IsEmpty * (): BOOLEAN;
BEGIN
  RETURN t.up = NIL;
END IsEmpty;


PROCEDURE (t: Tree) Backup * (VAR id: INT);
VAR
  s: State;
BEGIN
  IF t.state = NIL THEN NewStack(t.state) END;
  NEW(s);
  IF t.up # NIL THEN
    s.state:= t.up.up;
  ELSE
    s.state:= NIL;
  END;
  s.id:= t.id;
  t.state.Push(s);
  id:= t.id;
  INC(t.id);
END Backup;


PROCEDURE (t: Tree) Restore * (id: INT);
VAR
  e: Element;
BEGIN
  IF t.state.IsEmpty() THEN
    RETURN;
  ELSE
    t.state.Pop(e);
    IF e(State).id = id THEN
      IF e(State).state # NIL THEN
        t.up.up:= e(State).state(Tree);
      ELSIF t.up # NIL THEN
        t.up.up:= NIL;
      END;
    ELSE
      io.printf('Error has happend during Tree.Restore action');
    END;
    DEC(t.id);
  END;
END Restore;




(*-------------------------------------------------------
   
     Procedures for List

---------------------------------------------------------*)
PROCEDURE NewList * ( VAR l: List );
BEGIN
  get_list(l);
  l.e:= NIL;
  l.prev:= l;
  l.next:= l;
  l.curr:= NIL;
<* IF ListBackupRestoreDebug THEN *>
  l.state:= NIL;
  l.id:= 0;
<* END *>
END NewList;



PROCEDURE ( l: List ) Clean * ();
BEGIN
  IF l.next # l THEN
    l.next.prev:= l.prev;
    Deallocate(l.next);
    l.prev:= l;
    l.next:= l;
  END;
  l.curr:= NIL;
<* IF ListBackupRestoreDebug THEN *>
  l.state:= NIL;
  l.id:= 0;
<* END *>
END Clean;

PROCEDURE ( l: List ) Insert * ( e:Element );
(* Head of list exists ALWAYS,
   and it NEVER contains useful
   information ( field 'e' is NIL ).
   Field 'curr' of head of list points to node of list,
   that was found by procedures FindNext or FindPrev.
*)
VAR
  n: List;
BEGIN
  IF e # NIL THEN
    get_list(n);
    l.prev.next:= n;
    n.prev:= l.prev;
    l.prev:= n;
    n.next:= l;
    n.e:= e;
  END;
END Insert;

PROCEDURE ( l: List ) InsertAfterCurrent * (e:Element);
(* Head of list exists ALWAYS,
   and it NEVER contains useful
   information ( field 'e' is NIL ).
   Field 'curr' of head of list points to node of list,
   that was found by procedures FindNext or FindPrev.
*)
VAR
  n: List;
BEGIN
  IF e # NIL THEN
    get_list(n);
    n.next:= l.curr.next;
    n.prev:= l.curr;
    l.curr.next:= n;
    n.next.prev:= n;
    n.e:= e;
  END;
END InsertAfterCurrent;

PROCEDURE ( l: List ) InsertBeforeCurrent * (e:Element);
(* Head of list exists ALWAYS,
   and it NEVER contains useful
   information ( field 'e' is NIL ).
   Field 'curr' of head of list points to node of list,
   that was found by procedures FindNext or FindPrev.
*)
VAR
  n: List;
BEGIN
  IF e # NIL THEN
    get_list(n);
    n.next:= l.curr;
    n.prev:= l.curr.prev;
    l.curr.prev:= n;
    n.prev.next:= n;
    n.e:= e;
  END;
END InsertBeforeCurrent;


PROCEDURE ( l: List ) Find * ( pattr: Element; VAR found: Element );
VAR
  p: List;
BEGIN
  found:= NIL;
  IF pattr # NIL THEN
    p:= l.next;
    WHILE (p # l) & (pattr.Compare(p.e) # equal) DO
      p:= p.next;
    END;
    IF p # l THEN
      found:= p.e;
      l.curr:= p;
    END;
  END;
END Find;


PROCEDURE ( l: List ) FindAgain * ( VAR found: Element );
VAR
  p: List;
  pattr: Element;
BEGIN
  p:= l.curr.next;
  pattr:= l.curr.e;
  WHILE (p # l) & (pattr.Compare(p.e) # equal) DO
    p:= p.next;
  END;
  IF p = l THEN
    found:= NIL;
  ELSE
    found:= p.e;
  END; 
END FindAgain;


PROCEDURE ( l: List ) Reset * ();
BEGIN
  l.curr:= l;
END Reset;

PROCEDURE ( l: List ) FindNext * ( VAR found: Element );
(* Before first calling of Findnext
   it is required to call procedures Reset or FindFirst.
   After this first calling of FindNext will return first element of list
   (if was called Reset) or second element of list
   (if was called FindFirst).
*)
BEGIN
  IF (l.curr # NIL) & (l.curr # l.prev) THEN
    l.curr:= l.curr.next;
    found:= l.curr.e;
  ELSE
    l.Reset();
    found:= NIL;
  END;
END FindNext;


PROCEDURE ( l: List ) FindPrev * ( VAR found: Element );
(* Before first calling of FindPrev
   it is required to call procedures Reset or FindLast.
   After this first calling of FindPrev will return last element of list
   (if was called Reset) or prelast element of list
   (if was called FindLast).
*)

BEGIN
  IF (l.curr # NIL) & (l.curr # l.next) THEN
    l.curr:= l.curr.prev;
    found:= l.curr.e;
  ELSE
    l.Reset();
    found:= NIL;
  END;
END FindPrev;

PROCEDURE ( l: List ) FindFirst * ( VAR found: Element );
BEGIN
  l.Reset();
  l.FindNext(found);
END FindFirst;

PROCEDURE ( l: List ) FindLast * ( VAR found: Element );
BEGIN
  l.Reset();
  l.FindPrev(found);
END FindLast;

PROCEDURE ( l: List ) IsEmpty * (): BOOLEAN;
BEGIN
  RETURN l.next = l;
END IsEmpty;


PROCEDURE (l: List) DeleteCurrent * ();
BEGIN
<* IF ListBackupRestoreDebug THEN *>
  IF ~l.state.IsEmpty() THEN
    io.printf('Error has happend during List.DeleteCurrent action');
    RETURN;
  END;
<* END *>
  IF (l.curr # NIL) & (l.curr # l) THEN
    l.curr.next.prev:= l.curr.prev;
    l.curr.prev.next:= l.curr.next;
    put_list(l.curr);
  END;
END DeleteCurrent;

<* IF ListBackupRestoreDebug THEN *>

PROCEDURE (l: List) Backup * (VAR id: INT);
VAR
  s: State;
BEGIN
  IF l.state = NIL THEN NewStack(l.state) END;
  NEW(s);
  s.state:= l.curr;
  s.id:= l.id;
  l.state.Push(s);
  id:= l.id;
  INC(l.id);
END Backup;

PROCEDURE (l: List) Restore * (id: INT);
VAR
  e: Element;
BEGIN
  IF l.state.IsEmpty() THEN
    RETURN;
  ELSE
    l.state.Pop(e);
    IF e(State).id = id THEN
      l.curr:= sys.VAL(List, e(State).state);
    ELSE
      io.printf('Error has happend during List.Restore action');
    END;
    DEC(l.id);
  END;
END Restore;

<* ELSE *>

PROCEDURE (l: List) Backup * (VAR id: INT);
VAR ll: List;
BEGIN
  IF l.curr = NIL THEN
    id:= -1;
  ELSE
    id:= 0;
    ll:= l;
    WHILE (ll.next # l) & (ll # l.curr) DO INC(id); ll:= ll.next END;
  END;
END Backup;

PROCEDURE (l: List) Restore * (id: INT);
BEGIN
  IF id >= 0 THEN
    l.curr:= l;
    WHILE id > 0 DO l.curr:= l.curr.next; DEC(id) END;
  ELSE
    l.curr:= NIL;
  END;
END Restore;

<* END *>

(*-------------------------------------------------------
   
     Procedures for NamedElement

---------------------------------------------------------*)

PROCEDURE NewNamedElement * (VAR ne: NamedElement; name-: ARRAY OF CHAR);
VAR e: Element;
BEGIN
  IF NamedElementStorage.IsEmpty() THEN
    NEW(ne);
  ELSE
    NamedElementStorage.FindFirst(e);
    NamedElementStorage.DeleteCurrent();
    ne:= e(NamedElement);
  END;
  lstr.Assign(name, ne.name);
END NewNamedElement;

(*----------------------------------------------------*)
PROCEDURE ( ne: NamedElement ) SetName * ( name-: ARRAY OF CHAR ) ;
BEGIN
  lstr.Assign( name, ne.name );
END SetName;

(*----------------------------------------------------*)
PROCEDURE ( p: NamedElement ) Compare * ( e: Element ): INT;
BEGIN
  IF e IS NamedElement THEN
    IF p.name^ = e(NamedElement).name^ THEN
      RETURN equal;
    ELSIF p.name^ > e(NamedElement).name^ THEN
      RETURN more;
    ELSE
      RETURN less;
    END;
  ELSE
    RETURN noncompared;
  END;
END Compare;

(*------------------------------------------------------*)
PROCEDURE Deallocate * (e: Element);
VAR l: List;
    s: Stack;
    t: Tree;
BEGIN
  IF e = NIL THEN RETURN END;
  WITH
    e: List DO
      e.prev.next:= ListStorage; l:= e;
      WHILE l # ListStorage DO
        l.prev:= NIL;
        l.curr:= NIL;
        l.e:= NIL;
<* IF ListBackupRestoreDebug THEN *>
        l.state:= NIL
<* END *>
        l:= l.next;
        DEC(CurrList, SIZE(ListDesc));
      END;
      ListStorage:= e;
    |e: Stack DO
      s:= e;
      WHILE s.down # NIL DO
        s.e:= NIL;
        s:= s.down;
      END;
      s.e:= NIL;
      s.down:= StackStorage;
      StackStorage:= e;
    |e: Tree DO
      IF e.up # NIL THEN
        t:= e.up;
        LOOP
          IF t.left # NIL THEN
            t:= t.left;
          ELSIF t.right # NIL THEN
            t:= t.right;
          ELSE
            IF t = e.up THEN
              put_tree(e.up);
              EXIT;
            ELSIF t = t.up.left THEN
              t:= t.up;
              put_tree(t.left);
            ELSE
              t:= t.up;
              put_tree(t.right);
            END;
          END;
        END;
      END;
      t:= e; put_tree(t);
    |e: NamedElement DO
      NamedElementStorage.Insert(e);
  ELSE
  END;
END Deallocate;

(*------------------------------------------------------*)

BEGIN
  NewList(NamedElementStorage);
  CurrList:= 0;
  MaxList:= 0;
  CurrTree:= 0;
  MaxTree:= 0;
END adt.
(*
io.printf('100, token = %d, %s\n',token, text^);
*)
