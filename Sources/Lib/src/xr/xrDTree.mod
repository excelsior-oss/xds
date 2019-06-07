(* Copyright (c) 1999 Excelsior Ltd, Russia. All Rights Reserved. *)

<* +M2EXTENSIONS *>
<* +M2ADDTYPES   *>

IMPLEMENTATION MODULE xrDTree; (* VitVit *)

CONST
  LEFT  = 0;
  RIGHT = 1;

  ROOT_INDEX = 1;

VAR
  jumpTable :ARRAY[0..dTreeLen] OF CARDINAL;
  (* to jump on the ROOT-R node of the least subtree that

      - contains the current node as a leaf
      - has a path (from the root to a leaf) on the right of the current node
   *)

PROCEDURE ins (root :CARDINAL);
VAR
  i, count :CARDINAL;
BEGIN
  (* update the tree in bottom-up manner *)

  i := lastLevelBase + root;
  FOR count:=0 TO dTreeLevel DO
    INC (dTree[i]);
    i := i DIV 2;
  END;
END ins;


PROCEDURE del (root :CARDINAL);
VAR
  i, count :CARDINAL;
BEGIN
  i := lastLevelBase + root;

  ASSERT (dTree[i] # 0);
  (* update the tree in bottom-up manner *)


  FOR count:=0 TO dTreeLevel DO
    DEC (dTree[i]);
    i := i DIV 2;
  END;
END del;


PROCEDURE find (root :CARDINAL) :CARDINAL;
VAR
  i, i1 :CARDINAL;
BEGIN
  i := lastLevelBase+root; ASSERT (dTree[i] = 0);

  IF (i MOD 2 = 0) THEN INC (i) END;  -- it's a left node - go to the right

  WHILE (dTree[i] = 0) DO
    i := jumpTable[i];
    IF (i = ROOT_NOT_FOUND) THEN RETURN ROOT_NOT_FOUND END;
  END;

  (* update above levels *)
  i1 := i;
  WHILE (i1 # 0) DO
    DEC (dTree[i1]);
    i1 := i1 DIV 2;
  END;

  (* let's find the leftmost non-zero path to a leaf *)
  WHILE (i < lastLevelBase) DO
    i := i*2;
    (* "i" points to the left subtree *)

    IF (dTree[i+LEFT] = 0) THEN INC (i); END; -- choose the right subtree
    DEC (dTree[i]);
  END;

  RETURN i-lastLevelBase;
END find;

--------------


PROCEDURE weakIns (root :CARDINAL);
BEGIN
  INC (dTree[lastLevelBase+root]);
END weakIns;


PROCEDURE weakDel (root :CARDINAL);
BEGIN
  DEC (dTree[lastLevelBase+root]);
END weakDel;


PROCEDURE recalc();
VAR
  i :CARDINAL;
BEGIN
  FOR i:=lastLevelBase-1 TO ROOT_INDEX BY -1 DO
    dTree[i] := dTree[i*2+LEFT] + dTree[i*2+RIGHT]
  END;
END recalc;

--------------

PROCEDURE initJumpTable();
CONST
  jumpLevelEnd = VAL(CARDINAL, 2**(dTreeLevel-1)-1);
VAR
  i, root :CARDINAL;
BEGIN
  FOR i:=ROOT_INDEX TO dTreeLen DO
    jumpTable[i] := ROOT_NOT_FOUND;
  END;

  FOR root:= ROOT_INDEX TO jumpLevelEnd DO
    i  := root*2;                 -- choose L node from the current one
    WHILE (i < lastLevelBase) DO
      i := i*2+RIGHT;             -- go along R nodes
      jumpTable[i] := root*2+RIGHT;
    END;
  END;
END initJumpTable;


PROCEDURE initDTree();
VAR
  i :CARDINAL;
BEGIN
  FOR i:=ROOT_INDEX TO dTreeLen DO dTree[i] :=0; END;
END initDTree;


PROCEDURE init();
BEGIN
  initJumpTable();
  initDTree();
END init;


END xrDTree.
