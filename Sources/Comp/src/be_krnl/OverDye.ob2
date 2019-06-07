(**------------------------------------------------------------------
--
-- (c) 1998, XDS Ltd., Russia. All Rights Reserved.
--
-- Module      : OverDye
-- Created     : 2 november 98
-- Author      : Alex V. Yeryomin (AVY)
-- Modified By : AVY, KDV
--
-- Purpose: Calculate node and triade positions, make node order in
--          respect of their positions. This order will be used while
--          pasting together node code segments to procedure code segment.
--          Main goal: sequence of line numbers in table of correspondence
--          between of source lines and code (TLC) must be
--          monotone non-decreasing.
--
-- Export:  Variables:
--
--          IsOrder          - node positions order is made already
--          NodePosOrder     - node positions order
--          NodePosBackOrder - node positions inverse order
--
--          Functions:
--
--          ClearNodePosOrders    - initialize (clear) global all variables
--          SetInitialNodePos     - set up node positions initially, node
--                                  position is min among triade positions
--          SmoothTriadePositions - change triade position for right
--                                  order, i.e. starting with (current)
--                                  that node position triade positions
--                                  must be monotone non-decreasing.
--          AccurateNodePosOrder  - make accurate node positions order
--                                  by Color.GenOrder
--
-- Notes: Module was created originally for project Native SL1 with DWARF
--        debug information format. Problem occurred with MULTI
--        (it's Green Hills debugger): in case sequence of line numbers
--        in TLC is shuffled, debugger can not display source text and code
--        in mixed mode correctly. In my opinion, it is very very strange
--        because it is very very simply to do. Nevertheless, we are
--        forced to make a special bodily movement...
--
-- Relations:
--
-- All changes is made under conditional option OVERDYE. New option
-- DBGREFINE turn on/off pasting nodes in order its positions.
--
-- The module CodeDef.ob2 - procedure 'add_pos (offs, pos)' is changed, so
-- that new position will not be added if it will break the right order.
--
-- The module opAttrs.ob2 - new option 'DbgRefine' is defined.
--
-- The module ir.ob2 - new node field 'Position' is defined.
--
-- The module opCode.ob2 - procedure 'Optim_Do' is changed, at the
-- beginning of procedure call 'ClearNodePosOrders' and then after
-- all optiomizations call 'SetInitialNodePos' for set up node
-- positions initially.
--
-- The module Options.ob2 - make initialization options 'DbgRefine',
-- name of this options is 'DBGREFINE'.
--
-- The module r386.ob2 - method RDefs_386_IDB::FiMove is changed: do not
-- add position for fi-functions; method RDefs_386_IDB::EnterNode is
-- changed: before generate any node setup new current position which
-- correspond to node position. procedure SortSegments is changed:
-- fisrt, remake node position order by Color.GenOrder at that order
-- in respect position is being saved, and, second, Color.GenOrder remake
-- by new just now created node positions order.
--
-- The module TestIO.ob2 - added procedures for debugging.
--
-------------------------------------------------------------------*)

MODULE OverDye;

IMPORT ir;
IMPORT pc  := pcK;
IMPORT env := xiEnv;
IMPORT srt := Sorts;
IMPORT clr := Color;


(*----------------------------------------------------------------------
-- Function   : FindMinPos
-- Purpose    : Find minimal positions around of list of triades
-- Parameters : Head of a list of triades
-- Results    : Mininal position
----------------------------------------------------------------------*)

PROCEDURE FindMinPos (t: ir.TriadePtr): env.TPOS;
VAR
  curr: env.TPOS;
BEGIN
  curr := env.null_pos;
  WHILE t # NIL DO
    IF curr.cmp (t.Position) = 1 THEN
      curr := t.Position;
    END;
    t := t.Next;
  END;
  RETURN curr;
END FindMinPos;



(*----------------------------------------------------------------------
-- Function   : RefineNullPos
-- Purpose    : Change null position of traide to previous good position
-- Parameters : Head of a list of triades, initial position
----------------------------------------------------------------------*)

PROCEDURE RefineNullPos (t: ir.TriadePtr; pos: env.TPOS);
BEGIN
  WHILE t # NIL DO
    IF t.Position.IsNull() THEN
      t.Position := pos;
    ELSE
      pos := t.Position;
    END;
    t := t.Next;
  END;
END RefineNullPos;


VAR
  (** node positions order is made already *)
  IsOrder          *: BOOLEAN;
  (** node positions order *)
  NodePosOrder     *: ir.NodeArray;
  (** node positions inverse order *)
  NodePosBackOrder *: ir.NodeArray;


(**---------------------------------------------------------------------
-- Function    : ClearNodePosOrder
-- Purpose     : Initialize (clear) global all variables
-- Used globals: IsOrder, NodePosOrder, NodePosBackOrder
----------------------------------------------------------------------*)

PROCEDURE ClearNodePosOrders *;
BEGIN
  IsOrder := FALSE;
  NodePosOrder := NIL;
  NodePosBackOrder := NIL;
END ClearNodePosOrders;


(*----------------------------------------------------------------------
-- Function    : CompareNodes
-- Purpose     : compare two node positions
-- Parameters  : two numbers in node positions order
-- Used globals: NodePosOrder, ir.Nodes
----------------------------------------------------------------------*)

PROCEDURE CompareNodes (i: LONGINT; j: LONGINT): BOOLEAN;
VAR
  ipos, jpos: env.TPOS;
BEGIN
  ipos := ir.Nodes[NodePosOrder[i]].Position;
  jpos := ir.Nodes[NodePosOrder[j]].Position;
  -- make strong comparison only
  RETURN jpos.cmp (ipos) = -1;
END CompareNodes;


(*----------------------------------------------------------------------
-- Function    : ShakeNodes
-- Purpose     : Excahge two reference in node positions order
-- Parameters  : Two numbers in node positions order
-- Used globals: NodePosOrder
----------------------------------------------------------------------*)

PROCEDURE ShakeNodes (i: LONGINT; j: LONGINT);
VAR
  tmp: ir.Node;
BEGIN
  tmp := NodePosOrder[i];
  NodePosOrder[i] := NodePosOrder[j];
  NodePosOrder[j] := tmp;
END ShakeNodes;


(**---------------------------------------------------------------------
-- Function    : SetInitialNodePos
-- Purpose     : Set up node positions initially
--               node position is min among triade positions
-- Used globals: IsOrder, NodePosOrder, NodePosBackOrder, ir.Nodes
----------------------------------------------------------------------*)

PROCEDURE SetInitialNodePos *;
VAR
  n, N: ir.Node;
BEGIN
  ClearNodePosOrders;
  N := 0;
  -- count alive nodes only
  FOR n := 0 TO ir.Nnodes-1 DO
    IF ir.Nodes[n].Alive THEN
      INC(N);
    ELSE
      ir.Nodes[n].Position := env.null_pos;
    END;
  END;
  IF N > 0 THEN
    NEW (NodePosOrder, N);
    N := 0;
    NEW (NodePosBackOrder, ir.Nnodes);
    FOR n := 0 TO ir.Nnodes-1 DO
      IF ir.Nodes[n].Alive THEN
        ir.Nodes[n].Position := FindMinPos (ir.Nodes[n].First);
        -- delete null triade positions
        RefineNullPos (ir.Nodes[n].First, ir.Nodes[n].Position);
        NodePosOrder[N] := n;
        INC(N);
      END;
      NodePosBackOrder[n] := MAX(ir.Node);
    END;
    -- use steady sort method only
    srt.ShellSort (N, CompareNodes, ShakeNodes);
    -- back order may contain invalid node reference
    FOR n := 0 TO N-1 DO
      NodePosBackOrder[NodePosOrder[n]] := n;
    END;
    IsOrder := TRUE;
  END;
END SetInitialNodePos;


(**---------------------------------------------------------------------
-- Function    : SmoothTriadePositions
-- Purpose     : Change triade position for right order, i.e. starting
--               with (current) that node position triade positions must
--               be monotone non-decreasing.
--               node position is min among triade positions
-- Used globals: IsOrder, NodePosOrder, ir.Nodes
----------------------------------------------------------------------*)

PROCEDURE SmoothTriadePositions *;
VAR
  n, k: ir.Node;
  row1: ir.INT;
  col1: ir.INT;
  row2: ir.INT;
  col2: ir.INT;
  t   : ir.TriadePtr;
  pos : env.TPOS;
  name: pc.STRING;
BEGIN
  IF IsOrder THEN
    FOR n := 0 TO LEN(NodePosOrder^)-1 DO
      k := NodePosOrder[n];
      pos := ir.Nodes[k].Position;
      t := ir.Nodes[k].First;
      WHILE t # NIL DO
        pos.unpack (name, row1, col1);
        t.Position.unpack (name, row2, col2);
        IF row1 <= row2 THEN
          pos := t.Position;
        ELSE
          t.Position := pos;
        END;
        t := t.Next;
      END;
    END;
  END;
END SmoothTriadePositions;


(**---------------------------------------------------------------------
-- Function    : AccurateNodePosOrder
-- Purpose     : Make accurate node positions order by Color.GenOrder
-- Used globals: IsOrder, NodePosOrder, NodePosBackOrder, ir.Nodes,
--               Color.GenOrder, Color.StartOrder, Color.EndOrder
----------------------------------------------------------------------*)

PROCEDURE AccurateNodePosOrder *;
VAR
  l1, l2, c: ir.INT;
  n, b: ir.Node;
  NewOrder: ir.NodeArray;
BEGIN
  l1 := LEN(NodePosOrder^);
  l2 := clr.EndGenOrder-clr.StartGenOrder+1;
  IF l1 # l2 THEN
    -- clear all nodes
    FOR n := 0 TO LEN(NodePosOrder^)-1 DO
      NodePosOrder[n] := MAX(ir.Node);
    END;
    -- round nodes by Color.GenOrder
    FOR c := clr.StartGenOrder TO clr.EndGenOrder DO
      n := clr.GenOrder^[c];
      b := NodePosBackOrder[n];
      -- node must be in node positions order
      ASSERT(b < l1);
      NodePosOrder[b] := n;
    END;
    FOR n := 0 TO LEN(NodePosBackOrder^)-1 DO
      NodePosBackOrder[n] := MAX(ir.Node);
    END;
    NEW (NewOrder, l2);
    l2 := 0;
    FOR c := 0 TO LEN(NodePosOrder^)-1 DO
      n := NodePosOrder[c];
      IF n # MAX(ir.Node) THEN
        NewOrder[l2] := n;
        NodePosBackOrder[n] := VAL(ir.Node, l2);
        INC(l2);
      END;
    END;
    -- number of node which was not eliminated is equal number of all nodes
    ASSERT(l2 = LEN(NewOrder^));
    NodePosOrder := NewOrder;
  END;
END AccurateNodePosOrder;


BEGIN
  ClearNodePosOrders;
END OverDye.