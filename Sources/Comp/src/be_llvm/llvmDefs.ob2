--------------------------------------------------------------------------------
--                          Excelsior LLVM Compiler
--                         (c) 2020, Excelsior, LLC.
--
-- Module:   llvmDefs
-- Mission:  Type definitions exported to other compiler components
-- Synonym:  def
-- Authors:  Lvov Konstantin
-- Created:  11-Mar-2020
-- 
-- Created to break cyclic import dependencies between 
-- "be_krnl\llvmCodeSegment" and "be_llvm\CodeDef" modules.
--------------------------------------------------------------------------------
MODULE llvmDefs;

IMPORT ir;
IMPORT pc := pcK;

--------------------------------------------------------------------------------
VAR
  GetTypeName    *: PROCEDURE (VAR name: ARRAY OF CHAR; type: pc.STRUCT);
  GetTagTypeName *: PROCEDURE (VAR name: ARRAY OF CHAR; type: ir.TypeType; size: LONGINT);
  
--------------------------------------------------------------------------------
TYPE
  LabelName *= ARRAY 16 OF CHAR; 

  LabelHolder *= POINTER TO LabelHolderDesc;
  LabelHolderDesc *= RECORD
    offs    -: LONGINT;     -- offset inside segment 
    node_no -: ir.Node;
    label   -: LabelName;    
  END; 
  
  LHs *= POINTER TO ARRAY OF LabelHolder;

--------------------------------------------------------------------------------
PROCEDURE NewLabelHolder *(label -: LabelName; offs: LONGINT; node_no: ir.Node): LabelHolder;
VAR lh: LabelHolder;
BEGIN
  NEW(lh);
  COPY(label, lh.label);
  lh.offs    := offs;
  lh.node_no := node_no;
  RETURN lh;
END NewLabelHolder;

END llvmDefs.