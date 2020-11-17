--------------------------------------------------------------------------------
--                          Excelsior LLVM Compiler
--                          (c) 2020, Excelsior, LLC.
-- Module:   llvmCodeSegment
-- Mission:  Internal representation of LLVM code segment. 
-- Synonym:  cs
-- Authors:  Lvov Konstantin
-- Created:  04-Mar-2020
--------------------------------------------------------------------------------
MODULE llvmCodeSegment;

FROM SYSTEM IMPORT PRED, SUCC;
IMPORT sys := SYSTEM;

IMPORT DStrings;
IMPORT fmt := FormStr;

IMPORT  pc := pcK;
IMPORT env := xiEnv;

IMPORT  ir;
IMPORT  at := opAttrs;
IMPORT  cd := CodeDef;
IMPORT prc := opProcs;

IMPORT def := llvmDefs;

--------------------------------------------------------------------------------
TYPE
  InstrCode *= DStrings.String; 
  
  Instr *= POINTER TO InstrDesc;
  InstrDesc *= RECORD
    instr -: InstrCode;
    tpos  -: pc.TPOS;   -- position in source text
  END;

--------------------------------------------------------------------------------
TYPE
  DbgCall = RECORD
    local_no: ir.Local;
    offs:     LONGINT; 
  END; 
  DbgCalls = POINTER TO ARRAY OF DbgCall;

--------------------------------------------------------------------------------
TYPE
  SegmentCode *= POINTER TO ARRAY OF Instr;

  Segment *= POINTER TO SegmentDesc;
  SegmentDesc *= RECORD
    code         -: SegmentCode;
    code_len     -: LONGINT;
    node_no      -: ir.Node;
    label        -: def.LabelName;
    dbg_calls     : DbgCalls;    
    dbg_calls_len : LONGINT;    
  END;

  SegmentsList = POINTER TO ARRAY OF Segment;

--------------------------------------------------------------------------------
VAR  
  -- array contains code for all segments of current procedure 
  ProcSegms    -: SegmentsList;
  ProcSegmsLen -: LONGINT;

  CurrSegm    : Segment; -- all instruction generating procedures add to the segment
  CurrTPOS    : pc.TPOS; -- current source position
  GenTPOS     : BOOLEAN; -- should positions be generated
  LastLabelNo : LONGINT; -- last label number


--------------------------------------------------------------------------------
--                             Segments
--------------------------------------------------------------------------------

PROCEDURE GetNodeLable *(VAR label: ARRAY OF CHAR; node_no: ir.Node);
BEGIN
  ASSERT( node_no # ir.UndefNode );
  fmt.print(label, ".N..%d", node_no);
END GetNodeLable;

--------------------------------------------------------------------------------
PROCEDURE NewSegmentLabel (VAR label: ARRAY OF CHAR; node_no: ir.Node); 
BEGIN 
  IF node_no # ir.UndefNode THEN
    GetNodeLable(label, node_no);
  ELSE -- local label inside node
    INC(LastLabelNo);
    fmt.print(label, ".L..%d", LastLabelNo);  
  END;   
END NewSegmentLabel; 

--------------------------------------------------------------------------------
PROCEDURE SetSegment *(sg: Segment);
BEGIN
  CurrSegm := sg;
END SetSegment;

--------------------------------------------------------------------------------
PROCEDURE GetSegment *(): Segment;
BEGIN
  RETURN CurrSegm;
END GetSegment;

--------------------------------------------------------------------------------
PROCEDURE NewSegment (node_no: ir.Node): Segment;
VAR sg: Segment; 
BEGIN
  NEW(sg);
  NEW(sg.code, 32);
  sg.code_len := 0;
  sg.node_no := node_no;
  NewSegmentLabel(sg.label, node_no);
  sg.dbg_calls := NIL;
  sg.dbg_calls_len := 0;
  RETURN sg; 
END NewSegment;

--------------------------------------------------------------------------------
PROCEDURE ReallocProcSegments ();
VAR i: LONGINT;
    new_list: SegmentsList;
BEGIN
  IF ProcSegms = NIL THEN
    ASSERT( ProcSegmsLen = 0 );
    NEW(ProcSegms, ir.Nnodes);
  ELSIF ProcSegmsLen = LEN(ProcSegms^) THEN
    NEW(new_list, ProcSegmsLen+64);
    FOR i :=0 TO ProcSegmsLen-1 DO  new_list[i] := ProcSegms[i];  END;  
    ProcSegms := new_list;
  END;
END ReallocProcSegments;  

--------------------------------------------------------------------------------
PROCEDURE EnterNewSegment *(node_no:= ir.UndefNode: ir.Node);
VAR sg: Segment; 
BEGIN
  ReallocProcSegments();
  sg := NewSegment(node_no);
  SetSegment(sg); 
  ProcSegms[ProcSegmsLen] := sg;
  INC(ProcSegmsLen);
END EnterNewSegment;


--------------------------------------------------------------------------------
--                             Instructions
--------------------------------------------------------------------------------

PROCEDURE ReallocSegmentCode (sg: Segment);
VAR i: LONGINT;
    new_code: SegmentCode;
BEGIN
  IF sg.code = NIL THEN
    ASSERT( sg.code_len = 0 );
    NEW(sg.code, 32);
  ELSIF sg.code_len = LEN(sg.code^) THEN
    NEW(new_code, sg.code_len*2);
    FOR i := 0 TO sg.code_len-1 DO  new_code[i] := sg.code[i]  END;
    sg.code := new_code;
  END;
END ReallocSegmentCode;  

--------------------------------------------------------------------------------
PROCEDURE GenInstrExt *(instr-: ARRAY OF CHAR); 
VAR sg: Segment;
BEGIN 
  sg := CurrSegm;
  ReallocSegmentCode(sg);
  NEW(sg.code[sg.code_len]);
  DStrings.Assign(instr, sg.code[sg.code_len].instr);
  sg.code[sg.code_len].tpos := CurrTPOS;
  INC(sg.code_len);  
END GenInstrExt; 

--------------------------------------------------------------------------------
PROCEDURE GenInstr *(format-: ARRAY OF CHAR; SEQ args: sys.BYTE); 
VAR instr_code: ARRAY 1024 OF CHAR;
BEGIN 
  fmt.print(instr_code, format, args);
  GenInstrExt(instr_code);
END GenInstr; 


PROCEDURE GenDbgCall *(local_no: ir.Local; format-: ARRAY OF CHAR; SEQ args: sys.BYTE); 

    -- 1 -- AddDbgCall ---------------------------------------------------------
    PROCEDURE ReallocDbgCalls (sg: Segment);
    VAR i: LONGINT;
        new_dbg_calls: DbgCalls;
    BEGIN
      IF sg.dbg_calls = NIL THEN
        ASSERT( sg.dbg_calls_len = 0 );
        NEW(sg.dbg_calls, 32);
      ELSIF sg.dbg_calls_len = LEN(sg.dbg_calls^) THEN
        NEW(new_dbg_calls, sg.dbg_calls_len*2);
        FOR i := 0 TO sg.dbg_calls_len-1 DO new_dbg_calls[i] := sg.dbg_calls[i] END;
        sg.dbg_calls := new_dbg_calls;
      END;
    END ReallocDbgCalls;  

-- 0 -- AddDbgCall -------------------------------------------------------------
VAR sg: Segment;
BEGIN
  sg := CurrSegm;
  ReallocDbgCalls(sg);
  sg.dbg_calls[sg.dbg_calls_len].local_no := local_no;
  sg.dbg_calls[sg.dbg_calls_len].offs := sg.code_len;
  INC(sg.dbg_calls_len);
  GenInstr(format, args);  
END GenDbgCall; 


--------------------------------------------------------------------------------
-- Changes current position in source text.
-- @param[in]: pos - new source text position
PROCEDURE SetSourcePos *(pos-: pc.TPOS);
BEGIN
  IF pos.IsNull() OR pos.InSameFile(at.curr_proc.pos) THEN 
    CurrTPOS := pos; 
  END;
END SetSourcePos;


--------------------------------------------------------------------------------
--            Initialization & Finalization Routines
--------------------------------------------------------------------------------

PROCEDURE ReallocLabelHolders (VAR lhs: def.LHs; lhs_len: LONGINT);
VAR i: LONGINT;
    new_lhs: def.LHs;
BEGIN
  IF lhs = NIL THEN
    ASSERT( lhs_len = 0 );
    NEW(lhs, ir.Nnodes);
  ELSIF lhs_len = LEN(lhs^) THEN
    NEW(new_lhs, lhs_len+64);
    FOR i := 0 TO lhs_len-1 DO  new_lhs[i] := lhs[i];  END;  
    lhs := new_lhs;
  END;
END ReallocLabelHolders;  

--------------------------------------------------------------------------------
PROCEDURE AddLabel (sg: cd.CODE_SEGM; label-: def.LabelName; node_no: ir.Node);
BEGIN
  ReallocLabelHolders(sg.labels, sg.labels_len);
  sg.labels[sg.labels_len] := def.NewLabelHolder(label, sg.code_len, node_no);
  INC(sg.labels_len);
END AddLabel;

--------------------------------------------------------------------------------
PROCEDURE AddPos (tpos-: pc.TPOS);
BEGIN
  cd.mark_pos(tpos);
END AddPos;  

--------------------------------------------------------------------------------
-- Returns 'true' if position refer to same line in the source code?
-- @param[in]: p1, p2 - positions to be compared
-- @note Мы не можем пользоваться обычной процедурой, т.к. не надо учитывать позиции в строках.
PROCEDURE PosEqu *(p1-, p2-: ir.TPOS): BOOLEAN;
VAR fname1, fname2: env.String;
    line1, line2, col1, col2: LONGINT;
BEGIN
  p1.unpack(fname1, line1, col1);
  p2.unpack(fname2, line2, col2);
  RETURN (fname1 = fname2) & (line1 = line2);
END PosEqu;

--------------------------------------------------------------------------------
PROCEDURE SetObjCodeOffsAttr (local_no: ir.Local; offs: LONGINT); 
VAR attr: at.OFFS_EXT;
    obj: pc.OBJECT;
BEGIN
  obj := ir.Locals[local_no].Obj;
  ASSERT( at.attr(obj.ext, at.a_codeOFFS) = NIL );  -- the RE and IM parts of a COMPLEX parameter have the same Local[].Obj 
  NEW(attr);
  attr.offset := offs;
  at.app_obj_attr(obj, attr, at.a_codeOFFS);
END SetObjCodeOffsAttr; 

--------------------------------------------------------------------------------
PROCEDURE EmitSegment (sg: Segment; VAR cur_tpos: pc.TPOS);
VAR i: LONGINT;
    proc_sg: cd.CODE_SEGM;
    proc_code_len: LONGINT;
    is_same_pos_allowed: BOOLEAN;
BEGIN
  cd.get_segm(proc_sg);
  AddLabel(proc_sg, sg.label, sg.node_no);
  is_same_pos_allowed := FALSE;
  proc_code_len := proc_sg.code_len;
  FOR i := 0 TO sg.code_len-1 DO
    IF GenTPOS & ~PosEqu(sg.code[i].tpos, ir.NullPos)
     & ( ~PosEqu(sg.code[i].tpos, cur_tpos) OR is_same_pos_allowed)
    THEN
      AddPos(sg.code[i].tpos);
      cur_tpos := sg.code[i].tpos;
      is_same_pos_allowed := (i = 0) & (sg.node_no = ir.ZERONode);  -- the start position can hide binding of the rest instructions (KMP-197).  
    END;
    cd.GenInstr(sg.code[i].instr^);
  END;
  FOR i := 0 TO sg.dbg_calls_len-1 DO
    SetObjCodeOffsAttr(sg.dbg_calls[i].local_no, sg.dbg_calls[i].offs + proc_code_len);
  END;
END EmitSegment;  

--------------------------------------------------------------------------------
PROCEDURE EndGenProc *();
VAR i: LONGINT;
    proc_sg: cd.CODE_SEGM;
    cur_tpos: pc.TPOS;
BEGIN
  cd.new_segm(proc_sg);
  cd.set_segm(proc_sg);

  IF GenTPOS THEN
    at.GetStartTPos(at.curr_proc, cur_tpos);
    IF NOT ir.NullPos.equ(cur_tpos) THEN
      AddPos(cur_tpos);
    END;
  END;  

  FOR i := 0 TO ProcSegmsLen-1 DO
    EmitSegment(ProcSegms[i], cur_tpos);
  END;
  cd.set_ready(prc.ProcObj(at.curr_procno), proc_sg);

  CurrSegm  := NIL;
  ProcSegms := NIL;
  ProcSegmsLen := 0;
END EndGenProc;
  
--------------------------------------------------------------------------------
PROCEDURE Init *();
BEGIN
  CurrSegm := NIL;
  NEW(ProcSegms, ir.Nnodes);
  ProcSegmsLen := 0;
  CurrTPOS := ir.NullPos;
  GenTPOS  := (at.lineno IN at.COMP_MODE);
  LastLabelNo := ir.Nnodes - 1;
END Init;


END llvmCodeSegment.