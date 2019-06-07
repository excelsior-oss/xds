(* Created by KDV;
   implementation of entity `internal represenatation object code segment'
*)
MODULE ocir;

IMPORT ir,
       b := BitVect,
       def := OcirDef,
       SYSTEM,
       Emit;
IMPORT D := desc386;
CONST
     WITH_FSTP*  = 0;
     LAST_SEGM_ATTR* = WITH_FSTP;


TYPE Segment*    = POINTER TO SegmentDesc;
     SegmentCode = POINTER TO ARRAY OF def.Operation;
     SegmentDesc*= RECORD
                       code*       : SegmentCode;
                       code_len*   : INTEGER;
                       attrs*      : SET;
                       next*       : Segment;
                   END;


VAR  c_seg* : Segment;
VAR  ProcSegms* : POINTER TO ARRAY OF Segment;

CONST PORTION_SIZE* = 64;

PROCEDURE new_segm*(VAR sg: Segment);
BEGIN
  NEW(sg);
  sg^ := SegmentDesc{NIL,0,{},NIL};
END new_segm;

PROCEDURE SplitSegm*(sg : Segment);
VAR tmp : Segment;
    i, pos,
    last_len, len  : INTEGER;
    code      : SegmentCode;
BEGIN
    IF sg.code_len <= PORTION_SIZE THEN
        FOR i := 0 TO sg.code_len-1 DO sg.code[i].pos := i; END;
        RETURN;
    END;
    code      := sg.code;
    last_len  := sg.code_len;
    pos := 0;
    tmp := sg;
    len := PORTION_SIZE;
    LOOP
      FOR i := 0 TO len - 1 DO
         tmp^.code[i] := code[pos];
         tmp^.code[i].pos := i;
         INC(pos);
      END;
      tmp^.code_len := len;
      last_len := last_len - len;
      IF last_len = 0 THEN EXIT END;
      IF last_len < PORTION_SIZE THEN len := last_len;
      ELSE                            len := PORTION_SIZE;
      END;
      new_segm(tmp^.next);
      tmp := tmp^.next;
      NEW(tmp^.code, len);
    END;
END SplitSegm;

VAR WasFloatMemInLoop* : BOOLEAN;
    InLoop*            : BOOLEAN;

PROCEDURE InitProcSegms*();
   VAR i : ir.Node;
BEGIN
   WasFloatMemInLoop := FALSE;
   InLoop            := FALSE;
   NEW(ProcSegms, ir.Nnodes);
   FOR i:=ir.ZERONode TO SYSTEM.PRED(ir.Nnodes) DO new_segm(ProcSegms[i]); END;
END InitProcSegms;

PROCEDURE FreeProcSegms*();
   VAR j : LONGINT;
       i: ir.Node;
       tmp  : Segment;
BEGIN
   FOR i := ir.ZERONode TO SYSTEM.PRED(ir.Nnodes) DO
       tmp := ProcSegms[i];
       WHILE tmp <> NIL DO
           FOR j := 0 TO tmp.code_len-1 DO
               tmp.code[j].bin := NIL;
           END;
           tmp := tmp^.next;
       END;
       ProcSegms[i] := NIL;
   END;
   ProcSegms := NIL;
END FreeProcSegms;

PROCEDURE GetLastSegment*(sg : Segment) : Segment;
BEGIN
    IF sg = NIL THEN RETURN NIL; END;
    WHILE sg^.next <> NIL DO sg := sg^.next; END;
    RETURN sg;
END GetLastSegment;

PROCEDURE copy_segm_by_len*(VAR dest : Segment; clen : INTEGER);
BEGIN
  new_segm(dest);
  dest.code_len := clen;
  NEW(dest.code,clen);
  dest^.next := NIL;
END copy_segm_by_len;

(* добавляет add_sg в начало res_sg, размещая последний если нужно,
*)
PROCEDURE AddToStart*(VAR res_sg : Segment; add_sg : Segment);
VAR i       : INTEGER;
    new     : SegmentCode;
    new_len : INTEGER;
    NEW_PUSH_SIZE : ir.INT;
BEGIN
    IF res_sg.code = NIL THEN
        NEW(res_sg.code,add_sg.code_len);
        res_sg.code_len := add_sg.code_len;
        FOR i := 0 TO add_sg.code_len-1 DO
            res_sg.code[i] := add_sg.code[i];
            res_sg.code[i].PUSH_SIZE := 0;
            res_sg.code[i].NEXT_PUSH_SIZE := 0;
        END;
    ELSIF LEN(res_sg.code^) <= add_sg.code_len + res_sg.code_len THEN
       new_len := add_sg.code_len + res_sg.code_len;
       NEW(new,new_len);
       FOR i := 0 TO add_sg.code_len-1 DO
           new^[i]                := add_sg.code^[i];
           new^[i].PUSH_SIZE      := res_sg.code[0].PUSH_SIZE;
           new^[i].NEXT_PUSH_SIZE := res_sg.code[0].PUSH_SIZE;
       END;
       FOR i := add_sg.code_len TO new_len-1 DO
           new^[i]     := res_sg.code^[i-add_sg.code_len];
       END;
       res_sg.code := new;
       res_sg.code_len := new_len;
    ELSE
       FOR i := res_sg.code_len-1 TO 0 BY -1 DO
           res_sg.code^[i+add_sg.code_len] := res_sg.code^[i];
       END;
       NEW_PUSH_SIZE := res_sg.code^[0].PUSH_SIZE;
       FOR i := 0 TO add_sg.code_len-1 DO
           res_sg.code^[i]                := add_sg.code^[i];
           res_sg.code^[i].PUSH_SIZE      := NEW_PUSH_SIZE;
           res_sg.code^[i].NEXT_PUSH_SIZE := NEW_PUSH_SIZE;
       END;
       INC(res_sg.code_len,add_sg.code_len);
   END;
END AddToStart;

PROCEDURE simple_add*(VAR sg : Segment; o : def.Operation);
  VAR i: INTEGER; new: SegmentCode;
BEGIN
  IF sg = NIL THEN
    new_segm(sg);
    NEW(new, 8);
    sg.code := new;
  ELSIF sg.code_len = LEN(sg.code^) THEN
    NEW(new, sg.code_len*2);
    FOR i := 0 TO sg.code_len-1 DO new[i] := sg.code[i] END;
    sg.code := new;
  END;
  sg.code[sg.code_len] := o;
  INC(sg.code_len);
END simple_add;

PROCEDURE prepare_next*(bin : def.BinRecipe; code : def.OpCode);
  VAR i   : INTEGER;
      new : SegmentCode;
BEGIN
  IF c_seg.code = NIL THEN
      NEW(c_seg.code, 8);
  ELSIF c_seg.code_len = LEN(c_seg.code^) THEN
      NEW(new, c_seg.code_len*2);
      FOR i := 0 TO c_seg.code_len-1 DO new[i] := c_seg.code[i] END;
      c_seg.code := new;
  END;
  def.InitOperation(c_seg.code[c_seg.code_len], bin, code);
  INC(c_seg.code_len);
END prepare_next;

PROCEDURE prepare_nextC*(bin : def.BinRecipe; code : def.OpCode; c : SHORTINT);
BEGIN
  prepare_next(bin, code);
  c_seg.code[c_seg.code_len-1].attrs.clocks := c;
END prepare_nextC;

PROCEDURE prepare_nextP*(bin : def.BinRecipe; code : def.OpCode; p : def.PairTag);
BEGIN
  prepare_next(bin, code);
  c_seg.code[c_seg.code_len-1].attrs.pair :=  p;
END prepare_nextP;

PROCEDURE prepare_nextCP*( bin : def.BinRecipe;
                          code : def.OpCode;
                            c  : SHORTINT;
                            p  : def.PairTag);
BEGIN
  prepare_next(bin, code);
  c_seg.code[c_seg.code_len-1].attrs.clocks := c;
  c_seg.code[c_seg.code_len-1].attrs.pair :=  p;
END prepare_nextCP;

PROCEDURE EvalLastAttrs*();
VAR tags : def.OpTagAttrs;
BEGIN
    IF (c_seg.code_len = 1) THEN
        c_seg.code[c_seg.code_len-1].PUSH_SIZE := 0;
        c_seg.code[c_seg.code_len-1].NEXT_PUSH_SIZE := 0;
        c_seg.code[c_seg.code_len-1].FLOAT_SIZE := 0;
    ELSE
        tags := def.OpTagAttrs{def.NOT_MOVABLE, def.NOT_TRANSFER_LOCAL_MOVE} *
                        c_seg.code[c_seg.code_len-2].attrs.tag_attrs;
        IF tags # def.OpTagAttrs{} THEN
            c_seg.code[c_seg.code_len-1].attrs.tag_attrs :=
                c_seg.code[c_seg.code_len-1].attrs.tag_attrs + tags;
        END;
        c_seg.code[c_seg.code_len-1].PUSH_SIZE :=
            c_seg.code[c_seg.code_len-2].PUSH_SIZE;
        c_seg.code[c_seg.code_len-1].NEXT_PUSH_SIZE :=
            c_seg.code[c_seg.code_len-1].PUSH_SIZE;
        c_seg.code[c_seg.code_len-1].FLOAT_SIZE :=
            c_seg.code[c_seg.code_len-2].FLOAT_SIZE +
            def.GetFLOAT_SIZE_delta(c_seg.code[c_seg.code_len-2].code);
    END;
END EvalLastAttrs;

PROCEDURE SetPrefix*(p : SHORTINT);
BEGIN
  INCL(c_seg.code[c_seg.code_len-1].prefix,p);
END SetPrefix;

PROCEDURE SetOpCode*(o : def.OpCode);
BEGIN
  c_seg.code[c_seg.code_len-1].code := o;
END SetOpCode;

PROCEDURE SetOpClocks*(c : SHORTINT);
BEGIN
  c_seg.code[c_seg.code_len-1].attrs.clocks := c;
END SetOpClocks;

PROCEDURE SetDestReg*(r : def.Register);
BEGIN
  c_seg.code[c_seg.code_len-1].dest.r := r;
END SetDestReg;

PROCEDURE SetSrcReg*(r : def.Register);
BEGIN
  c_seg.code[c_seg.code_len-1].src.r := r;
END SetSrcReg;

PROCEDURE SetDestMem*(a-  : def.AddrMode);
BEGIN
  c_seg.code[c_seg.code_len-1].dest := def.OpOpnd{SYSTEM.VAL(def.Register,def.MEM_OPND_CODE),a};
END SetDestMem;

PROCEDURE SetDestMemComp*(r1, r2 : def.Register;
                          scale  : D.ScaleType);
BEGIN
  c_seg.code[c_seg.code_len-1].dest := def.OpOpnd{
      SYSTEM.VAL(def.Register,def.MEM_OPND_CODE),
      def.AddrMode{
          Emit.RegPlacement{ir.ZEROVarNum,VAL(D.Reg,r1)},
          Emit.RegPlacement{ir.ZEROVarNum,VAL(D.Reg,r2)},
          scale,
          0,
          ir.UNDEFINED,
          NIL,
          ir.ProcUNDEFINED,
          NIL,
          NIL}};
END SetDestMemComp;

PROCEDURE SetSrcMem*(a-  : def.AddrMode);
BEGIN
  c_seg.code[c_seg.code_len-1].src := def.OpOpnd{SYSTEM.VAL(def.Register,def.MEM_OPND_CODE),a};
END SetSrcMem;

PROCEDURE SetSrcMemComp*(r1, r2 : def.Register;
                         scale  : D.ScaleType);
BEGIN
  c_seg.code[c_seg.code_len-1].src := def.OpOpnd{
      SYSTEM.VAL(def.Register,def.MEM_OPND_CODE),
      def.AddrMode{
          Emit.RegPlacement{ir.ZEROVarNum,VAL(D.Reg,r1)},
          Emit.RegPlacement{ir.ZEROVarNum,VAL(D.Reg,r2)},
          scale,
          0,
          ir.UNDEFINED,
          NIL,
          ir.ProcUNDEFINED,
          NIL,
          NIL}};
END SetSrcMemComp;

PROCEDURE SetDestImm*();
BEGIN
  c_seg.code[c_seg.code_len-1].dest.r := SYSTEM.VAL(def.Register,def.IMM_OPND_CODE);
END SetDestImm;

PROCEDURE SetSrcImm*();
BEGIN
  c_seg.code[c_seg.code_len-1].src.r := SYSTEM.VAL(def.Register,def.IMM_OPND_CODE);
END SetSrcImm;

PROCEDURE SetOpR_R*(d,s : def.Register);
BEGIN
  c_seg.code[c_seg.code_len-1].dest.r := d;
  c_seg.code[c_seg.code_len-1].src.r := s;
END SetOpR_R;

PROCEDURE SetOpR_M*(r : def.Register; a- : def.AddrMode);
BEGIN
  c_seg.code[c_seg.code_len-1].dest.r := r;
  c_seg.code[c_seg.code_len-1].src := def.OpOpnd{SYSTEM.VAL(def.Register,def.MEM_OPND_CODE),a};
END SetOpR_M;

PROCEDURE SetOpR_I*(r : def.Register);
BEGIN
  c_seg.code[c_seg.code_len-1].dest.r := r;
  c_seg.code[c_seg.code_len-1].src.r := SYSTEM.VAL(def.Register,def.IMM_OPND_CODE);
END SetOpR_I;

PROCEDURE SetOpM_R*(a- : def.AddrMode; r : def.Register);
BEGIN
  c_seg.code[c_seg.code_len-1].dest := def.OpOpnd{SYSTEM.VAL(def.Register,def.MEM_OPND_CODE), a};
  c_seg.code[c_seg.code_len-1].src.r := r;
END SetOpM_R;

PROCEDURE SetOpM_M*(a1- : def.AddrMode; a2- : def.AddrMode);
BEGIN
  c_seg.code[c_seg.code_len-1].dest := def.OpOpnd{SYSTEM.VAL(def.Register,def.MEM_OPND_CODE), a1};
  c_seg.code[c_seg.code_len-1].src := def.OpOpnd{SYSTEM.VAL(def.Register,def.MEM_OPND_CODE), a2};
END SetOpM_M;

PROCEDURE SetOpM_I*(a- : def.AddrMode);
BEGIN
  c_seg.code[c_seg.code_len-1].dest := def.OpOpnd{SYSTEM.VAL(def.Register,def.MEM_OPND_CODE), a};
  c_seg.code[c_seg.code_len-1].src.r := SYSTEM.VAL(def.Register,def.IMM_OPND_CODE);
END SetOpM_I;

PROCEDURE AddSrcArg*();
BEGIN
  def.AddOpOpndArgAll(c_seg.code[c_seg.code_len-1].src,
                      c_seg.code[c_seg.code_len-1].attrs);
END AddSrcArg;

PROCEDURE AddDestArg*();
BEGIN
  def.AddOpOpndArgAll(c_seg.code[c_seg.code_len-1].dest,
                      c_seg.code[c_seg.code_len-1].attrs);
END AddDestArg;

PROCEDURE AddSrcDestArg*();
BEGIN
   def.AddOpOpndArgAll(c_seg.code[c_seg.code_len-1].src,
                       c_seg.code[c_seg.code_len-1].attrs);
   def.AddOpOpndArgAll(c_seg.code[c_seg.code_len-1].dest,
                       c_seg.code[c_seg.code_len-1].attrs);
END AddSrcDestArg;

PROCEDURE AddDestIndirectArg*();
BEGIN
   def.AddOpOpndArg(c_seg.code[c_seg.code_len-1].dest,
                    c_seg.code[c_seg.code_len-1].attrs);
END AddDestIndirectArg;

PROCEDURE AddSrcIndirectArg*();
BEGIN
   def.AddOpOpndArg(c_seg.code[c_seg.code_len-1].src,
                    c_seg.code[c_seg.code_len-1].attrs);
END AddSrcIndirectArg;

PROCEDURE AddDestRes*();
BEGIN
  def.AddDestRes(c_seg.code[c_seg.code_len-1]);
END AddDestRes;

PROCEDURE AddAssignOpArg*();
BEGIN
  def.AddAssignOpArg(c_seg.code[c_seg.code_len-1]);
END AddAssignOpArg;

PROCEDURE AddAssignOpArgRes*();
BEGIN
  def.AddAssignOpArgRes(c_seg.code[c_seg.code_len-1]);
END AddAssignOpArgRes;

PROCEDURE AddXchgOpArgRes*();
BEGIN
  def.AddXchgOpArgRes(c_seg.code[c_seg.code_len-1]);
END AddXchgOpArgRes;

PROCEDURE AddOpArgR*(rs : def.Regs);
BEGIN
  c_seg.code[c_seg.code_len-1].attrs.arg.r :=
      c_seg.code[c_seg.code_len-1].attrs.arg.r + rs;
END AddOpArgR;

PROCEDURE AddOpResR*(rs : def.Regs);
BEGIN
  c_seg.code[c_seg.code_len-1].attrs.res.r :=
      c_seg.code[c_seg.code_len-1].attrs.res.r + rs;
END AddOpResR;

PROCEDURE AddOpArgRA*(ras : def.Regs);
BEGIN
  c_seg.code[c_seg.code_len-1].attrs.arg.ra :=
      c_seg.code[c_seg.code_len-1].attrs.arg.ra + ras;
END AddOpArgRA;

PROCEDURE AddOpArgF*(fs : def.Flags);
BEGIN
  c_seg.code[c_seg.code_len-1].attrs.arg.f :=
      c_seg.code[c_seg.code_len-1].attrs.arg.f + fs;
END AddOpArgF;

PROCEDURE AddOpResF*(fs : def.Flags);
BEGIN
  c_seg.code[c_seg.code_len-1].attrs.res.f :=
      c_seg.code[c_seg.code_len-1].attrs.res.f + fs;
END AddOpResF;

PROCEDURE AddOpArgV*(vs : b.BitVector);
BEGIN
  def.AddOpArgV(c_seg.code[c_seg.code_len-1], vs);
END AddOpArgV;

PROCEDURE AddOpArgBadMem*();
BEGIN
  c_seg.code[c_seg.code_len-1].attrs.arg.v.l := def.BAD_MEM;
END AddOpArgBadMem;

PROCEDURE AddOpResV*(vs : b.BitVector);
BEGIN
  def.AddOpResV(c_seg.code[c_seg.code_len-1], vs);
END AddOpResV;

PROCEDURE AddOpResBadMem*();
BEGIN
  c_seg.code[c_seg.code_len-1].attrs.res.v.l := def.BAD_MEM;
END AddOpResBadMem;

PROCEDURE CheckLEA*();
BEGIN
  def.CheckLEA(c_seg.code[c_seg.code_len-1]);
END CheckLEA;

PROCEDURE CheckMoveLocal*();
BEGIN
  def.CheckMoveLocal(c_seg.code[c_seg.code_len-1]);
END CheckMoveLocal;

PROCEDURE AddBOpArgRes* (op : D.BinaryOp);
BEGIN
   def.AddOpOpndArgAll(c_seg.code[c_seg.code_len-1].src,
                       c_seg.code[c_seg.code_len-1].attrs);
   def.AddOpOpndArgAll(c_seg.code[c_seg.code_len-1].dest,
                       c_seg.code[c_seg.code_len-1].attrs);
   IF op <> D.TTT_cmp THEN AddDestRes(); END;
   c_seg.code[c_seg.code_len-1].attrs.res.f := def.ALL_AFLAGS;
END AddBOpArgRes;

PROCEDURE AddUOpArgRes*(op : D.UnaryOp);
BEGIN
   def.AddOpOpndArgAll(c_seg.code[c_seg.code_len-1].dest,
                       c_seg.code[c_seg.code_len-1].attrs);
   AddDestRes();
   c_seg.code[c_seg.code_len-1].attrs.res.f :=
        c_seg.code[c_seg.code_len-1].attrs.res.f + def.UOp2FConsts[op];
END AddUOpArgRes;

PROCEDURE GenFOp*(bin : def.BinRecipe; fop : D.FloatOp);
   VAR code   : def.OpCode;
       clocks : SHORTINT;
       pair   : def.PairTag;
       ArgR,
       ResR   : def.Regs;
       ResF   : def.Flags;
BEGIN
    def.GetFOpAttrs(fop,code,clocks,pair,ArgR,ResR,ResF);
    prepare_nextCP(bin,code,clocks,pair);
    c_seg.code[c_seg.code_len-1].attrs.arg.r :=
        c_seg.code[c_seg.code_len-1].attrs.arg.r + ArgR;
    c_seg.code[c_seg.code_len-1].attrs.res.r :=
        c_seg.code[c_seg.code_len-1].attrs.res.r + ResR;
    c_seg.code[c_seg.code_len-1].attrs.res.f :=
         c_seg.code[c_seg.code_len-1].attrs.res.f + ResF;
    IF fop = D.FXCH THEN
         c_seg.code[c_seg.code_len-1].dest.r := def.ST1;
    END;
END GenFOp;

TYPE ThruSegmentAction*    = PROCEDURE (VAR op : def.Operation);
TYPE ThruSegmentSetAction* = PROCEDURE (VAR op : def.Operation);

PROCEDURE ThruSegment*(s : Segment; action : ThruSegmentAction);
VAR i : INTEGER;
BEGIN
   FOR i := 0 TO s.code_len - 1 DO action(s.code[i]) END;
END ThruSegment;

PROCEDURE ThruSegmentBack*(s : Segment; action : ThruSegmentAction);
VAR i : INTEGER;
BEGIN
   IF s.code_len = 0 THEN RETURN END;
   FOR i := s.code_len - 1 TO 0 BY -1 DO action(s.code[i]) END;
END ThruSegmentBack;


PROCEDURE ThruSegmentSet*(s : Segment; action : ThruSegmentSetAction);
VAR i : INTEGER;
BEGIN
   FOR i := 0 TO s.code_len - 1 DO action(s.code[i]) END;
END ThruSegmentSet;

PROCEDURE ThruSegmentSetBack*(s : Segment; action : ThruSegmentSetAction);
VAR i : INTEGER;
BEGIN
   IF s.code_len = 0 THEN RETURN END;
   FOR i := s.code_len - 1 TO 0 BY -1 DO action(s.code[i]) END;
END ThruSegmentSetBack;

PROCEDURE ClearAttrs*(sg : Segment);
   VAR i : INTEGER;
BEGIN
   FOR i := 0 TO sg.code_len - 1 DO def.ClearOpAttrs(sg.code[i]); END;
END ClearAttrs;

PROCEDURE FindSetUsedFlags*(sg : Segment; used : def.Flags);
   VAR i : INTEGER;
BEGIN
   FOR i := sg.code_len-1 TO 0 BY -1 DO
       IF (sg.code[i].attrs.res.f * used) <> def.Flags{} THEN
           INCL(sg.code[i].attrs.tag_attrs, def.SET_USED_FLAGS);
           used := used - sg.code[i].attrs.res.f;
       END;
       used := used + sg.code[i].attrs.arg.f;
   END;
END FindSetUsedFlags;

PROCEDURE PrefixCorrect*(sg : Segment);
VAR i : INTEGER;
BEGIN
   FOR i := 0 TO sg.code_len-1 DO
       IF sg.code[i].prefix <> def.OpPrefix{} THEN
           INC(sg.code[i].attrs.clocks);
           IF sg.code[i].attrs.pair = def.UV
           THEN sg.code[i].attrs.pair := def.PU;
           ELSIF NOT (sg.code[i].attrs.pair IN def.PairTags{def.FP,def.FNP,def.PU})
           THEN sg.code[i].attrs.pair := def.NP;
           END;
       END;
   END;
END PrefixCorrect;

----------------------------------------------------------------------------


CONST BitsPerSet = SYSTEM.BITS (SET);
      NSets      = (PORTION_SIZE + BitsPerSet - 1) DIV BitsPerSet;

(* длина BV всегда NSets *)

TYPE  BV*  = POINTER TO ARRAY OF SET;

PROCEDURE BVNew*() : BV;
VAR p : BV;
    i : INTEGER;
BEGIN
    NEW(p,NSets);
    FOR i := 0 TO NSets-1 DO p^[i] := {}; END;
    RETURN p;
END BVNew;

PROCEDURE BVClear*(p : BV);
VAR i : INTEGER;
BEGIN
    FOR i := 0 TO NSets-1 DO p^[i] := {}; END;
END BVClear;

PROCEDURE BVIn*(p : BV; e : INTEGER) : BOOLEAN;
BEGIN
    RETURN (e MOD BitsPerSet) IN p^[e DIV BitsPerSet];
END BVIn;

PROCEDURE BVIncl* (p: BV; e: INTEGER);
BEGIN
    INCL (p^[e DIV BitsPerSet], e MOD BitsPerSet);
END BVIncl;

PROCEDURE BVExcl* (p: BV; e: INTEGER);
BEGIN
    EXCL (p^[e DIV BitsPerSet], e MOD BitsPerSet);
END BVExcl;

PROCEDURE BVUnion* (p1, p2, r: BV);
VAR i: INTEGER;
BEGIN
    FOR i:=0 TO NSets-1 DO r^[i] := p1^[i] + p2^[i]; END;
END BVUnion;

PROCEDURE BVUnionAssign* (d, s: BV): BOOLEAN;
VAR b: BOOLEAN;
    i: INTEGER;
    r: SET;
BEGIN
    b := FALSE;
    FOR i:=0 TO NSets-1 DO
        r := d^[i] + s^[i];
        IF r <> d^[i] THEN
           d^[i] := r;
           b := TRUE;
        END;
    END;
    RETURN b;
END BVUnionAssign;

VAR InOut* :  POINTER TO ARRAY OF RECORD
                 in*, out* : BV;
              END;

VAR InitInOut* : PROCEDURE();

PROCEDURE InitInOutEmpty();
END InitInOutEmpty;

PROCEDURE InitInOutFull();
VAR i : INTEGER;
BEGIN
    NEW(InOut,PORTION_SIZE);
    FOR i := 0 TO PORTION_SIZE -1 DO
        InOut[i].in  := BVNew();
        InOut[i].out := BVNew();
    END;
    InitInOut := InitInOutEmpty;
END InitInOutFull;

PROCEDURE ClearInOut*();
VAR i : INTEGER;
BEGIN
    FOR i := 0 TO PORTION_SIZE -1 DO
        BVClear(InOut[i].in);
        BVClear(InOut[i].out);
    END;
END ClearInOut;

BEGIN
    InitInOut := InitInOutFull;
END ocir.
