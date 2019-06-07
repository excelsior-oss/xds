(* Created by KDV;
   module evaluates informational, anti and output dependences
   and constructs correpsonding graph;
*)
MODULE OcGraph;

IMPORT
       ir,
       Color,
       ocir,
       Emit,
       def := OcirDef,
       b  := BitVect;


CONST  UNDEFINED_POS = -1;

VAR SEGM : ocir.Segment;
    SEGM_LEN : INTEGER;

VAR MAX_POSSIBLE_CHAIN*, CHAIN_CNT*,
    MAX_POSSIBLE_AGI*,   AGI_CNT*   : LONGINT;
    CHAIN_PRC*, AGI_PRC* : LONGINT;

(* the variables used to determine
    what criterion (agi or longest chains) is more important
    on the every step of reordering (and reevaluate during the process)
*)

VAR    POS_MASK : RECORD
                      r : ARRAY def.Register OF INTEGER;
                      f : ARRAY def.FlagsNum OF INTEGER;
                  END;

VAR    SCALE_MASK : RECORD
                      r    : ARRAY def.Register OF ocir.BV;
                      f    : ARRAY def.FlagsNum OF ocir.BV;
                      allf : ARRAY def.FlagsNum OF ocir.BV;
                    END;

TYPE   INT_SCALE = ARRAY ocir.PORTION_SIZE OF INTEGER;
VAR    NextMemShift,
       NOT_MOVABLE_MOVE_STORE_SCALE : INT_SCALE;
       PREV_DANG_OP,
       NextMemShiftLen,
       NOT_MOVABLE_MOVE_STORE_SCALELen : INTEGER;

PROCEDURE InitAuxScales();
    VAR i : def.Register;
        j : INTEGER;
BEGIN
   FOR i:=def.MINREG TO def.MAXFREG DO
       POS_MASK.r[i] := UNDEFINED_POS;
       ocir.BVClear(SCALE_MASK.r[i]);
   END;
   FOR j:=0 TO def.FlagsNum-1 DO
       POS_MASK.f[j] := UNDEFINED_POS;
       ocir.BVClear(SCALE_MASK.f[j]);
       ocir.BVClear(SCALE_MASK.allf[j]);
   END;
   NextMemShiftLen := 0;
   PREV_DANG_OP := UNDEFINED_POS;
   NOT_MOVABLE_MOVE_STORE_SCALELen := 0;
END InitAuxScales;

PROCEDURE add_operation_in(dst, src : INTEGER);
BEGIN
   ocir.BVIncl(ocir.InOut[dst].in,src);
   INCL(SEGM.code[dst].attrs.tag_attrs,def.HAS_IN);
END add_operation_in;

PROCEDURE add_operation_out(dst, src : INTEGER);
BEGIN
   ocir.BVIncl(ocir.InOut[dst].out, src);
   INCL(SEGM.code[dst].attrs.tag_attrs,def.HAS_OUT);
END add_operation_out;

PROCEDURE add_inout_by_scale(pos : INTEGER;
                             scale : INT_SCALE;
                             len : INTEGER);
   VAR i : INTEGER;
BEGIN
   FOR i := 0 TO len-1 DO
       ocir.BVIncl(ocir.InOut[pos].in, scale[i]);
       INCL(SEGM.code[pos].attrs.tag_attrs,def.HAS_IN);
       ocir.BVIncl(ocir.InOut[scale[i]].out, pos);
       INCL(SEGM.code[scale[i]].attrs.tag_attrs,def.HAS_OUT);
   END;
END add_inout_by_scale;

PROCEDURE AreLocationsPossiblyIntersect(v1-, v2- : def.Vars;
                                        VAR additional_check : BOOLEAN) : BOOLEAN;

    PROCEDURE IsDefined(l : ir.Local) : BOOLEAN;
    BEGIN
        RETURN (l <> ir.UNDEFINED) & (l <> def.BAD_MEM);
    END IsDefined;

BEGIN
   additional_check := FALSE;
   (* предполагается, что если мы сюда попали, то
     оба адреса нетривиальные
   *)
   IF v1.vs <> NIL THEN
       IF (IsDefined(v2.l) AND (v2.l < Color.NLocals) AND b.In(v1.vs,ORD(v2.l))) OR
          (v2.l = def.BAD_MEM) OR
          ((v2.vs <> NIL) AND b.Intersecting(v1.vs,v2.vs)) THEN
          RETURN TRUE;
       END;
   ELSIF v2.vs <> NIL THEN
       IF (v2.l = def.BAD_MEM) OR
          (IsDefined(v1.l) AND (v1.l < Color.NLocals) AND b.In(v2.vs,ORD(v1.l))) THEN
          RETURN TRUE;
       END;
   END;
   (* больше v_i.vs меня не интересуют *)
   IF IsDefined(v1.l) THEN (* v1 --- хороший *)
       IF IsDefined(v2.l) THEN (* v2 --- хороший *)
           additional_check := TRUE;
           RETURN v1.l = v2.l;
       ELSE (* v2 --- плохой *)
           RETURN (v1.l < Color.NLocals) AND
                  (ir.IsExternal(v1.l) OR ir.Locals[v1.l].Addressed);
       END;
   ELSIF ~IsDefined(v2.l) THEN
       additional_check := (v1.l = def.BAD_MEM) & (v2.l = def.BAD_MEM);
       RETURN TRUE;
   ELSE
       RETURN ~IsDefined(v2.l) OR
              ((v2.l < Color.NLocals) AND
               (ir.IsExternal(v2.l) OR ir.Locals[v2.l].Addressed));
   END;
END AreLocationsPossiblyIntersect;


(* сравнение списков смещений из адресов *)
PROCEDURE CompareOffsLists(l1,l2  : Emit.OffsRecPtr) : BOOLEAN;
BEGIN
    LOOP
       IF l1 = NIL THEN RETURN l2 = NIL;
       ELSIF l2 = NIL THEN RETURN FALSE;
       ELSIF l1^.name <> l2^.name THEN RETURN FALSE;
       ELSE l1 := l1^.next; l2 := l2^.next;
       END;
    END;
END CompareOffsLists;

(* сравнение списков процедур из адресов *)
PROCEDURE CompareProcLists(l1,l2  : Emit.ProcRecPtr) : BOOLEAN;
BEGIN
    LOOP
       IF l1 = NIL THEN RETURN l2 = NIL;
       ELSIF l2 = NIL THEN RETURN FALSE;
       ELSIF l1^.name <> l2^.name THEN RETURN FALSE;
       ELSE l1 := l1^.next; l2 := l2^.next;
       END;
    END;
END CompareProcLists;

(* сравнение адресов без смещений *)
PROCEDURE CompareAddrsNoOffs(a1-,a2- : def.AddrMode) : BOOLEAN;
BEGIN
    RETURN (a1.place1.r  = a2.place1.r)  &  (a1.place2.r = a2.place2.r) &
           (a1.scale = a2.scale) &  (a1.proc = a2.proc) &
           CompareOffsLists(a1.offslist,a2.offslist)                  &
           CompareProcLists(a1.proclist,a2.proclist);
END CompareAddrsNoOffs;

CONST MREAD*  = 0;
      MWRITE* = 1;

PROCEDURE AreLocationsIntersect*(op1-,op2- : def.Operation;
                                tag1,tag2 : SHORTINT) : BOOLEAN;


VAR v1,v2   : def.Vars;
    a1,a2   : def.AddrMode;
    sz1,sz2 : SHORTINT;

VAR additional_check : BOOLEAN;
    rs : def.Regs;
    from,to,i: LONGINT;

BEGIN


    IF tag1 = MREAD THEN v1 := op1.attrs.arg.v;
    ELSE                 v1 := op1.attrs.res.v;
    END;
    IF tag2 = MREAD THEN v2 := op2.attrs.arg.v;
    ELSE                 v2 := op2.attrs.res.v;
    END;

    IF ~ AreLocationsPossiblyIntersect(v1,v2,additional_check) THEN
        RETURN FALSE;
    ELSIF ~ additional_check THEN RETURN TRUE;
    ELSE
        IF tag1 = MREAD THEN a1 := op1.attrs.arg.a;
        ELSE                 a1 := op1.attrs.res.a;
        END;
        IF tag2 = MREAD THEN a2 := op2.attrs.arg.a;
        ELSE                 a2 := op2.attrs.res.a;
        END;
        IF CompareAddrsNoOffs(a1,def.EmptyAddrMode) OR
           CompareAddrsNoOffs(a2,def.EmptyAddrMode)
        THEN RETURN TRUE;
        END;
        IF ~ CompareAddrsNoOffs(a1,a2) THEN RETURN TRUE;
        ELSE
            rs := def.Regs{};
            IF a1.place1.r <> def.UNDEF_REG THEN INCL(rs,VAL(def.Register,a1.place1.r)); END;
            IF a1.place2.r <> def.UNDEF_REG THEN INCL(rs,VAL(def.Register,a1.place2.r)); END;
            IF op1.pos < op2.pos THEN from := op1.pos; to := op2.pos;
            ELSE                      from := op2.pos; to := op1.pos;
            END;
            FOR i := from TO to DO
                IF rs * SEGM.code[i].attrs.res.r <> def.Regs{} THEN
                    RETURN TRUE;
                END;
            END;
            IF tag1 = MREAD THEN sz1 := def.GetReadSz(op1);
            ELSE                 sz1 := def.GetWriteSz(op1);
            END;
            IF tag2 = MREAD THEN sz2 := def.GetReadSz(op2);
            ELSE                 sz2 := def.GetWriteSz(op2);
            END;

            IF (sz1 = def.UNDEF_SIZE) OR (sz2 = def.UNDEF_SIZE)
            THEN RETURN TRUE;
            END;
            RETURN ((a2.offs>=a1.offs) & (a2.offs<a1.offs+sz1)) OR
                   ((a1.offs>=a2.offs) & (a1.offs<a2.offs+sz2));
        END;
    END;
END AreLocationsIntersect;

PROCEDURE EvalAgi(pos : INTEGER);
   VAR low, i : INTEGER;
       rs  : def.Regs;
BEGIN
   rs := SEGM.code[pos].attrs.arg.ra;
   IF rs = def.Regs{} THEN RETURN END;
   INC(MAX_POSSIBLE_AGI);
   IF pos - 3 <= 0 THEN low := 0; ELSE low := pos-3; END;
   FOR i := low TO pos - 1 DO
       IF (SEGM.code[i].attrs.res.r * rs) <> def.Regs{} THEN
           INC(AGI_CNT);
           INCL(SEGM.code[pos].attrs.tag_attrs, def.WITH_AGI);
           RETURN;
       END;
   END;
END EvalAgi;

PROCEDURE UsedMem(op : def.Operation) : SHORTINT;
BEGIN
   IF op.dest.r = def.MEM_OPND_CODE THEN
       RETURN 1 + VAL(SHORTINT,ORD(op.src.r = def.MEM_OPND_CODE)) +
              VAL(SHORTINT,
                  ORD(def.GetOpGroup(op.code) IN
                      def.OpGroupSet{def.GDT_OGROUP,def.A_OGROUP,def.L_OGROUP}));
   ELSE
       RETURN VAL(SHORTINT,op.src.r = def.MEM_OPND_CODE)
   END;
END UsedMem;

PROCEDURE IsNotMovable*(o : def.Operation) : BOOLEAN;
BEGIN
    RETURN
        ((def.GetOpGroup(o.code) IN def.OpGroupSet{def.CT_OGROUP,def.CJ_OGROUP}) AND
         (o.code <> def.CALL) AND
         (NOT (def.MOV_CONTROL_TRANSFER IN o.attrs.tag_attrs))) OR
        (def.NOT_MOVABLE IN o.attrs.tag_attrs);
END IsNotMovable;

PROCEDURE IsDang*(o : def.Operation) : BOOLEAN;
BEGIN
   RETURN (o.attrs.arg.v.l = def.BAD_MEM) OR (o.attrs.res.v.l = def.BAD_MEM) OR
          (o.code = def._DIV) OR (o.code = def.IDIV) OR
          (o.attrs.pair IN def.PairTags{def.FP, def.FNP});
END IsDang;

PROCEDURE aux_infodeps(VAR o : def.Operation);
    VAR j, ii           : INTEGER;
        i: def.Register;
        sub_xor_except,
        mov_esp_except,
        oused_mem, oset_mem,
        iused_mem, iset_mem   : BOOLEAN;
BEGIN
   o.attrs.used_mem := UsedMem(o);
   IF o.pos > 0 THEN SEGM.code[o.pos-1].NEXT_PUSH_SIZE := o.PUSH_SIZE; END;
   oused_mem := FALSE;
   oset_mem := FALSE;
   sub_xor_except := ((o.code = def.XOR) OR (o.code = def.SUB)) AND
                     (VAL(SHORTINT,o.dest.r) >= 0)    AND
                     (o.dest.r = o.src.r);
   mov_esp_except := def.MOV_ESP_LOCAL IN o.attrs.tag_attrs;
   EvalAgi(o.pos);

   FOR i:=def.MINREG TO def.MAXFREG DO
       IF (POS_MASK.r[i] <> UNDEFINED_POS) AND
          (i IN o.attrs.arg.r) AND (NOT sub_xor_except) AND
          ((i <> def.ESP) OR (NOT mov_esp_except) OR (SEGM.code[POS_MASK.r[i]].code = def.CALL)) THEN
               add_operation_in(o.pos, POS_MASK.r[i]);
               add_operation_out(POS_MASK.r[i], o.pos);
       END;
       IF i IN o.attrs.res.r THEN POS_MASK.r[i] := o.pos; END;
   END;

   FOR j:=0 TO def.FlagsNum-1 DO
       IF (POS_MASK.f[j] <> def.UNDEF_POS) AND (j IN o.attrs.arg.f)
       THEN
           add_operation_in(o.pos, POS_MASK.f[j]);
           add_operation_out(POS_MASK.f[j], o.pos);
       END;
       IF j IN o.attrs.res.f THEN
           POS_MASK.f[j] := o.pos;
       END;
   END;

   IF IsNotMovable(o) THEN
       FOR j := 0 TO o.pos-1 DO
           add_operation_out(j,o.pos); add_operation_in(o.pos,j);
       END;
       FOR j := o.pos+1 TO SEGM_LEN-1 DO
           add_operation_in(j,o.pos);  add_operation_out(o.pos,j);
       END;
   END;

   IF (def.DANG_OP IN o.attrs.tag_attrs) OR IsDang(o) THEN
       IF PREV_DANG_OP <> UNDEFINED_POS THEN
           add_operation_in(o.pos,PREV_DANG_OP);
           add_operation_out(PREV_DANG_OP,o.pos);
       END;
       PREV_DANG_OP := o.pos;
   END;

   IF (o.attrs.arg.v.l <> ir.UNDEFINED) OR (o.attrs.arg.v.vs <> NIL) THEN
       INCL(o.attrs.tag_attrs, def.USED_MEM);
       oused_mem := TRUE;
   END;

   IF (o.attrs.res.v.l <> ir.UNDEFINED) OR (o.attrs.res.v.vs <> NIL) THEN
       INCL(o.attrs.tag_attrs, def.SET_MEM);
       oset_mem := TRUE;
       IF (o.attrs.res.v.l >= ir.ZEROVarNum) AND ir.IsExternal(o.attrs.res.v.l) THEN
       ELSE
          add_inout_by_scale(o.pos,
                             NOT_MOVABLE_MOVE_STORE_SCALE,
                             NOT_MOVABLE_MOVE_STORE_SCALELen);
      END;
   END;

   IF oused_mem OR oset_mem THEN
      (* определяем зависимости с предыдущими командами, пользующими память *)
      FOR j:=0 TO NextMemShiftLen-1 DO
          ii := NextMemShift[j];
          iused_mem := def.USED_MEM IN SEGM.code[ii].attrs.tag_attrs;
          iset_mem  := def.SET_MEM IN SEGM.code[ii].attrs.tag_attrs;
          IF (iset_mem & oused_mem &
              AreLocationsIntersect(SEGM.code[ii],o,MWRITE,MREAD))
             OR
             (iused_mem & oset_mem &
              AreLocationsIntersect(SEGM.code[ii],o,MREAD,MWRITE))
             OR
             (iset_mem & oset_mem &
              AreLocationsIntersect(SEGM.code[ii],o,MWRITE,MWRITE))
          THEN
              add_operation_in(o.pos,ii);
              add_operation_out(ii,o.pos);
          END;
      END;
      NextMemShift[NextMemShiftLen] := o.pos;
      INC(NextMemShiftLen);
   END;

   IF def.NOT_TRANSFER_LOCAL_MOVE IN o.attrs.tag_attrs THEN
       NOT_MOVABLE_MOVE_STORE_SCALE[NOT_MOVABLE_MOVE_STORE_SCALELen] := o.pos;
       INC(NOT_MOVABLE_MOVE_STORE_SCALELen);
   END;

END aux_infodeps;

PROCEDURE aux_backdeps(VAR o : def.Operation);
    VAR i: def.Register;
        j,k: INTEGER;
BEGIN
   FOR i:=def.MINREG TO def.MAXFREG DO
       IF i IN (o.attrs.res.r + o.attrs.arg.r)
       THEN
           IF ocir.BVUnionAssign(ocir.InOut[o.pos].out, SCALE_MASK.r[i]) THEN
               INCL(o.attrs.tag_attrs, def.HAS_OUT);
               FOR j := o.pos+1 TO SEGM_LEN-1 DO
                   IF ocir.BVIn(SCALE_MASK.r[i],j) THEN
                       add_operation_in(j,o.pos);
                       add_operation_out(o.pos,j);
                   END;
               END;
           END;
       END;
       IF i IN o.attrs.res.r THEN ocir.BVIncl(SCALE_MASK.r[i], o.pos); END;
   END;

   FOR k:=0 TO def.FlagsNum-1 DO
       IF k IN o.attrs.arg.f THEN
           IF ocir.BVUnionAssign(ocir.InOut[o.pos].out, SCALE_MASK.allf[k]) THEN
               INCL(o.attrs.tag_attrs, def.HAS_OUT);
               FOR j := o.pos+1 TO SEGM_LEN-1 DO
                   IF ocir.BVIn(SCALE_MASK.allf[k],j) THEN
                       add_operation_in(j,o.pos);
                       add_operation_out(o.pos,j);
                   END;
               END;
           END;
       END;
       IF k IN o.attrs.res.f THEN
           ocir.BVIncl(SCALE_MASK.allf[k], o.pos);
           IF ocir.BVUnionAssign(ocir.InOut[o.pos].out, SCALE_MASK.f[k]) THEN
               INCL(o.attrs.tag_attrs, def.HAS_OUT);
               FOR j := o.pos+1 TO SEGM_LEN-1 DO
                   IF ocir.BVIn(SCALE_MASK.f[k],j) THEN
                       add_operation_in(j,o.pos);
                       add_operation_out(o.pos,j);
                   END;
               END;
           END;

           IF def.SET_USED_FLAGS IN o.attrs.tag_attrs THEN
               ocir.BVIncl(SCALE_MASK.f[k], o.pos);
           END;
       END;
   END;
END aux_backdeps;

(* forward reordering *)
PROCEDURE aux_chainlength(VAR o : def.Operation);
VAR current_length, i : INTEGER;
BEGIN
   current_length := -1;
   FOR i := o.pos+1 TO SEGM_LEN-1 DO
       IF ocir.BVIn(ocir.InOut[o.pos].out,i) & (current_length < SEGM.code[i].attrs.chain)
       THEN
          current_length := SEGM.code[i].attrs.chain;
       END;
   END;
   o.attrs.chain := current_length + 1;
   IF o.attrs.chain > CHAIN_CNT THEN CHAIN_CNT := o.attrs.chain; END;
END aux_chainlength;

(* backward reordering *)
PROCEDURE aux_chainlength_back(VAR o : def.Operation);
VAR current_length, i : INTEGER;
BEGIN
   current_length := -1;
   FOR i := 0 TO o.pos-1 DO
       IF ocir.BVIn(ocir.InOut[o.pos].in,i) & (current_length < SEGM.code[i].attrs.chain)
       THEN
          current_length := SEGM.code[i].attrs.chain;
       END;
   END;
   o.attrs.chain := current_length + 1;
   IF o.attrs.chain > CHAIN_CNT THEN CHAIN_CNT := o.attrs.chain; END;
END aux_chainlength_back;

PROCEDURE EvalReady_Back();
VAR i, j : INTEGER;
BEGIN
   FOR i := 0 TO SEGM_LEN-1 DO
       FOR j := i+1 TO SEGM_LEN-1 DO
           INC(SEGM.code[i].attrs.ready,ORD(ocir.BVIn(ocir.InOut[i].out,j)));
       END;
   END;
END EvalReady_Back;

PROCEDURE ResetCHAIN_PRC*();
BEGIN
   IF MAX_POSSIBLE_CHAIN = 0 THEN CHAIN_PRC := 0;
   ELSE CHAIN_PRC := (CHAIN_CNT*100) DIV MAX_POSSIBLE_CHAIN;
   END;
END ResetCHAIN_PRC;

PROCEDURE ResetAGI_PRC*();
BEGIN
   IF AGI_CNT > MAX_POSSIBLE_AGI THEN
       AGI_PRC := 100;
       MAX_POSSIBLE_AGI := AGI_CNT;
   ELSIF MAX_POSSIBLE_AGI = 0 THEN AGI_PRC:= 0;
   ELSE AGI_PRC := (AGI_CNT*100) DIV MAX_POSSIBLE_AGI;
   END;
END ResetAGI_PRC;

(*
PROCEDURE ^ SetBefore();
PROCEDURE ^ SetAfter();
*)

PROCEDURE BuildGraph*(sg : ocir.Segment; forward : BOOLEAN);
VAR i, j : INTEGER;
BEGIN
   (*
   SetBefore;
   *)
   SEGM := sg;
   SEGM_LEN := SEGM.code_len;
   InitAuxScales();
   ocir.ClearInOut;

   MAX_POSSIBLE_CHAIN := SEGM_LEN-1;
   MAX_POSSIBLE_AGI := 0;
   CHAIN_CNT := 0;
   AGI_CNT := 0;

   (* collect informational (forward) dependences *)
   (* i.e. evaluate in and out attributes of SEGM's operations *)
   FOR i := 0 TO SEGM_LEN-1 DO aux_infodeps(SEGM.code^[i]); END;

   (* collect anti- and output (backward) dependences *)
   (* i.e continue evaliating of in and out attributes *)
   FOR i := SEGM_LEN-1 TO 0 BY -1 DO aux_backdeps(SEGM.code[i]); END;

   IF forward THEN
       (* evaluate max length of dep's chain from every operation to leaves *)
       (* evaluate power of `in'-set *)
       FOR i := SEGM_LEN-1 TO 0 BY -1 DO
           IF def.HAS_OUT IN SEGM.code[i].attrs.tag_attrs THEN
               aux_chainlength(SEGM.code[i]);
           END;
           IF def.HAS_IN IN SEGM.code[i].attrs.tag_attrs THEN
               FOR j := 0 TO i-1 DO
                   INC(SEGM.code[i].attrs.ready,
                       ORD(ocir.BVIn(ocir.InOut[i].in,j)));
               END;
           END;
       END;
   ELSE
       (* evaluate max length of dep's chain from every operation to root *)
       FOR i := 0 TO SEGM_LEN-1 DO aux_chainlength_back(SEGM.code[i]); END;
       (* evaluate power of `out'-set *)
       EvalReady_Back();
   END;

   IF MAX_POSSIBLE_CHAIN = 0 THEN CHAIN_PRC := 0;
   ELSE CHAIN_PRC := (CHAIN_CNT*100) DIV MAX_POSSIBLE_CHAIN;
   END;

   IF MAX_POSSIBLE_AGI = 0 THEN AGI_PRC:= 0;
   ELSE AGI_PRC := (AGI_CNT*100) DIV MAX_POSSIBLE_AGI;
   END;

   (*
   SetAfter;
   *)
END BuildGraph;

PROCEDURE HasInfoDeps*(sg : ocir.Segment;
                       from,to : INTEGER;
                       VAR include_r,
                           include_f,
                           include_m : BOOLEAN) : BOOLEAN;
   VAR from_op, to_op : def.Operation;
BEGIN
   from_op := sg.code[from];
   to_op   := sg.code[to];
   include_r := from_op.attrs.res.r * to_op.attrs.arg.r <> def.Regs{};
   include_f := from_op.attrs.res.f * to_op.attrs.arg.f <> def.Flags{};
   include_m := (def.SET_MEM IN from_op.attrs.tag_attrs) AND
                (def.USED_MEM IN to_op.attrs.tag_attrs)  AND
                AreLocationsIntersect(from_op,to_op,MWRITE,MREAD);
   RETURN include_r OR include_f OR include_m;
END HasInfoDeps;

PROCEDURE HasAntiDeps*(sg : ocir.Segment;
                       from,to : INTEGER;
                       VAR include_r,
                           include_f,
                           include_m : BOOLEAN) : BOOLEAN;
   VAR from_op, to_op : def.Operation;
BEGIN
   from_op := sg.code[from];
   to_op   := sg.code[to];
   include_r := from_op.attrs.arg.r * to_op.attrs.res.r <> def.Regs{};
   include_f := from_op.attrs.arg.f * to_op.attrs.res.f <> def.Flags{};
   include_m := (def.USED_MEM IN from_op.attrs.tag_attrs) AND
                (def.SET_MEM IN to_op.attrs.tag_attrs)    AND
                AreLocationsIntersect(from_op,to_op,MREAD,MWRITE);
   RETURN include_r OR include_f OR include_m;
END HasAntiDeps;

PROCEDURE HasOutputDeps*(sg : ocir.Segment;
                         from,to : INTEGER;
                         VAR include_r,
                             include_f,
                             include_m : BOOLEAN) : BOOLEAN;
   VAR from_op, to_op : def.Operation;
BEGIN
   from_op := sg.code[from];
   to_op   := sg.code[to];
   include_r := from_op.attrs.res.r * to_op.attrs.res.r <> def.Regs{};
   include_f := from_op.attrs.res.f * to_op.attrs.res.f <> def.Flags{};
   include_m := (def.SET_MEM IN from_op.attrs.tag_attrs) AND
                (def.SET_MEM IN to_op.attrs.tag_attrs)   AND
                AreLocationsIntersect(from_op,to_op,MWRITE,MWRITE);
   RETURN include_r OR include_f OR include_m;
END HasOutputDeps;

PROCEDURE NotDependant*(sg : ocir.Segment; from, to : INTEGER) : BOOLEAN;
   VAR r,f,m : BOOLEAN;
BEGIN
   RETURN NOT (HasInfoDeps(sg,from,to,r,f,m) OR
               HasAntiDeps(sg,from,to,r,f,m) OR
               HasOutputDeps(sg,from,to,r,f,m));
END NotDependant;

VAR InitOcGraph* : PROCEDURE();

PROCEDURE InitOcGraphEmpty();
END InitOcGraphEmpty;

PROCEDURE InitOcGraphFull();
VAR i : def.Register;
    j: INTEGER;
BEGIN
    FOR i:=def.MINREG TO def.MAXFREG DO
        SCALE_MASK.r[i] := ocir.BVNew();
    END;
    FOR j:=0 TO def.FlagsNum-1 DO
        SCALE_MASK.f[j] := ocir.BVNew();
        SCALE_MASK.allf[j] := ocir.BVNew();
        InitOcGraph := InitOcGraphEmpty;
    END;
END InitOcGraphFull;

(*
VAR ocgraph_total_time* : LONGINT;
    before,
    after  : LONGINT;

PROCEDURE time (VAR tm: LONGINT);
VAR t: SysClock.DateTime;
BEGIN
    SysClock.GetClock (t);
    tm := (t.hour * 60 + t.minute) * 60 + t.second;
END time;

PROCEDURE SetBefore*();
BEGIN
    time(before);
END SetBefore;

PROCEDURE SetAfter*();
BEGIN
    time(after);
    INC(ocgraph_total_time,after-before);
    opIO.print("ellapsed by OcGraph %d\n",ocgraph_total_time);
END SetAfter;
*)

BEGIN
    (*
    ocgraph_total_time := 0;
    *)
    InitOcGraph := InitOcGraphFull;
END OcGraph.
