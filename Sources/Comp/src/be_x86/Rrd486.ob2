(* Created by KDV;
   reordering of instructions for 486 processor
*)
MODULE Rrd486;

IMPORT ocir;
IMPORT def := OcirDef;
IMPORT ss  := ScheStr;

VAR
(* operations choosen for float and u pipe in current cycle *)
       f_op, f_op_coding,
       u_op, u_op_coding : INTEGER;

(* length of emitted part *)
       new_len           : INTEGER;

       RaSet             : def.Regs;

TYPE   INT_SCALE = ARRAY ocir.PORTION_SIZE OF INTEGER;

VAR
       NotEmitted        : INT_SCALE;
       NotEmittedLen     : INTEGER;

       MemAccessSeq      : SHORTINT;

CONST  MaxAnalizedMemAccessSeq = 4;
       SOSMemAccess            = 2;

PROCEDURE UsedMem0(op : def.Operation) : BOOLEAN;
BEGIN
   RETURN ((def.GetOpOpndKind(op.dest.r) = def.MEM) OR
           (def.GetOpOpndKind(op.src.r) = def.MEM))
          AND
          (def.GetOpGroup(op.code) IN
           def.OpGroupSet{def.GDT_OGROUP,def.A_OGROUP,def.L_OGROUP});
END UsedMem0;

PROCEDURE Emit(pos_coding : INTEGER; changeNotEmitted : BOOLEAN);
   VAR pos     : INTEGER;
BEGIN
   pos := NotEmitted[pos_coding];
   IF MemAccessSeq = MaxAnalizedMemAccessSeq THEN MemAccessSeq := 0; END;
   IF NOT UsedMem0(ss.SEGM.code[pos]) THEN
       MemAccessSeq := 0;
   ELSIF MemAccessSeq = MaxAnalizedMemAccessSeq THEN
       MemAccessSeq := 1;
   ELSE
       INC(MemAccessSeq);
   END;
   EXCL(ss.SEGM.code[pos].attrs.tag_attrs, def.MOV_ESP_LOCAL);
   ss.segm_changed := ss.segm_changed OR (new_len <> pos);
   IF (ss.SEGM.code[pos].PUSH_SIZE <>
       ss.SEGM.code[pos].NEXT_PUSH_SIZE) OR
       (def.ESP IN ss.SEGM.code[pos].attrs.res.r) THEN
      ss.CURRENT_PUSH_SIZE := ss.SEGM.code[pos].NEXT_PUSH_SIZE;
   ELSE
       ss.SEGM.code[pos].PUSH_SIZE := ss.CURRENT_PUSH_SIZE;
   END;
   ss.res_segm.code[new_len] := ss.SEGM.code[pos];
   INC(new_len);
   IF changeNotEmitted THEN
       DEC(NotEmittedLen);
       NotEmitted[pos_coding] := NotEmitted[NotEmittedLen];
   END;
END Emit;

PROCEDURE EmitFU();
   VAR LenSub2,LenSub1 : INTEGER;
BEGIN
   LenSub2 := NotEmittedLen-2;
   LenSub1 := NotEmittedLen-1;
   Emit(f_op_coding,FALSE);
   Emit(u_op_coding,FALSE);
   IF f_op_coding >= LenSub2 THEN
       IF u_op_coding >= LenSub2 THEN
       ELSIF f_op_coding = LenSub2 THEN
           NotEmitted[u_op_coding] := NotEmitted[LenSub1];
       ELSE
           NotEmitted[u_op_coding] := NotEmitted[LenSub2];
       END;
   ELSIF u_op_coding = LenSub2 THEN
       NotEmitted[f_op_coding] := NotEmitted[LenSub1];
   ELSIF u_op_coding = LenSub1 THEN
       NotEmitted[f_op_coding] := NotEmitted[LenSub2];
   ELSE
       NotEmitted[f_op_coding] := NotEmitted[LenSub1];
       NotEmitted[u_op_coding] := NotEmitted[LenSub2];
   END;
   DEC(NotEmittedLen,2);
END EmitFU;


PROCEDURE DecUse(pos : INTEGER);
   VAR out : ocir.BV;
       i,ii: INTEGER;
BEGIN
   IF NOT (def.HAS_OUT IN ss.SEGM.code[pos].attrs.tag_attrs) THEN RETURN END;
   out := ocir.InOut[pos].out;
   FOR ii:=0 TO NotEmittedLen-1  DO
       i := NotEmitted[ii];
       IF ocir.BVIn(out,i) THEN
           DEC(ss.SEGM.code[i].attrs.ready,
               ORD(ss.SEGM.code[i].attrs.ready > 0));
           ocir.BVExcl(ocir.InOut[i].in,pos);
       END;
   END;
END DecUse;

PROCEDURE SetRa(op- : def.Operation) : INTEGER;
   VAR i,ii: INTEGER;
       out : ocir.BV;
       rs  : def.Regs;
       current_distance : INTEGER;
       was_esp : BOOLEAN;
BEGIN
   rs := op.attrs.res.r;
   IF rs = def.Regs{} THEN RETURN 2 END;
   current_distance := 2;
   was_esp := def.ESP IN rs;
   out := ocir.InOut[op.pos].out;
   FOR ii:=0 TO NotEmittedLen-1 DO
       i := NotEmitted[ii];
       IF (ocir.BVIn(out,i) OR
           (was_esp AND (def.MOV_ESP_LOCAL IN ss.SEGM.code[i].attrs.tag_attrs))) AND
          ((rs * ss.SEGM.code[i].attrs.arg.ra) <> def.Regs{}) AND
          (ss.SEGM.code[i].attrs.ready < current_distance) THEN
           current_distance := ss.SEGM.code[i].attrs.ready;
       END;
   END;
   RETURN current_distance;
END SetRa;

(* variables used by criteria *)
VAR cur_nu, cur_u : def.Operation;
    cur_res : BOOLEAN;

TYPE UCriterion = PROCEDURE () : BOOLEAN;
(* criterion returns tag indicating need to return from calling procedure *)

PROCEDURE UChain() : BOOLEAN;
BEGIN
    cur_res := cur_u.attrs.chain < cur_nu.attrs.chain;
    RETURN cur_u.attrs.chain <> cur_nu.attrs.chain;
END UChain;

PROCEDURE UAgi() : BOOLEAN;
VAR  old_set_ra,
     new_set_ra : INTEGER;
BEGIN
    (* setra *)
    old_set_ra := SetRa(cur_u);
    new_set_ra := SetRa(cur_nu);
    IF old_set_ra <> new_set_ra THEN
        cur_res := old_set_ra < new_set_ra;
        RETURN TRUE;
    END;
    (* used local esp scale *)
    cur_res := (NOT (def.MOV_ESP_LOCAL IN cur_u.attrs.tag_attrs)) AND
               (def.MOV_ESP_LOCAL IN cur_nu.attrs.tag_attrs);
    RETURN cur_res;
END UAgi;

PROCEDURE UUsedMem() : BOOLEAN;
BEGIN
    cur_res := cur_u.attrs.used_mem > cur_nu.attrs.used_mem;
    RETURN cur_u.attrs.used_mem <> cur_nu.attrs.used_mem;
END UUsedMem;

VAR UC1, UC2, UC3 : UCriterion;

PROCEDURE BetterU(new_u,old_u : INTEGER) : BOOLEAN;
BEGIN
    IF old_u = def.UNDEF_POS THEN RETURN TRUE; END;
    cur_u  := ss.SEGM.code[old_u];
    cur_nu := ss.SEGM.code[new_u];
    IF UC1() THEN RETURN cur_res; END;
    IF (MemAccessSeq > SOSMemAccess) AND UsedMem0(cur_u) AND
       (NOT UsedMem0(cur_nu)) THEN RETURN TRUE;
    END;
    IF UC2() THEN RETURN cur_res; END;
    IF UC3() THEN RETURN cur_res; END;
    RETURN FALSE;
END BetterU;

PROCEDURE ExtractReady();
CONST
  f_max_chain = -1;

VAR
  i,ii: INTEGER;
  c_op: def.Operation;

BEGIN
   IF u_op <> def.UNDEF_POS THEN
       RaSet := ss.SEGM.code[u_op].attrs.res.r;
   ELSE RaSet := def.Regs{};
   END;
   u_op := def.UNDEF_POS;
   f_op := def.UNDEF_POS;
   FOR ii:=0 TO NotEmittedLen-1 DO
       i := NotEmitted[ii];
       c_op := ss.SEGM.code[i];
       IF (c_op.attrs.ready <= 0) AND
          ((c_op.attrs.arg.ra * RaSet) = def.Regs{}) THEN
           IF c_op.attrs.pair IN def.PairTags{def.FP,def.FNP} THEN
               IF c_op.attrs.chain >= f_max_chain THEN
                   f_op        := i;
                   f_op_coding := ii;
               END;
           ELSIF BetterU(i,u_op) THEN
               u_op := i;
               u_op_coding := ii;
           END;
       END;
   END;
   IF f_op <> def.UNDEF_POS THEN
       DecUse(f_op);
       IF u_op <> def.UNDEF_POS THEN
          DecUse(u_op);
          EmitFU();
       ELSE
          Emit(f_op_coding,TRUE);
       END;
   ELSIF u_op <> def.UNDEF_POS THEN
       DecUse(u_op);
       Emit(u_op_coding,TRUE);
   END;
END ExtractReady;

PROCEDURE Reordering();
BEGIN
   REPEAT ExtractReady();  UNTIL new_len = ss.SEGM_LEN;
END Reordering;

PROCEDURE DoReOrder*(sg : ocir.Segment);
   VAR i : INTEGER;
BEGIN
   IF sg.code_len = 0 THEN RETURN END;
   ss.ReorderingPrologue(sg,TRUE,TRUE);

   (* initialization of local structures *)
   FOR i:=0 TO ss.SEGM_LEN-1 DO NotEmitted[i] := i; END;
   NotEmittedLen := ss.SEGM_LEN;
   new_len := 0;
   u_op := def.UNDEF_POS;
   MemAccessSeq := 0;

   (* main step *)
   Reordering();
   ss.ReorderingEpilogue();

   IF ss.segm_changed THEN sg.code := ss.res_segm.code;END;

   (* food for garbage collector *)
END DoReOrder;

BEGIN
   UC1  := UAgi;
   UC2  := UChain;
   UC3  := UUsedMem;
END Rrd486.
