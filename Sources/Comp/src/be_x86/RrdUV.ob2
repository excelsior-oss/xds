(* Created by KDV;
   reordering of instructions for Pentium and 386 processors
*)
MODULE RrdUV;

IMPORT ocir,
       def := OcirDef,
       ss  := ScheStr,
       (*
       SysClock,
       opIO,
       *)
       g   := OcGraph;

VAR    no_analized : ocir.BV; (* operations with zero ready and
                                 cycle = current cycle *)

TYPE   INT_SCALE = ARRAY ocir.PORTION_SIZE OF INTEGER;

VAR
(* operations which can be executed on corresponding pipe *)
       u_pretendent,
       v_pretendent,
       f_pretendent      : INT_SCALE;
       u_pretendentLen,
       v_pretendentLen,
       f_pretendentLen   : INTEGER;
(*
   operations choosen for float, u and v pipe in current cycle
   and their coding in NotEmitted or NotEmittedF tables
*)
       f_op, f_op_coding,
       u_op, u_op_coding,
       v_op, v_op_coding : INTEGER;

(* length of emitted part *)
       new_len           : INTEGER;

(* first free cycle *)
       current_cycle,
(* next free cycle *)
       next_cycle        : LONGINT;

(* registers changed by last choosen u and v instructions *)
       RaSet             : def.Regs;

(* tables (for f pipe and for u/v pipes)
   containing instructons which are not emitted yet
*)
       NotEmittedF,
       NotEmitted        : INT_SCALE;
(* lengths of these tables *)
       NotEmittedFLen,
       NotEmittedLen     : INTEGER;


PROCEDURE EmitF(pos_coding : INTEGER);
   VAR pos     : INTEGER;
BEGIN
   pos := NotEmittedF[pos_coding];
   EXCL(ss.SEGM.code[pos].attrs.tag_attrs,def.MOV_ESP_LOCAL);
   ss.segm_changed := ss.segm_changed OR (new_len <> pos);
   IF (ss.SEGM.code[pos].PUSH_SIZE <> ss.SEGM.code[pos].NEXT_PUSH_SIZE) OR
      (def.ESP IN ss.SEGM.code[pos].attrs.res.r)
   THEN
      ss.CURRENT_PUSH_SIZE := ss.SEGM.code[pos].NEXT_PUSH_SIZE;
   ELSE
       ss.SEGM.code[pos].PUSH_SIZE := ss.CURRENT_PUSH_SIZE;
   END;
   ss.res_segm.code[new_len] := ss.SEGM.code[pos];
   IF def.WITH_AGI IN ss.SEGM.code[pos].attrs.tag_attrs THEN
       DEC(g.AGI_CNT);
       g.ResetAGI_PRC();
   END;
   INC(new_len);
   DEC(NotEmittedFLen);
   NotEmittedF[pos_coding] := NotEmittedF[NotEmittedFLen];
END EmitF;

PROCEDURE Emit(pos_coding : INTEGER; changeNotEmitted : BOOLEAN);
   VAR pos     : INTEGER;
BEGIN
   pos := NotEmitted[pos_coding];
   EXCL(ss.SEGM.code[pos].attrs.tag_attrs,def.MOV_ESP_LOCAL);
   ss.segm_changed := ss.segm_changed OR (new_len <> pos);
   IF (ss.SEGM.code[pos].PUSH_SIZE <> ss.SEGM.code[pos].NEXT_PUSH_SIZE) OR
      (def.ESP IN ss.SEGM.code[pos].attrs.res.r)
   THEN
      ss.CURRENT_PUSH_SIZE := ss.SEGM.code[pos].NEXT_PUSH_SIZE;
   ELSE
       ss.SEGM.code[pos].PUSH_SIZE := ss.CURRENT_PUSH_SIZE;
   END;
   ss.res_segm.code[new_len] := ss.SEGM.code[pos];
   IF def.WITH_AGI IN ss.SEGM.code[pos].attrs.tag_attrs THEN
       DEC(g.AGI_CNT);
       g.ResetAGI_PRC();
   END;
   INC(new_len);
   IF changeNotEmitted THEN
       DEC(NotEmittedLen);
       NotEmitted[pos_coding] := NotEmitted[NotEmittedLen];
   END;
END Emit;

PROCEDURE EmitUV();
   VAR LenSub2,LenSub1 : INTEGER;
BEGIN
   LenSub2 := NotEmittedLen-2;
   LenSub1 := NotEmittedLen-1;
   Emit(u_op_coding,FALSE);
   Emit(v_op_coding,FALSE);
   IF u_op_coding >= LenSub2 THEN
       IF v_op_coding >= LenSub2 THEN
       ELSIF u_op_coding = LenSub2 THEN
           NotEmitted[v_op_coding] := NotEmitted[LenSub1];
       ELSE
           NotEmitted[v_op_coding] := NotEmitted[LenSub2];
       END;
   ELSIF v_op_coding = LenSub2 THEN
       NotEmitted[u_op_coding] := NotEmitted[LenSub1];
   ELSIF v_op_coding = LenSub1 THEN
       NotEmitted[u_op_coding] := NotEmitted[LenSub2];
   ELSE
       NotEmitted[u_op_coding] := NotEmitted[LenSub1];
       NotEmitted[v_op_coding] := NotEmitted[LenSub2];
   END;
   DEC(NotEmittedLen,2);
END EmitUV;

PROCEDURE DecUseF(f_op : INTEGER);
   VAR out    : ocir.BV;
       ii, i  : INTEGER;
       clocks : SHORTINT;
       f_op_o : def.Operation;
BEGIN
   f_op_o := ss.SEGM.code[f_op];
   clocks := f_op_o.attrs.clocks;
   IF NOT (def.HAS_OUT IN f_op_o.attrs.tag_attrs) THEN RETURN END;
   out := ocir.InOut[f_op].out;
   FOR ii:=0 TO NotEmittedLen-1  DO
       i := NotEmitted[ii];
       IF ocir.BVIn(out,i) THEN
           IF next_cycle > ss.SEGM.code[i].attrs.cycle THEN
               next_cycle := ss.SEGM.code[i].attrs.cycle;
           END;
           DEC(ss.SEGM.code[i].attrs.ready,
               ORD(ss.SEGM.code[i].attrs.ready > 0));
           ocir.BVExcl(ocir.InOut[i].in,f_op);
       END;
   END;
   FOR ii:=0 TO NotEmittedFLen-1 DO
       i := NotEmittedF[ii];
       IF ocir.BVIn(out,i) THEN
           IF ss.SEGM.code[i].attrs.cycle < current_cycle+clocks THEN
               ss.SEGM.code[i].attrs.cycle := current_cycle+clocks;
           END;
           IF next_cycle > ss.SEGM.code[i].attrs.cycle THEN
               next_cycle := ss.SEGM.code[i].attrs.cycle;
           END;
           DEC(ss.SEGM.code[i].attrs.ready,
               ORD(ss.SEGM.code[i].attrs.ready > 0));
           ocir.BVExcl(ocir.InOut[i].in,f_op);
       END;
   END;
END DecUseF;

PROCEDURE DecUse(pos : INTEGER);
   VAR out   : ocir.BV;
       ii, i : INTEGER;

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
   FOR ii:=0 TO NotEmittedFLen-1 DO
       i := NotEmittedF[ii];
       IF ocir.BVIn(out,i) THEN
           DEC(ss.SEGM.code[i].attrs.ready,
               ORD(ss.SEGM.code[i].attrs.ready > 0));
           ocir.BVExcl(ocir.InOut[i].in,pos);
       END;
   END;
END DecUse;

PROCEDURE ExtractReady() : BOOLEAN;
   VAR i,ii    : INTEGER;
   VAR c_op    : def.Operation;
   VAR res,
       include : BOOLEAN;

BEGIN
   g.CHAIN_CNT := 0;
   u_pretendentLen := 0;
   v_pretendentLen := 0;
   f_pretendentLen := 0;
   res := FALSE;
   FOR ii:=0 TO NotEmittedLen-1 DO
       i := NotEmitted[ii];
       c_op := ss.SEGM.code[i];
       IF c_op.attrs.chain > g.CHAIN_CNT THEN
           g.CHAIN_CNT := c_op.attrs.chain;
       END;
       include :=
          (c_op.attrs.ready = 0) AND
          (c_op.attrs.cycle <= current_cycle) AND
          ((c_op.attrs.arg.ra * RaSet) = def.Regs{});
       IF include THEN
           res := TRUE;
           u_pretendent[u_pretendentLen] := ii;
           INC(u_pretendentLen);
           IF c_op.attrs.pair IN def.PairTags{def.UV, def.PV} THEN
               v_pretendent[v_pretendentLen] := ii;
               INC(v_pretendentLen);
           END;
       END;
   END;
   FOR ii:=0 TO NotEmittedFLen-1 DO
       i := NotEmittedF[ii];
       c_op := ss.SEGM.code[i];
       IF c_op.attrs.chain > g.CHAIN_CNT THEN
           g.CHAIN_CNT := c_op.attrs.chain;
       END;
       include :=
          ((c_op.attrs.arg.ra * RaSet) = def.Regs{}) AND
          (c_op.attrs.ready = 0) AND
          (c_op.attrs.cycle <= current_cycle);
       IF include THEN
           res := TRUE;
           f_pretendent[f_pretendentLen] := ii;
           INC(f_pretendentLen);
       END;
   END;
   g.ResetCHAIN_PRC();
   f_op := def.UNDEF_POS;
   u_op := def.UNDEF_POS;
   v_op := def.UNDEF_POS;
   RETURN res;
END ExtractReady;

(* return coding for waiting FXCH *)
PROCEDURE FXCH_Exists(VAR fxch_coding : INTEGER) : BOOLEAN;
VAR i, ii : INTEGER;
BEGIN
    FOR ii:=0 TO NotEmittedFLen-1 DO
        i := NotEmittedF[ii];
        IF (i <> f_op) & (ss.SEGM.code[i].attrs.ready = 0) &
           (ss.SEGM.code[i].code = def.FXCH)
        THEN
            fxch_coding := ii;
            RETURN TRUE;
        END;
    END;
    fxch_coding := def.UNDEF_POS;
    RETURN FALSE;
END FXCH_Exists;

PROCEDURE EvalCycles(op : INTEGER; first_cycle : LONGINT);
   VAR ii, i : INTEGER;
BEGIN
   IF NOT (def.HAS_OUT IN ss.SEGM.code[op].attrs.tag_attrs) THEN RETURN END;
   FOR ii:=0 TO NotEmittedLen-1  DO
       i := NotEmitted[ii];
       IF ocir.BVIn(ocir.InOut[op].out,i) THEN
           IF ss.SEGM.code[i].attrs.cycle < first_cycle THEN
               ss.SEGM.code[i].attrs.cycle := first_cycle;
           END;
           IF next_cycle > ss.SEGM.code[i].attrs.cycle THEN
               next_cycle := ss.SEGM.code[i].attrs.cycle;
           END;
       END;
   END;
   FOR ii:=0 TO NotEmittedFLen-1 DO
       i := NotEmittedF[ii];
       IF ocir.BVIn(ocir.InOut[op].out,i) THEN
           IF ss.SEGM.code[i].attrs.cycle < first_cycle THEN
               ss.SEGM.code[i].attrs.cycle := first_cycle;
           END;
           IF next_cycle > ss.SEGM.code[i].attrs.cycle THEN
               next_cycle := ss.SEGM.code[i].attrs.cycle;
           END;
       END;
   END;
END EvalCycles;

VAR uANDv_out : ocir.BV;

PROCEDURE EvalCyclesUV(first_cycle : LONGINT);
VAR ii, i : INTEGER;
BEGIN
   IF NOT (def.HAS_OUT IN
           (ss.SEGM.code[u_op].attrs.tag_attrs +
            ss.SEGM.code[v_op].attrs.tag_attrs)) THEN
       RETURN;
   END;
   ocir.BVUnion(ocir.InOut[u_op].out,ocir.InOut[v_op].out,uANDv_out);
   FOR ii:=0 TO NotEmittedLen-1 DO
       i := NotEmitted[ii];
       IF ocir.BVIn(uANDv_out,i) THEN
           IF ss.SEGM.code[i].attrs.cycle < first_cycle THEN
               ss.SEGM.code[i].attrs.cycle := first_cycle;
           END;
           IF next_cycle > ss.SEGM.code[i].attrs.cycle THEN
               next_cycle := ss.SEGM.code[i].attrs.cycle;
           END;
       END;
   END;
   FOR ii:=0 TO NotEmittedFLen-1 DO
       i := NotEmittedF[ii];
       IF ocir.BVIn(uANDv_out,i) THEN
           IF ss.SEGM.code[i].attrs.cycle < first_cycle THEN
               ss.SEGM.code[i].attrs.cycle := first_cycle;
           END;
           IF next_cycle > ss.SEGM.code[i].attrs.cycle THEN
               next_cycle := ss.SEGM.code[i].attrs.cycle;
           END;
       END;
   END;
END EvalCyclesUV;

PROCEDURE GetFInstr();
   VAR max_chain, i,ii : INTEGER;
       op : def.Operation;
BEGIN
   max_chain := -1;
   FOR ii:=0 TO f_pretendentLen-1 DO
       i := NotEmittedF[f_pretendent[ii]];
       op := ss.SEGM.code[i];
       IF ((f_op = def.UNDEF_POS)
           OR
           ((op.attrs.chain > max_chain) AND
             ((op.code <> def.FXCH) OR
              (ss.SEGM.code[f_op].code = def.FXCH)))
           OR
           ((op.code <> def.FXCH) AND
             (ss.SEGM.code[f_op].code = def.FXCH)))
       THEN
           f_op := i;
           f_op_coding := f_pretendent[ii];
           max_chain := ss.SEGM.code[i].attrs.chain;
       END;
   END;
   IF f_op <> def.UNDEF_POS THEN EmitF(f_op_coding); END;
END GetFInstr;

PROCEDURE SetRa(op : def.Operation; lookup_area: SHORTINT) : INTEGER;
   VAR i,ii: INTEGER;
       out : ocir.BV;
       rs  : def.Regs;
       current_distance : INTEGER;
       was_esp : BOOLEAN;
BEGIN
   current_distance := lookup_area + 1;
   rs := op.attrs.res.r;
   IF (rs = def.Regs{}) OR (NOT (def.HAS_OUT IN op.attrs.tag_attrs)) THEN
       RETURN current_distance;
   END;
   was_esp := def.ESP IN rs;
   out := ocir.InOut[op.pos].out;
   FOR ii:=0 TO NotEmittedLen-1 DO
       i := NotEmitted[ii];
       IF (ocir.BVIn(out,i) OR
          (was_esp AND (def.MOV_ESP_LOCAL IN ss.SEGM.code[i].attrs.tag_attrs))) AND
          ((rs * ss.SEGM.code[i].attrs.arg.ra) <> def.Regs{}) AND
           (ss.SEGM.code[i].attrs.ready < current_distance)
       THEN
           current_distance := ss.SEGM.code[i].attrs.ready;
       END;
   END;
   RETURN current_distance;
END SetRa;

(* variables used by criteria *)
VAR cur_nu, cur_nv, cur_u, cur_v : def.Operation;
    cur_res : BOOLEAN;

TYPE UCriterion = PROCEDURE () : BOOLEAN;
TYPE VCriterion = PROCEDURE () : BOOLEAN;
(* criterion returns tag indicating need to return from calling procedure *)

PROCEDURE PairingForU(new,old : def.Operation;
                      VAR res : BOOLEAN) : BOOLEAN;
BEGIN
    CASE old.attrs.pair OF
    | def.PU: res := FALSE;
              RETURN new.attrs.pair <> def.PU;
    | def.UV: res := new.attrs.pair = def.PU;
              RETURN res OR (new.attrs.pair <> def.UV);
    | def.NP: res := new.attrs.pair IN def.PairTags{def.UV,def.PU};
              RETURN res OR (new.attrs.pair <> def.NP);
    ELSE
        res := new.attrs.pair <> def.PV;
        RETURN res;
    END;
END PairingForU;

PROCEDURE PairingForV(new,old : def.Operation;
                      VAR res : BOOLEAN) : BOOLEAN;
BEGIN
    CASE old.attrs.pair OF
    | def.PV: res := FALSE;
              RETURN new.attrs.pair <> def.PV;
    | def.UV: res := new.attrs.pair = def.PV;
              RETURN res;
    END;
END PairingForV;

PROCEDURE UPairing() : BOOLEAN;
BEGIN
    RETURN PairingForU(cur_nu,cur_u,cur_res);
END UPairing;

PROCEDURE VPairing() : BOOLEAN;
BEGIN
    RETURN PairingForV(cur_nv,cur_v,cur_res);
END VPairing;

PROCEDURE UChain() : BOOLEAN;
BEGIN
    cur_res := cur_u.attrs.chain < cur_nu.attrs.chain;
    RETURN cur_u.attrs.chain <> cur_nu.attrs.chain;
END UChain;

PROCEDURE VChain() : BOOLEAN;
BEGIN
    cur_res := cur_v.attrs.chain < cur_nv.attrs.chain;
    RETURN cur_v.attrs.chain <> cur_nv.attrs.chain;
END VChain;

PROCEDURE UAgi() : BOOLEAN;
VAR  old_set_ra,
     new_set_ra : INTEGER;
BEGIN
    (* setra *)
    old_set_ra := SetRa(cur_u, 2);
    new_set_ra := SetRa(cur_nu, 2);
    IF old_set_ra <> new_set_ra THEN
        cur_res := old_set_ra < new_set_ra;
        RETURN TRUE;
    END;
    (* used local esp scale *)
    cur_res := (NOT (def.MOV_ESP_LOCAL IN cur_u.attrs.tag_attrs)) AND
               (def.MOV_ESP_LOCAL IN cur_nu.attrs.tag_attrs);
    RETURN cur_res;
END UAgi;

PROCEDURE VAgi() : BOOLEAN;
VAR  old_set_ra,
     new_set_ra : INTEGER;
BEGIN
    (* setra *)
    old_set_ra := SetRa(cur_v, 2);
    new_set_ra := SetRa(cur_nv, 2);
    IF old_set_ra <> new_set_ra THEN
        cur_res := old_set_ra < new_set_ra;
        RETURN TRUE;
    END;
    (* used local esp scale *)
    cur_res := (NOT (def.MOV_ESP_LOCAL IN cur_v.attrs.tag_attrs)) AND
               (def.MOV_ESP_LOCAL IN cur_nv.attrs.tag_attrs);
    RETURN cur_res;
END VAgi;

VAR UC1, UC2, UC3 : UCriterion;
VAR VC1, VC2, VC3 : UCriterion;

PROCEDURE BetterU(new_u,old_u : INTEGER) : BOOLEAN;
BEGIN
    cur_u  := ss.SEGM.code[old_u];
    cur_nu := ss.SEGM.code[new_u];
    IF UC1() OR UC2() OR UC3() THEN RETURN cur_res; END;
    RETURN FALSE;
END BetterU;

PROCEDURE BetterV(new_v,old_v : INTEGER) : BOOLEAN;
BEGIN
    cur_v  := ss.SEGM.code[old_v];
    cur_nv := ss.SEGM.code[new_v];
    IF VC1() OR VC2() OR VC3() THEN RETURN cur_res; END;
    RETURN FALSE;
END BetterV;

TYPE UVCriterion = PROCEDURE () : BOOLEAN;
(* criterion returns tag indicating need to return from calling procedure *)

PROCEDURE UVClocks () : BOOLEAN;
VAR  old_clocks_dif,
     new_clocks_dif : SHORTINT;
BEGIN
   old_clocks_dif := ABS(cur_u.attrs.clocks -  cur_v.attrs.clocks);
   new_clocks_dif := ABS(cur_nu.attrs.clocks - cur_nv.attrs.clocks);
   cur_res := old_clocks_dif > new_clocks_dif;
   RETURN old_clocks_dif <> new_clocks_dif;
END UVClocks;

PROCEDURE UVPairing () : BOOLEAN;
   VAR better_u, better_v,
       res1, res2           : BOOLEAN;
BEGIN
   res1 := PairingForU(cur_nu,cur_u,better_u);
   res2 := PairingForV(cur_nv,cur_v,better_v);
   cur_res := better_u OR better_v;
   RETURN res1 AND res2;
END UVPairing;

PROCEDURE UVChain () : BOOLEAN;
   VAR old_chain, new_chain : INTEGER;
BEGIN
   old_chain := cur_u.attrs.chain + cur_v.attrs.chain;
   new_chain := cur_nu.attrs.chain + cur_nv.attrs.chain;
   cur_res := old_chain < new_chain;
   RETURN old_chain <> new_chain;
END UVChain;

PROCEDURE UVAgi () : BOOLEAN;
VAR old_set_ra,
    new_set_ra,
    old_in_esp_scale,
    new_in_esp_scale : LONGINT;
BEGIN
   (* setra *)
   old_set_ra := SetRa(cur_u, 3)  + SetRa(cur_v, 2);
   new_set_ra := SetRa(cur_nu, 3) + SetRa(cur_nv, 2);
   IF old_set_ra <> new_set_ra THEN
       cur_res := old_set_ra < new_set_ra;
       RETURN TRUE;
   END;
   (* used local esp scale *)
   old_in_esp_scale :=
       ORD(def.MOV_ESP_LOCAL IN cur_u.attrs.tag_attrs) +
       ORD(def.MOV_ESP_LOCAL IN cur_v.attrs.tag_attrs);
   new_in_esp_scale :=
       ORD(def.MOV_ESP_LOCAL IN cur_nu.attrs.tag_attrs) +
       ORD(def.MOV_ESP_LOCAL IN cur_nv.attrs.tag_attrs);
   cur_res := old_in_esp_scale < new_in_esp_scale;
   RETURN cur_res;
END UVAgi;

VAR UVC1, UVC2, UVC3, UVC4 : UVCriterion;

PROCEDURE BetterPair(new_u,new_v,old_u,old_v : INTEGER) : BOOLEAN;
BEGIN
   IF old_v = def.UNDEF_POS THEN
       RETURN (new_v <> def.UNDEF_POS) OR BetterU(new_u,old_u);
   ELSIF new_v = def.UNDEF_POS THEN RETURN FALSE;
   END;
   cur_u  := ss.SEGM.code[old_u];
   cur_v  := ss.SEGM.code[old_v];
   cur_nu := ss.SEGM.code[new_u];
   cur_nv := ss.SEGM.code[new_v];
   IF UVC1() OR UVC2() OR UVC3() OR UVC4() THEN RETURN cur_res; END;
   RETURN FALSE;
END BetterPair;

PROCEDURE ExtractBestUV();
VAR old_u,old_v,new_u,new_v : INTEGER;
    uu, vv : INTEGER;
    oldRaSet : def.Regs;
BEGIN
   (* current initialization of criterion *)
   IF g.CHAIN_PRC > g.AGI_PRC THEN
       UC2  := UChain;  UC3  := UAgi;
       UVC3 := UVChain; UVC4 := UVAgi;
   ELSE
       UC2  := UAgi;    UC3  := UChain;
       UVC3 := UVAgi;   UVC4 := UVChain;
   END;

   old_u := NotEmitted[u_pretendent[0]];
   u_op_coding := u_pretendent[0];
   old_v := def.UNDEF_POS;
   FOR uu := 0 TO u_pretendentLen-1 DO
       oldRaSet := RaSet;
       new_u := NotEmitted[u_pretendent[uu]];
       RaSet := RaSet + ss.SEGM.code[new_u].attrs.res.r;
       ocir.BVClear(no_analized);
       ocir.BVIncl(no_analized,new_u);
       IF ss.SEGM.code[new_u].attrs.pair IN def.PairTags{def.NP,def.PV} THEN
           IF BetterPair(new_u,def.UNDEF_POS,old_u,old_v) THEN
               old_u := new_u;
               old_v := def.UNDEF_POS;
               u_op_coding := u_pretendent[uu];
           END;
       ELSE
           FOR vv := 0 TO v_pretendentLen-1 DO
               new_v := NotEmitted[v_pretendent[vv]];
               ocir.BVIncl(no_analized,new_v);
               IF (new_v <> new_u) AND
                  ((ss.SEGM.code[new_v].attrs.arg.ra * RaSet) = def.Regs{}) AND
                  BetterPair(new_u,new_v,old_u,old_v)
               THEN
                   old_u := new_u;
                   u_op_coding := u_pretendent[uu];
                   old_v := new_v;
                   v_op_coding := v_pretendent[vv];
               END;
           END;
           (* find exception *)
           FOR vv := 0 TO NotEmittedLen-1 DO
               new_v := NotEmitted[vv];
               IF (ss.SEGM.code[new_v].attrs.pair IN def.PairTags{def.PV,def.UV}) AND
                  (NOT ocir.BVIn(no_analized,new_v)) AND
                  (ss.SpecialPairException(new_u,new_v) OR
                   ss.AntiDepsException(new_u,new_v) OR
                   ss.EFLAGS_Exception(new_u,new_v)) AND
                   BetterPair(new_u,new_v,old_u,old_v) THEN
                   old_u := new_u;
                   u_op_coding := u_pretendent[uu];
                   old_v := new_v;
                   v_op_coding := vv;
               END;
           END;
       END;
       RaSet := oldRaSet;
   END;
   u_op := old_u; v_op := old_v;
   IF u_op <> def.UNDEF_POS THEN
       IF v_op <> def.UNDEF_POS THEN EmitUV();
       ELSE Emit(u_op_coding,TRUE);
       END;
   END;
END ExtractBestUV;

PROCEDURE ExtractBestV();
VAR old_v,new_v : INTEGER;
    vv : INTEGER;
    oldRaSet : def.Regs;
BEGIN
   (* current initialization of criterion *)
   IF g.CHAIN_PRC > g.AGI_PRC THEN
       VC2  := VChain;  VC3  := VAgi;
   ELSE
       VC2  := VAgi;    VC3  := VChain;
   END;

   v_op_coding := v_pretendent[0];
   old_v := NotEmitted[v_op_coding];
   FOR vv := 1 TO v_pretendentLen-1 DO
       oldRaSet := RaSet;
       new_v := NotEmitted[v_pretendent[vv]];
       RaSet := RaSet + ss.SEGM.code[new_v].attrs.res.r;
       IF BetterV(new_v,old_v) THEN
           old_v := new_v;
           v_op_coding := v_pretendent[vv];
       END;
       RaSet := oldRaSet;
   END;
   v_op := old_v;
   Emit(v_op_coding,TRUE);
END ExtractBestV;

CONST WasFloat = 0; -- на предыдущем шаге была выбрана только вещественная
                    -- команда;
      WasInt   = 1; -- на предыдущем шаге была выбрана (или две) целая команда

VAR PrevPassTag : LONGINT;

PROCEDURE DispatchCycle();
   VAR clocks_step : SHORTINT;
       code        : def.OpCode;
       fxch_coding,
       fxch_op     : INTEGER;
       what_choose : def.PairTag;
BEGIN
   RaSet := def.Regs{};
   IF u_op <> def.UNDEF_POS THEN
       RaSet := ss.SEGM.code[u_op].attrs.res.r;
   END;
   IF v_op <> def.UNDEF_POS THEN
       RaSet := RaSet + ss.SEGM.code[v_op].attrs.res.r;
   END;

   next_cycle := MAX(LONGINT);
   IF NOT ExtractReady() THEN
       PrevPassTag := WasInt;
       INC(current_cycle);
       RETURN;
   END;
   (* не будем выбирать вещественную операцию, если только что выбрали,
      и не выбирали целых к ней в придачу, а сейчас целые ждущие есть
   *)
   IF (PrevPassTag = WasFloat) &
      (u_pretendentLen > 0) THEN
      (* не будем выбирать вещественную операцию *)
       PrevPassTag := WasInt;
       what_choose := def.UV;
   ELSIF f_pretendentLen > 0 THEN
       GetFInstr();
       DecUseF(f_op);
       code := ss.SEGM.code^[f_op].code;
       (**)
       IF (ss.SEGM.code[f_op].attrs.pair = def.FP) & FXCH_Exists(fxch_coding)
       THEN
           fxch_op := NotEmittedF[fxch_coding];
           EmitF(fxch_coding);
           DecUse(fxch_op);
           EvalCycles(fxch_op,current_cycle+1);
           what_choose := def.UV;
       ELSIF ss.SEGM.code[f_op].attrs.pair = def.FP THEN
           what_choose := def.PV;
       ELSE
           what_choose := def.UV;
       END;
       PrevPassTag :=
           ORD((code<>def.FLD) & (code<>def.FST) & (code <> def.FSTP) &
               (code <> def.FLD1) & (code <> def.FLDZ));
   ELSE
       PrevPassTag := WasInt;
       what_choose := def.UV;
   END;
   CASE what_choose OF
   | def.UV :
       IF u_pretendentLen > 0 THEN
           PrevPassTag := WasInt;
           ExtractBestUV();
       END;
   | def.PV :
       IF v_pretendentLen > 0 THEN
           PrevPassTag := WasInt;
           ExtractBestV();
       END;
   END;
   IF u_op <> def.UNDEF_POS THEN
       DecUse(u_op);
       clocks_step := ss.SEGM.code[u_op].attrs.clocks;
       IF v_op <> def.UNDEF_POS THEN
           DecUse(v_op);
           IF clocks_step < ss.SEGM.code[v_op].attrs.clocks THEN
               clocks_step := ss.SEGM.code[v_op].attrs.clocks;
           END;
           EvalCyclesUV(current_cycle+clocks_step);
       ELSE
           EvalCycles(u_op,current_cycle+clocks_step);
       END;
   ELSIF v_op <> def.UNDEF_POS THEN
       DecUse(v_op);
       EvalCycles(v_op,current_cycle+ss.SEGM.code[v_op].attrs.clocks);
   END;
   IF (next_cycle <= current_cycle) OR (next_cycle = MAX(LONGINT)) THEN
       INC(current_cycle);
   ELSE
       current_cycle := next_cycle;
   END;
END DispatchCycle;

PROCEDURE Reordering();
BEGIN
   REPEAT DispatchCycle();  UNTIL new_len = ss.SEGM_LEN;
END Reordering;

(*
PROCEDURE ^ SetBefore();
PROCEDURE ^ SetAfter();
*)

PROCEDURE DoReOrder*(sg : ocir.Segment);
   VAR i : INTEGER;
BEGIN
   (*
   SetBefore;
   *)
   IF (sg.code_len <= 1) THEN
       (*
       SetAfter;
       *)
       RETURN
   END;
   ss.ReorderingPrologue(sg,TRUE,TRUE);

   (* initialization of local structures *)
   NotEmittedLen := 0;
   NotEmittedFLen := 0;
   uANDv_out := ocir.BVNew();
   FOR i:=0 TO ss.SEGM_LEN-1 DO
       IF def.GetOpGroup(ss.SEGM.code[i].code) = def.F_OGROUP THEN
           NotEmittedF[NotEmittedFLen] := i;
           INC(NotEmittedFLen);
       ELSE
           NotEmitted[NotEmittedLen] := i;
           INC(NotEmittedLen);
       END;
   END;
   u_pretendentLen := 0;
   v_pretendentLen := 0;
   f_pretendentLen := 0;
   PrevPassTag := WasInt;
   ocir.BVClear(no_analized);
   new_len := 0;
   current_cycle := 0;
   f_op := def.UNDEF_POS;
   u_op := def.UNDEF_POS;
   v_op := def.UNDEF_POS;
   (* main step *)
   Reordering();

   ss.ReorderingEpilogue();

   IF ss.segm_changed THEN sg.code := ss.res_segm.code;END;

   (*
   SetAfter;
   *)
END DoReOrder;

(*
VAR rrduv_total_time* : LONGINT;
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
    INC(rrduv_total_time,after-before);
    opIO.print("ellapsed by RrdUV %d\n",rrduv_total_time);
END SetAfter;
*)

BEGIN
   (*
   rrduv_total_time := 0;
   *)
   UC1  := UPairing;
   VC1  := VPairing;
   UVC1 := UVClocks;
   UVC2 := UVPairing;

   no_analized := ocir.BVNew();
END RrdUV.
