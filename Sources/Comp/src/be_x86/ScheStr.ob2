(* Created by KDV;
   module defines common structures and auxiliary procedures
   for all kinds of reordering;
*)
MODULE ScheStr;

IMPORT ir,
       ocir,
       def := OcirDef,
       Rrd2Bin,
       g := OcGraph;

IMPORT SYSTEM;
IMPORT D := desc386;
VAR    res_segm* : ocir.Segment;

VAR    SEGM_LEN*   : INTEGER;
       SEGM*       : ocir.Segment;

PROCEDURE SpecialPairException*(u, v : INTEGER) : BOOLEAN;
BEGIN
   RETURN
       ((SEGM.code[v].attrs.ready = 0) OR
        ((SEGM.code[v].attrs.ready = 1) AND ocir.BVIn(ocir.InOut[v].in,u)))
       AND
       (((def.IsPush_R_OR_I IN SEGM.code[u].attrs.tag_attrs) AND
         (def.IsPush_R_OR_I IN SEGM.code[v].attrs.tag_attrs))
         OR
         ((def.IsPush_R_OR_I IN SEGM.code[u].attrs.tag_attrs) AND
          (SEGM.code[v].code = def.CALL))
         OR
         ((def.IsPop_R IN SEGM.code[u].attrs.tag_attrs) AND
          (def.IsPop_R IN SEGM.code[v].attrs.tag_attrs))
         OR
         ((SEGM.code[u].code = def.CMP) AND
          (def.IsJcc IN SEGM.code[v].attrs.tag_attrs))
         OR
         ((SEGM.code[u].code = def.ADD) AND (SEGM.code[v].code = def.JNE)));
END SpecialPairException;

PROCEDURE AntiDepsException*(u, v : INTEGER) : BOOLEAN;
   VAR by_r, by_f, by_m : BOOLEAN;
BEGIN
   IF (((SEGM.code[v].attrs.ready = 1) AND ocir.BVIn(ocir.InOut[v].in,u))
       OR
       (SEGM.code[v].attrs.ready = 0))
      AND
      (NOT g.HasInfoDeps(SEGM,u,v,by_r,by_f,by_m))  AND
      (NOT g.HasOutputDeps(SEGM,u,v,by_r,by_f,by_m)) THEN
      IF g.HasAntiDeps(SEGM,u,v,by_r,by_f,by_m) AND (NOT by_f) AND (NOT by_m) THEN
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END;
   ELSE
       RETURN FALSE;
   END;
END AntiDepsException;


PROCEDURE EFLAGS_Exception*(u, v : INTEGER) : BOOLEAN;
   VAR by_r, by_f, by_m : BOOLEAN;
BEGIN
   (* 1) there are only output dependences between u and v
      2) v depends on u only
      3) dependences consists of EFLAGS only
   *)
   IF (((SEGM.code[v].attrs.ready = 1) AND ocir.BVIn(ocir.InOut[v].in,u))
       OR
       (SEGM.code[v].attrs.ready = 0))
      AND
      (NOT g.HasInfoDeps(SEGM,u,v,by_r,by_f,by_m)) AND
      (NOT g.HasAntiDeps(SEGM,u,v,by_r,by_f,by_m)) THEN
      IF g.HasOutputDeps(SEGM,u,v,by_r,by_f,by_m) AND (NOT by_r) AND (NOT by_m) THEN
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END;
   ELSE
       RETURN FALSE;
   END;
END EFLAGS_Exception;

PROCEDURE IsException*(u,v : INTEGER) : BOOLEAN;
BEGIN
    RETURN SpecialPairException(u,v) OR AntiDepsException(u,v) OR
           EFLAGS_Exception(u,v);
END IsException;

VAR segm_changed*      : BOOLEAN;
    CURRENT_PUSH_SIZE* : LONGINT;

------------------------------------------------------------------------------
(*
   кусок, предназначенный для выноса операций в следующие линейные
   участки,
*)

(* выкинуть pos-тую команду из линейного участка sg *)
PROCEDURE RemoveOpFromSeg(sg : ocir.Segment; pos : INTEGER);
VAR i : INTEGER;
BEGIN
    FOR i := pos TO sg.code_len-2 DO
        sg.code[i] := sg.code[i+1];
    END;
    DEC(sg.code_len);
END RemoveOpFromSeg;

VAR SegmForRemoved* : ocir.Segment;

TYPE
    RemoveOpPredicat* = PROCEDURE(sg  : ocir.Segment;
                                  pos : INTEGER;
                                  VAR continue : BOOLEAN) : BOOLEAN;

    RemoveOpAction*   = PROCEDURE(VAR op : def.Operation);

(* начиная с конца линейного участка sg, найти команду, удовлетворяющую
   условию predicat, выкинуть ее из sg, преобразовать с помощью
   action и добавить в конец SegmForRemoved (разместив, если надо, последний),
   возвращает признак, что нужно продолжить преобразование
*)
PROCEDURE RemoveOp(sg           : ocir.Segment;
                   predicat     : RemoveOpPredicat;
                   action       : RemoveOpAction);
VAR i  : INTEGER;
    op : def.Operation;
    continue : BOOLEAN;
BEGIN
    IF sg.code_len = 0 THEN RETURN END;
    FOR i := sg.code_len-1 TO 0 BY -1 DO
        IF predicat(sg,i,continue) THEN
            op := sg.code[i];
            action(op);
            RemoveOpFromSeg(sg,i);
            ocir.simple_add(SegmForRemoved,op);
            RETURN;
        ELSIF NOT continue THEN RETURN;
        END;
    END;
END RemoveOp;

VAR new_fstp_offset, new_recipe_fstp_offset : LONGINT;

PROCEDURE RemovableFSTP_FOpP(sg : ocir.Segment;
                             pos : INTEGER;
                             VAR continue : BOOLEAN) : BOOLEAN;
VAR op        : def.Operation;
    base,
    index     : def.Register;
    scale     : INTEGER;
    i         : INTEGER;
    bin       : def.BinRecipe;
    b_nesp,
    i_nesp    : BOOLEAN;
    with_mem  : BOOLEAN;

    PROCEDURE IsGoodForFSTP_FOpP(tested_o : def.Operation) : BOOLEAN;
    BEGIN
        (* следующий IF проверяет
           1) не  является ли tested_o вещественной операцией
           2) нет ли пересечения результата-памяти кандидата (fstp или fopp)
              с результатом/аргументом-памятью tested_o (если with_mem)
           3) tested_o - безопасная для перемещения кандидата команда
        *)
        IF (def.GetOpGroup(tested_o.code) = def.F_OGROUP) OR
           (with_mem &
            ((tested_o.attrs.arg.v.l <> ir.UNDEFINED) OR (tested_o.attrs.arg.v.vs <> NIL))
              & g.AreLocationsIntersect(op,tested_o,g.MWRITE,g.MREAD))  OR
           (with_mem &
            ((tested_o.attrs.res.v.l <> ir.UNDEFINED) OR (tested_o.attrs.res.v.vs <> NIL))
              & g.AreLocationsIntersect(op,tested_o,g.MWRITE,g.MWRITE))  OR
           g.IsNotMovable(tested_o) OR
           g.IsDang(tested_o)       OR
           (tested_o.code = def.CALL)
        THEN RETURN FALSE
        ELSIF NOT with_mem THEN RETURN TRUE
        ELSE
            (*
               теперь проверяется пересечение по регистрам - базе и индексу
               адреса из fstp и регистрами-результатами tested_o ---
               либо пересечения не должно быть, либо оно статическое,
               т.е. командами INC, DEC, ADD, SUB с константой или
               PUSH и POP
            *)
            IF (NOT (base IN tested_o.attrs.res.r)) &
               ((index = def.UNDEF_REGp) OR (NOT (index IN tested_o.attrs.res.r)))
            THEN
                RETURN TRUE;
            ELSE
                CASE tested_o.code OF
                | def._INC  :
                    IF VAL(SHORTINT,tested_o.dest.r) < 0 THEN
                        RETURN FALSE;
                    END;
                    IF b_nesp & (base = tested_o.dest.r)    THEN
                        DEC(new_fstp_offset);
                        DEC(new_recipe_fstp_offset);
                    END;
                    IF i_nesp & (index = tested_o.dest.r) THEN
                        DEC(new_fstp_offset,scale);
                        DEC(new_recipe_fstp_offset,scale);
                    END;
                | def._DEC  :
                    IF VAL(SHORTINT,tested_o.dest.r) < 0 THEN
                        RETURN FALSE;
                    END;
                    IF b_nesp & (base = tested_o.dest.r) THEN
                        INC(new_fstp_offset);
                        INC(new_recipe_fstp_offset);
                    END;
                    IF i_nesp & (index = tested_o.dest.r) THEN
                        INC(new_fstp_offset,scale);
                        INC(new_recipe_fstp_offset,scale);
                    END;
                | def.ADD   :
                    IF (def.GetOpOpndKind(tested_o.src.r) <> def.IMM) OR
                       (def.GetOpOpndKind(tested_o.dest.r) <> def.REG)
                    THEN RETURN FALSE;
                    END;
                    IF b_nesp & (tested_o.dest.r = base) THEN
                        DEC(new_fstp_offset,tested_o.bin.GetVal());
                        DEC(new_recipe_fstp_offset,tested_o.bin.GetVal());
                    END;
                    IF i_nesp & (tested_o.dest.r = index) THEN
                        DEC(new_fstp_offset,scale*tested_o.bin.GetVal());
                        DEC(new_recipe_fstp_offset,scale*tested_o.bin.GetVal());
                    END;
                | def.SUB   :
                    IF (def.GetOpOpndKind(tested_o.src.r) <> def.IMM) OR
                       (def.GetOpOpndKind(tested_o.dest.r) <> def.REG)
                    THEN RETURN FALSE;
                    END;
                    IF b_nesp & (tested_o.dest.r = base) THEN
                        INC(new_fstp_offset,tested_o.bin.GetVal());
                        INC(new_recipe_fstp_offset,tested_o.bin.GetVal());
                    END;
                    IF i_nesp & (tested_o.dest.r = index) THEN
                        INC(new_fstp_offset,scale*tested_o.bin.GetVal());
                        INC(new_recipe_fstp_offset,scale*tested_o.bin.GetVal());
                    END;
                | def.PUSH ,
                  def.POP    :
                ELSE
                    RETURN FALSE;
                END;
                RETURN TRUE;
            END;
        END;
    END IsGoodForFSTP_FOpP;

BEGIN
    scale := -1; base := def.UNDEF_REGp; index := def.UNDEF_REGp;
    i_nesp := FALSE; b_nesp := FALSE;
    op := sg.code[pos];
    CASE op.code OF
    | def.FSTP,
      def.FADDP,
      def.FSUBP, def.FSUBRP,
      def.FMULP,
      def.FDIVP, def.FDIVRP :
        continue := FALSE;
    ELSE
        continue := TRUE;
        RETURN FALSE;
    END;
    (* следующий IF проверяет
       1) не переносили ли уже эту команду
       2) разрешено ли в принципе таскать эту команду
       3) есть предыдущая команда
       4) предыдущая команда оправдывает перенос
          т.е. она вещественная и исполняется дольше, чем fstp
          (если текущая fstp)
       5) текушая д.б. в память или в STi
    *)
    IF (def.ALREADY_REMOVED IN op.attrs.tag_attrs) OR
       g.IsNotMovable(op) OR
       ((def.GetOpOpndKind(op.dest.r) <> def.MEM) &
        (NOT (op.dest.r IN def.Regs{def.ST0..def.ST7})))
    THEN RETURN FALSE;
    END;
    with_mem := (op.dest.r = SYSTEM.VAL(def.Register,def.MEM_OPND_CODE));
    IF with_mem THEN
        base  := VAL(def.Register,op.dest.a.place1.r); b_nesp :=  base <> D.ESPp;
        index := VAL(def.Register,op.dest.a.place2.r); i_nesp := index <> D.ESPp;
        CASE op.dest.a.scale OF
        | D.x1:   scale := 1;
        | D.x2:   scale := 2;
        | D.x4:   scale := 4;
        | D.x8:   scale := 8;
        ELSE
          ASSERT(FALSE);
        END;
        new_fstp_offset := op.dest.a.offs;
        bin := op.bin;
        WITH bin : Rrd2Bin.MoveM_TOS DO
            new_recipe_fstp_offset := bin.a.offs;
        END;
    END;
    FOR i := pos+1 TO sg.code_len-1 DO
        IF NOT IsGoodForFSTP_FOpP(sg.code^[i]) THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
END RemovableFSTP_FOpP;

PROCEDURE RecalcFSTP_FOpP(VAR fstp_fopp : def.Operation);
VAR bin : def.BinRecipe;
BEGIN
    INCL(fstp_fopp.attrs.tag_attrs, def.ALREADY_REMOVED);
    bin := fstp_fopp.bin;
    WITH bin : Rrd2Bin.MoveM_TOS DO
        bin.a.offs := new_recipe_fstp_offset;
        fstp_fopp.dest.a.offs := new_fstp_offset;
    ELSE
    END;
END RecalcFSTP_FOpP;

PROCEDURE DoRemove*(sg : ocir.Segment);
BEGIN
    SegmForRemoved := NIL;
    RemoveOp(sg,RemovableFSTP_FOpP,RecalcFSTP_FOpP);
END DoRemove;

PROCEDURE ReorderingPrologue*(sg : ocir.Segment;
                              forward,
                              init_res_sg : BOOLEAN);
BEGIN
   ocir.PrefixCorrect(sg);
   SEGM := sg;
   SEGM_LEN := SEGM.code_len;
   IF init_res_sg THEN
       ocir.copy_segm_by_len(res_segm,SEGM_LEN);
   END;
   g.BuildGraph(sg,forward);
   segm_changed := FALSE;
   CURRENT_PUSH_SIZE := SEGM.code[0].PUSH_SIZE;
END ReorderingPrologue;

PROCEDURE ReorderingEpilogue*();
BEGIN
   ocir.ClearAttrs(SEGM);
END ReorderingEpilogue;

END ScheStr.
