MODULE Optimize;

<* IF DEFINED(MeDebugMode) AND MeDebugMode THEN *>
  <* procinline- *>
  <* noregvars+  *>
<* ELSE *>
  <* procinline+ *>
<* END *>

(*
    В CSE нет:
    - load, store, более интеллектуальной обработки возможности слияния call
    - интеллекта при определении зависимости по чтению (для loadr)
    - нужны разные едининцы при разных длинах
   Нет оптимизации IF x > 5 THEN INC (a) => a := a + (x > 5) ? 0 : 1;
   Выключена оптимизация fi
   ABS: обрабатывать случай < 0!
   ReplaceCall: отключено для float
*)

IMPORT ir;
IMPORT gr := ControlGraph;
IMPORT Calc;
IMPORT BitVect;
IMPORT anz := Analysis;
IMPORT opTune;
IMPORT opAttrs;
IMPORT env := xiEnv;
IMPORT def := opDef;
IMPORT ope := opE;


<* IF TARGET_386 THEN *>
IMPORT opCard64;
<* END *>

IMPORT pc := pcK;

IMPORT SYSTEM;

(*----------------------------------------------------------------------------*)

TYPE
    INT         = ir.INT;
    TPOS        = ir.TPOS;
    VALUE       = ir.VALUE;
    Node        = ir.Node;
    Arc         = ir.Arc;
    TagType     = ir.TagType;
    TypeType    = ir.TypeType;
    SizeType    = ir.SizeType;
    TriadePtr   = ir.TriadePtr;
    TriadeArray = ir.TriadeArray;
    ParamPtr    = ir.ParamPtr;
    ParamArray  = ir.ParamArray;
    Local       = ir.Local;
    VarNum      = ir.VarNum;
    BitVector   = BitVect.BitVector;


(*----------------------------------------------------------------------------*)

VAR
    par:            ParamPtr;
    Changed:        BOOLEAN;
    ChangedGraph:   BOOLEAN;
    NextTriade*:    TriadePtr;
    Consts*:        TriadeArray;
    NConsts:        INT;
    index1, index2: INT;
    vec
   :    BitVector;

VAR MachineLevelOptimizations*: BOOLEAN;


VAR opt_nooptimize_fork, -- запрещает расклейку триады по входным дугам
    opt_nooptimize_add_uk: BOOLEAN; -- запрещает комбинацию сложений
(*----------------------------------------------------------------------------*)


<* IF TARGET_386 AND fast_bitvect_in THEN *>
<* PUSH *>
--<* +procinline *>
<* NEW no_set_range_check+ *>
<*-CHECKNIL    *>
<*-CHECKDIV    *>
<*-CHECKPROC   *>
<*-CHECKSET    *>
<*-CHECKINDEX  *>
<*-CHECKDINDEX  *>
<*-CHECKRANGE  *>
<*-CHECKNIL    *>

PROCEDURE BitVect_In (p: BitVect.BitVector; e: INT): BOOLEAN;
<* NEW override_max_set_len+ *>
VAR foo:BitVect.LongBitVector;
BEGIN
    foo := SYSTEM.VAL(BitVect.LongBitVector,p);
    RETURN e IN foo^.v;
END BitVect_In;


PROCEDURE BitVect_Incl (p: BitVect.BitVector; e: INT);
BEGIN
    INCL (p^.v [e DIV BitVect.BitsPerSet], e MOD BitVect.BitsPerSet);
END BitVect_Incl;

PROCEDURE BitVect_Excl (p: BitVect.BitVector; e: INT);
BEGIN
    EXCL (p^.v [e DIV BitVect.BitsPerSet], e MOD BitVect.BitsPerSet);
END BitVect_Excl;
<* POP *>

<* ELSE *>

PROCEDURE BitVect_In (p: BitVect.BitVector; e: INT): BOOLEAN;
BEGIN
    RETURN BitVect.In(p, e);
END BitVect_In;


PROCEDURE BitVect_Incl (p: BitVect.BitVector; e: INT);
BEGIN
    BitVect.Incl(p, e);
END BitVect_Incl;

PROCEDURE BitVect_Excl (p: BitVect.BitVector; e: INT);
BEGIN
    BitVect.Excl(p, e);
END BitVect_Excl;

<* END *>

(*
  А не 0 ли у нас?
*)

PROCEDURE IsZero (p: ParamPtr; t: TypeType; s: SizeType): BOOLEAN;
BEGIN
    RETURN ((p^.tag = ir.y_NumConst)  OR
            (p^.tag = ir.y_RealConst) OR
            (p^.tag = ir.y_ComplexConst)) &
            Calc.IsZero (p^.value, t, s);
END IsZero;

(*----------------------------------------------------------------------------*)

(*
  Можно ли оптимизировать эту конкретную триаду?
*)

PROCEDURE MustPreserve (p: TriadePtr): BOOLEAN;
BEGIN
    RETURN ir.o_NoOptimize IN p.Options;
END MustPreserve;

(*----------------------------------------------------------------------------*)

(*
  Подставить параметр вместо двух параметров в триаду
*)

PROCEDURE Replace2Pars* (p: TriadePtr; q: ParamPtr; k1, k2: INT): TriadePtr;
VAR    s: TriadePtr;
       r: ParamArray;
    i, j: INT;
       b: BOOLEAN;
BEGIN
    IF (LEN (p^.Params^) = 2) & NOT q^.reverse THEN
        s := p^.Next;
        gr.ReplaceByParam (p, q);
        RETURN s;
    END;
    r := ir.NewParams (LEN (p^.Params^) - 1, p);
    j := 0;
    b := TRUE;
    FOR i:=0 TO LEN(p^.Params^)-1 DO
        IF (i = k1) OR (i = k2) THEN
            IF b THEN
                ir.CopyParamWithRev (q, r^[j]);
                INC (j);
                b := FALSE;
            END;
        ELSE
            ir.CopyParamWithRev (p^.Params^[i], r^[j]);
            INC (j);
        END;
        IF p^.Params^[i].tag = ir.y_Variable THEN
            ir.RemoveUse (p^.Params^[i]);
        END;
    END;
    p^.Params := r;
    RETURN p;
END Replace2Pars;

(*----------------------------------------------------------------------------*)
(*
  Заменить последнюю триаду в узле на переход
*)

PROCEDURE ReplaceByGoto (n: Node; i: INT; ch_gr: BOOLEAN);
VAR p: TriadePtr;
    j: INT;
    s: TPOS;
    a: Arc;
BEGIN
    p := ir.Nodes^[n].Last;
    IF ir.o_Silent IN p^.Options THEN
        s := env.null_pos;
    ELSE
        s := p^.Position;
    END;
    gr.KillTriade(p);
    gr.MakeGoto (n);
    FOR j:=ir.Nodes^[n].NOut-1 TO 0 BY -1 DO
        IF i <> j THEN
            a := ir.Nodes^[n].OutArcs^[j];
            IF gr.Arcs^[a].Original & NOT s.IsNull () & ~ir.insideInline THEN
                env.errors.Warning (s, 902);
            END;
            gr.KillArc_Ex (a, ch_gr);
        END;
    END;
    ChangedGraph := ch_gr;
END ReplaceByGoto;

(*----------------------------------------------------------------------------*)

(*
  Сформировать триаду ERROR
*)

PROCEDURE MakeError (p: TriadePtr; e: INT);
VAR q: TriadePtr;
    n: Node;
    i: INT;
    sarg:ir.Arg;
BEGIN
    IF NOT p^.Position.IsNull () THEN
        CASE e OF
        | opTune.realValueException:    env.errors.Warning (p^.Position, 910);
        | opTune.realDivException:      env.errors.Warning (p^.Position, 916);
        | opTune.wholeValueException:   env.errors.Warning (p^.Position, 911);
        | opTune.wholeDivException:     env.errors.Warning (p^.Position, 912);
        | opTune.indexException:        env.errors.Warning (p^.Position, 913);
        | opTune.rangeException:        env.errors.Warning (p^.Position, 914);
        | opTune.invalidLocation:       IF (p^.Op = ir.o_checknil) &
                                           (p^.Params^[0].tag <> ir.y_Nothing)
                                        THEN
                                            env.errors.Warning (p^.Position,
                                                                315);
                                        ELSE
                                            env.errors.Warning(p^.Position,915);
                                        END;
        | ELSE
        END;
    END;


    q := ir.NewTriadeTInit(3, ir.o_error, ir.y_Nothing, opTune.index_ty, opTune.index_sz);
    q^.Position          := p^.Position;
    ir.MakeParNum( q.Params[0], Calc.GetInteger (e, opTune.index_sz) );
    ope.gen_modName(p^.Position, q.Params[1], q.Params[2]);

    gr.InsertTriade (q, p);
    n := p^.NodeNo;
    ir.MakeParNothing (par);
    REPEAT
        IF p^.Tag = ir.y_Variable THEN
            gr.ReplaceByParam (p, par);
            p := q^.Next;
        ELSE
            p := gr.KillTriade_Ex (p);
        END;
    UNTIL p = NIL;
    FOR i:=ir.Nodes^[n].NOut-1 TO 0 BY -1 DO
        gr.KillArc (ir.Nodes^[n].OutArcs^[i]);
    END;
    ChangedGraph := TRUE;
END MakeError;

(*----------------------------------------------------------------------------*)

(*
  Сформировать триаду "переполнение"
*)

PROCEDURE MakeOverflow (p: TriadePtr);
BEGIN
    IF p^.ResType = ir.t_float THEN
        MakeError (p, opTune.realValueException);
    ELSE
        MakeError (p, opTune.wholeValueException);
    END;
END MakeOverflow;

(*----------------------------------------------------------------------------*)

(*
  Сформировать триаду "ошибка, связанная с проверкой"
*)

PROCEDURE MakeCheckError (p: TriadePtr);
BEGIN
    IF ir.o_Division IN p^.Options THEN
        MakeError (p, opTune.wholeDivException);
    ELSIF ir.o_Index IN p^.Options THEN
        MakeError (p, opTune.indexException);
    ELSE
        MakeError (p, opTune.rangeException);
    END;
END MakeCheckError;

(*----------------------------------------------------------------------------*)

(*
  Пересекаются ли 2 обращения в память ?
*)

PROCEDURE RWDependence* (p, q: TriadePtr; pbv, qbv: BitVector) : BOOLEAN;

    (*
       if r1 then v1 := -v1; end;
       if r2 then v2 := -v2; end;
       return  [v1, v1+len] intersects with [v2, v2+len] ? TRUE : FALSE;
    *)
    PROCEDURE CanEqIntervals (v1: VALUE; r1: BOOLEAN;
                              v2: VALUE; r2: BOOLEAN; len: LONGINT): BOOLEAN;
    VAR i1, i2: LONGINT;
    BEGIN
        i1 := Calc.ToInteger (v1, opTune.addr_sz);
        IF r1 THEN
            IF i1 = MAX (LONGINT) THEN
                RETURN TRUE;
            END;
            i1 := -i1;
        END;
        i2 := Calc.ToInteger (v2, opTune.addr_sz);
        IF r2 THEN
            IF i2 = MAX (LONGINT) THEN
                RETURN TRUE;
            END;
            i2 := -i2;
        END;
        RETURN NOT ((i1 <= MAX (LONGINT) - len) & (i1 + len <= i2) OR
                    (i2 <= MAX (LONGINT) - len) & (i2 + len <= i1));
    END CanEqIntervals;

    (*
      Попытаться по 2 переменным выяснить, могут
      ли они указывать в одно и то же место
    *)

    PROCEDURE CanEqVars (v1, v2: VarNum; len: INT): BOOLEAN;
    VAR p1, p2: TriadePtr;
    BEGIN
    (*
      Прежде всего: v1 = v2 + int?
    *)
        p1 := ir.Vars^[v1].Def;
        IF (p1^.Op = ir.o_add) & (LEN (p1^.Params^) = 2) &
           (
            (p1^.Params^[0].tag = ir.y_Variable) & (p1^.Params^[0].name = v2) &
            (p1^.Params^[1].tag = ir.y_NumConst) &
            NOT CanEqIntervals (anz.Zeroes[ir.t_int][p1.OpSize].value, FALSE,
                                p1^.Params^[1].value, p1^.Params^[1].reverse,
                                len)
           OR
            (p1^.Params^[1].tag = ir.y_Variable) & (p1^.Params^[1].name = v2) &
            (p1^.Params^[0].tag = ir.y_NumConst) &
            NOT CanEqIntervals (anz.Zeroes[ir.t_int][p1.OpSize].value, FALSE,
                                p1^.Params^[0].value, p1^.Params^[0].reverse,
                                len)
           )
        THEN
            RETURN FALSE;
        END;
    (*
      Может быть, v2 = v1 + int?
    *)
        p2 := ir.Vars^[v2].Def;
        IF (p2^.Op = ir.o_add) & (LEN (p2^.Params^) = 2) &
           (
            (p2^.Params^[0].tag = ir.y_Variable) & (p2^.Params^[0].name = v1) &
            (p2^.Params^[1].tag = ir.y_NumConst) &
            NOT CanEqIntervals (anz.Zeroes[ir.t_int][p2.OpSize].value, FALSE,
                                p2^.Params^[1].value, p2^.Params^[1].reverse,
                                len)
           OR
            (p2^.Params^[1].tag = ir.y_Variable) & (p2^.Params^[1].name = v1) &
            (p2^.Params^[0].tag = ir.y_NumConst) &
            NOT CanEqIntervals (anz.Zeroes[ir.t_int][p2.OpSize].value, FALSE,
                                p2^.Params^[0].value, p2^.Params^[0].reverse,
                                len)
           )
        THEN
            RETURN FALSE;
        END;
    (*
      Ну а может быть, v1 = v + int и v2 = v + int?
    *)
        IF (p1^.Op = ir.o_add) & (LEN (p1^.Params^) = 2) &
           (p2^.Op = ir.o_add) & (LEN (p2^.Params^) = 2)
        THEN
           IF ir.EqParams (p1^.Params^[0], p2^.Params^[0],
                           opTune.addr_ty, opTune.addr_sz) &
              (p1^.Params^[1].tag = ir.y_NumConst) &
              (p2^.Params^[1].tag = ir.y_NumConst) &
              NOT CanEqIntervals (p1^.Params^[1].value, p1^.Params^[1].reverse,
                                  p2^.Params^[1].value, p2^.Params^[1].reverse,
                                  len)
           OR ir.EqParams (p1^.Params^[0], p2^.Params^[1],
                           opTune.addr_ty, opTune.addr_sz) &
              (p1^.Params^[1].tag = ir.y_NumConst) &
              (p2^.Params^[0].tag = ir.y_NumConst) &
              NOT CanEqIntervals (p1^.Params^[1].value, p1^.Params^[1].reverse,
                                  p2^.Params^[0].value, p2^.Params^[0].reverse,
                                  len)
           OR ir.EqParams (p1^.Params^[1], p2^.Params^[0],
                           opTune.addr_ty, opTune.addr_sz) &
              (p1^.Params^[0].tag = ir.y_NumConst) &
              (p2^.Params^[1].tag = ir.y_NumConst) &
              NOT CanEqIntervals (p1^.Params^[0].value, p1^.Params^[0].reverse,
                                  p2^.Params^[1].value, p2^.Params^[1].reverse,
                                  len)
           OR ir.EqParams (p1^.Params^[1], p2^.Params^[1],
                           opTune.addr_ty, opTune.addr_sz) &
              (p1^.Params^[0].tag = ir.y_NumConst) &
              (p2^.Params^[0].tag = ir.y_NumConst) &
              NOT CanEqIntervals (p1^.Params^[0].value, p1^.Params^[0].reverse,
                                  p2^.Params^[0].value, p2^.Params^[0].reverse,
                                  len)
            THEN
                RETURN FALSE;
            END;
        END;
    (*
      Увы...
    *)
        RETURN TRUE;
    END CanEqVars;

(*
  Если NOPTRALIAS, то обращения в память (load(r)/store(r) с разными
                   размерами/типами не могут пересекаться
*)
    PROCEDURE CheckIfPragma (p, q: TriadePtr): BOOLEAN;
    BEGIN
        RETURN
               (opAttrs.NOALIAS IN opAttrs.COMP_MODE) &
               (NOT (p.Op IN ir.OpSet{ ir.o_copy, ir.o_clear, ir.o_call, ir.o_cmpswap })) AND
               (NOT (q.Op IN ir.OpSet{ ir.o_copy, ir.o_clear, ir.o_call, ir.o_cmpswap })) AND
               ((p^.ResSize <> q^.ResSize) OR
                (p^.ResType <> q^.ResType) &
                NOT ((p^.ResType IN ir.TypeTypeSet{ ir.t_int, ir.t_unsign }) &
                     (q^.ResType IN ir.TypeTypeSet{ ir.t_int, ir.t_unsign })));
    END CheckIfPragma;

VAR l1, l2: Local;
    o1, o2: LONGINT;
    s1, s2: LONGINT;
    len: LONGINT;
BEGIN
(*
  Могут ли они вообще указывать в одно место ?
*)

    IF (pbv = NIL) OR (qbv = NIL) OR NOT BitVect.Intersecting (pbv, qbv) THEN
        RETURN FALSE;
    END;

    IF ( (p^.Op IN ir.OpSet{ ir.o_loadr, ir.o_cmpswap }) &
         (q^.Op IN ir.OpSet{ ir.o_storer, ir.o_cmpswap })
       OR
         (q^.Op IN ir.OpSet{ ir.o_loadr, ir.o_cmpswap }) &
         (p^.Op IN ir.OpSet{ ir.o_storer, ir.o_cmpswap})
       )
    THEN
      IF
         ir.EqParams (p^.Params^[0], q^.Params^[0],
                      opTune.addr_ty, opTune.addr_sz)
      THEN
        RETURN TRUE;
      END;
    END;
(*
  Проверить на специальные случаи (v и v + 4, v + 4 и v + 8)
*)
    len := p^.ResSize;
    IF ORD(q^.ResSize) > len THEN
        len := q^.ResSize;
    END;
    IF ((p^.Op = ir.o_loadr) OR (p^.Op = ir.o_storer)) &
       ((q^.Op = ir.o_loadr) OR (q^.Op = ir.o_storer)) &
       (p^.Params^[0].tag = ir.y_Variable) &
       (q^.Params^[0].tag = ir.y_Variable) &
       NOT CanEqVars (p^.Params^[0].name, q^.Params^[0].name, len)
    THEN
        RETURN FALSE;
    END;
(*
  Проверить вариант: оба адреса - адресные константы, диапазоны не перекрываются
*)
    CASE p^.Op OF
    | ir.o_load:
        l1 := p^.Params^[0].name;
        o1 := 0;
        s1 := p^.ResSize;
    | ir.o_loadr,
      ir.o_storer,
      ir.o_cmpswap:
        IF p^.Params^[0].tag <> ir.y_AddrConst THEN
            RETURN NOT CheckIfPragma (p, q);
        END;
        l1 := p^.Params^[0].name;
        o1 := p^.Params^[0].offset;
        s1 := p^.ResSize;
    | ir.o_store:
        l1 := p^.Name;
        o1 := 0;
        s1 := p^.ResSize;
    | ir.o_clear:
        IF (p^.Params^[0].tag <> ir.y_AddrConst) OR
           (p^.Params^[1].tag <> ir.y_NumConst)
        THEN
          RETURN TRUE;
        END;
        l1 := p^.Params^[0].name;
        o1 := p^.Params^[0].offset;
        s1 := Calc.ToInteger (p^.Params^[1].value, opTune.index_sz);
    | ir.o_copy:
        IF pbv = p^.Read THEN
            IF (p^.Params^[0].tag <> ir.y_AddrConst) OR
               (p^.Params^[2].tag <> ir.y_NumConst)
            THEN
                RETURN TRUE;
            END;
            l1 := p^.Params^[0].name;
            o1 := p^.Params^[0].offset;
            s1 := Calc.ToInteger (p^.Params^[2].value, opTune.index_sz);
        ELSE
            IF (p^.Params^[1].tag <> ir.y_AddrConst) OR
               (p^.Params^[2].tag <> ir.y_NumConst)
            THEN
                RETURN TRUE;
            END;
            l1 := p^.Params^[1].name;
            o1 := p^.Params^[1].offset;
            s1 := Calc.ToInteger (p^.Params^[2].value, opTune.index_sz);
        END;
    | ELSE
        RETURN TRUE;
    END;
    CASE q^.Op OF
    | ir.o_load:
        l2 := q^.Params^[0].name;
        o2 := 0;
        s2 := q^.ResSize;
    | ir.o_loadr,
      ir.o_storer,
      ir.o_cmpswap:
        IF q^.Params^[0].tag <> ir.y_AddrConst THEN
            RETURN NOT CheckIfPragma (p, q);
        END;
        l2 := q^.Params^[0].name;
        o2 := q^.Params^[0].offset;
        s2 := q^.ResSize;
    | ir.o_store:
        l2 := q^.Name;
        o2 := 0;
        s2 := q^.ResSize;
   | ir.o_clear:
        IF (q^.Params^[0].tag <> ir.y_AddrConst) OR
           (q^.Params^[1].tag <> ir.y_NumConst)
        THEN
          RETURN TRUE;
        END;
        l2 := q^.Params^[0].name;
        o2 := q^.Params^[0].offset;
        s2 := Calc.ToInteger (q^.Params^[1].value, opTune.index_sz);

    | ir.o_copy:
        IF qbv = q^.Read THEN
            IF (q^.Params^[0].tag <> ir.y_AddrConst) OR
               (q^.Params^[2].tag <> ir.y_NumConst)
            THEN
                RETURN TRUE;
            END;
            l2 := q^.Params^[0].name;
            o2 := q^.Params^[0].offset;
            s2 := Calc.ToInteger (q^.Params^[2].value, opTune.index_sz);
        ELSE
            IF (q^.Params^[1].tag <> ir.y_AddrConst) OR
               (q^.Params^[2].tag <> ir.y_NumConst)
            THEN
                RETURN TRUE;
            END;
            l2 := q^.Params^[1].name;
            o2 := q^.Params^[1].offset;
            s2 := Calc.ToInteger (q^.Params^[2].value, opTune.index_sz);
        END;
    | ELSE
        RETURN TRUE;
    END;
    IF (l1 <> l2) OR (o1 + s1 <= o2) OR (o2 + s2 <= o1) THEN
        RETURN FALSE;
    END;
(*
  Увы, могут...
*)
    RETURN TRUE;
END RWDependence;

(*----------------------------------------------------------------------------*)

(*
  Дано: p - триада, доминирует над q (и над s тоже); nq - узел q;
        bv = s^.Read или s^.Write
  Начиная с q, идти назад до p, проверяя, пишет ли кто-нибудь в ту
      память, откуда читает (или куда пишет) s
*)

PROCEDURE CheckLoadStep (p, q, s: TriadePtr; nq: Node; bv: BitVector): BOOLEAN;
VAR  i: INT;
     m: Node;
BEGIN
(*
  Сначала проверить все триады в этом узле
*)
    WHILE q <> NIL DO
        IF q = p THEN
            RETURN TRUE;
        END;
        IF (q^.Write <> NIL) & RWDependence (s, q, bv, q^.Write) THEN
            RETURN FALSE;
        END;
        q := q^.Prev;
    END;
(*
  Дошли до начала узла, не встретив p; надо проверять предшественников
*)
    FOR i := 0 TO ir.Nodes^[nq].NIn-1 DO
        m := ir.Nodes^[nq].In^[i];
        IF NOT BitVect_In (vec, ORD(m)) THEN
            BitVect_Incl (vec, ORD(m));
            IF NOT CheckLoadStep (p, ir.Nodes^[m].Last, s, m, bv) THEN
                RETURN FALSE;
            END;
        END;
    END;
    RETURN TRUE;
END CheckLoadStep;

(*----------------------------------------------------------------------------*)

(*
  Дано: p доминирует над q (или находится с ним в том же узле),
        q - работает с памятью, bv = q^.Read или q^.Write
  Проверить: пишет ли кто-нибудь туда, откуда читает (или куда пишет) q?
*)

PROCEDURE NobodyWrites (p, q: TriadePtr; bv: BitVector): BOOLEAN;
VAR r: TriadePtr;
BEGIN
    IF ir.o_Constant IN q^.Options THEN
        RETURN TRUE;
    END;
    r := q^.Prev;
    IF p^.NodeNo = q^.NodeNo THEN
        LOOP
            IF r = NIL THEN
                RETURN FALSE;
            ELSIF r = p THEN
                EXIT;
            END;
            IF (r^.Write <> NIL) & RWDependence (q, r, bv, r^.Write) THEN
                RETURN FALSE;
            END;
            r := r^.Prev;
        END;
    ELSE
        BitVect.Fill (vec, FALSE, ORD(ir.Nnodes));
        IF NOT CheckLoadStep (p, r, q, q^.NodeNo, bv) THEN
            RETURN FALSE;
        END;
    END;
    RETURN TRUE;
END NobodyWrites;

(*----------------------------------------------------------------------------*)

(*
  Дано: q - работает с памятью, bv = q^.Read или q^.Write
  Проверить: читает ли кто-нибудь оттуда же? - Один шаг проверки
*)

PROCEDURE CheckStoreStep (n: Node; q: TriadePtr; bv: BitVector): BOOLEAN;
VAR r: TriadePtr;
    i: INT;
    m: Node;
BEGIN
    r := ir.Nodes^[n].First;
    REPEAT
        IF r = q THEN
            RETURN TRUE;
        ELSIF (r^.Read <> NIL) & (ir.isRead IN ir.OpProperties[r^.Op]) &
              RWDependence (q, r, bv, r^.Read)
        THEN
            RETURN FALSE;
        END;
        r := r^.Next;
    UNTIL r = NIL;
    FOR i:=0 TO ir.Nodes^[n].NOut-1 DO
        m := ir.Nodes^[n].Out^[i];
        IF NOT BitVect_In (vec, ORD(m)) THEN
            BitVect_Incl (vec, ORD(m));
            IF NOT CheckStoreStep (m, q, bv) THEN
                RETURN FALSE;
            END;
        END;
    END;
    RETURN TRUE;
END CheckStoreStep;

(*----------------------------------------------------------------------------*)

(*
  Дано: q - работает с памятью, bv = q^.Read или q^.Write
  Проверить: читает ли кто-нибудь оттуда же?
*)

PROCEDURE NobodyReads (q: TriadePtr; bv: BitVector): BOOLEAN;
VAR r: TriadePtr;
    l: Local;
    i: INT;
    m: Node;

    PROCEDURE CheckSimple (n: Node; q: TriadePtr): BOOLEAN;
    VAR r: TriadePtr;
        i: INT;
        m: Node;
    BEGIN
        r := ir.Nodes^[n].First;
        REPEAT
            IF (r^.Op = ir.o_storer) &
               ir.EqParams (r^.Params^[0], q^.Params^[0],
                            opTune.addr_ty, opTune.addr_sz) &
               (r^.ResSize >= q^.ResSize)
            THEN
                RETURN TRUE;
            ELSIF (r^.Read <> NIL) OR (r^.Write <> NIL) OR
                  (ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * r^.Options <> ir.OptionsSet{})
            THEN
                RETURN FALSE;
            END;
            r := r^.Next;
        UNTIL r = NIL;
        FOR i:=0 TO ir.Nodes^[n].NOut-1 DO
            m := ir.Nodes^[n].Out^[i];
            IF NOT BitVect_In (vec, ORD(m)) THEN
                BitVect_Incl (vec, ORD(m));
                IF NOT CheckSimple (m, q) THEN
                    RETURN FALSE;
                END;
            END;
        END;
        RETURN TRUE;
    END CheckSimple;

    PROCEDURE Simple (q: TriadePtr): BOOLEAN;
    VAR p: TriadePtr;
        i: INT;
    BEGIN
        IF q^.Op <> ir.o_storer THEN
            RETURN FALSE;
        END;
        p := q^.Next;
        WHILE p <> NIL DO
            IF (p^.Op = ir.o_storer) &
               ir.EqParams (p^.Params^[0], q^.Params^[0],
                            opTune.addr_ty, opTune.addr_sz) &
               (p^.ResSize >= q^.ResSize)
            THEN
                RETURN TRUE;
            ELSIF (p^.Read <> NIL) OR (p^.Write <> NIL) OR
                  (ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options <> ir.OptionsSet{})
            THEN
                RETURN FALSE;
            END;
            p := p^.Next;
        END;
        BitVect.Fill (vec, FALSE, ORD(ir.Nnodes));
        FOR i:=0 TO ir.Nodes^[q^.NodeNo].NOut-1 DO
            m := ir.Nodes^[q^.NodeNo].Out^[i];
            IF NOT BitVect_In (vec, ORD(m)) THEN
                BitVect_Incl (vec, ORD(m));
                IF NOT CheckSimple (m, q) THEN
                    RETURN FALSE;
                END;
            END;
        END;
        RETURN TRUE;
    END Simple;

BEGIN
    IF
      BitVect_In(bv, ir.NLocals)
    THEN
        RETURN Simple (q);
    END;
    FOR l:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NLocals) DO
        IF
----
        (*ir_IsExternal (l)*) --inlined from ir
        (ir.Locals^[l].Scope >= ir.ZEROScope)
----
          & BitVect_In (bv, ORD(l)) THEN
            RETURN Simple (q);
        END;
    END;
    r := q;
    LOOP
        r := r^.Next;
        IF r = NIL THEN
            EXIT;
        ELSIF (r^.Read <> NIL) & (ir.isRead IN ir.OpProperties[r^.Op]) &
              RWDependence (q, r, bv, r^.Read)
        THEN
            RETURN Simple (q);
        END;
    END;
    BitVect.Fill (vec, FALSE, ORD(ir.Nnodes));
    FOR i:=0 TO ir.Nodes^[q^.NodeNo].NOut-1 DO
        m := ir.Nodes^[q^.NodeNo].Out^[i];
        IF NOT BitVect_In (vec, ORD(m)) THEN
            BitVect_Incl (vec, ORD(m));
            IF NOT CheckStoreStep (m, q, bv) THEN
                RETURN FALSE;
            END;
        END;
    END;
    RETURN TRUE;
END NobodyReads;

(*----------------------------------------------------------------------------*)

(*
  Совпадают ли адреса у load и loadr или storer?
*)

PROCEDURE EqLoadParams (p, q: ParamPtr): BOOLEAN;
BEGIN
    RETURN ir.EqParams (p, q, ir.t_ref, 4)
           OR
             (p^.tag  = ir.y_RealVar) & (q^.tag = ir.y_AddrConst) &
             (p^.name = q^.name)      & (q^.offset = 0)
           OR
             (q^.tag  = ir.y_RealVar) & (p^.tag = ir.y_AddrConst) &
             (q^.name = p^.name)      & (p^.offset = 0);
END EqLoadParams;

(*----------------------------------------------------------------------------*)

(*
  Пройти из узла m в узел n, проверяя при этом, что:
  - во все промежуточные узлы ровно один вход
  - по дороге может встретиться ровно одно сложение и ничего более
*)

PROCEDURE CheckPath (m, n: ir.Node; VAR p: TriadePtr; VAR a: Arc): BOOLEAN;
VAR q: TriadePtr;
BEGIN
    p := NIL;
    WHILE m <> n DO
        IF ir.Nodes^[m].NIn > 1 THEN
            RETURN FALSE;
        END;
        q := ir.Nodes^[m].First;
        IF (q^.Op = ir.o_add) & (p = NIL) THEN
            p := q;
            q := q^.Next;
        END;
        IF q^.Op <> ir.o_goto THEN
            RETURN FALSE;
        END;
        a := ir.Nodes^[m].OutArcs^[0];
        m := ir.Nodes^[m].Out^[0];
    END;
    ASSERT(p = NIL);
    RETURN TRUE;
END CheckPath;

(*----------------------------------------------------------------------------*)

(*
  Попытаться 1) Сделать переменную после FOR неопределенной
             2) поймать неопределенную переменную
             3) вместо fi-функции подставить условное выражение
*)

PROCEDURE OptimizeFi (p: TriadePtr): TriadePtr;
VAR m, n: Node;
    a1, a2: Arc;
    p1, p2, s, t: TriadePtr;
    i: INT;
BEGIN
    n := p^.NodeNo;
    FOR i:=0 TO LEN(p^.Params^)-1 DO
        IF (p^.Params^[i].tag = ir.y_Variable) THEN
            s := ir.Nodes^[ir.Nodes^[n].In^[i]].Last;
            IF ((s^.Op = ir.o_forstart) OR (s^.Op = ir.o_forcont)) AND
               (opAttrs.curr_mod.flag # pc.flag_java) &
               (p^.Params^[i].name = s^.Name) &
               (gr.FindOutArc (ir.Nodes^[n].InArcs^[i]) = 1)
            THEN
                ir.RemoveUse(p^.Params^[i]);
                ir.MakeParNothing( p^.Params^[i] );
                Changed := TRUE;
            END;
        END;
    END;
<* IF TARGET_VAX OR TARGET_RISC THEN *>
    RETURN gr.TestDeleteFi (p);
<* END *>
    IF (p^.Prev <> NIL) OR (p^.Next^.Op = ir.o_fi) THEN
        RETURN gr.TestDeleteFi (p);
    END;
    n := p^.NodeNo;
    IF (ir.Nodes^[n].NIn <> 2) OR
       (p^.Params^[0].tag <> ir.y_NumConst) OR
       (p^.Params^[1].tag <> ir.y_NumConst)
<* IF ~TARGET_SPARC THEN *>
       OR
       NOT (Calc.IsZero (p^.Params^[0].value, p^.ResType, p^.ResSize)
<* IF ~ TARGET_68k THEN *>
            & Calc.CompareWithInt (pc.sb_equ, p^.Params^[1].value, 1,
                                   p^.ResType, p^.ResSize)
<* END *>
            OR Calc.IsZero (p^.Params^[1].value, p^.ResType, p^.ResSize)
<* IF ~ TARGET_68k THEN *>
            & Calc.CompareWithInt (pc.sb_equ, p^.Params^[0].value, 1,
                                   p^.ResType, p^.ResSize)
<* END *>
           )
<* END *>
    THEN
        RETURN gr.TestDeleteFi (p);
    END;
    gr.MakeDominatorsTree;
    m  := ir.Nodes^[n].IDom;
    t  := ir.Nodes^[m].Last;
    a1 := ir.Nodes^[m].OutArcs^[0];
    a2 := ir.Nodes^[m].OutArcs^[1];
    IF (t^.Op <> ir.o_eq) & (t^.Op <> ir.o_le)         OR
       (t^.ResType = ir.t_float)                       OR
       (t^.ResSize = 8)                                OR
       NOT CheckPath (ir.Nodes^[m].Out^[0], n, p1, a1) OR
       NOT CheckPath (ir.Nodes^[m].Out^[1], n, p2, a2) OR
       (p1 <> NIL) OR (p2 <> NIL)
    THEN
        RETURN gr.TestDeleteFi (p);
    END;
(*
  Ok, we found fi-function we can optimize; now start optimization
*)
    IF t^.Op = ir.o_eq THEN
        s := ir.NewTriadeTInit (4, ir.o_move_eq, ir.y_Variable,
                                t^.ResType, t^.ResSize);
    ELSE (* t^.Op = ir.o_le *)
        s := ir.NewTriadeTInit (4, ir.o_move_le, ir.y_Variable,
                                t^.ResType, t^.ResSize);
    END;
    s^.ResType  := p^.ResType;
    s^.ResSize  := p^.ResSize;
    s^.Position := t^.Position;
    ir.CopyParam (t^.Params^[0], s^.Params^[0]);
    ir.CopyParam (t^.Params^[1], s^.Params^[1]);
    IF ir.Nodes^[n].InArcs^[0] = a1 THEN
        ir.CopyParam (p^.Params^[0], s^.Params^[2]);
        ir.CopyParam (p^.Params^[1], s^.Params^[3]);
    ELSE
        ir.CopyParam (p^.Params^[0], s^.Params^[3]);
        ir.CopyParam (p^.Params^[1], s^.Params^[2]);
    END;
    ir.SetDef (p^.Name, s);
    gr.InsertTriade (s, p);
    gr.KillTriade(p);
    gr.KillTriade(t);
(*
  Now join two nodes
*)
    ASSERT( (ir.Nodes^[m].Out[0] = n) OR
            (ir.Nodes[ ir.Nodes^[m].Out[0] ].Out[0] = n)
          );
    ASSERT( (ir.Nodes^[m].Out[1] = n) OR
            (ir.Nodes[ ir.Nodes^[m].Out[1] ].Out[0] = n)
          );
    gr.KillArc (ir.Nodes^[m].OutArcs^[1]);
    gr.KillArc (ir.Nodes^[m].OutArcs^[0]);
    WHILE ir.Nodes^[n].NIn <> 0 DO
        gr.KillArc (ir.Nodes^[n].InArcs^[0]);
    END;
    gr.RetargetAllArcs (m, n);
    p := ir.Nodes^[m].Last;
    WHILE (p <> NIL) & (p^.Op <> ir.o_getpar)
    DO
        t := p^.Prev;
        gr.DeleteTriade   (p);
        gr.PutTriadeFirst (p, n);
        p := t;
    END;
    IF m = ir.ZERONode THEN
        gr.MakeGoto (ir.ZERONode);
        gr.NewArc   (ir.ZERONode, n, TRUE);
    END;
(*
  All done... Now check for UNDEFINED...
*)
    ChangedGraph := TRUE;
(*
    IF s^.Params^[2].tag = ir.y_Nothing THEN
        gr.ReplaceByParam (s, s^.Params^[3]);
        RETURN ir.Nodes^[n].First;
    ELSIF s^.Params^[3].tag = ir.y_Nothing THEN
        gr.ReplaceByParam (s, s^.Params^[2]);
        RETURN ir.Nodes^[n].First;
    END;
*)
    RETURN s;
END OptimizeFi;

(*----------------------------------------------------------------------------*)

(*
  Выкинуть присваивание
*)

PROCEDURE TrySetPosition (p: ParamPtr; pos-: ir.TPOS);
VAR q: TriadePtr;
BEGIN
    IF p^.tag = ir.y_Variable THEN
        q := ir.Vars^[p^.name].Def;
        IF q^.Position.IsNull () THEN
            q^.Position := pos;
        END;
    END;
END TrySetPosition;

PROCEDURE OptimizeAssign (p: TriadePtr): TriadePtr;
VAR s: TriadePtr;

    PROCEDURE MoveCoords;
    VAR w: ParamPtr;
        q: TriadePtr;
    BEGIN
        IF NOT p^.Position.IsNull () THEN
            w := ir.Vars^[p^.Name].Use;
            WHILE w <> NIL DO
                q := w^.triade;
                IF (q^.Op = ir.o_fi) & w^.position.IsNull () THEN
                    w^.position := p^.Position;
                END;
                w := w^.next;
            END;
        END;
    END MoveCoords;

BEGIN
    s := p^.Next;
    IF (p^.Params^[0].tag = ir.y_Variable) &
       (ir.Vars^[p^.Params^[0].name].LocalNo = ir.TEMPORARY)
    THEN
        TrySetPosition (p^.Params^[0], p^.Position);
        MoveCoords;
        gr.ReplaceByParam (p, p^.Params^[0]);
        Changed := TRUE;
    ELSIF NOT MustPreserve (p) OR
          (p^.Params^[0].tag = ir.y_Variable) &
          (ir.Vars^[p^.Params^[0].name].LocalNo = ir.Vars^[p^.Name].LocalNo)
    THEN
        TrySetPosition (p^.Params^[0], p^.Position);
        MoveCoords;
        gr.ReplaceByParam (p, p^.Params^[0]);
        Changed := TRUE;
    END;
    RETURN s;
END OptimizeAssign;

(*----------------------------------------------------------------------------*)

(*
  Попытаться соптимизировать VAL
*)

PROCEDURE OptimizeVal (p: TriadePtr): TriadePtr;
VAR
  q, s: TriadePtr;
BEGIN
    s := p^.Next;
    IF (p^.ResSize = p^.OpSize) &
       (ir.ConstTags [p^.ResType] = ir.ConstTags [p^.OpType])
    THEN
        IF (p^.Params^[0].tag = ir.y_Variable) &
           (ir.Vars^[p^.Params^[0].name].LocalNo = ir.TEMPORARY)
        THEN
            ir.Vars^[p^.Params[0].name].LocalNo := ir.Vars^[p^.Name].LocalNo;
            ir.Vars^[p^.Name].LocalNo := ir.TEMPORARY;
            gr.ReplaceByParam (p, p^.Params^[0]);
            Changed := TRUE;
            RETURN s;
        ELSIF NOT MustPreserve (p) OR
              (p^.Params^[0].tag = ir.y_Variable) &
              (ir.Vars^[p^.Params^[0].name].LocalNo = ir.Vars^[p^.Name].LocalNo)
        THEN
            gr.ReplaceByParam (p, p^.Params^[0]);
            Changed := TRUE;
            RETURN s;
        END;
    END;
    IF (p^.Params^[0].tag = ir.ConstTags [p^.OpType]) & NOT MustPreserve (p) THEN
        ir.MakeParNum_Ex(p.Params[0],
                         ir.ConstTags [p^.ResType],
                         Calc.Val (p^.Params^[0].value,
                                   p^.OpType, p^.ResType,
                                   p^.OpSize, p^.ResSize),
                         FALSE
                         );
        IF Calc.overflow &
           (ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options <> ir.OptionsSet{})
        THEN
            MakeOverflow (p);
            RETURN NIL;
        END;
        gr.ReplaceByParam (p, p^.Params^[0]);
        Changed := TRUE;
        RETURN s;
    END;
    IF p^.Params^[0].tag = ir.y_Variable THEN
        q := ir.Vars^[p^.Params^[0].name].Def;
        IF (q^.Op = ir.o_val) &
           (q^.OpSize < q^.ResSize) & (p^.OpSize < p^.ResSize) &
           (q^.ResSize = p^.OpSize) & (q^.ResType = p^.OpType) &
           (ir.ConstTags [q^.OpType] = ir.y_NumConst) &
           (ir.ConstTags [q^.ResType] = ir.y_NumConst) &
           (ir.ConstTags [p^.OpType] = ir.y_NumConst) &
           (ir.ConstTags [p^.ResType] = ir.y_NumConst)
        THEN
            IF (q^.OpType = q^.ResType) & (p^.OpType = p^.ResType) OR
               (q^.OpType = ir.t_unsign)
            THEN
                ir.RemoveUse(p^.Params^[0]);
                ir.CopyParam (q^.Params^[0], p^.Params^[0]);
                p^.OpType := q^.OpType;
                p^.OpSize := q^.OpSize;
                Changed := TRUE;
                RETURN p;
            END;
        END;
    END;
    RETURN s;
END OptimizeVal;

(*----------------------------------------------------------------------------*)

PROCEDURE TryRemoveVC(p: TriadePtr): BOOLEAN;
VAR d: TriadePtr;
BEGIN
  IF p.Params[0].tag # ir.y_Variable THEN
    RETURN FALSE;
  END;
  d := ir.Vars[p.Params[0].name].Def;
  IF d.Op # ir.o_val THEN
    RETURN FALSE;
  END;

  -- если первая конверсия - расширяющая,
  -- а результирующий размер второй не меньше,
  -- чем то, что было с самого начала
  IF (d.ResSize >= d.OpSize) AND (d.OpSize = p.ResSize) THEN
    IF (d.OpType = d.ResType) AND
       (p.OpType = p.ResType)
    THEN
      gr.ReplaceByParam(p, d.Params[0]);
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END TryRemoveVC;

(*
  Попытаться соптимизировать CAST
*)

PROCEDURE OptimizeCast (p: TriadePtr): TriadePtr;
VAR s: TriadePtr;
    v: Calc.VALUE;
BEGIN
    s := p^.Next;
    IF (p^.ResSize = p^.OpSize) &
       (ir.ConstTags [p^.ResType] = ir.ConstTags [p^.OpType])
    THEN
        IF (p^.Params^[0].tag = ir.y_Variable) &
           (ir.Vars^[p^.Params^[0].name].LocalNo = ir.TEMPORARY)
        THEN
            ir.Vars^[p^.Params[0].name].LocalNo := ir.Vars^[p^.Name].LocalNo;
            ir.Vars^[p^.Name].LocalNo := ir.TEMPORARY;
            gr.ReplaceByParam (p, p^.Params^[0]);
            Changed := TRUE;
            RETURN s;
        ELSIF NOT MustPreserve (p) OR
              (p^.Params^[0].tag = ir.y_Variable) &
              (ir.Vars^[p^.Params^[0].name].LocalNo = ir.Vars^[p^.Name].LocalNo)
        THEN
            IF (p^.Params[0].tag = ir.y_NumConst) THEN
                 ir.MakeParNum(p^.Params^[0], Calc.Cast(p^.Params^[0].value,
                                                        p^.ResType,
                                                        p^.ResSize)
                              );
            END;
            gr.ReplaceByParam (p, p^.Params^[0]);
            Changed := TRUE;
            RETURN s;
        END;
    END;
   <* IF TARGET_386 OR TARGET_MIPS THEN *> -- host and target should comply to IEEE 754 for binary floatig point 
    IF (p^.Params^[0].tag = ir.ConstTags [p^.OpType]) & NOT MustPreserve (p) THEN
   <* ELSE *>
    IF (p^.Params^[0].tag = ir.ConstTags [p^.ResType]) & NOT MustPreserve (p) THEN
   <* END *>
        v := Calc.SystemCast( p^.Params^[0].value
                            , p^.OpType,p^.ResType
                            , p^.OpSize,p^.ResSize );
        ir.MakeParNum_Ex( p.Params[0]
                        , ir.ConstTags [p^.ResType]
                        , v
                        , FALSE
                        );
        gr.ReplaceByParam (p, p^.Params^[0]);
        Changed := TRUE;
        RETURN s;
    END;
    IF TryRemoveVC(p) THEN
      Changed := TRUE;
      RETURN s;
    END;
    RETURN s;
END OptimizeCast;

(*----------------------------------------------------------------------------*)

PROCEDURE MakeValBeforeTr(src: ir.ParamPtr;
                          src_type: ir.TypeType; src_sz: ir.SizeType;
                          dst_type: ir.TypeType; dst_sz: ir.SizeType;
                          q: ir.TriadePtr;
                          dst: ir.ParamPtr);
VAR val: ir.TriadePtr;
BEGIN
  IF (src_type # dst_type) OR
     (src_sz   # dst_sz)
  THEN
    val := ir.NewTriadeInit(1, ir.o_val, src_type, src_sz);
    val.ResType := dst_type;
    val.ResSize := dst_sz;
    ir.MoveParam(src, val.Params[0]);
    ir.GenResVar(val);
    gr.InsertTriade(val, q);
    ir.MakeParVar(dst, val.Name);
  ELSIF src # dst THEN
    ir.MoveParam(src, dst);
  END;
END MakeValBeforeTr;

-- insert o_hiword(par) before tr and return new variable created.
PROCEDURE MakeHiwordBeforeTr(par: ir.ParamPtr;
                     type: ir.TypeType; size: ir.SizeType;
                     tr: ir.TriadePtr ): ir.VarNum;
VAR h : ir.TriadePtr;
BEGIN
    h := ir.NewTriadeTInit(1, ir.o_hiword, ir.y_Variable,
                            type, size);
    h.ResSize := 4;
    ir.GenResVar(h);
    ir.CopyParam(par, h.Params[0]);
    gr.InsertTriade(h, tr);
    RETURN h.Name;
END MakeHiwordBeforeTr;

PROCEDURE ReplaceCheckLo64(p: TriadePtr);
VAR h1, h2 : ir.VarNum;
    le, eq, trap  : ir.TriadePtr;
    type   : ir.TypeType;
    nd0, nd1, nd2, nd3, nd4 : ir.Node;
BEGIN
    type := p.OpType;

    -- was: nd0: checklo: a1 >= a2 ? cont : trap
    --
    -- become:
    --      nd0: if hi(a1) <= hi(a2) then nd1 else nd4
    --
    --      nd1: if hi(a1) = hi(a2) then nd2 else nd3
    --
    --      nd2: checklo: lo(a1) >= lo(a2)
    --
    --      nd3: trap
    --
    --      nd4: continuation of the program
    --
    -- at first, we shall break the node immediately after p
    --
    nd0 := p.NodeNo;
    nd4 := gr.SplitNodeAfter(p, TRUE);
    gr.NewArc(nd0, nd4, TRUE);

    -- creating nd3 - node with o_error inside
    nd3 := gr.NewNode();

    nd1 := gr.SplitArc( ir.Nodes[nd0].OutArcs[0] );
    nd2 := gr.SplitArc( ir.Nodes[nd1].OutArcs[0] );
    gr.NewArc(nd1, nd3, TRUE);
    -- the control graph is built
    -- creating triades:
    -- nd0 : hiword, hiword, le
    h1 := MakeHiwordBeforeTr(p.Params[0], type, 8, p);
    h2 := MakeHiwordBeforeTr(p.Params[1], type, 8, p);
    le  := ir.NewTriadeTInit( 2, ir.o_le, ir.y_Nothing, type, 4 );
    ir.MakeParVar(le.Params[0], h1);
    ir.MakeParVar(le.Params[1], h2);
    gr.KillTriade(ir.Nodes[nd0].Last); -- kill o_goto
    gr.PutTriadeLast(le, nd0);

    -- nd3: error(indexException)
    trap := ir.NewTriadeTInit (3, ir.o_error, ir.y_Nothing,
                               opTune.index_ty, opTune.index_sz);
    ope.gen_modName(p^.Position, trap.Params[1], trap.Params[2]);
    trap^.Position := p^.Position;
    ir.MakeParNum( trap.Params[0],
                   Calc.GetInteger (opTune.indexException, opTune.index_sz) );
    gr.PutTriadeFirst(trap, nd3);

    -- nd1: eq
    eq  := ir.NewTriadeTInit( 2, ir.o_eq, ir.y_Nothing,
                              type, 4 );
    ir.MakeParVar(eq.Params[0], h1);
    ir.MakeParVar(eq.Params[1], h2);
    gr.KillTriade(ir.Nodes[nd1].Last); -- kill o_goto
    gr.PutTriadeLast(eq, nd1);

    -- nd2: val, val, checklo, goto. (we use p as checklo)
    gr.DeleteTriade(p);
    gr.PutTriadeFirst(p, nd2);
    MakeValBeforeTr( p.Params[0],
                     p.OpType, p.OpSize,
                     ir.t_unsign, 4,
                     p,
                     p.Params[0] );

    MakeValBeforeTr( p.Params[1],
                     p.OpType, p.OpSize,
                     ir.t_unsign, 4,
                     p,
                     p.Params[1] );
    p.OpSize := 4;
    p.ResSize := 4;
    p.OpType := ir.t_unsign;
    p.ResType := ir.t_unsign;
    -- we already have o_goto there
END ReplaceCheckLo64;

(*
  Попытаться соптимизировать Check
*)

PROCEDURE OptimizeCheckLo (p: TriadePtr): TriadePtr;
VAR
    q: TriadePtr;
BEGIN
    IF anz.AlwaysGeq (p^.Params^[0], p^.Params^[1],
                           p^.ResType, p^.ResSize, p)
    THEN
        Changed := TRUE;
        RETURN gr.KillTriade_Ex (p);
    ELSIF anz.AlwaysLss (p^.Params^[0], p^.Params^[1],
                              p^.ResType, p^.ResSize, p)
    THEN
        MakeCheckError (p);
        RETURN NIL;
    END;
    IF (p^.ResType = ir.t_int) &
       (p^.Params^[1].tag = ir.y_NumConst) &
       ir.EqParams (p^.Params^[1], anz.Ones [p^.ResType][p.ResSize], p^.ResType, p^.ResSize)
    THEN
        ir.SwapParams (p^.Params^[0], p^.Params^[1]);
        ir.CopyParam (anz.Zeroes [p^.ResType][p.ResSize], p^.Params^[0]);
        p^.Op := ir.o_checkhi;
        Changed := TRUE;
        RETURN p;
    END;
    q := p^.Next;
    IF MachineLevelOptimizations &
       (p^.ResType = ir.t_int) &
       (p^.Params^[1].tag = ir.y_NumConst) &
       Calc.IsZero (p^.Params^[1].value, p^.ResType, p^.ResSize) &
       ((q^.Op = ir.o_checkhi) &
        (q^.ResType = ir.t_int) & (q^.ResSize = p^.ResSize) &
        ir.EqParams (p^.Params^[0], q^.Params^[0], ir.t_int, p^.ResSize) OR
        (q^.Op = ir.o_checklo) &
        (q^.ResType = ir.t_int) & (q^.ResSize = p^.ResSize) &
        ir.EqParams (p^.Params^[0], q^.Params^[1], ir.t_int, p^.ResSize))
    THEN
        q^.ResType := ir.t_unsign;
        q^.OpType  := ir.t_unsign;
        RETURN gr.KillTriade_Ex (p);
    ELSIF MachineLevelOptimizations &
          (p.OpSize = 8) &
          (p.OpType IN ir.TypeTypeSet{ ir.t_int, ir.t_unsign })
    THEN
        ReplaceCheckLo64(p);
        ChangedGraph := TRUE;
        Changed := TRUE;
        RETURN q;
    END;
    RETURN q;
END OptimizeCheckLo;

(*----------------------------------------------------------------------------*)

PROCEDURE ReplaceCheckHi64(p: TriadePtr);
VAR h1, h2 : ir.VarNum;
    le, eq, trap  : ir.TriadePtr;
    type   : ir.TypeType;
    nd0, nd1, nd2, nd3, nd4 : ir.Node;
BEGIN
    type := p.OpType;

    -- was: nd0: checklo: a1 >= a2 ? cont : trap
    --
    -- become:
    --      nd0: if hi(a1) <= hi(a2) then nd1 else nd4
    --
    --      nd1: if hi(a1) = hi(a2) then nd2 else nd3
    --
    --      nd2: checklo: lo(a1) >= lo(a2)
    --
    --      nd3: trap
    --
    --      nd4: continuation of the program
    --
    -- at first, we shall break the node immediately after p
    --
    nd0 := p.NodeNo;
    nd4 := gr.SplitNodeAfter(p, TRUE);
    gr.NewArc(nd0, nd4, TRUE);

    -- creating nd3 - node with o_error inside
    nd3 := gr.NewNode();

    nd1 := gr.SplitArc( ir.Nodes[nd0].OutArcs[0] );
    nd2 := gr.SplitArc( ir.Nodes[nd1].OutArcs[0] );
    gr.NewArc(nd1, nd3, TRUE);
    -- the control graph is built
    -- creating triades:
    -- nd0 : hiword, hiword, le
    h1 := MakeHiwordBeforeTr(p.Params[0], type, 8, p);
    h2 := MakeHiwordBeforeTr(p.Params[1], type, 8, p);
    le  := ir.NewTriadeTInit( 2, ir.o_le, ir.y_Nothing, type, 4 );
    ir.MakeParVar(le.Params[0], h1);
    ir.MakeParVar(le.Params[1], h2);
    gr.KillTriade(ir.Nodes[nd0].Last); -- kill o_goto
    gr.PutTriadeLast(le, nd0);

    -- nd3: error(indexException)
    trap := ir.NewTriadeTInit (3, ir.o_error, ir.y_Nothing,
                               opTune.index_ty, opTune.index_sz);
    ope.gen_modName(p^.Position, trap.Params[1], trap.Params[2]);
    trap^.Position := p^.Position;
    ir.MakeParNum( trap.Params[0],
                   Calc.GetInteger (opTune.indexException, opTune.index_sz) );
    gr.PutTriadeFirst(trap, nd3);

    -- nd1: eq
    eq  := ir.NewTriadeTInit( 2, ir.o_eq, ir.y_Nothing,
                              type, 4 );
    ir.MakeParVar(eq.Params[0], h1);
    ir.MakeParVar(eq.Params[1], h2);
    gr.KillTriade(ir.Nodes[nd1].Last); -- kill o_goto
    gr.PutTriadeLast(eq, nd1);

    -- nd2: val, val, checklo, goto. (we use p as checklo)
    gr.DeleteTriade(p);
    gr.PutTriadeFirst(p, nd2);
    MakeValBeforeTr( p.Params[0],
                     p.OpType, p.OpSize,
                     ir.t_unsign, 4,
                     p,
                     p.Params[0] );

    MakeValBeforeTr( p.Params[1],
                     p.OpType, p.OpSize,
                     ir.t_unsign, 4,
                     p,
                     p.Params[1] );
    p.OpSize := 4;
    p.ResSize := 4;
    p.OpType := ir.t_unsign;
    p.ResType := ir.t_unsign;
    -- we already have o_goto there
END ReplaceCheckHi64;

(*
  Попытаться соптимизировать Check
*)

PROCEDURE OptimizeCheckHi (p: TriadePtr): TriadePtr;
VAR
    q: TriadePtr;
BEGIN
    IF anz.AlwaysLss (p^.Params^[0], p^.Params^[1],
                           p^.ResType, p^.ResSize, p)
    THEN
        Changed := TRUE;
        RETURN gr.KillTriade_Ex (p);
    ELSIF anz.AlwaysGeq (p^.Params^[0], p^.Params^[1],
                              p^.ResType, p^.ResSize, p)
    THEN
        anz.WriteTest("1", "Optimize.MakeError (1)");
        MakeCheckError (p);
        RETURN NIL;
    END;
    IF (p^.ResType = ir.t_int) &
       (p^.Params^[1].tag = ir.y_NumConst) &
       ir.EqParams (p^.Params^[1], anz.Ones [p^.ResType][p.ResSize], p^.ResType, p^.ResSize)
    THEN
        ir.SwapParams (p^.Params^[0], p^.Params^[1]);
        ir.CopyParam (anz.Zeroes [p^.ResType][p.ResSize], p^.Params^[0]);
        p^.Op := ir.o_checklo;
        Changed := TRUE;
        RETURN p;
    END;
    q := p^.Next;
    IF MachineLevelOptimizations &
       (p^.ResType = ir.t_int) &
       (q^.Op = ir.o_checklo) &
       (q^.ResType = ir.t_int) & (q^.ResSize = p^.ResSize) &
       ir.EqParams (p^.Params^[0], q^.Params^[0], ir.t_int, p^.ResSize) &
       (q^.Params^[1].tag = ir.y_NumConst) &
       Calc.IsZero (q^.Params^[1].value, p^.ResType, p^.ResSize)
    THEN
        p^.ResType := ir.t_unsign;
        p^.OpType  := ir.t_unsign;
        RETURN gr.KillTriade_Ex (q);
    ELSIF MachineLevelOptimizations &
          (p.OpSize = 8) &
          (p.OpType IN ir.TypeTypeSet{ ir.t_int, ir.t_unsign })
    THEN
        ReplaceCheckHi64(p);
        ChangedGraph := TRUE;
        Changed := TRUE;
        RETURN q;
    END;
    RETURN q;
END OptimizeCheckHi;

PROCEDURE OptimizeCheckB (p: TriadePtr): TriadePtr;
BEGIN
    IF anz.AlwaysLss (p^.Params^[0], p^.Params^[1],
                           p^.ResType, p^.ResSize, p)
    THEN
        Changed := TRUE;
        p.Op := ir.o_checklo;
        IF p.Params[1].tag = ir.y_Variable THEN
          ir.RemoveUse(p.Params[1]);
        END;
        ir.CopyParam(anz.Zeroes[p.ResType][p.ResSize], p.Params[1]);
        RETURN p.Next;
    ELSIF anz.AlwaysGeq (p^.Params^[0], p^.Params^[1],
                              p^.ResType, p^.ResSize, p)
    THEN
        MakeCheckError (p);
        RETURN NIL;
    ELSIF anz.AlwaysGeq (p^.Params^[0], anz.Zeroes[p.ResType][p.ResSize],
                              p^.ResType, p^.ResSize, p)
    THEN
        Changed := TRUE;
        p.Op := ir.o_checkhi;
    ELSIF anz.AlwaysLss (p^.Params^[0], anz.Zeroes[p.ResType][p.ResSize],
                              p^.ResType, p^.ResSize, p)
    THEN
        MakeCheckError (p);
        RETURN NIL;
    END;
    RETURN p.Next;
END OptimizeCheckB;

(*----------------------------------------------------------------------------*)

(*
  Parameter = NIL?
*)

PROCEDURE EqNil (p: ParamPtr; s: SizeType): BOOLEAN;
BEGIN
    RETURN (p^.tag = ir.y_NumConst) & (s = opTune.addr_sz) &
           Calc.CompareValues (pc.sb_equ, p^.value, Calc.GetNilPointer(),
                               opTune.addr_ty, opTune.addr_sz, TRUE);
END EqNil;

(*----------------------------------------------------------------------------*)

(*
  Попытаться соптимизировать Check
*)


PROCEDURE OptimizeCheckNil (p: TriadePtr): TriadePtr;
VAR q: TriadePtr;
BEGIN
    q := p.Next;
    IF EqNil (p^.Params^[0], p.OpSize) THEN
        MakeError (p, opTune.invalidLocation);
        RETURN NIL;
    ELSIF anz.AlwaysNeqNil (p^.Params^[0], p) THEN
        RETURN gr.KillTriade_Ex (p);
    END;
    RETURN q;
END OptimizeCheckNil;

(*----------------------------------------------------------------------------*)

(*
  Попробовать объединить 2 триады мультиопераций; это можно сделать, если:
    - результат первой используется только во второй
    - вторая операция совпадает с первой
    - совпадают длины
    - совпадают типы (ref, int и unsign считаются совпадающими)
    - либо обе триады безопасны, либо:
      - обе опасны
      - в точности совпадают типы
      - результатат первой не вычитается
      - результат первой стоит первым параметром во второй
    - вторая находится в том же или объемлющем по отношению к первой цикле
*)

PROCEDURE TryUniteMulti (p: TriadePtr; u: ParamPtr): BOOLEAN;
VAR i, j, k : INT;
    q       : TriadePtr;
    w       : ParamArray;
    r       : BOOLEAN;
BEGIN
    IF (u = NIL) OR (u^.next <> NIL) OR MachineLevelOptimizations THEN
        RETURN FALSE;
    END;
    q := u^.triade;
    IF (q^.Op <> p^.Op) OR MustPreserve (p)
       OR (q^.ResSize <> p^.ResSize)
       OR (p^.ResType <> q^.ResType) & (NOT ir.Ordinal [p^.ResType] OR
                                        NOT ir.Ordinal [q^.ResType])
       OR (q^.ResType = ir.t_float) & (LEN (p^.Params^) > 1)
       OR ((ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options <>
            ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * q^.Options)
           OR ((ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * q^.Options <> ir.OptionsSet{}) &
               ((p^.ResType <> q^.ResType) OR
                (u^.paramnumber <> 0) OR u^.reverse)))
          & (p^.ResType <> ir.t_float)
       OR ((ir.Nodes^[q^.NodeNo].LoopNo <> ir.Nodes^[p^.NodeNo].LoopNo) &
           NOT gr.LoopInLoop (ir.Nodes^[p^.NodeNo].LoopNo,
                              ir.Nodes^[q^.NodeNo].LoopNo))
    THEN
        RETURN FALSE;
    END;
    IF q^.Position.IsNull() THEN
        q^.Position := p^.Position;
    END;
    w := ir.NewParams (LEN (p^.Params^) + LEN (q^.Params^) - 1, q);
    k := 0;
    FOR i:=0 TO LEN(q^.Params^)-1 DO
        IF (q^.Params^[i].tag  = ir.y_Variable) &
           (q^.Params^[i].name = p^.Name)
        THEN
            r := q^.Params[i].reverse;
            FOR j:=0 TO LEN(p^.Params^)-1 DO
                ir.CopyParam (p^.Params^[j], w^[k]);
                ir.SetParamReverse(w^[k], p^.Params^[j].reverse <> r);
                INC (k);
            END;
        ELSE
            ir.CopyParamWithRev (q^.Params^[i], w^[k]);
            INC (k);
        END;
        IF q^.Params^[i].tag = ir.y_Variable THEN
            ir.RemoveUse(q^.Params^[i]);
        END;
    END;
    q^.Params := w;
    ir.RemoveVar (p^.Name);
    gr.KillTriade(p);
    Changed := TRUE;
    RETURN TRUE;
END TryUniteMulti;

(*----------------------------------------------------------------------------*)

(*
  Посчитать константный конструктор множества { a .. b }
*)

PROCEDURE OptimizeSetConstructor (p: TriadePtr): TriadePtr;
VAR s: TriadePtr;
    s1, s2: LONGINT;
BEGIN
    s := p^.Next;
    IF (p^.Params^[0].tag <> ir.y_NumConst) OR MustPreserve (p) THEN
        RETURN s;
    END;
    IF p^.Op = ir.o_loset THEN
        s1 := 0;
        s2 := Calc.ToInteger (p^.Params^[0].value, opTune.index_sz);
    ELSE
        s1 := Calc.ToInteger (p^.Params^[0].value, opTune.index_sz);
        s2 := p^.ResSize * 8 - 1;
    END;
    IF (s1 < 0) OR (s2 >= ORD(p^.ResSize) * 8) OR (s1 > s2) THEN
        MakeError (p, opTune.rangeException);
        RETURN NIL;
    END;
    ir.MakeParNum( par, Calc.SetConstructor (s1, s2, p^.ResSize) );
    gr.ReplaceByParam (p, par);
    RETURN s;
END OptimizeSetConstructor;

(*----------------------------------------------------------------------------*)

(*
  Соптимизировать INCL, EXCL
*)

PROCEDURE OptimizeInclExcl (p: TriadePtr): TriadePtr;
VAR r: TriadePtr;
    s: LONGINT;
--    u: SET;
BEGIN
    r := p^.Next;
    IF p^.Params^[1].tag = ir.y_NumConst THEN
        s := Calc.ToInteger (p^.Params^[1].value, opTune.index_sz);
        Changed := TRUE;
        IF (s < 0) OR (s >= ORD(p^.ResSize) * 8) THEN
            MakeError (p, opTune.rangeException);
            RETURN NIL;
        END;
--        u := { s };
        IF p^.Op = ir.o_excl THEN
            p^.Op := ir.o_and;
            ir.MakeParNum(p^.Params^[1], Calc.Unary (
                      pc.su_compl, p^.ResType, p^.ResSize, anz.Pow2[s]) );
        ELSE
            p^.Op := ir.o_or;
            ir.MakeParNum(p^.Params^[1], anz.Pow2[s] );
        END;
        RETURN p;
    END;
    RETURN r;
END OptimizeInclExcl;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать ABS
*)

(*
  Выразить ABS через размножение знака
*)


PROCEDURE DecomposeAbs (p: TriadePtr): TriadePtr;
VAR s, ext, xor, add: TriadePtr;
BEGIN
    s := p^.Next;
    IF ir.IsFloat [p^.ResType] THEN
        RETURN s;
    END;
    ext     := ir.NewTriadeLike (p, 1);
    EXCL (ext^.Options, ir.o_Checked);
    ext^.Op := ir.o_sgnext;
    ir.CopyParam (p^.Params^[0], ext^.Params^[0]);
    ir.GenVar (ir.TEMPORARY, ext^.Name, ext);
    gr.InsertTriade (ext, p);
    xor     := ir.NewTriadeLike (p, 2);
    EXCL (xor^.Options, ir.o_Checked);
    xor^.Op := ir.o_xor;
    ir.CopyParam  (p^.Params^[0], xor^.Params^[0]);
    ir.MakeParVar (xor^.Params^[1], ext^.Name);
    ir.GenVar (ir.TEMPORARY, xor^.Name, xor);
    gr.InsertTriade (xor, p);
    add     := ir.NewTriadeLike (p, 2);
    add^.Op := ir.o_add;
    ir.MakeParVar (add^.Params^[0], xor^.Name);
    ir.MakeParVar_Ex (add^.Params^[1], ext^.Name, TRUE, TRUE);
    ir.SetDef (p^.Name, add);
    gr.InsertTriade (add, p);
    gr.KillTriade(p);
    Changed := TRUE;
    RETURN ext;
END DecomposeAbs;

(*
  Оптимизировать ABS
*)


PROCEDURE OptimizeAbs (p: TriadePtr): TriadePtr;
VAR s: TriadePtr;
BEGIN
    s := p^.Next;
    IF NOT MustPreserve (p) THEN
        IF p^.Params^[0].tag = ir.ConstTags [p^.ResType] THEN
            ir.MakeParNum_Ex( par,
                              p^.Params^[0].tag,
                              Calc.Unary (pc.su_abs, p^.ResType, p^.ResSize,
                                          p^.Params^[0].value),
                              FALSE
                            );
            IF Calc.overflow &
               (ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options <> ir.OptionsSet{})
            THEN
                MakeError (p, opTune.wholeValueException);
                RETURN NIL;
            END;
            Changed := TRUE;
            gr.ReplaceByParam (p, par);
        ELSIF anz.AlwaysGeq (p^.Params^[0], anz.Zeroes [p^.ResType][p.ResSize],
                                  p^.ResType, p^.ResSize, p)
        THEN
            Changed := TRUE;
            gr.ReplaceByParam (p, p^.Params^[0]);
        ELSE
            RETURN DecomposeAbs (p);
        END;
    END;
    RETURN s;
END OptimizeAbs;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать shift
*)

PROCEDURE OptimizeShift (p: TriadePtr): TriadePtr;
VAR s: TriadePtr;
    r4: SYSTEM.SET32;    r2: SYSTEM.SET16;    r1: SYSTEM.SET8;
    c: SYSTEM.CARD32;
    i: LONGINT;
    v: VALUE;
    size: ir.SizeType;
BEGIN
    s := p^.Next;
    IF NOT MustPreserve (p) THEN
        IF IsZero (p^.Params^[1], p^.OpType,  p^.OpSize) OR
           IsZero (p^.Params^[0], p^.ResType, p^.ResSize)
        THEN
            Changed := TRUE;
            gr.ReplaceByParam (p, p^.Params^[0]);
        ELSIF (p^.Params^[0].tag = ir.y_NumConst) &
              (p^.Params^[1].tag = ir.y_NumConst)
        THEN
            v := p^.Params^[1].value;
            IF p^.OpType = ir.t_int THEN
                c := Calc.ToCardinal (v, p^.OpSize);
                IF c > MAX (LONGINT) THEN
                    RETURN s;
                END;
                i := c;
            ELSE
                i := Calc.ToInteger (v, p^.OpSize);
                IF i < 0 THEN
                    RETURN s;
                END;
            END;
            v := p^.Params^[0].value;
            size := p^.ResSize;
            CASE size OF
            | 4: IF p^.ResType = ir.t_int THEN
                     r4 := SYSTEM.VAL (SYSTEM.SET32, Calc.ToInteger (v, size));
                 ELSE
                     r4 := SYSTEM.VAL (SYSTEM.SET32, Calc.ToCardinal (v, size));
                 END;
                 REPEAT
                     CASE p^.Op OF
                     | ir.o_shl:     r4 := SYSTEM.SHIFT  (r4,  1);
                     | ir.o_shr:     r4 := SYSTEM.SHIFT  (r4, -1);
                     | ir.o_sar:     r4 := SYSTEM.SHIFT  (r4, -1) +
                                               r4 * (- { 0 .. size * 8 - 2 });
                     | ir.o_rol:     r4 := SYSTEM.ROTATE (r4,  1);
                     | ir.o_ror:     r4 := SYSTEM.ROTATE (r4, -1);
                     END;
                     DEC (i);
                 UNTIL i = 0;
                 IF p^.ResType = ir.t_int THEN
                     v := Calc.NewInteger (SYSTEM.VAL (LONGINT, r4), size);
                 ELSE
                     v := Calc.NewCardinal (SYSTEM.VAL (SYSTEM.CARD32, r4), size);
                 END;
            | 2: IF p^.ResType = ir.t_int THEN
                     r2 := SYSTEM.VAL (SYSTEM.SET16, Calc.ToInteger (v, size));
                 ELSE
                     r2 := SYSTEM.VAL (SYSTEM.SET16, Calc.ToCardinal (v, size));
                 END;
                 REPEAT
                   CASE p^.Op OF
                   | ir.o_shl:     r2 := SYSTEM.SHIFT  (r2,  1);
                   | ir.o_shr:     r2 := SYSTEM.SHIFT  (r2, -1);
                   | ir.o_sar:     r2 := SYSTEM.SHIFT  (r2, -1) +
                                     r2 * (- SYSTEM.SET16{ 0 .. size * 8 - 2 });
                   | ir.o_rol:     r2 := SYSTEM.ROTATE (r2,  1);
                   | ir.o_ror:     r2 := SYSTEM.ROTATE (r2, -1);
                   END;
                   DEC (i);
                 UNTIL i = 0;
                 IF p^.ResType = ir.t_int THEN
                     v := Calc.NewInteger (SYSTEM.VAL (SYSTEM.INT16, r2), size);
                 ELSE
                     v := Calc.NewCardinal (VAL (SYSTEM.CARD32,
                                       SYSTEM.VAL (SYSTEM.CARD16, r2)), size);
                 END;
            | 1: IF p^.ResType = ir.t_int THEN
                     r1 := SYSTEM.VAL (SYSTEM.SET8, Calc.ToInteger (v, size));
                 ELSE
                     r1 := SYSTEM.VAL (SYSTEM.SET8, Calc.ToCardinal (v, size));
                 END;
                 REPEAT
                   CASE p^.Op OF
                   | ir.o_shl:     r1 := SYSTEM.SHIFT  (r1,  1);
                   | ir.o_shr:     r1 := SYSTEM.SHIFT  (r1, -1);
                   | ir.o_sar:     r1 := SYSTEM.SHIFT  (r1, -1) +
                                     r1 * (- SYSTEM.SET8{ 0 .. size * 8 - 2 });
                   | ir.o_rol:     r1 := SYSTEM.ROTATE (r1,  1);
                   | ir.o_ror:     r1 := SYSTEM.ROTATE (r1, -1);
                   END;
                   DEC (i);
                 UNTIL i = 0;
                 IF p^.ResType = ir.t_int THEN
                     v := Calc.NewInteger (SYSTEM.VAL (SYSTEM.INT8, r1), size);
                 ELSE
                     v := Calc.NewCardinal (VAL (SYSTEM.CARD32,
                                       SYSTEM.VAL (SYSTEM.CARD8, r1)), size);
                 END;
            ELSE RETURN s
            END;

            p^.Params^[0].value := v;
            Changed := TRUE;
            gr.ReplaceByParam (p, p^.Params^[0]);
        END;
    END;
    RETURN s;
END OptimizeShift;

(*----------------------------------------------------------------------------*)

(* trying to make constant calculation *)
PROCEDURE OptimizeNOT(p: TriadePtr) :TriadePtr;
VAR s: TriadePtr;
BEGIN
  s := p^.Next;
  IF (p^.Params^[0].tag = ir.y_NumConst) & NOT MustPreserve (p) THEN
    ir.MakeParNum(par, Calc.Unary (pc.su_not, p^.ResType, p^.ResSize, p.Params[0].value) );
    gr.ReplaceByParam (p, par);
    Changed := TRUE;
  END;
  RETURN s;
END OptimizeNOT;

(* trying to make constant calculation *)
PROCEDURE OptimizeCAP(p: TriadePtr) :TriadePtr;
VAR s: TriadePtr;
BEGIN
  s := p^.Next;
  IF (p^.Params^[0].tag = ir.y_NumConst) & NOT MustPreserve (p) THEN
    ir.MakeParNum(par, Calc.Unary (pc.su_cap, p^.ResType, p^.ResSize, p.Params[0].value));
    gr.ReplaceByParam (p, par);
    Changed := TRUE;
  END;
  RETURN s;
END OptimizeCAP;

(*----------------------------------------------------------------------------*)

<* IF TARGET_68k THEN *>

(*
  Заменить
    a | (1 rol x)
  на
    INCL (a, x)
  и то же самое для & и EXCL.
*)

PROCEDURE MkInclExcl (p, s: TriadePtr;
                      l: INT; op: ir.Operation): TriadePtr;  -- p - OR, s - ROL
VAR r: TriadePtr;
    q: ParamPtr;
BEGIN
    INCL (s^.Options, ir.o_Silent);
    q := s^.Params^[1];
    r := ir.NewTriadeLike (p, 2);
    r^.OpSize := s^.OpSize;
    r^.OpType := s^.OpType;
    r^.Op     := op;
    gr.InsertTriade (r, p);
    ir.CopyParam (p^.Params^[0], r^.Params^[0]);
    IF l = 0 THEN
        ir.CopyParam (q, r^.Params^[1]);
    ELSE
        s := ir.NewTriadeTInit (2, ir.o_add, ir.y_Variable,
                                s^.OpType, s^.OpSize);
        s^.Position := p^.Position;
        ir.GenVar (ir.TEMPORARY, s^.Name, s);
        ir.CopyParam  (q, s^.Params^[0]);
        ir.MakeParNum (s^.Params^[1], Calc.GetInteger (l, s^.OpSize));
        gr.InsertTriade (s, r);
        ir.MakeParVar (r^.Params^[1], s^.Name);
    END;
    ir.SetDef (p^.Name, r);
    p := gr.KillTriade_Ex (p);
    Changed := TRUE;
    RETURN r;
END MkInclExcl;

<* END *>

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать OR
*)

PROCEDURE OptimizeOr (p: TriadePtr): TriadePtr;
VAR
  i: INT;
  q: ParamPtr;
  s: TriadePtr;
 <* IF TARGET_68k THEN *>
  l: INT;
  r: TriadePtr;
 <* END *>
BEGIN
(*
  Если нельзя серьезно оптимизировать - черт с ним
*)
    s := p^.Next;
    IF MustPreserve (p) THEN
        RETURN s;
    END;
(*
  Удалить OR с 0 и с одними единицами; попытаться сделать константный OR
*)
    q := NIL;
    FOR i:=0 TO LEN(p^.Params^)-1 DO
        IF p^.Params^[i].tag = ir.y_NumConst THEN
            IF q = NIL THEN
                q := p^.Params^[i];
                IF Calc.IsZero (q^.value, p^.ResType, p^.ResSize) THEN
                    Changed := TRUE;
                    RETURN gr.DeleteParameter (p, i);
                ELSIF Calc.IsOnes (q^.value, p^.ResType, p^.ResSize) THEN
                    Changed := TRUE;
                    gr.ReplaceByParam (p, q);
                    RETURN s;
                END;
            ELSE
                ir.MakeParNum( par, Calc.Binary (pc.sb_or,
                                                 p^.ResType, p^.ResSize,
                                                 q^.value, p^.Params^[i].value));
                Changed := TRUE;
                RETURN Replace2Pars (p, par, q^.paramnumber, i);
            END;
        END;
    END;
<* IF TARGET_68k THEN *>
    IF (LEN (p^.Params^) = 2) & (p^.Params^[1].tag = ir.y_Variable) &
       (ir.Vars^[p^.Params^[1].name].Def^.Op = ir.o_rol) &
       (ir.Vars^[p^.Params^[1].name].Def^.Params^[0].tag = ir.y_NumConst)
    THEN
        r := ir.Vars^[p^.Params^[1].name].Def;
        l := Calc.IsPowerOfTwo (r^.Params^[0].value, r^.ResType, r^.ResSize);
        IF l <> -1 THEN
            RETURN MkInclExcl (p, r, l, ir.o_incl);
        END;
    END;
<* END *>
(*
  Ничего не получилось
*)
    RETURN s;
END OptimizeOr;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать AND
*)

PROCEDURE OptimizeAnd (p: TriadePtr): TriadePtr;
VAR
  i: INT;
  q: ParamPtr;
  s: TriadePtr;
 <* IF TARGET_68k THEN *>
  r: TriadePtr;
  l: INT;
 <* END *>
BEGIN
(*
  Если нельзя серьезно оптимизировать - черт с ним
*)
    s := p^.Next;
    IF MustPreserve (p) THEN
        RETURN s;
    END;
(*
  Удалить AND с 0 и с одними единицами; попытаться сделать константный AND
*)
    q := NIL;
    FOR i:=0 TO LEN(p^.Params^)-1 DO
        IF p^.Params^[i].tag = ir.y_NumConst THEN
            IF q = NIL THEN
                q := p^.Params^[i];
                IF Calc.IsOnes (q^.value, p^.ResType, p^.ResSize) THEN
                    Changed := TRUE;
                    RETURN gr.DeleteParameter (p, i);
                ELSIF Calc.IsZero (q^.value, p^.ResType, p^.ResSize) THEN
                    Changed := TRUE;
                    gr.ReplaceByParam (p, q);
                    RETURN s;
                END;
            ELSE
                ir.MakeParNum(par,  Calc.Binary (pc.sb_and,
                                                 p^.ResType, p^.ResSize,
                                                 q^.value, p^.Params^[i].value));
                Changed := TRUE;
                RETURN Replace2Pars (p, par, q^.paramnumber, i);
            END;
        END;
    END;
<* IF TARGET_68k THEN *>
    IF (LEN (p^.Params^) = 2) & (p^.Params^[1].tag = ir.y_Variable) &
       (ir.Vars^[p^.Params^[1].name].Def^.Op = ir.o_rol) &
       (ir.Vars^[p^.Params^[1].name].Def^.Params^[0].tag = ir.y_NumConst)
    THEN
        r := ir.Vars^[p^.Params^[1].name].Def;
        l := Calc.IsNotPowerOfTwo (r^.Params^[0].value, r^.ResType, r^.ResSize);
        IF l <> -1 THEN
            RETURN MkInclExcl (p, r, l, ir.o_excl);
        END;
    END;
<* END *>
(*
  Ничего не получилось
*)
    RETURN s;
END OptimizeAnd;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать ANDNOT
*)

PROCEDURE OptimizeAndNot (p: TriadePtr): TriadePtr;
VAR i: INT;
    q: ParamPtr;
    s: TriadePtr;
BEGIN
(*
  Если нельзя серьезно оптимизировать - черт с ним
*)
    s := p^.Next;
    IF MustPreserve (p) THEN
        RETURN s;
    END;
(*
  Удалить ANDNOT с 0; попытаться сделать константный ANDNOT
*)
    q := NIL;
    FOR i:=0 TO LEN(p^.Params^)-1 DO
        IF p^.Params^[i].tag = ir.y_NumConst THEN
            IF q = NIL THEN
                q := p^.Params^[i];
                IF Calc.IsZero (q^.value, p^.ResType, p^.ResSize) THEN
                    Changed := TRUE;
                    IF i = 0 THEN
                        gr.ReplaceByParam (p, q);
                        RETURN s;
                    ELSE
                        RETURN gr.DeleteParameter (p, i);
                    END;
                END;
            ELSE
                ir.MakeParNum(par, Calc.Binary (pc.sb_bic,
                                                p^.ResType, p^.ResSize,
                                                q^.value, p^.Params^[i].value));
                Changed := TRUE;
                RETURN Replace2Pars (p, par, q^.paramnumber, i);
            END;
        END;
    END;
(*
  Ничего не получилось
*)
    RETURN s;
END OptimizeAndNot;

(*----------------------------------------------------------------------------*)

PROCEDURE TryMoveOutFromVals(p: TriadePtr): BOOLEAN;
VAR tr1, tr3: TriadePtr;
    par_v, par_c: ir.ParamPtr;
    t1, t2: ir.VarNum;
BEGIN
  IF (LEN(p.Params^) # 2) THEN
    RETURN FALSE;
  END;

  IF p.Params[0].tag = ir.y_NumConst THEN
    par_c := p.Params[0];
    par_v := p.Params[1];
    IF p.Params[1].tag # ir.y_Variable THEN
      RETURN FALSE;
    END;
  ELSIF p.Params[1].tag = ir.y_NumConst THEN
    par_c := p.Params[1];
    par_v := p.Params[0];
    IF p.Params[0].tag # ir.y_Variable THEN
      RETURN FALSE;
    END;
  ELSE
    RETURN FALSE;
  END;

  IF (ir.Vars[par_v.name].Use = NIL) OR
     (ir.Vars[par_v.name].Use.next # NIL)
  THEN
    RETURN FALSE;
  END;

  tr1 := ir.Vars[par_v.name].Def;
  IF (tr1.Op # ir.o_val) OR
     (tr1.OpSize > 4) OR (tr1.OpType = ir.t_float)
  THEN
    RETURN FALSE;
  END;

  IF (ir.Vars[p.Name].Use = NIL) OR
     (ir.Vars[p.Name].Use.next # NIL)
  THEN
    RETURN FALSE;
  END;

  tr3 := ir.Vars[p.Name].Use.triade;
  IF (tr3.Op # ir.o_val)    AND
     (tr3.Op # ir.o_cast)
  THEN
    RETURN FALSE;
  END;

  t1 := tr1.Name;
  t2 := p.Name;

  -- we've checked all conditions, now will make the optimization:
  -- was:
  --   t1 := val( t0 )    -- tr1
  --   t2 := t1 op par_c  -- p( par_v, par_c )
  --   t3 := cast( t2 )   -- tr3
  -- become:
  --   t1 := t0 op par_c1 -- p( t0, par_c' )
  --   t2 := val( t1 )    -- tr1
  --   t3 := cast( t2 )   -- tr3
  -- where
  -- C1 = cast C from t1 type to t0 type
  --
  p.OpType := tr1.OpType;
  p.ResType := tr1.OpType;
  p.OpSize := tr1.OpSize;
  p.ResSize := tr1.OpSize;

  gr.DeleteTriade(p);
  gr.InsertTriade(p, tr1);

  ir.MakeParNum(par_c, Calc.Cast(par_c.value, tr1.OpType, tr1.OpSize) );
  ir.RemoveUse(par_v);
  ir.MoveParam(tr1.Params[0], par_v);
  ir.SetDef(t1, p);

  ir.MakeParVar(tr1.Params[0], t1);
  ir.SetDef(t2, tr1);
  RETURN TRUE;
END TryMoveOutFromVals;

(*
  Оптимизировать XOR
*)

PROCEDURE OptimizeXor (p: TriadePtr): TriadePtr;
VAR i: INT;
    q: ParamPtr;
    nxt: TriadePtr;
BEGIN
(*
  Если нельзя серьезно оптимизировать - черт с ним
*)
    IF MustPreserve (p) THEN
        RETURN p^.Next;
    END;
(*
  Попытаться удалить XOR с 0, а также сделать константный XOR
*)
    q := NIL;
    FOR i:=0 TO LEN(p^.Params^)-1 DO
        IF p^.Params^[i].tag = ir.y_NumConst THEN
            IF q = NIL THEN
                q := p^.Params^[i];
                IF Calc.IsZero (q^.value, p^.ResType, p^.ResSize) THEN
                    Changed := TRUE;
                    RETURN gr.DeleteParameter (p, i);
                END;
            ELSE
                ir.MakeParNum(par, Calc.Binary (pc.sb_xor,
                                                p^.ResType, p^.ResSize,
                                                q.value, p^.Params^[i].value) );
                Changed := TRUE;
                RETURN Replace2Pars (p, par, q^.paramnumber, i);
            END;
        END;
    END;

    nxt :=p.Next;
    IF TryMoveOutFromVals(p) THEN
      Changed := TRUE;
      RETURN nxt;
    END;

    RETURN p^.Next;
END OptimizeXor;

(*----------------------------------------------------------------------------*)

(*
  Попытаться выполнить константную адресную арифметику; если удалось,
                то результат записать в par, а в Calc.overflow - FALSE
*)

PROCEDURE TryAddAddresses (p, q: ParamPtr): BOOLEAN;
VAR offset: LONGINT;
BEGIN
    IF (p^.tag = ir.y_AddrConst) & NOT p^.reverse THEN
        IF (q^.tag = ir.y_NumConst) THEN
            IF q^.reverse THEN
                offset := p^.offset - Calc.ToInteger (q^.value,
                                                           opTune.index_sz);
            ELSE
                offset := p^.offset + Calc.ToInteger (q^.value,
                                                           opTune.index_sz);
            END;
            ir.MakeParAddr(par, p.name, offset);
            par^.value   := p^.value;
            Calc.overflow := FALSE;
            RETURN TRUE;
        ELSIF (q^.tag = ir.y_AddrConst) & q^.reverse & (p^.name = q^.name) &
              ((p^.name <> MAX (Local)) OR (p^.value = q^.value))
        THEN
            ir.MakeParNum(par, Calc.GetInteger (p^.offset - q^.offset,
                                                opTune.index_sz)
                         );
            Calc.overflow := FALSE;
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END TryAddAddresses;

(*----------------------------------------------------------------------------*)

(*
  Попытаться сложить 2 параметра указанного типа; если удалось,
  то результат записать в par, а в Calc.overflow записать, было ли переполнение
*)

PROCEDURE TryAddConsts (p, q: ParamPtr; g: TagType;
                        t: TypeType; s: SizeType): BOOLEAN;
VAR v: ir.VALUE;
BEGIN
    IF (p^.tag = g) & (q^.tag = g) THEN
        IF p^.reverse THEN
            IF q^.reverse THEN
                v := Calc.Unary (pc.su_neg, t, s,
                                 Calc.Binary (pc.sb_plus, t, s,
                                              p^.value, q^.value));
            ELSE
                v := Calc.Binary (pc.sb_minus, t, s,
                                  q^.value, p^.value);
            END;
        ELSE
            IF q^.reverse THEN
                v := Calc.Binary (pc.sb_minus, t, s,
                                  p^.value, q^.value);
            ELSE
                v := Calc.Binary (pc.sb_plus, t, s,
                                  p^.value, q^.value);
            END;
        END;
        ir.MakeParNum_Ex(par, g, v, FALSE);
        RETURN TRUE;
    END;
    RETURN FALSE;
END TryAddConsts;

(*----------------------------------------------------------------------------*)

(*
  Можно ли сложить два параметра? Если да, то сложить; результат записать в par,
                                  а было ли переполнение - в Calc.overflow
*)

PROCEDURE TryAdd (p, q: ParamPtr; t: TypeType; s: SizeType): BOOLEAN;
BEGIN
    CASE t OF
    | ir.t_ref,
      ir.t_unsign,
      ir.t_int:     RETURN TryAddAddresses (p, q) OR
                           TryAddAddresses (q, p) OR
                           TryAddConsts    (p, q, ir.y_NumConst, t, s);
    | ir.t_float:   RETURN TryAddConsts (p, q, ir.y_RealConst, t, s);
    ELSE
        RETURN FALSE;
    END;
END TryAdd;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать сложение
*)

PROCEDURE OptimizeAddition (p: TriadePtr): TriadePtr;
VAR i: INT;
    v: VALUE;
    r: TriadePtr;
    w, q: ParamPtr;

    (*
      Заменить
          a * const + b + c * const
      на
          (a + c) * const + b
      Оптимизация написана в частном виде, т.к. она нужна исключительно для SL/1
    *)

    PROCEDURE UniteMultiplication (p1, p2: ParamPtr): TriadePtr;
    VAR s1, s2, a, m: TriadePtr;
    BEGIN
        s1 := ir.Vars^[p1^.name].Def;
        s2 := ir.Vars^[p2^.name].Def;
        a := ir.NewTriadeLike (p, 2);
        ir.CopyParam (s1^.Params^[0], a^.Params^[0]);
        ir.SetParamReverse(a^.Params^[0], p1^.reverse);
        ir.CopyParam (s2^.Params^[0], a^.Params^[1]);
        ir.SetParamReverse(a^.Params^[1], p2^.reverse);
        ir.GenVar (ir.TEMPORARY, a^.Name, a);
        IF s1^.NodeNo = p^.NodeNo THEN
            IF s2^.NodeNo = p^.NodeNo THEN
                IF gr.DominatesTriade (s1, s2) THEN
                    gr.InsertTriade (a, s2);
                ELSE
                    gr.InsertTriade (a, s1);
                END;
            ELSE
                gr.InsertTriade (a, s1);
            END;
        ELSIF s2^.NodeNo = p^.NodeNo THEN
                gr.InsertTriade (a, s2);
        ELSE
            m := ir.Nodes^[p^.NodeNo].First;
            WHILE m^.Op = ir.o_fi DO
                m := m^.Next;
            END;
            gr.InsertTriade (a, m);
        END;
        m := ir.NewTriadeLike (s1, 2);
        ir.MakeParVar (m^.Params^[0], a^.Name);
        ir.MakeParNum (m^.Params^[1], s1^.Params^[1].value);
        ir.GenVar (ir.TEMPORARY, m^.Name, m);
        gr.PutAfterTriade (m, a);
        INCL (s1^.Options, ir.o_Silent);
        INCL (s2^.Options, ir.o_Silent);
        ir.MakeParVar_Ex(par, m^.Name, FALSE, FALSE);
        Changed := TRUE;
        RETURN Replace2Pars (p, par, p1^.paramnumber, p2^.paramnumber);
    END UniteMultiplication;


VAR reverse: BOOLEAN;
BEGIN
(*
  Прежде всего заменить a + (-5) на a - 5, a - 0 на a + 0, - 5 + a на (-5) + a
*)
    CASE p^.ResType OF
    | ir.t_int,
      ir.t_ref,
      ir.t_unsign:
        FOR i:=0 TO LEN(p^.Params^)-1 DO
            IF (p^.Params^[i].tag = ir.y_NumConst) &
               (   (i <> 0) & p^.Params^[i].value.is_neg() &
                   NOT Calc.CompareValues(pc.sb_equ,
                                          p^.Params[i].value,
                                          Calc.MinValue(p^.ResType, p^.ResSize),
                                          p^.ResType, p^.ResSize,
                                          TRUE)
                 OR
                   p^.Params^[i].reverse &
                   Calc.IsZero (p^.Params^[i]^.value,
                                p^.ResType, p^.ResSize)
                 OR
                   p^.Params^[i].reverse &
                   (i = 0)
               )
            THEN
                v := Calc.Unary (pc.su_neg, p^.ResType, p^.ResSize,
                                 p^.Params^[i].value);
                IF NOT Calc.overflow THEN
                    reverse := p^.Params^[i].reverse;
                    ir.MakeParNum_Ex(p^.Params^[i],
                                     ir.ConstTags[p.ResType],
                                     v,
                                     NOT reverse);
                    Changed := TRUE;
                END;
            END;
        END;
    | ir.t_float:
        FOR i:=0 TO LEN(p^.Params^)-1 DO
            IF (p^.Params^[i].tag = ir.y_RealConst) &
               ((i <> 0) & p^.Params^[i].value.is_neg() OR
                ((i = 0) OR Calc.IsZero (p^.Params^[i].value,
                                         ir.t_float, p^.ResSize)) &
               p^.Params^[i].reverse)
            THEN
                v := Calc.Unary (pc.su_neg, p^.ResType, p^.ResSize,
                                 p^.Params^[i].value);
                IF NOT Calc.overflow THEN
                    reverse := p^.Params^[i].reverse;
                    ir.MakeParNum_Ex(p^.Params^[i],
                                     ir.ConstTags[p.ResType],
                                     v,
                                     NOT reverse);
                    Changed := TRUE;
                END;
            END;
        END;
    END;
    IF (LEN (p^.Params^) = 1) & NOT p^.Params^[0].reverse THEN
        Changed := TRUE;
        p^.Op := ir.o_assign;
        RETURN p;
    END;
(*
  Если нельзя серьезно оптимизировать - черт с ним
*)
    IF MustPreserve (p) THEN
        RETURN p^.Next;
    END;
(*
  Удалить сложение с 0
*)
    FOR i:=0 TO LEN(p^.Params^)-1 DO
        IF IsZero (p^.Params^[i], p^.ResType, p^.ResSize) THEN
          IF (p.ResType = ir.t_ref) AND (i=0) THEN
            MakeError(p, opTune.invalidLocation);
            Changed := TRUE;
            RETURN NIL;
          ELSE
            Changed := TRUE;
            RETURN gr.DeleteParameter (p, i);
          END;
        END;
    END;
(*
  Удалить a - a; заменить (x + y) - y на x
*)
    IF LEN (p^.Params^) >= 2 THEN
        ir.SetParamReverse(p^.Params^[0], NOT p^.Params^[0].reverse);
        IF ir.IsFloat [p^.ResType] OR
           (ir.OptionsSet{ ir.o_Dangerous, ir.o_Checked } * p^.Options <> ir.OptionsSet{}) THEN
            IF ir.EqParams (p^.Params^[0], p^.Params^[1],
                            p^.ResType, p^.ResSize)
            THEN
                Changed := TRUE;
                RETURN Replace2Pars (p, anz.Zeroes [p^.ResType][p^.ResSize], 0, 1);
            END;
        ELSE
            FOR i:=1 TO LEN(p^.Params^)-1 DO
                IF ir.EqParams (p^.Params^[0], p^.Params^[i],
                                p^.ResType, p^.ResSize)
                THEN
                    Changed := TRUE;
                    RETURN Replace2Pars (p, anz.Zeroes [p^.ResType][p.ResSize], 0, i);
                END;
            END;
        END;
        ir.SetParamReverse(p^.Params^[0], NOT p^.Params^[0].reverse);
        IF NOT p^.Params^[0].reverse & p^.Params^[1].reverse THEN
            IF (p^.ResType IN ir.TypeTypeSet{ir.t_int, ir.t_unsign}) &
               (*p^.Params^[1].reverse &  already checked in previous condition *)
               (p^.Params^[0].tag = ir.y_Variable)
            THEN                                        (* (x + y) - y --> x *)
                ir.SetParamReverse(p^.Params^[1], FALSE);
                r := ir.Vars^[p^.Params^[0].name].Def;
                IF (r^.Op = ir.o_add) &
                   (r^.ResType IN ir.TypeTypeSet{ir.t_int, ir.t_unsign}) &
                   (r^.ResSize = p^.ResSize) &
                   (LEN (r^.Params^) = 2)
                THEN
                    IF ir.EqParams (p^.Params^[1], r^.Params^[0],
                                    p^.ResType, p^.ResSize)
                    THEN
                        Changed := TRUE;
                        INCL (r^.Options, ir.o_Silent);
                        RETURN Replace2Pars (p, r^.Params^[1], 0, 1);
                    ELSIF ir.EqParams (p^.Params^[1], r^.Params^[1],
                                       p^.ResType, p^.ResSize)
                    THEN
                        Changed := TRUE;
                        INCL (r^.Options, ir.o_Silent);
                        RETURN Replace2Pars (p, r^.Params^[0], 0, 1);
                    END;
                END;
                ir.SetParamReverse(p^.Params^[1], TRUE);
            END;
        END;
    END;
(*
  Заменить (-a) + b на b - a
*)
    IF (ir.IsFloat [p^.ResType] OR
        (ir.OptionsSet{ ir.o_Dangerous, ir.o_Checked } * p^.Options = ir.OptionsSet{})) &
       (LEN (p^.Params^) > 1)
    THEN
        IF p^.Params^[0].reverse & NOT p^.Params^[1].reverse THEN
            ir.SwapParams (p^.Params^[0], p^.Params^[1]);
            Changed := TRUE;
        END;
    END;

(* Заменить a := - const на a := -const (больше это не делается нигде).
   При этом возможно переполнение.
*)

    IF (LEN (p^.Params^) = 1) & p^.Params^[0].reverse THEN
        IF  (p^.Params^[0].tag = ir.ConstTags [p^.ResType]) THEN
            ir.MakeParNum( p^.Params^[0], Calc.Unary (pc.su_neg,
                                                      p^.ResType,
                                                      p^.ResSize,
                                                      p^.Params^[0].value)
                         );
            p^.Op := ir.o_assign;
            Changed := TRUE;
            IF Calc.overflow AND
               (ir.IsFloat [p^.ResType] OR
                (ir.OptionsSet{ ir.o_Dangerous, ir.o_Checked } * p^.Options <> ir.OptionsSet{}))
            THEN
                MakeOverflow (p);
                RETURN NIL;
            END;

(* Заменить x := - y типа CARDINAL на check y <= 0; x := 0; *)

        ELSIF (p^.ResType = ir.t_unsign) &
              (ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options <> ir.OptionsSet{})
        THEN
            r := ir.NewTriadeLike (p, 2);
            r^.Op := ir.o_checkhi;
            r^.Tag := ir.y_Nothing;
            ir.MakeParNum(par, Calc.GetValue(1, p.OpType, p.OpSize) );
            ir.CopyParam (p^.Params [0], r^.Params [0]);
            ir.CopyParam (par, r^.Params [1]);
            gr.InsertTriade (r, p);
            r := p^.Next;
            ir.MakeParNum( par, Calc.GetValue(0, p.ResType, p.ResSize));
            gr.ReplaceByParam (p, par);
            Changed := TRUE;
            RETURN r;
        END;
    END;

    IF
       ir.IsFloat [p^.ResType] OR
       (ir.OptionsSet{ ir.o_Dangerous, ir.o_Checked } * p^.Options <> ir.OptionsSet{})
    THEN
(*
  Опасная операция - можно складывать только 2 первых параметра
*)
        IF (LEN (p^.Params^) >= 2) &
           TryAdd (p^.Params^[0], p^.Params^[1], p^.ResType, p^.ResSize)
        THEN
            IF Calc.overflow THEN
                MakeOverflow (p);
                RETURN NIL;
            ELSE
                Changed := TRUE;
                RETURN Replace2Pars (p, par, 0, 1);
            END;
        END;
    ELSE
(*
  Безопасная операция - можно складывать 2 любых параметра
*)
        q := NIL;
        FOR i:=0 TO LEN(p^.Params^)-1 DO
            w := p^.Params^[i];
            IF (w^.tag = ir.ConstTags [p^.ResType]) OR
               (p^.ResType IN ir.TypeTypeSet{ ir.t_int, ir.t_unsign, ir.t_ref }) &
               (w^.tag = ir.y_AddrConst)
            THEN
                IF q = NIL THEN
                    q := w;
                ELSIF TryAdd (q, w, p^.ResType, p^.ResSize) THEN
                    Changed := TRUE;
                    RETURN Replace2Pars (p, par, q^.paramnumber, i);
                END;
            END;
        END;
(*
  А теперь попробовать заменить
      a * const + b + c * const
  на
      (a + c) * const + b
  Оптимизация написана в частном виде, т.к. она нужна исключительно для SL/1
*)
        q := NIL;
        FOR i:=0 TO LEN(p^.Params^)-1 DO
            w := p^.Params^[i];
            IF (w^.tag = ir.y_Variable) & (w^.next = NIL) &
               (ir.Vars^[w^.name].Use = w) &
               (ir.Vars^[w^.name].Def^.Op = ir.o_mul) &
               NOT (ir.o_Checked IN ir.Vars^[w^.name].Def^.Options) &
               (LEN (ir.Vars^[w^.name].Def^.Params^) = 2) &
               (ir.Vars^[w^.name].Def^.Params^[1].tag = ir.y_NumConst)
            THEN
                IF q = NIL THEN
                    q := w;
                ELSIF ir.EqParams (ir.Vars^[w^.name].Def^.Params^[1],
                                   ir.Vars^[q^.name].Def^.Params^[1],
                                   p^.ResType, p^.ResSize)
                THEN
                    RETURN UniteMultiplication (q, w);
                END;
            END;
        END;
    END;

(*
  Ничего не получилось
*)
    RETURN p^.Next;
END OptimizeAddition;

(*----------------------------------------------------------------------------*)

(*
  В программе написано:
    T := A + ... + Const1 + ...
    U := T * Const2
  Сделать:
    T := A + ... (без Const1)
    W := T * Const2
    U := W + Const1 * Const2
*)

PROCEDURE TryMoveAddition (a, m: TriadePtr): BOOLEAN;
VAR i, j: INT;
       r: VALUE;
       q: TriadePtr;
BEGIN
    IF (m^.Op <> ir.o_mul) OR
       (ir.Vars^[a^.Name].Use^.next <> NIL) OR
       (m^.Options * a^.Options * ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} <> ir.OptionsSet{}) OR
       (ir.ConstTags [m^.ResType] <> ir.y_NumConst) OR
       (ir.ConstTags [a^.ResType] <> ir.y_NumConst) OR
       (a^.ResSize <> m^.ResSize) OR
       (LEN (m^.Params^) <> 2) OR
       MustPreserve (a)
    THEN
        RETURN FALSE;
    END;
    IF m^.Params^[0].tag = ir.y_NumConst THEN
        j := 0;
    ELSIF m^.Params^[1].tag = ir.y_NumConst THEN
        j := 1;
    ELSE
        RETURN FALSE;
    END;
    i := 0;
    LOOP
        IF a^.Params^[i].tag = ir.y_NumConst THEN
            EXIT;
        END;
        INC (i);
        IF i = LEN (a^.Params^) THEN
            RETURN FALSE;
        END;
    END;
    r := Calc.Binary (pc.sb_mul, m^.ResType, m^.ResSize,
                      a^.Params^[i].value, m^.Params^[j].value);
    Changed := TRUE;
    IF LEN (a^.Params^) = 1 THEN
        q := ir.NewTriadeLike (m, 1);
        gr.PutAfterTriade (q, m);
        ir.SetDef (m^.Name, q);
        ir.GenVar (ir.TEMPORARY, m^.Name, m);
        ir.MakeParNum_Ex (q^.Params^[0], ir.y_NumConst, r, a^.Params^[0].reverse);
        IF a^.Params^[0].reverse THEN
            q^.Op := ir.o_add;
        ELSE
            q^.Op := ir.o_assign;
        END;
    ELSE
        q := ir.NewTriadeLike (m, 2);
        q^.Op := ir.o_add;
        gr.PutAfterTriade (q, m);
        ir.SetDef (m^.Name, q);
        ir.GenVar (ir.TEMPORARY, m^.Name, m);
        ir.MakeParVar (q^.Params^[0], m^.Name);
        ir.MakeParNum_Ex (q^.Params^[1], ir.y_NumConst, r, a^.Params^[i].reverse);
        SYSTEM.EVAL(gr.DeleteParameter (a, i));
    END;
    RETURN TRUE;
END TryMoveAddition;

(*----------------------------------------------------------------------------*)

(*
  Можно ли умножить два параметра? Если да, то умножить; результат записать в
                                   par, а было ли переполнение - в Calc.overflow
*)

PROCEDURE TryMult (p, q: ParamPtr; t: TypeType; s: SizeType): BOOLEAN;
BEGIN
    IF (p^.tag <> ir.ConstTags [t]) OR (q^.tag <> ir.ConstTags [t]) THEN
        RETURN FALSE;
    END;
    ir.MakeParNum_Ex(par, ir.ConstTags [t],
                     Calc.Binary (pc.sb_mul, t, s, p^.value, q^.value),
                     FALSE);
    RETURN TRUE;
END TryMult;

(*----------------------------------------------------------------------------*)

(*
  Заменить умножение на константу на сдвиги, сложения и "хорошие" умножения
*)

PROCEDURE ReplaceByShifts (p: TriadePtr; i: INT): TriadePtr;
VAR c: SYSTEM.CARD32;
    r: TriadePtr;

    PROCEDURE MultByConst (v: VarNum; t: LONGINT);
    BEGIN
        r := ir.NewTriadeLike (p, 2);
        ir.GenVar (ir.TEMPORARY, r^.Name, r);
        gr.InsertTriade (r, p);
        ir.MakeParVar (r^.Params^[0], v);
<* IF TARGET_68k THEN *>
        IF t = 2 THEN
            r^.Op := ir.o_shl;
            ir.MakeParNum (r^.Params^[1], Calc.GetInteger (1, p^.ResSize));
            RETURN;
        ELSIF t = 4 THEN
            r^.Op := ir.o_shl;
            ir.MakeParNum (r^.Params^[1], Calc.GetInteger (2, p^.ResSize));
            RETURN;
        ELSIF t = 8 THEN
            r^.Op := ir.o_shl;
            ir.MakeParNum (r^.Params^[1], Calc.GetInteger (3, p^.ResSize));
            RETURN;
        END;
<* END *>
        ir.MakeParNum (r^.Params^[1], Calc.GetInteger (t, p^.ResSize));
    END MultByConst;

    PROCEDURE MultStep (v: VarNum; c: SYSTEM.CARD32);
    VAR r: TriadePtr;
        o: BOOLEAN;
        n: INT;
    BEGIN
<* IF ~TARGET_RISC & ~ TARGET_SPARC THEN *>
        REPEAT
            IF (c <= 5) OR (c = 8) OR (c = 9) THEN
                MultByConst (v, c);
                RETURN;
            ELSIF c MOD 9 = 0 THEN
                MultByConst (v, 9);
                c := c DIV 9;
                v := p^.Prev^.Name;
            ELSIF c MOD 3 = 0 THEN
                MultByConst (v, 3);
                c := c DIV 3;
                v := p^.Prev^.Name;
            ELSIF c MOD 5 = 0 THEN
                MultByConst (v, 5);
                c := c DIV 5;
                v := p^.Prev^.Name;
            ELSE
<* END *>
                o := ODD (c);
                n := 0;
                REPEAT
                    INC (n);
                    c := c DIV 2;
                UNTIL ODD (c);
                IF c <> 1 THEN
                    MultStep (v, c);
                END;
                r     := ir.NewTriadeLike (p, 2);
                r^.Op := ir.o_shl;
                r^.OpType := ir.t_unsign;
                (* opType - for r.Params[1], and
                   resType - for r.Params[0]
                *)
                r^.OpSize := 1;
                ir.GenVar (ir.TEMPORARY, r^.Name, r);
                gr.InsertTriade (r, p);
                IF c = 1 THEN
                    ir.MakeParVar (r^.Params^[0], v);
                ELSE
                    ir.MakeParVar (r^.Params^[0], r^.Prev^.Name);
                END;
                ir.MakeParNum (r^.Params^[1], Calc.GetInteger (n, r^.OpSize));
                IF o THEN
                    r     := ir.NewTriadeLike (p, 2);
                    r^.Op := ir.o_add;
                    ir.GenVar (ir.TEMPORARY, r^.Name, r);
                    gr.InsertTriade (r, p);
                    ir.MakeParVar (r^.Params^[0], r^.Prev^.Name);
                    ir.MakeParVar (r^.Params^[1], v);
                END;
<* IF ~ TARGET_RISC & ~ TARGET_SPARC THEN *>
                RETURN;
            END;
        UNTIL c = 1;
<* END *>
    END MultStep;

BEGIN
    IF LEN (p^.Params^) > 2 THEN
        r := ir.NewTriadeLike (p, 2);
        ir.SetDef (p^.Name, r);
        ir.GenVar (ir.TEMPORARY, p^.Name, p);
        ir.MakeParVar (r^.Params^[0], p^.Name);
        ir.CopyParamWithRev (p^.Params^[i], r^.Params^[1]);
        gr.PutAfterTriade (r, gr.DeleteParameter (p, i));
        p := r;
        i := 1;
    END;
    c := Calc.ToCardinal (p^.Params^[i].value, p^.ResSize);

<* IF TARGET_68k THEN *>
    IF (c = 3) OR (c = 5) OR (c = 9) OR
       (p^.Params^[1-i].tag <> ir.y_Variable)
    THEN
        RETURN p^.Next;
    END;
<* ELSE *>
    IF (c <= 5) OR (c = 8) OR (c = 9) OR
       (p^.Params^[1-i].tag <> ir.y_Variable)
    THEN
        RETURN p^.Next;
    END;
<* END *>
    MultStep (p^.Params^[1-i].name, c);
    r := p^.Prev;
    ir.RemoveVar (r^.Name);
    ir.SetDef    (p^.Name, r);
    RETURN gr.KillTriade_Ex (p);
END ReplaceByShifts;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать умножение
*)

PROCEDURE OptimizeMultiplication (p: TriadePtr): TriadePtr;
VAR    i: INT;
       s: TriadePtr;
    q, o: ParamPtr;
BEGIN
    IF MustPreserve (p) THEN
        RETURN p^.Next;
    END;
(*
  Удалить умножения на 0 и 1
*)
    o := anz.Ones [p^.ResType][p.ResSize];
    FOR i:=0 TO LEN(p^.Params^)-1 DO
        IF ir.EqParams (p^.Params^[i], o, p^.ResType, p^.ResSize) THEN
            Changed := TRUE;
            RETURN gr.DeleteParameter (p, i);
        ELSIF IsZero (p^.Params^[i], p^.ResType, p^.ResSize) THEN
            s := p^.Next;
            gr.ReplaceByParam (p, p^.Params^[i]);
            Changed := TRUE;
            RETURN s;
        END;
    END;

    IF ir.IsFloat [p^.ResType] OR
       (ir.OptionsSet{ ir.o_Dangerous, ir.o_Checked } * p^.Options <> ir.OptionsSet{})
    THEN
(*
  Опасная операция - можно умножать только 2 первых параметра
*)
        IF (LEN (p^.Params^) >= 2) THEN
            IF TryMult (p^.Params^[0], p^.Params^[1],
               p^.ResType, p^.ResSize)
            THEN
                IF Calc.overflow THEN
                    MakeOverflow (p);
                    RETURN NIL;
                ELSE
                    Changed := TRUE;
                    RETURN Replace2Pars (p, par, 0, 1);
                END;
(*
            ELSIF IsFloat [p^.ResType] THEN
                IF (p^.Params^[0].tag = ir.y_RealConst) &
                   Calc.CompareWithReal (pc.sb_equ, p^.Params^[0].value,
                                         2.0, p^.ResSize)
                THEN
                    p^.Op := ir.o_add;
                    ir.CopyParam (p^.Params^[1], p^.Params^[0]);
                    Changed := TRUE;
                    RETURN p;
                ELSIF (p^.Params^[1].tag = ir.y_RealConst) &
                   Calc.CompareWithReal (pc.sb_equ, p^.Params^[1].value,
                                         2.0, p^.ResSize)
                THEN
                    p^.Op := ir.o_add;
                    ir.CopyParam (p^.Params^[0], p^.Params^[1]);
                    Changed := TRUE;
                    RETURN p;
                END;
*)
            END;
        END;
    ELSE
(*
  Безопасная операция - можно умножать 2 любых параметра
*)
        q := NIL;
        FOR i:=0 TO LEN(p^.Params^)-1 DO
            IF p^.Params^[i].tag = ir.ConstTags [p^.ResType] THEN
                IF q = NIL THEN
                    q := p^.Params^[i];
                ELSIF TryMult (q, p^.Params^[i], p^.ResType, p^.ResSize) THEN
                    Changed := TRUE;
                    RETURN Replace2Pars (p, par, q^.paramnumber, i);
                END;
            END;
        END;
    END;
(*
  Попробовать заменить умножение на сложения и сдвиги - МАШИННОЗАВИСИМО!
*)
    IF MachineLevelOptimizations & NOT ir.IsFloat [p^.ResType]  &
       (ir.OptionsSet{ ir.o_Dangerous, ir.o_Checked } * p^.Options = ir.OptionsSet{})
<* IF TARGET_68k THEN *>
       & (opAttrs.CPU <> opAttrs.mc68060)
<* ELSIF NOT TARGET_VAX THEN *>
       & (opAttrs.CPU <> opAttrs.iPentiumPro)
<* END *>
    THEN
        FOR i:=0 TO LEN(p^.Params^)-1 DO
            IF (p^.Params^[i].tag = ir.y_NumConst) &
               opTune.IDB.NeedAdd (p^.Params^[i].value, p^.ResType, p^.ResSize)
            THEN
                RETURN ReplaceByShifts (p, i);
            END;
        END;
    END;
(*
  Ничего не получилось
*)
    RETURN p^.Next;
END OptimizeMultiplication;

(*----------------------------------------------------------------------------*)

(*
  Для деления на степень 2: делимое += sgnext (делимое) & (делитель - 1)
*)

PROCEDURE MakeAndWithSgnExt (p: TriadePtr; l: INT);
VAR
  ext, and, add: TriadePtr;
BEGIN
    and     := NIL;
    ext     := ir.NewTriadeLike (p, 1);
    ext^.Op := ir.o_sgnext;
    ir.CopyParam (p^.Params^[0], ext^.Params^[0]);
    ir.GenVar (ir.TEMPORARY, ext^.Name, ext);
    gr.InsertTriade (ext, p);
    IF l <> 1 THEN
        and     := ir.NewTriadeLike (p, 2);
        and^.Op := ir.o_and;
        ir.MakeParVar (and^.Params^[0], ext^.Name);
        ir.MakeParNum (and^.Params^[1],
                       Calc.Binary (pc.sb_minus,
                                    ir.t_int, p^.ResSize,
                                    p^.Params^[1].value,
                                    anz.Ones [ir.t_int][p.ResSize].value));
        ir.GenVar (ir.TEMPORARY, and^.Name, and);
        gr.InsertTriade (and, p);
    END;
    add     := ir.NewTriadeLike (p, 2);
    add^.Op := ir.o_add;
    ir.CopyParam (p^.Params^[0], add^.Params^[0]);
    IF l = 1 THEN
        ir.MakeParVar_Ex (add^.Params^[1], ext^.Name, TRUE, TRUE);
    ELSE
        ir.MakeParVar (add^.Params^[1], and^.Name);
    END;
    ir.GenVar (ir.TEMPORARY, add^.Name, add);
    gr.InsertTriade (add, p);
    IF p^.Params^[0].tag = ir.y_Variable THEN
        ir.RemoveUse(p^.Params^[0]);
    END;
    ir.MakeParVar (p^.Params^[0], add^.Name);
END MakeAndWithSgnExt;

(*----------------------------------------------------------------------------*)

<* IF NOT (TARGET_VAX OR TARGET_68k OR TARGET_RISC OR TARGET_SPARC) THEN *>

(*
  Следующие процедуры занимаются оптимизацией деления
*)

TYPE
    int   = opCard64.int;
    word  = opCard64.word;
    dword = opCard64.dword;

VAR
    m: dword;
    sh_pre, sh_post: int;

PROCEDURE choose (d: word; n, p: int): int;
VAR lo, lo1, hi1: dword;
    l:            int;
BEGIN
    l := VAL(int, opCard64.log2 (d));
    sh_post := l;
    lo := opCard64.add (opCard64.pow2 (n),
                        opCard64.div (opCard64.lshift_n (opCard64.sub (opCard64.pow2 (l),
                                                                       dword {d, 0}),
                                                         n),
                                      d));
          (* div (pow2 (n + l), d) - regrouped to avoid overflow *);
    m  := opCard64.add (opCard64.pow2 (l),
                        opCard64.div (opCard64.lshift_n (
                                         opCard64.sub (opCard64.add (opCard64.pow2 (n),
                                                                     opCard64.pow2 (n - p)),
                                                       dword {d, 0}),
                                         l),
                                      d));
          (* div (add (pow2 (n + l), pow2 (n + l - p)), d) - regrouped, too *)
    WHILE sh_post > 0 DO
        lo1 := opCard64.u_rshift (lo);
        hi1 := opCard64.u_rshift (m);
        IF opCard64.ge (lo1, hi1) THEN
            RETURN l;
        END;
        lo := lo1;
        m  := hi1;
        DEC (sh_post);
    END;
    RETURN l;
END choose;

PROCEDURE OpV (p: TriadePtr; VAR v: VarNum; op: ir.Operation; opd: VarNum);
VAR q: TriadePtr;
BEGIN
    q := ir.NewTriadeTInit (1, op, ir.y_Variable, p^.ResType, p^.ResSize);
    q^.Position := p^.Position;
    ir.MakeParVar (q^.Params^[0], opd);
    ir.GenVar (ir.TEMPORARY, q^.Name, q);
    gr.InsertTriade (q, p);
    v := q^.Name;
    IF op = ir.o_neg THEN
        q^.Op := ir.o_add;
        ir.SetParamReverse(q^.Params^[0], TRUE);
    END;
END OpV;

PROCEDURE OpVV (p: TriadePtr; VAR v: VarNum; op: ir.Operation; op1, op2: VarNum);
VAR q: TriadePtr;
BEGIN
    q := ir.NewTriadeTInit (2, op, ir.y_Variable, p^.ResType, p^.ResSize);
    q^.Position := p^.Position;
    ir.MakeParVar (q^.Params^[0], op1);
    ir.MakeParVar (q^.Params^[1], op2);
    ir.GenVar (ir.TEMPORARY, q^.Name, q);
    gr.InsertTriade (q, p);
    v := q^.Name;
    IF op = ir.o_sub THEN
        q^.Op := ir.o_add;
        ir.SetParamReverse(q^.Params^[1], TRUE);
    END;
END OpVV;

PROCEDURE OpVI (p: TriadePtr; VAR v: VarNum; op: ir.Operation;
                op1: VarNum; op2: word);
VAR q: TriadePtr;
BEGIN
    q := ir.NewTriadeTInit (2, op, ir.y_Variable, p^.ResType, p^.ResSize);
    q^.Position := p^.Position;
    ir.MakeParVar (q^.Params^[0], op1);
    IF p^.ResType = ir.t_int THEN
        ir.MakeParNum (q^.Params^[1],
                       Calc.NewInteger (SYSTEM.VAL (int, op2),
                                       p^.ResSize));
    ELSE
        ir.MakeParNum (q^.Params^[1], Calc.NewCardinal (op2, p^.ResSize));
    END;
    ir.GenVar (ir.TEMPORARY, q^.Name, q);
    gr.InsertTriade (q, p);
    v := q^.Name;
    IF (op = ir.o_sar) OR (op = ir.o_shr) OR (op = ir.o_shl) THEN
        q^.OpType := ir.t_int;
        q^.OpSize := opTune.addr_sz;
    ELSIF op = ir.o_sub THEN
        q^.Op := ir.o_add;
        ir.SetParamReverse(q^.Params^[1], TRUE);
    END;
END OpVI;

PROCEDURE UDiv (p: TriadePtr; _v: VarNum; d: word; n: int);
VAR
    t: word;
    _t, _u, _r, _s, _q: VarNum;
BEGIN
    IF opCard64.is_pow2 (d) THEN
        OpVI (p, _q, ir.o_shr, _v, opCard64.log2 (d));
        RETURN;
    END;
    SYSTEM.EVAL(choose (d, n, n));
    sh_pre := 0;
    IF NOT ODD (d) & opCard64.ge (m, opCard64.pow2 (n)) THEN
        REPEAT
            INC (sh_pre);
            d := d DIV 2;
        UNTIL ODD (d);
        SYSTEM.EVAL(choose (d, n, n - sh_pre));
    END;
    IF opCard64.ge (m, opCard64.pow2 (n)) THEN
        t := opCard64.lo (opCard64.sub (m, opCard64.pow2 (n)));
        OpVI (p, _t, ir.o_mulh, _v, t);
        OpVV (p, _u, ir.o_sub,  _v, _t);
        OpVI (p, _r, ir.o_shr,  _u, 1);
        OpVV (p, _s, ir.o_add,  _t, _r);
        OpVI (p, _q, ir.o_shr,  _s, VAL(word,sh_post - 1));
    ELSE
        t := m.lo;
        IF sh_pre <> 0 THEN
            OpVI (p, _u, ir.o_shr,  _v, VAL(word,sh_pre));
            OpVI (p, _r, ir.o_mulh, _u, t);
        ELSE
            OpVI (p, _r, ir.o_mulh, _v, t);
        END;
        OpVI (p, _r, ir.o_shr, _r, VAL(word,sh_post));
    END;
END UDiv;

PROCEDURE SDiv0 (p: TriadePtr; _v: VarNum; d: word; n: int);
VAR l: int;
    t: word;
    _t, _u, _r, _s, _q: VarNum;
BEGIN
    l := choose (opCard64.labs (d), n, n - 1);
    IF opCard64.labs (d) = 1 THEN
        OpV (p, _q, ir.o_assign, _v);
    ELSIF opCard64.is_pow2 (opCard64.labs (d)) THEN
        OpV (p, _t, ir.o_sgnext, _v);
        IF opCard64.labs (d) = 2 THEN
            OpVV (p, _r, ir.o_sub, _v, _t);
        ELSE
            OpVI (p, _u, ir.o_and, _t, opCard64.labs (d) - 1);
            OpVV (p, _r, ir.o_add, _v, _u);
        END;
        OpVI (p, _q, ir.o_sar, _r, VAL(word,l));
    ELSIF NOT opCard64.ge (m, opCard64.pow2 (n - 1)) THEN
        t := m.lo;
        OpVI (p, _r, ir.o_mulh,   _v, t);
        OpVI (p, _t, ir.o_sar,    _r, VAL(word,sh_post));
        OpV  (p, _u, ir.o_sgnext, _v);
        OpVV (p, _q, ir.o_sub,    _t, _u);
    ELSE
        t := opCard64.neg (opCard64.lo (opCard64.sub (opCard64.pow2 (n), m)));
        OpVI (p, _r, ir.o_mulh,   _v, t);
        OpVV (p, _s, ir.o_add,    _r, _v);
        OpVI (p, _t, ir.o_sar,    _s, VAL(word,sh_post));
        OpV  (p, _u, ir.o_sgnext, _v);
        OpVV (p, _q, ir.o_sub,    _t, _u);
    END;
    IF SYSTEM.VAL (int, d) < 0 THEN
        OpV  (p, _v, ir.o_neg, _q);
    END;
END SDiv0;

PROCEDURE SDiv (p: TriadePtr; _v: VarNum; d: word; n: int);
VAR
  t: word;
  _t, _u, _r, _s, _q: VarNum;
BEGIN
    IF SYSTEM.VAL (int, d) < 0 THEN
        RETURN;
    END;
    IF opCard64.is_pow2 (d) THEN
        OpVI (p, _q, ir.o_sar, _v, opCard64.log2 (d));
    ELSE
        SYSTEM.EVAL(choose (d, n, n - 1));
        p^.ResType := ir.t_unsign;
        t := m.lo;
        OpV  (p, _t, ir.o_sgnext, _v);
        OpVV (p, _r, ir.o_xor,    _t, _v);
        OpVI (p, _u, ir.o_mulh,   _r, t);
        OpVI (p, _s, ir.o_shr,    _u, VAL(word,sh_post));
        OpVV (p, _q, ir.o_xor,    _s, _t);
    END;
END SDiv;

<* END *> -- if not vax

PROCEDURE MayDivByConst (p: ParamPtr; val: ir.VALUE): BOOLEAN;
VAR q: TriadePtr;
  rem: ir.VALUE;
    i: INT;
BEGIN
    IF p^.tag = ir.y_NumConst THEN
        q := p^.triade;
        rem := Calc.Binary(pc.sb_rem, q.OpType, q.OpSize, p.value, val);
        IF rem.is_zero() THEN
            RETURN TRUE
        END;
    ELSIF p^.tag = ir.y_Variable THEN
        q := ir.Vars^[p^.name].Def;
        IF q.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous} = ir.OptionsSet{} THEN
            IF q.Op = ir.o_mul THEN
                FOR i := 0 TO LEN(q.Params^)-1 DO
                    IF MayDivByConst(q.Params[i], val) THEN
                        RETURN TRUE
                    END;
                END;
            ELSIF q.Op = ir.o_add THEN
                FOR i := 0 TO LEN(q.Params^)-1 DO
                    IF NOT MayDivByConst(q.Params[i], val) THEN
                        RETURN FALSE
                    END;
                END;
                RETURN TRUE
            END;
        END;
    END;
    RETURN FALSE
END MayDivByConst;

PROCEDURE DivByConst (op: pc.SUB_MODE; p: ParamPtr; val: ir.VALUE);
VAR q, r: TriadePtr;
  v: ir.VarNum;
  quo: ir.VALUE;
  done: BOOLEAN;
  i: INT;
BEGIN
    IF p^.tag = ir.y_NumConst THEN
        q := p^.triade;
        quo := Calc.Binary(op, q.OpType, q.OpSize, p.value, val);
        ir.MakeParNum_Ex(p, ir.y_NumConst, quo, p.reverse);
    ELSIF p^.tag = ir.y_Variable THEN
        v := p^.name;
        q := ir.Vars^[v].Def;
        r := ir.NewTriadeLike (q, LEN(q.Params^));
        gr.InsertTriade (r, q);
        IF q.Op = ir.o_mul THEN
            done := FALSE;
            FOR i := 0 TO LEN(q.Params^)-1 DO
                ir.CopyParam (q^.Params^[i], r^.Params^[i]);
                IF (NOT done) & MayDivByConst(q.Params[i], val) THEN
                    DivByConst(op, r^.Params^[i], val);
                    done := TRUE;
                END;
            END;
        ELSIF q.Op = ir.o_add THEN
            FOR i := 0 TO LEN(q.Params^)-1 DO
                ir.CopyParamWithRev (q^.Params^[i], r^.Params^[i]);
                DivByConst(op, r^.Params^[i], val);
            END;
        ELSE ASSERT(FALSE);
        END;
        ir.GenVar (ir.TEMPORARY, r^.Name, r);
        ir.RemoveUse(p);
        IF ir.FirstUse(v) = NIL THEN
          ir.RemoveVar(v);
          gr.KillTriade(q);
        END;
        ir.MakeParVar_Ex(p, r.Name, TRUE, p.reverse);
    END;
END DivByConst;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать деление
*)

PROCEDURE OptimizeDivision (p: TriadePtr; op: pc.SUB_MODE): TriadePtr;

  PROCEDURE IsCheck_NotZero(tr: TriadePtr;    par: ParamPtr;
                          type: ir.TypeType; size: ir.SizeType): BOOLEAN;
  BEGIN
    IF tr = NIL THEN RETURN FALSE; END;

    IF (tr.Op = ir.o_checknil) AND
       ir.EqParams(tr.Params[0], par, type, size)
    THEN
      RETURN TRUE;
    END;

    IF (tr.Op = ir.o_checkhi) AND
       ir.EqParams(tr.Params[1], par, type, size) AND
       (tr.Params[0].tag = ir.ConstTags[type]) AND
       Calc.IsZero(tr.Params[0].value, type, size)
    THEN
      RETURN TRUE;
    END;

    RETURN FALSE;
  END IsCheck_NotZero;

VAR s: TriadePtr;
    l: INT;
   tr: TriadePtr;
BEGIN
    s := p^.Next;
    IF MustPreserve (p) THEN
        RETURN s;
    END;
(*
  Удалить x/1
*)
    IF ir.EqParams (p^.Params^[1], anz.Ones [p^.ResType][p.ResSize], p^.ResType, p^.ResSize) THEN
        gr.ReplaceByParam (p, p^.Params^[0]);
        Changed := TRUE;
        RETURN s;
    END;


    IF IsZero (p^.Params^[0], p^.OpType, p^.OpSize) THEN
        -- this is a workaround. need to create 'checkneq' triade
        -- and correctly generate it in the back-end
        RETURN s;
(*
 *       -- replace division with the following code:
 *       -- was:
 *       --   c := 0 div a;
 *        -- replace with:
 *        --   checknil(a) +o_Division
 *        -- replace all usages of c by '0'
 *        tr := p.Prev;
 *        IF NOT IsCheck_NotZero(tr, p.Params[1], p.OpType, p.OpSize) THEN
 *          tr := ir.NewTriadeInit( 2, ir.o_checkneq, p.OpType, p.OpSize );
 *          INCL(tr.Options, ir.o_Division);
 *          INCL(tr.Options, ir.o_Silent);
 *          ir.CopyParam( p.Params[1], tr.Params[0] );
 *         ir.CopyParam( anz.Zeroes[p.OpType][p.OpSize], tr.Params[1]);
 *         gr.InsertTriade(tr, p); -- insert tr before p
 *       END;
 *       gr.ReplaceByParam(p, p.Params[0]);
 *       Changed := TRUE;
 *       RETURN s;
 *)
    END;

(*
  Пытаемся снять признак checked и dangerous
*)
    IF (p^.ResType = ir.t_unsign) OR
        (p^.ResType = ir.t_int) &
        (anz.AlwaysGtr (p^.Params^[0], anz.Zeroes [ir.t_int][p.ResSize],
                             ir.t_int, p^.ResSize, p) OR
         anz.AlwaysGtr (p^.Params^[1], anz.Zeroes [ir.t_int][p.ResSize],
                             ir.t_int, p^.ResSize, p))
    THEN
        EXCL (p^.Options, ir.o_Checked);
    END;
    IF ((p^.ResType = ir.t_unsign) OR (p^.ResType = ir.t_int)) &
       anz.AlwaysNeq (p^.Params^[1], anz.Zeroes [p^.ResType][p.ResSize],
                           p^.ResType, p^.ResSize, p)
    THEN
        EXCL (p^.Options, ir.o_Dangerous);
    END;
    IF p^.Params^[1].tag = ir.ConstTags [p^.ResType] THEN
(*
  А не делим ли мы на 0?
*)
        IF Calc.IsZero (p^.Params^[1].value, p^.ResType, p^.ResSize) THEN
            IF p^.ResType = ir.t_float THEN
                MakeError (p, opTune.realDivException);
                Changed := TRUE;
                RETURN NIL;
            ELSE
                MakeError (p, opTune.wholeDivException);
                Changed := TRUE;
                RETURN NIL;
            END;
        END;
(*
  Попытаемся константно поделить
*)
        IF p^.Params^[0].tag = ir.ConstTags [p^.ResType]
        THEN
            ir.MakeParNum_Ex(par, ir.ConstTags [p^.ResType],
                             Calc.Binary (op, p^.ResType, p^.ResSize,
                                          p^.Params^[0].value, p^.Params^[1].value),
                             FALSE);
            IF Calc.overflow THEN
                IF p^.ResType = ir.t_float THEN
                    MakeError (p, opTune.realDivException);
                ELSE
                    MakeError (p, opTune.wholeDivException);
                END;
                RETURN NIL;
            ELSE
                gr.ReplaceByParam (p, par);
                Changed := TRUE;
            END;
(*
  Пытаемся заменить деление на константу на умножение/сдвиг
*)
        ELSIF NOT ir.IsFloat [p^.ResType] THEN

(* -->>> LAZ  replace ((a*const+b*const+offs) DIV const) by  (a+b+offs DIV const) *)

           IF (p.Options * ir.OptionsSet{ir.o_Checked, ir.o_Dangerous } = ir.OptionsSet{}) &
               (p^.Params^[1].tag = ir.y_NumConst) &
               MayDivByConst (p^.Params^[0], p^.Params^[1].value)
           THEN
               DivByConst(op, p^.Params^[0], p^.Params^[1].value);
               gr.ReplaceByParam (p, p^.Params^[0]);
               Changed := TRUE;
               RETURN s
           END;

(* ---<<< LAZ  *)

<* IF NOT (TARGET_VAX OR TARGET_68k OR TARGET_RISC OR TARGET_SPARC) THEN *>
            IF NOT (opAttrs.SPACE IN opAttrs.COMP_MODE) THEN
                IF (p.OpSize = 8) &
                   ((p^.OpType = ir.t_int)OR(p^.OpType = ir.t_unsign))
                THEN
                    RETURN s;
                ELSIF (p^.ResType <> ir.t_int) OR
                   anz.AlwaysGeq (p^.Params^[0], anz.Zeroes [ir.t_int][p.ResSize],
                                       p^.ResType, p^.ResSize, p) &
                   anz.AlwaysGeq (p^.Params^[1], anz.Zeroes [ir.t_int][p.ResSize],
                                       p^.ResType, p^.ResSize, p)
                THEN
                    ASSERT (p^.Params^[0].tag = ir.y_Variable);
                    p^.ResType := ir.t_unsign;

                    UDiv (p, p^.Params^[0].name,
                          SYSTEM.VAL (word,
                                      Calc.ToCardinal (p^.Params^[1].value,
                                                       p^.ResSize)),
                          VAL(int,p^.ResSize) * 8);
                ELSIF p^.Op = ir.o_div THEN
                    IF NOT anz.AlwaysGeq (p^.Params^[1], anz.Zeroes [ir.t_int][p.ResSize],
                                               p^.ResType, p^.ResSize, p)
                    THEN
                        RETURN s;
                    END;
                    ASSERT (p^.Params^[0].tag = ir.y_Variable);

                    SDiv (p, p^.Params^[0].name,
                          SYSTEM.VAL (word,
                                      Calc.ToInteger (p^.Params^[1].value,
                                                      p^.ResSize)),
                          VAL(int, p^.ResSize) * 8);
                ELSE
                    ASSERT (p^.Params^[0].tag = ir.y_Variable);
                    SDiv0 (p, p^.Params^[0].name,
                           SYSTEM.VAL (word,
                                       Calc.ToInteger (p^.Params^[1].value,
                                                       p^.ResSize)),
                           VAL(int, p^.ResSize) * 8);
                    IF (Calc.ToInteger (p^.Params^[1].value, p^.ResSize) = -1) &
                       (ir.o_Checked IN p^.Options)
                    THEN
                        INCL (p^.Prev^.Options, ir.o_Checked);
                    END;
                END;
                Changed := TRUE;
                s := p^.Prev;
                ir.RemoveVar (s^.Name);
                ir.SetDef    (p^.Name, s);
                RETURN gr.KillTriade_Ex (p);
            END;
<* END *> -- IF not VAX

            l := Calc.IsPowerOfTwo (p^.Params^[1].value,
                                    p^.ResType, p^.ResSize);
            IF l <> -1 THEN
                p^.Options := p^.Options - ir.OptionsSet{ ir.o_Checked, ir.o_Dangerous };
                IF p^.ResType <> ir.t_int THEN
                    p^.Op := ir.o_shr;
                ELSIF p^.Op = ir.o_div THEN
                    p^.Op := ir.o_sar;
                ELSIF anz.AlwaysGeq (p^.Params^[0], anz.Zeroes [ir.t_int][p.ResSize],
                                          p^.ResType, p^.ResSize, p)
                THEN
                    p^.Op := ir.o_shr;
                ELSE
                    MakeAndWithSgnExt (p, l);
                    p^.Op := ir.o_sar;
                END;
                ir.MakeParNum(p.Params[1], Calc.GetValue (l, p.ResType, p^.ResSize) );
                Changed := TRUE;
            END;
        END;
    ELSIF ir.EqParams (p^.Params^[0], p^.Params^[1],
                       p^.ResType, p^.ResSize)
    THEN
        gr.ReplaceByParam (p, anz.Ones [p^.ResType][p.ResSize]);
        Changed := TRUE;
        RETURN s;
    END;
    RETURN s;
END OptimizeDivision;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать остаток
*)

(* what we have:
    b := OP (a, c);

   what we do:

    c2 := c - 1; (constant evaluation)
    if OP is 'mod' or (a always >= 0) then
      -- we needn't think about negative values of a
      b := a and c2;
    else
      foo1 := sgnext(a);
      foo2 := a xor foo1;
      foo3 := foo2 sub foo1
      foo4 := foo3 and c2
      foo5 := foo4 xor foo1;
      b := foo5 sub foo1;
    end;
*)
PROCEDURE TryModByPowerOf2(p: TriadePtr): BOOLEAN;
VAR needSgnExt: BOOLEAN;
    sgnext, xor, sub, sub2, xor2: TriadePtr;
    c2: ir.VALUE;
BEGIN
    IF (*MachineLevelOptimizations &*)
       NOT ir.IsFloat [p^.ResType] AND
       (Calc.IsPowerOfTwo (p^.Params^[1].value, p^.ResType, p^.ResSize) <> -1)
    THEN
        needSgnExt := ~ ((p^.Op = ir.o_mod) OR
                    anz.AlwaysGeq (p^.Params^[0], anz.Zeroes [ir.t_int][p.ResSize],
                                        p^.ResType, p^.ResSize, p));
        IF needSgnExt THEN
            sgnext := ir.NewTriadeLike(p, 1);
            sgnext.Op := ir.o_sgnext;
            ir.CopyParamWithRev(p.Params[0], sgnext.Params[0]);
            ir.GenResVar(sgnext);
            sgnext.Options := p^.Options - ir.OptionsSet{ ir.o_Checked, ir.o_Dangerous };
            gr.InsertTriade(sgnext, p);

            xor := ir.NewTriadeLike(sgnext, 2);
            xor.Op := ir.o_xor;
            ir.CopyParamWithRev(p.Params[0], xor.Params[0]);
            ir.MakeParVar(xor.Params[1], sgnext.Name);
            ir.GenResVar(xor);
            gr.PutAfterTriade(xor, sgnext);

            sub := ir.NewTriadeLike(xor, 2);
            sub.Op := ir.o_sub;
            ir.MakeParVar(sub.Params[0], xor.Name);
            ir.MakeParVar(sub.Params[1], sgnext.Name);
            ir.GenResVar(sub);
            gr.PutAfterTriade(sub, xor);

            ir.RemoveUse(p.Params[0]);
            ir.MakeParVar(p.Params[0], sub.Name);
        END;

        p^.Op := ir.o_and;
        p^.Options := p^.Options - ir.OptionsSet{ ir.o_Checked, ir.o_Dangerous };

        c2 := Calc.Binary (pc.sb_minus,
                           p^.ResType, p^.ResSize,
                           p^.Params^[1].value,
                           anz.Ones[ir.t_int][p.ResSize].value);
        ir.MakeParNum( p^.Params^[1], c2);

        IF needSgnExt THEN
            xor2 := ir.NewTriadeLike(xor, 2);
            xor2.Op := ir.o_xor;
            -- 0th parameter of xor2 is result of 'p' - see below
            ir.MakeParVar(xor2.Params[1], sgnext.Name);
            ir.GenResVar(xor2);
            gr.PutAfterTriade(xor2, p);

            sub2 := ir.NewTriadeLike(xor, 2);
            sub2.Op := ir.o_sub;
            ir.MakeParVar(sub2.Params[0], xor2.Name);
            ir.MakeParVar(sub2.Params[1], sgnext.Name);
            gr.PutAfterTriade(sub2, xor2);

            ir.SetDef(p.Name, sub2);
            ir.GenResVar(p);
            ir.MakeParVar(xor2.Params[0], p.Name);
        END;
--        anz.WriteTest("TryModByPowerOf2", " (q)");
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END;
END TryModByPowerOf2;
(*
            IF ((p^.Op = ir.o_mod) OR
                anz.AlwaysGeq (p^.Params^[0], anz.Zeroes [ir.t_int][p.ResSize],
                                    p^.ResType, p^.ResSize, p)) &
               (Calc.IsPowerOfTwo (p^.Params^[1].value, p^.ResType, p^.ResSize)
                <> -1)
            THEN
                p^.Op := ir.o_and;
                p^.Options := p^.Options - ir.OptionsSet{ ir.o_Checked, ir.o_Dangerous };
                ir.MakeParNum( p^.Params^[1],  Calc.Binary (pc.sb_minus,
                                                    p^.ResType, p^.ResSize,
                                                    p^.Params^[1].value,
                                                    anz.Ones[ir.t_int][p.ResSize].value)
                             );
                Changed := TRUE;
            END;
*)

PROCEDURE OptimizeModulo (p: TriadePtr; op: pc.SUB_MODE): TriadePtr;
VAR s: TriadePtr;

BEGIN
    s := p^.Next;
    IF MustPreserve (p) THEN
        RETURN s;
    END;
(*
  Удалить x MOD 1 и 0 MOD x
*)
    IF (p.ResType IN ir.WholeTypes) AND
       ir.EqParams (p^.Params^[1], anz.Ones[p^.ResType][p.ResSize], p^.ResType, p^.ResSize)
    OR
       IsZero (p^.Params^[0], p^.ResType, p^.ResSize)
    THEN
        gr.ReplaceByParam (p, anz.Zeroes [p^.ResType][p.ResSize]);
        Changed := TRUE;
        RETURN s;
    END;
    IF p^.Params^[1].tag = ir.ConstTags [p^.ResType] THEN
(*
  Попытаемся константно поделить
*)
        IF p^.Params^[0].tag = ir.ConstTags [p^.ResType] THEN
            ir.MakeParNum_Ex(par, ir.ConstTags [p^.ResType],
                             Calc.Binary (op, p^.ResType, p^.ResSize,
                                          p^.Params^[0].value, p^.Params^[1].value),
                             FALSE);
            IF Calc.overflow THEN
                MakeError (p, opTune.wholeDivException);
                RETURN NIL;
            ELSE
                gr.ReplaceByParam (p, par);
                Changed := TRUE;
            END;
(*
  Пытаемся заменить деление на степень двойки на AND
*)
        ELSIF TryModByPowerOf2(p) THEN
              Changed := TRUE;
              RETURN s;

        END;
    ELSIF ir.EqParams (p^.Params^[0], p^.Params^[1],
                       p^.ResType, p^.ResSize)
    THEN

        gr.ReplaceByParam (p, anz.Zeroes [p^.ResType][p.ResSize]);
        Changed := TRUE;
        RETURN s;
    END;
    RETURN s;
END OptimizeModulo;

(*----------------------------------------------------------------------------*)

(*
  Проследить, куда ведет дуга
*)

PROCEDURE TraceArc (a: Arc): Node;
VAR n: Node;
BEGIN
    LOOP
        n := gr.Arcs^[a].t;
        IF (ir.Nodes^[n].NIn <> 1) OR (ir.Nodes^[n].First^.Op <> ir.o_goto) THEN
            RETURN n;
        END;
        a := ir.Nodes^[n].OutArcs^[0];
    END;
END TraceArc;

(*----------------------------------------------------------------------------*)

(*
  Выставить признак silent
*)

PROCEDURE TraceArcAndSetSilent (a: Arc);
VAR n: Node;
BEGIN
    LOOP
        n := gr.Arcs^[a].t;
        IF (ir.Nodes^[n].NIn <> 1) OR (ir.Nodes^[n].First^.Op <> ir.o_goto) THEN
            RETURN;
        END;
        INCL (ir.Nodes^[n].First^.Options, ir.o_Silent);
        a := ir.Nodes^[n].OutArcs^[0];
    END;
END TraceArcAndSetSilent;

(*----------------------------------------------------------------------------*)

(*
  Попытаться заменить
    IF cond THEN RETURN 1 ELSE RETURN 0
  на
    RETURN cond
*)

PROCEDURE TryReplaceReturns (p: TriadePtr): BOOLEAN;
VAR n, n1, n2: Node;
    r, s, p1, p2: TriadePtr;
BEGIN
<* IF TARGET_VAX OR TARGET_RISC THEN *>
    RETURN FALSE;
<* END *>
    IF p^.ResType = ir.t_float THEN
        RETURN FALSE;
    END;
    n := p^.NodeNo;
    n1 := TraceArc (ir.Nodes^[n].OutArcs^[0]);
    IF (ir.Nodes^[n1].NIn <> 1) OR (ir.Nodes^[n1].First^.Op <> ir.o_ret) THEN
        RETURN FALSE;
    END;
    p1 := ir.Nodes^[n1].First;
    n2 := TraceArc (ir.Nodes^[n].OutArcs^[1]);
    IF (ir.Nodes^[n2].NIn <> 1) OR (ir.Nodes^[n2].First^.Op <> ir.o_ret) THEN
        RETURN FALSE;
    END;
    p2 := ir.Nodes^[n2].First;
    IF (p1^.ResSize <> p2^.ResSize) OR
       (p1^.ResType <> p2^.ResType) OR
       (p1^.Params = NIL) OR (p2^.Params = NIL) OR
       (p1^.Params^[0].tag <> ir.y_NumConst) OR
       (p2^.Params^[0].tag <> ir.y_NumConst)
<* IF~ TARGET_SPARC THEN *>
       OR
       NOT (Calc.IsZero (p1^.Params^[0].value, p1^.ResType, p1^.ResSize)
<* IF ~ TARGET_68k THEN *>
            & Calc.CompareWithInt (pc.sb_equ, p2^.Params^[0].value, 1,
                                   p2^.ResType, p2^.ResSize)
<* END *>
            OR Calc.IsZero (p2^.Params^[0].value, p2^.ResType, p2^.ResSize)
<* IF ~ TARGET_68k THEN *>
            & Calc.CompareWithInt (pc.sb_equ, p1^.Params^[0].value, 1,
                                   p1^.ResType, p1^.ResSize)
<* END *>
       )
<* END *>
    THEN
        RETURN FALSE;
    END;
    IF p^.Op = ir.o_eq THEN
        s := ir.NewTriadeTInit (4, ir.o_move_eq, ir.y_Variable,
                                p^.ResType, p^.ResSize);
    ELSE (* t^.Op = ir.o_le *)
        s := ir.NewTriadeTInit (4, ir.o_move_le, ir.y_Variable,
                                p^.ResType, p^.ResSize);
    END;
    s^.ResType  := p1^.ResType;
    s^.ResSize  := p1^.ResSize;
    s^.Position := p^.Position;
    ir.CopyParam (p^.Params^[0], s^.Params^[0]);
    ir.CopyParam (p^.Params^[1], s^.Params^[1]);
    ir.CopyParam (p1^.Params^[0], s^.Params^[2]);
    ir.CopyParam (p2^.Params^[0], s^.Params^[3]);
    ir.GenVar (ir.TEMPORARY, s^.Name, s);
    gr.InsertTriade (s, p);
    r := ir.NewTriadeInit (1, ir.o_ret, p1^.ResType, p1^.ResSize);
    r^.Position := p^.Position;
    ir.MakeParVar (r^.Params^[0], s^.Name);
    gr.InsertTriade (r, p);
    gr.KillTriade(p);
    INCL (p1^.Options, ir.o_Silent);
    INCL (p2^.Options, ir.o_Silent);
    TraceArcAndSetSilent (ir.Nodes^[n].OutArcs^[0]);
    TraceArcAndSetSilent (ir.Nodes^[n].OutArcs^[1]);
    gr.KillArc (ir.Nodes^[n].OutArcs^[1]);
    gr.KillArc (ir.Nodes^[n].OutArcs^[0]);
    ChangedGraph := TRUE;
    RETURN TRUE;
END TryReplaceReturns;

(*----------------------------------------------------------------------------*)

(*
  Попытаться заменить
    IF 1 <= (int) b THEN arc1 ELSE arc2
  на
    IF b <= 0 THEN arc2 ELSE arc1,

  а
    IF 1 <= (unsign) b THEN arc1 ELSE arc2
  на
    IF b = 0 THEN arc2 ELSE arc1,
*)

PROCEDURE TryCompareWith1 (p: TriadePtr) : BOOLEAN;
BEGIN
    IF (p^.ResType IN ir.TypeTypeSet{ ir.t_int, ir.t_unsign }) &
       (p^.Params^[0].tag = ir.y_NumConst) &
       ir.EqParams (p^.Params^[0],
                    anz.Ones[p^.ResType][p.ResSize],
                    p^.ResType, p^.ResSize)
    THEN
        ir.SwapParams (p^.Params^[0], p^.Params^[1]);
        IF p^.Op = ir.o_le THEN
            gr.ExchangeArcs (p^.NodeNo);
        ELSE
            ir.SwapParams (p^.Params^[2], p^.Params^[3]);
        END;
        IF p^.ResType <> ir.t_int THEN
            IF p^.Op = ir.o_le THEN
                p^.Op := ir.o_eq;
            ELSE
                p^.Op := ir.o_move_eq;
            END;
        END;
        ir.CopyParam (anz.Zeroes [p^.OpType][p.OpSize], p^.Params^[1]);
        Changed := TRUE;
        RETURN TRUE;
    END;
    RETURN FALSE;
END TryCompareWith1;

(*----------------------------------------------------------------------------*)

(*
  Если оба операнда сравнения получены с помощью одинаковых o_val, убрать o_val
*)

PROCEDURE TryRemoveVals (p: TriadePtr): BOOLEAN;
VAR
  p1, p2: TriadePtr;
BEGIN
    IF (p^.Params^[0].tag <> ir.y_Variable) OR
       (p^.Params^[1].tag <> ir.y_Variable)
    THEN
        RETURN FALSE;
    END;
    p1 := ir.Vars^[p^.Params^[0].name].Def;
    p2 := ir.Vars^[p^.Params^[1].name].Def;
    IF (p1^.Op <> ir.o_val) OR (p2^.Op <> ir.o_val) OR
       (p1^.ResType <> p2^.ResType) OR (p1^.ResSize <> p2^.ResSize) OR
       (p1^.OpType <> p2^.OpType) OR (p1^.OpSize <> p2^.OpSize) OR
       (p1^.ResType = ir.t_float) OR (p1^.OpType = ir.t_float) OR
       (p1^.ResSize < p1^.OpSize) OR
       (p1^.ResType <> p1^.OpType)
    THEN
        RETURN FALSE;
    END;
    ir.RemoveUse(p^.Params^[0]);
    ir.CopyParam (p1^.Params^[0], p^.Params^[0]);
    ir.RemoveUse(p^.Params^[1]);
    ir.CopyParam (p2^.Params^[0], p^.Params^[1]);
    p^.ResType := p1^.OpType;
    p^.OpType  := p1^.OpType;
    p^.ResSize := p1^.OpSize;
    p^.OpSize  := p1^.OpSize;
    Changed := TRUE;
    RETURN TRUE;
END TryRemoveVals;


(*----------------------------------------------------------------------------*)

PROCEDURE ExpandFi(tr: ir.TriadePtr; pos: INT);
VAR i : INT;
    params: ir.ParamArray;
BEGIN
  params := ir.NewParams(LEN(tr.Params^)+1, tr);
  FOR i := 0 TO LEN(tr.Params^)-1 DO
    ir.MoveParam(tr.Params[i], params[i]);
  END;

  ir.CopyParamWithRev( params[pos], params[ LEN(tr.Params^) ]);
  tr.Params := params;
END ExpandFi;

PROCEDURE ExpandFies(n: Node; pos: INT);
VAR tr: ir.TriadePtr;
BEGIN
  tr := ir.Nodes[n].First;
  WHILE( (tr # NIL) & (tr.Op = ir.o_fi)) DO
    ExpandFi(tr, pos);
    tr := tr.Next;
  END;
END ExpandFies;

PROCEDURE ReplaceLe64(p: ir.TriadePtr);
VAR h1, h2 : ir.VarNum;
    le_l, eq  : ir.TriadePtr;
    type   : ir.TypeType;
    size   : ir.SizeType;
    nd2, nd3,
    true, false : ir.Node;
BEGIN
    type := p.OpType;
    size := p.OpSize;

    true  := ir.Nodes[p.NodeNo].Out[0];
    false := ir.Nodes[p.NodeNo].Out[1];

    h1 := MakeHiwordBeforeTr(p.Params[0], type, size, p);
    h2 := MakeHiwordBeforeTr(p.Params[1], type, size, p);

    -- was: if a1 le a2 then
    --        goto true;
    --      else
    --        goto false;

    -- become:
    --      if hi(a1) le hi(a2) then         (1)
    --        if hi(a1) eq hi(a2) then       (2) nd2
    --          if lo(a1) le lo(a2) then     (3) nd3
    --            goto true;
    --          else
    --            goto false;
    --        else
    --          goto true;
    --      else
    --        goto false;
    --

    -- we don't modify parameters of p here,
    -- because we'll need them further.
    -- we will change them in the very end.

    -- (2)
    nd2 := gr.SplitArc( ir.Nodes[p.NodeNo].OutArcs[0] );
    -- SplitArc() makes goto here, we don't need it.
    gr.KillTriade(ir.Nodes[nd2].First);
    eq  := ir.NewTriadeTInit( 2, ir.o_eq, ir.y_Nothing,
                              type, 4 );
    ir.MakeParVar(eq.Params[0], h1);
    ir.MakeParVar(eq.Params[1], h2);
    gr.PutTriadeLast(eq, nd2);

    gr.NewArc(nd2, false, FALSE);
    ExpandFies(false,
               gr.FindInArc( ir.Nodes[p.NodeNo].OutArcs[1] )
              );
    gr.ExchangeArcs(nd2);
    -- now nd2 has two outgoint arcs, first to FALSE, second to TRUE

    -- (3)
    nd3 := gr.SplitArc( ir.Nodes[nd2].OutArcs[0] );

    -- note: we create le_l before o_val but insert it after.
    le_l := ir.NewTriadeTInit( 2, ir.o_le, ir.y_Nothing,
                              ir.t_unsign, 4 );

    MakeValBeforeTr(p.Params[0],
                    p.OpType, p.OpSize,
                    ir.t_unsign, 4,
                    ir.Nodes[nd3].First,
                    le_l.Params[0]);

    MakeValBeforeTr(p.Params[1],
                    p.OpType, p.OpSize,
                    ir.t_unsign, 4,
                    ir.Nodes[nd3].First,
                    le_l.Params[1]);

    -- SplitArc() makes goto here, we don't need it further.
    gr.KillTriade(ir.Nodes[nd3].Last);

    gr.PutTriadeLast(le_l, nd3);
    gr.NewArc(nd3, true, FALSE);
    ExpandFies(true,
               gr.FindInArc( ir.Nodes[nd2].OutArcs[1] )
              );
    gr.ExchangeArcs(nd3);
    -- now nd3 has two outgoing arcs: first to true, second to false

    -- (1)
    p.OpSize  := 4;
    p.ResSize := 4;
    ir.MakeParVar(p.Params[0], h1);
    ir.MakeParVar(p.Params[1], h2);
END ReplaceLe64;

(*
  Оптимизировать "<="
*)

-- looks for template like this:
--   op (a, b) then goto A else goto A;
-- and replaces it with goto A
-- pre-condition: A must contain no fi-functions
PROCEDURE TryRemoveCmpAsSenseless(p: TriadePtr): BOOLEAN;
VAR toNode: ir.Node;
BEGIN
  toNode := ir.Nodes[p.NodeNo].Out[0];
  IF (toNode = ir.Nodes[p.NodeNo].Out[1]) AND
     (ir.Nodes[toNode].First.Op # ir.o_fi)
  THEN
    ReplaceByGoto(p.NodeNo, 0, TRUE);
    Changed := TRUE;
    ChangedGraph := TRUE;
    RETURN TRUE;
  END;
  RETURN FALSE;
END TryRemoveCmpAsSenseless;

PROCEDURE OptimizeLe (p: TriadePtr): TriadePtr;
BEGIN
    IF NOT MustPreserve (p) THEN
        IF TryRemoveCmpAsSenseless(p) THEN
          RETURN NIL;
        ELSIF anz.AlwaysLeq (p^.Params^[0], p^.Params^[1],
                               p^.ResType, p^.ResSize, p)
        THEN
            ReplaceByGoto (p^.NodeNo, 0, TRUE);
            RETURN NIL;
        ELSIF anz.AlwaysGtr (p^.Params^[0], p^.Params^[1],
                                  p^.ResType, p^.ResSize, p)
        THEN
            ReplaceByGoto (p^.NodeNo, 1, TRUE);
            RETURN NIL;
        ELSIF TryRemoveVals (p) THEN
            RETURN NIL;
        ELSIF TryReplaceReturns (p) THEN
            RETURN NIL;
        ELSIF TryCompareWith1 (p) THEN
            RETURN NIL;
        END;
    END;
    -- we must replace o_le with number of triades comparing
    -- high and low parts of the values
    IF MachineLevelOptimizations &
       (p.OpSize = 8) &
       (p.OpType IN ir.TypeTypeSet{ ir.t_int, ir.t_unsign })
    THEN
      ReplaceLe64(p);
      ChangedGraph := TRUE;
      Changed := TRUE;
    END;
    RETURN NIL;
END OptimizeLe;

(*----------------------------------------------------------------------------*)

PROCEDURE WarningRedundantCondition (p: TriadePtr);
BEGIN
    IF NOT (ir.o_Silent IN p^.Options) & NOT p^.Position.IsNull () THEN
        env.errors.Warning (p^.Position, 902);
    END;
    Changed := TRUE;
END WarningRedundantCondition;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать "<="
*)

PROCEDURE OptimizeMoveLe (p: TriadePtr): TriadePtr;
VAR
  q, s: TriadePtr;
   use: ParamPtr;
 x_pos: LONGINT;
BEGIN
    s := p^.Next;
    IF NOT MustPreserve (p) THEN
      IF anz.AlwaysLeq (p^.Params^[0], p^.Params^[1],
                             p^.OpType, p^.OpSize, p)
      THEN
        WarningRedundantCondition (p);
        gr.ReplaceByParam (p, p^.Params^[2]);
      ELSIF anz.AlwaysGtr (p^.Params^[0], p^.Params^[1],
                                p^.OpType, p^.OpSize, p)
      THEN
        WarningRedundantCondition (p);
        gr.ReplaceByParam (p, p^.Params^[3]);
      ELSIF TryCompareWith1 (p) THEN
        ;
      ELSE
        use := ir.Vars[p.Name].Use;
        q   := use.triade;
        -- if the only usage of the result is o_eq,
        -- than we can optimze it.
        IF (use.next = NIL) AND (q.Op = ir.o_eq) THEN
          IF q.Params[0] = use THEN x_pos := 1;ELSE x_pos := 0;END;

          IF ir.EqParams(p.Params[2], q.Params[x_pos], q.OpType, q.OpSize)
          THEN
            --   c := (a<=b) ? t : y;
            --   if c = t then ...
            -- convert it to:
            --   if a <= b then ...
          ELSIF ir.EqParams(p.Params[3], q.Params[x_pos], q.OpType, q.OpSize)
          THEN
            --   c := (a<=b) ? t : y;
            --   if c = y then ...
            -- convert it to:
            --   if NOT(a <= b) then ...
            -- (changing o_eq to o_ne is done via arcs swapping)
            gr.ExchangeArcs(q.NodeNo);
          ELSE
             RETURN s;
          END;
          ir.KillParams(q);
          q.Op := ir.o_le;
          ir.SetParamTriade(q.Params[0], q);
          ir.SetParamTriade(q.Params[1], q);
          q.OpType  := p.OpType;
          q.OpSize  := p.OpSize;
          -- it's not a misprint - i'm sure OpType must be used
          -- because ResType is a nonsence for o_le
          q.ResType := p.OpType;
          q.ResSize := p.OpSize;
          ir.CopyParam(p.Params[0], q.Params[0]);
          ir.CopyParam(p.Params[1], q.Params[1]);
          ir.RemoveVar(p.Name);
          gr.KillTriade(p);
          Changed := TRUE;
          RETURN NIL;
        END;
      END;
    END;
    RETURN s;
END OptimizeMoveLe;

(*----------------------------------------------------------------------------*)

PROCEDURE DominatesLoadOrStore (p: TriadePtr; v: VarNum): BOOLEAN;
VAR r: ParamPtr;
BEGIN
    r := ir.Vars^[v].Use;
    REPEAT
        IF ((r^.triade^.Op = ir.o_loadr) OR
            (r^.triade^.Op = ir.o_storer) & (r^.paramnumber = 0)) &
           gr.DominatesTriadeApprox (r^.triade, p)
        THEN
            RETURN TRUE;
        END;
        r := r^.next;
    UNTIL r = NIL;
    RETURN FALSE;
END DominatesLoadOrStore;

(*----------------------------------------------------------------------------*)

PROCEDURE TryDeleteReverse(par: ir.ParamPtr;
                           type: ir.TypeType;
                           size: ir.SizeType
                          ): BOOLEAN;
BEGIN
  IF NOT par.reverse THEN
     RETURN TRUE;
  END;
  IF par.tag IN ir.NumericTags THEN
    ir.MakeParNum_Ex(par,
                     par.tag,
                     Calc.Unary(pc.su_neg,
                                type,
                                size,
                                par.value),
                     FALSE);
   RETURN TRUE;
 END;
 RETURN FALSE;
END TryDeleteReverse;

(*
  Оптимизировать "="
*)

PROCEDURE ConstantParams(p: ir.TriadePtr): BOOLEAN;
VAR i : LONGINT;
BEGIN
  IF (p.Params = NIL) OR (LEN(p.Params^)=0) THEN
    RETURN TRUE;
  END;
  FOR i := 0 TO LEN(p.Params^)-1 DO
    IF NOT (p.Params[i].tag IN ir.TagTypeSet{ ir.y_NumConst, ir.y_AddrConst,
                                         ir.y_RealConst, ir.y_ComplexConst,
                                         ir.y_ProcConst })
    THEN
      RETURN FALSE;
    END;
  END;
  RETURN TRUE;
END ConstantParams;

PROCEDURE FindInParams(par: ir.ParamPtr; tr: TriadePtr): LONGINT;
VAR i : LONGINT;
BEGIN
  IF (tr.Params = NIL) OR (LEN(tr.Params^)=0) THEN
    RETURN -1;
  END;
  FOR i := 0 TO LEN(tr.Params^)-1 DO
    IF ir.EqParams(par, tr.Params[i], tr.OpType, tr.OpSize) THEN
      RETURN i;
    END;
  END;
  RETURN -1;
END FindInParams;

(*
  Оптимизировать "="
*)


(* проводит дуги из входных узлов fi.Node в выходные
   по следующему правилу:
   if fi.Params[i] = par then
     arc(In[i], Out[0]);
   else
     arc(In[i], Out[1]);
   end;

   используется для оптимизации шаблона
   t1 = fi(const1, const2,....)
   eq( t1 == const3  )
 *)
PROCEDURE CloneEqs(fi: TriadePtr; par: ir.ParamPtr);
VAR j, k : LONGINT;
    n, m, l : ir.Node;
    a    : ir.Arc;
    from_n: LONGINT;
BEGIN
  n := fi.NodeNo;
  FOR j := 0 TO ir.Nodes[n].NIn-1 DO
    m := ir.Nodes[n].In[j]; -- откуда дуга
    a := ir.Nodes[n].InArcs[j]; -- какая дуга
    k := gr.FindOutArc( a );
    IF ir.EqParams(fi.Params[j], par, fi.OpType, fi.OpSize) THEN
      l := ir.Nodes[n].Out[0];
      from_n := gr.FindInArc( ir.Nodes[n].OutArcs[0] );
    ELSE
      l := ir.Nodes[n].Out[1];
      from_n := gr.FindInArc( ir.Nodes[n].OutArcs[1] );
    END;

    -- дугу "из m в n" поменять на дугу "из m в l"
    -- вставить ее в массив дуг в l,
    -- добавить m в массив нодов в l,
    -- раздвинуть fi-functions в l.
    -- возиться с нодой n не надо - мы ее счас убьем
    gr.Arcs[ a ].t := l;
    ir.Nodes[ m ].Out[ k ] := l;
    gr.AddNodeArc(ir.Nodes[l].InArcs, a,
                  ir.Nodes[l].In, m,
                  ir.Nodes[l].NIn);
    ExpandFies(l, from_n);
  END;
  ir.Nodes[n].NIn := 0;
  gr.MakeNodeSilent(n);
  gr.KillNode(n, TRUE);
--  anz.WriteTest("o", "Optimize.CloneEqs() finished (o)");
END CloneEqs;

PROCEDURE TryOptimizeEqWithConst(p: TriadePtr): BOOLEAN;
VAR t1, t2, c1, c2 : ParamPtr;
    a : TriadePtr;
    constTag    : ir.TagType;
    i : LONGINT;


  (* replace   t2 = t1 o_add c1;  -- triade 'a'
               o_eq( t2, c2 );    -- triade 'p'
     with

               o_eq( t1, W );
               where W = t1.reverse ? c1 add -c2 : c2 add -c1)
  *)
  PROCEDURE IncapsulateAddToEq(): BOOLEAN;
  VAR
    a_used_once : BOOLEAN;
  BEGIN
    IF t1.reverse THEN
      ir.SetParamReverse(c2, ~c2.reverse);
      ASSERT( TryAddConsts( c1, c2, constTag,
                            a.ResType, a.ResSize )
            );
      ir.SetParamReverse(c2, ~c2.reverse);
    ELSE
      ir.SetParamReverse(c1, ~c1.reverse);
      ASSERT( TryAddConsts( c1, c2, constTag,
                            a.ResType, a.ResSize )
            );
      ir.SetParamReverse(c1, ~c1.reverse);
    END;

    IF Calc.overflow THEN
      RETURN FALSE;
    END;

    ir.MoveParamWithRev(par, c2);

    a_used_once := (t2.next=NIL) & (t2.prev=NIL);
    ir.RemoveUse( t2 );

    IF a_used_once THEN
      ir.RemoveVar( t2.name );
    END;

    ir.CopyParamWithRev( t1, t2 ); -- t1 --> t2

    IF a_used_once THEN
      gr.KillTriade( a );
    END;

    ASSERT( TryDeleteReverse(c2, p.OpType, p.OpSize) );
    RETURN TRUE;
  END IncapsulateAddToEq;

BEGIN
  constTag := ir.ConstTags[p.OpType];

  IF p.Params[0].tag = constTag THEN
    c2 := p.Params[0];
    t2 := p.Params[1];
  ELSIF p.Params[1].tag = constTag THEN
    c2 := p.Params[1];
    t2 := p.Params[0];
  ELSE
    RETURN FALSE;
  END;

  IF t2.tag # ir.y_Variable THEN
    RETURN FALSE;
  END;

  a := ir.Vars[t2.name].Def;
  IF (a.Op=ir.o_add) AND (LEN(a.Params^)=2) THEN
    IF a.Params[0].tag = constTag THEN
      c1 := a.Params[0];
      t1 := a.Params[1];
    ELSIF a.Params[1].tag = constTag THEN
      c1 := a.Params[1];
      t1 := a.Params[0];
    ELSE
      RETURN FALSE;
    END;

    IF IncapsulateAddToEq() THEN
      Changed := TRUE;
      RETURN TRUE;
    END;
  ELSIF (a.Op = ir.o_fi) AND ConstantParams(a) THEN
    i := FindInParams(c2, a);
    IF i = -1 THEN
      ReplaceByGoto(p.NodeNo, 1, TRUE);
      Changed := TRUE;
      ChangedGraph := TRUE;
      RETURN TRUE;
    ELSIF (a.Prev = NIL ) AND (a.Next = p) AND -- only two triades in node
          (t2.next = NIL) AND (t2.prev = NIL) -- and the only usage of variable
    THEN
      CloneEqs(a, c2);
      Changed := TRUE;
      ChangedGraph := TRUE;
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END TryOptimizeEqWithConst;

PROCEDURE ReplaceEq64(p : TriadePtr);
VAR
  h1, h2 : ir.VarNum;
  eq_l   : ir.TriadePtr;
  type   : ir.TypeType;
  size   : ir.SizeType;
  nd     : ir.Node;
BEGIN
  type := p.OpType;
  size := p.OpSize;

  -- was:
  --   if a eq b then
  --
  -- become:
  --   if hi(a) eq hi(b) then    (1)
  --     if lo(a) eq lo(b) then  (2) nd
  --       goto TRUE;
  --     else
  --       goto FALSE;
  --   else
  --     goto TRUE;

  h1 := MakeHiwordBeforeTr(p.Params[0],
                           type, size,
                           p);

  h2 := MakeHiwordBeforeTr(p.Params[1],
                           type, size,
                           p);
  -- we don't modify p's parameters here, because we use them further.
  -- we will modify them in the very end.

  -- (2)
  nd := gr.SplitArc( ir.Nodes[p.NodeNo].OutArcs[0] );

  -- note: we create o_eq before o_val, but insert it after
  eq_l := ir.NewTriadeTInit( 2, ir.o_eq, ir.y_Nothing,
                             ir.t_unsign, 4 );

  MakeValBeforeTr( p.Params[0],
                   p.OpType, p.OpSize,
                   ir.t_unsign, 4,
                   ir.Nodes[nd].First,
                   eq_l.Params[0] );

  MakeValBeforeTr( p.Params[1],
                   p.OpType, p.OpSize,
                   ir.t_unsign, 4,
                   ir.Nodes[nd].First,
                   eq_l.Params[1] );

  -- SplitArc() creates o_goto, we don't need it
  gr.KillTriade(ir.Nodes[nd].Last);

  gr.PutTriadeLast(eq_l, nd);

  gr.NewArc(nd, ir.Nodes[p.NodeNo].Out[1], FALSE);
  ExpandFies(ir.Nodes[p.NodeNo].Out[1],
             gr.FindInArc( ir.Nodes[p.NodeNo].OutArcs[1] )
            );

  -- (1)
  p.OpSize  := 4;
  p.ResSize := 4;
  ir.MakeParVar(p.Params[0], h1);
  ir.MakeParVar(p.Params[1], h2);
END ReplaceEq64;

PROCEDURE OptimizeEq (p: TriadePtr): TriadePtr;

  PROCEDURE Replace(old, new: ir.ParamPtr);
  BEGIN
      IF old.tag = ir.y_Variable THEN
          ir.RemoveUse(old);
      END;
      ir.CopyParam(new, old); -- keeps old value of reverse
  END Replace;

  PROCEDURE TouchUsages(eq: TriadePtr): BOOLEAN;
  VAR var_par,
      cnst_par,
      use,
      next_use: ParamPtr;
      yes_arc: ir.Arc;
      touched: BOOLEAN;
      trd: TriadePtr;
      nd: ir.Node;
  BEGIN
    IF (eq.Params[0].tag = ir.y_Variable) AND
       (eq.Params[1].tag = ir.ConstTags [eq^.OpType])
    THEN
      var_par  := eq.Params[0];
      cnst_par := eq.Params[1];
    ELSIF (eq.Params[1].tag = ir.y_Variable) AND
          (eq.Params[0].tag = ir.ConstTags [eq^.OpType])
    THEN
      var_par  := eq.Params[1];
      cnst_par := eq.Params[0];
    ELSE
      RETURN FALSE;
    END;

    touched := FALSE;
    yes_arc := ir.Nodes[eq.NodeNo].OutArcs[0];

    use := ir.Vars[var_par.name].Use;
    WHILE use # NIL DO
      next_use := use.next;
      IF (use # var_par) THEN
        trd := use.triade;
        IF (trd.Op = ir.o_fi) THEN
          nd := ir.Nodes[ trd.NodeNo ].In[ use.paramnumber ];
        ELSE
          nd := trd.NodeNo;
        END;
        IF gr.aDominates(yes_arc, nd) THEN
          Replace(use, cnst_par);
          touched := TRUE;
        END;
      END;
      use := next_use;
    END;
    RETURN touched;
  END TouchUsages;

VAR is_trap: BOOLEAN;
BEGIN
    IF NOT MustPreserve (p) THEN
        IF TryRemoveCmpAsSenseless(p) THEN
          RETURN NIL;
        ELSIF anz.AlwaysEqu (p^.Params^[0], p^.Params^[1],
                               p^.ResType, p^.ResSize, p)
        THEN
            ReplaceByGoto (p^.NodeNo, 0, TRUE);
            RETURN NIL;
        ELSIF anz.AlwaysNeq (p^.Params^[0], p^.Params^[1],
                                  p^.ResType, p^.ResSize, p)

           OR ( (p^.Params^[1].tag = ir.y_AddrConst)
                OR
                ( (p^.Params^[1].tag = ir.y_Variable) &
                  DominatesLoadOrStore (p, p^.Params^[1].name)
                )
              )
              &
              EqNil (p^.Params^[0], p^.ResSize)

           OR ( (p^.Params^[1].tag = ir.y_AddrConst)
                OR
                ( (p^.Params^[0].tag = ir.y_Variable) &
                  DominatesLoadOrStore (p, p^.Params^[0].name)
                )
              )
              &
              EqNil (p^.Params^[1], p^.ResSize)
        THEN
            is_trap := ir.o_Trap IN p.Options;
            ReplaceByGoto (p^.NodeNo, 1, NOT is_trap);
            RETURN NIL;
        ELSIF TryReplaceReturns (p) THEN
            RETURN NIL;
        ELSIF TryOptimizeEqWithConst( p ) THEN
            RETURN NIL;
        END;

        IF TouchUsages(p) THEN -- attention! this is NOT MODIFICATION OF P
          Changed := TRUE;     -- but modification of usages of the variable
        END;                   -- that is involved into this comparison
    END;
    -- we must replace o_eq with number of triades comparing
    -- high and low parts of the values
    IF MachineLevelOptimizations &
       (p.OpSize = 8) &
       (p.OpType IN ir.TypeTypeSet{ ir.t_int, ir.t_unsign })
    THEN
      ReplaceEq64(p);
      ChangedGraph := TRUE;
      Changed := TRUE;
    END;
    RETURN NIL;
END OptimizeEq;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать "="
*)

PROCEDURE OptimizeMoveEq (p: TriadePtr): TriadePtr;


VAR
  q, s: TriadePtr;
   use: ParamPtr;
 x_pos: LONGINT;
BEGIN
    s := p^.Next;
    IF NOT MustPreserve (p) THEN
        IF anz.AlwaysEqu (p^.Params^[0], p^.Params^[1],
                               p^.OpType, p^.OpSize, p)
        THEN
            WarningRedundantCondition (p);
            gr.ReplaceByParam (p, p^.Params^[2]);
        ELSIF anz.AlwaysNeq (p^.Params^[0], p^.Params^[1],
                                  p^.OpType, p^.OpSize, p)
           OR (p^.Params^[1].tag = ir.y_AddrConst) &
              EqNil (p^.Params^[0], p^.OpSize)
           OR (p^.Params^[0].tag = ir.y_AddrConst) &
              EqNil (p^.Params^[1], p^.OpSize)
        THEN
            WarningRedundantCondition (p);
            gr.ReplaceByParam (p, p^.Params^[3]);
        ELSE
          use := ir.Vars[p.Name].Use;
          q   := use.triade;
          -- if the only usage of the result is o_eq,
          -- than we can optimze it.
          IF (use.next = NIL) AND (q.Op = ir.o_eq) THEN
            IF q.Params[0] = use THEN x_pos := 1;ELSE x_pos := 0;END;

            IF ir.EqParams(p.Params[2], q.Params[x_pos], q.OpType, q.OpSize)
            THEN
              --   c := (a=b) ? t : y;
              --   if c = t then ...
              -- convert it to:
              --   if a = b then ...
            ELSIF ir.EqParams(p.Params[3], q.Params[x_pos], q.OpType, q.OpSize)
            THEN
              --   c := (a=b) ? t : y;
              --   if c = y then ...
              -- convert it to:
              --   if a # b then ...
              -- (changing o_eq to o_ne is done via arcs swapping)
              gr.ExchangeArcs(q.NodeNo);
            ELSE
               RETURN s;
            END;
            ir.KillParams(q);
            ir.SetParamTriade(q.Params[0], q);
            ir.SetParamTriade(q.Params[1], q);
            q.OpType  := p.OpType;
            q.OpSize  := p.OpSize;
            -- it's not a misprint - i'm sure OpType must be used
            -- because ResType is a nonsence for o_le
            q.ResType := p.OpType;
            q.ResSize := p.OpSize;
            ir.CopyParam(p.Params[0], q.Params[0]);
            ir.CopyParam(p.Params[1], q.Params[1]);
            ir.RemoveVar(p.Name);
            gr.KillTriade(p);
            Changed := TRUE;
            RETURN NIL;
          END;
        END;
    END;
    RETURN s;
END OptimizeMoveEq;

(*----------------------------------------------------------------------------*)

(*
  Try optimize CASE statement
*)

PROCEDURE OptimizeCASE (p: TriadePtr): TriadePtr;

  PROCEDURE TryRemoveCaseAsSenseless(p: TriadePtr): BOOLEAN;
  VAR toNode: ir.Node;
      i : LONGINT;
  BEGIN
    toNode := ir.Nodes[p.NodeNo].Out[0];
    IF ir.Nodes[toNode].First.Op = ir.o_fi THEN
      RETURN FALSE;
    END;

    FOR i := 1 TO ir.Nodes[p.NodeNo].NOut-1 DO
      IF ir.Nodes[p.NodeNo].Out[i] # toNode THEN
        RETURN FALSE;
      END;
    END;

    ReplaceByGoto(p.NodeNo, 0, TRUE);
    Changed := TRUE;
    ChangedGraph := TRUE;
    RETURN TRUE;
  END TryRemoveCaseAsSenseless;

VAR i: INT;
BEGIN
    IF NOT MustPreserve (p) THEN
        FOR i:=1 TO LEN (p^.Params^)-1 BY 2 DO
            IF anz.AlwaysGeq (p^.Params^[0], p^.Params^[i],
                                   p^.ResType, p^.ResSize, p)
             & anz.AlwaysLeq (p^.Params^[0], p^.Params^[i+1],
                                   p^.ResType, p^.ResSize, p)
            THEN
                ReplaceByGoto (p^.NodeNo, i DIV 2, TRUE);
                RETURN NIL;
            END;
        END;
        IF anz.AlwaysLss (p^.Params^[0], p^.Params^[1],
                               p^.ResType, p^.ResSize, p)
        OR anz.AlwaysGtr (p^.Params^[0], p^.Params^[LEN(p^.Params^)-1],
                               p^.ResType, p^.ResSize, p)
        THEN
            ReplaceByGoto (p^.NodeNo, LEN (p^.Params^) DIV 2, TRUE);
            RETURN NIL;
        END;
        IF TryRemoveCaseAsSenseless(p) THEN
          RETURN NIL;
        END;
    END;
    RETURN NIL;
END OptimizeCASE;

(*----------------------------------------------------------------------------*)

(*
  Заменить последнюю триаду в узле на AND и сравнение с 0
*)

PROCEDURE AndCompare0 (p: TriadePtr; s: ParamPtr; u: VALUE): TriadePtr;
VAR q, r: TriadePtr;
BEGIN
    q := ir.NewTriadeTInit (2, ir.o_and, ir.y_Variable, p^.ResType, p^.ResSize);
    ir.GenVar (ir.TEMPORARY, q^.Name, q);
    q^.Position := p^.Position;
    ir.CopyParam  (s, q^.Params^[0]);
    ir.MakeParNum (q^.Params^[1], u);
    gr.InsertTriade (q, p);
    r := ir.NewTriadeTInit (2, ir.o_eq, ir.y_Nothing, p^.ResType, p^.ResSize);
    r^.Position := p^.Position;
    ir.MakeParVar (r^.Params^[0], q^.Name);
    ir.MakeParNum (r^.Params^[1], Calc.GetValue (0, p.ResType, p^.ResSize));
    gr.InsertTriade (r, p);
    RETURN q;
END AndCompare0;

(*----------------------------------------------------------------------------*)


PROCEDURE In (p: TriadePtr): BOOLEAN;
VAR i, j: INT;
BEGIN
    IF Calc.IsNegative (p^.Params^[0].value, opTune.index_sz) THEN
        RETURN FALSE;
    END;
    i := Calc.ToInteger (p^.Params^[0].value, opTune.index_sz);
    IF Calc.IsNegative (p^.Params^[1].value, p^.ResSize) THEN
        j := Calc.ToInteger (p^.Params^[1].value, p^.ResSize);
    ELSE
        j := SYSTEM.VAL (INT, Calc.ToCardinal (p^.Params^[1].value,p^.ResSize));
    END;
    RETURN i IN SYSTEM.VAL (SET, j);
END In;

(*----------------------------------------------------------------------------*)

(*
  Try optimize IF In
*)

PROCEDURE OptimizeIn (p: TriadePtr): TriadePtr;
VAR s: LONGINT;
<* IF NOT TARGET_68k THEN *>
    q: TriadePtr;
<* END *>
BEGIN
    IF MustPreserve (p) THEN
        RETURN NIL;
    END;
    IF p.ResType = ir.t_ref THEN
        RETURN NIL;
    END;
    IF TryRemoveCmpAsSenseless(p) THEN
        RETURN NIL;
    END;
    IF p^.Params^[0].tag = ir.y_NumConst THEN
        s := Calc.ToInteger (p^.Params^[0].value, opTune.index_sz);
        IF (s < 0) OR (s >= ORD(p^.ResSize) * 8) THEN
            MakeError (p, opTune.rangeException);
            RETURN NIL;
        END;
        IF p^.Params^[1].tag = ir.y_NumConst THEN
            ReplaceByGoto (p^.NodeNo, 1 - ORD (In (p)), TRUE);
            RETURN NIL;
        END;
        IF p.ResSize = 8 THEN
            RETURN NIL;
        END;
<* IF NOT TARGET_68k THEN *>
(*
    Заменить o_in на явно выписанные and и сравнение с 0
*)
        q := AndCompare0 (p, p^.Params^[1],
                          Calc.SetConstructor (s, s, p^.ResSize));
        gr.ExchangeArcs (p^.NodeNo);
        gr.KillTriade(p);
        RETURN q;
<* END *>
    END;
    RETURN NIL;
END OptimizeIn;

(*----------------------------------------------------------------------------*)

PROCEDURE LeSet (p: TriadePtr): BOOLEAN;
VAR s1, s2: SET;
    i:      ir.SizeType;
BEGIN
    IF p^.ResType = ir.t_int THEN
        s1 := SYSTEM.VAL (SET, Calc.ToInteger (p^.Params^[0].value,
                                               p^.ResSize));
        s2 := SYSTEM.VAL (SET, Calc.ToInteger (p^.Params^[1].value,
                                               p^.ResSize));
    ELSE
        s1 := SYSTEM.VAL (SET, Calc.ToCardinal (p^.Params^[0].value,
                                                p^.ResSize));
        s2 := SYSTEM.VAL (SET, Calc.ToCardinal (p^.Params^[1].value,
                                                p^.ResSize));
    END;
    FOR i:=0 TO p^.ResSize-1 DO
        IF (i IN s1) & NOT (i IN s2) THEN
            RETURN FALSE;
        END;
    END;
    RETURN TRUE;
END LeSet;

(*----------------------------------------------------------------------------*)

(*
  Try optimize IF set <= set
*)

PROCEDURE OptimizeLeSet (p: TriadePtr): TriadePtr;
VAR q, r: TriadePtr;
BEGIN
    IF NOT MustPreserve (p) &
       (p^.Params^[0].tag = ir.y_NumConst) &
       (p^.Params^[1].tag = ir.y_NumConst)
    THEN
        ReplaceByGoto (p^.NodeNo, 1 - ORD (LeSet (p)), TRUE);
    END;
    IF MachineLevelOptimizations THEN
        q := ir.NewTriadeTInit (2, ir.o_and, ir.y_Variable,
                                p^.ResType, p^.ResSize);
        ir.GenVar (ir.TEMPORARY, q^.Name, q);
        q^.Position := p^.Position;
        ir.CopyParam  (p^.Params^[0], q^.Params^[0]);
        ir.CopyParam  (p^.Params^[1], q^.Params^[1]);
        gr.InsertTriade (q, p);
        r := ir.NewTriadeTInit (2, ir.o_eq, ir.y_Nothing,
                                p^.ResType, p^.ResSize);
        r^.Position := p^.Position;
        ir.MakeParVar (r^.Params^[0], q^.Name);
        ir.CopyParam  (p^.Params^[0], r^.Params^[1]);
        gr.InsertTriade (r, p);
        gr.KillTriade(p);
    END;
    RETURN NIL;
END OptimizeLeSet;

(*----------------------------------------------------------------------------*)

PROCEDURE Odd (p: TriadePtr): INT;
BEGIN
    RETURN 1 - ORD( Calc.IsZero (Calc.Unary (pc.su_odd, p^.ResType,
                                            p^.ResSize, p^.Params^[0].value),
                                 p.ResType,
                                 p.ResSize
                                )
                  );
END Odd;

(*----------------------------------------------------------------------------*)

(*
  Try optimize IF Odd (x) THEN
*)

PROCEDURE OptimizeOdd (p: TriadePtr): TriadePtr;
<* IF ~TARGET_68k THEN *>
VAR q: TriadePtr;
<* END *>
BEGIN
    IF NOT MustPreserve (p) & (p^.Params^[0].tag = ir.ConstTags [p^.ResType]) THEN
        ReplaceByGoto (p^.NodeNo, 1 - Odd (p), TRUE);
        RETURN NIL;
    END;
<* IF TARGET_68k THEN *>
    RETURN p^.Next;
<* ELSE *>
(*
  Заменить o_odd на явно выписанные and и сравнение с 0
*)
    q := AndCompare0 (p, p^.Params^[0], Calc.GetValue (1, p.ResType, p^.ResSize));
    gr.ExchangeArcs (p^.NodeNo);
    gr.KillTriade(p);
    RETURN q;
<* END *>
END OptimizeOdd;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать начало цикла FOR
*)

PROCEDURE OptimizeForStart (p: TriadePtr): TriadePtr;
VAR n: Node;
BEGIN
    IF ir.Nodes^[p^.NodeNo].NOut = 2 THEN
        n := p^.NodeNo;

(*
  Рассмотрим 2 частных случая:
  - цикл гарантированно выполнится хоть раз - тогда выкинуть дугу ELSE
  - цикл гарантированно не выполнится не разу - заменить на GOTO
*)
        IF Calc.CompareWithInt (pc.sb_geq, p^.Params^[2].value, 0,
                                p^.ResType, p^.ResSize)
        THEN
            IF anz.AlwaysLeq (p^.Params^[0], p^.Params^[1],
                                   p^.ResType, p^.ResSize, p)
            THEN
                gr.KillArc (ir.Nodes^[n].OutArcs^[1]);
                ChangedGraph := TRUE;
            ELSIF anz.AlwaysGtr (p^.Params^[0], p^.Params^[1],
                                      p^.ResType, p^.ResSize, p)
            THEN
                ir.MakeParNothing(par);
                gr.ReplaceByParam (p, par);
                gr.MakeGoto (n);
                ReplaceByGoto (n, 1, TRUE);
            END;
        ELSE
            IF anz.AlwaysGeq (p^.Params^[0], p^.Params^[1],
                                    p^.ResType, p^.ResSize, p)
            THEN
                gr.KillArc (ir.Nodes^[n].OutArcs^[1]);
                ChangedGraph := TRUE;
            ELSIF anz.AlwaysLss (p^.Params^[0], p^.Params^[1],
                                      p^.ResType, p^.ResSize, p)
            THEN
                ir.MakeParNothing(par);
                IF NOT (ir.o_Silent IN p^.Options) &
                   NOT p^.Position.IsNull ()
                THEN
                    env.errors.Warning (p^.Position, 902);
                END;
                gr.ReplaceByParam (p, par);
                gr.MakeGoto (n);
                ReplaceByGoto (n, 1, TRUE);
            END;
        END;
    END;
    RETURN NIL;
END OptimizeForStart;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать конец цикла FOR - а конкретно, все использования переменной
                                   цикла после оного заменить на UNDEFINED
*)

PROCEDURE OptimizeForCont (p: TriadePtr): TriadePtr;
VAR q, s: ParamPtr;
BEGIN
    IF opAttrs.curr_mod.flag = pc.flag_java THEN RETURN NIL END;
    q := ir.Vars^[p^.Name].Use;
    WHILE q <> NIL DO
        IF q^.triade^.NodeNo = ir.Nodes^[p^.NodeNo].Out^[0] THEN
            q := q^.next;
        ELSE
            Changed := TRUE;
            s := ir.RemoveUse_Ex (q);
            ir.MakeParNothing(q);
            q := s;
        END;
    END;
    RETURN NIL;
END OptimizeForCont;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать store: выкинуть store R = UNDEFINED
                            (такое может возникнуть из-за цикла FOR)
                        попытаться выкинуть
                            load  t = R
                            store R = t
                        попытаться выкинуть
                            loadr t = * & R
                            store R = t
*)

PROCEDURE OptimizeStore (p: TriadePtr): TriadePtr;
VAR q: TriadePtr;
BEGIN
    CASE p^.Params^[0].tag OF
    | ir.y_Nothing:
        Changed := TRUE;
        RETURN gr.KillTriade_Ex (p);
    | ir.y_Variable:
        IF NOT (ir.o_Volatile IN p^.Options) &
           NOT (ir.o_Volatile IN ir.Locals^[p^.Name].Options)
        THEN
            q := ir.Vars^[p^.Params^[0].name].Def;
            IF NOT (ir.o_Volatile IN q^.Options) &
               ((
                 (q^.Op = ir.o_load)                 &
                 (q^.Params^[0].tag  = ir.y_RealVar) &
                 (q^.Params^[0].name = p^.Name)
                 OR
                 (q^.Op = ir.o_loadr)                    &
                 (q^.Params^[0].tag    = ir.y_AddrConst) &
                 (q^.Params^[0].name   = p^.Name)        &
                 (q^.Params^[0].offset = 0)
                ) & gr.Dominates (q^.NodeNo, p^.NodeNo)
                  & NobodyWrites (q, p, p^.Write)
                  & (p^.ResType = q^.ResType) & (p^.ResSize = q^.ResSize))
            THEN
                Changed := TRUE;
                RETURN gr.KillTriade_Ex (p);
            END;
        END;
    | ELSE
    END;
    RETURN p^.Next;
END OptimizeStore;

(*----------------------------------------------------------------------------*)

(*
  Оптимизировать storer: попытаться выкинуть
                            loadr t = * p
                            storer * p = t
                         попытаться выкинуть
                            load t = R
                            storer * & R = t
             а также
                * unreferenced_mem = t
*)

PROCEDURE OptimizeStorer (p: TriadePtr): TriadePtr;
VAR q: TriadePtr;
BEGIN
    IF NOT (ir.o_IsChecknil IN p.Options) AND
       NobodyReads (p, p^.Write) &
       NOT (ir.o_Volatile IN p^.Options) &
       ((p^.Params^[0].tag <> ir.y_AddrConst) OR
        NOT (ir.o_Volatile IN ir.Locals^[p^.Params^[0].name].Options))
    THEN
        Changed := TRUE;
        RETURN gr.KillTriade_Ex (p);
    ELSIF (p^.Params^[1].tag = ir.y_Variable) &
          NOT (ir.o_Volatile IN p^.Options)
    THEN
        q := ir.Vars^[p^.Params^[1].name].Def;
        IF ((q^.Op = ir.o_loadr) OR
            (q^.Op = ir.o_load) &
            NOT (ir.o_Volatile IN ir.Locals^[q^.Params^[0].name].Options))
           & NOT (ir.o_Volatile IN q^.Options)
           & EqLoadParams (p^.Params^[0], q^.Params^[0])
           & gr.Dominates (q^.NodeNo, p^.NodeNo)
           & NobodyWrites (q, p, p^.Write)
           & (p^.ResType = q^.ResType)
           & (p^.ResSize = q^.ResSize)
        THEN
            Changed := TRUE;
            IF (ir.o_IsChecknil IN p.Options) THEN
              INCL( q.Options, ir.o_IsChecknil );
            END;
            RETURN gr.KillTriade_Ex (p);
        END;
    END;
    RETURN p^.Next;
END OptimizeStorer;


(* if the nobody reads our first argument at all, or
   if the next usage is another o_clear triade -
   them remove p.
 *)
PROCEDURE OptimizeClear (p: TriadePtr): TriadePtr;
VAR q: TriadePtr;
BEGIN
  q := p.Next;
  IF p.Params[0].tag = ir.y_Variable THEN
    IF (p.Prev = NIL) AND (p.Next = NIL) THEN
     gr.KillTriade(p);
     Changed := TRUE;
    END;
  END;
  RETURN q;
END OptimizeClear;

PROCEDURE OptimizeHiWord (p: TriadePtr): TriadePtr;
VAR q: TriadePtr;
BEGIN
  q := p.Next;
  IF p.Params[0].tag = ir.y_NumConst THEN
    ir.MakeParNum(par, Calc.Unary(pc.su_hiword, p.ResType, p.ResSize,
                                  p.Params[0].value)
                  );
    gr.ReplaceByParam(p, par);
    Changed := TRUE;
  ELSE
    ir.MakeParNum( par, Calc.Val( Calc.MaxValue(ir.t_unsign, 4),
                                  ir.t_unsign, p^.OpType,
                                  4,           p^.OpSize
                                )
                 );
    IF anz.AlwaysLeq (p^.Params^[0], par,
                           p^.OpType, p^.OpSize, p)
    THEN
      ir.MakeParNum(par, Calc.NewValue(0, p.ResType, p.ResSize));
      gr.ReplaceByParam(p, par);
      Changed := TRUE;
    END;
  END;
  RETURN q;
END OptimizeHiWord;

(*----------------------------------------------------------------------------*)

-- ищет в фи-функциях узла n функцию с ResVAr = var
PROCEDURE FindInFies(n: ir.Node; var: ir.VarNum): ir.TriadePtr;
VAR p: ir.TriadePtr;
BEGIN
  p := ir.Nodes[n].First;
  WHILE p.Op = ir.o_fi DO
    IF p.Name = var THEN
      RETURN p;
    END;
    p := p.Next;
  END;
  RETURN NIL;
END FindInFies;

(* увеличивает кол-во параметров у fi
   и копирует туда либо fi.Params[j]
   либо параметры соотв. фи-функции из Node n
   если ее результат и есть fi.Params[j]
 *)
PROCEDURE CloneFi_1(fi: ir.TriadePtr; n: ir.Node; j: LONGINT);
VAR p: ir.ParamArray;
    i, new_NIn, old_NIn: LONGINT;
    fi_2: ir.TriadePtr;
BEGIN
  -- увеличиваем кол-во параметров у fi до nodes[l].NIn штук.
  new_NIn := ir.Nodes[fi.NodeNo].NIn;
  old_NIn := LEN(fi.Params^);
  p := ir.NewParams(new_NIn, fi);
  FOR i := 0 TO old_NIn-1 DO
    ir.CopyParam(fi.Params[i], p[i]);
  END;
  IF (fi.Params[j].tag = ir.y_Variable) THEN
    fi_2 := FindInFies(n, fi.Params[j].name);
    IF fi_2 # NIL THEN
      FOR i := old_NIn TO new_NIn-1 DO
        ir.CopyParam(fi_2.Params[i-old_NIn], p[i]);
      END;
      INCL(fi_2.Options, ir.o_Silent);
    ELSE
      FOR i := old_NIn TO new_NIn-1 DO
        ir.CopyParam(fi.Params[j], p[i]);
      END;
    END;
  ELSE
    FOR i := old_NIn TO new_NIn-1 DO
      ir.CopyParam(fi.Params[j], p[i]);
    END;
  END;
  ir.KillParams(fi);
  fi.Params := p;
END CloneFi_1;


(* переносит fi в ноду l если у fi.Name есть использования
   где-то еще, за исключением фи-функций ноды l.
 *)
PROCEDURE MoveFi_1(fi: ir.TriadePtr; l: ir.Node);
VAR p : ir.ParamPtr;
    parr: ir.ParamArray;
    i, npars, delta: LONGINT;
BEGIN
  p := ir.Vars[fi.Name].Use;
  WHILE(p#NIL) AND (p.triade.Op = ir.o_fi) AND (p.triade.NodeNo = l) DO
    p := p.next;
  END;
  IF p = NIL THEN
    RETURN;
  END;
  npars := ir.Nodes[l].NIn;
  delta := npars - LEN(fi.Params^);
  -- придется перенести fi в ноду l и расширить ее спереди параметрами Undefined
  parr := ir.NewParams(npars, fi);
  FOR i := 0 TO delta-1 DO
    ir.MakeParNothing(parr[i]);
  END;

  FOR i := 0 TO LEN(fi.Params^)-1 DO
    ir.CopyParam(fi.Params[i], parr[i+delta]);
  END;
  ir.KillParams(fi);
  fi.Params := parr;
  gr.DeleteTriade(fi);
  gr.PutTriadeFirst(fi, l);
END MoveFi_1;

PROCEDURE OptimizeGoto (p: TriadePtr): TriadePtr;
VAR fromN, toN, l, m, n: ir.Node;
    tr, Goto, fi, foo: ir.TriadePtr;
    dom: BitVect.BitVector;
    a, arc: ir.Arc;
    i, j, a_num : LONGINT;
BEGIN
  gr.FindLoops();
  fromN := p.NodeNo;
  toN   := ir.Nodes[fromN].Out[0];
  IF fromN = 0 THEN
    RETURN NIL;
  END;
  IF ir.Nodes[fromN].IsPreheaderOf # ir.UndefLoop THEN
    RETURN NIL;
  END;
  IF ir.Nodes[toN].IsPreheaderOf # ir.UndefLoop THEN
    RETURN NIL;
  END;
  FOR i := 0 TO ir.Nodes[toN].NIn-1 DO
    FOR j := 0 TO ir.Nodes[fromN].NIn-1 DO
      IF ir.Nodes[fromN].In[j] = ir.Nodes[toN].In[i] THEN
        RETURN NIL;
      END;
    END;
  END;

  anz.WriteTest("o", "Optimize.OptimizeGoto() started (o)");

  arc   := ir.Nodes[fromN].OutArcs[0];
  IF ir.Nodes[toN].NIn = 1 THEN
    -- join fromN and toN
    -- move all triades of toN to fromN (including the last triade)
    Goto := ir.Nodes[fromN].Last;
    gr.MoveTriadesBefore(ir.Nodes[toN].First, ir.Nodes[toN].Last,
                   ir.Nodes[fromN].Last);

    gr.KillTriade(Goto);
    gr.KillArc_Ex(arc, FALSE);
    gr.RetargetAllOutgoingArcs(toN, fromN);
    gr.KillNode(toN, FALSE);

    -- we can update dominators and aDominators manually
    IF gr.IsDominators THEN
      -- Printf.printf("optimize goto - dominators\n");
      -- exclude toN from all the nodes' dominators
      FOR n := 0 TO ir.Nnodes-1 DO
        dom := ir.Nodes[n].Dominators;
        IF dom # NIL THEN
          BitVect_Excl(dom, toN);
        END;
      END;
    END;

    IF gr.IsaDominators THEN
      -- exclude arc from all the nodes' adominators
      -- Printf.printf("optimize goto - Adominators\n");
      FOR n := 0 TO ir.Nnodes-1 DO
        dom := ir.Nodes[n].aDominators;
        IF dom # NIL THEN
          BitVect_Excl(dom, arc);
        END;
      END;
    END;
    -- but ir.Order and gr.Loops shall be recomputed
    Changed := TRUE;
    gr.IsOrder := FALSE;
    gr.IsDominatorsTree := FALSE;
    gr.IsPDominatorsTree := FALSE;
    gr.FindLoops();
    gr.TopSort();
    gr.FindArcsDominators();
    gr.FindDominators();
    anz.WriteTest("o", "Optimize.OptimizeGoto-1() finished (o)");
  ELSIF (p.Prev = NIL) OR (p.Prev.Op = ir.o_fi) THEN
    n := p.NodeNo;
    l := ir.Nodes[n].Out[0];

    -- сначала проверим, что никакие аргументы фи в n не есть
    -- результаты fi в l
    fi := ir.Nodes[n].First;
    WHILE fi.Op = ir.o_fi DO
      FOR i := 0 TO LEN(fi.Params^)-1 DO
        IF fi.Params[i].tag = ir.y_Variable THEN
          tr := ir.Vars[fi.Params[i].name].Def;
          IF (tr.Op = ir.o_fi) AND (tr.NodeNo = l) THEN
            RETURN NIL;
          END;
        END;
      END;
      fi := fi.Next;
    END;

    -- сначала переводим дуги идущие в n  в l,
    -- и убиваем дугу из n в l.
    FOR i := 0 TO ir.Nodes[n].NIn-1 DO
      m := ir.Nodes[n].In[i];
      a := ir.Nodes[n].InArcs[i];
      a_num := gr.FindOutArc(a);
      -- дугу "из m в n" поменять на дугу "из m в l"
      -- вставить ее в массив дуг в l,
      -- добавить m в массив нодов в l,
      gr.Arcs[ a ].t := l;
      ir.Nodes[ m ].Out[ a_num ] := l;
      gr.AddNodeArc( ir.Nodes[l].InArcs, a,
                     ir.Nodes[l].In, m,
                     ir.Nodes[l].NIn );
    END;

    a := ir.Nodes[n].OutArcs[0];
    a_num := gr.FindInArc(a);
    fi := ir.Nodes[l].First;
    WHILE(fi.Op = ir.o_fi) DO
      CloneFi_1(fi, n, a_num);
      fi := fi.Next;
    END;

    fi := ir.Nodes[n].First;
    WHILE(fi.Op = ir.o_fi) DO
      foo := fi.Next; -- fi.Next may be changed in next line
      MoveFi_1(fi, l);
      fi := foo;
    END;

    ir.Nodes[n].NIn := 0; -- we already re-targeted these arcs to l
    gr.KillArc( ir.Nodes[n].OutArcs[0] );
    ChangedGraph := TRUE;
    anz.WriteTest("o", "Optimize.OptimizeGoto-2() finished (o)");
  END;
  RETURN NIL;
END OptimizeGoto;

(*----------------------------------------------------------------------------*)

PROCEDURE ReplaceCall (q: TriadePtr);
BEGIN
    ASSERT (q^.Op = ir.o_call);
    IF q^.ResType <> ir.t_float THEN
        ir.RemoveVar (q^.Name);
        q^.Tag     := ir.y_Nothing;
        q^.OpType  := ir.t_void;
        q^.OpSize  := 0;
        q^.ResType := ir.t_void;
        q^.ResSize := 0;
    END;
    IF NOT (ir.o_Silent IN q^.Options) THEN
      IF ir.o_SideEffectSafe IN q.Options THEN
        gr.TryAddWarning(q, 904); -- call to safe function removed: result was not used
        gr.KillTriade(q);
      ELSE
        gr.TryAddWarning(q, 903); -- function result is not used
      END;
    END;
END ReplaceCall;

(*----------------------------------------------------------------------------*)

(*
  Выкинуть явно ненужные вычисления - рекурсивная процедура
  (т.к. могут стать ненужными какие-то из параметров)
*)

PROCEDURE RemoveRedundant (p: TriadePtr);
VAR i: INT;
    q: TriadePtr;
BEGIN
--    anz.WriteTest("o", "Optimize.RemoveRedundant started (o)");
    IF p^.Params <> NIL THEN
      FOR i:=0 TO LEN(p^.Params^)-1 DO
        IF (p^.Params^[i].tag = ir.y_Variable) THEN
          ir.RemoveUse(p^.Params^[i]);
          IF ir.Vars^[p^.Params^[i].name].Use = NIL THEN
            q := ir.Vars^[p^.Params^[i].name].Def;
            IF q.Op = ir.o_call THEN
              ReplaceCall (q);
            ELSIF (q.Write = NIL) &
                  ((ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * q.Options = ir.OptionsSet{}) OR
                   (ir.o_Removable IN q.Options)) &
                  (q.Op # ir.o_forstart) & (q.Op # ir.o_forcont)
            THEN
              RemoveRedundant (q);
            END;
          END;
          ir.MakeParNothing(p^.Params^[i]);
        END;
      END;
    END;
    IF NOT ((p^.Op = ir.o_assign) &
            (p^.Params^[0].tag = ir.y_Variable) &
            (p^.Params^[0].next = NIL) &
            (ir.Vars^[p^.Params^[0].name].Def^.Op = ir.o_call) &
            (ir.Vars^[p^.Params^[0].name].Use = p^.Params^[0])) &
       (p^.Op <> ir.o_getpar) & NOT p^.Position.IsNull () &
       NOT (ir.o_Silent IN p^.Options) &
       ((p^.Tag <> ir.y_Variable) OR
        (ir.Vars^[p^.Name].LocalNo <> ir.TEMPORARY)) &
       (p^.Op <> ir.o_forcont)
       & ~ir.insideInline
    THEN
        env.errors.Warning (p^.Position, 900);
    END;
    IF p^.Tag = ir.y_Variable THEN
        ir.RemoveVar (p^.Name);
    END;
    gr.KillTriade(p);
--    anz.WriteTest("o", "Optimize.RemoveRedundant finished (o)");
END RemoveRedundant;

(*----------------------------------------------------------------------------*)


(*
  Собственно оптимизировать триаду
*)

PROCEDURE OptimizeTriade (p: TriadePtr): TriadePtr;
VAR s: TriadePtr;
    r: ParamPtr;
BEGIN
    s := p^.Next;


    r := NIL;
    IF (p^.Tag = ir.y_Variable) THEN
        r := ir.Vars^[p^.Name].Use;
        IF (r = NIL) & (p^.Op <> ir.o_forstart) & (p^.Op <> ir.o_forcont) THEN
            IF p^.Write = NIL THEN
                IF ir.OptionsSet{ ir.o_Checked, ir.o_Dangerous } * p^.Options = ir.OptionsSet{} THEN
                    RemoveRedundant (p);
                    Changed := TRUE;
                    RETURN s;
                ELSIF NOT p^.Position.IsNull () &
                      NOT (ir.o_Silent IN p^.Options)
                THEN
                    env.errors.Warning (p^.Position, 901);
                END;
            ELSE
                ReplaceCall (p);
            END;
        END;
    END;

(*
  Не удалось - проинтерпретировать ее
*)
    CASE p^.Op OF
    | ir.o_fi:          s := OptimizeFi       (p);
    | ir.o_val:         s := OptimizeVal      (p);
    | ir.o_cast:        s := OptimizeCast     (p);
    | ir.o_assign:      s := OptimizeAssign   (p);
    | ir.o_checklo:     s := OptimizeCheckLo  (p);
    | ir.o_checkhi:     s := OptimizeCheckHi  (p);
    | ir.o_checkb:      s := OptimizeCheckB   (p);
    | ir.o_checknil:    s := OptimizeCheckNil (p);
--  | ir.o_power:       s := OptimizePower (p);
    | ir.o_add:         IF NOT ((r <> NIL) & (TryUniteMulti (p, r) OR
                                         TryMoveAddition (p, r^.triade)))
                        THEN
                          s := OptimizeAddition (p);
                        END;
    | ir.o_mul:         IF NOT TryUniteMulti (p, r) THEN
                          s := OptimizeMultiplication (p);
                        END;
    | ir.o_div:         s := OptimizeDivision       (p, pc.sb_div);
    | ir.o_dvd:         s := OptimizeDivision       (p, pc.sb_slash);
    | ir.o_mod:         s := OptimizeModulo         (p, pc.sb_mod);
    | ir.o_rem:         s := OptimizeModulo         (p, pc.sb_rem);
    | ir.o_and:         IF NOT TryUniteMulti (p, r) THEN
                          s := OptimizeAnd (p);
                        END;
    | ir.o_or:          IF NOT TryUniteMulti (p, r) THEN
                          s := OptimizeOr (p);
                        END;
    | ir.o_andnot:      (* IF TryUniteMulti (p, r) THEN
                            RETURN s;
                        END; *)
                        s := OptimizeAndNot (p);
    | ir.o_xor:         IF NOT TryUniteMulti (p, r) THEN
                          s := OptimizeXor (p);
                        END;
    | ir.o_shl,
      ir.o_shr,
      ir.o_sar,
      ir.o_rol,
      ir.o_ror:         s := OptimizeShift (p);
    | ir.o_abs:         s := OptimizeAbs (p);
    | ir.o_not:         s := OptimizeNOT (p);
    | ir.o_cap:         s := OptimizeCAP (p);
    | ir.o_loset,
      ir.o_hiset:       s := OptimizeSetConstructor (p);
    | ir.o_incl:        s := OptimizeInclExcl (p);
    | ir.o_excl:        s := OptimizeInclExcl (p);
    | ir.o_copy:        IF ir.EqParams (p^.Params^[0], p^.Params^[1],
                                        ir.t_int, 4) OR
                           IsZero (p^.Params^[2], ir.t_int, 4)
                        THEN
                            Changed := TRUE;
                            RemoveRedundant (p);
                        END;
    | ir.o_le:          s := OptimizeLe       (p);
    | ir.o_move_le:     s := OptimizeMoveLe   (p);
    | ir.o_eq:          s := OptimizeEq       (p);
    | ir.o_move_eq:     s := OptimizeMoveEq   (p);
    | ir.o_case:        s := OptimizeCASE     (p);
    | ir.o_odd:         s := OptimizeOdd      (p);
    | ir.o_in:          s := OptimizeIn       (p);
    | ir.o_leset:       s := OptimizeLeSet    (p);
    | ir.o_forstart:    s := OptimizeForStart (p);
    | ir.o_forcont:     s := OptimizeForCont  (p);
    | ir.o_store:       s := OptimizeStore    (p);
    | ir.o_storer:      IF ~opAttrs.fastcomp THEN s := OptimizeStorer   (p); END;
    | ir.o_clear:       s := OptimizeClear    (p);
    | ir.o_hiword:      s := OptimizeHiWord   (p);
    | ir.o_goto:        s := OptimizeGoto     (p);
    ELSE
    END;
    RETURN s;
END OptimizeTriade;

(*
  Суррогат оптимизации триад - замена тех триад, которые не умеет
  генерировать генератор, на те, которые умеет.
*)

PROCEDURE NoOptimizeTriade (p: TriadePtr): TriadePtr;
VAR
  s: TriadePtr;
BEGIN
    s := p^.Next;
    CASE p^.Op OF
    | ir.o_abs:    s := DecomposeAbs (p);
    | ir.o_odd:    s := OptimizeOdd  (p);
    | ir.o_leset:  s := OptimizeLeSet(p);
    ELSE
      IF MachineLevelOptimizations &
         (p.OpSize = 8) &
         (p.OpType IN ir.TypeTypeSet{ ir.t_int, ir.t_unsign })
      THEN
        CASE p^.Op OF
        | ir.o_eq:       ReplaceEq64(p);       
        | ir.o_le:       ReplaceLe64(p);       
        | ir.o_checklo:  ReplaceCheckLo64(p);  
        | ir.o_checkhi:  ReplaceCheckHi64(p);  
        ELSE
        END;
      END;
    END;
    RETURN s;
END NoOptimizeTriade;


(*----------------------------------------------------------------------------*)

(*
  Если триада есть в списке констант, то выкинуть ее оттуда
*)

PROCEDURE KillConst (p: TriadePtr);
BEGIN
    IF p = Consts^[index1] THEN
        Consts^[index1] := NIL;
    ELSIF p = Consts^[index2] THEN
        Consts^[index2] := NIL;
    END;
END KillConst;

(*----------------------------------------------------------------------------*)

(*
  Заменить одну триаду на другую: если есть результаты, то заменить все
                                  использования s на d, иначе просто выкинуть s.
*)

PROCEDURE RepTriade (s, d: TriadePtr);
BEGIN
    KillConst (s);
    IF s^.Tag = ir.y_Variable THEN
        ir.MakeParVar_Ex(par, d.Name, FALSE, FALSE);-- don't do AddUse
        gr.ReplaceByParam (s, par);
    ELSE
        gr.KillTriade(s);
    END;
END RepTriade;

(*----------------------------------------------------------------------------*)

(*
  Доминирует ли одна из выходящих из p дуг над q?
  Если да, то заодно вернуть ее номер.
*)

PROCEDURE DominatesOneArc (p, q: Node): INT;
VAR i: INT;
BEGIN
    FOR i:=0 TO ir.Nodes^[p].NOut-1 DO
        IF gr.aDominates (ir.Nodes^[p].OutArcs^[i], q) THEN
            RETURN i;
        END;
    END;
    RETURN -1;
END DominatesOneArc;

(*----------------------------------------------------------------------------*)

(*
  Попытаться CSE две конкретные триады.
  Порядок обхода графа гарантирует нам, что если одна из триад доминирует
  над другой, то доминирует p над q; если это не так, то даже не будем пытаться.
*)

PROCEDURE TryCSE (p, q: TriadePtr): BOOLEAN;
VAR i, l: INT;
BEGIN
(*
  Должны совпадать операции, размеры, длины, количество параметров
  (исключение: пара storer-loadr, и нельзя оптимизировать пару storer-storer)
*)
    IF (p^.Op <> q^.Op) & NOT ((p^.Op = ir.o_storer) & (q^.Op = ir.o_loadr)) OR
       (q^.Op = ir.o_storer) OR NOT gr.DominatesTriade (p, q) OR
       (p^.ResType <> q^.ResType) OR (p^.ResSize <> q^.ResSize) OR
       (p^.OpType  <> q^.OpType)  OR (p^.OpSize  <> q^.OpSize) OR
       (LEN (p^.Params^) <> LEN (q^.Params^)) &
       NOT ((p^.Op = ir.o_storer) & (q^.Op = ir.o_loadr)) OR
       MustPreserve (q)
    THEN
        RETURN FALSE;
    END;
(*
  Должны совпадать сами параметры
*)
    IF (p^.Op = ir.o_storer) & (q^.Op = ir.o_loadr) THEN
        l := 0;
    ELSE
        l := LEN (p^.Params^) - 1;
    END;
    FOR i:=0 TO l DO
        IF
           NOT ir.EqParams (p^.Params^[i], q^.Params^[i],
                            ir.ParamType (p, i), ir.ParamSize (p, i))
        THEN
            RETURN FALSE;
        END;
    END;
(*
  Отдельно обработать:
  - пару storer-loadr - надо заменить результат второй триады на
                        второй параметр первой
  - условие - доминирования мало; надо, чтобы доминировала одна из выходящих дуг
  - условный переход  - надо просто заменить его на безусловный переход
*)
    IF (p^.Op = ir.o_storer) & (q^.Op = ir.o_loadr) THEN
        IF NOT ( (q^.Read=NIL) OR NobodyWrites (p, q, q^.Read)) THEN
            RETURN FALSE;
        END;
        KillConst (q);
        IF ir.o_IsChecknil IN q.Options THEN
          INCL(p.Options, ir.o_IsChecknil);
        END;
        gr.ReplaceByParam (q, p^.Params^[1]);
    ELSIF q^.Next = NIL THEN
        i := DominatesOneArc (p^.NodeNo, q^.NodeNo);
        IF i = -1 THEN
            RETURN FALSE;
        END;
        ReplaceByGoto (q^.NodeNo, i, TRUE);
    ELSIF (q.Op = ir.o_storer) AND (p.Op = ir.o_storer) THEN
        IF NOT NobodyWrites(p, q, q.Write) THEN
          RETURN FALSE;
        END;
        RepTriade (q, p);
        IF ir.o_IsChecknil IN q.Options THEN
          INCL(p.Options, ir.o_IsChecknil);
        END;
    ELSE
        IF (q^.Op = ir.o_fi) & (p^.NodeNo <> q^.NodeNo) OR
           (q^.Op = ir.o_loadr) & NOT NobodyWrites (p, q, q^.Read)
        THEN
            RETURN FALSE;
        END;
        IF ir.o_IsChecknil IN q.Options THEN
          INCL(p.Options, ir.o_IsChecknil);
        END;
--        anz.WriteTest("h", "Optimize.TryCSE (h)");
        RepTriade (q, p);
    END;
    NextTriade := p;
    Changed := TRUE;
    RETURN TRUE;
END TryCSE;

(*----------------------------------------------------------------------------*)

(*
  Попытаться найти общие члены в мультиоперациях.
  Условия те же, что и для TryCSE, плюс:
  - обе триады должны быть безопасными (если это не так, то длина - максимум 2)
  - все целые и указательные типы одной длины считаются совпадающими
*)

PROCEDURE TryCSEMulti (p, q: TriadePtr): BOOLEAN;
VAR i, p1, q1, q2, lp, lq: INT;
                r: TriadePtr;

    PROCEDURE FindParam (s: TriadePtr; ls: INT; r: ParamPtr; q1: INT): INT;
    VAR j: INT;
    BEGIN
        FOR j:=0 TO ls-1 DO
            IF (j <> q1) & ir.EqParams (s^.Params^[j], r,
                                        s^.ResType, s^.ResSize)
            THEN
                RETURN j;
            END;
        END;
        RETURN -1;
    END FindParam;

BEGIN
    IF (p^.Op <> q^.Op) OR
       (ir.ConstTags [p^.ResType] <> ir.ConstTags [q^.ResType]) OR
       (ir.ConstTags [p^.OpType]  <> ir.ConstTags [q^.OpType])  OR
       (p^.ResSize <> q^.ResSize) OR
       (p^.OpSize  <> q^.OpSize)  OR
       MustPreserve (q) OR
       NOT gr.DominatesTriadeApprox (p, q)
    THEN
        RETURN FALSE;
    END;
    IF (ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options = ir.OptionsSet{}) OR
       (LEN (p^.Params^) <= 2)
    THEN
        lp := LEN (p^.Params^);
    ELSE
        lp := 2;
    END;
    IF (ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * q^.Options = ir.OptionsSet{}) OR
       (LEN (q^.Params^) <= 2)
    THEN
        lq := LEN (q^.Params^);
    ELSE
        lq := 2;
    END;
    q1 := -1;
    p1 := -1;
    FOR i:=0 TO lp-1 DO
        q2 := FindParam (q, lq, p^.Params^[i], q1);
        IF q2 <> -1 THEN
            IF q1 = -1 THEN
                p1 := i;
                q1 := q2;
            ELSE
--                anz.WriteTest("h", "Optimize.TryCSEMulti (h)");
                IF LEN (p^.Params^) = 2 THEN
                    IF LEN (q^.Params^) = 2 THEN
                        RepTriade (q, p);
                        NextTriade := p;
                        Changed := TRUE;
                        RETURN TRUE;
                    END;
                ELSE
                    r := ir.NewTriadeLike (p, 2);
                    IF (p^.ResType = ir.t_ref) & (p1 <> 0) THEN
                        r^.ResType := opTune.index_ty;
                        r^.OpType  := opTune.index_ty;
                    END;
                    gr.InsertTriade (r, p);
                    ir.GenVar (ir.TEMPORARY, r^.Name, r);
                    ir.CopyParamWithRev (p^.Params^[p1], r^.Params^[0]);
                    ir.CopyParamWithRev (p^.Params^[i],  r^.Params^[1]);
                    ir.MakeParVar_Ex(par, r^.Name, FALSE, FALSE);
                    SYSTEM.EVAL( Replace2Pars (p, par, p1, i) );
                    p := r;
                END;
                ir.MakeParVar_Ex(par, p^.Name, FALSE, FALSE);
                NextTriade := p;
                SYSTEM.EVAL( Replace2Pars (q, par, q1, q2) );
                Changed := TRUE;
                RETURN TRUE;
            END;
        END;
    END;
    RETURN FALSE;
END TryCSEMulti;

(*----------------------------------------------------------------------------*)

(*
  Для данной триады попытаться провести поиск общих подвыражений
*)

PROCEDURE DoCSE (p: TriadePtr);
VAR i, l: INT;
       q: ParamPtr;
       v: BOOLEAN;
       w: TriadePtr;
       r: TriadeArray;
BEGIN
    v := FALSE;
(*
  Сначала пытаемся найти хоть одну переменную и проследить ее Use-список
*)
    IF p^.Op = ir.o_storer THEN
        l := 0;
    ELSE
        l := LEN (p^.Params^) - 1;
    END;
    FOR i:=0 TO l DO
        IF p^.Params^[i].tag = ir.y_Variable THEN
            q := ir.Vars^[p^.Params^[i].name].Use;
            REPEAT
                IF (q^.triade <> p) THEN
                    IF TryCSE (p, q^.triade) OR
                       (ir.isMulti IN ir.OpProperties[p^.Op]) &
                       TryCSEMulti (p, q^.triade)
                    THEN
                        RETURN;
                    END;
                END;
                q := q^.next;
            UNTIL q = NIL;
(*
  Такой же триады не нашли - продолжать имеет смысл только для мультиоперации
*)
            IF NOT (ir.isMulti IN ir.OpProperties[p^.Op]) THEN
                RETURN;
            END;
            v := TRUE;
        END;
    END;
(*
  Если не было ни одной переменной - посмотреть в списке таких триад
  (это все для того, чтобы объединять loadr-ы из адресных констант,
   т.е. оптимизировать работу с записями)
*)
    IF NOT v THEN
        WHILE (NConsts > 1) & (Consts^[NConsts-1] = NIL) DO
            DEC (NConsts);
        END;
        IF Consts^[NConsts-1] <> p THEN
            IF NConsts = LEN (Consts^) THEN
                NEW (r, NConsts * 2);
                FOR i:=0 TO NConsts-1 DO
                    r^[i] := Consts^[i];
                END;
                Consts := r;
            END;
            Consts^[NConsts] := p;
            INC (NConsts);
        END;
        w := p^.Next;
        index1 := NConsts - 1;
        FOR i:=1 TO NConsts-2 DO
            IF Consts^[i] <> NIL THEN
                index2 := i;
                IF TryCSE (Consts^[i], p) THEN
                    NextTriade := w;
                    RETURN;
                END;
            END;
        END;
    END;
END DoCSE;

(*
  Поиск общих подвыражений - диспетчер:
    - можно ли вообще трогать эту триаду?
    - нельзя объединять getpar, return, stop, error, copy, load, store
    - пока что: нельзя трогать forstart, forcont
    - все остальные - общий случай
*)

PROCEDURE CSETriade (p: TriadePtr);
BEGIN
    IF opAttrs.fastcomp AND
       (p^.Op # ir.o_clinit)
    THEN
       RETURN;
    END;

    IF MustPreserve (p) OR
       (p^.Op = ir.o_call)   OR (p^.Op = ir.o_seqpoint) OR
       (p^.Op = ir.o_load)   OR (p^.Op = ir.o_store)  OR (p^.Op = ir.o_copy)  OR
       (p^.Op = ir.o_alloca) OR (p^.Op = ir.o_getpar) OR (p^.Op = ir.o_base)  OR
       (p^.Op = ir.o_ret)    OR (p^.Op = ir.o_stop)   OR (p^.Op = ir.o_error) OR
       (ir.o_Volatile IN p^.Options)
    THEN
        RETURN;
    ELSIF (p^.Next <> NIL) OR (p^.Op <> ir.o_goto)     &
                              (p^.Op <> ir.o_forstart) &
                              (p^.Op <> ir.o_forcont)
    THEN
        DoCSE (p);
    END;
END CSETriade;

(*----------------------------------------------------------------------------*)

(*
  Что надо выполнить до оптимизации:
*)

PROCEDURE StartOptimization;
BEGIN
    gr.KillDead;
    gr.FindLoops;
    gr.FindArcsDominators;
    gr.FindDominators;
    BitVect.Free (vec);
    vec := BitVect.New (ORD(ir.Nnodes), FALSE);
--    anz.WriteTest("h", "Optimize.StartOptimization (h)");
END StartOptimization;

(*----------------------------------------------------------------------------*)

PROCEDURE Interpretation();
VAR    j: INT;
       i: ir.TSNode;
       p: TriadePtr;
BEGIN
    REPEAT
      Changed := FALSE;
      FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
          p := ir.Nodes^[ir.Order^[i]].First;
          REPEAT
              IF (p^.Op<>ir.o_fi) & (p^.Op<>ir.o_store) & (p^.Params <> NIL) THEN
                  FOR j:=0 TO LEN(p^.Params^)-1 DO
                      IF p^.Params^[j].tag = ir.y_Nothing THEN
                          MakeError (p, opTune.invalidLocation);
                          StartOptimization;
                          Changed := TRUE;
                          RETURN;
                      END;
                  END;
              END;
              p := OptimizeTriade (p);
              IF ChangedGraph THEN
                  StartOptimization;
                  Changed := TRUE;
                  RETURN;
              END;
          UNTIL p = NIL;
      END;
    UNTIL NOT Changed;
END Interpretation;

PROCEDURE CSE();
VAR    i: ir.TSNode;
       p: TriadePtr;
BEGIN
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        p := ir.Nodes^[ir.Order^[i]].First;
        REPEAT
            ASSERT (p^.NodeNo = ir.Order^[i]);
            NextTriade := p^.Next;
            CSETriade (p);
            IF ChangedGraph THEN
                Changed := TRUE;
                StartOptimization;
                RETURN;
            END;
            p := NextTriade;
        UNTIL p = NIL;
    END;
END CSE;

PROCEDURE Iteration;
BEGIN
    Changed      := FALSE;
    ChangedGraph := FALSE;
    NConsts      := 1;
    index1       := 0;
    index2       := 0;
    Interpretation();
    IF ChangedGraph THEN
      RETURN;
    END;
    CSE();
END Iteration;

(*----------------------------------------------------------------------------*)

(*
  Повторять оптимизации, пока хоть чего-то меняется
*)

PROCEDURE OptimizeGraph*;
BEGIN
    NEW (Consts, 50);
    Consts^[0] := NIL;
    StartOptimization;
    REPEAT
        Iteration;
    UNTIL NOT Changed;
    Consts := NIL;
    BitVect.Free (vec);
END OptimizeGraph;

PROCEDURE NoOptimizeGraph*;
VAR i: ir.TSNode;
    p: TriadePtr;
BEGIN
    NConsts      := 1;
    index1       := 0;
    index2       := 0;
    StartOptimization;

    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        p := ir.Nodes^[ir.Order^[i]].First;
        REPEAT
            p := NoOptimizeTriade (p);
        UNTIL p = NIL;
    END;

    BitVect.Free (vec);
END NoOptimizeGraph;

(*----------------------------------------------------------------------------*)

PROCEDURE OptimizeReturns*;
VAR n: Node;
BEGIN
    FOR n:=ir.ZERONode TO SYSTEM.PRED(ir.Nnodes) DO
        IF (ir.Nodes^[n].Last <> NIL) &
           ((ir.Nodes^[n].Last^.Op = ir.o_eq) OR
            (ir.Nodes^[n].Last^.Op = ir.o_le))
        THEN
            IF TryReplaceReturns (ir.Nodes^[n].Last) THEN END;
        END;
    END;
    IF ChangedGraph THEN
        gr.KillDead;
        gr.FindLoops;
        gr.FindArcsDominators;
        gr.FindDominators;
    END;
END OptimizeReturns;

(*----------------------------------------------------------------------------*)

PROCEDURE FindCondMoves*();

  PROCEDURE MyStartOptimization();
  BEGIN
    gr.KillDead;
    gr.FindLoops;
    gr.FindArcsDominators;
    gr.FindDominators;
    BitVect.Free (vec);
    vec := BitVect.New (ORD(ir.Nnodes), FALSE);
  END MyStartOptimization;

  PROCEDURE MyIteration(): BOOLEAN;
  VAR n : ir.Node;
      tr: ir.TriadePtr;
  BEGIN
    Changed := FALSE;
    ChangedGraph := FALSE;
    FOR n := 0 TO ir.Nnodes-1 DO
      IF ir.Nodes[n].Alive AND
         (ir.Nodes[n].First # NIL) AND
         (ir.Nodes[n].First.Op = ir.o_fi)
      THEN
        tr := ir.Nodes[n].First;
        WHILE (tr # NIL) AND (tr.Op = ir.o_fi) DO
          tr := OptimizeFi(tr);
          IF ChangedGraph THEN
            MyStartOptimization();
            RETURN TRUE;
          END;
        END;
      END;
    END;
    RETURN Changed;
  END MyIteration;

BEGIN
  MyStartOptimization();
  WHILE MyIteration() DO END;
  BitVect.Free (vec);
  vec := NIL;
END FindCondMoves;


(*----------------------------------------------------------------------------*)

PROCEDURE Init*;
BEGIN
    (* create arrays of constants '0' and '1' of different types *)
    anz.CreatePredefValues();
    (* create temporary variable *)
    ir.NewParamPtr (par);
    opt_nooptimize_add_uk := env.config.Option("nooptimize_add_uk");
    opt_nooptimize_fork   := env.config.Option("nooptimize_fork");
END Init;

PROCEDURE Done*;
BEGIN
END Done;

END Optimize.
