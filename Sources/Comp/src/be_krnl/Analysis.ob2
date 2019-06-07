MODULE Analysis;

<* IF DEFINED(MeDebugMode) AND MeDebugMode THEN *>
  <* procinline- *>
  <* noregvars+  *>
<* ELSE *>
  <* procinline+ *>
<* END *>

-- Корректнее обрабатывать VAL!
-- Разумнее int <-> unsign (СЕЙЧАС МБ НЕКОРРЕКТНО)
-- AlwaysLeq: p1 <= p2, нашли check: p1 < C, можно проверить:  C - 1 <= p2
-- AlwaysLss: учитывать check-и

IMPORT SYSTEM,
       ir,
       Calc,
       opTune,
       opAttrs,
       pc := pcK,
       gr := ControlGraph,
       pr := opProcs;

(*----------------------------------------------------------------------------*)

TYPE
    TypeType  = ir.TypeType;
    SizeType  = ir.SizeType;
    TriadePtr = ir.TriadePtr;
    ParamPtr  = ir.ParamPtr;
    VarNum    = ir.VarNum;
    INT       = ir.INT;
    VALUE     = ir.VALUE;
    Node      = ir.Node;

(*----------------------------------------------------------------------------*)

VAR NilVal*: ParamPtr;

TYPE SizeTypeRange = ir.SizeTypeRange;

VAR Zeroes-, Ones- : ARRAY TypeType OF ARRAY SizeTypeRange OF ParamPtr;
TYPE _0_63= [0..63];
VAR Pow2-         : ARRAY _0_63 OF VALUE;

(*----------------------------------------------------------------------------*)

PROCEDURE ^ AlwaysLeq* (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                        p: TriadePtr): BOOLEAN;
PROCEDURE ^ AlwaysGeq* (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                        p: TriadePtr): BOOLEAN;
PROCEDURE ^ AlwaysLss* (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                        p: TriadePtr): BOOLEAN;
PROCEDURE ^ AlwaysGtr* (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                        p: TriadePtr): BOOLEAN;
PROCEDURE ^ AlwaysEqu* (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                        p: TriadePtr): BOOLEAN;
PROCEDURE ^ AlwaysNeq* (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                        p: TriadePtr): BOOLEAN;

(*----------------------------------------------------------------------------*)

VAR WriteTest* : PROCEDURE (id: CHAR; N-: ARRAY OF CHAR);

(*----------------------------------------------------------------------------*)

(*
  Если переменная - переменная цикла FOR, то выдать его начало;
                                          иначе вернуть NIL
*)

PROCEDURE GetForStart (v: VarNum): TriadePtr;
VAR q, fs, fc: TriadePtr;
BEGIN
    q := ir.Vars^[v].Def;
    IF (q^.Op = ir.o_fi) & (LEN (q^.Params^) = 2) &
       (q^.Params^[0].tag = ir.y_Variable) &
       (q^.Params^[1].tag = ir.y_Variable)
    THEN
        IF ir.Vars^[q^.Params^[0].name].Def^.Op = ir.o_forstart THEN
            fs := ir.Vars^[q^.Params^[0].name].Def;
            fc := ir.Vars^[q^.Params^[1].name].Def;
        ELSE
            fs := ir.Vars^[q^.Params^[1].name].Def;
            fc := ir.Vars^[q^.Params^[0].name].Def;
        END;
        IF (fs^.Op = ir.o_forstart) & (fc^.Op = ir.o_forcont) THEN
            RETURN fs;
        END;
    END;
    RETURN NIL;
END GetForStart;

(*----------------------------------------------------------------------------*)

(*
  Если переменная - переменная цикла, то проверить, выполняется
  ли отношение для указанной (0 - lo, 1 - hi) границы цикла
*)

PROCEDURE CheckForLimit (v: VarNum; r2: ParamPtr; i: INT;
                         t: TypeType; s: SizeType;
                         proc: PROCEDURE (p1, p2: ParamPtr;
                                          t: TypeType; s: SizeType;
                                          p: TriadePtr): BOOLEAN
                        ): BOOLEAN;
VAR q: TriadePtr;
BEGIN
    q := ir.Vars^[v].Def;
    IF (* (q^.ResType = t) & *) (q^.ResSize = s) THEN
        q := GetForStart (q^.Name);
        IF (q <> NIL) THEN
            IF NOT Calc.IsNegative (q^.Params^[2].value, s) THEN
                RETURN proc (q^.Params^[i],   r2, t, s, q);
            ELSE
                RETURN proc (q^.Params^[1-i], r2, t, s, q);
            END;
        END;
    END;
    RETURN FALSE;
END CheckForLimit;

(*----------------------------------------------------------------------------*)

(*
  Если переменная - результат VAL, то вызвать процедуру проверки диапазона
                                   для ее параметра
*)
-- this proc converts value of 'value' to type of q.OpType
-- with all necessary checks, returns 'true' iff this conversion is correct
PROCEDURE Conversion(q: TriadePtr; value: ParamPtr; VAR result: ParamPtr): BOOLEAN;
BEGIN
  ir.NewParamPtr (result);
-- in Modula, we just convert it, see for overflow and that's all
  ir.MakeParNum (result, Calc.Val (value^.value, q^.ResType, q^.OpType,
                                                 q^.ResSize, q^.OpSize));
  IF Calc.overflow THEN
    RETURN FALSE
  END;
  RETURN TRUE;
END Conversion;


-- attention: the last parameter is senseless in Modula!
-- only in Java-version it's checked.
-- it's semantics is this:
-- if 'eq' is the condition, and
-- if 'val' is found, and an attemt is made
-- to convert 'value' from ResType to OpType, and we see that it cannot 
-- be completed, so corresponding comparision always results as
-- 'not equal' - then we may safely return TRUE due to impossibility
-- to make a conversion.

PROCEDURE TryVal (p1, p2: ParamPtr;
                  t: TypeType; p: TriadePtr;
                  proc: PROCEDURE (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                                   p: TriadePtr): BOOLEAN
                 ): BOOLEAN;

VAR q: TriadePtr;
    r: ParamPtr;
    ok: BOOLEAN;
BEGIN
    IF ir.ConstTags [t] = ir.y_NumConst THEN
        IF p1^.tag = ir.y_Variable THEN
            q := ir.Vars^[p1^.name].Def;
            IF (q^.Op = ir.o_val) & (p2^.tag = ir.y_NumConst) THEN
                ok := Conversion(q, p2, r);
                IF ok AND
                   proc(q^.Params^[0], r, q^.OpType, q^.OpSize, p)
                THEN
                  RETURN TRUE;
                END;
            END;
        ELSIF p2^.tag = ir.y_Variable THEN
            q := ir.Vars^[p2^.name].Def;
            IF (q^.Op = ir.o_val) & (p1^.tag = ir.y_NumConst) THEN
                ok := Conversion(q, p1, r);
                IF ok AND
                   proc(r, q^.Params^[0], q^.OpType, q^.OpSize, p)
                THEN
                  RETURN TRUE;
                END;
            END;
        END;
    END;
    RETURN FALSE;
END TryVal;

(*----------------------------------------------------------------------------*)

PROCEDURE AlwaysLeq* (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                      p: TriadePtr): BOOLEAN;
VAR q: TriadePtr;
    r: ParamPtr;
BEGIN
    IF ir.EqParams (p1, p2, t, s) THEN
        RETURN TRUE;
    ELSIF p1^.reverse OR p2^.reverse THEN
        RETURN FALSE;
    ELSIF (p1^.tag = ir.ConstTags [t]) & (p2^.tag = ir.ConstTags [t]) THEN
        RETURN Calc.CompareValues (pc.sb_leq, p1^.value, p2^.value, t, s, TRUE);
    END;

    IF (ir.ConstTags [t] = ir.y_NumConst) THEN
        IF (p1.tag = ir.y_NumConst) &
           Calc.CompareValues (pc.sb_equ, p1^.value, Calc.MinValue (t, s),
                               t, s, FALSE)
        OR (p2^.tag = ir.y_NumConst) &
           Calc.CompareValues (pc.sb_equ, p2^.value, Calc.MaxValue (t, s),
                               t, s, FALSE)
        THEN
            RETURN TRUE;
        END;
    END;

    IF opAttrs.nooptimize IN opAttrs.COMP_MODE THEN
        RETURN FALSE;
    END;

    IF p1^.tag = ir.y_Variable THEN
--  Проверить p1 на переменную цикла FOR
        IF CheckForLimit (p1^.name, p2, 1, t, s, AlwaysLeq) THEN
            RETURN TRUE;
        END;
-- Пройтись по использованиям, поискать доминирующую проверку
        r := ir.Vars^[p1^.name].Use;
        WHILE r <> NIL DO
            q := r^.triade;
            IF q <> p THEN
                CASE q^.Op OF
                | ir.o_checkhi:
                    IF (r^.paramnumber = 0) &
                       (q^.ResType = t) &
                       (q^.ResSize = s) &
                       gr.DominatesTriade (q, p) &
                       AlwaysLeq (q^.Params^[1], p2, t, s, q)
                    THEN
                        RETURN TRUE;
                    END;
                | ir.o_checklo:
                    IF (r^.paramnumber = 1) &
                       (q^.ResType = t) &
                       (q^.ResSize = s) &
                       gr.DominatesTriade (q, p) &
                       AlwaysLeq (q^.Params^[0], p2, t, s, q)
                    THEN
                        RETURN TRUE;
                    END;
                | ir.o_checkb:
                    IF (r^.paramnumber = 0) &
                       (q^.ResType = t) &
                       (q^.ResSize = s) &
                       gr.DominatesTriade (q, p) &
                       AlwaysLeq (q^.Params^[1], p2, t, s, q)
                    THEN
                        RETURN TRUE;
                    END;
                | ir.o_le:
                    IF (r^.paramnumber = 0) &
                       (q^.ResType = t) & (q^.ResSize = s) &
                       gr.aDominates (ir.Nodes^[q^.NodeNo].OutArcs^[0],
                                      p^.NodeNo) &
                       AlwaysLeq (q^.Params^[1], p2, t, s, q)
                    THEN
                        RETURN TRUE;
                    END;
                | ELSE
                END;
            END;
            r := r^.next;
        END;
    END;

    IF p2^.tag = ir.y_Variable THEN
--  Проверить p2 на переменную цикла FOR
        IF CheckForLimit (p2^.name, p1, 0, t, s, AlwaysGeq) THEN
            RETURN TRUE;
        END;
-- Пройтись по использованиям, поискать доминирующую проверку
        r := ir.Vars^[p2^.name].Use;
        WHILE r <> NIL DO
            q := r^.triade;
            IF q <> p THEN
                CASE q^.Op OF
                | ir.o_checkhi:
                    IF (r^.paramnumber = 1) &
                       (q^.ResType = t) & (q^.ResSize = s) &
                       gr.DominatesTriade (q, p) &
                       AlwaysLeq (p1, q^.Params^[0], t, s, q)
                    THEN
                        RETURN TRUE;
                    END;
                | ir.o_checklo:
                    IF (r^.paramnumber = 0) &
                       (q^.ResType = t) & (q^.ResSize = s) &
                       gr.DominatesTriade (q, p) &
                       AlwaysLeq (p1, q^.Params^[1], t, s, q)
                    THEN
                        RETURN TRUE;
                    END;
                | ir.o_checkb:
                    IF (q^.ResType = t) & (q^.ResSize = s) THEN
                      IF (r^.paramnumber = 0) THEN
                        IF gr.DominatesTriade (q, p) &
                           AlwaysLeq (p1, Zeroes[ t ][ s ], t, s, p)
                        THEN
                          RETURN TRUE;
                        END;
                      ELSE
                        ASSERT(r^.paramnumber = 1);
                        IF gr.DominatesTriade (q, p) &
                           AlwaysLeq (p1, q.Params[0], t, s, q)
                        THEN
                          RETURN TRUE;
                        END;
                      END;
                    END;

                | ir.o_le:
                    IF (r^.paramnumber = 1) &
                       (q^.ResType = t) &
                       (q^.ResSize = s) &
                       gr.aDominates (ir.Nodes^[q^.NodeNo].OutArcs^[0],
                                      p^.NodeNo) &
                       AlwaysLeq (p1, q^.Params^[0], t, s, q)
                    THEN
                        RETURN TRUE;
                    END;
                | ELSE
                END;
            END;
            r := r^.next;
        END;
    END;
    IF TryVal (p1, p2, t, p, AlwaysLeq) THEN
        RETURN TRUE;
    END;
(*
  Проверить: x <= x + const
*)
    IF (t IN ir.TypeTypeSet{ir.t_int, ir.t_unsign}) &
       (p1^.tag = ir.y_Variable) & (p2^.tag = ir.y_Variable)
    THEN
        q := ir.Vars^[p2^.name].Def;
        IF (q^.Op = ir.o_add) &
           (q^.ResType = t) & (q^.ResSize = s) &
           (LEN (q^.Params^) = 2) & NOT q^.Params^[0].reverse &
                                    NOT q^.Params^[1].reverse &
           (ir.EqParams (p1, q^.Params^[0], t, s) &
            (q^.Params^[1].tag = ir.y_NumConst) &
             Calc.CompareWithInt (pc.sb_geq, q^.Params^[1].value, 0, t, s)
            OR
            ir.EqParams (p1, q^.Params^[1], t, s) &
            (q^.Params^[0].tag = ir.y_NumConst) &
             Calc.CompareWithInt (pc.sb_geq, q^.Params^[0].value, 0, t, s))
        THEN
            RETURN TRUE;
        END;
    END;
     
(*
  Ничего не смогли узнать
*)
    RETURN FALSE;
END AlwaysLeq;

(*----------------------------------------------------------------------------*)

PROCEDURE AlwaysGeq* (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                      p: TriadePtr): BOOLEAN;
BEGIN
    RETURN AlwaysLeq (p2, p1, t, s, p);
END AlwaysGeq;

(*----------------------------------------------------------------------------*)

PROCEDURE AlwaysLss* (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                      p: TriadePtr): BOOLEAN;
VAR r: ParamPtr;
    q: TriadePtr;
BEGIN
    IF (p1^.tag = ir.ConstTags [t]) & (p2^.tag = ir.ConstTags [t]) THEN
        RETURN Calc.CompareValues (pc.sb_lss, p1^.value, p2^.value, t, s, TRUE);
    END;

    IF opAttrs.nooptimize IN opAttrs.COMP_MODE THEN
        RETURN FALSE;
    END;

--  Проверить p1 на переменную цикла FOR
    IF (p1^.tag = ir.y_Variable) &
       CheckForLimit (p1^.name, p2, 1, t, s, AlwaysLss)
    THEN
        RETURN TRUE;
    END;
--  Проверить p2 на переменную цикла FOR
    IF (p2^.tag = ir.y_Variable) &
       CheckForLimit (p2^.name, p1, 0, t, s, AlwaysGtr)
    THEN
        RETURN TRUE;
    END;

-- Пойтись по использованиям
    IF (p1^.tag = ir.y_Variable) THEN
        r := ir.Vars^[p1^.name].Use;
    ELSIF (p2^.tag = ir.y_Variable) THEN
        r := ir.Vars^[p2^.name].Use;
    ELSE
        RETURN FALSE;
    END;
    WHILE r <> NIL DO
        q := r^.triade;
        IF (q <> p) & (q^.ResType = t) & (q^.ResSize = s) THEN
            CASE q^.Op OF
            | ir.o_le:
                IF gr.aDominates (ir.Nodes^[q^.NodeNo].OutArcs^[1], p^.NodeNo) &
                   ir.EqParams (p2, q^.Params^[0], t, s) &
                   ir.EqParams (p1, q^.Params^[1], t, s)
                OR
                   gr.aDominates (ir.Nodes^[q^.NodeNo].OutArcs^[0], p^.NodeNo) &
                   (AlwaysLeq (q^.Params^[1], p2, t, s, q) &
                    AlwaysLss (p1, q^.Params^[0], t, s, q) OR
                    AlwaysLeq (p1, q^.Params^[0], t, s, q) &
                    AlwaysLss (q^.Params^[1], p2, t, s, q))
(*
                   (AlwaysLss (p1, q^.Params^[0], t, s, q) &
                    AlwaysLeq (q^.Params^[1], p2, t, s, q) OR
                    AlwaysLeq (p1, q^.Params^[0], t, s, q) &
                    AlwaysLss (q^.Params^[1], p2, t, s, q))
*)
                THEN
                    RETURN TRUE;
                END;
            | ir.o_checkhi:
                IF ir.EqParams (p1, q^.Params^[0], t, s) THEN
                  IF gr.DominatesTriade (q, p) &
                     AlwaysLeq (q^.Params^[1], p2, t, s, q)
                  THEN
                    RETURN TRUE;
                  END;
                ELSIF ir.EqParams (p2, q^.Params^[1], t, s) THEN
                  IF gr.DominatesTriade (q, p) &
                     AlwaysLeq (p1, q^.Params^[0], t, s, q)
                  THEN
                    RETURN TRUE;
                  END;
                END;  
            | ir.o_checklo:
                IF ir.EqParams (p2, q^.Params^[0], t, s) THEN
                  IF gr.DominatesTriade (q, p) &
                     AlwaysLss (p1, q^.Params^[1], t, s, q)
                  THEN
                    RETURN TRUE;
                  END;
                ELSIF ir.EqParams (p1, q^.Params^[1], t, s) THEN
                  IF gr.DominatesTriade (q, p) &
                     AlwaysLss (q^.Params^[0], p2, t, s, q)
                  THEN
                    RETURN TRUE;
                  END;
                END;
            | ir.o_checkb:
                IF ir.EqParams (p1, q^.Params^[0], t, s) THEN
                  IF gr.DominatesTriade (q, p) &
                     AlwaysLeq (q^.Params^[1], p2, t, s, q)
                  THEN
                    RETURN TRUE;
                  END;
                ELSIF ir.EqParams (p2, q^.Params^[1], t, s) THEN
                  IF gr.DominatesTriade (q, p) &
                     AlwaysLeq (p1, q^.Params^[0], t, s, q)
                  THEN
                    RETURN TRUE;
                  END;
                END;  

             | ELSE
            END;
        END;
        r := r^.next;
    END;
    IF TryVal (p1, p2, t, p, AlwaysLss) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END AlwaysLss;

(*----------------------------------------------------------------------------*)

PROCEDURE AlwaysGtr* (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                      p: TriadePtr): BOOLEAN;
BEGIN
    RETURN AlwaysLss (p2, p1, t, s, p);
END AlwaysGtr;

(*----------------------------------------------------------------------------*)

(*
  Есть ли триада o_eq с параметрами p1 и p2, из которой
  выходная дуга с номером j доминирует над n?
*)

PROCEDURE CanFindEq (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                     n: Node; j: INT): BOOLEAN;
VAR r: ParamPtr;
    q: TriadePtr;
BEGIN
    IF (p1^.tag = ir.y_Variable) THEN
        r := ir.Vars^[p1^.name].Use;
    ELSIF (p2^.tag = ir.y_Variable) THEN
        r := ir.Vars^[p2^.name].Use;
    ELSE
        RETURN FALSE;
    END;

    WHILE r <> NIL DO
        q := r^.triade;
        IF (q^.Op = ir.o_eq) & (q^.NodeNo <> n) &
           (q^.ResType = t) & (q^.ResSize = s) &
           gr.aDominates (ir.Nodes^[q^.NodeNo].OutArcs^[j], n) &
           (ir.EqParams (p1, q^.Params^[0], t, s) &
            ir.EqParams (p2, q^.Params^[1], t, s) OR
            ir.EqParams (p1, q^.Params^[1], t, s) &
            ir.EqParams (p2, q^.Params^[0], t, s))
        THEN
            RETURN TRUE;
        END;
        r := r^.next;
    END;
    RETURN FALSE;
END CanFindEq;

(*----------------------------------------------------------------------------*)

PROCEDURE AlwaysEqu* (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                      p: TriadePtr): BOOLEAN;
BEGIN
    IF (p1^.tag = ir.ConstTags [t]) & (p2^.tag = ir.ConstTags [t]) THEN
        RETURN Calc.CompareValues (pc.sb_equ, p1^.value, p2^.value, t, s, TRUE);
    END;
    IF opAttrs.nooptimize IN opAttrs.COMP_MODE THEN
        RETURN FALSE;
    END;
    RETURN CanFindEq (p1, p2, t, s, p^.NodeNo, 0);
END AlwaysEqu;

(*----------------------------------------------------------------------------*)

PROCEDURE AlwaysNeq* (p1, p2: ParamPtr; t: TypeType; s: SizeType;
                      p: TriadePtr): BOOLEAN;
VAR q: TriadePtr;
BEGIN
    IF (p1^.tag = ir.ConstTags [t]) & (p2^.tag = ir.ConstTags [t]) THEN
        RETURN Calc.CompareValues (pc.sb_neq, p1^.value, p2^.value, t, s, TRUE);
    END;
    IF opAttrs.nooptimize IN opAttrs.COMP_MODE THEN
        RETURN FALSE;
    END;
    IF p1^.tag = ir.y_Variable THEN
        q := ir.Vars^[p1^.name].Def;
        IF (q^.Op = ir.o_add) & (LEN (q^.Params^) = 2) &
          (ir.EqParams (q^.Params^[0], p2, q^.ResType, q^.ResSize) &
           (q^.Params^[1].tag = ir.y_NumConst) &
           NOT Calc.IsZero (q^.Params^[1].value, q^.ResType, q^.ResSize)
          OR
           ir.EqParams (q^.Params^[1], p2, q^.ResType, q^.ResSize) &
           (q^.Params^[0].tag = ir.y_NumConst) &
           NOT Calc.IsZero (q^.Params^[0].value, q^.ResType, q^.ResSize)
          )
        THEN
            RETURN TRUE;
        END;
    END;
    IF p2^.tag = ir.y_Variable THEN
        q := ir.Vars^[p2^.name].Def;
        IF (q^.Op = ir.o_add) & (LEN (q^.Params^) = 2) &
          (ir.EqParams (q^.Params^[0], p1, q^.ResType, q^.ResSize) &
           (q^.Params^[1].tag = ir.y_NumConst) &
           NOT Calc.IsZero (q^.Params^[1].value, q^.ResType, q^.ResSize)
          OR
           ir.EqParams (q^.Params^[1], p1, q^.ResType, q^.ResSize) &
           (q^.Params^[0].tag = ir.y_NumConst) &
           NOT Calc.IsZero (q^.Params^[0].value, q^.ResType, q^.ResSize)
          )
        THEN
            RETURN TRUE;
        END;
    END;
    RETURN CanFindEq (p1, p2, t, s, p^.NodeNo, 1);
END AlwaysNeq;

(*----------------------------------------------------------------------------*)

PROCEDURE CheckForNew(par: ParamPtr): BOOLEAN;
VAR call: ir.TriadePtr;
    name: pr.ProcNum;
BEGIN
  IF par.tag # ir.y_Variable THEN
    RETURN FALSE;
  END;
  call := ir.Vars[par.name].Def;
  IF (call.Op # ir.o_call) OR
     (call.Params[0].tag # ir.y_ProcConst)
  THEN
    RETURN FALSE;
  END;

  name := VAL(pr.ProcNum, call.Params[0].name);
  RETURN (pr.ProcList[name].obj # NIL) AND
         ( (pc.xot_constr IN pr.ProcList[name].obj.xtags) OR
           (pc.xot_allocator IN pr.ProcList[name].obj.xtags)
         );
END CheckForNew;

PROCEDURE AlwaysNeqNil* (ptr: ParamPtr; p: TriadePtr): BOOLEAN;
VAR q: ParamPtr;
    t: TriadePtr;
BEGIN
    IF ptr^.tag = ir.y_AddrConst THEN
        RETURN TRUE;
    END;
    IF opAttrs.nooptimize IN opAttrs.COMP_MODE THEN
        RETURN FALSE;
    END;

    IF CheckForNew(ptr) THEN
        RETURN TRUE;
    END;

    IF ptr^.tag = ir.y_Variable THEN
        q := ir.Vars^[ptr^.name].Use;
        WHILE q <> NIL DO
            t := q^.triade;
            IF (t <> p) &
               (q # ptr) &
               ((t^.Op = ir.o_loadr) OR (t^.Op = ir.o_checknil)) &
               gr.DominatesTriade (t, p)
            THEN
                RETURN TRUE;
            END;
            q := q^.next;
        END;
    END;
    RETURN AlwaysNeq (ptr, NilVal, opTune.addr_ty, opTune.addr_sz, p);
END AlwaysNeqNil;


PROCEDURE MakeParNum(v: ir.VALUE) : ParamPtr;
VAR p : ParamPtr;
BEGIN
  ir.NewParamPtr(p);
  ir.MakeParNum(p, v);
  RETURN p;
END MakeParNum;

PROCEDURE MakeParFloat(v: ir.VALUE) : ParamPtr;
VAR p : ParamPtr;
BEGIN
  ir.NewParamPtr(p);
  ir.MakeParFloat(p, v);
  RETURN p;
END MakeParFloat;

PROCEDURE MakeParNil() : ParamPtr;
VAR p : ParamPtr;
BEGIN
  ir.NewParamPtr(p);
  ir.MakeParNil(p);
  RETURN p;
END MakeParNil;

PROCEDURE CreateZeroes();
VAR t : TypeType;
    s : SizeType;
BEGIN
(*
  Проинициализировать массив нулей
*)
  FOR t:=MIN(ir.TypeType) TO MAX(ir.TypeType) DO
      FOR s := 0 TO ir.MaxVarSize DO
        Zeroes[t][s] := NIL;
      END;
  END;

  Zeroes[ir.t_int][1] := MakeParNum(Calc.GetInteger(0,1));
  Zeroes[ir.t_int][2] := MakeParNum(Calc.GetInteger(0,2));
  Zeroes[ir.t_int][4] := MakeParNum(Calc.GetInteger(0,4));
  Zeroes[ir.t_int][8] := MakeParNum(Calc.GetInteger(0,8));
  Zeroes[ir.t_int][8] := MakeParNum(Calc.GetInteger(0,8));

  Zeroes[ir.t_unsign][1] := MakeParNum(Calc.GetCardinal(0,1));
  Zeroes[ir.t_unsign][2] := MakeParNum(Calc.GetCardinal(0,2));
  Zeroes[ir.t_unsign][4] := MakeParNum(Calc.GetCardinal(0,4));
  Zeroes[ir.t_unsign][8] := MakeParNum(Calc.GetCardinal(0,8));

  Zeroes[ir.t_ref][opTune.addr_sz] := MakeParNil();

<* IF ~nofloat THEN *>
  Zeroes[ir.t_float][opTune.real_sz]     :=
            MakeParFloat(Calc.GetFloat(0.0, opTune.real_sz));

  Zeroes[ir.t_float][opTune.longreal_sz] :=
            MakeParFloat(Calc.GetFloat(0.0, opTune.longreal_sz));

  Zeroes[ir.t_float][opTune.IDB.ld_real_sz] :=
            MakeParFloat(Calc.GetFloat(0.0, opTune.IDB.ld_real_sz));
<* END *>
END CreateZeroes;

PROCEDURE CreateOnes();
VAR t : TypeType;
    s : SizeType;
BEGIN
  FOR t:=MIN(ir.TypeType) TO MAX(ir.TypeType) DO
    FOR s := 0 TO ir.MaxVarSize DO
      Ones[t][s] := NIL;
    END;
  END;

  Ones[ir.t_int][1] := MakeParNum(Calc.GetInteger(1,1));
  Ones[ir.t_int][2] := MakeParNum(Calc.GetInteger(1,2));
  Ones[ir.t_int][4] := MakeParNum(Calc.GetInteger(1,4));
  Ones[ir.t_int][8] := MakeParNum(Calc.GetInteger(1,8));
  Ones[ir.t_unsign][1] := MakeParNum(Calc.GetCardinal(1,1));
  Ones[ir.t_unsign][2] := MakeParNum(Calc.GetCardinal(1,2));
  Ones[ir.t_unsign][4] := MakeParNum(Calc.GetCardinal(1,4));
  Ones[ir.t_unsign][8] := MakeParNum(Calc.GetCardinal(1,8));

<* IF ~nofloat THEN *>
  Ones[ir.t_float][opTune.real_sz]     :=
              MakeParFloat(Calc.GetFloat(1.0, opTune.real_sz));

  Ones[ir.t_float][opTune.longreal_sz] :=
              MakeParFloat(Calc.GetFloat(1.0, opTune.longreal_sz));

  Ones[ir.t_float][opTune.IDB.ld_real_sz] :=
              MakeParFloat(Calc.GetFloat(1.0, opTune.IDB.ld_real_sz));
<* END *>
END CreateOnes;

PROCEDURE Create2Pows();
VAR t : INT;
    foo: VALUE;
BEGIN
  foo := Calc.GetCardinal(1,8);
  FOR t:=0 TO 63 DO
    Pow2[t] := foo;
    foo := Calc.Binary(pc.sb_plus, ir.t_unsign, 8, foo,foo);
  END;
END Create2Pows;

PROCEDURE CreatePredefValues*();
BEGIN
  ir.NewParamPtr (NilVal);
  ir.MakeParNum (NilVal, Calc.GetNilPointer());
  CreateZeroes();
  CreateOnes();
  Create2Pows();
END CreatePredefValues;

(*----------------------------------------------------------------------------*)

BEGIN
    NilVal := NIL;
END Analysis.
