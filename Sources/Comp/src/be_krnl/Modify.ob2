MODULE Modify;

(*
  - Convert FOR and CASE statements to simpler parts
  - Code motion
  - Strength reduction
*)

IMPORT BitVect,
       ir,
       gr := ControlGraph,
       Optimize,
       Calc,
       opTune,
       opAttrs,
       Analysis,
       pc := pcK,
       xiEnv,
       SYSTEM;

(* ---------------------------- Various types ------------------------------- *)

TYPE
        BitVector  = BitVect.BitVector;
        INT        = ir.INT;
        VALUE      = ir.VALUE;
        VarNum     = ir.VarNum;
        Local      = ir.Local;
        Node       = ir.Node;
        Arc        = ir.Arc;
        Loop       = ir.Loop;
        ArcArray   = ir.ArcArray;
        ParamPtr   = ir.ParamPtr;
        ParamArray = ir.ParamArray;
        TriadePtr  = ir.TriadePtr;
        TagType    = ir.TagType;
        SizeType   = ir.SizeType;
        TypeType   = ir.TypeType;
        Operation  = ir.Operation;
VAR par1, par2 : ParamPtr;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                         Convert CASE statement                             *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

VAR buckets: POINTER TO ARRAY OF INT;
          v: ParamArray;
          a: ArcArray;
          p: TriadePtr;
         sz: SizeType;
         tp: TypeType;
         nb: INT;
          N: INT;
    default: Node;
     median,
          d,
      delta: pc.VALUE;
     isConstantCase: BOOLEAN;

CONST MinDensity = 40;              -- Минимальная плотность таблицы,
                                    -- при которой CASE еще делается
                                    -- таблицей, а не россыпью переходов
                                    -- (в процентах)

     UnrollLimit = 25;              -- Максимальное количество триад в
                                    -- unroll-аемых циклах
(*----------------------------------------------------------------------------*)

PROCEDURE Density (k, n: INT): INT;
VAR i, l: INT;
    c: SYSTEM.CARD32;
BEGIN
    l := 0;
    FOR i:=2*buckets^[n] TO 2*k BY 2 DO
        IF ir.EqParams (v^[i], v^ [i + 1], ir.t_unsign, sz) THEN
            INC (l);
        ELSE
            INC (l, 2);
        END;
    END;
    c := Calc.ToCardinal (Calc.Binary (pc.sb_minus, ir.t_unsign, p^.ResSize,
                                       v^[2*k+1]^.value,
                                       v^[2*buckets^[n]]^.value),
                          p^.ResSize);
    IF c <> Calc.MaxCard32 THEN
        c := c + 1;
    END;
    RETURN VAL (SYSTEM.CARD32, l * 100) DIV c;
END Density;

(*----------------------------------------------------------------------------*)

PROCEDURE Partition (): INT;
VAR k, d: INT;
BEGIN
    ASSERT (N <> 0);
    nb := 0;
    FOR k:=0 TO N-1 DO
        buckets^[nb] := k;
        LOOP
            IF nb = 0 THEN
                EXIT;
            END;
            d := Density (k, nb - 1);
            IF d < MinDensity THEN
                EXIT;
            END;
            DEC (nb);
        END;
        INC (nb);
    END;
    buckets^[nb] := N;
    RETURN nb;
END Partition;

(*----------------------------------------------------------------------------*)

PROCEDURE ChangeArc (s: INT; n: Node);
BEGIN
    gr.AddNodeArc (ir.Nodes^[n].OutArcs, a^[s],
                   ir.Nodes^[n].Out, gr.Arcs^[a^[s]].t,
                   ir.Nodes^[n].NOut);
    gr.Arcs^[a^[s]].f := n;
    ir.Nodes^[gr.Arcs^[a^[s]].t].In^[gr.FindInArc (a^[s])] := n;
END ChangeArc;

(*----------------------------------------------------------------------------*)

PROCEDURE CodeOneBucket (Expr: ParamPtr; tp: TypeType; b: INT; n: Node;
                         shiftval: VALUE);
VAR needdef: BOOLEAN;
          q: TriadePtr;
       i, k: INT;
BEGIN
    ASSERT (buckets^[b] < buckets^[b + 1] - 1);
    LOOP
        FOR i:=buckets^[b] TO buckets^[b+1]-2 DO
            IF Calc.CompareWithInt (pc.sb_neq,
                                    Calc.Binary (pc.sb_minus,
                                                 ir.t_unsign, sz,
                                                 v^[2*i+2]^.value,
                                                 v^[2*i+1]^.value),
                                     1, ir.t_unsign, sz)
            THEN
                needdef := TRUE;
                EXIT;
            END;
        END;
        needdef := FALSE;
        EXIT;
    END;
    IF (buckets^[b + 1] - buckets^[b] = 2) AND NOT needdef THEN
        q := ir.NewTriadeInit (2, ir.o_le, tp, sz);
        ir.CopyParam (v^[buckets^[b]*2+1], q^.Params^[1]);
        q^.Params^[1]^.value := Calc.Binary (pc.sb_minus, tp, sz,
                                             q^.Params^[1]^.value, shiftval);
        ChangeArc (buckets^[b],     n);
        ChangeArc (buckets^[b] + 1, n);
    ELSE
        FOR i:=buckets^[b] TO buckets^[b+1]-1 DO
            ChangeArc (i, n);
        END;
        IF needdef AND (default#MAX(Node)) THEN
            gr.NewArc (n, default, TRUE);
        END;
        q := ir.NewTriadeInit ((buckets^[b+1] - buckets^[b]) * 2 + 1,
                               ir.o_case, tp, sz);
        k := 1;
        FOR i:=buckets^[b]*2 TO buckets^[b+1]*2-1 DO
            ir.CopyParam (v^[i], q^.Params^[k]);
            q^.Params^[k]^.value := Calc.Binary (pc.sb_minus, tp, sz,
                                                 q^.Params^[k]^.value,
                                                 shiftval);
            INC (k);
        END;
    END;
    q^.Position := p^.Position;
    INCL (q^.Options, ir.o_Silent );
    IF isConstantCase THEN
        INCL (q^.Options, ir.o_Constant );
    END;
    ir.CopyParam (Expr, q^.Params^[0]);
    gr.PutTriadeLast (q, n);
END CodeOneBucket;

(*----------------------------------------------------------------------------*)

PROCEDURE MakeIfToBucket (op: Operation; tp: TypeType;
                          op1, op2: ParamPtr; b: INT; n: Node;
                          shiftval: VALUE; opd: ParamPtr);
VAR q: TriadePtr;
    m: Node;
BEGIN
    IF default=MAX(Node) THEN
      IF buckets^[b] = buckets^[b + 1] - 1 THEN
          gr.MakeGoto(n);
          ChangeArc (buckets^[b], n);
      ELSE
        m := gr.NewNode ();
        gr.MakeGoto(n);
        gr.NewArc (n, m, TRUE);
        CodeOneBucket (opd, tp, b, m, shiftval);
      END;
      RETURN;
    END;

    q := ir.NewTriadeInit (2, op, tp, sz);
    q^.Position := p^.Position;
    ir.CopyParam (op1, q^.Params^[0]);
    ir.CopyParam (op2, q^.Params^[1]);
    IF buckets^[b] = buckets^[b + 1] - 1 THEN
        ChangeArc (buckets^[b], n);
    ELSE
        m := gr.NewNode ();
        gr.NewArc (n, m, TRUE);
        CodeOneBucket (opd, tp, b, m, shiftval);
    END;
    gr.NewArc (n, default, TRUE);
    gr.PutTriadeLast (q, n);
END MakeIfToBucket;

(*----------------------------------------------------------------------------*)

(*
  Закодировать один интервал bucket-ов
*)

PROCEDURE Interval (lb: INT; ml: BOOLEAN;
                    ub: INT; mu: BOOLEAN;
                    Expr: ParamPtr; n: Node);
VAR   uv, lv, kv: ParamPtr;
               r: TriadePtr;
            i, k: INT;
              lo: BOOLEAN;
          m1, m2: Node;
BEGIN
    lv := v^[buckets^[lb] * 2];
    uv := v^[(buckets^[ub+1]-1) * 2 + 1];
    IF lb = ub THEN
        IF ml OR mu THEN
            IF ir.EqParams (lv, uv, ir.t_unsign, sz) THEN
                MakeIfToBucket (ir.o_eq, ir.t_unsign, Expr, lv, lb, n,
                                Calc.GetZZValue(0), Expr);
            ELSIF ml & mu THEN
                r := ir.NewTriadeTInit (2, ir.o_add, ir.y_Variable,
                                        ir.t_unsign, sz);
                r^.Position := p^.Position;
                ir.GenVar (ir.TEMPORARY, r^.Name, r);
                ir.CopyParam (Expr, r^.Params^[0]);
                ir.CopyParam (lv,   r^.Params^[1]);
                ir.SetParamReverse(r^.Params^[1], TRUE);
                gr.PutTriadeLast (r, n);

                ir.MakeParNum (par1, Calc.Binary (pc.sb_minus,
                                                  ir.t_unsign, sz,
                                                  uv^.value, lv^.value));
                ir.MakeParVar_Ex(par2, r.Name, FALSE, FALSE);
                MakeIfToBucket (ir.o_le, ir.t_unsign, par2, par1, lb, n,
                                lv^.value, par2);
            ELSIF ml THEN
                MakeIfToBucket (ir.o_le, tp, lv, Expr, lb, n, Calc.GetZZValue(0), Expr);
            ELSE
                MakeIfToBucket (ir.o_le, tp, Expr, uv, lb, n, Calc.GetZZValue(0), Expr);
            END;
        ELSIF buckets^[ub] = buckets^[ub + 1] - 1 THEN
            gr.MakeGoto (n);
            ChangeArc   (buckets^[ub], n);
        ELSE
            CodeOneBucket (Expr, tp, lb, n, Calc.GetZZValue(0));
        END;
    ELSE
        median.binary (pc.sb_plus, lv^.value, uv^.value);
        median.binary (pc.sb_slash, median, Calc.GetZZValue(2));
        delta.binary (pc.sb_minus, lv^.value, median);
        delta.unary  (pc.su_abs, delta);
        k  := lb;
        lo := FALSE;
        FOR i:=lb TO ub DO
            d := Calc.Binary(pc.sb_minus, ir.t_ZZ, 0,
                             v^[2*buckets^[i]]^.value, median);
            d := Calc.Unary (pc.su_abs,ir.t_ZZ, 0, d);
            IF NOT Calc.CompareValues(pc.sb_gtr, d, delta, ir.t_ZZ, 0, TRUE)
            THEN
                k  := i;
                delta := Calc.Unary(pc.su_conv, ir.t_ZZ, 0, d);
                lo := TRUE;
            END;
            d := Calc.Binary (pc.sb_minus, ir.t_ZZ, 0,
                              v^[2*(buckets^[i+1]-1)+1]^.value, median);
            d := Calc.Unary (pc.su_abs, ir.t_ZZ, 0, d);
            IF NOT Calc.CompareValues (pc.sb_gtr, d, delta, ir.t_ZZ, 0, TRUE)
            THEN
                k  := i;
                delta.unary (pc.su_conv, d);
                lo := FALSE;
            END;
        END;
        IF k = lb THEN
            lo := FALSE;
        ELSIF k = ub THEN
            lo := TRUE;
        END;
        r := ir.NewTriadeInit (2, ir.o_le, tp, sz);
        r^.Position := p^.Position;
        gr.PutTriadeLast (r, n);
        m1 := gr.NewNode ();
        gr.NewArc (n, m1, TRUE);
        m2 := gr.NewNode ();
        gr.NewArc (n, m2, TRUE);
        IF lo THEN
            kv := v^[2*buckets^[k]];
            ir.CopyParam (kv,   r^.Params^[0]);
            ir.CopyParam (Expr, r^.Params^[1]);
            Interval (k, FALSE, ub, mu, Expr, m1);
            Interval (lb, ml, k - 1,
                      Calc.CompareWithInt (pc.sb_neq,
                                           Calc.Binary (pc.sb_minus,
                                                        ir.t_unsign, sz,
                                                        kv^.value,
                                                        v^[(buckets^[k]-1)*2+1].value),
                                           1, ir.t_unsign, sz),
                      Expr, m2);
        ELSE
            kv := v^[2*(buckets^[k+1]-1)+1];
            ir.CopyParam (Expr, r^.Params^[0]);
            ir.CopyParam (kv,   r^.Params^[1]);
            Interval (lb, ml, k, FALSE, Expr, m1);
            Interval (k + 1,
                      Calc.CompareWithInt (pc.sb_neq,
                                           Calc.Binary (pc.sb_minus,
                                                        ir.t_unsign, sz,
                                                        v^[(buckets^[k]+1)*2].value,
                                                        kv^.value),
                                            1, ir.t_unsign, sz),
                      ub, mu, Expr, m2);
        END;
    END;
END Interval;

(*----------------------------------------------------------------------------*)

PROCEDURE ConvertOneCase (q: TriadePtr; nd: Node);
VAR
  i, n: INT;
BEGIN
    isConstantCase := ir.o_Constant IN q.Options;
    IF LEN (q^.Params^) = 1 THEN
        gr.MakeGoto (q^.NodeNo);
    ELSE
        IF median = NIL THEN
            median := pc.value.new (xiEnv.null_pos, pc.ZZ_type);
            delta  := pc.value.new (xiEnv.null_pos, pc.ZZ_type);
            d      := pc.value.new (xiEnv.null_pos, pc.ZZ_type);
        END;
        gr.FindDominators;
        gr.KillLoops (ir.Nodes^[nd].LoopNo);
        p  := q;
        sz := q^.ResSize;
        tp := q^.ResType;
        IF (tp = ir.t_int) &
           NOT Calc.IsNegative (q^.Params^[1].value, sz)
        THEN
            tp := ir.t_unsign;
        END;
        N := LEN (q^.Params^) DIV 2;
        IF (v = NIL) OR (LEN (v^) < 2 * N) THEN
            NEW (v, 2 * N);
            NEW (buckets, N + 1);
            NEW (a,       2 * N + 1);
        END;
        FOR i:=1 TO 2*N DO
            v^[i-1] := q^.Params^[i];
        END;
        n := Partition ();
        FOR i:=0 TO ir.Nodes^[nd].NOut-1 DO
            a^[i] := ir.Nodes^[nd].OutArcs^[i];
        END;
        IF ir.Nodes^[nd].NOut = N THEN
            default := MAX (Node);
        ELSE
            default := gr.NewNode ();
            gr.MakeGoto (default);
            ChangeArc   (ir.Nodes^[nd].NOut - 1, default);
        END;
        ir.Nodes^[nd].NOut := 0;
        gr.GraphChanged;
        Interval (0, Calc.CompareValues (pc.sb_gtr,
                                         q^.Params^[1].value,
                                         Calc.MinValue (tp, sz),
                                         tp, sz, TRUE),
                  n - 1, Calc.CompareValues (pc.sb_lss,
                                             q^.Params^[2*N].value,
                                             Calc.MaxValue (tp, sz),
                                             tp, sz, TRUE),
                  q^.Params^[0], nd);
    END;
    gr.KillTriade (q);
END ConvertOneCase;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                          Convert FOR statement                             *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

-- Пусть есть цикл
--     FOR i:=A TO B BY C DO ... END;
--
-- Предположим, что C > 0 (случай, когда C < 0, аналогичен). Тогда вокруг цикла
-- вставим
--         IF A <= B THEN ... END;
-- т.е. гарантируем, что цикл выполнится хоть раз.
--
-- Пусть T - это CARDINAL тип подходящей длины. Тогда цикл можно заменить на
--
--         t = (VAL (T, B) - VAL (T, A)) DIV C + 1; (* Игнорировать возможное
--                                                     переполнение *)
--         i = A;
--     L:
--         ...
--         i += C;                              (* Игнорировать переполнение *)
--         t --;                                (* И здесь тоже игнорировать *)
--         IF t <> 0 THEN GOTO L;
--
-- Кроме того, если
--   (1) B <= MAX (тип i) - C, или
--   (2) i типа INT, A >= 0, C >= 0
-- то цикл можно заменить на обычный цикл
--
--         i = A;
--     L:
--         ...
--         i += C;
--         IF i <= B THEN GOTO L;
--
-- (в случае (2) проверку надо делать как CARDINAL).
--
-- Этот вариант может уменьшить нагрузку на регистры, а может и нет,
-- поэтому, прежде чем его делать, надо проверить еще дополнительное
-- условие, а именно: переменная цикла используется внутри него и причем
-- так, что SR не сможет это использование вычистить.

VAR AbsC:      ParamPtr;
    LongAbsC:  ParamPtr;
    CPositive: BOOLEAN;
    compareType: ir.TypeType;
    counterSize: ir.SizeType;
    fs:        TriadePtr;
    s, b, e:   Node;

(* -------------------------------------------------------------------------- *)

(*
  Размер счетчика цикла
*)

PROCEDURE CounterSize (s: SizeType): SizeType;
BEGIN
    IF s > opTune.counter_sz THEN
        RETURN s;
    ELSE
        RETURN opTune.counter_sz;
    END;
END CounterSize;

(* -------------------------------------------------------------------------- *)

(*
  Привести начальное/конечное значение цикла к типу счетчика
*)

PROCEDURE CastLimit (src, dst: ParamPtr; n: Node): ParamPtr;
VAR p: TriadePtr;
BEGIN
    IF fs^.ResSize = counterSize THEN
        RETURN src;
    ELSIF src^.tag = ir.y_NumConst THEN
        ir.MakeParNum(dst,
                      Calc.Val (src^.value, fs^.ResType, fs^.ResType,
                                            fs^.ResSize, counterSize)
                     );
    ELSE
        p := ir.NewTriadeTInit (1, ir.o_val, ir.y_Variable,
                                fs^.ResType, fs^.ResSize);
        p^.ResSize  := counterSize;
        p^.Position := fs^.Position;
        ir.CopyParam (src, p^.Params^[0]);
        ir.GenVar (ir.TEMPORARY, p^.Name, p);
        gr.PutTriadeFirst (p, n);
        ir.MakeParVar_Ex(dst, p^.Name, FALSE, FALSE);
    END;
    RETURN dst;
END CastLimit;

(* -------------------------------------------------------------------------- *)

(*
  Посчитать t := (B - A) DIV C + 1
*)

PROCEDURE CalcInitialCounter (A, B: ParamPtr; s: TriadePtr): ParamPtr;
VAR p, q, r, val_tr : TriadePtr;
    a_val, b_val, foo_val: ir.VALUE;
BEGIN
    IF (A^.tag = ir.y_NumConst) & (B^.tag = ir.y_NumConst) THEN
        a_val := A.value;
        b_val := B.value;
        IF fs.OpSize # counterSize THEN
            a_val := Calc.Val(a_val, fs.OpType, fs.OpType,
                                     fs.OpSize, counterSize);
            b_val := Calc.Val(b_val, fs.OpType, fs.OpType,
                                     fs.OpSize, counterSize);
        END;
        foo_val := Calc.Binary(pc.sb_minus,
                               fs.OpType, counterSize,
                               b_val, a_val);
        foo_val := Calc.Val(foo_val, 
                            fs.OpType, ir.t_unsign,
                            fs.OpSize, counterSize);
        ir.MakeParNum(par1,
              Calc.Binary (pc.sb_plus, ir.t_unsign, counterSize,
                           Calc.Binary (pc.sb_div, ir.t_unsign, counterSize,
                                        foo_val,
                                        LongAbsC^.value),
                          Calc.GetCardinal (1, counterSize))
                     );
    ELSE
        p := ir.NewTriadeTInit (2, ir.o_add, ir.y_Variable, fs.OpType, counterSize);
        p^.Position := fs^.Position;
        ir.CopyParam(B, p.Params[0]);
        ir.CopyParam(A, p.Params[1]);

        ir.SetParamReverse(p^.Params^[1], TRUE);
        ir.GenVar (ir.TEMPORARY, p^.Name, p);
        gr.InsertTriade (p, s);

        val_tr := ir.NewTriadeTInit (1, ir.o_val, ir.y_Variable, 
                                    fs.OpType, counterSize);
        val_tr.ResType := ir.t_unsign;
        ir.MakeParVar(val_tr.Params[0], p^.Name);
        ir.GenVar (ir.TEMPORARY, val_tr.Name, val_tr);
        gr.InsertTriade (val_tr, s);

        IF Calc.CompareWithInt (pc.sb_equ, LongAbsC^.value, 1,
                                ir.t_unsign, counterSize)
        THEN
            q := val_tr;
        ELSE
            q := ir.NewTriadeTInit (2, ir.o_div, ir.y_Variable,
                                    ir.t_unsign, counterSize);
            q^.Position := fs^.Position;
            ir.MakeParVar (q^.Params^[0], val_tr^.Name);
            ir.CopyParam  (LongAbsC, q^.Params^[1]);
            ir.GenVar (ir.TEMPORARY, q^.Name, q);
            gr.InsertTriade (q, s);
        END;
        r := ir.NewTriadeTInit (2, ir.o_add, ir.y_Variable, ir.t_unsign, counterSize);
        r^.Position := fs^.Position;
        ir.MakeParVar (r^.Params^[0], q^.Name);
        ir.MakeParNum (r^.Params^[1], Calc.GetCardinal (1, counterSize));
        ir.GenVar (ir.TEMPORARY, r^.Name, r);
        gr.InsertTriade (r, s);

        ir.MakeParVar_Ex(par1, r^.Name, FALSE, FALSE);
    END;
    RETURN par1;
END CalcInitialCounter;

(* -------------------------------------------------------------------------- *)

(*
  Увеличить переменную цикла
*)

PROCEDURE IncrForVariable (forcont: TriadePtr);
VAR p: TriadePtr;
BEGIN
    p := ir.NewTriadeTInit (2, ir.o_add, ir.y_Variable,
                            forcont^.ResType, forcont^.ResSize);
    p^.Position := forcont^.Position;
    INCL (p^.Options, ir.o_Silent);
    ir.CopyParam (forcont^.Params^[0], p^.Params^[0]);
    ir.CopyParam (AbsC,                p^.Params^[1]);
    IF NOT CPositive THEN
        ir.SetParamReverse(p^.Params^[1], TRUE);
    END;
    p^.Name := forcont^.Name;
    ir.SetDef (p^.Name, p);
    gr.InsertTriade (p, forcont);
END IncrForVariable;

(* -------------------------------------------------------------------------- *)

(*
  Сформировать тело цикла FOR по счетчику: t - начальное значение счетчика,
                                           b - первый узел цикла,
                                           e - узел с FORCONT
*)

PROCEDURE UpdateCounterInLoop (initialCounter: ParamPtr);
VAR forcont, f, r, w: TriadePtr;
    j: INT;
    n: Node;
    a: Arc;
BEGIN
    forcont := ir.Nodes^[e].Last;
(*
  Increment for variable
*)
    IncrForVariable (forcont);
(*
  Make fi-function for counter
*)
    f := ir.NewTriadeTInit (2, ir.o_fi, ir.y_Variable, ir.t_unsign, counterSize);
    ir.GenVar (ir.TEMPORARY, f^.Name, f);
    gr.PutTriadeFirst (f, b);
(*
  Insert counter decrement and compare it to 0.
*)
    r := ir.NewTriadeTInit (2, ir.o_add, ir.y_Variable, ir.t_unsign, counterSize);
    ir.GenVar (ir.TEMPORARY, r^.Name, r);
    gr.InsertTriade (r, forcont);
    w := ir.NewTriadeTInit (2, ir.o_eq, ir.y_Nothing, ir.t_unsign, counterSize);
    gr.InsertTriade (w, forcont);
(*
  Fill he parameters
*)
    j := gr.FindInArc (ir.Nodes^[e].OutArcs^[0]);
(*
  j brings control flow here from o_forcont. 
  f is o_fi( j-th parameter is counter, (1-j) is initial counter value).
*)
    ir.CopyParam  (initialCounter, f^.Params^[1-j]);
    ir.MakeParVar (f^.Params^[j], r^.Name);
    ir.MakeParVar (r^.Params^[0], f^.Name);
    ir.MakeParNum (r^.Params^[1], Calc.GetCardinal (1, counterSize));
    ir.SetParamReverse(r^.Params^[1], TRUE);
    ir.MakeParVar (w^.Params^[0], r^.Name);
    ir.MakeParNum (w^.Params^[1], Calc.GetInteger (0, counterSize));
(*
  Swap outgoint arcs
*)
    a := ir.Nodes^[e].OutArcs^[0];
    ir.Nodes^[e].OutArcs^[0] := ir.Nodes^[e].OutArcs^[1];
    ir.Nodes^[e].OutArcs^[1] := a;
    n := ir.Nodes^[e].Out^[0];
    ir.Nodes^[e].Out^[0] := ir.Nodes^[e].Out^[1];
    ir.Nodes^[e].Out^[1] := n;
(*
  Completed. Kill o_forcont now.
*)
    gr.KillTriade (forcont);
END UpdateCounterInLoop;

(* -------------------------------------------------------------------------- *)

(*
  Сформировать тело цикла FOR с увеличением переменной и последующим сравнением
*)

PROCEDURE MakeComparison ();
VAR forcont, w: TriadePtr;
BEGIN
    forcont := ir.Nodes^[e].Last;
    IncrForVariable (forcont);
(*
  Сгенерировать сравнение
*)
    w := ir.NewTriadeTInit (2, ir.o_le, ir.y_Nothing,
                            compareType, forcont^.ResSize);
    gr.InsertTriade (w, forcont);
    w.Position := forcont.Position;
    IF CPositive THEN
        ir.MakeParVar (w^.Params^[0],  forcont^.Name);
        ir.CopyParam  (fs^.Params^[1], w^.Params^[1]);
    ELSE
        ir.MakeParVar (w^.Params^[1],  forcont^.Name);
        ir.CopyParam  (fs^.Params^[1], w^.Params^[0]);
    END;
    gr.KillTriade (forcont);
END MakeComparison;

(* -------------------------------------------------------------------------- *)

(*
  Нужно ли делать цикл по счетчику или с увеличением-сравнением переменной?
*)

PROCEDURE DoCompare (): BOOLEAN;

<* IF TARGET_RISC THEN *>

BEGIN
    RETURN FALSE;
END DoCompare;

<* ELSE *>

VAR possible: BOOLEAN;

    (*
      Проверить, что все использования переменной цикла внутри него -
      это безопасные умножения на "подходящую" константу (для x86 - 2, 4, 8)
      или сложения с целыми константами, результаты которых, в свою очередь,
      такие же.
    *)

    PROCEDURE GoodUsages (v: VarNum; e: TriadePtr): BOOLEAN;
    VAR q: ParamPtr;
        i: INT;
        p: TriadePtr;
    BEGIN
        q := ir.Vars^[v].Use;
        WHILE q <> NIL DO
            p := q^.triade;
            IF p <> e THEN
                IF ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options <> ir.OptionsSet{} THEN
                    RETURN FALSE;
                ELSIF p^.Op = ir.o_add THEN
                    FOR i:=0 TO LEN (p^.Params^)-1 DO
                        IF (i <> q^.paramnumber) &
                           (p^.Params^[i].tag = ir.y_Variable) (* &
                           ir.NodeInLoop (ir.Vars^[p^.Params^[i].name].Def^.NodeNo,
                                          e^.LoopNo) *)
                        THEN
                            RETURN FALSE;
                        END;
                    END;
                    IF NOT GoodUsages (p^.Name, e) THEN
                        RETURN FALSE;
                    END;
                ELSIF (p^.Op <> ir.o_mul) OR (LEN (p^.Params^) <> 2) OR
                      (p^.Params^[1-q^.paramnumber].tag <> ir.y_NumConst) OR
                      NOT opTune.IDB.NeedSR (p^.Params^[1-q^.paramnumber].value,
                                             p^.ResType, p^.ResSize)
                THEN
                    RETURN FALSE;
                END;
            END;
            q := q^.next;
        END;
        RETURN TRUE;
    END GoodUsages;

BEGIN
    IF CPositive THEN
        ir.MakeParNum( par1, Calc.Binary (pc.sb_minus, fs^.ResType, fs^.ResSize,
                                          Calc.MaxValue (fs^.ResType, fs^.ResSize),
                                          AbsC.value)
                     );
        possible := Analysis.AlwaysLeq (fs^.Params^[1], par1, fs^.ResType, fs^.ResSize,
                                        ir.Nodes^[e].Last);
        IF NOT possible & (compareType = ir.t_int) THEN
            compareType := ir.t_unsign;
            IF ir.o_Positive IN fs^.Options THEN
                possible := TRUE;
            ELSE
                ir.MakeParNum( par1, Calc.GetInteger (0, fs^.ResSize) );
                possible := Analysis.AlwaysGeq (fs^.Params^[0], par1, ir.t_int,
                                                fs^.ResSize, ir.Nodes^[e].Last);
            END;
        END;
    ELSE
        ir.MakeParNum(par1, Calc.Binary (pc.sb_plus, fs.ResType, fs^.ResSize,
                                         Calc.MinValue (fs.ResType, fs^.ResSize),
                                         AbsC.value)
                     );
        possible := Analysis.AlwaysGeq (fs^.Params^[1], par1, fs.ResType, fs^.ResSize,
                                        ir.Nodes^[e].Last);
    END;
    IF NOT possible THEN
        RETURN FALSE;
    END;

    IF opAttrs.nooptimize IN opAttrs.COMP_MODE THEN
        RETURN TRUE;
    END;

(*
  А теперь проверить, что все использования переменной цикла внутри него -
  это безопасные умножения на "подходящую" константу (для x86 - 2, 4, 8)
*)
    RETURN NOT GoodUsages (ir.Nodes^[e].Last^.Params^[0].name,
                           ir.Nodes^[e].Last);
END DoCompare;

<* END *>

(* -------------------------------------------------------------------------- *)

(*
  Преобразовать один FOR
*)

PROCEDURE ConvertFor;
VAR n:   Node; -- preheader
    p:   TriadePtr;
    val: ir.VALUE;
BEGIN
    ASSERT (fs^.Params^[2].tag = ir.y_NumConst);
    IF ir.Nodes^[s].Out^[0] = b THEN
        n := gr.SplitArc (ir.Nodes^[s].OutArcs^[0]);
    ELSE
        n := ir.Nodes^[s].Out^[0];
    END;
    gr.FindDominators;
    gr.FindArcsDominators;
(*
  Если цикл выполняется всего один раз, то сделать все проще
*)
    IF (fs^.Params^[0].tag = ir.y_NumConst) &
       (fs^.Params^[1].tag = ir.y_NumConst)
    THEN
        IF CPositive THEN
            val := Calc.Binary (pc.sb_minus, ir.t_ZZ, 0,
                                fs^.Params^[1].value, fs^.Params^[0].value);
        ELSE
            val := Calc.Binary (pc.sb_minus, ir.t_ZZ, 0,
                                fs^.Params^[0].value, fs^.Params^[1].value);
        END;
        val := Calc.Binary (pc.sb_div, ir.t_ZZ, 0,
                            val, AbsC^.value);
        IF Calc.IsZero (val, ir.t_ZZ, 0) THEN
            p := ir.Nodes^[e].Last;

            gr.RepParams( p^.Name, p.Params[0] );
            ir.RemoveVar (p^.Name);

            gr.KillTriade(p);
            gr.MakeGoto (e);
            IF ir.Nodes^[e].NOut = 2 THEN
                gr.KillArc (ir.Nodes^[e].OutArcs^[0]);
            END;
            RETURN;
        END;
    END;
(*
  Может много раз - по полной программе
*)
    compareType := fs^.ResType;
    IF DoCompare () THEN
        MakeComparison ();
    ELSIF CPositive THEN
        UpdateCounterInLoop (
            CalcInitialCounter (CastLimit (fs^.Params^[0], par1, n),
                                CastLimit (fs^.Params^[1], par2, n),
                                ir.Nodes^[n].Last));
    ELSE
        UpdateCounterInLoop (
            CalcInitialCounter (CastLimit (fs^.Params^[1], par1, n),
                                CastLimit (fs^.Params^[0], par2, n),
                                ir.Nodes^[n].Last));
    END;
END ConvertFor;

(* -------------------------------------------------------------------------- *)

(*
  Создать IF вокруг оператора FOR; выкинуть FORSTART
*)

PROCEDURE MakeIf;
VAR q: TriadePtr;
    w: ParamPtr;
BEGIN
    IF ir.Nodes^[s].NOut = 1 THEN
        gr.MakeGoto (s);
        ir.Nodes[s].Last.Position := fs^.Position;
    ELSE
        q := ir.NewTriadeTInit (2, ir.o_le, ir.y_Nothing,
                                fs^.ResType, fs^.ResSize);
        q^.Position := fs^.Position;
        INCL (q^.Options, ir.o_Silent);
        IF CPositive THEN
            ir.CopyParam (fs^.Params^[0], q^.Params^[0]);
            ir.CopyParam (fs^.Params^[1], q^.Params^[1]);
        ELSE
            ir.CopyParam (fs^.Params^[1], q^.Params^[0]);
            ir.CopyParam (fs^.Params^[0], q^.Params^[1]);
        END;
        gr.InsertTriade (q, fs);
    END;
    w := ir.Vars^[fs^.Name].Use;
    WHILE w <> NIL DO
        IF (w^.triade^.Op = ir.o_fi) & w^.position.IsNull () THEN
            w^.position := fs^.Position;
        END;
        w := w^.next;
    END;
    gr.ReplaceByParam (fs, fs^.Params^[0]);
END MakeIf;

(* -------------------------------------------------------------------------- *)

(*
  Попытаться преобразовать FOR:
  - если есть FORCONT (его из-за ошибок в программе может не быть),
    то преобразовать по-честному
  - иначе просто заменить на IF
*)

PROCEDURE DoConversion;
BEGIN
(*
  Занести используемые дальше глобалы
*)
    fs          := ir.Nodes^[s].Last;
    counterSize := CounterSize (fs^.ResSize);
    AbsC        := fs^.Params^[2];
    CPositive   := Calc.CompareWithInt (pc.sb_gtr, AbsC.value, 0,
                                        fs^.ResType, fs^.ResSize);
    AbsC^.value := Calc.Unary (pc.su_abs, fs.ResType, fs^.ResSize,
                               AbsC^.value);
    ir.CopyParam (AbsC, LongAbsC);
    LongAbsC^.value := Calc.Val (LongAbsC^.value, fs^.ResType, ir.t_unsign,
                                                  fs^.ResSize, counterSize);
(*
  Заменить FORSTART на IF
*)
    MakeIf;
(*
    Начиная с s, ищем b
*)
    e := s;
    b := ir.Nodes^[s].Out^[0];
    WHILE ir.Nodes^[b].NIn = 1 DO
        IF ir.Nodes^[b].Last^.Op <> ir.o_goto THEN
            RETURN;
        END;
        e := b;
        b := ir.Nodes^[b].Out^[0];
    END;
    IF ir.Nodes^[b].NIn <> 2 THEN
        RETURN;
    END;
(*
    Нашли - теперь ищем e
*)
    IF ir.Nodes^[b].In^[0] = e THEN
        e := ir.Nodes^[b].In^[1];
    ELSE
        e := ir.Nodes^[b].In^[0];
    END;
    WHILE ir.Nodes^[e].Last^.Op = ir.o_goto DO
        IF (e = b) OR (e = ir.ZERONode) THEN
            RETURN;
        END;
        e := ir.Nodes^[e].In^[0];
    END;
    IF ir.Nodes^[e].Last^.Op <> ir.o_forcont THEN
        RETURN;
    END;
(*
    Все нашли - преобразовать
*)
    ConvertFor;
END DoConversion;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                     Convert FOR and CASE statements                        *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Decompose*;
VAR n: Node;
    z: BitVector;
BEGIN
    gr.FindDominators;
    v := NIL;
    z := BitVect.New (ORD(ir.Nnodes), TRUE);
    FOR n:=ir.ZERONode TO SYSTEM.PRED(ir.Nnodes) DO
        IF NOT ir.Nodes^[n].Alive THEN
            BitVect.Excl (z, ORD(n));
        END;
    END;
    FOR n:=SYSTEM.PRED(ir.Nnodes) TO ir.ZERONode BY -1 DO
        IF ir.Nodes^[n].Alive THEN
            IF ir.Nodes^[n].Last^.Op = ir.o_forstart THEN
                s := n;
                DoConversion;
                gr.FindDominators;
            ELSIF ir.Nodes^[n].Last^.Op = ir.o_case THEN
                IF BitVect.In (z, ORD(n)) THEN
                    ConvertOneCase (ir.Nodes^[n].Last, n);
                    gr.FindDominators;
                END;
            END;
        END;
    END;
    BitVect.Free (z);
    a        := NIL;
    v        := NIL;
    p        := NIL;
    buckets  := NIL;
    AbsC     := NIL;
    fs       := NIL;
END Decompose;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                  Вставить там, где надо, пустые узлы                       *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

PROCEDURE InsertNodes*;
VAR i: ir.Arc;
BEGIN
    IF opAttrs.DOREORDER IN opAttrs.COMP_MODE THEN
      FOR i:=ir.ZEROArc TO SYSTEM.PRED(gr.Narcs) DO
          IF (gr.Arcs^[i].t <> ir.ZERONode) & -- the arc is alive
             (ir.Nodes[gr.Arcs^[i].f].NOut > 1) &
             (ir.Nodes[gr.Arcs^[i].t].NIn > 1)
          THEN
              SYSTEM.EVAL(gr.SplitArc (i));
          END;
      END;
    ELSE
      FOR i:=ir.ZEROArc TO SYSTEM.PRED(gr.Narcs) DO
          IF (gr.Arcs^[i].t <> ir.ZERONode) & -- the arc is alive
             (ir.Nodes^[gr.Arcs^[i].t].First.Op = ir.o_fi) 
          THEN
              SYSTEM.EVAL(gr.SplitArc (i));
          END;
      END;
    END;
END InsertNodes;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                 Move invariant triades out of loop                         *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

VAR Processed,
    BadNodes,
    Writes,
    Body,
    Visited,
    Exits         : BitVector;
    WritesSize    : SET;
    Head,
    Preheader,
    StartBackEdge : Node;
    LoopNo        : ir.Loop;

(* -------------------------------------------------------------------------- *)

(*
  Are parameter invariant in loop?
*)

PROCEDURE InvariantParameter (p: ParamPtr): BOOLEAN;
BEGIN
    RETURN (p^.tag <> ir.y_Variable) OR
           NOT BitVect.In (Body, ORD(ir.Vars^[p^.name].Def^.NodeNo));
END InvariantParameter;

(* -------------------------------------------------------------------------- *)

(*
  Are triade invariant in loop?
*)

PROCEDURE InvariantInLoop (p: TriadePtr): BOOLEAN;
VAR i: INT;
BEGIN
    IF p^.Params <> NIL THEN
        FOR i:=0 TO LEN (p^.Params^)-1 DO
            IF NOT InvariantParameter (p^.Params^[i]) THEN
                RETURN FALSE;
            END;
        END;
    END;
    RETURN TRUE;
END InvariantInLoop;

(* -------------------------------------------------------------------------- *)

(*
  Has multioperation invariant in loop parameters?
*)

PROCEDURE HaveInvariantParameters (p: TriadePtr; VAR n: INT): BOOLEAN;
VAR i: INT;
BEGIN
    IF ir.isMulti IN ir.OpProperties [p^.Op] THEN
        n := 0;
        IF ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options = ir.OptionsSet{} THEN
            FOR i:=0 TO LEN (p^.Params^)-1 DO
                IF InvariantParameter (p^.Params^[i]) THEN
                    INC (n);
                END;
            END;
        ELSE
            i := 0;
            WHILE InvariantParameter (p^.Params^[i]) DO
                INC (i);
            END;
            n := i;
        END;
        RETURN n > 1;
    END;
    RETURN FALSE;
END HaveInvariantParameters;

(* -------------------------------------------------------------------------- *)

(*
  Split partially invariant in loop multioperation
*)

PROCEDURE SplitMultioperation (p: TriadePtr; k: INT);
VAR q: TriadePtr;
    r: ParamArray;
    i, j, l: INT;
BEGIN
    q := ir.NewTriadeLike (p, k);
    ir.GenVar (ir.TEMPORARY, q^.Name, q);
    gr.InsertTriade (q, ir.Nodes^[Preheader].Last);
    r := ir.NewParams (LEN (p^.Params^) - k + 1, p);
    IF InvariantParameter (p^.Params^[0]) THEN
        ir.MakeParVar (r^[0], q^.Name);
        l := 1;
    ELSE
        q^.ResType := opTune.index_ty;
        ir.MakeParVar (r^[LEN(r^)-1], q^.Name);
        l := 0;
    END;
    j := 0;
    IF ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options = ir.OptionsSet{} THEN
        FOR i:=0 TO LEN(p^.Params^)-1 DO
            IF InvariantParameter (p^.Params^[i]) THEN
                ir.CopyParamWithRev (p^.Params^[i], q^.Params^[j]);
                INC (j);
            ELSE
                ir.CopyParamWithRev (p^.Params^[i], r^[l]);
                INC (l);
            END;
            IF p^.Params^[i].tag = ir.y_Variable THEN
                ir.RemoveUse(p^.Params^[i]);
            END;
        END;
    ELSE
        i := 0;
        WHILE InvariantParameter (p^.Params^[i]) DO
            ir.CopyParamWithRev (p^.Params^[i], q^.Params^[j]);
            IF p^.Params^[i].tag = ir.y_Variable THEN
                ir.RemoveUse(p^.Params^[i]);
            END;
            INC (i);
            INC (j);
        END;
        FOR i:=i TO LEN (p^.Params^)-1 DO
            ir.CopyParamWithRev (p^.Params^[i], r^[l]);
            IF p^.Params^[i].tag = ir.y_Variable THEN
                ir.RemoveUse (p^.Params^[i]);
            END;
            INC (l);
        END;
    END;
    p^.Params := r;
END SplitMultioperation;

(* -------------------------------------------------------------------------- *)

(*
  Graph traverse: are there any dangerous operations / memory writes before p?
*)

PROCEDURE CanMoveDangerous (n: Node; p: TriadePtr): BOOLEAN;
VAR i: INT;
    m: Node;
BEGIN

    WHILE p <> NIL DO
        IF (ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options <> ir.OptionsSet{})
           OR (p^.Write <> NIL)
        THEN
            BitVect.Incl (BadNodes, n);
            RETURN FALSE;
        END;
        p := p^.Prev;
    END;

    IF n <> Head THEN
        FOR i:=0 TO ir.Nodes^[n].NIn - 1 DO
            m := ir.Nodes^[n].In^[i];
            IF BitVect.In (BadNodes, ORD(m)) OR BitVect.In (Exits, ORD(m)) THEN
                RETURN FALSE;
            END;
            IF NOT BitVect.In (Processed, ORD(m)) THEN
                BitVect.Incl (Processed, ORD(m));
                IF NOT CanMoveDangerous (m, ir.Nodes^[m].Last) THEN
                    BitVect.Incl (BadNodes, ORD(n));
                    RETURN FALSE;
                END;
            END;
        END;
    END;
    RETURN TRUE;
END CanMoveDangerous;

(* -------------------------------------------------------------------------- *)

(*
  Graph traverse: Anybody reads memory where writes q?
*)

PROCEDURE NobodyReads (n: Node; p, q: TriadePtr): BOOLEAN;
VAR i: INT;
    m: Node;
BEGIN
    WHILE p <> NIL DO
        IF (p^.Read <> NIL) &
           p.IsRead() &
           Optimize.RWDependence (p, q, p^.Read, q^.Write)
        THEN
            RETURN FALSE;
        END;
        p := p^.Prev;
    END;
    IF n <> Head THEN
        FOR i:=0 TO ir.Nodes^[n].NIn - 1 DO
            m := ir.Nodes^[n].In^[i];
            IF BitVect.In (Exits, ORD(m)) THEN
                RETURN FALSE;
            END;
            IF NOT BitVect.In (Visited, ORD(m)) THEN
                BitVect.Incl (Visited, ORD(m));
                IF NOT NobodyReads (m, ir.Nodes^[m].Last, q) THEN
                    RETURN FALSE;
                END;
            END;
        END;
    END;
    RETURN TRUE;
END NobodyReads;

(* -------------------------------------------------------------------------- *)

(*
  Пишет ли кто-нибудь в цикле еще туда же, куда пишет q?
*)

PROCEDURE NobodyWrites (q: TriadePtr): BOOLEAN;
VAR i: ir.TSNode;
    n: Node;
    p: TriadePtr;
BEGIN
    FOR i:=SYSTEM.SUCC(ir.Nodes^[Preheader].TopNumber) TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        IF BitVect.In (Body, ORD(n)) THEN
            p := ir.Nodes^[n].First;
            REPEAT
                IF (p <> q) & (p^.Write <> NIL) &
                   Optimize.RWDependence (p, q, p^.Write, q^.Write)
                THEN
                    RETURN FALSE;
                END;
                p := p^.Next;
            UNTIL p = NIL;
        END;
    END;
    RETURN TRUE;
END NobodyWrites;

(* -------------------------------------------------------------------------- *)

(*
  Can we move triade out of loop?
*)

PROCEDURE DangerousMovePossible (p: TriadePtr): BOOLEAN;
BEGIN
    RETURN NOT BitVect.In (BadNodes, ORD(p^.NodeNo)) &
           gr.Dominates (p^.NodeNo, StartBackEdge) &
           CanMoveDangerous (p^.NodeNo, p^.Prev);
END DangerousMovePossible;

(* -------------------------------------------------------------------------- *)

(*
  Can we move memory acces out of loop?
*)

PROCEDURE MemoryAccessMovable (p: ParamPtr): BOOLEAN;
BEGIN
    RETURN Analysis.AlwaysNeqNil (p, ir.Nodes^[Head].First) OR
           DangerousMovePossible (p^.triade);
END MemoryAccessMovable;

(* -------------------------------------------------------------------------- *)

(*
  Can we move triade out of loop?
*)

PROCEDURE MovePossible (p: TriadePtr): BOOLEAN;
<* IF TARGET_386 THEN *>
VAR  par :ir.ParamPtr;
BEGIN
    par := ir.FirstUse (p^.Name);
    RETURN
         ~((p.Op = ir.o_and) & (par.next = NIL) & (par.triade.Op = ir.o_eq))&
<* ELSE *>
BEGIN
    RETURN
<* END *>
         ((ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options = ir.OptionsSet{})
            OR DangerousMovePossible(p));
END MovePossible;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                         Strength reduction                                 *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

TYPE    MultRec  = RECORD
                        p1, p2, r: ParamPtr;
                        t:         TypeType;
                        s:         SizeType;
                    END;
        MultArray = POINTER TO ARRAY OF MultRec;

CONST   undefinductive = 0;
        inductive      = 1;
        noninductive   = 2;

VAR iv: POINTER TO ARRAY OF SHORTINT;
  ivOk: BOOLEAN;
     m: MultArray;
    nm: INT;

(* -------------------------------------------------------------------------- *)

(*
  Is variable inductive in loop?
*)

PROCEDURE IsIV (v: VarNum): BOOLEAN;
VAR p: TriadePtr;
    i,j: VarNum;
    k: INT;
    iv1:POINTER TO ARRAY OF SHORTINT;
BEGIN
    IF NOT ivOk THEN
        IF (iv = NIL) OR (LEN (iv^) < ir.NVars) THEN
            NEW (iv, ir.NVars + ir.VarNum{ 10});
        END;
        FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO iv^[i] := undefinductive; END;
        ivOk := TRUE;
    END;
    p := ir.Vars^[v].Def;
    IF (p = NIL) OR (iv^[v] = noninductive) THEN
        RETURN FALSE;
    ELSIF iv^[v] = inductive THEN
        RETURN TRUE;
    ELSIF NOT BitVect.In (Body, ORD(p^.NodeNo)) THEN
        iv^[v] := inductive;
        RETURN TRUE;
    ELSIF ((p^.Op <> ir.o_add) & (p^.Op <> ir.o_fi)) OR
          (ir.Nodes^[p^.NodeNo].LoopNo <> LoopNo)
    THEN
        iv^[v] := noninductive;
        RETURN FALSE;
    END;
    iv^[v] := inductive;
    NEW (iv1, LEN(iv^));
    FOR i:=ir.ZEROVarNum TO SYSTEM.PRED(LEN(iv^)) DO iv1^[i] := iv^[i]; END;
    FOR k:=0 TO LEN(p^.Params^)-1 DO
        IF (p^.Params^[k].tag = ir.y_Variable) &
           NOT IsIV (p^.Params^[k].name)
        THEN
            FOR j := ir.ZEROVarNum TO SYSTEM.PRED(LEN(iv^)) DO
                IF (iv1^[j] <> inductive) & (iv^[j] = inductive) THEN
                    iv^[j] := iv1^[j];
                END;
            END;
            iv1 := NIL;
            iv^[v] := noninductive;
            RETURN FALSE;
        END;
    END;
    RETURN TRUE;
END IsIV;

(* -------------------------------------------------------------------------- *)

PROCEDURE NewParamNum(): ParamPtr;
VAR r: ParamPtr;
BEGIN
    ir.NewParamPtr (r);
    ir.MakeParNum(r, Calc.GetInteger(0, 1));
    RETURN r;
END NewParamNum;

PROCEDURE NewParamVar (): ParamPtr;
VAR r: ParamPtr;
    v: ir.VarNum;
BEGIN
    ir.NewParamPtr (r);
    ir.GenVar (ir.TEMPORARY, v, NIL);
    ir.MakeParVar_Ex(r, v, FALSE, FALSE);
    RETURN r;
END NewParamVar;

(* -------------------------------------------------------------------------- *)

(*
  Find multiplication in multiplication table
*)

PROCEDURE FindMult (p1, p2: ParamPtr; t: TypeType; s: SizeType): ParamPtr;
VAR i: INT;
    l: MultArray;
    z1, z2, r: ParamPtr;
BEGIN
    ir.NewParamPtr (z1);                -- Скопировать параметры чтобы
    ir.CopyParam (p1, z1);              -- убрать признак reverse
    IF z1^.tag = ir.y_Variable THEN
        ir.RemoveUse(z1);
    ELSIF z1^.tag = ir.y_Nothing THEN
        RETURN z1;
    END;
    ir.NewParamPtr (z2);
    ir.CopyParam (p2, z2);
    IF z2^.tag = ir.y_Variable THEN
        ir.RemoveUse(z2);
    ELSIF z2^.tag = ir.y_Nothing THEN
        RETURN z2;
    END;
    FOR i:=0 TO nm-1 DO
        IF (m^[i].t = t) & (m^[i].s = s) &
           (ir.EqParams (m^[i].p1, z1, t, s) &
            ir.EqParams (m^[i].p2, z2, t, s) OR
            ir.EqParams (m^[i].p1, z2, t, s) &
            ir.EqParams (m^[i].p2, z1, t, s))
        THEN
            RETURN m^[i].r;
        END;
    END;
    IF (m = NIL) OR (nm = LEN (m^)) THEN
        NEW (l, nm + 10);
        FOR i:=0 TO nm-1 DO
            l^[i] := m^[i];
        END;
        m := l;
    END;
    m^[nm].t  := t;
    m^[nm].s  := s;
    m^[nm].p1 := z1;
    m^[nm].p2 := z2;
    IF (z1^.tag = ir.y_NumConst) & (z2^.tag = ir.y_NumConst) THEN
        r := NewParamNum ();
        r^.value := Calc.Binary (pc.sb_mul, t, s, z1^.value, z2^.value);
    ELSIF (p1^.tag = ir.y_NumConst) THEN
        IF Calc.CompareWithInt (pc.sb_equ, z1^.value, 0, t, s) THEN
            r := z1;
        ELSIF Calc.CompareWithInt (pc.sb_equ, z1^.value, 1, t, s) THEN
            r := z2;
        ELSE
            r := NewParamVar ();
        END;
    ELSIF (p2^.tag = ir.y_NumConst) THEN
        IF Calc.CompareWithInt (pc.sb_equ, z2^.value, 0, t, s) THEN
            r := z2;
        ELSIF Calc.CompareWithInt (pc.sb_equ, z2^.value, 1, t, s) THEN
            r := z1;
        ELSE
            r := NewParamVar ();
        END;
    ELSE
        r := NewParamVar ();
    END;
    m^[nm].r := r;
    INC (nm);
    RETURN r;
END FindMult;

(* -------------------------------------------------------------------------- *)

(*
  Can we SR triade-parameter (it's always safe multiplication)? If so, do it.
*)

PROCEDURE Candidate (p: TriadePtr): TriadePtr;
VAR p1, p2, r: ParamPtr;
    i:         INT;
BEGIN
    p1 := NIL;
    i  := 0;
    LOOP
        IF ((p^.Params^[i].tag <> ir.y_Variable) &
            ((p^.Params^[i].tag <> ir.y_NumConst) OR
             opTune.IDB.NeedSR (p^.Params^[i].value, p^.ResType, p^.ResSize)))
        OR (p^.Params^[i].tag = ir.y_Variable) & IsIV (p^.Params^[i].name)
        THEN
            IF p1 = NIL THEN
                p1 := p^.Params^[i];
            ELSE
                p2 := p^.Params^[i];
                EXIT;
            END;
        END;
        INC (i);
        IF i=LEN(p^.Params^) THEN
            RETURN p^.Next;
        END;
    END;
    r := FindMult (p1, p2, p^.ResType, p^.ResSize);
    IF (LEN(p^.Params^) = 2) & (r^.tag = ir.y_Variable) THEN
        ir.Vars^[r^.name].LocalNo := ir.Vars^[p^.Name].LocalNo;
    END;
    RETURN Optimize.Replace2Pars (p, r, p1^.paramnumber, p2^.paramnumber);
END Candidate;

(* -------------------------------------------------------------------------- *)

(*
  True if p1 dominated by p2
*)

PROCEDURE Later (p1, p2: TriadePtr; n1, n2: Node): BOOLEAN;
VAR p: TriadePtr;
BEGIN
    IF n1 <> n2 THEN
        RETURN gr.Dominates (n2, n1);
    ELSE
        p := ir.Nodes^[n1].First;
        LOOP
            IF p = p1 THEN
                RETURN FALSE;
            ELSIF p = p2 THEN
                RETURN TRUE;
            END;
            p := p^.Next;
        END;
    END;
END Later;

(* -------------------------------------------------------------------------- *)

(*
  Создать триаду умножения и вставить ее до цикла
*)

PROCEDURE MakeInitial (p1, p2: ParamPtr; v: VarNum; t: TypeType; s: SizeType);
VAR p: TriadePtr;
BEGIN
    p := ir.NewTriadeInit (2, ir.o_mul, t, s);
    ir.SetDef (v, p);
    ir.CopyParam (p1, p^.Params^[0]);
    ir.CopyParam (p2, p^.Params^[1]);
    gr.InsertTriade (p, ir.Nodes^[Preheader].Last);
END MakeInitial;

(* -------------------------------------------------------------------------- *)

(*
  Даны две fi-функции в одном узле:
    t1 = fi (q1, q2, ...)
    t2 = fi (s1, s2, ...)
  Надо:
    v = t1 * t2
  Делаем:
    v  = fi (q1*s1, q2*s2, ...)
*)

PROCEDURE UniteFi (t1, t2: TriadePtr; v: VarNum; t: TypeType; s: SizeType);
VAR p: TriadePtr;
    i: INT;
BEGIN
    p := ir.NewTriadeInit (LEN (t1^.Params^), ir.o_fi, t, s);
    ir.SetDef (v, p);
    FOR i:=0 TO LEN(p^.Params^)-1 DO
        ir.CopyParam (FindMult (t1^.Params^[i], t2^.Params^[i], t, s),
                      p^.Params^[i]);
    END;
    gr.InsertTriade (p, t1);
END UniteFi;

(* -------------------------------------------------------------------------- *)

(*
  Дано:
    u = a + b + c + ...
  Нам надо:
    v = u * u
  Делаем:
    v = a*a + a*b + a*c + ... + b*a + b*b + b*c + ...
*)

PROCEDURE MakeSq (u: TriadePtr; v: VarNum; t: TypeType; s: SizeType);
VAR p:          TriadePtr;
    i, j, k, n: INT;
BEGIN
    INCL (u^.Options, ir.o_Silent);             -- to disable warning if that
                                                -- triade will be cleaned later
    n := LEN (u^.Params^);
    p := ir.NewTriadeInit (n * n, ir.o_add, t, s);
    ir.SetDef (v, p);
    k := 0;
    FOR i:=0 TO n-1 DO
        ir.CopyParam (FindMult (u^.Params^[i], u^.Params^[i], t, s),
                      p^.Params^[k]);
        INC (k);
        FOR j:=i+1 TO n-1 DO
            ir.CopyParam (FindMult (u^.Params^[i], u^.Params^[j], t, s),
                          p^.Params^[k]);
            ir.SetParamReverse(p^.Params^[k], u^.Params^[i].reverse <>
                                              u^.Params^[j].reverse);
            INC (k);
            ir.CopyParamWithRev (p^.Params^[k-1], p^.Params^[k]);
            INC (k);
        END;
    END;
    gr.PutAfterTriade (p, u);
END MakeSq;

(* -------------------------------------------------------------------------- *)

(*
  Дано:
    t1 = fi (q1, q2, ...)       или     t1 = q1 + q2 + ...
  Надо:
    v = t1 * p2
  Делаем:
    v = fi (q1*p2, q2*p2, ...)  или     v = q1*p2 + q2*p2 + ...
*)

PROCEDURE MakeMult (t1: TriadePtr; p2: ParamPtr;
                    v: VarNum; t: TypeType; s: SizeType);
VAR p: TriadePtr;
    i: INT;
BEGIN
    INCL (t1^.Options, ir.o_Silent);            -- to disable warning if that
                                                -- triade will be cleaned later
    p := ir.NewTriadeInit (LEN (t1^.Params^), t1^.Op, t, s);
    ir.SetDef (v, p);
    FOR i:=0 TO LEN (t1^.Params^)-1 DO
        ir.CopyParam (FindMult (t1^.Params^[i], p2, t, s), p^.Params^[i]);
        ir.SetParamReverse(p^.Params^[i], t1^.Params^[i].reverse);
    END;
    gr.PutAfterTriade (p, t1);
END MakeMult;

(* -------------------------------------------------------------------------- *)

(*
  Process one multiplication entry
*)

PROCEDURE SRProcess (p1, p2, r: ParamPtr; t: TypeType; s: SizeType);
VAR n1, n2: Node;
    t1, t2: TriadePtr;
    isfi1, isfi2: BOOLEAN;
BEGIN
    IF p1^.tag = ir.y_Variable THEN
        t1    := ir.Vars^[p1^.name].Def;
        n1    := t1^.NodeNo;
        isfi1 := t1^.Op = ir.o_fi;
    ELSE
        t1    := NIL;
        n1    := Preheader;
        isfi1 := FALSE;
    END;
    IF p2^.tag = ir.y_Variable THEN
        t2    := ir.Vars^[p2^.name].Def;
        n2    := t2^.NodeNo;
        isfi2 := t2^.Op = ir.o_fi;
    ELSE
        t2    := NIL;
        n2    := Preheader;
        isfi2 := FALSE;
    END;
    IF NOT BitVect.In (Body, ORD(n1)) & NOT BitVect.In (Body, ORD(n2)) THEN
        MakeInitial (p1, p2, r^.name, t, s);
    ELSIF (n1 = n2) & isfi1 & isfi2 THEN
        UniteFi (t1, t2, r^.name, t, s);
    ELSIF Later (t1, t2, n1, n2) THEN
        IF t1 = t2 THEN
            MakeSq (t1, r^.name, t, s);
        ELSE
            MakeMult (t1, p2, r^.name, t, s);
        END;
    ELSE
        IF t1 = t2 THEN
            MakeSq (t2, r^.name, t, s);
        ELSE
            MakeMult (t2, p1, r^.name, t, s);
        END;
    END;
END SRProcess;

(* -------------------------------------------------------------------------- *)

(*
  SR iteration - repeat until all done
*)

PROCEDURE SRIteration;
VAR i: INT;
BEGIN
    i := 0;
    WHILE i < nm DO
        IF (m^[i].r^.tag = ir.y_Variable) &
           (ir.Vars^[m^[i].r^.name].Def = NIL)
        THEN
            SRProcess (m^[i].p1, m^[i].p2, m^[i].r, m^[i].t, m^[i].s);
        END;
        INC (i);
    END;
END SRIteration;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*               Loops optimizations - main routines                          *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

PROCEDURE CalcWrites;
VAR i: ir.TSNode;
    n: Node;
    p: TriadePtr;
BEGIN
    BitVect.Fill (Writes, FALSE, ORD(ir.NLocals) + 1);
    IF opAttrs.NOALIAS IN opAttrs.COMP_MODE THEN
        WritesSize := {};
    ELSE
        WritesSize := - {};
    END;
    FOR i:=SYSTEM.SUCC(ir.Nodes^[Preheader].TopNumber) TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        IF BitVect.In (Body, ORD(n)) THEN
            p := ir.Nodes^[n].First;
            REPEAT
                IF p^.Write <> NIL THEN
                    BitVect.Union (Writes, p^.Write, Writes);
                    IF (p^.Op = ir.o_call) OR (p^.Op = ir.o_copy) THEN
                        WritesSize := - {};
                    ELSE
                        INCL (WritesSize, ORD(p^.ResSize));
                        IF ORD(p.ResSize) = 8 THEN
                          IF (p.Op = ir.o_store) AND
                             (p.Tag = ir.y_RealVar)
                          THEN
                             IF (ir.Locals[p.Name].Obj # NIL) AND
                                (pc.xot_interface IN ir.Locals[p.Name].Obj.xtags)
                             THEN
                               -- because of interface casts
                               INCL (WritesSize, 4);
                             END;
                          END;
                        END;
                    END;
                END;
                p := p^.Next;
            UNTIL p = NIL;
        END;
    END;
END CalcWrites;

(* -------------------------------------------------------------------------- *)

PROCEDURE MyCheck(p: ir.TriadePtr): BOOLEAN;
BEGIN
 RETURN( NOT (p^.Op IN ir.OpSet{ir.o_call, ir.o_copy}) AND
         NOT (p^.ResSize IN WritesSize) 
       );
END MyCheck;

(*
  Process one node in loop - code motion
*)

PROCEDURE ProcessNode (n: Node);
VAR p, q: TriadePtr;
       k: INT;
BEGIN
    p := ir.Nodes^[n].First;
    WHILE p^.Next <> NIL DO
        q := p^.Next;
        IF gr.Dominates (p^.NodeNo, StartBackEdge) &
           (
            (p^.Op = ir.o_copy) & InvariantParameter (p^.Params^[0]) &
                                  InvariantParameter (p^.Params^[1]) &
                                  InvariantParameter (p^.Params^[2]) &
                                  MemoryAccessMovable (p^.Params^[0]) &
                                  MemoryAccessMovable (p^.Params^[1])
            OR (p^.Op = ir.o_storer) & InvariantParameter (p^.Params^[0]) &
                                       InvariantParameter (p^.Params^[1]) &
                                       MemoryAccessMovable (p^.Params^[0])
            OR (p^.Op = ir.o_store) & InvariantParameter (p^.Params^[0])
           )
        THEN
            BitVect.Fill (Visited, FALSE, ORD(ir.Nnodes));
            IF ((p^.Read = NIL) OR NOT BitVect.Intersecting (p^.Read, Writes)) &
               NobodyReads (p^.NodeNo, p^.Prev, p) & NobodyWrites (p)
            THEN
                gr.DeleteTriade (p);
                gr.InsertTriade (p, ir.Nodes^[Preheader].Last);
                CalcWrites;
            END;
       ELSIF (ir.isMovable IN ir.OpProperties [p^.Op]) &
             (p^.Write = NIL) &
             ( NOT p.IsRead() OR
               ( (ir.o_Constant IN p^.Options) OR
                 (p^.Read=NIL) OR
                 NOT BitVect.Intersecting (p^.Read, Writes)  OR
                 MyCheck(p)
               ) &
               ( (p^.Op <> ir.o_loadr) OR MemoryAccessMovable (p^.Params^[0]) 
               )
             )
        THEN
            IF InvariantInLoop (p) THEN
                IF MovePossible (p) THEN
--
-- we're still arguing if we should move dangerous code out of the loop..
-- so if the compiler behaves strange - uncomment this code 
-- and look at the warnings.. %)
--
--                  IF ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options <> ir.OptionsSet{}
--                  THEN
--                    xiEnv.errors.Warning(p.Position, 4441);
--                  END;
                    gr.DeleteTriade (p);
                    gr.InsertTriade (p, ir.Nodes^[Preheader].Last);
--                    p.Position := xiEnv.null_pos;
                END;
            ELSIF HaveInvariantParameters (p, k) THEN
                IF MovePossible (p) THEN

--                  IF ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options <> ir.OptionsSet{}
--                  THEN
--                    xiEnv.errors.Warning(p.Position, 4441);
--                  END;

                    SplitMultioperation (p, k);
                END;
            END;
        END;
        p := q;
    END;
END ProcessNode;

(* -------------------------------------------------------------------------- *)

(*
  Collect all SR candidates in  one node
*)

PROCEDURE CandidatesInNode (n: Node);
VAR p: TriadePtr;
BEGIN
    p := ir.Nodes^[n].First;
    WHILE p^.Next <> NIL DO
        IF (p^.Op = ir.o_mul) & ir.Ordinal [p^.ResType] &
           (ir.OptionsSet{ir.o_Dangerous, ir.o_Checked} * p^.Options = ir.OptionsSet{})
        THEN
            p := Candidate (p);
        ELSE
            p := p^.Next;
        END;
    END;
END CandidatesInNode;

(* -------------------------------------------------------------------------- *)

(*
  (1) Move invariant in loop code
  (2) Make strength reduction
*)

PROCEDURE ProcessLoop (l: Loop);
VAR
    i,s: ir.TSNode;
    n:    Node;
BEGIN
    LoopNo        := l;
    Preheader     := gr.LoopList^[l].Preheader;
    Body          := gr.LoopList^[l].Body;
    Exits         := gr.LoopList^[l].Exits;
    Head          := gr.Arcs^[gr.LoopList^[l].BackEdge].t;
    StartBackEdge := gr.Arcs^[gr.LoopList^[l].BackEdge].f;
    BitVect.Fill (Processed, FALSE, ORD(ir.Nnodes));
    BitVect.Fill (BadNodes,  FALSE, ORD(ir.Nnodes));
    CalcWrites;
    s :=SYSTEM.SUCC(ir.Nodes^[Preheader].TopNumber);
    FOR i:=s TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        IF ir.Nodes^[n].LoopNo = l THEN
            ProcessNode (n);
        END;
    END;
    ivOk := FALSE;
    nm   := 0;
    FOR i:=s TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        IF ir.Nodes^[n].LoopNo = l THEN
            CandidatesInNode (n);
        END;
    END;
    SRIteration;
END ProcessLoop;

(* -------------------------------------------------------------------------- *)

PROCEDURE OptimizeLoops*;
VAR l: Loop;
BEGIN
    gr.FindLoops;
    gr.FindDominators;
    gr.FindArcsDominators;
    Processed := BitVect.New (ORD(ir.Nnodes),      FALSE);
    Visited   := BitVect.New (ORD(ir.Nnodes),      FALSE);
    BadNodes  := BitVect.New (ORD(ir.Nnodes),      FALSE);
    Writes    := BitVect.New (ORD(ir.NLocals) + 1, FALSE);
    FOR l:=ir.ZEROLoop TO SYSTEM.PRED(gr.NLoops) DO
        ProcessLoop (l);
    END;
    BitVect.Free (Processed);
    BitVect.Free (Visited);
    BitVect.Free (BadNodes);
    BitVect.Free (Writes);
    m := NIL;
END OptimizeLoops;

(* -------------------------------------------------------------------------- *)

(*
  Развернуть один цикл FOR
*)

PROCEDURE UnrollOneFor (n: ir.Node);
VAR p, q, r, s:        TriadePtr;
    m, m1, m2, m3, m4: Node;
    tp:                TypeType;
    sz:                SizeType;
    up:                BOOLEAN;
BEGIN
    m := ir.Nodes^[n].In^[0];
    IF m = n THEN
        m := ir.Nodes^[n].In^[1];
    END;
    LOOP
        s := ir.Nodes^[m].Last;
        IF s^.Op = ir.o_forstart THEN
            EXIT;
        END;
        m := ir.Nodes^[m].In^[0];
    END;
    IF (ir.Nodes^[m].NOut = 2) &
       (ir.Nodes^[ir.Nodes^[m].Out^[1]].First^.Op = ir.o_fi)
    THEN
        RETURN;
    END;

    p  := ir.Nodes^[n].Last;
    tp := p^.ResType;
    sz := p^.ResSize;
    up := Calc.CompareWithInt (pc.sb_equ, p^.Params^[2].value, 1, tp, sz);
    ir.SetSilentMode;
(*
  Прежде всего: присвоить переменной цикла начальное значение,
                посчитать (количество итераций - 1),
                проверить его на четность

*)
    m1 := ir.Nodes^[m].Out^[0];
    IF m1 = n THEN
        m1 := gr.SplitArc (ir.Nodes^[m].OutArcs^[0]);
    END;
    m4 := gr.SplitArc (ir.Nodes^[m1].OutArcs^[0]);      -- для нового forstart
    q  := ir.NewTriadeTInit (1, ir.o_assign, ir.y_RealVar, tp, sz);
    q^.Position := s^.Position;
    q^.Name     := p^.Name;
    ir.CopyParam (s^.Params^[0], q^.Params^[0]);
    gr.PutTriadeFirst (q, m1);
    gr.KillTriade (ir.Nodes^[m1].Last);
    r  := ir.NewTriadeTInit (2, ir.o_add, ir.y_Variable, ir.t_unsign, sz);
    r^.Position := p^.Position;
    IF up THEN
        ir.CopyParam (s^.Params^[1], r^.Params^[0]);
        ir.CopyParam (s^.Params^[0], r^.Params^[1]);
    ELSE
        ir.CopyParam (s^.Params^[0], r^.Params^[0]);
        ir.CopyParam (s^.Params^[1], r^.Params^[1]);
    END;
    ir.SetParamReverse(r^.Params^[1], TRUE);
    ir.GenVar (ir.TEMPORARY, r^.Name, r);
    gr.PutTriadeLast (r, m1);
    q := ir.NewTriadeTInit (1, ir.o_odd, ir.y_Nothing, ir.t_unsign, sz);
    q^.Position := p^.Position;
    ir.MakeParVar (q^.Params^[0], r^.Name);
    gr.PutTriadeLast (q, m1);
    gr.NewArc (m1, m4, FALSE);
(*
  Теперь сформировать: если (количество итераций - 1) четное, то
    - выполнить первую
    - проверить, а не единственная ли она?
*)
    m2 := gr.SplitArc (ir.Nodes^[m1].OutArcs^[1]);
    q := ir.Nodes^[m2].Last;
    gr.CopyTriades (ir.Nodes^[n].First, p, q, ir.UNDEFINED, ir.UNDEFINED);
    gr.KillTriade (q);
    q := ir.NewTriadeTInit (2, ir.o_eq, ir.y_Nothing, ir.t_unsign, sz);
    q^.Position := p^.Position;
    ir.MakeParVar (q^.Params^[0], r^.Name);
    ir.MakeParNum (q^.Params^[1], Calc.NewInteger (0, sz));
    gr.PutTriadeLast (q, m2);
    gr.NewArc (m2, ir.Nodes^[n].Out^[1], FALSE);
    gr.ExchangeArcs (m2);
(*
  Сгенерировать: если не единственная, то увеличить переменную
*)
    m3 := gr.SplitArc (ir.Nodes^[m2].OutArcs^[1]);
    q  := ir.NewTriadeTInit (2, ir.o_add, ir.y_RealVar, tp, sz);
    q^.Position := p^.Position;
    q^.Name     := p^.Name;
    ir.MakeParLocal (q^.Params^[0], p^.Name);
    ir.MakeParNum   (q^.Params^[1], Calc.NewInteger (1, sz));
    ir.SetParamReverse(q^.Params^[1], NOT up);
    gr.PutTriadeFirst (q, m3);
(*
  Теперь сдублировать тело цикла, вставив перед второй копией
  увеличение переменной цикла
*)
    q := ir.NewTriadeTInit (2, ir.o_add, ir.y_Variable, tp, sz);
    q^.Position := p^.Position;
    ir.MakeParLocal (q^.Params^[0], p^.Name);
    ir.MakeParNum   (q^.Params^[1], Calc.NewInteger (1, p^.ResSize));
    ir.SetParamReverse(q^.Params^[1], NOT up);
    ir.GenVar (ir.TEMPORARY, q^.Name, q);
    gr.InsertTriade (q, p);
    gr.CopyTriades (ir.Nodes^[n].First, q, p, p^.Name, q^.Name);
(*
  Теперь вставим новый forstart; если можно, отметим на нем положительность
                                 начального значения
*)
    gr.KillTriade (ir.Nodes^[m4].Last);
    q := ir.NewTriadeLike (s, 3);
    q^.Name := s^.Name;
    ir.MakeParLocal (q^.Params^[0], p^.Name);
    ir.CopyParam    (s^.Params^[1], q^.Params^[1]);
    IF up THEN
        ir.MakeParNum (q^.Params^[2], Calc.NewInteger (2, p^.ResSize));
        p^.Params^[2].value := Calc.NewInteger (2, p^.ResSize);
    ELSE
        ir.MakeParNum (q^.Params^[2], Calc.NewInteger (-2, p^.ResSize));
        p^.Params^[2].value := Calc.NewInteger (-2, p^.ResSize);
    END;
    gr.PutTriadeLast (q, m4);
    IF (tp = ir.t_int) & (s^.Params^[0].tag = ir.y_NumConst) &
       NOT Calc.IsNegative (s^.Params^[0].value, sz)
    THEN
        INCL (q^.Options, ir.o_Positive);
    END;
(*
  Теперь заменим старый forstart if-ом
*)
    IF ir.Nodes^[m].NOut = 2 THEN
        q  := ir.NewTriadeTInit (2, ir.o_le, ir.y_Nothing, tp, sz);
        q^.Position := s^.Position;
        IF up THEN
            ir.CopyParam (s^.Params^[0], q^.Params^[0]);
            ir.CopyParam (s^.Params^[1], q^.Params^[1]);
        ELSE
            ir.CopyParam (s^.Params^[1], q^.Params^[0]);
            ir.CopyParam (s^.Params^[0], q^.Params^[1]);
        END;
        gr.KillTriade (s);
        gr.PutTriadeLast (q, m);
        gr.Arcs^[ir.Nodes^[m].OutArcs^[1]].Original := FALSE;
    ELSE
        gr.KillTriade (s);
        gr.MakeGoto (m);
    END;
    ir.SetNormalMode;
END UnrollOneFor;

(* -------------------------------------------------------------------------- *)

(*
  Хороший ли цикл - можно ли его разворачивать?
  Да, если:
  - не очень много триад
  - нет вызовов
  - нет variables
*)

PROCEDURE CheckTriades (p: TriadePtr): BOOLEAN;
VAR i: INT;
    u: ParamPtr;
BEGIN
    i := 0;
    LOOP
        p := p^.Prev;
        IF p = NIL THEN
            RETURN (i > 0);
        END;
        INC (i);
        IF (i = UnrollLimit) OR (p^.Op = ir.o_call)
             OR (p^.Op = ir.o_fi)
            THEN RETURN FALSE;
        END;
        IF p^.Tag = ir.y_Variable THEN
            u := ir.Vars^[p^.Name].Use;
            WHILE u <> NIL DO
                IF u^.triade^.NodeNo <> p^.NodeNo THEN
                    RETURN FALSE;
                END;
                u := u^.next;
            END;
        END;
    END;
END CheckTriades;

(* -------------------------------------------------------------------------- *)

(*
  Unroll-нуть простые циклы for - вызывается до перевода в SSA-форму
*)

PROCEDURE UnrollFors*;
VAR i: ir.TSNode;
    n: Node;
    p: TriadePtr;
BEGIN
    gr.TopSort;
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        p := ir.Nodes^[n].Last;
        IF (p^.Op = ir.o_forcont) &
           (p^.Tag = ir.y_RealVar) &
           (ir.Nodes^[n].Out^[0] = n) &
           (p^.Params^[2].tag = ir.y_NumConst) &
           (Calc.CompareWithInt (pc.sb_equ, p^.Params^[2].value, 1,
                                 p^.ResType, p^.ResSize) OR
            Calc.CompareWithInt (pc.sb_equ, p^.Params^[2].value, -1,
                                 p^.ResType, p^.ResSize)) &
           CheckTriades (p)
         & (ir.Nodes^[ir.Nodes^[n].Out^[1]].First^.Op <> ir.o_fi)
        THEN
            UnrollOneFor (n);
        END;
    END;
END UnrollFors;

(* -------------------------------------------------------------------------- *)

BEGIN
    ir.NewParamPtr (par1);
    ir.NewParamPtr (par2);
    ir.NewParamPtr (LongAbsC);
END Modify.
