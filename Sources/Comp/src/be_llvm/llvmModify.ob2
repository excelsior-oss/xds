--------------------------------------------------------------------------------
--                          Excelsior LLVM Compiler
--                         (c) 2020, Excelsior, LLC.
--
-- Module:   llvmModify
-- Mission:  Convert FOR statements to simpler parts 
-- Authors:  
-- Created:  24-Aug-2020
--   
-- This is modified "be_krnl/Modify" to work with code that is not in SSA form.      
--------------------------------------------------------------------------------
MODULE llvmModify;

IMPORT BitVect,
       ir,
       gr := ControlGraph,
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

<* IF SOURCE_JAVA THEN *>
     UnrollLimit = 100;              -- Максимальное количество триад в
                                    -- unroll-аемых циклах
<* ELSE *>
     UnrollLimit = 25;              -- Максимальное количество триад в
                                    -- unroll-аемых циклах
<* END *>

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
            IF sz > 4 THEN -- 64bit-wide case
                EXIT;
            END;
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
--        IF needdef AND (default#MAX(Node)) THEN
--            gr.NewArc (n, default, TRUE);
--        END;
        ASSERT(default # MAX(Node));
        gr.NewArc (n, default, TRUE);
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
    IF opd.position.IsNull() THEN
      q^.Position := p^.Position;
    ELSE
      q^.Position := opd.position;
    END;
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
                par2.position := Expr.position;
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
    p := ir.NewTriadeTInit (2, ir.o_add, forcont.Tag,
                            forcont^.ResType, forcont^.ResSize);
    p^.Position := forcont^.Position;
    INCL (p^.Options, ir.o_Silent);
    ir.CopyParam (forcont^.Params^[0], p^.Params^[0]);
    ir.CopyParam (AbsC,                p^.Params^[1]);
    IF NOT CPositive THEN
        ir.SetParamReverse(p^.Params^[1], TRUE);
    END;
    p^.Name := forcont^.Name;
    gr.InsertTriade (p, forcont);
END IncrForVariable;

(* -------------------------------------------------------------------------- *)

(*
  Инициализировать переменную цикла
*)

PROCEDURE InitForVariable (forstart: TriadePtr; forheader: Node);
VAR p: TriadePtr;
BEGIN
  p := ir.NewTriadeTInit(1, ir.o_assign, forstart.Tag,
                         forstart.ResType, forstart.ResSize);
  p.Position := forstart.Position;
  p.Name     := forstart.Name;
  ir.CopyParam (forstart.Params[0], p.Params[0]);
  gr.PutTriadeFirst(p, forheader);                           
END InitForVariable;


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
    
    InitForVariable(fs, n);
    
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
    gr.KillTriade (fs);
--    gr.ReplaceByParam (fs, fs^.Params^[0]);
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
    CPositive   := Calc.CompareWithInt ( pc.sb_gtr, AbsC.value, 0
                                       , ir.t_int, fs^.ResSize );
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
    ConvertFor();
END DoConversion;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                     Convert FOR and CASE statements                        *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Decompose *();
VAR n: Node;
BEGIN
  gr.FindDominators;
  FOR n:=SYSTEM.PRED(ir.Nnodes) TO ir.ZERONode BY -1 DO
    IF ir.Nodes^[n].Alive THEN
      IF ir.Nodes^[n].Last^.Op = ir.o_forstart THEN
        s := n;
        DoConversion;
        gr.FindDominators;
      ELSIF ir.Nodes^[n].Last^.Op = ir.o_case THEN
        ConvertOneCase (ir.Nodes^[n].Last, n);
        gr.FindDominators;
      END;
    END;
  END;
  AbsC     := NIL;
  fs       := NIL;
END Decompose;

(* -------------------------------------------------------------------------- *)
(*                                                                            *)
(*                  Вставить там, где надо, пустые узлы                       *)
(*                                                                            *)
(* -------------------------------------------------------------------------- *)

PROCEDURE InsertNodes *();
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


BEGIN
    ir.NewParamPtr (par1);
    ir.NewParamPtr (par2);
    ir.NewParamPtr (LongAbsC);
END llvmModify.
