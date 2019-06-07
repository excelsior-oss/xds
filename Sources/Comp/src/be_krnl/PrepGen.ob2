MODULE PrepGen;

(*
  Подготовить процедуру к генерации (частично машинно-зависимый модуль):
  - разбить мультиоперации
  - заменить ANDNOT на AND NOT
  - вставить вычитания и унарные минусы
  - разбить вызовы процедур; явно вставить передачу параметров
  - где надо, заменит ret на retfun
  - заменить load/store на loadr/storer
  - заменить короткие COPY на простые пересылки (на процессорах с
    выравниванием придется переделывать)
*)

IMPORT ir,
       gr := ControlGraph,
       Calc,
       BitVect,
       opProcs,
       opTune,
       at := opAttrs,
       CodeDef,
       opt := Options,
       pc := pcK,
       opDef,
       opStd,
       SYSTEM;

TYPE INT        = ir.INT;
     CARD       = SYSTEM.CARD32;
     Node       = ir.Node;
     TriadePtr  = ir.TriadePtr;
     ParamPtr   = ir.ParamPtr;
     VarNum     = ir.VarNum;
     ParamArray = ir.ParamArray;
     SizeType   = ir.SizeType;

TYPE
     PrepGen_IDB     *= POINTER TO PrepGen_IDB_Rec;
     PrepGen_IDB_Rec *= RECORD
                        END;
VAR
     IDB *: PrepGen_IDB;

VAR PARAM_AREA_SIZE* : LONGINT;

--------------------------------------------------------------------------------

(*
  Вставлять ли вместо триады copy простые пересылки?
*)

PROCEDURE(idb : PrepGen_IDB)
         UseSimpleMoves(p: TriadePtr; n: LONGINT): BOOLEAN;
BEGIN
    RETURN n <= 5 * opTune.index_sz;
END UseSimpleMoves;

--------------------------------------------------------------------------------

--<* IF TARGET_SPARC OR TARGET_RISC THEN *>

PROCEDURE SinglePathAndNoDangerous(r, p: TriadePtr): BOOLEAN;
VAR n: ir.Node;
   tr: ir.TriadePtr;
BEGIN
 IF NOT gr.DominatesTriade(r, p) THEN
   RETURN FALSE;
 END;
 n := p.NodeNo;
 tr := p;
 LOOP
   WHILE tr # NIL DO
     IF tr = r THEN
       RETURN TRUE;
     END;
     IF (ir.OptionsSet{ ir.o_Dangerous, ir.o_Checked, ir.o_NoReturn } *
         tr.Options # ir.OptionsSet{}) OR
        (ir.isNoReturn IN ir.OpProperties[ tr.Op ])
     THEN
       RETURN FALSE;
     END;
     tr := tr.Prev;
   END;
   IF ir.Nodes[n].NIn # 1 THEN
     RETURN FALSE;
   END;
   n := ir.Nodes[n].In[0];
   tr := ir.Nodes[n].Last;
   IF tr.Op # ir.o_goto THEN
     RETURN FALSE;
   END;
 END;
END SinglePathAndNoDangerous;

--<* END *>

(*
Вставить триаду или в наиболее раннее место, или перед p
*)
PROCEDURE(idb : PrepGen_IDB) Insert(q,p : TriadePtr);
VAR i:    INT;
    r, s: TriadePtr;
BEGIN
    s := NIL;
    FOR i:=0 TO LEN (q^.Params^)-1 DO
        IF q^.Params^[i].tag = ir.y_Variable THEN
            r := ir.Vars^[q^.Params^[i].name].Def;
            IF (r^.NodeNo = p^.NodeNo)
               OR SinglePathAndNoDangerous(r, p)
            THEN
                IF (s = NIL) OR gr.DominatesTriade (s, r^.Next) THEN
                    s := r^.Next;
                END;
            END;
        END;
    END;
    IF s = NIL THEN
        s := p;
    ELSE
        WHILE (s^.Op = ir.o_fi) OR
              (s^.Op = ir.o_getpar) OR
              (s^.Op = ir.o_base)
        DO
            s := s^.Next;
        END;
<* IF NOT (TARGET_SPARC OR TARGET_RISC) THEN *>
        IF p.NodeNo = s.NodeNo THEN
            LOOP
                IF p = s THEN
                    EXIT;
                END;
                p := p^.Prev;
                IF p^.Op = ir.o_call THEN
                    s := p^.Next;
                    EXIT;
                END;
            END;
        END;
<* END *>
    END;
    gr.InsertTriade (q, s);
END Insert;

PROCEDURE ReplaceMulti*(p: TriadePtr);
VAR i: INT;
    q: TriadePtr;
    v: VarNum;
    w: ParamArray;

BEGIN
(*
  Прежде всего, разобраться с унарным минусом у первого параметра
*)
    IF p^.Params^[0].reverse THEN
        IF LEN (p^.Params^) = 1 THEN
            p^.Op := ir.o_neg;
            ir.SetParamReverse(p^.Params^[0], FALSE);
            RETURN;
        END;
        IF NOT p^.Params^[1].reverse THEN
            ir.SwapParams (p^.Params^[0], p^.Params^[1]);
        ELSE
            q := ir.NewTriadeLike (p, 1);
            q^.Op := ir.o_neg;
            ir.MoveParam (p^.Params^[0], q^.Params^[0]);
            ir.GenVar (ir.TEMPORARY, q^.Name, q);
            ir.MakeParVar (p^.Params^[0], q^.Name);
            IDB.Insert (q, p);
        END;
    END;
(*
  Если параметра 2, то максимум, что надо сделать - заменить o_add на o_sub
*)
    IF LEN (p^.Params^) = 2 THEN
        IF p^.Params^[1].reverse THEN
            p^.Op := ir.o_sub;
            ir.SetParamReverse(p^.Params^[1], FALSE);
        END;
        RETURN;
    END;
(*
  Разбить мультиоперацию
*)
    q := ir.NewTriadeLike (p, 2);
    IF p^.Params^[1].reverse THEN
        q^.Op := ir.o_sub;
    END;
    ir.MoveParam (p^.Params^[0], q^.Params^[0]);
    ir.MoveParam (p^.Params^[1], q^.Params^[1]);
    ir.GenVar (ir.TEMPORARY, q^.Name, q);
    IDB.Insert (q, p);
    v := q^.Name;
    FOR i:=2 TO LEN(p^.Params^)-2 DO
        q := ir.NewTriadeLike (p, 2);
        IF p^.Params^[i].reverse THEN
            q^.Op := ir.o_sub;
        END;
        ir.MakeParVar (q^.Params^[0], v);
        ir.MoveParam (p^.Params^[i], q^.Params^[1]);
        ir.GenVar (ir.TEMPORARY, q^.Name, q);
        IDB.Insert (q, p);
        v := q^.Name;
    END;
    i := LEN (p^.Params^) - 1;
    IF p^.Params^[i].reverse THEN
        p^.Op := ir.o_sub;
    END;
    w := ir.NewParams (2, p);
    ir.MakeParVar (w^[0], v);
    ir.MoveParam  (p^.Params^[i], w^[1]);
    p^.Params := w;
END ReplaceMulti;

--------------------------------------------------------------------------------

PROCEDURE IsChecknilOfVar( p: TriadePtr; q: ParamPtr): BOOLEAN;
BEGIN
  RETURN (p.Op = ir.o_checknil) AND
         (ir.EqParams(p.Params[0], q, p.OpType, p.OpSize));
END IsChecknilOfVar;

PROCEDURE PutParam (q: ParamPtr; p: TriadePtr; isFirst: BOOLEAN): TriadePtr;
VAR s: TriadePtr;
BEGIN
    IF p = NIL THEN
        s := NIL;
        IF q^.tag = ir.y_Variable THEN
            s := ir.Vars^[q^.name].Def;
            IF s^.NodeNo <> q^.triade^.NodeNo THEN
                s := NIL;
            END;
        END;
        p := q^.triade;
        WHILE (p^.Prev <> s) &
              NOT (p^.Prev^.Op IN
                   ir.OpSet{ ir.o_call, ir.o_putpar,
                             ir.o_alloca, ir.o_getpar, ir.o_fi}) &
              NOT IsChecknilOfVar(p.Prev, q)
        DO
            p := p^.Prev;
        END;
    END;
    s := ir.NewTriadeTInit (1, ir.o_putpar, ir.y_Nothing, ir.t_void, 0);
    IF isFirst THEN
      INCL(s.Options, ir.o_FirstPutPar);
    END;
    s^.Prototype := q^.triade^.Prototype;
    s^.Position  := q^.triade^.Position;
    s^.NPar      := VAL (SHORTINT, q^.paramnumber - 1);
    IF (s^.NPar >= opProcs.ProtoList[s^.Prototype].npar) OR
       (opProcs.ProtoList[s^.Prototype].par[s^.NPar].mode = opProcs.pm_seq)
    THEN
        IF (q^.tag = ir.y_RealConst) OR
           (q^.tag = ir.y_Variable) &
           (ir.Vars^[q^.name].Def^.ResType = ir.t_float)
        THEN
            s^.OpType := ir.t_float;
            s^.OpSize := opTune.longreal_sz;
        ELSIF (q^.tag = ir.y_Variable) & (ir.Vars^[q^.name].Def^.ResSize = 8) THEN
            s^.OpType := opTune.seq_item_type;
            s^.OpSize := 8;
        ELSE
            s^.OpType := opTune.seq_item_type;
            s^.OpSize := opTune.seq_item_sz;
        END;
    ELSE
        s^.OpType := opProcs.ProtoList[s^.Prototype].par[s^.NPar].type;
        s^.OpSize := opProcs.ProtoList[s^.Prototype].par[s^.NPar].size;
        IF (at.ir_strict IN at.COMP_MODE) THEN
            IF (q.tag = ir.y_Variable) THEN
               ASSERT( s^.OpSize = ir.Vars[q.name].Def.ResSize, q.name);
               ASSERT(opProcs.ProtoList[s^.Prototype].par[s^.NPar].type
                      = ir.Vars[q.name].Def.ResType, q.name);
            END;
        END;
    END;
    ir.MoveParam (q, s^.Params^[0]);
    gr.InsertTriade (s, p);
    RETURN s;
END PutParam;

--------------------------------------------------------------------------------

PROCEDURE(idb : PrepGen_IDB) ReplaceCall(p : TriadePtr);
VAR i, j: INT;
    w: ParamArray;
    q, r: TriadePtr;

    PROCEDURE CalcAtCall (q: ParamPtr): BOOLEAN;
    BEGIN
        RETURN (q^.tag = ir.y_Variable) &
               (ir.Vars^[q^.name].Def^.NodeNo = p^.NodeNo) &
               (ir.Vars^[q^.name].Def^.Op <> ir.o_fi);
    END CalcAtCall;

BEGIN
    IF LEN (p^.Params^) > 1 THEN
        IF NOT (opProcs.LangByProto (p^.Prototype) IN opt.LangsWithPushingParams_RtoL)
        THEN
            FOR i:=1 TO LEN (p^.Params^)-1 DO
                SYSTEM.EVAL(PutParam (p^.Params^[i], NIL, i=1));
            END;
        ELSE
            IF at.debug IN at.COMP_MODE THEN
                j := LEN (p^.Params^)-1;
                WHILE (j > 0) & NOT CalcAtCall (p^.Params^[j]) DO
                    DEC (j);
                END;
                q := NIL;
                IF j > 0 THEN
                    FOR i:=j TO 1 BY -1 DO
                        r := PutParam (p^.Params^[i], NIL, FALSE);
                        IF i = j THEN
                            q := r;
                        END;
                    END;
                ELSE
                    q := p;
                END;
                FOR i:=LEN (p^.Params^)-1 TO j+1 BY -1 DO
                    SYSTEM.EVAL(PutParam (p^.Params^[i], q, FALSE));
                END;
            ELSE
                FOR i:=LEN (p^.Params^)-1 TO 1 BY -1 DO
                    SYSTEM.EVAL(PutParam (p^.Params^[i], NIL, FALSE));
                END;
            END;
        END;
        w := ir.NewParams (1, p);
        ir.MoveParam  (p^.Params^[0], w^[0]);
        p^.Params := w;
    END;
END ReplaceCall;

--------------------------------------------------------------------------------

PROCEDURE ReplaceLoad (p: TriadePtr);
BEGIN
    p^.Op := ir.o_loadr;
    ir.MakeParAddr( p.Params[0], p.Params[0].name, 0);
END ReplaceLoad;

--------------------------------------------------------------------------------

PROCEDURE ReplaceStore (p: TriadePtr);
VAR w: ParamArray;
BEGIN
    w := ir.NewParams (2, p);
    ir.MakeParAddr (w^[0], p^.Name, 0);
    ir.MoveParam   (p^.Params^[0], w^[1]);
    p^.Op     := ir.o_storer;
    p^.Tag    := ir.y_Nothing;
    p^.Params := w;
END ReplaceStore;

--------------------------------------------------------------------------------

PROCEDURE(idb : PrepGen_IDB) AndNotOp() : ir.Operation;
BEGIN
  RETURN ir.o_and;
END AndNotOp;

PROCEDURE ReplaceAndNot*(p: TriadePtr);
VAR q: TriadePtr;
    cv: ir.VALUE;
    i: INT;
BEGIN
    FOR i:=1 TO LEN (p^.Params^)-1 DO
        IF p^.Params^[i].tag = ir.y_NumConst THEN
            p^.Params^[i].value := Calc.Unary (pc.su_compl,
                                               p^.ResType, p^.ResSize,
                                               p^.Params^[i].value);
        ELSE
            q := ir.NewTriadeTInit (2, ir.o_xor, ir.y_Variable,
                                    p^.ResType, p^.ResSize);
            ir.GenVar (ir.TEMPORARY, q^.Name, q);
            q^.Position := p^.Position;
            ir.CopyParam  (p^.Params^[i], q^.Params^[0]);
            IF p^.ResType = ir.t_int THEN
                ir.MakeParNum (q^.Params^[1], Calc.GetInteger (-1, p^.ResSize));
            ELSE
                cv := Calc.MaxValue( ir.t_unsign, p.ResSize );
                ir.MakeParNum (q^.Params^[1], cv);
            END;
            gr.InsertTriade (q, p);
            IF p^.Params^[i].tag = ir.y_Variable THEN
                ir.RemoveUse (p^.Params^[i]);
            END;
            ir.MakeParVar (p^.Params^[i], q^.Name);
        END;
    END;
    p^.Op := IDB.AndNotOp();
END ReplaceAndNot;

--------------------------------------------------------------------------------

(*
  В r записать p + o
*)

PROCEDURE MakeAddrPar (p, r: ParamPtr; o: LONGINT);
VAR q: TriadePtr;
BEGIN
    IF p^.tag = ir.y_AddrConst THEN
        ir.CopyParam (p, r);
        INC (r^.offset, o);
    ELSIF o = 0 THEN
        ir.CopyParam (p, r);
    ELSE
        q := ir.NewTriadeTInit (2, ir.o_add, ir.y_Variable,
                                ir.t_ref, opTune.addr_sz);
        q^.Position := p^.triade^.Position;
        ir.GenVar (ir.TEMPORARY, q^.Name, q);
        gr.InsertTriade (q, r^.triade);
        ir.CopyParam  (p, q^.Params^[0]);
        ir.MakeParNum (q^.Params^[1], Calc.GetInteger (o, opTune.addr_sz));
        ir.MakeParVar (r, q^.Name);
    END;
END MakeAddrPar;


--------------------------------------------------------------------------------

PROCEDURE MakeLoadR (p: TriadePtr; o: LONGINT; sz: SizeType): VarNum;
VAR r: TriadePtr;
BEGIN
    r := ir.NewTriadeTInit (1, ir.o_loadr, ir.y_Variable, ir.t_unsign, sz);
    r^.Position := p^.Position;
    ir.GenVar (ir.TEMPORARY, r^.Name, r);
    gr.InsertTriade (r, p);
    MakeAddrPar (p^.Params^[0], r^.Params^[0], o);
    r.Read := BitVect.MakeCopy (p^.Read);
    RETURN r^.Name;
END MakeLoadR;

--------------------------------------------------------------------------------

PROCEDURE MakeStoreR (p: TriadePtr; o: LONGINT; sz: SizeType; v: VarNum);
VAR r: TriadePtr;
BEGIN
    r := ir.NewTriadeInit (2, ir.o_storer, ir.t_unsign, sz);
    r^.Position := p^.Position;
    IF ir.o_Parameters IN p^.Options THEN
        INCL (r^.Options, ir.o_Parameters);
    END;
    gr.InsertTriade (r, p);
    MakeAddrPar (p^.Params^[1], r^.Params^[0], o);
    ir.MakeParVar (r^.Params^[1], v);
    r.Write := BitVect.MakeCopy (p^.Write);
END MakeStoreR;

--------------------------------------------------------------------------------

PROCEDURE MakeStoreR_Card (p: TriadePtr; o: LONGINT; sz: SizeType; c: CARD);
VAR r: TriadePtr;
BEGIN
    r := ir.NewTriadeInit (2, ir.o_storer, ir.t_unsign, sz);
    r^.Position := p^.Position;
    gr.InsertTriade (r, p);
    MakeAddrPar (p^.Params^[1], r^.Params^[0], o);
    ir.MakeParNum (r^.Params^[1], Calc.GetCardinal (c, sz));
    r.Write := BitVect.MakeCopy (p^.Write);
END MakeStoreR_Card;

--------------------------------------------------------------------------------

(*
  Заменить o_copy на более простые триады
*)

PROCEDURE ReplaceCopy (p: TriadePtr);
VAR o,n : LONGINT;

    PROCEDURE MoveSize (sz: SizeType);
    BEGIN
        WHILE n >= ORD(sz) DO
            MakeStoreR (p, o, sz, MakeLoadR (p, o, sz));
            INC (o, sz);
            DEC (n, sz);
        END;
    END MoveSize;

    PROCEDURE SetSize (sz: SizeType);
    VAR c: LONGINT;
        b: SYSTEM.BYTE;
        i: INT;
    BEGIN
        WHILE n >= ORD(sz) DO
            c := 0;
            FOR i:=0 TO ORD(sz-1) DO
                b := CodeDef.GetConstByte (p^.Params^[0].name,
                                           p^.Params^[0].offset + o + i);
                INC (c, ASH (VAL (LONGINT, SYSTEM.VAL (SYSTEM.CARD8, b)), i*8));
            END;
            MakeStoreR_Card (p, o, sz, SYSTEM.VAL (CARD, c));
            INC (o, sz);
            DEC (n, sz);
        END;
    END SetSize;

BEGIN
    IF p^.Params^[2].tag <> ir.y_NumConst THEN
        RETURN;
    END;
    n := Calc.ToInteger (p^.Params^[2].value, opTune.index_sz);
    IF IDB.UseSimpleMoves (p, n) THEN
        IF (p^.Params^[0].tag = ir.y_AddrConst) &
           (p^.Params^[0].name < ir.NLocals)
        THEN
            ir.Locals^[p^.Params^[0].name].Addressed := TRUE;
        END;
        IF (p^.Params^[1].tag = ir.y_AddrConst) &
           (p^.Params^[1].name < ir.NLocals)
        THEN
            ir.Locals^[p^.Params^[1].name].Addressed := TRUE;
        END;
        o := 0;
        IF (p^.Params^[0].tag = ir.y_AddrConst) &
           CodeDef.IsGoodConst (p^.Params^[0].name)
        THEN
            SetSize (4);
            SetSize (2);
            SetSize (1);
        ELSE
            MoveSize (4);
            MoveSize (2);
            MoveSize (1);
        END;
        gr.KillTriade (p);
    END;
END ReplaceCopy;

PROCEDURE ReplaceLongBinop(p: ir.TriadePtr; n: ir.INT);
VAR call: ir.TriadePtr;
BEGIN
  call := opDef.RTS_call(p.Position, n, FALSE);
  ir.CopyParam(p.Params[0], call.Params[1]);
  ir.CopyParam(p.Params[1], call.Params[2]);
  ir.SetDef(p.Name, call);
  gr.InsertTriade(call, p);
  gr.KillTriade(p);
END ReplaceLongBinop;


PROCEDURE ReplaceSeqpoint(p: TriadePtr);
BEGIN
  gr.KillTriade(p);
END ReplaceSeqpoint;

--------------------------------------------------------------------------------

PROCEDURE(idb : PrepGen_IDB) HasSpecificProcessing(op : ir.Operation) : BOOLEAN;
BEGIN
  RETURN FALSE;
END HasSpecificProcessing;

PROCEDURE(idb : PrepGen_IDB) SpecificProcessing(p : TriadePtr);
BEGIN
  ASSERT(FALSE);
END SpecificProcessing;

PROCEDURE(idb : PrepGen_IDB) PreparePostProc;
END PreparePostProc;

PROCEDURE Process( pass: PROCEDURE(p:TriadePtr) );
VAR p, q : TriadePtr;
    n: Node;
    i: ir.TSNode;
BEGIN
    FOR i:=ir.StartOrder TO ir.Nnodes-1 DO
        n := ir.Order^[i];
        p := ir.Nodes^[n].First;
        REPEAT
            q := p^.Next;
            pass(p);
            p := q;
        UNTIL p = NIL;
    END;
END Process;

VAR AllocaCount* : INTEGER; -- number of o_alloca triades in procedure
PROCEDURE PassOne(p: TriadePtr);
BEGIN
    IF IDB.HasSpecificProcessing(p.Op) THEN
    ELSE
        CASE p^.Op OF
        | ir.o_alloca:  INC(AllocaCount);
        | ir.o_load:    ReplaceLoad  (p);
        | ir.o_store:   ReplaceStore (p);
        | ir.o_add,
          ir.o_mul,
          ir.o_and,
          ir.o_or,
          ir.o_xor:     ReplaceMulti (p);
      <* IF NOT TARGET_RISC THEN*>
        | ir.o_andnot:  ReplaceAndNot (p);
                        ReplaceMulti  (p);
      <* END *>
        | ir.o_ret:     IF p^.Params <> NIL THEN p^.Op := ir.o_retfun; END;
        | ir.o_copy:    IF ~at.fastcomp THEN ReplaceCopy (p); END;
        | ir.o_seqpoint: ReplaceSeqpoint(p);
        | ELSE
        END;
    END;
END PassOne;

PROCEDURE PassTwo(p: TriadePtr);
BEGIN
    IF IDB.HasSpecificProcessing(p.Op) THEN
    ELSE
        IF (p.ResSize = 8) AND
           (p.ResType IN ir.TypeTypeSet{ir.t_int, ir.t_unsign})
        THEN
            CASE p^.Op OF
            | ir.o_mul: ReplaceLongBinop(p, opStd.X2J_MUL64);
            | ir.o_div, ir.o_dvd:
            -- o_div(t_int) must be rewritten (issue XDS-31)
                 CASE p.ResType OF
                 |ir.t_int:    ReplaceLongBinop(p, opStd.X2J_DIV64);
                 |ir.t_unsign: ReplaceLongBinop(p, opStd.X2J_UDIV64);
                 END;
            | ir.o_rem, ir.o_mod:
            -- o_mod(t_int) must be rewritten (issue XDS-31)
                 CASE p.ResType OF
                 |ir.t_int:    ReplaceLongBinop(p, opStd.X2J_REM64);
                 |ir.t_unsign: ReplaceLongBinop(p, opStd.X2J_UREM64);
                 END;
            | ir.o_shl: ReplaceLongBinop(p, opStd.X2J_SHL64);
            | ir.o_shr: ReplaceLongBinop(p, opStd.X2J_USHR64);
            | ir.o_sar: ReplaceLongBinop(p, opStd.X2J_SHR64);
            | ir.o_ror,
              ir.o_rol: ASSERT(FALSE);
            ELSE
            END;
        END;
    END;
END PassTwo;

PROCEDURE PassThree(p: TriadePtr);
BEGIN
    IF IDB.HasSpecificProcessing(p.Op) THEN
        IDB.SpecificProcessing(p);
    ELSE
        CASE p^.Op OF
        | ir.o_call: IDB.ReplaceCall (p);
        ELSE
        END;
    END;
END PassThree;

PROCEDURE Prepare*;
BEGIN
    PARAM_AREA_SIZE := 0;
    AllocaCount := 0;
    Process(PassOne);
    Process(PassTwo);
    Process(PassThree);
    IDB.PreparePostProc;
END Prepare;

BEGIN
  NEW(IDB);
END PrepGen.
