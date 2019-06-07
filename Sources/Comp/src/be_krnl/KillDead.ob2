<* +NOREGVARS *>
MODULE KillDead;

IMPORT ir,
       BitVect,
       Optimize,
       env := xiEnv,
       gr := ControlGraph,
       SYSTEM;

--------------------------------------------------------------------------------

TYPE
    INT         = ir.INT;
    Node        = ir.Node;
    Arc         = ir.Arc;
    TriadePtr   = ir.TriadePtr;
    Local       = ir.Local;
    VarNum      = ir.VarNum;
    NodeArray   = ir.NodeArray;
    BitVector   = BitVect.BitVector;

--------------------------------------------------------------------------------

VAR mked: BitVector;

--------------------------------------------------------------------------------

PROCEDURE Init;
BEGIN
    mked := BitVect.New (ORD(ir.Nnodes), FALSE);
END Init;

--------------------------------------------------------------------------------

PROCEDURE Free;
BEGIN
    BitVect.Free (mked);
END Free;

--------------------------------------------------------------------------------

PROCEDURE FindPreLive;
VAR n: Node;
    p: TriadePtr;
    i: INT;
BEGIN
    FOR n:=ir.ZERONode TO SYSTEM.PRED(ir.Nnodes) DO
        IF ir.Nodes^[n].Alive THEN
            p := ir.Nodes^[n].First;
            WHILE p <> NIL DO
                p^.Options := p^.Options - ir.OptionsSet{ ir.o_Processed, ir.o_Live };
                IF   (ir.isPrelive IN ir.OpProperties [p^.Op])
                   OR
                     (ir.OptionsSet{ ir.o_Checked, ir.o_Dangerous } * p^.Options <> ir.OptionsSet{}) &
                     NOT (ir.o_Removable IN p^.Options)
                   OR
                     (ir.o_IsChecknil IN p.Options)
                THEN
                    INCL (p^.Options, ir.o_Live);
                ELSE
                  IF p^.Params <> NIL THEN
                      FOR i:=0 TO LEN (p^.Params^)-1 DO
                        IF p^.Params[i].tag = ir.y_RealVar THEN
                          IF ir.is_volatile_loc(p^.Params^[i].name) THEN
                            INCL (p^.Options, ir.o_Live);
                          END;
                        END;
                      END;
                  END;
                END;
                p := p^.Next;
            END;
            IF (ir.Nodes^[n].IDom = gr.Stop) &
               (ir.Nodes^[n].Last#NIL)&
               (ir.Nodes^[n].Last^.Op = ir.o_goto)
            THEN
                INCL (ir.Nodes^[n].Last^.Options, ir.o_Live);
            END;
        END;
    END;
END FindPreLive;

--------------------------------------------------------------------------------

PROCEDURE Process (p: TriadePtr);
VAR i: INT;
    n: Node;
    a: NodeArray;
BEGIN
    INCL (p^.Options, ir.o_Processed);
    n := p^.NodeNo;
    IF p^.Op = ir.o_fi THEN
        FOR i:=0 TO LEN (p^.Params^)-1 DO
            INCL (ir.Nodes^[ir.Nodes^[n].In^[i]].Last^.Options, ir.o_Live);
        END;
    END;
    IF p^.Params <> NIL THEN
        FOR i:=0 TO LEN (p^.Params^)-1 DO
            IF p^.Params[i].tag = ir.y_Variable THEN
                INCL (ir.Vars^[p^.Params^[i].name].Def^.Options, ir.o_Live);
            END;
        END;
    END;
    IF NOT BitVect.In (mked, ORD(n)) THEN
        BitVect.Incl (mked, ORD(n));
        a := gr.dfArray^[n];
        FOR i:=0 TO gr.dfLen^[n]-1 DO
            INCL (ir.Nodes^[a^[i]].Last^.Options, ir.o_Live);
        END;
    END;
END Process;

--------------------------------------------------------------------------------

PROCEDURE MarkLive;
VAR o:       ir.TSNode;
    p:       TriadePtr;
    Changed: BOOLEAN;
BEGIN
    FindPreLive;
    REPEAT
        Changed := FALSE;
        FOR o:=SYSTEM.PRED(LEN(ir.Order^)) TO ir.StartOrder BY -1 DO
            p := ir.Nodes^[ir.Order^[o]].Last;
            WHILE p <> NIL DO
                IF p^.Options * ir.OptionsSet{ir.o_Live, ir.o_Processed} = ir.OptionsSet{ir.o_Live} THEN
                    Changed := TRUE;
                    Process (p);
                END;
                p := p^.Prev;
            END;
        END;
    UNTIL NOT Changed;
END MarkLive;

--------------------------------------------------------------------------------

PROCEDURE KillNotMarked;
VAR i: ir.TSNode;
    p: TriadePtr;
    n: Node;
  pos: ir.TPOS;
BEGIN
    FOR i:=ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
        n := ir.Order^[i];
        IF (n <> gr.Stop) THEN
            IF ir.Nodes[n].Last # NIL THEN
              pos := ir.Nodes[n].Last.Position;
            ELSE
              pos := ir.NullPos;
            END;
            p := ir.Nodes^[n].First;
            WHILE p <> NIL DO
                IF ir.o_Live IN p^.Options THEN
                    p := p^.Next;
                ELSE
                    IF p^.Tag = ir.y_Variable THEN
                        ir.RemoveVar (p^.Name);
                    END;
                    IF NOT p^.Position.IsNull () &
                       NOT ir.insideInline &
                       NOT (ir.o_Silent IN p^.Options) &
                       (p^.Op <> ir.o_getpar) &
                       (p^.Op <> ir.o_goto)   &
                       (p^.Op <> ir.o_fi)     &
                       NOT ((p^.Op = ir.o_assign) &
                            (p^.Params^[0].tag = ir.y_Variable) &
                            (p^.Params^[0].next = NIL) &
                            (ir.Vars^[p^.Params^[0].name].Def <> NIL) &
                            (ir.Vars^[p^.Params^[0].name].Def^.Op = ir.o_call) &
                            (ir.Vars^[p^.Params^[0].name].Use = p^.Params^[0]))
                    THEN
                        env.errors.Warning (p^.Position, 900);
                    END;
                    p := gr.KillTriade_Ex (p);
                END;
            END;
            p := ir.Nodes^[n].Last;
            IF (p = NIL) OR
               NOT ((ir.isLast IN ir.OpProperties [p^.Op]) OR
                    (ir.o_NoReturn   IN p.Options)
                   )
            THEN
                WHILE ir.Nodes^[n].NOut <> 0 DO
                    gr.KillArc (ir.Nodes^[n].OutArcs^[0]);
                END;
                gr.MakeGoto (n);
                ir.Nodes[n].Last.Position := pos;
                gr.NewArc (n, ir.Nodes^[n].IDom, FALSE);
            END;
        END;
    END;
END KillNotMarked;

--------------------------------------------------------------------------------

PROCEDURE KillDeadCode*;
BEGIN
    gr.MakeStop;
    gr.FindRDF;
    Init;
    MarkLive;
    KillNotMarked;
    Free;
    gr.KillDead;
    gr.KillStop;
END KillDeadCode;

--------------------------------------------------------------------------------

END KillDead.
