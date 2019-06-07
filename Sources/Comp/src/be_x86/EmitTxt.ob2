MODULE EmitTxt;

IMPORT Emit,
       BitVect,
       ir,
       CodeDef,
       FormStr,
       Calc,
       SYSTEM,
       opTune,
       xiEnv,
       at := opAttrs,
       prc := opProcs,
       pcK
;

--------------------------------------------------------------------------------

TYPE
    INT         = ir.INT;
    AddrMode    = Emit.AddrMode;
    VALUE       = pcK.VALUE;
    OffsRecPtr  = Emit.OffsRecPtr;
    ProcRecPtr  = Emit.ProcRecPtr;
    SelfTxtPtr *= POINTER TO SelfTxtRec;
    SelfTxtRec *= RECORD (Emit.SelfRec)
                  END;

--------------------------------------------------------------------------------
VAR
  buf: ARRAY 1024 OF CHAR;

PROCEDURE out;
BEGIN
  CodeDef.GenInstr(buf);
  buf[0] := 0X;
END out;

PROCEDURE print (fmt-: ARRAY OF CHAR; SEQ arg: SYSTEM.BYTE);
BEGIN
  FormStr.append(buf, fmt, arg);
END print;

--------------------------------------------------------------------------------

VAR sg: CodeDef.CODE_SEGM;          -- Текущий генерируемый сегмент

--------------------------------------------------------------------------------

PROCEDURE PrintReg (r: INT; s: INT);
BEGIN
    CASE s OF
    | 1:
        CASE r OF
        | Emit.AL:              print ("al");
        | Emit.BL:              print ("bl");
        | Emit.CL:              print ("cl");
        | Emit.DL:              print ("dl");
        | Emit.AH:              print ("ah");
        | Emit.BH:              print ("bh");
        | Emit.CH:              print ("ch");
        | Emit.DH:              print ("dh");
        END;
    | 2:
        CASE r OF
        | Emit.AX:              print ("ax");
        | Emit.BX:              print ("bx");
        | Emit.CX:              print ("cx");
        | Emit.DX:              print ("dx");
        | Emit.SP:              print ("sp");
        | Emit.BP:              print ("bp");
        | Emit.SI:              print ("si");
        | Emit.DI:              print ("di");
        END;
    | 4:
        CASE r OF
        | Emit.AX:              print ("eax");
        | Emit.BX:              print ("ebx");
        | Emit.CX:              print ("ecx");
        | Emit.DX:              print ("edx");
        | Emit.SP:              print ("esp");
        | Emit.BP:              print ("ebp");
        | Emit.SI:              print ("esi");
        | Emit.DI:              print ("edi");
        END;
    END;
END PrintReg;

--------------------------------------------------------------------------------

PROCEDURE PrintLocal (l: ir.Local);
BEGIN
    IF ir.Locals[l].Name <> NIL THEN
        print ("_");
        print (ir.Locals [l].Name^);
    ELSE
        print ("_tmp%d", l);
    END;
END PrintLocal;

--------------------------------------------------------------------------------

PROCEDURE PrintProcName (p: INTEGER);
VAR name: ir.NameType;
BEGIN
  name := prc.ProcName (p);
  print ("_");
  print (name^);
END PrintProcName;

--------------------------------------------------------------------------------

PROCEDURE PrintMem (a-: AddrMode; s: SHORTINT);
VAR b:  BOOLEAN;
    o:  INT;
    op: OffsRecPtr;
    pp: ProcRecPtr;
BEGIN
    CASE s OF
    | MIN (SHORTINT):
    | 1:                print ("byte ptr ");
    | 2:                print ("word ptr ");
    | 4:                print ("dword ptr ");
    | 8:                print ("qword ptr ");
    | 10, 12:           print ("tword ptr ");
    END;
    b := FALSE;
    o := a.offs;
    IF a.local <> ir.UNDEFINED THEN
        IF ir.IsExternal (a.local) THEN
            PrintLocal (a.local);
            b := TRUE;
        ELSE
            INC (o, ir.Locals^[a.local].Offset + Emit.LOCALS_OFFSET);
            IF Emit.BASE_REG = Emit.ESP THEN
                INC (o, Emit.PUSH_SIZE);
            END;
        END;
    END;
    op := a.l;
    WHILE op <> NIL DO
        IF ir.IsExternal (op^.name) THEN
            IF b THEN
                print ("+");
            END;
            PrintLocal (op^.name);
            b := TRUE;
        ELSE
            INC (o, ir.Locals^[op^.name].Offset + Emit.LOCALS_OFFSET);
            IF Emit.BASE_REG = Emit.ESP THEN
                INC (o, Emit.PUSH_SIZE);
            END;
        END;
        op := op^.next;
    END;
    IF a.proc <> ir.UNDEFINED THEN
        IF b THEN
            print ("+");
        END;
        PrintProcName (a.proc);
        b := TRUE;
    END;
    pp := a.p;
    WHILE pp <> NIL DO
        IF b THEN
            print ("+");
        END;
        PrintProcName (pp^.name);
        b := TRUE;
        pp := pp^.next;
    END;
    IF o <> 0 THEN
        IF o > 0 THEN
            IF b THEN
                print ("+");
            END;
        END;
        print ("%d", o);
    END;
    IF (a.r1.r <> Emit.UNDEF_REG) OR (a.r2.r <> Emit.UNDEF_REG) THEN
        b := FALSE;
        print ("[");
        IF a.r1.r <> Emit.UNDEF_REG THEN
            PrintReg (a.r1.r, 4);
            b := TRUE;
        END;
        IF a.r2.r <> Emit.UNDEF_REG THEN
            IF b THEN
                print ("+");
            END;
            PrintReg (a.r2.r, 4);
            CASE a.scale OF
            | Emit.x1:   print ("*1");
            | Emit.x2:   print ("*2");
            | Emit.x4:   print ("*4");
            | Emit.x8:   print ("*8");
            END;
        END;
        print ("]");
    END;
END PrintMem;

--------------------------------------------------------------------------------

PROCEDURE PrintVal (v: VALUE);
VAR s: ARRAY 200 OF CHAR;
BEGIN
    v.value_to_str (s, 2);
    print (s);
END PrintVal;

--------------------------------------------------------------------------------

PROCEDURE PrintOp (op: INT);
BEGIN
    CASE op OF
    | Emit.TTT_add:     print ("\tadd\t");
    | Emit.TTT_sub:     print ("\tsub\t");
    | Emit.TTT_and:     print ("\tand\t");
    | Emit.TTT_or:      print ("\tor\t");
    | Emit.TTT_xor:     print ("\txor\t");
    | Emit.TTT_mul:     print ("\timul\t");
    | Emit.TTT_cmp:     print ("\tcmp\t");
    END;
END PrintOp;

--------------------------------------------------------------------------------
--
--      Вход в линейный участок, координаты, etc...
--
--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) EnterNode* (n: ir.Node; seg: CodeDef.CODE_SEGM);
BEGIN
    buf[0] := 0X;
    sg := seg;
    CodeDef.set_segm (seg);
    Emit.PUSH_SIZE  := 0;
    Emit.FLOAT_SIZE := 0;
    print ("Node%d:", n);
    out;
END EnterNode;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) SetSegment* (n: ir.Node; seg: CodeDef.CODE_SEGM);
BEGIN
    sg := seg;
    CodeDef.set_segm (seg);
    print ("Set node %d:", n);
    out;
END SetSegment;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) EmptySegment* (n: ir.Node;
                                            sg: CodeDef.CODE_SEGM): BOOLEAN;
BEGIN
    RETURN sg.code_len = 0;
END EmptySegment;

--------------------------------------------------------------------------------
PROCEDURE (self: SelfTxtPtr) AddPosition* (pos-: ir.TPOS);
(*
VAR fname: xiEnv.String;
    line, col: LONGINT;
*)
BEGIN
(*
    IF NOT pos.IsNull () THEN
        pos.unpack (fname, line, col);
        print ("; line %d col %d", line, col);
	out;
    END;
*)
    IF NOT pos.IsNull () THEN
        CodeDef.add_pos (sg.code_len, pos);
    END;
END AddPosition;

--------------------------------------------------------------------------------
--
--      Метки
--
--------------------------------------------------------------------------------

VAR lab: Emit.LABEL;

PROCEDURE (self: SelfTxtPtr) NewLabel* (VAR x: Emit.LABEL);
BEGIN
    x := lab;
    INC (lab);
END NewLabel;

PROCEDURE (self: SelfTxtPtr) SetLabel* (x: Emit.LABEL);
BEGIN
    print ('L%d:', x); out;
END SetLabel;

PROCEDURE (self: SelfTxtPtr) InsertLabel* (VAR x: Emit.LABEL);
BEGIN
    print ('L%d:', x);
    CodeDef.InsInstr (buf);
    buf[0] := 0X;
    INC (lab);
END InsertLabel;

PROCEDURE (self: SelfTxtPtr) DwLabel* (x: Emit.LABEL);
BEGIN
    print ("\t.long\tL%d", x); out;
END DwLabel;

--------------------------------------------------------------------------------
--
--      Собственно генератор команд
--
--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveR_R* (d, s: SHORTINT; sz: SHORTINT);
BEGIN
    print ("\tmov\t");
    PrintReg   (d, sz);
    print (", ");
    PrintReg   (s, sz);
    out;
END GenMoveR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveR_M* (r: SHORTINT; a-: AddrMode; sz: SHORTINT);
BEGIN
    print ("\tmov\t");
    PrintReg   (r, sz);
    print (", ");
    PrintMem   (a, sz);
    out;
END GenMoveR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveR_INum* (r: SHORTINT; v: LONGINT; sz: SHORTINT);
BEGIN
    print ("\tmov\t");
    PrintReg   (r, sz);
    print (", %d", v);
    out;
END GenMoveR_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveR_Iglobal* (r: SHORTINT; a-: AddrMode; sz: SHORTINT);
BEGIN
    print ("\tmov\t");
    PrintReg   (r, sz);
    print (", offset ");
    PrintMem   (a, MIN (SHORTINT));
    out;
END GenMoveR_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveM_R* (a-: AddrMode; r: SHORTINT; sz: SHORTINT);
BEGIN
    print ("\tmov\t");
    PrintMem   (a, sz);
    print (", ");
    PrintReg   (r, sz);
    out;
END GenMoveM_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveM_INum* (a-: AddrMode; v: LONGINT; sz: SHORTINT);
BEGIN
    print ("\tmov\t");
    PrintMem   (a, sz);
    print (", %d", v);
    out;
END GenMoveM_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveM_Iglobal* (a-, ag-: AddrMode; sz: SHORTINT);
BEGIN
    print ("\tmov\t");
    PrintMem   (a, sz);
    print (", offset ");
    PrintMem   (ag, MIN (SHORTINT));
    out;
END GenMoveM_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenXchgR_R* (d, s: SHORTINT; sz: SHORTINT);
BEGIN
    print ("\txchg\t");
    PrintReg   (d, sz);
    print (", ");
    PrintReg   (s, sz);
    out;
END GenXchgR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenXchgR_M* (r: SHORTINT; a-: AddrMode; sz: SHORTINT);
BEGIN
    print ("\txchg\t");
    PrintReg   (r, sz);
    print (", ");
    PrintMem   (a, sz);
    out;
END GenXchgR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenPush_R* (r: SHORTINT);
BEGIN
    print ("\tpush\t");
    PrintReg   (r, 4);
    out;
END GenPush_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenPush_M* (a-: AddrMode);
BEGIN
    print ("\tpush\t");
    PrintMem   (a, 4);
    out;
END GenPush_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenPush_INum* (v: LONGINT; sz: SHORTINT);
BEGIN
    print ("\tpush\t%d", v);
    out;
END GenPush_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenPush_Iglobal* (a-: AddrMode);
BEGIN
    print ("\tpush\toffset ");
    PrintMem   (a, MIN (SHORTINT));
    out;
END GenPush_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenPop_R* (r: SHORTINT);
BEGIN
    print ("\tpop\t");
    PrintReg   (r, 4);
    out;
END GenPop_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenOpR_R* (TTT: LONGINT; d, s: SHORTINT;
                                        sz: SHORTINT);
BEGIN
    PrintOp    (TTT);
    PrintReg   (d, sz);
    print (", ");
    PrintReg   (s, sz);
    out;
END GenOpR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenOpR_M* (TTT: LONGINT; r: SHORTINT;
                                        a-: AddrMode; sz: SHORTINT);
BEGIN
    PrintOp    (TTT);
    PrintReg   (r, sz);
    print (", ");
    PrintMem   (a, sz);
    out;
END GenOpR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenOpR_INum* (TTT: LONGINT; r: SHORTINT;
                                           v: LONGINT; sz: SHORTINT);
BEGIN
    PrintOp    (TTT);
    PrintReg   (r, sz);
    print (", %d", v);
    out;
END GenOpR_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenOpR_Iglobal* (TTT: LONGINT; r: SHORTINT;
                                              a-: AddrMode; sz: SHORTINT);
BEGIN
    PrintOp    (TTT);
    PrintReg   (r, sz);
    print (", offset ");
    PrintMem   (a, MIN (SHORTINT));
    out;
END GenOpR_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenOpM_R* (TTT: LONGINT; a-: AddrMode;
                                        r: SHORTINT; sz: SHORTINT);
BEGIN
    PrintOp    (TTT);
    PrintMem   (a, sz);
    print (", ");
    PrintReg   (r, sz);
    out;
END GenOpM_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenOpM_INum* (TTT: LONGINT; a-: AddrMode;
                                           v: LONGINT; sz: SHORTINT);
BEGIN
    PrintOp    (TTT);
    PrintMem   (a, sz);
    print (", %d", v);
    out;
END GenOpM_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenOpM_Iglobal* (TTT: LONGINT; a-, ag-: AddrMode;
                                              sz: SHORTINT);
BEGIN
    PrintOp    (TTT);
    PrintMem   (a, sz);
    print (", offset ");
    PrintMem   (ag, MIN (SHORTINT));
    out;
END GenOpM_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenDiv_R* (r: SHORTINT; sz: SHORTINT;
                                        signed: BOOLEAN);
BEGIN
    IF signed THEN
        print ("\tidiv\t");
    ELSE
        print ("\tdiv\t");
    END;
    PrintReg   (r, sz);
    out;
END GenDiv_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenDiv_M* (a-: AddrMode; sz: SHORTINT;
                                        signed: BOOLEAN);
BEGIN
    IF signed THEN
        print ("\tidiv\t");
    ELSE
        print ("\tdiv\t");
    END;
    PrintMem   (a, sz);
    out;
END GenDiv_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenDiv_INum* (v: LONGINT; sz: SHORTINT;
                                           signed: BOOLEAN);
BEGIN
    IF signed THEN
        print ("\tidiv\t");
    ELSE
        print ("\tdiv\t");
    END;
    print ("='%d'", v);
    out;
END GenDiv_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMul_R* (r: SHORTINT; sz: SHORTINT;
                                        signed: BOOLEAN);
BEGIN
    IF signed THEN
        print ("\timul\t");
    ELSE
        print ("\tmul\t");
    END;
    PrintReg   (r, sz);
    out;
END GenMul_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMul_M* (a-: AddrMode; sz: SHORTINT;
                                        signed: BOOLEAN);
BEGIN
    IF signed THEN
        print ("\timul\t");
    ELSE
        print ("\tmul\t");
    END;
    PrintMem   (a, sz);
    out;
END GenMul_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMul_INum* (v: LONGINT; sz: SHORTINT;
                                           signed: BOOLEAN);
BEGIN
    IF signed THEN
        print ("\timul\t");
    ELSE
        print ("\tmul\t");
    END;
    print ("='%d'", v);
    out;
END GenMul_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenIMulR_RC* (d, s: SHORTINT; v: LONGINT;
                                           sz: SHORTINT);
BEGIN
    print ("\timul\t");
    PrintReg   (d, sz);
    print (", ");
    PrintReg   (s, sz);
    print (", %d", v);
    out;
END GenIMulR_RC;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenIMulR_MC* (r: SHORTINT; a-: AddrMode;
                                           v: LONGINT; sz: SHORTINT);
BEGIN
    print ("\timul\t");
    PrintReg   (r, sz);
    print (", ");
    PrintMem   (a, sz);
    print (", %d", v);
    out;
END GenIMulR_MC;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenTestR_R* (r1, r2: SHORTINT; sz: SHORTINT);
BEGIN
    print ("\ttest\t");
    PrintReg   (r1, sz);
    print (", ");
    PrintReg   (r2, sz);
    out;
END GenTestR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenTestR_INum* (r: SHORTINT; v: LONGINT; sz: SHORTINT);
BEGIN
    print ("\ttest\t");
    PrintReg   (r, sz);
    print (", %d", v);
    out;
END GenTestR_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenTestM_INum* (a-: AddrMode; v: LONGINT; sz: SHORTINT);
BEGIN
    print ("\ttest\t");
    PrintMem   (a, sz);
    print (", %d", v);
    out;
END GenTestM_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenLEA* (r: SHORTINT; a-: AddrMode);
BEGIN
    print ("\tlea\t");
    PrintReg   (r, 4);
    print (", ");
    PrintMem   (a, MIN (SHORTINT));
    out;
END GenLEA;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMovesxR_M* (r: SHORTINT; a-: AddrMode; rsz, sz: SHORTINT);
BEGIN
    print ("\tmovsx\t");
    PrintReg   (r, rsz);
    print (", ");
    PrintMem   (a, sz);
    out;
END GenMovesxR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMovesxR_R* (d, s: SHORTINT; rsz, sz: SHORTINT);
BEGIN
    print ("\tmovsx\t");
    PrintReg   (d, rsz);
    print (", ");
    PrintReg   (s, sz);
    out;
END GenMovesxR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMovezxR_M* (r: SHORTINT; a-: AddrMode; rsz, sz: SHORTINT);
BEGIN
    print ("\tmovzx\t");
    PrintReg   (r, rsz);
    print (", ");
    PrintMem   (a, sz);
    out;
END GenMovezxR_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMovezxR_R* (d, s: SHORTINT; rsz, sz: SHORTINT);
BEGIN
    print ("\tmovzx\t");
    PrintReg   (d, rsz);
    print (", ");
    PrintReg   (s, sz);
    out;
END GenMovezxR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenOpR* (TTT: LONGINT; r: SHORTINT; sz: SHORTINT);
BEGIN
    CASE TTT OF
    | Emit.TTT_inc: print ("\tinc\t");
    | Emit.TTT_dec: print ("\tdec\t");
    | Emit.TTT_not: print ("\tnot\t");
    | Emit.TTT_neg: print ("\tneg\t");
    END;
    PrintReg   (r, sz);
    out;
END GenOpR;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenOpM* (TTT: LONGINT; a-: AddrMode; sz: SHORTINT);
BEGIN
    CASE TTT OF
    | Emit.TTT_inc: print ("\tinc\t");
    | Emit.TTT_dec: print ("\tdec\t");
    | Emit.TTT_not: print ("\tnot\t");
    | Emit.TTT_neg: print ("\tneg\t");
    END;
    PrintMem   (a, sz);
    out;
END GenOpM;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenSetC_R* (r: SHORTINT; c: SHORTINT);
BEGIN
    CASE c OF
    | Emit.JO:  print ("\tseto\t");
    | Emit.JNO: print ("\tsetno\t");
    | Emit.JB:  print ("\tsetb\t");
    | Emit.JNC: print ("\tsetnc\t");
    | Emit.JE:  print ("\tsete\t");
    | Emit.JNE: print ("\tsetne\t");
    | Emit.JBE: print ("\tsetbe\t");
    | Emit.JA:  print ("\tseta\t");
    | Emit.JS:  print ("\tsets\t");
    | Emit.JNS: print ("\tsetns\t");
    | Emit.JPE: print ("\tsetpe\t");
    | Emit.JPO: print ("\tsetpo\t");
    | Emit.JL:  print ("\tsetl\t");
    | Emit.JGE: print ("\tsetge\t");
    | Emit.JLE: print ("\tsetle\t");
    | Emit.JG:  print ("\tsetge\t");
    END;
    PrintReg   (r, 1);
    out;
END GenSetC_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenSetC_M* (a-: AddrMode; c: SHORTINT);
BEGIN
    CASE c OF
    | Emit.JO:  print ("\tseto\t");
    | Emit.JNO: print ("\tsetno\t");
    | Emit.JB:  print ("\tsetb\t");
    | Emit.JNC: print ("\tsetnc\t");
    | Emit.JE:  print ("\tsete\t");
    | Emit.JNE: print ("\tsetne\t");
    | Emit.JBE: print ("\tsetbe\t");
    | Emit.JA:  print ("\tseta\t");
    | Emit.JS:  print ("\tsets\t");
    | Emit.JNS: print ("\tsetns\t");
    | Emit.JPE: print ("\tsetpe\t");
    | Emit.JPO: print ("\tsetpo\t");
    | Emit.JL:  print ("\tsetl\t");
    | Emit.JGE: print ("\tsetge\t");
    | Emit.JLE: print ("\tsetle\t");
    | Emit.JG:  print ("\tsetg\t");
    END;
    PrintMem   (a, 1);
    out;
END GenSetC_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenShiftR_R* (TTT: LONGINT; r: SHORTINT;
                                           sz: SHORTINT);
BEGIN
    CASE TTT OF
    | Emit.TTT_rol: print ("\trol\t");
    | Emit.TTT_ror: print ("\tror\t");
    | Emit.TTT_shl: print ("\tshl\t");
    | Emit.TTT_shr: print ("\tshr\t");
    | Emit.TTT_sar: print ("\tsar\t");
    END;
    PrintReg   (r, sz);
    print (", cl");
    out;
END GenShiftR_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenShiftR_INum* (TTT: LONGINT; r: SHORTINT;
                                              v: LONGINT; sz: SHORTINT);
BEGIN
    CASE TTT OF
    | Emit.TTT_rol:  print ("\trol\t");
    | Emit.TTT_ror:  print ("\tror\t");
    | Emit.TTT_shl:  print ("\tshl\t");
    | Emit.TTT_shr:  print ("\tshr\t");
    | Emit.TTT_sar:  print ("\tsar\t");
    END;
    PrintReg   (r, sz);
    print (", %d", v);
    out;
END GenShiftR_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenShiftM_INum* (TTT: LONGINT; a-: AddrMode;
                                              v: LONGINT; sz: SHORTINT);
BEGIN
    CASE TTT OF
    | Emit.TTT_rol:  print ("\trol\t");
    | Emit.TTT_ror:  print ("\tror\t");
    | Emit.TTT_shl:  print ("\tshl\t");
    | Emit.TTT_shr:  print ("\tshr\t");
    | Emit.TTT_sar:  print ("\tsar\t");
    END;
    PrintMem   (a, sz);
    print (", %d", v);
    out;
END GenShiftM_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenCBW*;
BEGIN
    print ("\tcbw");
    out;
END GenCBW;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenCDQ* (sz: SHORTINT);
BEGIN
    IF sz = 2 THEN
        print ("\tcwd");
    ELSE
        print ("\tcdq");
    END;
    out;
END GenCDQ;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenRepMovSD*;
BEGIN
    print ("\trep movsd");
    out;
END GenRepMovSD;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMovSD*;
BEGIN
    print ("\tmovsd");
    out;
END GenMovSD;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMovSW*;
BEGIN
    print ("\tmovsw");
    out;
END GenMovSW;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenRepMovSB*;
BEGIN
    print ("\trep movsb");
    out;
END GenRepMovSB;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMovSB*;
BEGIN
    print ("\tmovsb");
    out;
END GenMovSB;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenSahf*;
BEGIN
    print ("\tsahf");
    out;
END GenSahf;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenCall_M* (a-: AddrMode; u, m: SET;
                                         r, w: BitVect.BitVector);
BEGIN
    print ("\tcall\t");
    PrintMem   (a, 4);
    out;
END GenCall_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenCall_R* (r: SHORTINT; u, m: SET;
                                         rd, w: BitVect.BitVector);
BEGIN
    print ("\tcall\t");
    PrintReg   (r, 4);
    out;
END GenCall_R;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenCall_INum* (v: LONGINT; u, m: SET;
                                            r, w: BitVect.BitVector);
BEGIN
    ASSERT (FALSE);
END GenCall_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenCall_Iglobal* (a-: AddrMode; u, m: SET;
                                               r, w: BitVect.BitVector);
BEGIN
    print ("\tcall\t");
    PrintMem   (a, MIN (SHORTINT));
    out;
END GenCall_Iglobal;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenRet* (n: INT);
BEGIN
    IF n = 0 THEN
        print ("\tret");
    ELSE
        print ("\tret %d", n);
    END;
    out;
END GenRet;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenCase* (r: SHORTINT; v: LONGINT; lb: Emit.LABEL);
BEGIN
    IF v < 0 THEN
        print ("\tjmp L%d%d(", lb, v);
    ELSIF v = 0 THEN
        print ("\tjmp L%d(", lb);
    ELSE
        print ("\tjmp L%d+%d(", lb, v);
    END;
    PrintReg   (r, 4);
    print ("*4)\t\t; jump via CASE-table");
    out;
END GenCase;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveR_Table* (r, i: SHORTINT; t: pcK.OBJECT;
                                              sz: SHORTINT);
BEGIN
    print ("\tmov\t");
    PrintReg   (r, sz);
    print (", table(");
    PrintReg   (i, 4);
    print ("*4)");
    out;
END GenMoveR_Table;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenOpR_Table* (TTT: LONGINT; r, i: SHORTINT;
                                            t: pcK.OBJECT; sz: SHORTINT);
BEGIN
    PrintOp    (TTT);
    PrintReg   (r, sz);
    print (", table(");
    PrintReg   (i, 4);
    print ("*4)");
    out;
END GenOpR_Table;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenTestR_Table* (r, i: SHORTINT; t: pcK.OBJECT;
                                              sz: SHORTINT);
BEGIN
    print ("\ttest\t");
    PrintReg   (r, sz);
    print (", X2C_INCLs(");
    PrintReg   (i, 4);
    print ("*4)");
    out;
END GenTestR_Table;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenHalt*;
BEGIN
    print ("\tcall\tX2C_HALT");
    out;
END GenHalt;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenRaise* (is_assert: BOOLEAN);
BEGIN
    IF is_assert THEN
        print ("\tcall\tX2C_TRAP_A");
    ELSE
        print ("\tcall\tX2C_TRAP_F");
    END;
    out;
END GenRaise;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenSkipTrap* (j, intno: LONGINT; pos-: ir.TPOS);
BEGIN
    IF intno = Emit.IntOTrap THEN
        print ("\tinto");
    ELSE
        CASE j OF
        | Emit.JO:  print ("\tjo");
        | Emit.JNO: print ("\tjno");
        | Emit.JB:  print ("\tjb");
        | Emit.JNC: print ("\tjnc");
        | Emit.JE:  print ("\tje");
        | Emit.JNE: print ("\tjne");
        | Emit.JBE: print ("\tjbe");
        | Emit.JA:  print ("\tja");
        | Emit.JS:  print ("\tjs");
        | Emit.JNS: print ("\tjns");
        | Emit.JPE: print ("\tjpe");
        | Emit.JPO: print ("\tjpo");
        | Emit.JL:  print ("\tjl");
        | Emit.JGE: print ("\tjge");
        | Emit.JLE: print ("\tjle");
        | Emit.JG:  print ("\tjg");
        END;
        print ("\t$+5"); out;
	print ("\tcall\t");
        CASE intno OF
        | Emit.NilTrap:         print ("X2C_TRAP_NIL");
        | Emit.RangeTrap:       print ("X2C_TRAP_RANGE");
        | Emit.IndexTrap:       print ("X2C_TRAP_INDEX");
        | Emit.OverflowTrap:    print ("X2C_TRAP_OVERFLOW");
        | Emit.DivideTrap:      print ("X2C_TRAP_DIV");
        END;
    END;
    out;
END GenSkipTrap;

--------------------------------------------------------------------------------
--
--  Вещественная арифметика
--

--------------------------------------------------------------------------------

PROCEDURE PrintFOP (op: INT);
BEGIN
    CASE op OF
    | Emit.FADD:    print ("\tfadd");
    | Emit.FMUL:    print ("\tfmul");
    | Emit.FDIV:    print ("\tfdiv");
    | Emit.FDIVR:   print ("\tfdivr");
    | Emit.FSUB:    print ("\tfsub");
    | Emit.FSUBR:   print ("\tfsubr");
    END;
END PrintFOP;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveTOS_M* (a-: AddrMode; sz: SHORTINT);
BEGIN
    print ("\tfld\t");
    PrintMem   (a, sz);
    out;
END GenMoveTOS_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveTOS_INum* (w: VALUE; sz: SHORTINT);
BEGIN
    print ("\tfld\t='");
    PrintVal   (w);
    print ("'");
    out;
END GenMoveTOS_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenFILD* (a-: AddrMode; sz: SHORTINT);
BEGIN
    print ("\tfild\t");
    PrintMem   (a, sz);
    out;
END GenFILD;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveM_TOS* (a-: AddrMode; sz: SHORTINT);
BEGIN
    print ("\tfstp\t");
    PrintMem   (a, sz);
    out;
END GenMoveM_TOS;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveM_ST0* (a-: AddrMode; sz: SHORTINT);
BEGIN
    print ("\tfst\t");
    PrintMem   (a, sz);
    out;
END GenMoveM_ST0;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveSTi_TOS* (i: LONGINT);
BEGIN
    print ("\tfstp\tst(%d)", i);
    out;
END GenMoveSTi_TOS;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenMoveSTi_ST0* (i: LONGINT);
BEGIN
    print ("\tfst\tst(%d)", i);
    out;
END GenMoveSTi_ST0;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenFOpSTi_TOS* (op: INT; r: SHORTINT);
BEGIN
    PrintFOP   (op);
    print ("p\tst(%d), st", r);
    out;
END GenFOpSTi_TOS;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenFOpSTi_ST0* (op: INT; r: SHORTINT);
BEGIN
    PrintFOP   (op);
    print ("\tst(%d), st", r);
    out;
END GenFOpSTi_ST0;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenFOpST0_STi* (op: INT; r: SHORTINT);
BEGIN
    PrintFOP   (op);
    print ("\tst, st(%d)", r);
    out;
END GenFOpST0_STi;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenFOpST0_M* (op: INT; a-: AddrMode; sz: SHORTINT);
BEGIN
    PrintFOP   (op);
    print ("\t");
    PrintMem   (a, sz);
    out;
END GenFOpST0_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenFOpST0_IM* (op: INT; a-: AddrMode; sz: SHORTINT);
BEGIN
    CASE op OF
    | Emit.FADD:    print ("\tfiadd\t");
    | Emit.FMUL:    print ("\tfimul\t");
    | Emit.FDIV:    print ("\tfidiv\t");
    | Emit.FDIVR:   print ("\tfidivr\t");
    | Emit.FSUB:    print ("\tfisub\t");
    | Emit.FSUBR:   print ("\tfisubr\t");
    END;
    PrintMem   (a, sz);
    out;
END GenFOpST0_IM;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenFOpST0_INum* (op: INT; w: VALUE; sz: SHORTINT);
BEGIN
    PrintFOP   (op);
    print ("\t='");
    PrintVal   (w);
    print ("'");
    out;
END GenFOpST0_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenFOp* (code: LONGINT);
BEGIN
    CASE code OF
    | Emit.FXCH:    print ("\tfxch");
    | Emit.FABS:    print ("\tfabs");
    | Emit.FCHS:    print ("\tfchs");
    | Emit.FLD1:    print ("\tfld1");
    | Emit.FLDL2E:  print ("\tfldl2e");
    | Emit.FLDPI:   print ("\tfldpi");
    | Emit.FLDLG2:  print ("\tfldlg2");
    | Emit.FLDLN2:  print ("\tfldln2");
    | Emit.FLDZ:    print ("\tfldz");
    | Emit.F2XM1:   print ("\tf2xm1");
    | Emit.FYL2X:   print ("\tfyl2x");
    | Emit.FPTAN:   print ("\tfptan");
    | Emit.FPATAN:  print ("\tfpatan");
    | Emit.FPREM:   print ("\tfprem");
    | Emit.FSQRT:   print ("\tfsqrt");
    | Emit.FSCALE:  print ("\tfscale");
    | Emit.FSIN:    print ("\tfsin");
    | Emit.FCOS:    print ("\tfcos");
    END;
    out;
END GenFOp;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenFComTOS_TOS*;
BEGIN
    print ("\tfcompp"); out;
END GenFComTOS_TOS;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenFComTOS_INum* (w: VALUE; sz: SHORTINT);
BEGIN
    print ("\tfcomp\t='");
    PrintVal   (w);
    print ("'");
    out;
END GenFComTOS_INum;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenFComTOS_M* (a-: AddrMode; sz: SHORTINT);
BEGIN
    print ("\tfcomp\t");
    PrintMem   (a, sz);
    out;
END GenFComTOS_M;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenFComTOS_IM* (a-: AddrMode; sz: SHORTINT);
BEGIN
    print ("\tficomp\t");
    PrintMem   (a, sz);
    out;
END GenFComTOS_IM;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenFstsw*;
BEGIN
    print ("\tfstsw\tax"); out;
END GenFstsw;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfTxtPtr) GenCallToOrdinal* (card: BOOLEAN; u, m: SET);
BEGIN
    IF card THEN
        print ("\tcall\tX2C_TRUNCC");
    ELSE
        print ("\tcall\tX2C_TRUNCI");
    END;
    out;
END GenCallToOrdinal;

--------------------------------------------------------------------------------

VAR e: SelfTxtPtr;
PROCEDURE InitOutput*;
BEGIN
    NEW (e);
    Emit.full := e;
    buf[0] := 0X;
END InitOutput;

END EmitTxt.
