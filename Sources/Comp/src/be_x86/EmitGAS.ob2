<* -IOVERFLOW *>
<* -COVERFLOW *>
<* -CHECKINDEX *>
<* +PROCINLINE *>
<* +O2EXTENSIONS *>

MODULE EmitGAS;

  IMPORT
    io:=opIO,
    Emit,
    CodeDef,
    ObjNames,
    at := opAttrs,
    prc := opProcs,
    BitVect,
    pc:=pcK,
    Calc,
    opStd,
    env:=xiEnv,
    ir;
IMPORT D := desc386;
IMPORT reg := reg386;
IMPORT SYSTEM;

IMPORT FormStr, InOut;

TYPE
    INT  = ir.INT;
    AddrMode    = Emit.AddrMode;
    Node        = ir.Node;
    BitVector   = BitVect.BitVector;
    VALUE       = ir.VALUE;
    SizeType    = Emit.SizeType;
    PhysReg     = D.PhysReg;
    Reg         = D.Reg;
    RegSet      = D.RegSet;
    SelfAsmPtr *= POINTER TO SelfAsmRec;
    SelfAsmRec *= RECORD (Emit.SelfRec) END;
--------------------------------------------------------------------------------

VAR line:  ARRAY 1024 OF CHAR;
    pos:   INT;
    sg:    CodeDef.CODE_SEGM;
    cur_node: Node; -- *shell

    LabelPrefix: ARRAY 8 OF CHAR;

    LAST_FLAGS_OP:     INT;
    LAST_FLAGS_REG:    PhysReg;    -- С каким регистром выполнялась последняя
                                    --   команда, устанавливающая флажки
    LAST_FLAGS_SIZE:   SizeType;    -- размер этого регистра
    LAST_FLAGS_OFFSET: LONGINT;     -- смещение этой команды (в текущем узле)

CONST
    Tab  = 11C;
    TRAP_LABEL_FORMAT = "label_%s_%s";

--------------------------------------------------------------------------------

PROCEDURE SetLastFlags (op: INT; r: PhysReg; sz: SizeType);
BEGIN
    LAST_FLAGS_OP     := op;
    LAST_FLAGS_REG    := r;
    LAST_FLAGS_SIZE   := sz;
    LAST_FLAGS_OFFSET := sg.code_len;
END SetLastFlags;

--------------------------------------------------------------------------------

PROCEDURE Enter*;
BEGIN
    line [pos] := 0C;
<* IF ~nodebug THEN *>
    io.print("@@%s\n", line);
<* END *>
    CodeDef.GenInstr (line);
    pos := 0;
END Enter;

--------------------------------------------------------------------------------

PROCEDURE Char (c: CHAR);
BEGIN
    line [pos] := c;
    INC (pos);
END Char;

--------------------------------------------------------------------------------

PROCEDURE Space;
BEGIN
    line [pos] := Tab;
    INC (pos);
END Space;

--------------------------------------------------------------------------------

PROCEDURE Text* (p-: ARRAY OF CHAR);
VAR i: INT;
BEGIN
    i := 0;
    WHILE p [i] <> 0C DO
      line [pos] := p [i];
      INC (pos);
      INC (i);
      ASSERT(pos<LEN(line));
    END;
END Text;

--------------------------------------------------------------------------------

PROCEDURE Comma;
BEGIN
    line [pos]     := ',';
    line [pos + 1] := ' ';
    INC (pos, 2);
END Comma;

--------------------------------------------------------------------------------

PROCEDURE Digit (n: INT);
BEGIN
    ASSERT ((n >= 0) & (n <= 9));
    line [pos] := CHR (n + ORD ('0'));
    INC (pos);
END Digit;

--------------------------------------------------------------------------------

PROCEDURE Number* (n: INT);

  PROCEDURE Num (n: INT);
  BEGIN
    IF n > 9 THEN
      Num (n DIV 10);
    END;
    Digit (n MOD 10);
  END Num;

BEGIN
    IF n < 0 THEN
        IF n = MIN (LONGINT) THEN
            Text ("0x80000000");
        ELSE
            Char ('-');
            Num  (- n);
        END;
    ELSIF n = 0 THEN
        Char ('0');
    ELSE
        Num (n);
    END;
END Number;

--------------------------------------------------------------------------------
(*
PROCEDURE Hex (n, len: INT);
BEGIN
    IF len > 1 THEN
        Hex (n DIV 16, len - 1);
    END;
    n := n MOD 16;
    IF n < 10 THEN
        line [pos] := CHR (n + ORD('0') );
    ELSE
        line [pos] := CHR (n + ORD('A') - 10);
    END;
    INC (pos);
END Hex;
*)
--------------------------------------------------------------------------------

PROCEDURE OutReg (r: PhysReg; s: SHORTINT);
BEGIN
    CASE s OF
    | 1:
        CASE r OF
        | D.ALp:   Text ("%al");
        | D.BLp:   Text ("%bl");
        | D.CLp:   Text ("%cl");
        | D.DLp:   Text ("%dl");
        | D.AHp:   Text ("%ah");
        | D.BHp:   Text ("%bh");
        | D.CHp:   Text ("%ch");
        | D.DHp:   Text ("%dh");
        | D.SPILLED1p:   Text ("%spilled1");
        END;
    | 2:
        CASE r OF
        | D.AXp:   Text ("%ax");
        | D.BXp:   Text ("%bx");
        | D.CXp:   Text ("%cx");
        | D.DXp:   Text ("%dx");
        | D.SPp:   Text ("%sp");
        | D.BPp:   Text ("%bp");
        | D.SIp:   Text ("%si");
        | D.DIp:   Text ("%di");
        | D.SPILLED1p:   Text ("%spilled1_2");
        | D.SPILLED2p:   Text ("%spilled2");
        END;
    | 4:
        CASE r OF
        | D.EAXp:   Text ("%eax");
        | D.EBXp:   Text ("%ebx");
        | D.ECXp:   Text ("%ecx");
        | D.EDXp:   Text ("%edx");
        | D.ESPp:  Text ("%esp");
        | D.EBPp:   Text ("%ebp");
        | D.ESIp:   Text ("%esi");
        | D.EDIp:   Text ("%edi");
        | D.SPILLED1p:   Text ("%spilled1_4");
        | D.SPILLED2p:   Text ("%spilled2_4");
        | D.SPILLED4p:   Text ("%spilled4");
        | D.SPILLED8phi:   Text ("%spilled8hi");
        | D.SPILLED8plo:   Text ("%spilled8lo");
        END;
    END;
END OutReg;

--------------------------------------------------------------------------------

PROCEDURE OutObject (o: pc.OBJECT);
  VAR buf: ARRAY 1024 OF CHAR;
BEGIN
    ObjNames.makename (o, buf);
    Text (buf);
END OutObject;

--------------------------------------------------------------------------------

PROCEDURE OutMem* (a-: AddrMode);
  VAR o:  INT;
      b: BOOLEAN;
      r1, r2: PhysReg;
BEGIN
    ASSERT(a.offslist=NIL);
    ASSERT(a.proclist=NIL);
    ASSERT((a.proc = ir.ProcUNDEFINED) OR (a.local = ir.UNDEFINED));
    r1 := D.RegInfo[a.place1.r].code;
    r2 := D.RegInfo[a.place2.r].code;
    b := FALSE;
    o := a.offs;
    IF a.local <> ir.UNDEFINED THEN
        ASSERT(a.proc = ir.ProcUNDEFINED);
        IF ir.IsExternal (a.local) THEN
            OutObject (ir.Locals[a.local].Obj);
            b := TRUE;
        ELSE
            INC (o, ir.Locals^[a.local].Offset + Emit.LocalsOffset);
            IF Emit.baseReg = D.ESP THEN
                INC (o, Emit.PushSize);
            END;
            IF env.config.Option("asmlocals") THEN
              IF ir.Locals[a.local].Name # NIL THEN
                Text(" <");
                Text(ir.Locals[a.local].Name^);
                Text("> ");
              ELSE
                Text(" <tmp");
                Number(a.local);
                Text("> ");
              END;
            END;
        END;
    ELSIF a.proc <> ir.ProcUNDEFINED THEN
        OutObject (prc.ProcObj (a.proc));
        b := TRUE;
    END;
    IF o <> 0 THEN
        IF (o > 0) AND b THEN Char('+') END;
        Number(o);
    ELSIF (r1 = D.UNDEF_REGp) & (r2 = D.UNDEF_REGp) & ~b THEN
        Number (0);
    END;
    IF (r1 <> D.UNDEF_REGp) OR (r2 <> D.UNDEF_REGp) THEN
        Char('(');
        IF r2 = D.ESPp THEN
          IF ~reg.iteration THEN ASSERT ((r1 <> D.ESPp)& (a.scale = D.x1)); END;
          r2 := r1;
          r1 := D.ESPp;
        END;
        IF r1 <> D.UNDEF_REGp THEN
            OutReg (r1, 4);
        END;
        IF r2 <> D.UNDEF_REGp THEN
            Comma;
            OutReg (r2, 4);
            IF a.scale#D.x1 THEN
                Comma;
                CASE a.scale OF
                    | D.x2:   Char('2');
                    | D.x4:   Char('4');
                    | D.x8:   Char('8');
                END;
            END;
        END;
        Char(")");
    END;
END OutMem;

--------------------------------------------------------------------------------

PROCEDURE Size(sz: SHORTINT);
BEGIN
  CASE sz OF
    1: Char('b') |
    2: Char('w') |
    4: Char('l') |
    8: Char('q') |
  END;
END Size;

--------------------------------------------------------------------------------

PROCEDURE OutCommand(name-: ARRAY OF CHAR; sz: SHORTINT);
BEGIN
  Text(name);
  Size(sz);
  Space;
END OutCommand;

--------------------------------------------------------------------------------

PROCEDURE OutOp (op: D.BinaryOp; sz: SHORTINT);
BEGIN
    CASE op OF
    | D.TTT_add:     OutCommand (Tab + "add", sz);
    | D.TTT_or:      OutCommand (Tab + "or",  sz);
    | D.TTT_adc:     OutCommand (Tab + "adc", sz);
    | D.TTT_sbb:     OutCommand (Tab + "sbb", sz);
    | D.TTT_and:     OutCommand (Tab + "and", sz);
    | D.TTT_sub:     OutCommand (Tab + "sub", sz);
    | D.TTT_xor:     OutCommand (Tab + "xor", sz);
    | D.TTT_cmp:     OutCommand (Tab + "cmp", sz);
    | D.TTT_mul:     OutCommand (Tab + "imul",sz);
    | D.TTT_bt :     OutCommand (Tab + "bt",sz);
    | D.TTT_bts:     OutCommand (Tab + "bts",sz);
    | D.TTT_btr:     OutCommand (Tab + "btr",sz);
    END;
END OutOp;

--------------------------------------------------------------------------------

PROCEDURE OutSET(c: D.Condition);
BEGIN
  CASE c OF
    | D.JO:  OutCommand(Tab + "seto",1);
    | D.JNO: OutCommand(Tab + "setno",1);
    | D.JB:  OutCommand(Tab + "setb",1);
    | D.JNC: OutCommand(Tab + "setnc",1);
    | D.JE:  OutCommand(Tab + "sete",1);
    | D.JNE: OutCommand(Tab + "setne",1);
    | D.JBE: OutCommand(Tab + "setbe",1);
    | D.JA:  OutCommand(Tab + "seta",1);
    | D.JS:  OutCommand(Tab + "sets",1);
    | D.JNS: OutCommand(Tab + "setns",1);
    | D.JPE: OutCommand(Tab + "setpe",1);
    | D.JPO: OutCommand(Tab + "setpo",1);
    | D.JL:  OutCommand(Tab + "setl",1);
    | D.JGE: OutCommand(Tab + "setge",1);
    | D.JLE: OutCommand(Tab + "setle",1);
    | D.JG:  OutCommand(Tab + "setg",1);
  END;
END OutSET;

--------------------------------------------------------------------------------

PROCEDURE OutJmp (j: D.Condition);
BEGIN
  CASE j OF
    | D.UnJ: Text(Tab + "jmp" + Tab);
    | D.JO:  Text(Tab + "jo"  + Tab);
    | D.JNO: Text(Tab + "jno" + Tab);
    | D.JB:  Text(Tab + "jb"  + Tab);
    | D.JNC: Text(Tab + "jnc" + Tab);
    | D.JE:  Text(Tab + "je"  + Tab);
    | D.JNE: Text(Tab + "jne" + Tab);
    | D.JBE: Text(Tab + "jbe" + Tab);
    | D.JA:  Text(Tab + "ja"  + Tab);
    | D.JS:  Text(Tab + "js"  + Tab);
    | D.JNS: Text(Tab + "jns" + Tab);
    | D.JPE: Text(Tab + "jpe" + Tab);
    | D.JPO: Text(Tab + "jpo" + Tab);
    | D.JL:  Text(Tab + "jl"  + Tab);
    | D.JGE: Text(Tab + "jge" + Tab);
    | D.JLE: Text(Tab + "jle" + Tab);
    | D.JG:  Text(Tab + "jg"  + Tab);
  END;
END OutJmp;

PROCEDURE OutShift (ttt: D.ShiftOp; sz: SHORTINT);
BEGIN
  CASE ttt OF
    | D.TTT_rol: OutCommand(Tab + "rol",sz);
    | D.TTT_ror: OutCommand(Tab + "ror",sz);
    | D.TTT_shl: OutCommand(Tab + "shl",sz);
    | D.TTT_shr: OutCommand(Tab + "shr",sz);
    | D.TTT_sar: OutCommand(Tab + "sar",sz);
  END;
END OutShift;

--------------------------------------------------------------------------------

PROCEDURE CallStdProc (no: INT);
BEGIN
  Text(Tab + "call" + Tab);
  OutObject(opStd.Proc (no));
  Enter;
END CallStdProc;

--------------------------------------------------------------------------------
--
--      Вход в линейный участок, координаты, etc...
--
--------------------------------------------------------------------------------

PROCEDURE (self: SelfAsmPtr) EnterNode* (n: Node; seg: CodeDef.CODE_SEGM);
BEGIN
    sg := seg;
    cur_node := n; -- *shell
    CodeDef.set_segm (seg);
    Emit.PushSize  := 0;
    Emit.FloatSize := 0;
    LAST_FLAGS_REG    := D.UNDEF_REGp;
    LAST_FLAGS_OFFSET := MAX (LONGINT) DIV 2;
    LAST_FLAGS_SIZE   := 0;
END EnterNode;

PROCEDURE (self: SelfAsmPtr) SetSegment* (n: Node; seg: CodeDef.CODE_SEGM);
BEGIN
    sg := seg;
    CodeDef.set_segm (seg);
END SetSegment;

PROCEDURE (self: SelfAsmPtr) EmptySegment* (n: Node;
                                         sg: CodeDef.CODE_SEGM): BOOLEAN;
BEGIN
    RETURN sg.code_len = 0;
END EmptySegment;

PROCEDURE (self: SelfAsmPtr) AddPosition* (pos-: ir.TPOS);
<* IF ~nodebug THEN *>
  VAR ln,col: LONGINT;   fname: pc.STRING;
<* END *>
BEGIN
  IF NOT pos.IsNull () THEN
<* IF ~nodebug THEN *>
  IF io.needed AND env.config.Option("asmpos") THEN
       pos.unpack(fname, ln, col);
       Text("# [");
       Number(ln+1);
       Text(":");
       Number(col+1);
       Text("]");
       Enter;
  END;
<* END *>
    CodeDef.add_pos (sg.code_len, pos);
  END;
END AddPosition;

--------------------------------------------------------------------------------

VAR lab: Emit.LABEL;

PROCEDURE (self: SelfAsmPtr) NewLabel* (VAR x: Emit.LABEL);
BEGIN
    x := lab;
    INC (lab);
END NewLabel;

PROCEDURE (self: SelfAsmPtr) SetLabel* (x: Emit.LABEL);
BEGIN
    Text (LabelPrefix);
    Number (ORD(x));
    Char (':');
    Enter;
END SetLabel;

PROCEDURE (self: SelfAsmPtr) InsertLabel* (VAR x: Emit.LABEL);
BEGIN
    x := lab;
    Text (LabelPrefix);
    Number (ORD(x));
    Char (':');
    line [pos] := 0C;
    CodeDef.InsInstr (line,0);
    pos := 0;
    INC (lab);
END InsertLabel;

PROCEDURE (self: SelfAsmPtr) DwLabel* (x: Emit.LABEL);
BEGIN
    Text (Tab + ".long ");
    Text (LabelPrefix);
    Number (ORD(x));
    line [pos] := 0C;
    CodeDef.GenInstr (line);
    pos := 0;
END DwLabel;

--------------------------------------------------------------------------------
--
--      Собственно генератор команд
--
--------------------------------------------------------------------------------

PROCEDURE (self: SelfAsmPtr) GenMoveR_R* (d, s: PhysReg; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  b := (d <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
  OutCommand(Tab + "mov",sz);
  OutReg(s,sz);
  Comma;
  OutReg(d,sz);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenMoveR_R;

PROCEDURE (self: SelfAsmPtr) GenMoveR_M* (r: PhysReg; a-: AddrMode; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  b := (r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
  OutCommand(Tab + "mov",sz);
  OutMem(a);
  Comma;
  OutReg(r,sz);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenMoveR_M;

PROCEDURE (self: SelfAsmPtr) GenMoveR_INum* (r: PhysReg; v: LONGINT; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  b := (r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
  OutCommand(Tab + "mov",sz);
  Char('$');
  Number(v);
  Comma;
  OutReg(r,sz);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenMoveR_INum;

PROCEDURE (self: SelfAsmPtr) GenMoveR_Iglobal* (r: PhysReg; a-: AddrMode; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  b := (r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
  ASSERT( (a.place1.r = D.UNDEF_REG) AND (a.place2.r = D.UNDEF_REG));
  OutCommand(Tab + "mov",sz);
  Char('$');
  OutMem(a);
  Comma;
  OutReg(r,sz);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenMoveR_Iglobal;

PROCEDURE (self: SelfAsmPtr) GenMoveM_R* (a-: AddrMode; r: PhysReg; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  b := (LAST_FLAGS_OFFSET = sg.code_len);
  OutCommand(Tab + "mov",sz);
  OutReg(r,sz);
  Comma;
  OutMem(a);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenMoveM_R;

PROCEDURE (self: SelfAsmPtr) GenMoveM_INum* (a-: AddrMode; v: LONGINT; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  b := (LAST_FLAGS_OFFSET = sg.code_len);
  OutCommand(Tab + "mov",sz);
  Char('$');
  Number(v);
  Comma;
  OutMem(a);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenMoveM_INum;

PROCEDURE (self: SelfAsmPtr) GenMoveM_Iglobal* (a-, ag-: AddrMode; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  b := (LAST_FLAGS_OFFSET = sg.code_len);
  ASSERT( (ag.place1.r = D.UNDEF_REG) AND (ag.place2.r = D.UNDEF_REG));
  OutCommand(Tab + "mov",sz);
  Char('$');
  OutMem(ag);
  Comma;
  OutMem(a);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenMoveM_Iglobal;

PROCEDURE (self: SelfAsmPtr) GenXchgR_R* (d, s: PhysReg; sz: SizeType);
BEGIN
  OutCommand(Tab + "xchg",sz);
  OutReg(s,sz);
  Comma;
  OutReg(d,sz);
  Enter;
END GenXchgR_R;

PROCEDURE (self: SelfAsmPtr) GenXchgR_M* (r: PhysReg; a-: AddrMode; sz: SizeType);
BEGIN
  OutCommand(Tab + "xchg",sz);
  OutMem(a);
  Comma;
  OutReg(r,sz);
  Enter;
END GenXchgR_M;

PROCEDURE (self: SelfAsmPtr) GenPush_R* (r: PhysReg);
VAR b: BOOLEAN;
BEGIN
  b := (LAST_FLAGS_OFFSET = sg.code_len);
  OutCommand(Tab + "push",4);
  OutReg(r,4);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenPush_R;

PROCEDURE (self: SelfAsmPtr) GenPush_M* (a-: AddrMode;sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  ASSERT((sz=4) OR (sz=2));
  b := (LAST_FLAGS_OFFSET = sg.code_len);
  OutCommand(Tab + "push",sz);
  OutMem(a);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenPush_M;

PROCEDURE (self: SelfAsmPtr) GenPush_INum* (v: LONGINT; sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  b := (LAST_FLAGS_OFFSET = sg.code_len);
  OutCommand(Tab + "push",4);
  Char('$');
  Number(v);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenPush_INum;

PROCEDURE (self: SelfAsmPtr) GenPush_Iglobal* (a-: AddrMode);
VAR b: BOOLEAN;
BEGIN
  b := (LAST_FLAGS_OFFSET = sg.code_len);
  ASSERT( (a.place1.r = D.UNDEF_REG) AND (a.place2.r = D.UNDEF_REG));
  OutCommand(Tab + "push",4);
  Char('$');
  OutMem(a);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenPush_Iglobal;

PROCEDURE (self: SelfAsmPtr) GenPop_R* (r: PhysReg);
VAR b: BOOLEAN;
BEGIN
  b := (r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
  OutCommand(Tab + "pop",4);
  OutReg(r,4);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenPop_R;

PROCEDURE (self: SelfAsmPtr) GenOpINum_R* (ttt: D.BinaryOp; v: VALUE; sz: SizeType;
                                                r: PhysReg );
BEGIN
    ASSERT(FALSE);
END GenOpINum_R;

PROCEDURE (self: SelfAsmPtr) GenOpR_R* (ttt: D.BinaryOp; d, s: PhysReg;
                                     sz: SizeType);
BEGIN
  OutOp(ttt,sz);
  OutReg(s,sz);
  Comma;
  OutReg(d,sz);
  Enter;
  IF (ttt <> D.TTT_cmp) & (ttt <> D.TTT_mul) THEN
    SetLastFlags (ORD(ttt), d, sz);
  END;
END GenOpR_R;

PROCEDURE (self: SelfAsmPtr) GenOpR_M* (ttt: D.BinaryOp; r: PhysReg;
                                     a-: AddrMode; sz: SizeType);
BEGIN
  OutOp(ttt,sz);
  OutMem(a);
  Comma;
  OutReg(r,sz);
  Enter;
  IF (ttt <> D.TTT_cmp) & (ttt <> D.TTT_mul) THEN
    SetLastFlags (ORD(ttt), r, sz);
  END;
END GenOpR_M;

PROCEDURE (self: SelfAsmPtr) GenOpR_INum* (ttt: D.BinaryOp; r: PhysReg;
                                        v: LONGINT; sz: SizeType);
BEGIN
  OutOp(ttt,sz);
  Char('$');
  Number(v);
  Comma;
  OutReg(r,sz);
  Enter;
  IF (ttt <> D.TTT_cmp) & (ttt <> D.TTT_mul) THEN
    SetLastFlags (ORD(ttt), r, sz);
  END;
END GenOpR_INum;

PROCEDURE (self: SelfAsmPtr) GenOpR_Iglobal* (ttt: D.BinaryOp; r: PhysReg;
                                           a-: AddrMode; sz: SizeType);
BEGIN
  ASSERT( (a.place1.r = D.UNDEF_REG) AND (a.place2.r = D.UNDEF_REG));
  OutOp(ttt,sz);
  Char('$');
  OutMem(a);
  Comma;
  OutReg(r,sz);
  Enter;
END GenOpR_Iglobal;

PROCEDURE (self: SelfAsmPtr) GenOpM_R* (ttt: D.BinaryOp; a-: AddrMode;
                                     r: PhysReg; sz: SizeType);
BEGIN
  OutOp(ttt,sz);
  OutReg(r,sz);
  Comma;
  OutMem(a);
  Enter;
END GenOpM_R;

PROCEDURE (self: SelfAsmPtr) GenOpM_INum* (ttt: D.BinaryOp; a-: AddrMode;
                                        v: LONGINT; sz: SizeType);
BEGIN
  OutOp(ttt,sz);
  Char('$');
  Number(v);
  Comma;
  OutMem(a);
  Enter;
END GenOpM_INum;

PROCEDURE (self: SelfAsmPtr) GenOpM_Iglobal* (ttt: D.BinaryOp; a-, ag-: AddrMode;
                                           sz: SizeType);
BEGIN
  ASSERT( (ag.place1.r = D.UNDEF_REG) AND (ag.place2.r = D.UNDEF_REG));
  OutOp(ttt,sz);
  Char('$');
  OutMem(ag);
  Comma;
  OutMem(a);
  Enter;
END GenOpM_Iglobal;

PROCEDURE (self: SelfAsmPtr) GenDiv_R* (r: PhysReg; sz: SizeType;
                                     signed: BOOLEAN);
BEGIN
  IF signed THEN
    OutCommand(Tab + "idiv",sz)
  ELSE
    OutCommand(Tab + "div",sz)
  END;
  OutReg(r,sz);
  Enter;
END GenDiv_R;

PROCEDURE (self: SelfAsmPtr) GenDiv_M* (a-: AddrMode; sz: SizeType;
                                     signed: BOOLEAN);
BEGIN
  IF signed THEN
    OutCommand(Tab + "idiv",sz)
  ELSE
    OutCommand(Tab + "div",sz)
  END;
  OutMem(a);
  Enter;
END GenDiv_M;

PROCEDURE (self: SelfAsmPtr) GenDiv_INum* (v: LONGINT; sz: ir.SizeType;
                                        signed: BOOLEAN);
BEGIN
  IF signed THEN
    OutCommand(Tab + "idiv",sz)
  ELSE
    OutCommand(Tab + "div",sz)
  END;
  OutObject(CodeDef.new_integer(Calc.GetInteger (v, sz), ir.t_int, sz));
  Enter;
END GenDiv_INum;

PROCEDURE (self: SelfAsmPtr) GenMul_R* (r: PhysReg; sz: SizeType;
                                     signed: BOOLEAN);
BEGIN
  IF signed THEN
    OutCommand(Tab + "imul",sz)
  ELSE
    OutCommand(Tab + "mul",sz)
  END;
  OutReg(r,sz);
  Enter;
END GenMul_R;

PROCEDURE (self: SelfAsmPtr) GenMul_M* (a-: AddrMode; sz: SizeType;
                                     signed: BOOLEAN);
BEGIN
  IF signed THEN
    OutCommand(Tab + "imul",sz)
  ELSE
    OutCommand(Tab + "mul",sz)
  END;
  OutMem(a);
  Enter;
END GenMul_M;

PROCEDURE (self: SelfAsmPtr) GenMul_INum* (v: LONGINT; sz: SizeType;
                                        signed: BOOLEAN);
BEGIN
  IF signed THEN
    OutCommand(Tab + "imul",sz)
  ELSE
    OutCommand(Tab + "mul",sz)
  END;
  OutObject(CodeDef.new_integer(Calc.GetInteger (v, sz), ir.t_int, sz));
  Enter;
END GenMul_INum;

PROCEDURE (self: SelfAsmPtr) GenIMulR_RC* (d, s: PhysReg; v: LONGINT;
                                        sz: SizeType);
BEGIN
  OutCommand(Tab + "imul",sz);
  Char('$');
  Number(v);
  Comma;
  OutReg(s,sz);
  Comma;
  OutReg(d,sz);
  Enter;
END GenIMulR_RC;

PROCEDURE (self: SelfAsmPtr) GenIMulR_MC* (r: PhysReg; a-: AddrMode;
                                        v: LONGINT; sz: SizeType);
BEGIN
  OutCommand(Tab + "imul",sz);
  Char('$');
  Number(v);
  Comma;
  OutMem(a);
  Comma;
  OutReg(r,sz);
  Enter;
END GenIMulR_MC;

PROCEDURE (self: SelfAsmPtr) GenTestR_R* (r1, r2: PhysReg; sz: SizeType);
BEGIN
  IF (r1 <> r2) OR (r1 <> LAST_FLAGS_REG) OR (sz <> LAST_FLAGS_SIZE) OR
     (sg.code_len <> LAST_FLAGS_OFFSET) OR (VAL(D.BinaryOp, LAST_FLAGS_OP) = D.TTT_add)
  THEN
    OutCommand(Tab + "test",sz);
    OutReg(r2,sz);
    Comma;
    OutReg(r1,sz);
    Enter;
    IF r1 = r2 THEN
      SetLastFlags (ORD(D.TTT_and), r1, sz);
    END;
  END;
END GenTestR_R;

PROCEDURE (self: SelfAsmPtr) GenTestR_INum* (r: PhysReg; v: LONGINT; sz: SizeType);
BEGIN
  OutCommand(Tab + "test",sz);
  Char('$');
  Number(v);
  Comma;
  OutReg(r,sz);
  Enter;
END GenTestR_INum;

PROCEDURE (self: SelfAsmPtr) GenTestM_INum* (a-: AddrMode; v: LONGINT; sz: SizeType);
BEGIN
  OutCommand(Tab + "test",sz);
  Char('$');
  Number(v);
  Comma;
  OutMem(a);
  Enter;
END GenTestM_INum;

PROCEDURE (self: SelfAsmPtr) GenLEA* (r: PhysReg; a-: AddrMode);
BEGIN
  OutCommand(Tab + "lea",4);
  OutMem(a);
  Comma;
  OutReg(r,4);
  Enter;
END GenLEA;

PROCEDURE (self: SelfAsmPtr) GenMovesxR_M* (r: PhysReg; a-: AddrMode; rsz, sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  b := (r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
  Text(Tab + "movs");
  Size(sz);
  Size(rsz);
  Space;
  OutMem(a);
  Comma;
  OutReg(r,rsz);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenMovesxR_M;

PROCEDURE (self: SelfAsmPtr) GenMovesxR_R* (d, s: PhysReg; rsz, sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  b := (d <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
  Text(Tab + "movs");
  Size(sz);
  Size(rsz);
  Space;
  OutReg(s,sz);
  Comma;
  OutReg(d,rsz);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenMovesxR_R;

PROCEDURE (self: SelfAsmPtr) GenMovezxR_M* (r: PhysReg; a-: AddrMode; rsz, sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  b := (r <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
  Text(Tab + "movz");
  Size(sz);
  Size(rsz);
  Space;
  OutMem(a);
  Comma;
  OutReg(r,rsz);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenMovezxR_M;

PROCEDURE (self: SelfAsmPtr) GenMovezxR_R* (d, s: PhysReg; rsz, sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  b := (d <> LAST_FLAGS_REG) & (LAST_FLAGS_OFFSET = sg.code_len);
  Text(Tab + "movz");
  Size(sz);
  Size(rsz);
  Space;
  OutReg(s,sz);
  Comma;
  OutReg(d,rsz);
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenMovezxR_R;

PROCEDURE (self: SelfAsmPtr) GenOpR* (ttt: D.UnaryOp; r: PhysReg; sz: SizeType);
BEGIN
  CASE ttt OF
    | D.TTT_inc: OutCommand(Tab + "inc",sz);
    | D.TTT_dec: OutCommand(Tab + "dec",sz);
    | D.TTT_not: OutCommand(Tab + "not",sz);
    | D.TTT_neg: OutCommand(Tab + "neg",sz);
  END;
  OutReg(r, sz);
  Enter;
  IF ttt # D.TTT_not THEN
    SetLastFlags (ORD(ttt), r, sz);
  END;
END GenOpR;

PROCEDURE (self: SelfAsmPtr) GenOpM* (ttt: D.UnaryOp; a-: AddrMode; sz: SizeType);
BEGIN
  CASE ttt OF
    | D.TTT_inc: OutCommand(Tab + "inc",sz);
    | D.TTT_dec: OutCommand(Tab + "dec",sz);
    | D.TTT_not: OutCommand(Tab + "not",sz);
    | D.TTT_neg: OutCommand(Tab + "neg",sz);
  END;
  OutMem(a);
  Enter;
END GenOpM;

PROCEDURE (self: SelfAsmPtr) GenSetC_R* (r: PhysReg; c: D.Condition);
BEGIN
  OutSET(c);
  OutReg(r,1);
  Enter;
END GenSetC_R;

PROCEDURE (self: SelfAsmPtr) GenSetC_M* (a-: AddrMode; c: D.Condition);
BEGIN
  OutSET(c);
  OutMem(a);
  Enter;
END GenSetC_M;

PROCEDURE (self: SelfAsmPtr) GenShiftR_R* (ttt: D.ShiftOp; r: PhysReg;
                                        sz: SizeType);
BEGIN
  OutShift (ttt, sz);
  Text("%cl, ");
  OutReg (r, sz);
  Enter;
END GenShiftR_R;

PROCEDURE (self: SelfAsmPtr) GenShiftR_INum* (ttt: D.ShiftOp; r: PhysReg;
                                           v: LONGINT; sz: SizeType);
BEGIN
  OutShift (ttt, sz);
  Char('$');
  Number(v);
  Comma;
  OutReg(r, sz);
  Enter;
END GenShiftR_INum;

PROCEDURE (self: SelfAsmPtr) GenShiftM_INum* (ttt: D.ShiftOp; a-: AddrMode;
                                           v: LONGINT; sz: SizeType);
BEGIN
  OutShift (ttt, sz);
  Char('$');
  Number(v);
  Comma;
  OutMem(a);
  Enter;
END GenShiftM_INum;

PROCEDURE (self: SelfAsmPtr) GenCBW*;
VAR b: BOOLEAN;
BEGIN
  b := (LAST_FLAGS_OFFSET = sg.code_len);
  Text(Tab + "cbw");
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenCBW;

PROCEDURE (self: SelfAsmPtr) GenCDQ* (sz: SizeType);
VAR b: BOOLEAN;
BEGIN
  b := (LAST_FLAGS_OFFSET = sg.code_len);
  IF sz = 2 THEN
    Text(Tab + "cwd");
  ELSE
    Text(Tab + "cdq");
  END;
  Enter;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;
END GenCDQ;

PROCEDURE (self: SelfAsmPtr) GenRepMovSD*;
BEGIN
  Text(Tab + "rep");
  Enter;
  Text(Tab + "movsl");
  Enter;
END GenRepMovSD;

PROCEDURE (self: SelfAsmPtr) GenMovSD*;
BEGIN
  Text(Tab + "movsl");
  Enter;
END GenMovSD;

PROCEDURE (self: SelfAsmPtr) GenMovSW*;
BEGIN
  Text(Tab + "movsw");
  Enter;
END GenMovSW;

PROCEDURE (self: SelfAsmPtr) GenRepMovSB*;
BEGIN
  Text(Tab + "rep");
  Enter;
  Text(Tab + "movsb");
  Enter;
END GenRepMovSB;

PROCEDURE (self: SelfAsmPtr) GenMovSB*;
BEGIN
  Text(Tab + "movsb");
  Enter;
END GenMovSB;

PROCEDURE (self: SelfAsmPtr) GenSahf*;
BEGIN
  Text(Tab + "sahf");
  Enter;
END GenSahf;

PROCEDURE (self: SelfAsmPtr) GenJ* (j: D.Condition; o: LONGINT; long: BOOLEAN);
BEGIN
  OutJmp (j);
  Number(o);
  Enter;
END GenJ;

PROCEDURE (self: SelfAsmPtr) GenTxtJ* (j: D.Condition; lb: Emit.LABEL);
BEGIN
  OutJmp (j);
  Text(LabelPrefix);
  Number(ORD(lb));
  Enter;
END GenTxtJ;

PROCEDURE (self: SelfAsmPtr) GenCall_M* (a-: AddrMode;
                                      uses, modifies: D.PhysRegSet;
                                      r, w: BitVector);
BEGIN
  Text(Tab + "call" + Tab + "*");
  OutMem(a);
  Enter;
END GenCall_M;

PROCEDURE (self: SelfAsmPtr) GenCall_R* (r: PhysReg;
                                      uses, modifies: D.PhysRegSet;
                                      rd, w: BitVector);
BEGIN
  Text(Tab + "call" + Tab + "*");
  OutReg(r,4);
  Enter;
END GenCall_R;

PROCEDURE (self: SelfAsmPtr) GenCall_INum* (v: LONGINT;
                                         uses, modifies: D.PhysRegSet;
                                         r, w: BitVector);
BEGIN
  ASSERT(FALSE);
END GenCall_INum;

PROCEDURE (self: SelfAsmPtr) GenCall_Iglobal* (a-: AddrMode;
                                            uses, modifies: D.PhysRegSet;
                                            r, w: BitVector);
BEGIN
  ASSERT( (a.place1.r = D.UNDEF_REG) AND (a.place2.r = D.UNDEF_REG));
  Text(Tab + "call" + Tab);
  OutMem(a);
  Enter;
END GenCall_Iglobal;

PROCEDURE (self: SelfAsmPtr) GenRet* (n: INT);
BEGIN
  Text(Tab + "ret");
  IF n#0 THEN Text(Tab + "$"); Number(n) END;
  Enter;
END GenRet;

PROCEDURE (self: SelfAsmPtr) GenCase* (r: PhysReg; v: LONGINT; lb: Emit.LABEL);
BEGIN
  Text(Tab + "jmp" + Tab + "*");
  Text(LabelPrefix);
  Number(ORD(lb));
  v:=v*4;
  IF v<0 THEN Number(v)
  ELSIF v>0 THEN Char('+'); Number(v)
  END;
  Text("(,");
  OutReg(r,4);
  Text(",4)");
  Enter;
END GenCase;

PROCEDURE (self: SelfAsmPtr) GenMoveR_Table* (r, i: PhysReg; t: pc.OBJECT;
                                           sz: SizeType);
BEGIN
  OutCommand(Tab + "mov",sz);
  OutObject(t);
  Text("(,");
  OutReg(i,4);
  Text(",4),");
  OutReg(r,sz);
  Enter;
END GenMoveR_Table;

PROCEDURE (self: SelfAsmPtr) GenOpR_Table* (ttt: D.BinaryOp; r, i: PhysReg;
                                         t: pc.OBJECT; sz: SizeType);
BEGIN
  OutOp(ttt,sz);
  OutObject(t);
  Text("(,");
  OutReg(i,4);
  Text(",4),");
  OutReg(r,sz);
  Enter;
  SetLastFlags (ORD(ttt), r, sz);
END GenOpR_Table;

PROCEDURE (self: SelfAsmPtr) GenTestR_Table* (r, i: PhysReg; t: pc.OBJECT;
                                           sz: SizeType);
BEGIN
  OutCommand(Tab + "test",sz);
  OutObject(t);
  Text("(,");
  OutReg(i,4);
  Text(",4),");
  OutReg(r,sz);
  Enter;
END GenTestR_Table;

PROCEDURE (self: SelfAsmPtr) GenHalt*;
BEGIN
  CallStdProc (opStd.X2C_HALT);
END GenHalt;

PROCEDURE (self: SelfAsmPtr) GenRaise* (is_assert: BOOLEAN);
BEGIN
  IF is_assert THEN
    CallStdProc (opStd.X2C_ASSERT_F);
  ELSE
    CallStdProc (opStd.X2C_TRAP_F);
  END;
END GenRaise;

PROCEDURE (self: SelfAsmPtr) GenSkipTrap* (j:D.Condition; intno: Emit.Trap; pos-: ir.TPOS);
  VAR i: INT;
      b: BOOLEAN;
      buf, buf1: ARRAY 1024 OF CHAR;
BEGIN
  b := (LAST_FLAGS_OFFSET = sg.code_len);
  IF intno = Emit.IntOTrap THEN
    Text(Tab + "into");
  ELSE
    i := Emit.Traps [intno];
    IF i < 0 THEN
      IF at.OptimizeTraps IN at.COMP_MODE THEN
        IF Emit.TrapUsages[intno].Used = FALSE THEN
          Emit.TrapUsages[intno].Used := TRUE;
          ObjNames.makename (opStd.Proc(-i), buf);
          FormStr.print(buf1, TRAP_LABEL_FORMAT, at.curr_proc.name^, buf);
        END;

        CASE Emit.InverseCond(j) OF
        | D.JO:  Text(Tab + "jo"  + Tab);
        | D.JNO: Text(Tab + "jno" + Tab);
        | D.JB:  Text(Tab + "jb"  + Tab);
        | D.JNC: Text(Tab + "jnc" + Tab);
        | D.JE:  Text(Tab + "je"  + Tab);
        | D.JNE: Text(Tab + "jne" + Tab);
        | D.JBE: Text(Tab + "jbe" + Tab);
        | D.JA:  Text(Tab + "ja"  + Tab);
        | D.JS:  Text(Tab + "js"  + Tab);
        | D.JNS: Text(Tab + "jns" + Tab);
        | D.JPE: Text(Tab + "jpe" + Tab);
        | D.JPO: Text(Tab + "jpo" + Tab);
        | D.JL:  Text(Tab + "jl"  + Tab);
        | D.JGE: Text(Tab + "jge" + Tab);
        | D.JLE: Text(Tab + "jle" + Tab);
        | D.JG:  Text(Tab + "jg"  + Tab);
        END;
        Text(buf1);
        Enter;
      ELSE
        CASE j OF
        | D.JO:  Text(Tab + "jo"  + Tab + "0f");
        | D.JNO: Text(Tab + "jno" + Tab + "0f");
        | D.JB:  Text(Tab + "jb"  + Tab + "0f");
        | D.JNC: Text(Tab + "jnc" + Tab + "0f");
        | D.JE:  Text(Tab + "je"  + Tab + "0f");
        | D.JNE: Text(Tab + "jne" + Tab + "0f");
        | D.JBE: Text(Tab + "jbe" + Tab + "0f");
        | D.JA:  Text(Tab + "ja"  + Tab + "0f");
        | D.JS:  Text(Tab + "js"  + Tab + "0f");
        | D.JNS: Text(Tab + "jns" + Tab + "0f");
        | D.JPE: Text(Tab + "jpe" + Tab + "0f");
        | D.JPO: Text(Tab + "jpo" + Tab + "0f");
        | D.JL:  Text(Tab + "jl"  + Tab + "0f");
        | D.JGE: Text(Tab + "jge" + Tab + "0f");
        | D.JLE: Text(Tab + "jle" + Tab + "0f");
        | D.JG:  Text(Tab + "jg"  + Tab + "0f");
        END;
        Enter;
        CallStdProc (-i);
        Text("0:");
        Enter;
      END;
    ELSE
      Text(Tab + "int" + Tab + "$");
      Number(i);
      Enter;
    END;
  END;
  IF b THEN
    LAST_FLAGS_OFFSET := sg.code_len;
  END;

END GenSkipTrap;

--------------------------------------------------------------------------------
--
--  Вещественная арифметика
--
--------------------------------------------------------------------------------

PROCEDURE FOPSizeSuff(sz: SizeType);
BEGIN
  CASE sz OF
    4:  Char('s') |
    8:  Char('l') |
    10,
    12: Char('t') |
  END;
END FOPSizeSuff;

PROCEDURE OutFOPCommand(name-: ARRAY OF CHAR; sz: SizeType);
BEGIN
  at.UseFloatOps := TRUE;
  Text(name);
  FOPSizeSuff(sz);
  Space;
END OutFOPCommand;

PROCEDURE OutFOP (op: D.FloatOp);
BEGIN
  at.UseFloatOps := TRUE;
  CASE op OF
    | D.FADD:    Text(Tab + "fadd");
    | D.FMUL:    Text(Tab + "fmul");
    | D.FDIV:    Text(Tab + "fdiv");
    | D.FDIVR:   Text(Tab + "fdivr");
    | D.FSUB:    Text(Tab + "fsub");
    | D.FSUBR:   Text(Tab + "fsubr");
  END;
END OutFOP;

--------------------------------------------------------------------------------

PROCEDURE (self: SelfAsmPtr) GenMoveTOS_M* (a-: AddrMode; sz: SizeType);
BEGIN
  OutFOPCommand(Tab + "fld",sz);
  OutMem(a);
  Enter;
END GenMoveTOS_M;

PROCEDURE (self: SelfAsmPtr) GenMoveTOS_INum* (v: VALUE; sz: SizeType);
BEGIN
  OutFOPCommand(Tab + "fld",sz);
  OutObject(CodeDef.new_float(v,sz));
  Enter;
END GenMoveTOS_INum;

PROCEDURE (self: SelfAsmPtr) GenMoveTOS_STi* (i: LONGINT);
BEGIN
  at.UseFloatOps := TRUE;
  Text(Tab + "fld" + Tab + "%st(");
  Number(i);
  Char(')');
  Enter;
END GenMoveTOS_STi;

PROCEDURE (self: SelfAsmPtr) GenFILD* (a-: AddrMode; sz: SizeType);
BEGIN
  at.UseFloatOps := TRUE;
  OutCommand(Tab + "fild",sz);
  OutMem(a);
  Enter;
END GenFILD;

PROCEDURE (self: SelfAsmPtr) GenMoveM_TOS* (a-: AddrMode; sz: SizeType);
BEGIN
  OutFOPCommand(Tab + "fstp",sz);
  OutMem(a);
  Enter;
END GenMoveM_TOS;

PROCEDURE (self: SelfAsmPtr) GenMoveM_ST0* (a-: AddrMode; sz: SizeType);
BEGIN
  OutFOPCommand(Tab + "fst",sz);
  OutMem(a);
  Enter;
END GenMoveM_ST0;

PROCEDURE (self: SelfAsmPtr) GenMoveSTi_TOS* (i: LONGINT);
BEGIN
  at.UseFloatOps := TRUE;
  Text(Tab + "fstp" + Tab + "%st(");
  Number(i);
  Char(')');
  Enter;
END GenMoveSTi_TOS;

PROCEDURE (self: SelfAsmPtr) GenMoveSTi_ST0* (i: LONGINT);
BEGIN
  at.UseFloatOps := TRUE;
  Text(Tab + "fst" + Tab + "%st(");
  Number(i);
  Char(')');
  Enter;
END GenMoveSTi_ST0;

PROCEDURE (self: SelfAsmPtr) GenFOpSTi_TOS* (op: D.FloatOp; i: SHORTINT);
BEGIN
  OutFOP(op);
  Text("p" + Tab + "%st, %st(");
  Number(i);
  Char(')');
  Enter;
END GenFOpSTi_TOS;

PROCEDURE (self: SelfAsmPtr) GenFOpSTi_ST0* (op: D.FloatOp; i: SHORTINT);
BEGIN
  OutFOP(op);
  Text(Tab + "%st, %st(");
  Number(i);
  Char(')');
  Enter;
END GenFOpSTi_ST0;

PROCEDURE (self: SelfAsmPtr) GenFOpST0_STi* (op: D.FloatOp; i: SHORTINT);
BEGIN
  OutFOP(op);
  Text(Tab + "%st(");
  Number(i);
  Text("), %st");
  Enter;
END GenFOpST0_STi;

PROCEDURE (self: SelfAsmPtr) GenFOpST0_M* (op: D.FloatOp; a-: AddrMode; sz: SizeType);
BEGIN
  OutFOP(op);
  FOPSizeSuff(sz);
  Space;
  OutMem(a);
  Enter;
END GenFOpST0_M;

PROCEDURE (self: SelfAsmPtr) GenFOpST0_IM* (op: D.FloatOp; a-: AddrMode; sz: SizeType);

BEGIN
  at.UseFloatOps := TRUE;
  CASE op OF
    | D.FADD:    Text(Tab + "fiadd");
    | D.FMUL:    Text(Tab + "fimul");
    | D.FDIV:    Text(Tab + "fidiv");
    | D.FDIVR:   Text(Tab + "fidivr");
    | D.FSUB:    Text(Tab + "fisub");
    | D.FSUBR:   Text(Tab + "fisubr");
  END;
  CASE sz OF
    2: Char('s') |  -- придурки, блин.
    4: Char('l') |  
  END;
  Space;   
  OutMem(a);
  Enter;
END GenFOpST0_IM;

PROCEDURE (self: SelfAsmPtr) GenFOpST0_INum* (op: D.FloatOp; v: VALUE; sz: SizeType);
BEGIN
  OutFOP(op);
  FOPSizeSuff(sz);
  Space;
  OutObject(CodeDef.new_float(v,sz));
  Enter;
END GenFOpST0_INum;

PROCEDURE (self: SelfAsmPtr) GenFOp* (code: D.FloatOp);
BEGIN
  at.UseFloatOps := TRUE;
  CASE code OF
    | D.FXCH:    Text(Tab + "fxch");
    | D.FABS:    Text(Tab + "fabs");
    | D.FCHS:    Text(Tab + "fchs");
    | D.FLD1:    Text(Tab + "fld1");
    | D.FLDL2E:  Text(Tab + "fldl2e");
    | D.FLDPI:   Text(Tab + "fldpi");
    | D.FLDLG2:  Text(Tab + "fldlg2");
    | D.FLDLN2:  Text(Tab + "fldln2");
    | D.FLDZ:    Text(Tab + "fldz");
    | D.F2XM1:   Text(Tab + "f2xm1");
    | D.FYL2X:   Text(Tab + "fyl2x");
    | D.FPTAN:   Text(Tab + "fptan");
    | D.FPATAN:  Text(Tab + "fpatan");
    | D.FPREM:   Text(Tab + "fprem");
    | D.FSQRT:   Text(Tab + "fsqrt");
    | D.FSCALE:  Text(Tab + "fscale");
    | D.FSIN:    Text(Tab + "fsin");
    | D.FCOS:    Text(Tab + "fcos");
  END;
  Enter;
END GenFOp;

PROCEDURE (self: SelfAsmPtr) GenFComTOS_TOS*;
BEGIN
  at.UseFloatOps := TRUE;
  Text(Tab + "fcompp");
  Enter;
END GenFComTOS_TOS;

PROCEDURE (self: SelfAsmPtr) GenFComTOS_INum* (v: VALUE; sz: SizeType);
BEGIN
  OutFOPCommand(Tab + "fcomp",sz);
  OutObject(CodeDef.new_float(v,sz));
  Enter;
END GenFComTOS_INum;

PROCEDURE (self: SelfAsmPtr) GenFComTOS_M* (a-: AddrMode; sz: SizeType);
BEGIN
  OutFOPCommand(Tab + "fcomp",sz);
  OutMem(a);
  Enter;
END GenFComTOS_M;

PROCEDURE (self: SelfAsmPtr) GenFComTOS_STi* (i: SHORTINT);
BEGIN
  at.UseFloatOps := TRUE;
  Text(Tab + "fcomp" + Tab + "%st(");
  Number(i);
  Char(')');
  Enter;
END GenFComTOS_STi;

PROCEDURE (self: SelfAsmPtr) GenFComTOS_IM* (a-: AddrMode; sz: SizeType);
BEGIN
  at.UseFloatOps := TRUE;
  OutCommand(Tab + "ficomp",sz);
  OutMem(a);
  Enter;
END GenFComTOS_IM;

PROCEDURE (self: SelfAsmPtr) GenFstsw*;
BEGIN
  at.UseFloatOps := TRUE;
  Text(Tab + "fstsw" + Tab + "%ax");
  Enter;
END GenFstsw;

PROCEDURE (self: SelfAsmPtr) GenCallToOrdinal* (card: BOOLEAN;
                                             u,m: D.PhysRegSet; f: BOOLEAN; sz: SizeType);
BEGIN
  at.UseFloatOps := TRUE;
  IF sz = 8 THEN
    CallStdProc (opStd.X2J_TRUNCI64);
  ELSIF card THEN
    CallStdProc (opStd.X2C_TRUNCC)
  ELSE
    CallStdProc (opStd.X2C_TRUNCI)
  END;
END GenCallToOrdinal;

VAR e: SelfAsmPtr;
PROCEDURE InitOutput*;
BEGIN
  NEW (e);
  Emit.full := e;
  Emit.trace := e;
  lab:=Emit.ZEROLABEL;
  IF env.config.Option("DOTPREFIXEDLABELS") THEN 
    LabelPrefix:=".L";
  ELSE
    LabelPrefix:="L";
  END;
END InitOutput;









PROCEDURE (self: SelfAsmPtr) EndGenSkipTrap*();
VAR
  i: Emit.Trap;
  not_used: BOOLEAN;
  buf, buf1: ARRAY 1024 OF CHAR;
BEGIN
  not_used := TRUE;

  FOR i:=MIN(Emit.Trap) TO MAX(Emit.Trap) DO
    IF Emit.TrapUsages[i].Used = TRUE THEN
      IF not_used THEN
        Enter;
        Text("#Used Traps");
        Enter;
        Enter;
        not_used := FALSE;
      END;
      ObjNames.makename (opStd.Proc(-Emit.Traps[i]), buf);
      FormStr.print(buf1, TRAP_LABEL_FORMAT, at.curr_proc.name^, buf);
      Text(buf1);
      Text(":");
      Enter;
      CallStdProc(-Emit.Traps[i]);
    END;
  END;
END EndGenSkipTrap;




END EmitGAS.

(*
  2DO:

  1) А не равен ли PUSH_SIZE(да и FLOAT_SIZE тоже) сам по себе 0?
  2) Сделать генерацию позиций TPOS.

*)


