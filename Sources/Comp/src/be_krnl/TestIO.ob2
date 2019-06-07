MODULE TestIO;
--<* NEW noregvars+ *>

IMPORT ir, BitVect, SeqFile, TextIO, Color,
       prc := opProcs;
IMPORT gr := ControlGraph;
IMPORT Strings;
IMPORT SYSTEM;
IMPORT env := xiEnv;
IMPORT Options;
IMPORT pc  := pcK;
IMPORT at := opAttrs;
<* IF DEFINED(OVERDYE) AND OVERDYE THEN *>
IMPORT od  := OverDye;
<* END *>
IMPORT nms := ObjNames;
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
IMPORT hgl;
IMPORT hgr;
<* END *>
IMPORT Printf;
IMPORT WholeStr;
IMPORT DStrings;
IMPORT xfs:=xiFiles;
IMPORT Dirs;
--IMPORT ssa;
(*
    Correspondence of equation "QFILE" to parts of IR being flushed to qfile

i (initial)             - just after  IR building
l (before FindLoops)    - just before ir.FindLoops call
b (before ConvertToSSA) - just before SSA Conversion
s (after  ConvertToSSA) - just after  SSA Conversion
f (final)               - just before code generation

    Details of SSA Conversion
R (after ReplaceLoadsStores)
M (after MakeLoadsStores)
P (after PlaceFi)
V (after ReplaceRealVars)

    Option "QFILE" adds flags "i" "s" "f" to equation "QFILE"

    For ex.
    jc hello.class -qfile=if
    jc hello.class +qfile -qfile=bR
    jc hello.class +qfile

*)
TYPE
        SetOfChar = PACKEDSET OF CHAR;
VAR
        parts : SetOfChar;
        curr_proc_name*: pc.STRING;
        need_proc_name: pc.STRING;
TYPE
    ArrayOfString = ARRAY ir.Operation OF ARRAY 14 OF CHAR;
CONST
    TriadeName* = ArrayOfString
    { "o_invalid",
      "o_FI",
      "o_ASSIGN",
      "o_COPY",
      "o_VAL",
      "o_CAST",
      "o_CAP",
      "o_ABS",
      "o_COMPLEX",
      "o_RE",
      "o_IM",
      "o_ADD",
      "o_MUL",
      "o_MULH",
      "o_DIV",
      "o_DVD",
      "o_MOD",
      "o_REM",
      "o_POWER",
      "o_AND",
      "o_OR",
      "o_ANDNOT",
      "o_XOR",
      "o_NOT",
      "o_INCL",
      "o_EXCL",
      "o_LOSET",
      "o_HISET",
      "o_SHL",
      "o_SHR",
      "o_SAR",
      "o_ROL",
      "o_ROR",
      "o_SGNEXT",
      "o_BF_GET",
      "o_BF_PUT",
      "o_CALL",
      "o_RET",
      "o_CHECKNIL",
      "o_CHECKLO",
      "o_CHECKHI",
      "o_STOP",
      "o_ERROR",
      "o_LOAD",
      "o_STORE",
      "o_LOADR",
      "o_STORER",
      "o_FORSTART",
      "o_FORCONT",
      "o_EQ",
      "o_LE",
      "o_LESET",
      "o_IN",
      "o_ODD",
      "o_CASE",
      "o_GOTO",
      "o_GETPAR",
      "o_MOVE_EQ",
      "o_MOVE_LE",
      "o_ALLOCA",
      "O_NEG",
      "O_SUB",
      "O_PUTPAR",
      "O_COMMA",
      "O_PAR",
      "O_RETFUN",
      "O_SHIFT",
      "O_LOGICAL",
      "O_FBIN",
      "O_FUNARY",
      "O_FLE",
      "O_FEQ",
      "O_SIN",
      "O_COS",
      "O_TAN",
      "O_ATAN",
      "O_EXP",
      "O_SQRT",
      "O_LN",
      "O_LG",
      "O_EMPTY",
      "O_NEG_ABS",
      "__O_BASE__",
      "o_CLEAR",
      "o_HIWORD",
      "o_CLINIT",
      "o_CHECKB",
      "o_CMPSWAP",
      "o_SEQPOINT",
      "o_CONSTR",
      "o_CHECKNEQ"
       };

(* -------------------------------------------------------------------------- *)

TYPE    INT = ir.INT;

VAR
        pos:    INT;
        file:   SeqFile.ChanId;
        fname:  ARRAY 256 OF CHAR;    -- name of a Q-file
        qfileNo:LONGINT; -- number of current qfile from the compiler run


(* -------------------------------------------------------------------------- *)

PROCEDURE Open (s-: ARRAY OF CHAR);
  VAR res: SeqFile.OpenResults;
BEGIN
  SeqFile.OpenWrite (file, s, SeqFile.write + SeqFile.old, res);
END Open;

(* -------------------------------------------------------------------------- *)

PROCEDURE Append(s-: ARRAY OF CHAR);
  VAR res: SeqFile.OpenResults;
BEGIN
  SeqFile.OpenAppend (file, s, SeqFile.write + SeqFile.old, res);
END Append;

(* -------------------------------------------------------------------------- *)

PROCEDURE Close;
BEGIN
    SeqFile.Close (file);
END Close;

(* -------------------------------------------------------------------------- *)

--VAR nodeLabel: DStrings.String;

PROCEDURE WrChar (c: CHAR);
BEGIN
    TextIO.WriteChar (file, c);
--    DStrings.Append (c, nodeLabel);
    INC (pos);
END WrChar;

(* -------------------------------------------------------------------------- *)

PROCEDURE Ln;
BEGIN
    TextIO.WriteLn (file);
--    DStrings.Append ("\n", nodeLabel);
    pos := 0;
END Ln;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrStr (s-: ARRAY OF CHAR);
VAR i: INT;
BEGIN
    FOR i:=0 TO LEN(s)-1 DO
        IF s[i] = 0X THEN
            RETURN;
        END;
        WrChar (s [i]);
    END;
END WrStr;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrInt (n: INT);
BEGIN
    IF n < 0 THEN
        WrChar ('-');
        n := - n;
    END;
    IF n >= 10 THEN
        WrInt (n DIV 10);
    END;
    WrChar (CHR (n MOD 10 + 48));
END WrInt;

(* -------------------------------------------------------------------------- *)

PROCEDURE Tab (p: INT);
BEGIN
    REPEAT
        WrChar (' ');
    UNTIL p <= pos;
END Tab;

(* -------------------------------------------------------------------------- *)

PROCEDURE TabMinus (p: INT);
BEGIN
    REPEAT
        WrChar ('-');
    UNTIL p <= pos;
END TabMinus;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrLocal (l: ir.Local);
BEGIN
    IF (ir.o_Volatile IN ir.Locals^[l].Options) THEN
        WrStr ('!!!');
    END;
    IF ir.Locals^[l].Name <> NIL THEN
        WrStr (ir.Locals^[l].Name^);
    ELSE
        WrStr ('tmp');
        WrInt (ORD(l));
    END;
END WrLocal;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrVar (r: ir.VarNum);
BEGIN
    IF ir.Vars^[r].LocalNo = ir.TEMPORARY THEN
        WrChar ('t');
    ELSE
        WrLocal (ir.Vars^[r].LocalNo);
        WrChar  ('_');
    END;
    WrInt  (ORD(r));
END WrVar;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrProcName (p: ir.ProcNum);
  VAR name: ARRAY 256 OF CHAR;--ir.NameType;
BEGIN
--  name := prc.ProcName(p);
    nms.makename( prc.ProcList[p].obj, name );
  WrStr(name);
END WrProcName;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrType (t: ir.TypeType; s: ir.SizeType);
BEGIN
    WrInt  (s);
    WrChar (' ');
    IF t = ir.t_void THEN
        WrStr ('void');
    ELSIF t = ir.t_int THEN
        WrStr (' int');
    ELSIF t = ir.t_unsign THEN
        WrStr ('unsg');
    ELSIF t = ir.t_float THEN
        WrStr (' flt');
    ELSIF t = ir.t_ref THEN
        WrStr (' ref');
    ELSIF t = ir.t_arr THEN
        WrStr (' arr');
    ELSIF t = ir.t_rec THEN
        WrStr (' rec');
    ELSIF t = ir.t_flxarr THEN
        WrStr (' flx');
    ELSE
        WrStr ('   ?');
--        WrInt  (t);
    END;
END WrType;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrAssign (p: ir.TriadePtr);
BEGIN
    CASE p^.Tag OF
    | ir.y_RealVar:
        WrLocal (p^.Name);
    | ir.y_Variable:
        WrVar   (p^.Name);
    | ir.y_Nothing:
        RETURN;
    END;
    WrStr (' = ');
END WrAssign;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrUseList (q: ir.TriadePtr);
VAR par: ir.ParamPtr;
      n: INT;
      p: ir.TriadePtr;
BEGIN
    IF q^.Tag = ir.y_Variable THEN
         Tab    (60);
         WrChar ('(');
         par := ir.FirstUse (q^.Name);
         WHILE (par <> NIL) DO
             p := par^.triade;
             WrInt  (ORD(p^.NodeNo));
             WrChar ('.');
             n := 0;
             WHILE p^.Prev <> NIL DO
                 p := p^.Prev;
                 INC (n);
             END;
             WrInt (n);
             par := par^.next;
             IF par <> NIL THEN
                 WrChar ('_');
             END;
         END;
         WrChar (')');
    END;
END WrUseList;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrTextPos (p: ir.TPOS);
VAR
    fname: ir.NameType;
    line, col: INT;
BEGIN
  IF NOT p.IsNull() THEN
    p.unpack(fname,line,col);
    WrChar('['); WrStr(fname^); WrChar(':'); WrInt(line+1); WrChar(':'); WrInt(col+1); WrChar(']');
  END;
END WrTextPos;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrParam (p: ir.ParamPtr);
VAR s: ARRAY 200 OF CHAR;
BEGIN
    IF ir.popt_LastUse IN p.options THEN
        WrChar ('{');
    END;
    CASE p^.tag OF
    | ir.y_Nothing:
        WrStr ('UNDEFINED');
    | ir.y_NumConst:
        p^.value^.value_to_str (s, pc.flag_c);
        WrStr (s);
    | ir.y_RealConst:
        p^.value^.value_to_str (s, pc.flag_c);
        WrStr (s);
    | ir.y_RealVar:
        WrLocal (p^.name);
    | ir.y_Variable:
        WrVar (p^.name);
    | ir.y_ProcConst:
        WrProcName (VAL(ir.ProcNum, p^.name));
        IF p^.offset <> 0 THEN
            IF p^.offset > 0 THEN
                WrChar ('+');
            END;
            WrInt (p^.offset);
        END;
    | ir.y_AddrConst:
        IF p^.offset <> 0 THEN
            WrChar ('(');
        END;
        WrChar ('&');
        IF p^.name = MAX (ir.VarNum) THEN
            WrStr ('CONST');
        ELSE
            WrLocal (p^.name);
        END;
        IF p^.offset > 0 THEN
            WrChar ('+');
        END;
        IF p^.offset <> 0 THEN
            WrInt  (p^.offset);
            WrChar (')');
        END;
    | ELSE
        WrChar ('?');
        WrInt  (ORD(p^.tag));
    END;
    IF ir.popt_LastUse IN p.options THEN
        WrChar ('}');
    END;
    WrTextPos(p^.position);
END WrParam;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrOp (o: ir.Operation);
BEGIN
    CASE o OF
    | ir.o_add:
        WrStr (' + ');
    | ir.o_mul:
        WrStr (' * ');
    | ir.o_mulh:
        WrStr (' *h ');
    | ir.o_div:
        WrStr (' DIV ');
    | ir.o_power:
        WrStr (' ** ');
    | ir.o_dvd:
        WrStr (' / ');
    | ir.o_mod:
        WrStr (' MOD ');
    | ir.o_rem:
        WrStr (' REM ');
    | ir.o_and:
        WrStr (' AND ');
    | ir.o_or:
        WrStr (' or ');
    | ir.o_andnot:
        WrStr (' andnot ');
    | ir.o_xor:
        WrStr (' xor ');
    | ir.o_incl:
        WrStr (' incl ');
    | ir.o_excl:
        WrStr (' excl ');
    | ir.o_shl:
        WrStr (' << ');
    | ir.o_shr:
        WrStr (' >> ');
    | ir.o_sar:
        WrStr (' >a> ');
    | ir.o_rol:
        WrStr (' rol ');
    | ir.o_ror:
        WrStr (' ror ');
    | ir.o_shift:
        WrStr (' shift ');
    | ir.o_logical:
        WrStr (' logical ');
    | ir.o_sub:
        WrStr (' sub ');
    | ir.o_fbin:
        WrStr (' fbin ');
    ELSE
        WrChar ('?');
        WrInt  (ORD(o));
        WrChar ('?');
    END;
END WrOp;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrTriadeTextPos (p: ir.TriadePtr);
VAR
   i : INTEGER;
BEGIN
  IF NOT p^.Position.IsNull() THEN
    WrTextPos(p^.Position);
  ELSIF (p^.Op <> ir.o_fi) & (p^.Params <> NIL) THEN
      FOR i:=0 TO LEN (p^.Params^)-1 DO
          IF (p^.Params^[i]^.tag IN { ir.y_NumConst, ir.y_RealConst }) THEN
              WrTextPos(p^.Params^[i]^.value^.pos);
          END;
      END;
  END;
END WrTriadeTextPos;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrParamTypes (p: ir.TriadePtr);
VAR i: INTEGER;
BEGIN
    IF p^.Params # NIL THEN
        FOR i:=0 TO LEN(p^.Params^)-1 DO
            WrStr   (' <');
            IF p^.Params^[i].tag = ir.y_Variable THEN
                WrType (ir.Vars[p^.Params^[i].name].Def.ResType,
                         ir.Vars[p^.Params^[i].name].Def.ResSize);
            ELSE
                WrStr   ('     ?');
            END;
            WrStr   ('>');
            WrTextPos(p.Position);
        END;
    END;
END WrParamTypes;
(* -------------------------------------------------------------------------- *)

PROCEDURE WrTail (p: ir.TriadePtr);
BEGIN
    Tab (60);
    IF p.Op IN ir.OpSet{ir.o_putpar, ir.o_le, ir.o_eq, ir.o_cmpswap} THEN
        WrType (p^.OpType, p^.OpSize);
    ELSE
        WrType (p^.ResType, p^.ResSize);
    END;
    IF p.Op = ir.o_assign THEN
        WrStr   ('<');
        IF p^.Tag = ir.y_Variable THEN
            WrType (ir.Vars[p.Name].Def.ResType,
                     ir.Vars[p.Name].Def.ResSize);
        END;
        WrStr   ('> ');
    END;
    IF ir.o_Dangerous IN p^.Options THEN
        WrChar ('!');
    END;
    IF ir.o_Checked IN p^.Options THEN
        WrChar ('+');
    END;
    IF ir.o_Removable IN p^.Options THEN
        WrStr ('(m)');
    END;
    WrTriadeTextPos (p);
    WrUseList (p);
    Tab (100);
    WrParamTypes(p);
END WrTail;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrArc (n: ir.Node; j: INT);
VAR a: ir.Arc;
BEGIN
     a := ir.Nodes^[n].OutArcs^[j];
     WrInt (ORD(a));
END WrArc;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrArcs (n: ir.Node);
VAR i: INT;
BEGIN
    Tab (35);
    IF ir.Nodes^[n].NOut = 0 THEN
        WrStr ('(no arcs');
    ELSE
        WrStr ('(arc');
        IF ir.Nodes^[n].NOut <> 1 THEN
            WrChar ('s');
        END;
        WrStr (': ');
        FOR i := 0 TO ir.Nodes^[n].NOut - 2 DO
            WrArc (n, i);
            WrStr (', ');
        END;
        WrArc (n, ir.Nodes^[n].NOut - 1);
    END;
    WrChar (')');
END WrArcs;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrFor (p: ir.TriadePtr; s-: ARRAY OF CHAR);
BEGIN
    WrAssign (p);
    WrStr    (s);
    WrParam  (p^.Params^[0]);
    WrStr    (', ');
    WrParam  (p^.Params^[1]);
    WrStr    (', ');
    WrParam  (p^.Params^[2]);
    WrStr    (' then ');
    WrInt    (ORD(ir.Nodes^[p^.NodeNo].Out^[0]));
    IF ir.Nodes^[p^.NodeNo].NOut > 1 THEN
        WrStr (' else ');
        WrInt (ORD(ir.Nodes^[p^.NodeNo].Out^[1]));
    END;
    WrArcs (p^.NodeNo);
END WrFor;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrUnary (p: ir.TriadePtr; name-: ARRAY OF CHAR);
BEGIN
    WrAssign  (p);
    WrStr     (name);
    WrStr     (' (');
    WrParam   (p^.Params^[0]);
    WrChar    (')');
END WrUnary;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrCmpswap (p: ir.TriadePtr);
BEGIN
    WrStr   ('cmpswap ( * ');
    WrParam (p^.Params^[0]);
    WrStr  (', ');
    WrParam (p^.Params^[1]);
    WrStr   (', ');
    WrParam (p^.Params^[2]);
    WrStr   (' then ');
    WrInt   (ORD(ir.Nodes^[p^.NodeNo].Out^[0]));
    WrStr   (' else ');
    WrInt   (ORD(ir.Nodes^[p^.NodeNo].Out^[1]));
    WrArcs  (p^.NodeNo);
END WrCmpswap;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrIf (p: ir.TriadePtr; name-: ARRAY OF CHAR);
BEGIN
    WrStr   ('if ');
    WrParam (p^.Params^[0]);
    WrChar  (' ');
    WrStr   (name);
    WrChar  (' ');
    WrParam (p^.Params^[1]);
    WrStr   (' then ');
    WrInt   (ORD(ir.Nodes^[p^.NodeNo].Out^[0]));
    WrStr   (' else ');
    WrInt   (ORD(ir.Nodes^[p^.NodeNo].Out^[1]));
    WrArcs  (p^.NodeNo);
END WrIf;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrTriade (p: ir.TriadePtr);
CONST TRPOS = 45;
VAR i: INT;
   nm: pc.STRING;
BEGIN
    WrStr   ('  ');
    CASE p^.Op OF
    | ir.o_getpar:
        WrAssign (p);
        WrStr    ('get (');
        WrInt    (p^.NPar);
        WrChar   (')');
    | ir.o_stop:
        WrStr ('stop ');
        IF p^.Params <> NIL THEN
            WrParam (p^.Params^[0]);
        END;
    | ir.o_base:
        WrAssign (p);
        WrStr ('__BASE__ ');
    | ir.o_retfun:
        WrStr ('retfun ');
        WrParam (p^.Params^[0]);
    | ir.o_ret:
        WrStr ('return ');
        IF p^.Params <> NIL THEN
            WrParam (p^.Params^[0]);
        END;
    | ir.o_goto:
        WrStr  ('goto ');
        WrInt  (ORD(ir.Nodes^[p^.NodeNo].Out^[0]));
        WrArcs (p^.NodeNo);
    | ir.o_le:
        IF ir.o_IncomparableAsTrue IN p.Options THEN
          WrIf (p, '<=r');
        ELSE
          WrIf (p, '<=');
        END;
    | ir.o_fle:
        IF ir.o_IncomparableAsTrue IN p.Options THEN
          WrIf (p, 'f<=r');
        ELSE
          WrIf (p, 'f<=');
        END;
    | ir.o_eq:
        IF ir.o_IncomparableAsTrue IN p.Options THEN
          WrIf (p, '=r');
        ELSE
          WrIf (p, '=');
        END;
    | ir.o_cmpswap:
        WrCmpswap (p);
    | ir.o_seqpoint:
        WrStr('seqpoint');
    | ir.o_constr:
        WrAssign (p);
        WrStr   (' constr( ');
        WrParam (p^.Params^[0]);
        WrStr   (' , ');
        WrParam (p^.Params^[1]);
        WrStr   (' )');
    | ir.o_feq:
        IF ir.o_IncomparableAsTrue IN p.Options THEN
          WrIf (p, 'f=r');
        ELSE
          WrIf (p, 'f=');
        END;
    | ir.o_leset:
        WrIf (p, '<= set');
    | ir.o_in:
        WrIf (p, 'in');
    | ir.o_odd:
        WrStr   ('if odd (');
        WrParam (p^.Params^[0]);
        WrStr   (') then ');
        WrInt   (ORD(ir.Nodes^[p^.NodeNo].Out^[0]));
        WrStr   (' else ');
        WrInt   (ORD(ir.Nodes^[p^.NodeNo].Out^[1]));
        WrArcs  (p^.NodeNo);
    | ir.o_case:
        WrStr   ('case ');
        IF ir.o_Constant IN p^.Options THEN         WrStr   ('(const) '); END;
        WrParam (p^.Params^[0]);
        WrStr   (' of ');
        FOR i:=1 TO LEN(p^.Params^)-2 BY 2 DO
            WrParam (p^.Params^[i]);
            WrStr   ('..');
            WrParam (p^.Params^[i+1]);
            WrStr   (': ');
            WrInt   (ORD(ir.Nodes^[p^.NodeNo].Out^[i DIV 2]));
            IF i <> (LEN (p^.Params^) - 2) THEN
                WrStr (', ');
            END;
        END;
        IF LEN (p^.Params^) DIV 2 <> ir.Nodes^[p^.NodeNo].NOut THEN
            WrStr (' else ');
            WrInt (ORD(ir.Nodes^[p^.NodeNo].Out^[ir.Nodes^[p^.NodeNo].NOut-1]));
        END;
        WrArcs (p^.NodeNo);
    | ir.o_forstart:
        WrFor (p, 'for ');
    | ir.o_forcont:
        WrFor (p, 'forcont ');
    | ir.o_load:
        WrStr    ('Load ');
        WrAssign (p);
        IF ir.o_Volatile IN p^.Options THEN
            WrStr (' (volatile) ');
        END;
        WrParam  (p^.Params^[0]);
    | ir.o_store:
        WrStr    ('Store ');
        WrAssign (p);
        IF ir.o_Volatile IN p^.Options THEN
            WrStr (' (volatile) ');
        END;
        WrParam  (p^.Params^[0]);
    | ir.o_assign:
        WrAssign (p);
        WrParam  (p^.Params^[0]);
    | ir.o_storer:
        WrChar   ('*');
        IF ir.o_Volatile IN p^.Options THEN
            WrStr (' (volatile) ');
        END;
        WrParam  (p^.Params^[0]);
        WrStr    (' = ');
        WrParam  (p^.Params^[1]);
    | ir.o_loadr:
        WrAssign (p);
        WrChar   ('*');
        IF ir.o_Constant IN p^.Options THEN
            WrStr (' (const) ');
        END;
        IF ir.o_Volatile IN p^.Options THEN
            WrStr (' (volatile) ');
        END;
        WrParam  (p^.Params^[0]);
    | ir.o_checknil:
        WrStr   ('check not nil: ');
        WrParam  (p^.Params^[0]);
    | ir.o_checklo:
        WrStr   ('check: ');
        WrParam (p^.Params^[0]);
        WrStr   (' >= ');
        WrParam (p^.Params^[1]);
    | ir.o_checkhi:
        WrStr   ('check: ');
        WrParam (p^.Params^[0]);
        WrStr   (' < ');
        WrParam (p^.Params^[1]);
    | ir.o_checkneq:
        WrStr   ('check: ');
        WrParam (p^.Params^[0]);
        WrStr   (' != ');
        WrParam (p^.Params^[1]);
    | ir.o_checkb:
        WrStr   ('check: 0 <= ');
        WrParam (p^.Params^[0]);
        WrStr   (' < ');
        WrParam (p^.Params^[1]);
    | ir.o_copy:
        WrStr   ('copy ');
        WrParam (p^.Params^[0]);
        WrStr   (' -> ');
        WrParam (p^.Params^[1]);
        WrStr   (' (');
        WrParam (p^.Params^[2]);
        WrChar  (')');
        IF p^.NPar # 0 THEN
          WrStr ('  %');
          WrInt (p^.NPar);
        END;
    | ir.o_putpar:
        WrStr   ('put ');
        WrParam (p^.Params^[0]);
    | ir.o_abs:
        WrUnary (p, 'abs');
    | ir.o_sin:
        WrUnary (p, 'sin');
    | ir.o_cos:
        WrUnary (p, 'cos');
    | ir.o_tan:
        WrUnary (p, 'tan');
    | ir.o_atan:
        WrUnary (p, 'atan');
    | ir.o_exp:
        WrUnary (p, 'exp');
    | ir.o_sqrt:
        WrUnary (p, 'sqrt');
    | ir.o_ln:
        WrUnary (p, 'ln');
    | ir.o_lg:
        WrUnary (p, 'lg');
    | ir.o_neg:
        WrUnary (p, 'neg');
    | ir.o_funary:
        WrUnary (p, 'funary');
    | ir.o_cap:
        WrUnary (p, 'cap');
    | ir.o_sgnext:
        WrUnary (p, 'sex');
    | ir.o_val:
        WrAssign (p);
        WrStr    ('val ((');
        WrType   (p^.OpType, p^.OpSize);
        WrStr    (') ');
        WrParam  (p^.Params^[0]);
        WrChar   (')');
    | ir.o_cast:
        WrAssign (p);
        WrStr    ('cast ((');
        WrType   (p^.OpType, p^.OpSize);
        WrStr    (') ');
        WrParam  (p^.Params^[0]);
        WrChar   (')');
    | ir.o_hiword:
        WrAssign (p);
        WrStr    ('hiword (');
        WrParam  (p^.Params^[0]);
        WrChar   (')');
    | ir.o_clinit:
        WrAssign (p);
        WrStr    ('clinit (');
        WrParam  (p^.Params^[0]);
        WrChar   (')');
   | ir.o_loset:
        WrAssign (p);
        WrStr    ('{ 0 .. ');
        WrParam (p^.Params^[0]);
        WrStr   (' }');
    | ir.o_hiset:
        WrAssign (p);
        WrStr    ('{ ');
        WrParam (p^.Params^[0]);
        WrStr   (' .. ');
        WrInt   (p^.ResSize * 8 - 1);
        WrStr   (' }');
    | ir.o_fi:
        WrAssign (p);
        WrStr    ('fi (');
        FOR i:=0 TO LEN(p^.Params^)-2 DO
            WrParam (p^.Params^[i]);
            WrStr   (', ');
        END;
        WrParam (p^.Params^[LEN(p^.Params^)-1]);
        WrChar  (')');
    | ir.o_move_eq:
        WrAssign (p);
        WrParam (p^.Params^[0]);
        WrStr   (' = ');
        WrParam (p^.Params^[1]);
        WrStr   (' ? ');
        WrParam (p^.Params^[2]);
        WrStr   (' : ');
        WrParam (p^.Params^[3]);
    | ir.o_move_le:
        WrAssign (p);
        WrParam (p^.Params^[0]);
        WrStr   (' <= ');
        WrParam (p^.Params^[1]);
        WrStr   (' ? ');
        WrParam (p^.Params^[2]);
        WrStr   (' : ');
        WrParam (p^.Params^[3]);
    | ir.o_alloca:
        WrAssign (p);
        WrStr   ('alloca(');
        WrParam (p^.Params^[0]);
        WrChar  (')');
    | ir.o_clear:
        WrStr   ('clear ');
        WrParam (p^.Params^[0]);
        WrStr   (' ( ');
        WrParam (p^.Params^[1]);
        WrStr   (' bytes )');
    | ir.o_error:
        WrUnary (p, 'error');
        WrStr   (', ');
        WrParam (p^.Params^[1]);
        WrStr   (', ');
        WrParam (p^.Params^[2]);
    | ir.o_call:
        IF p^.Tag = ir.y_Nothing THEN
            WrStr ('Call ')
        ELSE
            WrAssign (p);
        END;
        WrParam (p^.Params^[0]);
        IF prc.ProtoList[p.Prototype].obj # NIL THEN
          WrStr(' @virt@ ');
          nm := prc.ProtoList[p.Prototype].obj.GetReadableName(TRUE);
          WrStr(nm^);
        END;
        WrChar(' ');
        IF LEN(p^.Params^) = 1 THEN
            IF p^.Tag <> ir.y_Nothing THEN
                WrStr (' ()');
            END;
        ELSE
            WrStr (' (');
            FOR i:=1 TO LEN (p^.Params^)-2 DO
                WrParam (p^.Params^[i]);
                WrStr   (', ');
            END;
            WrParam (p^.Params^[LEN(p^.Params^)-1]);
            WrChar  (')');
        END;
    ELSE
        WrAssign (p);
        IF (p^.Op = ir.o_add) & p^.Params^[0]^.reverse THEN
            WrStr ('- ');
        END;
        IF LEN (p^.Params^) = 1 THEN
            IF (p^.Op = ir.o_add) & NOT p^.Params^[0]^.reverse THEN
                WrStr ('+ ');
            ELSIF p^.Op = ir.o_not THEN
                WrStr ('NOT ');
            END;
        END;
        FOR i:=0 TO LEN (p^.Params^)-1 DO
            WrParam (p^.Params^[i]);
            IF (i + 1 <> LEN (p^.Params^)) THEN
                IF p^.Params^[i+1]^.reverse THEN
                    IF p^.Op = ir.o_add THEN
                        WrStr (' - ');
                    ELSE
                        WrStr ('-?-');
                    END;
                ELSE
                    WrOp (p^.Op);
                END;
            END;
        END;
    END;
    Tab (TRPOS);
    WrStr    (TriadeName[p^.Op]);
--    Tab (TRPOS+10);
--    WrInt    (ORD(p^.Op));
    WrTail (p);
END WrTriade;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrBitVect (p: BitVect.BitVector);
VAR l, n: ir.Local;
BEGIN
    IF Color.NNonTempVars <> ir.UNDEFINED THEN
        n := Color.NLocals;
    ELSE 
        n := ir.NLocals;
    END; 

    FOR l := 0 TO n-1 DO
      IF BitVect.In (p, ORD(l)) THEN
        WrLocal (l);
        WrChar  (' ');
      END;
    END;

END WrBitVect;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrTriades (p: ir.TriadePtr; n: ir.Node);
VAR i: INT;
BEGIN
    i := 0;
    WHILE (p <> NIL) DO
        WrInt  (ORD(n));
        WrChar ('.');
        IF i < 10 THEN WrStr(' '); END;
        WrInt  (i);
        WrTriade (p);
        Ln;
        IF p^.Read <> NIL THEN
            WrStr ('          Read:  ');
            WrBitVect (p^.Read);
            Ln;
        END;
        IF p^.Write <> NIL THEN
            WrStr ('          Write: ');
            WrBitVect (p^.Write);
            Ln;
        END;
        IF ir.o_NoReturn IN p^.Options THEN
            WrStr ('     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');
            Ln;
        END;
        p := p^.Next;
        INC (i);
    END;
END WrTriades;

(* -------------------------------------------------------------------------- *)

PROCEDURE GenMove (v: ir.VarNum; p: ir.ParamPtr);
BEGIN
    WrStr   ('  ');
    WrVar   (v);
    WrStr   (' := ');
    WrParam (p);
    Ln;
END GenMove;

PROCEDURE GenLoop (p: ir.VarNumArray; n: INT);
VAR i: INT;
BEGIN
    WrStr   ('  ');
    FOR i:=0 TO n-1 DO
        WrVar (p^[i]);
        WrStr (' -> ');
    END;
    WrVar (p^[0]);
    Ln;
END GenLoop;

PROCEDURE SameMemory (v: ir.VarNum; p: ir.ParamPtr): BOOLEAN;
BEGIN
    RETURN (p^.tag = ir.y_Variable) &
           (Color.Allocation^[p^.name].Location =
            Color.Allocation^[v].Location) &
           (Color.Allocation^[p^.name].Offset   =
            Color.Allocation^[v].Offset)
-- & (ir.Vars^[p^.name].Def^.ResSize = ir.Vars^[v].Def^.ResSize)
END SameMemory;

CONST IntersectMemory = SameMemory;

(* -------------------------------------------------------------------------- *)

PROCEDURE WrNode (node: ir.Node);
VAR
    i: INT;
    a: ir.Arc;
    n: ir.Node;
    v: ir.VarNum;
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
    str: ARRAY 4 OF CHAR;
<* END *>
BEGIN
    IF ir.Nodes^[node].Alive THEN
        Ln;
--        DStrings.Assign ("", nodeLabel);
        WrInt (ORD(node));
        WrStr (' : ----- #');
        WrInt (ORD(node));
        IF gr.IsOrder THEN
            WrStr  (' (');
            WrInt  (ORD(ir.Nodes^[node].TopNumber));
            WrChar (')');
        END;
        WrStr (' -- [');
        FOR i:=0 TO ir.Nodes^[node].NIn-1 DO
            WrStr  (' ');
            WrInt  (ORD(ir.Nodes^[node].In^[i]));
            WrChar ('/');
            WrInt  (gr.FindOutArc (ir.Nodes^[node].InArcs^[i]));
        END;
        WrStr    ('] ');
        WrChar('N');
        WrInt(ir.Nodes^[node].Nesting);
        TabMinus (51);
        IF gr.IsDominators AND (ir.Nodes^[node].Dominators # NIL) THEN
            WrStr (' DOM:');
            FOR n:=ir.ZERONode TO SYSTEM.PRED(ir.Nnodes) DO
                IF BitVect.In (ir.Nodes^[node].Dominators, ORD(n)) THEN
                    WrChar (' ');
                    WrInt  (ORD(n));
                END;
            END;
        END;
        IF gr.IsaDominators AND (ir.Nodes^[node].Dominators # NIL) THEN
            WrStr (' ADOM:');
            FOR a:=ir.ZEROArc TO SYSTEM.PRED(gr.Narcs) DO
                IF (gr.Arcs^[a].t <> ir.ZERONode) &
                   BitVect.In (ir.Nodes^[node].aDominators, ORD(a))
                THEN
                    WrChar (' ');
                    WrInt  (ORD(a));
                END;
            END;
        END;
        IF gr.IsDominatorsTree THEN
            IF ir.Nodes^[node].IDom <> ir.UndefNode THEN
                WrStr (' IDom: ');
                WrInt (ORD(ir.Nodes^[node].IDom));
            END;
            IF ir.Nodes^[node].DomChild <> ir.UndefNode THEN
                WrStr (' Down: ');
                WrInt (ORD(ir.Nodes^[node].DomChild));
            END;
            IF ir.Nodes^[node].DomLink <> ir.UndefNode THEN
                WrStr (' Next: ');
                WrInt (ORD(ir.Nodes^[node].DomLink));
            END;
        END;
        IF gr.IsDF THEN
            WrStr (' DF:');
            FOR i:=0 TO gr.dfLen^[node]-1 DO
                WrChar (' ');
                WrInt  (ORD(gr.dfArray^[node]^[i]));
            END;
        END;
(**)
        IF (Color.NNonTempVars <> ir.UNDEFINED)&
           (Color.LiveAtTop# NIL)&
           (Color.LiveAtBottom#NIL) THEN
            WrStr (' LT: ');
            FOR v:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                IF BitVect.In (Color.LiveAtTop^[node], ORD(v)) THEN
                    WrVar  (v);
                    WrChar (' ');
                END;
            END;
            WrStr (' LB: ');
            FOR v:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                IF BitVect.In (Color.LiveAtBottom^[node], ORD(v)) THEN
                    WrVar  (v);
                    WrChar (' ');
                END;
            END;
        END;
(**)
        Ln;
        WrTriades (ir.Nodes^[node].First, node);

        IF (Color.NNonTempVars <> ir.UNDEFINED)&
           (Color.Allocation # NIL) THEN
            Color.GenFies (node, GenMove, GenLoop, SameMemory, IntersectMemory);
        END;
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
        IF env.config.Option("gen_hgr") THEN
            FOR i:=ir.Nodes^[node].NOut-1 TO 0 BY -1 DO
                hgr.AddEdge(hgr.DefEdgeType, ORD(node), ORD(ir.Nodes^[node].Out^[i]));
            END;
        END;
        IF env.config.Option("gen_hgr") THEN
--            hgr.SetLabVal (hgl.ObjVertex, node, hgr.labelNode, nodeLabel);
        END;
<* END *>
    END;
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
    IF env.config.Option("gen_hgr") THEN
        WholeStr.IntToStr (ORD(node), str);
        hgr.SetLabVal (hgl.ObjVertex, ORD(node), hgr.labelNode,  str);
    END;
<* END *>
END WrNode;

(* -------------------------------------------------------------------------- *)

PROCEDURE WriteTest* (id: CHAR; N-: ARRAY OF CHAR);
VAR n: ir.Node;
    l: INT;
    m : ir.TSNode;
    k: Color.ClusterNum;
    v1, v2: ir.VarNum;
    loop: ir.Loop;
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
    hgrfname : pc.STRING;
    v: hgl.PVertex;
<* END *>
BEGIN
    IF ~(id IN parts)&~('*' IN parts) THEN RETURN; END;
    IF ((need_proc_name # NIL) & (need_proc_name^ # curr_proc_name^)) THEN
        RETURN;
    END;

<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
    IF env.config.Option("gen_hgr") THEN
        hgr.CreateNewGraph(1);
        hgrfname := at.make_name("%s %s %c", at.curr_mod^.name^, curr_proc_name^, id);
        hgr.SetFrTitle(0, hgrfname);
    END;
<* END *>

    Append(fname);
    pos := 0;
    WrStr (';================== test listing [');
    WrStr (curr_proc_name^);
    WrStr ('] :');
    WrStr (N);
    WrStr (' ================ ');

    WrStr ('@');

    INC(qfileNo);
    WrInt (qfileNo);

    Ln;
    IF gr.LoopsOk & (gr.NLoops <> ir.ZEROLoop) THEN
        WrStr ('; Loops:');
        Ln;
        FOR loop:=ir.ZEROLoop TO SYSTEM.PRED(gr.NLoops) DO
            WrStr ('; Preheader: ');
            WrInt (ORD(gr.LoopList^[loop].Preheader));
            WrStr ('; Nesting: ');
            WrInt (ORD(ir.Nodes[gr.LoopList^[loop].Preheader].Nesting+1));
            WrStr (' nodes:');
            FOR n:=ir.ZERONode TO SYSTEM.PRED(ir.Nnodes) DO
                IF BitVect.In (gr.LoopList^[loop].Body, ORD(n)) THEN
                    WrChar (' ');
                    WrInt  (ORD(n));
                END;
            END;
            IF gr.LoopList^[loop].Exits <> NIL THEN
                WrStr (', exits:');
                FOR n:=ir.ZERONode TO SYSTEM.PRED(ir.Nnodes) DO
                    IF BitVect.In (gr.LoopList^[loop].Exits, ORD(n)) THEN
                        WrChar (' ');
                        WrInt  (ORD(n));
                    END;
                END;
            END;
            WrStr (', arc: ');
            WrInt (ORD(gr.Arcs^[gr.LoopList^[loop].BackEdge].f));
            WrStr ('->');
            WrInt (ORD(gr.Arcs^[gr.LoopList^[loop].BackEdge].t));
            Ln;
        END;
    END;
    Ln;

<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
    IF env.config.Option("gen_hgr") THEN
        FOR n := ir.ZERONode TO SYSTEM.PRED(ir.Nnodes) DO
            hgr.AddVertex (hgr.irNodeType, 0, 0, 0);
            v := hgr.GetVertex(ORD(n));
            v.h := 15;
            v.w := 15;

    --        hgr.SetLabVal (0, n, labelNode, "1 : ----- #1 (3) -- [ 26/0 25/0] ------------------ DOM: 0 1 25 IDom: 25 Down: 5 DF: 1 2\\n");
    --      hgr.SetLabVal (0, n, labelNode, "1 : ----- #1 (3) -- [ 26/0 25/0] ------------------ DOM: 0 1 25 IDom: 25 Down: 5 DF: 1 2\\n");
        END;
    END;
<* END *>

    IF (gr.IsOrder) AND env.config^.Option("qfile_topsort") THEN
        FOR m := ir.StartOrder TO SYSTEM.PRED(LEN(ir.Order^)) DO
            WrNode (ir.Order^[m]);
        END;
    ELSE
        FOR n := ir.ZERONode TO SYSTEM.PRED(ir.Nnodes) DO
            WrNode (n);
        END;
    END;
    Ln;

    IF Color.NNonTempVars <> ir.UNDEFINED THEN
        WrStr ('Number of non-temporary variables: ');
        WrInt (ORD(Color.NNonTempVars));
        Ln;
        IF env.config.Option("qfile_hookedvars") THEN
          IF ir.NVars # ir.ZEROVarNum THEN
              WrStr ('Hooked variables:');
              Ln;
          END;
          IF Color.HookedVars # NIL THEN
            FOR v1:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                WrVar (v1);
                Tab   (8);
                WrStr (': ');
                FOR v2:=ir.ZEROVarNum TO SYSTEM.PRED(ir.NVars) DO
                    IF Color.AreHooked (v1, v2) THEN
                        WrVar  (v2);
                        WrChar (' ');
                    END;
                END;
                Ln;
            END;
            Ln;
          END;
        END;
        IF Color.Allocation# NIL THEN
          FOR v1:=ir.ZEROVarNum TO SYSTEM.PRED(Color.NNonTempVars)  DO
              WrVar   (v1);
              Tab     (8);
              WrStr   (' allocated to local #');
              WrInt   (Color.Allocation^[v1].Location);
              WrChar  ('=');
              WrLocal (Color.Allocation^[v1].Location);
              IF Color.Allocation^[v1].Offset <> 0 THEN
                  WrChar ('+');
                  WrInt (Color.Allocation^[v1].Offset);
              END;
              Ln;
          END;
          Ln;
          FOR k:=0 TO Color.NClusters-1 DO
              WrStr ('Cluster ');
              WrInt (k);
                  WrStr   (' in local #');
                  WrInt   (Color.Allocation^[Color.Clusters^[k].v^[0]].Location);
                  WrChar  ('=');
                  WrLocal (Color.Allocation^[Color.Clusters^[k].v^[0]].Location);
                  IF Color.Allocation^[Color.Clusters^[k].v^[0]].Offset <> 0 THEN
                      WrChar ('+');
                      WrInt (Color.Allocation^[Color.Clusters^[k].v^[0]].Offset);
                  END;
              WrStr (':');
              Tab (35);
              WrStr ('(Profit ');
              WrInt  (Color.CalcRegProfit (Color.Clusters^[k].v^[0]));
              WrStr (')  ');
              FOR l:=0 TO Color.Clusters^[k].N-1 DO
                  WrVar (Color.Clusters^[k].v^[l]);
                  WrChar  ('<');
                  WrType (ir.Vars[Color.Clusters^[k].v^[l]].Def.ResType,
                          ir.Vars[Color.Clusters^[k].v^[l]].Def.ResSize);
                  WrStr ('>; ');
              END;
              Ln;
          END;
          Ln;
          IF Color.NNonTempVars < ir.NVars THEN
              WrStr ('Register profits for temporary variables:');
              Ln;
          END;
          FOR v1:=Color.NNonTempVars TO SYSTEM.PRED(ir.NVars) DO
              WrVar  (v1);
              WrChar (':');
              Tab    (8);
              WrInt  (Color.CalcRegProfit (v1));
              Ln;
          END;
        END;
    END;

    Close;
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
    IF env.config.Option("gen_hgr") THEN
    --    hgr.OptimizeFrLoc(0,4);
        hgr.RearrangeGraph(ORD(ir.Nnodes), 30, 20);
        hgr.NormalizeGraph(1);
        hgrfname := at.make_name("%s_%s_%c.hgr", at.curr_mod^.name^, curr_proc_name^, id);
        hgr.SaveGraph(hgrfname);
    END;
<* END *>

END WriteTest;

<* IF DEFINED(OVERDYE) AND OVERDYE THEN *>
PROCEDURE WriteNodeOrder * (N: INTEGER; s-: ARRAY OF CHAR);
VAR
  n   : INT;
  row1: INT;
  col1: INT;
  row2: INT;
  col2: INT;
  t   : ir.TriadePtr;
  tpos: env.TPOS;
  name: pc.STRING;
  leq : INT;
  gr  : INT;
BEGIN
  IF od.IsOrder THEN
    Append(fname);
    Ln; WrStr ('===== Node positions order [');
    WrStr (s);
    WrStr ('] :');
    WrInt (N);
    WrStr (' ====='); Ln;
    leq := 0;
    gr  := 0;
    FOR n := 0 TO LEN(od.NodePosOrder^)-1 DO
      WrInt (od.NodePosOrder^[n]);
      tpos := ir.Nodes^[od.NodePosOrder^[n]].Position;
      tpos.unpack (name, row1, col1);
      WrStr (' - '); WrInt (row1); WrChar (':'); WrInt (col1);
      WrStr (' {');
      t := ir.Nodes^[od.NodePosOrder^[n]].First;
      WHILE t # NIL DO
        tpos.unpack (name, row1, col1);
        t^.Position.unpack (name, row2, col2);
        IF row1 <= row2 THEN
          WrStr (' <= ');
          INC(leq);
        ELSE
          WrStr (' > ');
          INC(gr);
        END;
        WrInt (row2); WrChar (':'); WrInt (col2);
        tpos := t^.Position;
        t := t^.Next;
      END;
      WrStr (' }');
      Ln;
    END;
    WrStr ('ratio order = '); WrInt (leq); WrChar ('/'); WrInt (gr);
    WrChar (' '); WrInt (leq*100 DIV (leq+gr)); WrChar ('%'); Ln; Ln;
    IF N = 2 THEN
      WrStr ('===== Node position back order ====='); Ln;
      FOR n := 0 TO LEN(od.NodePosBackOrder^)-1 DO
         WrInt (n); WrStr (' - '); WrInt (od.NodePosBackOrder^[n]); Ln;
      END;
      Ln; Ln;
    END;
    Close;
  END;
END WriteNodeOrder;
<* END *>

(* -------------------------------------------------------------------------- *)

PROCEDURE Init*();
VAR    i: INTEGER;
      qfile_string, fname_string  : pc.STRING;   (* for generating *.Q file *)
  VAR dir,name,ext: pc.STRING;
BEGIN
  env.config^.Equation("QFILE_PROC", need_proc_name);
  env.config^.Equation("QFILE",      qfile_string);
  IF qfile_string # NIL THEN
    FOR i := 0 TO LEN(qfile_string^)-1 DO
        INCL (parts, qfile_string^[i]);
    END;
  END;
  IF env.config^.Option("QFILE") THEN
    parts := parts + SetOfChar{"i", "s", "f", "l"};
  END;
  IF parts = SetOfChar{} THEN RETURN; END;

  fname_string := at.make_name("%s.q", at.curr_mod^.name^);
  COPY (fname_string^, fname);
  xfs.sys.Get(fname,dir,name,ext);
--  xfs.sys.ConvertToHost(dir^,a);
  SYSTEM.EVAL(Dirs.mkdirs(dir));
  Open(fname);
  Close;
<* IF DEFINED (HGR_SUPPORT) & HGR_SUPPORT THEN *>
  IF env.config.Option("gen_hgr") THEN
    hgr.Init;
  END;
<* END *>
END Init;

(* -------------------------------------------------------------------------- *)
BEGIN
  qfileNo := 0;
END TestIO.

