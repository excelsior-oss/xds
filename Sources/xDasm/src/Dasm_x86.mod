IMPLEMENTATION MODULE Dasm_x86;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;
IMPORT exc := EXCEPTIONS;

IMPORT kt  := KrnTypes;
IMPORT mem := Exe_Mem;

FROM Krn_Dasm IMPORT ResolveAddr, ResolveEA;


 IMPORT xs  := xStr;
<* IF DEST_XDASM THEN *>

<* ELSE *>

 FROM Krn_Dasm IMPORT Seg16;

<* END *>

-------------------------------- Дизассемблер -----------------------------
                                --------------
TYPE
  FMT_STR = ARRAY [0..15] OF CHAR;

VAR
  b1_fmt, b2_fmt, b4_fmt, no_align_fmt: FMT_STR;

VAR
  DisasmSource: exc.ExceptionSource;

<* IF DEST_XDASM THEN *>
  Seg16: BOOLEAN;
<* END *>

  resolve: BOOLEAN;



PROCEDURE DisasmError;
BEGIN
  exc.RAISE(DisasmSource, MAX(CARDINAL), '');
END DisasmError;

CONST (* общие регистры - поле reg *)

  al = 0;       ax = al;       eax = ax;
  cl = 1;       cx = cl;       ecx = cx;
  dl = 2;       dx = dl;       edx = dx;
  bl = 3;       bx = bl;       ebx = bx;
  ah = 4;       sp = ah;       esp = sp;
  ch = 5;       bp = ch;       ebp = bp;
  dh = 6;       si = dh;       esi = si;
  bh = 7;       di = bh;       edi = di;

  es = 0;
  cs = 1;
  ss = 2;
  ds = 3;
  fs = 4;
  gs = 5;

VAR
   d_str, d_info: ARRAY [0..256] OF CHAR;
   current: BOOLEAN;

   n : CARDINAL;

   mod, reg, rm: CARDINAL;      -- части байта mod/reg/rm
   sc, index, base: CARDINAL;     -- части байта s-i-b

   second_arg : BOOLEAN;         -- второй аргумент команды

   displ    : INTEGER;
   displ_pos: CARDINAL;

   data_pos : CARDINAL;
   selector : CARDINAL;
   data     :  INTEGER;

   cpos    : kt.ADDRESS;
   bpos    : kt.ADDRESS;

   prefix: BOOLEAN;          -- признак неоконченности команды

   size32, addr32: BOOLEAN;  -- размер операнда и адреса без учета префикса
   s32     : BOOLEAN;        -- размер операнда с учетом префикса
   a32     : BOOLEAN;        -- размер адреса с учетом префикса
   opsize  : BOOLEAN;        -- наличие префикса OPSIZE
   addrsize: BOOLEAN;        -- наличие префикса ADDRSIZE;
   was_error:BOOLEAN;

   segm_override: (none, _cs, _ds, _ss, _fs, _es, _gs);

   us, ua, useg : BOOLEAN;   -- было ли использовано ADRSIZ, OPSIZ, SEG

<* PUSH *>
<* WOFF304+ *>
PROCEDURE get_b1(): sys.INT8;
VAR
  b: sys.INT8;
BEGIN
  IF NOT mem.Get(cpos, sys.ADR(b), SIZE(b)) THEN DisasmError; END;
  INC(cpos);
  RETURN b;
END get_b1;

PROCEDURE get_b2(): sys.INT16;
VAR
  w: sys.INT16;
BEGIN
  IF NOT mem.Get(cpos, sys.ADR(w), SIZE(w)) THEN DisasmError; END;
  INC(cpos, 2);
  RETURN w;
END get_b2;

PROCEDURE get_b4(): INTEGER;
VAR
  d: sys.INT32;
BEGIN
  IF NOT mem.Get(cpos,sys.ADR(d), SIZE(d)) THEN DisasmError; END;
  INC(cpos, 4);
  RETURN d;
END get_b4;
<* POP *>

PROCEDURE ws(s-: ARRAY OF CHAR);
BEGIN
  fmt.append(d_str, "%s", s);
END ws;

PROCEDURE wc(ch: CHAR);
BEGIN
  fmt.append(d_str, "%c", ch);
END wc;

PROCEDURE wast(need: BOOLEAN);
BEGIN
  IF need THEN
    fmt.append(d_str, "*");
  END;
END wast;

PROCEDURE wfmt(f: ARRAY OF CHAR; SEQ arg: sys.BYTE);
BEGIN
  fmt.append(d_str, f, arg);
END wfmt;

PROCEDURE name(nm-: ARRAY OF CHAR);
BEGIN
  wfmt("%-9.9s ", nm);
END name;

PROCEDURE disp8;
BEGIN
  displ_pos := cpos;
  displ := get_b1();
END disp8;

PROCEDURE disp32;
BEGIN
  displ_pos := cpos;
  displ := get_b4();
END disp32;

PROCEDURE disp16;
BEGIN
  displ_pos := cpos;
  displ := get_b2();
END disp16;

PROCEDURE disp;
BEGIN
  ua := TRUE;
  IF a32 THEN
    disp32;
  ELSE
    disp16;
  END;
END disp;

PROCEDURE sib;
VAR
  bb: sys.CARD8;
BEGIN
  bb := sys.CARD8(get_b1());
  base  := VAL(CARDINAL, bb MOD 8);
  bb := bb DIV 8;
  index := VAL(CARDINAL, bb MOD 8);
  sc    := VAL(CARDINAL, bb DIV 8);
  CASE mod OF
  | 0H: IF (base = 5H) THEN disp32; END;
  | 1H: disp8;
  | 2H: disp32;
  END;
END sib;

PROCEDURE ea;
  VAR bb: sys.CARD8;
BEGIN
  bb := sys.CARD8(get_b1());
  rm  := VAL(CARDINAL, bb MOD 8); bb := bb DIV 8;
  reg := VAL(CARDINAL, bb MOD 8);
  mod := VAL(CARDINAL, bb DIV 8);
  ua := TRUE;
  IF a32 THEN
    CASE mod OF
    | 0H: IF    (rm = 4H) THEN sib;
          ELSIF (rm = 5H) THEN disp32;
          END;
    | 1H: IF (rm = 4H) THEN sib ELSE disp8; END;
    | 2H: IF (rm = 4H) THEN sib ELSE disp32; END;
    | 3H: (* ничего *)
    END;
  ELSE
    CASE mod OF
    | 0H: IF (rm = 6H) THEN disp16; END;
    | 1H: disp8;
    | 2H: disp16;
    | 3H: (* ничего *)
    END;
  END;
END ea;

PROCEDURE error;
BEGIN
  was_error:=TRUE;
END error;

PROCEDURE nameerror;
BEGIN
  name("????");
  was_error:=TRUE;
END nameerror;

PROCEDURE r8name(r: CARDINAL);
BEGIN
  CASE r OF
  | al: ws("al");
  | cl: ws("cl");
  | dl: ws("dl");
  | bl: ws("bl");
  | ah: ws("ah");
  | ch: ws("ch");
  | dh: ws("dh");
  | bh: ws("bh");
  END;
END r8name;

PROCEDURE r16name(r: CARDINAL);
BEGIN
  CASE r OF
  | ax: ws("ax");
  | cx: ws("cx");
  | dx: ws("dx");
  | bx: ws("bx");
  | sp: ws("sp");
  | bp: ws("bp");
  | si: ws("si");
  | di: ws("di");
  END;
END r16name;

PROCEDURE r32name(r: CARDINAL);
BEGIN
  CASE r OF
  | eax: ws("eax");
  | ecx: ws("ecx");
  | edx: ws("edx");
  | ebx: ws("ebx");
  | esp: ws("esp");
  | ebp: ws("ebp");
  | esi: ws("esi");
  | edi: ws("edi");
  END;
END r32name;

PROCEDURE r32conv(r: CARDINAL): CARDINAL;
BEGIN
  CASE r OF
  | eax: RETURN 10H;
  | ecx: RETURN 11H;
  | edx: RETURN 12H;
  | ebx: RETURN 13H;
  | esp: RETURN 14H;
  | ebp: RETURN 15H;
  | esi: RETURN 16H;
  | edi: RETURN 17H;
  END;
END r32conv;

PROCEDURE mmname(r:CARDINAL);
BEGIN
  wfmt("mm%d", r);
END mmname;

PROCEDURE reg8;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  r8name(reg);
END reg8;

PROCEDURE reg16;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  r16name(reg);
END reg16;

PROCEDURE reg32;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  r32name(reg);
END reg32;

PROCEDURE Reg;
BEGIN
  us := TRUE;
  IF s32 THEN
    reg32;
  ELSE
    reg16;
  END;
END Reg;

PROCEDURE mm;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  mmname(reg);
END mm;

PROCEDURE sreg;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  CASE reg OF
  | es: ws("es");
  | cs: ws("cs");
  | ss: ws("ss");
  | ds: ws("ds");
  | fs: ws("fs");
  | gs: ws("gs");
  ELSE
    ws("?s");
    was_error:=TRUE;
  END;
END sreg;

PROCEDURE n_reg;
VAR
  bb: sys.CARD8;
BEGIN
  bb := sys.CARD8(get_b1());
  reg  := VAL(CARDINAL, bb MOD 8);
  bb := bb DIV 8;
  n := VAL(CARDINAL, bb MOD 8);
  mod := VAL(CARDINAL, bb DIV 8);
  ua := TRUE;
  IF a32 THEN
    CASE mod OF
    | 3H: (* ничего *)
    END;
  ELSE
    CASE mod OF
    | 3H: (* ничего *)
    END;
  END;
END n_reg;

PROCEDURE CR(n: CARDINAL);
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;
  wfmt("CR%d", n);
  CASE n OF
  | 0,2,3,4:
  ELSE error;
  END;
END CR;

PROCEDURE DR(n: CARDINAL);
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  wfmt("DR%d",n);
  CASE n OF
  | 0..7:
  ELSE error;
  END;
END DR;

PROCEDURE TR(n: CARDINAL);
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  wfmt("TR%d",n);
  CASE n OF
  | 6,7:
  ELSE error;
  END;
END TR;


PROCEDURE _bracket;
BEGIN
  useg := TRUE;
  CASE segm_override OF
  | _es: ws("es:");
  | _cs: ws("cs:");
  | _ds: ws("ds:");
  | _fs: ws("fs:");
  | _gs: ws("gs:");
  | _ss: ws("ss:");
  | none:
  END;
  wc('[');
END _bracket;

PROCEDURE bracket_;
BEGIN
  wc(']');
END bracket_;

PROCEDURE dbracket;
BEGIN
  ws ("dword ptr ");
  _bracket;
END dbracket;

PROCEDURE wbracket;
BEGIN
  ws ("dword ptr ");
  _bracket;
END wbracket;

PROCEDURE bbracket;
BEGIN
  ws ("byte ptr ");
  _bracket;
END bbracket;

PROCEDURE bracket;
BEGIN
  us := TRUE;
  IF s32 THEN
    dbracket;
  ELSE
    wbracket;
  END;
END bracket;

PROCEDURE Bracket(len: CARDINAL; show: BOOLEAN);
BEGIN
  CASE len OF
  |  0:
  |  1: ws ("byte ptr ");  IF show THEN _bracket; END;
  |  2: ws ("word ptr ");  IF show THEN _bracket; END;
  |  4: ws ("dword ptr "); IF show THEN _bracket; END;
  |  6: ws ("pword ptr "); IF show THEN _bracket; END;
  |  8: ws ("qword ptr "); IF show THEN _bracket; END;
  | 11: ws ("tbyte ptr "); IF show THEN _bracket; END;
  |100: _bracket;
  |101:
    IF s32 THEN
      ws ("dword ptr "); IF show THEN _bracket; END;
    ELSE
      ws ("word ptr ");  IF show THEN _bracket; END;
    END;
  END;
END Bracket;

<* PUSH *>
<* COVERFLOW- *>

PROCEDURE _resolve(ea: kt.ADDRESS; len: CARDINAL; print: BOOLEAN);
BEGIN
  CASE len OF
  | 0:
    len := 1;
  | 101, 100:
    IF s32 THEN
      len := 4;
    ELSE
      len := 2;
    END;
  | 11:
    len := 10;
  ELSE
  END;
<* IF DEST_XDASM THEN *>
  IF NOT print THEN
    ResolveEA(0, ea, len, d_str);
  ELSE
    ResolveEA(displ_pos, ea, len, d_str);
  END;
<* ELSE *>
  IF print THEN
    wfmt(b4_fmt, ea);
  END;
  IF resolve THEN
    ResolveEA (0, ea, len, d_info);
  END;
<* END *>
END _resolve;

PROCEDURE adr_fix(len: CARDINAL; abs :BOOLEAN);
VAR
  ea : CARDINAL;
BEGIN
  ea := CARDINAL(displ);
<* IF DEST_XDASM THEN *>
  IF NOT abs THEN
    INC(ea, cpos);
  END;
<* END *>
  Bracket(len, TRUE);
  _resolve(ea ,len, TRUE);
  IF (len # 0) THEN
    bracket_;
  END;
END adr_fix;

<* POP *>

PROCEDURE ptr16_fix;
BEGIN
  IF s32 THEN
    wfmt("%$4X:%$8X", selector, displ);
  ELSE
    wfmt("%$4X:%$4X", selector, sys.CAST(sys.CARD16, displ));
  END;
END ptr16_fix;

PROCEDURE w_displ(displ: INTEGER);
BEGIN
  IF displ > 0 THEN
    wc('+');
    wfmt(no_align_fmt, displ);
  ELSIF displ < 0 THEN
    wc('-');
    wfmt(no_align_fmt, (CARDINAL(-BITSET(displ))+1));
  END;
END w_displ;

<* PUSH *>
<* COVERFLOW- *>

PROCEDURE _sib(len: CARDINAL);
TYPE
  AC = ARRAY [0..3] OF CARDINAL;
CONST
  scale = "1248";
  scale_ = AC{1,2,4,8};

VAR
  ea, t1: CARDINAL;
  ok: BOOLEAN;

BEGIN
  ea := 0;
  IF (mod=0) & (base=5) THEN
    wfmt(b4_fmt, displ); ok := TRUE; ea := CARDINAL(displ);
  ELSE
    r32name(base); ok := mem.GetReg(r32conv(base), ea);
  END;
  IF index # 4 THEN
    t1 := 0;
    wc('+'); r32name(index); ok := mem.GetReg(r32conv(index), t1);
    IF sc > 0 THEN
      wc('*'); wc(scale[sc]);
      t1 := t1 * scale_[sc];
    END;
    INC(ea,t1);
  END;
  IF mod>0 THEN w_displ(displ); INC(ea, CARDINAL(displ)); END;
  IF ok THEN
    _resolve(ea, len, FALSE)
  END;
END _sib;

PROCEDURE exec_adr(len: CARDINAL);
VAR
  ea: CARDINAL;
  ok: BOOLEAN;
BEGIN
  ea := 0;
  ok := FALSE;
  ua := TRUE;
  IF NOT (a32 & (mod = 0) & (rm = 5)) THEN
    Bracket(len, TRUE);
  END;
  IF a32 THEN
    CASE mod OF
    | 0H: CASE rm OF
          | 0H..3H,6H,7H:  r32name(rm); ok := mem.GetReg(r32conv(rm), ea);
          | 4H:           _sib(len);
          | 5H:            adr_fix(len, FALSE); RETURN; -- !!!
          END;
    | 1H..2H:
          IF (rm = 4H) THEN
            _sib(len);
          ELSE
            r32name(rm);
            w_displ(displ);
            ok := mem.GetReg(r32conv(rm), ea);
            INC(ea, CARDINAL(displ));
          END;
    | 3H: r32name(rm);  ok := mem.GetReg(r32conv(rm), ea);
    END;
  ELSE
    CASE mod OF
    | 0H..2H:
          CASE rm OF
          | 0H: ws("bx+si");
          | 1H: ws("bx+di");
          | 2H: ws("bp+si");
          | 3H: ws("bp+di");
          | 4H: ws("si");
          | 5H: ws("di");
          | 6H: IF mod#0 THEN
                  ws("bp")
                ELSE
                  w_displ(displ);
                END;
          | 7H: ws("bx");
          END;
          IF mod>0 THEN
            w_displ(displ);
          END;
    | 3H: r16name(rm);
    END;
  END;
  wc(']');
  IF ok THEN
    _resolve(ea, len, FALSE);
  END;
END exec_adr;

<* POP *>

PROCEDURE Ea;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;
  exec_adr(100);
END Ea;

PROCEDURE Eb;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;
  IF mod = 3 THEN
    r8name(rm);
  ELSE
    exec_adr(1);
  END;
END Eb;

PROCEDURE Ew;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;
  IF mod = 3 THEN
    r16name(rm);
  ELSE
    exec_adr(2);
  END;
END Ew;

PROCEDURE Ed;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;
  IF mod = 3 THEN
    r32name(rm);
  ELSE
    exec_adr(4);
  END;
END Ed;

PROCEDURE Ep;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;
  IF mod = 3 THEN
    r32name(rm);
    was_error:=TRUE;
  ELSE
    exec_adr(6);
  END;
END Ep;

PROCEDURE Eq;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;
  IF mod = 3 THEN
    r32name(rm);
    was_error:=TRUE;
  ELSE
    exec_adr(8);
  END;
END Eq;

PROCEDURE Et;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;
  IF mod = 3 THEN
    r32name(rm);
    was_error:=TRUE;
  ELSE
    exec_adr(11);
  END;
END Et;

PROCEDURE E;
BEGIN
  us := TRUE;
  IF s32 THEN
    Ed;
  ELSE
    Ew;
  END;
END E;

PROCEDURE Eww;
BEGIN
  us := TRUE;
  IF s32 THEN
    Ew;
  ELSE
    Ed;
  END;
END Eww;

PROCEDURE Em;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;
  IF mod = 3 THEN
    mmname(rm);
  ELSE
    exec_adr(8);
  END;
END Em;

PROCEDURE Emd;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;
  IF mod = 3 THEN
    mmname(rm);
  ELSE
    exec_adr(4);
  END;
END Emd;

PROCEDURE data8;
BEGIN
  data_pos:= cpos;
  data := get_b1();
END data8;

PROCEDURE data16;
BEGIN
  data_pos:= cpos;
  data := get_b2();
END data16;

PROCEDURE data32;
BEGIN
  data_pos:= cpos;
  data := get_b4();
END data32;

PROCEDURE Data;
BEGIN
  us := TRUE;
  IF s32 THEN
    data32
  ELSE
    data16
  END;
END Data;

PROCEDURE ptr16_;
BEGIN
  us := TRUE;
  IF s32 THEN
    disp32;
  ELSE
    disp16;
  END;
  selector := sys.CAST(sys.CARD16, get_b2());
END ptr16_;

PROCEDURE _resolve_data(): BOOLEAN;
<* IF DEST_XDASM THEN *>
VAR
  s: xs.String;
BEGIN
  s:= '';
  IF ResolveAddr(data_pos, MAX(kt.ADDRESS), s) THEN
    ws("offset ");
    ws(s);
    RETURN TRUE;
  END;
<* ELSE *>
VAR
  s: xs.String;
BEGIN
  s := "";
  IF (data >0 ) & ResolveAddr(0, data, s) THEN
    ws(s);
    RETURN TRUE;
  END;
<* END *>
  RETURN FALSE;
END _resolve_data;

PROCEDURE _data8;
VAR shortdata:SHORTINT;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  IF NOT _resolve_data() THEN
    IF s32 THEN
      shortdata := data REM 100H;
      wfmt(b1_fmt, shortdata);
    ELSE
      wfmt(b1_fmt, CARDINAL(data) MOD 100H);
    END;
  END;
END _data8;

PROCEDURE _data16;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  IF NOT _resolve_data() THEN
    wfmt(b2_fmt, CARDINAL(data) MOD 10000H);
  END;
END _data16;

PROCEDURE _data32;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  IF NOT _resolve_data() THEN
    wfmt(b4_fmt, data);
  END;
END _data32;

PROCEDURE _data;
BEGIN
  us := TRUE;
  IF s32 THEN
    _data32;
  ELSE
    _data16;
  END;
END _data;

PROCEDURE _disp8;
VAR
  addr: kt.ADDRESS;
BEGIN
  ASSERT ((displ >= -128) & (displ <= 127));
<* PUSH *>
<* COVERFLOW- *>
  addr := cpos+CARDINAL(displ);
<* POP *>
  IF resolve THEN
    sys.EVAL(ResolveAddr(cpos-1, addr, d_str));
  END;
END _disp8;

PROCEDURE _disp16;
VAR
  addr: kt.ADDRESS;
BEGIN
  ASSERT ((displ >= -32768) & (displ <= 32767));
<* PUSH *>
<* COVERFLOW- *>
  addr := cpos+CARDINAL(displ);
<* POP *>
  IF resolve THEN
    sys.EVAL(ResolveAddr(cpos-2, addr, d_str));
  END;
END _disp16;

PROCEDURE _disp32;
VAR
  addr: kt.ADDRESS;
BEGIN
  <* PUSH *>
  <* COVERFLOW- *>
  addr := cpos+CARDINAL(displ);
  <* POP *>
  IF resolve THEN
    sys.EVAL(ResolveAddr(cpos-4, addr, d_str));
  END;
END _disp32;

PROCEDURE _disp;
BEGIN
  ua := TRUE;
  IF a32 THEN
    _disp32;
  ELSE
    _disp16;
  END;
END _disp;

PROCEDURE _ptr16_;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE; END;
  ptr16_fix;
END _ptr16_;

PROCEDURE AL;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  r8name(al);
END AL;

PROCEDURE AX;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  r16name(ax);
END AX;

PROCEDURE EAX;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  r32name(eax);
END EAX;

PROCEDURE A;
BEGIN
  us := TRUE;
  IF s32 THEN
    EAX;
  ELSE
    AX;
  END;
END A;

PROCEDURE ST(i: CARDINAL);
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  wfmt('ST(%d)', i);
END ST;

PROCEDURE ST0;
BEGIN
  IF second_arg THEN ws(", ") ELSE second_arg := TRUE END;;
  wfmt('ST');
END ST0;

PROCEDURE float(cop: sys.CARD8);
VAR
  i: CARDINAL;
  b1: sys.CARD8;
BEGIN
  b1 := CARDINAL(get_b1());
  CASE cop OF
  | 0D8H:
     IF b1 < 0C0H THEN
      DEC(cpos);
      ea;
       CASE reg OF
       | 0H: name("fadd");  Ed;
       | 1H: name("fmul");  Ed;
       | 2H: name("fcom");  Ed;
       | 3H: name("fcomp"); Ed;
       | 4H: name("fsub");  Ed;
       | 5H: name("fsubr"); Ed;
       | 6H: name("fdiv");  Ed;
       | 7H: name("fdivr"); Ed;
       END;
     ELSE
       i := b1 MOD 8;
       CASE b1 OF
       | 0C0H..0C7H:        name("fadd");  ST(0); ST(i);
       | 0C8H..0CFH:        name("fmul");  ST(0); ST(i);
       | 0D0H, 0D2H..0D7H:  name("fcom");  ST(i);
       | 0D1H:              name("fcom");
       | 0D8H, 0DAH..0DFH:  name("fcomp"); ST(i);
       | 0D9H            :  name("fcomp");
       | 0E0H..0E7H:        name("fsub");  ST(0); ST(i);
       | 0E8H..0EFH:        name("fsubr"); ST(0); ST(i);
       | 0F0H..0F7H:        name("fdiv");  ST(0); ST(i);
       | 0F8H..0FFH:        name("fdivr"); ST(0); ST(i);
       END;
     END;
  | 0D9H:
     IF b1 < 0C0H THEN
       DEC(cpos);
       ea;
       CASE reg OF
       | 0H: name("fld");     Ed;
       | 2H: name("fst");     Ed;
       | 3H: name("fstp");    Ed;
       | 4H: name("fldenv");  Ea;
       | 5H: name("fldcw");   Ew;
       | 6H: name("fstenv");  Ea;
       | 7H: name("fnstcw");   Ew;
       ELSE nameerror; Ed;
       END;
     ELSE
       i := b1 MOD 8;
       CASE b1 OF
       | 0C0H..0C7H:  name("fld");   ST(i);
       | 0C8H..0CFH:  name("fxch");  ST(i);
       | 0D0H:        name("fnop");
       | 0E0H:        name("fchs");
       | 0E1H:        name("fabs");
       | 0E4H:        name("ftst");
       | 0E5H:        name("fxam");
       | 0E8H:        name("fld1");
       | 0E9H:        name("fldl2t");
       | 0EAH:        name("fldl2e");
       | 0EBH:        name("fldpi");
       | 0ECH:        name("fldlg2");
       | 0EDH:        name("fldln2");
       | 0EEH:        name("fldz");
       | 0F0H:        name("f2xm1");
       | 0F1H:        name("fyl2x");
       | 0F2H:        name("fptan");
       | 0F3H:        name("fpatan");
       | 0F4H:        name("fxtract");
       | 0F5H:        name("fprem1");
       | 0F6H:        name("fdecstp");
       | 0F7H:        name("fincstp");
       | 0F8H:        name("fprem");
       | 0F9H:        name("fyl2xp1");
       | 0FAH:        name("fsqrt");
       | 0FBH:        name("fsincos");
       | 0FCH:        name("frndint");
       | 0FDH:        name("fscale");
       | 0FEH:        name("fsin");
       | 0FFH:        name("fcos");
       ELSE error;
       END;
     END;
  | 0DAH:
     IF b1 < 0C0H THEN
       DEC(cpos);
       ea;
       CASE reg OF
       | 0H: name("fiadd");  Ed;
       | 1H: name("fimul");  Ed;
       | 2H: name("ficom");  Ed;
       | 3H: name("ficomp"); Ed;
       | 4H: name("fisub");  Ed;
       | 5H: name("fisubr"); Ed;
       | 6H: name("fidiv");  Ed;
       | 7H: name("fidivr"); Ed;
       END;
     ELSE
       i := b1 MOD 8;
       CASE b1 OF
       | 0C0H..0C7H:  name("fcmovb");  ST(0); ST(i);
       | 0C8H..0CFH:  name("fcmove");  ST(0); ST(i);
       | 0D0H..0D7H:  name("fcmovbe"); ST(0); ST(i);
       | 0D8H..0DFH:  name("fcmovu");  ST(0); ST(i);
       | 0E9H: name("fucompp");
       ELSE error;
       END;
     END;
  | 0DBH:
     IF b1 < 0C0H THEN
       DEC(cpos);
       ea;
       CASE reg OF
       | 0H: name("fild");  Ed;
       | 2H: name("fist");  Ed;
       | 3H: name("fistp"); Ed;
       | 5H: name("fld");   Et;
       | 7H: name("fstp");  Et;
       ELSE error;
       END;
     ELSE
       i := b1 MOD 8;
       CASE b1 OF
       | 0C0H..0C7H:  name("fcmovnb");  ST(0); ST(i);
       | 0C8H..0CFH:  name("fcmovne");  ST(0); ST(i);
       | 0D0H..0D7H:  name("fcmovnbe"); ST(0); ST(i);
       | 0D8H..0DFH:  name("fcmovnu");  ST(0); ST(i);
       | 0E2H:  name("fnclex");
       | 0E3H:  name("fninit");
       | 0E8H..0EFH:  name("fucomi");   ST0; ST(i);
       | 0F0H..0F7H:  name("fcomi");    ST0; ST(i);
       ELSE error;
       END;
     END;
  | 0DCH:
     IF b1 < 0C0H THEN
       DEC(cpos);
       ea;
       CASE reg OF
       | 0H: name("fadd");  Eq;
       | 1H: name("fmul");  Eq;
       | 2H: name("fcom");  Eq;
       | 3H: name("fcomp"); Eq;
       | 4H: name("fsub");  Eq;
       | 5H: name("fsubr"); Eq;
       | 6H: name("fdiv");  Eq;
       | 7H: name("fdivr"); Eq;
       END;
     ELSE
       i := b1 MOD 8;
       CASE b1 OF
       | 0C0H..0C7H:  name("fadd");  ST(i); ST(0);
       | 0C8H..0CFH:  name("fmul");  ST(i); ST(0);
       | 0E0H..0E7H:  name("fsubr"); ST(i); ST(0);
       | 0E8H..0EFH:  name("fsub");  ST(i); ST(0);
       | 0F0H..0F7H:  name("fdivr"); ST(i); ST(0);
       | 0F8H..0FFH:  name("fdiv");  ST(i); ST(0);
       ELSE error;
       END;
     END;
  | 0DDH:
     IF b1 < 0C0H THEN
       DEC(cpos);
       ea;
       CASE reg OF
       | 0H: name("fld");    Eq;
       | 2H: name("fst");    Eq;
       | 3H: name("fstp");   Eq;
       | 4H: name("frstor"); Ea;
       | 6H: name("fnsave");  Ea;
       | 7H: name("fnstsw");  Ew;
       ELSE error;
       END;
     ELSE
       i := b1 MOD 8;
       CASE b1 OF
       | 0C0H..0C7H:  name("ffree");  ST(i);
       | 0D0H..0D7H:  name("fst");    ST(i);
       | 0D8H..0DFH:  name("fstp");   ST(i);
       | 0E0H..0E7H:  name("fucom");  ST(i);
       | 0E8H..0EFH:  name("fucomp"); ST(i);
       ELSE error;
       END;
     END;
  | 0DEH:
     IF b1 < 0C0H THEN
       DEC(cpos);
       ea;
       CASE reg OF
       | 0H: name("fiadd");  Ew;
       | 1H: name("fimul");  Ew;
       | 2H: name("ficom");  Ew;
       | 3H: name("ficomp"); Ew;
       | 4H: name("fisub");  Ew;
       | 5H: name("fisubr"); Ew;
       | 6H: name("fidiv");  Ew;
       | 7H: name("fidivr"); Ew;
       ELSE error;
       END;
     ELSE
       i := b1 MOD 8;
       CASE b1 OF
       | 0C0H..0C7H:        name("faddp");  ST(i); ST(0);
       | 0C8H..0CFH:        name("fmulp");  ST(i); ST(0);
       | 0D9H:              name("fcompp");
       | 0E0H..0E7H:        name("fsubrp"); ST(i); ST(0);
       | 0E8H..0EFH:        name("fsubp");  ST(i); ST(0);
       | 0F0H..0F7H:        name("fdivrp"); ST(i); ST(0);
       | 0F8H..0FFH:        name("fdivp");  ST(i); ST(0);
       ELSE error;
       END;
     END;
  | 0DFH:
     IF b1 < 0C0H THEN
       DEC(cpos);
       ea;
       CASE reg OF
       | 0H: name("fild");  Ew;
       | 2H: name("fist");  Ew;
       | 3H: name("fistp"); Ew;
       | 4H: name("fbld");  Et;
       | 5H: name("fild");  Eq;
       | 6H: name("fbstp"); Et;
       | 7H: name("fistp"); Eq;
       ELSE error;
       END;
     ELSE
       i := b1 MOD 8;
       CASE b1 OF
       | 0E0H: name("fstsw"); AX;
       | 0E8H..0EFH:  name("fucomip"); ST0; ST(i);
       | 0F0H..0F7H:  name("fcomip");  ST0; ST(i);
       ELSE error;
       END;
     END;
  END;
END float;

PROCEDURE Grp1;
BEGIN
  CASE reg OF
  | 0H: name("add");
  | 1H: name("or");
  | 2H: name("adc");
  | 3H: name("sbb");
  | 4H: name("and");
  | 5H: name("sub");
  | 6H: name("xor");
  | 7H: name("cmp");
  END;
END Grp1;

(*
PROCEDURE ArOp2;
BEGIN
  CASE reg OF
  | 0H: name("add");
  | 2H: name("adc");
  | 3H: name("sbb");
  | 5H: name("sub");
  | 7H: name("cmp");
  ELSE
    error;
  END;
END ArOp2;
*)

PROCEDURE Grp2;
BEGIN
  CASE reg OF
  | 0H: name("rol");
  | 1H: name("ror");
  | 2H: name("rcl");
  | 3H: name("rcr");
  | 4H: name("shl");
  | 5H: name("shr");
  | 7H: name("sar");
  ELSE nameerror;
  END;
END Grp2;

PROCEDURE jump_name(cond: CARDINAL);
VAR
  flags: kt.FLAGS;
BEGIN
  IF current THEN
    current := mem.GetFlags(flags);
  END;
  CASE cond OF
  | 00H: wast(current AND  flags.O); name("jo");
  | 01H: wast(current AND ~flags.O); name("jno");
  | 02H: wast(current AND  flags.C); name("jb");
  | 03H: wast(current AND ~flags.C); name("jnb");
  | 04H: wast(current AND  flags.Z); name("jz");
  | 05H: wast(current AND ~flags.Z); name("jnz");
  | 06H: wast(current AND  (flags.C OR flags.Z)); name("jbe");
  | 07H: wast(current AND ~(flags.C OR flags.Z)); name("jnbe");
  | 08H: wast(current AND  flags.S); name("js");
  | 09H: wast(current AND ~flags.S); name("jns");
  | 0AH: wast(current AND  flags.P); name("jp");
  | 0BH: wast(current AND ~flags.P); name("jnp");
  | 0CH: wast(current AND ~(flags.S = flags.O)); name("jl");
  | 0DH: wast(current AND  (flags.S = flags.O)); name("jnl");
  | 0EH: wast(current AND  (flags.Z OR (flags.S # flags.O))); name("jle");
  | 0FH: wast(current AND ~(flags.Z OR (flags.S # flags.O))); name("jnle");
  END;
END jump_name;

CONST
  UP   = CHR(018H);
  DOWN = CHR(019H);
  SELF = CHR(01BH);
  INF  = 'infinite loop';

PROCEDURE _jump_direct8 (unconditional: BOOLEAN);
VAR
  addr: kt.ADDRESS;
BEGIN
<* PUSH *>
<* COVERFLOW- *>
  IF displ < 0 THEN
    ASSERT(-displ<=MAX(sys.CARD8));
    addr := cpos-CARDINAL(-displ);
  ELSE
    ASSERT(displ<=MAX(sys.CARD8));
    addr := cpos+VAL(CARDINAL,displ);
  END;
<* POP *>
  IF bpos > addr THEN
    ws(' ; '); wc(UP);
  ELSIF bpos < addr THEN
    ws(' ; '); wc(DOWN);
  ELSIF unconditional THEN
    ws(' ; '); ws(INF);
  ELSE
    ws(' ; '); wc(SELF);
  END;
END _jump_direct8;


PROCEDURE _jump_direct32 (unconditional: BOOLEAN);
VAR
  addr: kt.ADDRESS;
BEGIN
<* PUSH *>
<* COVERFLOW- *>
  IF displ >= 0 THEN
    addr := cpos + VAL(CARDINAL,displ);
  ELSE
    addr := cpos - (CARDINAL(-BITSET(displ))+1);
  END;
<* POP *>
  IF bpos > addr THEN
    ws(' ; '); wc(UP);
  ELSIF bpos < addr THEN
    ws(' ; '); wc(DOWN);
  ELSIF unconditional THEN
    ws(' ; '); ws(INF);
  ELSE
    ws(' ; '); wc(SELF);
  END;
END _jump_direct32;

PROCEDURE instr_0F;
VAR
  cop2: sys.CARD8;
BEGIN
  cop2 := sys.CARD8(get_b1());
  CASE cop2 OF
  | 000H: ea;
      CASE reg OF
      | 0H:   name("sldt"); Eww;
      | 1H:   name("str");  Eww;
      | 2H:   name("lldt"); Ew;
      | 3H:   name("ltr");  Ew;
      | 4H:   name("verr"); Ew;
      | 5H:   name("verw"); Ew;
      ELSE nameerror; Ew;
      END;
  | 001H: ea;
      CASE reg OF
      | 0H:   name("sgdt"); Ep;
      | 1H:   name("sidt"); Ep;
      | 2H:   name("lgdt"); Ep;
      | 3H:   name("lidt"); Ep;
      | 4H:   name("smsw"); Eww;
      | 6H:   name("lmsw"); Ew;
      | 7H:   name("invlpg"); Ea;
      ELSE nameerror; Ep;
      END;
  | 002H: ea; name("lar");  Reg; E;
  | 003H: ea; name("lsl");  Reg; E;
(* 4..5 *)
  | 006H:     name("clts");
(* 7 *)
  | 008H:     name ("invd");
  | 009H:     name ("wbinvd");
(* 0A *)
  | 00BH:     name ("ud2");
(* 0C..1F *)
  | 020H: n_reg; name("mov"); reg32; CR(n);
  | 021H: n_reg; name("mov"); reg32; DR(n);
  | 022H: n_reg; name("mov"); CR(n); reg32;
  | 023H: n_reg; name("mov"); DR(n); reg32;
  | 024H: n_reg; name("mov"); reg32; TR(n);
(* 25 *)
  | 026H: n_reg; name("mov"); TR(n); reg32;
(* 27..2F *)
  | 030H: name ("wrmsr");
  | 031H: name ("rdtsc");
  | 032H: name ("rdmsr");
  | 033H: name ("rdpmc");
(* 34..3F *)
  | 040H: ea;     name("cmovo");  Reg; E;
  | 041H: ea;     name("cmovno"); Reg; E;
  | 042H: ea;     name("cmovc");  Reg; E;
  | 043H: ea;     name("cmovnc"); Reg; E;
  | 044H: ea;     name("cmovz");  Reg; E;
  | 045H: ea;     name("cmovnz"); Reg; E;
  | 046H: ea;     name("cmovbe"); Reg; E;
  | 047H: ea;     name("cmova");  Reg; E;
  | 048H: ea;     name("cmovs");  Reg; E;
  | 049H: ea;     name("cmovns"); Reg; E;
  | 04AH: ea;     name("cmovp");  Reg; E;
  | 04BH: ea;     name("cmovnp"); Reg; E;
  | 04CH: ea;     name("cmovl");  Reg; E;
  | 04DH: ea;     name("cmovnl"); Reg; E;
  | 04EH: ea;     name("cmovle"); Reg; E;
  | 04FH: ea;     name("cmovnle");Reg; E;
(* 50..5F *)
  | 060H: ea;     name ("punpcklbw"); mm; Emd;
  | 061H: ea;     name ("punpcklwd"); mm; Emd;
  | 062H: ea;     name ("punpckldq"); mm; Emd;
  | 063H: ea;     name ("packsswb"); mm; Em;
  | 064H: ea;     name ("pcmpgtb"); mm; Em;
  | 065H: ea;     name ("pcmpgtw"); mm; Em;
  | 066H: ea;     name ("pcmpgtd"); mm; Em;
  | 067H: ea;     name ("packuswb"); mm; Em;
  | 068H: ea;     name ("punpckhbw"); mm; Emd;
  | 069H: ea;     name ("punpckhwd"); mm; Emd;
  | 06AH: ea;     name ("punpckhdq"); mm; Emd;
  | 06BH: ea;     name ("packssdw"); mm; Em;
(* 6C..6D *)
  | 06EH: ea;     name ("movd"); mm; Ed;
  | 06FH: ea;     name ("movq"); mm; Em;
(* 70 *)
  | 071H: ea; data8;
      CASE reg OF
      | 2H : name ("psrlw"); Em; _data8;
      | 4H : name ("psraw"); Em; _data8;
      | 6H : name ("psllw"); Em; _data8;
      ELSE
        nameerror; Em; _data8;
      END;
      IF mod # 3 THEN error; END;
  | 072H: ea; data8;
      CASE reg OF
      | 2H : name ("psrld"); Em; _data8;
      | 4H : name ("psrad"); Em; _data8;
      | 6H : name ("pslld"); Em; _data8;
      ELSE
        nameerror; Em; _data8;
      END;
      IF mod # 3 THEN error; END;
  | 073H: ea; data8;
      CASE reg OF
      | 2H : name ("psrlq"); Em; _data8;
      | 6H : name ("psllq"); Em; _data8;
      ELSE
        nameerror; Em; _data8;
      END;
      IF mod # 3 THEN error; END;
  | 074H: ea;     name ("pcmpeqb"); mm; Em;
  | 075H: ea;     name ("pcmpeqw"); mm; Em;
  | 076H: ea;     name ("pcmpeqd"); mm; Em;
  | 077H:         name ("emms");
(* 78..7D *)
  | 07EH: ea;     name ("movd"); Ed; mm;
  | 07FH: ea;     name ("movq"); Em; mm;
  | 080H..08FH: disp32;    jump_name(cop2 MOD 16);  _disp32; _jump_direct32 (FALSE);
  | 090H: ea;     name("seto");   Eb;
  | 091H: ea;     name("setno");  Eb;
  | 092H: ea;     name("setc");   Eb;
  | 093H: ea;     name("setnc");  Eb;
  | 094H: ea;     name("setz");   Eb;
  | 095H: ea;     name("setnz");  Eb;
  | 096H: ea;     name("setbe");  Eb;
  | 097H: ea;     name("seta");   Eb;
  | 098H: ea;     name("sets");   Eb;
  | 099H: ea;     name("setns");  Eb;
  | 09AH: ea;     name("setp");   Eb;
  | 09BH: ea;     name("setnp");  Eb;
  | 09CH: ea;     name("setl");   Eb;
  | 09DH: ea;     name("setnl");  Eb;
  | 09EH: ea;     name("setle");  Eb;
  | 09FH: ea;     name("setnle"); Eb;
  | 0A0H:         name("push"); ws("fs");
  | 0A1H:         name("pop");  ws("fs");
  | 0A2H:         name ("cpuid");
  | 0A3H: ea;           name("bt");   E; Reg;
  | 0A4H: ea; data8;    name("shld"); E; Reg; _data8;
  | 0A5H: ea;           name("shld"); E; Reg; ws(", cl");
(* A6 = CMPXCHG XBTS *)
(* A7 = CMPXCHG IBTS *)
  | 0A8H:               name("push"); ws("gs");
  | 0A9H:               name("pop");  ws("gs");
  | 0AAH:               name ("rsm");
  | 0ABH: ea;           name("bts");  E; Reg;
  | 0ACH: ea; data8;    name("shrd"); E; Reg; _data8;
  | 0ADH: ea;           name("shrd"); E; Reg; ws(", cl");
(* AE *)
  | 0AFH: ea;           name("imul"); Reg; E;
  | 0B0H: ea;           name("cmpxchg"); Eb; reg8;
  | 0B1H: ea;           name("cmpxchg"); E;  Reg;
  | 0B2H: ea;           name("lss"); Reg; us := TRUE; IF s32 THEN Ep ELSE Ed END;
                        IF mod=3 THEN error; END;
  | 0B3H: ea;           name("btr");  E; Reg;
  | 0B4H: ea;           name("lfs"); Reg; us := TRUE; IF s32 THEN Ep ELSE Ed END;
                        IF mod=3 THEN error; END;
  | 0B5H: ea;           name("lgs"); Reg; us := TRUE; IF s32 THEN Ep ELSE Ed END;
                        IF mod=3 THEN error; END;
  | 0B6H: ea;           name("movzx");  Reg; Eb;
  | 0B7H: ea;           name("movzx");  reg32; Ew; us := TRUE;
(* B8..B9 *)
  | 0BAH: ea;
      CASE reg OF
      | 4H: data8;   name("bt");  E; _data8;
      | 5H: data8;   name("bts"); E; _data8;
      | 6H: data8;   name("btr"); E; _data8;
      | 7H: data8;   name("btc"); E; _data8;
      ELSE error;
      END;
  | 0BBH: ea;    name("btc"); E; Reg;
  | 0BCH: ea;    name("bsf"); Reg; E;
  | 0BDH: ea;    name("bsr"); Reg; E;
  | 0BEH: ea;    name("movsx"); Reg; Eb;
  | 0BFH: ea;    name("movsx"); reg32; Ew;
  | 0C0H: ea;    name ("xadd"); Eb; reg8;
  | 0C1H: ea;    name ("xadd"); E;  Reg;
(* C2..C6 *)
  | 0C7H: ea;
      CASE reg OF
      | 1:       name ("cmpxchg8b"); Eq;
      ELSE error;
      END;
  | 0C8H..0CFH: name ("bswap"); r32name (cop2 MOD 8);
(* D0 *)
  | 0D1H: ea; name ("psrlw"); mm; Em;
  | 0D2H: ea; name ("psrld"); mm; Em;
  | 0D3H: ea; name ("psrlq"); mm; Em;
(* D4 *)
  | 0D5H: ea; name ("pmullw"); mm; Em;
(* D6..D7 *)
  | 0D8H: ea; name ("psubusb"); mm; Em;
  | 0D9H: ea; name ("psubusw"); mm; Em;
(* DA *)
  | 0DBH: ea; name ("pand"); mm; Em;
  | 0DCH: ea; name ("paddusb"); mm; Em;
  | 0DDH: ea; name ("paddusw"); mm; Em;
(* DE *)
  | 0DFH: ea; name ("pandn"); mm; Em;
(* E0 *)
  | 0E1H: ea; name ("psraw"); mm; Em;
  | 0E2H: ea; name ("psrad"); mm; Em;
(* E3..E4 *)
  | 0E5H: ea; name ("pmulhw"); mm; Em;
(* E6..E7 *)
  | 0E8H: ea; name ("psubsb"); mm; Em;
  | 0E9H: ea; name ("psubsw"); mm; Em;
(* EA *)
  | 0EBH: ea; name ("por"); mm; Em;
  | 0ECH: ea; name ("paddsb"); mm; Em;
  | 0EDH: ea; name ("paddsw"); mm; Em;
(* EE *)
  | 0EFH: ea; name ("pxor"); mm; Em;
(* F0 *)
  | 0F1H: ea; name ("psllw"); mm; Em;
  | 0F2H: ea; name ("pslld"); mm; Em;
  | 0F3H: ea; name ("psllq"); mm; Em;
(* F4 *)
  | 0F5H: ea; name ("pmaddwd"); mm; Em;
(* F6..F7 *)
  | 0F8H: ea; name ("psubb"); mm; Em;
  | 0F9H: ea; name ("psubw"); mm; Em;
  | 0FAH: ea; name ("psubd"); mm; Em;
(* FB *)
  | 0FCH: ea; name ("paddb"); mm; Em;
  | 0FDH: ea; name ("paddw"); mm; Em;
  | 0FEH: ea; name ("paddd"); mm; Em;
(* FF *)
  ELSE error;
  END;
END instr_0F;

PROCEDURE one_instr;
VAR
  cop : sys.CARD8;
BEGIN
  second_arg := FALSE;
  cop := sys.CARD8(get_b1());
  CASE cop OF
  | 000H: ea;     name("add"); Eb;     reg8;
  | 001H: ea;     name("add"); E;      Reg;
  | 002H: ea;     name("add"); reg8;   Eb;
  | 003H: ea;     name("add"); Reg;    E;
  | 004H: data8;  name("add"); AL;    _data8;
  | 005H: Data;   name("add"); A;     _data;
  | 006H:         name("push es");
  | 007H:         name("pop es");
  | 008H: ea;     name("or");  Eb;     reg8;
  | 009H: ea;     name("or");  E;      Reg;
  | 00AH: ea;     name("or");  reg8;   Eb;
  | 00BH: ea;     name("or");  Reg;    E;
  | 00CH: data8;  name("or");  AL;    _data8;
  | 00DH: Data;   name("or");  A;     _data;
  | 00EH:         name("push cs");
  | 00FH:         instr_0F;
  | 010H: ea;     name("adc"); Eb;    reg8;
  | 011H: ea;     name("adc"); E;     Reg;
  | 012H: ea;     name("adc"); reg8;  Eb;
  | 013H: ea;     name("adc"); Reg;   E;
  | 014H: data8;  name("adc"); AL;   _data8;
  | 015H: Data;   name("adc"); A;    _data;
  | 016H:         name("push ss");
  | 017H:         name("pop ss");
  | 018H: ea;     name("sbb"); Eb;    reg8;
  | 019H: ea;     name("sbb"); E;     Reg;
  | 01AH: ea;     name("sbb"); reg8;  Eb;
  | 01BH: ea;     name("sbb"); Reg;   E;
  | 01CH: data8;  name("sbb"); AL;   _data8;
  | 01DH: Data;   name("sbb"); A;    _data;
  | 01EH:         name("push ds");
  | 01FH:         name("pop ds");
  | 020H: ea;     name("and"); Eb;    reg8;
  | 021H: ea;     name("and"); E;     Reg;
  | 022H: ea;     name("and"); reg8;  Eb;
  | 023H: ea;     name("and"); Reg;   E;
  | 024H: data8;  name("and"); AL;   _data8;
  | 025H: Data;   name("and"); A;    _data;
  | 026H:                      prefix := TRUE; segm_override := _es;
  | 027H:         name("daa");
  | 028H: ea;     name("sub"); Eb;    reg8;
  | 029H: ea;     name("sub"); E;     Reg;
  | 02AH: ea;     name("sub"); reg8;  Eb;
  | 02BH: ea;     name("sub"); Reg;   E;
  | 02CH: data8;  name("sub"); AL;   _data8;
  | 02DH: Data;   name("sub"); A;    _data;
  | 02EH:                      prefix := TRUE; segm_override := _cs;
  | 02FH:         name("das");
  | 030H: ea;     name("xor"); Eb;    reg8;
  | 031H: ea;     name("xor"); E;     Reg;
  | 032H: ea;     name("xor"); reg8;  Eb;
  | 033H: ea;     name("xor"); Reg;   E;
  | 034H: data8;  name("xor"); AL;   _data8;
  | 035H: Data;   name("xor"); A;    _data;
  | 036H:                      prefix := TRUE; segm_override := _ss;
  | 037H:         name("aaa");
  | 038H: ea;     name("cmp"); Eb;    reg8;
  | 039H: ea;     name("cmp"); E;     Reg;
  | 03AH: ea;     name("cmp"); reg8;  Eb;
  | 03BH: ea;     name("cmp"); Reg;   E;
  | 03CH: data8;  name("cmp"); AL;   _data8;
  | 03DH: Data;   name("cmp"); A;    _data;
  | 03EH:                      prefix := TRUE; segm_override := _ds;
  | 03FH:         name("aas");
  | 040H..047H: reg := cop MOD 8; name("inc");  Reg;
  | 048H..04FH: reg := cop MOD 8; name("dec");  Reg;
  | 050H..057H: reg := cop MOD 8; name("push"); Reg;
  | 058H..05FH: reg := cop MOD 8; name("pop");  Reg;
  | 060H:         us := TRUE; IF s32 THEN name("pushad") ELSE name ("pusha"); END;
  | 061H:         us := TRUE; IF s32 THEN name("popad")  ELSE name ("popa");  END;
  | 062H: ea;     name("bound"); Reg; Ea;
  | 063H: ea;     name("arpl");  Ew; reg16;
  | 064H:         prefix := TRUE; segm_override := _fs;
  | 065H:         prefix := TRUE; segm_override := _gs;
  | 066H:         prefix := TRUE; opsize   := TRUE; s32 := NOT size32;
  | 067H:         prefix := TRUE; addrsize := TRUE; a32 := NOT addr32;
  | 068H: Data;       name("push"); _data;
  | 069H: ea; Data;   name("imul"); Reg; E; _data;
  | 06AH: data8;      name("push"); _data8;
  | 06BH: ea; data8;  name("imul"); Reg; E; _data8;
  | 06CH:         name("insb");
  | 06DH:         us := TRUE; IF s32 THEN name("insd") ELSE name ("insw") END;
  | 06EH:         IF segm_override = none THEN
                    name("outsb");
                  ELSE
                    name("outs"); ws ("dx, "); bbracket;
                    ua := TRUE;
                    IF a32 THEN ws("esi]") ELSE ws("si]"); END;
                  END;
  | 06FH:         IF segm_override = none THEN
                    us := TRUE;
                    IF s32 THEN name("outsd") ELSE name ("outsw") END;
                  ELSE
                    name("outs"); ws ("dx, "); bracket;
                    ua := TRUE;
                    IF a32 THEN ws("esi]") ELSE ws("si]"); END;
                  END;
  | 070H..07FH: disp8; jump_name(cop MOD 16); _disp8; _jump_direct8 (FALSE);
  | 080H: ea; data8;    Grp1; Eb; _data8;
  | 081H: ea; Data;     Grp1; E;  _data;
  | 082H: ea; data8;   Grp1;  Eb; _data8;
  | 083H: ea;  data8;   Grp1; E;  _data8;
  | 084H: ea;  name("test"); Eb;    reg8;
  | 085H: ea;  name("test"); E;     Reg;
  | 086H: ea;  name("xchg"); Eb;    reg8;
  | 087H: ea;  name("xchg"); E;     Reg;
  | 088H: ea;  name("mov");  Eb;    reg8;
  | 089H: ea;  name("mov");  E;     Reg;
  | 08AH: ea;  name("mov");  reg8;  Eb;
  | 08BH: ea;  name("mov");  Reg;   E;
  | 08CH: ea;  name("mov");  E;    sreg;
  | 08DH: ea;  name("lea");  Reg;   Ea;
  | 08EH: ea;  name("mov");  sreg;  E;
  | 08FH: ea;  name("pop");  E;
  | 090H:      name("nop");
  | 091H..097H:name("xchg"); reg := cop MOD 8; A; Reg;
  | 098H:      us:=TRUE; IF s32 THEN name("cwde") ELSE name("cbw") END;
  | 099H:      us:=TRUE; IF s32 THEN name("cdq") ELSE name("cwd") END;
  | 09AH: ptr16_; name("call far"); _ptr16_;
  | 09BH:      name("wait");
  | 09CH:      us:=TRUE; IF s32 THEN name("pushfd") ELSE name("pushf") END;
  | 09DH:      us:=TRUE; IF s32 THEN name("popfd")  ELSE name("popf")  END;
  | 09EH:      name("sahf");
  | 09FH:      name("lahf");
  | 0A0H: disp; name("mov"); AL; ws(', ');  adr_fix(1, TRUE);
  | 0A1H: disp; name("mov"); A;  ws(', ');  adr_fix(101, TRUE);
  | 0A2H: disp; name("mov"); adr_fix(1, TRUE); ws(', '); AL;
  | 0A3H: disp; name("mov"); adr_fix(101, TRUE); ws(', '); A;
  | 0A4H:    IF segm_override = none THEN
               name("movsb");
             ELSE
               name("movs");
               ua := TRUE;
               IF a32 THEN ws("es:[edi]") ELSE ws("es:[di]") END;
               bbracket;
               IF a32 THEN ws("esi]") ELSE ws("si]"); END;
             END;
  | 0A5H:    IF segm_override = none THEN
               us:=TRUE;
               IF s32 THEN name("movsd") ELSE name ("movsw") END;
             ELSE
               name("movs");
               ua := TRUE;
               IF a32 THEN ws("es:[edi]") ELSE ws("es:[di]") END;
               bracket;
               IF a32 THEN ws("esi]") ELSE ws("si]"); END;
             END;
  | 0A6H:    IF segm_override = none THEN
               name("cmpsb")
             ELSE
               name("cmps");
               bbracket;
               ua := TRUE;
               IF a32 THEN ws("esi], es:[edi]") ELSE ws("si], es:[di]"); END;
             END;
  | 0A7H:    IF segm_override = none THEN
               us:=TRUE;
               IF s32 THEN name("cmpsd") ELSE name ("cmpsw") END;
             ELSE
               name("cmps");
               bracket;
               ua := TRUE;
               IF a32 THEN ws("esi], es:[edi]") ELSE ws("si], es:[di]"); END;
             END;
  | 0A8H: data8;  name("test"); AL;     _data8;
  | 0A9H: Data;   name("test"); A;      _data;
  | 0AAH:      name("stosb");
  | 0ABH:      us:=TRUE; IF s32 THEN name("stosd") ELSE name("stosw") END;
  | 0ACH:      name("lodsb");
  | 0ADH:      us:=TRUE; IF s32 THEN name("lodsd") ELSE name("lodsw") END;
  | 0AEH:      name("scasb");
  | 0AFH:      us:=TRUE; IF s32 THEN name("scasd") ELSE name("scasw") END;
  | 0B0H..0B7H: reg := cop MOD 8; data8;  name("mov");  reg8; _data8;
  | 0B8H..0BFH: reg := cop MOD 8; Data;   name("mov");  Reg;  _data;
  | 0C0H: ea; data8; Grp2; Eb; _data8;
  | 0C1H: ea; data8; Grp2; E;  _data8;
  | 0C2H: data16; name("ret"); _data16;
  | 0C3H:         name("ret");
  | 0C4H: ea;     name("les"); Reg; us:=TRUE; IF s32 THEN Ep ELSE Ed END;
                  IF mod=3 THEN error; END;
  | 0C5H: ea;     name("lds"); Reg; us:=TRUE; IF s32 THEN Ep ELSE Ed END;
                  IF mod=3 THEN error; END;
  | 0C6H: ea; data8;   name("mov"); Eb; _data8;
  | 0C7H: ea; Data;    name("mov"); E;  _data;
  | 0C8H: data16; name("enter"); _data16; data8; _data8;
  | 0C9H:         name("leave");
  | 0CAH: data16; name("retf"); _data16;
  | 0CBH:         name("retf");
  | 0CCH:         name("int 3");
  | 0CDH: data8;  name("int"); _data8;
  | 0CEH:         name("into");
  | 0CFH:         us:=TRUE; IF s32 THEN name("iretd") ELSE name ("iret") END;
  | 0D0H: ea; Grp2; Eb; ws(', 1');
  | 0D1H: ea; Grp2; E;  ws(', 1');
  | 0D2H: ea; Grp2; Eb; ws(', cl');
  | 0D3H: ea; Grp2; E;  ws(', cl');
  | 0D4H: cop := CARDINAL(get_b1()); IF cop = 00AH THEN name("aam"); ELSE error; END;
  | 0D5H: cop := CARDINAL(get_b1()); IF cop = 00AH THEN name("aad"); ELSE error; END;
(* D6 *)
  | 0D7H:     name("xlat");
  | 0D8H..0DFH: float(cop);
  | 0E0H: disp8;  name("loopne"); _disp8;
  | 0E1H: disp8;  name("loope");  _disp8;
  | 0E2H: disp8;  name("loop");   _disp8;
  | 0E3H: disp8;  name("jcxz");   _disp8;
  | 0E4H: data8;  name("in");   AL;  _data8;
  | 0E5H: data8;  name("in");   A;   _data8;
  | 0E6H: data8;  name("out"); _data8; AL;
  | 0E7H: data8;  name("out"); _data8; A;
  | 0E8H: disp;  name("call"); _disp;
  | 0E9H: disp;  name("jmp");  _disp; _jump_direct32 (TRUE);
  | 0EAH: ptr16_;  name("jmp far"); _ptr16_;
  | 0EBH: disp8; name("jmp"); _disp8; _jump_direct8 (TRUE);
  | 0ECH:        name("in"); AL; ws (", dx");
  | 0EDH:        name("in"); A; ws (", dx");
  | 0EEH:        name("out"); ws ("dx, "); AL;
  | 0EFH:        name("out"); ws ("dx, "); A;
  | 0F0H:        name("lock");  prefix := TRUE;
(* 0F1H *)
  | 0F2H:        name("repne"); prefix := TRUE;
  | 0F3H:        name("rep");   prefix := TRUE;
  | 0F4H:        name("hlt");
  | 0F5H:        name("cmc");
  | 0F6H: ea;
      CASE reg OF
      | 0H: data8;  name("test"); Eb; _data8;
      | 2H:         name("not");  Eb;
      | 3H:         name("neg");  Eb;
      | 4H:         name("mul");  AL;    Eb;
      | 5H:         name("imul"); AL;    Eb;
      | 6H:         name("div");  AL;    Eb;
      | 7H:         name("idiv"); AL;    Eb;
      ELSE error;
      END;
  | 0F7H: ea;
      CASE reg OF
      | 0H: Data;    name("test"); E; _data;
      | 2H:          name("not");  E;
      | 3H:          name("neg");  E;
      | 4H:          name("mul");  A;    E;
      | 5H:          name("imul"); A;    E;
      | 6H:          name("div");  A;    E;
      | 7H:          name("idiv"); A;    E;
      ELSE error;
      END;
  | 0F8H:   name("clc");
  | 0F9H:   name("stc");
  | 0FAH:   name("cli");
  | 0FBH:   name("sti");
  | 0FCH:   name("cld");
  | 0FDH:   name("std");
  | 0FEH: ea;
      CASE reg OF
      | 0H: name("inc"); Eb;
      | 1H: name("dec"); Eb;
      ELSE error;
      END;
  | 0FFH: ea;
      CASE reg OF
      | 0H: name("inc"); E;
      | 1H: name("dec"); E;
      | 2H: name("call"); E;
      | 3H: name("call far"); Ea;
      | 4H: name("jmp"); E;
      | 5H: name("jmp far"); Ea;
      | 6H: name("push"); E;
      ELSE error;
      END;
  ELSE error;
  END;
END one_instr;

PROCEDURE IsCall(addr: kt.ADDRESS): CARDINAL;
VAR
  cop: sys.CARD8;
BEGIN
  size32 := NOT Seg16;
  addr32 := NOT Seg16;
  opsize := FALSE;
  segm_override := none;
  cpos := addr;
  s32  := size32;
  a32  := addr32;
  us := FALSE;
  ua := FALSE;
  useg := FALSE;
  was_error := FALSE;
  resolve := FALSE;

  LOOP
    cop := sys.CARD8(get_b1());
    CASE cop OF
    | 026H: prefix := TRUE; segm_override := _es;
    | 02EH: prefix := TRUE; segm_override := _cs;
    | 036H: prefix := TRUE; segm_override := _ss;
    | 03EH: prefix := TRUE; segm_override := _ds;
    | 064H: prefix := TRUE; segm_override := _fs;
    | 065H: prefix := TRUE; segm_override := _gs;
    | 066H: prefix := TRUE; opsize   := TRUE; s32 := NOT size32;
    | 067H: prefix := TRUE; addrsize := TRUE; a32 := NOT addr32;

    | 09AH: ptr16_; RETURN cpos - addr;
    | 0E8H: disp; RETURN cpos - addr;
    | 0FFH: ea;
      CASE reg OF
      | 2H: Ed; RETURN cpos - addr;
      | 3H: Ea; RETURN cpos - addr;
      ELSE
        RETURN 0;
      END;
    ELSE
      RETURN 0;
    END;
  END;
EXCEPT
  IF exc.IsCurrentSource(DisasmSource) THEN RETURN 0; END;
END IsCall;

PROCEDURE IsJmpForDll (addr: kt.ADDRESS): kt.ADDRESS;
VAR
  Addr: CARDINAL;
  save: CARDINAL;
BEGIN
  save := cpos;
  cpos := addr;
  resolve := FALSE;

  Addr := 0;
  IF get_b2() = 25FFH THEN
    IF mem.Get(kt.ADDRESS(get_b4()), sys.ADR(Addr), 4) THEN
      cpos := save;
      RETURN Addr
    END;
  END;
  cpos := save;
  RETURN 0;
EXCEPT
  cpos := save;
  IF exc.IsCurrentSource(DisasmSource) THEN
    RETURN 0;
  END;
END IsJmpForDll;


PROCEDURE IsRet(addr: kt.ADDRESS): BOOLEAN;
VAR
  cop: sys.CARD8;
BEGIN
  size32 := NOT Seg16;
  addr32 := NOT Seg16;
  opsize := FALSE;
  segm_override := none;
  cpos := addr;
  s32  := size32;
  a32  := addr32;
  us := FALSE;
  ua := FALSE;
  useg := FALSE;
  was_error := FALSE;
  resolve := FALSE;

  cop := sys.CARD8(get_b1());
  CASE cop OF
  | 0C3H, 0C2H, 0CBH, 0CAH: RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
EXCEPT
  IF exc.IsCurrentSource(DisasmSource) THEN RETURN FALSE; END;
END IsRet;

(* Дисассемблирование команды по адресу, результат - изображение   *)
(* команды и ее длина. При успешном дисассемблировании вернет TRUE *)
(* при невозможности - FALSE                                       *)

PROCEDURE Disasm( addr: kt.ADDRESS; curr: BOOLEAN; VAR asm, info: ARRAY OF CHAR; VAR ln: CARDINAL; get_prefix_only := FALSE: BOOLEAN): BOOLEAN;
BEGIN
  size32 := NOT Seg16;
  addr32 := NOT Seg16;
  segm_override := none;
  d_str [0] := 0C;
  d_info[0] := 0C;
  cpos := addr;
  bpos := addr;
  s32 := size32;
  a32 := addr32;
  us := FALSE;
  ua := FALSE;
  addrsize := FALSE;
  opsize := FALSE;
  useg := FALSE;
  was_error := FALSE;
  current := curr;
  resolve := TRUE;
  REPEAT
    prefix := FALSE;
    one_instr;
  UNTIL NOT prefix OR get_prefix_only;
  asm [0] := 0C;
  info[0] := 0C;
  IF was_error THEN
     COPY ("; ??? cannot disassemble command", asm);
  END;
  IF (segm_override # none) AND NOT useg THEN
    CASE segm_override OF
    | _es: fmt.append (asm, "es: ");
    | _cs: fmt.append (asm, "cs: ");
    | _ss: fmt.append (asm, "ss: ");
    | _ds: fmt.append (asm, "ds: ");
    | _fs: fmt.append (asm, "fs: ");
    | _gs: fmt.append (asm, "gs: ");
    END;
  END;
  IF addrsize AND NOT ua THEN
     fmt.append (asm, "adrsiz: ");
  END;
  IF opsize AND NOT us THEN
     fmt.append (asm, "opsiz: ");
  END;
  fmt.append (asm, "%s", d_str);
  COPY(d_info, info);
  ln := CARDINAL(cpos) - addr;
  RETURN NOT was_error;
EXCEPT
  IF exc.IsCurrentSource(DisasmSource) THEN RETURN FALSE; END;
END Disasm;

<* PUSH *> <* WOFF301+ *>

PROCEDURE dummy_ResolveAddr(loc: kt.ADDRESS; addr: kt.ADDRESS; VAR str: ARRAY OF CHAR): BOOLEAN;
BEGIN
  fmt.append(str, b4_fmt, addr);
  RETURN FALSE;
END dummy_ResolveAddr;

PROCEDURE dummy_ResolveEA(loc: kt.ADDRESS; addr: kt.ADDRESS; len: CARDINAL; VAR str: ARRAY OF CHAR);
BEGIN
END dummy_ResolveEA;

<* POP *>

BEGIN
  Seg16 := FALSE;
  ResolveAddr := dummy_ResolveAddr;
  ResolveEA   := dummy_ResolveEA;
  exc.AllocateSource(DisasmSource);
  b1_fmt       := '0%$2XH';
  b2_fmt       := '0%$4XH';
  b4_fmt       := '0%$8XH';
  no_align_fmt := '0%XH';
  resolve := TRUE;
END Dasm_x86.
