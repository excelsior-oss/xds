(* Created by KDV;
   pretty printing of internal representation including attributes of
   operations;
   also the module used to visualize results of reordering;
   the module is imported only in the case of show_segms option is turned on
*)

MODULE ocirtxt;

IMPORT o := ocir,
       def := OcirDef,
       ir,
       Emit,
       prc := opProcs,
       b := BitVect,
       env := xiEnv,
       StreamFile,
       opIO,
       xcStr,
       TextIO,
       SYSTEM;

VAR    CurSegmLen : INTEGER;
       stat_file : StreamFile.ChanId;
VAR    file_res  : StreamFile.OpenResults;

PROCEDURE print*(fmt: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
  VAR msg: env.String;        (* this procedure does not flush output buffer *)
BEGIN
  xcStr.dprn_txt(msg,fmt,x);
  TextIO.WriteString(stat_file,msg^);
END print;


VAR SHOW_ATTRS,
    LEFT       : BOOLEAN;

PROCEDURE new_line();
BEGIN
   print("\n");
   IF NOT LEFT THEN
       print("\t\t\t\t\t");
   END;
END new_line;


PROCEDURE PrintRegister*(r : def.Register);
BEGIN
   CASE r OF
      | def.EAX :  print("eax");
      | def.ECX :  print("ecx");
      | def.EDX :  print("edx");
      | def.EBX :  print("ebx");
      | def.ESP :  print("esp");
      | def.EBP :  print("ebp");
      | def.ESI :  print("esi");
      | def.EDI :  print("edi");
      | def.ST0..def.ST7
                :  print("st%d",r-def.ST0);
      | def.UNDEF_REG :
                 print("UNREG");
   END;
END PrintRegister;

PROCEDURE PrintRegs*(r : def.Regs);
   VAR i : SHORTINT;
BEGIN
   print("{ ");
   FOR i:=0 TO def.RegsNum-1 DO
       IF i IN r THEN
           PrintRegister(i);
           print(" ");
       END;
   END;
   print("}");
END PrintRegs;

PROCEDURE PrintFlag*(f : SHORTINT);
BEGIN
   CASE f OF
      | def.CF   :  print("cf");
      | def.PF   :  print("pf");
      | def.AF   :  print("af");
      | def.ZF   :  print("zf");
      | def.SF   :  print("sf");
      | def._OF  :  print("of");
      | def.C0   :  print("c0");
      | def.C1   :  print("c1");
      | def.C2   :  print("c2");
      | def.C3   :  print("c3");
   END;
END PrintFlag;

PROCEDURE PrintFlags*(f : def.Flags);
   VAR i : SHORTINT;
BEGIN
   print("{ ");
   FOR i:=0 TO def.FlagsNum-1 DO
       IF i IN f THEN
           PrintFlag(i);
           print(" ");
       END;
   END;
   print("}");
END PrintFlags;

PROCEDURE PrintLocal (l: ir.Local);
BEGIN
    IF ir.Locals[l].Name <> NIL THEN
        print ("_");
        print (ir.Locals [l].Name^);
    ELSE
        print ("_tmp%d", l);
    END;
END PrintLocal;

PROCEDURE PrintProcName (p: INTEGER);
VAR name: ir.NameType;
BEGIN
  name := prc.ProcName (p);
  print ("_");
  print (name^);
END PrintProcName;


PROCEDURE PrintMem* (a- : Emit.AddrMode);
VAR b:  BOOLEAN;
    op: Emit.OffsRecPtr;
    pp: Emit.ProcRecPtr;
BEGIN
    b := FALSE;
    IF a.local <> ir.UNDEFINED THEN
        IF ir.IsExternal (a.local) THEN
            PrintLocal (a.local);
            b := TRUE;
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
    IF a.offs <> 0 THEN
        IF a.offs > 0 THEN
            IF b THEN
                print ("+");
            END;
        END;
        print ("%d", a.offs);
    END;
    IF (a.r1.r <> Emit.UNDEF_REG) OR (a.r2.r <> Emit.UNDEF_REG) THEN
        b := FALSE;
        print ("[");
        IF a.r1.r <> Emit.UNDEF_REG THEN
            PrintRegister (a.r1.r);
            b := TRUE;
        END;
        IF a.r2.r <> Emit.UNDEF_REG THEN
            IF b THEN
                print ("+");
            END;
            PrintRegister (a.r2.r);
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


PROCEDURE PrintImm*();
BEGIN
   print("<cnst>");
END PrintImm;

PROCEDURE PrintOpCode*(op : def.OpCode);
BEGIN
   CASE op OF
     | def.BSWAP     :      print("bswap\t");
     | def.CMPXCHG   :      print("cmpxchg\t");
     | def.CMPXCHG8B :      print("cmpxchg8b\t");
     | def._IN       :      print("in\t");
     | def.LEA       :      print("lea\t");
     | def.MOV       :      print("mov\t");
     | def.MOVSX     :      print("movsx\t");
     | def.MOVZX     :      print("movzx\t");
     | def.OUT       :      print("out\t");
     | def.POP       :      print("pop\t");
     | def.POPA      :      print("popa\t");
     | def.POPAD     :      print("popad\t");
     | def.PUSH      :      print("push\t");
     | def.PUSHA     :      print("pusha\t");
     | def.PUSHAD    :      print("pushad\t");
     | def.XCHG      :      print("xchg\t");
     | def.LDS       :      print("lds\t");
     | def.LES       :      print("les\t");
     | def.LFS       :      print("lfs\t");
     | def.LGS       :      print("lgs\t");
     | def.LSS       :      print("lss\t");
     | def.CLC       :      print("clc\t");
     | def.CLD       :      print("cld\t");
     | def.CLI       :      print("cli\t");
     | def.CLTS      :      print("clts\t");
     | def.CMC       :      print("cmc\t");
     | def.LAHF      :      print("lahf\t");
     | def.POPF      :      print("popf\t");
     | def.POPFD     :      print("popfd\t");
     | def.PUSHF     :      print("pushf\t");
     | def.PUSHFD    :      print("pushfd\t");
     | def.SAHF      :      print("sahf\t");
     | def.STC       :      print("stc\t");
     | def.STD       :      print("std\t");
     | def.STI       :      print("sti\t");
     | def.AAA       :      print("aaa\t");
     | def.AAD       :      print("aad\t");
     | def.AAM       :      print("aam\t");
     | def.AAS       :      print("aas\t");
     | def.ADC       :      print("adc\t");
     | def.ADD       :      print("add\t");
     | def.CBW       :      print("cbw\t");
     | def.CDQ       :      print("cdq\t");
     | def.CMP       :      print("cmp\t");
     | def.CWD       :      print("cwd\t");
     | def.CWDE      :      print("cwde\t");
     | def.DAA       :      print("daa\t");
     | def.DAS       :      print("das\t");
     | def._DEC      :      print("dec\t");
     | def._DIV      :      print("div\t");
     | def.IDIV      :      print("idiv\t");
     | def._INC      :      print("inc\t");
     | def.IMUL      :      print("imul\t");
     | def.MUL       :      print("mul\t");
     | def.NEG       :      print("neg\t");
     | def.SBB       :      print("sbb\t");
     | def.SUB       :      print("sub\t");
     | def.XADD      :      print("xadd\t");
     | def._AND      :      print("and\t");
     | def._NOT      :      print("not\t");
     | def._OR       :      print("or\t");
     | def.RCL       :      print("rcl\t");
     | def.RCR       :      print("rcr\t");
     | def.ROL       :      print("rol\t");
     | def.ROR       :      print("ror\t");
     | def.SAL       :      print("sal\t");
     | def.SAR       :      print("sar\t");
     | def.SHL       :      print("shl\t");
     | def.SHLD      :      print("shld\t");
     | def.SHR       :      print("shr\t");
     | def.SHRD      :      print("shrd\t");
     | def.TEST      :      print("test\t");
     | def.XOR       :      print("xor\t");
     | def.CMPS      :      print("cmps\t");
     | def.CMPSB     :      print("cmpsb\t");
     | def.CMPSD     :      print("cmpsd\t");
     | def.CMPSW     :      print("cmpsw\t");
     | def.INS       :      print("ins\t");
     | def.INSB      :      print("insb\t");
     | def.INSD      :      print("insd\t");
     | def.INSW      :      print("insw\t");
     | def.LODS      :      print("lods\t");
     | def.LODSB     :      print("lodsb\t");
     | def.LODSD     :      print("lodsd\t");
     | def.LODSW     :      print("lodsw\t");
     | def.MOVS      :      print("movs\t");
     | def.MOVSB     :      print("movsb\t");
     | def.MOVSD     :      print("movsd\t");
     | def.MOVSW     :      print("movsw\t");
     | def.OUTS      :      print("outs\t");
     | def.OUTSB     :      print("outsb\t");
     | def.OUTSD     :      print("outsd\t");
     | def.OUTSW     :      print("outsw\t");
     | def.SCAS      :      print("scas\t");
     | def.SCASB     :      print("scasb\t");
     | def.SCASD     :      print("scasd\t");
     | def.SCASW     :      print("scasw\t");
     | def.STOS      :      print("stos\t");
     | def.STOSB     :      print("stosb\t");
     | def.STOSD     :      print("stosd\t");
     | def.STOSW     :      print("stosw\t");
     | def.XLAT      :      print("xlat\t");
     | def.XLATB     :      print("xlatb\t");
     | def.BSF       :      print("bsf\t");
     | def.BSR       :      print("bsr\t");
     | def.BT        :      print("bt\t");
     | def.BTC       :      print("btc\t");
     | def.BTR       :      print("btr\t");
     | def.BTS       :      print("bts\t");
     | def.CALL      :      print("call\t");
     | def.JMP       :      print("jmp\t");
     | def.RET       :      print("ret\t");
     | def.JA        :      print("ja\t");
     | def.JAE       :      print("jae\t");
     | def.JB        :      print("jb\t");
     | def.JBE       :      print("jbe\t");
     | def.JC        :      print("jc\t");
     | def.JCXZ      :      print("jcxz\t");
     | def.JE        :      print("je\t");
     | def.JECXZ     :      print("jecxz\t");
     | def.JG        :      print("jg\t");
     | def.JGE       :      print("jge\t");
     | def.JL        :      print("jl\t");
     | def.JLE       :      print("jle\t");
     | def.JNA       :      print("jna\t");
     | def.JNAE      :      print("jnae\t");
     | def.JNB       :      print("jnb\t");
     | def.JNBE      :      print("jnbe\t");
     | def.JNC       :      print("jnc\t");
     | def.JNE       :      print("jne\t");
     | def.JNG       :      print("jng\t");
     | def.JNGE      :      print("jnge\t");
     | def.JNL       :      print("jnl\t");
     | def.JNLE      :      print("jnle\t");
     | def.JNO       :      print("jno\t");
     | def.JNP       :      print("jnp\t");
     | def.JNS       :      print("jns\t");
     | def.JNZ       :      print("jnz\t");
     | def.JO        :      print("jo\t");
     | def.JP        :      print("jp\t");
     | def.JPE       :      print("jpe\t");
     | def.JPO       :      print("jpo\t");
     | def.JS        :      print("js\t");
     | def.JZ        :      print("jz\t");
     | def._LOOP     :      print("loop\t");
     | def.LOOPE     :      print("loope\t");
     | def.LOOPNE    :      print("loopne\t");
     | def.LOOPNZ    :      print("loopnz\t");
     | def.LOOPZ     :      print("loopz\t");
     | def.ENTER     :      print("enter\t");
     | def.LEAVE     :      print("leave\t");
     | def.SETA      :      print("seta\t");
     | def.SETAE     :      print("setae\t");
     | def.SETB      :      print("setb\t");
     | def.SETBE     :      print("setbe\t");
     | def.SETC      :      print("setc\t");
     | def.SETE      :      print("sete\t");
     | def.SETG      :      print("setg\t");
     | def.SETGE     :      print("setge\t");
     | def.SETL      :      print("setl\t");
     | def.SETLE     :      print("setle\t");
     | def.SETNA     :      print("setna\t");
     | def.SETNAE    :      print("setnae\t");
     | def.SETNB     :      print("setnb\t");
     | def.SETNBE    :      print("setnbe\t");
     | def.SETNC     :      print("setnc\t");
     | def.SETNE     :      print("setne\t");
     | def.SETNG     :      print("setng\t");
     | def.SETNGE    :      print("setnge\t");
     | def.SETNL     :      print("setnl\t");
     | def.SETNLE    :      print("setnle\t");
     | def.SETNO     :      print("setno\t");
     | def.SETNP     :      print("setnp\t");
     | def.SETNS     :      print("setns\t");
     | def.SETNZ     :      print("setnz\t");
     | def.SETO      :      print("seto\t");
     | def.SETP      :      print("setp\t");
     | def.SETPE     :      print("setpe\t");
     | def.SETPO     :      print("setpo\t");
     | def.SETS      :      print("sets\t");
     | def.SETZ      :      print("setz\t");
     | def.BOUND     :      print("bound\t");
     | def.INT       :      print("int\t");
     | def.INTO      :      print("into\t");
     | def.IRET      :      print("iret\t");
     | def.IRETD     :      print("iretd\t");
     | def.HLT       :      print("hlt\t");
     | def.NOP       :      print("nop\t");
     | def.WAIT      :      print("wait\t");
     | def.F2XM1     :      print("f2xm1\t");
     | def.FABS      :      print("fabs\t");
     | def.FADD      :      print("fadd\t");
     | def.FADDP     :      print("faddp\t");
     | def.FBLD      :      print("fbld\t");
     | def.FBSTP     :      print("fbstp\t");
     | def.FCHS      :      print("fchs\t");
     | def.FCLEX     :      print("fclex\t");
     | def.FCOM      :      print("fcom\t");
     | def.FCOMP     :      print("fcomp\t");
     | def.FCOMPP    :      print("fcompp\t");
     | def.FCOS      :      print("fcos\t");
     | def.FDECSTP   :      print("fdecstp\t");
     | def.FDIV      :      print("fdiv\t");
     | def.FDIVP     :      print("fdivp\t");
     | def.FMUL      :      print("fmul\t");
     | def.FMULP     :      print("fmulp\t");
     | def.FDIVR     :      print("fdivr\t");
     | def.FDIVRP    :      print("fdivrp\t");
     | def.FFREE     :      print("ffree\t");
     | def.FIADD     :      print("fiadd\t");
     | def.FICOM     :      print("ficom\t");
     | def.FICOMP    :      print("ficomp\t");
     | def.FIDIV     :      print("fidiv\t");
     | def.FIDIVR    :      print("fidivr\t");
     | def.FILD      :      print("fild\t");
     | def.FINCSTP   :      print("fincstp\t");
     | def.FINIT     :      print("finit\t");
     | def.FIST      :      print("fist\t");
     | def.FISTP     :      print("fistp\t");
     | def.FISUB     :      print("fisub\t");
     | def.FISUBR    :      print("fisubr\t");
     | def.FLD       :      print("fld\t");
     | def.FLD1      :      print("fld1\t");
     | def.FLDCW     :      print("fldcw\t");
     | def.FLDENV    :      print("fldenv\t");
     | def.FLDL2E    :      print("fldl2e\t");
     | def.FLDL2T    :      print("fldl2t\t");
     | def.FLDLG2    :      print("fldlg2\t");
     | def.FLDLN2    :      print("fldln2\t");
     | def.FLDPI     :      print("fldpi\t");
     | def.FLDZ      :      print("fldz\t");
     | def.FNCLEX    :      print("fnclex\t");
     | def.FNINIT    :      print("fninit\t");
     | def.FNOP      :      print("fnop\t");
     | def.FNSAVE    :      print("fnsave\t");
     | def.FNSTCW    :      print("fnstcw\t");
     | def.FNSTENV   :      print("fnstenv\t");
     | def.FNSTSW    :      print("fnstsw\t");
     | def.FPATAN    :      print("fpatan\t");
     | def.FPREM     :      print("fprem\t");
     | def.FPREM1    :      print("fprem1\t");
     | def.FPTAN     :      print("fptan\t");
     | def.FRNDINT   :      print("frndint\t");
     | def.FRSTOR    :      print("frstor\t");
     | def.FSAVE     :      print("fsave\t");
     | def.FSCALE    :      print("fscale\t");
     | def.FSIN      :      print("fsin\t");
     | def.FSINCOS   :      print("fsincos\t");
     | def.FSQRT     :      print("fsqrt\t");
     | def.FST       :      print("fst\t");
     | def.FSTCW     :      print("fstcw\t");
     | def.FSTENV    :      print("fstenv\t");
     | def.FSTP      :      print("fstp\t");
     | def.FSTSW     :      print("fstsw\t");
     | def.FSUB      :      print("fsub\t");
     | def.FSUBP     :      print("fsubp\t");
     | def.FSUBR     :      print("fsubr\t");
     | def.FSUBRP    :      print("fsubrp\t");
     | def.FTST      :      print("ftst\t");
     | def.FUCOM     :      print("fucom\t");
     | def.FUCOMP    :      print("fucomp\t");
     | def.FUCOMPP   :      print("fucompp\t");
     | def.FWAIT     :      print("fwait\t");
     | def.FXAM      :      print("fxam\t");
     | def.FXCH      :      print("fxch\t");
     | def.FXTRACT   :      print("fxtract\t");
     | def.FYL2X     :      print("fyl2x\t");
     | def.FYL2XP1   :      print("fyl2xp1\t");
     | def.ARPL      :      print("arpl\t");
     | def.CPUID     :      print("cpuid\t");
     | def.INVD      :      print("invd\t");
     | def.INVLPG    :      print("invlpg\t");
     | def.LAR       :      print("lar\t");
     | def.LGDT      :      print("lgdt\t");
     | def.LIDT      :      print("lidt\t");
     | def.LLDT      :      print("lldt\t");
     | def.LMSW      :      print("lmsw\t");
     | def.LSL       :      print("lsl\t");
     | def.LTR       :      print("ltr\t");
     | def.RDMSR     :      print("rdmsr\t");
     | def.RSM       :      print("rsm\t");
     | def.SGDT      :      print("sgdt\t");
     | def.SIDT      :      print("sidt\t");
     | def.SLDT      :      print("sldt\t");
     | def.SMSW      :      print("smsw\t");
     | def.STR       :      print("str\t");
     | def.VERR      :      print("verr\t");
     | def.VERW      :      print("verw\t");
     | def.WBINVD    :      print("wbinvd\t");
     | def.WRMSR     :      print("wrmsr\t");
     | def.UNDEF_OP  :      print("undef_op\t");
   END;
END PrintOpCode;

PROCEDURE PrintOpOpnd*(opnd : def.OpOpnd);
BEGIN
   CASE def.GetOpOpndKind(opnd.r) OF
     | def.REG         :
                         PrintRegister(opnd.r);
     | def.MEM         : PrintMem(opnd.a);
     | def.IMM         : PrintImm();
   END;
END PrintOpOpnd;

PROCEDURE PrintArg*(a- : def.OpArg);
BEGIN
     print("(* args:\t");
     PrintRegs(a.r);
     print(", ");
     PrintRegs(a.ra);
     print(", ");
     PrintFlags(a.f);
     print(", ");
     IF a.v.l = def.BAD_MEM THEN print("BAD MEM");
     ELSIF a.v.l <> ir.UNDEFINED THEN
         print("%d, EXT = %d, Addressed = %d",
               a.v.l,ir.IsExternal(a.v.l), ir.Locals[a.v.l].Addressed)
     END;
     print("*)");
     new_line();
END PrintArg;

PROCEDURE PrintRes*(r- : def.OpRes);
BEGIN
     print("(* res:\t");
     PrintRegs(r.r);
     print(", ");
     PrintFlags(r.f);
     print(", ");
     IF r.v.l = def.BAD_MEM THEN print("BAD MEM");
     ELSIF r.v.l <> ir.UNDEFINED THEN print("%d",r.v.l)
     END;
     print("*)");
     new_line();
END PrintRes;

PROCEDURE PrintArgRes*(op- : def.Operation);
BEGIN
     PrintArg(op.attrs.arg);
     PrintRes(op.attrs.res);
END PrintArgRes;

PROCEDURE PrintPUSH_SIZE(op- : def.Operation);
BEGIN
  print(" PUSH_SIZE = %d, NEXT_PUSH_SIZE = %d ",
        op.PUSH_SIZE,
        op.NEXT_PUSH_SIZE);
END PrintPUSH_SIZE;

PROCEDURE PrintClocks*(op- : def.Operation);
BEGIN
   IF (op.attrs.clocks <> def.UNDEF_CLOCKS) THEN
      print(" latency = %d ",op.attrs.clocks);
   ELSE
      print(" latency is undefined ");
   END;
END PrintClocks;

PROCEDURE PrintPairTag*(op- : def.Operation);
BEGIN
   print(" pair = ");
   CASE op.attrs.pair OF
     | def.FNP : print("FNP");
     | def.FP  : print("FX");
     | def.UV  : print("UV");
     | def.PU  : print("PU");
     | def.PV  : print("PV");
     | def.NP  : print("NP");
     | def.UNDEF_PAIR
             : print("undefined");
   END;
END PrintPairTag;

PROCEDURE PrintGraphInfo*(op- : def.Operation);
   VAR i : INTEGER;
BEGIN
   print("(* in nodes:\t");
   FOR i:=0 TO CurSegmLen-1 DO
       IF o.BVIn(o.InOut[op.pos].in, i) THEN print("%d ",i); END;
   END;
   print("\n   out nodes:\t");
   FOR i:=0 TO CurSegmLen-1 DO
       IF o.BVIn(o.InOut[op.pos].out, i) THEN print("%d ",i); END;
   END;
   print("\n*)\n");
END PrintGraphInfo;


PROCEDURE PrintAttrs*(op- : def.Operation);
(* implicitly used SEGM_LEN from reorder.ob2 *)
BEGIN
   PrintArgRes(op);
   PrintPUSH_SIZE(op);
   PrintClocks(op);
   PrintPairTag(op);
   PrintGraphInfo(op);
END PrintAttrs;

PROCEDURE PrintOperation*(VAR op : def.Operation);
BEGIN
   IF SHOW_ATTRS THEN
       print("(* pos = %d",op.pos);
       print("\t");
       PrintPUSH_SIZE(op);
       PrintClocks(op);
       print("\t");
       PrintPairTag(op);
       print("*)");
       new_line();
       PrintArgRes(op);
       PrintGraphInfo(op);
   END;
   PrintOpCode(op.code);
   IF def.GetOpOpndKind(op.dest.r) <> def.UNDEF_OPND THEN
       PrintOpOpnd(op.dest);
   END;
   IF def.GetOpOpndKind(op.src.r) <> def.UNDEF_OPND THEN
       print(", ");
       PrintOpOpnd(op.src);
   END;
   new_line();
END PrintOperation;

PROCEDURE PrintPairOperation*(op1-, op2- : def.Operation;
                              stream1, stream2 : StreamFile.ChanId);
BEGIN
   stat_file := stream1;
   PrintOpCode(op1.code);
   IF def.GetOpOpndKind(op1.dest.r) <> def.UNDEF_OPND THEN
       PrintOpOpnd(op1.dest);
   END;
   IF def.GetOpOpndKind(op1.src.r) <> def.UNDEF_OPND THEN
       print(", ");
       PrintOpOpnd(op1.src);
   END;
   new_line();
   IF stream1 <> stream2 THEN
       stat_file := stream2;
       PrintOpCode(op2.code);
       IF def.GetOpOpndKind(op2.dest.r) <> def.UNDEF_OPND THEN
           PrintOpOpnd(op2.dest);
       END;
       IF def.GetOpOpndKind(op2.src.r) <> def.UNDEF_OPND THEN
           print(", ");
           PrintOpOpnd(op2.src);
       END;
       new_line();
   END;
END PrintPairOperation;


PROCEDURE PrintSegment*(s : o.Segment; show_attrs : BOOLEAN; left : BOOLEAN);
BEGIN
   SHOW_ATTRS := show_attrs;
   LEFT := left;
   CurSegmLen := s.code_len;
   print("\n---------------------------------------------\n");
   o.ThruSegment(s,PrintOperation);
END PrintSegment;

PROCEDURE PrintSegmentInterval*(s : o.Segment; show_attrs : BOOLEAN; left : BOOLEAN;
                                from, to : INTEGER);
VAR i : INTEGER;
BEGIN
   SHOW_ATTRS := show_attrs;
   LEFT := left;
   CurSegmLen := s.code_len;
   print("\n---------------------------------------------\n");
   FOR i := from TO to DO PrintOperation(s.code[i]); END;
END PrintSegmentInterval;

PROCEDURE PrintPairSegment*(SegmentNum : LONGINT; s1, s2 : o.Segment;
                            stream1, stream2 : StreamFile.ChanId);
   VAR i : INTEGER;
   VAR old_stat_file : StreamFile.ChanId;
BEGIN
   CurSegmLen := s1.code_len;
   SHOW_ATTRS := FALSE;
   LEFT := TRUE;
   old_stat_file := stat_file;
   stat_file := stream1;
   print("\n------------- Segment No. %d ------------\n",SegmentNum);
   IF stream1 <> stream2 THEN
       stat_file := stream2;
       print("\n------------- Segment No. %d ------------\n",SegmentNum);
   END;
   FOR i:=0 TO CurSegmLen-1 DO
       PrintPairOperation(s1.code[i],s2.code[i], stream1, stream2);
   END;
   stat_file := old_stat_file;
END PrintPairSegment;

PROCEDURE Init_ocirtxt*(ocirtxt_name : ARRAY OF CHAR);
BEGIN

   CurSegmLen := 0;
   StreamFile.Open(stat_file,
                   ocirtxt_name,
                   StreamFile.write+StreamFile.text + StreamFile.old,
                   file_res);
   IF file_res <> StreamFile.opened THEN
       opIO.print("Statistics  file %s not opened, error %d.\n",
                  ocirtxt_name, file_res);
       ASSERT(FALSE);
   END;
END Init_ocirtxt;

END ocirtxt.
