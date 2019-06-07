<* IF TARGET_VAX AND (NOT DEFINED(VAX_GEN_ASM)) THEN *> -- only target VAX
  <* NEW VAX_GEN_ASM- *>
<* END *>

MODULE Options;
IMPORT
  pc  := pcK,
  env := xiEnv,
  ir,
  at  := opAttrs,
  std := opStd,
  str := Strings,
<* IF TARGET_RISC THEN *>
  nms  := ObjNames,
  tune := opTune,
  Emit_PPC,
<* END *>
  COMPILER,
  SYSTEM;
<* IF TARGET_386 THEN *> IMPORT xProfRTS; <* END *>
IMPORT tune:=opTune;
TYPE
  INT = LONGINT;

(* ------------ o p t i o n s   f o r   d e b u g g i n g ------------ *)
VAR
<* IF pcvis THEN *>
  pcvis* : BOOLEAN;    (* print internal tree *)
<* END *>


<* IF TARGET_VAX THEN *>

CONST
  OPT_CODENAMEPREFIXED* = "CODENAMEPREFIXED";
  OPT_DATAASOBJECT*     = "DATAASOBJECT";

<* END *>


(* ---- c o m p i l e r   o p t i o n s   &   e q u a t i o n s ------------- *)
CONST
  OPT_GENCPREF* = "GENCPREF";

CONST
  EQU_OBJFMT* = "OBJFMT";

CONST
  objFormat*  = "objFormat";

  objVMS*     = "VMS";
  objCOFF*    = "COFF";
  objELF*     = "ELF";
  objOMF*     = "OMF";
  objAOUT*    = "AOUT";
  objGO32*    = "GO32";
  objASM*     = "ASM";
  objGAS*     = "GAS";

VAR
  DefaultOBJFMT* : ARRAY 24 OF CHAR;

CONST
  OPT_DBGNESTEDPROC *= "DBGNESTEDPROC";
  OPT_DBG_QUALIDS *= "DBGQUALIDS";
  DBG_LEX_BLOCKS *= "DBGLEXBLOCKS";

  EQU_DBGFMT *= "DBGFMT";

  dbgFormat *= "dbgFormat";

  dbg_NONE   *= '';
  dbg_CV     *= "CODEVIEW";
  dbg_HLL    *= "HLL";
  dbg_XHLL   *= "XHLL";
  dbg_EDIF   *= "EDIF";
  dbg_STAB   *= "STAB";
  dbg_STABX  *= "STABX";
  dbg_DWARF  *= "DWARF";
  dbg_DWARF_1*= "DWARF1.1";
  dbg_DWARF_2*= "DWARF2.0";
  dbg_REF    *= "REF";
  dbg_GO32   *= "GO32";

  -- Текстовый формат представления отладочной информации,
  -- используется только для отладочных целей
  dbg_TEXT  *= "TEXT";




CONST
<* IF TARGET_RISC THEN *>
  EQU_ABI* = "ABI";
  abiPOWEROPEN = "POWEROPEN";
  abiV4ABI     = "V4ABI";
  abiEABI      = "EABI";
  DefaultABI   = abiPOWEROPEN;
<* ELSIF TARGET_VAX THEN *>
  EQU_MAINNAME = "MAINNAME";
<* END *>

CONST
  default_MODE = at.CompModeSet{
                   at.convert_while   -- преобразовывать WHILE в REPEAT
                  ,at.stack_checked   -- вставлять контроль за стеком
<* IF TARGET_VAX OR TARGET_68k OR TARGET_RISC OR TARGET_SPARC THEN *>
                  ,at.CAP_proc        -- CAP by procedure call
<* END *>
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
                  ,at.DOPEEPHOLE      -- ЇаRЁўR¤Ёвм й_<_ўл_ RЇвЁ┐Ё жЁЁ
<* END *>
              --  ,at.copystr_proc    -- копировать строки вызовом процедуры
                  ,at.new_segment     -- каждую процедуру в отдельный сегмент
              --  ,at.debug           -- generate debug information
              --  ,at.lineno          -- вставлять номера сторок в obj-файл
              --  ,at.history         -- include history output
              --  ,at.NOALIAS         -- в модуле нет ссылок на локалы
                  ,at.DEF_LIBs        -- библиотеки по умолчанию
                  ,at.INIT_PTR        -- инициализировать указатели NIL-ом
                  ,at.o2_cmds         -- include info on Oberon-2 commands
                 };
VAR
  DefaultLevel*: INTEGER;

CONST NO      = 0;
      YES     = 1;
      DEFAULT = 2;


CONST AllLanguages* = pc.AllLanguages;

CONST LangsWithCLikeStrings* = pc.LangsWithCLikeStrings;

      LangsWithOpenArrays* = pc.LangsWithOpenArrays;

      LangsWithTypedRecords* = pc.LangsWithTypedRecords;

      OOP_Langs* = pc.OOP_langs;

      LangsWithModuleConstructors* = pc.LangsWithModuleConstructors;
      
      LangsWithPushingParams_RtoL* = pc.LangsWithPushingParams_RtoL;

      LangsWithSpecifiedComputingParamsOrder* = pc.LangsWithSpecifiedComputingParamsOrder;

      LangsWithComputingParams_RtoL* = pc.LangsWithComputingParams_RtoL;

      LangsWithSEQParams* = pc.LangsWithSEQParams;

      LangsWithGarbageCollector* = pc.LangsWithGarbageCollector;

      LangsWithRTTI* = pc.LangsWithRTTI;

      LangsAllowCommands* = pc.LangsAllowCommands;

      LangsWithTypeDescriptors* = pc.LangsWithTypeDescriptors;

PROCEDURE OptionSpecified (s-: ARRAY OF CHAR): INT;
VAR b: BOOLEAN;
BEGIN
  b := env.config.Option (s);
  IF env.config.lev > DefaultLevel THEN
    RETURN ORD (b);
  ELSE
    RETURN DEFAULT;
  END;
END OptionSpecified;

PROCEDURE CheckFlag (s-: ARRAY OF CHAR; f: at.CompModeType);
BEGIN
  CASE OptionSpecified (s) OF
  | YES: INCL(at.COMP_MODE, f);
  | NO:  EXCL(at.COMP_MODE, f);
  ELSE
  END;
END CheckFlag;

PROCEDURE SetEqu_IfNotSpecified (name-, val-: ARRAY OF CHAR);
  VAR s : pc.STRING;
BEGIN
  env.config.Equation(name, s);
(*
  io.print("(lev = %d) (def_lev = %d) '%s' = ", env.config.lev, DefaultLevel, name);
  IF (s = NIL) THEN     io.print("NIL\n");
  ELSE                  io.print("'%s'\n", s^);
  END;
*)
  IF (env.config.lev <= DefaultLevel) OR (s = NIL) OR (s^[0] = 0C) THEN
    env.config.SetEquation(name, val);
--    io.print("SetEqu res = %d)\n", env.config.res);
  END;
END SetEqu_IfNotSpecified;

CONST sep = "|" ;

PROCEDURE find_val (str-: ARRAY OF CHAR; val-: ARRAY OF CHAR) : SHORTINT;
  VAR res : SHORTINT;
    i, j, ln: INT;
    ch : CHAR;
BEGIN
   i := 0; res := 0; ln := LENGTH(val);
   LOOP
     INC(res);
     j := 0;
     LOOP
       ch := str [i];
       IF j = ln THEN
         IF (ch = sep) OR (ch = 0X) THEN RETURN res END;
         EXIT
       ELSIF ch = val[j] THEN INC(i); INC(j);
       ELSE EXIT
       END;
     END;
     LOOP
       ch := str [i];
       IF ch = 0X THEN RETURN 0 END;
       INC (i);
       IF ch = sep THEN EXIT END;
     END;
   END;
END find_val;

PROCEDURE CheckEquationVal (name-: ARRAY OF CHAR;
                            alts-: ARRAY OF CHAR;
                         def_val-: ARRAY OF CHAR ) : SHORTINT;
  VAR s : pc.STRING;
    eq, def : ARRAY 32 OF CHAR;
    res : SHORTINT;
    pos : pc.TPOS;
BEGIN
  IF at.curr_mod = NIL THEN pos := env.null_pos
  ELSE                      pos := at.curr_mod.pos
  END;
  env.config.Equation(name, s);
  IF (s # NIL) & (s[0]#0X) THEN
    COPY(s^, eq);
    str.Capitalize(eq);
    res := find_val(alts, eq);
    IF res > 0 THEN
      RETURN res
    END;
    env.errors.Warning(pos, 434, name, def_val);
  END;
  COPY(def_val, def);
  str.Capitalize(def);
  res := find_val(alts, def);
  IF res <= 0 THEN
    env.errors.Fault(pos, 434, name, def_val);
  END;
  RETURN res
END CheckEquationVal;

CONST  (* ---- t a r g e t   e n v i r o n m e n t ------------------ *)
  envs = "X86"      + sep +
         "X86DOS"   + sep +
         "X86NT"    + sep +
         "X86OS2"   + sep +
         "X86LINUX" + sep +
         "X86GO32"  + sep +
         "68K"      + sep +
         "PPCAIX"   + sep +
         "PPCNT";

  env_x86       = 1;
  env_x86dos    = 2;
  env_x86nt     = 3;
  env_x86os2    = 4;
  env_x86linux  = 5;
  env_x86go32   = 6;
  env_68k       = 7;
  env_ppc_aix   = 8;
  env_ppc_nt    = 9;

CONST  (* ---- C   C a l l   C o m p a t i b i l i t y -------------- *)
  comps =   "SYMANTEC"
    + sep + "WATCOM"
    + sep + "BORLAND"
    + sep + "OS2SYSCALL"
    + sep + "MSVC"
    + sep + "GCC"
    + sep + "DJGPP"
    + sep + "NONE";

  cc_symantec    = 1;
  cc_watcom      = 2;
  cc_borland     = 3;
  cc_os2sys_call = 4;
  cc_msvc        = 5;
  cc_gcc         = 6;
  cc_djgpp       = 7;
  cc_none        = 8;

CONST  (* ---- M o d u l a   C a l l   C o m p a t i b i l i t y ---- *)
  modcomps =   "CC"
       + sep + "NATIVE";

  mc_cc     = 1;
  mc_native = 2;

PROCEDURE get_target_options * ();
  VAR
    trg, cc, mc: SHORTINT;
    cc_name, mc_name: ARRAY 32 OF CHAR;
  CONST name_sep = "_";
  VAR s : pc.STRING;
BEGIN
  at.COMP_MODE := default_MODE;
(*
  IF env.config.Option("CAP")  THEN INCL(at.COMP_MODE, at.CAP_proc)  END;
  IF env.config.Option("TRAP") THEN INCL(at.COMP_MODE, at.TRAP_proc) END;
*)
  trg := CheckEquationVal("ENV_TARGET", envs, COMPILER.TARGET);
  CASE trg OF
  | env_x86:
      at.TARGET := at.trg_FLASHTEK;
      at.CC := at.SYMANTEC;
      at.MC := at.CC;
      cc_name := "SYMANTEC";
      mc_name := "CC";
  | env_x86dos:
      at.TARGET := at.trg_DOS4G;
      at.CC := at.WATCOM;
      at.MC := at.NATIVE;
      cc_name := "WATCOM";
      mc_name := "NATIVE";
  | env_x86nt:
      at.TARGET := at.trg_NT;
      at.CC := at.WATCOM;
      at.MC := at.NATIVE;
      cc_name := "WATCOM";
      mc_name := "NATIVE";
  | env_x86os2:
      at.TARGET := at.trg_OS2;
      at.CC := at.WATCOM;
      at.MC := at.NATIVE;
      cc_name := "WATCOM";
      mc_name := "NATIVE";
  | env_x86linux:
      at.TARGET := at.trg_LINUX;
      at.CC := at.GCC;
      at.MC := at.NATIVE;
      cc_name := "GCC";
      mc_name := "NATIVE";
  | env_x86go32:
      at.TARGET := at.trg_LINUX;
      at.CC := at.DJGPP;
      at.MC := at.CC;
      cc_name := "DJGPP";
      mc_name := "CC";
  | env_68k:
      at.TARGET := at.trg_LINUX;
      at.CC := at.MSVC;
      at.MC := at.CC;
      cc_name := "MSVC";
      mc_name := "CC";
  | env_ppc_aix:
      at.TARGET := at.trg_LINUX;
      at.CC := at.MSVC;
      at.MC := at.CC;
      cc_name := "MSVC";
      mc_name := "CC";
     <* IF TARGET_RISC THEN *>
      nms.aix_name := TRUE;
      tune.BIG_END := TRUE;
     <* END *>
  | env_ppc_nt:
      at.TARGET := at.trg_NT;
      at.CC := at.MSVC;
      at.MC := at.CC;
      cc_name := "MSVC";
      mc_name := "CC";
     <* IF TARGET_RISC THEN *>
      nms.aix_name := FALSE;
      tune.BIG_END := FALSE;
     <* END *>
  END;

  cc := CheckEquationVal("CC", comps, cc_name);
  CASE cc OF
  | cc_symantec    : at.CC := at.SYMANTEC;
  | cc_watcom      : at.CC := at.WATCOM;
  | cc_borland     : at.CC := at.BORLAND;
  | cc_os2sys_call : at.CC := at.OS2SYS_CALL;
  | cc_msvc        : at.CC := at.MSVC;
  | cc_gcc         : at.CC := at.GCC;
  | cc_djgpp       : at.CC := at.DJGPP;
  | cc_none        : (* use default CC for this platform *)
  END;

  mc := CheckEquationVal("MC", modcomps, mc_name);
  CASE mc OF
  |mc_cc      : at.MC := at.CC;
  |mc_native  : at.MC := at.NATIVE;
  END;

  CASE at.CC OF
  | at.SYMANTEC
  , at.BORLAND
  , at.DJGPP :
  | at.MSVC :
      EXCL(at.COMP_MODE, at.new_segment);
      IF OptionSpecified (OPT_GENCPREF) = DEFAULT THEN 
        env.config.SetOption(OPT_GENCPREF, TRUE) 
      END;
  | at.WATCOM:
      INCL(at.COMP_MODE, at.new_segment);
      IF OptionSpecified (OPT_GENCPREF) = DEFAULT THEN 
        env.config.SetOption(OPT_GENCPREF, FALSE) 
      END;
  | at.GCC:
      EXCL(at.COMP_MODE, at.new_segment);
      IF OptionSpecified (OPT_GENCPREF) = DEFAULT THEN 
        env.config.SetOption(OPT_GENCPREF, FALSE) 
      END;
  | at.OS2SYS_CALL :
      EXCL(at.COMP_MODE, at.new_segment);
      IF OptionSpecified (OPT_GENCPREF) = DEFAULT THEN 
        env.config.SetOption(OPT_GENCPREF, FALSE) 
      END;
  END;

<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  SetEqu_IfNotSpecified("ALIGNMENT", "8");
<* ELSE *>
  IF (trg = env_x86linux) OR (trg = env_x86go32) THEN
    SetEqu_IfNotSpecified("ALIGNMENT", "4");
    IF OptionSpecified (OPT_GENCPREF) = DEFAULT THEN 
      env.config.SetOption(OPT_GENCPREF, FALSE) 
    END;
  ELSE
    SetEqu_IfNotSpecified("ALIGNMENT", "1");
  END;
<* END *>

  CheckFlag ("GENDEBUG", at.debug);
  IF at.debug IN at.COMP_MODE THEN
    at.COMP_MODE := at.COMP_MODE 
                  + at.CompModeSet{at.lineno(*, at.use_frame_ptr*)}
                  - at.CompModeSet{at.convert_while};
  <* IF TARGET_VAX THEN *>
    env.config.SetOption (OPT_CODENAMEPREFIXED, TRUE);
  <* END *>
  END;

  (* generate debug info with nested procedures *)
  CheckFlag (OPT_DBGNESTEDPROC, at.DbgNestedProc);

<* IF TARGET_386 THEN *>
  -- kevin: for generating stcalls x2c_profile_proc_start, etc.
  CASE CheckEquationVal("GENPROF", "NONE"+sep+"MIN"+sep+"STANDARD"+sep+"FULL", "NONE") OF
  |1 :    at.profilingMode := xProfRTS.PROF_MODE_NONE;
  |2 :    at.profilingMode := xProfRTS.PROF_MODE_MIN;
  |3 :    at.profilingMode := xProfRTS.PROF_MODE_STANDARD;
  |4 :    at.profilingMode := xProfRTS.PROF_MODE_FULL;
  END;

  IF at.profilingMode # xProfRTS.PROF_MODE_NONE THEN
    -- to avoid equal name of nested procedures
    EXCL (at.COMP_MODE, at.DbgNestedProc);
  END;
<*END*>

  -- AVY: less variables will be located at registers
  CheckFlag ("NOREGVARS", at.NoRegVars);
  CheckFlag ("ir_strict", at.ir_strict);

  -- AVY: see 'be_386\ssa.ob2.MakeFi'
  CheckFlag ("VOLATILEPLUS", at.VolatilePlus);

 <* IF DEFINED(OVERDYE) AND OVERDYE THEN *>
  -- AVY: reorder code/source table
  CheckFlag ("DBGREFINE", at.DbgRefine);
 <* END *>


<* IF TARGET_386 THEN *>
  CheckFlag ("NOOPTIMIZE", at.nooptimize);
  CheckFlag ("USEDLL", at.USEDLL);
<* END *>

 <* IF TARGET_386 AND value96 THEN *>
   IF at.nooptimize IN at.COMP_MODE THEN
     tune.BITSET_LEN_InInclExcl  := tune.BITSET_LEN;
   ELSE
     tune.BITSET_LEN_InInclExcl  := MAX(LONGINT);
   END;
   tune.BITSET_LEN_scalar      := 64;
 <* ELSE *>
   tune.BITSET_LEN_InInclExcl  := tune.BITSET_LEN;
   tune.BITSET_LEN_scalar      := tune.BITSET_LEN;
 <* END *>
   tune.lset_sz_InInclExcl := tune.BITSET_LEN_InInclExcl DIV 8;


  env.config.Equation("NAME_SEPARATOR", s);
  IF (s # NIL) THEN
    COPY(s^, at.NM_SEP);
  ELSE
    COPY(name_sep, at.NM_SEP);
  END;

END get_target_options;


(* ---------- g e n  c o d e   o p t i o n s --------------------------- *)

CONST  (* ---- C P U - t y p e ----------- *)

<* IF TARGET_68k THEN *>

  cpus =    "68040"     (* see values of CPU_type constants in opAttrs.ob2 *)
    + sep + "68060";

<* ELSIF TARGET_386 THEN *>

  cpus =    "386"      (* see values of CPU_type constants in opAttrs.ob2 *)
    + sep + "486"
    + sep + "GENERIC"
    + sep + "PENTIUM"
    + sep + "PENTIUMPRO";

<* ELSIF TARGET_RISC THEN *>

  ABIs =    abiPOWEROPEN  (* see values of ABI constants in opAttrs.ob2 *)
    + sep + abiV4ABI
    + sep + abiEABI;

 <* END *>

PROCEDURE get_gencode_options * ();
  VAR o: pc.OBJECT;
BEGIN
<* IF TARGET_68k THEN *>

  at.CPU    := CheckEquationVal("CPU",    cpus, "68040") + at.mc68040 - 1;
  at.minCPU := CheckEquationVal("MINCPU", cpus, "68060") + at.mc68040 - 1;

<* ELSIF TARGET_386 THEN *>

  at.CPU    := CheckEquationVal("CPU", cpus, "GENERIC");
  at.minCPU := CheckEquationVal("MINCPU", cpus, "386");
  IF at.minCPU = at.iGeneric THEN at.minCPU := at.i386 END;
  IF (at.minCPU > at.CPU) & (at.CPU # at.iGeneric) THEN
    env.errors.Warning(ir.NullPos, 434, "MINCPU", "386");
    at.minCPU := at.i386;
  END;

<* ELSIF TARGET_RISC THEN *>

  at.ABI    := CheckEquationVal(EQU_ABI, ABIs, DefaultABI);
  Emit_PPC.InitABI;

<* END *>

  CASE OptionSpecified ("ONECODESEG") OF
  | YES: EXCL(at.COMP_MODE, at.new_segment);
  | NO:  INCL(at.COMP_MODE, at.new_segment);
  ELSE
  END;
  IF env.config.Option ("WHILE") THEN
    EXCL(at.COMP_MODE, at.convert_while);
  END;
  CheckFlag ("NOPTRALIAS", at.NOALIAS);
  CheckFlag ("GENHISTORY", at.history);
  CheckFlag ("GENPTRINIT", at.INIT_PTR);
  CheckFlag ("GENFRAME",   at.use_frame_ptr);
  CheckFlag ("DEFLIBS",    at.DEF_LIBs);
  CheckFlag ("LINENO",     at.lineno);
  CheckFlag ("SPACE",      at.SPACE);

  CheckFlag ("DOREORDER",  at.DOREORDER);
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  CheckFlag ("DOPEEPHOLE",  at.DOPEEPHOLE);
<* END *>
  CheckFlag ("GENDLL",     at.GENDLL);

<* IF TARGET_VAX THEN *>
 <* IF VAX_GEN_ASM THEN *>
  CheckFlag ("GENASM", at.GENASM);
 <* ELSE *>
  EXCL(at.COMP_MODE, at.GENASM);
 <* END *>
  IF at.lineno IN at.COMP_MODE THEN
    env.config.SetOption (OPT_CODENAMEPREFIXED, TRUE);
  END;
<* ELSIF TARGET_68k THEN *>
  INCL(at.COMP_MODE, at.GENASM);
<* ELSE *>
  CheckFlag ("GENASM",     at.GENASM);
<* END *>

  IF at.GENASM IN at.COMP_MODE THEN
  <* IF TARGET_RISC THEN *>
    IF at.ABI = at.PowerOpen
    THEN
      DefaultOBJFMT := objASM;
    ELSE
      DefaultOBJFMT := objGAS;
    END;
  <* ELSIF TARGET_SPARC THEN *>
    DefaultOBJFMT := objGAS;
  <* ELSE *>
    DefaultOBJFMT := objASM;
  <* END *>
    env.config.SetEquation(EQU_OBJFMT, DefaultOBJFMT);
  ELSE (* NOT genasm *)
  <* IF TARGET_VAX THEN *>
    DefaultOBJFMT := objVMS;
  <* ELSIF TARGET_RISC THEN *>
    DefaultOBJFMT := objCOFF;
  <* ELSIF TARGET_SPARC THEN *>
    DefaultOBJFMT := objELF;
  <* ELSE *>
    DefaultOBJFMT := objOMF;
  <* END *>
  END;

(* -- turn off optimize traps - it nullifies traps' lineno & filenames
  -- doreorder и nooptimizetraps не должны быть включены одновременно
  IF at.DOREORDER IN at.COMP_MODE THEN
    EXCL(at.COMP_MODE, at.OptimizeTraps);
  ELSE
    CASE OptionSpecified ("NOOPTIMIZETRAPS") OF
      | YES: EXCL(at.COMP_MODE, at.OptimizeTraps);
      | NO : INCL(at.COMP_MODE, at.OptimizeTraps);
    ELSE
      EXCL(at.COMP_MODE, at.OptimizeTraps);
    END;
  END;
*)

  IF NOT (at.TARGET IN at.trg_SET{at.trg_NT, at.trg_OS2, at.trg_LINUX}) THEN
    CheckFlag ("GENEMU87", at.EMU_FPU)
  END;

<* IF pcvis THEN *>      pcvis := env.config.Option("VISUALIZE");   <* END *>

  CheckFlag ("GENCSTRINGS",      at.GenCStrings);
  CheckFlag ("GENCSTRINGSALWAYS",at.GenCStringsAlways);

END get_gencode_options;

CONST
  opts = "VERSIONKEY-"
       + "GENDEBUG-"
       + "LINENO-"
       + OPT_DBGNESTEDPROC+"-"
       + OPT_DBG_QUALIDS+"-"
       + "GENHISTORY-"
       + "ONECODESEG-"
       + "GENFRAME-"
       + "GENPTRINIT+"
       + "DEFLIBS+"
       + "GENEMU87+"
       + "NOPTRALIAS-"
       + "DS_NEQ_SS-"
       + "SPACE-"
       + "DOREORDER-"
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
       + "DOPEEPHOLE+"
<* END *>
       + "GENDLL-"
<* IF TARGET_68k THEN *>
       + "GENASM+"
<* ELSE *>
       + "GENASM-"
<* END *>
<* IF TARGET_386 THEN *>
       + "NOOPTIMIZE-"
       + "USEDLL-"
       + "DLLEXPORT-"
       + "IMPLIB+"
<* END *>

 <* IF TARGET_VAX THEN *>
       + OPT_CODENAMEPREFIXED + "-"
       + OPT_DATAASOBJECT + "-"
 <* END *>

       + "GENCSTRINGS+"
       ;

  equs = "CC;MC;CODENAME;DATANAME;"

 <* IF TARGET_68k     THEN *>  + "CPU=68040;MINCPU=68040;"
 <* ELSIF TARGET_386  THEN *>  + "CPU=GENERIC;MINCPU=386;"

                               + EQU_DBGFMT + ";"+"GENPROF=NONE;"

 <* ELSIF TARGET_RISC THEN *>  + EQU_ABI + "=" + DefaultABI + ";"
 <* ELSIF TARGET_VAX  THEN *>  + EQU_MAINNAME + ";"
 <* END *>

 <* IF ~TARGET_VAX THEN *>     + EQU_OBJFMT + (* "=" + DefaultOBJFMT +*) ";"
 <* END *>                     ;

PROCEDURE InitOptsEqus*;
BEGIN
  env.config.NewOption(OPT_GENCPREF, FALSE, env.gencpref);
  env.OptionList(opts);
  env.EquationList(equs);
END InitOptsEqus;

BEGIN

END Options.
