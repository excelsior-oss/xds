<* NEW db_attrs- *>
MODULE opAttrs;
IMPORT pc  := pcK,  
       pcO,
       ir,
       DStrings,
       fmt := xcStr,
       xfs := xiFiles,
       env := xiEnv,
       xcStr,
 <* IF db_attrs THEN *> io := Printf, <* END *>
       SYSTEM;
 <* IF TARGET_386 THEN *> IMPORT xProfRTS; <* END *>
(* -------------------------------------------------------------------------- *)

CONST CODE_VERSION* =
<* IF TARGET_VAX THEN *>
            "Vax, v1.01";
<* ELSIF TARGET_68K THEN *>
            "68k, v1.03";
<* ELSIF TARGET_RISC THEN *>
            "RISC, v0.99";
<* ELSIF TARGET_SPARC THEN *>
            "SPARC, v0.10";
<* ELSE *>
            "x86, v1.51";
<* END *>

(* -------------------------------------------------------------------------- *)

CONST   (* for bit field implementation *)
  BITS_PER_BYTE*         = 8;
  BITS_PER_VIRTUAL_WORD* = 
                           32;
  BITS_PER_REAL_WORD*    = 32;

  BITS_PER_OLD_VIRTUAL_WORD* = 16; (* for old style bit array packing *)

  BYTES_PER_WORD*        = BITS_PER_REAL_WORD DIV BITS_PER_BYTE;

  MAX_STRUCT_PARAM_SIZE* = 4 * BYTES_PER_WORD;  (* for SL-1 *)

TYPE
    ProcNum *= ir.ProcNum;
CONST
  INVPROCNUM = ProcNum{ -1};
VAR
  curr_proc*   : pc.OBJECT;
  curr_procno* : ProcNum;
  curr_user_procno* : ProcNum;
  curr_mod*    : pc.OBJECT;
  curr_mno*    : pc.Mno;
  main*        : BOOLEAN;


(* ------------- f l o a t i n g  ----------------------------------- *)

VAR UseFloatOps* : BOOLEAN;      (* в коде процедуры есть команды FPU *)
                                 (*   выставляется эмиттером кода     *)

  was_float_triade* : BOOLEAN;   (* были триады с t_float - выст. opE *)

(* ------------- c o m p i l e r   o p t i o n s -------------------- *)
VAR
  heap_lim*  : LONGINT;
  gc_auto*   : BOOLEAN;
  gc_thres*  : LONGINT;
  stk_lim*   : LONGINT;

  NM_SEP*: ARRAY 4 OF CHAR; -- symbols to separate module name from object name
                            -- changed in DEMO and TRIAL versions

                   (* --- code generation modes ------------------- *)
TYPE CompModeType *= (
       convert_while,      (* while... --> if... repeat *)
       stack_checked,      (* to check stack overflow   *)
       use_frame_ptr,      (* use frame pointer         *)
       INIT_PTR,           (* init pointers             *)
       one_return,         (* one return in a procedure *)
       nooptimize,         (* turn off any optimization *)

       copystr_proc,       (* COPY by procedure call *)
       CAP_proc,           (* CAP  by procedure call *)
       TRAP_proc,          (* TRAP by procedure call *)
                           (* --- optimization modes ---------------------- *)
       NOALIAS,            (* no aliases for variables   *)
       SPACE,              (* space/time optimization    *)
       DOREORDER,          (* perform command reordering *)
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
       DOPEEPHOLE,         (* perform pipehole optimization *)
<* END *>
                           (* --- object file modes ----------------------- *)
       new_segment,        (* new code segment for each procedure *)
       lineno,             (* insert linenum info into .obj file *)
       debug,              (* insert debug info                  *)
       history,            (* include history output on error    *)
       EMU_FPU,            (* use FPU emulator                   *)
       DEF_LIBs,           (* include default libraries names in obj-file   *)
       o2_cmds,            (* include information on commands for Oberon-2 modules *)
       GENDLL,             (* generate object file for DLL *)
       USEDLL,             (* use DLL version of standard library *)
       GENASM,             (* generate assembler or binary file *)

       -- AVY: generate debug info with nested procedures;
       -- use at 'DbgFace.ob2' and 'form*.ob2'
       DbgNestedProc ,

       -- AVY: if option turn on, less variables will be made
       -- temporary and more located in memory;
       -- see also be_386\DAG_I.ob2.SetLocation, be_386\DAG.ob2.MakeNode::MakeParam
       NoRegVars,

       -- AVY: see 'be_386\ssa.ob2.MakeFi'
       VolatilePlus,

<* IF DEFINED(OVERDYE) AND OVERDYE THEN *>
       -- AVY: reorder code/source table
       DbgRefine,
<* END *>
       ir_strict
       ,OptimizeTraps
       -- htayod: forces '\' in string literals that are passed as parameters 
       -- to "C" functions to be interpreted by C rules - e.g. \t, \n etc are
       -- recognized.
       -- See opDef.ob2::const_aggregate()
       ,GenCStrings
       ,GenCStringsAlways
    );

    CompModeSet *= PACKEDSET OF CompModeType;

VAR
  COMP_MODE* : CompModeSet;

<* IF TARGET_386 THEN *>
  profilingMode*: xProfRTS.ProfilingModeType;
<* END *>

CONST fastcomp *= FALSE;


(* ------- C P U  -  t y p e ---------- *)

VAR
  CPU*    : SHORTINT;  (* optimize program execution for CPU type (other - possible) *)
  minCPU* : SHORTINT;  (* minimal CPU type where compiled program will work (lower - impossible)*)

CONST
  i386 *        = 1;   (* these values correspond to CONST cpus in Options.ob2 *)
  i486 *        = 2;
  iGeneric *    = 3;
  iPentium *    = 4;
  iPentiumPro * = 5;

  mc68040 *     = 20;
  mc68060 *     = 21;

(* ------- PowerPC Application Binary Interface ---------- *)
<* IF TARGET_RISC THEN *>

VAR
  ABI*    : SHORTINT;  (* type of PowerPC ABI *)

CONST
  PowerOpen * = 1;   (* these values correspond to CONST ABIs in Options.ob2 *)
  v4abi *     = 2;   
  eabi *      = 3;   

<* END *> -- TARGET_RISC

(* ------- C - c o m p i l e r   c o m p a t i b i l i t y ---------- *)

VAR
  CC* : SHORTINT;   (* conventions for C-procedure call      *)
  MC* : SHORTINT;   (* conventions for Modula-procedure call *)

CONST
  NATIVE*        = 1;     (* NATIVE call conventions          *)
  SYMANTEC*      = 2;     (* SYMANTEC compatible obj-file     *)
  WATCOM*        = 3;     (* WATCOM compatible obj-file       *)
  BORLAND*       = 4;     (* BORLAND compatible obj-file      *)
  OS2SYS_CALL*   = 5;     (* OS2 SYS_CALL compatible obj-file *)
  MSVC*          = 6;     (* MS Visual C compatible obj-file  *)
  DJGPP*         = 7;     (* go32 extender compatibility      *)
  GCC*           = 8;     (* GCC (now only for Linux-x86)     *)
 
(* ------- T a r g e t   P l a t f o r m ---------------------------- *)

TYPE trg *= (
       trg_DOS4G,
       trg_FLASHTEK,
       trg_NT,
       trg_OS2,
       trg_LINUX,
       trg_AIX
     );
    trg_SET *= PACKEDSET OF trg;

VAR
  TARGET*: trg;

(** ---------------- B A C K - E N D   T A G s -------------------------- *)

CONST
  otag_declared*   = pc.otag_aux1; (* declared, may be not defined        *)
  otag_declaring*  = pc.otag_aux2; (* declaration in progress             *)
  otag_defining*   = pc.otag_aux3; (* definition in progress              *)
  otag_undef*      = pc.otag_aux4; (* declared but not defined            *)

  otag_versionkey* = pc.otag_aux6; (* version key in module body name *)
  otag_created*    = pc.otag_aux7; (* имеется триадный объект *)

(* значения omark используются только во время одной трансляции *)
(* и в sym-файлы не записываются                                *)

  omark_used*         = pc.omark_aux10; (* объект использован в программе *)
  omark_allocated*    = pc.omark_aux11; (* глобальной переменной приписан offset *)
  omark_nested_read*  = pc.omark_aux12; (* вложенные доступаются к локалам процедуры *)
  omark_nested_write* = pc.omark_aux13; (* вложенные доступаются к локалам процедуры *)
  omark_procdesc*     = pc.omark_aux14; (* объект - дескриптор процедуры *)

  omark_gen_FIRST*  = pc.omark_aux16;                   (* пометки формирования кода: *)
  omark_gen_ready*  = omark_gen_FIRST ;  (*   есть сгенерированный код *)
  omark_gen_marked* = SYSTEM.SUCC(omark_gen_FIRST);  (*   приписан адрес в коде    *)

  omark_gen_usetcf* = SYSTEM.SUCC(omark_gen_marked); (* found some tcfs in the procedure *)
(* значения ttag = 8-11,16 используются в sym-файлах конвертора - см. ccK.ob2 *)

--  ttag_intrinsic*  = pc.ttag_aux14; (* встроенная фун-ия - реализуется сопроцессором *)
                         (* value of the tag must be the same as in SL2.ob2 *)

(* значения tmark используются только во время одной трансляции *)
(* и в sym-файлы не записываются                                *)

  tmark_processed*  = pc.tmark_aux10; (* тип обработан   *)
  tmark_prototyped* = pc.tmark_aux11; (* создан прототип *)
  tmark_db_index*   = pc.tmark_aux12; (* has index number for debug info *)

(** ----------------- A L I G N M E N T --------------------------------- *)

VAR default_alignment* : SHORTINT;

(** ----------------- A T T R I B U T E S ------------------------------- *)

CONST  (* -- в и д ы   а т р и б у т о в -- *)
   a_self*        = 1;   (* тэг и номер объекта *)                   (* inf_ext  *)
   a_size*        = 2;   (* размер типа *)                           (* size_ext *)
   a_prot*        = 3;   (* номер прототипа *)                       (* prot_ext *)
   a_type*        = 4;   (* переменная-тип VAR-параметра *)          (* inf_ext  *)
   a_rtn*         = 5;   (* возвратный параметр *)                   (* inf_ext  *)
   a_desc*        = 6;   (* дескриптор типа (указатель на ??) *)     (* inf_ext  *)
   a_re*          = 7;
   a_im*          = 8;
   a_locOFFS*     = 9;   (* объем размещенных локалов *)             (* size_ext *)
   a_globOFFS*    = 10;  (* размещение глобальной переменной *)      (* size_ext *)
   a_name*        = 11;  (* object's external name *)                (* name_ext *)
   a_TOCoffs*     = 12;  (* offset in TOC *)                         (* TOC_ext  *)
   a_adr_glob*    = 13;  (* variable - address of SL-1 global *) (* inf_ext - use only 'name' *)

   a_mybase*      = 16;  (* база переменных процедуры *)             (* inf_ext  *)
   a_base*        = a_mybase+1; (*!!*)
                         (* a_base+i  --  база i-го охват. блока *)  (* inf_ext  *)
   a_len*         = 48;
                         (* a_len+i  -- длина по i-ому измерению *)  (* inf_ext  *)

   a_index*       = 80;  (* index in debug information *)            (* size_ext *)
   a_index2*      = 81;  (* additional index in debug information *) (* size_ext *)
   a_index_ref*   = 82;  (* reference index in debug information *)  (* size_ext *)
   a_dbg*         = 83;  (* debug info *)                            (* dbg_ext *)

   a_dllexported  = 84;  (* the object is DLL-exported *)            (* attr_ext *)
   a_stdcallmangle= 85;  (* the object's name is mangled *)          (* attr_ext *)
   a_gen_code*    = 86;  (* этот и все следующие - для формирования кода *)

TYPE
  ATTR_EXT* = POINTER TO attr_ext_rec;

  attr_ext_rec* = RECORD(pc.bext_rec)
    next* : ATTR_EXT;
    kind* : SHORTINT;   (* тип атрибута *)
  END;

PROCEDURE (att: ATTR_EXT) out*(file: xfs.SymFile);
BEGIN
  IF att.next = NIL THEN
    file.WriteInt(0);
  ELSE
    IF (att.kind = a_stdcallmangle) THEN file.WriteInt(a_stdcallmangle);
    ELSIF (att.kind = a_dllexported) THEN file.WriteInt(a_dllexported) END;
    att.next.out(file);
  END;
END out;

TYPE
  INFO_EXT* = POINTER TO inf_ext_rec;     (* для объектов *)
  OFFS_EXT* = POINTER TO offs_ext_rec;    (* для глобальных переменных *)
  SIZE_EXT* = POINTER TO size_ext_rec;    (* для структурных типов *)
  PROT_EXT* = POINTER TO proto_ext_rec;   (* для процедурных типов *)
  DBG_EXT*  = POINTER TO dbg_ext_rec;     (* для объектов, прежде всего переменных *) 

  InfExtName *= ir.VarNum;
CONST
  ZEROInfExtName *= InfExtName{ 0 };
TYPE
  inf_ext_rec = RECORD(attr_ext_rec)
    procno*: ProcNum;    (* номер процедуры, при обработке которой построен *)
    e_tag*:  ir.TagType;   (* ir.TagType [+ BASE] *)
    name* :  InfExtName;    (* номер локала, переменной или процедуры *)
    value*:  pc.VALUE;
    offs* :  LONGINT;    (* вообще-то было бы хорошо иметь не конкретное число,
                           а задать его через имя переменной *)
  END;

  size_ext_rec = RECORD(attr_ext_rec)
    size*: LONGINT;
  END;

  proto_ext_rec = RECORD(attr_ext_rec)
    proto*    : ir.ProtoNum;
  END;

  offs_ext_rec = RECORD(attr_ext_rec)
    offset*: LONGINT;
  END;

  dbg_ext_rec = RECORD(attr_ext_rec) 
    list* : POINTER TO 
               ARRAY OF 
                  RECORD 
                     what*: SHORTINT;        -- one of a_* constants
                     info*: ir.DebugInfo;    -- displacement
                  END; 
  END;

PROCEDURE (att: OFFS_EXT) out*(file: xfs.SymFile);
BEGIN
  file.WriteInt(a_globOFFS);
  file.WriteInt(att.offset);
  att.out^(file);
END out;

(* for C-converter compatibility *)
CONST (* -- номера имен для самого объекта и его производных объектов *)
  n_object_name    * = 0;
  n_type_desc_name * = 2;      (*  obj.mode = ob_type      *)
  n_BEGIN_name     * = 2;      (*  obj.mode = ob_module    *)

TYPE
  NAME_INFO = POINTER TO name_info_rec;

  name_info_rec = RECORD
    nxt* : NAME_INFO;
    name*: pc.STRING;
    no*  : INTEGER;
  END;

  NAME_EXT* = POINTER TO name_ext_rec;    (* список внешних имен *)

  name_ext_rec = RECORD(attr_ext_rec)
    list*: NAME_INFO;
  END;

(* -- *)

CONST BASE* = ir.TagType{100};

PROCEDURE must_have_fixed_address*(v: pc.OBJECT): BOOLEAN;
BEGIN
--  IF ~(pc.otag_no_threat IN v.tags)      THEN RETURN TRUE END;
--  IF ~(pc.omark_used_by_code IN v.marks) THEN RETURN TRUE END;
  RETURN (v.marks *
       pc.OMARK_SET{pc.omark_used_by_code,
        omark_nested_read,
        omark_nested_write}) # pc.OMARK_SET{};
END must_have_fixed_address;

PROCEDURE attr*(bex: pc.BEXT; kind: SHORTINT): ATTR_EXT;
  VAR att: ATTR_EXT;
BEGIN
<* IF db_attrs THEN *>
--  io.printf("attr(bex=%x, kind=%d)\n", bex, kind);
<* END *>
  IF bex = NIL THEN RETURN NIL END;
  att := bex(ATTR_EXT);
  WHILE (att # NIL) & (att.kind # kind) DO att := att.next END;
  RETURN att;
END attr;

PROCEDURE app_attr(VAR bex: pc.BEXT; att: ATTR_EXT; kind: SHORTINT);
  VAR a: ATTR_EXT;
BEGIN
  att.kind := kind;
  IF bex # NIL THEN
    a := bex(ATTR_EXT);
    WHILE a # NIL DO ASSERT(a.kind#kind); a := a.next END;
    att.next := bex(ATTR_EXT);
  ELSE att.next := NIL;
  END;
  bex := att;
END app_attr;

PROCEDURE app_struct_attr*(str: pc.STRUCT; att: ATTR_EXT; kind: SHORTINT);
BEGIN
  app_attr(str.ext, att, kind);
END app_struct_attr;

PROCEDURE app_obj_attr*(o: pc.OBJECT; att: ATTR_EXT; kind: SHORTINT);
  VAR nm: NAME_EXT;
BEGIN
  IF (o.ext = NIL) & (kind # a_name) THEN
    NEW(nm);
    nm.kind := a_name; nm.list := NIL; nm.next := NIL;
    o.ext := nm;
  END;
  app_attr(o.ext, att, kind);
END app_obj_attr;

PROCEDURE del_attr*(VAR bex: pc.BEXT; kind: SHORTINT);
  VAR a0, a: ATTR_EXT;
BEGIN
  IF bex # NIL THEN
    a0 := NIL;
    a := bex(ATTR_EXT);
    LOOP
      IF a.kind = kind THEN
        IF a0 = NIL THEN bex := a.next ELSE a0.next := a.next END;
        EXIT
      END;
      a0 :=a; a := a.next;
      IF a = NIL THEN EXIT END;
    END;
  END;
END del_attr;

PROCEDURE app_info*(o: pc.OBJECT;
                akind: SHORTINT;
                e_tag: ir.TagType;
                name:  InfExtName;
                value: pc.VALUE;
                offs:  LONGINT);
  VAR inf: INFO_EXT;
BEGIN
<* IF db_attrs THEN *>
  io.printf("app_info('%s', kind=%d, e_tag=%d, name=%d, offs=%d",
                  o.name^, akind,   e_tag,    name,    offs);
<* END *>
  NEW(inf);
  inf.procno := curr_procno;
  inf.e_tag := e_tag;
  inf.name  := name;
  inf.value := value;
  inf.offs  := offs;
  app_obj_attr(o, inf, akind);
<* IF db_attrs THEN *>
  io.printf(")\n");
<* END *>
END app_info;


(** ----------------------------- SYM_FILE --------------------------------- *)

(* these procedures (object names writing and reading)       *)
(* for C-converter compatibility                             *)

PROCEDURE (i: NAME_EXT) out*(f: xfs.SymFile);
  VAR j: NAME_INFO; n: LONGINT;
BEGIN
  f.WriteInt(a_name);
  j := i.list; n := 0;
  WHILE j # NIL DO INC(n); j := j.nxt; END;
  f.WriteInt(n);
  j := i.list;
  WHILE j # NIL DO
    f.WriteString(j.name^); f.WriteInt(j.no); j := j.nxt;
  END;
  i.out^(f);
END out;


(**---- Names ----*)

PROCEDURE append_name* (o: pc.OBJECT; no: SHORTINT; name-: ARRAY OF CHAR);
VAR
  a  :ATTR_EXT;
  nm :NAME_EXT;
  i  :NAME_INFO;
BEGIN
  NEW(i);
  DStrings.Assign(name, i.name);
  i.no := no;
  a := attr(o.ext, a_name);
  IF a = NIL THEN
    NEW(nm);
    nm.list := i;
    app_attr(o.ext, nm, a_name);  (* использовать app_obj_attr нельзя !!! *)
  ELSE
    nm := a(NAME_EXT);
    i.nxt := nm.list;
    nm.list := i;
  END;
END append_name;

PROCEDURE get_name*(o: pc.OBJECT; no: INTEGER) : pc.STRING;
  VAR a: ATTR_EXT; nm: NAME_EXT;
    i: NAME_INFO;
BEGIN
  a := attr(o.ext, a_name);
  IF a = NIL THEN RETURN NIL END;
  nm := a(NAME_EXT);
  i := nm.list;
  LOOP
    IF i = NIL THEN RETURN NIL END;
    IF i.no = no THEN RETURN i.name END;
    i := i.nxt;
  END;
END get_name;

PROCEDURE read_names (f: xfs.SymFile; o: pc.OBJECT);
  VAR n,l: LONGINT; bf: ARRAY 256 OF CHAR;
BEGIN
  f.ReadInt(n);  -- io.printf("inp_object('%s') n=%d\n", o.name^, n);
  WHILE (n > 0) DO
    f.ReadString(bf);
    f.ReadInt(l);
    append_name(o, VAL(SHORTINT, l), bf);  --io.printf("    '%s' l=%d\n", bf, l);
    DEC(n);
  END;
END read_names;

PROCEDURE skip_names(f: xfs.SymFile);
  VAR n,l: LONGINT; bf: ARRAY 256 OF CHAR;
BEGIN
  f.ReadInt(n);
  WHILE n>0 DO
    f.ReadString(bf);
    f.ReadInt(l);
    DEC(n);
  END;
END skip_names;

(* --------  B i t   F i e l d   I n f o  --------- *)

PROCEDURE bit_field_info * (f: pc.OBJECT;
              VAR offset     : LONGINT;
              VAR bit_offset : SHORTINT;
              VAR bit_width  : SHORTINT);
  VAR wofs, bofs, bwidth :pc.VALUE;
BEGIN
  ASSERT(f.mode = pc.ob_field_bts);

  bwidth := f.attr(pc.NODE).val;
  wofs   := f.attr(pc.NODE).l.val;
  bofs   := f.attr(pc.NODE).l.l.val;

  offset      := BYTES_PER_WORD * wofs.get_integer();
  bit_offset  := VAL (SHORTINT, bofs.get_integer());
  bit_width   := VAL (SHORTINT, bwidth.get_integer());
END bit_field_info;

(* --------  G l o b a l   V a r i a b l e s   A l l o c a t i o n  --------- *)

PROCEDURE set_global_offset*(o: pc.OBJECT; offs: LONGINT);
  VAR ofs: OFFS_EXT;
BEGIN
  ASSERT(((o.mode = pc.ob_var)OR(o.mode = pc.ob_cons))
           & (o.lev = 0) & ~(omark_allocated IN o.marks));
  NEW(ofs);
  ofs.offset := offs;
  app_attr(o.ext, ofs, a_globOFFS);
  INCL(o.marks, omark_allocated);
END set_global_offset;

PROCEDURE get_global_offset*(o: pc.OBJECT): LONGINT;
  VAR att: ATTR_EXT;
BEGIN
  ASSERT(((o.mode = pc.ob_var)OR(o.mode = pc.ob_cons))
           & (o.lev = 0) & (omark_allocated IN o.marks));
  att := attr(o.ext, a_globOFFS);
  RETURN att(OFFS_EXT).offset
END get_global_offset;

PROCEDURE read_globOFFS(f: xfs.SymFile; o: pc.OBJECT);
  VAR offs: LONGINT;
BEGIN
  f.ReadInt(offs);
  set_global_offset(o, offs);
END read_globOFFS;

PROCEDURE skip_globOFFS(f: xfs.SymFile);
  VAR offset: LONGINT;
BEGIN
  f.ReadInt(offset);
END skip_globOFFS;

(**---- DLL-exported Flag ----*)

PROCEDURE get_dllexported* ( o: pc.OBJECT ) :BOOLEAN;
BEGIN
  RETURN ( attr(o.ext, a_dllexported ) # NIL );
END get_dllexported;

PROCEDURE set_dllexported (o: pc.OBJECT );
VAR atr :ATTR_EXT;
BEGIN
  NEW(atr);
  app_obj_attr(o, atr, a_dllexported);
END set_dllexported;


(**---- StdCall-mangled Flag ----*)

PROCEDURE is_stdcall_mangled* ( o: pc.OBJECT ) :BOOLEAN;
BEGIN
  RETURN ( attr(o.ext, a_stdcallmangle ) # NIL );
END is_stdcall_mangled;

PROCEDURE set_stdcallmangle (o: pc.OBJECT );
VAR atr :ATTR_EXT;
BEGIN
  NEW(atr);
  app_obj_attr(o, atr, a_stdcallmangle);
END set_stdcallmangle;


(**-------- Object operations --------*)


PROCEDURE getPrjDllName ( VAR s :pc.STRING ); (* it needs to redo *)
BEGIN
  env.config.Equation("dllname",s);
  IF ( s = NIL ) OR ( s[0] = 0X ) THEN
    env.config.Equation("project",s);
  END;
END getPrjDllName;

PROCEDURE isSameDLL* (o: pc.OBJECT) :BOOLEAN;
  VAR
    prjDllName, dllName :pc.STRING;
BEGIN
  IF ~(USEDLL IN COMP_MODE) OR env.config.Option("__LIBSYMS__") THEN
    RETURN TRUE
  END;
  (* 1st case :

     USEDLL- means that DLLs aren't used at all, so compiler has to make
     no difference in the SYM-file processing; otherwise one should
     handle with two versions of SYMs that is very unconvinient

     2nd case (* M2/O2 only *):

     when LIBSYMS+, a sym has no dllname written to use the same syms
     both for single- & multithread libraries, thus absence of dllname
     (dllname = NIL) is always treated as another DLL
  *)
  dllName := get_name( o, pc.dllname_id );
  getPrjDllName (prjDllName);

    RETURN (dllName # NIL) & (prjDllName # NIL) & (prjDllName^=dllName^);
END isSameDLL;


PROCEDURE out_object* (f: xfs.SymFile; o: pc.OBJECT );
  VAR
    s    :pc.STRING;
    isJT :BOOLEAN;

  PROCEDURE ignored(o: pc.OBJECT): BOOLEAN;
    CONST
     IGNOREs = pc.OB_SET{pc.ob_cproc, pc.ob_eproc};

     IGNORED_CONSTs = - (pc.COMPOUNDs + pc.REALs + pc.CPLXs);
     (* Types of ignored constants (all except structures + floatings) *)
  BEGIN
    RETURN (o^.mode IN IGNOREs) OR
           ((o.mode = pc.ob_module) & (o # curr_mod)) OR
           (o^.mode=pc.ob_cons) & (o^.type#NIL) & (o^.type^.mode IN IGNORED_CONSTs);
  END ignored;
BEGIN

  isJT := (o.mode = pc.ob_type) & ( o.mno = curr_mno );

  IF isJT OR                           -- Java types is always exported from EXE & DLL
    env.config.Option("GENDLL") &
    ( (o.mode = pc.ob_module) & ( o.mno = curr_mno ) OR              -- BEGIN-part is always exported from DLL
        env.config.OptionAt(o.pos,"DLLEXPORT") & ~ignored(o) ) THEN

    IF NOT get_dllexported(o) THEN (* structure constants are written into all SYM - feature?? *)
        (* DLL's name is always needed to generate direct access to var
           from modules of the same DLL, except RT Lib where the same SYMs
           are used for (static OR dinamic) & ( single- OR multithread )
           library
        *)
        IF env.config.Option("__LIBSYMS__") THEN
          set_dllexported(o);
        ELSIF env.config.Option("GENDLL") THEN
          getPrjDllName(s); ASSERT(s # NIL);
          append_name ( o, pc.dllname_id, s^ );
          set_dllexported(o);
        END;
    END;
  END;
  IF env.config.OptionAt(o.pos,"GEN_STDCALL_MANGLE") THEN
    set_stdcallmangle(o);
  END;
  IF ( o.ext = NIL ) THEN (* !!!!!!!!!!!!! redo *)
    f.WriteInt(0);        (* write "end of sequence", because sym_info has already written into SYM by *)
  ELSE                    (* pcO.out_object ( it's slightly messy but what else?)  -- VitVit           *)
    o.ext.out(f);
  END;
END out_object;


PROCEDURE clear_object*(o :pc.OBJECT);
VAR
  cur   :ATTR_EXT;
  dllx, mngl :ATTR_EXT;
  names :NAME_EXT;
  dlln  :NAME_INFO;
BEGIN
  IF (o.ext = NIL) THEN RETURN END;
  dllx  := NIL;
  mngl  := NIL;
  names := NIL;
  cur   := o.ext(ATTR_EXT);
 
  (* find dllexported attrib & dllname *)
  WHILE (cur # NIL) DO
    WITH
      cur :NAME_EXT DO
        ASSERT (names = NIL);
        names := cur;
    ELSE
      IF (cur.kind = a_dllexported) THEN
         ASSERT (dllx = NIL);
         dllx := cur;
      ELSIF (cur.kind = a_stdcallmangle) THEN
         ASSERT (mngl = NIL);
         mngl := cur;
      END;
    END;
    cur  := cur.next; 
  END;

  IF (names # NIL) THEN
    dlln := names.list;
    WHILE (dlln # NIL) & (dlln.no # pc.dllname_id) DO
      dlln := dlln.nxt;
    END;
    IF (dlln = NIL) THEN
      (* remove entire name bundle *)
      names := NIL
    ELSE
      (* remain dllname string only *)
       names.list := dlln;
       dlln.nxt := NIL;
    END;
  END;

  IF (dllx # NIL) THEN dllx.next := NIL END;
  
  IF (names # NIL) THEN
    o.ext      := names;
    names.next := dllx;
  ELSE
    o.ext := dllx;
  END;
  IF mngl # NIL THEN
    mngl.next := o.ext(ATTR_EXT);
    o.ext := mngl;
  END;
END clear_object;


PROCEDURE inp_object* (f: xfs.SymFile; o: pc.OBJECT; id: LONGINT);
  VAR n: LONGINT;
BEGIN
  IF    id = pc.sym_C      THEN read_names(f, o);
  ELSIF id = pc.sym_native THEN (* nothing *)
  ELSE ASSERT(id = pc.sym_native+1);
    LOOP
      f.ReadInt(n);
      IF n = 0 THEN EXIT
      ELSIF n = a_name     THEN read_names(f, o);
      ELSIF n = a_globOFFS THEN read_globOFFS(f, o);
      ELSIF n = a_dllexported THEN set_dllexported(o);
      ELSIF n = a_stdcallmangle THEN set_stdcallmangle(o);
      ELSE ASSERT(FALSE)
      END;
    END;
  END;
END inp_object;

PROCEDURE skip_object* (f: xfs.SymFile; id: LONGINT);
  VAR n: LONGINT;
BEGIN
  IF    id = pc.sym_C        THEN skip_names(f);
  ELSIF id = pc.sym_native   THEN (* nothing *)
  ELSIF id = pc.sym_native+1 THEN
    LOOP
      f.ReadInt(n);
      IF n = 0 THEN EXIT
      ELSIF n = a_name THEN skip_names(f);
      ELSIF n = a_globOFFS THEN skip_globOFFS(f);
      ELSIF n = a_dllexported THEN ;
      ELSIF n = a_stdcallmangle THEN ;
      ELSE ASSERT(FALSE)
      END;
    END;
  ELSE ASSERT(FALSE);
  END;
END skip_object;

(** ------------------------------------------------------------------------ *)

(* для переменной или параметра выдает локал,     *)
(* к которому этот объект "привязан"              *)

PROCEDURE loc_by_obj*(o: pc.OBJECT): ir.Local;
  VAR a: ATTR_EXT;
    inf: INFO_EXT;
BEGIN
  ASSERT(o.mode IN pc.VARs);
  a := attr(o.ext, a_self);
  IF a = NIL THEN RETURN ir.UNDEFINED END;
  inf := a(INFO_EXT);
  RETURN VAL(ir.Local, inf.name)
END loc_by_obj;

PROCEDURE GetStartTPos*(p: pc.OBJECT; VAR pos: ir.TPOS);
  VAR n: pc.NODE;
BEGIN
  n := p.val;
  IF n # NIL THEN pos := n.pos;
  ELSE            pos := p.pos
  END;
END GetStartTPos;

(** --------- N A M E S   &   W O R K   O B J E C T s ------------------- *)

VAR work_objects* : pc.OBJECT;            (* список рабочих объектов *)

<* IF TARGET_RISC OR TARGET_SPARC THEN *>
VAR
  alloc_work_object*: PROCEDURE (o: pc.OBJECT);
  GlobTOC*    : pc.OBJECT;   -- object to represent global TOC
  LocalTOC*   : pc.OBJECT;   -- object to represent local TOC
<* END *>

PROCEDURE make_name*(frmt-: ARRAY OF CHAR; SEQ arg: SYSTEM.BYTE): pc.STRING;
  VAR nm: ARRAY pc.name_size OF CHAR;
    name: pc.STRING;
BEGIN
  fmt.prn_txt(nm, frmt, arg);
  DStrings.Assign(nm, name);
  RETURN name
END make_name;

PROCEDURE new_work_object*(
                name: pc.STRING;
                t,h : pc.STRUCT;
                mode: pc.OB_MODE;
                prof: BOOLEAN): pc.OBJECT;
  VAR o: pc.OBJECT;
BEGIN
  NEW(o);
  o.mode := mode;
  IF h # NIL THEN
    o.lev := h.obj.lev;
    IF h.obj.mode IN pc.PROCs THEN INC(o.lev) END;
    o.mno := h.mno;
  ELSE
    o.lev := 0;
    o.mno := pc.ZEROMno;
  END;
  o.type := t;
  o.sno:=-1;
  o.tags:=pc.OTAG_SET{};
  o.pos:=env.null_pos;
  o.end:=env.null_pos;
  IF (name = NIL) OR (LENGTH(name^)=0) THEN
    o.name := make_name("$$%.8X",o);  (* object's address in hex *)
  ELSE
    o.name := name;
  END;
  o.host := h;
  IF prof THEN (* ?? *)
    IF h.mode=pc.ty_module THEN INCL(o.tags,pc.otag_public) END;
  END;
  o.next := work_objects;
  work_objects := o;
  RETURN o;
END new_work_object;

PROCEDURE T_type (ty: ir.TypeType; sz: ir.SizeType): pc.STRUCT;
BEGIN
  CASE ty OF
  | ir.t_int:
       CASE sz OF
       | 1: RETURN pcO.shortint;
       | 2: RETURN pcO.integer;
       | 4: RETURN pcO.longint;
       | 8: RETURN pcO.longlongint;
       ELSE
       END;
  | ir.t_unsign:
       CASE sz OF
       | 1: RETURN pcO.shortcard;
       | 2: RETURN pcO.cardinal;
       | 4: RETURN pcO.longcard;
       ELSE
       END;
  |ir.t_float:
       CASE sz OF
       | 4: RETURN pcO.real;
       | 8: RETURN pcO.longreal;
       ELSE RETURN pcO.ld_real;
       END;
  ELSE
  END;
  env.errors.Fault(ir.NullPos, 960);   --- invalid constant value
  RETURN pcO.longint
END T_type;

TYPE
  float_cnst = POINTER TO float_cnst_rec;
  float_cnst_rec = RECORD                  (* это дело следует как-то      *)
                     ty: ir.TypeType;      (* унифицировать с def.TEMP_VAR *)
                     sz: ir.SizeType;
                    val : pc.VALUE;        (* ... или с work_objects через *)
                    obj : pc.OBJECT;       (*      obj.val := val,         *)
                    next: float_cnst;      (* но где тогда взять ty и sz   *)
                   END;
VAR
  floats    : float_cnst;
  floats_no : LONGINT;

PROCEDURE new_const*(v: pc.VALUE; ty: ir.TypeType; sz: ir.SizeType): pc.OBJECT;
  VAR nm : pc.STRING;
    o: pc.OBJECT;
    f: float_cnst;
BEGIN
  f := floats;
--  io.printf("new_const( %x)", v);
  LOOP
    IF f = NIL THEN EXIT END;
--    io.printf("ty = %d; sz = %d; f.ty = %d; f.sz = %d\n",ty, sz, f.ty, f.sz);
    IF (f.ty = ty)&(f.sz = sz)&ir.EQ(f.val, v, ty, sz, TRUE) THEN RETURN f.obj END;
    f := f.next;
  END;
  NEW(f);
  f.ty := ty;
  f.sz := sz;
  f.val := v;
  nm := make_name("R'%d", floats_no);
  o := new_work_object(nm, T_type(ty, sz), curr_mod.type, pc.ob_cons, FALSE);
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  alloc_work_object(o);
<* END *>
  INC(floats_no);
  f.obj := o;
  f.next := floats;
  floats := f;
  RETURN o
END new_const;

VAR
  incl_table*:   pc.OBJECT;
  excl_table*:   pc.OBJECT;
  incl_table_hi*:   pc.OBJECT;
  excl_table_hi*:   pc.OBJECT;
  loset_table*:  pc.OBJECT;
  hiset_table*:  pc.OBJECT;

PROCEDURE setop_table*(op: ir.Operation): pc.OBJECT;
  VAR o: pc.OBJECT;
BEGIN
  CASE op OF
  | ir.o_incl:  o := incl_table;
  | ir.o_excl:  o := excl_table;
  | ir.o_loset: o := loset_table;
  | ir.o_hiset: o := hiset_table;
  END;
  RETURN o
END setop_table;

PROCEDURE setop_table_hi*(op: ir.Operation): pc.OBJECT;
  VAR o: pc.OBJECT;
BEGIN
  CASE op OF
  | ir.o_incl:  o := incl_table_hi;
  | ir.o_excl:  o := excl_table_hi;
  END;
  RETURN o
END setop_table_hi;

<* IF TARGET_RISC OR TARGET_SPARC THEN *>

VAR
  case_table_no : LONGINT;

PROCEDURE new_case_table * (size: LONGINT): pc.OBJECT;  -- size in bytes
  VAR o: pc.OBJECT;
     nm: pc.STRING;
BEGIN
  nm := make_name("CS.%d", case_table_no);
  o := new_work_object(nm, NIL, curr_mod.type, pc.ob_cons, FALSE);
  INC(case_table_no);
  RETURN o
END new_case_table;
<* END *>

<* IF TARGET_RISC THEN *>

VAR FindProcByDesc* : PROCEDURE (dsc: pc.OBJECT) : pc.OBJECT;

<* END *>

PROCEDURE Ini*;
  VAR val : pc.STRING;  i : LONGINT;
BEGIN
  work_objects := NIL;
  curr_proc := NIL;
  curr_mod := NIL;
  curr_procno := INVPROCNUM;
  curr_user_procno := INVPROCNUM;
  curr_mno := pc.INVMno;
  incl_table  := NIL;
  excl_table  := NIL;
  incl_table_hi  := NIL;
  excl_table_hi  := NIL;
  loset_table := NIL;
  hiset_table := NIL;
  floats := NIL;
  floats_no := 0;
  env.config.Equation("ALIGNMENT", val);
  IF (val = NIL) OR NOT(xcStr.StrToInt(val^,i) & (i IN {1,2,4,8,16})) THEN
    env.errors.Fault(env.null_pos,450,"Invalid ALIGNMENT value specified");
  END;
  default_alignment := VAL(SHORTINT, i);
<* IF TARGET_RISC OR TARGET_SPARC  THEN *>
  case_table_no := 0;
  GlobTOC := NIL;
  LocalTOC := NIL;
<* END *>
END Ini;

PROCEDURE Exi*;
BEGIN
  incl_table := NIL;
  excl_table := NIL;
  incl_table_hi  := NIL;
  excl_table_hi  := NIL;
  loset_table := NIL;
  hiset_table := NIL;
  work_objects := NIL;
  curr_proc := NIL;
  curr_mod := NIL;
  curr_procno := INVPROCNUM;
  curr_user_procno := INVPROCNUM;
  curr_mno := pc.INVMno;
  floats := NIL;
  floats_no := 0;
<* IF TARGET_RISC OR TARGET_SPARC THEN *>
  case_table_no := 0;
  GlobTOC := NIL;
  LocalTOC := NIL;
<* END *>
END Exi;

BEGIN
  UseFloatOps := FALSE;
  was_float_triade := FALSE;
  TARGET := trg_FLASHTEK;
<* IF TARGET_68k THEN *>
  CPU    := mc68040;
  minCPU := mc68040;
<* ELSE *>
  CPU    := iGeneric;
  minCPU := i386;
<* END *>
  NM_SEP[0] := '_';
  NM_SEP[1] := 0C;
END opAttrs.
