(** Copyright (c) 1993,97 XDS Ltd, Russia. All Rights Reserved. *)
(** o2/m2 development system v2.0: Make facility *)
<*+ O2EXTENSIONS *>
<* IF ~ DEFINED(COMPONENT_TESTCOVERAGE) THEN *> <* NEW COMPONENT_TESTCOVERAGE- *> <* END *>
MODULE xcMake; (* Ned 24-Feb-94. *)
               (* Sem 10-Oct-95. *)

(** Sketch of usage:

  VAR p: Project; n: Node;

  p.SetEquations;
  LOOP p.Append(name) END;
  p.Regulate(n);
  WHILE n#NIL DO
    IF n.mod.IsCompilable(all) THEN
      Compile; n.mod.SetResult()
    END;
  END;
  p.SetFileNames;
*)

IMPORT
  SYSTEM,
  pc:=pcK,
  xfs:=xiFiles,
  env:=xiEnv,
  sts:=Strings,
  xcStr,
  xmm:=xmRTS,
  DStrings;
<* IF COMPONENT_TESTCOVERAGE THEN *>
IMPORT tcMain;
<* END *>

CONST
(** modes *)
  md_def*     = 3; (* Modula-2 definition module *)
  md_mod*     = 4; (* Modula-2 implementation module *)
  md_main*    = 5; (* Modula-2 main module *)
  md_oberon*  = 6; (* Oberon-2 module *)
  md_code*    = 7; (* C or C++ module *)
  md_header*  = 8; (* C or C++ header file *)
  md_obj*     = 9; (* object file *)
  md_asm*     =10; (* assembler module *)
  md_sym*     =11; (* sym-file *)
  md_sl1*     =12; (* SL1 module *)
  md_emf*     =13; (* emf-file used in InterView *)
  md_unknown  =14;


  (* IMPs - can produce object file *)
  IMPs* = {md_mod,md_main,md_oberon,md_code,md_obj,md_asm,md_sl1};

  (* DEFs - can produce symbol file *)
  DEFs* = {md_def,md_oberon};

  (* CMLs - can be compiled *)
  CMLs  = {md_def,md_mod,md_main,md_oberon,md_sl1};

  (* DEPs - can be parsed to get import list *)
  DEPs  = {md_def,md_mod,md_main,md_oberon};

  (* FNAMs - file name includes extension of source file and its own extension
     example : xcMake.ob2.emf *)
  FNAMs* = {md_emf};
CONST
(** tags *)
  userdef*  = 1; (** file name is user defined *)
  noheader* = 2; (** def without header *)
  nocode*   = 3; (** def without implementation *)
  cstdlib*  = 4; (** def for C standard library *)
  obmain*   = 5; (** main oberon module *)

  parsed    = 6; (* the module was parsed *)
  error     = 7; (* erroneous module *)
  ordered   = 8; (* inserted in the ordered list *)
  tied      = 9;
  tag_out   = 10;

CONST
  err_undefined_extension = 406;
  err_module_not_found    = 407;
  err_two_sources         = 423;
  err_def_expected        = 408;
  err_main_expected       = 409;
  err_oberon_expected     = 410;
  err_imp_or_main_expected= 411;
  err_imp_expected        = 444;
  err_modules_conflict    = 426;
  err_in_header           = 427;
  err_wrong_module_name   = 412;
  err_wrong_mname_import  = 417;
  err_cycle_in_import     = 436; (* %s %s *)
  err_open_error          = 425;

  reason_was_modified     = 404;
  reason_not_found        = 402;
  reason_was_modified_after = 405;
  reason_x_after_y        = 403; (* %s %s %s *)

  msg_no_modules          = 413; (* *)
  msg_alien_def           = 400; (* %s *)
  msg_alien               = 401; (* %s *)

TYPE
  String = xfs.String;

  TM = RECORD
    undef  : BOOLEAN;
    exist  : BOOLEAN;
    time   : xfs.Time;
  END;

  Node*    = POINTER TO NodeDesc;
  PreUnit  = POINTER TO PreUnitDesc;
  ScanUnit = POINTER TO ScanUnitDesc;
  Unit*    = POINTER TO UnitDesc;
  File*    = POINTER TO FileDesc;
  Project* = POINTER TO ProjectDesc;

  NodeDesc* = RECORD
    mod -  : File;
    next-  : Node;
  END;

  PreUnitDesc = RECORD (pc.unit_rec)
    name   : pc.NAME;
    fname  : xfs.String;
  END;

  ScanUnitDesc = RECORD (PreUnitDesc)
    mode  : INTEGER;
    imp   : BOOLEAN; (* only imp expected *)
    tags  : SET;
    inp   : Node;
    pro   : Project;
  END;

  UnitDesc* = RECORD (pc.unit_rec)
    name*  : pc.NAME;
    l-,r-  : Unit; (* Binary tree *)
    s-     : Unit; (* Synonyms *)
    u-     : Unit; (* First synonym *)
  END;

  FileDesc* = RECORD (UnitDesc)
    mode-  : INTEGER;
    fname- : pc.STRING; (* file name   *)
    inp-   : Node;
    out-   : Node;      (** out#NIL not only for XDS compiled modules! *)
    import-: Node;      (** import list *)
    next-  : File;      (** in the module list *)
    from*  : File;      (** depend on this file *)
    tags-  : SET;
    time   : TM;        (** not valid for output files *)
    pro    : Project;
  END;

  Ext = POINTER TO ExtDesc;
  ExtDesc = RECORD
    mode: INTEGER;
    lang: pc.Lang;
    name: String;
    next: Ext;
  END;

  ProjectDesc* = RECORD
    list-    : File;    (** list of all project files *)
    tree-    : Unit;
    nodes-   : Node;    (** ordered module list (after Regulate) *)
    errs*    : LONGINT; (** number of errors *)
    comp*    : LONGINT; (** number of compiled modules *)
    fname*   : String;  (** project name or NIL *)

    exts     : Ext;
    defExt   : String;
    modExt   : String;
    obeExt   : String;

    headers  : BOOLEAN;
    objects  : BOOLEAN;
    aliens   : BOOLEAN;
    verbose  : BOOLEAN;
    last     : File;
    tm       : TM;
    heaplimit: SYSTEM.CARD32;
    treshold : SYSTEM.CARD32;
  END;

(*---------------------------------------------------------------*)

PROCEDURE set_err(p: Project);
BEGIN
  INC(p.errs);
  INCL(env.err_sum,env.fault)
END set_err;

(*---------------------------------------------------------------*)

PROCEDURE (VAR t1: TM) Before(t2-: TM): BOOLEAN;
BEGIN
  IF t1.exist = t2.exist THEN RETURN t1.time < t2.time
  ELSE                        RETURN t2.exist
  END;
END Before;

(*---------------------------------------------------------------*)

PROCEDURE (p: Project) GetLang*(s: ARRAY OF CHAR): pc.Lang;
(* returns -1, if not found *)
  VAR x: Ext;
BEGIN
  x:=p.exts;
  WHILE x#NIL DO
    IF xfs.sys.CompareExtHost(x.name^, s) THEN RETURN x.lang END;
    x:=x.next;
  END;
  RETURN SYSTEM.VAL(pc.Lang,-1);
END GetLang;

PROCEDURE (p: Project) ModeToExt(mode: INTEGER): String;
  VAR x: Ext;
BEGIN
  ASSERT(mode >= 0);
  IF mode = md_main THEN RETURN p.modExt END;
  x:=p.exts;
  WHILE x#NIL DO
    IF x.mode = mode THEN RETURN x.name END;
    x:=x.next;
  END;
  RETURN NIL
END ModeToExt;

PROCEDURE (p: Project) ExtToMode(s: String): INTEGER;
(* returns -1, if not found *)
  VAR x: Ext;
BEGIN
  x:=p.exts;
  WHILE x#NIL DO
    IF xfs.sys.CompareExtHost(x.name^, s^) THEN RETURN x.mode END;
    x:=x.next;
  END;
  RETURN -1
END ExtToMode;

(*---------------------------------------------------------------*)

PROCEDURE Search*(t: Unit; nm-: ARRAY OF CHAR): Unit;
BEGIN
  LOOP
    IF t=NIL THEN RETURN NIL END;
    IF t.name=nm THEN RETURN t END;
    IF t.name<nm THEN t:=t.r ELSE t:=t.l END;
  END;
END Search;

PROCEDURE (u: Unit) Append*(VAR t: Unit);
  VAR l: Unit;
BEGIN
  u.l:=NIL;
  u.r:=NIL;
  u.s:=NIL;
  u.u:=NIL;
  IF t=NIL THEN t:=u; u.u:=u; RETURN END;
  l:=t;
  LOOP
    IF l.name=u.name THEN
      WHILE l.s#NIL DO l:=l.s END;
      l.s:=u; u.u:=l.u;
      RETURN;
    END;
    IF l.name<u.name THEN
      IF l.r=NIL THEN l.r:=u; u.u:=u; RETURN END;
      l:=l.r;
    ELSE
      IF l.l=NIL THEN l.l:=u; u.u:=u; RETURN END;
      l:=l.l;
    END;
  END;
END Append;

(*---------------------------------------------------------------*)

PROCEDURE (VAR tm: TM) GetTime(mod: File);
BEGIN
  ASSERT(mod.fname#NIL);
  IF mod.time.undef THEN
    tm.undef:=FALSE;
    xfs.sys.ModifyTime(mod.fname^,tm.time,tm.exist);
    IF ~(tag_out IN mod.tags) THEN mod.time:=tm END;
  ELSE
    tm:=mod.time;
  END;
END GetTime;

PROCEDURE (pro: Project) GetTime;
BEGIN
  IF pro.tm.undef THEN
    IF pro.fname=NIL THEN
      pro.tm.exist:=FALSE;
    ELSE
      xfs.sys.ModifyTime(pro.fname^,pro.tm.time,pro.tm.exist);
    END;
    IF NOT pro.tm.exist THEN pro.tm.time:=MIN(xfs.Time) END;
    pro.tm.undef:=FALSE;
  END;
END GetTime;

PROCEDURE (pro: Project) PrjTime*(): xfs.Time;
BEGIN
  pro.GetTime;
  RETURN pro.tm.time;
END PrjTime;

PROCEDURE (m: File) TextTime*(): xfs.Time;
(* all files produced from m must be after this date *)
  VAR t: xfs.Time; tm: TM; n: Node;
BEGIN
  m.pro.GetTime;
  tm.GetTime(m);
  IF tm.time>m.pro.tm.time THEN t:=tm.time ELSE t:=m.pro.tm.time END;
  IF m.mode=md_def THEN
    n:=m.inp;
    WHILE n#NIL DO
      tm.GetTime(n.mod);
      IF t<tm.time THEN t:=tm.time END;
      n:=n.next;
    END;
  END;
  RETURN t;
END TextTime;

PROCEDURE (m: File) IsCompilable*(all: BOOLEAN): BOOLEAN;
(** Returns TRUE, if no errors are known for the module and its import. *)

  PROCEDURE Invalidate(m: File);
    VAR l: Node;
  BEGIN
    l:=m.out; INCL(m.tags,error); set_err(m.pro);
    WHILE l#NIL DO Invalidate(l.mod); l:=l.next END;
  END Invalidate;

  VAR n: Node; tm: TM; t: xfs.Time; a: xfs.String;
BEGIN
  IF ~(m.mode IN CMLs) THEN RETURN FALSE END;
  IF error IN m.tags THEN Invalidate(m); RETURN FALSE END;
  n:=m.inp;
  t:=MIN(xfs.Time);
  a:=NIL;
  WHILE n#NIL DO
    IF error IN n.mod.tags THEN Invalidate(m); RETURN FALSE END;
    tm.GetTime(n.mod);
    IF ~tm.exist THEN
      all:=TRUE; (* let the compiler say about error *)
    ELSIF t<tm.time THEN
      t:=tm.time; a:=n.mod.fname
    END;
    n:=n.next;
  END;
  tm.GetTime(m);
  IF ~tm.exist THEN all:=TRUE END;
  IF all THEN RETURN TRUE END;
  IF t<tm.time THEN t:=tm.time; a:=m.fname END;
  n:=m.out;
  WHILE n#NIL DO
    ASSERT((n.mod.mode#md_header) OR ~(noheader IN m.tags));
    tm.GetTime(n.mod);
    IF ~tm.exist THEN
      IF m.pro.verbose THEN
        env.errors.Message(reason_not_found,m.fname^,n.mod.fname^);
      END;
      RETURN TRUE;
    END;
    IF (m.mode=md_oberon) &
       (n.mod.mode IN {md_sym,md_header})
    THEN
      (* nothing *)
    ELSIF t>tm.time THEN
      IF m.pro.verbose THEN
        env.errors.Message(reason_x_after_y,m.fname^,a^,n.mod.fname^);
      END;
      RETURN TRUE;
    END;
    n:=n.next;
  END;
  RETURN FALSE;
END IsCompilable;

PROCEDURE (m: File) SetResult*(batch,err: BOOLEAN);
(** Sets results of module processing. *)
  VAR l: Node;
BEGIN
(* !!!!!
  IF m.mode IN {def,oberon} THEN
    IF batch THEN m.sym.exist:=TRUE; m.sym.time:=MAX(xfs.Time);
    ELSE m.sym.undef:=TRUE;
    END;
    IF m.imp#NIL THEN m.imp.sym:=m.sym END;
  END;
*)
ASSERT(~(13 IN m.tags));
INCL(m.tags,13);
  IF err THEN
    l:=m.out; INCL(m.tags,error);
    WHILE l#NIL DO l.mod.SetResult(batch,err); l:=l.next END;
  END;
EXCL(m.tags,13);
END SetResult;

(*---------------------------------------------------------------*)
<* IF TARGET_IDB THEN *>

<* END *>

PROCEDURE Include(p: Project; name-: ARRAY OF CHAR;
                  mode: INTEGER; tags: SET; VAR x: File);

  PROCEDURE out(md: INTEGER);
    VAR n: Node; m: File;
  BEGIN
    IF  p.objects & (md=md_code)  THEN RETURN END;
    IF (nocode IN tags) & (md=md_code) THEN RETURN END;
    IF ~p.headers & (md=md_header) THEN RETURN END;
    IF (noheader IN tags) & (md=md_header) THEN RETURN END;
    IF ~p.objects & (md=md_obj) & (mode IN CMLs) THEN RETURN END;
    Include(p,name,md,{},m);
    NEW(n);
    n.mod:=m;
    n.next:=x.out;
    x.out:=n;
    n.mod.from:=x;
  END out;

  VAR l: Unit;
BEGIN
  l:=Search(p.tree,name);
  WHILE l#NIL DO
    IF l(File).mode=mode THEN x:=l(File); RETURN END;
    l:=l.s;
  END;
  NEW(x);
  COPY(name,x.name);
  x.fname:=NIL;
  x.mode:=mode;
  x.inp:=NIL;
  x.out:=NIL;
  x.import:=NIL;
  x.next:=NIL;
  x.tags:=tags;
  x.time.undef:=TRUE;
  x.pro :=p;
  IF p.list=NIL THEN p.list:=x ELSE p.last.next:=x END;
  p.last:=x;
  x.Append(p.tree);

<* IF COMPONENT_TESTCOVERAGE THEN *>
  IF mode IN {md_mod, md_main, md_oberon} THEN
    tcMain.AddModule(env.info.file^, name, mode = md_main);
  END;
<* END *>
 <* IF TARGET_IDB THEN *>
  IF env.InterViewMode  THEN;
    CASE mode OF
      |md_def   : out(md_sym);  out(md_header);out(md_emf);
      |md_mod   : out(md_code); out(md_emf);
      |md_main  : out(md_code); out(md_emf);
      |md_oberon: out(md_sym);  out(md_header);
                  out(md_code); out(md_emf);
      |md_sl1   : out(md_code); out(md_emf); out(md_header);
      |md_code  : out(md_emf);
    ELSE
    END;
  ELSE
    CASE mode OF
      |md_def   : out(md_sym);  out(md_header);
      |md_mod   : out(md_code); out(md_obj);
      |md_main  : out(md_code); out(md_obj);
      |md_oberon: out(md_sym);  out(md_header);
                  out(md_code); out(md_obj);
      |md_sl1   : out(md_code); out(md_obj); out(md_header);
      |md_code  : out(md_obj);
      |md_asm   : out(md_obj);
    ELSE
    END;
  END;
 <* ELSE *>
  CASE mode OF
    |md_def   : out(md_sym);  out(md_header);
    |md_mod   : out(md_code); out(md_obj);
    |md_main  : out(md_code); out(md_obj);
    |md_oberon: out(md_sym);  out(md_header);
                out(md_code); out(md_obj);
    |md_sl1   : out(md_code); out(md_obj); out(md_header);
    |md_code  : out(md_obj);
    |md_asm   : out(md_obj);
  ELSE
  END;
 <* END *>

  ASSERT(x.name=name);
END Include;

(*----------------------------------------------------------------*)

VAR
  scaner0: PreUnit;
  scaner1: ScanUnit;

PROCEDURE (u: PreUnit) Definition(name-: ARRAY OF CHAR);
BEGIN
  COPY(name,u.name);
END Definition;

PROCEDURE (u: PreUnit) Implementation(name-: ARRAY OF CHAR);
BEGIN
  COPY(name,u.name);
END Implementation;

PROCEDURE (u: PreUnit) Module(name-: ARRAY OF CHAR);
BEGIN
  COPY(name,u.name);
END Module;

PROCEDURE (u: PreUnit) Import(name-: ARRAY OF CHAR);
BEGIN
  ASSERT(name#'');
END Import;

(*----------------------------------------------------------------*)

PROCEDURE (u: ScanUnit) Expected;
  VAR no   : INTEGER;
      fname: xfs.String;
BEGIN
  CASE u.mode OF
    |md_def   : no:=err_def_expected;
    |md_oberon: no:=err_oberon_expected;
    |md_main  : no:=err_main_expected;
    |md_mod   : no:=err_imp_or_main_expected;
  END;
  xfs.sys.ConvertToHost(u.fname^,fname);
  env.errors.EnvError(no,fname^); set_err(u.pro);
END Expected;

PROCEDURE (u: ScanUnit) ChkName(name-: ARRAY OF CHAR);
VAR fname: xfs.String;
BEGIN
  IF u.name#name THEN
    xfs.sys.ConvertToHost(u.fname^, fname);
    env.errors.EnvError(err_wrong_module_name,fname^,u.name,name);
    set_err(u.pro);
  END;
END ChkName;

PROCEDURE (u: ScanUnit) Definition(name-: ARRAY OF CHAR);
BEGIN
  u.ChkName(name);
  IF u.mode#md_def THEN u.Expected END;
  IF env.config.Option("NOHEADER") THEN INCL(u.tags,noheader) END;
  IF env.config.Option("NOCODE")   THEN INCL(u.tags,nocode) END;
  IF env.config.Option("CSTDLIB")  THEN u.tags:=u.tags+{cstdlib,nocode} END;
END Definition;

PROCEDURE (u: ScanUnit) Implementation(name-: ARRAY OF CHAR);
BEGIN
  u.ChkName(name);
  IF u.mode#md_mod THEN u.Expected END;
END Implementation;

PROCEDURE (u: ScanUnit) Module(name-: ARRAY OF CHAR);
VAR fname: xfs.String;
BEGIN
  u.ChkName(name);
  IF u.mode = md_mod THEN
    IF u.imp THEN
      xfs.sys.ConvertToHost(u.fname^, fname);
      env.errors.EnvError(err_imp_expected,fname^);
      set_err(u.pro);
    ELSE u.mode:=md_main
    END;
  ELSIF ~ (u.mode IN {md_main,md_oberon}) THEN u.Expected
  END;
  IF env.config.Option("MAIN") THEN INCL(u.tags,obmain) END;
END Module;

PROCEDURE (u: ScanUnit) Import(name-: ARRAY OF CHAR);
  VAR l,n: Node;
BEGIN
  ASSERT(name#'');
  l:=u.inp;
  WHILE l#NIL DO IF l.mod.name=name THEN RETURN END; l:=l.next END;
  NEW(n); n.next:=u.inp; u.inp:=n;
  Include(u.pro,name,md_sym,{},n.mod);
  ASSERT(n.mod.name=name);
END Import;

(*----------------------------------------------------------------*)

PROCEDURE RunScanner0;
BEGIN
  pc.pars.chk_import(scaner0);
END RunScanner0;

PROCEDURE RunScanner1;
BEGIN
  pc.pars.chk_import(scaner1);
END RunScanner1;

PROCEDURE Scan(p    : Project;
               f    : xfs.TextFile;
               name-: ARRAY OF CHAR;
               mode : INTEGER;
               imp  : BOOLEAN;
               VAR mod: File);
  VAR err: BOOLEAN; lang: pc.Lang; fname: xfs.String;
BEGIN
  ASSERT(name#'');
  IF ~(mode IN DEPs) THEN
    Include(p,name,mode,{},mod);
    mod.fname:=f.name;
    f.Close;
  ELSE
    COPY(name,scaner1.name);
    scaner1.mode:=mode;
    scaner1.imp :=imp;
    scaner1.fname:=f.name;
    scaner1.tags:={parsed};
    scaner1.inp:=NIL;
    scaner1.pro:=p;
    IF mode=md_oberon THEN lang:=pc.flag_o2 ELSE lang:=pc.flag_m2 END;
    pc.execute(f,TRUE,FALSE,lang,0,RunScanner1,err);
    Include(p,name,scaner1.mode,scaner1.tags,mod);
    mod.fname:=scaner1.fname;
    mod.inp:=scaner1.inp;
    scaner1.inp:=NIL;
    scaner1.pro:=NIL;
    IF err THEN
      xfs.sys.ConvertToHost(mod.fname^, fname);
      env.errors.EnvError(err_in_header,fname^);
      INCL(mod.tags,error); set_err(p);
    END;
  END;
END Scan;

PROCEDURE PreScan(p: Project; f: xfs.TextFile; mode: INTEGER; VAR name: pc.NAME);
  VAR err: BOOLEAN; lang: pc.Lang; fname: xfs.String;
BEGIN
  scaner0.name:="";
  scaner0.fname:=f.name;
  IF mode=md_oberon THEN lang:=pc.flag_o2 ELSE lang:=pc.flag_m2 END;
  pc.execute(f,TRUE,FALSE,lang,0,RunScanner0,err);
  IF err THEN
    xfs.sys.ConvertToHost(scaner0.fname^, fname);
    env.errors.EnvError(err_in_header,fname^);
    set_err(p); name:="";
  ELSE
    name:=scaner0.name;
  END;
END PreScan;

(*---------------------------------------------------------------*)

PROCEDURE Open(p: Project; dir-,name-,ext-: ARRAY OF CHAR; VAR f: xfs.TextFile);
  (* RETURN NIL, if not exist *)
  VAR fname: xfs.FNAME; ok: BOOLEAN;
BEGIN
  f:=NIL;
  ok:=xfs.sys.sCreate(dir,name,ext,fname); ASSERT(ok);
  xfs.sys.sLookup(fname,fname);
  xfs.text.Open(fname,FALSE);
  IF xfs.text.file#NIL THEN
    f:=xfs.text.file(xfs.TextFile); xfs.text.file:=NIL;
  ELSIF xfs.sys.Exists(fname) THEN
    env.errors.EnvError(err_open_error,xfs.text.msg^);
    set_err(p);
  END;
END Open;

PROCEDURE SearchAnyImp(p: Project; dir-,name-: ARRAY OF CHAR;
                       VAR mode: INTEGER; VAR f: xfs.TextFile);

  PROCEDURE Try(md: INTEGER): BOOLEAN;
    VAR ext: String;
  BEGIN
    ext:=p.ModeToExt(md);
    Open(p,dir,name,ext^,f);
    IF f#NIL THEN mode:=md; RETURN TRUE END;
    RETURN FALSE
  END Try;

  VAR f0,f1: xfs.TextFile;
BEGIN
  f:=NIL;
  Open(p,dir,name,p.obeExt^,f0);
  Open(p,dir,name,p.modExt^,f1);
  IF (f0=NIL) & (f1#NIL) THEN
    f:=f1; mode:=md_mod;
  ELSIF (f0#NIL) & (f1=NIL) THEN
    f:=f0; mode:=md_oberon;
    Open(p,dir,name,p.defExt^,f1);
    IF f1#NIL THEN
      env.errors.EnvError(err_two_sources,f0.name^,f1.name^);
      set_err(p);
      f1.Close;
    END;
  ELSIF (f0#NIL) & (f1#NIL) THEN
    f:=f0; mode:=md_oberon;
    env.errors.EnvError(err_two_sources,f0.name^,f1.name^);
    set_err(p);
    f1.Close;
  ELSIF Try(md_sl1) OR Try(md_code) OR Try(md_obj) OR Try(md_asm) THEN
    (* nothing *)
  END;
  IF (f = NIL) & p.aliens THEN
    env.errors.Message(msg_alien,name);
  END;
END SearchAnyImp;

PROCEDURE SearchAnyDef(p: Project; dir-,name-: ARRAY OF CHAR;
                       VAR mode: INTEGER; VAR f: xfs.TextFile);
  VAR f0,f1: xfs.TextFile;
BEGIN
  f:=NIL;
  Open(p,dir,name,p.obeExt^,f0);
  Open(p,dir,name,p.defExt^,f1);
  IF (f0=NIL) & (f1#NIL) THEN
    f:=f1; mode:=md_def;
  ELSIF (f0#NIL) & (f1=NIL) THEN
    f:=f0; mode:=md_oberon;
  ELSIF (f0#NIL) & (f1#NIL) THEN
    f:=f0; mode:=md_oberon;
    env.errors.EnvError(err_two_sources,f0.name^,f1.name^);
    set_err(p);
    f1.Close;
  END;
  IF (f = NIL) & p.aliens THEN
    env.errors.Message(msg_alien_def,name);
  END;
END SearchAnyDef;

PROCEDURE (p: Project) Append*(fname-: ARRAY OF CHAR; lang: pc.Lang);
(** Tries to find implementation or program (oberon) module,
  if extension is omitted.
*)
  VAR
    dir,name,ext: xfs.String;
    f: xfs.TextFile;
    mod: File;
    mode: INTEGER;
    mname: pc.NAME;
BEGIN
  xfs.sys.Get(fname,dir,name,ext);
  IF ext[0]=0X THEN
    SearchAnyDef(p,dir^,name^,mode,f);
    IF f=NIL THEN
      SearchAnyImp(p,dir^,name^,mode,f);
      IF f=NIL THEN
        env.errors.EnvError(err_module_not_found,fname);
        set_err(p);
        RETURN;
      END;
    END;
  ELSIF VAL(SHORTINT,lang)>=0 THEN
    CASE lang OF
      |pc.flag_o2 : mode:=md_oberon;
      |pc.flag_m2 : IF ext^ = p.defExt^ THEN mode:=md_def;
                    ELSE mode:=md_mod;
                    END;
      |pc.flag_sl1: mode:=md_sl1;
    END;
    Open(p,dir^,name^,ext^,f);
    IF f=NIL THEN
      env.errors.EnvError(err_module_not_found,xfs.text.msg^);
      set_err(p);
      RETURN
    END;
  ELSE
    mode := p.ExtToMode(ext);
    IF mode < 0 THEN
      mode:=md_unknown;
(*!!!
      env.errors.EnvError(err_undefined_extension,ext^);
      set_err(p);
      RETURN;
*)
    END;
    Open(p,dir^,name^,ext^,f);
    IF f=NIL THEN
      env.errors.EnvError(err_module_not_found,xfs.text.msg^);
      set_err(p);
      RETURN;
    END;
  END;
  COPY(name^,mname);
  IF mode IN DEPs THEN
    (* Try to get real module name, it can differ from file name *)
    name:=f.name;
    PreScan(p,f,mode,mname);
    IF mname="" THEN RETURN END;
    xfs.text.Open(name^,FALSE);
    IF xfs.text.file=NIL THEN
      env.errors.EnvError(err_open_error,xfs.text.msg^);
      set_err(p);
      RETURN;
    END;
    f:=xfs.text.file(xfs.TextFile);
    xfs.text.file:=NIL;
  END;
  Scan(p,f,mname,mode,FALSE,mod);
  IF mod.mode=md_sl1 THEN INCL(mod.tags,obmain) END;
  INCL(mod.tags,userdef);
END Append;

(*----------------------------------------------------------------*)

PROCEDURE ChkFile(p: Project; name-: ARRAY OF CHAR; md: SET): BOOLEAN;
  VAR l: Unit;
BEGIN
  l:=Search(p.tree,name);
  WHILE l#NIL DO
    IF l(File).mode IN md THEN RETURN TRUE END;
    l:=l.s;
  END;
  RETURN FALSE;
END ChkFile;

PROCEDURE Parse(p: Project; mod: File);
  VAR f: xfs.TextFile; mode: INTEGER; ext: String;
BEGIN
  ASSERT(mod.import=NIL);
  IF parsed IN mod.tags THEN
    RETURN;
  ELSIF mod.mode=md_sym THEN
    IF ~ChkFile(p,mod.name,DEFs) THEN
      SearchAnyDef(p,"",mod.name,mode,f);
      IF f#NIL THEN Scan(p,f,mod.name,mode,FALSE,mod) END;
    END;
    IF ~ChkFile(p,mod.name,{md_header}) THEN
      ext:=p.ModeToExt(md_header);
      Open(p,"",mod.name,ext^,f);
      IF f#NIL THEN Scan(p,f,mod.name,md_header,FALSE,mod) END;
    END;
    IF ~ChkFile(p,mod.name,IMPs) THEN
      SearchAnyImp(p,"",mod.name,mode,f);
      IF f#NIL THEN Scan(p,f,mod.name,mode,TRUE,mod) END;
    END;
  ELSE
    ASSERT(~(mod.mode IN DEPs));
  END;
END Parse;

PROCEDURE MakeClosure(p: Project);
(** Appends all imported modules to module list *)
  VAR l: File;
BEGIN
  l:=p.list;
  WHILE l#NIL DO
    Parse(p,l);
    l:=l.next
  END;
END MakeClosure;

PROCEDURE MakeImport(p: Project);
  VAR l: File; n,o: Node; u: Unit;
BEGIN
  l:=p.list;
  WHILE l#NIL DO
    IF l.mode IN CMLs THEN
      n:=l.inp;
      WHILE n#NIL DO
        ASSERT( (n.mod.mode=md_sym) OR (n.mod.mode=md_emf) );
        IF n.mod.name#l.name THEN
          u:=n.mod.u;
          WHILE (u#NIL) & ~(u(File).mode IN DEFs) DO u:=u.s END;
          IF u#NIL THEN
            NEW(o);
            o.mod:=u(File);
            o.next:=l.import;
            l.import:=o;
          END;
        END;
        n:=n.next;
      END;
    END;
    l:=l.next
  END;
END MakeImport;

(*----------------------------------------------------------------*)

PROCEDURE (p: Project) Regulate*;
(** Builts module list in compilation order *)

  PROCEDURE tie(x: File; VAR tail: Node);
    VAR n: Node;
  BEGIN
    IF tied  IN x.tags THEN RETURN END;
    NEW(n);
    n.mod:=x; n.next:=NIL;
    tail.next:=n; tail:=n;
    INCL(x.tags,tied);
  END tie;

  PROCEDURE Order(x: File; VAR tail: Node);
    VAR n: Node;
  BEGIN
    IF ordered IN x.tags THEN RETURN END;
    INCL(x.tags,ordered);
    n:=x.import;
    WHILE n#NIL DO Order(n.mod,tail); n:=n.next END;
    n:=x.import;
    WHILE n#NIL DO
      IF ~(tied IN n.mod.tags) THEN
        env.errors.EnvError(err_cycle_in_import,x.fname^,n.mod.fname^);
        IF (x.mode#md_oberon) OR (n.mod.mode#md_oberon) THEN
          set_err(p);
        END;
      END;
      n:=n.next
    END;
    tie(x,tail);
  END Order;

  VAR
    l: File;
    head,tail,n: Node;
    ext : env.String;
    ok: BOOLEAN;
    fn: xfs.FNAME;
    src_ext,cplx_ext : env.String;
BEGIN
  IF p.errs>0 THEN RETURN END;
  MakeClosure(p);
  IF p.errs>0 THEN RETURN END;
  MakeImport(p);
  IF p.errs>0 THEN RETURN END;
  l:=p.list; NEW(head); head.next:=NIL; tail:=head;
  WHILE l#NIL DO
    (* by the way mark all output files *)
    n:=l.out;
    WHILE n#NIL DO
      ASSERT(n.mod.fname=NIL);
      INCL(n.mod.tags,tag_out);
      n:=n.next;
    END;
    IF l.mode IN DEFs THEN Order(l,tail) END;
    l:=l.next
  END;
  (* tie what is not tied and create file names *)
  l:=p.list;
  WHILE l#NIL DO
    IF l.fname=NIL THEN
      ext:=p.ModeToExt(l.mode);
      IF l.mode IN FNAMs THEN
        src_ext := p.ModeToExt( l.from.mode );
        NEW(cplx_ext, sts.Length(src_ext^)+sts.Length(ext^) + 1);
        sts.Concat(src_ext^, '.', cplx_ext^);
        sts.Append(ext^, cplx_ext^ );
        ok:=xfs.sys.sCreate('',l.name,cplx_ext^,fn); ASSERT(ok);
      ELSE
        ok:=xfs.sys.sCreate('',l.name,ext^,fn); ASSERT(ok);
      END;
      IF tag_out IN l.tags THEN
        xfs.sys.UseFirst(fn,l.fname);
      ELSE
        xfs.sys.Lookup(fn,l.fname);
      END;
    END;
    IF ~(tied IN l.tags) THEN tie(l,tail) END;
    l:=l.next
  END;
  head:=head.next;
  IF head = NIL THEN
    env.errors.EnvError(msg_no_modules); set_err(p);
  END;
  p.nodes:=head;
  IF p.errs#0 THEN INCL(env.err_sum,env.other) END;
(*
head:=p.nodes;
WHILE head#NIL DO
env.info.print("%08X %2d %-20s",head.mod,head.mod.mode,head.mod.name);
IF head.mod.fname#NIL THEN env.info.print("%-30s\n",head.mod.fname^);
ELSE env.info.print("%-30s\n",'  ');
END;
head:=head.next;
END;
*)
END Regulate;

PROCEDURE (p: Project) SetEquations*;

  PROCEDURE SetExt(name-,def-: ARRAY OF CHAR; mode: SHORTINT; lang: pc.Lang);
    VAR s: String; x: Ext;
  BEGIN
    env.config.Equation(name,s);
    IF s=NIL THEN DStrings.Assign(def,s)
    ELSE DStrings.Assign(s^,s);
    END;
    NEW(x); x.mode:=mode; x.lang:=lang; x.name:=s;
    x.next:=p.exts; p.exts:=x;
  END SetExt;

  VAR s: env.String; i: LONGINT;

BEGIN
  pc.search_back_end;

  SetExt("DEF",   "def",md_def,pc.flag_m2);
  SetExt("MOD",   "mod",md_mod,pc.flag_m2);
  SetExt("OBERON","ob2",md_oberon,pc.flag_o2);
  SetExt("PASCAL","pas",-1,pc.flag_bnrp);
  SetExt("SL1EXT","sl1",md_sl1,pc.flag_sl1);
  SetExt("SYM",   "sym",md_sym,SYSTEM.VAL(pc.Lang,-1));
  SetExt("OBJEXT","obj",md_obj,SYSTEM.VAL(pc.Lang,-1));
  SetExt("EMFEXT","emf",md_emf,SYSTEM.VAL(pc.Lang,-1));
  SetExt("ASMEXT","obj",md_asm,SYSTEM.VAL(pc.Lang,-1));
  SetExt("CODE",  "c",  md_code,pc.flag_c);
  SetExt("HEADER","h",  md_header,SYSTEM.VAL(pc.Lang,-1));

  p.defExt := p.ModeToExt(md_def);
  p.modExt := p.ModeToExt(md_mod);
  p.obeExt := p.ModeToExt(md_oberon);

  p.headers:=pc.code.head_ext#NIL;
  env.config.Equation("COMPILE",s);
  p.objects:=p.headers & (s#NIL) & (s[0]#0X) OR (pc.code.code_ext=NIL);
  p.verbose:=env.config.Option("VERBOSE");
  p.aliens :=env.config.Option("SHOWALIENS");

  env.config.Equation("COMPILERHEAP",s);
  IF (s#NIL) & xcStr.StrToInt(s^,i) & (i>0) THEN xmm.X2C_maxmem:=i END;
  env.config.Equation("COMPILERTHRES",s);
  IF (s#NIL) & xcStr.StrToInt(s^,i) & (i>0) THEN xmm.X2C_threshold:=i END;

  IF p.fname#NIL THEN
    env.config.SetEquation("PRJ",p.fname^);
    xfs.sys.GetName(p.fname^,s);
    env.config.SetEquation("PROJECT",s^);
  ELSE
    env.config.SetEquation("PROJECT","");
  END;
END SetEquations;

PROCEDURE (p: Project) Destroy*;
BEGIN
  xmm.X2C_maxmem:=p.heaplimit;
  xmm.X2C_threshold:=p.treshold;
END Destroy;

PROCEDURE New*(): Project;
  VAR p: Project;
BEGIN
  NEW(p);
  p.list:=NIL;
  p.last:=NIL;
  p.errs:=0;
  p.comp:=0;
  p.fname:=NIL;
  p.tm.undef:=TRUE;
  p.heaplimit:=xmm.X2C_maxmem;
  p.treshold:=xmm.X2C_threshold;
  p.SetEquations;
  RETURN p;
END New;


--------------------------------------------------------------------------------
PROCEDURE FindMain * (curpro:Project): File;
VAR f: File;
BEGIN
  f := curpro.list;
  LOOP
    IF f = NIL THEN (* main not found *) RETURN NIL END;
    IF (f.mode = md_main) OR
       (f.mode = md_oberon) & (obmain IN f.tags)
    THEN 
       RETURN f;
    END;
    f := f.next;
  END;
END FindMain;


BEGIN
  NEW(scaner0);
  NEW(scaner1);
END xcMake.
